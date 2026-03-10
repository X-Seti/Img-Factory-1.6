#this belongs in methods/gta_dat_parser.py - Version: 5
# X-Seti - March 2026 - IMG Factory 1.6 - GTA Data File Parser
"""
GTA3 + VC + SA + GTASOL Data File Parser — mirrors the RenderWare engine load chain exactly.

GTA3 load order (verified from real files):
  Phase 1: data/default.dat  -> IDE DATA/DEFAULT.IDE, TEXDICTION, MODELFILE, COLFILE
  Phase 2: data/gta3.dat     -> 16 IDEs, 16 COLFILEs (island index 0-3), 14 IPLs

VC load order (verified from real files):
  Phase 1: data/default.dat  -> IDE DATA/DEFAULT.IDE, TEXDICTION, MODELFILE, COLFILE
  Phase 2: data/gta_vc.dat   -> 31 IDEs, 1 COLFILE, 36 IPLs

SA load order (verified from real files):
  Phase 1: data/default.dat  -> 3 IDEs (DEFAULT.IDE + VEHICLES.IDE + PEDS.IDE), 1 COLFILE
  Phase 2: data/gta.dat      -> 3 IMGs, 54 IDEs, 52 IPLs, 0 COLFILEs
  Alt:     data/gta_quick.dat -> stripped dev variant (1 IMG, 13 IDE, 11 IPL)

SOL (GTASOL mod) load order (verified from real files):
  Phase 1: sol/special.dat   -> IDE models/gta3.ide, TEXDICTION, MODELFILE, 2 COLFILEs
  Phase 2: sol/gta_sol.dat   -> 12 CDIMAGEs, 16 IDEs (+5 .iFX lighting), 11 COLFILEs, 111 IPLs
  Alt DAT: sol/gtasol.dat    -> same content, no-underscore variant
  Sol dir: sol/ or SOL/      -> case-insensitive search on Linux required
  Paths:   relative to SA game root (not to dat file); sol/ and models/ prefixes
  Notes:   .iFX files are listed as IDE directives (SA 2dfx lighting extension)
           CDIMAGE and IMG are interchangeable directives (same meaning)

Field formats per game (verified from real .ide files):
  GTA3 objs: id, model, txd, meshCount, dist1[, dist2], flags
  GTA3 peds: id, model, txd, pedType, behaviour, animGroup, carsDriveMask
  GTA3 cars: id, model, txd, type, handlingId, gameName, class, freq, level, compRules[, wheelId, wheelScale]
  GTA3 weap: id, model, txd, meshCount, drawDist, flags
  GTA3 hier: id, model, txd
  GTA3 inst: id, model, px, py, pz, sx, sy, sz, rx, ry, rz, rw  (12 fields)

  VC/SA peds: ..., carsDriveMask, animFile, radio1, radio2        (+3 vs GTA3)
  VC/SA cars: ..., gameName, animFile, class, ...                  (+animFile vs GTA3)
  VC/SA weap: id, model, txd, animFile, meshCount, drawDist, flags (+animFile vs GTA3)
  VC   hier:  id, model, txd                                        (same as GTA3)
  SA   hier:  id, model, txd, animFile, drawDist                   (5 fields)
  SA   inst:  id, model, interior, px, py, pz, rx, ry, rz, rw[, lod]

  SOL: SA-format IDE/IPL sections (mod runs on SA engine)
"""

import os
import re
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, field


class GTAGame:
    GTA3 = "gta3"
    VC   = "vc"
    SA   = "sa"
    SOL  = "sol"   # GTASOL mod (SA engine, multi-city)

    DAT_FILE = {
        "gta3": "gta3.dat",
        "vc":   "gta_vc.dat",
        "sa":   "gta.dat",
        "sol":  "gta_sol.dat",   # lives in sol/ or SOL/ subfolder
    }

    # Alternative DAT names
    ALT_DAT_FILE = {
        "sa":  "gta_quick.dat",
        "sol": "gtasol.dat",     # no-underscore variant
    }

    # Phase-1 dat loaded before the main dat
    # SOL uses special.dat (in same sol/ folder) as its phase-1 loader
    DEFAULT_DAT = {
        "gta3": "default.dat",   # in data/
        "vc":   "default.dat",   # in data/
        "sa":   "default.dat",   # in data/
        "sol":  "special.dat",   # in sol/ or SOL/
    }

    DATA_SUBDIR = "data"
    SOL_SUBDIRS = ("sol", "SOL")  # case variants to try on Linux

    IDE_SECTIONS = {
        "gta3": {"objs", "tobj", "weap", "hier", "anim", "cars", "peds", "path"},
        "vc":   {"objs", "tobj", "weap", "hier", "anim", "cars", "peds", "path", "txdp"},
        "sa":   {"objs", "tobj", "weap", "hier", "anim", "cars", "peds", "path",
                 "txdp", "2dfx", "tanm"},
        "sol":  {"objs", "tobj", "weap", "hier", "anim", "cars", "peds", "path",
                 "txdp", "2dfx", "tanm"},  # SA engine — same sections
    }

    IPL_SECTIONS = {
        "gta3": {"inst", "cull", "pick", "jump", "enex", "cars", "auzo"},
        "vc":   {"inst", "cull", "pick", "jump", "enex", "cars", "auzo", "zone"},
        "sa":   {"inst", "cull", "pick", "jump", "enex", "cars", "auzo",
                 "zone", "occl", "mult", "grge", "tcyc", "scrn"},
        "sol":  {"inst", "cull", "pick", "jump", "enex", "cars", "auzo",
                 "zone", "occl", "mult", "grge", "tcyc", "scrn"},  # SA engine
    }

    ID_RANGES = {
        "gta3": (0,  5999),
        "vc":   (0,  5999),
        "sa":   (0, 19999),
        "sol":  (0, 65535),  # multi-city mod — expanded ID space
    }


@dataclass
class DATEntry:
    directive:   str
    path:        str
    abs_path:    str  = ""
    exists:      bool = False
    extra:       str  = ""     # island index for COLFILE
    source_dat:  str  = ""


@dataclass
class IDEObject:
    model_id:    int
    model_name:  str
    txd_name:    str
    obj_type:    str
    section:     str
    extra:       Dict[str, Any] = field(default_factory=dict)
    source_ide:  str = ""
    line_no:     int = 0


@dataclass
class IPLInstance:
    model_id:    int
    model_name:  str
    interior:    int
    pos_x:       float
    pos_y:       float
    pos_z:       float
    rot_x:       float
    rot_y:       float
    rot_z:       float
    rot_w:       float
    lod_index:   int  = -1
    source_ipl:  str  = ""
    line_no:     int  = 0


@dataclass
class ParseStats:
    total_lines:     int = 0
    ide_files:       int = 0
    ipl_files:       int = 0
    col_files:       int = 0
    img_files:       int = 0
    objects_loaded:  int = 0
    instances:       int = 0
    errors:          List[str] = field(default_factory=list)
    warnings:        List[str] = field(default_factory=list)


def _resolve_ci(base: str, rel_path: str) -> Optional[str]:
    """Case-insensitive path resolution from base directory.
    Walks each path component, matching case-insensitively.
    Returns the real absolute path if found, else None.
    Needed for SOL on Linux where sol/ vs SOL/ (case) appear in the same .dat file.
    """
    parts = rel_path.replace("\\", "/").split("/")
    current = base
    for part in parts:
        if not part:
            continue
        try:
            entries = os.listdir(current)
        except (PermissionError, NotADirectoryError, FileNotFoundError):
            return None
        part_lower = part.lower()
        match = next((e for e in entries if e.lower() == part_lower), None)
        if match is None:
            return None
        current = os.path.join(current, match)
    return current if os.path.isfile(current) else None


class DATParser: #vers 2
    """Parses a single GTA .dat file — handles COLFILE island index and strips inline comments."""

    def __init__(self, game: str = GTAGame.GTA3):
        self.game      = game
        self.game_root = ""
        self.dat_path  = ""
        self.entries:  List[DATEntry] = []
        self.stats     = ParseStats()

    def parse(self, dat_path: str, game_root: str = "") -> bool: #vers 2
        self.dat_path  = dat_path
        self.game_root = game_root or os.path.normpath(
            os.path.join(os.path.dirname(dat_path), ".."))
        self.entries.clear()
        self.stats = ParseStats()

        if not os.path.isfile(dat_path):
            self.stats.errors.append(f"DAT not found: {dat_path}")
            return False
        try:
            with open(dat_path, "r", encoding="ascii", errors="ignore") as f:
                lines = f.readlines()
        except Exception as e:
            self.stats.errors.append(f"Cannot read DAT {dat_path}: {e}")
            return False

        self.stats.total_lines = len(lines)
        dat_basename = os.path.basename(dat_path)

        for raw in lines:
            line = raw.split("#")[0].strip()
            if not line:
                continue
            parts = line.split()
            if len(parts) < 2:
                continue
            directive = parts[0].upper()

            if directive == "COLFILE":
                # COLFILE <island_int> <path>
                if len(parts) < 3:
                    continue
                island   = parts[1]
                raw_path = parts[2]
                abs_path = self._resolve(raw_path)
                self.entries.append(DATEntry(
                    directive=directive, path=raw_path, abs_path=abs_path,
                    exists=os.path.isfile(abs_path), extra=island,
                    source_dat=dat_basename))
                self.stats.col_files += 1
                continue

            if directive == "SPLASH":
                continue   # no file path

            raw_path = parts[1]
            abs_path = self._resolve(raw_path)
            self.entries.append(DATEntry(
                directive=directive, path=raw_path, abs_path=abs_path,
                exists=os.path.isfile(abs_path), source_dat=dat_basename))

            if directive == "IDE":
                self.stats.ide_files += 1
            elif directive == "IPL":
                self.stats.ipl_files += 1
            elif directive in ("IMG", "CDIMAGE"):
                self.stats.img_files += 1

        return True

    def _resolve(self, raw: str) -> str: #vers 3
        """Resolve a Windows-style relative path to an absolute path.
        Uses case-insensitive fallback for Linux (needed for SOL's mixed-case paths)."""
        norm = raw.strip().replace("\\", os.sep).replace("/", os.sep)
        if os.path.isabs(norm):
            return norm
        # Try game_root-relative first (most GTA paths are relative to install root)
        cand = os.path.normpath(os.path.join(self.game_root, norm))
        if os.path.isfile(cand):
            return cand
        # Try dat-file-relative
        cand2 = os.path.normpath(os.path.join(os.path.dirname(self.dat_path), norm))
        if os.path.isfile(cand2):
            return cand2
        # Case-insensitive fallback (Linux: sol/ vs SOL/ in same file)
        ci = _resolve_ci(self.game_root, norm)
        if ci:
            return ci
        return cand  # return game-root candidate even if not found

    def get_by_directive(self, d: str) -> List[DATEntry]:
        return [e for e in self.entries if e.directive == d.upper()]

    def ide_entries(self)  -> List[DATEntry]: return self.get_by_directive("IDE")
    def ipl_entries(self)  -> List[DATEntry]: return self.get_by_directive("IPL")
    def col_entries(self)  -> List[DATEntry]: return self.get_by_directive("COLFILE")
    def img_entries(self)  -> List[DATEntry]:
        return self.get_by_directive("IMG") + self.get_by_directive("CDIMAGE")


class IDEParser: #vers 2
    """
    Parses a single GTA3 .ide file.

    GTA3 objs: id, model, txd, meshCount, dist1[, dist2], flags
    GTA3 peds: id, model, txd, pedType, behaviour, animGroup, carsDriveMask
    GTA3 cars: id, model, txd, type, handlingId, gameName, class, freq, level, compRules[, wheelId, wheelScale]
    GTA3 hier: id, model, txd
    """

    def __init__(self, game: str = GTAGame.GTA3):
        self.game    = game
        self.objects: List[IDEObject] = []
        self.stats   = ParseStats()
        self._valid  = GTAGame.IDE_SECTIONS.get(game, GTAGame.IDE_SECTIONS[GTAGame.GTA3])

    def parse(self, ide_path: str) -> bool: #vers 2
        if not os.path.isfile(ide_path):
            self.stats.errors.append(f"IDE not found: {ide_path}")
            return False
        try:
            with open(ide_path, "r", encoding="ascii", errors="ignore") as f:
                lines = f.readlines()
        except Exception as e:
            self.stats.errors.append(f"Cannot read IDE {ide_path}: {e}")
            return False

        self.stats.total_lines = len(lines)
        current_section        = None
        basename               = os.path.basename(ide_path)

        for lineno, raw in enumerate(lines, 1):
            line = raw.split("#")[0].strip()
            if not line or line.startswith("//"):
                continue
            low = line.lower()
            if low == "end":
                current_section = None
                continue
            if low in self._valid or (re.match(r'^[a-z0-9_]{2,8}$', low) and "," not in line):
                current_section = low
                continue
            if current_section is None:
                continue

            obj = self._parse_line(current_section, line, basename, lineno)
            if obj:
                self.objects.append(obj)
                self.stats.objects_loaded += 1

        return True

    def _parse_line(self, section: str, line: str, source: str, lineno: int) -> Optional[IDEObject]: #vers 2
        try:
            parts = [p.strip() for p in line.split(",")]

            if section in ("objs", "tobj"):
                # id, model, txd, meshCount, dist1[, dist2], flags
                if len(parts) < 5:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                try:
                    mesh_count = int(parts[3])
                except ValueError:
                    mesh_count = 1
                extra: Dict[str, Any] = {"mesh_count": mesh_count}
                dist_end = 4 + mesh_count
                dists = []
                for i in range(4, min(dist_end, len(parts))):
                    try: dists.append(float(parts[i]))
                    except ValueError: pass
                if dists:
                    extra["draw_dist"] = dists[0]
                    if len(dists) > 1:
                        extra["draw_dist2"] = dists[1]
                if dist_end < len(parts):
                    try: extra["flags"] = int(parts[dist_end])
                    except ValueError: pass
                return IDEObject(model_id, model_name, txd_name,
                                 "object", section, extra, source, lineno)

            elif section == "cars":
                # GTA3: id, model, txd, type, handlingId, gameName, class, freq, level, compRules[, wheelId, wheelScale]
                # VC/SA: id, model, txd, type, handlingId, gameName, animFile, class, freq, level, compRules[, wheelId, wheelScale]
                # VC/SA add animFile between gameName and class — shift all subsequent fields by 1
                if len(parts) < 7:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra = {
                    "veh_type":  parts[3],
                    "handling":  parts[4],
                    "game_name": parts[5],
                }
                if self.game in (GTAGame.VC, GTAGame.SA):
                    # parts[6] = animFile, parts[7] = class, parts[8] = freq, ...
                    extra["anim_file"] = parts[6] if len(parts) > 6 else ""
                    class_idx = 7
                else:
                    # GTA3: parts[6] = class, parts[7] = freq, ...
                    class_idx = 6
                if len(parts) > class_idx:
                    extra["veh_class"] = parts[class_idx]
                if len(parts) > class_idx + 1:
                    try: extra["freq"]  = int(parts[class_idx + 1])
                    except ValueError: pass
                if len(parts) > class_idx + 2:
                    try: extra["level"] = int(parts[class_idx + 2])
                    except ValueError: pass
                wheel_idx = class_idx + 4
                if len(parts) > wheel_idx:
                    try: extra["wheel_model"] = int(parts[wheel_idx])
                    except ValueError: pass
                if len(parts) > wheel_idx + 1:
                    try: extra["wheel_scale"] = float(parts[wheel_idx + 1])
                    except ValueError: pass
                return IDEObject(model_id, model_name, txd_name,
                                 "vehicle", section, extra, source, lineno)

            elif section in ("peds", "ped"):
                # GTA3: id, model, txd, pedType, behaviour, animGroup, carsDriveMask           (7 fields)
                # VC:   id, model, txd, pedType, behaviour, animGroup, carsDriveMask,
                #            animFile, radio1, radio2                                           (10 fields)
                if len(parts) < 7:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra = {
                    "ped_type":        parts[3],
                    "behaviour":       parts[4],
                    "anim_group":      parts[5],
                    "cars_drive_mask": parts[6],
                }
                if self.game in (GTAGame.VC, GTAGame.SA) and len(parts) > 7:
                    extra["anim_file"] = parts[7]
                if self.game in (GTAGame.VC, GTAGame.SA) and len(parts) > 9:
                    try:
                        extra["radio1"] = int(parts[8])
                        extra["radio2"] = int(parts[9])
                    except ValueError:
                        pass
                return IDEObject(model_id, model_name, txd_name,
                                 "ped", section, extra, source, lineno)

            elif section == "weap":
                # GTA3: id, model, txd, meshCount, drawDist, flags             (6 fields)
                # VC/SA: id, model, txd, animFile, meshCount, drawDist, flags  (7 fields, adds animFile in slot 3)
                if len(parts) < 6:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra: Dict[str, Any] = {}
                if self.game in (GTAGame.VC, GTAGame.SA):
                    # slot 3 = animFile, slot 4 = meshCount, slot 5 = drawDist, slot 6 = flags
                    extra["anim_file"] = parts[3]
                    try: extra["mesh_count"] = int(parts[4])
                    except (ValueError, IndexError): pass
                    try: extra["draw_dist"]  = float(parts[5])
                    except (ValueError, IndexError): pass
                    if len(parts) > 6:
                        try: extra["flags"] = int(parts[6])
                        except ValueError: pass
                else:
                    # GTA3: slot 3 = meshCount, slot 4 = drawDist, slot 5 = flags
                    try: extra["mesh_count"] = int(parts[3])
                    except (ValueError, IndexError): pass
                    try: extra["draw_dist"]  = float(parts[4])
                    except (ValueError, IndexError): pass
                    if len(parts) > 5:
                        try: extra["flags"] = int(parts[5])
                        except ValueError: pass
                return IDEObject(model_id, model_name, txd_name,
                                 "weapon", section, extra, source, lineno)

            elif section in ("hier", "anim", "tanm"):
                # GTA3/VC hier: id, model, txd                             (3 fields)
                # SA      hier: id, model, txd, animFile, drawDist         (5 fields)
                if len(parts) < 3:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra: Dict[str, Any] = {}
                if self.game in (GTAGame.SA, GTAGame.SOL):
                    if len(parts) > 3:
                        extra["anim_file"] = parts[3]
                    if len(parts) > 4:
                        try: extra["draw_dist"] = float(parts[4])
                        except ValueError: pass
                return IDEObject(model_id, model_name, txd_name,
                                 "hierarchy", section, extra, source, lineno)

            elif section == "txdp":
                if len(parts) >= 2:
                    return IDEObject(0, parts[0], parts[1],
                                     "txdparent", section, {}, source, lineno)

            elif section == "2dfx":
                if len(parts) < 2:
                    return None
                model_id = int(parts[0])
                return IDEObject(model_id, f"2dfx_{model_id}", "",
                                 "2dfx", section, {}, source, lineno)

        except (ValueError, IndexError):
            pass
        return None


class IPLParser: #vers 2
    """
    Parses a single GTA3/VC/SA .ipl file.
    GTA3 inst: id, model, px, py, pz, sx, sy, sz, rx, ry, rz, rw  (12 fields)
    SA   inst: id, model, interior, px, py, pz, rx, ry, rz, rw[, lod]
    """

    def __init__(self, game: str = GTAGame.GTA3):
        self.game       = game
        self.instances: List[IPLInstance] = []
        self.zones:     List[Dict]        = []
        self.culls:     List[Dict]        = []
        self.stats      = ParseStats()
        self._valid     = GTAGame.IPL_SECTIONS.get(game, GTAGame.IPL_SECTIONS[GTAGame.GTA3])

    def parse(self, ipl_path: str) -> bool: #vers 2
        if not os.path.isfile(ipl_path):
            self.stats.errors.append(f"IPL not found: {ipl_path}")
            return False
        try:
            with open(ipl_path, "r", encoding="ascii", errors="ignore") as f:
                lines = f.readlines()
        except Exception as e:
            self.stats.errors.append(f"Cannot read IPL {ipl_path}: {e}")
            return False

        self.stats.total_lines = len(lines)
        current_section        = None
        basename               = os.path.basename(ipl_path)

        for lineno, raw in enumerate(lines, 1):
            line = raw.split("#")[0].strip()
            if not line or line.startswith("//"):
                continue
            low = line.lower()
            if low == "end":
                current_section = None
                continue
            if low in self._valid or (re.match(r'^[a-z0-9_]{2,8}$', low) and "," not in line):
                current_section = low
                continue
            if current_section is None:
                continue

            if current_section == "inst":
                obj = self._parse_inst(line, basename, lineno)
                if obj:
                    self.instances.append(obj)
                    self.stats.instances += 1
            elif current_section == "zone":
                z = self._parse_zone(line, lineno)
                if z:
                    self.zones.append(z)
            elif current_section == "cull":
                c = self._parse_cull(line, lineno)
                if c:
                    self.culls.append(c)
        return True

    def _parse_inst(self, line: str, source: str, lineno: int) -> Optional[IPLInstance]: #vers 2
        try:
            parts = [p.strip() for p in line.split(",")]
            if self.game in (GTAGame.SA, GTAGame.SOL):
                if len(parts) < 10:
                    return None
                return IPLInstance(
                    model_id=int(parts[0]), model_name=parts[1], interior=int(parts[2]),
                    pos_x=float(parts[3]), pos_y=float(parts[4]), pos_z=float(parts[5]),
                    rot_x=float(parts[6]), rot_y=float(parts[7]),
                    rot_z=float(parts[8]), rot_w=float(parts[9]),
                    lod_index=int(parts[10]) if len(parts) > 10 else -1,
                    source_ipl=source, line_no=lineno)
            else:
                # GTA3/VC: id, model, px, py, pz, sx, sy, sz, rx, ry, rz, rw
                if len(parts) < 12:
                    return None
                return IPLInstance(
                    model_id=int(parts[0]), model_name=parts[1], interior=0,
                    pos_x=float(parts[2]), pos_y=float(parts[3]), pos_z=float(parts[4]),
                    rot_x=float(parts[8]), rot_y=float(parts[9]),
                    rot_z=float(parts[10]), rot_w=float(parts[11]),
                    source_ipl=source, line_no=lineno)
        except (ValueError, IndexError):
            self.stats.warnings.append(f"Skipped INST line {lineno}: {line[:70]}")
        return None

    def _parse_zone(self, line: str, lineno: int) -> Optional[Dict]: #vers 1
        try:
            p = [x.strip() for x in line.split(",")]
            if len(p) < 8:
                return None
            return {"name": p[0], "type": int(p[1]),
                    "min_x": float(p[2]), "min_y": float(p[3]), "min_z": float(p[4]),
                    "max_x": float(p[5]), "max_y": float(p[6]), "max_z": float(p[7]),
                    "island": int(p[8]) if len(p) > 8 else 0,
                    "text_key": p[9] if len(p) > 9 else ""}
        except (ValueError, IndexError):
            pass
        return None

    def _parse_cull(self, line: str, lineno: int) -> Optional[Dict]: #vers 1
        try:
            p = [x.strip() for x in line.split(",")]
            if len(p) < 7:
                return None
            return {"center_x": float(p[0]), "center_y": float(p[1]), "center_z": float(p[2]),
                    "unknown1": float(p[3]), "width": float(p[4]),
                    "unknown2": float(p[5]), "height": float(p[6])}
        except (ValueError, IndexError):
            pass
        return None


class GTAWorldLoader: #vers 3
    """
    Orchestrates the full two-phase GTA3/VC/SA load chain in engine order:
      Phase 1: default.dat -> base IDEs (DEFAULT.IDE; SA also loads VEHICLES.IDE + PEDS.IDE)
      Phase 2: main .dat   -> map IDEs, then IPLs (SA: also IMG directives)
                              SA alt: gta_quick.dat (stripped dev variant, auto-detected)

    Later IDE definitions override earlier ones (matches engine behaviour).
    """

    def __init__(self, game: str = GTAGame.GTA3):
        self.game        = game
        self.default_dat = DATParser(game)
        self.main_dat    = DATParser(game)
        self.objects:    Dict[int, IDEObject] = {}
        self.instances:  List[IPLInstance]    = []
        self.zones:      List[Dict]           = []
        self.culls:      List[Dict]           = []
        # (phase, type, abs_path, success)
        self.load_log:   List[Tuple[str, str, str, bool]] = []
        self.stats       = ParseStats()
        self.progress_cb = None

    def load(self, game_root: str, progress_cb=None) -> bool: #vers 4
        """Full load from a game root directory."""
        self.progress_cb = progress_cb
        self._reset()

        # ── Locate phase-1 (default/special) dat ─────────────────────────
        default_path = find_default_dat(game_root, self.game)
        if default_path:
            self._progress(0, 1, f"Phase 1: {os.path.basename(default_path)}")
            self.default_dat.parse(default_path, game_root)
            self._process_dat(self.default_dat, "default")
        else:
            self.stats.warnings.append(
                f"Phase-1 dat not found for game '{self.game}' in {game_root}")

        # ── Locate phase-2 main dat ───────────────────────────────────────
        main_path = find_dat_file(game_root, self.game)
        if not main_path:
            self.stats.errors.append(
                f"Main DAT not found for game '{self.game}' in {game_root}")
            return False

        self._progress(0, 1, f"Phase 2: {os.path.basename(main_path)}")
        self.main_dat.parse(main_path, game_root)
        self._process_dat(self.main_dat, "main")

        self.stats.objects_loaded = len(self.objects)
        self.stats.instances      = len(self.instances)
        return True

    def load_from_dat(self, dat_path: str, game_root: str = "",
                      progress_cb=None) -> bool: #vers 1
        """Load from an explicit .dat path."""
        self.progress_cb = progress_cb
        self._reset()
        if not game_root:
            game_root = os.path.normpath(
                os.path.join(os.path.dirname(dat_path), ".."))
        data_dir     = os.path.dirname(dat_path)
        default_name = GTAGame.DEFAULT_DAT.get(self.game)
        if default_name:
            default_path = os.path.join(data_dir, default_name)
            if os.path.isfile(default_path):
                self._progress(0, 1, f"Phase 1: {default_name}")
                self.default_dat.parse(default_path, game_root)
                self._process_dat(self.default_dat, "default")
        self._progress(0, 1, f"Phase 2: {os.path.basename(dat_path)}")
        self.main_dat.parse(dat_path, game_root)
        self._process_dat(self.main_dat, "main")
        self.stats.objects_loaded = len(self.objects)
        self.stats.instances      = len(self.instances)
        return True

    def _process_dat(self, dat: DATParser, phase: str): #vers 2
        ide_list = [e for e in dat.entries if e.directive == "IDE"]
        ipl_list = [e for e in dat.entries if e.directive == "IPL"]
        total = len(ide_list) + len(ipl_list)
        done  = 0
        for entry in ide_list:
            done += 1
            self._progress(done, total, f"IDE: {os.path.basename(entry.path)}")
            self._load_ide(entry, phase)
        for entry in ipl_list:
            done += 1
            self._progress(done, total, f"IPL: {os.path.basename(entry.path)}")
            self._load_ipl(entry, phase)
        self.stats.ide_files += len(ide_list)
        self.stats.ipl_files += len(ipl_list)
        self.stats.col_files += len(dat.col_entries())
        self.stats.img_files += len(dat.img_entries())

    def _load_ide(self, entry: DATEntry, phase: str): #vers 2
        if not entry.exists:
            self.stats.warnings.append(f"[{phase}] IDE missing: {entry.path}")
            self.load_log.append((phase, "IDE", entry.abs_path, False))
            return
        parser = IDEParser(self.game)
        ok     = parser.parse(entry.abs_path)
        self.load_log.append((phase, "IDE", entry.abs_path, ok))
        for obj in parser.objects:
            self.objects[obj.model_id] = obj   # later overrides earlier
        self.stats.errors   += parser.stats.errors
        self.stats.warnings += parser.stats.warnings

    def _load_ipl(self, entry: DATEntry, phase: str): #vers 2
        if not entry.exists:
            self.stats.warnings.append(f"[{phase}] IPL missing: {entry.path}")
            self.load_log.append((phase, "IPL", entry.abs_path, False))
            return
        parser = IPLParser(self.game)
        ok     = parser.parse(entry.abs_path)
        self.load_log.append((phase, "IPL", entry.abs_path, ok))
        self.instances += parser.instances
        self.zones     += parser.zones
        self.culls     += parser.culls
        self.stats.errors   += parser.stats.errors
        self.stats.warnings += parser.stats.warnings

    def _reset(self): #vers 1
        self.objects.clear(); self.instances.clear()
        self.zones.clear();   self.culls.clear()
        self.load_log.clear(); self.stats = ParseStats()

    def _progress(self, cur: int, total: int, msg: str): #vers 1
        if callable(self.progress_cb):
            try: self.progress_cb(cur, total, msg)
            except Exception: pass

    def get_object(self, model_id: int) -> Optional[IDEObject]:
        return self.objects.get(model_id)

    def find_by_name(self, name: str) -> List[IDEObject]:
        n = name.lower()
        return [o for o in self.objects.values() if o.model_name.lower() == n]

    def get_instances_for_model(self, model_id: int) -> List[IPLInstance]:
        return [i for i in self.instances if i.model_id == model_id]

    def get_objects_by_type(self, obj_type: str) -> List[IDEObject]:
        t = obj_type.lower()
        return [o for o in self.objects.values() if o.obj_type == t]

    def lookup_img_entry(self, filename_no_ext: str) -> Optional[IDEObject]:
        """Find IDE def for a DFF/TXD name (no extension)."""
        n = filename_no_ext.lower()
        for obj in self.objects.values():
            if obj.model_name.lower() == n:
                return obj
        return None

    def get_summary(self) -> str: #vers 2
        return "\n".join([
            f"Game:        {self.game.upper()}",
            f"default.dat: {os.path.basename(self.default_dat.dat_path) or '(not loaded)'}",
            f"main .dat:   {os.path.basename(self.main_dat.dat_path) or '(not loaded)'}",
            f"IDE files:   {self.stats.ide_files}",
            f"IPL files:   {self.stats.ipl_files}",
            f"COL files:   {self.stats.col_files}",
            f"Objects:     {self.stats.objects_loaded}",
            f"Instances:   {self.stats.instances}",
            f"Zones:       {len(self.zones)}",
            f"Warnings:    {len(self.stats.warnings)}",
            f"Errors:      {len(self.stats.errors)}",
        ])


def _find_sol_dir(game_root: str) -> Optional[str]:
    """Return the absolute path to the sol folder (sol/ or SOL/), or None."""
    for name in GTAGame.SOL_SUBDIRS:
        candidate = os.path.join(game_root, name)
        if os.path.isdir(candidate):
            return candidate
    return None


def detect_game(game_root: str) -> Optional[str]: #vers 4
    """Detect which GTA game lives at game_root. Checks SA data/ and SOL sol/ subfolder."""
    data = os.path.join(game_root, "data")
    # SOL: check sol/ or SOL/ for gta_sol.dat or gtasol.dat
    sol_dir = _find_sol_dir(game_root)
    if sol_dir:
        for name in (GTAGame.DAT_FILE["sol"], GTAGame.ALT_DAT_FILE["sol"]):
            if os.path.isfile(os.path.join(sol_dir, name)):
                return GTAGame.SOL
    if os.path.isfile(os.path.join(data, "gta.dat")):       return GTAGame.SA
    if os.path.isfile(os.path.join(data, "gta_quick.dat")): return GTAGame.SA
    if os.path.isfile(os.path.join(data, "gta_vc.dat")):    return GTAGame.VC
    if os.path.isfile(os.path.join(data, "gta3.dat")):      return GTAGame.GTA3
    return None


def find_dat_file(game_root: str, game: str) -> Optional[str]: #vers 3
    """Return absolute path to the main .dat for the given game, or None.
    SOL: searches sol/ and SOL/ subfolders; tries alt name (gtasol.dat) if primary missing."""
    if game == GTAGame.SOL:
        sol_dir = _find_sol_dir(game_root)
        if not sol_dir:
            return None
        for name in (GTAGame.DAT_FILE["sol"], GTAGame.ALT_DAT_FILE["sol"]):
            c = os.path.join(sol_dir, name)
            if os.path.isfile(c):
                return c
        return None
    data = os.path.join(game_root, "data")
    name = GTAGame.DAT_FILE.get(game)
    if name:
        c = os.path.join(data, name)
        if os.path.isfile(c):
            return c
    alt = GTAGame.ALT_DAT_FILE.get(game)
    if alt:
        c = os.path.join(data, alt)
        if os.path.isfile(c):
            return c
    return None


def find_default_dat(game_root: str, game: str) -> Optional[str]: #vers 2
    """Return absolute path to the phase-1 dat (default.dat / special.dat), or None."""
    name = GTAGame.DEFAULT_DAT.get(game)
    if not name:
        return None
    if game == GTAGame.SOL:
        sol_dir = _find_sol_dir(game_root)
        if not sol_dir:
            return None
        c = os.path.join(sol_dir, name)
        # Also try case-insensitive on Linux
        if not os.path.isfile(c):
            ci = _resolve_ci(sol_dir, name)
            return ci
        return c
    c = os.path.join(game_root, "data", name)
    return c if os.path.isfile(c) else None


def integrate_gta_dat_parser(main_window) -> bool: #vers 3
    try:
        main_window.gta_world_loader = GTAWorldLoader()
        main_window.detect_gta_game  = detect_game
        main_window.find_dat_file    = find_dat_file
        if hasattr(main_window, "log_message"):
            main_window.log_message("GTA DAT/IDE/IPL parser integrated (v5, GTA3/VC/SA/SOL)")
        return True
    except Exception as e:
        if hasattr(main_window, "log_message"):
            main_window.log_message(f"DAT parser integrate error: {e}")
        return False


class GTAWorldXRef: #vers 1
    """
    Cross-reference index built from a loaded GTAWorldLoader.
    Used to produce hover tooltips on IMG Factory table entries.

    For a given stem (filename without extension):
      - model_map[stem]   -> IDEObject  (defined in some .ide)
      - txd_stems         -> set of txd names referenced by any IDE object
      - col_stems         -> set of COL file stems from COLFILE entries
      - img_stems         -> set of IMG/CDIMAGE archive stems

    Example tooltip for "landstal.dff":
      "Defined in default.ide (vehicle)
       TXD: landstal  [in gta3.img]
       COL: vehicles  [present]"
    """

    def __init__(self):
        self.model_map:  Dict[str, "IDEObject"] = {}  # stem.lower() -> IDEObject
        self.txd_stems:  set = set()                  # all txd_name.lower() values
        self.col_stems:  set = set()                  # col file stem.lower()
        self.img_stems:  set = set()                  # img/cdimage archive stems

    def tooltip_for(self, filename: str) -> str: #vers 3
        """Return a single-line hover tooltip for an IMG entry filename, or '' if nothing known.

        Format (DFF):
          Model defined in gta3.ide,  has txd - name.txd,  has col - name.col
          Model defined in gta3.ide,  has txd - name.txd,  missing name.col
          Model defined in gta3.ide,  missing name.txd,  missing name.col

        Format (TXD):
          TXD referenced by IDE - used by: model1, model2 ...

        Format (COL):
          COL listed in COLFILE directive
        """
        if not filename or "." not in filename:
            return ""
        stem = filename.rsplit(".", 1)[0].lower()
        ext  = filename.rsplit(".", 1)[1].lower()

        obj = self.model_map.get(stem)
        if obj:
            ide = obj.source_ide or "unknown.ide"
            parts = [f"Model defined in {ide}"]

            txd = obj.txd_name.lower() if obj.txd_name else ""
            if txd and txd != "null":
                txd_present = txd in self.txd_stems
                if txd_present:
                    parts.append(f"has txd - {txd}.txd")
                else:
                    parts.append(f"missing {txd}.txd")
            else:
                parts.append("no txd")

            if ext == "dff":
                col_present = stem in self.col_stems
                if col_present:
                    parts.append(f"has col - {stem}.col")
                else:
                    parts.append(f"missing {stem}.col")

            return ",  ".join(parts)

        elif ext == "txd":
            if stem in self.txd_stems:
                users = [o.model_name for o in self.model_map.values()
                         if o.txd_name and o.txd_name.lower() == stem][:5]
                suffix = " ..." if len(users) == 5 else ""
                if users:
                    return f"TXD referenced by IDE - used by: {', '.join(users)}{suffix}"
                return "TXD archive referenced by IDE model"

        elif ext == "col":
            if stem in self.col_stems:
                return "COL listed in COLFILE directive"

        return ""


def build_xref(loader: "GTAWorldLoader") -> GTAWorldXRef: #vers 1
    """Build a cross-reference index from a fully loaded GTAWorldLoader."""
    xref = GTAWorldXRef()

    def _stem(path: str) -> str:
        """Extract lowercase filename stem, handling both / and \\ separators."""
        name = path.replace("\\", "/").split("/")[-1]
        return name.rsplit(".", 1)[0].lower() if "." in name else name.lower()

    # Index all IDE objects by model name stem
    for obj in loader.objects.values():
        xref.model_map[obj.model_name.lower()] = obj
        if obj.txd_name and obj.txd_name.lower() not in ("null", ""):
            xref.txd_stems.add(obj.txd_name.lower())

    # Index COLFILE stems from both dat parsers
    for dat in (loader.default_dat, loader.main_dat):
        for entry in dat.col_entries():
            xref.col_stems.add(_stem(entry.path))

    # Index IMG/CDIMAGE archive stems
    for dat in (loader.default_dat, loader.main_dat):
        for entry in dat.img_entries():
            xref.img_stems.add(_stem(entry.path))

    return xref


__all__ = [
    "GTAGame", "DATEntry", "IDEObject", "IPLInstance", "ParseStats",
    "DATParser", "IDEParser", "IPLParser", "GTAWorldLoader",
    "GTAWorldXRef", "build_xref",
    "detect_game", "find_dat_file", "find_default_dat",
    "_find_sol_dir", "_resolve_ci",
    "integrate_gta_dat_parser",
]
