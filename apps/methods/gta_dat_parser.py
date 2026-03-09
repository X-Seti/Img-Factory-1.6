#this belongs in methods/gta_dat_parser.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - GTA Data File Parser
"""
GTA Data File Parser — mimics the RenderWare engine load order for GTA3, VC, SA.

Load order (per game):
  GTA3:  data/gta3.dat  → IMG, IDE, IPL, COLFILE, TEXDICTION, MODELFILE, ...
  VC:    data/gta_vc.dat → same structure, different paths
  SA:    data/gta.dat   → same, plus INTERIOR, SPLASH, LOD sections

Each .dat file lists IDE and IPL files in order — this parser walks that chain
and builds a unified world definition: all objects, vehicles, peds, placements.

Classes:
  GTAGame          — enum-like constants + per-game defaults
  DATParser        — parses gta.dat / gta3.dat / gta_vc.dat
  IDEParser        — parses .ide files (objs/tobj/cars/peds/weap/hier/anim/2dfx)
  IPLParser        — parses .ipl files (inst/zone/cull/pick/jump/enex/cars/auzo)
  GTAWorldLoader   — orchestrates full load chain, caches results
"""

import os
import re
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, field


# ─────────────────────────────────────────────────────────────────────────────
# Game constants
# ─────────────────────────────────────────────────────────────────────────────

class GTAGame:
    GTA3 = "gta3"
    VC   = "vc"
    SA   = "sa"

    # Primary .dat file name per game
    DAT_FILE = {
        GTA3: "gta3.dat",
        VC:   "gta_vc.dat",
        SA:   "gta.dat",
    }

    # Default data/ sub-path
    DATA_SUBDIR = "data"

    # IDE section names valid per game
    IDE_SECTIONS = {
        GTA3: {"objs", "tobj", "weap", "hier", "cars", "peds", "path"},
        VC:   {"objs", "tobj", "weap", "hier", "cars", "peds", "path", "txdp"},
        SA:   {"objs", "tobj", "weap", "hier", "cars", "peds", "path",
               "txdp", "anim", "2dfx", "tanm"},
    }

    # IPL section names valid per game
    IPL_SECTIONS = {
        GTA3: {"inst", "cull", "pick", "jump", "enex", "cars", "auzo"},
        VC:   {"inst", "cull", "pick", "jump", "enex", "cars", "auzo", "zone"},
        SA:   {"inst", "cull", "pick", "jump", "enex", "cars", "auzo",
               "zone", "occl", "mult", "grge", "tcyc", "scrn"},
    }

    # Expected object ID ranges per game (for validation)
    ID_RANGES = {
        GTA3: (170,  5999),
        VC:   (170,  5999),
        SA:   (170, 19999),
    }


# ─────────────────────────────────────────────────────────────────────────────
# Data containers
# ─────────────────────────────────────────────────────────────────────────────

@dataclass
class IDEObject:
    """A single definition from an IDE file."""
    model_id:   int
    model_name: str
    txd_name:   str
    obj_type:   str          # objs / tobj / cars / peds / weap / hier / anim
    section:    str
    extra:      Dict[str, Any] = field(default_factory=dict)  # draw_dist, flags, etc.
    source_ide: str = ""     # which .ide file this came from
    line_no:    int = 0


@dataclass
class IPLInstance:
    """A single INST placement from an IPL file."""
    model_id:   int
    model_name: str
    interior:   int          # interior number (0 = exterior world)
    pos_x:      float
    pos_y:      float
    pos_z:      float
    rot_x:      float
    rot_y:      float
    rot_z:      float
    rot_w:      float        # quaternion W (GTA3/VC use scale XYZ instead of quat)
    lod_index:  int = -1     # SA: index into lod instances
    source_ipl: str = ""
    line_no:    int = 0


@dataclass
class DATEntry:
    """A single directive from the master .dat file."""
    directive:  str          # IDE, IPL, IMG, COLFILE, TEXDICTION, MODELFILE, etc.
    path:       str          # raw path as written in the .dat
    abs_path:   str = ""     # resolved absolute path
    exists:     bool = False


@dataclass
class ParseStats:
    total_lines:    int = 0
    ide_files:      int = 0
    ipl_files:      int = 0
    img_files:      int = 0
    objects_loaded: int = 0
    instances:      int = 0
    errors:         List[str] = field(default_factory=list)
    warnings:       List[str] = field(default_factory=list)


# ─────────────────────────────────────────────────────────────────────────────
# DATParser — reads gta3.dat / gta_vc.dat / gta.dat
# ─────────────────────────────────────────────────────────────────────────────

class DATParser: #vers 1
    """Parses a GTA master .dat file and resolves all file paths."""

    # Directives we care about (others are skipped)
    KNOWN_DIRECTIVES = {
        "IDE", "IPL", "IMG", "COLFILE", "TEXDICTION",
        "MODELFILE", "CDIMAGE", "SPLASH", "INTERIOR",
    }

    def __init__(self, game: str = GTAGame.SA):
        self.game       = game
        self.game_root  = ""   # e.g. /home/user/GTA San Andreas
        self.dat_path   = ""
        self.entries:   List[DATEntry] = []
        self.stats      = ParseStats()

    def parse(self, dat_path: str, game_root: str = "") -> bool: #vers 1
        """Parse the .dat file at dat_path.

        game_root: installation root so that relative paths like
                   'data\\maps\\generic\\generic.ide' can be resolved.
                   If empty, dirname(dat_path)/.. is tried.
        """
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
            self.stats.errors.append(f"Cannot read DAT: {e}")
            return False

        self.stats.total_lines = len(lines)

        for lineno, raw in enumerate(lines, 1):
            line = raw.strip()
            if not line or line.startswith("#"):
                continue

            parts = line.split(None, 1)
            if len(parts) < 2:
                continue

            directive = parts[0].upper()
            raw_path  = parts[1].strip()

            entry = DATEntry(directive=directive, path=raw_path)
            entry.abs_path = self._resolve_path(raw_path)
            entry.exists   = os.path.isfile(entry.abs_path)

            self.entries.append(entry)

            if directive == "IDE":
                self.stats.ide_files += 1
            elif directive == "IPL":
                self.stats.ipl_files += 1
            elif directive in ("IMG", "CDIMAGE"):
                self.stats.img_files += 1

        return True

    def _resolve_path(self, raw: str) -> str: #vers 1
        """Resolve a GTA path (may use backslashes, may be relative to root)."""
        # Normalise path separators
        norm = raw.replace("\\", os.sep).replace("/", os.sep)
        # If already absolute
        if os.path.isabs(norm):
            return norm
        # Try relative to game_root
        candidate = os.path.join(self.game_root, norm)
        if os.path.isfile(candidate):
            return os.path.normpath(candidate)
        # Try relative to dat's own directory
        candidate2 = os.path.join(os.path.dirname(self.dat_path), norm)
        return os.path.normpath(candidate2)

    def get_entries_by_directive(self, directive: str) -> List[DATEntry]: #vers 1
        return [e for e in self.entries if e.directive == directive.upper()]

    def ide_entries(self) -> List[DATEntry]:
        return self.get_entries_by_directive("IDE")

    def ipl_entries(self) -> List[DATEntry]:
        return self.get_entries_by_directive("IPL")

    def img_entries(self) -> List[DATEntry]:
        result = self.get_entries_by_directive("IMG")
        result += self.get_entries_by_directive("CDIMAGE")
        return result


# ─────────────────────────────────────────────────────────────────────────────
# IDEParser — parses a single .ide file
# ─────────────────────────────────────────────────────────────────────────────

class IDEParser: #vers 1
    """Parses a single GTA .ide file into IDEObject records."""

    def __init__(self, game: str = GTAGame.SA):
        self.game    = game
        self.objects: List[IDEObject] = []
        self.stats   = ParseStats()
        self._valid_sections = GTAGame.IDE_SECTIONS.get(game, GTAGame.IDE_SECTIONS[GTAGame.SA])

    def parse(self, ide_path: str) -> bool: #vers 1
        if not os.path.isfile(ide_path):
            self.stats.errors.append(f"IDE not found: {ide_path}")
            return False

        try:
            with open(ide_path, "r", encoding="ascii", errors="ignore") as f:
                lines = f.readlines()
        except Exception as e:
            self.stats.errors.append(f"Cannot read IDE: {e}")
            return False

        self.stats.total_lines = len(lines)
        current_section = None
        basename = os.path.basename(ide_path)

        for lineno, raw in enumerate(lines, 1):
            line = raw.strip()
            if not line or line.startswith("#") or line.startswith("//"):
                continue

            low = line.lower()

            # Section boundary
            if low == "end":
                current_section = None
                continue

            if low in self._valid_sections:
                current_section = low
                continue

            # Unknown section name on its own line — also a section header
            if re.match(r'^[a-z0-9_]{2,8}$', low) and "," not in line:
                current_section = low
                continue

            if current_section is None:
                continue

            obj = self._parse_line(current_section, line, basename, lineno)
            if obj:
                self.objects.append(obj)
                self.stats.objects_loaded += 1

        return True

    def _parse_line(self, section: str, line: str,
                    source: str, lineno: int) -> Optional[IDEObject]: #vers 1
        """Parse one data line within a section."""
        try:
            parts = [p.strip() for p in line.split(",")]

            if section in ("objs", "tobj"):
                # GTA3/VC: id, model, txd, meshCount, drawDist..., flags
                # SA:      id, model, txd, drawDist, flags
                if len(parts) < 3:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra: Dict[str, Any] = {}
                if len(parts) > 3:
                    # Try to extract numeric fields
                    try:
                        extra["draw_dist"] = float(parts[3])
                    except ValueError:
                        pass
                if len(parts) > 4:
                    try:
                        extra["flags"] = int(parts[4])
                    except ValueError:
                        pass
                return IDEObject(model_id, model_name, txd_name,
                                 "object", section, extra, source, lineno)

            elif section == "cars":
                # id, model, txd, type, handlingId, gameName, animGroup,
                #   freq, level, compRules, wheelModId, wheelScale, [upgrades]
                if len(parts) < 3:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra = {}
                if len(parts) > 3:
                    extra["veh_type"] = parts[3]
                if len(parts) > 4:
                    extra["handling"] = parts[4]
                return IDEObject(model_id, model_name, txd_name,
                                 "vehicle", section, extra, source, lineno)

            elif section in ("peds", "ped"):
                # id, model, txd, pedType, behaviour, animGroup, driveMask, [flags, animFile, radio1, radio2]
                if len(parts) < 3:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra = {}
                if len(parts) > 3:
                    extra["ped_type"] = parts[3]
                if len(parts) > 4:
                    extra["behaviour"] = parts[4]
                return IDEObject(model_id, model_name, txd_name,
                                 "ped", section, extra, source, lineno)

            elif section == "weap":
                # id, model, txd, animFile, meshCount, drawDist, flags
                if len(parts) < 3:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra = {}
                if len(parts) > 3:
                    extra["anim"] = parts[3]
                return IDEObject(model_id, model_name, txd_name,
                                 "weapon", section, extra, source, lineno)

            elif section in ("hier", "anim", "tanm"):
                # id, model, txd, [animFile]
                if len(parts) < 3:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                return IDEObject(model_id, model_name, txd_name,
                                 "hierarchy", section, {}, source, lineno)

            elif section == "txdp":
                # txdName, parentTxdName  (VC/SA texture dictionary inheritance)
                if len(parts) >= 2:
                    # No model_id — use 0 as sentinel
                    return IDEObject(0, parts[0], parts[1],
                                     "txdparent", section, {}, source, lineno)

            elif section == "2dfx":
                # id, x, y, z, r, g, b, a, type, ...  (SA particle/light effects)
                if len(parts) < 2:
                    return None
                model_id   = int(parts[0])
                return IDEObject(model_id, f"2dfx_{model_id}", "",
                                 "2dfx", section, {}, source, lineno)

        except (ValueError, IndexError):
            self.stats.warnings.append(f"Skipped malformed line {lineno}: {line[:60]}")

        return None


# ─────────────────────────────────────────────────────────────────────────────
# IPLParser — parses a single .ipl file
# ─────────────────────────────────────────────────────────────────────────────

class IPLParser: #vers 1
    """Parses a single GTA .ipl (Item Placement List) file."""

    def __init__(self, game: str = GTAGame.SA):
        self.game      = game
        self.instances: List[IPLInstance] = []
        self.zones:     List[Dict]  = []
        self.culls:     List[Dict]  = []
        self.stats      = ParseStats()
        self._valid_sections = GTAGame.IPL_SECTIONS.get(game, GTAGame.IPL_SECTIONS[GTAGame.SA])

    def parse(self, ipl_path: str) -> bool: #vers 1
        if not os.path.isfile(ipl_path):
            self.stats.errors.append(f"IPL not found: {ipl_path}")
            return False

        try:
            with open(ipl_path, "r", encoding="ascii", errors="ignore") as f:
                lines = f.readlines()
        except Exception as e:
            self.stats.errors.append(f"Cannot read IPL: {e}")
            return False

        self.stats.total_lines = len(lines)
        current_section = None
        basename = os.path.basename(ipl_path)

        for lineno, raw in enumerate(lines, 1):
            line = raw.strip()
            if not line or line.startswith("#") or line.startswith("//"):
                continue

            low = line.lower()

            if low == "end":
                current_section = None
                continue

            if low in self._valid_sections:
                current_section = low
                continue

            if re.match(r'^[a-z0-9_]{2,8}$', low) and "," not in line:
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

    def _parse_inst(self, line: str, source: str, lineno: int) -> Optional[IPLInstance]: #vers 1
        """Parse INST section line — differs between GTA3/VC and SA."""
        try:
            parts = [p.strip() for p in line.split(",")]
            if self.game == GTAGame.SA:
                # SA: id, modelName, interior, posX, posY, posZ, rotX, rotY, rotZ, rotW, lodIndex
                if len(parts) < 10:
                    return None
                return IPLInstance(
                    model_id   = int(parts[0]),
                    model_name = parts[1],
                    interior   = int(parts[2]),
                    pos_x      = float(parts[3]),
                    pos_y      = float(parts[4]),
                    pos_z      = float(parts[5]),
                    rot_x      = float(parts[6]),
                    rot_y      = float(parts[7]),
                    rot_z      = float(parts[8]),
                    rot_w      = float(parts[9]),
                    lod_index  = int(parts[10]) if len(parts) > 10 else -1,
                    source_ipl = source,
                    line_no    = lineno,
                )
            else:
                # GTA3 / VC: id, modelName, posX, posY, posZ, scaleX, scaleY, scaleZ, rotX, rotY, rotZ, rotW
                if len(parts) < 12:
                    return None
                return IPLInstance(
                    model_id   = int(parts[0]),
                    model_name = parts[1],
                    interior   = 0,
                    pos_x      = float(parts[2]),
                    pos_y      = float(parts[3]),
                    pos_z      = float(parts[4]),
                    rot_x      = float(parts[8]),
                    rot_y      = float(parts[9]),
                    rot_z      = float(parts[10]),
                    rot_w      = float(parts[11]),
                    source_ipl = source,
                    line_no    = lineno,
                )
        except (ValueError, IndexError) as e:
            self.stats.warnings.append(f"Skipped INST line {lineno}: {line[:60]}")
        return None

    def _parse_zone(self, line: str, lineno: int) -> Optional[Dict]: #vers 1
        """Parse ZONE section line."""
        try:
            parts = [p.strip() for p in line.split(",")]
            if len(parts) < 8:
                return None
            return {
                "name":     parts[0],
                "type":     int(parts[1]),
                "min_x":    float(parts[2]),
                "min_y":    float(parts[3]),
                "min_z":    float(parts[4]),
                "max_x":    float(parts[5]),
                "max_y":    float(parts[6]),
                "max_z":    float(parts[7]),
                "island":   int(parts[8]) if len(parts) > 8 else 0,
                "text_key": parts[9] if len(parts) > 9 else "",
            }
        except (ValueError, IndexError):
            pass
        return None

    def _parse_cull(self, line: str, lineno: int) -> Optional[Dict]: #vers 1
        """Parse CULL section line."""
        try:
            parts = [p.strip() for p in line.split(",")]
            if len(parts) < 7:
                return None
            return {
                "center_x": float(parts[0]),
                "center_y": float(parts[1]),
                "center_z": float(parts[2]),
                "unknown1": float(parts[3]),
                "width":    float(parts[4]),
                "unknown2": float(parts[5]),
                "height":   float(parts[6]),
            }
        except (ValueError, IndexError):
            pass
        return None


# ─────────────────────────────────────────────────────────────────────────────
# GTAWorldLoader — full orchestrated load chain
# ─────────────────────────────────────────────────────────────────────────────

class GTAWorldLoader: #vers 1
    """
    Orchestrates the full GTA data load chain in RenderWare engine order:
      1. Locate and parse the master .dat file
      2. For each IDE entry in .dat order → parse IDE
      3. For each IPL entry in .dat order → parse IPL
      4. Build unified object registry and instance list
    """

    def __init__(self, game: str = GTAGame.SA):
        self.game     = game
        self.dat      = DATParser(game)

        # Results
        self.objects:   Dict[int, IDEObject]  = {}   # model_id → IDEObject
        self.instances: List[IPLInstance]     = []
        self.zones:     List[Dict]            = []
        self.culls:     List[Dict]            = []

        # Load order log — same order as engine would process
        self.load_log:  List[Tuple[str, str, bool]] = []  # (type, path, success)
        self.stats      = ParseStats()

        # Progress callback: fn(current, total, message) or None
        self.progress_cb = None

    def load(self, dat_path: str, game_root: str = "",
             progress_cb=None) -> bool: #vers 1
        """Full load chain from a .dat path."""
        self.progress_cb = progress_cb
        self.objects.clear()
        self.instances.clear()
        self.zones.clear()
        self.culls.clear()
        self.load_log.clear()
        self.stats = ParseStats()

        # Step 1 — parse the .dat
        self._progress(0, 1, f"Parsing {os.path.basename(dat_path)}…")
        if not self.dat.parse(dat_path, game_root):
            self.stats.errors += self.dat.stats.errors
            return False

        ide_list = self.dat.ide_entries()
        ipl_list = self.dat.ipl_entries()
        total    = len(ide_list) + len(ipl_list)
        done     = 0

        # Step 2 — parse all IDEs in .dat order
        for entry in ide_list:
            done += 1
            self._progress(done, total, f"IDE: {os.path.basename(entry.path)}")
            self._load_ide(entry)

        # Step 3 — parse all IPLs in .dat order
        for entry in ipl_list:
            done += 1
            self._progress(done, total, f"IPL: {os.path.basename(entry.path)}")
            self._load_ipl(entry)

        # Tally
        self.stats.ide_files      = len(ide_list)
        self.stats.ipl_files      = len(ipl_list)
        self.stats.objects_loaded = len(self.objects)
        self.stats.instances      = len(self.instances)

        return True

    def _load_ide(self, entry: DATEntry): #vers 1
        if not entry.exists:
            msg = f"IDE missing: {entry.path}"
            self.stats.warnings.append(msg)
            self.load_log.append(("IDE", entry.abs_path, False))
            return

        parser = IDEParser(self.game)
        ok = parser.parse(entry.abs_path)
        self.load_log.append(("IDE", entry.abs_path, ok))

        for obj in parser.objects:
            # Later definitions override earlier ones (matches engine behaviour)
            self.objects[obj.model_id] = obj

        self.stats.errors   += parser.stats.errors
        self.stats.warnings += parser.stats.warnings

    def _load_ipl(self, entry: DATEntry): #vers 1
        if not entry.exists:
            self.stats.warnings.append(f"IPL missing: {entry.path}")
            self.load_log.append(("IPL", entry.abs_path, False))
            return

        parser = IPLParser(self.game)
        ok = parser.parse(entry.abs_path)
        self.load_log.append(("IPL", entry.abs_path, ok))

        self.instances += parser.instances
        self.zones     += parser.zones
        self.culls     += parser.culls

        self.stats.errors   += parser.stats.errors
        self.stats.warnings += parser.stats.warnings

    def _progress(self, current: int, total: int, msg: str): #vers 1
        if callable(self.progress_cb):
            try:
                self.progress_cb(current, total, msg)
            except Exception:
                pass

    # ── Query helpers ──────────────────────────────────────────────────────

    def get_object(self, model_id: int) -> Optional[IDEObject]: #vers 1
        return self.objects.get(model_id)

    def find_by_name(self, name: str) -> List[IDEObject]: #vers 1
        n = name.lower()
        return [o for o in self.objects.values() if o.model_name.lower() == n]

    def get_instances_for_model(self, model_id: int) -> List[IPLInstance]: #vers 1
        return [i for i in self.instances if i.model_id == model_id]

    def get_objects_by_type(self, obj_type: str) -> List[IDEObject]: #vers 1
        t = obj_type.lower()
        return [o for o in self.objects.values() if o.obj_type == t]

    def get_summary(self) -> str: #vers 1
        lines = [
            f"Game:       {self.game.upper()}",
            f"DAT file:   {os.path.basename(self.dat.dat_path)}",
            f"IDE files:  {self.stats.ide_files}",
            f"IPL files:  {self.stats.ipl_files}",
            f"Objects:    {self.stats.objects_loaded}",
            f"Instances:  {self.stats.instances}",
            f"Zones:      {len(self.zones)}",
            f"Warnings:   {len(self.stats.warnings)}",
            f"Errors:     {len(self.stats.errors)}",
        ]
        return "\n".join(lines)


# ─────────────────────────────────────────────────────────────────────────────
# Auto-detect which game a directory belongs to
# ─────────────────────────────────────────────────────────────────────────────

def detect_game(game_root: str) -> Optional[str]: #vers 1
    """Inspect a game root directory and return GTAGame constant or None."""
    data = os.path.join(game_root, "data")
    if os.path.isfile(os.path.join(data, "gta.dat")):
        return GTAGame.SA
    if os.path.isfile(os.path.join(data, "gta_vc.dat")):
        return GTAGame.VC
    if os.path.isfile(os.path.join(data, "gta3.dat")):
        return GTAGame.GTA3
    return None


def find_dat_file(game_root: str, game: str) -> Optional[str]: #vers 1
    """Return absolute path to the .dat for the given game, or None."""
    name = GTAGame.DAT_FILE.get(game)
    if not name:
        return None
    candidate = os.path.join(game_root, "data", name)
    return candidate if os.path.isfile(candidate) else None


# ─────────────────────────────────────────────────────────────────────────────
# Integration hook
# ─────────────────────────────────────────────────────────────────────────────

def integrate_gta_dat_parser(main_window) -> bool: #vers 1
    """Attach a GTAWorldLoader to main_window for use by the UI."""
    try:
        main_window.gta_world_loader = GTAWorldLoader()
        main_window.detect_gta_game  = detect_game
        main_window.find_dat_file    = find_dat_file
        if hasattr(main_window, "log_message"):
            main_window.log_message("GTA DAT/IDE/IPL parser integrated")
        return True
    except Exception as e:
        if hasattr(main_window, "log_message"):
            main_window.log_message(f"DAT parser integrate error: {e}")
        return False


__all__ = [
    "GTAGame",
    "IDEObject",
    "IPLInstance",
    "DATEntry",
    "ParseStats",
    "DATParser",
    "IDEParser",
    "IPLParser",
    "GTAWorldLoader",
    "detect_game",
    "find_dat_file",
    "integrate_gta_dat_parser",
]
