#this belongs in apps/methods/master_ide.py - Version: 7

##Methods list -
# MasterIDEResult
# collect_ide_paths_from_dat
# load_master_ide
# write_master_ide

"""master_ide.py - Master IDE feature (Sep 5 2026)"""

import os
import re
from dataclasses import dataclass, field
from typing import Dict, List

_EDITABLE_SECTIONS = ("objs", "tobj")   # only sections this app reconstructs from parsed fields

# Base engine declaration files (Sep 12 2026, per Keith's own real
# files: "default.ide is found in the /data folder, gta3.ide in
# some versions is found in /models/... with the gta3.img dir") -
# these ship with every install and declare only the base engine's
# own peds/cars/wheels/weapons/hier (see the real "ID Key (usage)"
# doc's own 0-299ish ranges), never a project's own world content.
# Matched by basename only, regardless of which real folder they
# live in, since that varies by game/version.
_BASE_ENGINE_FILENAMES = {"default.ide", "gta3.ide"}


def _is_base_engine_file(path: str) -> bool: #vers 1
    return os.path.basename(path).lower() in _BASE_ENGINE_FILENAMES

# Sections whose leading number is NOT a real declared object ID -
# excluded from every ID-based check (Sep 12 2026, per Keith's own
# real ID Key doc + txdp bug found while extending these checks):
# 2dfx's leading field references an EXISTING model's ID (attaching
# effects to it, never declaring a new one - see IDEParser's own
# "2dfx_<id>" stub docstring). txdp has NO real model ID at all -
# IDEParser hardcodes model_id=0 for every txdp line (it's really a
# TXD-inherits-from-TXD declaration, not an object), so every txdp
# entry across every file was falsely colliding at id=0 before this
# fix - a real bug caught while confirming peds/cars/weap/hier/anim
# (all genuine ID-declaring sections) were already correctly covered.
_ID_EXCLUDED_SECTIONS = ("2dfx", "txdp")

# GTASOL's own real per-file ID range convention (Sep 12 2026, from
# Keith's own real "ID Key (usage)" reference doc) - each of these
# source files is meant to own a distinct, non-overlapping ID block.
# Keyed by lowercase filename stem (no extension).
SOL_FILE_RANGES = {
    "special":    (300, 615),
    "generics":   (616, 1987),
    "game_vc":    (1987, 4766),
    "game_lc":    (4767, 6202),
    "game_ext":   (6203, 6479),
    "game_sp":    (6480, 6679),
    "game_la":    (6680, 8314),
    "game_sf":    (8315, 9590),
    "game_lv":    (9591, 10970),
    "game_sa":    (10971, 12841),
    "game_mll":   (13964, 14763),
    "skyeffects": (27071, 30010),
    "seabed":     (30100, 30861),
}


@dataclass
class MasterIDECollision: #vers 1
    model_id: int
    entries: list   # list of (model_name, source_ide) sharing this real ID


@dataclass
class MasterIDENameCollision: #vers 1
    model_name: str
    entries: list   # list of (model_id, source_ide) sharing this real name


@dataclass
class MasterIDERedefinition: #vers 1
    model_id: int
    model_name: str
    entries: list   # list of (txd_name, section, source_ide) - same id+name, different data


@dataclass
class MasterIDEOutOfRange: #vers 1
    model_id: int
    model_name: str
    source_ide: str
    min_id: int
    max_id: int


@dataclass
class MasterIDEFileRangeViolation: #vers 1
    model_id: int
    model_name: str
    source_ide: str
    expected_min: int
    expected_max: int


@dataclass
class MasterIDEResult: #vers 4
    objects_by_section: Dict[str, list] = field(default_factory=dict)   # section -> list of IDEObject, sorted by model_id
    raw_section_lines: Dict[str, List[str]] = field(default_factory=dict)   # section -> real raw lines, verbatim, pooled across files
    source_files: List[str] = field(default_factory=list)
    collisions: List[MasterIDECollision] = field(default_factory=list)
    name_collisions: List[MasterIDENameCollision] = field(default_factory=list)
    redefinitions: List[MasterIDERedefinition] = field(default_factory=list)
    out_of_range: List[MasterIDEOutOfRange] = field(default_factory=list)
    file_range_violations: List[MasterIDEFileRangeViolation] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)

    @property
    def total_objects(self): #vers 1
        return sum(len(v) for v in self.objects_by_section.values())


def _section_order_and_raw(text): #vers 1
    """Real section names in the order they appear in one real file,
    plus each real section's own raw lines verbatim (same keyword-
    detection rule the real parser uses) - shared by load_master_ide
    (pooling non-editable sections across files for the combined
    output) and master_ide_edit.py's own per-file write-back."""
    order = []
    raw = {}
    current = None
    for line in text.splitlines():
        stripped = line.split("#")[0].strip()
        low = stripped.lower()
        if current is None:
            if low and re.match(r'^[a-z0-9_]{2,8}$', low) and "," not in stripped:
                current = low
                order.append(current)
                raw[current] = []
            continue
        if low == "end":
            current = None
            continue
        raw[current].append(line.rstrip("\r"))
    return order, raw


def _leading_id(line: str) -> int: #vers 1
    """The real leading numeric field of a raw section line, for
    numeric sort - lines that don't start with one sort last rather
    than crashing."""
    first = line.strip().split(",", 1)[0].strip()
    try:
        return int(first)
    except ValueError:
        return 2**31 - 1


def collect_ide_paths_from_dat(dat_path: str, game_root: str = None, game: str = None,
                                ignore_base_files: bool = False): #vers 3
    """Resolve every real IDE file a game's .dat actually loads (Sep
    2026, per Keith: "load them all from /data/gta*.dat or /sol/
    gta*.dat and combine"). Reuses GTAWorldLoader's own real 2-phase
    load (default.dat/special.dat, then the main dat) instead of
    duplicating that logic - just pulls the resolved IDE paths back
    out afterwards. Falls back to GTA3-style parsing (game=None,
    Sep 12 2026, per Keith: "gta(anyother).dat to load another gta
    modding project") when the .dat's own filename isn't one of the
    known real names - a different modding project's own .dat, not
    silently ignored. ignore_base_files=True drops default.ide/
    gta3.ide (matched by basename regardless of folder) so real
    world-content ID counting/reassignment starts from the first
    real "world (generic)" IDE file instead (Sep 12 2026, per Keith:
    "option needed to ignore these two files, starting the id's
    from world (generic) ide"). Returns (ide_paths, game)."""
    from apps.methods.gta_dat_parser import (
        detect_game_from_dat_filename, GTAWorldLoader, GTAGame)

    if not game:
        game = detect_game_from_dat_filename(dat_path) or GTAGame.GTA3
    if not game_root:
        game_root = os.path.normpath(os.path.join(os.path.dirname(dat_path), ".."))

    loader = GTAWorldLoader(game)
    loader.lazy_ipl_loading = True
    loader.load_from_dat(dat_path, game_root)

    paths, seen = [], set()
    for dat in (loader.default_dat, loader.main_dat):
        for entry in dat.ide_entries():
            if entry.exists and entry.abs_path not in seen:
                if ignore_base_files and _is_base_engine_file(entry.abs_path):
                    continue
                seen.add(entry.abs_path)
                paths.append(entry.abs_path)
    return paths, game


def load_master_ide(ide_paths: List[str], game: str = None) -> MasterIDEResult: #vers 3
    """Load and merge any number of real .ide files. Each real file
    parses independently (its own real objects, own real section
    tags); merging just groups everything by section and sorts by ID
    within each group - it does NOT rename, renumber, or otherwise
    touch anything yet (see this module's own docstring for why).
    Runs 4 real checks (Sep 12 2026, per Keith: "all possible checks
    you think could be needed"): same ID/different name (collision -
    real crash risk), same name/different ID (name_collision -
    inconsistent depending on load order), same ID+name but
    different data across files (redefinition - silent override),
    and ID outside the target game's real supported range."""
    from apps.methods.gta_dat_parser import IDEParser, GTAGame

    result = MasterIDEResult()
    seen_ids: Dict[int, list] = {}     # model_id -> [(model_name, source_ide), ...]
    seen_names: Dict[str, list] = {}   # lowercase model_name -> [(model_id, source_ide), ...]
    seen_defs: Dict[tuple, list] = {}  # (model_id, lowercase name) -> [(txd_name, section, source_ide, sig), ...]

    for path in ide_paths:
        if not path or not os.path.isfile(path):
            result.errors.append(f"IDE file not found: {path}")
            continue
        try:
            parser = IDEParser(game or GTAGame.GTA3)
            if not parser.parse(path):
                result.errors.append(f"Failed to parse: {path}")
                continue
            result.source_files.append(path)
            for obj in parser.objects:
                result.objects_by_section.setdefault(obj.section, []).append(obj)
                # 2dfx/txdp entries carry no real declared object ID
                # of their own (see this module's own _ID_EXCLUDED_
                # SECTIONS docstring above) - excluded from every
                # check below. Still grouped under their own section
                # above like every other entry.
                if obj.section in _ID_EXCLUDED_SECTIONS:
                    continue
                seen_ids.setdefault(obj.model_id, []).append((obj.model_name, obj.source_ide))
                seen_names.setdefault(obj.model_name.lower(), []).append((obj.model_id, obj.source_ide))
                key = (obj.model_id, obj.model_name.lower())
                sig = (obj.txd_name.lower(), obj.section, tuple(sorted((obj.extra or {}).items())))
                seen_defs.setdefault(key, []).append(
                    (obj.model_name, obj.txd_name, obj.section, obj.source_ide, sig))

            # Real 2dfx (and any other non-objs/tobj) lines carry a
            # field layout this app doesn't fully round-trip through
            # IDEObject.extra (2dfx corona effects have quoted string
            # fields and more trailing values than the parser keeps) -
            # reconstructing them from parsed fields silently corrupts
            # real data (Sep 12 2026, per Keith: real bug report,
            # "2dfx doesn't use model names, just the ID... the data
            # is being completely changed"). Pool the RAW original
            # lines for every non-editable section instead, combined
            # numerically at write time - never reconstructed.
            with open(path, "r", encoding="ascii", errors="ignore") as f:
                raw_text = f.read()
            _order, raw_sections = _section_order_and_raw(raw_text)
            for section, raw_lines in raw_sections.items():
                if section in _EDITABLE_SECTIONS:
                    continue
                result.raw_section_lines.setdefault(section, []).extend(raw_lines)
        except Exception as e:
            result.errors.append(f"Error parsing {path}: {e}")

    for section in result.objects_by_section:
        result.objects_by_section[section].sort(key=lambda o: o.model_id)

    for model_id, entries in seen_ids.items():
        # A real collision is the SAME id used by DIFFERENT real
        # model names - the game can only resolve one, real crash
        # risk (Sep 12 2026, per Keith: "there should never be an ID
        # like 1234, foobar.. used twice it would crash the game").
        distinct_names = {name.lower() for name, _ in entries}
        if len(entries) > 1 and len(distinct_names) > 1:
            result.collisions.append(MasterIDECollision(model_id=model_id, entries=entries))

    for model_name, entries in seen_names.items():
        # The reverse case - the same real model name declared under
        # DIFFERENT ids across files. Not an instant crash the way an
        # id collision is, but which id actually wins depends on
        # source-file load order - worth flagging (Sep 12 2026, per
        # Keith).
        distinct_ids = {mid for mid, _ in entries}
        if len(entries) > 1 and len(distinct_ids) > 1:
            result.name_collisions.append(
                MasterIDENameCollision(model_name=model_name, entries=entries))

    for (model_id, _name_lower), entries in seen_defs.items():
        # Same id+name pair defined by more than one real file with
        # genuinely different data (txd/section/extra fields) - the
        # engine's own "later definition overrides earlier" behaviour
        # means which file's version actually loads depends silently
        # on load order.
        distinct_sigs = {sig for _n, _t, _s, _src, sig in entries}
        if len(entries) > 1 and len(distinct_sigs) > 1:
            model_name = entries[0][0]
            result.redefinitions.append(MasterIDERedefinition(
                model_id=model_id, model_name=model_name,
                entries=[(txd, sec, src) for _n, txd, sec, src, _sig in entries]))

    min_max = GTAGame.ID_RANGES.get(game, None)
    if min_max:
        min_id, max_id = min_max
        for model_id, entries in seen_ids.items():
            if model_id < min_id or model_id > max_id:
                for model_name, source_ide in entries:
                    result.out_of_range.append(MasterIDEOutOfRange(
                        model_id=model_id, model_name=model_name, source_ide=source_ide,
                        min_id=min_id, max_id=max_id))

    if game == GTAGame.SOL:
        # Each real SOL source file is meant to own its own distinct
        # ID block (Sep 12 2026, from Keith's own real "ID Key
        # (usage)" doc) - flag anything that strays outside the file
        # it actually came from's own documented range.
        for model_id, entries in seen_ids.items():
            for model_name, source_ide in entries:
                stem = os.path.splitext(os.path.basename(source_ide))[0].lower()
                expected = SOL_FILE_RANGES.get(stem)
                if expected and not (expected[0] <= model_id <= expected[1]):
                    result.file_range_violations.append(MasterIDEFileRangeViolation(
                        model_id=model_id, model_name=model_name, source_ide=source_ide,
                        expected_min=expected[0], expected_max=expected[1]))

    return result


def _format_objs_or_tobj_line(obj) -> str: #vers 1
    """Rebuild one real objs/tobj line, preserving whichever real
    field-count variant this specific object was originally parsed
    with (Sep 5 2026)"""
    extra = obj.extra or {}
    parts = [str(obj.model_id), obj.model_name, obj.txd_name]
    if 'mesh_count' in extra:
        parts.append(str(extra['mesh_count']))
        if 'draw_dist' in extra:
            parts.append(_fmt_num(extra['draw_dist']))
        if 'draw_dist2' in extra:
            parts.append(_fmt_num(extra['draw_dist2']))
        if 'flags' in extra:
            parts.append(str(extra['flags']))
    else:
        if 'draw_dist' in extra:
            parts.append(_fmt_num(extra['draw_dist']))
        if 'flags' in extra:
            parts.append(str(extra['flags']))
    if obj.section == 'tobj' and 'time_on' in extra and 'time_off' in extra:
        parts.append(str(extra['time_on']))
        parts.append(str(extra['time_off']))
    return ", ".join(parts)


def _fmt_num(val) -> str: #vers 1
    """ IDE files write whole-number draw distances without a
    trailing .0 real example: "299", not "299.0")"""
    if isinstance(val, float) and val == int(val):
        return str(int(val))
    return str(val)


def write_master_ide(result: MasterIDEResult, output_path: str) -> bool: #vers 2
    """Write the merged result back out as one real, combined .ide
    file - grouped by section (never mixed), sorted by ID within
    each group. Only objs/tobj are reconstructed from parsed fields;
    every other real section (2dfx, cars, peds, weap, hier, anim,
    txdp) is written from each source file's own pooled RAW lines,
    sorted numerically by their own leading ID field - never rebuilt
    from IDEObject.extra, which doesn't retain every real field for
    those section types (Sep 12 2026, per Keith's own real bug
    report - see load_master_ide's own docstring)."""
    try:
        lines = []
        for section in _EDITABLE_SECTIONS:
            objs = result.objects_by_section.get(section)
            if not objs:
                continue
            lines.append(section)
            for obj in objs:
                lines.append(_format_objs_or_tobj_line(obj))
            lines.append("end")
            lines.append("")

        for section, raw_lines in result.raw_section_lines.items():
            if not raw_lines:
                continue
            lines.append(section)
            lines.extend(sorted(raw_lines, key=_leading_id))
            lines.append("end")
            lines.append("")

        with open(output_path, 'w', encoding='ascii', errors='ignore') as f:
            f.write("\n".join(lines))
        return True
    except Exception:
        return False
