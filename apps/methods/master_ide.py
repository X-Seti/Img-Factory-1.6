#this belongs in apps/methods/master_ide.py - Version: 4

##Methods list -
# MasterIDEResult
# collect_ide_paths_from_dat
# load_master_ide
# write_master_ide

"""master_ide.py - Master IDE feature (Sep 5 2026)"""

import os
from dataclasses import dataclass, field
from typing import Dict, List


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
class MasterIDEResult: #vers 2
    objects_by_section: Dict[str, list] = field(default_factory=dict)   # section -> list of IDEObject, sorted by model_id
    source_files: List[str] = field(default_factory=list)
    collisions: List[MasterIDECollision] = field(default_factory=list)
    name_collisions: List[MasterIDENameCollision] = field(default_factory=list)
    redefinitions: List[MasterIDERedefinition] = field(default_factory=list)
    out_of_range: List[MasterIDEOutOfRange] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)

    @property
    def total_objects(self): #vers 1
        return sum(len(v) for v in self.objects_by_section.values())


def collect_ide_paths_from_dat(dat_path: str, game_root: str = None, game: str = None): #vers 2
    """Resolve every real IDE file a game's .dat actually loads (Sep
    2026, per Keith: "load them all from /data/gta*.dat or /sol/
    gta*.dat and combine"). Reuses GTAWorldLoader's own real 2-phase
    load (default.dat/special.dat, then the main dat) instead of
    duplicating that logic - just pulls the resolved IDE paths back
    out afterwards. Falls back to GTA3-style parsing (game=None,
    Sep 12 2026, per Keith: "gta(anyother).dat to load another gta
    modding project") when the .dat's own filename isn't one of the
    known real names - a different modding project's own .dat, not
    silently ignored. Returns (ide_paths, game)."""
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
                # 2dfx entries deliberately share their base object's
                # real model_id (a synthetic "2dfx_<id>" stub name,
                # see IDEParser's own docstring) - that is expected
                # attachment, not a real duplicate ID assignment, so
                # they're excluded from every check below (Sep 12
                # 2026, per Keith: "we don't need to list the id's
                # again from the 2dfx section/ifx files"). Still
                # grouped under their own "2dfx" section above like
                # every other entry.
                if obj.section == "2dfx":
                    continue
                seen_ids.setdefault(obj.model_id, []).append((obj.model_name, obj.source_ide))
                seen_names.setdefault(obj.model_name.lower(), []).append((obj.model_id, obj.source_ide))
                key = (obj.model_id, obj.model_name.lower())
                sig = (obj.txd_name.lower(), obj.section, tuple(sorted((obj.extra or {}).items())))
                seen_defs.setdefault(key, []).append(
                    (obj.model_name, obj.txd_name, obj.section, obj.source_ide, sig))
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


def write_master_ide(result: MasterIDEResult, output_path: str) -> bool: #vers 1
    """Write the merged result back out as one real, combined .ide
    file - grouped by section (never mixed), sorted by ID within each group."""
    try:
        lines = []
        for section, objs in result.objects_by_section.items():
            lines.append(section)
            for obj in objs:
                if section in ('objs', 'tobj'):
                    lines.append(_format_objs_or_tobj_line(obj))
                else:
                    # Not yet verified for this section type - best
                    # effort using whatever raw values are available.
                    extra_vals = ", ".join(str(v) for v in (obj.extra or {}).values())
                    line = f"{obj.model_id}, {obj.model_name}, {obj.txd_name}"
                    if extra_vals:
                        line += f", {extra_vals}"
                    lines.append(line)
            lines.append("end")
            lines.append("")

        with open(output_path, 'w', encoding='ascii', errors='ignore') as f:
            f.write("\n".join(lines))
        return True
    except Exception:
        return False
