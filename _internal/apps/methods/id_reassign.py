#this belongs in apps/methods/id_reassign.py - Version: 9
# X-Seti - September 12 2026 - IMG Factory 1.6 - ID Block Reassignment

"""id_reassign.py - Move + ID reassignment + cascading (Sep 12 2026,
step 5 of Keith's own Master IDE build order - "the real, high-risk
core: reassign IDs and propagate to IPL/2DFX", built on the already-
proven collision checks and backup system from earlier steps).

Shifts a contiguous block of real object IDs [min_id, max_id] by a
fixed offset. Cascades automatically into 2dfx entries sharing those
IDs (same in-memory objects_by_section data Master IDE already
loads) and into every given real IPL file's own "inst"/"cars" lines
that reference them - by substituting ONLY the leading ID field on
a matching line, leaving every other byte of that line untouched.
Collision-checked against every ID OUTSIDE the moved block before
anything is written - all-or-nothing, no partial shift."""

##Methods list -
# IDShiftPlan
# plan_id_shift
# apply_id_shift
# apply_id_shift_and_write
# cascade_ipl_files
# cascade_ipl_rename
# cascade_2dfx_sections
# cascade_delete_2dfx
# find_ipl_usages
# remove_ipl_lines
# cascade_path_ids
# cascade_path_rename
# find_path_usages
# remove_path_blocks
# plan_add_ids
# apply_add_ids
# FreeIdCollapsePlan
# plan_collapse_free_ids
# apply_collapse_free_ids
# plan_delete_and_collapse
# apply_delete_and_collapse
# InsertRelocationPlan
# plan_insert_relocation
# apply_insert_relocation
# RenameRangePlan
# plan_prefix_suffix_rename
# apply_prefix_suffix_rename
# find_free_id_gaps
# plan_compact_all_gaps
# plan_swap_ids
# find_usages
# preview_cascade
# plan_splice_move
# validate_contiguous_selection

import os
from dataclasses import dataclass, field
from typing import Dict, List

from apps.methods.file_backup import backup_file

_IPL_ID_SECTIONS = ("inst", "cars")

# Every section that declares a real model_id sharing the global ID
# space - NOT just objs/tobj (Sep 12 2026, real bug caught by
# Keith's own worked example: an anim-section entry, e.g. SFs.ide's
# real "792,vgegassgn01_lvs,vgegassign,vegasE,150,128", was
# completely invisible to every shift/collision check here, so a
# shift could both silently miss moving it AND silently create a
# real ID collision against it without ever reporting a conflict).
_ID_DECLARING_SECTIONS = ("objs", "tobj", "anim", "hier", "cars", "peds", "weap")

# Of those, only the sections this app can actually re-serialize a
# changed model_id back to disk for (write_source_file/write_master_
# ide have a real verified formatter for objs/tobj/anim only - see
# master_ide.py's own _format_objs_or_tobj_line/_format_anim_line;
# hier/cars/peds/weap still go through raw-text passthrough, so
# their own real model_id can never actually change on disk yet).
_MOVABLE_SECTIONS = ("objs", "tobj", "anim")


def _find_unmovable_in_range(result, min_id: int, max_id: int): #vers 1
    """Real entries within [min_id, max_id] that declare a real ID
    but live in a section this app can't yet re-serialize a changed
    ID for (hier/cars/peds/weap) - moving everything AROUND one of
    these while leaving it in place would either strand it on a
    stale ID or collide with whatever the shift moves into its
    slot. Returns a list of (model_id, model_name, source_ide,
    section) - callers should refuse the whole plan if this is
    non-empty rather than silently leaving it unmigrated."""
    blockers = []
    for section in _ID_DECLARING_SECTIONS:
        if section in _MOVABLE_SECTIONS:
            continue
        for obj in result.objects_by_section.get(section, []):
            if min_id <= obj.model_id <= max_id:
                blockers.append((obj.model_id, obj.model_name, obj.source_ide, section))
    return blockers


@dataclass
class IDShiftPlan: #vers 1
    moved: list = field(default_factory=list)        # (old_id, new_id, model_name, source_ide)
    conflicts: list = field(default_factory=list)     # (new_id, conflicting_name, conflicting_source)
    id_map: Dict[int, int] = field(default_factory=dict)   # old_id -> new_id, moved entries only

    @property
    def ok(self): #vers 1
        return bool(self.moved) and not self.conflicts


def plan_id_shift(result, min_id: int, max_id: int, offset: int) -> IDShiftPlan: #vers 2
    """Dry run only - computes what WOULD move and any real conflicts,
    touches nothing. A conflict is a moved entry's new_id landing on
    a real ID that stays in place (outside the moved block), OR a
    real entry inside the range belonging to a section this app
    can't yet re-serialize (see _find_unmovable_in_range) - either
    way the whole plan is rejected (plan.ok is False), matching
    Keith's own "no fallback code - works or doesn't". Collision
    detection scans every real ID-declaring section (not just objs/
    tobj), so a shift can never land on top of a real anim/hier/
    cars/peds/weap entry without being flagged."""
    plan = IDShiftPlan()
    if offset == 0 or min_id > max_id:
        return plan

    all_ids: Dict[int, tuple] = {}   # id -> (model_name, source_ide)
    for section in _ID_DECLARING_SECTIONS:
        for obj in result.objects_by_section.get(section, []):
            all_ids[obj.model_id] = (obj.model_name, obj.source_ide)

    stationary_ids = {i: v for i, v in all_ids.items() if not (min_id <= i <= max_id)}

    for section in _MOVABLE_SECTIONS:
        for obj in result.objects_by_section.get(section, []):
            if min_id <= obj.model_id <= max_id:
                new_id = obj.model_id + offset
                plan.moved.append((obj.model_id, new_id, obj.model_name, obj.source_ide))
                plan.id_map[obj.model_id] = new_id

    for old_id, new_id, model_name, source_ide in plan.moved:
        if new_id < 0:
            plan.conflicts.append((new_id, f"{model_name} (negative ID)", source_ide))
        elif new_id in stationary_ids:
            conf_name, conf_source = stationary_ids[new_id]
            plan.conflicts.append((new_id, conf_name, conf_source))

    for model_id, model_name, source_ide, section in _find_unmovable_in_range(result, min_id, max_id):
        plan.conflicts.append((model_id, f"{model_name} (in '{section}' section - "
                                          f"can't be re-written yet)", source_ide))

    return plan


def apply_id_shift(result, plan: IDShiftPlan) -> List[str]: #vers 3
    """Apply an already-planned, conflict-free shift in memory:
    objs/tobj/anim entries get their real model_id updated (the 3
    sections this app can actually re-serialize - see _MOVABLE_
    SECTIONS); 2dfx entries sharing an old_id (same real model_id,
    see IDEParser's own 2dfx-stub docstring) get remapped alongside
    their base object, including their synthetic "2dfx_<id>" display
    name. Returns the list of real basenames now needing write_
    source_file(). Refuses (returns []) if plan.ok is False, or if
    plan.id_map touches any real entry in a non-movable section
    (hier/cars/peds/weap) - a defensive backstop in case a caller
    built id_map some other way than plan_id_shift's own checks."""
    if not plan.ok:
        return []
    for old_id in plan.id_map:
        for section in _ID_DECLARING_SECTIONS:
            if section in _MOVABLE_SECTIONS:
                continue
            if any(o.model_id == old_id for o in result.objects_by_section.get(section, [])):
                return []

    touched = set()
    for section in _MOVABLE_SECTIONS:
        for obj in result.objects_by_section.get(section, []):
            if obj.model_id in plan.id_map:
                obj.model_id = plan.id_map[obj.model_id]
                touched.add(os.path.basename(obj.source_ide))
        if section in result.objects_by_section:
            result.objects_by_section[section].sort(key=lambda o: o.model_id)

    for obj in result.objects_by_section.get("2dfx", []):
        if obj.model_id in plan.id_map:
            new_id = plan.id_map[obj.model_id]
            obj.model_id = new_id
            if obj.model_name.startswith("2dfx_"):
                obj.model_name = f"2dfx_{new_id}"
            touched.add(os.path.basename(obj.source_ide))
    if "2dfx" in result.objects_by_section:
        result.objects_by_section["2dfx"].sort(key=lambda o: o.model_id)

    return sorted(touched)


def _remap_id_field_line(line: str, id_map: Dict[int, int]): #vers 1
    """If this real line's own leading field is an ID in id_map,
    return the line with ONLY that field substituted - every other
    byte (commas, spacing, all remaining fields) untouched. Returns
    None if this line doesn't start with a mapped ID at all."""
    stripped = line.strip()
    if not stripped or "," not in stripped:
        return None
    first, rest = stripped.split(",", 1)
    try:
        old_id = int(first.strip())
    except ValueError:
        return None
    if old_id not in id_map:
        return None
    # Preserve the line's own real leading whitespace and the exact
    # comma/space style already used after the ID field.
    prefix_ws = line[:len(line) - len(line.lstrip())]
    sep = "," + rest[:len(rest) - len(rest.lstrip())]
    return f"{prefix_ws}{id_map[old_id]}{sep}{rest.lstrip()}"


def _keep_eol(raw: str, new_line: str) -> str: #vers 1
    """new_line with the original line's own ending (CRLF / LF / none), so
    a cascade never converts a file's line endings."""
    return new_line.rstrip("\r\n") + raw[len(raw.rstrip("\r\n")):]


def _remap_section_ids_in_file(file_path: str, section_names, id_map: Dict[int, int]) -> bool: #vers 1
    """Shared low-level rewrite - given any real text file (.ipl or
    .ide both work), substitute ONLY the leading ID field on lines
    inside any of the given section_names that match id_map, byte-
    for-byte otherwise. Backs up first. Returns True only if
    something actually changed and the write succeeded; False for
    "nothing matched" (not an error) or any real failure."""
    if not file_path or not os.path.isfile(file_path):
        return False
    try:
        with open(file_path, "r", encoding="latin-1", newline="") as f:
            lines = f.readlines()
    except Exception:
        return False

    current_section = None
    changed = False
    out_lines = []
    for raw in lines:
        stripped = raw.split("#")[0].strip()
        low = stripped.lower()
        if low == "end":
            current_section = None
            out_lines.append(raw)
            continue
        if current_section is None and low in section_names:
            current_section = low
            out_lines.append(raw)
            continue
        if current_section in section_names:
            remapped = _remap_id_field_line(raw, id_map)
            if remapped is not None:
                out_lines.append(_keep_eol(raw, remapped))
                changed = True
                continue
        out_lines.append(raw)

    if not changed:
        return False
    if backup_file(file_path) is None:
        return False
    try:
        with open(file_path, "w", encoding="latin-1", newline="") as f:
            f.writelines(out_lines)
        return True
    except Exception:
        return False


def cascade_ipl_files(ipl_paths: List[str], id_map: Dict[int, int]) -> Dict[str, bool]: #vers 2
    """Rewrite every given real IPL file's own "inst"/"cars" section
    lines whose leading ID field is in id_map - substituting only
    that field, backing up each real file first. Returns a dict of
    ipl_path -> True/False (False = no matching lines found, file
    unchanged, no backup made - not an error)."""
    return {p: _remap_section_ids_in_file(p, _IPL_ID_SECTIONS, id_map) for p in ipl_paths}


def _remap_name_field_line(line: str, model_id: int, old_name: str, new_name: str): #vers 1
    """If this real line's leading field is model_id AND its second
    field matches old_name (case-insensitive), return the line with
    ONLY the name field substituted - every other byte untouched.
    Returns None if this line doesn't match both the ID and the old
    name (a renamed entry's own numeric ID never changes, so an ID
    match alone isn't enough to tell a real placement of THIS model
    from an unrelated one that happens to share the checked ID)."""
    stripped = line.strip()
    if not stripped or stripped.count(",") < 1:
        return None
    parts = stripped.split(",", 2)
    if len(parts) < 2:
        return None
    try:
        line_id = int(parts[0].strip())
    except ValueError:
        return None
    if line_id != model_id or parts[1].strip().lower() != old_name.strip().lower():
        return None
    prefix_ws = line[:len(line) - len(line.lstrip())]
    id_field = parts[0]
    name_ws = parts[1][:len(parts[1]) - len(parts[1].lstrip())]
    rest = parts[2] if len(parts) > 2 else ""
    return f"{prefix_ws}{id_field},{name_ws}{new_name},{rest}" if rest else \
           f"{prefix_ws}{id_field},{name_ws}{new_name}"


def _remap_names_in_file(file_path: str, section_names, model_id: int,
                          old_name: str, new_name: str) -> bool: #vers 1
    """Same shared scan as _remap_section_ids_in_file, substituting
    only the model-name field (via _remap_name_field_line) instead
    of the ID field. Backs up first. Returns True only if something
    actually changed and the write succeeded."""
    if not file_path or not os.path.isfile(file_path):
        return False
    try:
        with open(file_path, "r", encoding="latin-1", newline="") as f:
            lines = f.readlines()
    except Exception:
        return False

    current_section = None
    changed = False
    out_lines = []
    for raw in lines:
        stripped = raw.split("#")[0].strip()
        low = stripped.lower()
        if low == "end":
            current_section = None
            out_lines.append(raw)
            continue
        if current_section is None and low in section_names:
            current_section = low
            out_lines.append(raw)
            continue
        if current_section in section_names:
            remapped = _remap_name_field_line(raw, model_id, old_name, new_name)
            if remapped is not None:
                out_lines.append(_keep_eol(raw, remapped))
                changed = True
                continue
        out_lines.append(raw)

    if not changed:
        return False
    if backup_file(file_path) is None:
        return False
    try:
        with open(file_path, "w", encoding="latin-1", newline="") as f:
            f.writelines(out_lines)
        return True
    except Exception:
        return False


def cascade_ipl_rename(ipl_paths: List[str], model_id: int, old_name: str,
                        new_name: str) -> Dict[str, bool]: #vers 1
    """Rewrite every given real IPL file's own "inst"/"cars" lines
    whose ID+name match this renamed entry, substituting only the
    model-name field. A rename never changes the numeric ID, so
    unlike cascade_ipl_files this is keyed on ID+old-name together,
    not id_map. Returns ipl_path -> True/False (False = no matching
    lines found, not an error)."""
    return {p: _remap_names_in_file(p, _IPL_ID_SECTIONS, model_id, old_name, new_name)
            for p in ipl_paths}


def _remove_id_lines_in_file(file_path: str, section_names, model_ids) -> bool: #vers 1
    """Shared scan - drop every line inside the given section_names
    whose leading ID field is in model_ids, backing up first. Same
    section-tracking approach as _remap_section_ids_in_file. Returns
    True only if a line was actually dropped and the write
    succeeded."""
    if not file_path or not os.path.isfile(file_path):
        return False
    try:
        with open(file_path, "r", encoding="latin-1", newline="") as f:
            lines = f.readlines()
    except Exception:
        return False

    current_section = None
    changed = False
    out_lines = []
    for raw in lines:
        stripped = raw.split("#")[0].strip()
        low = stripped.lower()
        if low == "end":
            current_section = None
            out_lines.append(raw)
            continue
        if current_section is None and low in section_names:
            current_section = low
            out_lines.append(raw)
            continue
        if current_section in section_names and stripped:
            first = stripped.split(",", 1)[0].strip()
            try:
                line_id = int(first)
            except ValueError:
                line_id = None
            if line_id is not None and line_id in model_ids:
                changed = True
                continue
        out_lines.append(raw)

    if not changed:
        return False
    if backup_file(file_path) is None:
        return False
    try:
        with open(file_path, "w", encoding="latin-1", newline="") as f:
            f.writelines(out_lines)
        return True
    except Exception:
        return False


def cascade_delete_2dfx(ide_paths: List[str], model_ids) -> Dict[str, bool]: #vers 1
    """Remove every real 2dfx line attached to a deleted model_id -
    a deleted objs/tobj entry leaves its own 2dfx effects (coronas,
    lights, etc, keyed on the same numeric ID) orphaned otherwise.
    model_ids may be a single int or any collection of ints. Returns
    ide_path -> True/False (False = nothing matched, not an error)."""
    ids = {model_ids} if isinstance(model_ids, int) else set(model_ids)
    return {p: _remove_id_lines_in_file(p, ("2dfx",), ids) for p in ide_paths}


def find_ipl_usages(ipl_paths: List[str], model_id: int) -> List[dict]: #vers 1
    """Read-only scan (no write, no backup) of every given real IPL
    file's own "inst"/"cars" lines for a real placement of model_id -
    used to warn before Remove/Delete ID leaves these orphaned.
    Returns a list of {ipl_path, line_no, model_name}."""
    usages = []
    for path in ipl_paths:
        if not path or not os.path.isfile(path):
            continue
        try:
            with open(path, "r", encoding="latin-1", newline="") as f:
                lines = f.readlines()
        except Exception:
            continue
        current_section = None
        for line_no, raw in enumerate(lines, start=1):
            stripped = raw.split("#")[0].strip()
            low = stripped.lower()
            if low == "end":
                current_section = None
                continue
            if current_section is None and low in _IPL_ID_SECTIONS:
                current_section = low
                continue
            if current_section in _IPL_ID_SECTIONS and stripped:
                parts = stripped.split(",", 2)
                try:
                    line_id = int(parts[0].strip())
                except (ValueError, IndexError):
                    continue
                if line_id == model_id:
                    usages.append({
                        'ipl_path': path, 'line_no': line_no,
                        'model_name': parts[1].strip() if len(parts) > 1 else '',
                    })
    return usages


def remove_ipl_lines(ipl_paths: List[str], model_ids) -> Dict[str, bool]: #vers 1
    """Remove every real "inst"/"cars" line placing a deleted
    model_id - offered after find_ipl_usages warns the user which
    files/lines would be affected. model_ids may be a single int or
    any collection of ints. Returns ipl_path -> True/False (False =
    nothing matched, not an error)."""
    ids = {model_ids} if isinstance(model_ids, int) else set(model_ids)
    return {p: _remove_id_lines_in_file(p, _IPL_ID_SECTIONS, ids) for p in ipl_paths}


def cascade_2dfx_sections(ide_paths: List[str], id_map: Dict[int, int]) -> Dict[str, bool]: #vers 1
    """Real bug fix (Sep 12 2026, per Keith's own worked example) -
    apply_id_shift already updates 2dfx's IN-MEMORY parsed model_id,
    but write_source_file/write_master_ide always write 2dfx from
    RAW TEXT passthrough (deliberately, to avoid the original real
    2dfx-corruption bug - see load_master_ide's own docstring on
    that), meaning that in-memory update was silently discarded at
    write time every single time a 2dfx cascade ran, for every real
    shift this app has ever performed. Same real technique as
    cascade_ipl_files, applied to a real IDE file's own "2dfx"
    section instead of an IPL's "inst"/"cars" - substitutes only the
    leading ID field, every other byte (including the quoted corona
    strings) stays exactly as it was. Must be called AFTER write_
    source_file has already written the objs/tobj/anim part for
    each file, so it operates on the just-written real file, not a
    stale copy."""
    return {p: _remap_section_ids_in_file(p, ("2dfx",), id_map) for p in ide_paths}


# GTA III/VC's own IDE "path" section is genuinely shaped differently
# from every other section here (Sep 17 2026, per Keith's own real
# comnbtm.ide example: "car, 2084, custom_rd4_ug" then N indented
# numeric node rows, no per-block "end") - the header's ID is its
# SECOND field, not the first, so none of the leading-field helpers
# above ever match it at all (they safely no-op on it, never crash,
# but also never cascade it). GTA SA doesn't use this IDE path-header
# form - its own path nodes live in the IPL's own "path" section as
# plain numeric rows with no ID/name header - so this is GTA III/VC-
# only and deliberately never touches SA's "path" section.
import re
_PATH_HEADER_RE = re.compile(
    r'^(?P<indent>[ \t]*)(?P<kind>car|ped)(?P<sep1>[ \t]*,[ \t]*)'
    r'(?P<id>-?\d+)(?P<sep2>[ \t]*,[ \t]*)(?P<name>.*)$', re.IGNORECASE)


def _remap_path_header_id_line(line: str, id_map: Dict[int, int]): #vers 1
    """If this real "path" section header line's own ID field (2nd
    position - "car"/"ped", ID, ModelName) is in id_map, return the
    line with ONLY that field substituted. Returns None otherwise."""
    m = _PATH_HEADER_RE.match(line.rstrip("\r\n"))
    if not m:
        return None
    old_id = int(m.group("id"))
    if old_id not in id_map:
        return None
    return (f"{m.group('indent')}{m.group('kind')}{m.group('sep1')}"
            f"{id_map[old_id]}{m.group('sep2')}{m.group('name')}")


def _remap_path_header_name_line(line: str, model_id: int, old_name: str, new_name: str): #vers 1
    """Same as _remap_path_header_id_line but for a rename - matches
    on ID+old-name together (a rename never changes the ID), only
    substituting the ModelName field."""
    m = _PATH_HEADER_RE.match(line.rstrip("\r\n"))
    if not m:
        return None
    if int(m.group("id")) != model_id or m.group("name").strip().lower() != old_name.strip().lower():
        return None
    return (f"{m.group('indent')}{m.group('kind')}{m.group('sep1')}"
            f"{m.group('id')}{m.group('sep2')}{new_name}")


def _scan_path_section(file_path: str, line_fn): #vers 1
    """Shared scan of a real IDE file's own "path" section only -
    calls line_fn(raw_line) for every line inside that section
    (header AND indented node rows both passed through; line_fn
    returns a replacement string or None to keep it unchanged), and
    writes back + backs up first if anything actually changed.
    Returns True/False the same way every other cascade helper here
    does."""
    if not file_path or not os.path.isfile(file_path):
        return False
    try:
        with open(file_path, "r", encoding="latin-1", newline="") as f:
            lines = f.readlines()
    except Exception:
        return False

    in_path_section = False
    changed = False
    out_lines = []
    for raw in lines:
        stripped = raw.split("#")[0].strip()
        low = stripped.lower()
        if not in_path_section:
            if low == "path":
                in_path_section = True
            out_lines.append(raw)
            continue
        if low == "end":
            in_path_section = False
            out_lines.append(raw)
            continue
        remapped = line_fn(raw)
        if remapped is not None:
            out_lines.append(_keep_eol(raw, remapped))
            changed = True
        else:
            out_lines.append(raw)

    if not changed:
        return False
    if backup_file(file_path) is None:
        return False
    try:
        with open(file_path, "w", encoding="latin-1", newline="") as f:
            f.writelines(out_lines)
        return True
    except Exception:
        return False


def cascade_path_ids(ide_paths: List[str], id_map: Dict[int, int]) -> Dict[str, bool]: #vers 1
    """Rewrite every real "path" section header's own ID field
    (GTA III/VC only - see the module note above) for every real ID
    in id_map. Indented node rows never carry a model ID, so they're
    always left untouched."""
    return {p: _scan_path_section(p, lambda line: _remap_path_header_id_line(line, id_map))
            for p in ide_paths}


def cascade_path_rename(ide_paths: List[str], model_id: int, old_name: str,
                         new_name: str) -> Dict[str, bool]: #vers 1
    """Rewrite every real "path" section header's own ModelName
    field for a renamed entry (GTA III/VC only)."""
    return {p: _scan_path_section(
                p, lambda line: _remap_path_header_name_line(line, model_id, old_name, new_name))
            for p in ide_paths}


def find_path_usages(ide_paths: List[str], model_id: int) -> List[dict]: #vers 1
    """Read-only scan for real "path" section headers referencing
    model_id (GTA III/VC only) - used to warn before Remove/Delete
    ID leaves an orphaned path block. Returns a list of {ide_path,
    line_no, model_name}."""
    usages = []
    for path in ide_paths:
        if not path or not os.path.isfile(path):
            continue
        try:
            with open(path, "r", encoding="latin-1", newline="") as f:
                lines = f.readlines()
        except Exception:
            continue
        in_path_section = False
        for line_no, raw in enumerate(lines, start=1):
            stripped = raw.split("#")[0].strip()
            low = stripped.lower()
            if not in_path_section:
                if low == "path":
                    in_path_section = True
                continue
            if low == "end":
                in_path_section = False
                continue
            m = _PATH_HEADER_RE.match(raw.rstrip("\r\n"))
            if m and int(m.group("id")) == model_id:
                usages.append({'ide_path': path, 'line_no': line_no, 'model_name': m.group("name").strip()})
    return usages


def remove_path_blocks(ide_paths: List[str], model_ids) -> Dict[str, bool]: #vers 1
    """Remove every real "path" block (header + all its indented
    node rows) for a deleted model_id (GTA III/VC only) - offered
    after find_path_usages warns the user. model_ids may be a
    single int or any collection of ints."""
    ids = {model_ids} if isinstance(model_ids, int) else set(model_ids)

    def _process(file_path: str) -> bool:
        if not file_path or not os.path.isfile(file_path):
            return False
        try:
            with open(file_path, "r", encoding="latin-1", newline="") as f:
                lines = f.readlines()
        except Exception:
            return False

        in_path_section = False
        skipping = False
        changed = False
        out_lines = []
        for raw in lines:
            stripped = raw.split("#")[0].strip()
            low = stripped.lower()
            if not in_path_section:
                if low == "path":
                    in_path_section = True
                out_lines.append(raw)
                continue
            if low == "end":
                in_path_section = False
                skipping = False
                out_lines.append(raw)
                continue
            m = _PATH_HEADER_RE.match(raw.rstrip("\r\n"))
            if m:
                skipping = int(m.group("id")) in ids
                if skipping:
                    changed = True
                    continue
            elif skipping:
                changed = True
                continue
            out_lines.append(raw)

        if not changed:
            return False
        if backup_file(file_path) is None:
            return False
        try:
            with open(file_path, "w", encoding="latin-1", newline="") as f:
                f.writelines(out_lines)
            return True
        except Exception:
            return False

    return {p: _process(p) for p in ide_paths}


def apply_id_shift_and_write(result, plan: IDShiftPlan) -> List[str]: #vers 2
    """The real, complete apply step every caller should use instead
    of doing apply_id_shift() + its own write_source_file() loop by
    hand (Sep 12 2026 - centralizing this exact sequence is what
    catches the 2dfx write-back bug uniformly everywhere, rather
    than needing every dialog's own call site fixed separately).
    Applies the shift in memory, writes every real touched IDE file
    (objs/tobj/anim, from parsed data), THEN cascades into each of
    those same files' own 2dfx AND path sections (surgical
    substitution on the just-written file; path is GTA III/VC only,
    a no-op elsewhere). Returns the touched real basenames, or []
    on any failure - never partially applies."""
    from apps.methods.master_ide_edit import write_source_file

    touched = apply_id_shift(result, plan)
    if not touched:
        return []
    failures = []
    for basename in touched:
        source_path = next((p for p in result.source_files
                             if os.path.basename(p) == basename), None)
        if not source_path or not write_source_file(result, source_path):
            failures.append(basename)
    if failures:
        return []

    # Scan EVERY loaded file, not just the ones that got an objs/
    # tobj/anim write (Sep 17 2026, real gap found against SOL's own
    # files: game_lc.ide has no 2dfx section of its own at all - its
    # models' 2dfx/lighting effects live entirely in a separate
    # loaded file, GAME_LC.IFX, which "touched" would never include
    # since it was never itself written to for the objs/tobj change.
    # Cheap and always safe either way - every cascade helper here
    # is already a no-op on a file with no matching lines).
    cascade_2dfx_sections(result.source_files, plan.id_map)
    cascade_path_ids(result.source_files, plan.id_map)
    return touched


def _max_used_id(result) -> int: #vers 2
    """Highest real declared ID currently loaded, across every real
    ID-declaring section (not just objs/tobj - Sep 12 2026, same
    real bug fix as plan_id_shift's own broadened scan) - same
    convention as id_shift_dialog.py's own "To highest loaded ID"
    button."""
    all_ids = [obj.model_id for section in _ID_DECLARING_SECTIONS
               for obj in result.objects_by_section.get(section, [])]
    return max(all_ids) if all_ids else -1


def plan_add_ids(result, after_id: int, count: int) -> IDShiftPlan: #vers 1
    """Reserve count new free ID slots right after after_id, by
    shifting everything with a real ID > after_id up by count (Sep
    12 2026, per Keith: "Add would create xN of ID's from the
    selected line... shift everything after by 1000+"). Pure re-use
    of plan_id_shift - adding IDs is just a shift with nothing new
    written. If nothing real exists above after_id, there is
    nothing to shift and the reserved space already exists trivially
    (plan.moved stays empty - not a conflict, not a failure)."""
    max_id = _max_used_id(result)
    if max_id <= after_id:
        return IDShiftPlan()   # trivially satisfied - nothing above after_id to move
    return plan_id_shift(result, after_id + 1, max_id, count)


def apply_add_ids(result, plan: IDShiftPlan) -> List[str]: #vers 2
    """Apply a plan_add_ids() plan - writes every real touched file
    and cascades 2dfx itself (Sep 12 2026, uses apply_id_shift_and_
    write so callers never need to remember the 2dfx step). A plan
    with nothing moved (the trivial "nothing above after_id" case)
    is a valid no-op success, not a refusal - apply_id_shift itself
    would treat empty-moved as not-ok, so that case is handled here
    instead."""
    if not plan.moved:
        return [] if not plan.conflicts else None
    if not plan.ok:
        return None
    return apply_id_shift_and_write(result, plan)


@dataclass
class FreeIdCollapsePlan: #vers 1
    start_id: int = 0
    requested_count: int = 0
    free_ids_found: list = field(default_factory=list)   # contiguous, in order
    blocking_id: int = None
    blocking_name: str = None
    blocking_source: str = None

    @property
    def fully_free(self): #vers 1
        return self.blocking_id is None and len(self.free_ids_found) == self.requested_count


def plan_collapse_free_ids(result, start_id: int, count: int) -> FreeIdCollapsePlan: #vers 2
    """Dry run only - scans forward from start_id counting real free
    (unassigned) IDs until either count are found, or a real assigned
    ID is hit first (Sep 12 2026, per Keith: "if there are 5 free
    ID's and I delete 6 of them, it won't delete what isn't free...
    If the 6th is assigned I get a warning"). Scans every real ID-
    declaring section (not just objs/tobj) so a hier/cars/peds/weap-
    occupied slot is never wrongly treated as free. plan.fully_free
    is True only when every one of the count IDs scanned was
    genuinely free - the only case apply_collapse_free_ids will act
    on."""
    used = {obj.model_id: obj for section in _ID_DECLARING_SECTIONS
            for obj in result.objects_by_section.get(section, [])}
    plan = FreeIdCollapsePlan(start_id=start_id, requested_count=count)
    cur = start_id
    while len(plan.free_ids_found) < count:
        if cur in used:
            obj = used[cur]
            plan.blocking_id = cur
            plan.blocking_name = obj.model_name
            plan.blocking_source = obj.source_ide
            return plan
        plan.free_ids_found.append(cur)
        cur += 1
    return plan


def apply_collapse_free_ids(result, plan: FreeIdCollapsePlan): #vers 3
    """Apply an already-planned free-ID collapse: shift everything
    above the scanned free range DOWN by requested_count, closing
    the gap - writes every real touched file and cascades 2dfx/path
    itself (Sep 12 2026, uses apply_id_shift_and_write). Refuses
    (returns None) unless plan.fully_free - never partially
    collapses, and never touches a real assigned entry (that's
    Delete ID's job, a separate, explicit operation). Returns
    (touched_basenames, id_map) - the caller still owns any real IPL
    cascade (cascade_ipl_files(ipl_paths, id_map)), same as every
    other operation here that can't reach a real .ipl without a
    dat_path."""
    if not plan.fully_free:
        return None
    collapse_from = plan.free_ids_found[-1] + 1
    max_id = _max_used_id(result)
    if collapse_from > max_id:
        return [], {}   # nothing above the freed range - already a clean no-op success
    shift_plan = plan_id_shift(result, collapse_from, max_id, -plan.requested_count)
    if not shift_plan.ok:
        return None
    touched = apply_id_shift_and_write(result, shift_plan)
    return touched, shift_plan.id_map


def plan_delete_and_collapse(result, start_id: int, count: int) -> List[tuple]: #vers 2
    """Dry run only - every real assigned entry within [start_id,
    start_id+count-1] that would actually be deleted (Sep 12 2026,
    per Keith: "until i want to remove lines altogether, removing
    those models" - the explicit escalation past a free-ID collapse
    refusal). Scans every real ID-declaring section so a hier/cars/
    peds/weap entry is correctly identified as occupying its slot -
    remove_entry() itself will then honestly refuse to delete it
    (objs/tobj only), which is the correct outcome here: apply_
    delete_and_collapse stops rather than silently treating that
    slot as cleared. Returns a list of (model_id, model_name,
    source_ide); empty means every ID in range was already free."""
    used = {obj.model_id: obj for section in _ID_DECLARING_SECTIONS
            for obj in result.objects_by_section.get(section, [])}
    to_delete = []
    for i in range(start_id, start_id + count):
        if i in used:
            obj = used[i]
            to_delete.append((i, obj.model_name, obj.source_ide))
    return to_delete


def apply_delete_and_collapse(result, start_id: int, count: int, to_delete: List[tuple]): #vers 3
    """Actually remove every real to_delete entry (writing each
    touched file immediately, then cascading 2dfx/path deletion into
    those same just-written files - Sep 17 2026, per Keith: "when
    changing ID's in the IDE, other entries need to be accounted
    for" - this delete path used to bypass that entirely), then
    collapse the now-fully-free [start_id, start_id+count-1] range by
    shifting everything above it down by count - writing that too,
    and cascading 2dfx/path itself (Sep 12 2026, uses apply_id_shift_
    and_write). Stops and refuses (returns None) on the first real
    removal or write failure rather than partially applying. Returns
    (removed_ide_basenames, shift_touched_basenames, shift_id_map,
    deleted_model_ids) on success - the caller still owns any real
    IPL cascade (find_ipl_usages/remove_ipl_lines for deleted_model_
    ids, cascade_ipl_files for shift_id_map), same as every other
    operation here that can't reach a real .ipl without a dat_path."""
    from apps.methods.master_ide_edit import remove_entry, write_source_file

    removed_files = set()
    deleted_ids = set()
    for model_id, _model_name, source_ide in to_delete:
        err = remove_entry(result, model_id, source_ide)
        if err:
            return None
        removed_files.add(os.path.basename(source_ide))
        deleted_ids.add(model_id)

    for basename in removed_files:
        source_path = next((p for p in result.source_files
                             if os.path.basename(p) == basename), None)
        if not source_path or not write_source_file(result, source_path):
            return None

    if deleted_ids:
        # Every loaded file, not just the ones written for the
        # objs/tobj removal itself - same real gap as apply_id_
        # shift_and_write's own fix (a deleted model's 2dfx effects
        # can live in a separate loaded file, e.g. SOL's GAME_LC.IFX).
        cascade_delete_2dfx(result.source_files, deleted_ids)
        remove_path_blocks(result.source_files, deleted_ids)

    max_id = _max_used_id(result)
    collapse_from = start_id + count
    if collapse_from > max_id:
        return sorted(removed_files), [], {}, deleted_ids
    shift_plan = plan_id_shift(result, collapse_from, max_id, -count)
    if not shift_plan.ok:
        return None
    shift_touched = apply_id_shift_and_write(result, shift_plan)
    return sorted(removed_files), shift_touched, shift_plan.id_map, deleted_ids


@dataclass
class InsertRelocationPlan: #vers 1
    id_map: Dict[int, int] = field(default_factory=dict)     # incoming file's old_id -> new target id
    moved: list = field(default_factory=list)                # (old_id, new_id, model_name, source_ide) - incoming entries only
    id_conflicts: list = field(default_factory=list)          # (new_id, conflicting_name, conflicting_source) - target slot already used
    name_conflicts: list = field(default_factory=list)        # (incoming_name, incoming_old_id, existing_id, existing_source)

    @property
    def ok(self): #vers 1
        return bool(self.moved) and not self.id_conflicts and not self.name_conflicts


def plan_insert_relocation(result, incoming_source_ide: str, target_start_id: int) -> InsertRelocationPlan: #vers 1
    """Dry run only - relocate one already-loaded file's own real
    entries (typically just-added via Insert IDE File, possibly from
    an entirely different game, see this module's own docstring) onto
    a contiguous target range starting at target_start_id, in their
    own existing relative order. Two separate real checks, both
    required for plan.ok: (1) the target ID isn't already used by
    anything outside the incoming file, and (2) the incoming entry's
    own model NAME doesn't already exist anywhere else in the merge -
    a real risk when relocating a foreign file, since two different
    games' own IDE files can easily reuse common names (e.g.
    "generic1") even once their numeric IDs no longer clash (Sep 12
    2026, per Keith: "insert ID, moves other ide files into that
    area... would need a model name check")."""
    incoming_base = os.path.basename(incoming_source_ide)
    incoming = []
    incoming_unmovable = []
    other_by_id = {}
    other_by_name = {}
    for section in _ID_DECLARING_SECTIONS:
        for obj in result.objects_by_section.get(section, []):
            if os.path.basename(obj.source_ide) == incoming_base:
                if section in _MOVABLE_SECTIONS:
                    incoming.append(obj)
                else:
                    incoming_unmovable.append(obj)
            else:
                other_by_id[obj.model_id] = (obj.model_name, obj.source_ide)
                other_by_name.setdefault(obj.model_name.lower(), []).append((obj.model_id, obj.source_ide))

    incoming.sort(key=lambda o: o.model_id)
    plan = InsertRelocationPlan()
    for i, obj in enumerate(incoming):
        new_id = target_start_id + i
        plan.id_map[obj.model_id] = new_id
        plan.moved.append((obj.model_id, new_id, obj.model_name, obj.source_ide))
        if new_id in other_by_id:
            conf_name, conf_source = other_by_id[new_id]
            plan.id_conflicts.append((new_id, conf_name, conf_source))
        for existing_id, existing_source in other_by_name.get(obj.model_name.lower(), []):
            plan.name_conflicts.append((obj.model_name, obj.model_id, existing_id, existing_source))
    for obj in incoming_unmovable:
        # Sep 12 2026, same real gap as plan_id_shift's own -
        # the incoming file may itself have hier/cars/peds/weap
        # entries this app can't yet re-serialize a moved ID for.
        plan.id_conflicts.append((obj.model_id, f"{obj.model_name} (in '{obj.section}' "
                                                  f"section - can't be relocated yet)", obj.source_ide))
    return plan


def apply_insert_relocation(result, plan: InsertRelocationPlan) -> List[str]: #vers 2
    """Apply an already-planned, conflict-free relocation - writes
    every real touched file and cascades 2dfx itself (Sep 12 2026,
    uses apply_id_shift_and_write). Refuses (returns []) unless
    plan.ok, matching every other operation in this module's own
    all-or-nothing rule."""
    if not plan.ok:
        return []
    shift_plan = IDShiftPlan(moved=list(plan.moved), conflicts=[], id_map=dict(plan.id_map))
    return apply_id_shift_and_write(result, shift_plan)


@dataclass
class RenameRangePlan: #vers 1
    renames: list = field(default_factory=list)          # (model_id, old_name, new_name, source_ide)
    name_conflicts: list = field(default_factory=list)    # (new_name, model_id, conflicting_id, conflicting_source)
    internal_duplicates: list = field(default_factory=list)   # (new_name, model_id_a, model_id_b) - two renamed entries landing on the same name

    @property
    def ok(self): #vers 1
        return bool(self.renames) and not self.name_conflicts and not self.internal_duplicates


def plan_prefix_suffix_rename(result, model_ids, prefix: str = "", suffix: str = "") -> RenameRangePlan: #vers 2
    """Dry run only - for every given real model_id (a contiguous
    range or an arbitrary selected list, either works), compute the
    new name after adding prefix/suffix and check it two ways (Sep
    12 2026, per Keith: "option to surfix or prefix an ID modelname
    range, selected"): against every OTHER entry's existing real
    name (name_conflicts), and against every OTHER entry in this
    SAME batch landing on the identical new name (internal_
    duplicates) - two originally-different names can collapse onto
    one after the same prefix/suffix is applied to both."""
    id_set = set(model_ids)
    all_by_id = {}
    all_by_name = {}
    for section in _ID_DECLARING_SECTIONS:
        for obj in result.objects_by_section.get(section, []):
            all_by_id[obj.model_id] = obj
            all_by_name.setdefault(obj.model_name.lower(), []).append((obj.model_id, obj.source_ide))

    plan = RenameRangePlan()
    new_names_seen = {}   # new_name.lower() -> first model_id in this batch that produced it
    for mid in sorted(id_set):
        obj = all_by_id.get(mid)
        if not obj:
            continue   # a free/unassigned id in the range - nothing to rename
        new_name = f"{prefix}{obj.model_name}{suffix}"
        plan.renames.append((mid, obj.model_name, new_name, obj.source_ide))

        key = new_name.lower()
        if key in new_names_seen and new_names_seen[key] != mid:
            plan.internal_duplicates.append((new_name, new_names_seen[key], mid))
        else:
            new_names_seen[key] = mid

        for existing_id, existing_source in all_by_name.get(key, []):
            if existing_id != mid and existing_id not in id_set:
                plan.name_conflicts.append((new_name, mid, existing_id, existing_source))
    return plan


def apply_prefix_suffix_rename(result, plan: RenameRangePlan) -> List[str]: #vers 1
    """Apply an already-planned, conflict-free batch rename. Stops
    and refuses (returns []) on the first real rename_entry()
    failure rather than partially applying - matches every other
    operation in this module's own all-or-nothing rule."""
    if not plan.ok:
        return []
    from apps.methods.master_ide_edit import rename_entry
    touched = set()
    for model_id, _old_name, new_name, source_ide in plan.renames:
        err = rename_entry(result, model_id, new_name, source_ide=source_ide)
        if err:
            return []
        touched.add(os.path.basename(source_ide))
    return sorted(touched)


def find_free_id_gaps(result, min_id: int, max_id: int) -> List[tuple]: #vers 2
    """Every real contiguous free (unassigned) ID range within
    [min_id, max_id] - the same real scan plan_collapse_free_ids
    already does, just reported instead of applied. Scans every
    real ID-declaring section so a hier/cars/peds/weap-occupied ID
    is never wrongly reported as free. Returns a list of (gap_start,
    gap_end) tuples, inclusive, in ascending order."""
    used_ids = {obj.model_id for section in _ID_DECLARING_SECTIONS
                for obj in result.objects_by_section.get(section, [])}
    gaps = []
    gap_start = None
    for i in range(min_id, max_id + 1):
        if i in used_ids:
            if gap_start is not None:
                gaps.append((gap_start, i - 1))
                gap_start = None
        elif gap_start is None:
            gap_start = i
    if gap_start is not None:
        gaps.append((gap_start, max_id))
    return gaps


def plan_compact_all_gaps(result, min_id: int, max_id: int) -> IDShiftPlan: #vers 2
    """Single holistic plan closing EVERY gap within [min_id, max_id]
    in one pass, instead of one Remove ID/plan_collapse_free_ids at
    a time (Sep 12 2026, per Keith's own "any other use cases"
    follow-up). Every used ID shifts down by however many free IDs
    exist below it within the range - provably conflict-free by
    construction PROVIDED every real occupied ID in range is
    actually movable; a real hier/cars/peds/weap entry in range
    would break that proof (it can't move, but its slot still
    counts as "used" for the free-count math), so the whole plan is
    refused if any real one is found (same as plan_id_shift's own
    _find_unmovable_in_range check)."""
    blockers = _find_unmovable_in_range(result, min_id, max_id)
    if blockers:
        plan = IDShiftPlan()
        for model_id, model_name, source_ide, section in blockers:
            plan.conflicts.append((model_id, f"{model_name} (in '{section}' section - "
                                              f"can't be re-written yet)", source_ide))
        return plan

    used_objs = [obj for section in _MOVABLE_SECTIONS
                 for obj in result.objects_by_section.get(section, [])
                 if min_id <= obj.model_id <= max_id]
    used_objs.sort(key=lambda o: o.model_id)

    plan = IDShiftPlan()
    free_count_so_far = 0
    prev = min_id - 1
    for obj in used_objs:
        free_count_so_far += (obj.model_id - prev - 1)
        new_id = obj.model_id - free_count_so_far
        if new_id != obj.model_id:
            plan.moved.append((obj.model_id, new_id, obj.model_name, obj.source_ide))
            plan.id_map[obj.model_id] = new_id
        prev = obj.model_id
    return plan


def plan_swap_ids(result, id_a: int, id_b: int) -> IDShiftPlan: #vers 2
    """Swap whatever real entries currently occupy id_a and id_b -
    both must already be assigned AND belong to a section this app
    can actually re-serialize (objs/tobj/anim) - this isn't a move-
    into-free-space operation, see plan_id_shift/plan_insert_
    relocation for that. Returns an empty, not-ok plan if either id
    isn't actually assigned, or belongs to a hier/cars/peds/weap
    entry this app can't yet move - never guesses which one the
    caller meant."""
    all_by_id = {}
    for section in _ID_DECLARING_SECTIONS:
        for obj in result.objects_by_section.get(section, []):
            all_by_id[obj.model_id] = (obj, section)
    if id_a not in all_by_id or id_b not in all_by_id or id_a == id_b:
        return IDShiftPlan()
    obj_a, section_a = all_by_id[id_a]
    obj_b, section_b = all_by_id[id_b]
    if section_a not in _MOVABLE_SECTIONS or section_b not in _MOVABLE_SECTIONS:
        return IDShiftPlan()
    return IDShiftPlan(
        moved=[(id_a, id_b, obj_a.model_name, obj_a.source_ide),
               (id_b, id_a, obj_b.model_name, obj_b.source_ide)],
        conflicts=[],
        id_map={id_a: id_b, id_b: id_a})


def find_usages(result, model_id: int = None, model_name: str = None) -> List[dict]: #vers 1
    """Every real place a given ID or model name appears, across
    EVERY section including 2dfx (unlike every other check in this
    module, 2dfx is deliberately included here - the whole point is
    showing whether a given ID has real 2dfx entries attached before
    you Delete ID or relocate it, Sep 12 2026, per Keith's own "any
    other use cases" follow-up). Pass model_id, model_name, or both
    (both narrows to entries matching either)."""
    matches = []
    for section, objs in result.objects_by_section.items():
        for obj in objs:
            id_match = model_id is not None and obj.model_id == model_id
            name_match = model_name is not None and obj.model_name.lower() == model_name.lower()
            if id_match or name_match:
                matches.append({
                    'section': section, 'model_id': obj.model_id, 'model_name': obj.model_name,
                    'txd_name': obj.txd_name, 'source_ide': os.path.basename(obj.source_ide),
                })
    return matches


def plan_splice_move(result, move_start: int, move_end: int, target_start: int) -> IDShiftPlan: #vers 1
    """Real drag-move semantics, pinned down against Keith's own
    real screenshots (moving IDs 12918-12927 to land at 12910): the
    ID column itself never changes shape - it's the real PAYLOAD
    (model name/txd/dist/flags) that moves between fixed slots.
    Moving a block to target_start displaces whatever currently
    occupies the space it sweeps through, shifting that displaced
    range by exactly the moved block's own size (N) to close the gap
    left behind and open the space now needed - the same "swap
    payloads between fixed slots" behaviour confirmed against the
    real before/after images (12910-12917's real occupants landed
    at 12920-12927 after the 10-entry block moved to 12910).

    Provably conflict-free by construction, same proof style as
    plan_compact_all_gaps: the moved block and the displaced range
    are adjacent and non-overlapping, and together their new
    positions cover EXACTLY the same combined span [min(move_start,
    target_start), max(move_end, target_start+N-1)] the two ranges
    covered before the move - just re-partitioned, nothing outside
    that span is ever touched. Returns an empty, not-ok plan if
    target_start falls inside [move_start, move_end] (nothing to
    do - the block would be "moving into itself")."""
    if move_start > move_end or move_start <= target_start <= move_end:
        return IDShiftPlan()

    n = move_end - move_start + 1
    id_map: Dict[int, int] = {}

    if target_start < move_start:
        # Moving to a lower ID - the real 12918-12927 -> 12910 case.
        # Displaced range: [target_start, move_start-1], shifts up by n.
        for old_id in range(move_start, move_end + 1):
            id_map[old_id] = old_id + (target_start - move_start)
        for old_id in range(target_start, move_start):
            id_map[old_id] = old_id + n
    else:
        # Moving to a higher ID - symmetric case.
        # Displaced range: [move_end+1, target_start+n-1], shifts down by n.
        for old_id in range(move_start, move_end + 1):
            id_map[old_id] = old_id + (target_start - move_start)
        for old_id in range(move_end + 1, target_start + n):
            id_map[old_id] = old_id - n

    all_by_id = {obj.model_id: obj for section in _MOVABLE_SECTIONS
                 for obj in result.objects_by_section.get(section, [])}
    plan = IDShiftPlan(id_map=id_map)
    for old_id, new_id in id_map.items():
        obj = all_by_id.get(old_id)
        if obj:
            plan.moved.append((old_id, new_id, obj.model_name, obj.source_ide))

    # Same real risk as plan_compact_all_gaps's own proof: the
    # conflict-free-by-construction guarantee only holds if every
    # real occupied ID in the combined swept span is actually
    # movable - a real hier/cars/peds/weap entry anywhere in that
    # span breaks it (Sep 12 2026, same fix as everywhere else in
    # this module).
    span_lo, span_hi = min(id_map), max(id_map)
    for model_id, model_name, source_ide, section in _find_unmovable_in_range(result, span_lo, span_hi):
        plan.conflicts.append((model_id, f"{model_name} (in '{section}' section - "
                                          f"can't be re-written yet)", source_ide))
    return plan


def validate_contiguous_selection(entry_rows: List[tuple], selected_indices) -> tuple: #vers 1
    """Pure logic, no Qt - given every real 'entry' table row in
    display order (row_index, model_id, section) and the set of
    row_index values the user has actually selected, validates the
    selection is a single contiguous block within one section before
    a drag-move is allowed to proceed (Sep 12 2026, per Keith's own
    drag-move UI request). Returns (move_start, move_end, section)
    on success, or a real error string explaining why not - never
    guesses a "best effort" range for a scattered or mixed-section
    selection."""
    selected_entries = [r for r in entry_rows if r[0] in selected_indices]
    if not selected_entries:
        return "No real entries selected."
    sections = {r[2] for r in selected_entries}
    if len(sections) > 1:
        return "Selection spans more than one section (objs/tobj) - not supported."
    min_row = min(r[0] for r in selected_entries)
    max_row = max(r[0] for r in selected_entries)
    for row_index, _model_id, _section in entry_rows:
        if min_row <= row_index <= max_row and row_index not in selected_indices:
            return "Selection must be one contiguous block, not scattered rows."
    ids = sorted(r[1] for r in selected_entries)
    return (ids[0], ids[-1], sections.pop())


def preview_cascade(ide_paths: List[str], ipl_paths: List[str], model_ids) -> Dict[str, dict]: #vers 1
    """Dry run - for every given model ID, count the real lines a
    change would touch in each file, writing nothing (Sep 20 2026,
    per Keith: show what an ID change/rename/remove would affect
    first). Returns {path: {'2dfx': n, 'path': n, 'inst': n}} with
    only files that have at least one hit. path blocks are GTA III/VC
    only, 2dfx and inst/cars work for every game."""
    ids = {model_ids} if isinstance(model_ids, int) else set(model_ids)
    out: Dict[str, dict] = {}

    def _bump(path, key):
        out.setdefault(path, {'2dfx': 0, 'path': 0, 'inst': 0})[key] += 1

    def _scan(path, sections, key):
        if not path or not os.path.isfile(path):
            return
        cur = None
        try:
            lines = open(path, "r", encoding="latin-1", newline="").readlines()
        except Exception:
            return
        for raw in lines:
            s = raw.split("#")[0].strip()
            low = s.lower()
            if low == "end":
                cur = None
                continue
            if cur is None and low in sections:
                cur = low
                continue
            if cur in sections and s:
                try:
                    if int(s.split(",", 1)[0].strip()) in ids:
                        _bump(path, key)
                except ValueError:
                    pass

    for p in ide_paths:
        _scan(p, ("2dfx",), '2dfx')
        for u in find_path_usages([p], next(iter(ids))) if len(ids) == 1 else \
                [u for i in ids for u in find_path_usages([p], i)]:
            _bump(p, 'path')
    for p in ipl_paths:
        _scan(p, _IPL_ID_SECTIONS, 'inst')
    return out
