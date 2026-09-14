#this belongs in apps/methods/id_reassign.py - Version: 7
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
# cascade_ipl_files
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
# plan_splice_move
# validate_contiguous_selection

import os
from dataclasses import dataclass, field
from typing import Dict, List

from apps.methods.file_backup import backup_file

_EDITABLE_SECTIONS = ("objs", "tobj")
_IPL_ID_SECTIONS = ("inst", "cars")


@dataclass
class IDShiftPlan: #vers 1
    moved: list = field(default_factory=list)        # (old_id, new_id, model_name, source_ide)
    conflicts: list = field(default_factory=list)     # (new_id, conflicting_name, conflicting_source)
    id_map: Dict[int, int] = field(default_factory=dict)   # old_id -> new_id, moved entries only

    @property
    def ok(self): #vers 1
        return bool(self.moved) and not self.conflicts


def plan_id_shift(result, min_id: int, max_id: int, offset: int) -> IDShiftPlan: #vers 1
    """Dry run only - computes what WOULD move and any real conflicts,
    touches nothing. A conflict is a moved entry's new_id landing on
    a real ID that stays in place (outside the moved block) - the
    whole plan is rejected if any conflict exists (plan.ok is False),
    matching Keith's own "no fallback code - works or doesn't"."""
    plan = IDShiftPlan()
    if offset == 0 or min_id > max_id:
        return plan

    all_ids: Dict[int, tuple] = {}   # id -> (model_name, source_ide)
    for section in _EDITABLE_SECTIONS:
        for obj in result.objects_by_section.get(section, []):
            all_ids[obj.model_id] = (obj.model_name, obj.source_ide)

    stationary_ids = {i: v for i, v in all_ids.items() if not (min_id <= i <= max_id)}

    for section in _EDITABLE_SECTIONS:
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

    return plan


def apply_id_shift(result, plan: IDShiftPlan) -> List[str]: #vers 2
    """Apply an already-planned, conflict-free shift in memory:
    objs/tobj entries get their real model_id updated; 2dfx entries
    sharing an old_id (same real model_id, see IDEParser's own 2dfx-
    stub docstring) get remapped alongside their base object,
    including their synthetic "2dfx_<id>" display name. Returns the
    list of real basenames now needing write_source_file(). Refuses
    (returns []) if plan.ok is False - callers must not apply a
    plan with real conflicts."""
    if not plan.ok:
        return []

    touched = set()
    for section in _EDITABLE_SECTIONS:
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


def cascade_ipl_files(ipl_paths: List[str], id_map: Dict[int, int]) -> Dict[str, bool]: #vers 1
    """Rewrite every given real IPL file's own "inst"/"cars" section
    lines whose leading ID field is in id_map - substituting only
    that field, backing up each real file first. Returns a dict of
    ipl_path -> True/False (False = no matching lines found, file
    unchanged, no backup made - not an error)."""
    results = {}
    for ipl_path in ipl_paths:
        if not ipl_path or not os.path.isfile(ipl_path):
            results[ipl_path] = False
            continue
        try:
            with open(ipl_path, "r", encoding="ascii", errors="ignore") as f:
                lines = f.readlines()
        except Exception:
            results[ipl_path] = False
            continue

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
            if current_section is None and low in _IPL_ID_SECTIONS:
                current_section = low
                out_lines.append(raw)
                continue
            if current_section in _IPL_ID_SECTIONS:
                remapped = _remap_id_field_line(raw, id_map)
                if remapped is not None:
                    out_lines.append(remapped if remapped.endswith("\n") else remapped + "\n")
                    changed = True
                    continue
            out_lines.append(raw)

        if not changed:
            results[ipl_path] = False
            continue

        if backup_file(ipl_path) is None:
            results[ipl_path] = False
            continue
        try:
            with open(ipl_path, "w", encoding="ascii", errors="ignore") as f:
                f.writelines(out_lines)
            results[ipl_path] = True
        except Exception:
            results[ipl_path] = False

    return results


def _max_used_id(result) -> int: #vers 1
    """Highest real declared ID currently loaded (objs/tobj only) -
    same convention as id_shift_dialog.py's own "To highest loaded
    ID" button."""
    all_ids = [obj.model_id for section in _EDITABLE_SECTIONS
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


def apply_add_ids(result, plan: IDShiftPlan) -> List[str]: #vers 1
    """Apply a plan_add_ids() plan. A plan with nothing moved (the
    trivial "nothing above after_id" case) is a valid no-op success,
    not a refusal - apply_id_shift itself would treat empty-moved as
    not-ok, so that case is handled here instead."""
    if not plan.moved:
        return [] if not plan.conflicts else None
    if not plan.ok:
        return None
    return apply_id_shift(result, plan)


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


def plan_collapse_free_ids(result, start_id: int, count: int) -> FreeIdCollapsePlan: #vers 1
    """Dry run only - scans forward from start_id counting real free
    (unassigned) IDs until either count are found, or a real assigned
    ID is hit first (Sep 12 2026, per Keith: "if there are 5 free
    ID's and I delete 6 of them, it won't delete what isn't free...
    If the 6th is assigned I get a warning"). plan.fully_free is
    True only when every one of the count IDs scanned was genuinely
    free - the only case apply_collapse_free_ids will act on."""
    used = {obj.model_id: obj for section in _EDITABLE_SECTIONS
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


def apply_collapse_free_ids(result, plan: FreeIdCollapsePlan) -> List[str]: #vers 1
    """Apply an already-planned free-ID collapse: shift everything
    above the scanned free range DOWN by requested_count, closing
    the gap. Refuses (returns None) unless plan.fully_free - never
    partially collapses, and never touches a real assigned entry
    (that's Delete ID's job, a separate, explicit operation)."""
    if not plan.fully_free:
        return None
    collapse_from = plan.free_ids_found[-1] + 1
    max_id = _max_used_id(result)
    if collapse_from > max_id:
        return []   # nothing above the freed range - already a clean no-op success
    shift_plan = plan_id_shift(result, collapse_from, max_id, -plan.requested_count)
    if not shift_plan.ok:
        return None
    return apply_id_shift(result, shift_plan)


def plan_delete_and_collapse(result, start_id: int, count: int) -> List[tuple]: #vers 1
    """Dry run only - every real assigned entry within [start_id,
    start_id+count-1] that would actually be deleted (Sep 12 2026,
    per Keith: "until i want to remove lines altogether, removing
    those models" - the explicit escalation past a free-ID collapse
    refusal). Returns a list of (model_id, model_name, source_ide);
    empty means every ID in range was already free (equivalent to a
    plain collapse, no real deletions needed)."""
    used = {obj.model_id: obj for section in _EDITABLE_SECTIONS
            for obj in result.objects_by_section.get(section, [])}
    to_delete = []
    for i in range(start_id, start_id + count):
        if i in used:
            obj = used[i]
            to_delete.append((i, obj.model_name, obj.source_ide))
    return to_delete


def apply_delete_and_collapse(result, start_id: int, count: int, to_delete: List[tuple]): #vers 1
    """Actually remove every real to_delete entry, then collapse the
    now-fully-free [start_id, start_id+count-1] range by shifting
    everything above it down by count. Stops and refuses (returns
    None) on the first real removal failure rather than partially
    applying. Returns (removed_ide_basenames, shift_touched_
    basenames) on success."""
    from apps.methods.master_ide_edit import remove_entry

    removed_files = set()
    for model_id, _model_name, source_ide in to_delete:
        err = remove_entry(result, model_id, source_ide)
        if err:
            return None
        removed_files.add(os.path.basename(source_ide))

    max_id = _max_used_id(result)
    collapse_from = start_id + count
    if collapse_from > max_id:
        return sorted(removed_files), []
    shift_plan = plan_id_shift(result, collapse_from, max_id, -count)
    if not shift_plan.ok:
        return None
    shift_touched = apply_id_shift(result, shift_plan)
    return sorted(removed_files), shift_touched


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
    other_by_id = {}
    other_by_name = {}
    for section in _EDITABLE_SECTIONS:
        for obj in result.objects_by_section.get(section, []):
            if os.path.basename(obj.source_ide) == incoming_base:
                incoming.append(obj)
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
    return plan


def apply_insert_relocation(result, plan: InsertRelocationPlan) -> List[str]: #vers 1
    """Apply an already-planned, conflict-free relocation. Reuses
    apply_id_shift directly (it operates purely off plan.id_map, so
    a real IDShiftPlan built from this plan's own data cascades into
    2dfx/write-back exactly the same way an ordinary shift does) -
    refuses (returns []) unless plan.ok, matching every other
    operation in this module's own all-or-nothing rule."""
    if not plan.ok:
        return []
    shift_plan = IDShiftPlan(moved=list(plan.moved), conflicts=[], id_map=dict(plan.id_map))
    return apply_id_shift(result, shift_plan)


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
    for section in _EDITABLE_SECTIONS:
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


def find_free_id_gaps(result, min_id: int, max_id: int) -> List[tuple]: #vers 1
    """Every real contiguous free (unassigned) ID range within
    [min_id, max_id] - the same real scan plan_collapse_free_ids
    already does, just reported instead of applied. Returns a list
    of (gap_start, gap_end) tuples, inclusive, in ascending order."""
    used_ids = {obj.model_id for section in _EDITABLE_SECTIONS
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


def plan_compact_all_gaps(result, min_id: int, max_id: int) -> IDShiftPlan: #vers 1
    """Single holistic plan closing EVERY gap within [min_id, max_id]
    in one pass, instead of one Remove ID/plan_collapse_free_ids at
    a time (Sep 12 2026, per Keith's own "any other use cases"
    follow-up). Every used ID shifts down by however many free IDs
    exist below it within the range - provably conflict-free by
    construction (new IDs come out strictly increasing, never
    touching anything outside the range), so plan.conflicts is
    always empty here; still returned for IDShiftPlan.ok's own
    uniform check."""
    used_objs = [obj for section in _EDITABLE_SECTIONS
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


def plan_swap_ids(result, id_a: int, id_b: int) -> IDShiftPlan: #vers 1
    """Swap whatever real entries currently occupy id_a and id_b -
    both must already be assigned (this isn't a move-into-free-space
    operation, see plan_id_shift/plan_insert_relocation for that).
    Returns an empty, not-ok plan if either id isn't actually
    assigned - never guesses which one the caller meant."""
    all_by_id = {obj.model_id: obj for section in _EDITABLE_SECTIONS
                 for obj in result.objects_by_section.get(section, [])}
    if id_a not in all_by_id or id_b not in all_by_id or id_a == id_b:
        return IDShiftPlan()
    obj_a, obj_b = all_by_id[id_a], all_by_id[id_b]
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

    all_by_id = {obj.model_id: obj for section in _EDITABLE_SECTIONS
                 for obj in result.objects_by_section.get(section, [])}
    plan = IDShiftPlan(id_map=id_map)
    for old_id, new_id in id_map.items():
        obj = all_by_id.get(old_id)
        if obj:
            plan.moved.append((old_id, new_id, obj.model_name, obj.source_ide))
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
