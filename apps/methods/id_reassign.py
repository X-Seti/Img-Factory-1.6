#this belongs in apps/methods/id_reassign.py - Version: 1
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


def apply_id_shift(result, plan: IDShiftPlan) -> List[str]: #vers 1
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
