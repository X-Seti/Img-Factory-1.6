#this belongs in apps/methods/master_ide_edit.py - Version: 2
# X-Seti - September 12 2026 - IMG Factory 1.6 - Master IDE Single-Entry Edits

"""master_ide_edit.py - safe single-entry operations for Master IDE
(Sep 12 2026, step 4 of Keith's own build order: "Safe single-entry
operations - rename, add, remove one entry at a time, with backup,
no ID cascading yet"). Edits happen against a loaded MasterIDEResult
in memory; write_source_file() then backs up and rewrites the one
real source .ide file that entry belongs to.

Only objs/tobj are editable - every other real section (cars, peds,
weap, hier, anim, txdp, 2dfx, path) is copied through from the
original file's own raw text verbatim on write, never reconstructed
from the generic best-effort formatter write_master_ide's own "Save
as Master IDE" uses for a brand-new combined file. That generic
formatter is lossy for anything but objs/tobj (real 2dfx/cars/peds
lines have entirely different field layouts) - fine for a fresh
combined output the user reviews before using, not acceptable for
silently overwriting a real existing file in place."""

##Methods list -
# rename_entry
# add_entry
# remove_entry
# write_source_file

import os
from apps.methods.file_backup import backup_file
from apps.methods.master_ide import _section_order_and_raw

_EDITABLE_SECTIONS = ("objs", "tobj")


def _find_entries(result, model_id, source_ide=None): #vers 2
    """Real matching IDEObject(s) for model_id across objs/tobj,
    optionally scoped to one source file. Matches by basename, not
    full path - IDEParser itself only ever stores obj.source_ide as
    a basename (see gta_dat_parser.py's own IDEParser.parse), so two
    loaded files sharing an identical basename from different
    folders can't be told apart here either - the same pre-existing
    limitation Master IDE's own display already has."""
    source_base = os.path.basename(source_ide) if source_ide else None
    matches = []
    for section in _EDITABLE_SECTIONS:
        for obj in result.objects_by_section.get(section, []):
            if obj.model_id == model_id and (
                    source_base is None or os.path.basename(obj.source_ide) == source_base):
                matches.append(obj)
    return matches


def rename_entry(result, model_id, new_name, source_ide=None): #vers 1
    """Rename the real matching entry's model_name in memory. Returns
    None on success, or an error string - fails rather than guessing
    which entry to rename if model_id matches more than one real
    object without a source_ide to disambiguate."""
    matches = _find_entries(result, model_id, source_ide)
    if not matches:
        where = f" in {os.path.basename(source_ide)}" if source_ide else ""
        return f"No entry with ID {model_id}{where}"
    if len(matches) > 1:
        files = ", ".join(sorted({os.path.basename(m.source_ide) for m in matches}))
        return f"ID {model_id} matches entries in {files} - pass source_ide to disambiguate"
    matches[0].model_name = new_name
    return None


def add_entry(result, section, model_id, model_name, txd_name, source_ide, extra=None): #vers 3
    """Add a new real entry to the in-memory result, tagged with the
    real source file it will be written back to. Returns None on
    success, or an error string. source_ide must be one of the
    already-loaded real files - never a guessed/new path. section
    must be objs or tobj - every other real section isn't editable
    yet (see this module's own docstring). extra must include the
    real required fields for the section (draw_dist/flags for objs;
    also time_on/time_off for tobj) - an incomplete extra would
    write a line with fewer than the real format's minimum fields,
    which IDEParser itself silently fails to re-parse at all (Sep 12
    2026 - caught by round-trip testing this module before use)."""
    if os.path.basename(source_ide) not in {os.path.basename(p) for p in result.source_files}:
        return f"{source_ide} is not one of the loaded files"
    if section not in _EDITABLE_SECTIONS:
        return f"Adding entries to '{section}' isn't supported yet - only objs/tobj"
    existing = _find_entries(result, model_id)
    if existing:
        return f"ID {model_id} already used by {existing[0].model_name} - remove or rename it first"
    extra = extra or {}
    required = ("draw_dist", "flags") if section == "objs" else (
        "draw_dist", "flags", "time_on", "time_off")
    missing = [k for k in required if k not in extra]
    if missing:
        return f"Missing required field(s) for {section}: {', '.join(missing)}"
    from apps.methods.gta_dat_parser import IDEObject
    obj = IDEObject(model_id, model_name, txd_name, "object", section, extra,
                     os.path.basename(source_ide), 0)
    result.objects_by_section.setdefault(section, []).append(obj)
    result.objects_by_section[section].sort(key=lambda o: o.model_id)
    return None


def remove_entry(result, model_id, source_ide): #vers 2
    """Remove the real matching entry from the in-memory result.
    Returns None on success, or an error string. source_ide is
    required, not optional - removal is destructive, so which file/
    entry must be explicit, never guessed."""
    source_base = os.path.basename(source_ide)
    for section in _EDITABLE_SECTIONS:
        objs = result.objects_by_section.get(section, [])
        match = next((o for o in objs if o.model_id == model_id
                      and os.path.basename(o.source_ide) == source_base), None)
        if match:
            objs.remove(match)
            return None
    return f"No entry with ID {model_id} in {source_base}"


def write_source_file(result, source_path) -> bool: #vers 2
    """Back up then rewrite ONE real source .ide file with whatever
    edits are currently in result. objs/tobj are rebuilt from the
    in-memory objects belonging to this file; every other real
    section is copied through from the ORIGINAL file's own raw text
    verbatim (see this module's own docstring for why)."""
    from apps.methods.master_ide import _format_objs_or_tobj_line

    source_base = os.path.basename(source_path)
    if source_base not in {os.path.basename(p) for p in result.source_files}:
        return False
    if not os.path.isfile(source_path):
        return False
    try:
        with open(source_path, "r", encoding="ascii", errors="ignore") as f:
            original_text = f.read()
    except Exception:
        return False

    order, raw_sections = _section_order_and_raw(original_text)

    # New entries can add a section that didn't exist in the
    # original file at all (e.g. a file with no tobj section gets
    # its first tobj entry added) - append it at the end, keeping
    # every originally-present section in its own original order.
    for section in _EDITABLE_SECTIONS:
        if any(os.path.basename(o.source_ide) == source_base
               for o in result.objects_by_section.get(section, [])) and section not in order:
            order.append(section)

    if backup_file(source_path) is None:
        return False

    lines = []
    for section in order:
        if section in _EDITABLE_SECTIONS:
            file_objs = sorted(
                (o for o in result.objects_by_section.get(section, [])
                 if os.path.basename(o.source_ide) == source_base),
                key=lambda o: o.model_id)
            if not file_objs:
                continue
            lines.append(section)
            for obj in file_objs:
                lines.append(_format_objs_or_tobj_line(obj))
            lines.append("end")
            lines.append("")
        else:
            lines.append(section)
            lines.extend(raw_sections.get(section, []))
            lines.append("end")
            lines.append("")

    try:
        with open(source_path, "w", encoding="ascii", errors="ignore") as f:
            f.write("\n".join(lines))
        return True
    except Exception:
        return False
