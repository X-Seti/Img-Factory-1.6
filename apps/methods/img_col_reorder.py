#this belongs in apps/methods/img_col_reorder.py - Version: 2
# X-Seti - September 12 2026 - IMG Factory 1.6 - IMG/COL Physical Reorder

"""img_col_reorder.py - the final Master IDE plan step: physically
rebuild an IMG/COL archive so its entries appear in the same order
as their real declared IDE IDs (Sep 12 2026, per Keith's own
original 7-step plan: "IMG/COL physical reorder — rebuild those
archives to match the new order once the ID/IDE side is solid").

Neither IMGFile.save_img_file() nor COLFile.save_to_file() need any
new binary-writing code at all - both already rebuild their whole
real file purely from whatever order self.entries/self.models
currently holds, and both read all real entry data into memory
BEFORE opening the file for writing (checked directly against their
own code, not assumed) - so reordering that in-memory list first and
calling the already-existing save is genuinely safe. This module
only ever decides the target order and backs up before writing;
the actual binary rebuild is 100% the existing, already-tested
code path.

IMG entries are reordered conservatively: only DFF entries are
permuted among THEIR OWN original positions (matched by name against
the given id_by_name map) - every other entry (TXD, COL, etc.) never
moves at all, since there's no single well-defined "correct" position
for a texture shared by many models. A standalone COL file's own
model list, by contrast, IS entirely collision data, so every real
model is reordered."""

##Methods list -
# build_id_by_name
# plan_img_reorder
# apply_img_reorder
# plan_col_reorder
# apply_col_reorder

import os


def build_id_by_name(result): #vers 2
    """Real lowercase model_name -> declared ID, from every section
    that declares a real DFF model with its own ID - not just objs/
    tobj (Sep 12 2026, real gap caught by Keith: "anim, tojs the
    ID" - anim/hier/cars/peds/weap ALL declare real model_id+name
    pairs with their own real DFF entries too, e.g. SFs.ide's own
    BS_building_SFS in its anim section). Deliberately excludes only
    2dfx/txdp - neither has a real declared model ID of its own (see
    master_ide.py's own _ID_EXCLUDED_SECTIONS for why)."""
    id_by_name = {}
    for section, objs in result.objects_by_section.items():
        if section in ("2dfx", "txdp"):
            continue
        for obj in objs:
            id_by_name[obj.model_name.lower()] = obj.model_id
    return id_by_name


def plan_img_reorder(entries, id_by_name): #vers 1
    """Pure logic, no file I/O - entries is any list of objects
    exposing .name/.extension (a real IMGFile.entries list works
    directly). Only DFF entries are permuted, and only among their
    OWN original index positions - every other entry stays exactly
    where it was. Unmatched DFF names (no real declared ID found)
    keep their original relative order, sorted after every matched
    one rather than guessed into some arbitrary position. Returns
    (new_entries_list, unmatched_names) - never mutates entries
    itself."""
    dff_positions = [i for i, e in enumerate(entries) if e.extension.upper() == 'DFF']
    dff_entries = [entries[i] for i in dff_positions]

    def sort_key(indexed_item):
        idx, e = indexed_item
        name = os.path.splitext(e.name)[0].lower()
        model_id = id_by_name.get(name)
        return (0, model_id) if model_id is not None else (1, idx)

    indexed = sorted(enumerate(dff_entries), key=sort_key)
    sorted_dffs = [e for _, e in indexed]
    unmatched = [e.name for e in dff_entries
                 if os.path.splitext(e.name)[0].lower() not in id_by_name]

    new_entries = list(entries)
    for pos, e in zip(dff_positions, sorted_dffs):
        new_entries[pos] = e
    return new_entries, unmatched


def apply_img_reorder(img_path: str, result) -> tuple: #vers 1
    """Real reorder + rebuild of one real IMG archive. Backs up via
    this app's own timestamped file_backup (in addition to
    IMGFile.save_img_file()'s own single .backup copy - real extra
    safety margin for a real binary archive rewrite, matching every
    other destructive operation's own backup discipline this
    session). Returns (success, unmatched_names) - unmatched_names
    is still populated even on failure, so a caller can show what
    would have been left in original order."""
    from apps.methods.img_core_classes import IMGFile
    from apps.methods.file_backup import backup_file

    if not img_path or not os.path.isfile(img_path):
        return False, []
    id_by_name = build_id_by_name(result)

    try:
        img_file = IMGFile(img_path)
        if not img_file.open():
            return False, []
        new_entries, unmatched = plan_img_reorder(img_file.entries, id_by_name)
        if backup_file(img_path) is None:
            return False, unmatched
        img_file.entries = new_entries
        success = img_file.save()
        return bool(success), unmatched
    except Exception:
        return False, []


def plan_col_reorder(models, id_by_name): #vers 1
    """Pure logic, no file I/O - models is any list of objects
    exposing .name (a real COLFile.models list works directly).
    Unlike an IMG, a standalone COL file's own model list IS
    entirely collision data, so every real model is reordered (not
    just a subset kept in original positions). Unmatched names keep
    their original relative order, sorted after every matched one.
    Returns (new_models_list, unmatched_names)."""
    def sort_key(indexed_item):
        idx, m = indexed_item
        model_id = id_by_name.get((m.name or "").lower())
        return (0, model_id) if model_id is not None else (1, idx)

    indexed = sorted(enumerate(models), key=sort_key)
    new_models = [m for _, m in indexed]
    unmatched = [m.name for m in models if (m.name or "").lower() not in id_by_name]
    return new_models, unmatched


def apply_col_reorder(col_path: str, result) -> tuple: #vers 1
    """Real reorder + rebuild of one real standalone COL file. Backs
    up via this app's own timestamped file_backup before writing -
    COLFile.save_to_file() has no built-in backup of its own at all,
    unlike IMGFile.save_img_file(), so this is the ONLY backup for a
    COL rewrite. Returns (success, unmatched_names)."""
    from apps.methods.col_core_classes import COLFile
    from apps.methods.file_backup import backup_file

    if not col_path or not os.path.isfile(col_path):
        return False, []
    id_by_name = build_id_by_name(result)

    try:
        col_file = COLFile()
        if not col_file.load_from_file(col_path):
            return False, []
        new_models, unmatched = plan_col_reorder(col_file.models, id_by_name)
        if backup_file(col_path) is None:
            return False, unmatched
        col_file.models = new_models
        success = col_file.save_to_file(col_path)
        return bool(success), unmatched
    except Exception:
        return False, []
