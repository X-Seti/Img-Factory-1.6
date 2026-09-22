#this belongs in apps/methods/file_backup.py - Version: 1
# X-Seti - September 12 2026 - IMG Factory 1.6 - Backup-Before-Write

"""file_backup.py - generic backup-before-write mechanism (Sep 12
2026, per Keith's own build order: "Backup system - the actual
backup-before-write mechanism, tested in isolation" - step 3 of the
Master IDE ID-reassignment plan, required before any real write
touches a file). Not IDE-specific - any real write path (Master IDE
save, future IMG/COL physical reorder) can reuse this."""

##Methods list -
# backup_file
# backup_files
# list_backups
# restore_backup
# note_change
# undo_last_change
# export_change_log

import os
import shutil
from datetime import datetime
from typing import Dict, List, Optional

_BACKUP_DIRNAME = ".imgfactory_backups"


def _backup_dir_for(file_path: str) -> str: #vers 1
    return os.path.join(os.path.dirname(file_path), _BACKUP_DIRNAME)


def backup_file(file_path: str) -> Optional[str]: #vers 1
    """Copy file_path into a real timestamped backup before any write
    touches it. Returns the real backup path on success, None on any
    failure (missing source, permission error, disk full) - never
    raises, never returns a partial/half-written backup as success."""
    if not file_path or not os.path.isfile(file_path):
        return None
    try:
        backup_dir = _backup_dir_for(file_path)
        os.makedirs(backup_dir, exist_ok=True)
        stamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        base = os.path.basename(file_path)
        backup_path = os.path.join(backup_dir, f"{base}.{stamp}.bak")
        # Avoid a same-second collision silently overwriting a real
        # previous backup - add a counter suffix if needed.
        counter = 1
        while os.path.isfile(backup_path):
            backup_path = os.path.join(backup_dir, f"{base}.{stamp}_{counter}.bak")
            counter += 1
        shutil.copy2(file_path, backup_path)
        # Verify the real copy actually matches before trusting it.
        if os.path.getsize(backup_path) != os.path.getsize(file_path):
            return None
        _journal_add(file_path, backup_path)
        return backup_path
    except Exception:
        return None


def backup_files(file_paths: List[str]) -> Dict[str, Optional[str]]: #vers 1
    """Backup every real file in file_paths independently - one
    file's failure doesn't block backing up the others. Returns a
    dict of file_path -> backup_path (None where that one failed),
    so the caller can check every entry before proceeding with any
    real write that depends on ALL of them being backed up."""
    return {p: backup_file(p) for p in file_paths}


def list_backups(file_path: str) -> List[str]: #vers 1
    """Real existing backups for file_path, newest first."""
    backup_dir = _backup_dir_for(file_path)
    base = os.path.basename(file_path)
    if not os.path.isdir(backup_dir):
        return []
    matches = [os.path.join(backup_dir, f) for f in os.listdir(backup_dir)
               if f.startswith(base + ".") and f.endswith(".bak")]
    return sorted(matches, key=os.path.getmtime, reverse=True)


def restore_backup(backup_path: str, restore_to: str) -> bool: #vers 1
    """Copy a real backup back over restore_to. Returns True only on
    a verified, size-matching copy."""
    if not backup_path or not os.path.isfile(backup_path):
        return False
    try:
        shutil.copy2(backup_path, restore_to)
        return os.path.getsize(restore_to) == os.path.getsize(backup_path)
    except Exception:
        return False


# - Change journal (Sep 20 2026, per Keith: group undo + change log).
# Every successful backup_file() is also recorded here, so one
# operation that rewrites an IDE, several IPLs and an IFX can be
# undone or logged as ONE change. Backups made within _GROUP_GAP
# seconds of each other belong to the same change.

import json
import time

_GROUP_GAP = 5.0
_pending_label = ""


def _journal_path() -> str: #vers 1
    return os.path.join(os.path.expanduser("~"), ".config", "imgfactory", "change_journal.json")


def _journal_load() -> list: #vers 1
    try:
        with open(_journal_path(), "r", encoding="utf-8") as f:
            return json.load(f)
    except Exception:
        return []


def _journal_save(entries: list): #vers 1
    try:
        os.makedirs(os.path.dirname(_journal_path()), exist_ok=True)
        with open(_journal_path(), "w", encoding="utf-8") as f:
            json.dump(entries[-500:], f, indent=1)
    except Exception:
        pass


def _journal_add(file_path: str, backup_path: str): #vers 1
    global _pending_label
    entries = _journal_load()
    now = time.time()
    if entries and now - entries[-1]["time"] <= _GROUP_GAP:
        group = entries[-1]["group"]
        label = entries[-1].get("label", "")
    else:
        group = int(now * 1000)
        label = _pending_label
    _pending_label = ""
    entries.append({"group": group, "time": now, "file": file_path,
                    "backup": backup_path, "label": label})
    _journal_save(entries)


def note_change(label: str): #vers 1
    """Name the NEXT change (e.g. "Rename ID 865 ap_tower -> ap_tower_v2")
    so the change log reads clearly. Call just before the writes."""
    global _pending_label
    _pending_label = label


def undo_last_change() -> Optional[List[str]]: #vers 1
    """Restore every file of the most recent change group from its
    backup, then drop that group. Returns the restored file paths,
    or None if there's nothing to undo."""
    entries = _journal_load()
    if not entries:
        return None
    group = entries[-1]["group"]
    batch = [e for e in entries if e["group"] == group]
    restored = []
    # oldest backup per file = the state before this change began
    first_by_file = {}
    for e in batch:
        first_by_file.setdefault(e["file"], e["backup"])
    for path, bak in first_by_file.items():
        try:
            shutil.copy2(bak, path)
            restored.append(path)
        except Exception:
            pass
    _journal_save([e for e in entries if e["group"] != group])
    return restored


def export_change_log(dest_path: str) -> bool: #vers 1
    """Write the journal as a readable text log, one block per change."""
    entries = _journal_load()
    if not entries:
        return False
    groups: Dict[int, list] = {}
    for e in entries:
        groups.setdefault(e["group"], []).append(e)
    lines = ["Img Factory change log", ""]
    for g in sorted(groups):
        batch = groups[g]
        when = datetime.fromtimestamp(batch[0]["time"]).strftime("%Y-%m-%d %H:%M:%S")
        label = batch[0].get("label") or "(unlabelled change)"
        lines.append(f"{when}  {label}")
        for path in sorted({e["file"] for e in batch}):
            lines.append(f"    {path}")
        lines.append("")
    try:
        with open(dest_path, "w", encoding="utf-8") as f:
            f.write("\n".join(lines))
        return True
    except Exception:
        return False


def safe_write_bytes(path: str, data: bytes, label: str = "") -> None: #vers 1
    """Write data to path the safe way: note the change in the journal, back
    the existing file up (raises if that fails - nothing is overwritten), then
    write to a temp file in the same folder and swap it in."""
    import tempfile
    if os.path.exists(path):
        note_change(label or f"Save {os.path.basename(path)}")
        if backup_file(path) is None:
            raise IOError(f"Backup of {os.path.basename(path)} failed - file not overwritten")
    d = os.path.dirname(os.path.abspath(path))
    fd, tmp = tempfile.mkstemp(dir=d, prefix=".sv_", suffix=".tmp")
    try:
        with os.fdopen(fd, "wb") as f:
            f.write(data)
        if os.path.exists(path):
            try:
                os.chmod(tmp, os.stat(path).st_mode & 0o7777)
            except OSError:
                pass
        os.replace(tmp, path)
    except Exception:
        if os.path.exists(tmp):
            os.unlink(tmp)
        raise
