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
