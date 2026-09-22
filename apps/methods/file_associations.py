#this belongs in methods/file_associations.py - Version: 2
# X-Seti - Sep 22 2026 - IMG Factory 1.6 - File Type Associations

"""
File Type Associations - maps file extensions to internal workshops
or external apps. User overrides persist in file_associations.json.
"""

import json
import subprocess
from pathlib import Path

##Methods list -
# get_associations
# get_handler
# is_internal
# launch_external
# save_associations

CONFIG_PATH = Path.home() / '.config' / 'imgfactory' / 'file_associations.json'

INTERNAL_HANDLERS = {'model_workshop', 'txd_workshop', 'col_workshop', 'img_factory'}

DEFAULT_ASSOCIATIONS = {
    'dff': 'model_workshop',
    'txd': 'txd_workshop',
    'col': 'col_workshop',
    'img': 'img_factory', 'dir': 'img_factory',
    'mp4': 'mpv', 'mkv': 'mpv', 'avi': 'mpv', 'mov': 'mpv', 'webm': 'mpv',
    'mp3': 'clementine', 'wav': 'clementine', 'ogg': 'clementine', 'flac': 'clementine',
    'png': 'gwenview', 'jpg': 'gwenview', 'jpeg': 'gwenview', 'bmp': 'gwenview',
    'tga': 'gwenview', 'gif': 'gwenview',
    'txt': 'kate', 'log': 'kate', 'ini': 'kate', 'cfg': 'kate', 'py': 'kate',
    'json': 'kate', 'ide': 'kate', 'ipl': 'kate', 'dat': 'kate',
    'doc': 'openoffice', 'docx': 'openoffice', 'odt': 'openoffice',
    'xls': 'openoffice', 'xlsx': 'openoffice', 'csv': 'openoffice',
    'pdf': 'xpdf',
    'zip': 'xarchiver', '7z': 'xarchiver', 'rar': 'xarchiver',
}


def get_associations(): #vers 1
    """Load associations, user overrides merged onto defaults."""
    data = dict(DEFAULT_ASSOCIATIONS)
    try:
        data.update(json.loads(CONFIG_PATH.read_text()))
    except Exception:
        pass
    return data


def save_associations(assoc): #vers 1
    """Persist the full associations dict."""
    try:
        CONFIG_PATH.parent.mkdir(parents=True, exist_ok=True)
        CONFIG_PATH.write_text(json.dumps(assoc, indent=2))
    except Exception:
        pass


def get_handler(path): #vers 1
    """Return the handler name for a file's extension, or None."""
    ext = Path(path).suffix.lstrip('.').lower()
    return get_associations().get(ext)


def is_internal(handler): #vers 1
    """True if handler is an IMG Factory workshop, not an external app."""
    return handler in INTERNAL_HANDLERS


def launch_external(app_name, path): #vers 1
    """Launch an external app on a file. Returns True on success."""
    try:
        subprocess.Popen([app_name, path])
        return True
    except Exception:
        return False
