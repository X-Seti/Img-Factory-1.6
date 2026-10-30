#this belongs in methods/common_functions.py - Version: 1
# X-Seti - November29 2025 - IMG Factory 1.5 - Common Functions

"""
Common Functions - Shared utility functions for import operations
Consolidates duplicate functions from core/impotr.py and core/import_via.py
"""

import os
import re
from typing import List, Optional, Dict, Any, Tuple
from apps.methods.rw_versions import parse_rw_version, get_rw_version_name

##Methods list -
# sanitize_filename
# detect_file_type
# detect_rw_version

def sanitize_filename(filename: str) -> str: #vers 1
    """Sanitize filename to remove problematic characters"""
    # Remove invalid characters and replace with underscore
    sanitized = re.sub(r'[<>:"/\\|?*]', '_', filename)
    # Remove control characters
    sanitized = ''.join(c for c in sanitized if ord(c) >= 32 and ord(c) != 127)
    return sanitized


def detect_file_type(filename: str) -> str: #vers 1
    """Detect file type based on extension"""
    ext = os.path.splitext(filename)[1].lower()
    if ext in ['.dff']:
        return 'MODEL'
    elif ext in ['.txd']:
        return 'TEXTURE'
    elif ext in ['.col']:
        return 'COLLISION'
    elif ext in ['.wav', '.mp3', '.ogg']:
        return 'AUDIO'
    elif ext in ['.dat', '.txt']:
        return 'TEXT'
    else:
        return 'UNKNOWN'


def detect_rw_version(file_path: str) -> tuple[int, str]: #vers 1
    """Detect RenderWare version from file data"""
    try:
        with open(file_path, 'rb') as f:
            data = f.read()
        if len(data) >= 12:
            version_val, version_name = parse_rw_version(data[8:12])
            if version_val > 0:
                return version_val, version_name
        return 0, "Unknown"
    except Exception:
        return 0, "Unknown"


__all__ = [
    'sanitize_filename',
    'detect_file_type', 
    'detect_rw_version'
]