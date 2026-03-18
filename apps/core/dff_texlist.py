#this belongs in apps/core/dff_texlist.py - Version: 3
# X-Seti - March 2026 - IMG Factory 1.6 - DFF Texture List Reader
"""
DFF Texture List Reader
Byte-scan for Texture (0x0006) chunks — handles all RW versions (GTA3 through SA/LCS).
Fallback: null-terminated string scan for models with no Texture sub-chunks.
"""

import os
import re
import struct
from typing import List, Dict, Optional

##Methods list -
# _build_txd_set
# check_txd_in_img
# check_txd_on_disk
# find_missing_txds
# get_dff_texture_report
# ide_txd_in_img
# parse_dff_textures

RW_STRING  = 0x0002
RW_TEXTURE = 0x0006

# Words that indicate non-texture strings
_SKIP_WORDS = frozenset([
    'collision', 'mesh', 'frame', 'bone', 'skin', 'anim',
    'normal', 'matrix', 'object', 'model', 'geometry',
])


def parse_dff_textures(data: bytes) -> List[str]: #vers 3
    """Extract texture names from a DFF binary.
    Byte-scan for Texture (0x0006) chunks, reads first String (0x0002) child.
    Works across all RW versions — GTA3 (0x0800/0x0C02), VC, SA, LCS.
    Falls back to null-terminated string scan if no Texture chunks found.
    """
    names = _scan_texture_chunks(data)
    if not names:
        names = _fallback_string_scan(data)
    # Deduplicate preserving order, sort
    seen = set()
    result = []
    for n in names:
        if n.lower() not in seen:
            seen.add(n.lower())
            result.append(n)
    result.sort(key=str.lower)
    return result


def _scan_texture_chunks(data: bytes) -> List[str]: #vers 1
    """Byte-by-byte scan for Texture chunk signature 06 00 00 00.
    Reads first String sub-chunk for the diffuse texture name.
    """
    names = []
    limit = len(data) - 12
    i = 0
    while i < limit:
        # Look for Texture chunk type bytes
        if data[i] == 0x06 and data[i+1] == 0x00 and data[i+2] == 0x00 and data[i+3] == 0x00:
            s = struct.unpack_from('<I', data, i + 4)[0]
            if 8 < s < 512:
                body_start = i + 12
                body_end   = body_start + s
                if body_end <= len(data):
                    name = _first_string(data, body_start, body_end)
                    if name:
                        names.append(name)
                i = body_end
                continue
        i += 1
    return names


def _first_string(data: bytes, start: int, end: int) -> Optional[str]: #vers 1
    """Return first String (0x0002) chunk value between start and end."""
    j = start
    while j + 12 <= end:
        ct = struct.unpack_from('<I', data, j)[0]
        cs = struct.unpack_from('<I', data, j + 4)[0]
        body     = j + 12
        body_end = body + cs
        if body_end > end or cs > (end - start):
            break
        if ct == RW_STRING and 0 < cs < 64:
            raw  = data[body:body_end]
            name = raw.split(b'\x00')[0].decode('ascii', errors='ignore').strip()
            if len(name) > 1:
                return name
        j += 12 + cs
    return None


def _fallback_string_scan(data: bytes) -> List[str]: #vers 1
    """Fallback: scan for null-terminated ASCII strings that look like texture names.
    Used when DFF has no Texture sub-chunks (some GTA3/VC models).
    """
    raw = re.findall(rb'[A-Za-z][A-Za-z0-9_]{3,31}\x00', data)
    names = []
    for b in raw:
        n = b.rstrip(b'\x00').decode('ascii', 'ignore')
        low = n.lower()
        if any(w in low for w in _SKIP_WORDS):
            continue
        # Must contain a digit or underscore to look like a texture name
        if '_' in n or any(c.isdigit() for c in n):
            names.append(n)
    return list(dict.fromkeys(names))


def check_txd_in_img(tex_names: List[str], img_entries) -> Dict[str, bool]: #vers 2
    """Check which texture names have a matching .txd stem in the IMG entries.
    Note: GTA uses shared TXDs — a texture name is unlikely to match a TXD stem.
    Use ide_txd_in_img() to check the IDE-declared TXD instead.
    """
    txd_set = _build_txd_set(img_entries)
    return {name: name.lower() in txd_set for name in tex_names}


def ide_txd_in_img(ide_txd_name: str, img_entries) -> bool: #vers 1
    """Check if the IDE-declared TXD name exists as a .txd entry in the IMG."""
    if not ide_txd_name:
        return False
    txd_set = _build_txd_set(img_entries)
    return ide_txd_name.lower() in txd_set


def _build_txd_set(img_entries) -> set: #vers 1
    """Build a set of lowercase TXD stems from IMG entries."""
    result = set()
    for entry in (img_entries or []):
        n = getattr(entry, 'name', '')
        if n.lower().endswith('.txd'):
            result.add(os.path.splitext(n.lower())[0])
    return result


def check_txd_on_disk(tex_names: List[str], search_dirs: List[str]) -> Dict[str, Optional[str]]: #vers 1
    """Check which texture names have a matching .txd file on disk."""
    result = {}
    for name in tex_names:
        found = None
        for d in search_dirs:
            for candidate in (name + '.txd', name + '.TXD'):
                path = os.path.join(d, candidate)
                if os.path.isfile(path):
                    found = path
                    break
            if found:
                break
        result[name] = found
    return result


def get_dff_texture_report(data: bytes, img_entries=None, search_dirs=None) -> dict: #vers 2
    """Full report: texture names + IMG check + disk check."""
    names   = parse_dff_textures(data)
    in_img  = check_txd_in_img(names, img_entries or [])
    on_disk = check_txd_on_disk(names, search_dirs or [])
    return {'textures': names, 'in_img': in_img, 'on_disk': on_disk}


def find_missing_txds(main_window) -> list: #vers 1
    """Scan all DFF entries in the loaded IMG.
    For each DFF, use xref to get IDE-declared TXD name.
    Returns list of dicts:
      {dff, ide_txd, in_img, textures_in_dff, textures_missing}
    """
    results = []
    img = getattr(main_window, 'current_img', None)
    if not img or not hasattr(img, 'entries'):
        return results

    xref = getattr(main_window, 'xref', None)
    model_map = xref.model_map if xref and hasattr(xref, 'model_map') else {}

    # Build set of txd stems in the IMG for fast lookup
    txd_in_img = _build_txd_set(img.entries)

    dff_entries = [e for e in img.entries if getattr(e,'name','').lower().endswith('.dff')]

    for entry in dff_entries:
        stem = entry.name.rsplit('.', 1)[0].lower()
        ide_obj = model_map.get(stem)
        ide_txd = ide_obj.txd_name if ide_obj and getattr(ide_obj,'txd_name',None) else None

        # Only flag if IDE says there should be a TXD and it's not in the IMG
        txd_found = (ide_txd.lower() in txd_in_img) if ide_txd else None

        # Read DFF textures — skip if TXD exists (no problem)
        tex_in_dff = []
        tex_missing = []
        if ide_txd and not txd_found:
            try:
                data = img.read_entry_data(entry)
                if data:
                    tex_in_dff = parse_dff_textures(data)
                    tex_missing = [t for t in tex_in_dff if t.lower() not in txd_in_img]
            except Exception:
                pass

        results.append({
            'dff':              entry.name,
            'ide_txd':          ide_txd,
            'in_img':           txd_found,
            'textures_in_dff':  tex_in_dff,
            'textures_missing': tex_missing,
        })

    return results
