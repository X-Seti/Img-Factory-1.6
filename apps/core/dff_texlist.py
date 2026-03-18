#this belongs in apps/core/dff_texlist.py - Version: 2
# X-Seti - March 2026 - IMG Factory 1.6 - DFF Texture List Reader
"""
DFF Texture List Reader
Linear scan for Texture (0x0006) chunks containing String (0x0002) names.
Avoids recursive walking which breaks on Geometry chunks with compressed sizes.
"""

import os
import struct
from typing import List, Dict, Optional

##Methods list -
# check_txd_in_img
# check_txd_on_disk
# get_dff_texture_report
# parse_dff_textures

RW_STRING  = 0x0002
RW_TEXTURE = 0x0006


def parse_dff_textures(data: bytes) -> List[str]: #vers 2
    """Linear scan through DFF binary for Texture (0x0006) chunks.
    Reads the first String (0x0002) child of each Texture chunk — the diffuse name.
    Returns deduplicated sorted list. Safe against Geometry chunks with corrupt sizes.
    """
    names = []
    seen  = set()
    i = 0
    limit = len(data) - 12

    while i < limit:
        chunk_type = struct.unpack_from('<I', data, i)[0]
        chunk_size = struct.unpack_from('<I', data, i + 4)[0]

        if chunk_type == RW_TEXTURE and 8 < chunk_size < 512:
            body  = i + 12
            end   = body + chunk_size
            if end <= len(data):
                name = _first_string(data, body, end)
                if name and name.lower() not in seen:
                    seen.add(name.lower())
                    names.append(name)
            i = end
        else:
            # Advance by 4 — linear scan, don't trust size fields globally
            i += 4

    names.sort(key=str.lower)
    return names


def _first_string(data: bytes, start: int, end: int) -> Optional[str]: #vers 1
    """Return first String (0x0002) chunk value found between start and end."""
    j = start
    while j + 12 <= end:
        ct = struct.unpack_from('<I', data, j)[0]
        cs = struct.unpack_from('<I', data, j + 4)[0]
        body = j + 12
        body_end = body + cs
        if body_end > end or cs > end - start:
            break
        if ct == RW_STRING and 0 < cs < 64:
            raw  = data[body:body_end]
            name = raw.split(b'\x00')[0].decode('ascii', errors='ignore').strip()
            if len(name) > 1:
                return name
        j += 12 + cs
    return None


def check_txd_in_img(tex_names: List[str], img_entries) -> Dict[str, bool]: #vers 1
    """Check which texture names have a matching .txd in the loaded IMG entries."""
    txd_set = set()
    for entry in (img_entries or []):
        n = getattr(entry, 'name', '')
        if n.lower().endswith('.txd'):
            txd_set.add(os.path.splitext(n.lower())[0])
    return {name: name.lower() in txd_set for name in tex_names}


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


def get_dff_texture_report(data: bytes, img_entries=None, search_dirs=None) -> dict: #vers 1
    """Full report: texture names + IMG check + disk check."""
    names   = parse_dff_textures(data)
    in_img  = check_txd_in_img(names, img_entries or [])
    on_disk = check_txd_on_disk(names, search_dirs or [])
    return {'textures': names, 'in_img': in_img, 'on_disk': on_disk}
