#this belongs in apps/core/dff_texlist.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - DFF Texture List Reader
"""
DFF Texture List Reader
Walks RenderWare chunks to extract texture names from DFF model files.
No regex guessing — reads the actual RW binary structure.
"""

import os
import struct
from typing import List, Dict, Optional

##Methods list -
# parse_dff_textures
# check_txd_in_img
# check_txd_on_disk
# get_dff_texture_report

# RW chunk type IDs
RW_STRING      = 0x0002
RW_EXTENSION   = 0x0003
RW_TEXTURE     = 0x0006
RW_MATERIAL    = 0x0007
RW_MATLIST     = 0x0008
RW_CLUMP       = 0x0010
RW_STRUCT      = 0x0001


def parse_dff_textures(data: bytes) -> List[str]: #vers 1
    """Walk RW chunks in a DFF binary and return all texture names found.
    Looks for String (0x0002) chunks directly inside Texture (0x0006) chunks.
    Returns a deduplicated, sorted list of texture names.
    """
    names = []
    _walk_chunks(data, 0, len(data), names)
    # deduplicate preserving first occurrence order, then sort
    seen = set()
    result = []
    for n in names:
        nl = n.lower()
        if nl and nl not in seen:
            seen.add(nl)
            result.append(n)
    result.sort(key=str.lower)
    return result


def _walk_chunks(data: bytes, start: int, end: int, names: list): #vers 1
    """Recursively walk RW chunk tree between start and end offsets."""
    offset = start
    while offset + 12 <= end:
        chunk_type = struct.unpack_from('<I', data, offset)[0]
        chunk_size = struct.unpack_from('<I', data, offset + 4)[0]
        # chunk_version at offset+8, unused here
        body_start = offset + 12
        body_end   = body_start + chunk_size

        if body_end > end or chunk_size > len(data):
            break

        if chunk_type == RW_TEXTURE:
            # First child should be Struct (0x0001), second is String = texture name
            _extract_texture_name(data, body_start, body_end, names)
        elif chunk_type in (RW_CLUMP, RW_MATLIST, RW_MATERIAL, RW_EXTENSION):
            # These are containers — recurse
            _walk_chunks(data, body_start, body_end, names)

        offset = body_end


def _extract_texture_name(data: bytes, start: int, end: int, names: list): #vers 1
    """Extract the texture name string from inside a Texture chunk."""
    offset = start
    string_count = 0
    while offset + 12 <= end:
        chunk_type = struct.unpack_from('<I', data, offset)[0]
        chunk_size = struct.unpack_from('<I', data, offset + 4)[0]
        body_start = offset + 12
        body_end   = body_start + chunk_size

        if body_end > end:
            break

        if chunk_type == RW_STRING and chunk_size > 0:
            raw = data[body_start:body_end]
            name = raw.split(b'\x00')[0].decode('ascii', errors='ignore').strip()
            if name and 1 < len(name) < 64:
                if string_count == 0:
                    # First string = diffuse texture name
                    names.append(name)
                # Second string = alpha mask name (skip for now)
            string_count += 1

        offset = body_end


def check_txd_in_img(tex_names: List[str], img_entries) -> Dict[str, bool]: #vers 1
    """Check which texture names have a matching .txd in the loaded IMG entries.
    img_entries: list of IMGEntry objects with .name attribute.
    Returns dict: {tex_name: found_bool}
    """
    txd_set = set()
    for entry in (img_entries or []):
        n = getattr(entry, 'name', '')
        if n.lower().endswith('.txd'):
            txd_set.add(os.path.splitext(n.lower())[0])

    return {name: name.lower() in txd_set for name in tex_names}


def check_txd_on_disk(tex_names: List[str], search_dirs: List[str]) -> Dict[str, Optional[str]]: #vers 1
    """Check which texture names have a matching .txd file on disk.
    Returns dict: {tex_name: full_path_or_None}
    """
    result = {}
    for name in tex_names:
        found = None
        for d in search_dirs:
            for candidate in [name + '.txd', name + '.TXD']:
                path = os.path.join(d, candidate)
                if os.path.isfile(path):
                    found = path
                    break
            if found:
                break
        result[name] = found
    return result


def get_dff_texture_report(data: bytes, img_entries=None, search_dirs=None) -> dict: #vers 1
    """Full report: texture names + IMG check + disk check.
    Returns:
      {
        'textures': [str, ...],
        'in_img':   {name: bool},
        'on_disk':  {name: path_or_None},
      }
    """
    names = parse_dff_textures(data)
    in_img  = check_txd_in_img(names, img_entries or [])
    on_disk = check_txd_on_disk(names, search_dirs or [])
    return {'textures': names, 'in_img': in_img, 'on_disk': on_disk}
