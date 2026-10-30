#this belongs in apps/methods/bully_world.py - Version: 1
# X-Seti - October 21 2026 - IMG Factory 1.6 - Bully world (.idb / .ipb)

"""
Bully Scholarship Edition world: Objects/ide.img (.idb objects) and
Stream/World.img (.ipb placements, .nif models, .nft textures).
"""

##Methods list -
# _cstr
# bully_paths
# parse_idb
# parse_ipb
# parse_text_ide
# read_img_entries

import os
import struct
from typing import Dict, List, Optional, Tuple

# .ipb section record sizes (inst: placed objects, spec: special/scripted objects)
_IPB_STRIDE = {b'inst': 120, b'spec': 128}


def bully_paths(root: str) -> Optional[Tuple[str, str]]: #vers 1
    """(Objects/ide.img, Stream/World.img) under a Bully root, or None."""
    from apps.methods.gta_dat_parser import _resolve_ci
    if not root or not os.path.isdir(root):
        return None
    ide, world = _resolve_ci(root, 'Objects/ide.img'), _resolve_ci(root, 'Stream/World.img')
    return (ide, world) if ide and world else None


def _cstr(d: bytes, o: int) -> Tuple[str, int]: #vers 1
    """Null-terminated string padded to 4 bytes; (text, next offset)."""
    e = d.index(b'\0', o)
    return d[o:e].decode('latin-1'), o + ((e - o) // 4 + 1) * 4


def parse_idb(data: bytes) -> List[dict]: #vers 1
    """objs / tobj records of a binary .idb (other sections skipped)."""
    size = struct.unpack_from('<I', data, 0)[0]
    o, out = 4, []
    while o + 12 <= size:
        tag = data[o:o + 4][::-1]
        count = struct.unpack_from('<I', data, o + 4)[0]
        if tag not in (b'objs', b'tobj'):
            break
        o += 12
        for _ in range(count):
            mid = struct.unpack_from('<I', data, o)[0]
            name, o = _cstr(data, o + 4)
            txd, o = _cstr(data, o)
            mesh = struct.unpack_from('<I', data, o)[0]
            dists = struct.unpack_from(f'<{mesh}f', data, o + 4)
            o += 4 + 4 * mesh
            flags = struct.unpack_from('<I', data, o)[0]
            o += 36                                     # flags + 32 unknown bytes
            rec = {'section': tag.decode(), 'id': mid, 'name': name, 'txd': txd,
                   'mesh_count': mesh, 'dists': dists, 'flags': flags}
            if tag == b'tobj':
                rec['time_on'], rec['time_off'] = struct.unpack_from('<2i', data, o)
                o += 8
            out.append(rec)
        o += 4                                          # u32 before the next section tag
    return out


def parse_ipb(data: bytes) -> List[dict]: #vers 1
    """inst / spec placements of a binary .ipb (later script sections skipped)."""
    if data[:4] != b'Ipl$':
        raise ValueError("not a Bully .ipb (no Ipl$ header)")
    o, out = 0x17, []
    while o + 8 <= len(data):
        tag = data[o:o + 4]
        stride = _IPB_STRIDE.get(tag)
        if stride is None:
            break
        count = struct.unpack_from('<I', data, o + 4)[0]
        o += 8
        for i in range(count):
            r = o + i * stride
            mid = struct.unpack_from('<I', data, r)[0]
            name = data[r + 4:r + 68].split(b'\0', 1)[0].decode('latin-1')
            pos = struct.unpack_from('<3f', data, r + 72)
            scale = struct.unpack_from('<3f', data, r + 84)
            rot = struct.unpack_from('<4f', data, r + 96)
            out.append({'section': tag.decode(), 'id': mid, 'name': name, 'pos': pos,
                        'scale': scale, 'rot': rot, 'offset': r})
        o += count * stride
    return out


def parse_text_ide(text: str) -> List[dict]: #vers 1
    """id, model, txd of every numbered line in a text .ide (weap, panm, item ...)."""
    import re
    out, section = [], ''
    for line in text.splitlines():
        line = line.split('#', 1)[0].strip()
        if not line:
            continue
        parts = [p for p in re.split(r'[,\s]+', line) if p]
        if len(parts) == 1 and not parts[0][0].isdigit():
            section = '' if parts[0].lower() == 'end' else parts[0].lower()
            continue
        if section and parts[0].isdigit() and len(parts) >= 3:
            out.append({'section': section, 'id': int(parts[0]), 'name': parts[1], 'txd': parts[2]})
    return out


def read_img_entries(img_path: str, ext: str) -> List[Tuple[str, bytes]]: #vers 1
    """(name, bytes) of every entry with this extension in a Bully .dir/.img."""
    from apps.methods.img_core_classes import IMGFile
    img = IMGFile(img_path)
    if not img.open():
        raise IOError(f"can't open {img_path}")
    return [(e.name, img.read_entry_data(e)) for e in img.entries
            if e.name.lower().endswith(ext)]
