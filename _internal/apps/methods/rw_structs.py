#!/usr/bin/env python3
#this belongs in apps/methods/rw_structs.py - Version: 1
# X-Seti - September24 2026 - IMG Factory 1.6 - RenderWare struct field layouts

"""
RenderWare struct field layouts (GTAMods wiki) for the common Struct
sections, so a hex editor can show and edit them as named fields:
Clump, Frame List, Geometry, Material, Texture, Atomic, Texture Native (D3D),
Texture Dictionary, Material List.
"""

##Methods list -
# decode_struct
# encode_field
# _layout
# _size

import struct
from typing import List, Tuple

# fmt codes: struct module letters, or 'sNN' fixed NUL-padded string
_T = {'i': 4, 'I': 4, 'H': 2, 'h': 2, 'B': 1, 'f': 4}


def _size(fmt): #vers 1
    return int(fmt[1:]) if fmt.startswith('s') else _T[fmt]


def _layout(parent_type: int, size: int, data: bytes, off: int, version: int) -> List[Tuple[str, int, str]]: #vers 1
    """[(name, offset in payload, fmt)] for a Struct under parent_type."""
    f = []
    if parent_type == 0x10:                                  # Clump
        f = [("atomics", 0, 'i')]
        if size >= 12:
            f += [("lights", 4, 'i'), ("cameras", 8, 'i')]
    elif parent_type == 0x0E:                                # Frame List
        n = struct.unpack_from('<i', data, off)[0] if size >= 4 else 0
        f = [("frames", 0, 'i')]
        for k in range(max(0, min(n, (size - 4) // 56))):
            b = 4 + k * 56
            for j, nm in enumerate(("right.x", "right.y", "right.z", "up.x", "up.y", "up.z",
                                    "at.x", "at.y", "at.z", "pos.x", "pos.y", "pos.z")):
                f.append((f"[{k}] {nm}", b + j * 4, 'f'))
            f += [(f"[{k}] parent", b + 48, 'i'), (f"[{k}] flags", b + 52, 'I')]
    elif parent_type == 0x0F:                                # Geometry
        f = [("flags", 0, 'H'), ("uv sets", 2, 'B'), ("native flags", 3, 'B'),
             ("triangles", 4, 'i'), ("vertices", 8, 'i'), ("morph targets", 12, 'i')]
        if version < 0x34000 and size >= 28:                 # pre-3.4 lighting floats
            f += [("ambient", 16, 'f'), ("specular", 20, 'f'), ("diffuse", 24, 'f')]
    elif parent_type == 0x07:                                # Material
        f = [("flags", 0, 'i'), ("colour r", 4, 'B'), ("colour g", 5, 'B'), ("colour b", 6, 'B'),
             ("colour a", 7, 'B'), ("unused", 8, 'i'), ("textured", 12, 'i')]
        if size >= 28:
            f += [("ambient", 16, 'f'), ("specular", 20, 'f'), ("diffuse", 24, 'f')]
    elif parent_type == 0x06:                                # Texture
        f = [("filter", 0, 'B'), ("address uv", 1, 'B'), ("mipmaps", 2, 'H')]
    elif parent_type == 0x14:                                # Atomic
        f = [("frame", 0, 'i'), ("geometry", 4, 'i'), ("flags", 8, 'i'), ("unused", 12, 'i')]
    elif parent_type == 0x08:                                # Material List
        n = struct.unpack_from('<i', data, off)[0] if size >= 4 else 0
        f = [("materials", 0, 'i')] + [(f"[{k}] instance of", 4 + k * 4, 'i')
                                       for k in range(max(0, min(n, (size - 4) // 4)))]
    elif parent_type == 0x16:                                # Texture Dictionary
        f = [("textures", 0, 'H'), ("device id", 2, 'H')]
    elif parent_type == 0x15 and size >= 88:                 # Texture Native (PC D3D8/9)
        f = [("platform", 0, 'I'), ("filter", 4, 'B'), ("address uv", 5, 'B'), ("pad", 6, 'H'),
             ("name", 8, 's32'), ("mask", 40, 's32'), ("raster format", 72, 'I'),
             ("d3d format / alpha", 76, 'I'), ("width", 80, 'H'), ("height", 82, 'H'),
             ("depth", 84, 'B'), ("mip levels", 85, 'B'), ("raster type", 86, 'B'), ("compression", 87, 'B')]
    return [x for x in f if x[1] + _size(x[2]) <= size]


def decode_struct(data: bytes, node, parent_type: int, version: int): #vers 1
    """[(name, absolute offset, fmt, value)] for a Struct node; [] if the layout is unknown."""
    base = node.data_start
    out = []
    for name, rel, fmt in _layout(parent_type, node.size, data, base, version):
        o = base + rel
        if fmt.startswith('s'):
            v = bytes(data[o:o + _size(fmt)]).split(b"\0", 1)[0].decode('latin-1')
        else:
            v = struct.unpack_from('<' + fmt, data, o)[0]
        out.append((name, o, fmt, v))
    return out


def encode_field(fmt: str, text: str) -> bytes: #vers 1
    """Bytes for an edited field value; raises ValueError if it doesn't fit."""
    if fmt.startswith('s'):
        n = _size(fmt)
        raw = text.encode('latin-1')
        if len(raw) >= n:
            raise ValueError(f"at most {n - 1} characters")
        return raw + b"\0" * (n - len(raw))
    if fmt == 'f':
        return struct.pack('<f', float(text))
    return struct.pack('<' + fmt, int(text, 0))
