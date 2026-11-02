#!/usr/bin/env python3
#this belongs in apps/methods/rw_structs.py - Version: 2
# X-Seti - September24 2026 - IMG Factory 1.6 - RenderWare struct field layouts

"""
RenderWare struct field layouts (GTAMods wiki) for the common Struct
sections, so a hex editor can show and edit them as named fields:
Clump, Frame List, Geometry, Material, Texture, Atomic, Texture Native (D3D),
Texture Dictionary, Material List, Geometry List, Light, Camera; and the
plugin sections Morph, Skin, HAnim, Material Effects, Bin Mesh, Frame,
2D Effect, Pipeline Set, Specular / Reflection Material, Night / Extra
Vertex Colours, Breakable, User Data.
"""

##Methods list -
# decode_section
# decode_struct
# encode_field
# _layout
# _plugin_layout
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
    elif parent_type == 0x1A:                                # Geometry List
        f = [("geometries", 0, 'i')]
    elif parent_type == 0x12:                                # Light
        f = [("radius", 0, 'f'), ("red", 4, 'f'), ("green", 8, 'f'), ("blue", 12, 'f'),
             ("minus cos angle", 16, 'f'), ("flags", 20, 'H'), ("type", 22, 'H')]
    elif parent_type == 0x05:                                # Camera
        f = [("view window x", 0, 'f'), ("view window y", 4, 'f'), ("view offset x", 8, 'f'),
             ("view offset y", 12, 'f'), ("near plane", 16, 'f'), ("far plane", 20, 'f'),
             ("fog plane", 24, 'f'), ("projection", 28, 'i')]
    elif parent_type == 0x15 and size >= 88:                 # Texture Native (PC D3D8/9)
        f = [("platform", 0, 'I'), ("filter", 4, 'B'), ("address uv", 5, 'B'), ("pad", 6, 'H'),
             ("name", 8, 's32'), ("mask", 40, 's32'), ("raster format", 72, 'I'),
             ("d3d format / alpha", 76, 'I'), ("width", 80, 'H'), ("height", 82, 'H'),
             ("depth", 84, 'B'), ("mip levels", 85, 'B'), ("raster type", 86, 'B'), ("compression", 87, 'B')]
    return [x for x in f if x[1] + _size(x[2]) <= size]


def _plugin_layout(t: int, size: int, data: bytes, off: int) -> List[Tuple[str, int, str]]: #vers 1
    """[(name, offset in payload, fmt)] for a plugin / data section of type t."""
    u = lambda o: struct.unpack_from('<I', data, off + o)[0] if o + 4 <= size else 0
    f = []
    if t == 0x0105:                                          # Morph PLG
        f = [("morph target", 0, 'i')]
    elif t == 0x0116:                                        # Skin PLG
        f = [("bones", 0, 'B'), ("used bones", 1, 'B'), ("max weights per vertex", 2, 'B'), ("pad", 3, 'B')]
    elif t == 0x011E:                                        # HAnim PLG
        f = [("version", 0, 'I'), ("node id", 4, 'i'), ("bones", 8, 'i')]
        n = u(8)
        if n and size >= 20:
            f += [("flags", 12, 'I'), ("key frame size", 16, 'I')]
            for k in range(min(n, (size - 20) // 12)):
                b = 20 + k * 12
                f += [(f"[{k}] node id", b, 'i'), (f"[{k}] index", b + 4, 'i'), (f"[{k}] flags", b + 8, 'I')]
    elif t == 0x0120:                                        # Material Effects PLG
        f = [("effect type", 0, 'I')]
        if size >= 8:
            f.append(("effect 1 type", 4, 'I'))
    elif t == 0x050E:                                        # Bin Mesh PLG
        f = [("flags (1 = strips)", 0, 'I'), ("meshes", 4, 'I'), ("indices", 8, 'I')]
        meshes, total = u(4), u(8)
        native = size == 12 + meshes * 8 and total
        b = 12
        for k in range(min(meshes, 4096)):
            if b + 8 > size:
                break
            f += [(f"[{k}] indices", b, 'I'), (f"[{k}] material", b + 4, 'I')]
            b += 8 + (0 if native else u(b) * 4)
    elif t == 0x0253F2FE:                                    # Frame (name, no terminator)
        f = [("name", 0, f's{size}')] if size else []
    elif t == 0x0253F2F3:                                    # Pipeline Set
        f = [("pipeline id", 0, 'I')]
    elif t == 0x0253F2F6:                                    # Specular Material
        f = [("level", 0, 'f'), ("texture", 4, 's24')]
    elif t == 0x0253F2FC:                                    # Reflection Material
        f = [("scale x", 0, 'f'), ("scale y", 4, 'f'), ("offset x", 8, 'f'), ("offset y", 12, 'f'),
             ("intensity", 16, 'f'), ("environment texture", 20, 'I')]
    elif t in (0x0253F2F4, 0x0253F2F9):                      # Night / Extra Vertex Colours
        f = [("has colours", 0, 'I')]
    elif t == 0x0253F2FD:                                    # Breakable
        f = [("flags", 0, 'I')]
        if size >= 56:
            f += [("position rule", 4, 'I'), ("vertices", 8, 'H'), ("triangles", 18, 'H'), ("materials", 26, 'H')]
    elif t == 0x0253F2F8:                                    # 2D Effect
        f = [("entries", 0, 'I')]
        b = 4
        for k in range(min(u(0), 1024)):
            if b + 20 > size:
                break
            f += [(f"[{k}] pos x", b, 'f'), (f"[{k}] pos y", b + 4, 'f'), (f"[{k}] pos z", b + 8, 'f'),
                  (f"[{k}] type", b + 12, 'I'), (f"[{k}] data size", b + 16, 'I')]
            b += 20 + u(b + 16)
    elif t == 0x011F:                                        # User Data PLG: sets and values
        f = [("sets", 0, 'I')]
        b = 4
        for k in range(min(u(0), 256)):
            if b + 4 > size:
                break
            nl = u(b)
            f.append((f"[{k}] name", b + 4, f's{nl}') if nl else (f"[{k}] name length", b, 'I'))
            b += 4 + nl
            if b + 8 > size:
                break
            kind, cnt = u(b), u(b + 4)
            f += [(f"[{k}] type (1 int, 2 float, 3 string)", b, 'I'), (f"[{k}] values", b + 4, 'I')]
            b += 8
            for j in range(min(cnt, 256)):
                if kind in (1, 2):
                    f.append((f"[{k}] value {j}", b, 'i' if kind == 1 else 'f'))
                    b += 4
                elif kind == 3:
                    sl = u(b)
                    if sl:
                        f.append((f"[{k}] value {j}", b + 4, f's{sl}'))
                    b += 4 + sl
                else:
                    break
    return [x for x in f if x[1] + _size(x[2]) <= size]


def decode_section(data: bytes, node, version: int): #vers 1
    """Named fields for a Struct (by parent type) or a known plugin section; [] if unknown."""
    if node is None:
        return []
    if node.type == 0x01 and node.parent is not None:
        return decode_struct(data, node, node.parent.type, version)
    base, out = node.data_start, []
    for name, rel, fmt in _plugin_layout(node.type, node.size, data, base):
        o = base + rel
        if fmt.startswith('s'):
            v = bytes(data[o:o + _size(fmt)]).split(b"\0", 1)[0].decode('latin-1')
        else:
            v = struct.unpack_from('<' + fmt, data, o)[0]
        out.append((name, o, fmt, v))
    return out


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
