#this belongs in apps/methods/lcs_geometry.py - Version: 2
# X-Seti - October 07 2026 - IMG Factory 1.6 - LCS/VCS mobile native geometry

"""
GTA LCS mobile/PSP DFFs: RW clump whose geometry chunk (version 0xF00D)
holds PS2-style VIF packets. Decoded to DFFModel for Map Workshop.
"""

##Methods list -
# _batches
# _chunks
# _geometry
# _material_names
# _strip_triangles
# is_lcs_dff
# parse_lcs_dff

import struct
from typing import List, Optional, Tuple

import numpy as np

from apps.methods.dff_classes import (BoundingSphere, DFFModel, Geometry, Material,
                                      RGBA, TexCoord, Triangle, Vector3)

_NATIVE_VER = 0xF00D
_CLUMP, _ATOMIC, _GEOMETRY, _MATLIST, _MATERIAL, _TEXTURE, _STRING = 0x10, 0x14, 0x0F, 0x08, 0x07, 0x06, 0x02
_HEADER, _MESH = 0x40, 0x30
# VIF unpack: format nibble -> (components, bits)
_UNPACK = {0x0: (1, 32), 0x1: (1, 16), 0x2: (1, 8), 0x4: (2, 32), 0x5: (2, 16), 0x6: (2, 8),
           0x8: (3, 32), 0x9: (3, 16), 0xA: (3, 8), 0xC: (4, 32), 0xD: (4, 16), 0xE: (4, 8),
           0xF: (4, 5)}
# RW geometry flags: positions, textured, prelit, light
_GEOM_FLAGS = 0x02 | 0x04 | 0x08 | 0x20


def _chunks(d: bytes, off: int, end: int):
    """(type, data offset, size, version) of RW chunks in a range."""
    while off + 12 <= end:
        t, s, v = struct.unpack_from('<III', d, off)
        if s > end - off - 12:
            return
        yield t, off + 12, s, v
        off += 12 + s


def is_lcs_dff(data: bytes) -> bool: #vers 2
    """True for a clump with an LCS native (0xF00D) geometry chunk."""
    if len(data) < 24 or struct.unpack_from('<I', data, 0)[0] != _CLUMP:
        return False
    tag, i = struct.pack('<I', _NATIVE_VER), 0
    while True:
        i = data.find(tag, i + 1)
        if i < 0:
            return False
        if i >= 8 and struct.unpack_from('<I', data, i - 8)[0] == _GEOMETRY:
            return True


def _material_names(d: bytes, off: int, end: int) -> List[str]: #vers 1
    """Texture name per material from a material list chunk."""
    names = []
    for t, o, s, _v in _chunks(d, off, end):
        if t != _MATERIAL:
            continue
        name = ''
        for t2, o2, s2, _v2 in _chunks(d, o, o + s):
            if t2 == _TEXTURE:
                strings = [d[o3:o3 + s3].split(b'\0', 1)[0].decode('latin-1')
                           for t3, o3, s3, _v3 in _chunks(d, o2, o2 + s2) if t3 == _STRING]
                name = strings[0] if strings else ''
        names.append(name)
    return names


def _batches(g: bytes, off: int, end: int, pos_formats=(0x8, 0x9), pos_slot=None) -> List[dict]: #vers 3
    """VIF unpacks of one mesh grouped into strips (one per position unpack)."""
    out, cur = [], None
    while off + 4 <= end:
        imm, num, cmd = struct.unpack_from('<HBB', g, off)
        c = cmd & 0x7F
        if c >= 0x60:
            comps, bits = _UNPACK.get(c & 0xF, (0, 0))
            n = num or 256
            size = ((comps * bits * n + 31) // 32) * 4
            raw = g[off + 4:off + 4 + size]
            fmt = c & 0xF
            if fmt in pos_formats and (pos_slot is None or (imm & 0x3FF) == pos_slot):
                cur = {'n': n, 'pos': (raw, bits)}
                out.append(cur)
            elif cur is not None and fmt in (0x4, 0x5, 0x6):
                cur['uv'] = (raw, bits)
            elif cur is not None and fmt == 0xA:
                cur['nrm'] = raw
            elif cur is not None and fmt in (0xF, 0xE) and 'col' not in cur:
                cur['col'] = (raw, fmt)
            off += 4 + size
            continue
        if c in (0x50, 0x51):                          # DIRECT
            off += 4 + imm * 16
        elif c == 0x20:                                # STMASK
            off += 8
        elif c in (0x30, 0x31):                        # STROW / STCOL
            off += 20
        else:
            off += 4
    return out


def _strip_triangles(n: int, base: int) -> List[Tuple[int, int, int]]: #vers 1
    """Triangle strip to triangles, alternate winding, degenerates kept out by caller."""
    return [(base + i, base + i + 1, base + i + 2) if i % 2 == 0 else
            (base + i + 1, base + i, base + i + 2) for i in range(n - 2)]


def _geometry(g: bytes, names: List[str]) -> Geometry: #vers 1
    """One native geometry block to a Geometry (all meshes merged)."""
    packed, = struct.unpack_from('<I', g, 0x10)
    num_meshes, data_end = packed >> 20, packed & 0xFFFFF
    packets = struct.unpack_from('<H', g, 0x1A)[0]
    scale = np.array(struct.unpack_from('<3f', g, 0x28))
    pivot = np.array(struct.unpack_from('<3f', g, 0x34))
    geom = Geometry(flags=_GEOM_FLAGS, uv_layer_count=1)
    verts, uvs, cols = [], [], []
    for m in range(num_meshes):
        mo = _HEADER + m * _MESH
        us, vs = struct.unpack_from('<2f', g, mo + 0x10)
        dma, ntris, mat = struct.unpack_from('<IHh', g, mo + 0x1C)
        tag = packets + dma
        qwc = struct.unpack_from('<I', g, tag)[0] & 0xFFFF
        for b in _batches(g, tag + 16, min(tag + 16 + qwc * 16, data_end)):
            n = b['n']
            raw, bits = b['pos']
            if bits == 16:
                p = np.frombuffer(raw, '<i2', n * 3).reshape(n, 3) / 32768.0 * scale + pivot
            else:
                p = np.frombuffer(raw, '<f4', n * 3).reshape(n, 3)
            if 'uv' in b:
                raw, ubits = b['uv']
                if ubits == 8:
                    t = np.frombuffer(raw, np.uint8, n * 2).reshape(n, 2) / 128.0
                else:
                    t = np.frombuffer(raw, '<i2', n * 2).reshape(n, 2) / 4096.0
                t = t * (us, vs)
            else:
                t = np.zeros((n, 2))
            if 'col' in b and b['col'][1] == 0xF:
                c = np.frombuffer(b['col'][0], '<u2', n)
                rgba = np.stack([(c & 31) * 255 // 31, ((c >> 5) & 31) * 255 // 31,
                                 ((c >> 10) & 31) * 255 // 31, np.where(c >> 15, 255, 255)], 1)
            else:
                rgba = np.full((n, 4), 255)
            base = len(verts)
            verts.extend(map(tuple, p.tolist()))
            uvs.extend(map(tuple, t.tolist()))
            cols.extend(map(tuple, rgba.tolist()))
            for a, b_, c_ in _strip_triangles(n, base):
                if verts[a] != verts[b_] and verts[b_] != verts[c_] and verts[a] != verts[c_]:
                    geom.triangles.append(Triangle(a, b_, c_, max(mat, 0)))
    geom.vertices = [Vector3(*v) for v in verts]
    geom.uv_layers = [[TexCoord(u, v) for u, v in uvs]]
    geom.colors = [RGBA(*map(int, c)) for c in cols]
    for name in names or ['']:
        mat_ = Material(texture_name=name)
        mat_.colour = mat_.color
        geom.materials.append(mat_)
    bx, by, bz, br = struct.unpack_from('<4f', g, 0)
    geom.bounding_sphere = BoundingSphere(Vector3(bx, by, bz), br)
    return geom


def parse_lcs_dff(data: bytes, name: str = '') -> Optional[DFFModel]: #vers 1
    """LCS mobile/PSP DFF to DFFModel, or None when no native geometry."""
    model = DFFModel(source_path=name)
    for t, o, s, _v in _chunks(data, 0, len(data)):
        if t != _CLUMP:
            continue
        for t2, o2, s2, _v2 in _chunks(data, o, o + s):
            if t2 != _ATOMIC:
                continue
            for t3, o3, s3, v3 in _chunks(data, o2, o2 + s2):
                if t3 != _GEOMETRY or v3 != _NATIVE_VER:
                    continue
                g = data[o3:o3 + s3]
                data_end = struct.unpack_from('<I', g, 0x10)[0] & 0xFFFFF
                names = []
                for t4, o4, s4, _v4 in _chunks(g, data_end, len(g)):
                    if t4 == _MATLIST:
                        names = _material_names(g, o4, o4 + s4)
                model.geometries.append(_geometry(g, names))
        break
    return model if model.geometries else None
