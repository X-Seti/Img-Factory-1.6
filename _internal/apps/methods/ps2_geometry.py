#this belongs in apps/methods/ps2_geometry.py - Version: 1
# X-Seti - October 08 2026 - IMG Factory 1.6 - PS2 native DFF geometry

"""
GTA III/VC/SA PS2 DFFs: geometry vertex data lives in the Native Data
PLG (0x510) as DMA-chained VIF packets. Decoded to DFFModel.
"""

##Methods list -
# _batch_arrays
# _fit_scale
# _geometry
# _mesh_blocks
# _vif_stream
# is_ps2_native_dff
# parse_ps2_native_dff

import struct
from typing import List, Optional, Tuple

import numpy as np

from apps.methods.dff_classes import (BoundingSphere, DFFModel, Geometry, Material,
                                      RGBA, TexCoord, Triangle, Vector3)
from apps.methods.lcs_geometry import (_ATOMIC, _CLUMP, _GEOMETRY, _GEOM_FLAGS,
                                       _MATLIST, _batches, _chunks, _material_names,
                                       _strip_triangles)

_GEOMLIST, _EXTENSION, _STRUCT = 0x1A, 0x03, 0x01
_BINMESH, _NATIVE = 0x50E, 0x510
_PLATFORM_PS2 = 4


def is_ps2_native_dff(data: bytes) -> bool: #vers 1
    """True for a clump whose geometry carries PS2 native data."""
    if len(data) < 24 or struct.unpack_from('<I', data, 0)[0] != _CLUMP:
        return False
    tag, i = struct.pack('<I', _NATIVE), 0
    while True:
        i = data.find(tag, i + 1)
        if i < 0:
            return False
        if i + 28 <= len(data) and struct.unpack_from('<I', data, i + 12)[0] == _STRUCT \
                and struct.unpack_from('<I', data, i + 24)[0] == _PLATFORM_PS2:
            return True


def _mesh_blocks(nd: bytes) -> List[bytes]: #vers 1
    """Per-mesh DMA blocks from a native data chunk body."""
    t, s, _v = struct.unpack_from('<III', nd, 0)
    if t != _STRUCT or struct.unpack_from('<I', nd, 12)[0] != _PLATFORM_PS2:
        return []
    out, p, end = [], 16, min(len(nd), 12 + s)
    while p + 8 <= end:
        size, _noptr = struct.unpack_from('<II', nd, p)
        p += 8
        out.append(nd[p:p + size])
        p += size
    return out


def _vif_stream(blk: bytes) -> bytes: #vers 2
    """VIF data of a mesh DMA chain; stops at ret/end (rest is slack)."""
    parts, q = [], 0
    while q + 16 <= len(blk):
        lo, addr = struct.unpack_from('<II', blk, q)
        qwc, tid = lo & 0xFFFF, (lo >> 28) & 7
        if tid == 3:                                   # ref: data elsewhere
            parts.append(blk[q + 8:q + 16])
            parts.append(blk[addr * 16:addr * 16 + qwc * 16])
            q += 16
            continue
        parts.append(blk[q + 8:q + 16 + qwc * 16])
        if tid in (0, 6, 7):                           # refe / ret / end
            break
        q += 16 + qwc * 16
    return b''.join(parts)


def _batch_arrays(b: dict) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]: #vers 2
    """Positions, UVs, RGBA and ADC (no-draw) flags for one VIF batch."""
    n = b['n']
    raw, bits = b['pos']
    adc = np.zeros(n, bool)
    if bits == 32:
        p = np.frombuffer(raw, '<f4', n * 3).reshape(n, 3).astype(float)
    elif len(raw) >= n * 8:                            # SA V4-16: xyz/128, w = ADC
        q = np.frombuffer(raw, '<i2', n * 4).reshape(n, 4)
        p = q[:, :3] / 128.0
        adc = (q[:, 3].view(np.uint16) & 0x8000) != 0
    else:
        p = np.frombuffer(raw, '<i2', n * 3).reshape(n, 3) / 128.0
    t = np.zeros((n, 2))
    if 'uv' in b:
        raw, ubits = b['uv']
        if ubits == 32:
            t = np.frombuffer(raw, '<f4', n * 2).reshape(n, 2).astype(float)
        elif ubits == 16:
            t = np.frombuffer(raw, '<i2', n * 2).reshape(n, 2) / 4096.0
    rgba = np.full((n, 4), 255)
    if 'col' in b and b['col'][1] == 0xE and len(b['col'][0]) >= n * 4:
        rgba = np.frombuffer(b['col'][0], np.uint8, n * 4).reshape(n, 4).astype(int)
        rgba[:, 3] = np.minimum(255, rgba[:, 3] * 2)
    return p, t, rgba, adc


def _geometry(geo: bytes, names: List[str]) -> Optional[Geometry]: #vers 4
    """One PS2 native geometry chunk body to a Geometry."""
    strip, nd, meshes = False, None, []
    for t, o, s, _v in _chunks(geo, 0, len(geo)):
        if t != _EXTENSION:
            continue
        for t2, o2, s2, _v2 in _chunks(geo, o, o + s):
            if t2 == _BINMESH:
                flags, count = struct.unpack_from('<II', geo, o2)
                strip = bool(flags & 1)
                meshes = [struct.unpack_from('<II', geo, o2 + 12 + k * 8) for k in range(count)]
            elif t2 == _NATIVE:
                nd = geo[o2:o2 + s2]
    if nd is None:
        return None
    geom = Geometry(flags=_GEOM_FLAGS, uv_layer_count=1)
    verts, uvs, cols, int_pos = [], [], [], False
    for mi, blk in enumerate(_mesh_blocks(nd)):
        need, mat = meshes[mi] if mi < len(meshes) else (1 << 30, mi)
        vif = _vif_stream(blk)
        for k, b in enumerate(_batches(vif, 0, len(vif), (0x8, 0x9, 0xD), 0)):
            if need <= 0:
                break
            lap = 2 if strip and k else 0               # strip batches overlap 2
            n = min(b['n'], need + lap)                 # drop padding vertices
            need -= n - lap
            p, t, rgba, adc = (x[:n] for x in _batch_arrays(b))
            int_pos = int_pos or b['pos'][1] == 16
            base = len(verts)
            verts.extend(map(tuple, p.tolist()))
            uvs.extend(map(tuple, t.tolist()))
            cols.extend(map(tuple, rgba.tolist()))
            if strip:
                tris = [tr for k, tr in enumerate(_strip_triangles(n, base)) if not adc[k + 2]]
            else:
                tris = [(base + i, base + i + 1, base + i + 2) for i in range(0, n - 2, 3)]
            for a, b_, c_ in tris:
                if verts[a] != verts[b_] and verts[b_] != verts[c_] and verts[a] != verts[c_]:
                    geom.triangles.append(Triangle(a, b_, c_, mat))
    if not verts:
        return None
    if int_pos:
        verts = _fit_scale(geo, verts, geom.triangles)
    geom.vertices = [Vector3(*v) for v in verts]
    geom.uv_layers = [[TexCoord(u, v) for u, v in uvs]]
    geom.colors = [RGBA(*map(int, c)) for c in cols]
    for name in names or ['']:
        mat_ = Material(texture_name=name)
        mat_.colour = mat_.color
        geom.materials.append(mat_)
    arr = np.array(verts)
    centre = (arr.min(0) + arr.max(0)) / 2.0
    radius = float(np.linalg.norm(arr - centre, axis=1).max())
    geom.bounding_sphere = BoundingSphere(Vector3(*centre.tolist()), radius)
    return geom


def _fit_scale(geo: bytes, verts: list, tris: list) -> list: #vers 2
    """Skinned SA models use 1/1024 not 1/128; pick by stored bound sphere."""
    t, s, v = struct.unpack_from('<III', geo, 0)
    lib = (((v >> 14) & 0x3FF00) + 0x30000) | ((v >> 16) & 0x3F) if v & 0xFFFF0000 else v << 8
    at = 12 + 16 + (12 if lib < 0x34000 else 0)        # old structs carry 3 light floats
    if t != _STRUCT or s + 12 < at + 16 or not tris:
        return verts
    cx, cy, cz, r = struct.unpack_from('<4f', geo, at)
    if not (r > 0):
        return verts
    used = np.array(verts)[np.unique([i for tr in tris for i in (tr.v1, tr.v2, tr.v3)])]
    reach = float(np.linalg.norm(used - (cx, cy, cz), axis=1).max())
    reach8 = float(np.linalg.norm(used / 8.0 - (cx, cy, cz), axis=1).max())
    if reach > r * 1.5 and abs(reach8 - r) < abs(reach - r):
        return [(x / 8.0, y / 8.0, z / 8.0) for x, y, z in verts]
    return verts


def parse_ps2_native_dff(data: bytes, name: str = '') -> Optional[DFFModel]: #vers 1
    """PS2 native DFF to DFFModel, or None when no native geometry."""
    model = DFFModel(source_path=name)
    for t, o, s, _v in _chunks(data, 0, len(data)):
        if t != _CLUMP:
            continue
        for t2, o2, s2, _v2 in _chunks(data, o, o + s):
            if t2 != _GEOMLIST:
                continue
            for t3, o3, s3, _v3 in _chunks(data, o2, o2 + s2):
                if t3 != _GEOMETRY:
                    continue
                geo = data[o3:o3 + s3]
                names = []
                for t4, o4, s4, _v4 in _chunks(geo, 0, len(geo)):
                    if t4 == _MATLIST:
                        names = _material_names(geo, o4, o4 + s4)
                g = _geometry(geo, names)
                if g is not None:
                    model.geometries.append(g)
        break
    return model if model.geometries else None
