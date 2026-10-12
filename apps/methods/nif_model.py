#this belongs in apps/methods/nif_model.py - Version: 2
# X-Seti - October 12 2026 - IMG Factory 1.6 - Gamebryo NIF models

"""
Bully PC Gamebryo .nif (20.3.0.9) geometry to DFFModel: NiNode tree,
NiTriShape / NiTriStrips, base texture names from NiSourceTexture.
"""

##Methods list -
# _av_object
# _base_texture
# _geometry_data
# _material
# _shape
# _strip_tris
# is_nif_model
# parse_nif_model

import struct
from typing import List, Optional, Tuple

import numpy as np

from apps.methods.dff_classes import (BoundingSphere, DFFModel, Geometry, Material,
                                      RGBA, TexCoord, Triangle, Vector3)
from apps.methods.nif_textures import _MAGIC, _blocks, _endian

_SHAPES = ('NiTriShape', 'NiTriStrips')
_FLAGS_BASE = 0x02 | 0x04 | 0x20          # positions, textured, light


def is_nif_model(data: bytes) -> bool: #vers 1
    """True for a Gamebryo NIF holding triangle geometry."""
    return data[:20] == _MAGIC and (b'NiTriShape' in data[:4096] or b'NiTriStrips' in data[:4096])


def _av_object(d: bytes, o: int, E: str = '<') -> Tuple[np.ndarray, List[int], int]: #vers 1
    """(4x4 local matrix, property refs, offset after) of an NiAVObject."""
    p = o + 4
    nx = struct.unpack_from(E + 'I', d, p)[0]
    p += 4 + 4 * nx + 4 + 2                       # extra data, controller, flags
    t = struct.unpack_from(E + '3f', d, p); p += 12
    r = np.array(struct.unpack_from(E + '9f', d, p)).reshape(3, 3); p += 36
    s = struct.unpack_from(E + 'f', d, p)[0]; p += 4
    n = struct.unpack_from(E + 'I', d, p)[0]; p += 4
    props = list(struct.unpack_from(E + f'{n}i', d, p)); p += 4 * n + 4   # + collision
    m = np.identity(4)
    m[:3, :3] = r * s                              # row-vector convention: v @ m
    m[3, :3] = t
    return m, props, p


def _strip_tris(strips: List[List[int]]) -> List[Tuple[int, int, int]]: #vers 1
    """Triangle list from strips, alternating winding, degenerates dropped."""
    out = []
    for s in strips:
        for i in range(len(s) - 2):
            a, b, c = (s[i], s[i + 1], s[i + 2]) if i % 2 == 0 else (s[i], s[i + 2], s[i + 1])
            if a != b and b != c and a != c:
                out.append((a, b, c))
    return out


def _geometry_data(d: bytes, o: int, strips: bool, E: str = '<'): #vers 1
    """(verts, normals, colours, uvs, tris) of NiTriShapeData / NiTriStripsData."""
    p = o + 4
    nv = struct.unpack_from(E + 'H', d, p)[0]; p += 4
    has = d[p]; p += 1
    verts = np.frombuffer(d, E + 'f4', nv * 3, p).reshape(nv, 3) if has else np.zeros((nv, 3))
    p += 12 * nv if has else 0
    vflags = struct.unpack_from(E + 'H', d, p)[0]; p += 2
    has = d[p]; p += 1
    norms = None
    if has:
        norms = np.frombuffer(d, E + 'f4', nv * 3, p).reshape(nv, 3); p += 12 * nv
        if vflags & 0x1000:                        # tangents + bitangents
            p += 24 * nv
    p += 16                                        # bound centre + radius
    has = d[p]; p += 1
    cols = None
    if has:
        cols = np.frombuffer(d, E + 'f4', nv * 4, p).reshape(nv, 4); p += 16 * nv
    nuv = vflags & 0x3F
    uvs = np.frombuffer(d, E + 'f4', nv * 2, p).reshape(nv, 2) if nuv else None
    p += 8 * nv * nuv + 2 + 4                      # consistency, additional data
    if strips:
        _nt, ns = struct.unpack_from(E + 'HH', d, p); p += 4
        lens = struct.unpack_from(E + f'{ns}H', d, p); p += 2 * ns
        tris = []
        if d[p]:
            p += 1
            sl = []
            for n in lens:
                sl.append(list(struct.unpack_from(E + f'{n}H', d, p))); p += 2 * n
            tris = _strip_tris(sl)
    else:
        nt = struct.unpack_from(E + 'H', d, p)[0]; p += 2 + 4
        tris = []
        if d[p]:
            tris = [tuple(t) for t in np.frombuffer(d, E + 'u2', nt * 3, p + 1).reshape(nt, 3).tolist()]
    return verts, norms, cols, uvs, tris


def _base_texture(d: bytes, types, offs, strs, ref: int, E: str = '<') -> str: #vers 1
    """Base texture file name (no folder/extension) of an NiTexturingProperty."""
    p = offs[ref] + 4
    nx = struct.unpack_from(E + 'I', d, p)[0]
    p += 4 + 4 * nx + 4 + 2 + 4                    # NET, flags, texture count
    if not d[p]:
        return ''
    src = struct.unpack_from(E + 'i', d, p + 1)[0]
    if not (0 <= src < len(types)) or types[src] != 'NiSourceTexture':
        return ''
    q = offs[src] + 4
    nx = struct.unpack_from(E + 'I', d, q)[0]
    q += 4 + 4 * nx + 4
    sidx = struct.unpack_from(E + 'i', d, q + 1)[0]
    name = strs[sidx] if 0 <= sidx < len(strs) else ''
    return name.replace('\\', '/').rsplit('/', 1)[-1].rsplit('.', 1)[0]


def _material(d: bytes, types, offs, strs, props: List[int], E: str = '<') -> Material: #vers 1
    """Material from a shape's properties (texture, colour, alpha)."""
    mat = Material()
    for ref in props:
        if not (0 <= ref < len(types)):
            continue
        t = types[ref]
        if t == 'NiTexturingProperty':
            mat.texture_name = _base_texture(d, types, offs, strs, ref, E)
        elif t == 'NiMaterialProperty':
            nx = struct.unpack_from(E + 'I', d, offs[ref] + 4)[0]
            base = offs[ref] + 12 + 4 * nx              # after NiObjectNET
            dr, dg, db = struct.unpack_from(E + '3f', d, base + 12)    # diffuse
            alpha = struct.unpack_from(E + 'f', d, base + 52)[0]
            mat.color = RGBA(*(int(max(0, min(1, c)) * 255) for c in (dr, dg, db, alpha)))
    mat.colour = mat.color
    return mat


def _shape(d: bytes, types, offs, strs, i: int, world: np.ndarray, E: str = '<') -> Optional[Geometry]: #vers 1
    """One NiTriShape / NiTriStrips to a Geometry in model space."""
    m, props, p = _av_object(d, offs[i], E)
    data_ref = struct.unpack_from(E + 'i', d, p)[0]
    if not (0 <= data_ref < len(types)) or not types[data_ref].endswith('Data'):
        return None
    verts, norms, cols, uvs, tris = _geometry_data(
        d, offs[data_ref], types[data_ref] == 'NiTriStripsData', E)
    if not len(verts) or not tris:
        return None
    mw = m @ world
    v = verts.astype(float) @ mw[:3, :3] + mw[3, :3]
    geom = Geometry(flags=_FLAGS_BASE | (0x10 if norms is not None else 0)
                    | (0x08 if cols is not None else 0), uv_layer_count=1)
    geom.vertices = [Vector3(*r) for r in v.tolist()]
    if norms is not None:
        nr = norms.astype(float) @ mw[:3, :3]
        nr /= np.maximum(np.linalg.norm(nr, axis=1, keepdims=True), 1e-9)
        geom.normals = [Vector3(*r) for r in nr.tolist()]
    if cols is not None:
        c = (np.clip(cols, 0, 1) * 255).astype(int).tolist()
        geom.colors = [RGBA(*r) for r in c]
    geom.uv_layers = [[TexCoord(u, vv) for u, vv in (uvs.tolist() if uvs is not None
                                                      else [(0.0, 0.0)] * len(v))]]
    geom.triangles = [Triangle(a, b, c_, 0) for a, b, c_ in tris]
    geom.materials = [_material(d, types, offs, strs, props, E)]
    centre = (v.min(0) + v.max(0)) / 2.0
    geom.bounding_sphere = BoundingSphere(Vector3(*centre.tolist()),
                                          float(np.linalg.norm(v - centre, axis=1).max()))
    return geom


def parse_nif_model(data: bytes, name: str = '') -> Optional[DFFModel]: #vers 2
    """Gamebryo NIF to DFFModel, all shapes placed by their node chain."""
    if data[:20] != _MAGIC:
        return None
    types, offs, _sizes, strs = _blocks(data)
    E = _endian(data)
    model = DFFModel(source_path=name)
    seen = set()
    stack = [(0, np.identity(4))]
    while stack:
        i, world = stack.pop()
        if i in seen or not (0 <= i < len(types)):
            continue
        seen.add(i)
        t = types[i]
        if t in _SHAPES:
            g = _shape(data, types, offs, strs, i, world, E)
            if g is not None:
                model.geometries.append(g)
        elif t == 'NiNode' or t.endswith('Node'):
            m, _props, p = _av_object(data, offs[i], E)
            n = struct.unpack_from(E + 'I', data, p)[0]
            for c in struct.unpack_from(E + f'{n}i', data, p + 4):
                stack.append((c, m @ world))
    return model if model.geometries else None
