#this belongs in apps/methods/nif_model.py - Version: 3
# X-Seti - October 12 2026 - IMG Factory 1.6 - Gamebryo NIF models

"""
Bully PC Gamebryo .nif (20.3.0.9) geometry to DFFModel: NiNode tree,
NiTriShape / NiTriStrips, base texture names from NiSourceTexture.
"""

##Methods list -
# _av_object
# _base_texture
# _data_block
# _data_layout
# _patch_block
# _same_arrays
# _geometry_data
# _material
# _shape
# _strip_tris
# is_nif_model
# parse_nif_model
# write_nif_model

import struct
from typing import List, Optional, Tuple

import numpy as np

from apps.methods.dff_classes import (BoundingSphere, DFFModel, Geometry, Material,
                                      RGBA, TexCoord, Triangle, Vector3)
from apps.methods.nif_textures import _MAGIC, _blocks, _endian, _string_table

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


def _shape(d: bytes, types, offs, strs, i: int, world: np.ndarray, E: str = '<') -> Optional[Geometry]: #vers 2
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
    geom._nif = (data_ref, types[data_ref] == 'NiTriStripsData', mw)   # for write_nif_model
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


def _data_layout(d: bytes, o: int, strips: bool, E: str = '<') -> dict: #vers 1
    """Byte positions inside NiTriShapeData / NiTriStripsData (same walk as _geometry_data)."""
    L = {'nv': o + 4}
    nv = struct.unpack_from(E + 'H', d, o + 4)[0]
    p = o + 8
    has = d[p]; p += 1
    L['verts'] = p if has else None
    p += 12 * nv if has else 0
    L['vflags'] = p
    vflags = struct.unpack_from(E + 'H', d, p)[0]; p += 2
    has = d[p]; p += 1
    L['norms'] = p if has else None
    if has:
        p += 12 * nv + (24 * nv if vflags & 0x1000 else 0)
    L['bound'] = p; p += 16
    has = d[p]; p += 1
    L['cols'] = p if has else None
    p += 16 * nv if has else 0
    L['uvs'] = p
    p += 8 * nv * (vflags & 0x3F)
    L['tail'] = p                                    # consistency + additional data
    L['tris'] = p + 6
    L.update(nv=nv, vflags=vflags)
    return L


def _data_block(d: bytes, o: int, strips: bool, geom, inv: np.ndarray, t: np.ndarray, E: str) -> bytes: #vers 2
    """New geometry data block (positions back in the shape's local space)."""
    L = _data_layout(d, o, strips, E)
    nv = len(geom.vertices)
    v = (np.array([[p.x, p.y, p.z] for p in geom.vertices], float) - t) @ inv
    keep_tan = bool(L['vflags'] & 0x1000) and nv == L['nv'] and L['norms'] is not None \
        and len(geom.normals) == nv                  # same vertices: tangents still valid
    vflags = L['vflags'] if keep_tan else L['vflags'] & ~0x1000
    nuv = vflags & 0x3F
    f = E + 'f4'
    out = bytearray(d[o:o + 4]) + struct.pack(E + 'H', nv) + d[o + 6:o + 8]
    out += bytes([1]) + v.astype(f).tobytes()
    out += struct.pack(E + 'H', vflags)
    if L['norms'] is not None and len(geom.normals) == nv:
        n = np.array([[p.x, p.y, p.z] for p in geom.normals], float) @ inv
        n /= np.maximum(np.linalg.norm(n, axis=1, keepdims=True), 1e-9)
        out += bytes([1]) + n.astype(f).tobytes()
        if keep_tan:
            out += d[L['norms'] + 12 * nv:L['norms'] + 36 * nv]
    else:
        out += bytes([0])
    c = (v.min(0) + v.max(0)) / 2 if nv else np.zeros(3)
    r = float(np.linalg.norm(v - c, axis=1).max()) if nv else 0.0
    out += np.array([*c, r], float).astype(f).tobytes()
    if L['cols'] is not None:
        cols = (np.array([[q.r, q.g, q.b, q.a] for q in geom.colors], float) / 255.0
                if len(geom.colors) == nv else np.ones((nv, 4)))
        out += bytes([1]) + cols.astype(f).tobytes()
    else:
        out += bytes([0])
    layers = list(geom.uv_layers or [])
    for k in range(nuv):
        uv = (np.array([[q.u, q.v] for q in layers[k]], float) if k < len(layers) and len(layers[k]) == nv
              else np.zeros((nv, 2)))
        out += uv.astype(f).tobytes()
    out += d[L['tail']:L['tail'] + 6]                 # consistency, additional data
    tris = [(q.v1, q.v2, q.v3) for q in geom.triangles]
    if strips:                                        # one 3-point strip per triangle
        out += struct.pack(E + 'HH', len(tris), len(tris))
        out += struct.pack(E + f'{len(tris)}H', *([3] * len(tris)))
        out += bytes([1]) + struct.pack(E + f'{3 * len(tris)}H', *[i for t3 in tris for i in t3])
    else:
        out += struct.pack(E + 'HI', len(tris), 3 * len(tris))
        out += bytes([1]) + struct.pack(E + f'{3 * len(tris)}H', *[i for t3 in tris for i in t3])
        out += struct.pack(E + 'H', 0)                # no match groups
    return bytes(out)


def _same_arrays(a, b) -> bool: #vers 1
    """Same positions, normals, UVs and colours."""
    def pts(x, y):
        return len(x) == len(y) and all((p.x, p.y, p.z) == (q.x, q.y, q.z) for p, q in zip(x, y))
    uv = lambda g: [(t.u, t.v) for l in (g.uv_layers or []) for t in l]
    col = lambda g: [(c.r, c.g, c.b, c.a) for c in (g.colors or [])]
    return pts(a.vertices, b.vertices) and pts(a.normals or [], b.normals or []) and uv(a) == uv(b) and col(a) == col(b)


def _patch_block(d: bytes, o: int, size: int, strips: bool, geom, inv, t, E: str) -> bytes: #vers 1
    """Same counts: positions, normals, UVs and colours written over the original block."""
    L = _data_layout(d, o, strips, E)
    nv = L['nv']
    f = E + 'f4'
    out = bytearray(d[o:o + size])
    def put(pos, arr):
        raw = arr.astype(f).tobytes()
        out[pos - o:pos - o + len(raw)] = raw
    v = (np.array([[p.x, p.y, p.z] for p in geom.vertices], float) - t) @ inv
    if L['verts'] is not None:
        put(L['verts'], v)
    if L['norms'] is not None and len(geom.normals) == nv:
        n = np.array([[p.x, p.y, p.z] for p in geom.normals], float) @ inv
        put(L['norms'], n / np.maximum(np.linalg.norm(n, axis=1, keepdims=True), 1e-9))
    c = (v.min(0) + v.max(0)) / 2
    put(L['bound'], np.array([*c, float(np.linalg.norm(v - c, axis=1).max())]))
    if L['cols'] is not None and len(geom.colors) == nv:
        put(L['cols'], np.array([[q.r, q.g, q.b, q.a] for q in geom.colors], float) / 255.0)
    for k, layer in enumerate(list(geom.uv_layers or [])[:L['vflags'] & 0x3F]):
        if len(layer) == nv:
            put(L['uvs'] + 8 * nv * k, np.array([[q.u, q.v] for q in layer], float))
    return bytes(out)


def write_nif_model(original: bytes, model) -> Tuple[bytes, List[str]]: #vers 1
    """Write edited NIF shape geometry back into the original file; (bytes, report)."""
    if original[:20] != _MAGIC:
        raise ValueError("source is not a Gamebryo NIF")
    E = _endian(original)
    types, offs, sizes, _strs = _blocks(original)
    nb = len(sizes)
    start, _end = _string_table(original)
    sizes_pos = start - 4 * nb
    blocks = [original[o:o + n] for o, n in zip(offs, sizes)]
    base = {g._nif[0]: g for g in (parse_nif_model(original) or DFFModel()).geometries}
    report = []
    for gi, geom in enumerate(model.geometries):
        info = getattr(geom, '_nif', None)
        if info is None:
            report.append(f"not written: geometry {gi} has no NIF shape (added after load)")
            continue
        ref, strips, mw = info
        if len(geom.vertices) > 0xFFFF:
            report.append(f"not written: geometry {gi} has over 65535 vertices")
            continue
        inv = np.linalg.inv(mw[:3, :3])
        old = base.get(ref)
        same_tris = old is not None and len(old.vertices) == len(geom.vertices) and \
            [(t.v1, t.v2, t.v3) for t in old.triangles] == [(t.v1, t.v2, t.v3) for t in geom.triangles]
        if same_tris and _same_arrays(old, geom):
            continue                                  # untouched: original bytes kept
        if same_tris:
            blocks[ref] = _patch_block(original, offs[ref], sizes[ref], strips, geom, inv, mw[3, :3], E)
            report.append(f"geometry {gi}: vertices updated")
        else:
            blocks[ref] = _data_block(original, offs[ref], strips, geom, inv, mw[3, :3], E)
            report.append(f"geometry {gi}: mesh rebuilt ({len(geom.vertices)} vertices, {len(geom.triangles)} triangles)")
    if not any(r.startswith('geometry') for r in report):
        return original, report
    out = bytearray(original[:offs[0]] if offs else original)
    struct.pack_into(E + f'{nb}I', out, sizes_pos, *[len(b) for b in blocks])
    tail = original[offs[-1] + sizes[-1]:] if offs else b''
    data = bytes(out) + b''.join(blocks) + tail
    back = parse_nif_model(data)
    if back is None or [len(g.triangles) for g in back.geometries] != \
            [len(g.triangles) for g in model.geometries if getattr(g, '_nif', None) is not None]:
        raise ValueError("written NIF did not read back the same")
    return data, report
