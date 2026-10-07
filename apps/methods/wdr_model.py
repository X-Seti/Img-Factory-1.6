#this belongs in apps/methods/wdr_model.py - Version: 2
# X-Seti - October 07 2026 - IMG Factory 1.6 - GTA IV drawables

"""
GTA IV .wdr / .wdd / .wft (RSC5 drawables) to DFFModel geometry for Map Workshop.
"""

##Methods list -
# _bone_matrices
# _collection
# _cstr
# _decode_vertices
# _drawable_model
# _ptr
# _shader_textures
# _unpack
# is_iv_drawable
# parse_wdd
# parse_wdr
# parse_wft
# wdd_hashes
# wdr_embedded_textures

import struct
import zlib
from typing import Dict, List, Optional, Tuple

import numpy as np

from apps.methods.dff_classes import (BoundingSphere, DFFModel, Geometry, Material,
                                      TexCoord, Triangle, Vector3)

_RSC5 = b'RSC\x05'
_DRAWABLE_TYPE, _FRAGMENT_TYPE = 110, 112
_FRAG_CHILD_VTABLE = 0x6A35F4
_BONE_SIZE = 0xE0
# vertex element bits
_POS, _NORMAL, _UV0 = 0, 3, 6
# decoder type -> byte size
_TYPE_SIZE = {0: 2, 1: 4, 2: 6, 3: 8, 4: 4, 5: 8, 6: 12, 7: 16, 8: 4, 9: 4, 10: 4}
_DIFFUSE_HASH = 0x2B5170FD
_BUMP_HASH, _SPEC_HASH = 0x46B7C64F, 0x608799C6
# RW geometry flags: positions, textured, normals, light
_GEOM_FLAGS = 0x02 | 0x04 | 0x10 | 0x20


def is_iv_drawable(data: bytes) -> bool: #vers 2
    """True for an RSC5 drawable or fragment (.wdr / .wdd / .wft)."""
    return len(data) > 12 and data[:4] == _RSC5 and \
        struct.unpack_from('<I', data, 4)[0] in (_DRAWABLE_TYPE, _FRAGMENT_TYPE)


def _unpack(data: bytes) -> Tuple[bytes, int]: #vers 2
    """Decompressed resource and virtual segment size."""
    from apps.methods.xtd_textures import _rsc5_sizes
    if not is_iv_drawable(data):
        raise ValueError("Not a GTA IV drawable or fragment")
    vs, _ps = _rsc5_sizes(struct.unpack_from('<I', data, 8)[0])
    return zlib.decompress(data[12:]), vs


def _ptr(z: bytes, vs: int, at: int) -> Optional[int]: #vers 1
    """Resolve pointer stored at offset: 0x5 virtual, 0x6 physical."""
    p = struct.unpack_from('<I', z, at)[0]
    if p >> 28 == 5:
        return p & 0xFFFFFFF
    if p >> 28 == 6:
        return vs + (p & 0xFFFFFFF)
    return None


def _cstr(z: bytes, at: int) -> str: #vers 1
    """Null-terminated string, pack:/ and .dds stripped."""
    s = z[at:z.index(b'\0', at)].decode('latin-1').split(':/', 1)[-1]
    return s[:-4] if s.lower().endswith('.dds') else s


def _collection(z: bytes, vs: int, at: int) -> List[int]: #vers 1
    """Offsets of a pgPtrCollection (ptr array, u16 count)."""
    arr = _ptr(z, vs, at)
    if arr is None:
        return []
    count = struct.unpack_from('<H', z, at + 4)[0]
    out = []
    for i in range(count):
        o = _ptr(z, vs, arr + 4 * i)
        if o is not None:
            out.append(o)
    return out


def _shader_textures(z: bytes, vs: int, drawable: int) -> List[str]: #vers 1
    """Diffuse texture name per shader ('' when none)."""
    sg = _ptr(z, vs, drawable + 0x08)
    if sg is None:
        return []
    names = []
    for s in _collection(z, vs, sg + 0x08):
        params, types, hashes = (_ptr(z, vs, s + 0x14), _ptr(z, vs, s + 0x24),
                                 _ptr(z, vs, s + 0x34))
        count = struct.unpack_from('<I', z, s + 0x1C)[0]
        found = {}
        if None not in (params, types, hashes):
            for j in range(count):
                if z[types + j] != 0:
                    continue
                ref = _ptr(z, vs, params + 4 * j)
                name_at = _ptr(z, vs, ref + 0x14) if ref is not None else None
                if name_at is not None:
                    found[struct.unpack_from('<I', z, hashes + 4 * j)[0]] = _cstr(z, name_at)
        name = found.get(_DIFFUSE_HASH) or next(
            (n for h, n in found.items() if h not in (_BUMP_HASH, _SPEC_HASH)), '')
        names.append(name)
    return names


def _decode_vertices(z: bytes, vs: int, vb: int): #vers 1
    """(positions, normals, uvs) arrays from one vertex buffer."""
    count = struct.unpack_from('<H', z, vb + 0x04)[0]
    stride = struct.unpack_from('<I', z, vb + 0x0C)[0]
    decl = _ptr(z, vs, vb + 0x10)
    data = _ptr(z, vs, vb + 0x18)
    if data is None:
        data = _ptr(z, vs, vb + 0x08)
    if decl is None or data is None:
        raise ValueError("Vertex buffer has no declaration or data")
    used, _st, _a, _b, types = struct.unpack_from('<IHBBQ', z, decl)
    raw = np.frombuffer(z, np.uint8, count * stride, data).reshape(count, stride)
    cols, off = {}, 0
    for bit in range(16):
        if used & (1 << bit):
            t = (types >> (4 * bit)) & 0xF
            cols[bit] = (off, t)
            off += _TYPE_SIZE[t]
    if off != stride:
        raise ValueError(f"Vertex layout {off} bytes, stride {stride}")

    def _floats(bit, n):
        o, t = cols[bit]
        if t in (4, 5, 6, 7):
            return raw[:, o:o + 4 * n].copy().view('<f4')
        if t in (0, 1, 2, 3):
            return raw[:, o:o + 2 * n].copy().view('<f2').astype(np.float32)
        if t == 10:      # Dec3N packed normal
            v = raw[:, o:o + 4].copy().view('<u4')[:, 0]
            parts = [((v >> s) & 0x3FF).astype(np.int32) for s in (0, 10, 20)]
            return np.stack([np.where(p > 511, p - 1024, p) / 511.0 for p in parts], 1)
        raise ValueError(f"Vertex element type {t} not supported")

    if _POS not in cols:
        raise ValueError("Vertex buffer has no positions")
    pos = _floats(_POS, 3)[:, :3]
    nrm = _floats(_NORMAL, 3)[:, :3] if _NORMAL in cols else None
    uv = _floats(_UV0, 2)[:, :2] if _UV0 in cols else None
    return pos, nrm, uv


def _bone_matrices(z: bytes, vs: int, drawable: int) -> List[np.ndarray]: #vers 1
    """World 4x4 matrix per skeleton bone (parent chain, offset + quaternion)."""
    sk = _ptr(z, vs, drawable + 0x0C)
    bones = _ptr(z, vs, sk) if sk is not None else None
    if bones is None:
        return []
    count = struct.unpack_from('<H', z, sk + 0x14)[0]
    out = []
    for i in range(count):
        b = bones + i * _BONE_SIZE
        tx, ty, tz = struct.unpack_from('<3f', z, b + 0x20)
        x, y, zq, w = struct.unpack_from('<4f', z, b + 0x40)
        local = np.identity(4)
        local[:3, :3] = [[1 - 2 * (y * y + zq * zq), 2 * (x * y - zq * w), 2 * (x * zq + y * w)],
                         [2 * (x * y + zq * w), 1 - 2 * (x * x + zq * zq), 2 * (y * zq - x * w)],
                         [2 * (x * zq - y * w), 2 * (y * zq + x * w), 1 - 2 * (x * x + y * y)]]
        local[:3, 3] = (tx, ty, tz)
        parent = _ptr(z, vs, b + 0x10)
        pi = (parent - bones) // _BONE_SIZE if parent is not None else -1
        out.append(out[pi] @ local if 0 <= pi < i else local)
    return out


def _drawable_model(z: bytes, vs: int, drawable: int, name: str,
                    textures: Optional[List[str]] = None,
                    bones: Optional[List[np.ndarray]] = None,
                    out: Optional[DFFModel] = None) -> DFFModel: #vers 3
    """Highest LOD of one drawable as a DFFModel, bone transforms applied."""
    if textures is None:
        textures = _shader_textures(z, vs, drawable)
    if bones is None:
        bones = _bone_matrices(z, vs, drawable)
    models = []
    for lod in range(4):            # highest LOD present
        coll = _ptr(z, vs, drawable + 0x40 + 4 * lod)
        models = _collection(z, vs, coll) if coll is not None else []
        if models:
            break
    if not models:
        raise ValueError(f"{name}: drawable has no models")
    if out is None:
        out = DFFModel(source_path=name)
    for m in models:
        geoms = _collection(z, vs, m + 0x04)
        smap = _ptr(z, vs, m + 0x10)
        skinned, bone = z[m + 0x15], z[m + 0x17]
        mat = bones[bone] if (not skinned and bone < len(bones)) else None
        for gi, g in enumerate(geoms):
            vb, ib = _ptr(z, vs, g + 0x0C), _ptr(z, vs, g + 0x1C)
            if vb is None or ib is None:
                continue
            pos, nrm, uv = _decode_vertices(z, vs, vb)
            if mat is not None:          # bone space to model space
                pos = pos @ mat[:3, :3].T + mat[:3, 3]
                if nrm is not None:
                    nrm = nrm @ mat[:3, :3].T
            icount = struct.unpack_from('<I', z, ib + 0x04)[0]
            idata = _ptr(z, vs, ib + 0x08)
            idx = np.frombuffer(z, '<u2', icount - icount % 3, idata).reshape(-1, 3)
            shader = struct.unpack_from('<H', z, smap + 2 * gi)[0] if smap is not None else 0
            geom = Geometry(flags=_GEOM_FLAGS)
            geom.vertices = [Vector3(*map(float, p)) for p in pos]
            if nrm is not None:
                geom.normals = [Vector3(*map(float, n)) for n in nrm]
            if uv is not None:
                geom.uv_layers = [[TexCoord(float(a), float(b)) for a, b in uv]]
            geom.uv_layer_count = 1 if uv is not None else 0
            geom.triangles = [Triangle(int(a), int(b), int(c), 0) for a, b, c in idx]
            tex = textures[shader] if shader < len(textures) else ''
            mat_ = Material(texture_name=tex)
            mat_.colour = mat_.color          # viewport reads .colour
            geom.materials = [mat_]
            lo, hi = pos.min(0), pos.max(0)
            geom.bounding_sphere = BoundingSphere(Vector3(*map(float, (lo + hi) / 2)),
                                                  float(np.linalg.norm(hi - lo) / 2))
            out.geometries.append(geom)
    return out


def parse_wdr(data: bytes, name: str = '') -> DFFModel: #vers 2
    """GTA IV .wdr to DFFModel (highest LOD, diffuse textures)."""
    if struct.unpack_from('<I', data, 4)[0] == _FRAGMENT_TYPE:
        return parse_wft(data, name)
    z, vs = _unpack(data)
    return _drawable_model(z, vs, 0, name)


def parse_wft(data: bytes, name: str = '') -> DFFModel: #vers 1
    """GTA IV .wft fragment: every child's undamaged drawable merged."""
    z, vs = _unpack(data)
    main = _ptr(z, vs, 0xB4)
    if main is None:
        raise ValueError(f"{name}: fragment has no drawable")
    textures, bones = _shader_textures(z, vs, main), _bone_matrices(z, vs, main)
    out = DFFModel(source_path=name)
    children = _ptr(z, vs, 0xD4)
    for i in range(z[0x1F3] if children is not None else 0):   # child count
        child = _ptr(z, vs, children + 4 * i)
        if child is None or struct.unpack_from('<I', z, child)[0] != _FRAG_CHILD_VTABLE:
            raise ValueError(f"{name}: fragment child {i} unreadable")
        drawable = _ptr(z, vs, child + 0x90)
        if drawable is not None:
            try:
                _drawable_model(z, vs, drawable, name, textures, bones, out)
            except ValueError:
                pass                     # child without geometry
    if not out.geometries:
        raise ValueError(f"{name}: fragment has no geometry")
    return out


def wdd_hashes(data: bytes) -> List[int]: #vers 1
    """Model name hashes held in a .wdd drawable dictionary."""
    z, vs = _unpack(data)
    hp, hc = _ptr(z, vs, 0x10), struct.unpack_from('<H', z, 0x14)[0]
    return list(struct.unpack_from(f'<{hc}I', z, hp)) if hp is not None else []


def parse_wdd(data: bytes, name_hash: int, name: str = '') -> Optional[DFFModel]: #vers 1
    """One drawable from a .wdd by model name hash, or None."""
    z, vs = _unpack(data)
    hashes = wdd_hashes(data)
    if name_hash not in hashes:
        return None
    drawables = _collection(z, vs, 0x18)
    return _drawable_model(z, vs, drawables[hashes.index(name_hash)], name)


def wdr_embedded_textures(data: bytes) -> List[dict]: #vers 2
    """Textures embedded in a .wdr / .wft shader group (top level only)."""
    from apps.methods.xtd_textures import _iv_textures
    z, vs = _unpack(data)
    frag = struct.unpack_from('<I', data, 4)[0] == _FRAGMENT_TYPE
    drawable = _ptr(z, vs, 0xB4) if frag else 0
    sg = _ptr(z, vs, drawable + 0x08) if drawable is not None else None
    td = _ptr(z, vs, sg + 0x04) if sg is not None else None
    return _iv_textures(z, vs, td, levels=False) if td is not None else []
