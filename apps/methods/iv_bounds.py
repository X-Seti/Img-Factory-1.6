#this belongs in apps/methods/iv_bounds.py - Version: 1
# X-Seti - October 07 2026 - IMG Factory 1.6 - GTA IV collision bounds

"""
GTA IV .wbn (static world bounds) and .wbd (bound dictionaries) to
collision triangles for Map Workshop: [(x, y, z)], [(a, b, c, r, g, b)].
"""

##Methods list -
# _aabb_box
# _bound_mesh
# _geometry_mesh
# _material_colour
# _ptr
# _unpack
# is_iv_bounds
# parse_wbd
# parse_wbn
# wbd_hashes

import colorsys
import struct
import zlib
from typing import List, Optional, Tuple

import numpy as np

_RSC5 = b'RSC\x05'
_BOUNDS_TYPE = 32
# phBound types
_CAPSULE, _BOX, _GEOMETRY, _BVH, _COMPOSITE = 1, 3, 4, 10, 12
_POLY_SIZE = 32
_BOX_TRIS = [(0, 1, 3), (0, 3, 2), (4, 6, 7), (4, 7, 5), (0, 4, 5), (0, 5, 1),
             (2, 3, 7), (2, 7, 6), (0, 2, 6), (0, 6, 4), (1, 5, 7), (1, 7, 3)]

Mesh = Tuple[List[Tuple[float, float, float]], List[tuple]]


def is_iv_bounds(data: bytes) -> bool: #vers 1
    """True for an RSC5 bounds resource (.wbn / .wbd)."""
    return len(data) > 12 and data[:4] == _RSC5 and \
        struct.unpack_from('<I', data, 4)[0] == _BOUNDS_TYPE


def _unpack(data: bytes) -> bytes: #vers 1
    """Decompressed bounds resource."""
    if not is_iv_bounds(data):
        raise ValueError("Not a GTA IV bounds resource")
    return zlib.decompress(data[12:])


def _ptr(z: bytes, at: int) -> Optional[int]: #vers 1
    """Virtual pointer stored at offset, or None."""
    p = struct.unpack_from('<I', z, at)[0]
    return p & 0xFFFFFFF if p >> 28 == 5 else None


def _material_colour(index: int) -> Tuple[float, float, float]: #vers 1
    """Stable distinct colour per material index."""
    r, g, b = colorsys.hsv_to_rgb((index * 0.618034) % 1.0, 0.45, 0.85)
    return r, g, b


def _geometry_mesh(z: bytes, b: int, mat: np.ndarray, verts: list, tris: list): #vers 1
    """phBoundGeometry / BVH: quantized vertices, tri or quad polygons."""
    nv, npoly = struct.unpack_from('<II', z, b + 0xC8)
    vp, pp = _ptr(z, b + 0xB0), _ptr(z, b + 0x8C)
    if not nv or not npoly or vp is None or pp is None:
        return
    quantum = np.array(struct.unpack_from('<3f', z, b + 0x90))
    offset = np.array(struct.unpack_from('<3f', z, b + 0xA0))
    v = np.frombuffer(z, '<i2', nv * 3, vp).reshape(-1, 3) * quantum + offset
    v = v @ mat[:3, :3] + mat[3, :3]
    base = len(verts)
    verts.extend(tuple(map(float, p)) for p in v)
    polys = np.frombuffer(z, np.uint8, npoly * _POLY_SIZE, pp).reshape(npoly, _POLY_SIZE)
    area_raw = polys[:, 12:16].copy().view('<u4')[:, 0]
    areas = polys[:, 12:16].copy().view('<f4')[:, 0]
    idx = polys[:, 16:24].copy().view('<u2')

    ok = (idx[:, :3].max(1) < nv)
    idx, areas, mats = idx[ok].astype(np.int64), areas[ok], (area_raw[ok] & 0xFF)
    a, b_, c, d = idx[:, 0], idx[:, 1], idx[:, 2], idx[:, 3]
    tri = np.linalg.norm(np.cross(v[b_] - v[a], v[c] - v[a]), axis=1) / 2
    has_d = d < nv
    dd = np.where(has_d, d, a)
    quad = tri + np.linalg.norm(np.cross(v[c] - v[a], v[dd] - v[a]), axis=1) / 2
    is_quad = has_d & (np.abs(quad - areas) < np.abs(tri - areas))   # stored area picks quad
    cols = {m: _material_colour(int(m)) for m in np.unique(mats)}   # material in area low byte
    for i in range(len(idx)):
        col = cols[mats[i]]
        tris.append((base + int(a[i]), base + int(b_[i]), base + int(c[i])) + col)
        if is_quad[i]:
            tris.append((base + int(a[i]), base + int(c[i]), base + int(d[i])) + col)


def _aabb_box(z: bytes, b: int, mat: np.ndarray, verts: list, tris: list): #vers 1
    """Box / capsule bounds drawn as their bounding box."""
    hi = np.array(struct.unpack_from('<3f', z, b + 0x10))
    lo = np.array(struct.unpack_from('<3f', z, b + 0x20))
    corners = np.array([[x, y, zz] for x in (lo[0], hi[0]) for y in (lo[1], hi[1])
                        for zz in (lo[2], hi[2])])
    corners = corners @ mat[:3, :3] + mat[3, :3]
    base = len(verts)
    verts.extend(tuple(map(float, p)) for p in corners)
    col = _material_colour(0)
    tris.extend((base + a, base + b_, base + c) + col for a, b_, c in _BOX_TRIS)


def _bound_mesh(z: bytes, b: int, mat: np.ndarray, verts: list, tris: list): #vers 1
    """Append one bound (composite children recursed) to verts/tris."""
    kind = z[b + 4]
    if kind in (_GEOMETRY, _BVH):
        _geometry_mesh(z, b, mat, verts, tris)
    elif kind in (_BOX, _CAPSULE):
        _aabb_box(z, b, mat, verts, tris)
    elif kind == _COMPOSITE:
        children, matrices = _ptr(z, b + 0x80), _ptr(z, b + 0x84)
        count = struct.unpack_from('<H', z, b + 0x90)[0]
        for i in range(count if children is not None else 0):
            child = _ptr(z, children + 4 * i)
            if child is None:
                continue
            m = np.identity(4)
            if matrices is not None:
                m = np.array(struct.unpack_from('<16f', z, matrices + 64 * i)).reshape(4, 4)
                m[:, 3] = (0, 0, 0, 1)                   # w lanes hold padding
            _bound_mesh(z, child, m @ mat, verts, tris)


def parse_wbn(data: bytes) -> Mesh: #vers 1
    """Static world bounds (.wbn): world-space collision mesh."""
    z = _unpack(data)
    root = _ptr(z, 0x08)
    verts, tris = [], []
    if root is not None:
        _bound_mesh(z, root, np.identity(4), verts, tris)
    return verts, tris


def wbd_hashes(data: bytes) -> List[int]: #vers 1
    """Model name hashes held in a .wbd bound dictionary."""
    z = _unpack(data)
    hp, count = _ptr(z, 0x10), struct.unpack_from('<H', z, 0x14)[0]
    return list(struct.unpack_from(f'<{count}I', z, hp)) if hp is not None else []


def parse_wbd(data: bytes, name_hash: int) -> Optional[Mesh]: #vers 1
    """One model's collision from a .wbd by name hash, or None."""
    hashes = wbd_hashes(data)
    if name_hash not in hashes:
        return None
    z = _unpack(data)
    bound = _ptr(z, _ptr(z, 0x18) + 4 * hashes.index(name_hash))
    verts, tris = [], []
    if bound is not None:
        _bound_mesh(z, bound, np.identity(4), verts, tris)
    return (verts, tris) if tris else None
