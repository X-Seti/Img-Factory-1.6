#this belongs in apps/methods/iv_anim.py - Version: 1
# X-Seti - October 23 2026 - IMG Factory 1.6 - GTA IV ped skeleton and WAD anims

"""
GTA IV ped animation: the skeleton of a ped .wft as a dff_skin Skeleton
(frames carry the IV bone ids), skins for the .wdd component meshes, and
.wad anim dictionaries (RSC5 crAnimation: quantized / static / raw float
channels) read into IFPAnim objects so Ped Workshop plays them like IFPs.
"""

##Methods list -
# _channel
# _quat_row
# _track_values
# _unpack_wad
# iv_skeleton
# iv_skins
# parse_wad
# wad_anim_list

import struct
from typing import List

import numpy as np

from apps.methods.dff_skin import Skeleton
from apps.methods.ifp_format import IFPAnim, IFPBone, IFPFile, IFPKey
from apps.methods.wdr_model import _ptr, _unpack

_BONE_SIZE = 0xE0
FPS = 30.0
_ROT, _TRANS = 1, 0                    # bone tracks; 5 / 6 mover, 0x80+ events


def _quat_row(q) -> np.ndarray: #vers 1
    """3x3 row-vector rotation of quat (x, y, z, w)."""
    from apps.methods.dff_skin import quat_to_matrix
    return quat_to_matrix(q)


def iv_skeleton(wft: bytes) -> Skeleton: #vers 1
    """Ped skeleton of a .wft (frames: name, local row 4x4, parent, node_id = bone id)
    plus a last 'model' frame (identity) the component meshes hang from."""
    z, vs = _unpack(wft)
    drawable = _ptr(z, vs, 0xB4)
    skel = _ptr(z, vs, drawable + 0x0C) if drawable is not None else None
    bones = _ptr(z, vs, skel) if skel is not None else None
    if bones is None:
        raise ValueError("No skeleton in this .wft")
    count = struct.unpack_from('<H', z, skel + 0x14)[0]
    sk = Skeleton()
    for i in range(count):
        b = bones + i * _BONE_SIZE
        na = _ptr(z, vs, b)
        name = z[na:z.index(b'\0', na)].decode('latin-1') if na is not None else f"bone{i}"
        bone_id = struct.unpack_from('<H', z, b + 0x16)[0]
        parent = _ptr(z, vs, b + 0x10)
        local = np.eye(4)
        local[:3, :3] = _quat_row(struct.unpack_from('<4f', z, b + 0x40))
        local[3, :3] = struct.unpack_from('<3f', z, b + 0x20)
        sk.frames.append({'name': name, 'local': local, 'node_id': bone_id,
                          'parent': (parent - bones) // _BONE_SIZE if parent is not None else -1})
        sk.hierarchy.append((bone_id, i, 0))
    sk.frames.append({'name': 'model', 'local': np.eye(4), 'node_id': -1, 'parent': -1})
    return sk


def iv_skins(sk: Skeleton, geometries) -> None: #vers 1
    """Skins (bone index / weight / inverse bind) for component meshes with blend data."""
    bind = sk.bind_ltms()
    nb = len(sk.hierarchy)
    inv = np.array([np.linalg.inv(bind[i]) for i in range(nb)])
    model_frame = len(sk.frames) - 1
    for gi, g in enumerate(geometries):
        sk.atomics[gi] = model_frame
        blend = getattr(g, '_iv_blend', None)
        if blend is not None and len(blend[0]) == len(g.vertices):
            idx, w = blend
            sk.skins[gi] = {'num_bones': nb, 'indices': np.minimum(idx, nb - 1).copy(),
                            'weights': w.copy(), 'inv': inv}


def _channel(z: bytes, vs: int, ch: int, frames: int) -> np.ndarray: #vers 1
    """Float per frame of one channel (static, quantized or raw)."""
    kind = z[ch + 5]
    if kind == 4:                                         # static float
        return np.full(frames, struct.unpack_from('<f', z, ch + 8)[0])
    if kind == 1:                                         # raw floats
        data, n = _ptr(z, vs, ch + 8), struct.unpack_from('<H', z, ch + 12)[0]
        v = np.frombuffer(z, '<f4', n, data).astype(np.float64)
        return np.resize(v, frames) if n else np.zeros(frames)
    if kind == 6:                                         # quantized bit stream
        data = _ptr(z, vs, ch + 8)
        bits, n = struct.unpack_from('<II', z, ch + 12)
        scale, offset = struct.unpack_from('<ff', z, ch + 20)
        if bits == 0 or n == 0:
            return np.full(frames, offset)
        words = (bits * n + 31) // 32
        stream = np.unpackbits(np.frombuffer(z, np.uint8, words * 4, data), bitorder='little')
        q = stream[:bits * n].reshape(n, bits).astype(np.int64) @ (1 << np.arange(bits, dtype=np.int64))
        return np.resize(offset + q * scale, frames)
    raise ValueError(f"WAD channel type {kind} not supported")


def _track_values(z: bytes, vs: int, td: int, fmt: int, frames: int) -> np.ndarray: #vers 1
    """(frames, 3) translation or (frames, 4) quat of one sequence track."""
    n = struct.unpack_from('<I', z, td + 0x14)[0]
    width = 4 if fmt & 0x0F == 1 else 3
    if fmt >> 4 == 0:                                     # one channel: static quat / vec3
        ch = _ptr(z, vs, td + 4)
        v = struct.unpack_from(f'<{width}f', z, _ptr(z, vs, ch + 8))
        return np.tile(np.array(v, np.float64), (frames, 1))
    cols = [_channel(z, vs, _ptr(z, vs, td + 4 + 4 * k), frames) for k in range(min(n, width))]
    return np.stack(cols, 1)


def _unpack_wad(data: bytes): #vers 1
    """Decompressed RSC5 anim dictionary and virtual segment size."""
    import zlib
    from apps.methods.xtd_textures import _rsc5_sizes
    if data[:4] != b'RSC\x05':
        raise ValueError("Not a GTA IV .wad (RSC5) file")
    vs, _ps = _rsc5_sizes(struct.unpack_from('<I', data, 8)[0])
    return zlib.decompress(data[12:]), vs


def parse_wad(data: bytes, name: str = 'wad') -> IFPFile: #vers 1
    """GTA IV .wad as an IFPFile (version 'WAD', keys every frame, bone_id = IV bone id)."""
    z, vs = _unpack_wad(data)
    count = struct.unpack_from('<H', z, 0x14)[0]
    arr = _ptr(z, vs, 0x18)
    out = IFPFile('WAD', name)
    for i in range(count):
        a = _ptr(z, vs, arr + 4 * i)
        frames, chunk = struct.unpack_from('<HH', z, a + 8)
        na = _ptr(z, vs, a + 0x1C)
        aname = z[na:z.index(b'\0', na)].decode('latin-1').split(':/', 1)[-1] if na else f"anim{i}"
        aname = aname[:-5] if aname.lower().endswith('.anim') else aname
        seqs = [_ptr(z, vs, _ptr(z, vs, a + 0x14) + 4 * s) for s in range(struct.unpack_from('<H', z, a + 0x18)[0])]
        tracks = {}                                       # (track, bone) -> [per-sequence arrays]
        for s, sq in enumerate(seqs):
            sframes = struct.unpack_from('<I', z, sq + 0x0C)[0]
            dp, n = _ptr(z, vs, sq), struct.unpack_from('<H', z, sq + 4)[0]
            for k in range(n):
                td = _ptr(z, vs, dp + 4 * k)
                tid = struct.unpack_from('<I', z, td)[0]
                track, fmt, bone = tid & 0xFF, (tid >> 8) & 0xFF, tid >> 16
                if track not in (_ROT, _TRANS):
                    continue
                v = _track_values(z, vs, td, fmt, sframes)
                tracks.setdefault((track, bone), []).append(v if s == 0 else v[1:])   # 1 frame overlap
        anim = IFPAnim(aname)
        for bone in sorted({b for _t, b in tracks}):
            rot = tracks.get((_ROT, bone))
            tr = tracks.get((_TRANS, bone))
            rot = np.concatenate(rot)[:frames] if rot else None
            tr = np.concatenate(tr)[:frames] if tr else None
            nk = len(rot) if rot is not None else len(tr)
            b = IFPBone(f"bone{bone}", bone, 'KRT0' if tr is not None else 'KR00', True)
            for f in range(nk):                         # rot None: bind rotation kept
                b.keys.append(IFPKey(f / FPS, tuple(rot[f]) if rot is not None else None,
                                     tuple(tr[f]) if tr is not None else None))
            anim.bones.append(b)
        out.anims.append(anim)
    return out


def wad_anim_list(data: bytes) -> List[tuple]: #vers 1
    """(anim name, duration s) of every anim in a .wad, header only (fast)."""
    z, vs = _unpack_wad(data)
    count = struct.unpack_from('<H', z, 0x14)[0]
    arr = _ptr(z, vs, 0x18)
    out = []
    for i in range(count):
        a = _ptr(z, vs, arr + 4 * i)
        na = _ptr(z, vs, a + 0x1C)
        n = z[na:z.index(b'\0', na)].decode('latin-1').split(':/', 1)[-1] if na else f"anim{i}"
        out.append((n[:-5] if n.lower().endswith('.anim') else n, struct.unpack_from('<f', z, a + 0x0C)[0]))
    return out
