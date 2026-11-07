#this belongs in apps/methods/iv_anim.py - Version: 3
# X-Seti - October 23 2026 - IMG Factory 1.6 - GTA IV ped skeleton and WAD anims

"""
GTA IV ped animation: the skeleton of a ped .wft as a dff_skin Skeleton
(frames carry the IV bone ids), skins for the .wdd component meshes, and
.wad anim dictionaries (RSC5 crAnimation: quantized / static / raw float
channels) read into IFPAnim objects so Ped Workshop plays them like IFPs;
cutscene mover and camera tracks (position, rotation, FOV) per anim, and
edited camera tracks written back in place (same layout, re-quantized).
"""

##Methods list -
# _channel
# _quat_row
# _track_values
# _unpack_wad
# iv_skeleton
# iv_skins
# parse_wad
# _put_channel
# cam_quat_view
# cam_view_quat
# wad_anim_list
# wad_extra_tracks
# write_wad_camera

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


EXTRA_TRACKS = {5: 'mover_pos', 6: 'mover_rot', 7: 'cam_pos', 8: 'cam_rot', 136: 'cam_fov'}


def wad_extra_tracks(data: bytes) -> dict: #vers 1
    """{anim name (lower): {'mover_pos' / 'mover_rot' / 'cam_pos' / 'cam_rot' / 'cam_fov': (frames, n)}}."""
    z, vs = _unpack_wad(data)
    count = struct.unpack_from('<H', z, 0x14)[0]
    arr = _ptr(z, vs, 0x18)
    out = {}
    for i in range(count):
        a = _ptr(z, vs, arr + 4 * i)
        frames = struct.unpack_from('<H', z, a + 8)[0]
        na = _ptr(z, vs, a + 0x1C)
        aname = z[na:z.index(b'\0', na)].decode('latin-1').split(':/', 1)[-1] if na else f"anim{i}"
        aname = (aname[:-5] if aname.lower().endswith('.anim') else aname).lower()
        seqs = [_ptr(z, vs, _ptr(z, vs, a + 0x14) + 4 * s) for s in range(struct.unpack_from('<H', z, a + 0x18)[0])]
        got = {}
        for s, sq in enumerate(seqs):
            sframes = struct.unpack_from('<I', z, sq + 0x0C)[0]
            dp, n = _ptr(z, vs, sq), struct.unpack_from('<H', z, sq + 4)[0]
            for k in range(n):
                td = _ptr(z, vs, dp + 4 * k)
                tid = struct.unpack_from('<I', z, td)[0]
                track, fmt = tid & 0xFF, (tid >> 8) & 0xFF
                if track not in EXTRA_TRACKS or tid >> 16:
                    continue
                if fmt & 0x0F == 2:                       # one float
                    ch = _ptr(z, vs, td + 4)
                    vp = _ptr(z, vs, ch + 8) if fmt >> 4 == 0 else None
                    v = (np.full((sframes, 1), struct.unpack_from('<f', z, vp)[0]) if vp is not None
                         else _channel(z, vs, ch, sframes)[:, None])
                else:
                    v = _track_values(z, vs, td, fmt, sframes)
                got.setdefault(EXTRA_TRACKS[track], []).append(v if s == 0 else v[1:])
        if got:
            out[aname] = {k: np.concatenate(v)[:frames] for k, v in got.items()}
    return out


def _put_channel(z: bytearray, vs: int, ch: int, values: np.ndarray) -> bool: #vers 1
    """Write one channel's values in place; False if it can't hold them (static, too short)."""
    kind = z[ch + 5]
    if kind == 4:                                         # static: only when the values don't change
        if np.ptp(values) > 1e-5:
            return False
        struct.pack_into('<f', z, ch + 8, float(values[0]))
        return True
    if kind == 1:
        data, n = _ptr(z, vs, ch + 8), struct.unpack_from('<H', z, ch + 12)[0]
        z[data:data + 4 * n] = np.resize(values, n).astype('<f4').tobytes()
        return True
    if kind == 6:
        data = _ptr(z, vs, ch + 8)
        bits, n = struct.unpack_from('<II', z, ch + 12)
        if bits == 0 or n == 0:
            if np.ptp(values) > 1e-5:
                return False
            struct.pack_into('<f', z, ch + 24, float(values[0]))
            return True
        v = np.resize(values, n).astype(np.float64)
        lo, hi = float(v.min()), float(v.max())
        top = (1 << bits) - 1
        scale = (hi - lo) / top if hi > lo else 0.0
        q = np.round((v - lo) / scale).astype(np.int64) if scale else np.zeros(n, np.int64)
        q = np.clip(q, 0, top)
        bitarr = ((q[:, None] >> np.arange(bits)) & 1).astype(np.uint8).reshape(-1)
        words = (bits * n + 31) // 32
        packed = np.packbits(np.concatenate([bitarr, np.zeros(words * 32 - len(bitarr), np.uint8)]),
                             bitorder='little')
        z[data:data + words * 4] = packed.tobytes()
        struct.pack_into('<ff', z, ch + 20, scale, lo)
        return True
    return False


def write_wad_camera(data: bytes, cam_name: str, pos=None, rot=None, fov=None) -> bytes: #vers 1
    """.wad with one camera anim's position / rotation / FOV (per frame) replaced in place."""
    import zlib
    z, vs = _unpack_wad(data)
    z = bytearray(z)
    count = struct.unpack_from('<H', z, 0x14)[0]
    arr = _ptr(z, vs, 0x18)
    want = {7: pos, 8: rot, 136: fov}
    done = set()
    for i in range(count):
        a = _ptr(z, vs, arr + 4 * i)
        na = _ptr(z, vs, a + 0x1C)
        aname = z[na:z.index(b'\0', na)].decode('latin-1').split(':/', 1)[-1] if na else ''
        aname = (aname[:-5] if aname.lower().endswith('.anim') else aname).lower()
        if aname != cam_name.lower():
            continue
        seqs = [_ptr(z, vs, _ptr(z, vs, a + 0x14) + 4 * s) for s in range(struct.unpack_from('<H', z, a + 0x18)[0])]
        start = 0
        for sq in seqs:
            sframes = struct.unpack_from('<I', z, sq + 0x0C)[0]
            dp, n = _ptr(z, vs, sq), struct.unpack_from('<H', z, sq + 4)[0]
            for k in range(n):
                td = _ptr(z, vs, dp + 4 * k)
                tid = struct.unpack_from('<I', z, td)[0]
                track, fmt = tid & 0xFF, (tid >> 8) & 0xFF
                vals = want.get(track)
                if vals is None or tid >> 16:
                    continue
                seg = np.asarray(vals, np.float64)[start:start + sframes]
                seg = np.concatenate([seg, np.repeat(seg[-1:], sframes - len(seg), 0)]) if len(seg) < sframes else seg
                seg = seg.reshape(len(seg), -1)
                if fmt >> 4 == 0:                         # one static value / vector
                    if np.ptp(seg, axis=0).max() > 1e-5:
                        raise ValueError(f"{cam_name}: track {track} is fixed in the file; can't animate it")
                    ch = _ptr(z, vs, td + 4)
                    vp = _ptr(z, vs, ch + 8)
                    if vp is not None:
                        z[vp:vp + 4 * seg.shape[1]] = seg[0].astype('<f4').tobytes()
                    else:
                        struct.pack_into('<f', z, ch + 8, float(seg[0][0]))
                else:
                    nch = struct.unpack_from('<I', z, td + 0x14)[0]
                    for c in range(min(nch, seg.shape[1])):
                        if not _put_channel(z, vs, _ptr(z, vs, td + 4 + 4 * c), seg[:, c]):
                            raise ValueError(f"{cam_name}: track {track} channel {c} can't hold the new values")
                done.add(track)
            start += sframes - 1                          # 1 frame overlap between sequences
    if not done:
        raise ValueError(f"{cam_name}: camera anim not found in the .wad")
    return bytes(data[:12]) + zlib.compress(bytes(z), 9)


def cam_quat_view(q) -> tuple: #vers 1
    """IV camera quat -> (forward vector, roll degrees); the camera looks along local +Y, up +Z."""
    import math
    from apps.methods.dff_skin import quat_to_matrix
    R = quat_to_matrix(tuple(q))
    right, fwd, up = R[0], R[1], R[2]
    return fwd, math.degrees(math.atan2(-right[2], up[2]))


def cam_view_quat(fwd, roll: float, near=None) -> np.ndarray: #vers 1
    """(forward vector, roll degrees) -> IV camera quat (x, y, z, w); sign kept close to near."""
    import math
    f = np.asarray(fwd, np.float64)
    f = f / (np.linalg.norm(f) or 1.0)
    r0 = np.cross(f, (0.0, 0.0, 1.0))
    r0 = r0 / (np.linalg.norm(r0) or 1.0)
    u0 = np.cross(r0, f)
    a = math.radians(-roll)
    right, up = r0 * math.cos(a) + u0 * math.sin(a), -r0 * math.sin(a) + u0 * math.cos(a)
    m = np.array([right, f, up]).T                         # column-vector rotation (rows were v @ M)
    tr = m[0, 0] + m[1, 1] + m[2, 2]
    if tr > 0:
        w = math.sqrt(1.0 + tr) / 2
        q = np.array([(m[2, 1] - m[1, 2]) / (4 * w), (m[0, 2] - m[2, 0]) / (4 * w), (m[1, 0] - m[0, 1]) / (4 * w), w])
    else:
        i = int(np.argmax([m[0, 0], m[1, 1], m[2, 2]]))
        j, k = (i + 1) % 3, (i + 2) % 3
        t = math.sqrt(1.0 + m[i, i] - m[j, j] - m[k, k]) * 2
        q = np.zeros(4)
        q[i], q[3] = t / 4, (m[k, j] - m[j, k]) / t
        q[j], q[k] = (m[j, i] + m[i, j]) / t, (m[k, i] + m[i, k]) / t
    if near is not None and np.dot(q, near) < 0:
        q = -q
    return q
