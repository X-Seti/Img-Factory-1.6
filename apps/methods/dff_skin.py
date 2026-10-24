#this belongs in apps/methods/dff_skin.py - Version: 2
# X-Seti - October 21 2026 - IMG Factory 1.6 - DFF skin and bone hierarchy

"""
Reads skinned-model data from DFF bytes: frames with HAnim node ids, the
bone hierarchy, per-geometry skin weights / bone indices / inverse bind
matrices, and poses the mesh (CPU skinning, row-vector RW matrices).
"""

##Methods list -
# _chunks
# _frame_matrix
# quat_to_matrix
# read_skeleton
# _read_skin
# write_skin_weights
#
##class Skeleton: -
# __init__
# anim_locals
# bind_ltms
# bone_of_frame
# pose_ltms
# skin_vertices

import struct

import numpy as np

_STRUCT, _EXT, _FRAMELIST, _GEOMETRY, _CLUMP, _GEOMLIST, _ATOMIC = 1, 3, 0x0E, 0x0F, 0x10, 0x1A, 0x14
_SKIN, _HANIM, _FRAMENAME = 0x116, 0x11E, 0x253F2FE


def _chunks(d: bytes, start: int, end: int): #vers 1
    """(type, payload start, payload end) of each chunk in a range."""
    o = start
    while o + 12 <= end:
        t, size = struct.unpack_from('<II', d, o)
        yield t, o + 12, min(o + 12 + size, end)
        o += 12 + size


def quat_to_matrix(q) -> np.ndarray: #vers 1
    """3x3 row-vector rotation matrix (v @ M) from an IFP quat (x, y, z, w)."""
    x, y, z, w = q
    return np.array([
        [1 - 2 * (y * y + z * z), 2 * (x * y + w * z), 2 * (x * z - w * y)],
        [2 * (x * y - w * z), 1 - 2 * (x * x + z * z), 2 * (y * z + w * x)],
        [2 * (x * z + w * y), 2 * (y * z - w * x), 1 - 2 * (x * x + y * y)]])


def _frame_matrix(rot9, pos) -> np.ndarray: #vers 1
    m = np.eye(4)
    m[:3, :3] = np.asarray(rot9, dtype=np.float64).reshape(3, 3)
    m[3, :3] = pos
    return m


class Skeleton: #vers 1
    """Frames, bone hierarchy and skins of one DFF."""

    def __init__(self): #vers 1
        self.frames = []          # dicts: name, local (4x4), parent, node_id
        self.hierarchy = []       # (node_id, index, flags) in bone index order
        self.skins = {}           # geometry index -> dict(indices, weights, inv, num_bones)
        self.atomics = {}         # geometry index -> frame index

    def bone_of_frame(self) -> dict: #vers 1
        """Frame index -> bone index (via HAnim node ids)."""
        by_id = {f['node_id']: i for i, f in enumerate(self.frames) if f['node_id'] >= 0}
        return {by_id[nid]: bi for bi, (nid, _x, _f) in enumerate(self.hierarchy) if nid in by_id}

    def pose_ltms(self, local_override=None) -> list: #vers 1
        """World (LTM) matrix per frame; local_override: {frame: 4x4 local}."""
        out = [None] * len(self.frames)
        for i, f in enumerate(self.frames):
            local = (local_override or {}).get(i, f['local'])
            p = f['parent']
            out[i] = local @ out[p] if 0 <= p < i else local
        return out

    def anim_locals(self, anim, t: float) -> dict: #vers 1
        """{frame: local 4x4} for an IFPAnim at time t (SA by bone id, III / VC by name)."""
        out = {}
        for i, f in enumerate(self.frames):
            b = None
            if f['node_id'] >= 0:
                b = next((x for x in anim.bones if x.bone_id == f['node_id']), None)
            if b is None and f['name']:
                b = anim.bone(f['name'].strip())
            if b is None or not b.keys:
                continue
            q, tr = b.sample(t)
            m = np.eye(4)
            m[:3, :3] = quat_to_matrix(q)
            m[3, :3] = tr if tr is not None else f['local'][3, :3]
            out[i] = m
        return out

    def bind_ltms(self) -> list: #vers 1
        return self.pose_ltms()

    def skin_vertices(self, gi: int, verts: np.ndarray, normals, ltms) -> tuple: #vers 1
        """Posed (vertices, normals) for geometry gi from frame LTMs."""
        sk = self.skins[gi]
        f2b = self.bone_of_frame()
        b2f = {b: f for f, b in f2b.items()}
        root_inv = np.linalg.inv(ltms[self.atomics.get(gi, 0)])   # mesh lives in the atomic's frame
        mats = np.zeros((sk['num_bones'], 4, 4))
        for b in range(sk['num_bones']):
            fi = b2f.get(b)
            ltm = ltms[fi] @ root_inv if fi is not None else np.eye(4)
            mats[b] = sk['inv'][b] @ ltm
        v4 = np.concatenate([verts, np.ones((len(verts), 1))], axis=1)
        out = np.zeros((len(verts), 3))
        nout = np.zeros((len(verts), 3)) if normals is not None and len(normals) else None
        for k in range(4):
            w = sk['weights'][:, k:k + 1]
            m = mats[sk['indices'][:, k]]
            out += w * np.einsum('ni,nij->nj', v4, m)[:, :3]
            if nout is not None:
                nout += w * np.einsum('ni,nij->nj', normals, m[:, :3, :3])
        if nout is not None:
            ln = np.linalg.norm(nout, axis=1, keepdims=True)
            nout = nout / np.where(ln > 0, ln, 1.0)
        return out, nout


def read_skeleton(raw: bytes) -> Skeleton: #vers 1
    """Skeleton from DFF bytes (empty skins when the model is not skinned)."""
    sk = Skeleton()
    clump = next(((s, e) for t, s, e in _chunks(raw, 0, len(raw)) if t == _CLUMP), None)
    if clump is None:
        raise ValueError("not a DFF clump")
    for t, s, e in _chunks(raw, *clump):
        if t == _FRAMELIST:
            parts = list(_chunks(raw, s, e))
            st = parts[0]
            n = struct.unpack_from('<I', raw, st[1])[0]
            for i in range(n):
                o = st[1] + 4 + 56 * i
                vals = struct.unpack_from('<12f2i', raw, o)
                sk.frames.append({'name': '', 'local': _frame_matrix(vals[:9], vals[9:12]),
                                  'parent': vals[12], 'node_id': -1})
            exts = [p for p in parts[1:] if p[0] == _EXT]
            for i, (_t, es, ee) in enumerate(exts[:n]):
                for ct, cs, ce in _chunks(raw, es, ee):
                    if ct == _FRAMENAME:
                        sk.frames[i]['name'] = raw[cs:ce].split(b'\0', 1)[0].decode('latin-1')
                    elif ct == _HANIM:
                        _ver, nid, nnodes = struct.unpack_from('<3i', raw, cs)
                        sk.frames[i]['node_id'] = nid
                        if nnodes:
                            o = cs + 20
                            sk.hierarchy = [struct.unpack_from('<3i', raw, o + 12 * k) for k in range(nnodes)]
        elif t == _ATOMIC:
            st = next((cs for ct, cs, _ce in _chunks(raw, s, e) if ct == _STRUCT), None)
            if st is not None:
                fi, gi = struct.unpack_from('<2i', raw, st)
                sk.atomics[gi] = fi
        elif t == _GEOMLIST:
            gi = 0
            for gt, gs, ge in _chunks(raw, s, e):
                if gt != _GEOMETRY:
                    continue
                nverts = 0
                for ct, cs, ce in _chunks(raw, gs, ge):
                    if ct == _STRUCT:
                        nverts = struct.unpack_from('<i', raw, cs + 8)[0]
                    elif ct == _EXT:
                        for xt, xs, xe in _chunks(raw, cs, ce):
                            if xt == _SKIN:
                                sk.skins[gi] = _read_skin(raw, xs, nverts)
                gi += 1
    return sk


def _read_skin(d: bytes, o: int, n: int) -> dict: #vers 2
    """Skin PLG: bone indices / weights (with file offsets), inverse binds."""
    nb, nused, _maxw, _pad = d[o:o + 4]
    o += 4 + nused
    i_off = o
    idx = np.frombuffer(d, np.uint8, n * 4, o).reshape(n, 4).astype(np.int64)
    o += n * 4
    w_off = o
    w = np.frombuffer(d, '<f4', n * 4, o).reshape(n, 4).astype(np.float64)
    o += n * 16
    inv = np.zeros((nb, 4, 4))
    for b in range(nb):
        if nused == 0:
            o += 4                                   # old format: 0xDEADDEAD marker
        inv[b] = np.frombuffer(d, '<f4', 16, o).reshape(4, 4)
        o += 64
    inv[:, :, 3] = (0, 0, 0, 1)
    return {'num_bones': nb, 'indices': np.minimum(idx, max(nb - 1, 0)), 'weights': w, 'inv': inv,
            'i_off': i_off, 'w_off': w_off}


def write_skin_weights(raw: bytes, skins: dict) -> bytes: #vers 1
    """DFF bytes with each skin's indices / weights patched in place (same size)."""
    out = bytearray(raw)
    for sk in skins.values():
        idx = np.asarray(sk['indices'], dtype=np.uint8)
        w = np.asarray(sk['weights'], dtype='<f4')
        out[sk['i_off']:sk['i_off'] + idx.size] = idx.tobytes()
        out[sk['w_off']:sk['w_off'] + w.size * 4] = w.tobytes()
    return bytes(out)
