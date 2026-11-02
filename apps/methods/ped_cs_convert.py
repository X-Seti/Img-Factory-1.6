#this belongs in apps/methods/ped_cs_convert.py - Version: 1
# X-Seti - November 02 2026 - IMG Factory 1.6 - Cutscene ped to in-game ped

"""
SA cutscene ped DFF (cutscene.img cs*.dff, 60+ bones with face and finger
bones) to an in-game ped DFF on the standard 32-bone SA skeleton. Mesh and
materials are kept; extra bones' skin weights move to their nearest game
bone (brows to the brow bones, jaw and lower lips to Jaw, rest of the face
to Head, thumbs to the hand).
"""

##Methods list -
# _frame_list
# _skin_payload
# _target_of
# _world
# convert_cutscene_ped

import struct
from typing import Dict, List, Tuple

import numpy as np

from apps.methods import rw_chunks as rw

# SA ped skeleton (models/gta3.img peds): frame order (name, HAnim id, parent frame; frame 0 = clump)
SA_PED_FRAMES = [
    ('Root', 0, 0), (' Pelvis', 1, 1), (' R Thigh', 51, 2), (' L Thigh', 41, 2), (' Spine', 2, 2),
    ('Belly', 201, 5), (' Spine1', 3, 5), ('R breast', 301, 7), ('L breast', 302, 7), (' Neck', 4, 7),
    ('Bip01 R Clavicle', 21, 10), ('Bip01 L Clavicle', 31, 10), (' Head', 5, 10), ('R Brow', 7, 13),
    ('L Brow', 6, 13), ('Jaw', 8, 13), (' L UpperArm', 32, 12), (' L ForeArm', 33, 17), (' L Hand', 34, 18),
    (' L Finger', 35, 19), (' L Finger01', 36, 20), (' R UpperArm', 22, 11), (' R ForeArm', 23, 22),
    (' R Hand', 24, 23), (' R Finger', 25, 24), (' R Finger01', 26, 25), (' L Calf', 42, 4),
    (' L Foot', 43, 27), (' L Toe0', 44, 28), (' R Calf', 52, 3), (' R Foot', 53, 30), (' R Toe0', 54, 31)]
# HAnim hierarchy (bone index order): (id, flags 1 pop / 2 push)
SA_PED_HIER = [
    (0, 0), (1, 0), (2, 2), (3, 2), (4, 2), (5, 2), (8, 3), (6, 3), (7, 1), (31, 2), (32, 0), (33, 0),
    (34, 0), (35, 0), (36, 1), (21, 0), (22, 0), (23, 0), (24, 0), (25, 0), (26, 1), (302, 3), (301, 1),
    (201, 1), (41, 2), (42, 0), (43, 0), (44, 1), (51, 0), (52, 0), (53, 0), (54, 1)]
# game bones a cutscene ped lacks, taken from these cutscene bones
_FACE_SOURCE = {7: 'rbrow1', 6: 'lbrow1', 8: 'jaw1'}


def _frame_list(raw: bytes, fl) -> List[dict]: #vers 1
    """Frames of a Frame List: matrix, parent, flags, name, HAnim id, extension bytes."""
    st = fl.children[0]
    n = struct.unpack_from('<i', raw, st.data_start)[0]
    exts = [c for c in fl.children if c.type == 0x03]
    out = []
    for i in range(n):
        o = st.data_start + 4 + i * 56
        m = np.eye(4)
        v = struct.unpack_from('<12f', raw, o)
        m[:3, :3] = np.array(v[:9]).reshape(3, 3)
        m[3, :3] = v[9:]
        par, flags = struct.unpack_from('<iI', raw, o + 48)
        name, hid = '', None
        for c in (exts[i].children if i < len(exts) else []):
            if c.type == 0x0253F2FE:
                name = bytes(raw[c.data_start:c.end]).decode('latin-1')
            elif c.type == 0x11E:
                hid = struct.unpack_from('<i', raw, c.data_start + 4)[0]
        out.append({'m': m, 'parent': par, 'flags': flags, 'name': name, 'id': hid,
                    'ext': bytes(raw[exts[i].offset:exts[i].end]) if i < len(exts) else b''})
    return out


def _world(frames: List[dict]) -> List[np.ndarray]: #vers 1
    """World matrix of every frame (row vectors: local @ parent world)."""
    w = [None] * len(frames)
    for i, f in enumerate(frames):
        w[i] = f['m'] if f['parent'] < 0 else f['m'] @ w[f['parent']]
    return w


def _target_of(frames: List[dict], i: int, by_id: Dict[int, int]) -> int: #vers 1
    """Game bone id that cutscene frame i's skin weights move to."""
    f = frames[i]
    if f['id'] in by_id:
        return f['id']
    nm = f['name'].strip().lower()
    if f['id'] is not None and 5000 <= f['id'] < 6000 or nm.startswith(('jaw', 'llip', 'rlip')):
        if nm.startswith('rbrow'):
            return 7
        if nm.startswith('lbrow'):
            return 6
        if nm.startswith(('jaw', 'llip', 'rlip')) or nm.endswith('corner'):
            return 8
        return 5
    p = f['parent']
    while p > 0 and frames[p]['id'] not in by_id:
        p = frames[p]['parent']
    return frames[p]['id'] if p > 0 else 0


def _skin_payload(raw: bytes, sk, nverts: int, remap: np.ndarray, inv_new: np.ndarray) -> bytes: #vers 1
    """New Skin PLG payload on the game skeleton (weights merged, top 4, normalised)."""
    o = sk.data_start
    nb, nused, _mw, _pad = raw[o:o + 4]
    o += 4 + nused
    idx = np.frombuffer(raw, np.uint8, nverts * 4, o).reshape(nverts, 4).astype(np.int64)
    o += nverts * 4
    w = np.frombuffer(raw, '<f4', nverts * 4, o).reshape(nverts, 4).astype(np.float64)
    nbn = len(SA_PED_HIER)
    acc = np.zeros((nverts, nbn))
    tgt = remap[np.minimum(idx, nb - 1)]
    for k in range(4):
        np.add.at(acc, (np.arange(nverts), tgt[:, k]), w[:, k])
    order = np.argsort(-acc, axis=1)[:, :4]
    nw = np.take_along_axis(acc, order, 1)
    s = nw.sum(1, keepdims=True)
    nw = np.where(s > 0, nw / np.where(s > 0, s, 1), 0)
    order = np.where(nw > 0, order, 0)
    used = sorted(set(order[nw > 0].tolist()) | {0})
    maxw = int((nw > 0).sum(1).max()) if nverts else 0
    body = bytes([nbn, len(used), max(1, maxw), 0]) + bytes(used)
    body += order.astype(np.uint8).tobytes() + nw.astype('<f4').tobytes()
    body += inv_new.astype('<f4').tobytes()
    return body + struct.pack('<3i', 0, 0, 0)                   # no skin split


def convert_cutscene_ped(raw: bytes) -> Tuple[bytes, str]: #vers 1
    """Cutscene ped DFF bytes -> (in-game ped DFF bytes, short report)."""
    roots = rw.parse_rw(raw)
    clump = next((r for r in roots if r.type == 0x10), None)
    fl = next((c for c in clump.children if c.type == 0x0E), None) if clump else None
    if fl is None:
        raise ValueError("Not a clump DFF")
    frames = _frame_list(raw, fl)
    if len(frames) <= len(SA_PED_FRAMES) + 1 or not any(f['id'] and f['id'] >= 5000 for f in frames):
        raise ValueError("Not a cutscene ped (no face bones); in-game peds need no conversion")
    by_name = {f['name'].strip().lower(): i for i, f in enumerate(frames)}
    src_of_id = {f['id']: i for i, f in enumerate(frames) if f['id'] is not None}
    for gid, nm in _FACE_SOURCE.items():
        if gid not in src_of_id and nm in by_name:
            src_of_id[gid] = by_name[nm]
    missing = [n for n, gid, _p in SA_PED_FRAMES if gid not in src_of_id]
    if missing:
        raise ValueError("Cutscene ped lacks bones: " + ", ".join(m.strip() for m in missing))
    world = _world(frames)
    stamp = fl.stamp
    # new frame list: frame 0 kept, then the 32 game bones
    game_id_frame = {gid: k + 1 for k, (_n, gid, _p) in enumerate(SA_PED_FRAMES)}
    mats, exts = [frames[0]['m']], [frames[0]['ext']]
    flags = [frames[0]['flags']] + [3] * len(SA_PED_FRAMES)
    parents = [-1] + [p for _n, _g, p in SA_PED_FRAMES]
    new_world = [world[0]]
    for k, (name, gid, par) in enumerate(SA_PED_FRAMES):
        src = src_of_id[gid]
        new_world.append(world[src])
        mats.append(world[src] @ np.linalg.inv(new_world[par]))
        flags[k + 1] = frames[src]['flags']
        hanim = struct.pack('<Iii', 256, gid, 0)
        if gid == 0:
            hanim = struct.pack('<IiiII', 256, 0, len(SA_PED_HIER), 0, 36) + b''.join(
                struct.pack('<iiI', hid, j, fl_) for j, (hid, fl_) in enumerate(SA_PED_HIER))
        exts.append(rw.make_section(0x03, stamp, rw.make_section(0x11E, stamp, hanim)
                                    + rw.make_section(0x0253F2FE, stamp, name.encode('latin-1'))))
    body = struct.pack('<i', len(mats))
    for m, p, f in zip(mats, parents, flags):
        body += struct.pack('<9f', *m[:3, :3].reshape(9)) + struct.pack('<3f', *m[3, :3]) + struct.pack('<iI', p, f)
    new_fl = rw.make_section(0x01, fl.children[0].stamp, body) + b''.join(exts)
    # skin: cutscene bone index (hierarchy order) -> game bone index
    root_h = next((c for f in fl.children if f.type == 0x03 for c in f.children
                   if c.type == 0x11E and c.size > 12), None)
    nb_cs = struct.unpack_from('<i', raw, root_h.data_start + 8)[0]
    cs_hier_ids = [struct.unpack_from('<i', raw, root_h.data_start + 20 + j * 12)[0] for j in range(nb_cs)]
    game_index = {hid: j for j, (hid, _f) in enumerate(SA_PED_HIER)}
    by_id = {hid: 1 for hid, _f in SA_PED_HIER}
    remap = np.array([game_index[_target_of(frames, src_of_id.get(h, 0), by_id)] for h in cs_hier_ids])
    cs_skins = []
    for x in clump.walk():
        if x.type == 0x116:
            geo = x.parent.parent
            nverts = struct.unpack_from('<i', raw, geo.children[0].data_start + 8)[0]
            o = x.data_start
            nb, nused = raw[o], raw[o + 1]
            o += 4 + nused + nverts * 20
            inv_cs = np.frombuffer(raw, '<f4', nb * 16, o).reshape(nb, 16)
            cs_index = {h: j for j, h in enumerate(cs_hier_ids)}
            inv_new = np.array([inv_cs[cs_index[frames[src_of_id[hid]]['id']]] for hid, _f in SA_PED_HIER])
            cs_skins.append((x.offset, _skin_payload(raw, x, nverts, remap, inv_new)))
    # atomics: frame index remapped
    atom_fix = []
    for a in clump.children:
        if a.type == 0x14:
            fi = struct.unpack_from('<i', raw, a.children[0].data_start)[0]
            tgt = game_id_frame.get(_target_of(frames, fi, by_id), 0) if fi > 0 else 0
            atom_fix.append((a.children[0].data_start, tgt))
    out = bytearray(raw)
    for off, tgt in atom_fix:
        struct.pack_into('<i', out, off, tgt)
    out = bytes(out)
    for off, payload in sorted(cs_skins, reverse=True):       # later sections first: offsets stay valid
        out = rw.replace_payload(out, rw.node_at(rw.parse_rw(out), off), payload)
    fl_node = next(c for c in rw.parse_rw(out)[0].children if c.type == 0x0E)
    out = rw.replace_payload(out, fl_node, new_fl)
    moved = sorted({frames[src_of_id.get(h, 0)]['name'].strip() for h in cs_hier_ids
                    if h not in by_id}, key=str.lower)
    return out, (f"{len(frames) - 1} bones -> {len(SA_PED_FRAMES)}; "
                 f"{len(moved)} merged: " + ", ".join(moved))
