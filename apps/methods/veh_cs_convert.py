#this belongs in apps/methods/veh_cs_convert.py - Version: 1
# X-Seti - November 06 2026 - IMG Factory 1.6 - Cutscene vehicle to in-game vehicle

"""
Cutscene vehicle DFF (SA cs*92 / VC cs_* models: body parts only, no
dummies, damage parts or collision) onto a donor in-game vehicle: each
donor part takes the matching cutscene mesh, moved into the donor part's
frame; dummies, collision and unmatched parts stay the donor's. TXD: the
cutscene textures plus any donor textures the kept parts use.
"""

##Methods list -
# _atomics
# _base
# _clump
# _move_geometry
# convert_cutscene_vehicle
# find_ide_line
# guess_part_map
# merge_txd
# part_names

import re
import struct
from typing import Dict, List, Optional, Tuple

import numpy as np

from apps.methods import rw_chunks as rw
from apps.methods.dff_patch import _geometry_layout
from apps.methods.ped_cs_convert import _frame_list, _world

# donor part base -> words a cutscene part name may use for it
ALIASES = {
    'chassis': ('chassis', 'body', 'bod'), 'bonnet': ('bonnet', 'bont', 'hood'), 'boot': ('boot', 'trunk'),
    'bump_front': ('bump_front', 'bmpf', 'fbump'), 'bump_rear': ('bump_rear', 'bmpr', 'rbump'),
    'door_lf': ('door_lf', 'lfdr'), 'door_rf': ('door_rf', 'rfdr'), 'door_lr': ('door_lr', 'lrdr'),
    'door_rr': ('door_rr', 'rrdr'), 'windscreen': ('windscreen', 'wnds', 'wind'),
    'wing_lf': ('wing_lf', 'wglf'), 'wing_rf': ('wing_rf', 'wgrf'), 'wheel': ('wheel', 'whl'),
    'exhaust': ('exhaust',), 'extra1': ('extra1',), 'extra2': ('extra2',)}
_SUFFIX = re.compile(r'(_hi)?(_ok|_dam|_vlo)$|_hi$')


def _clump(raw: bytes):
    """Clump node and its frame list node."""
    roots = rw.parse_rw(raw)
    clump = next((r for r in roots if r.type == 0x10), None)
    fl = next((c for c in clump.children if c.type == 0x0E), None) if clump else None
    if fl is None:
        raise ValueError("Not a clump DFF")
    return clump, fl


def _atomics(raw: bytes, clump) -> List[Tuple[int, int]]:
    """(frame index, geometry index) of every atomic, in file order."""
    return [struct.unpack_from('<ii', raw, a.children[0].data_start)
            for a in clump.children if a.type == 0x14]


def _base(name: str) -> str:
    """Part base name: door_lf_hi_ok -> door_lf."""
    return _SUFFIX.sub('', name.strip().lower())


def part_names(raw: bytes) -> List[str]: #vers 1
    """Frame names that carry a mesh."""
    clump, fl = _clump(raw)
    frames = _frame_list(raw, fl)
    return [frames[fi]['name'] for fi, _gi in _atomics(raw, clump)]


def guess_part_map(cs_parts: List[str], donor_parts: List[str]) -> Dict[str, str]: #vers 1
    """Donor part -> cutscene part, by name, then by alias words."""
    low = {p.lower(): p for p in cs_parts}
    out = {}
    for d in donor_parts:
        dl, base = d.lower(), _base(d)
        hit = low.get(dl) or low.get(base + '_ok') or low.get(base)
        if hit is None and base == 'chassis':
            hit = next((low[c] for c in low if c.startswith('chassis')), None)
        if hit is None:
            words = ALIASES.get(base, ())
            cands = [low[c] for c in low if any(w in c for w in words)]
            if base == 'wheel' and len(cands) > 1:   # one wheel mesh: front right
                cands.sort(key=lambda c: ('rf' not in c.lower(), c))
            hit = cands[0] if cands else None
        if hit is not None:
            out[d] = hit
    return out


def _move_geometry(raw: bytes, gnode, T: np.ndarray) -> bytes: #vers 1
    """Geometry chunk bytes with positions / normals moved by row-vector matrix T."""
    lay = _geometry_layout(raw, gnode)
    out = bytearray(raw[gnode.offset:gnode.end])
    base, n = gnode.offset, lay['verts']
    if lay['pos'] is not None and n:
        o = lay['pos'] - base
        P = np.frombuffer(bytes(out[o:o + 12 * n]), '<f4').reshape(n, 3).astype(np.float64)
        P = P @ T[:3, :3] + T[3, :3]
        out[o:o + 12 * n] = P.astype('<f4').tobytes()
        c = (P.min(0) + P.max(0)) / 2
        r = float(np.linalg.norm(P - c, axis=1).max())
        struct.pack_into('<4fII', out, lay['bsphere'] - base, *c, r, 1, 1 if lay['nrm'] else 0)
    if lay['nrm'] is not None and n:
        o = lay['nrm'] - base
        N = np.frombuffer(bytes(out[o:o + 12 * n]), '<f4').reshape(n, 3).astype(np.float64) @ T[:3, :3]
        N /= np.maximum(np.linalg.norm(N, axis=1, keepdims=True), 1e-9)
        out[o:o + 12 * n] = N.astype('<f4').tobytes()
    return bytes(out)


def convert_cutscene_vehicle(cs_raw: bytes, donor_raw: bytes,
                             part_map: Optional[Dict[str, str]] = None) -> Tuple[bytes, str, List[str]]: #vers 1
    """(vehicle DFF, report, donor parts kept) with the cutscene meshes on the donor's parts."""
    cs_clump, cs_fl = _clump(cs_raw)
    d_clump, d_fl = _clump(donor_raw)
    cs_frames, d_frames = _frame_list(cs_raw, cs_fl), _frame_list(donor_raw, d_fl)
    cs_world, d_world = _world(cs_frames), _world(d_frames)
    cs_geo = [g for g in next(c for c in cs_clump.children if c.type == 0x1A).children if g.type == 0x0F]
    d_glist = next(c for c in d_clump.children if c.type == 0x1A)
    d_geo = [g for g in d_glist.children if g.type == 0x0F]
    cs_by_name = {cs_frames[fi]['name'].lower(): (fi, gi) for fi, gi in _atomics(cs_raw, cs_clump)}
    if part_map is None:
        part_map = guess_part_map([cs_frames[fi]['name'] for fi, _g in _atomics(cs_raw, cs_clump)],
                                  [d_frames[fi]['name'] for fi, _g in _atomics(donor_raw, d_clump)])
    d_atoms = _atomics(donor_raw, d_clump)
    d_chassis = next((fi for fi, _g in d_atoms if d_frames[fi]['name'].lower() in ('chassis', 'chassis_hi')), None)
    cs_chassis = cs_by_name.get(part_map.get(d_frames[d_chassis]['name'], '').lower()) if d_chassis else None
    if cs_chassis is None:
        raise ValueError("Map a cutscene part to the donor's chassis first")
    align = np.linalg.inv(cs_world[cs_chassis[0]]) @ d_world[d_chassis]   # cutscene car onto the donor's chassis
    new_geo = {}
    done, kept = [], []
    for fi, gi in d_atoms:
        dn = d_frames[fi]['name']
        src = cs_by_name.get(part_map.get(dn, '').lower())
        if src is None:
            kept.append(dn)
            continue
        cfi, cgi = src
        if _base(dn) == 'wheel':                     # one mesh for 4 wheels: kept centred on its frame
            T = np.eye(4)
        else:                                        # same place on the car, in the donor part's frame
            T = cs_world[cfi] @ align @ np.linalg.inv(d_world[fi])
        new_geo[gi] = _move_geometry(cs_raw, cs_geo[cgi], T)
        done.append(f"{dn}<-{cs_frames[cfi]['name']}")
    if not new_geo:
        raise ValueError("No cutscene part matched the donor's parts")
    st = d_glist.children[0]
    body = donor_raw[st.offset:st.end]
    for gi, g in enumerate(d_geo):
        body += new_geo.get(gi, donor_raw[g.offset:g.end])
    out = rw.replace_payload(donor_raw, d_glist, body)
    rep = f"{len(done)} parts from the cutscene model; {len(kept)} kept from the donor"
    return out, rep + (": " + ", ".join(kept) if kept else ""), kept


def merge_txd(cs_txd: bytes, donor_txd: Optional[bytes], wanted: Optional[List[str]] = None) -> bytes: #vers 1
    """Cutscene TXD plus donor textures it lacks (only those named in wanted, if given)."""
    from apps.methods.txd_splice import _chunk_names, _platform_of, split_txd
    a = split_txd(cs_txd)
    b = split_txd(donor_txd) if donor_txd else None
    if a is None:
        raise ValueError("Cutscene TXD not readable")
    head, st, natives, tail = a
    pos = 28 + sum(len(c) for c in natives)
    tail = tail[:max(0, 12 + struct.unpack_from('<I', cs_txd, 4)[0] - pos)]   # IMG padding dropped
    if b is not None:
        have = {_chunk_names(c)[0].lower() for c in natives}
        plat = _platform_of(natives[0]) if natives else None
        want = {w.lower() for w in wanted} if wanted is not None else None
        for c in b[2]:
            nm = _chunk_names(c)[0].lower()
            if nm not in have and (want is None or nm in want) and (plat is None or _platform_of(c) == plat):
                natives.append(c)
                have.add(nm)
    st = st[:12] + struct.pack('<HH', len(natives), struct.unpack_from('<HH', st, 12)[1])
    payload = st + b''.join(natives) + tail
    return struct.pack('<III', 0x16, len(payload), struct.unpack_from('<I', head, 8)[0]) + payload


def find_ide_line(ide_text: str, model: str) -> Optional[str]: #vers 1
    """The 'cars' section line of model in vehicles.ide / default.ide."""
    sec = None
    for line in ide_text.splitlines():
        s = line.split('#')[0].strip()
        low = s.lower()
        if low in ('cars', 'peds', 'objs', 'tobj', 'weap', 'hier', 'anim', 'txdp', 'path', '2dfx'):
            sec = low
            continue
        if low == 'end':
            sec = None
            continue
        parts = [p.strip() for p in s.split(',')]
        if sec == 'cars' and len(parts) > 2 and parts[1].lower() == model.lower():
            return s
    return None
