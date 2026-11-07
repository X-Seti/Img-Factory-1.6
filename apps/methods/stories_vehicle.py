#this belongs in apps/methods/stories_vehicle.py - Version: 1
# X-Seti - November 07 2026 - IMG Factory 1.6 - LCS/VCS vehicle export

"""
LCS / VCS vehicles to PC: the .mdl node tree becomes DFF frames (dummies,
lights, wheel dummies), each part's two elements become _hi_ok / _hi_dam
atomics; the car's .chk / .xtx textures plus the shared xv_ textures from
GAME.DTZ go into one TXD so the car renders on its own.
"""

##Methods list -
# _node_tree
# dtz_texlist
# export_vehicle
# vehicle_model
# vehicle_textures

import os
import struct
from typing import Dict, List, Optional, Tuple

from apps.methods.dff_classes import RGBA, Atomic, DFFModel, Frame, Vector3
from apps.methods.stories_mdl import _cstr, _element_geometry, _elements, is_stories_mdl

GAME_RW = {'iii': 0x32000, 'vc': 0x33002, 'sa': 0x36003}      # target DFF / TXD version
_RENDER = 0x04                                                 # element flag: shown (ok part)


def _node_tree(d: bytes, root: int) -> List[Tuple[int, int]]: #vers 1
    """[(node offset, parent list index)] depth first (child +144, next +148)."""
    u = lambda o: struct.unpack_from('<I', d, o)[0]
    out, stack, seen = [], [(root, -1)], set()
    while stack:
        n, parent = stack.pop()
        while 0 < n < len(d) - 168 and n not in seen:
            seen.add(n)
            out.append((n, parent))
            child = u(n + 144)
            if child:
                stack.append((child, len(out) - 1))
            n = u(n + 148)
    return out


def vehicle_model(data: bytes, name: str = '') -> Optional[DFFModel]: #vers 1
    """DFFModel with frames and atomics from an LCS / VCS vehicle .mdl; None if not one."""
    if not is_stories_mdl(data):
        return None
    d = data
    u = lambda o: struct.unpack_from('<I', d, o)[0]
    els = _elements(d)
    if not els:
        return None
    first = u(els[0] + 4)
    root = u(first + 152) or first
    tree = _node_tree(d, root)
    if not tree:
        return None
    model = DFFModel(source_path=name)
    index = {}
    ok = lambda o: sum(bool(_cstr(d, u(n + o))) for n, _p in tree)
    name_at = 164 if ok(164) >= ok(168) else 168                # LCS 164, VCS 168
    for n, parent in tree:
        m = struct.unpack_from('<16f', d, n + 16)
        index[n] = len(model.frames)
        model.frames.append(Frame(rotation=[m[0], m[1], m[2], m[4], m[5], m[6], m[8], m[9], m[10]],
                                  position=Vector3(m[12], m[13], m[14]), parent_index=parent,
                                  name=_cstr(d, u(n + name_at))[:23]))
    if model.frames[0].name == '' and name:
        model.frames[0].name = os.path.splitext(os.path.basename(name))[0][:23]
    for e in els:
        node = u(e + 4)
        g = _element_geometry(d, e)
        if node not in index or g is None or not g.vertices:
            continue
        fi = index[node]
        fname = model.frames[fi].name
        if fname.endswith('_dummy'):                           # part: ok / dam child frame
            part = fname[:-6] + ('_hi_ok' if d[e + 2] & _RENDER else '_hi_dam')
            model.frames.append(Frame(parent_index=fi, name=part[:23]))
            fi = len(model.frames) - 1
        mats = u(u(e + 20) + 12)
        for i, mat in enumerate(g.materials):                  # RslMaterial colour (+4)
            mp = u(mats + i * 4) if 0 < mats < len(d) - 4 * (i + 1) else 0
            if 0 < mp < len(d) - 8:
                mat.color = RGBA(*d[mp + 4:mp + 8])
        model.geometries.append(g)
        model.atomics.append(Atomic(fi, len(model.geometries) - 1, 5))
    return model if model.atomics else None


def dtz_texlist(dtz_data: bytes, texture: str) -> List[Dict]: #vers 1
    """Every texture of the GAME.DTZ texlist holding `texture` (PS2 LCS / VCS rasters)."""
    from apps.methods.xtx_reader import _raster_try, decode_raster
    d = dtz_data
    u = lambda o: struct.unpack_from('<I', d, o)[0]
    at = d.find(texture.encode('latin-1') + b'\0')
    if at < 16:
        return []
    texlist = u(at - 16 + 4)
    head = texlist + 8
    out, link, seen = [], u(head), set()
    while link != head and 8 <= link < len(d) - 80 and link not in seen:
        seen.add(link)
        t = link - 8
        r = u(t)
        lay = None
        for kind in ('PS2_LCS', 'PS2_VCS', 'PSP'):
            lay = _raster_try(d, r, kind, len(d), strict=False) if 0 < r < len(d) else None
            if lay:
                break
        if lay:
            rgba, idx, pal = decode_raster(d, lay)
            out.append({'name': _cstr(d, t + 16), 'alpha_name': _cstr(d, t + 48),
                        'width': lay['width'], 'height': lay['height'], 'depth': 32,
                        'rgba_data': bytes(rgba), 'has_alpha': bool((pal[:, 3] < 255).any()),
                        'format': 'RGBA', 'mipmap_levels': []})
        link = u(link)
    return out


def vehicle_textures(tex_data: bytes, dtz_data: Optional[bytes], model: DFFModel) -> Tuple[List[Dict], List[str]]: #vers 1
    """(textures the model uses, names still missing): car .chk / .xtx first, then DTZ shared."""
    from apps.methods.xtx_reader import parse_stories_textures
    have = {}
    if tex_data:
        for t in parse_stories_textures(tex_data):
            have.setdefault(t['name'].lower(), {
                'name': t['name'], 'alpha_name': t.get('alpha_name', ''), 'width': t['width'],
                'height': t['height'], 'depth': 32, 'rgba_data': bytes(t['rgba_data']),
                'has_alpha': t['has_alpha'], 'format': 'RGBA', 'mipmap_levels': []})
    used = {m.texture_name.lower() for g in model.geometries for m in g.materials if m.texture_name}
    missing = sorted(used - set(have))
    if missing and dtz_data:
        for want in list(missing):
            if want in have:
                continue
            for t in dtz_texlist(dtz_data, want):
                have.setdefault(t['name'].lower(), t)
        missing = sorted(used - set(have))
    return [have[n] for n in sorted(have) if n in used], missing


def export_vehicle(mdl: bytes, tex: bytes, dtz: Optional[bytes], name: str, out_dir: str,
                   game: str = 'vc') -> List[str]: #vers 1
    """Write <name>.dff and <name>.txd for a PC game; report lines."""
    from apps.methods import rw_chunks as rw
    from apps.methods.dff_patch import build_dff
    from apps.methods.txd_splice import txd_from_textures
    model = vehicle_model(mdl, name + '.mdl')
    if model is None:
        raise ValueError(f"{name}.mdl: not an LCS / VCS vehicle model")
    ver = GAME_RW[game]
    with open(os.path.join(out_dir, name + '.dff'), 'wb') as f:
        f.write(build_dff(model, rw.stamp_for(ver)))
    texs, missing = vehicle_textures(tex, dtz, model)
    report = [f"{name}.dff: {len(model.frames)} frames, {len(model.atomics)} parts"]
    if texs:
        with open(os.path.join(out_dir, name + '.txd'), 'wb') as f:
            f.write(txd_from_textures(texs, None, rw.stamp_for(ver)))
        report.append(f"{name}.txd: {len(texs)} textures")
    if missing:
        report.append(f"{name}: textures not found: {', '.join(missing)}")
    return report
