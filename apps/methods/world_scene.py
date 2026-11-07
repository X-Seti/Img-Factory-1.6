#this belongs in apps/methods/world_scene.py - Version: 4
# X-Seti - November 02 2026 - IMG Factory 1.6 - World scene around a point

"""
Load a game's map (IDE / IPL, SA binary stream IPLs from the IMGs, IV WPLs)
and build viewport world entries for the objects around a point (cutscene
backdrops), turned into the scene's frame (IV .cut orient).
"""

##Methods list -
# _is_lod
# _mlo_entities
# _qmul
# build_entries
# footprints
# instances_near
# load_world

import math
import os
import re
from dataclasses import replace
from typing import List, Tuple

from apps.methods.gta_dat_parser import BinaryIPLParser, GTAWorldLoader

_STREAM_RE = re.compile(r'_stream\d+\.ipl$', re.IGNORECASE)
_LOD_RE = re.compile(r'^s?lod|lod\d*$')


def _is_lod(name: str) -> bool: #vers 1
    """LOD model names: lod* (SA), slod* / *lod / *lodN (IV)."""
    return bool(_LOD_RE.search(name))


def _qmul(a, b) -> tuple: #vers 1
    """Quaternion product a * b, (x, y, z, w)."""
    ax, ay, az, aw = a
    bx, by, bz, bw = b
    return (aw * bx + ax * bw + ay * bz - az * by, aw * by - ax * bz + ay * bw + az * bx,
            aw * bz + ax * by - ay * bx + az * bw, aw * bw - ax * bx - ay * by - az * bz)


def _mlo_entities(loader, inst, obj) -> list: #vers 1
    """IV interior (mlo) placement as world instances of its entities."""
    out = []
    q = (-inst.rot_x, -inst.rot_y, -inst.rot_z, inst.rot_w)   # stored as the conjugate
    for name, (px, py, pz), (ex, ey, ez, ew) in obj.extra.get('mlo_entities', []):
        hit = loader._iv_name_ids.get(name.lower())
        if hit is None:
            continue
        v = _qmul(_qmul(q, (px, py, pz, 0.0)), (-q[0], -q[1], -q[2], q[3]))   # entity offset turned
        x, y, z, w = _qmul(q, (-ex, -ey, -ez, ew))
        out.append(replace(inst, model_id=hit[0], model_name=name, pos_x=inst.pos_x + v[0],
                           pos_y=inst.pos_y + v[1], pos_z=inst.pos_z + v[2],
                           rot_x=-x, rot_y=-y, rot_z=-z, rot_w=w))
    return out


def load_world(game_root: str, game: str): #vers 1
    """(loader, model cache) for a game root; SA stream IPLs from its IMGs added."""
    from apps.methods.img_core_classes import IMGFile
    from apps.methods.model_cache import ModelCache
    loader = GTAWorldLoader(game)
    if not loader.load(game_root):
        return None, None
    imgs = [e[2] for e in loader.load_log if len(e) > 3 and e[1] == 'IMG' and e[3]]
    models = next((os.path.join(game_root, d) for d in os.listdir(game_root) if d.lower() == 'models'), '')
    gint = next((os.path.join(models, f) for f in (os.listdir(models) if models else [])
                 if f.lower() == 'gta_int.img'), None)
    if gint and gint not in imgs:                # SA interiors: loaded by the exe, not gta.dat
        imgs.append(gint)
    for path in imgs:                            # gta3.img, gta_int.img: *_streamN.ipl
        arc = IMGFile(path)
        if not arc.open():
            continue
        for e in arc.entries:
            if _STREAM_RE.search(e.name):
                p = BinaryIPLParser(game)
                if p.parse(arc.read_entry_data(e), e.name):
                    loader.instances += p.instances
    cache = ModelCache()
    cache.index_img_files(imgs)
    return loader, cache


def instances_near(loader, centre, radius: float = 150.0, height: float = 60.0) -> list: #vers 3
    """Non-LOD instances within radius (x / y) and height (z) of centre; IV interiors expanded."""
    out = []
    for inst in loader.instances:
        if abs(inst.pos_z - centre[2]) > height or \
                math.hypot(inst.pos_x - centre[0], inst.pos_y - centre[1]) > radius:
            continue
        obj = loader.get_object(inst.model_id)
        if obj is not None and obj.section == 'mlo':
            for sub in _mlo_entities(loader, inst, obj):
                so = loader.get_object(sub.model_id)
                if not _is_lod(sub.model_name.lower()):
                    out.append((sub, sub.model_name.lower(), so.txd_name if so else ''))
            continue
        name = (inst.model_name or (obj.model_name if obj else '')).lower()
        if name and not _is_lod(name):
            out.append((inst, name, obj.txd_name if obj else ''))
    return out


def build_entries(cache, near, origin, heading: float = 0.0) -> Tuple[List[dict], list]: #vers 2
    """Viewport world entries in the scene frame (origin, heading degrees) and their textures."""
    converted, entries, textures, seen_txd = {}, [], [], set()
    h = math.radians(heading) / 2
    oz, ow = math.sin(h), math.cos(h)                # scene quat, stored like an IPL rotation
    ca, sa = math.cos(2 * h), math.sin(2 * h)
    for inst, name, txd in near:
        if name not in converted:
            model = cache.get_geometry(name)
            if model is None or not model.geometries:
                converted[name] = None
            else:
                verts, norms, uvs, prelit, tris, mats = [], [], [], [], [], []
                colours = any(getattr(g, 'colors', None) for g in model.geometries)
                for g in model.geometries:
                    vo, mo = len(verts), len(mats)
                    verts.extend((v.x, v.y, v.z) for v in g.vertices)
                    if g.normals:
                        norms.extend((n.x, n.y, n.z) for n in g.normals)
                    if g.uv_layers:
                        uvs.extend((u.u, u.v) for u in g.uv_layers[0])
                    if getattr(g, 'colors', None):
                        prelit.extend((c.r, c.g, c.b, c.a) for c in g.colors)
                    elif colours:
                        prelit.extend((255, 255, 255, 255) for _ in g.vertices)
                    tris.extend((t.v1 + vo, t.v2 + vo, t.v3 + vo, t.material_id + mo) for t in g.triangles)
                    mats.extend(g.materials)
                converted[name] = {'vertices': verts, 'normals': norms, 'uvs': uvs, 'triangles': tris,
                                   'materials': mats, 'prelit': prelit,
                                   'geom_flags': getattr(model.geometries[0], 'flags', 0)}
                textures.extend(getattr(model, 'embedded_textures', {}).values())   # IV .wdr textures
            if txd and txd.lower() not in seen_txd:
                seen_txd.add(txd.lower())
                tex = cache.get_textures(txd)
                if tex:
                    textures.extend(tex.values())
        base = converted[name]
        if base is None:
            continue
        e = dict(base)
        dx, dy = inst.pos_x - origin[0], inst.pos_y - origin[1]
        e['pos'] = (ca * dx - sa * dy, sa * dx + ca * dy, inst.pos_z - origin[2])
        x, y, z, w = -inst.rot_x, -inst.rot_y, -inst.rot_z, inst.rot_w   # IPL stores the conjugate
        e['rot'] = (ow * x - oz * y, ow * y + oz * x, ow * z + oz * w, ow * w - oz * z)   # scene quat * rot
        s = (inst.scale_x, inst.scale_y, inst.scale_z)
        e['scale'] = (1.0, 1.0, 1.0) if s == (0.0, 0.0, 0.0) else s
        e['model_key'] = name
        entries.append(e)
    return entries, textures


def footprints(entries) -> list: #vers 1
    """Top-down outline (4 corners, scene x / y) of each world entry, for a plan view."""
    out = []
    for e in entries:
        V = e.get('vertices')
        if not V:
            continue
        xs, ys = [v[0] for v in V], [v[1] for v in V]
        sx, sy = e.get('scale', (1.0, 1.0, 1.0))[:2]
        x, y, z, w = e['rot']
        r00, r01 = 1 - 2 * (y * y + z * z), 2 * (x * y - w * z)       # rotation, x / y rows
        r10, r11 = 2 * (x * y + w * z), 1 - 2 * (x * x + z * z)
        px, py = e['pos'][0], e['pos'][1]
        out.append([(px + r00 * a * sx + r01 * b * sy, py + r10 * a * sx + r11 * b * sy)
                    for a, b in ((min(xs), min(ys)), (max(xs), min(ys)), (max(xs), max(ys)), (min(xs), max(ys)))])
    return out
