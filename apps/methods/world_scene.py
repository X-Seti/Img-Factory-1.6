#this belongs in apps/methods/world_scene.py - Version: 1
# X-Seti - November 02 2026 - IMG Factory 1.6 - World scene around a point

"""
Load a game's map (IDE / IPL, SA binary stream IPLs from the IMGs) and build
viewport world entries for the objects around a point (cutscene backdrops).
"""

##Methods list -
# build_entries
# instances_near
# load_world

import math
import os
import re
from typing import List, Tuple

from apps.methods.gta_dat_parser import BinaryIPLParser, GTAWorldLoader

_STREAM_RE = re.compile(r'_stream\d+\.ipl$', re.IGNORECASE)


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


def instances_near(loader, centre, radius: float = 150.0, height: float = 60.0) -> list: #vers 1
    """Non-LOD instances within radius (x / y) and height (z) of centre."""
    out = []
    for inst in loader.instances:
        if abs(inst.pos_z - centre[2]) > height or \
                math.hypot(inst.pos_x - centre[0], inst.pos_y - centre[1]) > radius:
            continue
        obj = loader.get_object(inst.model_id)
        name = (inst.model_name or (obj.model_name if obj else '')).lower()
        if name and not name.startswith('lod'):
            out.append((inst, name, obj.txd_name if obj else ''))
    return out


def build_entries(cache, near, origin) -> Tuple[List[dict], list]: #vers 1
    """Viewport world entries (positions relative to origin) and their textures."""
    converted, entries, textures, seen_txd = {}, [], [], set()
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
            if txd and txd.lower() not in seen_txd:
                seen_txd.add(txd.lower())
                tex = cache.get_textures(txd)
                if tex:
                    textures.extend(tex.values())
        base = converted[name]
        if base is None:
            continue
        e = dict(base)
        e['pos'] = (inst.pos_x - origin[0], inst.pos_y - origin[1], inst.pos_z - origin[2])
        e['rot'] = (-inst.rot_x, -inst.rot_y, -inst.rot_z, inst.rot_w)   # IPL stores the conjugate
        s = (inst.scale_x, inst.scale_y, inst.scale_z)
        e['scale'] = (1.0, 1.0, 1.0) if s == (0.0, 0.0, 0.0) else s
        e['model_key'] = name
        entries.append(e)
    return entries, textures
