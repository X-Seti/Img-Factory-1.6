#this belongs in apps/methods/retro_mesh.py - Version: 3
# X-Seti - October 24 2026 - IMG Factory 1.6 - top-down GTA map blocks to 3D

"""
GTA1 / GTA2 map blocks as 3D meshes: every block a box, its lid on top
and its four wall faces on the block edges (tile, flip, lid rotation
kept; gradient slopes 1-44 tilt the lid, diagonal / partial blocks drawn as
boxes; lids under blocks and walls against walls left out). An area limits it. Textures are the style tiles ('tile_NNN', the
same names as the style TXD export). Output: one OBJ of the whole map
(world coordinates), and per sector a DFF + COL model (sector-local),
with IDE / IPL lines placing the sectors and a TXD of the used tiles.
"""

##Methods list -
# _hidden
# _quad
# lid_heights
# export_map_3d
# rgba_txd_entry
# sector_model

import os
from typing import Dict, List, Tuple

import numpy as np

from apps.methods.col_workshop_classes import (COLBounds, COLFace, COLHeader, COLModel, COLVersion,
                                              COLVertex)
from apps.methods.dff_classes import (Atomic, DFFModel, Frame, Geometry, Material, TexCoord, Triangle,
                                      Vector3)
from apps.methods.gta2_map import face_flags, face_tile

SECTOR = 16                              # blocks per sector side
WALLS = {'left': ((0, 0), (0, 1)), 'right': ((1, 1), (1, 0)),   # (x, y) of the wall's bottom corners, left to
         'top': ((1, 0), (0, 0)), 'bottom': ((0, 1), (1, 1))}   # right seen from outside (world y is -map y)
NEXT = {'left': (-1, 0, 'right'), 'right': (1, 0, 'left'), 'top': (0, -1, 'bottom'), 'bottom': (0, 1, 'top')}


def rgba_txd_entry(name: str, rgba: np.ndarray) -> dict: #vers 1
    """32-bit TXD texture dict (SA / D3D8 layout) for build_txd."""
    return {'name': name[:31], 'width': rgba.shape[1], 'height': rgba.shape[0],
            'rgba_data': np.ascontiguousarray(rgba).tobytes(), 'has_alpha': True,
            'format': 'ARGB8888', 'alpha_name': '', 'mipmaps': 1, 'mipmap_levels': [],
            'raster_format_flags': 0x0500, 'depth': 32, 'platform_id': 8, 'filter_flags': 0x1102}


def _quad(geom: Geometry, pts, word: int, mat: int, lid: bool): #vers 2
    """Two triangles + UVs for corner points (bottom-left, bottom-right, top-right, top-left)."""
    uv = [(0, 1), (1, 1), (1, 0), (0, 0)]
    f = face_flags(word)
    if f['flip']:
        uv = [(1 - u, v) for u, v in uv]
    if lid:                                              # game: rotate clockwise, then flip
        for _r in range((4 - f['rotation']) % 4 if f['flip'] else f['rotation']):
            uv = uv[1:] + uv[:1]
    base = len(geom.vertices)
    for (px, py, pz), (u, v) in zip(pts, uv):
        geom.vertices.append(Vector3(px, py, pz))
        geom.uv_layers[0].append(TexCoord(u, v))
    geom.triangles.append(Triangle(base, base + 1, base + 2, mat))
    geom.triangles.append(Triangle(base, base + 2, base + 3, mat))


def lid_heights(slope: int) -> Tuple[float, float, float, float]: #vers 1
    """Lid corner heights (NW, NE, SE, SW, 0-1 of the block) of slope types 1-44; flat otherwise.
    1-8 26 deg (up, down, left, right x low / high), 9-40 7 deg (8 parts each), 41-44 45 deg."""
    if 1 <= slope <= 8:
        d, n, k = (slope - 1) // 2, 2, (slope - 1) % 2
    elif 9 <= slope <= 40:
        d, n, k = (slope - 9) // 8, 8, (slope - 9) % 8
    elif 41 <= slope <= 44:
        d, n, k = slope - 41, 1, 0
    else:
        return (1.0, 1.0, 1.0, 1.0)
    lo, hi = k / n, (k + 1) / n
    return ((hi, hi, lo, lo), (lo, lo, hi, hi), (hi, lo, lo, hi), (lo, hi, hi, lo))[d]


def _hidden(gmap, x: int, y: int, z: int, face: str) -> bool: #vers 1
    """Lid under a block, or wall against a neighbour's wall: not drawn."""
    if face == 'lid':
        bi = gmap.column(x, y).get(z + 1)
        return bi is not None and any(face_tile(gmap.face_word(gmap.blocks[bi], f)) for f in ('lid',) + tuple(WALLS))
    dx, dy, opp = NEXT[face]
    if not (0 <= x + dx < 256 and 0 <= y + dy < 256):
        return False
    bi = gmap.column(x + dx, y + dy).get(z)
    return bi is not None and face_tile(gmap.face_word(gmap.blocks[bi], opp)) != 0


def sector_model(gmap, style, sx: int, sy: int, scale: float, origin: Tuple[float, float],
                 used: set, area: Tuple[int, int, int, int] = (0, 0, 255, 255)) -> Geometry: #vers 2
    """Geometry of one sector (its own tile materials, cells inside area only); used collects tiles."""
    geom = Geometry(flags=0x77, uv_layer_count=1)
    tiles: Dict[int, int] = {}
    geom.uv_layers = [[]]
    ox, oy = origin
    ax0, ay0, ax1, ay1 = area
    for y in range(max(sy, ay0), min(sy + SECTOR, ay1 + 1)):
        for x in range(max(sx, ax0), min(sx + SECTOR, ax1 + 1)):
            for z, bi in gmap.column(x, y).items():
                b = gmap.blocks[bi]
                hts = lid_heights(gmap.slope_of(b))
                corner = {(0, 0): hts[0], (1, 0): hts[1], (1, 1): hts[2], (0, 1): hts[3]}
                faces = [('lid', gmap.face_word(b, 'lid'))] + [(f, gmap.face_word(b, f)) for f in WALLS]
                for face, word in faces:
                    kind = 'lid' if face == 'lid' else 'side'
                    g = style.face_global(kind, face_tile(word))
                    if g is None or _hidden(gmap, x, y, z, face):
                        continue
                    if g not in tiles:
                        tiles[g] = len(geom.materials)
                        geom.materials.append(Material(texture_name=f"tile_{g:03d}"))
                        used.add(g)
                    mat = tiles[g]

                    def P(px, py, pz):
                        return ((x + px) * scale - ox, -(y + py) * scale - oy, (z + pz) * scale)
                    if face == 'lid':
                        pts = [P(0, 1, corner[0, 1]), P(1, 1, corner[1, 1]), P(1, 0, corner[1, 0]),
                               P(0, 0, corner[0, 0])]
                    else:
                        (ax, ay), (bx, by) = WALLS[face]
                        if not (corner[ax, ay] or corner[bx, by]):
                            continue
                        pts = [P(ax, ay, 0), P(bx, by, 0), P(bx, by, corner[bx, by]), P(ax, ay, corner[ax, ay])]
                    _quad(geom, pts, word, mat, face == 'lid')
    return geom


def export_map_3d(gmap, style, folder: str, stem: str, scale: float = 4.0, dff_label: str = '',
                  first_id: int = 20000, progress=None,
                  area: Tuple[int, int, int, int] = (0, 0, 255, 255)) -> List[str]: #vers 2
    """Write OBJ (+ tile PNGs), sector DFFs, one COL, IDE, IPL and TXD; returns report lines."""
    from PyQt6.QtGui import QImage
    from apps.methods.col_workshop_writer import save_col_file
    from apps.methods.model_convert import model_to_obj, save_model
    from apps.methods.txd_splice import build_txd
    os.makedirs(folder, exist_ok=True)
    used = set()
    world = DFFModel(source_path=stem)
    cols, ide, ipl, report = [], ["objs"], ["inst"], []
    n_sec = (256 // SECTOR) ** 2
    for k in range(n_sec):
        sy, sx = divmod(k, 256 // SECTOR)
        sx, sy = sx * SECTOR, sy * SECTOR
        if progress and not progress(k, n_sec):
            raise InterruptedError("Export cancelled")
        cx, cy = (sx + SECTOR / 2) * scale, -(sy + SECTOR / 2) * scale
        if sx > area[2] or sy > area[3] or sx + SECTOR <= area[0] or sy + SECTOR <= area[1]:
            continue
        geom = sector_model(gmap, style, sx, sy, scale, (cx, cy), used, area)
        if not geom.triangles:
            continue
        name = f"{stem}_{sx // SECTOR:02d}_{sy // SECTOR:02d}"[:23]
        model = DFFModel(frames=[Frame(name=name)], geometries=[geom], atomics=[Atomic(0, 0, 5)], source_path=name)
        if dff_label:
            save_model(model, os.path.join(folder, name + '.dff'), dff_label)
        verts = [COLVertex(v.x, v.y, v.z) for v in geom.vertices]
        faces = [COLFace(t.v1, t.v2, t.v3, 0, 0, 0, 0) for t in geom.triangles]
        lo = [min(getattr(v, a) for v in verts) for a in 'xyz']
        hi = [max(getattr(v, a) for v in verts) for a in 'xyz']
        mid = [(a + b) / 2 for a, b in zip(lo, hi)]
        rad = max(np.linalg.norm(np.subtract(hi, lo)) / 2, 0.1)
        cols.append(COLModel(header=COLHeader(b'COL3', 0, name, 0, COLVersion.COL_3),
                             bounds=COLBounds(rad, mid, lo, hi), spheres=[], boxes=[],
                             vertices=verts, faces=faces))
        oid = first_id + len(ide) - 1
        ide.append(f"{oid}, {name}, {stem}, 1, 299, 0")
        ipl.append(f"{oid}, {name}, {cx:.3f}, {cy:.3f}, 0.000, 1, 1, 1, 0, 0, 0, 1")
        wg = Geometry(flags=0x77, uv_layer_count=1, vertices=[Vector3(v.x + cx, v.y + cy, v.z) for v in geom.vertices],
                      uv_layers=geom.uv_layers, triangles=geom.triangles, materials=geom.materials)
        world.geometries.append(wg)
        world.atomics.append(Atomic(len(world.frames), len(world.geometries) - 1, 5))
        world.frames.append(Frame(name=name))
    v, f = model_to_obj(world, os.path.join(folder, stem + '.obj'))
    report.append(f"OBJ: {v} vertices, {f} triangles, {len(world.geometries)} sectors")
    texs = []
    for g in sorted(used):
        a = np.ascontiguousarray(style.tile_rgba(g))
        QImage(a.data, a.shape[1], a.shape[0], a.shape[1] * 4, QImage.Format.Format_RGBA8888).save(
            os.path.join(folder, f"tile_{g:03d}.png"), 'PNG')
        texs.append(rgba_txd_entry(f"tile_{g:03d}", a))
    with open(os.path.join(folder, stem + '.txd'), 'wb') as fh:
        fh.write(build_txd(texs, 0x1803FFFF, platform=8))
    if not save_col_file(cols, os.path.join(folder, stem + '.col')):
        raise ValueError("COL write failed")
    with open(os.path.join(folder, stem + '.ide'), 'w') as fh:
        fh.write("\n".join(ide + ["end", ""]))
    with open(os.path.join(folder, stem + '.ipl'), 'w') as fh:
        fh.write("\n".join(ipl + ["end", ""]))
    report.append(f"{len(cols)} sector models" + (f" ({dff_label})" if dff_label else "") +
                  f", {len(texs)} tiles, COL / IDE / IPL / TXD written")
    return report
