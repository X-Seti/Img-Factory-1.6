#this belongs in apps/methods/terrain_export.py - Version: 3
# X-Seti - October 22 2026 - IMG Factory 1.6 - Terrain export to game files

"""
Writes a Terrain as game files: one DFF + COL model per tile, one TXD with
the used material textures, an IDE (objs) and an IPL (inst) placing the tiles;
optional low-detail LOD tiles (SA: IPL LOD index, III / VC: LOD name match);
optionally adds the DFF / COL / TXD entries to an IMG archive.
"""

##Methods list -
# export_terrain
# tile_col
# tile_dff

import os
from typing import Callable, Dict, List

from apps.methods.terrain_mesh import grid_normals, lod_terrain, tile_mesh, tile_ranges

MAX_TILE_VERTS = 65535                                # RW geometry vertex index limit

RW_VERSION = {'gta3': 0x31001, 'vc': 0x34003, 'sa': 0x36003}


def tile_dff(name: str, mesh: dict, library: List[Dict], stamp: int) -> bytes: #vers 1
    """DFF clump for one tile (one frame, one geometry, materials remapped)."""
    from apps.methods.dff_classes import (Atomic, BoundingSphere, DFFModel, Frame, Geometry,
                                          Material, RGBA, TexCoord, Triangle, Vector3)
    from apps.methods.dff_patch import build_dff
    used = mesh['used']
    remap = {m: k for k, m in enumerate(used)}
    g = Geometry(flags=0x02 | 0x04 | 0x08 | 0x10 | 0x20 | 0x40)
    g.vertices = [Vector3(*v) for v in mesh['vertices']]
    g.normals = [Vector3(*n) for n in mesh['normals']]
    g.uv_layers = [[TexCoord(u, v) for u, v in mesh['uvs']]]
    g.colors = [RGBA(255, 255, 255, 255) for _ in mesh['vertices']]
    g.triangles = [Triangle(a, b, c, remap[m]) for a, b, c, m in mesh['triangles']]
    g.materials = [Material(texture_name=library[m]['texture']) for m in used]
    xs, ys, zs = zip(*mesh['vertices'])
    c = ((min(xs) + max(xs)) / 2, (min(ys) + max(ys)) / 2, (min(zs) + max(zs)) / 2)
    r = max(((x - c[0]) ** 2 + (y - c[1]) ** 2 + (z - c[2]) ** 2) ** 0.5 for x, y, z in mesh['vertices'])
    g.bounding_sphere = BoundingSphere(Vector3(*c), r)
    model = DFFModel(frames=[Frame(name=name)], geometries=[g], atomics=[Atomic(0, 0)])
    return build_dff(model, stamp)


def tile_col(name: str, mesh: dict, surfaces: Dict[int, int], game: str):
    """COLModel for one tile: the same triangles with each material's surface."""
    from apps.methods.col_workshop_classes import (COLBounds, COLFace, COLHeader, COLModel,
                                                   COLVersion, COLVertex)
    ver = COLVersion.COL_3 if game == 'sa' else COLVersion.COL_1
    verts, index = [], {}
    faces = []
    for a, b, c, m in mesh['triangles']:
        ids = []
        for k in (a, b, c):
            key = tuple(round(v, 4) for v in mesh['vertices'][k])
            if key not in index:                     # COL shares corner vertices
                index[key] = len(verts)
                verts.append(COLVertex(*key))
            ids.append(index[key])
        faces.append(COLFace(ids[0], ids[1], ids[2], surfaces.get(m, 0), 0, 0, 255))
    xs = [v.x for v in verts]; ys = [v.y for v in verts]; zs = [v.z for v in verts]
    lo, hi = (min(xs), min(ys), min(zs)), (max(xs), max(ys), max(zs))
    centre = tuple((a + b) / 2 for a, b in zip(lo, hi))
    radius = max(((v.x - centre[0]) ** 2 + (v.y - centre[1]) ** 2 + (v.z - centre[2]) ** 2) ** 0.5 for v in verts)
    fourcc = b'COL3' if game == 'sa' else b'COLL'
    return COLModel(header=COLHeader(fourcc, 0, name, 0, ver),
                    bounds=COLBounds(radius, centre, lo, hi), spheres=[], boxes=[],
                    vertices=verts, faces=faces)


def export_terrain(t, library: List[Dict], out_dir: str, game: str, prefix: str, id_start: int,
                   texture_fn: Callable, img_path: str = '', draw_dist: float = 300.0,
                   progress: Callable = None, assets=None, lod: int = 0,
                   lod_dist: float = 1500.0) -> Dict: #vers 3
    """Write models/<tile>.dff, <prefix>.col, <prefix>.txd, <prefix>.ide, <prefix>.ipl.
    texture_fn(material) -> texture dict; assets: placed game objects, added to the IPL;
    lod: LOD detail step (0 none, 2 / 4 / 8). Returns the written file paths and counts."""
    from apps.methods import rw_chunks
    from apps.methods.col_materials import COLGame, material_id_from_name
    from apps.methods.col_workshop_parser import COLWriter
    from apps.methods.txd_splice import build_txd
    rw = RW_VERSION[game]
    stamp = rw_chunks.stamp_for(rw)
    prefix = ''.join(ch for ch in prefix.lower() if ch.isalnum() or ch == '_')[:12] or 'terrain'
    if lod and game != 'sa' and len(prefix) < 3:
        raise ValueError("LODs for III / VC need a name of 3+ characters (LOD names match from the 4th)")
    if (min(t.tile_cells, t.nx) * min(t.tile_cells, t.ny) * 4) > MAX_TILE_VERTS:
        raise ValueError(f"tiles of {t.tile_cells} x {t.tile_cells} cells are over the "
                         f"{MAX_TILE_VERTS} vertex limit - use a smaller tile size")
    cg = COLGame.SA if game == 'sa' else COLGame.VC
    key = 'surface_sa' if game == 'sa' else 'surface_vc'
    surfaces = {i: material_id_from_name(m.get(key, ''), cg) or 0 for i, m in enumerate(library)}
    os.makedirs(os.path.join(out_dir, 'models'), exist_ok=True)
    pts, nrm = t.grid_points(), grid_normals(t)
    files, cols, used_all, ide, ipl = {}, [], set(), [], []
    tiles = tile_ranges(t)
    def uv(m):
        return library[m]["uv_size"]
    lt = lod_terrain(t, lod) if lod else None
    lod_meshes = {}
    if lt is not None:                                       # same tile grid, fewer points
        lpts, lnrm = lt.grid_points(), grid_normals(lt)
        for tx, ty, i0, j0 in tile_ranges(lt):
            lod_meshes[(tx, ty)] = tile_mesh(lt, i0, j0, uv, lpts, lnrm)
    lod_ide, lod_ipl = [], []
    interior = "0, " if game == 'vc' else ""

    def place(mid, name, pos, lod_index):
        if game == 'sa':
            return (f"{mid}, {name}, 0, {pos[0]:.4f}, {pos[1]:.4f}, {pos[2]:.4f}, 0, 0, 0, 1, {lod_index}")
        return f"{mid}, {name}, {interior}{pos[0]:.4f}, {pos[1]:.4f}, {pos[2]:.4f}, 1, 1, 1, 0, 0, 0, 1"

    def objs(mid, name, dist):
        return f"{mid}, {name}, {prefix}, {dist:g}, 0" if game == 'sa' else f"{mid}, {name}, {prefix}, 1, {dist:g}, 0"

    for n, (tx, ty, i0, j0) in enumerate(tiles):
        name = f"{prefix}_{tx}_{ty}"
        mesh = tile_mesh(t, i0, j0, uv, pts, nrm)
        used_all.update(mesh['used'])
        files[f"{name}.dff"] = tile_dff(name, mesh, library, stamp)
        cols.append(tile_col(name, mesh, surfaces, game))
        lmesh = lod_meshes.get((tx, ty))
        lod_index = -1
        if lmesh is not None:
            lname = f"lod{name}" if game == 'sa' else f"LOD{name[3:]}"
            lid = id_start + len(tiles) + len(lod_ide)
            lod_index = len(tiles) + len(lod_ipl)            # SA: line of the LOD in this IPL
            files[f"{lname}.dff"] = tile_dff(lname, lmesh, library, stamp)
            lod_ide.append(objs(lid, lname, lod_dist))
            lod_ipl.append(place(lid, lname, lmesh['pos'], -1))
        ide.append(objs(id_start + n, name, draw_dist))
        ipl.append(place(id_start + n, name, mesh['pos'], lod_index))
        if progress:
            progress(n + 1, len(tiles))
    ide += lod_ide
    ipl += lod_ipl
    for a in assets or []:                                 # placed game objects (their own IDs)
        (x, y, z), (qx, qy, qz, qw) = a['pos'], a['rot']
        if game == 'sa':
            ipl.append(f"{a['id']}, {a['name']}, 0, {x:.4f}, {y:.4f}, {z:.4f}, {qx:.6f}, {qy:.6f}, {qz:.6f}, {qw:.6f}, -1")
        else:
            interior = "0, " if game == 'vc' else ""
            ipl.append(f"{a['id']}, {a['name']}, {interior}{x:.4f}, {y:.4f}, {z:.4f}, 1, 1, 1, "
                       f"{qx:.6f}, {qy:.6f}, {qz:.6f}, {qw:.6f}")
    files[f"{prefix}.col"] = COLWriter.write_file(cols)
    textures = [texture_fn(library[m]) for m in sorted(used_all)]
    files[f"{prefix}.txd"] = build_txd(textures, stamp)
    written = []
    for fname, data in files.items():
        p = os.path.join(out_dir, 'models', fname)
        with open(p, 'wb') as f:
            f.write(data)
        written.append(p)
    for ext, lines, head in (('ide', ide, 'objs'), ('ipl', ipl, 'inst')):
        p = os.path.join(out_dir, f"{prefix}.{ext}")
        with open(p, 'w', newline='\r\n') as f:
            f.write(f"# {prefix} terrain - IMG Factory Terrain Workshop\n{head}\n" + "\n".join(lines) + "\nend\n")
        written.append(p)
    if img_path:
        from apps.methods.img_core_classes import IMGFile
        arc = IMGFile(img_path)
        if not arc.open():
            raise IOError(f"can't open {img_path}")
        for fname, data in files.items():
            old = next((e for e in arc.entries if e.name.lower() == fname.lower()), None)
            if old is not None and hasattr(arc, 'remove_entry'):
                arc.remove_entry(old)
            if not arc.add_entry(fname, data):
                raise IOError(f"{fname}: {getattr(arc, 'last_error', '') or 'IMG refused the entry'}")
    return {'files': written, 'tiles': len(tiles), 'lods': len(lod_ide),
            'ids': (id_start, id_start + len(tiles) + len(lod_ide) - 1), 'assets': len(assets or [])}
