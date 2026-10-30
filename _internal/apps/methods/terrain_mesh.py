#this belongs in apps/methods/terrain_mesh.py - Version: 5
# X-Seti - October 22 2026 - IMG Factory 1.6 - Terrain grid and tiles

"""
Terrain grid for Terrain Workshop: heights per grid point, a material per
cell, sliced into game-sized tiles with planar UVs scaled per material.
Saved as a .terrain.json project (heights / materials base64).
"""

##Methods list -
# cell_centres
# grid_normals
# load_terrain
# lod_terrain
# new_terrain
# save_terrain
# tile_mesh
# tile_ranges
#
##class Terrain: -
# __init__
# grid_points
# height_at
# size

import base64
import json
from typing import List, Tuple

import numpy as np


class Terrain: #vers 3
    """Height grid (ny+1, nx+1), cell materials and water levels (ny, nx) at a world origin."""

    def __init__(self, nx: int, ny: int, cell: float, origin=(0.0, 0.0, 0.0),
                 tile_cells: int = 32, name: str = "terrain"): #vers 3
        self.nx, self.ny, self.cell = int(nx), int(ny), float(cell)
        self.origin = tuple(float(v) for v in origin)
        self.tile_cells = int(tile_cells)
        self.name = name
        self.heights = np.zeros((self.ny + 1, self.nx + 1), dtype=np.float64)
        self.mats = np.zeros((self.ny, self.nx), dtype=np.uint8)
        self.water = np.full((self.ny, self.nx), np.nan, dtype=np.float32)   # world water Z per cell, nan = dry
        self.guide = {}                  # picture source: {'image': path} or {'radar': game, 'img': path}
        self.assets = []                 # placed game objects: {'id', 'name', 'pos', 'rot'} (IPL quaternion)

    def size(self) -> Tuple[float, float]: #vers 1
        return self.nx * self.cell, self.ny * self.cell

    def grid_points(self) -> np.ndarray: #vers 1
        """World (x, y, z) of every grid point, shape (ny+1, nx+1, 3)."""
        ox, oy, oz = self.origin
        xs = ox + np.arange(self.nx + 1) * self.cell
        ys = oy + np.arange(self.ny + 1) * self.cell
        gx, gy = np.meshgrid(xs, ys)
        return np.stack([gx, gy, self.heights + oz], axis=-1)

    def height_at(self, x: float, y: float) -> float: #vers 1
        """Bilinear ground height at world x, y (clamped to the grid)."""
        fx = np.clip((x - self.origin[0]) / self.cell, 0, self.nx)
        fy = np.clip((y - self.origin[1]) / self.cell, 0, self.ny)
        i, j = min(int(fx), self.nx - 1), min(int(fy), self.ny - 1)
        tx, ty = fx - i, fy - j
        h = self.heights
        top = h[j, i] * (1 - tx) + h[j, i + 1] * tx
        bot = h[j + 1, i] * (1 - tx) + h[j + 1, i + 1] * tx
        return float(top * (1 - ty) + bot * ty + self.origin[2])


def new_terrain(width: float, depth: float, cell: float = 4.0, origin=(0.0, 0.0, 0.0),
                tile_size: float = 128.0, name: str = "terrain") -> Terrain: #vers 1
    """Flat terrain of about width x depth metres."""
    nx, ny = max(1, round(width / cell)), max(1, round(depth / cell))
    return Terrain(nx, ny, cell, origin, max(1, round(tile_size / cell)), name)


def cell_centres(t: Terrain) -> np.ndarray: #vers 1
    """World (x, y) of every cell centre, shape (ny, nx, 2)."""
    ox, oy, _oz = t.origin
    xs = ox + (np.arange(t.nx) + 0.5) * t.cell
    ys = oy + (np.arange(t.ny) + 0.5) * t.cell
    gx, gy = np.meshgrid(xs, ys)
    return np.stack([gx, gy], axis=-1)


def tile_ranges(t: Terrain) -> List[Tuple[int, int, int, int]]: #vers 1
    """(tx, ty, i0, j0) cell starts of every tile."""
    out = []
    for j0 in range(0, t.ny, t.tile_cells):
        for i0 in range(0, t.nx, t.tile_cells):
            out.append((i0 // t.tile_cells, j0 // t.tile_cells, i0, j0))
    return out


def grid_normals(t: Terrain) -> np.ndarray: #vers 1
    """Smooth unit normal per grid point from the height field."""
    gx = np.gradient(t.heights, t.cell, axis=1)
    gy = np.gradient(t.heights, t.cell, axis=0)
    nrm = np.stack([-gx, -gy, np.ones_like(t.heights)], axis=-1)
    return nrm / np.linalg.norm(nrm, axis=-1, keepdims=True)


def tile_mesh(t: Terrain, i0: int, j0: int, uv_scale, pts=None, nrm=None) -> dict: #vers 1
    """One tile as a mesh in tile-local space (origin at the tile's corner):
    pos, vertices, normals, uvs, triangles (a, b, c, material), used material ids.
    Each cell has its own 4 corners so every material can tile at its own scale."""
    i1, j1 = min(i0 + t.tile_cells, t.nx), min(j0 + t.tile_cells, t.ny)
    pts = t.grid_points() if pts is None else pts
    nrm = grid_normals(t) if nrm is None else nrm
    base = pts[j0, i0].copy()
    base[2] = t.origin[2]
    verts, norms, uvs, tris = [], [], [], []
    for j in range(j0, j1):
        for i in range(i0, i1):
            m = int(t.mats[j, i])
            s = float(uv_scale(m)) or 1.0
            k = len(verts)
            for dj, di in ((0, 0), (0, 1), (1, 1), (1, 0)):
                p = pts[j + dj, i + di]
                verts.append(tuple((p - base).tolist()))
                norms.append(tuple(nrm[j + dj, i + di].tolist()))
                uvs.append((float(p[0] / s), float(-p[1] / s)))
            tris.append((k, k + 1, k + 2, m))
            tris.append((k, k + 2, k + 3, m))
    return {'pos': tuple(base.tolist()), 'vertices': verts, 'normals': norms, 'uvs': uvs,
            'triangles': tris, 'used': sorted({tr[3] for tr in tris}), 'cells': (i0, j0, i1, j1)}


def lod_terrain(t: Terrain, f: int) -> Terrain: #vers 1
    """Coarse copy (every f-th grid point, same tiles) for LOD models."""
    while f > 1 and t.tile_cells % f:
        f //= 2                                     # LOD tiles must line up with the tiles
    nx, ny = -(-t.nx // f), -(-t.ny // f)
    lod = Terrain(nx, ny, t.cell * f, t.origin, max(1, t.tile_cells // f), t.name)
    ii = np.minimum(np.arange(nx + 1) * f, t.nx)
    jj = np.minimum(np.arange(ny + 1) * f, t.ny)
    lod.heights = t.heights[np.ix_(jj, ii)].copy()
    ci = np.minimum(np.arange(nx) * f + f // 2, t.nx - 1)
    cj = np.minimum(np.arange(ny) * f + f // 2, t.ny - 1)
    lod.mats = t.mats[np.ix_(cj, ci)].copy()
    return lod


def save_terrain(t: Terrain, path: str, library: list) -> None: #vers 4
    """Project file: grid, heights, materials, water, library and placed assets."""
    data = {'name': t.name, 'nx': t.nx, 'ny': t.ny, 'cell': t.cell, 'origin': t.origin,
            'tile_cells': t.tile_cells, 'library': library, 'guide': t.guide, 'assets': t.assets,
            'heights': base64.b64encode(t.heights.astype('<f4').tobytes()).decode(),
            'mats': base64.b64encode(t.mats.tobytes()).decode(),
            'water': base64.b64encode(t.water.astype('<f4').tobytes()).decode()}
    with open(path, 'w') as f:
        json.dump(data, f, indent=1)


def load_terrain(path: str) -> Tuple[Terrain, list]: #vers 4
    """(Terrain, library) from a project file."""
    with open(path) as f:
        d = json.load(f)
    t = Terrain(d['nx'], d['ny'], d['cell'], d['origin'], d.get('tile_cells', 32), d.get('name', 'terrain'))
    t.heights = np.frombuffer(base64.b64decode(d['heights']), dtype='<f4').astype(np.float64).reshape(t.ny + 1, t.nx + 1)
    t.mats = np.frombuffer(base64.b64decode(d['mats']), dtype=np.uint8).reshape(t.ny, t.nx).copy()
    t.guide = d.get('guide', {})
    t.assets = d.get('assets', [])
    if 'water' in d:
        t.water = np.frombuffer(base64.b64decode(d['water']), dtype='<f4').reshape(t.ny, t.nx).copy()
    return t, d.get('library', [])
