#this belongs in apps/methods/dff_mesh_ops.py - Version: 3
# X-Seti - October 12 2026 - IMG Factory 1.6 - DFF mesh edits via COL mesh ops

"""
DFF Geometry <-> COL-style mesh so col_mesh_ops edits work on models.
Normals, UV layers and vertex colours ride on each vertex; new vertices
take them from a same-position vertex or their face neighbours.
"""

##Methods list -
# _bounds
# _split_uv_group
# extract_geometry
# _face_normal
# apply_mesh_op
# geometry_to_mesh
# mesh_to_geometry
# optimum_sphere
# prelight_geometry
# uv_map
#
##class MeshVertex: -
#
##class MeshModel: -
# __init__

import math
from dataclasses import dataclass
from typing import Callable, Optional, Tuple

from apps.methods.col_workshop_classes import COLFace, COLVertex
from apps.methods.dff_classes import BoundingSphere, RGBA, TexCoord, Triangle, Vector3


@dataclass
class MeshVertex(COLVertex):
    """COL vertex carrying the DFF per-vertex attributes."""
    normal: Optional[Tuple[float, float, float]] = None
    uvs: Optional[Tuple[Tuple[float, float], ...]] = None
    colour: Optional[Tuple[int, int, int, int]] = None


class MeshModel:
    """Minimal COLModel stand-in: vertices + faces (no boxes/spheres)."""

    def __init__(self, name=''): #vers 1
        self.name = name
        self.vertices = []
        self.faces = []
        self.spheres = []
        self.boxes = []
        self.shadow_vertices = []
        self.shadow_faces = []


def geometry_to_mesh(geom, name='') -> MeshModel: #vers 1
    """DFF Geometry to MeshModel (face material = DFF material index)."""
    m = MeshModel(name)
    norms = geom.normals or []
    layers = geom.uv_layers or []
    cols = geom.colors or []
    for i, v in enumerate(geom.vertices):
        n = norms[i] if i < len(norms) else None
        uv = tuple((l[i].u, l[i].v) for l in layers if i < len(l)) if layers else None
        c = cols[i] if i < len(cols) else None
        m.vertices.append(MeshVertex(v.x, v.y, v.z,
                                     (n.x, n.y, n.z) if n is not None else None,
                                     uv, (c.r, c.g, c.b, c.a) if c is not None else None))
    for t in geom.triangles:
        m.faces.append(COLFace(t.v1, t.v2, t.v3, t.material_id, 0, 0, 0))
    return m


def _face_normal(m, f): #vers 1
    """Unit normal of a mesh face."""
    a, b, c = (m.vertices[i] for i in (f.a, f.b, f.c))
    ux, uy, uz = b.x - a.x, b.y - a.y, b.z - a.z
    vx, vy, vz = c.x - a.x, c.y - a.y, c.z - a.z
    n = (uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx)
    L = math.sqrt(n[0] ** 2 + n[1] ** 2 + n[2] ** 2) or 1.0
    return (n[0] / L, n[1] / L, n[2] / L)


def _bounds(verts) -> BoundingSphere: #vers 1
    """Bounding sphere around the vertex box centre."""
    if not verts:
        return BoundingSphere()
    xs, ys, zs = [v.x for v in verts], [v.y for v in verts], [v.z for v in verts]
    c = ((min(xs) + max(xs)) / 2, (min(ys) + max(ys)) / 2, (min(zs) + max(zs)) / 2)
    r = max(math.sqrt((v.x - c[0]) ** 2 + (v.y - c[1]) ** 2 + (v.z - c[2]) ** 2) for v in verts)
    return BoundingSphere(Vector3(*c), r)


def mesh_to_geometry(m: MeshModel, geom) -> None: #vers 1
    """Write MeshModel back into geom; attributes filled for new vertices."""
    had_n, had_c = bool(geom.normals), bool(geom.colors)
    n_layers = len(geom.uv_layers or [])
    by_pos = {}
    for v in m.vertices:
        if isinstance(v, MeshVertex):
            by_pos.setdefault((v.x, v.y, v.z), v)
    users = {}
    for f in m.faces:
        for i in (f.a, f.b, f.c):
            users.setdefault(i, []).append(f)
    out = []
    for i, v in enumerate(m.vertices):
        if isinstance(v, MeshVertex):
            out.append(v)
            continue
        src = by_pos.get((v.x, v.y, v.z))
        src = MeshVertex(v.x, v.y, v.z, src.normal, src.uvs, src.colour) if src else None
        if src is None:                         # average the face neighbours
            nb = [m.vertices[j] for f in users.get(i, []) for j in (f.a, f.b, f.c)
                  if j != i and isinstance(m.vertices[j], MeshVertex)]
            src = MeshVertex(v.x, v.y, v.z)
            if nb:
                if nb[0].uvs:
                    src.uvs = tuple(tuple(sum(n.uvs[k][a] for n in nb) / len(nb) for a in (0, 1))
                                    for k in range(len(nb[0].uvs)))
                if nb[0].colour:
                    src.colour = tuple(int(sum(n.colour[a] for n in nb) / len(nb)) for a in range(4))
        out.append(MeshVertex(v.x, v.y, v.z, src.normal, src.uvs, src.colour))
    for i, nv in enumerate(out):
        if nv.normal is None and users.get(i):
            fn = [_face_normal(m, f) for f in users[i]]
            s = [sum(n[a] for n in fn) for a in range(3)]
            L = math.sqrt(sum(c * c for c in s)) or 1.0
            nv.normal = (s[0] / L, s[1] / L, s[2] / L)
    geom.vertices = [Vector3(v.x, v.y, v.z) for v in out]
    if had_n:
        geom.normals = [Vector3(*(v.normal or (0.0, 0.0, 1.0))) for v in out]
    if n_layers:
        geom.uv_layers = [[TexCoord(*(v.uvs[k] if v.uvs and k < len(v.uvs) else (0.0, 0.0)))
                           for v in out] for k in range(n_layers)]
    if had_c:
        geom.colors = [RGBA(*(v.colour or (255, 255, 255, 255))) for v in out]
    geom.triangles = [Triangle(f.a, f.b, f.c, int(getattr(f.material, 'material_id', f.material)))
                      for f in m.faces]
    geom.bounding_sphere = _bounds(geom.vertices)
    geom.edited = True                          # rebuilt on save (counts may change)


def apply_mesh_op(geom, op: Callable, *args, **kwargs): #vers 1
    """Run a col_mesh_ops function on a DFF Geometry; moved vertices get new normals."""
    m = geometry_to_mesh(geom)
    before = {id(v): (v.x, v.y, v.z) for v in m.vertices}
    result = op(m, *args, **kwargs)
    for v in m.vertices:
        if isinstance(v, MeshVertex) and v.normal is not None and before.get(id(v)) != (v.x, v.y, v.z):
            v.normal = None                     # recomputed from faces below
    mesh_to_geometry(m, geom)
    return result


def extract_geometry(geom, face_ids):  #vers 1
    """New Geometry holding copies of the selected triangles; source unchanged."""
    import copy
    new = copy.deepcopy(geom)
    keep = [geom.triangles[i] for i in sorted(set(face_ids)) if i < len(geom.triangles)]
    used = sorted({i for t in keep for i in (t.v1, t.v2, t.v3)})
    remap = {old: n for n, old in enumerate(used)}

    def pick(arr):
        return [copy.deepcopy(arr[i]) for i in used] if len(arr) == len(geom.vertices) else []
    new.vertices = pick(geom.vertices)
    new.normals = pick(geom.normals or [])
    new.colors = pick(geom.colors or [])
    new.uv_layers = [pick(l) for l in (geom.uv_layers or [])]
    new.triangles = [Triangle(remap[t.v1], remap[t.v2], remap[t.v3], t.material_id) for t in keep]
    new.bounding_sphere = _bounds(new.vertices)
    new.edited = True
    return new


def _split_uv_group(m, group): #vers 1
    """Own vertex copies (all attributes) for group faces shared with other faces."""
    outside = set()
    for fi, f in enumerate(m.faces):
        if fi not in group:
            outside.update((f.a, f.b, f.c))
    remap = {}
    for fi in group:
        f = m.faces[fi]
        for i in (f.a, f.b, f.c):
            if i in outside and i not in remap:
                v = m.vertices[i]
                remap[i] = len(m.vertices)
                m.vertices.append(MeshVertex(v.x, v.y, v.z, getattr(v, 'normal', None),
                                             getattr(v, 'uvs', None), getattr(v, 'colour', None)))
        f.a, f.b, f.c = remap.get(f.a, f.a), remap.get(f.b, f.b), remap.get(f.c, f.c)


def uv_map(m, face_ids, mode='keep', su=1.0, sv=1.0, ou=0.0, ov=0.0, rot=0.0,
           fit=False, layer=0): #vers 1
    """UVs for faces (all if none): 'keep' | 'box' | 'planar_x/y/z', then fit, scale, rotate, offset."""
    faces = sorted(face_ids) if face_ids else list(range(len(m.faces)))
    groups = {}
    for fi in faces:
        if mode == 'box':
            n = _face_normal(m, m.faces[fi])
            ax = max(range(3), key=lambda k: abs(n[k]))
        else:
            ax = {'planar_x': 0, 'planar_y': 1, 'planar_z': 2}.get(mode)
        groups.setdefault(ax, set()).add(fi)
    if len(groups) > 1 or len(faces) < len(m.faces):
        for g in groups.values():
            _split_uv_group(m, g)
    uv = {}
    for ax, g in groups.items():
        for fi in g:
            f = m.faces[fi]
            for i in (f.a, f.b, f.c):
                v = m.vertices[i]
                if ax is None:
                    cur = getattr(v, 'uvs', None)
                    uv[i] = cur[layer] if cur and layer < len(cur) else (0.0, 0.0)
                else:
                    p = (v.x, v.y, v.z)
                    a, b = [k for k in range(3) if k != ax]
                    uv[i] = (p[a], -p[b])
    if not uv:
        return 0
    us, vs = [t[0] for t in uv.values()], [t[1] for t in uv.values()]
    if fit:
        du, dv = (max(us) - min(us)) or 1.0, (max(vs) - min(vs)) or 1.0
        u0, v0 = min(us), min(vs)
        uv = {i: ((u - u0) / du, (v - v0) / dv) for i, (u, v) in uv.items()}
        us, vs = [t[0] for t in uv.values()], [t[1] for t in uv.values()]
    cu, cv = (min(us) + max(us)) / 2, (min(vs) + max(vs)) / 2
    c, s_ = math.cos(math.radians(rot)), math.sin(math.radians(rot))
    for i, (u, v) in uv.items():
        x, y = (u - cu) * su, (v - cv) * sv
        x, y = x * c - y * s_, x * s_ + y * c
        new = (x + cu + ou, y + cv + ov)
        vert = m.vertices[i]
        cur = list(getattr(vert, 'uvs', None) or ())
        cur += [(0.0, 0.0)] * (layer + 1 - len(cur))
        cur[layer] = new
        if not isinstance(vert, MeshVertex):
            vert = m.vertices[i] = MeshVertex(vert.x, vert.y, vert.z)
        vert.uvs = tuple(cur)
    return len(uv)


def optimum_sphere(geom) -> None:  #vers 1
    """Tight bounding sphere around the vertices."""
    from apps.methods.col_mesh_ops import min_sphere
    pts = [(v.x, v.y, v.z) for v in geom.vertices]
    if len(pts) < 2:
        return
    c, r = min_sphere(pts)
    geom.bounding_sphere = BoundingSphere(Vector3(*c), r)


def prelight_geometry(geom, intensity=1.0, azimuth=45.0, altitude=45.0, ambient=0.3,
                      directional=True, tint=(255, 255, 255)) -> None:  #vers 1
    """Prelit vertex colours from a directional light (CE II style)."""
    az, al = math.radians(azimuth), math.radians(altitude)
    ld = (math.cos(al) * math.cos(az), math.cos(al) * math.sin(az), math.sin(al))
    norms = [(n.x, n.y, n.z) for n in geom.normals] if len(geom.normals or []) == len(geom.vertices) else None
    if norms is None:                           # average face normals
        m = geometry_to_mesh(geom)
        acc = [[0.0, 0.0, 0.0] for _ in geom.vertices]
        for f in m.faces:
            fn = _face_normal(m, f)
            for i in (f.a, f.b, f.c):
                for k in range(3):
                    acc[i][k] += fn[k]
        norms = [tuple(c / (math.sqrt(sum(x * x for x in a)) or 1.0) for c in a) for a in acc]
    alphas = [c.a for c in geom.colors] if len(geom.colors or []) == len(geom.vertices) else None
    cols = []
    for i, n in enumerate(norms):
        lit = max(0.0, n[0] * ld[0] + n[1] * ld[1] + n[2] * ld[2]) if directional else 1.0
        k = min(1.0, ambient + intensity * lit * (1.0 - ambient))
        cols.append(RGBA(*(int(round(t * k)) for t in tint), alphas[i] if alphas else 255))
    geom.colors = cols
    geom.flags |= 0x08                          # rpGEOMETRYPRELIT
    geom.edited = True
