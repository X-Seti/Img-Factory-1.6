#this belongs in apps/methods/col_mesh_ops.py - Version: 2
# X-Seti - Sept 30 2026 - IMG Factory 1.6 - COL Mesh Operations

"""
COL Mesh Operations - geometry edits on COLModel objects (no UI).
Face selections are sets of face indices; vertex selections sets of vertex indices.
"""

##Methods list -
# _all_points
# _cross
# _dot
# _ear_clip
# _mat
# _normal
# _sub
# _v
# _dominant_material
# _new_face
# _rot
# box_to_mesh
# clean_mesh
# compact_vertices
# decimate
# delete_faces
# detach_faces
# extract_faces
# faces_to_box
# faces_to_sphere
# fill_holes
# icosphere
# merge_coplanar
# merge_models
# recalc_bounds
# rotate
# scale
# selection_centre
# selection_vertices
# sphere_to_mesh
# translate
# weld_vertices

import copy
import math
from collections import Counter, defaultdict

from apps.methods.col_workshop_classes import (COLBox, COLFace, COLSphere, COLVertex,
                                               Vector3)


def selection_vertices(model, face_ids): #vers 1
    """Vertex indices used by the given faces."""
    out = set()
    faces = model.faces
    for i in face_ids:
        if 0 <= i < len(faces):
            f = faces[i]
            out.update((f.a, f.b, f.c))
    return out


def _all_points(model): #vers 1
    """Every editable point: vertices, sphere centres, box corners."""
    pts = [(v.x, v.y, v.z) for v in model.vertices]
    pts += [tuple(s.center) for s in model.spheres]
    for b in model.boxes:
        pts += [tuple(b.min), tuple(b.max)]
    return pts


def selection_centre(model, face_ids=None): #vers 1
    """Centre of the selected faces' vertices, else of the whole model."""
    if face_ids:
        vs = [model.vertices[i] for i in selection_vertices(model, face_ids)
              if i < len(model.vertices)]
        pts = [(v.x, v.y, v.z) for v in vs]
    else:
        pts = _all_points(model)
    if not pts:
        return (0.0, 0.0, 0.0)
    n = len(pts)
    return (sum(p[0] for p in pts) / n, sum(p[1] for p in pts) / n, sum(p[2] for p in pts) / n)


def translate(model, face_ids, dx, dy, dz): #vers 1
    """Move selected faces' vertices, or the whole model when nothing is selected."""
    if face_ids:
        for i in selection_vertices(model, face_ids):
            v = model.vertices[i]
            v.x += dx; v.y += dy; v.z += dz
        return
    for v in list(model.vertices) + list(model.shadow_vertices):
        v.x += dx; v.y += dy; v.z += dz
    for s in model.spheres:
        s.center.x += dx; s.center.y += dy; s.center.z += dz
    for b in model.boxes:
        for p in (b.min, b.max):
            p.x += dx; p.y += dy; p.z += dz


def _rot(x, y, z, axis, c, s, pivot):
    px, py, pz = pivot
    x, y, z = x - px, y - py, z - pz
    if axis == 'X':
        y, z = y * c - z * s, y * s + z * c
    elif axis == 'Y':
        x, z = x * c + z * s, -x * s + z * c
    else:
        x, y = x * c - y * s, x * s + y * c
    return x + px, y + py, z + pz


def rotate(model, face_ids, axis, degrees, pivot): #vers 1
    """Rotate selected faces (or whole model) about pivot on axis 'X'/'Y'/'Z'.
    Boxes stay axis-aligned: their rotated corners are re-boxed."""
    r = math.radians(degrees)
    c, s = math.cos(r), math.sin(r)
    ids = selection_vertices(model, face_ids) if face_ids else range(len(model.vertices))
    for i in ids:
        v = model.vertices[i]
        v.x, v.y, v.z = _rot(v.x, v.y, v.z, axis, c, s, pivot)
    if face_ids:
        return
    for v in model.shadow_vertices:
        v.x, v.y, v.z = _rot(v.x, v.y, v.z, axis, c, s, pivot)
    for sp in model.spheres:
        sp.center.x, sp.center.y, sp.center.z = _rot(*sp.center, axis, c, s, pivot)
    for b in model.boxes:
        corners = [_rot(x, y, z, axis, c, s, pivot)
                   for x in (b.min.x, b.max.x) for y in (b.min.y, b.max.y) for z in (b.min.z, b.max.z)]
        b.min = Vector3(*(min(p[k] for p in corners) for k in range(3)))
        b.max = Vector3(*(max(p[k] for p in corners) for k in range(3)))


def scale(model, face_ids, sx, sy, sz, pivot): #vers 1
    """Scale selected faces (or whole model) about pivot; sphere radius uses the mean factor."""
    px, py, pz = pivot

    def sc(p):
        p.x = px + (p.x - px) * sx
        p.y = py + (p.y - py) * sy
        p.z = pz + (p.z - pz) * sz

    ids = selection_vertices(model, face_ids) if face_ids else range(len(model.vertices))
    for i in ids:
        sc(model.vertices[i])
    if face_ids:
        return
    for v in model.shadow_vertices:
        sc(v)
    mean = (abs(sx) + abs(sy) + abs(sz)) / 3.0
    for sp in model.spheres:
        sc(sp.center)
        sp.radius = float(sp.radius) * mean
    for b in model.boxes:
        sc(b.min); sc(b.max)
        lo = Vector3(min(b.min.x, b.max.x), min(b.min.y, b.max.y), min(b.min.z, b.max.z))
        hi = Vector3(max(b.min.x, b.max.x), max(b.min.y, b.max.y), max(b.min.z, b.max.z))
        b.min, b.max = lo, hi


def recalc_bounds(model): #vers 1
    """Rebuild bounds min/max/centre/radius from vertices, spheres and boxes."""
    pts = [(v.x, v.y, v.z) for v in model.vertices]
    for s in model.spheres:
        r = float(s.radius)
        pts += [(s.center.x - r, s.center.y - r, s.center.z - r),
                (s.center.x + r, s.center.y + r, s.center.z + r)]
    for b in model.boxes:
        pts += [tuple(b.min), tuple(b.max)]
    bd = model.bounds
    if not pts:
        bd.min, bd.max, bd.center, bd.radius = Vector3(), Vector3(), Vector3(), 0.0
        return
    lo = [min(p[k] for p in pts) for k in range(3)]
    hi = [max(p[k] for p in pts) for k in range(3)]
    ctr = [(lo[k] + hi[k]) / 2 for k in range(3)]
    rad = max(math.dist(ctr, p) for p in pts)
    for s in model.spheres:
        rad = max(rad, math.dist(ctr, tuple(s.center)) + float(s.radius))
    bd.min, bd.max, bd.center, bd.radius = Vector3(*lo), Vector3(*hi), Vector3(*ctr), rad


def compact_vertices(model): #vers 1
    """Drop vertices no face uses and renumber faces."""
    used = sorted({i for f in model.faces for i in (f.a, f.b, f.c)})
    remap = {old: new for new, old in enumerate(used)}
    model.vertices = [model.vertices[i] for i in used]
    for f in model.faces:
        f.a, f.b, f.c = remap[f.a], remap[f.b], remap[f.c]


def delete_faces(model, face_ids): #vers 1
    """Remove faces and any vertices left unused."""
    keep = set(range(len(model.faces))) - set(face_ids)
    model.faces = [model.faces[i] for i in sorted(keep)]
    compact_vertices(model)


def detach_faces(model, face_ids): #vers 1
    """Give selected faces their own copies of vertices shared with other faces.
    Returns the number of vertices duplicated."""
    sel = set(face_ids)
    outside = selection_vertices(model, set(range(len(model.faces))) - sel)
    remap = {}
    for i in sorted(selection_vertices(model, sel) & outside):
        v = model.vertices[i]
        remap[i] = len(model.vertices)
        model.vertices.append(COLVertex(v.x, v.y, v.z))
    for fi in sel:
        f = model.faces[fi]
        f.a, f.b, f.c = remap.get(f.a, f.a), remap.get(f.b, f.b), remap.get(f.c, f.c)
    return len(remap)


def extract_faces(model, face_ids, name): #vers 1
    """New model holding copies of the selected faces (same version); source unchanged."""
    new = copy.deepcopy(model)
    new.name = name
    new.spheres, new.boxes = [], []
    new.shadow_vertices, new.shadow_faces = [], []
    new.lines_raw, new.lines_count = b'', 0
    for attr in ('_orig_record', '_orig_fp', '_orig_shadow_fp', '_orphans_after'):
        new.__dict__.pop(attr, None)
    new.faces = [new.faces[i] for i in sorted(set(face_ids))]
    compact_vertices(new)
    recalc_bounds(new)
    return new


def weld_vertices(model, vert_ids): #vers 1
    """Join the given vertices into one at their centre; faces that collapse are removed.
    Returns the number of faces removed."""
    ids = sorted(set(vert_ids))
    if len(ids) < 2:
        return 0
    keep = ids[0]
    vs = [model.vertices[i] for i in ids]
    model.vertices[keep] = COLVertex(sum(v.x for v in vs) / len(vs),
                                     sum(v.y for v in vs) / len(vs),
                                     sum(v.z for v in vs) / len(vs))
    gone = set(ids[1:])
    before = len(model.faces)
    for f in model.faces:
        f.a, f.b, f.c = (keep if i in gone else i for i in (f.a, f.b, f.c))
    model.faces = [f for f in model.faces if len({f.a, f.b, f.c}) == 3]
    compact_vertices(model)
    return before - len(model.faces)


def _new_face(a, b, c, like): #vers 1
    return COLFace(a, b, c, like.material, like.flag, like.brightness, like.light)


def _dominant_material(faces): #vers 1
    """Face whose material is most common (template for new faces/shapes)."""
    top = Counter(int(getattr(f.material, 'material_id', f.material)) for f in faces).most_common(1)[0][0]
    return next(f for f in faces if int(getattr(f.material, 'material_id', f.material)) == top)


def fill_holes(model, face_ids): #vers 1
    """Close open edge loops (holes) whose every vertex belongs to the selected
    faces (select the faces around the hole). Each loop is fanned from its centre.
    Returns faces added."""
    edge_count = Counter()
    edge_face = {}
    for fi, f in enumerate(model.faces):
        for a, b in ((f.a, f.b), (f.b, f.c), (f.c, f.a)):
            k = (min(a, b), max(a, b))
            edge_count[k] += 1
            edge_face[(a, b)] = fi
    # boundary half-edges keep winding: reverse direction to face the hole
    nxt = defaultdict(list)
    for (a, b), fi in edge_face.items():
        if edge_count[(min(a, b), max(a, b))] == 1:
            nxt[b].append((a, fi))
    near = selection_vertices(model, face_ids)
    added, used = 0, set()
    for start in list(nxt):
        if start in used or not nxt[start]:
            continue
        loop, cur, guard = [start], start, 0
        while guard < 10000:
            guard += 1
            opts = [(v, fi) for v, fi in nxt[cur] if v not in used]
            if not opts:
                break
            v, fi = opts[0]
            if v == start:
                break
            loop.append(v)
            cur = v
        if len(loop) < 3 or not set(loop) <= near:
            continue
        used.update(loop)
        like = model.faces[edge_face.get((loop[1], loop[0]), next(iter(face_ids)))]
        if len(loop) == 3:
            model.faces.append(_new_face(loop[0], loop[1], loop[2], like))
            added += 1
            continue
        cx = sum(model.vertices[i].x for i in loop) / len(loop)
        cy = sum(model.vertices[i].y for i in loop) / len(loop)
        cz = sum(model.vertices[i].z for i in loop) / len(loop)
        ci = len(model.vertices)
        model.vertices.append(COLVertex(cx, cy, cz))
        for k in range(len(loop)):
            model.faces.append(_new_face(loop[k], loop[(k + 1) % len(loop)], ci, like))
            added += 1
    return added


def box_to_mesh(model, box_index): #vers 1
    """Replace a box with 12 triangles (8 vertices) of its material."""
    b = model.boxes.pop(box_index)
    base = len(model.vertices)
    for x in (b.min.x, b.max.x):
        for y in (b.min.y, b.max.y):
            for z in (b.min.z, b.max.z):
                model.vertices.append(COLVertex(x, y, z))
    quads = [(0, 2, 3, 1), (4, 5, 7, 6), (0, 1, 5, 4), (2, 6, 7, 3), (0, 4, 6, 2), (1, 3, 7, 5)]
    for q in quads:
        a, b2, c, d = (base + k for k in q)
        model.faces.append(COLFace(a, b2, c, b.material, b.flag, b.brightness, b.light))
        model.faces.append(COLFace(a, c, d, b.material, b.flag, b.brightness, b.light))
    return 12


def icosphere(subdiv=1): #vers 1
    """Unit icosphere: (vertices, faces) lists; subdiv 1 = 42 verts, 80 faces."""
    t = (1 + 5 ** 0.5) / 2
    vs = [(-1, t, 0), (1, t, 0), (-1, -t, 0), (1, -t, 0), (0, -1, t), (0, 1, t),
          (0, -1, -t), (0, 1, -t), (t, 0, -1), (t, 0, 1), (-t, 0, -1), (-t, 0, 1)]
    vs = [tuple(c / math.sqrt(sum(k * k for k in v)) for c in v) for v in vs]
    fs = [(0, 11, 5), (0, 5, 1), (0, 1, 7), (0, 7, 10), (0, 10, 11), (1, 5, 9), (5, 11, 4),
          (11, 10, 2), (10, 7, 6), (7, 1, 8), (3, 9, 4), (3, 4, 2), (3, 2, 6), (3, 6, 8),
          (3, 8, 9), (4, 9, 5), (2, 4, 11), (6, 2, 10), (8, 6, 7), (9, 8, 1)]
    for _ in range(subdiv):
        cache, nf = {}, []

        def mid(i, j):
            k = (min(i, j), max(i, j))
            if k not in cache:
                m = [(vs[i][n] + vs[j][n]) / 2 for n in range(3)]
                L = math.sqrt(sum(c * c for c in m))
                vs.append(tuple(c / L for c in m))
                cache[k] = len(vs) - 1
            return cache[k]
        for a, b, c in fs:
            ab, bc, ca = mid(a, b), mid(b, c), mid(c, a)
            nf += [(a, ab, ca), (b, bc, ab), (c, ca, bc), (ab, bc, ca)]
        fs = nf
    return vs, fs


def sphere_to_mesh(model, sphere_index, subdiv=1): #vers 1
    """Replace a sphere with an icosphere mesh of its material."""
    s = model.spheres.pop(sphere_index)
    vs, fs = icosphere(subdiv)
    base = len(model.vertices)
    r = float(s.radius)
    for x, y, z in vs:
        model.vertices.append(COLVertex(s.center.x + x * r, s.center.y + y * r, s.center.z + z * r))
    for a, b, c in fs:
        model.faces.append(COLFace(base + a, base + b, base + c, s.material, s.flag, s.brightness, s.light))
    return len(fs)


def faces_to_box(model, face_ids): #vers 1
    """Replace selected faces with one box around them (most common material)."""
    ids = selection_vertices(model, face_ids)
    vs = [model.vertices[i] for i in ids]
    like = _dominant_material([model.faces[i] for i in face_ids])
    box = COLBox((min(v.x for v in vs), min(v.y for v in vs), min(v.z for v in vs)),
                 (max(v.x for v in vs), max(v.y for v in vs), max(v.z for v in vs)),
                 like.material, like.flag, like.brightness, like.light)
    model.boxes.append(box)
    delete_faces(model, face_ids)
    return box


def faces_to_sphere(model, face_ids): #vers 1
    """Replace selected faces with one sphere enclosing them (most common material)."""
    ids = selection_vertices(model, face_ids)
    vs = [(model.vertices[i].x, model.vertices[i].y, model.vertices[i].z) for i in ids]
    ctr = tuple(sum(v[k] for v in vs) / len(vs) for k in range(3))
    like = _dominant_material([model.faces[i] for i in face_ids])
    sph = COLSphere(max(math.dist(ctr, v) for v in vs), ctr,
                    like.material, like.flag, like.brightness, like.light)
    model.spheres.append(sph)
    delete_faces(model, face_ids)
    return sph


def merge_models(target, source): #vers 1
    """Append source's spheres, boxes and mesh into target."""
    base = len(target.vertices)
    target.spheres += copy.deepcopy(source.spheres)
    target.boxes += copy.deepcopy(source.boxes)
    target.vertices += copy.deepcopy(source.vertices)
    for f in copy.deepcopy(source.faces):
        f.a, f.b, f.c = f.a + base, f.b + base, f.c + base
        target.faces.append(f)
    recalc_bounds(target)



#    Optimise (Sep 30 2026)

def _mat(f):
    return int(getattr(f.material, 'material_id', f.material))


def _v(model, i):
    v = model.vertices[i]
    return (v.x, v.y, v.z)


def _sub(a, b): return (a[0] - b[0], a[1] - b[1], a[2] - b[2])


def _cross(a, b): return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0])


def _dot(a, b): return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def _normal(model, f): #vers 1
    """Unit normal and doubled area of a face."""
    n = _cross(_sub(_v(model, f.b), _v(model, f.a)), _sub(_v(model, f.c), _v(model, f.a)))
    L = math.sqrt(_dot(n, n))
    return ((n[0] / L, n[1] / L, n[2] / L) if L > 0 else (0.0, 0.0, 0.0)), L


def clean_mesh(model, tol=0.001): #vers 1
    """Lossless clean: weld vertices within tol, drop zero-area, repeated and
    duplicate faces, drop unused vertices. Returns (verts_removed, faces_removed)."""
    nv0, nf0 = len(model.vertices), len(model.faces)
    grid, remap, keep = {}, {}, []
    for i, v in enumerate(model.vertices):
        key = (round(v.x / tol), round(v.y / tol), round(v.z / tol))
        if key in grid:
            remap[i] = grid[key]
        else:
            grid[key] = remap[i] = len(keep)
            keep.append(v)
    model.vertices = keep
    seen, faces = set(), []
    for f in model.faces:
        f.a, f.b, f.c = remap[f.a], remap[f.b], remap[f.c]
        if len({f.a, f.b, f.c}) < 3 or _normal(model, f)[1] <= 1e-9:
            continue
        k = frozenset((f.a, f.b, f.c))
        if k in seen:
            continue
        seen.add(k)
        faces.append(f)
    model.faces = faces
    compact_vertices(model)
    return nv0 - len(model.vertices), nf0 - len(model.faces)


def _ear_clip(pts2): #vers 1
    """Triangulate a simple 2D polygon (counter-clockwise list); index triples or None."""
    idx = list(range(len(pts2)))
    tris, guard = [], 0

    def area2(a, b, c):
        return (pts2[b][0] - pts2[a][0]) * (pts2[c][1] - pts2[a][1]) - (pts2[b][1] - pts2[a][1]) * (pts2[c][0] - pts2[a][0])

    def inside(p, a, b, c):
        return area2(a, b, p) >= 0 and area2(b, c, p) >= 0 and area2(c, a, p) >= 0
    while len(idx) > 3 and guard < 10000:
        guard += 1
        for k in range(len(idx)):
            a, b, c = idx[k - 1], idx[k], idx[(k + 1) % len(idx)]
            if area2(a, b, c) <= 1e-12:
                continue
            if any(inside(p, a, b, c) for p in idx if p not in (a, b, c)
                   and pts2[p] not in (pts2[a], pts2[b], pts2[c])):
                continue
            tris.append((a, b, c))
            idx.pop(k)
            break
        else:
            return None
    if len(idx) == 3:
        tris.append(tuple(idx))
    return tris


def merge_coplanar(model, angle_deg=0.5): #vers 1
    """Merge neighbouring same-material faces lying in one plane and re-triangulate
    each flat area from its outline (shape unchanged). Returns faces removed."""
    faces = model.faces
    cos_tol = math.cos(math.radians(angle_deg))
    normals = [_normal(model, f)[0] for f in faces]
    edge_faces = defaultdict(list)
    for fi, f in enumerate(faces):
        for a, b in ((f.a, f.b), (f.b, f.c), (f.c, f.a)):
            edge_faces[(min(a, b), max(a, b))].append(fi)
    group = [-1] * len(faces)
    groups = []
    for start in range(len(faces)):
        if group[start] >= 0:
            continue
        g, stack = [], [start]
        group[start] = len(groups)
        n0, m0 = normals[start], _mat(faces[start])
        d0 = _dot(n0, _v(model, faces[start].a))
        while stack:
            fi = stack.pop(); g.append(fi); f = faces[fi]
            for a, b in ((f.a, f.b), (f.b, f.c), (f.c, f.a)):
                for nb in edge_faces[(min(a, b), max(a, b))]:
                    if group[nb] >= 0 or _mat(faces[nb]) != m0:
                        continue
                    if _dot(normals[nb], n0) < cos_tol:
                        continue
                    if any(abs(_dot(n0, _v(model, i)) - d0) > 0.01 for i in (faces[nb].a, faces[nb].b, faces[nb].c)):
                        continue
                    group[nb] = len(groups); stack.append(nb)
        groups.append(g)
    new_faces, removed = [], 0
    for g in groups:
        if len(g) < 2:
            new_faces += [faces[i] for i in g]
            continue
        # outline: directed edges used once inside the group
        cnt = Counter()
        directed = {}
        for fi in g:
            f = faces[fi]
            for a, b in ((f.a, f.b), (f.b, f.c), (f.c, f.a)):
                cnt[(min(a, b), max(a, b))] += 1
                directed[(a, b)] = fi
        nxt = {}
        simple = True
        for (a, b) in directed:
            if cnt[(min(a, b), max(a, b))] == 1:
                if a in nxt: simple = False
                nxt[a] = b
        if not simple or not nxt:
            new_faces += [faces[i] for i in g]; continue
        start = next(iter(nxt)); loop, cur = [start], nxt[start]
        while cur != start and len(loop) <= len(nxt):
            loop.append(cur); cur = nxt.get(cur)
            if cur is None: break
        if cur != start or len(loop) != len(nxt) or len(loop) - 2 >= len(g):
            new_faces += [faces[i] for i in g]; continue         # holes / no gain
        n0 = normals[g[0]]
        ax = (1, 0, 0) if abs(n0[0]) < 0.9 else (0, 1, 0)
        u = _cross(n0, ax); L = math.sqrt(_dot(u, u)); u = (u[0] / L, u[1] / L, u[2] / L)
        w = _cross(n0, u)
        pts2 = [(_dot(_v(model, i), u), _dot(_v(model, i), w)) for i in loop]
        tris = _ear_clip(pts2)
        if tris is None or len(tris) >= len(g):
            new_faces += [faces[i] for i in g]; continue
        like = faces[g[0]]
        for a, b, c in tris:
            nf = _new_face(loop[a], loop[b], loop[c], like)
            if _dot(_normal(model, nf)[0], n0) < 0:
                nf.b, nf.c = nf.c, nf.b
            new_faces.append(nf)
        removed += len(g) - len(tris)
    model.faces = new_faces
    compact_vertices(model)
    return removed


def decimate(model, ratio=0.5): #vers 1
    """Lossy: collapse the cheapest edges (quadric error) until faces <= ratio x original.
    Material borders and open edges stay fixed; collapses that flip faces are skipped.
    Returns faces removed."""
    import heapq
    faces = [[f.a, f.b, f.c] for f in model.faces]
    mats = [_mat(f) for f in model.faces]
    pos = [list(_v(model, i)) for i in range(len(model.vertices))]
    target = max(4, int(len(faces) * ratio))
    Q = [[0.0] * 10 for _ in pos]

    def plane(fi):
        a, b, c = (pos[i] for i in faces[fi])
        n = _cross(_sub(b, a), _sub(c, a)); L = math.sqrt(_dot(n, n)) or 1.0
        n = (n[0] / L, n[1] / L, n[2] / L)
        return n + (-_dot(n, a),)
    for fi in range(len(faces)):
        a, b, c, d = plane(fi)
        k = (a*a, a*b, a*c, a*d, b*b, b*c, b*d, c*c, c*d, d*d)
        for v in faces[fi]:
            Q[v] = [x + y for x, y in zip(Q[v], k)]

    def err(q, p):
        x, y, z = p
        return (q[0]*x*x + 2*q[1]*x*y + 2*q[2]*x*z + 2*q[3]*x + q[4]*y*y + 2*q[5]*y*z
                + 2*q[6]*y + q[7]*z*z + 2*q[8]*z + q[9])
    vfaces = defaultdict(set)
    ecount, emats = Counter(), defaultdict(set)
    for fi, f in enumerate(faces):
        for v in f: vfaces[v].add(fi)
        for a, b in ((f[0], f[1]), (f[1], f[2]), (f[2], f[0])):
            e = (min(a, b), max(a, b)); ecount[e] += 1; emats[e].add(mats[fi])
    locked = set()
    for e, n in ecount.items():
        if n != 2 or len(emats[e]) > 1:
            locked.update(e)
    alive = [True] * len(faces)
    live = len(faces)
    heap = []
    for (a, b) in ecount:
        if a in locked and b in locked: continue
        q = [x + y for x, y in zip(Q[a], Q[b])]
        p = pos[b] if a in locked else pos[a] if b in locked else [(pos[a][k] + pos[b][k]) / 2 for k in range(3)]
        heapq.heappush(heap, (err(q, p), a, b))
    merged = list(range(len(pos)))

    def root(v):
        while merged[v] != v:
            merged[v] = merged[merged[v]]; v = merged[v]
        return v
    while live > target and heap:
        _, a, b = heapq.heappop(heap)
        a, b = root(a), root(b)
        if a == b or (a in locked and b in locked): continue
        keep, drop = (b, a) if a not in locked and b in locked else (a, b)
        if keep in locked:
            newp = pos[keep][:]
        elif drop in locked:
            keep, drop = drop, keep; newp = pos[keep][:]
        else:
            newp = [(pos[a][k] + pos[b][k]) / 2 for k in range(3)]
        # reject collapses that flip a surviving face
        flip = False
        for fi in vfaces[drop] | vfaces[keep]:
            if not alive[fi]: continue
            f = [root(v) for v in faces[fi]]
            if keep in f and drop in f: continue
            old = [pos[v] for v in f]
            new = [newp if v in (keep, drop) else pos[v] for v in f]
            n1 = _cross(_sub(old[1], old[0]), _sub(old[2], old[0]))
            n2 = _cross(_sub(new[1], new[0]), _sub(new[2], new[0]))
            if _dot(n1, n2) <= 0:
                flip = True; break
        if flip: continue
        merged[drop] = keep
        pos[keep] = newp
        Q[keep] = [x + y for x, y in zip(Q[keep], Q[drop])]
        for fi in vfaces[drop]:
            if not alive[fi]: continue
            f = [root(v) for v in faces[fi]]
            if len(set(f)) < 3:
                alive[fi] = False; live -= 1
        vfaces[keep] |= vfaces[drop]
        for fi in vfaces[keep]:
            if not alive[fi]: continue
            for v in faces[fi]:
                v = root(v)
                if v != keep and not (v in locked and keep in locked):
                    q = [x + y for x, y in zip(Q[keep], Q[v])]
                    p = pos[v] if keep not in locked and v in locked else pos[keep] if keep in locked else [(pos[keep][k] + pos[v][k]) / 2 for k in range(3)]
                    heapq.heappush(heap, (err(q, p), keep, v))
    before = len(model.faces)
    out = []
    for fi, f in enumerate(model.faces):
        if not alive[fi]: continue
        f.a, f.b, f.c = (root(v) for v in faces[fi])
        if len({f.a, f.b, f.c}) == 3:
            out.append(f)
    for i, p in enumerate(pos):
        model.vertices[i].x, model.vertices[i].y, model.vertices[i].z = p
    model.faces = out
    compact_vertices(model)
    return before - len(model.faces)


__all__ = ['box_to_mesh', 'compact_vertices', 'delete_faces', 'detach_faces', 'extract_faces',
           'faces_to_box', 'faces_to_sphere', 'fill_holes', 'icosphere', 'merge_models',
           'recalc_bounds', 'rotate', 'scale', 'selection_centre', 'selection_vertices',
           'sphere_to_mesh', 'translate', 'weld_vertices',
           'clean_mesh', 'merge_coplanar', 'decimate']
