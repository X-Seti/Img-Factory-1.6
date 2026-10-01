#this belongs in apps/methods/col_exchange.py - Version: 2
# X-Seti - Oct 01 2026 - IMG Factory 1.6 - COL data exchange

"""
COL data exchange - collision scripts (CST v1 and CE II CST2), 3DS and .X
mesh import, embedding a COL model in a DFF clump, COL mesh and surfaces from DFF.

CST2 layout written here (sections "count, Name", one comma list per item):
  Vertex: x, y, z | Face: a, b, c, material, light
  Sphere: x, y, z, radius, material, flag, brightness, light
  Box: minx, miny, minz, maxx, maxy, maxz, material, flag, brightness, light
  ShadVert / ShadFace: as Vertex / Face.
"""

##Methods list -
# _cst1_parts
# _cst2_parts
# _fmt
# attach_col_to_dff
# dff_collision
# dff_triangles
# model_from_parts
# read_3ds_mesh
# read_cst
# read_x_mesh
# surfaces_from_triangles
# write_cst2

import re
import struct

from apps.methods.col_workshop_classes import (COLBounds, COLBox, COLFace, COLHeader, COLModel,
                                               COLSphere, COLVersion, COLVertex, Vector3)

COLLISION_CHUNK = 0x0253F2FA
_FOURCC = {COLVersion.COL_1: b'COLL', COLVersion.COL_2: b'COL2', COLVersion.COL_3: b'COL3'}


def _fmt(v): #vers 1
    """Float as CE II writes it (0.###)."""
    t = f"{float(v):.3f}".rstrip('0').rstrip('.')
    return '0' if t in ('-0', '') else t


def write_cst2(model) -> str: #vers 1
    """Collision script (CST2) text for one model."""
    out = ["# Exported with IMG Factory COL Workshop", "CST2"]

    def mat(it):  #vers 1
        return int(getattr(it, 'material_id', it.material))

    out.append(f"{len(model.vertices)}, Vertex")
    out += [", ".join(_fmt(c) for c in (v.x, v.y, v.z)) for v in model.vertices]
    out.append(f"{len(model.faces)}, Face")
    out += [f"{f.a}, {f.b}, {f.c}, {mat(f)}, {int(f.light or 0)}" for f in model.faces]
    out.append(f"{len(model.spheres)}, Sphere")
    out += [", ".join([_fmt(s.center.x), _fmt(s.center.y), _fmt(s.center.z), _fmt(s.radius)])
            + f", {mat(s)}, {int(s.flag or 0)}, {int(s.brightness or 0)}, {int(s.light or 0)}"
            for s in model.spheres]
    out.append(f"{len(model.boxes)}, Box")
    out += [", ".join(_fmt(c) for c in (b.min.x, b.min.y, b.min.z, b.max.x, b.max.y, b.max.z))
            + f", {mat(b)}, {int(b.flag or 0)}, {int(b.brightness or 0)}, {int(b.light or 0)}"
            for b in model.boxes]
    sv = getattr(model, 'shadow_vertices', None) or []
    sf = getattr(model, 'shadow_faces', None) or []
    if sf:
        out.append(f"{len(sv)}, ShadVert")
        out += [", ".join(_fmt(c) for c in (v.x, v.y, v.z)) for v in sv]
        out.append(f"{len(sf)}, ShadFace")
        out += [f"{f.a}, {f.b}, {f.c}, {mat(f)}, {int(f.light or 0)}" for f in sf]
    return "\n".join(out) + "\n"


def _cst2_parts(lines): #vers 1
    """Sections {name: [number lists]} from CST2 lines."""
    parts, name, left = {}, None, 0
    for ln in lines:
        ln = ln.split('#', 1)[0].strip()
        if not ln or ln == 'CST2':
            continue
        m = re.match(r'^(\d+)\s*,\s*([A-Za-z]+)$', ln)
        if m and left == 0:
            name, left = m.group(2), int(m.group(1))
            parts[name] = []
            continue
        if name and left:
            parts[name].append([float(x) for x in re.split(r'[,;\s]+', ln) if x])
            left -= 1
    return parts


def _cst1_parts(lines): #vers 1
    """Sections from Steve M's GTA3 CST v1 ('S n: r | x; y; z | [m]' lines)."""
    parts = {'Vertex': [], 'Face': [], 'Sphere': [], 'Box': []}
    nums = lambda t: [float(x) for x in re.findall(r'-?\d+(?:\.\d+)?(?:e-?\d+)?', t)]
    for ln in lines:
        ln = ln.strip()
        if not ln or ln[0] not in 'SBVF' or ':' not in ln:
            continue
        kind, rest = ln[0], nums(ln.split(':', 1)[1])
        if kind == 'S' and len(rest) >= 5:          # r | x y z | [m]
            parts['Sphere'].append([rest[1], rest[2], rest[3], rest[0], rest[4]])
        elif kind == 'B' and len(rest) >= 7:
            parts['Box'].append(rest[:7])
        elif kind == 'V' and len(rest) >= 3:
            parts['Vertex'].append(rest[:3])
        elif kind == 'F' and len(rest) >= 4:
            parts['Face'].append(rest[:4])
    return parts


def read_cst(text):  #vers 1
    """Parts dict from CST2 or CST v1 text."""
    lines = text.splitlines()
    if any(ln.strip() == 'CST2' for ln in lines[:5]):
        return _cst2_parts(lines)
    return _cst1_parts(lines)


def model_from_parts(parts, name, version=COLVersion.COL_3): #vers 1
    """COLModel from parts (Vertex, Face, Sphere, Box, ShadVert, ShadFace)."""
    from apps.methods.col_mesh_ops import recalc_bounds
    g = lambda row, i: int(row[i]) if len(row) > i else 0

    def faces(rows):  #vers 1
        return [COLFace(int(r[0]), int(r[1]), int(r[2]), g(r, 3), 0, 0, g(r, 4)) for r in rows]

    hdr = COLHeader(fourcc=_FOURCC[version], size=0, name=name[:22], model_id=0, version=version)
    m = COLModel(header=hdr, bounds=COLBounds(),
                 spheres=[COLSphere(r[3], (r[0], r[1], r[2]), g(r, 4), g(r, 5), g(r, 6), g(r, 7))
                          for r in parts.get('Sphere', [])],
                 boxes=[COLBox((r[0], r[1], r[2]), (r[3], r[4], r[5]), g(r, 6), g(r, 7), g(r, 8), g(r, 9))
                        for r in parts.get('Box', [])],
                 vertices=[COLVertex(*r[:3]) for r in parts.get('Vertex', [])],
                 faces=faces(parts.get('Face', [])))
    if version.value >= 3:
        m.shadow_vertices = [COLVertex(*r[:3]) for r in parts.get('ShadVert', [])]
        m.shadow_faces = faces(parts.get('ShadFace', []))
    nv = len(m.vertices)
    m.faces = [f for f in m.faces if max(f.a, f.b, f.c) < nv]
    recalc_bounds(m)
    return m


def read_3ds_mesh(data): #vers 1
    """(vertices, faces) merged from every trimesh in a 3DS file."""
    verts, faces, base = [], [], [0]

    def walk(pos, end):  #vers 1
        while pos + 6 <= end:
            cid, size = struct.unpack_from('<HI', data, pos)
            if size < 6 or pos + size > end:
                return
            body = pos + 6
            if cid in (0x4D4D, 0x3D3D, 0x4100):
                walk(body, pos + size)
            elif cid == 0x4000:                        # object: name then sub-chunks
                z = data.index(b'\x00', body)
                walk(z + 1, pos + size)
            elif cid == 0x4110:
                n = struct.unpack_from('<H', data, body)[0]
                base[0] = len(verts)
                verts.extend(struct.unpack_from('<3f', data, body + 2 + 12 * i) for i in range(n))
            elif cid == 0x4120:
                n = struct.unpack_from('<H', data, body)[0]
                b = base[0]
                for i in range(n):
                    a, c, d, _ = struct.unpack_from('<4H', data, body + 2 + 8 * i)
                    faces.append((a + b, c + b, d + b))
            pos += size

    walk(0, len(data))
    return verts, faces


def read_x_mesh(text): #vers 1
    """(vertices, faces) from the first Mesh block of a text .X file; quads split."""
    m = re.search(r'Mesh\s*\w*\s*\{', text)
    if not m:
        return [], []
    nums = re.findall(r'-?\d+(?:\.\d+)?(?:[eE]-?\d+)?', text[m.end():])
    it = iter(nums)
    nv = int(next(it))
    verts = [(float(next(it)), float(next(it)), float(next(it))) for _ in range(nv)]
    nf = int(next(it))
    faces = []
    for _ in range(nf):
        k = int(next(it))
        idx = [int(next(it)) for _ in range(k)]
        faces += [(idx[0], idx[j], idx[j + 1]) for j in range(1, k - 1)]
    return verts, faces


def dff_collision(dff): #vers 1
    """Embedded COL bytes of a DFF clump, or None."""
    from apps.methods.rw_chunks import parse_rw
    for root in parse_rw(dff):
        for n in root.walk():
            if n.type == COLLISION_CHUNK:
                return bytes(dff[n.data_start:n.end])
    return None


def attach_col_to_dff(dff, col_bytes): #vers 1
    """DFF bytes with col_bytes as the clump's Collision Model extension (replaced if present)."""
    from apps.methods.rw_chunks import insert_section, make_section, parse_rw, replace_payload
    roots = parse_rw(dff)
    clump = next((r for r in roots if r.type == 0x10), None)
    if clump is None:
        raise ValueError("No clump in DFF")
    ext = next((c for c in reversed(clump.children) if c.type == 0x03), None)
    if ext is None:
        raise ValueError("Clump has no extension section")
    old = next((c for c in ext.children if c.type == COLLISION_CHUNK), None)
    if old is not None:
        return replace_payload(dff, old, col_bytes)
    chunk = make_section(COLLISION_CHUNK, clump.stamp, col_bytes)
    if ext.children:
        return insert_section(dff, ext.children[-1], chunk)
    return insert_section(dff, None, chunk, parent=ext)


def dff_triangles(dff_model, skip_lod=True): #vers 1
    """World-space (vertices, [(a, b, c, texture)]) from every atomic of a DFF model.
    skip_lod drops frames named *_vlo, *_dam and *_lod."""
    frames = dff_model.frames

    def apply(fi, p):  #vers 1
        while 0 <= fi < len(frames):
            f = frames[fi]
            r = f.rotation
            p = (p[0] * r[0] + p[1] * r[3] + p[2] * r[6] + f.position.x,
                 p[0] * r[1] + p[1] * r[4] + p[2] * r[7] + f.position.y,
                 p[0] * r[2] + p[1] * r[5] + p[2] * r[8] + f.position.z)
            fi = f.parent_index
        return p

    verts, tris = [], []
    pairs = ([(a.frame_index, a.geometry_index) for a in dff_model.atomics]
             or [(-1, i) for i in range(len(dff_model.geometries))])
    for fi, gi in pairs:
        fname = frames[fi].name.lower() if 0 <= fi < len(frames) else ''
        if skip_lod and (fname.endswith(('_vlo', '_lod')) or '_dam' in fname):
            continue
        if not 0 <= gi < len(dff_model.geometries):
            continue
        g = dff_model.geometries[gi]
        base = len(verts)
        verts += [apply(fi, (v.x, v.y, v.z)) for v in g.vertices]
        for t in g.triangles:
            tex = g.materials[t.material_id].texture_name if 0 <= t.material_id < len(g.materials) else ''
            tris.append((t.v1 + base, t.v2 + base, t.v3 + base, tex))
    return verts, tris


def surfaces_from_triangles(model, verts, tris, game): #vers 1
    """Set each COL face's surface from the texture of the nearest DFF triangle; returns faces set."""
    from apps.methods.col_materials import material_from_texture
    if not tris or not model.faces:
        return 0
    cen = lambda pts: tuple(sum(q[k] for q in pts) / 3 for k in range(3))
    dcen = [cen([verts[t[0]], verts[t[1]], verts[t[2]]]) for t in tris]
    mats = [material_from_texture(t[3], game) for t in tris]
    cell = max(1.0, (model.bounds.radius or 10.0) / 20.0)
    grid = {}
    for i, c in enumerate(dcen):
        if mats[i] is not None:
            grid.setdefault(tuple(int(c[k] // cell) for k in range(3)), []).append(i)
    if not grid:
        return 0
    n = 0
    for f in model.faces:
        c = cen([(model.vertices[i].x, model.vertices[i].y, model.vertices[i].z) for i in (f.a, f.b, f.c)])
        key = tuple(int(c[k] // cell) for k in range(3))
        best, ring = None, 0
        while best is None and ring <= 4:
            cand = [i for dx in range(-ring, ring + 1) for dy in range(-ring, ring + 1)
                    for dz in range(-ring, ring + 1)
                    for i in grid.get((key[0] + dx, key[1] + dy, key[2] + dz), ())]
            if cand:
                best = min(cand, key=lambda i: sum((dcen[i][k] - c[k]) ** 2 for k in range(3)))
            ring += 1
        if best is not None:
            f.material = mats[best]
            n += 1
    return n
