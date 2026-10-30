#this belongs in apps/methods/model_convert.py - Version: 3
# X-Seti - October 13 2026 - IMG Factory 1.6 - Model load / save as / batch convert

"""
Load any supported model, save it as DFF (GTA III / VC / SA) or OBJ,
and batch convert a source folder into a destination folder.
"""

##Methods list -
# batch_convert
# _dff_ready
# _is_native
# load_model
# model_to_obj
# obj_to_model
# save_model
# target_ext

import math
import os
from typing import Callable, List, Optional, Tuple

from apps.methods.dff_classes import (Atomic, BoundingSphere, DFFModel, Frame, Geometry,
                                      Material, TexCoord, Triangle, Vector3)

# label -> (kind, RW version for DFF)
TARGETS = {
    "DFF - GTA San Andreas": ('dff', 0x36003),
    "DFF - GTA Vice City":   ('dff', 0x33002),
    "DFF - GTA III":         ('dff', 0x32000),
    "OBJ - Wavefront":       ('obj', None),
    "NIF - Bully (edits into the source .nif)": ('nif', None),
    "MDL - LCS / VCS (edits into the source .mdl)": ('mdl', None),
    "WDR - GTA IV (edits into the source .wdr)": ('wdr', None),
    "WFT - GTA IV (edits into the source .wft)": ('wft', None),
}
# kinds written back into the model's own source file
_IN_PLACE = {'nif': ('apps.methods.nif_model', 'write_nif_model'),
             'mdl': ('apps.methods.stories_mdl', 'write_stories_mdl'),
             'wdr': ('apps.methods.wdr_model', 'write_wdr_model'),
             'wft': ('apps.methods.wdr_model', 'write_wdr_model')}
CONVERT_TARGETS = [lab for lab, (kind, _v) in TARGETS.items() if kind not in _IN_PLACE]
SOURCE_EXTS = ('.dff', '.mdl', '.wdr', '.wft', '.nif', '.obj')


def target_ext(label: str) -> str: #vers 1
    return '.' + TARGETS[label][0]


def load_model(path: str) -> Optional[DFFModel]: #vers 1
    """Any supported model file (DFF, PS2/PSP/mobile DFF, MDL, WDR, WFT, NIF, OBJ)."""
    if path.lower().endswith('.obj'):
        return obj_to_model(path)
    from apps.methods.dff_parser import parse_model_any
    with open(path, 'rb') as f:
        data = f.read()
    return parse_model_any(data, path)


def save_model(model: DFFModel, path: str, label: str) -> List[str]: #vers 3
    """Write model as the target format; returns report lines."""
    kind, ver = TARGETS[label]
    if kind in _IN_PLACE:
        import importlib
        mod, fn = _IN_PLACE[kind]
        src = getattr(model, 'source_path', '') or ''
        if not src.lower().endswith('.' + kind) or not os.path.isfile(src):
            raise ValueError(f"{kind.upper()} output writes edits into the model's own .{kind} - open a .{kind} first")
        with open(src, 'rb') as f:
            data, report = getattr(importlib.import_module(mod), fn)(f.read(), model)
        with open(path, 'wb') as f:
            f.write(data)
        return report
    if kind == 'obj':
        v, f = model_to_obj(model, path)
        return [f"OBJ: {v} vertices, {f} triangles"]
    from apps.methods import rw_chunks as rw
    src = getattr(model, 'source_path', '') or ''
    report = []
    data = None
    if (src.lower().endswith('.dff') or getattr(model, 'raw', None)) and not _is_native(model):
        from apps.methods.dff_parser import DFFWriter
        try:
            data = DFFWriter.write(model)                 # keeps every plugin section
            report += DFFWriter.last_report
        except Exception as e:
            report.append(f"rebuilt from scratch ({e})")
            data = None
    if data is not None and rw.version_of(model.rw_version or 0) != ver:
        from apps.methods.rw_convert import convert_dff
        data, rep = convert_dff(data, ver)
        report += rep
    if data is None:
        from apps.methods.dff_patch import build_dff
        data = build_dff(_dff_ready(model), rw.stamp_for(ver))
        report.append("written as a new DFF (plugins of the source format not kept)")
    with open(path, 'wb') as f:
        f.write(data)
    return report


def _is_native(model: DFFModel) -> bool: #vers 1
    """True for console (PS2 / PSP / Xbox native) DFF geometry: rebuilt as PC data."""
    import struct
    from apps.methods import rw_chunks as rw
    raw = getattr(model, 'raw', None)
    if not raw:
        try:
            with open(model.source_path, 'rb') as f:
                raw = f.read()
        except OSError:
            return False
    for root in rw.parse_rw(raw):
        for n in root.walk():
            if n.type == 0x510:
                return True
            if n.type == 0x0F:
                st = next((c for c in n.children if c.type == 1), None)
                if st is not None and st.size >= 4 and raw[st.data_start + 3] & 1:
                    return True
    return False


def _dff_ready(model: DFFModel) -> DFFModel: #vers 1
    """Model with a frame per atomic and one atomic per geometry (DFF needs both)."""
    if not model.frames:
        model.frames = [Frame(name=os.path.splitext(os.path.basename(model.source_path or 'model'))[0][:23])]
    used = {a.geometry_index for a in model.atomics}
    for gi in range(len(model.geometries)):
        if gi not in used:
            model.atomics.append(Atomic(0, gi, 5))
    return model


def model_to_obj(model: DFFModel, out_path: str) -> Tuple[int, int]: #vers 1
    """Wavefront OBJ + MTL; one object per geometry, faces grouped by material."""
    mtl_path = os.path.splitext(out_path)[0] + ".mtl"
    obj = ["# IMG Factory 1.6", f"# Source: {getattr(model, 'source_path', '')}",
           f"mtllib {os.path.basename(mtl_path)}", ""]
    mtl = ["# IMG Factory 1.6", ""]
    vo = uo = 1
    for gi, geom in enumerate(model.geometries):
        atomic = next((a for a in model.atomics if a.geometry_index == gi), None)
        name = (model.get_frame_name(atomic.frame_index) if atomic else f"geometry_{gi}")
        name = name.replace(' ', '_').replace('/', '_')
        obj.append(f"o {name}")
        obj += [f"v {v.x:.6f} {v.y:.6f} {v.z:.6f}" for v in geom.vertices]
        has_n, has_uv = bool(geom.normals), bool(geom.uv_layers)
        if has_n:
            obj += [f"vn {n.x:.6f} {n.y:.6f} {n.z:.6f}" for n in geom.normals]
        if has_uv:
            obj += [f"vt {t.u:.6f} {1.0 - t.v:.6f}" for t in geom.uv_layers[0]]
        for mi, mat in enumerate(geom.materials):
            c = mat.color
            mtl += [f"newmtl {name}_mat{mi}", f"Kd {c.r / 255:.4f} {c.g / 255:.4f} {c.b / 255:.4f}", "d 1.0"]
            if mat.texture_name:
                mtl.append(f"map_Kd {mat.texture_name}.png")
            mtl.append("")
        by_mat = {}
        for t in geom.triangles:
            by_mat.setdefault(t.material_id, []).append(t)
        for mi, tris in sorted(by_mat.items()):
            obj.append(f"usemtl {name}_mat{mi}" if mi < len(geom.materials) else "usemtl default")
            for t in tris:
                ids = [i + vo for i in (t.v1, t.v2, t.v3)]
                uvs = [i + uo for i in (t.v1, t.v2, t.v3)]
                if has_uv and has_n:
                    obj.append("f " + " ".join(f"{a}/{b}/{a}" for a, b in zip(ids, uvs)))
                elif has_uv:
                    obj.append("f " + " ".join(f"{a}/{b}" for a, b in zip(ids, uvs)))
                elif has_n:
                    obj.append("f " + " ".join(f"{a}//{a}" for a in ids))
                else:
                    obj.append("f " + " ".join(str(a) for a in ids))
        vo += len(geom.vertices)
        if has_uv:
            uo += len(geom.uv_layers[0])
        obj.append("")
    with open(out_path, 'w') as f:
        f.write("\n".join(obj))
    with open(mtl_path, 'w') as f:
        f.write("\n".join(mtl))
    return (sum(len(g.vertices) for g in model.geometries),
            sum(len(g.triangles) for g in model.geometries))


def obj_to_model(path: str) -> Optional[DFFModel]: #vers 1
    """Wavefront OBJ to DFFModel: one geometry per 'o' / 'g', materials from usemtl."""
    pos, uvs, nrm = [], [], []
    groups = []                                   # [name, {key: idx}, verts, faces, mats]
    cur = None

    def _group(name):
        g = [name, {}, [], [], []]
        groups.append(g)
        return g
    with open(path, 'r', errors='replace') as fh:
        mat = None
        for line in fh:
            t = line.split()
            if not t:
                continue
            if t[0] == 'v' and len(t) >= 4:
                pos.append(tuple(float(x) for x in t[1:4]))
            elif t[0] == 'vt' and len(t) >= 3:
                uvs.append((float(t[1]), 1.0 - float(t[2])))
            elif t[0] == 'vn' and len(t) >= 4:
                nrm.append(tuple(float(x) for x in t[1:4]))
            elif t[0] in ('o', 'g') and len(t) > 1:
                cur = _group(t[1])
            elif t[0] == 'usemtl' and len(t) > 1:
                mat = t[1]
            elif t[0] == 'f' and len(t) >= 4:
                cur = cur or _group(os.path.splitext(os.path.basename(path))[0])
                if mat not in cur[4]:
                    cur[4].append(mat)
                idx = []
                for s in t[1:]:
                    parts = (s.split('/') + ['', ''])[:3]
                    key = tuple(int(p) if p else 0 for p in parts)
                    if key not in cur[1]:
                        cur[1][key] = len(cur[2])
                        cur[2].append(key)
                    idx.append(cur[1][key])
                for k in range(1, len(idx) - 1):          # fan polygons into triangles
                    cur[3].append((idx[0], idx[k], idx[k + 1], cur[4].index(mat)))
    model = DFFModel(source_path=path)
    model.frames.append(Frame(name=os.path.splitext(os.path.basename(path))[0][:23]))
    for name, _keys, verts, faces, mats in groups:
        if not faces:
            continue
        g = Geometry(flags=0x02 | 0x04 | 0x20 | 0x40, uv_layer_count=1)

        def _at(lst, i):
            return lst[i - 1 if i > 0 else len(lst) + i] if i else None
        g.vertices = [Vector3(*_at(pos, v)) for v, _t, _n in verts]
        g.uv_layers = [[TexCoord(*(_at(uvs, t) or (0.0, 0.0))) for _v, t, _n in verts]]
        if all(n for _v, _t, n in verts) and nrm:
            g.normals = [Vector3(*_at(nrm, n)) for _v, _t, n in verts]
            g.flags |= 0x10
        g.triangles = [Triangle(a, b, c, m) for a, b, c, m in faces]
        g.materials = [Material(texture_name=(m or '')) for m in mats] or [Material()]
        for m in g.materials:
            m.colour = m.color                       # alias the viewers read
        if g.vertices:
            xs, ys, zs = ([v.x for v in g.vertices], [v.y for v in g.vertices], [v.z for v in g.vertices])
            c = ((min(xs) + max(xs)) / 2, (min(ys) + max(ys)) / 2, (min(zs) + max(zs)) / 2)
            r = max(math.dist(c, (v.x, v.y, v.z)) for v in g.vertices)
            g.bounding_sphere = BoundingSphere(Vector3(*c), r)
        model.geometries.append(g)
        model.atomics.append(Atomic(0, len(model.geometries) - 1, 5))
    return model if model.geometries else None


def batch_convert(src_dir: str, dst_dir: str, src_exts, label: str, recursive: bool = True,
                  progress: Optional[Callable[[int, int, str], bool]] = None) -> List[Tuple[str, bool, str]]: #vers 1
    """Convert every source model under src_dir into dst_dir (same sub folders).
    progress(done, total, name) returning False cancels. Returns (file, ok, message)."""
    files = []
    for root, _dirs, names in os.walk(src_dir):
        files += [os.path.join(root, n) for n in sorted(names) if n.lower().endswith(tuple(src_exts))]
        if not recursive:
            break
    out = []
    ext = target_ext(label)
    for i, path in enumerate(files):
        if progress and progress(i, len(files), os.path.basename(path)) is False:
            break
        rel = os.path.relpath(path, src_dir)
        dst = os.path.join(dst_dir, os.path.splitext(rel)[0] + ext)
        try:
            model = load_model(path)
            if model is None:
                out.append((rel, False, "not a supported model"))
                continue
            os.makedirs(os.path.dirname(dst), exist_ok=True)
            rep = save_model(model, dst, label)
            out.append((rel, True, "; ".join(r for r in rep if r.startswith("not written") or "new DFF" in r)))
        except Exception as e:
            out.append((rel, False, str(e)))
    return out
