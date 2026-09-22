#this belongs in apps/methods/dff_patch.py - Version: 1
# X-Seti - September 21 2026 - IMG Factory 1.6 - DFF patch-in-place writer

"""dff_patch.py - Writes edits made in Model Workshop back into the DFF they
were loaded from. The original bytes are kept; only the fields that differ
from a fresh parse of those bytes are patched (materials: colour, flags,
lighting, texture name/mask/filter/wrap; frames: position, rotation, parent,
flags, name; geometry: vertices, normals, UVs, vertex colours with the same
counts). Section sizes of every parent are fixed after a length change.
Changes that need a new structure (different vertex/triangle/frame counts,
added or removed geometries) are NOT written - they are listed in the report
and the file is left with everything else patched, never corrupted. The
result is re-parsed and checked against the edited model before it is
returned."""

##Methods list -
# patch_dff
# patch_report_text
# _geometry_layout
# _Patcher

import struct
from typing import List, Tuple

from apps.methods import rw_chunks as rw

T_STRUCT, T_STRING, T_EXT, T_TEXTURE, T_MATERIAL, T_MATLIST = 1, 2, 3, 6, 7, 8
T_FRAMELIST, T_GEOMETRY, T_CLUMP, T_GEOMLIST, T_ATOMIC = 0x0E, 0x0F, 0x10, 0x1A, 0x14
FRAME_NAME_TYPES = (0x253F2FE,)


class DFFPatchError(Exception):
    pass


def _child(node, type_, nth=0):
    hits = [c for c in node.children if c.type == type_]
    return hits[nth] if len(hits) > nth else None


def _nodes(data):
    roots = [r for r in rw.parse_rw(data) if r.type != 0]
    if not roots or roots[0].type != T_CLUMP:
        raise DFFPatchError("the source file is not a DFF clump")
    return roots[0]


def _geometry_layout(data, gnode) -> dict: #vers 1
    """Byte offsets of the arrays inside one geometry (standard RW layout)."""
    st = _child(gnode, T_STRUCT)
    ver = rw.version_of(gnode.stamp)
    p = st.data_start
    flags, uvc, _u, tris, verts, morphs = struct.unpack_from("<HBBiii", data, p)
    p += 16
    if _u & 1:
        raise DFFPatchError("native (console) geometry")
    if ver < 0x34000:
        p += 12                                  # ambient / specular / diffuse
    lay = {"flags": flags, "verts": verts, "tris": tris, "morphs": morphs,
           "colors": None, "uvs": [], "bsphere": None, "pos": None, "nrm": None}
    if flags & 0x08:
        lay["colors"] = p
        p += 4 * verts
    layers = uvc if uvc else (1 if flags & (0x04 | 0x80) else 0)
    for _ in range(layers):
        lay["uvs"].append(p)
        p += 8 * verts
    p += 8 * tris
    if morphs >= 1:
        lay["bsphere"] = p
        p += 16
        has_pos, has_nrm = struct.unpack_from("<II", data, p)
        p += 8
        if has_pos:
            lay["pos"] = p
            p += 12 * verts
        if has_nrm:
            lay["nrm"] = p
    return lay


def moved_any(geom, base) -> bool:
    """True if any per-vertex array of `geom` differs from `base`."""
    def pts(a, b):
        return len(a) == len(b) and any((x.x, x.y, x.z) != (y.x, y.y, y.z) for x, y in zip(a, b))
    if pts(geom.vertices, base.vertices) or pts(geom.normals, base.normals):
        return True
    for n, o in zip(geom.uv_layers, base.uv_layers):
        if len(n) == len(o) and any((a.u, a.v) != (b.u, b.v) for a, b in zip(n, o)):
            return True
    return len(geom.colors) == len(base.colors) and any(
        (a.r, a.g, a.b, a.a) != (b.r, b.g, b.b, b.a) for a, b in zip(geom.colors, base.colors))


class _Patcher:
    def __init__(self, data: bytes):
        self.data = data
        self.root = _nodes(data)
        self.edits: List[Tuple[int, int, bytes, object]] = []   # start, end, bytes, container node
        self.report: List[str] = []
        self.changed = 0

    # - low level
    def put(self, offset: int, raw: bytes):
        self.edits.append((offset, offset + len(raw), raw, None))

    def splice(self, start: int, end: int, raw: bytes, container):
        self.edits.append((start, end, raw, container))

    def skip(self, why: str):
        self.report.append("not written: " + why)

    def apply(self) -> bytes:
        buf = bytearray(self.data)
        for start, end, raw, container in sorted(self.edits, key=lambda e: (e[0], e[1]), reverse=True):
            buf[start:end] = raw
            delta = len(raw) - (end - start)
            if delta and container is not None:
                n = container
                while n is not None:
                    size = struct.unpack_from("<I", buf, n.offset + 4)[0]
                    struct.pack_into("<I", buf, n.offset + 4, size + delta)
                    n = n.parent
        return bytes(buf)

    @staticmethod
    def string_chunk(text: str, stamp: int) -> bytes:
        raw = text.encode("latin-1", "replace") + b"\0"
        raw += b"\0" * (-len(raw) % 4)
        return struct.pack("<III", T_STRING, len(raw), stamp) + raw

    # - materials
    def patch_material(self, mnode, base, mat, where: str):
        st = _child(mnode, T_STRUCT)
        p = st.data_start
        flags, r, g, b, a, unused, textured = struct.unpack_from("<I4BII", self.data, p)
        if mat.flags != base.flags:
            self.put(p, struct.pack("<I", mat.flags & 0xFFFFFFFF)); self.changed += 1
        if (mat.color.r, mat.color.g, mat.color.b, mat.color.a) != (base.color.r, base.color.g, base.color.b, base.color.a):
            self.put(p + 4, bytes(int(max(0, min(255, c))) for c in (mat.color.r, mat.color.g, mat.color.b, mat.color.a)))
            self.changed += 1
        if (mat.ambient, mat.specular, mat.diffuse) != (base.ambient, base.specular, base.diffuse):
            if st.size >= 28:
                self.put(p + 16, struct.pack("<3f", mat.ambient, mat.specular, mat.diffuse)); self.changed += 1
            else:
                self.skip(f"{where}: lighting values (material struct too short)")
        tex = _child(mnode, T_TEXTURE)
        name_changed = mat.texture_name != base.texture_name
        mask_changed = mat.texture_mask != base.texture_mask
        tex_flags_changed = (mat.wrap_u, mat.wrap_v, mat.filter_mode) != (base.wrap_u, base.wrap_v, base.filter_mode)
        if not (name_changed or mask_changed or tex_flags_changed):
            return
        stamp = mnode.stamp
        if tex is None:
            if not mat.texture_name:
                return
            tflags = (mat.filter_mode & 0xFF) | ((mat.wrap_u & 0xF) << 8) | ((mat.wrap_v & 0xF) << 12)
            tstruct = struct.pack("<III", T_STRUCT, 4, stamp) + struct.pack("<HH", tflags, 0)
            body = (tstruct + self.string_chunk(mat.texture_name, stamp)
                    + self.string_chunk(mat.texture_mask or "", stamp)
                    + struct.pack("<III", T_EXT, 0, stamp))
            chunk = struct.pack("<III", T_TEXTURE, len(body), stamp) + body
            ext = _child(mnode, T_EXT)
            at = ext.offset if ext is not None else mnode.end
            self.splice(at, at, chunk, mnode)
            self.put(p + 12, struct.pack("<I", 1))            # textured flag
            self.changed += 1
            return
        if not mat.texture_name and name_changed:
            self.splice(tex.offset, tex.end, b"", mnode)      # texture removed
            self.put(p + 12, struct.pack("<I", 0))
            self.changed += 1
            return
        tstruct = _child(tex, T_STRUCT)
        strings = [c for c in tex.children if c.type == T_STRING]
        if tex_flags_changed and tstruct is not None and tstruct.size >= 2:
            tflags = (mat.filter_mode & 0xFF) | ((mat.wrap_u & 0xF) << 8) | ((mat.wrap_v & 0xF) << 12)
            self.put(tstruct.data_start, struct.pack("<H", tflags)); self.changed += 1
        for idx, (new, old, label) in enumerate(((mat.texture_name, base.texture_name, "name"),
                                                 (mat.texture_mask, base.texture_mask, "mask"))):
            if new == old:
                continue
            if idx >= len(strings):
                self.skip(f"{where}: texture {label} (section missing in the file)")
                continue
            sn = strings[idx]
            self.splice(sn.offset, sn.end, self.string_chunk(new, sn.stamp), tex)
            self.changed += 1

    # - frames
    def patch_frames(self, flist, base_frames, frames):
        st = _child(flist, T_STRUCT)
        count = struct.unpack_from("<I", self.data, st.data_start)[0]
        exts = [c for c in flist.children if c.type == T_EXT]
        for i, (bf, f) in enumerate(zip(base_frames, frames)):
            fo = st.data_start + 4 + i * 56
            if list(f.rotation) != list(bf.rotation):
                self.put(fo, struct.pack("<9f", *f.rotation)); self.changed += 1
            if (f.position.x, f.position.y, f.position.z) != (bf.position.x, bf.position.y, bf.position.z):
                self.put(fo + 36, struct.pack("<3f", f.position.x, f.position.y, f.position.z)); self.changed += 1
            if (f.parent_index, f.flags) != (bf.parent_index, bf.flags):
                self.put(fo + 48, struct.pack("<iI", f.parent_index, f.flags & 0xFFFFFFFF)); self.changed += 1
            if f.name != bf.name:
                node = None
                if i < len(exts):
                    node = next((c for c in exts[i].children if c.type in FRAME_NAME_TYPES), None)
                if node is None:
                    self.skip(f"frame {i}: name (the file has no frame-name section for it)")
                    continue
                raw = f.name.encode("latin-1", "replace")
                raw += b"\0" * (-len(raw) % 4 or (4 if not raw else 0))
                self.splice(node.offset, node.end, struct.pack("<III", node.type, len(raw), node.stamp) + raw, exts[i])
                self.changed += 1

    # - geometry
    def patch_geometry(self, gnode, base, geom, gi: int):
        where = f"geometry {gi}"
        if len(geom.vertices) != len(base.vertices):
            self.skip(f"{where}: vertex count changed ({len(base.vertices)} -> {len(geom.vertices)})")
            return
        tri_now = [(t.v1, t.v2, t.v3, t.material_id) for t in geom.triangles]
        tri_was = [(t.v1, t.v2, t.v3, t.material_id) for t in base.triangles]
        if tri_now != tri_was:
            self.skip(f"{where}: triangle list changed")
        if not (moved_any(geom, base)):
            return
        try:
            lay = _geometry_layout(self.data, gnode)
        except Exception as e:
            self.skip(f"{where}: {e}" if isinstance(e, DFFPatchError) else f"{where}: unreadable geometry layout")
            return

        def same(a, b):
            return all(x.x == y.x and x.y == y.y and x.z == y.z for x, y in zip(a, b))

        moved = not same(geom.vertices, base.vertices)
        normals_changed = len(geom.normals) == len(base.normals) and not same(geom.normals, base.normals)
        if moved or normals_changed:
            # The viewer's parser and the file layout must agree before writing back
            ok = lay["pos"] is not None and lay["verts"] == len(base.vertices)
            if ok:
                for k in range(0, len(base.vertices), max(1, len(base.vertices) // 40)):
                    fx = struct.unpack_from("<3f", self.data, lay["pos"] + 12 * k)
                    bv = base.vertices[k]
                    if (fx[0], fx[1], fx[2]) != (bv.x, bv.y, bv.z):
                        ok = False
                        break
            if not ok:
                self.skip(f"{where}: the viewer's vertex layout does not match the file, vertex edits skipped")
                return
        if moved:
            vs = geom.vertices
            self.put(lay["pos"], b"".join(struct.pack("<3f", v.x, v.y, v.z) for v in vs))
            if lay["bsphere"] is not None and vs:
                xs, ys, zs = [v.x for v in vs], [v.y for v in vs], [v.z for v in vs]
                cx, cy, cz = (min(xs) + max(xs)) / 2, (min(ys) + max(ys)) / 2, (min(zs) + max(zs)) / 2
                rad = max(((v.x - cx) ** 2 + (v.y - cy) ** 2 + (v.z - cz) ** 2) ** 0.5 for v in vs)
                self.put(lay["bsphere"], struct.pack("<4f", cx, cy, cz, rad))
            self.changed += 1
        if normals_changed and lay["nrm"] is not None:
            self.put(lay["nrm"], b"".join(struct.pack("<3f", v.x, v.y, v.z) for v in geom.normals))
            self.changed += 1
        for li, (new_l, old_l) in enumerate(zip(geom.uv_layers, base.uv_layers)):
            if li < len(lay["uvs"]) and len(new_l) == len(old_l) == lay["verts"] \
                    and any((a.u, a.v) != (b.u, b.v) for a, b in zip(new_l, old_l)):
                self.put(lay["uvs"][li], b"".join(struct.pack("<2f", t.u, t.v) for t in new_l))
                self.changed += 1
        if lay["colors"] is not None and len(geom.colors) == len(base.colors) == lay["verts"] \
                and any((a.r, a.g, a.b, a.a) != (b.r, b.g, b.b, b.a) for a, b in zip(geom.colors, base.colors)):
            self.put(lay["colors"], b"".join(bytes((c.r & 255, c.g & 255, c.b & 255, c.a & 255)) for c in geom.colors))
            self.changed += 1


def patch_dff(original: bytes, model) -> Tuple[bytes, List[str]]: #vers 1
    """Return (new bytes, report lines). Raises DFFPatchError if the source is
    not a DFF or the result fails its re-parse check."""
    from apps.methods.dff_parser import DFFParser

    base = DFFParser(original).parse()
    px = _Patcher(original)
    clump = px.root
    if (len(model.frames), len(model.geometries), len(model.atomics)) != \
            (len(base.frames), len(base.geometries), len(base.atomics)):
        px.skip("frames / geometries / atomics were added or removed "
                f"({len(base.frames)}/{len(base.geometries)}/{len(base.atomics)} -> "
                f"{len(model.frames)}/{len(model.geometries)}/{len(model.atomics)})")
        return original, px.report

    flist = _child(clump, T_FRAMELIST)
    if flist is not None:
        px.patch_frames(flist, base.frames, model.frames)
    for i, (a, b) in enumerate(zip(base.atomics, model.atomics)):
        if (a.frame_index, a.geometry_index, a.flags) != (b.frame_index, b.geometry_index, b.flags):
            px.skip(f"atomic {i}: link/flags edits are not written")

    glist = _child(clump, T_GEOMLIST)
    gnodes = [c for c in glist.children if c.type == T_GEOMETRY] if glist is not None else []
    if len(gnodes) != len(base.geometries):
        px.skip("geometry list could not be matched to the parsed model; geometry edits skipped")
        gnodes = []
    for gi, (gn, bg, g) in enumerate(zip(gnodes, base.geometries, model.geometries)):
        if len(g.materials) != len(bg.materials):
            px.skip(f"geometry {gi}: material count changed")
        else:
            ml = _child(gn, T_MATLIST)
            mnodes = [c for c in ml.children if c.type == T_MATERIAL] if ml is not None else []
            if len(mnodes) == len(bg.materials):
                for mi, (mn, bm, m) in enumerate(zip(mnodes, bg.materials, g.materials)):
                    px.patch_material(mn, bm, m, f"geometry {gi} material {mi}")
        px.patch_geometry(gn, bg, g, gi)

    out = px.apply()
    if px.changed == 0 and not any(e for e in px.edits):
        return original, px.report

    # verify: structure valid and the parsed result matches the edited model
    problems = rw.validate(rw.parse_rw(out))
    if problems:
        raise DFFPatchError("result failed the structure check: " + problems[0])
    after = DFFParser(out).parse()
    for gi, (ag, g) in enumerate(zip(after.geometries, model.geometries)):
        for mi, (am, m) in enumerate(zip(ag.materials, g.materials)):
            if (am.texture_name, am.texture_mask) != (m.texture_name, m.texture_mask) and \
                    not any("texture" in r and f"geometry {gi} material {mi}" in r for r in px.report):
                raise DFFPatchError(f"geometry {gi} material {mi}: texture did not round-trip")
            if (am.color.r, am.color.g, am.color.b) != (m.color.r, m.color.g, m.color.b):
                raise DFFPatchError(f"geometry {gi} material {mi}: colour did not round-trip")
    return out, px.report


def patch_report_text(report: List[str]) -> str: #vers 1
    return "\n".join(report)
