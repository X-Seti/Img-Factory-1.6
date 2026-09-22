#this belongs in apps/methods/col_splice.py - Version: 1
# X-Seti - September 21 2026 - IMG Factory 1.6 - COL save by splicing original records

"""col_splice.py - Save a COL file without re-encoding what was not edited.
COLWriter.write_file() re-serialises every model from the parsed data; on real
files that changes the size (data it does not model is dropped) and it raises
on COL1 files. So a plain save could shrink or destroy a collision file.
Here every model keeps its ORIGINAL record bytes; a model is only re-written
(with COLWriter) when its data really changed, and the save is refused - not
guessed - when a changed model cannot be written."""

##Methods list -
# split_records
# fingerprint
# tag_models
# patch_record
# write_new_record
# build_col_bytes

import struct
from typing import List, Optional

_MAGIC = (b"COLL", b"COL2", b"COL3", b"COL4")


def split_records(data):
    """(records, tail): every model record (4-byte magic, u32 size, size bytes) and
    whatever follows the last one."""
    out, pos, n = [], 0, len(data)
    while pos + 8 <= n and bytes(data[pos:pos + 4]) in _MAGIC:
        size = struct.unpack_from("<I", data, pos + 4)[0]
        end = pos + 8 + size
        if end > n:
            break
        out.append(bytes(data[pos:end]))
        pos = end
    return out, bytes(data[pos:])


def _rec_name(rec: bytes) -> str:
    return rec[8:30].split(b"\0", 1)[0].decode("latin1", "ignore").lower()


def _dump(o, depth=0):
    """Nested tuples of primitives describing an object's data (no private/callable attrs)."""
    if depth > 6:
        return None
    if o is None or isinstance(o, (bool, int, float, str, bytes)):
        return o
    if isinstance(o, (list, tuple, set)):
        return tuple(_dump(x, depth + 1) for x in o)
    if isinstance(o, dict):
        return tuple(sorted((str(k), _dump(v, depth + 1)) for k, v in o.items()))
    if hasattr(o, "value") and hasattr(o, "name") and type(o).__module__ != "builtins" and not hasattr(o, "__dict__"):
        return str(o)
    d = getattr(o, "__dict__", None)
    if d is not None:
        return tuple((k, _dump(v, depth + 1)) for k, v in sorted(d.items())
                     if not k.startswith("_") and not callable(v))
    return str(o)


def fingerprint(model) -> int:
    try:
        return hash(_dump(model))
    except Exception:
        return 0


def tag_models(models: list, raw) -> Optional[dict]:
    """After loading: pair every parsed model with its original record (matched in
    order by model name), remember a fingerprint of its data, and keep every record
    the loader did not turn into a model (they are written back untouched, in place).
    Returns {'head': [...], 'tail': bytes} to hand to build_col_bytes, or None when
    the models cannot be matched to the file (then do not save by splicing)."""
    recs, tail = split_records(raw) if raw else ([], b"")
    if not recs:
        return None
    cur, head, last_model = 0, [], None
    for m in models:
        nm = str(getattr(m, "name", "")).lower()
        j = next((k for k in range(cur, len(recs)) if _rec_name(recs[k]) == nm), None)
        if j is None:
            return None
        for k in range(cur, j):                    # records the loader skipped
            (head if last_model is None else last_model._orphans_after).append(recs[k])
        m._orig_record, m._orig_fp, m._orphans_after = recs[j], fingerprint(m), []
        last_model, cur = m, j + 1
    for k in range(cur, len(recs)):
        (head if last_model is None else last_model._orphans_after).append(recs[k])
    return {"head": head, "tail": tail}


def _ver(rec: bytes) -> int:
    return {b"COLL": 1, b"COL2": 2, b"COL3": 3, b"COL4": 4}[rec[:4]]


def _layout(rec: bytes):
    """Byte positions of the data blocks of a record, mirroring COLParser.parse_model:
    (bounds_off, sphere_off, box_off, vert_off, face_off, n_s, n_b, n_v, n_f)."""
    import struct as st
    v = _ver(rec)
    if v == 1:
        pos = 32 + 40
        n_s = st.unpack_from("<I", rec, pos)[0]
        s_off = pos + 4
        pos = s_off + 20 * n_s + 4                  # + the always-zero 'unknown' u32
        n_b = st.unpack_from("<I", rec, pos)[0]
        b_off = pos + 4
        pos = b_off + 28 * n_b
        n_v = st.unpack_from("<I", rec, pos)[0]
        v_off = pos + 4
        pos = v_off + 12 * n_v
        n_f = st.unpack_from("<I", rec, pos)[0]
        return 32, s_off, b_off, v_off, pos + 4, n_s, n_b, n_v, n_f
    n_s, n_b, n_f, _nl, _flags, s_o, b_o, _l_o, v_o, f_o, _t_o = st.unpack_from("<HHHBxIIIIIII", rec, 72)
    at = lambda off: off + 4                        # offsets are from the fourcc, past the u32 count
    n_v = 0
    faces = []
    if n_f and f_o:
        faces = [st.unpack_from("<HHH", rec, at(f_o) + 8 * i) for i in range(n_f)]
    if v_o and faces:
        n_v = max(max(f) for f in faces) + 1
    return (32, at(s_o) if n_s and s_o else 0, at(b_o) if n_b and b_o else 0,
            at(v_o) if n_v else 0, at(f_o) if n_f and f_o else 0, n_s, n_b, n_v, n_f)


def patch_record(rec: bytes, m) -> bytes:
    """The ORIGINAL record with the edited values of model m written back in place
    (bounds, sphere/box/vertex/face values). Everything the editor does not model -
    COL2/3 flags, suspension lines, face groups, planes, shadow mesh - stays exactly
    as it was. Raises ValueError when the number of spheres/boxes/vertices/faces
    changed (that needs a re-layout, which is not done here)."""
    import struct as st
    v = _ver(rec)
    b_off, s_off, x_off, v_off, f_off, n_s, n_b, n_v, n_f = _layout(rec)
    spheres, boxes = list(getattr(m, "spheres", None) or []), list(getattr(m, "boxes", None) or [])
    verts, faces = list(getattr(m, "vertices", None) or []), list(getattr(m, "faces", None) or [])
    if (len(spheres), len(boxes), len(verts), len(faces)) != (n_s, n_b, n_v, n_f):
        raise ValueError("the number of spheres / boxes / vertices / faces was changed")
    out = bytearray(rec)

    def put(fmt, off, *vals):
        if st.unpack_from(fmt, out, off) != tuple(vals):
            st.pack_into(fmt, out, off, *vals)

    def xyz(p):
        return (p.x, p.y, p.z) if hasattr(p, "x") else tuple(p)

    bd = m.bounds
    if v == 1:
        put("<f", b_off, float(bd.radius)); put("<3f", b_off + 4, *map(float, xyz(bd.center)))
        put("<3f", b_off + 16, *map(float, xyz(bd.min))); put("<3f", b_off + 28, *map(float, xyz(bd.max)))
    else:
        put("<3f", b_off, *map(float, xyz(bd.min))); put("<3f", b_off + 12, *map(float, xyz(bd.max)))
        put("<3f", b_off + 24, *map(float, xyz(bd.center))); put("<f", b_off + 36, float(bd.radius))
    for i, sp in enumerate(spheres):
        o = s_off + 20 * i
        put("<f", o, float(sp.radius)); put("<3f", o + 4, *map(float, xyz(sp.center)))
        put("<BB", o + 16, int(sp.material) & 255, int(sp.flag) & 255)
    for i, bx in enumerate(boxes):
        o = x_off + 28 * i
        put("<3f", o, *map(float, xyz(bx.min))); put("<3f", o + 12, *map(float, xyz(bx.max)))
        put("<BBBB", o + 24, int(bx.material) & 255, int(bx.flag) & 255, int(bx.brightness) & 255, int(bx.light) & 255)
    for i, vt in enumerate(verts):
        if v == 1:
            put("<3f", v_off + 12 * i, float(vt.x), float(vt.y), float(vt.z))
        else:
            q = [max(-32768, min(32767, int(round(c * 128.0)))) for c in (vt.x, vt.y, vt.z)]
            cur = st.unpack_from("<3h", out, v_off + 6 * i)
            if tuple(round(c / 128.0, 6) for c in cur) != tuple(round(c, 6) for c in (vt.x, vt.y, vt.z)):
                st.pack_into("<3h", out, v_off + 6 * i, *q)
    for i, fc in enumerate(faces):
        if v == 1:
            o = f_off + 16 * i
            put("<3I", o, int(fc.a), int(fc.b), int(fc.c)); put("<BB", o + 12, int(fc.material) & 255, int(fc.light) & 255)
        else:
            o = f_off + 8 * i
            put("<3H", o, int(fc.a), int(fc.b), int(fc.c)); put("<BB", o + 6, int(fc.material) & 255, int(fc.light) & 255)
    return bytes(out)


def write_new_record(m) -> bytes:
    """A valid record for a model that has no original (new / imported): COL1 in the
    sequential layout, COL2/3/4 with the offset table (spheres, boxes, vertices,
    faces only - no lines / face groups / shadow mesh)."""
    import struct as st
    ver = m.header.version.value if hasattr(m.header.version, "value") else int(m.header.version)
    magic = {1: b"COLL", 2: b"COL2", 3: b"COL3", 4: b"COL4"}[ver]
    xyz = lambda p: (p.x, p.y, p.z) if hasattr(p, "x") else tuple(p)
    spheres, boxes = list(m.spheres or []), list(m.boxes or [])
    verts, faces = list(m.vertices or []), list(m.faces or [])
    name = (m.header.name or "").encode("ascii", "ignore")[:22].ljust(22, b"\0")
    bd = m.bounds
    head = name + st.pack("<H", m.header.model_id & 0xFFFF)
    sph = b"".join(st.pack("<f3fBBxx", float(s.radius), *map(float, xyz(s.center)), int(s.material) & 255, int(s.flag) & 255)
                   for s in spheres)
    box = b"".join(st.pack("<6fBBBB", *map(float, xyz(b.min)), *map(float, xyz(b.max)), int(b.material) & 255,
                           int(b.flag) & 255, int(b.brightness) & 255, int(b.light) & 255) for b in boxes)
    if ver == 1:
        head += st.pack("<f3f3f3f", float(bd.radius), *map(float, xyz(bd.center)), *map(float, xyz(bd.min)), *map(float, xyz(bd.max)))
        body = (st.pack("<I", len(spheres)) + sph + st.pack("<I", 0) + st.pack("<I", len(boxes)) + box
                + st.pack("<I", len(verts)) + b"".join(st.pack("<3f", float(v.x), float(v.y), float(v.z)) for v in verts)
                + st.pack("<I", len(faces)) + b"".join(st.pack("<3IBBxx", int(f.a), int(f.b), int(f.c), int(f.material) & 255,
                                                               int(f.light) & 255) for f in faces))
    else:
        head += st.pack("<3f3f3ff", *map(float, xyz(bd.min)), *map(float, xyz(bd.max)), *map(float, xyz(bd.center)), float(bd.radius))
        vb = b"".join(st.pack("<3h", *[max(-32768, min(32767, int(round(c * 128.0)))) for c in (v.x, v.y, v.z)]) for v in verts)
        fb = b"".join(st.pack("<3HBB", int(f.a), int(f.b), int(f.c), int(f.material) & 255, int(f.light) & 255) for f in faces)
        tbl_len = 36 + (12 if ver >= 3 else 0) + (4 if ver >= 4 else 0)
        cursor = 32 + 40 + tbl_len                   # first data block, measured from the record start
        offs, blocks = {}, b""
        for key, data, cnt in (("s", sph, len(spheres)), ("b", box, len(boxes)),
                               ("v", vb, len(verts)), ("f", fb, len(faces))):
            if cnt:
                offs[key] = cursor                   # offset -> the u32 count; the data follows it
                blk = st.pack("<I", cnt) + data
                blocks += blk
                cursor += len(blk)
        flags = 0x02 if (spheres or boxes or faces) else 0     # bit 1 = "not empty"
        tbl = st.pack("<HHHBxIIIIIII", len(spheres), len(boxes), len(faces), 0, flags,
                      offs.get("s", 0), offs.get("b", 0), 0, offs.get("v", 0), offs.get("f", 0), 0)
        if ver >= 3:
            tbl += st.pack("<III", 0, 0, 0)
        if ver >= 4:
            tbl += st.pack("<I", 0)
        body = tbl + blocks
    payload = head + body
    return magic + st.pack("<I", len(payload)) + payload


def build_col_bytes(models: list, writer, info: Optional[dict] = None) -> bytes:
    """Concatenate: skipped records, then each model - untouched ones as their
    original record, changed / new ones through writer.write_model() - then the
    original tail. Raises ValueError (nothing written) when a changed model cannot
    be serialised."""
    info = info or {"head": [], "tail": b""}
    parts = list(info["head"])
    for i, m in enumerate(models):
        rec: Optional[bytes] = getattr(m, "_orig_record", None)
        name = getattr(m, "name", f"model {i}")
        if rec is not None and getattr(m, "_orig_fp", None) == fingerprint(m):
            parts.append(rec)
        elif rec is not None:                      # edited: patch the original record in place
            try:
                parts.append(patch_record(rec, m))
            except Exception as e:
                raise ValueError(f"'{name}' was edited but cannot be saved safely: {e}")
        else:                                      # new model: write a fresh record
            try:
                parts.append(write_new_record(m))
            except Exception as e:
                raise ValueError(f"new model '{name}' cannot be written: {e}")
        parts.extend(getattr(m, "_orphans_after", []))
    parts.append(info["tail"])
    return b"".join(parts)
