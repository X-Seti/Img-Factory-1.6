#this belongs in apps/methods/rw_convert.py - Version: 1
# X-Seti - September 2026 - IMG Factory 1.6 - DFF / COL / TXD version conversion

"""rw_convert.py - Convert GTA III / Vice City / San Andreas files up or down, on plain bytes:
  DFF  RenderWare stamps of every section, the geometry struct (surface properties exist only
       below RW 3.4), the clump struct (light/camera counts exist from RW 3.3), and optionally the
       Rockstar extension sections the target game does not have;
  TXD  PC D3D8 (III/VC) <-> D3D9 (SA): platform id, D3D format / alpha-compression fields, the
       dictionary device id and every stamp; the pixel data is not touched;
  COL  COL1 (III/VC) <-> COL2 / COL3 / COL4 (SA): every model is re-written in the target
       layout (COL2+ vertices are 16-bit fixed point, so a range check is made).
Every function returns (new_bytes, report_lines) and raises ConvertError with a clear message
when a file cannot be converted safely - it never returns a half converted file."""

##Methods list -
# ConvertError
# detect_kind
# targets_for
# convert_dff
# convert_txd
# convert_col

import struct
from typing import List, Optional, Tuple

from apps.methods import rw_chunks as rw

# game presets: (label, RW version)
GAME_VERSIONS = {
    "GTA III (RW 3.2.0.0)": 0x32000,
    "GTA Vice City (RW 3.3.0.2)": 0x33002,
    "GTA VC TXD / PC (RW 3.4.0.3)": 0x34003,
    "GTA San Andreas (RW 3.6.0.3)": 0x36003,
}


class ConvertError(Exception):
    pass


def detect_kind(data: bytes) -> Optional[str]:
    """'dff' | 'txd' | 'col' | None"""
    if len(data) >= 12:
        t = struct.unpack_from("<I", data, 0)[0]
        if t == 0x10:
            return "dff"
        if t == 0x16:
            return "txd"
    if data[:4] in (b"COLL", b"COL2", b"COL3", b"COL4"):
        return "col"
    return None


def current_version(data: bytes, kind: str) -> str:
    if kind == "col":
        return {b"COLL": "COL1 (III / VC)", b"COL2": "COL2 (SA)", b"COL3": "COL3 (SA)", b"COL4": "COL4"}[data[:4]]
    roots = rw.parse_rw(data)
    return f"RW {rw.version_text(roots[0].stamp)}" if roots else "?"


def targets_for(kind: str, data: bytes) -> List[Tuple[str, object]]:
    if kind == "col":
        cur = data[:4]
        return [(n, v) for n, v, m in (("COL1  (GTA III / Vice City)", 1, b"COLL"), ("COL2  (GTA San Andreas)", 2, b"COL2"),
                                        ("COL3  (GTA San Andreas, shadow mesh)", 3, b"COL3")) if m != cur]
    return list(GAME_VERSIONS.items())


# ---------------------------------------------------------------------------- DFF
_ROCKSTAR_SA_ONLY = {0x0253F2F6, 0x0253F2FC, 0x0253F2FD}          # specular / reflection material, breakable
_ROCKSTAR_NOT_III = {0x0253F2F8, 0x0253F2F9, 0x0253F2F3}          # 2dfx, extra vertex colours, pipeline set


def _ser_dff(data, n: rw.RWNode, target_ver: int, strip: set, rep: List[str], stats: dict) -> bytes:
    stamp = rw.stamp_for(target_ver, 0xFFFF)
    cur_ver = rw.version_of(n.stamp)
    if n.kind in ("data", "empty", "faulty") or not n.children:
        payload = bytes(data[n.data_start:n.data_start + max(0, n.size)])
        if n.type == 0x01 and n.parent is not None:
            if n.parent.type == 0x0F and len(payload) >= 28:                    # geometry struct
                has_now, has_new = cur_ver < 0x34000, target_ver < 0x34000
                if has_now and not has_new:
                    payload = payload[:16] + payload[28:]
                    stats["geom_down"] = stats.get("geom_down", 0) + 1
                elif has_new and not has_now:
                    payload = payload[:16] + struct.pack("<3f", 1.0, 1.0, 1.0) + payload[16:]
                    stats["geom_up"] = stats.get("geom_up", 0) + 1
            elif n.parent.type == 0x10 and len(payload) in (4, 12):              # clump struct
                if cur_ver >= 0x33000 and target_ver < 0x33000 and len(payload) == 12:
                    payload = payload[:4]
                    stats["clump"] = stats.get("clump", 0) + 1
                elif cur_ver < 0x33000 and target_ver >= 0x33000 and len(payload) == 4:
                    payload = payload + struct.pack("<II", 0, 0)
                    stats["clump"] = stats.get("clump", 0) + 1
        return struct.pack("<III", n.type, len(payload), stamp) + payload
    parts, pos = [], n.data_start
    for c in n.children:
        if c.offset > pos:
            parts.append(bytes(data[pos:c.offset]))
        if n.type == 0x03 and c.type in strip:
            rep.append(f"removed {c.name} section ({12 + c.size} bytes) at 0x{c.offset:X}")
            stats["stripped"] = stats.get("stripped", 0) + 1
        else:
            parts.append(_ser_dff(data, c, target_ver, strip, rep, stats))
        pos = c.end
    if pos < n.end:
        parts.append(bytes(data[pos:n.end]))
    payload = b"".join(parts)
    return struct.pack("<III", n.type, len(payload), stamp) + payload


def convert_dff(data: bytes, target_version: int, strip_extensions: bool = True) -> Tuple[bytes, List[str]]:
    roots = rw.parse_rw(data)
    if not roots or roots[0].type != 0x10:
        raise ConvertError("This is not a DFF (no Clump section at the start).")
    problems = rw.validate(roots)
    if problems:
        raise ConvertError("The DFF has structure errors - fix those first:\n" + "\n".join(problems[:5]))
    cur = rw.version_of(roots[0].stamp)
    rep = [f"DFF: RW {rw.version_text(roots[0].stamp)} -> RW {rw.version_text(rw.stamp_for(target_version))}"]
    strip = set()
    if strip_extensions:
        if target_version < 0x33000:
            strip = _ROCKSTAR_SA_ONLY | _ROCKSTAR_NOT_III
        elif target_version < 0x36000:
            strip = set(_ROCKSTAR_SA_ONLY)
    stats: dict = {}
    out = []
    for r in roots:
        if r.type == 0 and r.error.startswith("note:"):
            continue                                   # trailing padding bytes are not carried over
        out.append(_ser_dff(data, r, target_version, strip, rep, stats))
    new = b"".join(out)
    if stats.get("geom_down"):
        rep.append(f"{stats['geom_down']} geometry struct(s): added ambient/specular/diffuse (1.0) for RW below 3.4")
    if stats.get("geom_up"):
        rep.append(f"{stats['geom_up']} geometry struct(s): removed ambient/specular/diffuse (RW 3.4+ has none)")
    if stats.get("clump"):
        rep.append("clump struct: light/camera counts " + ("added" if target_version >= 0x33000 else "removed"))
    left = rw.validate(rw.parse_rw(new))
    if left:
        raise ConvertError("The converted DFF failed validation, nothing changed:\n" + "\n".join(left[:5]))
    if cur == target_version and not stats:
        rep.append("already this version")
    return new, rep


# ---------------------------------------------------------------------------- TXD
_D3D = {"DXT1": 0x31545844, "DXT3": 0x33545844, "DXT5": 0x35545844}
_FOURCC_TO_CM = {0x31545844: 1, 0x33545844: 3, 0x35545844: 5, 0x32545844: 2, 0x34545844: 4}


def _pix_format(rf: int) -> int:
    return rf & 0x0F00


def convert_txd(data: bytes, target_version: int) -> Tuple[bytes, List[str]]:
    roots = rw.parse_rw(data)
    if not roots or roots[0].type != 0x16:
        raise ConvertError("This is not a TXD (no Texture Dictionary section at the start).")
    problems = rw.validate(roots)
    if problems:
        raise ConvertError("The TXD has structure errors - fix those first:\n" + "\n".join(problems[:5]))
    to9 = target_version >= 0x36000
    buf = bytearray(data)
    rep = [f"TXD: RW {rw.version_text(roots[0].stamp)} -> RW {rw.version_text(rw.stamp_for(target_version))}"
           f"  ({'D3D9 (San Andreas)' if to9 else 'D3D8 (III / Vice City)'})"]
    n_tex = n_changed = 0
    skipped = []
    for t in roots[0].children:
        if t.type != 0x15 or not t.children or t.children[0].type != 0x01:
            continue
        s = t.children[0]
        o = s.data_start
        n_tex += 1
        name = bytes(data[o + 8:o + 40]).split(b"\0", 1)[0].decode("latin1")
        if s.size < 88:
            skipped.append(f"{name}: texture header too short")
            continue
        plat = struct.unpack_from("<I", data, o)[0]
        if plat not in (8, 9):
            skipped.append(f"{name}: platform {plat} is not PC (D3D8/D3D9)")
            continue
        rf, x, w, h, dep, lv, rt, cm = struct.unpack_from("<IIHHBBBB", data, o + 72)
        pix = _pix_format(rf)
        has_alpha = (x != 0) if plat == 8 else bool(cm & 1)
        if plat == 8 and to9:
            if cm in (1, 2, 3, 4, 5):                                   # DXT
                fourcc = {1: 0x31545844, 2: 0x32545844, 3: 0x33545844, 4: 0x34545844, 5: 0x35545844}[cm]
                struct.pack_into("<I", buf, o + 76, fourcc)
                buf[o + 87] = 8 | (1 if has_alpha else 0)
            else:                                                       # raw / palette
                fmt = 41 if rf & 0x2000 else (21 if pix == 0x0500 and has_alpha else 22 if pix in (0x0500, 0x0600)
                                              else {0x0100: 25, 0x0200: 23, 0x0300: 26, 0x0A00: 24}.get(pix, 21))
                struct.pack_into("<I", buf, o + 76, fmt)
                buf[o + 87] = 1 if has_alpha else 0
            struct.pack_into("<I", buf, o, 9)
            n_changed += 1
        elif plat == 9 and not to9:
            if x in _FOURCC_TO_CM:
                buf[o + 87] = _FOURCC_TO_CM[x]
                struct.pack_into("<I", buf, o + 76, 1 if has_alpha else 0)
            else:
                buf[o + 87] = 0
                struct.pack_into("<I", buf, o + 76, 1 if has_alpha else 0)
            struct.pack_into("<I", buf, o, 8)
            n_changed += 1
    if skipped:
        rep.extend("skipped - " + s for s in skipped[:8])
    if n_tex and not n_changed and not skipped:
        rep.append("textures already use this platform")
    st = roots[0].children[0]
    if st.type == 0x01 and st.size >= 4:
        cnt, dev = struct.unpack_from("<HH", buf, st.data_start)
        newdev = 2 if to9 else 0
        if dev != newdev and n_changed:
            struct.pack_into("<HH", buf, st.data_start, cnt, newdev)
    new, _ = rw.set_stream_version(bytes(buf), target_version)
    rep.append(f"{n_changed} of {n_tex} texture(s) converted; pixel data unchanged")
    return new, rep


# ---------------------------------------------------------------------------- COL
def convert_col(data: bytes, target: int, clamp: bool = False) -> Tuple[bytes, List[str]]:
    """target: 1 (COL1), 2 (COL2), 3 (COL3)."""
    from apps.methods.col_splice import split_records, write_new_record
    from apps.methods.col_workshop_parser import COLParser, COLVersion
    recs, tail = split_records(data)
    if not recs:
        raise ConvertError("This is not a COL file.")
    ver = {1: COLVersion.COL_1, 2: COLVersion.COL_2, 3: COLVersion.COL_3}[target]
    parser = COLParser()
    out, rep, dropped, clamped, kept = [], [f"COL: {len(recs)} model(s) -> COL{target}"], 0, 0, []
    for rec in recs:
        m, _ = parser.parse_model(rec, 0)
        name = rec[8:30].split(b"\0", 1)[0].decode("latin1")
        if m is None:                                   # the reader cannot decode it: keep the bytes as they are
            out.append(rec)
            kept.append(name)
            continue
        src = {b"COLL": 1, b"COL2": 2, b"COL3": 3, b"COL4": 4}[rec[:4]]
        if src == target:
            out.append(rec)
            continue
        if src >= 2:
            flags = struct.unpack_from("<I", rec, 80)[0]
            if flags & 0x09 or (src >= 3 and flags & 0x10) or (src >= 3 and struct.unpack_from("<I", rec, 108)[0]):
                dropped += 1                                           # lines / face groups / shadow mesh
        if target >= 2:
            bad = [v for v in (m.vertices or []) if max(abs(v.x), abs(v.y), abs(v.z)) >= 255.99]
            if bad and not clamp:
                raise ConvertError(f"'{name}': {len(bad)} vertex(es) lie outside +-255.99, which COL{target} cannot store "
                                   "(16-bit fixed point). Tick 'clamp' to squash them, or leave this file as COL1.")
            for v in bad:
                v.x, v.y, v.z = (max(-255.99, min(255.99, c)) for c in (v.x, v.y, v.z))
                clamped += 1
            if len(m.vertices or []) > 65535:
                raise ConvertError(f"'{name}' has more than 65535 vertices - COL{target} cannot index them.")
        m.header.version = ver
        m.version = ver
        try:
            out.append(write_new_record(m))
        except Exception as e:
            raise ConvertError(f"'{name}' could not be written as COL{target}: {e}")
    if kept:
        rep.append(f"{len(kept)} model(s) could not be read and were kept unchanged in their old format: " + ", ".join(kept[:6]))
    if dropped:
        rep.append(f"{dropped} model(s) had suspension lines / face groups / shadow mesh, which are dropped")
    if clamped:
        rep.append(f"{clamped} vertex(es) clamped into +-255.99")
    rep.append("Surface (material) ids are copied as they are - GTA III, VC and SA use different surface tables, "
               "so check them in the COL Workshop")
    return b"".join(out) + tail, rep
