#this belongs in apps/methods/stories_mdl.py - Version: 4
# X-Seti - October 08 2026 - IMG Factory 1.6 - LCS/VCS .mdl models

"""
LCS/VCS PS2/PSP .mdl: relocatable 'ldm' chunk of RslElement(s) whose
geometry holds sPs2Geometry (VIF) or sPspGeometry (packed verts).
Layouts per librwgta storiesconv (aap).
"""

##Methods list -
# _cstr
# _element_geometry
# _elements
# _fit_axis
# _insert
# _is_psp_geometry
# _material_slots
# _node_matrix
# _pack_555
# _ped_group
# _ps2_block
# _psp_block
# _psp_geometry
# _strips
# _vertex_normals
# _weld
# _write_geometry
# is_stories_mdl
# parse_stories_mdl
# write_stories_mdl

import math
import struct
from typing import List, Optional, Tuple

import numpy as np

from apps.methods.dff_classes import (BoundingSphere, DFFModel, Geometry, Material,
                                      RGBA, TexCoord, Triangle, Vector3)
from apps.methods.lcs_geometry import (_GEOM_FLAGS, _HEADER as _PS2_HEADER, _MESH as _PS2_MESH, _UNPACK,
                                      _geometry as _ps2_geometry, _strip_triangles, _vif_commands)

MDL_IDENT = b'ldm\x00'
_HEADER = 32
_ELEMENT, _ELEMENTGROUP, _GEOM = 1, 2, 8
_PSP_FLAGS = {0x120, 0x121, 0x115, 0x114, 0xA1, 0x1C321}


def is_stories_mdl(data: bytes) -> bool: #vers 1
    """True for an LCS/VCS relocatable model chunk."""
    return len(data) > _HEADER and data[:4] == MDL_IDENT


def _cstr(d: bytes, p: int) -> str:
    return d[p:p + 32].split(b'\0', 1)[0].decode('latin-1') if 0 < p < len(d) else ''


def _elements(d: bytes) -> List[int]: #vers 2
    """RslElement offsets: simple model array, or element group atomics (peds: 2nd pointer)."""
    u = lambda o: struct.unpack_from('<I', d, o)[0]
    n_funcs = struct.unpack_from('<H', d, 30)[0]
    first = u(_HEADER)
    if not (0 < first < len(d)):
        return []
    if d[first] == _ELEMENT and n_funcs:
        els = [u(_HEADER + i * 4) for i in range(n_funcs)]
        return [e for e in els if 0 < e < len(d) and d[e] == _ELEMENT][:1]   # L0 only
    group = _HEADER if d[_HEADER] == _ELEMENTGROUP else (first if d[first] == _ELEMENTGROUP else 0)
    group = group or _ped_group(d)
    if not group:
        return []
    out, link, seen = [], u(group + 8), set()
    while 0 < link < len(d) and link != group + 8 and link not in seen:
        seen.add(link)
        e = link - 28                                   # inElementGroupLink
        if d[e] == _ELEMENT:
            out.append(e)
        link = u(link)
    return out


def _ped_group(d: bytes) -> int: #vers 1
    """Ped .mdl element group (header: colmodel, group pointers), or 0."""
    first, second = struct.unpack_from('<2I', d, _HEADER)
    if 0 < first < len(d) and d[first] not in (_ELEMENT, _ELEMENTGROUP) and \
            0 < second < len(d) - 12 and d[second] == _ELEMENTGROUP:
        return second
    return 0


def _is_psp_geometry(d: bytes, g: int) -> bool: #vers 1
    """sPspGeometry starts with size, VTYPE flags, strip count."""
    size, flags, strips = struct.unpack_from('<3I', d, g)
    return flags in _PSP_FLAGS and 0 < strips < 1024 and 0x48 <= size < len(d)


def _psp_geometry(d: bytes, g: int, names: List[str]) -> Optional[Geometry]: #vers 2
    """sPspGeometry strips (packed uv/colour/normal/position) to Geometry."""
    flags, strips = struct.unpack_from('<2I', d, g + 4)
    scale = struct.unpack_from('<3f', d, g + 32)
    pos = struct.unpack_from('<3f', d, g + 48)
    vbase = g + struct.unpack_from('<I', d, g + 64)[0]
    uvf, colf, nrmf, posf = flags & 3, flags >> 2 & 7, flags >> 5 & 3, flags >> 7 & 3
    nweights = ((flags >> 14 & 7) + 1) if flags >> 9 & 3 else 0
    geom = Geometry(flags=_GEOM_FLAGS, uv_layer_count=1)
    verts, uvs, cols = [], [], []
    recs, strip_recs = [], []                        # file offsets, for write back
    for s in range(strips):
        so = g + 0x48 + s * 0x30
        off, ntris, mat = struct.unpack_from('<IHH', d, so)
        us, vs = struct.unpack_from('<2f', d, so + 12)
        p, o, base = vbase + off, 0, len(verts)
        n = ntris + 2
        strip_recs.append((so, base, n))
        for _ in range(n):
            o += nweights
            u = v = 0.0
            uv_at = col_at = None
            if uvf == 1:
                uv_at = p + o
                u, v = d[p + o] / 128.0 * us, d[p + o + 1] / 128.0 * vs
                o += 2
            c = (255, 255, 255, 255)
            if colf == 5:
                o += o & 1
                col_at = p + o
                w = struct.unpack_from('<H', d, p + o)[0]
                c = ((w & 31) * 255 // 31, (w >> 5 & 31) * 255 // 31, (w >> 10 & 31) * 255 // 31, 255)
                o += 2
            if nrmf == 1:
                o += 3
            if posf == 1:
                recs.append((uv_at, col_at, p + o))
                q = struct.unpack_from('<3b', d, p + o); k = 128.0; o += 3
            else:
                o += o & 1
                recs.append((uv_at, col_at, p + o))
                q = struct.unpack_from('<3h', d, p + o); k = 32768.0; o += 6
            verts.append(tuple(q[i] / k * scale[i] + pos[i] for i in range(3)))
            uvs.append((u, v))
            cols.append(c)
        for a, b, c_ in _strip_triangles(n, base):
            if verts[a] != verts[b] and verts[b] != verts[c_] and verts[a] != verts[c_]:
                geom.triangles.append(Triangle(a, b, c_, mat))
    if not verts:
        return None
    geom._psp = (strip_recs, recs, 8 if posf == 1 else 16)
    geom.vertices = [Vector3(*v) for v in verts]
    geom.uv_layers = [[TexCoord(u, v) for u, v in uvs]]
    geom.colors = [RGBA(*c) for c in cols]
    for name in names or ['']:
        m = Material(texture_name=name)
        m.colour = m.color
        geom.materials.append(m)
    arr = np.array(verts)
    centre = (arr.min(0) + arr.max(0)) / 2.0
    geom.bounding_sphere = BoundingSphere(Vector3(*centre.tolist()),
                                          float(np.linalg.norm(arr - centre, axis=1).max()))
    return geom


def _element_geometry(d: bytes, e: int) -> Optional[Geometry]: #vers 1
    """Geometry of one RslElement (material names from its RslMaterials)."""
    u = lambda o: struct.unpack_from('<I', d, o)[0]
    g = u(e + 20)
    if not (0 < g < len(d) - 32) or d[g] != _GEOM:
        return None
    mats, count = u(g + 12), u(g + 16)
    names = [_cstr(d, u(u(mats + i * 4))) for i in range(count)] if 0 < mats < len(d) else []
    nb = g + 32
    if _is_psp_geometry(d, nb):
        return _psp_geometry(d, nb, names)
    size = u(nb + 0x10) & 0xFFFFF
    return _ps2_geometry(d[nb:nb + size], names)


def _node_matrix(d: bytes, e: int) -> Optional[np.ndarray]: #vers 1
    """World 4x4 of an element's RslNode chain (modelling matrices)."""
    u = lambda o: struct.unpack_from('<I', d, o)[0]
    node, acc, seen = u(e + 4), np.identity(4), set()
    while 0 < node < len(d) - 80 and node not in seen and d[node] == 0:
        seen.add(node)
        m = np.array(struct.unpack_from('<16f', d, node + 16)).reshape(4, 4)
        m[:, 3] = (0, 0, 0, 1)
        acc = acc @ m
        node = u(node + 4)
    return None if np.allclose(acc, np.identity(4)) else acc


def parse_stories_mdl(data: bytes, name: str = '') -> Optional[DFFModel]: #vers 4
    """LCS/VCS .mdl to DFFModel (simple objects: highest LOD; peds: skin in bind pose)."""
    if not is_stories_mdl(data):
        return None
    model = DFFModel(source_path=name)
    ped = bool(_ped_group(data))
    for e in _elements(data):
        g = _element_geometry(data, e)
        if g is None or not g.vertices:
            continue
        m = None if ped else _node_matrix(data, e)     # skinned verts are model space
        g._mdl = (e, m)                                 # for write_stories_mdl
        if m is not None:                               # hierarchy part offset
            v = np.array([(p.x, p.y, p.z, 1.0) for p in g.vertices]) @ m
            g.vertices = [Vector3(*r[:3].tolist()) for r in v]
        model.geometries.append(g)
    return model if model.geometries else None


def _fit_axis(vals: np.ndarray, scale: float, k: float, lo: int, hi: int,
              pow2: bool = False) -> Tuple[float, np.ndarray]: #vers 2
    """Quantise vals as q / k * scale; widens scale (power of two for positions) when out of range."""
    q = vals / scale * k if scale else np.full(vals.shape, np.inf)
    if not np.all(np.isfinite(q)) or q.min() < lo - 0.5 or q.max() > hi + 0.5:
        top = max(abs(vals.min()), abs(vals.max())) if lo < 0 else vals.max()
        scale = max(float(top) * k / hi, 1e-6)
        if pow2:
            scale = 2.0 ** math.ceil(math.log2(scale))
        q = vals / scale * k
    return scale, np.clip(np.rint(q), lo, hi).astype(int)


def _pack_555(rgba, old: int) -> int: #vers 1
    """RGBA to 5551 word, alpha bit kept from the old word."""
    r, g, b = (int(round(c * 31 / 255)) for c in rgba[:3])
    return (old & 0x8000) | r | g << 5 | b << 10


def _material_slots(d: bytes, e: int) -> List[Tuple[int, int]]: #vers 1
    """(name offset, name length) per material of an element."""
    u = lambda o: struct.unpack_from('<I', d, o)[0]
    g = u(e + 20)
    mats, count = u(g + 12), u(g + 16)
    out = []
    for i in range(count if 0 < mats < len(d) else 0):
        p = u(u(mats + i * 4))
        out.append((p, len(_cstr(d, p))))
    return out


def _write_geometry(out: bytearray, nb: int, geom, old, local: np.ndarray) -> List[str]: #vers 1
    """Patch positions / uvs / colours of one geometry in place; returns what changed."""
    done = []
    psp = getattr(old, '_psp', None)
    new_uv = np.array([(t.u, t.v) for t in geom.uv_layers[0]]) if geom.uv_layers else None
    old_uv = np.array([(t.u, t.v) for t in old.uv_layers[0]])
    old_pos = np.array([(v.x, v.y, v.z) for v in old.vertices])
    world = np.array([(v.x, v.y, v.z) for v in geom.vertices])
    if not np.allclose(world, old_pos, atol=1e-5):
        if psp:
            strips, recs, bits = psp
            k, lo, hi, fmt = (128.0, -128, 127, '<3b') if bits == 8 else (32768.0, -32768, 32767, '<3h')
            scale, pos = struct.unpack_from('<3f', out, nb + 32), struct.unpack_from('<3f', out, nb + 48)
            sc, q = [], []
            for a in range(3):
                vals = local[:, a] - pos[a]
                s, qa = _fit_axis(vals, scale[a], k, lo, hi, True)
                if s != scale[a]:                           # out of range: recentre this axis
                    c = (local[:, a].min() + local[:, a].max()) / 2.0
                    s, qa = _fit_axis(local[:, a] - c, 0.0, k, lo, hi, True)
                    pos = pos[:a] + (c,) + pos[a + 1:]
                sc.append(s); q.append(qa)
            struct.pack_into('<3f', out, nb + 32, *sc)
            struct.pack_into('<3f', out, nb + 48, *pos)
            for i, (_uv, _col, at) in enumerate(recs):
                struct.pack_into(fmt, out, at, int(q[0][i]), int(q[1][i]), int(q[2][i]))
            sphere_at = nb + 16
        else:
            scale = list(struct.unpack_from('<3f', out, nb + 0x28))
            pivot = list(struct.unpack_from('<3f', out, nb + 0x34))
            rows, i = [], 0
            for _mo, n, at, bits, _ua, _ub, _ca in old._vif:
                rows.append((i, n, at, bits)); i += n
            q16 = np.concatenate([np.arange(i0, i0 + n) for i0, n, _a, b in rows if b == 16] or [np.zeros(0, int)])
            if len(q16):
                for a in range(3):
                    s, _q = _fit_axis(local[q16, a] - pivot[a], scale[a], 32768.0, -32768, 32767, True)
                    if s != scale[a]:
                        pivot[a] = float(local[q16, a].min() + local[q16, a].max()) / 2.0
                        s, _q = _fit_axis(local[q16, a] - pivot[a], 0.0, 32768.0, -32768, 32767, True)
                    scale[a] = s
                struct.pack_into('<3f', out, nb + 0x28, *scale)
                struct.pack_into('<3f', out, nb + 0x34, *pivot)
            for i0, n, at, bits in rows:
                blk = local[i0:i0 + n]
                if bits == 16:
                    q = np.clip(np.rint((blk - pivot) / scale * 32768.0), -32768, 32767).astype('<i2')
                    out[nb + at:nb + at + n * 6] = q.tobytes()
                else:
                    out[nb + at:nb + at + n * 12] = blk.astype('<f4').tobytes()
            sphere_at = nb
        cx, cy, cz, r = struct.unpack_from('<4f', out, sphere_at)
        if np.linalg.norm(local - (cx, cy, cz), axis=1).max() > r * 1.001:
            c = (local.min(0) + local.max(0)) / 2.0
            struct.pack_into('<4f', out, sphere_at, *c.tolist(), float(np.linalg.norm(local - c, axis=1).max()))
        done.append("positions")
    if new_uv is not None and len(new_uv) == len(old_uv) and not np.allclose(new_uv, old_uv, atol=1e-6):
        if psp:
            groups = [(so + 12, np.arange(b, b + n), 8) for so, b, n in psp[0]]
            ats = [r[0] for r in psp[1]]
        else:
            groups, i, ats = {}, 0, []
            for mo, n, _at, _b, ua, ub, _ca in old._vif:
                groups.setdefault(mo, [nb + mo + 0x10, [], ub])[1].extend(range(i, i + n))
                ats += [None if ua is None else nb + ua + j * (2 if ub == 8 else 4) for j in range(n)]
                i += n
            groups = [(at, np.array(ix), ub) for at, ix, ub in groups.values()]
        if any(a is None for a in ats):
            done.append("uvs not written (model has no stored uvs)")
        elif new_uv.min() < 0 and any(ub == 8 for _a, _ix, ub in groups):
            done.append("uvs not written (negative uv)")
        else:
            for at, ix, ub in groups:
                scales = list(struct.unpack_from('<2f', out, at))
                k, lo, hi, fmt = (128.0, 0, 255, '<2B') if ub == 8 else (4096.0, -32768, 32767, '<2h')
                qs = []
                for a in range(2):
                    scales[a], qa = _fit_axis(new_uv[ix, a], scales[a], k, lo, hi)
                    qs.append(qa)
                struct.pack_into('<2f', out, at, *scales)
                for j, vi in enumerate(ix):
                    struct.pack_into(fmt, out, ats[vi], int(qs[0][j]), int(qs[1][j]))
            done.append("uvs")
    if geom.colors and len(geom.colors) == len(old.colors) and \
            [(c.r, c.g, c.b) for c in geom.colors] != [(c.r, c.g, c.b) for c in old.colors]:
        if psp:
            ats = [r[1] for r in psp[1]]
        else:
            ats = []
            for _mo, n, _at, _b, _ua, _ub, ca in old._vif:
                ats += [None if ca is None else nb + ca + j * 2 for j in range(n)]
        if any(a is None for a in ats):
            done.append("colours not written (model has no vertex colours)")
        else:
            for c, at in zip(geom.colors, ats):
                w = struct.unpack_from('<H', out, at)[0]
                struct.pack_into('<H', out, at, _pack_555((c.r, c.g, c.b), w))
            done.append("colours")
    return done


def write_stories_mdl(original: bytes, model) -> Tuple[bytes, List[str]]: #vers 1
    """Write edits back into the original .mdl (same mesh layout); (bytes, report)."""
    if not is_stories_mdl(original):
        raise ValueError("source is not an LCS/VCS .mdl")
    u = lambda o: struct.unpack_from('<I', original, o)[0]
    base = {g._mdl[0]: g for g in (parse_stories_mdl(original) or DFFModel()).geometries}
    out = bytearray(original)
    report, rebuild = [], []
    for gi, geom in enumerate(model.geometries):
        info = getattr(geom, '_mdl', None)
        old = base.get(info[0]) if info else None
        if old is None:
            report.append(f"not written: geometry {gi} was added after load")
            continue
        e, m = info
        world = np.array([(v.x, v.y, v.z, 1.0) for v in geom.vertices]).reshape(-1, 4)
        local = (world @ np.linalg.inv(m) if m is not None else world)[:, :3]
        if len(geom.vertices) != len(old.vertices) or \
                [(t.v1, t.v2, t.v3, t.material_id) for t in geom.triangles] != \
                [(t.v1, t.v2, t.v3, t.material_id) for t in old.triangles]:
            try:
                block = _psp_block(original, u(e + 20), geom, local) if getattr(old, '_psp', None) is not None \
                    else _ps2_block(original, u(e + 20), old, geom, local)
                rebuild.append((gi, e, block, len(geom.triangles)))
            except ValueError as ex:
                report.append(f"not written: geometry {gi} mesh rebuild - {ex}")
                continue
            done = []
        else:
            done = _write_geometry(out, u(e + 20) + 32, geom, old, local)
        slots = _material_slots(original, e)
        for mi, mat in enumerate(geom.materials[:len(slots)]):
            p, n = slots[mi]
            name = (mat.texture_name or '').encode('latin-1', 'replace')
            if name == original[p:p + n]:
                continue
            if len(name) > n:
                done.append(f"texture '{mat.texture_name}' not written (max {n} chars)")
                continue
            out[p:p + n] = name.ljust(n, b'\0')
            done.append(f"texture {mi}")
        ok = [x for x in done if 'not written' not in x]
        report += [f"not written: geometry {gi} {x.replace(' not written', '')}" for x in done if x not in ok]
        if ok:
            report.append(f"geometry {gi}: " + ", ".join(ok) + " updated")
    for gi, e, block, ntris in rebuild:               # new geometry after the data
        data_end = struct.unpack_from('<I', out, 12)[0]
        at = (data_end + 15) & ~15
        ins = bytearray(bytes(at - data_end) + block + bytes(-len(block) % 16))
        _insert(out, data_end, bytes(ins))
        mp = at + 12                                   # materials pointer of the new header
        v = struct.unpack_from('<I', out, mp)[0]
        if v >= data_end:
            struct.pack_into('<I', out, mp, v + len(ins))
        reloc, nrel = struct.unpack_from('<2I', out, 16)
        _insert(out, reloc + 4 * nrel, struct.pack('<I', mp))
        struct.pack_into('<I', out, 20, nrel + 1)
        struct.pack_into('<I', out, e + 20, at)
        report.append(f"geometry {gi}: mesh rebuilt ({ntris} triangles)")
    if rebuild:                                        # trim old sector padding, pad again
        out = out[:struct.unpack_from('<I', out, 8)[0]]
        out += bytes(-len(out) % 2048)
    data = bytes(out)
    if data == original:
        return original, report
    back = parse_stories_mdl(data)
    want = {gi: n for gi, _e, _b, n in rebuild}
    if back is None or len(back.geometries) != len(base) or any(
            len(back.geometries[gi].triangles) != n for gi, n in want.items()):
        raise ValueError("written MDL did not read back the same")
    return data, report


def _strips(tris: List[Tuple[int, int, int]], cap: int = 2048) -> List[List[int]]: #vers 1
    """Greedy triangle strips joined by degenerates; each list under cap vertices."""
    edge = {}
    for i, (a, b, c) in enumerate(tris):
        for p, q, r in ((a, b, c), (b, c, a), (c, a, b)):
            edge.setdefault((p, q), []).append((i, r))
    used = [False] * len(tris)
    runs = []
    for i, (a, b, c) in enumerate(tris):
        if used[i]:
            continue
        used[i] = True
        seq = [a, b, c]
        while len(seq) < cap - 6:
            p, q = (seq[-1], seq[-2]) if len(seq) % 2 == 1 else (seq[-2], seq[-1])
            nxt = next(((j, r) for j, r in edge.get((p, q), []) if not used[j]), None)
            if nxt is None:
                break
            used[nxt[0]] = True
            seq.append(nxt[1])
        runs.append(seq)
    out, cur = [], []
    for seq in runs:
        if cur and len(cur) + len(seq) + 3 <= cap:
            cur += [cur[-1], seq[0]]
            if len(cur) % 2:
                cur.append(seq[0])
            cur += seq
        else:
            if cur:
                out.append(cur)
            cur = list(seq)
    if cur:
        out.append(cur)
    return out


def _weld(local: np.ndarray, uv: np.ndarray, cols) -> np.ndarray: #vers 1
    """Index of the first vertex with the same position / uv / colour (strip adjacency)."""
    seen, out = {}, np.arange(len(local))
    for i in range(len(local)):
        c = cols[i] if cols else None
        key = (tuple(np.round(local[i], 5)), tuple(np.round(uv[i], 5)), (c.r, c.g, c.b) if c else None)
        out[i] = seen.setdefault(key, i)
    return out


def _vertex_normals(pos: np.ndarray, tris: List[Tuple[int, int, int]]) -> np.ndarray: #vers 1
    """Area weighted vertex normals."""
    n = np.zeros_like(pos)
    if tris:
        t = np.array(tris)
        f = np.cross(pos[t[:, 1]] - pos[t[:, 0]], pos[t[:, 2]] - pos[t[:, 0]])
        for k in range(3):
            np.add.at(n, t[:, k], f)
    ln = np.linalg.norm(n, axis=1, keepdims=True)
    return np.divide(n, ln, out=np.zeros_like(n), where=ln > 0)


def _psp_block(d: bytes, g: int, geom, local: np.ndarray) -> bytes: #vers 1
    """New RslGeometry + sPspGeometry for a changed mesh (old header as template)."""
    nb = g + 32
    flags = struct.unpack_from('<I', d, nb + 4)[0]
    uvf, colf, nrmf, posf = flags & 3, flags >> 2 & 7, flags >> 5 & 3, flags >> 7 & 3
    if flags >> 9 & 3:
        raise ValueError("skinned mesh")
    if uvf > 1 or colf not in (0, 5) or nrmf > 1 or posf not in (1, 2):
        raise ValueError(f"vertex format 0x{flags:X} not supported")
    nmat = struct.unpack_from('<I', d, g + 16)[0]
    if any(t.material_id >= max(nmat, 1) for t in geom.triangles):
        raise ValueError("new materials (MDL keeps its material list)")
    tmpl = d[nb + 0x48:nb + 0x78]
    uv = np.array([(t.u, t.v) for t in geom.uv_layers[0]]) if geom.uv_layers and \
        len(geom.uv_layers[0]) == len(local) else np.zeros((len(local), 2))
    if uvf and uv.min() < 0:
        raise ValueError("negative uvs")
    cols = geom.colors if len(geom.colors) == len(local) else None
    nrm = np.array([(n.x, n.y, n.z) for n in geom.normals]) if len(geom.normals) == len(local) else \
        _vertex_normals(local, [(t.v1, t.v2, t.v3) for t in geom.triangles])
    k, lo, hi = (128.0, -128, 127) if posf == 1 else (32768.0, -32768, 32767)
    centre = (local.min(0) + local.max(0)) / 2.0
    scale, q = [], []
    for a in range(3):
        s_, qa = _fit_axis(local[:, a] - centre[a], 0.0, k, lo, hi, True)
        scale.append(s_); q.append(qa)
    q = np.stack(q, 1)
    w = _weld(local, uv, cols)
    by_mat = {}
    for t in geom.triangles:
        by_mat.setdefault(t.material_id, []).append((w[t.v1], w[t.v2], w[t.v3]))
    strips = [(m, s_) for m, tris in sorted(by_mat.items()) for s_ in _strips(tris)]
    recs, vdata = [], bytearray()
    for mat, seq in strips:
        while len(vdata) % 4:
            vdata.append(0)
        ix = np.array(seq)
        us = max(float(uv[ix, 0].max()) * 128.0 / 255.0, 1e-6) if uvf else 1.0
        vs = max(float(uv[ix, 1].max()) * 128.0 / 255.0, 1e-6) if uvf else 1.0
        start = len(vdata)
        for vi in seq:
            if uvf:
                vdata += bytes((min(255, int(round(uv[vi, 0] / us * 128.0))),
                                min(255, int(round(uv[vi, 1] / vs * 128.0)))))
            if colf == 5:
                if len(vdata) & 1:
                    vdata.append(0)
                c = cols[vi] if cols else None
                vdata += struct.pack('<H', _pack_555((c.r, c.g, c.b), 0x8000) if c else 0xFFFF)
            if nrmf:
                vdata += struct.pack('<3b', *(int(np.clip(round(x * 127), -127, 127)) for x in nrm[vi]))
            if posf == 1:
                vdata += struct.pack('<3b', *map(int, q[vi]))
            else:
                if len(vdata) & 1:
                    vdata.append(0)
                vdata += struct.pack('<3h', *map(int, q[vi]))
        pts = local[ix]
        c = (pts.min(0) + pts.max(0)) / 2.0
        recs.append((start, len(seq) - 2, mat, us, vs, c, float(np.linalg.norm(pts - c, axis=1).max())))
    vbase = (0x48 + 0x30 * len(recs) + 15) & ~15
    hdr = bytearray(d[nb:nb + 0x48])
    c = (local.min(0) + local.max(0)) / 2.0
    struct.pack_into('<3I', hdr, 0, vbase + len(vdata), flags, len(recs))
    struct.pack_into('<4f', hdr, 16, *c.tolist(), float(np.linalg.norm(local - c, axis=1).max()))
    struct.pack_into('<3fI3f', hdr, 32, *scale, sum(len(s_) for _m, s_ in strips), *centre.tolist())
    struct.pack_into('<I', hdr, 64, vbase)
    body = bytearray()
    for start, ntris, mat, us, vs, c, r in recs:
        rec = bytearray(tmpl)
        struct.pack_into('<IHH', rec, 0, start, ntris, mat)
        struct.pack_into('<2f', rec, 12, us, vs)
        struct.pack_into('<4f', rec, 20, *c.tolist(), r)
        body += rec
    out = bytearray(d[g:g + 32]) + hdr + body
    out += bytes(32 + vbase - len(out)) + vdata
    return bytes(out)


def _insert(out: bytearray, at: int, block: bytes) -> None: #vers 1
    """Insert bytes into a relocatable chunk, shifting offsets and pointers at or after it."""
    n = len(block)
    shrink, file_end, data_end, reloc, nrel, glob = struct.unpack_from('<6I', out, 4)
    relocs = list(struct.unpack_from(f'<{nrel}I', out, reloc))
    out[at:at] = block
    sh = lambda v: v + n if v >= at else v
    struct.pack_into('<6I', out, 4, shrink, sh(file_end), sh(data_end), sh(reloc), nrel, sh(glob))
    relocs = [sh(r) for r in relocs]
    for r in relocs:
        struct.pack_into('<I', out, r, sh(struct.unpack_from('<I', out, r)[0]))
    struct.pack_into(f'<{nrel}I', out, sh(reloc), *relocs)


def _ps2_block(d: bytes, g: int, old, geom, local: np.ndarray) -> bytes: #vers 1
    """New RslGeometry + sPs2Geometry (VIF batches from the old meshes as template)."""
    nb = g + 32
    G = d[nb:]
    packed, flags = struct.unpack_from('<2I', G, 0x10)
    nm, pk = packed >> 20, struct.unpack_from('<H', G, 0x1A)[0]
    meshes = {}
    for m in range(nm):
        mo = _PS2_HEADER + m * _PS2_MESH
        mat = struct.unpack_from('<h', G, mo + 0x22)[0]
        meshes.setdefault(max(mat, 0), mo)
    if any(t.material_id not in meshes for t in geom.triangles):
        raise ValueError("new materials (MDL keeps its material list)")
    maxn = max(n for _mo, n, *_r in old._vif) & ~1
    if maxn < 4:
        raise ValueError("batch size too small")

    def _template(mo):
        dma = struct.unpack_from('<I', G, mo + 0x1C)[0]
        tag = pk + dma
        qwc = struct.unpack_from('<I', G, tag)[0] & 0xFFFF
        cmds = list(_vif_commands(G, tag + 16, tag + 16 + qwc * 16))
        calls = [i for i, c in enumerate(cmds) if c[2] in (0x14, 0x15, 0x17)]
        if not calls:
            raise ValueError("no MSCAL in mesh packet")
        return G[tag:tag + 16], cmds[:calls[0] + 1], cmds[calls[-1] + 1:]
    uv = np.array([(t.u, t.v) for t in geom.uv_layers[0]]) if geom.uv_layers and \
        len(geom.uv_layers[0]) == len(local) else np.zeros((len(local), 2))
    cols = geom.colors if len(geom.colors) == len(local) else None
    nrm = np.array([(n.x, n.y, n.z) for n in geom.normals]) if len(geom.normals) == len(local) else \
        _vertex_normals(local, [(t.v1, t.v2, t.v3) for t in geom.triangles])
    centre = (local.min(0) + local.max(0)) / 2.0
    scale, q = [], []
    for a in range(3):
        s_, qa = _fit_axis(local[:, a] - centre[a], 0.0, 32768.0, -32768, 32767, True)
        scale.append(s_); q.append(qa)
    q = np.stack(q, 1)
    w = _weld(local, uv, cols)
    by_mat = {}
    for t in geom.triangles:
        by_mat.setdefault(t.material_id, []).append((w[t.v1], w[t.v2], w[t.v3]))
    mesh_recs, packets, total = [], bytearray(), 0
    for mat, tris in sorted(by_mat.items()):
        seq = _strips(tris, 1 << 30)[0]
        mo_t = meshes[mat]
        tag, batch, tail = _template(mo_t)
        uvb = next((_UNPACK[c & 0xF][1] for _a, _s, c, _n, _i in batch if c >= 0x60 and c & 0xF in (4, 5, 6)), 0)
        ix = np.array(seq)
        if uvb == 8 and uv[ix].min() < 0:
            raise ValueError("negative uvs")
        k_uv, hi_uv = (128.0, 255) if uvb == 8 else (4096.0, 32767)
        us = max(float(np.abs(uv[ix, 0]).max()) * k_uv / hi_uv, 1e-6)
        vs = max(float(np.abs(uv[ix, 1]).max()) * k_uv / hi_uv, 1e-6)
        body = bytearray(tag[8:])
        s0 = 0
        while True:
            part = seq[s0:s0 + maxn]
            n = len(part)
            pi = np.array(part)
            for at, size, c, num, imm in batch:
                if c < 0x60:
                    body += G[at:at + size]
                    continue
                fmt = c & 0xF
                raw = G[at + 4:at + size]
                if fmt == 0xC and num == 1:              # batch header: vertex count
                    w = list(struct.unpack_from('<4I', raw))
                    w[2], w[3] = n, (w[3] & ~0x7FFF) | n
                    data = struct.pack('<4I', *w)
                    body += struct.pack('<HBB', imm, 1, G[at + 3]) + data
                    continue
                if fmt == 0x9:
                    data = q[pi].astype('<i2').tobytes()
                elif fmt == 0x8:
                    data = local[pi].astype('<f4').tobytes()
                elif fmt == 0x6:
                    data = np.clip(np.rint(uv[pi] / (us, vs) * 128.0), 0, 255).astype(np.uint8).tobytes()
                elif fmt == 0x5:
                    data = np.clip(np.rint(uv[pi] / (us, vs) * 4096.0), -32768, 32767).astype('<i2').tobytes()
                elif fmt == 0xA:
                    data = np.clip(np.rint(nrm[pi] * 127.0), -127, 127).astype(np.int8).tobytes()
                elif fmt == 0xF:
                    data = np.array([_pack_555((cols[v].r, cols[v].g, cols[v].b), 0x8000) if cols else 0xFFFF
                                     for v in part], '<u2').tobytes()
                elif fmt == 0xC:
                    raise ValueError("skinned mesh (bone weights)")
                else:
                    raise ValueError(f"VIF unpack format 0x{fmt:X} not supported")
                data += bytes(-len(data) % 4)
                body += struct.pack('<HBB', imm, n & 0xFF, G[at + 3]) + data
            if s0 + maxn >= len(seq):
                break
            s0 += maxn - 2
        for at, size, _c, _n, _i in tail:
            body += G[at:at + size]
        body += bytes(-(len(body) + 8) % 16)
        qwc = (len(body) - 8) // 16
        tag_w = (struct.unpack_from('<I', tag)[0] & ~0xFFFF) | qwc
        pts = local[ix]
        c = (pts.min(0) + pts.max(0)) / 2.0
        mesh_recs.append((mo_t, mat, len(packets), len(seq) - 2, c, float(np.linalg.norm(pts - c, axis=1).max()),
                          us, vs, q[ix].min(0), q[ix].max(0)))
        packets += struct.pack('<I', tag_w) + tag[4:8] + body
        total += len(seq)
    nm2 = len(mesh_recs)
    pk2 = (_PS2_HEADER + nm2 * _PS2_MESH + 15) & ~15
    hdr = bytearray(G[:_PS2_HEADER])
    c = (local.min(0) + local.max(0)) / 2.0
    struct.pack_into('<4f', hdr, 0, *c.tolist(), float(np.linalg.norm(local - c, axis=1).max()))
    struct.pack_into('<2I', hdr, 0x10, nm2 << 20 | (pk2 + len(packets)), flags)
    struct.pack_into('<HH6h', hdr, 0x18, total & 0xFFFF, pk2, *q.min(0).tolist(), *q.max(0).tolist())
    struct.pack_into('<6f', hdr, 0x28, *scale, *centre.tolist())
    body = bytearray()
    for mo_t, mat, dma, ntris, c, r, us, vs, mn, mx in mesh_recs:
        rec = bytearray(G[mo_t:mo_t + _PS2_MESH])
        struct.pack_into('<4f2f', rec, 0, *c.tolist(), r, us, vs)
        struct.pack_into('<IHh6h', rec, 0x1C, dma, ntris, mat, *mn.tolist(), *mx.tolist())
        body += rec
    out = bytearray(d[g:g + 32]) + hdr + body
    out += bytes(32 + pk2 - len(out)) + packets
    return bytes(out)
