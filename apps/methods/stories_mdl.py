#this belongs in apps/methods/stories_mdl.py - Version: 1
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
# _is_psp_geometry
# _node_matrix
# _psp_geometry
# is_stories_mdl
# parse_stories_mdl

import struct
from typing import List, Optional

import numpy as np

from apps.methods.dff_classes import (BoundingSphere, DFFModel, Geometry, Material,
                                      RGBA, TexCoord, Triangle, Vector3)
from apps.methods.lcs_geometry import _GEOM_FLAGS, _geometry as _ps2_geometry, _strip_triangles

MDL_IDENT = b'ldm\x00'
_HEADER = 32
_ELEMENT, _ELEMENTGROUP, _GEOM = 1, 2, 8
_PSP_FLAGS = {0x120, 0x121, 0x115, 0x114, 0xA1, 0x1C321}


def is_stories_mdl(data: bytes) -> bool: #vers 1
    """True for an LCS/VCS relocatable model chunk."""
    return len(data) > _HEADER and data[:4] == MDL_IDENT


def _cstr(d: bytes, p: int) -> str:
    return d[p:p + 32].split(b'\0', 1)[0].decode('latin-1') if 0 < p < len(d) else ''


def _elements(d: bytes) -> List[int]: #vers 1
    """RslElement offsets: simple model array, or element group atomics."""
    u = lambda o: struct.unpack_from('<I', d, o)[0]
    n_funcs = struct.unpack_from('<H', d, 30)[0]
    first = u(_HEADER)
    if not (0 < first < len(d)):
        return []
    if d[first] == _ELEMENT and n_funcs:
        els = [u(_HEADER + i * 4) for i in range(n_funcs)]
        return [e for e in els if 0 < e < len(d) and d[e] == _ELEMENT][:1]   # L0 only
    group = _HEADER if d[_HEADER] == _ELEMENTGROUP else (first if d[first] == _ELEMENTGROUP else 0)
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


def _is_psp_geometry(d: bytes, g: int) -> bool: #vers 1
    """sPspGeometry starts with size, VTYPE flags, strip count."""
    size, flags, strips = struct.unpack_from('<3I', d, g)
    return flags in _PSP_FLAGS and 0 < strips < 1024 and 0x48 <= size < len(d)


def _psp_geometry(d: bytes, g: int, names: List[str]) -> Optional[Geometry]: #vers 1
    """sPspGeometry strips (packed uv/colour/normal/position) to Geometry."""
    flags, strips = struct.unpack_from('<2I', d, g + 4)
    scale = struct.unpack_from('<3f', d, g + 32)
    pos = struct.unpack_from('<3f', d, g + 48)
    vbase = g + struct.unpack_from('<I', d, g + 64)[0]
    uvf, colf, nrmf, posf = flags & 3, flags >> 2 & 7, flags >> 5 & 3, flags >> 7 & 3
    nweights = ((flags >> 14 & 7) + 1) if flags >> 9 & 3 else 0
    geom = Geometry(flags=_GEOM_FLAGS, uv_layer_count=1)
    verts, uvs, cols = [], [], []
    for s in range(strips):
        so = g + 0x48 + s * 0x30
        off, ntris, mat = struct.unpack_from('<IHH', d, so)
        us, vs = struct.unpack_from('<2f', d, so + 12)
        p, o, base = vbase + off, 0, len(verts)
        n = ntris + 2
        for _ in range(n):
            o += nweights
            u = v = 0.0
            if uvf == 1:
                u, v = d[p + o] / 128.0 * us, d[p + o + 1] / 128.0 * vs
                o += 2
            c = (255, 255, 255, 255)
            if colf == 5:
                o += o & 1
                w = struct.unpack_from('<H', d, p + o)[0]
                c = ((w & 31) * 255 // 31, (w >> 5 & 31) * 255 // 31, (w >> 10 & 31) * 255 // 31, 255)
                o += 2
            if nrmf == 1:
                o += 3
            if posf == 1:
                q = struct.unpack_from('<3b', d, p + o); k = 128.0; o += 3
            else:
                o += o & 1
                q = struct.unpack_from('<3h', d, p + o); k = 32768.0; o += 6
            verts.append(tuple(q[i] / k * scale[i] + pos[i] for i in range(3)))
            uvs.append((u, v))
            cols.append(c)
        for a, b, c_ in _strip_triangles(n, base):
            if verts[a] != verts[b] and verts[b] != verts[c_] and verts[a] != verts[c_]:
                geom.triangles.append(Triangle(a, b, c_, mat))
    if not verts:
        return None
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


def parse_stories_mdl(data: bytes, name: str = '') -> Optional[DFFModel]: #vers 2
    """LCS/VCS .mdl to DFFModel (simple objects: highest LOD only)."""
    if not is_stories_mdl(data):
        return None
    model = DFFModel(source_path=name)
    for e in _elements(data):
        g = _element_geometry(data, e)
        if g is None or not g.vertices:
            continue
        m = _node_matrix(data, e)
        if m is not None:                               # hierarchy part offset
            v = np.array([(p.x, p.y, p.z, 1.0) for p in g.vertices]) @ m
            g.vertices = [Vector3(*r[:3].tolist()) for r in v]
        model.geometries.append(g)
    return model if model.geometries else None
