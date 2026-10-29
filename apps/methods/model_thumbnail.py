#this belongs in apps/methods/model_thumbnail.py - Version: 1
# X-Seti - October 28 2026 - IMG Factory 1.6 - DFF model thumbnails

"""
Small software-rendered DFF thumbnails (no OpenGL): three-quarter view,
flat shading, each face in its material / texture average colour.
"""

##Methods list -
# _frame_offsets
# render_model_thumbnail
# texture_average

import math

import numpy as np
from PyQt6.QtCore import QPointF, Qt
from PyQt6.QtGui import QColor, QImage, QPainter, QPolygonF

MAX_TRIS = 5000                                         # biggest faces drawn on huge models


def texture_average(tex: dict): #vers 1
    """(r, g, b) average of a texture dict's RGBA data, or None."""
    data = tex.get('rgba_data') if tex else None
    if not data:
        return None
    a = np.frombuffer(data, np.uint8)
    if a.size < 4:
        return None
    a = a[: a.size // 4 * 4].reshape(-1, 4)[::7]
    return tuple(int(v) for v in a[:, :3].mean(axis=0))


def _frame_offsets(model) -> list: #vers 1
    """World offset of each frame (positions along the parent chain)."""
    out = []
    for i, f in enumerate(model.frames):
        p = np.array([f.position.x, f.position.y, f.position.z], float)
        j = f.parent_index
        seen = 0
        while 0 <= j < len(model.frames) and j != i and seen < 64:
            q = model.frames[j].position
            p += (q.x, q.y, q.z)
            j = model.frames[j].parent_index
            seen += 1
        out.append(p)
    return out


def render_model_thumbnail(model, size: int = 96, texture_colour=None, background=None,
                           yaw: float = 35.0, pitch: float = 28.0) -> QImage: #vers 1
    """QImage of a DFFModel. texture_colour(name) -> (r, g, b) or None."""
    img = QImage(size, size, QImage.Format.Format_ARGB32)
    img.fill(background if background is not None else Qt.GlobalColor.transparent)
    offsets = _frame_offsets(model) if getattr(model, 'frames', None) else []
    atom_frame = {}
    for a in getattr(model, 'atomics', []) or []:
        atom_frame[getattr(a, 'geometry_index', -1)] = getattr(a, 'frame_index', -1)
    verts, tris, cols = [], [], []
    base = 0
    for gi, g in enumerate(model.geometries):
        if not g.vertices or not g.triangles:
            continue
        v = np.array([(p.x, p.y, p.z) for p in g.vertices], float)
        fi = atom_frame.get(gi, -1)
        if 0 <= fi < len(offsets):
            v += offsets[fi]
        mat_cols = []
        for m in g.materials or []:
            c = (m.color.r, m.color.g, m.color.b)
            tc = texture_colour(m.texture_name) if (texture_colour and m.texture_name) else None
            if tc is not None:
                c = tuple(int(a * b / 255) for a, b in zip(tc, c))
            mat_cols.append(c)
        for t in g.triangles:
            tris.append((t.v1 + base, t.v2 + base, t.v3 + base))
            cols.append(mat_cols[t.material_id] if 0 <= t.material_id < len(mat_cols) else (180, 180, 180))
        verts.append(v)
        base += len(v)
    if not tris:
        return img
    V = np.concatenate(verts)
    T = np.array(tris, int)
    T = T[(T < len(V)).all(axis=1)]
    C = np.array(cols[:len(T)], float)
    y, p = math.radians(yaw), math.radians(pitch)
    rz = np.array([[math.cos(y), -math.sin(y), 0], [math.sin(y), math.cos(y), 0], [0, 0, 1]])
    rx = np.array([[1, 0, 0], [0, math.cos(p), -math.sin(p)], [0, math.sin(p), math.cos(p)]])
    W = V @ rz.T @ rx.T                                 # x right, y depth, z up
    a, b, c = W[T[:, 0]], W[T[:, 1]], W[T[:, 2]]
    n = np.cross(b - a, c - a)
    area = np.linalg.norm(n, axis=1)
    keep = area > 1e-9
    if keep.sum() > MAX_TRIS:
        keep &= area >= np.sort(area)[-MAX_TRIS]
    a, b, c, n, area, C = a[keep], b[keep], c[keep], n[keep], area[keep], C[keep]
    n = n / area[:, None]
    light = np.array([0.35, -0.55, 0.75])
    shade = 0.45 + 0.55 * np.abs(n @ (light / np.linalg.norm(light)))
    lo, hi = W.min(axis=0), W.max(axis=0)
    span = max(hi[0] - lo[0], hi[2] - lo[2], 1e-6)
    scale = (size - 6) / span
    cx, cz = (lo[0] + hi[0]) / 2, (lo[2] + hi[2]) / 2
    order = np.argsort(-(a[:, 1] + b[:, 1] + c[:, 1]))  # far first (painter's)
    P = QPainter(img)
    P.setRenderHint(QPainter.RenderHint.Antialiasing)
    P.setPen(Qt.PenStyle.NoPen)
    half = size / 2
    for k in order:
        r, g, bl = (np.clip(C[k] * shade[k], 0, 255)).astype(int)
        P.setBrush(QColor(int(r), int(g), int(bl)))
        P.drawPolygon(QPolygonF([QPointF(half + (q[0] - cx) * scale, half - (q[2] - cz) * scale)
                                 for q in (a[k], b[k], c[k])]))
    P.end()
    return img
