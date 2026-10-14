#!/usr/bin/env python3
#this belongs in apps/components/Map_Editor/depends/map_prelight.py - Version: 3
# X-Seti - September23 2026 - IMG Factory 1.6 - Map Workshop prelight bake

"""
Prelight bake - ambient + directional light baked into a DFF's existing
vertex colour (prelit) channel, written back with dff_patch.
"""

##Methods list -
# apply_instance_colours
# bake_prelight
# _entry_world
# light_vector
# _light_basis
# _quat_matrix
# _shadow_map
# world_prelight

##class PrelightDialog: -
# __init__
# _paint
# _pick
# values

import math

from PyQt6.QtGui import QColor
from PyQt6.QtWidgets import (QColorDialog, QDialog, QDialogButtonBox, QDoubleSpinBox, QFormLayout,
                             QPushButton)


def light_vector(azimuth_deg, elevation_deg): #vers 1
    """Unit vector pointing from the surface toward the light (world, Z up)."""
    az, el = math.radians(azimuth_deg), math.radians(elevation_deg)
    return (math.cos(el) * math.sin(az), math.cos(el) * math.cos(az), math.sin(el))


def _vertex_normals(g): #vers 1
    """Per-vertex normals: the geometry's own, else averaged face normals."""
    if len(g.normals) == len(g.vertices):
        return [(n.x, n.y, n.z) for n in g.normals]
    acc = [[0.0, 0.0, 0.0] for _ in g.vertices]
    for t in g.triangles:
        ids = (t.v1, t.v2, t.v3)
        a, b, c = (g.vertices[i] for i in ids)
        ux, uy, uz = b.x - a.x, b.y - a.y, b.z - a.z
        vx, vy, vz = c.x - a.x, c.y - a.y, c.z - a.z
        n = (uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx)
        for i in ids:
            acc[i][0] += n[0]; acc[i][1] += n[1]; acc[i][2] += n[2]
    out = []
    for x, y, z in acc:
        ln = math.sqrt(x * x + y * y + z * z) or 1.0
        out.append((x / ln, y / ln, z / ln))
    return out


def _quat_matrix(q): #vers 1
    """3x3 rotation matrix from (x, y, z, w)."""
    import numpy as np
    x, y, z, w = q
    return np.array([[1 - 2 * (y * y + z * z), 2 * (x * y - z * w), 2 * (x * z + y * w)],
                     [2 * (x * y + z * w), 1 - 2 * (x * x + z * z), 2 * (y * z - x * w)],
                     [2 * (x * z - y * w), 2 * (y * z + x * w), 1 - 2 * (x * x + y * y)]])


def _entry_world(e): #vers 1
    """(world verts N x 3, world normals N x 3, tris M x 3) of a world entry, numpy."""
    import numpy as np
    v = np.asarray(e['vertices'], dtype=np.float64).reshape(-1, 3)
    t = np.asarray([tr[:3] for tr in e['triangles']], dtype=np.int64).reshape(-1, 3)
    m = _quat_matrix(e.get('rot', (0.0, 0.0, 0.0, 1.0)))
    sc = np.asarray(e.get('scale', (1.0, 1.0, 1.0)), dtype=np.float64)
    wv = (v * sc) @ m.T + np.asarray(e.get('pos', (0.0, 0.0, 0.0)), dtype=np.float64)
    if len(e.get('normals') or []) == len(v):
        n = np.asarray(e['normals'], dtype=np.float64).reshape(-1, 3)
    else:                                            # averaged face normals
        n = np.zeros_like(v)
        if len(t):
            fn = np.cross(v[t[:, 1]] - v[t[:, 0]], v[t[:, 2]] - v[t[:, 0]])
            for k in range(3):
                np.add.at(n, t[:, k], fn)
    n = (n / np.where(sc == 0, 1.0, sc)) @ m.T       # normals use inverse scale
    n /= np.maximum(np.linalg.norm(n, axis=1, keepdims=True), 1e-9)
    return wv, n, t


def _light_basis(sun): #vers 1
    """Two unit vectors perpendicular to the sun direction (shadow map axes)."""
    import numpy as np
    L = np.asarray(sun, dtype=np.float64)
    a = np.array([0.0, 0.0, 1.0]) if abs(L[2]) < 0.9 else np.array([1.0, 0.0, 0.0])
    u = np.cross(a, L); u /= np.linalg.norm(u)
    return u, np.cross(L, u)


def _shadow_map(worlds, sun, res, progress=None): #vers 1
    """Orthographic depth map from the sun: max (p . sun) per cell over every triangle."""
    import numpy as np
    L = np.asarray(sun, dtype=np.float64)
    u, w = _light_basis(L)
    allv = np.concatenate([wv for wv, _n, _t in worlds if len(wv)])
    pu, pw = allv @ u, allv @ w
    lo = np.array([pu.min(), pw.min()]) - 1.0
    span = max(pu.max() - lo[0], pw.max() - lo[1]) + 2.0
    cell = span / res
    depth = np.full((res, res), -np.inf)
    for k, (wv, _n, t) in enumerate(worlds):
        if not len(t):
            continue
        a, b, c = wv[t[:, 0]], wv[t[:, 1]], wv[t[:, 2]]
        uv = lambda p: np.stack([(p @ u - lo[0]) / cell, (p @ w - lo[1]) / cell], 1)
        ua, ub, uc = uv(a), uv(b), uv(c)
        area = np.abs(np.cross(ub - ua, uc - ua)) * 0.5        # in cells
        nsamp = np.clip(np.ceil(np.sqrt(area) * 1.5), 1, 128).astype(np.int64)
        for n_ in np.unique(nsamp):                            # barycentric grid per size
            sel = nsamp == n_
            g = np.array([(i / n_, j / n_) for i in range(n_ + 1) for j in range(n_ + 1 - i)])
            s1, s2 = g[:, 0][None, :, None], g[:, 1][None, :, None]
            pa, pb, pc = a[sel][:, None, :], b[sel][:, None, :], c[sel][:, None, :]
            pts = (pa + (pb - pa) * s1 + (pc - pa) * s2).reshape(-1, 3)
            iu = np.clip(((pts @ u - lo[0]) / cell).astype(np.int64), 0, res - 1)
            iw = np.clip(((pts @ w - lo[1]) / cell).astype(np.int64), 0, res - 1)
            np.maximum.at(depth, (iu, iw), pts @ L)
        if progress and k % 50 == 0:
            progress(k, len(worlds) * 2)
    return depth, u, w, lo, cell


def world_prelight(entries, sun, ambient, diffuse, strength, shadows=True, shadow_dark=0.6,
                   res=2048, progress=None): #vers 1
    """Per-instance prelit colours (entry['inst_prelit']) for displayed world entries:
    ambient + sun * N.L, darkened where the sun is blocked. Returns instances lit."""
    import numpy as np
    L = np.asarray(sun, dtype=np.float64)
    L /= np.linalg.norm(L)
    live = [e for e in entries if e.get('vertices') and e.get('triangles')]
    worlds = [_entry_world(e) for e in live]
    smap = _shadow_map(worlds, L, res, progress) if shadows and worlds else None
    amb, dif = np.asarray(ambient, dtype=np.float64), np.asarray(diffuse, dtype=np.float64)
    for k, (e, (wv, n, _t)) in enumerate(zip(live, worlds)):
        lit = np.clip(n @ L, 0.0, 1.0)
        if smap is not None:
            depth, u, w, lo, cell = smap
            p = wv + n * cell * 0.75                      # lift off the surface (bias)
            fu, fw, d = (p @ u - lo[0]) / cell, (p @ w - lo[1]) / cell, p @ L
            cos_t = np.clip(n @ L, 0.05, 1.0)           # slope bias: steep to the sun needs more
            bias = cell * (1.5 + 2.0 * np.minimum(np.sqrt(1.0 - cos_t ** 2) / cos_t, 6.0))
            shade = np.zeros(len(p))
            for du in (-1, 0, 1):                        # 3 x 3 soft edge
                for dw in (-1, 0, 1):
                    iu = np.clip(fu.astype(np.int64) + du, 0, depth.shape[0] - 1)
                    iw = np.clip(fw.astype(np.int64) + dw, 0, depth.shape[1] - 1)
                    shade += depth[iu, iw] > d + bias
            lit *= 1.0 - shadow_dark * (shade / 9.0)
        rgb = np.clip(amb[None, :] + dif[None, :] * lit[:, None], 0, 255)
        old = e.get('prelit') or []
        if len(old) == len(wv):
            o = np.asarray(old, dtype=np.float64)
            rgb = o[:, :3] + (rgb - o[:, :3]) * strength
            alpha = o[:, 3]
        else:
            alpha = np.full(len(wv), 255.0)
        e['inst_prelit'] = [tuple(int(x) for x in row) for row in
                            np.column_stack([np.round(rgb), alpha]).astype(np.int64)]
        if progress and k % 50 == 0:
            progress(len(live) + k, len(live) * 2)
    return len(live)


def apply_instance_colours(model, colours): #vers 1
    """Write merged per-vertex colours (all geometries in order) into a parsed DFF.
    Only geometries that already have prelit colours change. Returns geometries written."""
    done, k = 0, 0
    for g in model.geometries:
        n = len(g.vertices)
        part = colours[k:k + n]
        k += n
        if not g.colors or len(g.colors) != n or len(part) != n:
            continue
        for c, (r, gg, b, a) in zip(g.colors, part):
            c.r, c.g, c.b, c.a = int(r), int(gg), int(b), int(a)
        done += 1
    return done


def bake_prelight(model, light_model, ambient, diffuse, strength): #vers 1
    """Rewrite vertex colours of every geometry that already has them. Returns geometries baked."""
    done = 0
    for g in model.geometries:
        if not g.colors or len(g.colors) != len(g.vertices):
            continue
        for c, n in zip(g.colors, _vertex_normals(g)):
            k = max(0.0, n[0] * light_model[0] + n[1] * light_model[1] + n[2] * light_model[2])
            for ch, amb, dif in (('r', ambient[0], diffuse[0]), ('g', ambient[1], diffuse[1]),
                                 ('b', ambient[2], diffuse[2])):
                lit = min(255.0, amb + dif * k)
                old = getattr(c, ch)
                setattr(c, ch, int(round(old + (lit - old) * strength)))
        done += 1
    return done


class PrelightDialog(QDialog):
    """Ambient / sun colour, sun direction, strength."""

    def __init__(self, parent, settings, world=False): #vers 2
        super().__init__(parent)
        self.world = world
        self.setWindowTitle("Generate World Prelighting" if world else "Bake Prelight to DFF")
        self.settings = settings
        form = QFormLayout(self)
        self.amb = tuple(settings.get('prelight_ambient') or (90, 90, 100))
        self.dif = tuple(settings.get('prelight_diffuse') or (200, 190, 170))
        self.amb_btn = QPushButton(); self.dif_btn = QPushButton()
        self.amb_btn.clicked.connect(lambda: self._pick('amb'))
        self.dif_btn.clicked.connect(lambda: self._pick('dif'))
        self._paint()
        form.addRow("Ambient colour:", self.amb_btn)
        form.addRow("Sun colour:", self.dif_btn)
        self.az = QDoubleSpinBox(); self.az.setRange(0, 360); self.az.setValue(float(settings.get('prelight_azimuth') or 135))
        self.el = QDoubleSpinBox(); self.el.setRange(0, 90); self.el.setValue(float(settings.get('prelight_elevation') or 45))
        self.st = QDoubleSpinBox(); self.st.setRange(0.0, 1.0); self.st.setSingleStep(0.1)
        self.st.setValue(float(settings.get('prelight_strength') or 1.0))
        form.addRow("Sun azimuth (deg, 0 = north):", self.az)
        form.addRow("Sun elevation (deg):", self.el)
        form.addRow("Strength (0 keeps old colours):", self.st)
        if world:
            from PyQt6.QtWidgets import QCheckBox, QSpinBox
            self.sh = QCheckBox("Directional shadows")
            self.sh.setChecked(bool(settings.get('prelight_shadows') if settings.get('prelight_shadows') is not None else True))
            self.sd = QDoubleSpinBox(); self.sd.setRange(0.0, 1.0); self.sd.setSingleStep(0.1)
            self.sd.setValue(float(settings.get('prelight_shadow_dark') or 0.6))
            self.rs = QSpinBox(); self.rs.setRange(256, 4096); self.rs.setSingleStep(256)
            self.rs.setValue(int(settings.get('prelight_shadow_res') or 2048))
            form.addRow(self.sh)
            form.addRow("Shadow darkness:", self.sd)
            form.addRow("Shadow detail (map size):", self.rs)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(self.accept); bb.rejected.connect(self.reject)
        form.addRow(bb)

    def _paint(self): #vers 1
        for btn, c in ((self.amb_btn, self.amb), (self.dif_btn, self.dif)):
            btn.setText(f"{c[0]}, {c[1]}, {c[2]}")
            btn.setStyleSheet(f"background: rgb({c[0]},{c[1]},{c[2]});")

    def _pick(self, which): #vers 1
        cur = self.amb if which == 'amb' else self.dif
        c = QColorDialog.getColor(QColor(*cur), self)
        if c.isValid():
            if which == 'amb':
                self.amb = (c.red(), c.green(), c.blue())
            else:
                self.dif = (c.red(), c.green(), c.blue())
            self._paint()

    def values(self): #vers 2
        """(ambient rgb, sun rgb, azimuth, elevation, strength[, shadows, darkness, res]); saved."""
        v = (self.amb, self.dif, self.az.value(), self.el.value(), self.st.value())
        keys = ('prelight_ambient', 'prelight_diffuse', 'prelight_azimuth',
                'prelight_elevation', 'prelight_strength')
        if self.world:
            v += (self.sh.isChecked(), self.sd.value(), self.rs.value())
            keys += ('prelight_shadows', 'prelight_shadow_dark', 'prelight_shadow_res')
        for k, x in zip(keys, v):
            self.settings.set(k, x)
        return v
