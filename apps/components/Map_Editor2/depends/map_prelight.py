#!/usr/bin/env python3
#this belongs in apps/components/Map_Editor/depends/map_prelight.py - Version: 1
# X-Seti - September23 2026 - IMG Factory 1.6 - Map Workshop prelight bake

"""
Prelight bake - ambient + directional light baked into a DFF's existing
vertex colour (prelit) channel, written back with dff_patch.
"""

##Methods list -
# bake_prelight
# light_vector

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

    def __init__(self, parent, settings): #vers 1
        super().__init__(parent)
        self.setWindowTitle("Bake Prelight to DFF")
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

    def values(self): #vers 1
        """(ambient rgb, sun rgb, azimuth, elevation, strength); also remembered in settings."""
        v = (self.amb, self.dif, self.az.value(), self.el.value(), self.st.value())
        for k, x in zip(('prelight_ambient', 'prelight_diffuse', 'prelight_azimuth',
                         'prelight_elevation', 'prelight_strength'), v):
            self.settings.set(k, x)
        return v
