#this belongs in apps/components/DP5_Workshop/depends/dp5_perspective.py - Version: 1
# X-Seti - November 14 2026 - IMG Factory 1.6 - DP5 perspective: turn the open area on three axes

"""
Perspective for the open area, like Deluxe Paint's rotate-on-three-axes brush effect:
the open area is lifted as a brush, turned in X, Y and Z with perspective depth,
and put back centred where it was. Live preview, OK is one undo step.
"""

##Methods list -
# perspective_brush
# stamp_over

##class PerspectiveDialog: -
# __init__
# _apply
# _schedule
# accept
# reject

import numpy as np
from PyQt6.QtCore import Qt, QTimer
from PyQt6.QtWidgets import (QDialog, QFormLayout, QHBoxLayout, QLabel, QPushButton, QSlider, QVBoxLayout)


def perspective_brush(brush, rx: float, ry: float, rz: float, depth: float): #vers 1
    """(out h x w x 4 uint8, centre offset) of brush turned rx, ry, rz degrees; depth = camera distance in brush sizes."""
    bh, bw = brush.shape[:2]
    hw, hh = bw / 2.0, bh / 2.0
    ax, ay, az = np.radians([rx, ry, rz])
    cx, sx, cy, sy, cz, sz = np.cos(ax), np.sin(ax), np.cos(ay), np.sin(ay), np.cos(az), np.sin(az)
    Rx = np.array([[1, 0, 0], [0, cx, -sx], [0, sx, cx]])
    Ry = np.array([[cy, 0, sy], [0, 1, 0], [-sy, 0, cy]])
    Rz = np.array([[cz, -sz, 0], [sz, cz, 0], [0, 0, 1]])
    R = Rz @ Ry @ Rx
    D = max(bw, bh) * max(depth, 1.2)
    src = np.array([(-hw, -hh), (hw, -hh), (hw, hh), (-hw, hh)], np.float64)
    dst = []
    for x, y in src:
        p = R @ np.array([x, y, 0.0])
        k = D / (D + p[2])
        dst.append((p[0] * k, p[1] * k))
    dst = np.array(dst)
    x0, y0 = np.floor(dst.min(0)).astype(int)
    x1, y1 = np.ceil(dst.max(0)).astype(int)
    ow, oh = max(1, x1 - x0), max(1, y1 - y0)
    A, b = [], []
    for (u, v), (x, y) in zip(dst, src):                 # homography dest -> src
        A.append([u, v, 1, 0, 0, 0, -x * u, -x * v]); b.append(x)
        A.append([0, 0, 0, u, v, 1, -y * u, -y * v]); b.append(y)
    try:
        hmat = np.linalg.solve(np.array(A, np.float64), np.array(b, np.float64))
    except np.linalg.LinAlgError:
        return brush.copy(), (0, 0)
    H = np.append(hmat, 1.0).reshape(3, 3)
    ys, xs = np.mgrid[0:oh, 0:ow]
    u, v = xs + x0 + 0.5, ys + y0 + 0.5
    den = H[2, 0] * u + H[2, 1] * v + H[2, 2]
    sxp = (H[0, 0] * u + H[0, 1] * v + H[0, 2]) / den + hw
    syp = (H[1, 0] * u + H[1, 1] * v + H[1, 2]) / den + hh
    ix, iy = np.floor(sxp).astype(int), np.floor(syp).astype(int)
    ok = (ix >= 0) & (ix < bw) & (iy >= 0) & (iy < bh) & (den > 0)
    out = np.zeros((oh, ow, 4), np.uint8)
    out[ok] = brush[iy[ok], ix[ok]]
    return out, (int(x0 + hw), int(y0 + hh))


def stamp_over(canvas, brush, x: int, y: int): #vers 1
    """Source-over brush onto canvas h x w x 4 at (x, y), clipped; changes canvas."""
    h, w = canvas.shape[:2]
    bh, bw = brush.shape[:2]
    sx0, sy0 = max(0, -x), max(0, -y)
    ex, ey = min(bw, w - x), min(bh, h - y)
    if ex <= sx0 or ey <= sy0:
        return
    s = brush[sy0:ey, sx0:ex].astype(np.float64)
    d = canvas[y + sy0:y + ey, x + sx0:x + ex].astype(np.float64)
    sa, da = s[..., 3:4] / 255.0, d[..., 3:4] / 255.0
    oa = sa + da * (1 - sa)
    rgb = np.where(oa > 0, (s[..., :3] * sa + d[..., :3] * da * (1 - sa)) / np.maximum(oa, 1e-9), 0)
    canvas[y + sy0:y + ey, x + sx0:x + ex] = np.concatenate([rgb, oa * 255], axis=2).round().astype(np.uint8)


class PerspectiveDialog(QDialog): #vers 1
    """Turn the open area on X, Y and Z with perspective; live preview."""

    def __init__(self, editor, canvas, mask): #vers 1
        super().__init__(editor)
        self.setWindowTitle("Perspective")
        self._ed, self._c, self._mask = editor, canvas, mask
        self._orig = bytes(canvas.rgba)
        a = np.frombuffer(self._orig, np.uint8).reshape(canvas.tex_h, canvas.tex_w, 4)
        ys, xs = np.nonzero(mask)
        self._y0, self._x0 = int(ys.min()), int(xs.min())
        y1, x1 = int(ys.max()) + 1, int(xs.max()) + 1
        self._brush = a[self._y0:y1, self._x0:x1].copy()
        self._brush[~mask[self._y0:y1, self._x0:x1]] = 0
        self._timer = QTimer(self)
        self._timer.setSingleShot(True)
        self._timer.setInterval(60)
        self._timer.timeout.connect(self._apply)
        lay = QVBoxLayout(self)
        lay.addWidget(QLabel("Turn the open area in space; the original is removed."))
        form = QFormLayout()
        self._sl = {}
        for key, label, lo, hi, val in (('rx', "Tilt (X axis)", -80, 80, 0), ('ry', "Turn (Y axis)", -80, 80, 0),
                                        ('rz', "Roll (Z axis)", -180, 180, 0), ('d', "Perspective depth", 12, 80, 30)):
            row = QHBoxLayout()
            s = QSlider(Qt.Orientation.Horizontal)
            s.setRange(lo, hi)
            s.setValue(val)
            lab = QLabel(str(val))
            lab.setMinimumWidth(36)
            s.valueChanged.connect(lambda v, l=lab: l.setText(str(v)))
            s.valueChanged.connect(self._schedule)
            row.addWidget(s, 1)
            row.addWidget(lab)
            form.addRow(label, row)
            self._sl[key] = s
        lay.addLayout(form)
        btns = QHBoxLayout()
        ok, no = QPushButton("OK"), QPushButton("Cancel")
        ok.clicked.connect(self.accept)
        no.clicked.connect(self.reject)
        btns.addStretch(1)
        btns.addWidget(ok)
        btns.addWidget(no)
        lay.addLayout(btns)
        self._apply()

    def _schedule(self, *_): #vers 1
        self._timer.start()

    def _apply(self): #vers 1
        c = self._c
        a = np.frombuffer(self._orig, np.uint8).reshape(c.tex_h, c.tex_w, 4).copy()
        a[self._mask] = 0
        out, (cx, cy) = perspective_brush(self._brush, self._sl['rx'].value(), self._sl['ry'].value(),
                                          self._sl['rz'].value(), self._sl['d'].value() / 10.0)
        stamp_over(a, out, self._x0 + cx, self._y0 + cy)
        c.rgba[:] = a.tobytes()
        c.update()

    def accept(self): #vers 1
        self._timer.stop()
        self._apply()
        final = bytes(self._c.rgba)
        self._c.rgba[:] = self._orig
        self._ed._push_undo()
        self._c.rgba[:] = final
        self._c.update()
        self._ed._set_status("Perspective applied")
        super().accept()

    def reject(self): #vers 1
        self._timer.stop()
        self._c.rgba[:] = self._orig
        self._c.update()
        super().reject()
