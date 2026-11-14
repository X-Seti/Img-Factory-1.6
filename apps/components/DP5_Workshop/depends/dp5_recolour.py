#this belongs in apps/components/DP5_Workshop/depends/dp5_recolour.py - Version: 4
# X-Seti - November 14 2026 - IMG Factory 1.6 - DP5 recolour selection maths and dialog

"""
Recolour only the selected region: hue shift, shift to a colour, colourise
(keeps shading), blend, multiply, screen. Multiply suits white / light
objects, screen and blend suit black / dark ones, where hue shifts show nothing.
"""

##Methods list -
# _clip
# _hsv2rgb
# _lum
# _rgb2hsv
# _setlum
# colour_near
# filter_mask
# region_stats
# suggest_mode
# recolour

##class RecolourDialog: -
# __init__
# _apply
# _params
# _mode_changed
# _pick_colour
# _pick_filter
# _eff_mask
# _schedule
# accept
# reject

import numpy as np
from PyQt6.QtCore import Qt, QTimer
from PyQt6.QtGui import QColor
from PyQt6.QtWidgets import (QCheckBox, QComboBox, QColorDialog, QDialog, QFormLayout, QHBoxLayout,
                             QLabel, QPushButton, QSlider, QVBoxLayout)

MODES = [
    ('hue',      "Hue shift (slider)"),
    ('shift',    "Shift to colour (keeps variation)"),
    ('colourise', "Colourise (keeps shading)"),
    ('blend',    "Blend colour (mix)"),
    ('multiply', "Multiply (light / white objects)"),
    ('screen',   "Screen (dark / black objects)"),
]
# mode: (hue slider, saturation, lightness, default strength %)
_MODE_CTRL = {
    'hue':       (True,  True,  True,  100),
    'shift':     (False, True,  True,  100),
    'colourise': (False, True,  True,  100),
    'blend':     (False, False, False, 50),
    'multiply':  (False, False, False, 100),
    'screen':    (False, False, False, 60),
}


def _rgb2hsv(c): #vers 1
    """c: (n, 3) floats 0..1 -> h, s, v arrays (h 0..1)."""
    r, g, b = c[:, 0], c[:, 1], c[:, 2]
    mx, mn = c.max(1), c.min(1)
    d = mx - mn
    dd = np.maximum(d, 1e-9)
    s = np.where(mx > 0, d / np.maximum(mx, 1e-9), 0.0)
    h = np.zeros_like(mx)
    h = np.where(mx == r, ((g - b) / dd) % 6, h)
    h = np.where((mx == g) & (mx != r), (b - r) / dd + 2, h)
    h = np.where((mx == b) & (mx != r) & (mx != g), (r - g) / dd + 4, h)
    h = np.where(d == 0, 0.0, h) / 6.0
    return h, s, mx


def _hsv2rgb(h, s, v): #vers 1
    h6 = (h % 1.0) * 6.0
    i = np.floor(h6).astype(int) % 6
    f = h6 - np.floor(h6)
    p, q, t = v * (1 - s), v * (1 - f * s), v * (1 - (1 - f) * s)
    r = np.choose(i, [v, q, p, p, t, v])
    g = np.choose(i, [t, v, v, q, p, p])
    b = np.choose(i, [p, p, t, v, v, q])
    return np.stack([r, g, b], 1)


def _lum(c): #vers 1
    return 0.3 * c[:, 0] + 0.59 * c[:, 1] + 0.11 * c[:, 2]


def _clip(c): #vers 1
    l = _lum(c)[:, None]
    n = c.min(1)[:, None]
    c = np.where(n < 0, l + (c - l) * l / np.maximum(l - n, 1e-9), c)
    x = c.max(1)[:, None]
    return np.where(x > 1, l + (c - l) * (1 - l) / np.maximum(x - l, 1e-9), c)


def _setlum(c, l): #vers 1
    """Colour c moved to luminance l (per pixel)."""
    return _clip(c + (l - _lum(c))[:, None])


def region_stats(rgba, w: int, h: int, mask): #vers 1
    """(pixels, mean saturation, mean value) of opaque pixels in mask; None when empty."""
    a = np.frombuffer(bytes(rgba), np.uint8).reshape(h, w, 4)
    idx = mask & (a[..., 3] > 0)
    if not idx.any():
        return None
    _, s, v = _rgb2hsv(a[idx][:, :3].astype(np.float64) / 255.0)
    return int(idx.sum()), float(s.mean()), float(v.mean())


def colour_near(rgba, w: int, h: int, colour, tol: float): #vers 1
    """h x w bool: pixels within tol (0-100) of colour."""
    a = np.frombuffer(bytes(rgba), np.uint8).reshape(h, w, 4)
    d = a[..., :3].astype(np.int32) - np.array(colour[:3], np.int32)
    return (d * d).sum(axis=2) <= (tol / 100.0) ** 2 * (255 * 255 * 3)


def filter_mask(mask, rgba, w: int, h: int, how: str, colour, tol: float): #vers 1
    """mask limited by colour: how 'only' keeps pixels near colour, 'exclude' drops them."""
    if how == 'only':
        return mask & colour_near(rgba, w, h, colour, tol)
    if how == 'exclude':
        return mask & ~colour_near(rgba, w, h, colour, tol)
    return mask


def suggest_mode(stats): #vers 1
    """(mode, note) for the region: grey or very dark / light areas need blend modes."""
    if stats is None:
        return 'colourise', "No opaque pixels in the region."
    _, s, v = stats
    if v < 0.22:
        return 'screen', "Near black: hue shift cannot show. Screen or Blend lightens it to the colour."
    if v > 0.85 and s < 0.15:
        return 'multiply', "Near white: hue shift cannot show. Multiply tints it to the colour."
    if s < 0.15:
        return 'colourise', "Grey area: hue shift cannot show. Colourise or Blend adds the colour."
    return 'shift', "Coloured area: Shift to colour moves it to the target and keeps variation."


def recolour(rgba, w: int, h: int, mask, mode: str, hue: float = 0.0, sat: float = 0.0,
             light: float = 0.0, colour=(255, 0, 0), strength: float = 1.0) -> np.ndarray: #vers 2
    """New h x w x 4 uint8 with only opaque pixels inside mask changed; alpha kept.
    hue degrees, sat / light -100..100, strength 0..1."""
    a = np.frombuffer(bytes(rgba), np.uint8).reshape(h, w, 4).copy()
    idx = mask & (a[..., 3] > 0)
    if not idx.any():
        return a
    src = a[idx][:, :3].astype(np.float64) / 255.0
    tgt = np.array(colour[:3], np.float64) / 255.0
    sf, lf = sat / 100.0, light / 100.0
    if mode in ('hue', 'shift'):
        ch, cs, cv = _rgb2hsv(src)
        if mode == 'shift':
            th = _rgb2hsv(tgt[None, :])[0][0]
            ang = 2 * np.pi * ch
            sx, sy = (cs * np.sin(ang)).sum(), (cs * np.cos(ang)).sum()
            mean_h = (np.arctan2(sx, sy) / (2 * np.pi)) % 1.0 if (sx or sy) else th
            delta = th - mean_h
        else:
            delta = hue / 360.0
        res = _hsv2rgb(ch + delta, np.clip(cs * (1 + sf), 0, 1), np.clip(cv + lf, 0, 1))
    elif mode == 'colourise':
        gl = _lum(tgt[None, :])[0]
        t = np.clip(gl + (tgt - gl) * (1 + sf if sf < 0 else 1.0), 0, 1)
        res = _setlum(np.tile(t, (len(src), 1)), _lum(src))
        res = np.clip(res + lf, 0, 1)
    elif mode == 'blend':
        res = np.tile(tgt, (len(src), 1))
    elif mode == 'multiply':
        res = src * tgt
    else:                                                   # screen
        res = 1 - (1 - src) * (1 - tgt)
    out = np.clip(src * (1 - strength) + res * strength, 0, 1)
    a[idx, :3] = np.rint(out * 255).astype(np.uint8)
    return a


class RecolourDialog(QDialog): #vers 1
    """Live preview on the canvas; OK keeps it (one undo step), Cancel restores."""

    def __init__(self, editor, canvas, mask, scope_text: str): #vers 1
        super().__init__(editor)
        self.setWindowTitle("Recolour selection")
        self._ed, self._c, self._mask = editor, canvas, mask
        self._orig = bytes(canvas.rgba)
        self._colour = QColor(canvas.color)
        self._timer = QTimer(self)
        self._timer.setSingleShot(True)
        self._timer.setInterval(60)
        self._timer.timeout.connect(self._apply)

        lay = QVBoxLayout(self)
        lay.addWidget(QLabel(f"Region: {scope_text}"))
        form = QFormLayout()
        self._mode = QComboBox()
        for key, label in MODES:
            self._mode.addItem(label, key)
        form.addRow("Mode", self._mode)
        self._colour_btn = QPushButton("Pick colour…")
        self._colour_btn.clicked.connect(self._pick_colour)
        form.addRow("Colour", self._colour_btn)
        self._sl = {}
        for key, label, lo, hi, val in (('hue', "Hue", -180, 180, 0), ('sat', "Saturation", -100, 100, 0),
                                        ('light', "Lightness", -100, 100, 0), ('str', "Strength", 0, 100, 100)):
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
            self._sl[key] = (s, form.rowCount() - 1)
        lay.addLayout(form)
        self._form = form
        self._hint = QLabel()
        self._hint.setWordWrap(True)
        lay.addWidget(self._hint)
        frow = QHBoxLayout()
        self._fhow = QComboBox()
        for key, label in (('all', "Whole open area"), ('only', "Only pixels near colour"),
                           ('exclude', "Exclude pixels near colour")):
            self._fhow.addItem(label, key)
        self._fcol = QColor(255, 255, 255)
        self._fbtn = QPushButton("Pick…")
        self._fbtn.clicked.connect(self._pick_filter)
        self._ftol = QSlider(Qt.Orientation.Horizontal)
        self._ftol.setRange(0, 100)
        self._ftol.setValue(15)
        self._ftol.setToolTip("Tolerance")
        self._fhow.currentIndexChanged.connect(self._schedule)
        self._ftol.valueChanged.connect(self._schedule)
        frow.addWidget(self._fhow)
        frow.addWidget(self._fbtn)
        frow.addWidget(self._ftol, 1)
        form.addRow("Colour filter", frow)
        self._fbtn.setStyleSheet(f"QPushButton {{ border-left: 24px solid {self._fcol.name()}; }}")
        self._others = editor._anim_other_selected() if hasattr(editor, '_anim_other_selected') else []
        self._frames_cb = QCheckBox(f"Apply to {len(self._others)} other selected frame(s)")
        self._frames_cb.setChecked(True)
        self._frames_cb.setVisible(bool(self._others))
        lay.addWidget(self._frames_cb)
        btns = QHBoxLayout()
        ok, cancel = QPushButton("OK"), QPushButton("Cancel")
        ok.clicked.connect(self.accept)
        cancel.clicked.connect(self.reject)
        btns.addStretch(1)
        btns.addWidget(ok)
        btns.addWidget(cancel)
        lay.addLayout(btns)

        mode, note = suggest_mode(region_stats(canvas.rgba, canvas.tex_w, canvas.tex_h, mask))
        self._hint.setText(f"Suggested: {note}")
        self._mode.setCurrentIndex(max(0, self._mode.findData(mode)))
        self._mode.currentIndexChanged.connect(self._mode_changed)
        self._mode_changed()

    def _pick_colour(self): #vers 1
        col = QColorDialog.getColor(self._colour, self, "Recolour target colour")
        if col.isValid():
            self._colour = col
            self._colour_btn.setStyleSheet(f"QPushButton {{ border-left: 24px solid {col.name()}; }}")
            self._schedule()

    def _pick_filter(self): #vers 1
        col = QColorDialog.getColor(self._fcol, self, "Colour to keep / exclude")
        if col.isValid():
            self._fcol = col
            self._fbtn.setStyleSheet(f"QPushButton {{ border-left: 24px solid {col.name()}; }}")
            if self._fhow.currentData() == 'all':
                self._fhow.setCurrentIndex(2)
            self._schedule()

    def _eff_mask(self, rgba): #vers 1
        c = self._c
        return filter_mask(self._mask, rgba, c.tex_w, c.tex_h, self._fhow.currentData(),
                           (self._fcol.red(), self._fcol.green(), self._fcol.blue()), self._ftol.value())

    def _mode_changed(self): #vers 1
        hue_on, sat_on, light_on, strength = _MODE_CTRL[self._mode.currentData()]
        for key, on in (('hue', hue_on), ('sat', sat_on), ('light', light_on)):
            s, row = self._sl[key]
            s.setEnabled(on)
        self._sl['str'][0].setValue(strength)
        self._colour_btn.setStyleSheet(f"QPushButton {{ border-left: 24px solid {self._colour.name()}; }}")
        self._schedule()

    def _schedule(self, *_): #vers 1
        self._timer.start()

    def _apply(self): #vers 1
        c = self._c
        out = recolour(self._orig, c.tex_w, c.tex_h, self._eff_mask(self._orig), *self._params())
        c.rgba[:] = out.tobytes()
        c.update()

    def _params(self): #vers 1
        return (self._mode.currentData(), self._sl['hue'][0].value(), self._sl['sat'][0].value(),
                self._sl['light'][0].value(), (self._colour.red(), self._colour.green(), self._colour.blue()),
                self._sl['str'][0].value() / 100.0)

    def accept(self): #vers 2
        self._timer.stop()
        c = self._c
        self._apply()
        final = bytes(c.rgba)
        c.rgba[:] = self._orig                          # undo step holds the original
        self._ed._push_undo()
        c.rgba[:] = final
        c.update()
        done = 0
        if self._frames_cb.isChecked():
            for i in self._others:
                f = self._ed._frames[i]
                if len(f) == len(final):
                    self._ed._frames[i] = bytearray(recolour(f, c.tex_w, c.tex_h, self._eff_mask(f), *self._params()).tobytes())
                    done += 1
            if done:
                self._ed._anim_refresh_thumbs()
        extra = f" + {done} frame(s)" if done else ""
        self._ed._set_status(f"Recoloured {int(self._eff_mask(self._orig).sum())} pixels{extra} ({self._mode.currentText()})")
        super().accept()

    def reject(self): #vers 1
        self._timer.stop()
        self._c.rgba[:] = self._orig
        self._c.update()
        super().reject()
