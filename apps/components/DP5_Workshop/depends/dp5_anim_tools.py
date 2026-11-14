#this belongs in apps/components/DP5_Workshop/depends/dp5_anim_tools.py - Version: 2
# X-Seti - November 14 2026 - IMG Factory 1.6 - DP5 animation tools: delays, cycling, move brush, camera, anim brush, palettes

"""
Animation helpers for DP5 frames (lists of RGBA bytearrays): per-frame delays,
colour cycling over a range of colours, moving a brush across frames with fade,
camera moves, anim brushes and per-frame palettes (remap, fade).
All work stays inside the open area (drape) of the current frame.
"""

##Methods list -
# camera_frames
# cycle_frames
# fade_frames
# grab_anim_brush
# lift_brush
# move_frames
# range_from_area
# remap_frame
# stamp_anim_brush

##class AnimBrushDialog: -
# __init__
# accept
##class CameraDialog: -
# __init__
# accept
##class CycleDialog: -
# __init__
# _build
# accept
##class FrameDelayDialog: -
# __init__
# accept
##class MoveBrushDialog: -
# __init__
# accept
##class PaletteDialog: -
# __init__
# accept

import numpy as np
from PyQt6.QtWidgets import (QCheckBox, QComboBox, QDialog, QDoubleSpinBox, QFormLayout, QHBoxLayout,
                             QLabel, QPushButton, QSpinBox, QVBoxLayout)


def range_from_area(rgba, w: int, h: int, mask, limit: int = 256): #vers 1
    """Unique opaque colours inside mask, dark to light; None when over limit."""
    a = np.frombuffer(bytes(rgba), np.uint8).reshape(h, w, 4)
    idx = mask & (a[..., 3] > 0)
    px = a[idx][:, :3]
    if not len(px):
        return []
    u = np.unique(px, axis=0)
    if len(u) > limit:
        return None
    lum = 0.3 * u[:, 0] + 0.59 * u[:, 1] + 0.11 * u[:, 2]
    return [tuple(int(v) for v in c) for c in u[np.argsort(lum)]]


def cycle_frames(rgba, w: int, h: int, mask, colours, count: int, back: bool = False) -> list: #vers 1
    """count frames; frame k has range colours rotated k steps inside mask (exact matches)."""
    base = np.frombuffer(bytes(rgba), np.uint8).reshape(h, w, 4)
    n = len(colours)
    pal = np.array(colours, np.uint8)
    flat = base[..., :3].reshape(-1, 3).astype(np.int32)
    code = flat[:, 0] * 65536 + flat[:, 1] * 256 + flat[:, 2]
    keys = pal[:, 0].astype(np.int32) * 65536 + pal[:, 1].astype(np.int32) * 256 + pal[:, 2]
    lookup = {int(k): i for i, k in enumerate(keys)}
    pos = np.full(len(code), -1, np.int32)
    for k, i in lookup.items():
        pos[code == k] = i
    pos[~mask.reshape(-1)] = -1
    hit = pos >= 0
    out = []
    for step in range(count):
        s = -step if back else step
        f = base.copy().reshape(-1, 4)
        f[hit, :3] = pal[(pos[hit] + s) % n]
        out.append(bytearray(f.tobytes()))
    return out


def lift_brush(rgba, w: int, h: int, mask): #vers 1
    """(brush h x w x 4 uint8 cut to mask, x0, y0); None when mask empty."""
    ys, xs = np.nonzero(mask)
    if not len(ys):
        return None
    a = np.frombuffer(bytes(rgba), np.uint8).reshape(h, w, 4)
    y0, y1, x0, x1 = ys.min(), ys.max() + 1, xs.min(), xs.max() + 1
    b = a[y0:y1, x0:x1].copy()
    b[~mask[y0:y1, x0:x1]] = 0
    return b, int(x0), int(y0)


def move_frames(frames, w: int, h: int, mask, cur: int, idxs, dx: float, dy: float,
                fade0: float, fade1: float, lift: bool) -> int: #vers 1
    """Stamp the current frame's open-area brush on frames idxs, moving by (dx, dy) with fade. Returns count."""
    got = lift_brush(frames[cur], w, h, mask)
    if got is None or len(idxs) < 1:
        return 0
    brush, x0, y0 = got
    bh, bw = brush.shape[:2]
    n = len(idxs)
    for k, i in enumerate(idxs):
        t = k / (n - 1) if n > 1 else 0.0
        a = np.frombuffer(bytes(frames[i]), np.uint8).reshape(h, w, 4).copy()
        if lift:
            a[mask] = 0
        ox, oy = int(round(x0 + dx * t)), int(round(y0 + dy * t))
        sx0, sy0 = max(0, -ox), max(0, -oy)
        ex, ey = min(bw, w - ox), min(bh, h - oy)
        if ex > sx0 and ey > sy0:
            src = brush[sy0:ey, sx0:ex].astype(np.float64)
            dst = a[oy + sy0:oy + ey, ox + sx0:ox + ex].astype(np.float64)
            sa = src[..., 3:4] / 255.0 * (fade0 + (fade1 - fade0) * t)
            da = dst[..., 3:4] / 255.0
            oa = sa + da * (1 - sa)
            rgb = np.where(oa > 0, (src[..., :3] * sa + dst[..., :3] * da * (1 - sa)) / np.maximum(oa, 1e-9), 0)
            a[oy + sy0:oy + ey, ox + sx0:ox + ex] = np.concatenate([rgb, oa * 255], axis=2).round().astype(np.uint8)
        frames[i] = bytearray(a.tobytes())
    return n


def camera_frames(frames, w: int, h: int, src, idxs, dx: float, dy: float, z0: float, z1: float, wrap: bool) -> int: #vers 1
    """Frames idxs become views of src scrolled by (dx, dy) and zoomed z0 to z1 over the run."""
    a = np.frombuffer(bytes(src), np.uint8).reshape(h, w, 4)
    ys, xs = np.mgrid[0:h, 0:w]
    n = len(idxs)
    for k, i in enumerate(idxs):
        t = k / (n - 1) if n > 1 else 0.0
        z = max(0.05, z0 + (z1 - z0) * t)
        sx = ((xs + 0.5 - w / 2.0) / z + w / 2.0 + dx * t).astype(int)
        sy = ((ys + 0.5 - h / 2.0) / z + h / 2.0 + dy * t).astype(int)
        if wrap:
            out = a[sy % h, sx % w]
        else:
            ok = (sx >= 0) & (sx < w) & (sy >= 0) & (sy < h)
            out = np.zeros_like(a)
            out[ok] = a[sy[ok], sx[ok]]
        frames[i] = bytearray(out.tobytes())
    return n


def grab_anim_brush(frames, w: int, h: int, mask, idxs): #vers 1
    """(list of brush arrays, x0, y0): the mask box cut from each frame in idxs."""
    ys, xs = np.nonzero(mask)
    if not len(ys):
        return None
    y0, y1, x0, x1 = ys.min(), ys.max() + 1, xs.min(), xs.max() + 1
    out = []
    for i in idxs:
        a = np.frombuffer(bytes(frames[i]), np.uint8).reshape(h, w, 4)
        b = a[y0:y1, x0:x1].copy()
        b[~mask[y0:y1, x0:x1]] = 0
        out.append(b)
    return out, int(x0), int(y0)


def stamp_anim_brush(frames, w: int, h: int, brush, idxs, x: int, y: int) -> int: #vers 1
    """Frame k of idxs gets brush frame k (looping) stamped at (x, y)."""
    from apps.components.DP5_Workshop.depends.dp5_perspective import stamp_over
    for k, i in enumerate(idxs):
        a = np.frombuffer(bytes(frames[i]), np.uint8).reshape(h, w, 4).copy()
        stamp_over(a, brush[k % len(brush)], x, y)
        frames[i] = bytearray(a.tobytes())
    return len(idxs)


def remap_frame(rgba, w: int, h: int, pal, mask) -> bytearray: #vers 1
    """Opaque pixels inside mask take the nearest colour of pal (n x 3)."""
    a = np.frombuffer(bytes(rgba), np.uint8).reshape(h, w, 4).copy()
    idx = mask & (a[..., 3] > 0)
    px = a[idx][:, :3].astype(np.int32)
    if len(px):
        p = np.asarray(pal, np.int32)
        best = np.empty(len(px), np.int32)
        for s in range(0, len(px), 20000):
            d = ((px[s:s + 20000, None, :] - p[None, :, :]) ** 2).sum(axis=2)
            best[s:s + 20000] = d.argmin(axis=1)
        a[idx, :3] = p[best].astype(np.uint8)
    return bytearray(a.tobytes())


def fade_frames(frames, w: int, h: int, idxs, colour, p0: float, p1: float, mask) -> int: #vers 1
    """Blend opaque pixels inside mask toward colour: p0 on the first frame to p1 on the last (0-1)."""
    n = len(idxs)
    col = np.array(colour[:3], np.float64)
    for k, i in enumerate(idxs):
        t = k / (n - 1) if n > 1 else 0.0
        amt = p0 + (p1 - p0) * t
        a = np.frombuffer(bytes(frames[i]), np.uint8).reshape(h, w, 4).copy()
        idx = mask & (a[..., 3] > 0)
        a[idx, :3] = np.rint(a[idx, :3] * (1 - amt) + col * amt).astype(np.uint8)
        frames[i] = bytearray(a.tobytes())
    return n


class FrameDelayDialog(QDialog): #vers 1
    """Set the delay of the selected frames (or all)."""

    def __init__(self, editor, idxs): #vers 1
        super().__init__(editor)
        self.setWindowTitle("Frame delays")
        self._ed, self._idxs = editor, idxs
        lay = QVBoxLayout(self)
        lay.addWidget(QLabel(f"{len(idxs)} frame(s): " + ", ".join(str(i + 1) for i in idxs[:12]) + (" ..." if len(idxs) > 12 else "")))
        form = QFormLayout()
        self._ms = QSpinBox()
        self._ms.setRange(10, 60000)
        self._ms.setSuffix(" ms")
        cur = editor._frame_delays[idxs[0]] if idxs[0] < len(editor._frame_delays) else 100
        self._ms.setValue(int(cur))
        form.addRow("Delay", self._ms)
        lay.addLayout(form)
        row = QHBoxLayout()
        ok, no = QPushButton("OK"), QPushButton("Cancel")
        ok.clicked.connect(self.accept)
        no.clicked.connect(self.reject)
        row.addStretch(1)
        row.addWidget(ok)
        row.addWidget(no)
        lay.addLayout(row)

    def accept(self): #vers 1
        ed = self._ed
        n = len(ed._frames)
        d = list(ed._frame_delays) + [self._ms.value()] * (n - len(ed._frame_delays))
        for i in self._idxs:
            d[i] = self._ms.value()
        ed._frame_delays = d[:n]
        ed._set_status(f"Delay {self._ms.value()} ms on {len(self._idxs)} frame(s)")
        super().accept()


class CycleDialog(QDialog): #vers 1
    """Colour cycling: the colours in the open area rotate through the range, one frame per step."""

    def __init__(self, editor, colours): #vers 1
        super().__init__(editor)
        self.setWindowTitle("Colour cycle")
        self._ed, self._cols = editor, colours
        lay = QVBoxLayout(self)
        lay.addWidget(QLabel(f"Range: {len(colours)} colours in the open area, dark to light"))
        form = QFormLayout()
        self._count = QSpinBox()
        self._count.setRange(2, 256)
        self._count.setValue(min(len(colours), 32))
        form.addRow("Frames", self._count)
        self._dir = QComboBox()
        self._dir.addItem("Forward", False)
        self._dir.addItem("Backward", True)
        form.addRow("Direction", self._dir)
        self._ms = QSpinBox()
        self._ms.setRange(10, 2000)
        self._ms.setSuffix(" ms")
        self._ms.setValue(80)
        form.addRow("Frame delay", self._ms)
        lay.addLayout(form)
        row = QHBoxLayout()
        ok, no = QPushButton("Make frames"), QPushButton("Cancel")
        ok.clicked.connect(self.accept)
        no.clicked.connect(self.reject)
        row.addStretch(1)
        row.addWidget(ok)
        row.addWidget(no)
        lay.addLayout(row)

    def accept(self): #vers 1
        ed = self._ed
        c = ed.dp5_canvas
        mask, _t = c.recolour_mask()
        ed._anim_save_current_frame()
        frames = cycle_frames(c.rgba, c.tex_w, c.tex_h, mask, self._cols, self._count.value(), self._dir.currentData())
        ed._frames_backup = ([bytearray(f) for f in ed._frames], c.tex_w, c.tex_h)
        ed._frames = frames
        ed._frame_delays = [self._ms.value()] * len(frames)
        ed._current_frame = 0
        c._prot_key = None
        c.rgba[:] = frames[0]
        c.update()
        ed._anim_refresh_thumbs()
        ed._set_status(f"Cycle: {len(frames)} frames (File > Animation > Revert clean-up undoes)")
        super().accept()


class MoveBrushDialog(QDialog): #vers 1
    """Move the open-area brush across the selected frames, with fade."""

    def __init__(self, editor, idxs, cur): #vers 1
        super().__init__(editor)
        self.setWindowTitle("Move brush across frames")
        self._ed, self._idxs, self._cur = editor, idxs, cur
        lay = QVBoxLayout(self)
        lay.addWidget(QLabel(f"{len(idxs)} frame(s), first to last; brush = open area of frame {cur + 1}"))
        form = QFormLayout()
        self._dx, self._dy = QSpinBox(), QSpinBox()
        for s in (self._dx, self._dy):
            s.setRange(-4096, 4096)
        form.addRow("Move X (pixels)", self._dx)
        form.addRow("Move Y (pixels)", self._dy)
        self._f0, self._f1 = QSpinBox(), QSpinBox()
        for s, v in ((self._f0, 100), (self._f1, 100)):
            s.setRange(0, 100)
            s.setSuffix(" %")
            s.setValue(v)
        form.addRow("Opacity at start", self._f0)
        form.addRow("Opacity at end", self._f1)
        self._lift = QCheckBox("Remove the original from every frame")
        self._lift.setChecked(True)
        form.addRow(self._lift)
        lay.addLayout(form)
        row = QHBoxLayout()
        ok, no = QPushButton("OK"), QPushButton("Cancel")
        ok.clicked.connect(self.accept)
        no.clicked.connect(self.reject)
        row.addStretch(1)
        row.addWidget(ok)
        row.addWidget(no)
        lay.addLayout(row)

    def accept(self): #vers 1
        ed = self._ed
        c = ed.dp5_canvas
        mask, _t = c.recolour_mask()
        ed._anim_save_current_frame()
        ed._frames_backup = ([bytearray(f) for f in ed._frames], c.tex_w, c.tex_h)
        n = move_frames(ed._frames, c.tex_w, c.tex_h, mask, self._cur, self._idxs, self._dx.value(), self._dy.value(),
                        self._f0.value() / 100.0, self._f1.value() / 100.0, self._lift.isChecked())
        c._prot_key = None
        c.rgba[:] = ed._frames[ed._current_frame]
        c.update()
        ed._anim_refresh_thumbs()
        ed._set_status(f"Moved brush across {n} frame(s) (File > Animation > Revert clean-up undoes)")
        super().accept()


def _row(lay, ok_text, ok_fn, no_fn): #vers 1
    row = QHBoxLayout()
    ok, no = QPushButton(ok_text), QPushButton("Cancel")
    ok.clicked.connect(ok_fn)
    no.clicked.connect(no_fn)
    row.addStretch(1)
    row.addWidget(ok)
    row.addWidget(no)
    lay.addLayout(row)


def _backup(ed, c): #vers 1
    ed._frames_backup = ([bytearray(f) for f in ed._frames], c.tex_w, c.tex_h)


def _reload(ed, c): #vers 1
    c._prot_key = None
    c.rgba[:] = ed._frames[ed._current_frame]
    c.update()
    ed._anim_refresh_thumbs()


class CameraDialog(QDialog): #vers 1
    """Camera move: scroll and zoom the current frame across the selected frames."""

    def __init__(self, editor, idxs, cur): #vers 1
        super().__init__(editor)
        self.setWindowTitle("Camera move")
        self._ed, self._idxs, self._cur = editor, idxs, cur
        lay = QVBoxLayout(self)
        lay.addWidget(QLabel(f"{len(idxs)} frame(s) become views of frame {cur + 1}"))
        form = QFormLayout()
        self._dx, self._dy = QSpinBox(), QSpinBox()
        for s in (self._dx, self._dy):
            s.setRange(-4096, 4096)
        form.addRow("Scroll X (pixels)", self._dx)
        form.addRow("Scroll Y (pixels)", self._dy)
        self._z0, self._z1 = QDoubleSpinBox(), QDoubleSpinBox()
        for s in (self._z0, self._z1):
            s.setRange(0.1, 20.0)
            s.setSingleStep(0.1)
            s.setValue(1.0)
        form.addRow("Zoom at start", self._z0)
        form.addRow("Zoom at end", self._z1)
        self._wrap = QCheckBox("Wrap around (scrolling background)")
        form.addRow(self._wrap)
        lay.addLayout(form)
        _row(lay, "OK", self.accept, self.reject)

    def accept(self): #vers 1
        ed = self._ed
        c = ed.dp5_canvas
        ed._anim_save_current_frame()
        _backup(ed, c)
        src = bytes(ed._frames[self._cur])
        n = camera_frames(ed._frames, c.tex_w, c.tex_h, src, self._idxs, self._dx.value(), self._dy.value(),
                          self._z0.value(), self._z1.value(), self._wrap.isChecked())
        _reload(ed, c)
        ed._set_status(f"Camera move on {n} frame(s) (File > Animation > Revert clean-up undoes)")
        super().accept()


class AnimBrushDialog(QDialog): #vers 1
    """Paste the grabbed anim brush onto the selected frames, one brush frame per frame."""

    def __init__(self, editor, idxs): #vers 1
        super().__init__(editor)
        self.setWindowTitle("Paste anim brush")
        self._ed, self._idxs = editor, idxs
        br, x0, y0 = editor._anim_brush
        lay = QVBoxLayout(self)
        lay.addWidget(QLabel(f"Anim brush: {len(br)} frame(s), {br[0].shape[1]} x {br[0].shape[0]}; onto {len(idxs)} frame(s)"))
        form = QFormLayout()
        c = editor.dp5_canvas
        self._x, self._y = QSpinBox(), QSpinBox()
        self._x.setRange(-4096, 4096)
        self._y.setRange(-4096, 4096)
        self._x.setValue(x0)
        self._y.setValue(y0)
        form.addRow("X", self._x)
        form.addRow("Y", self._y)
        lay.addLayout(form)
        _row(lay, "OK", self.accept, self.reject)

    def accept(self): #vers 1
        ed = self._ed
        c = ed.dp5_canvas
        ed._anim_save_current_frame()
        _backup(ed, c)
        n = stamp_anim_brush(ed._frames, c.tex_w, c.tex_h, ed._anim_brush[0], self._idxs, self._x.value(), self._y.value())
        _reload(ed, c)
        ed._set_status(f"Anim brush pasted on {n} frame(s) (File > Animation > Revert clean-up undoes)")
        super().accept()


class PaletteDialog(QDialog): #vers 1
    """Per-frame palettes: use the current frame's colours on the selected frames, or fade them to a colour."""

    def __init__(self, editor, idxs, cur): #vers 1
        super().__init__(editor)
        self.setWindowTitle("Frame palettes")
        self._ed, self._idxs, self._cur = editor, idxs, cur
        from PyQt6.QtGui import QColor
        self._col = QColor(0, 0, 0)
        lay = QVBoxLayout(self)
        lay.addWidget(QLabel(f"{len(idxs)} selected frame(s), inside the open area"))
        form = QFormLayout()
        self._mode = QComboBox()
        self._mode.addItem("Use current frame's colours (nearest)", 'remap')
        self._mode.addItem("Fade to colour", 'fade')
        form.addRow("Action", self._mode)
        self._btn = QPushButton("Fade colour…")
        self._btn.clicked.connect(self._pick)
        form.addRow("Colour", self._btn)
        self._p0, self._p1 = QSpinBox(), QSpinBox()
        for s, v in ((self._p0, 0), (self._p1, 100)):
            s.setRange(0, 100)
            s.setSuffix(" %")
            s.setValue(v)
        form.addRow("Fade at start", self._p0)
        form.addRow("Fade at end", self._p1)
        lay.addLayout(form)
        _row(lay, "OK", self.accept, self.reject)

    def _pick(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        col = QColorDialog.getColor(self._col, self, "Fade colour")
        if col.isValid():
            self._col = col
            self._btn.setStyleSheet(f"QPushButton {{ border-left: 24px solid {col.name()}; }}")

    def accept(self): #vers 1
        ed = self._ed
        c = ed.dp5_canvas
        mask, _t = c.recolour_mask()
        ed._anim_save_current_frame()
        _backup(ed, c)
        if self._mode.currentData() == 'remap':
            pal = range_from_area(ed._frames[self._cur], c.tex_w, c.tex_h, np.ones((c.tex_h, c.tex_w), bool), 256)
            if not pal:
                ed._set_status("Current frame has no usable palette (over 256 colours)")
                super().reject()
                return
            for i in self._idxs:
                if i != self._cur:
                    ed._frames[i] = remap_frame(ed._frames[i], c.tex_w, c.tex_h, pal, mask)
            n = len(self._idxs)
        else:
            n = fade_frames(ed._frames, c.tex_w, c.tex_h, self._idxs, (self._col.red(), self._col.green(), self._col.blue()),
                            self._p0.value() / 100.0, self._p1.value() / 100.0, mask)
        _reload(ed, c)
        ed._set_status(f"Frame palettes changed on {n} frame(s) (File > Animation > Revert clean-up undoes)")
        super().accept()
