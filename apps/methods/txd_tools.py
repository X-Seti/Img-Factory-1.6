"""
apps/methods/txd_tools.py  —  Texture processing tools for TXD Workshop
Build 1

Modules:
  ColourAdjustDialog   — brightness/contrast/hue/sat/sharp/opacity/premultiply/cutout
  SeamlessDialog       — wrap-blend + histogram-preserving seamless conversion
  TiledPreviewWidget   — 3x3 tiling preview toggle
  MipmapAlphaCoverage  — scale alpha for mipmap coverage (foliage/fences)
  SnowDialog           — snow effect generator
"""

from __future__ import annotations
import struct, math
from typing import Optional, Tuple
from pathlib import Path

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton, QSlider,
    QSpinBox, QDoubleSpinBox, QGroupBox, QCheckBox, QComboBox,
    QWidget, QFrame, QSplitter, QScrollArea, QApplication, QSizePolicy,
    QTabWidget, QProgressDialog,
)
from PyQt6.QtGui import QPixmap, QImage, QColor
from PyQt6.QtCore import Qt, QSize, pyqtSignal


# ── PIL import ────────────────────────────────────────────────────────────────
try:
    from PIL import Image, ImageEnhance, ImageFilter, ImageOps
    _PIL = True
except ImportError:
    _PIL = False


def _rgba_to_pil(rgba: bytes, w: int, h: int) -> Optional['Image.Image']:
    if not _PIL or not rgba or w <= 0 or h <= 0:
        return None
    try:
        return Image.frombytes('RGBA', (w, h), rgba)
    except Exception:
        return None


def _pil_to_rgba(img: 'Image.Image') -> bytes:
    return img.convert('RGBA').tobytes()


def _pil_to_qpixmap(img: 'Image.Image', max_size: int = 400) -> QPixmap:
    w, h = img.size
    scale = min(1.0, max_size / max(w, h, 1))
    if scale < 1.0:
        img = img.resize((int(w * scale), int(h * scale)), Image.LANCZOS)
    rgba = img.convert('RGBA').tobytes()
    qi = QImage(rgba, img.width, img.height, img.width * 4, QImage.Format.Format_RGBA8888)
    return QPixmap.fromImage(qi)


def rgba_to_qpixmap(rgba: bytes, w: int, h: int) -> QPixmap:
    if not rgba or w <= 0 or h <= 0:
        return QPixmap()
    qi = QImage(rgba, w, h, w * 4, QImage.Format.Format_RGBA8888)
    return QPixmap.fromImage(qi)


# =============================================================================
# Colour Adjustments Dialog
# =============================================================================

class ColourAdjustDialog(QDialog):
    """Non-destructive colour adjustment panel.
    Brightness / Contrast / Hue / Saturation / Sharpness /
    Opacity / Premultiplied Alpha / Cutout Alpha Threshold
    """

    applied = pyqtSignal(bytes)   # emits new rgba bytes

    def __init__(self, rgba: bytes, width: int, height: int,
                 tex_name: str = "", parent=None):
        super().__init__(parent)
        self.setWindowTitle(f"Colour Adjustments — {tex_name}")
        self.setMinimumSize(680, 480)
        self._orig_rgba = rgba
        self._w = width
        self._h = height
        self._live = True
        self._sliders = {}
        self._setup_ui()
        self._update_preview()

    def _setup_ui(self):
        main = QHBoxLayout(self)
        main.setSpacing(8)

        # ── Left: sliders ──────────────────────────────────────────────────
        ctrl = QWidget(); cl = QVBoxLayout(ctrl); cl.setSpacing(4)
        cl.setContentsMargins(4, 4, 4, 4)

        def _slider(label, lo, hi, default, decimals=0):
            grp = QGroupBox(label)
            gl = QHBoxLayout(grp); gl.setSpacing(4)
            sl = QSlider(Qt.Orientation.Horizontal)
            sl.setRange(lo, hi); sl.setValue(default)
            sp = QDoubleSpinBox() if decimals else QSpinBox()
            if decimals:
                sp.setDecimals(decimals)
                sp.setRange(lo / 10**decimals, hi / 10**decimals)
                sp.setValue(default / 10**decimals)
                sp.setSingleStep(0.01)
                sl.valueChanged.connect(lambda v, s=sp: s.setValue(v / 10**decimals))
                sp.valueChanged.connect(lambda v, s=sl: s.setValue(int(v * 10**decimals)))
            else:
                sp.setRange(lo, hi); sp.setValue(default)
                sl.valueChanged.connect(sp.setValue)
                sp.valueChanged.connect(sl.setValue)
            sl.setFixedHeight(20); sp.setFixedWidth(60)
            gl.addWidget(sl, 1); gl.addWidget(sp)
            sl.valueChanged.connect(lambda _: self._update_preview())
            self._sliders[label] = (sl, decimals)
            cl.addWidget(grp)
            return sl

        _slider("Brightness",  -100, 100,    0)
        _slider("Contrast",    -100, 100,    0)
        _slider("Hue",         -180, 180,    0)
        _slider("Saturation",  -100, 100,    0)
        _slider("Sharpness",      0, 200,  100)
        _slider("Opacity",        0, 100,  100)

        # Cutout alpha threshold
        grp_cut = QGroupBox("Cutout Alpha Threshold")
        gl_cut = QHBoxLayout(grp_cut)
        self._cutout_check = QCheckBox("Enable")
        self._cutout_thresh = QSpinBox()
        self._cutout_thresh.setRange(0, 255); self._cutout_thresh.setValue(128)
        self._cutout_thresh.setEnabled(False)
        self._cutout_check.toggled.connect(self._cutout_thresh.setEnabled)
        self._cutout_check.toggled.connect(lambda _: self._update_preview())
        self._cutout_thresh.valueChanged.connect(lambda _: self._update_preview())
        gl_cut.addWidget(self._cutout_check); gl_cut.addWidget(self._cutout_thresh)
        cl.addWidget(grp_cut)

        # Premultiplied alpha
        self._premultiply = QCheckBox("Premultiplied Alpha (apply to export)")
        self._premultiply.toggled.connect(lambda _: self._update_preview())
        cl.addWidget(self._premultiply)

        # Live preview toggle
        self._live_check = QCheckBox("Live preview")
        self._live_check.setChecked(True)
        self._live_check.toggled.connect(self._on_live_toggle)
        cl.addWidget(self._live_check)

        cl.addStretch()

        # Buttons
        btn_row = QHBoxLayout()
        reset_btn = QPushButton("Reset")
        reset_btn.clicked.connect(self._reset)
        apply_btn = QPushButton("Apply")
        apply_btn.setDefault(True)
        apply_btn.clicked.connect(self._apply)
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        btn_row.addWidget(reset_btn)
        btn_row.addStretch()
        btn_row.addWidget(apply_btn)
        btn_row.addWidget(cancel_btn)
        cl.addLayout(btn_row)

        ctrl.setFixedWidth(280)
        main.addWidget(ctrl)

        # ── Right: before/after preview ──────────────────────────────────
        prev = QWidget(); pl = QVBoxLayout(prev); pl.setSpacing(4)
        pl.setContentsMargins(0, 0, 0, 0)

        lbl_row = QHBoxLayout()
        lbl_row.addWidget(QLabel("Original"), 1, Qt.AlignmentFlag.AlignCenter)
        lbl_row.addWidget(QLabel("Adjusted"), 1, Qt.AlignmentFlag.AlignCenter)
        pl.addLayout(lbl_row)

        img_row = QHBoxLayout(); img_row.setSpacing(4)
        self._orig_lbl = QLabel(); self._orig_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._prev_lbl = QLabel(); self._prev_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        for lbl in (self._orig_lbl, self._prev_lbl):
            lbl.setMinimumSize(200, 200)
            lbl.setStyleSheet("background:#1a1a1a; border:1px solid #444;")
            img_row.addWidget(lbl, 1)
        pl.addLayout(img_row, 1)
        main.addWidget(prev, 1)

        # Show original
        pm = rgba_to_qpixmap(self._orig_rgba, self._w, self._h)
        if not pm.isNull():
            self._orig_lbl.setPixmap(pm.scaled(
                360, 360, Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation))

    def _get_val(self, label: str) -> float:
        sl, dec = self._sliders[label]
        return sl.value() / (10**dec) if dec else sl.value()

    def _on_live_toggle(self, checked: bool):
        self._live = checked
        if checked:
            self._update_preview()

    def _update_preview(self):
        if not self._live:
            return
        rgba = self._process()
        if rgba:
            pm = rgba_to_qpixmap(rgba, self._w, self._h)
            if not pm.isNull():
                self._prev_lbl.setPixmap(pm.scaled(
                    360, 360, Qt.AspectRatioMode.KeepAspectRatio,
                    Qt.TransformationMode.SmoothTransformation))

    def _process(self) -> Optional[bytes]:
        if not _PIL:
            return self._orig_rgba
        img = _rgba_to_pil(self._orig_rgba, self._w, self._h)
        if img is None:
            return self._orig_rgba
        try:
            # Brightness
            bv = self._get_val("Brightness")
            if bv != 0:
                rgb = img.convert('RGB')
                rgb = ImageEnhance.Brightness(rgb).enhance(1.0 + bv / 100.0)
                img.paste(rgb, mask=img.split()[3])

            # Contrast
            cv = self._get_val("Contrast")
            if cv != 0:
                rgb = img.convert('RGB')
                rgb = ImageEnhance.Contrast(rgb).enhance(1.0 + cv / 100.0)
                img.paste(rgb, mask=img.split()[3])

            # Hue + Saturation
            hv = self._get_val("Hue")
            sv = self._get_val("Saturation")
            if hv != 0 or sv != 0:
                hsv = img.convert('HSV') if hasattr(Image, 'HSV') else None
                if hsv is None:
                    import colorsys
                    arr = list(img.getdata())
                    out = []
                    for r, g, b, a in arr:
                        h, s, v = colorsys.rgb_to_hsv(r/255, g/255, b/255)
                        h = (h + hv/360.0) % 1.0
                        s = max(0.0, min(1.0, s + sv/100.0))
                        r2, g2, b2 = colorsys.hsv_to_rgb(h, s, v)
                        out.append((int(r2*255), int(g2*255), int(b2*255), a))
                    img = Image.new('RGBA', (self._w, self._h))
                    img.putdata(out)

            # Sharpness
            shv = self._get_val("Sharpness")
            if shv != 100:
                rgb = img.convert('RGB')
                rgb = ImageEnhance.Sharpness(rgb).enhance(shv / 100.0)
                img.paste(rgb, mask=img.split()[3])

            # Opacity
            opv = self._get_val("Opacity")
            if opv != 100:
                r, g, b, a = img.split()
                a = a.point(lambda x: int(x * opv / 100))
                img = Image.merge('RGBA', (r, g, b, a))

            # Cutout alpha
            if self._cutout_check.isChecked():
                t = self._cutout_thresh.value()
                r, g, b, a = img.split()
                a = a.point(lambda x: 255 if x >= t else 0)
                img = Image.merge('RGBA', (r, g, b, a))

            # Premultiplied alpha
            if self._premultiply.isChecked():
                arr = list(img.getdata())
                out = []
                for r, g, b, a in arr:
                    f = a / 255.0
                    out.append((int(r*f), int(g*f), int(b*f), a))
                img = Image.new('RGBA', (self._w, self._h))
                img.putdata(out)

            return _pil_to_rgba(img)
        except Exception as e:
            print(f"[ColourAdjust] {e}")
            return self._orig_rgba

    def _reset(self):
        for label, (sl, _) in self._sliders.items():
            defaults = {"Brightness":0,"Contrast":0,"Hue":0,"Saturation":0,
                       "Sharpness":100,"Opacity":100}
            sl.setValue(defaults.get(label, 0))
        self._cutout_check.setChecked(False)
        self._premultiply.setChecked(False)
        self._update_preview()

    def _apply(self):
        rgba = self._process()
        if rgba:
            self.applied.emit(rgba)
        self.accept()

    def get_result(self) -> Optional[bytes]:
        return self._process()


# =============================================================================
# Seamless Texture Tool
# =============================================================================

class SeamlessDialog(QDialog):
    """Convert a repeating texture to seamless using multiple methods."""

    applied = pyqtSignal(bytes)

    def __init__(self, rgba: bytes, width: int, height: int,
                 tex_name: str = "", parent=None):
        super().__init__(parent)
        self.setWindowTitle(f"Seamless Tool — {tex_name}")
        self.setMinimumSize(760, 520)
        self._orig_rgba = rgba
        self._w = width
        self._h = height
        self._result = None
        self._setup_ui()

    def _setup_ui(self):
        main = QHBoxLayout(self)

        # ── Controls ───────────────────────────────────────────────────────
        ctrl = QWidget(); cl = QVBoxLayout(ctrl)
        ctrl.setFixedWidth(260)

        mode_grp = QGroupBox("Method")
        ml = QVBoxLayout(mode_grp)
        self._mode = QComboBox()
        self._mode.addItems([
            "Wrap Blend (gradient fade)",
            "Patch / Heal (centre crop)",
            "Histogram-Preserving Blend",
            "Offset & Mirror",
        ])
        ml.addWidget(self._mode)
        cl.addWidget(mode_grp)

        blend_grp = QGroupBox("Blend Width (%)")
        bl = QHBoxLayout(blend_grp)
        self._blend_sl = QSlider(Qt.Orientation.Horizontal)
        self._blend_sl.setRange(5, 50); self._blend_sl.setValue(25)
        self._blend_sp = QSpinBox()
        self._blend_sp.setRange(5, 50); self._blend_sp.setValue(25)
        self._blend_sl.valueChanged.connect(self._blend_sp.setValue)
        self._blend_sp.valueChanged.connect(self._blend_sl.setValue)
        bl.addWidget(self._blend_sl, 1); bl.addWidget(self._blend_sp)
        cl.addWidget(blend_grp)

        cl.addWidget(QLabel("Preview mode:"))
        self._prev_mode = QComboBox()
        self._prev_mode.addItems(["1×1 (single)", "2×2 (tiled)", "3×3 (tiled)"])
        self._prev_mode.setCurrentIndex(2)
        self._prev_mode.currentIndexChanged.connect(self._update_preview)
        cl.addWidget(self._prev_mode)

        run_btn = QPushButton("Generate")
        run_btn.clicked.connect(self._run)
        cl.addWidget(run_btn)

        cl.addStretch()

        btn_row = QHBoxLayout()
        apply_btn = QPushButton("Apply")
        apply_btn.setDefault(True)
        apply_btn.clicked.connect(self._apply)
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        btn_row.addWidget(apply_btn); btn_row.addWidget(cancel_btn)
        cl.addLayout(btn_row)

        main.addWidget(ctrl)

        # ── Preview panels ─────────────────────────────────────────────────
        prev = QWidget(); pl = QVBoxLayout(prev)
        hdr = QHBoxLayout()
        hdr.addWidget(QLabel("Original (tiled)"), 1, Qt.AlignmentFlag.AlignCenter)
        hdr.addWidget(QLabel("Seamless (tiled)"), 1, Qt.AlignmentFlag.AlignCenter)
        pl.addLayout(hdr)
        img_row = QHBoxLayout()
        self._orig_lbl = QLabel(); self._orig_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._prev_lbl = QLabel(); self._prev_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        for lbl in (self._orig_lbl, self._prev_lbl):
            lbl.setMinimumSize(220, 220)
            lbl.setStyleSheet("background:#1a1a1a; border:1px solid #444;")
            img_row.addWidget(lbl, 1)
        pl.addLayout(img_row, 1)
        main.addWidget(prev, 1)

        self._update_preview()

    def _tile_pixmap(self, rgba: bytes, w: int, h: int, n: int) -> QPixmap:
        if not _PIL or not rgba:
            return QPixmap()
        img = _rgba_to_pil(rgba, w, h)
        if img is None:
            return QPixmap()
        tiled = Image.new('RGBA', (w * n, h * n))
        for y in range(n):
            for x in range(n):
                tiled.paste(img, (x * w, y * h))
        return _pil_to_qpixmap(tiled, 400)

    def _update_preview(self):
        n = [1, 2, 3][self._prev_mode.currentIndex()]
        pm_orig = self._tile_pixmap(self._orig_rgba, self._w, self._h, n)
        if not pm_orig.isNull():
            self._orig_lbl.setPixmap(pm_orig.scaled(
                380, 380, Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation))
        if self._result:
            pm = self._tile_pixmap(self._result, self._w, self._h, n)
            if not pm.isNull():
                self._prev_lbl.setPixmap(pm.scaled(
                    380, 380, Qt.AspectRatioMode.KeepAspectRatio,
                    Qt.TransformationMode.SmoothTransformation))

    def _run(self):
        if not _PIL:
            return
        method = self._mode.currentIndex()
        blend_pct = self._blend_sl.value() / 100.0
        img = _rgba_to_pil(self._orig_rgba, self._w, self._h)
        if img is None:
            return
        try:
            if method == 0:
                result = _seamless_wrap_blend(img, blend_pct)
            elif method == 1:
                result = _seamless_patch(img, blend_pct)
            elif method == 2:
                result = _seamless_histogram(img, blend_pct)
            else:
                result = _seamless_offset_mirror(img)
            self._result = _pil_to_rgba(result)
            self._update_preview()
        except Exception as e:
            print(f"[Seamless] {e}")

    def _apply(self):
        if self._result:
            self.applied.emit(self._result)
            self.accept()

    def get_result(self) -> Optional[bytes]:
        return self._result


def _seamless_wrap_blend(img: 'Image.Image', blend: float) -> 'Image.Image':
    """Gradient fade blend at edges — most universal method."""
    import numpy as np
    w, h = img.size
    arr = np.array(img, dtype=np.float32)

    bx = max(1, int(w * blend))
    by = max(1, int(h * blend))

    # Rolled (offset by half) version
    rolled = np.roll(np.roll(arr, w // 2, axis=1), h // 2, axis=0)

    # Build gradient mask
    mask = np.ones((h, w), dtype=np.float32)

    # Horizontal fade
    fade_x = np.linspace(0, 1, bx)
    mask[:, :bx]  = np.minimum(mask[:, :bx],  fade_x[np.newaxis, :])
    mask[:, -bx:] = np.minimum(mask[:, -bx:], fade_x[::-1][np.newaxis, :])

    # Vertical fade
    fade_y = np.linspace(0, 1, by)
    mask[:by, :]  = np.minimum(mask[:by, :],  fade_y[:, np.newaxis])
    mask[-by:, :] = np.minimum(mask[-by:, :], fade_y[::-1][:, np.newaxis])

    mask = mask[:, :, np.newaxis]
    out = arr * mask + rolled * (1.0 - mask)
    out = np.clip(out, 0, 255).astype(np.uint8)
    return Image.fromarray(out, 'RGBA')


def _seamless_patch(img: 'Image.Image', blend: float) -> 'Image.Image':
    """Offset image by half, then blend seams with Gaussian."""
    w, h = img.size
    import numpy as np
    arr = np.array(img, dtype=np.float32)
    rolled = np.roll(np.roll(arr, w // 2, axis=1), h // 2, axis=0)

    bx = max(2, int(w * blend // 2))
    by = max(2, int(h * blend // 2))

    result = rolled.copy()
    # Blend the seam lines (centre cross after roll = original edges)
    cx, cy = w // 2, h // 2

    # Horizontal seam
    for dy in range(-by, by + 1):
        y = (cy + dy) % h
        t = 1.0 - abs(dy) / (by + 1)
        result[y] = result[y] * (1 - t) + arr[y] * t

    # Vertical seam
    for dx in range(-bx, bx + 1):
        x = (cx + dx) % w
        t = 1.0 - abs(dx) / (bx + 1)
        result[:, x] = result[:, x] * (1 - t) + arr[:, x] * t

    out = np.clip(result, 0, 255).astype(np.uint8)
    return Image.fromarray(out, 'RGBA')


def _seamless_histogram(img: 'Image.Image', blend: float) -> 'Image.Image':
    """Histogram-preserving blend — keeps colour distribution across seam."""
    # Blend two half-offset versions with histogram matching
    w, h = img.size
    import numpy as np

    arr = np.array(img, dtype=np.float32)
    rolled = np.roll(np.roll(arr, w // 2, axis=1), h // 2, axis=0)

    # Match histogram of rolled to original per channel
    for c in range(3):
        src = rolled[:, :, c].flatten()
        ref = arr[:, :, c].flatten()
        src_sorted = np.sort(src)
        ref_sorted = np.sort(ref)
        # Map src values to ref distribution
        src_ranks = np.searchsorted(src_sorted, src)
        src_ranks = np.clip(src_ranks, 0, len(ref_sorted) - 1)
        matched = ref_sorted[src_ranks].reshape(h, w)
        rolled[:, :, c] = matched

    # Now blend at edges
    bx = max(1, int(w * blend))
    by = max(1, int(h * blend))
    mask = np.ones((h, w), dtype=np.float32)
    fx = np.linspace(0, 1, bx)
    mask[:, :bx]  = np.minimum(mask[:, :bx],  fx)
    mask[:, -bx:] = np.minimum(mask[:, -bx:], fx[::-1])
    fy = np.linspace(0, 1, by)
    mask[:by, :]  = np.minimum(mask[:by, :],  fy[:, np.newaxis])
    mask[-by:, :] = np.minimum(mask[-by:, :], fy[::-1][:, np.newaxis])
    mask = mask[:, :, np.newaxis]

    out = arr * mask + rolled * (1.0 - mask)
    out = np.clip(out, 0, 255).astype(np.uint8)
    return Image.fromarray(out, 'RGBA')


def _seamless_offset_mirror(img: 'Image.Image') -> 'Image.Image':
    """Offset + mirror — great for organic textures like grass/dirt."""
    w, h = img.size
    half_w, half_h = w // 2, h // 2

    result = Image.new('RGBA', (w, h))
    # Quadrant tiling with mirrors
    q = img.crop((0, 0, half_w, half_h))
    result.paste(q, (0, 0))
    result.paste(q.transpose(Image.FLIP_LEFT_RIGHT), (half_w, 0))
    result.paste(q.transpose(Image.FLIP_TOP_BOTTOM), (0, half_h))
    result.paste(q.transpose(Image.ROTATE_180), (half_w, half_h))
    return result


# =============================================================================
# Tiled Preview Widget
# =============================================================================

class TiledPreviewWidget(QLabel):
    """Drop-in replacement preview that can tile the texture N×N."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._rgba  = b''
        self._w = self._h = 0
        self._tile_n = 1
        self.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.setStyleSheet("background:#1a1a1a;")
        self.setMinimumSize(200, 200)

    def set_texture(self, rgba: bytes, w: int, h: int):
        self._rgba = rgba; self._w = w; self._h = h
        self._refresh()

    def set_tile(self, n: int):
        self._tile_n = max(1, min(6, n))
        self._refresh()

    def _refresh(self):
        if not self._rgba or not _PIL:
            return
        img = _rgba_to_pil(self._rgba, self._w, self._h)
        if img is None:
            return
        n = self._tile_n
        if n > 1:
            tiled = Image.new('RGBA', (self._w * n, self._h * n))
            for y in range(n):
                for x in range(n):
                    tiled.paste(img, (x * self._w, y * self._h))
            img = tiled
        sz = min(self.width() or 400, self.height() or 400, 800)
        pm = _pil_to_qpixmap(img, sz)
        if not pm.isNull():
            self.setPixmap(pm.scaled(
                self.size(),
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation))


# =============================================================================
# Mipmap Alpha Coverage
# =============================================================================

def scale_alpha_for_coverage(rgba: bytes, w: int, h: int,
                              target_coverage: float,
                              mip_level: int = 0) -> bytes:
    """Scale alpha channel so mip-level alpha coverage matches mip-0.
    Critical for SA foliage/fences: leaves disappear at distance without this.

    target_coverage: fraction of pixels that should be opaque (0..1)
                     pass the mip-0 coverage here.
    mip_level: informational only (for logging).
    """
    if not rgba or w <= 0 or h <= 0:
        return rgba
    try:
        arr = bytearray(rgba)
        n = w * h

        # Compute current coverage
        current = sum(1 for i in range(3, len(arr), 4) if arr[i] > 127) / n

        if abs(current - target_coverage) < 0.005:
            return rgba  # close enough

        # Binary search for alpha scale factor
        lo, hi = 0.0, 4.0
        for _ in range(20):
            mid = (lo + hi) / 2.0
            scaled_coverage = sum(
                1 for i in range(3, len(arr), 4)
                if min(255, int(arr[i] * mid)) > 127
            ) / n
            if scaled_coverage < target_coverage:
                lo = mid
            else:
                hi = mid

        scale = (lo + hi) / 2.0
        for i in range(3, len(arr), 4):
            arr[i] = min(255, int(arr[i] * scale))

        return bytes(arr)
    except Exception:
        return rgba


def compute_mip0_coverage(rgba: bytes, w: int, h: int,
                           threshold: int = 127) -> float:
    """Compute fraction of opaque pixels in mip-0."""
    if not rgba or w <= 0 or h <= 0:
        return 1.0
    total = w * h
    opaque = sum(1 for i in range(3, len(rgba), 4) if rgba[i] > threshold)
    return opaque / max(1, total)


# =============================================================================
# Snow Generator Dialog
# =============================================================================

class SnowDialog(QDialog):
    """Generate snow cover on top of a texture."""

    applied = pyqtSignal(bytes)

    def __init__(self, rgba: bytes, width: int, height: int,
                 tex_name: str = "", parent=None):
        super().__init__(parent)
        self.setWindowTitle(f"Snow — {tex_name}")
        self.setMinimumSize(680, 480)
        self._orig_rgba = rgba
        self._w = width; self._h = height
        self._result = None
        self._setup_ui()

    def _setup_ui(self):
        main = QHBoxLayout(self)
        ctrl = QWidget(); cl = QVBoxLayout(ctrl); ctrl.setFixedWidth(240)

        def _sl(label, lo, hi, val):
            grp = QGroupBox(label)
            gl = QHBoxLayout(grp)
            sl = QSlider(Qt.Orientation.Horizontal)
            sl.setRange(lo, hi); sl.setValue(val)
            sp = QSpinBox(); sp.setRange(lo, hi); sp.setValue(val)
            sl.valueChanged.connect(sp.setValue)
            sp.valueChanged.connect(sl.setValue)
            gl.addWidget(sl, 1); gl.addWidget(sp)
            cl.addWidget(grp)
            return sl

        self._threshold = _sl("B/W Threshold", 0, 255, 180)
        self._depth     = _sl("Surface Depth", 0, 100, 30)
        self._coverage  = _sl("Coverage %", 0, 100, 70)
        self._layers    = _sl("Layers", 1, 8, 3)

        grp_tile = QGroupBox("Tile")
        tl = QHBoxLayout(grp_tile)
        self._tile_sl = QSlider(Qt.Orientation.Horizontal)
        self._tile_sl.setRange(1, 8); self._tile_sl.setValue(2)
        self._tile_sp = QSpinBox(); self._tile_sp.setRange(1, 8); self._tile_sp.setValue(2)
        self._tile_sl.valueChanged.connect(self._tile_sp.setValue)
        self._tile_sp.valueChanged.connect(self._tile_sl.setValue)
        tl.addWidget(self._tile_sl, 1); tl.addWidget(self._tile_sp)
        cl.addWidget(grp_tile)

        run_btn = QPushButton("Generate Snow")
        run_btn.clicked.connect(self._run)
        cl.addWidget(run_btn)
        cl.addStretch()

        btn_row = QHBoxLayout()
        apply_btn = QPushButton("Apply")
        apply_btn.setDefault(True)
        apply_btn.clicked.connect(self._apply)
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        btn_row.addWidget(apply_btn); btn_row.addWidget(cancel_btn)
        cl.addLayout(btn_row)
        main.addWidget(ctrl)

        prev = QWidget(); pl = QVBoxLayout(prev)
        lbl_row = QHBoxLayout()
        lbl_row.addWidget(QLabel("Original"), 1, Qt.AlignmentFlag.AlignCenter)
        lbl_row.addWidget(QLabel("With Snow"), 1, Qt.AlignmentFlag.AlignCenter)
        pl.addLayout(lbl_row)
        img_row = QHBoxLayout()
        self._orig_lbl = QLabel(); self._orig_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._prev_lbl = QLabel(); self._prev_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        for lbl in (self._orig_lbl, self._prev_lbl):
            lbl.setMinimumSize(200, 200)
            lbl.setStyleSheet("background:#1a1a1a; border:1px solid #444;")
            img_row.addWidget(lbl, 1)
        pl.addLayout(img_row, 1)
        main.addWidget(prev, 1)

        pm = rgba_to_qpixmap(self._orig_rgba, self._w, self._h)
        if not pm.isNull():
            self._orig_lbl.setPixmap(pm.scaled(360,360,
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation))

    def _run(self):
        if not _PIL:
            return
        result = _apply_snow(
            self._orig_rgba, self._w, self._h,
            threshold=self._threshold.value(),
            depth=self._depth.value() / 100.0,
            coverage=self._coverage.value() / 100.0,
            layers=self._layers.value(),
            tile=self._tile_sl.value(),
        )
        self._result = result
        pm = rgba_to_qpixmap(result, self._w, self._h)
        if not pm.isNull():
            self._prev_lbl.setPixmap(pm.scaled(360,360,
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation))

    def _apply(self):
        if self._result:
            self.applied.emit(self._result)
            self.accept()


def _apply_snow(rgba: bytes, w: int, h: int, threshold=180, depth=0.3,
                coverage=0.7, layers=3, tile=2) -> bytes:
    """Generate layered snow accumulation on a texture."""
    if not _PIL or not rgba:
        return rgba
    import random
    img = _rgba_to_pil(rgba, w, h)
    if img is None:
        return rgba

    snow_white = (240, 248, 255, 255)

    # Greyscale luminance for brightness detection
    grey = img.convert('L')
    grey_arr = list(grey.getdata())

    # Build snow mask across multiple layers
    import numpy as np
    snow_mask = np.zeros((h, w), dtype=np.float32)

    rng = random.Random(42)
    for layer in range(layers):
        scale = tile * (layer + 1)
        # Generate noise pattern at different scales
        noise = np.zeros((h, w), dtype=np.float32)
        for y in range(h):
            for x in range(w):
                # Simple value noise
                xi, yi = (x * scale) // w, (y * scale) // h
                rng.seed(xi * 1000 + yi + layer * 100000)
                noise[y, x] = rng.random()

        # Bright areas get more snow
        lum = np.array(grey_arr, dtype=np.float32).reshape(h, w) / 255.0
        bright_bias = (lum > threshold / 255.0).astype(np.float32) * 0.5

        layer_coverage = coverage * (1.0 - layer * 0.15)
        snow_here = (noise + bright_bias) > (1.0 - layer_coverage)
        snow_mask = np.maximum(snow_mask, snow_here.astype(np.float32) * (1.0 - layer * 0.2))

    # Apply depth darkening at snow edges using simple numpy gradient
    try:
        from scipy.ndimage import sobel as _sobel
        edge_h = _sobel(snow_mask, axis=0)
        edge_v = _sobel(snow_mask, axis=1)
    except ImportError:
        # Pure numpy Sobel approximation — no extra deps
        edge_h = np.gradient(snow_mask, axis=0)
        edge_v = np.gradient(snow_mask, axis=1)
    edges = np.sqrt(edge_h**2 + edge_v**2)
    edges = np.clip(edges * 3, 0, 1)
    snow_depth = np.clip(snow_mask - edges * depth, 0, 1)

    # Composite snow over texture
    arr = np.array(img, dtype=np.float32)
    sm = snow_depth[:, :, np.newaxis]
    sw = np.array([240, 248, 255, 255], dtype=np.float32)
    out = arr * (1 - sm) + sw * sm
    out = np.clip(out, 0, 255).astype(np.uint8)
    return Image.fromarray(out, 'RGBA').tobytes()
