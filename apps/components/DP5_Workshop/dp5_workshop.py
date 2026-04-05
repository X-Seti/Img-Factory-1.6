#!/usr/bin/env python3
# apps/components/DP5_Workshop/dp5_workshop.py - Version: 2 (Build 233)
# X-Seti - April 2026 - Deluxe Paint 5 Clone - Img Factory 1.6 bitmap editor.
#
# Merged from:
#   dp5_workshop.py   (v1 container skeleton)
#   dp5_functions.py  (DP5Canvas, DP5PaletteBar, DP5PaintEditor logic)
#   color_pal_presets.py (ColorPalPresetsMixin — retro palette presets)
#   dp5_workshop_concept.py (dual palette concept)
#   dp5_paint_clone.py (tool system reference)
#
# Layout (DPaint5-faithful):
#   Left:   bitmap list panel (import / export / delete)
#   Centre: menubar + zoomable scrollable DP5Canvas
#   Right:  2-col tool gadget bar (SVG icons) + brush size slider +
#           FG/BG swatches + IMAGE palette strip + USER palette (retro presets)

import os, sys, random
from collections import deque
from pathlib import Path
from typing import Optional, List, Tuple

os.environ['QT_QPA_PLATFORM'] = 'xcb'
os.environ['QSG_RHI_BACKEND'] = 'opengl'

current_dir  = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QSplitter,
    QListWidget, QListWidgetItem, QLabel, QPushButton, QFrame,
    QLineEdit, QMessageBox, QGroupBox, QComboBox,
    QSpinBox, QTabWidget, QScrollArea, QCheckBox, QDialog,
    QFormLayout, QFontComboBox, QSlider, QSizePolicy,
    QAbstractItemView, QMenu, QMenuBar, QStatusBar,
    QFileDialog, QColorDialog, QGridLayout, QInputDialog
)
from PyQt6.QtCore import Qt, QPoint, QRect, pyqtSignal, QSize, QTimer
from PyQt6.QtGui import (
    QImage, QPixmap, QPainter, QColor, QCursor, QAction,
    QMouseEvent, QWheelEvent, QFont, QIcon, QPen, QBrush,
    QPainterPath, QKeySequence
)

App_name = "DP5 Workshop"
DEBUG_STANDALONE = False

# ── Tool IDs ──────────────────────────────────────────────────────────────────
TOOL_PENCIL = 'pencil'
TOOL_FILL   = 'fill'
TOOL_SPRAY  = 'spray'
TOOL_LINE   = 'line'
TOOL_RECT   = 'rect'
TOOL_CIRCLE = 'circle'
TOOL_PICKER = 'picker'
TOOL_ERASER = 'eraser'

# ── Try importing shared infrastructure ───────────────────────────────────────
try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
    ICONS_AVAILABLE = True
except ImportError:
    ICONS_AVAILABLE = False
    class SVGIconFactory:
        def __getattr__(self, name):
            return lambda *a, **k: QIcon()
        @staticmethod
        def clear_cache(): pass
        @staticmethod
        def settings_icon(size=20, color='#ffffff'): return QIcon()
        @staticmethod
        def properties_icon(size=20, color='#ffffff'): return QIcon()
        @staticmethod
        def minimize_icon(size=20, color='#ffffff'): return QIcon()
        @staticmethod
        def maximize_icon(size=20, color='#ffffff'): return QIcon()
        @staticmethod
        def close_icon(size=20, color='#ffffff'): return QIcon()
        @staticmethod
        def ai_icon(size=20, color='#ffffff'): return QIcon()

try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    APPSETTINGS_AVAILABLE = False
    AppSettings   = None
    SettingsDialog = None


# ══════════════════════════════════════════════════════════════════════════════
#  DP5 Canvas — pixel-accurate zoomable paint surface
# ══════════════════════════════════════════════════════════════════════════════

class DP5Canvas(QWidget):
    """Zoomable pixel-accurate paint canvas (inlined from dp5_functions.py)."""

    pixel_changed = pyqtSignal(int, int)

    def __init__(self, width: int, height: int, rgba: bytearray, parent=None):
        super().__init__(parent)
        self.tex_w      = width
        self.tex_h      = height
        self.rgba       = rgba
        self.zoom       = 4
        self.offset     = QPoint(0, 0)
        self.tool       = TOOL_PENCIL
        self.color      = QColor(255, 0, 0, 255)
        self.brush_size = 1
        self.show_grid  = True
        self._drawing   = False
        self._last_pt   = None
        self._preview_start = None
        self._preview_end   = None
        self._editor    = None   # set by DP5Workshop after creation

        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self.setMinimumSize(200, 200)

    # ── Coordinate helpers ────────────────────────────────────────────────────

    def _widget_to_tex(self, p: QPoint) -> Tuple[int, int]:
        x = (p.x() + self.offset.x()) // self.zoom
        y = (p.y() + self.offset.y()) // self.zoom
        return x, y

    def _tex_to_widget(self, tx: int, ty: int) -> QPoint:
        return QPoint(tx * self.zoom - self.offset.x(),
                      ty * self.zoom - self.offset.y())

    # ── Pixel access ──────────────────────────────────────────────────────────

    def get_pixel(self, x: int, y: int) -> QColor:
        if 0 <= x < self.tex_w and 0 <= y < self.tex_h:
            i = (y * self.tex_w + x) * 4
            return QColor(self.rgba[i], self.rgba[i+1],
                          self.rgba[i+2], self.rgba[i+3])
        return QColor(0, 0, 0, 0)

    def set_pixel(self, x: int, y: int, c: QColor):
        if 0 <= x < self.tex_w and 0 <= y < self.tex_h:
            i = (y * self.tex_w + x) * 4
            self.rgba[i:i+4] = [c.red(), c.green(), c.blue(), c.alpha()]

    def set_pixel_brush(self, cx: int, cy: int, c: QColor):
        s = self.brush_size
        for dy in range(-s+1, s):
            for dx in range(-s+1, s):
                if s == 1 or (dx*dx + dy*dy) < s*s:
                    self.set_pixel(cx+dx, cy+dy, c)

    # ── Drawing ops ───────────────────────────────────────────────────────────

    def flood_fill(self, sx: int, sy: int, fill_col: QColor):
        if not (0 <= sx < self.tex_w and 0 <= sy < self.tex_h):
            return
        target = self.get_pixel(sx, sy)
        if (target.red()   == fill_col.red()  and
                target.green() == fill_col.green() and
                target.blue()  == fill_col.blue()  and
                target.alpha() == fill_col.alpha()):
            return
        stack, visited = [(sx, sy)], set()
        while stack:
            x, y = stack.pop()
            if (x, y) in visited: continue
            if not (0 <= x < self.tex_w and 0 <= y < self.tex_h): continue
            px = self.get_pixel(x, y)
            if (px.red()!=target.red() or px.green()!=target.green() or
                    px.blue()!=target.blue() or px.alpha()!=target.alpha()):
                continue
            visited.add((x, y))
            self.set_pixel(x, y, fill_col)
            stack.extend([(x+1,y),(x-1,y),(x,y+1),(x,y-1)])

    def draw_line(self, x0, y0, x1, y1, c: QColor):
        dx, dy = abs(x1-x0), abs(y1-y0)
        sx = 1 if x0 < x1 else -1
        sy = 1 if y0 < y1 else -1
        err = dx - dy
        while True:
            self.set_pixel_brush(x0, y0, c)
            if x0 == x1 and y0 == y1: break
            e2 = 2*err
            if e2 > -dy: err -= dy; x0 += sx
            if e2 <  dx: err += dx; y0 += sy

    def draw_rect(self, x0, y0, x1, y1, c: QColor):
        if x0 > x1: x0,x1 = x1,x0
        if y0 > y1: y0,y1 = y1,y0
        self.draw_line(x0,y0,x1,y0,c); self.draw_line(x1,y0,x1,y1,c)
        self.draw_line(x1,y1,x0,y1,c); self.draw_line(x0,y1,x0,y0,c)

    def draw_circle(self, cx, cy, rx, ry, c: QColor):
        x, y = 0, ry
        d1 = ry*ry - rx*rx*ry + 0.25*rx*rx
        dx, dy = 2*ry*ry*x, 2*rx*rx*y
        while dx < dy:
            for px,py in [(cx+x,cy+y),(cx-x,cy+y),(cx+x,cy-y),(cx-x,cy-y)]:
                self.set_pixel(px,py,c)
            if d1<0: dx+=2*ry*ry; d1+=dx+ry*ry; x+=1
            else: dx+=2*ry*ry; dy-=2*rx*rx; d1+=dx-dy+ry*ry; x+=1; y-=1
        d2 = ry*ry*(x+0.5)**2 + rx*rx*(y-1)**2 - rx*rx*ry*ry
        while y >= 0:
            for px,py in [(cx+x,cy+y),(cx-x,cy+y),(cx+x,cy-y),(cx-x,cy-y)]:
                self.set_pixel(px,py,c)
            if d2>0: dy-=2*rx*rx; d2+=rx*rx-dy; y-=1
            else: dx+=2*ry*ry; dy-=2*rx*rx; d2+=dx-dy+rx*rx; x+=1; y-=1

    def _do_spray(self, cx: int, cy: int):
        r = self.brush_size * 5
        for _ in range(max(1, r)):
            ddx, ddy = random.randint(-r,r), random.randint(-r,r)
            if ddx*ddx + ddy*ddy <= r*r:
                self.set_pixel(cx+ddx, cy+ddy, self.color)

    # ── Paint ─────────────────────────────────────────────────────────────────

    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, False)

        img = QImage(bytes(self.rgba), self.tex_w, self.tex_h,
                     self.tex_w * 4, QImage.Format.Format_RGBA8888)
        scaled = img.scaled(self.tex_w * self.zoom, self.tex_h * self.zoom,
                             Qt.AspectRatioMode.KeepAspectRatio,
                             Qt.TransformationMode.FastTransformation)
        painter.drawImage(-self.offset.x(), -self.offset.y(), scaled)

        if self.show_grid and self.zoom >= 4:
            pen = QPen(QColor(128, 128, 128, 60), 1)
            painter.setPen(pen)
            ox = -(self.offset.x() % self.zoom)
            oy = -(self.offset.y() % self.zoom)
            for x in range(ox, self.width(), self.zoom):
                painter.drawLine(x, 0, x, self.height())
            for y in range(oy, self.height(), self.zoom):
                painter.drawLine(0, y, self.width(), y)

        if self._preview_start and self._preview_end and self.tool in (TOOL_LINE, TOOL_RECT, TOOL_CIRCLE):
            pen = QPen(self.color, 1, Qt.PenStyle.DashLine)
            painter.setPen(pen); painter.setBrush(Qt.BrushStyle.NoBrush)
            s = self._tex_to_widget(*self._preview_start)
            e = self._tex_to_widget(*self._preview_end)
            if self.tool == TOOL_LINE:
                painter.drawLine(s.x(), s.y(), e.x(), e.y())
            elif self.tool == TOOL_RECT:
                painter.drawRect(QRect(s, e).normalized())
            elif self.tool == TOOL_CIRCLE:
                painter.drawEllipse(QRect(s, e).normalized())

    # ── Mouse events ──────────────────────────────────────────────────────────

    def mousePressEvent(self, e: QMouseEvent):
        if e.button() != Qt.MouseButton.LeftButton: return
        tx, ty = self._widget_to_tex(e.position().toPoint())
        self._drawing = True; self._last_pt = (tx, ty)

        if self.tool == TOOL_PENCIL:
            self.set_pixel_brush(tx, ty, self.color); self.update()
        elif self.tool == TOOL_ERASER:
            self.set_pixel_brush(tx, ty, QColor(0,0,0,0)); self.update()
        elif self.tool == TOOL_FILL:
            self.flood_fill(tx, ty, self.color); self.update()
        elif self.tool == TOOL_PICKER:
            c = self.get_pixel(tx, ty)
            if c.isValid():
                self.color = c
                ed = self._editor
                if ed: ed._update_color_swatches()
        elif self.tool in (TOOL_LINE, TOOL_RECT, TOOL_CIRCLE):
            self._preview_start = (tx, ty); self._preview_end = (tx, ty)
        elif self.tool == TOOL_SPRAY:
            self._do_spray(tx, ty); self.update()

    def mouseMoveEvent(self, e: QMouseEvent):
        tx, ty = self._widget_to_tex(e.position().toPoint())
        ed = self._editor
        if ed and hasattr(ed, '_update_status'):
            ed._update_status(tx, ty, self.get_pixel(tx, ty))

        if not (e.buttons() & Qt.MouseButton.LeftButton) or not self._drawing:
            return
        if self.tool == TOOL_PENCIL:
            if self._last_pt:
                self.draw_line(self._last_pt[0], self._last_pt[1], tx, ty, self.color)
            self._last_pt = (tx, ty); self.update()
        elif self.tool == TOOL_ERASER:
            self.set_pixel_brush(tx, ty, QColor(0,0,0,0))
            self._last_pt = (tx, ty); self.update()
        elif self.tool == TOOL_SPRAY:
            self._do_spray(tx, ty); self.update()
        elif self.tool in (TOOL_LINE, TOOL_RECT, TOOL_CIRCLE):
            self._preview_end = (tx, ty); self.update()

    def mouseReleaseEvent(self, e: QMouseEvent):
        if e.button() != Qt.MouseButton.LeftButton: return
        tx, ty = self._widget_to_tex(e.position().toPoint())
        if self.tool == TOOL_LINE and self._preview_start:
            self.draw_line(self._preview_start[0], self._preview_start[1], tx, ty, self.color)
        elif self.tool == TOOL_RECT and self._preview_start:
            self.draw_rect(self._preview_start[0], self._preview_start[1], tx, ty, self.color)
        elif self.tool == TOOL_CIRCLE and self._preview_start:
            rx = abs(tx - self._preview_start[0])
            ry = abs(ty - self._preview_start[1])
            self.draw_circle(self._preview_start[0], self._preview_start[1],
                             max(1,rx), max(1,ry), self.color)
        self._drawing = False; self._preview_start = self._preview_end = None
        self._last_pt = None; self.update()
        self.pixel_changed.emit(tx, ty)

    def wheelEvent(self, e: QWheelEvent):
        if e.modifiers() & Qt.KeyboardModifier.ControlModifier:
            d = e.angleDelta().y()
            if d > 0: self.zoom = min(16, self.zoom + 1)
            else:     self.zoom = max(1,  self.zoom - 1)
            ed = self._editor
            if ed and hasattr(ed, '_update_zoom_label'):
                ed._update_zoom_label()
        self.update()


# ══════════════════════════════════════════════════════════════════════════════
#  DP5 Palette Bar — vertical 24px swatch strip (right side, DP5 style)
# ══════════════════════════════════════════════════════════════════════════════

class DP5PaletteBar(QWidget):
    """Vertical colour swatch strip — IMAGE palette extracted from loaded image."""

    color_picked = pyqtSignal(QColor)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.palette  = self._default_palette()
        self.selected = 1
        self.setFixedWidth(24)
        self.setMinimumHeight(200)
        self.setToolTip("Image palette — click to select colour")

    def _default_palette(self) -> List[QColor]:
        entries = [
            (0,0,0),(255,255,255),(255,0,0),(0,255,0),(0,0,255),(255,255,0),
            (255,0,255),(0,255,255),(128,0,0),(0,128,0),(0,0,128),(128,128,0),
            (128,0,128),(0,128,128),(192,192,192),(128,128,128),
            (255,128,0),(0,255,128),(128,0,255),(255,0,128),
            (255,128,128),(128,255,128),(128,128,255),(255,255,128),
            (64,64,64),(160,160,160),(200,100,50),(100,200,50),
            (50,100,200),(200,50,100),(100,50,200),(50,200,100),
        ]
        cols = [QColor(*c) for c in entries]
        while len(cols) < 256:
            cols.append(QColor(0, 0, 0))
        return cols

    def paintEvent(self, event):
        p = QPainter(self)
        sw = self.width()
        for i, c in enumerate(self.palette):
            y = i * sw
            if y >= self.height(): break
            p.fillRect(0, y, sw, sw, c)
            if i == self.selected:
                p.setPen(QPen(QColor(255,255,255), 2))
                p.drawRect(1, y+1, sw-3, sw-3)

    def mousePressEvent(self, e: QMouseEvent):
        idx = e.position().toPoint().y() // self.width()
        if 0 <= idx < len(self.palette):
            self.selected = idx
            self.color_picked.emit(self.palette[idx])
            self.update()

    def set_palette(self, palette_data: List[Tuple]):
        self.palette = []
        for entry in palette_data[:256]:
            if isinstance(entry, QColor):
                self.palette.append(entry)
            elif len(entry) >= 3:
                self.palette.append(QColor(entry[0], entry[1], entry[2]))
        while len(self.palette) < 256:
            self.palette.append(QColor(0, 0, 0))
        self.update()

    def set_selection_by_color(self, c: QColor):
        for i, p in enumerate(self.palette):
            if p.rgb() == c.rgb():
                self.selected = i
                self.update()
                return


# ══════════════════════════════════════════════════════════════════════════════
#  User Palette Grid — retro preset palettes (below image palette)
# ══════════════════════════════════════════════════════════════════════════════

class UserPaletteGrid(QWidget):
    """Horizontal grid of colour swatches for the user/retro preset palette."""

    color_picked = pyqtSignal(QColor)

    def __init__(self, parent=None):
        super().__init__(parent)
        self._colors: List[QColor] = []
        self._cols   = 8
        self._cell   = 14   # pixels per swatch
        self._selected = -1
        self.setMinimumHeight(28)
        self.setToolTip("User palette — click to select colour")

    def set_colors(self, colors: List[QColor], cols: int = 8):
        self._colors  = list(colors)
        self._cols    = cols
        rows = max(1, (len(colors) + cols - 1) // cols)
        self.setFixedHeight(rows * self._cell + 2)
        self.update()

    def paintEvent(self, event):
        p = QPainter(self)
        c = self._cell
        for i, col in enumerate(self._colors):
            x = (i % self._cols) * c
            y = (i // self._cols) * c
            p.fillRect(x, y, c-1, c-1, col)
            if i == self._selected:
                p.setPen(QPen(QColor(255,255,255), 1))
                p.drawRect(x, y, c-2, c-2)

    def mousePressEvent(self, e: QMouseEvent):
        c  = self._cell
        col = e.position().toPoint().x() // c
        row = e.position().toPoint().y() // c
        idx = row * self._cols + col
        if 0 <= idx < len(self._colors):
            self._selected = idx
            self.color_picked.emit(self._colors[idx])
            self.update()


# ══════════════════════════════════════════════════════════════════════════════
#  Colour Picker Widget (simple fallback — no screen capture)
# ══════════════════════════════════════════════════════════════════════════════

class ColorPickerWidget(QWidget):
    """Simple colour picker — opens QColorDialog."""

    color_picked = pyqtSignal(QColor)

    def __init__(self, parent=None):
        super().__init__(parent)
        lay = QVBoxLayout(self)
        lay.setContentsMargins(2, 2, 2, 2)
        self._swatch = QFrame()
        self._swatch.setFixedHeight(28)
        self._swatch.setStyleSheet("background:#ff0000; border:1px solid #888;")
        self._color  = QColor(255, 0, 0)
        btn = QPushButton("Pick Colour…")
        btn.clicked.connect(self._pick)
        lay.addWidget(self._swatch)
        lay.addWidget(btn)

    def _pick(self):
        c = QColorDialog.getColor(self._color, self, "Pick Colour",
                                   QColorDialog.ColorDialogOption.ShowAlphaChannel)
        if c.isValid():
            self._color = c
            self._swatch.setStyleSheet(
                f"background:{c.name()}; border:1px solid #888;")
            self.color_picked.emit(c)

    def current_color(self) -> QColor:
        return self._color


# ══════════════════════════════════════════════════════════════════════════════
#  ColorPalPresetsMixin — retro palette presets (inlined from color_pal_presets.py)
# ══════════════════════════════════════════════════════════════════════════════

class ColorPalPresetsMixin:
    """Retro palette preset data and helpers — mixed into DP5Workshop."""

    def _build_retro_palettes(self):
        """Initialise the retro palette registry. Call once from __init__."""

        zx_spectrum = [
            "#000000","#0000D7","#D70000","#D700D7",
            "#00D700","#00D7D7","#D7D700","#D7D7D7",
            "#000000","#0000FF","#FF0000","#FF00FF",
            "#00FF00","#00FFFF","#FFFF00","#FFFFFF",
        ]

        commodore_64 = [
            "#000000","#FFFFFF","#813338","#75CEC8",
            "#8E3C97","#56AC4D","#2E2C9B","#EDF171",
            "#8E5029","#553800","#C46C71","#4A4A4A",
            "#7B7B7B","#A9FF9F","#706DEB","#B2B2B2",
        ]

        amstrad_cpc = [
            "#000000","#000080","#0000FF","#800000","#800080","#8000FF",
            "#FF0000","#FF0080","#FF00FF","#008000","#008080","#0080FF",
            "#808000","#808080","#8080FF","#00FF00","#00FF80","#00FFFF",
            "#FF8000","#FF8080","#FF80FF","#FFFF00","#FFFF80","#FFFFFF",
            "#008000","#00C000","#C0C000",
        ]

        amiga_default = [
            "#000000","#111111","#222222","#333333","#444444","#555555","#666666","#777777",
            "#888888","#999999","#AAAAAA","#BBBBBB","#CCCCCC","#DDDDDD","#EEEEEE","#FFFFFF",
            "#0000AA","#AA0000","#00AA00","#AAAA00","#00AAAA","#AA00AA","#AAAAAA","#FF0000",
            "#00FF00","#0000FF","#FFFF00","#00FFFF","#FF00FF","#FF8800","#FF0088","#8888FF",
        ]

        atari_800 = [
            "#000000","#404040","#6C6C6C","#909090","#B0B0B0","#C8C8C8","#DCDCDC","#ECECEC",
            "#444400","#646410","#848424","#A0A034","#B8B840","#D0D050","#E8E85C","#FCFC68",
            "#702800","#844414","#985C28","#AC783C","#BC8C4C","#CCA05C","#DCB468","#ECC878",
            "#841800","#983418","#AC502C","#C06840","#D07C50","#E09060","#F0A070","#FFB480",
            "#880000","#9C2020","#B03C3C","#C05858","#D07070","#E08888","#F0A0A0","#FFB8B8",
        ]

        atari_2600 = [
            "#000000","#404040","#6c6c6c","#909090","#b0b0b0","#c8c8c8","#dcdcdc","#ececec",
            "#444400","#646410","#848424","#a0a034","#b8b840","#d0d050","#e8e85c","#fcfc68",
            "#702800","#844414","#985c28","#ac783c","#bc8c4c","#cca05c","#dcb468","#e8cc7c",
            "#841800","#983418","#ac5030","#c06848","#d0805c","#e09470","#eca880","#fcbc94",
            "#880000","#9c2020","#b03c3c","#c05858","#d07070","#e08888","#eca0a0","#fcb4b4",
            "#78005c","#8c2074","#a03c88","#b0589c","#c070b0","#d084c0","#dc9cd0","#ecb0e0",
            "#480078","#602090","#783ca4","#8c58b8","#a070cc","#b484dc","#c49cec","#d4b0fc",
            "#140084","#302098","#4c3cac","#6858c0","#7c70d0","#9488e0","#a8a0ec","#bcb4fc",
            "#000088","#1c209c","#3840b0","#505cc0","#6874d0","#7c8ce0","#90a4ec","#a4b8fc",
            "#00187c","#1c3890","#3854a8","#5070bc","#6888cc","#7c9cdc","#90b4ec","#a4c8fc",
            "#002c5c","#1c4c78","#386890","#5084ac","#689cc0","#7cb0d0","#90c4e0","#a4d4ec",
            "#00402c","#1c5c48","#387c64","#509c80","#68b494","#7cc8a8","#90d8bc","#a4e8d0",
            "#003c00","#205c20","#407c40","#5c9c5c","#74b474","#88cc88","#9ce09c","#b0f4b0",
            "#143800","#345c1c","#507c38","#6c9850","#84b468","#9ccc7c","#b0e090","#c4f4a4",
            "#2c3000","#4c501c","#687034","#848c4c","#9ca864","#b0bc78","#c4d08c","#d8e4a0",
            "#442800","#644818","#846830","#a08444","#b8a058","#ccb46c","#e0c880","#f4dc94",
        ]

        self.retro_palettes = {
            "Amiga OCS":       (amiga_default, (4, 8)),
            "C64":             (commodore_64,  (4, 4)),
            "ZX Spectrum":     (zx_spectrum,   (4, 4)),
            "Amstrad CPC":     (amstrad_cpc,   (3, 9)),
            "Atari 800":       (atari_800,     (5, 8)),
            "Atari 2600 NTSC": (atari_2600,    (16, 8)),
        }
        self.current_retro_palette = "Amiga OCS"

    def _get_retro_colors(self, name: str) -> Tuple[List[QColor], int]:
        """Return (colors, cols) for named retro palette."""
        data, (rows, cols) = self.retro_palettes.get(
            name, (self.retro_palettes["Amiga OCS"]))
        colors = [QColor(h) for h in data]
        return colors, cols

    def _apply_retro_palette(self, name: str):
        """Load a retro preset into the user palette grid."""
        self.current_retro_palette = name
        colors, cols = self._get_retro_colors(name)
        if hasattr(self, '_user_pal_grid'):
            self._user_pal_grid.set_colors(colors, cols)
        if hasattr(self, '_retro_btn'):
            self._retro_btn.setText(f"{name} ▼")

    def _show_retro_menu(self):
        menu = QMenu(self)
        for name in self.retro_palettes:
            act = menu.addAction(name)
            act.triggered.connect(lambda _, n=name: self._apply_retro_palette(n))
        if hasattr(self, '_retro_btn'):
            menu.exec(self._retro_btn.mapToGlobal(
                self._retro_btn.rect().bottomLeft()))


# ══════════════════════════════════════════════════════════════════════════════
#  DP5Workshop — main container (DPaint5-faithful layout)
# ══════════════════════════════════════════════════════════════════════════════

class DP5Workshop(ColorPalPresetsMixin, QWidget):
    """Deluxe Paint 5 inspired bitmap editor — standalone + embeddable."""

    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    # ── Init ──────────────────────────────────────────────────────────────────

    def __init__(self, parent=None, main_window=None):
        super().__init__(parent)

        self.main_window     = main_window
        self.standalone_mode = (main_window is None)
        self.is_docked       = not self.standalone_mode

        # Fonts
        self.title_font  = QFont("Arial", 14)
        self.panel_font  = QFont("Arial", 10)
        self.button_font = QFont("Arial", 10)

        # Window chrome
        self.use_system_titlebar  = False
        self.window_always_on_top = False
        self.dragging             = False
        self.drag_position        = None
        self.resizing             = False
        self.resize_corner        = None
        self.corner_size          = 20
        self.hover_corner         = None

        # Canvas state
        self._canvas_width  = 320
        self._canvas_height = 200
        self._canvas_zoom   = 4
        self._undo_stack    = deque(maxlen=32)
        self._redo_stack    = deque(maxlen=32)
        self.dp5_canvas     = None   # set by _create_centre_panel

        # Bitmap list (left panel)
        self._bitmap_list: List[dict] = []   # [{name, rgba, w, h}]
        self._current_bitmap = -1

        # AppSettings
        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        elif APPSETTINGS_AVAILABLE:
            try:
                self.app_settings = AppSettings()
            except Exception:
                self.app_settings = None
        else:
            self.app_settings = None

        if self.app_settings and hasattr(self.app_settings, 'theme_changed'):
            self.app_settings.theme_changed.connect(self._refresh_icons)

        # Icon factory
        self.icon_factory = SVGIconFactory()

        # Retro palette data (from mixin)
        self._build_retro_palettes()

        self.setWindowTitle(App_name)
        self.resize(1400, 800)
        self.setMinimumSize(900, 560)

        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        else:
            self.setWindowFlags(Qt.WindowType.Widget)

        if parent:
            p = parent.pos()
            self.move(p.x() + 50, p.y() + 80)

        self.setup_ui()
        self._apply_theme()

    # ── UI construction ───────────────────────────────────────────────────────

    def setup_ui(self):
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(5, 5, 5, 5)
        main_layout.setSpacing(4)

        toolbar = self._create_toolbar()
        self._workshop_toolbar = toolbar
        toolbar.setVisible(self.standalone_mode)
        main_layout.addWidget(toolbar)

        splitter = QSplitter(Qt.Orientation.Horizontal)

        left   = self._create_left_panel()
        centre = self._create_centre_panel()
        right  = self._create_right_panel()

        splitter.addWidget(left)
        splitter.addWidget(centre)
        splitter.addWidget(right)
        splitter.setStretchFactor(0, 0)   # bitmap list — fixed
        splitter.setStretchFactor(1, 1)   # canvas — stretch
        splitter.setStretchFactor(2, 0)   # tools / palette — fixed

        main_layout.addWidget(splitter)

        # Status bar
        self._status_bar = QStatusBar()
        self._status_bar.setMaximumHeight(22)
        main_layout.addWidget(self._status_bar)
        self._set_status(f"Canvas: {self._canvas_width}×{self._canvas_height}")

        # Initial tool
        QTimer.singleShot(0, lambda: self._select_tool(TOOL_PENCIL))

    # ── Toolbar (standalone titlebar) ─────────────────────────────────────────

    def _create_toolbar(self):
        self.titlebar = QFrame()
        self.titlebar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.titlebar.setFixedHeight(45)
        self.titlebar.setObjectName("titlebar")
        self.titlebar.installEventFilter(self)
        self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.titlebar.setMouseTracking(True)

        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setMaximumHeight(50)

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(5)

        icon_color = self._get_icon_color()

        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(SVGIconFactory.settings_icon(20, icon_color))
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip(App_name + " Settings")
        self.settings_btn.setVisible(self.standalone_mode)
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        title_row = QHBoxLayout()
        title_row.setSpacing(6)
        ai_icon_lbl = QLabel()
        if ICONS_AVAILABLE:
            pix = SVGIconFactory.ai_icon(20, icon_color).pixmap(20, 20)
            ai_icon_lbl.setPixmap(pix)
        title_row.addWidget(ai_icon_lbl)
        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_row.addWidget(self.title_label)
        layout.addLayout(title_row)

        layout.addStretch()

        self.properties_btn = QPushButton()
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(20, icon_color))
        self.properties_btn.setIconSize(QSize(20, 20))
        self.properties_btn.setFixedSize(35, 35)
        self.properties_btn.setToolTip("Theme Settings")
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        layout.addWidget(self.properties_btn)

        if self.standalone_mode:
            for attr, icon_method, slot, tip in [
                ('minimize_btn', 'minimize_icon', self.showMinimized,    "Minimize"),
                ('maximize_btn', 'maximize_icon', self._toggle_maximize, "Maximize"),
                ('close_btn',    'close_icon',    self.close,            "Close"),
            ]:
                btn = QPushButton()
                btn.setIcon(getattr(SVGIconFactory, icon_method)(20, icon_color))
                btn.setIconSize(QSize(20, 20))
                btn.setMinimumWidth(40); btn.setMaximumWidth(40); btn.setMinimumHeight(30)
                btn.clicked.connect(slot)
                btn.setToolTip(tip)
                setattr(self, attr, btn)
                layout.addWidget(btn)

        return self.toolbar

    # ── Left panel: bitmap list ───────────────────────────────────────────────

    def _create_left_panel(self):
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(150)
        panel.setMaximumWidth(240)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(4)

        # Header
        hdr = QHBoxLayout()
        header = QLabel("Bitmaps")
        header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        hdr.addWidget(header)
        hdr.addStretch()
        layout.addLayout(hdr)

        # Bitmap list
        self._bitmap_lw = QListWidget()
        self._bitmap_lw.setObjectName("bitmap_list")
        self._bitmap_lw.currentRowChanged.connect(self._on_bitmap_selected)
        layout.addWidget(self._bitmap_lw, 1)

        # Bottom button row — define all buttons BEFORE connecting signals
        import_btn = QPushButton("Import")
        import_btn.setFont(self.button_font)
        import_btn.setToolTip("Import Bitmap")

        export_btn = QPushButton("Export")
        export_btn.setFont(self.button_font)
        export_btn.setToolTip("Export current bitmap")

        del_btn = QPushButton("Delete")
        del_btn.setFont(self.button_font)
        del_btn.setToolTip("Remove from list")

        # Connect signals after all three are defined
        import_btn.clicked.connect(self._import_bitmap)
        export_btn.clicked.connect(self._export_bitmap)
        del_btn.clicked.connect(self._delete_bitmap)

        btn_row = QHBoxLayout()
        btn_row.addWidget(import_btn)
        btn_row.addWidget(export_btn)
        btn_row.addWidget(del_btn)
        layout.addLayout(btn_row)

        return panel

    def _on_bitmap_selected(self, row: int):
        if 0 <= row < len(self._bitmap_list):
            self._current_bitmap = row
            bm = self._bitmap_list[row]
            if self.dp5_canvas:
                self.dp5_canvas.tex_w = bm['w']
                self.dp5_canvas.tex_h = bm['h']
                self.dp5_canvas.rgba  = bm['rgba']
                self.dp5_canvas.update()
                self._set_status(f"{bm['name']}  {bm['w']}×{bm['h']}")

    def _delete_bitmap(self):
        row = self._bitmap_lw.currentRow()
        if 0 <= row < len(self._bitmap_list):
            self._bitmap_list.pop(row)
            self._bitmap_lw.takeItem(row)
            if self._bitmap_list:
                self._bitmap_lw.setCurrentRow(
                    min(row, len(self._bitmap_list)-1))

    # ── Centre panel: canvas ──────────────────────────────────────────────────

    def _create_centre_panel(self):
        panel = QGroupBox("")
        layout = QVBoxLayout(panel)
        layout.setContentsMargins(2, 2, 2, 2)
        layout.setSpacing(2)

        # Menu bar
        mb = QMenuBar(panel)
        self._build_canvas_menus(mb)
        layout.addWidget(mb)

        # Canvas
        try:
            w, h = self._canvas_width, self._canvas_height
            self.canvas_rgba = bytearray(b'\x80\x80\x80\xff' * (w * h))
            self.dp5_canvas  = DP5Canvas(w, h, self.canvas_rgba, panel)
            self.dp5_canvas._editor = self
            self.dp5_canvas.pixel_changed.connect(self._on_canvas_changed)
            scroll = QScrollArea()
            scroll.setWidget(self.dp5_canvas)
            scroll.setWidgetResizable(True)
            layout.addWidget(scroll, 1)
        except Exception as e:
            err = QLabel(f"Canvas error: {e}")
            layout.addWidget(err)
            self.dp5_canvas = None

        return panel

    def _build_canvas_menus(self, mb: QMenuBar):
        # File
        fm = mb.addMenu("File")
        fm.addAction("New canvas…",    self._new_canvas)
        fm.addAction("Open image…",    self._import_bitmap)
        fm.addAction("Save as PNG…",   self._export_bitmap)
        fm.addAction("Export IFF…",    self._export_iff)
        # Edit
        em = mb.addMenu("Edit")
        em.addAction("Undo\tCtrl+Z",   self._undo_canvas)
        em.addAction("Redo\tCtrl+Y",   self._redo_canvas)
        em.addSeparator()
        em.addAction("Clear",          self._clear_canvas)
        em.addAction("Fill colour",    self._fill_canvas)
        # Picture
        pm = mb.addMenu("Picture")
        pm.addAction("Flip horizontal", self._flip_h)
        pm.addAction("Flip vertical",   self._flip_v)
        pm.addAction("Invert",          self._invert)
        pm.addAction("Brighten +25",    lambda: self._adjust(25))
        pm.addAction("Darken -25",      lambda: self._adjust(-25))
        # View
        vm = mb.addMenu("View")
        vm.addAction("Zoom in  +",  lambda: self._set_zoom(self._canvas_zoom + 1))
        vm.addAction("Zoom out -",  lambda: self._set_zoom(self._canvas_zoom - 1))
        for z in (1, 2, 4, 8, 16):
            vm.addAction(f"{z}×", lambda _, zz=z: self._set_zoom(zz))
        ga = vm.addAction("Pixel grid")
        ga.setCheckable(True); ga.setChecked(True)
        ga.triggered.connect(
            lambda v: (setattr(self.dp5_canvas, 'show_grid', v),
                       self.dp5_canvas.update()) if self.dp5_canvas else None)

    # ── Right panel: gadget bar + palettes ────────────────────────────────────

    def _create_right_panel(self):
        """Right panel: DPaint5 2-col gadget bar, FG/BG swatches, image pal, user pal."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(80)
        panel.setMaximumWidth(160)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(3, 3, 3, 3)
        layout.setSpacing(4)

        # ── 2-column tool gadget grid (DPaint5 style) ──
        gadget_grid = QGridLayout()
        gadget_grid.setSpacing(2)
        gadget_grid.setContentsMargins(0, 0, 0, 0)

        # Tool definitions: (tool_id, svg_method_or_fallback_label, tooltip, row, col)
        # SVG icons loaded via SVGIconFactory if available, else fallback text
        tools = [
            (TOOL_PENCIL, 'pencil_icon',  '✏',  'Pencil (P)',      0, 0),
            (TOOL_ERASER, 'eraser_icon',  '⬜', 'Eraser (E)',      0, 1),
            (TOOL_FILL,   'fill_icon',    '▓',  'Flood Fill (F)',   1, 0),
            (TOOL_PICKER, 'picker_icon',  '⊕',  'Colour Picker (K)',1, 1),
            (TOOL_LINE,   'line_icon',    '╱',  'Line (L)',         2, 0),
            (TOOL_RECT,   'rect_icon',    '▭',  'Rect (R)',         2, 1),
            (TOOL_CIRCLE, 'circle_icon',  '○',  'Circle (C)',       3, 0),
            (TOOL_SPRAY,  'spray_icon',   '·:', 'Spray (S)',        3, 1),
        ]

        icon_color = self._get_icon_color()
        self._tool_btns = {}
        for tool_id, svg_method, fallback, tip, row, col in tools:
            btn = QPushButton()
            btn.setFixedSize(36, 36)
            btn.setCheckable(True)
            btn.setToolTip(tip)
            # Try SVG icon first, fall back to unicode label
            if ICONS_AVAILABLE and hasattr(SVGIconFactory, svg_method):
                ico = getattr(SVGIconFactory, svg_method)(24, icon_color)
                if not ico.isNull():
                    btn.setIcon(ico)
                    btn.setIconSize(QSize(24, 24))
                else:
                    btn.setText(fallback)
            else:
                btn.setText(fallback)
                btn.setFont(QFont("Arial", 14))
            btn.clicked.connect(lambda _, t=tool_id: self._select_tool(t))
            self._tool_btns[tool_id] = btn
            gadget_grid.addWidget(btn, row, col)

        layout.addLayout(gadget_grid)

        # ── UNDO / CLR row (DPaint5 style) ──
        undo_clr = QHBoxLayout()
        undo_clr.setSpacing(2)
        undo_btn = QPushButton("UN\nDO")
        undo_btn.setFixedSize(36, 36)
        undo_btn.setFont(QFont("Arial", 7))
        undo_btn.setToolTip("Undo (Ctrl+Z)")
        undo_btn.clicked.connect(self._undo_canvas)
        clr_btn = QPushButton("CLR")
        clr_btn.setFixedSize(36, 36)
        clr_btn.setFont(QFont("Arial", 9))
        clr_btn.setToolTip("Clear canvas")
        clr_btn.clicked.connect(self._clear_canvas)
        undo_clr.addWidget(undo_btn)
        undo_clr.addWidget(clr_btn)
        layout.addLayout(undo_clr)

        # ── Brush size slider ──
        layout.addWidget(QLabel("Size"))
        self._size_sl = QSlider(Qt.Orientation.Horizontal)
        self._size_sl.setRange(1, 10)
        self._size_sl.setValue(1)
        self._size_sl.setFixedHeight(18)
        self._size_sl.valueChanged.connect(self._set_brush_size)
        layout.addWidget(self._size_sl)

        # ── Zoom label ──
        self._zoom_lbl = QLabel("4×")
        self._zoom_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._zoom_lbl.setFont(QFont("Arial", 8))
        layout.addWidget(self._zoom_lbl)

        layout.addSpacing(4)

        # ── FG / BG colour swatches ──
        swatch_lbl = QLabel("FG / BG")
        swatch_lbl.setFont(QFont("Arial", 8))
        layout.addWidget(swatch_lbl)

        swatch_row = QHBoxLayout()
        swatch_row.setSpacing(2)
        self.fg_btn = QPushButton()
        self.fg_btn.setFixedSize(34, 22)
        self.fg_btn.setToolTip("Foreground colour — click to pick")
        self.fg_btn.clicked.connect(self._pick_fg_color)
        self.bg_btn = QPushButton()
        self.bg_btn.setFixedSize(34, 22)
        self.bg_btn.setToolTip("Background colour — click to pick")
        self.bg_btn.clicked.connect(self._pick_bg_color)
        self._bg_color = QColor(0, 0, 0, 255)
        swatch_row.addWidget(self.fg_btn)
        swatch_row.addWidget(self.bg_btn)
        layout.addLayout(swatch_row)

        layout.addSpacing(4)

        # ── IMAGE palette (extracted from current image, vertical strip) ──
        img_pal_lbl = QLabel("Image Pal")
        img_pal_lbl.setFont(QFont("Arial", 8, QFont.Weight.Bold))
        layout.addWidget(img_pal_lbl)

        self.pal_bar = DP5PaletteBar(panel)
        self.pal_bar.color_picked.connect(self._on_image_palette_color)
        layout.addWidget(self.pal_bar, 1)

        layout.addSpacing(4)

        # ── USER palette (retro presets) ──
        user_pal_hdr = QHBoxLayout()
        user_pal_lbl = QLabel("User Pal")
        user_pal_lbl.setFont(QFont("Arial", 8, QFont.Weight.Bold))
        user_pal_hdr.addWidget(user_pal_lbl)

        self._retro_btn = QPushButton("Amiga OCS ▼")
        self._retro_btn.setFont(QFont("Arial", 7))
        self._retro_btn.setFixedHeight(18)
        self._retro_btn.setToolTip("Choose retro palette preset")
        self._retro_btn.clicked.connect(self._show_retro_menu)
        user_pal_hdr.addWidget(self._retro_btn)
        layout.addLayout(user_pal_hdr)

        self._user_pal_grid = UserPaletteGrid(panel)
        self._user_pal_grid.color_picked.connect(self._on_user_palette_color)
        layout.addWidget(self._user_pal_grid)

        # Load default retro palette
        self._apply_retro_palette("Amiga OCS")

        return panel

    # ── Tool / colour helpers ─────────────────────────────────────────────────

    def _select_tool(self, tool_id: str):
        if self.dp5_canvas:
            self.dp5_canvas.tool = tool_id
        for tid, btn in getattr(self, '_tool_btns', {}).items():
            btn.setChecked(tid == tool_id)

    def _set_brush_size(self, v: int):
        if self.dp5_canvas:
            self.dp5_canvas.brush_size = v

    def _pick_fg_color(self):
        if not self.dp5_canvas: return
        c = QColorDialog.getColor(self.dp5_canvas.color, self, "Foreground Colour",
                                   QColorDialog.ColorDialogOption.ShowAlphaChannel)
        if c.isValid():
            self.dp5_canvas.color = c
            self._update_color_swatches()

    def _pick_bg_color(self):
        c = QColorDialog.getColor(self._bg_color, self, "Background Colour",
                                   QColorDialog.ColorDialogOption.ShowAlphaChannel)
        if c.isValid():
            self._bg_color = c
            self._update_color_swatches()

    def _on_image_palette_color(self, c: QColor):
        if self.dp5_canvas:
            self.dp5_canvas.color = c
            self._update_color_swatches()

    def _on_user_palette_color(self, c: QColor):
        if self.dp5_canvas:
            self.dp5_canvas.color = c
            self._update_color_swatches()

    def _update_color_swatches(self):
        """Refresh FG/BG swatch buttons after colour change."""
        if not hasattr(self, 'fg_btn'): return
        if self.dp5_canvas:
            c  = self.dp5_canvas.color
            luma = (c.red()*299 + c.green()*587 + c.blue()*114) // 1000
            fg_text = "#000" if luma > 128 else "#fff"
            self.fg_btn.setStyleSheet(
                f"background:{c.name()}; color:{fg_text}; border:2px solid #888;")
            self.fg_btn.setText(c.name().upper()[-6:])
        bg = self._bg_color
        luma2 = (bg.red()*299 + bg.green()*587 + bg.blue()*114) // 1000
        bg_text = "#000" if luma2 > 128 else "#fff"
        self.bg_btn.setStyleSheet(
            f"background:{bg.name()}; color:{bg_text}; border:2px solid #888;")
        self.bg_btn.setText(bg.name().upper()[-6:])

        # Sync image palette bar selection
        if self.dp5_canvas:
            self.pal_bar.set_selection_by_color(self.dp5_canvas.color)

    # ── Canvas signal callbacks ───────────────────────────────────────────────

    def _on_canvas_changed(self, x: int, y: int):
        if self.dp5_canvas:
            self._update_status(x, y, self.dp5_canvas.get_pixel(x, y))

    def _update_status(self, x: int, y: int, colour: QColor):
        zoom = self._canvas_zoom
        tool = getattr(self.dp5_canvas, 'tool', '?') if self.dp5_canvas else '?'
        self._set_status(
            f"Pos: {x},{y}  |  "
            f"RGBA({colour.red()},{colour.green()},{colour.blue()},{colour.alpha()})  |  "
            f"Zoom: {zoom}×  |  Tool: {tool}")

    def _update_zoom_label(self):
        if self.dp5_canvas:
            self._canvas_zoom = self.dp5_canvas.zoom
        if hasattr(self, '_zoom_lbl'):
            self._zoom_lbl.setText(f"{self._canvas_zoom}×")

    # ── Canvas operations ─────────────────────────────────────────────────────

    def _push_undo(self):
        if self.dp5_canvas:
            self._undo_stack.append(bytes(self.dp5_canvas.rgba))
            self._redo_stack.clear()

    def _undo_canvas(self):
        if self.dp5_canvas and self._undo_stack:
            self._redo_stack.append(bytes(self.dp5_canvas.rgba))
            self.dp5_canvas.rgba[:] = self._undo_stack.pop()
            self.dp5_canvas.update()

    def _redo_canvas(self):
        if self.dp5_canvas and self._redo_stack:
            self._undo_stack.append(bytes(self.dp5_canvas.rgba))
            self.dp5_canvas.rgba[:] = self._redo_stack.pop()
            self.dp5_canvas.update()

    def _clear_canvas(self):
        if not self.dp5_canvas: return
        self._push_undo()
        self.dp5_canvas.rgba[:] = b'\x00' * len(self.dp5_canvas.rgba)
        self.dp5_canvas.update()

    def _fill_canvas(self):
        if not self.dp5_canvas: return
        self._push_undo()
        c = self.dp5_canvas.color
        for i in range(self._canvas_width * self._canvas_height):
            self.dp5_canvas.rgba[i*4:i*4+4] = [c.red(),c.green(),c.blue(),c.alpha()]
        self.dp5_canvas.update()

    def _set_zoom(self, z: int):
        if not self.dp5_canvas: return
        self.dp5_canvas.zoom = max(1, min(16, z))
        self._canvas_zoom    = self.dp5_canvas.zoom
        self._update_zoom_label()
        self.dp5_canvas.update()

    def _flip_h(self):
        if not self.dp5_canvas: return
        self._push_undo()
        from PIL import Image
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.dp5_canvas.rgba))
        self.dp5_canvas.rgba[:] = img.transpose(Image.Transpose.FLIP_LEFT_RIGHT).tobytes()
        self.dp5_canvas.update()

    def _flip_v(self):
        if not self.dp5_canvas: return
        self._push_undo()
        from PIL import Image
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.dp5_canvas.rgba))
        self.dp5_canvas.rgba[:] = img.transpose(Image.Transpose.FLIP_TOP_BOTTOM).tobytes()
        self.dp5_canvas.update()

    def _invert(self):
        if not self.dp5_canvas: return
        self._push_undo()
        from PIL import Image, ImageOps
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.dp5_canvas.rgba))
        r,g,b,a = img.split()
        img2 = Image.merge('RGBA', (ImageOps.invert(r), ImageOps.invert(g),
                                    ImageOps.invert(b), a))
        self.dp5_canvas.rgba[:] = img2.tobytes()
        self.dp5_canvas.update()

    def _adjust(self, delta: int):
        if not self.dp5_canvas: return
        self._push_undo()
        for i in range(0, len(self.dp5_canvas.rgba), 4):
            for j in range(3):
                self.dp5_canvas.rgba[i+j] = max(0, min(255,
                    self.dp5_canvas.rgba[i+j] + delta))
        self.dp5_canvas.update()

    def _new_canvas(self):
        w, ok1 = QInputDialog.getInt(self, "New Canvas", "Width:",  320, 8, 4096)
        if not ok1: return
        h, ok2 = QInputDialog.getInt(self, "New Canvas", "Height:", 200, 8, 4096)
        if not ok2: return
        self._canvas_width  = w
        self._canvas_height = h
        if self.dp5_canvas:
            self.dp5_canvas.tex_w = w
            self.dp5_canvas.tex_h = h
            self.dp5_canvas.rgba  = bytearray(b'\x80\x80\x80\xff' * (w * h))
            self.dp5_canvas.update()
        self._set_status(f"New canvas: {w}×{h}")

    # ── File I/O ──────────────────────────────────────────────────────────────

    def _import_bitmap(self):
        path, _ = QFileDialog.getOpenFileName(
            self, "Open Image", "",
            "Images (*.png *.bmp *.jpg *.jpeg *.iff *.lbm);;All Files (*)")
        if not path or not self.dp5_canvas: return
        try:
            from PIL import Image
            img = Image.open(path).convert('RGBA')
            w, h = img.size
            self._canvas_width  = w
            self._canvas_height = h
            new_rgba = bytearray(img.tobytes())
            self.dp5_canvas.tex_w = w
            self.dp5_canvas.tex_h = h
            self.dp5_canvas.rgba  = new_rgba
            self.dp5_canvas.update()

            # Extract palette from image and load into image palette bar
            p_img = img.quantize(colors=256)
            pal_flat = p_img.getpalette()
            palette = [(pal_flat[i*3], pal_flat[i*3+1], pal_flat[i*3+2])
                       for i in range(256)]
            self.pal_bar.set_palette(palette)

            # Add to bitmap list
            name = os.path.basename(path)
            self._bitmap_list.append({'name': name, 'rgba': new_rgba, 'w': w, 'h': h})
            self._bitmap_lw.addItem(name)
            self._bitmap_lw.setCurrentRow(len(self._bitmap_list)-1)

            self._set_status(f"Opened: {name}  {w}×{h}")
        except Exception as e:
            QMessageBox.warning(self, "Open Error", str(e))

    def _export_bitmap(self):
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Save PNG", "untitled.png", "PNG (*.png);;BMP (*.bmp)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                                  bytes(self.dp5_canvas.rgba))
            img.save(path)
            self._set_status(f"Saved: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "Save Error", str(e))

    def _export_iff(self):
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export IFF ILBM", "untitled.iff", "IFF ILBM (*.iff)")
        if not path: return
        try:
            from PIL import Image
            from apps.methods.iff_ilbm import write_iff_ilbm
            img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                                  bytes(self.dp5_canvas.rgba))
            p_img = img.quantize(colors=256)
            pal_flat = p_img.getpalette()
            palette  = [(pal_flat[i*3], pal_flat[i*3+1], pal_flat[i*3+2])
                        for i in range(256)]
            data = write_iff_ilbm(self._canvas_width, self._canvas_height,
                                  palette, bytes(p_img.tobytes()))
            open(path, 'wb').write(data)
            self._set_status(f"Exported IFF: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "IFF Export Error", str(e))

    # ── Settings / theme ──────────────────────────────────────────────────────

    def _show_workshop_settings(self):
        try:
            if self.app_settings and APPSETTINGS_AVAILABLE:
                dlg = SettingsDialog(self.app_settings, self)
                dlg.exec()
                self._apply_theme()
        except Exception as e:
            QMessageBox.warning(self, "Settings", str(e))

    def _apply_theme(self):
        try:
            app_settings = self.app_settings
            if not app_settings and self.main_window:
                app_settings = getattr(self.main_window, 'app_settings', None)
            if app_settings:
                self.setStyleSheet(app_settings.get_stylesheet())
            else:
                self.setStyleSheet("""
                    QWidget { background-color: #2b2b2b; color: #e0e0e0; }
                    QTextEdit, QListWidget { background-color: #1e1e1e; border: 1px solid #3a3a3a; }
                    QGroupBox { border: 1px solid #3a3a3a; margin-top: 6px; }
                    QPushButton {
                        background-color: #3c3f41; border: 1px solid #555;
                        color: #e0e0e0; padding: 2px 4px;
                    }
                    QPushButton:checked { background-color: #1976d2; color: #fff; }
                    QPushButton:hover   { background-color: #4a4d50; }
                """)
            self._update_color_swatches()
        except Exception as e:
            print(f"[DP5 Workshop] Theme error: {e}")

    def _refresh_icons(self):
        SVGIconFactory.clear_cache()
        color = self._get_icon_color()
        for attr, method in [
            ('settings_btn',    'settings_icon'),
            ('properties_btn',  'properties_icon'),
        ]:
            if hasattr(self, attr):
                getattr(self, attr).setIcon(
                    getattr(SVGIconFactory, method)(20, color))
        for attr, method in [('minimize_btn','minimize_icon'),
                              ('maximize_btn','maximize_icon'),
                              ('close_btn','close_icon')]:
            if hasattr(self, attr):
                getattr(self, attr).setIcon(
                    getattr(SVGIconFactory, method)(20, color))

    def _launch_theme_settings(self):
        try:
            if not APPSETTINGS_AVAILABLE: return
            dialog = SettingsDialog(self.app_settings, self)
            dialog.themeChanged.connect(lambda _: self._apply_theme())
            if dialog.exec():
                self._apply_theme()
        except Exception as e:
            QMessageBox.warning(self, "Theme Error", str(e))

    def _get_icon_color(self) -> str:
        if self.app_settings:
            colors = self.app_settings.get_theme_colors()
            return colors.get('text_primary', '#ffffff')
        return '#ffffff'

    # ── Window management ─────────────────────────────────────────────────────

    def _set_status(self, msg: str):
        if hasattr(self, '_status_bar'):
            self._status_bar.showMessage(msg)

    def _toggle_maximize(self):
        if self.isMaximized(): self.showNormal()
        else: self.showMaximized()

    def keyPressEvent(self, e):
        k   = e.key()
        mod = e.modifiers()
        tool_keys = {
            Qt.Key.Key_P: TOOL_PENCIL, Qt.Key.Key_E: TOOL_ERASER,
            Qt.Key.Key_F: TOOL_FILL,   Qt.Key.Key_K: TOOL_PICKER,
            Qt.Key.Key_L: TOOL_LINE,   Qt.Key.Key_R: TOOL_RECT,
            Qt.Key.Key_C: TOOL_CIRCLE, Qt.Key.Key_S: TOOL_SPRAY,
        }
        if k in tool_keys:
            self._select_tool(tool_keys[k])
        elif mod == Qt.KeyboardModifier.ControlModifier:
            if k == Qt.Key.Key_Z: self._undo_canvas()
            elif k == Qt.Key.Key_Y: self._redo_canvas()
        elif k in (Qt.Key.Key_Plus, Qt.Key.Key_Equal):
            self._set_zoom(self._canvas_zoom + 1)
        elif k == Qt.Key.Key_Minus:
            self._set_zoom(self._canvas_zoom - 1)
        else:
            super().keyPressEvent(e)

    def closeEvent(self, event):
        self.window_closed.emit()
        event.accept()

    # ── Corner resize + dragging (COL Workshop pattern) ───────────────────────

    def _get_resize_corner(self, pos):
        size = self.corner_size; w = self.width(); h = self.height()
        if pos.x() < size and pos.y() < size:           return "top-left"
        if pos.x() > w - size and pos.y() < size:       return "top-right"
        if pos.x() < size and pos.y() > h - size:       return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:   return "bottom-right"
        return None

    def _update_cursor(self, direction):
        cursors = {
            "top-left":     Qt.CursorShape.SizeFDiagCursor,
            "bottom-right": Qt.CursorShape.SizeFDiagCursor,
            "top-right":    Qt.CursorShape.SizeBDiagCursor,
            "bottom-left":  Qt.CursorShape.SizeBDiagCursor,
        }
        self.setCursor(cursors.get(direction, Qt.CursorShape.ArrowCursor))

    def _is_on_draggable_area(self, pos):
        if not hasattr(self, 'titlebar'):
            return False
        if not self.titlebar.rect().contains(pos):
            return False
        for w in self.titlebar.findChildren(QPushButton):
            if w.isVisible() and w.geometry().contains(pos):
                return False
        return True

    def mousePressEvent(self, event):
        if event.button() != Qt.MouseButton.LeftButton:
            return super().mousePressEvent(event)
        pos = event.pos()
        self.resize_corner = self._get_resize_corner(pos)
        if self.resize_corner:
            self.resizing = True
            self.drag_position    = event.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            event.accept(); return
        if hasattr(self, 'titlebar') and self.titlebar.geometry().contains(pos):
            tb_pos = self.titlebar.mapFromParent(pos)
            if self._is_on_draggable_area(tb_pos):
                handle = self.windowHandle()
                if handle: handle.startSystemMove()
                event.accept(); return
        super().mousePressEvent(event)

    def mouseMoveEvent(self, event):
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept(); return
        else:
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()
            self._update_cursor(corner)
        super().mouseMoveEvent(event)

    def mouseReleaseEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()

    def _handle_corner_resize(self, global_pos):
        if not self.resize_corner or not self.drag_position: return
        delta    = global_pos - self.drag_position
        geometry = self.initial_geometry
        min_w, min_h = 900, 560
        if self.resize_corner == "bottom-right":
            nw = geometry.width() + delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h: self.resize(nw, nh)
        elif self.resize_corner == "bottom-left":
            nx = geometry.x() + delta.x()
            nw = geometry.width() - delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(nx, geometry.y(), nw, nh)
        elif self.resize_corner == "top-right":
            ny = geometry.y() + delta.y()
            nw = geometry.width() + delta.x()
            nh = geometry.height() - delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(geometry.x(), ny, nw, nh)
        elif self.resize_corner == "top-left":
            nx = geometry.x() + delta.x()
            ny = geometry.y() + delta.y()
            nw = geometry.width() - delta.x()
            nh = geometry.height() - delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(nx, ny, nw, nh)

    def paintEvent(self, event):
        super().paintEvent(event)
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        if self.app_settings:
            colors = self.app_settings.get_theme_colors()
            accent = QColor(colors.get('accent_primary', '#1976d2'))
        else:
            accent = QColor(100, 150, 255)
        accent.setAlpha(180)
        hover = QColor(accent); hover.setAlpha(255)
        w, h, size = self.width(), self.height(), self.corner_size
        corners = {
            'top-left':     [(0,0),(size,0),(0,size)],
            'top-right':    [(w,0),(w-size,0),(w,size)],
            'bottom-left':  [(0,h),(size,h),(0,h-size)],
            'bottom-right': [(w,h),(w-size,h),(w,h-size)],
        }
        for name, pts in corners.items():
            path = QPainterPath()
            path.moveTo(*pts[0]); path.lineTo(*pts[1]); path.lineTo(*pts[2])
            path.closeSubpath()
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(hover if self.hover_corner == name else accent))
            painter.drawPath(path)
        painter.end()


# ══════════════════════════════════════════════════════════════════════════════
#  Public factory function
# ══════════════════════════════════════════════════════════════════════════════

def open_dp5_workshop(main_window=None) -> DP5Workshop:
    """Open DP5 Workshop standalone or embedded."""
    try:
        workshop = DP5Workshop(None, main_window)
        workshop.setWindowFlags(Qt.WindowType.Window)
        workshop.setWindowTitle(App_name)
        workshop.resize(1400, 800)
        workshop.show()
        return workshop
    except Exception as e:
        if main_window:
            QMessageBox.critical(main_window, App_name + " Error", str(e))
        return None


# ══════════════════════════════════════════════════════════════════════════════
#  Standalone entry point
# ══════════════════════════════════════════════════════════════════════════════

if __name__ == "__main__":
    import traceback

    print(f"{App_name} starting…")
    try:
        app = QApplication(sys.argv)
        w   = DP5Workshop()
        w.setWindowTitle(App_name + " – Standalone")
        w.resize(1400, 800)
        w.show()
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)


__all__ = ['DP5Workshop', 'DP5Canvas', 'DP5PaletteBar', 'open_dp5_workshop']
