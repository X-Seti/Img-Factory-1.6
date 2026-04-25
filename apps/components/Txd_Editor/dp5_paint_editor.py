#!/usr/bin/env python3
#this belongs in apps/components/Txd_Editor/dp5_paint_editor.py - Version: 2
# X-Seti - Apr 2026 - IMG Factory 1.6
# Deluxe Paint 5 style texture paint editor — standalone + embedded
"""
Deluxe Paint 5 inspired paint editor for TXD Workshop.

Standalone:  python3 dp5_paint_editor.py [image.png]
Embedded:    DP5PaintEditor(tex_dict, parent=txd_workshop_widget)

Theme-aware: reads app_settings from parent chain; falls back to system theme
             when run standalone.  Reconnects on theme_changed signal.

Layout mirrors DP5:
  - Top menubar: File | Edit | Picture | View
  - Left toolbar: 8 tools + brush-size slider + Undo/CLR
  - Centre: zoomable scrollable canvas
  - Right: foreground swatch, 256-colour palette strip, Apply/Cancel
  - Bottom: status bar (pos, RGBA, zoom, tool)

File I/O: IFF ILBM (Amiga Deluxe Paint native), BMP, PNG
"""

App_name = "DP5 Paint"

import os, sys
from collections import deque
from typing import Optional, List, Tuple

from PyQt6.QtWidgets import (
    QApplication, QDialog, QWidget,
    QVBoxLayout, QHBoxLayout,
    QPushButton, QLabel, QSlider, QScrollArea, QFrame,
    QMenuBar, QMenu, QFileDialog, QColorDialog, QStatusBar,
    QSizePolicy, QMessageBox
)
from PyQt6.QtGui import (
    QImage, QPixmap, QPainter, QColor, QPen,
    QCursor, QFont, QAction, QMouseEvent, QWheelEvent
)
from PyQt6.QtCore import Qt, QPoint, QRect, pyqtSignal

## Methods list -
# class DP5Canvas
# class DP5PaletteBar
# class DP5PaintEditor
# open_dp5_paint_editor   (launcher helper used by txd_workshop)

# ── Tool IDs ──────────────────────────────────────────────────────────────────
TOOL_PENCIL = 'pencil'
TOOL_FILL   = 'fill'
TOOL_SPRAY  = 'spray'
TOOL_LINE   = 'line'
TOOL_RECT   = 'rect'
TOOL_CIRCLE = 'circle'
TOOL_PICKER = 'picker'
TOOL_ERASER = 'eraser'


# ── Theme helper ──────────────────────────────────────────────────────────────

def _get_app_settings(widget):
    """Walk parent chain to find app_settings. Returns None if not found."""
    try:
        node = widget
        while node is not None:
            if hasattr(node, 'app_settings') and node.app_settings:
                return node.app_settings
            parent = node.parent() if callable(getattr(node, 'parent', None)) else None
            if parent is node or parent is None:
                break
            node = parent
    except Exception:
        pass
    return None


def _get_stylesheet(widget) -> str:
    """Return the current app stylesheet, or a sensible fallback."""
    try:
        from apps.core.theme_utils import get_theme_colors, build_dialog_stylesheet
        colors = get_theme_colors(widget)
        return build_dialog_stylesheet(colors)
    except Exception:
        pass
    # Minimal fallback — matches the app's default palette roughly
    return ""


# ── Canvas widget ─────────────────────────────────────────────────────────────

class DP5Canvas(QWidget):
    """Zoomable pixel-accurate paint canvas."""

    pixel_changed = pyqtSignal(int, int)

    def __init__(self, width: int, height: int, rgba: bytearray, parent=None):
        super().__init__(parent)
        self.tex_w  = width
        self.tex_h  = height
        self.rgba   = rgba
        self.zoom   = 4
        self.offset = QPoint(0, 0)
        self.tool   = TOOL_PENCIL
        self.color  = QColor(255, 0, 0, 255)
        self.brush_size = 1
        self.show_grid  = True
        self._drawing   = False
        self._last_pt   = None
        self._preview_start = None
        self._preview_end   = None
        # Direct reference to editor — set by DP5PaintEditor after creation
        self._editor = None

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
        import random
        r = self.brush_size * 5
        for _ in range(max(1, r)):
            dx, dy = random.randint(-r,r), random.randint(-r,r)
            if dx*dx + dy*dy <= r*r:
                self.set_pixel(cx+dx, cy+dy, self.color)

    # ── Paint ──────────────────────────────────────────────────────────────────

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

        if self._preview_start and self._preview_end and self.tool in (TOOL_LINE,TOOL_RECT,TOOL_CIRCLE):
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

    # ── Mouse events ─────────────────────────────────────────────────────────

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
                if ed: ed._update_color_btn()
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
            self.draw_circle(self._preview_start[0], self._preview_start[1], max(1,rx), max(1,ry), self.color)
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


# ── Palette bar ───────────────────────────────────────────────────────────────

class DP5PaletteBar(QWidget):
    """Vertical 24px-wide colour swatch strip, DP5 right-side style."""

    color_picked = pyqtSignal(QColor)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.palette  = self._default_palette()
        self.selected = 1   # start on white
        self.setFixedWidth(24)
        self.setMinimumHeight(200)
        self.setToolTip("Click to select colour")


    def _get_ui_color(self, key): #vers 1
        """Return theme-aware QColor. No hardcoded colors - everything via app_settings."""
        from PyQt6.QtGui import QColor
        try:
            app_settings = getattr(self, 'app_settings', None) or \
                getattr(getattr(self, 'main_window', None), 'app_settings', None)
            if app_settings and hasattr(app_settings, 'get_ui_color'):
                return app_settings.get_ui_color(key)
        except Exception:
            pass
        pal = self.palette()
        if key == 'viewport_bg':
            return pal.color(pal.ColorRole.Base)
        if key == 'viewport_text':
            return pal.color(pal.ColorRole.PlaceholderText)
        if key == 'border':
            return pal.color(pal.ColorRole.Mid)
        return pal.color(pal.ColorRole.WindowText)

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
                p.setPen(QPen(self._get_ui_color('viewport_bg'), 2))
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
            if len(entry) >= 3:
                self.palette.append(QColor(entry[0], entry[1], entry[2]))
        while len(self.palette) < 256:
            self.palette.append(QColor(0,0,0))
        self.update()


# ── Main editor ───────────────────────────────────────────────────────────────

class DP5PaintEditor(QDialog):
    """
    Deluxe Paint 5 style paint editor.
    Standalone or embedded in TXD Workshop.
    """

    def __init__(self, tex: dict, parent=None):
        super().__init__(parent)
        self.tex    = tex
        self.width  = tex.get('width',  64)
        self.height = tex.get('height', 64)
        self.rgba   = bytearray(
            tex.get('rgba_data', b'\x00' * self.width * self.height * 4))

        # App settings — walk parent chain; standalone creates its own
        self.app_settings = _get_app_settings(parent) or _get_app_settings(self)
        if self.app_settings is None:
            try:
                from apps.utils.app_settings_system import AppSettings
                self.app_settings = AppSettings()
            except Exception:
                self.app_settings = None

        # Undo / redo
        self._undo_stack: deque = deque(maxlen=16)
        self._redo_stack: deque = deque(maxlen=16)

        self.setWindowTitle(
            f"{App_name} — {tex.get('name','texture')}  "
            f"({self.width}×{self.height})")
        self.setModal(True)
        self.resize(960, 700)

        self._build_ui()
        self._apply_theme()

        # Connect live theme changes
        if self.app_settings and hasattr(self.app_settings, 'theme_changed'):
            self.app_settings.theme_changed.connect(self._apply_theme)

    # ── Theme ─────────────────────────────────────────────────────────────────

    def _apply_theme(self): #vers 1
        """Re-apply the current app theme stylesheet."""
        try:
            if self.app_settings and hasattr(self.app_settings, 'get_stylesheet'):
                self.setStyleSheet(self.app_settings.get_stylesheet())
                return
        except Exception:
            pass
        # Fallback via theme_utils
        self.setStyleSheet(_get_stylesheet(self))

    # ── UI construction ───────────────────────────────────────────────────────

    def _build_ui(self):
        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(0)

        # Menu bar
        mb = QMenuBar(self)
        self._build_menus(mb)
        root.addWidget(mb)

        # Body
        body = QHBoxLayout()
        body.setContentsMargins(4, 4, 4, 4)
        body.setSpacing(4)

        # Left tool panel (built before canvas — does NOT call _select_tool)
        tool_col = self._build_tool_panel()
        body.addWidget(tool_col)

        # Canvas — created BEFORE _select_tool is called
        self.canvas = DP5Canvas(self.width, self.height, self.rgba, self)
        self.canvas._editor = self   # direct back-reference, no parent() chains
        self.canvas.pixel_changed.connect(self._on_canvas_changed)
        scroll = QScrollArea()
        scroll.setWidget(self.canvas)
        scroll.setWidgetResizable(True)
        body.addWidget(scroll, 1)

        # Right panel
        right_col = self._build_right_panel()
        body.addWidget(right_col)

        root.addLayout(body, 1)

        # Status bar
        self.status = QStatusBar(self)
        root.addWidget(self.status)

        # Initial tool selection (canvas now exists)
        self._select_tool(TOOL_PENCIL)
        self._update_zoom_label()
        self._refresh_status()

    def _build_menus(self, mb: QMenuBar):
        file_m = mb.addMenu('File')
        file_m.addAction('Import IFF / ILBM…', self._import_iff)
        file_m.addAction('Import BMP / PNG…',  self._import_image)
        file_m.addSeparator()
        file_m.addAction('Export IFF / ILBM…', self._export_iff)
        file_m.addAction('Export BMP…',         self._export_bmp)
        file_m.addAction('Export PNG…',         self._export_png)
        file_m.addSeparator()
        file_m.addAction('Apply to texture',    self._apply)
        file_m.addAction('Close',               self.reject)

        edit_m = mb.addMenu('Edit')
        edit_m.addAction('Undo\tCtrl+Z', self._undo)
        edit_m.addAction('Redo\tCtrl+Y', self._redo)
        edit_m.addSeparator()
        edit_m.addAction('Clear canvas',     self._clear)
        edit_m.addAction('Fill with colour', self._fill_all)

        pic_m = mb.addMenu('Picture')
        pic_m.addAction('Flip horizontal',  self._flip_h)
        pic_m.addAction('Flip vertical',    self._flip_v)
        pic_m.addAction('Invert colours',   self._invert)
        pic_m.addAction('Brighten +25',     lambda: self._adjust(25))
        pic_m.addAction('Darken -25',       lambda: self._adjust(-25))

        view_m = mb.addMenu('View')
        view_m.addAction('Zoom in  Ctrl++', lambda: self._set_zoom(self.canvas.zoom+1))
        view_m.addAction('Zoom out Ctrl+-', lambda: self._set_zoom(self.canvas.zoom-1))
        view_m.addAction('1×',  lambda: self._set_zoom(1))
        view_m.addAction('4×',  lambda: self._set_zoom(4))
        view_m.addAction('8×',  lambda: self._set_zoom(8))
        view_m.addAction('16×', lambda: self._set_zoom(16))
        view_m.addSeparator()
        ga = view_m.addAction('Pixel grid')
        ga.setCheckable(True); ga.setChecked(True)
        ga.triggered.connect(
            lambda v: (setattr(self.canvas, 'show_grid', v), self.canvas.update()))

        if self.app_settings:
            view_m.addSeparator()
            view_m.addAction('Theme settings…', self._open_theme)

    def _build_tool_panel(self) -> QWidget:
        frame = QFrame()
        frame.setFixedWidth(52)
        frame.setObjectName('ToolPanel')
        lay = QVBoxLayout(frame)
        lay.setContentsMargins(2, 2, 2, 2)
        lay.setSpacing(2)

        tools = [
            (TOOL_PENCIL, '✏',  'Pencil (P)'),
            (TOOL_ERASER, '⬜', 'Eraser (E)'),
            (TOOL_FILL,   '▓',  'Flood Fill (F)'),
            (TOOL_PICKER, '⊕',  'Colour Picker (K)'),
            (TOOL_LINE,   '╱',  'Line (L)'),
            (TOOL_RECT,   '▭',  'Rectangle (R)'),
            (TOOL_CIRCLE, '○',  'Circle (C)'),
            (TOOL_SPRAY,  '·:', 'Spray (S)'),
        ]
        self._tool_btns = {}
        for tool_id, icon, tip in tools:
            btn = QPushButton(icon)
            btn.setFixedSize(44, 34)
            btn.setCheckable(True)
            btn.setToolTip(tip)
            btn.clicked.connect(lambda _, t=tool_id: self._select_tool(t))
            self._tool_btns[tool_id] = btn
            lay.addWidget(btn)

        lay.addSpacing(6)
        lay.addWidget(QLabel('Size'))
        self.size_sl = QSlider(Qt.Orientation.Vertical)
        self.size_sl.setRange(1, 10)
        self.size_sl.setValue(1)
        self.size_sl.setFixedHeight(60)
        self.size_sl.valueChanged.connect(self._set_brush_size)
        lay.addWidget(self.size_sl)

        lay.addStretch()

        self.zoom_lbl = QLabel('4×')
        self.zoom_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        lay.addWidget(self.zoom_lbl)

        undo_btn = QPushButton('UN\nDO')
        undo_btn.setFixedSize(44, 34)
        undo_btn.clicked.connect(self._undo)
        lay.addWidget(undo_btn)

        clr_btn = QPushButton('CLR')
        clr_btn.setFixedSize(44, 34)
        clr_btn.clicked.connect(self._clear)
        lay.addWidget(clr_btn)

        # NOTE: _select_tool NOT called here — canvas not yet created
        return frame

    def _build_right_panel(self) -> QWidget:
        frame = QFrame()
        frame.setFixedWidth(84)
        frame.setObjectName('RightPanel')
        lay = QVBoxLayout(frame)
        lay.setContentsMargins(2, 2, 2, 2)
        lay.setSpacing(4)

        # Colour swatch button
        self.color_btn = QPushButton()
        self.color_btn.setFixedSize(74, 28)
        self.color_btn.setToolTip('Click to pick colour')
        self.color_btn.clicked.connect(self._pick_color)
        lay.addWidget(self.color_btn)

        lay.addWidget(QLabel('Palette'))

        # Palette strip
        self.pal_bar = DP5PaletteBar(self)
        self.pal_bar.color_picked.connect(self._on_palette_color)
        # Load CLUT if texture is palettised
        if self.tex.get('clut'):
            clut = self.tex['clut']
            pal = [(clut[i*4], clut[i*4+1], clut[i*4+2])
                   for i in range(len(clut)//4)]
            self.pal_bar.set_palette(pal)
        lay.addWidget(self.pal_bar, 1)

        lay.addSpacing(4)
        apply_btn  = QPushButton('Apply')
        cancel_btn = QPushButton('Cancel')
        apply_btn.setFixedHeight(28)
        cancel_btn.setFixedHeight(28)
        apply_btn.clicked.connect(self._apply)
        cancel_btn.clicked.connect(self.reject)
        lay.addWidget(apply_btn)
        lay.addWidget(cancel_btn)

        return frame

    # ── Tool / zoom helpers ───────────────────────────────────────────────────

    def _select_tool(self, tool_id: str):
        self.canvas.tool = tool_id
        for tid, btn in self._tool_btns.items():
            btn.setChecked(tid == tool_id)

    def _set_brush_size(self, v: int):
        self.canvas.brush_size = v

    def _set_zoom(self, z: int):
        self.canvas.zoom = max(1, min(16, z))
        self._update_zoom_label()
        self.canvas.update()

    def _update_zoom_label(self):
        self.zoom_lbl.setText(f"{self.canvas.zoom}×")

    def _update_color_btn(self):
        c = self.canvas.color
        luma = (c.red()*299 + c.green()*587 + c.blue()*114) // 1000
        fg = '#000000' if luma > 128 else '#ffffff'
        self.color_btn.setStyleSheet(
            f"background:{c.name()}; color:{fg}; "
            f"border: 2px solid {'palette(buttonText)' if luma<128 else 'palette(windowText)'};")
        self.color_btn.setText(c.name().upper())

    def _pick_color(self):
        c = QColorDialog.getColor(
            self.canvas.color, self, "Pick Colour",
            QColorDialog.ColorDialogOption.ShowAlphaChannel)
        if c.isValid():
            self.canvas.color = c
            self._update_color_btn()

    def _on_palette_color(self, c: QColor):
        self.canvas.color = c
        self._update_color_btn()

    def _on_canvas_changed(self, x: int, y: int):
        self._update_status(x, y, self.canvas.get_pixel(x, y))

    def _update_status(self, x: int, y: int, c: QColor):
        self.status.showMessage(
            f"Pos: {x},{y}  |  "
            f"RGBA({c.red()},{c.green()},{c.blue()},{c.alpha()})  |  "
            f"Zoom: {self.canvas.zoom}×  |  Tool: {self.canvas.tool}")

    def _refresh_status(self):
        self.status.showMessage(
            f"Canvas: {self.width}×{self.height}  |  "
            f"Zoom: {self.canvas.zoom}×  |  "
            f"Tool: {self.canvas.tool}")
        self._update_color_btn()

    # ── Undo / redo ───────────────────────────────────────────────────────────

    def _push_undo(self):
        self._undo_stack.append(bytes(self.rgba))
        self._redo_stack.clear()

    def _undo(self):
        if self._undo_stack:
            self._redo_stack.append(bytes(self.rgba))
            self.rgba[:] = self._undo_stack.pop()
            self.canvas.rgba = self.rgba
            self.canvas.update()

    def _redo(self):
        if self._redo_stack:
            self._undo_stack.append(bytes(self.rgba))
            self.rgba[:] = self._redo_stack.pop()
            self.canvas.rgba = self.rgba
            self.canvas.update()

    # ── Picture ops ───────────────────────────────────────────────────────────

    def _clear(self):
        self._push_undo()
        self.rgba[:] = b'\x00' * len(self.rgba)
        self.canvas.update()

    def _fill_all(self):
        self._push_undo()
        c = self.canvas.color
        for i in range(self.width * self.height):
            self.rgba[i*4:i*4+4] = [c.red(), c.green(), c.blue(), c.alpha()]
        self.canvas.update()

    def _pil_op(self, fn):
        from PIL import Image
        self._push_undo()
        img = Image.frombytes('RGBA', (self.width, self.height), bytes(self.rgba))
        img = fn(img)
        self.rgba[:] = img.tobytes()
        self.canvas.update()

    def _flip_h(self):
        from PIL import Image
        self._pil_op(lambda i: i.transpose(Image.Transpose.FLIP_LEFT_RIGHT))

    def _flip_v(self):
        from PIL import Image
        self._pil_op(lambda i: i.transpose(Image.Transpose.FLIP_TOP_BOTTOM))

    def _invert(self):
        from PIL import Image, ImageOps
        def inv(img):
            r,g,b,a = img.split()
            return Image.merge('RGBA', (ImageOps.invert(r), ImageOps.invert(g),
                                        ImageOps.invert(b), a))
        self._pil_op(inv)

    def _adjust(self, delta: int):
        self._push_undo()
        for i in range(0, len(self.rgba), 4):
            self.rgba[i]   = max(0, min(255, self.rgba[i]   + delta))
            self.rgba[i+1] = max(0, min(255, self.rgba[i+1] + delta))
            self.rgba[i+2] = max(0, min(255, self.rgba[i+2] + delta))
        self.canvas.update()

    # ── File I/O ──────────────────────────────────────────────────────────────

    def _import_iff(self):
        path, _ = QFileDialog.getOpenFileName(
            self, "Import IFF ILBM", "",
            "IFF ILBM (*.iff *.lbm *.ilbm);;All Files (*)")
        if not path: return
        try:
            from apps.methods.iff_ilbm import read_iff_ilbm
            result = read_iff_ilbm(open(path, 'rb').read())
            if not result:
                QMessageBox.warning(self, "IFF Error", "Not a valid IFF ILBM file.")
                return
            w, h, palette, pixels = result
            self._push_undo()
            from PIL import Image
            img = Image.new('P', (w, h))
            flat = []
            for r,g,b in palette: flat += [r,g,b]
            img.putpalette(flat); img.putdata(pixels)
            img = img.convert('RGBA').resize((self.width, self.height), Image.NEAREST)
            self.rgba[:] = img.tobytes()
            self.pal_bar.set_palette(palette)
            self.canvas.update()
        except Exception as e:
            QMessageBox.warning(self, "Import Error", str(e))

    def _import_image(self):
        path, _ = QFileDialog.getOpenFileName(
            self, "Import Image", "",
            "Images (*.bmp *.png *.jpg *.jpeg *.tga);;All Files (*)")
        if not path: return
        try:
            from PIL import Image
            self._push_undo()
            img = Image.open(path).convert('RGBA').resize(
                (self.width, self.height), Image.LANCZOS)
            self.rgba[:] = img.tobytes()
            self.canvas.update()
        except Exception as e:
            QMessageBox.warning(self, "Import Error", str(e))

    def _export_iff(self):
        path, _ = QFileDialog.getSaveFileName(
            self, "Export IFF ILBM", f"{self.tex.get('name','texture')}.iff",
            "IFF ILBM (*.iff);;All Files (*)")
        if not path: return
        try:
            from PIL import Image
            from apps.methods.iff_ilbm import write_iff_ilbm
            img = Image.frombytes('RGBA', (self.width, self.height), bytes(self.rgba))
            p_img = img.quantize(colors=256)
            pal_flat = p_img.getpalette()
            palette = [(pal_flat[i*3], pal_flat[i*3+1], pal_flat[i*3+2]) for i in range(256)]
            data = write_iff_ilbm(self.width, self.height, palette, bytes(p_img.tobytes()))
            open(path, 'wb').write(data)
            QMessageBox.information(self, "Exported",
                f"IFF ILBM saved:\n{os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    def _export_bmp(self):
        path, _ = QFileDialog.getSaveFileName(
            self, "Export BMP", f"{self.tex.get('name','texture')}.bmp",
            "BMP (*.bmp)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (self.width, self.height), bytes(self.rgba))
            img.convert('RGB').save(path, 'BMP')
            QMessageBox.information(self, "Exported", "BMP saved.")
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    def _export_png(self):
        path, _ = QFileDialog.getSaveFileName(
            self, "Export PNG", f"{self.tex.get('name','texture')}.png",
            "PNG (*.png)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (self.width, self.height), bytes(self.rgba))
            img.save(path, 'PNG')
            QMessageBox.information(self, "Exported", "PNG saved.")
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    def _open_theme(self):
        try:
            from apps.utils.app_settings_system import SettingsDialog
            dlg = SettingsDialog(self.app_settings, self)
            if hasattr(self.app_settings, 'theme_changed'):
                self.app_settings.theme_changed.connect(self._apply_theme)
            dlg.exec()
        except Exception as e:
            QMessageBox.warning(self, "Theme", f"Could not open theme settings:\n{e}")

    # ── Keyboard shortcuts ────────────────────────────────────────────────────

    def keyPressEvent(self, e):
        k = e.key()
        mod = e.modifiers()
        if   k == Qt.Key.Key_P: self._select_tool(TOOL_PENCIL)
        elif k == Qt.Key.Key_E: self._select_tool(TOOL_ERASER)
        elif k == Qt.Key.Key_F: self._select_tool(TOOL_FILL)
        elif k == Qt.Key.Key_K: self._select_tool(TOOL_PICKER)
        elif k == Qt.Key.Key_L: self._select_tool(TOOL_LINE)
        elif k == Qt.Key.Key_R: self._select_tool(TOOL_RECT)
        elif k == Qt.Key.Key_C: self._select_tool(TOOL_CIRCLE)
        elif k == Qt.Key.Key_S: self._select_tool(TOOL_SPRAY)
        elif k in (Qt.Key.Key_Plus, Qt.Key.Key_Equal): self._set_zoom(self.canvas.zoom+1)
        elif k == Qt.Key.Key_Minus: self._set_zoom(self.canvas.zoom-1)
        elif mod == Qt.KeyboardModifier.ControlModifier:
            if   k == Qt.Key.Key_Z: self._undo()
            elif k == Qt.Key.Key_Y: self._redo()
        else:
            super().keyPressEvent(e)

    # ── Apply ─────────────────────────────────────────────────────────────────

    def _apply(self):
        self.tex['rgba_data'] = bytes(self.rgba)
        self.accept()


# ── Launcher helper ───────────────────────────────────────────────────────────

def open_dp5_paint_editor(tex: dict, parent=None) -> bool:
    """
    Open the DP5 paint editor for *tex*.
    Returns True if the user applied changes, False if cancelled.
    """
    editor = DP5PaintEditor(tex, parent)
    return editor.exec() == QDialog.DialogCode.Accepted


# ── Standalone entry point ────────────────────────────────────────────────────

if __name__ == '__main__':
    import traceback
    print(f"{App_name} starting standalone...")

    app = QApplication(sys.argv)
    app.setApplicationName(App_name)

    # Load image from command-line arg or create blank 64×64
    tex = {'name': 'untitled', 'width': 64, 'height': 64,
           'rgba_data': b'\x80\x80\x80\xff' * (64*64)}

    if len(sys.argv) > 1:
        path = sys.argv[1]
        try:
            from PIL import Image
            img = Image.open(path).convert('RGBA')
            w, h = img.size
            tex = {
                'name':      os.path.splitext(os.path.basename(path))[0],
                'width':     w,
                'height':    h,
                'rgba_data': img.tobytes(),
            }
            print(f"Loaded: {path}  ({w}×{h})")
        except Exception as e:
            print(f"Could not load {path}: {e}")

    try:
        editor = DP5PaintEditor(tex)
        editor.setWindowFlags(
            Qt.WindowType.Window |
            Qt.WindowType.WindowCloseButtonHint |
            Qt.WindowType.WindowMinMaxButtonsHint)
        editor.resize(1000, 740)
        editor.show()
        code = app.exec()
        if editor.result() == QDialog.DialogCode.Accepted:
            print("Applied — saving output.png")
            from PIL import Image
            img = Image.frombytes('RGBA', (tex['width'], tex['height']),
                                  tex['rgba_data'])
            img.save('output.png')
        sys.exit(code)
    except Exception:
        traceback.print_exc()
        sys.exit(1)


__all__ = ['DP5PaintEditor', 'open_dp5_paint_editor']
