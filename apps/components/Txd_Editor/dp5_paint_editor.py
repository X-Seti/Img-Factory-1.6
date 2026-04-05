#!/usr/bin/env python3
#this belongs in apps/components/Txd_Editor/dp5_paint_editor.py - Version: 1
# X-Seti - Apr 2026 - IMG Factory 1.6
# Deluxe Paint 5 style texture paint editor
"""
Deluxe Paint 5 inspired paint editor for TXD Workshop.

Layout mirrors DP5:
  - Top menubar: File | Edit | Picture | Brush | Techniques | Stencil | Misc
  - Right toolbar: tools column, palette strip (24 colours visible), Undo/CLR
  - Main canvas with zoom, grid-aware pixel editing
  - Bottom status bar: cursor position, colour info

Features:
  - Draw tools: Pencil, Fill (flood), Spray, Line, Rectangle, Circle
  - Import/export: IFF ILBM, BMP, PNG
  - 256-colour palette editor (for palettised textures)
  - Undo/redo stack (16 levels)
  - Zoom 1×–16×
  - Apply back to TXD Workshop texture slot
"""

from PyQt6.QtWidgets import (
    QDialog, QWidget, QVBoxLayout, QHBoxLayout, QGridLayout,
    QPushButton, QLabel, QSlider, QScrollArea, QFrame,
    QMenuBar, QMenu, QFileDialog, QColorDialog, QStatusBar,
    QSizePolicy, QToolButton, QMessageBox, QInputDialog
)
from PyQt6.QtGui import (
    QImage, QPixmap, QPainter, QColor, QPen, QBrush,
    QCursor, QFont, QAction, QMouseEvent, QWheelEvent
)
from PyQt6.QtCore import Qt, QPoint, QRect, QSize, pyqtSignal

from collections import deque
from typing import Optional, List, Tuple
import struct, os

## Methods list -
# class DP5Canvas
# class DP5PaletteBar
# class DP5ToolBar
# class DP5PaintEditor


# ── Tool IDs ─────────────────────────────────────────────────────────────────
TOOL_PENCIL = 'pencil'
TOOL_FILL   = 'fill'
TOOL_SPRAY  = 'spray'
TOOL_LINE   = 'line'
TOOL_RECT   = 'rect'
TOOL_CIRCLE = 'circle'
TOOL_PICKER = 'picker'
TOOL_ERASER = 'eraser'

DP5_BG = '#1a1a2e'       # dark navy background like DP5
DP5_PANEL = '#16213e'
DP5_BORDER = '#0f3460'
DP5_TEXT = '#e0e0e0'
DP5_ACCENT = '#e94560'


# ── Canvas widget ─────────────────────────────────────────────────────────────

class DP5Canvas(QWidget):
    """Zoomable pixel-accurate paint canvas."""

    pixel_changed = pyqtSignal(int, int)   # x, y after any paint op

    def __init__(self, width: int, height: int, rgba: bytearray, parent=None):
        super().__init__(parent)
        self.tex_w  = width
        self.tex_h  = height
        self.rgba   = rgba          # mutable bytearray, 4 bytes per pixel
        self.zoom   = 4             # default 4× zoom
        self.offset = QPoint(0, 0)  # scroll offset in canvas pixels
        self.tool   = TOOL_PENCIL
        self.color  = QColor(255, 0, 0, 255)
        self.brush_size = 1
        self.show_grid  = True

        self._drawing   = False
        self._last_pt   = None
        self._spray_timer = None

        # Line/rect/circle preview
        self._preview_start = None
        self._preview_end   = None

        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self.setMinimumSize(200, 200)

    # ── Coordinate helpers ────────────────────────────────────────────────────

    def _widget_to_tex(self, p: QPoint) -> Tuple[int, int]:
        """Map widget pixel to texture pixel."""
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
        """Paint with current brush size."""
        s = self.brush_size
        for dy in range(-s+1, s):
            for dx in range(-s+1, s):
                if s == 1 or (dx*dx + dy*dy) < s*s:
                    self.set_pixel(cx+dx, cy+dy, c)

    # ── Flood fill ────────────────────────────────────────────────────────────

    def flood_fill(self, sx: int, sy: int, fill_col: QColor):
        if not (0 <= sx < self.tex_w and 0 <= sy < self.tex_h):
            return
        target = self.get_pixel(sx, sy)
        if (target.red() == fill_col.red() and
                target.green() == fill_col.green() and
                target.blue() == fill_col.blue() and
                target.alpha() == fill_col.alpha()):
            return
        stack = [(sx, sy)]
        visited = set()
        while stack:
            x, y = stack.pop()
            if (x, y) in visited: continue
            if not (0 <= x < self.tex_w and 0 <= y < self.tex_h): continue
            px = self.get_pixel(x, y)
            if (px.red() != target.red() or px.green() != target.green() or
                    px.blue() != target.blue() or px.alpha() != target.alpha()):
                continue
            visited.add((x, y))
            self.set_pixel(x, y, fill_col)
            stack.extend([(x+1,y),(x-1,y),(x,y+1),(x,y-1)])

    # ── Line drawing ─────────────────────────────────────────────────────────

    def draw_line(self, x0, y0, x1, y1, c: QColor):
        """Bresenham line."""
        dx, dy = abs(x1-x0), abs(y1-y0)
        sx = 1 if x0 < x1 else -1
        sy = 1 if y0 < y1 else -1
        err = dx - dy
        while True:
            self.set_pixel_brush(x0, y0, c)
            if x0 == x1 and y0 == y1: break
            e2 = 2 * err
            if e2 > -dy: err -= dy; x0 += sx
            if e2 <  dx: err += dx; y0 += sy

    def draw_rect(self, x0, y0, x1, y1, c: QColor, fill=False):
        if x0 > x1: x0, x1 = x1, x0
        if y0 > y1: y0, y1 = y1, y0
        if fill:
            for y in range(y0, y1+1):
                for x in range(x0, x1+1):
                    self.set_pixel(x, y, c)
        else:
            self.draw_line(x0,y0,x1,y0,c)
            self.draw_line(x1,y0,x1,y1,c)
            self.draw_line(x1,y1,x0,y1,c)
            self.draw_line(x0,y1,x0,y0,c)

    def draw_circle(self, cx, cy, rx, ry, c: QColor, fill=False):
        """Midpoint ellipse."""
        x, y = 0, ry
        d1 = (ry*ry) - (rx*rx*ry) + (0.25*rx*rx)
        dx = 2*ry*ry*x; dy = 2*rx*rx*y
        while dx < dy:
            if fill:
                self.draw_line(cx-x,cy+y,cx+x,cy+y,c)
                self.draw_line(cx-x,cy-y,cx+x,cy-y,c)
            else:
                for px,py in [(cx+x,cy+y),(cx-x,cy+y),(cx+x,cy-y),(cx-x,cy-y)]:
                    self.set_pixel(px,py,c)
            if d1 < 0: dx+=2*ry*ry; d1+=dx+ry*ry; x+=1
            else: dx+=2*ry*ry; dy-=2*rx*rx; d1+=dx-dy+ry*ry; x+=1; y-=1
        d2 = ((ry*ry)*((x+0.5)*(x+0.5))) + ((rx*rx)*((y-1)*(y-1))) - (rx*rx*ry*ry)
        while y >= 0:
            if fill:
                self.draw_line(cx-x,cy+y,cx+x,cy+y,c)
                self.draw_line(cx-x,cy-y,cx+x,cy-y,c)
            else:
                for px,py in [(cx+x,cy+y),(cx-x,cy+y),(cx+x,cy-y),(cx-x,cy-y)]:
                    self.set_pixel(px,py,c)
            if d2 > 0: dy-=2*rx*rx; d2+=rx*rx-dy; y-=1
            else: dx+=2*ry*ry; dy-=2*rx*rx; d2+=dx-dy+rx*rx; x+=1; y-=1

    # ── Painting ──────────────────────────────────────────────────────────────

    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, False)

        # Draw texture pixels
        img = QImage(bytes(self.rgba), self.tex_w, self.tex_h,
                     self.tex_w * 4, QImage.Format.Format_RGBA8888)
        scaled = img.scaled(self.tex_w * self.zoom, self.tex_h * self.zoom,
                             Qt.AspectRatioMode.KeepAspectRatio,
                             Qt.TransformationMode.FastTransformation)
        painter.drawImage(-self.offset.x(), -self.offset.y(), scaled)

        # Pixel grid at zoom ≥ 4
        if self.show_grid and self.zoom >= 4:
            pen = QPen(QColor(60, 60, 80, 100), 1)
            painter.setPen(pen)
            ox = -(self.offset.x() % self.zoom)
            oy = -(self.offset.y() % self.zoom)
            for x in range(ox, self.width(), self.zoom):
                painter.drawLine(x, 0, x, self.height())
            for y in range(oy, self.height(), self.zoom):
                painter.drawLine(0, y, self.width(), y)

        # Preview overlay for line/rect/circle
        if self._preview_start and self._preview_end and self.tool in (TOOL_LINE, TOOL_RECT, TOOL_CIRCLE):
            pen = QPen(self.color, 1, Qt.PenStyle.DashLine)
            painter.setPen(pen)
            painter.setBrush(Qt.BrushStyle.NoBrush)
            sx, sy = self._tex_to_widget(*self._preview_start).x(), self._tex_to_widget(*self._preview_start).y()
            ex, ey = self._tex_to_widget(*self._preview_end).x(), self._tex_to_widget(*self._preview_end).y()
            if self.tool == TOOL_LINE:
                painter.drawLine(sx, sy, ex, ey)
            elif self.tool == TOOL_RECT:
                painter.drawRect(QRect(QPoint(sx,sy), QPoint(ex,ey)).normalized())
            elif self.tool == TOOL_CIRCLE:
                painter.drawEllipse(QRect(QPoint(sx,sy), QPoint(ex,ey)).normalized())

    # ── Mouse events ─────────────────────────────────────────────────────────

    def mousePressEvent(self, e: QMouseEvent):
        if e.button() != Qt.MouseButton.LeftButton: return
        tx, ty = self._widget_to_tex(e.position().toPoint())
        self._drawing = True
        self._last_pt = (tx, ty)

        if self.tool == TOOL_PENCIL:
            self.set_pixel_brush(tx, ty, self.color); self.update()
        elif self.tool == TOOL_ERASER:
            self.set_pixel_brush(tx, ty, QColor(0,0,0,0)); self.update()
        elif self.tool == TOOL_FILL:
            self.flood_fill(tx, ty, self.color); self.update()
        elif self.tool == TOOL_PICKER:
            c = self.get_pixel(tx, ty)
            if c.isValid(): self.color = c
            p = getattr(self, '_editor', None)
            if p: p._update_color_btn()
        elif self.tool in (TOOL_LINE, TOOL_RECT, TOOL_CIRCLE):
            self._preview_start = (tx, ty)
            self._preview_end   = (tx, ty)
        elif self.tool == TOOL_SPRAY:
            self._do_spray(tx, ty); self.update()

    def mouseMoveEvent(self, e: QMouseEvent):
        tx, ty = self._widget_to_tex(e.position().toPoint())
        p = getattr(self, '_editor', None)
        if p and hasattr(p, '_update_status'):
            p._update_status(tx, ty, self.get_pixel(tx, ty))

        if not (e.buttons() & Qt.MouseButton.LeftButton): return
        if not self._drawing: return

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

        self._drawing = False
        self._preview_start = self._preview_end = None
        self._last_pt = None
        self.update()
        self.pixel_changed.emit(tx, ty)

    def wheelEvent(self, e: QWheelEvent):
        delta = e.angleDelta().y()
        if e.modifiers() & Qt.KeyboardModifier.ControlModifier:
            old = self.zoom
            if delta > 0: self.zoom = min(16, self.zoom + 1)
            else:         self.zoom = max(1,  self.zoom - 1)
            p = getattr(self, '_editor', None)
            if p and hasattr(p, '_update_zoom_label'):
                p._update_zoom_label()
        self.update()

    def _do_spray(self, cx: int, cy: int):
        import random
        r = self.brush_size * 5
        for _ in range(max(1, r)):
            dx = random.randint(-r, r)
            dy = random.randint(-r, r)
            if dx*dx + dy*dy <= r*r:
                self.set_pixel(cx+dx, cy+dy, self.color)


# ── Palette bar ───────────────────────────────────────────────────────────────

class DP5PaletteBar(QWidget):
    """Vertical strip of colour swatches, DP5 right-side style."""

    color_picked = pyqtSignal(QColor)

    def __init__(self, parent=None):
        super().__init__(parent)
        # Default Amiga-style 32-colour palette (expandable to 256)
        self.palette = self._default_palette()
        self.selected = 0
        self.setFixedWidth(24)
        self.setMinimumHeight(200)

    def _default_palette(self) -> List[QColor]:
        base = [
            (0,0,0),(255,255,255),(255,0,0),(0,255,0),(0,0,255),(255,255,0),
            (255,0,255),(0,255,255),(128,0,0),(0,128,0),(0,0,128),(128,128,0),
            (128,0,128),(0,128,128),(192,192,192),(128,128,128),
            (255,128,0),(0,255,128),(128,0,255),(255,0,128),
            (255,128,128),(128,255,128),(128,128,255),(255,255,128),
            (64,64,64),(160,160,160),(200,100,50),(100,200,50),
            (50,100,200),(200,50,100),(100,50,200),(50,200,100),
        ]
        cols = [QColor(*c) for c in base]
        while len(cols) < 256: cols.append(QColor(0,0,0))
        return cols

    def paintEvent(self, event):
        p = QPainter(self)
        sw = self.width()
        n  = self.height() // sw
        for i in range(min(n, len(self.palette))):
            c = self.palette[i]
            p.fillRect(0, i*sw, sw, sw, c)
            if i == self.selected:
                p.setPen(QPen(QColor(255,255,255), 2))
                p.drawRect(1, i*sw+1, sw-3, sw-3)

    def mousePressEvent(self, e: QMouseEvent):
        sw = self.width()
        idx = e.position().toPoint().y() // sw
        if 0 <= idx < len(self.palette):
            self.selected = idx
            self.color_picked.emit(self.palette[idx])
            self.update()

    def set_palette_from_rgba(self, palette_data: List[Tuple]):
        """Load palette from list of (R,G,B) or (R,G,B,A) tuples."""
        self.palette = []
        for entry in palette_data[:256]:
            if len(entry) >= 3:
                self.palette.append(QColor(entry[0], entry[1], entry[2]))
        while len(self.palette) < 256:
            self.palette.append(QColor(0,0,0))
        self.update()


# ── Main editor dialog ────────────────────────────────────────────────────────

class DP5PaintEditor(QDialog):
    """
    Deluxe Paint 5 style paint editor dialog.
    """

    def __init__(self, tex: dict, parent=None):
        super().__init__(parent)
        self.tex    = tex
        self.width  = tex['width']
        self.height = tex['height']
        self.rgba   = bytearray(tex.get('rgba_data', b'\x00' * self.width * self.height * 4))

        # Undo stack
        self._undo_stack: deque = deque(maxlen=16)
        self._redo_stack: deque = deque(maxlen=16)

        self.setWindowTitle(f"DP5 Paint — {tex['name']}  ({self.width}×{self.height})")
        self.setModal(True)
        self.resize(900, 680)
        self.setStyleSheet(f"""
            QDialog, QWidget {{ background: {DP5_BG}; color: {DP5_TEXT}; }}
            QPushButton {{
                background: {DP5_PANEL}; color: {DP5_TEXT};
                border: 1px solid {DP5_BORDER}; padding: 3px 8px;
                min-width: 40px;
            }}
            QPushButton:hover {{ background: {DP5_BORDER}; }}
            QPushButton:checked {{ background: {DP5_ACCENT}; color: white; }}
            QMenuBar {{ background: {DP5_PANEL}; color: {DP5_TEXT}; }}
            QMenuBar::item:selected {{ background: {DP5_BORDER}; }}
            QMenu {{ background: {DP5_PANEL}; color: {DP5_TEXT};
                     border: 1px solid {DP5_BORDER}; }}
            QMenu::item:selected {{ background: {DP5_BORDER}; }}
            QLabel {{ color: {DP5_TEXT}; }}
            QStatusBar {{ background: {DP5_PANEL}; color: {DP5_TEXT};
                          border-top: 1px solid {DP5_BORDER}; }}
        """)

        self._build_ui()

    def _build_ui(self):
        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(0)

        # ── Menu bar ──
        mb = QMenuBar(self)
        self._build_menus(mb)
        root.addWidget(mb)

        # ── Main body ──
        body = QHBoxLayout()
        body.setContentsMargins(4, 4, 4, 4)
        body.setSpacing(4)

        # Left: tool buttons
        tool_col = self._build_tool_panel()
        body.addWidget(tool_col)

        # Centre: scroll canvas
        self.canvas = DP5Canvas(self.width, self.height, self.rgba, self)
        self.canvas.pixel_changed.connect(self._on_canvas_changed)
        # Give canvas a direct reference to the editor (avoids fragile parent chains)
        self.canvas._editor = self
        # Now canvas exists — set initial tool
        self._select_tool(TOOL_PENCIL)
        scroll = QScrollArea()
        scroll.setWidget(self.canvas)
        scroll.setWidgetResizable(True)
        scroll.setStyleSheet(f"QScrollArea {{ border: 1px solid {DP5_BORDER}; }}")
        body.addWidget(scroll, 1)

        # Right: palette strip + info panel
        right_col = self._build_right_panel()
        body.addWidget(right_col)

        root.addLayout(body, 1)

        # ── Status bar ──
        self.status = QStatusBar(self)
        self.status.showMessage(f"Canvas: {self.width}×{self.height}  |  Zoom: {self.canvas.zoom}×  |  Pos: 0,0")
        root.addWidget(self.status)

    # ── Menu bar ──────────────────────────────────────────────────────────────

    def _build_menus(self, mb: QMenuBar):
        # File menu
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

        # Edit menu
        edit_m = mb.addMenu('Edit')
        edit_m.addAction('Undo\tCtrl+Z', self._undo)
        edit_m.addAction('Redo\tCtrl+Y', self._redo)
        edit_m.addSeparator()
        edit_m.addAction('Clear canvas',         self._clear)
        edit_m.addAction('Fill with colour',     self._fill_all)

        # Picture menu
        pic_m = mb.addMenu('Picture')
        pic_m.addAction('Flip horizontal',   self._flip_h)
        pic_m.addAction('Flip vertical',     self._flip_v)
        pic_m.addAction('Invert colours',    self._invert)
        pic_m.addAction('Brighten +25',      lambda: self._adjust(25))
        pic_m.addAction('Darken -25',        lambda: self._adjust(-25))

        # View menu
        view_m = mb.addMenu('View')
        view_m.addAction('Zoom in  +',  lambda: self._set_zoom(self.canvas.zoom + 1))
        view_m.addAction('Zoom out -',  lambda: self._set_zoom(self.canvas.zoom - 1))
        view_m.addAction('Zoom 1×',     lambda: self._set_zoom(1))
        view_m.addAction('Zoom 4×',     lambda: self._set_zoom(4))
        view_m.addAction('Zoom 8×',     lambda: self._set_zoom(8))
        view_m.addSeparator()
        grid_act = view_m.addAction('Pixel grid')
        grid_act.setCheckable(True)
        grid_act.setChecked(True)
        grid_act.triggered.connect(lambda v: setattr(self.canvas, 'show_grid', v) or self.canvas.update())

    # ── Tool panel ────────────────────────────────────────────────────────────

    def _build_tool_panel(self) -> QWidget:
        frame = QFrame()
        frame.setFixedWidth(52)
        frame.setStyleSheet(f"QFrame {{ background: {DP5_PANEL}; border: 1px solid {DP5_BORDER}; }}")
        lay = QVBoxLayout(frame)
        lay.setContentsMargins(2, 2, 2, 2)
        lay.setSpacing(2)

        tools = [
            (TOOL_PENCIL, '✏',  'Pencil (P)'),
            (TOOL_ERASER, '⬜', 'Eraser (E)'),
            (TOOL_FILL,   '🪣', 'Flood Fill (F)'),
            (TOOL_PICKER, '💉', 'Colour Picker (K)'),
            (TOOL_LINE,   '╱',  'Line (L)'),
            (TOOL_RECT,   '▭',  'Rectangle (R)'),
            (TOOL_CIRCLE, '○',  'Circle (C)'),
            (TOOL_SPRAY,  '💨', 'Spray (S)'),
        ]
        self._tool_btns = {}
        for tool_id, icon, tip in tools:
            btn = QPushButton(icon)
            btn.setFixedSize(44, 36)
            btn.setCheckable(True)
            btn.setToolTip(tip)
            btn.setFont(QFont('Segoe UI Emoji', 11))
            btn.clicked.connect(lambda _, t=tool_id: self._select_tool(t))
            self._tool_btns[tool_id] = btn
            lay.addWidget(btn)

        lay.addSpacing(8)

        # Brush size slider
        lay.addWidget(QLabel('Size'))
        self.size_sl = QSlider(Qt.Orientation.Vertical)
        self.size_sl.setRange(1, 10)
        self.size_sl.setValue(1)
        self.size_sl.setFixedHeight(60)
        self.size_sl.valueChanged.connect(self._set_brush_size)
        lay.addWidget(self.size_sl)

        lay.addStretch()

        # Zoom label
        self.zoom_lbl = QLabel('4×')
        self.zoom_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        lay.addWidget(self.zoom_lbl)

        # Undo / CLR
        undo_btn = QPushButton('UN\nDO')
        undo_btn.setFixedSize(44, 36)
        undo_btn.clicked.connect(self._undo)
        lay.addWidget(undo_btn)

        clr_btn = QPushButton('CLR')
        clr_btn.setFixedSize(44, 36)
        clr_btn.clicked.connect(self._clear)
        lay.addWidget(clr_btn)

        # Note: _select_tool called after canvas is built (see _build_ui)
        return frame

    # ── Right panel ───────────────────────────────────────────────────────────

    def _build_right_panel(self) -> QWidget:
        frame = QFrame()
        frame.setFixedWidth(80)
        frame.setStyleSheet(f"QFrame {{ background: {DP5_PANEL}; border: 1px solid {DP5_BORDER}; }}")
        lay = QVBoxLayout(frame)
        lay.setContentsMargins(2, 2, 2, 2)
        lay.setSpacing(4)

        # Current colour swatch
        self.color_btn = QPushButton()
        self.color_btn.setFixedSize(70, 30)
        self.color_btn.setToolTip('Click to pick colour')
        self.color_btn.clicked.connect(self._pick_color)
        self._update_color_btn()
        lay.addWidget(self.color_btn)

        # Foreground / background labels
        fg_bg = QHBoxLayout()
        fg_lbl = QLabel('FG')
        fg_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        fg_bg.addWidget(fg_lbl)
        lay.addLayout(fg_bg)

        # Palette bar
        self.pal_bar = DP5PaletteBar(self)
        self.pal_bar.color_picked.connect(self._on_palette_color)
        # Load texture palette if palettised
        if self.tex.get('clut'):
            pal_rgb = [(self.tex['clut'][i*4], self.tex['clut'][i*4+1], self.tex['clut'][i*4+2])
                       for i in range(len(self.tex['clut'])//4)]
            self.pal_bar.set_palette_from_rgba(pal_rgb)
        lay.addWidget(self.pal_bar, 1)

        # Apply / Cancel
        apply_btn  = QPushButton('Apply')
        cancel_btn = QPushButton('Cancel')
        apply_btn.clicked.connect(self._apply)
        cancel_btn.clicked.connect(self.reject)
        lay.addWidget(apply_btn)
        lay.addWidget(cancel_btn)

        return frame

    # ── Helpers ───────────────────────────────────────────────────────────────

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
        self.color_btn.setStyleSheet(
            f"background:{c.name()}; border: 2px solid white;")
        self.color_btn.setToolTip(f"Colour: {c.name()}  α={c.alpha()}")

    def _pick_color(self):
        c = QColorDialog.getColor(
            self.canvas.color, self,
            "Pick Colour",
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
            f"RGBA: ({c.red()},{c.green()},{c.blue()},{c.alpha()})  |  "
            f"Zoom: {self.canvas.zoom}×  |  "
            f"Tool: {self.canvas.tool}")

    # ── Undo / Redo ───────────────────────────────────────────────────────────

    def _push_undo(self):
        self._undo_stack.append(bytes(self.rgba))

    def _undo(self):
        if self._undo_stack:
            self._redo_stack.append(bytes(self.rgba))
            state = self._undo_stack.pop()
            self.rgba[:] = state
            self.canvas.rgba = self.rgba
            self.canvas.update()

    def _redo(self):
        if self._redo_stack:
            self._undo_stack.append(bytes(self.rgba))
            state = self._redo_stack.pop()
            self.rgba[:] = state
            self.canvas.rgba = self.rgba
            self.canvas.update()

    # ── Picture operations ────────────────────────────────────────────────────

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

    def _flip_h(self):
        self._push_undo()
        from PIL import Image
        img = Image.frombytes('RGBA', (self.width, self.height), bytes(self.rgba))
        img = img.transpose(Image.Transpose.FLIP_LEFT_RIGHT)
        self.rgba[:] = img.tobytes()
        self.canvas.update()

    def _flip_v(self):
        self._push_undo()
        from PIL import Image
        img = Image.frombytes('RGBA', (self.width, self.height), bytes(self.rgba))
        img = img.transpose(Image.Transpose.FLIP_TOP_BOTTOM)
        self.rgba[:] = img.tobytes()
        self.canvas.update()

    def _invert(self):
        self._push_undo()
        from PIL import Image, ImageOps
        img = Image.frombytes('RGBA', (self.width, self.height), bytes(self.rgba))
        r,g,b,a = img.split()
        img2 = Image.merge('RGBA', (ImageOps.invert(r), ImageOps.invert(g),
                                     ImageOps.invert(b), a))
        self.rgba[:] = img2.tobytes()
        self.canvas.update()

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
            self, "Import IFF / ILBM", "",
            "IFF ILBM (*.iff *.lbm *.ilbm);;All Files (*)")
        if not path: return
        try:
            from apps.methods.iff_ilbm import read_iff_ilbm
            result = read_iff_ilbm(open(path,'rb').read())
            if not result:
                QMessageBox.warning(self, "IFF Error", "Not a valid IFF ILBM file.")
                return
            w, h, palette, pixels = result
            self._push_undo()
            # Convert palettised → RGBA and resize if needed
            from PIL import Image as PILImage
            img = PILImage.new('P', (w, h))
            pal_flat = []
            for r,g,b in palette: pal_flat += [r,g,b]
            img.putpalette(pal_flat)
            img.putdata(pixels)
            rgba_img = img.convert('RGBA').resize(
                (self.width, self.height), PILImage.NEAREST)
            self.rgba[:] = rgba_img.tobytes()
            self.pal_bar.set_palette_from_rgba(palette)
            self.canvas.update()
        except Exception as e:
            QMessageBox.warning(self, "Import Error", str(e))

    def _import_image(self):
        path, _ = QFileDialog.getOpenFileName(
            self, "Import Image", "",
            "Images (*.bmp *.png *.jpg *.jpeg *.tga);;All Files (*)")
        if not path: return
        try:
            from PIL import Image as PILImage
            self._push_undo()
            img = PILImage.open(path).convert('RGBA').resize(
                (self.width, self.height), PILImage.LANCZOS)
            self.rgba[:] = img.tobytes()
            self.canvas.update()
        except Exception as e:
            QMessageBox.warning(self, "Import Error", str(e))

    def _export_iff(self):
        path, _ = QFileDialog.getSaveFileName(
            self, "Export IFF ILBM", f"{self.tex['name']}.iff",
            "IFF ILBM (*.iff *.lbm);;All Files (*)")
        if not path: return
        try:
            from PIL import Image as PILImage
            from apps.methods.iff_ilbm import write_iff_ilbm
            img = PILImage.frombytes('RGBA', (self.width, self.height), bytes(self.rgba))
            p_img = img.quantize(colors=256)
            palette = [tuple(p_img.getpalette()[i*3:i*3+3]) for i in range(256)]
            pixels = bytes(p_img.tobytes())
            data = write_iff_ilbm(self.width, self.height, palette, pixels)
            open(path, 'wb').write(data)
            QMessageBox.information(self, "Exported",
                f"IFF ILBM saved to {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    def _export_bmp(self):
        path, _ = QFileDialog.getSaveFileName(
            self, "Export BMP", f"{self.tex['name']}.bmp", "BMP (*.bmp)")
        if not path: return
        try:
            from PIL import Image as PILImage
            img = PILImage.frombytes('RGBA', (self.width, self.height), bytes(self.rgba))
            img.convert('RGB').save(path, 'BMP')
            QMessageBox.information(self, "Exported", f"BMP saved.")
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    def _export_png(self):
        path, _ = QFileDialog.getSaveFileName(
            self, "Export PNG", f"{self.tex['name']}.png", "PNG (*.png)")
        if not path: return
        try:
            from PIL import Image as PILImage
            img = PILImage.frombytes('RGBA', (self.width, self.height), bytes(self.rgba))
            img.save(path, 'PNG')
            QMessageBox.information(self, "Exported", f"PNG saved.")
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    # ── Keyboard shortcuts ────────────────────────────────────────────────────

    def keyPressEvent(self, e):
        k = e.key()
        if   k == Qt.Key.Key_P: self._select_tool(TOOL_PENCIL)
        elif k == Qt.Key.Key_E: self._select_tool(TOOL_ERASER)
        elif k == Qt.Key.Key_F: self._select_tool(TOOL_FILL)
        elif k == Qt.Key.Key_K: self._select_tool(TOOL_PICKER)
        elif k == Qt.Key.Key_L: self._select_tool(TOOL_LINE)
        elif k == Qt.Key.Key_R: self._select_tool(TOOL_RECT)
        elif k == Qt.Key.Key_C: self._select_tool(TOOL_CIRCLE)
        elif k == Qt.Key.Key_S: self._select_tool(TOOL_SPRAY)
        elif k == Qt.Key.Key_Plus  or k == Qt.Key.Key_Equal: self._set_zoom(self.canvas.zoom + 1)
        elif k == Qt.Key.Key_Minus: self._set_zoom(self.canvas.zoom - 1)
        elif e.modifiers() == Qt.KeyboardModifier.ControlModifier:
            if   k == Qt.Key.Key_Z: self._undo()
            elif k == Qt.Key.Key_Y: self._redo()
        else:
            super().keyPressEvent(e)

    # ── Apply ─────────────────────────────────────────────────────────────────

    def _apply(self):
        """Apply canvas back to the texture dict."""
        self.tex['rgba_data'] = bytes(self.rgba)
        self.accept()


__all__ = ['DP5PaintEditor']
