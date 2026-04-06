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

import os, sys, random, json
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
TOOL_PENCIL        = 'pencil'
TOOL_ERASER        = 'eraser'
TOOL_FILL          = 'fill'
TOOL_SPRAY         = 'spray'
TOOL_LINE          = 'line'
TOOL_CURVE         = 'curve'
TOOL_RECT          = 'rect'
TOOL_FILLED_RECT   = 'filled_rect'
TOOL_CIRCLE        = 'circle'
TOOL_FILLED_CIRCLE = 'filled_circle'
TOOL_TRIANGLE      = 'triangle'
TOOL_FILLED_TRIANGLE = 'filled_triangle'
TOOL_POLYGON       = 'polygon'
TOOL_FILLED_POLYGON = 'filled_polygon'
TOOL_STAR          = 'star'
TOOL_FILLED_STAR   = 'filled_star'
TOOL_LASSO         = 'lasso'
TOOL_FILLED_LASSO  = 'filled_lasso'   # right-click fill toggle
TOOL_PICKER        = 'picker'
TOOL_SELECT        = 'select'
TOOL_MOVE          = 'move'
TOOL_ZOOM          = 'zoom'
TOOL_TEXT          = 'text'
TOOL_STAMP         = 'stamp'          # stamp/paste brush from buffer

# Shape tools that have an outline/fill toggle via right-click
SHAPE_FILL_PAIRS = {
    TOOL_RECT:     TOOL_FILLED_RECT,
    TOOL_CIRCLE:   TOOL_FILLED_CIRCLE,
    TOOL_TRIANGLE: TOOL_FILLED_TRIANGLE,
    TOOL_POLYGON:  TOOL_FILLED_POLYGON,
    TOOL_STAR:     TOOL_FILLED_STAR,
    TOOL_LASSO:    TOOL_FILLED_LASSO,
}

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
#  Tool icon renderer — Photoshop-style white silhouettes on dark tile
# ══════════════════════════════════════════════════════════════════════════════

def _make_tool_icon(shape: str, size: int = 42,
                    active: bool = False) -> QIcon:
    """
    Render a tool icon.  Uses SVGIconFactory.dp_*_icon() when available
    (icons defined in imgfactory_svg_icons.py), otherwise falls back to the
    inline QPainter renderer below.

    Normal:  dark tile (#1e1e24) + white/light icon
    Active:  light tile (#d8d8e0) + dark icon (inverted, DP5 style)
    """
    # ── SVG icon map: shape → SVGIconFactory method name ─────────────────────
    # Add entries here as you create new dp_*_icon() methods in
    # imgfactory_svg_icons.py — they'll be picked up automatically.
    _SVG_MAP = {
        'pencil':        'dp_pencil_icon',
        'eraser':        'dp_eraser_icon',
        'fill':          'dp_bucket_icon',
        'spray':         'dp_brush_icon',
        'picker':        'dp_color_picker_icon',
        'line':          'dp_line_icon',
        'zoom':          'dp_magnify_icon',
        # Add more as you create dp_*_icon() methods:
        # 'stamp':       'dp_stamp_icon',
        # 'curve':       'dp_curve_icon',
        # 'select':      'dp_select_icon',
        # 'text':        'dp_text_icon',
    }

    if ICONS_AVAILABLE and shape in _SVG_MAP:
        method_name = _SVG_MAP[shape]
        fn = getattr(SVGIconFactory, method_name, None)
        if fn is not None:
            tile_bg  = '#1e1e24' if not active else '#d8d8e0'
            icon_col = '#f0f0f4' if not active else '#101014'
            try:
                return fn(size, color=icon_col, bg_color=tile_bg)
            except TypeError:
                # bg_color not supported — render icon then composite onto tile
                ico = fn(size, color=icon_col)
                px  = QPixmap(size, size)
                px.fill(QColor(tile_bg))
                p   = QPainter(px)
                p.drawPixmap(0, 0, ico.pixmap(size, size))
                p.end()
                return QIcon(px)

    # ── QPainter fallback (shapes, lasso, select, text, etc.) ────────────────
    import math as _m

    tile_bg = QColor('#1e1e24') if not active else QColor('#d8d8e0')
    ink     = QColor('#f0f0f4') if not active else QColor('#101014')

    px = QPixmap(size, size)
    px.fill(tile_bg)

    p = QPainter(px)
    p.setRenderHint(QPainter.RenderHint.Antialiasing)
    p.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)

    # Work in a normalised 48×48 space, then scale down via transform
    N  = 48.0
    sc = size / N
    p.scale(sc, sc)

    # Helpers in 48-unit space
    def mk_pen(w=2.5, cap=Qt.PenCapStyle.RoundCap,
               join=Qt.PenJoinStyle.RoundJoin):
        return QPen(ink, w, Qt.PenStyle.SolidLine, cap, join)

    def solid_brush():
        return QBrush(ink)

    def poly(*args):
        """Accept poly((x,y),(x,y)...) or poly([(x,y),...]) or poly(x,y,x,y...)."""
        from PyQt6.QtGui import QPolygonF
        from PyQt6.QtCore import QPointF
        # Single list/tuple of pairs
        if len(args) == 1 and hasattr(args[0], '__iter__'):
            pts = [(x, y) for x, y in args[0]]
        # Flat numeric sequence: poly(x1,y1,x2,y2,...)
        elif all(isinstance(a, (int, float)) for a in args) and len(args) % 2 == 0:
            pts = [(args[i], args[i+1]) for i in range(0, len(args), 2)]
        # Sequence of (x,y) tuples
        else:
            pts = list(args)
        return QPolygonF([QPointF(x, y) for x, y in pts])

    def path_from(*segments):
        """Build QPainterPath from ('M',x,y), ('L',x,y), ('C',x1,y1,x2,y2,x,y),
        ('Q',x1,y1,x,y), ('Z',) tuples."""
        from PyQt6.QtCore import QPointF
        pa = QPainterPath()
        for seg in segments:
            cmd = seg[0]
            if cmd == 'M': pa.moveTo(seg[1], seg[2])
            elif cmd == 'L': pa.lineTo(seg[1], seg[2])
            elif cmd == 'C': pa.cubicTo(seg[1],seg[2],seg[3],seg[4],seg[5],seg[6])
            elif cmd == 'Q': pa.quadTo(seg[1],seg[2],seg[3],seg[4])
            elif cmd == 'Z': pa.closeSubpath()
        return pa

    # ─────────────────────────────────────────────────────────────────────────

    if shape == 'pencil':
        # Photoshop-style pencil: long diagonal body, pointed tip bottom-left,
        # small eraser rectangle top-right, ~45° angle
        # Body (parallelogram)
        p.setPen(mk_pen(0))
        p.setBrush(solid_brush())
        p.drawPolygon(poly(
            8, 40,   # tip bottom-left
            12, 36,  # left shoulder
            36, 12,  # right shoulder
            40, 16,  # top-right body
            16, 40,  # bottom body
        ))
        # Tip triangle (darker notch)
        p.setBrush(QBrush(tile_bg))
        p.drawPolygon(poly(8,40, 13,38, 10,35))
        # Eraser cap rectangle top-right
        p.setBrush(solid_brush())
        p.setPen(mk_pen(0))
        # Rotate the eraser cap to match pencil angle
        from PyQt6.QtCore import QPointF
        from PyQt6.QtGui import QPolygonF
        p.drawPolygon(poly(36,8, 40,12, 44,8, 40,4))
        # Ferrule line (separator between body and eraser)
        p.setPen(mk_pen(1.5))
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawLine(QPoint(36,12), QPoint(40,8))

    elif shape == 'eraser':
        # Wide rounded rectangle eraser — iconic PS shape
        # Main body
        p.setPen(mk_pen(0))
        p.setBrush(solid_brush())
        p.drawRoundedRect(6, 16, 36, 18, 4, 4)
        # Stripe in the middle (erased area — slightly darker)
        p.setBrush(QBrush(tile_bg.lighter(130)))
        p.setPen(mk_pen(0))
        p.drawRoundedRect(6, 22, 18, 12, 2, 2)
        # Bottom shadow line
        p.setPen(mk_pen(1.5))
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawLine(QPoint(6,38), QPoint(42,38))

    elif shape == 'fill':
        # Paint bucket — bucket body + handle arc + small drop below spout
        # Bucket body (trapezoid, narrower top)
        p.setPen(mk_pen(0))
        p.setBrush(solid_brush())
        p.drawPolygon(poly(
            (14,20), (34,20),   # top
            (38,40), (10,40),   # bottom (wider)
        ))
        # Bucket top rim
        p.drawRoundedRect(12, 15, 24, 7, 2, 2)
        # Handle arc (over the top)
        p.setPen(mk_pen(2.5))
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        handle = QPainterPath()
        handle.moveTo(17, 15)
        handle.quadTo(24, 4, 31, 15)
        p.drawPath(handle)
        # Small round paint drop sitting BELOW the bucket base centre
        p.setPen(mk_pen(0))
        p.setBrush(solid_brush())
        p.drawEllipse(QPoint(24, 44), 3, 3)

    elif shape == 'spray':
        # Spray can / airbrush — body + nozzle + spray dots
        # Can body
        p.setPen(mk_pen(0))
        p.setBrush(solid_brush())
        p.drawRoundedRect(20, 10, 18, 30, 4, 4)
        # Nozzle spout (left side)
        p.drawRoundedRect(8, 14, 14, 6, 2, 2)
        # Spray cloud of dots (scattered left)
        import random as _rng
        _rng.seed(42)
        for _ in range(12):
            dx = int(_rng.gauss(6, 5))
            dy = int(_rng.gauss(24, 8))
            r  = max(1, int(_rng.uniform(1, 2.5)))
            p.drawEllipse(QPoint(max(2,min(18,dx)), max(6,min(42,dy))), r, r)
        # Button on top of can
        p.drawRoundedRect(26, 6, 8, 6, 2, 2)

    elif shape == 'picker':
        # Eyedropper — long body, round glass bulb top-right, pointed tip bottom-left
        # Main body diagonal
        p.setPen(mk_pen(0))
        p.setBrush(solid_brush())
        # Body shaft (rotated rectangle)
        p.drawPolygon(poly(
            8,40,  12,44,  36,20,  32,16
        ))
        # Tip point
        p.drawPolygon(poly(6,42, 10,38, 8,44))
        # Round glass bulb top-right
        p.drawEllipse(QPoint(34,12), 8, 8)
        # Band between bulb and shaft
        p.setBrush(QBrush(tile_bg))
        p.setPen(mk_pen(0))
        p.drawPolygon(poly(28,18, 32,14, 36,18, 32,22))
        p.setBrush(solid_brush())
        p.drawRect(29,15, 6, 6)   # re-fill centre

    elif shape == 'line':
        # Diagonal line with round endpoint handles — PS line tool
        p.setPen(mk_pen(3.0))
        p.drawLine(QPoint(8, 40), QPoint(40, 8))
        # Endpoint circles
        p.setPen(mk_pen(1.5))
        p.setBrush(solid_brush())
        p.drawEllipse(QPoint(8, 40), 4, 4)
        p.drawEllipse(QPoint(40, 8), 4, 4)

    elif shape == 'curve':
        # Bézier curve tool — S-curve with 2 visible control handles + dots
        path = path_from(
            ('M', 8, 40),
            ('C', 8, 18,  40, 30,  40, 8),
            ('Z',) if False else ('M',8,40),   # no close
        )
        # Re-draw properly
        path2 = QPainterPath()
        path2.moveTo(8, 40)
        path2.cubicTo(8, 18, 40, 30, 40, 8)
        p.setPen(mk_pen(3.0))
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawPath(path2)
        # Control point handles (squares with lines)
        p.setPen(mk_pen(1.5))
        p.drawLine(QPoint(8,40), QPoint(8,18))
        p.drawLine(QPoint(40,8), QPoint(40,30))
        p.setBrush(solid_brush())
        p.drawRect(5, 15, 6, 6)
        p.drawRect(37, 27, 6, 6)
        # Anchor dots
        p.drawEllipse(QPoint(8,40), 3, 3)
        p.drawEllipse(QPoint(40,8), 3, 3)

    elif shape == 'rect':
        # Hollow rectangle — clean outline with visible corners
        p.setPen(mk_pen(2.5, Qt.PenCapStyle.SquareCap, Qt.PenJoinStyle.MiterJoin))
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawRect(8, 8, 32, 32)

    elif shape == 'filled_rect':
        # Filled rectangle — solid white box
        p.setPen(mk_pen(1.5, Qt.PenCapStyle.SquareCap, Qt.PenJoinStyle.MiterJoin))
        p.setBrush(solid_brush())
        p.drawRect(8, 8, 32, 32)

    elif shape == 'circle':
        # Hollow ellipse — clean outline
        p.setPen(mk_pen(2.5))
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawEllipse(QPoint(24, 24), 16, 16)

    elif shape == 'filled_circle':
        # Solid filled ellipse
        p.setPen(mk_pen(1.5))
        p.setBrush(solid_brush())
        p.drawEllipse(QPoint(24, 24), 16, 16)

    elif shape == 'triangle':
        # Isoceles triangle outline — pointed up
        p.setPen(mk_pen(2.5))
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawPolygon(poly(24,6, 6,42, 42,42))

    elif shape == 'filled_triangle':
        p.setPen(mk_pen(1.5))
        p.setBrush(solid_brush())
        p.drawPolygon(poly(24,6, 6,42, 42,42))

    elif shape == 'polygon':
        # Regular hexagon outline
        pts = [(int(24 + 18*_m.cos(_m.pi/2 + _m.pi*i/3)),
                int(24 + 18*_m.sin(_m.pi/2 + _m.pi*i/3))) for i in range(6)]
        p.setPen(mk_pen(2.5))
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawPolygon(poly(*[(x,y) for x,y in pts]))

    elif shape == 'filled_polygon':
        pts = [(int(24 + 18*_m.cos(_m.pi/2 + _m.pi*i/3)),
                int(24 + 18*_m.sin(_m.pi/2 + _m.pi*i/3))) for i in range(6)]
        p.setPen(mk_pen(1.5))
        p.setBrush(solid_brush())
        p.drawPolygon(poly(*[(x,y) for x,y in pts]))

    elif shape == 'star':
        # 5-point star — outline only
        outer, inner = 20, 8
        pts = []
        for i in range(10):
            a   = _m.pi * i / 5 - _m.pi / 2
            rad = outer if i % 2 == 0 else inner
            pts.append((24 + rad*_m.cos(a), 24 + rad*_m.sin(a)))
        p.setPen(mk_pen(2.5))
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawPolygon(poly(*pts))

    elif shape == 'filled_star':
        outer, inner = 20, 8
        pts = []
        for i in range(10):
            a   = _m.pi * i / 5 - _m.pi / 2
            rad = outer if i % 2 == 0 else inner
            pts.append((24 + rad*_m.cos(a), 24 + rad*_m.sin(a)))
        p.setPen(mk_pen(1.5))
        p.setBrush(solid_brush())
        p.drawPolygon(poly(*pts))

    elif shape == 'select':
        # Marquee selection — dashed rectangle with corner handles
        pen_dash = QPen(ink, 2.0, Qt.PenStyle.DashLine,
                        Qt.PenCapStyle.SquareCap, Qt.PenJoinStyle.MiterJoin)
        pen_dash.setDashPattern([3, 2])
        p.setPen(pen_dash)
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawRect(8, 8, 32, 32)
        # Corner handles
        p.setPen(mk_pen(0))
        p.setBrush(solid_brush())
        for hx, hy in [(6,6),(38,6),(6,38),(38,38),(22,6),(22,38),(6,22),(38,22)]:
            p.drawRect(hx-2, hy-2, 4, 4)

    elif shape == 'lasso':
        # Freehand lasso — kidney-bean loop, open at bottom-right, dashed
        path3 = QPainterPath()
        path3.moveTo(24, 42)
        path3.cubicTo(6, 42,  6, 6,  24, 6)
        path3.cubicTo(42, 6,  42, 30, 32, 36)
        pen_dash2 = QPen(ink, 2.5, Qt.PenStyle.DashLine,
                         Qt.PenCapStyle.RoundCap, Qt.PenJoinStyle.RoundJoin)
        pen_dash2.setDashPattern([4, 2])
        p.setPen(pen_dash2)
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawPath(path3)
        p.setPen(mk_pen(2.5))
        p.drawLine(QPoint(32,36), QPoint(38,44))

    elif shape == 'filled_lasso':
        # Filled lasso — same shape but solid filled
        path3 = QPainterPath()
        path3.moveTo(24, 42)
        path3.cubicTo(6, 42,  6, 6,  24, 6)
        path3.cubicTo(42, 6,  42, 30, 32, 36)
        path3.closeSubpath()
        p.setPen(mk_pen(1.5))
        p.setBrush(solid_brush())
        p.drawPath(path3)

    elif shape == 'move':
        # Classic 4-way arrow move tool — solid arrowheads, thin cross arms
        # Centre cross arms
        p.setPen(mk_pen(2.0))
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawLine(QPoint(24,8), QPoint(24,40))
        p.drawLine(QPoint(8,24), QPoint(40,24))
        # 4 arrowheads — solid triangles
        p.setPen(mk_pen(0))
        p.setBrush(solid_brush())
        # Up
        p.drawPolygon(poly(24,4, 20,12, 28,12))
        # Down
        p.drawPolygon(poly(24,44, 20,36, 28,36))
        # Left
        p.drawPolygon(poly(4,24, 12,20, 12,28))
        # Right
        p.drawPolygon(poly(44,24, 36,20, 36,28))

    elif shape == 'zoom':
        # Magnifying glass — circle lens (top-left), diagonal handle (bottom-right)
        # Lens ring
        p.setPen(mk_pen(3.5))
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawEllipse(QPoint(18, 18), 12, 12)
        # Handle
        p.setPen(mk_pen(4.0, Qt.PenCapStyle.RoundCap))
        p.drawLine(QPoint(28, 28), QPoint(42, 42))
        # Plus inside lens
        p.setPen(mk_pen(2.0))
        p.drawLine(QPoint(18, 13), QPoint(18, 23))
        p.drawLine(QPoint(13, 18), QPoint(23, 18))

    elif shape == 'text':
        # Capital T — serif style, wide top bar, serifs on feet — classic PS text tool
        p.setPen(mk_pen(0))
        p.setBrush(solid_brush())
        # Top horizontal bar
        p.drawRect(8, 8, 32, 5)
        # Vertical stem
        p.drawRect(20, 13, 8, 27)
        # Bottom serifs
        p.drawRect(14, 38, 8, 4)
        p.drawRect(26, 38, 8, 4)
        # Cursor I-beam line (right of T)
        p.setPen(mk_pen(1.5))
        p.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        p.drawLine(QPoint(40, 10), QPoint(40, 38))
        p.drawLine(QPoint(37, 10), QPoint(43, 10))
        p.drawLine(QPoint(37, 38), QPoint(43, 38))

    elif shape == 'stamp':
        # Rubber stamp — handle bar top, stamp body bottom, wavy ink dots
        p.setPen(mk_pen(0))
        p.setBrush(solid_brush())
        # Handle grip (top)
        p.drawRoundedRect(16, 6, 16, 10, 3, 3)
        # Neck connecting handle to pad
        p.drawRect(20, 16, 8, 6)
        # Stamp pad (wide flat block)
        p.drawRoundedRect(8, 22, 32, 10, 2, 2)
        # Ink impression dots below (showing it's been used)
        p.setPen(mk_pen(1.5))
        p.setBrush(solid_brush())
        for dot_x in [14, 20, 26, 32]:
            p.drawEllipse(QPoint(dot_x, 38), 2, 2)

    p.end()
    return QIcon(px)


# ══════════════════════════════════════════════════════════════════════════════
#  DP5Settings — per-tool settings (JSON, separate from global AppSettings)
# ══════════════════════════════════════════════════════════════════════════════

class DP5Settings:
    """
    Lightweight JSON settings for DP5 Workshop.
    Stored at ~/.config/imgfactory/dp5_workshop.json
    Completely separate from the global AppSettings/theme system.
    """

    DEFAULTS = {
        'show_bitmap_list':  False,    # left panel visible
        'tool_icon_size':    42,       # tool button pixel size (24–64)
        'tool_icon_color':   'color',  # 'color' | 'white' | 'dark'
        'tool_columns':      0,        # 0=auto, 2, 3, or 4
        'default_zoom':      4,        # startup zoom level
        'undo_levels':       32,
        'default_width':     320,
        'default_height':    200,
        'retro_palette':     'Amiga OCS',
        'show_pixel_grid':   True,
    }

    def __init__(self):
        cfg_dir = Path.home() / '.config' / 'imgfactory'
        cfg_dir.mkdir(parents=True, exist_ok=True)
        self._path = cfg_dir / 'dp5_workshop.json'
        self._data = dict(self.DEFAULTS)
        self._load()

    def _load(self):
        try:
            if self._path.exists():
                loaded = json.loads(self._path.read_text())
                self._data.update({k: v for k, v in loaded.items()
                                   if k in self.DEFAULTS})
        except Exception:
            pass

    def save(self):
        try:
            self._path.write_text(json.dumps(self._data, indent=2))
        except Exception:
            pass

    def get(self, key, default=None):
        return self._data.get(key, default if default is not None
                              else self.DEFAULTS.get(key))

    def set(self, key, value):
        if key in self.DEFAULTS:
            self._data[key] = value


class DP5SettingsDialog(QDialog):
    """Settings dialog for DP5 Workshop — does NOT touch global AppSettings."""

    def __init__(self, dp5_settings: DP5Settings, parent=None):
        super().__init__(parent)
        self.s = dp5_settings
        self.setWindowTitle("DP5 Workshop — Settings")
        self.setMinimumWidth(380)
        self.setModal(True)

        root = QVBoxLayout(self)
        tabs = QTabWidget()

        # ── Canvas tab ──────────────────────────────────────────────────────
        canvas_tab = QWidget()
        cl = QFormLayout(canvas_tab)
        cl.setSpacing(8)

        self._w_spin = QSpinBox(); self._w_spin.setRange(8, 4096)
        self._w_spin.setValue(self.s.get('default_width'))
        cl.addRow("Default width:", self._w_spin)

        self._h_spin = QSpinBox(); self._h_spin.setRange(8, 4096)
        self._h_spin.setValue(self.s.get('default_height'))
        cl.addRow("Default height:", self._h_spin)

        self._zoom_spin = QSpinBox(); self._zoom_spin.setRange(1, 16)
        self._zoom_spin.setValue(self.s.get('default_zoom'))
        cl.addRow("Default zoom:", self._zoom_spin)

        self._undo_spin = QSpinBox(); self._undo_spin.setRange(4, 128)
        self._undo_spin.setValue(self.s.get('undo_levels'))
        cl.addRow("Undo levels:", self._undo_spin)

        self._grid_chk = QCheckBox()
        self._grid_chk.setChecked(self.s.get('show_pixel_grid'))
        cl.addRow("Show pixel grid:", self._grid_chk)

        tabs.addTab(canvas_tab, "Canvas")

        # ── Interface tab ────────────────────────────────────────────────────
        ui_tab = QWidget()
        ul = QFormLayout(ui_tab)
        ul.setSpacing(8)

        self._bitmap_chk = QCheckBox()
        self._bitmap_chk.setChecked(self.s.get('show_bitmap_list'))
        ul.addRow("Show bitmap list panel:", self._bitmap_chk)

        self._icon_size_spin = QSpinBox(); self._icon_size_spin.setRange(24, 64)
        self._icon_size_spin.setValue(self.s.get('tool_icon_size'))
        ul.addRow("Tool icon size (px):", self._icon_size_spin)

        self._icon_color_combo = QComboBox()
        self._icon_color_combo.addItems(['color', 'white', 'dark'])
        idx = {'color': 0, 'white': 1, 'dark': 2}.get(
            self.s.get('tool_icon_color'), 0)
        self._icon_color_combo.setCurrentIndex(idx)
        ul.addRow("Tool icon colour:", self._icon_color_combo)

        self._cols_combo = QComboBox()
        self._cols_combo.addItems(['Auto (fit to panel)', '2 columns', '3 columns', '4 columns'])
        col_idx = {0: 0, 2: 1, 3: 2, 4: 3}.get(self.s.get('tool_columns'), 0)
        self._cols_combo.setCurrentIndex(col_idx)
        ul.addRow("Gadget bar columns:", self._cols_combo)

        tabs.addTab(ui_tab, "Interface")

        root.addWidget(tabs)

        # OK / Cancel
        btns = QHBoxLayout()
        btns.addStretch()
        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(self._accept)
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        btns.addWidget(ok_btn); btns.addWidget(cancel_btn)
        root.addLayout(btns)

    def _accept(self):
        self.s.set('default_width',    self._w_spin.value())
        self.s.set('default_height',   self._h_spin.value())
        self.s.set('default_zoom',     self._zoom_spin.value())
        self.s.set('undo_levels',      self._undo_spin.value())
        self.s.set('show_pixel_grid',  self._grid_chk.isChecked())
        self.s.set('show_bitmap_list', self._bitmap_chk.isChecked())
        self.s.set('tool_icon_size',   self._icon_size_spin.value())
        self.s.set('tool_icon_color',  self._icon_color_combo.currentText())
        self.s.set('tool_columns',     [0, 2, 3, 4][self._cols_combo.currentIndex()])
        self.s.save()
        self.accept()


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
        self.zoom       = 4        # can be float < 1 for zoom-out
        self.offset     = QPoint(0, 0)
        self.tool       = TOOL_PENCIL
        self.color      = QColor(255, 0, 0, 255)
        self.brush_size = 1
        self.show_grid  = True
        self._drawing   = False
        self._last_pt   = None
        self._preview_start = None
        self._preview_end   = None
        # Selection state (TOOL_SELECT / TOOL_LASSO)
        self._selection_rect: Optional[QRect] = None
        self._sel_active    = False
        self._lasso_pts:  List[QPoint] = []
        self.snap_grid      = False    # snap drawing coords to pixel grid
        self._editor    = None   # set by DP5Workshop after creation

        # Curve tool state
        self._curve_pts: List[QPoint] = []   # control points being placed
        # Polygon tool state
        self._polygon_sides = 6              # configurable N
        self._poly_pts: List[tuple] = []     # tex-coords being clicked
        # Text tool state
        self._text_input    = ""
        # Move / pan tool state (middle-button drag or TOOL_MOVE)
        self._pan_start:   Optional[QPoint] = None
        self._pan_offset_start: Optional[QPoint] = None
        self._space_panning = False   # True while space is held for temp pan
        # Selection clipboard
        self._sel_buffer: Optional[bytearray] = None  # copied RGBA bytes
        self._sel_buf_w  = 0
        self._sel_buf_h  = 0
        # Selection move / float state
        self._sel_floating   = False          # True while lifted and being dragged
        self._sel_float_orig: Optional[bytearray] = None  # canvas backup before lift
        self._sel_drag_start: Optional[Tuple[int,int]] = None  # tex-coords at drag start
        self._sel_float_pos:  Optional[Tuple[int,int]] = None  # current top-left in tex-space

        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)
        self.setMinimumSize(200, 200)

    def sizeHint(self):
        """Tell the scroll area exactly how big the zoomed canvas is."""
        z = max(0.01, self.zoom)
        return QSize(max(200, int(self.tex_w * z)),
                     max(200, int(self.tex_h * z)))

    # ── Coordinate helpers ────────────────────────────────────────────────────

    def _widget_to_tex(self, p: QPoint) -> Tuple[int, int]:
        z = max(0.01, self.zoom)
        x = int(p.x() / z)
        y = int(p.y() / z)
        if self.snap_grid and self.brush_size > 1:
            snap = self.brush_size
            x = (x // snap) * snap
            y = (y // snap) * snap
        return x, y

    def _tex_to_widget(self, tx: int, ty: int) -> QPoint:
        z = max(0.01, self.zoom)
        return QPoint(int(tx * z), int(ty * z))

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

    def draw_filled_rect(self, x0, y0, x1, y1, c: QColor):
        if x0 > x1: x0,x1 = x1,x0
        if y0 > y1: y0,y1 = y1,y0
        for yy in range(y0, y1+1):
            for xx in range(x0, x1+1):
                self.set_pixel(xx, yy, c)

    def draw_filled_circle(self, cx, cy, rx, ry, c: QColor):
        """Filled ellipse via scanline."""
        for yy in range(-ry, ry+1):
            if ry == 0: continue
            half_w = int(rx * (1 - (yy/ry)**2)**0.5)
            for xx in range(-half_w, half_w+1):
                self.set_pixel(cx+xx, cy+yy, c)

    def draw_triangle(self, x0, y0, x1, y1, x2, y2, c: QColor):
        """Outline triangle through three points."""
        self.draw_line(x0, y0, x1, y1, c)
        self.draw_line(x1, y1, x2, y2, c)
        self.draw_line(x2, y2, x0, y0, c)

    def draw_star(self, cx, cy, outer_r, inner_r, points, c: QColor):
        """Outline star polygon."""
        import math
        pts = []
        for i in range(points * 2):
            angle = math.pi * i / points - math.pi / 2
            r = outer_r if i % 2 == 0 else inner_r
            pts.append((int(cx + r * math.cos(angle)),
                        int(cy + r * math.sin(angle))))
        for i in range(len(pts)):
            x0,y0 = pts[i]
            x1,y1 = pts[(i+1) % len(pts)]
            self.draw_line(x0, y0, x1, y1, c)

    def draw_bezier_curve(self, pts_widget: List[QPoint], c: QColor):
        """
        Draw a cubic Bézier through widget-space control points onto the canvas.
        Rasterises 256 steps and plots with set_pixel_brush.
        """
        if len(pts_widget) < 2:
            return
        # Build control points in tex-space
        tp = [self._widget_to_tex(p) for p in pts_widget]
        # Pad to at least 4 control points by repeating endpoints
        while len(tp) < 4:
            tp = [tp[0]] + tp + [tp[-1]]

        steps = 512
        prev  = None
        for i in range(steps + 1):
            t   = i / steps
            t2  = t * t; t3 = t2 * t
            mt  = 1 - t; mt2 = mt * mt; mt3 = mt2 * mt
            x = int(mt3*tp[0][0] + 3*mt2*t*tp[1][0] +
                    3*mt*t2*tp[2][0] + t3*tp[3][0])
            y = int(mt3*tp[0][1] + 3*mt2*t*tp[1][1] +
                    3*mt*t2*tp[2][1] + t3*tp[3][1])
            if prev:
                self.draw_line(prev[0], prev[1], x, y, c)
            prev = (x, y)

    def draw_regular_polygon(self, cx, cy, radius, sides, c: QColor):
        """Draw a regular N-sided polygon outline."""
        import math
        pts = []
        for i in range(sides):
            a = math.pi * 2 * i / sides - math.pi / 2
            pts.append((int(cx + radius * math.cos(a)),
                        int(cy + radius * math.sin(a))))
        for i in range(len(pts)):
            x0,y0 = pts[i]
            x1,y1 = pts[(i+1) % len(pts)]
            self.draw_line(x0, y0, x1, y1, c)

    # ── Selection clipboard ops ───────────────────────────────────────────────

    def copy_selection(self):
        """Copy the selected rectangle to the internal buffer."""
        if not self._sel_active or not self._selection_rect: return
        r = self._selection_rect
        x0 = max(0, r.x()); y0 = max(0, r.y())
        x1 = min(self.tex_w, r.x() + r.width())
        y1 = min(self.tex_h, r.y() + r.height())
        w  = x1 - x0; h = y1 - y0
        if w <= 0 or h <= 0: return
        buf = bytearray(w * h * 4)
        for row in range(h):
            for col in range(w):
                si = ((y0 + row) * self.tex_w + (x0 + col)) * 4
                di = (row * w + col) * 4
                buf[di:di+4] = self.rgba[si:si+4]
        self._sel_buffer = buf
        self._sel_buf_w  = w
        self._sel_buf_h  = h

    def cut_selection(self):
        """Copy selected area then clear it to transparent."""
        self.copy_selection()
        if not self._sel_active or not self._selection_rect: return
        r = self._selection_rect
        x0 = max(0, r.x()); y0 = max(0, r.y())
        x1 = min(self.tex_w, r.x() + r.width())
        y1 = min(self.tex_h, r.y() + r.height())
        for row in range(y0, y1):
            for col in range(x0, x1):
                i = (row * self.tex_w + col) * 4
                self.rgba[i:i+4] = b'\x00\x00\x00\x00'
        self.update()

    def paste_selection(self, dx: int = 0, dy: int = 0):
        """Paste the clipboard buffer at (dx, dy) offset from selection origin."""
        if not self._sel_buffer: return
        x0 = (self._selection_rect.x() if self._sel_active and self._selection_rect
               else dx)
        y0 = (self._selection_rect.y() if self._sel_active and self._selection_rect
               else dy)
        x0 += dx; y0 += dy
        w, h = self._sel_buf_w, self._sel_buf_h
        for row in range(h):
            for col in range(w):
                tx = x0 + col; ty = y0 + row
                if 0 <= tx < self.tex_w and 0 <= ty < self.tex_h:
                    si = (row * w + col) * 4
                    di = (ty * self.tex_w + tx) * 4
                    src_a = self._sel_buffer[si+3]
                    if src_a == 0: continue
                    self.rgba[di:di+4] = self._sel_buffer[si:si+4]
        self._sel_active = True
        self._selection_rect = QRect(x0 - dx, y0 - dy, w, h)
        self.update()

    # ── Selection move helpers ────────────────────────────────────────────────

    def _point_in_sel_rect(self, tx: int, ty: int) -> bool:
        """True if tex-coord (tx,ty) is inside the committed selection rect."""
        if not self._sel_active or not self._selection_rect:
            return False
        r = self._selection_rect
        return r.x() <= tx < r.x() + r.width() and r.y() <= ty < r.y() + r.height()

    def _lift_selection(self):
        """
        Lift the selected pixels off the canvas into _sel_buffer and clear the
        source area to transparent.  Saves a full canvas backup in _sel_float_orig
        so Escape can cancel the move.
        """
        if not self._sel_active or not self._selection_rect: return
        # Backup whole canvas for cancel
        self._sel_float_orig = bytearray(self.rgba)
        # Copy selected region
        self.copy_selection()
        # Clear source region (lift leaves a hole)
        r = self._selection_rect
        x0 = max(0, r.x()); y0 = max(0, r.y())
        x1 = min(self.tex_w, r.x() + r.width())
        y1 = min(self.tex_h, r.y() + r.height())
        for row in range(y0, y1):
            for col in range(x0, x1):
                i = (row * self.tex_w + col) * 4
                self.rgba[i:i+4] = b'\x00\x00\x00\x00'
        self._sel_floating  = True
        self._sel_float_pos = (r.x(), r.y())

    def _stamp_selection(self, keep_floating: bool = False):
        """
        Blit the floating buffer at _sel_float_pos onto the canvas.
        If keep_floating is True the buffer stays in memory (still draggable).
        """
        if not self._sel_buffer or not self._sel_float_pos: return
        ox, oy = self._sel_float_pos
        w, h   = self._sel_buf_w, self._sel_buf_h
        for row in range(h):
            for col in range(w):
                tx = ox + col; ty = oy + row
                if 0 <= tx < self.tex_w and 0 <= ty < self.tex_h:
                    si = (row * w + col) * 4
                    di = (ty * self.tex_w + tx) * 4
                    if self._sel_buffer[si+3] == 0: continue
                    self.rgba[di:di+4] = self._sel_buffer[si:si+4]
        if not keep_floating:
            self._sel_floating   = False
            self._sel_float_orig = None
            self._selection_rect = QRect(ox, oy, w, h)
        self.update()

    def cancel_sel_move(self):
        """Escape while floating — restore canvas to pre-lift state."""
        if self._sel_floating and self._sel_float_orig:
            self.rgba[:] = self._sel_float_orig
            self._sel_floating   = False
            self._sel_float_orig = None
        self.update()

    def nudge_float(self, dx: int, dy: int):
        """Move the floating object by (dx, dy) pixels in tex-space."""
        if not self._sel_floating or not self._sel_float_pos:
            return
        ox, oy = self._sel_float_pos
        self._sel_float_pos = (ox + dx, oy + dy)
        if self._sel_active and self._selection_rect:
            r = self._selection_rect
            self._selection_rect = QRect(r.x()+dx, r.y()+dy, r.width(), r.height())
        self.update()

    def _get_scroll_area(self):
        """Walk up the parent chain to find the QScrollArea containing this canvas."""
        p = self.parent()
        while p is not None:
            if isinstance(p, QScrollArea):
                return p
            p = p.parent() if callable(getattr(p, 'parent', None)) else None
        return None

    def _scroll_by(self, dx: int, dy: int):
        """Pan the scroll area by (dx, dy) pixels."""
        sa = self._get_scroll_area()
        if sa:
            hb = sa.horizontalScrollBar()
            vb = sa.verticalScrollBar()
            hb.setValue(hb.value() + dx)
            vb.setValue(vb.value() + dy)

    def _do_spray(self, cx: int, cy: int):
        """Spray paint random pixels in a circle around (cx, cy)."""
        r = self.brush_size * 5
        for _ in range(max(1, r)):
            ddx, ddy = random.randint(-r, r), random.randint(-r, r)
            if ddx*ddx + ddy*ddy <= r*r:
                self.set_pixel(cx + ddx, cy + ddy, self.color)

    # ── Paint ─────────────────────────────────────────────────────────────────

    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, False)

        z  = max(0.01, self.zoom)
        sw = max(1, int(self.tex_w * z))
        sh = max(1, int(self.tex_h * z))

        img = QImage(bytes(self.rgba), self.tex_w, self.tex_h,
                     self.tex_w * 4, QImage.Format.Format_RGBA8888)
        scaled = img.scaled(sw, sh,
                             Qt.AspectRatioMode.IgnoreAspectRatio,
                             Qt.TransformationMode.FastTransformation)
        # Draw at (0,0) — scroll area handles viewport offset via scrollbars
        painter.drawImage(0, 0, scaled)

        # Pixel grid (only at zoom ≥4)
        if self.show_grid and z >= 4:
            iz = int(z)
            pen = QPen(QColor(128, 128, 128, 60), 1)
            painter.setPen(pen)
            for x in range(0, sw, iz):
                painter.drawLine(x, 0, x, sh)
            for y in range(0, sh, iz):
                painter.drawLine(0, y, sw, y)

        # Shape / selection preview overlay (drag-to-draw tools only)
        shape_tools = (TOOL_LINE,
                       TOOL_RECT,     TOOL_FILLED_RECT,
                       TOOL_CIRCLE,   TOOL_FILLED_CIRCLE,
                       TOOL_TRIANGLE, TOOL_FILLED_TRIANGLE,
                       TOOL_STAR,     TOOL_FILLED_STAR,
                       TOOL_SELECT)
        if self._preview_start and self._preview_end and self.tool in shape_tools:
            pen = QPen(self.color, 1, Qt.PenStyle.DashLine)
            painter.setPen(pen)
            painter.setBrush(Qt.BrushStyle.NoBrush)
            s = self._tex_to_widget(*self._preview_start)
            e = self._tex_to_widget(*self._preview_end)
            if self.tool == TOOL_LINE:
                painter.drawLine(s.x(), s.y(), e.x(), e.y())
            elif self.tool in (TOOL_RECT, TOOL_FILLED_RECT, TOOL_SELECT):
                painter.drawRect(QRect(s, e).normalized())
            elif self.tool in (TOOL_CIRCLE, TOOL_FILLED_CIRCLE):
                painter.drawEllipse(QRect(s, e).normalized())
            elif self.tool in (TOOL_TRIANGLE, TOOL_FILLED_TRIANGLE):
                mid_x = (s.x() + e.x()) // 2
                from PyQt6.QtGui import QPolygon
                tri = QPolygon([QPoint(mid_x, s.y()), s, e])
                painter.drawPolygon(tri)
            elif self.tool in (TOOL_STAR, TOOL_FILLED_STAR):
                painter.drawEllipse(QRect(s, e).normalized())

        # Polygon tool — draw committed edges + line to current mouse position
        if self.tool in (TOOL_POLYGON, TOOL_FILLED_POLYGON) and self._poly_pts:
            pen = QPen(self.color, 1, Qt.PenStyle.SolidLine)
            painter.setPen(pen)
            painter.setBrush(Qt.BrushStyle.NoBrush)
            pts_w = [self._tex_to_widget(px, py) for px, py in self._poly_pts]
            # Draw committed edges
            for i in range(len(pts_w) - 1):
                painter.drawLine(pts_w[i], pts_w[i+1])
            # Dashed closing line back to first point
            if len(pts_w) >= 2:
                pen2 = QPen(self.color, 1, Qt.PenStyle.DashLine)
                painter.setPen(pen2)
                painter.drawLine(pts_w[-1], pts_w[0])
            # Small dot at each vertex
            painter.setPen(QPen(self.color, 1))
            painter.setBrush(QBrush(self.color))
            for pt in pts_w:
                painter.drawEllipse(pt, 3, 3)

        # Curve tool — live preview of control points + curve
        if self.tool == TOOL_CURVE and self._curve_pts:
            pen = QPen(self.color, 1, Qt.PenStyle.DashLine)
            painter.setPen(pen)
            if len(self._curve_pts) >= 2:
                path = QPainterPath()
                path.moveTo(float(self._curve_pts[0].x()),
                            float(self._curve_pts[0].y()))
                cps = list(self._curve_pts)
                while len(cps) < 4:
                    cps = [cps[0]] + cps + [cps[-1]]
                path.cubicTo(float(cps[1].x()), float(cps[1].y()),
                             float(cps[2].x()), float(cps[2].y()),
                             float(cps[3].x()), float(cps[3].y()))
                painter.drawPath(path)
            # Draw control point handles
            painter.setPen(QPen(QColor(255, 200, 0), 1))
            painter.setBrush(QBrush(QColor(255, 200, 0, 120)))
            for pt in self._curve_pts:
                painter.drawEllipse(QPoint(pt.x(), pt.y()), 4, 4)

        # Lasso preview
        if self.tool in (TOOL_LASSO, TOOL_FILLED_LASSO) and len(self._lasso_pts) > 1:
            pen = QPen(self.color, 1,
                       Qt.PenStyle.SolidLine if self.tool == TOOL_FILLED_LASSO
                       else Qt.PenStyle.DashLine)
            painter.setPen(pen)
            for i in range(len(self._lasso_pts) - 1):
                painter.drawLine(self._lasso_pts[i], self._lasso_pts[i+1])
            # Closing line back to start
            if len(self._lasso_pts) >= 3:
                pen2 = QPen(self.color, 1, Qt.PenStyle.DashLine)
                painter.setPen(pen2)
                painter.drawLine(self._lasso_pts[-1], self._lasso_pts[0])

        # Committed selection rect (marching ants)
        if self._sel_active and self._selection_rect:
            pen = QPen(QColor(0, 180, 255), 1, Qt.PenStyle.DashLine)
            painter.setPen(pen)
            painter.setBrush(Qt.BrushStyle.NoBrush)
            r   = self._selection_rect
            sx  = int(r.x()      * z)
            sy  = int(r.y()      * z)
            sw2 = int(r.width()  * z)
            sh2 = int(r.height() * z)
            painter.drawRect(QRect(sx, sy, sw2, sh2))

        # Floating selection — render buffer at current drag position
        if self._sel_floating and self._sel_buffer and self._sel_float_pos:
            ox, oy = self._sel_float_pos
            w2, h2 = self._sel_buf_w, self._sel_buf_h
            fimg = QImage(bytes(self._sel_buffer), w2, h2,
                          w2 * 4, QImage.Format.Format_RGBA8888)
            fw = max(1, int(w2 * z)); fh = max(1, int(h2 * z))
            scaled_f = fimg.scaled(fw, fh,
                                   Qt.AspectRatioMode.IgnoreAspectRatio,
                                   Qt.TransformationMode.FastTransformation)
            fx = int(ox * z)
            fy = int(oy * z)
            painter.setOpacity(0.85)
            painter.drawImage(fx, fy, scaled_f)
            painter.setOpacity(1.0)
            pen2 = QPen(QColor(255, 220, 0), 1, Qt.PenStyle.DashLine)
            painter.setPen(pen2)
            painter.setBrush(Qt.BrushStyle.NoBrush)
            painter.drawRect(QRect(fx, fy, fw, fh))

        # Text cursor indicator
        if self.tool == TOOL_TEXT and hasattr(self, '_text_cursor_pos'):
            tc = self._text_cursor_pos
            wx = int(tc[0] * z)
            wy = int(tc[1] * z)
            painter.setPen(QPen(self.color, 1))
            painter.drawLine(wx, wy, wx, wy + max(8, int(12 * z)))

        # Stamp ghost — show buffer preview under cursor at 50% opacity
        if self.tool == TOOL_STAMP and self._sel_buffer and self._sel_buf_w > 0:
            if hasattr(self, '_stamp_cursor_pos'):
                scx, scy = self._stamp_cursor_pos
                sw2 = max(1, int(self._sel_buf_w * z))
                sh2 = max(1, int(self._sel_buf_h * z))
                simg = QImage(bytes(self._sel_buffer),
                              self._sel_buf_w, self._sel_buf_h,
                              self._sel_buf_w * 4, QImage.Format.Format_RGBA8888)
                sscaled = simg.scaled(sw2, sh2,
                                      Qt.AspectRatioMode.IgnoreAspectRatio,
                                      Qt.TransformationMode.FastTransformation)
                painter.setOpacity(0.55)
                painter.drawImage(int(scx * z), int(scy * z), sscaled)
                painter.setOpacity(1.0)
                # Dashed border
                painter.setPen(QPen(QColor('#00e5ff'), 1, Qt.PenStyle.DashLine))
                painter.setBrush(Qt.BrushStyle.NoBrush)
                painter.drawRect(int(scx*z), int(scy*z), sw2, sh2)

    # ── Mouse events ──────────────────────────────────────────────────────────

    def mousePressEvent(self, e: QMouseEvent):
        btn = e.button()
        tx, ty = self._widget_to_tex(e.position().toPoint())

        # Middle button always pans
        if btn == Qt.MouseButton.MiddleButton:
            self._pan_start = e.position().toPoint()
            return

        # Spacebar + left click = temporary pan
        if btn == Qt.MouseButton.LeftButton and self._space_panning:
            self._pan_start = e.position().toPoint()
            self.setCursor(Qt.CursorShape.ClosedHandCursor)
            return

        # Right-click zoom-out for zoom tool
        if btn == Qt.MouseButton.RightButton and self.tool == TOOL_ZOOM:
            ed = self._editor
            if ed: ed._set_zoom(max(0.05, self._editor._canvas_zoom * 0.5))
            return

        if btn != Qt.MouseButton.LeftButton:
            return

        self._drawing = True
        self._last_pt = (tx, ty)

        if self.tool == TOOL_PENCIL:
            self._push_undo_canvas()
            self.set_pixel_brush(tx, ty, self.color); self.update()

        elif self.tool == TOOL_ERASER:
            self._push_undo_canvas()
            self.set_pixel_brush(tx, ty, QColor(0,0,0,0)); self.update()

        elif self.tool == TOOL_FILL:
            self._push_undo_canvas()
            self.flood_fill(tx, ty, self.color); self.update()

        elif self.tool == TOOL_SPRAY:
            self._push_undo_canvas()
            self._do_spray(tx, ty); self.update()

        elif self.tool == TOOL_PICKER:
            c = self.get_pixel(tx, ty)
            if c.isValid():
                self.color = c
                ed = self._editor
                if ed: ed._update_color_swatches()

        elif self.tool == TOOL_ZOOM:
            ed = self._editor
            if ed: ed._set_zoom(min(16, self._editor._canvas_zoom * 2))

        elif self.tool == TOOL_MOVE:
            if self._sel_buffer and self._sel_buf_w > 0:
                # ── Floating object mode ──
                # If not already floating, make it float at current pos or centre
                if not self._sel_floating:
                    if not self._sel_float_pos:
                        # Default position: canvas centre
                        cx = max(0, self.tex_w // 2 - self._sel_buf_w // 2)
                        cy = max(0, self.tex_h // 2 - self._sel_buf_h // 2)
                        self._sel_float_pos = (cx, cy)
                        if self._sel_active and self._selection_rect:
                            self._sel_float_pos = (self._selection_rect.x(),
                                                   self._selection_rect.y())
                    self._sel_floating = True
                self._sel_drag_start = (tx, ty)
                self._drawing = True
            else:
                # ── Pan mode (no object to move) ──
                self._pan_start = e.position().toPoint()

        elif self.tool == TOOL_TEXT:
            # Show inline text input dialog
            self._text_cursor_pos = (tx, ty)
            self.update()
            ed = self._editor
            if ed: ed._place_text_at(tx, ty)

        elif self.tool == TOOL_STAMP:
            # Stamp the copy buffer at click position (top-left = click point)
            if self._sel_buffer and self._sel_buf_w > 0:
                self._push_undo_canvas()
                w, h = self._sel_buf_w, self._sel_buf_h
                for row in range(h):
                    for col in range(w):
                        px = tx + col; py = ty + row
                        if 0 <= px < self.tex_w and 0 <= py < self.tex_h:
                            si = (row * w + col) * 4
                            if self._sel_buffer[si+3] > 0:
                                di = (py * self.tex_w + px) * 4
                                self.rgba[di:di+4] = self._sel_buffer[si:si+4]
                self.update()

        elif self.tool == TOOL_CURVE:
            # Accumulate control points; double-click or Enter commits
            self._curve_pts.append(e.position().toPoint())
            self.update()

        elif self.tool in (TOOL_FILLED_POLYGON, TOOL_POLYGON):
            # Accumulate polygon vertices; double-click closes
            self._poly_pts.append((tx, ty))
            self.update()

        elif self.tool in (TOOL_LINE,
                           TOOL_RECT,     TOOL_FILLED_RECT,
                           TOOL_CIRCLE,   TOOL_FILLED_CIRCLE,
                           TOOL_TRIANGLE, TOOL_FILLED_TRIANGLE,
                           TOOL_STAR,     TOOL_FILLED_STAR,
                           TOOL_SELECT):

            if self.tool == TOOL_SELECT and self._sel_active and \
               self._point_in_sel_rect(tx, ty):
                # ── Click INSIDE committed selection → lift and start move ──
                if not self._sel_floating:
                    self._push_undo_canvas()
                    self._lift_selection()
                self._sel_drag_start = (tx, ty)
                self._drawing = True
            else:
                # ── Click OUTSIDE → stamp any float, start new selection ──
                if self._sel_floating:
                    self._stamp_selection(keep_floating=False)
                self._preview_start = (tx, ty)
                self._preview_end   = (tx, ty)
                self._sel_active    = False
                self._sel_floating  = False

        elif self.tool in (TOOL_LASSO, TOOL_FILLED_LASSO):
            self._lasso_pts  = [e.position().toPoint()]
            self._sel_active = False

    def mouseDoubleClickEvent(self, e: QMouseEvent):
        """Double-click commits curve/polygon."""
        if e.button() != Qt.MouseButton.LeftButton: return

        if self.tool == TOOL_CURVE and len(self._curve_pts) >= 2:
            self._push_undo_canvas()
            self.draw_bezier_curve(self._curve_pts, self.color)
            self._curve_pts = []
            self.update()

        elif self.tool in (TOOL_POLYGON, TOOL_FILLED_POLYGON) and len(self._poly_pts) >= 3:
            self._push_undo_canvas()
            for i in range(len(self._poly_pts)):
                x0,y0 = self._poly_pts[i]
                x1,y1 = self._poly_pts[(i+1) % len(self._poly_pts)]
                self.draw_line(x0, y0, x1, y1, self.color)
            if self.tool == TOOL_FILLED_POLYGON:
                # Flood fill the interior from centroid
                cx = sum(p[0] for p in self._poly_pts) // len(self._poly_pts)
                cy = sum(p[1] for p in self._poly_pts) // len(self._poly_pts)
                self.flood_fill(cx, cy, self.color)
            self._poly_pts = []
            self.update()

    def keyPressEvent(self, e):
        if e.key() == Qt.Key.Key_Space and not e.isAutoRepeat():
            self._space_panning = True
            self.setCursor(Qt.CursorShape.OpenHandCursor)
        else:
            e.ignore()   # let parent handle all other keys

    def keyReleaseEvent(self, e):
        if e.key() == Qt.Key.Key_Space and not e.isAutoRepeat():
            self._space_panning = False
            self.setCursor(Qt.CursorShape.ArrowCursor)
        else:
            e.ignore()

    def _push_undo_canvas(self):
        """Push undo state from within the canvas (used by tool handlers)."""
        ed = self._editor
        if ed and hasattr(ed, '_push_undo'):
            ed._push_undo()

    def mouseMoveEvent(self, e: QMouseEvent):
        tx, ty = self._widget_to_tex(e.position().toPoint())
        ed = self._editor
        if ed and hasattr(ed, '_update_status'):
            ed._update_status(tx, ty, self.get_pixel(tx, ty))

        # Stamp ghost always tracks mouse when stamp tool active
        if self.tool == TOOL_STAMP:
            self._stamp_cursor_pos = (tx, ty)
            self.update()

        # Middle-button pan
        if e.buttons() & Qt.MouseButton.MiddleButton:
            if self._pan_start:
                delta = e.position().toPoint() - self._pan_start
                self._scroll_by(-delta.x(), -delta.y())
                self._pan_start = e.position().toPoint()
            return

        # Spacebar pan
        if self._space_panning and e.buttons() & Qt.MouseButton.LeftButton:
            if self._pan_start:
                delta = e.position().toPoint() - self._pan_start
                self._scroll_by(-delta.x(), -delta.y())
                self._pan_start = e.position().toPoint()
                self.setCursor(Qt.CursorShape.ClosedHandCursor)
            return

        if not (e.buttons() & Qt.MouseButton.LeftButton) or not self._drawing:
            if self.tool == TOOL_CURVE and self._curve_pts:
                self.update()   # live preview tracks mouse
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

        elif self.tool == TOOL_MOVE:
            if self._sel_floating and self._sel_drag_start:
                # Drag the floating object
                dx = tx - self._sel_drag_start[0]
                dy = ty - self._sel_drag_start[1]
                if self._sel_float_pos:
                    ox, oy = self._sel_float_pos
                    self._sel_float_pos = (ox + dx, oy + dy)
                    if self._sel_active and self._selection_rect:
                        r = self._selection_rect
                        self._selection_rect = QRect(r.x()+dx, r.y()+dy,
                                                     r.width(), r.height())
                self._sel_drag_start = (tx, ty)
                self.update()
            elif self._pan_start:
                delta = e.position().toPoint() - self._pan_start
                self._scroll_by(-delta.x(), -delta.y())
                self._pan_start = e.position().toPoint()

        elif self.tool == TOOL_SELECT and self._sel_floating and self._sel_drag_start:
            # ── Dragging a floating selection ──
            dx = tx - self._sel_drag_start[0]
            dy = ty - self._sel_drag_start[1]
            r  = self._selection_rect
            if r:
                new_x = r.x() + dx
                new_y = r.y() + dy
                self._sel_float_pos = (new_x, new_y)
                self._selection_rect = QRect(new_x, new_y, r.width(), r.height())
                self._sel_drag_start = (tx, ty)   # update for smooth incremental move
            self.update()

        elif self.tool in (TOOL_LINE,
                           TOOL_RECT,     TOOL_FILLED_RECT,
                           TOOL_CIRCLE,   TOOL_FILLED_CIRCLE,
                           TOOL_TRIANGLE, TOOL_FILLED_TRIANGLE,
                           TOOL_STAR,     TOOL_FILLED_STAR,
                           TOOL_SELECT,
                           TOOL_POLYGON,  TOOL_FILLED_POLYGON):
            self._preview_end = (tx, ty); self.update()

        elif self.tool in (TOOL_LASSO, TOOL_FILLED_LASSO):
            self._lasso_pts.append(e.position().toPoint()); self.update()

    def mouseReleaseEvent(self, e: QMouseEvent):
        if e.button() == Qt.MouseButton.MiddleButton:
            self._pan_start = None
            return

        if e.button() == Qt.MouseButton.LeftButton and self._space_panning:
            self._pan_start = None
            self.setCursor(Qt.CursorShape.OpenHandCursor)
            return

        if e.button() != Qt.MouseButton.LeftButton:
            return

        tx, ty = self._widget_to_tex(e.position().toPoint())
        ps = self._preview_start

        if self.tool == TOOL_MOVE:
            if self._sel_floating and self._sel_drag_start:
                self._sel_drag_start = None   # end drag, float stays
            else:
                self._pan_start = None

        elif self.tool == TOOL_LINE and ps:
            self._push_undo_canvas()
            self.draw_line(ps[0], ps[1], tx, ty, self.color)

        elif self.tool == TOOL_RECT and ps:
            self._push_undo_canvas()
            self.draw_rect(ps[0], ps[1], tx, ty, self.color)

        elif self.tool == TOOL_FILLED_RECT and ps:
            self._push_undo_canvas()
            self.draw_filled_rect(ps[0], ps[1], tx, ty, self.color)

        elif self.tool == TOOL_CIRCLE and ps:
            self._push_undo_canvas()
            rx = abs(tx - ps[0]); ry = abs(ty - ps[1])
            self.draw_circle(ps[0], ps[1], max(1,rx), max(1,ry), self.color)

        elif self.tool == TOOL_FILLED_CIRCLE and ps:
            self._push_undo_canvas()
            rx = abs(tx - ps[0]); ry = abs(ty - ps[1])
            self.draw_filled_circle(ps[0], ps[1], max(1,rx), max(1,ry), self.color)

        elif self.tool == TOOL_TRIANGLE and ps:
            self._push_undo_canvas()
            mid_x = (ps[0] + tx) // 2
            self.draw_triangle(mid_x, ps[1], ps[0], ty, tx, ty, self.color)

        elif self.tool == TOOL_FILLED_TRIANGLE and ps:
            self._push_undo_canvas()
            mid_x = (ps[0] + tx) // 2
            # Filled triangle via scanline flood
            self.draw_triangle(mid_x, ps[1], ps[0], ty, tx, ty, self.color)
            self.flood_fill(mid_x, (ps[1] + ty) // 2, self.color)

        elif self.tool == TOOL_POLYGON and ps:
            # Polygon is committed via double-click; drag just updates preview
            pass

        elif self.tool == TOOL_FILLED_POLYGON and ps:
            pass

        elif self.tool == TOOL_STAR and ps:
            self._push_undo_canvas()
            rx = abs(tx - ps[0]); ry = abs(ty - ps[1])
            r_outer = max(rx, ry, 2)
            self.draw_star(ps[0], ps[1], r_outer, r_outer//2, 5, self.color)

        elif self.tool == TOOL_FILLED_STAR and ps:
            self._push_undo_canvas()
            rx = abs(tx - ps[0]); ry = abs(ty - ps[1])
            r_outer = max(rx, ry, 2)
            self.draw_star(ps[0], ps[1], r_outer, r_outer//2, 5, self.color)
            self.flood_fill(ps[0], ps[1], self.color)

        elif self.tool == TOOL_SELECT:
            if self._sel_floating and self._sel_drag_start:
                # ── End of floating drag — stamp non-destructively and stay floating ──
                # (User can drag again; clicking outside will stamp permanently)
                self._sel_drag_start = None
                # keep _sel_floating = True so they can reposition
            elif ps:
                # ── New marquee drawn ──
                x0,y0 = min(ps[0],tx), min(ps[1],ty)
                x1,y1 = max(ps[0],tx), max(ps[1],ty)
                if x1 > x0 and y1 > y0:
                    self._selection_rect = QRect(x0, y0, x1-x0, y1-y0)
                    self._sel_active     = True
                    self._sel_floating   = False

        elif self.tool in (TOOL_LASSO, TOOL_FILLED_LASSO):
            if self.tool == TOOL_FILLED_LASSO and len(self._lasso_pts) >= 3:
                # Draw outline then flood fill from centroid
                self._push_undo_canvas()
                z = max(0.01, self.zoom)
                pts_tex = [self._widget_to_tex(pt) for pt in self._lasso_pts]
                for i in range(len(pts_tex)):
                    x0, y0 = pts_tex[i]
                    x1, y1 = pts_tex[(i + 1) % len(pts_tex)]
                    self.draw_line(x0, y0, x1, y1, self.color)
                # Flood fill from centroid
                cx = sum(p[0] for p in pts_tex) // len(pts_tex)
                cy = sum(p[1] for p in pts_tex) // len(pts_tex)
                self.flood_fill(cx, cy, self.color)
            self._lasso_pts  = []
            self._sel_active = False

        self._drawing = False
        self._preview_start = self._preview_end = None
        self._last_pt = None
        self.update()
        self.pixel_changed.emit(tx, ty)

    def wheelEvent(self, e: QWheelEvent):
        if not (e.modifiers() & Qt.KeyboardModifier.ControlModifier):
            return   # pass non-Ctrl scroll to scroll area for panning
        d        = e.angleDelta().y()
        old_zoom = self.zoom

        # Calculate new zoom level
        if old_zoom >= 1:
            new_zoom = min(16, old_zoom + 1) if d > 0 else max(1, old_zoom - 1)
        else:
            step = 0.1
            new_zoom = min(1.0, round(old_zoom + step, 2)) if d > 0 \
                       else max(0.05, round(old_zoom - step, 2))

        if new_zoom == old_zoom:
            return

        # Ask the editor to apply zoom with mouse position as anchor
        ed = self._editor
        if ed and hasattr(ed, '_set_zoom'):
            mouse_in_canvas = e.position().toPoint()
            sa = getattr(ed, '_canvas_scroll', None)
            scroll_anchor = (self.mapTo(sa.viewport(), mouse_in_canvas)
                             if sa else None)
            ed._set_zoom(new_zoom, anchor_widget_pos=scroll_anchor)
        else:
            self.zoom = new_zoom
            self.update()


# ══════════════════════════════════════════════════════════════════════════════
#  PaletteGrid — shared 2D swatch grid used for BOTH image and user palettes
# ══════════════════════════════════════════════════════════════════════════════

class PaletteGrid(QWidget):
    """
    2D colour swatch grid that fills its available area.
    Used for both the image palette (16 cols, auto-rows) and the
    user/retro preset palette (cols vary by preset).
    """

    color_picked = pyqtSignal(QColor)

    _DEFAULT_ENTRIES = [
        (0,0,0),(255,255,255),(255,0,0),(0,255,0),(0,0,255),(255,255,0),
        (255,0,255),(0,255,255),(128,0,0),(0,128,0),(0,0,128),(128,128,0),
        (128,0,128),(0,128,128),(192,192,192),(128,128,128),
        (255,128,0),(0,255,128),(128,0,255),(255,0,128),
        (255,128,128),(128,255,128),(128,128,255),(255,255,128),
        (64,64,64),(160,160,160),(200,100,50),(100,200,50),
        (50,100,200),(200,50,100),(100,50,200),(50,200,100),
    ]

    def __init__(self, cols: int = 16, cell: int = 13, parent=None):
        super().__init__(parent)
        self._cols     = cols
        self._cell     = cell      # swatch size in pixels
        self._colors: List[QColor] = [QColor(*e) for e in self._DEFAULT_ENTRIES]
        self._selected = -1
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self.setToolTip("Click to select colour — right-click to copy hex")
        self._recalc_height()

    def _recalc_height(self):
        rows = max(1, (len(self._colors) + self._cols - 1) // self._cols)
        self.setFixedHeight(rows * self._cell + 1)

    def set_colors(self, colors: List[QColor], cols: int = None):
        """Load a new colour list, optionally changing column count."""
        if cols is not None:
            self._cols = cols
        self._colors   = list(colors)
        self._selected = -1
        self._recalc_height()
        self.update()

    def set_palette_raw(self, palette_data):
        """Accept list of (r,g,b) tuples, QColors or hex strings."""
        out = []
        for entry in palette_data[:256]:
            if isinstance(entry, QColor):
                out.append(entry)
            elif isinstance(entry, str):
                out.append(QColor(entry))
            elif hasattr(entry, '__len__') and len(entry) >= 3:
                out.append(QColor(entry[0], entry[1], entry[2]))
        self._colors   = out
        self._selected = -1
        self._recalc_height()
        self.update()

    def set_selection_by_color(self, c: QColor):
        for i, p in enumerate(self._colors):
            if p.rgb() == c.rgb():
                self._selected = i
                self.update()
                return

    # ── Paint ─────────────────────────────────────────────────────────────────

    def paintEvent(self, event):
        p   = QPainter(self)
        cs  = self._cell
        for i, col in enumerate(self._colors):
            x = (i % self._cols) * cs
            y = (i // self._cols) * cs
            p.fillRect(x, y, cs - 1, cs - 1, col)
            if i == self._selected:
                # White inner highlight + dark outer border for visibility on
                # both light and dark swatches
                p.setPen(QPen(QColor(0, 0, 0), 1))
                p.drawRect(x, y, cs - 2, cs - 2)
                p.setPen(QPen(QColor(255, 255, 255), 1))
                p.drawRect(x + 1, y + 1, cs - 4, cs - 4)

    # ── Mouse ─────────────────────────────────────────────────────────────────

    def mousePressEvent(self, e: QMouseEvent):
        cs  = self._cell
        col = e.position().toPoint().x() // cs
        row = e.position().toPoint().y() // cs
        idx = row * self._cols + col
        if 0 <= idx < len(self._colors):
            self._selected = idx
            self.color_picked.emit(self._colors[idx])
            self.update()

    # ── Compat alias (legacy callers used set_palette) ────────────────────────

    def set_palette(self, palette_data):
        self.set_palette_raw(palette_data)


# Keep an alias so any stale references to DP5PaletteBar still resolve
DP5PaletteBar = PaletteGrid


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
#  FGBGSwatch — single DPaint5-style nested FG/BG colour indicator
# ══════════════════════════════════════════════════════════════════════════════

class FGBGSwatch(QWidget):
    """
    Classic DPaint5 nested colour swatch:
      - outer rect = Background colour  (click outer area to pick BG)
      - inner rect = Foreground colour  (click inner area to pick FG)
    Swap button (S key / double-click) swaps FG ↔ BG.
    """

    fg_changed = pyqtSignal(QColor)
    bg_changed = pyqtSignal(QColor)

    def __init__(self, parent=None):
        super().__init__(parent)
        self._fg = QColor(255, 0,   0,   255)
        self._bg = QColor(0,   0,   0,   255)
        self.setFixedSize(64, 48)
        self.setToolTip(
            "FG (inner) / BG (outer)\n"
            "Click inner area → pick FG\n"
            "Click outer area → pick BG\n"
            "Double-click → swap FG↔BG")

    # ── Properties ────────────────────────────────────────────────────────────

    @property
    def fg(self) -> QColor: return QColor(self._fg)
    @property
    def bg(self) -> QColor: return QColor(self._bg)

    def set_fg(self, c: QColor):
        self._fg = QColor(c); self.update(); self.fg_changed.emit(self._fg)
    def set_bg(self, c: QColor):
        self._bg = QColor(c); self.update(); self.bg_changed.emit(self._bg)

    def swap(self):
        self._fg, self._bg = self._bg, self._fg
        self.update()
        self.fg_changed.emit(self._fg)
        self.bg_changed.emit(self._bg)

    # ── Paint ─────────────────────────────────────────────────────────────────

    def paintEvent(self, _):
        p  = QPainter(self)
        w, h = self.width(), self.height()
        pad, gap = 4, 8    # outer border, FG offset from BG rect

        # BG rect (outer, slightly offset toward bottom-right)
        bg_r = QRect(gap, gap, w - gap - pad, h - gap - pad)
        p.fillRect(bg_r, self._bg)
        p.setPen(QPen(QColor(160, 160, 160), 1))
        p.drawRect(bg_r)

        # FG rect (inner, offset toward top-left)
        fg_r = QRect(pad, pad, w - gap - pad, h - gap - pad)
        p.fillRect(fg_r, self._fg)
        p.setPen(QPen(QColor(220, 220, 220), 1))
        p.drawRect(fg_r)

    def _fg_rect(self) -> QRect:
        w, h = self.width(), self.height()
        pad, gap = 4, 8
        return QRect(pad, pad, w - gap - pad, h - gap - pad)

    def _bg_rect(self) -> QRect:
        w, h = self.width(), self.height()
        pad, gap = 4, 8
        return QRect(gap, gap, w - gap - pad, h - gap - pad)

    # ── Mouse ─────────────────────────────────────────────────────────────────

    def mousePressEvent(self, e: QMouseEvent):
        if e.button() == Qt.MouseButton.LeftButton:
            if self._fg_rect().contains(e.position().toPoint()):
                self._pick_fg()
            else:
                self._pick_bg()

    def mouseDoubleClickEvent(self, e: QMouseEvent):
        if e.button() == Qt.MouseButton.LeftButton:
            self.swap()

    def _pick_fg(self):
        c = QColorDialog.getColor(self._fg, self, "Foreground Colour",
                                   QColorDialog.ColorDialogOption.ShowAlphaChannel)
        if c.isValid():
            self.set_fg(c)

    def _pick_bg(self):
        c = QColorDialog.getColor(self._bg, self, "Background Colour",
                                   QColorDialog.ColorDialogOption.ShowAlphaChannel)
        if c.isValid():
            self.set_bg(c)


# ══════════════════════════════════════════════════════════════════════════════
#  BrushManager — panel listing saved brushes, load/save/delete
# ══════════════════════════════════════════════════════════════════════════════

class BrushManager(QWidget):
    """
    Floating/dockable brush library panel.
    Brushes are stored as PNG files in ~/.config/imgfactory/dp5_brushes/
    Click a brush → loads it into the copy buffer (ready to stamp).
    """
    brush_selected = pyqtSignal(bytearray, int, int)   # buf, w, h

    _BRUSH_DIR = Path.home() / '.config' / 'imgfactory' / 'dp5_brushes'

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Brush Manager")
        self.setMinimumWidth(180)
        self._brushes: List[Path] = []
        self._setup_ui()
        self._brush_dir.mkdir(parents=True, exist_ok=True)
        self._refresh()

    @property
    def _brush_dir(self) -> Path:
        return BrushManager._BRUSH_DIR

    def _setup_ui(self):
        lay = QVBoxLayout(self)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(3)

        hdr = QLabel("Brushes")
        hdr.setFont(QFont("Arial", 9, QFont.Weight.Bold))
        lay.addWidget(hdr)

        self._list = QListWidget()
        self._list.setIconSize(QSize(48, 48))
        self._list.setViewMode(QListWidget.ViewMode.IconMode)
        self._list.setResizeMode(QListWidget.ResizeMode.Adjust)
        self._list.setGridSize(QSize(64, 72))
        self._list.setSpacing(2)
        self._list.setMovement(QListWidget.Movement.Static)
        self._list.itemDoubleClicked.connect(self._on_brush_selected)
        self._list.setToolTip("Double-click to load brush into stamp buffer")
        lay.addWidget(self._list, 1)

        btns = QHBoxLayout()
        btns.setSpacing(3)
        for text, tip, slot in [
            ("Save",   "Save current copy buffer as brush",  self._save_brush),
            ("Delete", "Delete selected brush",              self._delete_brush),
            ("Import", "Import PNG as brush",                self._import_brush),
        ]:
            btn = QPushButton(text)
            btn.setFont(QFont("Arial", 8))
            btn.setToolTip(tip)
            btn.setMinimumHeight(24)
            btn.clicked.connect(slot)
            btns.addWidget(btn)
        lay.addLayout(btns)

    def _refresh(self):
        self._list.clear()
        self._brushes = sorted(self._brush_dir.glob("*.png"))
        for path in self._brushes:
            px = QPixmap(str(path)).scaled(
                48, 48,
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation)
            item = QListWidgetItem(QIcon(px), path.stem)
            item.setData(Qt.ItemDataRole.UserRole, str(path))
            self._list.addItem(item)

    def _on_brush_selected(self, item: QListWidgetItem):
        path = item.data(Qt.ItemDataRole.UserRole)
        try:
            from PIL import Image
            img  = Image.open(path).convert('RGBA')
            w, h = img.size
            buf  = bytearray(img.tobytes())
            self.brush_selected.emit(buf, w, h)
        except Exception as e:
            QMessageBox.warning(self, "Brush Load Error", str(e))

    def save_current_buffer(self, buf: bytearray, w: int, h: int):
        """Save the current canvas copy buffer as a named brush PNG."""
        if not buf or w <= 0: return
        name, ok = QInputDialog.getText(self, "Save Brush", "Brush name:")
        if not ok or not name.strip(): return
        path = self._brush_dir / f"{name.strip()}.png"
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (w, h), bytes(buf))
            img.save(str(path))
            self._refresh()
        except Exception as e:
            QMessageBox.warning(self, "Save Error", str(e))

    def _save_brush(self):
        self.brush_selected.emit(bytearray(), 0, 0)   # asks parent to call save_current_buffer

    def _delete_brush(self):
        item = self._list.currentItem()
        if not item: return
        path = Path(item.data(Qt.ItemDataRole.UserRole))
        reply = QMessageBox.question(self, "Delete Brush",
                                     f"Delete '{path.stem}'?",
                                     QMessageBox.StandardButton.Yes |
                                     QMessageBox.StandardButton.No)
        if reply == QMessageBox.StandardButton.Yes:
            path.unlink(missing_ok=True)
            self._refresh()

    def _import_brush(self):
        path, _ = QFileDialog.getOpenFileName(
            self, "Import Brush PNG", "", "PNG Images (*.png);;All Files (*)")
        if not path: return
        try:
            import shutil
            dest = self._brush_dir / Path(path).name
            shutil.copy2(path, dest)
            self._refresh()
        except Exception as e:
            QMessageBox.warning(self, "Import Error", str(e))


# ══════════════════════════════════════════════════════════════════════════════
#  BrushThumbnail — preview of the copy buffer, click to activate stamp mode
# ══════════════════════════════════════════════════════════════════════════════

class BrushThumbnail(QWidget):
    """
    Small thumbnail showing the current copy/cut buffer.
    Click  → activate stamp mode (TOOL_STAMP) so user clicks to place.
    Right-click → clear the buffer.
    Shows a checkerboard when empty.
    """
    stamp_requested = pyqtSignal()   # emitted when user clicks to stamp
    clear_requested = pyqtSignal()   # emitted when user right-clicks to clear

    def __init__(self, parent=None):
        super().__init__(parent)
        self._buf:   Optional[bytearray] = None
        self._buf_w  = 0
        self._buf_h  = 0
        self._active = False   # True when stamp mode is on
        self.setFixedSize(64, 48)
        self.setToolTip("Copy buffer — click to stamp, right-click to clear")
        self.setCursor(Qt.CursorShape.PointingHandCursor)

    def set_buffer(self, buf: Optional[bytearray], w: int, h: int):
        self._buf   = buf
        self._buf_w = w
        self._buf_h = h
        self.update()

    def set_active(self, active: bool):
        self._active = active
        self.update()

    def paintEvent(self, _):
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing, False)
        w, h = self.width(), self.height()

        # Checkerboard background
        cs = 6
        for row in range(h // cs + 1):
            for col in range(w // cs + 1):
                colour = QColor('#555') if (row + col) % 2 == 0 else QColor('#888')
                p.fillRect(col*cs, row*cs, cs, cs, colour)

        # Buffer preview
        if self._buf and self._buf_w > 0 and self._buf_h > 0:
            img = QImage(bytes(self._buf), self._buf_w, self._buf_h,
                         self._buf_w * 4, QImage.Format.Format_RGBA8888)
            scaled = img.scaled(w, h,
                                 Qt.AspectRatioMode.KeepAspectRatio,
                                 Qt.TransformationMode.SmoothTransformation)
            ox = (w - scaled.width())  // 2
            oy = (h - scaled.height()) // 2
            p.drawImage(ox, oy, scaled)

        # Active border — bright cyan highlight
        if self._active:
            p.setPen(QPen(QColor('#00e5ff'), 2))
            p.drawRect(1, 1, w-2, h-2)
        else:
            p.setPen(QPen(QColor('#444'), 1))
            p.drawRect(0, 0, w-1, h-1)

        p.end()

    def mousePressEvent(self, e: QMouseEvent):
        if e.button() == Qt.MouseButton.LeftButton:
            if self._buf:
                self.stamp_requested.emit()
        elif e.button() == Qt.MouseButton.RightButton:
            self.clear_requested.emit()


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

        amiga_ocs = [
            "#000000","#111111","#222222","#333333","#444444","#555555","#666666","#777777",
            "#888888","#999999","#AAAAAA","#BBBBBB","#CCCCCC","#DDDDDD","#EEEEEE","#FFFFFF",
            "#0000AA","#AA0000","#00AA00","#AAAA00","#00AAAA","#AA00AA","#AAAAAA","#FF0000",
            "#00FF00","#0000FF","#FFFF00","#00FFFF","#FF00FF","#FF8800","#FF0088","#8888FF",
        ]

        amiga_aga = [
            "#A69F9E","#CECECE","#B9B9B9","#949694","#838583","#777371","#5C4B4A","#685555",
            "#A8A2A1","#898988","#919291","#8D8E8D","#868886","#818280","#7C7B79","#7E7C7B",
            "#8A8A89","#B1B2B1","#999594","#EFEEEF","#B5A9AA","#FFFEFF","#F1EDEE","#CABFC0",
            "#B2A3A3","#D4CBCC","#FAF8F9","#ECEBEC","#D6D6D6","#D2D3D2","#C8C3C4","#B8ACAC",
            "#877071","#DED6D7","#826A6B","#482525","#998585","#F2EFF0","#D1D2D1","#9D9F9D",
            "#959695","#7C7271","#563939","#3F1A1A","#D8CFD0","#6E5152","#290000","#2E0707",
            "#3E1F1E","#492F2E","#5D4242","#725757","#B1A2A2","#F6F3F4","#CACACA","#767170",
            "#665A59","#635655","#6A605F","#7A7776","#8B8D8B","#A1A2A1","#AFB0AF","#9A9C9A",
            "#848684","#6F7E96","#5876AC","#5575AF","#6079A4","#76818F","#828482","#818281",
            "#808180","#828382","#8A8B8A","#919391","#8E8F8E","#878987","#868682","#8D8881",
            "#918980","#8A8782","#988B86","#AD9189","#AF9289","#A38E87","#8D8884","#898B89",
            "#898A89","#957D6C","#A87455","#A97455","#9E7962","#8B8179","#859079","#8AA565",
            "#8CB05B","#8BAD5D","#879871","#838582","#968E6B","#A99755","#9D9263","#8B897A",
            "#7E808F","#7678A4","#7274AF","#7375AC","#7C7E96","#838484","#88729A","#8C61AF",
            "#896DA1","#857F8B","#769184","#5FA785","#55B086","#59AC86","#709784","#3A1818",
            "#351212","#5B4A49","#75706F","#A49F9E","#D8D1D1","#EDE8E9","#FCFBFC","#371514",
            "#472D2C","#6D6463","#969796","#BBBBBB","#5274B1","#1C62E4","#145FEC","#2E68D3",
            "#657BA0","#7E7E7E","#7C7B7C","#808080","#939493","#A4A5A4","#A6A7A6","#9D9E9D",
            "#8C8E8C","#9C8C7E","#A68F7C","#A58F7C","#948A80","#B4938A","#E8A291","#ECA391",
            "#D09B8D","#9C8C86","#8D8F8D","#AF714D","#DC5C16","#DD5C14","#C36834","#977C6B",
            "#889F6A","#93D13B","#98EB22","#97E627","#8DB457","#848681","#B19C4A","#DDB114",
            "#C2A336","#958E6D","#787AA0","#6365D3","#5A5CEC","#5C5EE5","#7173B1","#848287",
            "#8E58BA","#9830EC","#914BCA","#877696","#63A285","#2CD588","#14EB8A","#1EE289",
            "#A5A7A5","#C1C3C1","#ACADAC","#371515","#645756","#BBBCBB","#F8F7F8","#5D4D4C",
            "#310C0C","#3C1D1C","#695E5D","#999B99","#D7D7D7","#C5C5C5","#4A71B9","#095BF6",
            "#0058FF","#1E62E2","#5F79A5","#818381","#7D7D7D","#7B797B","#7F7F7F","#AAABAA",
            "#A2A3A2","#8C8881","#A08E7D","#AC917B","#978B7F","#BE968B","#FBA893","#FFA994",
            "#DE9F8F","#A18E87","#8F918F","#949594","#B86D43","#EC5502","#EE5400","#CF6226",
            "#9A7A66","#89A466","#96DF2D","#9CFE10","#9BF816","#8EBC4E","#858681","#BAA040",
            "#EEB900","#CDA928","#988F69","#7678A5","#5D5FE2","#5254FF","#5557F7","#6E70B9",
            "#848187","#9050C4","#9C20FF","#9441D7","#877399","#5DA885","#1CE489","#00FE8B",
            "#0BF48A","#4CB886","#ABADAB","#CDCECD","#B4B5B4","#310909","#624747","#857B7A",
        ]

        amiga_aga_wb = [
            "#828781","#2D0001","#FFFFFF","#0157FF","#7C797A","#ACACAC","#AB917A","#FDAA92",
            "#959597","#EE5500","#9EFE10","#EABB00","#4F53FD","#9B20FF","#04FD87","#CECECE",
            "#000000","#320001","#000000","#CBD800","#414440","#4F5551","#646560","#747570",
            "#898989","#9E9B9E","#A8ABAA","#BAB9C0","#D0CDCE","#E0DFE2","#F2EDEF","#FFFFFF",
            "#747174","#1F4C44","#7B6AFF","#FEFEAC","#7B6D42","#B4945B","#FFFFFF","#DEFEFF",
            "#C5FAFF","#A5F5FF","#8CF6FF","#69F2FF","#50EFFF","#32EEFF","#19E9FE","#00E6FF",
            "#393939","#313131","#292929","#1F201D","#1A1818","#090C08","#0000FD","#00029C",
            "#020055","#010902","#02DEFC","#02B8FC","#FE45FE","#FB39D3","#FF2DAB","#FF2082",
            "#FF1452","#FF072E","#FF0002","#FF0C01","#FC1800","#FF2300","#FF2F02","#FF4004",
            "#FF4C00","#FF5802","#FD6500","#FF7300","#FF7F01","#FF8C04","#FE9905","#FDA604",
            "#FFB400","#FFBB03","#FCCB00","#FDD904","#FFE801","#FEF102","#FFFD00","#F6FD03",
            "#E8FC04","#D5FF00","#C4FD04","#BBFF00","#ACFE01","#98FC00","#8AFF00","#82FE00",
            "#73FF00","#62FD01","#53FE00","#41FF00","#39FD00","#2AFE00","#18FE00","#06FE02",
            "#01FE00","#01F600","#00EC00","#00E700","#00DF00","#00D600","#03CF01","#02C600",
            "#05BC01","#00B801","#01AC00","#01A500","#029E00","#019600","#018D00","#048500",
            "#027F05","#007600","#036D01","#006405","#005C04","#005201","#004901","#004600",
            "#033B00","#003400","#012D01","#052300","#021C03","#001402","#280101","#3F0102",
            "#500100","#6A0100","#870002","#940004","#AC0104","#C50004","#D30200","#EE0102",
            "#FD0100","#EB0015","#E20232","#CC0046","#BE025F","#AB0076","#9F0092","#8C00A7",
            "#7602C9","#6102E1","#65CF64","#264522","#AFFFB0","#D2C20D","#BBAB13","#AD9414",
            "#948115","#846A1A","#71541A","#634222","#522D21","#003CF4","#024AF6","#004EF3",
            "#015DF4","#0367F7","#0771F6","#0378F4","#0781F7","#068DF8","#0695F7","#059FF8",
            "#292222","#F9B0B2","#EAFEAE","#CCFE9E","#9DF975","#83FB58","#71F83B","#53F41A",
            "#A9A6AA","#F0F0F0","#200021","#2B052B","#470734","#540F3D","#6F1042","#800F4C",
            "#8D1A4E","#9C1C57","#B1205F","#BE2468","#D82A73","#E52F83","#5BA5A9","#7EEAE8",
            "#F8D093","#8B6748","#FCFED9","#C5996A","#3B3004","#FFFFBA","#FFFFF4","#8EC993",
            "#50674D","#CDF0CE","#6A936A","#2E362F","#B2EEAF","#E5F0E1","#C7C898","#67694F",
            "#F5F2CC","#969369","#3B3732","#F3F2B3","#CCD0D3","#52BF8C","#334457","#30498C",
            "#726764","#939094","#A08E7F","#A49891","#D25B8A","#7F3F4E","#D475B7","#AE4969",
            "#4C3037","#D467D2","#D482CB","#8A5A88","#4C3C4C","#B470B0","#654967","#37203A",
            "#976996","#BD81BC","#AF383C","#A63A3F","#9E3836","#913637","#893534","#7E3B32",
            "#753333","#6F3733","#5C95A4","#8E8F8A","#827F80","#9F9C9D","#968E7C","#C59A8B",
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

        ula_plus = [
            "#000000","#000154","#0000AA","#0000FE","#270100","#270055","#2700A9","#2800FF",
            "#4A0000","#4B0055","#4C00AA","#4B00FF","#6E0000","#700056","#7000AA","#6F00FF",
            "#920000","#940056","#9300A9","#9300FF","#B70100","#B70055","#B700AA","#B800FF",
            "#DA0000","#DB0056","#DC00AA","#DC00FF","#FE0000","#FF0054","#FF00AA","#FF00FE",
            "#012700","#012756","#0027AA","#0027FF","#272800","#262755","#2727A9","#2728FF",
            "#4A2700","#4B2755","#4B28AA","#4A27FF","#6F2700","#6F2755","#6E27A9","#6E27FF",
            "#932700","#932856","#9227A9","#9227FF","#B72800","#B62755","#B727AA","#B728FF",
            "#DA2700","#DB2756","#DB28AA","#DA27FF","#FF2700","#FF2756","#FF28AA","#FE27FF",
            "#014B00","#014B56","#004AA9","#004BFF","#274B01","#264B54","#274BAB","#274CFF",
            "#4A4B00","#4B4B55","#4B4CA9","#4B4CFF","#6F4B00","#6F4B55","#6F4CAA","#6E4BFF",
            "#934B00","#934B56","#924BA9","#924BFF","#B74B00","#B64B55","#B64BA9","#B74BFF",
            "#DB4C00","#DB4B55","#DB4BAA","#DB4CFF","#FF4B00","#FF4B56","#FF4CAA","#FE4BFF",
            "#016F00","#016F56","#016FAA","#006FFF","#276F01","#277055","#276FAA","#276FFF",
            "#4B6F01","#4B6F55","#4B6FAB","#4B70FF","#6F6F00","#6F6F55","#6F70A9","#6E6FFF",
            "#936F00","#936F55","#926FA9","#926FFF","#B76F00","#B76F56","#B66FA9","#B76FFF",
            "#DB6F00","#DB6F55","#DB6FA9","#DB6FFF","#FF6F00","#FF6F55","#FF6FAA","#FE6FFF",
            "#009300","#019355","#0193AA","#0092FF","#279301","#279355","#2693AA","#2793FF",
            "#4B9301","#4B9354","#4B93AB","#4B93FF","#6F9200","#6F9355","#6F93AB","#6F94FF",
            "#939300","#939355","#9394A9","#9293FF","#B79300","#B79355","#B693A9","#B793FF",
            "#DB9300","#DA9355","#DB93A9","#DB93FF","#FF9400","#FF9355","#FF93AA","#FF93FF",
            "#00B700","#00B755","#01B7AA","#00B6FF","#27B700","#27B755","#26B7AA","#27B7FE",
            "#4BB701","#4CB855","#4BB7AA","#4BB7FF","#70B701","#6FB754","#6FB7AB","#6FB7FF",
            "#93B600","#93B755","#93B7AB","#92B7FE","#B7B700","#B7B755","#B7B8AA","#B7B7FF",
            "#DBB700","#DBB756","#DBB7A9","#DBB7FF","#FFB700","#FFB755","#FFB7A9","#FFB7FF",
            "#00DC00","#00DB55","#00DCA9","#01DBFF","#27DB00","#27DB54","#27DBAB","#27DBFE",
            "#4BDB00","#4BDB55","#4BDBAA","#4BDBFE","#70DB01","#6FDB54","#6FDBAA","#6FDBFF",
            "#94DB01","#93DB55","#93DBAB","#93DCFF","#B7DA00","#B7DB55","#B7DBAB","#B7DBFF",
            "#DBDB00","#DBDB55","#DBDBA9","#DBDBFF","#FFDB00","#FFDA55","#FFDBA9","#FFDBFF",
            "#00FF01","#00FF55","#00FFAB","#00FFFF","#27FE00","#27FF54","#27FFAA","#27FFFE",
            "#4BFF00","#4BFF55","#4BFEAA","#4BFFFE","#6FFF00","#70FF55","#6FFFAA","#6FFFFF",
            "#94FF01","#93FF54","#93FFAA","#93FFFF","#B7FE00","#B7FF55","#B7FFAB","#B7FFFE",
            "#DBFE01","#DBFF55","#DCFFAB","#DBFFFF","#FFFF00","#FFFF55","#FFFFA9","#FFFFFF",
        ]

        # Registry: name -> (hex_list, cols)
        # cols = how many swatches wide the grid should be for this palette
        self.retro_palettes = {
            "Amiga OCS":       (amiga_ocs,    8),   # 32 colours — 4 rows × 8
            "Amiga AGA":       (amiga_aga,   16),   # 256 colours — 16×16
            "Amiga AGA WB":    (amiga_aga_wb,16),   # 256 colours — 16×16
            "C64":             (commodore_64, 8),   # 16 colours — 2 rows × 8
            "ZX Spectrum":     (zx_spectrum,  8),   # 16 colours — 2 rows × 8
            "Amstrad CPC":     (amstrad_cpc,  9),   # 27 colours — 3 rows × 9
            "Atari 800":       (atari_800,    8),   # 40 colours — 5 rows × 8
            "Atari 2600 NTSC": (atari_2600,   8),   # 128 colours — 16 rows × 8
            "ULA Plus":        (ula_plus,    16),   # 256 colours — 16×16
        }
        self.current_retro_palette = "Amiga OCS"

    def _get_retro_colors(self, name: str) -> Tuple[List[QColor], int]:
        """Return (colors, cols) for named retro palette."""
        data, cols = self.retro_palettes.get(
            name, self.retro_palettes["Amiga OCS"])
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

        # DP5-specific settings (JSON, separate from global theme)
        self.dp5_settings = DP5Settings()

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

        # Canvas state — initial values from dp5_settings
        self._canvas_width  = self.dp5_settings.get('default_width')
        self._canvas_height = self.dp5_settings.get('default_height')
        self._canvas_zoom   = self.dp5_settings.get('default_zoom')
        self._undo_stack    = deque(maxlen=self.dp5_settings.get('undo_levels'))
        self._redo_stack    = deque(maxlen=self.dp5_settings.get('undo_levels'))
        self.dp5_canvas     = None   # set by _create_centre_panel

        # Bitmap list (left panel)
        self._bitmap_list: List[dict] = []   # [{name, rgba, w, h}]
        self._current_bitmap = -1

        # AppSettings (global theme only)
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

        # Fill-mode state for shape tools — False=outline, True=filled
        # Right-clicking the button toggles this
        self._shape_fill_state: dict = {
            TOOL_RECT:     False,
            TOOL_CIRCLE:   False,
            TOOL_TRIANGLE: False,
            TOOL_POLYGON:  False,
            TOOL_STAR:     False,
            TOOL_LASSO:    False,
        }

        self.setWindowTitle(App_name)
        self.resize(1400, 800)
        self.setMinimumSize(900, 560)

        # Window icon — shows in taskbar, alt-tab, and title bar
        try:
            from apps.methods.imgfactory_svg_icons import get_dp5_workshop_icon
            self.setWindowIcon(get_dp5_workshop_icon(64))
        except Exception:
            pass

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

        self._splitter = QSplitter(Qt.Orientation.Horizontal)

        self._left_panel  = self._create_left_panel()
        centre            = self._create_centre_panel()
        right             = self._create_right_panel()

        self._splitter.addWidget(self._left_panel)
        self._splitter.addWidget(centre)
        self._splitter.addWidget(right)
        self._splitter.setStretchFactor(0, 0)   # bitmap list
        self._splitter.setStretchFactor(1, 1)   # canvas — stretches
        self._splitter.setStretchFactor(2, 0)   # tools / palette

        # Left panel: hidden by default — toggle via DP5 Settings
        self._left_panel.setVisible(self.dp5_settings.get('show_bitmap_list'))

        main_layout.addWidget(self._splitter)

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
        layout.setSpacing(4)

        icon_color = self._get_icon_color()

        # ── _tb helper — adds button to layout with optional SVG icon ─────
        def _tb(text, tip, slot, icon_fn=None):
            btn = QPushButton(text)
            btn.setFont(self.button_font)
            btn.setToolTip(tip)
            btn.setMinimumHeight(28)
            btn.setMaximumHeight(28)
            if icon_fn:
                try:
                    btn.setIcon(icon_fn(18, icon_color))
                    btn.setIconSize(QSize(18, 18))
                except Exception:
                    pass
            if slot: btn.clicked.connect(slot)
            layout.addWidget(btn)
            return btn

        # ── Cog / Settings (standalone only) ──────────────────────────────
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

        # ── Title with DP5 icon ────────────────────────────────────────────
        title_row = QHBoxLayout()
        title_row.setSpacing(6)
        dp5_icon_lbl = QLabel()
        try:
            from apps.methods.imgfactory_svg_icons import get_dp5_workshop_icon
            pix = get_dp5_workshop_icon(22, icon_color).pixmap(22, 22)
            dp5_icon_lbl.setPixmap(pix)
        except Exception:
            if ICONS_AVAILABLE:
                pix = SVGIconFactory.paint_icon(20, icon_color).pixmap(20, 20)
                dp5_icon_lbl.setPixmap(pix)
        title_row.addWidget(dp5_icon_lbl)
        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_row.addWidget(self.title_label)
        layout.addLayout(title_row)

        layout.addStretch()

        # ── [Load][Save][Import][Export][Undo] after title ─────────────────
        # Reuse existing SVGIconFactory icons — no duplication needed
        self.tb_load_btn   = _tb("Load",   "Load / open image file",
                                  self._import_bitmap,
                                  SVGIconFactory.open_icon)
        self.tb_save_btn   = _tb("Save",   "Save canvas as PNG",
                                  self._export_bitmap,
                                  SVGIconFactory.save_icon)
        self.tb_import_btn = _tb("Import", "Import image (IFF, BMP, older formats)",
                                  self._import_bitmap,
                                  SVGIconFactory.import_icon)
        self.tb_export_btn = _tb("Export", "Export canvas (IFF, BMP, older formats)",
                                  self._export_bitmap,
                                  SVGIconFactory.export_icon)
        self.tb_undo_btn   = _tb("Undo",   "Undo last action  (Ctrl+Z)",
                                  self._undo_canvas,
                                  SVGIconFactory.undo_icon)

        # ── Brush Manager button ───────────────────────────────────────────
        self.brush_mgr_btn = QPushButton("Brushes")
        self.brush_mgr_btn.setFont(self.button_font)
        self.brush_mgr_btn.setToolTip("Open brush manager panel")
        self.brush_mgr_btn.setMinimumHeight(28)
        self.brush_mgr_btn.setMaximumHeight(28)
        self.brush_mgr_btn.clicked.connect(self._toggle_brush_manager)
        try:
            from apps.methods.imgfactory_svg_icons import get_brushes_icon
            self.brush_mgr_btn.setIcon(get_brushes_icon(18, icon_color))
            self.brush_mgr_btn.setIconSize(QSize(18, 18))
        except Exception:
            pass
        layout.addWidget(self.brush_mgr_btn)

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
            scroll.setWidgetResizable(False)   # canvas sizeHint drives size
            scroll.setAlignment(Qt.AlignmentFlag.AlignCenter)
            self._canvas_scroll = scroll
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
        em.addAction("Undo\tCtrl+Z",       self._undo_canvas)
        em.addAction("Redo\tCtrl+Y",       self._redo_canvas)
        em.addSeparator()
        em.addAction("Cut\tCtrl+X",        self._cut_selection)
        em.addAction("Copy\tCtrl+C",       self._copy_selection)
        em.addAction("Paste\tCtrl+V",      self._paste_selection)
        em.addSeparator()
        em.addAction("Select All\tCtrl+A", self._select_all)
        em.addAction("Deselect\tEsc",      self._deselect)
        em.addSeparator()
        em.addAction("Clear canvas",       self._clear_canvas)
        em.addAction("Fill with colour",   self._fill_canvas)
        # Picture
        pm = mb.addMenu("Picture")
        # Flip / mirror
        pm.addAction("Flip horizontal",     self._mirror_h)
        pm.addAction("Flip vertical",       self._mirror_v)
        pm.addSeparator()
        # Rotate
        rm = pm.addMenu("Rotate")
        rm.addAction("90° clockwise",       self._rotate_90_cw)
        rm.addAction("90° counter-clockwise", self._rotate_90_ccw)
        rm.addAction("180°",                self._rotate_180)
        rm.addAction("Arbitrary angle…",    self._rotate_arbitrary)
        pm.addSeparator()
        # Scale
        pm.addAction("Scale canvas…",       self._scale_canvas)
        pm.addSeparator()
        pm.addAction("Set polygon sides…",  self._set_polygon_sides)
        pm.addSeparator()
        # Colour ops
        pm.addAction("Invert colours",      self._invert)
        pm.addAction("Brighten +25",        lambda: self._adjust(25))
        pm.addAction("Darken -25",          lambda: self._adjust(-25))
        # View
        vm = mb.addMenu("View")
        vm.addAction("Zoom in  Ctrl++",  lambda: self._set_zoom(
            self._canvas_zoom * 1.25 if self._canvas_zoom < 1
            else min(16, self._canvas_zoom + 1)))
        vm.addAction("Zoom out  Ctrl+-", lambda: self._set_zoom(
            max(0.05, self._canvas_zoom * 0.8 if self._canvas_zoom <= 1
            else self._canvas_zoom - 1)))
        vm.addSeparator()
        for z in (0.1, 0.25, 0.5, 1, 2, 4, 8, 16):
            lbl = f"{int(z)}×" if z >= 1 else f"{z}×"
            vm.addAction(lbl, lambda _, zz=z: self._set_zoom(zz))
        ga = vm.addAction("Pixel grid")
        ga.setCheckable(True); ga.setChecked(True)
        ga.triggered.connect(
            lambda v: (setattr(self.dp5_canvas, 'show_grid', v),
                       self.dp5_canvas.update()) if self.dp5_canvas else None)

    # ── Right panel: gadget bar + palettes ────────────────────────────────────

    def _create_right_panel(self):
        """Right panel: adaptive-column gadget bar, FGBGSwatch, palettes."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)

        icon_color = self._get_icon_color()

        # ── Column count: auto or explicit ────────────────────────────────
        icon_sz   = self.dp5_settings.get('tool_icon_size')   # 24–64 px
        btn_sz    = icon_sz + 6                               # icon + padding
        gap       = 3                                          # grid spacing

        # Auto mode: pick columns so the panel stays ≤ ~280px
        # btn_sz=48 → 3col=150+framing=162, 4col=198+framing=210
        # btn_sz=36 → 3col=114, 4col=150  btn_sz=60 → 3col=186, 4col=246
        req_cols = self.dp5_settings.get('tool_columns')      # 0=auto
        if req_cols == 0:
            # auto: prefer 4 if icon_sz ≤ 36, 3 if ≤ 50, else 2
            if icon_sz <= 36:
                n_cols = 4
            elif icon_sz <= 50:
                n_cols = 3
            else:
                n_cols = 2
        else:
            n_cols = max(2, min(4, req_cols))

        # Panel width: n_cols buttons + gaps + outer margins
        panel_w = btn_sz * n_cols + gap * (n_cols - 1) + 16
        panel.setMinimumWidth(panel_w)
        panel.setMaximumWidth(panel_w + 20)   # small slack for scroll bars

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(4, 4, 4, 4)
        layout.setSpacing(3)

        # ── Flat ordered tool list (no row/col — computed below) ───────────
        # Order: pencil, eraser, fill, spray, picker, curve,
        #        line, rect, circle, polygon, triangle, star,
        #        select, lasso, move, zoom, text
        # Shapes with fill toggle (right-click): rect, circle, triangle, polygon, star
        TOOL_ORDER = [
            (TOOL_PENCIL,   'pencil',   'Pencil — freehand (P)'),
            (TOOL_ERASER,   'eraser',   'Eraser (E)'),
            (TOOL_FILL,     'fill',     'Flood fill (F)'),
            (TOOL_SPRAY,    'spray',    'Airbrush / spray (S)'),
            (TOOL_PICKER,   'picker',   'Colour picker (K)'),
            (TOOL_CURVE,    'curve',    'Bézier curve — click pts, dbl to commit (Q)'),
            (TOOL_LINE,     'line',     'Straight line (L)'),
            (TOOL_RECT,     'rect',     'Rectangle  (R) — right-click to toggle fill'),
            (TOOL_CIRCLE,   'circle',   'Ellipse  (C) — right-click to toggle fill'),
            (TOOL_TRIANGLE, 'triangle', 'Triangle  (T) — right-click to toggle fill'),
            (TOOL_POLYGON,  'polygon',  'Polygon  (O) — click verts, dbl to close, right-click fills'),
            (TOOL_STAR,     'star',     'Star  (*) — right-click to toggle fill'),
            (TOOL_SELECT,   'select',   'Select (M) — drag to select, drag inside to move'),
            (TOOL_LASSO,    'lasso',    'Lasso  (G) — right-click to fill shape'),
            (TOOL_ZOOM,     'zoom',     'Zoom — click in, right-click out (Z)'),
            (TOOL_TEXT,     'text',     'Place text on canvas (I)'),
        ]

        # ── Build gadget grid ──────────────────────────────────────────────
        gadget_grid = QGridLayout()
        gadget_grid.setSpacing(gap)
        gadget_grid.setContentsMargins(0, 0, 0, 0)
        for c in range(n_cols):
            gadget_grid.setColumnMinimumWidth(c, btn_sz)

        self._tool_btns    = {}
        self._tool_icon_sz = icon_sz
        self._n_cols       = n_cols
        _ws = self   # workshop ref for closure

        # Tiny subclass to capture right-click on shape toggle buttons
        class ShapeToolButton(QPushButton):
            def __init__(self, tool_id, shape_key, **kw):
                super().__init__(**kw)
                self._tool_id  = tool_id
                self._shape_key = shape_key
            def mousePressEvent(self, ev):
                if ev.button() == Qt.MouseButton.RightButton:
                    _ws._toggle_shape_fill(self._tool_id)
                else:
                    super().mousePressEvent(ev)

        for idx, (tool_id, shape, tip) in enumerate(TOOL_ORDER):
            row = idx // n_cols
            col = idx %  n_cols

            if tool_id in SHAPE_FILL_PAIRS:
                btn = ShapeToolButton(tool_id, shape)
            else:
                btn = QPushButton()

            btn.setFixedSize(btn_sz, btn_sz)
            btn.setCheckable(True)
            btn.setToolTip(tip)
            ico = _make_tool_icon(shape, icon_sz, active=False)
            btn.setIcon(ico)
            btn.setIconSize(QSize(icon_sz, icon_sz))
            btn.clicked.connect(lambda _, t=tool_id: self._select_tool(t))
            self._tool_btns[tool_id] = btn
            gadget_grid.addWidget(btn, row, col)

        layout.addLayout(gadget_grid)

        # ── UNDO / CLR — always 2 buttons side by side ────────────────────
        undo_clr = QHBoxLayout()
        undo_clr.setSpacing(gap)

        undo_btn = QPushButton("UN\nDO")
        undo_btn.setFixedSize(btn_sz, btn_sz)
        undo_btn.setFont(QFont("Arial", max(7, icon_sz // 6)))
        undo_btn.setToolTip("Undo  Ctrl+Z")
        undo_btn.clicked.connect(self._undo_canvas)
        try:
            undo_btn.setIcon(SVGIconFactory.undo_icon(max(14, icon_sz - 14), icon_color))
            undo_btn.setIconSize(QSize(max(14, icon_sz - 14), max(14, icon_sz - 14)))
        except Exception:
            pass

        clr_btn = QPushButton("CLR")
        clr_btn.setFixedSize(btn_sz, btn_sz)
        clr_btn.setFont(QFont("Arial", max(8, icon_sz // 5)))
        clr_btn.setToolTip("Clear canvas")
        clr_btn.clicked.connect(self._clear_canvas)
        try:
            from apps.methods.imgfactory_svg_icons import get_clear_canvas_icon
            clr_btn.setIcon(get_clear_canvas_icon(max(14, icon_sz - 14), icon_color))
            clr_btn.setIconSize(QSize(max(14, icon_sz - 14), max(14, icon_sz - 14)))
        except Exception:
            pass

        undo_clr.addWidget(undo_btn)
        undo_clr.addWidget(clr_btn)
        undo_clr.addStretch()
        layout.addLayout(undo_clr)

        layout.addSpacing(4)

        # ── Brush size slider + value label ───────────────────────────────
        size_hdr = QHBoxLayout()
        size_lbl = QLabel("Size")
        size_lbl.setFont(QFont("Arial", 9))
        size_hdr.addWidget(size_lbl)
        self._size_val_lbl = QLabel("1")
        self._size_val_lbl.setAlignment(Qt.AlignmentFlag.AlignRight |
                                        Qt.AlignmentFlag.AlignVCenter)
        self._size_val_lbl.setFont(QFont("Arial", 9, QFont.Weight.Bold))
        self._size_val_lbl.setFixedWidth(28)
        size_hdr.addWidget(self._size_val_lbl)
        layout.addLayout(size_hdr)

        self._size_sl = QSlider(Qt.Orientation.Horizontal)
        self._size_sl.setRange(1, 20)
        self._size_sl.setValue(1)
        self._size_sl.setMinimumHeight(22)
        self._size_sl.valueChanged.connect(self._set_brush_size)
        layout.addWidget(self._size_sl)

        # ── Snap to grid toggle ───────────────────────────────────────────
        snap_row = QHBoxLayout()
        self._snap_chk = QCheckBox("Snap to grid")
        self._snap_chk.setFont(QFont("Arial", 8))
        self._snap_chk.setChecked(False)
        self._snap_chk.setToolTip("Snap drawing to pixel grid")
        self._snap_chk.toggled.connect(self._set_snap_grid)
        self._grid_chk2 = QCheckBox("Show grid")
        self._grid_chk2.setFont(QFont("Arial", 8))
        self._grid_chk2.setChecked(self.dp5_settings.get('show_pixel_grid'))
        self._grid_chk2.toggled.connect(self._set_show_grid)
        snap_row.addWidget(self._snap_chk)
        layout.addLayout(snap_row)
        layout.addWidget(self._grid_chk2)

        # ── Zoom label ────────────────────────────────────────────────────
        self._zoom_lbl = QLabel(f"{self._canvas_zoom}×")
        self._zoom_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._zoom_lbl.setFont(QFont("Arial", 8))
        layout.addWidget(self._zoom_lbl)

        layout.addSpacing(4)

        # ── FG / BG swatch  +  brush thumbnail ───────────────────────────
        fgbg_row_lbl = QHBoxLayout()
        fgbg_lbl = QLabel("FG / BG")
        fgbg_lbl.setFont(QFont("Arial", 8))
        fgbg_row_lbl.addWidget(fgbg_lbl)
        fgbg_row_lbl.addStretch()
        brush_lbl = QLabel("Brush")
        brush_lbl.setFont(QFont("Arial", 8))
        fgbg_row_lbl.addWidget(brush_lbl)
        layout.addLayout(fgbg_row_lbl)

        fgbg_row = QHBoxLayout()
        fgbg_row.setSpacing(4)

        self._fgbg_swatch = FGBGSwatch()
        self._fgbg_swatch.fg_changed.connect(self._on_fg_changed)
        self._fgbg_swatch.bg_changed.connect(self._on_bg_changed)
        fgbg_row.addWidget(self._fgbg_swatch)

        fgbg_row.addStretch()

        self._brush_thumb = BrushThumbnail()
        self._brush_thumb.stamp_requested.connect(self._activate_stamp_mode)
        self._brush_thumb.clear_requested.connect(self._clear_brush)
        fgbg_row.addWidget(self._brush_thumb)

        layout.addLayout(fgbg_row)

        layout.addSpacing(4)

        # ── IMAGE palette ─────────────────────────────────────────────────
        img_pal_lbl = QLabel("Image Palette")
        img_pal_lbl.setFont(QFont("Arial", 8, QFont.Weight.Bold))
        layout.addWidget(img_pal_lbl)

        img_pal_scroll = QScrollArea()
        img_pal_scroll.setWidgetResizable(True)
        img_pal_scroll.setHorizontalScrollBarPolicy(
            Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        img_pal_scroll.setMinimumHeight(80)
        img_pal_scroll.setMaximumHeight(200)
        self.pal_bar = PaletteGrid(cols=16, cell=12)
        self.pal_bar.color_picked.connect(self._on_image_palette_color)
        img_pal_scroll.setWidget(self.pal_bar)
        layout.addWidget(img_pal_scroll)

        layout.addSpacing(4)

        # ── USER palette (retro presets) ──────────────────────────────────
        user_pal_hdr = QHBoxLayout()
        user_pal_lbl = QLabel("User Palette")
        user_pal_lbl.setFont(QFont("Arial", 8, QFont.Weight.Bold))
        user_pal_hdr.addWidget(user_pal_lbl)

        self._retro_btn = QPushButton("Amiga OCS ▼")
        self._retro_btn.setFont(QFont("Arial", 7))
        self._retro_btn.setFixedHeight(18)
        self._retro_btn.setToolTip("Choose retro palette preset")
        self._retro_btn.clicked.connect(self._show_retro_menu)
        user_pal_hdr.addWidget(self._retro_btn)
        layout.addLayout(user_pal_hdr)

        user_pal_scroll = QScrollArea()
        user_pal_scroll.setWidgetResizable(True)
        user_pal_scroll.setHorizontalScrollBarPolicy(
            Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        user_pal_scroll.setMinimumHeight(60)
        user_pal_scroll.setMaximumHeight(160)
        self._user_pal_grid = PaletteGrid(cols=8, cell=12)
        self._user_pal_grid.color_picked.connect(self._on_user_palette_color)
        user_pal_scroll.setWidget(self._user_pal_grid)
        layout.addWidget(user_pal_scroll)

        # Load default retro palette
        self._apply_retro_palette(self.dp5_settings.get('retro_palette'))

        return panel

    # ── Tool / colour helpers ─────────────────────────────────────────────────

    def _toggle_shape_fill(self, primary_tool_id: str):
        """Right-click handler — flip fill mode and update button icon + canvas tool."""
        if primary_tool_id not in self._shape_fill_state:
            return
        # Toggle
        self._shape_fill_state[primary_tool_id] = not self._shape_fill_state[primary_tool_id]
        filled = self._shape_fill_state[primary_tool_id]

        # Shape key for icon: e.g. 'rect' → 'filled_rect'
        outline_shape = primary_tool_id          # e.g. 'rect'
        filled_shape  = f'filled_{primary_tool_id}'  # e.g. 'filled_rect'

        icon_sz  = self.dp5_settings.get('tool_icon_size')
        btn      = self._tool_btns.get(primary_tool_id)

        # Determine whether this tool is currently active
        is_active = (self.dp5_canvas and
                     self.dp5_canvas.tool in (primary_tool_id,
                                               SHAPE_FILL_PAIRS[primary_tool_id]))

        # Update icon to show new mode
        shape_key = filled_shape if filled else outline_shape
        if btn:
            btn.setIcon(_make_tool_icon(shape_key, icon_sz, active=is_active))
            btn.setIconSize(QSize(icon_sz, icon_sz))
            mode_str = 'filled' if filled else 'outline'
            btn.setToolTip(f'{primary_tool_id.capitalize()}  [{mode_str}]  — right-click to toggle')

        # If this tool is currently active, switch canvas tool immediately
        if is_active:
            actual = SHAPE_FILL_PAIRS[primary_tool_id] if filled else primary_tool_id
            if self.dp5_canvas:
                self.dp5_canvas.tool = actual

    def _select_tool(self, tool_id: str):
        """Select a tool, resolving fill state for shape tools."""
        # For shape tools, resolve to outline or filled variant based on fill state
        actual_tool = tool_id
        if tool_id in self._shape_fill_state:
            if self._shape_fill_state[tool_id]:
                actual_tool = SHAPE_FILL_PAIRS[tool_id]

        if self.dp5_canvas:
            self.dp5_canvas.tool = actual_tool
            self.dp5_canvas._curve_pts = []
            self.dp5_canvas._poly_pts  = []
            # When switching to TOOL_MOVE with a buffer, auto-float it
            if tool_id == TOOL_MOVE:
                c = self.dp5_canvas
                if c._sel_buffer and c._sel_buf_w > 0 and not c._sel_floating:
                    if not c._sel_float_pos:
                        if c._sel_active and c._selection_rect:
                            c._sel_float_pos = (c._selection_rect.x(),
                                                c._selection_rect.y())
                        else:
                            c._sel_float_pos = (
                                max(0, c.tex_w//2 - c._sel_buf_w//2),
                                max(0, c.tex_h//2 - c._sel_buf_h//2))
                    c._sel_floating = True
                    c.update()

        icon_sz = self.dp5_settings.get('tool_icon_size')

        # Shape key map — includes all variants
        _shape_map = {
            TOOL_PENCIL: 'pencil', TOOL_ERASER: 'eraser', TOOL_FILL: 'fill',
            TOOL_SPRAY: 'spray', TOOL_PICKER: 'picker', TOOL_CURVE: 'curve',
            TOOL_LINE: 'line',
            TOOL_RECT:     'rect',     TOOL_FILLED_RECT:     'filled_rect',
            TOOL_CIRCLE:   'circle',   TOOL_FILLED_CIRCLE:   'filled_circle',
            TOOL_TRIANGLE: 'triangle', TOOL_FILLED_TRIANGLE: 'filled_triangle',
            TOOL_POLYGON:  'polygon',  TOOL_FILLED_POLYGON:  'filled_polygon',
            TOOL_STAR:     'star',     TOOL_FILLED_STAR:     'filled_star',
            TOOL_SELECT: 'select', TOOL_LASSO: 'lasso',
            TOOL_FILLED_LASSO: 'filled_lasso',
            TOOL_MOVE: 'move', TOOL_ZOOM: 'zoom', TOOL_TEXT: 'text',
            TOOL_STAMP: 'stamp',
        }

        for tid, btn in getattr(self, '_tool_btns', {}).items():
            is_active = (tid == tool_id)
            btn.setChecked(is_active)
            # For shape buttons show current fill-mode icon when active
            if tid in self._shape_fill_state and is_active:
                filled    = self._shape_fill_state[tid]
                shape_key = f'filled_{tid}' if filled else tid
            else:
                shape_key = _shape_map.get(tid, tid)
            btn.setIcon(_make_tool_icon(shape_key, icon_sz, active=is_active))
            btn.setIconSize(QSize(icon_sz, icon_sz))

        # Sync brush thumbnail active border
        if hasattr(self, '_brush_thumb'):
            self._brush_thumb.set_active(tool_id == TOOL_STAMP)

    def _set_brush_size(self, v: int):
        if self.dp5_canvas:
            self.dp5_canvas.brush_size = v
        if hasattr(self, '_size_val_lbl'):
            self._size_val_lbl.setText(str(v))

    def _on_fg_changed(self, c: QColor):
        if self.dp5_canvas:
            self.dp5_canvas.color = c
        self.pal_bar.set_selection_by_color(c)

    def _on_bg_changed(self, c: QColor):
        # Background colour stored in swatch; eraser uses it in future
        pass

    def _on_image_palette_color(self, c: QColor):
        if self.dp5_canvas:
            self.dp5_canvas.color = c
        self._fgbg_swatch.set_fg(c)

    def _on_user_palette_color(self, c: QColor):
        if self.dp5_canvas:
            self.dp5_canvas.color = c
        self._fgbg_swatch.set_fg(c)

    def _update_color_swatches(self):
        """Sync FGBGSwatch after colour changes (e.g. picker tool)."""
        if not hasattr(self, '_fgbg_swatch'): return
        if self.dp5_canvas:
            self._fgbg_swatch.set_fg(self.dp5_canvas.color)
            self.pal_bar.set_selection_by_color(self.dp5_canvas.color)

    def _set_snap_grid(self, on: bool):
        if self.dp5_canvas:
            self.dp5_canvas.snap_grid = on

    def _set_show_grid(self, on: bool):
        if self.dp5_canvas:
            self.dp5_canvas.show_grid = on
            self.dp5_canvas.update()
        self.dp5_settings.set('show_pixel_grid', on)

    def _place_text_at(self, tx: int, ty: int):
        """Prompt for text string and paint it onto the canvas at tex coords."""
        if not self.dp5_canvas: return
        text, ok = QInputDialog.getText(self, "Place Text", "Text:")
        if not ok or not text: return

        font_size, ok2 = QInputDialog.getInt(self, "Font Size", "Size (pixels):", 12, 4, 200)
        if not ok2: return

        self._push_undo()
        # Render text onto a QImage then blit onto canvas
        tmp = QImage(self._canvas_width, self._canvas_height,
                     QImage.Format.Format_RGBA8888)
        tmp.fill(Qt.GlobalColor.transparent)
        painter = QPainter(tmp)
        painter.setFont(QFont("Arial", font_size))
        painter.setPen(self.dp5_canvas.color)
        painter.drawText(tx, ty + font_size, text)
        painter.end()
        # Composite onto canvas
        for row in range(min(self._canvas_height, tmp.height())):
            for col in range(min(self._canvas_width, tmp.width())):
                pix = tmp.pixel(col, row)
                a = (pix >> 24) & 0xFF
                if a > 0:
                    i = (row * self._canvas_width + col) * 4
                    self.dp5_canvas.rgba[i]   = (pix >> 16) & 0xFF
                    self.dp5_canvas.rgba[i+1] = (pix >>  8) & 0xFF
                    self.dp5_canvas.rgba[i+2] =  pix        & 0xFF
                    self.dp5_canvas.rgba[i+3] = a
        self.dp5_canvas.update()
        self._set_status(f"Text placed at {tx},{ty}")

    def _toggle_brush_manager(self):
        """Show/hide the brush manager as a floating panel."""
        if not hasattr(self, '_brush_mgr_panel'):
            self._brush_mgr_panel = BrushManager(self)
            self._brush_mgr_panel.setWindowFlags(
                Qt.WindowType.Tool | Qt.WindowType.WindowStaysOnTopHint)
            self._brush_mgr_panel.brush_selected.connect(self._on_brush_mgr_selected)

        if self._brush_mgr_panel.isVisible():
            self._brush_mgr_panel.hide()
        else:
            # Position near toolbar
            pos = self.mapToGlobal(self.toolbar.pos())
            self._brush_mgr_panel.move(pos.x(), pos.y() + self.toolbar.height() + 4)
            self._brush_mgr_panel.resize(220, 320)
            self._brush_mgr_panel.show()
            self._brush_mgr_panel.raise_()

    def _on_brush_mgr_selected(self, buf: bytearray, w: int, h: int):
        """Handle brush_selected signal — load brush OR save current buffer."""
        if not buf or w == 0:
            # Empty emit = save request
            if self.dp5_canvas and self.dp5_canvas._sel_buffer:
                self._brush_mgr_panel.save_current_buffer(
                    self.dp5_canvas._sel_buffer,
                    self.dp5_canvas._sel_buf_w,
                    self.dp5_canvas._sel_buf_h)
            return
        # Load brush into canvas copy buffer
        if self.dp5_canvas:
            self.dp5_canvas._sel_buffer  = buf
            self.dp5_canvas._sel_buf_w   = w
            self.dp5_canvas._sel_buf_h   = h
        self._sync_brush_thumb()
        self._activate_stamp_mode()
        self._set_status(f"Brush loaded ({w}×{h}) — click to stamp")

    def _activate_stamp_mode(self):
        """Switch to stamp tool so user clicks anywhere to place the buffer."""
        if not self.dp5_canvas or not self.dp5_canvas._sel_buffer:
            return
        self._select_tool(TOOL_STAMP)
        self._set_status("Stamp mode — click to place, press Esc to exit")

    def _clear_brush(self):
        """Clear the copy buffer and brush thumbnail."""
        if self.dp5_canvas:
            self.dp5_canvas._sel_buffer  = None
            self.dp5_canvas._sel_buf_w   = 0
            self.dp5_canvas._sel_buf_h   = 0
            self.dp5_canvas._sel_floating = False
        if hasattr(self, '_brush_thumb'):
            self._brush_thumb.set_buffer(None, 0, 0)
            self._brush_thumb.set_active(False)
        self._set_status("Brush cleared")

    def _sync_brush_thumb(self):
        """Update the brush thumbnail from the current copy buffer."""
        if not hasattr(self, '_brush_thumb') or not self.dp5_canvas:
            return
        c = self.dp5_canvas
        self._brush_thumb.set_buffer(c._sel_buffer, c._sel_buf_w, c._sel_buf_h)

    def _cut_selection(self):
        if not self.dp5_canvas: return
        self._push_undo()
        self.dp5_canvas.cut_selection()
        self._sync_brush_thumb()
        self._set_status("Selection cut — click Brush thumbnail to stamp")

    def _copy_selection(self):
        if not self.dp5_canvas: return
        self.dp5_canvas.copy_selection()
        self._sync_brush_thumb()
        self._set_status("Selection copied — click Brush thumbnail to stamp")

    def _paste_selection(self):
        """Paste: activate stamp mode so user clicks to place."""
        if not self.dp5_canvas: return
        c = self.dp5_canvas
        if c._sel_buffer and c._sel_buf_w:
            self._activate_stamp_mode()
            self._set_status("Click anywhere to stamp — Esc to exit stamp mode")
        else:
            self._set_status("Nothing to paste")

    def _select_all(self):
        if not self.dp5_canvas: return
        self.dp5_canvas._selection_rect = QRect(0, 0,
                                                self._canvas_width,
                                                self._canvas_height)
        self.dp5_canvas._sel_active = True
        self.dp5_canvas.update()
        self._select_tool(TOOL_SELECT)
        self._set_status("All selected")

    def _deselect(self):
        if not self.dp5_canvas: return
        c = self.dp5_canvas
        if c._sel_floating:
            c._stamp_selection(keep_floating=False)
        c._sel_active        = False
        c._sel_floating      = False
        c._sel_drag_start    = None
        c._selection_rect    = None
        c.update()

    def _set_polygon_sides(self):
        if not self.dp5_canvas: return
        n, ok = QInputDialog.getInt(self, "Polygon", "Number of sides:", 6, 3, 32)
        if ok:
            self.dp5_canvas._polygon_sides = n
            self._set_status(f"Polygon: {n} sides")

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
            z = self._canvas_zoom
            self._zoom_lbl.setText(f"{int(z)}×" if z >= 1 else f"{z:.2f}×")

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

    def _set_zoom(self, z, anchor_widget_pos=None):
        """
        Set zoom level.  anchor_widget_pos: QPoint in scroll-area viewport
        coordinates to keep fixed.  If None, anchors to viewport centre.
        """
        if not self.dp5_canvas: return
        old_z = max(0.01, self.dp5_canvas.zoom)
        z     = max(0.05, min(16, float(z)))
        self.dp5_canvas.zoom = z
        self._canvas_zoom    = z
        self._update_zoom_label()

        # Resize canvas widget to match new zoom
        new_w = max(200, int(self.dp5_canvas.tex_w * z))
        new_h = max(200, int(self.dp5_canvas.tex_h * z))
        self.dp5_canvas.resize(new_w, new_h)
        self.dp5_canvas.updateGeometry()

        # Scroll to keep anchor point fixed
        sa = getattr(self, '_canvas_scroll', None)
        if sa:
            hb = sa.horizontalScrollBar()
            vb = sa.verticalScrollBar()
            if anchor_widget_pos is None:
                # Default anchor: current viewport centre
                vp = sa.viewport()
                anchor_widget_pos = QPoint(vp.width() // 2, vp.height() // 2)
            # Current scroll position + anchor gives the tex-space point
            old_sx = hb.value() + anchor_widget_pos.x()
            old_sy = vb.value() + anchor_widget_pos.y()
            # New scroll position that keeps the same tex point under anchor
            ratio  = z / old_z
            new_sx = int(old_sx * ratio) - anchor_widget_pos.x()
            new_sy = int(old_sy * ratio) - anchor_widget_pos.y()
            hb.setValue(max(0, new_sx))
            vb.setValue(max(0, new_sy))

        self.dp5_canvas.update()

    def _flip_h(self):   self._mirror_h()   # legacy alias
    def _flip_v(self):   self._mirror_v()   # legacy alias

    def _invert(self):
        from PIL import Image, ImageOps
        def _inv(img):
            r, g, b, a = img.split()
            return Image.merge('RGBA', (ImageOps.invert(r), ImageOps.invert(g),
                                        ImageOps.invert(b), a))
        self._pil_transform(_inv)

    def _adjust(self, delta: int):
        if not self.dp5_canvas: return
        self._push_undo()
        for i in range(0, len(self.dp5_canvas.rgba), 4):
            for j in range(3):
                self.dp5_canvas.rgba[i+j] = max(0, min(255,
                    self.dp5_canvas.rgba[i+j] + delta))
        self.dp5_canvas.update()

    def _pil_transform(self, fn):
        """Apply a PIL Image transform to the canvas."""
        if not self.dp5_canvas: return
        self._push_undo()
        from PIL import Image
        img = Image.frombytes('RGBA',
                              (self._canvas_width, self._canvas_height),
                              bytes(self.dp5_canvas.rgba))
        img2 = fn(img)
        # Update canvas dimensions if they changed (scale/rotate 90)
        w2, h2 = img2.size
        self._canvas_width  = w2
        self._canvas_height = h2
        new_rgba = bytearray(img2.tobytes())
        self.dp5_canvas.tex_w = w2
        self.dp5_canvas.tex_h = h2
        self.dp5_canvas.rgba  = new_rgba
        self.dp5_canvas.update()
        self._set_status(f"Canvas: {w2}×{h2}")

    def _rotate_90_cw(self):
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.ROTATE_270))

    def _rotate_90_ccw(self):
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.ROTATE_90))

    def _rotate_180(self):
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.ROTATE_180))

    def _rotate_arbitrary(self):
        deg, ok = QInputDialog.getInt(self, "Rotate", "Degrees (clockwise):",
                                      45, -359, 359)
        if not ok: return
        from PIL import Image
        self._pil_transform(lambda i: i.rotate(-deg, expand=True,
                                               resample=Image.Resampling.BILINEAR))

    def _mirror_h(self):
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.FLIP_LEFT_RIGHT))

    def _mirror_v(self):
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.FLIP_TOP_BOTTOM))

    def _scale_canvas(self):
        """Scale canvas to a new size with a dialog offering presets."""
        if not self.dp5_canvas: return
        dlg = QDialog(self)
        dlg.setWindowTitle("Scale Canvas")
        dlg.setModal(True)
        lay = QVBoxLayout(dlg)
        form = QFormLayout()

        w_spin = QSpinBox(); w_spin.setRange(8, 8192)
        w_spin.setValue(self._canvas_width)
        h_spin = QSpinBox(); h_spin.setRange(8, 8192)
        h_spin.setValue(self._canvas_height)
        form.addRow("Width:", w_spin)
        form.addRow("Height:", h_spin)
        lay.addLayout(form)

        # Quick-preset buttons
        preset_lbl = QLabel("Presets:")
        preset_lbl.setFont(QFont("Arial", 8))
        lay.addWidget(preset_lbl)
        presets = [
            ("32×32",   32,   32),   ("64×64",   64,   64),
            ("128×128", 128,  128),  ("256×256", 256,  256),
            ("512×512", 512,  512),  ("1024×1024",1024, 1024),
            ("2048×2048",2048,2048), ("320×200", 320,  200),
            ("640×480", 640,  480),  ("1280×720",1280, 720),
        ]
        grid = QGridLayout()
        for i, (lbl, pw, ph) in enumerate(presets):
            b = QPushButton(lbl)
            b.setFont(QFont("Arial", 8))
            b.clicked.connect(lambda _, pw=pw, ph=ph:
                              (w_spin.setValue(pw), h_spin.setValue(ph)))
            grid.addWidget(b, i // 4, i % 4)
        lay.addLayout(grid)

        # Resampling method
        resamp_combo = QComboBox()
        resamp_combo.addItems(["Nearest (pixelart)", "Bilinear", "Bicubic", "Lanczos"])
        resamp_form = QFormLayout()
        resamp_form.addRow("Resample:", resamp_combo)
        lay.addLayout(resamp_form)

        btns = QHBoxLayout()
        ok_btn = QPushButton("Scale"); ok_btn.setDefault(True)
        cancel_btn = QPushButton("Cancel")
        ok_btn.clicked.connect(dlg.accept)
        cancel_btn.clicked.connect(dlg.reject)
        btns.addStretch(); btns.addWidget(ok_btn); btns.addWidget(cancel_btn)
        lay.addLayout(btns)

        if dlg.exec():
            from PIL import Image
            methods = [Image.Resampling.NEAREST, Image.Resampling.BILINEAR,
                       Image.Resampling.BICUBIC, Image.Resampling.LANCZOS]
            method = methods[resamp_combo.currentIndex()]
            nw, nh = w_spin.value(), h_spin.value()
            self._pil_transform(lambda i, nw=nw, nh=nh, m=method:
                                i.resize((nw, nh), m))

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
        self._import_bitmap_path(path)

    def _import_bitmap_path(self, path: str):
        """Load an image directly from a file path (called by imgfactory docked)."""
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

            # Auto-fit zoom
            sa = getattr(self, '_canvas_scroll', None)
            if sa:
                vw = sa.viewport().width()
                vh = sa.viewport().height()
            else:
                vw, vh = 800, 600
            z_w = (vw * 0.9) / max(1, w)
            z_h = (vh * 0.9) / max(1, h)
            fit_z = min(z_w, z_h)
            for snap in (16, 8, 4, 2, 1):
                if fit_z >= snap:
                    fit_z = snap; break
            self._set_zoom(max(0.05, fit_z))

            # Extract palette
            p_img    = img.quantize(colors=256)
            pal_flat = p_img.getpalette()
            palette  = [(pal_flat[i*3], pal_flat[i*3+1], pal_flat[i*3+2])
                        for i in range(256)]
            self.pal_bar.set_palette_raw(palette)

            # Add to bitmap list
            name = os.path.basename(path)
            self._bitmap_list.append({'name': name, 'rgba': new_rgba, 'w': w, 'h': h})
            self._bitmap_lw.addItem(name)
            self._bitmap_lw.setCurrentRow(len(self._bitmap_list)-1)

            self._set_status(f"Opened: {name}  {w}×{h}  zoom {self.dp5_canvas.zoom:.2f}×")
        except Exception as e:
            QMessageBox.warning(self, "Open Error", str(e))

    # ── Settings / theme ──────────────────────────────────────────────────────

    def _show_workshop_settings(self):
        """Open DP5-specific settings dialog (NOT the global theme dialog)."""
        old_icon_sz = self.dp5_settings.get('tool_icon_size')
        old_cols    = self.dp5_settings.get('tool_columns')

        dlg = DP5SettingsDialog(self.dp5_settings, self)
        if dlg.exec():
            # Apply changed settings live
            show_left = self.dp5_settings.get('show_bitmap_list')
            if hasattr(self, '_left_panel'):
                self._left_panel.setVisible(show_left)

            if self.dp5_canvas:
                self.dp5_canvas.show_grid = self.dp5_settings.get('show_pixel_grid')
                self.dp5_canvas.update()

            # If icon size or column count changed, rebuild the right panel
            new_icon_sz = self.dp5_settings.get('tool_icon_size')
            new_cols    = self.dp5_settings.get('tool_columns')
            if new_icon_sz != old_icon_sz or new_cols != old_cols:
                self._rebuild_right_panel()

            self._set_status("Settings saved.")

    def _rebuild_right_panel(self):
        """Tear down and reconstruct the right panel in-place after settings change."""
        if not hasattr(self, '_splitter'): return
        old_panel = self._splitter.widget(2)
        if old_panel is None: return
        new_panel = self._create_right_panel()
        self._splitter.replaceWidget(2, new_panel)
        old_panel.deleteLater()
        # Re-select current tool so icons reflect active state
        if self.dp5_canvas:
            self._select_tool(self.dp5_canvas.tool)
        self._update_color_swatches()
        self._sync_brush_thumb()   # restore thumbnail after rebuild

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
        Ctrl  = Qt.KeyboardModifier.ControlModifier
        Shift = Qt.KeyboardModifier.ShiftModifier
        NoMod = Qt.KeyboardModifier.NoModifier

        # Ctrl combos
        if mod == Ctrl:
            if k == Qt.Key.Key_Z: self._undo_canvas()
            elif k == Qt.Key.Key_Y: self._redo_canvas()
            elif k == Qt.Key.Key_X: self._cut_selection()
            elif k == Qt.Key.Key_C: self._copy_selection()
            elif k == Qt.Key.Key_V: self._paste_selection()
            elif k == Qt.Key.Key_A: self._select_all()
            elif k in (Qt.Key.Key_Plus, Qt.Key.Key_Equal):
                z = self._canvas_zoom
                self._set_zoom(z * 1.25 if z < 1 else min(16, z + 1))
            elif k == Qt.Key.Key_Minus:
                z = self._canvas_zoom
                self._set_zoom(max(0.05, z * 0.8 if z <= 1 else z - 1))
            else: super().keyPressEvent(e)
            return

        # Escape — exit stamp mode, cancel floating move, or deselect
        if k == Qt.Key.Key_Escape:
            if self.dp5_canvas and self.dp5_canvas.tool == TOOL_STAMP:
                # Exit stamp mode → back to select tool
                self._select_tool(TOOL_SELECT)
                self._set_status("Stamp mode off")
            elif self.dp5_canvas and self.dp5_canvas._sel_floating:
                self.dp5_canvas.cancel_sel_move()
                self.dp5_canvas._sel_active = True
            else:
                self._deselect()
            if self.dp5_canvas:
                self.dp5_canvas._curve_pts = []
                self.dp5_canvas._poly_pts  = []
                self.dp5_canvas.update()
            return

        # Arrow keys — nudge float OR scroll viewport
        arrow_map = {
            Qt.Key.Key_Left:  (-1,  0),
            Qt.Key.Key_Right: ( 1,  0),
            Qt.Key.Key_Up:    ( 0, -1),
            Qt.Key.Key_Down:  ( 0,  1),
        }
        if k in arrow_map:
            dx, dy = arrow_map[k]
            step = 10 if mod == Shift else 1
            c = self.dp5_canvas
            if c and c._sel_floating:
                # Nudge the floating object
                c.nudge_float(dx * step, dy * step)
            else:
                # Scroll the viewport
                sa = getattr(self, '_canvas_scroll', None)
                if sa:
                    scroll_step = step * max(1, int(self._canvas_zoom))
                    sa.horizontalScrollBar().setValue(
                        sa.horizontalScrollBar().value() + dx * scroll_step)
                    sa.verticalScrollBar().setValue(
                        sa.verticalScrollBar().value() + dy * scroll_step)
            return

        # Enter/Return — stamp floating object and deselect
        if k in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
            if self.dp5_canvas and self.dp5_canvas._sel_floating:
                self.dp5_canvas._stamp_selection(keep_floating=False)
                self.dp5_canvas._sel_active  = False
                self.dp5_canvas._sel_buffer  = None
                self.dp5_canvas._sel_buf_w   = 0
                self.dp5_canvas._sel_buf_h   = 0
                self.dp5_canvas._sel_float_pos = None
                self.dp5_canvas.update()
                self._set_status("Object stamped")
            return
        tool_keys = {
            Qt.Key.Key_P: TOOL_PENCIL,
            Qt.Key.Key_E: TOOL_ERASER,
            Qt.Key.Key_F: TOOL_FILL,
            Qt.Key.Key_S: TOOL_SPRAY,
            Qt.Key.Key_K: TOOL_PICKER,
            Qt.Key.Key_Q: TOOL_CURVE,
            Qt.Key.Key_L: TOOL_LINE,
            Qt.Key.Key_R: TOOL_RECT,
            Qt.Key.Key_C: TOOL_CIRCLE,
            Qt.Key.Key_O: TOOL_POLYGON,
            Qt.Key.Key_T: TOOL_TRIANGLE,
            Qt.Key.Key_Asterisk: TOOL_STAR,
            Qt.Key.Key_M: TOOL_SELECT,
            Qt.Key.Key_G: TOOL_LASSO,
            Qt.Key.Key_H: TOOL_MOVE,
            Qt.Key.Key_Z: TOOL_ZOOM,
            Qt.Key.Key_I: TOOL_TEXT,
        }
        if k in tool_keys:
            self._select_tool(tool_keys[k])
        elif k == Qt.Key.Key_X and mod == NoMod:
            if hasattr(self, '_fgbg_swatch'):
                self._fgbg_swatch.swap()
        elif k == Qt.Key.Key_Delete:
            self._cut_selection()
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
        try:
            from apps.methods.imgfactory_svg_icons import get_dp5_workshop_icon
            workshop.setWindowIcon(get_dp5_workshop_icon(64))
        except Exception:
            pass
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
        app.setApplicationName(App_name)
        app.setApplicationVersion("1.6")
        app.setOrganizationName("X-Seti")

        # Set app icon — appears in taskbar, alt-tab, dock
        try:
            from apps.methods.imgfactory_svg_icons import get_dp5_workshop_icon
            app_icon = get_dp5_workshop_icon(64)
            app.setWindowIcon(app_icon)
        except Exception:
            pass

        w = DP5Workshop()
        w.setWindowTitle(App_name + " – Standalone")
        w.resize(1400, 800)
        w.show()
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)


__all__ = ['DP5Workshop', 'DP5Canvas', 'DP5PaletteBar', 'open_dp5_workshop']
