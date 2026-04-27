#!/usr/bin/env python3
# apps/components/DP5_Workshop/dp5_workshop.py - Version: 13 (Build 333)
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

# - Tool IDs
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
TOOL_CROP          = 'crop'           # crop canvas to selection
TOOL_RESIZE        = 'resize'         # resize canvas
TOOL_DITHER        = 'dither'         # dither brush
TOOL_SYMMETRY      = 'symmetry'       # symmetry/mirror drawing
TOOL_BLUR_BRUSH    = 'blur_brush'     # blur brush (gaussian soften under cursor)
TOOL_SMUDGE        = 'smudge'        # smudge/blend pixels under cursor
TOOL_LIGHTEN       = 'lighten'       # dodge — lighten pixels under cursor
TOOL_DARKEN        = 'darken'        # burn  — darken pixels under cursor

# Shape tools that have an outline/fill toggle via right-click
SHAPE_FILL_PAIRS = {
    TOOL_RECT:     TOOL_FILLED_RECT,
    TOOL_CIRCLE:   TOOL_FILLED_CIRCLE,
    TOOL_TRIANGLE: TOOL_FILLED_TRIANGLE,
    TOOL_POLYGON:  TOOL_FILLED_POLYGON,
    TOOL_STAR:     TOOL_FILLED_STAR,
    TOOL_LASSO:    TOOL_FILLED_LASSO,
}

# - Try importing shared infrastructure
try:
    from apps.gui.tool_menu_mixin import ToolMenuMixin as _ToolMenuMixin
except Exception:
    class _ToolMenuMixin:
        pass

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


# - Tool icon renderer — Photoshop-style white silhouettes on dark tile
def _load_tool_icon(shape: str, size: int = 42, active: bool = False,
                    tile_bg: str = '', icon_col: str = '') -> QIcon:  #vers 4
    """
    Load tool icon — checks in order:
    1. apps/icons/{shape}.svg or .png  (shared icons folder)
    2. DP5_Workshop/icons/{shape}.svg or .png  (DP5-specific overrides)
    3. _make_tool_icon SVG/QPainter renderer  (built-in fallback)
    """
    import os
    # 1. Shared apps/icons/ folder
    try:
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        icon = SVGIconFactory._load_from_file(shape, size, icon_col or None)
        if icon is not None:
            return icon
    except Exception:
        pass
    # 2. DP5-local icons folder
    local_dir = os.path.join(os.path.dirname(__file__), 'icons')
    for ext in ('svg', 'png'):
        fpath = os.path.join(local_dir, f'{shape}.{ext}')
        if os.path.isfile(fpath):
            pix = QPixmap(fpath).scaled(
                size, size,
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation)
            return QIcon(pix)
    # 3. Built-in renderer
    return _make_tool_icon(shape, size, active, tile_bg=tile_bg, icon_col=icon_col)


def _make_tool_icon(shape: str, size: int = 42,
                    active: bool = False,
                    tile_bg: str = '', icon_col: str = '') -> QIcon: #vers 3
    """
    Render a tool icon.  Uses SVGIconFactory.dp_*_icon() when available.
    tile_bg / icon_col override theme defaults — pass from _get_icon_color().
    Normal:  panel_bg tile + text_primary icon (from theme)
    Active:  accent tile + inverted icon
    """
    #    SVG icon map: shape → SVGIconFactory method name                      
    # Add entries here as you create new dp_*_icon() methods in
    # imgfactory_svg_icons.py — they'll be picked up automatically.
    _SVG_MAP = {
        'pencil':          'dp_pencil_icon',
        'eraser':          'dp_eraser_icon',
        'fill':            'dp_bucket_icon',
        'spray':           'dp_brush_icon',
        'picker':          'dp_color_picker_icon',
        'line':            'dp_line_icon',
        'zoom':            'dp_magnify_icon',
        'curve':           'dp_curve_icon',
        'rect':            'dp_rect_icon',
        'filled_rect':     'dp_filled_rect_icon',
        'circle':          'dp_circle_icon',
        'filled_circle':   'dp_filled_circle_icon',
        'triangle':        'dp_triangle_icon',
        'filled_triangle': 'dp_filled_triangle_icon',
        'polygon':         'dp_polygon_icon',
        'filled_polygon':  'dp_filled_polygon_icon',
        'star':            'dp_star_icon',
        'filled_star':     'dp_filled_star_icon',
        'select':          'dp_select_icon',
        'lasso':           'dp_lasso_icon',
        'filled_lasso':    'dp_filled_lasso_icon',
        'text':            'dp_text_icon',
        'stamp':           'dp_stamp_icon',
        'crop':            'dp_crop_icon',
        'resize':          'dp_resize_icon',
        'dither':          'dp_dither_icon',
        'symmetry':        'dp_symmetry_icon',
        'blur_brush':      'dp_blur_brush_icon',
        'smudge':          'dp_smudge_icon',
        'lighten':         'dp_lighten_icon',
        'darken':          'dp_darken_icon',
    }

    if ICONS_AVAILABLE and shape in _SVG_MAP:
        method_name = _SVG_MAP[shape]
        fn = getattr(SVGIconFactory, method_name, None)
        if fn is not None:
            # Use passed colours or fall back to safe defaults
            if not tile_bg:
                tile_bg  = '#d8d8e0' if active else '#1e1e24'
            if not icon_col:
                icon_col = '#101014' if active else '#f0f0f4'
            try:
                # Get the icon rendered transparent (no bg_color — avoids SVG corruption)
                ico = fn(size, color=icon_col)
                # Composite onto tile background manually
                px = QPixmap(size, size)
                px.fill(QColor(tile_bg))
                p  = QPainter(px)
                p.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)
                p.drawPixmap(0, 0, ico.pixmap(size, size))
                p.end()
                return QIcon(px)
            except Exception:
                pass  # fall through to QPainter

    # - QPainter fallback (shapes, lasso, select, text, etc.)
    import math as _m

    _tbg = tile_bg if tile_bg else ('#d8d8e0' if active else '#1e1e24')
    _ink = icon_col if icon_col else ('#101014' if active else '#f0f0f4')
    tile_bg_c = QColor(_tbg)
    ink     = QColor(_ink)

    px = QPixmap(size, size)
    px.fill(tile_bg_c)

    p = QPainter(px)
    p.setRenderHint(QPainter.RenderHint.Antialiasing)
    p.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)

    # Work in a normalised 48×48 space, then scale down via transform
    N  = 48.0
    sc = size / N
    p.scale(sc, sc)

    # Helpers in 48-unit space
    def mk_pen(w=2.5, cap=Qt.PenCapStyle.RoundCap,
               join=Qt.PenJoinStyle.RoundJoin): #vers 1
        return QPen(ink, w, Qt.PenStyle.SolidLine, cap, join)

    def solid_brush(): #vers 1
        return QBrush(ink)

    def poly(*args): #vers 1
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


    def path_from(*segments): #vers 1
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



#  DP5Settings — per-tool settings (JSON, separate from global AppSettings)
class DP5Settings:
    """
    Lightweight JSON settings for DP5 Workshop.
    Stored at ~/.config/imgfactory/dp5_workshop.json
    Completely separate from the global AppSettings/theme system.
    """

    DEFAULTS = {
        'show_bitmap_list':  False,    # left panel visible
        'tool_icon_size':    24,       # tool button pixel size (20–64)
        'tool_icon_color':   'color',  # 'color' | 'white' | 'dark'
        'tool_columns':      3,        # 3, 4, 5, or 6
        'hidden_tools':      [],       # list of tool_ids to hide
        'img_pal_cols':      16,       # image palette columns
        'img_pal_rows':      16,       # image palette max visible rows
        'user_pal_cols':     16,       # user palette columns
        'user_pal_rows':     16,       # user palette max visible rows
        'default_zoom':      4,        # startup zoom level
        'undo_levels':       32,
        'default_width':     320,
        'default_height':    200,
        'retro_palette':     'Amiga AGA WB',
        'show_pixel_grid':   True,
        'grid_color':        '#808080',  # pixel grid colour (hex)
        'platform_mode':     'none',   # 'none'|'c64'|'c64m'|'spectrum'|'msx'|'cpc'|'atari_st'|'amiga'
        'show_cell_grid':    False,    # show platform cell boundaries
        'show_statusbar':    True,     # show bottom status bar
        'ui_font_size':      10,       # toolbar/button font size
        'canvas_mode':       'free',   # 'free'|'platform'|'texture'|'icon'
        'show_anim_strip':   False,    # show animation timeline strip
        'anim_fps':          12,       # default animation FPS
        'zoom_to_fit_resize': False,
        'show_menubar':       False,     # hidden by default — enable in Settings > Menu
        'menu_style':         'dropdown', # 'topbar' | 'dropdown'
        'menu_bar_font_size':  9,         # topbar menubar font size (pt)
        'menu_bar_height':     22,        # topbar menubar height (px)
        'menu_dropdown_font_size': 9,     # dropdown menu item font size (pt)
        'splitter_sizes':    [],             # [left, canvas, right] — saved on close
        # Icon editor
        'char_editor_docked':  False,  # _CharFontEditor dock state
        'sprite_editor_docked': False, # _SpriteEditor dock state
        'svg_browser_docked':  False,  # SVGIconBrowser dock state
        'icon_editor_docked':  False,  # True = snapped to overlay, False = floating
        'icon_editor_x':       -1,     # last window X (-1 = auto)
        'icon_editor_y':       -1,     # last window Y
        'icon_editor_out_fmt': 'PNG',  # last export format
        'icon_editor_alpha':   True,   # colour 0 = alpha
        'icon_editor_alpha_r': 0,      # alpha colour R
        'icon_editor_alpha_g': 0,      # alpha colour G
        'icon_editor_alpha_b': 0,      # alpha colour B
        'icon_editor_amiga_pal':'AGA Workbench (WB3.9)',
    }

    def __init__(self): #vers 1
        cfg_dir = Path.home() / '.config' / 'imgfactory'
        cfg_dir.mkdir(parents=True, exist_ok=True)
        self._path = cfg_dir / 'dp5_workshop.json'
        self._data = dict(self.DEFAULTS)
        self._load()


    def _load(self): #vers 1
        try:
            if self._path.exists():
                loaded = json.loads(self._path.read_text())
                self._data.update({k: v for k, v in loaded.items()
                                   if k in self.DEFAULTS})
        except Exception:
            pass

    def save(self): #vers 1
        try:
            self._path.write_text(json.dumps(self._data, indent=2))
        except Exception:
            pass

    def get(self, key, default=None): #vers 1
        return self._data.get(key, default if default is not None
                              else self.DEFAULTS.get(key))

    def set(self, key, value): #vers 1
        if key in self.DEFAULTS:
            self._data[key] = value


class DP5SettingsDialog(QDialog):
    """Settings dialog for DP5 Workshop — does NOT touch global AppSettings."""

    def __init__(self, dp5_settings: DP5Settings, parent=None): #vers 3
        super().__init__(parent)
        self.s = dp5_settings
        self.setWindowTitle(App_name + " - Settings")
        self.setMinimumWidth(380)
        self.setModal(True)

        root = QVBoxLayout(self)
        tabs = QTabWidget()

        # - Canvas tab
        canvas_tab = QWidget()
        cl = QFormLayout(canvas_tab)
        cl.setSpacing(8)

        self._w_spin = QSpinBox(); self._w_spin.setRange(8, 4096)
        self._w_spin.setValue(self.s.get('default_width'))
        cl.addRow("Default width:", self._w_spin)

        self._h_spin = QSpinBox(); self._h_spin.setRange(8, 4096)
        self._h_spin.setValue(self.s.get('default_height'))
        cl.addRow("Default height:", self._h_spin)

        self._zoom_spin = QSpinBox(); self._zoom_spin.setRange(1, 64)
        self._zoom_spin.setValue(self.s.get('default_zoom'))
        cl.addRow("Default zoom:", self._zoom_spin)

        self._undo_spin = QSpinBox(); self._undo_spin.setRange(4, 128)
        self._undo_spin.setValue(self.s.get('undo_levels'))
        cl.addRow("Undo levels:", self._undo_spin)

        self._grid_chk = QCheckBox()
        self._grid_chk.setChecked(self.s.get('show_pixel_grid'))
        cl.addRow("Show pixel grid:", self._grid_chk)

        self._fit_resize_chk = QCheckBox()
        self._fit_resize_chk.setChecked(self.s.get('zoom_to_fit_resize'))
        self._fit_resize_chk.setToolTip("Always scale canvas to fill the viewport on window resize")
        cl.addRow("Zoom to fit on resize:", self._fit_resize_chk)

        self._img_pal_cols_spin = QSpinBox(); self._img_pal_cols_spin.setRange(4, 32)
        self._img_pal_cols_spin.setValue(self.s.get('img_pal_cols'))
        cl.addRow("Image palette cols:", self._img_pal_cols_spin)

        self._img_pal_rows_spin = QSpinBox(); self._img_pal_rows_spin.setRange(1, 32)
        self._img_pal_rows_spin.setValue(self.s.get('img_pal_rows'))
        cl.addRow("Image palette max rows:", self._img_pal_rows_spin)

        self._user_pal_cols_spin = QSpinBox(); self._user_pal_cols_spin.setRange(4, 32)
        self._user_pal_cols_spin.setValue(self.s.get('user_pal_cols'))
        cl.addRow("User palette cols:", self._user_pal_cols_spin)

        self._user_pal_rows_spin = QSpinBox(); self._user_pal_rows_spin.setRange(1, 32)
        self._user_pal_rows_spin.setValue(self.s.get('user_pal_rows'))
        cl.addRow("User palette max rows:", self._user_pal_rows_spin)

        self._platform_combo = QComboBox()
        self._platform_combo.addItems([
            'none','amiga','amiga_aga','amiga_ham','amiga_ham8','amiga_rtg',
            'c64','c64m','spectrum','zx80','zx81','specnext',
            'msx','cpc','cpc1','atari_st','atari_800','plus4','vic20'])
        self._platform_combo.setCurrentText(self.s.get('platform_mode'))
        cl.addRow("Platform mode:", self._platform_combo)

        self._cell_grid_chk = QCheckBox()
        self._cell_grid_chk.setChecked(self.s.get('show_cell_grid'))
        cl.addRow("Show cell grid:", self._cell_grid_chk)

        self._grid_color_btn = QPushButton()
        gc = QColor(self.s.get('grid_color'))
        self._grid_color_btn.setStyleSheet(f"background:{gc.name()};")
        self._grid_color_btn.setFixedHeight(22)
        self._grid_color_btn.setToolTip("Click to pick pixel grid colour")
        def _pick_grid_color():
            c = QColorDialog.getColor(QColor(self.s.get('grid_color')), self, "Grid Colour",
                                      QColorDialog.ColorDialogOption.ShowAlphaChannel)
            if c.isValid():
                self._grid_color_btn.setStyleSheet(f"background:{c.name()};")
                self._grid_color_btn._chosen = c.name(QColor.NameFormat.HexArgb)
        self._grid_color_btn._chosen = self.s.get('grid_color')
        self._grid_color_btn.clicked.connect(_pick_grid_color)
        cl.addRow("Pixel grid colour:", self._grid_color_btn)

        tabs.addTab(canvas_tab, "Canvas")

        # - Interface tab
        ui_tab = QWidget()
        ul = QFormLayout(ui_tab)
        ul.setSpacing(8)

        self._bitmap_chk = QCheckBox()
        self._bitmap_chk.setChecked(self.s.get('show_bitmap_list'))
        ul.addRow("Show bitmap list panel:", self._bitmap_chk)

        self._statusbar_chk = QCheckBox()
        self._statusbar_chk.setChecked(self.s.get('show_statusbar'))
        ul.addRow("Show status bar:", self._statusbar_chk)

        self._font_size_spin = QSpinBox(); self._font_size_spin.setRange(7, 18)
        self._font_size_spin.setValue(self.s.get('ui_font_size'))
        ul.addRow("UI font size:", self._font_size_spin)

        self._icon_size_spin = QSpinBox(); self._icon_size_spin.setRange(16, 64)
        self._icon_size_spin.setValue(self.s.get('tool_icon_size'))
        ul.addRow("Tool icon size (px):", self._icon_size_spin)

        self._icon_color_combo = QComboBox()
        self._icon_color_combo.addItems(['color', 'white', 'dark'])
        idx = {'color': 0, 'white': 1, 'dark': 2}.get(
            self.s.get('tool_icon_color'), 0)
        self._icon_color_combo.setCurrentIndex(idx)
        ul.addRow("Tool icon colour:", self._icon_color_combo)

        self._cols_combo = QComboBox()
        self._cols_combo.addItems(['3 columns', '4 columns', '5 columns', '6 columns'])
        col_idx = {3: 0, 4: 1, 5: 2, 6: 3}.get(self.s.get('tool_columns'), 0)
        self._cols_combo.setCurrentIndex(col_idx)
        ul.addRow("Gadget columns:", self._cols_combo)

        tabs.addTab(ui_tab, "Interface")

        # - Gadgets tab
        gadgets_tab = QWidget()
        gl = QVBoxLayout(gadgets_tab)
        gl.setSpacing(4)
        gl.addWidget(QLabel("Click to toggle tool visibility (highlighted = visible):"))
        hidden = self.s.get('hidden_tools') or []
        icon_sz = self.s.get('tool_icon_size')
        btn_sz  = max(26, icon_sz + 2)  # min 26px so labels stay readable
        self._gadget_chks = {}
        TOOL_LABELS = [
            ('pencil','Pencil'), ('eraser','Eraser'), ('fill','Fill'),
            ('spray','Spray'), ('picker','Picker'), ('curve','Curve'),
            ('line','Line'), ('rect','Rectangle'), ('circle','Circle'),
            ('triangle','Triangle'), ('polygon','Polygon'), ('star','Star'),
            ('select','Select'), ('lasso','Lasso'), ('zoom','Zoom'),
            ('text','Text'), ('crop','Crop'), ('resize','Resize'),
            ('dither','Dither'), ('symmetry','Symmetry'),
        ]
        grid_w = QWidget()
        grid_l = QGridLayout(grid_w)
        grid_l.setSpacing(4)
        grid_l.setContentsMargins(0, 0, 0, 0)
        cols = 4
        for idx, (tool_id, label) in enumerate(TOOL_LABELS):
            btn = QPushButton()
            btn.setCheckable(True)
            btn.setChecked(tool_id not in hidden)
            btn.setFixedSize(btn_sz, btn_sz + 14)
            btn.setToolTip(label)
            # Use parent workshop's icon colour if available
            _ws  = parent if hasattr(parent, '_get_icon_color') else None
            _col = _ws._get_icon_color() if _ws else '#eeeeee'
            _tbg = ''
            if _ws and _ws.app_settings:
                _tc = _ws.app_settings.get_theme_colors() or {}
                _tbg = _tc.get('gadgetbar_bg', _tc.get('toolbar_bg', ''))
            ico = _load_tool_icon(tool_id, icon_sz, tile_bg=_tbg, icon_col=_col)
            btn.setIcon(ico)
            btn.setIconSize(QSize(icon_sz, icon_sz))
            lbl_short = label[:6]
            btn.setText(lbl_short)
            # Theme-aware stylesheet — no hardcoded colours
            acc = '#4a8a4a'
            if _ws and _ws.app_settings:
                _tc2 = _ws.app_settings.get_theme_colors() or {}
                acc  = _tc2.get('accent_primary', acc)
            btn.setStyleSheet(
                f"QPushButton {{ font-size: 8px; color: palette(mid); "
                f"background: palette(base); border: 1px solid palette(mid); "
                f"padding-top: 2px; }} "
                f"QPushButton:checked {{ background: {acc}; "
                f"border: 1px solid palette(highlight); }}"
            )
            btn.setLayoutDirection(Qt.LayoutDirection.LeftToRight)
            grid_l.addWidget(btn, idx // cols, idx % cols)
            self._gadget_chks[tool_id] = btn
        gl.addWidget(grid_w)
        gl.addStretch()

        # - Menu tab  
        menu_tab = QWidget()
        ml = QFormLayout(menu_tab)
        ml.setSpacing(8)

        self._menu_style_combo = QComboBox()
        self._menu_style_combo.addItems(['topbar', 'dropdown'])
        self._menu_style_combo.setCurrentText(self.s.get('menu_style'))
        ml.addRow("Menu orientation:", self._menu_style_combo)

        self._menu_bar_height_spin = QSpinBox()
        self._menu_bar_height_spin.setRange(16, 40)
        self._menu_bar_height_spin.setValue(self.s.get('menu_bar_height'))
        self._menu_bar_height_spin.setSuffix(" px")
        ml.addRow("Topbar height:", self._menu_bar_height_spin)

        self._menu_bar_font_spin = QSpinBox()
        self._menu_bar_font_spin.setRange(7, 16)
        self._menu_bar_font_spin.setValue(self.s.get('menu_bar_font_size'))
        self._menu_bar_font_spin.setSuffix(" pt")
        ml.addRow("Topbar font size:", self._menu_bar_font_spin)

        self._menu_dropdown_font_spin = QSpinBox()
        self._menu_dropdown_font_spin.setRange(7, 16)
        self._menu_dropdown_font_spin.setValue(self.s.get('menu_dropdown_font_size'))
        self._menu_dropdown_font_spin.setSuffix(" pt")
        ml.addRow("Dropdown font size:", self._menu_dropdown_font_spin)

        tabs.addTab(menu_tab, "Menu")

        tabs.addTab(gadgets_tab, "Gadgets")

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

    def _accept(self): #vers 1
        self.s.set('default_width',    self._w_spin.value())
        self.s.set('default_height',   self._h_spin.value())
        self.s.set('default_zoom',     self._zoom_spin.value())
        self.s.set('undo_levels',      self._undo_spin.value())
        self.s.set('show_pixel_grid',  self._grid_chk.isChecked())
        self.s.set('zoom_to_fit_resize', self._fit_resize_chk.isChecked())
        self.s.set('menu_style',              self._menu_style_combo.currentText())
        self.s.set('menu_bar_height',         self._menu_bar_height_spin.value())
        self.s.set('menu_bar_font_size',      self._menu_bar_font_spin.value())
        self.s.set('menu_dropdown_font_size', self._menu_dropdown_font_spin.value())
        self.s.set('img_pal_cols',       self._img_pal_cols_spin.value())
        self.s.set('img_pal_rows',       self._img_pal_rows_spin.value())
        self.s.set('user_pal_cols',      self._user_pal_cols_spin.value())
        self.s.set('user_pal_rows',      self._user_pal_rows_spin.value())
        self.s.set('platform_mode',      self._platform_combo.currentText())
        self.s.set('show_cell_grid',     self._cell_grid_chk.isChecked())
        self.s.set('grid_color',         self._grid_color_btn._chosen)
        self.s.set('show_bitmap_list', self._bitmap_chk.isChecked())
        self.s.set('show_statusbar',   self._statusbar_chk.isChecked())
        self.s.set('ui_font_size',     self._font_size_spin.value())
        self.s.set('tool_icon_size',   self._icon_size_spin.value())
        self.s.set('tool_icon_color',  self._icon_color_combo.currentText())
        self.s.set('tool_columns',     [3, 4, 5, 6][self._cols_combo.currentIndex()])
        hidden = [tid for tid, chk in self._gadget_chks.items() if not chk.isChecked()]
        self.s.set('hidden_tools',     hidden)
        self.s.save()
        self.accept()



#  DP5 Canvas — pixel-accurate zoomable paint surface

class DP5Canvas(QWidget):
    """Zoomable pixel-accurate paint canvas (inlined from dp5_functions.py)."""

    pixel_changed = pyqtSignal(int, int)

    def __init__(self, width: int, height: int, rgba: bytearray, parent=None): #vers 1
        super().__init__(parent)
        self.tex_w      = width
        self.tex_h      = height
        self.rgba       = rgba
        self.zoom       = 4        # can be float < 1 for zoom-out
        self.offset     = QPoint(0, 0)
        self.tool       = TOOL_PENCIL
        self.color      = QColor(255, 0, 0, 255)
        self.brush_size    = 1
        self.opacity       = 1.0
        self.dither_mode   = 'off'  # 'off' | 'checker' | 'bayer' | 'floyd'
        self.symmetry_mode = 'off'  # 'off' | 'H' | 'V' | 'quad'
        self._dither_toggle = False  # alternates per pixel in dither mode
        self.show_grid  = True  # overridden after settings load in _create_centre_panel
        self.show_cell_grid = False
        self.cell_w = 8
        self.cell_h = 8
        self.grid_color = QColor(128, 128, 128, 60)
        self.onion_skin = False
        self.onion_rgba = None
        self._zoom_mode = 'in'   # 'in'|'out'|'box'|'fit'  — set by gadget right-click
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

    def sizeHint(self): #vers 1
        """Tell the scroll area exactly how big the zoomed canvas is."""
        z = max(0.01, self.zoom)
        return QSize(max(200, int(self.tex_w * z)),
                     max(200, int(self.tex_h * z)))

    #    Coordinate helpers                                                     

    def _widget_to_tex(self, p: QPoint) -> Tuple[int, int]: #vers 1
        z = max(0.01, self.zoom)
        x = int(p.x() / z)
        y = int(p.y() / z)
        if self.snap_grid and self.brush_size > 1:
            snap = self.brush_size
            x = (x // snap) * snap
            y = (y // snap) * snap
        return x, y

    def _tex_to_widget(self, tx: int, ty: int) -> QPoint: #vers 1
        z = max(0.01, self.zoom)
        return QPoint(int(tx * z), int(ty * z))

    #    Pixel access                                                           

    def get_pixel(self, x: int, y: int) -> QColor: #vers 1
        if 0 <= x < self.tex_w and 0 <= y < self.tex_h:
            i = (y * self.tex_w + x) * 4
            return QColor(self.rgba[i], self.rgba[i+1],
                          self.rgba[i+2], self.rgba[i+3])
        return QColor(0, 0, 0, 0)

    def set_pixel(self, x: int, y: int, c: QColor): #vers 1
        if 0 <= x < self.tex_w and 0 <= y < self.tex_h:
            i = (y * self.tex_w + x) * 4
            self.rgba[i:i+4] = [c.red(), c.green(), c.blue(), c.alpha()]

    def set_pixel_brush(self, cx: int, cy: int, c: QColor): #vers 2
        """Paint brush with optional dither and symmetry."""
        s = self.brush_size
        BAYER4 = [[0,8,2,10],[12,4,14,6],[3,11,1,9],[15,7,13,5]]
        if self.dither_mode != 'off':
            swatch = getattr(self._editor, '_fgbg_swatch', None)
            bg_color = swatch._bg if swatch else QColor(0,0,0,255)
        else:
            bg_color = c
        for dy in range(-s+1, s):
            for dx in range(-s+1, s):
                if s == 1 or (dx*dx + dy*dy) < s*s:
                    if self.dither_mode == 'checker':
                        px_c = bg_color if (cx+dx+cy+dy) % 2 == 0 else c
                    elif self.dither_mode == 'bayer':
                        thresh = BAYER4[(cy+dy)%4][(cx+dx)%4] / 16.0
                        px_c = c if thresh < 0.5 else bg_color
                    else:
                        px_c = c
                    self.set_pixel(cx+dx, cy+dy, px_c)
                    # Symmetry mirroring
                    sym = self.symmetry_mode
                    if sym in ('H', 'quad'):
                        mx = self.tex_w - 1 - (cx+dx)
                        self.set_pixel(mx, cy+dy, px_c)
                    if sym in ('V', 'quad'):
                        my = self.tex_h - 1 - (cy+dy)
                        self.set_pixel(cx+dx, my, px_c)
                    if sym == 'quad':
                        mx = self.tex_w - 1 - (cx+dx)
                        my = self.tex_h - 1 - (cy+dy)
                        self.set_pixel(mx, my, px_c)

    #    Drawing ops                                                            

    def flood_fill(self, sx: int, sy: int, fill_col: QColor): #vers 3
        """Scanline flood fill — O(n) time, O(sqrt n) stack depth."""
        if not (0 <= sx < self.tex_w and 0 <= sy < self.tex_h): return
        w, h = self.tex_w, self.tex_h
        ti = (sy*w+sx)*4
        tr,tg,tb,ta = self.rgba[ti], self.rgba[ti+1], self.rgba[ti+2], self.rgba[ti+3]
        fr,fg,fb,fa = fill_col.red(), fill_col.green(), fill_col.blue(), fill_col.alpha()
        if (tr,tg,tb,ta) == (fr,fg,fb,fa): return

        def match(x, y):
            i = (y*w+x)*4
            return (self.rgba[i]==tr and self.rgba[i+1]==tg and
                    self.rgba[i+2]==tb and self.rgba[i+3]==ta)

        stack = [(sx, sy)]
        while stack:
            x, y = stack.pop()
            if not (0 <= y < h): continue
            if not (0 <= x < w and match(x, y)): continue
            # Extend left
            x1 = x
            while x1 > 0 and match(x1-1, y): x1 -= 1
            # Extend right
            x2 = x
            while x2 < w-1 and match(x2+1, y): x2 += 1
            # Paint span
            for xi in range(x1, x2+1):
                i = (y*w+xi)*4
                self.rgba[i]=fr; self.rgba[i+1]=fg
                self.rgba[i+2]=fb; self.rgba[i+3]=fa
            # Seed rows above and below — track entry/exit of matching regions
            for dy in (-1, 1):
                ny = y + dy
                if not (0 <= ny < h): continue
                in_span = False
                for xi in range(x1, x2+1):
                    if match(xi, ny):
                        if not in_span:
                            stack.append((xi, ny))
                            in_span = True
                    else:
                        in_span = False
        self.update()

    def draw_line(self, x0, y0, x1, y1, c: QColor): #vers 1
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

    #    Selection clipboard ops                                                

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

    #    Selection move helpers                                                 

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

    #    Paint                                                                  

    def _do_blur_brush(self, cx: int, cy: int): #vers 1
        """Gaussian-soften the pixels within brush_size radius of (cx, cy)."""
        r = max(1, self.brush_size * 3)
        w, h = self.tex_w, self.tex_h
        buf = self.rgba
        x0 = max(0, cx - r); x1 = min(w, cx + r + 1)
        y0 = max(0, cy - r); y1 = min(h, cy + r + 1)
        bw, bh = x1 - x0, y1 - y0
        if bw <= 0 or bh <= 0:
            return
        # Simple box blur (3 passes) as fast approximation
        for _ in range(3):
            for y in range(y0, y1):
                for x in range(x0, x1):
                    # skip pixels outside circular brush
                    if (x - cx) ** 2 + (y - cy) ** 2 > r * r:
                        continue
                    rc = gc = bc = ac = cnt = 0
                    for dy in (-1, 0, 1):
                        for dx in (-1, 0, 1):
                            nx, ny = x + dx, y + dy
                            if 0 <= nx < w and 0 <= ny < h:
                                i = (ny * w + nx) * 4
                                rc += buf[i]; gc += buf[i+1]
                                bc += buf[i+2]; ac += buf[i+3]
                                cnt += 1
                    if cnt:
                        i = (y * w + x) * 4
                        buf[i]   = rc // cnt; buf[i+1] = gc // cnt
                        buf[i+2] = bc // cnt; buf[i+3] = ac // cnt

    def _do_smudge(self, x0: int, y0: int, x1: int, y1: int): #vers 1
        """Drag/smear pixels from (x0,y0) toward (x1,y1) within brush_size radius."""
        r   = max(1, self.brush_size * 2)
        w, h = self.tex_w, self.tex_h
        buf = self.rgba
        dx, dy = x1 - x0, y1 - y0
        if dx == 0 and dy == 0:
            return
        # Smear: blend each pixel with its neighbour in the drag direction
        strength = 0.4
        px0 = max(0, min(x0, x1) - r); px1 = min(w, max(x0, x1) + r + 1)
        py0 = max(0, min(y0, y1) - r); py1 = min(h, max(y0, y1) + r + 1)
        for y in range(py0, py1):
            for x in range(px0, px1):
                if (x - x1) ** 2 + (y - y1) ** 2 > r * r:
                    continue
                sx = max(0, min(w - 1, x - dx))
                sy = max(0, min(h - 1, y - dy))
                si = (sy * w + sx) * 4
                di = (y  * w + x ) * 4
                for c in range(4):
                    buf[di + c] = int(buf[di + c] * (1 - strength)
                                    + buf[si + c] * strength)

    def _do_dodge_burn(self, cx: int, cy: int, amount: int): #vers 1
        """Lighten (amount>0) or darken (amount<0) pixels in brush radius."""
        r  = max(1, self.brush_size * 3)
        w, h = self.tex_w, self.tex_h
        buf = self.rgba
        for y in range(max(0, cy - r), min(h, cy + r + 1)):
            for x in range(max(0, cx - r), min(w, cx + r + 1)):
                if (x - cx) ** 2 + (y - cy) ** 2 > r * r:
                    continue
                i = (y * w + x) * 4
                # Feather by distance
                dist = ((x-cx)**2 + (y-cy)**2) ** 0.5
                fade = max(0.0, 1.0 - dist / r)
                adj  = int(amount * fade)
                buf[i]   = max(0, min(255, buf[i]   + adj))
                buf[i+1] = max(0, min(255, buf[i+1] + adj))
                buf[i+2] = max(0, min(255, buf[i+2] + adj))


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

        # Onion skin — previous frame at 40% opacity
        if self.onion_skin and self.onion_rgba and len(self.onion_rgba) == self.tex_w*self.tex_h*4:
            onion_img = QImage(bytes(self.onion_rgba), self.tex_w, self.tex_h,
                               self.tex_w*4, QImage.Format.Format_RGBA8888)
            onion_scaled = onion_img.scaled(sw, sh, Qt.AspectRatioMode.IgnoreAspectRatio,
                                            Qt.TransformationMode.FastTransformation)
            painter.setOpacity(0.4)
            painter.drawImage(0, 0, onion_scaled)
            painter.setOpacity(1.0)

        # Pixel grid (visible at zoom ≥2)
        if self.show_grid and z >= 2:
            iz = max(1, int(z))
            pen = QPen(self.grid_color, 1)
            painter.setPen(pen)
            for x in range(0, sw, iz):
                painter.drawLine(x, 0, x, sh)
            for y in range(0, sh, iz):
                painter.drawLine(0, y, sw, y)

        # Platform cell grid
        if self.show_cell_grid:
            cw = max(1, int(self.cell_w * z))
            ch = max(1, int(self.cell_h * z))
            pen = QPen(QColor(255, 100, 0, 120), 1, Qt.PenStyle.DashLine)
            painter.setPen(pen)
            for x in range(0, sw+1, cw):
                painter.drawLine(x, 0, x, sh)
            for y in range(0, sh+1, ch):
                painter.drawLine(0, y, sw, y)

        # Colour clash visualiser — highlight ZX Spectrum attribute violations
        if getattr(self, '_show_clash', False) and self.cell_w == 8 and self.cell_h == 8:
            cols_cells = self.tex_w // 8
            rows_cells = self.tex_h // 8
            painter.setOpacity(0.45)
            for cy in range(rows_cells):
                for cx in range(cols_cells):
                    colours = set()
                    for py in range(8):
                        for px in range(8):
                            i = ((cy*8+py)*self.tex_w + (cx*8+px))*4
                            r,g,b,a = self.rgba[i:i+4]
                            if a > 0:
                                colours.add((r>>5, g>>5, b>>5))
                    if len(colours) > 2:
                        x0 = int(cx*8*z); y0 = int(cy*8*z)
                        cw2 = max(1,int(8*z)); ch2 = max(1,int(8*z))
                        painter.fillRect(x0, y0, cw2, ch2, QColor(255,0,0,160))
            painter.setOpacity(1.0)

        # Shape / selection preview overlay (drag-to-draw tools only)
        shape_tools = (TOOL_LINE,
                       TOOL_RECT,     TOOL_FILLED_RECT,
                       TOOL_CIRCLE,   TOOL_FILLED_CIRCLE,
                       TOOL_TRIANGLE, TOOL_FILLED_TRIANGLE,
                       TOOL_STAR,     TOOL_FILLED_STAR,
                       TOOL_SELECT)
        # Also draw box-zoom preview
        is_box_zoom = (self.tool == TOOL_ZOOM and self._zoom_mode == 'box')
        if self._preview_start and self._preview_end and \
                (self.tool in shape_tools or is_box_zoom):
            pen = QPen(QColor(0,200,255) if is_box_zoom else self.color,
                       1, Qt.PenStyle.DashLine)
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

    #    Mouse events                                                           

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

        elif self.tool == TOOL_BLUR_BRUSH:
            self._push_undo_canvas()
            self._do_blur_brush(tx, ty); self.update()

        elif self.tool == TOOL_SMUDGE:
            self._push_undo_canvas()
            self._smudge_last = (tx, ty)
            self._do_smudge(tx, ty, tx, ty); self.update()

        elif self.tool == TOOL_LIGHTEN:
            self._push_undo_canvas()
            self._do_dodge_burn(tx, ty, 30); self.update()

        elif self.tool == TOOL_DARKEN:
            self._push_undo_canvas()
            self._do_dodge_burn(tx, ty, -30); self.update()

        elif self.tool == TOOL_PICKER:
            c = self.get_pixel(tx, ty)
            if c.isValid():
                self.color = c
                ed = self._editor
                if ed: ed._update_color_swatches()

        elif self.tool == TOOL_ZOOM:
            ed = self._editor
            if not ed: pass
            elif self._zoom_mode == 'out':
                ed._set_zoom(max(0.05, ed._canvas_zoom * 0.5))
            elif self._zoom_mode == 'fit':
                ed._fit_canvas_to_viewport()
            elif self._zoom_mode == 'box':
                # Start box-zoom drag — handled in mouseMoveEvent/mouseReleaseEvent
                self._box_zoom_start = (tx, ty)
                self._box_zoom_end   = (tx, ty)
                self._preview_start  = (tx, ty)
                self._preview_end    = (tx, ty)
            else:  # 'in'
                ed._set_zoom(min(64, ed._canvas_zoom * 2))

        elif self.tool == TOOL_MOVE:
            if self._sel_buffer and self._sel_buf_w > 0:
                #    Floating object mode   
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
                #    Pan mode (no object to move)   
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
                #    Click INSIDE committed selection → lift and start move   
                if not self._sel_floating:
                    self._push_undo_canvas()
                    self._lift_selection()
                self._sel_drag_start = (tx, ty)
                self._drawing = True
            else:
                #    Click OUTSIDE → stamp any float, start new selection   
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
        # Store for zoom lens tracking
        self._zoom_lens_pos = (tx, ty)

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

        elif self.tool == TOOL_BLUR_BRUSH:
            self._do_blur_brush(tx, ty); self.update()

        elif self.tool == TOOL_LIGHTEN:
            self._do_dodge_burn(tx, ty, 30); self.update()

        elif self.tool == TOOL_DARKEN:
            self._do_dodge_burn(tx, ty, -30); self.update()

        elif self.tool == TOOL_SMUDGE:
            lp = getattr(self, '_smudge_last', None)
            if lp:
                self._do_smudge(lp[0], lp[1], tx, ty)
            self._smudge_last = (tx, ty); self.update()

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
            #    Dragging a floating selection   
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

        elif self.tool == TOOL_ZOOM and self._zoom_mode == 'box' and                 hasattr(self, '_box_zoom_start') and self._box_zoom_start:
            self._box_zoom_end = (tx, ty)
            self._preview_end  = (tx, ty)
            self.update()

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

        elif self.tool == TOOL_ZOOM and self._zoom_mode == 'box' and \
                hasattr(self, '_box_zoom_start') and self._box_zoom_start:
            # Box zoom — calculate zoom to fit selection into viewport
            ed = self._editor
            if ed:
                x0, y0 = self._box_zoom_start
                x1, y1 = tx, ty
                bw = max(1, abs(x1-x0)); bh = max(1, abs(y1-y0))
                sa = getattr(ed, '_canvas_scroll', None)
                vw = sa.viewport().width()  if sa else 800
                vh = sa.viewport().height() if sa else 600
                z = min(vw/bw, vh/bh, 16)
                ed._set_zoom(max(0.05, z))
                # Scroll to centre the zoomed region
                if sa:
                    cx = (min(x0,x1) + bw//2) * z
                    cy = (min(y0,y1) + bh//2) * z
                    sa.horizontalScrollBar().setValue(int(cx - vw//2))
                    sa.verticalScrollBar().setValue(int(cy - vh//2))
            self._box_zoom_start = None
            self._preview_start  = None
            self._preview_end    = None

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
                #    End of floating drag — stamp non-destructively and stay floating   
                # (User can drag again; clicking outside will stamp permanently)
                self._sel_drag_start = None
                # keep _sel_floating = True so they can reposition
            elif ps:
                #    New marquee drawn   
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
            new_zoom = min(64, old_zoom + 1) if d > 0 else max(1, old_zoom - 1)
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



#  PaletteGrid — shared 2D swatch grid used for BOTH image and user palettes


class PaletteGrid(QWidget):
    """
    2D colour swatch grid that auto-wraps to fill its available width.
    Column count is computed from widget width ÷ cell size, so the grid
    reflows automatically when the panel is resized.
    Used for both the image palette and user/retro preset palette.
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
        self._cols_hint = cols   # used only when widget has no width yet
        self._cell      = cell
        self._colors: List[QColor] = [QColor(*e) for e in self._DEFAULT_ENTRIES]
        self._selected  = -1
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self.setToolTip("Click to select colour — right-click to copy hex")
        self._recalc_height()


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

    def _effective_cols(self) -> int:
        """Columns that fit in current width, falling back to hint."""
        w = self.width()
        if w > self._cell:
            return max(1, w // self._cell)
        return max(1, self._cols_hint)

    def _recalc_height(self):
        cols = self._effective_cols()
        rows = max(1, (len(self._colors) + cols - 1) // cols)
        self.setFixedHeight(rows * self._cell + 1)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        self._recalc_height()

    def set_colors(self, colors: List[QColor], cols: int = None): #vers 5
        """Load a new colour list. Auto-scales cell size; cols wraps to width."""
        self._colors   = list(colors)
        self._selected = -1
        n = len(colors)
        if n >= 4096:
            self._cell = 2
        elif n >= 512:
            self._cell = 4
        elif n > 256:
            self._cell = 4
        elif n > 64:
            self._cell = 8
        else:
            self._cell = 13
        if cols:
            self._cols_hint = cols
        self._recalc_height()
        self.update()

    def set_palette_raw(self, palette_data): #vers 4
        """Accept list of (r,g,b) tuples, QColors or hex strings. No size limit."""
        out = []
        for entry in palette_data:
            if isinstance(entry, QColor):
                out.append(entry)
            elif isinstance(entry, str):
                out.append(QColor(entry))
            elif hasattr(entry, '__len__') and len(entry) >= 3:
                out.append(QColor(int(entry[0]), int(entry[1]), int(entry[2])))
        self._colors   = out
        self._selected = -1
        n = len(out)
        if n >= 4096:
            self._cell = 2
        elif n >= 512:
            self._cell = 4
        elif n > 256:
            self._cell = 4
        elif n > 64:
            self._cell = 8
        else:
            self._cell = 13
        self._recalc_height()
        self.update()

    def set_selection_by_color(self, c: QColor):
        for i, p in enumerate(self._colors):
            if p.rgb() == c.rgb():
                self._selected = i
                self.update()
                return

    #    Paint                                                                  

    def paintEvent(self, event):
        p    = QPainter(self)
        cs   = self._cell
        cols = self._effective_cols()
        gap  = 1 if cs >= 4 else 0
        for i, col in enumerate(self._colors):
            x = (i % cols) * cs
            y = (i // cols) * cs
            p.fillRect(x, y, max(1, cs - gap), max(1, cs - gap), col)
            if i == self._selected and cs >= 4:
                p.setPen(QPen(QColor(0, 0, 0), 1))
                p.drawRect(x, y, cs - 2, cs - 2)
                p.setPen(QPen(self._get_ui_color('viewport_bg'), 1))
                p.drawRect(x + 1, y + 1, cs - 4, cs - 4)

    #    Mouse                                                                  

    def mousePressEvent(self, e: QMouseEvent):
        cs   = self._cell
        cols = self._effective_cols()
        col  = e.position().toPoint().x() // cs
        row  = e.position().toPoint().y() // cs
        idx  = row * cols + col
        if 0 <= idx < len(self._colors):
            self._selected = idx
            self.color_picked.emit(self._colors[idx])
            self.update()

    #    Compat alias (legacy callers used set_palette)                         

    def set_palette(self, palette_data):
        self.set_palette_raw(palette_data)



class _AutoCellPaletteGrid(PaletteGrid):
    """User palette variant: cell size grows to fill available width.

    Instead of a fixed cell size, we divide the widget width by the column
    count so swatches always tile edge-to-edge regardless of panel width.
    On resize the cells re-scale automatically.
    """


    def _get_ui_color(self, key): #vers 1
        """Return theme QColor via palette fallback."""
        from PyQt6.QtGui import QColor
        pal = self.palette()
        _map = {
            'viewport_bg':   pal.ColorRole.Base,
            'viewport_text': pal.ColorRole.PlaceholderText,
            'border':        pal.ColorRole.Mid,
            'bg_primary':    pal.ColorRole.Window,
            'bg_secondary':  pal.ColorRole.AlternateBase,
            'text_primary':  pal.ColorRole.WindowText,
            'accent_primary':pal.ColorRole.Highlight,
        }
        return pal.color(_map.get(key, pal.ColorRole.WindowText))

    def __init__(self, cols: int = 16, parent=None):
        self._fixed_cols = cols   # must be set BEFORE super().__init__ calls _recalc_height
        super().__init__(cols=cols, cell=13, parent=parent)

    def _effective_cols(self) -> int:
        return max(1, self._fixed_cols)

    def _cell_size(self) -> int:
        """Cell size = floor(widget_width / cols), minimum 4px."""
        w = self.width()
        if w <= 0:
            return self._cell
        return max(4, w // self._fixed_cols)

    def set_colors(self, colors, cols: int = None):
        if cols:
            self._fixed_cols = max(1, cols)
        super().set_colors(colors, cols=None)   # don't let super touch _cols_hint

    def set_palette_raw(self, palette_data):
        super().set_palette_raw(palette_data)

    def _recalc_height(self):
        cs   = self._cell_size()
        cols = self._fixed_cols
        rows = max(1, (len(self._colors) + cols - 1) // cols)
        self.setFixedHeight(rows * cs + 1)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        self._recalc_height()
        self.update()

    def paintEvent(self, event):
        from PyQt6.QtGui import QPainter, QPen, QColor
        p    = QPainter(self)
        cs   = self._cell_size()
        cols = self._fixed_cols
        gap  = 1 if cs >= 6 else 0
        for i, col in enumerate(self._colors):
            x = (i % cols) * cs
            y = (i // cols) * cs
            p.fillRect(x, y, max(1, cs - gap), max(1, cs - gap), col)
            if i == self._selected and cs >= 6:
                from PyQt6.QtGui import QPen
                p.setPen(QPen(QColor(0, 0, 0), 1))
                p.drawRect(x, y, cs - 2, cs - 2)
                p.setPen(QPen(self._get_ui_color('viewport_bg'), 1))
                p.drawRect(x + 1, y + 1, cs - 4, cs - 4)

    def mousePressEvent(self, e):
        from PyQt6.QtCore import Qt
        cs   = self._cell_size()
        cols = self._fixed_cols
        col  = e.position().toPoint().x() // cs
        row  = e.position().toPoint().y() // cs
        idx  = row * cols + col
        if 0 <= idx < len(self._colors):
            self._selected = idx
            self.color_picked.emit(self._colors[idx])
            self.update()


# Keep an alias so any stale references to DP5PaletteBar still resolve
DP5PaletteBar = PaletteGrid



#  Colour Picker Widget (simple fallback — no screen capture)


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



#  FGBGSwatch — single DPaint5-style nested FG/BG colour indicator


class FGBGSwatch(QWidget):
    """
    Classic DPaint5 nested colour swatch:
      - outer rect = Background colour  (click outer area to pick BG)
      - inner rect = Foreground colour  (click inner area to pick FG)
    Swap button (S key / double-click) swaps FG ↔ BG.
    """

    fg_changed = pyqtSignal(QColor)
    bg_changed = pyqtSignal(QColor)


    def _get_ui_color(self, key): #vers 1
        """Return theme QColor via palette fallback."""
        from PyQt6.QtGui import QColor
        pal = self.palette()
        _map = {
            'viewport_bg':   pal.ColorRole.Base,
            'viewport_text': pal.ColorRole.PlaceholderText,
            'border':        pal.ColorRole.Mid,
            'bg_primary':    pal.ColorRole.Window,
            'bg_secondary':  pal.ColorRole.AlternateBase,
            'text_primary':  pal.ColorRole.WindowText,
            'accent_primary':pal.ColorRole.Highlight,
        }
        return pal.color(_map.get(key, pal.ColorRole.WindowText))

    def __init__(self, parent=None):
        super().__init__(parent)
        self._fg = QColor(255, 0,   0,   255)
        self._bg = QColor(0,   0,   0,   255)
        # No fixed size — grows with panel width (min 40x30)
        self.setMinimumSize(40, 30)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Preferred)
        self.setToolTip(
            "FG (inner) / BG (outer)\n"
            "Click inner area → pick FG\n"
            "Click outer area → pick BG\n"
            "Double-click → swap FG↔BG")

    def sizeHint(self):
        from PyQt6.QtCore import QSize
        return QSize(64, 48)

    def heightForWidth(self, w: int) -> int:
        return max(30, int(w * 0.75))

    #    Properties                                                             

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

    #    Paint                                                                  

    def paintEvent(self, _):
        p  = QPainter(self)
        w, h = self.width(), self.height()
        pad, gap = 4, 8    # outer border, FG offset from BG rect

        # BG rect (outer, slightly offset toward bottom-right)
        bg_r = QRect(gap, gap, w - gap - pad, h - gap - pad)
        p.fillRect(bg_r, self._bg)
        p.setPen(QPen(self._get_ui_color('viewport_text'), 1))
        p.drawRect(bg_r)

        # FG rect (inner, offset toward top-left)
        fg_r = QRect(pad, pad, w - gap - pad, h - gap - pad)
        p.fillRect(fg_r, self._fg)
        p.setPen(QPen(self._get_ui_color('border'), 1))
        p.drawRect(fg_r)

    def _fg_rect(self) -> QRect:
        w, h = self.width(), self.height()
        pad, gap = 4, 8
        return QRect(pad, pad, w - gap - pad, h - gap - pad)

    def _bg_rect(self) -> QRect:
        w, h = self.width(), self.height()
        pad, gap = 4, 8
        return QRect(gap, gap, w - gap - pad, h - gap - pad)

    #    Mouse                                                                  

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



#  BrushManager — panel listing saved brushes, load/save/delete


class _CanvasTextOverlay(QWidget):
    """Inline text input that floats over the canvas — no dialog needed.
    User types directly; Enter commits to canvas, Escape cancels."""

    def __init__(self, editor, tx: int, ty: int, zoom: float, canvas, parent=None):
        super().__init__(parent or editor)
        self._editor = editor
        self._tx = tx; self._ty = ty
        self._zoom = zoom; self._canvas = canvas

        self.setWindowFlags(Qt.WindowType.FramelessWindowHint |
                            Qt.WindowType.Tool)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)
        self.setAutoFillBackground(False)

        lay = QHBoxLayout(self)
        lay.setContentsMargins(2, 2, 2, 2)
        lay.setSpacing(4)

        self._edit = QLineEdit()
        self._edit.setPlaceholderText("Type text…")
        self._edit.setMinimumWidth(120)
        self._edit.returnPressed.connect(self._commit)
        self._edit.setStyleSheet(
            "QLineEdit { background:palette(base); color:palette(buttonText); border:1px solid #00aaff;"
            " padding:2px 4px; font-size:11px; }")

        self._size_spin = QSpinBox()
        self._size_spin.setRange(4, 200)
        self._size_spin.setValue(12)
        self._size_spin.setFixedWidth(44)
        self._size_spin.setToolTip("Font size (px)")
        self._size_spin.setStyleSheet("QSpinBox { background:palette(base); color:#fff; border:1px solid palette(mid); }")

        ok_btn = QPushButton("✓")
        ok_btn.setFixedSize(22, 22)
        ok_btn.clicked.connect(self._commit)
        ok_btn.setStyleSheet("QPushButton { background:palette(highlight); color:#fff; border:none; }"
                             "QPushButton:hover { background:palette(highlight); }")
        esc_btn = QPushButton("✕")
        esc_btn.setFixedSize(22, 22)
        esc_btn.clicked.connect(self.close)
        esc_btn.setStyleSheet("QPushButton { background:#440000; color:#fff; border:none; }"
                              "QPushButton:hover { background:#880000; }")

        lay.addWidget(self._edit)
        lay.addWidget(self._size_spin)
        lay.addWidget(ok_btn)
        lay.addWidget(esc_btn)
        self.adjustSize()

    def keyPressEvent(self, e):
        if e.key() == Qt.Key.Key_Escape:
            self.close()
        else:
            super().keyPressEvent(e)

    def _commit(self): #vers 1
        """Blit the typed text onto the canvas at (tx, ty)."""
        text = self._edit.text().strip()
        if not text:
            self.close(); return
        fs = self._size_spin.value()
        ed = self._editor
        if not ed.dp5_canvas:
            self.close(); return
        ed._push_undo()
        tmp = QImage(ed._canvas_width, ed._canvas_height,
                     QImage.Format.Format_RGBA8888)
        tmp.fill(Qt.GlobalColor.transparent)
        p = QPainter(tmp)
        font = QFont("Arial", fs)
        p.setFont(font)
        p.setPen(ed.dp5_canvas.color)
        p.drawText(self._tx, self._ty + fs, text)
        p.end()
        for row in range(ed._canvas_height):
            for col in range(ed._canvas_width):
                pix = tmp.pixel(col, row)
                a = (pix >> 24) & 0xFF
                if a > 0:
                    i = (row * ed._canvas_width + col) * 4
                    ed.dp5_canvas.rgba[i]   = (pix >> 16) & 0xFF
                    ed.dp5_canvas.rgba[i+1] = (pix >>  8) & 0xFF
                    ed.dp5_canvas.rgba[i+2] =  pix        & 0xFF
                    ed.dp5_canvas.rgba[i+3] = a
        ed.dp5_canvas.update()
        ed._set_status(f"Text: '{text}' at {self._tx},{self._ty}")
        self.close()


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



#  BrushThumbnail — preview of the copy buffer, click to activate stamp mode


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



#  ColorPalPresetsMixin — retro palette presets (inlined from color_pal_presets.py)


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

        # Amstrad CPC — 27 hardware colours (3 levels each of R,G,B)
        amstrad_cpc = [
            "#000000","#000080","#0000FF",
            "#800000","#800080","#8000FF",
            "#FF0000","#FF0080","#FF00FF",
            "#008000","#008080","#0080FF",
            "#808000","#808080","#8080FF",
            "#FF8000","#FF8080","#FF80FF",
            "#00FF00","#00FF80","#00FFFF",
            "#80FF00","#80FF80","#80FFFF",
            "#FFFF00","#FFFF80","#FFFFFF",
        ]

        # Amiga OCS — 32 colour default Workbench palette (4096 colour HAM space, 32 registers)
        amiga_ocs = [
            "#000000","#FFFFFF","#888888","#FF8800",
            "#CC0000","#0000CC","#00AAAA","#CC00CC",
            "#00CC00","#EEEE00","#FF6600","#AA4400",
            "#4488FF","#AA00AA","#00BBBB","#AAAAAA",
            "#222222","#444444","#666666","#999999",
            "#BBBBBB","#DDDDDD","#1166CC","#CC4400",
            "#006600","#BB8800","#880000","#004488",
            "#CC8844","#448844","#884488","#44AACC",
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
            # Greyscale (hue 0)
            "#060606","#060606","#111111","#222222","#2F2F2F","#3D3D3D","#4F4F4F","#636363",
            "#6A6A6A","#7A7A7A","#8E8E8E","#A3A3A3","#B4B4B4","#C7C7C7","#E1E1E1","#F4F4F4",
            # Gold/yellow-green (hue 1)
            "#0B0100","#160C03","#261908","#352A0A","#44370B","#55460F","#665712","#7C6C15",
            "#837216","#948317","#A7981B","#B9AA19","#D1C01A","#E2D11F","#EDE72D","#F0EF3C",
            # Orange-brown (hue 2)
            "#1E0002","#2C0603","#3E0C05","#501B0D","#5F2A0B","#6D360D","#804509","#91550C",
            "#9C5F0F","#B0720F","#C68213","#D8961C","#E5A927","#ECBA37","#EED348","#EDE457",
            # Red-orange (hue 3)
            "#2D0505","#3A0706","#4C0501","#5A0A03","#6D1708","#80260E","#953618","#A54423",
            "#AE4A28","#C15A37","#D76F48","#E7815B","#E8936C","#E8A37A","#E8B78C","#EACB9D",
            # Pink-red (hue 4)
            "#2E000A","#400116","#4E0322","#5F022F","#6E0A3C","#80184B","#97275F","#AA3772",
            "#AF3F79","#C34D87","#D95F9C","#E16FAD","#E07FC2","#E18FD1","#E4A5DE","#E3B4E0",
            # Purple (hue 5)
            "#29034C","#35055B","#45006B","#56007D","#63078C","#72149E","#8721B1","#982EC2",
            "#9D35CA","#B144D5","#C655D8","#D865DA","#DC76DA","#DD85DC","#DD97DD","#DEA9DF",
            # Violet-blue (hue 6)
            "#150089","#24039C","#3303AF","#4005C1","#4F0BD2","#5C15D5","#6F24D9","#7F30D9",
            "#8436D8","#9744DA","#AA55D7","#B865D7","#CA72D6","#D783D8","#D695D7","#D5A5D5",
            # Blue (hue 7)
            "#0705A8","#0B01B8","#1A00D1","#2406DA","#3111D8","#3B1BD6","#4D29D5","#5937D3",
            "#643ED2","#734BD2","#865DD5","#946BD5","#A379CF","#B38BD2","#C49CD0","#CFADD0",
            # Blue-cyan (hue 8)
            "#0505A1","#0204B1","#0809C6","#0B14D5","#161CD4","#2127D3","#2F3AD4","#3B46D3",
            "#454DD4","#5258D2","#616AD1","#7078CD","#8087CD","#8E9ACE","#9FACCC","#B1BACB",
            # Cyan-teal (hue 9)
            "#04006F","#000A83","#001594","#071FA3","#042CB0","#0E38C2","#164AD2","#2155CD",
            "#265BCB","#306ACD","#447DCC","#518BCB","#5D9AC9","#6CA9C6","#80BCC6","#8EC6C7",
            # Teal-green (hue A)
            "#040C30","#04183B","#002147","#012E58","#003D64","#074A74","#0D5C87","#136692",
            "#196D99","#237CA8","#348FBC","#3E9BC7","#49ABC6","#58BEC3","#69C4C7","#79C5C5",
            # Green-teal (hue B)
            "#041608","#002209","#053207","#003B0D","#074919","#005825","#016936","#0D743F",
            "#157C45","#238A52","#319E65","#39A96F","#46BA7D","#56C68C","#67C69E","#75C7B0",
            # Green (hue C)
            "#011B02","#012702","#063604","#033E04","#094D00","#075C00","#126F05","#197E08",
            "#1E8208","#2B900E","#36A51A","#44B326","#50C131","#61C53E","#71C64D","#82C75C",
            # Yellow-green (hue D)
            "#011806","#052402","#003404","#054000","#074B00","#135900","#1F6D00","#2D7D00",
            "#338306","#3F9100","#51A207","#5EB107","#6BC300","#7DCC0E","#8CCB1A","#9ECB28",
            # Olive (hue E)
            "#040F00","#011B00","#0B2903","#153704","#1F4305","#2B5002","#396003","#497005",
            "#507805","#608801","#6F9B00","#81AB09","#8EBB00","#9FCB06","#B2D00C","#C4D014",
            # Dark gold (hue F)
            "#080401","#140D03","#221C04","#312904","#3D3707","#4C4406","#5C5504","#6D6407",
            "#746B06","#807907","#938A07","#A79D08","#B6AF06","#C8BF00","#D3D40E","#D5D619",
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

        # MSX1 / TMS9918 — 16 colours
        msx1 = [
            "#000000","#010101","#3EB849","#74D07D",
            "#5955E0","#8076F1","#B95E51","#65DBEF",
            "#DB6559","#FF897D","#CCC35E","#DED087",
            "#3AA241","#B766B5","#CCCCCC","#FFFFFF",
        ]

        # Atari ST — 16 standard colours (default low-res Workbench palette)
        atari_st = [
            "#FFFFFF","#FF0000","#00FF00","#FFFF00",
            "#0000FF","#FF00FF","#00FFFF","#BBBBBB",
            "#888888","#FF8888","#88FF88","#FFFF88",
            "#8888FF","#FF88FF","#88FFFF","#000000",
        ]

        # Commodore Plus/4 — 16 base colours (standard luma 7 approximation)
        plus4 = [
            "#000000","#FFFFFF","#F36C6C","#73C3C3",
            "#D36CD3","#73C373","#6C6CD3","#D3D373",
            "#D38B6C","#8B8B6C","#F38BC3","#6C6C6C",
            "#8B8B8B","#B3F3B3","#9B9BF3","#B3B3B3",
        ]

        # VIC-20 — 16 colours
        vic20 = [
            "#000000","#FFFFFF","#782922","#87D6DD",
            "#AA5FB6","#55A049","#40318D","#BFCE72",
            "#AA7449","#C9B887","#EA9090","#B2D4DC",
            "#CE8DF6","#9DE88B","#8080E0","#E0E084",
        ]

        # VIC-20 — 16 colours
        vic20 = [
            "#000000","#FFFFFF","#782922","#87D6DD",
            "#AA5FB6","#55A049","#40318D","#BFCE72",
            "#AA7449","#C9B887","#EA9090","#B2D4DC",
            "#CE8DF6","#9DE88B","#8080E0","#E0E084",
        ]

        # Atari ST — 9-bit palette (3R 3G 3B) 512 colours, show 64 representative
        # 16 on screen in low-res. Scale: val*36 + val//2
        def _st9(v): return v*36 + v//2
        atari_st_512 = [f"#{_st9(r):02X}{_st9(g):02X}{_st9(b):02X}"
                        for r in range(8) for g in range(8) for b in range(8)]
        # Show 64 evenly-sampled colours (every 8th) for swatch display
        atari_st_full = atari_st_512[::8]

        # Atari STe — 12-bit palette (4R 4G 4B) = 4096 colours, same as Amiga OCS
        # 16 on screen. Scale: val*17
        atari_ste = [f"#{r*17:02X}{g*17:02X}{b*17:02X}"
                     for r in range(16) for g in range(16) for b in range(16)]
        # Show 64 sample colours for swatch
        atari_ste_sample = atari_ste[::64]

        # Atari Falcon — 16-bit (65536 colours), show 64 samples
        atari_falcon = [f"#{r:02X}{g:02X}{b:02X}"
                        for r in range(0,256,36) for g in range(0,256,36) for b in range(0,256,36)][:64]

        # Amiga ECS — Extra Half-Brite: 64 colours (32 + 32 half-brightness)
        # Same 12-bit OCS palette, but 32 extra are half-brightness of first 32
        amiga_ecs = [
            "#000000","#FFFFFF","#888888","#FF8800",
            "#CC0000","#0000CC","#00AAAA","#CC00CC",
            "#00CC00","#EEEE00","#FF6600","#AA4400",
            "#4488FF","#AA00AA","#00BBBB","#AAAAAA",
            "#222222","#444444","#666666","#999999",
            "#BBBBBB","#DDDDDD","#1166CC","#CC4400",
            "#006600","#BB8800","#880000","#004488",
            "#CC8844","#448844","#884488","#44AACC",
            # Half-brightness versions of above (EHB registers 32-63)
            "#000000","#7F7F7F","#444444","#7F4400",
            "#660000","#000066","#005555","#660066",
            "#006600","#777700","#7F3300","#552200",
            "#2244AA","#550055","#005D5D","#555555",
            "#111111","#222222","#333333","#4C4C4C",
            "#5D5D5D","#6E6E6E","#0833BB","#662200",
            "#003300","#5D4400","#440000","#002244",
            "#664422","#224422","#442244","#225566",
        ]

        # Sega Master System — 6-bit (2R 2G 2B), 64 colours
        sega_ms = [f"#{r:02X}{g:02X}{b:02X}"
                   for r in [0,85,170,255] for g in [0,85,170,255] for b in [0,85,170,255]]

        # Sega Mega Drive / Genesis — 9-bit (3R 3G 3B), 512 colours, 64 on screen
        # Scale: 0,36,73,109,146,182,219,255
        _md_v = [0,36,73,109,146,182,219,255]
        sega_md = [f"#{r:02X}{g:02X}{b:02X}"
                   for r in _md_v for g in _md_v for b in _md_v]
        # Show 64 sample colours
        sega_md_sample = sega_md[::8]

        # Sega Game Gear — 12-bit (4R 4G 4B), 4096 colours, 32 on screen
        # Same 12-bit scale as Amiga OCS
        sega_gg = [f"#{r*17:02X}{g*17:02X}{b*17:02X}"
                   for r in range(16) for g in range(16) for b in range(16)]
        sega_gg_sample = sega_gg[::64]  # 64 samples

        # Sega SG-1000 — TMS9918 (same as MSX1)
        sega_sg1000 = [
            "#000000","#010101","#3EB849","#74D07D",
            "#5955E0","#8076F1","#B95E51","#65DBEF",
            "#DB6559","#FF897D","#CCC35E","#DED087",
            "#3AA241","#B766B5","#CCCCCC","#FFFFFF",
        ]

        # SNES — 15-bit (5R 5G 5B), 32768 colours
        # Correct 5-bit scale: val*8 + val//4 gives exact GameBoy/SNES levels
        def _snes5(v): return min(255, v * 8 + v // 4)
        _sv = [_snes5(i) for i in range(32)]
        snes = [f"#{r:02X}{g:02X}{b:02X}" for r in _sv for g in _sv for b in _sv]

        # Game Boy Color / GBA — 15-bit (5R 5G 5B), 32768 colours — same scale as SNES
        game_boy_color = snes   # identical colour space
        game_boy_advance = snes  # identical colour space, different screen size

        # Motorola 6847 — CoCo 1/2, Dragon 32/64, BBC Micro (semi-graphics modes)
        m6847 = [
            "#000000","#00FF00","#FFFF00","#0000FF",
            "#FF0000","#00FFFF","#FF00FF","#FFFFFF",
        ]

        # TRS-80 CoCo 3 GIME — 64 colours (6-bit: 2R 2G 2B)
        coco3 = [f"#{r:02X}{g:02X}{b:02X}"
                 for r in [0,85,170,255] for g in [0,85,170,255] for b in [0,85,170,255]]

        # Dragon 32/64 — identical to CoCo 1/2 (same Motorola 6847)
        dragon = m6847

        # Acorn BBC Micro — 8 colours (ULA chip)
        bbc_micro = [
            "#000000","#FF0000","#00FF00","#FFFF00",
            "#0000FF","#FF00FF","#00FFFF","#FFFFFF",
        ]

        # Acorn Electron — same 8 colours as BBC
        acorn_electron = bbc_micro

        # Acorn Archimedes VIDC — 256 from 16.7M, show 64 samples
        acorn_archimedes = [
            "#000000","#111111","#222222","#333333","#444444","#555555","#666666","#777777",
            "#888888","#999999","#AAAAAA","#BBBBBB","#CCCCCC","#DDDDDD","#EEEEEE","#FFFFFF",
            "#FF0000","#CC0000","#990000","#660000","#330000","#FF3300","#FF6600","#FF9900",
            "#FFCC00","#FFFF00","#CCFF00","#99FF00","#66FF00","#33FF00","#00FF00","#00CC00",
            "#009900","#006600","#003300","#00FF33","#00FF66","#00FF99","#00FFCC","#00FFFF",
            "#00CCFF","#0099FF","#0066FF","#0033FF","#0000FF","#3300FF","#6600FF","#9900FF",
            "#CC00FF","#FF00FF","#FF00CC","#FF0099","#FF0066","#FF0033","#FF8888","#88FF88",
            "#8888FF","#FFFF88","#FF88FF","#88FFFF","#FFAA88","#AAFFAA","#AAAABB","#CCAACC",
        ]

        # MSX2 V9938 — 9-bit (3R 3G 3B), 512 colours
        # Same colour space as Atari ST, Mega Drive, PC Engine — all 9-bit RGB
        msx2_full = atari_st_512

        # PC Engine HuC6260 — 9-bit (3R 3G 3B), 512 colours
        # Same colour space as MSX2/Atari ST/Mega Drive — identical 9-bit RGB encoding
        # Difference is on-screen layout: 16 sprite palettes + 16 BG palettes of 16col each
        pc_engine_full = atari_st_512

        # Sinclair QL — 8 colours
        sinclair_ql = [
            "#000000","#FF0000","#00FF00","#FFFF00",
            "#0000FF","#FF00FF","#00FFFF","#FFFFFF",
        ]

        # ZX80 / ZX81 — black and white only, no colour hardware
        # TV RF output: paper=white, ink=black, inverse video swaps them
        zx80 = ["#FFFFFF","#000000"]
        zx81 = ["#FFFFFF","#000000"]  # identical — same display hardware

        # ZX Spectrum Next — 9-bit palette (3R 3G 3B), 512 colours, 256 active
        # Layer 2: 320×256 or 640×256 at up to 256 colours
        # Same 9-bit colour space as Atari ST, MSX2, Mega Drive
        specnext_9bit = atari_st_512  # same 9-bit scale

        # Timex Sinclair 2068 / TC2048 — adds HiRes 512×192 B&W and column-colour mode
        # Standard mode: same 16 colours as ZX Spectrum
        # Column colour mode: 8 colours per 8-pixel column (not 8×8 cell)
        # HiRes mode: 512×192 monochrome — reuse zx80 2-colour palette
        timex_colour = zx_spectrum   # same 16 ULA colours
        timex_hires  = zx80          # B&W only in 512×192 mode

        # Pentagon / Soviet Spectrum clones
        # Pentagon 128/512/1024, Scorpion ZS, Kay, Profi — all identical ULA display
        # Same 16-colour palette as ZX Spectrum — no display differences
        pentagon = zx_spectrum

        # Jupiter Ace — B&W character mode only (Forth machine)
        jupiter_ace = ["#000000","#FFFFFF"]

        # BBC Micro / Acorn — 8 colours (SAA5050 / 6845 ULA)
        bbc_micro = [
            "#000000","#FF0000","#00FF00","#FFFF00",
            "#0000FF","#FF00FF","#00FFFF","#FFFFFF",
        ]

        # Apple II Lo-Res — 16 colours
        apple2_lores = [
            "#000000","#9D0948","#FF0000","#FF6A00",
            "#00A308","#555555","#2FB702","#E6D217",
            "#7B4900","#D94BD5","#AAAAAA","#FF9BFF",
            "#0D2DFF","#11A9D7","#5DD6FF","#FFFFFF",
        ]

        # Apple II Hi-Res — 6 artefact colours (NTSC)
        apple2_hires = [
            "#000000","#2FB702","#D94BD5",
            "#FFFFFF","#FF6A00","#0D2DFF",
        ]

        # NES/Famicom — 64 entry palette (54 visible, Nestopia NTSC)
        nes = [
            "#626262","#002E98","#0C11C2","#3B008E","#710070","#890024","#7E0000","#591A00",
            "#243400","#004A00","#005200","#004816","#003B5B","#000000","#000000","#000000",
            "#ABABAB","#1266D3","#4040FF","#7B1AFF","#B400CB","#CC0065","#C71220","#9D3200",
            "#5C5200","#177200","#007E00","#007944","#006D99","#000000","#000000","#000000",
            "#FFFFFF","#63B8FF","#8B8BFF","#C065FF","#EE4CFF","#FF4DA6","#FF6257","#FF8229",
            "#E6BA00","#90D300","#42E100","#34DB68","#40CBE3","#494949","#000000","#000000",
            "#FFFFFF","#BEE0FF","#C9C9FF","#E3B7FF","#F7ADFF","#FFADE2","#FFBAB7","#FFCB9D",
            "#F5E58A","#D5F09A","#ADEEAF","#ABEBCC","#A8E5EE","#AEAEAE","#000000","#000000",
        ]

        # Game Boy — 4 shades (original green LCD)
        game_boy = [
            "#0F380F","#306230","#8BAC0F","#9BBC0F",
        ]

        # Game Boy Pocket — 4 shades (grey LCD)
        game_boy_pocket = [
            "#000000","#555555","#AAAAAA","#FFFFFF",
        ]

        # Mega Drive / Genesis — 64 on-screen from 512 (Sega 9-bit palette, common defaults)
        mega_drive = [
            "#000000","#AAAAAA","#FFFFFF","#550000","#FF0000","#FF5500","#AA5500","#FFAA00",
            "#FFFF00","#005500","#00AA00","#00FF00","#005555","#00AAAA","#00FFFF","#000055",
            "#0000AA","#0000FF","#550055","#AA00AA","#FF00FF","#FF00AA","#AA0055","#FF5555",
            "#FFAAAA","#55FF55","#AAFFAA","#55FFFF","#AAFFFF","#5555FF","#AAAAFF","#FF55FF",
            "#555500","#888800","#AAAA00","#CCCC00","#005500","#008800","#00AA00","#00CC00",
            "#000055","#000088","#0000AA","#0000CC","#550000","#880000","#AA0000","#CC0000",
            "#333333","#555555","#777777","#999999","#BBBBBB","#DDDDDD","#222222","#444444",
            "#666666","#888888","#AAAAAA","#CCCCCC","#EEEEEE","#FF8800","#FF4400","#FF0044",
        ]

        # SAM Coupé — 128 colours (16 hues × 8 brightness)
        sam_coupe = [
            "#000000","#000099","#0000CC","#0000FF",
            "#990000","#990099","#9900CC","#9900FF",
            "#CC0000","#CC0099","#CC00CC","#CC00FF",
            "#FF0000","#FF0099","#FF00CC","#FF00FF",
            "#009900","#009999","#0099CC","#0099FF",
            "#999900","#999999","#9999CC","#9999FF",
            "#CC9900","#CC9999","#CC99CC","#CC99FF",
            "#FF9900","#FF9999","#FF99CC","#FF99FF",
            "#00CC00","#00CC99","#00CCCC","#00CCFF",
            "#99CC00","#99CC99","#99CCCC","#99CCFF",
            "#CCCC00","#CCCC99","#CCCCCC","#CCCCFF",
            "#FFCC00","#FFCC99","#FFCCCC","#FFCCFF",
            "#00FF00","#00FF99","#00FFCC","#00FFFF",
            "#99FF00","#99FF99","#99FFCC","#99FFFF",
            "#CCFF00","#CCFF99","#CCFFCC","#CCFFFF",
            "#FFFF00","#FFFF99","#FFFFCC","#FFFFFF",
            # Darker variants (half brightness)
            "#000000","#00004C","#000066","#000080",
            "#4C0000","#4C004C","#4C0066","#4C0080",
            "#660000","#66004C","#660066","#660080",
            "#800000","#80004C","#800066","#800080",
            "#004C00","#004C4C","#004C66","#004C80",
            "#4C4C00","#4C4C4C","#4C4C66","#4C4C80",
            "#664C00","#664C4C","#664C66","#664C80",
            "#804C00","#804C4C","#804C66","#804C80",
            "#006600","#00664C","#006666","#006680",
            "#4C6600","#4C664C","#4C6666","#4C6680",
            "#666600","#66664C","#666666","#666680",
            "#806600","#80664C","#806666","#806680",
            "#008000","#00804C","#008066","#008080",
            "#4C8000","#4C804C","#4C8066","#4C8080",
            "#668000","#66804C","#668066","#668080",
            "#808000","#80804C","#808066","#808080",
        ]

        # MSX2 and PC Engine use msx2_full / pc_engine_full (full 9-bit 512-colour palettes)

        # Commodore 128 (same C64 colours but noting different VDC chip for 80-col)
        # VDC 80-col mode has 16 colours same as C64 for compatibility

        # Atari Lynx — 16 colours from 4096 (12-bit), default palette
        atari_lynx = [
            "#000000","#FFFFFF","#FF0000","#00FF00",
            "#0000FF","#FFFF00","#FF00FF","#00FFFF",
            "#FF8800","#0088FF","#88FF00","#FF0088",
            "#8800FF","#00FF88","#888888","#444444",
        ]

        # Atari Jaguar — 24-bit true colour, show 16 representative
        atari_jaguar = [
            "#000000","#FFFFFF","#FF0000","#00FF00",
            "#0000FF","#FFFF00","#FF00FF","#00FFFF",
            "#FF8800","#8800FF","#00FF88","#FF0088",
            "#888888","#444444","#CCCCCC","#FF4444",
        ]

        # Atari 2600 PAL — different hues from NTSC
        atari_2600_pal = [
            "#000000","#404040","#6c6c6c","#909090","#b0b0b0","#c8c8c8","#dcdcdc","#ececec",
            "#1a1a00","#3a3a00","#5c5c00","#7a7a10","#989820","#b0b030","#c8c844","#dcdce0",
            "#001800","#003000","#004800","#106010","#207820","#389038","#50a850","#68c068",
            "#000018","#000038","#100058","#280070","#440088","#5c00a0","#7400b8","#8c00d0",
            "#180000","#380000","#580000","#780000","#980010","#b82020","#d84040","#f06060",
            "#000014","#000034","#000054","#100074","#300094","#5000b4","#7020d4","#9040f4",
            "#181800","#383800","#585800","#787820","#989840","#b8b860","#d8d880","#f8f8a0",
            "#001414","#003434","#005454","#107474","#209494","#38b4b4","#50d4d4","#68f4f4",
        ]

        # Amstrad PCW — 2 colours (green phosphor)
        amstrad_pcw = ["#1a3300","#33ff00"]  # dark bg, bright green

        # Amstrad CPC+ / GX4000 — 4096 colours (12-bit, same as STe)
        # Same palette space as atari_ste
        amstrad_cpc_plus = atari_ste  # identical 12-bit colour space

        # Amstrad NC100/NC200 — 4 shades (grey LCD)
        amstrad_nc = ["#000000","#555555","#AAAAAA","#FFFFFF"]

        # RM Nimbus (Research Machines) — 16 colours (PC-compatible EGA-like)
        # Nimbus had its own unusual colour palette in low-res mode
        rm_nimbus = [
            "#000000","#0000AA","#00AA00","#00AAAA",
            "#AA0000","#AA00AA","#AA5500","#AAAAAA",
            "#555555","#5555FF","#55FF55","#55FFFF",
            "#FF5555","#FF55FF","#FFFF55","#FFFFFF",
        ]

        # Atari 5200 / 7800 — same GTIA chip as Atari 800
        # (reuse atari_800 palette)

        # Registry: name -> (hex_list, cols)
        self.retro_palettes = {
            #    Amiga                                                     
            "Amiga OCS":          (amiga_ocs,           8),  # 32col 12-bit 4096
            "Amiga ECS":          (amiga_ecs,           8),  # 64col EHB mode
            "Amiga AGA":          (amiga_aga,          16),  # 256col 24-bit
            "Amiga AGA WB":       (amiga_aga_wb,       16),  # AGA Workbench
            #    Commodore                                                 
            "C64":                (commodore_64,         8),  # 16col VIC-II
            "VIC-20":             (vic20,                8),  # 16col VIC-I
            "Plus/4":             (plus4,                8),  # 16col TED
            #    Sinclair / ZX                                             
            "ZX Spectrum":        (zx_spectrum,          8),  # 16col ULA
            "ZX Spectrum 128K":   (zx_spectrum,          8),  # same display
            "ZX80":               (zx80,                 2),  # B&W
            "ZX81":               (zx81,                 2),  # B&W
            "ULA Plus":           (ula_plus,            16),  # 256col
            "ZX Spectrum Next":   (specnext_9bit,       16),  # 512col 9-bit
            "Timex TS2068":       (timex_colour,         8),  # 16col standard mode
            "Timex HiRes":        (timex_hires,          2),  # B&W 512×192 mode
            "Pentagon":           (pentagon,             8),  # 16col — same as Spectrum
            "Jupiter Ace":        (jupiter_ace,          2),  # B&W char mode
            "Sinclair QL":        (sinclair_ql,          8),  # 8col
            #    Atari                                                     
            "Atari 2600 NTSC":    (atari_2600,           8),  # 128col TIA
            "Atari 2600 PAL":     (atari_2600_pal,        8),  # 128col PAL
            "Atari 800 GTIA":     (atari_800,            16),  # 256col GTIA
            "Atari 5200":         (atari_800,            16),  # same GTIA chip
            "Atari 7800":         (atari_800,            16),  # same GTIA chip
            "Atari Lynx":         (atari_lynx,            8),  # 16col 12-bit
            "Atari ST":           (atari_st_512,         16),  # 512col (9-bit)
            "Atari STe":          (atari_ste,            16),  # 4096col (12-bit)
            "Atari Falcon":       (atari_falcon,          8),  # 64 of 65536 (16-bit)
            "Atari Jaguar":       (atari_jaguar,          8),  # 24-bit sample
            #    Amstrad                                                   
            "Amstrad CPC":        (amstrad_cpc,           9),  # 27col hardware
            "Amstrad CPC+":       (amstrad_cpc_plus,     16),  # 4096col 12-bit
            "Amstrad PCW":        (amstrad_pcw,           2),  # 2col green phosphor
            "Amstrad NC100/200":  (amstrad_nc,            4),  # 4 shades grey
            #    Acorn                                                     
            "BBC Micro":          (bbc_micro,            8),  # 8col 6845 ULA
            "Acorn Electron":     (acorn_electron,       8),  # 8col
            "Acorn Archimedes":   (acorn_archimedes,     8),  # 64 of 16.7M VIDC
            #    Tandy / Dragon                                            
            "CoCo 1/2":           (m6847,                8),  # 8col 6847
            "CoCo 3":             (coco3,                8),  # 64col GIME
            "Dragon 32/64":       (dragon,               8),  # 8col 6847
            #    MSX                                                       
            "MSX1":               (msx1,                 8),  # 16col TMS9918
            "MSX2":               (msx2_full,            16),  # 512col full V9938
            #    NES / Nintendo                                            
            "NES":                (nes,                  8),  # 64col PPU
            "SNES":               (snes,               128),  # 32768col full (15-bit)
            #    Game Boy                                                  
            "Game Boy":           (game_boy,             4),  # 4 shades green
            "Game Boy Pocket":    (game_boy_pocket,      4),  # 4 shades grey
            "Game Boy Color":     (game_boy_color,     128),  # 32768col full (15-bit)
            "Game Boy Advance":   (game_boy_advance,   128),  # 32768col full (15-bit)
            #    Sega                                                      
            "Sega SG-1000":       (sega_sg1000,          8),  # 16col TMS9918
            "Sega Master System": (sega_ms,              8),  # 64col 6-bit
            "Sega Mega Drive":    (sega_md,              16),  # 512col full (9-bit)
            "Sega Game Gear":     (sega_gg,              16),  # 4096col full (12-bit)
            #    PC Engine / TurboGrafx                                    
            "PC Engine":          (pc_engine_full,       16),  # 512col full (9-bit)
            #    SAM Coupé                                                 
            "SAM Coupé":          (sam_coupe,           16),  # 128col
            #    Apple                                                     
            "Apple II Lo-Res":    (apple2_lores,         8),  # 16col
            "Apple II Hi-Res":    (apple2_hires,         6),  # 6 artefact col
            #    RM Nimbus                                                  
            "RM Nimbus":          (rm_nimbus,             8),  # 16col EGA-like
        }
        self.current_retro_palette = "Amiga OCS"

    def _get_retro_colors(self, name: str) -> Tuple[List[QColor], int]:
        """Return (colors, cols) for named retro palette."""
        data, cols = self.retro_palettes.get(
            name, self.retro_palettes["Amiga OCS"])
        colors = [QColor(h) for h in data]
        return colors, cols

    def _apply_retro_palette(self, name: str): #vers 4
        """Load a retro preset into the user palette grid."""
        self.current_retro_palette = name
        colors, cols = self._get_retro_colors(name)
        if hasattr(self, '_user_pal_grid'):
            # _AutoCellPaletteGrid: pass cols so it knows how many columns to use;
            # cell size is computed automatically from widget width
            if hasattr(self._user_pal_grid, '_fixed_cols'):
                self._user_pal_grid._fixed_cols = max(1, cols)
            self._user_pal_grid.set_colors(colors, cols)
            self._user_pal_grid._recalc_height()
            self._user_pal_grid.update()
        if hasattr(self, '_retro_btn'):
            self._retro_btn.setText(f"{name}")
        # Show dither button only for small palettes
        if hasattr(self, '_pal_dither_btn'):
            self._pal_dither_btn.setVisible(len(colors) < 16)

    def _cycle_pal_dither(self): #vers 1
        """Cycle palette dither mode: off → floyd → bayer → checker → off."""
        cycle = {'off':'floyd','floyd':'bayer','bayer':'checker','checker':'off'}
        self._pal_dither_mode = cycle.get(self._pal_dither_mode, 'off')
        labels = {'off':'Dith','floyd':'F-S','bayer':'Bayr','checker':'Chkr'}
        tips = {
            'off':     'Dither: Off — hard snap to palette',
            'floyd':   'Dither: Floyd-Steinberg error diffusion',
            'bayer':   'Dither: Bayer 4×4 ordered',
            'checker': 'Dither: Checkerboard FG/BG',
        }
        if hasattr(self, '_pal_dither_btn'):
            self._pal_dither_btn.setText(labels[self._pal_dither_mode])
            self._pal_dither_btn.setChecked(self._pal_dither_mode != 'off')
            self._pal_dither_btn.setToolTip(tips[self._pal_dither_mode])
        self._set_status(f"Palette dither: {self._pal_dither_mode}")

    def _apply_user_palette_dither(self, img): #vers 1
        """Apply current palette dither mode when snapping to user palette."""
        from PIL import Image
        palette = self._get_user_palette_rgb()
        if not palette: return img
        n = min(len(palette), 256)
        mode = getattr(self, '_pal_dither_mode', 'off')

        if mode == 'floyd':
            # Floyd-Steinberg via PIL quantize dither=1
            pal_img = Image.new('P', (1,1))
            flat = []
            for r,g,b in palette[:n]: flat += [r,g,b]
            flat += [0]*(768-len(flat))
            pal_img.putpalette(flat)
            return img.convert('RGB').quantize(palette=pal_img, dither=1).convert('RGB').convert('RGBA')

        elif mode == 'bayer':
            BAYER = [[0,8,2,10],[12,4,14,6],[3,11,1,9],[15,7,13,5]]
            rgb = img.convert('RGB')
            w, h = rgb.size
            out = Image.new('RGB', (w,h))
            px_in = rgb.load(); px_out = out.load()
            for y in range(h):
                for x in range(w):
                    r,g,b = px_in[x,y]
                    t = BAYER[y%4][x%4]/16.0
                    # Shift pixel by threshold then snap
                    sr = min(255,int(r+(t-0.5)*48))
                    sg = min(255,int(g+(t-0.5)*48))
                    sb = min(255,int(b+(t-0.5)*48))
                    best = min(palette[:n],
                               key=lambda c:(c[0]-sr)**2+(c[1]-sg)**2+(c[2]-sb)**2)
                    px_out[x,y] = best
            return out.convert('RGBA')

        elif mode == 'checker':
            # Checkerboard — alternate between nearest and second-nearest
            rgb = img.convert('RGB')
            w, h = rgb.size
            out = Image.new('RGB', (w,h))
            px_in = rgb.load(); px_out = out.load()
            for y in range(h):
                for x in range(w):
                    r,g,b = px_in[x,y]
                    dists = sorted(palette[:n],
                                   key=lambda c:(c[0]-r)**2+(c[1]-g)**2+(c[2]-b)**2)
                    px_out[x,y] = dists[0] if (x+y)%2==0 else (dists[1] if len(dists)>1 else dists[0])
            return out.convert('RGBA')

        else:
            # No dither — hard snap
            return self._snap_image_to_user_palette(img)

    def _show_retro_menu(self): #vers 1
        """Show user palette picker as hierarchical platform submenus."""
        menu = QMenu(self)

        GROUPS = [
            ("Amiga", [
                "Amiga OCS", "Amiga ECS", "Amiga AGA", "Amiga AGA WB",
            ]),
            ("Commodore", [
                "C64", "VIC-20", "Plus/4",
            ]),
            ("Sinclair / ZX", [
                "ZX Spectrum", "ZX Spectrum 128K",
                "ZX80", "ZX81", "ULA Plus",
                "ZX Spectrum Next",
                "Timex TS2068", "Timex HiRes",
                "Pentagon", "Jupiter Ace",
                "Sinclair QL",
            ]),
            ("Atari", [
                "Atari 2600 NTSC", "Atari 2600 PAL",
                "Atari 800 GTIA", "Atari 5200", "Atari 7800",
                "Atari ST", "Atari STe", "Atari Falcon",
                "Atari Lynx", "Atari Jaguar",
            ]),
            ("Amstrad", [
                "Amstrad CPC", "Amstrad CPC+",
                "Amstrad PCW", "Amstrad NC100/200",
            ]),
            ("Acorn", [
                "BBC Micro", "Acorn Electron", "Acorn Archimedes",
            ]),
            ("Tandy / Dragon", [
                "CoCo 1/2", "CoCo 3", "Dragon 32/64",
            ]),
            ("MSX", [
                "MSX1", "MSX2",
            ]),
            ("Nintendo", [
                "NES", "SNES",
                "Game Boy", "Game Boy Pocket", "Game Boy Color", "Game Boy Advance",
            ]),
            ("Sega", [
                "Sega SG-1000", "Sega Master System",
                "Sega Mega Drive", "Sega Game Gear",
            ]),
            ("NEC / Hudson", [
                "PC Engine",
            ]),
            ("Other", [
                "SAM Coupé", "Apple II Lo-Res", "Apple II Hi-Res",
                "RM Nimbus",
            ]),
        ]

        for group_name, palette_names in GROUPS:
            sub = menu.addMenu(group_name)
            for name in palette_names:
                if name in self.retro_palettes:
                    act = sub.addAction(name)
                    act.triggered.connect(lambda _, n=name: self._apply_retro_palette(n))

        if hasattr(self, '_retro_btn'):
            menu.exec(self._retro_btn.mapToGlobal(
                self._retro_btn.rect().bottomLeft()))



#  DP5Workshop — main container (DPaint5-faithful layout)




class _CornerOverlay(QWidget):
    """Transparent overlay that draws corner resize triangles on top of all children.
    Uses setMask() so only the triangle pixels exist — fully transparent elsewhere.
    WA_AlwaysStackOnTop keeps it above all sibling widgets on Wayland/KDE."""

    SIZE = 20   # triangle leg size in pixels

    def __init__(self, parent): #vers 3
        super().__init__(parent)
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, True)
        self.setAttribute(Qt.WidgetAttribute.WA_NoSystemBackground, True)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground, True)
        self.setAttribute(Qt.WidgetAttribute.WA_AlwaysStackOnTop, True)
        self.setWindowFlags(Qt.WindowType.Widget)
        self._hover_corner = None
        self._app_settings = None
        self.setGeometry(0, 0, parent.width(), parent.height())
        self._update_mask()

    def _update_mask(self): #vers 1
        """Create a mask covering only the four corner triangles."""
        from PyQt6.QtGui import QRegion, QPolygon
        from PyQt6.QtCore import QPoint
        s = self.SIZE
        w, h = self.width(), self.height()
        region = QRegion()
        for pts in [
            [QPoint(0,0),    QPoint(s,0),    QPoint(0,s)],     # top-left
            [QPoint(w,0),    QPoint(w-s,0),  QPoint(w,s)],     # top-right
            [QPoint(0,h),    QPoint(s,h),    QPoint(0,h-s)],   # bottom-left
            [QPoint(w,h),    QPoint(w-s,h),  QPoint(w,h-s)],   # bottom-right
        ]:
            region = region.united(QRegion(QPolygon(pts)))
        self.setMask(region)

    def update_state(self, hover_corner, app_settings): #vers 1
        self._hover_corner = hover_corner
        self._app_settings = app_settings
        self.update()

    def setGeometry(self, *args): #vers 1
        super().setGeometry(*args)
        self._update_mask()

    def resizeEvent(self, event): #vers 1
        super().resizeEvent(event)
        self._update_mask()

    def paintEvent(self, event): #vers 2
        s = self.SIZE
        _p = self.palette()
        _accent_fallback = _p.color(_p.ColorRole.Highlight)
        if self._app_settings:
            try:
                colors = self._app_settings.get_theme_colors()
                accent = QColor(colors.get('accent_primary', _accent_fallback.name()))
            except Exception:
                accent = _accent_fallback
        else:
            accent = _accent_fallback
        accent.setAlpha(200)
        hover_c = QColor(accent); hover_c.setAlpha(255)
        w, h = self.width(), self.height()
        corners = {
            'top-left':     [(0,0),  (s,0),   (0,s)],
            'top-right':    [(w,0),  (w-s,0), (w,s)],
            'bottom-left':  [(0,h),  (s,h),   (0,h-s)],
            'bottom-right': [(w,h),  (w-s,h), (w,h-s)],
        }
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        for name, pts in corners.items():
            path = QPainterPath()
            path.moveTo(*pts[0]); path.lineTo(*pts[1]); path.lineTo(*pts[2])
            path.closeSubpath()
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(hover_c if self._hover_corner == name else accent))
            painter.drawPath(path)
        painter.end()


class DP5Workshop(ColorPalPresetsMixin, _ToolMenuMixin, QWidget):
    """Deluxe Paint 5 inspired bitmap editor — standalone + embeddable."""

    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    #    Init                                                                   

    def __init__(self, parent=None, main_window=None): #vers 1
        super().__init__(parent)

        self.main_window     = main_window
        self.standalone_mode = (main_window is None)
        self.is_docked       = not self.standalone_mode

        # DP5-specific settings (JSON, separate from global theme)
        self.dp5_settings = DP5Settings()

        # Fonts — size from settings
        fs = self.dp5_settings.get('ui_font_size')
        self.title_font  = QFont("Arial", fs + 4)
        self.panel_font  = QFont("Arial", fs)
        self.button_font = QFont("Arial", fs)
        self.fonthsize   = max(7, fs - 1)

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
        self._canvas_bit_depth = 8  # 0=32bit, 1=24bit, 2=16bit, 3=8bit
        self._dither_mode   = 'off'  # 'off' | 'checker' | 'bayer' | 'floyd'
        self._symmetry_mode = 'off'  # cycles: off → H → V → quad
        self._platform_mode = 'none'
        self._enforce_constraints = False
        self._pal_dither_mode = 'off'
        self._show_clash_overlay = False   # 'off'|'floyd'|'bayer'|'checker'
        self._canvas_mode   = 'free'
        self._mode_locked   = False
        self._mode_btns     = {}
        # Animation
        self._frames        = []    # list of bytearray (rgba per frame)
        self._frame_delays  = []    # ms delay per frame
        self._current_frame = 0
        self._anim_playing  = False
        self._anim_timer    = None
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

    #    UI construction                                                        

    def setup_ui(self): #vers 1
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(1, 1, 1, 1)
        main_layout.setSpacing(0)   # no gaps — each widget manages its own margin

        toolbar = self._create_toolbar()
        self._workshop_toolbar = toolbar
        if self.standalone_mode:
            # Standalone: toolbar is the titlebar/drag handle — always visible
            main_layout.addWidget(toolbar)
            toolbar.setVisible(True)
        # Docked: don't add toolbar to layout at all — avoids any ghost height

        # Internal QMenuBar — built always so menus exist for both modes.
        # Standalone: shown as topbar or hidden per settings.
        # Docked: hidden — a single dropdown button provides access instead.
        from PyQt6.QtWidgets import QHBoxLayout as _QHL
        self._menu_bar_container = QWidget(self)
        self._menu_bar_container.setObjectName("dp5_menu_bar_container")
        _chl = _QHL(self._menu_bar_container)
        _chl.setContentsMargins(0, 0, 0, 0)
        _chl.setSpacing(0)

        mb = QMenuBar(self._menu_bar_container)
        self._menu_bar = mb
        self._build_canvas_menus(mb)
        self._apply_menu_bar_style()
        _chl.addWidget(mb)

        if self.standalone_mode:
            show_mb = (self.dp5_settings.get('show_menubar', False) and
                       self.dp5_settings.get('menu_style', 'dropdown') == 'topbar')
            if show_mb:
                self._menu_bar_container.setMinimumHeight(0)
                self._menu_bar_container.setMaximumHeight(16777215)
                self._menu_bar_container.setVisible(True)
            else:
                self._menu_bar_container.setVisible(False)
                self._menu_bar_container.setFixedHeight(0)
            main_layout.addWidget(self._menu_bar_container)
        else:
            # Docked: hide the QMenuBar container entirely.
            # A slim dropdown button row gives full menu access instead.
            self._menu_bar_container.setVisible(False)
            self._menu_bar_container.setFixedHeight(0)
            # Don't add _menu_bar_container to layout — keeps zero height.

            # Titlebar [DP5] button is the menu entry point when docked
            self._register_titlebar_tool_btn()

        self._splitter = QSplitter(Qt.Orientation.Horizontal)
        self._splitter.splitterMoved.connect(self._on_splitter_moved)

        self._left_panel  = self._create_left_panel()
        centre            = self._create_centre_panel()
        right             = self._create_right_panel()

        self._splitter.addWidget(self._left_panel)
        self._splitter.addWidget(centre)
        self._splitter.addWidget(right)
        self._splitter.setStretchFactor(0, 0)   # bitmap list
        self._splitter.setStretchFactor(1, 1)   # canvas — stretches
        self._splitter.setStretchFactor(2, 0)   # tools / palette
        self._splitter.setCollapsible(2, False)
        right.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Expanding)
        # No setMaximumWidth — splitter controls the width freely

        # Restore saved splitter sizes (if any)
        from PyQt6.QtCore import QTimer
        _saved_sizes = self.dp5_settings.get('splitter_sizes')
        if _saved_sizes and len(_saved_sizes) == self._splitter.count():
            QTimer.singleShot(0, lambda s=list(_saved_sizes): self._splitter.setSizes(s))

        # Left panel: hidden by default — toggle via DP5 Settings
        self._left_panel.setVisible(self.dp5_settings.get('show_bitmap_list'))

        main_layout.addWidget(self._splitter)

        self._set_status(f"Canvas: {self._canvas_width}×{self._canvas_height}")

        # Initial tool
        QTimer.singleShot(0, lambda: self._select_tool(TOOL_PENCIL))

        # Corner resize overlay
        if self.standalone_mode:
            QTimer.singleShot(0, self._setup_corner_overlay)

    # - Toolbar (standalone titlebar)


    def _create_toolbar(self): #vers 2
        # Read sizes from app_settings so they match Global App System Settings
        try:
            from apps.utils.app_settings_system import get_titlebar_sizes as _gts
            _as = getattr(self, 'app_settings', None) or getattr(
                  getattr(self, 'main_window', None), 'app_settings', None)
            _sz = _gts(_as)
            _TB_H    = _sz['tb_height']
            _BTN_SZ  = _sz['btn_size']
            _ICO_SZ  = _sz['icon_size']
            _BTN_H   = _sz['btn_height']
        except Exception:
            _TB_H, _BTN_SZ, _ICO_SZ, _BTN_H = 32, 32, 20, 24
        # titlebar and toolbar are the SAME widget — avoids a floating 45px ghost
        # that was rendering at (0,0) and creating blank space above the canvas.
        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.NoFrame)
        self.toolbar.setMaximumHeight(_TB_H + 10)
        self.toolbar.setObjectName("titlebar")
        self.toolbar.installEventFilter(self)
        self.toolbar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.toolbar.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        self.toolbar.setMouseTracking(True)
        self.titlebar = self.toolbar   # alias — drag detection uses self.titlebar
        # gadgetbar_bg applied via QFrame#titlebar rule in global stylesheet

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(4)

        icon_color = self._get_icon_color()

        # -_tb helper — adds button to layout with optional SVG icon
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

        # - Cog / Settings (standalone only)
        self.menu_toggle_btn = QPushButton("Menu")
        self.menu_toggle_btn.setFont(self.button_font)
        self.menu_toggle_btn.setToolTip("Show menu (topbar or dropdown — set in Settings)")
        self.menu_toggle_btn.setMinimumHeight(28)
        self.menu_toggle_btn.setMaximumHeight(28)
        self.menu_toggle_btn.clicked.connect(self._on_menu_btn_clicked)
        self.menu_toggle_btn.setVisible(self.standalone_mode)
        layout.addWidget(self.menu_toggle_btn)

        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(SVGIconFactory.settings_icon(20, icon_color))
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(_ICO_SZ, _ICO_SZ))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip(App_name + " Settings")
        self.settings_btn.setVisible(self.standalone_mode)
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        # - Title with DP5 icon
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

        # - [New][Load][Save][Undo][Clear][Brushes] after title
        self.tb_new_btn    = _tb("New",    "New canvas… (right-click to set mode)",
                                  self._new_canvas,
                                  SVGIconFactory.new_icon)
        self.tb_new_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.tb_new_btn.customContextMenuRequested.connect(self._new_btn_context_menu)
        self.tb_load_btn = _tb("Load", "Click for load options",
                                self._show_load_menu,
                                SVGIconFactory.open_icon)
        self.tb_load_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.tb_load_btn.customContextMenuRequested.connect(self._show_load_menu_at)
        self.tb_save_btn   = _tb("Save",   "Save canvas as PNG",
                                  self._export_bitmap,
                                  SVGIconFactory.save_icon)
        # self.tb_import_btn = _tb("Import", "Import image (IFF, BMP, older formats)",
        #                           self._import_bitmap,
        #                           SVGIconFactory.import_icon)
        # self.tb_export_btn = _tb("Export", "Export canvas (IFF, BMP, older formats)",
        #                           self._export_bitmap,
        #                           SVGIconFactory.export_icon)
        self.tb_undo_btn   = _tb("Undo",   "Undo last action  (Ctrl+Z)",
                                  self._undo_canvas,
                                  SVGIconFactory.undo_icon)
        try:
            from apps.methods.imgfactory_svg_icons import get_clear_canvas_icon
            self.tb_clr_btn = _tb("Clear", "Clear canvas",
                                   self._clear_canvas,
                                   lambda sz, col: get_clear_canvas_icon(sz, col))
        except Exception:
            self.tb_clr_btn = _tb("Clear", "Clear canvas", self._clear_canvas)

        #    Brush Manager button                                            
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

    #    Docked compact action bar                                             

    def _create_docked_bar(self): #vers 1
        """Compact New/Load/Save/Undo/Clear bar shown only when docked.
        Gives access to essential file ops that the hidden standalone toolbar
        and suppressed menubar would otherwise block.
        """
        from PyQt6.QtWidgets import QHBoxLayout, QSizePolicy
        from PyQt6.QtCore import QSize
        from apps.methods.imgfactory_svg_icons import SVGIconFactory

        bar = QWidget()
        bar.setFixedHeight(28)
        bar.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        hl  = QHBoxLayout(bar)
        hl.setContentsMargins(2, 1, 2, 1)
        hl.setSpacing(2)

        icon_color = '#cccccc'
        if self.app_settings:
            try:
                colors = self.app_settings.get_theme_colors()
                icon_color = colors.get('text_primary', '#cccccc')
            except Exception:
                pass

        def _btn(label, tip, slot, icon_fn):
            b = QPushButton(label)
            b.setFixedHeight(24)
            b.setToolTip(tip)
            try:
                b.setIcon(icon_fn(14, icon_color))
                b.setIconSize(QSize(14, 14))
            except Exception:
                pass
            b.clicked.connect(slot)
            return b

        hl.addWidget(_btn("New",   "New canvas",          self._new_canvas,     SVGIconFactory.new_icon))
        hl.addWidget(_btn("Load",  "Load image",          self._show_load_menu, SVGIconFactory.open_icon))
        hl.addWidget(_btn("Save",  "Save / export",       self._export_bitmap,  SVGIconFactory.save_icon))
        hl.addWidget(_btn("Undo",  "Undo  (Ctrl+Z)",      self._undo_canvas,    SVGIconFactory.undo_icon))
        hl.addWidget(_btn("Clear", "Clear canvas",        self._clear_canvas,   SVGIconFactory.new_icon))
        hl.addStretch()

        # IFF round-trip save (native DP5 format)
        iff_btn = _btn("IFF", "Save as IFF ILBM (native DP5)", self._export_iff, SVGIconFactory.save_icon)
        hl.addWidget(iff_btn)

        return bar

        #    Left panel: bitmap list                                                

    def _create_left_panel(self): #vers 1
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

    def _on_bitmap_selected(self, row: int): #vers 1
        if 0 <= row < len(self._bitmap_list):
            self._current_bitmap = row
            bm = self._bitmap_list[row]
            if self.dp5_canvas:
                self.dp5_canvas.tex_w = bm['w']
                self.dp5_canvas.tex_h = bm['h']
                self.dp5_canvas.rgba  = bm['rgba']
                self.dp5_canvas.update()
                self._set_status(f"{bm['name']}  {bm['w']}×{bm['h']}")

    def _delete_bitmap(self): #vers 1
        row = self._bitmap_lw.currentRow()
        if 0 <= row < len(self._bitmap_list):
            self._bitmap_list.pop(row)
            self._bitmap_lw.takeItem(row)
            if self._bitmap_list:
                self._bitmap_lw.setCurrentRow(
                    min(row, len(self._bitmap_list)-1))

    #    Centre panel: canvas                                                   

    def _create_centre_panel(self): #vers 2
        panel = QWidget()
        layout = QVBoxLayout(panel)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Canvas
        try:
            w, h = self._canvas_width, self._canvas_height
            self.canvas_rgba = bytearray(b'\x80\x80\x80\xff' * (w * h))
            self.dp5_canvas  = DP5Canvas(w, h, self.canvas_rgba, panel)
            self.dp5_canvas._editor = self
            self.dp5_canvas.pixel_changed.connect(self._on_canvas_changed)
            self.dp5_canvas.show_grid  = self.dp5_settings.get('show_pixel_grid')
            self.dp5_canvas.grid_color = QColor(self.dp5_settings.get('grid_color'))
            scroll = QScrollArea()
            scroll.setWidget(self.dp5_canvas)
            scroll.setWidgetResizable(False)
            scroll.setAlignment(Qt.AlignmentFlag.AlignCenter)
            self._canvas_scroll = scroll
            layout.addWidget(scroll, 1)
        except Exception as e:
            err = QLabel(f"Canvas error: {e}")
            layout.addWidget(err)
            self.dp5_canvas = None

        # Animation timeline strip
        self._anim_strip = self._create_anim_strip()
        self._anim_strip.setVisible(self.dp5_settings.get('show_anim_strip', False))
        layout.addWidget(self._anim_strip)

        # Status bar — canvas-wide only, 22px fixed height
        self._status_bar = QStatusBar()
        self._status_bar.setSizeGripEnabled(False)
        self._status_bar.setFixedHeight(22)
        self._status_bar.setVisible(self.dp5_settings.get('show_statusbar'))
        # Permanent right-side info labels
        self._status_size_lbl  = QLabel("320×256")
        self._status_depth_lbl = QLabel("RGBA32")
        self._status_size_lbl.setStyleSheet("padding: 0 6px; color: palette(mid);")
        self._status_depth_lbl.setStyleSheet("padding: 0 6px; color: palette(mid);")
        self._status_bar.addPermanentWidget(self._status_depth_lbl)
        self._status_bar.addPermanentWidget(self._status_size_lbl)
        layout.addWidget(self._status_bar)

        return panel

    def _build_canvas_menus(self, mb): #vers 2
        """Populate a QMenuBar (topbar) or QMenu (docked/dropdown) with all DP5 menus.
        Both share the same addMenu()/addAction() API so one method serves all cases."""
        # File
        fm = mb.addMenu("File")
        fm.addAction("New canvas…",    self._new_canvas)
        fm.addSeparator()
        fm.addAction("Open image…",                           self._import_bitmap)
        fm.addAction("Snap to pal…",                          self._import_bitmap_snap_user_pal)
        fm.addAction("Snap to pal (dither)…",                 self._import_bitmap_snap_dither)
        fm.addAction("Snap to pal, canvas size…",             self._import_bitmap_snap_canvas_size)
        fm.addAction("Snap to pal, canvas size (dither)…",    self._import_bitmap_snap_canvas_size_dither)
        # Import submenu — all supported formats
        fim = fm.addMenu("Import")
        fim.addAction("PNG / BMP / JPEG / WebP…",  self._import_bitmap)
        fim.addAction("TIFF…",                      self._import_tiff)
        fim.addAction("GIF (incl. animated)…",      self._import_gif)
        fim.addAction("TGA (Targa)…",               self._import_tga)
        fim.addAction("PCX…",                       self._import_pcx)
        fim.addAction("DDS (DirectDraw Surface)…",  self._import_dds)
        fim.addAction("PSD (Photoshop read)…",      self._import_psd)
        fim.addAction("IFF ILBM (Amiga)…",          self._import_iff)
        amiga_im = fim.addMenu("Amiga .info Icon")
        amiga_im.addAction("WB 3.9 AGA (256col) — default…",
            lambda: self._import_amiga_info('wb39'))
        amiga_im.addAction("WB 3.9 XL AGA (256col)…",
            lambda: self._import_amiga_info('wb39xl'))
        amiga_im.addAction("AGA standard (16col)…",
            lambda: self._import_amiga_info('aga'))
        amiga_im.addAction("MagicWB (8col)…",
            lambda: self._import_amiga_info('magicwb'))
        amiga_im.addAction("OCS/ECS WB3 (4col)…",
            lambda: self._import_amiga_info('ocs'))
        amiga_im.addAction("User palette…",
            lambda: self._import_amiga_info('user'))
        fim.addSeparator()
        fim.addAction("Apple ICNS…",                self._import_icns)
        fim.addAction("Windows ICO…",               self._import_ico)
        fim.addAction("SVG…",                       self._import_svg)
        fm.addSeparator()
        fm.addAction("Save as PNG…",   self._export_bitmap)
        fm.addAction("Export IFF…",    self._export_iff)
        fm.addSeparator()
        am = fm.addMenu("Animation")
        am.addAction("Export animated GIF…",  self._anim_export_gif)
        am.addAction("Export PNG sequence…",  self._anim_export_png_seq)
        fm.addSeparator()

        #    Platform                                                       
        pm_menu = fm.addMenu("Platform")
        pm_im = pm_menu.addMenu("Import")
        pm_im.addAction("ZX Spectrum SCR…",  self._import_scr)
        pm_im.addAction("MSX SC2…",          self._import_sc2)
        pm_im.addAction("Atari ST PI1…",     self._import_pi1)
        pm_im.addAction("C64 Koala…",        self._import_koala)
        pm_im.addAction("C64 Art Studio…",   self._import_art_studio)
        pm_im.addSeparator()
        pm_im.addAction("ZX Next NXI…",      self._import_nxi)
        pm_im.addAction("ZX Next PAL…",      self._import_pal)
        pm_ex = pm_menu.addMenu("Export")
        pm_ex.addAction("ZX Spectrum SCR…",  self._export_scr)
        pm_ex.addAction("MSX SC2…",          self._export_sc2)
        pm_ex.addAction("Atari ST PI1…",     self._export_pi1)
        pm_ex.addAction("C64 Koala…",        self._export_koala)
        pm_ex.addAction("C64 Art Studio…",   self._export_art_studio)
        pm_ex.addSeparator()
        pm_ex.addAction("ZX Next NXI…",      self._export_nxi)
        pm_ex.addAction("ZX Next PAL…",      self._export_pal)
        pm_xe = pm_menu.addMenu("Export Executable")
        pm_xe.addAction("ZX Spectrum TAP…",  self._export_tap)
        pm_xe.addAction("ZX Next NEX…",      self._export_nex)
        pm_xe.addAction("Amiga IFF HAM…",    self._export_iff_ham)
        pm_xe.addAction("C64 PRG (hires)…",  self._export_c64prg)
        pm_xe.addAction("C64 PRG (multi)…",  self._export_c64mprg)
        pm_xe.addAction("MSX COM…",          self._export_msxcom)
        pm_xe.addAction("Plus/4 PRG…",       self._export_plus4prg)
        pm_xe.addAction("VIC-20 PRG…",       self._export_vicprg)

        #    Texture                                                        
        tx_menu = fm.addMenu("Texture")
        tx_menu.addAction("Export PNG (current depth)…", self._export_texture_png)
        tx_menu.addAction("Export BMP…",                 self._export_texture_bmp)
        tx_menu.addAction("Export TGA…",                 self._export_tga)
        tx_menu.addAction("Export DDS…",                 self._export_dds)
        tx_menu.addAction("Export PCX…",                 self._export_pcx)
        tx_menu.addSeparator()
        tx_menu.addAction("Snap to user palette",        self._snap_canvas_to_user_palette)

        #    Icons                                                          
        ic_menu = fm.addMenu("Icons")
        ic_menu.addAction("Export Windows ICO…",   self._export_ico)
        ic_menu.addAction("Export Linux SVG…",      self._export_svg_icon)
        ic_menu.addAction("Export Apple ICNS…",     self._export_icns)
        ic_menu.addAction("Export Amiga Icon…",     self._export_amiga_icon)
        ic_menu.addSeparator()
        amiga_ic = ic_menu.addMenu("Import Amiga .info")
        amiga_ic.addAction("WB 3.9 AGA (256col) — default…", lambda: self._import_amiga_info('wb39'))
        amiga_ic.addAction("WB 3.9 XL AGA (256col)…",        lambda: self._import_amiga_info('wb39xl'))
        amiga_ic.addAction("AGA standard (16col)…",           lambda: self._import_amiga_info('aga'))
        amiga_ic.addAction("MagicWB (8col)…",                 lambda: self._import_amiga_info('magicwb'))
        amiga_ic.addAction("OCS/ECS WB3 (4col)…",            lambda: self._import_amiga_info('ocs'))
        amiga_ic.addAction("User palette…",                   lambda: self._import_amiga_info('user'))
        ic_menu.addAction("Import Windows ICO…",   self._import_ico)
        ic_menu.addAction("Import Apple ICNS…",    self._import_icns)
        ic_menu.addAction("Import SVG…",           self._import_svg)
        ic_menu.addSeparator()
        ic_menu.addAction("Snap to user palette",   self._snap_canvas_to_user_palette)

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
        em.addAction("Rotate Selection…",  self._rotate_selection_dialog)
        em.addSeparator()
        em.addAction("Clear canvas",       self._clear_canvas)
        em.addAction("Fill with colour",   self._fill_canvas)
        # Picture
        pm = mb.addMenu("Picture")
        # Flip / mirror
        pm.addAction("Zoom Lens…",          self._open_zoom_lens)
        pm.addSeparator()
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
        pm.addSeparator()
        pm.addAction("Colour Adjustments…", self._open_dp5_colour_adjust)
        pm.addAction("Seamless Tool…",      self._open_dp5_seamless)
        pm.addAction("Snow Effect…",        self._open_dp5_snow)
        pm.addSeparator()
        filters_m = pm.addMenu("Filters")
        filters_m.addAction("Sharpen",         lambda: self._dp5_sharpen(1.5))
        filters_m.addAction("Sharpen ×2",      lambda: self._dp5_sharpen(3.0))
        filters_m.addAction("Blur (r=1)",      lambda: self._dp5_blur(1.0))
        filters_m.addAction("Blur (r=2)",      lambda: self._dp5_blur(2.0))
        filters_m.addAction("Emboss",           self._dp5_emboss)
        filters_m.addAction("Edge Detect",      self._dp5_edge_detect)
        pm.addSeparator()
        pm.addAction("Snap to user palette",          self._snap_canvas_to_user_palette)
        pm.addAction("Snap to user palette (dithered)…", self._snap_canvas_to_user_palette_dither)
        pm.addSeparator()
        dm = pm.addMenu("Dither canvas")
        dm.addAction("Floyd-Steinberg…",    self._dither_floyd_steinberg)
        dm.addAction("Ordered Bayer 4×4…",  self._dither_bayer_canvas)
        dm.addAction("Checkerboard…",       self._dither_checker_canvas)
        # View
        vm = mb.addMenu("View")
        vm.addAction("Zoom in  Ctrl++",  lambda: self._set_zoom(
            self._canvas_zoom * 1.25 if self._canvas_zoom < 1
            else min(64, self._canvas_zoom + 1)))
        vm.addAction("Zoom out  Ctrl+-", lambda: self._set_zoom(
            max(0.05, self._canvas_zoom * 0.8 if self._canvas_zoom <= 1
            else self._canvas_zoom - 1)))
        vm.addSeparator()
        for z in (0.1, 0.25, 0.5, 1, 2, 4, 8, 16):
            lbl = f"{int(z)}×" if z >= 1 else f"{z}×"
            vm.addAction(lbl, lambda _, zz=z: self._set_zoom(zz))
        ga = vm.addAction("Pixel grid")
        ga.setCheckable(True)
        ga.setChecked(self.dp5_settings.get('show_pixel_grid'))
        ga.triggered.connect(self._set_show_grid)
        cg = vm.addAction("Cell grid (platform)")
        cg.setCheckable(True); cg.setChecked(False)
        cg.triggered.connect(self._toggle_cell_grid)
        sb = vm.addAction("Status bar")
        sb.setCheckable(True); sb.setChecked(self.dp5_settings.get('show_statusbar'))
        sb.triggered.connect(self._toggle_statusbar)
        an = vm.addAction("Animation timeline")
        an.setCheckable(True); an.setChecked(self.dp5_settings.get('show_anim_strip', False))
        an.triggered.connect(self._toggle_anim_strip)
        os_act = vm.addAction("Onion skin")
        os_act.setCheckable(True); os_act.setChecked(False)
        os_act.triggered.connect(self._toggle_onion_skin)
        vm.addSeparator()
        clash_act = vm.addAction("Colour clash visualiser")
        clash_act.setCheckable(True); clash_act.setChecked(False)
        clash_act.triggered.connect(self._toggle_clash_visualiser)
        self._clash_act = clash_act
        vm.addSeparator()

        # Canvas mode
        cm = vm.addMenu("Canvas Mode")
        for mode_id, mode_label in [
            ('free',     'Free — no constraints'),
            ('platform', 'Platform — retro computer'),
            ('texture',  'Texture — game textures'),
            ('icon',     'Icon — icons and sprites'),
        ]:
            a = cm.addAction(mode_label, lambda _, m=mode_id: self._set_canvas_mode(m, confirm=True))
            a.setCheckable(True)
            a.setChecked(mode_id == self._canvas_mode)

        # Tools menu (Renamed variable to 'tm' to avoid shadowing File 'fm')
        tm = mb.addMenu("Tools")
        bm = tm.addMenu("Batch Convert")
        bm.addAction("Icons…",    self._batch_convert_icons)
        bm.addAction("Textures…", self._batch_convert_textures)
        bm.addSeparator()

        # Correctly attaching these to 'tm' instead of 'vm'
        tm.addAction("Character/Font Editor…", self._open_char_editor)
        tm.addAction("Sprite Editor…",         self._open_sprite_editor)
        tm.addSeparator()

        # Correctly attaching 'Render as' to 'tm' instead of 'pm'
        rm = tm.addMenu("Render as")
        rm.addAction("ASCII art",     self._render_as_ascii)
        rm.addAction("ANSI art",      self._render_as_ansi)
        rm.addAction("PETSCII",       self._render_as_petscii)
        rm.addAction("Teletext",      self._render_as_teletext)

        # ... later in the _pm helper ...

        def _pm(label, items):
            sub = plm.addMenu(label)
            for name, mode in items:
                # Use 'checked' as a throwaway variable to catch the signal's boolean
                sub.addAction(name, lambda checked, m=mode: self._set_platform(m))

        # Platform menu
        plm = mb.addMenu("Platform")
        plm.addAction("None (free)", lambda: self._set_platform('none'))
        plm.addSeparator()

        def _pm(label, items):
            sub = plm.addMenu(label)
            for name, mode in items:
                sub.addAction(name, lambda m=mode: self._set_platform(m))

        _pm("Amiga", [
            ("OCS PAL LowRes  320×256  32col",  'amiga'),
            ("OCS NTSC LowRes 320×200  32col",  'amiga_ntsc'),
            ("OCS PAL HiRes   640×256  32col",  'amiga_hi'),
            ("OCS PAL LoRes interlace 320×512", 'amiga_lace'),
            ("ECS PAL          320×256  64col", 'amiga_ecs'),
            ("ECS PAL HiRes    640×256  64col", 'amiga_ecs_hi'),
            ("AGA PAL          320×256  256col",'amiga_aga'),
            ("AGA PAL HiRes    640×256  256col",'amiga_aga_hi'),
            ("HAM6             320×256  4096col",'amiga_ham'),
            ("HAM8             320×256  16Mcol", 'amiga_ham8'),
            ("RTG 640×480",                      'amiga_rtg'),
            ("RTG 800×600",                      'amiga_rtg_800'),
            ("RTG 1024×768",                     'amiga_rtg_1024'),
            ("RTG 720×576 PAL broadcast",        'amiga_rtg_pal'),
            ("RTG 720×480 NTSC broadcast",       'amiga_rtg_ntsc'),
        ])
        _pm("Commodore", [
            ("C64 Hires    320×200  2col/cell", 'c64'),
            ("C64 Multicolor 160×200 4col",     'c64m'),
            ("VIC-20       176×184",             'vic20'),
            ("Plus/4       320×200",             'plus4'),
        ])
        _pm("Sinclair / ZX", [
            ("Spectrum 48K   256×192  2col/cell", 'spectrum'),
            ("Spectrum 128K  256×192  same disp", 'spectrum128'),
            ("ZX80           256×192  B&W hard",  'zx80'),
            ("ZX81 WRX       256×192  B&W dither",'zx81'),
            ("ZX Next L2     320×256  256col",    'specnext'),
            ("ZX Next ULA    256×192  classic",   'specnext_ul'),
            ("Timex TS2068   256×192  standard",  'timex'),
            ("Timex HiRes    512×192  B&W",       'timex_hi'),
            ("Pentagon       256×192  like Spec", 'pentagon'),
            ("Jupiter Ace    256×192  B&W Forth", 'jupiter'),
        ])
        _pm("Atari", [
            ("2600 NTSC    160×96",              'atari_2600'),
            ("800/XL/XE    320×192  GTIA 256col",'atari_800'),
            ("5200         320×192  GTIA",       'atari_5200'),
            ("7800         320×200  GTIA",       'atari_7800'),
            ("ST           320×200  16col",      'atari_st'),
            ("STe          320×200  16col 12bit",'atari_ste'),
            ("Lynx         160×102  16col",      'atari_lynx'),
            ("Falcon       320×200  16bit",      'atari_falcon'),
            ("Jaguar       320×240  24bit",      'atari_jaguar'),
        ])
        _pm("Amstrad", [
            ("CPC Mode 0   160×200  4col",  'cpc'),
            ("CPC Mode 1   320×200  2col",  'cpc1'),
            ("CPC+/GX4000  320×200  4096col",'cpc_plus'),
            ("PCW           720×256  2col", 'pcw'),
            ("NC100/200    480×128  4col",  'nc'),
        ])
        _pm("MSX", [
            ("MSX1         256×192  16col", 'msx'),
            ("MSX2         256×212  512col",'msx2'),
        ])
        _pm("Other", [
            ("RM Nimbus    320×250  16col", 'nimbus'),
        ])
        plm.addSeparator()
        plm.addAction("Enforce colour constraints (toggle)", self._toggle_colour_constraints)

    #    Right panel: gadget bar + palettes                                     

    def get_menu_title(self) -> str: #vers 1
        """Short label for imgfactory titlebar button."""
        return "DP5"

    def _get_tool_menu_style(self) -> str: #vers 1
        """Read menu_style from dp5_settings."""
        return self.dp5_settings.get('menu_style', 'dropdown')

    def _build_menus_into_qmenu(self, parent_menu): #vers 4
        """Build all DP5 menus into parent_menu (QMenu or QMenuBar)."""
        self._build_canvas_menus(parent_menu)

    def _apply_menu_bar_style(self): #vers 3
        """Apply font size, height and colours to the topbar menubar.
        Uses explicit colours so it stays readable regardless of app theme.
        """
        mb = getattr(self, '_menu_bar', None)
        if not mb:
            return
        bar_h  = self.dp5_settings.get('menu_bar_height', 22)
        bar_fs = self.dp5_settings.get('menu_bar_font_size', 9)
        dd_fs  = self.dp5_settings.get('menu_dropdown_font_size', 9)

        # Get theme colours if available, otherwise use sensible defaults
        bg   = '#2b2b2b'
        fg   = '#e0e0e0'
        sel  = '#1976d2'
        selfg = '#ffffff'
        border = '#555555'
        try:
            app_settings = getattr(self, 'app_settings', None)
            if not app_settings and self.main_window:
                app_settings = getattr(self.main_window, 'app_settings', None)
            if app_settings:
                tc = app_settings.get_theme_colors() or {}
                bg    = tc.get('bg_primary',   bg)
                fg    = tc.get('text_primary',  fg)
                sel   = tc.get('accent',        sel)
                border = tc.get('border',       border)
        except Exception:
            pass

        # Height controlled by container — NOT by stylesheet min/max-height
        # (stylesheet height properties override Qt layout and prevent the bar showing)
        mb.setStyleSheet(f"""
            QMenuBar {{
                background-color: {bg};
                color: {fg};
                border-bottom: 1px solid {border};
                font-size: {bar_fs}pt;
            }}
            QMenuBar::item {{
                background-color: transparent;
                padding: 2px 6px;
            }}
            QMenuBar::item:selected {{
                background-color: {sel};
                color: {selfg};
            }}
            QMenu {{
                background-color: {bg};
                color: {fg};
                border: 1px solid {border};
                font-size: {dd_fs}pt;
            }}
            QMenu::item:selected {{
                background-color: {sel};
                color: {selfg};
            }}
        """)

        # Size the container based on settings, not isVisible() 
        # (isVisible() is False during __init__ even if widget will be shown)
        c = getattr(self, '_menu_bar_container', None)
        if c:
            show = (self.dp5_settings.get('show_menubar', False) and
                    self.dp5_settings.get('menu_style', 'dropdown') == 'topbar')
            if show:
                c.setMinimumHeight(0)
                c.setMaximumHeight(bar_h)
            # Don't touch height here if hiding — setup_ui and set_menu_orientation handle that


    def set_menu_orientation(self, style: str): #vers 5
        """Switch DP5 menu between 'topbar' (internal) and 'dropdown' (host menubar).
        When docked the internal bar is always suppressed regardless of style —
        imgfactory's top bar owns the menus via ToolMenuMixin injection.
        """
        self.dp5_settings.set('menu_style', style)
        self.dp5_settings.set('show_menubar', style == 'topbar')

        container = getattr(self, '_menu_bar_container', None) or getattr(self, '_menu_bar', None)
        if container:
            if style == 'topbar' and self.standalone_mode:
                # Only show internal bar in standalone mode
                container.setMinimumHeight(0)
                container.setMaximumHeight(16777215)
                container.setVisible(True)
                container.updateGeometry()
                self._apply_menu_bar_style()
            else:
                # Docked always hides internal bar; dropdown mode always hides it too
                container.setVisible(False)
                container.setMinimumHeight(0)
                container.setMaximumHeight(0)
                container.setFixedHeight(0)

        # Notify imgfactory to inject/remove tool menu when docked
        if not self.standalone_mode:
            mw = getattr(self, 'main_window', None)
            if mw and hasattr(mw, 'menu_bar_system'):
                if style == 'dropdown':
                    mw.menu_bar_system._inject_tool_menu(self)
                else:
                    mw.menu_bar_system._remove_tool_menu()


    def _create_right_panel(self): #vers 2
        """Right panel: adaptive-column gadget bar, FGBGSwatch, palettes."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)

        icon_color = self._get_icon_color()

        #    Column count: fill available width                            
        icon_sz   = self.dp5_settings.get('tool_icon_size')   # 16–64 px
        btn_sz    = icon_sz + 6   # button size = icon + padding
        gap       = 2             # grid spacing

        # Minimum columns from settings — floor, not ceiling
        min_cols = max(2, self.dp5_settings.get('tool_columns'))

        # Available width: read the actual splitter size if possible.
        available_w = 0
        if hasattr(self, '_splitter') and self._splitter:
            sizes = self._splitter.sizes()
            if sizes:
                available_w = sizes[-1]
        if available_w < 60:
            try:
                from PyQt6.QtWidgets import QApplication as _QApp
                sw = (_QApp.primaryScreen().availableSize().width()
                      if _QApp.primaryScreen() else 1400)
            except Exception:
                sw = 1400
            available_w = max(180, sw // 5)

        # Columns that fit in available_w
        usable = max(btn_sz, available_w - 20)
        n_cols = max(min_cols, usable // (btn_sz + gap))

        # Do NOT setFixedWidth — let the splitter control panel width freely.
        # Only set a minimum so it can't collapse to nothing.
        min_panel_w = min_cols * (btn_sz + gap) - gap + 20
        panel.setMinimumWidth(min_panel_w)
        panel.setMaximumWidth(16777215)   # unconstrained

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(4, 4, 4, 4)
        layout.setSpacing(3)

        #    Flat ordered tool list (no row/col — computed below)            
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
            (TOOL_CROP,     'crop',     'Crop canvas to selection (X)'),
            (TOOL_RESIZE,   'resize',   'Resize canvas (V)'),
            (TOOL_DITHER,   'dither',     'Dither brush — checkerboard FG/BG pattern (D)'),
            (TOOL_SYMMETRY, 'symmetry',   'Symmetry — click to cycle: H / V / Quad / Off (Y)'),
            (TOOL_BLUR_BRUSH,'blur_brush', 'Blur brush — soften under cursor (B)'),
            (TOOL_SMUDGE,   'smudge',     'Smudge — blend/drag pixels (U)'),
            (TOOL_LIGHTEN,  'lighten',    'Lighten (Dodge) — brighten under cursor (,)'),
            (TOOL_DARKEN,   'darken',     'Darken (Burn) — darken under cursor (.)'),
        ]
        hidden_tools = self.dp5_settings.get('hidden_tools') or []
        TOOL_ORDER = [(t, s, tip) for t, s, tip in TOOL_ORDER if t not in hidden_tools]

        #    Build gadget grid                                               
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

        # Determine tile/icon colours from theme for initial render
        _icon_col = self._get_icon_color()
        _tile_bg  = ''
        try:
            if self.app_settings:
                _tc = self.app_settings.get_theme_colors() or {}
                _tile_bg = _tc.get('gadgetbar_bg',
                               _tc.get('toolbar_bg',
                                   _tc.get('bg_secondary', '')))
        except Exception:
            pass

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
            ico = _load_tool_icon(shape, icon_sz, active=False,
                                  tile_bg=_tile_bg, icon_col=_icon_col)
            btn.setIcon(ico)
            btn.setIconSize(QSize(icon_sz, icon_sz))
            btn.clicked.connect(lambda _, t=tool_id: self._select_tool(t))
            self._tool_btns[tool_id] = btn
            gadget_grid.addWidget(btn, row, col)
            # Zoom button gets right-click mode selector
            if tool_id == TOOL_ZOOM:
                btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
                btn.customContextMenuRequested.connect(self._zoom_mode_menu)
                btn.setToolTip("Zoom — left-click to zoom in\nRight-click to select zoom mode")

        layout.addLayout(gadget_grid)

        layout.addSpacing(4)

        #    Brush size slider + value label                                
        size_hdr = QHBoxLayout()
        size_lbl = QLabel("Size")
        size_lbl.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        size_hdr.addWidget(size_lbl)
        self._size_sl = QSlider(Qt.Orientation.Horizontal)
        self._size_sl.setRange(1, 20)
        self._size_sl.setValue(1)
        self._size_sl.setMinimumHeight(24)
        self._size_sl.valueChanged.connect(self._set_brush_size)
        size_hdr.addWidget(self._size_sl)
        self._size_val_lbl = QLabel("1")
        self._size_val_lbl.setAlignment(Qt.AlignmentFlag.AlignRight |
                                        Qt.AlignmentFlag.AlignVCenter)
        self._size_val_lbl.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        self._size_val_lbl.setFixedWidth(28)
        size_hdr.addWidget(self._size_val_lbl)
        layout.addLayout(size_hdr)

        #    Snap to grid toggle                                            
        snap_row = QHBoxLayout()
        self._snap_chk = QCheckBox("Snap to grid")
        self._snap_chk.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        self._snap_chk.setChecked(False)
        self._snap_chk.setToolTip("Snap drawing to pixel grid")
        self._snap_chk.toggled.connect(self._set_snap_grid)
        self._grid_chk2 = QCheckBox("Show grid")
        self._grid_chk2.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        self._grid_chk2.setChecked(self.dp5_settings.get('show_pixel_grid'))
        self._grid_chk2.toggled.connect(self._set_show_grid)
        self._zoom_lbl = QLabel(f"{self._canvas_zoom}×")
        self._zoom_lbl.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        self._zoom_lbl.setFont(QFont("Arial", self.fonthsize))
        snap_row.addWidget(self._snap_chk)
        snap_row.addWidget(self._grid_chk2)
        snap_row.addWidget(self._zoom_lbl)
        layout.addLayout(snap_row)

        layout.addSpacing(4)

        #    FG / BG swatch  +  brush thumbnail                            
        fgbg_row_lbl = QHBoxLayout()
        fgbg_lbl = QLabel("FG / BG")
        fgbg_lbl.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        fgbg_row_lbl.addWidget(fgbg_lbl)
        brush_lbl = QLabel("Brush")
        brush_lbl.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        fgbg_row_lbl.addWidget(brush_lbl)
        layout.addLayout(fgbg_row_lbl)

        fgbg_row = QHBoxLayout()
        fgbg_row.setSpacing(4)

        self._fgbg_swatch = FGBGSwatch()
        self._fgbg_swatch.fg_changed.connect(self._on_fg_changed)
        self._fgbg_swatch.bg_changed.connect(self._on_bg_changed)
        fgbg_row.addWidget(self._fgbg_swatch)

        self._brush_thumb = BrushThumbnail()
        self._brush_thumb.stamp_requested.connect(self._activate_stamp_mode)
        self._brush_thumb.clear_requested.connect(self._clear_brush)
        fgbg_row.addWidget(self._brush_thumb)

        zoom_stack = QVBoxLayout()
        zoom_stack.setSpacing(2)
        zoom_in_btn = QPushButton()
        zoom_in_btn.setFixedSize(24, 24)
        zoom_in_btn.setToolTip("Zoom in")
        zoom_in_btn.clicked.connect(lambda: self._set_zoom(
            self._canvas_zoom * 1.25 if self._canvas_zoom < 1 else min(64, self._canvas_zoom + 1)))
        try:
            zoom_in_btn.setIcon(SVGIconFactory.zoom_in_icon(14, icon_color))
            zoom_in_btn.setIconSize(QSize(14, 14))
        except Exception:
            zoom_in_btn.setText("+")
        zoom_out_btn = QPushButton()
        zoom_out_btn.setFixedSize(24, 24)
        zoom_out_btn.setToolTip("Zoom out")
        zoom_out_btn.clicked.connect(lambda: self._set_zoom(
            max(0.05, self._canvas_zoom * 0.8 if self._canvas_zoom <= 1 else self._canvas_zoom - 1)))
        try:
            zoom_out_btn.setIcon(SVGIconFactory.zoom_out_icon(14, icon_color))
            zoom_out_btn.setIconSize(QSize(14, 14))
        except Exception:
            zoom_out_btn.setText("-")
        zoom_stack.addWidget(zoom_in_btn)
        zoom_stack.addWidget(zoom_out_btn)
        fgbg_row.addLayout(zoom_stack)

        layout.addLayout(fgbg_row)

        #    Opacity                                                        
        op_row = QHBoxLayout()
        op_lbl = QLabel("Opacity")
        op_lbl.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        op_row.addWidget(op_lbl)
        self._opacity_sl = QSlider(Qt.Orientation.Horizontal)
        self._opacity_sl.setRange(0, 100)
        self._opacity_sl.setValue(100)
        self._opacity_sl.setMinimumHeight(24)
        self._opacity_sl.valueChanged.connect(self._set_opacity)
        op_row.addWidget(self._opacity_sl)
        self._opacity_val_lbl = QLabel("100%")
        self._opacity_val_lbl.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        self._opacity_val_lbl.setFixedWidth(34)
        self._opacity_val_lbl.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        op_row.addWidget(self._opacity_val_lbl)
        layout.addLayout(op_row)

        #    Colour history                                                 
        hist_lbl = QLabel("Recent")
        hist_lbl.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        layout.addWidget(hist_lbl)
        hist_row = QHBoxLayout()
        hist_row.setSpacing(2)
        self._color_history = []
        self._color_hist_btns = []
        for _ in range(12):
            b = QPushButton()
            b.setFixedSize(12, 12)
            b.setStyleSheet("background:palette(base); border:1px solid palette(mid);")
            b.setEnabled(False)
            hist_row.addWidget(b)
            self._color_hist_btns.append(b)
        layout.addLayout(hist_row)

        layout.addSpacing(4)

        #    IMAGE palette                                                  
        img_pal_lbl = QLabel("Image Palette:")
        img_pal_lbl.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        layout.addWidget(img_pal_lbl)

        img_pal_ctrl = QHBoxLayout()
        self._bit_depth_combo = QComboBox()
        self._bit_depth_combo.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        self._bit_depth_combo.addItems(["32bit", "24bit", "16bit", "8bit"])
        self._bit_depth_combo.setFixedHeight(24)
        self._bit_depth_combo.setFixedWidth(40)
        self._bit_depth_combo.setToolTip("Colour depth for quantization")
        img_pal_ctrl.addWidget(self._bit_depth_combo)
        img_pal_apply_btn = QPushButton("Apply")
        img_pal_apply_btn.setFont(QFont("Arial", self.fonthsize))
        img_pal_apply_btn.setFixedHeight(24)
        img_pal_apply_btn.setToolTip("Quantize canvas to selected bit depth")
        img_pal_apply_btn.clicked.connect(self._apply_bit_depth)
        img_pal_ctrl.addWidget(img_pal_apply_btn)
        self._img_pal_group_btn = QPushButton("Group")
        self._img_pal_group_btn.setFont(QFont("Arial", self.fonthsize))
        self._img_pal_group_btn.setFixedHeight(24)
        self._img_pal_group_btn.setToolTip("Sort palette by hue — click to toggle asc/desc")
        self._img_pal_group_btn.clicked.connect(self._group_palette)
        img_pal_ctrl.addWidget(self._img_pal_group_btn)
        self._group_palette_asc = True
        layout.addLayout(img_pal_ctrl)

        # Image palette — auto-wraps columns to fill available panel width.
        # Height is uncapped (scroll area grows with colour count); vertical
        # scrollbar appears if it would exceed the panel. PaletteGrid recalculates
        # columns from its own width on every resize, so no manual col setting needed.
        img_cols = self.dp5_settings.get('img_pal_cols')
        self.pal_bar = PaletteGrid(cols=img_cols, cell=12)
        self.pal_bar.color_picked.connect(self._on_image_palette_color)
        self._img_pal_scroll = QScrollArea()
        self._img_pal_scroll.setWidget(self.pal_bar)
        self._img_pal_scroll.setWidgetResizable(True)   # lets pal_bar fill width
        self._img_pal_scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._img_pal_scroll.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self._img_pal_scroll.setMinimumHeight(12)
        # Max height: up to ~half the screen, so it never dominates the panel
        from PyQt6.QtWidgets import QApplication as _QApp
        _sh = _QApp.primaryScreen().availableSize().height() if _QApp.primaryScreen() else 800
        self._img_pal_scroll.setMaximumHeight(_sh // 3)
        layout.addWidget(self._img_pal_scroll, stretch=1)

        #    USER palette (retro presets)                                   
        user_pal_hdr = QHBoxLayout()
        user_pal_lbl = QLabel("User Palette:")
        user_pal_lbl.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        user_pal_hdr.addWidget(user_pal_lbl)
        self._retro_btn = QPushButton("Amiga AGA WB")
        self._retro_btn.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        self._retro_btn.setFixedHeight(24)
        self._retro_btn.setToolTip("User palette — choose retro preset")
        self._retro_btn.clicked.connect(self._show_retro_menu)
        user_pal_hdr.addWidget(self._retro_btn)
        self._pal_dither_btn = QPushButton("Dith")
        self._pal_dither_btn.setVisible(False)  # dither via File menu instead
        layout.addLayout(user_pal_hdr)

        user_rows = self.dp5_settings.get('user_pal_rows')
        user_pal_max_h = max(user_rows * 12 + 2, 130)
        # User palette: cell size computed to fill panel width.
        # We use a ResizingPaletteGrid that recalculates cell on resize
        # so swatches always fill the available width cleanly.
        self._user_pal_grid = _AutoCellPaletteGrid()
        self._user_pal_grid.color_picked.connect(self._on_user_palette_color)
        user_pal_scroll = QScrollArea()
        user_pal_scroll.setWidget(self._user_pal_grid)
        user_pal_scroll.setWidgetResizable(True)
        user_pal_scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        user_pal_scroll.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        user_pal_scroll.setMaximumHeight(user_pal_max_h)
        user_pal_scroll.setMinimumHeight(12)
        self._user_pal_scroll = user_pal_scroll
        layout.addWidget(user_pal_scroll, stretch=1)

        #    Image operation quick buttons                                 
        imgop_lbl = QLabel("Image Ops:")
        imgop_lbl.setFont(QFont("Arial", self.fonthsize, QFont.Weight.Bold))
        layout.addWidget(imgop_lbl)

        imgop_row = QHBoxLayout()
        imgop_row.setSpacing(3)

        def _imgop_btn(icon_method, tip, slot):
            b = QPushButton()
            b.setFixedSize(32, 32)
            b.setToolTip(tip)
            try:
                b.setIcon(getattr(SVGIconFactory, icon_method)(20, icon_color))
                b.setIconSize(QSize(20, 20))
            except Exception:
                b.setText(tip[:3])
            b.clicked.connect(slot)
            imgop_row.addWidget(b)
            return b

        _imgop_btn('dp_colour_correct_icon', 'Colour Adjustments…', self._open_dp5_colour_adjust)
        _imgop_btn('dp_seamless_op_icon',    'Seamless Tool…',       self._open_dp5_seamless)
        _imgop_btn('snow_icon',              'Snow Effect…',         self._open_dp5_snow)
        _imgop_btn('zoom_in_icon',           'Zoom Lens…',           self._open_zoom_lens)
        _imgop_btn('svg_edit_icon',          'SVG Icon Browser…',    self._open_icon_browser)
        _imgop_btn('folder_icon',            'Icon Editor…',         self._open_icon_editor)
        imgop_row.addStretch()
        layout.addLayout(imgop_row)

        # Load default retro palette
        self._apply_retro_palette(self.dp5_settings.get('retro_palette'))

        return panel

    #    Tool / colour helpers                                                  

    def _toggle_shape_fill(self, primary_tool_id: str): #vers 2
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


    def _select_tool(self, tool_id: str): #vers 2
        """Select a tool, resolving fill state for shape tools."""
        # Crop and resize are immediate actions, not persistent tools
        if tool_id == TOOL_CROP:
            self._crop_to_selection()
            return
        if tool_id == TOOL_RESIZE:
            self._resize_canvas_dialog()
            return
        # Dither and symmetry are toggles
        if tool_id == TOOL_DITHER:
            self._toggle_dither_mode()
            return
        if tool_id == TOOL_SYMMETRY:
            self._toggle_symmetry_mode()
            return

        actual_tool = tool_id
        if tool_id in self._shape_fill_state:
            if self._shape_fill_state[tool_id]:
                actual_tool = SHAPE_FILL_PAIRS[tool_id]

        if self.dp5_canvas:
            # Clear selection marching-ants when leaving SELECT tool
            if self.dp5_canvas.tool in (TOOL_SELECT, 'lasso') and actual_tool not in (TOOL_SELECT, 'lasso'):
                self.dp5_canvas._sel_active    = False
                self.dp5_canvas._selection_rect = None
                self.dp5_canvas._sel_floating   = False
                self.dp5_canvas.update()
            self.dp5_canvas.tool = actual_tool
            self.dp5_canvas._curve_pts = []
            self.dp5_canvas._poly_pts  = []
            # Set cursor for zoom mode
            if tool_id == TOOL_ZOOM:
                zm = getattr(self.dp5_canvas, '_zoom_mode', 'in')
                if zm == 'box':
                    self.dp5_canvas.setCursor(Qt.CursorShape.CrossCursor)
                elif zm == 'out':
                    self.dp5_canvas.setCursor(Qt.CursorShape.SizeAllCursor)
                else:
                    self.dp5_canvas.setCursor(Qt.CursorShape.ArrowCursor)
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


    def _set_brush_size(self, v: int):  #vers 2
        if self.dp5_canvas:
            self.dp5_canvas.brush_size = v
        if hasattr(self, '_size_val_lbl'):
            self._size_val_lbl.setText(str(v))


    def _toggle_dither_mode(self): #vers 2
        """Cycle dither: off → checker → bayer → off."""
        cycle = {'off': 'checker', 'checker': 'bayer', 'bayer': 'off'}
        self._dither_mode = cycle[self._dither_mode]
        if self.dp5_canvas:
            self.dp5_canvas.dither_mode = self._dither_mode
        btn = self._tool_btns.get(TOOL_DITHER)
        if btn:
            labels = {'off':'Dither','checker':'Dthr ⊞','bayer':'Dthr ▦'}
            btn.setChecked(self._dither_mode != 'off')
            btn.setToolTip(f"Dither: {self._dither_mode} — click to cycle")
        self._set_status(f"Dither: {self._dither_mode}")


    def _toggle_symmetry_mode(self): #vers 2
        """Cycle symmetry: off → H → V → quad → off."""
        cycle = {'off': 'H', 'H': 'V', 'V': 'quad', 'quad': 'off'}
        self._symmetry_mode = cycle[self._symmetry_mode]
        if self.dp5_canvas:
            self.dp5_canvas.symmetry_mode = self._symmetry_mode
        btn = self._tool_btns.get(TOOL_SYMMETRY)
        if btn:
            labels = {'off': 'Sym', 'H': 'Sym H', 'V': 'Sym V', 'quad': 'Sym X'}
            btn.setChecked(self._symmetry_mode != 'off')
            btn.setToolTip(f"Symmetry: {self._symmetry_mode.upper()} — click to cycle")
        self._set_status(f"Symmetry: {self._symmetry_mode.upper()}")

    def _set_opacity(self, v: int): #vers 1
        if self.dp5_canvas:
            self.dp5_canvas.opacity = v / 100.0
        if hasattr(self, '_opacity_val_lbl'):
            self._opacity_val_lbl.setText(f"{v}%")

    def _on_fg_changed(self, c: QColor): #vers 2
        if self.dp5_canvas:
            self.dp5_canvas.color = c
        self.pal_bar.set_selection_by_color(c)
        self._push_color_history(c)

    def _push_color_history(self, c: QColor): #vers 1
        hex_c = c.name()
        if self._color_history and self._color_history[0] == hex_c:
            return
        if hex_c in self._color_history:
            self._color_history.remove(hex_c)
        self._color_history.insert(0, hex_c)
        self._color_history = self._color_history[:12]
        for i, btn in enumerate(self._color_hist_btns):
            if i < len(self._color_history):
                col = self._color_history[i]
                btn.setStyleSheet(f"background:{col}; border:1px solid palette(mid);")
                btn.setEnabled(True)
                btn.setToolTip(col)
                btn.clicked.disconnect() if btn.receivers(btn.clicked) > 0 else None
                btn.clicked.connect(lambda _, hc=col: self._fgbg_swatch.set_fg(QColor(hc)))
            else:
                btn.setStyleSheet("background:#222; border:1px solid palette(mid);")
                btn.setEnabled(False)

    def _on_bg_changed(self, c: QColor):
        # Background colour stored in swatch; eraser uses it in future
        pass

    def _on_image_palette_color(self, c: QColor): #vers 1
        if self.dp5_canvas:
            self.dp5_canvas.color = c
        self._fgbg_swatch.set_fg(c)

    def _on_user_palette_color(self, c: QColor): #vers 1
        if self.dp5_canvas:
            self.dp5_canvas.color = c
        self._fgbg_swatch.set_fg(c)

    def _apply_bit_depth(self): #vers 3
        """Quantize canvas RGBA to selected bit depth and update palette grid."""
        if not self.dp5_canvas: return
        depth = self._bit_depth_combo.currentText()
        try:
            from PIL import Image
            w, h = self.dp5_canvas.tex_w, self.dp5_canvas.tex_h
            img = Image.frombytes('RGBA', (w, h), bytes(self.dp5_canvas.rgba))

            if depth == "32bit":
                out_img = img                              # no change
            elif depth == "24bit":
                out_img = img.convert('RGB').convert('RGBA')  # drop alpha
            elif depth == "16bit":
                rgb = img.convert('RGB')
                pixels = rgb.tobytes()
                buf = bytearray(len(pixels))
                for i in range(0, len(pixels), 3):
                    buf[i]   = (pixels[i]   >> 3) << 3    # 5-bit R
                    buf[i+1] = (pixels[i+1] >> 2) << 2    # 6-bit G
                    buf[i+2] = (pixels[i+2] >> 3) << 3    # 5-bit B
                out_img = Image.frombytes('RGB', (w, h), bytes(buf)).convert('RGBA')
            elif depth == "8bit":
                q = img.convert('RGB').quantize(colors=256)
                out_img = q.convert('RGB').convert('RGBA')
            else:
                return

            self.dp5_canvas.rgba = bytearray(out_img.tobytes())
            self.dp5_canvas.update()

            p_img = out_img.convert('RGB').quantize(colors=256)
            pal_flat = p_img.getpalette()
            palette = [(pal_flat[i*3], pal_flat[i*3+1], pal_flat[i*3+2]) for i in range(256)]
            self.pal_bar.set_palette_raw(palette)
            self._set_status(f"Applied {depth} quantization")
        except Exception as e:
            QMessageBox.warning(self, "Bit Depth Error", str(e))

    def _apply_palette0_alpha(self, img): #vers 2
        """Make palette index 0 (or first colour) transparent.
        Pixels matching pal[0] RGB OR already fully transparent become alpha=0;
        everything else gets alpha=255.
        """
        if img.mode != 'RGBA':
            img = img.convert('RGBA')

        px = img.load()
        w, h = img.size

        # Get first palette colour (fallback to pure black if none)
        pal = self._get_user_palette_rgb() or []
        key = tuple(pal[0][:3]) if pal else (0, 0, 0)

        for y in range(h):
            for x in range(w):
                r, g, b, a = px[x, y]
                if (r, g, b) == key or a == 0:
                    px[x, y] = (r, g, b, 0)
                else:
                    px[x, y] = (r, g, b, 255)

        return img

    def _fit_img_pal_height(self): #vers 1
        """Resize the image palette scroll area to fit its content snugly,
        up to the scroll area's maximum height."""
        if not hasattr(self, '_img_pal_scroll') or not hasattr(self, 'pal_bar'):
            return
        # pal_bar.fixedHeight is set by _recalc_height() after set_palette_raw
        content_h = self.pal_bar.height()
        scroll = self._img_pal_scroll
        max_h = scroll.maximumHeight()
        # Add a little for the scrollbar chrome
        target = min(content_h + 4, max_h)
        scroll.setFixedHeight(max(12, target))

    def _group_palette(self): #vers 3
        """Sort current image palette by hue, toggling asc/desc each click."""
        try:
            import colorsys
            if not hasattr(self.pal_bar, '_colors') or not self.pal_bar._colors:
                return
            self._group_palette_asc = not getattr(self, '_group_palette_asc', True)
            asc = self._group_palette_asc
            def hue_key(qc):
                h, s, v = colorsys.rgb_to_hsv(qc.red()/255, qc.green()/255, qc.blue()/255)
                return (h, -v, -s)
            sorted_colors = sorted(self.pal_bar._colors, key=hue_key, reverse=not asc)
            palette = [(c.red(), c.green(), c.blue()) for c in sorted_colors]
            self.pal_bar.set_palette_raw(palette)
            if hasattr(self, '_img_pal_group_btn'):
                self._img_pal_group_btn.setText("Group ↑" if asc else "Group ↓")
            self._set_status(f"Palette grouped by hue ({'asc' if asc else 'desc'})")
        except Exception as e:
            QMessageBox.warning(self, "Group Error", str(e))


    def _update_color_swatches(self): #vers 1
        """Sync FGBGSwatch after colour changes (e.g. picker tool)."""
        if not hasattr(self, '_fgbg_swatch'): return
        if self.dp5_canvas:
            self._fgbg_swatch.set_fg(self.dp5_canvas.color)
            self.pal_bar.set_selection_by_color(self.dp5_canvas.color)


    def _on_menu_btn_clicked(self): #vers 3
        style = self.dp5_settings.get('menu_style')
        if style == 'dropdown':
            self._show_dropdown_menu()
        else:
            on = not self.dp5_settings.get('show_menubar')
            self.dp5_settings.set('show_menubar', on)
            self.dp5_settings.save()
            c = getattr(self, '_menu_bar_container', self._menu_bar if hasattr(self, '_menu_bar') else None)
            if c:
                c.setMinimumHeight(0)
                c.setMaximumHeight(16777215 if on else 0)
                c.setVisible(on)


    def _show_dropdown_menu(self): #vers 2
        """Pop up the canvas menus as a single QMenu dropdown — standalone safe."""
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        try:
            self._build_menus_into_qmenu(menu)
        except Exception as _e:
            menu.addAction(f"Menu error: {_e}").setEnabled(False)
        btn = getattr(self, 'menu_toggle_btn', None)
        if btn:
            menu.exec(btn.mapToGlobal(btn.rect().bottomLeft()))
        else:
            from PyQt6.QtGui import QCursor
            menu.exec(QCursor.pos())


    def _toggle_menubar(self, on: bool): #vers 3
        self.dp5_settings.set('show_menubar', on)
        self.dp5_settings.save()
        c = getattr(self, '_menu_bar_container', self._menu_bar if hasattr(self, '_menu_bar') else None)
        if c:
            c.setMinimumHeight(0)
            c.setMaximumHeight(16777215 if on else 0)
            c.setVisible(on)


    def _set_snap_grid(self, on: bool): #vers 1
        if self.dp5_canvas:
            self.dp5_canvas.snap_grid = on


    def _set_show_grid(self, on: bool): #vers 2
        if self.dp5_canvas:
            self.dp5_canvas.show_grid = bool(on)
            self.dp5_canvas.update()
        self.dp5_settings.set('show_pixel_grid', bool(on))
        self.dp5_settings.save()
        if hasattr(self, '_grid_chk2'):
            self._grid_chk2.setChecked(bool(on))

    # Platform cell sizes: (cell_w, cell_h, max_colours_per_cell)
    _PLATFORM_CELLS = {
        'none':        (1,  1,   256),
        'amiga':       (8,  1,   32),
        'amiga_ecs':   (8,  1,   64),
        'amiga_aga':   (8,  1,   256),
        'amiga_ham':   (1,  1,   4096),
        'amiga_ham8':  (1,  1,   16777216),
        'amiga_rtg':   (1,  1,   256),
        'amiga_ntsc':  (8,  1,   32),    # OCS NTSC 320×200
        'amiga_hi':    (16, 1,   32),    # OCS HiRes 640×256
        'amiga_lace':  (8,  1,   32),    # OCS PAL interlace 320×512
        'amiga_ecs_hi':(16, 1,   64),    # ECS HiRes 640×256
        'amiga_aga_hi':(16, 1,   256),   # AGA HiRes 640×256
        'amiga_rtg_800':  (1,1,  256),   # RTG 800×600
        'amiga_rtg_1024': (1,1,  256),   # RTG 1024×768
        'amiga_rtg_pal':  (1,1,  256),   # RTG 720×576 PAL
        'amiga_rtg_ntsc': (1,1,  256),   # RTG 720×480 NTSC
        'c64':         (8,  8,   2),
        'c64m':        (4,  8,   4),
        'spectrum':    (8,  8,   2),
        'spectrum128': (8,  8,   2),   # same display as 48K
        'zx80':        (8,  8,   2),
        'zx81':        (8,  8,   2),
        'specnext':    (1,  1,   256), # Layer 2 free pixel mode
        'specnext_ul': (8,  8,   2),   # Next classic ULA mode
        'timex':       (8,  8,   2),   # TS2068 standard mode
        'timex_hi':    (1,  1,   2),   # TS2068 HiRes 512×192 B&W
        'pentagon':    (8,  8,   2),   # same as Spectrum
        'jupiter':     (8,  8,   2),   # Jupiter Ace B&W
        'msx':         (8,  8,   2),
        'msx2':        (8,  8,   16),
        'cpc':         (4,  8,   4),
        'cpc1':        (8,  8,   2),
        'cpc_plus':    (8,  8,   16),
        'pcw':         (8,  8,   2),
        'nc':          (8,  8,   4),
        'atari_2600':  (2,  1,   4),
        'atari_st':    (16, 1,   16),
        'atari_ste':   (16, 1,   16),
        'atari_800':   (2,  1,   4),
        'atari_5200':  (2,  1,   4),
        'atari_7800':  (2,  1,   4),
        'atari_lynx':  (1,  1,   16),
        'atari_falcon':(1,  1,   65536),
        'atari_jaguar':(1,  1,   16777216),
        'plus4':       (8,  8,   2),
        'vic20':       (8,  8,   2),
        'nimbus':      (4,  4,   16),
        'nimbus_hi':   (4,  4,   16),   # Nimbus 640×250
        'nes':         (8,  8,   4),    # NES 256×240, 4col/8×8 tile
        'snes':        (8,  8,   16),   # SNES 256×224, 16col/8×8 tile
        'game_boy':    (8,  8,   4),    # GB 160×144, 4 shades
        'game_boy_pocket':(8,8,  4),
        'game_boy_color': (8,8,  4),    # GBC 160×144
        'game_boy_advance':(8,8, 4),    # GBA 240×160
        'sg1000':      (8,  8,   16),   # SG-1000 256×192
        'master_sys':  (8,  8,   16),   # Master System 256×192
        'mega_drive':  (8,  8,   16),   # Mega Drive 320×224
        'game_gear':   (8,  8,   16),   # Game Gear 160×144
        'pc_engine':   (8,  8,   16),   # PC Engine 256×240
    }


    def _set_platform(self, mode: str): #vers 4
        """Set platform mode — cell grid, auto-load palette, resize canvas, fit zoom."""
        self._platform_mode = mode
        cw, ch, _ = self._PLATFORM_CELLS.get(mode, (1,1,256))
        if self.dp5_canvas:
            self.dp5_canvas.cell_w = cw
            self.dp5_canvas.cell_h = ch
            if mode != 'none':
                self.dp5_canvas.show_cell_grid = True
            self.dp5_canvas.update()
        self.dp5_settings.set('platform_mode', mode)

        _pal_map = {
            'c64': 'C64', 'c64m': 'C64',
            'spectrum': 'ZX Spectrum', 'spectrum128': 'ZX Spectrum 128K',
            'specnext': 'ZX Spectrum Next', 'specnext_ul': 'ZX Spectrum',
            'zx80': 'ZX80', 'zx81': 'ZX81',
            'timex': 'Timex TS2068', 'timex_hi': 'Timex HiRes',
            'pentagon': 'Pentagon', 'jupiter': 'Jupiter Ace',
            'msx': 'MSX1', 'msx2': 'MSX2',
            'cpc': 'Amstrad CPC', 'cpc1': 'Amstrad CPC',
            'cpc_plus': 'Amstrad CPC+',
            'pcw':  'Amstrad PCW',
            'nc':   'Amstrad NC100/200',
            'atari_2600': 'Atari 2600 NTSC',
            'atari_st': 'Atari ST', 'atari_ste': 'Atari STe',
            'atari_800': 'Atari 800 GTIA',
            'atari_5200': 'Atari 5200',
            'atari_7800': 'Atari 7800',
            'atari_lynx': 'Atari Lynx',
            'atari_falcon': 'Atari Falcon',
            'atari_jaguar': 'Atari Jaguar',
            'amiga': 'Amiga OCS', 'amiga_ntsc': 'Amiga OCS',
            'amiga_hi': 'Amiga OCS', 'amiga_lace': 'Amiga OCS',
            'amiga_ecs': 'Amiga ECS', 'amiga_ecs_hi': 'Amiga ECS',
            'amiga_aga_hi': 'Amiga AGA',
            'amiga_rtg_800': 'Amiga AGA WB', 'amiga_rtg_1024': 'Amiga AGA WB',
            'amiga_rtg_pal': 'Amiga AGA WB', 'amiga_rtg_ntsc': 'Amiga AGA WB',
            'amiga_aga': 'Amiga AGA',
            'amiga_ham': 'Amiga OCS',
            'amiga_ham8': 'Amiga AGA',
            'amiga_rtg': 'Amiga AGA WB',
            'plus4': 'Plus/4', 'vic20': 'VIC-20',
            'nimbus': 'RM Nimbus',
        }
        if mode in _pal_map:
            self._apply_retro_palette(_pal_map[mode])

        # Resize canvas to platform native resolution
        _plat_res = {
            'c64':         (320, 200), 'c64m':        (160, 200),
            'spectrum':    (256, 192), 'spectrum128': (256, 192),
            'zx80':        (256, 192), 'zx81':        (256, 192),
            'specnext':    (320, 256), 'specnext_ul': (256, 192),
            'timex':       (256, 192), 'timex_hi':    (512, 192),
            'pentagon':    (256, 192), 'jupiter':     (256,  192),
            'msx':         (256, 192), 'msx2':        (256, 212),
            'cpc':         (160, 200), 'cpc1':        (320, 200),
            'cpc_plus':    (320, 200), 'pcw':         (720, 256),
            'nc':          (480, 128),
            'atari_2600':  (160, 192),  # NTSC standard kernel 'atari_st':    (320, 200),
            'atari_ste':   (320, 200), 'atari_800':   (320, 192),
            'atari_5200':  (320, 192), 'atari_7800':  (160, 240),  # 160×240 NTSC most common
            'atari_lynx':  (160, 102), 'atari_falcon': (320, 200), 'atari_falcon_hi': (640, 480),
            'atari_jaguar': (320, 240),
            'amiga':       (320, 256), 'amiga_ntsc':  (320, 200),
            'amiga_hi':    (640, 256), 'amiga_lace':  (320, 512),
            'amiga_ecs':   (320, 256), 'amiga_ecs_hi':(640, 256),
            'amiga_aga_hi':(640, 256),
            'amiga_rtg_800':  (800, 600), 'amiga_rtg_1024': (1024, 768),
            'amiga_rtg_pal':  (720, 576), 'amiga_rtg_ntsc': (720, 480),
            'amiga_aga':   (320, 256), 'amiga_ham':   (320, 256),
            'amiga_ham8':  (320, 256), 'amiga_rtg':   (640, 480),
            'plus4':       (320, 200), 'vic20':       (176, 184),
            'nes':         (256, 240), 'snes':        (256, 224),
            'game_boy':    (160, 144), 'game_boy_pocket': (160, 144),
            'game_boy_color': (160, 144), 'game_boy_advance': (240, 160),
            'nimbus':      (320, 250), 'nimbus_hi':   (640, 250),
            'nes':         (256, 240), 'snes':        (256, 224),
            'game_boy':    (160, 144), 'game_boy_pocket': (160, 144),
            'game_boy_color': (160, 144), 'game_boy_advance': (240, 160),
            'sg1000':      (256, 192), 'master_sys':  (256, 192),
            'mega_drive':  (320, 224), 'game_gear':   (160, 144),
            'pc_engine':   (256, 240), 'nimbus_hi': (640, 250),
        }
        if mode in _plat_res and self.dp5_canvas:
            pw, ph = _plat_res[mode]
            if (pw, ph) != (self._canvas_width, self._canvas_height):
                from PIL import Image
                img = Image.frombytes('RGBA',
                    (self._canvas_width, self._canvas_height),
                    bytes(self.dp5_canvas.rgba))
                img = img.resize((pw, ph), Image.LANCZOS)
                self._canvas_width  = pw
                self._canvas_height = ph
                self.dp5_canvas.tex_w = pw
                self.dp5_canvas.tex_h = ph
                self.dp5_canvas.rgba  = bytearray(img.tobytes())
                self.dp5_canvas.update()

        if self.dp5_canvas and mode != 'none':
            self._fit_canvas_to_viewport()
        self._set_status(
            f"Platform: {mode.upper()}  {self._canvas_width}×{self._canvas_height}  cell {cw}×{ch}")


    def _set_canvas_mode(self, mode: str, confirm: bool = True, apply: bool = True): #vers 2
        """Switch canvas mode — platform/texture/icon/free."""
        if mode == self._canvas_mode and self._mode_locked:
            return
        if confirm and self._mode_locked and self.dp5_canvas:
            has_content = any(self.dp5_canvas.rgba[i] != 128
                              for i in range(0, min(len(self.dp5_canvas.rgba), 400), 4))
            if has_content:
                reply = QMessageBox.question(
                    self, "Switch Mode",
                    f"Switch to {mode.title()} mode?\n"
                    f"The canvas will be converted to match {mode} constraints.",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
                if reply != QMessageBox.StandardButton.Yes:
                    self._update_mode_buttons()
                    return

        self._canvas_mode  = mode
        self._mode_locked  = (mode != 'free')
        self.dp5_settings.set('canvas_mode', mode)
        self._update_mode_buttons()

        # Only apply to existing canvas when not creating a new one
        if apply and self.dp5_canvas and self._mode_locked:
            rgba = bytes(self.dp5_canvas.rgba)
            if len(rgba) == self._canvas_width * self._canvas_height * 4:
                self._apply_mode_to_canvas(mode)

        self._set_status(f"Mode: {mode.title()}")


    def _update_mode_buttons(self): #vers 1
        """Sync toolbar mode button checked states."""
        for m, btn in self._mode_btns.items():
            btn.setChecked(m == self._canvas_mode)


    def _apply_mode_to_canvas(self, mode: str): #vers 1
        """Convert current canvas to match mode constraints."""
        if not self.dp5_canvas: return
        from PIL import Image
        rgba = bytes(self.dp5_canvas.rgba)
        w, h = self._canvas_width, self._canvas_height
        img = Image.frombytes('RGBA', (w, h), rgba)

        if mode == 'platform':
            plat = self._platform_mode
            plat_res = {
                'c64':      (320, 200), 'c64m':     (160, 200),
                'spectrum': (256, 192), 'zx80': (256, 192), 'zx81': (256, 192), 'specnext': (320, 256),
                'msx':      (256, 192), 'cpc':      (160, 200),
                'cpc1':     (320, 200), 'atari_st': (320, 200),
                'amiga':    (320, 256), 'amiga_aga':(320, 256),
                'plus4':    (320, 200), 'vic20':    (176, 184),
            }
            if plat in plat_res:
                pw, ph = plat_res[plat]
                img = img.resize((pw, ph), Image.LANCZOS)
                self._canvas_width  = pw
                self._canvas_height = ph
            # Snap to platform palette then apply cell constraints
            img = self._snap_image_to_platform_palette(img)
            self._canvas_bit_depth = 3

        elif mode == 'texture':
            # Snap to nearest power-of-2 size
            import math
            pw = 2 ** round(math.log2(max(w, 1)))
            ph = 2 ** round(math.log2(max(h, 1)))
            if pw != w or ph != h:
                img = img.resize((pw, ph), Image.LANCZOS)
                self._canvas_width  = pw
                self._canvas_height = ph
            # Apply current bit depth
            depth = self._canvas_bit_depth
            if depth == 2:   # 16-bit
                rgb = img.convert('RGB'); px = rgb.tobytes()
                buf = bytearray(len(px))
                for i in range(0, len(px), 3):
                    buf[i]  = (px[i]   >> 3) << 3
                    buf[i+1]= (px[i+1] >> 2) << 2
                    buf[i+2]= (px[i+2] >> 3) << 3
                img = Image.frombytes('RGB',(pw,ph),bytes(buf)).convert('RGBA')
            elif depth == 3: # 8-bit
                img = img.convert('RGB').quantize(colors=256).convert('RGB').convert('RGBA')

        elif mode == 'icon':
            # Snap to nearest standard icon size
            ICON_SIZES = [16, 32, 48, 64, 128, 256]
            best = min(ICON_SIZES, key=lambda s: abs(s - max(w, h)))
            if best != w or best != h:
                img = img.resize((best, best), Image.LANCZOS)
                self._canvas_width  = best
                self._canvas_height = best

        self.dp5_canvas.tex_w = self._canvas_width
        self.dp5_canvas.tex_h = self._canvas_height
        self.dp5_canvas.rgba  = bytearray(img.tobytes())
        self.dp5_canvas.update()
        self._fit_canvas_to_viewport()


    def _toggle_cell_grid(self): #vers 1
        if not self.dp5_canvas: return
        self.dp5_canvas.show_cell_grid = not self.dp5_canvas.show_cell_grid
        self.dp5_canvas.update()


    def _toggle_statusbar(self, on: bool): #vers 1
        self.dp5_settings.set('show_statusbar', on)
        self.dp5_settings.save()
        if hasattr(self, '_status_bar'):
            self._status_bar.setVisible(on)


    def _zoom_mode_menu(self, pos): #vers 1
        """Right-click context menu on zoom gadget — select zoom mode."""
        menu = QMenu(self)
        modes = [
            ('in',  '🔍  Zoom In      (click to zoom in 2×)'),
            ('out', '🔎  Zoom Out     (click to zoom out ½×)'),
            ('box', '⬚   Box Zoom     (drag to zoom to selection)'),
            ('fit', '⊡   Zoom to Fit  (click to fit canvas in view)'),
        ]
        current = getattr(self.dp5_canvas, '_zoom_mode', 'in') if self.dp5_canvas else 'in'
        for mode_id, label in modes:
            a = menu.addAction(label)
            a.setCheckable(True)
            a.setChecked(mode_id == current)
            a.triggered.connect(lambda _, m=mode_id: self._set_zoom_mode(m))
        btn = self._tool_btns.get(TOOL_ZOOM)
        menu.exec(btn.mapToGlobal(pos) if btn else self.cursor().pos())


    def _set_zoom_mode(self, mode: str): #vers 1
        """Set the zoom tool sub-mode and update tooltip."""
        if self.dp5_canvas:
            self.dp5_canvas._zoom_mode = mode
        labels = {'in':'Zoom In','out':'Zoom Out','box':'Box Zoom','fit':'Fit'}
        tips = {
            'in':  'Zoom In — click to zoom in 2×\nRight-click to change mode',
            'out': 'Zoom Out — click to zoom out ½×\nRight-click to change mode',
            'box': 'Box Zoom — drag a rectangle to zoom to it\nRight-click to change mode',
            'fit': 'Zoom to Fit — click to fit canvas in view\nRight-click to change mode',
        }
        btn = self._tool_btns.get(TOOL_ZOOM)
        if btn:
            btn.setToolTip(tips[mode])
        self._set_status(f"Zoom mode: {labels[mode]}")
        # Also activate the zoom tool
        self._select_tool(TOOL_ZOOM)


    #    Colour Clash Visualiser                                                

    def _toggle_clash_visualiser(self, on: bool): #vers 1
        """Toggle ZX Spectrum colour clash overlay — red = more than 2 colours in 8×8 cell."""
        if self.dp5_canvas:
            self.dp5_canvas._show_clash = on
            self.dp5_canvas.update()
        if on:
            self._set_status("Clash visualiser ON — red cells have >2 colours")
        else:
            self._set_status("Clash visualiser OFF")


    #    Character / Font Editor                                                

    def _open_char_editor(self): #vers 2
        """Toggle character/font editor floating panel."""
        if not hasattr(self, "_char_editor_panel") or \
                not self._char_editor_panel.isVisible():
            self._char_editor_panel = _CharFontEditor(self)
            # Position to right of DP5 window
            pos = self.mapToGlobal(self.rect().topRight())
            self._char_editor_panel.move(pos.x() + 6, pos.y())
            self._char_editor_panel.show()
            self._char_editor_panel.raise_()
        else:
            self._char_editor_panel.hide()


    #    Sprite Editor                                                          

    def _open_sprite_editor(self): #vers 2
        """Toggle sprite editor floating panel."""
        if not hasattr(self, "_sprite_editor_panel") or \
                not self._sprite_editor_panel.isVisible():
            self._sprite_editor_panel = _SpriteEditor(self)
            pos = self.mapToGlobal(self.rect().topRight())
            self._sprite_editor_panel.move(pos.x() + 6, pos.y() + 60)
            self._sprite_editor_panel.show()
            self._sprite_editor_panel.raise_()
        else:
            self._sprite_editor_panel.hide()


    def _open_icon_editor(self): #vers 1
        """Toggle icon editor floating panel."""
        if not hasattr(self, "_icon_editor_panel") or \
                not self._icon_editor_panel.isVisible():
            self._icon_editor_panel = _IconEditor(self)
            pos = self.mapToGlobal(self.rect().topRight())
            self._icon_editor_panel.move(pos.x() + 6, pos.y() + 120)
            self._icon_editor_panel.show()
            self._icon_editor_panel.raise_()
        else:
            self._icon_editor_panel.hide()

    def _toggle_anim_strip(self, on: bool): #vers 1
        self.dp5_settings.set('show_anim_strip', on)
        self.dp5_settings.save()
        if hasattr(self, '_anim_strip'):
            self._anim_strip.setVisible(on)
            if on and not self._frames:
                self._anim_init_frames()

    def _toggle_onion_skin(self, on: bool): #vers 1
        if self.dp5_canvas:
            self.dp5_canvas.onion_skin = on
            self.dp5_canvas.onion_rgba = (
                bytearray(self._frames[max(0, self._current_frame-1)])
                if on and len(self._frames) > 1 else None)
            self.dp5_canvas.update()

    def _toggle_colour_constraints(self): #vers 1
        """Toggle enforcement of per-cell colour limits for current platform."""
        self._enforce_constraints = not getattr(self, '_enforce_constraints', False)
        self._set_status(f"Colour constraints: {'ON' if self._enforce_constraints else 'OFF'}")

    def _place_text_at(self, tx: int, ty: int): #vers 2
        """Show inline text input overlay on canvas at click position."""
        if not self.dp5_canvas: return
        ed = self
        z = self.dp5_canvas.zoom

        # Create floating text entry if not already visible
        if hasattr(self, '_text_overlay') and self._text_overlay and \
                self._text_overlay.isVisible():
            self._text_overlay.close()

        overlay = _CanvasTextOverlay(self, tx, ty, z, self.dp5_canvas)
        self._text_overlay = overlay
        # Position the overlay widget over the canvas at the click point
        canvas_widget = self.dp5_canvas
        px = int(tx * z)
        py = int(ty * z)
        # Map to scroll area viewport coords
        sa = getattr(self, '_canvas_scroll', None)
        if sa:
            px -= sa.horizontalScrollBar().value()
            py -= sa.verticalScrollBar().value()
        pos = canvas_widget.mapTo(self, canvas_widget.pos())
        overlay.move(pos.x() + px, pos.y() + py)
        overlay.show()
        overlay.setFocus()

    def _toggle_brush_manager(self): #vers 1
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

    def _on_brush_mgr_selected(self, buf: bytearray, w: int, h: int): #vers 1
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

    def _activate_stamp_mode(self): #vers 1
        """Switch to stamp tool so user clicks anywhere to place the buffer."""
        if not self.dp5_canvas or not self.dp5_canvas._sel_buffer:
            return
        self._select_tool(TOOL_STAMP)
        self._set_status("Stamp mode — click to place, press Esc to exit")

    def _clear_brush(self): #vers 1
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

    def _sync_brush_thumb(self): #vers 1
        """Update the brush thumbnail from the current copy buffer."""
        if not hasattr(self, '_brush_thumb') or not self.dp5_canvas:
            return
        c = self.dp5_canvas
        self._brush_thumb.set_buffer(c._sel_buffer, c._sel_buf_w, c._sel_buf_h)

    def _cut_selection(self): #vers 1
        if not self.dp5_canvas: return
        self._push_undo()
        self.dp5_canvas.cut_selection()
        self._sync_brush_thumb()
        self._set_status("Selection cut — click Brush thumbnail to stamp")

    def _copy_selection(self): #vers 1
        if not self.dp5_canvas: return
        self.dp5_canvas.copy_selection()
        self._sync_brush_thumb()
        self._set_status("Selection copied — click Brush thumbnail to stamp")

    def _paste_selection(self): #vers 1
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

    def _rotate_selection_dialog(self): #vers 2
        """Rotate the active selection — inline overlay panel."""
        if not self.dp5_canvas: return
        c = self.dp5_canvas
        if not (c._sel_active and c._selection_rect):
            self._set_status("No selection to rotate — use Select tool first")
            return
        if not c._sel_buffer:
            c.copy_selection()
        if not c._sel_buffer:
            return

        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout,
            QPushButton, QDoubleSpinBox, QLabel, QSlider, QCheckBox)

        ctrl = QWidget()
        cl = QVBoxLayout(ctrl); cl.setContentsMargins(0,0,0,0); cl.setSpacing(4)

        cl.addWidget(QLabel("Rotation angle (°):"))
        deg_spin = QDoubleSpinBox()
        deg_spin.setRange(-359.9, 359.9); deg_spin.setDecimals(1)
        deg_spin.setSingleStep(1.0); deg_spin.setValue(0.0); deg_spin.setSuffix("°")
        cl.addWidget(deg_spin)

        sl = QSlider(Qt.Orientation.Horizontal)
        sl.setRange(-180, 180); sl.setValue(0)
        sl.valueChanged.connect(lambda v: deg_spin.setValue(float(v)))
        deg_spin.valueChanged.connect(lambda v: sl.setValue(int(v)))
        cl.addWidget(sl)

        preset_row = QHBoxLayout()
        for label, angle in [("−90°",-90),("−45°",-45),("+45°",45),("+90°",90),("180°",180)]:
            b = QPushButton(label); b.setFixedHeight(24)
            b.clicked.connect(lambda _=False, a=angle: deg_spin.setValue(float(a)))
            preset_row.addWidget(b)
        cl.addLayout(preset_row)

        expand_cb = QCheckBox("Expand to fit")
        expand_cb.setChecked(True)
        cl.addWidget(expand_cb)

        parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

        def _apply():
            angle = deg_spin.value()
            if angle != 0.0:
                self._apply_selection_rotation(angle, expand=expand_cb.isChecked())
                self._set_status(f"Selection rotated {angle}°")

        self._ToolOverlay(parent_vp, self, "Rotate Selection",
                          ctrl, apply_fn=_apply, generate_fn=None)

    def _apply_selection_rotation(self, angle: float, expand: bool = True): #vers 1
        """Rotate the selection buffer by angle degrees (CCW positive).
        Updates _sel_buffer and re-floats the selection."""
        c = self.dp5_canvas
        if not c._sel_buffer or c._sel_buf_w <= 0:
            return
        try:
            from PIL import Image
            w, h = c._sel_buf_w, c._sel_buf_h
            img = Image.frombytes('RGBA', (w, h), bytes(c._sel_buffer))
            # PIL rotate: positive = CCW; we want CW for consistency with _rotate_90_cw
            rotated = img.rotate(-angle, expand=expand,
                                 resample=Image.Resampling.BILINEAR,
                                 fillcolor=(0, 0, 0, 0))
            nw, nh = rotated.size
            c._sel_buffer = bytearray(rotated.tobytes())
            c._sel_buf_w  = nw
            c._sel_buf_h  = nh
            # Float the selection at its original position
            if c._selection_rect and not c._sel_floating:
                ox = c._selection_rect.x() + (c._selection_rect.width()  - nw) // 2
                oy = c._selection_rect.y() + (c._selection_rect.height() - nh) // 2
                c._sel_float_pos = (max(0, ox), max(0, oy))
            c._sel_floating = True
            c._sel_active   = True
            c._selection_rect = None   # float replaces rect
            c.update()
            self._set_status(f"Selection rotated {angle:+.1f}°  —  stamp with TOOL_MOVE or ✓")
        except ImportError:
            self._set_status("PIL (Pillow) not available — install with: pip install Pillow")
        except Exception as e:
            self._set_status(f"Rotate error: {e}")


    def _deselect(self): #vers 1
        if not self.dp5_canvas: return
        c = self.dp5_canvas
        if c._sel_floating:
            c._stamp_selection(keep_floating=False)
        c._sel_active        = False
        c._sel_floating      = False
        c._sel_drag_start    = None
        c._selection_rect    = None
        c.update()

    def _set_polygon_sides(self): #vers 1
        if not self.dp5_canvas: return
        n, ok = QInputDialog.getInt(self, "Polygon", "Number of sides:", 6, 3, 32)
        if ok:
            self.dp5_canvas._polygon_sides = n
            self._set_status(f"Polygon: {n} sides")


    #    Canvas signal callbacks                                                

    def _on_canvas_changed(self, x: int, y: int): #vers 1
        if self.dp5_canvas:
            self._update_status(x, y, self.dp5_canvas.get_pixel(x, y))
            if self._enforce_constraints and self._platform_mode != 'none':
                # Debounce — only apply constraint after mouse settles
                if not hasattr(self, '_constraint_timer'):
                    from PyQt6.QtCore import QTimer
                    self._constraint_timer = QTimer()
                    self._constraint_timer.setSingleShot(True)
                    self._constraint_timer.timeout.connect(self._apply_pending_constraint)
                self._constraint_pending = (x, y)
                self._constraint_timer.start(80)  # 80ms debounce

    def _apply_pending_constraint(self): #vers 1
        if hasattr(self, '_constraint_pending') and self.dp5_canvas:
            x, y = self._constraint_pending
            self._apply_cell_constraint(x, y)

    # Platform palettes for constraint snapping
    _ZX_PALETTE = [
        (0,0,0),(0,0,215),(215,0,0),(215,0,215),
        (0,215,0),(0,215,215),(215,215,0),(215,215,215),
        (0,0,0),(0,0,255),(255,0,0),(255,0,255),
        (0,255,0),(0,255,255),(255,255,0),(255,255,255),
    ]
    _C64_PALETTE = [
        (0,0,0),(255,255,255),(136,0,0),(170,255,238),
        (204,68,204),(0,204,85),(0,0,170),(238,238,119),
        (221,136,85),(102,68,0),(255,119,119),(51,51,51),
        (119,119,119),(170,255,102),(0,136,255),(187,187,187),
    ]
    _CPC_PALETTE = [
        (0,0,0),(0,0,128),(0,0,255),(128,0,0),(128,0,128),(128,0,255),
        (255,0,0),(255,0,128),(255,0,255),(0,128,0),(0,128,128),(0,128,255),
        (128,128,0),(128,128,128),(128,128,255),(255,128,0),(255,128,128),(255,128,255),
        (0,255,0),(0,255,128),(0,255,255),(128,255,0),(128,255,128),(128,255,255),
        (255,255,0),(255,255,128),(255,255,255),
    ]
    _MSX_PALETTE = [
        (0,0,0),(0,0,0),(62,184,73),(116,208,128),
        (89,85,224),(128,118,241),(185,94,81),(101,219,239),
        (219,101,89),(255,137,125),(204,195,94),(222,208,135),
        (58,162,65),(183,102,181),(204,204,204),(255,255,255),
    ]
    _ATARI_ST_PALETTE = [
        (255,255,255),(255,0,0),(0,255,0),(255,255,0),
        (0,0,255),(255,0,255),(0,255,255),(187,187,187),
        (136,136,136),(255,136,136),(136,255,136),(255,255,136),
        (136,136,255),(255,136,255),(136,255,255),(0,0,0),
    ]

    # Full 256-colour Atari 8-bit GTIA palette (from image reference)
    _ATARI_800_PALETTE = [
        (6,6,6),(6,6,6),(17,17,17),(34,34,34),(47,47,47),(61,61,61),(79,79,79),(99,99,99),
        (106,106,106),(122,122,122),(142,142,142),(163,163,163),(180,180,180),(199,199,199),(225,225,225),(244,244,244),
        (11,1,0),(22,12,3),(38,25,8),(53,42,10),(68,55,11),(85,70,15),(102,87,18),(124,108,21),
        (131,114,22),(148,131,23),(167,152,27),(185,170,25),(209,192,26),(226,209,31),(237,231,45),(240,239,60),
        (30,0,2),(44,6,3),(62,12,5),(80,27,13),(95,42,11),(109,54,13),(128,69,9),(145,85,12),
        (156,95,15),(176,114,15),(198,130,19),(216,150,28),(229,169,39),(236,186,55),(238,211,72),(237,228,87),
        (45,5,5),(58,7,6),(76,5,1),(90,10,3),(109,23,8),(128,38,14),(149,54,24),(165,68,35),
        (174,74,40),(193,90,55),(215,111,72),(231,129,91),(232,147,108),(232,163,122),(232,183,140),(234,203,157),
        (46,0,10),(64,1,22),(78,3,34),(95,2,47),(110,10,60),(128,24,75),(151,39,95),(170,55,114),
        (175,63,121),(195,77,135),(217,95,156),(225,111,173),(224,127,194),(225,143,209),(228,165,222),(227,180,224),
        (41,3,76),(53,5,91),(69,0,107),(86,0,125),(99,7,140),(114,20,158),(135,33,177),(152,46,194),
        (157,53,202),(177,68,213),(198,85,216),(216,101,218),(220,118,218),(221,133,220),(221,151,221),(222,169,223),
        (21,0,137),(36,3,156),(51,3,175),(64,5,193),(79,11,210),(92,21,213),(111,36,217),(127,48,217),
        (132,54,216),(151,68,218),(170,85,215),(184,101,215),(202,115,214),(215,131,216),(214,149,215),(213,165,213),
        (7,5,168),(11,1,184),(26,0,209),(36,6,218),(49,17,216),(59,27,214),(77,41,213),(89,55,211),
        (100,62,210),(115,75,210),(134,93,213),(148,107,213),(163,121,207),(179,139,210),(196,156,208),(207,173,208),
        (5,5,161),(2,4,177),(8,9,198),(11,20,213),(22,28,212),(33,39,211),(47,58,212),(59,70,211),
        (69,77,212),(82,88,210),(97,106,209),(112,120,205),(128,135,205),(142,154,206),(159,172,204),(177,186,203),
        (4,0,111),(0,10,131),(0,21,148),(7,31,163),(4,44,176),(14,56,194),(22,74,210),(33,85,205),
        (38,91,203),(48,106,205),(68,125,204),(81,139,203),(93,154,201),(108,169,198),(128,188,198),(142,198,199),
        (4,12,48),(4,24,59),(0,33,71),(1,46,88),(0,61,100),(7,74,116),(13,92,135),(19,102,146),
        (25,109,153),(35,124,168),(52,143,188),(62,155,199),(73,171,198),(88,190,195),(105,196,199),(121,197,197),
        (4,22,8),(0,34,9),(5,50,7),(0,59,13),(7,73,25),(0,88,37),(1,105,54),(13,116,63),
        (21,124,69),(35,138,82),(49,158,101),(57,169,111),(70,186,125),(86,198,140),(103,198,158),(117,199,176),
        (1,27,2),(1,39,2),(6,54,4),(3,62,4),(9,77,0),(7,92,0),(18,111,5),(25,126,8),
        (30,130,8),(43,144,14),(54,165,26),(68,179,38),(80,193,49),(97,197,62),(113,198,77),(130,199,92),
        (1,24,6),(5,36,2),(0,52,4),(5,64,0),(7,75,0),(19,89,0),(31,109,0),(45,125,0),
        (51,131,6),(63,145,0),(81,162,7),(94,177,7),(107,195,0),(125,204,14),(140,203,26),(158,203,40),
        (4,15,0),(1,27,0),(11,41,3),(21,55,4),(31,67,5),(43,80,2),(57,96,3),(73,112,5),
        (80,120,5),(96,136,1),(111,155,0),(129,171,9),(142,187,0),(159,203,6),(178,208,12),(196,208,20),
        (8,4,1),(20,13,3),(34,28,4),(49,41,4),(61,55,7),(76,68,6),(92,85,4),(109,100,7),
        (116,107,6),(128,121,7),(147,138,7),(167,157,8),(182,175,6),(200,191,0),(211,212,14),(213,214,25),
    ]

    def _nearest_in_palette(self, r: int, g: int, b: int, palette: list) -> tuple: #vers 1
        return min(palette, key=lambda c:(c[0]-r)**2+(c[1]-g)**2+(c[2]-b)**2)


    def _nearest_zx_colour(self, r: int, g: int, b: int) -> tuple: #vers 1
        return self._nearest_in_palette(r, g, b, self._ZX_PALETTE)


    def _snap_cell_to_palette(self, cx, cy, cw, ch, w, h, palette): #vers 1
        """Snap all pixels in cell to nearest colour from given palette."""
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                r,g,b = self.dp5_canvas.rgba[i:i+3]
                best = self._nearest_in_palette(r, g, b, palette)
                self.dp5_canvas.rgba[i:i+3] = list(best)


    def _limit_cell_colours(self, cx, cy, cw, ch, w, h, max_c): #vers 1
        """After palette snap, enforce max_c colours per cell."""
        colours = {}
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                key = tuple(self.dp5_canvas.rgba[i:i+3])
                colours[key] = colours.get(key, 0) + 1
        if len(colours) <= max_c: return
        kept = sorted(colours, key=lambda k: -colours[k])[:max_c]
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                key = tuple(self.dp5_canvas.rgba[i:i+3])
                if key not in kept:
                    best = min(kept, key=lambda k:(k[0]-key[0])**2+(k[1]-key[1])**2+(k[2]-key[2])**2)
                    self.dp5_canvas.rgba[i:i+3] = list(best)


    def _apply_cell_constraint(self, px: int, py: int): #vers 3
        """Dispatch platform-specific colour constraint for the cell at (px,py)."""
        if not self.dp5_canvas: return
        cw, ch, max_c = self._PLATFORM_CELLS.get(self._platform_mode, (1,1,256))
        if max_c >= 256: return
        cx = (px // cw) * cw
        cy = (py // ch) * ch
        w = self.dp5_canvas.tex_w
        h = self.dp5_canvas.tex_h
        mode = self._platform_mode

        if mode in ('spectrum', 'spectrum128', 'timex', 'pentagon'):
            self._apply_spectrum_clash(cx, cy, cw, ch, w, h)
        elif mode in ('zx80', 'jupiter'):
            # Hard B&W — ZX80/Jupiter Ace character cell mode
            for dy in range(ch):
                for dx in range(cw):
                    tx, ty = cx+dx, cy+dy
                    if not (0 <= tx < w and 0 <= ty < h): continue
                    i = (ty*w+tx)*4
                    r,g,b = self.dp5_canvas.rgba[i:i+3]
                    lum = int(0.299*r + 0.587*g + 0.114*b)
                    v = 255 if lum >= 128 else 0
                    self.dp5_canvas.rgba[i:i+3] = [v, v, v]
            self.dp5_canvas.update()
        elif mode in ('zx81', 'timex_hi'):
            # Bayer dither — ZX81 WRX / Timex HiRes B&W modes
            self._apply_zx8x_dither(cx, cy, cw, ch, w, h)
        elif mode in ('c64', 'c64m'):
            self._snap_cell_to_palette(cx, cy, cw, ch, w, h, self._C64_PALETTE)
            self._limit_cell_colours(cx, cy, cw, ch, w, h, max_c)
        elif mode in ('cpc', 'cpc1'):
            self._snap_cell_to_palette(cx, cy, cw, ch, w, h, self._CPC_PALETTE)
            self._limit_cell_colours(cx, cy, cw, ch, w, h, max_c)
        elif mode == 'msx':
            self._apply_msx_constraint(cx, cy, cw, ch, w, h)
        elif mode == 'atari_st':
            self._apply_atari_st_constraint(cx, cy, cw, ch, w, h)
        elif mode == 'atari_800':
            self._snap_cell_to_palette(cx, cy, cw, ch, w, h, self._ATARI_800_PALETTE)
            self._limit_cell_colours(cx, cy, cw, ch, w, h, max_c)
        elif mode in ('plus4', 'vic20'):
            # Plus/4 and VIC-20: snap to C64-like palette, 2 colours per cell
            self._snap_cell_to_palette(cx, cy, cw, ch, w, h, self._C64_PALETTE)
            self._limit_cell_colours(cx, cy, cw, ch, w, h, max_c)
        elif mode == 'amiga':
            # Amiga OCS: snap to 32-colour OCS palette (no per-cell limit)
            amiga_ocs = [
                (0,0,0),(255,255,255),(170,0,0),(85,255,255),
                (170,0,170),(85,255,85),(0,0,170),(255,255,85),
                (170,85,0),(85,85,0),(255,119,119),(85,85,85),
                (119,119,119),(170,255,170),(85,136,255),(170,170,170),
                (0,0,0),(17,17,17),(34,34,34),(51,51,51),
                (68,68,68),(85,85,85),(102,102,102),(119,119,119),
                (136,136,136),(153,153,153),(170,170,170),(187,187,187),
                (204,204,204),(221,221,221),(238,238,238),(255,255,255),
            ]
            self._snap_cell_to_palette(cx, cy, cw, ch, w, h, amiga_ocs)
        elif mode == 'amiga_aga':
            # AGA 256 colour: no per-cell constraint, free palette
            pass
        elif mode in ('amiga_ham', 'amiga_ham8'):
            # HAM: simulate hold-and-modify smearing on scanlines touching this cell
            self._apply_ham_constraint(cx, cy, cw, ch, w, h, mode)
        elif mode == 'amiga_rtg':
            # RTG chunky: snap to AGA WB palette, no constraint
            pass
        else:
            self._apply_generic_constraint(cx, cy, cw, ch, w, h, max_c)

        self.dp5_canvas.update()


    def _get_user_palette_rgb(self): #vers 1
        """Return current user palette as list of (r,g,b) tuples, or None if empty."""
        if not hasattr(self, '_user_pal_grid'): return None
        colors = getattr(self._user_pal_grid, '_colors', [])
        if not colors: return None
        return [(c.red(), c.green(), c.blue()) for c in colors if c.isValid()]


    def _snap_image_to_user_palette(self, img): #vers 1
        """Snap every pixel in a PIL RGBA image to nearest colour in the user palette."""
        from PIL import Image
        palette = self._get_user_palette_rgb()
        if not palette:
            QMessageBox.warning(self, "Snap to User Palette",
                                "No user palette loaded. Load a palette first.")
            return img
        n = min(len(palette), 256)
        pal_img = Image.new('P', (1, 1))
        flat = []
        for r, g, b in palette[:n]:
            flat += [r, g, b]
        flat += [0] * (768 - len(flat))
        pal_img.putpalette(flat)
        return img.convert('RGB').quantize(palette=pal_img, dither=0).convert('RGB').convert('RGBA')


    #    Render As                                                             

    def _render_as_ascii(self): #vers 1
        """Convert canvas to ASCII art — map brightness to characters, render back to canvas."""
        if not self.dp5_canvas: return
        from PIL import Image, ImageDraw, ImageFont

        # ASCII ramp — dark to light
        RAMP = ' .\'`^",:;Il!i><~+_-?][}{1)(|/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$'

        cols, ok = QInputDialog.getInt(self, "ASCII Art", "Characters wide:", 80, 10, 320)
        if not ok: return

        self._push_undo()
        from PIL import Image
        src = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                              bytes(self.dp5_canvas.rgba)).convert('L')

        char_w, char_h = 6, 10   # monospace character size in pixels
        rows = int(cols * (self._canvas_height / self._canvas_width) * (char_w / char_h))
        rows = max(4, rows)

        # Resize to character grid
        small = src.resize((cols, rows), Image.LANCZOS)
        pixels = list(small.getdata())

        # Map brightness to character
        chars = []
        for px in pixels:
            idx = int(px / 255 * (len(RAMP)-1))
            chars.append(RAMP[idx])

        # Render back to canvas as pixel art
        cell_pw = self._canvas_width  // cols
        cell_ph = self._canvas_height // rows
        cell_pw = max(1, cell_pw); cell_ph = max(1, cell_ph)

        out_w = cols * cell_pw
        out_h = rows * cell_ph
        out = Image.new('RGB', (out_w, out_h), (20, 20, 20))
        draw = ImageDraw.Draw(out)

        try:
            font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", cell_ph)
        except Exception:
            font = ImageFont.load_default()

        for i, ch in enumerate(chars):
            col = i % cols; row = i // cols
            x = col * cell_pw; y = row * cell_ph
            # Brightness-based grey
            brightness = int(list(small.getdata())[i])
            grey = (brightness, brightness, brightness)
            draw.text((x, y), ch, fill=grey, font=font)

        out_rgba = out.convert('RGBA')
        self._canvas_width  = out_w
        self._canvas_height = out_h
        self.dp5_canvas.tex_w = out_w
        self.dp5_canvas.tex_h = out_h
        self.dp5_canvas.rgba  = bytearray(out_rgba.tobytes())
        self.dp5_canvas.update()
        self._fit_canvas_to_viewport()
        self._set_status(f"ASCII art: {cols}×{rows} chars → {out_w}×{out_h}px")

        # Offer text export
        if QMessageBox.question(self, "Export ASCII", "Export as text file?",
            QMessageBox.StandardButton.Yes|QMessageBox.StandardButton.No) == QMessageBox.StandardButton.Yes:
            path, _ = QFileDialog.getSaveFileName(self,"Save ASCII","art.txt","Text (*.txt)")
            if path:
                lines = [''.join(chars[r*cols:(r+1)*cols]) for r in range(rows)]
                open(path,'w').write('\n'.join(lines))


    def _render_as_ansi(self): #vers 1
        """Convert canvas to ANSI art using block chars █▌▐▄▀ with 16 ANSI colours."""
        if not self.dp5_canvas: return
        from PIL import Image

        cols, ok = QInputDialog.getInt(self, "ANSI Art", "Characters wide:", 80, 10, 320)
        if not ok: return

        self._push_undo()

        # ANSI 16-colour palette (standard terminal)
        ANSI_PAL = [
            (0,0,0),(170,0,0),(0,170,0),(170,170,0),
            (0,0,170),(170,0,170),(0,170,170),(170,170,170),
            (85,85,85),(255,85,85),(85,255,85),(255,255,85),
            (85,85,255),(255,85,255),(85,255,255),(255,255,255),
        ]

        # Block characters — each char cell is split top/bottom half
        # Use upper-half block '▀' with fg=top colour, bg=bottom colour
        # This gives 2 vertical pixels per character row

        char_w = 8; char_h = 8
        rows = max(4, int(cols * (self._canvas_height / self._canvas_width) * (char_w / char_h) * 2))

        src = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                              bytes(self.dp5_canvas.rgba)).convert('RGB')
        # Resize to cols × rows (each row = half-block = 2 pixel rows)
        small = src.resize((cols, rows), Image.LANCZOS)
        pixels = list(small.getdata())

        def nearest_ansi(r,g,b):
            return min(range(16), key=lambda i:(ANSI_PAL[i][0]-r)**2+(ANSI_PAL[i][1]-g)**2+(ANSI_PAL[i][2]-b)**2)

        # Build ANSI escape sequence string
        ansi_lines = []
        char_rows = rows // 2
        ansi_out = []
        for row in range(char_rows):
            line = ''
            ansi_row = []
            for col in range(cols):
                top = pixels[(row*2)*cols+col]
                bot = pixels[(row*2+1)*cols+col] if (row*2+1)*cols+col < len(pixels) else top
                fg = nearest_ansi(*top)
                bg = nearest_ansi(*bot)
                ansi_row.append((fg, bg))
                # ANSI escape: \033[38;5;{fg}m\033[48;5;{bg}m▀
                line += f'\033[38;5;{fg}m\033[48;5;{bg}m▀'
            line += '\033[0m'
            ansi_lines.append(line)
            ansi_out.append(ansi_row)

        # Render to canvas
        from PIL import ImageDraw, ImageFont
        cell_ph = max(4, self._canvas_height // char_rows)
        cell_pw = max(4, self._canvas_width  // cols)
        out_w = cols * cell_pw
        out_h = char_rows * cell_ph
        out = Image.new('RGB', (out_w, out_h), (0,0,0))

        for row, row_data in enumerate(ansi_out):
            for col, (fg, bg) in enumerate(row_data):
                x = col * cell_pw; y = row * cell_ph
                top_c = ANSI_PAL[fg]; bot_c = ANSI_PAL[bg]
                # Top half
                for py in range(cell_ph//2):
                    for px in range(cell_pw):
                        out.putpixel((x+px, y+py), top_c)
                # Bottom half
                for py in range(cell_ph//2, cell_ph):
                    for px in range(cell_pw):
                        out.putpixel((x+px, y+py), bot_c)

        out_rgba = out.convert('RGBA')
        self._canvas_width  = out_w
        self._canvas_height = out_h
        self.dp5_canvas.tex_w = out_w
        self.dp5_canvas.tex_h = out_h
        self.dp5_canvas.rgba  = bytearray(out_rgba.tobytes())
        self.dp5_canvas.update()
        self._fit_canvas_to_viewport()
        self._set_status(f"ANSI art: {cols}×{char_rows} chars → {out_w}×{out_h}px")

        if QMessageBox.question(self, "Export ANSI", "Export as .ans file?",
            QMessageBox.StandardButton.Yes|QMessageBox.StandardButton.No) == QMessageBox.StandardButton.Yes:
            path, _ = QFileDialog.getSaveFileName(self,"Save ANSI","art.ans","ANSI (*.ans)")
            if path:
                open(path,'w').write('\n'.join(ansi_lines)+'\n')


    def _render_as_petscii(self): #vers 1
        """Convert canvas to PETSCII block art using C64 16-colour palette and block chars."""
        if not self.dp5_canvas: return
        from PIL import Image

        cols, ok = QInputDialog.getInt(self, "PETSCII", "Characters wide (max 40):", 40, 10, 80)
        if not ok: return

        self._push_undo()

        C64_PAL = [
            (0,0,0),(255,255,255),(136,0,0),(170,255,238),
            (204,68,204),(0,204,85),(0,0,170),(238,238,119),
            (221,136,85),(102,68,0),(255,119,119),(51,51,51),
            (119,119,119),(170,255,102),(0,136,255),(187,187,187),
        ]

        # PETSCII block chars rendered as 2×2 sub-cell pixel blocks
        # Each char cell = 8×8px, split into 4 quadrants: TL TR BL BR
        # Characters: space=0000, ▘=1000, ▝=0100, ▀=1100,
        #             ▖=0010, ▌=1010, ▞=0110, ▛=1110,
        #             ▗=0001, ▚=1001, ▐=0101, ▜=1101,
        #             ▄=0011, ▙=1011, ▟=0111, █=1111

        BLOCKS = [' ','▘','▝','▀','▖','▌','▞','▛','▗','▚','▐','▜','▄','▙','▟','█']

        char_w = 8; char_h = 8
        rows = max(4, int(cols * (self._canvas_height / self._canvas_width)))

        src = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                              bytes(self.dp5_canvas.rgba)).convert('RGB')
        # Resize to cols*2 × rows*2 (2×2 sub-pixels per char)
        small = src.resize((cols*2, rows*2), Image.LANCZOS)
        pixels = list(small.getdata())
        sw = cols*2

        def nearest_c64(r,g,b):
            return min(range(16), key=lambda i:(C64_PAL[i][0]-r)**2+(C64_PAL[i][1]-g)**2+(C64_PAL[i][2]-b)**2)

        # For each char cell, pick dominant fg/bg from 4 sub-pixels
        cell_data = []  # (fg_idx, bg_idx, block_bits)
        for row in range(rows):
            for col in range(cols):
                # 4 sub-pixels: TL, TR, BL, BR
                quads = [
                    pixels[(row*2)*sw   + col*2],
                    pixels[(row*2)*sw   + col*2+1],
                    pixels[(row*2+1)*sw + col*2],
                    pixels[(row*2+1)*sw + col*2+1],
                ]
                idxs = [nearest_c64(*q) for q in quads]
                # Find 2 most common colours
                from collections import Counter
                counts = Counter(idxs)
                top2 = [c for c,_ in counts.most_common(2)]
                fg = top2[0]; bg = top2[1] if len(top2)>1 else fg
                # Assign bits: 1=fg, 0=bg for TL,TR,BL,BR
                bits = sum((1<<(3-i)) for i,idx in enumerate(idxs) if idx==fg)
                cell_data.append((fg, bg, bits))

        # Render to canvas
        cell_pw = char_w; cell_ph = char_h
        out_w = cols * cell_pw; out_h = rows * cell_ph
        out = Image.new('RGB', (out_w, out_h), (0,0,0))
        px_data = out.load()

        for row in range(rows):
            for col in range(cols):
                fg, bg, bits = cell_data[row*cols+col]
                fc = C64_PAL[fg]; bc = C64_PAL[bg]
                x0 = col*cell_pw; y0 = row*cell_ph
                hw = cell_pw//2; hh = cell_ph//2
                sub = [(bits>>3)&1,(bits>>2)&1,(bits>>1)&1,bits&1]
                quadrants = [(0,0,hw,hh),(hw,0,cell_pw,hh),(0,hh,hw,cell_ph),(hw,hh,cell_pw,cell_ph)]
                for (x1,y1,x2,y2),on in zip(quadrants,sub):
                    c = fc if on else bc
                    for py in range(y1,y2):
                        for px in range(x1,x2):
                            px_data[x0+px, y0+py] = c

        out_rgba = out.convert('RGBA')
        self._canvas_width  = out_w; self._canvas_height = out_h
        self.dp5_canvas.tex_w = out_w; self.dp5_canvas.tex_h = out_h
        self.dp5_canvas.rgba  = bytearray(out_rgba.tobytes())
        self.dp5_canvas.update()
        self._fit_canvas_to_viewport()
        self._set_status(f"PETSCII: {cols}×{rows} chars → {out_w}×{out_h}px")

        if QMessageBox.question(self, "Export PETSCII", "Export as C64 PRG?",
            QMessageBox.StandardButton.Yes|QMessageBox.StandardButton.No) == QMessageBox.StandardButton.Yes:
            path, _ = QFileDialog.getSaveFileName(self,"Save PETSCII PRG","petscii.prg","PRG (*.prg)")
            if path:
                # Screen RAM + colour RAM format (screen codes + colour)
                screen = bytearray(1000); colour = bytearray(1000)
                for i,(fg,bg,bits) in enumerate(cell_data[:1000]):
                    screen[i] = bits  # approximate PETSCII screen code
                    colour[i] = fg & 0xF
                # BASIC stub + screen data at $0400
                prg = b'\x01\x08' + bytearray(14) + b'\x00\x04' + screen + b'\xD8\x07' + colour
                open(path,'wb').write(prg)


    def _render_as_teletext(self): #vers 1
        """Convert canvas to Teletext mosaic block art (2×3 sub-blocks per cell, 8 colours)."""
        if not self.dp5_canvas: return
        from PIL import Image

        cols, ok = QInputDialog.getInt(self, "Teletext", "Characters wide (40=standard):", 40, 10, 80)
        if not ok: return

        self._push_undo()

        # Teletext 8 colours (RGB combinations)
        TT_PAL = [
            (0,0,0),(255,0,0),(0,255,0),(255,255,0),
            (0,0,255),(255,0,255),(0,255,255),(255,255,255),
        ]

        def nearest_tt(r,g,b):
            return min(range(8), key=lambda i:(TT_PAL[i][0]-r)**2+(TT_PAL[i][1]-g)**2+(TT_PAL[i][2]-b)**2)

        # Teletext mosaic: each char cell = 2×3 grid of sub-blocks
        # Cell size: 6×10 pixels (or 12×20 for 2x rendering)
        # 6 bits → 64 possible mosaic characters
        # Sub-block layout:
        #  [0][1]
        #  [2][3]
        #  [4][5]
        rows = max(4, int(cols * (self._canvas_height / self._canvas_width) * (2/3)))

        src = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                              bytes(self.dp5_canvas.rgba)).convert('RGB')
        small = src.resize((cols*2, rows*3), Image.LANCZOS)
        pixels = list(small.getdata())
        sw = cols*2

        cell_data = []  # (fg_idx, bits6)
        for row in range(rows):
            for col in range(cols):
                # 6 sub-pixels (2 wide × 3 tall)
                subs = [
                    pixels[(row*3+sr)*sw + col*2+sc]
                    for sr in range(3) for sc in range(2)
                ]
                idxs = [nearest_tt(*s) for s in subs]
                from collections import Counter
                counts = Counter(idxs)
                fg = counts.most_common(1)[0][0]
                # Bit = 1 if matches fg colour, 0 = background (black)
                bits = sum((1<<i) for i,idx in enumerate(idxs) if idx==fg)
                cell_data.append((fg, bits))

        # Render — cell pixel size
        cell_pw = 12; cell_ph = 20  # 2× for readability
        out_w = cols * cell_pw; out_h = rows * cell_ph
        out = Image.new('RGB', (out_w, out_h), (0,0,0))
        px_data = out.load()

        for row in range(rows):
            for col in range(cols):
                fg, bits = cell_data[row*cols+col]
                fc = TT_PAL[fg]; bc = (0,0,0)
                x0 = col*cell_pw; y0 = row*cell_ph
                bw = cell_pw//2; bh = cell_ph//3
                for si in range(6):
                    sr = si//2; sc = si%2
                    on = (bits>>si)&1
                    c = fc if on else bc
                    for py in range(bh):
                        for px in range(bw):
                            px_data[x0+sc*bw+px, y0+sr*bh+py] = c

        out_rgba = out.convert('RGBA')
        self._canvas_width  = out_w; self._canvas_height = out_h
        self.dp5_canvas.tex_w = out_w; self.dp5_canvas.tex_h = out_h
        self.dp5_canvas.rgba  = bytearray(out_rgba.tobytes())
        self.dp5_canvas.update()
        self._fit_canvas_to_viewport()
        self._set_status(f"Teletext: {cols}×{rows} chars → {out_w}×{out_h}px")

        if QMessageBox.question(self, "Export Teletext", "Export as .tti file?",
            QMessageBox.StandardButton.Yes|QMessageBox.StandardButton.No) == QMessageBox.StandardButton.Yes:
            path, _ = QFileDialog.getSaveFileName(self,"Save Teletext","page.tti","TTI (*.tti)")
            if path:
                # TTI format: simple text with colour codes
                lines = ['OL,100']  # page header
                TT_CODES = ['\\0','\\1','\\2','\\3','\\4','\\5','\\6','\\7']
                for row in range(rows):
                    row_cells = cell_data[row*cols:(row+1)*cols]
                    # Build teletext line with colour changes
                    line_str = ''
                    cur_fg = -1
                    for fg, bits in row_cells:
                        if fg != cur_fg:
                            line_str += TT_CODES[fg]
                            cur_fg = fg
                        # Map 6-bit mosaic to Unicode block char
                        mosaic_char = chr(0x23A0 + bits) if bits > 0 else ' '
                        line_str += mosaic_char
                    lines.append(f'OL,{100+row+1},{line_str}')
                open(path,'w',encoding='utf-8').write('\n'.join(lines)+'\n')


    def _snap_canvas_to_user_palette(self): #vers 3
        """Hard-snap entire canvas to current user palette (no dither)."""
        if not self.dp5_canvas: return
        palette = self._get_user_palette_rgb()
        if not palette:
            QMessageBox.warning(self, "Snap to Palette", "No user palette loaded.")
            return
        self._push_undo()
        from PIL import Image
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.dp5_canvas.rgba))
        snapped = self._snap_image_to_user_palette(img)
        self.dp5_canvas.rgba = bytearray(snapped.tobytes())
        self.dp5_canvas.update()
        self._set_status(f"Snapped to palette: {len(palette)} colours")


    def _snap_canvas_to_user_palette_dither(self): #vers 1
        """Snap canvas to user palette with dither — asks which method."""
        palette = self._get_user_palette_rgb()
        if not palette:
            QMessageBox.warning(self, "Snap + Dither", "No user palette loaded.")
            return
        # Quick pick via input dialog
        method, ok = QInputDialog.getItem(
            self, "Dither Method", "Choose dither:",
            ["Floyd-Steinberg", "Bayer 4×4", "Checkerboard"], 0, False)
        if not ok: return
        mode_map = {"Floyd-Steinberg": "floyd", "Bayer 4×4": "bayer", "Checkerboard": "checker"}
        mode = mode_map[method]
        self._push_undo()
        old_mode = getattr(self, '_pal_dither_mode', 'off')
        self._pal_dither_mode = mode
        from PIL import Image
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.dp5_canvas.rgba))
        snapped = self._apply_user_palette_dither(img)
        self._pal_dither_mode = old_mode
        self.dp5_canvas.rgba = bytearray(snapped.tobytes())
        self.dp5_canvas.update()
        self._set_status(f"Snapped ({mode}): {len(palette)} colours")
        """Snap entire canvas to current user palette, with optional dithering."""
        if not self.dp5_canvas: return
        palette = self._get_user_palette_rgb()
        if not palette:
            QMessageBox.warning(self, "Snap to User Palette", "No user palette loaded.")
            return
        self._push_undo()
        from PIL import Image
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.dp5_canvas.rgba))
        mode = getattr(self, '_pal_dither_mode', 'off')
        if mode != 'off':
            snapped = self._apply_user_palette_dither(img)
        else:
            snapped = self._snap_image_to_user_palette(img)
        self.dp5_canvas.rgba = bytearray(snapped.tobytes())
        self.dp5_canvas.update()
        self._set_status(f"Snapped to user palette ({len(palette)} colours, dither:{mode})")


    def _snap_image_to_platform_palette(self, img): #vers 1
        """Snap every pixel in a PIL RGBA image to the nearest platform palette colour.
        Returns the modified image. Used when loading an image in platform mode."""
        mode = self._platform_mode
        # Get the platform palette as a flat list of (r,g,b) tuples
        pal_map = {
            'c64':       self._C64_PALETTE,
            'c64m':      self._C64_PALETTE,
            'spectrum': self._ZX_PALETTE,
            'zx80':        'threshold_bw',
            'zx81':        'bayer_bw',
            'timex_hi':    'threshold_bw',  # HiRes mode is B&W
            'jupiter':     'threshold_bw',  # Jupiter Ace is B&W
            'specnext':  None,   # 256 colour — no snap needed
            'msx':       self._MSX_PALETTE,
            'cpc':       self._CPC_PALETTE,
            'cpc1':      self._CPC_PALETTE,
            'atari_st':  self._ATARI_ST_PALETTE,
            'atari_800': self._ATARI_800_PALETTE,
            'amiga':     [(0,0,0),(255,255,255),(170,0,0),(85,255,255),
                          (170,0,170),(85,255,85),(0,0,170),(255,255,85),
                          (170,85,0),(85,85,0),(255,119,119),(85,85,85),
                          (119,119,119),(170,255,170),(85,136,255),(170,170,170),
                          (0,0,0),(17,17,17),(34,34,34),(51,51,51),
                          (68,68,68),(85,85,85),(102,102,102),(119,119,119),
                          (136,136,136),(153,153,153),(170,170,170),(187,187,187),
                          (204,204,204),(221,221,221),(238,238,238),(255,255,255)],
            'amiga_aga': 'user',   # 256 colour — snap to user palette if loaded
            'amiga_ham': None,   # handled by HAM constraint
            'amiga_ham8':None,
            'amiga_rtg': None,
            'plus4':     self._C64_PALETTE,
            'vic20':     self._C64_PALETTE,
        }
        palette = pal_map.get(mode)
        if palette is None:
            return img   # no snap for full-colour modes
        if palette == 'user':
            # Use current user palette
            user_pal = self._get_user_palette_rgb()
            if not user_pal: return img
            palette = user_pal
        if palette == 'bayer_bw':
            from PIL import Image as PILImage
            BAYER = [[0,8,2,10],[12,4,14,6],[3,11,1,9],[15,7,13,5]]
            rgb = img.convert('RGB')
            w2, h2 = rgb.size
            out = PILImage.new('RGB', (w2, h2))
            px_in = rgb.load(); px_out = out.load()
            for y in range(h2):
                for x in range(w2):
                    r, g, b = px_in[x, y]
                    lum = int(0.299*r + 0.587*g + 0.114*b)
                    thresh = int(BAYER[y%4][x%4] / 16.0 * 255)
                    v = 255 if lum > thresh else 0
                    px_out[x, y] = (v, v, v)
            return out.convert('RGBA')
        if palette == 'threshold_bw':
            # ZX80: hard threshold — average per 8×8 block then B&W
            from PIL import Image as PILImage
            rgb = img.convert('L')  # greyscale
            out = PILImage.new('L', rgb.size)
            px = rgb.load(); po = out.load()
            w2, h2 = rgb.size
            for y in range(h2):
                for x in range(w2):
                    po[x, y] = 255 if px[x, y] >= 128 else 0
            return out.convert('RGBA')

        from PIL import Image
        rgb = img.convert('RGB')
        px = list(rgb.getdata())
        w, h = rgb.size

        # Build a PIL palette image for fast quantization to platform colours
        n = len(palette)
        pal_img = Image.new('P', (1, 1))
        flat = []
        for r,g,b in palette:
            flat += [r,g,b]
        flat += [0] * (768 - len(flat))
        pal_img.putpalette(flat)

        # Quantize to platform palette (nearest colour, no dither)
        snapped = rgb.quantize(palette=pal_img, dither=0).convert('RGB').convert('RGBA')
        return snapped


    def _apply_spectrum_clash(self, cx, cy, cw, ch, w, h): #vers 1
        """Enforce ZX Spectrum colour clash: max 2 colours per 8×8 cell,
        both snapped to ZX palette, both from same brightness group."""
        # Step 1: snap all pixels in cell to nearest ZX colour
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                r,g,b = self.dp5_canvas.rgba[i:i+3]
                zx = self._nearest_zx_colour(r,g,b)
                self.dp5_canvas.rgba[i:i+3] = list(zx)

        # Step 2: collect unique ZX colours in cell
        colours = {}
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                key = tuple(self.dp5_canvas.rgba[i:i+3])
                colours[key] = colours.get(key, 0) + 1

        if len(colours) <= 2: return

        # Step 3: determine dominant brightness (bright if >half pixels are bright)
        bright_count = 0; total = 0
        for c, cnt in colours.items():
            total += cnt
            idx = self._ZX_PALETTE.index(c) if c in self._ZX_PALETTE else 0
            if idx >= 8: bright_count += cnt
        use_bright = bright_count > total // 2

        # Step 4: restrict palette to 8 colours of correct brightness
        valid = self._ZX_PALETTE[8:16] if use_bright else self._ZX_PALETTE[0:8]

        # Step 5: re-snap all pixels to valid group
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                r,g,b = self.dp5_canvas.rgba[i:i+3]
                best = min(valid, key=lambda c: (c[0]-r)**2+(c[1]-g)**2+(c[2]-b)**2)
                self.dp5_canvas.rgba[i:i+3] = list(best)

        # Step 6: collect again and keep only top 2 (ink + paper)
        colours = {}
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                key = tuple(self.dp5_canvas.rgba[i:i+3])
                colours[key] = colours.get(key, 0) + 1

        if len(colours) <= 2: return
        kept = sorted(colours, key=lambda k: -colours[k])[:2]

        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                key = tuple(self.dp5_canvas.rgba[i:i+3])
                if key not in kept:
                    best = min(kept, key=lambda c: (c[0]-key[0])**2+(c[1]-key[1])**2+(c[2]-key[2])**2)
                    self.dp5_canvas.rgba[i:i+3] = list(best)

        self.dp5_canvas.update()


    def _apply_ham_constraint(self, cx, cy, cw, ch, w, h, mode): #vers 1
        """Simulate Amiga HAM hold-and-modify colour smearing on affected scanlines.

        HAM6: 16 base colours. Each pixel either picks a base colour or modifies
              the R, G, or B component of the previous pixel by 4-bit value.
        HAM8: 256 base colours. Same but 8-bit components.

        We simulate by re-encoding the scanline left-to-right using HAM rules,
        then decoding back to RGBA so the user sees the actual HAM output.
        """
        is_ham8 = (mode == 'amiga_ham8')
        bits = 8 if is_ham8 else 4
        comp_max = (1 << bits) - 1   # 255 for HAM8, 15 for HAM6
        scale = 255 // comp_max       # 1 for HAM8, 17 for HAM6

        # Build base palette from user palette grid colours
        if hasattr(self, '_user_pal_grid') and self._user_pal_grid._colors:
            n_base = 256 if is_ham8 else 16
            base_pal = [(c.red(), c.green(), c.blue())
                        for c in self._user_pal_grid._colors[:n_base]]
            while len(base_pal) < n_base:
                base_pal.append((0, 0, 0))
        else:
            n_base = 256 if is_ham8 else 16
            base_pal = [(i*scale, i*scale, i*scale) for i in range(n_base)]

        def nearest_base(r, g, b):
            return min(base_pal, key=lambda c:(c[0]-r)**2+(c[1]-g)**2+(c[2]-b)**2)

        def ham_encode_decode_row(ty):
            """Re-encode one scanline as HAM and return the decoded RGBA."""
            prev_r, prev_g, prev_b = 0, 0, 0
            out = []
            for tx in range(w):
                if not (0 <= tx < w): continue
                i = (ty*w+tx)*4
                r, g, b = self.dp5_canvas.rgba[i:i+3]
                # Option 1: use a base colour
                bc = nearest_base(r, g, b)
                err_base = (bc[0]-r)**2+(bc[1]-g)**2+(bc[2]-b)**2
                # Option 2: modify R of prev
                mr = (r >> (8-bits)) * scale
                err_r = (mr-r)**2+(prev_g-g)**2+(prev_b-b)**2
                # Option 3: modify G of prev
                mg = (g >> (8-bits)) * scale
                err_g = (prev_r-r)**2+(mg-g)**2+(prev_b-b)**2
                # Option 4: modify B of prev
                mb = (b >> (8-bits)) * scale
                err_b = (prev_r-r)**2+(prev_g-g)**2+(mb-b)**2

                best_err = min(err_base, err_r, err_g, err_b)
                if best_err == err_base:
                    out.append(bc)
                    prev_r, prev_g, prev_b = bc
                elif best_err == err_r:
                    out.append((mr, prev_g, prev_b))
                    prev_r = mr
                elif best_err == err_g:
                    out.append((prev_r, mg, prev_b))
                    prev_g = mg
                else:
                    out.append((prev_r, prev_g, mb))
                    prev_b = mb
            return out

        # Process all scanlines in the affected cell rows
        for dy in range(ch):
            ty = cy + dy
            if not (0 <= ty < h): continue
            decoded = ham_encode_decode_row(ty)
            for tx, c in enumerate(decoded):
                i = (ty*w+tx)*4
                self.dp5_canvas.rgba[i:i+4] = [c[0], c[1], c[2], 255]


    def _apply_zx8x_dither(self, cx, cy, cw, ch, w, h): #vers 1
        """ZX80/ZX81 B&W constraint with Bayer ordered dithering to simulate grey tones.
        Each 8×8 character cell gets dithered using the cell's average brightness.
        The ZX machines used character patterns to fake grey — we simulate that."""
        # 4×4 Bayer threshold matrix (normalised 0-1)
        BAYER = [
            [ 0/16,  8/16,  2/16, 10/16],
            [12/16,  4/16, 14/16,  6/16],
            [ 3/16, 11/16,  1/16,  9/16],
            [15/16,  7/16, 13/16,  5/16],
        ]
        # ZX81 character block patterns for different grey levels
        # These mimic the actual ZX81 character ROM grey patterns
        ZX_PATTERNS = {
            0:   [[0,0,0,0,0,0,0,0]]*8,  # black
            1:   [[0x80,0,0,0,0,0,0,0]]*8,  # ~6%
            2:   [[0x88,0,0,0,0,0,0,0]]*8,  # ~12%
            3:   [[0x88,0,0x22,0,0,0,0,0,0]]*8,
            4:   [[0x88,0,0x22,0]*2]*8,  # ~25% checkerboard-like
            8:   [[0xAA,0x55]*4]*8,  # 50% medium grey
            12:  [[0xFF,0x55,0xFF,0xAA]*2]*8,  # ~75%
            15:  [[0xFF]*8]*8,  # white
        }
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                r,g,b = self.dp5_canvas.rgba[i:i+3]
                # Convert to luminance
                lum = (0.299*r + 0.587*g + 0.114*b) / 255.0
                # Bayer threshold
                thresh = BAYER[dy%4][dx%4]
                pixel_on = lum > thresh
                val = 255 if pixel_on else 0
                self.dp5_canvas.rgba[i:i+3] = [val, val, val]
        self.dp5_canvas.update()


    def _apply_msx_constraint(self, cx, cy, cw, ch, w, h): #vers 1
        """MSX1: 2 colours per 8-pixel row within each 8×8 cell (fg+bg per scanline)."""
        # First snap all to MSX palette
        self._snap_cell_to_palette(cx, cy, cw, ch, w, h, self._MSX_PALETTE)
        # Then enforce 2 colours per row
        for dy in range(ch):
            ty = cy + dy
            if not (0 <= ty < h): continue
            row_colours = {}
            for dx in range(cw):
                tx = cx + dx
                if not (0 <= tx < w): continue
                i = (ty*w+tx)*4
                key = tuple(self.dp5_canvas.rgba[i:i+3])
                row_colours[key] = row_colours.get(key, 0) + 1
            if len(row_colours) <= 2: continue
            kept = sorted(row_colours, key=lambda k: -row_colours[k])[:2]
            for dx in range(cw):
                tx = cx + dx
                if not (0 <= tx < w): continue
                i = (ty*w+tx)*4
                key = tuple(self.dp5_canvas.rgba[i:i+3])
                if key not in kept:
                    best = min(kept, key=lambda k:(k[0]-key[0])**2+(k[1]-key[1])**2+(k[2]-key[2])**2)
                    self.dp5_canvas.rgba[i:i+3] = list(best)


    def _apply_atari_st_constraint(self, cx, cy, cw, ch, w, h): #vers 1
        """Atari ST: 16 colours per scanline from ST palette."""
        # Snap to Atari ST palette first
        self._snap_cell_to_palette(cx, cy, cw, ch, w, h, self._ATARI_ST_PALETTE)
        # Enforce max 16 colours per scanline (entire row, not just cell)
        for dy in range(ch):
            ty = cy + dy
            if not (0 <= ty < h): continue
            row_colours = {}
            for tx in range(w):
                i = (ty*w+tx)*4
                key = tuple(self.dp5_canvas.rgba[i:i+3])
                row_colours[key] = row_colours.get(key, 0) + 1
            if len(row_colours) <= 16: continue
            kept = sorted(row_colours, key=lambda k: -row_colours[k])[:16]
            for tx in range(w):
                i = (ty*w+tx)*4
                key = tuple(self.dp5_canvas.rgba[i:i+3])
                if key not in kept:
                    best = min(kept, key=lambda k:(k[0]-key[0])**2+(k[1]-key[1])**2+(k[2]-key[2])**2)
                    self.dp5_canvas.rgba[i:i+3] = list(best)


    def _apply_generic_constraint(self, cx, cy, cw, ch, w, h, max_c): #vers 1
        """Enforce generic max-colours-per-cell constraint."""
        colours = {}
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                key = tuple(self.dp5_canvas.rgba[i:i+3])
                colours[key] = colours.get(key, 0) + 1
        if len(colours) <= max_c: return
        kept = sorted(colours, key=lambda k: -colours[k])[:max_c]
        def nearest(c):
            return min(kept, key=lambda k:(k[0]-c[0])**2+(k[1]-c[1])**2+(k[2]-c[2])**2)
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                key = tuple(self.dp5_canvas.rgba[i:i+3])
                if key not in kept:
                    self.dp5_canvas.rgba[i:i+3] = list(nearest(key))
        self.dp5_canvas.update()
        self.dp5_canvas.update()


    def _update_status(self, x: int, y: int, colour: QColor): #vers 1
        zoom = self._canvas_zoom
        tool = getattr(self.dp5_canvas, 'tool', '?') if self.dp5_canvas else '?'
        self._set_status(
            f"Pos: {x},{y}  |  "
            f"RGBA({colour.red()},{colour.green()},{colour.blue()},{colour.alpha()})  |  "
            f"Zoom: {zoom}×  |  Tool: {tool}")


    def _update_zoom_label(self): #vers 1
        if self.dp5_canvas:
            self._canvas_zoom = self.dp5_canvas.zoom
        if hasattr(self, '_zoom_lbl'):
            z = self._canvas_zoom
            self._zoom_lbl.setText(f"{int(z)}×" if z >= 1 else f"{z:.2f}×")

    #    Canvas operations                                                      


    def _push_undo(self):
        if self.dp5_canvas: #vers 1
            self._undo_stack.append(bytes(self.dp5_canvas.rgba))
            self._redo_stack.clear()


    def _undo_canvas(self): #vers 1
        if self.dp5_canvas and self._undo_stack:
            self._redo_stack.append(bytes(self.dp5_canvas.rgba))
            self.dp5_canvas.rgba[:] = self._undo_stack.pop()
            self.dp5_canvas.update()


    def _redo_canvas(self): #vers 1
        if self.dp5_canvas and self._redo_stack:
            self._undo_stack.append(bytes(self.dp5_canvas.rgba))
            self.dp5_canvas.rgba[:] = self._redo_stack.pop()
            self.dp5_canvas.update()


    def _clear_canvas(self): #vers 1
        if not self.dp5_canvas: return
        self._push_undo()
        self.dp5_canvas.rgba[:] = b'\x00' * len(self.dp5_canvas.rgba)
        self.dp5_canvas.update()


    def _fill_canvas(self): #vers 1
        if not self.dp5_canvas: return
        self._push_undo()
        c = self.dp5_canvas.color
        for i in range(self._canvas_width * self._canvas_height):
            self.dp5_canvas.rgba[i*4:i*4+4] = [c.red(),c.green(),c.blue(),c.alpha()]
        self.dp5_canvas.update()


    def _fit_canvas_to_viewport(self): #vers 1
        if not self.dp5_canvas: return
        sa = getattr(self, '_canvas_scroll', None)
        vw = sa.viewport().width()  if sa else self.width()
        vh = sa.viewport().height() if sa else self.height()
        w = max(1, self.dp5_canvas.tex_w)
        h = max(1, self.dp5_canvas.tex_h)
        fit_z = min(vw / w, vh / h)
        for snap in (16, 8, 4, 2, 1):
            if fit_z >= snap:
                fit_z = snap; break
        self._set_zoom(max(0.05, fit_z))


    def resizeEvent(self, event): #vers 2
        super().resizeEvent(event)
        if self.dp5_settings.get('zoom_to_fit_resize'):
            self._fit_canvas_to_viewport()
        self._refresh_corner_overlay()


    def _set_zoom(self, z, anchor_widget_pos=None): #vers 1
        """
        Set zoom level.  anchor_widget_pos: QPoint in scroll-area viewport
        coordinates to keep fixed.  If None, anchors to viewport centre.
        """
        if not self.dp5_canvas: return
        old_z = max(0.01, self.dp5_canvas.zoom)
        z     = max(0.05, min(64.0, float(z)))
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


    def _invert(self): #vers 1
        from PIL import Image, ImageOps
        def _inv(img):
            r, g, b, a = img.split()
            return Image.merge('RGBA', (ImageOps.invert(r), ImageOps.invert(g),
                                        ImageOps.invert(b), a))
        self._pil_transform(_inv)


    def _adjust(self, delta: int): #vers 1
        if not self.dp5_canvas: return
        self._push_undo()
        for i in range(0, len(self.dp5_canvas.rgba), 4):
            for j in range(3):
                self.dp5_canvas.rgba[i+j] = max(0, min(255,
                    self.dp5_canvas.rgba[i+j] + delta))
        self.dp5_canvas.update()


    def _dither_floyd_steinberg(self): #vers 1
        """Apply Floyd-Steinberg error-diffusion dither to canvas (reduces to 16 colours)."""
        if not self.dp5_canvas: return
        from PIL import Image
        n, ok = QInputDialog.getInt(self, "Floyd-Steinberg Dither",
                                    "Colours to reduce to:", 16, 2, 256)
        if not ok: return
        self._push_undo()
        img = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                               bytes(self.dp5_canvas.rgba)).convert('RGB')
        # PIL quantize with dither=1 = Floyd-Steinberg
        q = img.quantize(colors=n, dither=1).convert('RGB').convert('RGBA')
        self.dp5_canvas.rgba = bytearray(q.tobytes())
        self.dp5_canvas.update()
        self._set_status(f"Floyd-Steinberg dither → {n} colours")


    def _dither_bayer_canvas(self): #vers 1
        """Apply 4×4 Bayer ordered dither to canvas."""
        if not self.dp5_canvas: return
        from PIL import Image
        n, ok = QInputDialog.getInt(self, "Bayer Dither",
                                    "Colours to reduce to:", 16, 2, 256)
        if not ok: return
        self._push_undo()
        BAYER4 = [[0,8,2,10],[12,4,14,6],[3,11,1,9],[15,7,13,5]]
        img = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                               bytes(self.dp5_canvas.rgba)).convert('RGB')
        # Quantize first to get palette, then apply bayer threshold
        q_pal = img.quantize(colors=n, dither=0)
        pal_flat = q_pal.getpalette()
        pal = [(pal_flat[i*3],pal_flat[i*3+1],pal_flat[i*3+2]) for i in range(n)]
        px = list(img.getdata())
        w,h = self._canvas_width, self._canvas_height
        out = bytearray(w*h*4)
        for y in range(h):
            for x in range(w):
                r,g,b = px[y*w+x]
                t = BAYER4[y%4][x%4]/16.0
                # Shift pixel value by threshold amount
                sr = min(255,int(r + (t-0.5)*32))
                sg = min(255,int(g + (t-0.5)*32))
                sb = min(255,int(b + (t-0.5)*32))
                best = min(pal, key=lambda c: (c[0]-sr)**2+(c[1]-sg)**2+(c[2]-sb)**2)
                i = (y*w+x)*4
                out[i:i+4] = [best[0],best[1],best[2],255]
        self.dp5_canvas.rgba = out
        self.dp5_canvas.update()
        self._set_status(f"Bayer 4×4 dither → {n} colours")


    def _dither_checker_canvas(self): #vers 1
        """Apply checkerboard FG/BG dither to entire canvas."""
        if not self.dp5_canvas: return
        self._push_undo()
        fg = self.dp5_canvas.color
        bg = self._fgbg_swatch._bg if hasattr(self,'_fgbg_swatch') else QColor(0,0,0,255)
        w,h = self._canvas_width, self._canvas_height
        out = bytearray(self.dp5_canvas.rgba)
        for y in range(h):
            for x in range(w):
                if (x+y)%2==0:
                    i=(y*w+x)*4
                    out[i:i+4]=[bg.red(),bg.green(),bg.blue(),bg.alpha()]
        self.dp5_canvas.rgba = out
        self.dp5_canvas.update()
        self._set_status("Checkerboard dither applied")


    def _pil_transform(self, fn): #vers 1
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


    def _rotate_90_cw(self): #vers 1
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.ROTATE_270))


    def _rotate_90_ccw(self): #vers 1
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.ROTATE_90))


    def _rotate_180(self): #vers 1
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.ROTATE_180))


    def _rotate_arbitrary(self): #vers 1
        deg, ok = QInputDialog.getInt(self, "Rotate", "Degrees (clockwise):",
                                      45, -359, 359)
        if not ok: return
        from PIL import Image
        self._pil_transform(lambda i: i.rotate(-deg, expand=True,
                                               resample=Image.Resampling.BILINEAR))

    def _mirror_h(self): #vers 1
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.FLIP_LEFT_RIGHT))


    def _mirror_v(self): #vers 1
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.FLIP_TOP_BOTTOM))


    def _scale_canvas(self): #vers 2
        """Scale canvas — inline overlay panel."""
        if not self.dp5_canvas: return

        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout,
            QGridLayout, QFormLayout, QSpinBox, QLabel, QPushButton, QComboBox)

        ctrl = QWidget()
        cl = QVBoxLayout(ctrl); cl.setContentsMargins(0,0,0,0); cl.setSpacing(3)

        form = QFormLayout()
        w_spin = QSpinBox(); w_spin.setRange(8, 8192); w_spin.setValue(self._canvas_width)
        h_spin = QSpinBox(); h_spin.setRange(8, 8192); h_spin.setValue(self._canvas_height)
        form.addRow("Width:", w_spin)
        form.addRow("Height:", h_spin)
        cl.addLayout(form)

        cl.addWidget(QLabel("Presets:"))
        grid = QGridLayout(); grid.setSpacing(2)
        for i, (lbl, pw, ph) in enumerate([
            ("32×32",32,32),("64×64",64,64),("128×128",128,128),("256×256",256,256),
            ("512×512",512,512),("1024×1024",1024,1024),("320×200",320,200),("640×480",640,480),
        ]):
            b = QPushButton(lbl); b.setFixedHeight(22)
            b.clicked.connect(lambda _=False, pw=pw, ph=ph:
                              (w_spin.setValue(pw), h_spin.setValue(ph)))
            grid.addWidget(b, i // 4, i % 4)
        cl.addLayout(grid)

        resamp_combo = QComboBox()
        resamp_combo.addItems(["Nearest", "Bilinear", "Bicubic", "Lanczos"])
        rf = QFormLayout(); rf.addRow("Resample:", resamp_combo)
        cl.addLayout(rf)

        parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

        def _apply():
            from PIL import Image
            methods = [Image.Resampling.NEAREST, Image.Resampling.BILINEAR,
                       Image.Resampling.BICUBIC, Image.Resampling.LANCZOS]
            nw, nh = w_spin.value(), h_spin.value()
            self._pil_transform(lambda i, nw=nw, nh=nh, m=methods[resamp_combo.currentIndex()]:
                                i.resize((nw, nh), m))

        self._ToolOverlay(parent_vp, self, "Scale Canvas",
                          ctrl, apply_fn=_apply, generate_fn=None)

    def _new_btn_context_menu(self, pos): #vers 1
        """Right-click on New button — pick mode then open New Canvas on that tab."""
        menu = QMenu(self)
        menu.addAction("New — Free canvas",     lambda: (self._set_canvas_mode('free',     confirm=False, apply=False), self._new_canvas()))
        menu.addAction("New — Platform canvas", lambda: (self._set_canvas_mode('platform', confirm=False, apply=False), self._new_canvas()))
        menu.addAction("New — Texture canvas",  lambda: (self._set_canvas_mode('texture',  confirm=False, apply=False), self._new_canvas()))
        menu.addAction("New — Icon canvas",     lambda: (self._set_canvas_mode('icon',     confirm=False, apply=False), self._new_canvas()))
        btn = getattr(self, 'tb_new_btn', None)
        menu.exec(btn.mapToGlobal(pos) if btn else self.cursor().pos())

    def _new_canvas(self): #vers 4
        """New canvas dialog — tabbed: Platform / Texture / Icon / Custom."""
        dlg = QDialog(self)
        dlg.setWindowTitle("New Canvas")
        dlg.setMinimumWidth(380)
        root = QVBoxLayout(dlg)
        tabs = QTabWidget()

        #    Helper: build a preset combo + w/h/depth form                  
        def make_preset_tab(presets, default_w, default_h, default_d):
            w = QWidget(); fl = QFormLayout(w); fl.setSpacing(6)
            pc = QComboBox()
            for name, pw, ph, pd in presets:
                pc.addItem(name, (pw, ph, pd))
            fl.addRow("Preset:", pc)
            ws = QSpinBox(); ws.setRange(1,4096); ws.setValue(default_w)
            hs = QSpinBox(); hs.setRange(1,4096); hs.setValue(default_h)
            dc = QComboBox()
            dc.addItems(["32-bit RGBA","24-bit RGB","16-bit (R5G6B5)","8-bit indexed"])
            dc.setCurrentIndex(default_d)
            fl.addRow("Width:",     ws)
            fl.addRow("Height:",    hs)
            fl.addRow("Bit depth:", dc)
            fc = QComboBox()
            fc.addItems(["Grey (128,128,128)","Black","White","Transparent"])
            fl.addRow("Fill:", fc)
            def on_preset(idx):
                data = pc.itemData(idx)
                if data:
                    pw,ph,pd = data
                    if pw>0: ws.setValue(pw)
                    if ph>0: hs.setValue(ph)
                    if pd is not None: dc.setCurrentIndex(pd)
            pc.currentIndexChanged.connect(on_preset)
            return w, ws, hs, dc, fc, pc

        #    Platform tab                                                    
        PLATFORM_PRESETS = [
            ("Custom",                    0,   0,  3),
            ("   Amiga   ",                    0,   0,  0),
            ("OCS PAL LowRes    320×256",   320, 256,  3),
            ("OCS NTSC LowRes   320×200",   320, 200,  3),
            ("OCS PAL HiRes     640×256",   640, 256,  3),
            ("OCS PAL Lace      320×512",   320, 512,  3),
            ("ECS PAL LowRes    320×256",   320, 256,  3),
            ("ECS PAL HiRes     640×256",   640, 256,  3),
            ("AGA PAL LowRes    320×256",   320, 256,  3),
            ("AGA PAL HiRes     640×256",   640, 256,  3),
            ("RTG 640×480",                 640, 480,  3),
            ("RTG 800×600",                 800, 600,  3),
            ("RTG 1024×768",               1024, 768,  3),
            ("RTG 720×576 PAL broadcast",   720, 576,  3),
            ("RTG 720×480 NTSC broadcast",  720, 480,  3),
            ("   Commodore 64   ",            0,   0,  0),
            ("C64 Hires      320×200",      320, 200,  3),
            ("C64 Multicolor 160×200",      160, 200,  3),
            ("   ZX Spectrum   ",          0,   0,  0),
            ("Spectrum 48K   256×192",   256, 192,  3),
            ("Spectrum 128K  256×192",   256, 192,  3),
            ("ZX80           256×192",   256, 192,  3),
            ("ZX81           256×192",   256, 192,  3),
            ("ZX Next L2     320×256",   320, 256,  3),
            ("ZX Next L2 640 640×256",   640, 256,  3),
            ("ZX Next ULA    256×192",   256, 192,  3),
            ("Timex TS2068   256×192",   256, 192,  3),
            ("Timex HiRes    512×192",   512, 192,  3),
            ("Pentagon       256×192",   256, 192,  3),
            ("Jupiter Ace    256×192",   256, 192,  3),
            ("   MSX   ",                  0,   0,  0),
            ("MSX1           256×192",   256, 192,  3),
            ("MSX2           256×212",   256, 212,  3),
            ("   Amstrad CPC   ",           0,   0,  0),
            ("CPC Mode 0     160×200",   160, 200,  3),
            ("CPC Mode 1     320×200",   320, 200,  3),
            ("CPC Mode 2     640×200",   640, 200,  3),
            ("CPC+/GX4000    320×200",   320, 200,  3),
            ("PCW            720×256",   720, 256,  3),
            ("NC100/200      480×128",   480, 128,  3),
            ("   Atari   ",                0,   0,  0),
            ("Atari 2600 NTSC 160×192",  160, 192,  3),
            ("Atari 800/XL   320×192",   320, 192,  3),
            ("Atari 5200     320×192",   320, 192,  3),
            ("Atari 7800      160×240",  160, 240,  3),
            ("Atari ST Low   320×200",   320, 200,  3),
            ("Atari ST Med   640×200",   640, 200,  3),
            ("Atari STe Low  320×200",   320, 200,  3),
            ("Atari Lynx     160×102",   160, 102,  3),
            ("Atari Falcon LowRes 320×200", 320, 200, 3),
            ("Atari Falcon HiRes  640×480", 640, 480, 3),
            ("Atari Jaguar   320×240",   320, 240,  3),
            ("   Plus/4   ",               0,   0,  0),
            ("Plus/4 Hires   320×200",   320, 200,  3),
            ("Plus/4 Multi   160×200",   160, 200,  3),
            ("   VIC-20   ",               0,   0,  0),
            ("VIC-20         176×184",   176, 184,  3),
            ("   Sinclair QL   ",           0,   0,  0),
            ("QL Low         256×256",   256, 256,  3),
            ("   Nintendo   ",              0,   0,  0),
            ("NES            256×240",   256, 240,  3),
            ("SNES           256×224",   256, 224,  3),
            ("SNES HiRes     512×224",   512, 224,  3),
            ("Game Boy        160×144",  160, 144,  3),
            ("Game Boy Color  160×144",  160, 144,  3),
            ("Game Boy Adv    240×160",  240, 160,  3),
            ("   Sega   ",                  0,   0,  0),
            ("SG-1000        256×192",   256, 192,  3),
            ("Master System  256×192",   256, 192,  3),
            ("Mega Drive     320×224",   320, 224,  3),
            ("Game Gear      160×144",   160, 144,  3),
            ("   NEC   ",                   0,   0,  0),
            ("PC Engine      256×240",   256, 240,  3),
            ("PC Engine CD   256×240",   256, 240,  3),
            ("   RM Nimbus   ",             0,   0,  0),
            ("Nimbus LowRes  320×250",   320, 250,  3),
            ("Nimbus HiRes   640×250",   640, 250,  3),
        ]
        plat_tab, plat_w, plat_h, plat_d, plat_f, plat_pc = make_preset_tab(
            PLATFORM_PRESETS, 320, 200, 3)
        tabs.addTab(plat_tab, "Platform")

        #    Texture tab                                                     
        TEX_PRESETS = [
            ("Custom",               0,    0,   0),
            ("   8-bit   ",          0,    0,   0),
            ("8b   16×16",          16,   16,   3),
            ("8b   32×32",          32,   32,   3),
            ("8b   64×64",          64,   64,   3),
            ("8b  128×128",        128,  128,   3),
            ("   16-bit   ",         0,    0,   0),
            ("16b  32×32",          32,   32,   2),
            ("16b  64×64",          64,   64,   2),
            ("16b 128×128",        128,  128,   2),
            ("16b 256×256",        256,  256,   2),
            ("16b 512×512",        512,  512,   2),
            ("   24/32-bit   ",      0,    0,   0),
            ("32b  64×64",          64,   64,   0),
            ("32b 128×128",        128,  128,   0),
            ("32b 256×256",        256,  256,   0),
            ("32b 512×512",        512,  512,   0),
            ("32b 1024×1024",     1024, 1024,   0),
            ("32b 2048×2048",     2048, 2048,   0),
        ]
        tex_tab, tex_w, tex_h, tex_d, tex_f, tex_pc = make_preset_tab(
            TEX_PRESETS, 256, 256, 0)
        tabs.addTab(tex_tab, "Texture")

        # - Icon tab
        ICON_PRESETS = [
            ("Custom",               0,   0,  0),
            ("   Windows ICO   ",    0,   0,  0),
            ("ICO  16×16",          16,  16,  0),
            ("ICO  32×32",          32,  32,  0),
            ("ICO  48×48",          48,  48,  0),
            ("ICO  64×64",          64,  64,  0),
            ("ICO 128×128",        128, 128,  0),
            ("ICO 256×256",        256, 256,  0),
            ("   Linux / Web   ",    0,   0,  0),
            ("SVG  32×32",          32,  32,  0),
            ("SVG  64×64",          64,  64,  0),
            ("SVG 128×128",        128, 128,  0),
            ("   Amiga   ",          0,   0,  0),
            ("Amiga  32×32",        32,  32,  3),
            ("Amiga  64×64",        64,  64,  3),
            ("   Sprites   ",        0,   0,  0),
            ("Sprite 16×16",        16,  16,  3),
            ("Sprite 16×32",        16,  32,  3),
            ("Sprite 32×32",        32,  32,  3),
            ("Sprite 32×64",        32,  64,  3),
        ]
        icon_tab, icon_w, icon_h, icon_d, icon_f, icon_pc = make_preset_tab(
            ICON_PRESETS, 32, 32, 0)
        tabs.addTab(icon_tab, "Icon")


        # = Free tab
        FREE_PRESETS = [
            ("Custom",               0,    0,  0),
            ("HD     1280×720",   1280,  720,  1),
            ("FHD   1920×1080",   1920, 1080,  1),
            ("4K    3840×2160",   3840, 2160,  1),
        ]
        free_tab, free_w, free_h, free_d, free_f, free_pc = make_preset_tab(
            FREE_PRESETS,
            self.dp5_settings.get('default_width'),
            self.dp5_settings.get('default_height'), 0)
        tabs.addTab(free_tab, "Free")

        # Set active tab to current mode
        mode_tab = {'platform':0,'texture':1,'icon':2,'free':3}
        tabs.setCurrentIndex(mode_tab.get(self._canvas_mode, 3))

        root.addWidget(tabs)
        btns = QHBoxLayout(); btns.addStretch()
        ok = QPushButton("Create"); ok.setDefault(True)
        can = QPushButton("Cancel")
        ok.clicked.connect(dlg.accept); can.clicked.connect(dlg.reject)
        btns.addWidget(ok); btns.addWidget(can)
        root.addLayout(btns)

        if dlg.exec() != QDialog.DialogCode.Accepted: return

        tab_idx = tabs.currentIndex()
        tab_mode = ['platform','texture','icon','free'][tab_idx]
        w_spin, h_spin, depth_combo, fill_combo, preset_combo = [
            (plat_w, plat_h, plat_d, plat_f, plat_pc),
            (tex_w,  tex_h,  tex_d,  tex_f,  tex_pc),
            (icon_w, icon_h, icon_d, icon_f, icon_pc),
            (free_w, free_h, free_d, free_f, free_pc),
        ][tab_idx]

        w = w_spin.value(); h = h_spin.value()
        fill_idx = fill_combo.currentIndex()
        if   fill_idx == 0: fill = b'\x80\x80\x80\xff'
        elif fill_idx == 1: fill = b'\x00\x00\x00\xff'
        elif fill_idx == 2: fill = b'\xff\xff\xff\xff'
        else:               fill = b'\x00\x00\x00\x00'

        self._canvas_width  = w
        self._canvas_height = h
        self._canvas_bit_depth = depth_combo.currentIndex()

        # Lock mode — don't apply to canvas (we're about to write fresh rgba)
        self._set_canvas_mode(tab_mode, confirm=False, apply=False)

        preset_name = preset_combo.currentText()
        _preset_platform = {
            'C64 Hires': 'c64', 'C64 Multicolor': 'c64m',
            'Spectrum       256': 'spectrum', 'Spectrum Next  320': 'specnext',
            'MSX1': 'msx',
            'CPC Mode 0': 'cpc', 'CPC Mode 1': 'cpc1', 'CPC Mode 2': 'cpc1',
            'Atari ST Low': 'atari_st', 'Atari ST Med': 'atari_st',
            'Amiga OCS': 'amiga', 'Amiga AGA': 'amiga_aga',
            'Plus/4 Hires': 'plus4', 'Plus/4 Multi': 'plus4',
            'VIC-20': 'vic20',
        }
        plat = next((v for k,v in _preset_platform.items() if k in preset_name), 'none')
        if tab_mode == 'platform' and plat != 'none':
            self._set_platform(plat)
            self._enforce_constraints = True

        if self.dp5_canvas:
            self.dp5_canvas.tex_w = w
            self.dp5_canvas.tex_h = h
            self.dp5_canvas.rgba  = bytearray(fill * (w * h))
            self.dp5_canvas.update()
            self._fit_canvas_to_viewport()
            # Reset animation to single blank frame
            self._frames = [bytearray(self.dp5_canvas.rgba)]
            self._frame_delays = [1000 // max(1, self.dp5_settings.get('anim_fps'))]
            self._current_frame = 0
            if hasattr(self, '_anim_strip'):
                self._anim_refresh_thumbs()

        mode_labels = {'platform':'Platform','texture':'Texture','icon':'Icon','free':'Free'}
        self._set_status(f"New canvas: {w}\u00d7{h}  {depth_combo.currentText()}  [{mode_labels[tab_mode]}]")


    def _crop_to_selection(self): #vers 1
        """Crop canvas to the current selection rect."""
        if not self.dp5_canvas: return
        c = self.dp5_canvas
        if not c._sel_active or not c._selection_rect:
            self._set_status("No selection to crop to")
            return
        r = c._selection_rect
        x, y, w, h = r.x(), r.y(), r.width(), r.height()
        if w <= 0 or h <= 0: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (c.tex_w, c.tex_h), bytes(c.rgba))
            cropped = img.crop((x, y, x + w, y + h))
            c.tex_w, c.tex_h = w, h
            self._canvas_width, self._canvas_height = w, h
            c.rgba = bytearray(cropped.tobytes())
            c._sel_active = False
            c._selection_rect = None
            c.update()
            self._fit_canvas_to_viewport()
            self._set_status(f"Cropped to {w}×{h}")
        except Exception as e:
            QMessageBox.warning(self, "Crop Error", str(e))


    def _resize_canvas_dialog(self): #vers 3
        """Resize canvas — inline overlay panel."""
        if not self.dp5_canvas: return

        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QFormLayout,
            QSpinBox, QComboBox)

        ctrl = QWidget()
        cl = QVBoxLayout(ctrl); cl.setContentsMargins(0,0,0,0); cl.setSpacing(3)
        form = QFormLayout()

        w_spin = QSpinBox(); w_spin.setRange(1, 4096); w_spin.setValue(self.dp5_canvas.tex_w)
        h_spin = QSpinBox(); h_spin.setRange(1, 4096); h_spin.setValue(self.dp5_canvas.tex_h)
        form.addRow("Width:", w_spin)
        form.addRow("Height:", h_spin)

        depth_combo = QComboBox()
        depth_combo.addItems(["32-bit RGBA","24-bit RGB","16-bit (R5G6B5)","8-bit indexed"])
        depth_combo.setCurrentIndex(getattr(self, '_canvas_bit_depth', 0))
        form.addRow("Bit depth:", depth_combo)

        resample_combo = QComboBox()
        resample_combo.addItems(["Nearest","Bilinear","Lanczos"])
        form.addRow("Resample:", resample_combo)
        cl.addLayout(form)

        parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

        def _apply():
            w, h = w_spin.value(), h_spin.value()
            self._canvas_bit_depth = depth_combo.currentIndex()
            try:
                from PIL import Image
                resample = [Image.NEAREST, Image.BILINEAR, Image.LANCZOS][resample_combo.currentIndex()]
                img = Image.frombytes('RGBA', (self.dp5_canvas.tex_w, self.dp5_canvas.tex_h),
                                      bytes(self.dp5_canvas.rgba))
                resized = img.resize((w, h), resample)
                self._push_undo()
                self.dp5_canvas.tex_w = self._canvas_width  = w
                self.dp5_canvas.tex_h = self._canvas_height = h
                self.dp5_canvas.rgba  = bytearray(resized.tobytes())
                self.dp5_canvas.update()
                self._fit_canvas_to_viewport()
                self._set_status(f"Resized to {w}×{h}  {depth_combo.currentText()}")
            except Exception as e:
                self._set_status(f"Resize error: {e}")

        self._ToolOverlay(parent_vp, self, "Resize Canvas",
                          ctrl, apply_fn=_apply, generate_fn=None)


    #    File I/O                                                               

    def _import_bitmap(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Open Image", "",
            "Images (*.png *.bmp *.jpg *.jpeg *.iff *.lbm *.iff);;All Files (*)")
        if not path or not self.dp5_canvas: return
        self._import_bitmap_path(path)


    def _show_load_menu(self): #vers 1
        """Show load options menu from Load button left-click."""
        btn = getattr(self, 'tb_load_btn', None)
        menu = self._build_load_menu()
        if btn:
            menu.exec(btn.mapToGlobal(btn.rect().bottomLeft()))
        else:
            menu.exec(self.cursor().pos())


    def _show_load_menu_at(self, pos): #vers 1
        """Show load options menu at right-click position."""
        btn = self.tb_load_btn
        self._build_load_menu().exec(btn.mapToGlobal(pos))


    def _build_load_menu(self): #vers 1
        """Build the 4-option load menu."""
        menu = QMenu(self)
        menu.addAction("Open…",                               self._import_bitmap)
        menu.addAction("Snap to pal…",                        self._import_bitmap_snap_user_pal)
        menu.addAction("Snap to pal (dither)…",               self._import_bitmap_snap_dither)
        menu.addAction("Snap to pal, canvas size…",           self._import_bitmap_snap_canvas_size)
        menu.addAction("Snap to pal, canvas size (dither)…",  self._import_bitmap_snap_canvas_size_dither)
        return menu


    def _load_btn_context_menu(self, pos): #vers 2
        """Right-click Load button — delegates to _show_load_menu_at."""
        self._show_load_menu_at(pos)


    def _import_bitmap_snap_canvas_size(self): #vers 1
        """Open image, resize to current canvas size, hard-snap to user palette."""
        palette = self._get_user_palette_rgb()
        if not palette:
            QMessageBox.warning(self, "Snap to Palette",
                                "No user palette loaded. Load a palette first.")
            return
        path, _ = QFileDialog.getOpenFileName(
            self, "Open + Snap to Pal, Canvas Size", "",
            "Images (*.png *.bmp *.jpg *.jpeg *.iff *.lbm *.tga *.tiff *.gif *.dds *.psd);;All Files (*)")
        if not path or not self.dp5_canvas: return
        from PIL import Image
        img = Image.open(path).convert('RGBA')
        img = img.resize((self._canvas_width, self._canvas_height), Image.LANCZOS)
        self._push_undo()
        snapped = self._snap_image_to_user_palette(img)
        self.dp5_canvas.rgba = bytearray(snapped.tobytes())
        self.dp5_canvas.update()
        self._set_status(
            f"Opened + snapped {self._canvas_width}×{self._canvas_height}: {os.path.basename(path)}")


    def _import_bitmap_snap_canvas_size_dither(self): #vers 1
        """Open image, resize to current canvas size, dithered snap to user palette."""
        palette = self._get_user_palette_rgb()
        if not palette:
            QMessageBox.warning(self, "Snap + Dither",
                                "No user palette loaded. Load a palette first.")
            return
        # Ask dither method
        method, ok = QInputDialog.getItem(
            self, "Dither Method", "Choose dither:",
            ["Floyd-Steinberg", "Bayer 4×4", "Checkerboard"], 0, False)
        if not ok: return
        mode_map = {"Floyd-Steinberg":"floyd","Bayer 4×4":"bayer","Checkerboard":"checker"}
        mode = mode_map[method]

        path, _ = QFileDialog.getOpenFileName(
            self, f"Open + Snap, Canvas Size ({method})", "",
            "Images (*.png *.bmp *.jpg *.jpeg *.iff *.lbm *.tga *.tiff *.gif *.dds *.psd);;All Files (*)")
        if not path or not self.dp5_canvas: return
        from PIL import Image
        img = Image.open(path).convert('RGBA')
        img = img.resize((self._canvas_width, self._canvas_height), Image.LANCZOS)
        self._push_undo()
        old_mode = getattr(self, '_pal_dither_mode', 'off')
        self._pal_dither_mode = mode
        snapped = self._apply_user_palette_dither(img)
        self._pal_dither_mode = old_mode
        self.dp5_canvas.rgba = bytearray(snapped.tobytes())
        self.dp5_canvas.update()
        self._set_status(
            f"Opened + {mode} dither {self._canvas_width}×{self._canvas_height}: {os.path.basename(path)}")


    def _import_bitmap_snap_user_pal(self): #vers 3
        """Open image then hard-snap every pixel to nearest user palette colour."""
        palette = self._get_user_palette_rgb()
        if not palette:
            QMessageBox.warning(self, "Snap to Palette",
                                "No user palette loaded. Load a palette first.")
            return
        path, _ = QFileDialog.getOpenFileName(
            self, "Open + Snap to Palette", "",
            "Images (*.png *.bmp *.jpg *.jpeg *.iff *.lbm *.tga *.tiff *.gif *.dds *.psd);;All Files (*)")
        if not path or not self.dp5_canvas: return
        self._import_bitmap_path(path)
        from PIL import Image
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.dp5_canvas.rgba))
        snapped = self._snap_image_to_user_palette(img)
        self.dp5_canvas.rgba = bytearray(snapped.tobytes())
        self.dp5_canvas.update()
        self._set_status(f"Opened + snapped: {os.path.basename(path)}")


    def _import_bitmap_snap_dither(self): #vers 1
        """Open image, snap to user palette with dithering — ask which dither type."""
        palette = self._get_user_palette_rgb()
        if not palette:
            QMessageBox.warning(self, "Snap + Dither",
                                "No user palette loaded. Load a palette first.")
            return
        # Inline overlay — pick dither method then open file
        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QLabel,
            QRadioButton, QButtonGroup)

        ctrl = QWidget()
        cl = QVBoxLayout(ctrl); cl.setContentsMargins(0,0,0,0); cl.setSpacing(4)
        cl.addWidget(QLabel(
            f"Palette: {self.current_retro_palette}  ({len(palette)} colours)"))
        cl.addWidget(QLabel("Dither method:"))

        bg = QButtonGroup(ctrl)
        opts = [
            ('floyd',   'Floyd-Steinberg'),
            ('bayer',   'Bayer 4×4 ordered'),
            ('checker', 'Checkerboard'),
        ]
        radios = []
        for mode_id, label in opts:
            rb = QRadioButton(label)
            bg.addButton(rb); cl.addWidget(rb)
            radios.append((mode_id, rb))
        radios[0][1].setChecked(True)

        parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

        def _apply():
            mode = next(m for m, rb in radios if rb.isChecked())
            path, _ = QFileDialog.getOpenFileName(
                self, f"Open + Snap ({mode} dither)", "",
                "Images (*.png *.bmp *.jpg *.jpeg *.iff *.lbm *.tga *.tiff *.gif *.dds *.psd);;All Files (*)")
            if not path or not self.dp5_canvas: return
            self._import_bitmap_path(path)
            old_mode = getattr(self, '_pal_dither_mode', 'off')
            self._pal_dither_mode = mode
            from PIL import Image
            img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                                  bytes(self.dp5_canvas.rgba))
            snapped = self._apply_user_palette_dither(img)
            self._pal_dither_mode = old_mode
            self.dp5_canvas.rgba = bytearray(snapped.tobytes())
            self.dp5_canvas.update()
            self._set_status(f"Opened + {mode} dither: {os.path.basename(path)}")

        self._ToolOverlay(parent_vp, self, "Dither Method",
                          ctrl, apply_fn=_apply, generate_fn=None)


    def _import_bitmap_path(self, path: str): #vers 3
        """Load an image, auto-reduce to current mode constraints if locked."""
        if not path or not self.dp5_canvas: return
        # Save current canvas state into animation frame before overwriting
        if self._frames:
            self._anim_save_current_frame()
        try:
            from PIL import Image
            img = Image.open(path).convert('RGBA')
            w, h = img.size

            mode = getattr(self, '_canvas_mode', 'free')
            locked = getattr(self, '_mode_locked', False)

            if locked and mode == 'platform':
                # 1. Resize to platform resolution
                plat = getattr(self, '_platform_mode', 'none')
                plat_res = {
                    'c64':(320,200),'c64m':(160,200),'spectrum':(256,192),'zx80':(256,192),'zx81':(256,192),
                    'specnext':(320,256),'msx':(256,192),'cpc':(160,200),
                    'cpc1':(320,200),'atari_st':(320,200),'amiga':(320,256),
                    'amiga_aga':(320,256),'plus4':(320,200),'vic20':(176,184),
                }
                if plat in plat_res:
                    w, h = plat_res[plat]
                    img = img.resize((w, h), Image.LANCZOS)
                # 2. Snap every pixel to nearest platform palette colour
                img = self._snap_image_to_platform_palette(img)
                self._canvas_bit_depth = 3

            elif locked and mode == 'texture':
                import math
                pw = 2 ** round(math.log2(max(w, 1)))
                ph = 2 ** round(math.log2(max(h, 1)))
                if pw != w or ph != h:
                    img = img.resize((pw, ph), Image.LANCZOS)
                    w, h = pw, ph
                depth = self._canvas_bit_depth
                if depth == 1:
                    img = img.convert('RGB').convert('RGBA')
                elif depth == 2:
                    rgb = img.convert('RGB'); px = rgb.tobytes()
                    buf = bytearray(len(px))
                    for i in range(0, len(px), 3):
                        buf[i]  = (px[i]   >> 3) << 3
                        buf[i+1]= (px[i+1] >> 2) << 2
                        buf[i+2]= (px[i+2] >> 3) << 3
                    img = Image.frombytes('RGB',(w,h),bytes(buf)).convert('RGBA')
                elif depth == 3:
                    img = img.convert('RGB').quantize(colors=256).convert('RGB').convert('RGBA')

            elif locked and mode == 'icon':
                ICON_SIZES = [16, 32, 48, 64, 128, 256]
                best = min(ICON_SIZES, key=lambda s: abs(s - max(w, h)))
                img = img.resize((best, best), Image.LANCZOS)
                w, h = best, best

            else:
                # Free mode: apply bit depth only
                depth = getattr(self, '_canvas_bit_depth', 0)
                if depth == 1:
                    img = img.convert('RGB').convert('RGBA')
                elif depth == 2:
                    rgb = img.convert('RGB'); px = rgb.tobytes()
                    buf = bytearray(len(px))
                    for i in range(0, len(px), 3):
                        buf[i]  = (px[i]   >> 3) << 3
                        buf[i+1]= (px[i+1] >> 2) << 2
                        buf[i+2]= (px[i+2] >> 3) << 3
                    img = Image.frombytes('RGB',(w,h),bytes(buf)).convert('RGBA')
                elif depth == 3:
                    img = img.convert('RGB').quantize(colors=256).convert('RGB').convert('RGBA')

            self._canvas_width  = w
            self._canvas_height = h
            new_rgba = bytearray(img.tobytes())
            self.dp5_canvas.tex_w = w
            self.dp5_canvas.tex_h = h
            self.dp5_canvas.rgba  = new_rgba
            self.dp5_canvas.update()

            # Auto-fit zoom — pixel-perfect for small canvases
            sa = getattr(self, '_canvas_scroll', None)
            vw = sa.viewport().width()  if sa else 800
            vh = sa.viewport().height() if sa else 600
            fit_z = min(vw/max(1,w), vh/max(1,h))
            for snap in (16,8,4,2,1):
                if fit_z >= snap: fit_z = snap; break
            self._set_zoom(max(0.05, fit_z))

            # Update image palette display
            if locked and mode == 'platform' and self._platform_mode in (
                    'c64','c64m','spectrum','msx','cpc','cpc1','atari_st',
                    'amiga','plus4','vic20'):
                # Show the actual platform palette
                pal_src = {
                    'c64':self._C64_PALETTE,'c64m':self._C64_PALETTE,
                    'spectrum':self._ZX_PALETTE,'msx':self._MSX_PALETTE,
                    'cpc':self._CPC_PALETTE,'cpc1':self._CPC_PALETTE,
                    'atari_st':self._ATARI_ST_PALETTE,
                    'plus4':self._C64_PALETTE,'vic20':self._C64_PALETTE,
                }.get(self._platform_mode, [])
                if pal_src:
                    self.pal_bar.set_palette_raw(pal_src)
            else:
                p_img    = img.quantize(colors=256)
                pal_flat = p_img.getpalette()
                palette  = [(pal_flat[i*3], pal_flat[i*3+1], pal_flat[i*3+2]) for i in range(256)]
                self.pal_bar.set_palette_raw(palette)
                self._fit_img_pal_height()

            # Apply platform cell constraints across the whole image
            if locked and mode == 'platform' and self._enforce_constraints:
                cw = max(1, self.dp5_canvas.cell_w)
                ch = max(1, self.dp5_canvas.cell_h)
                for cy in range(0, h, ch):
                    for cx in range(0, w, cw):
                        self._apply_cell_constraint(cx, cy)
                self.dp5_canvas.update()

            name = os.path.basename(path)
            self._bitmap_list.append({'name': name, 'rgba': new_rgba, 'w': w, 'h': h})
            self._bitmap_lw.addItem(name)
            self._bitmap_lw.setCurrentRow(len(self._bitmap_list)-1)

            self._set_status(f"Opened: {name}  {w}×{h}  zoom {self.dp5_canvas.zoom:.2f}×")
        except Exception as e:
            QMessageBox.warning(self, "Open Error", str(e))


    #    Zoom Lens                                                                 

    def _open_zoom_lens(self): #vers 2
        """Embedded overlay zoom lens — top-left corner of the canvas scroll area.
        Never drops behind. Resizable by scroll wheel or +/- buttons when hovered.
        Toggle: calling again hides/shows."""
        # Toggle if already open
        existing = getattr(self, '_zoom_lens', None)
        if existing:
            existing.setVisible(not existing.isVisible())
            if existing.isVisible():
                existing.raise_()
                existing._refresh()
            return

        from PyQt6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QLabel, QPushButton
        from PyQt6.QtCore import Qt, QTimer, QRect
        from PyQt6.QtGui import QImage, QPixmap, QPainter, QColor, QPen

        # Parent = scroll area viewport so it stays inside and never goes behind
        sa = getattr(self, '_canvas_scroll', None)
        if not sa:
            return
        parent_vp = sa.viewport()

        class _ZoomOverlay(QWidget):
            """Overlay lens widget — parented to canvas viewport, top-left anchored."""

            def __init__(self, workshop):
                super().__init__(parent_vp)
                self._ws   = workshop
                self._mag  = [8]    # mutable magnification
                self._sz   = [180]  # overlay pixel size (square)
                self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
                self.setMouseTracking(True)
                self._hovered = False
                self._drag_start = None
                self._rebuild()
                self._timer = QTimer(self)
                self._timer.timeout.connect(self._refresh)
                self._timer.start(80)
                self._place()

            def _rebuild(self):
                sz = self._sz[0]
                self.setFixedSize(sz, sz + 22)   # +22 for header bar

            def _place(self):
                self.move(4, 4)

            def _refresh(self):
                if not self.isVisible(): return
                ws = self._ws
                canvas = getattr(ws, 'dp5_canvas', None)
                if not canvas: return
                try:
                    mag = self._mag[0]
                    # Use mouse position if available, fall back to scroll centre
                    if hasattr(canvas, '_zoom_lens_pos'):
                        cx, cy = canvas._zoom_lens_pos
                    else:
                        z   = getattr(canvas, 'zoom', 1)
                        sa2 = getattr(ws, '_canvas_scroll', None)
                        if sa2:
                            cx = int((sa2.horizontalScrollBar().value()
                                      + sa2.viewport().width() // 2) / max(1, z))
                            cy = int((sa2.verticalScrollBar().value()
                                      + sa2.viewport().height() // 2) / max(1, z))
                        else:
                            cx, cy = canvas.tex_w // 2, canvas.tex_h // 2

                    tw, th = canvas.tex_w, canvas.tex_h
                    sz  = self._sz[0]
                    lw  = max(1, sz // mag); lh = max(1, sz // mag)
                    x0  = max(0, min(cx - lw // 2, tw - lw))
                    y0  = max(0, min(cy - lh // 2, th - lh))
                    x1, y1 = x0 + lw, y0 + lh

                    rgba     = bytes(canvas.rgba)
                    crop_w   = x1 - x0; crop_h = y1 - y0
                    if crop_w <= 0 or crop_h <= 0: return

                    cropped = bytearray(crop_h * crop_w * 4)
                    for row in range(crop_h):
                        s = ((y0 + row) * tw + x0) * 4
                        d = row * crop_w * 4
                        cropped[d:d + crop_w * 4] = rgba[s:s + crop_w * 4]

                    qi = QImage(bytes(cropped), crop_w, crop_h,
                                crop_w * 4, QImage.Format.Format_RGBA8888)
                    self._pixmap = QPixmap.fromImage(qi).scaled(
                        sz, sz,
                        Qt.AspectRatioMode.IgnoreAspectRatio,
                        Qt.TransformationMode.FastTransformation)
                    self._info = f"{cx},{cy}  {mag}×"
                    self.update()
                except Exception:
                    pass

            def paintEvent(self, ev):
                from PyQt6.QtGui import QPainter, QColor, QPen, QFont
                p = QPainter(self)
                if not p.isActive():
                    return
                sz = self._sz[0]
                pal = self.palette()
                # Header uses toolbar bg so it matches the rest of the UI
                _hdr_bg  = pal.color(pal.ColorRole.Window)
                _hdr_txt = pal.color(pal.ColorRole.BrightText)
                _hint_c  = pal.color(pal.ColorRole.PlaceholderText)
                _brd_c   = pal.color(pal.ColorRole.Highlight)
                _vp_bg   = pal.color(pal.ColorRole.Base)
                # Header bar
                p.fillRect(0, 0, sz, 22, _hdr_bg)
                p.setPen(_hdr_txt)
                f = QFont("Arial", 8); p.setFont(f)
                info = getattr(self, '_info', 'Zoom Lens')
                # SVG zoom icon drawn as simple geometric shapes
                p.setBrush(_hdr_txt)
                p.setPen(Qt.PenStyle.NoPen)
                # Circle
                p.drawEllipse(3, 4, 10, 10)
                p.setBrush(_hdr_bg)
                p.drawEllipse(5, 6, 6, 6)
                # Handle
                p.setBrush(_hdr_txt)
                p.drawRect(12, 13, 5, 2)
                p.setPen(_hdr_txt)
                p.setFont(QFont("Arial", 8))
                p.drawText(20, 15, info)
                # [+] [-] zoom magnification buttons
                btn_w, btn_h = 18, 14
                btn_y = 4
                # [-] button
                minus_x = sz - btn_w - 2
                p.setBrush(_hint_c); p.setPen(Qt.PenStyle.NoPen)
                p.drawRoundedRect(minus_x, btn_y, btn_w, btn_h, 2, 2)
                p.setPen(_hdr_bg)
                p.setFont(QFont("Arial", 8, QFont.Weight.Bold))
                p.drawText(minus_x + 5, btn_y + 11, "-")
                # [+] button
                plus_x = minus_x - btn_w - 2
                p.setBrush(_hint_c); p.setPen(Qt.PenStyle.NoPen)
                p.drawRoundedRect(plus_x, btn_y, btn_w, btn_h, 2, 2)
                p.setPen(_hdr_bg)
                p.drawText(plus_x + 4, btn_y + 11, "+")
                # Lens image
                pm = getattr(self, '_pixmap', None)
                if pm:
                    p.drawPixmap(0, 22, pm)
                    # Tint overlay — right-click to set
                    tint_a = getattr(self, '_tint_alpha', 0)
                    if tint_a > 0:
                        from PyQt6.QtGui import QColor as _TC
                        tc = QColor(getattr(self, '_tint_color',
                                            QColor(255, 255, 255)))
                        tc.setAlpha(tint_a)
                        p.fillRect(0, 22, sz, sz, tc)
                else:
                    p.fillRect(0, 22, sz, sz, _vp_bg)
                # Resize grip — bottom-right corner dots
                p.setPen(_hint_c)
                for gx, gy in [(sz-4,sz+18),(sz-4,sz+14),(sz-8,sz+18)]:
                    p.drawEllipse(gx, gy, 2, 2)
                # Border
                p.setPen(QPen(_brd_c, 1))
                p.drawRect(0, 0, sz - 1, sz + 22 - 1)
                # Crosshair
                mid = sz // 2
                p.setPen(QPen(QColor(255, 80, 80, 160), 1))
                p.drawLine(mid - 8, 22 + mid, mid + 8, 22 + mid)
                p.drawLine(mid, 22 + mid - 8, mid, 22 + mid + 8)
                p.end()

            def _change_mag(self, delta):
                self._mag[0] = max(2, min(32, self._mag[0] + delta))
                self._refresh()

            def _change_size(self, delta):
                self._sz[0] = max(100, min(400, self._sz[0] + delta))
                self._rebuild()
                self.update()

            def wheelEvent(self, ev):
                """Scroll to resize the overlay panel."""
                delta = 1 if ev.angleDelta().y() > 0 else -1
                self._change_size(delta * 20)
                ev.accept()

            def mousePressEvent(self, ev):
                sz = self._sz[0]
                x, y = ev.position().x(), ev.position().y()
                total_h = sz + 22
                # Resize grip: bottom-right 14×14 corner
                if x > sz - 14 and y > total_h - 14:
                    self._resize_drag  = True
                    self._resize_start = (ev.globalPosition().toPoint(), sz)
                    ev.accept(); return
                self._resize_drag = False
                if y < 22:   # header — check [+] [-] or start drag
                    btn_w = 18
                    minus_x = sz - btn_w - 2
                    plus_x  = minus_x - btn_w - 2
                    if x >= minus_x:
                        self._change_mag(-1)
                    elif x >= plus_x:
                        self._change_mag(1)
                    else:
                        self._drag_start = ev.globalPosition().toPoint() - self.pos()
                ev.accept()

            def contextMenuEvent(self, ev):
                """Right-click: tint controls."""
                from PyQt6.QtWidgets import QMenu, QWidgetAction, QSlider, QLabel, QColorDialog
                menu = QMenu(self)
                menu.addAction("No tint",   lambda: self._set_tint(0))
                menu.addAction("Light tint (25%)", lambda: self._set_tint(64))
                menu.addAction("Medium tint (50%)", lambda: self._set_tint(128))
                menu.addAction("Strong tint (75%)", lambda: self._set_tint(192))
                menu.addSeparator()
                menu.addAction("Pick tint colour…", self._pick_tint_color)
                menu.exec(ev.globalPos())

            def _set_tint(self, alpha):
                self._tint_alpha = alpha
                self._refresh()

            def _pick_tint_color(self):
                from PyQt6.QtWidgets import QColorDialog
                from PyQt6.QtGui import QColor
                c = QColorDialog.getColor(
                    getattr(self, "_tint_color", QColor(255,255,255)),
                    self, "Pick Tint Colour")
                if c.isValid():
                    self._tint_color = c
                    if not getattr(self, "_tint_alpha", 0):
                        self._tint_alpha = 64
                    self._refresh()

            def mouseMoveEvent(self, ev):
                if getattr(self, '_resize_drag', False) and \
                        ev.buttons() & Qt.MouseButton.LeftButton:
                    gp = ev.globalPosition().toPoint()
                    start_gp, start_sz = self._resize_start
                    new_sz = max(120, min(400, start_sz + gp.x() - start_gp.x()))
                    self._sz[0] = new_sz
                    self.setFixedSize(new_sz, new_sz + 22)
                    self.update()
                    ev.accept(); return
                if self._drag_start and ev.buttons() & Qt.MouseButton.LeftButton:
                    new_pos = ev.globalPosition().toPoint() - self._drag_start
                    pw = self.parent().width()  - self.width()
                    ph = self.parent().height() - self.height()
                    self.move(max(0, min(new_pos.x(), pw)),
                              max(0, min(new_pos.y(), ph)))
                ev.accept()

            def mouseReleaseEvent(self, ev):
                self._drag_start  = None
                self._resize_drag = False
                ev.accept()

        overlay = _ZoomOverlay(self)
        overlay.show()
        overlay.raise_()
        self._zoom_lens = overlay

    # ─────────────────────────────────────────────────────────────────────
    #  Canvas Tool Overlay — shared by Snow, Colour Adjust, Seamless
    # ─────────────────────────────────────────────────────────────────────
    class _ToolOverlay(QWidget): #vers 1
        """
        Lens-style overlay parented to the canvas viewport.
        Shows Original | Result side-by-side with scrollbars,
        height-capped, with action buttons at the bottom.
        """
        def __init__(self, parent_vp, workshop, title,
                     controls_widget, apply_fn, generate_fn=None):
            super().__init__(parent_vp)
            self._ws   = workshop
            self._apply_fn    = apply_fn
            self._generate_fn = generate_fn
            self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
            self.setAutoFillBackground(True)

            # Size: compact fixed width, capped height, bottom-left corner
            vp_w = parent_vp.width()  or 600
            vp_h = parent_vp.height() or 400
            panel_w = min(520, max(360, vp_w // 2))
            panel_h = min(340, max(240, vp_h // 2))
            self.setFixedWidth(panel_w)
            self.setFixedHeight(panel_h)
            # Anchor to bottom-left of viewport
            self.move(0, vp_h - panel_h)
            self.raise_()
            self.show()

            root = QVBoxLayout(self)
            root.setContentsMargins(4, 4, 4, 4)
            root.setSpacing(3)

            # Title bar
            title_row = QHBoxLayout()
            title_lbl = QLabel(f"  {title}")
            title_lbl.setFont(QFont("Arial", 9, QFont.Weight.Bold))
            close_btn = QPushButton()
            close_btn.setFixedSize(22, 22)
            close_btn.setFlat(True)
            close_btn.setToolTip("Close")
            close_btn.clicked.connect(self._close)
            try:
                _ic = workshop._get_icon_color() if hasattr(workshop, "_get_icon_color") else "#cccccc"
                close_btn.setIcon(SVGIconFactory.close_icon(16, _ic))
                close_btn.setIconSize(QSize(16, 16))
            except Exception:
                close_btn.setText("✕")
            title_row.addWidget(title_lbl, 1)
            title_row.addWidget(close_btn)
            root.addLayout(title_row)

            # Preview area — two scroll areas side by side
            preview_row = QHBoxLayout()
            preview_row.setSpacing(3)
            for side in ('orig', 'result'):
                col = QVBoxLayout()
                hdr = QLabel("Original" if side == 'orig' else "Result")
                hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
                hdr.setFont(QFont("Arial", 8))
                col.addWidget(hdr)
                sa = QScrollArea()
                sa.setWidgetResizable(True)
                sa.setHorizontalScrollBarPolicy(
                    Qt.ScrollBarPolicy.ScrollBarAsNeeded)
                sa.setVerticalScrollBarPolicy(
                    Qt.ScrollBarPolicy.ScrollBarAsNeeded)
                lbl = QLabel()
                lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
                lbl.setMinimumSize(80, 80)
                sa.setWidget(lbl)
                col.addWidget(sa, 1)
                preview_row.addLayout(col, 1)
                if side == 'orig':
                    self._orig_lbl = lbl
                else:
                    self._result_lbl = lbl
            root.addLayout(preview_row, 1)

            # Controls + buttons
            ctrl_row = QHBoxLayout()
            ctrl_row.setSpacing(6)
            if controls_widget:
                ctrl_row.addWidget(controls_widget, 1)

            btn_col = QVBoxLayout()
            apply_btn = QPushButton("Apply")
            apply_btn.clicked.connect(self._apply)
            btn_col.addWidget(apply_btn)
            if generate_fn:
                gen_btn = QPushButton("Generate")
                gen_btn.clicked.connect(self._generate)
                btn_col.addWidget(gen_btn)
            close_btn2 = QPushButton("Cancel")
            close_btn2.clicked.connect(self._close)
            btn_col.addWidget(close_btn2)
            btn_col.addStretch()
            ctrl_row.addLayout(btn_col)
            root.addLayout(ctrl_row)

        def set_orig_pixmap(self, pm):
            if not pm.isNull():
                sz = self._orig_lbl.size()
                self._orig_lbl.setPixmap(
                    pm.scaled(max(80, sz.width()), max(80, sz.height()),
                              Qt.AspectRatioMode.KeepAspectRatio,
                              Qt.TransformationMode.SmoothTransformation))

        def set_result_pixmap(self, pm):
            if not pm.isNull():
                sz = self._result_lbl.size()
                self._result_lbl.setPixmap(
                    pm.scaled(max(80, sz.width()), max(80, sz.height()),
                              Qt.AspectRatioMode.KeepAspectRatio,
                              Qt.TransformationMode.SmoothTransformation))

        def _generate(self):
            if self._generate_fn:
                self._generate_fn()

        def _apply(self):
            if self._apply_fn:
                self._apply_fn()
            self._close()

        def _close(self):
            self.hide()
            self.deleteLater()

        def resizeEvent(self, e):
            super().resizeEvent(e)


    #    Image tools (seamless, colour correction, snow, sharpen, blur)          

    def _open_dp5_seamless(self): #vers 2
        """Seamless tiling — inline canvas overlay."""
        if not self.dp5_canvas: return
        try:
            from apps.methods.txd_tools import rgba_to_qpixmap, _apply_seamless, _preview_bg
            from PyQt6.QtWidgets import (QWidget, QHBoxLayout, QVBoxLayout,
                QLabel, QSlider, QSpinBox, QComboBox, QScrollArea)
            rgba = bytes(self.dp5_canvas.rgba)
            w, h = self.dp5_canvas.tex_w, self.dp5_canvas.tex_h

            ctrl = QWidget()
            cl = QVBoxLayout(ctrl); cl.setContentsMargins(0,0,0,0); cl.setSpacing(2)

            cl.addWidget(QLabel("Method:"))
            _mode = QComboBox()
            _mode.addItems(["Wrap Blend", "Patch / Heal",
                             "Histogram Blend", "Offset & Mirror"])
            cl.addWidget(_mode)

            blend_row = QHBoxLayout()
            blend_row.addWidget(QLabel("Blend %:"))
            _blend_sl = QSlider(Qt.Orientation.Horizontal)
            _blend_sl.setRange(5, 50); _blend_sl.setValue(25)
            _blend_sp = QSpinBox(); _blend_sp.setRange(5, 50); _blend_sp.setValue(25)
            _blend_sl.valueChanged.connect(_blend_sp.setValue)
            _blend_sp.valueChanged.connect(_blend_sl.setValue)
            blend_row.addWidget(_blend_sl, 1); blend_row.addWidget(_blend_sp)
            cl.addLayout(blend_row)

            cl.addWidget(QLabel("Preview tiling:"))
            _tile_mode = QComboBox()
            _tile_mode.addItems(["1×1", "2×2", "3×3"])
            _tile_mode.setCurrentIndex(2)
            cl.addWidget(_tile_mode)

            _result = [None]
            parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

            def _gen():
                result = _apply_seamless(rgba, w, h,
                    mode=_mode.currentIndex(),
                    blend=_blend_sl.value() / 100.0)
                _result[0] = result
                n = _tile_mode.currentIndex() + 1
                from apps.methods.txd_tools import _tile_rgba
                tiled = _tile_rgba(result, w, h, n)
                pm = rgba_to_qpixmap(tiled, w*n, h*n, _preview_bg(overlay))
                overlay.set_result_pixmap(pm)

            def _apply():
                if _result[0]:
                    self._push_undo()
                    self.dp5_canvas.rgba = bytearray(_result[0])
                    self.dp5_canvas.update()
                    self._set_status("Seamless applied")

            _tile_mode.currentIndexChanged.connect(lambda _: _gen() if _result[0] else None)

            overlay = self._ToolOverlay(
                parent_vp, self, "Seamless — Canvas",
                ctrl, apply_fn=_apply, generate_fn=_gen)

            n = _tile_mode.currentIndex() + 1
            from apps.methods.txd_tools import _tile_rgba
            tiled_orig = _tile_rgba(rgba, w, h, n)
            pm_orig = rgba_to_qpixmap(tiled_orig, w*n, h*n, _preview_bg(overlay))
            overlay.set_orig_pixmap(pm_orig)

        except Exception as e:
            self._set_status(f"Seamless error: {e}")

    def _open_dp5_colour_adjust(self): #vers 2
        """Colour adjustments — inline canvas overlay."""
        if not self.dp5_canvas: return
        try:
            from apps.methods.txd_tools import rgba_to_qpixmap, _apply_colour_adjust, _preview_bg
            from PyQt6.QtWidgets import (QWidget, QHBoxLayout, QVBoxLayout,
                QLabel, QSlider, QSpinBox, QCheckBox, QScrollArea)
            rgba = bytes(self.dp5_canvas.rgba)
            w, h = self.dp5_canvas.tex_w, self.dp5_canvas.tex_h

            ctrl = QWidget()
            cl = QVBoxLayout(ctrl); cl.setContentsMargins(0,0,0,0); cl.setSpacing(2)

            sliders = {}
            def _sl(label, lo, hi, val):
                row = QHBoxLayout()
                row.addWidget(QLabel(f"{label}:"))
                sl = QSlider(Qt.Orientation.Horizontal)
                sl.setRange(lo, hi); sl.setValue(val)
                sp = QSpinBox(); sp.setRange(lo, hi); sp.setValue(val)
                sl.valueChanged.connect(sp.setValue)
                sp.valueChanged.connect(sl.setValue)
                row.addWidget(sl, 1); row.addWidget(sp)
                cl.addLayout(row)
                sliders[label] = sl
                return sl

            _sl("Brightness", -100, 100, 0)
            _sl("Contrast",   -100, 100, 0)
            _sl("Hue",        -180, 180, 0)
            _sl("Saturation", -100, 100, 0)
            _sl("Sharpness",     0, 200, 100)
            _sl("Opacity",       0, 100, 100)

            _result = [None]
            parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

            def _gen():
                result = _apply_colour_adjust(rgba, w, h,
                    brightness=sliders["Brightness"].value(),
                    contrast=sliders["Contrast"].value(),
                    hue=sliders["Hue"].value(),
                    saturation=sliders["Saturation"].value(),
                    sharpness=sliders["Sharpness"].value() / 100.0,
                    opacity=sliders["Opacity"].value() / 100.0)
                _result[0] = result
                pm = rgba_to_qpixmap(result, w, h, _preview_bg(overlay))
                overlay.set_result_pixmap(pm)

            def _apply():
                if _result[0]:
                    self._push_undo()
                    self.dp5_canvas.rgba = bytearray(_result[0])
                    self.dp5_canvas.update()
                    self._set_status("Colour adjustments applied")

            # Wire sliders to live preview
            for sl in sliders.values():
                sl.valueChanged.connect(lambda _: _gen())

            overlay = self._ToolOverlay(
                parent_vp, self, "Colour Adjust — Canvas",
                ctrl, apply_fn=_apply, generate_fn=_gen)

            pm_orig = rgba_to_qpixmap(rgba, w, h, _preview_bg(overlay))
            overlay.set_orig_pixmap(pm_orig)
            _gen()  # initial preview

        except Exception as e:
            self._set_status(f"Colour adjust error: {e}")

    def _open_dp5_snow(self): #vers 2
        """Snow effect generator — inline canvas overlay."""
        if not self.dp5_canvas: return
        try:
            from apps.methods.txd_tools import rgba_to_qpixmap, _apply_snow, _preview_bg
            from PyQt6.QtWidgets import (QWidget, QHBoxLayout, QVBoxLayout,
                QLabel, QSlider, QSpinBox, QGroupBox, QScrollArea)
            rgba = bytes(self.dp5_canvas.rgba)
            w, h = self.dp5_canvas.tex_w, self.dp5_canvas.tex_h

            # Build compact controls
            ctrl = QWidget()
            cl = QVBoxLayout(ctrl); cl.setContentsMargins(0,0,0,0); cl.setSpacing(2)

            def _sl(label, lo, hi, val):
                row = QHBoxLayout()
                row.addWidget(QLabel(f"{label}:"))
                sl = QSlider(Qt.Orientation.Horizontal)
                sl.setRange(lo, hi); sl.setValue(val)
                sp = QSpinBox(); sp.setRange(lo, hi); sp.setValue(val)
                sl.valueChanged.connect(sp.setValue)
                sp.valueChanged.connect(sl.setValue)
                row.addWidget(sl, 1); row.addWidget(sp)
                cl.addLayout(row)
                return sl

            _threshold = _sl("B/W Threshold", 0, 255, 180)
            _depth     = _sl("Surface Depth", 0, 100, 30)
            _coverage  = _sl("Coverage %",    0, 100, 70)
            _layers    = _sl("Layers",        1, 8,    3)
            _tile      = _sl("Tile",          1, 8,    2)

            _result = [None]
            parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

            overlay = self._ToolOverlay(
                parent_vp, self, "Snow — Canvas", ctrl,
                apply_fn=None, generate_fn=None)

            def _gen():
                result = _apply_snow(rgba, w, h,
                    threshold=_threshold.value(),
                    depth=_depth.value() / 100.0,
                    coverage=_coverage.value() / 100.0,
                    layers=_layers.value(),
                    tile=_tile.value())
                _result[0] = result
                pm = rgba_to_qpixmap(result, w, h, _preview_bg(overlay))
                overlay.set_result_pixmap(pm)

            def _apply():
                if _result[0]:
                    self._push_undo()
                    self.dp5_canvas.rgba = bytearray(_result[0])
                    self.dp5_canvas.update()
                    self._set_status("Snow applied")

            overlay._generate_fn = _gen
            overlay._apply_fn = _apply

            pm_orig = rgba_to_qpixmap(rgba, w, h, _preview_bg(overlay))
            overlay.set_orig_pixmap(pm_orig)

        except Exception as e:
            self._set_status(f"Snow error: {e}")

    def _open_icon_browser(self): #vers 2
        """Open SVG icon browser as floating panel — click icon to load into canvas."""
        try:
            from apps.components.DP5_Workshop.svg_icon_browser import SVGIconBrowser
            if not hasattr(self, '_svg_browser_panel'):
                self._svg_browser_panel = SVGIconBrowser(workshop=self, parent=None)

            if self._svg_browser_panel.isVisible():
                self._svg_browser_panel.hide()
            else:
                # Position to the left of the DP5 window
                pos = self.mapToGlobal(self.rect().topLeft())
                pw  = self._svg_browser_panel.width()
                self._svg_browser_panel.move(
                    max(0, pos.x() - pw - 6), pos.y() + 40)
                self._svg_browser_panel.resize(220, self.height() - 60)
                self._svg_browser_panel.show()
                self._svg_browser_panel.raise_()
        except Exception as e:
            self._set_status(f"Icon Browser error: {e}")

    def _dp5_sharpen(self, amount: float = 1.5): #vers 1
        """Sharpen the canvas using PIL ImageFilter."""
        if not self.dp5_canvas: return
        try:
            from PIL import Image, ImageFilter
            rgba = bytes(self.dp5_canvas.rgba)
            w, h = self.dp5_canvas.tex_w, self.dp5_canvas.tex_h
            img = Image.frombytes('RGBA', (w, h), rgba)
            # Unsharp mask: radius, percent, threshold
            sharpened = img.filter(ImageFilter.UnsharpMask(
                radius=1, percent=int(amount * 100), threshold=3))
            self._push_undo()
            self.dp5_canvas.rgba = bytearray(sharpened.tobytes())
            self.dp5_canvas.update()
            self._set_status(f"Sharpened ×{amount}")
        except Exception as e:
            self._set_status(f"Sharpen error: {e}")

    def _dp5_blur(self, radius: float = 1.0): #vers 1
        """Gaussian blur the canvas."""
        if not self.dp5_canvas: return
        try:
            from PIL import Image, ImageFilter
            rgba = bytes(self.dp5_canvas.rgba)
            w, h = self.dp5_canvas.tex_w, self.dp5_canvas.tex_h
            img = Image.frombytes('RGBA', (w, h), rgba)
            blurred = img.filter(ImageFilter.GaussianBlur(radius=radius))
            self._push_undo()
            self.dp5_canvas.rgba = bytearray(blurred.tobytes())
            self.dp5_canvas.update()
            self._set_status(f"Blurred r={radius}")
        except Exception as e:
            self._set_status(f"Blur error: {e}")

    def _dp5_emboss(self): #vers 1
        """Emboss filter."""
        if not self.dp5_canvas: return
        try:
            from PIL import Image, ImageFilter
            rgba = bytes(self.dp5_canvas.rgba)
            w, h = self.dp5_canvas.tex_w, self.dp5_canvas.tex_h
            img = Image.frombytes('RGBA', (w, h), rgba)
            rgb = img.convert('RGB').filter(ImageFilter.EMBOSS).convert('RGBA')
            # Preserve original alpha
            r, g, b, _ = rgb.split()
            _, _, _, a  = img.split()
            result = Image.merge('RGBA', (r, g, b, a))
            self._push_undo()
            self.dp5_canvas.rgba = bytearray(result.tobytes())
            self.dp5_canvas.update()
            self._set_status("Emboss applied")
        except Exception as e:
            self._set_status(f"Emboss error: {e}")

    def _dp5_edge_detect(self): #vers 1
        """Edge detection filter."""
        if not self.dp5_canvas: return
        try:
            from PIL import Image, ImageFilter
            rgba = bytes(self.dp5_canvas.rgba)
            w, h = self.dp5_canvas.tex_w, self.dp5_canvas.tex_h
            img = Image.frombytes('RGBA', (w, h), rgba)
            rgb = img.convert('RGB').filter(ImageFilter.FIND_EDGES).convert('RGBA')
            r, g, b, _ = rgb.split()
            _, _, _, a  = img.split()
            result = Image.merge('RGBA', (r, g, b, a))
            self._push_undo()
            self.dp5_canvas.rgba = bytearray(result.tobytes())
            self.dp5_canvas.update()
            self._set_status("Edge detect applied")
        except Exception as e:
            self._set_status(f"Edge detect error: {e}")

    #    Settings / theme                                                       

    def _show_workshop_settings(self): #vers 1
        """Open DP5-specific settings dialog (NOT the global theme dialog)."""
        old_icon_sz = self.dp5_settings.get('tool_icon_size')
        old_cols    = self.dp5_settings.get('tool_columns')

        dlg = DP5SettingsDialog(self.dp5_settings, self)
        if dlg.exec():
            # Apply menu bar style changes live
            self._apply_menu_bar_style()
            # Also apply orientation change if it changed
            self.set_menu_orientation(self.dp5_settings.get('menu_style', 'topbar'))

            # Apply changed settings live
            show_left = self.dp5_settings.get('show_bitmap_list')
            if hasattr(self, '_left_panel'):
                self._left_panel.setVisible(show_left)
            if hasattr(self, '_status_bar'):
                self._status_bar.setVisible(self.dp5_settings.get('show_statusbar'))

            if self.dp5_canvas:
                self.dp5_canvas.show_grid = self.dp5_settings.get('show_pixel_grid')
                self.dp5_canvas.grid_color = QColor(self.dp5_settings.get('grid_color'))
                self.dp5_canvas.show_cell_grid = self.dp5_settings.get('show_cell_grid')
                self.dp5_canvas.update()
                if self.dp5_settings.get('zoom_to_fit_resize'):
                    self._fit_canvas_to_viewport()
            self._set_platform(self.dp5_settings.get('platform_mode'))

            show_mb = (self.dp5_settings.get('show_menubar') and
                       self.dp5_settings.get('menu_style') == 'topbar')
            c = getattr(self, '_menu_bar_container', self._menu_bar if hasattr(self, '_menu_bar') else None)
            if c:
                c.setMinimumHeight(0)
                c.setMaximumHeight(16777215 if show_mb else 0)
                c.setVisible(show_mb)

            # If icon size or column count changed, rebuild the right panel
            new_icon_sz = self.dp5_settings.get('tool_icon_size')
            new_cols    = self.dp5_settings.get('tool_columns')
            if new_icon_sz != old_icon_sz or new_cols != old_cols:
                self._rebuild_right_panel()

            self._set_status("Settings saved.")


    def _rebuild_right_panel(self): #vers 2
        """Tear down and reconstruct the right panel in-place.
        Called after settings change or splitter resize."""
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

    def _on_splitter_moved(self, pos: int, index: int): #vers 2
        """Reflow gadget grid columns when splitter is dragged.
        Reads the panel's actual width, computes how many columns fit,
        and rebuilds only when the column count changes."""
        if not hasattr(self, '_splitter'): return
        # Get actual panel width from the widget itself (not splitter sizes,
        # which can be stale mid-drag)
        right_panel = self._splitter.widget(self._splitter.count() - 1)
        if right_panel is None: return
        new_w = right_panel.width()

        icon_sz  = self.dp5_settings.get('tool_icon_size')
        btn_sz   = icon_sz + 6
        gap      = 2
        min_cols = max(2, self.dp5_settings.get('tool_columns'))
        usable   = max(btn_sz, new_w - 20)
        new_cols = max(min_cols, usable // (btn_sz + gap))
        old_cols = getattr(self, '_n_cols', 0)

        # Only rebuild when column count actually changes
        if new_cols != old_cols:
            self._rebuild_right_panel()


    def _export_bitmap(self): #vers 1
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


    def _export_iff(self): #vers 1
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


    def _export_iff_ham(self): #vers 2
        """Export Amiga IFF ILBM in HAM6 mode (6 bitplanes, CAMG=0x800)."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export IFF HAM", "image_ham.iff", "IFF ILBM (*.iff)")
        if not path: return
        try:
            import struct
            w, h = self._canvas_width, self._canvas_height
            # Build 16-colour base palette
            if hasattr(self,'_user_pal_grid') and self._user_pal_grid._colors:
                base_pal = [(c.red(),c.green(),c.blue())
                            for c in self._user_pal_grid._colors[:16]]
            else:
                base_pal = [(i*17,i*17,i*17) for i in range(16)]
            while len(base_pal) < 16: base_pal.append((0,0,0))

            def nearest_base(r,g,b):
                return min(range(16),
                    key=lambda i:(base_pal[i][0]-r)**2+(base_pal[i][1]-g)**2+(base_pal[i][2]-b)**2)

            # HAM6 encode — 6 bits per pixel, 6 bitplanes
            # control bits 5-4: 00=palette, 01=mod B, 10=mod R, 11=mod G
            # bits 3-0: 4-bit value
            rows = []
            for ty in range(h):
                pr,pg,pb = 0,0,0
                row = []
                for tx in range(w):
                    i = (ty*w+tx)*4
                    r,g,b = self.dp5_canvas.rgba[i:i+3]
                    bi = nearest_base(r,g,b)
                    bc = base_pal[bi]
                    err_idx = (bc[0]-r)**2+(bc[1]-g)**2+(bc[2]-b)**2
                    r4,g4,b4 = r>>4, g>>4, b>>4
                    err_r = (r4*17-r)**2+(pg-g)**2+(pb-b)**2
                    err_g = (pr-r)**2+(g4*17-g)**2+(pb-b)**2
                    err_b = (pr-r)**2+(pg-g)**2+(b4*17-b)**2
                    best = min(err_idx,err_r,err_g,err_b)
                    if best == err_idx:
                        row.append(bi); pr,pg,pb = bc
                    elif best == err_r:
                        row.append(0x20|r4); pr = r4*17
                    elif best == err_g:
                        row.append(0x30|g4); pg = g4*17
                    else:
                        row.append(0x10|b4); pb = b4*17
                rows.append(row)

            # Pack 6 bitplanes per row
            n_planes = 6
            row_bytes = (w + 15) // 16 * 2  # words per row
            body = bytearray()
            for row in rows:
                planes = [bytearray(row_bytes) for _ in range(n_planes)]
                for x, px in enumerate(row):
                    for p in range(n_planes):
                        if px & (1 << p):
                            byte_i = x // 8
                            planes[p][byte_i] |= 0x80 >> (x % 8)
                for p in planes:
                    body += bytes(p)

            def chunk(tag, data):
                d = bytes(data)
                c = tag.encode() + struct.pack('>I', len(d)) + d
                if len(d) % 2: c += b'\x00'
                return c

            bmhd = struct.pack('>HHhhBBBBHBBhh',
                w, h, 0, 0, n_planes, 0, 0, 0, 1, 8, 8, w, h)
            cmap = b''.join(bytes(c) for c in base_pal)
            camg = struct.pack('>I', 0x0800)  # HAM mode flag
            form_data = (b'ILBM' + chunk('BMHD', bmhd) +
                         chunk('CMAP', cmap) + chunk('CAMG', camg) +
                         chunk('BODY', body))
            out = b'FORM' + struct.pack('>I', len(form_data)) + form_data
            open(path,'wb').write(out)
            self._set_status(f"Exported IFF HAM: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "IFF HAM Export Error", str(e))


    def _export_scr(self): #vers 1
        """Export ZX Spectrum SCR (256×192, 6144 bitmap + 768 attr bytes)."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export ZX Spectrum SCR", "screen.scr", "SCR (*.scr)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                                  bytes(self.dp5_canvas.rgba)).convert('RGB')
            img = img.resize((256, 192), Image.NEAREST)
            # Quantize to ZX Spectrum 8 colours
            zx_pal = [0,0,0, 215,0,0, 0,215,0, 215,215,0,
                      0,0,215, 215,0,215, 0,215,215, 215,215,215]
            pal_img = Image.new('P', (1,1))
            pal_img.putpalette(zx_pal + [0]*768)
            q = img.quantize(palette=pal_img, dither=0)
            pixels = list(q.getdata())
            # Build bitmap (6144 bytes) — ZX order: 8 rows of 8, per third
            bitmap = bytearray(6144)
            for third in range(3):
                for row in range(8):
                    for char_y in range(8):
                        y = third*64 + char_y*8 + row
                        for char_x in range(32):
                            byte = 0
                            for bit in range(8):
                                px = pixels[y*256 + char_x*8 + bit]
                                if px & 1:
                                    byte |= (0x80 >> bit)
                            addr = third*2048 + row*256 + char_y*32 + char_x
                            bitmap[addr] = byte
            # Build attrs (768 bytes) — simplified: ink=colour, paper=0
            attrs = bytearray(768)
            for ay in range(24):
                for ax in range(32):
                    px = pixels[(ay*8)*256 + ax*8]
                    attrs[ay*32+ax] = px & 7
            open(path, 'wb').write(bitmap + attrs)
            self._set_status(f"Exported SCR: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "SCR Export Error", str(e))


    def _export_sc2(self): #vers 1
        """Export MSX SC2 raw (256×192, 6144 pattern + 6144 colour + 768 name)."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export MSX SC2", "screen.sc2", "SC2 (*.sc2)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                                  bytes(self.dp5_canvas.rgba)).convert('RGB')
            img = img.resize((256, 192), Image.NEAREST)
            q = img.quantize(colors=16, dither=0)
            pixels = list(q.getdata())
            pal_flat = q.getpalette()
            pattern = bytearray(6144)
            colour  = bytearray(6144)
            name    = bytearray(768)
            for by in range(24):
                for bx in range(32):
                    name[by*32+bx] = by*32+bx
                    for row in range(8):
                        y = by*8+row
                        idx = (by*32+bx)*8+row
                        byte = 0
                        fg = pixels[y*256+bx*8] & 0xF
                        bg = 0
                        for bit in range(8):
                            px = pixels[y*256+bx*8+bit] & 0xF
                            if px == fg:
                                byte |= (0x80>>bit)
                        pattern[idx] = byte
                        colour[idx]  = (fg<<4)|bg
            open(path,'wb').write(pattern+colour+name)
            self._set_status(f"Exported SC2: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "SC2 Export Error", str(e))


    def _export_pi1(self): #vers 1
        """Export Atari ST Degas PI1 (320×200, 16 colours, 4 bitplanes)."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Atari ST PI1", "image.pi1", "PI1 (*.pi1)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                                  bytes(self.dp5_canvas.rgba)).convert('RGB')
            img = img.resize((320, 200), Image.NEAREST)
            q = img.quantize(colors=16, dither=0)
            pixels = list(q.getdata())
            pal_flat = q.getpalette()
            # Header: resolution word (0=low)
            out = bytearray()
            out += (0).to_bytes(2,'big')
            # Palette: 16 × 3-nibble ST colour (0RGB, 3 bits each)
            for i in range(16):
                r = pal_flat[i*3]>>5
                g = pal_flat[i*3+1]>>5
                b = pal_flat[i*3+2]>>5
                word = (r<<8)|(g<<4)|b
                out += word.to_bytes(2,'big')
            # Bitmap: 4 interleaved bitplanes, 80 bytes per row
            for y in range(200):
                for word_x in range(20):
                    planes = [0,0,0,0]
                    for bit in range(16):
                        px = pixels[y*320 + word_x*16 + bit]
                        for p in range(4):
                            if px & (1<<p):
                                planes[p] |= (0x8000>>bit)
                    for p in range(4):
                        out += planes[p].to_bytes(2,'big')
            open(path,'wb').write(out)
            self._set_status(f"Exported PI1: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "PI1 Export Error", str(e))


    def _export_koala(self): #vers 1
        """Export C64 Koala multicolour (160×200, 3 colours+bg per 4×8 cell)."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export C64 Koala", "image.kla", "Koala (*.kla)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                                  bytes(self.dp5_canvas.rgba)).convert('RGB')
            img = img.resize((160, 200), Image.NEAREST)
            q = img.quantize(colors=16, dither=0)
            pixels = list(q.getdata())
            # Koala: 2-byte load addr + 8000 bitmap + 1000 screen + 1000 colram + 1 bg
            out = bytearray(b'\x00\x60')  # load address $6000
            # Bitmap: 8000 bytes, each byte = 4 pairs of 2-bit colour indices
            bitmap = bytearray(8000)
            screen = bytearray(1000)
            colram = bytearray(1000)
            for cell_y in range(25):
                for cell_x in range(40):
                    cell_idx = cell_y*40+cell_x
                    # collect colours in this cell
                    cell_colours = {}
                    for row in range(8):
                        for col in range(2):  # 2 pixels wide per nibble in 160px
                            px = pixels[(cell_y*8+row)*160 + cell_x*2+col]
                            cell_colours[px] = cell_colours.get(px,0)+1
                    # most common = screen colour, second = colram
                    sorted_c = sorted(cell_colours, key=lambda k: -cell_colours[k])
                    bg = 0
                    c1 = sorted_c[0] if len(sorted_c)>0 else 0
                    c2 = sorted_c[1] if len(sorted_c)>1 else 0
                    c3 = sorted_c[2] if len(sorted_c)>2 else 0
                    screen[cell_idx] = (c1<<4)|c2
                    colram[cell_idx] = c3 & 0xF
                    colour_map = {bg:0, c1:1, c2:2, c3:3}
                    for row in range(8):
                        byte = 0
                        for col in range(4):
                            px = pixels[(cell_y*8+row)*160 + cell_x*4+col] if cell_x*4+col < 160 else 0
                            pair = colour_map.get(px, 0)
                            byte = (byte<<2)|pair
                        bitmap[cell_idx*8+row] = byte
            out += bitmap + screen + colram + bytearray(b'\x00')
            open(path,'wb').write(out)
            self._set_status(f"Exported Koala: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "Koala Export Error", str(e))


    def _export_art_studio(self): #vers 1
        """Export C64 Art Studio hires (320×200, 2 colours per 8×8 cell)."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export C64 Art Studio", "image.art", "Art Studio (*.art)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                                  bytes(self.dp5_canvas.rgba)).convert('RGB')
            img = img.resize((320, 200), Image.NEAREST)
            q = img.quantize(colors=16, dither=0)
            pixels = list(q.getdata())
            # Art Studio: 2-byte header + 8000 bitmap + 1000 colour + 1 border + 6 footer
            out = bytearray(b'\x00\x20')  # load $2000
            bitmap = bytearray(8000)
            colour = bytearray(1000)
            for cell_y in range(25):
                for cell_x in range(40):
                    cell_idx = cell_y*40+cell_x
                    cell_pxs = [pixels[(cell_y*8+r)*320+cell_x*8+c]
                                 for r in range(8) for c in range(8)]
                    counts = {}
                    for p in cell_pxs: counts[p]=counts.get(p,0)+1
                    sorted_c = sorted(counts, key=lambda k:-counts[k])
                    fg = sorted_c[0] if sorted_c else 0
                    bg = sorted_c[1] if len(sorted_c)>1 else 0
                    colour[cell_idx] = (fg<<4)|bg
                    for row in range(8):
                        byte = 0
                        for bit in range(8):
                            px = pixels[(cell_y*8+row)*320+cell_x*8+bit]
                            if px == fg: byte |= (0x80>>bit)
                        bitmap[cell_idx*8+row] = byte
            out += bitmap + colour + b'\x00' + b'\x4D\x20\x27\x53\x54\x55'
            open(path,'wb').write(out)
            self._set_status(f"Exported Art Studio: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "Art Studio Export Error", str(e))


    #    Platform format imports                                                

    def _import_scr(self): #vers 1
        """Import ZX Spectrum SCR (6144 bitmap + 768 attr bytes → 256×192 RGBA)."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import ZX Spectrum SCR", "", "SCR (*.scr);;All Files (*)")
        if not path: return
        try:
            data = open(path,'rb').read()
            if len(data) < 6912:
                raise ValueError(f"SCR too small: {len(data)} bytes")
            bitmap = data[:6144]; attrs = data[6144:6912]
            ZX_COLS = [
                (0,0,0),(215,0,0),(0,215,0),(215,215,0),
                (0,0,215),(215,0,215),(0,215,215),(215,215,215),
                (0,0,0),(255,0,0),(0,255,0),(255,255,0),
                (0,0,255),(255,0,255),(0,255,255),(255,255,255),
            ]
            w,h = 256,192
            rgba = bytearray(w*h*4)
            for ay in range(24):
                for ax in range(32):
                    attr = attrs[ay*32+ax]
                    bright = (attr>>6)&1
                    ink   = (attr&7) + (8 if bright else 0)
                    paper = ((attr>>3)&7) + (8 if bright else 0)
                    ink_c   = ZX_COLS[ink]
                    paper_c = ZX_COLS[paper]
                    for row in range(8):
                        y = ay*8+row
                        # ZX addressing: third/row/char
                        third   = y//64
                        char_y  = (y%64)//8
                        scan    = y%8
                        baddr   = third*2048 + scan*256 + char_y*32 + ax
                        byte    = bitmap[baddr]
                        for bit in range(8):
                            x = ax*8+bit
                            on = (byte >> (7-bit)) & 1
                            c = ink_c if on else paper_c
                            i = (y*w+x)*4
                            rgba[i:i+4] = [c[0],c[1],c[2],255]
            self._load_rgba(rgba, w, h, os.path.basename(path))
        except Exception as e:
            QMessageBox.warning(self, "SCR Import Error", str(e))


    def _import_sc2(self): #vers 1
        """Import MSX SC2 (pattern+colour+name tables → 256×192 RGBA)."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import MSX SC2", "", "SC2 (*.sc2);;All Files (*)")
        if not path: return
        try:
            data = open(path,'rb').read()
            if len(data) < 13312: raise ValueError("SC2 too small")
            pattern = data[:6144]; colour = data[6144:12288]
            w,h = 256,192
            rgba = bytearray(w*h*4)
            MSX_PAL = [
                (0,0,0),(0,0,0),(62,184,73),(116,208,128),
                (89,85,224),(128,118,241),(185,94,81),(101,219,239),
                (219,101,89),(255,137,125),(204,195,94),(222,208,135),
                (58,162,65),(183,102,181),(204,204,204),(255,255,255),
            ]
            for by in range(24):
                for bx in range(32):
                    for row in range(8):
                        y = by*8+row
                        idx = (by*32+bx)*8+row
                        pat_byte  = pattern[idx]
                        col_byte  = colour[idx]
                        fg_idx = (col_byte>>4)&0xF
                        bg_idx = col_byte&0xF
                        fg = MSX_PAL[fg_idx]; bg = MSX_PAL[bg_idx]
                        for bit in range(8):
                            x = bx*8+bit
                            on = (pat_byte>>(7-bit))&1
                            c = fg if on else bg
                            i = (y*w+x)*4
                            rgba[i:i+4] = [c[0],c[1],c[2],255]
            self._load_rgba(rgba, w, h, os.path.basename(path))
        except Exception as e:
            QMessageBox.warning(self, "SC2 Import Error", str(e))


    def _import_pi1(self): #vers 1
        """Import Atari ST Degas PI1 (320×200, 4 bitplanes) → RGBA."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import Atari ST PI1", "", "PI1 (*.pi1);;All Files (*)")
        if not path: return
        try:
            data = open(path,'rb').read()
            if len(data) < 32034: raise ValueError("PI1 too small")
            # Parse 16-colour palette (words at offset 2)
            pal = []
            for i in range(16):
                word = (data[2+i*2]<<8)|data[3+i*2]
                r = ((word>>8)&7)*36; g = ((word>>4)&7)*36; b = (word&7)*36
                pal.append((r,g,b))
            # Bitmap: 4 interleaved bitplanes, 16-bit words
            w,h = 320,200
            rgba = bytearray(w*h*4)
            offset = 34
            for y in range(200):
                for word_x in range(20):
                    planes = []
                    for p in range(4):
                        planes.append((data[offset]<<8)|data[offset+1])
                        offset += 2
                    for bit in range(16):
                        x = word_x*16+bit
                        px = 0
                        for p in range(4):
                            if planes[p] & (0x8000>>bit):
                                px |= (1<<p)
                        c = pal[px]
                        i = (y*w+x)*4
                        rgba[i:i+4] = [c[0],c[1],c[2],255]
            self._load_rgba(rgba, w, h, os.path.basename(path))
        except Exception as e:
            QMessageBox.warning(self, "PI1 Import Error", str(e))


    def _import_koala(self): #vers 1
        """Import C64 Koala multicolour (160×200) → RGBA."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import C64 Koala", "", "Koala (*.kla *.koa);;All Files (*)")
        if not path: return
        try:
            data = open(path,'rb').read()
            # May have 2-byte load address prefix
            offset = 2 if len(data) == 10003 else 0
            bitmap = data[offset:offset+8000]
            screen = data[offset+8000:offset+9000]
            colram = data[offset+9000:offset+10000]
            bg     = data[offset+10000] & 0xF
            C64_PAL = [
                (0,0,0),(255,255,255),(136,0,0),(170,255,238),
                (204,68,204),(0,204,85),(0,0,170),(238,238,119),
                (221,136,85),(102,68,0),(255,119,119),(51,51,51),
                (119,119,119),(170,255,102),(0,136,255),(187,187,187),
            ]
            w,h = 160,200
            rgba = bytearray(w*h*4)
            for cell_y in range(25):
                for cell_x in range(40):
                    cell_idx = cell_y*40+cell_x
                    sc = screen[cell_idx]
                    c1 = (sc>>4)&0xF; c2 = sc&0xF
                    c3 = colram[cell_idx]&0xF
                    colour_map = {0:C64_PAL[bg],1:C64_PAL[c1],2:C64_PAL[c2],3:C64_PAL[c3]}
                    for row in range(8):
                        byte = bitmap[cell_idx*8+row]
                        for col in range(4):
                            pair = (byte>>(6-col*2))&3
                            c = colour_map[pair]
                            x = cell_x*4+col; y = cell_y*8+row
                            i = (y*w+x)*4
                            rgba[i:i+4] = [c[0],c[1],c[2],255]
            self._load_rgba(rgba, w, h, os.path.basename(path))
        except Exception as e:
            QMessageBox.warning(self, "Koala Import Error", str(e))


    def _import_art_studio(self): #vers 1
        """Import C64 Art Studio hires (320×200) → RGBA."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import C64 Art Studio", "", "Art Studio (*.art);;All Files (*)")
        if not path: return
        try:
            data = open(path,'rb').read()
            offset = 2  # skip load address
            bitmap = data[offset:offset+8000]
            colour = data[offset+8000:offset+9000]
            C64_PAL = [
                (0,0,0),(255,255,255),(136,0,0),(170,255,238),
                (204,68,204),(0,204,85),(0,0,170),(238,238,119),
                (221,136,85),(102,68,0),(255,119,119),(51,51,51),
                (119,119,119),(170,255,102),(0,136,255),(187,187,187),
            ]
            w,h = 320,200
            rgba = bytearray(w*h*4)
            for cell_y in range(25):
                for cell_x in range(40):
                    cell_idx = cell_y*40+cell_x
                    col_byte = colour[cell_idx]
                    fg = C64_PAL[(col_byte>>4)&0xF]
                    bg = C64_PAL[col_byte&0xF]
                    for row in range(8):
                        byte = bitmap[cell_idx*8+row]
                        for bit in range(8):
                            on = (byte>>(7-bit))&1
                            c = fg if on else bg
                            x = cell_x*8+bit; y = cell_y*8+row
                            i = (y*w+x)*4
                            rgba[i:i+4] = [c[0],c[1],c[2],255]
            self._load_rgba(rgba, w, h, os.path.basename(path))
        except Exception as e:
            QMessageBox.warning(self, "Art Studio Import Error", str(e))


    def _load_rgba(self, rgba: bytearray, w: int, h: int, name: str = ""): #vers 2
        """Load raw RGBA data into canvas, update palette, fit zoom."""
        if not self.dp5_canvas: return
        self._canvas_width = w; self._canvas_height = h
        self.dp5_canvas.tex_w = w; self.dp5_canvas.tex_h = h
        self.dp5_canvas.rgba = rgba
        self.dp5_canvas.update()
        self._fit_canvas_to_viewport()
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (w,h), bytes(rgba)).convert('RGB')
            p = img.quantize(colors=256)
            pf = p.getpalette()
            self.pal_bar.set_palette_raw([(pf[i*3],pf[i*3+1],pf[i*3+2]) for i in range(256)])
        except Exception:
            pass
        self._set_status(f"Loaded: {name}  {w}×{h}" if name else f"Canvas: {w}×{h}")
        if name:
            self._bitmap_list.append({'name':name,'rgba':rgba,'w':w,'h':h})
            self._bitmap_lw.addItem(name)
            self._bitmap_lw.setCurrentRow(len(self._bitmap_list)-1)
        self._set_status(f"Loaded: {name}  {w}×{h}")


    #    Executable exports                                                     

    def _export_tap(self): #vers 1
        """Export ZX Spectrum TAP — screen$ block (6912 bytes) wrapped in TAP format."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export ZX Spectrum TAP", "screen.tap", "TAP (*.tap)")
        if not path: return
        try:
            # First generate SCR data
            from PIL import Image
            img = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                                  bytes(self.dp5_canvas.rgba)).convert('RGB')
            img = img.resize((256,192), Image.NEAREST)
            zx_pal = [0,0,0,215,0,0,0,215,0,215,215,0,0,0,215,215,0,215,0,215,215,215,215,215]
            pi = Image.new('P',(1,1)); pi.putpalette(zx_pal+[0]*744)
            q = img.quantize(palette=pi, dither=0)
            pixels = list(q.getdata())
            bitmap = bytearray(6144); attrs = bytearray(768)
            for third in range(3):
                for row in range(8):
                    for char_y in range(8):
                        y = third*64+char_y*8+row
                        for char_x in range(32):
                            byte = 0
                            for bit in range(8):
                                if pixels[y*256+char_x*8+bit]&1:
                                    byte |= (0x80>>bit)
                            bitmap[third*2048+row*256+char_y*32+char_x] = byte
            for ay in range(24):
                for ax in range(32):
                    attrs[ay*32+ax] = pixels[(ay*8)*256+ax*8]&7
            scr = bitmap+attrs
            # Wrap in TAP: header block + data block
            def tap_block(flag, data):
                payload = bytes([flag])+bytes(data)
                chk = 0
                for b in payload: chk ^= b
                block = payload + bytes([chk])
                return len(block).to_bytes(2,'little') + block
            # Header: type 3 (CODE), filename "SCREEN$   ", length 6912, param1 16384, param2 32768
            hdr  = bytes([3])+b'SCREEN$   '+bytes([0,27,0,0,64,0,128])
            tap  = tap_block(0, hdr) + tap_block(0xFF, scr)
            open(path,'wb').write(tap)
            self._set_status(f"Exported TAP: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "TAP Export Error", str(e))


    def _export_c64prg(self): #vers 1
        """Export C64 hires PRG — Art Studio bitmap wrapped with BASIC loader."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export C64 PRG (hires)", "image.prg", "PRG (*.prg)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                                  bytes(self.dp5_canvas.rgba)).convert('RGB')
            img = img.resize((320,200),Image.NEAREST)
            q = img.quantize(colors=16,dither=0)
            pixels = list(q.getdata())
            bitmap = bytearray(8000); colour = bytearray(1000)
            for cy in range(25):
                for cx in range(40):
                    ci = cy*40+cx
                    cell = [pixels[(cy*8+r)*320+cx*8+c] for r in range(8) for c in range(8)]
                    counts = {}
                    for p in cell: counts[p]=counts.get(p,0)+1
                    sc = sorted(counts,key=lambda k:-counts[k])
                    fg = sc[0] if sc else 0; bg = sc[1] if len(sc)>1 else 0
                    colour[ci] = (fg<<4)|bg
                    for row in range(8):
                        byte = 0
                        for bit in range(8):
                            if pixels[(cy*8+row)*320+cx*8+bit]==fg: byte|=(0x80>>bit)
                        bitmap[ci*8+row] = byte
            # BASIC loader stub at $0801
            basic = bytearray(b'\x01\x08\x0b\x08\x00\x00\x9e\x32\x30\x36\x31\x00\x00\x00')
            # Bitmap at $2000, colour at $6800
            prg  = b'\x01\x08' + basic
            prg += b'\x00\x20' + bitmap  # bitmap load addr $2000
            prg += b'\x00\x68' + colour  # colour load addr $6800
            open(path,'wb').write(prg)
            self._set_status(f"Exported C64 PRG: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "C64 PRG Export Error", str(e))


    def _export_c64mprg(self): #vers 1
        """Export C64 multicolour PRG — Koala bitmap wrapped with BASIC loader."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export C64 PRG (multicolour)", "image_m.prg", "PRG (*.prg)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                                  bytes(self.dp5_canvas.rgba)).convert('RGB')
            img = img.resize((160,200),Image.NEAREST)
            q = img.quantize(colors=16,dither=0)
            pixels = list(q.getdata())
            bitmap=bytearray(8000); screen=bytearray(1000); colram=bytearray(1000)
            bg=0
            for cy in range(25):
                for cx in range(40):
                    ci=cy*40+cx
                    cell_c={}
                    for row in range(8):
                        for col in range(4):
                            p=pixels[(cy*8+row)*160+cx*4+col] if cx*4+col<160 else 0
                            cell_c[p]=cell_c.get(p,0)+1
                    sc=sorted(cell_c,key=lambda k:-cell_c[k])
                    c1=sc[0] if sc else 0; c2=sc[1] if len(sc)>1 else 0; c3=sc[2] if len(sc)>2 else 0
                    screen[ci]=(c1<<4)|c2; colram[ci]=c3&0xF
                    cm={bg:0,c1:1,c2:2,c3:3}
                    for row in range(8):
                        byte=0
                        for col in range(4):
                            p=pixels[(cy*8+row)*160+cx*4+col] if cx*4+col<160 else 0
                            byte=(byte<<2)|cm.get(p,0)
                        bitmap[ci*8+row]=byte
            basic=bytearray(b'\x01\x08\x0b\x08\x00\x00\x9e\x32\x30\x36\x31\x00\x00\x00')
            prg=b'\x01\x08'+basic+b'\x00\x60'+bitmap+screen+colram+bytes([bg])
            open(path,'wb').write(prg)
            self._set_status(f"Exported C64 Multi PRG: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "C64 Multi PRG Error", str(e))


    def _export_msxcom(self): #vers 1
        """Export MSX COM — SC2 data as a relocatable COM executable."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export MSX COM", "image.com", "COM (*.com)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                                  bytes(self.dp5_canvas.rgba)).convert('RGB')
            img = img.resize((256,192),Image.NEAREST)
            q = img.quantize(colors=16,dither=0)
            pixels = list(q.getdata())
            pal_flat=q.getpalette()
            pattern=bytearray(6144); colour=bytearray(6144); name=bytearray(768)
            for by in range(24):
                for bx in range(32):
                    name[by*32+bx]=by*32+bx
                    for row in range(8):
                        y=by*8+row; idx=(by*32+bx)*8+row
                        byte=0; fg=pixels[y*256+bx*8]&0xF; bg=0
                        for bit in range(8):
                            if pixels[y*256+bx*8+bit]&0xF==fg: byte|=(0x80>>bit)
                        pattern[idx]=byte; colour[idx]=(fg<<4)|bg
            # Minimal Z80 stub: SCREEN 2, BLOAD, RET
            stub = bytearray(b'\xCD\x01\x00\xC9')  # placeholder CALL/RET
            com  = stub + pattern + colour + name
            open(path,'wb').write(com)
            self._set_status(f"Exported MSX COM: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "MSX COM Error", str(e))


    def _export_plus4prg(self): #vers 1
        """Export Plus/4 PRG — hires bitmap with BASIC loader."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Plus/4 PRG", "image.prg", "PRG (*.prg)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                                  bytes(self.dp5_canvas.rgba)).convert('RGB')
            img = img.resize((320,200),Image.NEAREST)
            q = img.quantize(colors=16,dither=0)
            pixels = list(q.getdata())
            bitmap=bytearray(8000); colour=bytearray(1000)
            for cy in range(25):
                for cx in range(40):
                    ci=cy*40+cx
                    cell=[pixels[(cy*8+r)*320+cx*8+c] for r in range(8) for c in range(8)]
                    counts={}
                    for p in cell: counts[p]=counts.get(p,0)+1
                    sc=sorted(counts,key=lambda k:-counts[k])
                    fg=sc[0] if sc else 0; bg=sc[1] if len(sc)>1 else 0
                    colour[ci]=(fg<<4)|bg
                    for row in range(8):
                        byte=0
                        for bit in range(8):
                            if pixels[(cy*8+row)*320+cx*8+bit]==fg: byte|=(0x80>>bit)
                        bitmap[ci*8+row]=byte
            # Plus/4 BASIC: SYS 7169
            basic=b'\x01\x10\x0c\x10\x00\x00\x9e\x37\x31\x36\x39\x00\x00\x00'
            prg=b'\x01\x10'+basic+b'\x00\x1c'+bitmap+colour
            open(path,'wb').write(prg)
            self._set_status(f"Exported Plus/4 PRG: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "Plus/4 PRG Error", str(e))


    def _export_vicprg(self): #vers 1
        """Export VIC-20 PRG — raw bitmap with BASIC loader."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export VIC-20 PRG", "image.prg", "PRG (*.prg)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                                  bytes(self.dp5_canvas.rgba)).convert('RGB')
            img = img.resize((176,184),Image.NEAREST)
            q = img.quantize(colors=16,dither=0)
            pixels = list(q.getdata())
            w,h = 176,184
            bitmap = bytearray(w*h//8 * 8)
            for y in range(h):
                for xb in range(w//8):
                    byte=0
                    for bit in range(8):
                        if pixels[y*w+xb*8+bit]&1: byte|=(0x80>>bit)
                    bitmap[y*(w//8)+xb]=byte
            basic=b'\x01\x10\x0c\x10\x00\x00\x9e\x34\x39\x31\x35\x00\x00\x00'
            prg=b'\x01\x10'+basic+b'\x00\x10'+bitmap
            open(path,'wb').write(prg)
            self._set_status(f"Exported VIC-20 PRG: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "VIC-20 PRG Error", str(e))


    #    ZX Spectrum Next formats                                               

    def _rgb_to_9bit(self, r: int, g: int, b: int): #vers 1
        """Convert 8-bit RGB to ZX Next 9-bit palette bytes (RRRGGGBB + B LSB)."""
        r3 = r // 36; g3 = g // 36; b3 = b // 36
        hi = (r3 << 5) | (g3 << 2) | (b3 >> 1)
        lo = b3 & 1
        return hi, lo


    def _9bit_to_rgb(self, hi: int, lo: int): #vers 2
        """Convert ZX Next 9-bit palette bytes back to 8-bit RGB."""
        r3 = (hi >> 5) & 7; g3 = (hi >> 2) & 7; b3 = ((hi & 3) << 1) | (lo & 1)
        # Scale 3-bit to 8-bit: val*36 + val//2
        r = r3*36 + r3//2; g = g3*36 + g3//2; b = b3*36 + b3//2
        return min(255,r), min(255,g), min(255,b)


    def _canvas_to_256colour_indexed(self, w: int, h: int): #vers 1
        """Quantize canvas to 256 colours, return (pixels_list, palette_rgb_list)."""
        from PIL import Image
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.dp5_canvas.rgba)).convert('RGB')
        img = img.resize((w, h), Image.NEAREST)
        q = img.quantize(colors=256, dither=0)
        pixels = list(q.getdata())
        pf = q.getpalette()
        palette = [(pf[i*3], pf[i*3+1], pf[i*3+2]) for i in range(256)]
        return pixels, palette


    def _export_nxi(self): #vers 1
        """Export ZX Spectrum Next NXI (256×192 or 320×256, 9-bit palette + indexed pixels)."""
        if not self.dp5_canvas: return
        # Ask mode
        mode_dlg = QDialog(self)
        mode_dlg.setWindowTitle("NXI Export Mode")
        vl = QVBoxLayout(mode_dlg)
        vl.addWidget(QLabel("Select resolution:"))
        combo = QComboBox()
        combo.addItems(["256×192 (ZX Spectrum Next 256)", "320×256 (ZX Spectrum Next 320)"])
        vl.addWidget(combo)
        btns = QHBoxLayout(); ok=QPushButton("OK"); can=QPushButton("Cancel")
        ok.clicked.connect(mode_dlg.accept); can.clicked.connect(mode_dlg.reject)
        btns.addStretch(); btns.addWidget(ok); btns.addWidget(can)
        vl.addLayout(btns)
        if mode_dlg.exec() != QDialog.DialogCode.Accepted: return
        is_320 = combo.currentIndex() == 1
        w, h = (320, 256) if is_320 else (256, 192)

        path, _ = QFileDialog.getSaveFileName(
            self, "Export ZX Next NXI", "image.nxi", "NXI (*.nxi)")
        if not path: return
        try:
            pixels, palette = self._canvas_to_256colour_indexed(w, h)
            out = bytearray()
            # 9-bit palette: 256 × 2 bytes = 512 bytes
            for r, g, b in palette:
                hi, lo = self._rgb_to_9bit(r, g, b)
                out += bytes([hi, lo])
            # Pixel data
            if is_320:
                # 320-mode: column-major order
                grid = [pixels[y*w+x] for x in range(w) for y in range(h)]
                out += bytes(grid)
            else:
                out += bytes(pixels)
            open(path, 'wb').write(out)
            self._set_status(f"Exported NXI: {os.path.basename(path)}  {w}×{h}")
        except Exception as e:
            QMessageBox.warning(self, "NXI Export Error", str(e))


    def _import_nxi(self): #vers 1
        """Import ZX Spectrum Next NXI file."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import ZX Next NXI", "", "NXI (*.nxi);;All Files (*)")
        if not path: return
        try:
            data = open(path, 'rb').read()
            size = len(data)
            # Detect mode by file size
            # 256×192 + palette: 512+49152=49664  without palette: 49152
            # 320×256 + palette: 512+81920=82432  without palette: 81920
            has_pal = size in (49664, 82432)
            is_320  = size in (81920, 82432)
            w, h = (320, 256) if is_320 else (256, 192)
            offset = 0
            palette = None
            if has_pal:
                palette = []
                for i in range(256):
                    hi = data[i*2]; lo = data[i*2+1]
                    palette.append(self._9bit_to_rgb(hi, lo))
                offset = 512
            pixel_bytes = data[offset:]
            rgba = bytearray(w*h*4)
            if palette is None:
                # No palette — use greyscale index
                palette = [(i, i, i) for i in range(256)]
            if is_320:
                # Column-major → row-major
                for xi in range(w):
                    for yi in range(h):
                        p = pixel_bytes[xi*h+yi]
                        c = palette[p % len(palette)]
                        i = (yi*w+xi)*4
                        rgba[i:i+4] = [c[0],c[1],c[2],255]
            else:
                for idx, p in enumerate(pixel_bytes[:w*h]):
                    c = palette[p % len(palette)]
                    i = idx*4
                    rgba[i:i+4] = [c[0],c[1],c[2],255]
            self._load_rgba(rgba, w, h, os.path.basename(path))
            # Load palette into user palette grid
            if palette:
                self._user_pal_grid.set_palette_raw(palette)
        except Exception as e:
            QMessageBox.warning(self, "NXI Import Error", str(e))


    def _export_pal(self): #vers 1
        """Export ZX Spectrum Next 9-bit PAL palette (512 bytes)."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export ZX Next PAL", "palette.pal", "PAL (*.pal)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                                  bytes(self.dp5_canvas.rgba)).convert('RGB')
            q = img.quantize(colors=256, dither=0)
            pf = q.getpalette()
            out = bytearray()
            for i in range(256):
                r, g, b = pf[i*3], pf[i*3+1], pf[i*3+2]
                hi, lo = self._rgb_to_9bit(r, g, b)
                out += bytes([hi, lo])
            open(path, 'wb').write(out)
            self._set_status(f"Exported PAL: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "PAL Export Error", str(e))


    def _import_pal(self): #vers 1
        """Import ZX Spectrum Next 9-bit PAL palette into user palette grid."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import ZX Next PAL", "", "PAL (*.pal);;All Files (*)")
        if not path: return
        try:
            data = open(path, 'rb').read()
            if len(data) != 512:
                raise ValueError(f"PAL must be 512 bytes, got {len(data)}")
            palette = []
            for i in range(256):
                hi = data[i*2]; lo = data[i*2+1]
                palette.append(self._9bit_to_rgb(hi, lo))
            self._user_pal_grid.set_palette_raw(palette)
            self._retro_btn.setText("ZX Next PAL")
            self._set_status(f"Loaded PAL: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "PAL Import Error", str(e))


    def _export_nex(self): #vers 1
        """Export ZX Spectrum Next NEX executable (loading screen, 256×192)."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export ZX Next NEX", "image.nex", "NEX (*.nex)")
        if not path: return
        try:
            pixels, palette = self._canvas_to_256colour_indexed(256, 192)
            # 512-byte NEX header
            hdr = bytearray(512)
            hdr[0:4]   = b'Next'
            hdr[4:8]   = b'V1.1'
            hdr[8]     = 0        # RAM required
            hdr[9]     = 4        # number of banks
            hdr[10]    = 0        # loading screen blocks = Layer2 w/palette
            hdr[11]    = 0        # border colour
            hdr[12:14] = b'\x80\x80'  # SP=$8000
            hdr[14:16] = b'\x00\x80'  # PC=$8000
            hdr[16:18] = (0).to_bytes(2, 'little')  # extra files
            # Bank flags: banks 0,9,10,11 included (bitmap at 0, palette+code at 9-11)
            bank_flags = [0]*112
            bank_flags[0]=1; bank_flags[9]=1; bank_flags[10]=1; bank_flags[11]=1
            hdr[18:18+112] = bytearray(bank_flags)
            hdr[130]   = 0   # loading bar
            hdr[131]   = 7   # loading bar colour (white)
            # 9-bit palette (512 bytes) — goes in bank 9
            pal_data = bytearray()
            for r, g, b in palette:
                hi, lo = self._rgb_to_9bit(r, g, b)
                pal_data += bytes([hi, lo])
            # Pixel data (49152 bytes = 3 × 16K banks)
            pix_data = bytearray(pixels[:49152])
            # Simple Z80 code stub in bank 9 after palette: waits for keypress, loops
            z80_stub = bytearray(b'\xFB\x76\xC3\x00\x80')  # EI; HALT; JP $8000
            bank9 = pal_data + z80_stub + bytearray(16384 - len(pal_data) - len(z80_stub))
            bank10 = pix_data[:16384]
            bank11 = pix_data[16384:32768] + bytearray(16384-(len(pix_data)-16384))
            bank0  = pix_data[32768:49152] + bytearray(16384-(len(pix_data)-32768))
            out = bytes(hdr) + bytes(bank0) + bytes(bank9) + bytes(bank10) + bytes(bank11)
            open(path, 'wb').write(out)
            self._set_status(f"Exported NEX: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "NEX Export Error", str(e))


    #    Icon export / import                                                   

    def _get_canvas_pil(self): #vers 1
        """Return current canvas as PIL RGBA Image."""
        from PIL import Image
        return Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                               bytes(self.dp5_canvas.rgba))

    def _export_texture_png(self): #vers 1
        """Export texture as PNG at current bit depth."""
        if not self.dp5_canvas: return
        depth_names = {0:'32bit', 1:'24bit', 2:'16bit', 3:'8bit'}
        d = depth_names.get(self._canvas_bit_depth, '32bit')
        w, h = self._canvas_width, self._canvas_height
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Texture PNG",
            f"texture_{w}x{h}_{d}.png", "PNG (*.png)")
        if not path: return
        try:
            from PIL import Image
            img = self._get_canvas_pil()
            depth = self._canvas_bit_depth
            if depth == 1:
                img = img.convert('RGB').convert('RGBA')
            elif depth == 2:
                rgb = img.convert('RGB'); px = rgb.tobytes()
                buf = bytearray(len(px))
                for i in range(0, len(px), 3):
                    buf[i]  =(px[i]   >>3)<<3
                    buf[i+1]=(px[i+1] >>2)<<2
                    buf[i+2]=(px[i+2] >>3)<<3
                img = Image.frombytes('RGB',(w,h),bytes(buf)).convert('RGBA')
            elif depth == 3:
                img = img.convert('RGB').quantize(colors=256).convert('RGB').convert('RGBA')
            img.save(path, 'PNG')
            self._set_status(f"Exported texture: {os.path.basename(path)}  {w}×{h} {d}")
        except Exception as e:
            QMessageBox.warning(self, "Texture Export Error", str(e))


    def _export_texture_bmp(self): #vers 1
        """Export texture as BMP at current bit depth."""
        if not self.dp5_canvas: return
        w, h = self._canvas_width, self._canvas_height
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Texture BMP", f"texture_{w}x{h}.bmp", "BMP (*.bmp)")
        if not path: return
        try:
            img = self._get_canvas_pil().convert('RGB')
            if self._canvas_bit_depth == 3:
                img = img.quantize(colors=256).convert('RGB')
            img.save(path, 'BMP')
            self._set_status(f"Exported BMP: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "BMP Export Error", str(e))


    #    Extended format imports / exports                                      

    def _import_iff(self): #vers 2
        """Import Amiga IFF ILBM — supports 8-bit indexed, 24-bit true colour,
        HAM6/HAM8, with or without palette, PackBits or uncompressed BODY."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import IFF ILBM", "",
            "IFF (*.iff *.lbm *.ilbm);;All Files (*)")
        if not path: return
        try:
            data = open(path, 'rb').read()
            rgba = self._decode_iff_ilbm(data)
            if rgba is None:
                # Fall back to PIL for formats it understands
                from PIL import Image
                img = Image.open(path).convert('RGBA')
                rgba = bytearray(img.tobytes())
                w, h = img.size
            else:
                import struct
                bmhd = self._iff_find_chunk(data, b'BMHD')
                w, h = struct.unpack_from('>HH', bmhd)
            self._load_rgba(rgba, w, h, os.path.basename(path))
        except Exception as e:
            QMessageBox.warning(self, "IFF Import Error", str(e))


    def _iff_find_chunk(self, data: bytes, tag: bytes) -> bytes: #vers 1
        """Find and return data of first matching IFF chunk."""
        import struct
        offset = 12  # skip FORM+size+subtype
        while offset < len(data) - 8:
            ctag = data[offset:offset+4]
            csize = struct.unpack_from('>I', data, offset+4)[0]
            if ctag == tag:
                return data[offset+8:offset+8+csize]
            offset += 8 + csize + (csize % 2)
        return b''


    def _iff_unpack_body(self, body: bytes, row_bytes: int, n_rows: int,
                         compression: int) -> bytes: #vers 1
        """Unpack IFF BODY — compression 0=raw, 1=PackBits/ByteRun1."""
        if compression == 0:
            return body
        # PackBits (ByteRun1) decompression
        out = bytearray()
        i = 0
        total = row_bytes * n_rows
        while i < len(body) and len(out) < total:
            n = body[i]; i += 1
            if n <= 127:
                # Copy n+1 literal bytes
                out += body[i:i+n+1]; i += n+1
            elif n >= 129:
                # Repeat next byte (257-n) times
                out += bytes([body[i]]) * (257-n); i += 1
            # n==128 is NOP
        return bytes(out)


    def _decode_iff_ilbm(self, data: bytes): #vers 1
        """Decode IFF ILBM to RGBA bytearray. Returns None if unsupported."""
        import struct
        if data[0:4] != b'FORM' or data[8:12] != b'ILBM':
            return None

        bmhd_raw = self._iff_find_chunk(data, b'BMHD')
        if len(bmhd_raw) < 20: return None
        w, h       = struct.unpack_from('>HH', bmhd_raw, 0)
        planes     = bmhd_raw[8]
        masking    = bmhd_raw[9]
        compression= bmhd_raw[10]

        cmap_raw = self._iff_find_chunk(data, b'CMAP')
        camg_raw = self._iff_find_chunk(data, b'CAMG')
        body_raw = self._iff_find_chunk(data, b'BODY')

        camg = struct.unpack_from('>I', camg_raw)[0] if len(camg_raw)>=4 else 0
        is_ham  = bool(camg & 0x0800)
        is_ehb  = bool(camg & 0x0080)
        is_ham8 = is_ham and planes == 8

        # Build palette from CMAP (3 bytes per entry)
        palette = []
        if cmap_raw:
            for i in range(0, len(cmap_raw)-2, 3):
                palette.append((cmap_raw[i], cmap_raw[i+1], cmap_raw[i+2]))
        # EHB: duplicate palette at half brightness
        if is_ehb and len(palette) == 32:
            palette += [(r>>1, g>>1, b>>1) for r,g,b in palette]

        # Row bytes — always word-aligned per bitplane
        row_bytes = ((w + 15) // 16) * 2
        # Total body rows — includes mask plane if masking==1
        n_planes_body = planes + (1 if masking == 1 else 0)
        body = self._iff_unpack_body(body_raw, row_bytes * n_planes_body, h, compression)

        rgba = bytearray(w * h * 4)

        if planes == 24:
            # 24-bit true colour: 8 planes R, 8 planes G, 8 planes B interleaved per row
            for y in range(h):
                row_off = y * row_bytes * n_planes_body
                # Extract each channel from bitplanes
                channels = []
                for p in range(24):
                    plane_row = body[row_off + p*row_bytes : row_off + p*row_bytes + row_bytes]
                    channels.append(plane_row)
                for x in range(w):
                    byte_idx = x // 8
                    bit_mask = 0x80 >> (x % 8)
                    # Planes 0-7 = Red, 8-15 = Green, 16-23 = Blue
                    r = sum(((1 if (channels[p][byte_idx] & bit_mask) else 0) << p)
                             for p in range(8))
                    g = sum(((1 if (channels[8+p][byte_idx] & bit_mask) else 0) << p)
                             for p in range(8))
                    b = sum(((1 if (channels[16+p][byte_idx] & bit_mask) else 0) << p)
                             for p in range(8))
                    i = (y*w+x)*4
                    rgba[i:i+4] = [r, g, b, 255]

        elif is_ham:
            # HAM6 or HAM8
            bits = 8 if is_ham8 else 6
            base_planes = bits - 2
            base_n = 1 << base_planes  # 16 for HAM6, 64 for HAM8
            if not palette:
                palette = [(i*17, i*17, i*17) for i in range(16)]
            for y in range(h):
                row_off = y * row_bytes * n_planes_body
                plane_rows = [body[row_off + p*row_bytes : row_off + p*row_bytes + row_bytes]
                              for p in range(planes)]
                pr, pg, pb = 0, 0, 0
                for x in range(w):
                    byte_idx = x // 8
                    bit_mask = 0x80 >> (x % 8)
                    px = sum(((1 if (plane_rows[p][byte_idx] & bit_mask) else 0) << p)
                              for p in range(planes))
                    ctrl = px >> base_planes
                    val  = px & (base_n - 1)
                    if ctrl == 0:
                        if val < len(palette): pr,pg,pb = palette[val]
                    elif ctrl == 1:  # modify blue
                        pb = val << (8-base_planes) if is_ham8 else val*17
                    elif ctrl == 2:  # modify red
                        pr = val << (8-base_planes) if is_ham8 else val*17
                    elif ctrl == 3:  # modify green
                        pg = val << (8-base_planes) if is_ham8 else val*17
                    i = (y*w+x)*4
                    rgba[i:i+4] = [pr, pg, pb, 255]

        else:
            # Indexed (1-8 bit planes)
            if not palette:
                n = 1 << planes
                palette = [(i*255//(n-1),)*3 for i in range(n)]
            for y in range(h):
                row_off = y * row_bytes * n_planes_body
                plane_rows = [body[row_off + p*row_bytes : row_off + p*row_bytes + row_bytes]
                              for p in range(planes)]
                for x in range(w):
                    byte_idx = x // 8
                    bit_mask = 0x80 >> (x % 8)
                    px = sum(((1 if (plane_rows[p][byte_idx] & bit_mask) else 0) << p)
                              for p in range(planes))
                    c = palette[px] if px < len(palette) else (0,0,0)
                    i = (y*w+x)*4
                    rgba[i:i+4] = [c[0], c[1], c[2], 255]

        return rgba


    def _import_tiff(self): #vers 1
        """Import TIFF (single or multi-page — loads first page)."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import TIFF", "",
            "TIFF (*.tif *.tiff);;All Files (*)")
        if not path: return
        try:
            from PIL import Image
            img = Image.open(path)
            img.seek(0)
            img = img.convert('RGBA')
            self._load_rgba(bytearray(img.tobytes()), img.width, img.height,
                           os.path.basename(path))
        except Exception as e:
            QMessageBox.warning(self, "TIFF Import Error", str(e))


    def _import_gif(self): #vers 1
        """Import GIF — animated GIF loads all frames into animation timeline."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import GIF", "",
            "GIF (*.gif);;All Files (*)")
        if not path: return
        try:
            from PIL import Image
            gif = Image.open(path)
            frames = []
            try:
                while True:
                    f = gif.copy().convert('RGBA')
                    frames.append(f)
                    gif.seek(gif.tell()+1)
            except EOFError:
                pass
            if not frames: return
            # Load first frame to canvas
            img = frames[0]
            self._canvas_width  = img.width
            self._canvas_height = img.height
            self.dp5_canvas.tex_w = img.width
            self.dp5_canvas.tex_h = img.height
            self.dp5_canvas.rgba  = bytearray(img.tobytes())
            self.dp5_canvas.update()
            self._fit_canvas_to_viewport()
            # Load all frames into animation if >1
            if len(frames) > 1 and hasattr(self, '_frames'):
                self._frames = [bytearray(f.resize(
                    (img.width, img.height), Image.NEAREST).convert('RGBA').tobytes())
                    for f in frames]
                self._frame_delays = [gif.info.get('duration', 100)] * len(frames)
                self._current_frame = 0
                if hasattr(self, '_anim_strip') and self._anim_strip.isVisible():
                    self._anim_refresh_thumbs()
                self._set_status(
                    f"Loaded GIF: {os.path.basename(path)}  {len(frames)} frames")
            else:
                self._set_status(f"Loaded GIF: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "GIF Import Error", str(e))


    def _import_tga(self): #vers 1
        """Import TGA (Targa) — common in game textures."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import TGA", "",
            "TGA (*.tga *.targa);;All Files (*)")
        if not path: return
        self._import_bitmap_path(path)


    def _import_pcx(self): #vers 1
        """Import PCX — classic DOS bitmap format."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import PCX", "",
            "PCX (*.pcx);;All Files (*)")
        if not path: return
        self._import_bitmap_path(path)


    def _import_dds(self): #vers 1
        """Import DDS (DirectDraw Surface) — GTA and game engine textures."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import DDS", "",
            "DDS (*.dds);;All Files (*)")
        if not path: return
        try:
            from PIL import Image
            # Try PIL first (needs pillow-dds or wand)
            try:
                img = Image.open(path).convert('RGBA')
            except Exception:
                # Fallback: try imageio
                import imageio
                arr = imageio.imread(path)
                from PIL import Image as PILImage
                img = PILImage.fromarray(arr).convert('RGBA')
            self._load_rgba(bytearray(img.tobytes()), img.width, img.height,
                           os.path.basename(path))
        except Exception as e:
            QMessageBox.warning(self, "DDS Import Error",
                f"{e}\n\nFor DDS support install: pip install pillow-dds")


    def _import_psd(self): #vers 1
        """Import PSD (Photoshop) — reads merged composite layer."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import PSD", "",
            "PSD (*.psd *.psb);;All Files (*)")
        if not path: return
        try:
            from PIL import Image
            img = Image.open(path).convert('RGBA')
            self._load_rgba(bytearray(img.tobytes()), img.width, img.height,
                           os.path.basename(path))
        except Exception as e:
            QMessageBox.warning(self, "PSD Import Error",
                f"{e}\n\nFor PSD support install: pip install psd-tools")


    def _import_amiga_info(self, palette_mode: str = 'aga_wb'): #vers 5
        """Import Amiga .info icon with named palette.
        palette_mode: 'aga_wb' | 'aga' | 'magicwb' | 'ocs' | 'user'
        """
        mode_labels = {
            'wb39':   'WB 3.9 AGA (256col)',
            'wb39xl': 'WB 3.9 XL AGA (256col)',
            'aga':    'AGA standard (16col)',
            'magicwb':'MagicWB (8col)',
            'ocs':    'OCS/ECS WB3 (4col)',
            'user':   f'User palette ({self.current_retro_palette})',
        }
        path, _ = QFileDialog.getOpenFileName(
            self, f"Import Amiga .info — {mode_labels.get(palette_mode,'')}",
            "", "Amiga Icon (*.info);;All Files (*)")
        if not path: return
        try:
            data = open(path, 'rb').read()
            rgba, w, h, fmt = self._decode_amiga_info(data, palette_mode)
            if rgba is None:
                QMessageBox.warning(self, "Amiga Icon Import",
                    f"Format not supported: {fmt}\n\n"
                    "Supported: Classic bitplane, NewIcon (IM1=)\n"
                    "Unsupported: OS3.5 ICONFACE, GlowIcon ARGB")
                return
            self._load_rgba(bytearray(rgba), w, h,
                           f"{os.path.basename(path)} [{fmt}]")
        except Exception as e:
            QMessageBox.warning(self, "Amiga Info Import Error", str(e))


    def _decode_amiga_info(self, data: bytes, palette_mode: str = 'aga_wb'): #vers 4
        """Decode Amiga .info to (rgba, w, h, format_name) or (None,0,0,reason).
        palette_mode: 'wb39' | 'wb39xl' | 'aga' | 'magicwb' | 'ocs' | 'user'
        Palettes extracted from real WB3.9 and WB3.9XL palette.prefs files."""
        import struct
        if len(data) < 78 or data[0:2] != bytes([0xE3, 0x10]):
            return None, 0, 0, "Not a valid .info file"
        w = struct.unpack_from('>H', data, 12)[0]
        h = struct.unpack_from('>H', data, 14)[0]
        if w == 0 or h == 0 or w > 1024 or h > 1024:
            return None, 0, 0, f"Invalid dimensions {w}\xd7{h}"
        drawer_ptr = struct.unpack_from('>I', data, 66)[0]

        #    NewIcon                                                    
        if b'IM1=' in data:
            try:
                rgba = self._decode_newicon_im1(data, w, h)
                if rgba: return rgba, w, h, 'NewIcon-IM1'
            except Exception:
                pass
        #    OS3.5 ICONFACE                                             
        if b':ICONFACE' in data:
            return None, 0, 0, "OS3.5-ICONFACE (proprietary format)"

        #    Real WB3.9 256-colour palette (from actual WB3_9.pal prefs)   
        WB39 = [
            (144,148,149),(43,0,0),(255,255,255),(0,98,255),(120,120,120),(175,175,175),(170,144,124),(255,169,151),  # 0
            (149,149,149),(238,85,0),(153,255,17),(238,187,0),(85,85,255),(153,34,255),(0,255,136),(204,204,204),  # 8
            (0,0,0),(219,117,65),(0,0,0),(255,255,255),(68,68,68),(85,85,85),(102,102,102),(119,119,119),  # 16
            (136,136,136),(153,153,153),(170,170,170),(187,187,187),(204,204,204),(221,221,221),(238,238,238),(255,255,255),  # 24
            (0,0,255),(39,79,68),(120,106,253),(255,255,171),(122,110,68),(177,149,93),(249,255,255),(222,253,255),  # 32
            (194,250,255),(166,247,255),(138,244,255),(111,242,255),(83,239,255),(55,236,255),(27,233,255),(0,230,255),  # 40
            (57,56,56),(49,49,49),(41,42,42),(34,34,35),(27,27,27),(14,14,12),(0,0,255),(0,0,152),  # 48
            (0,0,83),(0,10,0),(0,220,255),(0,186,255),(255,68,255),(255,57,213),(255,45,170),(255,34,128),  # 56
            (255,23,85),(255,11,43),(255,0,0),(255,13,0),(255,26,0),(255,38,0),(255,51,0),(255,64,0),  # 64
            (255,77,0),(255,89,0),(255,102,0),(255,115,0),(255,128,0),(255,140,0),(255,153,0),(255,166,0),  # 72
            (255,179,0),(255,191,0),(255,204,0),(255,217,0),(255,230,0),(255,242,0),(255,255,0),(241,255,0),  # 80
            (227,255,0),(213,255,0),(199,255,0),(184,255,0),(170,255,0),(156,255,0),(142,255,0),(128,255,0),  # 88
            (113,255,0),(99,255,0),(85,255,0),(71,255,0),(56,255,0),(42,255,0),(28,255,0),(14,255,0),  # 96
            (0,255,0),(0,247,0),(0,239,0),(0,231,0),(0,223,0),(0,215,0),(0,207,0),(0,199,0),  # 104
            (0,191,0),(0,183,0),(0,175,0),(0,167,0),(0,159,0),(0,151,0),(0,143,0),(0,135,0),  # 112
            (0,127,0),(0,119,0),(0,111,0),(0,103,0),(0,95,0),(0,87,0),(0,79,0),(0,71,0),  # 120
            (0,63,0),(0,55,0),(0,47,0),(0,39,0),(0,31,0),(0,23,0),(43,0,0),(64,0,0),  # 128
            (85,0,0),(107,0,0),(128,0,0),(149,0,0),(170,0,0),(192,0,0),(213,0,0),(234,0,0),  # 136
            (255,0,0),(238,0,25),(221,0,49),(204,0,74),(187,0,98),(170,0,123),(153,0,147),(136,0,172),  # 144
            (119,0,196),(102,0,221),(102,204,102),(34,68,34),(170,255,170),(203,192,12),(186,171,16),(168,149,20),  # 152
            (151,128,23),(134,107,27),(117,86,31),(99,65,35),(82,44,39),(0,62,255),(0,72,255),(0,79,255),  # 160
            (0,92,255),(0,102,255),(0,112,255),(0,121,255),(0,131,255),(0,141,255),(0,151,255),(0,161,255),  # 168
            (47,34,34),(255,170,170),(228,255,175),(197,255,145),(147,255,106),(125,255,73),(110,255,52),(76,255,10),  # 176
            (170,170,170),(238,238,238),(34,0,32),(54,3,40),(74,6,49),(94,9,58),(114,12,67),(134,15,75),  # 184
            (155,18,84),(175,21,93),(195,24,102),(215,27,110),(235,31,119),(255,34,128),(85,170,170),(119,238,238),  # 192
            (255,204,136),(136,102,68),(255,255,204),(204,153,102),(68,51,0),(255,255,170),(255,255,238),(136,204,136),  # 200
            (68,102,68),(204,255,204),(102,153,102),(34,51,34),(170,255,170),(238,255,238),(204,204,136),(102,102,68),  # 208
            (255,255,204),(153,150,102),(51,51,46),(255,255,170),(215,220,218),(68,204,136),(40,62,95),(38,71,152),  # 216
            (104,95,91),(144,144,144),(170,144,124),(172,157,151),(255,68,136),(136,34,68),(255,102,204),(204,51,102),  # 224
            (68,17,34),(255,85,255),(255,119,238),(136,68,136),(68,34,68),(204,102,204),(102,51,102),(34,0,32),  # 232
            (170,85,170),(238,119,238),(223,0,0),(206,0,0),(190,0,0),(174,0,0),(158,0,0),(142,0,0),  # 240
            (125,0,0),(109,0,0),(62,162,190),(144,148,149),(123,123,123),(175,175,175),(170,144,124),(255,169,151),  # 248
        ]
        #    Real WB3.9 XL 256-colour palette (from amigaos3_9xl.pal)   
        WB39_XL = [
            (144,148,149),(43,0,0),(255,255,255),(0,98,255),(123,121,123),(175,175,175),(170,144,124),(255,169,151),  # 0
            (149,149,149),(238,85,0),(153,255,17),(238,187,0),(85,85,255),(153,34,255),(0,255,136),(204,204,204),  # 8
            (0,0,0),(224,4,64),(0,0,0),(224,224,192),(68,68,68),(85,85,85),(102,102,102),(119,119,119),  # 16
            (136,136,136),(153,153,153),(170,170,170),(187,187,187),(204,204,204),(221,221,221),(238,238,238),(255,255,255),  # 24
            (0,0,255),(39,79,68),(120,106,253),(255,255,171),(122,110,68),(177,149,93),(249,255,255),(222,253,255),  # 32
            (194,250,255),(166,247,255),(138,244,255),(111,242,255),(83,239,255),(55,236,255),(27,233,255),(0,230,255),  # 40
            (57,56,56),(49,49,49),(41,42,42),(34,34,35),(27,27,27),(14,14,12),(0,0,255),(0,0,152),  # 48
            (0,0,83),(0,10,0),(0,220,255),(0,186,255),(255,68,255),(255,57,213),(255,45,170),(255,34,128),  # 56
            (255,23,85),(255,11,43),(255,0,0),(255,13,0),(255,26,0),(255,38,0),(255,51,0),(255,64,0),  # 64
            (255,77,0),(255,89,0),(255,102,0),(255,115,0),(255,128,0),(255,140,0),(255,153,0),(255,166,0),  # 72
            (255,179,0),(255,191,0),(255,204,0),(255,217,0),(255,230,0),(255,242,0),(255,255,0),(241,255,0),  # 80
            (227,255,0),(213,255,0),(199,255,0),(184,255,0),(170,255,0),(156,255,0),(142,255,0),(128,255,0),  # 88
            (113,255,0),(99,255,0),(85,255,0),(71,255,0),(56,255,0),(42,255,0),(28,255,0),(14,255,0),  # 96
            (0,255,0),(0,247,0),(0,239,0),(0,231,0),(0,223,0),(0,215,0),(0,207,0),(0,199,0),  # 104
            (0,191,0),(0,183,0),(0,175,0),(0,167,0),(0,159,0),(0,151,0),(0,143,0),(0,135,0),  # 112
            (0,127,0),(0,119,0),(0,111,0),(0,103,0),(0,95,0),(0,87,0),(0,79,0),(0,71,0),  # 120
            (0,63,0),(0,55,0),(0,47,0),(0,39,0),(0,31,0),(0,23,0),(43,0,0),(64,0,0),  # 128
            (85,0,0),(107,0,0),(128,0,0),(149,0,0),(170,0,0),(192,0,0),(213,0,0),(234,0,0),  # 136
            (255,0,0),(238,0,25),(221,0,49),(204,0,74),(187,0,98),(170,0,123),(153,0,147),(136,0,172),  # 144
            (119,0,196),(102,0,221),(102,204,102),(34,68,34),(170,255,170),(203,192,12),(186,171,16),(168,149,20),  # 152
            (151,128,23),(134,107,27),(117,86,31),(99,65,35),(82,44,39),(0,62,255),(0,72,255),(0,79,255),  # 160
            (0,92,255),(0,102,255),(0,112,255),(0,121,255),(0,131,255),(0,141,255),(0,151,255),(0,161,255),  # 168
            (47,34,34),(255,170,170),(228,255,175),(197,255,145),(147,255,106),(125,255,73),(110,255,52),(76,255,10),  # 176
            (170,170,170),(238,238,238),(34,0,32),(54,3,40),(74,6,49),(94,9,58),(114,12,67),(134,15,75),  # 184
            (155,18,84),(175,21,93),(195,24,102),(215,27,110),(235,31,119),(255,34,128),(85,170,170),(119,238,238),  # 192
            (255,204,136),(136,102,68),(255,255,204),(204,153,102),(68,51,0),(255,255,170),(255,255,238),(136,204,136),  # 200
            (68,102,68),(204,255,204),(102,153,102),(34,51,34),(170,255,170),(238,255,238),(204,204,136),(102,102,68),  # 208
            (255,255,204),(153,150,102),(51,51,46),(255,255,170),(215,220,218),(68,204,136),(40,62,95),(38,71,152),  # 216
            (104,95,91),(144,144,144),(170,144,124),(172,157,151),(255,68,136),(136,34,68),(255,102,204),(204,51,102),  # 224
            (68,17,34),(255,85,255),(255,119,238),(136,68,136),(68,34,68),(204,102,204),(102,51,102),(34,0,32),  # 232
            (170,85,170),(238,119,238),(223,0,0),(206,0,0),(190,0,0),(174,0,0),(158,0,0),(142,0,0),  # 240
            (125,0,0),(109,0,0),(62,162,190),(144,148,149),(123,123,123),(175,175,175),(170,144,124),(255,169,151),  # 248
        ]
        #    OCS/ECS WB3 4-colour                                      
        OCS = [(0,0,0),(255,255,255),(0,0,0),(102,136,187)]
        #    MagicWB 8-colour                                           
        MAGIC = [(0,0,0),(0,0,0),(255,255,255),(59,103,162),
                 (123,123,123),(149,149,149),(170,144,124),(255,169,0)]
        #    AGA WB 16-colour                                           
        AGA16 = [(144,148,149),(43,0,0),(255,255,255),(0,98,255),
                 (120,120,120),(175,175,175),(170,144,124),(255,169,151),
                 (149,149,149),(238,85,0),(153,255,17),(238,187,0),
                 (85,85,255),(153,34,255),(0,255,136),(204,204,204)]

        if palette_mode == 'ocs':
            palette = OCS
        elif palette_mode == 'magicwb':
            palette = MAGIC
        elif palette_mode == 'aga':
            palette = AGA16
        elif palette_mode == 'wb39xl':
            palette = WB39_XL
        elif palette_mode == 'user':
            user = self._get_user_palette_rgb()
            palette = [(r,g,b) for r,g,b in user] if user else WB39
        else:  # 'wb39' or 'aga_wb' — default to real WB3.9 palette
            palette = WB39

        #    Classic bitplane decode                                    
        base = 78 + (56 if drawer_ptr else 0)
        if base + 20 > len(data):
            return None, 0, 0, "File truncated at Image struct"
        img_depth = struct.unpack_from('>H', data, base + 8)[0]
        if img_depth == 0 or img_depth > 8:
            img_depth = 4
        data_off = base + 20
        row_bytes = ((w + 15) // 16) * 2
        plane_size = row_bytes * h
        depth = img_depth
        for try_d in [img_depth, 4, 2]:
            if data_off + plane_size * try_d <= len(data):
                depth = try_d; break
        else:
            return None, 0, 0, "File too small for bitplane data"

        n_pal = len(palette)
        rgba = bytearray(w * h * 4)
        for y in range(h):
            for x in range(w):
                px = 0
                for p in range(depth):
                    off = data_off + p * plane_size + y * row_bytes + x // 8
                    if off < len(data) and data[off] & (0x80 >> (x % 8)):
                        px |= (1 << p)
                c = palette[px % n_pal]
                i = (y * w + x) * 4
                rgba[i:i+4] = [c[0], c[1], c[2], 0 if px == 0 else 255]
        return bytes(rgba), w, h, f'Classic-{depth}bp-{palette_mode}'


    def _decode_newicon_im1(self, data: bytes, w: int, h: int): #vers 1
        """Decode NewIcon IM1=/IM2= encoded image from ToolTypes."""
        import re
        chunks = []
        for m in re.finditer(rb'IM\d=([^\x00]+)', data):
            chunks.append(m.group(1))
        if not chunks: return None
        raw = b''.join(chunks)
        decoded = bytes(b - 0x21 for b in raw if b >= 0x21)
        if len(decoded) < 4: return None
        n_col = decoded[0]
        if len(decoded) < 1 + n_col*3: return None
        pal = []
        for i in range(n_col):
            r,g,b = decoded[1+i*3], decoded[2+i*3], decoded[3+i*3]
            pal.append((r*4, g*4, b*4, 0 if i==0 else 255))
        px_data = decoded[1+n_col*3:]
        rgba = bytearray(w*h*4)
        for i in range(min(w*h, len(px_data))):
            c = pal[px_data[i] % len(pal)]
            rgba[i*4:i*4+4] = c
        return bytes(rgba)

    #    Batch Converters                                                       


    def _batch_convert_icons(self): #vers 5
        """Batch convert icons — inline _ToolOverlay panel."""
        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout, QLabel,
            QComboBox, QPushButton, QFileDialog, QLineEdit,
            QProgressBar, QTextEdit, QCheckBox, QSpinBox, QApplication)
        from PyQt6.QtGui import QImage, QPixmap

        ctrl = QWidget()
        vl = QVBoxLayout(ctrl); vl.setContentsMargins(0,0,0,0); vl.setSpacing(3)

        # Source folder
        hl_src = QHBoxLayout()
        src_edit = QLineEdit(); src_edit.setPlaceholderText("Source folder…")
        src_btn  = QPushButton("…"); src_btn.setFixedWidth(28)
        src_btn.clicked.connect(lambda: src_edit.setText(
            QFileDialog.getExistingDirectory(ctrl, "Source folder")))
        hl_src.addWidget(QLabel("From:")); hl_src.addWidget(src_edit, 1); hl_src.addWidget(src_btn)
        vl.addLayout(hl_src)

        # Input/Output format row
        fmt_row = QHBoxLayout()
        ifmt = QComboBox()
        ifmt.addItems(["Auto-detect","Amiga .info","Windows ICO",
                       "Apple ICNS","PNG","BMP","SVG","Any image"])
        ofmt = QComboBox()
        ofmt.addItems(["PNG","BMP","ICO (Windows)","ICNS (Apple)",
                       "Amiga .info (AGA WB)","Amiga .info (MagicWB)",
                       "Amiga .info (OCS)","TGA","SVG"])
        fmt_row.addWidget(QLabel("In:")); fmt_row.addWidget(ifmt, 1)
        fmt_row.addWidget(QLabel("Out:")); fmt_row.addWidget(ofmt, 1)
        vl.addLayout(fmt_row)

        # Dest folder
        hl_dst = QHBoxLayout()
        dst_edit = QLineEdit(); dst_edit.setPlaceholderText("Output folder…")
        dst_btn  = QPushButton("…"); dst_btn.setFixedWidth(28)
        dst_btn.clicked.connect(lambda: dst_edit.setText(
            QFileDialog.getExistingDirectory(ctrl, "Output folder")))
        hl_dst.addWidget(QLabel("To:")); hl_dst.addWidget(dst_edit, 1); hl_dst.addWidget(dst_btn)
        vl.addLayout(hl_dst)

        # Options row
        opt_row = QHBoxLayout()
        resize_chk = QCheckBox("Resize:")
        sz_w = QSpinBox(); sz_w.setRange(1,4096); sz_w.setValue(256); sz_w.setEnabled(False)
        sz_h = QSpinBox(); sz_h.setRange(1,4096); sz_h.setValue(256); sz_h.setEnabled(False)
        resize_chk.toggled.connect(lambda v: (sz_w.setEnabled(v), sz_h.setEnabled(v)))
        alpha_chk = QCheckBox("Alpha=col 0")
        alpha_chk.setChecked(True)
        pot_chk   = QCheckBox("Pow2")
        recur_chk = QCheckBox("Recurse")
        recur_chk.setChecked(True)
        opt_row.addWidget(resize_chk); opt_row.addWidget(sz_w)
        opt_row.addWidget(QLabel("×")); opt_row.addWidget(sz_h)
        opt_row.addWidget(alpha_chk); opt_row.addWidget(pot_chk)
        opt_row.addWidget(recur_chk); opt_row.addStretch()
        vl.addLayout(opt_row)

        # Progress + log
        prog = QProgressBar(); prog.setValue(0)
        log  = QTextEdit(); log.setReadOnly(True); log.setMaximumHeight(80)
        vl.addWidget(prog); vl.addWidget(log)

        parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

        def _run():
            src = src_edit.text(); dst = dst_edit.text()
            if not src or not dst:
                log.append("ERROR: set source and output folders"); return
            import os
            from PIL import Image

            def nearest_pot(x): return 1 << (x-1).bit_length()

            # Gather files
            files = []
            if recur_chk.isChecked():
                for root, _, fnames in os.walk(src):
                    for f in fnames: files.append(os.path.join(root, f))
            else:
                files = [os.path.join(src, f) for f in os.listdir(src)]

            EXT_IN = {
                "Auto-detect": [".info",".ico",".icns",".png",".bmp",".svg",
                                ".tga",".jpg",".jpeg",".gif",".webp"],
                "Amiga .info": [".info"], "Windows ICO": [".ico"],
                "Apple ICNS": [".icns"], "PNG": [".png"], "BMP": [".bmp"],
                "SVG": [".svg"], "Any image": [".png",".bmp",".tga",".ico",
                    ".jpg",".jpeg",".gif",".webp",".info"],
            }
            EXT_OUT = {
                "PNG":".png","BMP":".bmp","ICO (Windows)":".ico",
                "ICNS (Apple)":".icns","TGA":".tga","SVG":".svg",
                "Amiga .info (AGA WB)":".info","Amiga .info (MagicWB)":".info",
                "Amiga .info (OCS)":".info",
            }
            valid_exts = EXT_IN.get(ifmt.currentText(), [])
            out_ext    = EXT_OUT.get(ofmt.currentText(), ".png")
            files = [f for f in files
                     if os.path.splitext(f)[1].lower() in valid_exts]
            if not files:
                log.append("No matching files found"); return

            prog.setMaximum(len(files)); ok = err = 0
            for idx, fpath in enumerate(files):
                prog.setValue(idx); QApplication.processEvents()
                fname = os.path.basename(fpath)
                base  = os.path.splitext(fname)[0]
                out_path = os.path.join(dst, base + out_ext)
                try:
                    ext = os.path.splitext(fpath)[1].lower()
                    if ext == ".info":
                        with open(fpath, "rb") as f: data = f.read()
                        pal = ("magicwb" if "MagicWB" in ofmt.currentText()
                               else "ocs" if "OCS" in ofmt.currentText() else "wb39")
                        rgba, w, h, fmt = self._decode_amiga_info(data, pal)
                        if rgba is None: raise ValueError(fmt)
                        img = Image.frombytes("RGBA", (w, h), bytes(rgba))
                    else:
                        img = Image.open(fpath).convert("RGBA")
                    if alpha_chk.isChecked():
                        r,g,b,a = img.split()
                        pal_data = img.getpalette()
                        if pal_data:
                            c0 = tuple(pal_data[:3])
                            arr = img.load()
                            for y in range(img.height):
                                for x in range(img.width):
                                    if arr[x,y][:3] == c0:
                                        arr[x,y] = c0 + (0,)
                    if resize_chk.isChecked():
                        nw, nh = sz_w.value(), sz_h.value()
                        if pot_chk.isChecked():
                            nw, nh = nearest_pot(nw), nearest_pot(nh)
                        img = img.resize((nw, nh), Image.LANCZOS)
                    out_fmt = ofmt.currentText()
                    if "Amiga .info" in out_fmt:
                        pal = ("magicwb" if "MagicWB" in out_fmt
                               else "ocs" if "OCS" in out_fmt else "aga_wb")
                        enc = self._encode_amiga_info(
                            bytearray(img.tobytes()), img.width, img.height, pal)
                        with open(out_path, "wb") as f: f.write(enc)
                    else:
                        save_fmt = out_fmt.split()[0].upper()
                        img.save(out_path, save_fmt)
                    log.append(f"✓ {fname}"); ok += 1
                except Exception as e:
                    log.append(f"✗ {fname}: {e}"); err += 1
                log.repaint()
            prog.setValue(len(files))
            log.append(f"Done: {ok} ok, {err} errors")

        ov = self._ToolOverlay(parent_vp, self, "Batch Convert Icons",
                               ctrl, apply_fn=_run, generate_fn=None)
        # Widen overlay for batch UI
        ov.setFixedWidth(min(700, max(500, parent_vp.width() - 40)))
        ov.move(max(0, (parent_vp.width() - ov.width()) // 2),
                parent_vp.height() - ov.height())

    def _batch_convert_textures(self): #vers 2
        """Batch convert textures — inline _ToolOverlay panel."""
        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout, QLabel,
            QComboBox, QPushButton, QFileDialog, QLineEdit,
            QProgressBar, QTextEdit, QCheckBox, QSpinBox, QApplication)

        ctrl = QWidget()
        vl = QVBoxLayout(ctrl); vl.setContentsMargins(0,0,0,0); vl.setSpacing(3)

        # Source
        hl_src = QHBoxLayout()
        src_edit = QLineEdit(); src_edit.setPlaceholderText("Source folder…")
        src_btn  = QPushButton("…"); src_btn.setFixedWidth(28)
        src_btn.clicked.connect(lambda: src_edit.setText(
            QFileDialog.getExistingDirectory(ctrl, "Source folder")))
        hl_src.addWidget(QLabel("From:")); hl_src.addWidget(src_edit, 1); hl_src.addWidget(src_btn)
        vl.addLayout(hl_src)

        # Format row
        fmt_row = QHBoxLayout()
        ifmt = QComboBox()
        ifmt.addItems(["Any image","PNG","BMP","TGA","DDS","JPG","TIFF","PCX"])
        ofmt = QComboBox()
        ofmt.addItems(["PNG","BMP","TGA","DDS","JPG","TIFF","PCX"])
        fmt_row.addWidget(QLabel("In:")); fmt_row.addWidget(ifmt, 1)
        fmt_row.addWidget(QLabel("Out:")); fmt_row.addWidget(ofmt, 1)
        vl.addLayout(fmt_row)

        # Dest
        hl_dst = QHBoxLayout()
        dst_edit = QLineEdit(); dst_edit.setPlaceholderText("Output folder…")
        dst_btn  = QPushButton("…"); dst_btn.setFixedWidth(28)
        dst_btn.clicked.connect(lambda: dst_edit.setText(
            QFileDialog.getExistingDirectory(ctrl, "Output folder")))
        hl_dst.addWidget(QLabel("To:")); hl_dst.addWidget(dst_edit, 1); hl_dst.addWidget(dst_btn)
        vl.addLayout(hl_dst)

        # Options
        opt_row = QHBoxLayout()
        resize_chk = QCheckBox("Resize:")
        sz_w = QSpinBox(); sz_w.setRange(1,4096); sz_w.setValue(256); sz_w.setEnabled(False)
        sz_h = QSpinBox(); sz_h.setRange(1,4096); sz_h.setValue(256); sz_h.setEnabled(False)
        resize_chk.toggled.connect(lambda v: (sz_w.setEnabled(v), sz_h.setEnabled(v)))
        pot_chk = QCheckBox("Pow2")
        opt_row.addWidget(resize_chk); opt_row.addWidget(sz_w)
        opt_row.addWidget(QLabel("×")); opt_row.addWidget(sz_h)
        opt_row.addWidget(pot_chk); opt_row.addStretch()
        vl.addLayout(opt_row)

        prog = QProgressBar(); prog.setValue(0)
        log  = QTextEdit(); log.setReadOnly(True); log.setMaximumHeight(80)
        vl.addWidget(prog); vl.addWidget(log)

        parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

        def _run():
            src = src_edit.text(); dst = dst_edit.text()
            if not src or not dst:
                log.append("ERROR: set source and output folders"); return
            import os
            from PIL import Image

            def nearest_pot(n):
                p = 1
                while p < n: p <<= 1
                return p

            EXT_MAP = {
                "Any image": [".png",".bmp",".tga",".dds",".jpg",".jpeg",
                              ".tiff",".tif",".pcx",".gif",".webp"],
                "PNG":[".png"],"BMP":[".bmp"],"TGA":[".tga"],"DDS":[".dds"],
                "JPG":[".jpg",".jpeg"],"TIFF":[".tiff",".tif"],"PCX":[".pcx"],
            }
            OUT_EXT = {"PNG":".png","BMP":".bmp","TGA":".tga","DDS":".dds",
                       "JPG":".jpg","TIFF":".tiff","PCX":".pcx"}
            exts  = EXT_MAP.get(ifmt.currentText(), [".png"])
            out_e = OUT_EXT.get(ofmt.currentText(), ".png")
            do_resize = resize_chk.isChecked()
            do_pot    = pot_chk.isChecked()
            files = [f for f in os.listdir(src)
                     if os.path.splitext(f)[1].lower() in exts]
            if not files:
                log.append("No matching files found"); return
            prog.setMaximum(len(files)); ok = err = 0
            for idx, fname in enumerate(files):
                prog.setValue(idx); QApplication.processEvents()
                src_path = os.path.join(src, fname)
                base = os.path.splitext(fname)[0]
                dst_path = os.path.join(dst, base + out_e)
                try:
                    img = Image.open(src_path).convert("RGBA")
                    if do_resize:
                        nw, nh = sz_w.value(), sz_h.value()
                        if do_pot: nw, nh = nearest_pot(nw), nearest_pot(nh)
                        img = img.resize((nw, nh), Image.LANCZOS)
                    img.save(dst_path, ofmt.currentText().split()[0].upper())
                    log.append(f"✓ {fname}"); ok += 1
                except Exception as e:
                    log.append(f"✗ {fname}: {e}"); err += 1
                log.repaint()
            prog.setValue(len(files))
            log.append(f"Done: {ok} ok, {err} errors")

        ov = self._ToolOverlay(parent_vp, self, "Batch Convert Textures",
                               ctrl, apply_fn=_run, generate_fn=None)
        ov.setFixedWidth(min(700, max(500, parent_vp.width() - 40)))
        ov.move(max(0, (parent_vp.width() - ov.width()) // 2),
                parent_vp.height() - ov.height())

    def _make_checkerboard(self, w, h, size=8): #vers 1
        from PyQt6.QtGui import QPixmap, QPainter, QColor

        pm = QPixmap(w, h)
        pm.fill(self._get_ui_color('border'))
        p = QPainter(pm)

        alt = self._get_ui_color('viewport_text')
        for y in range(0, h, size):
            for x in range(0, w, size):
                if (x//size + y//size) % 2:
                    p.fillRect(x, y, size, size, alt)

        p.end()
        return pm


    def _export_amiga_icon(self): #vers 2
        """Export Amiga .info DiskObject — any canvas size, correct structure."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Amiga Icon", "icon.info", "Amiga Icon (*.info)")
        if not path: return
        try:
            self._write_amiga_info(path, bytes(self.dp5_canvas.rgba),
                                   self._canvas_width, self._canvas_height)
            self._set_status(f"Exported Amiga icon: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "Amiga Icon Export Error", str(e))


    def _export_ico(self): #vers 1
        """Export Windows ICO — multiple sizes embedded (16,32,48,64,128,256)."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Windows ICO", "icon.ico", "ICO (*.ico)")
        if not path: return
        try:
            from PIL import Image
            img = self._get_canvas_pil()
            sizes = [s for s in [16,32,48,64,128,256]
                     if s <= max(self._canvas_width, self._canvas_height) * 2]
            frames = [img.resize((s,s), Image.LANCZOS) for s in sizes]
            frames[0].save(path, 'ICO', sizes=[(s,s) for s in sizes],
                           append_images=frames[1:])
            self._set_status(f"Exported ICO: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "ICO Export Error", str(e))


    def _export_svg_icon(self): #vers 1
        """Export canvas as SVG — embeds canvas as base64 PNG."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export SVG Icon", "icon.svg", "SVG (*.svg)")
        if not path: return
        try:
            import base64, io
            buf = io.BytesIO()
            self._get_canvas_pil().save(buf, 'PNG')
            b64 = base64.b64encode(buf.getvalue()).decode()
            w, h = self._canvas_width, self._canvas_height
            svg = (f'<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}">'
                   f'<image width="{w}" height="{h}" href="data:image/png;base64,{b64}"/></svg>')
            open(path, 'w').write(svg)
            self._set_status(f"Exported SVG: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "SVG Export Error", str(e))


    def _export_tga(self): #vers 1
        """Export TGA (Targa) — uncompressed RGBA."""
        if not self.dp5_canvas: return
        w, h = self._canvas_width, self._canvas_height
        path, _ = QFileDialog.getSaveFileName(
            self, "Export TGA", f"texture_{w}x{h}.tga", "TGA (*.tga)")
        if not path: return
        try:
            self._get_canvas_pil().save(path, 'TGA')
            self._set_status(f"Exported TGA: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "TGA Export Error", str(e))


    def _export_dds(self): #vers 1
        """Export DDS (DirectDraw Surface) — uncompressed BGRA8."""
        if not self.dp5_canvas: return
        w, h = self._canvas_width, self._canvas_height
        path, _ = QFileDialog.getSaveFileName(
            self, "Export DDS", f"texture_{w}x{h}.dds", "DDS (*.dds)")
        if not path: return
        try:
            import struct
            rgba = bytes(self.dp5_canvas.rgba)
            bgra = bytearray(len(rgba))
            for i in range(0, len(rgba), 4):
                bgra[i]=rgba[i+2]; bgra[i+1]=rgba[i+1]
                bgra[i+2]=rgba[i]; bgra[i+3]=rgba[i+3]
            hdr = bytearray(128)
            hdr[0:4] = b'DDS '
            struct.pack_into('<I',hdr,4,124)
            struct.pack_into('<I',hdr,8,0x00021007)
            struct.pack_into('<I',hdr,12,h)
            struct.pack_into('<I',hdr,16,w)
            struct.pack_into('<I',hdr,20,w*4)
            struct.pack_into('<I',hdr,28,1)
            struct.pack_into('<I',hdr,76,32)
            struct.pack_into('<I',hdr,80,0x41)
            struct.pack_into('<I',hdr,88,32)
            struct.pack_into('<I',hdr,92,0x00FF0000)
            struct.pack_into('<I',hdr,96,0x0000FF00)
            struct.pack_into('<I',hdr,100,0x000000FF)
            struct.pack_into('<I',hdr,104,0xFF000000)
            struct.pack_into('<I',hdr,108,0x00001000)
            open(path,'wb').write(bytes(hdr)+bytes(bgra))
            self._set_status(f"Exported DDS: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "DDS Export Error", str(e))


    def _export_pcx(self): #vers 1
        """Export PCX — classic DOS bitmap."""
        if not self.dp5_canvas: return
        w, h = self._canvas_width, self._canvas_height
        path, _ = QFileDialog.getSaveFileName(
            self, "Export PCX", f"image_{w}x{h}.pcx", "PCX (*.pcx)")
        if not path: return
        try:
            self._get_canvas_pil().convert('RGB').save(path, 'PCX')
            self._set_status(f"Exported PCX: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "PCX Export Error", str(e))


    def _write_amiga_info(self, path: str, rgba: bytes, w: int, h: int): #vers 1
        """Write Amiga .info DiskObject from RGBA pixel data."""
        import struct
        from PIL import Image
        img = Image.frombytes('RGBA',(w,h),rgba).convert('RGB')
        WB_PAL = [(0,0,0),(255,255,255),(85,170,255),(255,136,0),
                  (170,170,170),(0,0,170),(255,85,0),(170,0,170),
                  (85,85,85),(0,170,170),(170,85,0),(0,170,0),
                  (170,0,0),(0,85,170),(255,255,85),(255,85,85)]
        pal_img = Image.new('P',(1,1))
        flat = sum([list(c) for c in WB_PAL],[]) + [0]*720
        pal_img.putpalette(flat)
        q = img.quantize(palette=pal_img, dither=0)
        pixels = list(q.getdata())
        n_planes = 4
        row_bytes = ((w+15)//16)*2
        planes = [bytearray(row_bytes*h) for _ in range(n_planes)]
        for y in range(h):
            for x in range(w):
                px = pixels[y*w+x] & 15
                for p in range(n_planes):
                    if (px>>p)&1:
                        planes[p][y*row_bytes+x//8] |= 0x80>>(x%8)
        do = bytearray(78)
        struct.pack_into('>H',do,0,0xE310)
        struct.pack_into('>H',do,2,1)
        struct.pack_into('>H',do,12,w)
        struct.pack_into('>H',do,14,h)
        struct.pack_into('>H',do,16,0x0006)
        struct.pack_into('>H',do,20,0x0001)
        struct.pack_into('>I',do,22,0x00000001)
        do[48]=3
        struct.pack_into('>I',do,58,0x80000000)
        struct.pack_into('>I',do,62,0x80000000)
        img_hdr = bytearray(40)
        struct.pack_into('>H',img_hdr,4,w)
        struct.pack_into('>H',img_hdr,6,h)
        struct.pack_into('>H',img_hdr,8,n_planes)
        img_hdr[12] = (1<<n_planes)-1
        open(path,'wb').write(bytes(do)+bytes(img_hdr)+b''.join(bytes(p) for p in planes))


    def _write_icns(self, path: str, img): #vers 2
        """Write Apple ICNS file from PIL image."""
        import struct, io
        from PIL import Image
        sizes = [(16,b'icp4'),(32,b'icp5'),(64,b'icp6'),
                 (128,b'ic07'),(256,b'ic08'),(512,b'ic09'),(1024,b'ic10')]
        chunks = bytearray()
        for sz, code in sizes:
            frame = img.resize((sz,sz), Image.LANCZOS)
            buf = io.BytesIO(); frame.save(buf,'PNG')
            png = buf.getvalue()
            chunks += code + struct.pack('>I', 8+len(png)) + png
        total = 8+len(chunks)
        open(path,'wb').write(b'icns'+struct.pack('>I',total)+bytes(chunks))


    def _import_icns(self): #vers 1
        """Import Apple ICNS — loads largest available size."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import Apple ICNS", "", "ICNS (*.icns);;All Files (*)")
        if not path: return
        try:
            from PIL import Image
            img = Image.open(path).convert('RGBA')
            self._load_rgba(bytearray(img.tobytes()), img.width, img.height,
                           os.path.basename(path))
        except Exception as e:
            QMessageBox.warning(self, "ICNS Import Error", str(e))


    def _export_icns(self): #vers 1
        """Export Apple ICNS — macOS icon bundle with multiple sizes."""
        if not self.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Apple ICNS", "icon.icns", "ICNS (*.icns)")
        if not path: return
        try:
            self._write_icns(path, self._get_canvas_pil())
            self._set_status(f"Exported ICNS: {os.path.basename(path)}")
        except Exception as e:
            QMessageBox.warning(self, "ICNS Export Error", str(e))


    def _import_ico(self): #vers 1
        """Import Windows ICO — load largest frame into canvas."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import Windows ICO", "", "ICO (*.ico);;All Files (*)")
        if not path: return
        try:
            from PIL import Image
            ico = Image.open(path)
            # Get largest frame
            sizes = ico.info.get('sizes', [(ico.width, ico.height)])
            largest = max(sizes, key=lambda s: s[0]*s[1])
            ico.size = largest
            ico.seek(0)
            # Find frame matching largest size
            try:
                frames = []
                while True:
                    frames.append((ico.size, ico.copy().convert('RGBA')))
                    ico.seek(ico.tell()+1)
            except EOFError:
                pass
            if frames:
                best = max(frames, key=lambda f: f[0][0]*f[0][1])
                img = best[1]
            else:
                img = ico.convert('RGBA')
            # Apply bit depth if set
            self._canvas_bit_depth = getattr(self,'_canvas_bit_depth',0)
            self._load_rgba(bytearray(img.tobytes()), img.width, img.height,
                           os.path.basename(path))
        except Exception as e:
            QMessageBox.warning(self, "ICO Import Error", str(e))


    def _import_svg(self): #vers 1
        """Import SVG icon — rasterize to current canvas size."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import SVG", "", "SVG (*.svg);;All Files (*)")
        if not path: return
        try:
            # Try cairosvg first, fall back to Qt SVG renderer
            w = self._canvas_width; h = self._canvas_height
            try:
                import cairosvg, io
                from PIL import Image
                png_data = cairosvg.svg2png(url=path, output_width=w, output_height=h)
                img = Image.open(io.BytesIO(png_data)).convert('RGBA')
            except ImportError:
                from PyQt6.QtSvg import QSvgRenderer
                from PyQt6.QtGui import QImage, QPainter
                renderer = QSvgRenderer(path)
                qimg = QImage(w, h, QImage.Format.Format_ARGB32)
                qimg.fill(0)
                painter = QPainter(qimg)
                renderer.render(painter)
                painter.end()
                from PIL import Image
                buf = qimg.bits().asarray(w*h*4)
                # Qt ARGB32 → RGBA
                arr = bytearray(buf)
                for i in range(0, len(arr), 4):
                    arr[i], arr[i+2] = arr[i+2], arr[i]  # BGR→RGB
                img = Image.frombytes('RGBA',(w,h),bytes(arr))
            self._load_rgba(bytearray(img.tobytes()), w, h, os.path.basename(path))
        except Exception as e:
            QMessageBox.warning(self, "SVG Import Error", str(e))


    #    Animation                                                              

    def _create_anim_strip(self): #vers 1
        """Create the animation timeline strip widget."""
        from PyQt6.QtCore import QTimer
        strip = QWidget()
        strip.setFixedHeight(64)
        strip.setStyleSheet("background:palette(base);")
        hl = QHBoxLayout(strip)
        hl.setContentsMargins(4, 2, 4, 2)
        hl.setSpacing(4)

        # Transport buttons
        def tbtn(label, tip, slot):
            b = QPushButton(label)
            b.setFixedSize(28, 28)
            b.setToolTip(tip)
            b.clicked.connect(slot)
            b.setStyleSheet("QPushButton{background:#333;color:palette(windowText);border:1px solid palette(mid);border-radius:3px;}"
                            "QPushButton:hover{background:palette(mid);}")
            return b

        hl.addWidget(tbtn("|◀", "First frame",    self._anim_first))
        hl.addWidget(tbtn("◀",  "Previous frame",  self._anim_prev))
        self._anim_play_btn = tbtn("▶", "Play / Stop", self._anim_toggle_play)
        hl.addWidget(self._anim_play_btn)
        hl.addWidget(tbtn("▶|", "Next frame",     self._anim_next))
        hl.addWidget(tbtn("|▶|","Last frame",     self._anim_last))
        hl.addSpacing(6)
        hl.addWidget(tbtn("+",  "Add frame (copy current)", self._anim_add_frame))
        hl.addWidget(tbtn("×",  "Delete current frame",      self._anim_del_frame))
        hl.addWidget(tbtn("⬆", "Duplicate frame",            self._anim_dup_frame))
        hl.addSpacing(6)

        # FPS
        fps_lbl = QLabel("FPS:")
        fps_lbl.setStyleSheet("color:palette(mid); font-size:11px;")
        self._anim_fps_spin = QSpinBox()
        self._anim_fps_spin.setRange(1, 60)
        self._anim_fps_spin.setValue(self.dp5_settings.get('anim_fps'))
        self._anim_fps_spin.setFixedWidth(52)
        self._anim_fps_spin.setStyleSheet("background:#333;color:palette(windowText);border:1px solid palette(mid);")
        hl.addWidget(fps_lbl)
        hl.addWidget(self._anim_fps_spin)
        hl.addSpacing(6)

        # Frame counter
        self._anim_frame_lbl = QLabel("Frame 1/1")
        self._anim_frame_lbl.setStyleSheet("color:palette(mid); font-size:11px; min-width:70px;")
        hl.addWidget(self._anim_frame_lbl)

        # Frame thumbnail scroll
        self._anim_thumb_area = QScrollArea()
        self._anim_thumb_area.setFixedHeight(58)
        self._anim_thumb_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOn)
        self._anim_thumb_area.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._anim_thumb_area.setStyleSheet("background:#111; border:none;")
        self._anim_thumb_container = QWidget()
        self._anim_thumb_layout = QHBoxLayout(self._anim_thumb_container)
        self._anim_thumb_layout.setContentsMargins(2,2,2,2)
        self._anim_thumb_layout.setSpacing(3)
        self._anim_thumb_area.setWidget(self._anim_thumb_container)
        self._anim_thumb_area.setWidgetResizable(True)
        hl.addWidget(self._anim_thumb_area, 1)

        # Init with single frame from current canvas
        self._anim_timer = QTimer()
        self._anim_timer.timeout.connect(self._anim_tick)
        self._anim_init_frames()

        return strip


    def _anim_init_frames(self): #vers 1
        """Initialise animation with the current canvas as frame 0."""
        if self.dp5_canvas and self.dp5_canvas.rgba:
            self._frames = [bytearray(self.dp5_canvas.rgba)]
        else:
            w, h = self._canvas_width, self._canvas_height
            self._frames = [bytearray(b'\x80\x80\x80\xff' * (w * h))]
        self._frame_delays = [1000 // max(1, self.dp5_settings.get('anim_fps'))]
        self._current_frame = 0
        self._anim_refresh_thumbs()


    def _anim_save_current_frame(self): #vers 1
        """Write canvas rgba back into current frame buffer."""
        if self.dp5_canvas and 0 <= self._current_frame < len(self._frames):
            self._frames[self._current_frame] = bytearray(self.dp5_canvas.rgba)


    def _anim_load_frame(self, idx: int): #vers 2
        """Load frame idx onto the canvas."""
        if not self.dp5_canvas: return
        if not (0 <= idx < len(self._frames)): return
        self._anim_save_current_frame()
        self._current_frame = idx
        rgba = self._frames[idx]
        if len(rgba) == len(self.dp5_canvas.rgba):
            self.dp5_canvas.rgba[:] = rgba
        else:
            self.dp5_canvas.rgba = bytearray(rgba)
        # Update onion skin to previous frame
        if self.dp5_canvas.onion_skin and idx > 0:
            self.dp5_canvas.onion_rgba = bytearray(self._frames[idx-1])
        elif self.dp5_canvas.onion_skin and len(self._frames) > 1:
            self.dp5_canvas.onion_rgba = bytearray(self._frames[-1])
        self.dp5_canvas.update()
        self._anim_update_label()
        self._anim_highlight_thumb(idx)


    def _anim_update_label(self): #vers 1
        if hasattr(self, '_anim_frame_lbl'):
            self._anim_frame_lbl.setText(
                f"Frame {self._current_frame+1}/{len(self._frames)}")


    def _anim_refresh_thumbs(self): #vers 1
        """Rebuild all frame thumbnails."""
        if not hasattr(self, '_anim_thumb_layout'): return
        # Clear
        while self._anim_thumb_layout.count():
            w = self._anim_thumb_layout.takeAt(0).widget()
            if w: w.deleteLater()
        # Rebuild
        tw, th = 48, 36
        for i, frame in enumerate(self._frames):
            btn = QPushButton()
            btn.setFixedSize(tw+4, th+4)
            btn.setCheckable(True)
            btn.setChecked(i == self._current_frame)
            btn.setToolTip(f"Frame {i+1}")
            # Draw thumbnail
            from PyQt6.QtGui import QPixmap, QImage
            w, h = self._canvas_width, self._canvas_height
            if len(frame) == w*h*4:
                qimg = QImage(bytes(frame), w, h, w*4, QImage.Format.Format_RGBA8888)
                pix = QPixmap.fromImage(qimg).scaled(tw, th, Qt.AspectRatioMode.KeepAspectRatio)
                btn.setIcon(QIcon(pix))
                btn.setIconSize(pix.size())
            btn.clicked.connect(lambda _, idx=i: self._anim_load_frame(idx))
            btn.setStyleSheet(
                f"QPushButton{{background:{'#4466aa' if i==self._current_frame else '#333'};"
                "border:1px solid #666;border-radius:2px;padding:1px;}"
                "QPushButton:checked{border:2px solid #88aaff;}")
            self._anim_thumb_layout.addWidget(btn)
        self._anim_thumb_layout.addStretch()
        self._anim_update_label()


    def _anim_highlight_thumb(self, idx: int): #vers 1
        """Just update highlight without full rebuild."""
        layout = self._anim_thumb_layout
        for i in range(layout.count()):
            item = layout.itemAt(i)
            if item and item.widget():
                item.widget().setChecked(i == idx)
                item.widget().setStyleSheet(
                    f"QPushButton{{background:{'#4466aa' if i==idx else '#333'};"
                    "border:1px solid #666;border-radius:2px;padding:1px;}"
                    "QPushButton:checked{border:2px solid #88aaff;}")


    def _anim_add_frame(self): #vers 1
        """Add a new frame (copy of current)."""
        self._anim_save_current_frame()
        new_frame = bytearray(self._frames[self._current_frame])
        fps = self._anim_fps_spin.value() if hasattr(self,'_anim_fps_spin') else 12
        self._frames.insert(self._current_frame+1, new_frame)
        self._frame_delays.insert(self._current_frame+1, 1000//fps)
        self._current_frame += 1
        self._anim_load_frame(self._current_frame)
        self._anim_refresh_thumbs()


    def _anim_dup_frame(self): #vers 1
        """Duplicate current frame at end."""
        self._anim_save_current_frame()
        self._frames.append(bytearray(self._frames[self._current_frame]))
        fps = self._anim_fps_spin.value() if hasattr(self,'_anim_fps_spin') else 12
        self._frame_delays.append(1000//fps)
        self._anim_refresh_thumbs()


    def _anim_del_frame(self): #vers 1
        """Delete current frame (min 1 frame)."""
        if len(self._frames) <= 1:
            self._set_status("Cannot delete the only frame")
            return
        self._frames.pop(self._current_frame)
        self._frame_delays.pop(self._current_frame)
        self._current_frame = max(0, self._current_frame-1)
        self._anim_load_frame(self._current_frame)
        self._anim_refresh_thumbs()


    def _anim_first(self): #vers 1
        self._anim_load_frame(0)


    def _anim_last(self): #vers 1
        self._anim_load_frame(len(self._frames)-1)


    def _anim_prev(self): #vers 1
        self._anim_load_frame(max(0, self._current_frame-1))


    def _anim_next(self): #vers 1
        self._anim_load_frame(min(len(self._frames)-1, self._current_frame+1))


    def _anim_toggle_play(self): #vers 1
        if self._anim_playing:
            self._anim_playing = False
            self._anim_timer.stop()
            if hasattr(self, '_anim_play_btn'):
                self._anim_play_btn.setText("▶")
            self._set_status("Animation stopped")
        else:
            self._anim_save_current_frame()
            self._anim_playing = True
            fps = self._anim_fps_spin.value() if hasattr(self,'_anim_fps_spin') else 12
            self._anim_timer.start(1000 // fps)
            if hasattr(self, '_anim_play_btn'):
                self._anim_play_btn.setText("⏹")
            self._set_status(f"Playing {len(self._frames)} frames @ {fps}fps")


    def _anim_tick(self): #vers 1
        """Advance to next frame during playback."""
        if not self._anim_playing: return
        next_f = (self._current_frame + 1) % len(self._frames)
        self._current_frame = next_f
        if self.dp5_canvas and len(self._frames[next_f]) == len(self.dp5_canvas.rgba):
            self.dp5_canvas.rgba[:] = self._frames[next_f]
            self.dp5_canvas.update()
        self._anim_update_label()
        self._anim_highlight_thumb(next_f)


    def _anim_export_gif(self): #vers 1
        """Export all frames as an animated GIF."""
        if not self._frames: return
        fps = self._anim_fps_spin.value() if hasattr(self,'_anim_fps_spin') else 12
        delay = 1000 // fps  # ms per frame
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Animated GIF", "animation.gif", "GIF (*.gif)")
        if not path: return
        try:
            from PIL import Image
            w, h = self._canvas_width, self._canvas_height
            pil_frames = []
            for frame in self._frames:
                if len(frame) == w*h*4:
                    img = Image.frombytes('RGBA', (w,h), bytes(frame)).convert('RGBA')
                    pil_frames.append(img)
            if not pil_frames: return
            pil_frames[0].save(
                path, format='GIF', save_all=True,
                append_images=pil_frames[1:],
                duration=delay, loop=0, optimize=True)
            self._set_status(
                f"Exported GIF: {os.path.basename(path)}  "
                f"{len(pil_frames)} frames @ {fps}fps")
        except Exception as e:
            QMessageBox.warning(self, "GIF Export Error", str(e))


    def _anim_export_png_seq(self): #vers 1
        """Export all frames as a numbered PNG sequence."""
        if not self._frames: return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export PNG Sequence (base name)", "frame_0001.png", "PNG (*.png)")
        if not path: return
        try:
            from PIL import Image
            import re as _re
            base = _re.sub(r'\d+\.png$', '', path)
            w, h = self._canvas_width, self._canvas_height
            for i, frame in enumerate(self._frames):
                if len(frame) == w*h*4:
                    img = Image.frombytes('RGBA',(w,h),bytes(frame))
                    img.save(f"{base}{i+1:04d}.png", 'PNG')
            self._set_status(f"Exported {len(self._frames)} PNG frames")
        except Exception as e:
            QMessageBox.warning(self, "PNG Sequence Error", str(e))


    def _apply_theme(self): #vers 5
        """Apply global app theme — uses QApplication stylesheet set by app_settings."""
        try:
            mw = getattr(self, 'main_window', None)
            app_settings = None
            if hasattr(self, 'app_settings') and self.app_settings:
                app_settings = self.app_settings
            elif mw and hasattr(mw, 'app_settings'):
                app_settings = mw.app_settings

            if app_settings and hasattr(app_settings, 'get_stylesheet'):
                # Apply to QApplication so all widgets inherit it
                from PyQt6.QtWidgets import QApplication
                ss = app_settings.get_stylesheet()
                if ss:
                    QApplication.instance().setStyleSheet(ss)
                    # Apply panel effects (fill/gradient/pattern) if configured
            try:
                from apps.utils.app_settings_system import apply_panel_effects
                apply_panel_effects(self, app_settings)
            except Exception:
                pass
            # Clear any widget-level override so we inherit from QApplication
            self.setStyleSheet("")
            # gadgetbar_bg applied via QFrame#titlebar in global stylesheet — no manual refresh needed
            # Refresh icons so they contrast correctly with new theme
            self._refresh_icons()
        except Exception as e:
            print(f"Theme application error: {e}")


    def _refresh_icons(self): #vers 2
        """Refresh ALL icons with current theme colours."""
        SVGIconFactory.clear_cache()
        icon_col = self._get_icon_color()

        # Get tile_bg from theme (panel_bg or bg_secondary)
        tile_bg = ''
        try:
            if self.app_settings:
                tc = self.app_settings.get_theme_colors() or {}
                tile_bg = tc.get('gadgetbar_bg',
                            tc.get('toolbar_bg',
                                tc.get('bg_secondary', '')))
        except Exception:
            pass

        # Gadget bar buttons (settings, properties, window chrome)
        for attr, method in [
            ('settings_btn',    'settings_icon'),
            ('properties_btn',  'properties_icon'),
        ]:
            if hasattr(self, attr):
                getattr(self, attr).setIcon(
                    getattr(SVGIconFactory, method)(20, icon_col))
        for attr, method in [('minimize_btn','minimize_icon'),
                              ('maximize_btn','maximize_icon'),
                              ('close_btn','close_icon')]:
            if hasattr(self, attr):
                getattr(self, attr).setIcon(
                    getattr(SVGIconFactory, method)(20, icon_col))

        # Tool grid buttons — redraw with theme colours
        icon_sz = self.dp5_settings.get('tool_icon_size', 22)
        for tool_id, btn in getattr(self, '_tool_btns', {}).items():
            from apps.components.DP5_Workshop.dp5_workshop import _load_tool_icon
            active = btn.isChecked()
            ico = _load_tool_icon(tool_id, icon_sz, active=active,
                                  tile_bg=tile_bg, icon_col=icon_col)
            btn.setIcon(ico)
            btn.setIconSize(QSize(icon_sz, icon_sz))

        # Brush manager button if present
        if hasattr(self, 'brush_mgr_btn'):
            try:
                from apps.components.DP5_Workshop.dp5_workshop import get_brushes_icon
                self.brush_mgr_btn.setIcon(get_brushes_icon(18, icon_col))
            except Exception:
                pass

        # Recent colour buttons — update background to theme base
        try:
            if self.app_settings:
                tc = self.app_settings.get_theme_colors() or {}
                empty_bg = tc.get('bg_secondary', tc.get('panel_primary', ''))
            else:
                empty_bg = ''
        except Exception:
            empty_bg = ''
        for i, btn in enumerate(getattr(self, '_color_hist_btns', [])):
            if i < len(getattr(self, '_color_history', [])):
                h = self._color_history[i]
                btn.setStyleSheet(f"background:{h}; border:1px solid palette(mid);")
            else:
                bg = empty_bg if empty_bg else 'palette(base)'
                btn.setStyleSheet(f"background:{bg}; border:1px solid palette(mid);")


    def _launch_theme_settings(self): #vers 1
        try:
            if not APPSETTINGS_AVAILABLE: return
            dialog = SettingsDialog(self.app_settings, self)
            dialog.themeChanged.connect(lambda _: self._apply_theme())
            if dialog.exec():
                self._apply_theme()
        except Exception as e:
            QMessageBox.warning(self, "Theme Error", str(e))


    def _get_icon_color(self) -> str: #vers 4
        """Return icon colour per tool_icon_color setting and theme."""
        mode = self.dp5_settings.get('tool_icon_color', 'color')
        if self.app_settings:
            colors = self.app_settings.get_theme_colors()
            if mode == 'white':
                return colors.get('text_background', colors.get('text_primary', '#eeeeee'))
            if mode == 'dark':
                return colors.get('text_primary', colors.get('panel_primary', '#222222'))
            # 'color' — auto-detect from gadgetbar_bg
            bar_bg = colors.get('gadgetbar_bg', colors.get('toolbar_bg', ''))
            try:
                r = int(bar_bg[1:3], 16)
                g = int(bar_bg[3:5], 16)
                b = int(bar_bg[5:7], 16)
                if (r*299 + g*587 + b*114) // 1000 > 128:
                    return colors.get('text_primary', colors.get('panel_primary', '#222222'))
                else:
                    return colors.get('text_background', colors.get('text_primary', '#eeeeee'))
            except Exception:
                return colors.get('text_primary', '#eeeeee')
        return '#eeeeee'


    #    Window management                                                      

    def _set_status(self, msg: str): #vers 1
        if hasattr(self, '_status_bar'):
            self._status_bar.showMessage(msg)
        # Always refresh permanent info labels
        if hasattr(self, '_status_size_lbl'):
            w = getattr(self, '_canvas_width', 0)
            h = getattr(self, '_canvas_height', 0)
            self._status_size_lbl.setText(f"{w}×{h}")
        if hasattr(self, '_status_depth_lbl'):
            depth = getattr(self, '_canvas_bit_depth', 0)
            labels = {0:'RGBA32', 1:'RGB24', 2:'RGB16', 3:'Idx8'}
            self._status_depth_lbl.setText(labels.get(depth, 'RGBA32'))


    def _toggle_maximize(self): #vers 1
        if self.isMaximized(): self.showNormal()
        else: self.showMaximized()


    def keyPressEvent(self, e): #vers 1
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
                self._set_zoom(z * 1.25 if z < 1 else min(64, z + 1))
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
        Qt.Key.Key_B: TOOL_BLUR_BRUSH,
        Qt.Key.Key_U: TOOL_SMUDGE,
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
        # Animation shortcuts (only when anim strip visible)
        elif k == Qt.Key.Key_Comma and hasattr(self,'_anim_strip') and self._anim_strip.isVisible():
            self._anim_prev()
        elif k == Qt.Key.Key_Period and hasattr(self,'_anim_strip') and self._anim_strip.isVisible():
            self._anim_next()
        elif k == Qt.Key.Key_Space and hasattr(self,'_anim_strip') and self._anim_strip.isVisible():
            self._anim_toggle_play()
        else:
            super().keyPressEvent(e)


    def closeEvent(self, event): #vers 2
        # Save splitter positions so they restore on next open
        if hasattr(self, '_splitter') and self._splitter:
            self.dp5_settings.set('splitter_sizes', self._splitter.sizes())
            self.dp5_settings.save()
        # Remove injected tool menu from imgfactory menubar
        try:
            mw = getattr(self, 'main_window', None) or getattr(self, '_imgfactory', None)
            if mw and hasattr(mw, '_update_tool_menu_for_tab'):
                mw._update_tool_menu_for_tab(None)
        except Exception:
            pass
        self.window_closed.emit()
        event.accept()


    #    Corner resize + dragging (COL Workshop pattern)                        

    def _update_transform_text_panel_visibility(self): #vers 2
        """Toggle between text+icon panel (wide) and icon-only strip (narrow).
        Reads threshold from IMG Factory settings. Also collapses bottom buttons."""
        tp   = getattr(self, '_transform_text_panel_ref', None)
        ip   = getattr(self, '_transform_icon_panel_ref', None)
        mode = getattr(self, 'button_display_mode', 'both')

        if mode == 'icons':
            if tp: tp.setVisible(False)
            if ip: ip.setVisible(True)
            return
        if mode == 'text':
            if tp: tp.setVisible(True)
            if ip: ip.setVisible(False)
            return

        # Measure right panel width directly
        rp = getattr(self, '_right_panel_ref', None)
        if rp:
            ref_w = rp.width()
        else:
            splitter = getattr(self, '_main_splitter', None)
            ref_w = self.width()
            if splitter and tp:
                w = tp
                while w and w.parent() is not splitter:
                    w = w.parent() if hasattr(w, 'parent') else None
                if w:
                    ref_w = w.width()

        try:
            from apps.methods.imgfactory_ui_settings import get_collapse_threshold
            threshold = get_collapse_threshold(getattr(self, 'main_window', None))
        except Exception:
            threshold = 550
        wide = ref_w >= threshold
        if tp: tp.setVisible(wide)
        if ip: ip.setVisible(not wide)

        # Toggle bottom panel rows the same way
        btr = getattr(self, '_bottom_text_row', None)
        bir = getattr(self, '_bottom_icon_row', None)
        if btr: btr.setVisible(wide)
        if bir: bir.setVisible(not wide)


    def _get_resize_corner(self, pos): #vers 1
        size = self.corner_size; w = self.width(); h = self.height()
        if pos.x() < size and pos.y() < size:           return "top-left"
        if pos.x() > w - size and pos.y() < size:       return "top-right"
        if pos.x() < size and pos.y() > h - size:       return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:   return "bottom-right"
        return None


    def _update_cursor(self, direction): #vers 1
        cursors = {
            "top-left":     Qt.CursorShape.SizeFDiagCursor,
            "bottom-right": Qt.CursorShape.SizeFDiagCursor,
            "top-right":    Qt.CursorShape.SizeBDiagCursor,
            "bottom-left":  Qt.CursorShape.SizeBDiagCursor,
        }
        self.setCursor(cursors.get(direction, Qt.CursorShape.ArrowCursor))


    def _get_resize_direction(self, pos): #vers 1
        """Determine resize direction based on mouse position"""
        rect = self.rect()
        margin = self.resize_margin

        left = pos.x() < margin
        right = pos.x() > rect.width() - margin
        top = pos.y() < margin
        bottom = pos.y() > rect.height() - margin

        if left and top:
            return "top-left"
        elif right and top:
            return "top-right"
        elif left and bottom:
            return "bottom-left"
        elif right and bottom:
            return "bottom-right"
        elif left:
            return "left"
        elif right:
            return "right"
        elif top:
            return "top"
        elif bottom:
            return "bottom"

        return None


    def _is_on_draggable_area(self, pos): #vers 1
        if not hasattr(self, 'titlebar'):
            return False
        if not self.titlebar.rect().contains(pos):
            return False
        for w in self.titlebar.findChildren(QPushButton):
            if w.isVisible() and w.geometry().contains(pos):
                return False
        return True


    def mousePressEvent(self, event): #vers 1
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


    def mouseMoveEvent(self, event): #vers 1
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept(); return
        else:
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()
            self._refresh_corner_overlay()
            self._update_cursor(corner)
        super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event): #vers 1
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    def _handle_corner_resize(self, global_pos): #vers 1
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


    def _handle_resize(self, global_pos): #vers 1
        """Handle window resizing"""
        if not self.resize_direction or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.frameGeometry()

        min_width = 800
        min_height = 600

        # Handle horizontal resizing
        if "left" in self.resize_direction:
            new_width = geometry.width() - delta.x()
            if new_width >= min_width:
                geometry.setLeft(geometry.left() + delta.x())
        elif "right" in self.resize_direction:
            new_width = geometry.width() + delta.x()
            if new_width >= min_width:
                geometry.setRight(geometry.right() + delta.x())

        # Handle vertical resizing
        if "top" in self.resize_direction:
            new_height = geometry.height() - delta.y()
            if new_height >= min_height:
                geometry.setTop(geometry.top() + delta.y())
        elif "bottom" in self.resize_direction:
            new_height = geometry.height() + delta.y()
            if new_height >= min_height:
                geometry.setBottom(geometry.bottom() + delta.y())

        self.setGeometry(geometry)
        self.drag_position = global_pos


    def paintEvent(self, event): #vers 2
        super().paintEvent(event)
        # Corner handles drawn by _corner_overlay overlay widget

    def _setup_corner_overlay(self): #vers 3
        """Create or re-raise the corner resize overlay.
        Only active in standalone (frameless) mode.
        Called from showEvent and resizeEvent with a delay so all child
        widgets are laid out before we raise_() above them.
        """
        if not self.standalone_mode:
            return
        if not (self.windowFlags() & Qt.WindowType.FramelessWindowHint):
            return
        if hasattr(self, '_corner_overlay') and self._corner_overlay:
            self._corner_overlay.setGeometry(0, 0, self.width(), self.height())
            self._corner_overlay.raise_()
            self._corner_overlay.update_state(
                getattr(self, 'hover_corner', None), self.app_settings)
            return
        overlay = _CornerOverlay(self)
        overlay.update_state(getattr(self, 'hover_corner', None), self.app_settings)
        self._corner_overlay = overlay
        overlay.setGeometry(0, 0, self.width(), self.height())
        overlay.show()
        overlay.raise_()

    def showEvent(self, event): #vers 3
        super().showEvent(event)
        from PyQt6.QtCore import QTimer
        # Two-shot: 100ms for layout settle, 400ms to ensure all children rendered
        QTimer.singleShot(100, self._setup_corner_overlay)
        QTimer.singleShot(400, self._setup_corner_overlay)
        # Rebuild right panel so tool icons lay out correctly on first show
        QTimer.singleShot(150, self._rebuild_right_panel)

    def resizeEvent(self, event): #vers 1
        super().resizeEvent(event)
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(50, self._setup_corner_overlay)

    def _refresh_corner_overlay(self): #vers 2
        if hasattr(self, '_corner_overlay') and self._corner_overlay:
            self._corner_overlay.setGeometry(0, 0, self.width(), self.height())
            self._corner_overlay.update_state(
                getattr(self, 'hover_corner', None), self.app_settings)
            self._corner_overlay.raise_()


#  Public factory function

def open_dp5_workshop(main_window=None) -> DP5Workshop: #vers 1
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


#  Character / Font Editor


class _DockablePanelMixin:
    """
    Mixin for floating DP5 Tool panels.
    Adds a [D] button that snaps the panel to the left or right edge of the
    canvas viewport, or returns it to a free-floating Tool window.
    Usage: inherit alongside QWidget, call _init_dock(workshop, side='left').
    """

    def _init_dock(self, workshop, settings_key: str = '',
                   side: str = 'left'): #vers 1
        """Call from __init__ after _build_ui(). workshop = DP5Workshop instance."""
        self._dmp_workshop    = workshop
        self._dmp_settings_key = settings_key  # e.g. 'svg_browser_docked'
        self._dmp_side        = side            # 'left' or 'right'
        self._dmp_docked      = False
        self._dmp_dock_btn    = None            # set by _add_dock_button()

    def _add_dock_button(self, layout): #vers 1
        """Insert a [D] checkable button into the given QHBoxLayout."""
        from PyQt6.QtWidgets import QPushButton
        btn = QPushButton("D")
        btn.setFixedSize(22, 22)
        btn.setFlat(True)
        btn.setCheckable(True)
        btn.setToolTip("Snap to canvas edge / float")
        btn.clicked.connect(self._dmp_toggle_dock)
        self._dmp_dock_btn = btn
        layout.addWidget(btn)
        # Restore saved state
        ws = getattr(self, '_dmp_workshop', None)
        if ws and self._dmp_settings_key and hasattr(ws, 'dp5_settings'):
            saved = ws.dp5_settings.get(self._dmp_settings_key, False)
            if saved:
                btn.setChecked(True)
                from PyQt6.QtCore import QTimer
                QTimer.singleShot(150, self._dmp_snap)

    def _dmp_toggle_dock(self): #vers 1
        if self._dmp_dock_btn and self._dmp_dock_btn.isChecked():
            self._dmp_snap()
        else:
            self._dmp_float()
        self._dmp_save()

    def _dmp_snap(self): #vers 2
        """Snap panel to left or right edge of canvas viewport."""
        ws = getattr(self, '_dmp_workshop', None)
        if not ws or not hasattr(ws, '_canvas_scroll'):
            return
        vp = ws._canvas_scroll.viewport()
        self.setWindowFlags(Qt.WindowType.Widget)
        self.setParent(vp)
        # Semi-transparent background
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground, False)
        self.setWindowOpacity(0.88)
        self._dmp_docked = True
        if self._dmp_dock_btn:
            self._dmp_dock_btn.setChecked(True)
        # Position after reparent — use timer so Qt processes the parent change first
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(0, self._dmp_reposition)
        self.show(); self.raise_()

    def _dmp_reposition(self): #vers 2
        """Reposition docked panel to correct edge."""
        ws = getattr(self, '_dmp_workshop', None)
        if not ws or not hasattr(ws, '_canvas_scroll'):
            return
        vp = ws._canvas_scroll.viewport()
        vp_w = vp.width(); vp_h = vp.height()
        pw = min(240, vp_w // 3)  # max 1/3 of canvas width
        ph = vp_h - 8
        if self._dmp_side == 'right':
            self.move(vp_w - pw - 4, 4)
        else:
            self.move(4, 4)
        self.resize(pw, ph)
        self.raise_()

    def _dmp_float(self): #vers 2
        """Return panel to free-floating Tool window."""
        try:
            pos = self.mapToGlobal(self.rect().topLeft())
        except Exception:
            pos = None
        self.setWindowOpacity(1.0)
        self.setParent(None)
        self.setWindowFlags(Qt.WindowType.Tool |
                            Qt.WindowType.WindowStaysOnTopHint)
        if pos:
            self.move(pos)
        self.show()
        self._dmp_docked = False
        if self._dmp_dock_btn:
            self._dmp_dock_btn.setChecked(False)

    def _dmp_save(self): #vers 1
        ws = getattr(self, '_dmp_workshop', None)
        if ws and self._dmp_settings_key and hasattr(ws, 'dp5_settings'):
            ws.dp5_settings.set(self._dmp_settings_key, self._dmp_docked)
            ws.dp5_settings.save()


class _CharFontEditor(_DockablePanelMixin, QWidget):
    """Edit 8×8 or 8×16 pixel character sets — bit grid per character,
    load/save binary, export as C header or ASM data.
    Left panel: system font browser — click to load glyphs.
    Floating Tool window, stays on top.
    """

    CELL = 24   # display pixels per bit-cell

    def __init__(self, parent=None): #vers 2
        super().__init__(parent, Qt.WindowType.Tool |
                         Qt.WindowType.WindowStaysOnTopHint)
        self.setWindowTitle("Character / Font Editor")
        self.setMinimumSize(820, 560)
        self._char_w = 8
        self._char_h = 8
        self._n_chars = 128
        # Each char: list of n_chars lists, each char_h bytes
        self._chars = [[0]*self._char_h for _ in range(self._n_chars)]
        self._current = 0
        self._init_dock(parent, 'char_editor_docked', 'left')
        self._build_ui()
        self._refresh_grid()
        self._refresh_char_list()


    def _build_ui(self): #vers 1
        lay = QHBoxLayout(self)

        #    Left: font browser + character list
        left = QVBoxLayout()

        # Title + D button
        _te_row = QHBoxLayout()
        _te_row.addWidget(QLabel("Font / Char Editor"))
        self._add_dock_button(_te_row)
        left.addLayout(_te_row)

        # Font browser
        left.addWidget(QLabel("System Font:"))
        self._font_search = QLineEdit()
        self._font_search.setPlaceholderText("Filter fonts…")
        self._font_search.textChanged.connect(self._filter_fonts)
        left.addWidget(self._font_search)
        self._font_list = QListWidget()
        self._font_list.setFixedWidth(160)
        self._font_list.setMaximumHeight(160)
        self._font_list.itemDoubleClicked.connect(self._load_system_font)
        self._font_list.setToolTip("Double-click to load font glyphs into grid")
        left.addWidget(self._font_list)
        self._populate_font_list()

        # Divider
        from PyQt6.QtWidgets import QFrame as _QF
        _sep = _QF(); _sep.setFrameShape(_QF.Shape.HLine)
        left.addWidget(_sep)

        # Char set size + character list
        hl = QHBoxLayout()
        hl.addWidget(QLabel("Char set:"))
        self._size_combo = QComboBox()
        self._size_combo.addItems(["8×8","8×16"])
        self._size_combo.currentTextChanged.connect(self._on_size_change)
        hl.addWidget(self._size_combo)
        left.addLayout(hl)

        self._char_list = QListWidget()
        self._char_list.setFixedWidth(160)
        self._char_list.currentRowChanged.connect(self._on_char_select)
        left.addWidget(self._char_list)

        btn_row = QHBoxLayout()
        clr_btn = QPushButton("Clear")
        clr_btn.clicked.connect(self._clear_char)
        inv_btn = QPushButton("Invert")
        inv_btn.clicked.connect(self._invert_char)
        btn_row.addWidget(clr_btn); btn_row.addWidget(inv_btn)
        left.addLayout(btn_row)

        shift_row = QHBoxLayout()
        for lbl, fn in [("←",self._shift_l),("→",self._shift_r),
                        ("↑",self._shift_u),("↓",self._shift_d)]:
            b = QPushButton(lbl); b.setFixedWidth(28)
            b.clicked.connect(fn); shift_row.addWidget(b)
        left.addLayout(shift_row)
        lay.addLayout(left)

        #    Centre: bit grid                                          
        centre = QVBoxLayout()
        self._grid_widget = _CharGrid(self)
        self._grid_widget.bit_toggled.connect(self._on_bit_toggle)
        centre.addWidget(self._grid_widget, alignment=Qt.AlignmentFlag.AlignCenter)
        self._hex_label = QLabel("Hex: 00 00 00 00 00 00 00 00")
        self._hex_label.setFont(QFont("Courier", 9))
        centre.addWidget(self._hex_label)
        lay.addLayout(centre)

        #    Right: actions                                            
        right = QVBoxLayout()
        right.addWidget(QLabel("File:"))

        load_btn = QPushButton("Load binary…")
        load_btn.clicked.connect(self._load_binary)
        save_btn = QPushButton("Save binary…")
        save_btn.clicked.connect(self._save_binary)
        c_btn = QPushButton("Export C header…")
        c_btn.clicked.connect(self._export_c)
        asm_btn = QPushButton("Export ASM…")
        asm_btn.clicked.connect(self._export_asm)
        canvas_btn = QPushButton("→ Paint canvas")
        canvas_btn.clicked.connect(self._stamp_to_canvas)
        canvas_btn.setToolTip("Stamp current character onto paint canvas at top-left")

        for b in (load_btn,save_btn,c_btn,asm_btn,canvas_btn):
            right.addWidget(b)

        right.addSpacing(12)
        right.addWidget(QLabel("Preview:"))
        self._preview = QLabel()
        self._preview.setFixedSize(64,64)
        self._preview.setStyleSheet("background:palette(shadow); border:1px solid palette(mid);")
        right.addWidget(self._preview)
        right.addStretch()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.close)
        right.addWidget(close_btn)
        lay.addLayout(right)


    def _populate_font_list(self): #vers 1
        """Populate font list with system fonts."""
        from PyQt6.QtGui import QFontDatabase
        self._all_fonts = sorted(QFontDatabase.families())
        self._font_list.clear()
        for f in self._all_fonts:
            self._font_list.addItem(f)

    def _filter_fonts(self, text): #vers 1
        text = text.lower()
        self._font_list.clear()
        for f in self._all_fonts:
            if text in f.lower():
                self._font_list.addItem(f)

    def _load_system_font(self, item): #vers 1
        """Render glyphs from selected system font into character grid."""
        from PyQt6.QtGui import QFont as _QFont, QImage as _QImg, QPainter as _QPainter
        font_name = item.text()
        try:
            font = _QFont(font_name, self._char_h - 1)
            font.setStyleHint(_QFont.StyleHint.Monospace)
            for ci in range(self._n_chars):
                ch_str = chr(ci) if 32 <= ci < 127 else ' '
                img = _QImg(self._char_w, self._char_h, _QImg.Format.Format_Mono)
                img.fill(0)
                p = _QPainter(img)
                p.setFont(font)
                p.drawText(0, self._char_h - 2, ch_str)
                p.end()
                row_data = []
                for row in range(self._char_h):
                    byte = 0
                    for col in range(self._char_w):
                        if img.pixel(col, row) & 0xFFFFFF:
                            byte |= (0x80 >> col)
                    row_data.append(byte)
                self._chars[ci] = row_data
            self._refresh_char_list()
            self._on_char_select(self._current)
            self.setWindowTitle(f"Font Editor — {font_name}")
        except Exception as e:
            print(f"[_load_system_font] {e}")

    def _on_size_change(self, txt): #vers 1
        self._char_h = 16 if txt == "8×16" else 8
        self._chars = [[0]*self._char_h for _ in range(self._n_chars)]
        self._grid_widget.set_size(self._char_w, self._char_h)
        self._refresh_char_list()
        self._on_char_select(0)


    def _on_char_select(self, idx): #vers 1
        if 0 <= idx < self._n_chars:
            self._current = idx
            self._grid_widget.set_data(self._chars[idx])
            self._refresh_hex()
            self._refresh_preview()

    def _on_bit_toggle(self, row, col, val): #vers 1
        ch = self._chars[self._current]
        if val: ch[row] |=  (0x80 >> col)
        else:   ch[row] &= ~(0x80 >> col)
        self._refresh_hex()
        self._refresh_preview()


    def _refresh_grid(self): #vers 1
        self._grid_widget.set_size(self._char_w, self._char_h)
        self._grid_widget.set_data(self._chars[self._current])


    def _refresh_char_list(self): #vers 1
        self._char_list.clear()
        for i in range(self._n_chars):
            ch = chr(i) if 32 <= i < 127 else f"[{i}]"
            self._char_list.addItem(f"{i:3d} {ch}")
        self._char_list.setCurrentRow(self._current)


    def _refresh_hex(self): #vers 1
        ch = self._chars[self._current]
        self._hex_label.setText("Hex: " + " ".join(f"{b:02X}" for b in ch))


    def _refresh_preview(self): #vers 1
        from PIL import Image
        ch = self._chars[self._current]
        img = Image.new('RGB', (self._char_w, self._char_h), (0,0,0))
        px = img.load()
        for row, byte in enumerate(ch):
            for col in range(self._char_w):
                if byte & (0x80 >> col):
                    px[col, row] = (0, 255, 0)
        img = img.resize((64, 64), Image.NEAREST)
        from PyQt6.QtGui import QPixmap
        import io
        buf = io.BytesIO(); img.save(buf, 'PNG'); buf.seek(0)
        pm = QPixmap(); pm.loadFromData(buf.read())
        self._preview.setPixmap(pm)


    def _clear_char(self): #vers 1
        self._chars[self._current] = [0]*self._char_h
        self._grid_widget.set_data(self._chars[self._current])
        self._refresh_hex(); self._refresh_preview()


    def _invert_char(self): #vers 1
        ch = self._chars[self._current]
        mask = (1<<self._char_w)-1
        self._chars[self._current] = [((~b) & 0xFF) for b in ch]
        self._grid_widget.set_data(self._chars[self._current])
        self._refresh_hex(); self._refresh_preview()


    def _shift_l(self): #vers 1
        ch = self._chars[self._current]
        self._chars[self._current] = [((b<<1)&0xFF) for b in ch]
        self._grid_widget.set_data(self._chars[self._current]); self._refresh_hex()


    def _shift_r(self): #vers 1
        ch = self._chars[self._current]
        self._chars[self._current] = [(b>>1) for b in ch]
        self._grid_widget.set_data(self._chars[self._current]); self._refresh_hex()


    def _shift_u(self): #vers 1
        ch = self._chars[self._current]
        self._chars[self._current] = ch[1:] + [0]
        self._grid_widget.set_data(self._chars[self._current]); self._refresh_hex()


    def _shift_d(self): #vers 1
        ch = self._chars[self._current]
        self._chars[self._current] = [0] + ch[:-1]
        self._grid_widget.set_data(self._chars[self._current]); self._refresh_hex()


    def _load_binary(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(self,"Load charset","","Binary (*.bin *.chr *.fnt);;All Files (*)")
        if not path: return
        data = open(path,'rb').read()
        char_size = self._char_h
        n = min(self._n_chars, len(data)//char_size)
        for i in range(n):
            self._chars[i] = list(data[i*char_size:(i+1)*char_size])
        self._on_char_select(self._current)


    def _save_binary(self): #vers 1
        path, _ = QFileDialog.getSaveFileName(self,"Save charset","charset.bin","Binary (*.bin);;All (*)")
        if not path: return
        data = bytearray()
        for ch in self._chars: data += bytearray(ch)
        open(path,'wb').write(data)


    def _export_c(self): #vers 1
        path, _ = QFileDialog.getSaveFileName(self,"Export C header","charset.h","C Header (*.h)")
        if not path: return
        lines = [f"/* {self._char_w}×{self._char_h} character set — {self._n_chars} chars */",
                 f"const uint8_t charset[{self._n_chars}][{self._char_h}] = {{"]
        for i,ch in enumerate(self._chars):
            hex_row = ", ".join(f"0x{b:02X}" for b in ch)
            comment = chr(i) if 32<=i<127 else f"#{i}"
            lines.append(f"    {{ {hex_row} }},  /* {comment} */")
        lines.append("};")
        open(path,'w').write('\n'.join(lines)+'\n')


    def _export_asm(self): #vers 1
        path, _ = QFileDialog.getSaveFileName(self,"Export ASM","charset.asm","ASM (*.asm *.s)")
        if not path: return
        lines = [f"; {self._char_w}×{self._char_h} charset — {self._n_chars} chars","charset:"]
        for i,ch in enumerate(self._chars):
            comment = chr(i) if 32<=i<127 else f"#{i}"
            hex_row = ",".join(f"${b:02X}" for b in ch)
            lines.append(f"    defb {hex_row}  ; {comment}")
        open(path,'w').write('\n'.join(lines)+'\n')


    def _stamp_to_canvas(self): #vers 1
        """Stamp current character onto the parent paint canvas."""
        p = self.parent()
        if not p or not hasattr(p,'dp5_canvas') or not p.dp5_canvas: return
        ch = self._chars[self._current]
        p._push_undo()
        fg = p.dp5_canvas.color
        r,g,b = fg.red(), fg.green(), fg.blue()
        for row, byte in enumerate(ch):
            for col in range(self._char_w):
                if byte & (0x80>>col):
                    if row < p._canvas_height and col < p._canvas_width:
                        i = (row*p._canvas_width+col)*4
                        p.dp5_canvas.rgba[i:i+4] = [r,g,b,255]
        p.dp5_canvas.update()
        p._set_status(f"Stamped char {self._current} to canvas")


class _CharGrid(QWidget):
    """Clickable bit grid for character editor."""
    bit_toggled = pyqtSignal(int, int, bool)
    CELL = 24


    def _get_ui_color(self, key): #vers 1
        """Return theme QColor via palette fallback."""
        from PyQt6.QtGui import QColor
        pal = self.palette()
        _map = {
            'viewport_bg':   pal.ColorRole.Base,
            'viewport_text': pal.ColorRole.PlaceholderText,
            'border':        pal.ColorRole.Mid,
            'bg_primary':    pal.ColorRole.Window,
            'bg_secondary':  pal.ColorRole.AlternateBase,
            'text_primary':  pal.ColorRole.WindowText,
            'accent_primary':pal.ColorRole.Highlight,
        }
        return pal.color(_map.get(key, pal.ColorRole.WindowText))

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._w = 8; self._h = 8
        self._data = [0]*8
        self._drawing = False; self._draw_val = True

    def set_size(self, w, h): #vers 1
        self._w = w; self._h = h
        self._data = [0]*h
        self.setFixedSize(w*self.CELL+1, h*self.CELL+1)
        self.update()

    def set_data(self, data): #vers 1
        self._data = list(data)
        self.update()

    def paintEvent(self, _): #vers 1
        p = QPainter(self)
        for row in range(self._h):
            for col in range(self._w):
                x = col*self.CELL; y = row*self.CELL
                bit = bool(self._data[row] & (0x80>>col)) if row<len(self._data) else False
                p.fillRect(x, y, self.CELL-1, self.CELL-1,
                           self._get_ui_color("accent_primary") if bit else self._get_ui_color("bg_secondary"))
                p.setPen(QPen(self._get_ui_color('bg_secondary')))
                p.drawRect(x, y, self.CELL-1, self.CELL-1)


    def _cell(self, pos): #vers 1
        return pos.x()//self.CELL, pos.y()//self.CELL


    def mousePressEvent(self, e): #vers 1
        col, row = self._cell(e.position().toPoint())
        if 0<=col<self._w and 0<=row<self._h:
            bit = bool(self._data[row] & (0x80>>col))
            self._draw_val = not bit
            self._drawing = True
            self.bit_toggled.emit(row, col, self._draw_val)
            self.update()


    def mouseMoveEvent(self, e): #vers 1
        if not self._drawing: return
        col, row = self._cell(e.position().toPoint())
        if 0<=col<self._w and 0<=row<self._h:
            cur = bool(self._data[row] & (0x80>>col))
            if cur != self._draw_val:
                self.bit_toggled.emit(row, col, self._draw_val)
                self.update()


    def mouseReleaseEvent(self, e): #vers 1
        self._drawing = False


#  Sprite Editor

class _SpriteEditor(_DockablePanelMixin, QWidget):
    """View and edit sprites with platform native size constraints.
    Shows the canvas sliced into sprite-sized frames.
    Floating Tool window — stays on top of DP5 canvas.
    """

    def __init__(self, parent=None): #vers 2
        super().__init__(parent, Qt.WindowType.Tool |
                         Qt.WindowType.WindowStaysOnTopHint)
        self.setWindowTitle("Sprite Editor")
        self.setMinimumSize(640, 480)
        self._editor = parent
        self._sprite_w = 16; self._sprite_h = 16
        self._current_frame = 0
        self._zoom = 4
        self._init_dock(parent, 'sprite_editor_docked', 'left')
        self._build_ui()
        self._refresh_frames()

    def _build_ui(self): #vers 1
        lay = QHBoxLayout(self)

        # - Left: frame list
        left = QVBoxLayout()
        left.addWidget(QLabel("Frames:"))
        self._frame_list = QListWidget()
        self._frame_list.setFixedWidth(80)
        self._frame_list.currentRowChanged.connect(self._on_frame_select)
        left.addWidget(self._frame_list)
        lay.addLayout(left)

        # - Centre: sprite view
        centre = QVBoxLayout()
        ctrl = QHBoxLayout()

        self._add_dock_button(ctrl)
        ctrl.addWidget(QLabel("Sprite size:"))
        sizes = ["8×8","8×16","16×16","16×32","32×32","32×64","64×64"]
        self._size_combo = QComboBox()
        self._size_combo.addItems(sizes)
        self._size_combo.setCurrentText("16×16")
        self._size_combo.currentTextChanged.connect(self._on_size_change)
        ctrl.addWidget(self._size_combo)
        ctrl.addWidget(QLabel("Zoom:"))
        self._zoom_spin = QSpinBox()
        self._zoom_spin.setRange(1,16); self._zoom_spin.setValue(4)
        self._zoom_spin.valueChanged.connect(self._on_zoom)
        ctrl.addWidget(self._zoom_spin)
        ctrl.addStretch()
        export_btn = QPushButton("Export sheet…")
        export_btn.clicked.connect(self._export_sheet)
        ctrl.addWidget(export_btn)
        centre.addLayout(ctrl)

        self._sprite_view = _SpriteView(self)
        centre.addWidget(self._sprite_view, 1)

        # Frame info
        self._info_lbl = QLabel("Frame 0  —  0,0")
        self._info_lbl.setFont(QFont("Courier",9))
        centre.addWidget(self._info_lbl)
        lay.addLayout(centre, 1)


        # - Right: platform presets
        right = QVBoxLayout()
        right.addWidget(QLabel("Platform:"))
        presets = [
            ("ZX Spectrum  16×16", 16,16),
            ("C64 sprite   24×21", 24,21),
            ("Amiga OCS    16×16", 16,16),
            ("Amiga AGA    32×32", 32,32),
            ("NES tile      8×8",   8, 8),
            ("SNES tile     8×8",   8, 8),
            ("Game Boy      8×8",   8, 8),
            ("Mega Drive   32×32", 32,32),
            ("PC Engine    16×16", 16,16),
        ]
        for lbl, w, h in presets:
            b = QPushButton(lbl)
            b.clicked.connect(lambda _,sw=w,sh=h: self._set_sprite_size(sw,sh))
            right.addWidget(b)
        right.addStretch()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.close)
        right.addWidget(close_btn)
        lay.addLayout(right)


    def _populate_font_list(self): #vers 1
        """Populate font list with system fonts."""
        from PyQt6.QtGui import QFontDatabase
        self._all_fonts = sorted(QFontDatabase.families())
        self._font_list.clear()
        for f in self._all_fonts:
            self._font_list.addItem(f)

    def _filter_fonts(self, text): #vers 1
        text = text.lower()
        self._font_list.clear()
        for f in self._all_fonts:
            if text in f.lower():
                self._font_list.addItem(f)

    def _load_system_font(self, item): #vers 1
        """Render glyphs from selected system font into character grid."""
        from PyQt6.QtGui import QFont as _QFont, QImage as _QImg, QPainter as _QPainter
        font_name = item.text()
        try:
            font = _QFont(font_name, self._char_h - 1)
            font.setStyleHint(_QFont.StyleHint.Monospace)
            for ci in range(self._n_chars):
                ch_str = chr(ci) if 32 <= ci < 127 else ' '
                img = _QImg(self._char_w, self._char_h, _QImg.Format.Format_Mono)
                img.fill(0)
                p = _QPainter(img)
                p.setFont(font)
                p.drawText(0, self._char_h - 2, ch_str)
                p.end()
                row_data = []
                for row in range(self._char_h):
                    byte = 0
                    for col in range(self._char_w):
                        if img.pixel(col, row) & 0xFFFFFF:
                            byte |= (0x80 >> col)
                    row_data.append(byte)
                self._chars[ci] = row_data
            self._refresh_char_list()
            self._on_char_select(self._current)
            self.setWindowTitle(f"Font Editor — {font_name}")
        except Exception as e:
            print(f"[_load_system_font] {e}")

    def _on_size_change(self, txt): #vers 1
        w,h = map(int, txt.split("×"))
        self._set_sprite_size(w, h)


    def _set_sprite_size(self, w, h): #vers 1
        self._sprite_w = w; self._sprite_h = h
        self._size_combo.setCurrentText(f"{w}×{h}" if f"{w}×{h}" in
            [self._size_combo.itemText(i) for i in range(self._size_combo.count())] else
            self._size_combo.currentText())
        self._refresh_frames()


    def _on_zoom(self, z): #vers 1
        self._zoom = z
        self._sprite_view.set_zoom(z)


    def _refresh_frames(self): #vers 1
        ed = self._editor
        if not ed or not hasattr(ed,'dp5_canvas') or not ed.dp5_canvas: return
        cw = ed._canvas_width; ch = ed._canvas_height
        cols = max(1, cw // self._sprite_w)
        rows = max(1, ch // self._sprite_h)
        self._frame_list.clear()
        self._frames = []
        n = 0
        for ry in range(rows):
            for rx in range(cols):
                self._frame_list.addItem(f"#{n:03d}")
                self._frames.append((rx*self._sprite_w, ry*self._sprite_h))
                n += 1
        if n > 0:
            self._frame_list.setCurrentRow(0)
            self._on_frame_select(0)


    def _on_frame_select(self, idx): #vers 1
        if idx < 0 or idx >= len(self._frames): return
        self._current_frame = idx
        ox, oy = self._frames[idx]
        ed = self._editor
        if not ed or not ed.dp5_canvas: return
        # Extract sprite RGBA
        cw = ed._canvas_width
        sw = self._sprite_w; sh = self._sprite_h
        sprite_rgba = bytearray(sw*sh*4)
        for row in range(sh):
            for col in range(sw):
                sx = ox+col; sy = oy+row
                if sx < cw and sy < ed._canvas_height:
                    si = (sy*cw+sx)*4
                    di = (row*sw+col)*4
                    sprite_rgba[di:di+4] = ed.dp5_canvas.rgba[si:si+4]
        self._sprite_view.set_sprite(sprite_rgba, sw, sh, self._zoom)
        self._info_lbl.setText(
            f"Frame {idx}  —  {ox},{oy}  ({sw}×{sh}px)")


    def _export_sheet(self): #vers 1
        ed = self._editor
        if not ed or not ed.dp5_canvas: return
        path, _ = QFileDialog.getSaveFileName(self,"Export Sprite Sheet",
            "sprites.png","PNG (*.png)")
        if not path: return
        from PIL import Image
        img = Image.frombytes('RGBA',(ed._canvas_width,ed._canvas_height),
                              bytes(ed.dp5_canvas.rgba))
        sw = self._sprite_w; sh = self._sprite_h
        n = len(self._frames)
        cols_out = min(16, n); rows_out = (n+cols_out-1)//cols_out
        out = Image.new('RGBA',(cols_out*sw, rows_out*sh),(0,0,0,0))
        for i,(ox,oy) in enumerate(self._frames):
            frame = img.crop((ox,oy,ox+sw,oy+sh))
            dx = (i%cols_out)*sw; dy = (i//cols_out)*sh
            out.paste(frame,(dx,dy))
        out.save(path)
        QMessageBox.information(self,"Export","Exported {n} sprites to {path}")



class _IconEditor(QWidget): #vers 1
    """
    Floating icon editor panel — load, edit, export icons in multiple formats.

    Left panel: file browser (load .info/.ico/.icns/.iff/.png/.svg)
    Centre:     format toggles + alpha colour picker
    Right:      export options + batch convert shortcut
    Bottom:     status + progress
    Loads selected icon into DP5 canvas for pixel editing.
    Exports via iff_ilbm + ico_handler — full format support.
    """

    FORMATS_IN  = [".info",".ico",".icns",".iff",".lbm",".ilbm",
                   ".png",".bmp",".tga",".gif",".webp",".svg"]
    FORMATS_OUT = ["PNG","IFF ILBM (indexed)","IFF ILBM (24-bit)",
                   "HAM6 IFF","HAM8 IFF","ICO (Windows)","ICNS (Apple)",
                   "Amiga .info (AGA WB)","Amiga .info (MagicWB)",
                   "Amiga .info (OCS)","BMP","TGA"]

    def __init__(self, parent=None): #vers 2
        super().__init__(parent, Qt.WindowType.Tool |
                         Qt.WindowType.WindowStaysOnTopHint)
        self._editor   = parent
        self._variants = []
        self._current  = 0
        self._src_path = None
        self._docked   = False   # True = snapped overlay, False = floating
        self._overlay_widget = None

        # Load saved settings
        self._settings = None
        if parent and hasattr(parent, 'dp5_settings'):
            self._settings = parent.dp5_settings

        def _s(key, default=None):
            return self._settings.get(key, default) if self._settings else default

        self._alpha_color = (
            _s('icon_editor_alpha_r', 0),
            _s('icon_editor_alpha_g', 0),
            _s('icon_editor_alpha_b', 0),
        )
        self._docked = _s('icon_editor_docked', False)
        self.setWindowTitle("Icon Editor")
        self.resize(480, 560)
        self._build_ui()

        # Restore choices
        fmt = _s('icon_editor_out_fmt', 'PNG')
        idx = self._out_fmt.findText(fmt)
        if idx >= 0:
            self._out_fmt.setCurrentIndex(idx)

        alpha = _s('icon_editor_alpha', True)
        self._alpha_chk.setChecked(alpha)

        pal = _s('icon_editor_amiga_pal', 'AGA Workbench (WB3.9)')
        idx = self._amiga_pal_combo.findText(pal)
        if idx >= 0:
            self._amiga_pal_combo.setCurrentIndex(idx)

        self._refresh_alpha_swatch()

        # Restore position
        x = _s('icon_editor_x', -1)
        y = _s('icon_editor_y', -1)
        if x >= 0 and y >= 0:
            self.move(x, y)

        if self._docked:
            QTimer.singleShot(100, self._snap_to_overlay)

    def _build_ui(self): #vers 1
        from PyQt6.QtWidgets import (QVBoxLayout, QHBoxLayout, QLabel,
            QListWidget, QComboBox, QPushButton, QCheckBox, QLineEdit,
            QProgressBar, QFrame, QFileDialog, QGroupBox, QColorDialog,
            QScrollArea, QSpinBox, QTextEdit, QFormLayout)

        root = QVBoxLayout(self)
        root.setContentsMargins(4,4,4,4); root.setSpacing(4)

        # ── Title bar with [D] dock toggle ───────────────────────────────────
        title_row = QHBoxLayout()
        title_lbl = QLabel("Icon Editor")
        title_lbl.setFont(QFont("Arial", 9, QFont.Weight.Bold))
        self._dock_btn = QPushButton("D")
        self._dock_btn.setFixedSize(22, 22)
        self._dock_btn.setFlat(True)
        self._dock_btn.setToolTip("Toggle dock: snap to canvas overlay / float")
        self._dock_btn.setCheckable(True)
        self._dock_btn.setChecked(self._docked)
        self._dock_btn.clicked.connect(self._toggle_dock)
        title_row.addWidget(title_lbl, 1)
        title_row.addWidget(self._dock_btn)
        root.addLayout(title_row)

        # ── Load row ─────────────────────────────────────────────────────────
        load_row = QHBoxLayout()
        self._path_edit = QLineEdit()
        self._path_edit.setPlaceholderText("Icon file path…")
        self._path_edit.setReadOnly(True)
        browse_btn = QPushButton("Open…")
        browse_btn.clicked.connect(self._browse_open)
        load_row.addWidget(QLabel("File:"))
        load_row.addWidget(self._path_edit, 1)
        load_row.addWidget(browse_btn)
        root.addLayout(load_row)

        # ── Variants list (sizes/depths found in file) ────────────────────────
        root.addWidget(QLabel("Variants in file:"))
        self._variants_list = QListWidget()
        self._variants_list.setMaximumHeight(90)
        self._variants_list.currentRowChanged.connect(self._on_variant_select)
        root.addWidget(self._variants_list)

        # ── Open in canvas button ─────────────────────────────────────────────
        open_btn = QPushButton("▶  Open Selected in Canvas")
        open_btn.clicked.connect(self._open_in_canvas)
        f = open_btn.font(); f.setBold(True); open_btn.setFont(f)
        root.addWidget(open_btn)

        line = QFrame(); line.setFrameShape(QFrame.Shape.HLine)
        root.addWidget(line)

        # ── Alpha colour ──────────────────────────────────────────────────────
        alpha_row = QHBoxLayout()
        self._alpha_chk = QCheckBox("Colour 0 = alpha")
        self._alpha_chk.setChecked(True)
        self._alpha_chk.setToolTip(
            "Treat palette colour 0 as transparent on export\n"
            "(Amiga .info default)")
        self._alpha_swatch = QPushButton()
        self._alpha_swatch.setFixedSize(24, 24)
        self._alpha_swatch.setStyleSheet("background:#000000; border:1px solid palette(mid);")
        self._alpha_swatch.setToolTip("Click to pick alpha colour")
        self._alpha_swatch.clicked.connect(self._pick_alpha)
        alpha_row.addWidget(self._alpha_chk)
        alpha_row.addWidget(QLabel("Colour:"))
        alpha_row.addWidget(self._alpha_swatch)
        alpha_row.addStretch()
        root.addLayout(alpha_row)

        # ── Export format ─────────────────────────────────────────────────────
        fmt_row = QHBoxLayout()
        self._out_fmt = QComboBox()
        self._out_fmt.addItems(self.FORMATS_OUT)
        fmt_row.addWidget(QLabel("Export as:"))
        fmt_row.addWidget(self._out_fmt, 1)
        root.addLayout(fmt_row)

        # Amiga palette preset (shown when Amiga format selected)
        self._amiga_pal_combo = QComboBox()
        self._amiga_pal_combo.addItems([
            "AGA Workbench (WB3.9)","AGA Workbench XL",
            "MagicWB","OCS Workbench","User palette"])
        self._amiga_pal_row = QHBoxLayout()
        self._amiga_pal_row.addWidget(QLabel("Amiga palette:"))
        self._amiga_pal_row.addWidget(self._amiga_pal_combo, 1)
        root.addLayout(self._amiga_pal_row)
        self._out_fmt.currentTextChanged.connect(self._on_format_changed)
        self._out_fmt.currentTextChanged.connect(lambda _: self._save_settings())
        self._alpha_chk.stateChanged.connect(lambda _: self._save_settings())
        self._on_format_changed(self._out_fmt.currentText())

        # ── Export single ─────────────────────────────────────────────────────
        exp_row = QHBoxLayout()
        exp_single = QPushButton("Export…")
        exp_single.clicked.connect(self._export_single)
        exp_all = QPushButton("All Variants…")
        exp_all.clicked.connect(self._export_all)
        exp_row.addWidget(exp_single); exp_row.addWidget(exp_all)
        root.addLayout(exp_row)

        # ── Batch convert (quick launch) ──────────────────────────────────────
        line2 = QFrame(); line2.setFrameShape(QFrame.Shape.HLine)
        root.addWidget(line2)
        batch_btn = QPushButton("Batch Convert…")
        batch_btn.clicked.connect(
            lambda: self._editor._batch_convert_icons()
            if self._editor else None)
        batch_btn.setToolTip("Opens batch converter overlay on the canvas")
        root.addWidget(batch_btn)

        # ── Status ────────────────────────────────────────────────────────────
        self._status = QLabel("")
        self._status.setFont(QFont("Arial", 8))
        self._status.setWordWrap(True)
        root.addWidget(self._status)

    # ── Format visibility ─────────────────────────────────────────────────────

    def _on_format_changed(self, fmt): #vers 1
        amiga = "Amiga" in fmt or ".info" in fmt
        self._amiga_pal_combo.setEnabled(amiga)
        for i in range(self._amiga_pal_row.count()):
            w = self._amiga_pal_row.itemAt(i).widget()
            if w:
                w.setVisible(amiga)

    # ── Load ──────────────────────────────────────────────────────────────────

    def _browse_open(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Open Icon",
            "",
            "Icons (*.info *.ico *.icns *.iff *.lbm *.ilbm *.png *.bmp "
            "*.tga *.gif *.webp *.svg);;All Files (*)")
        if path:
            self._load_file(path)

    def _load_file(self, path: str): #vers 1
        """Auto-detect format and load all variants."""
        import os
        self._src_path = path
        self._path_edit.setText(path)
        self._variants.clear()
        self._variants_list.clear()

        ext = os.path.splitext(path)[1].lower()
        try:
            data = open(path, 'rb').read()

            if ext == '.ico':
                from apps.methods.ico_handler import read_ico
                self._variants = read_ico(data)

            elif ext == '.icns':
                from apps.methods.ico_handler import read_icns
                self._variants = read_icns(data)

            elif ext == '.info':
                if self._editor and hasattr(self._editor, '_decode_amiga_info'):
                    pal = self._amiga_pal_to_mode()
                    rgba, w, h, fmt = self._editor._decode_amiga_info(data, pal)
                    if rgba:
                        self._variants = [(w, h, bytes(rgba))]
                    else:
                        self._status.setText(f"Failed: {fmt}")
                        return

            elif ext in ('.iff','.lbm','.ilbm'):
                from apps.methods.iff_ilbm import read_iff_ilbm_rgba
                result = read_iff_ilbm_rgba(data,
                    alpha_index=0 if self._alpha_chk.isChecked() else None)
                if result:
                    w, h, rgba = result
                    self._variants = [(w, h, rgba)]

            elif ext == '.svg':
                self._load_svg(data, path)
                return

            else:
                # PIL/QImage fallback
                self._load_raster(data, path)
                return

            self._populate_variants()

        except Exception as e:
            self._status.setText(f"Load error: {e}")

    def _load_raster(self, data: bytes, path: str): #vers 1
        """Load a raster image via PIL or QImage."""
        try:
            from PIL import Image
            import io
            img = Image.open(io.BytesIO(data)).convert('RGBA')
            self._variants = [(img.width, img.height, img.tobytes())]
            self._populate_variants()
        except Exception:
            try:
                from PyQt6.QtGui import QImage
                from PyQt6.QtCore import QByteArray
                img = QImage()
                img.loadFromData(QByteArray(data))
                img = img.convertToFormat(QImage.Format.Format_RGBA8888)
                rgba = bytes(img.bits().asarray(img.width() * img.height() * 4))
                self._variants = [(img.width(), img.height(), rgba)]
                self._populate_variants()
            except Exception as e:
                self._status.setText(f"Load error: {e}")

    def _load_svg(self, data: bytes, path: str): #vers 1
        """Render SVG to canvas size RGBA."""
        try:
            from PyQt6.QtSvg import QSvgRenderer
            from PyQt6.QtGui import QPainter
            renderer = QSvgRenderer(data)
            sz = renderer.defaultSize()
            w = sz.width() or 32; h = sz.height() or 32
            pm = QPixmap(w, h)
            pm.fill(Qt.GlobalColor.transparent)
            p = QPainter(pm)
            renderer.render(p); p.end()
            img = pm.toImage().convertToFormat(QImage.Format.Format_RGBA8888)
            rgba = bytes(img.bits().asarray(w * h * 4))
            self._variants = [(w, h, rgba)]
            self._populate_variants()
        except Exception as e:
            self._status.setText(f"SVG error: {e}")

    def _populate_variants(self): #vers 1
        self._variants_list.clear()
        for i, (w, h, rgba) in enumerate(self._variants):
            self._variants_list.addItem(f"{w}×{h}  ({len(rgba)//4} px)")
        if self._variants:
            self._variants_list.setCurrentRow(0)
            self._status.setText(
                f"Loaded {len(self._variants)} variant(s) from "
                f"{os.path.basename(self._src_path)}")
        import os

    def _on_variant_select(self, idx): #vers 1
        if 0 <= idx < len(self._variants):
            self._current = idx

    # ── Alpha picker ──────────────────────────────────────────────────────────

    def _refresh_alpha_swatch(self): #vers 1
        r, g, b = self._alpha_color
        self._alpha_swatch.setStyleSheet(
            f"background:#{r:02x}{g:02x}{b:02x}; border:1px solid palette(mid);")

    def _pick_alpha(self): #vers 2
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        c = QColorDialog.getColor(QColor(*self._alpha_color), self, "Alpha Colour")
        if c.isValid():
            self._alpha_color = (c.red(), c.green(), c.blue())
            self._refresh_alpha_swatch()
            self._save_settings()

    def _save_settings(self): #vers 1
        """Persist icon editor choices to dp5_settings."""
        if not self._settings:
            return
        pos = self.pos()
        self._settings.set('icon_editor_docked',  self._docked)
        self._settings.set('icon_editor_x',        pos.x())
        self._settings.set('icon_editor_y',        pos.y())
        self._settings.set('icon_editor_out_fmt',  self._out_fmt.currentText())
        self._settings.set('icon_editor_alpha',    self._alpha_chk.isChecked())
        self._settings.set('icon_editor_alpha_r',  self._alpha_color[0])
        self._settings.set('icon_editor_alpha_g',  self._alpha_color[1])
        self._settings.set('icon_editor_alpha_b',  self._alpha_color[2])
        self._settings.set('icon_editor_amiga_pal',self._amiga_pal_combo.currentText())
        self._settings.save()

    def _toggle_dock(self): #vers 1
        """Toggle between floating Tool window and canvas-overlay snap."""
        self._docked = self._dock_btn.isChecked()
        if self._docked:
            self._snap_to_overlay()
        else:
            self._float_panel()
        self._save_settings()

    def _snap_to_overlay(self): #vers 2
        """Reparent into canvas viewport, snap to right edge."""
        if not self._editor or not hasattr(self._editor, '_canvas_scroll'):
            return
        vp = self._editor._canvas_scroll.viewport()
        self.setWindowFlags(Qt.WindowType.Widget)
        self.setParent(vp)
        self.setWindowOpacity(0.88)
        self._dock_btn.setChecked(True)
        self._docked = True
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(0, self._reposition_overlay)
        self.show(); self.raise_()

    def _reposition_overlay(self): #vers 2
        """Reposition icon editor to right edge of viewport."""
        if not self._editor or not hasattr(self._editor, '_canvas_scroll'):
            return
        vp = self._editor._canvas_scroll.viewport()
        vp_w = vp.width(); vp_h = vp.height()
        pw = min(240, vp_w // 3)
        ph = vp_h - 8
        self.setMinimumSize(0, 0)
        self.setMaximumSize(16777215, 16777215)
        self.move(vp_w - pw - 4, 4)
        self.resize(pw, ph)
        self.raise_()

    def _float_panel(self): #vers 2
        """Detach from viewport, return to floating Tool window."""
        try:
            pos = self.mapToGlobal(self.rect().topLeft())
        except Exception:
            pos = None
        self.setWindowOpacity(1.0)
        self.setParent(None)
        self.setWindowFlags(Qt.WindowType.Tool |
                            Qt.WindowType.WindowStaysOnTopHint)
        if pos:
            self.move(pos)
        self.resize(480, 560)
        self.show()
        self._dock_btn.setChecked(False)
        self._docked = False

    def closeEvent(self, event): #vers 1
        self._save_settings()
        super().closeEvent(event)


    # ── Open in canvas ────────────────────────────────────────────────────────

    def _open_in_canvas(self): #vers 1
        if not self._variants or not self._editor:
            return
        if not hasattr(self._editor, 'dp5_canvas') or not self._editor.dp5_canvas:
            self._status.setText("DP5 canvas not ready")
            return
        w, h, rgba = self._variants[self._current]
        try:
            ws = self._editor
            if hasattr(ws, '_push_undo'):
                ws._push_undo()
            ws.dp5_canvas.tex_w = w
            ws.dp5_canvas.tex_h = h
            ws.dp5_canvas.rgba  = bytearray(rgba)
            ws._canvas_width    = w
            ws._canvas_height   = h
            ws.dp5_canvas.update()
            if hasattr(ws, '_fit_canvas_to_viewport'):
                ws._fit_canvas_to_viewport()
            if hasattr(ws, '_set_status'):
                ws._set_status(f"Icon: {w}×{h} loaded for editing")
            self._status.setText(f"Opened {w}×{h} in canvas")
        except Exception as e:
            self._status.setText(f"Canvas error: {e}")

    # ── Export ────────────────────────────────────────────────────────────────

    def _amiga_pal_to_mode(self): #vers 1
        t = self._amiga_pal_combo.currentText()
        if 'XL' in t:   return 'wb39xl'
        if 'MagicWB' in t: return 'magicwb'
        if 'OCS' in t:  return 'ocs'
        if 'User' in t: return 'user'
        return 'wb39'

    def _get_export_rgba(self) -> tuple: #vers 1
        """Get RGBA from canvas if available, else from loaded variant."""
        ws = self._editor
        if ws and hasattr(ws, 'dp5_canvas') and ws.dp5_canvas:
            c = ws.dp5_canvas
            return c.tex_w, c.tex_h, bytes(c.rgba)
        if self._variants:
            return self._variants[self._current]
        return None, None, None

    def _export_rgba(self, rgba: bytes, w: int, h: int,
                     path: str, fmt: str): #vers 1
        """Write RGBA to path in the given format."""
        import os
        alpha_idx = 0 if self._alpha_chk.isChecked() else None
        alpha_col = self._alpha_color if self._alpha_chk.isChecked() else None

        if fmt == "PNG":
            from PyQt6.QtGui import QImage
            img = QImage(rgba, w, h, w*4, QImage.Format.Format_RGBA8888)
            img.save(path, 'PNG')

        elif fmt == "IFF ILBM (indexed)":
            from apps.methods.iff_ilbm import write_iff_ilbm_rgba
            data = write_iff_ilbm_rgba(rgba, w, h, n_planes=8,
                alpha_color=alpha_col, alpha_is_index0=self._alpha_chk.isChecked())
            open(path, 'wb').write(data)

        elif fmt == "IFF ILBM (24-bit)":
            from apps.methods.iff_ilbm import write_iff_24bit
            open(path, 'wb').write(write_iff_24bit(rgba, w, h))

        elif fmt == "HAM6 IFF":
            from apps.methods.iff_ilbm import write_iff_ham
            open(path, 'wb').write(write_iff_ham(rgba, w, h, ham8=False))

        elif fmt == "HAM8 IFF":
            from apps.methods.iff_ilbm import write_iff_ham
            open(path, 'wb').write(write_iff_ham(rgba, w, h, ham8=True))

        elif fmt == "ICO (Windows)":
            from apps.methods.ico_handler import write_ico
            open(path, 'wb').write(write_ico([(w, h, rgba)]))

        elif fmt == "ICNS (Apple)":
            from apps.methods.ico_handler import write_icns
            open(path, 'wb').write(write_icns([(w, h, rgba)]))

        elif "Amiga .info" in fmt:
            if self._editor and hasattr(self._editor, '_encode_amiga_info'):
                pal_mode = self._amiga_pal_to_mode()
                data = self._editor._encode_amiga_info(
                    bytearray(rgba), w, h, pal_mode)
                open(path, 'wb').write(data)
            else:
                raise ValueError("_encode_amiga_info not available")

        elif fmt == "BMP":
            from PyQt6.QtGui import QImage
            img = QImage(rgba, w, h, w*4, QImage.Format.Format_RGBA8888)
            img.save(path, 'BMP')

        elif fmt == "TGA":
            from PIL import Image
            Image.frombytes('RGBA', (w, h), rgba).save(path, 'TGA')

    def _fmt_ext(self, fmt: str) -> str: #vers 1
        exts = {
            "PNG":".png","IFF ILBM (indexed)":".iff","IFF ILBM (24-bit)":".iff",
            "HAM6 IFF":".iff","HAM8 IFF":".iff","ICO (Windows)":".ico",
            "ICNS (Apple)":".icns","BMP":".bmp","TGA":".tga",
        }
        for k,v in exts.items():
            if fmt.startswith(k): return v
        return ".info" if "info" in fmt else ".png"

    def _export_single(self): #vers 1
        w, h, rgba = self._get_export_rgba()
        if not rgba:
            self._status.setText("Nothing to export"); return
        fmt = self._out_fmt.currentText()
        ext = self._fmt_ext(fmt)
        path, _ = QFileDialog.getSaveFileName(
            self, f"Export {fmt}", f"icon{ext}", f"Files (*{ext})")
        if not path: return
        try:
            self._export_rgba(rgba, w, h, path, fmt)
            self._status.setText(f"✓ Exported: {os.path.basename(path)}")
        except Exception as e:
            self._status.setText(f"Export error: {e}")
        import os

    def _export_all(self): #vers 1
        """Export all loaded variants as a multi-size ICO or individual files."""
        if not self._variants:
            self._status.setText("No variants loaded"); return
        fmt = self._out_fmt.currentText()
        if fmt == "ICO (Windows)" and len(self._variants) > 1:
            path, _ = QFileDialog.getSaveFileName(
                self, "Export Multi-size ICO", "icon.ico", "ICO Files (*.ico)")
            if not path: return
            try:
                from apps.methods.ico_handler import write_ico
                open(path, 'wb').write(write_ico(self._variants))
                self._status.setText(
                    f"✓ Exported {len(self._variants)} sizes to ICO")
            except Exception as e:
                self._status.setText(f"Export error: {e}")
        else:
            folder = QFileDialog.getExistingDirectory(
                self, "Export folder for all variants")
            if not folder: return
            import os
            ext = self._fmt_ext(fmt)
            ok = 0
            for i, (w, h, rgba) in enumerate(self._variants):
                try:
                    p = os.path.join(folder, f"icon_{w}x{h}{ext}")
                    self._export_rgba(rgba, w, h, p, fmt)
                    ok += 1
                except Exception as e:
                    self._status.setText(f"Error on {w}×{h}: {e}")
            self._status.setText(f"✓ Exported {ok} variants to {folder}")


class _SpriteView(QWidget):
    """Zoomed sprite display widget."""
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._rgba = None; self._w = 16; self._h = 16; self._zoom = 4
        self.setMinimumSize(200,200)

    def set_sprite(self, rgba, w, h, zoom):
        self._rgba=rgba; self._w=w; self._h=h; self._zoom=zoom
        self.setFixedSize(w*zoom+2, h*zoom+2)
        self.update()

    def set_zoom(self, z): #vers 1
        self._zoom=z
        if self._rgba:
            self.setFixedSize(self._w*z+2, self._h*z+2)
        self.update()

    def paintEvent(self, _): #vers 1
        if not self._rgba: return
        p = QPainter(self)
        z = self._zoom; w = self._w; h = self._h
        for row in range(h):
            for col in range(w):
                i = (row*w+col)*4
                r,g,b,a = self._rgba[i:i+4]
                if a > 0:
                    p.fillRect(col*z, row*z, z, z, QColor(r,g,b,a))
                else:
                    # Checkerboard for transparent
                    shade = 160 if (row+col)%2==0 else 100
                    p.fillRect(col*z, row*z, z, z, QColor(shade,shade,shade))
        # Grid
        p.setPen(QPen(QColor(60,60,60,180),1))
        for x in range(0, w*z+1, z): p.drawLine(x,0,x,h*z)
        for y in range(0, h*z+1, z): p.drawLine(0,y,w*z,y)


#  Standalone entry point
if __name__ == "__main__": #vers 1
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
            app_icon = QIcon()
            for sz in (16, 32, 48, 64, 128):
                ico = get_dp5_workshop_icon(sz)
                app_icon.addPixmap(ico.pixmap(sz, sz))
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
