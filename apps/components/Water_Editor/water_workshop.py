#!/usr/bin/env python3
# apps/components/Water_Editor/water_workshop.py - Version: 1
# X-Seti - Apr 2026 - IMG Factory 1.6 - Water Workshop
# Supports: GTA III / VC / PS2 LC / SOL — waterpro.dat + water.dat
#           GTA SA — water.dat / water1.dat (quad format)

import struct, re, math, json
from pathlib import Path
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QLabel, QToolButton,
    QPushButton, QFrame, QSizePolicy, QListWidget, QListWidgetItem,
    QFileDialog, QMessageBox, QTabWidget, QAbstractItemView, QProgressDialog,
    QDialog, QApplication, QLineEdit, QDoubleSpinBox, QFormLayout,
    QDialogButtonBox, QScrollArea, QComboBox, QCheckBox, QMenu, QSpinBox,
    QGroupBox, QRadioButton, QButtonGroup
)
from PyQt6.QtGui import (
    QColor, QImage, QPixmap, QPainter, QPen, QFont, QIcon, QKeySequence,
    QShortcut
)
from PyQt6.QtCore import Qt, QSize, QPoint, pyqtSignal

try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
except ImportError:
    class SVGIconFactory:
        @staticmethod
        def _stub(size=20, color=None): return QIcon()
        open_icon = save_icon = export_icon = import_icon = delete_icon = \
        undo_icon = info_icon = paint_icon = fill_icon = dropper_icon = \
        zoom_in_icon = zoom_out_icon = fit_grid_icon = locate_icon = \
        search_icon = rotate_cw_icon = rotate_ccw_icon = flip_horz_icon = \
        flip_vert_icon = line_icon = rect_icon = rect_fill_icon = \
        scissors_icon = paste_brush_icon = staticmethod(_stub)

try:
    from apps.utils.app_settings_system import AppSettings
except ImportError:
    AppSettings = None

App_name  = "Water Workshop"
App_build = "Apr 2026"
Build     = "Build 1"

# ── World coordinate ranges ───────────────────────────────────────────────────
# III/VC/PS2: world ±2048 mapped to 64×64 grid  (32m per cell)
# SOL:        world ±6144 mapped to 384×384 grid (32m per cell)
# SA:         world ±3000 approx, quad-based

CELL_SIZE = 32.0   # world units per grid cell (constant across all binary games)


# ─────────────────────────────────────────────────────────────────────────────
# Parsers
# ─────────────────────────────────────────────────────────────────────────────

class WaterproParser:
    """Read/write binary waterpro.dat (GTA III, VC, PS2 LC, SOL)."""

    HEADER_SIZE   = 964   # fixed header before grid data
    WATER_LEVELS  = 48    # max water level float entries

    def __init__(self):
        self.water_levels_count = 0
        self.water_level_data   = [0.0] * self.WATER_LEVELS  # float heights
        self.unk_block          = bytes(768)  # offset 196-963, preserve
        self.grid_w             = 0           # physical grid width
        self.phys_grid          = bytearray() # GW*GW bytes, 0 or 128
        self.vis_grid           = bytearray() # (2GW)*(2GW) bytes, 0 or 128
        self.path               = None

    def load(self, path: str):
        data = Path(path).read_bytes()
        size = len(data)
        if size < self.HEADER_SIZE + 5:
            raise ValueError(f"File too small: {size} bytes")

        # Verify grid size
        remainder = size - self.HEADER_SIZE
        if remainder % 5 != 0:
            raise ValueError(f"Invalid size {size}: (size-964) must be divisible by 5")

        gw_sq = remainder // 5
        gw    = int(math.isqrt(gw_sq))
        if gw * gw != gw_sq:
            raise ValueError(f"Grid area {gw_sq} is not a perfect square")

        self.grid_w             = gw
        self.water_levels_count = data[0]
        self.water_level_data   = list(struct.unpack_from(f'<{self.WATER_LEVELS}f', data, 4))
        self.unk_block          = data[196:964]
        self.phys_grid          = bytearray(data[964 : 964 + gw*gw])
        self.vis_grid           = bytearray(data[964 + gw*gw : 964 + gw*gw + (2*gw)*(2*gw)])
        self.path               = path

    def save(self, path: str):
        gw = self.grid_w
        out = bytearray(self.HEADER_SIZE + gw*gw + (2*gw)*(2*gw))
        out[0] = self.water_levels_count & 0xFF
        struct.pack_into(f'<{self.WATER_LEVELS}f', out, 4, *self.water_level_data)
        out[196:964] = self.unk_block
        out[964 : 964+gw*gw] = self.phys_grid
        out[964+gw*gw : 964+gw*gw+(2*gw)*(2*gw)] = self.vis_grid
        Path(path).write_bytes(bytes(out))

    def cell_to_world(self, cx: int, cy: int) -> tuple:
        """Convert grid cell to world (x, y) centre coordinate."""
        half = self.grid_w * CELL_SIZE / 2.0
        wx = cx * CELL_SIZE - half + CELL_SIZE / 2
        wy = cy * CELL_SIZE - half + CELL_SIZE / 2
        return wx, wy

    def world_to_cell(self, wx: float, wy: float) -> tuple:
        half = self.grid_w * CELL_SIZE / 2.0
        cx = int((wx + half) / CELL_SIZE)
        cy = int((wy + half) / CELL_SIZE)
        return max(0, min(self.grid_w-1, cx)), max(0, min(self.grid_w-1, cy))


class WaterDatParser:
    """Read/write text water.dat (III, VC, PS2 — rect format)."""

    def __init__(self):
        self.rects  = []   # list of (level, x1, y1, x2, y2)
        self.header = ""   # preserve comment header
        self.path   = None

    def load(self, path: str):
        text = Path(path).read_text(encoding='latin1', errors='replace')
        self.path   = path
        self.rects  = []
        header_lines = []
        in_header = True
        for line in text.splitlines():
            stripped = line.strip()
            if stripped.startswith('*'):
                break
            if not stripped or stripped.startswith(';'):
                if in_header:
                    header_lines.append(line)
                continue
            in_header = False
            parts = re.split(r'[\s,]+', stripped.rstrip(','))
            parts = [p for p in parts if p]
            if len(parts) >= 5:
                try:
                    self.rects.append(tuple(float(p) for p in parts[:5]))
                except ValueError:
                    pass
        self.header = '\n'.join(header_lines)

    def save(self, path: str):
        lines = [self.header, '']
        for r in self.rects:
            lines.append(f"{r[0]:.4f},\t{r[1]:.4f},\t{r[2]:.4f},\t{r[3]:.4f},\t{r[4]:.4f},")
        lines.append('* ;end of file')
        Path(path).write_text('\n'.join(lines), encoding='latin1')


class SaWaterParser:
    """Read/write SA water.dat / water1.dat (quad format)."""

    def __init__(self):
        self.quads  = []   # list of dicts with 4 corners + flag
        self.path   = None

    def load(self, path: str):
        text  = Path(path).read_text(encoding='latin1', errors='replace')
        self.path  = path
        self.quads = []
        for line in text.splitlines():
            s = line.strip()
            if not s or s.startswith(';') or s.startswith('*') or s == 'processed':
                continue
            parts = s.split()
            if len(parts) < 29:
                continue
            try:
                corners = []
                for c in range(4):
                    o = c * 7
                    corners.append({
                        'x': float(parts[o]),
                        'y': float(parts[o+1]),
                        'f': [float(parts[o+k]) for k in range(2, 7)]
                    })
                self.quads.append({'corners': corners, 'flag': int(parts[28])})
            except (ValueError, IndexError):
                pass

    def save(self, path: str):
        lines = ['processed']
        for q in self.quads:
            parts = []
            for c in q['corners']:
                parts.append(f"{c['x']:.4f} {c['y']:.4f} "
                              + ' '.join(f"{v:.5f}" for v in c['f']))
            lines.append('    '.join(parts) + f"  {q['flag']}")
        Path(path).write_text('\n'.join(lines), encoding='latin1')

    def world_bbox(self) -> tuple:
        """Return (x_min, y_min, x_max, y_max) of all quads."""
        if not self.quads: return -3000, -3000, 3000, 3000
        xs = [c['x'] for q in self.quads for c in q['corners']]
        ys = [c['y'] for q in self.quads for c in q['corners']]
        return min(xs), min(ys), max(xs), max(ys)


# ─────────────────────────────────────────────────────────────────────────────
# Grid canvas widget
# ─────────────────────────────────────────────────────────────────────────────

class WaterGridWidget(QWidget):
    """Displays a binary water grid. Supports zoom, pan, draw."""

    cell_clicked      = pyqtSignal(int, int)         # cx, cy
    cell_right_clicked = pyqtSignal(int, int, QPoint) # cx, cy, global pos
    color_picked      = pyqtSignal(bool, bool)        # is_water, is_left

    # Colours
    COL_WATER = QColor(30, 120, 220, 255)
    COL_DRY   = QColor(30,  30,  30, 255)
    COL_GRID  = QColor(60,  60,  80, 120)
    COL_SEL   = QColor(255, 200,  0, 200)
    COL_HOVER = QColor(255, 255, 255,  40)

    def __init__(self, parent=None):
        super().__init__(parent)
        self._grid_w      = 0
        self._grid        = bytearray()  # 0 or 128
        self._zoom        = 1.0
        self._pan_x       = 0
        self._pan_y       = 0
        self._sel_cx      = -1
        self._sel_cy      = -1
        self._hover_cx    = -1
        self._hover_cy    = -1
        self._drawing     = False
        self._draw_val    = 128           # 128=water, 0=dry
        self._line_start  = None
        self._preview_cells = None
        self._workshop    = None
        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.WheelFocus)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)

    def setup(self, grid_w: int, grid: bytearray):
        self._grid_w = grid_w
        self._grid   = grid
        self._sel_cx = self._sel_cy = -1
        self._zoom   = 1.0
        self._pan_x  = self._pan_y = 0
        self.update()

    def set_grid(self, grid: bytearray):
        self._grid = grid
        self.update()

    def _ts(self) -> int:
        if not self._grid_w: return 8
        base = min(self.width() // self._grid_w, self.height() // self._grid_w)
        return max(2, int(base * self._zoom))

    def _cell_at(self, pos) -> tuple:
        ts = self._ts()
        ax = pos.x() - self._pan_x
        ay = pos.y() - self._pan_y
        if ax < 0 or ay < 0: return -1, -1
        cx, cy = ax // ts, ay // ts
        if 0 <= cx < self._grid_w and 0 <= cy < self._grid_w:
            return cx, cy
        return -1, -1

    def _cell_val(self, cx: int, cy: int) -> int:
        if 0 <= cx < self._grid_w and 0 <= cy < self._grid_w:
            return self._grid[cy * self._grid_w + cx]
        return 0

    def paintEvent(self, ev):
        if not self._grid_w: return
        ts  = self._ts()
        gw  = self._grid_w
        px0 = self._pan_x
        py0 = self._pan_y
        p   = QPainter(self)

        for cy in range(gw):
            for cx in range(gw):
                x = px0 + cx * ts
                y = py0 + cy * ts
                val = self._grid[cy * gw + cx]

                # Preview overlay
                if self._preview_cells and (cx, cy) in self._preview_cells:
                    col = self.COL_WATER if self._draw_val == 128 else self.COL_DRY
                    p.fillRect(x, y, ts, ts, col.lighter(140))
                elif val == 128:
                    p.fillRect(x, y, ts, ts, self.COL_WATER)
                else:
                    p.fillRect(x, y, ts, ts, self.COL_DRY)

                if (cx, cy) == (self._hover_cx, self._hover_cy):
                    p.fillRect(x, y, ts, ts, self.COL_HOVER)

        # Selection
        if self._sel_cx >= 0:
            x = px0 + self._sel_cx * ts
            y = py0 + self._sel_cy * ts
            p.setPen(QPen(self.COL_SEL, 2))
            p.drawRect(x+1, y+1, ts-2, ts-2)

        # Grid lines (only when zoomed enough)
        if ts >= 4:
            p.setPen(QPen(self.COL_GRID, 1))
            for c in range(gw+1):
                p.drawLine(px0+c*ts, py0, px0+c*ts, py0+gw*ts)
            for r in range(gw+1):
                p.drawLine(px0, py0+r*ts, px0+gw*ts, py0+r*ts)
        p.end()

    def mouseMoveEvent(self, ev):
        cx, cy = self._cell_at(ev.pos())
        if (cx, cy) != (self._hover_cx, self._hover_cy):
            self._hover_cx, self._hover_cy = cx, cy
            self.update()

        ws = self._workshop
        if not ws or not self._drawing: return
        tool = ws._draw_tool
        if tool == 'pencil' and cx >= 0:
            self._set_cell(cx, cy, self._draw_val)
            self.update()
        elif tool in ('line', 'rect', 'rect_fill') and self._line_start and cx >= 0:
            self._preview_cells = self._shape_cells(tool, self._line_start, (cx,cy))
            self.update()

    def mousePressEvent(self, ev):
        cx, cy = self._cell_at(ev.pos())
        is_left  = ev.button() == Qt.MouseButton.LeftButton
        is_right = ev.button() == Qt.MouseButton.RightButton
        is_mid   = ev.button() == Qt.MouseButton.MiddleButton

        ws   = self._workshop
        tool = ws._draw_tool if ws else 'pencil'

        if is_mid:
            self._pan_drag_start  = ev.pos()
            self._pan_start_xy    = (self._pan_x, self._pan_y)
            return

        if tool == 'zoom':
            factor = 1.3 if is_left else (1/1.3)
            self._zoom = max(0.1, min(20.0, self._zoom * factor))
            self.update()
            return

        if cx < 0: return
        self._sel_cx, self._sel_cy = cx, cy
        self.cell_clicked.emit(cx, cy)
        self.update()

        if tool == 'picker':
            is_water = (self._cell_val(cx, cy) == 128)
            self.color_picked.emit(is_water, is_left)
            return

        # Left = draw water, right = erase to dry
        self._draw_val = 128 if is_left else 0

        if ws: ws._push_undo_grid()

        if tool == 'pencil':
            self._drawing = True
            self._set_cell(cx, cy, self._draw_val)
            self.update()
        elif tool == 'fill':
            self._flood_fill(cx, cy, self._draw_val)
            self.update()
            if ws: ws._on_grid_changed()
        elif tool in ('line', 'rect', 'rect_fill'):
            self._drawing    = True
            self._line_start = (cx, cy)
            self._preview_cells = set()
        elif tool == 'rect_all':
            # Fill entire grid with water
            for i in range(len(self._grid)):
                self._grid[i] = self._draw_val
            self.update()
            if ws: ws._on_grid_changed()

        if is_right:
            self.cell_right_clicked.emit(cx, cy, ev.globalPosition().toPoint())

    def mouseReleaseEvent(self, ev):
        if not self._drawing: return
        cx, cy = self._cell_at(ev.pos())
        ws   = self._workshop
        tool = ws._draw_tool if ws else 'pencil'

        if tool in ('line', 'rect', 'rect_fill') and self._line_start:
            if cx >= 0:
                for cell_cx, cell_cy in self._shape_cells(tool, self._line_start, (cx, cy)):
                    self._set_cell(cell_cx, cell_cy, self._draw_val)
        self._drawing       = False
        self._line_start    = None
        self._preview_cells = None
        if ws: ws._on_grid_changed()
        self.update()

    def wheelEvent(self, ev):
        delta = ev.angleDelta().y()
        if not delta: return
        factor = 1.12 if delta > 0 else (1/1.12)
        self._zoom = max(0.1, min(20.0, self._zoom * factor))
        self.update()

    def _set_cell(self, cx: int, cy: int, val: int):
        if 0 <= cx < self._grid_w and 0 <= cy < self._grid_w:
            self._grid[cy * self._grid_w + cx] = val

    def _shape_cells(self, tool: str, p0: tuple, p1: tuple) -> set:
        x0, y0 = min(p0[0],p1[0]), min(p0[1],p1[1])
        x1, y1 = max(p0[0],p1[0]), max(p0[1],p1[1])
        cells = set()
        if tool == 'line':
            # Bresenham
            cx0, cy0 = p0; cx1, cy1 = p1
            dx, dy = abs(cx1-cx0), abs(cy1-cy0)
            sx, sy = (1 if cx0<cx1 else -1), (1 if cy0<cy1 else -1)
            err = dx - dy
            while True:
                cells.add((cx0, cy0))
                if cx0 == cx1 and cy0 == cy1: break
                e2 = 2*err
                if e2 > -dy: err -= dy; cx0 += sx
                if e2 <  dx: err += dx; cy0 += sy
        elif tool == 'rect':
            for x in range(x0, x1+1):
                cells.add((x, y0)); cells.add((x, y1))
            for y in range(y0, y1+1):
                cells.add((x0, y)); cells.add((x1, y))
        elif tool == 'rect_fill':
            for y in range(y0, y1+1):
                for x in range(x0, x1+1):
                    cells.add((x, y))
        return cells

    def _flood_fill(self, cx: int, cy: int, val: int):
        target = self._cell_val(cx, cy)
        if target == val: return
        gw = self._grid_w
        stack = [(cx, cy)]
        visited = set()
        while stack:
            x, y = stack.pop()
            if (x, y) in visited: continue
            if not (0 <= x < gw and 0 <= y < gw): continue
            if self._grid[y*gw+x] != target: continue
            visited.add((x, y))
            self._grid[y*gw+x] = val
            stack.extend([(x+1,y),(x-1,y),(x,y+1),(x,y-1)])

    def fit(self):
        self._zoom  = 1.0
        self._pan_x = self._pan_y = 0
        self.update()

    def grid_as_rgba(self) -> bytes:
        """Render grid as RGBA bytes for export/thumbnail."""
        gw = self._grid_w
        out = bytearray(gw * gw * 4)
        for i in range(gw * gw):
            val = self._grid[i]
            if val == 128:
                out[i*4:i*4+4] = [30, 120, 220, 255]
            else:
                out[i*4:i*4+4] = [30, 30, 30, 255]
        return bytes(out)


# ─────────────────────────────────────────────────────────────────────────────
# SA quad canvas widget
# ─────────────────────────────────────────────────────────────────────────────

class SaWaterCanvas(QWidget):
    """Displays SA water quads on a world-space canvas. Supports selection/edit."""

    quad_selected = pyqtSignal(int)  # quad index

    COL_WATER  = QColor(30, 120, 220, 180)
    COL_SEL    = QColor(255, 200,   0, 220)
    COL_BORDER = QColor(80, 160, 255, 200)
    COL_BG     = QColor(20,  20,  20, 255)

    def __init__(self, parent=None):
        super().__init__(parent)
        self._quads   = []
        self._sel_idx = -1
        self._zoom    = 1.0
        self._pan_x   = 0
        self._pan_y   = 0
        self._bbox    = (-3000, -3000, 3000, 3000)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self.setMouseTracking(True)

    def setup(self, quads: list, bbox: tuple):
        self._quads   = quads
        self._bbox    = bbox
        self._sel_idx = -1
        self._fit()
        self.update()

    def _fit(self):
        if not self._quads: return
        x0, y0, x1, y1 = self._bbox
        world_w = x1 - x0
        world_h = y1 - y0
        if world_w <= 0 or world_h <= 0: return
        scale_x = self.width()  / world_w
        scale_y = self.height() / world_h
        self._zoom  = min(scale_x, scale_y) * 0.9
        self._pan_x = int(self.width()  / 2 - (x0 + world_w/2) * self._zoom)
        self._pan_y = int(self.height() / 2 - (y0 + world_h/2) * self._zoom)

    def _w2s(self, wx: float, wy: float) -> tuple:
        return int(wx * self._zoom + self._pan_x), int(wy * self._zoom + self._pan_y)

    def _s2w(self, sx: int, sy: int) -> tuple:
        return (sx - self._pan_x) / self._zoom, (sy - self._pan_y) / self._zoom

    def paintEvent(self, ev):
        p = QPainter(self)
        p.fillRect(self.rect(), self.COL_BG)
        if not self._quads:
            p.setPen(QColor(100, 100, 100))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "No SA water quads loaded")
            p.end()
            return

        for idx, q in enumerate(self._quads):
            corners = q['corners']
            pts = [QPoint(*self._w2s(c['x'], c['y'])) for c in corners]
            is_sel = (idx == self._sel_idx)
            p.setPen(QPen(self.COL_SEL if is_sel else self.COL_BORDER, 2 if is_sel else 1))
            p.setBrush(self.COL_SEL.lighter(120) if is_sel else self.COL_WATER)
            from PyQt6.QtGui import QPolygon
            poly = QPolygon(pts)
            p.drawPolygon(poly)
        p.end()

    def mousePressEvent(self, ev):
        if ev.button() != Qt.MouseButton.LeftButton: return
        wx, wy = self._s2w(ev.pos().x(), ev.pos().y())
        # Find closest quad by checking if click is inside
        for idx, q in enumerate(self._quads):
            if self._point_in_quad(wx, wy, q):
                self._sel_idx = idx
                self.quad_selected.emit(idx)
                self.update()
                return
        self._sel_idx = -1
        self.update()

    def _point_in_quad(self, wx: float, wy: float, q: dict) -> bool:
        corners = q['corners']
        xs = [c['x'] for c in corners]
        ys = [c['y'] for c in corners]
        return (min(xs) <= wx <= max(xs)) and (min(ys) <= wy <= max(ys))

    def wheelEvent(self, ev):
        delta = ev.angleDelta().y()
        factor = 1.12 if delta > 0 else (1/1.12)
        self._zoom = max(0.01, min(50.0, self._zoom * factor))
        self.update()

    def fit(self):
        self._fit()
        self.update()


# ─────────────────────────────────────────────────────────────────────────────
# Settings
# ─────────────────────────────────────────────────────────────────────────────

class WaterSettings:
    PATH = Path.home() / '.config' / 'imgfactory' / 'water_workshop.json'

    def __init__(self):
        self._data = {'recent_files': [], 'window_x': 100, 'window_y': 100,
                      'window_w': 1200, 'window_h': 700}
        self.load()

    def load(self):
        try:
            if self.PATH.exists():
                self._data.update(json.loads(self.PATH.read_text()))
        except Exception: pass

    def save(self):
        try:
            self.PATH.parent.mkdir(parents=True, exist_ok=True)
            self.PATH.write_text(json.dumps(self._data, indent=2))
        except Exception: pass

    def add_recent(self, path: str):
        r = self._data.setdefault('recent_files', [])
        path = str(path)
        if path in r: r.remove(path)
        r.insert(0, path)
        self._data['recent_files'] = r[:10]
        self.save()

    def get_recent(self) -> list:
        return [p for p in self._data.get('recent_files', []) if Path(p).exists()]


# ─────────────────────────────────────────────────────────────────────────────
# Main Water Workshop
# ─────────────────────────────────────────────────────────────────────────────

class WaterWorkshop(QWidget):
    """GTA Water Editor — waterpro.dat (binary) + water.dat (text) + SA quads."""

    def __init__(self, parent=None, main_window=None):
        super().__init__(parent)
        self.main_window    = main_window
        self.WAT_settings   = WaterSettings()

        # State
        self._waterpro      = None   # WaterproParser or None
        self._waterdat      = None   # WaterDatParser or None
        self._sa_water      = None   # SaWaterParser or None
        self._file_path     = None   # currently loaded file
        self._file_type     = None   # 'waterpro' | 'waterdat' | 'sa_water'
        self._dirty_phys    = False
        self._dirty_vis     = False

        # Undo stacks for each grid
        self._undo_phys: list = []   # list of bytearray snapshots
        self._undo_vis:  list = []
        self._redo_phys: list = []
        self._redo_vis:  list = []

        self._draw_tool     = 'pencil'
        self._active_grid   = 'phys'  # 'phys' or 'vis'

        self._setup_ui()
        self._setup_shortcuts()
        self._restore_geometry()

    # ── UI Setup ──────────────────────────────────────────────────────────────

    def _get_icon_color(self) -> str:
        bg = self.palette().window().color()
        return '#e0e0e0' if bg.lightness() < 128 else '#202020'

    def _setup_ui(self):
        self.setWindowTitle(f"Water Workshop — {Build}")
        self.setMinimumSize(900, 600)
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(0, 0, 0, 0)
        main_layout.setSpacing(0)
        main_layout.addLayout(self._build_toolbar())
        main_layout.addWidget(self._build_body(), 1)
        main_layout.addWidget(self._build_statusbar())

    def _build_toolbar(self) -> QHBoxLayout:
        layout = QHBoxLayout()
        layout.setContentsMargins(4, 2, 4, 2)
        layout.setSpacing(4)
        ic = self._get_icon_color()

        def _btn(icon_fn, tip, slot):
            b = QPushButton()
            try: b.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
            except Exception: pass
            b.setIconSize(QSize(20, 20))
            b.setFixedSize(35, 35)
            b.setToolTip(tip)
            b.clicked.connect(slot)
            layout.addWidget(b)
            return b

        _btn('open_icon',   "Load waterpro.dat / water.dat (Ctrl+O)", self._open_file)
        self.save_btn = _btn('save_icon', "Save (Ctrl+S)", self._save_file)
        self.save_btn.setEnabled(False)
        layout.addSpacing(8)
        _btn('export_icon', "Export grids as BMP", self._export_bmp)
        _btn('import_icon', "Import BMP grids",    self._import_bmp)
        layout.addSpacing(8)
        _btn('undo_icon',   "Undo (Ctrl+Z)",        self._undo)
        _btn('info_icon',   "About Water Workshop", self._show_about)
        layout.addStretch()

        # File info label
        self._file_lbl = QLabel("No file loaded")
        self._file_lbl.setStyleSheet("font-size:11px; color:#aaa;")
        layout.addWidget(self._file_lbl)
        return layout

    def _build_body(self) -> QWidget:
        body = QSplitter(Qt.Orientation.Horizontal)

        # ── Left panel: water levels / quad list ──────────────────────────────
        left = QFrame()
        left.setFrameStyle(QFrame.Shape.StyledPanel)
        left.setFixedWidth(200)
        ll = QVBoxLayout(left)
        ll.setContentsMargins(4, 4, 4, 4)
        ll.setSpacing(4)

        lbl = QLabel("Water Levels")
        lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        lbl.setStyleSheet("font-size:10px; font-weight:bold;")
        ll.addWidget(lbl)

        self._levels_list = QListWidget()
        self._levels_list.setAlternatingRowColors(True)
        self._levels_list.itemDoubleClicked.connect(self._edit_level)
        ll.addWidget(self._levels_list)

        self._level_add_btn = QPushButton("+ Add Level")
        self._level_del_btn = QPushButton("− Remove")
        self._level_add_btn.clicked.connect(self._add_level)
        self._level_del_btn.clicked.connect(self._del_level)
        row = QHBoxLayout()
        row.addWidget(self._level_add_btn)
        row.addWidget(self._level_del_btn)
        ll.addLayout(row)

        self._dirty_lbl = QLabel("Modified: no")
        self._dirty_lbl.setStyleSheet("font-size:9px;")
        ll.addWidget(self._dirty_lbl)

        body.addWidget(left)

        # ── Centre: tab view ──────────────────────────────────────────────────
        self._view_tabs = QTabWidget()
        self._view_tabs.setDocumentMode(True)
        self._view_tabs.currentChanged.connect(self._on_tab_changed)

        # Physical grid tab
        self._phys_canvas = WaterGridWidget()
        self._phys_canvas._workshop = self
        phys_scroll = QScrollArea()
        phys_scroll.setWidget(self._phys_canvas)
        phys_scroll.setWidgetResizable(True)
        self._view_tabs.addTab(phys_scroll, "Physical (64×64)")

        # Visible grid tab
        self._vis_canvas = WaterGridWidget()
        self._vis_canvas._workshop = self
        vis_scroll = QScrollArea()
        vis_scroll.setWidget(self._vis_canvas)
        vis_scroll.setWidgetResizable(True)
        self._view_tabs.addTab(vis_scroll, "Visible (128×128)")

        # SA quads tab
        self._sa_canvas = SaWaterCanvas()
        self._sa_canvas.quad_selected.connect(self._on_quad_selected)
        self._view_tabs.addTab(self._sa_canvas, "SA Quads")

        body.addWidget(self._view_tabs)

        # ── Right sidebar ─────────────────────────────────────────────────────
        body.addWidget(self._build_sidebar())
        body.setSizes([200, 900, 80])
        self._body_splitter = body
        return body

    def _build_sidebar(self) -> QFrame:
        sidebar = QFrame()
        sidebar.setFrameStyle(QFrame.Shape.StyledPanel)
        sidebar.setFixedWidth(80)
        sl = QVBoxLayout(sidebar)
        sl.setContentsMargins(2, 4, 2, 4)
        sl.setSpacing(2)
        ic = self._get_icon_color()
        BTN = 36

        def _nb(icon_fn, tip, slot, checkable=False):
            b = QToolButton()
            b.setFixedSize(BTN, BTN)
            try: b.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
            except Exception: pass
            b.setToolTip(tip)
            b.setCheckable(checkable)
            b.clicked.connect(slot)
            return b

        def _row(*btns):
            row = QHBoxLayout()
            row.setSpacing(2); row.setContentsMargins(0,0,0,0)
            for b in btns: row.addWidget(b)
            if len(btns) == 1: row.addStretch()
            sl.addLayout(row)

        def _sep():
            s = QFrame(); s.setFrameShape(QFrame.Shape.HLine)
            sl.addSpacing(2); sl.addWidget(s); sl.addSpacing(2)

        # View tools
        _row(_nb('zoom_in_icon',  "Zoom in",          lambda: self._zoom(1.25)),
             _nb('zoom_out_icon', "Zoom out",          lambda: self._zoom(0.8)))
        _row(_nb('fit_grid_icon', "Fit (Ctrl+0)",      self._fit),
             _nb('search_icon',   "Toggle grid lines", self._toggle_grid))

        _sep()

        # Draw tools
        self._draw_tool = 'pencil'
        self._draw_btns = {}

        def _tool_btn(icon_fn, tip, tool):
            b = _nb(icon_fn, tip, lambda checked=False, t=tool: self._set_draw_tool(t),
                    checkable=True)
            self._draw_btns[tool] = b
            return b

        _row(_tool_btn('paint_icon',    "Pencil — draw water (P)",   'pencil'),
             _tool_btn('fill_icon',     "Flood fill (F)",             'fill'))
        _row(_tool_btn('line_icon',     "Line (L)",                   'line'),
             _tool_btn('rect_icon',     "Rect outline (R)",           'rect'))
        _row(_tool_btn('rect_fill_icon',"Filled rect (Shift+R)",      'rect_fill'),
             _tool_btn('dropper_icon',  "Dropper — pick water/dry (K)",'picker'))
        _row(_tool_btn('scissors_icon', "Erase all (clear grid)",     'erase_all'),
             _tool_btn('zoom_in_icon',  "Zoom tool (Z)",              'zoom'))
        self._draw_btns['pencil'].setChecked(True)

        _sep()

        # Water / Dry colour swatches
        sl.addWidget(QLabel("Draw:"))
        self._water_btn = QPushButton("Water")
        self._water_btn.setFixedSize(72, 22)
        self._water_btn.setToolTip("Left-click draws water (128)")
        self._water_btn.setStyleSheet(f"background-color: #1e78dc; color: white;")
        self._dry_btn = QPushButton("Dry")
        self._dry_btn.setFixedSize(72, 22)
        self._dry_btn.setToolTip("Right-click erases to dry (0)")
        self._dry_btn.setStyleSheet("background-color: #1e1e1e; color: #aaa;")
        sl.addWidget(self._water_btn)
        sl.addWidget(self._dry_btn)

        sl.addStretch(0)
        return sidebar

    def _build_statusbar(self) -> QLabel:
        self._status_bar = QLabel("Ready — Load a waterpro.dat or water.dat file")
        self._status_bar.setStyleSheet(
            "padding:2px 6px; background:#1a1a1a; color:#ccc; font-size:11px;")
        self._status_bar.setFixedHeight(22)
        return self._status_bar

    def _setup_shortcuts(self):
        for key, fn in [
            ("Ctrl+O", self._open_file),
            ("Ctrl+S", self._save_file),
            ("Ctrl+Z", self._undo),
            ("Ctrl+Y", self._redo),
            ("Ctrl+0", self._fit),
        ]:
            QShortcut(QKeySequence(key), self).activated.connect(fn)
        for key, tool in [
            ("P", "pencil"), ("F", "fill"), ("L", "line"),
            ("R", "rect"),   ("K", "picker"), ("Z", "zoom"),
        ]:
            QShortcut(QKeySequence(key), self).activated.connect(
                lambda t=tool: self._set_draw_tool(t))
        QShortcut(QKeySequence("Shift+R"), self).activated.connect(
            lambda: self._set_draw_tool('rect_fill'))

    # ── Geometry ──────────────────────────────────────────────────────────────

    def _restore_geometry(self):
        d = self.WAT_settings._data
        self.resize(d.get('window_w', 1200), d.get('window_h', 700))
        self.move(d.get('window_x', 100), d.get('window_y', 100))

    def closeEvent(self, ev):
        g = self.geometry()
        self.WAT_settings._data.update({
            'window_x': g.x(), 'window_y': g.y(),
            'window_w': g.width(), 'window_h': g.height()})
        self.WAT_settings.save()
        super().closeEvent(ev)

    # ── Status / helpers ──────────────────────────────────────────────────────

    def _set_status(self, msg: str):
        self._status_bar.setText(msg)

    def _on_grid_changed(self):
        self._dirty_phys = True
        self._dirty_lbl.setText("Modified: yes")
        self.save_btn.setEnabled(True)

    def _on_tab_changed(self, idx: int):
        self._active_grid = 'phys' if idx == 0 else 'vis'

    def _active_canvas(self) -> WaterGridWidget:
        return self._phys_canvas if self._active_grid == 'phys' else self._vis_canvas

    # ── Undo/redo ─────────────────────────────────────────────────────────────

    def _push_undo_grid(self):
        if self._active_grid == 'phys' and self._waterpro:
            self._undo_phys.append(bytearray(self._waterpro.phys_grid))
            if len(self._undo_phys) > 20: self._undo_phys.pop(0)
            self._redo_phys.clear()
        elif self._active_grid == 'vis' and self._waterpro:
            self._undo_vis.append(bytearray(self._waterpro.vis_grid))
            if len(self._undo_vis) > 20: self._undo_vis.pop(0)
            self._redo_vis.clear()

    def _undo(self):
        if self._active_grid == 'phys':
            stack, redo, grid_attr, canvas = \
                self._undo_phys, self._redo_phys, 'phys_grid', self._phys_canvas
        else:
            stack, redo, grid_attr, canvas = \
                self._undo_vis, self._redo_vis, 'vis_grid', self._vis_canvas
        if not stack:
            self._set_status("Nothing to undo"); return
        if self._waterpro:
            redo.append(bytearray(getattr(self._waterpro, grid_attr)))
            setattr(self._waterpro, grid_attr, stack.pop())
            canvas.set_grid(getattr(self._waterpro, grid_attr))
        self._set_status("Undo")

    def _redo(self):
        if self._active_grid == 'phys':
            stack, undo, grid_attr, canvas = \
                self._redo_phys, self._undo_phys, 'phys_grid', self._phys_canvas
        else:
            stack, undo, grid_attr, canvas = \
                self._redo_vis, self._undo_vis, 'vis_grid', self._vis_canvas
        if not stack:
            self._set_status("Nothing to redo"); return
        if self._waterpro:
            undo.append(bytearray(getattr(self._waterpro, grid_attr)))
            setattr(self._waterpro, grid_attr, stack.pop())
            canvas.set_grid(getattr(self._waterpro, grid_attr))
        self._set_status("Redo")

    # ── Draw tools ────────────────────────────────────────────────────────────

    def _set_draw_tool(self, tool: str):
        if tool == 'erase_all':
            self._erase_all_grid()
            return
        self._draw_tool = tool
        for name, btn in self._draw_btns.items():
            btn.setChecked(name == tool)
        cursors = {
            'pencil': Qt.CursorShape.CrossCursor,
            'fill':   Qt.CursorShape.PointingHandCursor,
            'line':   Qt.CursorShape.CrossCursor,
            'rect':   Qt.CursorShape.CrossCursor,
            'rect_fill': Qt.CursorShape.CrossCursor,
            'picker': Qt.CursorShape.WhatsThisCursor,
            'zoom':   Qt.CursorShape.SizeFDiagCursor,
        }
        cur = cursors.get(tool, Qt.CursorShape.ArrowCursor)
        self._phys_canvas.setCursor(cur)
        self._vis_canvas.setCursor(cur)

    def _erase_all_grid(self):
        canvas = self._active_canvas()
        if not canvas._grid_w: return
        if QMessageBox.question(self, "Clear Grid",
                "Clear entire grid (set all cells to Dry)?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                ) != QMessageBox.StandardButton.Yes: return
        self._push_undo_grid()
        canvas._grid[:] = bytearray(len(canvas._grid))
        canvas.update()
        self._on_grid_changed()

    # ── Zoom/fit ──────────────────────────────────────────────────────────────

    def _zoom(self, f: float):
        for c in (self._phys_canvas, self._vis_canvas):
            c._zoom = max(0.1, min(20.0, c._zoom * f))
            c.update()

    def _fit(self):
        for c in (self._phys_canvas, self._vis_canvas):
            c.fit()
        self._sa_canvas.fit()

    def _toggle_grid(self):
        pass  # grid lines auto-hide at small zoom — placeholder

    # ── File operations ───────────────────────────────────────────────────────

    def _open_file(self, path: str = None):
        if not path:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open Water File", "",
                "Water Files (*.dat *.DAT);;All Files (*)")
        if not path: return
        self._load_file(path)

    def _load_file(self, path: str):
        try:
            data = Path(path).read_bytes()
            name = Path(path).name.upper()

            # Detect format
            if name in ('WATERPRO.DAT', 'WATERPRO') or self._is_binary_waterpro(data):
                self._load_waterpro(path, data)
            elif self._is_sa_water(data):
                self._load_sa_water(path)
            else:
                self._load_waterdat(path)

            self.WAT_settings.add_recent(path)
            self._file_path = path
            self._dirty_phys = self._dirty_vis = False
            self._dirty_lbl.setText("Modified: no")
            self.save_btn.setEnabled(False)
            self._file_lbl.setText(Path(path).name)

        except Exception as e:
            import traceback
            QMessageBox.critical(self, "Load Error",
                f"Failed to load {Path(path).name}:\n{e}\n\n{traceback.format_exc()[-300:]}")

    def _is_binary_waterpro(self, data: bytes) -> bool:
        if len(data) < 100: return False
        remainder = len(data) - 964
        if remainder <= 0 or remainder % 5 != 0: return False
        gw_sq = remainder // 5
        gw = int(math.isqrt(gw_sq))
        return gw * gw == gw_sq

    def _is_sa_water(self, data: bytes) -> bool:
        try:
            text = data[:20].decode('latin1')
            return text.startswith('processed')
        except Exception:
            return False

    def _load_waterpro(self, path: str, data: bytes):
        wp = WaterproParser()
        wp.load(path)
        self._waterpro   = wp
        self._waterdat   = None
        self._sa_water   = None
        self._file_type  = 'waterpro'
        gw = wp.grid_w

        # Setup grids
        self._phys_canvas.setup(gw, wp.phys_grid)
        self._vis_canvas.setup(gw * 2, wp.vis_grid)
        self._view_tabs.setTabText(0, f"Physical ({gw}×{gw})")
        self._view_tabs.setTabText(1, f"Visible ({gw*2}×{gw*2})")
        self._view_tabs.setTabEnabled(0, True)
        self._view_tabs.setTabEnabled(1, True)
        self._view_tabs.setTabEnabled(2, False)

        # Populate levels list
        self._refresh_levels_list()

        n_water_phys = sum(1 for b in wp.phys_grid if b == 128)
        n_water_vis  = sum(1 for b in wp.vis_grid  if b == 128)
        self._set_status(
            f"Loaded {Path(path).name}  —  {gw}×{gw} physical "
            f"({n_water_phys} water cells), {gw*2}×{gw*2} visible "
            f"({n_water_vis} water cells),  {wp.water_levels_count} water level(s)")

    def _load_waterdat(self, path: str):
        wd = WaterDatParser()
        wd.load(path)
        self._waterdat  = wd
        self._waterpro  = None
        self._sa_water  = None
        self._file_type = 'waterdat'

        # Disable binary grid tabs, show info in levels list
        self._view_tabs.setTabEnabled(0, False)
        self._view_tabs.setTabEnabled(1, False)
        self._view_tabs.setTabEnabled(2, False)

        self._levels_list.clear()
        for i, r in enumerate(wd.rects):
            item = QListWidgetItem(
                f"[{i}] Z={r[0]:.1f}  ({r[1]:.0f},{r[2]:.0f}) → ({r[3]:.0f},{r[4]:.0f})")
            self._levels_list.addItem(item)

        self._set_status(
            f"Loaded {Path(path).name}  —  {len(wd.rects)} water rectangle(s)")

    def _load_sa_water(self, path: str):
        sa = SaWaterParser()
        sa.load(path)
        self._sa_water  = sa
        self._waterpro  = None
        self._waterdat  = None
        self._file_type = 'sa_water'

        self._view_tabs.setTabEnabled(0, False)
        self._view_tabs.setTabEnabled(1, False)
        self._view_tabs.setTabEnabled(2, True)
        self._view_tabs.setCurrentIndex(2)

        self._sa_canvas.setup(sa.quads, sa.world_bbox())

        self._levels_list.clear()
        for i, q in enumerate(sa.quads):
            c = q['corners'][0]
            item = QListWidgetItem(
                f"[{i}] ({c['x']:.0f},{c['y']:.0f})  flag={q['flag']}")
            self._levels_list.addItem(item)

        self._set_status(
            f"Loaded {Path(path).name}  —  {len(sa.quads)} SA water quads")

    def _refresh_levels_list(self):
        self._levels_list.clear()
        if not self._waterpro: return
        wp = self._waterpro
        for i in range(wp.water_levels_count):
            h = wp.water_level_data[i]
            item = QListWidgetItem(f"Level {i}:  Z = {h:.4f}")
            self._levels_list.addItem(item)

    def _save_file(self):
        if not self._file_path:
            self._set_status("No file loaded"); return

        if self._file_type == 'waterpro' and self._waterpro:
            path, _ = QFileDialog.getSaveFileName(
                self, "Save waterpro.dat", self._file_path,
                "DAT Files (*.dat *.DAT);;All Files (*)")
            if not path: return
            try:
                self._waterpro.save(path)
                self._dirty_phys = self._dirty_vis = False
                self._dirty_lbl.setText("Modified: no")
                self.save_btn.setEnabled(False)
                self._set_status(f"Saved → {Path(path).name}")
            except Exception as e:
                QMessageBox.critical(self, "Save Error", str(e))

        elif self._file_type == 'waterdat' and self._waterdat:
            path, _ = QFileDialog.getSaveFileName(
                self, "Save water.dat", self._file_path,
                "DAT Files (*.dat *.DAT);;All Files (*)")
            if not path: return
            try:
                self._waterdat.save(path)
                self._set_status(f"Saved → {Path(path).name}")
            except Exception as e:
                QMessageBox.critical(self, "Save Error", str(e))

        elif self._file_type == 'sa_water' and self._sa_water:
            path, _ = QFileDialog.getSaveFileName(
                self, "Save SA water.dat", self._file_path,
                "DAT Files (*.dat);;All Files (*)")
            if not path: return
            try:
                self._sa_water.save(path)
                self._set_status(f"Saved → {Path(path).name}")
            except Exception as e:
                QMessageBox.critical(self, "Save Error", str(e))

    # ── Export / Import BMP ───────────────────────────────────────────────────

    def _export_bmp(self):
        if not self._waterpro:
            QMessageBox.information(self, "Export", "Load a binary waterpro.dat first.")
            return
        wp  = self._waterpro
        gw  = wp.grid_w
        stem = Path(self._file_path).stem if self._file_path else "water"

        try:
            from PIL import Image

            def grid_to_img(grid, w, h):
                img = Image.new("L", (w, h), 0)
                for i in range(w * h):
                    img.putpixel((i % w, i // w), 128 if grid[i] == 128 else 0)
                img = img.rotate(90)  # match WaterproGen orientation
                return img

            # Physical
            p_path, _ = QFileDialog.getSaveFileName(
                self, "Export Physical Grid", f"{stem}_physical.bmp", "BMP (*.bmp)")
            if p_path:
                grid_to_img(wp.phys_grid, gw, gw).save(p_path)
                self._set_status(f"Exported physical grid → {Path(p_path).name}")

            # Visible
            v_path, _ = QFileDialog.getSaveFileName(
                self, "Export Visible Grid", f"{stem}_visible.bmp", "BMP (*.bmp)")
            if v_path:
                grid_to_img(wp.vis_grid, gw*2, gw*2).save(v_path)
                self._set_status(f"Exported visible grid → {Path(v_path).name}")

        except Exception as e:
            QMessageBox.critical(self, "Export Error", str(e))

    def _import_bmp(self):
        if not self._waterpro:
            QMessageBox.information(self, "Import", "Load a binary waterpro.dat first.")
            return
        wp = self._waterpro
        gw = wp.grid_w

        path, _ = QFileDialog.getOpenFileName(
            self, "Import Grid BMP", "",
            "Images (*.bmp *.png);;All Files (*)")
        if not path: return

        try:
            from PIL import Image
            img = Image.open(path).convert("L")
            w, h = img.size

            if w == gw and h == gw:
                target = 'phys'
            elif w == gw*2 and h == gw*2:
                target = 'vis'
            else:
                QMessageBox.warning(self, "Size Mismatch",
                    f"Image {w}×{h} doesn't match physical ({gw}×{gw}) "
                    f"or visible ({gw*2}×{gw*2}) grid size.")
                return

            img = img.rotate(-90)  # reverse WaterproGen rotation
            grid = bytearray(w * h)
            for y in range(h):
                for x in range(w):
                    grid[y*w+x] = 128 if img.getpixel((x, y)) > 64 else 0

            self._push_undo_grid()
            if target == 'phys':
                wp.phys_grid = grid
                self._phys_canvas.set_grid(grid)
                self._set_status(f"Imported physical grid from {Path(path).name}")
            else:
                wp.vis_grid = grid
                self._vis_canvas.set_grid(grid)
                self._set_status(f"Imported visible grid from {Path(path).name}")

            self._on_grid_changed()

        except Exception as e:
            QMessageBox.critical(self, "Import Error", str(e))

    # ── Water levels ─────────────────────────────────────────────────────────

    def _edit_level(self, item: QListWidgetItem):
        if not self._waterpro: return
        idx = self._levels_list.row(item)
        wp  = self._waterpro
        if idx < 0 or idx >= wp.water_levels_count: return

        dlg = QDialog(self)
        dlg.setWindowTitle(f"Edit Water Level {idx}")
        layout = QFormLayout(dlg)
        spin = QDoubleSpinBox()
        spin.setRange(-1000.0, 1000.0)
        spin.setDecimals(4)
        spin.setValue(wp.water_level_data[idx])
        layout.addRow(f"Level {idx} Z height:", spin)
        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                                QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        layout.addRow(btns)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            wp.water_level_data[idx] = spin.value()
            self._refresh_levels_list()
            self._on_grid_changed()

    def _add_level(self):
        if not self._waterpro: return
        wp = self._waterpro
        if wp.water_levels_count >= WaterproParser.WATER_LEVELS:
            QMessageBox.information(self, "At Maximum",
                f"Maximum {WaterproParser.WATER_LEVELS} water levels."); return
        wp.water_level_data[wp.water_levels_count] = 0.0
        wp.water_levels_count += 1
        self._refresh_levels_list()
        self._on_grid_changed()

    def _del_level(self):
        if not self._waterpro: return
        wp  = self._waterpro
        idx = self._levels_list.currentRow()
        if idx < 0 or idx >= wp.water_levels_count: return
        wp.water_level_data[idx:wp.water_levels_count-1] = \
            wp.water_level_data[idx+1:wp.water_levels_count]
        wp.water_level_data[wp.water_levels_count-1] = 0.0
        wp.water_levels_count -= 1
        self._refresh_levels_list()
        self._on_grid_changed()

    # ── SA quad selection ─────────────────────────────────────────────────────

    def _on_quad_selected(self, idx: int):
        if not self._sa_water: return
        self._levels_list.setCurrentRow(idx)
        q = self._sa_water.quads[idx]
        c = q['corners'][0]
        self._set_status(
            f"Quad {idx}: ({c['x']:.1f}, {c['y']:.1f})  flag={q['flag']}  "
            f"f={[f'{v:.3f}' for v in c['f']]}")

    # ── About ─────────────────────────────────────────────────────────────────

    def _show_about(self):
        QMessageBox.information(self, "Water Workshop",
            f"Water Workshop {Build}\n"
            f"GTA III / VC / PS2 LC / SOL — waterpro.dat + water.dat\n"
            f"GTA SA — water.dat / water1.dat (quad format)\n\n"
            f"Formats:\n"
            f"  waterpro.dat: binary grid (64×64 phys + 128×128 vis)\n"
            f"  water.dat: text rectangles (level, x1, y1, x2, y2)\n"
            f"  SA water: text quads (4 corners + 5 floats + flag)\n\n"
            f"Part of IMG Factory 1.6 — X-Seti Apr 2026")
