#!/usr/bin/env python3
# apps/components/Water_Editor/water_workshop.py - Version: 3
# X-Seti - Apr 2026 - IMG Factory 1.6 - Water Workshop
# Built on temp_workshop.py / GUIWorkshop base
# Section 1: Parsers (WaterproParser, WaterDatParser, SaWaterParser)
# Section 2: Canvas widgets (WaterGridWidget, SaWaterCanvas)
# Section 3: WaterWorkshop(GUIWorkshop) - panel overrides + menus
# Section 4: Water logic - open/save/draw/undo/levels

import sys, struct, re, math
from pathlib import Path

from PyQt6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QFrame,
    QLabel, QToolButton, QPushButton, QListWidget, QListWidgetItem,
    QFileDialog, QMessageBox, QTabWidget, QScrollArea, QSizePolicy,
    QDialog, QFormLayout, QDialogButtonBox, QDoubleSpinBox, QMenu,
    QSplitter
)
from PyQt6.QtGui import (
    QColor, QPainter, QPen, QFont, QIcon, QPolygon,
    QKeySequence, QShortcut
)
from PyQt6.QtCore import Qt, QSize, QPoint, pyqtSignal

try:
    from apps.components.Tmp_Template.gui_workshop import GUIWorkshop
except ImportError:
    import os
    sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))
    from apps.components.Tmp_Template.gui_workshop import GUIWorkshop

try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
except ImportError:
    class SVGIconFactory:
        @staticmethod
        def _s(sz=20, c=None): return QIcon()
        open_icon = save_icon = export_icon = import_icon = undo_icon = \
        info_icon = properties_icon = settings_icon = zoom_in_icon = \
        zoom_out_icon = fit_grid_icon = locate_icon = paint_icon = \
        fill_icon = dropper_icon = line_icon = rect_icon = rect_fill_icon = \
        scissors_icon = rotate_cw_icon = search_icon = staticmethod(_s)

def _apply_dialog_theme(dlg, mw):
    try:
        from apps.core.theme_utils import apply_dialog_theme
        apply_dialog_theme(dlg, mw)
    except Exception:
        pass

App_name  = "Water Workshop"
App_build = "Apr 2026"
Build     = "Build 3"


# =============================================================================
# SECTION 1 - Parsers
# =============================================================================

class WaterproParser:
    HEADER_SIZE  = 964
    WATER_LEVELS = 48

    def __init__(self):
        self.water_levels_count = 0
        self.water_level_data   = [0.0] * self.WATER_LEVELS
        self.unk_block          = bytes(768)
        self.grid_w             = 0
        self.phys_grid          = bytearray()
        self.vis_grid           = bytearray()
        self.path               = None

    def load(self, path: str):
        data = Path(path).read_bytes()
        rem  = len(data) - self.HEADER_SIZE
        if rem <= 0 or rem % 5 != 0:
            raise ValueError(f"Invalid waterpro.dat size: {len(data)} bytes")
        gw = int(math.isqrt(rem // 5))
        if gw * gw != rem // 5:
            raise ValueError("Grid area is not a perfect square")
        self.grid_w             = gw
        self.water_levels_count = data[0]
        self.water_level_data   = list(struct.unpack_from(f"<{self.WATER_LEVELS}f", data, 4))
        self.unk_block          = data[196:964]
        self.phys_grid          = bytearray(data[964 : 964 + gw*gw])
        self.vis_grid           = bytearray(data[964+gw*gw : 964+gw*gw+(2*gw)*(2*gw)])
        self.path               = path

    def save(self, path: str):
        gw  = self.grid_w
        out = bytearray(self.HEADER_SIZE + gw*gw + (2*gw)*(2*gw))
        out[0] = self.water_levels_count & 0xFF
        struct.pack_into(f"<{self.WATER_LEVELS}f", out, 4, *self.water_level_data)
        out[196:964]       = self.unk_block
        out[964:964+gw*gw] = self.phys_grid
        out[964+gw*gw:]    = self.vis_grid
        Path(path).write_bytes(bytes(out))


class WaterDatParser:
    def __init__(self):
        self.rects  = []
        self.header = ""
        self.path   = None

    def load(self, path: str):
        self.path  = path
        self.rects = []
        hdr        = []
        in_hdr     = True
        for line in Path(path).read_text(encoding="latin1", errors="replace").splitlines():
            s = line.strip()
            if s.startswith("*"):
                break
            if not s or s.startswith(";"):
                if in_hdr:
                    hdr.append(line)
                continue
            in_hdr = False
            parts  = [p for p in re.split(r"[\s,]+", s.rstrip(",")) if p]
            if len(parts) >= 5:
                try:
                    self.rects.append(tuple(float(p) for p in parts[:5]))
                except ValueError:
                    pass
        self.header = "\n".join(hdr)

    def save(self, path: str):
        lines = [self.header, ""]
        for r in self.rects:
            lines.append(f"{r[0]:.4f},\t{r[1]:.4f},\t{r[2]:.4f},\t{r[3]:.4f},\t{r[4]:.4f},")
        lines.append("* ;end of file")
        Path(path).write_text("\n".join(lines), encoding="latin1")


class SaWaterParser:
    def __init__(self):
        self.quads = []
        self.path  = None

    def load(self, path: str):
        self.path  = path
        self.quads = []
        for line in Path(path).read_text(encoding="latin1", errors="replace").splitlines():
            s = line.strip()
            if not s or s.startswith(";") or s.startswith("*") or s == "processed":
                continue
            parts = s.split()
            if len(parts) < 29:
                continue
            try:
                corners = [
                    {"x": float(parts[c*7]),
                     "y": float(parts[c*7+1]),
                     "f": [float(parts[c*7+k]) for k in range(2, 7)]}
                    for c in range(4)
                ]
                self.quads.append({"corners": corners, "flag": int(parts[28])})
            except (ValueError, IndexError):
                pass

    def save(self, path: str):
        lines = ["processed"]
        for q in self.quads:
            row = "    ".join(
                f"{c['x']:.4f} {c['y']:.4f} " + " ".join(f"{v:.5f}" for v in c["f"])
                for c in q["corners"]
            )
            lines.append(row + f"  {q['flag']}")
        Path(path).write_text("\n".join(lines), encoding="latin1")

    def world_bbox(self):
        if not self.quads:
            return -3000, -3000, 3000, 3000
        xs = [c["x"] for q in self.quads for c in q["corners"]]
        ys = [c["y"] for q in self.quads for c in q["corners"]]
        return min(xs), min(ys), max(xs), max(ys)


# =============================================================================
# SECTION 2 - Canvas widgets
# =============================================================================

class WaterGridWidget(QWidget):
    cell_clicked       = pyqtSignal(int, int)
    cell_right_clicked = pyqtSignal(int, int, QPoint)
    color_picked       = pyqtSignal(bool, bool)

    COL_WATER  = QColor(30, 120, 220, 255)
    COL_DRY    = QColor(60,  45,  25, 255)
    COL_LEVELS = [
        QColor(30,  120, 220, 255),
        QColor(20,   80, 180, 255),
        QColor(10,  160, 200, 255),
        QColor(50,  200, 220, 255),
        QColor(100, 180, 255, 255),
    ]
    COL_GRID  = QColor(60,  60,  80, 120)
    COL_SEL   = QColor(255, 200,   0, 200)
    COL_HOVER = QColor(255, 255, 255,  40)
    COL_PREV  = QColor(80,  180, 255, 160)

    def __init__(self, parent=None):
        super().__init__(parent)
        self._grid_w        = 0
        self._grid          = bytearray()
        self._zoom          = 1.0
        self._pan_x         = 0
        self._pan_y         = 0
        self._sel_cx        = -1
        self._sel_cy        = -1
        self._hover_cx      = -1
        self._hover_cy      = -1
        self._drawing       = False
        self._draw_val      = 128
        self._line_start    = None
        self._preview_cells = set()
        self._workshop      = None
        self._show_grid     = True
        self._pan_drag      = None
        self._pan_start     = (0, 0)
        self._colour_flipped = False
        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.WheelFocus)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)

    def setup(self, grid_w: int, grid: bytearray):
        self._grid_w = grid_w
        self._grid   = grid
        self._sel_cx = self._sel_cy = -1
        self._zoom   = 1.0
        self._pan_x  = self._pan_y = 0
        if hasattr(self, "_img_cache"): del self._img_cache
        self.update()

    def set_grid(self, grid: bytearray):
        self._grid = grid
        if hasattr(self, "_img_cache"): del self._img_cache
        self.update()

    def _cell_col(self, val: int) -> QColor:
        if val == 128:
            return self.COL_WATER if self._colour_flipped else self.COL_DRY
        elif val == 0:
            return self.COL_DRY if self._colour_flipped else self.COL_WATER
        else:
            idx = min(val, len(self.COL_LEVELS) - 1)
            col = self.COL_LEVELS[idx]
            if self._colour_flipped:
                return QColor(255-col.red(), 255-col.green(), 255-col.blue(), 255)
            return col

    def _ts(self):
        if not self._grid_w:
            return 8
        base = min(self.width() // self._grid_w, self.height() // self._grid_w)
        return max(2, int(base * self._zoom))

    def _cell_at(self, pos):
        ts     = self._ts()
        ax, ay = pos.x() - self._pan_x, pos.y() - self._pan_y
        if ax < 0 or ay < 0:
            return -1, -1
        cx, cy = ax // ts, ay // ts
        if 0 <= cx < self._grid_w and 0 <= cy < self._grid_w:
            return cx, cy
        return -1, -1

    def _cell_val(self, cx, cy):
        if 0 <= cx < self._grid_w and 0 <= cy < self._grid_w:
            return self._grid[cy * self._grid_w + cx]
        return 0

    def _set_cell(self, cx, cy, val):
        if 0 <= cx < self._grid_w and 0 <= cy < self._grid_w:
            self._grid[cy * self._grid_w + cx] = val

    def _rebuild_cache(self):
        """Render grid data to a QImage for fast blitting."""
        from PyQt6.QtGui import QImage
        gw  = self._grid_w
        img = QImage(gw, gw, QImage.Format.Format_RGB32)
        for cy in range(gw):
            for cx in range(gw):
                img.setPixel(cx, cy, self._cell_col(self._grid[cy*gw+cx]).rgb())
        self._img_cache  = img
        self._cache_flip = self._colour_flipped

    def paintEvent(self, ev):
        if not self._grid_w:
            return
        ts  = self._ts()
        gw  = self._grid_w
        px0 = self._pan_x
        py0 = self._pan_y
        p   = QPainter(self)

        # Fast path: scale the cached QImage to current zoom
        if not hasattr(self, "_img_cache") or                 getattr(self, "_cache_flip", None) != self._colour_flipped:
            self._rebuild_cache()
        from PyQt6.QtCore import QRect
        p.drawImage(QRect(px0, py0, gw*ts, gw*ts),
                    self._img_cache, self._img_cache.rect())

        # Preview cells on top
        for (cx, cy) in self._preview_cells:
            p.fillRect(px0+cx*ts, py0+cy*ts, ts, ts, self.COL_PREV)

        # Hover overlay
        if self._hover_cx >= 0:
            p.fillRect(px0+self._hover_cx*ts, py0+self._hover_cy*ts,
                       ts, ts, self.COL_HOVER)

        # Selection
        if self._sel_cx >= 0:
            x, y = px0 + self._sel_cx*ts, py0 + self._sel_cy*ts
            p.setPen(QPen(self.COL_SEL, 2))
            p.drawRect(x+1, y+1, ts-2, ts-2)

        # Grid lines when zoomed in enough
        if self._show_grid and ts >= 4:
            p.setPen(QPen(self.COL_GRID, 1))
            for c in range(gw+1):
                p.drawLine(px0+c*ts, py0, px0+c*ts, py0+gw*ts)
            for r in range(gw+1):
                p.drawLine(px0, py0+r*ts, px0+gw*ts, py0+r*ts)

        ws   = self._workshop
        tool = ws._active_tool if ws else "pencil"
        p.setPen(QColor(255, 255, 255, 180))
        p.setFont(QFont("monospace", 8))
        p.drawText(4, self.height()-6,
                   f"{gw}x{gw}  z={self._zoom:.1f}x  [{tool}]  L=sea(0) R=land(128)")
        p.end()

    def mouseMoveEvent(self, ev):
        if self._pan_drag and ev.buttons() & Qt.MouseButton.MiddleButton:
            dx = ev.pos().x() - self._pan_drag.x()
            dy = ev.pos().y() - self._pan_drag.y()
            self._pan_x = self._pan_start[0] + dx
            self._pan_y = self._pan_start[1] + dy
            self.update()
            return
        cx, cy = self._cell_at(ev.pos())
        if (cx, cy) != (self._hover_cx, self._hover_cy):
            self._hover_cx, self._hover_cy = cx, cy
            self.update()
        ws = self._workshop
        if not ws or not self._drawing:
            return
        tool = ws._active_tool
        if tool == "pencil" and cx >= 0:
            self._set_cell(cx, cy, self._draw_val)
            self.update()
        elif tool in ("line", "rect", "rect_fill") and self._line_start and cx >= 0:
            self._preview_cells = self._shape_cells(tool, self._line_start, (cx, cy))
            self.update()

    def mousePressEvent(self, ev):
        is_left  = ev.button() == Qt.MouseButton.LeftButton
        is_right = ev.button() == Qt.MouseButton.RightButton
        is_mid   = ev.button() == Qt.MouseButton.MiddleButton
        ws   = self._workshop
        tool = ws._active_tool if ws else "pencil"
        if is_mid:
            self._pan_drag  = ev.pos()
            self._pan_start = (self._pan_x, self._pan_y)
            return
        if tool == "zoom":
            f  = 1.3 if is_left else 1/1.3
            cx, cy = ev.pos().x(), ev.pos().y()
            old = self._ts()
            self._zoom = max(0.1, min(20.0, self._zoom * f))
            new = self._ts()
            self._pan_x = cx - int((cx - self._pan_x) * new / max(1, old))
            self._pan_y = cy - int((cy - self._pan_y) * new / max(1, old))
            self.update()
            return
        cx, cy = self._cell_at(ev.pos())
        if cx < 0:
            return
        self._sel_cx, self._sel_cy = cx, cy
        self.cell_clicked.emit(cx, cy)
        self.update()
        if tool == "picker":
            self.color_picked.emit(self._cell_val(cx, cy) == 0, is_left)  # 0=water
            return
        self._draw_val = 0 if is_left else 128  # L=draw water(0), R=draw land(128)
        if ws:
            ws._push_undo_grid()
        if tool == "pencil":
            self._drawing = True
            self._set_cell(cx, cy, self._draw_val)
            self.update()
        elif tool == "fill":
            self._flood_fill(cx, cy, self._draw_val)
            self.update()
            if ws:
                ws._on_grid_changed()
        elif tool in ("line", "rect", "rect_fill"):
            self._drawing    = True
            self._line_start = (cx, cy)
            self._preview_cells = set()
        if is_right and cx >= 0:
            self.cell_right_clicked.emit(cx, cy, ev.globalPosition().toPoint())

    def mouseReleaseEvent(self, ev):
        self._pan_drag = None
        if not self._drawing:
            return
        cx, cy = self._cell_at(ev.pos())
        ws   = self._workshop
        tool = ws._active_tool if ws else "pencil"
        if tool in ("line", "rect", "rect_fill") and self._line_start:
            if cx >= 0:
                for ccx, ccy in self._shape_cells(tool, self._line_start, (cx, cy)):
                    self._set_cell(ccx, ccy, self._draw_val)
        self._drawing       = False
        self._line_start    = None
        self._preview_cells = set()
        if ws:
            ws._on_grid_changed()
        self.update()

    def wheelEvent(self, ev):
        delta = ev.angleDelta().y()
        if not delta:
            return
        f  = 1.12 if delta > 0 else 1/1.12
        cx, cy = ev.position().x(), ev.position().y()
        old = self._ts()
        self._zoom = max(0.1, min(20.0, self._zoom * f))
        new = self._ts()
        self._pan_x = int(cx - (cx - self._pan_x) * new / max(1, old))
        self._pan_y = int(cy - (cy - self._pan_y) * new / max(1, old))
        self.update()

    def _shape_cells(self, tool, p0, p1):
        x0, y0 = min(p0[0], p1[0]), min(p0[1], p1[1])
        x1, y1 = max(p0[0], p1[0]), max(p0[1], p1[1])
        cells  = set()
        if tool == "line":
            cx0, cy0 = p0
            cx1, cy1 = p1
            dx, dy   = abs(cx1-cx0), abs(cy1-cy0)
            sx, sy   = (1 if cx0<cx1 else -1), (1 if cy0<cy1 else -1)
            err      = dx - dy
            while True:
                cells.add((cx0, cy0))
                if cx0 == cx1 and cy0 == cy1:
                    break
                e2 = 2 * err
                if e2 > -dy: err -= dy; cx0 += sx
                if e2 <  dx: err += dx; cy0 += sy
        elif tool == "rect":
            for x in range(x0, x1+1):
                cells.add((x, y0)); cells.add((x, y1))
            for y in range(y0+1, y1):
                cells.add((x0, y)); cells.add((x1, y))
        elif tool == "rect_fill":
            for y in range(y0, y1+1):
                for x in range(x0, x1+1):
                    cells.add((x, y))
        return cells

    def _flood_fill(self, cx, cy, val):
        target  = self._cell_val(cx, cy)
        if target == val:
            return
        gw      = self._grid_w
        stack   = [(cx, cy)]
        visited = set()
        while stack:
            x, y = stack.pop()
            if (x, y) in visited:
                continue
            if not (0 <= x < gw and 0 <= y < gw):
                continue
            if self._grid[y*gw+x] != target:
                continue
            visited.add((x, y))
            self._grid[y*gw+x] = val
            stack.extend([(x+1,y),(x-1,y),(x,y+1),(x,y-1)])

    def fit(self):
        self._zoom  = 1.0
        self._pan_x = self._pan_y = 0
        self.update()


class SaWaterCanvas(QWidget):
    quad_selected = pyqtSignal(int)

    COL_WATER  = QColor(30, 120, 220, 160)
    COL_SEL    = QColor(255, 200,   0, 220)
    COL_BORDER = QColor(80,  160, 255, 200)
    COL_BG     = QColor(20,   20,  20)

    def __init__(self, parent=None):
        super().__init__(parent)
        self._quads     = []
        self._sel_idx   = -1
        self._zoom      = 1.0
        self._pan_x     = 0
        self._pan_y     = 0
        self._bbox      = (-3000, -3000, 3000, 3000)
        self._pan_drag  = None
        self._pan_start = (0, 0)
        self.setMouseTracking(True)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)

    def setup(self, quads, bbox):
        self._quads   = quads
        self._bbox    = bbox
        self._sel_idx = -1
        self._fit()
        self.update()

    def _fit(self):
        if not self._quads:
            return
        x0, y0, x1, y1 = self._bbox
        ww, wh = x1-x0, y1-y0
        if ww <= 0 or wh <= 0:
            return
        self._zoom  = min(self.width()/ww, self.height()/wh) * 0.9
        self._pan_x = int(self.width()  / 2 - (x0 + ww/2) * self._zoom)
        self._pan_y = int(self.height() / 2 - (y0 + wh/2) * self._zoom)

    def _w2s(self, wx, wy):
        return int(wx * self._zoom + self._pan_x), int(wy * self._zoom + self._pan_y)

    def paintEvent(self, ev):
        p = QPainter(self)
        p.fillRect(self.rect(), self.COL_BG)
        if not self._quads:
            p.setPen(QColor(100, 100, 100))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter,
                       "No SA water quads — load SA water.dat")
            p.end()
            return
        for idx, q in enumerate(self._quads):
            # Sort corners by angle from centroid to avoid self-intersecting quads
            cx_avg = sum(c["x"] for c in q["corners"]) / 4
            cy_avg = sum(c["y"] for c in q["corners"]) / 4
            import math as _m
            sorted_corners = sorted(q["corners"],
                key=lambda c: _m.atan2(c["y"] - cy_avg, c["x"] - cx_avg))
            pts = [QPoint(*self._w2s(c["x"], c["y"])) for c in sorted_corners]
            sel = (idx == self._sel_idx)
            p.setPen(QPen(self.COL_SEL if sel else self.COL_BORDER, 2 if sel else 1))
            p.setBrush(self.COL_SEL.lighter(120) if sel else self.COL_WATER)
            p.drawPolygon(QPolygon(pts))
        p.end()

    def mousePressEvent(self, ev):
        if ev.button() == Qt.MouseButton.MiddleButton:
            self._pan_drag  = ev.pos()
            self._pan_start = (self._pan_x, self._pan_y)
            return
        if ev.button() != Qt.MouseButton.LeftButton:
            return
        wx = (ev.pos().x() - self._pan_x) / self._zoom
        wy = (ev.pos().y() - self._pan_y) / self._zoom
        for idx, q in enumerate(self._quads):
            xs = [c["x"] for c in q["corners"]]
            ys = [c["y"] for c in q["corners"]]
            if min(xs) <= wx <= max(xs) and min(ys) <= wy <= max(ys):
                self._sel_idx = idx
                self.quad_selected.emit(idx)
                self.update()
                return
        self._sel_idx = -1
        self.update()

    def mouseMoveEvent(self, ev):
        if self._pan_drag and ev.buttons() & Qt.MouseButton.MiddleButton:
            dx = ev.pos().x() - self._pan_drag.x()
            dy = ev.pos().y() - self._pan_drag.y()
            self._pan_x = self._pan_start[0] + dx
            self._pan_y = self._pan_start[1] + dy
            self.update()

    def mouseReleaseEvent(self, ev):
        self._pan_drag = None

    def wheelEvent(self, ev):
        f = 1.12 if ev.angleDelta().y() > 0 else 1/1.12
        self._zoom = max(0.01, min(50.0, self._zoom * f))
        self.update()

    def fit(self):
        self._fit()
        self.update()


# =============================================================================
# SECTION 3 - WaterWorkshop: GUIWorkshop subclass
# =============================================================================

class WaterWorkshop(GUIWorkshop):

    App_name        = "Water Workshop"
    App_build       = Build
    App_author      = "X-Seti"
    App_year        = "2026"
    App_description = ("GTA III / VC / PS2 LC / SOL - waterpro.dat + water.dat\n"
                       "GTA SA - water.dat / water1.dat (quad format)")
    config_key      = "water_workshop"

    def __init__(self, parent=None, main_window=None):
        self._waterpro    = None
        self._waterdat    = None
        self._sa_water    = None
        self._file_path   = ""
        self._file_type   = ""
        self._undo_phys   = []
        self._undo_vis    = []
        self._redo_phys   = []
        self._redo_vis    = []
        self._active_grid = "phys"
        self._grid_offset_x = 0
        self._grid_offset_y = 0
        self._grid_offset_z = 0.0
        super().__init__(parent, main_window)
        # Set anchor icon
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVG
            self.setWindowIcon(_SVG.water_workshop_icon(64))
        except Exception:
            pass

    def _build_menus_into_qmenu(self, pm):
        fm = pm.addMenu("File")
        fm.addAction("Load  Ctrl+O",         self._open_file)
        fm.addAction("Save  Ctrl+S",         self._save_file)
        fm.addSeparator()
        fm.addAction("Export Grids as BMP",  self._export_bmp)
        fm.addAction("Import BMP Grid",      self._import_bmp)
        fm.addSeparator()
        recent = self.WS.get_recent()
        if recent:
            rm = fm.addMenu("Recent Files")
            for rp in recent:
                act = rm.addAction(Path(rp).name)
                act.setToolTip(rp)
                act.triggered.connect(lambda checked=False, p=rp: self._load_file(p))
            rm.addSeparator()
            rm.addAction("Clear Recent", self._clear_recent)
        em = pm.addMenu("Edit")
        em.addAction("Undo  Ctrl+Z",         self._undo)
        em.addAction("Redo  Ctrl+Y",         self._redo)
        em.addSeparator()
        em.addAction("Clear Active Grid",    self._erase_all_grid)
        em.addAction("Invert Active Grid",   self._invert_grid)
        em.addAction("Water Statistics",     self._show_stats)
        vm = pm.addMenu("View")
        vm.addAction("Zoom In  +",           lambda: self._zoom(1.25))
        vm.addAction("Zoom Out  -",          lambda: self._zoom(0.8))
        vm.addAction("Fit  Ctrl+0",          self._fit)
        vm.addSeparator()
        vm.addAction("Toggle Grid Lines",    self._toggle_grid)
        vm.addAction("Flip Display Colours", self._flip_colours)
        vm.addAction("Grid Offset / Shift…", self._show_offset_dialog)
        vm.addSeparator()
        vm.addAction("About Water Workshop", self._show_about)

    def _create_left_panel(self):
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ll  = QVBoxLayout(panel)
        ll.setContentsMargins(*self.get_panel_margins())
        hdr = QLabel("Water Levels / Rects")
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        hdr.setFont(self.panel_font)
        hdr.setStyleSheet("font-weight:bold; padding:2px;")
        ll.addWidget(hdr)
        self._levels_list = QListWidget()
        self._levels_list.setAlternatingRowColors(True)
        self._levels_list.itemDoubleClicked.connect(self._edit_level)
        ll.addWidget(self._levels_list)
        br = QHBoxLayout()
        self._add_btn = QPushButton("+ Level")
        self._del_btn = QPushButton("- Remove")
        self._add_btn.clicked.connect(self._add_level)
        self._del_btn.clicked.connect(self._del_level)
        br.addWidget(self._add_btn)
        br.addWidget(self._del_btn)
        ll.addLayout(br)
        sep = QFrame()
        sep.setFrameShape(QFrame.Shape.HLine)
        ll.addWidget(sep)
        self._dirty_lbl = QLabel("Modified: no")
        self._dirty_lbl.setFont(self.infobar_font)
        ll.addWidget(self._dirty_lbl)
        return panel

    def _create_centre_panel(self):
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel)
        cl.setContentsMargins(0, 0, 0, 0)
        cl.setSpacing(0)
        self._view_tabs = QTabWidget()
        self._view_tabs.setDocumentMode(True)
        self._view_tabs.currentChanged.connect(self._on_tab_changed)
        self._phys_canvas = WaterGridWidget()
        self._phys_canvas._workshop = self
        sc1 = QScrollArea()
        sc1.setWidget(self._phys_canvas)
        sc1.setWidgetResizable(True)
        self._view_tabs.addTab(sc1, "Physical (64x64)")
        self._vis_canvas = WaterGridWidget()
        self._vis_canvas._workshop = self
        sc2 = QScrollArea()
        sc2.setWidget(self._vis_canvas)
        sc2.setWidgetResizable(True)
        self._view_tabs.addTab(sc2, "Visible (128x128)")
        self._sa_canvas = SaWaterCanvas()
        self._sa_canvas.quad_selected.connect(self._on_quad_selected)
        self._view_tabs.addTab(self._sa_canvas, "SA Quads")
        cl.addWidget(self._view_tabs)
        return panel

    def _populate_sidebar(self):
        sl  = self._sidebar_layout
        ic  = self._get_icon_color()
        BTN = 36

        def _nb(icon_fn, tip, slot, checkable=False):
            b = QToolButton()
            b.setFixedSize(BTN, BTN)
            try:
                b.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
            except Exception:
                b.setText(tip[:2])
            b.setToolTip(tip)
            b.setCheckable(checkable)
            b.clicked.connect(slot)
            return b

        def _row(*btns):
            row = QHBoxLayout()
            row.setSpacing(2)
            row.setContentsMargins(0, 0, 0, 0)
            for b in btns:
                row.addWidget(b)
            if len(btns) == 1:
                row.addStretch()
            sl.addLayout(row)

        def _sep():
            s = QFrame()
            s.setFrameShape(QFrame.Shape.HLine)
            sl.addSpacing(2)
            sl.addWidget(s)
            sl.addSpacing(2)

        def _tool(icon_fn, tip, name):
            b = _nb(icon_fn, tip,
                    lambda checked=False, t=name: self._set_active_tool(t),
                    checkable=True)
            self._draw_btns[name] = b
            return b

        _row(_nb("zoom_in_icon",  "Zoom in (+)",     lambda: self._zoom(1.25)),
             _nb("zoom_out_icon", "Zoom out (-)",    lambda: self._zoom(0.8)))
        _row(_nb("fit_grid_icon", "Fit  Ctrl+0",     self._fit),
             _nb("locate_icon",   "Toggle grid",     self._toggle_grid))
        _sep()
        _row(_tool("paint_icon",     "Pencil (P)",           "pencil"),
             _tool("fill_icon",      "Flood fill (F)",       "fill"))
        _row(_tool("line_icon",      "Line (L)",             "line"),
             _tool("rect_icon",      "Rect outline (R)",     "rect"))
        _row(_tool("rect_fill_icon", "Filled rect (Shift+R)","rect_fill"),
             _tool("dropper_icon",   "Dropper (K)",          "picker"))
        _row(_tool("zoom_in_icon",   "Zoom tool (Z)",        "zoom"),
             _nb("scissors_icon",    "Erase all",            self._erase_all_grid))
        _row(_nb("rotate_cw_icon",   "Invert grid data",     self._invert_grid),
             _nb("flip_horz_icon",   "Flip display colours", self._flip_colours))
        _row(_nb("search_icon",      "Statistics",           self._show_stats),
             _nb("locate_icon",      "Grid offset / shift",  self._show_offset_dialog))
        _sep()
        sl.addWidget(QLabel("L=Sea  R=Land", alignment=Qt.AlignmentFlag.AlignCenter))
        wf = QFrame()
        wf.setFixedSize(74, 14)
        wf.setStyleSheet("background:#1e78dc; border:1px solid #555;")
        df = QFrame()
        df.setFixedSize(74, 14)
        df.setStyleSheet("background:#3d2b0f; border:1px solid #555;")  # land = brown
        sl.addWidget(wf)
        sl.addWidget(df)
        if "pencil" in self._draw_btns:
            self._draw_btns["pencil"].setChecked(True)
            self._active_tool = "pencil"


# =============================================================================
# SECTION 4 - Water logic
# =============================================================================

    def _on_grid_changed(self):
        self._dirty_lbl.setText("Modified: yes")
        self.save_btn.setEnabled(True)
        for c in (self._phys_canvas, self._vis_canvas):
            if hasattr(c, "_img_cache"): del c._img_cache

    def _on_tab_changed(self, idx: int):
        self._active_grid = "phys" if idx == 0 else "vis"

    def _active_canvas(self):
        return self._phys_canvas if self._active_grid == "phys" else self._vis_canvas

    def _push_undo_grid(self):
        if self._active_grid == "phys" and self._waterpro:
            self._undo_phys.append(bytearray(self._waterpro.phys_grid))
            if len(self._undo_phys) > 20:
                self._undo_phys.pop(0)
            self._redo_phys.clear()
        elif self._active_grid == "vis" and self._waterpro:
            self._undo_vis.append(bytearray(self._waterpro.vis_grid))
            if len(self._undo_vis) > 20:
                self._undo_vis.pop(0)
            self._redo_vis.clear()

    def _undo(self):
        if self._active_grid == "phys":
            s, r, attr, canvas = self._undo_phys, self._redo_phys, "phys_grid", self._phys_canvas
        else:
            s, r, attr, canvas = self._undo_vis,  self._redo_vis,  "vis_grid",  self._vis_canvas
        if not s:
            self._set_status("Nothing to undo")
            return
        if self._waterpro:
            r.append(bytearray(getattr(self._waterpro, attr)))
            setattr(self._waterpro, attr, s.pop())
            canvas.set_grid(getattr(self._waterpro, attr))
        self._set_status("Undo")

    def _redo(self):
        if self._active_grid == "phys":
            s, u, attr, canvas = self._redo_phys, self._undo_phys, "phys_grid", self._phys_canvas
        else:
            s, u, attr, canvas = self._redo_vis,  self._undo_vis,  "vis_grid",  self._vis_canvas
        if not s:
            self._set_status("Nothing to redo")
            return
        if self._waterpro:
            u.append(bytearray(getattr(self._waterpro, attr)))
            setattr(self._waterpro, attr, s.pop())
            canvas.set_grid(getattr(self._waterpro, attr))
        self._set_status("Redo")

    def _zoom(self, f):
        for c in (self._phys_canvas, self._vis_canvas):
            c._zoom = max(0.1, min(20.0, c._zoom * f))
            c.update()

    def _fit(self):
        for c in (self._phys_canvas, self._vis_canvas):
            c.fit()
        self._sa_canvas.fit()

    def _toggle_grid(self):
        for c in (self._phys_canvas, self._vis_canvas):
            c._show_grid = not c._show_grid
            c.update()

    def _flip_colours(self):
        for c in (self._phys_canvas, self._vis_canvas):
            c._colour_flipped = not c._colour_flipped
            c.update()
        state = "flipped" if self._phys_canvas._colour_flipped else "normal"
        self._set_status(f"Colour display: {state}  (data unchanged)")

    def _set_active_tool(self, tool: str):
        self._active_tool = tool
        for name, btn in self._draw_btns.items():
            btn.setChecked(name == tool)

    def _erase_all_grid(self):
        canvas = self._active_canvas()
        if not canvas._grid_w:
            return
        if QMessageBox.question(self, "Clear Grid",
                "Clear entire active grid to Dry?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                ) != QMessageBox.StandardButton.Yes:
            return
        self._push_undo_grid()
        canvas._grid[:] = bytearray(len(canvas._grid))  # fill with 0 = open water
        canvas.update()
        self._on_grid_changed()

    def _invert_grid(self):
        canvas = self._active_canvas()
        if not canvas._grid_w:
            return
        self._push_undo_grid()
        for i in range(len(canvas._grid)):
            canvas._grid[i] = 128 if canvas._grid[i] == 0 else 0
        canvas.update()
        self._on_grid_changed()

    def _is_binary_waterpro(self, data):
        if len(data) < 100:
            return False
        rem = len(data) - 964
        if rem <= 0 or rem % 5 != 0:
            return False
        gw = int(math.isqrt(rem // 5))
        return gw * gw == rem // 5

    def _is_sa_water(self, data):
        try:
            return data[:9].decode("latin1") == "processed"
        except Exception:
            return False

    def _open_file(self, path=None):
        if not path:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open Water File", "",
                "Water Files (*.dat *.DAT);;All Files (*)")
        if path:
            self._load_file(path)

    def _load_file(self, path):
        try:
            data = Path(path).read_bytes()
            if self._is_binary_waterpro(data):
                self._load_waterpro(path, data)
            elif self._is_sa_water(data):
                self._load_sa_water(path)
            else:
                self._load_waterdat(path)
            self.WS.add_recent(str(path))
            self._file_path = str(path)
            self._dirty_lbl.setText("Modified: no")
            self.save_btn.setEnabled(False)
        except Exception as e:
            import traceback
            QMessageBox.critical(self, "Load Error",
                f"Failed to load {Path(path).name}:\n{e}\n\n{traceback.format_exc()[-400:]}")

    def _load_waterpro(self, path, data):
        wp = WaterproParser()
        wp.load(path)
        self._waterpro  = wp
        self._waterdat  = self._sa_water = None
        self._file_type = "waterpro"
        gw = wp.grid_w
        self._phys_canvas.setup(gw, wp.phys_grid)
        self._vis_canvas.setup(gw*2, wp.vis_grid)
        self._view_tabs.setTabText(0, f"Physical ({gw}x{gw})")
        self._view_tabs.setTabText(1, f"Visible ({gw*2}x{gw*2})")
        for i, e in enumerate([True, True, False]):
            self._view_tabs.setTabEnabled(i, e)
        self._view_tabs.setCurrentIndex(0)
        self._refresh_levels_list()
        nw = sum(1 for b in wp.phys_grid if b == 128)
        nv = sum(1 for b in wp.vis_grid  if b == 128)
        self._set_status(
            f"Loaded {Path(path).name}  |  {gw}x{gw} physical ({nw} water cells)"
            f"  |  {gw*2}x{gw*2} visible ({nv} water cells)"
            f"  |  {wp.water_levels_count} level(s)")

    def _load_waterdat(self, path):
        wd = WaterDatParser()
        wd.load(path)
        self._waterdat  = wd
        self._waterpro  = self._sa_water = None
        self._file_type = "waterdat"
        for i in range(3):
            self._view_tabs.setTabEnabled(i, False)
        self._levels_list.clear()
        for i, r in enumerate(wd.rects):
            self._levels_list.addItem(QListWidgetItem(
                f"[{i}] Z={r[0]:.1f}  ({r[1]:.0f},{r[2]:.0f}) to ({r[3]:.0f},{r[4]:.0f})"))
        self._set_status(
            f"Loaded {Path(path).name}  |  {len(wd.rects)} rectangle(s)  (text)")

    def _load_sa_water(self, path):
        sa = SaWaterParser()
        sa.load(path)
        self._sa_water  = sa
        self._waterpro  = self._waterdat = None
        self._file_type = "sa_water"
        for i, e in enumerate([False, False, True]):
            self._view_tabs.setTabEnabled(i, e)
        self._view_tabs.setCurrentIndex(2)
        self._sa_canvas.setup(sa.quads, sa.world_bbox())
        self._levels_list.clear()
        for i, q in enumerate(sa.quads):
            c = q["corners"][0]
            self._levels_list.addItem(QListWidgetItem(
                f"[{i}] ({c['x']:.0f},{c['y']:.0f})  flag={q['flag']}"))
        self._set_status(
            f"Loaded {Path(path).name}  |  {len(sa.quads)} SA water quads")

    def _refresh_levels_list(self):
        self._levels_list.clear()
        if not self._waterpro:
            return
        wp = self._waterpro
        for i in range(wp.water_levels_count):
            self._levels_list.addItem(QListWidgetItem(
                f"Level {i}:  Z = {wp.water_level_data[i]:.4f}"))

    def _save_file(self):
        if not self._file_path:
            self._set_status("No file loaded")
            return
        if self._file_type == "waterpro" and self._waterpro:
            p, _ = QFileDialog.getSaveFileName(
                self, "Save waterpro.dat", self._file_path,
                "DAT Files (*.dat *.DAT);;All Files (*)")
            if not p:
                return
            try:
                self._waterpro.save(p)
                self._dirty_lbl.setText("Modified: no")
                self.save_btn.setEnabled(False)
                self._set_status(f"Saved to {Path(p).name}")
            except Exception as e:
                QMessageBox.critical(self, "Save Error", str(e))
        elif self._file_type == "waterdat" and self._waterdat:
            p, _ = QFileDialog.getSaveFileName(
                self, "Save water.dat", self._file_path, "DAT Files (*.dat)")
            if p:
                try:
                    self._waterdat.save(p)
                    self._set_status(f"Saved to {Path(p).name}")
                except Exception as e:
                    QMessageBox.critical(self, "Save Error", str(e))
        elif self._file_type == "sa_water" and self._sa_water:
            p, _ = QFileDialog.getSaveFileName(
                self, "Save SA water.dat", self._file_path, "DAT Files (*.dat)")
            if p:
                try:
                    self._sa_water.save(p)
                    self._set_status(f"Saved to {Path(p).name}")
                except Exception as e:
                    QMessageBox.critical(self, "Save Error", str(e))

    def _export_file(self): self._export_bmp()
    def _import_file(self): self._import_bmp()

    def _export_bmp(self):
        if not self._waterpro:
            QMessageBox.information(self, "Export", "Load a binary waterpro.dat first.")
            return
        wp   = self._waterpro
        gw   = wp.grid_w
        stem = Path(self._file_path).stem if self._file_path else "water"
        try:
            from PIL import Image
            def g2i(grid, w, h):
                img = Image.new("L", (w, h), 0)
                for i in range(w*h):
                    img.putpixel((i%w, i//w), 128 if grid[i] == 128 else 0)
                return img.rotate(90)
            for grid, dflt, w, h in [
                (wp.phys_grid, f"{stem}_physical.bmp", gw,   gw),
                (wp.vis_grid,  f"{stem}_visible.bmp",  gw*2, gw*2),
            ]:
                p, _ = QFileDialog.getSaveFileName(self, "Export BMP", dflt, "BMP (*.bmp)")
                if p:
                    g2i(grid, w, h).save(p)
                    self._set_status(f"Exported {Path(p).name}")
        except Exception as e:
            QMessageBox.critical(self, "Export Error", str(e))

    def _import_bmp(self):
        if not self._waterpro:
            QMessageBox.information(self, "Import", "Load a binary waterpro.dat first.")
            return
        wp = self._waterpro
        gw = wp.grid_w
        p, _ = QFileDialog.getOpenFileName(
            self, "Import Grid BMP", "", "Images (*.bmp *.png)")
        if not p:
            return
        try:
            from PIL import Image
            img    = Image.open(p).convert("L")
            w, h   = img.size
            if w == gw and h == gw:
                target = "phys"
            elif w == gw*2 and h == gw*2:
                target = "vis"
            else:
                QMessageBox.warning(self, "Size Mismatch",
                    f"{w}x{h} doesn't match {gw}x{gw} or {gw*2}x{gw*2}")
                return
            img  = img.rotate(-90)
            grid = bytearray(128 if img.getpixel((i%w, i//w)) > 64 else 0
                             for i in range(w*h))
            self._push_undo_grid()
            if target == "phys":
                wp.phys_grid = grid
                self._phys_canvas.set_grid(grid)
            else:
                wp.vis_grid = grid
                self._vis_canvas.set_grid(grid)
            self._on_grid_changed()
            self._set_status(f"Imported {target} grid from {Path(p).name}")
        except Exception as e:
            QMessageBox.critical(self, "Import Error", str(e))

    def _edit_level(self, item):
        if not self._waterpro:
            return
        idx = self._levels_list.row(item)
        wp  = self._waterpro
        if idx < 0 or idx >= wp.water_levels_count:
            return
        dlg  = QDialog(self)
        dlg.setWindowTitle(f"Edit Water Level {idx}")
        _apply_dialog_theme(dlg, self.main_window)
        fl   = QFormLayout(dlg)
        spin = QDoubleSpinBox()
        spin.setRange(-1000, 1000)
        spin.setDecimals(4)
        spin.setValue(wp.water_level_data[idx])
        fl.addRow(f"Level {idx} Z height:", spin)
        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok |
            QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        fl.addRow(btns)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            wp.water_level_data[idx] = spin.value()
            self._refresh_levels_list()
            self._on_grid_changed()

    def _add_level(self):
        if not self._waterpro:
            return
        wp = self._waterpro
        if wp.water_levels_count >= WaterproParser.WATER_LEVELS:
            QMessageBox.information(self, "At Maximum",
                f"Maximum {WaterproParser.WATER_LEVELS} levels.")
            return
        wp.water_level_data[wp.water_levels_count] = 0.0
        wp.water_levels_count += 1
        self._refresh_levels_list()
        self._on_grid_changed()

    def _del_level(self):
        if not self._waterpro:
            return
        wp  = self._waterpro
        idx = self._levels_list.currentRow()
        if idx < 0 or idx >= wp.water_levels_count:
            return
        wp.water_level_data[idx:wp.water_levels_count-1] = \
            wp.water_level_data[idx+1:wp.water_levels_count]
        wp.water_level_data[wp.water_levels_count-1] = 0.0
        wp.water_levels_count -= 1
        self._refresh_levels_list()
        self._on_grid_changed()

    def _on_quad_selected(self, idx):
        if not self._sa_water:
            return
        self._levels_list.setCurrentRow(idx)
        q    = self._sa_water.quads[idx]
        c    = q["corners"][0]
        vals = ", ".join(f"{v:.3f}" for v in c["f"])
        self._set_status(
            f"Quad {idx}: ({c['x']:.1f},{c['y']:.1f})"
            f"  flag={q['flag']}  f=[{vals}]")

    def _show_stats(self):
        if self._sa_water and not self._waterpro:
            q     = self._sa_water.quads
            b     = self._sa_water.world_bbox()
            flags = sorted(set(x["flag"] for x in q))
            QMessageBox.information(self, "SA Water Statistics",
                f"Quads:       {len(q)}\n"
                f"World bbox:  ({b[0]:.0f},{b[1]:.0f}) to ({b[2]:.0f},{b[3]:.0f})\n"
                f"Flags used:  {flags}")
            return
        if not self._waterpro:
            QMessageBox.information(self, "Stats", "Load a waterpro.dat first.")
            return
        wp      = self._waterpro
        gw      = wp.grid_w
        pw      = sum(1 for b in wp.phys_grid if b == 128)
        vw      = sum(1 for b in wp.vis_grid  if b == 128)
        tot_p   = gw * gw
        tot_v   = (gw*2) ** 2
        heights = [round(wp.water_level_data[i], 2)
                   for i in range(wp.water_levels_count)]
        QMessageBox.information(self, "Water Statistics",
            f"Grid width:    {gw}\n"
            f"Physical grid: {gw}x{gw} = {tot_p} cells\n"
            f"  Water:       {pw} ({100*pw//tot_p}%)\n"
            f"  Dry:         {tot_p-pw}\n\n"
            f"Visible grid:  {gw*2}x{gw*2} = {tot_v} cells\n"
            f"  Water:       {vw} ({100*vw//tot_v}%)\n"
            f"  Dry:         {tot_v-vw}\n\n"
            f"Water levels:  {wp.water_levels_count}\n"
            f"  Heights:     {heights}")

    def _clear_recent(self):
        self.WS._data["recent_files"] = []
        self.WS.save()
        self._set_status("Recent files cleared")

    def _show_offset_dialog(self):
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QFormLayout, QSpinBox, QDoubleSpinBox, QDialogButtonBox, QGroupBox, QLabel
        dlg = QDialog(self)
        dlg.setWindowTitle("Grid Offset / Shift")
        _apply_dialog_theme(dlg, self.main_window)
        lo  = QVBoxLayout(dlg)
        grp = QGroupBox("World coordinate offset (applied on save)")
        fl  = QFormLayout(grp)
        sx = QSpinBox(); sx.setRange(-8192, 8192); sx.setValue(self._grid_offset_x); sx.setSuffix(" units")
        sy = QSpinBox(); sy.setRange(-8192, 8192); sy.setValue(self._grid_offset_y); sy.setSuffix(" units")
        sz = QDoubleSpinBox(); sz.setRange(-500, 500); sz.setDecimals(3); sz.setValue(self._grid_offset_z); sz.setSuffix("m")
        fl.addRow("X offset:", sx)
        fl.addRow("Y offset:", sy)
        fl.addRow("Z offset (all levels):", sz)
        lo.addWidget(grp)
        lo.addWidget(QLabel("Tip: ±2000 units shifts water table by one map sector.", styleSheet="color:#888;font-style:italic;"))
        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept); btns.rejected.connect(dlg.reject)
        lo.addWidget(btns)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        self._grid_offset_x = sx.value()
        self._grid_offset_y = sy.value()
        self._grid_offset_z = sz.value()
        # Apply Z offset to all water level heights
        if self._grid_offset_z != 0 and self._waterpro:
            for i in range(self._waterpro.water_levels_count):
                self._waterpro.water_level_data[i] += self._grid_offset_z
            self._grid_offset_z = 0
            self._refresh_levels_list()
            self._on_grid_changed()
        self._set_status(f"Grid offset: X={self._grid_offset_x} Y={self._grid_offset_y}")


# =============================================================================
# Standalone launcher
# =============================================================================

if __name__ == "__main__":
    import traceback
    print(f"{App_name} {Build} starting...")
    try:
        app = QApplication(sys.argv)
        w   = WaterWorkshop()
        w.setWindowTitle(f"{App_name} - Standalone")
        w.resize(1300, 800)
        w.show()
        if len(sys.argv) > 1 and Path(sys.argv[1]).is_file():
            w._load_file(sys.argv[1])
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)
