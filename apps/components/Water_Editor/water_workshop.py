#!/usr/bin/env python3
#this belongs in apps/components/Water_Editor/water_workshop.py - Version: 25
# X-Seti - Apr 2026 - IMG Factory 1.6 - Water Workshop
# Built on temp_workshop.py / GUIWorkshop base

# Section 1: Parsers (WaterproParser, WaterDatParser, SaWaterParser)
# Section 2: Canvas widgets (WaterGridWidget, SaWaterCanvas)
# Section 3: WaterWorkshop(GUIWorkshop) - panel overrides + menus
# Section 4: Water logic - open/save/draw/undo/levels
##Methods list -
# _apply_dialog_theme
# _atomic_write
#
##class WaterproParser: -
# __init__
# load
# save
#
##class WaterDatParser: -
# __init__
# _fmt
# load
# save
# to_text
# translate_all
# world_bbox
#
##class SaWaterParser: -
# __init__
# _fmt
# _is_quad_line
# _sig
# is_iv
# load
# save
# to_text
# translate_all
# world_bbox
#
##class WaterGridWidget: -
# __init__
# _base_ts
# _cell_at
# _cell_col
# _cell_val
# _depth_col
# _flood_fill
# _get_ui_color
# _grid_idx
# _levels_z
# _rebuild_cache
# _screen_index
# _set_cell
# _shape_cells
# _ts
# fit
# minimumSizeHint
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# paintEvent
# selected_z
# set_grid
# set_selected_z
# setup
# sizeHint
# wheelEvent
# z_range
# z_values
# zoom_max
#
##class WaterObjectCanvas: -
# __init__
# _depth_col
# _fit
# _get_ui_color
# _hit_corner
# _hit_obj
# _move_corner
# _ordered
# _s2w
# _snap
# _view_rect
# _w2s
# _z
# _z_range
# fit
# keyPressEvent
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# move_selected
# paintEvent
# selected_z
# selection
# set_selected_z
# set_selection
# setup
# sizeHint
# wheelEvent
# z_range
# z_values
#
##class WaterZGauge: -
# __init__
# _bar
# _pos_of
# _t_of
# _z_at
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# paintEvent
# set_horizontal
#
##class WaterWorkshop: -
# __init__
# _active_canvas
# _add_level
# _bmp_index
# _build_menus_into_qmenu
# _build_ribbons
# _clear_recent
# _create_centre_panel
# _create_left_panel
# _create_toolbar
# _current_parser
# _del_level
# _edit_level
# _erase_all_grid
# _export_bmp
# _export_file
# _fit
# _flip_colours
# _import_bmp
# _import_file
# _invert_grid
# _is_binary_waterpro
# _is_sa_water
# _load_file
# _load_sa_water
# _load_waterdat
# _load_waterpro
# _mark_dirty
# _obj_add
# _obj_delete
# _obj_edit_started
# _obj_edited
# _obj_refresh
# _obj_set_objs
# _obj_set_snap
# _obj_set_z
# _obj_undo_redo
# _objs_to_rects
# _on_grid_changed
# _on_list_row
# _on_quad_selected
# _on_tab_changed
# _open_file
# _push_undo_grid
# _rects_to_objs
# _redo
# _refresh_levels_list
# _save_file
# _save_file_as
# _set_active_tool
# _set_depth_view
# _set_obj_tool
# _show_offset_dialog
# _show_stats
# _spin_row
# _toggle_grid
# _undo
# _water_objs
# _write_water
# _zoom
# closeEvent
# get_water_quads
# get_water_rects
# setup_ui
# translate_water

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

import sys
from pathlib import Path
# Add project root so apps.* imports resolve when run standalone
_root = Path(__file__).resolve().parents[3]  # apps/components/Water_Editor -> project root
if str(_root) not in sys.path: sys.path.insert(0, str(_root))

from apps.methods.gui_workshop import GUIWorkshop
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.ribbon_system import RibbonMixin

App_name  = "Water Workshop"
App_build = "Apr 2026"
Build     = "Build 10"

# SECTION 1 - Parsers

def _apply_dialog_theme(dlg, mw): #vers 1
        try:
            from apps.core.theme_utils import apply_dialog_theme
            apply_dialog_theme(dlg, mw)
        except Exception:
            pass

def _atomic_write(path, data: bytes): #vers 1
    """Write to a temp file in the same folder then swap it in, so a
    crash or full disk can never leave a half-written water file."""
    import os, tempfile
    d = os.path.dirname(os.path.abspath(path))
    fd, tmp = tempfile.mkstemp(dir=d, prefix=".water_", suffix=".tmp")
    try:
        with os.fdopen(fd, "wb") as f:
            f.write(data)
        if os.path.exists(path):
            try:
                os.chmod(tmp, os.stat(path).st_mode & 0o7777)
            except OSError:
                pass
        os.replace(tmp, path)
    except Exception:
        if os.path.exists(tmp):
            os.unlink(tmp)
        raise


class WaterproParser:
    HEADER_SIZE  = 964
    WATER_LEVELS = 48
    # SOL tile layout (from WaterHack.cpp)
    WATER_GRID_WIDTH = 32   # cells per tile dimension
    MAP_TILE_WIDTH   = 6    # 6x6 tiles
    VIS_TILE_SIZE    = 4    # bytes per cell in visible grid  (4*32*32 = 4096 per tile)
    PHYS_TILE_SIZE   = 16   # bytes per cell in physical grid (16*32*32 = 16384 per tile)

    def __init__(self):
        self.water_levels_count = 0
        self.water_level_data   = [0.0] * self.WATER_LEVELS
        self.unk_block          = bytes(768)
        self.grid_w             = 0
        self.vis_grid           = bytearray()
        self.phys_grid          = bytearray()
        self.path               = None

    def load(self, path: str): #vers 2
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
        # First block (gw*gw bytes) = visible grid (4*32*32 per tile)
        self.vis_grid           = bytearray(data[964 : 964 + gw*gw])
        # Second block (2*gw * 2*gw bytes) = physical grid (16*32*32 per tile)
        self.phys_grid          = bytearray(data[964+gw*gw : 964+gw*gw+(2*gw)*(2*gw)])
        self.path               = path

    def save(self, path: str): #vers 2
        gw  = self.grid_w
        out = bytearray(self.HEADER_SIZE + gw*gw + (2*gw)*(2*gw))
        out[0] = self.water_levels_count & 0xFF
        struct.pack_into(f"<{self.WATER_LEVELS}f", out, 4, *self.water_level_data)
        out[196:964]       = self.unk_block
        out[964:964+gw*gw] = self.vis_grid
        out[964+gw*gw:]    = self.phys_grid
        _atomic_write(path, bytes(out))


class WaterDatParser:
    """water.dat (GTA III/VC/LCS text rects). Keeps every original line
    (comments, spacing, CRLF) so an untouched file saves byte-identical
    and only edited/added/removed rows change."""
    def __init__(self): #vers 2
        self.rects  = []
        self.header = ""
        self.path   = None
        self._eol   = "\r\n"
        self._lines = []
        self._row_of = {}
        self._orig_rects = []
        self._end_idx = None

    def load(self, path: str): #vers 2
        self.path  = path
        self.rects = []
        raw = Path(path).read_bytes().decode("latin1")
        self._eol   = "\r\n" if "\r\n" in raw else "\n"
        self._lines = raw.split(self._eol)
        self._row_of, self._end_idx = {}, None
        hdr, in_hdr = [], True
        for i, line in enumerate(self._lines):
            s = line.strip()
            if s.startswith("*"):
                self._end_idx = i
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
                    self._row_of[i] = len(self.rects) - 1
                except ValueError:
                    pass
        self.header = "\n".join(hdr)
        self._orig_rects = list(self.rects)

    @staticmethod
    def _fmt(r): #vers 1
        return f"{r[0]:.4f},\t{r[1]:.4f},\t{r[2]:.4f},\t{r[3]:.4f},\t{r[4]:.4f},"

    def to_text(self) -> str: #vers 1
        cur, orig = self.rects, self._orig_rects
        same_len  = len(cur) == len(orig)
        remaining = [] if same_len else list(cur)
        out = []
        def _flush_new():
            for r in remaining:
                out.append(self._fmt(r))
            remaining.clear()
        for i, line in enumerate(self._lines):
            if i == self._end_idx:
                _flush_new()
            if i in self._row_of:
                k = self._row_of[i]
                if same_len:
                    out.append(line if cur[k] == orig[k] else self._fmt(cur[k]))
                else:
                    o = orig[k]
                    if o in remaining:
                        remaining.remove(o)
                        out.append(line)
            else:
                out.append(line)
        if not same_len and self._end_idx is None:
            _flush_new()
            out.append("* ;end of file")
        return self._eol.join(out)

    def save(self, path: str): #vers 2
        _atomic_write(path, self.to_text().encode("latin1", errors="replace"))
        self.path = path

    def translate_all(self, dx: float, dy: float, dz: float = 0.0): #vers 2
        """Translate all water rectangles. Rect format: (level, x1, y1, x2, y2)."""
        self.rects = [
            (r[0]+dz, r[1]+dx, r[2]+dy, r[3]+dx, r[4]+dy)
            for r in self.rects
        ]

    def world_bbox(self): #vers 2
        if not self.rects:
            return -3000, -3000, 3000, 3000
        xs = [r[1] for r in self.rects] + [r[3] for r in self.rects]
        ys = [r[2] for r in self.rects] + [r[4] for r in self.rects]
        return min(xs), min(ys), max(xs), max(ys)


class SaWaterParser:
    """SA water.dat / water1.dat quads (4 corners x 7 values, optional
    trailing flag; GTA IV adds one more value). Original lines are kept so
    untouched quads, the "processed" line and any comments save unchanged."""
    def __init__(self): #vers 2
        self.quads = []
        self.path  = None
        self._eol  = "\n"
        self._lines = []

    @staticmethod
    def _sig(q): #vers 2
        return (tuple((c["x"], c["y"], *c["f"]) for c in q["corners"]), q.get("flag"),
                tuple(q.get("extra", ())))

    def load(self, path: str): #vers 4
        """Quads (4 corners) and triangles (3 corners), 7 values per corner."""
        self.path  = path
        self.quads = []
        raw = Path(path).read_bytes().decode("latin1")
        self._eol   = "\r\n" if "\r\n" in raw else "\n"
        self._lines = raw.split(self._eol)
        for i, line in enumerate(self._lines):
            s = line.strip()
            if not s or s.startswith(";") or s.startswith("*") or s == "processed":
                continue
            parts = s.split()
            if len(parts) < 21:
                continue
            n = 4 if len(parts) >= 28 else 3          # quad or triangle
            try:
                corners = [
                    {"x": float(parts[c*7]),
                     "y": float(parts[c*7+1]),
                     "f": [float(parts[c*7+k]) for k in range(2, 7)]}
                    for c in range(n)
                ]
                flag = int(parts[n*7]) if len(parts) > n*7 else None
                extra = [p for p in parts[n*7+1:]]       # GTA IV trailing value
                [float(p) for p in extra]
            except (ValueError, IndexError):
                continue
            q = {"corners": corners, "flag": flag, "extra": extra, "_line": i}
            q["_sig"] = self._sig(q)
            self.quads.append(q)

    @staticmethod
    def _fmt(q): #vers 2
        row = "    ".join(
            f"{c['x']:.4f} {c['y']:.4f} " + " ".join(f"{v:.5f}" for v in c["f"])
            for c in q["corners"])
        row += f"  {q['flag']}" if q.get("flag") is not None else ""
        return row + "".join(f" {v}" for v in q.get("extra", ()))

    def is_iv(self) -> bool: #vers 1
        """GTA IV water.dat: no "processed" line, extra value per quad."""
        return any(q.get("extra") for q in self.quads)

    def to_text(self) -> str: #vers 1
        by_line = {q["_line"]: q for q in self.quads if "_line" in q}
        new = [q for q in self.quads if "_line" not in q]
        out = []
        for i, line in enumerate(self._lines):
            q = by_line.get(i)
            if q is not None:
                out.append(line if self._sig(q) == q["_sig"] else self._fmt(q))
            elif self._is_quad_line(line):
                continue                      # a quad that was deleted
            else:
                out.append(line)
        if new:
            while out and out[-1] == "":
                out.pop()
            out.extend(self._fmt(q) for q in new)
            out.append("")
        return self._eol.join(out)

    @staticmethod
    def _is_quad_line(line): #vers 2
        s = line.strip()
        return bool(s) and not s.startswith((";", "*")) and s != "processed" and len(s.split()) >= 21

    def save(self, path: str): #vers 2
        _atomic_write(path, self.to_text().encode("latin1", errors="replace"))
        self.path = path

    def translate_all(self, dx: float, dy: float, dz: float = 0.0): #vers 1
        """Translate all quad corners by (dx, dy, dz)."""
        for q in self.quads:
            for c in q["corners"]:
                c["x"] += dx
                c["y"] += dy
                # z-like values stored in c["f"][0..4] — shift f[0] as height
                if dz and len(c["f"]) > 0:
                    c["f"] = [c["f"][0]+dz] + c["f"][1:]

    def world_bbox(self): #vers 1
        if not self.quads:
            return -3000, -3000, 3000, 3000
        xs = [c["x"] for q in self.quads for c in q["corners"]]
        ys = [c["y"] for q in self.quads for c in q["corners"]]
        return min(xs), min(ys), max(xs), max(ys)



# SECTION 2 - Canvas widgets

class WaterGridWidget(QWidget):

    def _get_ui_color(self, key): #vers 2
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)
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

    def __init__(self, parent=None): #vers 1
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
        self._depth_mode    = False     # colour water cells by level Z
        self._paint_level   = 0         # level index painted by left-click
        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.WheelFocus)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)


    def setup(self, grid_w: int, grid: bytearray): #vers 1
        self._grid_w = grid_w
        self._grid   = grid
        self._sel_cx = self._sel_cy = -1
        self._zoom   = 1.0
        self._pan_x  = self._pan_y = 0
        if hasattr(self, "_img_cache"): del self._img_cache
        self.update()


    def set_grid(self, grid: bytearray): #vers 1
        self._grid = grid
        if hasattr(self, "_img_cache"): del self._img_cache
        self.update()


    def _levels_z(self): #vers 1
        """Z of each waterpro level (empty for other formats)."""
        wp = getattr(self._workshop, '_waterpro', None) if self._workshop else None
        return list(wp.water_level_data[:wp.water_levels_count]) if wp else []

    def _depth_col(self, z: float, zmin: float, zmax: float) -> QColor: #vers 2
        """Blue shade for a Z: deep/low dark, high light."""
        from apps.methods.ui_color import water_depth_rgb
        return QColor(*water_depth_rgb(0.5 if zmax <= zmin else (z - zmin) / (zmax - zmin)))

    def _cell_col(self, val: int) -> QColor: #vers 3
        levels = getattr(self, '_cache_levels', None)
        if self._depth_mode and levels and val != 128 and val < len(levels):
            return self._depth_col(levels[val], min(levels), max(levels))
        if val == 128:
            return self.COL_WATER if self._colour_flipped else self.COL_DRY
        else:
            return self.COL_DRY if self._colour_flipped else self.COL_WATER


    def _ts(self): #vers 2
        if not self._grid_w:
            return 8
        return max(1, int(self._base_ts() * self._zoom))  # ts=1 allowed for SOL grids

    def _base_ts(self) -> float: #vers 1
        """Cell size at zoom 1 (fit), fractional for grids wider than the view."""
        return max(1e-3, min(self.width(), self.height()) / max(1, self._grid_w))

    def zoom_max(self) -> float: #vers 1
        """Zoom limit: cells up to 64 px, whatever the grid size."""
        return max(20.0, 64.0 / self._base_ts())


    def _cell_at(self, pos): #vers 2
        """Map screen position to data (col, row). No Y-flip."""
        ts     = self._ts()
        ax, ay = pos.x() - self._pan_x, pos.y() - self._pan_y
        if ax < 0 or ay < 0:
            return -1, -1
        cx = ax // ts
        cy = ay // ts
        if 0 <= cx < self._grid_w and 0 <= cy < self._grid_w:
            return cx, cy
        return -1, -1


    def _grid_idx(self, cx, cy): #vers 3
        """Screen cell to byte index; SOL 6x6 tiles shown north-up, [x][y] cells."""
        gw = self._grid_w
        if gw % 6 == 0:
            tile_w = gw // 6
            y = gw - 1 - cy                        # north up, as SOL Water Editor
            tile_idx = (y // tile_w) * 6 + cx // tile_w
            return tile_idx * tile_w * tile_w + (cx % tile_w) * tile_w + y % tile_w
        return cy * gw + cx

    def _cell_val(self, cx, cy): #vers 2
        if 0 <= cx < self._grid_w and 0 <= cy < self._grid_w:
            return self._grid[self._grid_idx(cx, cy)]
        return 0

    def _set_cell(self, cx, cy, val): #vers 2
        if 0 <= cx < self._grid_w and 0 <= cy < self._grid_w:
            self._grid[self._grid_idx(cx, cy)] = val


    def _screen_index(self): #vers 1
        """Grid byte index per screen cell (row-major), cached per width."""
        import numpy as np
        gw = self._grid_w
        cached = getattr(self, '_sidx', None)
        if cached is None or cached[0] != gw:
            if gw % 6 == 0:
                cy, cx = np.mgrid[0:gw, 0:gw]
                t, y = gw // 6, gw - 1 - cy
                idx = ((y // t) * 6 + cx // t) * t * t + (cx % t) * t + y % t
            else:
                idx = np.arange(gw * gw).reshape(gw, gw)
            cached = self._sidx = (gw, idx)
        return cached[1]

    def _rebuild_cache(self): #vers 6
        """Render grid data to QImage (SOL tiles via _grid_idx layout)."""
        import numpy as np
        from PyQt6.QtGui import QImage
        gw = self._grid_w
        self._cache_levels = self._levels_z()
        lut = np.array([self._cell_col(v).rgb() & 0xFFFFFFFF for v in range(256)], np.uint32)
        vals = np.frombuffer(bytes(self._grid), np.uint8)[self._screen_index()]
        rgb = np.ascontiguousarray(lut[vals])
        self._img_cache  = QImage(rgb.tobytes(), gw, gw, gw * 4, QImage.Format.Format_RGB32).copy()
        self._cache_flip = self._colour_flipped
        self._cache_key  = (self._colour_flipped, self._depth_mode, tuple(self._cache_levels))


    def paintEvent(self, ev): #vers 2
        if not self._grid_w:
            return
        ts  = self._ts()
        gw  = self._grid_w
        px0 = self._pan_x
        py0 = self._pan_y
        p   = QPainter(self)

        # Draw cached QImage scaled to current zoom using explicit scale transform
        key = (self._colour_flipped, self._depth_mode, tuple(self._levels_z()))
        if not hasattr(self, "_img_cache") or getattr(self, "_cache_key", None) != key:
            self._rebuild_cache()
        p.save()
        p.translate(px0, py0)
        p.scale(ts, ts)
        p.drawImage(0, 0, self._img_cache)
        p.restore()

        # Preview cells
        for (cx, cy) in self._preview_cells:
            p.fillRect(px0 + cx*ts, py0 + cy*ts, ts, ts, self.COL_PREV)

        # Hover overlay
        if self._hover_cx >= 0:
            p.fillRect(px0 + self._hover_cx*ts,
                       py0 + self._hover_cy*ts,
                       ts, ts, self.COL_HOVER)

        # Selection
        if self._sel_cx >= 0:
            x = px0 + self._sel_cx*ts
            y = py0 + self._sel_cy*ts
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
        levels = self._levels_z()
        paint = f"L=level {self._paint_level}"
        if self._paint_level < len(levels):
            paint += f" (Z {levels[self._paint_level]:.2f})"
        hover = ""
        hv = self._cell_val(self._hover_cx, self._hover_cy) if self._hover_cx >= 0 else None
        if hv is not None:
            hover = "  cell: land" if hv == 128 else (
                f"  cell: L{hv} Z {levels[hv]:.2f}" if hv < len(levels) else f"  cell: L{hv}")
        p.setPen(QColor(255, 255, 255, 180))
        p.setFont(QFont("monospace", 8))
        p.drawText(4, self.height()-6,
                   f"{gw}x{gw}  z={self._zoom:.1f}x  [{tool}]  {paint}  R=land(128){hover}")
        p.end()

    # depth gauge source (WaterZGauge ribbon): picks the paint level

    def z_range(self): #vers 1
        from apps.methods.ui_color import depth_range
        return depth_range(self._levels_z() or [0.0])

    def z_values(self): #vers 1
        return self._levels_z()

    def selected_z(self): #vers 1
        levels = self._levels_z()
        return levels[self._paint_level] if self._paint_level < len(levels) else None

    def set_selected_z(self, z, start=False): #vers 1
        """Gauge drag: paint level becomes the level nearest z."""
        levels = self._levels_z()
        if levels:
            self._paint_level = min(range(len(levels)), key=lambda i: abs(levels[i] - z))
            self.update()
            g = getattr(self._workshop, '_depth_gauge', None) if self._workshop else None
            if g is not None:
                g.update()


    def mouseMoveEvent(self, ev): #vers 1
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


    def mousePressEvent(self, ev): #vers 3
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
            self._zoom = max(0.1, min(self.zoom_max(), self._zoom * f))
            new = self._ts()
            self._pan_x = cx - int((cx - self._pan_x) * new / max(1, old))
            self._pan_y = cy - int((cy - self._pan_y) * new / max(1, old))
            self.update()
            return
        levels = self._levels_z()
        cx, cy = self._cell_at(ev.pos())
        if cx < 0:
            return
        self._sel_cx, self._sel_cy = cx, cy
        self.cell_clicked.emit(cx, cy)
        self.update()
        if tool == "picker":
            self.color_picked.emit(self._cell_val(cx, cy) == 0, is_left)  # 0=water
            return
        lv = self._paint_level if self._paint_level < max(1, len(levels)) else 0
        self._draw_val = lv if is_left else 128  # L=water at gauge level, R=land(128)
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


    def mouseReleaseEvent(self, ev): #vers 1
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


    def wheelEvent(self, ev): #vers 2
        delta = ev.angleDelta().y()
        if not delta:
            return
        f  = 1.12 if delta > 0 else 1/1.12
        cx, cy = ev.position().x(), ev.position().y()
        old = self._ts()
        self._zoom = max(0.1, min(self.zoom_max(), self._zoom * f))
        new = self._ts()
        self._pan_x = int(cx - (cx - self._pan_x) * new / max(1, old))
        self._pan_y = int(cy - (cy - self._pan_y) * new / max(1, old))
        self.update()


    def _shape_cells(self, tool, p0, p1): #vers 1
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


    def _flood_fill(self, cx, cy, val): #vers 1
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


    def sizeHint(self): #vers 1
        from PyQt6.QtCore import QSize
        if self._grid_w:
            ts = max(2, min(self.width() or 800, self.height() or 600) // self._grid_w)
            sz = self._grid_w * ts
            return QSize(sz, sz)
        return QSize(400, 400)


    def minimumSizeHint(self): #vers 1
        from PyQt6.QtCore import QSize
        return QSize(200, 200)


    def fit(self): #vers 1
        self._zoom  = 1.0
        self._pan_x = self._pan_y = 0
        self.update()


class WaterObjectCanvas(QWidget):
    """Edit water.dat objects for every game: SA/IV quads and triangles,
    III/VC/LCS rectangles. Select, drag corners or whole objects, add by
    dragging a box, delete, and set Z on the depth gauge (right side)."""
    quad_selected = pyqtSignal(int)        # primary selected object, -1 none
    edit_started  = pyqtSignal()           # before a change (undo snapshot)
    edited        = pyqtSignal()           # after a change
    edit_cancelled = pyqtSignal()          # press without a change (drop the undo point)
    add_requested = pyqtSignal(float, float, float, float, str)   # x0, y0, x1, y1, kind
    delete_requested = pyqtSignal()

    COL_SEL    = QColor(255, 200,   0, 230)
    COL_BORDER = QColor(80,  160, 255, 200)
    COL_BG     = QColor(20,   20,  20)
    HANDLE     = 5

    def __init__(self, parent=None): #vers 3
        super().__init__(parent)
        self._quads     = []
        self._sel       = set()
        self._sel_idx   = -1
        self._zoom      = 1.0
        self._pan_x     = 0
        self._pan_y     = 0
        self._bbox      = (-3000, -3000, 3000, 3000)
        self._pan_drag  = None
        self._pan_start = (0, 0)
        self._drag      = None            # ('corner'|'move'|'box'|'add'|'gauge', data)
        self.add_kind   = None            # 'quad' / 'tri' while adding
        self.snap       = 1.0
        self.gauge      = None            # WaterZGauge ribbon, repainted with the canvas
        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)

    def _get_ui_color(self, key): #vers 2
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)

    #    data helpers

    def setup(self, quads, bbox, keep_view=False): #vers 2
        self._quads = quads
        self._bbox  = bbox
        self._sel   = {i for i in self._sel if i < len(quads)} if keep_view else set()
        self._sel_idx = min(self._sel) if self._sel else -1
        if not keep_view:
            self._fit()
        self.update()

    @staticmethod
    def _z(q): #vers 1
        return sum(c["f"][0] for c in q["corners"]) / len(q["corners"])

    def _z_range(self): #vers 3
        """Shade / gauge Z range: 2nd-98th percentile so one odd height can't flatten the rest."""
        from apps.methods.ui_color import depth_range
        return depth_range([self._z(q) for q in self._quads])

    @staticmethod
    def _depth_col(z, zmin, zmax, alpha=170): #vers 2
        """Blue shade for a Z: low dark, high light (matches the grid gauge)."""
        from apps.methods.ui_color import water_depth_rgb
        return QColor(*water_depth_rgb(0.5 if zmax <= zmin else (z - zmin) / (zmax - zmin)), alpha)

    def _ordered(self, q): #vers 1
        """Corner indices in drawing order (corners are stored Z-order)."""
        cs = q["corners"]
        cx = sum(c["x"] for c in cs) / len(cs)
        cy = sum(c["y"] for c in cs) / len(cs)
        return sorted(range(len(cs)), key=lambda i: math.atan2(cs[i]["y"] - cy, cs[i]["x"] - cx))

    def _snap(self, v): #vers 1
        return round(v / self.snap) * self.snap if self.snap > 0 else v

    #    view

    def _view_rect(self): #vers 2
        return self.rect()

    def _fit(self): #vers 2
        x0, y0, x1, y1 = self._bbox
        ww, wh = x1 - x0, y1 - y0
        vr = self._view_rect()
        if ww <= 0 or wh <= 0:
            return
        self._zoom  = min(vr.width() / ww, vr.height() / wh) * 0.9
        self._pan_x = int(vr.width() / 2 - (x0 + ww / 2) * self._zoom)
        self._pan_y = int(vr.height() / 2 + (y0 + wh / 2) * self._zoom)

    def _w2s(self, wx, wy): #vers 2
        """World to screen; north (+Y) is up."""
        return wx * self._zoom + self._pan_x, -wy * self._zoom + self._pan_y

    def _s2w(self, sx, sy): #vers 1
        return (sx - self._pan_x) / self._zoom, -(sy - self._pan_y) / self._zoom

    def fit(self): #vers 1
        self._fit()
        self.update()

    #    painting

    def paintEvent(self, ev): #vers 3
        from PyQt6.QtCore import QPointF, QRectF
        from PyQt6.QtGui import QPolygonF
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        p.fillRect(self.rect(), self.COL_BG)
        if not self._quads and self.add_kind is None:
            p.setPen(self._get_ui_color('viewport_text'))
            p.drawText(self._view_rect(), Qt.AlignmentFlag.AlignCenter,
                       "No water objects - load a water.dat (SA/IV quads, III/VC rects)")
        zmin, zmax = self._z_range()
        for idx, q in enumerate(self._quads):
            if idx in self._sel:
                continue
            pts = QPolygonF([QPointF(*self._w2s(q["corners"][i]["x"], q["corners"][i]["y"])) for i in self._ordered(q)])
            p.setPen(QPen(self.COL_BORDER, 1))
            p.setBrush(self._depth_col(self._z(q), zmin, zmax))
            p.drawPolygon(pts)
        for idx in sorted(self._sel):                      # selected on top, with corner handles
            if idx >= len(self._quads):
                continue
            q = self._quads[idx]
            pts = QPolygonF([QPointF(*self._w2s(q["corners"][i]["x"], q["corners"][i]["y"])) for i in self._ordered(q)])
            p.setPen(QPen(self.COL_SEL, 2))
            p.setBrush(self._depth_col(self._z(q), zmin, zmax, 220))
            p.drawPolygon(pts)
            p.setBrush(self.COL_SEL)
            for c in q["corners"]:
                sx, sy = self._w2s(c["x"], c["y"])
                p.drawRect(QRectF(sx - self.HANDLE, sy - self.HANDLE, self.HANDLE * 2, self.HANDLE * 2))
        if self._drag and self._drag[0] in ('box', 'add'):
            (ax, ay), (bx, by) = self._drag[1]
            p.setPen(QPen(self.COL_SEL, 1, Qt.PenStyle.DashLine))
            p.setBrush(QColor(255, 200, 0, 40) if self._drag[0] == 'add' else Qt.BrushStyle.NoBrush)
            if self._drag[0] == 'add' and self.add_kind == 'tri':
                p.drawPolygon(QPolygonF([QPointF(ax, by), QPointF(bx, by), QPointF(ax, ay)]))
            else:
                p.drawRect(QRectF(QPointF(ax, ay), QPointF(bx, by)).normalized())
        p.end()
        if self.gauge is not None:
            self.gauge.update()

    #    picking

    def _hit_obj(self, sx, sy): #vers 1
        """Smallest object under the screen point, or -1."""
        from PyQt6.QtCore import QPointF
        from PyQt6.QtGui import QPolygonF
        wx, wy = self._s2w(sx, sy)
        best, best_a = -1, None
        for idx, q in enumerate(self._quads):
            poly = QPolygonF([QPointF(q["corners"][i]["x"], q["corners"][i]["y"]) for i in self._ordered(q)])
            if poly.containsPoint(QPointF(wx, wy), Qt.FillRule.OddEvenFill):
                br = poly.boundingRect()
                a = br.width() * br.height()
                if best_a is None or a < best_a:
                    best, best_a = idx, a
        return best

    def _hit_corner(self, sx, sy): #vers 1
        """(object, corner) handle of a selected object under the point, or None."""
        for idx in self._sel:
            if idx >= len(self._quads):
                continue
            for ci, c in enumerate(self._quads[idx]["corners"]):
                cx, cy = self._w2s(c["x"], c["y"])
                if abs(cx - sx) <= self.HANDLE + 2 and abs(cy - sy) <= self.HANDLE + 2:
                    return idx, ci
        return None

    def set_selection(self, sel, primary=None): #vers 1
        self._sel = set(sel)
        self._sel_idx = primary if primary is not None else (min(self._sel) if self._sel else -1)
        self.quad_selected.emit(self._sel_idx)
        self.update()

    def selection(self): #vers 1
        return sorted(i for i in self._sel if i < len(self._quads))

    #    edits

    def _move_corner(self, idx, ci, x, y): #vers 1
        """Move one corner; rectangle objects keep their edges straight."""
        q = self._quads[idx]
        cs = q["corners"]
        if q.get("_rect"):
            for k, c in enumerate(cs):
                if (k & 1) == (ci & 1):
                    c["x"] = x
                if (k & 2) == (ci & 2):
                    c["y"] = y
        else:
            cs[ci]["x"], cs[ci]["y"] = x, y

    def move_selected(self, dx, dy): #vers 1
        for idx in self.selection():
            for c in self._quads[idx]["corners"]:
                c["x"] += dx; c["y"] += dy

    def set_selected_z(self, z, start=False, emit=True): #vers 2
        """Z of the selected objects; start / emit drive the undo + edited signals."""
        if start:
            self.edit_started.emit()
        for idx in self.selection():
            for c in self._quads[idx]["corners"]:
                c["f"] = [z] + list(c["f"][1:])
        if emit:
            self.edited.emit()
            self.update()

    def z_range(self): #vers 1
        return self._z_range()

    def z_values(self): #vers 1
        return [self._z(q) for q in self._quads]

    def selected_z(self): #vers 1
        sel = [self._quads[i] for i in self.selection()]
        return sum(self._z(q) for q in sel) / len(sel) if sel else None

    #    mouse

    def mousePressEvent(self, ev): #vers 4
        x, y = ev.position().x(), ev.position().y()
        if ev.button() == Qt.MouseButton.MiddleButton:
            self._pan_drag  = ev.pos()
            self._pan_start = (self._pan_x, self._pan_y)
            return
        if ev.button() != Qt.MouseButton.LeftButton:
            return
        if self.add_kind:
            self._drag = ('add', [(x, y), (x, y)])
            return
        hc = self._hit_corner(x, y)
        if hc is not None:
            self.edit_started.emit()
            self._drag = ('corner', [hc[0], hc[1], False])
            return
        ctrl = bool(ev.modifiers() & Qt.KeyboardModifier.ControlModifier)
        idx = self._hit_obj(x, y)
        if idx >= 0:
            if ctrl:
                self.set_selection(self._sel ^ {idx}, idx if idx not in self._sel else None)
                return
            if idx not in self._sel:
                self.set_selection({idx}, idx)
            self.edit_started.emit()
            wx, wy = self._s2w(x, y)
            self._drag = ('move', [self._snap(wx), self._snap(wy), False])
            return
        self._drag = ('box', [(x, y), (x, y), ctrl])

    def mouseMoveEvent(self, ev): #vers 4
        x, y = ev.position().x(), ev.position().y()
        if self._pan_drag and ev.buttons() & Qt.MouseButton.MiddleButton:
            self._pan_x = self._pan_start[0] + ev.pos().x() - self._pan_drag.x()
            self._pan_y = self._pan_start[1] + ev.pos().y() - self._pan_drag.y()
            self.update()
            return
        if not self._drag:
            wx, wy = self._s2w(x, y)
            self.setToolTip(f"{wx:.0f}, {wy:.0f}")
            return
        kind, data = self._drag
        if kind == 'corner':
            wx, wy = self._s2w(x, y)
            self._move_corner(data[0], data[1], self._snap(wx), self._snap(wy))
            data[2] = True
            self.edited.emit()
        elif kind == 'move':
            wx, wy = (self._snap(v) for v in self._s2w(x, y))
            if (wx, wy) != (data[0], data[1]):
                self.move_selected(wx - data[0], wy - data[1])
                data[0], data[1], data[2] = wx, wy, True
                self.edited.emit()
        else:                                              # box / add rubber band
            data[1] = (x, y)
        self.update()

    def mouseReleaseEvent(self, ev): #vers 3
        if ev.button() == Qt.MouseButton.MiddleButton:
            self._pan_drag = None
            return
        drag, self._drag = self._drag, None
        if not drag:
            return
        kind, data = drag
        if kind in ('move', 'corner') and not data[2]:
            self.edit_cancelled.emit()
        if kind == 'add':
            (ax, ay), (bx, by) = data
            if abs(bx - ax) > 4 and abs(by - ay) > 4:
                x0, y1 = self._s2w(min(ax, bx), min(ay, by))
                x1, y0 = self._s2w(max(ax, bx), max(ay, by))
                self.add_requested.emit(self._snap(x0), self._snap(y0), self._snap(x1), self._snap(y1),
                                        self.add_kind)
        elif kind == 'box':
            (ax, ay), (bx, by), add = data
            if abs(bx - ax) <= 3 and abs(by - ay) <= 3:
                if not add:
                    self.set_selection(set())
            else:
                x0, y1 = self._s2w(min(ax, bx), min(ay, by))
                x1, y0 = self._s2w(max(ax, bx), max(ay, by))
                hit = {i for i, q in enumerate(self._quads)
                       if all(x0 <= c["x"] <= x1 and y0 <= c["y"] <= y1 for c in q["corners"])}
                self.set_selection((self._sel | hit) if add else hit)
        self.update()

    def wheelEvent(self, ev): #vers 2
        """Zoom around the cursor."""
        f = 1.12 if ev.angleDelta().y() > 0 else 1 / 1.12
        x, y = ev.position().x(), ev.position().y()
        wx, wy = self._s2w(x, y)
        self._zoom = max(0.005, min(50.0, self._zoom * f))
        self._pan_x = x - wx * self._zoom
        self._pan_y = y + wy * self._zoom
        self.update()

    def keyPressEvent(self, ev): #vers 1
        """Delete removes, arrows nudge by the snap step, Esc ends add mode."""
        k = ev.key()
        step = self.snap or 1.0
        moves = {Qt.Key.Key_Left: (-step, 0), Qt.Key.Key_Right: (step, 0),
                 Qt.Key.Key_Up: (0, step), Qt.Key.Key_Down: (0, -step)}
        if k in (Qt.Key.Key_Delete, Qt.Key.Key_Backspace):
            self.delete_requested.emit()
        elif k in moves and self.selection():
            self.edit_started.emit()
            self.move_selected(*moves[k])
            self.edited.emit(); self.update()
        elif k == Qt.Key.Key_Escape:
            self.add_kind = None
            self.set_selection(set())
        else:
            super().keyPressEvent(ev)

    def sizeHint(self): #vers 1
        return QSize(600, 600)


class WaterZGauge(QWidget):
    """Depth gang ribbon (26px): same blue scale as the water shading, a tick per
    water Z, marker for the selection; drag sets selection Z. source gives
    z_range(), z_values(), selected_z() and set_selected_z(z) (None = read only).
    Horizontal in a top/bottom ribbon: low Z left."""

    def __init__(self, source, parent=None): #vers 4
        super().__init__(parent)
        self.setObjectName("Depth Gauge")              # Ribbon Manager label
        self._src = source
        self._dragging = False
        self._horiz = False
        self.setMinimumHeight(120)
        self.setMouseTracking(True)

    def set_horizontal(self, horiz): #vers 1
        self._horiz = bool(horiz)
        self.update()

    def _bar(self): #vers 2
        from PyQt6.QtCore import QRect
        if self._horiz:
            return QRect(22, 7, max(40, self.width() - 44), max(4, self.height() - 9))
        return QRect(7, 16, max(4, self.width() - 9), max(40, self.height() - 32))

    def _t_of(self, z, zmin, zmax): #vers 1
        return 0.5 if zmax <= zmin else max(0.0, min(1.0, (z - zmin) / (zmax - zmin)))

    def _pos_of(self, z, zmin, zmax): #vers 1
        """Pixel along the bar for z (x when horizontal, else y)."""
        r, t = self._bar(), self._t_of(z, zmin, zmax)
        if self._horiz:
            return int(r.left() + t * r.width())
        return int(r.top() + (1.0 - t) * r.height())

    def _z_at(self, pos): #vers 3
        """Z under a mouse QPointF."""
        zmin, zmax = self._src.z_range()
        r = self._bar()
        if self._horiz:
            t = max(0.0, min(1.0, (pos.x() - r.left()) / max(1, r.width())))
        else:
            t = 1.0 - max(0.0, min(1.0, (pos.y() - r.top()) / max(1, r.height())))
        return zmin + t * (zmax - zmin)

    def paintEvent(self, ev): #vers 3
        from PyQt6.QtGui import QLinearGradient
        from apps.methods.ui_color import water_depth_rgb
        zmin, zmax = self._src.z_range()
        r = self._bar()
        p = QPainter(self)
        p.fillRect(self.rect(), QColor(10, 14, 24))
        if self._horiz:
            g = QLinearGradient(r.right(), 0, r.left(), 0)
        else:
            g = QLinearGradient(0, r.top(), 0, r.bottom())
        g.setColorAt(0.0, QColor(*water_depth_rgb(1.0)))     # identical scale to the tiles
        g.setColorAt(1.0, QColor(*water_depth_rgb(0.0)))
        p.fillRect(r, g)
        p.setPen(QPen(QColor(255, 255, 255, 110), 1))
        for z in sorted({round(z, 2) for z in self._src.z_values()}):
            k = self._pos_of(z, zmin, zmax)
            if self._horiz:
                p.drawLine(k, r.top(), k, r.bottom())
            else:
                p.drawLine(r.left(), k, r.right(), k)
        p.setFont(QFont("monospace", 6))
        p.setPen(QColor(255, 255, 255, 200))
        if self._horiz:
            p.drawText(1, self.height() // 2 + 3, f"{zmin:.0f}")
            p.drawText(r.right() + 3, self.height() // 2 + 3, f"{zmax:.0f}")
        else:
            p.drawText(0, 10, f"{zmax:.0f}")
            p.drawText(0, self.height() - 4, f"{zmin:.0f}")
        z = self._src.selected_z()
        if z is not None:
            k = self._pos_of(z, zmin, zmax)
            p.setBrush(QColor(255, 200, 0)); p.setPen(Qt.PenStyle.NoPen)
            tri = [QPoint(k - 5, 0), QPoint(k, 6), QPoint(k + 5, 0)] if self._horiz else \
                [QPoint(0, k - 5), QPoint(6, k), QPoint(0, k + 5)]
            p.drawPolygon(QPolygon(tri))
        p.end()

    def mousePressEvent(self, ev): #vers 3
        if ev.button() == Qt.MouseButton.LeftButton and self._src.selected_z() is not None:
            self._dragging = True
            self._src.set_selected_z(round(self._z_at(ev.position()), 2), start=True)

    def mouseMoveEvent(self, ev): #vers 3
        if self._dragging:
            self._src.set_selected_z(round(self._z_at(ev.position()), 2))
        else:
            self.setToolTip(f"Z {self._z_at(ev.position()):.1f}")

    def mouseReleaseEvent(self, ev): #vers 1
        self._dragging = False


# =============================================================================
# SECTION 3 - WaterWorkshop: GUIWorkshop subclass
# =============================================================================

class WaterWorkshop(RibbonMixin, GUIWorkshop):

    App_name        = "Water Workshop"
    App_build       = Build
    App_author      = "X-Seti"
    App_year        = "2026"
    App_description = ("GTA III / VC / PS2 LC / SOL - waterpro.dat + water.dat\n"
                       "GTA SA - water.dat / water1.dat (quad format)")
    config_key      = "water_workshop"
    WS_DEFAULTS     = {"depth_colour": False, "depth_gauge_side": "", "object_snap": 2.0}
    _ribbon_name    = "water_workshop"
    # Bump when the set of ribbons changes (1 = File/Edit/View/Draw ribbons)
    _RIBBON_LAYOUT_VERSION = 3
    DROP_EXTS = ('.dat',)          # waterpro.dat / water.dat


    def __init__(self, parent=None, main_window=None): #vers 2
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
        self._dirty       = False
        self._grid_offset_x = 0
        self._grid_offset_y = 0
        self._grid_offset_z = 0.0
        self._rect_objs   = []            # water.dat rects as editable objects
        self._undo_objs   = []
        self._redo_objs   = []
        super().__init__(parent, main_window)
        # Set anchor icon
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVG
            self.setWindowIcon(_SVG.water_workshop_icon(64))
        except Exception:
            pass
        self.ribbon_restore_state()


    def _create_toolbar(self): #vers 2
        """Titlebar keeps Settings / title / Undo / Info / Theme; the file
        buttons moved to the File ribbon."""
        tb = super()._create_toolbar()
        for name in ("open_btn", "save_btn", "export_btn", "import_btn"):
            btn = getattr(self, name, None)
            if btn:
                btn.setVisible(False)
        return tb


    def setup_ui(self): #vers 4
        """Titlebar / Water dock | ribbon-wrapped canvas tabs / status bar."""
        ml = QVBoxLayout(self)
        ml.setContentsMargins(*self.get_content_margins())
        ml.setSpacing(self.setspacing)
        ml.addWidget(self._create_toolbar())
        left_panel = self._create_left_panel()
        centre_panel = self._create_centre_panel()
        self._draw_btns = {}
        ml.addWidget(self._dock_layout(self.ribbon_wrap(centre_panel),
                                       [("Water Levels / Rects", left_panel, 200, Qt.DockWidgetArea.LeftDockWidgetArea)]), 1)
        self._build_ribbons()

        self._status_widget = self._create_status_bar()
        ml.addWidget(self._status_widget)
        self._status_widget.setVisible(self.WS.get("show_statusbar", True))


    def _build_ribbons(self): #vers 3
        """File / Edit / View / Draw ribbons (replaces the old right-hand
        tool sidebar and the titlebar/left-panel file buttons)."""
        B = self.ribbon_button

        tb = self.ribbon_toolbar("File")
        B(tb, "open_icon",   "Load water file  (Ctrl+O)", self._open_file)
        self.save_btn = B(tb, "save_icon", "Save  (Ctrl+S)", self._save_file, enabled=False)
        B(tb, "saveas_icon", "Save As...", self._save_file_as)
        tb.addSeparator()
        B(tb, "export_icon", "Export grids as BMP", self._export_bmp)
        B(tb, "import_icon", "Import BMP grid",     self._import_bmp)

        tb = self.ribbon_toolbar("Edit")
        B(tb, "undo_icon",   "Undo  (Ctrl+Z)", self._undo)
        B(tb, "redo_icon",   "Redo  (Ctrl+Y)", self._redo)
        tb.addSeparator()
        B(tb, "scissors_icon",   "Erase all (active grid)",  self._erase_all_grid)
        B(tb, "rotate_cw_icon",  "Invert grid data",         self._invert_grid)
        B(tb, "flip_horz_icon",  "Flip display colours",     self._flip_colours)
        tb.addSeparator()
        B(tb, "search_icon",     "Water statistics",         self._show_stats)
        B(tb, "locate_icon",     "Grid offset / shift",      self._show_offset_dialog)

        tb = self.ribbon_toolbar("View")
        B(tb, "zoom_in_icon",    "Zoom in  (+)",  lambda: self._zoom(1.25))
        B(tb, "zoom_out_icon",   "Zoom out  (-)", lambda: self._zoom(0.8))
        B(tb, "fit_grid_icon",   "Fit  (Ctrl+0)", self._fit)
        B(tb, "grid_icon",       "Toggle grid lines", self._toggle_grid)

        tb = self.ribbon_toolbar("Draw")
        for icon, tip, name in (
                ("paint_icon",     "Pencil (P)",            "pencil"),
                ("fill_icon",      "Flood fill (F)",        "fill"),
                ("line_icon",      "Line (L)",              "line"),
                ("rect_icon",      "Rect outline (R)",      "rect"),
                ("rect_fill_icon", "Filled rect (Shift+R)", "rect_fill"),
                ("dropper_icon",   "Dropper (K)",           "picker"),
                ("zoom_in_icon",   "Zoom tool (Z)",         "zoom")):
            self._draw_btns[name] = B(
                tb, icon, tip, lambda checked=False, t=name: self._set_active_tool(t),
                checkable=True)
        self._draw_btns["pencil"].setChecked(True)
        self._active_tool = "pencil"
        self.ribbon_label(tb, "L = Sea   R = Land")

        self._ribbon_mw.addToolBarBreak(Qt.ToolBarArea.TopToolBarArea)
        tb = self.ribbon_toolbar("Objects")            # water.dat quads / triangles / rects
        self._obj_btns = {
            None:   B(tb, "dp_select_icon",   "Select / move (drag corners or objects)",
                      lambda checked=False: self._set_obj_tool(None), checkable=True),
            'quad': B(tb, "rect_icon",        "Add quad / rect (drag a box)",
                      lambda checked=False: self._set_obj_tool('quad'), checkable=True),
            'tri':  B(tb, "create_face_icon", "Add triangle (SA, drag a box)",
                      lambda checked=False: self._set_obj_tool('tri'), checkable=True)}
        self._obj_btns[None].setChecked(True)
        B(tb, "delete_face_icon", "Delete selected (Del)", self._obj_delete)
        tb.addSeparator()
        self.ribbon_label(tb, "Z")
        self._obj_z_spin = QDoubleSpinBox()
        self._obj_z_spin.setFixedSize(100, 26)
        self._obj_z_spin.setRange(-1000, 1000); self._obj_z_spin.setDecimals(2)
        self._obj_z_spin.setToolTip("Z of the selected objects (also drag the depth gauge)")
        self._obj_z_spin.editingFinished.connect(lambda: self._obj_set_z(self._obj_z_spin.value()))
        tb.addWidget(self._obj_z_spin)
        self.ribbon_label(tb, "Snap")
        self._obj_snap_spin = QDoubleSpinBox()
        self._obj_snap_spin.setFixedSize(100, 26)
        self._obj_snap_spin.setRange(0, 100); self._obj_snap_spin.setDecimals(1)
        self._obj_snap_spin.setValue(float(self.WS.get("object_snap", 2.0)))
        self._obj_snap_spin.setToolTip("Corner / move snap step (0 = off)")
        self._obj_snap_spin.valueChanged.connect(self._obj_set_snap)
        tb.addWidget(self._obj_snap_spin)
        self._sa_canvas.snap = self._obj_snap_spin.value()
        self._sa_canvas.gauge = self._depth_gauge = WaterZGauge(self._sa_canvas)
        self._depth_tb = self.ribbon_side_toolbar("Depth", self._depth_gauge)


    def _build_menus_into_qmenu(self, pm): #vers 4
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
        gm = vm.addMenu("Depth Gauge (Z)")
        cur = self.WS.get("depth_gauge_side", "")
        for label, side in (("Off", ""), ("Left", "left"), ("Right", "right")):
            act = gm.addAction(label)
            act.setCheckable(True)
            act.setChecked(cur == side)
            act.triggered.connect(lambda _c=False, sd=side: self._set_depth_view(gauge_side=sd))
        act = vm.addAction("Colour Water by Depth (Z)")
        act.setCheckable(True)
        act.setChecked(bool(self.WS.get("depth_colour", False)))
        act.toggled.connect(lambda on: self._set_depth_view(depth_colour=on))
        vm.addSeparator()
        vm.addAction("About Water Workshop", self._show_about)


    def _create_left_panel(self): #vers 2
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ll  = QVBoxLayout(panel)
        ll.setContentsMargins(*self.get_panel_margins())

        hdr = QLabel("Water Levels / Rects")
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        hdr.setFont(self.panel_font)
        hdr.setStyleSheet("font-weight:bold; padding:2px;")
        #ll.addWidget(hdr)
        self._levels_list = QListWidget()
        self._levels_list.setAlternatingRowColors(True)
        self._levels_list.itemDoubleClicked.connect(self._edit_level)
        self._levels_list.currentRowChanged.connect(self._on_list_row)
        ll.addWidget(self._levels_list)
        br = QHBoxLayout()
        self._add_btn = QPushButton("+ Add")
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


    def _create_centre_panel(self): #vers 5
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
        for cv in (self._phys_canvas, self._vis_canvas):
            cv._depth_mode = bool(self.WS.get("depth_colour", False))
        sc2 = QScrollArea()
        sc2.setWidget(self._vis_canvas)
        sc2.setWidgetResizable(True)
        self._view_tabs.addTab(sc2, "Visible (128x128)")
        # SA Quads tab: canvas + translate controls
        sa_frame = QFrame()
        sa_lay   = QVBoxLayout(sa_frame)
        sa_lay.setContentsMargins(0, 0, 0, 0)
        sa_lay.setSpacing(2)
        self._sa_canvas = WaterObjectCanvas()
        self._sa_canvas.quad_selected.connect(self._on_quad_selected)
        self._sa_canvas.edit_started.connect(self._obj_edit_started)
        self._sa_canvas.edited.connect(self._obj_edited)
        self._sa_canvas.edit_cancelled.connect(lambda: self._undo_objs and self._undo_objs.pop())
        self._sa_canvas.add_requested.connect(self._obj_add)
        self._sa_canvas.delete_requested.connect(self._obj_delete)
        sa_lay.addWidget(self._sa_canvas, 1)

        # Translate bar
        from PyQt6.QtWidgets import QGroupBox, QHBoxLayout, QDoubleSpinBox
        tbar = QGroupBox("Translate all water data")
        tbl  = QHBoxLayout(tbar); tbl.setSpacing(6)
        self._wdx = QDoubleSpinBox(); self._wdx.setRange(-9999,9999); self._wdx.setDecimals(2)
        self._wdx.setMinimumWidth(110); self._wdx.setMinimumHeight(28); self._wdx.setPrefix("dX: ")
        self._wdy = QDoubleSpinBox(); self._wdy.setRange(-9999,9999); self._wdy.setDecimals(2)
        self._wdy.setMinimumWidth(110); self._wdy.setMinimumHeight(28); self._wdy.setPrefix("dY: ")
        self._wdz = QDoubleSpinBox(); self._wdz.setRange(-9999,9999); self._wdz.setDecimals(2)
        self._wdz.setMinimumWidth(110); self._wdz.setMinimumHeight(28); self._wdz.setPrefix("dZ: ")
        for sp in (self._wdx, self._wdy, self._wdz):
            tbl.addWidget(sp)
        from PyQt6.QtWidgets import QPushButton as _PB
        apply_btn = _PB("Apply"); apply_btn.setMinimumHeight(28)
        apply_btn.setToolTip("Translate all water quads and rects by dX/dY/dZ")
        apply_btn.clicked.connect(lambda: self.translate_water(
            self._wdx.value(), self._wdy.value(), self._wdz.value()))
        tbl.addWidget(apply_btn)
        tbl.addStretch()
        sa_lay.addWidget(tbar)

        self._view_tabs.addTab(sa_frame, "Water Objects")
        cl.addWidget(self._view_tabs)
        return panel

# SECTION 4 - Water logic

    def _mark_dirty(self, dirty=True): #vers 1
        self._dirty = dirty
        self._dirty_lbl.setText("Modified: yes" if dirty else "Modified: no")
        self.save_btn.setEnabled(dirty)

    def _on_grid_changed(self): #vers 2
        self._mark_dirty(True)
        for c in (self._phys_canvas, self._vis_canvas):
            if hasattr(c, "_img_cache"): del c._img_cache

    def _on_tab_changed(self, idx: int): #vers 2
        """Track the active grid; the depth gauge follows the shown view."""
        self._active_grid = "phys" if idx == 0 else "vis"
        g = getattr(self, '_depth_gauge', None)
        if g is not None:
            g._src = self._sa_canvas if idx == 2 or self._file_type != "waterpro" else self._active_canvas()
            g.update()

    def _active_canvas(self): #vers 1
        return self._phys_canvas if self._active_grid == "phys" else self._vis_canvas

    def _push_undo_grid(self): #vers 2
        """Snapshot the active tab's grid (tab 0 canvas shows vis_grid)."""
        if self._active_grid == "phys" and self._waterpro:
            self._undo_phys.append(bytearray(self._waterpro.vis_grid))
            if len(self._undo_phys) > 20:
                self._undo_phys.pop(0)
            self._redo_phys.clear()
        elif self._active_grid == "vis" and self._waterpro:
            self._undo_vis.append(bytearray(self._waterpro.phys_grid))
            if len(self._undo_vis) > 20:
                self._undo_vis.pop(0)
            self._redo_vis.clear()

    def _undo(self): #vers 3
        if self._view_tabs.currentIndex() == 2:
            return self._obj_undo_redo(self._undo_objs, self._redo_objs, "Undo")
        if self._active_grid == "phys":
            s, r, attr, canvas = self._undo_phys, self._redo_phys, "vis_grid", self._phys_canvas
        else:
            s, r, attr, canvas = self._undo_vis,  self._redo_vis,  "phys_grid", self._vis_canvas
        if not s:
            self._set_status("Nothing to undo")
            return
        if self._waterpro:
            r.append(bytearray(getattr(self._waterpro, attr)))
            setattr(self._waterpro, attr, s.pop())
            canvas.set_grid(getattr(self._waterpro, attr))
        self._set_status("Undo")


    def _redo(self): #vers 3
        if self._view_tabs.currentIndex() == 2:
            return self._obj_undo_redo(self._redo_objs, self._undo_objs, "Redo")
        if self._active_grid == "phys":
            s, u, attr, canvas = self._redo_phys, self._undo_phys, "vis_grid", self._phys_canvas
        else:
            s, u, attr, canvas = self._redo_vis,  self._undo_vis,  "phys_grid", self._vis_canvas
        if not s:
            self._set_status("Nothing to redo")
            return
        if self._waterpro:
            u.append(bytearray(getattr(self._waterpro, attr)))
            setattr(self._waterpro, attr, s.pop())
            canvas.set_grid(getattr(self._waterpro, attr))
        self._set_status("Redo")


    def _zoom(self, f): #vers 2
        for c in (self._phys_canvas, self._vis_canvas):
            c._zoom = max(0.1, min(c.zoom_max(), c._zoom * f))
            c.update()


    def _fit(self): #vers 1
        for c in (self._phys_canvas, self._vis_canvas):
            c.fit()
        self._sa_canvas.fit()


    def _toggle_grid(self): #vers 1
        for c in (self._phys_canvas, self._vis_canvas):
            c._show_grid = not c._show_grid
            c.update()


    def _set_depth_view(self, gauge_side=None, depth_colour=None): #vers 2
        """Depth gauge ribbon off / left / right, and depth colouring of both grids."""
        if gauge_side is not None:
            self.WS._data["depth_gauge_side"] = gauge_side
            tb = getattr(self, '_depth_tb', None)
            if tb is not None:
                if gauge_side:
                    area = Qt.ToolBarArea.LeftToolBarArea if gauge_side == 'left' else Qt.ToolBarArea.RightToolBarArea
                    self._ribbon_mw.addToolBar(area, tb)
                tb.setVisible(bool(gauge_side))
                self._ribbon_schedule_save()
        if depth_colour is not None:
            self.WS._data["depth_colour"] = bool(depth_colour)
        self.WS.save()
        for cv in (getattr(self, '_phys_canvas', None), getattr(self, '_vis_canvas', None)):
            if cv is not None:
                cv._depth_mode = bool(self.WS.get("depth_colour", False))
                cv.update()

    def _flip_colours(self): #vers 1
        for c in (self._phys_canvas, self._vis_canvas):
            c._colour_flipped = not c._colour_flipped
            c.update()
        state = "flipped" if self._phys_canvas._colour_flipped else "normal"
        self._set_status(f"Colour display: {state}  (data unchanged)")


    def _set_active_tool(self, tool: str): #vers 1
        self._active_tool = tool
        for name, btn in self._draw_btns.items():
            btn.setChecked(name == tool)


    def _erase_all_grid(self): #vers 1
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


    def _invert_grid(self): #vers 1
        canvas = self._active_canvas()
        if not canvas._grid_w:
            return
        self._push_undo_grid()
        for i in range(len(canvas._grid)):
            canvas._grid[i] = 128 if canvas._grid[i] == 0 else 0
        canvas.update()
        self._on_grid_changed()


    def _is_binary_waterpro(self, data): #vers 1
        if len(data) < 100:
            return False
        rem = len(data) - 964
        if rem <= 0 or rem % 5 != 0:
            return False
        gw = int(math.isqrt(rem // 5))
        return gw * gw == rem // 5


    def _is_sa_water(self, data): #vers 3
        """SA ("processed" header) or GTA IV (no header, quad lines) water.dat."""
        try:
            text = data[:4096].decode("latin1")
        except Exception:
            return False
        if text[:9] == "processed":
            return True
        first = next((l.split() for l in text.splitlines()
                      if l.strip() and not l.lstrip().startswith((";", "#"))), [])
        if len(first) < 28:
            return False
        try:
            [float(p) for p in first]
        except ValueError:
            return False
        return True


    def _open_file(self, path=None): #vers 3
        if not path:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open Water File", "",
                "Water Files (*water*.dat *Water*.dat *WATER*.DAT *WATER*.dat)")
        if path:
            self._load_file(path)


    def _load_file(self, path): #vers 4
        """Open a water file; names without 'water' are refused (crash guard)."""
        if "water" not in Path(path).name.lower():
            QMessageBox.warning(self, "Open",
                f"{Path(path).name} is not a water file (waterpro.dat / water.dat).")
            return
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
            self._undo_phys.clear(); self._undo_vis.clear()
            self._redo_phys.clear(); self._redo_vis.clear()
            self._undo_objs.clear(); self._redo_objs.clear()
            self._mark_dirty(False)
            self._on_tab_changed(self._view_tabs.currentIndex())     # gauge follows the view
        except Exception as e:
            import traceback
            QMessageBox.critical(self, "Load Error",
                f"Failed to load {Path(path).name}:\n{e}\n\n{traceback.format_exc()[-400:]}")


    def _load_waterpro(self, path, data): #vers 3
        wp = WaterproParser()
        wp.load(path)
        self._waterpro  = wp
        self._waterdat  = self._sa_water = None
        self._file_type = "waterpro"
        gw = wp.grid_w
        # vis_grid is the smaller block (gw*gw), phys_grid is the larger (2*gw * 2*gw)
        self._phys_canvas.setup(gw,   wp.vis_grid)
        self._vis_canvas.setup(gw*2,  wp.phys_grid)
        self._view_tabs.setTabText(0, f"Visible ({gw}x{gw})")
        self._view_tabs.setTabText(1, f"Physical ({gw*2}x{gw*2})")
        for i, e in enumerate([True, True, False]):
            self._view_tabs.setTabEnabled(i, e)
        self._view_tabs.setCurrentIndex(0)
        self._refresh_levels_list()
        nv = sum(1 for b in wp.vis_grid  if b == 128)
        np_ = sum(1 for b in wp.phys_grid if b == 128)
        self._set_status(
            f"Loaded {Path(path).name}  |  {gw}x{gw} visible ({nv} land cells)"
            f"  |  {gw*2}x{gw*2} physical ({np_} land cells)"
            f"  |  {wp.water_levels_count} level(s)")


    def _load_waterdat(self, path): #vers 3
        wd = WaterDatParser()
        wd.load(path)
        self._waterdat  = wd
        self._waterpro  = self._sa_water = None
        self._file_type = "waterdat"
        for i, e in enumerate([False, False, True]):
            self._view_tabs.setTabEnabled(i, e)
        self._view_tabs.setCurrentIndex(2)
        self._rects_to_objs()
        self._sa_canvas.setup(self._rect_objs, wd.world_bbox())
        self._levels_list.clear()
        for i, r in enumerate(wd.rects):
            self._levels_list.addItem(QListWidgetItem(
                f"[{i}] Z={r[0]:.1f}  ({r[1]:.0f},{r[2]:.0f}) to ({r[3]:.0f},{r[4]:.0f})"))
        self._set_status(
            f"Loaded {Path(path).name}  |  {len(wd.rects)} rectangle(s)  (text)")


    def _load_sa_water(self, path): #vers 3
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
        kind = "GTA IV" if sa.is_iv() else "SA"
        self._set_status(
            f"Loaded {Path(path).name}  |  {len(sa.quads)} {kind} water quads")


    def _refresh_levels_list(self): #vers 3
        self._levels_list.clear()
        if self._waterpro:
            wp = self._waterpro
            for i in range(wp.water_levels_count):
                self._levels_list.addItem(QListWidgetItem(
                    f"Level {i}:  Z = {wp.water_level_data[i]:.4f}"))
        elif self._waterdat:
            for i, r in enumerate(self._waterdat.rects):
                self._levels_list.addItem(QListWidgetItem(
                    f"[{i}] Z={r[0]:.1f}  ({r[1]:.0f},{r[2]:.0f}) to ({r[3]:.0f},{r[4]:.0f})"))
        elif self._sa_water:
            for i, q in enumerate(self._sa_water.quads):
                c = q["corners"][0]
                self._levels_list.addItem(QListWidgetItem(
                    f"[{i}] ({c['x']:.0f},{c['y']:.0f})  flag={q['flag']}"))


    def _current_parser(self): #vers 1
        return self._waterpro or self._waterdat or self._sa_water

    def _write_water(self, path) -> bool: #vers 1
        """Back up any existing file at path, then write atomically."""
        from apps.methods.file_backup import backup_file, note_change
        parser = self._current_parser()
        if parser is None:
            return False
        try:
            if Path(path).exists():
                note_change(f"Save {Path(path).name}")
                if backup_file(str(path)) is None:
                    QMessageBox.warning(self, "Save",
                        "Backup failed - file not overwritten.")
                    return False
            parser.save(str(path))
        except Exception as e:
            QMessageBox.critical(self, "Save Error", f"{Path(path).name}:\n{e}")
            return False
        self._file_path = str(path)
        self.WS.add_recent(str(path))
        self._mark_dirty(False)
        self._set_status(f"Saved {Path(path).name}")
        return True

    def _save_file(self): #vers 3
        """Save over the loaded file (previous version is backed up first)."""
        if not self._current_parser() or not self._file_path:
            self._set_status("No file loaded")
            return
        self._write_water(self._file_path)

    def _save_file_as(self): #vers 1
        if not self._current_parser():
            self._set_status("No file loaded")
            return
        p, _ = QFileDialog.getSaveFileName(
            self, "Save water file as", self._file_path,
            "DAT Files (*.dat *.DAT);;All Files (*)")
        if p:
            self._write_water(p)


    def _export_file(self): #vers 2
        self._export_bmp()


    def _import_file(self): #vers 2
        self._import_bmp()


    def _bmp_index(self, canvas, w): #vers 1
        """Grid byte index per BMP pixel: SOL as shown, others raw (rotated)."""
        if w % 6 == 0:
            return [canvas._grid_idx(x, y) for y in range(w) for x in range(w)]
        return list(range(w * w))

    def _export_bmp(self): #vers 3
        """Visible and physical grids to BMP (land white)."""
        if not self._waterpro:
            QMessageBox.information(self, "Export", "Load a binary waterpro.dat first.")
            return
        wp   = self._waterpro
        gw   = wp.grid_w
        stem = Path(self._file_path).stem if self._file_path else "water"
        try:
            from PIL import Image
            for grid, dflt, w, canvas in [
                (wp.vis_grid,  f"{stem}_visible.bmp",  gw,   self._phys_canvas),
                (wp.phys_grid, f"{stem}_physical.bmp", gw*2, self._vis_canvas),
            ]:
                p, _ = QFileDialog.getSaveFileName(self, "Export BMP", dflt, "BMP (*.bmp)")
                if p:
                    px = bytes(255 if grid[i] == 128 else 0 for i in self._bmp_index(canvas, w))
                    img = Image.frombytes("L", (w, w), px)
                    (img if w % 6 == 0 else img.rotate(90)).save(p)
                    self._set_status(f"Exported {Path(p).name}")
        except Exception as e:
            QMessageBox.critical(self, "Export Error", str(e))


    def _import_bmp(self): #vers 4
        """BMP land mask (white = land) into the matching grid; water cells keep
        their level. Colour lake pixels (SOL Water Editor BMPs) stay water."""
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
                attr, canvas, undo, redo = "vis_grid", self._phys_canvas, self._undo_phys, self._redo_phys
            elif w == gw*2 and h == gw*2:
                attr, canvas, undo, redo = "phys_grid", self._vis_canvas, self._undo_vis, self._redo_vis
            else:
                QMessageBox.warning(self, "Size Mismatch",
                    f"{w}x{h} doesn't match {gw}x{gw} or {gw*2}x{gw*2}")
                return
            if w % 6:
                img = img.rotate(-90)
            px = img.tobytes()
            old = getattr(wp, attr)
            grid = bytearray(old)
            for k, i in enumerate(self._bmp_index(canvas, w)):
                if px[k] > 127:                     # white land; red/blue lakes are darker
                    grid[i] = 128
                elif old[i] == 128:
                    grid[i] = 0
            undo.append(bytearray(old)); redo.clear()
            setattr(wp, attr, grid)
            canvas.set_grid(grid)
            self._on_grid_changed()
            self._set_status(f"Imported {attr.split('_')[0]} grid from {Path(p).name}")
        except Exception as e:
            QMessageBox.critical(self, "Import Error", str(e))


    def _edit_level(self, item): #vers 5
        idx = self._levels_list.row(item)
        dlg = QDialog(self)
        _apply_dialog_theme(dlg, self.main_window)
        fl  = QFormLayout(dlg)
        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)

        def _spin(val, lo=-100000, hi=100000, dec=4):
            sp = QDoubleSpinBox()
            sp.setRange(lo, hi)
            sp.setDecimals(dec)
            sp.setValue(val)
            return sp

        if self._waterpro:
            wp = self._waterpro
            if idx < 0 or idx >= wp.water_levels_count:
                return
            dlg.setWindowTitle(f"Edit Water Level {idx}")
            spin = _spin(wp.water_level_data[idx], -1000, 1000)
            fl.addRow(f"Level {idx} Z height:", spin)
            fl.addRow(btns)
            if dlg.exec() == QDialog.DialogCode.Accepted:
                wp.water_level_data[idx] = spin.value()
                self._refresh_levels_list()
                self._on_grid_changed()
        elif self._waterdat:
            rects = self._waterdat.rects
            if idx < 0 or idx >= len(rects):
                return
            dlg.setWindowTitle(f"Edit Water Rect {idx}")
            r = rects[idx]
            sp = [_spin(v, dec=2) for v in r]
            for lbl, w in zip(("Z level", "X left", "Y bottom", "X right", "Y top"), sp):
                fl.addRow(lbl + ":", w)
            fl.addRow(btns)
            if dlg.exec() == QDialog.DialogCode.Accepted:
                self._obj_edit_started()
                rects[idx] = tuple(w.value() for w in sp)
                self._rects_to_objs()
                self._sa_canvas.setup(self._rect_objs, self._waterdat.world_bbox(), keep_view=True)
                self._refresh_levels_list()
                self._mark_dirty(True)
        elif self._sa_water:
            quads = self._sa_water.quads
            if idx < 0 or idx >= len(quads):
                return
            dlg.setWindowTitle(f"Edit Water Quad {idx}")
            from PyQt6.QtWidgets import QSpinBox
            q = quads[idx]
            corner_spins = []
            for n, c in enumerate(q["corners"]):
                xyz = (_spin(c["x"], dec=4), _spin(c["y"], dec=4), _spin(c["f"][0], dec=4))
                fl.addRow(f"Corner {n + 1} X / Y / Z:", self._spin_row(xyz))
                corner_spins.append(xyz)
            fs = QSpinBox()
            fs.setRange(0, 255)
            fs.setValue(q["flag"] if q["flag"] is not None else 0)
            fl.addRow("Flag:", fs)
            fl.addRow(btns)
            if dlg.exec() == QDialog.DialogCode.Accepted:
                for c, (sx, sy, sz) in zip(q["corners"], corner_spins):
                    c["x"], c["y"] = sx.value(), sy.value()
                    c["f"] = [sz.value()] + c["f"][1:]
                if q["flag"] is not None or fs.value():
                    q["flag"] = fs.value()
                self._sa_canvas.setup(self._sa_water.quads, self._sa_water.world_bbox())
                self._refresh_levels_list()
                self._mark_dirty(True)

    @staticmethod
    def _spin_row(widgets): #vers 1
        """Spin boxes side by side in one form row."""
        from PyQt6.QtWidgets import QHBoxLayout, QWidget
        row = QWidget()
        lay = QHBoxLayout(row)
        lay.setContentsMargins(0, 0, 0, 0)
        for w in widgets:
            lay.addWidget(w)
        return row


    def _add_level(self): #vers 4
        if self._waterpro:
            wp = self._waterpro
            if wp.water_levels_count >= WaterproParser.WATER_LEVELS:
                QMessageBox.information(self, "At Maximum",
                    f"Maximum {WaterproParser.WATER_LEVELS} levels.")
                return
            wp.water_level_data[wp.water_levels_count] = 0.0
            wp.water_levels_count += 1
            self._refresh_levels_list()
            self._on_grid_changed()
        elif self._waterdat or self._sa_water:
            x0, y0, x1, y1 = self._current_parser().world_bbox()
            cx, cy = (x0 + x1) / 2, (y0 + y1) / 2
            self._obj_add(cx - 50, cy - 50, cx + 50, cy + 50, 'quad')


    def _del_level(self): #vers 3
        idx = self._levels_list.currentRow()
        if self._waterpro:
            wp = self._waterpro
            if idx < 0 or idx >= wp.water_levels_count:
                return
            wp.water_level_data[idx:wp.water_levels_count-1] = \
                wp.water_level_data[idx+1:wp.water_levels_count]
            wp.water_level_data[wp.water_levels_count-1] = 0.0
            wp.water_levels_count -= 1
            self._refresh_levels_list()
            self._on_grid_changed()
        elif (self._waterdat or self._sa_water) and 0 <= idx < len(self._water_objs()):
            self._sa_canvas.set_selection({idx}, idx)
            self._obj_delete()


    def get_water_quads(self): #vers 1
        """Return SA water quads list for external overlays (e.g. IPL World Map).
        Each quad: {'corners': [{'x':f,'y':f,'f':[...]}, ...], 'flag': int}"""
        return self._sa_water.quads if self._sa_water else []


    def get_water_rects(self): #vers 1
        """Return GTA3/VC water.dat rects for external overlays.
        Each rect: (x1, y1, x2, y2, level)"""
        return self._waterdat.rects if self._waterdat else []


    def translate_water(self, dx: float, dy: float, dz: float = 0.0): #vers 2
        """Translate all loaded water data by (dx, dy, dz) and refresh canvas."""
        changed = False
        if self._sa_water:
            self._sa_water.translate_all(dx, dy, dz)
            self._sa_canvas.setup(self._sa_water.quads, self._sa_water.world_bbox())
            changed = True
        if self._waterdat:
            self._waterdat.translate_all(dx, dy, dz)
            self._rects_to_objs()
            self._sa_canvas.setup(self._rect_objs, self._waterdat.world_bbox())
            changed = True
        if changed:
            self._mark_dirty(True)
            self._set_status(
                f"Water translated  dX={dx:+.1f}  dY={dy:+.1f}  dZ={dz:+.1f}")
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Water Workshop: translated by dX={dx:+.1f} dY={dy:+.1f} dZ={dz:+.1f}")


    def _on_quad_selected(self, idx): #vers 2
        objs = self._water_objs()
        if not 0 <= idx < len(objs):
            return
        self._levels_list.blockSignals(True)
        self._levels_list.setCurrentRow(idx)
        self._levels_list.blockSignals(False)
        q = objs[idx]
        c = q["corners"][0]
        self._obj_z_spin.setValue(WaterObjectCanvas._z(q))
        n = len(self._sa_canvas.selection())
        kind = "Rect" if q.get("_rect") else ("Triangle" if len(q["corners"]) == 3 else "Quad")
        self._set_status(f"{kind} {idx}: ({c['x']:.1f},{c['y']:.1f})  Z={WaterObjectCanvas._z(q):.2f}"
                         + (f"  flag={q['flag']}" if q.get('flag') is not None else "")
                         + (f"  ({n} selected)" if n > 1 else ""))

    def _on_list_row(self, row): #vers 1
        """List row picked: select that object on the canvas."""
        if (self._waterdat or self._sa_water) and 0 <= row < len(self._water_objs()):
            self._sa_canvas.set_selection({row}, row)

    #    Water objects (water.dat): shared by SA/IV quads and III/VC rects

    def _water_objs(self): #vers 1
        """Editable object list for the loaded water.dat."""
        if self._sa_water:
            return self._sa_water.quads
        return self._rect_objs if self._waterdat else []

    def _rects_to_objs(self): #vers 1
        """III/VC rects (level, x1, y1, x2, y2) as 4-corner objects (Z-order)."""
        self._rect_objs = [{"_rect": True, "corners": [
            {"x": x, "y": y, "f": [r[0]]} for y in (r[2], r[4]) for x in (r[1], r[3])]}
            for r in self._waterdat.rects]

    def _objs_to_rects(self): #vers 1
        """Write rect objects back to the water.dat rects."""
        self._waterdat.rects = [
            (WaterObjectCanvas._z(q), min(c["x"] for c in q["corners"]), min(c["y"] for c in q["corners"]),
             max(c["x"] for c in q["corners"]), max(c["y"] for c in q["corners"]))
            for q in self._rect_objs]

    def _obj_edit_started(self): #vers 1
        """Undo point before an object edit."""
        import copy
        self._undo_objs.append(copy.deepcopy(self._water_objs()))
        del self._undo_objs[:-50]
        self._redo_objs.clear()

    def _obj_edited(self): #vers 1
        """After an object edit: sync rects, dirty flag, list refresh (coalesced)."""
        if self._waterdat:
            self._objs_to_rects()
        self._mark_dirty(True)
        if not getattr(self, '_obj_refresh_pending', False):
            from PyQt6.QtCore import QTimer
            self._obj_refresh_pending = True
            QTimer.singleShot(150, self._obj_refresh)

    def _obj_refresh(self): #vers 1
        self._obj_refresh_pending = False
        row = self._sa_canvas._sel_idx
        self._levels_list.blockSignals(True)
        self._refresh_levels_list()
        if row >= 0:
            self._levels_list.setCurrentRow(row)
        self._levels_list.blockSignals(False)
        self._on_quad_selected(row)

    def _obj_set_objs(self, objs): #vers 1
        """Replace the object list (undo/redo) and redraw."""
        if self._sa_water:
            self._sa_water.quads = objs
        else:
            self._rect_objs = objs
            self._objs_to_rects()
        self._sa_canvas.setup(objs, self._current_parser().world_bbox(), keep_view=True)

    def _obj_undo_redo(self, src, dst, label): #vers 1
        import copy
        if not src:
            self._set_status(f"Nothing to {label.lower()}")
            return
        dst.append(copy.deepcopy(self._water_objs()))
        self._obj_set_objs(src.pop())
        self._obj_edited()
        self._set_status(label)

    def _set_obj_tool(self, kind): #vers 1
        """Select (None) or add 'quad' / 'tri' mode on the object canvas."""
        if kind == 'tri' and not self._sa_water:
            self._set_status("Triangles are SA / IV water.dat only")
            kind = None
        self._sa_canvas.add_kind = kind
        for k, b in self._obj_btns.items():
            b.setChecked(k == kind)
        self._view_tabs.setCurrentIndex(2)

    def _obj_set_snap(self, v): #vers 1
        self._sa_canvas.snap = v
        self.WS._data["object_snap"] = v
        self.WS.save()

    def _obj_add(self, x0, y0, x1, y1, kind): #vers 1
        """New quad / triangle / rect from a box, at the selection's (or last) Z."""
        objs = self._water_objs()
        if not (self._sa_water or self._waterdat):
            return
        sel = self._sa_canvas.selection()
        ref = objs[sel[0]] if sel else (objs[-1] if objs else None)
        z = WaterObjectCanvas._z(ref) if ref else 0.0
        self._obj_edit_started()
        pts = [(x0, y0), (x1, y0), (x0, y1)] if kind == 'tri' else [(x0, y0), (x1, y0), (x0, y1), (x1, y1)]
        if self._sa_water:
            tmpl = ref["corners"][0]["f"][1:] if ref else [0.0, 0.0, 0.0, 0.0]
            obj = {"corners": [{"x": x, "y": y, "f": [z] + list(tmpl)} for x, y in pts],
                   "flag": ref.get("flag", 1) if ref else 1,
                   "extra": list(ref.get("extra", ())) if ref else []}
        else:
            obj = {"_rect": True, "corners": [{"x": x, "y": y, "f": [z]} for x, y in pts]}
        objs.append(obj)
        self._sa_canvas.setup(objs, self._current_parser().world_bbox(), keep_view=True)
        self._sa_canvas.set_selection({len(objs) - 1})
        self._obj_edited()

    def _obj_delete(self): #vers 1
        """Delete the selected objects."""
        sel = self._sa_canvas.selection()
        if not sel:
            return
        self._obj_edit_started()
        objs = self._water_objs()
        for i in reversed(sel):
            del objs[i]
        self._sa_canvas.set_selection(set())
        self._sa_canvas.setup(objs, self._current_parser().world_bbox(), keep_view=True)
        self._obj_edited()
        self._set_status(f"Deleted {len(sel)} object(s)")

    def _obj_set_z(self, z): #vers 2
        """Set Z of the selected objects (Z field)."""
        if not self._sa_canvas.selection():
            return
        self._sa_canvas.set_selected_z(z, start=True)


    def closeEvent(self, ev): #vers 1
        if self.standalone_mode and self._dirty:
            r = QMessageBox.question(
                self, App_name, "Save changes before closing?",
                QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard
                | QMessageBox.StandardButton.Cancel)
            if r == QMessageBox.StandardButton.Cancel:
                ev.ignore()
                return
            if r == QMessageBox.StandardButton.Save:
                self._save_file()
                if self._dirty:
                    ev.ignore()
                    return
        self.ribbon_save_state()
        super().closeEvent(ev)

    def _show_stats(self): #vers 1
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


    def _clear_recent(self): #vers 2
        self.WS._data["recent_files"] = []
        self.WS.save()
        self._set_status("Recent files cleared")


    def _show_offset_dialog(self): #vers 2
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


# Standalone launcher

if __name__ == "__main__": #vers 2
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
