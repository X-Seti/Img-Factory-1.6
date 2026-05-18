#!/usr/bin/env python3
#this belongs in apps/components/Path_Workshop/path_workshop.py - Version: 1
# X-Seti - May08 2026 - Img Factory 1.6 - Path Workshop

"""
Path Workshop — tabbed editor for GTA III/VC/SA path files.
Tabs: Train Paths | Flight Paths | Static Paths (spath0 VC text)

Each tab shows:
  Left  = waypoint list (index, x, y, z, speed, flags)
  Right = 2D overhead map canvas with waypoints plotted, lines connecting them
  Bottom = selected waypoint field editor

Map canvas: uses radar tiles as background if available, otherwise plain grid.
Supports: train.dat, train2.dat, flight.dat, flight2.dat, flight3.dat, spath0.dat (VC text)
"""

##Methods list -
# Waypoint.__init__
# PathRoute.__init__
# TrainParser.__init__
# TrainParser.load
# TrainParser.save
# TrainParser._detect_terminator
# FlightParser.__init__
# FlightParser.load
# FlightParser.save
# SpathParser.__init__
# SpathParser.load
# SpathParser.save
# PathMapCanvas.__init__
# PathMapCanvas.set_route
# PathMapCanvas.set_radar_tiles
# PathMapCanvas.paintEvent
# PathMapCanvas.mousePressEvent
# PathMapCanvas.mouseMoveEvent
# PathMapCanvas.mouseReleaseEvent
# PathMapCanvas.wheelEvent
# PathMapCanvas._world_to_screen
# PathMapCanvas._screen_to_world
# PathMapCanvas._hit_test
# TrainTab.__init__
# TrainTab._build_ui
# TrainTab.load_file
# TrainTab.save_file
# TrainTab._refresh_list
# TrainTab._on_select
# TrainTab._populate_fields
# TrainTab._on_field_changed
# TrainTab._add_waypoint
# TrainTab._delete_waypoint
# TrainTab._move_up
# TrainTab._move_down
# TrainTab._on_map_click
# FlightTab.__init__
# FlightTab._build_ui
# FlightTab.load_file
# FlightTab.save_file
# SpathTab.__init__
# SpathTab._build_ui
# SpathTab.load_file
# SpathTab.save_file
# PathWorkshop.__init__
# PathWorkshop.setup_ui
# PathWorkshop._open_file
# PathWorkshop._save_file
# PathWorkshop._load_radar_background
# PathWorkshop._build_menus_into_qmenu
# open_path_workshop

import sys, os, math
from pathlib import Path
from typing import List, Optional, Tuple
from dataclasses import dataclass, field

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = Path(current_dir).parents[2]
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QLabel, QLineEdit,
    QListWidget, QListWidgetItem, QScrollArea, QTabWidget,
    QDoubleSpinBox, QSpinBox, QPushButton, QFileDialog, QMessageBox,
    QApplication, QFormLayout, QFrame, QSizePolicy, QMenu, QCheckBox,
    QGroupBox, QAbstractItemView
)
from PyQt6.QtCore import Qt, QPointF, QRectF, pyqtSignal
from PyQt6.QtGui import (
    QColor, QFont, QPainter, QPen, QBrush, QImage, QPolygonF, QKeySequence
)

try:
    from apps.components.Tmp_Template.gui_workshop import GUIWorkshop
except ImportError:
    from apps.methods.gui_workshop import GUIWorkshop


# ─────────────────────────────────────────────────────────────────────────────
# Data classes
# ─────────────────────────────────────────────────────────────────────────────

@dataclass
class Waypoint:  #vers 1
    x:     float = 0.0
    y:     float = 0.0
    z:     float = 0.0
    speed: float = 10.0
    flags: int   = 0


@dataclass
class PathRoute:  #vers 1
    name:      str            = ""
    waypoints: List[Waypoint] = field(default_factory=list)
    comment:   str            = ""


# ─────────────────────────────────────────────────────────────────────────────
# Parsers
# ─────────────────────────────────────────────────────────────────────────────

class TrainParser:  #vers 1
    """Parses train.dat / train2.dat  (VC + SA text format)."""

    def __init__(self):  #vers 1
        self.routes:       List[PathRoute] = []
        self.header_lines: List[str]       = []

    def load(self, path: str) -> bool:  #vers 1
        try:
            self.routes.clear(); self.header_lines.clear()
            route = PathRoute(name=os.path.basename(path))
            with open(path, 'r', encoding='latin-1') as f:
                for ln in f:
                    s = ln.strip()
                    if not s or s.startswith(';') or s.startswith('#'):
                        if not route.waypoints:
                            self.header_lines.append(ln)
                        continue
                    parts = s.replace(',', ' ').split()
                    if len(parts) < 3:
                        continue
                    try:
                        x, y, z = float(parts[0]), float(parts[1]), float(parts[2])
                        speed = float(parts[3]) if len(parts) > 3 else 10.0
                        flags = int(parts[4])   if len(parts) > 4 else 0
                        if speed < 0:
                            # Terminator node — still add it with speed=0 for display
                            route.waypoints.append(Waypoint(x, y, z, 0.0, flags))
                            break
                        route.waypoints.append(Waypoint(x, y, z, speed, flags))
                    except ValueError:
                        continue
            if route.waypoints:
                self.routes.append(route)
            return True
        except Exception as ex:
            print(f"TrainParser.load: {ex}"); return False

    def save(self, path: str) -> bool:  #vers 1
        try:
            with open(path, 'w', encoding='latin-1') as f:
                for ln in self.header_lines:
                    f.write(ln)
                if self.routes:
                    for wp in self.routes[0].waypoints[:-1]:
                        f.write(f"{wp.x:.3f}  {wp.y:.3f}  {wp.z:.3f}  {wp.speed:.2f}  {wp.flags}\n")
                    # Terminator
                    f.write(f"0, 0, 0, -1\n")
            return True
        except Exception as ex:
            print(f"TrainParser.save: {ex}"); return False


class FlightParser:  #vers 1
    """Parses flight.dat / flight2.dat / flight3.dat  (VC text format)."""

    def __init__(self):  #vers 1
        self.routes:       List[PathRoute] = []
        self.header_lines: List[str]       = []

    def load(self, path: str) -> bool:  #vers 1
        try:
            self.routes.clear(); self.header_lines.clear()
            route = PathRoute(name=os.path.basename(path))
            with open(path, 'r', encoding='latin-1') as f:
                for ln in f:
                    s = ln.strip()
                    if not s or s.startswith(';') or s.startswith('#'):
                        if not route.waypoints:
                            self.header_lines.append(ln)
                        continue
                    parts = s.replace(',', ' ').split()
                    if len(parts) < 3:
                        continue
                    try:
                        x, y, z = float(parts[0]), float(parts[1]), float(parts[2])
                        speed = float(parts[3]) if len(parts) > 3 else 30.0
                        if speed < 0:
                            break
                        route.waypoints.append(Waypoint(x, y, z, speed))
                    except ValueError:
                        continue
            if route.waypoints:
                self.routes.append(route)
            return True
        except Exception as ex:
            print(f"FlightParser.load: {ex}"); return False

    def save(self, path: str) -> bool:  #vers 1
        try:
            with open(path, 'w', encoding='latin-1') as f:
                for ln in self.header_lines:
                    f.write(ln)
                if self.routes:
                    for wp in self.routes[0].waypoints:
                        f.write(f"{wp.x:.3f}  {wp.y:.3f}  {wp.z:.3f}  {wp.speed:.2f}\n")
                    f.write(f"0  0  0  -1\n")
            return True
        except Exception as ex:
            print(f"FlightParser.save: {ex}"); return False


class SpathParser:  #vers 1
    """Parses spath0.dat (VC text format: x y z per line, terminated by END)."""

    def __init__(self):  #vers 1
        self.routes:       List[PathRoute] = []
        self.header_lines: List[str]       = []

    def load(self, path: str) -> bool:  #vers 1
        try:
            self.routes.clear(); self.header_lines.clear()
            route = PathRoute(name=os.path.basename(path))
            with open(path, 'r', encoding='latin-1') as f:
                for ln in f:
                    s = ln.strip()
                    if not s or s.startswith(';'):
                        if not route.waypoints:
                            self.header_lines.append(ln)
                        continue
                    if s.upper() == 'END':
                        break
                    parts = s.split()
                    if len(parts) < 3:
                        continue
                    try:
                        x, y, z = float(parts[0]), float(parts[1]), float(parts[2])
                        route.waypoints.append(Waypoint(x, y, z))
                    except ValueError:
                        continue
            if route.waypoints:
                self.routes.append(route)
            return True
        except Exception as ex:
            print(f"SpathParser.load: {ex}"); return False

    def save(self, path: str) -> bool:  #vers 1
        try:
            with open(path, 'w', encoding='latin-1') as f:
                for ln in self.header_lines:
                    f.write(ln)
                if self.routes:
                    for wp in self.routes[0].waypoints:
                        f.write(f"{wp.x:.3f}  {wp.y:.3f}  {wp.z:.3f}\n")
                    f.write("END\n")
            return True
        except Exception as ex:
            print(f"SpathParser.save: {ex}"); return False


# ─────────────────────────────────────────────────────────────────────────────
# Map canvas
# ─────────────────────────────────────────────────────────────────────────────

class PathMapCanvas(QWidget):  #vers 1
    """2D overhead map canvas. Draws radar tiles as background + path overlay.

    Coordinate conventions:
      GTA world: X=east, Y=north, Z=up  (Y increases going north)
      Screen:    x=right, y=down
    Map bounds VC:  X=-3000..3000, Y=-3000..3000
    Map bounds SA:  X=-3000..3000, Y=-3000..3000
    """

    node_clicked    = pyqtSignal(int)       # waypoint index clicked
    node_moved      = pyqtSignal(int, float, float, float)  # idx, x, y, z

    # Map world bounds (VC/SA share similar extents for the radar)
    WORLD_MIN_X = -3000.0
    WORLD_MAX_X =  3000.0
    WORLD_MIN_Y = -3000.0
    WORLD_MAX_Y =  3000.0

    # Visual settings
    NODE_RADIUS   = 6
    SEL_RADIUS    = 9
    LINE_WIDTH    = 2.0
    NODE_COLOR    = QColor(80, 220, 120)       # green nodes
    LINE_COLOR    = QColor(80, 180, 255, 200)  # blue path lines
    SEL_COLOR     = QColor(255, 220, 50)       # yellow selected
    TERM_COLOR    = QColor(255, 80, 80)        # red terminator
    ARROW_SIZE    = 10

    def __init__(self, parent=None):  #vers 1
        super().__init__(parent)
        self._routes:   List[PathRoute] = []
        self._radar:    Optional[QImage] = None   # composited radar background
        self._sel_idx:  int   = -1
        self._drag_idx: int   = -1
        self._drag_offset = QPointF(0, 0)
        self._pan_x:    float = 0.0
        self._pan_y:    float = 0.0
        self._zoom:     float = 1.0
        self._last_pan  = None
        self._show_arrows   = True
        self._show_labels   = True
        self._active_route  = 0
        self.setMinimumSize(400, 400)
        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.WheelFocus)
        # Initial fit
        self._zoom = 1.0
        self._pan_x = 0.0
        self._pan_y = 0.0

    def set_route(self, route_idx: int, routes: List[PathRoute]):  #vers 1
        self._routes    = routes
        self._active_route = route_idx
        self._sel_idx   = -1
        self.fit_all()
        self.update()

    def set_radar_tiles(self, image: QImage):  #vers 1
        self._radar = image
        self.update()

    def fit_all(self):  #vers 1
        """Auto-fit zoom/pan so all waypoints are visible."""
        if not self._routes:
            return
        all_wps = [wp for r in self._routes for wp in r.waypoints]
        if not all_wps:
            return
        xs = [wp.x for wp in all_wps]
        ys = [wp.y for wp in all_wps]
        min_x, max_x = min(xs), max(xs)
        min_y, max_y = min(ys), max(ys)
        # Add 10% padding
        pad_x = max((max_x - min_x) * 0.1, 100.0)
        pad_y = max((max_y - min_y) * 0.1, 100.0)
        min_x -= pad_x; max_x += pad_x
        min_y -= pad_y; max_y += pad_y
        W, H = max(self.width(), 400), max(self.height(), 400)
        span_x = max_x - min_x; span_y = max_y - min_y
        if span_x <= 0 or span_y <= 0:
            return
        self._zoom = min(W / span_x, H / span_y)
        # Centre
        cx = (min_x + max_x) / 2.0
        cy = (min_y + max_y) / 2.0
        sx, sy = self._world_to_screen_raw(cx, cy)
        self._pan_x = W / 2 - sx
        self._pan_y = H / 2 - sy
        self.update()

    def _world_to_screen_raw(self, wx: float, wy: float) -> Tuple[float, float]:
        """World → screen WITHOUT pan (used during fit_all)."""
        # Flip Y: GTA Y increases northward, screen Y increases downward
        sx = wx * self._zoom
        sy = -wy * self._zoom
        return sx, sy

    def _world_to_screen(self, wx: float, wy: float) -> QPointF:  #vers 1
        sx, sy = self._world_to_screen_raw(wx, wy)
        return QPointF(sx + self._pan_x, sy + self._pan_y)

    def _screen_to_world(self, sx: float, sy: float) -> Tuple[float, float]:  #vers 1
        wx = (sx - self._pan_x) / self._zoom
        wy = -(sy - self._pan_y) / self._zoom
        return wx, wy

    def _hit_test(self, sx: float, sy: float) -> int:  #vers 1
        """Return waypoint index under screen point, or -1."""
        r = self.NODE_RADIUS + 4
        route = self._routes[self._active_route] if self._routes else None
        if not route:
            return -1
        for i, wp in enumerate(route.waypoints):
            pt = self._world_to_screen(wp.x, wp.y)
            if abs(pt.x() - sx) < r and abs(pt.y() - sy) < r:
                return i
        return -1

    def paintEvent(self, event):  #vers 1
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        W, H = self.width(), self.height()

        # Background: radar or plain dark grid
        if self._radar:
            # Scale radar image to cover world bounds in screen space
            tl = self._world_to_screen(self.WORLD_MIN_X, self.WORLD_MAX_Y)
            br = self._world_to_screen(self.WORLD_MAX_X, self.WORLD_MIN_Y)
            p.drawImage(
                QRectF(tl, br),
                self._radar,
                QRectF(0, 0, self._radar.width(), self._radar.height()))
        else:
            p.fillRect(self.rect(), QColor(25, 30, 40))
            # Grid lines every 500 world units
            p.setPen(QPen(QColor(50, 60, 80), 1))
            step = 500.0
            x = math.ceil(self.WORLD_MIN_X / step) * step
            while x <= self.WORLD_MAX_X:
                pt1 = self._world_to_screen(x, self.WORLD_MIN_Y)
                pt2 = self._world_to_screen(x, self.WORLD_MAX_Y)
                p.drawLine(pt1, pt2)
                x += step
            y = math.ceil(self.WORLD_MIN_Y / step) * step
            while y <= self.WORLD_MAX_Y:
                pt1 = self._world_to_screen(self.WORLD_MIN_X, y)
                pt2 = self._world_to_screen(self.WORLD_MAX_X, y)
                p.drawLine(pt1, pt2)
                y += step
            # Axis
            p.setPen(QPen(QColor(80, 80, 120), 1, Qt.PenStyle.DashLine))
            ox = self._world_to_screen(0, self.WORLD_MIN_Y)
            ox2 = self._world_to_screen(0, self.WORLD_MAX_Y)
            p.drawLine(ox, ox2)
            oy = self._world_to_screen(self.WORLD_MIN_X, 0)
            oy2 = self._world_to_screen(self.WORLD_MAX_X, 0)
            p.drawLine(oy, oy2)

        if not self._routes:
            p.setPen(QColor(150, 150, 180))
            p.setFont(QFont("Arial", 12))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "No path loaded")
            return

        # Draw each route with distinct colour
        route_colors = [
            self.LINE_COLOR,
            QColor(255, 140, 60, 200),   # orange — train 2
            QColor(220, 80, 220, 200),   # purple — flight 3
        ]

        for ri, route in enumerate(self._routes):
            wps = route.waypoints
            if not wps:
                continue
            col = route_colors[ri % len(route_colors)]
            node_col = self.NODE_COLOR if ri == 0 else QColor(255, 180, 60)

            # Path lines
            p.setPen(QPen(col, self.LINE_WIDTH))
            for i in range(len(wps) - 1):
                pt1 = self._world_to_screen(wps[i].x, wps[i].y)
                pt2 = self._world_to_screen(wps[i+1].x, wps[i+1].y)
                p.drawLine(pt1, pt2)
                # Direction arrow
                if self._show_arrows:
                    mx = (pt1.x() + pt2.x()) / 2
                    my = (pt1.y() + pt2.y()) / 2
                    dx = pt2.x() - pt1.x(); dy = pt2.y() - pt1.y()
                    length = math.sqrt(dx*dx + dy*dy)
                    if length > 20:
                        dx /= length; dy /= length
                        asz = self.ARROW_SIZE
                        ax = mx - dx * asz; ay = my - dy * asz
                        lx = ax + dy * asz * 0.4; ly = ay - dx * asz * 0.4
                        rx = ax - dy * asz * 0.4; ry = ay + dx * asz * 0.4
                        p.setBrush(QBrush(col))
                        p.drawPolygon(QPolygonF([
                            QPointF(mx, my), QPointF(lx, ly), QPointF(rx, ry)]))
                        p.setBrush(Qt.BrushStyle.NoBrush)

            # Closing line for train (loops back)
            if len(wps) > 1 and 'train' in route.name.lower():
                pt1 = self._world_to_screen(wps[-1].x, wps[-1].y)
                pt2 = self._world_to_screen(wps[0].x, wps[0].y)
                p.setPen(QPen(col, self.LINE_WIDTH, Qt.PenStyle.DashLine))
                p.drawLine(pt1, pt2)

            # Nodes
            for i, wp in enumerate(wps):
                pt = self._world_to_screen(wp.x, wp.y)
                r = self.SEL_RADIUS if i == self._sel_idx else self.NODE_RADIUS
                if i == self._sel_idx:
                    p.setBrush(QBrush(self.SEL_COLOR))
                    p.setPen(QPen(QColor(255, 255, 255), 1))
                elif i == len(wps) - 1 and 'train' in route.name.lower():
                    p.setBrush(QBrush(self.TERM_COLOR))
                    p.setPen(QPen(QColor(255, 200, 200), 1))
                else:
                    p.setBrush(QBrush(node_col))
                    p.setPen(QPen(QColor(0, 0, 0, 120), 1))
                p.drawEllipse(pt, r, r)
                if self._show_labels:
                    p.setPen(QColor(220, 220, 255))
                    p.setFont(QFont("Arial", 7))
                    p.drawText(int(pt.x()) + r + 2, int(pt.y()) + 4, str(i))

        # HUD
        p.setPen(QColor(180, 180, 200))
        p.setFont(QFont("Arial", 8))
        if self._routes:
            active = self._routes[self._active_route]
            p.drawText(6, 16, f"{active.name}  —  {len(active.waypoints)} waypoints   zoom:{self._zoom:.2f}x")
        if self._sel_idx >= 0 and self._routes:
            wp = self._routes[self._active_route].waypoints[self._sel_idx]
            p.drawText(6, H - 6, f"[{self._sel_idx}]  X:{wp.x:.2f}  Y:{wp.y:.2f}  Z:{wp.z:.2f}  spd:{wp.speed:.1f}")

    def mousePressEvent(self, event):  #vers 1
        pos = event.position()
        if event.button() == Qt.MouseButton.LeftButton:
            idx = self._hit_test(pos.x(), pos.y())
            if idx >= 0:
                self._sel_idx = idx
                self._drag_idx = idx
                self.node_clicked.emit(idx)
            else:
                self._sel_idx = -1
                self._drag_idx = -1
            self.update()
        elif event.button() == Qt.MouseButton.MiddleButton:
            self._last_pan = pos

    def mouseMoveEvent(self, event):  #vers 1
        pos = event.position()
        if event.buttons() & Qt.MouseButton.MiddleButton and self._last_pan:
            dx = pos.x() - self._last_pan.x()
            dy = pos.y() - self._last_pan.y()
            self._pan_x += dx; self._pan_y += dy
            self._last_pan = pos
            self.update()
        elif event.buttons() & Qt.MouseButton.LeftButton and self._drag_idx >= 0:
            wx, wy = self._screen_to_world(pos.x(), pos.y())
            wp = self._routes[self._active_route].waypoints[self._drag_idx]
            wp.x = wx; wp.y = wy
            self.node_moved.emit(self._drag_idx, wx, wy, wp.z)
            self.update()
        # Right-drag = pan
        elif event.buttons() & Qt.MouseButton.RightButton:
            if self._last_pan:
                dx = pos.x() - self._last_pan.x()
                dy = pos.y() - self._last_pan.y()
                self._pan_x += dx; self._pan_y += dy
            self._last_pan = pos
            self.update()

    def mouseReleaseEvent(self, event):  #vers 1
        self._drag_idx = -1
        self._last_pan = None

    def wheelEvent(self, event):  #vers 1
        factor = 1.15 if event.angleDelta().y() > 0 else 1.0 / 1.15
        pos = event.position()
        # Zoom toward cursor
        wx, wy = self._screen_to_world(pos.x(), pos.y())
        self._zoom *= factor
        self._zoom = max(0.02, min(self._zoom, 50.0))
        sx, sy = self._world_to_screen_raw(wx, wy)
        self._pan_x = pos.x() - sx
        self._pan_y = pos.y() - sy
        self.update()

    def keyPressEvent(self, event):  #vers 1
        if event.key() == Qt.Key.Key_F:
            self.fit_all()
        elif event.key() == Qt.Key.Key_A and self._show_arrows:
            self._show_arrows = not self._show_arrows
            self.update()


# ─────────────────────────────────────────────────────────────────────────────
# Shared waypoint editor panel (reused by all tabs)
# ─────────────────────────────────────────────────────────────────────────────

class _WaypointPanel(QWidget):  #vers 1
    """Left panel: waypoint list + field editor + buttons."""
    changed = pyqtSignal()

    def __init__(self, has_speed: bool = True, has_flags: bool = False, parent=None):
        super().__init__(parent)
        self._parser    = None
        self._cur_idx   = -1
        self._blocking  = False
        self._has_speed = has_speed
        self._has_flags = has_flags
        self._build_ui()

    def _build_ui(self):  #vers 1
        root = QVBoxLayout(self)
        root.setContentsMargins(2, 2, 2, 2); root.setSpacing(4)

        root.addWidget(QLabel("Waypoints"))
        self._list = QListWidget()
        self._list.currentRowChanged.connect(self._on_select)
        root.addWidget(self._list)

        # Field editor
        grp = QGroupBox("Selected Waypoint")
        fl = QFormLayout(grp); fl.setSpacing(3)
        self._wx = QDoubleSpinBox(); self._wx.setRange(-10000, 10000); self._wx.setDecimals(3)
        self._wy = QDoubleSpinBox(); self._wy.setRange(-10000, 10000); self._wy.setDecimals(3)
        self._wz = QDoubleSpinBox(); self._wz.setRange(-1000,  1000);  self._wz.setDecimals(3)
        fl.addRow("X", self._wx)
        fl.addRow("Y", self._wy)
        fl.addRow("Z", self._wz)
        if self._has_speed:
            self._spd = QDoubleSpinBox(); self._spd.setRange(0, 300); self._spd.setDecimals(2)
            fl.addRow("Speed", self._spd)
        else:
            self._spd = None
        if self._has_flags:
            self._flags = QSpinBox(); self._flags.setRange(0, 255)
            fl.addRow("Flags", self._flags)
        else:
            self._flags = None
        for w in [self._wx, self._wy, self._wz]:
            w.valueChanged.connect(self._on_edit)
        if self._spd:   self._spd.valueChanged.connect(self._on_edit)
        if self._flags: self._flags.valueChanged.connect(self._on_edit)
        root.addWidget(grp)

        # Buttons
        br = QHBoxLayout()
        for lbl, fn in [("Add", self._add), ("Del", self._delete),
                         ("↑", self._move_up), ("↓", self._move_down)]:
            b = QPushButton(lbl); b.setFixedHeight(24)
            b.clicked.connect(fn); br.addWidget(b)
        root.addLayout(br)

    def set_parser(self, parser, route_idx: int = 0):  #vers 1
        self._parser = parser
        self._cur_idx = -1
        self._refresh()

    def _route(self):  #vers 1
        if self._parser and self._parser.routes:
            return self._parser.routes[0]
        return None

    def _refresh(self):  #vers 1
        self._list.clear()
        r = self._route()
        if not r: return
        for i, wp in enumerate(r.waypoints):
            self._list.addItem(f"[{i:3d}]  {wp.x:9.2f}  {wp.y:9.2f}  {wp.z:6.2f}"
                               + (f"  {wp.speed:.0f}" if self._has_speed else ""))

    def _on_select(self, row: int):  #vers 1
        r = self._route()
        if not r or row < 0 or row >= len(r.waypoints): return
        self._cur_idx = row
        self._blocking = True
        wp = r.waypoints[row]
        self._wx.setValue(wp.x); self._wy.setValue(wp.y); self._wz.setValue(wp.z)
        if self._spd:   self._spd.setValue(wp.speed)
        if self._flags: self._flags.setValue(wp.flags)
        self._blocking = False

    def _on_edit(self):  #vers 1
        if self._blocking or self._cur_idx < 0: return
        r = self._route()
        if not r: return
        wp = r.waypoints[self._cur_idx]
        wp.x = self._wx.value(); wp.y = self._wy.value(); wp.z = self._wz.value()
        if self._spd:   wp.speed = self._spd.value()
        if self._flags: wp.flags = self._flags.value()
        # Update list item text
        item = self._list.item(self._cur_idx)
        if item:
            item.setText(f"[{self._cur_idx:3d}]  {wp.x:9.2f}  {wp.y:9.2f}  {wp.z:6.2f}"
                         + (f"  {wp.speed:.0f}" if self._has_speed else ""))
        self.changed.emit()

    def select_waypoint(self, idx: int):  #vers 1
        if 0 <= idx < self._list.count():
            self._list.setCurrentRow(idx)

    def update_waypoint_pos(self, idx: int, x: float, y: float, z: float):  #vers 1
        r = self._route()
        if not r or idx >= len(r.waypoints): return
        r.waypoints[idx].x = x; r.waypoints[idx].y = y
        item = self._list.item(idx)
        if item:
            wp = r.waypoints[idx]
            item.setText(f"[{idx:3d}]  {wp.x:9.2f}  {wp.y:9.2f}  {wp.z:6.2f}"
                         + (f"  {wp.speed:.0f}" if self._has_speed else ""))
        if idx == self._cur_idx:
            self._blocking = True
            self._wx.setValue(x); self._wy.setValue(y)
            self._blocking = False

    def _add(self):  #vers 1
        r = self._route()
        if not r: return
        wp = Waypoint()
        if r.waypoints: wp = Waypoint(r.waypoints[-1].x, r.waypoints[-1].y, r.waypoints[-1].z)
        r.waypoints.append(wp)
        self._refresh()
        self._list.setCurrentRow(len(r.waypoints)-1)
        self.changed.emit()

    def _delete(self):  #vers 1
        r = self._route()
        if not r or self._cur_idx < 0: return
        r.waypoints.pop(self._cur_idx)
        self._cur_idx = -1
        self._refresh()
        self.changed.emit()

    def _move_up(self):  #vers 1
        r = self._route()
        if not r or self._cur_idx <= 0: return
        i = self._cur_idx
        r.waypoints[i], r.waypoints[i-1] = r.waypoints[i-1], r.waypoints[i]
        self._cur_idx = i - 1
        self._refresh()
        self._list.setCurrentRow(self._cur_idx)
        self.changed.emit()

    def _move_down(self):  #vers 1
        r = self._route()
        if not r or self._cur_idx < 0 or self._cur_idx >= len(r.waypoints)-1: return
        i = self._cur_idx
        r.waypoints[i], r.waypoints[i+1] = r.waypoints[i+1], r.waypoints[i]
        self._cur_idx = i + 1
        self._refresh()
        self._list.setCurrentRow(self._cur_idx)
        self.changed.emit()


# ─────────────────────────────────────────────────────────────────────────────
# Per-file tabs
# ─────────────────────────────────────────────────────────────────────────────

def _make_path_tab(parser_cls, has_speed=True, has_flags=False):  #vers 1
    """Factory: returns a QWidget tab for a given parser class."""

    class _Tab(QWidget):
        def __init__(self, parent=None):
            super().__init__(parent)
            self._parser = parser_cls()
            self._path: Optional[str] = None
            self._build_ui()

        def _build_ui(self):
            root = QHBoxLayout(self); root.setContentsMargins(4,4,4,4)
            sp = QSplitter(Qt.Orientation.Horizontal)
            self._panel = _WaypointPanel(has_speed=has_speed, has_flags=has_flags)
            self._map   = PathMapCanvas()
            self._panel.changed.connect(self._map.update)
            self._panel._list.currentRowChanged.connect(
                lambda row: self._map.__setattr__('_sel_idx', row) or self._map.update())
            self._map.node_clicked.connect(self._panel.select_waypoint)
            self._map.node_moved.connect(
                lambda i,x,y,z: self._panel.update_waypoint_pos(i,x,y,z))
            # Toolbar above map
            right = QWidget(); rl = QVBoxLayout(right); rl.setContentsMargins(0,0,0,0)
            tb = QHBoxLayout()
            fit_btn = QPushButton("Fit [F]"); fit_btn.setFixedHeight(24)
            fit_btn.clicked.connect(self._map.fit_all)
            arrows_cb = QCheckBox("Arrows")
            arrows_cb.setChecked(True)
            arrows_cb.stateChanged.connect(lambda v: setattr(self._map,'_show_arrows',bool(v)) or self._map.update())
            labels_cb = QCheckBox("Labels")
            labels_cb.setChecked(True)
            labels_cb.stateChanged.connect(lambda v: setattr(self._map,'_show_labels',bool(v)) or self._map.update())
            tb.addWidget(fit_btn); tb.addWidget(arrows_cb); tb.addWidget(labels_cb)
            tb.addStretch()
            self._status = QLabel("No file loaded"); tb.addWidget(self._status)
            rl.addLayout(tb); rl.addWidget(self._map, 1)
            sp.addWidget(self._panel); sp.addWidget(right)
            sp.setSizes([260, 740]); root.addWidget(sp)

        def load_file(self, path: str) -> bool:
            ok = self._parser.load(path)
            if ok:
                self._path = path
                self._panel.set_parser(self._parser)
                self._map.set_route(0, self._parser.routes)
                n = len(self._parser.routes[0].waypoints) if self._parser.routes else 0
                self._status.setText(f"{os.path.basename(path)}  —  {n} waypoints")
            return ok

        def save_file(self, path: str) -> bool:
            return self._parser.save(path)

        @property
        def current_path(self): return self._path

        def set_radar(self, image):
            self._map.set_radar_tiles(image)

    return _Tab


TrainTab   = _make_path_tab(TrainParser,  has_speed=True,  has_flags=True)
FlightTab  = _make_path_tab(FlightParser, has_speed=True,  has_flags=False)
SpathTab   = _make_path_tab(SpathParser,  has_speed=False, has_flags=False)


# ─────────────────────────────────────────────────────────────────────────────
# Main workshop
# ─────────────────────────────────────────────────────────────────────────────

class PathWorkshop(QWidget):  #vers 2
    App_name   = "Path Workshop"
    App_build  = "Build 1"
    App_auth   = "X-Seti"
    config_key = "path_workshop"

    _FILE_ROUTES = {
        "tracks":  0, "tracks2": 0,
        "train":   0, "train2":  0,
        "flight":  1, "flight2": 1, "flight3": 1, "flight4": 1,
        "spath0":  2,
    }

    def __init__(self, main_window=None, parent=None):
        super().__init__(parent)
        self.main_window   = main_window
        self._radar_image: Optional[QImage] = None
        self._setup_ui()

    def _setup_ui(self):  #vers 1
        root = QVBoxLayout(self)
        root.setContentsMargins(4, 4, 4, 4); root.setSpacing(4)

        # Toolbar
        tb = QHBoxLayout()
        open_btn = QPushButton("Open…");   open_btn.setFixedHeight(26)
        save_btn = QPushButton("Save");    save_btn.setFixedHeight(26)
        radar_btn = QPushButton("Radar…"); radar_btn.setFixedHeight(26)
        open_btn.clicked.connect(self._open_file)
        save_btn.clicked.connect(self._save_file)
        radar_btn.clicked.connect(self._load_radar_background)
        self._status_lbl = QLabel("Open a path file to begin")
        self._status_lbl.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        for w in (open_btn, save_btn, radar_btn, self._status_lbl):
            tb.addWidget(w)
        root.addLayout(tb)

        # Tabs
        self._tabs = QTabWidget()
        self._tab_train  = TrainTab()
        self._tab_flight = FlightTab()
        self._tab_spath  = SpathTab()
        self._tabs.addTab(self._tab_train,  "Train / Tracks")
        self._tabs.addTab(self._tab_flight, "Flight Paths")
        self._tabs.addTab(self._tab_spath,  "Static Paths")
        root.addWidget(self._tabs, 1)

    def _set_status(self, msg: str):  #vers 1
        self._status_lbl.setText(msg)

    def _open_file(self, path: str = None):  #vers 1
        if path is None:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open Path File", "",
                "Path files (train.dat train2.dat flight*.dat spath0.dat *.dat);;All files (*)")
        if not path:
            return
        stem = os.path.splitext(os.path.basename(path))[0].lower()
        tab_idx = self._FILE_ROUTES.get(stem, 0)
        tabs = [self._tab_train, self._tab_flight, self._tab_spath]
        if tab_idx >= len(tabs): tab_idx = 0
        tab  = tabs[tab_idx]
        if tab.load_file(path):
            if self._radar_image and hasattr(tab, 'set_radar'):
                tab.set_radar(self._radar_image)
            self._tabs.setCurrentIndex(tab_idx)
            self._set_status(f"Loaded: {os.path.basename(path)}")
        else:
            QMessageBox.critical(self, "Error", f"Failed to load {os.path.basename(path)}")

    def _save_file(self):  #vers 1
        idx  = self._tabs.currentIndex()
        tabs = [self._tab_train, self._tab_flight, self._tab_spath]
        tab  = tabs[idx]
        path = tab.current_path
        if not path:
            path, _ = QFileDialog.getSaveFileName(self, "Save As", "", "DAT files (*.dat)")
        if path and tab.save_file(path):
            self._set_status(f"Saved {os.path.basename(path)}")

    def _save_as(self):  #vers 1
        idx  = self._tabs.currentIndex()
        tabs = [self._tab_train, self._tab_flight, self._tab_spath]
        tab  = tabs[idx]
        path, _ = QFileDialog.getSaveFileName(self, "Save As", "", "DAT files (*.dat)")
        if path and tab.save_file(path):
            self._set_status(f"Saved {os.path.basename(path)}")

    def _load_radar_background(self, img_path: str = None):  #vers 1
        """Load a radar background image (PNG/TGA composite or from IMG/TXD)."""
        if img_path is None:
            img_path, _ = QFileDialog.getOpenFileName(
                self, "Load Radar Background", "",
                "Images (*.png *.jpg *.bmp *.tga);;All files (*)")
        if not img_path:
            return
        img = QImage(img_path)
        if img.isNull():
            QMessageBox.warning(self, "Radar", f"Could not load {img_path}")
            return
        self._radar_image = img
        for tab in [self._tab_train, self._tab_flight, self._tab_spath]:
            tab.set_radar(img)
        self._set_status(f"Radar background: {os.path.basename(img_path)}")

    def _build_menus_into_qmenu(self, pm):  #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open…",            self._open_file)
        fm.addAction("Save",             self._save_file)
        fm.addAction("Save As…",         self._save_as)
        fm.addSeparator()
        fm.addAction("Load Radar Background…", self._load_radar_background)
        fm.addSeparator()
        fm.addAction("Open train.dat",   lambda: self._open_specific("train"))
        fm.addAction("Open train2.dat",  lambda: self._open_specific("train2"))
        fm.addAction("Open flight.dat",  lambda: self._open_specific("flight"))
        fm.addAction("Open flight2.dat", lambda: self._open_specific("flight2"))
        fm.addAction("Open flight3.dat", lambda: self._open_specific("flight3"))
        fm.addAction("Open spath0.dat",  lambda: self._open_specific("spath0"))
        fm.addSeparator()
        fm.addAction("Close", self.close)

    def _open_specific(self, stem: str):  #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, f"Open {stem}.dat", "", f"DAT files ({stem}.dat *.dat)")
        if path:
            self._open_file(path)


def open_path_workshop(main_window=None, path: str = None):  #vers 2
    app = QApplication.instance() or QApplication(sys.argv)
    w = PathWorkshop(main_window=main_window)
    w.setWindowTitle(f"Path Workshop — {PathWorkshop.App_build}")
    w.resize(1200, 750)
    w.show()
    if path: w._open_file(path)
    return w


if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = PathWorkshop()
    w.resize(1200, 750)
    w.show()
    sys.exit(app.exec())
