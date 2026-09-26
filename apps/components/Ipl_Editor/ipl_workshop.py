#!/usr/bin/env python3
#this belongs in apps/components/Ipl_Editor/ipl_workshop.py - Version: 5
# X-Seti - Apr 2026 - IMG Factory 1.6 - IPL Workshop
# Item Placement List editor for GTA III / VC / SA / SOL
# Built on GUIWorkshop base (temp_workshop pattern)
# Section 1: IPL parser / writer
# Section 2: IPL Workshop UI (GUIWorkshop subclass)
# Section 3: IPL logic - open/save/edit/search/filter

import sys, os, re
from pathlib import Path
from typing import List, Optional

from PyQt6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QFrame,
    QLabel, QToolButton, QPushButton, QListWidget, QListWidgetItem,
    QFileDialog, QMessageBox, QTabWidget, QScrollArea, QSizePolicy,
    QDialog, QFormLayout, QDialogButtonBox, QDoubleSpinBox, QMenu,
    QSplitter, QTableWidget, QTableWidgetItem, QHeaderView,
    QLineEdit, QComboBox, QCheckBox, QAbstractItemView
)
from PyQt6.QtGui import (
    QColor, QPainter, QFont, QIcon, QKeySequence, QShortcut
)
from PyQt6.QtCore import Qt, QSize, pyqtSignal, QSortFilterProxyModel

import sys, os
from pathlib import Path
_root = Path(__file__).resolve().parents[3]  # apps/components/Ipl_Editor -> project root
if str(_root) not in sys.path: sys.path.insert(0, str(_root))

#    GUIWorkshop base                                                           
from apps.components.Ipl_Editor.depends.diffcode import GUIWorkshop

try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
except ImportError:
    class SVGIconFactory:
        @staticmethod
        def _s(sz=20, c=None): return QIcon()
        open_icon = save_icon = export_icon = import_icon = undo_icon = \
        search_icon = locate_icon = edit_icon = remove_icon = \
        add_icon = info_icon = staticmethod(_s)

App_name = "IPL Workshop"
Build    = "Build 1"


# =============================================================================
# SECTION 1 - IPL file model lives in apps/methods/ipl_file.py (byte-exact
# round trip: untouched lines are written back as they were)
# =============================================================================
from apps.methods.ipl_file import IPLEntry, IPLSection, IPLFile
from apps.methods.ribbon_system import RibbonMixin
from apps.components.Ide_Editor.ide_editor import IDEPanel


# =============================================================================
# SECTION 2 — IPL Workshop (GUIWorkshop subclass)
# =============================================================================

class IPLMapView(QFrame):  # vers 1

    def _get_ui_color(self, key): #vers 2
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)
    """Interactive 2D top-down world map showing IPL instances as cubes.
    Supports pan (middle/left-drag), zoom (wheel), multi-select, and
    bulk translate/rotate operations."""

    selection_changed = pyqtSignal(list)   # emits list of selected IPLEntry

    # GTA world extents (SA/SOL coordinate space)
    WORLD_MIN_X, WORLD_MAX_X = -3000.0,  3000.0
    WORLD_MIN_Y, WORLD_MAX_Y = -3000.0,  3000.0

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setFrameStyle(QFrame.Shape.StyledPanel)
        self.setMinimumSize(400, 300)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setCursor(Qt.CursorShape.CrossCursor)

        self._entries: list   = []      # list of IPLEntry
        self._selected: set   = set()   # indices of selected entries
        self._ipl_colors: dict= {}      # source_ipl → QColor
        self._show_paths      = True
        self._path_nodes: list = []   # list of (x, y, z, type) tuples
        self._show_all_ipls   = True
        self._active_ipls: set= set()   # filter: only show these source_ipls
        self._radar_image       = None   # QImage composite from RadarWorkshop
        self._radar_world_bounds= (-3000.0, 3000.0, -3000.0, 3000.0)  # (xmin,xmax,ymin,ymax)
        self._show_radar        = True
        self._water_quads: list = []   # SA water quads: [{'corners':[{x,y}...],'flag':int}]
        self._water_rects: list = []   # GTA3/VC water.dat rects: [(x1,y1,x2,y2,level)]
        self._show_water        = True

        # View state
        self._zoom   = 1.0
        self._pan_x  = 0.0
        self._pan_y  = 0.0
        self._drag_start = None
        self._drag_pan   = False
        self._sel_rect   = None   # rubber-band selection (screen coords)

        self._dirty = True

    #    Data loading                                                       
    def load_entries(self, entries: list, ipl_filter: set = None):
        """Load IPL entries. ipl_filter: set of source_ipl basenames to show."""
        self._entries = entries or []
        self._selected.clear()
        self._active_ipls = ipl_filter or set()

        # Auto-colour each source IPL
        import hashlib
        self._ipl_colors.clear()
        for e in self._entries:
            key = e.source_line or ''
            if key not in self._ipl_colors:
                h = int(hashlib.md5(key.encode()).hexdigest()[:6], 16)
                r = (h >> 16) & 0xFF
                g = (h >> 8)  & 0xFF
                b =  h        & 0xFF
                # Ensure visible on dark bg — lift brightness
                mn = 80
                r = max(r, mn); g = max(g, mn); b = max(b, mn)
                self._ipl_colors[key] = QColor(r, g, b, 200)

        # Also extract path nodes from path sections if available
        self._path_nodes.clear()
        self._fit_all()
        self.update()

    def load_path_nodes(self, ipl_sections: list): #vers 1
        """Load path nodes from IPL path sections for overlay on the map.
        ipl_sections: list of IPLSection objects with name == 'path'."""
        self._path_nodes.clear()
        for sec in ipl_sections:
            if not hasattr(sec, 'name') or sec.name != 'path':
                continue
            for line in getattr(sec, 'lines', []):
                line = line.strip()
                if not line or line.startswith('#'):
                    continue
                parts = [p.strip() for p in line.split(',')]
                if len(parts) >= 4:
                    try:
                        # path format: type, posX, posY, posZ[, ...]
                        node_type = parts[0].strip()
                        px = float(parts[1])
                        py = float(parts[2])
                        pz = float(parts[3])
                        self._path_nodes.append((px, py, pz, node_type))
                    except (ValueError, IndexError):
                        pass
        self.update()

    def load_radar_image(self, radar_image, world_bounds): #vers 1
        """Set the radar composite image for background overlay.
        radar_image: QImage from RadarWorkshop.get_composite_image()
        world_bounds: (xmin, xmax, ymin, ymax) from get_world_bounds()"""
        self._radar_image        = radar_image
        self._radar_world_bounds = world_bounds
        self.update()

    def load_water(self, quads: list, rects: list): #vers 1
        """Load water geometry for overlay.
        quads: SA water quads from SaWaterParser.quads
        rects: GTA3/VC rects from WaterDatParser.rects"""
        self._water_quads = quads or []
        self._water_rects = rects or []
        self.update()

    def set_ipl_filter(self, active_ipls: set):
        self._active_ipls = active_ipls
        self.update()

    #    Coordinate transform                                               
    def _world_to_screen(self, wx, wy):
        """Convert GTA world XY → screen pixel."""
        W, H = self.width(), self.height()
        cx = W / 2 + self._pan_x
        cy = H / 2 + self._pan_y
        scale = min(W, H) / (self.WORLD_MAX_X - self.WORLD_MIN_X) * self._zoom
        sx =  cx + wx * scale
        sy =  cy - wy * scale   # Y flipped (screen Y down, world Y up)
        return sx, sy

    def _screen_to_world(self, sx, sy):
        W, H = self.width(), self.height()
        cx = W / 2 + self._pan_x
        cy = H / 2 + self._pan_y
        scale = min(W, H) / (self.WORLD_MAX_X - self.WORLD_MIN_X) * self._zoom
        wx = (sx - cx) / scale
        wy = (cy - sy) / scale
        return wx, wy

    def _fit_all(self):
        """Fit all visible entries in view."""
        visible = self._visible_entries()
        if not visible:
            self._zoom = 1.0; self._pan_x = self._pan_y = 0.0; return
        xs = [e.px for e in visible]
        ys = [e.py for e in visible]
        cx = (min(xs) + max(xs)) / 2
        cy = (min(ys) + max(ys)) / 2
        rng = max(max(xs)-min(xs), max(ys)-min(ys), 100)
        W, H = max(self.width(),400), max(self.height(),300)
        scale = min(W, H) / (self.WORLD_MAX_X - self.WORLD_MIN_X)
        self._zoom = min(W, H) / (rng + 200) / scale
        # Pan so world (cx,cy) lands at screen centre
        self._pan_x = -cx * (min(W,H) / (self.WORLD_MAX_X - self.WORLD_MIN_X) * self._zoom)
        self._pan_y =  cy * (min(W,H) / (self.WORLD_MAX_X - self.WORLD_MIN_X) * self._zoom)

    def _visible_entries(self):
        if self._active_ipls:
            return [e for e in self._entries
                    if (e.source_line or '') in self._active_ipls]
        return self._entries

    #    Paint                                                              
    def paintEvent(self, event):
        from PyQt6.QtGui import QPainter, QPen, QBrush, QColor, QFont
        from PyQt6.QtCore import QRectF
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        W, H = self.width(), self.height()

        # Background
        p.fillRect(self.rect(), self._get_ui_color('viewport_bg'))

        # Radar map background image
        if self._show_radar and self._radar_image and not self._radar_image.isNull():
            xmin, xmax, ymin, ymax = self._radar_world_bounds
            # World corners → screen corners
            sx0, sy0 = self._world_to_screen(xmin, ymax)   # top-left (world Y max = screen top)
            sx1, sy1 = self._world_to_screen(xmax, ymin)   # bottom-right
            from PyQt6.QtCore import QRectF as _QRF2
            p.setOpacity(0.55)
            p.drawImage(_QRF2(sx0, sy0, sx1-sx0, sy1-sy0), self._radar_image)
            p.setOpacity(1.0)

        # Grid lines
        self._draw_grid(p, W, H)

        # Entries as cubes
        visible = self._visible_entries()
        cube_sz = max(2.0, 6.0 * self._zoom)
        half    = cube_sz / 2

        from PyQt6.QtCore import QRectF
        for i, e in enumerate(self._entries):
            if self._active_ipls and (e.source_line or '') not in self._active_ipls:
                continue
            sx, sy = self._world_to_screen(e.px, e.py)
            col = self._ipl_colors.get(e.source_line or '', QColor(140, 160, 200, 180))
            is_sel = i in self._selected

            if is_sel:
                p.setPen(QPen(QColor(255, 220, 50), 1.5))
                p.setBrush(QBrush(QColor(255, 220, 50, 200)))
            else:
                p.setPen(QPen(col.darker(140), 0.5))
                p.setBrush(QBrush(col))

            p.drawRect(QRectF(sx - half, sy - half, cube_sz, cube_sz))

        # Rubber-band selection rect
        if self._sel_rect:
            p.setPen(QPen(QColor(100, 200, 255), 1, Qt.PenStyle.DashLine))
            p.setBrush(QBrush(QColor(100, 200, 255, 30)))
            p.drawRect(self._sel_rect)

        # Path nodes overlay
        if self._show_paths and self._path_nodes:
            from PyQt6.QtCore import QRectF as _QRF
            node_sz = max(3.0, 4.0 * self._zoom)
            p.setPen(QPen(QColor(255, 200, 50, 180), 1.0))
            p.setBrush(QBrush(QColor(255, 200, 50, 100)))
            for nx, ny, nz, ntype in self._path_nodes:
                sx2, sy2 = self._world_to_screen(nx, ny)
                p.drawEllipse(_QRF(sx2-node_sz/2, sy2-node_sz/2, node_sz, node_sz))

        # Water overlay
        if self._show_water and (self._water_quads or self._water_rects):
            from PyQt6.QtCore import QRectF as _WQRF
            from PyQt6.QtGui import QPainterPath as _QPP
            p.setPen(QPen(QColor(30, 140, 255, 200), 1.0))
            p.setBrush(QBrush(QColor(20, 100, 220, 40)))
            # SA quads (4-corner polygons)
            for q in self._water_quads:
                corners = q.get("corners", [])
                if len(corners) < 3:
                    continue
                pp = _QPP()
                sx0, sy0 = self._world_to_screen(corners[0]["x"], corners[0]["y"])
                pp.moveTo(sx0, sy0)
                for c in corners[1:]:
                    sx, sy = self._world_to_screen(c["x"], c["y"])
                    pp.lineTo(sx, sy)
                pp.closeSubpath()
                p.drawPath(pp)
            # GTA3/VC rects (x1,y1,x2,y2,level)
            p.setPen(QPen(QColor(80, 160, 255, 180), 0.8))
            p.setBrush(QBrush(QColor(20, 100, 220, 30)))
            for r in self._water_rects:
                if len(r) < 4:
                    continue
                sx0, sy0 = self._world_to_screen(r[0], r[3])  # x1,y2 (top-left in screen)
                sx1, sy1 = self._world_to_screen(r[2], r[1])  # x2,y1 (bottom-right)
                p.drawRect(_WQRF(sx0, sy0, sx1-sx0, sy1-sy0))

        # HUD
        p.setPen(self._get_ui_color('border'))
        p.setFont(QFont('Arial', 9))
        n_sel   = len(self._selected)
        n_vis   = len(visible)
        n_paths = len(self._path_nodes)
        n_water = len(self._water_quads) + len(self._water_rects)
        p.drawText(6, 16,
            f"Zoom: {self._zoom:.2f}×   Objects: {n_vis:,}"
            + (f"   Paths: {n_paths}" if n_paths else "")
            + (f"   Water: {n_water}" if n_water else "")
            + (f"   Selected: {n_sel}" if n_sel else ""))

        p.end()

    def _draw_grid(self, p, W, H):
        from PyQt6.QtGui import QPen, QColor
        from PyQt6.QtCore import QLineF
        # Draw world-space grid at nice intervals
        for interval in [100, 500, 1000, 2000]:
            sx0, _ = self._world_to_screen(0, 0)
            sx1, _ = self._world_to_screen(interval, 0)
            px_per_unit = abs(sx1 - sx0)
            if px_per_unit > 40:
                break
        col = QColor(45, 50, 65)
        col_axis = QColor(70, 80, 110)
        p.setPen(QPen(col, 0.5))
        x = -((self.WORLD_MAX_X) // interval) * interval
        while x <= self.WORLD_MAX_X:
            sx, _ = self._world_to_screen(x, 0)
            pen = QPen(col_axis if x == 0 else col, 0.5)
            p.setPen(pen)
            p.drawLine(int(sx), 0, int(sx), H)
            x += interval
        y = -((self.WORLD_MAX_Y) // interval) * interval
        while y <= self.WORLD_MAX_Y:
            _, sy = self._world_to_screen(0, y)
            pen = QPen(col_axis if y == 0 else col, 0.5)
            p.setPen(pen)
            p.drawLine(0, int(sy), W, int(sy))
            y += interval

    #    Mouse interaction                                                  
    def wheelEvent(self, event):
        factor = 1.15 if event.angleDelta().y() > 0 else 1/1.15
        self._zoom = max(0.05, min(200.0, self._zoom * factor))
        self.update()

    def mousePressEvent(self, event):
        if event.button() == Qt.MouseButton.MiddleButton:
            self._drag_start = event.position()
            self._drag_pan   = True
        elif event.button() == Qt.MouseButton.LeftButton:
            self._drag_start = event.position()
            self._drag_pan   = False
            self._sel_rect   = None

    def mouseMoveEvent(self, event):
        if self._drag_start is None:
            return
        dx = event.position().x() - self._drag_start.x()
        dy = event.position().y() - self._drag_start.y()
        if self._drag_pan or (event.buttons() & Qt.MouseButton.MiddleButton):
            self._pan_x += dx; self._pan_y += dy
            self._drag_start = event.position()
            self.update()
        elif event.buttons() & Qt.MouseButton.LeftButton:
            from PyQt6.QtCore import QRectF
            x0 = min(self._drag_start.x(), event.position().x())
            y0 = min(self._drag_start.y(), event.position().y())
            x1 = max(self._drag_start.x(), event.position().x())
            y1 = max(self._drag_start.y(), event.position().y())
            self._sel_rect = QRectF(x0, y0, x1-x0, y1-y0)
            self.update()

    def mouseReleaseEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton and self._drag_start is not None:
            if self._sel_rect and (self._sel_rect.width() > 4 or
                                    self._sel_rect.height() > 4):
                # Box select
                mods = event.modifiers()
                if not (mods & Qt.KeyboardModifier.ShiftModifier):
                    self._selected.clear()
                r = self._sel_rect
                for i, e in enumerate(self._entries):
                    if self._active_ipls and (e.source_line or '') not in self._active_ipls:
                        continue
                    sx, sy = self._world_to_screen(e.px, e.py)
                    if r.contains(sx, sy):
                        self._selected.add(i)
            else:
                # Point click — find nearest entry
                px = event.position().x(); py = event.position().y()
                best_i = None; best_d = 12.0
                for i, e in enumerate(self._entries):
                    if self._active_ipls and (e.source_line or '') not in self._active_ipls:
                        continue
                    sx, sy = self._world_to_screen(e.px, e.py)
                    d = ((sx-px)**2 + (sy-py)**2)**0.5
                    if d < best_d:
                        best_d = d; best_i = i
                mods = event.modifiers()
                if best_i is not None:
                    if mods & Qt.KeyboardModifier.ShiftModifier:
                        if best_i in self._selected:
                            self._selected.discard(best_i)
                        else:
                            self._selected.add(best_i)
                    else:
                        self._selected = {best_i}
                elif not (mods & Qt.KeyboardModifier.ShiftModifier):
                    self._selected.clear()

            self._sel_rect = None
            self.update()
            self.selection_changed.emit(
                [self._entries[i] for i in sorted(self._selected)])
        self._drag_start = None
        self._drag_pan   = False

    def keyPressEvent(self, event):
        if event.key() == Qt.Key.Key_A and \
                event.modifiers() & Qt.KeyboardModifier.ControlModifier:
            self._selected = set(range(len(self._entries)))
            self.selection_changed.emit(list(self._entries))
            self.update()
        elif event.key() == Qt.Key.Key_Escape:
            self._selected.clear()
            self.selection_changed.emit([])
            self.update()
        elif event.key() == Qt.Key.Key_F:
            self._fit_all(); self.update()

    #    Selection helpers                                                  
    def select_by_ipl(self, ipl_name: str, add=False):
        if not add:
            self._selected.clear()
        for i, e in enumerate(self._entries):
            if (e.source_line or '') == ipl_name:
                self._selected.add(i)
        self.update()
        self.selection_changed.emit(
            [self._entries[i] for i in sorted(self._selected)])

    def selected_entries(self):
        return [self._entries[i] for i in sorted(self._selected)]

    #    Bulk operations                                                    
    def translate_selected(self, dx: float, dy: float, dz: float):
        """Move selected entries by (dx, dy, dz)."""
        if not self._selected:
            return
        for i in self._selected:
            e = self._entries[i]
            e.px += dx
            e.py += dy
            e.pz += dz
        self.update()

    def rotate_selected_yaw(self, degrees: float):
        """Rotate selected entries around Z axis (yaw) about their centroid."""
        import math
        if not self._selected:
            return
        sel = [self._entries[i] for i in self._selected]
        cx = sum(e.px for e in sel) / len(sel)
        cy = sum(e.py for e in sel) / len(sel)
        rad = math.radians(degrees)
        cos_a, sin_a = math.cos(rad), math.sin(rad)
        for i in self._selected:
            e = self._entries[i]
            rx = e.px - cx; ry = e.py - cy
            e.px = cx + rx * cos_a - ry * sin_a
            e.py = cy + rx * sin_a + ry * cos_a
            # Update quaternion — rotate around Z
            # Current quat: (rx, ry, rz, rw)
            half = rad / 2
            dqz, dqw = math.sin(half), math.cos(half)
            qx, qy, qz, qw = e.rx, e.ry, e.rz, e.rw
            e.rx = qw*0   + qx*dqw + qy*dqz - qz*0
            e.ry = qw*0   - qx*dqz + qy*dqw + qz*0
            e.rz = qw*dqz + qx*0   - qy*0   + qz*dqw
            e.rw = qw*dqw - qx*0   - qy*0   - qz*dqz
        self.update()



class IPLMapPanel(QFrame):  # vers 1
    """Full map panel with map view + translate/rotate controls."""

    def __init__(self, workshop, parent=None):
        super().__init__(parent)
        self._ws = workshop
        self._build_ui()

    def _build_ui(self):
        from PyQt6.QtWidgets import (QVBoxLayout, QHBoxLayout, QGroupBox,
            QDoubleSpinBox, QPushButton, QLabel, QCheckBox, QComboBox)
        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(2)

        #    Toolbar                                                        
        from PyQt6.QtWidgets import QToolBar as _QTB
        bar = _QTB()
        bar.setMovable(False)
        bar.setContextMenuPolicy(Qt.ContextMenuPolicy.PreventContextMenu)

        fit_btn = QPushButton("Fit [F]")
        fit_btn.setMinimumHeight(28)
        fit_btn.setToolTip("Fit all objects in view (F)")
        fit_btn.clicked.connect(self._fit)
        bar.addWidget(fit_btn)

        sel_ipl_btn = QPushButton("Select by IPL")
        sel_ipl_btn.setMinimumHeight(28)
        sel_ipl_btn.setToolTip("Select all objects from a specific IPL file")
        sel_ipl_btn.clicked.connect(self._select_by_ipl_dialog)
        bar.addWidget(sel_ipl_btn)

        sel_all_btn = QPushButton("Select All [Ctrl+A]")
        sel_all_btn.setMinimumHeight(28)
        sel_all_btn.clicked.connect(self._select_all)
        bar.addWidget(sel_all_btn)

        clr_btn = QPushButton("Clear [Esc]")
        clr_btn.setMinimumHeight(28)
        clr_btn.clicked.connect(self._clear_sel)
        bar.addWidget(clr_btn)

        path_btn = QPushButton("Paths")
        path_btn.setCheckable(True)
        path_btn.setChecked(True)
        path_btn.setMinimumHeight(28)
        path_btn.setToolTip("Toggle path node overlay (yellow circles)")
        path_btn.toggled.connect(lambda v: setattr(self._map,'_show_paths',v) or self._map.update())
        bar.addWidget(path_btn)

        radar_btn = QPushButton("Radar")
        radar_btn.setCheckable(True)
        radar_btn.setChecked(True)
        radar_btn.setMinimumHeight(28)
        radar_btn.setToolTip("Toggle radar map background (from open Radar Workshop tab)")
        radar_btn.toggled.connect(lambda v: setattr(self._map,'_show_radar',v) or self._map.update())
        bar.addWidget(radar_btn)

        load_radar_btn = QPushButton("Load Radar…")
        load_radar_btn.setMinimumHeight(28)
        load_radar_btn.setToolTip(
            "Load radar from Radar Workshop tab or browse for BMP/PNG")
        load_radar_btn.clicked.connect(self._load_radar)
        bar.addWidget(load_radar_btn)

        water_btn = QPushButton("Water")
        water_btn.setCheckable(True)
        water_btn.setChecked(True)
        water_btn.setMinimumHeight(28)
        water_btn.setToolTip("Toggle water geometry overlay (blue polygons)")
        water_btn.toggled.connect(lambda v: setattr(self._map,'_show_water',v) or self._map.update())
        bar.addWidget(water_btn)

        load_water_btn = QPushButton("Load Water…")
        load_water_btn.setMinimumHeight(28)
        load_water_btn.setToolTip("Load water geometry from open Water Workshop tab or browse")
        load_water_btn.clicked.connect(self._load_water)
        bar.addWidget(load_water_btn)

        bar.addSeparator()
        self._sel_lbl = QLabel("No selection")
        self._sel_lbl.setStyleSheet("color: palette(mid);")
        bar.addWidget(self._sel_lbl)
        root.addWidget(bar)

        #    Map view                                                       
        self._map = IPLMapView()
        self._map.selection_changed.connect(self._on_selection_changed)
        root.addWidget(self._map, stretch=1)

        #    Translate / Rotate controls                                    
        ctrl = QHBoxLayout(); ctrl.setContentsMargins(4, 2, 4, 4); ctrl.setSpacing(8)

        # Translate group
        tg = QGroupBox("Translate selection")
        tl = QHBoxLayout(tg); tl.setSpacing(4)
        self._tx = self._spin(-9999, 9999, 0, "X offset")
        self._ty = self._spin(-9999, 9999, 0, "Y offset")
        self._tz = self._spin(-9999, 9999, 0, "Z offset")
        for lbl, sp in [("X:", self._tx), ("Y:", self._ty), ("Z:", self._tz)]:
            tl.addWidget(QLabel(lbl)); tl.addWidget(sp)
        apply_t = QPushButton("Apply")
        apply_t.setMinimumHeight(28)
        apply_t.setToolTip("Move selected objects by X/Y/Z offset")
        apply_t.clicked.connect(self._apply_translate)
        tl.addWidget(apply_t)
        ctrl.addWidget(tg)

        # Rotate group
        rg = QGroupBox("Rotate selection (yaw around Z)")
        rl = QHBoxLayout(rg); rl.setSpacing(4)
        self._rdeg = self._spin(-360, 360, 90, "Degrees to rotate")
        rl.addWidget(QLabel("°:"))
        rl.addWidget(self._rdeg)
        for deg_lbl, deg_val in [("-90°", -90), ("+90°", 90), ("180°", 180)]:
            b = QPushButton(deg_lbl); b.setMinimumHeight(28); b.setMinimumWidth(58)
            b.clicked.connect(lambda _=False, d=deg_val: self._rotate(d))
            rl.addWidget(b)
        apply_r = QPushButton("Apply")
        apply_r.setMinimumHeight(28)
        apply_r.clicked.connect(lambda: self._rotate(self._rdeg.value()))
        rl.addWidget(apply_r)
        ctrl.addWidget(rg)

        root.addLayout(ctrl)

    def _spin(self, lo, hi, val, tip):
        from PyQt6.QtWidgets import QDoubleSpinBox
        s = QDoubleSpinBox(); s.setRange(lo, hi); s.setValue(val)
        s.setDecimals(2); s.setMinimumWidth(84); s.setMinimumHeight(28)
        s.setToolTip(tip)
        return s

    def refresh(self, entries, ipl_filter=None):
        self._map.load_entries(entries, ipl_filter)

    def _fit(self):
        self._map._fit_all(); self._map.update()

    def _select_all(self):
        self._map._selected = set(range(len(self._map._entries)))
        self._map.update()
        self._on_selection_changed(self._map._entries)

    def _clear_sel(self):
        self._map._selected.clear()
        self._map.update()
        self._on_selection_changed([])

    def _on_selection_changed(self, entries):
        n = len(entries)
        self._sel_lbl.setText(
            f"{n:,} object(s) selected" if n else "No selection")

    def _select_by_ipl_dialog(self):
        from PyQt6.QtWidgets import QInputDialog
        ipls = sorted({(e.source_line or '') for e in self._map._entries} - {''})
        if not ipls:
            return
        name, ok = QInputDialog.getItem(
            self, "Select by IPL", "IPL file:", ipls, 0, False)
        if ok and name:
            mods = self._map.focusWidget()
            self._map.select_by_ipl(name)

    def _load_water(self): #vers 1
        """Load water geometry from open Water Workshop tab."""
        mw = getattr(self._ws, 'main_window', None)
        water_ws = None
        if mw and hasattr(mw, 'main_tab_widget'):
            tw = mw.main_tab_widget
            try:
                from apps.components.Water_Editor.water_workshop import WaterWorkshop
                for i in range(tw.count()):
                    w = tw.widget(i)
                    if isinstance(w, WaterWorkshop):
                        water_ws = w; break
                    for child in (w.findChildren(WaterWorkshop) if w else []):
                        water_ws = child; break
                    if water_ws:
                        break
            except ImportError:
                pass

        if water_ws:
            quads = water_ws.get_water_quads()
            rects = water_ws.get_water_rects()
            if quads or rects:
                self._map.load_water(quads, rects)
                n = len(quads) + len(rects)
                if mw and hasattr(mw,'log_message'):
                    mw.log_message(
                        f"IPL Map: water overlay loaded ({len(quads)} quads, {len(rects)} rects)")
                return
            else:
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.information(
                    self,"No Water Data",
                    "Water Workshop is open but no water file is loaded.")
                return

        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(
            self,"No Water Workshop",
            "Open Water Workshop and load a water.dat or waterpro.dat first,\n"
            "then click Load Water to overlay it on the map.")

    def _get_asset_db(self): #vers 1
        """Return asset_db from main_window if available."""
        mw = getattr(self,'main_window',None)
        return getattr(mw,'asset_db',None) if mw else None

    def _load_radar(self): #vers 1
        """Load radar composite from open Radar Workshop tab, or browse for image."""
        # First try to find an open RadarWorkshop tab
        mw = getattr(self._ws, 'main_window', None)
        radar_ws = None
        if mw and hasattr(mw, 'main_tab_widget'):
            tw = mw.main_tab_widget
            for i in range(tw.count()):
                w = tw.widget(i)
                # RadarWorkshop may be inside a container
                from apps.components.Radar_Editor.radar_workshop import RadarWorkshop
                if isinstance(w, RadarWorkshop):
                    radar_ws = w; break
                # Check children
                for child in w.findChildren(RadarWorkshop) if w else []:
                    radar_ws = child; break
                if radar_ws:
                    break

        if radar_ws and radar_ws._tile_rgba:
            img    = radar_ws.get_composite_image(max_size=4096)
            bounds = radar_ws.get_world_bounds()
            if img and not img.isNull():
                self._map.load_radar_image(img, bounds)
                n = len(radar_ws._tile_rgba)
                if mw and hasattr(mw,'log_message'):
                    mw.log_message(
                        f"IPL Map: radar background loaded ({n} tiles, "
                        f"bounds {bounds[0]:.0f}..{bounds[1]:.0f})")
                return

        # No radar workshop open — browse for a BMP/PNG file
        from PyQt6.QtWidgets import QFileDialog, QInputDialog
        path, _ = QFileDialog.getOpenFileName(
            self, "Load Radar Map Image", "",
            "Images (*.bmp *.png *.jpg *.jpeg);;All Files (*)")
        if not path:
            return

        from PyQt6.QtGui import QImage
        img = QImage(path)
        if img.isNull():
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self,"Load Failed","Could not load image:\n"+path)
            return

        # Ask for world bounds
        game_presets = [
            ("GTA III / VC  (-2000 to 2000)", (-2000.0, 2000.0, -2000.0, 2000.0)),
            ("SA / SOL  (-3000 to 3000)",     (-3000.0, 3000.0, -3000.0, 3000.0)),
            ("SOL Large  (-6000 to 6000)",    (-6000.0, 6000.0, -6000.0, 6000.0)),
        ]
        choice, ok = QInputDialog.getItem(
            self, "World Bounds", "Select game world coverage:",
            [g[0] for g in game_presets], 1, False)
        if not ok:
            return
        bounds = next(g[1] for g in game_presets if g[0]==choice)
        self._map.load_radar_image(img, bounds)
        if mw and hasattr(mw,'log_message'):
            mw.log_message(f"IPL Map: radar background loaded from {path}")

    def _apply_translate(self):
        dx = self._tx.value(); dy = self._ty.value(); dz = self._tz.value()
        if dx == dy == dz == 0:
            return
        self._ws._push_undo()
        self._map.translate_selected(dx, dy, dz)
        n = len(self._map._selected)
        # Sync back to workshop table
        if hasattr(self._ws, '_table'):
            self._ws._populate_table(self._ws._current_entries())
        if self._ws.main_window and hasattr(self._ws.main_window, 'log_message'):
            self._ws.main_window.log_message(
                f"IPL: moved {n} object(s) by X={dx:+.1f} Y={dy:+.1f} Z={dz:+.1f}")

    def _rotate(self, degrees):
        self._ws._push_undo()
        self._map.rotate_selected_yaw(degrees)
        n = len(self._map._selected)
        if hasattr(self._ws, '_table'):
            self._ws._populate_table(self._ws._current_entries())
        if self._ws.main_window and hasattr(self._ws.main_window, 'log_message'):
            self._ws.main_window.log_message(
                f"IPL: rotated {n} object(s) by {degrees:+.0f}°")

class IPLWorkshop(RibbonMixin, GUIWorkshop):
    """GTA Item Placement List editor — docks in IMG Factory or runs standalone."""

    App_name        = "IPL Workshop"
    App_build       = Build
    App_author      = "X-Seti"
    App_year        = "2026"
    App_description = ("GTA III / VC / SA / SOL — .ide item definitions + .ipl placements\n"
                       "Edit object instances: position, rotation, model ID\n"
                       "Supports inst / zone / cull / cars and all other sections")
    config_key      = "ipl_workshop"
    _ribbon_name    = "ipl_workshop"
    # Bump when the set of ribbons changes (3 = Sort buttons added)
    _RIBBON_LAYOUT_VERSION = 3

    # Column indices for the instance table
    COL_ID    = 0
    COL_MODEL = 1
    COL_INT   = 2
    COL_PX    = 3
    COL_PY    = 4
    COL_PZ    = 5
    COL_RX    = 6
    COL_RY    = 7
    COL_RZ    = 8
    COL_RW    = 9
    COL_LOD   = 10
    COL_FILE  = 11
    NUM_COLS  = 12

    COL_HEADERS = ["ID", "Model", "Int", "X", "Y", "Z",
                   "Rot X", "Rot Y", "Rot Z", "Rot W", "LOD", "File"]

    def __init__(self, parent=None, main_window=None):
        self._defer_setup_ui = True
        self._ipl         = None      # first IPLFile (compat)
        self._files       = []        # every loaded IPLFile - each saves to its own path
        self._owner       = {}        # id(entry) -> IPLFile
        self._dat_path    = None
        self._dat_game    = None
        self._file_path   = ""
        self._undo_stack  = []
        self._redo_stack  = []
        self._active_section = "inst"
        self._active_file = -1
        super().__init__(parent, main_window)
        self.setup_ui()
        self.ribbon_restore_state()
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(0, lambda: self._on_centre_tab_changed(self._centre_tabs.currentIndex()))
        if main_window and hasattr(self, "toolbar"): self.toolbar.hide()
        if main_window: self.setWindowFlags(Qt.WindowType.Widget)
        try:
            from apps.methods.imgfactory_svg_icons import get_ipl_editor_icon
            self.setWindowIcon(get_ipl_editor_icon(64))
        except Exception:
            pass

    #    Menu                                                                   
    def _build_menus_into_qmenu(self, pm):
        fm = pm.addMenu("File")
        fm.addAction("Open IPL…  Ctrl+O",     self._open_file)
        fm.addAction("Load all IPLs from DAT…", self._load_all_from_dat)
        fm.addAction("Save       Ctrl+S",      self._save_file)
        fm.addAction("Save As…",               self._save_as)
        fm.addSeparator()
        fm.addAction("Export CSV…",            self._export_csv)
        fm.addSeparator()
        recent = self.WS.get_recent()
        if recent:
            rm = fm.addMenu("Recent Files")
            for rp in recent:
                act = rm.addAction(Path(rp).name)
                act.triggered.connect(lambda c=False, p=rp: self._open_file(p))
            rm.addSeparator()
            rm.addAction("Clear Recent", self._clear_recent)

        em = pm.addMenu("Edit")
        em.addAction("Undo  Ctrl+Z",           self._undo)
        em.addAction("Redo  Ctrl+Y",           self._redo)
        em.addSeparator()
        em.addAction("Add Entry",              self._add_entry)
        em.addAction("Delete Selected",        self._delete_selected)
        em.addAction("Duplicate Selected",     self._duplicate_selected)
        em.addSeparator()
        em.addAction("Select All  Ctrl+A",     self._select_all)
        em.addAction("Find…  Ctrl+F",          self._show_find)

        vm = pm.addMenu("View")
        vm.addAction("Fit Columns",            self._fit_columns)
        vm.addAction("Filter by Interior…",    self._filter_interior)
        vm.addAction("Statistics",             self._show_stats)
        vm.addSeparator()
        vm.addAction("About IPL Workshop",     self._show_about)

    #    Left panel — section list                                              
    def _create_left_panel(self):
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ll = QVBoxLayout(panel)
        ll.setContentsMargins(*self.get_panel_margins())

        hdr = QLabel("Sections")
        hdr.setFont(self.panel_font)
        hdr.setStyleSheet("font-weight:bold; padding:2px;")
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        ll.addWidget(hdr)

        self._section_list = QListWidget()
        self._section_list.setAlternatingRowColors(True)
        self._section_list.currentRowChanged.connect(self._on_section_changed)
        ll.addWidget(self._section_list)

        sep2 = QFrame(); sep2.setFrameShape(QFrame.Shape.HLine)
        ll.addWidget(sep2)

        self._info_lbl = QLabel("No file loaded")
        self._info_lbl.setFont(self.infobar_font)
        self._info_lbl.setWordWrap(True)
        self._info_lbl.setStyleSheet("padding:2px; color:palette(mid);")
        ll.addWidget(self._info_lbl)

        self._dirty_lbl = QLabel("Modified: no")
        self._dirty_lbl.setFont(self.infobar_font)
        ll.addWidget(self._dirty_lbl)
        return panel

    #    Centre panel — instance table + search                                 
    def _create_centre_panel(self): #vers 2
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel)
        cl.setContentsMargins(0, 0, 0, 0)
        cl.setSpacing(0)

        # Tabs: Table editor | World Map
        self._centre_tabs = QTabWidget()
        self._centre_tabs.setTabPosition(QTabWidget.TabPosition.North)

        #    Tab 0: IDE (an IPL needs its IDE list first)                   
        self._ide_panel = IDEPanel(self)
        self._ide_panel.cascade_handler = self._cascade_ide_changes
        self._ide_panel.usage_provider = self._usage_counts
        self._ide_panel.dirty_changed.connect(lambda _d: self._update_dirty())
        self._ide_panel.status_message.connect(self._set_status)
        self._centre_tabs.addTab(self._ide_panel, "IDE")

        #    Tab 1: Table editor                                            
        table_widget = QFrame()
        tl = QVBoxLayout(table_widget)
        tl.setContentsMargins(0, 2, 0, 0)
        tl.setSpacing(2)

        sbar = QHBoxLayout(); sbar.setSpacing(4); sbar.setContentsMargins(4,2,4,2)
        self._search_box = QLineEdit()
        self._search_box.setPlaceholderText("Search model name…")
        self._search_box.textChanged.connect(self._on_search_changed)
        self._search_box.setMinimumHeight(28)
        sbar.addWidget(QLabel("Search")); sbar.addWidget(self._search_box)

        self._int_filter = QComboBox()
        self._int_filter.addItem("All interiors")
        self._int_filter.setFixedWidth(120)
        self._int_filter.currentIndexChanged.connect(self._on_filter_changed)
        sbar.addWidget(self._int_filter)

        clr = QPushButton("Clear")
        clr.setMinimumHeight(28); clr.setToolTip("Clear search")
        clr.clicked.connect(self._clear_search)
        sbar.addWidget(clr)
        tl.addLayout(sbar)

        self._table = QTableWidget(0, self.NUM_COLS)
        self._table.setHorizontalHeaderLabels(self.COL_HEADERS)
        self._table.setAlternatingRowColors(True)
        self._table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self._table.setEditTriggers(QAbstractItemView.EditTrigger.DoubleClicked |
                                    QAbstractItemView.EditTrigger.SelectedClicked)
        self._table.setSortingEnabled(True)
        self._table.horizontalHeader().setStretchLastSection(False)
        self._table.horizontalHeader().setSectionResizeMode(
            self.COL_MODEL, QHeaderView.ResizeMode.Stretch)
        self._table.itemChanged.connect(self._on_cell_edited)
        self._table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._table.customContextMenuRequested.connect(self._show_context_menu)
        tl.addWidget(self._table)

        from PyQt6.QtWidgets import QTextEdit
        self._text_view = QTextEdit()
        self._text_view.setReadOnly(False)
        self._text_view.setFont(QFont("Courier New", 9))
        self._text_view.setPlaceholderText("Section content appears here…")
        self._text_view.setVisible(False)
        self._text_view.textChanged.connect(self._on_text_edited)
        tl.addWidget(self._text_view)

        self._centre_tabs.addTab(table_widget, "IPL Table")

        #    Tab 2: World Map                                               
        self._map_panel = IPLMapPanel(self)
        self._centre_tabs.addTab(self._map_panel, "World Map")

        # Switch to map → refresh with current entries
        self._centre_tabs.currentChanged.connect(self._on_centre_tab_changed)

        cl.addWidget(self._centre_tabs)
        return panel

    def _on_centre_tab_changed(self, idx: int): #vers 3
        """Refresh the map when its tab opens; hide the IPL section list while
        the IDE tab (which has its own file/section lists) is showing."""
        w = self._centre_tabs.widget(idx)
        if hasattr(self, "_main_splitter"):
            self._main_splitter.widget(0).setVisible(w is not self._ide_panel)
        if w is self._map_panel:
            self._map_panel.refresh(self._current_entries())
            if self._files:
                path_secs = [s for f in self._files for s in f.sections if s.name == 'path']
                self._map_panel._map.load_path_nodes(path_secs)

    def _current_entries(self) -> list: #vers 2
        """Every loaded file's inst entries (what the table and map show)."""
        return self._view_entries()

    #    Right sidebar                                                          
# =============================================================================
# SECTION 3 — IPL logic
# =============================================================================

    #    File ops                                                               
    def _open_file(self, path=None): #vers 2
        if not path:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open IPL File", "",
                "IPL Files (*.ipl *.IPL);;All Files (*)")
        if not path:
            return
        if path.lower().endswith((".ide", ".ifx")):
            self.load_ide_file(path)
            return
        try:
            ipl = IPLFile()
            ipl.load(path)
        except Exception as e:
            QMessageBox.critical(self, "Load Error", f"Failed to load {Path(path).name}:\n{e}")
            return
        self._files, self._ipl, self._file_path = [ipl], ipl, path
        self._dat_path = None
        self.WS.add_recent(path)
        self._after_load()
        self._set_status(f"Loaded {Path(path).name}  |  {len(ipl.instances)} instances"
                         f"  |  game={ipl.game}  |  sections: {', '.join(ipl.section_names())}")

    def _save_file(self): #vers 3
        """Save every changed IDE file first (their ID / name changes are applied
        to the loaded IPL entries), then every changed IPL - each back to its own
        path, each backed up first."""
        ide_saved = self._ide_panel.has_unsaved()
        if ide_saved:
            self._ide_panel.save_ide_file()
        dirty = [f for f in self._files if f.dirty]
        saved = [Path(f.path).name for f in dirty if self._write_ipl(f, f.path)]
        if dirty:
            self._populate_table()
        self._update_dirty()
        if saved or ide_saved:
            self._set_status(("IDE saved  |  " if ide_saved else "") + ("Saved " + ", ".join(saved) if saved else ""))

    def _save_as(self): #vers 2
        if len(self._files) != 1:
            QMessageBox.information(self, "Save As",
                "Several IPL files are loaded - Save writes each one back to its own file. "
                "Open a single IPL to use Save As.")
            return
        ipl = self._files[0]
        p, _ = QFileDialog.getSaveFileName(
            self, "Save IPL As", ipl.path or "", "IPL Files (*.ipl);;All Files (*)")
        if p and self._write_ipl(ipl, p):
            self._file_path = p
            self._populate_table()
            self._update_dirty()
            self._set_status(f"Saved as {Path(p).name}")

    def _write_ipl(self, ipl, path) -> bool: #vers 1
        from apps.methods.file_backup import backup_file, note_change
        try:
            if os.path.exists(path):
                note_change(f"Save IPL {Path(path).name}")
                if backup_file(path) is None:
                    QMessageBox.warning(self, "Save", "Backup failed - file not overwritten.")
                    return False
            ipl.save(path)
            return True
        except Exception as e:
            QMessageBox.critical(self, "Save Error", f"{Path(path).name}:\n{e}")
            return False

    def _export_file(self): self._export_csv()
    def _import_file(self): self._open_file()

    def _export_csv(self): #vers 2
        view = self._view_entries()
        if not view:
            QMessageBox.information(self, "Export", "Load an IPL file first.")
            return
        p, _ = QFileDialog.getSaveFileName(
            self, "Export CSV", (Path(self._file_path).stem if self._file_path else "ipl") + "_inst.csv",
            "CSV Files (*.csv)")
        if not p:
            return
        try:
            rows = ["file,id,model,interior,x,y,z,rx,ry,rz,rw,lod"]
            for e in view:
                rows.append(f"{self._file_of(e)},{e.model_id},{e.model_name},{e.interior},"
                            f"{e.px:.6f},{e.py:.6f},{e.pz:.6f},"
                            f"{e.rx:.6f},{e.ry:.6f},{e.rz:.6f},{e.rw:.6f},{e.lod}")
            Path(p).write_text("\n".join(rows), encoding="utf-8")
            self._set_status(f"Exported {len(view)} instances to {Path(p).name}")
        except Exception as e:
            QMessageBox.critical(self, "Export Error", str(e))

    def _clear_recent(self):
        self.WS._data["recent_files"] = []; self.WS.save()
        self._set_status("Recent cleared")

    #    Section management                                                     
    def _populate_section_list(self): #vers 2
        self._section_list.clear()
        if not self._files:
            return
        multi = len(self._files) > 1
        item = QListWidgetItem(f"inst  ({len(self._view_entries())})")
        item.setData(Qt.ItemDataRole.UserRole, ("inst", -1))
        item.setForeground(QColor("#4a9fd4"))
        self._section_list.addItem(item)
        for fi, f in enumerate(self._files):
            for sec in f.sections:
                if sec.is_inst():
                    continue
                label = f"{sec.name}  ({len(sec.raw)})" + (f"  [{Path(f.path).stem}]" if multi else "")
                it = QListWidgetItem(label)
                it.setData(Qt.ItemDataRole.UserRole, (sec.name, fi))
                self._section_list.addItem(it)

    def _select_inst_section(self): #vers 2
        if self._section_list.count():
            self._section_list.setCurrentRow(0)

    def _on_section_changed(self, row: int): #vers 2
        if not self._files or row < 0:
            return
        item = self._section_list.item(row)
        if not item:
            return
        name, fi = item.data(Qt.ItemDataRole.UserRole)
        self._active_section, self._active_file = name, fi
        if name == "inst":
            self._text_view.setVisible(False)
            self._table.setVisible(True)
            self._populate_table()
            self._update_int_filter(self._view_entries())
            return
        sec = next((s for s in self._files[fi].sections if s.name == name), None)
        if not sec:
            return
        self._table.setVisible(False)
        self._text_view.setVisible(True)
        self._text_view.blockSignals(True)
        self._text_view.setPlainText("\n".join(sec.raw))
        self._text_view.blockSignals(False)
        self._set_status(f"Section '{name}' - {len(sec.raw)} lines (raw text)")

    #    Table population                                                       
    def _populate_table(self, entries=None): #vers 3
        """Rebuild the table from every loaded file's inst entries. The row's
        entry index is kept in the ID cell's data, so sorting the table can
        never make an edit land on the wrong entry."""
        view = self._view_entries()
        self._owner = {id(e): f for f in self._files for e in f.instances}
        multi = len(self._files) > 1
        self._table.blockSignals(True)
        self._table.setSortingEnabled(False)
        self._table.setRowCount(len(view))
        self._table.setColumnHidden(self.COL_FILE, not multi)

        def _num(v) -> QTableWidgetItem:
            item = QTableWidgetItem(f"{v:.6f}" if isinstance(v, float) else str(v))
            item.setTextAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
            return item

        for row, e in enumerate(view):
            idc = QTableWidgetItem(str(e.model_id))
            idc.setData(Qt.ItemDataRole.UserRole, row)
            self._table.setItem(row, self.COL_ID, idc)
            self._table.setItem(row, self.COL_MODEL, QTableWidgetItem(e.model_name))
            self._table.setItem(row, self.COL_INT, _num(e.interior))
            for col, v in ((self.COL_PX, e.px), (self.COL_PY, e.py), (self.COL_PZ, e.pz),
                           (self.COL_RX, e.rx), (self.COL_RY, e.ry), (self.COL_RZ, e.rz),
                           (self.COL_RW, e.rw)):
                self._table.setItem(row, col, _num(v))
            lod_item = _num(e.lod)
            if e.lod < 0:
                lod_item.setForeground(QColor("#666"))
            self._table.setItem(row, self.COL_LOD, lod_item)
            fitem = QTableWidgetItem(self._file_of(e))
            fitem.setFlags(fitem.flags() & ~Qt.ItemFlag.ItemIsEditable)
            self._table.setItem(row, self.COL_FILE, fitem)
            self._table.setRowHeight(row, 20)
        self._table.setSortingEnabled(True)
        self._table.blockSignals(False)
        self._update_dirty()
        self._set_status(f"Section 'inst' - {len(view)} instances")

    def _update_int_filter(self, entries: List[IPLEntry]):
        interiors = sorted(set(e.interior for e in entries))
        self._int_filter.blockSignals(True)
        self._int_filter.clear()
        self._int_filter.addItem("All interiors")
        for i in interiors:
            self._int_filter.addItem(f"Interior {i}", i)
        self._int_filter.blockSignals(False)

    #    Search / filter                                                        
    def _on_search_changed(self, text: str):
        self._apply_filter(text, self._int_filter.currentData())

    def _on_filter_changed(self):
        self._apply_filter(self._search_box.text(),
                           self._int_filter.currentData())

    def _apply_filter(self, text: str, interior):
        text = text.lower()
        for row in range(self._table.rowCount()):
            model_item = self._table.item(row, self.COL_MODEL)
            int_item   = self._table.item(row, self.COL_INT)
            model_match = not text or (model_item and text in model_item.text().lower())
            int_match   = interior is None or (
                int_item and int(int_item.text() or "0") == interior)
            self._table.setRowHidden(row, not (model_match and int_match))

    def _clear_search(self):
        self._search_box.clear()
        self._int_filter.setCurrentIndex(0)
        for row in range(self._table.rowCount()):
            self._table.setRowHidden(row, False)

    #    Editing                                                                
    def _on_cell_edited(self, item: QTableWidgetItem): #vers 2
        view = self._view_entries()
        idc = self._table.item(item.row(), self.COL_ID)
        idx = idc.data(Qt.ItemDataRole.UserRole) if idc else None
        if idx is None or idx >= len(view):
            return
        e, col, val = view[idx], item.column(), item.text().strip()
        conv = {self.COL_ID: ("model_id", int), self.COL_MODEL: ("model_name", str),
                self.COL_INT: ("interior", int), self.COL_PX: ("px", float),
                self.COL_PY: ("py", float), self.COL_PZ: ("pz", float),
                self.COL_RX: ("rx", float), self.COL_RY: ("ry", float),
                self.COL_RZ: ("rz", float), self.COL_RW: ("rw", float),
                self.COL_LOD: ("lod", int)}.get(col)
        if not conv:
            return
        attr, fn = conv
        try:
            new = fn(val)
        except ValueError:
            self._table.blockSignals(True)          # bad input - show the old value again
            item.setText(str(getattr(e, attr)))
            self._table.blockSignals(False)
            return
        if new == getattr(e, attr):
            return
        self._push_undo()
        setattr(e, attr, new)
        self._update_dirty()

    def _on_text_edited(self): #vers 2
        if not self._files or getattr(self, "_active_file", -1) < 0:
            return
        sec = next((s for s in self._files[self._active_file].sections
                    if s.name == self._active_section), None)
        if sec and not sec.is_inst():
            sec.raw = self._text_view.toPlainText().split("\n")
            self._update_dirty()

    def _mark_dirty(self): #vers 2
        self._update_dirty()

    def _current_inst_section(self) -> Optional[IPLSection]: #vers 2
        return self._files[0].inst_section if self._files else None

    #    Load all from DAT                                                     
    def _load_all_from_dat(self):
        """Collect all IPL paths from a .dat file and load them as merged IPLFile."""
        dat_path = self._pick_dat_path()
        if not dat_path:
            return
        try:
            self._do_load_all_from_dat(dat_path)
        except Exception as e:
            import traceback
            QMessageBox.critical(self, "Load Error",
                f"Failed to load from DAT:\n{e}\n\n{traceback.format_exc()[-500:]}")

    def _pick_dat_path(self) -> str:
        """Return a .dat path — from DAT Browser if open, else file dialog."""
        # Try DAT Browser's loaded dat first
        mw = self.main_window
        if mw:
            db = getattr(mw, 'dat_browser', None)
            if db and hasattr(db, 'loader'):
                main_dat = getattr(db.loader, 'main_dat', None)
                if main_dat and getattr(main_dat, 'dat_path', ''):
                    p = main_dat.dat_path
                    reply = QMessageBox.question(self, "Load from DAT",
                        f"Use DAT Browser's loaded file?\n\n{p}",
                        QMessageBox.StandardButton.Yes |
                        QMessageBox.StandardButton.No |
                        QMessageBox.StandardButton.Cancel)
                    if reply == QMessageBox.StandardButton.Cancel:
                        return ""
                    if reply == QMessageBox.StandardButton.Yes:
                        return p
        # File dialog
        p, _ = QFileDialog.getOpenFileName(self, "Select DAT File", "",
            "DAT Files (*.dat *.DAT);;All Files (*)")
        return p

    def _do_load_all_from_dat(self, dat_path: str):
        """Parse DAT, collect all IPL entries, merge into one multi-section IPLFile."""
        from apps.methods.gta_dat_parser import DATParser, GTAGame

        # Detect game from DAT filename
        name = Path(dat_path).stem.lower()
        if "gta_sa" in name or "sa" in name:
            game = GTAGame.SA
        elif "vice" in name or "vc" in name:
            game = GTAGame.VC
        else:
            game = GTAGame.GTA3

        # IDE files first: the IPL entries are only meaningful against them
        try:
            from apps.methods.asset_checker import find_game_asset_files
            _img, _col, ides, _g = find_game_asset_files(dat_path)
            if ides:
                self._ide_panel._dat_path = dat_path
                self._ide_panel._load_files(list(ides), replace=True)
        except Exception as ex:
            print(f"[IPL Workshop] IDE load from dat: {ex}")

        game_root = str(Path(dat_path).parent.parent)
        dat = DATParser(game)
        dat.parse(dat_path, game_root)
        ipl_entries = dat.ipl_entries()

        if not ipl_entries:
            QMessageBox.information(self, "No IPL Files",
                f"No IPL directives found in:\n{dat_path}")
            return

        # Show selection dialog
        ipl_entries = [e for e in ipl_entries]  # all
        sel = self._select_ipls_dialog(ipl_entries, dat_path)
        if sel is None:  # cancelled
            return
        if not sel:
            self._set_status("No IPL files selected")
            return

        # Load every selected IPL as its OWN file (each saves to its own path)
        files, failed, total_inst = [], [], 0
        for entry in sel:
            if not entry.exists:
                failed.append(f"MISSING: {entry.path}")
                continue
            sub = IPLFile()
            try:
                sub.load(entry.abs_path)
            except Exception as ex:
                failed.append(f"SKIPPED {Path(entry.abs_path).name}: {ex}")
                continue
            sub.game = "sa" if game == GTAGame.SA else ("vc" if game == GTAGame.VC else sub.game)
            for e in sub.instances:
                e.source_line = entry.path
            total_inst += len(sub.instances)
            files.append(sub)
        if not files:
            QMessageBox.warning(self, "Load from DAT", "No IPL files could be loaded.\n\n" + "\n".join(failed[:20]))
            return

        self._files, self._ipl = files, files[0]
        self._file_path = files[0].path
        self._dat_path, self._dat_game = dat_path, game
        self.WS.add_recent(dat_path)
        self._after_load()
        self._set_status(f"Loaded {len(files)} IPL files  |  {total_inst:,} total instances"
                         + (f"  |  {len(failed)} skipped" if failed else ""))
        if failed:
            QMessageBox.warning(self, "Some Files Skipped",
                "These IPL files could not be loaded:\n\n" + "\n".join(failed[:20]))

    def _select_ipls_dialog(self, entries, dat_path: str):
        """Show a checklist dialog — let user pick which IPLs to load."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout,
                                      QListWidget, QDialogButtonBox,
                                      QListWidgetItem, QPushButton, QLabel)
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Select IPL Files — {Path(dat_path).name}")
        dlg.resize(500, 500)
        lo = QVBoxLayout(dlg)

        lo.addWidget(QLabel(f"DAT: {dat_path}\n"
                            f"Found {len(entries)} IPL files — select which to load:"))

        lst = QListWidget()
        lst.setSelectionMode(QAbstractItemView.SelectionMode.MultiSelection)
        for e in entries:
            label = Path(e.path).name + ("" if e.exists else "  (missing)")
            item  = QListWidgetItem(label)
            item.setData(Qt.ItemDataRole.UserRole, e)
            if not e.exists:
                item.setForeground(QColor("#ff6666"))
            lst.addItem(item)
        # Select all existing by default
        for i in range(lst.count()):
            item = lst.item(i)
            e = item.data(Qt.ItemDataRole.UserRole)
            if e.exists:
                item.setSelected(True)
        lo.addWidget(lst)

        # Select all / none buttons
        br = QHBoxLayout()
        for label, sel in [("All", True), ("None", False), ("Existing only", None)]:
            b = QPushButton(label)
            def _click(checked=False, s=sel, L=lst, E=entries):
                for i in range(L.count()):
                    item = L.item(i)
                    e = item.data(Qt.ItemDataRole.UserRole)
                    if s is None:
                        item.setSelected(e.exists)
                    else:
                        item.setSelected(s)
            b.clicked.connect(_click)
            br.addWidget(b)
        lo.addLayout(br)

        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                                 QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        lo.addWidget(btns)

        if dlg.exec() != QDialog.DialogCode.Accepted:
            return None
        return [lst.item(i).data(Qt.ItemDataRole.UserRole)
                for i in range(lst.count()) if lst.item(i).isSelected()]

    #    Entry operations                                                       
    def _push_undo(self): #vers 2
        """Snapshot every loaded file's inst entries (one undo step)."""
        if not self._files:
            return
        import copy
        self._undo_stack.append(self._snapshot(copy))
        del self._undo_stack[:-30]
        self._redo_stack.clear()

    def _snapshot(self, copy):
        return [(f, [copy.deepcopy(e) for e in f.instances], f.inst_section.reordered)
                for f in self._files if f.inst_section]

    def _restore(self, snap):
        for f, entries, reordered in snap:
            f.inst_section.entries = entries
            f.inst_section.reordered = reordered
        self._populate_table()

    def _undo(self): #vers 2
        if not self._files or not self._undo_stack:
            self._set_status("Nothing to undo")
            return
        import copy
        self._redo_stack.append(self._snapshot(copy))
        self._restore(self._undo_stack.pop())
        self._set_status("Undo")

    def _redo(self): #vers 2
        if not self._files or not self._redo_stack:
            self._set_status("Nothing to redo")
            return
        import copy
        self._undo_stack.append(self._snapshot(copy))
        self._restore(self._redo_stack.pop())
        self._set_status("Redo")

    def _add_entry(self): #vers 2
        if not self._files:
            self._set_status("Load an IPL file first")
            return
        rows = self._selected_view_indices()
        view = self._view_entries()
        owner = self._owner.get(id(view[rows[-1]])) if rows else self._files[0]
        sec = owner.inst_section
        if sec is None:
            self._set_status("That file has no inst section")
            return
        self._push_undo()
        new_e = IPLEntry(model_id=0, model_name="new_obj", layout=owner._new_layout())
        pos = sec.entries.index(view[rows[-1]]) + 1 if rows else len(sec.entries)
        sec.entries.insert(pos, new_e)
        self._populate_table()
        self._update_section_list_count()
        self._set_status("Entry added")

    def _delete_selected(self): #vers 2
        rows = self._selected_view_indices()
        if not rows:
            return
        if QMessageBox.question(self, "Delete", f"Delete {len(rows)} selected entries?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                ) != QMessageBox.StandardButton.Yes:
            return
        self._push_undo()
        view = self._view_entries()
        doomed = {id(view[r]) for r in rows if r < len(view)}
        for f in self._files:
            if f.inst_section:
                f.inst_section.entries = [e for e in f.inst_section.entries if id(e) not in doomed]
        self._populate_table()
        self._update_section_list_count()
        self._set_status(f"Deleted {len(doomed)} entries")

    def _duplicate_selected(self): #vers 2
        rows = self._selected_view_indices()
        if not rows:
            return
        view = self._view_entries()
        self._push_undo()
        for r in reversed(rows):
            e = view[r]
            sec = self._owner[id(e)].inst_section
            sec.entries.insert(sec.entries.index(e) + 1, e.copy_as_new())
        self._populate_table()
        self._update_section_list_count()
        self._set_status(f"Duplicated {len(rows)} entries")

    def _select_all(self):
        self._table.selectAll()

    def _update_section_list_count(self, sec=None): #vers 2
        item = self._section_list.item(0) if self._section_list.count() else None
        if item and item.data(Qt.ItemDataRole.UserRole) == ("inst", -1):
            item.setText(f"inst  ({len(self._view_entries())})")

    #    Search / stats                                                         
    def _show_find(self):
        text, ok = self._input_dialog("Find Model", "Model name contains:")
        if ok and text:
            self._search_box.setText(text)

    def _filter_interior(self):
        text, ok = self._input_dialog("Filter Interior", "Interior index (0=outdoor):")
        if ok:
            try:
                idx = int(text)
                for i in range(self._int_filter.count()):
                    if self._int_filter.itemData(i) == idx:
                        self._int_filter.setCurrentIndex(i)
                        return
            except ValueError:
                pass

    def _input_dialog(self, title, label):
        from PyQt6.QtWidgets import QInputDialog
        return QInputDialog.getText(self, title, label)

    def _fit_columns(self): #vers 1
        self._table.resizeColumnsToContents()
        self._table.horizontalHeader().setSectionResizeMode(
            self.COL_MODEL, QHeaderView.ResizeMode.Stretch)

    def _show_stats(self): #vers 2
        view = self._view_entries()
        if not view:
            QMessageBox.information(self, "Stats", "Load an IPL file first.")
            return
        from collections import Counter
        top = Counter(e.model_name for e in view).most_common(8)
        interiors = sorted(set(e.interior for e in view))
        QMessageBox.information(self, "IPL Statistics",
            f"Files:      {len(self._files)}\n"
            f"Game:       {self._files[0].game}\n"
            f"Instances:  {len(view)}\n"
            f"Interiors:  {len(interiors)}  ({interiors[:5]}{'...' if len(interiors) > 5 else ''})\n"
            f"Models:     {len(set(e.model_name for e in view))} unique\n\n"
            "Most placed:\n" + "\n".join(f"  {n}: {c}" for n, c in top))

    def _update_info(self): #vers 2
        if not self._files:
            self._info_lbl.setText("No file loaded")
            return
        name = Path(self._file_path).name if len(self._files) == 1 else f"{len(self._files)} IPL files"
        self._info_lbl.setText(f"{name}\n{len(self._view_entries())} instances\ngame={self._files[0].game}")

    #    Context menu                                                           
    def _show_context_menu(self, pos):
        menu = QMenu(self)
        menu.addAction("Add Entry",           self._add_entry)
        menu.addAction("Delete Selected",     self._delete_selected)
        menu.addAction("Duplicate Selected",  self._duplicate_selected)
        menu.addSeparator()
        menu.addAction("Export CSV…",         self._export_csv)
        menu.exec(self._table.viewport().mapToGlobal(pos))

    #    Ribbon UI / state                                                      
    def _create_toolbar(self): #vers 2
        """Titlebar keeps Settings / title / Undo / Info / Theme; file buttons
        moved to the File ribbon."""
        tb = super()._create_toolbar()
        for name in ("open_btn", "save_btn", "export_btn", "import_btn"):
            btn = getattr(self, name, None)
            if btn:
                btn.setVisible(False)
        return tb

    def setup_ui(self): #vers 2
        from PyQt6.QtWidgets import QSplitter
        ml = QVBoxLayout(self)
        ml.setContentsMargins(*self.get_content_margins())
        ml.setSpacing(self.setspacing)
        ml.addWidget(self._create_toolbar())
        sp = QSplitter(Qt.Orientation.Horizontal)
        sp.addWidget(self._create_left_panel())
        sp.addWidget(self._create_centre_panel())
        sp.setStretchFactor(0, 1)
        sp.setStretchFactor(1, 6)
        sp.setSizes([200, 1100])
        self._main_splitter = sp
        self._draw_btns = {}
        ml.addWidget(self.ribbon_wrap(sp), 1)
        self._build_ribbons()
        self._status_widget = self._create_status_bar()
        ml.addWidget(self._status_widget)
        self._status_widget.setVisible(self.WS.get("show_statusbar", True))

    def _ide_active(self) -> bool:
        return self._centre_tabs.currentWidget() is self._ide_panel

    def _ctx(self, ide_fn, ipl_fn):
        """Button action that goes to the IDE tab or the IPL table, whichever is showing."""
        return lambda: (ide_fn() if self._ide_active() else ipl_fn())

    def _build_ribbons(self): #vers 2
        B, P = self.ribbon_button, self._ide_panel
        tb = self.ribbon_toolbar("File")
        B(tb, "open_icon",   "Open an IPL or IDE file  (Ctrl+O)", self._open_any, text="Open")
        B(tb, "import_icon", "Load a whole game from its .dat: the IDE files first, then the IPLs", self._load_all_from_dat)
        self.save_btn = B(tb, "save_icon", "Save every changed IDE and IPL file  (Ctrl+S)", self._save_file, enabled=False)
        B(tb, "saveas_icon", "Save As (single IPL file)...", self._save_as)
        tb.addSeparator()
        B(tb, "export_icon", "Export CSV of the IPL entries", self._export_csv)

        tb = self.ribbon_toolbar("Edit")
        B(tb, "undo_icon", "Undo  (Ctrl+Z)", self._ctx(P._undo, self._undo))
        B(tb, "redo_icon", "Redo  (Ctrl+Y)", self._ctx(P._redo, self._redo))
        tb.addSeparator()
        B(tb, "add_icon",   "Add row / entry", self._ctx(P.add_ide_entry, self._add_entry))
        B(tb, "trash_icon", "Delete selected", self._ctx(P.delete_ide_entry, self._delete_selected))
        B(tb, "edit_icon",  "Duplicate selected", self._ctx(P._duplicate_rows, self._duplicate_selected))
        tb.addSeparator()
        B(tb, "search_icon", "Find  (Ctrl+F)", self._ctx(lambda: P._search.setFocus(), self._show_find))
        B(tb, "locate_icon", "Select all  (Ctrl+A)", self._ctx(lambda: P._table.selectAll(), self._select_all))

        tb = self.ribbon_toolbar("View")
        B(tb, "fit_grid_icon", "Fit columns", self._ctx(lambda: P._table.resizeColumnsToContents(), self._fit_columns))
        B(tb, "info_icon",     "Statistics", self._ctx(P._show_stats, self._show_stats))

        self._ribbon_mw.addToolBarBreak()
        tb = self.ribbon_toolbar("IDE Tools")
        self.ribbon_label(tb, "IDE")
        B(tb, "check_icon",   "Check the IDE: duplicate IDs / names, empty TXD, long names", P._run_checks)
        B(tb, "search_icon",  "Free IDs", P._show_free_ids, text="Free")
        B(tb, "edit_icon",    "Set one column on the selected rows...", P._set_column, text="Set")
        B(tb, "convert_icon", "Renumber the selected rows from an ID...", P._renumber, text="Renum")
        B(tb, "edit_icon",    "Add a prefix / suffix to the selected model names...", P._prefix_suffix, text="Name")
        B(tb, "convert_icon", "Sort this IDE section (ID, name, TXD, draw distance, most placed...)", P._sort_menu, text="Sort")
        B(tb, "package_icon", "Merge rows from another IDE file...", P._merge_ide, text="Merge")

        tb = self.ribbon_toolbar("IPL Tools")
        self.ribbon_label(tb, "IPL")
        B(tb, "check_icon",   "Check IPL: duplicates, LOD links, bad positions, unknown models (vs the loaded IDE)", self._run_checks)
        B(tb, "convert_icon", "Sort the IPL entries (ID, name, position, most placed...)", self._sort_menu_ipl, text="Sort")
        B(tb, "trash_icon",   "Remove exact duplicate placements", self._remove_duplicates, text="Dupes")
        B(tb, "convert_icon", "Move selected (or all) by X/Y/Z...", self._translate_dialog, text="Move")
        B(tb, "edit_icon",    "Set model ID / name on selected...", self._set_model_dialog, text="Model")
        B(tb, "package_icon", "Merge entries from other IPL file(s)...", self._merge_ipl, text="Merge")
        B(tb, "export_icon",  "Export selected (or all) entries to a NEW IPL...", self._export_selection, text="Split")

    def closeEvent(self, ev): #vers 1
        if self.standalone_mode and (any(f.dirty for f in self._files) or self._ide_panel.has_unsaved()):
            r = QMessageBox.question(
                self, App_name, "Save changed IPL files before closing?",
                QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard
                | QMessageBox.StandardButton.Cancel)
            if r == QMessageBox.StandardButton.Cancel:
                ev.ignore()
                return
            if r == QMessageBox.StandardButton.Save:
                self._save_file()
                if any(f.dirty for f in self._files) or self._ide_panel.has_unsaved():
                    ev.ignore()
                    return
        self.ribbon_save_state()
        super().closeEvent(ev)

    def _view_entries(self) -> list: #vers 1
        return [e for f in self._files for e in f.instances]

    def _file_of(self, e) -> str: #vers 1
        f = self._owner.get(id(e))
        return Path(f.path).name if f else ""

    def _selected_view_indices(self) -> list: #vers 1
        rows = {i.row() for i in self._table.selectedItems()}
        out = []
        for r in sorted(rows):
            idc = self._table.item(r, self.COL_ID)
            if idc is not None and idc.data(Qt.ItemDataRole.UserRole) is not None:
                out.append(idc.data(Qt.ItemDataRole.UserRole))
        return sorted(set(out))

    def _after_load(self): #vers 1
        self._undo_stack.clear()
        self._redo_stack.clear()
        self._owner = {id(e): f for f in self._files for e in f.instances}
        self._populate_section_list()
        self._select_inst_section()
        self._populate_table()
        self._update_info()

    def _update_dirty(self): #vers 2
        n = sum(1 for f in self._files if f.dirty)
        m = sum(1 for f in self._ide_panel._files if f.dirty)
        parts = ([f"{m} IDE"] if m else []) + ([f"{n} IPL"] if n else [])
        self._dirty_lbl.setText("Modified: " + " + ".join(parts) if parts else "Modified: no")
        if hasattr(self, "save_btn"):
            self.save_btn.setEnabled(bool(n or m))

    #    IDE + IPL together                                                    
    def _open_any(self): #vers 1
        paths, _ = QFileDialog.getOpenFileNames(
            self, "Open IPL or IDE file(s)", "",
            "IPL / IDE Files (*.ipl *.IPL *.ide *.IDE *.ifx);;IPL (*.ipl *.IPL);;IDE (*.ide *.IDE *.ifx);;All Files (*)")
        ides = [p for p in paths if p.lower().endswith((".ide", ".ifx"))]
        ipls = [p for p in paths if p not in ides]
        if ides:
            self._ide_panel._load_files(ides, replace=True)
            self.show_ide_tab()
        if ipls:
            self._open_file(ipls[0])

    def load_ide_file(self, file_path: str) -> bool: #vers 1
        """Load an IDE into the IDE tab (API used by IMG Factory / DAT Browser)."""
        ok = self._ide_panel.load_ide_file(file_path)
        self.show_ide_tab()
        return ok

    def show_ide_tab(self): #vers 1
        self._centre_tabs.setCurrentWidget(self._ide_panel)

    def _cascade_ide_changes(self, id_map, renames): #vers 1
        """IDE -> IPL: apply saved ID changes / renames to the placements loaded
        here (in memory, one undo step); Save writes those IPL files. Returns None
        when no IPL is loaded so the IDE panel falls back to editing files on disk."""
        if not self._files:
            return None
        hits = []
        for e in self._view_entries():
            new_id = id_map.get(e.model_id, e.model_id)
            new_name = e.model_name
            for rid, old, new in renames:
                if rid == new_id and e.model_name.lower() == old.lower():
                    new_name = new
                    break
            if new_id != e.model_id or new_name != e.model_name:
                hits.append((e, new_id, new_name))
        if hits:
            self._push_undo()
            for e, nid, nn in hits:
                e.model_id, e.model_name = nid, nn
            self._populate_table()
        return f"  |  {len(hits)} IPL placement(s) updated - Save writes them"

    #    Sorting (same idea as the Map Workshop's Sort menu)                   
    def _usage_counts(self):
        """model id -> how many placements use it across every loaded IPL."""
        from collections import Counter
        return Counter(e.model_id for e in self._view_entries())

    def _sort_menu_ipl(self): #vers 1
        from PyQt6.QtWidgets import QMenu
        from PyQt6.QtGui import QCursor
        m = QMenu(self)
        for label, key, rev in (
                ("Model ID (low to high)", "id", False), ("Model ID (high to low)", "id", True),
                ("Model name A-Z", "name", False), ("Model name Z-A", "name", True),
                ("Position X then Y", "xy", False), ("Position Y then X", "yx", False),
                ("Height Z (low to high)", "z", False), ("Interior", "interior", False),
                ("Most placed models first", "usage", True)):
            m.addAction(label, lambda k=key, r=rev, lb=label: self._sort_ipl(k, r, lb))
        m.exec(QCursor.pos())

    def _sort_ipl(self, key: str, reverse: bool = False, label: str = ""): #vers 1
        """Re-order the inst entries of every loaded file (each file on its own).
        Comment / other lines stay where they are. SA LOD links point at entry
        NUMBERS, so they are re-pointed to follow their target."""
        if not self._files:
            self._set_status("Load an IPL file first")
            return
        use = self._usage_counts() if key == "usage" else None
        keyf = {
            "id":       lambda e: (e.model_id, e.model_name.lower()),
            "name":     lambda e: (e.model_name.lower(), e.model_id),
            "xy":       lambda e: (round(e.px, 3), round(e.py, 3)),
            "yx":       lambda e: (round(e.py, 3), round(e.px, 3)),
            "z":        lambda e: e.pz,
            "interior": lambda e: (e.interior, e.model_id),
            "usage":    lambda e: (use[e.model_id], -e.model_id),
        }[key]
        self._push_undo()
        moved_lods = 0
        for f in self._files:
            sec = f.inst_section
            if sec is None or len(sec.entries) < 2:
                continue
            old = list(sec.entries)
            new = sorted(old, key=keyf, reverse=reverse)
            if new == old:
                continue
            pos = {id(e): i for i, e in enumerate(new)}
            if any(e.layout == "sa" and e.lod >= 0 for e in old):
                for e in old:                           # re-point LOD links at the moved targets
                    if e.layout == "sa" and 0 <= e.lod < len(old):
                        e.lod = pos[id(old[e.lod])]
                        moved_lods += 1
            sec.entries = new
            sec.reordered = True
        self._populate_table()
        self._update_section_list_count()
        self._set_status(f"Sorted IPL entries by {label or key}"
                         + (f"  |  {moved_lods} LOD link(s) re-pointed" if moved_lods else ""))

    #    Tools (use cases)                                                      
    def _ide_declared(self, ask=True):  #vers 1
        """{model_id: lowercase name} from the game's IDE files, or None."""
        from apps.methods.master_ide import load_master_ide
        from apps.methods.id_reassign import _ID_DECLARING_SECTIONS
        if self._ide_panel._files:              # the IDE tab is the source (incl. unsaved edits)
            declared = {}
            for f in self._ide_panel._files:
                for sec in f.sections:
                    if sec.name in _ID_DECLARING_SECTIONS:
                        for r in sec.rows:
                            try:
                                declared.setdefault(int(r.fields[0]), r.fields[1].lower() if sec.name != "2dfx" else "")
                            except (ValueError, IndexError):
                                pass
            return {k: v for k, v in declared.items() if v}
        ides = []
        if self._dat_path:
            try:
                from apps.methods.asset_checker import find_game_asset_files
                _i, _c, ides, _g = find_game_asset_files(self._dat_path)
            except Exception:
                ides = []
        if not ides and ask:
            ides, _ = QFileDialog.getOpenFileNames(
                self, "Pick the IDE file(s) these placements use (Cancel to skip)", "",
                "IDE Files (*.ide *.IDE *.ifx);;All Files (*)")
        if not ides:
            return None
        res = load_master_ide(list(ides))
        declared = {}
        for sec in _ID_DECLARING_SECTIONS:
            for o in res.objects_by_section.get(sec, []):
                declared.setdefault(o.model_id, o.model_name.lower())
        return declared

    def _run_checks(self): #vers 1
        """Read-only report: duplicate placements, LOD links (SA), bad
        positions, and (when IDE files are available) unknown / renamed models."""
        if not self._files:
            self._set_status("Load an IPL file first")
            return
        import math
        out, problems = [], 0
        def sect(title, rows):
            nonlocal problems
            problems += len(rows)
            out.append(f"== {title}: {len(rows)}")
            out.extend(rows[:150])
            if len(rows) > 150:
                out.append(f"   ... {len(rows) - 150} more")
            out.append("")
        dups, lods, bad = [], [], []
        for f in self._files:
            nm = Path(f.path).name
            seen = {}
            ents = f.instances
            for i, e in enumerate(ents):
                key = (e.model_id, round(e.px, 3), round(e.py, 3), round(e.pz, 3))
                if key in seen:
                    dups.append(f"   {nm}: #{i} {e.model_name} ({e.model_id}) same spot as #{seen[key]}")
                else:
                    seen[key] = i
                if e.layout == "sa" and e.lod >= 0 and (e.lod >= len(ents) or e.lod == i):
                    lods.append(f"   {nm}: #{i} {e.model_name} lod index {e.lod} (file has {len(ents)} entries)")
                if any(math.isnan(v) or abs(v) > 20000 for v in (e.px, e.py, e.pz)):
                    bad.append(f"   {nm}: #{i} {e.model_name} at {e.px:g}, {e.py:g}, {e.pz:g}")
        sect("Duplicate placements (same model, same position)", dups)
        sect("Broken LOD links (SA)", lods)
        sect("Positions outside +-20000 / not a number", bad)
        declared = self._ide_declared()
        if declared is None:
            out.append("== Model check skipped (no IDE files chosen)")
        else:
            unk, ren = [], []
            for f in self._files:
                nm = Path(f.path).name
                for i, e in enumerate(f.instances):
                    if e.model_id not in declared:
                        unk.append(f"   {nm}: #{i} id {e.model_id} '{e.model_name}' not declared in the IDE files")
                    elif declared[e.model_id] != e.model_name.lower():
                        ren.append(f"   {nm}: #{i} id {e.model_id} '{e.model_name}' but IDE says '{declared[e.model_id]}'")
            sect("Placements of unknown model IDs", unk)
            sect("Model name differs from IDE", ren)
            out.append("(unknown IDs are only meaningful when every IDE of the game was loaded)")
        from apps.methods.asset_integrity import show_integrity_dialog
        show_integrity_dialog(self, f"Problems found: {problems}\n\n" + "\n".join(out))

    def _remove_duplicates(self): #vers 1
        if not self._files:
            return
        doomed = []
        for f in self._files:
            seen = set()
            for e in f.instances:
                key = (e.model_id, round(e.px, 3), round(e.py, 3), round(e.pz, 3), e.interior)
                if key in seen:
                    doomed.append(id(e))
                seen.add(key)
        if not doomed:
            QMessageBox.information(self, "Duplicates", "No exact duplicate placements found.")
            return
        if QMessageBox.question(self, "Remove duplicates",
                f"Remove {len(doomed)} duplicate placement(s)? (the first of each stays)"
                ) != QMessageBox.StandardButton.Yes:
            return
        self._push_undo()
        dead = set(doomed)
        for f in self._files:
            f.inst_section.entries = [e for e in f.instances if id(e) not in dead]
        self._populate_table()
        self._update_section_list_count()
        self._set_status(f"Removed {len(doomed)} duplicate placements")

    def _targets(self):
        """Selected entries, or every entry when nothing is selected."""
        view = self._view_entries()
        rows = self._selected_view_indices()
        return [view[r] for r in rows] if rows else view, bool(rows)

    def _translate_dialog(self): #vers 1
        ents, sel = self._targets()
        if not ents:
            return
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Move {len(ents)} {'selected' if sel else 'all'} entries")
        fl = QFormLayout(dlg)
        sp = []
        for label in ("X", "Y", "Z"):
            d = QDoubleSpinBox(); d.setRange(-100000, 100000); d.setDecimals(3)
            fl.addRow(f"Offset {label}:", d); sp.append(d)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        fl.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        dx, dy, dz = (d.value() for d in sp)
        if dx == dy == dz == 0:
            return
        self._push_undo()
        for e in ents:
            e.px += dx; e.py += dy; e.pz += dz
        self._populate_table()
        self._set_status(f"Moved {len(ents)} entries by {dx:+g}, {dy:+g}, {dz:+g}")

    def _set_model_dialog(self): #vers 1
        ents, sel = self._targets()
        if not sel:
            QMessageBox.information(self, "Set model", "Select the rows to change first.")
            return
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Set model on {len(ents)} entries")
        fl = QFormLayout(dlg)
        from PyQt6.QtWidgets import QSpinBox
        idsp = QSpinBox(); idsp.setRange(0, 65535); idsp.setValue(ents[0].model_id)
        name = QLineEdit(ents[0].model_name)
        fl.addRow("Model ID:", idsp); fl.addRow("Model name:", name)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        fl.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted or not name.text().strip():
            return
        self._push_undo()
        for e in ents:
            e.model_id, e.model_name = idsp.value(), name.text().strip()
        self._populate_table()
        self._set_status(f"Set model {name.text().strip()} ({idsp.value()}) on {len(ents)} entries")

    def _merge_ipl(self): #vers 1
        """Copy the inst entries of other IPL file(s) into the target file
        (the one owning the first selected row, else the first loaded)."""
        if not self._files:
            self._set_status("Load an IPL file first")
            return
        paths, _ = QFileDialog.getOpenFileNames(self, "Merge entries from IPL file(s)", "",
                                                "IPL Files (*.ipl *.IPL);;All Files (*)")
        if not paths:
            return
        view, rows = self._view_entries(), self._selected_view_indices()
        target = self._owner.get(id(view[rows[0]])) if rows else self._files[0]
        if target.inst_section is None:
            self._set_status("Target file has no inst section")
            return
        incoming, failed = [], []
        for p in paths:
            src = IPLFile()
            try:
                src.load(p)
            except Exception as e:
                failed.append(f"{Path(p).name}: {e}")
                continue
            incoming += [e.copy_as_new() for e in src.instances]
        if not incoming:
            QMessageBox.warning(self, "Merge", "Nothing to merge.\n" + "\n".join(failed))
            return
        if QMessageBox.question(self, "Merge",
                f"Add {len(incoming)} entries to {Path(target.path).name}?"
                ) != QMessageBox.StandardButton.Yes:
            return
        self._push_undo()
        for e in incoming:
            e.layout = target._new_layout()
        target.inst_section.entries.extend(incoming)
        self._populate_table()
        self._update_section_list_count()
        self._set_status(f"Merged {len(incoming)} entries into {Path(target.path).name}")

    def _export_selection(self): #vers 1
        ents, sel = self._targets()
        if not ents:
            return
        p, _ = QFileDialog.getSaveFileName(self, "Export entries to a new IPL", "split.ipl",
                                           "IPL Files (*.ipl)")
        if not p:
            return
        if os.path.exists(p):
            QMessageBox.warning(self, "Export", "That file already exists - pick a new name "
                                "(use Save to change existing files).")
            return
        try:
            lay = self._files[0]._new_layout()
            lines = []
            for e in ents:
                c = e.copy_as_new(); c.layout = lay
                lines.append(c.new_line())
            Path(p).write_text("\n".join(["inst"] + lines + ["end", ""]), encoding="latin1")
            self._set_status(f"Exported {len(ents)} entries to {Path(p).name}")
        except Exception as e:
            QMessageBox.critical(self, "Export Error", str(e))


    #    GUIWorkshop stubs                                                      
    def _on_list_selection_changed(self, row: int): pass
    def _on_add_item(self): self._add_entry()
    def _on_remove_item(self): self._delete_selected()
    def _on_tab_changed(self, idx: int): pass
    def _zoom(self, f): pass
    def _fit(self): self._fit_columns()
    def _jump(self): pass
    def _on_toolbar_action(self, action: str): pass
    def _copy_item(self): pass
    def _paste_item(self): pass


# =============================================================================
# Docked opener (called from imgfactory)
# =============================================================================

def open_ipl_workshop(main_window, file_path=None):
    """Open IPL Workshop docked in IMG Factory tab. Returns workshop instance."""
    try:
        from PyQt6.QtWidgets import QVBoxLayout, QWidget
        from PyQt6.QtCore import Qt
        from apps.methods.imgfactory_svg_icons import SVGIconFactory

        if hasattr(main_window, 'main_tab_widget') and main_window.main_tab_widget:
            tw = main_window.main_tab_widget
            # Re-use existing tab
            for i in range(tw.count()):
                w = tw.widget(i)
                if w:
                    found = w.findChildren(IPLWorkshop)
                    if found:
                        tw.setCurrentIndex(i)
                        if file_path:
                            found[0]._open_file(file_path)
                        return found[0]

            # New docked tab
            tab = QWidget()
            tab.file_type = "WORKSHOP"
            lo = QVBoxLayout(tab)
            lo.setContentsMargins(0, 0, 0, 0)
            lo.setSpacing(0)

            workshop = IPLWorkshop(tab, main_window)
            workshop.setWindowFlags(Qt.WindowType.Widget)
            lo.addWidget(workshop)

            try:
                from apps.methods.imgfactory_svg_icons import get_ipl_editor_icon
                icon = get_ipl_editor_icon(20)
                idx = tw.addTab(tab, icon, "IPL")
            except Exception:
                idx = tw.addTab(tab, "IPL")
            tw.setCurrentIndex(idx)
            workshop.show()

            if file_path:
                from PyQt6.QtCore import QTimer
                QTimer.singleShot(100, lambda: workshop._open_file(file_path))

            # Ensure tab area visible
            if hasattr(main_window, '_ensure_tab_area_visible'):
                main_window._ensure_tab_area_visible()

            # Register in taskbar
            try:
                from apps.gui.gui_layout import _register_tool_taskbar
                from apps.methods.imgfactory_svg_icons import get_ipl_editor_icon
                _register_tool_taskbar(main_window, "ipl", "IPL",
                    get_ipl_editor_icon,
                    "IPL Workshop — Item Placement Editor",
                    target=tab)
            except Exception as e:
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"IPL taskbar error: {e}")

            if hasattr(main_window, 'log_message'):
                main_window.log_message("IPL Workshop opened (docked)")
            return workshop

        # Standalone fallback (outside imgfactory)
        workshop = IPLWorkshop(parent=None, main_window=main_window)
        workshop.setWindowTitle("IPL Workshop — Standalone")
        workshop.resize(1300, 800)
        workshop.show()
        if file_path:
            workshop._open_file(file_path)
        return workshop

    except Exception as e:
        import traceback
        traceback.print_exc()
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"IPL Workshop error: {e}")


# =============================================================================
# Standalone launcher
# =============================================================================

if __name__ == "__main__":
    import traceback
    print(f"{App_name} {Build} starting...")
    try:
        app = QApplication(sys.argv)
        w = IPLWorkshop()
        w.setWindowTitle(f"{App_name} — Standalone")
        w.resize(1300, 800)
        w.show()
        if len(sys.argv) > 1 and Path(sys.argv[1]).is_file():
            w._open_file(sys.argv[1])
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)


#    IPL World Map                                                              

