#this belongs in apps/components/Model_Workshop/model_workshop.py - Version: 1
# X-Seti - Apr 2026 - Model Workshop
# Based on COL Workshop architecture (X-Seti/Img-Factory-1.6)
"""
Model Workshop — GTA RenderWare DFF model viewer and editor.

Companion tool to IMG Factory 1.6, Col Workshop, and TXD Workshop.
Supports GTA III / VC / SA PC DFF files.

Architecture mirrors COL Workshop:
  - Standalone mode (main_window=None): full window with titlebar
  - Docked mode (main_window=<ref>): embedded in IMG Factory tab
  - SVGIconFactory for all icons (no emoji, no bitmap icons)
  - QWidget base (not QMainWindow) for docking support
"""

import os
import sys
import struct
from typing import Optional, List, Dict

from PyQt6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QSplitter,
    QListWidget, QListWidgetItem, QLabel, QPushButton, QFrame,
    QFileDialog, QMessageBox, QTableWidget, QTableWidgetItem,
    QAbstractItemView, QSizePolicy, QGroupBox, QHeaderView,
    QMenu, QStatusBar, QComboBox
)
from PyQt6.QtCore import Qt, pyqtSignal, QSize, QPoint, QRect
from PyQt6.QtGui import QFont, QIcon, QColor, QPainter, QPen, QBrush, QCursor

from apps.methods.imgfactory_svg_icons import SVGIconFactory

try:
    from apps.methods.dff_parser import load_dff, detect_dff
    from apps.methods.dff_classes import DFFModel, Geometry, Frame, Material
    DFF_AVAILABLE = True
except ImportError:
    DFF_AVAILABLE = False

App_name       = "Model Workshop"
App_version    = "1.0"
App_build_num  = "Build 1"
DEBUG_STANDALONE = False


# ── Simple 3D preview widget ───────────────────────────────────────────────────

class ModelViewport(QWidget):  #vers 1
    """Minimal wireframe preview for DFF geometry using QPainter orthographic projection."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setMinimumSize(300, 300)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self.setStyleSheet("background-color: #2a2a2a;")

        self._geometry: Optional[object] = None   # Geometry dataclass
        self._yaw   = 30.0
        self._pitch = 20.0
        self._zoom  = 1.0
        self._drag_start: Optional[QPoint] = None
        self._last_yaw  = self._yaw
        self._last_pitch = self._pitch
        self.placeholder = "No model loaded"
        self.setMouseTracking(True)

    def set_geometry(self, geom):
        """Set a Geometry object to display."""
        self._geometry = geom
        self._auto_fit()
        self.update()

    def clear(self):
        self._geometry = None
        self.update()

    def _auto_fit(self):
        """Auto-scale zoom to fit bounding sphere."""
        if self._geometry and self._geometry.bounding_sphere.radius > 0:
            r = self._geometry.bounding_sphere.radius
            self._zoom = min(self.width(), self.height()) / (r * 4.0) if r > 0 else 1.0

    def paintEvent(self, event):
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)

        w, h = self.width(), self.height()
        cx, cy = w // 2, h // 2

        # Background
        p.fillRect(0, 0, w, h, QColor(42, 42, 42))

        if not self._geometry or not self._geometry.vertices:
            p.setPen(QColor(100, 100, 100))
            p.setFont(QFont("Arial", 11))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, self.placeholder)
            return

        geom = self._geometry
        verts = geom.vertices
        tris  = geom.triangles

        # Simple orthographic projection with yaw/pitch rotation
        import math
        yaw_r   = math.radians(self._yaw)
        pitch_r = math.radians(self._pitch)
        cy_r, sy_r = math.cos(yaw_r),   math.sin(yaw_r)
        cp_r, sp_r = math.cos(pitch_r), math.sin(pitch_r)

        bs = geom.bounding_sphere
        ox, oy, oz = bs.center.x, bs.center.y, bs.center.z

        def project(v):
            x, y, z = v.x - ox, v.y - oy, v.z - oz
            # Yaw rotation (around Z)
            rx =  x * cy_r + y * sy_r
            ry = -x * sy_r + y * cy_r
            rz = z
            # Pitch rotation (around X)
            px  =  rx
            py  =  ry * cp_r - rz * sp_r
            pz  =  ry * sp_r + rz * cp_r
            scale = self._zoom
            sx = int(cx + px * scale)
            sy = int(cy - py * scale)
            return sx, sy, pz

        # Project all vertices
        proj = [project(v) for v in verts]

        # Draw wireframe triangles
        p.setPen(QPen(QColor(80, 180, 120), 0.8))
        for tri in tris:
            try:
                ax, ay, az = proj[tri.v1]
                bx, by, bz = proj[tri.v2]
                cx2, cy2, cz2 = proj[tri.v3]
                p.drawLine(ax, ay, bx, by)
                p.drawLine(bx, by, cx2, cy2)
                p.drawLine(cx2, cy2, ax, ay)
            except IndexError:
                pass

        # Stats overlay
        p.setPen(QColor(160, 160, 160))
        p.setFont(QFont("Courier New", 8))
        p.drawText(6, 16, f"Verts: {geom.vertex_count}  Tris: {geom.triangle_count}")
        p.drawText(6, 28, f"Mats:  {geom.material_count}")

    def mousePressEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton:
            self._drag_start  = e.position().toPoint()
            self._last_yaw    = self._yaw
            self._last_pitch  = self._pitch

    def mouseMoveEvent(self, e):
        if self._drag_start and e.buttons() & Qt.MouseButton.LeftButton:
            delta = e.position().toPoint() - self._drag_start
            self._yaw   = self._last_yaw   + delta.x() * 0.5
            self._pitch = self._last_pitch + delta.y() * 0.5
            self.update()

    def mouseReleaseEvent(self, e):
        self._drag_start = None

    def wheelEvent(self, e):
        delta = e.angleDelta().y()
        factor = 1.1 if delta > 0 else 0.9
        self._zoom = max(0.01, min(1000.0, self._zoom * factor))
        self.update()


# ── Main Model Workshop widget ─────────────────────────────────────────────────

class ModelWorkshop(QWidget):  #vers 1
    """
    Model Workshop main widget.
    Mirrors COL Workshop architecture: standalone or docked.
    """

    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    def __init__(self, parent=None, main_window=None):  #vers 1
        super().__init__(parent)
        self.setWindowTitle(App_name)

        try:
            self.setWindowIcon(SVGIconFactory.col_workshop_icon())
        except Exception:
            pass

        self.icon_factory   = SVGIconFactory()
        self.main_window    = main_window
        self.standalone_mode = (main_window is None)

        # State
        self.current_dff_path: Optional[str] = None
        self.current_model:    Optional[object] = None   # DFFModel
        self.selected_geometry_index: int = -1
        self.undo_stack: List[dict] = []

        # Fonts
        self.title_font   = QFont("Arial", 14)
        self.panel_font   = QFont("Arial", 10)
        self.button_font  = QFont("Arial", 10)

        # App settings
        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        else:
            try:
                from apps.utils.app_settings_system import AppSettings
                self.app_settings = AppSettings()
            except Exception:
                self.app_settings = None

        # Docking
        self.is_docked = (main_window is not None)

        # Window setup
        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        self.setMinimumSize(900, 600)
        self.resize(1300, 800)

        # Corner drag state
        self.dragging      = False
        self.drag_position = None
        self.resizing      = False
        self.resize_corner = None
        self.corner_size   = 20
        self.hover_corner  = None

        self.setup_ui()
        self._setup_hotkeys()
        self._apply_theme()

    # ── UI construction ──────────────────────────────────────────────────────

    def setup_ui(self):  #vers 1
        """Build the main layout: toolbar / left list / centre viewport / right info."""
        root = QVBoxLayout(self)
        root.setContentsMargins(5, 5, 5, 5)
        root.setSpacing(4)

        # Titlebar (standalone only)
        if self.standalone_mode:
            root.addWidget(self._create_titlebar())

        # Toolbar
        root.addWidget(self._create_toolbar())

        # Content splitter
        splitter = QSplitter(Qt.Orientation.Horizontal)

        splitter.addWidget(self._create_left_panel())    # geometry/mesh list
        splitter.addWidget(self._create_viewport())      # 3D wireframe
        splitter.addWidget(self._create_right_panel())   # info + materials

        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 3)
        splitter.setStretchFactor(2, 0)
        splitter.setSizes([220, 700, 280])
        root.addWidget(splitter, stretch=1)

        # Status bar
        root.addWidget(self._create_statusbar())

    def _create_titlebar(self) -> QWidget:  #vers 1
        """Custom frameless titlebar (standalone mode)."""
        bar = QFrame()
        bar.setFixedHeight(30)
        bar.setStyleSheet("background-color: #1a1a2e; border-radius: 4px;")
        lay = QHBoxLayout(bar)
        lay.setContentsMargins(8, 0, 4, 0)

        lbl = QLabel(f"⬡  {App_name}  {App_build_num}")
        lbl.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        lbl.setStyleSheet("color: #c0a0e0;")
        lay.addWidget(lbl)
        lay.addStretch()

        for sym, handler in [("─", self.showMinimized),
                              ("□", self._toggle_maximize),
                              ("✕", self.close)]:
            btn = QPushButton(sym)
            btn.setFixedSize(24, 22)
            btn.setFont(QFont("Arial", 9))
            btn.setStyleSheet("QPushButton{color:#aaa;border:none;background:transparent;}"
                              "QPushButton:hover{background:#3a2a4a;color:white;}")
            btn.clicked.connect(handler)
            lay.addWidget(btn)

        bar.mousePressEvent   = lambda e: self._title_mouse_press(e)
        bar.mouseMoveEvent    = lambda e: self._title_mouse_move(e)
        bar.mouseReleaseEvent = lambda e: self._title_mouse_release(e)
        return bar

    def _create_toolbar(self) -> QWidget:  #vers 1
        """Main toolbar."""
        bar = QFrame()
        bar.setFrameStyle(QFrame.Shape.StyledPanel)
        bar.setFixedHeight(42)
        lay = QHBoxLayout(bar)
        lay.setContentsMargins(4, 2, 4, 2)
        lay.setSpacing(3)

        ic = self.icon_factory
        icon_color = "#c0a0e0"

        def _btn(icon_fn, tip, handler, color=None):
            b = QPushButton()
            b.setFixedSize(34, 34)
            b.setFont(self.button_font)
            try:
                b.setIcon(icon_fn(color=color or icon_color))
                b.setIconSize(QSize(22, 22))
            except Exception:
                pass
            b.setToolTip(tip)
            b.clicked.connect(handler)
            lay.addWidget(b)
            return b

        self.open_btn   = _btn(ic.open_icon,    "Open DFF (Ctrl+O)",  self.open_dff_file)
        self.reload_btn = _btn(ic.reload_icon,  "Reload file",         self._reload_file)
        lay.addWidget(self._separator())
        self.export_btn = _btn(ic.package_icon, "Export…",            self._export_model)
        lay.addWidget(self._separator())
        self.wireframe_btn = _btn(ic.view_icon, "Toggle wireframe",    self._toggle_wireframe)
        self.fit_btn       = _btn(ic.zoom_fit_icon if hasattr(ic,'zoom_fit_icon') else ic.zoom_in_icon,
                                  "Fit to view", self._fit_view)
        lay.addStretch()

        # Build info label
        info = QLabel(f"{App_name}  {App_build_num}")
        info.setFont(QFont("Courier New", 8))
        info.setStyleSheet("color: #666; padding: 0 4px;")
        lay.addWidget(info)
        return bar

    def _separator(self) -> QFrame:
        sep = QFrame()
        sep.setFrameShape(QFrame.Shape.VLine)
        sep.setFixedWidth(2)
        sep.setStyleSheet("color: #444;")
        return sep

    def _create_left_panel(self) -> QWidget:  #vers 1
        """Left panel: list of geometries / meshes in the DFF."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(180)
        panel.setMaximumWidth(280)
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(3)

        hdr = QLabel("Geometries")
        hdr.setFont(self.title_font)
        lay.addWidget(hdr)

        self.geometry_list = QListWidget()
        self.geometry_list.setAlternatingRowColors(True)
        self.geometry_list.setStyleSheet("QListWidget{font-size:10px;}")
        self.geometry_list.currentRowChanged.connect(self._on_geometry_selected)
        lay.addWidget(self.geometry_list)

        # Frame tree label
        hdr2 = QLabel("Frames")
        hdr2.setFont(self.panel_font)
        lay.addWidget(hdr2)

        self.frame_list = QListWidget()
        self.frame_list.setMaximumHeight(120)
        self.frame_list.setStyleSheet("QListWidget{font-size:9px;}")
        lay.addWidget(self.frame_list)

        return panel

    def _create_viewport(self) -> QWidget:  #vers 1
        """Centre: 3D wireframe viewport."""
        container = QFrame()
        container.setFrameStyle(QFrame.Shape.StyledPanel)
        lay = QVBoxLayout(container)
        lay.setContentsMargins(0, 0, 0, 0)

        self.viewport = ModelViewport()
        lay.addWidget(self.viewport)

        # Viewport controls strip
        ctrl = QHBoxLayout()
        ctrl.setContentsMargins(4, 2, 4, 2)
        for label, handler in [("XY", lambda: self._set_view(0,0)),
                                ("XZ", lambda: self._set_view(0,90)),
                                ("YZ", lambda: self._set_view(90,0)),
                                ("Iso", lambda: self._set_view(45,30))]:
            b = QPushButton(label)
            b.setFixedSize(36, 22)
            b.setFont(QFont("Arial", 8))
            b.clicked.connect(handler)
            ctrl.addWidget(b)
        ctrl.addStretch()

        lbl = QLabel("Drag: rotate  |  Scroll: zoom")
        lbl.setFont(QFont("Arial", 8))
        lbl.setStyleSheet("color: #666;")
        ctrl.addWidget(lbl)
        lay.addLayout(ctrl)
        return container

    def _create_right_panel(self) -> QWidget:  #vers 1
        """Right panel: mesh info + material list."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(220)
        panel.setMaximumWidth(320)
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(4)

        # Info group
        info_grp = QGroupBox("Mesh Info")
        info_grp.setFont(self.panel_font)
        info_lay = QVBoxLayout(info_grp)
        info_lay.setSpacing(2)

        self.info_labels: Dict[str, QLabel] = {}
        for key in ["Name", "Vertices", "Triangles", "Materials",
                    "UV Layers", "Has Normals", "Has Colors"]:
            row = QHBoxLayout()
            lbl_k = QLabel(f"{key}:")
            lbl_k.setFont(QFont("Arial", 9, QFont.Weight.Bold))
            lbl_k.setFixedWidth(80)
            lbl_v = QLabel("—")
            lbl_v.setFont(QFont("Courier New", 9))
            self.info_labels[key] = lbl_v
            row.addWidget(lbl_k)
            row.addWidget(lbl_v)
            info_lay.addLayout(row)

        lay.addWidget(info_grp)

        # Material list
        mat_hdr = QLabel("Materials")
        mat_hdr.setFont(self.panel_font)
        lay.addWidget(mat_hdr)

        self.material_table = QTableWidget()
        self.material_table.setColumnCount(2)
        self.material_table.setHorizontalHeaderLabels(["Texture", "Color"])
        self.material_table.horizontalHeader().setStretchLastSection(True)
        self.material_table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.material_table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self.material_table.setAlternatingRowColors(True)
        self.material_table.setFont(QFont("Arial", 9))
        lay.addWidget(self.material_table)

        # Bounding sphere
        bs_grp = QGroupBox("Bounding Sphere")
        bs_grp.setFont(self.panel_font)
        bs_lay = QVBoxLayout(bs_grp)
        self.bs_label = QLabel("—")
        self.bs_label.setFont(QFont("Courier New", 8))
        self.bs_label.setWordWrap(True)
        bs_lay.addWidget(self.bs_label)
        lay.addWidget(bs_grp)

        lay.addStretch()
        return panel

    def _create_statusbar(self) -> QLabel:  #vers 1
        self._status_label = QLabel("Ready")
        self._status_label.setFont(QFont("Courier New", 9))
        self._status_label.setStyleSheet(
            "color: #aaa; padding: 2px 6px; border-top: 1px solid #333;")
        return self._status_label

    # ── File operations ──────────────────────────────────────────────────────

    def open_dff_file(self, file_path: str = None):  #vers 1
        """Open a DFF file and display its contents."""
        if not file_path:
            file_path, _ = QFileDialog.getOpenFileName(
                self, "Open DFF Model", "",
                "DFF Files (*.dff);;All Files (*)"
            )
        if not file_path:
            return

        if not DFF_AVAILABLE:
            QMessageBox.critical(self, "Error",
                "DFF parser not available.\n"
                "Check apps/methods/dff_parser.py is installed.")
            return

        try:
            model = load_dff(file_path)
            if model is None:
                QMessageBox.warning(self, "Invalid DFF",
                    "File does not appear to be a valid RenderWare DFF file.")
                return

            self.current_dff_path = file_path
            self.current_model    = model
            self._populate_ui(model)

            name = os.path.basename(file_path)
            self.setWindowTitle(f"{App_name}: {name} "
                                f"[{model.frame_count}F / {model.geometry_count}G]")
            self._set_status(f"Opened: {name}  —  "
                             f"{model.geometry_count} geometries, "
                             f"{model.frame_count} frames")

        except Exception as e:
            import traceback; traceback.print_exc()
            QMessageBox.critical(self, "Error", f"Failed to open DFF:\n{e}")

    def _reload_file(self):  #vers 1
        if self.current_dff_path:
            self.open_dff_file(self.current_dff_path)

    def _export_model(self):  #vers 1
        """Export placeholder — to be implemented."""
        QMessageBox.information(self, "Export",
            "Export functionality coming in a future build.\n\n"
            "Planned: OBJ export, DFF re-save.")

    # ── UI population ────────────────────────────────────────────────────────

    def _populate_ui(self, model):  #vers 1
        """Fill geometry list, frame list, and select first geometry."""
        self.geometry_list.clear()
        self.frame_list.clear()
        self.material_table.setRowCount(0)

        for i, geom in enumerate(model.geometries):
            atomic = next((a for a in model.atomics if a.geometry_index == i), None)
            frame_name = model.get_frame_name(atomic.frame_index) if atomic else f"geom_{i}"
            label = (f"[{i}]  {frame_name}  "
                     f"({geom.vertex_count}v / {geom.triangle_count}t)")
            item = QListWidgetItem(label)
            self.geometry_list.addItem(item)

        for i, frame in enumerate(model.frames):
            fname = frame.name or f"Frame {i}"
            parent_str = f" ← {frame.parent_index}" if frame.parent_index >= 0 else " (root)"
            self.frame_list.addItem(f"[{i}] {fname}{parent_str}")

        if model.geometries:
            self.geometry_list.setCurrentRow(0)

    def _on_geometry_selected(self, row: int):  #vers 1
        """Update viewport and info panel when a geometry is selected."""
        if not self.current_model or row < 0 or row >= len(self.current_model.geometries):
            return

        self.selected_geometry_index = row
        geom = self.current_model.geometries[row]

        # Update 3D viewport
        self.viewport.set_geometry(geom)

        # Update info labels
        has_n = "Yes" if geom.normals else "No"
        has_c = "Yes" if geom.colors  else "No"
        atomic = next((a for a in self.current_model.atomics
                       if a.geometry_index == row), None)
        name = (self.current_model.get_frame_name(atomic.frame_index)
                if atomic else f"geom_{row}")
        values = {
            "Name":       name,
            "Vertices":   str(geom.vertex_count),
            "Triangles":  str(geom.triangle_count),
            "Materials":  str(geom.material_count),
            "UV Layers":  str(len(geom.uv_layers)),
            "Has Normals": has_n,
            "Has Colors":  has_c,
        }
        for key, val in values.items():
            if key in self.info_labels:
                self.info_labels[key].setText(val)

        # Update bounding sphere
        bs = geom.bounding_sphere
        self.bs_label.setText(
            f"Centre: ({bs.center.x:.2f}, {bs.center.y:.2f}, {bs.center.z:.2f})\n"
            f"Radius: {bs.radius:.3f}")

        # Update material table
        self.material_table.setRowCount(0)
        for mat in geom.materials:
            r = self.material_table.rowCount()
            self.material_table.insertRow(r)
            tex_item = QTableWidgetItem(mat.texture_name or "(none)")
            col_item = QTableWidgetItem(
                f"#{mat.color.r:02X}{mat.color.g:02X}{mat.color.b:02X}")
            col_item.setBackground(QColor(mat.color.r, mat.color.g, mat.color.b))
            self.material_table.setItem(r, 0, tex_item)
            self.material_table.setItem(r, 1, col_item)

    # ── Viewport controls ────────────────────────────────────────────────────

    def _set_view(self, yaw: float, pitch: float):  #vers 1
        self.viewport._yaw   = yaw
        self.viewport._pitch = pitch
        self.viewport.update()

    def _fit_view(self):  #vers 1
        self.viewport._auto_fit()
        self.viewport.update()

    def _toggle_wireframe(self):  #vers 1
        # Placeholder — viewport always wireframe for now
        self._set_status("Wireframe mode (solid rendering coming in future build)")

    # ── Hotkeys ──────────────────────────────────────────────────────────────

    def _setup_hotkeys(self):  #vers 1
        from PyQt6.QtGui import QShortcut, QKeySequence
        QShortcut(QKeySequence("Ctrl+O"), self).activated.connect(self.open_dff_file)
        QShortcut(QKeySequence("Ctrl+R"), self).activated.connect(self._reload_file)
        QShortcut(QKeySequence("F"),      self).activated.connect(self._fit_view)

    # ── Theme ────────────────────────────────────────────────────────────────

    def _apply_theme(self):  #vers 1
        self.setStyleSheet("""
            QWidget { background-color: #252535; color: #e0d0f0; }
            QFrame  { border: 1px solid #3a3a5a; }
            QListWidget { background: #1e1e2e; border: 1px solid #3a3a5a; }
            QListWidget::item:selected { background: #4a2a6a; }
            QListWidget::item:alternate { background: #232333; }
            QPushButton {
                background: #3a2a5a; color: #e0d0f0;
                border: 1px solid #5a4a7a; border-radius: 3px;
                padding: 2px 6px;
            }
            QPushButton:hover  { background: #5a3a8a; }
            QPushButton:pressed { background: #2a1a4a; }
            QGroupBox { border: 1px solid #4a4a6a; border-radius: 4px;
                        margin-top: 8px; padding-top: 4px; }
            QGroupBox::title { subcontrol-origin: margin; left: 8px; color: #a080c0; }
            QTableWidget { background: #1e1e2e; gridline-color: #333; }
            QTableWidget::item:selected { background: #4a2a6a; }
            QHeaderView::section { background: #2a2a4a; border: none; padding: 2px; }
            QLabel  { border: none; }
            QSplitter::handle { background: #3a3a5a; }
        """)

    # ── Status ───────────────────────────────────────────────────────────────

    def _set_status(self, msg: str):  #vers 1
        if hasattr(self, '_status_label'):
            self._status_label.setText(msg)

    # ── Window drag (standalone frameless) ───────────────────────────────────

    def _toggle_maximize(self):  #vers 1
        if self.isMaximized(): self.showNormal()
        else: self.showMaximized()

    def _title_mouse_press(self, e):
        if e.button() == Qt.MouseButton.LeftButton:
            self.drag_position = e.globalPosition().toPoint() - self.frameGeometry().topLeft()

    def _title_mouse_move(self, e):
        if e.buttons() & Qt.MouseButton.LeftButton and self.drag_position:
            self.move(e.globalPosition().toPoint() - self.drag_position)

    def _title_mouse_release(self, e):
        self.drag_position = None

    # ── Close ────────────────────────────────────────────────────────────────

    def closeEvent(self, e):
        self.workshop_closed.emit()
        self.window_closed.emit()
        super().closeEvent(e)


# ── Alias for IMG Factory integration ─────────────────────────────────────────
ModelWorkshopDialog = ModelWorkshop


# ── Factory function (mirrors open_col_workshop) ──────────────────────────────

def open_model_workshop(main_window, dff_path: str = None):  #vers 1
    """Open Model Workshop — embedded in tab if main_window has tab widget, else standalone."""
    try:
        from PyQt6.QtWidgets import QVBoxLayout, QWidget
        from PyQt6.QtCore import Qt
        import os

        if not main_window or not hasattr(main_window, 'main_tab_widget'):
            # Standalone window
            workshop = ModelWorkshop(None, main_window)
            workshop.setWindowFlags(Qt.WindowType.Window)
            if dff_path:
                workshop.open_dff_file(dff_path)
            workshop.setWindowTitle(f"{App_name} — Standalone")
            workshop.resize(1300, 800)
            workshop.show()
            return workshop

        # Embedded tab
        tab_container = QWidget()
        tab_layout = QVBoxLayout(tab_container)
        tab_layout.setContentsMargins(0, 0, 0, 0)

        workshop = ModelWorkshop(tab_container, main_window)
        workshop.setWindowFlags(Qt.WindowType.Widget)
        tab_layout.addWidget(workshop)

        if dff_path:
            workshop.open_dff_file(dff_path)

        tab_label = os.path.splitext(os.path.basename(dff_path))[0] if dff_path else "Model Workshop"

        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            icon = SVGIconFactory().open_icon(color="#a080d0")
            idx = main_window.main_tab_widget.addTab(tab_container, icon, tab_label)
        except Exception:
            idx = main_window.main_tab_widget.addTab(tab_container, tab_label)

        main_window.main_tab_widget.setCurrentIndex(idx)
        workshop.show()
        return workshop

    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Model Workshop error: {e}")
        import traceback; traceback.print_exc()
        return None


# ── Standalone entry point ────────────────────────────────────────────────────
if __name__ == "__main__":
    import traceback

    print(f"{App_name} {App_build_num} — Starting")
    try:
        app = QApplication(sys.argv)
        workshop = ModelWorkshop()
        workshop.setWindowTitle(f"{App_name} — Standalone")
        workshop.show()
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)
