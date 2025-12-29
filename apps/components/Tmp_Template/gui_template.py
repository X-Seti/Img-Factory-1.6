#!/usr/bin/env python3
#this belongs in ~/Desktop/gui_template,.py - Version: 1
# X-Seti - December11 2025 - template - placeholder


"""
Gui dest, Information
"""

import os
import sys
from pathlib import Path
from typing import Optional, List, Dict, Tuple, Any
from PyQt6.QtWidgets import (
    QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QListWidget, QListWidgetItem, QLabel, QPushButton, QFileDialog, QMessageBox, QGroupBox, QFormLayout, QLineEdit, QTextEdit, QTableWidget, QTableWidgetItem, QHeaderView, QAbstractItemView, QMenu, QDialog, QCheckBox, QSpinBox, QDoubleSpinBox, QComboBox, QTabWidget, QScrollArea, QFrame,
    QInputDialog, QProgressDialog)


from PyQt6.QtCore import Qt, pyqtSignal, QSize, QPoint, QRect, QTimer
from PyQt6.QtGui import (
    QFont, QIcon, QPixmap, QColor, QPainter, QPen, QBrush, QAction, QCursor, QKeySequence, QPainterPath)

from depends.svg_icon_factory import SVGIconFactory
#from depends.img_debug_functions import img_debugger # TODO - Debugging is status window exists, otherwise the terminal will do.

# OpenGL for 3D viewport
try:
    from PyQt6.QtOpenGLWidgets import QOpenGLWidget
    from OpenGL.GL import *
    from OpenGL.GLU import *
    OPENGL_AVAILABLE = True
except ImportError:
    OPENGL_AVAILABLE = False
    print("Warning: OpenGL not available, 3D viewport disabled")

# Detect if running standalone or docked BEFORE any conditional imports
def _is_standalone():
    """Detect if running standalone (not imported by IMG Factory)"""
    import inspect
    frame = inspect.currentframe()
    try:
        # Check if we're being called from imgfactory.py
        for _ in range(10):  # Check up to 10 frames up
            frame = frame.f_back
            if frame is None:
                break
            if 'imgfactory' in frame.f_code.co_filename.lower():
                return False  # Docked mode
        return True  # Standalone mode
    finally:
        del frame

APPSETTINGS_AVAILABLE = None
STANDALONE_MODE = _is_standalone()
App_name = "Col Workshop"
App_build = "December 11 - "
App_auth = "X-Seti"


# Conditional imports based on mode
if STANDALONE_MODE:
    # STANDALONE MODE - Use local depends/ folder
    print("COL Workshop: Standalone mode detected")

    # Add current directory to path for depends imports
    current_dir = os.path.dirname(os.path.abspath(__file__))
    if current_dir not in sys.path:
        sys.path.insert(0, current_dir)

    try:
        from depends.col_core_classes import (
            COLFile, COLModel, COLVersion, COLMaterial, COLFaceGroup,
            COLSphere, COLBox, COLVertex, COLFace, Vector3, BoundingBox
        )
        from depends.col_dialogs import (
            show_col_analysis_dialog, show_col_batch_processor,
            show_col_file_dialog
        )
        from depends.svg_icon_factory import SVGIconFactory
        from depends.img_debug_functions import img_debugger
    except ImportError as e:
        print(f"Warning: Missing standalone dependencies: {e}")
        # Minimal fallbacks
        class SVGIconFactory:
            @staticmethod
            def _create_icon(svg_data, size=20, color=None):
                return None

        class img_debugger:
            @staticmethod
            def debug(msg): print(f"DEBUG: {msg}")
            @staticmethod
            def error(msg): print(f"ERROR: {msg}")
            @staticmethod
            def warning(msg): print(f"WARNING: {msg}")
            @staticmethod
            def success(msg): print(f"SUCCESS: {msg}")

        # Minimal COL classes
        class COLFile:
            def __init__(self): pass
        class COLModel:
            def __init__(self): pass

else:
    # DOCKED MODE - Use main app structure (apps.*)
    print("COL Workshop: Docked mode detected")

    try:
        from apps.methods.col_core_classes import (
            COLFile, COLModel, COLVersion, COLMaterial, COLFaceGroup,
            COLSphere, COLBox, COLVertex, COLFace, Vector3, BoundingBox
        )
        from apps.gui.col_dialogs import (
            show_col_analysis_dialog, show_col_batch_processor,
            show_col_file_dialog
        )
        from apps.methods.imgfactory_svg_icons import (
            get_save_icon, get_open_icon, get_refresh_icon,
            get_close_icon, get_add_icon, get_remove_icon,
            get_export_icon, get_import_icon, get_settings_icon,
            get_view_icon, get_edit_icon
        )
        from apps.debug.debug_functions import img_debugger
    except ImportError as e:
        print(f"Warning: Missing docked mode imports: {e}")
        # This shouldn't happen in docked mode, but provide fallback
        STANDALONE_MODE = True
        print("Falling back to standalone mode due to import failure")


# OpenGL placeholder (kept for layout compatibility)
OPENGL_AVAILABLE = False

## Method list.
#
#_create_transform_panel
#_create_toolbar
#_create_left_panel
#_create_middle_panel
#_create_right_panel
#_create_preview_controls
#_create_preview_widget
#_create_info_panel
#_create_status_bar
#_create_level_card
#_create_preview_widget
#_create_info_section
#_create_action_section
#_create_stats_grid
#_create_stat_box
#_create_merged_icons_line
#_create_viewport_controls

# Detect mode
def _is_standalone():
    return True

STANDALONE_MODE = _is_standalone()
App_name = "GUI Workshop" # change me
App_build = "December 11 - "
App_auth = "X-Seti"


# Fallback minimal classes for structure only
class GenericFile: pass

class GenericObject: pass

class COLVersion: COL_1 = None

class img_debugger:
    @staticmethod
    def debug(msg): pass
    @staticmethod
    def error(msg): pass
    @staticmethod
    def warning(msg): pass
    @staticmethod
    def success(msg): pass


# - GUI SHELL ONLY BELOW
class ObjListWidget(QListWidget):
    model_selected = pyqtSignal(int)
    model_context_menu = pyqtSignal(int, object)

    def __init__(self, parent=None): #ver 1
        super().__init__(parent)

        self.current_file = None

        # Enable context menu
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self.show_context_menu)

        # Connect selection
        self.currentRowChanged.connect(self.on_selection_changed)

    def on_selection_changed(self, row): #ver 1
        if row >= 0:
            self.model_selected.emit(row)

    def show_context_menu(self, position): #ver 1
        item = self.itemAt(position)
        if item:
            model_index = item.data(Qt.ItemDataRole.UserRole)
            self.model_context_menu.emit(model_index, self.mapToGlobal(position))


class GUIPropertiesWidget(QTabWidget): #ver 1
    property_changed = pyqtSignal(str, object)

    def __init__(self, parent=None): #ver 1
        super().__init__(parent)
        self.setup_tabs()

    def setup_tabs(self): #ver 1
        self.model_tab = QWidget()
        self.setup_model_tab()
        self.addTab(self.model_tab, "  Model") # TODO Missing SVG icon for model
        self.spheres_tab = QWidget()
        self.setup_spheres_tab()
        self.addTab(self.spheres_tab, "🔵 Spheres") # TODO Replace with SVG icon for Spheres
        self.boxes_tab = QWidget()
        self.setup_boxes_tab()
        self.addTab(self.boxes_tab, "  Boxes") # TODO Missing SVG icon for Boxes
        self.mesh_tab = QWidget()
        self.setup_mesh_tab()
        self.addTab(self.mesh_tab, "  Mesh") # TODO Missing SVG icon for Mesh
        self.vert_tab = QWidget()
        self.setup_vert_tab()
        self.addTab(self.vert_tab, "  Vertices") # TODO Missing SVG icon for Vertices

        # TODO other tabs for models functions.

    def setup_model_tab(self): #ver 1
        layout = QVBoxLayout(self.model_tab)
        info_group = QGroupBox("Model Information")
        info_layout = QFormLayout(info_group)

        self.model_name_edit = QLineEdit()
        self.model_name_edit.textChanged.connect(lambda text: self.property_changed.emit('name', text))
        info_layout.addRow("Name:", self.model_name_edit)
        self.model_version_combo = QComboBox()

        for version in [COLVersion.COL_1]:
            self.model_version_combo.addItem(f"COL{version}", version)

        info_layout.addRow("Version:", self.model_version_combo)
        self.model_id_spin = QSpinBox()
        self.model_id_spin.setRange(0, 65535)
        self.model_id_spin.valueChanged.connect(lambda value: self.property_changed.emit('model_id', value))

        info_layout.addRow("Object ID:", self.model_id_spin)
        layout.addWidget(info_group)

        bbox_group = QGroupBox("Bounding Box")
        bbox_layout = QFormLayout(bbox_group)
        center_layout = QHBoxLayout()

        self.center_x_spin = QDoubleSpinBox(); self.center_x_spin.setRange(-999999, 999999); self.center_x_spin.setDecimals(3)
        self.center_y_spin = QDoubleSpinBox(); self.center_y_spin.setRange(-999999, 999999); self.center_y_spin.setDecimals(3)
        self.center_z_spin = QDoubleSpinBox(); self.center_z_spin.setRange(-999999, 999999); self.center_z_spin.setDecimals(3)

        center_layout.addWidget(self.center_x_spin); center_layout.addWidget(self.center_y_spin); center_layout.addWidget(self.center_z_spin)
        bbox_layout.addRow("Center (X,Y,Z):", center_layout)

        self.radius_spin = QDoubleSpinBox(); self.radius_spin.setRange(0, 999999); self.radius_spin.setDecimals(3)
        bbox_layout.addRow("Radius:", self.radius_spin)
        layout.addWidget(bbox_group)
        layout.addStretch()

    def setup_spheres_tab(self): #ver 1
        pass

    def setup_boxes_tab(self): #ver 1
        pass

    def setup_mesh_tab(self): #ver 1
        pass

    def setup_vert_tab(self): #ver 1
        pass

    def setup_poly_tab(self): #ver 1
        pass


class App_Settings: # TODO Run app_settings_system.py
    #found in Apps/Utils/app_settings_system.py
    #theme aware logic here.
    pass

class RototePreview(QLabel): #Example
    pass

class PanPreview(QLabel): #Example
    pass

class ZoomablePreview(QLabel): #Example
    # Zoom in and out for right preview panel
    def __init__(self, parent=None): #ver 1
        super().__init__(parent)

        self.setMinimumSize(400, 400)
        self.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.setStyleSheet("border: 1px solid #3a3a3a;")
        self.setMouseTracking(True)
        self.zoom_level = 1.0
        #self.zoom_in()
        #self.zoom_out()
        self.pan_offset = QPoint(0, 0)
        self.bg_color = QColor(42, 42, 42)
        self.placeholder_text = "Select a model to preview"
        self.background_mode = 'solid'
        self._checkerboard_size = 16


    def setPixmap(self, pixmap): #ver 1
        self.update()


    def paintEvent(self, event): #ver 1
        painter = QPainter(self)
        painter.fillRect(self.rect(), self.bg_color)
        painter.setPen(QColor(150, 150, 150))
        painter.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, self.placeholder_text)


class GUIWorkshop(QWidget): #ver 1
    workshop_closed = pyqtSignal()
    window_closed = pyqtSignal()

    def __init__(self, parent=None, main_window=None): #ver 1
        super().__init__(parent)

        self.main_window = main_window
        self.undo_stack = []
        self.button_display_mode = 'both'
        self.standalone_mode = (main_window is None)
        self.is_docked = (main_window is not None)
        self.use_system_titlebar = False
        self.window_always_on_top = False
        self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        self.dragging = False
        self.drag_position = None
        self.resizing = False
        self.resize_corner = None
        self.corner_size = 20
        self.hover_corner = None
        self.setWindowTitle(App_name + ": No File")
        self.resize(1400, 800)
        self.setMouseTracking(True)
        self.dock_display_mode = None
        self.file_form = []

        # Get app_settings from main_window if available
        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        else:
            self.app_settings = None

        # Fonts
        self.setFont(QFont("Fira Sans Condensed", 14))
        self.title_font = QFont("Arial", 14)
        self.panel_font = QFont("Arial", 10)
        self.button_font = QFont("Arial", 10)
        self.infobar_font = QFont("Courier New", 9)

        # Spacing and Margins - MUST BE DEFINED BEFORE setup_ui()
        # TODO: new options for app_system_settings
        self.contmergina = 1
        self.contmerginb = 1
        self.contmerginc = 1
        self.contmergind = 1
        self.setspacing = 2

        # Panel spacing
        self.panelmergina = 5
        self.panelmerginb = 5
        self.panelmerginc = 5
        self.panelmergind = 5
        self.panelspacing = 5

        # TitleBar height - 45
        self.titlebarheight = 45
        self.toolbarheight = 50

        # Status Bar height - 22
        self.tabmerginsa = 5
        self.tabmerginsb = 0
        self.tabmerginsc = 5
        self.tabmerginsd = 0
        self.statusheight = 22

        # Tabbing Bar height - 22
        self.tabbarheight = 22
        self.tabbarspacing = 15

        # Icon only sizes - TODO need to be defined in Local settings and app_system_settings
        self.iconsizex = 64
        self.iconsizey = 64

        # Button icons + text size
        self.buticonsizex = 20
        self.buticonsizey = 20

        # Gadget icons + text size
        self.gadiconsizex = 20
        self.gadiconsizey = 20

        # Preview panel width - 40
        self.previframewidth = 40
        self.previframeheight = 40
        self.ctrlspacing = 10

        # Transform panel width - 50
        self.transframewidth = 50
        self.transframeheight = 50
        self.ctrispacing = 5
        self.ctrlmerginsa = 5
        self.ctrlmerginsb = 5
        self.ctrlmerginsc = 5
        self.ctrlmerginsd = 5

        self.setup_ui()
        self._setup_hotkeys()
        self._apply_theme()

        if hasattr(self, '_update_dock_button_visibility'):
            self._update_dock_button_visibility()


    def get_content_margins(self):
        """Returns main layout margins as a tuple (left, top, right, bottom)"""
        return (self.contmergina, self.contmerginb, self.contmerginc, self.contmergind)

    def get_panel_margins(self):
        """Returns panel layout margins as a tuple (left, top, right, bottom)"""
        return (self.panelmergina, self.panelmerginb, self.panelmerginc, self.panelmergind)

    def get_tab_margins(self):
        """Returns tab layout margins as a tuple (left, top, right, bottom)"""
        return (self.tabmerginsa, self.tabmerginsb, self.tabmerginsc, self.tabmerginsd)

    def get_ctrl_margins(self):
        """Returns control layout margins as a tuple (left, top, right, bottom)"""
        return (self.ctrlmerginsa, self.ctrlmerginsb, self.ctrlmerginsc, self.ctrlmerginsd)


        #Spacing and Mergins # TODO new options for app_system_settings
        self.contmergina = 1; self.contmerginb = 1; self.contmerginc = 1; self.contmergind = 1; self.setspacing = 2

        # Panel spacing.
        self.panelmergina = 5; self.panelmerginb = 5; self.panelmerginc = 5; self.panelmergind = 5; self.panelspacing = 5

        # TitleBar height - 45
        self.titlebarheight = 45; self.toolbarheight = 50

        # Status Bar height - 22
        self.tabmerginsa = 5; self.tabmerginsb = 0; self.tabmerginsc = 5; self.tabmerginsd = 0; self.statusheight = 22

        # Tabbing Bar height - 22
        self.tabbarheight = 22; self.tabbarspacing = 15

        # Icon only sizes = TODO need to be defined in Local settings and app_system_settings
        self.iconsizex = 64; self.iconsizey = 64

        # Button icons + text size
        self.buticonsizex = 20; self.buticonsizey = 20

        # Gadget icons + text size
        self.gadiconsizex = 20; self.gadiconsizey = 20

        # Preview panel width - 40
        self.previframewidth = 40; self.previframeheight = 40; self.ctrlspacing = 10

        # Transform panel width - 50
        self.transframewidth = 50; self.transframeheight = 50; self.ctrispacing = 5; self.ctrlmerginsa = 5; self.ctrlmerginsb = 5; self.ctrlmerginsc = 5; self.ctrlmerginsd = 5


    def setup_ui(self): #ver 1
        #Setup GUI panels.

        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(*self.get_content_margins())
        main_layout.setSpacing(self.setspacing)

        toolbar = self._create_toolbar()
        main_layout.addWidget(toolbar)

        main_splitter = QSplitter(Qt.Orientation.Horizontal)

        left_panel = self._create_left_panel()
        middle_panel = self._create_middle_panel()
        right_panel = self._create_right_panel()

        # Add panels to splitter based on mode
        if left_panel is not None:  # IMG Factory mode
            main_splitter.addWidget(left_panel)
            main_splitter.addWidget(middle_panel)
            main_splitter.addWidget(right_panel)

            # Set proportions (2:3:5)
            main_splitter.setStretchFactor(0, 2)
            main_splitter.setStretchFactor(1, 3)
            main_splitter.setStretchFactor(2, 5)

        else:  # Standalone mode
            main_splitter.addWidget(middle_panel)
            main_splitter.addWidget(right_panel)
            # Set proportions (1:1)

            main_splitter.setStretchFactor(0, 1)
            main_splitter.setStretchFactor(1, 1)

        main_layout.addWidget(main_splitter)

        # Status indicators if available
        if hasattr(self, '_setup_status_indicators'):
            status_frame = self._setup_status_indicators()
            main_layout.addWidget(status_frame)

        # Right panel - 3D viewer
        viewer_group = QGroupBox("3D Viewer")
        viewer_layout = QVBoxLayout(viewer_group)

        #if VIEWPORT_AVAILABLE:
        #    self.viewer_3d = COL3DViewport()
        #    viewer_layout.addWidget(self.viewer_3d)

        #    Add 3DS Max style controls at bottom
        #    controls = self._create_viewport_controls()
        #    viewer_layout.addWidget(controls)
        #else:
        #    self.viewer_3d = QLabel("3D Viewport unavailable\nInstall: pip install PyOpenGL")
        #    self.viewer_3d.setAlignment(Qt.AlignmentFlag.AlignCenter)
        #    viewer_layout.addWidget(self.viewer_3d)

    def _update_status_indicators(self): #vers 1
        pass


# - Panel Creation

    def _create_toolbar(self): #vers 1
        #Create toolbar - FIXED: Hide drag button when docked, ensure buttons visible
        from depends.svg_icon_factory import SVGIconFactory

        self.titlebar = QFrame()
        self.titlebar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.titlebar.setFixedHeight(self.titlebarheight)
        self.titlebar.setObjectName("titlebar")

        # Install event filter for drag detection
        self.titlebar.installEventFilter(self)
        self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.titlebar.setMouseTracking(True)

        self.layout = QHBoxLayout(self.titlebar)
        self.layout.setContentsMargins(*self.get_content_margins())
        self.layout.setSpacing(self.setspacing)

        # Get icon color from theme
        icon_color = self._get_icon_color()

        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setMaximumHeight(self.toolbarheight)

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(*self.get_panel_margins())
        layout.setSpacing(self.panelspacing)

        # Settings button
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(self._create_settings_icon())
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip("Workshop Settings")
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        # App title in center
        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(self.title_label)

        layout.addStretch()
        #layout.addStretch()

        # Only show "Open IMG" button if NOT standalone
        if not self.standalone_mode:
            self.open_img_btn = QPushButton("OpenIMG")
            self.open_img_btn.setFont(self.button_font)
            self.open_img_btn.setIcon(self._create_folder_icon())
            self.open_img_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
            self.open_img_btn.clicked.connect(self.open_img_archive)
            layout.addWidget(self.open_img_btn)

        # Open button
        self.open_btn = QPushButton()
        self.open_btn.setFont(self.button_font)
        self.open_btn.setIcon(self._create_open_icon())
        self.open_btn.setText("Open")
        self.open_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        self.open_btn.setShortcut("Ctrl+O")
        if self.button_display_mode == 'icons':
            self.open_btn.setFixedSize(self.iconsizex, self.iconsizey)

        self.open_btn.setToolTip("Open COL file (Ctrl+O)")
        self.open_btn.clicked.connect(self._open_file)
        layout.addWidget(self.open_btn)

        # Save button
        self.save_btn = QPushButton()
        self.save_btn.setFont(self.button_font)
        self.save_btn.setIcon(self._create_save_icon())
        self.save_btn.setText("Save")
        self.save_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        self.save_btn.setShortcut("Ctrl+S")
        if self.button_display_mode == 'icons':
            self.save_btn.setFixedSize(self.iconsizex, self.iconsizey)

        self.save_btn.setEnabled(False)  # Enable when modified
        self.save_btn.setToolTip("Save COL file (Ctrl+S)")
        self.save_btn.clicked.connect(self._save_file)
        layout.addWidget(self.save_btn)

        # Save button
        self.saveall_btn = QPushButton()
        self.saveall_btn.setFont(self.button_font)
        self.saveall_btn.setIcon(self._create_saveas_icon())
        self.saveall_btn.setText("Save All")
        self.saveall_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        self.saveall_btn.setShortcut("Ctrl+S")
        if self.button_display_mode == 'icons':
            self.saveall_btn.setFixedSize(self.iconsizex, self.iconsizey)

        self.saveall_btn.setEnabled(False)  # Enable when modified
        self.saveall_btn.setToolTip("Save COL file (Ctrl+S)")
        #self.saveall_btn.clicked.connect(self._saveall_file)
        #layout.addWidget(self.saveall_btn)

        self.export_all_btn = QPushButton("Extract")
        self.export_all_btn.setFont(self.button_font)
        self.export_all_btn.setIcon(self._create_package_icon())
        self.export_all_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.export_all_btn.setFixedSize(self.iconsizex, self.iconsizey)

        self.export_all_btn.setToolTip("Export all as ojs files")
        #self.export_all_btn.clicked.connect(self.export_all)
        self.export_all_btn.setEnabled(False)
        layout.addWidget(self.export_all_btn)

        self.undo_btn = QPushButton()
        self.undo_btn.setFont(self.button_font)
        self.undo_btn.setIcon(self._create_undo_icon())
        self.undo_btn.setText("Undo")
        self.undo_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.undo_btn.setFixedSize(self.iconsizex, self.iconsizey)

        #self.undo_btn.clicked.connect(self._undo_last_action)
        self.undo_btn.setEnabled(False)
        self.undo_btn.setToolTip("Undo last change")
        layout.addWidget(self.undo_btn)

        # Info button
        self.info_btn = QPushButton("")
        self.info_btn.setText("")  # CHANGED from "Info"
        self.info_btn.setIcon(self._create_info_icon())
        self.info_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        #self.info_btn.setMinimumHeight(30)
        self.info_btn.setToolTip("Information")
        self.info_btn.setStyleSheet("""
            QPushButton {
                font-weight: bold;
                background-color: #4a4a4a;
                border: 1px solid #5a5a5a;
                border-radius: 3px;
            }
            QPushButton:hover {
                background-color: #5a5a5a;
            }
        """)
        self.info_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.info_btn.setFixedSize(self.iconsizex, self.iconsizey)

        #self.info_btn.clicked.connect(self._show_ojb_info)
        layout.addWidget(self.info_btn)

        # Properties/Theme button
        self.properties_btn = QPushButton()
        self.properties_btn.setFont(self.button_font)
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(self.buticonsizex, icon_color))
        self.properties_btn.setToolTip("Theme")
        self.properties_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.properties_btn.setFixedSize(self.iconsizex, self.iconsizey)

        self.properties_btn.clicked.connect(self._show_settings_dialog)
        self.properties_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.properties_btn.customContextMenuRequested.connect(self._show_settings_context_menu)
        layout.addWidget(self.properties_btn)

        # Dock button [D]
        self.dock_btn = QPushButton("D") #TODO needs to be a SVG icon
        #self.dock_btn.setFont(self.button_font)
        self.dock_btn.setMinimumWidth(self.gadiconsizex)
        self.dock_btn.setMaximumWidth(self.gadiconsizey)
        if self.dock_display_mode == 'icons':
            self.dock_btn.setFixedSize(self.iconsizex, self.iconsizey)

        self.dock_btn.setToolTip("Dock")
        self.dock_btn.setStyleSheet("""
            QPushButton {
                font-weight: bold;
                background-color: #4a4a4a;
                border: 1px solid #5a5a5a;
                border-radius: 3px;
            }
            QPushButton:hover {
                background-color: #5a5a5a;
            }
        """)
        self.dock_btn.clicked.connect(self.toggle_dock_mode)
        layout.addWidget(self.dock_btn)

                # Tear-off button [T] - only in IMG Factory mode
        if not self.standalone_mode:
            self.tearoff_btn = QPushButton("T") #TODO needs to be a SVG icon
            #self.tearoff_btn.setFont(self.button_font)
            self.tearoff_btn.setMinimumWidth(self.gadiconsizex)
            self.tearoff_btn.setMaximumWidth(self.gadiconsizey)
            self.tearoff_btn.clicked.connect(self._toggle_tearoff)
            self.tearoff_btn.setToolTip("TXD Workshop - Tearoff window")
            self.tearoff_btn.setStyleSheet("""
                QPushButton {
                    font-weight: bold;
                    background-color: #4a4a4a;
                    border: 1px solid #5a5a5a;
                    border-radius: 3px;
                }
                QPushButton:hover {
                    background-color: #5a5a5a;
                }
            """)
            layout.addWidget(self.tearoff_btn)

        # Window controls
        self.minimize_btn = QPushButton()
        self.minimize_btn.setIcon(self._create_minimize_icon())
        self.minimize_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.minimize_btn.setFixedSize(self.iconsizex, self.iconsizey)

        self.minimize_btn.setMinimumWidth(self.gadiconsizex)
        self.minimize_btn.setMaximumWidth(self.gadiconsizey)
        self.minimize_btn.clicked.connect(self.showMinimized)
        self.minimize_btn.setToolTip("Minimize Window") # click tab to restore
        layout.addWidget(self.minimize_btn)

        self.maximize_btn = QPushButton()
        self.maximize_btn.setIcon(self._create_maximize_icon())
        self.maximize_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.maximize_btn.setFixedSize(self.iconsizex, self.iconsizey)

        self.maximize_btn.setMinimumWidth(self.gadiconsizex)
        self.maximize_btn.setMaximumWidth(self.gadiconsizey)
        self.maximize_btn.clicked.connect(self._toggle_maximize)
        self.maximize_btn.setToolTip("Maximize/Restore Window")
        layout.addWidget(self.maximize_btn)

        self.close_btn = QPushButton()
        self.close_btn.setIcon(self._create_close_icon())
        self.close_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.close_btn.setFixedSize(self.iconsizex, self.iconsizey)

        self.close_btn.setMinimumWidth(self.gadiconsizex)
        self.close_btn.setMaximumWidth(self.gadiconsizey)
        self.close_btn.clicked.connect(self.close)
        self.close_btn.setToolTip("Close Window") # closes tab
        layout.addWidget(self.close_btn)

        return self.toolbar


    def _create_left_panel(self): #vers 5
        # Create left panel - COL file list (only in IMG Factory mode)
        # In standalone mode, don't create this panel
        if self.standalone_mode:
            self.col_list_widget = None  # Explicitly set to None
            return None

        if not self.main_window:
            # Standalone mode - return None to hide this panel
            return None

        # Only create panel in IMG Factory mode
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(200)
        panel.setMaximumWidth(300)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(*self.get_panel_margins())

        header = QLabel("Ojs Files")
        header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        layout.addWidget(header)

        self.col_list_widget = QListWidget()
        self.col_list_widget.setAlternatingRowColors(True)
        self.col_list_widget.itemClicked.connect(self._on_col_selected)
        layout.addWidget(self.col_list_widget)
        return panel

    def _create_middle_panel(self): #ver 1

        panel = QGroupBox("Obj Objects")
        panel.setStyleSheet("""
            QGroupBox {
                font-weight: bold;
                font-size: 14px;
                border: 1px solid #3a3a3a;
                border-radius: 1px;
                margin-top: 10px;
                padding-top: 10px;
                background-color: #2b2b2b;
            }
            QGroupBox::title {
                subcontrol-origin: margin;
                subcontrol-position: top right;
                right: 20px;
                padding: 0 5px;
                color: #e0e0e0;
            }
        """)
        layout = QVBoxLayout(panel)
        layout.setSpacing(self.panelspacing)
        self.middle_list = QTableWidget()
        self.middle_list.setColumnCount(2)
        self.middle_list.setHorizontalHeaderLabels(["Preview", "Details"])
        self.middle_list.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.middle_list.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        self.middle_list.setAlternatingRowColors(True)
        self.middle_list.setIconSize(QSize(self.iconsizex, self.iconsizey))
        self.middle_list.setColumnWidth(0, 80)
        self.middle_list.horizontalHeader().setStretchLastSection(True)
        layout.addWidget(self.middle_list)

        return panel

    def _create_right_panel(self): #vers 10
        #Create right panel with editing controls - compact layout
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(400)
        has_bumpmap = False
        main_layout = QVBoxLayout(panel)
        main_layout.setContentsMargins(*self.get_panel_margins())
        top_layout = QHBoxLayout()

        # Transform panel (left)
        transform_panel = self._create_transform_panel()
        top_layout.addWidget(transform_panel, stretch=1)

        # Preview area (center) - 3D Viewport
        top_layout = QHBoxLayout()
        top_label = QLabel("Display:")
        top_label.setFont(self.panel_font)
        top_layout.addWidget(top_label, stretch=2)

        # Preview controls (right side, vertical)

        self.preview_controls = self._create_preview_controls()
        top_layout.addWidget(self.preview_controls, stretch=0)
        main_layout.addLayout(top_layout, stretch=1)

        # Information group below
        info_group = QGroupBox("")
        info_group.setFont(self.title_font)
        info_layout = QVBoxLayout(info_group)
        info_group.setMaximumHeight(140)

        # === LINE 1: collision name ===
        name_layout = QHBoxLayout()
        name_label = QLabel("Obj Name:")
        name_label.setFont(self.panel_font)
        name_layout.addWidget(name_label)

        self.info_name = QLineEdit()
        self.info_name.setText("Click to edit...")
        self.info_name.setFont(self.panel_font)
        self.info_name.setReadOnly(True)
        self.info_name.setStyleSheet("padding: px; border: 1px solid #3a3a3a;")
        #self.info_name.returnPressed.connect(self._save_surface_name)
        #self.info_name.editingFinished.connect(self._save_surface_name)
        self.info_name.mousePressEvent = lambda e: self._enable_name_edit(e, False)
        name_layout.addWidget(self.info_name, stretch=1)
        info_layout.addLayout(name_layout)

        # === LINES 2 & 3: Adaptive based on display mode ===
        if self.button_display_mode == 'icons':
            # MERGED: Single compact line for icon mode
            merged_line = self._create_merged_icons_line()
            info_layout.addLayout(merged_line)
        else:
            # SEPARATE: Original two-line layout for text/both modes
            # Line 2: Format controls
            format_layout = QHBoxLayout()
            format_layout.setSpacing(self.panelspacing)

            self.format_combo = QComboBox()
            self.format_combo.setFont(self.panel_font)
            self.format_combo.addItems(["ojb", "ojb2", "obj3", "obj4"])
            #self.format_combo.currentTextChanged.connect(self._change_format)
            self.format_combo.setEnabled(False)
            self.format_combo.setMaximumWidth(100)
            format_layout.addWidget(self.format_combo)

            format_layout.addStretch()

            # Switch button
            self.switch_btn = QPushButton("Switch")
            self.switch_btn.setFont(self.button_font)
            self.switch_btn.setIcon(self._create_flip_vert_icon())
            self.switch_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
            #self.switch_btn.clicked.connect(self.switch_surface_view)
            self.switch_btn.setEnabled(False)
            self.switch_btn.setToolTip("Cycle: Wireframe → Mesh → Painted → Overlay")
            format_layout.addWidget(self.switch_btn)

            # Convert
            self.convert_btn = QPushButton("Convert")
            self.convert_btn.setFont(self.button_font)
            self.convert_btn.setIcon(self._create_convert_icon())
            self.convert_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
            self.convert_btn.setToolTip("Convert Collision format")
            #self.convert_btn.clicked.connect(self._convert_surface)
            self.convert_btn.setEnabled(False)
            format_layout.addWidget(self.convert_btn)

            # Line 3: shadow + Bumpmaps
            mipbump_layout = QHBoxLayout()
            mipbump_layout.setSpacing(self.panelspacing)

            self.info_format = QLabel("Example: ")
            self.info_format.setFont(self.panel_font)
            self.info_format.setMinimumWidth(100)
            mipbump_layout.addWidget(self.info_format)

            self.show_shadow_btn = QPushButton("View")
            self.show_shadow_btn.setFont(self.button_font)
            self.show_shadow_btn.setIcon(self._create_view_icon())
            self.show_shadow_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
            self.show_shadow_btn.setToolTip("View all levels")
            #self.show_shadow_btn.clicked.connect(self._open_mipmap_manager)
            self.show_shadow_btn.setEnabled(False)
            mipbump_layout.addWidget(self.show_shadow_btn)

            self.create_shadow_btn = QPushButton("Create")
            self.create_shadow_btn.setFont(self.button_font)
            self.create_shadow_btn.setIcon(self._create_add_icon())
            self.create_shadow_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
            self.create_shadow_btn.setToolTip("Generate Shadow Mesh")
            #self.create_shadow_btn.clicked.connect(self._create_shadow_dialog)
            self.create_shadow_btn.setEnabled(False)
            mipbump_layout.addWidget(self.create_shadow_btn)

            self.remove_shadow_btn = QPushButton("Remove")
            self.remove_shadow_btn.setFont(self.button_font)
            self.remove_shadow_btn.setIcon(self._create_delete_icon())
            self.remove_shadow_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
            self.remove_shadow_btn.setToolTip("Remove Shodow Mesh")
            #self.remove_shadow_btn.clicked.connect(self._remove_shadow)
            self.remove_shadow_btn.setEnabled(False)
            mipbump_layout.addWidget(self.remove_shadow_btn)

            mipbump_layout.addSpacing(self.panelspacing)
            view_layout = QHBoxLayout()

            self.compress_btn = QPushButton("Compress")
            self.compress_btn.setFont(self.button_font)
            self.compress_btn.setIcon(self._create_compress_icon())
            self.compress_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
            self.compress_btn.setToolTip("Compress Collision")
            #self.compress_btn.clicked.connect(self._compress_surface)
            self.compress_btn.setEnabled(False)
            format_layout.addWidget(self.compress_btn)

            self.uncompress_btn = QPushButton("Uncompress")
            self.uncompress_btn.setFont(self.button_font)
            self.uncompress_btn.setIcon(self._create_uncompress_icon())
            self.uncompress_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
            self.uncompress_btn.setToolTip("Uncompress Collision")
            #self.uncompress_btn.clicked.connect(self._uncompress_surface)
            self.uncompress_btn.setEnabled(False)
            format_layout.addWidget(self.uncompress_btn)

            self.import_btn = QPushButton("Import")
            self.import_btn.setFont(self.button_font)
            self.import_btn.setIcon(self._create_import_icon())
            self.import_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
            self.import_btn.setToolTip("Import col, cst, 3ds files")
            #self.import_btn.clicked.connect(self._import_selected)
            self.import_btn.setEnabled(False)
            format_layout.addWidget(self.import_btn)

            self.export_btn = QPushButton("Export")
            self.export_btn.setFont(self.button_font)
            self.export_btn.setIcon(self._create_export_icon())
            self.export_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
            self.export_btn.setToolTip("Export col, cst, 3ds files")
            #self.export_btn.clicked.connect(self.export_selected)
            self.export_btn.setEnabled(False)
            format_layout.addWidget(self.export_btn)

            info_layout.addLayout(format_layout)
            info_layout.addLayout(view_layout)
            info_layout.addLayout(mipbump_layout)

        main_layout.addWidget(info_group, stretch=0)
        return panel


    def _create_transform_panel(self): #vers 1
        #Create transform panel with variable width - no headers
        self.transform_panel = QFrame()
        self.transform_panel.setFrameStyle(QFrame.Shape.StyledPanel)
        self.transform_panel.setMinimumWidth(30)
        self.transform_panel.setMaximumWidth(200)
        if self.button_display_mode == 'icons':
            self.transform_panel.setMaximumWidth(self.transframewidth)
            self.transform_panel.setMinimumWidth(self.transframewidth)
            layout.addSpacing(self.ctrispacing)

        layout = QVBoxLayout(self.transform_panel)
        layout.setContentsMargins(*self.get_ctrl_margins())
        layout.setSpacing(self.ctrispacing)

        # Flip Vertical
        self.flip_vert_btn = QPushButton()
        self.flip_vert_btn.setFont(self.button_font)
        self.flip_vert_btn.setIcon(self._create_flip_vert_icon())
        self.flip_vert_btn.setText("Flip Vertical")
        self.flip_vert_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.flip_vert_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.flip_vert_btn.clicked.connect(self._flip_vertical)
        self.flip_vert_btn.setEnabled(False)
        self.flip_vert_btn.setToolTip("Flip col vertically")
        layout.addWidget(self.flip_vert_btn)

        # Flip Horizontal
        self.flip_horz_btn = QPushButton()
        self.flip_horz_btn.setFont(self.button_font)
        self.flip_horz_btn.setIcon(self._create_flip_horz_icon())
        self.flip_horz_btn.setText("Flip Horizontal")
        self.flip_horz_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.flip_horz_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.flip_horz_btn.clicked.connect(self._flip_horizontal)
        self.flip_horz_btn.setEnabled(False)
        self.flip_horz_btn.setToolTip("Flip col horizontally")
        layout.addWidget(self.flip_horz_btn)

        layout.addSpacing(self.ctrispacing)

        # Rotate Clockwise
        self.rotate_cw_btn = QPushButton()
        self.rotate_cw_btn.setFont(self.button_font)
        self.rotate_cw_btn.setIcon(self._create_rotate_cw_icon())
        self.rotate_cw_btn.setText("Rotate 90° CW")
        self.rotate_cw_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.rotate_cw_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.rotate_cw_btn.clicked.connect(self._rotate_clockwise)
        self.rotate_cw_btn.setEnabled(False)
        self.rotate_cw_btn.setToolTip("Rotate 90 degrees clockwise")
        layout.addWidget(self.rotate_cw_btn)

        # Rotate Counter-Clockwise
        self.rotate_ccw_btn = QPushButton()
        self.rotate_ccw_btn.setFont(self.button_font)
        self.rotate_ccw_btn.setIcon(self._create_rotate_ccw_icon())
        self.rotate_ccw_btn.setText("Rotate 90° CCW")
        self.rotate_ccw_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.rotate_ccw_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.rotate_ccw_btn.clicked.connect(self._rotate_counterclockwise)
        self.rotate_ccw_btn.setEnabled(False)
        self.rotate_ccw_btn.setToolTip("Rotate 90 degrees counter-clockwise")
        layout.addWidget(self.rotate_ccw_btn)

        layout.addSpacing(self.ctrispacing)

        # Analyze button
        self.analyze_btn = QPushButton()
        self.analyze_btn.setFont(self.button_font)
        self.analyze_btn.setIcon(self._create_analyze_icon())
        self.analyze_btn.setText("Analyze")
        self.analyze_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.analyze_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.analyze_btn.clicked.connect(self._analyze_collision)
        self.analyze_btn.setEnabled(False)  # Enable when COL loaded
        self.analyze_btn.setToolTip("Analyze collision data")
        layout.addWidget(self.analyze_btn)

        layout.addStretch()

        # Copy
        self.copy_btn = QPushButton()
        self.copy_btn.setFont(self.button_font)
        self.copy_btn.setIcon(self._create_copy_icon())
        self.copy_btn.setText("Copy")
        self.copy_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.copy_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.copy_btn.clicked.connect(self._copy_surface)
        self.copy_btn.setEnabled(False)
        self.copy_btn.setToolTip("Copy col to clipboard")
        layout.addWidget(self.copy_btn)

        # Paste
        self.paste_btn = QPushButton()
        self.paste_btn.setFont(self.button_font)
        self.paste_btn.setIcon(self._create_paste_icon())
        self.paste_btn.setText("Paste")
        self.paste_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.paste_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.paste_btn.clicked.connect(self._paste_surface)
        self.paste_btn.setEnabled(False)
        self.paste_btn.setToolTip("Paste col from clipboard")
        layout.addWidget(self.paste_btn)

        # Create Collision
        self.create_surface_btn = QPushButton()
        self.create_surface_btn.setFont(self.button_font)
        self.create_surface_btn.setIcon(self._create_create_icon())
        self.create_surface_btn.setText("Create")
        self.create_surface_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.create_surface_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.create_surface_btn.clicked.connect(self._create_new_surface_entry)
        self.create_surface_btn.setToolTip("Create new blank Collision")
        layout.addWidget(self.create_surface_btn)

        # Delete collision
        self.delete_surface_btn = QPushButton()
        self.delete_surface_btn.setFont(self.button_font)
        self.delete_surface_btn.setIcon(self._create_delete_icon())
        self.delete_surface_btn.setText("Delete")
        self.delete_surface_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.delete_surface_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.delete_surface_btn.clicked.connect(self._delete_surface)
        self.delete_surface_btn.setEnabled(False)
        self.delete_surface_btn.setToolTip("Remove selected Collision")
        layout.addWidget(self.delete_surface_btn)

        # Duplicate Collision
        self.duplicate_surface_btn = QPushButton()
        self.duplicate_surface_btn.setFont(self.button_font)
        self.duplicate_surface_btn.setIcon(self._create_duplicate_icon())
        self.duplicate_surface_btn.setText("Duplicate")
        self.duplicate_surface_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.duplicate_surface_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.duplicate_surface_btn.clicked.connect(self._duplicate_surface)
        self.duplicate_surface_btn.setEnabled(False)
        self.duplicate_surface_btn.setToolTip("Clone selected Collision")
        layout.addWidget(self.duplicate_surface_btn)

        self.paint_btn = QPushButton()
        self.paint_btn.setFont(self.button_font)
        self.paint_btn.setIcon(self._create_paint_icon())
        self.paint_btn.setText("Paint")
        self.paint_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.paint_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.paint_btn.clicked.connect(self._open_paint_editor)
        self.paint_btn.setEnabled(False)
        self.paint_btn.setToolTip("Paint free hand on surface")
        layout.addWidget(self.paint_btn)

        layout.addSpacing(self.ctrispacing)

        # Check col vs DFF
        self.surface_type_btn = QPushButton()
        self.surface_type_btn.setFont(self.button_font)
        self.surface_type_btn.setIcon(self._create_check_icon())
        self.surface_type_btn.setText("Surface type")
        self.surface_type_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.surface_type_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.surface_type_btn.clicked.connect(self._check_col_type)
        self.surface_type_btn.setToolTip("Surface types")
        layout.addWidget(self.surface_type_btn)

        # Check col vs DFF
        self.surface_edit_btn = QPushButton()
        self.surface_edit_btn.setFont(self.button_font)
        self.surface_edit_btn.setIcon(self._create_check_icon())
        self.surface_edit_btn.setText("Surface Edit")
        self.surface_edit_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.surface_edit_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.surface_type_btn.clicked.connect(self._col_edit)
        self.surface_edit_btn.setToolTip("Surface Editor")
        layout.addWidget(self.surface_edit_btn)

        self.build_from_txd_btn = QPushButton()
        self.build_from_txd_btn.setFont(self.button_font)
        self.build_from_txd_btn.setIcon(self._create_build_icon())
        self.build_from_txd_btn.setText("Build col via")
        self.build_from_txd_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        if self.button_display_mode == 'icons':
            self.build_from_txd_btn.setIconSize(QSize(self.iconsizex, self.iconsizey))
        #self.build_from_dff_btn.clicked.connect(self._build_col_from_dff)
        self.build_from_txd_btn.setToolTip("Create col surface from txd texture names")
        layout.addWidget(self.build_from_txd_btn)

        layout.addSpacing(self.ctrispacing)

        layout.addStretch()

        return self.transform_panel


    def _create_preview_controls(self): #vers 5
        """Create preview control buttons - vertical layout on right"""
        controls_frame = QFrame()
        controls_frame.setFrameStyle(QFrame.Shape.StyledPanel)
        controls_frame.setMaximumWidth(50)
        controls_layout = QVBoxLayout(controls_frame)
        controls_layout.setContentsMargins(*self.get_ctrl_margins())
        controls_layout.setSpacing(self.panelspacing)

        # Check if using 3D viewport or 2D preview
        #is_3d_viewport = VIEWPORT_AVAILABLE and isinstance(self.preview_widget, COL3DViewport)

        # Zoom In
        zoom_in_btn = QPushButton()
        zoom_in_btn.setIcon(self._create_zoom_in_icon())
        zoom_in_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        zoom_in_btn.setFixedSize(self.previframewidth, self.previframeheight)
        zoom_in_btn.setToolTip("Zoom In")
        #zoom_in_btn.clicked.connect(self.preview_widget.zoom_in)
        controls_layout.addWidget(zoom_in_btn)

        # Zoom Out
        zoom_out_btn = QPushButton()
        zoom_out_btn.setIcon(self._create_zoom_out_icon())
        zoom_out_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        zoom_out_btn.setFixedSize(self.previframewidth, self.previframeheight)
        zoom_out_btn.setToolTip("Zoom Out")
        #zoom_out_btn.clicked.connect(self.preview_widget.zoom_out)
        controls_layout.addWidget(zoom_out_btn)

        # Reset
        reset_btn = QPushButton()
        reset_btn.setIcon(self._create_reset_icon())
        reset_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        reset_btn.setFixedSize(self.previframewidth, self.previframeheight)
        reset_btn.setToolTip("Reset View")
        #reset_btn.clicked.connect(self.preview_widget.reset_view)
        controls_layout.addWidget(reset_btn)

        # Fit
        fit_btn = QPushButton()
        fit_btn.setIcon(self._create_fit_icon())
        fit_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        fit_btn.setFixedSize(self.previframewidth, self.previframeheight)
        fit_btn.setToolTip("Fit to Window")
        #fit_btn.clicked.connect(self.preview_widget.fit_to_window)
        controls_layout.addWidget(fit_btn)

        controls_layout.addSpacing(self.panelspacing)

        # Pan Up
        pan_up_btn = QPushButton()
        pan_up_btn.setIcon(self._create_arrow_up_icon())
        pan_up_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        pan_up_btn.setFixedSize(self.previframewidth, self.previframeheight)
        pan_up_btn.setToolTip("Pan Up")
        #pan_up_btn.clicked.connect(lambda: self._pan_preview(0, -20))
        controls_layout.addWidget(pan_up_btn)

        # Pan Down
        pan_down_btn = QPushButton()
        pan_down_btn.setIcon(self._create_arrow_down_icon())
        pan_down_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        pan_down_btn.setFixedSize(self.previframewidth, self.previframeheight)
        pan_down_btn.setToolTip("Pan Down")
        #pan_down_btn.clicked.connect(lambda: self._pan_preview(0, 20))
        controls_layout.addWidget(pan_down_btn)

        # Pan Left
        pan_left_btn = QPushButton()
        pan_left_btn.setIcon(self._create_arrow_left_icon())
        pan_left_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        pan_left_btn.setFixedSize(self.previframewidth, self.previframeheight)
        pan_left_btn.setToolTip("Pan Left")
        #pan_left_btn.clicked.connect(lambda: self._pan_preview(-20, 0))
        controls_layout.addWidget(pan_left_btn)

        # Pan Right
        pan_right_btn = QPushButton()
        pan_right_btn.setIcon(self._create_arrow_right_icon())
        pan_right_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        pan_right_btn.setFixedSize(self.previframewidth, self.previframeheight)
        pan_right_btn.setToolTip("Pan Right")
        #pan_right_btn.clicked.connect(lambda: self._pan_preview(20, 0))
        controls_layout.addWidget(pan_right_btn)

        # Color picker
        bg_custom_btn = QPushButton()
        bg_custom_btn.setIcon(self._create_color_picker_icon())
        bg_custom_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        bg_custom_btn.setFixedSize(self.previframewidth, self.previframeheight)
        bg_custom_btn.setToolTip("Pick Background Color")
        #bg_custom_btn.clicked.connect(self._pick_background_color)
        controls_layout.addWidget(bg_custom_btn)

        controls_layout.addSpacing(self.panelspacing)

        # View Spheres toggle
        self.view_spheres_btn = QPushButton()
        self.view_spheres_btn.setIcon(self._create_sphere_icon())
        self.view_spheres_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        self.view_spheres_btn.setFixedSize(self.previframewidth, self.previframeheight)
        self.view_spheres_btn.setCheckable(True)
        self.view_spheres_btn.setChecked(True)
        self.view_spheres_btn.setToolTip("Toggle Spheres")
        #self.view_spheres_btn.clicked.connect(self._toggle_spheres)
        controls_layout.addWidget(self.view_spheres_btn)

        # View Boxes toggle
        self.view_boxes_btn = QPushButton()
        self.view_boxes_btn.setIcon(self._create_box_icon())
        self.view_boxes_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        self.view_boxes_btn.setFixedSize(self.previframewidth, self.previframeheight)
        self.view_boxes_btn.setCheckable(True)
        self.view_boxes_btn.setChecked(True)
        self.view_boxes_btn.setToolTip("Toggle Boxes")
        #self.view_boxes_btn.clicked.connect(self._toggle_boxes)
        controls_layout.addWidget(self.view_boxes_btn)

        # View Mesh toggle
        self.view_mesh_btn = QPushButton()
        self.view_mesh_btn.setIcon(self._create_mesh_icon())
        self.view_mesh_btn.setIconSize(QSize(self.buticonsizex, self.buticonsizey))
        self.view_mesh_btn.setFixedSize(self.previframewidth, self.previframeheight)
        self.view_mesh_btn.setCheckable(True)
        self.view_mesh_btn.setChecked(True)
        self.view_mesh_btn.setToolTip("Toggle Mesh")
        #self.view_mesh_btn.clicked.connect(self._toggle_mesh)
        controls_layout.addWidget(self.view_mesh_btn)
        controls_layout.addSpacing(self.panelspacing)

        return controls_frame

    def _create_status_bar(self): #vers 1
        from PyQt6.QtWidgets import QFrame, QHBoxLayout, QLabel

        status_bar = QFrame()
        status_bar.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        status_bar.setFixedHeight(self.statusheight)

        layout = QHBoxLayout(status_bar)
        layout.setContentsMargins(*self.get_tab_margins())
        layout.setSpacing(self.tabbarspacing)

        # Left: Ready
        self.status_label = QLabel("Ready")
        layout.addWidget(self.status_label)

        if hasattr(self, 'status_info'):
            size_kb = len(col_data) / 1024
            obj_count = len(self.object_list)
            #self.status_ojs_info.setText(f"Object: {obj_count} | obj: {size_kb:.1f} KB")

        return status_bar

# - Fonts settings

    def _apply_title_font(self): #vers 1
        """Apply title font to title bar labels"""
        if hasattr(self, 'title_font'):
            # Find all title labels
            for label in self.findChildren(QLabel):
                if label.objectName() == "title_label" or "🗺️" in label.text():
                    label.setFont(self.title_font)


    def _apply_panel_font(self): #vers 1
        """Apply panel font to info panels and labels"""
        if hasattr(self, 'panel_font'):
            # Apply to info labels (Mipmaps, Bumpmaps, status labels)
            for label in self.findChildren(QLabel):
                if any(x in label.text() for x in ["Mipmaps:", "Bumpmaps:", "Status:", "Type:", "Format:"]):
                    label.setFont(self.panel_font)


    def _apply_button_font(self): #vers 1
        """Apply button font to all buttons"""
        if hasattr(self, 'button_font'):
            for button in self.findChildren(QPushButton):
                button.setFont(self.button_font)


    def _apply_infobar_font(self): #vers 1
        """Apply fixed-width font to info bar at bottom"""
        if hasattr(self, 'infobar_font'):
            if hasattr(self, 'info_bar'):
                self.info_bar.setFont(self.infobar_font)

# - Save, Load config dialogs

    def _open_settings_dialog(self): #vers 1
        """Open settings dialog and refresh on save"""
        dialog = SettingsDialog(self.mel_settings, self)
        if dialog.exec():
            # Refresh platform list with new ROM path
            self._scan_platforms()
            self.status_label.setText("Settings saved - platforms refreshed")


    def _setup_settings_button(self): #vers 1
        """Setup settings button in UI"""
        settings_btn = QPushButton("âš™ Settings")
        settings_btn.clicked.connect(self._open_settings_dialog)
        settings_btn.setMaximumWidth(120)
        return settings_btn


    def _show_settings_dialog(self): #vers 5
        """Show comprehensive settings dialog with all tabs including hotkeys"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                                    QWidget, QLabel, QPushButton, QGroupBox,
                                    QCheckBox, QSpinBox, QFormLayout, QScrollArea,
                                    QKeySequenceEdit, QComboBox, QMessageBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QKeySequence

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        dialog.setMinimumWidth(700)
        dialog.setMinimumHeight(600)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # === DISPLAY TAB ===
        display_tab = QWidget()
        display_layout = QVBoxLayout(display_tab)

        # Thumbnail settings
        thumb_group = QGroupBox("Thumbnail Display")
        thumb_layout = QVBoxLayout()

        thumb_size_layout = QHBoxLayout()
        thumb_size_layout.addWidget(QLabel("Thumbnail size:"))
        thumb_size_spin = QSpinBox()
        thumb_size_spin.setRange(32, 256)
        thumb_size_spin.setValue(self.thumbnail_size if hasattr(self, 'thumbnail_size') else 64)
        thumb_size_spin.setSuffix(" px")
        thumb_size_layout.addWidget(thumb_size_spin)
        thumb_size_layout.addStretch()
        thumb_layout.addLayout(thumb_size_layout)

        thumb_group.setLayout(thumb_layout)
        display_layout.addWidget(thumb_group)

        # Table display settings
        table_group = QGroupBox("Table Display")
        table_layout = QVBoxLayout()

        row_height_layout = QHBoxLayout()
        row_height_layout.addWidget(QLabel("Row height:"))
        row_height_spin = QSpinBox()
        row_height_spin.setRange(50, 200)
        row_height_spin.setValue(getattr(self, 'table_row_height', 100))
        row_height_spin.setSuffix(" px")
        row_height_layout.addWidget(row_height_spin)
        row_height_layout.addStretch()
        table_layout.addLayout(row_height_layout)

        show_grid_check = QCheckBox("Show grid lines")
        show_grid_check.setChecked(getattr(self, 'show_grid_lines', True))
        table_layout.addWidget(show_grid_check)

        table_group.setLayout(table_layout)
        display_layout.addWidget(table_group)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")

        # === PREVIEW TAB ===
        preview_tab = QWidget()
        preview_layout = QVBoxLayout(preview_tab)

        # Preview window settings
        preview_window_group = QGroupBox("Preview Window")
        preview_window_layout = QVBoxLayout()

        show_preview_check = QCheckBox("Show preview window by default")
        show_preview_check.setChecked(getattr(self, 'show_preview_default', True))
        show_preview_check.setToolTip("Automatically open preview when selecting surface")
        preview_window_layout.addWidget(show_preview_check)

        auto_refresh_check = QCheckBox("Auto-refresh preview on selection")
        auto_refresh_check.setChecked(getattr(self, 'auto_refresh_preview', True))
        auto_refresh_check.setToolTip("Update preview immediately when clicking surface")
        preview_window_layout.addWidget(auto_refresh_check)

        preview_window_group.setLayout(preview_window_layout)
        preview_layout.addWidget(preview_window_group)

        # Preview size settings
        preview_size_group = QGroupBox("Preview Size")
        preview_size_layout = QVBoxLayout()

        preview_width_layout = QHBoxLayout()
        preview_width_layout.addWidget(QLabel("Default width:"))
        preview_width_spin = QSpinBox()
        preview_width_spin.setRange(200, 1920)
        preview_width_spin.setValue(getattr(self, 'preview_width', 512))
        preview_width_spin.setSuffix(" px")
        preview_width_layout.addWidget(preview_width_spin)
        preview_width_layout.addStretch()
        preview_size_layout.addLayout(preview_width_layout)

        preview_height_layout = QHBoxLayout()
        preview_height_layout.addWidget(QLabel("Default height:"))
        preview_height_spin = QSpinBox()
        preview_height_spin.setRange(200, 1080)
        preview_height_spin.setValue(getattr(self, 'preview_height', 512))
        preview_height_spin.setSuffix(" px")
        preview_height_layout.addWidget(preview_height_spin)
        preview_height_layout.addStretch()
        preview_size_layout.addLayout(preview_height_layout)

        preview_size_group.setLayout(preview_size_layout)
        preview_layout.addWidget(preview_size_group)

        # Preview background
        preview_bg_group = QGroupBox("Preview Background")
        preview_bg_layout = QVBoxLayout()

        bg_combo = QComboBox()
        bg_combo.addItems(["Black", "White", "Gray", "Custom Color"])
        bg_combo.setCurrentText(getattr(self, 'preview_background', 'Checkerboard'))
        preview_bg_layout.addWidget(bg_combo)

        preview_bg_group.setLayout(preview_bg_layout)
        preview_layout.addWidget(preview_bg_group)

        # Preview zoom
        preview_zoom_group = QGroupBox("Preview Zoom")
        preview_zoom_layout = QVBoxLayout()

        fit_to_window_check = QCheckBox("Fit to window by default")
        fit_to_window_check.setChecked(getattr(self, 'preview_fit_to_window', True))
        preview_zoom_layout.addWidget(fit_to_window_check)

        smooth_zoom_check = QCheckBox("Use smooth scaling")
        smooth_zoom_check.setChecked(getattr(self, 'preview_smooth_scaling', True))
        smooth_zoom_check.setToolTip("Better quality but slower for large model mesh")
        preview_zoom_layout.addWidget(smooth_zoom_check)

        preview_zoom_group.setLayout(preview_zoom_layout)
        preview_layout.addWidget(preview_zoom_group)

        preview_layout.addStretch()
        tabs.addTab(preview_tab, "Preview")

        # === EXPORT TAB ===
        export_tab = QWidget()
        export_layout = QVBoxLayout(export_tab)

        # Export format
        format_group = QGroupBox("Default Collision Export Format")
        format_layout = QVBoxLayout()

        format_combo = QComboBox()
        format_combo.addItems(["COL", "COL2", "COL3", "CST", "3DS"])
        format_combo.setCurrentText(getattr(self, 'default_export_format', 'COL'))
        format_layout.addWidget(format_combo)
        format_hint = QLabel("COL recommended for GTAIII/VC, COL2 for SA")
        format_hint.setStyleSheet("color: #888; font-style: italic;")
        format_layout.addWidget(format_hint)

        format_group.setLayout(format_layout)
        export_layout.addWidget(format_group)

        # Export options
        export_options_group = QGroupBox("Export Options")
        export_options_layout = QVBoxLayout()

        preserve_shadow_check = QCheckBox("Preserve Shadow Mesh when exporting")
        preserve_shadow_check.setChecked(getattr(self, 'export_preserve_shadow', True))
        export_options_layout.addWidget(preserve_shadow_check)

        export_shadowm_check = QCheckBox("Export shadow as separate files")
        export_shadowm_check.setChecked(getattr(self, 'export_shadow_separate', False))
        export_shadowm_check.setToolTip("Save each shadow map as _shadow.col, etc.")
        export_options_layout.addWidget(export_shadowm_check)

        create_subfolders_check = QCheckBox("Create subfolders when exporting all")
        create_subfolders_check.setChecked(getattr(self, 'export_create_subfolders', False))
        create_subfolders_check.setToolTip("Organize exports into folders by col type")
        export_options_layout.addWidget(create_subfolders_check)

        export_options_group.setLayout(export_options_layout)
        export_layout.addWidget(export_options_group)

        # Compatibility note
        compat_label = QLabel(
            "Note: PLACEholder." #TODO
        )
        compat_label.setWordWrap(True)
        compat_label.setStyleSheet("padding: 10px; background-color: #3a3a3a; border-radius: 4px;")
        export_layout.addWidget(compat_label)

        export_layout.addStretch()
        tabs.addTab(export_tab, "Export")

        # === IMPORT TAB ===
        import_tab = QWidget()
        import_layout = QVBoxLayout(import_tab)

        # Import behavior
        import_behavior_group = QGroupBox("Import Behavior")
        import_behavior_layout = QVBoxLayout()

        replace_check = QCheckBox("Replace existing collision with same name")
        replace_check.setChecked(getattr(self, 'import_replace_existing', False))
        import_behavior_layout.addWidget(replace_check)

        auto_format_check = QCheckBox("Automatically select best format")
        auto_format_check.setChecked(getattr(self, 'import_auto_format', True))
        auto_format_check.setToolTip("Choose COL/COL3 based on collision version")
        import_behavior_layout.addWidget(auto_format_check)

        import_behavior_group.setLayout(import_behavior_layout)
        import_layout.addWidget(import_behavior_group)

        # Import format
        import_format_group = QGroupBox("Default Collision Format")
        import_format_layout = QVBoxLayout()

        import_format_combo = QComboBox()
        import_format_combo.addItems(["COL", "COL2", "COL3", "CST", "3DS"])
        import_format_combo.setCurrentText(getattr(self, 'default_import_format', 'COL'))
        import_format_layout.addWidget(import_format_combo)

        format_note = QLabel("COL2/COL3: compression\n, COL type 1 always Uncompressed")
        format_note.setStyleSheet("color: #888; font-style: italic;")
        import_format_layout.addWidget(format_note)

        import_format_group.setLayout(import_format_layout)
        import_layout.addWidget(import_format_group)

        import_layout.addStretch()
        tabs.addTab(import_tab, "Import")

        # === Collision CONSTRAINTS TAB ===
        constraints_tab = QWidget()
        constraints_layout = QVBoxLayout(constraints_tab)

        # Collision naming
        naming_group = QGroupBox("Collision Naming")
        naming_layout = QVBoxLayout()

        name_limit_check = QCheckBox("Enable name length limit")
        name_limit_check.setChecked(getattr(self, 'name_limit_enabled', True))
        name_limit_check.setToolTip("Enforce maximum Collision name length")
        naming_layout.addWidget(name_limit_check)

        char_limit_layout = QHBoxLayout()
        char_limit_layout.addWidget(QLabel("Maximum characters:"))
        char_limit_spin = QSpinBox()
        char_limit_spin.setRange(8, 64)
        char_limit_spin.setValue(getattr(self, 'max_collision_name_length', 32))
        char_limit_spin.setToolTip("RenderWare default is 32 characters")
        char_limit_layout.addWidget(char_limit_spin)
        char_limit_layout.addStretch()
        naming_layout.addLayout(char_limit_layout)

        naming_group.setLayout(naming_layout)
        constraints_layout.addWidget(naming_group)

        # Format support
        format_support_group = QGroupBox("Format Support")
        format_support_layout = QVBoxLayout()

        format_support_group.setLayout(format_support_layout)
        constraints_layout.addWidget(format_support_group)

        constraints_layout.addStretch()
        tabs.addTab(constraints_tab, "Constraints")

        # === KEYBOARD SHORTCUTS TAB ===
        hotkeys_tab = QWidget()
        hotkeys_layout = QVBoxLayout(hotkeys_tab)

        # Add scroll area for hotkeys
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)

        # File Operations Group
        file_group = QGroupBox("File Operations")
        file_form = QFormLayout()

        hotkey_edit_open = QKeySequenceEdit(self.hotkey_open.key() if hasattr(self, 'hotkey_open') else QKeySequence.StandardKey.Open)
        file_form.addRow("Open col:", hotkey_edit_open)

        hotkey_edit_save = QKeySequenceEdit(self.hotkey_save.key() if hasattr(self, 'hotkey_save') else QKeySequence.StandardKey.Save)
        file_form.addRow("Save col:", hotkey_edit_save)

        hotkey_edit_force_save = QKeySequenceEdit(self.hotkey_force_save.key() if hasattr(self, 'hotkey_force_save') else QKeySequence("Alt+Shift+S"))
        force_save_layout = QHBoxLayout()
        force_save_layout.addWidget(hotkey_edit_force_save)
        force_save_hint = QLabel("(Force save even if unmodified)")
        force_save_hint.setStyleSheet("color: #888; font-style: italic;")
        force_save_layout.addWidget(force_save_hint)
        file_form.addRow("Force Save:", force_save_layout)

        hotkey_edit_save_as = QKeySequenceEdit(self.hotkey_save_as.key() if hasattr(self, 'hotkey_save_as') else QKeySequence.StandardKey.SaveAs)
        file_form.addRow("Save As:", hotkey_edit_save_as)

        hotkey_edit_close = QKeySequenceEdit(self.hotkey_close.key() if hasattr(self, 'hotkey_close') else QKeySequence.StandardKey.Close)
        file_form.addRow("Close:", hotkey_edit_close)

        file_group.setLayout(file_form)
        scroll_layout.addWidget(file_group)

        # Edit Operations Group

        # Collision Operations Group
        #coll_group = QGroupBox("Collision Operations")
        #coll_group = QFormLayout()

        hotkey_edit_import = QKeySequenceEdit(self.hotkey_import.key() if hasattr(self, 'hotkey_import') else QKeySequence("Ctrl+I"))
        #coll_form.addRow("Import Collision:", hotkey_edit_import)

        hotkey_edit_export = QKeySequenceEdit(self.hotkey_export.key() if hasattr(self, 'hotkey_export') else QKeySequence("Ctrl+E"))
        #coll_form.addRow("Export Collision:", hotkey_edit_export)

        hotkey_edit_export_all = QKeySequenceEdit(self.hotkey_export_all.key() if hasattr(self, 'hotkey_export_all') else QKeySequence("Ctrl+Shift+E"))
        #coll_form.addRow("Export All:", hotkey_edit_export_all)

        #coll_group.setLayout(coll_form)

        # View Operations Group
        view_group = QGroupBox("View Operations")
        view_form = QFormLayout()

        hotkey_edit_refresh = QKeySequenceEdit(self.hotkey_refresh.key() if hasattr(self, 'hotkey_refresh') else QKeySequence.StandardKey.Refresh)
        view_form.addRow("Refresh:", hotkey_edit_refresh)

        hotkey_edit_properties = QKeySequenceEdit(self.hotkey_properties.key() if hasattr(self, 'hotkey_properties') else QKeySequence("Alt+Return"))
        view_form.addRow("Properties:", hotkey_edit_properties)

        hotkey_edit_find = QKeySequenceEdit(self.hotkey_find.key() if hasattr(self, 'hotkey_find') else QKeySequence.StandardKey.Find)
        view_form.addRow("Find/Search:", hotkey_edit_find)

        hotkey_edit_help = QKeySequenceEdit(self.hotkey_help.key() if hasattr(self, 'hotkey_help') else QKeySequence.StandardKey.HelpContents)
        view_form.addRow("Help:", hotkey_edit_help)

        view_group.setLayout(view_form)
        #scroll_layout.addWidget(view_group)

        scroll_layout.addStretch()

        scroll.setWidget(scroll_widget)
        #hotkeys_layout.addWidget(scroll)

        # Reset to defaults button
        reset_layout = QHBoxLayout()
        reset_layout.addStretch()
        reset_hotkeys_btn = QPushButton("Reset to Plasma6 Defaults")

        def reset_hotkeys():
            reply = QMessageBox.question(dialog, "Reset Hotkeys",
                "Reset all keyboard shortcuts to Plasma6 defaults?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)

            if reply == QMessageBox.StandardButton.Yes:
                hotkey_edit_open.setKeySequence(QKeySequence.StandardKey.Open)
                hotkey_edit_save.setKeySequence(QKeySequence.StandardKey.Save)
                hotkey_edit_force_save.setKeySequence(QKeySequence("Alt+Shift+S"))
                hotkey_edit_save_as.setKeySequence(QKeySequence.StandardKey.SaveAs)
                hotkey_edit_close.setKeySequence(QKeySequence.StandardKey.Close)
                hotkey_edit_undo.setKeySequence(QKeySequence.StandardKey.Undo)
                hotkey_edit_copy.setKeySequence(QKeySequence.StandardKey.Copy)
                hotkey_edit_paste.setKeySequence(QKeySequence.StandardKey.Paste)
                hotkey_edit_delete.setKeySequence(QKeySequence.StandardKey.Delete)
                hotkey_edit_duplicate.setKeySequence(QKeySequence("Ctrl+D"))
                hotkey_edit_rename.setKeySequence(QKeySequence("F2"))
                hotkey_edit_import.setKeySequence(QKeySequence("Ctrl+I"))
                hotkey_edit_export.setKeySequence(QKeySequence("Ctrl+E"))
                hotkey_edit_export_all.setKeySequence(QKeySequence("Ctrl+Shift+E"))
                hotkey_edit_refresh.setKeySequence(QKeySequence.StandardKey.Refresh)
                hotkey_edit_properties.setKeySequence(QKeySequence("Alt+Return"))
                hotkey_edit_find.setKeySequence(QKeySequence.StandardKey.Find)
                hotkey_edit_help.setKeySequence(QKeySequence.StandardKey.HelpContents)

        reset_hotkeys_btn.clicked.connect(reset_hotkeys)
        reset_layout.addWidget(reset_hotkeys_btn)
        hotkeys_layout.addLayout(reset_layout)

        tabs.addTab(hotkeys_tab, "Keyboard Shortcuts")

        # Add tabs widget to main layout
        layout.addWidget(tabs)

        # Dialog buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        def apply_settings(close_dialog=False):
            """Apply all settings"""
            # Apply display settings
            self.thumbnail_size = thumb_size_spin.value()
            self.table_row_height = row_height_spin.value()
            self.show_grid_lines = show_grid_check.isChecked()

            # Apply preview settings
            self.show_preview_default = show_preview_check.isChecked()
            self.auto_refresh_preview = auto_refresh_check.isChecked()
            self.preview_width = preview_width_spin.value()
            self.preview_height = preview_height_spin.value()
            self.preview_background = bg_combo.currentText()
            self.preview_fit_to_window = fit_to_window_check.isChecked()
            self.preview_smooth_scaling = smooth_zoom_check.isChecked()

            # Apply export settings
            self.default_export_format = format_combo.currentText()
            self.export_preserve_alpha = preserve_alpha_check.isChecked()
            self.export_shadow_separate = export_shadow_check.isChecked()
            self.export_create_subfolders = create_subfolders_check.isChecked()

            # Apply import settings
            self.import_auto_name = auto_name_check.isChecked()
            self.import_replace_existing = replace_check.isChecked()
            self.import_auto_format = auto_format_check.isChecked()
            self.default_import_format = import_format_combo.currentText()

            # Apply constraint settings
            self.dimension_limiting_enabled = dimension_check.isChecked()
            self.splash_screen_mode = splash_check.isChecked()
            self.custom_max_dimension = max_dim_spin.value()
            self.name_limit_enabled = name_limit_check.isChecked()
            self.max_surface_name_length = char_limit_spin.value()
            self.iff_import_enabled = iff_check.isChecked()

            # Apply hotkeys
            if hasattr(self, 'hotkey_open'):
                self.hotkey_open.setKey(hotkey_edit_open.keySequence())
            if hasattr(self, 'hotkey_save'):
                self.hotkey_save.setKey(hotkey_edit_save.keySequence())
            if hasattr(self, 'hotkey_force_save'):
                self.hotkey_force_save.setKey(hotkey_edit_force_save.keySequence())
            if hasattr(self, 'hotkey_save_as'):
                self.hotkey_save_as.setKey(hotkey_edit_save_as.keySequence())
            if hasattr(self, 'hotkey_close'):
                self.hotkey_close.setKey(hotkey_edit_close.keySequence())
            if hasattr(self, 'hotkey_undo'):
                self.hotkey_undo.setKey(hotkey_edit_undo.keySequence())
            if hasattr(self, 'hotkey_copy'):
                self.hotkey_copy.setKey(hotkey_edit_copy.keySequence())
            if hasattr(self, 'hotkey_paste'):
                self.hotkey_paste.setKey(hotkey_edit_paste.keySequence())
            if hasattr(self, 'hotkey_delete'):
                self.hotkey_delete.setKey(hotkey_edit_delete.keySequence())
            if hasattr(self, 'hotkey_duplicate'):
                self.hotkey_duplicate.setKey(hotkey_edit_duplicate.keySequence())
            if hasattr(self, 'hotkey_rename'):
                self.hotkey_rename.setKey(hotkey_edit_rename.keySequence())
            if hasattr(self, 'hotkey_import'):
                self.hotkey_import.setKey(hotkey_edit_import.keySequence())
            if hasattr(self, 'hotkey_export'):
                self.hotkey_export.setKey(hotkey_edit_export.keySequence())
            if hasattr(self, 'hotkey_export_all'):
                self.hotkey_export_all.setKey(hotkey_edit_export_all.keySequence())
            if hasattr(self, 'hotkey_refresh'):
                self.hotkey_refresh.setKey(hotkey_edit_refresh.keySequence())
            if hasattr(self, 'hotkey_properties'):
                self.hotkey_properties.setKey(hotkey_edit_properties.keySequence())
            if hasattr(self, 'hotkey_find'):
                self.hotkey_find.setKey(hotkey_edit_find.keySequence())
            if hasattr(self, 'hotkey_help'):
                self.hotkey_help.setKey(hotkey_edit_help.keySequence())

            # Refresh UI with new settings
            if hasattr(self, '_reload_surface_table'):
                self._reload_surface_table()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Settings applied")

            if close_dialog:
                dialog.accept()

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(lambda: apply_settings(close_dialog=False))
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(lambda: apply_settings(close_dialog=True))
        button_layout.addWidget(ok_btn)

        layout.addLayout(button_layout)

        dialog.exec()

    def _show_settings_context_menu(self, pos): #vers 1
        """Show context menu for Settings button"""
        from PyQt6.QtWidgets import QMenu

        menu = QMenu(self)

        # Move window action
        move_action = menu.addAction("Move Window")
        move_action.triggered.connect(self._enable_move_mode)

        # Maximize window action
        max_action = menu.addAction("Maximize Window")
        max_action.triggered.connect(self._toggle_maximize)

        # Minimize action
        min_action = menu.addAction("Minimize")
        min_action.triggered.connect(self.showMinimized)

        menu.addSeparator()

        # Upscale Native action
        upscale_action = menu.addAction("Upscale Native")
        upscale_action.setCheckable(True)
        upscale_action.setChecked(False)
        upscale_action.triggered.connect(self._toggle_upscale_native)

        # Shaders action
        shaders_action = menu.addAction("Shaders")
        shaders_action.triggered.connect(self._show_shaders_dialog)

        menu.addSeparator()

        # Icon display mode submenu # TODO icon only system is missing.
        display_menu = menu.addMenu("Platform Display")

        icons_text_action = display_menu.addAction("Icons & Text")
        icons_text_action.setCheckable(True)
        icons_text_action.setChecked(self.icon_display_mode == "icons_and_text")
        icons_text_action.triggered.connect(lambda: self._set_icon_display_mode("icons_and_text"))

        icons_only_action = display_menu.addAction("Icons Only")
        icons_only_action.setCheckable(True)
        icons_only_action.setChecked(self.icon_display_mode == "icons_only")
        icons_only_action.triggered.connect(lambda: self._set_icon_display_mode("icons_only"))

        text_only_action = display_menu.addAction("Text Only")
        text_only_action.setCheckable(True)
        text_only_action.setChecked(self.icon_display_mode == "text_only")
        text_only_action.triggered.connect(lambda: self._set_icon_display_mode("text_only"))

        # Show menu at button position
        menu.exec(self.settings_btn.mapToGlobal(pos))


# - Panel settings

    def _show_workshop_settings(self): #vers 1
        #Show complete workshop settings dialog
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                    QTabWidget, QWidget, QGroupBox, QFormLayout,
                                    QSpinBox, QComboBox, QSlider, QLabel, QCheckBox,
                                    QFontComboBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + "Settings")
        dialog.setMinimumWidth(650)
        dialog.setMinimumHeight(550)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # TAB 1: FONTS (FIRST TAB)

        fonts_tab = QWidget()
        fonts_layout = QVBoxLayout(fonts_tab)

        # Default Font
        default_font_group = QGroupBox("Default Font")
        default_font_layout = QHBoxLayout()

        default_font_combo = QFontComboBox()
        default_font_combo.setCurrentFont(self.font())
        default_font_layout.addWidget(default_font_combo)

        default_font_size = QSpinBox()
        default_font_size.setRange(8, 24)
        default_font_size.setValue(self.font().pointSize())
        default_font_size.setSuffix(" pt")
        default_font_size.setFixedWidth(80)
        default_font_layout.addWidget(default_font_size)

        default_font_group.setLayout(default_font_layout)
        fonts_layout.addWidget(default_font_group)

        # Title Font
        title_font_group = QGroupBox("Title Font")
        title_font_layout = QHBoxLayout()

        title_font_combo = QFontComboBox()

        if hasattr(self, 'title_font'):
            title_font_combo.setCurrentFont(self.title_font)
        else:
            title_font_combo.setCurrentFont(QFont("Arial", 14))
        title_font_layout.addWidget(title_font_combo)

        title_font_size = QSpinBox()
        title_font_size.setRange(10, 32)
        title_font_size.setValue(getattr(self, 'title_font', QFont("Arial", 14)).pointSize())
        title_font_size.setSuffix(" pt")
        title_font_size.setFixedWidth(80)
        title_font_layout.addWidget(title_font_size)

        title_font_group.setLayout(title_font_layout)
        fonts_layout.addWidget(title_font_group)

        # Panel Font
        panel_font_group = QGroupBox("Panel Headers Font")
        panel_font_layout = QHBoxLayout()

        panel_font_combo = QFontComboBox()
        if hasattr(self, 'panel_font'):
            panel_font_combo.setCurrentFont(self.panel_font)
        else:
            panel_font_combo.setCurrentFont(QFont("Arial", 10))
        panel_font_layout.addWidget(panel_font_combo)

        panel_font_size = QSpinBox()
        panel_font_size.setRange(8, 18)
        panel_font_size.setValue(getattr(self, 'panel_font', QFont("Arial", 10)).pointSize())
        panel_font_size.setSuffix(" pt")
        panel_font_size.setFixedWidth(80)
        panel_font_layout.addWidget(panel_font_size)

        panel_font_group.setLayout(panel_font_layout)
        fonts_layout.addWidget(panel_font_group)

        # Button Font
        button_font_group = QGroupBox("Button Font")
        button_font_layout = QHBoxLayout()

        button_font_combo = QFontComboBox()
        if hasattr(self, 'button_font'):
            button_font_combo.setCurrentFont(self.button_font)
        else:
            button_font_combo.setCurrentFont(QFont("Arial", 10))
        button_font_layout.addWidget(button_font_combo)

        button_font_size = QSpinBox()
        button_font_size.setRange(8, 16)
        button_font_size.setValue(getattr(self, 'button_font', QFont("Arial", 10)).pointSize())
        button_font_size.setSuffix(" pt")
        button_font_size.setFixedWidth(80)
        button_font_layout.addWidget(button_font_size)

        button_font_group.setLayout(button_font_layout)
        fonts_layout.addWidget(button_font_group)

        # Info Bar Font
        infobar_font_group = QGroupBox("Info Bar Font")
        infobar_font_layout = QHBoxLayout()

        infobar_font_combo = QFontComboBox()
        if hasattr(self, 'infobar_font'):
            infobar_font_combo.setCurrentFont(self.infobar_font)
        else:
            infobar_font_combo.setCurrentFont(QFont("Courier New", 9))
        infobar_font_layout.addWidget(infobar_font_combo)

        infobar_font_size = QSpinBox()
        infobar_font_size.setRange(7, 14)
        infobar_font_size.setValue(getattr(self, 'infobar_font', QFont("Courier New", 9)).pointSize())
        infobar_font_size.setSuffix(" pt")
        infobar_font_size.setFixedWidth(80)
        infobar_font_layout.addWidget(infobar_font_size)

        infobar_font_group.setLayout(infobar_font_layout)
        fonts_layout.addWidget(infobar_font_group)

        fonts_layout.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        # TAB 2: DISPLAY SETTINGS

        display_tab = QWidget()
        display_layout = QVBoxLayout(display_tab)

        # Button display mode
        button_group = QGroupBox("Button Display Mode")
        button_layout = QVBoxLayout()

        button_mode_combo = QComboBox()
        button_mode_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])
        current_mode = getattr(self, 'button_display_mode', 'both')
        mode_map = {'both': 0, 'icons': 1, 'text': 2}
        button_mode_combo.setCurrentIndex(mode_map.get(current_mode, 0))
        button_layout.addWidget(button_mode_combo)

        button_hint = QLabel("Changes how toolbar buttons are displayed")
        button_hint.setStyleSheet("color: #888; font-style: italic;")
        button_layout.addWidget(button_hint)

        button_group.setLayout(button_layout)
        display_layout.addWidget(button_group)

        # Table display
        table_group = QGroupBox("Surface List Display")
        table_layout = QVBoxLayout()

        show_thumbnails = QCheckBox("Show Surface types")
        show_thumbnails.setChecked(True)
        table_layout.addWidget(show_thumbnails)

        show_warnings = QCheckBox("Show warning icons for suspicious files")
        show_warnings.setChecked(True)
        show_warnings.setToolTip("Shows surface types")
        table_layout.addWidget(show_warnings)

        table_group.setLayout(table_layout)
        display_layout.addWidget(table_group)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")

        # TAB 3: placeholder
        # TAB 4: PERFORMANCE

        perf_tab = QWidget()
        perf_layout = QVBoxLayout(perf_tab)

        perf_group = QGroupBox("Performance Settings")
        perf_form = QFormLayout()

        preview_quality = QComboBox()
        preview_quality.addItems(["Low (Fast)", "Medium", "High (Slow)"])
        preview_quality.setCurrentIndex(1)
        perf_form.addRow("Preview Quality:", preview_quality)

        thumb_size = QSpinBox()
        thumb_size.setRange(32, 128)
        thumb_size.setValue(64)
        thumb_size.setSuffix(" px")
        perf_form.addRow("Thumbnail Size:", thumb_size)

        perf_group.setLayout(perf_form)
        perf_layout.addWidget(perf_group)

        # Caching
        cache_group = QGroupBox("Caching")
        cache_layout = QVBoxLayout()

        enable_cache = QCheckBox("Enable surface preview caching")
        enable_cache.setChecked(True)
        cache_layout.addWidget(enable_cache)

        cache_hint = QLabel("Caching improves performance but uses more memory")
        cache_hint.setStyleSheet("color: #888; font-style: italic;")
        cache_layout.addWidget(cache_hint)

        cache_group.setLayout(cache_layout)
        perf_layout.addWidget(cache_group)

        perf_layout.addStretch()
        tabs.addTab(perf_tab, "Performance")

        # TAB 5: PREVIEW SETTINGS (LAST TAB)

        preview_tab = QWidget()
        preview_layout = QVBoxLayout(preview_tab)

        # Zoom Settings
        zoom_group = QGroupBox("Zoom Settings")
        zoom_form = QFormLayout()

        zoom_spin = QSpinBox()
        zoom_spin.setRange(10, 500)
        zoom_spin.setValue(int(getattr(self, 'zoom_level', 1.0) * 100))
        zoom_spin.setSuffix("%")
        zoom_form.addRow("Default Zoom:", zoom_spin)

        zoom_group.setLayout(zoom_form)
        preview_layout.addWidget(zoom_group)

        # Background Settings
        bg_group = QGroupBox("Background Settings")
        bg_layout = QVBoxLayout()

        # Background mode
        bg_mode_layout = QFormLayout()
        bg_mode_combo = QComboBox()
        bg_mode_combo.addItems(["Solid Color", "Checkerboard", "Grid"])
        current_bg_mode = getattr(self, 'background_mode', 'solid')
        mode_idx = {"solid": 0, "checkerboard": 1, "checker": 1, "grid": 2}.get(current_bg_mode, 0)
        bg_mode_combo.setCurrentIndex(mode_idx)
        bg_mode_layout.addRow("Background Mode:", bg_mode_combo)
        bg_layout.addLayout(bg_mode_layout)

        bg_layout.addSpacing(10)

        # Checkerboard size
        cb_label = QLabel("Checkerboard Size:")
        bg_layout.addWidget(cb_label)

        cb_layout = QHBoxLayout()
        cb_slider = QSlider(Qt.Orientation.Horizontal)
        cb_slider.setMinimum(4)
        cb_slider.setMaximum(64)
        cb_slider.setValue(getattr(self, '_checkerboard_size', 16))
        cb_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        cb_slider.setTickInterval(8)
        cb_layout.addWidget(cb_slider)

        cb_spin = QSpinBox()
        cb_spin.setMinimum(4)
        cb_spin.setMaximum(64)
        cb_spin.setValue(getattr(self, '_checkerboard_size', 16))
        cb_spin.setSuffix(" px")
        cb_spin.setFixedWidth(80)
        cb_layout.addWidget(cb_spin)

        bg_layout.addLayout(cb_layout)

        # Connect checkerboard controls
        #cb_slider.valueChanged.connect(cb_spin.setValue)
        #cb_spin.valueChanged.connect(cb_slider.setValue)

        # Hint
        cb_hint = QLabel("Smaller = tighter pattern, larger = bigger squares")
        cb_hint.setStyleSheet("color: #888; font-style: italic; font-size: 10px;")
        bg_layout.addWidget(cb_hint)

        bg_group.setLayout(bg_layout)
        preview_layout.addWidget(bg_group)

        # Overlay Settings
        overlay_group = QGroupBox("Overlay View Settings")
        overlay_layout = QVBoxLayout()

        overlay_label = QLabel("Overlay Opacity (Wireframe over mesh):")
        overlay_layout.addWidget(overlay_label)

        opacity_layout = QHBoxLayout()
        opacity_slider = QSlider(Qt.Orientation.Horizontal)
        opacity_slider.setMinimum(0)
        opacity_slider.setMaximum(100)
        opacity_slider.setValue(getattr(self, '_overlay_opacity', 50))
        opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        opacity_slider.setTickInterval(10)
        opacity_layout.addWidget(opacity_slider)

        opacity_spin = QSpinBox()
        opacity_spin.setMinimum(0)
        opacity_spin.setMaximum(100)
        opacity_spin.setValue(getattr(self, '_overlay_opacity', 50))
        opacity_spin.setSuffix(" %")
        opacity_spin.setFixedWidth(80)
        opacity_layout.addWidget(opacity_spin)

        overlay_layout.addLayout(opacity_layout)

        # Connect opacity controls
        #opacity_slider.valueChanged.connect(opacity_spin.setValue)
        #opacity_spin.valueChanged.connect(opacity_slider.setValue)

        # Hint
        opacity_hint = QLabel("0")
        opacity_hint.setStyleSheet("color: #888; font-style: italic; font-size: 10px;")
        overlay_layout.addWidget(opacity_hint)

        overlay_group.setLayout(overlay_layout)
        preview_layout.addWidget(overlay_group)

        preview_layout.addStretch()
        tabs.addTab(preview_tab, "Preview")

        # Add tabs to dialog
        layout.addWidget(tabs)

        # BUTTONS

        btn_layout = QHBoxLayout()
        btn_layout.addStretch()

        # Apply button
        apply_btn = QPushButton("Apply Settings")
        apply_btn.setStyleSheet("""
            QPushButton {
                background: #0078d4;
                color: white;
                padding: 10px 24px;
                font-weight: bold;
                border-radius: 4px;
                font-size: 13px;
            }
            QPushButton:hover {
                background: #1984d8;
            }
        """)

    def _pick_background_color(self): #vers 1
        """Open color picker for background"""
        color = QColorDialog.getColor(self.preview_widget.bg_color, self, "Pick Background Color")
        if color.isValid():
            self.preview_widget.set_background_color(color)

# - ICON STUBS

# - UI ACTIONS (NO LOGIC) ---

    def _pan_preview(self, dx, dy): pass
    def _toggle_spheres(self, checked): pass
    def _toggle_boxes(self, checked): pass
    def _toggle_mesh(self, checked): pass


    def _create_preview_widget(self, level_data=None): #vers 5
        """Create preview widget - PreviewWidget for col_workshop"""
        if level_data is None:
            # Return collision preview widget for main preview area
            preview = CollisionPreviewWidget(self)
            img_debugger.debug(f"Created PreviewWidget: {type(preview)}")  # ADD THIS
            return preview

        # Original logic with level_data for mipmap/level cards (if needed)
        level_num = level_data.get('level', 0)
        width = level_data.get('width', 0)
        height = level_data.get('height', 0)
        rgba_data = level_data.get('rgba_data')
        preview_size = max(45, 120 - (level_num * 15))

        preview = QLabel()
        preview.setFixedSize(preview_size, preview_size)
        preview.setStyleSheet("""
            QLabel {
                background: #0a0a0a;
                border: 2px solid #3a3a3a;
                border-radius: 3px;
            }
        """)
        preview.setAlignment(Qt.AlignmentFlag.AlignCenter)

        if rgba_data and width > 0:
            try:
                image = QImage(rgba_data, width, height, width * 4, QImage.Format.Format_RGBA8888)
                if not image.isNull():
                    pixmap = QPixmap.fromImage(image)
                    scaled_pixmap = pixmap.scaled(
                        preview_size - 10, preview_size - 10,
                        Qt.AspectRatioMode.KeepAspectRatio,
                        Qt.TransformationMode.SmoothTransformation
                    )
                    preview.setPixmap(scaled_pixmap)
            except:
                preview.setText("No Data")
        else:
            preview.setText("No Data")

        return preview


    def _create_info_section(self, level_data): #vers 1
        """Create info section with stats grid"""
        info_widget = QWidget()
        layout = QVBoxLayout(info_widget)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(10)

        # Header with level number and dimensions
        header_layout = QHBoxLayout()

        level_num = level_data.get('level', 0)
        level_badge = QLabel(f"Level {level_num}")
        level_badge.setStyleSheet("""
            QLabel {
                background: #0d47a1;
                color: white;
                padding: 4px 12px;
                border-radius: 3px;
                font-weight: bold;
                font-size: 13px;
            }
        """)
        header_layout.addWidget(level_badge)

        width = level_data.get('width', 0)
        height = level_data.get('height', 0)
        dim_label = QLabel(f"{width} x {height}")
        dim_label.setStyleSheet("font-size: 16px; font-weight: bold; color: #4a9eff;")
        header_layout.addWidget(dim_label)

        # Main indicator
        if level_num == 0:
            main_badge = QLabel("Main Surface")
            main_badge.setStyleSheet("color: #4caf50; font-size: 12px;")
            header_layout.addWidget(main_badge)

        header_layout.addStretch()
        layout.addLayout(header_layout)

        # Stats grid
        stats_grid = self._create_stats_grid(level_data)
        layout.addWidget(stats_grid)

        return info_widget


    def _create_stats_grid(self, level_data): #vers 1
        """Create stats grid"""
        grid_widget = QWidget()
        grid_layout = QHBoxLayout(grid_widget)
        grid_layout.setContentsMargins(0, 0, 0, 0)
        grid_layout.setSpacing(8)

        fmt = level_data.get('format', self.collision_data.get('format', 'Unknown'))
        size = level_data.get('compressed_size', 0)
        size_kb = size / 1024

        # Format stat
        format_stat = self._create_stat_box("Format:", fmt)
        grid_layout.addWidget(format_stat)

        # Size stat
        size_stat = self._create_stat_box("Size:", f"{size_kb:.1f} KB")
        grid_layout.addWidget(size_stat)
        grid_layout.addWidget(comp_stat)

        # Status stat
        is_modified = level_data.get('level', 0) in self.modified_levels
        status_text = "⚠ Modified" if is_modified else "✓ Valid"
        status_color = "#ff9800" if is_modified else "#4caf50"
        status_stat = self._create_stat_box("Status:", status_text, status_color)
        grid_layout.addWidget(status_stat)

        return grid_widget


    def _create_stat_box(self, label, value, value_color="#e0e0e0"): #vers 1
        """Create individual stat box"""
        stat = QFrame()
        stat.setStyleSheet("""
            QFrame {
                background: #252525;
                border-radius: 3px;
                padding: 6px 10px;
            }
        """)

        layout = QHBoxLayout(stat)
        layout.setContentsMargins(8, 4, 8, 4)

        label_widget = QLabel(label)
        label_widget.setStyleSheet("color: #888; font-size: 12px;")
        layout.addWidget(label_widget)

        value_widget = QLabel(value)
        value_widget.setStyleSheet(f"color: {value_color}; font-weight: bold; font-size: 12px;")
        layout.addWidget(value_widget)

        return stat


    def _create_action_section(self, level_data): #vers 1
        """Create action buttons section"""
        action_widget = QWidget()
        layout = QVBoxLayout(action_widget)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(5)

        level_num = level_data.get('level', 0)

        # Export button
        export_btn = QPushButton("Export")
        export_btn.setStyleSheet("""
            QPushButton {
                background: #2e5d2e;
                border: 1px solid #3d7d3d;
                color: white;
                padding: 6px 12px;
                border-radius: 3px;
                font-size: 11px;
            }
            QPushButton:hover {
                background: #3d7d3d;
            }
        """)
        export_btn.clicked.connect(lambda: self._export_level(level_num))
        layout.addWidget(export_btn)

        # Import button
        import_btn = QPushButton("Import")
        import_btn.setStyleSheet("""
            QPushButton {
                background: #5d3d2e;
                border: 1px solid #7d4d3d;
                color: white;
                padding: 6px 12px;
                border-radius: 3px;
                font-size: 11px;
            }
            QPushButton:hover {
                background: #7d4d3d;
            }
        """)
        import_btn.clicked.connect(lambda: self._import_level(level_num))
        layout.addWidget(import_btn)

        # Delete button (not for level 0) or Edit button (for level 0)
        if level_num == 0:
            edit_btn = QPushButton("Edit")
            edit_btn.setStyleSheet("""
                QPushButton {
                    background: #3a3a3a;
                    border: 1px solid #4a4a4a;
                    color: white;
                    padding: 6px 12px;
                    border-radius: 3px;
                    font-size: 11px;
                }
                QPushButton:hover {
                    background: #4a4a4a;
                }
            """)
            edit_btn.clicked.connect(self._edit_main_surface)
            layout.addWidget(edit_btn)
        else:
            delete_btn = QPushButton("Delete")
            delete_btn.setStyleSheet("""
                QPushButton {
                    background: #5d2e2e;
                    border: 1px solid #7d3d3d;
                    color: white;
                    padding: 6px 12px;
                    border-radius: 3px;
                    font-size: 11px;
                }
                QPushButton:hover {
                    background: #7d3d3d;
                }
            """)
            delete_btn.clicked.connect(lambda: self._delete_level(level_num))
            layout.addWidget(delete_btn)

        return action_widget


# - Docking functions

    def _update_dock_button_visibility(self): #vers 2
        """Show/hide dock and tearoff buttons based on docked state"""
        if hasattr(self, 'dock_btn'):
            # Hide D button when docked, show when standalone
            self.dock_btn.setVisible(not self.is_docked)

        if hasattr(self, 'tearoff_btn'):
            # T button only visible when docked and not in standalone mode
            self.tearoff_btn.setVisible(self.is_docked and not self.standalone_mode)


    def toggle_dock_mode(self): #vers 2
        """Toggle between docked and standalone mode"""
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

        self._update_dock_button_visibility()


    def _toggle_tearoff(self): #vers 2
        """Toggle tear-off state (merge back to IMG Factory) - IMPROVED"""
        try:
            if self.is_docked:
                # Undock from main window
                self._undock_from_main()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{App_name} torn off from main window")
            else:
                # Dock back to main window
                self._dock_to_main()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{App_name} docked back to main window")

        except Exception as e:
            img_debugger.error(f"Error toggling tear-off: {str(e)}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Tear-off Error", f"Could not toggle tear-off state:\n{str(e)}")


    def _dock_to_main(self): #vers 9
        """Dock handled by overlay system in imgfactory - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                self.show()
                self.raise_()
                return

            # For proper docking, we need to be called from imgfactory
            # This method should be handled by imgfactory's overlay system
            if self.main_window and hasattr(self.main_window, App_name + '_docked'):
                # If available, use the main window's docking system
                self.main_window.open_col_workshop_docked()
            else:
                # Fallback: just show the window
                self.show()
                self.raise_()

            # Update dock state
            self.is_docked = True
            self._update_dock_button_visibility()

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} docked to main window")


        except Exception as e:
            img_debugger.error(f"Error docking: {str(e)}")
            self.show()


    def _undock_from_main(self): #vers 4
        """Undock from overlay mode to standalone window - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                # Switch from overlay to normal window
                self.setWindowFlags(Qt.WindowType.Window)
                self.is_overlay = False
                self.overlay_table = None

            # Set proper window flags for standalone mode
            self.setWindowFlags(Qt.WindowType.Window)

            # Ensure proper size when undocking
            if hasattr(self, 'original_size'):
                self.resize(self.original_size)
            else:
                self.resize(1000, 700)  # Reasonable default size

            self.is_docked = False
            self._update_dock_button_visibility()

            self.show()
            self.raise_()

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} undocked to standalone")

        except Exception as e:
            img_debugger.error(f"Error undocking: {str(e)}")
            # Fallback
            self.setWindowFlags(Qt.WindowType.Window)
            self.show()


    def update_view_options(viewer: 'COL3DViewport', **options): #vers 1
        """Update 3D viewer options"""
        try:
            viewer.set_view_options(**options)
            img_debugger.debug(f"View options updated: {options}")

            def _ensure_standalone_functionality(self): #vers 1
                """Ensure popped-out windows work independently of img factory"""
                try:
                    # When popped out, ensure all necessary functionality is available
                    if not self.is_docked or getattr(self, 'is_overlay', False) == False:
                        # Enable all UI elements that might be disabled when docked
                        if hasattr(self, 'toolbar') and self.toolbar:
                            self.toolbar.setEnabled(True)

                        # Ensure all buttons and controls work independently
                        if hasattr(self, 'main_window') and self.main_window is None:
                            # This is truly standalone, enable all features
                            if hasattr(self, 'dock_btn'):
                                self.dock_btn.setText("X")  # Change to close button when standalone
                                self.dock_btn.setToolTip("Close window")

                        # Set proper window flags for standalone operation
                        if not getattr(self, 'is_overlay', False):
                            self.setWindowFlags(Qt.WindowType.Window)

                except Exception as e:
                    img_debugger.error(f"Error ensuring standalone functionality: {str(e)}")

        except Exception as e:
            img_debugger.error(f"Error updating view options: {str(e)}")


    def _apply_button_mode(self, dialog): #vers 1
        """Apply button display mode"""
        mode_index = self.button_mode_combo.currentIndex()
        mode_map = {0: 'both', 1: 'icons', 2: 'text'}

        new_mode = mode_map[mode_index]

        if new_mode != self.button_display_mode:
            self.button_display_mode = new_mode
            self._update_all_buttons()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                mode_names = {0: 'Icons + Text', 1: 'Icons Only', 2: 'Text Only'}
                self.main_window.log_message(f"Button style: {mode_names[mode_index]}")

        dialog.close()


    def _apply_fonts_to_widgets(self): #vers 1
        """Apply fonts from AppSettings to all widgets"""
        if not hasattr(self, 'default_font'):
            return

        print("\n=== Applying Fonts ===")
        print(f"Default font: {self.default_font.family()} {self.default_font.pointSize()}pt")
        print(f"Title font: {self.title_font.family()} {self.title_font.pointSize()}pt")
        print(f"Panel font: {self.panel_font.family()} {self.panel_font.pointSize()}pt")
        print(f"Button font: {self.button_font.family()} {self.button_font.pointSize()}pt")

        # Apply default font to main window
        self.setFont(self.default_font)

        # Apply title font to titlebar
        if hasattr(self, 'title_label'):
            self.title_label.setFont(self.title_font)

        # Apply panel font to lists
        if hasattr(self, 'platform_list'):
            self.platform_list.setFont(self.panel_font)
        if hasattr(self, 'game_list'):
            self.game_list.setFont(self.panel_font)

        # Apply button font to all buttons
        for btn in self.findChildren(QPushButton):
            btn.setFont(self.button_font)

        print("Fonts applied to widgets")
        print("======================\n")


    def _update_toolbar_for_docking_state(self): #vers 1
        """Update toolbar visibility based on docking state"""
        # Hide/show drag button based on docking state
        if hasattr(self, 'drag_btn'):
            self.drag_btn.setVisible(not self.is_docked)


# - GUI button update

    def _update_all_buttons(self): #vers 4
        # Update all buttons to match display mode
        buttons_to_update = [
            # Toolbar buttons
            ('open_btn', 'Open'),
            ('save_btn', 'Save'),
        ]


    def _get_icon_color(self): #vers 1
        """Get icon color from current theme"""
        if APPSETTINGS_AVAILABLE and self.app_settings:
            colors = self.app_settings.get_theme_colors()
            return colors.get('text_primary', '#ffffff')
        return '#ffffff'


    def _apply_theme(self): #vers 2
        """Apply theme from main window"""
        try:
            if self.main_window and hasattr(self.main_window, 'app_settings'):
                # Get current theme
                theme_name = self.main_window.app_settings.current_settings.get('theme', 'IMG_Factory')
                stylesheet = self.main_window.app_settings.get_stylesheet()

                # Apply to App
                self.setStyleSheet(stylesheet)

                # Force update
                self.update()

                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"theme applied: {theme_name}")
            else:
                # Fallback dark theme
                self.setStyleSheet("""
                    QWidget {
                        background-color: #2b2b2b;
                        color: #e0e0e0;
                    }
                    QListWidget, QTableWidget, QTextEdit {
                        background-color: #1e1e1e;
                        border: 1px solid #3a3a3a;
                    }
                """)
        except Exception as e:
            print(f"Theme application error: {e}")


    def _apply_settings(self, dialog): #vers 5
        """Apply settings from dialog"""
        from PyQt6.QtGui import QFont

        # Store font settings
        self.title_font = QFont(self.title_font_combo.currentFont().family(), self.title_font_size.value())
        self.panel_font = QFont(self.panel_font_combo.currentFont().family(), self.panel_font_size.value())
        self.button_font = QFont(self.button_font_combo.currentFont().family(), self.button_font_size.value())
        self.infobar_font = QFont(self.infobar_font_combo.currentFont().family(), self.infobar_font_size.value())

        # Apply fonts to specific elements
        self._apply_title_font()
        self._apply_panel_font()
        self._apply_button_font()
        self._apply_infobar_font()
        self.default_export_format = format_combo.currentText()

        # Apply button display mode
        mode_map = ["icons", "text", "both"]
        new_mode = mode_map[self.settings_display_combo.currentIndex()]
        if new_mode != self.button_display_mode:
            self.button_display_mode = new_mode
            self._update_all_buttons()

        # Locale setting (would need implementation)
        locale_text = self.settings_locale_combo.currentText()


    def _open_file(self): #vers 1
        """Open file dialog and load COL file"""
        try:
            file_path, _ = QFileDialog.getOpenFileName(
                self,
                "Open Obj File",
                "",
                "Obj Files (*.col);;All Files (*)"
            )

            if file_path:
                self.open_obj_file(file_path)

        except Exception as e:
            img_debugger.error(f"Error in open file dialog: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to open file:\n{str(e)}")


    def _save_file(self): #vers 1
        """Save current COL file"""
        try:
            if not self.current_col_file:
                QMessageBox.warning(self, "Save", "No Obj file loaded to save")
                return

            if not self.current_file_path:
                # No path yet, do Save As
                self._save_file_as()
                return

            # Save to current path
            if self.current_obj_file.save():
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Saved Ojb: {os.path.basename(self.current_file_path)}")

                QMessageBox.information(self, "Save", f"Obj file saved successfully:\n{os.path.basename(self.current_file_path)}")
                img_debugger.success(f"Saved Obj file: {self.current_file_path}")
            else:
                error_msg = self.current_col_file.save_error if hasattr(self.current_col_file, 'save_error') else "Unknown error"
                QMessageBox.critical(self, "Save Error", f"Failed to save Obj file:\n{error_msg}")
                img_debugger.error(f"Save failed: {error_msg}")

        except Exception as e:
            img_debugger.error(f"Error saving file: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to save file:\n{str(e)}")


    def _save_file_as(self): #vers 1
        """Save As dialog"""
        try:
            file_path, _ = QFileDialog.getSaveFileName(
                self,
                "Save Obj File As",
                "",
                "Obj Files (*.col);;All Files (*)"
            )

            if file_path:
                self.current_file_path = file_path
                self.current_col_file.file_path = file_path
                self._save_file()

        except Exception as e:
            img_debugger.error(f"Error in save as dialog: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to save file:\n{str(e)}")


    def _load_settings(self): #vers 1
        """Load settings from config file"""
        import json

        settings_file = os.path.join(
            os.path.dirname(__file__),
            'obj_workshop_settings.json'
        )

        try:
            if os.path.exists(settings_file):
                with open(settings_file, 'r') as f:
                    settings = json.load(f)
                    self.save_to_source_location = settings.get('save_to_source_location', True)
                    self.last_save_directory = settings.get('last_save_directory', None)
        except Exception as e:
            print(f"Failed to load settings: {e}")


    def _save_settings(self): #vers 1
        """Save settings to config file"""
        import json

        settings_file = os.path.join(
            os.path.dirname(__file__),
            'obj_workshop_settings.json'
        )

        try:
            settings = {
                'save_to_source_location': self.save_to_source_location,
                'last_save_directory': self.last_save_directory
            }

            with open(settings_file, 'w') as f:
                json.dump(settings, indent=2, fp=f)
        except Exception as e:
            print(f"Failed to save settings: {e}")

# - Hotkeys sections

    def _setup_hotkeys(self): #vers 3
        """Setup Plasma6-style keyboard shortcuts for this application - checks for existing methods"""
        from PyQt6.QtGui import QShortcut, QKeySequence
        from PyQt6.QtCore import Qt

        # === FILE OPERATIONS ===

        # Open col (Ctrl+O)
        self.hotkey_open = QShortcut(QKeySequence.StandardKey.Open, self)
        if hasattr(self, 'open_col_file'):
            self.hotkey_open.activated.connect(self.open_col_file)
        elif hasattr(self, '_open_col_file'):
            self.hotkey_open.activated.connect(self._open_col_file)

        # Save col (Ctrl+S)
        self.hotkey_save = QShortcut(QKeySequence.StandardKey.Save, self)
        if hasattr(self, '_save_col_file'):
            self.hotkey_save.activated.connect(self._save_col_file)
        elif hasattr(self, 'save_col_file'):
            self.hotkey_save.activated.connect(self.save_col_file)

        # Force Save col (Alt+Shift+S)
        self.hotkey_force_save = QShortcut(QKeySequence("Alt+Shift+S"), self)
        if not hasattr(self, '_force_save_col'):
            # Create force save method inline if it doesn't exist
            def force_save():
                if not self.collision_list:
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.warning(self, "No Collision", "No Collision to save")
                    return
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Force save triggered (Alt+Shift+S)")
                # Call save regardless of modified state
                if hasattr(self, '_save_col_file'):
                    self._save_col_file()

            self.hotkey_force_save.activated.connect(force_save)
        else:
            self.hotkey_force_save.activated.connect(self._force_save_col)

        # Save As (Ctrl+Shift+S)
        self.hotkey_save_as = QShortcut(QKeySequence.StandardKey.SaveAs, self)
        if hasattr(self, '_save_as_col_file'):
            self.hotkey_save_as.activated.connect(self._save_as_col_file)
        elif hasattr(self, '_save_col_file'):
            self.hotkey_save_as.activated.connect(self._save_col_file)

        # Close (Ctrl+W)
        self.hotkey_close = QShortcut(QKeySequence.StandardKey.Close, self)
        self.hotkey_close.activated.connect(self.close)

        # === EDIT OPERATIONS ===

        # Undo (Ctrl+Z)
        self.hotkey_undo = QShortcut(QKeySequence.StandardKey.Undo, self)
        if hasattr(self, '_undo_last_action'):
            self.hotkey_undo.activated.connect(self._undo_last_action)
        # else: not implemented yet, no connection

        # Copy (Ctrl+C)
        self.hotkey_copy = QShortcut(QKeySequence.StandardKey.Copy, self)
        if hasattr(self, '_copy_collision'):
            self.hotkey_copy.activated.connect(self._copy_surface)


        # Paste (Ctrl+V)
        self.hotkey_paste = QShortcut(QKeySequence.StandardKey.Paste, self)
        if hasattr(self, '_paste_collision'):
            self.hotkey_paste.activated.connect(self._paste_surface)


        # Delete (Delete)
        self.hotkey_delete = QShortcut(QKeySequence.StandardKey.Delete, self)
        if hasattr(self, '_delete_collision'):
            self.hotkey_delete.activated.connect(self._delete_surface)


        # Duplicate (Ctrl+D)
        self.hotkey_duplicate = QShortcut(QKeySequence("Ctrl+D"), self)
        if hasattr(self, '_duplicate_collision'):
            self.hotkey_duplicate.activated.connect(self._duplicate_surface)


        # Rename (F2)
        self.hotkey_rename = QShortcut(QKeySequence("F2"), self)
        if not hasattr(self, '_rename_collsion_shortcut'):
            # Create rename shortcut method inline
            def rename_shortcut():
                # Focus the name input field if it exists
                if hasattr(self, 'info_name'):
                    self.info_name.setReadOnly(False)
                    self.info_name.selectAll()
                    self.info_name.setFocus()
            self.hotkey_rename.activated.connect(rename_shortcut)
        else:
            self.hotkey_rename.activated.connect(self._rename_shadow_shortcut)

        # === Collision OPERATIONS ===

        # Import Collision (Ctrl+I)
        self.hotkey_import = QShortcut(QKeySequence("Ctrl+I"), self)
        if hasattr(self, '_import_collision'):
            self.hotkey_import.activated.connect(self._import_surface)

        # Export Collision (Ctrl+E)
        self.hotkey_export = QShortcut(QKeySequence("Ctrl+E"), self)
        if hasattr(self, 'export_selected_collision'):
            self.hotkey_export.activated.connect(self.export_selected_surface)

        # Export All (Ctrl+Shift+E)
        self.hotkey_export_all = QShortcut(QKeySequence("Ctrl+Shift+E"), self)
        if hasattr(self, 'export_all_collision'):
            self.hotkey_export_all.activated.connect(self.export_all_surfaces)


        # === VIEW OPERATIONS ===

        # Refresh (F5)d_btn
        self.hotkey_refresh = QShortcut(QKeySequence.StandardKey.Refresh, self)
        if hasattr(self, '_reload_surface_table'):
            self.hotkey_refresh.activated.connect(self._reload_surface_table)
        elif hasattr(self, 'reload_surface_table'):
            self.hotkey_refresh.activated.connect(self.reload_surface_table)
        elif hasattr(self, 'refresh'):
            self.hotkey_refresh.activated.connect(self.refresh)

        # Properties (Alt+Enter)
        self.hotkey_properties = QShortcut(QKeySequence("Alt+Return"), self)
        if hasattr(self, '_show_detailed_info'):
            self.hotkey_properties.activated.connect(self._show_detailed_info)
        elif hasattr(self, '_show_surface_info'):
            self.hotkey_properties.activated.connect(self._show_surface_info)

        # Settings (Ctrl+,)
        self.hotkey_settings = QShortcut(QKeySequence.StandardKey.Preferences, self)
        if hasattr(self, '_show_settings_dialog'):
            self.hotkey_settings.activated.connect(self._show_settings_dialog)
        elif hasattr(self, 'show_settings_dialog'):
            self.hotkey_settings.activated.connect(self.show_settings_dialog)
        elif hasattr(self, '_show_settings_hotkeys'):
            self.hotkey_settings.activated.connect(self._show_settings_hotkeys)

        # === NAVIGATION ===

        # Select All (Ctrl+A) - reserved for future
        self.hotkey_select_all = QShortcut(QKeySequence.StandardKey.SelectAll, self)
        # Not connected - reserved for future multi-select

        # Find (Ctrl+F)
        self.hotkey_find = QShortcut(QKeySequence.StandardKey.Find, self)
        if not hasattr(self, '_focus_search'):
            # Create focus search method inline
            def focus_search():
                if hasattr(self, 'search_input'):
                    self.search_input.setFocus()
                    self.search_input.selectAll()
            self.hotkey_find.activated.connect(focus_search)
        else:
            self.hotkey_find.activated.connect(self._focus_search)

        # === HELP ===

        # Help (F1)
        self.hotkey_help = QShortcut(QKeySequence.StandardKey.HelpContents, self)

        if hasattr(self, 'show_help'):
            self.hotkey_help.activated.connect(self.show_help)

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Hotkeys initialized (Plasma6 standard)")


    def _reset_hotkeys_to_defaults(self, parent_dialog): #vers 1
        """Reset all hotkeys to Plasma6 defaults"""
        from PyQt6.QtWidgets import QMessageBox
        from PyQt6.QtGui import QKeySequence

        reply = QMessageBox.question(parent_dialog, "Reset Hotkeys",
            "Reset all keyboard shortcuts to Plasma6 defaults?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)

        if reply == QMessageBox.StandardButton.Yes:
            # Reset to defaults
            self.hotkey_edit_open.setKeySequence(QKeySequence.StandardKey.Open)
            self.hotkey_edit_save.setKeySequence(QKeySequence.StandardKey.Save)
            self.hotkey_edit_force_save.setKeySequence(QKeySequence("Alt+Shift+S"))
            self.hotkey_edit_save_as.setKeySequence(QKeySequence.StandardKey.SaveAs)
            self.hotkey_edit_close.setKeySequence(QKeySequence.StandardKey.Close)
            self.hotkey_edit_undo.setKeySequence(QKeySequence.StandardKey.Undo)
            self.hotkey_edit_copy.setKeySequence(QKeySequence.StandardKey.Copy)
            self.hotkey_edit_paste.setKeySequence(QKeySequence.StandardKey.Paste)
            self.hotkey_edit_delete.setKeySequence(QKeySequence.StandardKey.Delete)
            self.hotkey_edit_duplicate.setKeySequence(QKeySequence("Ctrl+D"))
            self.hotkey_edit_rename.setKeySequence(QKeySequence("F2"))
            self.hotkey_edit_import.setKeySequence(QKeySequence("Ctrl+I"))
            self.hotkey_edit_export.setKeySequence(QKeySequence("Ctrl+E"))
            self.hotkey_edit_export_all.setKeySequence(QKeySequence("Ctrl+Shift+E"))
            self.hotkey_edit_refresh.setKeySequence(QKeySequence.StandardKey.Refresh)
            self.hotkey_edit_properties.setKeySequence(QKeySequence("Alt+Return"))
            self.hotkey_edit_find.setKeySequence(QKeySequence.StandardKey.Find)
            self.hotkey_edit_help.setKeySequence(QKeySequence.StandardKey.HelpContents)


    def _apply_hotkey_settings(self, dialog, close=False): #vers 1
        """Apply hotkey changes"""
        # Update all hotkeys with new sequences
        self.hotkey_open.setKey(self.hotkey_edit_open.keySequence())
        self.hotkey_save.setKey(self.hotkey_edit_save.keySequence())
        self.hotkey_force_save.setKey(self.hotkey_edit_force_save.keySequence())
        self.hotkey_save_as.setKey(self.hotkey_edit_save_as.keySequence())
        self.hotkey_close.setKey(self.hotkey_edit_close.keySequence())
        self.hotkey_undo.setKey(self.hotkey_edit_undo.keySequence())
        self.hotkey_copy.setKey(self.hotkey_edit_copy.keySequence())
        self.hotkey_paste.setKey(self.hotkey_edit_paste.keySequence())
        self.hotkey_delete.setKey(self.hotkey_edit_delete.keySequence())
        self.hotkey_duplicate.setKey(self.hotkey_edit_duplicate.keySequence())
        self.hotkey_rename.setKey(self.hotkey_edit_rename.keySequence())
        self.hotkey_import.setKey(self.hotkey_edit_import.keySequence())
        self.hotkey_export.setKey(self.hotkey_edit_export.keySequence())
        self.hotkey_export_all.setKey(self.hotkey_edit_export_all.keySequence())
        self.hotkey_refresh.setKey(self.hotkey_edit_refresh.keySequence())
        self.hotkey_properties.setKey(self.hotkey_edit_properties.keySequence())
        self.hotkey_find.setKey(self.hotkey_edit_find.keySequence())
        self.hotkey_help.setKey(self.hotkey_edit_help.keySequence())

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Hotkeys updated")

        # TODO: Save to config file for persistence

        if close:
            dialog.accept()


    def _show_settings_hotkeys(self): #vers 1
        """Show settings dialog with hotkey customization"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                                    QWidget, QLabel, QLineEdit, QPushButton,
                                    QGroupBox, QFormLayout, QKeySequenceEdit)
        from PyQt6.QtCore import Qt

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        dialog.setMinimumWidth(600)
        dialog.setMinimumHeight(500)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # === HOTKEYS TAB ===
        hotkeys_tab = QWidget()
        hotkeys_layout = QVBoxLayout(hotkeys_tab)

        # File Operations Group
        file_group = QGroupBox("File Operations")
        file_form = QFormLayout()

        self.hotkey_edit_open = QKeySequenceEdit(self.hotkey_open.key())
        file_form.addRow("Open col:", self.hotkey_edit_open)

        self.hotkey_edit_save = QKeySequenceEdit(self.hotkey_save.key())
        file_form.addRow("Save col:", self.hotkey_edit_save)

        self.hotkey_edit_force_save = QKeySequenceEdit(self.hotkey_force_save.key())
        force_save_layout = QHBoxLayout()
        force_save_layout.addWidget(self.hotkey_edit_force_save)
        force_save_hint = QLabel("(Force save even if unmodified)")
        force_save_hint.setStyleSheet("color: #888; font-style: italic;")
        force_save_layout.addWidget(force_save_hint)
        file_form.addRow("Force Save:", force_save_layout)

        self.hotkey_edit_save_as = QKeySequenceEdit(self.hotkey_save_as.key())
        file_form.addRow("Save As:", self.hotkey_edit_save_as)

        self.hotkey_edit_close = QKeySequenceEdit(self.hotkey_close.key())
        file_form.addRow("Close:", self.hotkey_edit_close)

        file_group.setLayout(file_form)
        hotkeys_layout.addWidget(file_group)

        # Edit Operations Group
        edit_group = QGroupBox("Edit Operations")
        edit_form = QFormLayout()

        self.hotkey_edit_undo = QKeySequenceEdit(self.hotkey_undo.key())
        edit_form.addRow("Undo:", self.hotkey_edit_undo)

        self.hotkey_edit_copy = QKeySequenceEdit(self.hotkey_copy.key())
        edit_form.addRow("Copy Collision:", self.hotkey_edit_copy)

        self.hotkey_edit_paste = QKeySequenceEdit(self.hotkey_paste.key())
        edit_form.addRow("Paste Collision:", self.hotkey_edit_paste)

        self.hotkey_edit_delete = QKeySequenceEdit(self.hotkey_delete.key())
        edit_form.addRow("Delete:", self.hotkey_edit_delete)

        self.hotkey_edit_duplicate = QKeySequenceEdit(self.hotkey_duplicate.key())
        edit_form.addRow("Duplicate:", self.hotkey_edit_duplicate)

        self.hotkey_edit_rename = QKeySequenceEdit(self.hotkey_rename.key())
        edit_form.addRow("Rename:", self.hotkey_edit_rename)

        edit_group.setLayout(edit_form)
        hotkeys_layout.addWidget(edit_group)

        # Collision Group
        coll_group = QGroupBox("Collision Operations")
        coll_form = QFormLayout()

        self.hotkey_edit_import = QKeySequenceEdit(self.hotkey_import.key())
        coll_form.addRow("Import Collision:", self.hotkey_edit_import)

        self.hotkey_edit_export = QKeySequenceEdit(self.hotkey_export.key())
        coll_form.addRow("Export Collision:", self.hotkey_edit_export)

        self.hotkey_edit_export_all = QKeySequenceEdit(self.hotkey_export_all.key())
        coll_form.addRow("Export All:", self.hotkey_edit_export_all)

        coll_group.setLayout(coll_form)
        hotkeys_layout.addWidget(coll_group)

        # View Operations Group
        view_group = QGroupBox("View Operations")
        view_form = QFormLayout()

        self.hotkey_edit_refresh = QKeySequenceEdit(self.hotkey_refresh.key())
        view_form.addRow("Refresh:", self.hotkey_edit_refresh)

        self.hotkey_edit_properties = QKeySequenceEdit(self.hotkey_properties.key())
        view_form.addRow("Properties:", self.hotkey_edit_properties)

        self.hotkey_edit_find = QKeySequenceEdit(self.hotkey_find.key())
        view_form.addRow("Find/Search:", self.hotkey_edit_find)

        self.hotkey_edit_help = QKeySequenceEdit(self.hotkey_help.key())
        view_form.addRow("Help:", self.hotkey_edit_help)

        view_group.setLayout(view_form)
        hotkeys_layout.addWidget(view_group)

        hotkeys_layout.addStretch()

        # Reset to defaults button
        reset_hotkeys_btn = QPushButton("Reset to Plasma6 Defaults")
        reset_hotkeys_btn.clicked.connect(lambda: self._reset_hotkeys_to_defaults(dialog))
        hotkeys_layout.addWidget(reset_hotkeys_btn)

        tabs.addTab(hotkeys_tab, "Keyboard Shortcuts")

        # === GENERAL TAB (for future settings) ===
        general_tab = QWidget()
        general_layout = QVBoxLayout(general_tab)

        placeholder_label = QLabel("Additional settings will appear here in future versions.")
        placeholder_label.setStyleSheet("color: #888; font-style: italic; padding: 20px;")
        placeholder_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        general_layout.addWidget(placeholder_label)
        general_layout.addStretch()

        tabs.addTab(general_tab, "General")

        layout.addWidget(tabs)

        # Dialog buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(lambda: self._apply_hotkey_settings(dialog))
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(lambda: self._apply_hotkey_settings(dialog, close=True))
        button_layout.addWidget(ok_btn)

        layout.addLayout(button_layout)

        dialog.exec()

# - GUI Events, MouseButton interactions

    def keyPressEvent(self, event): #vers 1
        """Handle keyboard shortcuts"""
        from PyQt6.QtCore import Qt

        # D key - Dock/Undock toggle
        if event.key() == Qt.Key.Key_D and not event.modifiers():
            self.toggle_dock_mode()
            event.accept()
            return

        # T key - Tear out (same as undock)
        if event.key() == Qt.Key.Key_T and not event.modifiers():
            if self.is_docked:
                self._undock_from_main()
            event.accept()
            return

        super().keyPressEvent(event)


    def _enable_move_mode(self): #vers 2
        # Enable move window mode using system move
        # Use Qt's system move which works on Windows, Linux, etc.
        if hasattr(self.windowHandle(), 'startSystemMove'):
            self.windowHandle().startSystemMove()
        else:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "Move Window",
                "Drag the titlebar to move the window")


    def _toggle_upscale_native(self): #vers 1
        # Toggle upscale native resolution
        # Placeholder for upscale native functionality
        print("Upscale Native toggled")


    def _is_on_draggable_area(self, pos): #vers 7
        """Check if position is on draggable titlebar area

        Args:
            pos: Position in titlebar coordinates (from eventFilter)

        Returns:
            True if position is on titlebar but not on any button
        """
        if not hasattr(self, 'titlebar'):
            print("[DRAG] No titlebar attribute"); return False

        # Verify pos is within titlebar bounds
        if not self.titlebar.rect().contains(pos):
            print(f"[DRAG] Position {pos} outside titlebar rect {self.titlebar.rect()}")
            return False

        # Check if clicking on any button - if so, NOT draggable
        for widget in self.titlebar.findChildren(QPushButton):
            if widget.isVisible():
                # Get button geometry in titlebar coordinates
                button_rect = widget.geometry()
                if button_rect.contains(pos):
                    print(f"[DRAG] Clicked on button: {widget.toolTip()}")
                    return False

        # Not on any button = draggable
        print(f"[DRAG] On draggable area at {pos}")
        return True


    def _apply_always_on_top(self): #vers 1
        """Apply always on top window flag"""
        current_flags = self.windowFlags()

        if self.window_always_on_top:
            new_flags = current_flags | Qt.WindowType.WindowStaysOnTopHint
        else:
            new_flags = current_flags & ~Qt.WindowType.WindowStaysOnTopHint

        if new_flags != current_flags:
            # Save state
            current_geometry = self.geometry()
            was_visible = self.isVisible()

            self.setWindowFlags(new_flags)

            self.setGeometry(current_geometry)
            if was_visible:
                self.show()


    def keyPressEvent(self, event): #ver 1
        if event.key() == Qt.Key.Key_D and not event.modifiers():
            self.toggle_dock_mode(); event.accept(); return

        if event.key() == Qt.Key.Key_T and not event.modifiers():
            if self.is_docked: self._undock_from_main(); event.accept(); return

        super().keyPressEvent(event)


    def paintEvent(self, event): #vers 2
        """Paint corner resize triangles"""
        super().paintEvent(event)

        from PyQt6.QtGui import QPainter, QColor, QPen, QBrush, QPainterPath

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Colors
        normal_color = QColor(100, 100, 100, 150)
        hover_color = QColor(150, 150, 255, 200)

        w = self.width()
        h = self.height()
        grip_size = 8  # Make corners visible (8x8px)
        size = self.corner_size

        # Define corner triangles
        corners = {
            'top-left': [(0, 0), (size, 0), (0, size)],
            'top-right': [(w, 0), (w-size, 0), (w, size)],
            'bottom-left': [(0, h), (size, h), (0, h-size)],
            'bottom-right': [(w, h), (w-size, h), (w, h-size)]
        }
        corners2 = {
            "top-left": [(0, grip_size), (0, 0), (grip_size, 0)],
            "top-right": [(w-grip_size, 0), (w, 0), (w, grip_size)],
            "bottom-left": [(0, h-grip_size), (0, h), (grip_size, h)],
            "bottom-right": [(w-grip_size, h), (w, h), (w, h-grip_size)]
        }

        # Get theme colors for corner indicators
        if self.app_settings:
            theme_colors = self._get_theme_colors("default")
            accent_color = QColor(theme_colors.get('accent_primary', '#1976d2'))
            accent_color.setAlpha(180)
        else:
            accent_color = QColor(100, 150, 255, 180)

        hover_color = QColor(accent_color)
        hover_color.setAlpha(255)

        # Draw all corners with hover effect
        for corner_name, points in corners.items():
            path = QPainterPath()
            path.moveTo(points[0][0], points[0][1])
            path.lineTo(points[1][0], points[1][1])
            path.lineTo(points[2][0], points[2][1])
            path.closeSubpath()

            # Use hover color if mouse is over this corner
            color = hover_color if self.hover_corner == corner_name else accent_color

            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(color))
            painter.drawPath(path)

        painter.end()


    def _get_resize_corner(self, pos): #vers 3
        """Determine which corner is under mouse position"""
        size = self.corner_size; w = self.width(); h = self.height()

        if pos.x() < size and pos.y() < size:
            return "top-left"
        if pos.x() > w - size and pos.y() < size:
            return "top-right"
        if pos.x() < size and pos.y() > h - size:
            return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:
            return "bottom-right"

        return None

    def mouseMoveEvent(self, event): #vers 4
        """Handle mouse move for resizing and hover effects

        Window dragging is handled by eventFilter to avoid conflicts
        """
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept()
                return
        else:
            # Update hover state and cursor
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()  # Trigger repaint for hover effect
            self._update_cursor(corner)

        # Let parent handle everything else
        super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event): #vers 2
        """Handle mouse release"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = False
            self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    # mousePressEvent with this logic:
    def mousePressEvent(self, event): #vers 8
        """Handle ALL mouse press - dragging and resizing"""
        if event.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(event)
            return

        pos = event.pos()

        # Check corner resize FIRST
        self.resize_corner = self._get_resize_corner(pos)
        if self.resize_corner:
            self.resizing = True
            self.drag_position = event.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            event.accept()
            return

        # Check if on titlebar
        if hasattr(self, 'titlebar') and self.titlebar.geometry().contains(pos):
            titlebar_pos = self.titlebar.mapFromParent(pos)
            if self._is_on_draggable_area(titlebar_pos):
                self.windowHandle().startSystemMove()
                event.accept()
                return

        super().mousePressEvent(event)


    def _handle_corner_resize(self, global_pos): #vers 2
        """Handle window resizing from corners"""
        if not self.resize_corner or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.initial_geometry

        min_width = 800
        min_height = 600

        # Calculate new geometry based on corner
        if self.resize_corner == "top-left":
            # Move top-left corner
            new_x = geometry.x() + delta.x()
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() - delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, new_y, new_width, new_height)

        elif self.resize_corner == "top-right":
            # Move top-right corner
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() - delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(geometry.x(), new_y, new_width, new_height)

        elif self.resize_corner == "bottom-left":
            # Move bottom-left corner
            new_x = geometry.x() + delta.x()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() + delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, geometry.y(), new_width, new_height)

        elif self.resize_corner == "bottom-right":
            # Move bottom-right corner
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() + delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.resize(new_width, new_height)


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


    def _update_cursor(self, direction): #vers 1
        """Update cursor based on resize direction"""
        if direction == "top" or direction == "bottom":
            self.setCursor(Qt.CursorShape.SizeVerCursor)
        elif direction == "left" or direction == "right":
            self.setCursor(Qt.CursorShape.SizeHorCursor)
        elif direction == "top-left" or direction == "bottom-right":
            self.setCursor(Qt.CursorShape.SizeFDiagCursor)
        elif direction == "top-right" or direction == "bottom-left":
            self.setCursor(Qt.CursorShape.SizeBDiagCursor)
        else:
            self.setCursor(Qt.CursorShape.ArrowCursor)


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


    def resizeEvent(self, event): #vers 1
        '''Keep resize grip in bottom-right corner'''
        super().resizeEvent(event)
        if hasattr(self, 'size_grip'):
            self.size_grip.move(self.width() - 16, self.height() - 16)


    def mouseDoubleClickEvent(self, event): #vers 2
        """Handle double-click - maximize/restore

        Handled here instead of eventFilter for better control
        """
        if event.button() == Qt.MouseButton.LeftButton:
            # Convert to titlebar coordinates if needed
            if hasattr(self, 'titlebar'):
                titlebar_pos = self.titlebar.mapFromParent(event.pos())
                if self._is_on_draggable_area(titlebar_pos):
                    self._toggle_maximize()
                    event.accept()
                    return

        super().mouseDoubleClickEvent(event)




    def _toggle_maximize(self): #vers 1
        #Toggle window maximize state
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()


    def closeEvent(self, event): #ver 1
        self.window_closed.emit()
        event.accept()


# - SVG icons should be in there own folder, example depends/svg_icons.py

    def _set_checkerboard_bg(self): #vers 1
        """Set checkerboard background"""
        # Create checkerboard pattern
        self.preview_widget.setStyleSheet("""
            border: 1px solid #3a3a3a;
            background-image:
                linear-gradient(45deg, #333 25%, transparent 25%),
                linear-gradient(-45deg, #333 25%, transparent 25%),
                linear-gradient(45deg, transparent 75%, #333 75%),
                linear-gradient(-45deg, transparent 75%, #333 75%);
            background-size: 20px 20px;
            background-position: 0 0, 0 10px, 10px -10px, -10px 0px;
        """)


    def _create_bitdepth_icon(self): #vers 3
        """Create bit depth icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M3,5H9V11H3V5M5,7V9H7V7H5M11,7H21V9H11V7M11,15H21V17H11V15M5,20L1.5,16.5L2.91,15.09L5,17.17L9.59,12.59L11,14L5,20Z"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_resize_icon(self): #vers 2
        """Create resize icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M10,21V19H6.41L10.91,14.5L9.5,13.09L5,17.59V14H3V21H10M14.5,10.91L19,6.41V10H21V3H14V5H17.59L13.09,9.5L14.5,10.91Z"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_warning_icon_svg(self): #vers 1
        """Create SVG warning icon for table display"""
        svg_data = b"""
        <svg width="16" height="16" viewBox="0 0 16 16">
            <path fill="#FFA500" d="M8 1l7 13H1z"/>
            <text x="8" y="12" font-size="10" fill="black" text-anchor="middle">!</text>
        </svg>
        """
        return QIcon(QPixmap.fromImage(
            QImage.fromData(QByteArray(svg_data))
        ))


    def _create_resize_icon2(self): #vers 1
        """Resize grip icon - diagonal arrows"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M14 6l-8 8M10 6h4v4M6 14v-4h4" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_upscale_icon(self): #vers 3
        """Create AI upscale icon - brain/intelligence style"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <!-- Brain outline -->
            <path d="M12 3 C8 3 5 6 5 9 C5 10 5.5 11 6 12 C5.5 13 5 14 5 15 C5 18 8 21 12 21 C16 21 19 18 19 15 C19 14 18.5 13 18 12 C18.5 11 19 10 19 9 C19 6 16 3 12 3 Z"
                fill="none" stroke="currentColor" stroke-width="1.5"/>

            <!-- Neural pathways inside -->
            <path d="M9 8 L10 10 M14 8 L13 10 M10 12 L14 12 M9 14 L12 16 M15 14 L12 16"
                stroke="currentColor" stroke-width="1" fill="none"/>

            <!-- Upward indicator -->
            <path d="M19 8 L19 4 M17 6 L19 4 L21 6"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_upscale_icon(self): #vers 3
        """Create AI upscale icon - sparkle/magic AI style"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <!-- Large sparkle -->
            <path d="M12 2 L13 8 L12 14 L11 8 Z M8 12 L2 11 L8 10 L14 11 Z"
                fill="currentColor"/>

            <!-- Small sparkles -->
            <circle cx="18" cy="6" r="1.5" fill="currentColor"/>
            <circle cx="6" cy="18" r="1.5" fill="currentColor"/>
            <circle cx="19" cy="16" r="1" fill="currentColor"/>

            <!-- Upward arrow -->
            <path d="M16 20 L20 20 M18 18 L18 22"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_upscale_icon(self): #vers 3
        """Create AI upscale icon - neural network style"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <!-- Neural network nodes -->
            <circle cx="6" cy="6" r="2" fill="currentColor"/>
            <circle cx="18" cy="6" r="2" fill="currentColor"/>
            <circle cx="6" cy="18" r="2" fill="currentColor"/>
            <circle cx="18" cy="18" r="2" fill="currentColor"/>
            <circle cx="12" cy="12" r="2.5" fill="currentColor"/>

            <!-- Connecting lines -->
            <path d="M7.5 7.5 L10.5 10.5 M13.5 10.5 L16.5 7.5 M7.5 16.5 L10.5 13.5 M13.5 13.5 L16.5 16.5"
                stroke="currentColor" stroke-width="1.5" fill="none"/>

            <!-- Upward arrow overlay -->
            <path d="M12 3 L12 9 M9 6 L12 3 L15 6"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_manage_icon(self): #vers 1
        """Create manage/settings icon for bumpmap manager"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <!-- Gear/cog icon for management -->
            <circle cx="12" cy="12" r="3" stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M12 1v4M12 19v4M4.22 4.22l2.83 2.83M16.95 16.95l2.83 2.83M1 12h4M19 12h4M4.22 19.78l2.83-2.83M16.95 7.05l2.83-2.83"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_paint_icon(self): #vers 1
        """Create paint brush icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M20.71 7.04c.39-.39.39-1.04 0-1.41l-2.34-2.34c-.37-.39-1.02-.39-1.41 0l-1.84 1.83 3.75 3.75M3 17.25V21h3.75L17.81 9.93l-3.75-3.75L3 17.25z"
                fill="currentColor"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_compress_icon(self): #vers 2
        """Create compress icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M4,2H20V4H13V10H20V12H4V10H11V4H4V2M4,13H20V15H13V21H20V23H4V21H11V15H4V13Z"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_build_icon(self): #vers 1
        """Create build/construct icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M22,9 L12,2 L2,9 L12,16 L22,9 Z M12,18 L4,13 L4,19 L12,24 L20,19 L20,13 L12,18 Z"
                fill="currentColor"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_uncompress_icon(self): #vers 2
        """Create uncompress icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path fill="currentColor" d="M11,4V2H13V4H11M13,21V19H11V21H13M4,12V10H20V12H4Z"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_view_icon(self): #vers 2
        """Create view/eye icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M12,9A3,3 0 0,0 9,12A3,3 0 0,0 12,15A3,3 0 0,0 15,12A3,3 0 0,0 12,9
                    M12,17A5,5 0 0,1 7,12A5,5 0 0,1 12,7A5,5 0 0,1 17,12A5,5 0 0,1 12,17
                    M12,4.5C7,4.5 2.73,7.61 1,12C2.73,16.39 7,19.5 12,19.5
                    C17,19.5 21.27,16.39 23,12
                    C21.27,7.61 17,4.5 12,4.5Z"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_add_icon(self): #vers 2
        """Create add/plus icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path fill="currentColor" d="M19,13H13V19H11V13H5V11H11V5H13V11H19V13Z"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_delete_icon(self): #vers 2
        """Create delete/minus icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path fill="currentColor" d="M19,13H5V11H19V13Z"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_color_picker_icon(self): #vers 1
        """Color picker icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <circle cx="10" cy="10" r="7" stroke="currentColor" stroke-width="2"/>
            <path d="M10 3v4M10 13v4M3 10h4M13 10h4" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_zoom_in_icon(self): #vers 1
        """Zoom in icon (+)"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <circle cx="8" cy="8" r="6" stroke="currentColor" stroke-width="2"/>
            <path d="M8 5v6M5 8h6" stroke="currentColor" stroke-width="2"/>
            <path d="M13 13l4 4" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_zoom_out_icon(self): #vers 1
        """Zoom out icon (-)"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <circle cx="8" cy="8" r="6" stroke="currentColor" stroke-width="2"/>
            <path d="M5 8h6" stroke="currentColor" stroke-width="2"/>
            <path d="M13 13l4 4" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_reset_icon(self): #vers 1
        """Reset/1:1 icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M16 10A6 6 0 1 1 4 10M4 10l3-3m-3 3l3 3" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_fit_icon(self): #vers 1
        """Fit to window icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <rect x="3" y="3" width="14" height="14" stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M7 7l6 6M13 7l-6 6" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_arrow_up_icon(self): #vers 1
        """Arrow up"""
        svg_data = b'''<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M8 3v10M4 7l4-4 4 4" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=16)


    def _create_arrow_down_icon(self): #vers 1
        """Arrow down"""
        svg_data = b'''<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M8 13V3M12 9l-4 4-4-4" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=16)


    def _create_arrow_left_icon(self): #vers 1
        """Arrow left"""
        svg_data = b'''<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M3 8h10M7 4L3 8l4 4" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=16)


    def _create_arrow_right_icon(self): #vers 1
        """Arrow right"""
        svg_data = b'''<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M13 8H3M9 12l4-4-4-4" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=16)


    def _create_flip_vert_icon(self): #vers 1
        """Create vertical flip SVG icon"""
        from PyQt6.QtGui import QIcon, QPixmap, QPainter
        from PyQt6.QtSvg import QSvgRenderer

        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M3 12h18M8 7l-4 5 4 5M16 7l4 5-4 5" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''

        return self._svg_to_icon(svg_data)


    def _create_flip_horz_icon(self): #vers 1
        """Create horizontal flip SVG icon"""
        from PyQt6.QtGui import QIcon, QPixmap, QPainter
        from PyQt6.QtSvg import QSvgRenderer

        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M12 3v18M7 8l5-4 5 4M7 16l5 4 5-4" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''

        return self._svg_to_icon(svg_data)


    def _create_rotate_cw_icon(self): #vers 1
        """Create clockwise rotation SVG icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M21 12a9 9 0 11-9-9v6M21 3l-3 6-6-3" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''

        return self._svg_to_icon(svg_data)


    def _create_rotate_ccw_icon(self): #vers 1
        """Create counter-clockwise rotation SVG icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M3 12a9 9 0 109-9v6M3 3l3 6 6-3" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''

        return self._svg_to_icon(svg_data)


    def _create_copy_icon(self): #vers 1
        """Create copy SVG icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <rect x="9" y="9" width="13" height="13" rx="2" stroke="currentColor" stroke-width="2"/>
            <path d="M5 15H4a2 2 0 01-2-2V4a2 2 0 012-2h9a2 2 0 012 2v1" stroke="currentColor" stroke-width="2"/>
        </svg>'''

        return self._svg_to_icon(svg_data)


    def _create_paste_icon(self): #vers 1
        """Create paste SVG icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M16 4h2a2 2 0 012 2v14a2 2 0 01-2 2H6a2 2 0 01-2-2V6a2 2 0 012-2h2" stroke="currentColor" stroke-width="2"/>
            <rect x="8" y="2" width="8" height="4" rx="1" stroke="currentColor" stroke-width="2"/>
        </svg>'''

        return self._svg_to_icon(svg_data)


    def _create_edit_icon(self): #vers 1
        """Create edit SVG icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M11 4H4a2 2 0 00-2 2v14a2 2 0 002 2h14a2 2 0 002-2v-7" stroke="currentColor" stroke-width="2"/>
            <path d="M18.5 2.5a2.121 2.121 0 013 3L12 15l-4 1 1-4 9.5-9.5z" stroke="currentColor" stroke-width="2"/>
        </svg>'''

        return self._svg_to_icon(svg_data)


    def _create_convert_icon(self): #vers 1
        """Create convert SVG icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M3 12h18M3 12l4-4M3 12l4 4M21 12l-4-4M21 12l-4 4" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <circle cx="12" cy="12" r="3" stroke="currentColor" stroke-width="2"/>
        </svg>'''

        return self._svg_to_icon(svg_data)


    def _create_undo_icon(self): #vers 2
        """Undo - Curved arrow icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M3 7v6h6M3 13a9 9 0 1018 0 9 9 0 00-18 0z"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_info_icon(self): #vers 1
        """Info - circle with 'i' icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <circle cx="12" cy="12" r="9" stroke="currentColor" stroke-width="2"/>
            <path d="M12 11v6M12 8v.5" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_folder_icon(self): #vers 1
        """Open IMG - Folder icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-7l-2-2H5a2 2 0 00-2 2z" stroke="currentColor" stroke-width="2" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_file_icon(self): #vers 1
        """Open col - File icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z" stroke="currentColor" stroke-width="2"/>
            <path d="M14 2v6h6" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_open_icon(self): #vers 1
        """Open - Folder icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M3 7v13a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_save_icon(self): #vers 1
        """Save col - Floppy disk icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M19 21H5a2 2 0 01-2-2V5a2 2 0 012-2h11l5 5v11a2 2 0 01-2 2z" stroke="currentColor" stroke-width="2"/>
            <path d="M17 21v-8H7v8M7 3v5h8" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_import_icon(self): #vers 1
        """Import - Download/Import icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4M7 10l5 5 5-5M12 15V3" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_export_icon(self): #vers 1
        """Export - Upload/Export icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4M17 8l-5-5-5 5M12 3v12" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_saveas_icon(self): #vers 1
        """Save - Floppy disk icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M19 21H5a2 2 0 01-2-2V5a2 2 0 012-2h11l5 5v11a2 2 0 01-2 2z"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="17 21 17 13 7 13 7 21"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="7 3 7 8 15 8"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_analyze_icon(self): #vers 1
        """Analyze - Bar chart icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <line x1="18" y1="20" x2="18" y2="10"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="12" y1="20" x2="12" y2="4"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="6" y1="20" x2="6" y2="14"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_sphere_icon(self): #vers 1
        """Sphere - Circle icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="10"
                stroke="currentColor" stroke-width="2"
                fill="none"/>
            <path d="M2 12h20M12 2a15.3 15.3 0 014 10 15.3 15.3 0 01-4 10 15.3 15.3 0 01-4-10 15.3 15.3 0 014-10z"
                stroke="currentColor" stroke-width="2"
                fill="none"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_box_icon(self): #vers 1
        """Box - Cube icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M21 16V8a2 2 0 00-1-1.73l-7-4a2 2 0 00-2 0l-7 4A2 2 0 003 8v8a2 2 0 001 1.73l7 4a2 2 0 002 0l7-4A2 2 0 0021 16z"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="3.27 6.96 12 12.01 20.73 6.96"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="22.08" x2="12" y2="12"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_mesh_icon(self): #vers 1
        """Mesh - Grid/wireframe icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <rect x="3" y="3" width="18" height="18"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="3" y1="9" x2="21" y2="9"
                stroke="currentColor" stroke-width="2"/>
            <line x1="3" y1="15" x2="21" y2="15"
                stroke="currentColor" stroke-width="2"/>
            <line x1="9" y1="3" x2="9" y2="21"
                stroke="currentColor" stroke-width="2"/>
            <line x1="15" y1="3" x2="15" y2="21"
                stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_package_icon(self): #vers 1
        """Export All - Package/Box icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M21 16V8a2 2 0 00-1-1.73l-7-4a2 2 0 00-2 0l-7 4A2 2 0 003 8v8a2 2 0 001 1.73l7 4a2 2 0 002 0l7-4A2 2 0 0021 16z" stroke="currentColor" stroke-width="2"/>
            <path d="M3.27 6.96L12 12.01l8.73-5.05M12 22.08V12" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_properties_icon(self): #vers 1
        """Properties - Info/Details icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <circle cx="12" cy="12" r="10" stroke="currentColor" stroke-width="2"/>
            <path d="M12 16v-4M12 8h.01" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    # CONTEXT MENU ICONS

    def _create_plus_icon(self): #vers 1
        """Create New Entry - Plus icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <circle cx="12" cy="12" r="10" stroke="currentColor" stroke-width="2"/>
            <path d="M12 8v8M8 12h8" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_document_icon(self): #vers 1
        """Create New col - Document icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z" stroke="currentColor" stroke-width="2"/>
            <path d="M14 2v6h6M16 13H8M16 17H8M10 9H8" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_filter_icon(self): #vers 1
        """Filter/sliders icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <circle cx="6" cy="4" r="2" fill="currentColor"/>
            <rect x="5" y="8" width="2" height="8" fill="currentColor"/>
            <circle cx="14" cy="12" r="2" fill="currentColor"/>
            <rect x="13" y="4" width="2" height="6" fill="currentColor"/>
            <circle cx="10" cy="8" r="2" fill="currentColor"/>
            <rect x="9" y="12" width="2" height="4" fill="currentColor"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_add_icon(self): #vers 1
        """Add/plus icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M10 4v12M4 10h12" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_trash_icon(self): #vers 1
        """Delete/trash icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M3 5h14M8 5V3h4v2M6 5v11a1 1 0 001 1h6a1 1 0 001-1V5" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_filter_icon(self): #vers 1
        """Filter/sliders icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <circle cx="6" cy="4" r="2" fill="currentColor"/>
            <rect x="5" y="8" width="2" height="8" fill="currentColor"/>
            <circle cx="14" cy="12" r="2" fill="currentColor"/>
            <rect x="13" y="4" width="2" height="6" fill="currentColor"/>
            <circle cx="10" cy="8" r="2" fill="currentColor"/>
            <rect x="9" y="12" width="2" height="4" fill="currentColor"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_delete_icon(self): #vers 1
        """Delete/trash icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M3 5h14M8 5V3h4v2M6 5v11a1 1 0 001 1h6a1 1 0 001-1V5" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_duplicate_icon(self): #vers 1
        """Duplicate/copy icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <rect x="6" y="6" width="10" height="10" stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M4 4h8v2H6v8H4V4z" fill="currentColor"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_create_icon(self): #vers 1
        """Create/new icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M10 4v12M4 10h12" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_filter_icon(self): #vers 1
        """Filter/sliders icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <circle cx="6" cy="4" r="2" fill="currentColor"/>
            <rect x="5" y="8" width="2" height="8" fill="currentColor"/>
            <circle cx="14" cy="12" r="2" fill="currentColor"/>
            <rect x="13" y="4" width="2" height="6" fill="currentColor"/>
            <circle cx="10" cy="8" r="2" fill="currentColor"/>
            <rect x="9" y="12" width="2" height="4" fill="currentColor"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_pencil_icon(self): #vers 1
        """Edit - Pencil icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M17 3a2.83 2.83 0 114 4L7.5 20.5 2 22l1.5-5.5L17 3z" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_trash_icon(self): #vers 1
        """Delete - Trash icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M3 6h18M19 6v14a2 2 0 01-2 2H7a2 2 0 01-2-2V6M8 6V4a2 2 0 012-2h4a2 2 0 012 2v2" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_check_icon(self): #vers 2
        """Create check/verify icon - document with checkmark"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z"
                fill="none" stroke="currentColor" stroke-width="2"/>
            <path d="M14 2v6h6"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M9 13l2 2 4-4"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_eye_icon(self): #vers 1
        """View - Eye icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z" stroke="currentColor" stroke-width="2"/>
            <circle cx="12" cy="12" r="3" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_list_icon(self): #vers 1
        """Properties List - List icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M8 6h13M8 12h13M8 18h13M3 6h.01M3 12h.01M3 18h.01" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    # WINDOW CONTROL ICONS

    def _create_minimize_icon(self): #vers 1
        """Minimize - Horizontal line"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M5 12h14" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_maximize_icon(self): #vers 1
        """Maximize - Square"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <rect x="3" y="3" width="18" height="18" rx="2" stroke="currentColor" strokea-width="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_close_icon(self): #vers 1
        """Close - X icon"""
        svg_data = b'''<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M18 6L6 18M6 6l12 12" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data)


    def _create_settings_icon(self): #vers 1
        """Settings/gear icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <circle cx="10" cy="10" r="3" stroke="currentColor" stroke-width="2"/>
            <path d="M10 2v2M10 16v2M2 10h2M16 10h2M4.93 4.93l1.41 1.41M13.66 13.66l1.41 1.41M4.93 15.07l1.41-1.41M13.66 6.34l1.41-1.41" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_minimize_icon(self): #vers 1
        """Minimize - Horizontal line icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <line x1="5" y1="12" x2="19" y2="12"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_maximize_icon(self): #vers 1
        """Maximize - Square icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <rect x="5" y="5" width="14" height="14"
                stroke="currentColor" stroke-width="2"
                fill="none" rx="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_close_icon(self): #vers 1
        """Close - X icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <line x1="6" y1="6" x2="18" y2="18"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
            <line x1="18" y1="6" x2="6" y2="18"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_add_icon(self): #vers 1
        """Add - Plus icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <line x1="12" y1="5" x2="12" y2="19"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
            <line x1="5" y1="12" x2="19" y2="12"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_delete_icon(self): #vers 1
        """Delete - Trash icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <polyline points="3 6 5 6 21 6"
                    stroke="currentColor" stroke-width="2"
                    fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M19 6v14a2 2 0 01-2 2H7a2 2 0 01-2-2V6m3 0V4a2 2 0 012-2h4a2 2 0 012 2v2"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_import_icon(self): #vers 1
        """Import - Download arrow icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="7 10 12 15 17 10"
                    stroke="currentColor" stroke-width="2"
                    fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="15" x2="12" y2="3"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_export_icon(self): #vers 1
        """Export - Upload arrow icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="17 8 12 3 7 8"
                    stroke="currentColor" stroke-width="2"
                    fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="3" x2="12" y2="15"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_checkerboard_icon(self): #vers 1
        """Create checkerboard pattern icon"""
        svg_data = b'''<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
            <rect x="0" y="0" width="5" height="5" fill="currentColor"/>
            <rect x="5" y="5" width="5" height="5" fill="currentColor"/>
            <rect x="10" y="0" width="5" height="5" fill="currentColor"/>
            <rect x="15" y="5" width="5" height="5" fill="currentColor"/>
            <rect x="0" y="10" width="5" height="5" fill="currentColor"/>
            <rect x="5" y="15" width="5" height="5" fill="currentColor"/>
            <rect x="10" y="10" width="5" height="5" fill="currentColor"/>
            <rect x="15" y="15" width="5" height="5" fill="currentColor"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _create_undo_icon(self): #vers 2
        """Undo - Curved arrow icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M3 7v6h6M3 13a9 9 0 1018 0 9 9 0 00-18 0z"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


    def _svg_to_icon(self, svg_data, size=24): #vers 2
        """Convert SVG data to QIcon with theme color support"""
        from PyQt6.QtGui import QIcon, QPixmap, QPainter, QColor
        from PyQt6.QtSvg import QSvgRenderer
        from PyQt6.QtCore import QByteArray

        try:
            # Get current text color from palette
            text_color = self.palette().color(self.foregroundRole())

            # Replace currentColor with actual color
            svg_str = svg_data.decode('utf-8')
            svg_str = svg_str.replace('currentColor', text_color.name())
            svg_data = svg_str.encode('utf-8')

            renderer = QSvgRenderer(QByteArray(svg_data))
            pixmap = QPixmap(size, size)
            pixmap.fill(QColor(0, 0, 0, 0))  # Transparent background

            painter = QPainter(pixmap)
            renderer.render(painter)
            painter.end()

            return QIcon(pixmap)
        except:
            # Fallback to no icon if SVG fails
            return QIcon()


# - External AI upscaler integration helper
import subprocess
import tempfile
import shutil
import sys


def open_workshop(main_window, img_path=None): #vers 3
    """Open Workshop from main window - works with or without IMG"""
    try:
        workshop = COLWorkshop(main_window, main_window)

        if img_path:
            # Check if it's a col file or IMG file
            if img_path.lower().endswith('.col'):
                # Load standalone col file
                workshop.open_col_file(img_path)
            else:
                # Load from IMG archive
                workshop.load_from_img_archive(img_path)
        else:
            # Open in standalone mode (no IMG loaded)
            if main_window and hasattr(main_window, 'log_message'):
                main_window.log_message(App_name + " opened in standalone mode")

        workshop.show()
        return workshop
    except Exception as e:
        QMessageBox.critical(main_window, App_name * " Error", f"Failed to open: {str(e)}")
        return None


def apply_changes(main_window) -> bool: #vers 1
    """Apply all pending changes"""
    try:
        # TODO: Implement change application
        img_debugger.info("Apply changes - not yet implemented")
        return True

    except Exception as e:
        img_debugger.error(f"Error applying changes: {str(e)}")
        return False


# Convenience stubs
def open_workshop(main_window, img_path=None): #vers 1
    workshop = COLWorkshop(main_window, main_window)
    workshop.show()
    return workshop

if __name__ == "__main__":
    import sys
    import traceback

    print(App_name + " Starting.")

    try:
        app = QApplication(sys.argv)
        print("QApplication created")

        workshop = GUIWorkshop()
        print(App_name + " instance created")

        workshop.setWindowTitle(App_name + " - Standalone")
        workshop.resize(1200, 800)
        workshop.show()
        print("Window shown, entering event loop")
        print(f"Window visible: {workshop.isVisible()}")
        print(f"Window geometry: {workshop.geometry()}")

        sys.exit(app.exec())

    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)
