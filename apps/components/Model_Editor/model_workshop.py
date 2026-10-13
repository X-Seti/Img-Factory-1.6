#!/usr/bin/env python3
#this belongs in apps/components/Model_Editor/model_workshop.py - Version: 218
# X-Seti - Apr 2026 - Model Workshop (based on COL Workshop)
# [FIX] _make_slot_pix crash: imported QPolygonF into local scope.
# [FIX] Material Editor cube preview crash: added missing QPolygonF import to _open_dff_material_list scope.
# [FIX] _rebuild_grid QWidget crash: removed redundant deleteLater (QScrollArea auto-deletes old widget).
# [FIX] _rebuild_grid QFrame deletion crash: reparent slots before scroll widget swap.
# X-Seti - May08 2026 - Model editor
# X-Seti - Jul07 2026 - Added 3ds Max style 4-pane viewport (Top/Front/Side/Perspective) via QStackedWidget central widget; user-assignable per pane by right-click; splitter-resizable; layout persists to model_workshop.json.
# X-Seti - Jul11 2026 - Diagnostic rollback to pre-4-pane state then restored:
# black-window/QOpenGLWidget context failure confirmed via journalctl to be a
# hardware/driver issue (PCIe BadTLP errors + NVIDIA GSP firmware load failure
# on the GPU, starting Jul 10) - not caused by this file or dff_viewport.py.
# The existing QT_QPA_PLATFORM=xcb / QSG_RHI_BACKEND=opengl forcing below
# doesn't help since the GPU itself is failing at the hardware level.

import os
# Force X11/GLX backend for NVIDIA on Wayland
import sys
if sys.platform.startswith('linux'):   # xcb only exists on Linux
    os.environ['QT_QPA_PLATFORM'] = 'xcb'
    os.environ['QSG_RHI_BACKEND'] = 'opengl'
    os.environ['LIBGL_ALWAYS_SOFTWARE'] = '0'  # Use hardware acceleration

import sys


# Add project root to path for standalone mode
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

# Import PyQt6
from PyQt6.QtWidgets import (QApplication, QWidget, QVBoxLayout,
    QHBoxLayout, QSplitter, QListWidget, QDialog, QListWidgetItem, QLabel, QPushButton, QFrame,  QFileDialog, QLineEdit, QMessageBox, QTableWidget, QTableWidgetItem, QAbstractItemView, QMenu, QComboBox, QStyledItemDelegate
)

from PyQt6.QtCore import Qt, pyqtSignal, QSize, QPoint
from PyQt6.QtGui import QFont, QIcon

# Shared DFFViewport — import from methods/, fallback to local methods/
from apps.methods.ribbon_dialog import RibbonIconsMixin
from apps.methods.doc_tabs import DocTabsMixin
from apps.methods.dff_viewport import DFFViewport

try:
    from PyQt6.QtGui import QAction
except ImportError:
    from PyQt6.QtWidgets import QAction

# Import project modules AFTER path setup
from apps.methods.imgfactory_svg_icons import SVGIconFactory

from apps.methods.txd_parser import parse_txd as _parse_txd_shared


# COL Workshop parser system

from apps.gui.tool_menu_mixin import ToolMenuMixin
try:
    from apps.methods.gl_viewport_mixin import GLViewportMixin
except ImportError:
    class GLViewportMixin: pass



##Methods list -
# open_model_workshop
#
##class ModelWorkshop: -
# _apply_button_font
# _apply_hotkey_settings
# _apply_infobar_font
# _apply_panel_font
# _apply_theme
# _apply_title_font
# _build_menus_into_qmenu
# closeEvent
# _dock_to_main
# _get_icon_color
# _get_icon_set
# get_menu_title
# _get_ui_color
# __init__
# _launch_theme_settings
# _reset_hotkeys_to_defaults
# _save_settings
# _setup_hotkeys
# setup_ui
# _show_about
# show_help
# _show_settings_context_menu
# show_settings_dialog
# _show_settings_dialog
# _show_settings_hotkeys
# _show_workshop_settings
# toggle_dock_mode
# _undock_from_main
# _update_dock_button_visibility
#
##Mixins (depends/) -
# ModelWindowMixin - model_win_func.py
# ModelSetupUIMixin - model_setup_ui_func.py
# ModelCoreLogicMixin - model_core_logic_func.py
# ModelListMixin - model_list_func.py (also _DFFGeometryAdapter, _ModelListDelegate)
# ModelViewportMixin - model_viewport_func.py
# ModelEditMixin - model_edit_func.py
# ModelTextureMixin - model_texture_func.py
# RibbonIconsMixin - methods/ribbon_dialog.py
# DocTabsMixin - methods/doc_tabs.py

#TODO some gta3 dff files show as unknown format, effects standalone and docked versions, loading files from the img files.

# Model Workshop icon available: SVGIconFactory.model_workshop_icon()
# Use for: DFF edit button in main toolbar, Model Workshop tab icon.

from apps.components.Model_Editor.depends.model_win_func import ModelWindowMixin
from apps.components.Model_Editor.depends.model_setup_ui_func import ModelSetupUIMixin
from apps.components.Model_Editor.depends.model_core_logic_func import ModelCoreLogicMixin
from apps.components.Model_Editor.depends.model_list_func import ModelListMixin
from apps.components.Model_Editor.depends.model_viewport_func import ModelViewportMixin
from apps.components.Model_Editor.depends.model_edit_func import ModelEditMixin
from apps.components.Model_Editor.depends.model_texture_func import ModelTextureMixin
from apps.components.Model_Editor.depends.model_setup_ui_func import App_name, App_build, VIEWPORT_AVAILABLE, DEBUG_STANDALONE, APPSETTINGS_AVAILABLE
from apps.components.Model_Editor.depends.model_list_func import _DFFGeometryAdapter, _ModelListDelegate


class ModelWorkshop(ModelWindowMixin, ModelSetupUIMixin, ModelCoreLogicMixin, ModelListMixin, ModelViewportMixin, ModelEditMixin, ModelTextureMixin,
                    DocTabsMixin, GLViewportMixin, RibbonIconsMixin, ToolMenuMixin, QWidget): #vers 6
    """Model Workshop - Main window"""

    # - ToolMenuMixin implementation

    def _get_ui_color(self, key): #vers 1
        """Theme colour via methods/ui_color."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)

    def get_menu_title(self) -> str: #vers 1
        """Return menu label for imgfactory menu bar."""
        return "DFF"

    def _build_menus_into_qmenu(self, parent_menu): #vers 3
        """Populate parent_menu with Model Workshop actions."""
        fm = parent_menu.addMenu("File")
        fm.addAction("Open…",    self._open_file if hasattr(self, '_open_file') else lambda: None)
        fm.addAction("Open in New Tab…", self._open_file_new_tab)
        fm.addAction("Save",     self._save_file if hasattr(self, '_save_file') else lambda: None)
        fm.addAction("Save As…", self._save_file_as if hasattr(self, '_save_file_as') else lambda: None)
        fm.addAction("Batch Convert…", self._batch_convert)
        fm.addSeparator()
        fm.addAction("Export…",  self._export_col_data if hasattr(self, '_export_col_data') else lambda: None)

        col_m = parent_menu.addMenu("COL")
        col_m.addAction("Build COL from DFF…",    lambda: self._dff_to_col_surfaces(single=True))
        col_m.addAction("Batch COL from DFFs…",   lambda: self._dff_to_col_surfaces(single=False))
        col_m.addSeparator()
        col_m.addAction("Convert Surface…",        self._convert_surface if hasattr(self, '_convert_surface') else lambda: None)
        col_m.addAction("New Surface…",            self._create_new_surface if hasattr(self, '_create_new_surface') else lambda: None)

        vm = parent_menu.addMenu("View")
        vm.addAction("Sort",     self._show_sort_menu if hasattr(self, '_show_sort_menu') else lambda: None)

    workshop_closed = pyqtSignal()
    window_closed = pyqtSignal()

    # Bump whenever the set of ribbon toolbars OR dock widgets changes
    # (added/removed/renamed) so a saved layout from an older structure is
    # cleanly rejected by _restore_toolbar_state instead of Qt silently
    # failing to restore it. History: 1 = Selection/Snap Targets/Edit
    # Geometry/Navigation/Render ribbons. 2 = Files/Models panels converted
    # to QDockWidgets in the same QMainWindow (EXPERIMENTAL, Jul 2026) -
    # user can now freely drag/float/tab any panel, not just ribbons.
    _RIBBON_LAYOUT_VERSION = 4


    def __init__(self, parent=None, main_window=None): #vers 11
        """initialize_features"""
        if DEBUG_STANDALONE and main_window is None:
            print(App_name + " Initializing ...")

        super().__init__(parent)
        self.setWindowTitle(App_name)
        try:
            self.setWindowIcon(SVGIconFactory.mesh_icon(32, '#4a9fd4'))
        except Exception:
            pass
        self.icon_factory = SVGIconFactory()

        self.main_window = main_window

        self.undo_stack = []
        self.button_display_mode = 'both'
        self.last_save_directory = None
        # Thumbnail spin animation state
        self._spin_timer  = None
        self._spin_row    = None
        self._spin_model  = None
        self._spin_yaw    = 0.0
        self._spin_pitch  = 0.0
        self._spin_dyaw   = 1.0
        self._spin_dpitch = 0.2
        # Thumbnail view axis (applied to all static thumbnails)
        self._thumb_yaw   = 0.0    # top-down (XY plane) by default
        self._thumb_pitch = 0.0

        # Set default fonts
        from PyQt6.QtGui import QFont
        default_font = QFont("Fira Sans Condensed", 14)
        self.setFont(default_font)
        self.title_font = QFont("Arial", 14)
        self.panel_font = QFont("Arial", 10)
        self.button_font = QFont("Arial", 10)
        self.infobar_font = QFont("Courier New", 9)
        # Section titles (Models/Frame Hierarchy/Textures/Files dock
        # titles, panel headers) - one consistent size, slightly larger
        # than panel_font, instead of the mix of ad-hoc sizes (9/10/11)
        # scattered across different panels previously.
        self.section_title_font = QFont("Arial", 11, QFont.Weight.Bold)
        self.standalone_mode = (main_window is None)

        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        else:
            # FIXED: Create AppSettings for standalone mode
            try:
                from apps.utils.app_settings_system import AppSettings
                self.app_settings = AppSettings()
            except Exception as e:
                print(f"Could not initialize AppSettings: {e}")
                self.app_settings = None
        if hasattr(self.app_settings, 'theme_changed'):
            self.app_settings.theme_changed.connect(self._refresh_icons)
        # Load persisted texlist folder path
        self._texlist_folder = ''
        self._load_texlist_setting()
        # IDE database (lazy — populated on first lookup)
        self._ide_db        = None
        self._ide_db_root   = ''
        self._source_img_path = ''

        self._show_boxes = True
        self._show_mesh = True

        self._checkerboard_size = 16
        self._overlay_opacity = 50
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)
        _win = self.palette().color(self.palette().ColorRole.Window)
        #self.background_color = self._get_ui_color('viewport_bg') #crashes app
        self.background_mode = 'solid'
        self.placeholder_text = "No Surface"
        self.setMinimumSize(200, 200)
        preview_widget = False

        # Docking state
        self.is_docked = (main_window is not None)
        self.dock_widget = None
        self.is_overlay = False
        self.overlay_table = None
        self.overlay_tab_index = -1

        self.setWindowTitle(App_name + ": No File")
        self.resize(1400, 800)
        self.use_system_titlebar = False
        self.window_always_on_top = False

        # Window flags — FramelessWindowHint only valid for top-level windows.
        # When docked as a tab child, it breaks QPainter and causes bleed.
        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

        self._initialize_features()

        # Corner resize variables
        self.dragging = False
        self.drag_position = None
        self.resizing = False
        self.resize_corner = None
        self.corner_size = 20
        self.hover_corner = None

        if parent:
            parent_pos = parent.pos()
            self.move(parent_pos.x() + 50, parent_pos.y() + 80)


        # Paint toolbar attrs — set by _create_paint_bar() called from _create_right_panel
        self.paint_toolbar   = None
        self.paint_mat_combo = None
        self.paint_swatch    = None
        self.paint_undo_btn  = None
        self.paint_exit_btn  = None
        self.tool_paint_btn  = None
        self.tool_dropper_btn = None
        self.tool_fill_btn   = None

        # Setup UI FIRST
        self.setup_ui()
        # Setup hotkeys
        self._setup_hotkeys()

        # Apply theme ONCE at the end
        self._apply_theme()


    def setup_ui(self): #vers 15
        """Setup the main UI layout.

        EXPERIMENTAL (Jul 2026): three independent QMainWindows, nested:
          - _outer_mw: hosts Files/Models as QDockWidgets, with the
            Viewport panel as its central widget. Dragging Files/Models
            anywhere (including onto/around the viewport) uses this one.
          - _inner_mw (built in _create_right_panel, unchanged): hosts
            just the viewport + its own ribbons (Selection/Snap/Geometry/
            Navigation/Render) - clean 4-sided docking around the
            viewport, independent of where Files/Models currently sit.
          - _middle_mw (built in _create_models_table_panel): hosts the
            Models table + the Model Name/IDE ribbons - same idea, one
            level down. Frame Hierarchy and Textures are now separate
            top-level dock widgets of their own, not nested here.
        Earlier attempt put Files/Models directly into _inner_mw, which
        meant the viewport's own ribbon edges and the outer panel edges
        were the same areas - dragging a ribbon towards the middle panel
        could end up spanning the whole window instead of just that panel.
        Rollback points: 8b950b9 (original QSplitter), cb81f7f
        (Files/Models as docks inside _inner_mw, before this change).
        """
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(5, 5, 5, 5)
        main_layout.setSpacing(5)

        # Toolbar - hidden when embedded in main window tab
        toolbar = self._create_toolbar()
        self._workshop_toolbar = toolbar
        if not self.standalone_mode:
            toolbar.setVisible(False)
        main_layout.addWidget(toolbar)

        # Build all panels - right panel builds self._inner_mw (viewport +
        # its own ribbons), fully self-contained. Models table, Frame
        # Hierarchy, and Textures are now three independent sections
        # instead of being stacked into one combined middle panel - every
        # one of them can be dragged/floated/docked on its own, same as
        # Files already could.
        right_panel = self._create_right_panel()
        left_panel = self._create_left_panel()
        models_table_panel = self._create_models_table_panel()   # is self._middle_mw
        frame_hierarchy_panel = self._create_frame_hierarchy_panel()
        texture_panel = self._create_texture_panel()

        from PyQt6.QtWidgets import QDockWidget, QMainWindow
        outer_mw = QMainWindow()
        outer_mw.setWindowFlags(Qt.WindowType.Widget)
        outer_mw.setDockOptions(
            QMainWindow.DockOption.AllowNestedDocks |
            QMainWindow.DockOption.AllowTabbedDocks)
        # Explicit separator styling - when docked inside IMG Factory,
        # this nested QMainWindow inherits the main app's theme stylesheet
        # cascading down through the tab widget (standalone has no parent
        # stylesheet to inherit), which was zeroing out the dock separator
        # entirely - no visible line/handle between panels, reported bug.
        # A locally-set stylesheet rule takes precedence over anything
        # inherited from a parent, so this holds regardless of theme.
        outer_mw.setStyleSheet(
            "QMainWindow::separator { "
            "background: palette(mid); width: 5px; height: 5px; } "
            "QMainWindow::separator:hover { background: palette(highlight); }")
        self._outer_mw = outer_mw
        from apps.methods.grip_splitter import GripDockSeparators
        self._grip_docks = GripDockSeparators(outer_mw)    # ribbon grip on dock separators
        self.setAcceptDrops(True)       # models / COL / TXD / IMG dropped here open

        # Viewport (right_panel, wrapping _inner_mw) is the central widget -
        # always visible, never accidentally closed/floated away entirely.
        # Every dock can go to any of its 4 edges.
        outer_mw.setCentralWidget(right_panel)

        self._left_dock = None
        self._middle_dock = None
        self._frame_hierarchy_dock = None
        self._texture_dock = None
        icon_color = self._get_icon_color()

        if left_panel is not None:  # IMG Factory mode
            self._left_dock = QDockWidget("Files", self)
            self._left_dock.setObjectName("Files")
            self._left_dock.setWidget(left_panel)
            self._left_dock.setMinimumWidth(150)
            self._left_dock.setFeatures(
                QDockWidget.DockWidgetFeature.DockWidgetMovable |
                QDockWidget.DockWidgetFeature.DockWidgetFloatable)
            outer_mw.addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, self._left_dock)

        self._middle_dock = QDockWidget("Models", self)
        self._middle_dock.setObjectName("Models")
        self._middle_dock.setWidget(models_table_panel)
        # Set explicit here, not just on the inner panel - with nested
        # QMainWindows (outer_mw -> this dock -> _middle_mw -> panel), a
        # minimum width set only on the innermost panel doesn't reliably
        # propagate up through multiple layers to the outer dock-resize
        # logic. Without this, dragging the divider could shrink the dock
        # to fully hidden but not grow it back past a point - reported bug.
        self._middle_dock.setMinimumWidth(250)
        self._middle_dock.setFeatures(
            QDockWidget.DockWidgetFeature.DockWidgetMovable |
            QDockWidget.DockWidgetFeature.DockWidgetFloatable)
        outer_mw.addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, self._middle_dock)

        self._frame_hierarchy_dock = QDockWidget("Frame Hierarchy", self)
        self._frame_hierarchy_dock.setObjectName("Frame Hierarchy")
        self._frame_hierarchy_dock.setWidget(frame_hierarchy_panel)
        self._frame_hierarchy_dock.setMinimumWidth(200)
        self._frame_hierarchy_dock.setFeatures(
            QDockWidget.DockWidgetFeature.DockWidgetMovable |
            QDockWidget.DockWidgetFeature.DockWidgetFloatable)
        outer_mw.addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, self._frame_hierarchy_dock)

        self._texture_dock = QDockWidget("Textures", self)
        self._texture_dock.setObjectName("Textures")
        self._texture_dock.setWidget(texture_panel)
        self._texture_dock.setMinimumWidth(200)
        self._texture_dock.setFeatures(
            QDockWidget.DockWidgetFeature.DockWidgetMovable |
            QDockWidget.DockWidgetFeature.DockWidgetFloatable)
        outer_mw.addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, self._texture_dock)

        if self._left_dock is not None:
            # Side-by-side by default (Files | Models), matching the old
            # splitter's left-to-right order - user can still drag them
            # apart, tab together, float, or move either beside/around
            # the viewport from here.
            outer_mw.splitDockWidget(self._left_dock, self._middle_dock,
                                      Qt.Orientation.Horizontal)
        # Frame Hierarchy and Textures default to stacking below Models -
        # a sensible starting point, freely rearrangeable afterwards.
        outer_mw.splitDockWidget(self._middle_dock, self._frame_hierarchy_dock,
                                  Qt.Orientation.Vertical)
        outer_mw.splitDockWidget(self._frame_hierarchy_dock, self._texture_dock,
                                  Qt.Orientation.Vertical)

        # Double-click any dock's title bar to collapse it down to just the
        # title (content hidden), double-click again to restore - replaces
        # the ad-hoc per-panel collapse buttons that used to live inside
        # Models/Frame Hierarchy/Textures with one consistent interaction
        # across every dock, including Files.
        for _dock, _title in ((self._left_dock, "Files"),
                               (self._middle_dock, "Models"),
                               (self._frame_hierarchy_dock, "Frame Hierarchy"),
                               (self._texture_dock, "Textures")):
            if _dock is not None:
                self._make_dock_collapsible(_dock, _title)

        main_layout.addWidget(outer_mw)

        # Restore saved outer layout (Files/Models positions around the
        # viewport) - separate from _inner_mw's own ribbon state.
        from PyQt6.QtCore import QTimer as _QTOuter
        _QTOuter.singleShot(400, self._restore_outer_layout)
        self.window_closed.connect(self._save_outer_layout)

        # Status indicators - hidden when embedded in main window tab
        if hasattr(self, '_setup_status_indicators'):
            status_frame = self._setup_status_indicators()
            if not self.standalone_mode:
                status_frame.setVisible(False)
            main_layout.addWidget(status_frame)

        # Apply theme colours to all icons now that UI is fully built
        self._refresh_icons()
    def show_help(self, *a, **kw): pass  #vers 1
    def show_settings_dialog(self, *a, **kw): pass  #vers 1

    def _show_workshop_settings(self): #vers 1

        """Show complete workshop settings dialog"""
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
        button_hint.setStyleSheet("color: palette(placeholderText); font-style: italic;")
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
        cache_hint.setStyleSheet("color: palette(placeholderText); font-style: italic;")
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
        cb_hint.setStyleSheet("color: palette(placeholderText); font-style: italic; font-size: 10px;")
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
        opacity_hint.setStyleSheet("color: palette(placeholderText); font-style: italic; font-size: 10px;")
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
                background: palette(highlight);
                color: white;
                padding: 10px 24px;
                font-weight: bold;
                border-radius: 4px;
                font-size: 13px;
            }
            QPushButton:hover {
                background: palette(highlight);
            }
        """)

        def apply_settings():  #vers 1
            # Adjusted for COL Wireframe, Mesh
            self.setFont(QFont(default_font_combo.currentFont().family(),
                            default_font_size.value()))
            self.title_font = QFont(title_font_combo.currentFont().family(),
                                title_font_size.value())
            self.panel_font = QFont(panel_font_combo.currentFont().family(),
                                panel_font_size.value())
            self.button_font = QFont(button_font_combo.currentFont().family(),
                                    button_font_size.value())
            self.infobar_font = QFont(infobar_font_combo.currentFont().family(),
                                    infobar_font_size.value())

            # Apply fonts to UI
            self._apply_title_font()
            self._apply_panel_font()
            self._apply_button_font()
            self._apply_infobar_font()

            mode_map = {0: 'both', 1: 'icons', 2: 'text'}
            self.button_display_mode = mode_map[button_mode_combo.currentIndex()]

            # EXPORT
            self.default_export_format = self.format_combo.currentText()

            # PREVIEW
            self.zoom_level = zoom_spin.value() / 100.0

            bg_modes = ['solid', 'checkerboard', 'grid']
            self.background_mode = bg_modes[bg_mode_combo.currentIndex()]

            self._checkerboard_size = cb_spin.value()
            self._overlay_opacity = opacity_spin.value()

            # Update preview widget
            if hasattr(self, 'preview_widget'):
                if self.background_mode == 'checkerboard':
                    self.preview_widget.set_checkerboard_background()
                    self.preview_widget._checkerboard_size = self._checkerboard_size
                else:
                    self.preview_widget.set_background_color(self.preview_widget.bg_color)

            # Apply button display mode
            if hasattr(self, '_update_all_buttons'):
                self._update_all_buttons()

            # Refresh display

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Workshop settings updated successfully")

        apply_btn.clicked.connect(apply_settings)
        btn_layout.addWidget(apply_btn)

        # Close button
        close_btn = QPushButton("Close")
        close_btn.setStyleSheet("padding: 10px 24px; font-size: 13px;")
        close_btn.clicked.connect(dialog.close)
        btn_layout.addWidget(close_btn)

        layout.addLayout(btn_layout)

        # Show dialog
        dialog.exec()


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


    def _dock_to_main(self): #vers 10
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
            self._sync_middle_btn_row_visibility()

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} docked to main window")


        except Exception as e:
            print(f"Error docking: {str(e)}")
            self.show()


    def _undock_from_main(self): #vers 5
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
            self._sync_middle_btn_row_visibility()

            self.show()
            self.raise_()

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} undocked to standalone")
                
        except Exception as e:
            print(f"Error undocking: {str(e)}")
            # Fallback
            self.setWindowFlags(Qt.WindowType.Window)
            self.show()


    def closeEvent(self, event): #vers 2
        """Handle close — _save_toolbar_state fires via window_closed signal."""
        self.window_closed.emit()
        try:
            mw = getattr(self, 'main_window', None) or getattr(self, '_imgfactory', None)
            if mw and hasattr(mw, '_update_tool_menu_for_tab'):
                mw._update_tool_menu_for_tab(None)
        except Exception:
            pass
        event.accept()

    # Outer layout (Files/Models/Frame Hierarchy/Textures dock widgets
    # around the viewport) - a separate QMainWindow from _inner_mw, so it
    # gets its own save/restore with its own version constant. History:
    # 1 = Files/Models only. 2 = Frame Hierarchy and Textures split out
    # into their own top-level dock widgets too, same as Files.
    _OUTER_LAYOUT_VERSION = 2


# - Rest of the logic for the panels

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


# - Marker 6


    def _launch_theme_settings(self): #vers 2
        """Launch theme engine from app_settings_system"""
        try:
            from apps.utils.app_settings_system import AppSettings, SettingsDialog

            # Get or create app_settings
            if not hasattr(self, 'app_settings') or self.app_settings is None:
                self.app_settings = AppSettings()
                if not hasattr(self.app_settings, 'current_settings'):
                    print("AppSettings failed to initialize")
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.warning(self, "Error", "Could not initialize theme system")
                    return

            # Launch settings dialog
            dialog = SettingsDialog(self.app_settings, self)

            # Connect theme change signal to apply theme
            dialog.themeChanged.connect(lambda theme: self._apply_theme())

            if dialog.exec():
                # Apply theme after dialog closes
                self._apply_theme()
                print("Theme settings applied")
                if hasattr(self, 'main_window') and self.main_window:
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Theme settings updated")

        except Exception as e:
            print(f"Theme settings error: {e}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Theme Error", f"Could not load theme system:\n{e}")



    def _show_settings_dialog(self): #vers 5
        """Show comprehensive settings dialog with all tabs including hotkeys"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                                    QWidget, QLabel, QPushButton, QGroupBox,
                                    QCheckBox, QSpinBox, QFormLayout, QScrollArea,
                                    QKeySequenceEdit, QComboBox, QMessageBox)
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
        format_hint.setStyleSheet("color: palette(placeholderText); font-style: italic;")
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
            "Export settings apply to the active model. "
            "COL export uses the format set in the Format combo. "
            "OBJ export includes vertex positions and face indices."
        )
        compat_label.setWordWrap(True)
        compat_label.setStyleSheet("padding: 10px; background-color: palette(mid); border-radius: 4px;")
        export_layout.addWidget(compat_label)

        # - Texture Sources
        tex_src_group = QGroupBox("Texture Sources")
        tex_src_layout = QVBoxLayout(tex_src_group)

        tex_src_layout.addWidget(QLabel(
            "texlist/ folder: pre-exported PNG/IFF/TGA textures.\nFallback when TXD not found in any open IMG."))

        texlist_row = QHBoxLayout()
        texlist_row.addWidget(QLabel("texlist/ folder:"))
        texlist_edit = QLineEdit()
        texlist_edit.setPlaceholderText("Browse to set, or leave blank for auto-discover")
        texlist_edit.setText(getattr(self, '_texlist_folder', '') or '')
        texlist_edit.setFixedHeight(26)
        texlist_row.addWidget(texlist_edit, 1)

        texlist_browse = QPushButton("…")
        texlist_browse.setFixedSize(28, 24)
        texlist_browse.setToolTip("Browse for texlist/ root folder")
        texlist_browse.clicked.connect(lambda: (
            (folder := __import__('PyQt6.QtWidgets', fromlist=['QFileDialog'])
                       .QFileDialog.getExistingDirectory(
                           dialog, "Select texlist/ folder",
                           texlist_edit.text() or __import__('os').path.expanduser('~'))),
            texlist_edit.setText(folder) if folder else None))
        texlist_row.addWidget(texlist_browse)
        tex_src_layout.addLayout(texlist_row)

        auto_discover_lbl = QLabel(
            "If blank: auto-discovers texlist/ next to the loaded DFF file.")
        auto_discover_lbl.setStyleSheet("color: palette(mid); font-style: italic;")
        tex_src_layout.addWidget(auto_discover_lbl)

        export_layout.addWidget(tex_src_group)

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
        format_note.setStyleSheet("color: palette(placeholderText); font-style: italic;")
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
        force_save_hint.setStyleSheet("color: palette(placeholderText); font-style: italic;")
        force_save_layout.addWidget(force_save_hint)
        file_form.addRow("Force Save:", force_save_layout)

        hotkey_edit_save_as = QKeySequenceEdit(self.hotkey_save_as.key() if hasattr(self, 'hotkey_save_as') else QKeySequence.StandardKey.SaveAs)
        file_form.addRow("Save As:", hotkey_edit_save_as)

        hotkey_edit_close = QKeySequenceEdit(self.hotkey_close.key() if hasattr(self, 'hotkey_close') else QKeySequence.StandardKey.Close)
        file_form.addRow("Close:", hotkey_edit_close)

        file_group.setLayout(file_form)
        scroll_layout.addWidget(file_group)

        # Edit Operations Group
        edit_group = QGroupBox("Edit Operations")
        edit_form = QFormLayout()

        hotkey_edit_undo = QKeySequenceEdit(self.hotkey_undo.key() if hasattr(self, 'hotkey_undo') else QKeySequence.StandardKey.Undo)
        edit_form.addRow("Undo:", hotkey_edit_undo)

        hotkey_edit_copy = QKeySequenceEdit(self.hotkey_copy.key() if hasattr(self, 'hotkey_copy') else QKeySequence.StandardKey.Copy)
        edit_form.addRow("Copy Collision:", hotkey_edit_copy)

        hotkey_edit_paste = QKeySequenceEdit(self.hotkey_paste.key() if hasattr(self, 'hotkey_paste') else QKeySequence.StandardKey.Paste)
        edit_form.addRow("Paste Collision:", hotkey_edit_paste)

        hotkey_edit_delete = QKeySequenceEdit(self.hotkey_delete.key() if hasattr(self, 'hotkey_delete') else QKeySequence.StandardKey.Delete)
        edit_form.addRow("Delete:", hotkey_edit_delete)

        hotkey_edit_duplicate = QKeySequenceEdit(self.hotkey_duplicate.key() if hasattr(self, 'hotkey_duplicate') else QKeySequence("Ctrl+D"))
        edit_form.addRow("Duplicate:", hotkey_edit_duplicate)

        hotkey_edit_rename = QKeySequenceEdit(self.hotkey_rename.key() if hasattr(self, 'hotkey_rename') else QKeySequence("F2"))
        edit_form.addRow("Rename:", hotkey_edit_rename)

        edit_group.setLayout(edit_form)
        scroll_layout.addWidget(edit_group)

        # Collision Operations Group
        coll_group = QGroupBox("Collision Operations")
        coll_form = QFormLayout()

        hotkey_edit_import = QKeySequenceEdit(self.hotkey_import.key() if hasattr(self, 'hotkey_import') else QKeySequence("Ctrl+I"))
        coll_form.addRow("Import Collision:", hotkey_edit_import)

        hotkey_edit_export = QKeySequenceEdit(self.hotkey_export.key() if hasattr(self, 'hotkey_export') else QKeySequence("Ctrl+E"))
        coll_form.addRow("Export Collision:", hotkey_edit_export)

        hotkey_edit_export_all = QKeySequenceEdit(self.hotkey_export_all.key() if hasattr(self, 'hotkey_export_all') else QKeySequence("Ctrl+Shift+E"))
        coll_form.addRow("Export All:", hotkey_edit_export_all)

        coll_group.setLayout(coll_form)
        scroll_layout.addWidget(coll_group)

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
        scroll_layout.addWidget(view_group)

        scroll_layout.addStretch()

        scroll.setWidget(scroll_widget)
        hotkeys_layout.addWidget(scroll)

        # Reset to defaults button
        reset_layout = QHBoxLayout()
        reset_layout.addStretch()
        reset_hotkeys_btn = QPushButton("Reset to Plasma6 Defaults")

        def reset_hotkeys():  #vers 1
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

        def apply_settings(close_dialog=False):  #vers 1
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
            self.export_preserve_shadow = preserve_shadow_check.isChecked()
            self.export_shadow_separate = export_shadowm_check.isChecked()
            self.export_create_subfolders = create_subfolders_check.isChecked()
            # Texture Sources — texlist folder
            new_texlist = texlist_edit.text().strip()
            if new_texlist != getattr(self, '_texlist_folder', ''):
                self._texlist_folder = new_texlist
                self._save_texlist_setting()

            # Apply import settings
            self.import_replace_existing = replace_check.isChecked()
            self.import_auto_format = auto_format_check.isChecked()
            self.default_import_format = import_format_combo.currentText()

            # Apply constraint settings
            self.name_limit_enabled = name_limit_check.isChecked()
            self.max_surface_name_length = char_limit_spin.value()

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


    def _show_settings_context_menu(self, pos): #vers 2
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

        # Icon display mode submenu (icon-only mode uses _update_tex_btn_compact)
        display_menu = menu.addMenu("Button Display")

        icons_text_action = display_menu.addAction("Icons & Text")
        icons_text_action.setCheckable(True)
        icons_text_action.setChecked(self.button_display_mode == 'both')
        icons_text_action.triggered.connect(lambda: self._set_icon_display_mode('both'))

        icons_only_action = display_menu.addAction("Icons Only")
        icons_only_action.setCheckable(True)
        icons_only_action.setChecked(self.button_display_mode == 'icons')
        icons_only_action.triggered.connect(lambda: self._set_icon_display_mode('icons'))

        text_only_action = display_menu.addAction("Text Only")
        text_only_action.setCheckable(True)
        text_only_action.setChecked(self.button_display_mode == 'text')
        text_only_action.triggered.connect(lambda: self._set_icon_display_mode('text'))

        # Show menu at button position
        menu.exec(self.properties_btn.mapToGlobal(pos))


    def _get_icon_color(self): #vers 3
        """Get icon colour from current theme — returns text_primary.
        Falls back to main_window app_settings if own settings not loaded."""
        as_ = (self.app_settings
               or getattr(getattr(self, 'main_window', None), 'app_settings', None))
        if as_:
            try:
                colors = as_.get_theme_colors() or {}
                return colors.get('text_primary', '#cccccc')
            except Exception:
                pass
        return '#cccccc'

    def _get_icon_set(self) -> str: #vers 2
        """Return active icon set: 'default' or '3dsmax'.
        Read from model_workshop.json 'icon_set' key."""
        try:
            import json
            from pathlib import Path
            path = get_user_config_dir() / 'model_workshop.json'
            if path.exists():
                return json.loads(path.read_text()).get('icon_set', 'default')
        except Exception:
            pass
        return 'default'



    def _apply_theme(self): #vers 8
        """Apply theme — matches COL/TXD pattern exactly.
        Sets QApplication stylesheet globally, clears own override to inherit."""
        try:
            mw = getattr(self, 'main_window', None)
            app_settings = None
            if hasattr(self, 'app_settings') and self.app_settings:
                app_settings = self.app_settings
            elif mw and hasattr(mw, 'app_settings'):
                app_settings = mw.app_settings

            if app_settings and hasattr(app_settings, 'get_stylesheet'):
                from PyQt6.QtWidgets import QApplication
                ss = app_settings.get_stylesheet()
                if ss:
                    QApplication.instance().setStyleSheet(ss)
            # Clear widget-level override — inherit from QApplication like COL/TXD
            self.setStyleSheet("")
        except Exception as e:
            print(f"Theme application error: {e}")



    def _save_settings(self): #vers 1
        """Save settings to config file"""
        import json

        settings_file = os.path.join(
            os.path.dirname(__file__),
            'col_workshop_settings.json'
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



# ----- Render functions

    def _setup_hotkeys(self): #vers 3
        """Setup Plasma6-style keyboard shortcuts for this application - checks for existing methods"""
        from PyQt6.QtGui import QShortcut, QKeySequence

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
            def force_save():  #vers 1
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
            def rename_shortcut():  #vers 1
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

        # Select All (Ctrl+A)
        self.hotkey_select_all = QShortcut(QKeySequence.StandardKey.SelectAll, self)
        self.hotkey_select_all.activated.connect(self._select_all_models)

        # Invert Selection (Ctrl+I)
        from PyQt6.QtGui import QKeySequence as _KS
        self.hotkey_invert = QShortcut(_KS("Ctrl+I"), self)
        self.hotkey_invert.activated.connect(self._invert_selection)

        # Find (Ctrl+F)
        self.hotkey_find = QShortcut(QKeySequence.StandardKey.Find, self)
        if not hasattr(self, '_focus_search'):
            # Create focus search method inline
            def focus_search():  #vers 1
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


    def _apply_hotkey_settings(self, dialog, close=False): #vers 2
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

        # Save hotkeys to config
        try:
            import json, os
            cfg_path = os.path.join(get_user_config_dir(), 'model_workshop.json')
            try:
                cfg = json.load(open(cfg_path))
            except Exception:
                cfg = {}
            cfg['hotkeys'] = {
                'save':       self.hotkey_edit_save.keySequence().toString(),
                'open':       self.hotkey_edit_open.keySequence().toString(),
                'undo':       self.hotkey_edit_undo.keySequence().toString(),
                'refresh':    self.hotkey_edit_refresh.keySequence().toString(),
                'properties': self.hotkey_edit_properties.keySequence().toString(),
                'find':       self.hotkey_edit_find.keySequence().toString(),
                'help':       self.hotkey_edit_help.keySequence().toString(),
            }
            os.makedirs(os.path.dirname(cfg_path), exist_ok=True)
            json.dump(cfg, open(cfg_path, 'w'), indent=2)
        except Exception as _e:
            pass   # non-fatal if config can't be written

        if close:
            dialog.accept()


    def _show_settings_hotkeys(self): #vers 1
        """Show settings dialog with hotkey customization"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                                    QWidget, QLabel, QPushButton, QGroupBox,
                                    QFormLayout, QKeySequenceEdit)
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
        force_save_hint.setStyleSheet("color: palette(placeholderText); font-style: italic;")
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
        placeholder_label.setStyleSheet("color: palette(placeholderText); font-style: italic; padding: 20px;")
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

    def _show_about(self): #vers 1
        """Show Model Workshop about dialog."""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton, QTextEdit
        from PyQt6.QtGui import QFont
        from PyQt6.QtCore import Qt
        dlg = QDialog(self)
        dlg.setWindowTitle(f"About {App_name}")
        dlg.setMinimumWidth(480); dlg.setMinimumHeight(360)
        lay = QVBoxLayout(dlg); lay.setSpacing(10)

        hdr = QLabel(f"{App_name}  —  Build {App_build}")
        hdr.setFont(QFont('Arial', 14, QFont.Weight.Bold))
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        lay.addWidget(hdr)

        lay.addWidget(QLabel(f"Author: X-Seti  |  Part of IMG Factory 1.6",
                             alignment=Qt.AlignmentFlag.AlignCenter))

        info = QTextEdit(); info.setReadOnly(True)
        info.setHtml("""
            <b>Model Workshop Capabilities:</b><br><br>
            <b>✓ DFF Support:</b> GTA III / VC / SA RenderWare DFF model files<br>
            <b>✓ COL Support:</b> COL1 (III/VC) COL2/3/4 (SA) collision files<br>
            <b>✓ Viewport:</b> QPainter 2D + OpenGL 3D (GL toggle)<br>
            <b>✓ Editing:</b> Geometry, materials, UV, normals, vertex colours<br>
            <b>✓ Export:</b> OBJ, COL, texture extraction<br>
            <b>✓ IDE Integration:</b> Link to DAT Browser IDE entries<br>
            <b>✓ TXD:</b> Auto-load matching TXD for textured preview<br><br>
            <b>Shortcuts:</b><br>
            Ctrl+O: Open  |  Ctrl+S: Save  |  Ctrl+D: Open DFF<br>
            Ctrl+T: Open TXD  |  Ctrl+Z: Undo  |  Delete: Remove
        """)
        lay.addWidget(info, 1)

        btn = QPushButton('Close'); btn.clicked.connect(dlg.accept)
        lay.addWidget(btn)
        dlg.exec()



    # _export_col_data implemented above (line ~5136) — this stub removed


# Convenience functions


# (moved into ModelWorkshop class — see _ensure_standalone_functionality method above)


# --- External AI upscaler integration helper ---
import sys
from apps.methods.img_factory_settings import get_user_config_dir


def open_model_workshop(main_window, dff_path=None,
                        original_dff_name=None): #vers 7
    """Open Model Workshop — routes DFF/COL/IMG correctly.
    original_dff_name: the DFF entry name from the IMG (e.g. 'airportwall_2_2.dff')
    so that IDE lookup works even when the DFF was extracted to /tmp/ with a random suffix."""
    try:
        # Try to dock in main window tab if available
        if main_window and hasattr(main_window, 'main_tab_widget'):
            import os as _os
            from PyQt6.QtWidgets import QWidget, QVBoxLayout
            container = QWidget()
            layout = QVBoxLayout(container)
            layout.setContentsMargins(0, 0, 0, 0)
            workshop = ModelWorkshop(container, main_window)
            workshop.setWindowFlags(Qt.WindowType.Widget)
            layout.addWidget(workshop)
            tab_label = _os.path.splitext(_os.path.basename(dff_path))[0] if dff_path else "Model Workshop"
            try:
                from apps.methods.imgfactory_svg_icons import get_dff_edit_icon
                icon = get_dff_edit_icon()
                idx = main_window.main_tab_widget.addTab(container, icon, tab_label)
            except Exception:
                idx = main_window.main_tab_widget.addTab(container, tab_label)
            main_window.main_tab_widget.setCurrentIndex(idx)
            if hasattr(main_window, '_ensure_tab_area_visible'):
                main_window._ensure_tab_area_visible()
            workshop.show()
        else:
            # Standalone window
            workshop = ModelWorkshop(main_window=main_window)
            workshop.setWindowFlags(Qt.WindowType.Window)
            workshop.setWindowTitle(f"Model Workshop — {App_name}")
            workshop.resize(1200, 800)
            workshop.show()

        # Store source IMG path — DFFs extracted to /tmp lose game context
        if main_window:
            _ci = getattr(main_window, 'current_img', None)
            if _ci:
                _sp = getattr(_ci, 'file_path', '') or ''
                if _sp:
                    workshop._source_img_path = _sp

        # Store original DFF name BEFORE open_dff_file so _lookup_ide_for_dff sees it
        if original_dff_name:
            workshop._original_dff_name = original_dff_name

        # Route the file
        if dff_path:
            workshop._open_path(dff_path)       # every model format, COL, TXD, IMG
        else:
            # No explicit file — use already-open IMG from main window directly
            if main_window:                     # every open IMG tab, not just the current one
                workshop._populate_left_panel_from_open_imgs()
                if hasattr(main_window, 'log_message'):
                    main_window.log_message("Model Workshop opened")

        return workshop
    except Exception as e:
        import traceback; traceback.print_exc()
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Model Workshop error: {e}")
        return None



MDLEditorDialog = ModelWorkshop
MODWorkshop     = ModelWorkshop
ModelWorkshopDialog = ModelWorkshop

if __name__ == "__main__":
    import sys
    import traceback

    print(App_name + " Starting.")

    try:
        app = QApplication(sys.argv)
        print("QApplication created")

        workshop = ModelWorkshop()
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

