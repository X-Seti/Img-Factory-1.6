#!/usr/bin/env python3
#this belongs in apps/components/Model_Editor/model_workshop.py - Version: 223
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
from apps.methods.statusbar_func import add_statusbar_checkbox, register_statusbar
from apps.methods.ribbon_dialog import RibbonIconsMixin
from apps.methods.doc_tabs import DocTabsMixin
from apps.methods.dock_panels import DockPanelsMixin
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
# _build_menus_into_qmenu
# closeEvent
# _dock_to_main
# get_menu_title
# _get_ui_color
# __init__
# setup_ui
# show_help
# show_settings_dialog
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
# ModelSettingsMixin - model_settings_func.py
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
from apps.components.Model_Editor.depends.model_settings_func import ModelSettingsMixin
from apps.components.Model_Editor.depends.model_setup_ui_func import App_name, App_build, VIEWPORT_AVAILABLE, DEBUG_STANDALONE, APPSETTINGS_AVAILABLE
from apps.components.Model_Editor.depends.model_list_func import _DFFGeometryAdapter, _ModelListDelegate


class ModelWorkshop(ModelWindowMixin, ModelSetupUIMixin, ModelCoreLogicMixin, ModelListMixin, ModelViewportMixin, ModelEditMixin, ModelTextureMixin,
                    ModelSettingsMixin, DocTabsMixin, DockPanelsMixin, GLViewportMixin, RibbonIconsMixin, ToolMenuMixin, QWidget): #vers 8
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


    def setup_ui(self): #vers 18
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
        from apps.methods.dock_panels import add_title_widget
        from apps.methods.file_list_filter import make_type_filter
        for _b in (self._tex_view_btn,):                                      # fit the slim title bar
            _b.setFixedHeight(18)
            _b.setStyleSheet("padding: 0px 4px;")
        self._texture_dock._title_label.setVisible(False)                     # count replaces "Textures"
        add_title_widget(self._texture_dock, self._tex_count_lbl, 0)
        add_title_widget(self._texture_dock, self._tex_view_btn)
        if self._left_dock is not None:                                       # Files: counts, type, search
            self._left_dock._title_label.setVisible(False)
            add_title_widget(self._left_dock, self._left_panel_header, 0)
            add_title_widget(self._left_dock, make_type_filter(self))
            add_title_widget(self._left_dock, self._model_search_btn)

        self._outer_docks = [d for d in (self._left_dock, self._middle_dock,
                                         self._frame_hierarchy_dock, self._texture_dock) if d is not None]
        main_layout.addWidget(outer_mw)

        # Restore saved outer layout (Files/Models positions around the
        # viewport) - separate from _inner_mw's own ribbon state.
        from PyQt6.QtCore import QTimer as _QTOuter
        _QTOuter.singleShot(400, self._restore_outer_layout)
        self.window_closed.connect(self._save_outer_layout)

        # Status indicators - hidden when embedded in main window tab
        status_frame = self._create_status_bar()
        main_layout.addWidget(status_frame)
        register_statusbar(self, status_frame, docked_hide=True)

        # Apply theme colours to all icons now that UI is fully built
        self._refresh_icons()
    def show_help(self, *a, **kw): pass  #vers 1
    def show_settings_dialog(self, *a, **kw): pass  #vers 1

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


    def _dock_to_main(self): #vers 11
        """Return a torn-off workshop to its IMG Factory tab."""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                self.show()
                self.raise_()
                return

            # For proper docking, we need to be called from imgfactory
            # This method should be handled by imgfactory's overlay system
            from apps.methods.dock_panels import redock_to_tab
            if not redock_to_tab(self):      # no tab to return to
                return

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

