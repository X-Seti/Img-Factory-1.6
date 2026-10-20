#!/usr/bin/env python3
#this belongs in apps/components/Map_Editor/map_workshop.py - Version: 256
# X-Seti - see CHANGELOG.md in this folder for the full dated history

import os
import math
import json
# Force X11/GLX backend for NVIDIA on Wayland
import sys
if sys.platform.startswith('linux'):   # xcb only exists on Linux
    os.environ['QT_QPA_PLATFORM'] = 'xcb'
    os.environ['QSG_RHI_BACKEND'] = 'opengl'
    os.environ['LIBGL_ALWAYS_SOFTWARE'] = '0'  # Use hardware acceleration

import shutil
import sys
import base64
from pathlib import Path


# Add project root to path for standalone mode
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

# Import PyQt6
from PyQt6.QtWidgets import (QApplication, QCheckBox, QWidget,
    QVBoxLayout, QHBoxLayout, QSplitter, QListWidget, QDialog, QSpinBox, QListWidgetItem, QLabel,  QPushButton, QFrame, QFileDialog, QLineEdit, QMessageBox, QScrollArea, QGroupBox, QTableWidget, QTableWidgetItem, QColorDialog, QHeaderView, QAbstractItemView, QMenu, QComboBox, QInputDialog, QTabWidget, QDoubleSpinBox, QRadioButton, QStyledItemDelegate, QTimeEdit, QDockWidget, QSizePolicy, QStatusBar,
    QProgressDialog, QStackedWidget, QGridLayout, QToolButton, QProgressBar
)

from PyQt6.QtCore import Qt, pyqtSignal, QSize, QPoint, QByteArray, QPointF, QTimer, QAbstractTableModel, QObject, QEventLoop
from PyQt6.QtGui import QFont, QIcon, QPixmap, QImage, QPainter, QPen, QBrush, QColor, QCursor

# Shared DFFViewport — import from methods/, fallback to local methods/
from apps.methods.statusbar_func import add_statusbar_checkbox, register_statusbar
from apps.methods.ribbon_dialog import RibbonIconsMixin
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
from apps.methods.img_factory_settings import get_user_config_dir
try:
    from apps.methods.gl_viewport_mixin import GLViewportMixin
except ImportError:
    class GLViewportMixin: pass


from apps.components.Map_Editor.depends.map_win_func import MapWindowMixin
from apps.components.Map_Editor.depends.map_setup_ui_func import MapSetupUIMixin
from apps.components.Map_Editor.depends.map_settings_func import MapSettingsMixin
from apps.components.Map_Editor.depends.map_model_func import MapModelMixin
from apps.components.Map_Editor.depends.map_world_func import MapWorldMixin
from apps.components.Map_Editor.depends.map_ipl_func import MapIplMixin
from apps.components.Map_Editor.depends.map_edit_func import MapEditMixin
from apps.components.Map_Editor.depends.map_inst_func import MapInstMixin
from apps.components.Map_Editor.depends.map_overlay_func import MapOverlayMixin
from apps.components.Map_Editor.depends.map_widgets_func import (
    App_name, DEBUG_STANDALONE, MapSettings, _write_ipl_lines)

##Methods list -
# open_map_workshop

##class ModelWorkshop: -
# _build_menus_into_qmenu
# _focus_search
# get_menu_title
# __init__
# _rename_shadow_shortcut
# setup_ui
# _show_detailed_info
# show_help
# show_settings_dialog
# _show_surface_info

##Mixins (depends/) -
# MapWindowMixin - map_win_func.py
# MapSetupUIMixin - map_setup_ui_func.py
# MapSettingsMixin - map_settings_func.py
# MapModelMixin - map_model_func.py
# MapWorldMixin - map_world_func.py
# MapIplMixin - map_ipl_func.py
# MapEditMixin - map_edit_func.py
# MapInstMixin - map_inst_func.py
# MapOverlayMixin - map_overlay_func.py
# helper classes, MapSettings, constants - map_widgets_func.py
# RibbonIconsMixin - methods/ribbon_dialog.py
# DockPanelsMixin - methods/dock_panels.py

#  diagnostic marker (Aug 20 2026) - see __init__'s own docstring
_MODEL_WORKSHOP_INSTANCE_COUNT = 0


class ModelWorkshop(MapWindowMixin, MapSetupUIMixin, MapSettingsMixin, MapModelMixin, MapWorldMixin, MapIplMixin, MapEditMixin, MapInstMixin, MapOverlayMixin,
                    DockPanelsMixin, GLViewportMixin, RibbonIconsMixin, ToolMenuMixin, QWidget): #vers 6
    """Model Workshop - Main window"""

    # Compact button height for grafted Control Panel widgets
    _COMPACT_BUTTON_H = 18

    # - ToolMenuMixin implementation

    def get_menu_title(self) -> str: #vers 1
        """Return menu label for imgfactory menu bar."""
        return "DFF"

    def _build_menus_into_qmenu(self, parent_menu): #vers 1
        """Populate parent_menu with Model Workshop actions."""
        fm = parent_menu.addMenu("File")
        fm.addAction("Open…",    self._open_file if hasattr(self, '_open_file') else lambda: None)
        fm.addAction("Save",     self._save_file if hasattr(self, '_save_file') else lambda: None)
        fm.addAction("Save As…", self._save_file_as if hasattr(self, '_save_file_as') else lambda: None)
        fm.addSeparator()
        fm.addAction("Export…",  self._export_col_data if hasattr(self, '_export_col_data') else lambda: None)
        fm.addSeparator()
        fm.addAction("Preload Game Data Files…", self._show_preload_dialog)

        col_m = parent_menu.addMenu("COL")
        col_m.addAction("Build COL from DFF…",    lambda: self._dff_to_col_surfaces(single=True))
        col_m.addAction("Batch COL from DFFs…",   lambda: self._dff_to_col_surfaces(single=False))
        col_m.addSeparator()
        col_m.addAction("Convert Surface…",        self._convert_surface if hasattr(self, '_convert_surface') else lambda: None)
        col_m.addAction("New Surface…",            self._create_new_surface if hasattr(self, '_create_new_surface') else lambda: None)

        vm = parent_menu.addMenu("View")
        vm.addAction("Sort",     self._show_sort_menu if hasattr(self, '_show_sort_menu') else lambda: None)
        vm.addSeparator()
        panels_m = vm.addMenu("Panels")
        outer_mw = getattr(self, '_outer_mw', None)
        if outer_mw is not None:
            from PyQt6.QtWidgets import QDockWidget
            docks = outer_mw.findChildren(QDockWidget)
            # Stable order (alphabetical by title) rather than whatever
            # order findChildren happens to return, which can shift
            # around depending on how docks were added/split.
            for dock in sorted(docks, key=lambda d: d.windowTitle()):
                panels_m.addAction(dock.toggleViewAction())
        if panels_m.isEmpty():
            no_panels_action = panels_m.addAction("(no panels)")
            no_panels_action.setEnabled(False)

    workshop_closed = pyqtSignal()
    window_closed = pyqtSignal()

    # Bump whenever the set of ribbon toolbars OR dock widgets changes
    # (added/removed/renamed) so a saved layout from an older structure is
    # cleanly rejected by _restore_toolbar_state instead of Qt silently
    # failing to restore it. History: 1 = Selection/Snap Targets/Edit
    # Geometry/Navigation/Render ribbons. 2 = Files/Models panels converted
    # to QDockWidgets in the same QMainWindow (EXPERIMENTAL, Jul 2026) -
    # user can now freely drag/float/tab any panel, not just ribbons.
    # 3 = Overlays ribbon changed substantially (Aug 21 2026, per
    #  : "the save function for the ribbon manager isn't
    # working, or the config isn't being picked up, when the ribbon
    # manager is called") - Cycle, Undo, and Grge's own show/hide
    # toggle were all added this session, and a dozen separate +X/-X/
    # Save X buttons were added and then removed again a few turns
    # later once moved into each toggle's own middle-click menu -
    # this version constant was never bumped through any of that, so
    # a real save from partway through that churn could restoreState
    # against a real toolbar that no longer matches what was actually
    # saved, without ever being cleanly rejected as stale the way
    # this version check exists to guarantee. Bumping here forces any
    # such stale save to fall back to the default layout instead.
    _RIBBON_LAYOUT_VERSION = 4


    def __init__(self, parent=None, main_window=None): #vers 13
        """initialize_features"""
        #  diagnostic marker (Aug 20 2026,  "we need a
        # marker to show in the terminal for each dialog window,
        # there coming up twice, and loading the img file twice") -
        # module-level counter so a real double-construction (the
        # most direct explanation for both symptoms - two full
        # ModelWorkshop instances, each with its own deferred auto-
        # load-last-world firing on its own) shows up unmistakably in
        # the terminal, not just guessed at from code reading alone.
        global _MODEL_WORKSHOP_INSTANCE_COUNT
        _MODEL_WORKSHOP_INSTANCE_COUNT += 1
        print(f"[MapWorkshop-MARKER] __init__ call #{_MODEL_WORKSHOP_INSTANCE_COUNT} "
              f"(parent={parent!r}, main_window={main_window!r})")
        if DEBUG_STANDALONE and main_window is None:
            print(App_name + " Initializing ...")

        super().__init__(parent)
        self.setWindowTitle(App_name)
        try:
            self.setWindowIcon(SVGIconFactory.map_app_icon(64))
        except Exception:
            pass
        self.icon_factory = SVGIconFactory()

        # Map Workshop settings (separate from global app_settings/theme) -
        # required by the grafted-in Object Browser/Instance List/Editing
        # Panel/World Viewport/Control Panel methods.
        self.map_settings = MapSettings.shared()
        # Extra safety net for the debounced save (Aug 18 2026)

        app = QApplication.instance()
        if app is not None:
            app.aboutToQuit.connect(lambda: self.map_settings.save())

        self.main_window = main_window

        self.undo_stack = []
        # Real, previously-hardcoded settings, now loaded from self.
        # map_settings (Aug 20 2026)
        self.button_display_mode = self.map_settings.get('button_display_mode')
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

        # Set default fonts (Aug 20 2026 - see the button_display_mode
        # comment above for the fuller "why loaded now" explanation;
        # same real bug, same fix, applies to every font here too)
        from PyQt6.QtGui import QFont
        default_font = QFont(self.map_settings.get('default_font_family'),
                             self.map_settings.get('default_font_size'))
        self.setFont(default_font)
        self.title_font = QFont(self.map_settings.get('title_font_family'),
                                self.map_settings.get('title_font_size'))
        self.panel_font = QFont(self.map_settings.get('panel_font_family'),
                                self.map_settings.get('panel_font_size'))
        self.button_font = QFont(self.map_settings.get('button_font_family'),
                                 self.map_settings.get('button_font_size'))
        self.infobar_font = QFont(self.map_settings.get('infobar_font_family'),
                                  self.map_settings.get('infobar_font_size'))
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

        #  settings, now loaded from self.map_settings (Aug 20
        # 2026 - same real bug/fix as button_display_mode/fonts
        # above: these lived as ad-hoc self.xxx attributes that Apply
        # updated correctly in-memory but never actually persisted).
        self._checkerboard_size = self.map_settings.get('checkerboard_size')
        self._overlay_opacity = self.map_settings.get('overlay_opacity')
        self.zoom_level = self.map_settings.get('zoom_level')
        self.pan_offset = QPoint(0, 0)
        _win = self.palette().color(self.palette().ColorRole.Window)
        #self.background_color = self._get_ui_color('viewport_bg') #crashes app
        self.background_mode = self.map_settings.get('background_mode')
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

        # Restore a previously-saved dock/splitter layout, if one
        # exists (Aug 19 2026)
        QTimer.singleShot(0, self._restore_dock_state)

        #  fix (Aug 20 2026,  "I've noticed moving icons
        # to hidden, and save, these movements dont get saved") -
        # _save_toolbar_state was correctly writing the toolbar state
        # (including which icons had been moved to the Hidden toolbar)
        # to disk every time, but _restore_toolbar_state - the method
        # that reads it back - was never actually called anywhere in
        # this whole file. The save always worked; the restore side
        # just never ran. Deferred the same real way _restore_dock_
        # state already is, so this runs after every real toolbar has
        # actually been constructed and added to _inner_mw.
        QTimer.singleShot(0, self._restore_toolbar_state)

        # Move the overlay toggle buttons onto the new "Overlays"
        # ribbon (Aug 20 2026,  "The New Icons on the IPL
        # Control pane, can be moved to the ribbon") - deferred the
        # same real way as _restore_toolbar_state just above, and for
        # the same real reason: _build_toolbars (where the "Overlays"
        # ribbon is actually created) hasn't run yet at this exact
        # point in the startup sequence, only on the next event-loop
        # tick.
        QTimer.singleShot(0, self._move_overlay_buttons_to_ribbon)
        QTimer.singleShot(0, self._apply_ipl_controls_display_style)

        # Auto-load the most recently used game world on startup (Aug
        # 20 2026  own explicit "option 2" choice: "build
        # auto-restore last world on startup, so preload can fire
        # without a manual reload each time" - confirmed still needed
        # by his own follow-up: "nothing about loading preloaded
        # files" - traced this to no world ever being loaded at all
        # during that test, not a bug in the preload hook itself).
        # Deferred the same real way _restore_dock_state already is,
        # so this runs after the UI is fully constructed and visible,
        # not blocking the initial show.
        QTimer.singleShot(0, self._auto_load_last_world)
        self._init_change_tracking()
        QTimer.singleShot(0, self._apply_gamepad_setting)


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
                QDockWidget.DockWidgetFeature.DockWidgetFloatable |
                QDockWidget.DockWidgetFeature.DockWidgetClosable)
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
            QDockWidget.DockWidgetFeature.DockWidgetFloatable |
            QDockWidget.DockWidgetFeature.DockWidgetClosable)
        outer_mw.addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, self._middle_dock)

        self._frame_hierarchy_dock = QDockWidget("Frame Hierarchy", self)
        self._frame_hierarchy_dock.setObjectName("Frame Hierarchy")
        self._frame_hierarchy_dock.setWidget(frame_hierarchy_panel)
        self._frame_hierarchy_dock.setMinimumWidth(200)
        self._frame_hierarchy_dock.setFeatures(
            QDockWidget.DockWidgetFeature.DockWidgetMovable |
            QDockWidget.DockWidgetFeature.DockWidgetFloatable |
            QDockWidget.DockWidgetFeature.DockWidgetClosable)
        outer_mw.addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, self._frame_hierarchy_dock)

        self._texture_dock = QDockWidget("Textures", self)
        self._texture_dock.setObjectName("Textures")
        self._texture_dock.setWidget(texture_panel)
        self._texture_dock.setMinimumWidth(200)
        self._texture_dock.setFeatures(
            QDockWidget.DockWidgetFeature.DockWidgetMovable |
            QDockWidget.DockWidgetFeature.DockWidgetFloatable |
            QDockWidget.DockWidgetFeature.DockWidgetClosable)
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

        # Grafted in from map_workshop_old.py (Aug 1 2026)

        #Works
        object_browser_dock = self._create_object_browser_dock()
        object_browser_dock.setMinimumWidth(250)
        self._make_dock_collapsible(object_browser_dock, "Object Browser")
        outer_mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, object_browser_dock)

        #Works
        ipl_inst_file_dock = self._create_ipl_inst_file_panel()
        ipl_inst_file_dock.setMinimumWidth(250)
        self._make_dock_collapsible(ipl_inst_file_dock, "IPL File Display")
        outer_mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, ipl_inst_file_dock)

        ipl_controls_dock = self._create_ipl_controls_dock()
        ipl_controls_dock.setMinimumWidth(250)
        self._make_dock_collapsible(ipl_controls_dock, "IPL Controls")
        outer_mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, ipl_controls_dock)

        #This breaks the docking.
        #outer_mw.splitDockWidget(object_browser_dock, ipl_inst_file_dock, Qt.Orientation.Vertical)

        # DISABLED (Aug 1 2026)

        #outer_mw.splitDockWidget(ipl_inst_file_dock, control_panel_dock, Qt.Orientation.Vertical)

        main_layout.addWidget(outer_mw)

        # Restore saved outer layout (Files/Models positions around the
        # viewport) - separate from _inner_mw's own ribbon state.
        from PyQt6.QtCore import QTimer as _QTOuter
        _QTOuter.singleShot(400, self._restore_outer_layout)
        self.window_closed.connect(self._save_outer_layout)

        # Status bar (Aug 1 2026)
        status_frame = self._create_status_bar()
        main_layout.addWidget(status_frame)
        register_statusbar(self, status_frame)

        # Apply theme colours to all icons now that UI is fully built
        self._refresh_icons()



    def _focus_search(self, *a, **kw): pass  #vers 1
    def _rename_shadow_shortcut(self, *a, **kw): pass  #vers 1
    def _show_detailed_info(self, *a, **kw): pass  #vers 1
    def _show_surface_info(self, *a, **kw): pass  #vers 1
    def show_help(self, *a, **kw): pass  #vers 1
    def show_settings_dialog(self, *a, **kw): pass  #vers 1

    # Outer layout (Files/Models/Frame Hierarchy/Textures dock widgets
    # around the viewport) - a separate QMainWindow from _inner_mw, so it
    # gets its own save/restore with its own version constant. History:
    # 1 = Files/Models only. 2 = Frame Hierarchy and Textures split out
    # into their own top-level dock widgets too, same as Files.
    _OUTER_LAYOUT_VERSION = 2

    _GAME_LABELS = {'gta3': "GTA III", 'vc': "Vice City", 'sa': "San Andreas"}


    # _export_col_data implemented above (line ~5136) — this stub removed


# Convenience functions














def open_map_workshop(main_window, game_root=None, dat_path=None, force_preload_img=False): #vers 3
    """Open Map Workshop and load a GTA world - either a whole game
    folder (game_root) or one specific .dat file (dat_path), matching
    the signature apps/components/Img_Factory/imgfactory.py's
    open_map_workshop_docked() actually calls this with (game_root
    from Dat Browser's currently-loaded game root; dat_path from
    right-clicking a .dat entry in the Dat Browser tree).
    force_preload_img (Aug 16 2026)"""
    try:
        if main_window and hasattr(main_window, 'main_tab_widget'):
            from PyQt6.QtWidgets import QWidget, QVBoxLayout
            container = QWidget()
            layout = QVBoxLayout(container)
            layout.setContentsMargins(0, 0, 0, 0)
            workshop = ModelWorkshop(container, main_window)
            workshop.setWindowFlags(Qt.WindowType.Widget)
            layout.addWidget(workshop)
            tab_label = "Map Workshop"
            icon = SVGIconFactory.map_app_icon(20)
            idx = main_window.main_tab_widget.addTab(container, icon, tab_label)
            main_window.main_tab_widget.setCurrentIndex(idx)
            if hasattr(main_window, 'register_tool'):
                main_window.register_tool("map", "Map", icon, container, "Map Workshop")
            if hasattr(main_window, '_ensure_tab_area_visible'):
                main_window._ensure_tab_area_visible()
            workshop.show()
        else:
            workshop = ModelWorkshop(main_window=main_window)
            workshop.setWindowFlags(Qt.WindowType.Window)
            workshop.setWindowTitle(f"Map Workshop — {App_name}")
            workshop.resize(1200, 800)
            workshop.show()

        if game_root:
            workshop._load_game_folder(game_root)
        elif dat_path:
            workshop._load_game_dat_file(dat_path, force_preload_img=force_preload_img)

        return workshop
    except Exception as e:
        import traceback; traceback.print_exc()
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Map Workshop error: {e}")
        return None




if __name__ == "__main__":
    import sys
    import traceback

    print(App_name + " Starting.")

    try:
        app = QApplication(sys.argv)
        print("QApplication created")

        workshop = ModelWorkshop()
        print(App_name + " instance created")

        # Also set the QApplication-level icon (Aug 1 2026)
        app.setWindowIcon(workshop.windowIcon())

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

