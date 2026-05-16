#!/usr/bin/env python3
#this belongs in apps/components/Vehicle_Workshop/vehicle_workshop.py - Version: 5
# X-Seti - May08 2026 - Img Factory 1.6 - Vehicle Workshop

"""
Vehicle Workshop — tabbed editor for GTA III/VC/SA vehicle data files.
Tabs: Handling (handling.cfg) | Car Colours (carcols.dat) | Car Mods (carmods.dat SA)

Parsers are self-contained. Handling parser imported from Handling_Editor.
"""
import os, json, sys, requests, threading, struct, re, math, shutil
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field

os.environ.setdefault('QT_QPA_PLATFORM', 'xcb')
os.environ.setdefault('QSG_RHI_BACKEND',  'opengl')

#Adding Standalone
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = Path(current_dir).parents[2]
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QGridLayout, QSplitter, QLabel, QLineEdit,
    QListWidget, QListWidgetItem, QScrollArea, QGroupBox, QTabWidget,
    QDoubleSpinBox, QSpinBox, QComboBox, QCheckBox, QPushButton,
    QFileDialog, QMessageBox, QApplication, QFormLayout, QFrame,
    QTableWidget, QTableWidgetItem, QHeaderView, QAbstractItemView,
    QColorDialog, QProgressBar, QMenu, QSizePolicy, QToolButton,
    QDialog, QFontComboBox, QDialogButtonBox, QTextEdit, QButtonGroup
)

from PyQt6.QtCore import Qt, QPoint, QSize, QThread, pyqtSignal, QTimer

from PyQt6.QtGui import QAction, QBrush, QColor, QFont, QIcon, QImage, QKeySequence, QPainter, QPainterPath, QPen, QPixmap, QPolygon, QShortcut

try:
    from PyQt6.QtOpenGLWidgets import QOpenGLWidget
    #from PyQt6.QtOpenGL import QOpenGLContext
    from PyQt6.QtGui import QSurfaceFormat
    from OpenGL.GL  import *
    from OpenGL.GLU import *
    OPENGL_AVAILABLE = True
    # Request compatibility profile globally — required for fixed-function GL
    # (glMatrixMode, glLightfv, glBegin etc.) on PyQt6 / Python 3.12+
    _fmt = QSurfaceFormat()
    _fmt.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
    _fmt.setVersion(2, 1)
    QSurfaceFormat.setDefaultFormat(_fmt)
except Exception:
    QOpenGLWidget      = QWidget
    OPENGL_AVAILABLE   = False
    print("[Vehicle_Workshop] PyOpenGL not available — install python3-opengl")

# - Path setup
# depends/ = tool-specific only (handling_editor, svg_icons, tool_menu_mixin)
# apps/ = shared (methods, utils, themes) — via project_root when in IMG Factory
_depends = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'depends')
if _depends not in sys.path:
    sys.path.insert(0, _depends)

# AppSettings — apps/utils/ (shipped with standalone tools per architecture)
APPSETTINGS_AVAILABLE = False
try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    try:
        from app_settings_system import AppSettings, SettingsDialog
        APPSETTINGS_AVAILABLE = True
    except ImportError:
        AppSettings = SettingsDialog = None

# SVGIconFactory — depends/ tool-specific copy
ICONS_AVAILABLE = False
try:
    from imgfactory_svg_icons import SVGIconFactory
    ICONS_AVAILABLE = True
except ImportError:
    try:
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        ICONS_AVAILABLE = True
    except ImportError:
        class SVGIconFactory:
            @staticmethod
            def _s(sz=20, c=None): return QIcon()
            open_icon = save_icon = export_icon = import_icon = delete_icon = \
            undo_icon = info_icon = properties_icon = minimize_icon = \
            maximize_icon = close_icon = settings_icon = search_icon = \
            zoom_in_icon = zoom_out_icon = fit_grid_icon = locate_icon = \
            paint_icon = fill_icon = dropper_icon = line_icon = rect_icon = \
            rect_fill_icon = scissors_icon = paste_brush_icon = \
            rotate_cw_icon = rotate_ccw_icon = flip_horz_icon = \
            flip_vert_icon = folder_icon = staticmethod(_s)

# ToolMenuMixin — depends/ tool-specific copy
try:
    from tool_menu_mixin import ToolMenuMixin
except ImportError:
    try:
        from apps.gui.tool_menu_mixin import ToolMenuMixin
    except ImportError:
        class ToolMenuMixin:
            def _build_menus_into_qmenu(self, pm): pass

# GLViewportMixin — embedded in this file
class GLViewportMixin: pass

# HandlingParser — depends/ tool-specific copy
try:
    from handling_editor import HandlingParser, HandlingEntry, VC_FIELDS, HANDLING_FLAGS
    _HANDLING_AVAILABLE = True
except ImportError:
    try:
        from apps.components.Handling_Editor.handling_editor import (
            HandlingParser, HandlingEntry, VC_FIELDS, HANDLING_FLAGS
        )
        _HANDLING_AVAILABLE = True
    except ImportError:
        _HANDLING_AVAILABLE = False

    _HANDLING_AVAILABLE = False

# Parser imports — fall back to local depends/ when running standalone
try:
    from apps.methods.dff_parser import load_dff, DFFParser
    from apps.methods.txd_parser import parse_txd
except ImportError:
    try:
        from apps.components.Vehicle_Workshop.depends.dff_parser import load_dff, DFFParser
    except ImportError:
        load_dff = DFFParser = None
    try:
        from apps.components.Vehicle_Workshop.depends.txd_parser import parse_txd
    except ImportError:
        parse_txd = None

# - Detect standalone vs docked
def _is_standalone():
    import inspect
    frame = inspect.currentframe()
    try:
        for _ in range(10):
            frame = frame.f_back
            if frame is None: break
            if 'imgfactory' in frame.f_code.co_filename.lower(): return False
        return True
    finally:
        del frame

STANDALONE_MODE = _is_standalone()
DEBUG_STANDALONE = False

App_name   = "Vehicle Workshop"
App_build  = "Build 2"
App_auth   = "X-Seti"
config_key = "vehicle_workshop"

##Methods list -
# CarColour.__init__
# CarColsParser.__init__
# CarColsParser.load
# CarColsParser.save
# CarColsParser._detect_game
# CarModEntry.__init__
# CarModsParser.__init__
# CarModsParser.load
# CarModsParser.save
# ColourSwatchGrid.__init__
# ColourSwatchGrid.set_palette
# ColourSwatchGrid.paintEvent
# ColourSwatchGrid.mousePressEvent
# HandlingTab.__init__
# HandlingTab._build_ui
# HandlingTab.load_file
# HandlingTab.save_file
# CarColoursTab.__init__
# CarColoursTab._build_ui
# CarColoursTab.load_file
# CarColoursTab.save_file
# CarColoursTab._on_vehicle_selected
# CarColoursTab._populate_vehicle_colours
# CarColoursTab._on_colour_index_clicked
# CarColoursTab._add_vehicle
# CarColoursTab._delete_vehicle
# CarColoursTab._edit_selected_colour
# CarColoursTab._add_pair
# CarColoursTab._remove_pair
# CarModsTab.__init__
# CarModsTab._build_ui
# CarModsTab.load_file
# CarModsTab.save_file
# CarModsTab._on_vehicle_selected
# CarModsTab._populate_mods
# CarModsTab._add_vehicle
# CarModsTab._delete_vehicle
# CarModsTab._add_mod
# CarModsTab._remove_mod
# VehicleWorkshop.__init__
# VehicleWorkshop._build_menus_into_qmenu
# VehicleWorkshop._open_file
# VehicleWorkshop._save_file
# VehicleWorkshop._save_as
# VehicleWorkshop._open_specific
# VehicleWorkshop.setup_ui
# open_vehicle_workshop

# - WorkshopSettings
class WorkshopSettings:
    """Per-app JSON settings.  Stored at ~/.config/imgfactory/{config_key}.json
    Same pattern as RADSettings / WATSettings across all workshops.
    """
    MAX_RECENT = 10

    DEFAULTS = {
        # Window geometry
        "window_x": -1,  "window_y": -1,
        "window_w": 1400, "window_h": 800,
        # Toolbar / menu
        "show_menubar":            False,
        "menu_style":              "dropdown",  # "dropdown" | "topbar"
        "menu_bar_font_size":      9,
        "menu_bar_height":         22,
        "menu_dropdown_font_size": 9,
        # Status bar
        "show_statusbar":          True,
        # Fonts
        "font_title_family":  "Arial",        "font_title_size":   14,
        "font_panel_family":  "Arial",        "font_panel_size":   10,
        "font_button_family": "Arial",        "font_button_size":  10,
        "font_info_family":   "Courier New",  "font_info_size":     9,
        # Display
        "button_display_mode": "both",         # "both"|"icons"|"text"
        "sidebar_width":       82,
        # Recent files
        "recent_files": [],
    }

    def __init__(self, config_key: str = "gui_workshop"): #vers 1
        cfg = Path.home() / ".config" / "imgfactory"
        cfg.mkdir(parents=True, exist_ok=True)
        self._path = cfg / f"{config_key}.json"
        self._data = dict(self.DEFAULTS)
        self._load()

    def _load(self): #vers 1
        try:
            if self._path.exists():
                self._data.update(
                    {k: v for k, v in json.loads(self._path.read_text()).items()
                     if k in self.DEFAULTS})
        except Exception:
            pass

    def save(self): #vers 1
        try: self._path.write_text(json.dumps(self._data, indent=2))
        except Exception: pass

    def get(self, key, default=None):
        return self._data.get(
            key, default if default is not None else self.DEFAULTS.get(key))

    def set(self, key, value): #vers 1
        if key in self.DEFAULTS:
            self._data[key] = value

    def add_recent(self, path: str): #vers 1
        r = [p for p in self._data.get("recent_files", []) if p != str(path)]
        r.insert(0, str(path))
        self._data["recent_files"] = r[:self.MAX_RECENT]
        self.save()

    def get_recent(self) -> list: #vers 1
        return [p for p in self._data.get("recent_files", [])
                if Path(p).exists()]


# - OpenGL viewport
# DFFViewport replaced by VehicleViewport from methods/dff_viewport
try:
    from apps.methods.dff_viewport import DFFViewport
    from apps.components.Vehicle_Workshop.methods.dff_viewport import VehicleViewport
except ImportError:
    from apps.components.Vehicle_Workshop.methods.dff_viewport import VehicleViewport, DFFViewport

class _CornerOverlay(QWidget):
    """Transparent overlay that draws accent-coloured resize triangles.
    Shared by all GUIWorkshop subclasses — do not modify.
    """
    SIZE = 20

    def __init__(self, parent): #vers 1
        super().__init__(parent)
        for attr in [Qt.WidgetAttribute.WA_TransparentForMouseEvents,
                     Qt.WidgetAttribute.WA_NoSystemBackground,
                     Qt.WidgetAttribute.WA_TranslucentBackground,
                     Qt.WidgetAttribute.WA_AlwaysStackOnTop]:
            self.setAttribute(attr, True)
        self.setWindowFlags(Qt.WindowType.Widget)
        self._hover_corner = None
        self._app_settings = getattr(parent, "app_settings", None)
        self.setGeometry(0, 0, parent.width(), parent.height())
        self._update_mask()


    def _get_ui_color(self, key): #vers 1
        """Return theme-aware QColor. No hardcoded colors - everything via app_settings."""
        from PyQt6.QtGui import QColor
        try:
            app_settings = getattr(self, 'app_settings', None) or \
                getattr(getattr(self, 'main_window', None), 'app_settings', None)
            if app_settings and hasattr(app_settings, 'get_ui_color'):
                return app_settings.get_ui_color(key)
        except Exception:
            pass
        pal = self.palette()
        if key == 'viewport_bg':
            return pal.color(pal.ColorRole.Base)
        if key == 'viewport_text':
            return pal.color(pal.ColorRole.PlaceholderText)
        if key == 'border':
            return pal.color(pal.ColorRole.Mid)
        return pal.color(pal.ColorRole.WindowText)

    def _update_mask(self): #vers 1
        from PyQt6.QtGui import QRegion
        s = self.SIZE; w, h = self.width(), self.height()
        region = QRegion()
        for pts in [
            [QPoint(0,0),   QPoint(s,0),   QPoint(0,s)],
            [QPoint(w,0),   QPoint(w-s,0), QPoint(w,s)],
            [QPoint(0,h),   QPoint(s,h),   QPoint(0,h-s)],
            [QPoint(w,h),   QPoint(w-s,h), QPoint(w,h-s)],
        ]:
            region = region.united(QRegion(QPolygon(pts)))
        self.setMask(region)

    def update_state(self, hover_corner, app_settings): #vers 1
        self._hover_corner = hover_corner
        self._app_settings = app_settings
        self.update()

    def setGeometry(self, *a): #vers 1
        super().setGeometry(*a); self._update_mask()

    def resizeEvent(self, ev): #vers 1
        super().resizeEvent(ev); self._update_mask()

    def paintEvent(self, ev): #vers 1
        s = self.SIZE
        try:
            accent = QColor(
                self._app_settings.get_theme_colors()
                    .get("accent_primary", "#4682FF"))
        except Exception:
            accent = self._get_ui_color('accent_primary') if hasattr(self,'_get_ui_color') else QColor(70,130,255)
        accent.setAlpha(200)
        hc = QColor(accent); hc.setAlpha(255)
        w, h = self.width(), self.height()
        corners = {
            "top-left":     [(0,0),   (s,0),   (0,s)],
            "top-right":    [(w,0),   (w-s,0), (w,s)],
            "bottom-left":  [(0,h),   (s,h),   (0,h-s)],
            "bottom-right": [(w,h),   (w-s,h), (w,h-s)],
        }
        p = QPainter(self)
        for name, pts in corners.items():
            p.setBrush(hc if name == self._hover_corner else accent)
            p.setPen(Qt.PenStyle.NoPen)
            p.drawPolygon(QPolygon([QPoint(x, y) for x, y in pts]))
        p.end()


class _ToolbarMixin:
    """Toolbar + Settings dialog + theme methods.
    Mixed into GUIWorkshop — not used standalone.
    """

    #    Toolbar creation

    def _create_toolbar(self): #vers 3
        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setFixedHeight(self.toolbarheight)
        self.toolbar.setObjectName("titlebar")
        self.toolbar.installEventFilter(self)
        self.toolbar.setMouseTracking(True)
        self.titlebar = self.toolbar   # alias for drag detection

        toolbar = QHBoxLayout(self.toolbar)
        toolbar.setContentsMargins(5, 4, 5, 4)
        toolbar.setSpacing(4)
        ic = self._get_icon_color()

        # Helper: create a fixed-size icon button
        def _ibtn(icon_fn, tip, slot):
            btn = QPushButton()
            try:
                btn.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
                btn.setIconSize(QSize(20, 20))
            except Exception:
                pass
            btn.setFixedSize(35, 35)
            btn.setToolTip(tip)
            btn.clicked.connect(slot)
            return btn

        #    Left: [Menu] [Settings]
        self.menu_btn = QPushButton("Menu")
        self.menu_btn.setFont(self.button_font)
        self.menu_btn.setMinimumHeight(28)
        self.menu_btn.setMaximumHeight(35)
        self.menu_btn.setToolTip(
            "Show menu (dropdown or top bar — set in Settings)")
        self.menu_btn.clicked.connect(self._on_menu_btn_clicked)
        toolbar.addWidget(self.menu_btn)

        self.settings_btn = QPushButton()
        try:
            self.settings_btn.setIcon(SVGIconFactory.settings_icon(20, ic))
            self.settings_btn.setIconSize(QSize(20, 20))
        except Exception:
            pass
        self.settings_btn.setText(" Settings")
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setMinimumHeight(28)
        self.settings_btn.setMaximumHeight(35)
        self.settings_btn.setToolTip(
            "Workshop settings — Fonts, Display, Menu, About")
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        toolbar.addWidget(self.settings_btn)

        toolbar.addSpacing(4)
        toolbar.addStretch()

        #    Centre: title
        self.title_label = QLabel(self.App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.title_label.setObjectName("title_label")
        toolbar.addWidget(self.title_label)
        toolbar.addStretch()
        toolbar.addSpacing(4)

        #    Right: action buttons
        self.open_btn = _ibtn("open_icon", "Open Ctrl+O", self._open_file) #TODO does not always work when switching tabs, should load the needed type for other tabs.
        self.save_btn = _ibtn("save_icon", "Save Ctrl+S", self._save_file)
        self.export_btn = _ibtn("export_icon", "Export", self._export_file)
        self.import_btn = _ibtn("import_icon", "Import", self._import_file)
        for btn in (self.open_btn, self.save_btn,
                  self.export_btn, self.import_btn):
            toolbar.addWidget(btn)

        toolbar.addSpacing(6)

        self.undo_btn = _ibtn("undo_icon", "Undo Ctrl+Z", self._undo)
        toolbar.addWidget(self.undo_btn)
        toolbar.addSpacing(4)

        # [ℹ] Info — About this workshop
        self.info_btn = _ibtn("info_icon", "About / Info", self._show_about)
        toolbar.addWidget(self.info_btn)

        # [⚙] Cog — Global AppSettings theme dialog
        self.properties_btn = _ibtn(
            "properties_icon",
            "Global Theme Settings  (AppSettings)",
            self._launch_theme_settings)
        toolbar.addWidget(self.properties_btn)
        toolbar.addSpacing(4)

        # [_] [⬜] [✕] — Window controls (standalone only)
        if self.standalone_mode:
            self.minimize_btn = _ibtn("minimize_icon", "Minimise",
                                      self.showMinimized)
            self.maximize_btn = _ibtn("maximize_icon", "Maximise / Restore",
                                      self._toggle_maximize)
            self.close_btn    = _ibtn("close_icon",    "Close",
                                      self.close)
            for btn in (self.minimize_btn, self.maximize_btn, self.close_btn):
                toolbar.addWidget(btn)
        else:
            self.dock_btn = QPushButton("D")
            self.dock_btn.setFixedSize(35, 35)
            self.dock_btn.setToolTip("Dock / Undock")
            self.dock_btn.clicked.connect(self.toggle_dock_mode)
            toolbar.addWidget(self.dock_btn)

        return self.toolbar


    def _toggle_assembly_mode(self, enabled: bool): #vers 2
        self.viewport.set_assembly_mode(enabled)
        if enabled and self._dff_model:
            damaged = getattr(self,'_damage_mode',False)
            self.viewport.load_all_geometries(
                self._dff_model.geometries,
                [g.materials for g in self._dff_model.geometries],
                self._dff_model.frames,
                self._dff_model.atomics,
                damaged=damaged)
        self._geom_list.setEnabled(not enabled)

    def _toggle_damage_mode(self, enabled: bool): #vers 2
        self._damage_mode = enabled
        m = getattr(self, '_dff_model', None)
        if m and m.frames and m.atomics:
            self.viewport.load_all_geometries(
                m.geometries, [g.materials for g in m.geometries],
                m.frames, m.atomics, damaged=enabled)
            self.viewport.update()

    def _toggle_lod_mode(self, enabled: bool): #vers 1
        self.viewport.set_show_lod(enabled)
        if getattr(self,'_assemble_btn',None) and self._assemble_btn.isChecked():
            self._toggle_assembly_mode(True)

    def _toggle_animation(self, enabled: bool): #vers 2
        """Toggle door open/close animation cycle."""
        vp = self.viewport
        if enabled:
            if not hasattr(self, '_anim_timer') or not self._anim_timer:
                self._anim_timer = QTimer(self)  # parent=self → auto-stopped on delete
            self._anim_angle = 0.0
            speed = getattr(self, '_anim_speed', 1.0)
            def _step():
                try:
                    self._anim_angle = (self._anim_angle + speed * 2.0) % 180.0
                    vp._door_angles = getattr(vp, '_door_angles', {})
                    for dname in getattr(self, '_door_open', set()):
                        vp._door_angles[dname] = self._anim_angle if self._anim_angle < 90 else 180 - self._anim_angle
                    vp.update()
                except RuntimeError:
                    if hasattr(self, '_anim_timer') and self._anim_timer:
                        self._anim_timer.stop()
            self._anim_timer.timeout.connect(_step)
            self._anim_timer.start(33)
        else:
            if hasattr(self, '_anim_timer') and self._anim_timer:
                self._anim_timer.stop()
            vp.update()

    def _set_anim_speed(self, value: int): #vers 1
        """Set animation speed from slider."""
        self._anim_speed = value / 10.0

    def _toggle_door(self, name: str, open: bool): #vers 1
        """Track which doors are toggled open."""
        self._door_open = getattr(self, '_door_open', set())
        if open: self._door_open.add(name)
        else:    self._door_open.discard(name)
        self.viewport.update()

    def _set_wheel_steer(self, angle: int): #vers 1
        """Set front wheel steering angle in viewport."""
        if hasattr(self.viewport, 'set_wheel_heading'):
            self.viewport.set_wheel_heading(angle)
        else:
            self.viewport._wheel_steer = angle
            self.viewport.update()

    def _cycle_carcol(self): #vers 2
        """Cycle through carcols.dat pairs for the current vehicle. Asks for file if not found."""
        stem = os.path.splitext(os.path.basename(
            getattr(self, '_current_dff_path', '')))[0].lower()
        if not stem: return
        game_root = self._get_game_root()
        # Try cached carcols path first
        mw = getattr(self, 'main_window', None)
        carcols_path = getattr(mw, 'vehicle_data_paths', {}).get('carcols', '')
        if not carcols_path:
            # Try standard locations
            for sub in ('data/carcols.dat','DATA/CARCOLS.DAT','data/CARCOLS.DAT'):
                p = os.path.join(game_root, sub) if game_root else ''
                if p and os.path.isfile(p):
                    carcols_path = p; break
        if not carcols_path:
            # Ask user
            carcols_path, _ = QFileDialog.getOpenFileName(
                self, "Open carcols.dat", game_root or '',
                "Carcols (carcols.dat CARCOLS.DAT);;All Files (*)")
            if not carcols_path: return
            # Cache it
            if mw:
                if not hasattr(mw, 'vehicle_data_paths'):
                    mw.vehicle_data_paths = {}
                mw.vehicle_data_paths['carcols'] = carcols_path
        try:
            from apps.methods.carcols_parser import parse_carcols
            data = parse_carcols(carcols_path)
            pairs = data.get_colours(stem)
            if not pairs:
                self._set_status(f"No carcols entry for '{stem}'")
                return
            idx = getattr(self, '_carcol_idx', -1) + 1
            if idx >= len(pairs): idx = 0
            self._carcol_idx = idx
            p1, p2 = pairs[idx]
            self._set_paint_pair(p1, p2)
            if hasattr(self, '_carcol_btn'):
                self._carcol_btn.setToolTip(
                    f"Pair {idx+1}/{len(pairs)} — "
                    f"Pri: rgb({int(p1[0]*255)},{int(p1[1]*255)},{int(p1[2]*255)}) "
                    f"Sec: rgb({int(p2[0]*255)},{int(p2[1]*255)},{int(p2[2]*255)})")
            self._set_status(f"Carcol {idx+1}/{len(pairs)} for {stem}")
        except Exception as e:
            self._set_status(f'Carcol error: {e}')

    def _toggle_com_indicator(self): #vers 1
        """Toggle Centre of Mass indicator in viewport."""
        show = not getattr(self, '_show_com', False)
        self._show_com = show
        # Read CoM from handling if available
        if show:
            try:
                m = getattr(self, '_dff_model', None)
                stem = os.path.splitext(os.path.basename(getattr(self,'_current_dff_path','')))[0].lower()
                if hasattr(self,'_tab_handling') and self._tab_handling._parser:
                    entry = next((e for e in self._tab_handling._parser.entries
                                  if e.name.lower()==stem), None)
                    if entry and len(entry.values) > 7:
                        cx,cy,cz = float(entry.values[4]),float(entry.values[5]),float(entry.values[6])
                        self.viewport._com_pos = (cx,cy,cz)
                        self._set_status(f'CoM: ({cx:.2f}, {cy:.2f}, {cz:.2f})')
            except Exception:
                pass
        else:
            self.viewport._com_pos = None
        self.viewport.update()

    def _toggle_dummy_overlay(self): #vers 1
        """Toggle dummy frame position markers in viewport."""
        self.viewport._show_dummies = not getattr(self.viewport, '_show_dummies', False)
        self.viewport.update()

    def _toggle_suspension_vis(self): #vers 1
        """Toggle suspension travel range visualisation."""
        self.viewport._show_suspension = not getattr(self.viewport, '_show_suspension', False)
        self.viewport.update()

    def _toggle_seat_overlay(self): #vers 1
        """Toggle seat position markers."""
        self.viewport._show_seats = not getattr(self.viewport, '_show_seats', False)
        self.viewport.update()

    def _toggle_bounds(self): #vers 1
        """Toggle bounding box display."""
        self.viewport._show_bounds = not getattr(self.viewport, '_show_bounds', False)
        self.viewport.update()

    def _export_dff(self): #vers 1
        """Export current DFF geometry (stub — routes to model workshop)."""
        self._set_status("Export: use Model Workshop for DFF export")

    def _parse_sa_vehicles_ide(self, path: str): #vers 5
        """Parse vehicles.ide/default.ide for wheel scales - handles GTA3, VC and SA."""
        self._sa_vehicle_data = {}
        try:
            mw = getattr(self, 'main_window', None)
            ide_db = getattr(mw, 'ide_db', None) if mw else None
            raw = getattr(ide_db, '_game', None) or getattr(ide_db, 'game', None)
            game_ver = str(raw).lower().split('.')[-1] if raw else 'vc'
            # Detect from filename: vehicles.ide = SA (VC uses default.ide)
            fname_lower = os.path.basename(path).lower()
            if fname_lower == 'vehicles.ide' and game_ver in ('vc', 'gta3'):
                game_ver = 'sa'
            elif fname_lower == 'default.ide' and game_ver == 'sa':
                game_ver = 'vc'  # SA doesn't use default.ide

            # GTA3 wheelModelId -> wheel name (IDs 160-166)
            GTA3_WHEEL_ID = {
                160: 'wheel_sport',    161: 'wheel_saloon',
                162: 'wheel_lightvan', 163: 'wheel_classic',
                164: 'wheel_alloy',    165: 'wheel_lighttruck',
                166: 'wheel_smallcar',
            }
            # VC wheelModelId -> wheel name (IDs 237-256)
            VC_WHEEL_ID = {
                237: 'wheel_rim',        238: 'wheel_offroad',
                239: 'wheel_truck',      250: 'wheel_sport',
                251: 'wheel_saloon',     252: 'wheel_lightvan',
                253: 'wheel_classic',    254: 'wheel_alloy',
                255: 'wheel_lighttruck', 256: 'wheel_smallcar',
            }
            WHEEL_ID_MAP = GTA3_WHEEL_ID if game_ver == 'gta3' else VC_WHEEL_ID

            # Field positions:
            # GTA3: id,model,txd,type,handlingId,gameName,class,freq,level,compRules,wheelId,wheelScale
            #        0   1    2   3    4           5        6     7    8     9         10      11
            # VC:   id,model,txd,type,handlingId,gameName,animFile,class,freq,level,compRules,wheelId,wheelScale
            #        0   1    2   3    4           5        6        7     8    9     10        11      12
            # SA:   id,model,txd,type,handlingId,gameName,animFile,class,freq,level,compRules,wheelId,frontScale,rearScale
            #        0   1    2   3    4           5        6        7     8    9     10        11      12         13
            if game_ver == 'gta3':
                scale_idx = 11; class_idx = 6; type_idx = 3; wheel_id_idx = 10
            else:  # vc and sa same layout
                scale_idx = 12; class_idx = 7; type_idx = 3; wheel_id_idx = 11

            SA_CLASS_WHEEL = {
                'richfamily':'wheel_saloon', 'executive':'wheel_saloon',
                'normal':'wheel_saloon',     'poorfamily':'wheel_saloon',
                'worker':'wheel_truck',      'ignore':'wheel_truck',
                'big':'wheel_lighttruck',    'taxi':'wheel_saloon',
                'offroad':'wheel_offroad',   'sport':'wheel_sport',
                'lightvan':'wheel_lightvan', 'van':'wheel_lightvan',
                'lighttruck':'wheel_lighttruck', 'classic':'wheel_classic',
                'bicycle':'wheel_rim',       'bike':'wheel_rim',
            }

            with open(path, 'r', errors='ignore') as f:
                in_cars = False
                for line in f:
                    line = line.strip()
                    if not line or line.startswith('#') or line.startswith(';'):
                        continue
                    if line.lower() == 'cars':
                        in_cars = True; continue
                    if line.lower() == 'end':
                        in_cars = False; continue
                    if not in_cars: continue
                    parts = [p.strip() for p in line.split(',')]
                    if len(parts) < scale_idx + 1: continue
                    try:
                        model      = parts[1].strip().lower()
                        veh_type   = parts[type_idx].strip().lower() if len(parts) > type_idx else ''
                        veh_class  = parts[class_idx].strip().lower() if len(parts) > class_idx else ''
                        front_scale = float(parts[scale_idx])
                        rear_scale  = float(parts[scale_idx+1]) if (
                            game_ver == 'sa' and len(parts) > scale_idx+1) else front_scale

                        # Wheel type from numeric ID first
                        wheel_model = 'wheel_saloon'
                        if len(parts) > wheel_id_idx:
                            try:
                                wid = int(parts[wheel_id_idx])
                                wheel_model = WHEEL_ID_MAP.get(wid, '')
                            except ValueError:
                                pass
                        # Fall back to class if no ID match
                        if not wheel_model:
                            if veh_type in ('bike','mtruck') or 'bike' in veh_type:
                                wheel_model = 'wheel_rim'
                            elif 'van' in veh_type or veh_type == 'truck':
                                wheel_model = 'wheel_lightvan'
                            else:
                                wheel_model = SA_CLASS_WHEEL.get(veh_class, 'wheel_saloon')

                        self._sa_vehicle_data[model] = {
                            'front_scale': front_scale,
                            'rear_scale':  rear_scale,
                            'wheel_model': wheel_model,
                        }
                    except (ValueError, IndexError):
                        continue
            self._set_status(f"IDE ({game_ver}): {len(self._sa_vehicle_data)} vehicles")
        except Exception as e:
            self._set_status(f"IDE parse error: {e}")
    def _toggle_show_wheels(self, enabled: bool): #vers 2
        """Load wheels.DFF + wheels.txd if needed and trigger re-assembly."""
        vp = self.viewport
        vp._show_wheels = enabled
        if enabled:
            game_root = self._find_game_root()
            wheels_path = getattr(vp, '_wheels_model_path', '')
            # Find wheels.DFF
            if not wheels_path and game_root:
                import os as _os
                for p in [
                    _os.path.join(game_root,'models','generic','wheels.DFF'),
                    _os.path.join(game_root,'models','generic','wheels.dff'),
                    _os.path.join(game_root,'models','Generic','wheels.DFF'),
                    _os.path.join(game_root,'models','Generic','wheels.dff'),
                ]:
                    if _os.path.isfile(p):
                        wheels_path = p; break
            if wheels_path and not getattr(vp, '_wheels_model', None):
                vp.load_wheels_dff(wheels_path)
            # Log what wheel type will be used
            wheel_data = vp._get_wheel_geom_data()
            if wheel_data:
                wm = wheel_data[4] if len(wheel_data) > 4 else []
                needed = [getattr(m,'texture_name','') for m in (wm or []) if getattr(m,'texture_name','')]
                self._set_status(f'Wheels: {vp._wheel_type} | textures: {", ".join(needed[:4])}')
            # Find and load wheels.txd (SA) or generic.txd contains wheel textures (VC/GTA3)
            if game_root:
                import os as _os
                # VC/GTA3/LC: wheel textures are in misc.txd (+ generic.txd)
                for fname in ['misc.txd','MISC.TXD','wheels.txd','wheels.TXD','generic.txd']:
                    p = _os.path.join(game_root,'models', fname)
                    if _os.path.isfile(p):
                        try:
                            from apps.methods.txd_parser import parse_txd
                            with open(p,'rb') as f: data = f.read()
                            textures = parse_txd(data)
                            if textures:
                                self._set_status(f'{fname}: {", ".join(t["name"] for t in textures[:6])}')
                                from PyQt6.QtCore import QTimer
                                QTimer.singleShot(150, lambda t=textures: vp._upload_textures(t, additive=True))
                        except Exception as e:
                            self._set_status(f'{fname} error: {e}')
        # Rebuild assembly
        m = getattr(self, '_dff_model', None)
        if m and m.frames and m.atomics:
            vp.load_all_geometries(
                m.geometries, [g.materials for g in m.geometries],
                m.frames, m.atomics,
                damaged=getattr(self,'_damage_mode',False))
        vp.update()
        if enabled:
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(100, self._auto_load_shared_txds)

    def _update_paint_btns(self): #vers 1
        vp = self.viewport
        def _css(rgb): return f'background-color:rgb({int(rgb[0]*255)},{int(rgb[1]*255)},{int(rgb[2]*255)});color:{"#000" if sum(rgb)>1.5 else "#fff"}'
        if hasattr(self,'_paint1_btn'): self._paint1_btn.setStyleSheet(_css(vp._paint1))
        if hasattr(self,'_paint2_btn'): self._paint2_btn.setStyleSheet(_css(vp._paint2))

    def _pick_paint1(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp = self.viewport
        col = QColorDialog.getColor(QColor(int(vp._paint1[0]*255),int(vp._paint1[1]*255),int(vp._paint1[2]*255)),self)
        if col.isValid(): vp._paint1=(col.redF(),col.greenF(),col.blueF()); self._update_paint_btns(); vp.update()

    def _pick_paint2(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp = self.viewport
        col = QColorDialog.getColor(QColor(int(vp._paint2[0]*255),int(vp._paint2[1]*255),int(vp._paint2[2]*255)),self)
        if col.isValid(): vp._paint2=(col.redF(),col.greenF(),col.blueF()); self._update_paint_btns(); vp.update()

    def _set_paint_pair(self, p1, p2): #vers 1
        self.viewport._paint1=p1; self.viewport._paint2=p2
        self._update_paint_btns(); self.viewport.update()

    def _load_vehicle_meta(self, vehicle_name: str): #vers 2
        """Load vehicles.ide (wheel type) and carcols colours for vehicle."""
        game_root = self._get_game_root()
        # vehicles.ide — wheel type
        if game_root:
            try:
                from apps.methods.vehicles_ide_parser import get_vehicle_info
                entry = get_vehicle_info(game_root, vehicle_name)
                if entry and entry.wheel_model:
                    wheel_dff = entry.wheel_dff_name()
                    self.viewport._wheel_type = wheel_dff
                    self._set_status(f'IDE: {vehicle_name} txd={entry.txd_name} wheel={wheel_dff}')
            except Exception:
                pass
        # handling.cfg or SA vehicles.ide — front/rear tyre scales
        try:
            stem = vehicle_name.lower()
            # SA: wheel scales are in vehicles.ide
            sa_data = getattr(self, '_sa_vehicle_data', {})
            self._set_status(f'Meta: sa_data={len(sa_data)} entries, stem={stem!r}, found={stem in sa_data}')
            if sa_data and stem in sa_data:
                self.viewport._wheel_front_scale = sa_data[stem]['front_scale']
                self.viewport._wheel_rear_scale  = sa_data[stem]['rear_scale']
                wheel_model = sa_data[stem].get('wheel_model', 'wheel_saloon_l0')
                if not wheel_model.endswith('_l0'):
                    wheel_model = wheel_model + '_l0'
                self.viewport._wheel_type = wheel_model
                self._set_status(f'SA IDE: wheel={wheel_model} front={sa_data[stem]["front_scale"]:.2f} rear={sa_data[stem]["rear_scale"]:.2f}')
            else:
                # VC/GTA3: wheel scales in handling.cfg fields 38/40
                # SA handling.cfg only has 36 fields - wheel scale is in vehicles.ide
                if hasattr(self, '_tab_handling') and hasattr(self._tab_handling, '_parser'):
                    p = self._tab_handling._parser
                    if p and p.entries:
                        entry = next((e for e in p.entries if e.name.lower() == stem), None)
                        if entry and len(entry.values) > 40:
                            try:
                                front_scale = float(entry.values[38])
                                rear_scale  = float(entry.values[40])
                                self.viewport._wheel_front_scale = front_scale
                                self.viewport._wheel_rear_scale  = rear_scale
                                self._set_status(f'Wheel scale: front={front_scale:.2f} rear={rear_scale:.2f}')
                            except (ValueError, IndexError):
                                pass
                        elif entry and len(entry.values) <= 40:
                            # SA handling.cfg - no wheel scale, use IDE default 1.0
                            self.viewport._wheel_front_scale = 1.0
                            self.viewport._wheel_rear_scale  = 1.0
        except Exception:
            pass
        self._load_carcols(vehicle_name)

    def _get_game_root(self): #vers 1
        """Get game root from viewport or main_window."""
        game_root = ''
        if hasattr(self.viewport,'_find_game_root'):
            game_root = self.viewport._find_game_root()
        if not game_root:
            mw = self.main_window
            if mw:
                for attr in ('game_root','_game_root','game_directory'):
                    val=getattr(mw,attr,None)
                    if val and os.path.isdir(str(val)): game_root=str(val); break
        return game_root

    def _load_carcols(self, vehicle_name: str): #vers 1

        lay = getattr(self,'_carcols_lay',None)
        if not lay: return
        while lay.count():
            item = lay.takeAt(0)
            if item.widget(): item.widget().deleteLater()
        game_root = self._get_game_root()
        if not game_root: return
        try:
            from apps.methods.carcols_parser import get_vehicle_colours
            pairs = get_vehicle_colours(game_root, vehicle_name)
            if not pairs: return
            lbl = QLabel(f'Carcols ({len(pairs)} pairs)')
            lbl.setFont(self.infobar_font); lay.addWidget(lbl)
            for i,(p1,p2) in enumerate(pairs[:8]):
                row=QWidget(); rl=QHBoxLayout(row)
                rl.setContentsMargins(0,0,0,0); rl.setSpacing(2)
                def _css(rgb): return f'background-color:rgb({int(rgb[0]*255)},{int(rgb[1]*255)},{int(rgb[2]*255)});min-height:16px;border:1px solid #333'
                b1=QPushButton(); b1.setFixedSize(20,16); b1.setStyleSheet(_css(p1))
                b2=QPushButton(); b2.setFixedSize(20,16); b2.setStyleSheet(_css(p2))
                b1.setToolTip(f'Primary: rgb({int(p1[0]*255)},{int(p1[1]*255)},{int(p1[2]*255)})')
                b2.setToolTip(f'Secondary: rgb({int(p2[0]*255)},{int(p2[1]*255)},{int(p2[2]*255)})')
                ab=QPushButton(f'#{i+1}'); ab.setFixedHeight(16); ab.setFont(self.infobar_font)
                ab.setToolTip(f'Apply colour pair {i+1}')
                ab.clicked.connect(lambda _=False,a=p1,b=p2: self._set_paint_pair(a,b))
                rl.addWidget(b1); rl.addWidget(b2); rl.addWidget(ab,1)
                lay.addWidget(row)
        except Exception as e:
            print(f'[carcols] {e}')


    def _set_mode(self, mode: str): #vers 1
        self.viewport.set_render_mode(mode)


    # - file operations
    def _open_dff(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Open DFF", self._last_dir,
            "DFF Models (*.dff);;All Files (*)")
        if path:
            self._last_dir = os.path.dirname(path)
            self.WS.set('last_dir', self._last_dir)
            self.load_dff(path)
            for ext in ('.txd','.TXD'):
                txd = os.path.splitext(path)[0]+ext
                if os.path.isfile(txd): self.load_txd(txd); break

    def _open_txd(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Open TXD", self._last_dir,
            "TXD Files (*.txd);;All Files (*)")
        if path:
            self._last_dir = os.path.dirname(path)
            self.load_txd(path)

    def load_dff(self, path: str): #vers 4
        try:
            from apps.methods.dff_parser import load_dff
            self._show_progress(True)
            self._set_status(f"Parsing {os.path.basename(path)}…")
            from PyQt6.QtWidgets import QApplication
            QApplication.processEvents()
            model = load_dff(path)
            if not model or not model.geometries:
                self._set_status(f"Failed: {os.path.basename(path)}"); return
            self._dff_model = model
            self._current_dff_path = path
            # Clear texture cache when loading new DFF
            self.viewport.clear_textures()
            self._tex_list.clear()
            self.WS.add_recent(path); self.WS.save()
            self._populate_geom_list()
            self._geom_list.setCurrentRow(0)
            self._set_status(
                f"Loaded: {os.path.basename(path)} — "
                f"{len(model.geometries)} geometries, {len(model.frames)} frames")
            # Populate frame hierarchy tree
            self._populate_frame_tree()
            # Load vehicles.ide info (wheel type) + carcols colours
            stem = os.path.splitext(os.path.basename(path))[0]
            from PyQt6.QtCore import QTimer
            def _meta_then_wheels(s=stem):
                self._load_vehicle_meta(s)
                # Reset carcol index for new vehicle
                self._carcol_idx = -1
                # Rebuild wheels if they were showing
                if getattr(self.viewport, '_show_wheels', False):
                    self._toggle_show_wheels(True)
            QTimer.singleShot(100, _meta_then_wheels)
            # Auto-load shared TXDs after primary TXD is loaded
            QTimer.singleShot(500, self._auto_load_shared_txds)
        except Exception as e:
            import traceback; traceback.print_exc()
            self._set_status(f"Error: {e}")
        finally:
            self._show_progress(False)

    def _strip_tex_suffix(self, name: str) -> str: #vers 2
        """Strip GTA texture suffix.
        Handles: buildrt4_fehihwm (alpha suffix) and vehiclegeneric256 (numeric size suffix)."""
        n = re.sub(r'_[a-z]{4,8}$', '', name)
        n = re.sub(r'\d+$', '', n)
        return n

    def _get_ide_db(self): #vers 1
        """Return IDEDatabase from mw.ide_db if loaded, else None."""
        mw = getattr(self, 'main_window', None)
        db = getattr(mw, 'ide_db', None)
        if db and getattr(db, '_loaded', False) and db.model_map:
            return db
        db2 = getattr(getattr(mw, 'dat_browser', None), '_ide_db', None)
        if db2 and db2.model_map:
            return db2
        return None

    def _lookup_txd_for_stem(self, stem: str) -> str: #vers 1
        """Return txd_name from IDE DB for model stem, or empty string."""
        db = self._get_ide_db()
        if not db:
            return ''
        obj = db.model_map.get(stem.lower())
        if obj:
            return obj.txd_name.lower()
        # Try stripped stem
        base = self._strip_tex_suffix(stem.lower())
        if base != stem.lower():
            obj = db.model_map.get(base)
            if obj:
                return obj.txd_name.lower()
        return ''

    def _collect_needed_textures(self): #vers 2
        """Return set of texture names the current DFF needs.
        Stores both full name and suffix-stripped base for fuzzy matching."""
        if not self._dff_model: return set()
        needed = set()
        for g in self._dff_model.geometries:
            for mat in g.materials:
                name = (mat.texture_name or '').strip().lower()
                if not name:
                    continue
                needed.add(name)
                base = self._strip_tex_suffix(name)
                if base != name:
                    needed.add(base)
        return needed

    def _upload_txd_additive(self, path: str): #vers 1
        """Load a TXD and upload textures WITHOUT clearing existing ones."""
        try:
            from apps.methods.txd_parser import parse_txd
            from PyQt6.QtGui import QIcon, QImage, QPixmap
            from PyQt6.QtWidgets import QListWidgetItem
            from PyQt6.QtCore import Qt
            with open(path, 'rb') as f: data = f.read()
            textures = parse_txd(data)
            if not textures: return 0
            # Only upload textures not already loaded
            new_textures = [t for t in textures
                            if t['name'].lower() not in self.viewport._tex_ids]
            if new_textures:
                # Additive upload — don't clear existing
                self.viewport.makeCurrent()
                from OpenGL.GL import (glGenTextures, glBindTexture, GL_TEXTURE_2D,
                    glTexParameteri, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR,
                    GL_TEXTURE_MAG_FILTER, GL_LINEAR, GL_TEXTURE_WRAP_S,
                    GL_TEXTURE_WRAP_T, GL_REPEAT, GL_CLAMP_TO_EDGE, GL_MIRRORED_REPEAT,
                    glTexImage2D, GL_RGBA, GL_UNSIGNED_BYTE, glGenerateMipmap, glDeleteTextures)
                def _rw_wrap(rw):
                    if rw == 2: return GL_CLAMP_TO_EDGE
                    if rw == 3: return GL_MIRRORED_REPEAT
                    return GL_REPEAT
                for t in new_textures:
                    name = t['name'].lower()
                    rgba = t.get('rgba_data', b'')
                    w = t.get('width', 0); h = t.get('height', 0)
                    if not (rgba and w > 0 and h > 0): continue
                    wrap_s = _rw_wrap(t.get('wrap_u', 1))
                    wrap_t = _rw_wrap(t.get('wrap_v', 1))
                    gl_id = glGenTextures(1)
                    glBindTexture(GL_TEXTURE_2D, gl_id)
                    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
                    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
                    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wrap_s)
                    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrap_t)
                    try:
                        glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,w,h,0,GL_RGBA,GL_UNSIGNED_BYTE,rgba)
                        glGenerateMipmap(GL_TEXTURE_2D)
                        self.viewport._tex_ids[name] = gl_id
                        self.viewport._tex_wrap[name] = (t.get('wrap_u',1), t.get('wrap_v',1))
                    except Exception:
                        glDeleteTextures(1,[gl_id])
                self.viewport.doneCurrent()
                # Add to texture list
                for t in new_textures:
                    item = QListWidgetItem(f"{t['name']}  {t['width']}\xd7{t['height']}")
                    rgba=t.get('rgba_data',b''); w=t.get('width',0); h=t.get('height',0)
                    if rgba and w>0 and h>0:
                        try:
                            img=QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888)
                            px=QPixmap.fromImage(img).scaled(32,32,
                                Qt.AspectRatioMode.KeepAspectRatio,
                                Qt.TransformationMode.SmoothTransformation)
                            item.setIcon(QIcon(px))
                        except Exception: pass
                    self._tex_list.addItem(item)
                self.viewport.update()
            return len(new_textures)
        except Exception as e:
            return 0

    def _find_game_root(self): #vers 1
        """Try to find the GTA SA game root from the DFF path or main_window settings."""
        # From main_window app_settings
        mw = self.main_window
        if mw:
            for attr in ('game_root','_game_root','game_directory'):
                val = getattr(mw, attr, None)
                if val and os.path.isdir(val): return val
            if hasattr(mw,'app_settings'):
                settings = mw.app_settings
                for key in ('game_root','game_directory','sa_root'):
                    val = getattr(settings,'get',lambda k,d=None:d)(key)
                    if val and os.path.isdir(str(val)): return str(val)
        # Walk up from DFF path looking for models/ or data/ folder
        if self._current_dff_path:
            p = os.path.dirname(self._current_dff_path)
            for _ in range(8):
                if os.path.isdir(os.path.join(p,'models')) and os.path.isdir(os.path.join(p,'data')):
                    return p
                p = os.path.dirname(p)
        return ''

    def _auto_load_shared_txds(self): #vers 5
        """Find shared TXDs — uses preloaded cache first, worker for remainder."""
        if not self._dff_model: return
        needed  = self._collect_needed_textures()
        if getattr(self.viewport, '_show_wheels', False):
            wheel_data = self.viewport._get_wheel_geom_data()
            if wheel_data:
                wm = wheel_data[4] if len(wheel_data) > 4 else []
                for mat in (wm or []):
                    tname = getattr(mat, 'texture_name', '') or ''
                    if tname:
                        needed.add(tname.lower())
                        base = self._strip_tex_suffix(tname.lower())
                        if base != tname.lower(): needed.add(base)
        already = set(self.viewport._tex_ids.keys())
        missing = needed - already
        if not missing: return

        # Step 0: Use preloaded shared cache from DAT Browser (instant, no worker)
        mw = getattr(self, 'main_window', None)
        preloaded = getattr(mw, 'shared_vehicle_textures', {})
        if preloaded:
            hits = []
            for name in list(missing):
                t = preloaded.get(name)
                if not t:
                    t = preloaded.get(self._strip_tex_suffix(name))
                if t:
                    hits.append(t); missing.discard(name)
                    missing.discard(self._strip_tex_suffix(name))
            if hits:
                self.viewport._upload_textures(hits, additive=True)
                self.viewport.update()
                if hasattr(mw, 'log_message'):
                    mw.log_message(f"+{len(hits)} shared textures loaded")
            if not missing:
                return  # everything satisfied from cache

        # Snapshot everything the worker needs — no shared mutable state
        game_root  = self._find_game_root()
        dff_dir    = os.path.dirname(self._current_dff_path) if self._current_dff_path else ''
        dff_stem   = os.path.splitext(os.path.basename(self._current_dff_path))[0].lower() if self._current_dff_path else ''
        img        = getattr(self, '_current_img', None)
        ide_txd    = self._lookup_txd_for_stem(dff_stem)  # from IDE DB
        _ide_db    = self._get_ide_db()
        game_ver   = getattr(_ide_db, '_game', None) or getattr(_ide_db, 'game', 'vc')

        from PyQt6.QtCore import QThread, pyqtSignal as _sig

        strip_suffix = self._strip_tex_suffix
        viewer_ref = self

        class _Worker(QThread):
            found = _sig(list)   # emits list of {'name','rgba_data','width','height','format'}
            status = _sig(str)

            def run(self):
                from apps.methods.txd_parser import parse_txd
                import tempfile
                collected = []
                
                miss = set(missing)  # local copy
                # Build suffix-stripped alias map: base_name -> full_name
                # so we can match TXD textures against suffixed DFF names
                alias = {}
                for n in list(miss):
                    base = strip_suffix(n)
                    if base != n:
                        alias[base] = n  # base -> suffixed

                def _try_txd_data(data):
                    nonlocal miss
                    try:
                        textures = parse_txd(data)
                        hits = []
                        for t in textures:
                            tname = t['name'].lower()
                            if not (t.get('rgba_data') and t['width'] > 0):
                                continue
                            tbase = strip_suffix(tname)
                            if tname in miss:
                                # Exact match
                                hits.append(t)
                            elif tname in alias:
                                # TXD has base name, miss has suffixed name
                                t2 = dict(t); t2['name'] = alias[tname]
                                hits.append(t2); hits.append(t)
                            elif tbase in miss:
                                # TXD has suffixed name (armytruk8bit128), miss has base (armytruk8bit)
                                hits.append(t)
                            elif tbase in alias:
                                # Both suffixed — map to what DFF expects
                                t2 = dict(t); t2['name'] = alias[tbase]
                                hits.append(t2); hits.append(t)
                        if hits:
                            collected.extend(hits)
                            for t in hits:
                                miss.discard(t['name'].lower())
                                miss.discard(strip_suffix(t['name'].lower()))
                        return len(hits)
                    except Exception:
                        return 0

                # 0. Shared TXDs from models/ — game-version aware
                # GTA3/LC: models/generic.txd, models/Generic/wheels.DFF
                # VC:      models/generic.txd, models/Generic/wheels.DFF
                # SA:      models/generic/vehicle.txd, models/generic/wheels.txd, models/generic/wheels.DFF
                if game_root:
                    m = os.path.join(game_root, 'models')
                    if game_ver == 'sa':
                        shared_txds = [
                            os.path.join(m, 'generic', 'vehicle.txd'),
                            os.path.join(m, 'generic', 'wheels.txd'),
                            os.path.join(m, 'misc.txd'),  # SA has misc.txd in models/
                            # Additional SA shared vehicle TXDs on disk
                            os.path.join(m, 'generic', 'vehiclecommon.txd'),
                            os.path.join(m, 'generic', 'vehiclegeneric.txd'),
                            os.path.join(m, 'generic', 'vehiclegrunge.txd'),
                            os.path.join(m, 'generic', 'vehiclelights.txd'),
                            os.path.join(m, 'generic', 'vehicletyres.txd'),
                        ]
                        # Also scan models/txd/ if it exists
                        txd_dir = os.path.join(m, 'txd')
                        if os.path.isdir(txd_dir):
                            for fn in os.listdir(txd_dir):
                                if fn.lower().startswith('vehicle') and fn.lower().endswith('.txd'):
                                    shared_txds.append(os.path.join(txd_dir, fn))
                        wheel_dffs = [
                            os.path.join(m, 'generic', 'wheels.DFF'),
                            os.path.join(m, 'generic', 'wheels.dff'),
                        ]
                    else:
                        # GTA3 / VC / LC: generic.txd + misc.txd in models/, wheels.DFF in models/Generic/
                        shared_txds = [
                            os.path.join(m, 'generic.txd'),
                            os.path.join(m, 'particle.txd'),
                            os.path.join(m, 'misc.txd'),
                            os.path.join(m, 'MISC.TXD'),
                            os.path.join(m, 'wheels.txd'),
                            os.path.join(m, 'wheels.TXD'),
                        ]
                        wheel_dffs = [
                            os.path.join(m, 'Generic', 'wheels.DFF'),
                            os.path.join(m, 'Generic', 'wheels.dff'),
                            os.path.join(m, 'generic', 'wheels.DFF'),
                            os.path.join(m, 'generic', 'wheels.dff'),
                        ]
                    for p in shared_txds:
                        if os.path.isfile(p) and miss:
                            try:
                                with open(p, 'rb') as f: _try_txd_data(f.read())
                            except Exception: pass
                    for wp in wheel_dffs:
                        if os.path.isfile(wp):
                            viewer_ref._wheels_model_path = wp; break
                    if not miss:
                        if collected: self.found.emit(collected)
                        return

                # 1. Same directory as DFF
                if dff_dir and miss:
                    try:
                        for fn in os.listdir(dff_dir):
                            if not fn.lower().endswith('.txd'): continue
                            if fn[:-4].lower() == dff_stem: continue
                            if not miss: break
                            try:
                                with open(os.path.join(dff_dir,fn),'rb') as f: _try_txd_data(f.read())
                            except Exception: pass
                    except Exception: pass

                # 1a. IDE DB lookup — exact TXD name from parsed IDE files
                if miss and img and hasattr(img, 'entries') and ide_txd:
                    txd_key = ide_txd if ide_txd.endswith('.txd') else ide_txd + '.txd'
                    txd_map = {e.name.lower(): e for e in img.entries if e.name.lower().endswith('.txd')}
                    entry = txd_map.get(txd_key)
                    if entry:
                        self.status.emit(f'IDE: loading {txd_key}...')
                        try:
                            data = img.read_entry_data(entry)
                            if data: _try_txd_data(data)
                        except Exception: pass

                # 1b. GTA3 only: vehicle TXDs are in models/txd.img not gta3.img
                if miss and img and getattr(img, 'file_path', '') and game_ver == 'gta3':
                    img_dir = os.path.dirname(img.file_path)
                    txd_img_path = os.path.join(img_dir, 'txd.img')
                    if os.path.isfile(txd_img_path) and ide_txd:
                        try:
                            from apps.methods.img_core_classes import IMGFile
                            sib = IMGFile(txd_img_path)
                            txd_key = ide_txd if ide_txd.endswith('.txd') else ide_txd + '.txd'
                            for e in sib.entries:
                                if e.name.lower() == txd_key:
                                    data = sib.read_entry_data(e)
                                    if data: _try_txd_data(data)
                                    break
                        except Exception: pass

                # 1b. Current IMG (gta3.img) — look for vehicle*.txd entries
                # SA stores vehiclegeneric256 etc inside gta3.img as separate TXDs
                if miss and img and hasattr(img,'entries'):
                    self.status.emit('Scanning gta3.img for vehicle textures...')
                    txd_map={e.name.lower():e for e in img.entries if e.name.lower().endswith('.txd')}
                    # Known SA shared vehicle TXD names inside gta3.img
                    candidates=['vehiclecommon.txd','vehicle.txd','vehicles.txd',
                                 'vehiclegeneric.txd','vehiclegrunge.txd',
                                 'vehiclelights.txd','vehicletyres.txd',
                                 # SA uses full names with size suffix inside gta3.img
                                 'vehiclegeneric256.txd','vehiclegrunge256.txd',
                                 'vehiclelights128.txd','vehicletyres128.txd',
                                 'carcommon.txd','carpback.txd','carplate.txd',]
                    # Also try prefix match for any vehicle*.txd or car*.txd
                    candidates += [n for n in txd_map if n.startswith('vehicle') and n not in candidates]
                    candidates += [n for n in txd_map if n.startswith('car') and n not in candidates]
                    tried=set()
                    for cand in candidates:
                        if not miss: break
                        if cand in txd_map and cand not in tried:
                            tried.add(cand)
                            try:
                                data=img.read_entry_data(txd_map[cand])
                                if data: _try_txd_data(data)
                            except Exception: pass

                # 2. Current IMG — ONLY look up exact stem.txd entries, no full scan
                if miss and img and hasattr(img,'entries'):
                    miss_names = ', '.join(sorted(miss)[:8])
                    self.status.emit(f'Missing textures: {miss_names}')
                    # Build a map of entry names first (fast, no data read)
                    txd_entries = {e.name.lower(): e for e in img.entries
                                   if e.name.lower().endswith('.txd')}
                    # Only try TXDs whose name hints at containing missing textures
                    # Heuristic: match first 6 chars of texture name to TXD stem
                    tried = set()
                    for tex_name in list(miss):
                        if not miss: break
                        stem6 = tex_name[:6].lower()
                        for txd_name, entry in txd_entries.items():
                            if txd_name[:-4] in tried: continue
                            if txd_name.startswith(stem6) or stem6.startswith(txd_name[:4]):
                                tried.add(txd_name[:-4])
                                try:
                                    data = img.read_entry_data(entry)
                                    if data: _try_txd_data(data)
                                except Exception: pass
                                break

                # 3. Fallback — scan all vehicle*.txd in models/ subdirs on disk
                if miss and game_root:
                    m2 = os.path.join(game_root, 'models')
                    for subdir in ('', 'generic', 'txd'):
                        d = os.path.join(m2, subdir) if subdir else m2
                        if not os.path.isdir(d): continue
                        for fn in sorted(os.listdir(d)):
                            if not miss: break
                            fnl = fn.lower()
                            if not fnl.endswith('.txd'): continue
                            if not any(fnl.startswith(p) for p in ('vehicle','carpl','carpb','misc','generic')):
                                continue
                            try:
                                with open(os.path.join(d, fn),'rb') as f: _try_txd_data(f.read())
                            except Exception: pass
                if collected:
                    found_names = ', '.join(t['name'] for t in collected[:8])
                    self.status.emit(f'Found textures: {found_names}')
                    self.found.emit(collected)
                if miss:
                    self.status.emit(f'Still missing: {", ".join(sorted(miss)[:8])}')

        self._shared_txd_worker = _Worker()
        self._shared_txd_worker.status.connect(self._set_status)
        self._shared_txd_worker.found.connect(self._on_shared_txds_found)
        self._shared_txd_worker.finished.connect(lambda: self._show_progress(False))
        self._show_progress(True)
        self._shared_txd_worker.start()

    def _on_shared_txds_found(self, textures: list): #vers 2
        """Receive shared textures from worker thread and upload to GL on main thread."""
        # Load wheels.DFF if path was discovered by worker
        wheels_path = getattr(self.viewport,'_wheels_model_path',None)
        if wheels_path and not getattr(self.viewport,'_wheels_model',None):
            self.viewport.load_wheels_dff(wheels_path)
        if not textures: return
        # Filter already-loaded
        new = [t for t in textures if t['name'].lower() not in self.viewport._tex_ids]
        if not new: return
        # Defer upload until GL context ready
        def _do_shared_upload():
            try:
                vp = self.viewport
                if hasattr(vp, 'isValid') and not vp.isValid():
                    from PyQt6.QtCore import QTimer
                    QTimer.singleShot(200, _do_shared_upload)
                    return
                vp._upload_textures(new, additive=True)
                vp.update()
            except Exception:
                pass
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(50, _do_shared_upload)
        # Add to tex list
        from PyQt6.QtGui import QIcon, QImage, QPixmap
        from PyQt6.QtWidgets import QListWidgetItem
        from PyQt6.QtCore import Qt
        for t in new:
            item = QListWidgetItem(f"{t['name']}  {t['width']}\xd7{t['height']}")
            rgba=t.get('rgba_data',b''); w=t.get('width',0); h=t.get('height',0)
            if rgba and w>0 and h>0:
                try:
                    img=QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888)
                    px=QPixmap.fromImage(img).scaled(32,32,
                        Qt.AspectRatioMode.KeepAspectRatio,
                        Qt.TransformationMode.SmoothTransformation)
                    item.setIcon(QIcon(px))
                except Exception: pass
            self._tex_list.addItem(item)
        self._set_status(f'+{len(new)} shared textures loaded')
        self.viewport.update()

    def load_txd(self, path: str): #vers 2
        try:
            from apps.methods.txd_parser import parse_txd
            from PyQt6.QtGui import QIcon, QImage, QPixmap
            from PyQt6.QtWidgets import QListWidgetItem
            from PyQt6.QtCore import Qt, QTimer
            with open(path,'rb') as f: data=f.read()
            textures = parse_txd(data)
            if not textures:
                self._set_status(f'No textures: {os.path.basename(path)}'); return
            # GL upload — defer until GL context is initialized
            def _do_upload():
                try:
                    vp = self.viewport
                    if hasattr(vp, 'isValid') and not vp.isValid():
                        QTimer.singleShot(200, _do_upload)
                        return
                    vp._upload_textures(textures)
                    vp.update()
                except Exception as ue:
                    self._set_status(f'GL upload: {ue}')
            QTimer.singleShot(50, _do_upload)
            # Texture list with thumbnails
            self._tex_list.clear()
            for t in textures:
                item = QListWidgetItem(f"{t['name']}  {t['width']}\xd7{t['height']}")
                rgba=t.get('rgba_data',b''); w=t.get('width',0); h=t.get('height',0)
                if rgba and w>0 and h>0:
                    try:
                        img=QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888)
                        px=QPixmap.fromImage(img).scaled(32,32,
                            Qt.AspectRatioMode.KeepAspectRatio,
                            Qt.TransformationMode.SmoothTransformation)
                        item.setIcon(QIcon(px))
                    except Exception: pass
                self._tex_list.addItem(item)
            self._set_status(f'Textures: {len(textures)} from {os.path.basename(path)}')
        except Exception as e:
            import traceback; traceback.print_exc()
            self._set_status(f'TXD error: {e}')

    def load_img(self, img): #vers 3
        """Populate IMG list with vehicle DFF entries only (from handling.cfg / IDE DB).
        Also auto-loads vehicle data files from mw.vehicle_data_paths if not yet loaded."""
        self._current_img = img
        # Auto-load vehicle data files if not already loaded
        mw = getattr(self, 'main_window', None)
        vdata = getattr(mw, 'vehicle_data_paths', {})
        if vdata and not getattr(self, '_handling_loaded', False):
            for key in ('handling', 'carcols', 'carmods', 'cargrp', 'vehicles_ide'):
                p = vdata.get(key, '')
                if p and os.path.isfile(p):
                    self._open_file(p)
            self._handling_loaded = True
        self._img_list.clear()
        if not img or not hasattr(img, 'entries'):
            return
        # Build vehicle name filter from handling parser or mw.vehicle_names
        vehicle_names = set()
        try:
            if hasattr(self, '_tab_handling') and hasattr(self._tab_handling, '_parser'):
                p = self._tab_handling._parser
                if p and p.entries:
                    vehicle_names = {e.name.lower() for e in p.entries if e.name}
        except Exception:
            pass
        if not vehicle_names:
            mw = getattr(self, 'main_window', None)
            vehicle_names = getattr(mw, 'vehicle_names', set())
        for entry in img.entries:
            if not entry.name.lower().endswith('.dff'):
                continue
            stem = entry.name.rsplit('.', 1)[0].lower()
            # Only show if known vehicle, or no filter available (show all)
            if vehicle_names and stem not in vehicle_names:
                continue
            stem, ext = os.path.splitext(entry.name)
            display = re.sub(r'_[a-z0-9]{6,12}$', '', stem, flags=re.IGNORECASE) + ext
            item = QListWidgetItem(display)
            item.setData(Qt.ItemDataRole.UserRole, entry)
            self._img_list.addItem(item)
        count = self._img_list.count()
        total = sum(1 for e in img.entries if e.name.lower().endswith('.dff'))
        self._set_status(f"IMG: {count} vehicles ({total} DFFs total)")

    def _on_img_entry_dclicked(self, item): #vers 2
        """Double-click IMG entry — extract and load DFF + auto TXD with progress."""
        entry = item.data(Qt.ItemDataRole.UserRole)
        img   = getattr(self, '_current_img', None)
        if not entry or not img:
            return
        from PyQt6.QtWidgets import QApplication
        from PyQt6.QtCore import Qt as _Qt
        name = entry.name
        stem0, ext0 = os.path.splitext(name)
        display_name = re.sub(r'_[a-z0-9]{6,12}$', '', stem0, flags=re.IGNORECASE) + ext0
        try:
            import tempfile
            # Show busy cursor + progress bar immediately
            QApplication.setOverrideCursor(_Qt.CursorShape.WaitCursor)
            self._show_progress(True)
            self._set_status(f"Loading {display_name}…")
            QApplication.processEvents()

            # Step 1: Extract DFF
            self._set_status(f"Extracting {name}…")
            QApplication.processEvents()
            data = img.read_entry_data(entry)
            if not data:
                return
            tmp_dir  = tempfile.mkdtemp()
            dff_path = os.path.join(tmp_dir, name)
            with open(dff_path,'wb') as f: f.write(data)

            # Step 2: Parse + load DFF, switch to 3D Preview tab first
            self._set_status(f"Parsing {name}…")
            QApplication.processEvents()
            if hasattr(self, '_tabs'):
                self._tabs.setCurrentIndex(0)
                QApplication.processEvents()
            self.load_dff(dff_path)
            QApplication.processEvents()

            # Step 3: Find + extract TXD
            stem = os.path.splitext(name)[0].lower()
            txd_entry = next((e for e in img.entries if e.name.lower() == stem + '.txd'), None)
            if txd_entry:
                self._set_status(f"Loading textures for {stem}…")
                QApplication.processEvents()
                txd_data = img.read_entry_data(txd_entry)
                if txd_data:
                    txd_path = os.path.join(tmp_dir, txd_entry.name)
                    with open(txd_path,'wb') as f: f.write(txd_data)
                    self.load_txd(txd_path)
                    QApplication.processEvents()

        except Exception as ex:
            self._set_status(f"Load error: {ex}")
        finally:
            self._show_progress(False)
            QApplication.restoreOverrideCursor()


    #    Menu button handler
    def _on_menu_btn_clicked(self): #vers 2
        pm = QMenu(self)
        self._build_menus_into_qmenu(pm)
        btn = getattr(self, 'menu_btn', None)
        if btn:
            pm.exec(btn.mapToGlobal(btn.rect().bottomLeft()))
        else:
            pm.exec(self.cursor().pos())


    def _show_dropdown_menu(self): #vers 1
        """Pop up the workshop menus as a QMenu below the [Menu] button."""
        menu = QMenu(self)
        self._build_menus_into_qmenu(menu)
        btn = getattr(self, "menu_btn", None)
        pos = btn.mapToGlobal(btn.rect().bottomLeft()) if btn else self.cursor().pos()
        menu.exec(pos)

    def _show_popup_menu(self): #vers 1
        self._show_dropdown_menu()

    #    [ℹ] Info — About dialog

    def _show_about(self): #vers 1
        """[ℹ] button — show About / Info for this workshop."""
        author = getattr(self, "App_author",      "X-Seti")
        year   = getattr(self, "App_year",        "2026")
        desc   = getattr(self, "App_description", "")
        QMessageBox.information(self, f"About {self.App_name}",
            f"{self.App_name}   {self.App_build}\n\n"
            + (f"{desc}\n\n" if desc else "")
            + f"Copyright \u00a9 {year}  {author}\n"
              f"Part of IMG Factory 1.6 — a GTA modding toolkit.")

    def _open_app_settings(self): #vers 1
        try:
            try:
                from apps.utils.app_settings_system import SettingsDialog
            except ImportError:
                from app_settings_system import SettingsDialog
            if self.app_settings:
                dlg = SettingsDialog(self.app_settings, self)
                dlg.exec()
            else:
                self._show_workshop_settings()
        except Exception as e:
            self._set_status(f'Settings error: {e}')
            self._show_workshop_settings()

    #    [⚙] Cog — Global AppSettings theme dialog

    def _launch_theme_settings(self): #vers 1
        """[⚙] Cog — opens the global AppSettings / SettingsDialog.
        Identical pattern to radar_workshop._launch_theme_settings.
        """
        try:
            if not APPSETTINGS_AVAILABLE:
                QMessageBox.information(self, "Theme",
                    "AppSettings not available in this environment.")
                return
            if not self.app_settings:
                self.app_settings = AppSettings()
            dialog = SettingsDialog(self.app_settings, self)
            dialog.themeChanged.connect(lambda _: self._apply_theme())
            if dialog.exec():
                self._apply_theme()
                self._refresh_icons()
        except Exception as e:
            QMessageBox.warning(self, "Theme Error",
                f"Could not open theme settings:\n{e}")

    #    [Settings] — Workshop-local settings dialog

    def _show_workshop_settings(self): #vers 1
        """[Settings] button — workshop-local settings.
        Tabs: Fonts / Display / Menu / About
        Reads/writes WorkshopSettings (per-app JSON).
        """
        dlg = QDialog(self)
        dlg.setWindowTitle(f"{self.App_name} — Settings")
        dlg.setMinimumSize(520, 460)
        try:
            from apps.core.theme_utils import apply_dialog_theme
            apply_dialog_theme(dlg, self.main_window)
        except Exception:
            pass

        showws  = QVBoxLayout(dlg)
        tabs = QTabWidget()
        ws  = self.WS

        #    Tab 1: Fonts
        ft  = QWidget(); fl = QVBoxLayout(ft)

        def _font_row(label, fam_key, sz_key, def_fam, def_sz, mn=7, mx=32):
            grp = QGroupBox(label); row = QHBoxLayout(grp)
            fc = QFontComboBox()
            fc.setCurrentFont(__import__("PyQt6.QtGui", fromlist=["QFont"])
                              .QFont(ws.get(fam_key, def_fam)))
            sc = QSpinBox(); sc.setRange(mn, mx)
            sc.setValue(ws.get(sz_key, def_sz))
            sc.setSuffix(" pt"); sc.setFixedWidth(75)
            row.addWidget(fc); row.addWidget(sc)
            fl.addWidget(grp)
            return fc, sc

        fc_tit, sc_tit = _font_row("Title Font",
            "font_title_family",  "font_title_size",  "Arial",       14, 10, 32)
        fc_pan, sc_pan = _font_row("Panel / Header Font",
            "font_panel_family",  "font_panel_size",  "Arial",       10)
        fc_btn, sc_btn = _font_row("Button Font",
            "font_button_family", "font_button_size", "Arial",       10)
        fc_inf, sc_inf = _font_row("Info Bar Font",
            "font_info_family",   "font_info_size",   "Courier New",  9)
        fl.addStretch()
        tabs.addTab(ft, "Fonts")

        #    Tab 2: Display
        dt = QWidget(); dl = QVBoxLayout(dt)

        bm_grp = QGroupBox("Button Display Mode"); bm_lo = QVBoxLayout(bm_grp)
        bm_cb  = QComboBox()
        bm_cb.addItems(["Icons + Text", "Icons Only", "Text Only"])
        bm_cb.setCurrentIndex(
            {"both":0,"icons":1,"text":2}.get(ws.get("button_display_mode","both"),0))
        bm_lo.addWidget(bm_cb)
        bm_lo.addWidget(QLabel("Restart required to change button mode.",
                               styleSheet="color:#888;font-style:italic;"))
        dl.addWidget(bm_grp)

        sb_grp = QGroupBox("Status Bar"); sb_lo = QVBoxLayout(sb_grp)
        sb_chk = QCheckBox("Show status bar at bottom")
        sb_chk.setChecked(bool(ws.get("show_statusbar", True)))
        sb_lo.addWidget(sb_chk)
        dl.addWidget(sb_grp)

        sw_grp = QGroupBox("Sidebar"); sw_lo = QVBoxLayout(sw_grp)
        from PyQt6.QtWidgets import QFormLayout
        sw_form = QFormLayout()
        sw_spin = QSpinBox(); sw_spin.setRange(60,200)
        sw_spin.setValue(ws.get("sidebar_width", 82)); sw_spin.setSuffix(" px")
        sw_form.addRow("Sidebar width:", sw_spin)
        sw_lo.addLayout(sw_form)
        dl.addWidget(sw_grp)

        dl.addStretch()
        tabs.addTab(dt, "Display")

        #    Tab 3: Menu
        mt = QWidget(); ml = QVBoxLayout(mt)

        ms_grp = QGroupBox("Menu Style"); ms_lo = QVBoxLayout(ms_grp)
        ms_cb  = QComboBox()
        ms_cb.addItems(["Dropdown  ☰  (default)", "Top menu bar"])
        ms_cb.setCurrentIndex(
            0 if ws.get("menu_style","dropdown") == "dropdown" else 1)
        ms_lo.addWidget(ms_cb)
        ms_lo.addWidget(QLabel("Restart required to switch menu style.",
                               styleSheet="color:#888;font-style:italic;"))
        ml.addWidget(ms_grp)

        mf_grp = QGroupBox("Menu Font Size"); mf_lo = QFormLayout(mf_grp)
        mf_dd  = QSpinBox(); mf_dd.setRange(7,16)
        mf_dd.setValue(ws.get("menu_dropdown_font_size",9)); mf_dd.setSuffix(" pt")
        mf_lo.addRow("Dropdown font:", mf_dd)
        mf_bh  = QSpinBox(); mf_bh.setRange(18,40)
        mf_bh.setValue(ws.get("menu_bar_height",22)); mf_bh.setSuffix(" px")
        mf_lo.addRow("Bar height:", mf_bh)
        ml.addWidget(mf_grp)
        ml.addStretch()
        tabs.addTab(mt, "Menu")

        #    Tab 4: About
        at  = QWidget(); al = QVBoxLayout(at)
        atx = QTextEdit(); atx.setReadOnly(True)
        author = getattr(self, "App_author",      "X-Seti")
        year   = getattr(self, "App_year",        "2026")
        desc   = getattr(self, "App_description", "GUIWorkshop — IMG Factory 1.6")
        atx.setHtml(
            f"<h2>{self.App_name}</h2>"
            f"<p><b>Build:</b> {self.App_build}</p>"
            f"<p>{desc}</p>"
            f"<hr>"
            f"<p>Copyright &copy; {year} <b>{author}</b></p>"
            f"<p>Part of <b>IMG Factory 1.6</b> — a GTA modding toolkit.</p>"
            f"<p style='color:#888;'>Not affiliated with Rockstar Games "
            f"or Take-Two Interactive.</p>")
        al.addWidget(atx)
        tabs.addTab(at, "About")

        #    Tab: Vehicle (game path, viewport settings)
        vt = QWidget(); vl = QVBoxLayout(vt)

        game_grp = QGroupBox("GTA Game Root"); gl = QHBoxLayout(game_grp)
        game_edit = QLineEdit(ws.get("game_root", ""))
        game_edit.setPlaceholderText("e.g. /home/user/GTASA-PC")
        browse_btn = QPushButton("Browse…")
        def _browse_game():
            from PyQt6.QtWidgets import QFileDialog
            d = QFileDialog.getExistingDirectory(dlg, "Select GTA Game Root", game_edit.text())
            if d: game_edit.setText(d)
        browse_btn.clicked.connect(_browse_game)
        gl.addWidget(game_edit, 1); gl.addWidget(browse_btn)
        vl.addWidget(game_grp)

        vp_grp = QGroupBox("3D Viewport"); vpl = QVBoxLayout(vp_grp)
        grid_cb = QCheckBox("Show grid on startup")
        grid_cb.setChecked(ws.get("viewport_grid", True))
        cull_cb = QCheckBox("Backface culling on startup")
        cull_cb.setChecked(ws.get("viewport_cull", True))
        vpl.addWidget(grid_cb); vpl.addWidget(cull_cb)
        vl.addWidget(vp_grp)
        vl.addStretch()
        tabs.addTab(vt, "Vehicle")

        #    Dialog buttons
        showws.addWidget(tabs)
        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok |
            QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        showws.addWidget(btns)

        if dlg.exec() != QDialog.DialogCode.Accepted:
            return

        #    Save
        ws.set("font_title_family",        fc_tit.currentFont().family())
        ws.set("font_title_size",          sc_tit.value())
        ws.set("font_panel_family",        fc_pan.currentFont().family())
        ws.set("font_panel_size",          sc_pan.value())
        ws.set("font_button_family",       fc_btn.currentFont().family())
        ws.set("font_button_size",         sc_btn.value())
        ws.set("font_info_family",         fc_inf.currentFont().family())
        ws.set("font_info_size",           sc_inf.value())
        ws.set("button_display_mode",      ["both","icons","text"][bm_cb.currentIndex()])
        ws.set("show_statusbar",           sb_chk.isChecked())
        ws.set("sidebar_width",            sw_spin.value())
        ws.set("menu_style",               "dropdown" if ms_cb.currentIndex()==0 else "topbar")
        ws.set("menu_dropdown_font_size",  mf_dd.value())
        ws.set("menu_bar_height",          mf_bh.value())
        ws.set("game_root",                game_edit.text())
        ws.set("viewport_grid",            grid_cb.isChecked())
        ws.set("viewport_cull",            cull_cb.isChecked())
        ws.save()

        # Live-apply without restart where possible
        self._load_fonts_from_settings()
        if hasattr(self, "title_label"):
            self.title_label.setFont(self.title_font)
        if hasattr(self, "_status_widget"):
            self._status_widget.setVisible(ws.get("show_statusbar", True))
        if hasattr(self, "_sidebar_frame"):
            self._sidebar_frame.setFixedWidth(ws.get("sidebar_width", 82))
        self._set_status("Settings saved.")

    #    Theme helpers

    def _get_icon_color(self) -> str: #vers 1
        """Returns text_primary from current theme."""
        if APPSETTINGS_AVAILABLE and self.app_settings:
            try:
                return self.app_settings.get_theme_colors().get(
                    "text_primary", "#e0e0e0")
            except Exception:
                pass
        bg = self.palette().window().color()
        return "#e0e0e0" if bg.lightness() < 128 else "#202020"

    def _get_accent_color(self) -> str: #vers 1
        """Returns accent_primary from current theme."""
        if APPSETTINGS_AVAILABLE and self.app_settings:
            try:
                return self.app_settings.get_theme_colors().get(
                    "accent_primary", "#4682FF")
            except Exception:
                pass
        return "#4682FF"

    def _apply_theme(self): #vers 1
        """Apply QSS from AppSettings."""
        if self.app_settings:
            try:
                qss = self.app_settings.get_stylesheet()
                if qss: self.setStyleSheet(qss)
            except Exception:
                pass

    def _refresh_icons(self): #vers 1
        """Called on theme change — re-apply theme and rebuild toolbar icons."""
        self._apply_theme()
        if hasattr(self, "_corner_overlay"):
            self._corner_overlay.update_state(self.hover_corner, self.app_settings)
        ic = self._get_icon_color()
        for btn_name, icon_fn in {
            "open_btn":       "open_icon",
            "save_btn":       "save_icon",
            "export_btn":     "export_icon",
            "import_btn":     "import_icon",
            "undo_btn":       "undo_icon",
            "info_btn":       "info_icon",
            "properties_btn": "properties_icon",
            "settings_btn":   "settings_icon",
        }.items():
            btn = getattr(self, btn_name, None)
            if btn:
                try: btn.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
                except Exception: pass
        if self.standalone_mode:
            for btn_name, icon_fn in {
                "minimize_btn": "minimize_icon",
                "maximize_btn": "maximize_icon",
                "close_btn":    "close_icon",
            }.items():
                btn = getattr(self, btn_name, None)
                if btn:
                    try: btn.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
                    except Exception: pass


#
# SECTION 3 — Layout: setup_ui, left panel, centre panel, right panel, status
#

class _LayoutMixin:
    """Panel creation and layout.
    Mixed into GUIWorkshop — not used standalone.
    Override any _create_* method in your subclass to replace that panel.
    """

    def get_content_margins(self): #vers 1
        return (self.contmergina, self.contmerginb, self.contmerginc, self.contmergind)

    def get_panel_margins(self): #vers 1
        return (self.panelmergina, self.panelmerginb, self.panelmerginc, self.panelmergind)

    def get_tab_margins(self): #vers 1
        return (self.tabmerginsa, self.tabmerginsb, self.tabmerginsc, self.tabmerginsd)

    def setup_ui(self): #vers 2
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(*self.get_content_margins())
        main_layout.setSpacing(self.setspacing)

        # Viewport must exist before toolbar (toolbar buttons reference it)
        self.viewport = VehicleViewport()
        self.viewport.app_settings = self.app_settings

        main_layout.addWidget(self._create_toolbar())

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.addWidget(self._create_left_panel())
        splitter.addWidget(self._create_centre_panel())
        splitter.addWidget(self._create_right_panel())
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 5)
        splitter.setStretchFactor(2, 0)
        splitter.setSizes([200, 800, 180])
        main_layout.addWidget(splitter, 1)
        main_layout.addWidget(self._create_status_bar())

    def _create_left_panel(self): #vers 2
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(180); panel.setMaximumWidth(280)
        outer = QVBoxLayout(panel)
        outer.setContentsMargins(*self.get_panel_margins())
        outer.setSpacing(2)


        # Vertical splitter — all three sections resizable
        splitter = QSplitter(Qt.Orientation.Vertical)
        outer.addWidget(splitter, 1)


        # - IMG Entries section
        img_sec = QWidget(); img_lay = QVBoxLayout(img_sec); img_lay.setContentsMargins(0,0,0,0); img_lay.setSpacing(2)
        img_lay.addWidget(self._make_section_header("IMG Entries (DFF)", self._filter_img_list))
        self._img_list = QListWidget()
        self._img_list.setFont(self.panel_font)
        self._img_list.itemDoubleClicked.connect(self._on_img_entry_dclicked)
        img_lay.addWidget(self._img_list)
        splitter.addWidget(img_sec)


        # - Geometries section
        geom_sec = QWidget(); gl = QVBoxLayout(geom_sec); gl.setContentsMargins(0,0,0,0); gl.setSpacing(2)
        gl.addWidget(self._make_section_header("Geometries"))
        self._geom_list = QListWidget()
        self._geom_list.setFont(self.panel_font)
        self._geom_list.currentRowChanged.connect(self._on_geom_selected)
        gl.addWidget(self._geom_list)
        splitter.addWidget(geom_sec)


        # - Textures section with thumbnails
        tex_sec = QWidget(); tl = QVBoxLayout(tex_sec); tl.setContentsMargins(0,0,0,0); tl.setSpacing(2)
        tl.addWidget(self._make_section_header("Textures"))
        self._tex_list = QListWidget()
        self._tex_list.setFont(self.panel_font)
        self._tex_list.setIconSize(QSize(32,32))
        self._tex_list.setViewMode(QListWidget.ViewMode.ListMode)
        tl.addWidget(self._tex_list)
        splitter.addWidget(tex_sec)

        # - Frame Hierarchy section with visibility checkboxes
        frame_sec = QWidget(); fl = QVBoxLayout(frame_sec)
        fl.setContentsMargins(0,0,0,0); fl.setSpacing(2)
        fl.addWidget(self._make_section_header('Frame Hierarchy'))
        from PyQt6.QtWidgets import QTreeWidget, QTreeWidgetItem
        self._frame_tree = QTreeWidget()
        self._frame_tree.setFont(self.panel_font)
        self._frame_tree.setHeaderHidden(True)
        self._frame_tree.setColumnCount(1)
        self._frame_tree.itemChanged.connect(self._on_frame_visibility_changed)
        fl.addWidget(self._frame_tree)
        splitter.addWidget(frame_sec)

        splitter.setSizes([180, 120, 100, 120])

        # Open DFF / Open TXD buttons at bottom of left panel

        btn_row = QHBoxLayout(); btn_row.setSpacing(4)
        from PyQt6.QtWidgets import QToolButton
        ic=self._get_icon_color()
        for label,tip,cb,iname in [
                ('Open DFF','Open DFF model',self._vw_pick_dff,'open'),
                ('Open TXD','Open TXD textures',self._vw_pick_txd,'open')]:
            b=QToolButton(); b.setFixedHeight(28); b.setFont(self.panel_font); b.setToolTip(tip)
            b.setText(label)
            try:
                ico=getattr(SVGIconFactory,f'{iname}_icon',None)
                if ico: b.setIcon(ico(14,ic)); b.setIconSize(QSize(14,14)); b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            except Exception: pass
            b.setSizePolicy(QSizePolicy.Policy.Expanding,QSizePolicy.Policy.Fixed)
            b.clicked.connect(cb); btn_row.addWidget(b)
        self._vw_open_dff_btn=btn_row.itemAt(0).widget()
        self._vw_open_txd_btn=btn_row.itemAt(1).widget()

        outer.addLayout(btn_row)

        return panel

    def _create_left_panel_old(self): #vers 1
        """Left panel — list + Add/Remove + info label.
        Override to replace with your own content.
        """
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ll = QVBoxLayout(panel)
        ll.setContentsMargins(*self.get_panel_margins())

        hdr = QLabel("Models")
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        hdr.setFont(self.panel_font)
        hdr.setStyleSheet("font-weight:bold; padding:2px;")
        ll.addWidget(hdr)

        self._item_list = QListWidget()
        self._item_list.setAlternatingRowColors(True)
        self._item_list.currentRowChanged.connect(
            self._on_list_selection_changed)
        ll.addWidget(self._item_list)

        br = QHBoxLayout()
        self._add_item_btn = QPushButton("+ Add")
        self._del_item_btn = QPushButton("− Remove")
        self._add_item_btn.clicked.connect(self._on_add_item)
        self._del_item_btn.clicked.connect(self._on_remove_item)
        br.addWidget(self._add_item_btn)
        br.addWidget(self._del_item_btn)
        ll.addLayout(br)

        sep = QFrame(); sep.setFrameShape(QFrame.Shape.HLine)
        ll.addWidget(sep)

        self._info_lbl = QLabel("No file loaded")
        self._info_lbl.setFont(self.infobar_font)
        ll.addWidget(self._info_lbl)

        return panel

    def _create_centre_panel(self): #Vers 1
        """Centre panel — tab view with placeholder tabs.
        Override to replace with your own canvas/tabs.
        """
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel)
        cl.setContentsMargins(0, 0, 0, 0)
        cl.setSpacing(0)

        self._view_tabs = QTabWidget()
        self._view_tabs.setDocumentMode(True)
        self._view_tabs.currentChanged.connect(self._on_tab_changed)

        for label in ("View A", "View B"):
            tab = QWidget()
            tl  = QVBoxLayout(tab)
            tl.addWidget(QLabel(
                f"[ {label} — override _create_centre_panel() ]",
                alignment=Qt.AlignmentFlag.AlignCenter))
            self._view_tabs.addTab(tab, label)

        cl.addWidget(self._view_tabs)
        return panel

    def _create_right_panel_old(self): #Vers 1
        """Right panel — sidebar with 2-col tool button grid.
        Override _populate_sidebar() to change tool buttons.
        """
        sidebar = QFrame()
        sidebar.setFrameStyle(QFrame.Shape.StyledPanel)
        sidebar.setFixedWidth(self.WS.get("sidebar_width", 82))
        sl = QVBoxLayout(sidebar)
        sl.setContentsMargins(2, 4, 2, 4)
        sl.setSpacing(2)
        self._sidebar_layout = sl
        self._sidebar_frame  = sidebar
        self._draw_btns      = {}

        self._populate_sidebar()

        sl.addStretch(0)
        return sidebar


        # - right panel — model info
    def _create_right_panel(self): #vers 4
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(155); panel.setMaximumWidth(200)
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(*self.get_panel_margins())
        lay.setSpacing(2)

        ic = self._get_icon_color()
        def _icon(fname, size=14):
            if not ICONS_AVAILABLE: return None
            try:
                fn = getattr(SVGIconFactory, f'get_{fname}_icon', None)
                return fn(size, ic) if fn else None
            except Exception: return None

        from PyQt6.QtWidgets import QToolButton as _QTB, QSlider

        def _tbtn(text, tip, cb, iname=None, checkable=False, checked=False):
            b = _QTB(); b.setFont(self.infobar_font)
            b.setToolTip(tip); b.setFixedHeight(24)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(13,13))
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
                b.setToolTip(f'{text} — {tip}')
            else:
                b.setText(text)
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextOnly)
            b.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
            if checkable:
                b.setCheckable(True); b.setChecked(checked)
                b.toggled.connect(cb)
            else:
                b.clicked.connect(lambda _=False: cb())
            return b

        def _tbtn_txt(text, tip, cb, iname=None, checkable=False, checked=False):
            """Button with icon+short text side by side."""
            b = _QTB(); b.setFont(self.infobar_font)
            b.setToolTip(tip); b.setFixedHeight(24)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(13,13))
                b.setText(text)
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            else:
                b.setText(text)
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextOnly)
            b.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
            if checkable:
                b.setCheckable(True); b.setChecked(checked)
                b.toggled.connect(cb)
            else:
                b.clicked.connect(lambda _=False: cb())
            return b

        def _row(*widgets, spacing=2):
            w = QWidget(); h = QHBoxLayout(w)
            h.setContentsMargins(0,0,0,0); h.setSpacing(spacing)
            for ww in widgets:
                if isinstance(ww, int): h.addSpacing(ww)
                elif ww == 'stretch':  h.addStretch()
                else: h.addWidget(ww)  # no stretch factor — use natural size
            return w

        def _sep():
            from PyQt6.QtWidgets import QFrame as _QF
            s = _QF(); s.setFrameShape(_QF.Shape.HLine)
            s.setFixedHeight(1); s.setStyleSheet("background:#333;")
            return s

        def _lbl(text):
            l = QLabel(text); l.setFont(self.panel_font); return l

        # ── Render ──────────────────────────────────────
        lay.addWidget(_lbl("Render"))
        self._mode_group = QButtonGroup(self); self._mode_group.setExclusive(True)
        mode_row = QWidget(); mode_h = QHBoxLayout(mode_row)
        mode_h.setContentsMargins(0,0,0,0); mode_h.setSpacing(2)
        for label, mode, iname in [("Wire","wireframe","dff_edit"),
                                    ("Solid","solid","properties"),
                                    ("Tex","textured","image")]:
            b = _QTB(); b.setFont(self.infobar_font); b.setFixedHeight(24)
            b.setToolTip(f"{mode.capitalize()} render mode")
            ico = _icon(iname)
            if ico: b.setIcon(ico); b.setIconSize(QSize(13,13))
            b.setText(label)
            b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            b.setCheckable(True); b.setChecked(mode=='solid')
            b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            b.toggled.connect(lambda checked, m=mode: self._set_mode(m) if checked else None)
            self._mode_group.addButton(b); mode_h.addWidget(b)
        lay.addWidget(mode_row)

        # Backface + Grid + PreLit — icon-only row
        self._cull_btn   = _tbtn("Cull",   "Backface culling", self.viewport.set_backface_cull, 'col_file', True, False)
        self._grid_btn   = _tbtn("Grid",   "Toggle grid",      self.viewport.set_show_grid,     'fit_grid', True, True)
        self._prelit_btn = _tbtn("PreLit", "Pre-lighting",     self.viewport.set_prelight,      'brighten', True, False)
        lay.addWidget(_row(self._cull_btn, self._grid_btn, self._prelit_btn))
        lay.addWidget(_sep())

        # ── Camera + Light ───────────────────────────────
        _reset_btn = _tbtn_txt("Reset", "Reset camera", self.viewport.reset_camera, 'refresh')
        _light_btn = _tbtn_txt("Light", "Light setup",  self._light_setup_dialog,   'weather')
        lay.addWidget(_row(_reset_btn, _light_btn))
        lay.addWidget(_sep())

        # ── Paint ────────────────────────────────────────
        lay.addWidget(_lbl("Paint"))
        self._paint1_btn = QPushButton("Pri")
        self._paint2_btn = QPushButton("Sec")
        self._carcol_btn = QPushButton("Carcol")
        for b in (self._paint1_btn, self._paint2_btn, self._carcol_btn):
            b.setFixedHeight(24); b.setFont(self.infobar_font)
        self._paint1_btn.clicked.connect(self._pick_paint1)
        self._paint2_btn.clicked.connect(self._pick_paint2)
        self._carcol_btn.clicked.connect(self._cycle_carcol)
        self._carcol_btn.setToolTip("Cycle through carcols.dat colour pairs")
        lay.addWidget(_row(self._paint1_btn, self._paint2_btn, self._carcol_btn))
        self._update_paint_btns()

        self._carcols_widget = QWidget()
        self._carcols_lay = QVBoxLayout(self._carcols_widget)
        self._carcols_lay.setContentsMargins(0,0,0,2); self._carcols_lay.setSpacing(2)
        lay.addWidget(self._carcols_widget)
        lay.addWidget(_sep())

        # ── Assembly ─────────────────────────────────────
        lay.addWidget(_lbl("Assembly"))
        self._assemble_btn = _tbtn_txt("All",    "Show all parts", self._toggle_assembly_mode, 'select_all', True, False)
        self._damage_btn   = _tbtn_txt("Damage", "Damaged state",  self._toggle_damage_mode,   'warning',    True, False)
        self._lod_btn      = _tbtn_txt("LOD",    "Show LOD",       self._toggle_lod_mode,      'search',     True, False)
        lay.addWidget(_row(self._assemble_btn, self._damage_btn, self._lod_btn))
        lay.addWidget(_sep())

        # ── Animate ──────────────────────────────────────
        lay.addWidget(_lbl("Animate"))
        self._anim_btn = _tbtn_txt('Play', 'Play door animation',
            self._toggle_animation, 'go', True, False)
        self._anim_btn.setFixedWidth(60)
        spd_slider = QSlider(Qt.Orientation.Horizontal)
        spd_slider.setRange(1, 30); spd_slider.setValue(10)
        spd_slider.setToolTip('Animation speed')
        spd_slider.valueChanged.connect(self._set_anim_speed)
        self._anim_speed_slider = spd_slider
        anim_row = QWidget(); anim_h = QHBoxLayout(anim_row)
        anim_h.setContentsMargins(0,0,0,0); anim_h.setSpacing(3)
        anim_h.addWidget(self._anim_btn)
        anim_h.addWidget(spd_slider, 1)
        lay.addWidget(anim_row)

        # Door toggles — icon-only, 6 buttons
        lay.addWidget(_lbl("Doors"))
        door_row = QWidget(); door_h = QHBoxLayout(door_row)
        door_h.setContentsMargins(0,0,0,0); door_h.setSpacing(2)
        self._door_btns = {}
        for dlabel, dname in [('LF','door_lf'),('RF','door_rf'),('LR','door_lr'),
                               ('RR','door_rr'),('Hd','bonnet'),('Bt','boot')]:
            db = _QTB(); db.setFixedHeight(22); db.setFixedWidth(28)
            db.setFont(self.infobar_font); db.setText(dlabel)
            db.setToolTip(f'Open/close {dname}')
            db.setCheckable(True)
            db.toggled.connect(lambda checked, n=dname: self._toggle_door(n, checked))
            door_h.addWidget(db); self._door_btns[dname] = db
        lay.addWidget(door_row)
        lay.addWidget(_sep())

        # ── Wheels ───────────────────────────────────────
        self._wheels_btn = _tbtn_txt("Wheels", "Show wheels",
                                     self._toggle_show_wheels, 'col_workshop', True, False)
        self._wheels_btn.setFixedWidth(72)
        steer_slider = QSlider(Qt.Orientation.Horizontal)
        steer_slider.setRange(-35, 35); steer_slider.setValue(0)
        steer_slider.setToolTip('Front wheel steer angle')
        steer_slider.valueChanged.connect(self._set_wheel_steer)
        self._wheel_heading_slider = steer_slider
        wheel_row = QWidget(); wheel_h = QHBoxLayout(wheel_row)
        wheel_h.setContentsMargins(0,0,0,0); wheel_h.setSpacing(3)
        wheel_h.addWidget(self._wheels_btn); wheel_h.addWidget(steer_slider, 1)
        lay.addWidget(wheel_row)

        # Wheel scale adjustment (+/- 10%)
        scale_row = QWidget(); scale_h = QHBoxLayout(scale_row)
        scale_h.setContentsMargins(0,0,0,0); scale_h.setSpacing(3)
        scale_lbl = QLabel('Size'); scale_lbl.setFont(self.infobar_font)
        scale_lbl.setFixedWidth(28)
        wheel_scale_slider = QSlider(Qt.Orientation.Horizontal)
        wheel_scale_slider.setRange(-10, 10); wheel_scale_slider.setValue(0)
        wheel_scale_slider.setToolTip('Wheel size offset ±10% (does not save)')
        self._wheel_scale_slider = wheel_scale_slider
        self._wheel_scale_lbl = QLabel('0%'); self._wheel_scale_lbl.setFont(self.infobar_font)
        self._wheel_scale_lbl.setFixedWidth(28)
        def _on_scale_change(v):
            self._wheel_scale_lbl.setText(f'{v:+d}%')
            mult = 1.0 + v / 100.0
            self.viewport._wheel_scale_mult = mult
            if getattr(self.viewport, '_show_wheels', False):
                self._toggle_show_wheels(True)
        wheel_scale_slider.valueChanged.connect(_on_scale_change)
        scale_h.addWidget(scale_lbl); scale_h.addWidget(wheel_scale_slider, 1)
        scale_h.addWidget(self._wheel_scale_lbl)
        lay.addWidget(scale_row)
        lay.addWidget(_sep())

        # ── Model Info (collapsible) ──────────────────────
        info_toggle = _QTB(); info_toggle.setText("▸ Info")
        info_toggle.setFont(self.infobar_font); info_toggle.setFixedHeight(22)
        info_toggle.setCheckable(True)
        info_toggle.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextOnly)
        info_toggle.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        lay.addWidget(info_toggle)

        self._info_lbl = QLabel('—')
        self._info_lbl.setFont(self.infobar_font)
        self._info_lbl.setWordWrap(True)
        self._info_lbl.setVisible(False)
        lay.addWidget(self._info_lbl)
        info_toggle.toggled.connect(lambda c: (
            self._info_lbl.setVisible(c),
            info_toggle.setText(("▾ Info" if c else "▸ Info"))))
        lay.addWidget(_sep())

        # ── Editing ──────────────────────────────────────
        lay.addWidget(_lbl("Editing"))
        # Row 1: CoM, Dummies, Suspension
        # Row 2: Seats, Bounds, Export
        edit_defs = [
            ("CoM",  "Centre of Mass — reads from handling.cfg", self._toggle_com_indicator,  'locate',    True),
            ("Dum",  "Dummy frame positions",                     self._toggle_dummy_overlay,  'tree',      True),
            ("Susp", "Suspension travel range",                   self._toggle_suspension_vis, 'split',     True),
            ("Seat", "Seat positions from ped_frontseat frames",  self._toggle_seat_overlay,   'properties',True),
            ("Box",  "Bounding box",                              self._toggle_bounds,         'col_file',  True),
            ("Exp",  "Export DFF to OBJ/DFF",                     self._export_dff,            'export',    False),
        ]
        self._edit_btns = []
        er1 = QWidget(); eh1 = QHBoxLayout(er1); eh1.setContentsMargins(0,0,0,0); eh1.setSpacing(2)
        er2 = QWidget(); eh2 = QHBoxLayout(er2); eh2.setContentsMargins(0,0,0,0); eh2.setSpacing(2)
        for i, (label, tip, cb, iname, checkable) in enumerate(edit_defs):
            b = _QTB(); b.setFixedHeight(24)
            b.setFont(self.infobar_font); b.setToolTip(tip)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(13,13))
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
            else:
                b.setText(label)
            b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            if checkable:
                b.setCheckable(True)
                b.toggled.connect(lambda checked, fn=cb, lbl=label: fn() if True else None)
            else:
                b.clicked.connect(lambda _=False, fn=cb: fn())
            self._edit_btns.append(b)
            (eh1 if i < 3 else eh2).addWidget(b)
        lay.addWidget(er1); lay.addWidget(er2)

        lay.addStretch()
        return panel
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(160); panel.setMaximumWidth(210)
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(*self.get_panel_margins())
        lay.setSpacing(3)

        ic = self._get_icon_color()
        def _icon(name, size=16):
            if not ICONS_AVAILABLE: return None
            try:
                fn = getattr(SVGIconFactory, name+'_icon', None)
                return fn(size, ic) if fn else None
            except Exception: return None

        from PyQt6.QtWidgets import QToolButton as _QTB, QSlider

        def _tbtn(text, tip, cb, iname=None, checkable=False, checked=False, w=None):
            b = _QTB(); b.setFont(self.infobar_font)
            b.setToolTip(tip); b.setFixedHeight(26)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(14,14))
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            b.setText(text)
            if w: b.setFixedWidth(w)
            else: b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            if checkable:
                b.setCheckable(True); b.setChecked(checked)
                b.toggled.connect(cb)
            else:
                b.clicked.connect(lambda _=False: cb())
            return b

        def _row(*widgets, spacing=3):
            w = QWidget(); h = QHBoxLayout(w)
            h.setContentsMargins(0,0,0,0); h.setSpacing(spacing)
            for ww in widgets:
                if isinstance(ww, int): h.addSpacing(ww)
                elif ww == 'stretch':  h.addStretch()
                else: h.addWidget(ww, 1)
            return w

        def _sep():
            from PyQt6.QtWidgets import QFrame as _QF
            s = _QF(); s.setFrameShape(_QF.Shape.HLine)
            s.setFixedHeight(1)
            s.setStyleSheet("background:#333;")
            return s

        def _lbl(text):
            l = QLabel(text); l.setFont(self.panel_font)
            return l

        # ── Render ──────────────────────────────────────
        lay.addWidget(_lbl("Render"))
        self._mode_group = QButtonGroup(self); self._mode_group.setExclusive(True)
        mode_row = QWidget(); mode_h = QHBoxLayout(mode_row)
        mode_h.setContentsMargins(0,0,0,0); mode_h.setSpacing(2)
        for label, mode, iname in [("Wire","wireframe","wireframe"),
                                    ("Solid","solid","solid"),
                                    ("Tex","textured","texture")]:
            b = _QTB(); b.setFont(self.infobar_font); b.setFixedHeight(26)
            b.setToolTip(f"{mode.capitalize()} render mode")
            ico = _icon(iname)
            if ico: b.setIcon(ico); b.setIconSize(QSize(14,14))
            b.setText(label)
            b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            b.setCheckable(True); b.setChecked(mode=='solid')
            b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            b.toggled.connect(lambda checked, m=mode: self._set_mode(m) if checked else None)
            self._mode_group.addButton(b); mode_h.addWidget(b)
        lay.addWidget(mode_row)

        # ── View toggles (Backface + Grid same row) ──────
        self._cull_btn   = _tbtn("Backface", "Toggle backface culling",  self.viewport.set_backface_cull, 'backface', True, False)
        self._grid_btn   = _tbtn("Grid",     "Toggle grid",              self.viewport.set_show_grid,     'grid',     True, True)
        self._prelit_btn = _tbtn("PreLit",   "Vertex pre-lighting",      self.viewport.set_prelight,      'shading',  True, False)
        lay.addWidget(_row(self._cull_btn, self._grid_btn))
        lay.addWidget(self._prelit_btn)

        lay.addWidget(_sep())

        # ── Camera + Light same row ──────────────────────
        _reset_btn = _tbtn("Reset", "Reset camera", self.viewport.reset_camera, 'reset')
        _light_btn = _tbtn("Light", "Light setup",  self._light_setup_dialog,   'light')
        lay.addWidget(_row(_reset_btn, _light_btn))

        lay.addWidget(_sep())

        # ── Paint ────────────────────────────────────────
        lay.addWidget(_lbl("Paint"))
        self._paint1_btn = QPushButton("Pri")
        self._paint2_btn = QPushButton("Sec")
        for b in (self._paint1_btn, self._paint2_btn):
            b.setFixedHeight(26); b.setFont(self.infobar_font)
        self._paint1_btn.clicked.connect(self._pick_paint1)
        self._paint2_btn.clicked.connect(self._pick_paint2)
        lay.addWidget(_row(self._paint1_btn, self._paint2_btn))
        self._update_paint_btns()

        # Carcols swatches
        self._carcols_widget = QWidget()
        self._carcols_lay = QVBoxLayout(self._carcols_widget)
        self._carcols_lay.setContentsMargins(0,0,0,2); self._carcols_lay.setSpacing(2)
        lay.addWidget(self._carcols_widget)

        lay.addWidget(_sep())

        # ── Assembly (3 buttons same row with icons) ─────
        lay.addWidget(_lbl("Assembly"))
        self._assemble_btn = _tbtn("All",    "Show all parts assembled", self._toggle_assembly_mode, 'mesh',     True, False)
        self._damage_btn   = _tbtn("Damage", "Show damaged state",       self._toggle_damage_mode,   'warning',  True, False)
        self._lod_btn      = _tbtn("LOD",    "Show LOD meshes",          self._toggle_lod_mode,      'zoom_out', True, False)
        lay.addWidget(_row(self._assemble_btn, self._damage_btn, self._lod_btn))

        lay.addWidget(_sep())

        # ── Animate: [Play] Speed [---x---] same row ─────
        lay.addWidget(_lbl("Animate"))
        self._anim_btn = _tbtn('Play', 'Start/stop animation',
            lambda: self.viewport.set_animation(self._anim_btn.isChecked()),
            'play', True, False)
        self._anim_btn.setFixedWidth(54)
        spd_slider = QSlider(Qt.Orientation.Horizontal)
        spd_slider.setRange(1, 30); spd_slider.setValue(10)
        spd_slider.setToolTip('Animation speed')
        spd_slider.valueChanged.connect(lambda v: self.viewport.set_animation_speed(v/10.0))
        self._anim_speed_slider = spd_slider
        anim_row = QWidget(); anim_h = QHBoxLayout(anim_row)
        anim_h.setContentsMargins(0,0,0,0); anim_h.setSpacing(4)
        anim_h.addWidget(self._anim_btn)
        spd_lbl = QLabel('Spd'); spd_lbl.setFont(self.infobar_font)
        anim_h.addWidget(spd_lbl); anim_h.addWidget(spd_slider, 1)
        lay.addWidget(anim_row)

        # Door toggles row
        door_row = QWidget(); door_h = QHBoxLayout(door_row)
        door_h.setContentsMargins(0,0,0,0); door_h.setSpacing(2)
        for dlabel, dname in [('LF','door_lf'),('RF','door_rf'),
                               ('LR','door_lr'),('RR','door_rr'),
                               ('Hood','bonnet'),('Boot','boot')]:
            db = QPushButton(dlabel); db.setFixedHeight(22)
            db.setFont(self.infobar_font); db.setToolTip(f'Toggle {dname}')
            db.clicked.connect(lambda _=False, n=dname: self.viewport.toggle_door(n))
            door_h.addWidget(db)
        lay.addWidget(door_row)

        lay.addWidget(_sep())

        # ── Wheels: [Show Wheels] Steer [--x--] same row ─
        self._wheels_btn = _tbtn("Wheels", "Show wheels at dummy positions",
                                 self._toggle_show_wheels, 'mesh', True, False)
        self._wheels_btn.setFixedWidth(70)
        steer_slider = QSlider(Qt.Orientation.Horizontal)
        steer_slider.setRange(-35, 35); steer_slider.setValue(0)
        steer_slider.setToolTip('Front wheel steering angle')
        steer_slider.valueChanged.connect(lambda v: self.viewport.set_wheel_heading(v))
        self._wheel_heading_slider = steer_slider
        wheel_row = QWidget(); wheel_h = QHBoxLayout(wheel_row)
        wheel_h.setContentsMargins(0,0,0,0); wheel_h.setSpacing(4)
        wheel_h.addWidget(self._wheels_btn)
        steer_lbl = QLabel('Steer'); steer_lbl.setFont(self.infobar_font)
        wheel_h.addWidget(steer_lbl); wheel_h.addWidget(steer_slider, 1)
        lay.addWidget(wheel_row)

        lay.addWidget(_sep())

        # ── Model Info (collapsible) ──────────────────────
        info_header = QWidget(); info_h = QHBoxLayout(info_header)
        info_h.setContentsMargins(0,0,0,0); info_h.setSpacing(4)
        info_toggle = _QTB(); info_toggle.setText("▸ Model Info")
        info_toggle.setFont(self.infobar_font); info_toggle.setFixedHeight(22)
        info_toggle.setCheckable(True); info_toggle.setChecked(False)
        info_toggle.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextOnly)
        info_toggle.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        info_h.addWidget(info_toggle)
        lay.addWidget(info_header)

        self._info_lbl = QLabel('—')
        self._info_lbl.setFont(self.infobar_font)
        self._info_lbl.setWordWrap(True)
        self._info_lbl.setVisible(False)
        self._info_lbl.setAlignment(Qt.AlignmentFlag.AlignTop)
        lay.addWidget(self._info_lbl)

        def _toggle_info(checked):
            self._info_lbl.setVisible(checked)
            info_toggle.setText(("▾" if checked else "▸") + " Model Info")
        info_toggle.toggled.connect(_toggle_info)

        lay.addWidget(_sep())

        # ── Edit Buttons (icon + text, collapse to icon-only when narrow) ──
        lay.addWidget(_lbl("Editing"))
        edit_actions = [
            ("CoM",     "Centre of Mass indicator",       self._toggle_com_indicator,  'target',   True),
            ("Dummies", "Show dummy frame positions",     self._toggle_dummy_overlay,  'mesh',     True),
            ("Susp",    "Suspension travel visualiser",  self._toggle_suspension_vis, 'bounds',   True),
            ("Seats",   "Show seat positions",            self._toggle_seat_overlay,   'view',     True),
            ("Bounds",  "Show bounding box",              self._toggle_bounds,         'wireframe',True),
            ("Export",  "Export DFF geometry",            self._export_dff,            'export',   False),
        ]
        edit_row1 = QWidget(); er1 = QHBoxLayout(edit_row1)
        edit_row1.setContentsMargins(0,0,0,0) if hasattr(edit_row1,'setContentsMargins') else None
        er1.setContentsMargins(0,0,0,0); er1.setSpacing(2)
        edit_row2 = QWidget(); er2 = QHBoxLayout(edit_row2)
        er2.setContentsMargins(0,0,0,0); er2.setSpacing(2)
        self._edit_btns = []
        for i, (label, tip, cb, iname, checkable) in enumerate(edit_actions):
            b = _QTB(); b.setFixedHeight(26)
            b.setFont(self.infobar_font); b.setToolTip(tip)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(14,14))
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            b.setText(label)
            b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            if checkable:
                b.setCheckable(True)
                b.toggled.connect(lambda checked, fn=cb: fn())
            else:
                b.clicked.connect(lambda _=False, fn=cb: fn())
            self._edit_btns.append(b)
            (er1 if i < 3 else er2).addWidget(b)
        lay.addWidget(edit_row1)
        lay.addWidget(edit_row2)

        # Collapse edit buttons to icon-only when panel is narrow
        def _on_panel_resize(event):
            narrow = panel.width() < 170
            style = Qt.ToolButtonStyle.ToolButtonIconOnly if narrow else Qt.ToolButtonStyle.ToolButtonTextBesideIcon
            for b in self._edit_btns:
                if b.icon() and not b.icon().isNull():
                    b.setToolButtonStyle(style)
            QFrame.resizeEvent(panel, event)
        panel.resizeEvent = _on_panel_resize

        lay.addStretch()
        return panel
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(160); panel.setMaximumWidth(200)
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(*self.get_panel_margins())
        lay.setSpacing(4)

        ic = self._get_icon_color()
        def _icon(name, size=16):
            if not ICONS_AVAILABLE: return None
            try:
                fn = getattr(SVGIconFactory, name+'_icon', None)
                return fn(size, ic) if fn else None
            except Exception: return None

        from PyQt6.QtWidgets import QToolButton as _QTB

        def _btn(text, tip, cb, iname=None, checkable=False, checked=False): #Vers 1
            b = _QTB(); b.setFont(self.panel_font)
            b.setToolTip(tip); b.setFixedHeight(28)
            b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(16,16))
                b.setText(text)
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            else:
                b.setText(text)
            if checkable:
                b.setCheckable(True); b.setChecked(checked)
                b.toggled.connect(cb)
            else:
                b.clicked.connect(cb)
            return b

        #  Render mode
        lbl_r = QLabel("Render"); lbl_r.setFont(self.panel_font)
        lay.addWidget(lbl_r)
        self._mode_group = QButtonGroup(self); self._mode_group.setExclusive(True)
        for label, mode, iname in [
            ("Wireframe", "wireframe", "wireframe"),
            ("Solid",     "solid",     "solid"),
            ("Textured",  "textured",  "texture"),
        ]:
            b = _btn(label, f"{mode.capitalize()} render mode",
                     lambda checked=False, m=mode: (self._set_mode(m) if checked else None),
                     iname, checkable=True, checked=(mode=='solid'))
            self._mode_group.addButton(b); lay.addWidget(b)

        lay.addSpacing(6)

        #  View toggles
        lbl_v = QLabel("View"); lbl_v.setFont(self.panel_font)
        lay.addWidget(lbl_v)
        self._cull_btn   = _btn("Backface Cull", "Toggle backface culling",  self.viewport.set_backface_cull, 'backface', True, True)
        self._grid_btn   = _btn("Grid",          "Toggle grid",              self.viewport.set_show_grid,    'grid',     True, True)
        self._prelit_btn = _btn("Pre-Lighting",  "Vertex pre-lighting",      self.viewport.set_prelight,     'shading',  True, False)
        lay.addWidget(self._cull_btn)
        lay.addWidget(self._grid_btn)
        lay.addWidget(self._prelit_btn)

        lay.addSpacing(6)

        #  Camera
        lbl_c = QLabel("Camera"); lbl_c.setFont(self.panel_font)
        lay.addWidget(lbl_c)
        lay.addWidget(_btn("Reset Camera", "Reset camera to default", self.viewport.reset_camera, 'reset'))
        lay.addWidget(_btn("Light Setup",  "Adjust light direction",  self._light_setup_dialog,   'light'))

        lay.addSpacing(6)

        #  Paint Colours
        lbl_p = QLabel("Paint"); lbl_p.setFont(self.panel_font)
        lay.addWidget(lbl_p)

        # Colour swatches row — primary + secondary
        swatch_row = QWidget()
        swatch_lay = QHBoxLayout(swatch_row)
        swatch_lay.setContentsMargins(0,0,0,0); swatch_lay.setSpacing(4)

        self._paint1_btn = QPushButton("Pri")
        self._paint2_btn = QPushButton("Sec")
        for btn in (self._paint1_btn, self._paint2_btn):
            btn.setFixedHeight(28)
            btn.setFont(self.infobar_font)
        self._paint1_btn.clicked.connect(self._pick_paint1)
        self._paint2_btn.clicked.connect(self._pick_paint2)
        swatch_lay.addWidget(self._paint1_btn)
        swatch_lay.addWidget(self._paint2_btn)
        lay.addWidget(swatch_row)
        self._update_paint_btns()

        # Carcols swatches — populated when DFF loads
        self._carcols_widget = QWidget()
        self._carcols_lay = QVBoxLayout(self._carcols_widget)
        self._carcols_lay.setContentsMargins(0,0,0,2); self._carcols_lay.setSpacing(2)
        lay.addWidget(self._carcols_widget)

        lay.addSpacing(4)

        #  Assembly
        lbl_a = QLabel("Assembly"); lbl_a.setFont(self.panel_font)
        lay.addWidget(lbl_a)
        self._assemble_btn = _btn("All Parts", "Show all parts assembled at world positions", self._toggle_assembly_mode, None, True, False)
        self._damage_btn   = _btn("Damage",    "Show damaged state (_dam parts)",             self._toggle_damage_mode,   None, True, False)
        self._lod_btn      = _btn("Show LOD",  "Show LOD meshes (_vlo parts)",                self._toggle_lod_mode,      None, True, False)
        lay.addWidget(self._assemble_btn)
        lay.addWidget(self._damage_btn)
        lay.addWidget(self._lod_btn)

        lay.addSpacing(6)

        #  Animate
        lbl_an = QLabel('Animate'); lbl_an.setFont(self.panel_font)
        lay.addWidget(lbl_an)
        self._anim_btn = _btn('Play', 'Start/stop animation',
            lambda checked: self.viewport.set_animation(checked), None, True, False)
        lay.addWidget(self._anim_btn)

        # Speed
        from PyQt6.QtWidgets import QSlider
        spd_row = QHBoxLayout()
        spd_lbl = QLabel('Speed'); spd_lbl.setFont(self.infobar_font)
        self._anim_speed_slider = QSlider(Qt.Orientation.Horizontal)
        self._anim_speed_slider.setRange(1, 30)
        self._anim_speed_slider.setValue(10)
        self._anim_speed_slider.setToolTip('Animation speed')
        self._anim_speed_slider.valueChanged.connect(
            lambda v: self.viewport.set_animation_speed(v / 10.0))
        spd_row.addWidget(spd_lbl); spd_row.addWidget(self._anim_speed_slider, 1)
        lay.addLayout(spd_row)

        # Door toggles
        door_row = QHBoxLayout(); door_row.setSpacing(2)
        for dlabel, dname in [('LF','door_lf'),('RF','door_rf'),
                               ('LR','door_lr'),('RR','door_rr')]:
            db = QPushButton(dlabel); db.setFixedHeight(22); db.setFixedWidth(28)
            db.setFont(self.infobar_font)
            db.setToolTip(f'Toggle {dname}')
            db.clicked.connect(lambda _=False, n=dname: self.viewport.toggle_door(n))
            door_row.addWidget(db)
        door_row.addStretch()
        # Bonnet / Boot
        for dlabel, dname in [('Hood','bonnet'),('Boot','boot')]:
            db = QPushButton(dlabel); db.setFixedHeight(22)
            db.setFont(self.infobar_font); db.setToolTip(f'Toggle {dname}')
            db.clicked.connect(lambda _=False, n=dname: self.viewport.toggle_door(n))
            door_row.addWidget(db)
        lay.addLayout(door_row)

        lay.addSpacing(4)

        #  Wheels
        lbl_w = QLabel('Wheels'); lbl_w.setFont(self.panel_font)
        lay.addWidget(lbl_w)
        self._wheels_btn = _btn("Show Wheels", "Load and show wheels at dummy positions",
                                self._toggle_show_wheels, None, True, False)
        lay.addWidget(self._wheels_btn)

        from PyQt6.QtWidgets import QSlider
        self._wheel_heading_slider = QSlider(Qt.Orientation.Horizontal)
        self._wheel_heading_slider.setRange(-35, 35)
        self._wheel_heading_slider.setValue(0)
        self._wheel_heading_slider.setToolTip('Front wheel heading angle')
        self._wheel_heading_slider.valueChanged.connect(
            lambda v: self.viewport.set_wheel_heading(v))
        heading_row = QHBoxLayout()
        heading_lbl = QLabel('Steer'); heading_lbl.setFont(self.infobar_font)
        heading_row.addWidget(heading_lbl)
        heading_row.addWidget(self._wheel_heading_slider, 1)
        lay.addLayout(heading_row)

        lay.addSpacing(6)

        #  Model Info
        lbl_i = QLabel('Model Info'); lbl_i.setFont(self.panel_font)
        lay.addWidget(lbl_i)
        self._info_lbl = QLabel('—')
        self._info_lbl.setFont(self.infobar_font)
        self._info_lbl.setWordWrap(True)
        self._info_lbl.setAlignment(Qt.AlignmentFlag.AlignTop)
        lay.addWidget(self._info_lbl)

        lay.addStretch()
        return panel



    def _populate_sidebar(self): #Vers 1
        """Build the 2-col icon grid in the right sidebar.
        Override to change or extend the tool set.
        """
        sl = self._sidebar_layout
        ic = self._get_icon_color()
        BTN = 36

        def _nb(icon_fn, tip, slot, checkable=False): #vers 1
            b = QToolButton(); b.setFixedSize(BTN, BTN)
            try: b.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
            except Exception: b.setText(tip[:2])
            b.setToolTip(tip); b.setCheckable(checkable)
            b.clicked.connect(slot); return b

        def _row(*btns): #vers 1
            row = QHBoxLayout()
            row.setSpacing(2); row.setContentsMargins(0, 0, 0, 0)
            for b in btns: row.addWidget(b)
            if len(btns) == 1: row.addStretch()
            sl.addLayout(row)

        def _sep(): #vers 1
            s = QFrame(); s.setFrameShape(QFrame.Shape.HLine)
            sl.addSpacing(2); sl.addWidget(s); sl.addSpacing(2)

        def _tool(icon_fn, tip, name): #vers 1
            b = _nb(icon_fn, tip,
                    lambda checked=False, t=name: self._set_active_tool(t),
                    checkable=True)
            self._draw_btns[name] = b; return b

        # Row 1-2: View controls
        _row(_nb("zoom_in_icon",  "Zoom in  (+)",      lambda: self._zoom(1.25)),
             _nb("zoom_out_icon", "Zoom out  (-)",     lambda: self._zoom(0.8)))
        _row(_nb("fit_grid_icon", "Fit  Ctrl+0",       self._fit),
             _nb("locate_icon",   "Jump to selection", self._jump))
        _sep()

        # Rows 3-6: Draw tools (2 per row)
        _row(_tool("paint_icon",     "Pencil (P)",           "pencil"),
             _tool("fill_icon",      "Flood fill (F)",       "fill"))
        _row(_tool("line_icon",      "Line (L)",             "line"),
             _tool("rect_icon",      "Rect outline (R)",     "rect"))
        _row(_tool("rect_fill_icon", "Filled rect (Shift+R)","rect_fill"),
             _tool("dropper_icon",   "Colour picker (K)",    "picker"))
        _row(_tool("scissors_icon",  "Cut (X)",              "cut"),
             _tool("paste_brush_icon","Paste (V)",           "paste"))
        _row(_tool("zoom_in_icon",   "Zoom tool (Z)",        "zoom"),
             _nb("search_icon",      "Open in editor tab",
                 lambda: self._on_toolbar_action("edit")))
        _sep()

        # Rows 7-8: Transform tools
        _row(_nb("rotate_cw_icon",  "Rotate +90°",
                 lambda: self._on_toolbar_action("rotate_cw")),
             _nb("rotate_ccw_icon", "Rotate -90°",
                 lambda: self._on_toolbar_action("rotate_ccw")))
        _row(_nb("flip_horz_icon",  "Flip Horizontal",
                 lambda: self._on_toolbar_action("flip_h")),
             _nb("flip_vert_icon",  "Flip Vertical",
                 lambda: self._on_toolbar_action("flip_v")))

        if "pencil" in self._draw_btns:
            self._draw_btns["pencil"].setChecked(True)
            self._active_tool = "pencil"


    # - status bar
    def _create_status_bar(self): #vers 2
        bar = QFrame()
        bar.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        bar.setFixedHeight(self.statusheight)
        hl = QHBoxLayout(bar)
        hl.setContentsMargins(*self.get_tab_margins())
        self.status_label = QLabel("No model loaded")
        self.status_label.setFont(self.infobar_font)
        hl.addWidget(self.status_label, 1)
        from PyQt6.QtWidgets import QProgressBar
        self._progress = QProgressBar()
        self._progress.setFixedWidth(120)
        self._progress.setFixedHeight(14)
        self._progress.setRange(0, 0)   # indeterminate / pulsing
        self._progress.setVisible(False)
        self._progress.setTextVisible(False)
        hl.addWidget(self._progress)
        return bar


    # - left panel — geometry + texture lists
    def _make_section_header(self, title, search_cb=None): #vers 1
        """Collapsible section header row with optional search box."""
        row = QWidget(); rl = QHBoxLayout(row); rl.setContentsMargins(0,0,0,0); rl.setSpacing(2)
        lbl = QLabel(title); lbl.setFont(self.panel_font); rl.addWidget(lbl, 1)
        if search_cb:
            from PyQt6.QtWidgets import QLineEdit
            se = QLineEdit(); se.setPlaceholderText("Filter…")
            se.setFixedHeight(20); se.setMaximumWidth(90)
            se.textChanged.connect(search_cb)
            rl.addWidget(se)
        return row

    def _filter_img_list(self, text): #vers 1
        ft = text.lower()
        for i in range(self._img_list.count()):
            item = self._img_list.item(i)
            item.setHidden(bool(ft) and ft not in item.text().lower())

    def _on_img_entry_dclicked(self, item): #vers 2
        """Double-click IMG entry — extract and load DFF + auto TXD with progress."""
        entry = item.data(Qt.ItemDataRole.UserRole)
        img   = getattr(self, '_current_img', None)
        if not entry or not img:
            return
        from PyQt6.QtWidgets import QApplication
        from PyQt6.QtCore import Qt as _Qt
        name = entry.name
        stem0, ext0 = os.path.splitext(name)
        display_name = re.sub(r'_[a-z0-9]{6,12}$', '', stem0, flags=re.IGNORECASE) + ext0
        try:
            import tempfile
            # Show busy cursor + progress bar immediately
            QApplication.setOverrideCursor(_Qt.CursorShape.WaitCursor)
            self._show_progress(True)
            self._set_status(f"Loading {display_name}…")
            QApplication.processEvents()

            # Step 1: Extract DFF
            self._set_status(f"Extracting {name}…")
            QApplication.processEvents()
            data = img.read_entry_data(entry)
            if not data:
                return
            tmp_dir  = tempfile.mkdtemp()
            dff_path = os.path.join(tmp_dir, name)
            with open(dff_path,'wb') as f: f.write(data)

            # Step 2: Parse + load DFF
            self._set_status(f"Parsing {name}…")
            QApplication.processEvents()
            self.load_dff(dff_path)
            QApplication.processEvents()

            # Step 3: Find + extract TXD
            stem = os.path.splitext(name)[0].lower()
            txd_entry = next((e for e in img.entries if e.name.lower() == stem + '.txd'), None)
            if txd_entry:
                self._set_status(f"Loading textures for {stem}…")
                QApplication.processEvents()
                txd_data = img.read_entry_data(txd_entry)
                if txd_data:
                    txd_path = os.path.join(tmp_dir, txd_entry.name)
                    with open(txd_path,'wb') as f: f.write(txd_data)
                    self.load_txd(txd_path)
                    QApplication.processEvents()

        except Exception as ex:
            self._set_status(f"Load error: {ex}")
        finally:
            self._show_progress(False)
            QApplication.restoreOverrideCursor()


    def _light_setup_dialog(self): #vers 2
        """Light direction, intensity and vehicle paint colour dialog."""
        dlg = QDialog(self); dlg.setWindowTitle("Light & Paint Setup"); dlg.resize(360, 380)
        lay = QVBoxLayout(dlg)
        form = QFormLayout()

        vp = self.viewport

        # Light direction sliders
        def _slider(lo, hi, val, scale=10):
            s = QSlider(Qt.Orientation.Horizontal)
            s.setRange(int(lo*scale), int(hi*scale))
            s.setValue(int(val*scale))
            return s

        sx = _slider(-10, 10, vp._light_dir[0])
        sy = _slider(-10, 10, vp._light_dir[1])
        sz = _slider(-10, 10, vp._light_dir[2])
        sa = _slider(0, 1, vp._ambient, 100)
        sd = _slider(0, 2, vp._diffuse, 100)

        lx_lbl = QLabel(f"{vp._light_dir[0]:.1f}")
        ly_lbl = QLabel(f"{vp._light_dir[1]:.1f}")
        lz_lbl = QLabel(f"{vp._light_dir[2]:.1f}")
        la_lbl = QLabel(f"{vp._ambient:.2f}")
        ld_lbl = QLabel(f"{vp._diffuse:.2f}")

        def _upd():
            x = sx.value()/10; y = sy.value()/10; z = sz.value()/10
            a = sa.value()/100; d = sd.value()/100
            lx_lbl.setText(f"{x:.1f}"); ly_lbl.setText(f"{y:.1f}"); lz_lbl.setText(f"{z:.1f}")
            la_lbl.setText(f"{a:.2f}"); ld_lbl.setText(f"{d:.2f}")
            vp.set_light_dir(x, y, z); vp.set_ambient(a); vp.set_diffuse(d)

        for s in (sx,sy,sz,sa,sd): s.valueChanged.connect(_upd)

        def _row(lbl, slider, val_lbl):
            rw = QWidget(); rl = QHBoxLayout(rw); rl.setContentsMargins(0,0,0,0)
            rl.addWidget(slider,1); rl.addWidget(val_lbl)
            form.addRow(lbl, rw)

        _row("Light X:", sx, lx_lbl)
        _row("Light Y:", sy, ly_lbl)
        _row("Light Z:", sz, lz_lbl)
        _row("Ambient:", sa, la_lbl)
        _row("Diffuse:", sd, ld_lbl)

        lay.addLayout(form)

        # Paint colours
        from PyQt6.QtWidgets import QGroupBox, QColorDialog
        paint_grp = QGroupBox("Vehicle Paint Preview")
        paint_lay = QHBoxLayout(paint_grp)

        def _colour_btn(label, current_rgb, setter):
            def _to_qcolor(rgb):
                from PyQt6.QtGui import QColor
                return QColor(int(rgb[0]*255), int(rgb[1]*255), int(rgb[2]*255))
            btn = QPushButton(label)
            btn.setStyleSheet(f"background-color: rgb({int(current_rgb[0]*255)},{int(current_rgb[1]*255)},{int(current_rgb[2]*255)})")
            btn.setFixedHeight(28)
            def _pick():
                col = QColorDialog.getColor(_to_qcolor(setter.__self__._paint1 if 'paint1' in label.lower() or 'primary' in label.lower() else setter.__self__._paint2), dlg)
                if col.isValid():
                    rgb = (col.redF(), col.greenF(), col.blueF())
                    setter(rgb)
                    btn.setStyleSheet(f"background-color: {col.name()}")
                    vp.update()
            btn.clicked.connect(_pick)
            return btn

        vp = self.viewport
        p1_btn = QPushButton("Primary Paint")
        p1_btn.setStyleSheet(f"background-color: rgb({int(vp._paint1[0]*255)},{int(vp._paint1[1]*255)},{int(vp._paint1[2]*255)})")
        p1_btn.setFixedHeight(28)
        def _pick1():
            from PyQt6.QtGui import QColor
            col = QColorDialog.getColor(QColor(int(vp._paint1[0]*255),int(vp._paint1[1]*255),int(vp._paint1[2]*255)), dlg)
            if col.isValid():
                vp._paint1 = (col.redF(), col.greenF(), col.blueF())
                p1_btn.setStyleSheet(f"background-color: {col.name()}")
                vp.update()
        p1_btn.clicked.connect(_pick1)

        p2_btn = QPushButton("Secondary Paint")
        p2_btn.setStyleSheet(f"background-color: rgb({int(vp._paint2[0]*255)},{int(vp._paint2[1]*255)},{int(vp._paint2[2]*255)})")
        p2_btn.setFixedHeight(28)
        def _pick2():
            from PyQt6.QtGui import QColor
            col = QColorDialog.getColor(QColor(int(vp._paint2[0]*255),int(vp._paint2[1]*255),int(vp._paint2[2]*255)), dlg)
            if col.isValid():
                vp._paint2 = (col.redF(), col.greenF(), col.blueF())
                p2_btn.setStyleSheet(f"background-color: {col.name()}")
                vp.update()
        p2_btn.clicked.connect(_pick2)

        paint_lay.addWidget(p1_btn); paint_lay.addWidget(p2_btn)
        lay.addWidget(paint_grp)

        reset_btn = QPushButton("Reset Defaults")
        def _reset():
            sx.setValue(10); sy.setValue(20); sz.setValue(15)
            sa.setValue(30); sd.setValue(85)
            vp._paint1 = (0.80, 0.20, 0.20)
            vp._paint2 = (0.20, 0.20, 0.80)
            p1_btn.setStyleSheet("background-color: rgb(204,51,51)")
            p2_btn.setStyleSheet("background-color: rgb(51,51,204)")
            vp.update()
        reset_btn.clicked.connect(_reset)

        btn_row = QHBoxLayout()
        close_btn = QPushButton("Close"); close_btn.clicked.connect(dlg.accept)
        btn_row.addWidget(reset_btn); btn_row.addStretch(); btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)
        dlg.exec()


    def _populate_frame_tree(self): #vers 1
        tree = getattr(self,'_frame_tree',None)
        if not tree or not self._dff_model: return
        from PyQt6.QtWidgets import QTreeWidgetItem
        from PyQt6.QtCore import Qt
        tree.blockSignals(True)
        tree.clear()
        frames = self._dff_model.frames
        items = {}
        # Build tree from parent indices
        for i,f in enumerate(frames):
            name = f.name or f'frame_{i}'
            item = QTreeWidgetItem([name])
            item.setFlags(item.flags() | Qt.ItemFlag.ItemIsUserCheckable)
            item.setCheckState(0, Qt.CheckState.Checked)
            item.setData(0, Qt.ItemDataRole.UserRole, name.lower())
            items[i] = item
        for i,f in enumerate(frames):
            p = f.parent_index
            if p >= 0 and p != i and p in items:
                items[p].addChild(items[i])
            elif p == -1 or p == i:
                tree.addTopLevelItem(items[i])
        tree.expandAll()
        tree.blockSignals(False)

    def _on_frame_visibility_changed(self, item, col): #vers 2
        from PyQt6.QtCore import Qt
        name = item.data(0, Qt.ItemDataRole.UserRole) or ''
        if not hasattr(self.viewport, '_hidden_frames'):
            self.viewport._hidden_frames = set()
        if item.checkState(0) == Qt.CheckState.Checked:
            self.viewport._hidden_frames.discard(name)
        else:
            self.viewport._hidden_frames.add(name)
        # Always rebuild — hidden_frames filter is applied in load_all_geometries
        m = getattr(self, '_dff_model', None)
        if m and m.frames and m.atomics:
            self.viewport.load_all_geometries(
                m.geometries, [g.materials for g in m.geometries],
                m.frames, m.atomics,
                damaged=getattr(self, '_damage_mode', False))
        self.viewport.update()

    def _populate_geom_list(self): #vers 1

        self._geom_list.clear()
        if not self._dff_model: return
        for i, g in enumerate(self._dff_model.geometries):
            name = f"geometry_{i}"
            a = next((a for a in self._dff_model.atomics if a.geometry_index==i), None)
            if a and a.frame_index < len(self._dff_model.frames):
                fn = self._dff_model.frames[a.frame_index].name
                if fn: name = fn
            self._geom_list.addItem(f"[{i}] {name}  {len(g.vertices)}v {len(g.triangles)}t")


    def _on_geom_selected(self, row: int): #vers 5
        if not self._dff_model or row < 0 or row >= len(self._dff_model.geometries): return
        self._current_geom = row
        m = self._dff_model
        g = m.geometries[row]
        damaged = getattr(self, '_damage_mode', False)
        def _do_load():
            if m.frames and m.atomics:
                self.viewport.load_all_geometries(
                    m.geometries, [geom.materials for geom in m.geometries],
                    m.frames, m.atomics, damaged=damaged)
                if not getattr(self.viewport, '_all_geoms', None) and not self.viewport._vertices:
                    self.viewport.load_geometry(g, g.materials)
            else:
                self.viewport.load_geometry(g, g.materials)
            self.viewport.update()
        from PyQt6.QtCore import QTimer
        # Defer so GL context is initialized before first render
        if hasattr(self.viewport, 'isValid') and not self.viewport.isValid():
            QTimer.singleShot(150, _do_load)
        else:
            _do_load()
        has_prelit = bool(g.colors)
        self._prelit_btn.setEnabled(has_prelit)
        self._info_lbl.setText(
            f"Verts:  {len(g.vertices)}\n"
            f"Tris:   {len(g.triangles)}\n"
            f"Mats:   {len(g.materials)}\n"
            f"UVs:    {len(g.uv_layers)}\n"
            f"Norms:  {'yes' if g.normals else 'no'}\n"
            f"PreLit: {'yes' if has_prelit else 'no'}")

    def _set_status(self, msg: str): #vers 1
        self.status_label.setText(msg)
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"[ModelViewer] {msg}")




class _LogicStubsMixin:
    """Stub methods for subclass override.
    All return immediately or show a 'not implemented' status message.
    Replace these with your actual file format, drawing, and undo logic.
    """

    #    ToolMenuMixin protocol
    def get_menu_title(self) -> str: #vers 1
        return self.App_name

    def _build_menus_into_qmenu(self, pm): #vers 1
        """Override to populate File / Edit / View menus for your app."""
        fm = pm.addMenu("File")
        fm.addAction("Open…  Ctrl+O",  self._open_file)
        fm.addAction("Save…  Ctrl+S",  self._save_file)
        fm.addSeparator()
        fm.addAction("Export…",        self._export_file)
        fm.addAction("Import…",        self._import_file)
        fm.addSeparator()
        recent = self.WS.get_recent()
        if recent:
            rm = fm.addMenu("Recent Files")
            for rp in recent:
                act = rm.addAction(Path(rp).name); act.setToolTip(rp)
                act.triggered.connect(
                    lambda checked=False, p=rp: self._open_file(p))
            rm.addSeparator()
            rm.addAction("Clear Recent", self._clear_recent)
        em = pm.addMenu("Edit")
        em.addAction("Undo  Ctrl+Z",   self._undo)
        em.addAction("Redo  Ctrl+Y",   self._redo)
        vm = pm.addMenu("View")
        vm.addAction("Zoom In  +",     lambda: self._zoom(1.25))
        vm.addAction("Zoom Out  -",    lambda: self._zoom(0.8))
        vm.addAction("Fit  Ctrl+0",    self._fit)
        vm.addSeparator()
        vm.addAction("About " + self.App_name, self._show_about)

    #    File operations
    def _open_file(self, path=None):   pass   # override: load your format
    def _save_file(self):              pass   # override: save your format
    def _export_file(self): #vers 1
        QMessageBox.information(self, "Export", "Export not yet implemented.")
    def _import_file(self): #vers 1
        QMessageBox.information(self, "Import", "Import not yet implemented.")
    def _clear_recent(self): #vers 1
        self.WS._data["recent_files"] = []; self.WS.save()
        self._set_status("Recent files cleared")

    #    Edit operations
    def _undo(self):         self._set_status("Undo — override in subclass")
    def _redo(self):         self._set_status("Redo — override in subclass")
    def _copy_item(self):    pass   # override: copy selection
    def _paste_item(self):   pass   # override: paste clipboard

    #    View operations
    def _zoom(self, factor: float): pass   # override: zoom your canvas
    def _fit(self):                 pass   # override: fit view
    def _jump(self):                pass   # override: jump to selection

    #    Panel callbacks
    def _on_list_selection_changed(self, row: int): pass
    def _on_tab_changed(self, idx: int):            pass
    def _on_add_item(self):
        self._item_list.addItem(
            QListWidgetItem(f"Item {self._item_list.count()}"))

    def _on_remove_item(self): #vers 1
        row = self._item_list.currentRow()
        if row >= 0: self._item_list.takeItem(row)

    #    Toolbar actions
    def _on_toolbar_action(self, action: str): #vers 1
        pass  # rotate/flip/edit etc.

    #    Tool management
    def _set_active_tool(self, tool: str): #vers 1
        self._active_tool = tool
        for name, btn in self._draw_btns.items():
            btn.setChecked(name == tool)


# GUIWorkshop — assembles all four sections

class GUIWorkshop(_ToolbarMixin, _LayoutMixin, _LogicStubsMixin,
                  ToolMenuMixin, QWidget):
    """Reusable workshop base.  Subclass this, override App_name/config_key
    and the stubs in Section 4.  All chrome, theme, settings, and window
    management are inherited from the four sections above.
    """

    #    Subclass identity — OVERRIDE ALL OF THESE
    App_name        = "Vehicle Workshop"
    App_build       = "Build 1"
    App_author      = "X-Seti"
    App_year        = "2026"
    App_description = "GUIWorkshop base template — IMG Factory 1.6"
    config_key      = "gui_workshop"

    #    Signals
    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    #    Init
    def __init__(self, parent=None, main_window=None): #vers 1
        super().__init__(parent)
        self.main_window     = main_window
        self.standalone_mode = (main_window is None)
        self.is_docked       = not self.standalone_mode
        self.dock_widget     = None

        # Fonts (loaded from settings below)
        self.title_font   = QFont("Arial", 14)
        self.panel_font   = QFont("Arial", 10)
        self.button_font  = QFont("Arial", 10)
        self.infobar_font = QFont("Courier New", 9)
        self.button_display_mode = "both"

        # Margins / spacing (consistent across all workshops)
        self.contmergina = 1; self.contmerginb = 1
        self.contmerginc = 1; self.contmergind = 1; self.setspacing = 2
        self.panelmergina = 5; self.panelmerginb = 5
        self.panelmerginc = 5; self.panelmergind = 5
        self.tabmerginsa  = 2; self.tabmerginsb  = 2
        self.tabmerginsc  = 2; self.tabmerginsd  = 2
        self.panelspacing = 4; self.titlebarheight = 36
        self.toolbarheight = 50; self.statusheight = 22

        # Window chrome state
        self.dragging         = False; self.drag_position    = None
        self.resizing         = False; self.resize_corner    = None
        self.initial_geometry = None;  self.corner_size      = 20
        self.hover_corner     = None

        # AppSettings (global theme)
        if main_window and hasattr(main_window, "app_settings"):
            self.app_settings = main_window.app_settings
        elif APPSETTINGS_AVAILABLE:
            try:    self.app_settings = AppSettings()
            except Exception: self.app_settings = None
        else:
            self.app_settings = None

        if self.app_settings and hasattr(self.app_settings, "theme_changed"):
            self.app_settings.theme_changed.connect(self._refresh_icons)

        # Per-app settings
        self.WS = WorkshopSettings(self.config_key)
        if self.standalone_mode:
            self.resize(max(800, self.WS.get("window_w", 1400)),
                        max(500, self.WS.get("window_h",  800)))
            wx, wy = self.WS.get("window_x", -1), self.WS.get("window_y", -1)
            if wx >= 0 and wy >= 0: self.move(wx, wy)

        self._load_fonts_from_settings()
        self.icon_factory = SVGIconFactory()
        self.setWindowTitle(self.App_name)
        self.setMinimumSize(800, 500)
        self._active_tool = "pencil"
        self._draw_btns   = {}

        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        else:
            self.setWindowFlags(Qt.WindowType.Widget)

        if parent:
            p = parent.pos(); self.move(p.x() + 50, p.y() + 80)

        self.setup_ui()
        self._setup_shortcuts()
        self._apply_theme()

    def _load_fonts_from_settings(self): #vers 1
        ws = self.WS
        self.title_font   = QFont(ws.get("font_title_family",  "Arial"),
                                  ws.get("font_title_size",     14))
        self.panel_font   = QFont(ws.get("font_panel_family",  "Arial"),
                                  ws.get("font_panel_size",     10))
        self.button_font  = QFont(ws.get("font_button_family", "Arial"),
                                  ws.get("font_button_size",    10))
        self.infobar_font = QFont(ws.get("font_info_family",   "Courier New"),
                                  ws.get("font_info_size",       9))
        self.button_display_mode = ws.get("button_display_mode", "both")

    def get_content_margins(self): #vers 1
        return (self.contmergina, self.contmerginb,
                self.contmerginc, self.contmergind)

    def get_panel_margins(self): #vers 1
        return (self.panelmergina, self.panelmerginb,
                self.panelmerginc, self.panelmergind)

    def _setup_shortcuts(self): #vers 1
        for key, fn in [("Ctrl+O", self._open_file), ("Ctrl+S", self._save_file),
                        ("Ctrl+Z", self._undo), ("Ctrl+Y", self._redo),
                        ("Ctrl+Shift+Z", self._redo), ("Ctrl+0", self._fit),
                        ("Ctrl+C", self._copy_item), ("Ctrl+V", self._paste_item)]:
            QShortcut(QKeySequence(key), self).activated.connect(fn)
        for key, tool in [("P","pencil"),("F","fill"),("L","line"),("R","rect"),
                          ("K","picker"),("Z","zoom"),("X","cut"),("V","paste")]:
            QShortcut(QKeySequence(key), self).activated.connect(
                lambda t=tool: self._set_active_tool(t))
        QShortcut(QKeySequence("Shift+R"), self).activated.connect(
            lambda: self._set_active_tool("rect_fill"))


    #    Window chrome
    def _get_resize_corner(self, pos): #Vers 1
        s = self.corner_size; x, y = pos.x(), pos.y()
        w, h = self.width(), self.height()
        if x < s and y < s:    return "top-left"
        if x > w-s and y < s:  return "top-right"
        if x < s and y > h-s:  return "bottom-left"
        if x > w-s and y > h-s: return "bottom-right"
        return None


    def _update_cursor(self, direction): #Vers 2
        cursors = {
            "top":          Qt.CursorShape.SizeVerCursor,
            "bottom":       Qt.CursorShape.SizeVerCursor,
            "left":         Qt.CursorShape.SizeHorCursor,
            "right":        Qt.CursorShape.SizeHorCursor,
            "top-left":     Qt.CursorShape.SizeFDiagCursor,
            "bottom-right": Qt.CursorShape.SizeFDiagCursor,
            "top-right":    Qt.CursorShape.SizeBDiagCursor,
            "bottom-left":  Qt.CursorShape.SizeBDiagCursor,
        }
        self.setCursor(cursors.get(direction, Qt.CursorShape.ArrowCursor))


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


    def _is_on_draggable_area(self, pos): #Vers 1
        if not hasattr(self, 'titlebar'):
            return False
        if not self.titlebar.rect().contains(pos):
            return False
        for w in self.titlebar.findChildren(QPushButton):
            if w.isVisible() and w.geometry().contains(pos):
                return False
        return True


    def mousePressEvent(self, ev): #vers 2
        if ev.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(ev); return
        self.resize_corner = self._get_resize_corner(ev.pos())
        if self.resize_corner:
            self.resizing = True
            self.drag_position = ev.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            ev.accept(); return
        if (hasattr(self, 'titlebar') and
                self.titlebar.geometry().contains(ev.pos())):
            # Only start drag if click is not on a button/interactive child
            from PyQt6.QtWidgets import QAbstractButton, QComboBox, QSlider
            child = self.childAt(ev.pos())
            if child is None or not isinstance(child, (QAbstractButton, QComboBox, QSlider)):
                handle = self.windowHandle()
                if handle: handle.startSystemMove()
                ev.accept(); return
        super().mousePressEvent(ev)


    def _toggle_maximize(self): #Vers 1
        if self.isMaximized(): self.showNormal()
        else: self.showMaximized()


    def toggle_dock_mode(self): #vers 1
        pass  # override if dock support needed


    def mouseReleaseEvent(self, event): #Vers 2
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    def _handle_corner_resize(self, global_pos): #Vers 1
        if not self.resize_corner or not self.drag_position:
            return
        delta = global_pos - self.drag_position
        geometry = self.initial_geometry
        min_w, min_h = 800, 500
        if self.resize_corner == "bottom-right":
            nw = geometry.width() + delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h:
                self.resize(nw, nh)
        elif self.resize_corner == "bottom-left":
            nx = geometry.x() + delta.x()
            nw = geometry.width() - delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(nx, geometry.y(), nw, nh)
        elif self.resize_corner == "top-right":
            ny = geometry.y() + delta.y()
            nw = geometry.width() + delta.x()
            nh = geometry.height() - delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(geometry.x(), ny, nw, nh)
        elif self.resize_corner == "top-left":
            nx = geometry.x() + delta.x()
            ny = geometry.y() + delta.y()
            nw = geometry.width() - delta.x()
            nh = geometry.height() - delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(nx, ny, nw, nh)


    def paintEvent(self, event): #Vers 2
        super().paintEvent(event)
        # Corner handles drawn by _corner_overlay — see _setup_corner_overlay

    def _setup_corner_overlay(self): #vers 4
        """Create or refresh the corner resize overlay."""
        if not self.standalone_mode:
            return
        # Destroy stale overlay if window was resized before it was created
        existing = getattr(self, '_corner_overlay', None)
        if existing is not None:
            existing.setGeometry(0, 0, self.width(), self.height())
            existing.raise_()
            existing.update()
            return
        overlay = _CornerOverlay(self)
        self._corner_overlay = overlay
        overlay.setGeometry(0, 0, self.width(), self.height())
        overlay.show()
        overlay.raise_()

    def _refresh_corner_overlay(self): #vers 1
        if hasattr(self, '_corner_overlay'):
            self._corner_overlay.setGeometry(0, 0, self.width(), self.height())
            self._corner_overlay.update_state(
                getattr(self, 'hover_corner', None),
                self.app_settings)
            self._corner_overlay.raise_()

    def showEvent(self, event): #vers 2
        super().showEvent(event)
        if self.standalone_mode:
            # Small delay ensures window geometry is finalised
            QTimer.singleShot(150, self._setup_corner_overlay)

    def resizeEvent(self, event): #vers 2
        super().resizeEvent(event)
        if hasattr(self,'size_grip'): self.size_grip.move(self.width()-16,self.height()-16)
        self._refresh_corner_overlay()

    def closeEvent(self, event): #Vers 3
        # Stop any running timers before widget is destroyed
        try:
            if hasattr(self, '_anim_timer') and self._anim_timer:
                self._anim_timer.stop(); self._anim_timer = None
        except Exception: pass
        try:
            if hasattr(self, '_shared_txd_worker') and self._shared_txd_worker:
                self._shared_txd_worker.quit()
        except Exception: pass
        # Save window geometry
        if self.standalone_mode:
            g = self.geometry()
            self.WS.set('window_x', g.x())
            self.WS.set('window_y', g.y())
            self.WS.set('window_w', g.width())
            self.WS.set('window_h', g.height())
            self.WS.save()
        self.window_closed.emit()
        event.accept()

# carcols.dat parser

@dataclass
class CarColour: #vers 1
    r: int = 0
    g: int = 0
    b: int = 0

    def to_qcolor(self) -> QColor:
        return QColor(self.r, self.g, self.b)

    def __str__(self):
        return f"{self.r},{self.g},{self.b}"


@dataclass
class CarColEntry: #vers 1
    name:     str                    = ""
    palettes: List[Tuple[int, int]]  = field(default_factory=list)


class CarColsParser: #vers 1
    def __init__(self): #vers 1
        self.colours:      List[CarColour]   = []
        self.vehicles:     List[CarColEntry] = []
        self.header_lines: List[str]         = []
        self.game:         str               = "VC"

    def _detect_game(self, lines: List[str]) -> str: #vers 1
        for ln in lines:
            s = ln.strip()
            if s.lower().startswith("col") and not s.startswith(";"):
                if len(s.split()) > 15:
                    return "SA"
        return "VC"

    def load(self, path: str) -> bool: #vers 2
        try:
            self.colours.clear(); self.vehicles.clear(); self.header_lines.clear()
            with open(path, "r", encoding="latin-1") as f:
                lines = f.readlines()
            self.game = self._detect_game(lines)
            section = None
            for ln in lines:
                s = ln.strip()
                if '#' in s: s = s[:s.index('#')].strip()
                if ';' in s: s = s[:s.index(';')].strip()
                if not s: continue
                sl = s.lower()
                if sl == 'col':  section = 'col'; continue
                if sl == 'car':  section = 'car'; continue
                if sl == 'end':  section = None;  continue
                if section == 'col':
                    try:
                        parts = s.replace('\t', ' ').split()
                        rgb = parts[0].split(',')
                        if len(rgb) == 3:
                            self.colours.append(CarColour(int(rgb[0]),int(rgb[1]),int(rgb[2])))
                        elif len(parts) >= 3:
                            self.colours.append(CarColour(int(parts[0]),int(parts[1]),int(parts[2])))
                    except (ValueError, IndexError):
                        pass
                elif section == 'car':
                    parts = [p.strip() for p in s.split(',')]
                    if not parts or not parts[0]: continue
                    entry = CarColEntry(name=parts[0])
                    nums = []
                    for p in parts[1:]:
                        try: nums.append(int(p.strip()))
                        except ValueError: pass
                    for i in range(0, len(nums)-1, 2):
                        entry.palettes.append((nums[i], nums[i+1]))
                    if entry.palettes:
                        self.vehicles.append(entry)
            return True
        except Exception as ex:
            print(f"CarColsParser.load: {ex}"); return False

    def save(self, path: str) -> bool: #vers 1
        try:
            with open(path, "w", encoding="latin-1") as f:
                for ln in self.header_lines: f.write(ln)
                for c in self.colours: f.write(f"{c.r},{c.g},{c.b}\n")
                f.write("\n")
                for v in self.vehicles:
                    pairs = "   ".join(f"{p},{s}" for p, s in v.palettes)
                    f.write(f"col   {v.name}   {pairs}\n")
            return True
        except Exception as ex:
            print(f"CarColsParser.save: {ex}"); return False


# carmods.dat parser

@dataclass
class CarModEntry: #vers 1
    vehicle: str       = ""
    mods:    List[str] = field(default_factory=list)


class CarModsParser: #vers 1
    def __init__(self): #vers 1
        self.entries:      List[CarModEntry] = []
        self.header_lines: List[str]         = []

    def load(self, path: str) -> bool: #vers 1
        try:
            self.entries.clear(); self.header_lines.clear()
            in_data = False
            with open(path, "r", encoding="latin-1") as f:
                for ln in f:
                    s = ln.strip()
                    if not s or s.startswith(";") or s.startswith("#"):
                        if not in_data: self.header_lines.append(ln)
                        continue
                    in_data = True
                    parts = s.split()
                    if parts:
                        self.entries.append(CarModEntry(vehicle=parts[0], mods=parts[1:]))
            return True
        except Exception as ex:
            print(f"CarModsParser.load: {ex}"); return False

    def save(self, path: str) -> bool: #vers 1
        try:
            with open(path, "w", encoding="latin-1") as f:
                for ln in self.header_lines: f.write(ln)
                for e in self.entries:
                    f.write(f"{e.vehicle}   {chr(32).join(e.mods)}\n")
            return True
        except Exception as ex:
            print(f"CarModsParser.save: {ex}"); return False


# Colour swatch grid

class ColourSwatchGrid(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self._palette:  List[CarColour] = []
        self._selected: int = -1
        self._cols      = 16
        self._cell      = 24
        self.setMinimumHeight(80)
        self.setCursor(Qt.CursorShape.PointingHandCursor)

    def set_palette(self, palette: List[CarColour]): #vers 1
        self._palette = palette
        rows = max(1, (len(palette) + self._cols - 1) // self._cols)
        self.setMinimumHeight(rows * self._cell + 4)
        self.update()

    def paintEvent(self, event): #vers 1
        p = QPainter(self)
        cs = self._cell
        for i, col in enumerate(self._palette):
            x = (i % self._cols) * cs
            y = (i // self._cols) * cs
            p.fillRect(x, y, cs, cs, col.to_qcolor())
            if i == self._selected:
                p.setPen(QPen(QColor(255, 255, 255), 2))
                p.drawRect(x+1, y+1, cs-2, cs-2)
            else:
                p.setPen(QPen(QColor(0, 0, 0, 60), 1))
                p.drawRect(x, y, cs, cs)
        if not self._palette:
            p.setPen(QColor(120, 120, 120))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "No palette loaded")

    def mousePressEvent(self, event): #vers 1
        cs = self._cell
        idx = (int(event.position().y()) // cs) * self._cols + (int(event.position().x()) // cs)
        if 0 <= idx < len(self._palette):
            self._selected = idx
            self.update()
            self.colour_clicked(idx)

    def colour_clicked(self, idx: int): pass  # override


# Tab 1 — Handling

class HandlingTab(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self._parser   = HandlingParser() if _HANDLING_AVAILABLE else None
        self._cur_idx  = -1
        self._blocking = False
        self._field_widgets: Dict[str, QWidget] = {}
        self._stat_bars:     Dict[str, tuple]   = {}
        self._flag_labels:   Dict[int, QLabel]  = {}
        self._build_ui()

    def _build_ui(self): #vers 1
        root = QHBoxLayout(self); root.setContentsMargins(4, 4, 4, 4)
        if not _HANDLING_AVAILABLE:
            root.addWidget(QLabel("Handling Editor unavailable — check Handling_Editor/handling_editor.py"))
            return
        sp = QSplitter(Qt.Orientation.Horizontal)
        # Left
        left = QWidget(); ll = QVBoxLayout(left); ll.setContentsMargins(2,2,2,2)
        ll.addWidget(QLabel("Vehicles"))
        self._search = QLineEdit(); self._search.setPlaceholderText("Search…")
        self._search.textChanged.connect(lambda t: self._refresh_list(t))
        ll.addWidget(self._search)
        self._veh_list = QListWidget()
        self._veh_list.currentRowChanged.connect(self._on_select)
        ll.addWidget(self._veh_list)
        br = QHBoxLayout()
        for lbl, tip, fn, iname in [
                ("Add", "Add entry",       self._add,    "add"),
                ("Del", "Delete entry",    self._delete, "delete"),
                ("Dup", "Duplicate entry", self._dup,    "duplicate")]:
            from PyQt6.QtWidgets import QToolButton
            b = QToolButton(); b.setFixedHeight(28); b.setToolTip(tip)
            b.setText(lbl)
            try:
                ico = getattr(SVGIconFactory, f'{iname}_icon', None)
                if ico:
                    b.setIcon(ico(16, '#ffffff'))
                    b.setIconSize(QSize(14,14))
                    b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            except Exception:
                pass
            b.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
            b.clicked.connect(fn); br.addWidget(b)
        ll.addLayout(br)
        sp.addWidget(left)

        # Centre
        scroll = QScrollArea(); scroll.setWidgetResizable(True)
        ctr = QWidget(); scroll.setWidget(ctr)
        self._form = QFormLayout(ctr); self._form.setSpacing(3); self._form.setContentsMargins(6,6,6,6)
        for fname, ftype, fmin, fmax, tip in VC_FIELDS:
            lbl = QLabel(fname); lbl.setToolTip(tip); lbl.setFixedWidth(200)
            if ftype == "float":
                w = QDoubleSpinBox(); w.setRange(float(fmin), float(fmax)); w.setDecimals(4); w.setSingleStep(0.01)
                w.valueChanged.connect(lambda v, n=fname: self._changed(n, v))
            elif ftype == "int":
                w = QSpinBox(); w.setRange(int(fmin), int(fmax))
                w.valueChanged.connect(lambda v, n=fname: self._changed(n, v))
            elif ftype == "bool":
                w = QCheckBox()
                w.stateChanged.connect(lambda v, n=fname: self._changed(n, int(v > 0)))
            elif ftype == "char":
                w = QComboBox()
                w.addItems(["F","R","4"] if fname == "DriveType" else ["P","D","E"])
                w.currentTextChanged.connect(lambda v, n=fname: self._changed(n, v))
            else:
                w = QLineEdit()
                w.textChanged.connect(lambda v, n=fname: self._changed(n, v))
            w.setToolTip(tip)
            self._field_widgets[fname] = w
            self._form.addRow(lbl, w)
        sp.addWidget(scroll)

        # Right
        right = QWidget(); rl = QVBoxLayout(right); rl.setContentsMargins(2,2,2,2)
        rl.addWidget(QLabel("Vehicle Stats"))
        for label, fn, mx in [("Top Speed","MaxVelocity",200),("Mass","Mass",5000),
                               ("Braking","BrakeDeceleration",30),("Traction","TractionMultiplier",3),
                               ("Engine","EngineAcceleration",20),("Suspension","SuspensionForceLevel",5)]:
            row = QHBoxLayout(); l2 = QLabel(label); l2.setFixedWidth(80)
            bar = QProgressBar(); bar.setRange(0,100); bar.setFixedHeight(18)
            self._stat_bars[fn] = (bar, mx); row.addWidget(l2); row.addWidget(bar); rl.addLayout(row)
        rl.addStretch()
        grp = QGroupBox("Active Flags"); fl = QVBoxLayout(grp)
        for bit, name in list(HANDLING_FLAGS.items())[:16]:
            lbl2 = QLabel(name); lbl2.setStyleSheet("color:#888;")
            lbl2.setFont(QFont("Monospace",8))
            self._flag_labels[bit] = lbl2; fl.addWidget(lbl2)
        rl.addWidget(grp)
        sp.addWidget(right)
        sp.setSizes([200, 600, 220])
        root.addWidget(sp)

    def load_file(self, path: str) -> bool: #vers 1
        if not self._parser: return False
        ok = self._parser.load(path)
        if ok: self._refresh_list()
        return ok

    def save_file(self, path: str) -> bool: #vers 1
        return self._parser.save(path) if self._parser else False

    def _refresh_list(self, ft: str = ""): #vers 1
        self._veh_list.clear()
        for i, e in enumerate(self._parser.entries):
            if ft and ft.lower() not in e.name.lower(): continue
            item = QListWidgetItem(e.name); item.setData(Qt.ItemDataRole.UserRole, i)
            self._veh_list.addItem(item)

    def _on_select(self, row): #vers 1
        item = self._veh_list.item(row)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None or idx >= len(self._parser.entries): return
        self._cur_idx = idx; self._populate(self._parser.entries[idx])

    def _populate(self, entry): #vers 1
        self._blocking = True
        for i, (fname, ftype, *_) in enumerate(VC_FIELDS):
            if i >= len(entry.values): break
            w = self._field_widgets.get(fname)
            if not w: continue
            try:
                v = entry.values[i]
                if ftype == "float": w.setValue(float(v))
                elif ftype == "int": w.setValue(int(v))
                elif ftype == "bool": w.setChecked(int(v) != 0)
                elif ftype == "char" and hasattr(w,"setCurrentText"): w.setCurrentText(str(v))
                elif hasattr(w,"setText"): w.setText(str(v))
            except: pass
        self._blocking = False
        self._update_stats(entry)

    def _changed(self, fname, value): #vers 1
        if self._blocking or self._cur_idx < 0: return
        entry = self._parser.entries[self._cur_idx]
        for i, (fn, *_) in enumerate(VC_FIELDS):
            if fn == fname and i < len(entry.values): entry.values[i] = str(value); break
        self._update_stats(entry)

    def _update_stats(self, entry): #vers 1
        fm = {f[0]: i for i, f in enumerate(VC_FIELDS)}
        for fn, (bar, mx) in self._stat_bars.items():
            idx = fm.get(fn)
            if idx is not None and idx < len(entry.values):
                try: v=float(entry.values[idx]); bar.setValue(min(100,int(v/mx*100))); bar.setFormat(f"{v:.1f}")
                except: bar.setValue(0)
        hf = fm.get("HandlingFlags")
        if hf and hf < len(entry.values):
            try:
                flags = int(entry.values[hf], 16)
                for bit, lbl in self._flag_labels.items():
                    lbl.setStyleSheet("color:#50e090;font-weight:bold;" if flags & bit else "color:#888;")
            except: pass

    def _add(self): #vers 1
        if not self._parser: return
        tmpl = self._parser.entries[0].values[:] if self._parser.entries else ["NEWVEHICLE"]+["0.0"]*36
        tmpl[0] = "NEWVEHICLE"
        e = HandlingEntry(); e.values = tmpl
        self._parser.entries.append(e); self._refresh_list(self._search.text())
        self._veh_list.setCurrentRow(self._veh_list.count()-1)

    def _delete(self): #vers 1
        if self._cur_idx < 0 or not self._parser: return
        name = self._parser.entries[self._cur_idx].name
        if QMessageBox.question(self,"Delete",f"Delete {name}?") != QMessageBox.StandardButton.Yes: return
        self._parser.entries.pop(self._cur_idx); self._cur_idx = -1
        self._refresh_list(self._search.text())

    def _dup(self): #vers 1
        if self._cur_idx < 0 or not self._parser: return
        src = self._parser.entries[self._cur_idx]
        e = HandlingEntry(); e.values = src.values[:]; e.values[0] = src.values[0]+"_COPY"
        self._parser.entries.insert(self._cur_idx+1, e); self._refresh_list(self._search.text())


# Tab 2 — Car Colours

class CarColoursTab(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self._parser  = CarColsParser()
        self._cur_veh = -1
        self._sel_col = -1
        self._build_ui()

    def _build_ui(self): #vers 1
        root = QVBoxLayout(self); root.setContentsMargins(4,4,4,4)
        sp = QSplitter(Qt.Orientation.Horizontal)

        # Left
        left = QWidget(); ll = QVBoxLayout(left); ll.setContentsMargins(2,2,2,2)
        ll.addWidget(QLabel("Vehicles"))
        self._search = QLineEdit(); self._search.setPlaceholderText("Search…")
        self._search.textChanged.connect(lambda t: self._refresh_veh_list(t))
        ll.addWidget(self._search)
        self._veh_list = QListWidget()
        self._veh_list.currentRowChanged.connect(self._on_vehicle_selected)
        ll.addWidget(self._veh_list)
        br = QHBoxLayout()
        for lbl, tip, fn, iname in [("Add",'Add',self._add_vehicle,'add'),("Del",'Delete',self._delete_vehicle,'delete')]:
            from PyQt6.QtWidgets import QToolButton
            b=QToolButton(); b.setFixedHeight(28); b.setToolTip(tip); b.setText(lbl)
            try:
                ico=getattr(SVGIconFactory,f'{iname}_icon',None)
                if ico: b.setIcon(ico(14,'#ffffff')); b.setIconSize(QSize(14,14)); b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            except Exception: pass
            b.setSizePolicy(QSizePolicy.Policy.Preferred,QSizePolicy.Policy.Fixed)

            b=QPushButton(lbl); b.setFixedHeight(26); b.clicked.connect(fn); br.addWidget(b)
        ll.addLayout(br)
        sp.addWidget(left)

        # Centre
        centre = QWidget(); cl = QVBoxLayout(centre); cl.setContentsMargins(4,4,4,4)
        cl.addWidget(QLabel("Global Colour Palette  (click to select / double-click to edit)"))
        self._pal_grid = ColourSwatchGrid()
        self._pal_grid.colour_clicked = self._on_colour_index_clicked
        pal_scroll = QScrollArea(); pal_scroll.setWidgetResizable(True)
        pal_scroll.setWidget(self._pal_grid); pal_scroll.setFixedHeight(160)
        cl.addWidget(pal_scroll)
        cl.addWidget(QLabel("Vehicle Colour Pairs"))
        self._pairs = QTableWidget(0, 3)
        self._pairs.setHorizontalHeaderLabels(["#","Primary","Secondary"])
        self._pairs.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        self._pairs.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        cl.addWidget(self._pairs)
        pr = QHBoxLayout()
        for lbl, fn in [("Add Pair",self._add_pair),("Remove Pair",self._remove_pair)]:
            b=QPushButton(lbl); b.setFixedHeight(28); b.clicked.connect(fn); pr.addWidget(b)
        cl.addLayout(pr)
        sp.addWidget(centre)

        # Right
        right = QWidget(); rl = QVBoxLayout(right); rl.setContentsMargins(4,4,4,4)
        rl.addWidget(QLabel("Selected Colour"))
        self._swatch = QFrame(); self._swatch.setFixedSize(80,80)
        self._swatch.setStyleSheet("background:rgb(0,0,0);border:2px solid #555;")
        rl.addWidget(self._swatch)
        self._col_info = QLabel("Index: —\nRGB: —"); rl.addWidget(self._col_info)
        edit_btn = QPushButton("Edit Colour"); edit_btn.clicked.connect(self._edit_selected_colour)
        rl.addWidget(edit_btn); rl.addStretch()
        self._pal_count = QLabel("0 colours"); rl.addWidget(self._pal_count)
        sp.addWidget(right)
        sp.setSizes([200,560,150]); root.addWidget(sp)

    def load_file(self, path: str) -> bool: #vers 1
        ok = self._parser.load(path)
        if ok:
            self._pal_grid.set_palette(self._parser.colours)
            self._pal_count.setText(f"{len(self._parser.colours)} colours")
            self._refresh_veh_list()
        return ok

    def save_file(self, path: str) -> bool: #vers 1
        return self._parser.save(path)

    def _refresh_veh_list(self, ft: str = ""): #vers 1
        self._veh_list.clear()
        for i, v in enumerate(self._parser.vehicles):
            if ft and ft.lower() not in v.name.lower(): continue
            item = QListWidgetItem(v.name); item.setData(Qt.ItemDataRole.UserRole, i)
            self._veh_list.addItem(item)

    def _on_vehicle_selected(self, row): #vers 1
        item = self._veh_list.item(row)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None: return
        self._cur_veh = idx; self._populate_vehicle_colours(self._parser.vehicles[idx])

    def _populate_vehicle_colours(self, entry: CarColEntry): #vers 1
        self._pairs.setRowCount(0)
        for i, (p, s) in enumerate(entry.palettes):
            self._pairs.insertRow(i)
            self._pairs.setItem(i, 0, QTableWidgetItem(f"Pair {i+1}"))
            pc = self._parser.colours[p] if p < len(self._parser.colours) else CarColour()
            pi = QTableWidgetItem(f"{p}  ({pc.r},{pc.g},{pc.b})")
            pi.setBackground(pc.to_qcolor())
            pi.setForeground(QColor(255,255,255) if (pc.r+pc.g+pc.b)<384 else QColor(0,0,0))
            self._pairs.setItem(i, 1, pi)
            sc2 = self._parser.colours[s] if s < len(self._parser.colours) else CarColour()
            si = QTableWidgetItem(f"{s}  ({sc2.r},{sc2.g},{sc2.b})")
            si.setBackground(sc2.to_qcolor()); si.setForeground(QColor(255,255,255) if (sc2.r+sc2.g+sc2.b)<384 else QColor(0,0,0))
            self._pairs.setItem(i, 2, si)

    def _on_colour_index_clicked(self, idx): #vers 1
        self._sel_col = idx
        if idx < len(self._parser.colours):
            c = self._parser.colours[idx]
            self._swatch.setStyleSheet(f"background:rgb({c.r},{c.g},{c.b});border:2px solid #555;")
            self._col_info.setText(f"Index: {idx}\nRGB: {c.r}, {c.g}, {c.b}")

    def _edit_selected_colour(self): #vers 1
        idx = self._sel_col
        if idx < 0 or idx >= len(self._parser.colours): return
        c = self._parser.colours[idx]
        col = QColorDialog.getColor(c.to_qcolor(), self, f"Edit Colour {idx}")
        if not col.isValid(): return
        self._parser.colours[idx] = CarColour(col.red(), col.green(), col.blue())
        self._pal_grid.set_palette(self._parser.colours)
        self._on_colour_index_clicked(idx)
        if self._cur_veh >= 0: self._populate_vehicle_colours(self._parser.vehicles[self._cur_veh])

    def _add_vehicle(self): #vers 1
        self._parser.vehicles.append(CarColEntry(name="NEWVEHICLE", palettes=[(0,1)]))
        self._refresh_veh_list(self._search.text())

    def _delete_vehicle(self): #vers 1
        if self._cur_veh < 0: return
        name = self._parser.vehicles[self._cur_veh].name
        if QMessageBox.question(self,"Delete",f"Delete {name}?") != QMessageBox.StandardButton.Yes: return
        self._parser.vehicles.pop(self._cur_veh); self._cur_veh = -1
        self._refresh_veh_list(self._search.text())

    def _add_pair(self): #vers 1
        if self._cur_veh < 0: return
        self._parser.vehicles[self._cur_veh].palettes.append((0,1))
        self._populate_vehicle_colours(self._parser.vehicles[self._cur_veh])

    def _remove_pair(self): #vers 1
        if self._cur_veh < 0: return
        row = self._pairs.currentRow(); v = self._parser.vehicles[self._cur_veh]
        if 0 <= row < len(v.palettes): v.palettes.pop(row); self._populate_vehicle_colours(v)


# Tab 3 — Car Mods (SA only)

class CarModsTab(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self._parser  = CarModsParser()
        self._cur_idx = -1
        self._build_ui()

    def _build_ui(self): #vers 1
        root = QHBoxLayout(self); root.setContentsMargins(4,4,4,4)
        sp = QSplitter(Qt.Orientation.Horizontal)
        left = QWidget(); ll = QVBoxLayout(left); ll.setContentsMargins(2,2,2,2)
        ll.addWidget(QLabel("Vehicles (SA only)"))
        self._search = QLineEdit(); self._search.setPlaceholderText("Search…")
        self._search.textChanged.connect(lambda t: self._refresh_list(t)); ll.addWidget(self._search)
        self._veh_list = QListWidget(); self._veh_list.currentRowChanged.connect(self._on_vehicle_selected)
        ll.addWidget(self._veh_list)
        br = QHBoxLayout()
        for lbl, tip, fn, iname in [("Add",'Add',self._add_vehicle,'add'),("Del",'Delete',self._delete_vehicle,'delete')]:
            from PyQt6.QtWidgets import QToolButton
            b=QToolButton(); b.setFixedHeight(28); b.setToolTip(tip); b.setText(lbl)
            try:
                ico=getattr(SVGIconFactory,f'{iname}_icon',None)
                if ico: b.setIcon(ico(14,'#ffffff')); b.setIconSize(QSize(14,14)); b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            except Exception: pass
            b.setSizePolicy(QSizePolicy.Policy.Preferred,QSizePolicy.Policy.Fixed)

            b=QPushButton(lbl); b.setFixedHeight(28); b.clicked.connect(fn); br.addWidget(b)
        ll.addLayout(br); sp.addWidget(left)
        right = QWidget(); rl = QVBoxLayout(right); rl.setContentsMargins(4,4,4,4)
        rl.addWidget(QLabel("Mod Slots"))
        self._mod_list = QListWidget()
        self._mod_list.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        rl.addWidget(self._mod_list)
        mr = QHBoxLayout()
        self._new_mod = QLineEdit(); self._new_mod.setPlaceholderText("mod_model_name"); mr.addWidget(self._new_mod)
        for lbl, fn in [("Add",self._add_mod),("Remove",self._remove_mod)]:
            b=QPushButton(lbl); b.setFixedHeight(28); b.clicked.connect(fn); mr.addWidget(b)
        rl.addLayout(mr); sp.addWidget(right)
        sp.setSizes([250,600]); root.addWidget(sp)

    def load_file(self, path: str) -> bool: #vers 1
        ok = self._parser.load(path)
        if ok: self._refresh_list()
        return ok

    def save_file(self, path: str) -> bool: #vers 1
        return self._parser.save(path)

    def _refresh_list(self, ft: str = ""): #vers 1
        self._veh_list.clear()
        for i, e in enumerate(self._parser.entries):
            if ft and ft.lower() not in e.vehicle.lower(): continue
            item = QListWidgetItem(e.vehicle); item.setData(Qt.ItemDataRole.UserRole, i)
            self._veh_list.addItem(item)

    def _on_vehicle_selected(self, row): #vers 1
        item = self._veh_list.item(row)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None: return
        self._cur_idx = idx; self._populate_mods(self._parser.entries[idx])

    def _populate_mods(self, entry: CarModEntry): #vers 1
        self._mod_list.clear()
        for mod in entry.mods: self._mod_list.addItem(mod)

    def _add_vehicle(self): #vers 1
        self._parser.entries.append(CarModEntry(vehicle="newvehicle",mods=[]))
        self._refresh_list(self._search.text())

    def _delete_vehicle(self): #vers 1
        if self._cur_idx < 0: return
        name = self._parser.entries[self._cur_idx].vehicle
        if QMessageBox.question(self,"Delete",f"Delete {name}?") != QMessageBox.StandardButton.Yes: return
        self._parser.entries.pop(self._cur_idx); self._cur_idx = -1
        self._refresh_list(self._search.text())

    def _add_mod(self): #vers 1
        if self._cur_idx < 0: return
        name = self._new_mod.text().strip()
        if not name: return
        entry = self._parser.entries[self._cur_idx]
        entry.mods.append(name); self._populate_mods(entry); self._new_mod.clear()

    def _remove_mod(self): #vers 1
        if self._cur_idx < 0: return
        rows = sorted({i.row() for i in self._mod_list.selectedItems()}, reverse=True)
        entry = self._parser.entries[self._cur_idx]
        for r in rows:
            if 0 <= r < len(entry.mods): entry.mods.pop(r)
        self._populate_mods(entry)


# Main workshop
class VehicleWorkshop(GLViewportMixin, GUIWorkshop): #vers 3

    def __init__(self, main_window=None, parent=None): #vers 4
        super().__init__(parent)
        self.main_window = main_window
        self._handling_path: Optional[str] = None
        self._carcols_path:  Optional[str] = None
        self._carmods_path:  Optional[str] = None
        # Docked mode: no FramelessWindowHint, widget is embedded in tab
        if parent is None and main_window is None:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        # setup_ui() already called by GUIWorkshop.__init__ via super().__init__()

    def _create_centre_panel(self): #vers 1
        from PyQt6.QtWidgets import QFrame, QVBoxLayout
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        lay = QVBoxLayout(panel); lay.setContentsMargins(0,0,0,0); lay.setSpacing(0)
        self._tabs = QTabWidget()
        self._tab_handling = HandlingTab()
        self._tab_carcols  = CarColoursTab()
        self._tab_carmods  = CarModsTab()
        self._tab_preview = self._create_preview_tab() #TODO this seems bugged, does not show self.viewport
        self._tabs.setCurrentIndex(0)
        self._tabs.addTab(self._tab_preview, "3D Preview")
        self._tabs.addTab(self._tab_handling, "Handling")
        self._tabs.addTab(self._tab_carcols,  "Car Colours")
        self._tabs.addTab(self._tab_carmods,  "Car Mods (SA)")
        lay.addWidget(self._tabs)
        return panel

    def setup_ui(self): #vers 3
        super().setup_ui()

    def _create_preview_tab(self): #vers 1
        """Build the 3D Preview tab with DFFViewport and vehicle controls."""
        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout,
                                     QPushButton, QLabel, QComboBox, QSplitter)
        from PyQt6.QtCore import Qt
        tab = QWidget()
        lay = QVBoxLayout(tab); lay.setContentsMargins(4,4,4,4); lay.setSpacing(4)

        # No toolbar bar in 3D Preview tab — controls are in right panel
        # Open DFF/TXD are in the left panel; Render/Assembly/Paint in right panel

        # GL Viewport — use the shared self.viewport created in setup_ui
        self._vw_viewport = self.viewport
        lay.addWidget(self._vw_viewport, 1)

        # Status
        self._vw_status = QLabel('Open a DFF to preview')
        lay.addWidget(self._vw_status)
        return tab


    # Methods from model_viewer

    def _show_progress(self, visible: bool): #vers 1
        if hasattr(self, '_progress'):
            self._progress.setVisible(visible)
            if visible:
                from PyQt6.QtWidgets import QApplication
                QApplication.processEvents()

    def _light_setup_dialog(self): #vers 2
        """Light direction, intensity and vehicle paint colour dialog."""
        dlg = QDialog(self)
        dlg.setWindowTitle("Light & Paint Setup")
        dlg.resize(360, 380)
        lay = QVBoxLayout(dlg)
        form = QFormLayout()

        vp = self.viewport
        from PyQt6.QtWidgets import QSlider

        # Light direction sliders
        def _slider(lo, hi, val, scale=10):
            s = QSlider(Qt.Orientation.Horizontal)
            s.setRange(int(lo*scale), int(hi*scale))
            s.setValue(int(val*scale))
            return s

        sx = _slider(-10, 10, vp._light_dir[0])
        sy = _slider(-10, 10, vp._light_dir[1])
        sz = _slider(-10, 10, vp._light_dir[2])
        sa = _slider(0, 1, vp._ambient, 100)
        sd = _slider(0, 2, vp._diffuse, 100)

        lx_lbl = QLabel(f"{vp._light_dir[0]:.1f}")
        ly_lbl = QLabel(f"{vp._light_dir[1]:.1f}")
        lz_lbl = QLabel(f"{vp._light_dir[2]:.1f}")
        la_lbl = QLabel(f"{vp._ambient:.2f}")
        ld_lbl = QLabel(f"{vp._diffuse:.2f}")

        def _upd():
            x = sx.value()/10; y = sy.value()/10; z = sz.value()/10
            a = sa.value()/100; d = sd.value()/100
            lx_lbl.setText(f"{x:.1f}"); ly_lbl.setText(f"{y:.1f}"); lz_lbl.setText(f"{z:.1f}")
            la_lbl.setText(f"{a:.2f}"); ld_lbl.setText(f"{d:.2f}")
            vp.set_light_dir(x, y, z); vp.set_ambient(a); vp.set_diffuse(d)

        for s in (sx,sy,sz,sa,sd): s.valueChanged.connect(_upd)

        def _row(lbl, slider, val_lbl):
            rw = QWidget(); rl = QHBoxLayout(rw); rl.setContentsMargins(0,0,0,0)
            rl.addWidget(slider,1); rl.addWidget(val_lbl)
            form.addRow(lbl, rw)

        _row("Light X:", sx, lx_lbl)
        _row("Light Y:", sy, ly_lbl)
        _row("Light Z:", sz, lz_lbl)
        _row("Ambient:", sa, la_lbl)
        _row("Diffuse:", sd, ld_lbl)

        lay.addLayout(form)

        # Paint colours
        from PyQt6.QtWidgets import QGroupBox, QColorDialog
        paint_grp = QGroupBox("Vehicle Paint Preview")
        paint_lay = QHBoxLayout(paint_grp)

        def _colour_btn(label, current_rgb, setter):
            def _to_qcolor(rgb):
                from PyQt6.QtGui import QColor
                return QColor(int(rgb[0]*255), int(rgb[1]*255), int(rgb[2]*255))
            btn = QPushButton(label)
            btn.setStyleSheet(f"background-color: rgb({int(current_rgb[0]*255)},{int(current_rgb[1]*255)},{int(current_rgb[2]*255)})")
            btn.setFixedHeight(28)
            def _pick():
                col = QColorDialog.getColor(_to_qcolor(setter.__self__._paint1 if 'paint1' in label.lower() or 'primary' in label.lower() else setter.__self__._paint2), dlg)
                if col.isValid():
                    rgb = (col.redF(), col.greenF(), col.blueF())
                    setter(rgb)
                    btn.setStyleSheet(f"background-color: {col.name()}")
                    vp.update()
            btn.clicked.connect(_pick)
            return btn

        vp = self.viewport
        p1_btn = QPushButton("Primary Paint")
        p1_btn.setStyleSheet(f"background-color: rgb({int(vp._paint1[0]*255)},{int(vp._paint1[1]*255)},{int(vp._paint1[2]*255)})")
        p1_btn.setFixedHeight(28)
        def _pick1():
            from PyQt6.QtGui import QColor
            col = QColorDialog.getColor(QColor(int(vp._paint1[0]*255),int(vp._paint1[1]*255),int(vp._paint1[2]*255)), dlg)
            if col.isValid():
                vp._paint1 = (col.redF(), col.greenF(), col.blueF())
                p1_btn.setStyleSheet(f"background-color: {col.name()}")
                vp.update()
        p1_btn.clicked.connect(_pick1)

        p2_btn = QPushButton("Secondary Paint")
        p2_btn.setStyleSheet(f"background-color: rgb({int(vp._paint2[0]*255)},{int(vp._paint2[1]*255)},{int(vp._paint2[2]*255)})")
        p2_btn.setFixedHeight(28)
        def _pick2():
            from PyQt6.QtGui import QColor
            col = QColorDialog.getColor(QColor(int(vp._paint2[0]*255),int(vp._paint2[1]*255),int(vp._paint2[2]*255)), dlg)
            if col.isValid():
                vp._paint2 = (col.redF(), col.greenF(), col.blueF())
                p2_btn.setStyleSheet(f"background-color: {col.name()}")
                vp.update()
        p2_btn.clicked.connect(_pick2)

        paint_lay.addWidget(p1_btn); paint_lay.addWidget(p2_btn)
        lay.addWidget(paint_grp)

        reset_btn = QPushButton("Reset Defaults")
        def _reset():
            sx.setValue(10); sy.setValue(20); sz.setValue(15)
            sa.setValue(30); sd.setValue(85)
            vp._paint1 = (0.80, 0.20, 0.20)
            vp._paint2 = (0.20, 0.20, 0.80)
            p1_btn.setStyleSheet("background-color: rgb(204,51,51)")
            p2_btn.setStyleSheet("background-color: rgb(51,51,204)")
            vp.update()
        reset_btn.clicked.connect(_reset)

        btn_row = QHBoxLayout()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dlg.accept)
        btn_row.addWidget(reset_btn)
        btn_row.addStretch()
        btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)
        dlg.exec()

    def _vw_set_mode(self, mode): #vers 1
        vp = getattr(self,'_vw_viewport',None)
        if vp and hasattr(vp,'set_render_mode'): vp.set_render_mode(mode)

    def _vw_pick_dff(self): #vers 1
        from PyQt6.QtWidgets import QFileDialog
        path,_=QFileDialog.getOpenFileName(self,'Open DFF','','DFF (*.dff);;All (*)')
        if not path: return
        self._vw_load_dff(path)

    def _vw_pick_txd(self): #vers 1
        from PyQt6.QtWidgets import QFileDialog
        path,_=QFileDialog.getOpenFileName(self,'Open TXD','','TXD (*.txd);;All (*)')
        if not path: return
        self._vw_load_txd(path)

    def _vw_load_dff(self, path: str): #vers 2
        try:
            from apps.methods.dff_parser import load_dff
            m = load_dff(path)
            if not m: return
            self._vw_model = m
            self._vw_status.setText(f'{os.path.basename(path)} — {len(m.geometries)} geoms')
            # Also feed the full load_dff pipeline (geom list, frame tree, shared TXDs)
            self.load_dff(path)
            # Auto-load matching TXD
            import os as _os
            for ext in ('.txd','.TXD'):
                txd=_os.path.splitext(path)[0]+ext
                if _os.path.isfile(txd): self._vw_load_txd(txd); break
        except Exception as e:
            self._vw_status.setText(f'Error: {e}')

    def _vw_load_txd(self, path: str): #vers 2
        try:
            from apps.methods.txd_parser import parse_txd
            with open(path,'rb') as f: data=f.read()
            textures=parse_txd(data)
            vp=getattr(self,'_vw_viewport',None)
            if vp and hasattr(vp,'_upload_textures') and textures:
                vp._upload_textures(textures, additive=True)
                vp.update()
            # Populate texture list
            if textures and hasattr(self,'_tex_list'):
                from PyQt6.QtGui import QIcon, QImage, QPixmap
                from PyQt6.QtWidgets import QListWidgetItem
                from PyQt6.QtCore import Qt
                self._tex_list.clear()
                for t in textures:
                    item = QListWidgetItem(f"{t['name']}  {t.get('width',0)}\xd7{t.get('height',0)}")
                    rgba=t.get('rgba_data',b''); w=t.get('width',0); h=t.get('height',0)
                    if rgba and w>0 and h>0:
                        try:
                            img=QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888)
                            px=QPixmap.fromImage(img).scaled(32,32,
                                Qt.AspectRatioMode.KeepAspectRatio,
                                Qt.TransformationMode.SmoothTransformation)
                            item.setIcon(QIcon(px))
                        except Exception: pass
                    self._tex_list.addItem(item)
        except Exception as e:
            self._vw_status.setText(f'TXD error: {e}')

    def _vw_toggle_assembly(self, enabled: bool): #vers 1
        m = getattr(self,'_vw_model',None)
        vp = getattr(self,'_vw_viewport',None)
        if not m or not vp or not hasattr(vp,'set_assembly_mode'): return
        vp.set_assembly_mode(enabled)
        if enabled:
            vp.load_all_geometries(m.geometries,[g.materials for g in m.geometries],
                                   m.frames,m.atomics,
                                   damaged=getattr(self,'_damage_mode',False))
        elif m.geometries:
            vp.load_geometry(m.geometries[0],m.geometries[0].materials)

    def _vw_pick_paint1(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp=getattr(self,'_vw_viewport',None)
        if not vp or not hasattr(vp,'_paint1'): return
        p=vp._paint1
        col=QColorDialog.getColor(QColor(int(p[0]*255),int(p[1]*255),int(p[2]*255)),self)
        if col.isValid():
            vp._paint1=(col.redF(),col.greenF(),col.blueF())
            self._vw_paint1.setStyleSheet(f'background:{col.name()}')
            vp.update()

    def _vw_pick_paint2(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp=getattr(self,'_vw_viewport',None)
        if not vp or not hasattr(vp,'_paint2'): return
        p=vp._paint2
        col=QColorDialog.getColor(QColor(int(p[0]*255),int(p[1]*255),int(p[2]*255)),self)
        if col.isValid():
            vp._paint2=(col.redF(),col.greenF(),col.blueF())
            self._vw_paint2.setStyleSheet(f'background:{col.name()}')
            vp.update()

    def _open_file(self, path=None): #vers 4
        """Open button — shows dialog when no path given.
        When path is provided, loads directly without any dialog."""
        # Qt triggered signal passes checked=False/True — treat as no path
        if not isinstance(path, (str, bytes, os.PathLike)):
            path = None
        if path is not None:
            name = os.path.basename(path).lower()
            if name.endswith('.dff'):
                self._vw_load_dff(path)
                self._tabs.setCurrentIndex(0)
            elif name.endswith('.txd'):
                self._vw_load_txd(path)
                self._tabs.setCurrentIndex(0)
            elif 'handling' in name:
                if self._tab_handling.load_file(path):
                    self._handling_path = path
                    self._set_status(f"Handling: {os.path.basename(path)}")
                    try:
                        mw = getattr(self, 'main_window', None)
                        if mw and hasattr(self._tab_handling, '_parser') and self._tab_handling._parser:
                            names = {e.name.lower() for e in self._tab_handling._parser.entries if e.name}
                            mw.vehicle_names = getattr(mw, 'vehicle_names', set()) | names
                    except Exception:
                        pass
            elif 'carcols' in name:
                if self._tab_carcols.load_file(path):
                    self._carcols_path = path
                    self._set_status(f"Car Colours: {os.path.basename(path)}")
            elif 'carmods' in name:
                if self._tab_carmods.load_file(path):
                    self._carmods_path = path
                    self._set_status(f"Car Mods: {os.path.basename(path)}")
            elif ('vehicles' in name or 'default' in name) and name.endswith('.ide'):
                # SA/VC vehicles.ide or GTA3 default.ide — parse wheel scales
                try:
                    self._vehicles_ide_path = path
                    self._parse_sa_vehicles_ide(path)
                    self._set_status(f"Vehicles IDE: {os.path.basename(path)}")
                except Exception as e:
                    self._set_status(f"Vehicles IDE error: {e}")
            elif 'cargrp' in name:
                # cargrp.dat — informational for now
                self._cargrp_path = path
                self._set_status(f"Car Groups: {os.path.basename(path)}")
            return
        # No path — show dialog based on active tab
        active_tab = self._tabs.currentIndex() if hasattr(self, '_tabs') else -1
        if active_tab == 0:
            self._vw_pick_dff()
            return
        path, _ = QFileDialog.getOpenFileName(
            self, "Open Vehicle Data File", "",
            "All supported (*.dff *.cfg *.dat *.txd);;"
            "DFF Model (*.dff);;TXD Textures (*.txd);;"
            "Handling (handling.cfg *.cfg);;Car Colours (carcols.dat *.dat);;"
            "Car Mods (carmods.dat *.dat);;All files (*)")
        if not path: return
        name = os.path.basename(path).lower()
        if name.endswith('.dff'):
            self._vw_load_dff(path)
            self._tabs.setCurrentIndex(0)
        elif name.endswith('.txd'):
            self._vw_load_txd(path)
            self._tabs.setCurrentIndex(0)
        elif "handling" in name:
            if self._tab_handling.load_file(path):
                self._handling_path = path
                self._tabs.setCurrentWidget(self._tab_handling)
                self._set_status(f"Handling: {os.path.basename(path)}")
                # Cache vehicle names on main_window for right-click menu
                try:
                    mw = getattr(self, 'main_window', None)
                    if mw and hasattr(self._tab_handling, '_parser') and self._tab_handling._parser:
                        names = {e.name.lower() for e in self._tab_handling._parser.entries if e.name}
                        existing = getattr(mw, 'vehicle_names', set())
                        mw.vehicle_names = existing | names
                except Exception:
                    pass
        elif "carcols" in name:
            if self._tab_carcols.load_file(path):
                self._carcols_path = path
                self._tabs.setCurrentWidget(self._tab_carcols)
                self._set_status(f"Car Colours: {os.path.basename(path)}")
        elif "carmods" in name:
            if self._tab_carmods.load_file(path):
                self._carmods_path = path
                self._tabs.setCurrentWidget(self._tab_carmods)
                self._set_status(f"Car Mods: {os.path.basename(path)}")
        else:
            QMessageBox.warning(self, "Unknown",
                "Expected a .dff, .txd, handling.cfg, carcols.dat or carmods.dat")

    def _save_file(self): #vers 1
        idx = self._tabs.currentIndex()
        if idx == 0 and self._handling_path:
            self._tab_handling.save_file(self._handling_path)
            self._set_status(f"Saved {os.path.basename(self._handling_path)}")
        elif idx == 1 and self._carcols_path:
            self._tab_carcols.save_file(self._carcols_path)
            self._set_status(f"Saved {os.path.basename(self._carcols_path)}")
        elif idx == 2 and self._carmods_path:
            self._tab_carmods.save_file(self._carmods_path)
            self._set_status(f"Saved {os.path.basename(self._carmods_path)}")
        else:
            self._save_as()

    def _save_as(self): #vers 1
        idx = self._tabs.currentIndex()
        hints = ["Handling (handling.cfg *.cfg)", "Car Colours (carcols.dat *.dat)", "Car Mods (carmods.dat *.dat)"]
        path, _ = QFileDialog.getSaveFileName(self, "Save As", "", hints[idx])
        if not path: return
        if idx == 0: self._handling_path = path; self._tab_handling.save_file(path)
        elif idx == 1: self._carcols_path = path; self._tab_carcols.save_file(path)
        elif idx == 2: self._carmods_path = path; self._tab_carmods.save_file(path)

    def _open_specific(self, kind: str): #vers 1
        f = {"handling":"Handling (handling.cfg *.cfg)",
             "carcols": "Car Colours (carcols.dat *.dat)",
             "carmods":  "Car Mods (carmods.dat *.dat)"}[kind]
        path, _ = QFileDialog.getOpenFileName(self, f"Open {kind}", "", f)
        if path: self._open_file(path)

    def _build_menus_into_qmenu(self, pm): #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open…",              self._open_file)
        fm.addAction("Save",               self._save_file)
        fm.addAction("Save As…",           self._save_as)
        fm.addSeparator()
        fm.addAction("Open handling.cfg",  lambda: self._open_specific("handling"))
        fm.addAction("Open carcols.dat",   lambda: self._open_specific("carcols"))
        fm.addAction("Open carmods.dat",   lambda: self._open_specific("carmods"))
        fm.addSeparator()
        fm.addAction("Close", self.close)


# - Main workshop widget (RadarWorkshop pattern)
def open_vehicle_workshop(main_window=None, path: str = None): #vers 1
    if not QApplication.instance():
        try:
            from PyQt6.QtGui import QSurfaceFormat
            _f = QSurfaceFormat()
            _f.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
            _f.setVersion(2, 1)
            QSurfaceFormat.setDefaultFormat(_f)
        except Exception:
            pass
    app = QApplication.instance() or QApplication(sys.argv)
    w = VehicleWorkshop(main_window)
    w.resize(1200, 720)
    w.show()
    if path: w._open_file(path)
    return w


if __name__ == "__main__": #vers 1
    # Must set compat profile BEFORE QApplication
    try:
        from PyQt6.QtGui import QSurfaceFormat
        _f = QSurfaceFormat()
        _f.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
        _f.setVersion(2, 1)
        QSurfaceFormat.setDefaultFormat(_f)
    except Exception:
        pass
    app = QApplication(sys.argv)
    w = VehicleWorkshop()
    w.resize(1200, 720)
    w.show()
    sys.exit(app.exec())
