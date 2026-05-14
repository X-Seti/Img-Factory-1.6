#!/usr/bin/env python3
#this belongs in apps/components/Model_Viewer/model_viewer.py - Version: 2
# X-Seti - May10 2026 - IMG Factory 1.6 - DFF Model Viewer
"""
DFF Model Viewer - OpenGL hardware 3D viewer for GTA RenderWare DFF files.
UI based on RadarWorkshop template (full titlebar, menus, settings, chrome).
Canvas: DFFViewport(QOpenGLWidget).
View-only. Orbit/pan/zoom, wireframe/solid/textured, prelighting, light setup.
"""

import os, json, sys, requests, threading, struct, re, math, shutil
from datetime import datetime
from pathlib import Path
from typing import Optional, List, Dict, Tuple

os.environ.setdefault('QT_QPA_PLATFORM', 'xcb')
os.environ.setdefault('QSG_RHI_BACKEND',  'opengl')

#Adding Standalone
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QSplitter,
    QLabel, QPushButton, QListWidget, QListWidgetItem, QFrame,
    QFileDialog, QSizePolicy, QButtonGroup, QMessageBox,
    QDialog, QFormLayout, QDoubleSpinBox, QCheckBox, QGroupBox,
    QTabWidget, QSlider, QComboBox, QSpinBox, QMenu, QScrollArea,
    QFontComboBox
)
from PyQt6.QtCore import pyqtSignal, Qt, QPoint, QSize, QThread, QTimer
from PyQt6.QtGui import  QAction, QBrush, QColor, QFont, QIcon, QImage, QKeySequence, QPainter, QPainterPath, QPen, QPixmap, QShortcut

try:
    from PyQt6.QtOpenGLWidgets import QOpenGLWidget
    from PyQt6.QtGui import QSurfaceFormat
    from OpenGL.GL  import *
    from OpenGL.GLU import *
    OPENGL_AVAILABLE = True
    _fmt = QSurfaceFormat()
    _fmt.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
    _fmt.setVersion(2, 1)
    QSurfaceFormat.setDefaultFormat(_fmt)
except Exception:
    QOpenGLWidget      = QWidget
    OPENGL_AVAILABLE   = False
    print("[ModelViewer] PyOpenGL not available — install python3-opengl")

# Shared DFFViewport — import from methods/, fallback to local methods/
try:
    from apps.methods.dff_viewport import DFFViewport
except ImportError:
    from apps.components.Model_Viewer.methods.dff_viewport import DFFViewport

try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
    ICONS_AVAILABLE = True
except ImportError:
    ICONS_AVAILABLE = False


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

App_name  = "Model Viewer"
App_build = "May 2026"
Build     = "Build 023"


#    Infrastructure imports
try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
    ICONS_AVAILABLE = True
except ImportError:
    ICONS_AVAILABLE = False
    class SVGIconFactory:
        @staticmethod
        def settings_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def properties_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def info_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def open_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def save_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def minimize_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def maximize_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def close_icon(s=20, c='#fff'): return QIcon()

try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    APPSETTINGS_AVAILABLE = False
    AppSettings = None


try:
    from apps.gui.tool_menu_mixin import ToolMenuMixin
except ImportError:
    class ToolMenuMixin:
        def get_menu_title(self): return App_name
        def _build_menus_into_qmenu(self, m): pass
        def _get_tool_menu_style(self): return 'dropdown'


## Methods list -
# MVSettings.__init__ / get / set / save
# DFFViewport.__init__
# DFFViewport._get_ui_color
# DFFViewport.initializeGL / resizeGL / paintGL
# DFFViewport._draw_grid / _draw_axes
# DFFViewport._draw_wireframe / _draw_solid / _draw_textured
# DFFViewport._upload_textures / clear_textures / _auto_fit
# DFFViewport.load_geometry / set_render_mode / set_backface_cull
# DFFViewport.set_show_grid / set_prelight / set_light_dir / reset_camera
# DFFViewport.mousePressEvent / mouseMoveEvent / mouseReleaseEvent / wheelEvent
# ModelViewer._build_menus_into_qmenu
# ModelViewer.__init__ / get_content_margins / get_panel_margins / get_tab_margins
# ModelViewer.setup_ui / _create_toolbar / _create_left_panel
# ModelViewer._create_centre_panel / _create_right_panel / _create_status_bar
# ModelViewer._show_workshop_settings / _light_setup_dialog
# ModelViewer._refresh_icons / _apply_theme / _get_icon_color / _on_menu_btn_clicked
# ModelViewer._open_dff / _open_txd / load_dff / load_txd
# ModelViewer._populate_geom_list / _on_geom_selected / _set_status
# ModelViewer._get_resize_corner / _update_cursor / _is_on_draggable_area
# ModelViewer._handle_corner_resize / _setup_corner_overlay / _refresh_corner_overlay
# ModelViewer.mousePressEvent / mouseMoveEvent / mouseReleaseEvent
# ModelViewer.resizeEvent / showEvent / closeEvent / paintEvent
# _CornerOverlay.__init__ / paintEvent / update_state
# open_model_viewer



# - Settings
class MVSettings:
    """Lightweight JSON settings for Model Viewer."""
    _PATH = os.path.expanduser('~/.config/imgfactory/model_viewer.json')

    def __init__(self): #vers 1
        self._data = {}
        try:
            if os.path.isfile(self._PATH):
                with open(self._PATH) as f:
                    self._data = json.load(f)
        except Exception:
            pass

    def get(self, key, default=None): #vers 1
        return self._data.get(key, default)

    def set(self, key, value): #vers 1
        self._data[key] = value

    def save(self): #vers 1
        try:
            os.makedirs(os.path.dirname(self._PATH), exist_ok=True)
            with open(self._PATH, 'w') as f:
                json.dump(self._data, f, indent=2)
        except Exception:
            pass

    def get_recent(self):
        return self._data.get('recent', [])

    def add_recent(self, path):
        r = self.get_recent()
        if path in r: r.remove(path)
        r.insert(0, path)
        self._data['recent'] = r[:10]


# - Corner overlay (from RadarWorkshop)
class _CornerOverlay(QWidget):
    """Transparent overlay that draws corner resize triangles on top of all children.
    Uses setMask() so only the triangle pixels exist — fully transparent elsewhere.
    WA_AlwaysStackOnTop keeps it above all sibling widgets on Wayland/KDE."""

    SIZE = 20   # triangle leg size in pixels

    def __init__(self, parent): #vers 3
        super().__init__(parent)
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, True)
        self.setAttribute(Qt.WidgetAttribute.WA_NoSystemBackground, True)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground, True)
        self.setAttribute(Qt.WidgetAttribute.WA_AlwaysStackOnTop, True)
        self.setWindowFlags(Qt.WindowType.Widget)
        self._hover_corner = None
        self._app_settings = None
        self.setGeometry(0, 0, parent.width(), parent.height())
        self._update_mask()

    def _update_mask(self): #vers 1
        """Create a mask covering only the four corner triangles."""
        from PyQt6.QtGui import QRegion, QPolygon
        from PyQt6.QtCore import QPoint
        s = self.SIZE
        w, h = self.width(), self.height()
        region = QRegion()
        for pts in [
            [QPoint(0,0),    QPoint(s,0),    QPoint(0,s)],     # top-left
            [QPoint(w,0),    QPoint(w-s,0),  QPoint(w,s)],     # top-right
            [QPoint(0,h),    QPoint(s,h),    QPoint(0,h-s)],   # bottom-left
            [QPoint(w,h),    QPoint(w-s,h),  QPoint(w,h-s)],   # bottom-right
        ]:
            region = region.united(QRegion(QPolygon(pts)))
        self.setMask(region)

    def update_state(self, hover_corner, app_settings): #vers 2
        self._hover = hover_corner
        self._settings = app_settings
        self.update()

    def setGeometry(self, *args): #vers 1
        super().setGeometry(*args)
        self._update_mask()

    def resizeEvent(self, event): #vers 1
        super().resizeEvent(event)
        self._update_mask()

    def paintEvent(self, event): #vers 2
        s = self.SIZE
        if self._app_settings:
            try:
                colors = self._app_settings.get_theme_colors()
                accent = QColor(colors.get('accent_primary', '#4682FF'))
            except Exception:
                accent = QColor(70, 130, 255)
        else:
            accent = QColor(70, 130, 255)
        accent.setAlpha(200)
        hover_c = QColor(accent); hover_c.setAlpha(255)
        w, h = self.width(), self.height()
        corners = {
            'top-left':     [(0,0),  (s,0),   (0,s)],
            'top-right':    [(w,0),  (w-s,0), (w,s)],
            'bottom-left':  [(0,h),  (s,h),   (0,h-s)],
            'bottom-right': [(w,h),  (w-s,h), (w,h-s)],
        }
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        for name, pts in corners.items():
            path = QPainterPath()
            path.moveTo(*pts[0]); path.lineTo(*pts[1]); path.lineTo(*pts[2])
            path.closeSubpath()
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(hover_c if self._hover_corner == name else accent))
            painter.drawPath(path)
        painter.end()

# DFFViewport imported from apps.methods.dff_viewport

# - Main workshop widget (RadarWorkshop pattern)
class ModelViewer(ToolMenuMixin, QWidget):
    """Model Viewer — RadarWorkshop UI template with DFFViewport canvas."""

    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    def _build_menus_into_qmenu(self, pm): #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open DFF…",  self._open_dff)
        fm.addAction("Open TXD…",  self._open_txd)
        fm.addSeparator()
        recent = self.MV_settings.get_recent()
        if recent:
            rm = fm.addMenu("Recent DFFs")
            for p in recent:
                a = rm.addAction(Path(p).name)
                a.setToolTip(p)
                a.triggered.connect(lambda _=False, pp=p: self.load_dff(pp))
        fm.addSeparator()
        fm.addAction("Close", self.close)
        vm = pm.addMenu("View")
        vm.addAction("Wireframe",  lambda: self._set_mode('wireframe'))
        vm.addAction("Solid",      lambda: self._set_mode('solid'))
        vm.addAction("Textured",   lambda: self._set_mode('textured'))
        vm.addSeparator()
        vm.addAction("Reset Camera", self.viewport.reset_camera)
        vm.addSeparator()
        vm.addAction("Light Setup…", self._light_setup_dialog)
        vm.addSeparator()
        vm.addAction("About", self._show_about)

    def __init__(self, parent=None, main_window=None): #vers 1
        super().__init__(parent)
        self.main_window      = main_window
        self.standalone_mode  = (main_window is None)
        self.is_docked        = not self.standalone_mode
        self.button_display_mode = 'both'
        self.dock_display_mode   = None

        # Fonts
        self.title_font   = QFont("Arial", 14)
        self.panel_font   = QFont("Arial", 10)
        self.button_font  = QFont("Arial", 10)
        self.infobar_font = QFont("Courier New", 9)

        # Window chrome
        self.use_system_titlebar  = False
        self.window_always_on_top = False
        self.dragging             = False
        self.drag_position        = None
        self.resizing             = False
        self.resize_corner        = None
        self.corner_size          = 20
        self.hover_corner         = None

        # App settings (theme)
        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        elif APPSETTINGS_AVAILABLE:
            try:    self.app_settings = AppSettings()
            except: self.app_settings = None
        else:
            self.app_settings = None

        if self.app_settings and hasattr(self.app_settings, 'theme_changed'):
            self.app_settings.theme_changed.connect(self._refresh_icons)

        # Per-tool settings
        self.MV_settings = MVSettings()

        # Spacing/margins (template pattern)
        self.contmergina=1; self.contmerginb=1; self.contmerginc=1; self.contmergind=1
        self.setspacing=2
        self.panelmergina=5; self.panelmerginb=5; self.panelmerginc=5; self.panelmergind=5
        self.panelspacing=5
        self.titlebarheight=45; self.toolbarheight=50
        self.tabmerginsa=5; self.tabmerginsb=0; self.tabmerginsc=5; self.tabmerginsd=0
        self.statusheight=22

        self.icon_factory = SVGIconFactory() if ICONS_AVAILABLE else None

        self.setWindowTitle(App_name)
        self.resize(1200, 780)
        self.setMinimumSize(800, 500)

        # Viewer state
        self._dff_model    = None
        self._last_dir     = self.MV_settings.get('last_dir', '')
        self._current_geom = 0

        # Restore geometry
        if self.standalone_mode:
            wx = self.MV_settings.get('window_x', -1)
            wy = self.MV_settings.get('window_y', -1)
            ww = self.MV_settings.get('window_w', 1200)
            wh = self.MV_settings.get('window_h', 780)
            self.resize(max(800, ww), max(500, wh))
            if wx >= 0 and wy >= 0:
                self.move(wx, wy)

        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        else:
            self.setWindowFlags(Qt.WindowType.Widget)

        if parent:
            p = parent.pos(); self.move(p.x()+50, p.y()+80)

        self.setup_ui()
        self._apply_theme()
        # App icon — colour from theme bg_panel so it updates with theme changes
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SIF
            _ic = '#4a9fd4'
            if self.app_settings:
                _colors = self.app_settings.get_theme_colors()
                _ic = _colors.get('bg_panel', _colors.get('accent_primary', '#4a9fd4'))
            self.setWindowIcon(_SIF.mesh_icon(32, _ic))
        except Exception:
            pass

    # - margins (template)

    def get_content_margins(self): #vers 1
        return (self.contmergina, self.contmerginb, self.contmerginc, self.contmergind)

    def get_panel_margins(self): #vers 1
        return (self.panelmergina, self.panelmerginb, self.panelmerginc, self.panelmergind)

    def get_tab_margins(self): #vers 1
        return (self.tabmerginsa, self.tabmerginsb, self.tabmerginsc, self.tabmerginsd)

    # - setup_ui

    def setup_ui(self): #vers 2
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(*self.get_content_margins())
        main_layout.setSpacing(self.setspacing)

        # Viewport must exist before toolbar (toolbar buttons reference it)
        self.viewport = DFFViewport()
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


    # - toolbar
    def _create_toolbar(self): #vers 2
        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setObjectName("titlebar")
        self.toolbar.installEventFilter(self)
        self.toolbar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.toolbar.setMouseTracking(True)
        self.toolbar.setFixedHeight(self.titlebarheight)
        self.titlebar = self.toolbar
        ic = self._get_icon_color()
        lay = QHBoxLayout(self.toolbar)
        lay.setContentsMargins(*self.get_panel_margins())
        lay.setSpacing(self.panelspacing)

        def _icon(name, size=20):
            if not ICONS_AVAILABLE: return None
            try:
                fn = getattr(SVGIconFactory, name+'_icon', None)
                return fn(size, ic) if fn else None
            except Exception: return None

        def _ibtn(tip, cb, iname, size=35):
            b = QPushButton(); b.setFixedSize(size, 28)
            b.setToolTip(tip)
            ico = _icon(iname)
            if ico: b.setIcon(ico); b.setIconSize(QSize(size-15, size-15))
            b.clicked.connect(cb)
            return b

        def _tbtn(text, tip, cb, iname=None):
            from PyQt6.QtWidgets import QToolButton
            b = QToolButton(); b.setToolTip(tip); b.setFixedHeight(28)
            b.setFont(self.button_font)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(16,16))
                b.setText(text)
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            else:
                b.setText(text)
            b.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
            b.clicked.connect(cb)
            return b

        # Menu | Settings
        self.menu_toggle_btn = QPushButton("Menu")
        self.menu_toggle_btn.setFont(self.button_font)
        self.menu_toggle_btn.setFixedHeight(28)
        ico = _icon('menu')
        if ico: self.menu_toggle_btn.setIcon(ico); self.menu_toggle_btn.setIconSize(QSize(16,16))
        self.menu_toggle_btn.clicked.connect(self._on_menu_btn_clicked)
        lay.addWidget(self.menu_toggle_btn)

        self.settings_btn = _ibtn("Viewer Settings", self._show_workshop_settings, 'settings')
        lay.addWidget(self.settings_btn)

        lay.addSpacing(4)

        # → title (draggable centre area) →
        self._title_lbl = QLabel(App_name)
        self._title_lbl.setFont(self.title_font)
        self._title_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._title_lbl.setVisible(self.standalone_mode)
        lay.addStretch()
        lay.addWidget(self._title_lbl)
        lay.addStretch()

        # Open DFF | Open TXD
        self.open_dff_btn = _tbtn("Open DFF", "Open DFF model (Ctrl+O)", self._open_dff, 'open')
        self.open_txd_btn = _tbtn("Open TXD", "Open TXD textures",       self._open_txd, 'open')
        lay.addWidget(self.open_dff_btn)
        lay.addWidget(self.open_txd_btn)

        lay.addSpacing(4)

        # ⓘ ⚙ − □ ✕  (standalone only)
        if self.standalone_mode:
            self.info_radar_btn = _ibtn("About Model Viewer", self._show_about,         'info')
            self.properties_btn = _ibtn("Theme / App Settings", self._open_app_settings,'properties')
            lay.addWidget(self.info_radar_btn)
            lay.addWidget(self.properties_btn)

            lay.addSpacing(4)
            self.minimize_btn = QPushButton(); self.minimize_btn.setFixedSize(32,28)
            self.maximize_btn = QPushButton(); self.maximize_btn.setFixedSize(32,28)
            self.close_btn    = QPushButton(); self.close_btn.setFixedSize(32,28)
            for btn, iname, fallback, tip, cb in [
                (self.minimize_btn,'minimize','—','Minimise', self.showMinimized),
                (self.maximize_btn,'maximize','□','Maximise', self._toggle_maximise),
                (self.close_btn,   'close',   '✕','Close',    self.close),
            ]:
                ico = _icon(iname)
                if ico: btn.setIcon(ico); btn.setIconSize(QSize(14,14))
                else:   btn.setText(fallback)
                btn.setToolTip(tip); btn.clicked.connect(cb)
                lay.addWidget(btn)

        return self.toolbar



    def _toggle_maximise(self): #vers 1
        if self.isMaximized(): self.showNormal()
        else:                  self.showMaximized()


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

        splitter.setSizes([200, 180, 150])
        return panel


    # - centre panel — OpenGL viewport
    def _create_centre_panel(self): #vers 2
        return self.viewport


    # - right panel — model info
    def _create_right_panel(self): #vers 2
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

        def _btn(text, tip, cb, iname=None, checkable=False, checked=False):
            b = _QTB(); b.setFont(self.panel_font)
            b.setToolTip(tip); b.setFixedHeight(26)
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

        self._paint1_btn = QPushButton("Primary")
        self._paint2_btn = QPushButton("Secondary")
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

        #  Model Info
        lbl_i = QLabel("Model Info"); lbl_i.setFont(self.panel_font)
        lay.addWidget(lbl_i)
        self._info_lbl = QLabel("—")
        self._info_lbl.setFont(self.infobar_font)
        self._info_lbl.setWordWrap(True)
        self._info_lbl.setAlignment(Qt.AlignmentFlag.AlignTop)
        lay.addWidget(self._info_lbl)

        lay.addStretch()
        return panel


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

    def _show_progress(self, visible: bool): #vers 1
        if hasattr(self, '_progress'):
            self._progress.setVisible(visible)
            if visible:
                from PyQt6.QtWidgets import QApplication
                QApplication.processEvents()


    # - settings dialog
    def _show_workshop_settings(self): #vers 1
        dlg = QDialog(self); dlg.setWindowTitle("Model Viewer Settings"); dlg.resize(400,300)
        lay = QVBoxLayout(dlg)
        tabs = QTabWidget(); lay.addWidget(tabs, 1)

        # Display tab
        disp = QWidget(); dl = QFormLayout(disp)
        bg_check = QCheckBox("Dark viewport background"); bg_check.setChecked(True)
        dl.addRow(bg_check)
        tabs.addTab(disp, "Display")

        # About tab
        about = QWidget(); al = QVBoxLayout(about)
        al.addWidget(QLabel(f"{App_name}\n{Build}\n{App_build}"))
        tabs.addTab(about, "About")

        btn_row = QHBoxLayout()
        ok = QPushButton("Close"); ok.clicked.connect(dlg.accept)
        btn_row.addStretch(); btn_row.addWidget(ok)
        lay.addLayout(btn_row)
        dlg.exec()


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

    def _show_about(self): #vers 1
        QMessageBox.about(self, App_name, f"{App_name}\n{Build} — {App_build}\n\nOpenGL DFF viewer for GTA models.")

    def _open_app_settings(self): #vers 1
        try:
            from apps.utils.app_settings_system import SettingsDialog
            if self.app_settings:
                dlg = SettingsDialog(self.app_settings, self)
                dlg.exec()
            else:
                self._show_workshop_settings()
        except Exception as e:
            self._set_status(f'Settings error: {e}')
            self._show_workshop_settings()

    # - theme / icons


    def _apply_theme(self): #vers 1
        try:
            if self.app_settings:
                self.setStyleSheet(self.app_settings.get_stylesheet())
            else:
                self.setStyleSheet(
                    "QWidget{background:#1a1d26;color:#cccccc;}"
                    "QFrame{background:#1a1d26;}"
                    "QPushButton{background:#2a2d3a;border:1px solid #444;color:#ccc;"
                    "padding:2px 6px;border-radius:3px;}"
                    "QPushButton:hover{background:#3a3d4a;}"
                    "QPushButton:checked{background:#4a6090;border-color:#6a90d0;}"
                    "QListWidget{background:#141620;border:1px solid #333;color:#ccc;}"
                    "QLabel{color:#aaa;}"
                    "QSplitter::handle{background:#333;}")
        except Exception as e:
            print(f"[ModelViewer] Theme: {e}")

    def _get_icon_color(self): #vers 1
        try:
            if self.app_settings:
                return self.app_settings.get_theme_colors().get('text_primary','#ffffff')
        except Exception: pass
        return '#ffffff'

    def _refresh_icons(self): #vers 2
        if ICONS_AVAILABLE:
            SVGIconFactory.clear_cache()
        self._apply_theme()
        # Refresh window icon with updated theme colour
        try:
            _ic = '#4a9fd4'
            if self.app_settings:
                _colors = self.app_settings.get_theme_colors()
                _ic = _colors.get('bg_panel', _colors.get('accent_primary', '#4a9fd4'))
            self.setWindowIcon(SVGIconFactory.mesh_icon(32, _ic))
        except Exception:
            pass

    def _on_menu_btn_clicked(self): #vers 1
        if hasattr(self, '_show_topbar_menu'):
            self._show_topbar_menu()
            return
        pm = QMenu(self)
        self._build_menus_into_qmenu(pm)
        btn = self.menu_toggle_btn
        pm.exec(btn.mapToGlobal(btn.rect().bottomLeft()))

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

    def _toggle_damage_mode(self, enabled: bool): #vers 1
        self._damage_mode = enabled
        if getattr(self,'_assemble_btn',None) and self._assemble_btn.isChecked():
            self._toggle_assembly_mode(True)

    def _toggle_lod_mode(self, enabled: bool): #vers 1
        self.viewport.set_show_lod(enabled)
        if getattr(self,'_assemble_btn',None) and self._assemble_btn.isChecked():
            self._toggle_assembly_mode(True)

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

    def _load_vehicle_meta(self, vehicle_name: str): #vers 1
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
            except Exception as e:
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
            self.MV_settings.set('last_dir', self._last_dir)
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
            self.MV_settings.add_recent(path); self.MV_settings.save()
            self._populate_geom_list()
            self._geom_list.setCurrentRow(0)
            self._set_status(
                f"Loaded: {os.path.basename(path)} — "
                f"{len(model.geometries)} geometries, {len(model.frames)} frames")
            # Load vehicles.ide info (wheel type) + carcols colours
            stem = os.path.splitext(os.path.basename(path))[0]
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(100, lambda s=stem: self._load_vehicle_meta(s))
            # Auto-load shared TXDs after primary TXD is loaded
            QTimer.singleShot(500, self._auto_load_shared_txds)
        except Exception as e:
            import traceback; traceback.print_exc()
            self._set_status(f"Error: {e}")
        finally:
            self._show_progress(False)

    def _strip_tex_suffix(self, name: str) -> str: #vers 1
        """Strip GTA streaming suffix e.g. buildrt4_fehihwm -> buildrt4.
        Pattern: trailing underscore + 4-8 lowercase letters only."""
        return re.sub(r'_[a-z]{4,8}$', '', name)

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

    def _auto_load_shared_txds(self): #vers 3
        """Find shared TXDs in a background thread — never blocks the UI."""
        if not self._dff_model: return
        needed  = self._collect_needed_textures()
        already = set(self.viewport._tex_ids.keys())
        missing = needed - already
        if not missing: return

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
                            if tname in miss:
                                hits.append(t)
                            elif tname in alias:
                                # TXD has base name, DFF used suffixed name
                                # Serve texture under the suffixed name the DFF expects
                                t2 = dict(t); t2['name'] = alias[tname]
                                hits.append(t2)
                                hits.append(t)  # also store base for future lookups
                        if hits:
                            collected.extend(hits)
                            for t in hits:
                                miss.discard(t['name'].lower())
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
                        ]
                        wheel_dffs = [
                            os.path.join(m, 'generic', 'wheels.DFF'),
                            os.path.join(m, 'generic', 'wheels.dff'),
                        ]
                    else:
                        # GTA3 / VC / LC: generic.txd in models/, wheels.DFF in models/Generic/
                        shared_txds = [
                            os.path.join(m, 'generic.txd'),
                            os.path.join(m, 'particle.txd'),
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

                # 1b. Current IMG (gta3.img) — look for vehicle*.txd entries
                # SA stores vehiclegeneric256 etc inside gta3.img as separate TXDs
                if miss and img and hasattr(img,'entries'):
                    self.status.emit('Scanning gta3.img for vehicle textures...')
                    txd_map={e.name.lower():e for e in img.entries if e.name.lower().endswith('.txd')}
                    # Known SA shared vehicle TXD names inside gta3.img
                    candidates=['vehiclecommon.txd','vehicle.txd','vehicles.txd',
                                 'vehiclegeneric.txd','vehiclegrunge.txd',
                                 'vehiclelights.txd','vehicletyres.txd']
                    # Also try prefix match for any vehicle*.txd
                    candidates += [n for n in txd_map if n.startswith('vehicle') and n not in candidates]
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
                    self.status.emit(f'Scanning IMG for {len(miss)} missing textures…')
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

                if collected:
                    self.found.emit(collected)

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
        self.viewport._upload_textures(new)
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
            # GL upload — defer if widget not yet shown
            def _do_upload():
                try: self.viewport._upload_textures(textures); self.viewport.update()
                except Exception as ue: self._set_status(f'GL upload: {ue}')
            if self.viewport.isVisible():
                _do_upload()
            else:
                QTimer.singleShot(400, _do_upload)
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

    def load_img(self, img): #vers 1
        """Populate IMG list with DFF entries for quick selection."""
        self._current_img = img
        self._img_list.clear()
        if not img or not hasattr(img, 'entries'):
            return
        for entry in img.entries:
            if entry.name.lower().endswith('.dff'):
                item = QListWidgetItem(entry.name)
                item.setData(Qt.ItemDataRole.UserRole, entry)
                self._img_list.addItem(item)
        count = self._img_list.count()
        self._set_status(f"IMG: {count} DFF entries loaded")

    def _on_img_entry_dclicked(self, item): #vers 2
        """Double-click IMG entry — extract and load DFF + auto TXD with progress."""
        entry = item.data(Qt.ItemDataRole.UserRole)
        img   = getattr(self, '_current_img', None)
        if not entry or not img:
            return
        from PyQt6.QtWidgets import QApplication
        from PyQt6.QtCore import Qt as _Qt
        name = entry.name
        try:
            import tempfile
            # Show busy cursor + progress bar immediately
            QApplication.setOverrideCursor(_Qt.CursorShape.WaitCursor)
            self._show_progress(True)
            self._set_status(f"Loading {name}…")
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

    def _on_geom_selected(self, row: int): #vers 2
        if not self._dff_model or row < 0 or row >= len(self._dff_model.geometries): return
        self._current_geom = row
        m = self._dff_model
        g = m.geometries[row]
        if m.frames and m.atomics:
            self.viewport.load_all_geometries(
                m.geometries, [geom.materials for geom in m.geometries],
                m.frames, m.atomics)
            if not getattr(self.viewport, '_all_geoms', None) and not self.viewport._vertices:
                self.viewport.load_geometry(g, g.materials)
        else:
            self.viewport.load_geometry(g, g.materials)
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


    # - window chrome (from RadarWorkshop)
    def _get_resize_corner(self, pos): #vers 1
        s=self.corner_size; w=self.width(); h=self.height()
        if pos.x()<s and pos.y()<s:           return "top-left"
        if pos.x()>w-s and pos.y()<s:         return "top-right"
        if pos.x()<s and pos.y()>h-s:         return "bottom-left"
        if pos.x()>w-s and pos.y()>h-s:       return "bottom-right"
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


    def mousePressEvent(self, event): #Vers 1
        if event.button() != Qt.MouseButton.LeftButton:
            return super().mousePressEvent(event)
        pos = event.pos()
        self.resize_corner = self._get_resize_corner(pos)
        if self.resize_corner:
            self.resizing = True
            self.drag_position = event.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            event.accept(); return
        if hasattr(self, 'titlebar') and self.titlebar.geometry().contains(pos):
            tb_pos = self.titlebar.mapFromParent(pos)
            if self._is_on_draggable_area(tb_pos):
                handle = self.windowHandle()
                if handle:
                    handle.startSystemMove()
                event.accept(); return
        super().mousePressEvent(event)


    def mouseMoveEvent(self, event): #Vers 1
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept(); return
        else:
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()
            self._refresh_corner_overlay()
            self._update_cursor(corner)
        super().mouseMoveEvent(event)


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

    def resizeEvent(self, event): #vers 2
        super().resizeEvent(event)
        self._refresh_corner_overlay()

    def showEvent(self, event): #vers 2
        super().showEvent(event)
        if self.standalone_mode:
            # Small delay ensures window geometry is finalised
            QTimer.singleShot(150, self._setup_corner_overlay)

    def resizeEvent(self, event): #vers 2
        super().resizeEvent(event)
        if hasattr(self,'size_grip'): self.size_grip.move(self.width()-16,self.height()-16)
        self._refresh_corner_overlay()

    def closeEvent(self, event): #Vers 2
        # Save window geometry
        if self.standalone_mode:
            g = self.geometry()
            self.MV_settings.set('window_x', g.x())
            self.MV_settings.set('window_y', g.y())
            self.MV_settings.set('window_w', g.width())
            self.MV_settings.set('window_h', g.height())
            self.MV_settings.save()
        self.window_closed.emit()
        event.accept()

    #End of Class


#  Entry point
def open_model_viewer(main_window=None, dff_path=None, txd_path=None, img=None): #vers 3
    """Open the Model Viewer docked in main_window if available, else floating."""
    # Prefer docked mode when main_window supports it
    if main_window and hasattr(main_window, "open_model_viewer_docked"):
        viewer = main_window.open_model_viewer_docked(dff_path, txd_path, img)
        if viewer:
            return viewer, viewer
    # Fallback: floating window
    viewer = ModelViewer(None, main_window)
    if dff_path: viewer.load_dff(dff_path)
    if txd_path: viewer.load_txd(txd_path)
    if img:      viewer.load_img(img)
    viewer.show()
    viewer.raise_()
    viewer.activateWindow()
    return viewer, viewer


if __name__ == '__main__':
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
    viewer, _ = open_model_viewer()
    if len(sys.argv) > 1: viewer.load_dff(sys.argv[1])
    if len(sys.argv) > 2: viewer.load_txd(sys.argv[2])
    sys.exit(app.exec())
