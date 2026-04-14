#!/usr/bin/env python3
# apps/components/Tmp_Template/gui_workshop.py - Version: 1
# X-Seti - Apr 2026 - IMG Factory 1.6
# GUIWorkshop — reusable base template for all workshop tools
# Inherit this class, override the stub methods, add your logic.
#
# Toolbar layout:
# [☰ Menu] [Settings] <stretch> <title> <stretch> [other btns] [Undo] [ℹ] [⚙] [_] [⬜] [✕]
#
# Usage:
#   class MyWorkshop(GUIWorkshop):
#       App_name  = "My Workshop"
#       App_build = "Build 1"
#       def _open_file(self): ...   # override stubs
#       def _build_menus_into_qmenu(self, pm): ...

import sys, json
from pathlib import Path
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QLabel, QToolButton,
    QPushButton, QFrame, QSizePolicy, QListWidget, QListWidgetItem,
    QFileDialog, QMessageBox, QTabWidget, QAbstractItemView,
    QDialog, QApplication, QDoubleSpinBox, QFormLayout, QDialogButtonBox,
    QScrollArea, QMenu, QSpinBox, QGroupBox, QComboBox, QCheckBox,
    QFontComboBox, QVBoxLayout as QVBox, QHBoxLayout as QHBox
)
from PyQt6.QtGui import (
    QColor, QPixmap, QPainter, QPen, QFont, QIcon, QKeySequence,
    QShortcut, QPolygon
)
from PyQt6.QtCore import Qt, QSize, QPoint, QRect, pyqtSignal

# ── Optional deps ─────────────────────────────────────────────────────────────
APPSETTINGS_AVAILABLE = False
try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    AppSettings = SettingsDialog = None

try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
except ImportError:
    class SVGIconFactory:
        @staticmethod
        def _s(sz=20, c=None): return QIcon()
        open_icon = save_icon = export_icon = import_icon = delete_icon = \
        undo_icon = info_icon = properties_icon = minimize_icon = maximize_icon = \
        close_icon = settings_icon = search_icon = zoom_in_icon = zoom_out_icon = \
        fit_grid_icon = locate_icon = paint_icon = fill_icon = dropper_icon = \
        line_icon = rect_icon = rect_fill_icon = scissors_icon = paste_brush_icon = \
        rotate_cw_icon = rotate_ccw_icon = flip_horz_icon = flip_vert_icon = \
        folder_icon = staticmethod(_s)

try:
    from apps.gui.tool_menu_mixin import ToolMenuMixin
except ImportError:
    class ToolMenuMixin:
        def _build_menus_into_qmenu(self, pm): pass
        def _show_popup_menu(self): pass

# App identity — override in subclass
App_name  = "Workshop"
App_build = "Build 1"
Build     = "Build 1"
__version__ = "1.0.0"
__author__  = "X-Seti"
__year__    = "2026"


# ─────────────────────────────────────────────────────────────────────────────
# Per-app JSON settings  (identical pattern to RADSettings / WATSettings)
# ─────────────────────────────────────────────────────────────────────────────

class WorkshopSettings:
    """Lightweight JSON settings for any GUIWorkshop subclass.
    Stored at ~/.config/imgfactory/{config_key}.json
    """
    MAX_RECENT = 10

    DEFAULTS = {
        # Window
        "window_x": -1, "window_y": -1, "window_w": 1400, "window_h": 800,
        # Toolbar / menu
        "show_menubar":            False,
        "menu_style":              "dropdown",   # "dropdown" | "topbar"
        "menu_bar_font_size":      9,
        "menu_bar_height":         22,
        "menu_dropdown_font_size": 9,
        # Status bar
        "show_statusbar":          True,
        # Fonts (stored as family|size pairs)
        "font_default_family":     "Arial",       "font_default_size": 10,
        "font_title_family":       "Arial",       "font_title_size":   14,
        "font_panel_family":       "Arial",       "font_panel_size":   10,
        "font_button_family":      "Arial",       "font_button_size":  10,
        "font_infobar_family":     "Courier New", "font_infobar_size":  9,
        # Display
        "button_display_mode":     "both",        # "both"|"icons"|"text"
        "sidebar_width":           82,
        # Recent files
        "recent_files": [],
    }

    def __init__(self, config_key: str = "gui_workshop"):
        cfg = Path.home() / ".config" / "imgfactory"
        cfg.mkdir(parents=True, exist_ok=True)
        self._path = cfg / f"{config_key}.json"
        self._data = dict(self.DEFAULTS)
        self._load()

    def _load(self):
        try:
            if self._path.exists():
                loaded = json.loads(self._path.read_text())
                self._data.update({k: v for k, v in loaded.items()
                                   if k in self.DEFAULTS})
        except Exception:
            pass

    def save(self):
        try:
            self._path.write_text(json.dumps(self._data, indent=2))
        except Exception:
            pass

    def get(self, key, default=None):
        return self._data.get(key,
               default if default is not None else self.DEFAULTS.get(key))

    def set(self, key, value):
        if key in self.DEFAULTS:
            self._data[key] = value

    def add_recent(self, path: str):
        r = [p for p in self._data.get("recent_files", []) if p != str(path)]
        r.insert(0, str(path))
        self._data["recent_files"] = r[:self.MAX_RECENT]
        self.save()

    def get_recent(self) -> list:
        return [p for p in self._data.get("recent_files", [])
                if Path(p).exists()]


# ─────────────────────────────────────────────────────────────────────────────
# Corner resize overlay  (shared across all workshops)
# ─────────────────────────────────────────────────────────────────────────────

class _CornerOverlay(QWidget):
    """Transparent overlay drawing accent-coloured resize triangles."""
    SIZE = 20

    def __init__(self, parent):
        super().__init__(parent)
        for attr in [Qt.WidgetAttribute.WA_TransparentForMouseEvents,
                     Qt.WidgetAttribute.WA_NoSystemBackground,
                     Qt.WidgetAttribute.WA_TranslucentBackground,
                     Qt.WidgetAttribute.WA_AlwaysStackOnTop]:
            self.setAttribute(attr, True)
        self.setWindowFlags(Qt.WindowType.Widget)
        self._hover_corner  = None
        # Grab app_settings from parent immediately so corners paint on first show
        self._app_settings  = getattr(parent, 'app_settings', None)
        self.setGeometry(0, 0, parent.width(), parent.height())
        self._update_mask()

    def _update_mask(self):
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

    def update_state(self, hover_corner, app_settings):
        self._hover_corner = hover_corner
        self._app_settings = app_settings
        self.update()

    def setGeometry(self, *a):
        super().setGeometry(*a); self._update_mask()

    def resizeEvent(self, ev):
        super().resizeEvent(ev); self._update_mask()

    def paintEvent(self, ev):
        s = self.SIZE
        try:
            accent = QColor(
                self._app_settings.get_theme_colors()
                    .get("accent_primary", "#4682FF"))
        except Exception:
            accent = QColor(70, 130, 255)
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
            col = hc if name == self._hover_corner else accent
            p.setBrush(col); p.setPen(Qt.PenStyle.NoPen)
            p.drawPolygon(QPolygon([QPoint(x, y) for x, y in pts]))
        p.end()


# ─────────────────────────────────────────────────────────────────────────────
# GUIWorkshop — base class
# ─────────────────────────────────────────────────────────────────────────────

class GUIWorkshop(ToolMenuMixin, QWidget):
    """Reusable workshop base.  Override App_name, config_key, and stub methods.

    Subclass example::

        class WaterWorkshop(GUIWorkshop):
            App_name   = "Water Workshop"
            config_key = "water_workshop"

            def _open_file(self):
                path, _ = QFileDialog.getOpenFileName(...)
                ...

            def _build_menus_into_qmenu(self, pm):
                fm = pm.addMenu("File")
                fm.addAction("Open", self._open_file)
                ...
    """

    # ── Subclass identity  (override ALL of these in your subclass) ──────────
    App_name        = "Workshop"          # shown in title bar and About
    App_build       = "Build 1"           # shown in About / status
    App_author      = "X-Seti"            # Copyright line in About
    App_year        = "2026"              # Copyright year in About
    App_description = (                   # Short description for About tab
        "GUIWorkshop base template — IMG Factory 1.6")
    config_key      = "gui_workshop"      # → ~/.config/imgfactory/{key}.json

    # ── Signals ───────────────────────────────────────────────────────────────
    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    # ── ToolMenuMixin protocol ────────────────────────────────────────────────
    def get_menu_title(self) -> str:
        return self.App_name

    def _build_menus_into_qmenu(self, pm):
        """Override in subclass to populate File/Edit/View menus."""
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
                act = rm.addAction(Path(rp).name)
                act.setToolTip(rp)
                act.triggered.connect(
                    lambda checked=False, p=rp: self._load_recent(p))
            rm.addSeparator()
            rm.addAction("Clear Recent", self._clear_recent)
        em = pm.addMenu("Edit")
        em.addAction("Undo  Ctrl+Z",   self._undo)
        em.addAction("Redo  Ctrl+Y",   self._redo)
        vm = pm.addMenu("View")
        vm.addAction("Zoom In  +",     lambda: None)
        vm.addAction("Zoom Out  -",    lambda: None)
        vm.addSeparator()
        vm.addAction("About " + self.App_name, self._show_about)

    # ── Init ──────────────────────────────────────────────────────────────────
    def __init__(self, parent=None, main_window=None):
        super().__init__(parent)
        self.main_window     = main_window
        self.standalone_mode = (main_window is None)
        self.is_docked       = not self.standalone_mode
        self.dock_widget     = None

        # ── Fonts ──────────────────────────────────────────────────────────
        self.title_font   = QFont("Arial", 14)
        self.panel_font   = QFont("Arial", 10)
        self.button_font  = QFont("Arial", 10)
        self.infobar_font = QFont("Courier New", 9)
        self.button_display_mode = "both"

        # ── Spacing / margins (same across all workshops) ──────────────────
        self.contmergina = 1; self.contmerginb = 1
        self.contmerginc = 1; self.contmergind = 1; self.setspacing = 2
        self.panelmergina = 5; self.panelmerginb = 5
        self.panelmerginc = 5; self.panelmergind = 5; self.panelspacing = 5
        self.titlebarheight = 45; self.toolbarheight = 50; self.statusheight = 22
        self.buticonsizex = 20; self.buticonsizey = 20
        self.gadiconsizex = 20; self.gadiconsizey = 20

        # ── Window chrome state ────────────────────────────────────────────
        self.dragging         = False
        self.drag_position    = None
        self.resizing         = False
        self.resize_corner    = None
        self.initial_geometry = None
        self.corner_size      = 20
        self.hover_corner     = None

        # ── AppSettings (theme) ────────────────────────────────────────────
        if main_window and hasattr(main_window, "app_settings"):
            self.app_settings = main_window.app_settings
        elif APPSETTINGS_AVAILABLE:
            try:    self.app_settings = AppSettings()
            except Exception: self.app_settings = None
        else:
            self.app_settings = None

        if self.app_settings and hasattr(self.app_settings, "theme_changed"):
            self.app_settings.theme_changed.connect(self._refresh_icons)

        # ── Per-app settings ───────────────────────────────────────────────
        self.WS = WorkshopSettings(self.config_key)
        if self.standalone_mode:
            self.resize(max(800, self.WS.get("window_w", 1400)),
                        max(500, self.WS.get("window_h",  800)))
            wx = self.WS.get("window_x", -1)
            wy = self.WS.get("window_y", -1)
            if wx >= 0 and wy >= 0:
                self.move(wx, wy)

        # ── Load saved fonts ───────────────────────────────────────────────
        self._load_fonts_from_settings()

        self.icon_factory = SVGIconFactory()
        self.setWindowTitle(self.App_name)
        self.setMinimumSize(800, 500)

        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        else:
            self.setWindowFlags(Qt.WindowType.Widget)

        if parent:
            p = parent.pos()
            self.move(p.x() + 50, p.y() + 80)

        self.setup_ui()
        self._setup_shortcuts()
        self._apply_theme()

    def _load_fonts_from_settings(self):
        """Restore fonts from per-app JSON settings."""
        ws = self.WS
        self.title_font   = QFont(ws.get("font_title_family",   "Arial"),
                                  ws.get("font_title_size",      14))
        self.panel_font   = QFont(ws.get("font_panel_family",   "Arial"),
                                  ws.get("font_panel_size",      10))
        self.button_font  = QFont(ws.get("font_button_family",  "Arial"),
                                  ws.get("font_button_size",     10))
        self.infobar_font = QFont(ws.get("font_infobar_family", "Courier New"),
                                  ws.get("font_infobar_size",    9))
        self.button_display_mode = ws.get("button_display_mode", "both")

    def get_content_margins(self):
        return (self.contmergina, self.contmerginb,
                self.contmerginc, self.contmergind)

    def get_panel_margins(self):
        return (self.panelmergina, self.panelmerginb,
                self.panelmerginc, self.panelmergind)

    # ── setup_ui ─────────────────────────────────────────────────────────────
    def setup_ui(self):
        ml = QVBoxLayout(self)
        ml.setContentsMargins(*self.get_content_margins())
        ml.setSpacing(self.setspacing)
        ml.addWidget(self._create_toolbar())

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.addWidget(self._create_left_panel())
        splitter.addWidget(self._create_centre_panel())
        splitter.addWidget(self._create_right_panel())
        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 5)
        splitter.setStretchFactor(2, 0)
        splitter.setSizes([200, 950, self.WS.get("sidebar_width", 82)])
        self._main_splitter = splitter
        ml.addWidget(splitter)

        self._status_widget = self._create_status_bar()
        ml.addWidget(self._status_widget)
        self._status_widget.setVisible(self.WS.get("show_statusbar", True))

    # ── Toolbar ───────────────────────────────────────────────────────────────
    def _create_toolbar(self):
        """Toolbar layout:
        [☰ Menu] [Settings] <stretch> <Title> <stretch> [btns...] [Undo] [ℹ] [⚙] [_] [⬜] [✕]
        """
        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setFixedHeight(self.toolbarheight)
        self.toolbar.setObjectName("titlebar")
        self.toolbar.installEventFilter(self)
        self.toolbar.setMouseTracking(True)
        self.titlebar = self.toolbar   # alias for drag detection

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(4)
        ic = self._get_icon_color()

        def _btn(icon_fn, tip, slot, text="", fixed_w=35):
            b = QPushButton()
            try:
                b.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
                b.setIconSize(QSize(20, 20))
            except Exception:
                pass
            if text and self.button_display_mode in ("both", "text"):
                b.setText(text)
            b.setFixedSize(fixed_w if not text else max(fixed_w, 35 + len(text)*6), 35)
            b.setToolTip(tip)
            b.clicked.connect(slot)
            return b

        # ── Left group: Menu + Settings ───────────────────────────────────
        self.menu_btn = QPushButton("Menu")
        self.menu_btn.setFont(self.button_font)
        self.menu_btn.setToolTip("Show menu (dropdown or top bar — set in Settings)")
        self.menu_btn.setMinimumHeight(28)
        self.menu_btn.setMaximumHeight(35)
        self.menu_btn.clicked.connect(self._on_menu_btn_clicked)
        layout.addWidget(self.menu_btn)

        self.settings_btn = _btn("settings_icon", "Workshop Settings",
                                 self._show_workshop_settings, "Settings", 90)
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        # ── Centre: title ─────────────────────────────────────────────────
        self.title_label = QLabel(self.App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.title_label.setObjectName("title_label")
        layout.addWidget(self.title_label)

        layout.addStretch()

        # ── Right group: action btns → Undo → ℹ → ⚙ → _ ⬜ ✕ ─────────────

        # Open / Save (subclass can hide/replace via _setup_action_buttons)
        self.open_btn = _btn("open_icon", "Open (Ctrl+O)", self._open_file)
        layout.addWidget(self.open_btn)

        self.save_btn = _btn("save_icon", "Save (Ctrl+S)", self._save_file)
        self.save_btn.setEnabled(False)
        layout.addWidget(self.save_btn)

        self.export_btn = _btn("export_icon", "Export", self._export_file)
        layout.addWidget(self.export_btn)

        self.import_btn = _btn("import_icon", "Import", self._import_file)
        layout.addWidget(self.import_btn)

        layout.addSpacing(8)

        # Undo
        self.undo_btn = _btn("undo_icon", "Undo (Ctrl+Z)", self._undo)
        layout.addWidget(self.undo_btn)

        layout.addSpacing(4)

        # Info [ℹ]
        self.info_btn = _btn("info_icon", "About / Info", self._show_about)
        layout.addWidget(self.info_btn)

        # Theme cog [⚙] — opens global AppSettings theme dialog
        self.properties_btn = _btn("properties_icon",
                                   "Global Theme / AppSettings",
                                   self._launch_theme_settings)
        layout.addWidget(self.properties_btn)

        layout.addSpacing(4)

        # Window controls — only in standalone mode
        if self.standalone_mode:
            self.minimize_btn = _btn("minimize_icon", "Minimise",
                                     self.showMinimized)
            layout.addWidget(self.minimize_btn)

            self.maximize_btn = _btn("maximize_icon", "Maximise / Restore",
                                     self._toggle_maximize)
            layout.addWidget(self.maximize_btn)

            self.close_btn = _btn("close_icon", "Close", self.close)
            layout.addWidget(self.close_btn)
        else:
            # Docked: Dock and Tearoff buttons
            self.dock_btn = QPushButton("D")
            self.dock_btn.setFixedSize(35, 35)
            self.dock_btn.setToolTip("Dock / Undock")
            self.dock_btn.clicked.connect(self.toggle_dock_mode)
            layout.addWidget(self.dock_btn)

        return self.toolbar

    # ── Left panel ────────────────────────────────────────────────────────────
    def _create_left_panel(self):
        """Left panel — list + header.  Override to add content."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ll = QVBoxLayout(panel)
        ll.setContentsMargins(*self.get_panel_margins())

        hdr = QLabel("Items")
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        hdr.setFont(self.panel_font)
        hdr.setStyleSheet("font-weight:bold; padding:2px;")
        ll.addWidget(hdr)

        self._item_list = QListWidget()
        self._item_list.setAlternatingRowColors(True)
        self._item_list.currentRowChanged.connect(self._on_list_selection_changed)
        ll.addWidget(self._item_list)

        # Placeholder add/remove buttons
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

    # ── Centre panel ──────────────────────────────────────────────────────────
    def _create_centre_panel(self):
        """Centre panel — tab view.  Override to add content to tabs."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel)
        cl.setContentsMargins(0, 0, 0, 0)
        cl.setSpacing(0)

        self._view_tabs = QTabWidget()
        self._view_tabs.setDocumentMode(True)
        self._view_tabs.currentChanged.connect(self._on_tab_changed)

        # Placeholder tabs — subclass replaces these
        tab1 = QWidget()
        t1l = QVBoxLayout(tab1)
        t1l.addWidget(QLabel("[ Tab 1 — override _create_centre_panel() ]",
                             alignment=Qt.AlignmentFlag.AlignCenter))
        self._view_tabs.addTab(tab1, "View A")

        tab2 = QWidget()
        t2l = QVBoxLayout(tab2)
        t2l.addWidget(QLabel("[ Tab 2 — override _create_centre_panel() ]",
                             alignment=Qt.AlignmentFlag.AlignCenter))
        self._view_tabs.addTab(tab2, "View B")

        cl.addWidget(self._view_tabs)
        return panel

    # ── Right sidebar ─────────────────────────────────────────────────────────
    def _create_right_panel(self):
        """Right sidebar — 2-col 36px icon grid.  Override _populate_sidebar()."""
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

    def _populate_sidebar(self):
        """Build the sidebar icon grid.  Override to change tools."""
        sl = self._sidebar_layout
        ic = self._get_icon_color()
        BTN = 36

        def _nb(icon_fn, tip, slot, checkable=False):
            b = QToolButton()
            b.setFixedSize(BTN, BTN)
            try:
                b.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
            except Exception:
                b.setText(tip[:2])
            b.setToolTip(tip)
            b.setCheckable(checkable)
            b.clicked.connect(slot)
            return b

        def _row(*btns):
            row = QHBoxLayout()
            row.setSpacing(2)
            row.setContentsMargins(0, 0, 0, 0)
            for b in btns:
                row.addWidget(b)
            if len(btns) == 1:
                row.addStretch()
            sl.addLayout(row)

        def _sep():
            s = QFrame()
            s.setFrameShape(QFrame.Shape.HLine)
            sl.addSpacing(2)
            sl.addWidget(s)
            sl.addSpacing(2)

        def _tool(icon_fn, tip, tool_name):
            b = _nb(icon_fn, tip,
                    lambda checked=False, t=tool_name: self._set_active_tool(t),
                    checkable=True)
            self._draw_btns[tool_name] = b
            return b

        # ── Row 1: Zoom in / Zoom out ─────────────────────────────────────
        _row(_nb("zoom_in_icon",  "Zoom in (+)",   lambda: self._zoom(1.25)),
             _nb("zoom_out_icon", "Zoom out (-)",  lambda: self._zoom(0.8)))

        # ── Row 2: Fit / Jump ─────────────────────────────────────────────
        _row(_nb("fit_grid_icon", "Fit  Ctrl+0",       self._fit),
             _nb("locate_icon",   "Jump to selection", self._jump))

        _sep()

        # ── Draw tool rows (2 per row) ────────────────────────────────────
        _row(_tool("paint_icon",    "Pencil (P)",           "pencil"),
             _tool("fill_icon",     "Flood fill (F)",       "fill"))
        _row(_tool("line_icon",     "Line (L)",             "line"),
             _tool("rect_icon",     "Rect outline (R)",     "rect"))
        _row(_tool("rect_fill_icon","Filled rect (Shift+R)","rect_fill"),
             _tool("dropper_icon",  "Colour picker (K)",    "picker"))
        _row(_tool("scissors_icon", "Cut (X)",              "cut"),
             _tool("paste_brush_icon","Paste (V)",          "paste"))
        _row(_tool("zoom_in_icon",  "Zoom tool (Z)",        "zoom"),
             _nb("search_icon",     "Open in editor tab",
                 lambda: self._on_toolbar_action("edit")))

        _sep()

        # ── Transform row ─────────────────────────────────────────────────
        _row(_nb("rotate_cw_icon",  "Rotate +90°", lambda: self._on_toolbar_action("rotate_cw")),
             _nb("rotate_ccw_icon", "Rotate -90°", lambda: self._on_toolbar_action("rotate_ccw")))
        _row(_nb("flip_horz_icon",  "Flip H",      lambda: self._on_toolbar_action("flip_h")),
             _nb("flip_vert_icon",  "Flip V",       lambda: self._on_toolbar_action("flip_v")))

        # Activate default tool
        if "pencil" in self._draw_btns:
            self._draw_btns["pencil"].setChecked(True)
            self._active_tool = "pencil"

    # ── Status bar ────────────────────────────────────────────────────────────
    def _create_status_bar(self):
        self._status_bar = QLabel(f"Ready  |  {self.App_name}  {self.App_build}")
        self._status_bar.setFixedHeight(self.statusheight)
        self._status_bar.setFont(self.infobar_font)
        self._status_bar.setStyleSheet("padding:2px 6px;")
        return self._status_bar

    def _set_status(self, msg: str):
        if hasattr(self, "_status_bar"):
            self._status_bar.setText(msg)

    # ── Shortcuts ─────────────────────────────────────────────────────────────
    def _setup_shortcuts(self):
        for key, fn in [
            ("Ctrl+O", self._open_file),
            ("Ctrl+S", self._save_file),
            ("Ctrl+Z", self._undo),
            ("Ctrl+Y", self._redo),
            ("Ctrl+Shift+Z", self._redo),
            ("Ctrl+0", self._fit),
            ("Ctrl+C", self._copy_item),
            ("Ctrl+V", self._paste_item),
        ]:
            QShortcut(QKeySequence(key), self).activated.connect(fn)
        for key, tool in [
            ("P", "pencil"), ("F", "fill"), ("L", "line"),
            ("R", "rect"),   ("K", "picker"), ("Z", "zoom"),
            ("X", "cut"),    ("V", "paste"),
        ]:
            QShortcut(QKeySequence(key), self).activated.connect(
                lambda t=tool: self._set_active_tool(t))
        QShortcut(QKeySequence("Shift+R"), self).activated.connect(
            lambda: self._set_active_tool("rect_fill"))

    # ── Theme / icons ─────────────────────────────────────────────────────────
    def _get_icon_color(self) -> str:
        """Returns text_primary from current theme, or guesses from palette."""
        if APPSETTINGS_AVAILABLE and self.app_settings:
            try:
                colors = self.app_settings.get_theme_colors()
                return colors.get("text_primary", "#e0e0e0")
            except Exception:
                pass
        bg = self.palette().window().color()
        return "#e0e0e0" if bg.lightness() < 128 else "#202020"

    def _get_accent_color(self) -> str:
        """Returns accent_primary from current theme."""
        if APPSETTINGS_AVAILABLE and self.app_settings:
            try:
                return self.app_settings.get_theme_colors().get(
                    "accent_primary", "#4682FF")
            except Exception:
                pass
        return "#4682FF"

    def _apply_theme(self):
        if self.app_settings:
            try:
                qss = self.app_settings.get_stylesheet()
                if qss:
                    self.setStyleSheet(qss)
            except Exception:
                pass

    def _refresh_icons(self):
        """Called on theme change — re-apply theme and update icon colours."""
        self._apply_theme()
        # Update corner overlay accent colour
        if hasattr(self, '_corner_overlay'):
            self._corner_overlay.update_state(self.hover_corner, self.app_settings)
        # Regenerate toolbar button icons with new colour
        ic = self._get_icon_color()
        icon_map = {
            'open_btn':       'open_icon',
            'save_btn':       'save_icon',
            'export_btn':     'export_icon',
            'import_btn':     'import_icon',
            'undo_btn':       'undo_icon',
            'info_btn':       'info_icon',
            'properties_btn': 'properties_icon',
            'settings_btn':   'settings_icon',
        }
        if self.standalone_mode:
            icon_map.update({'minimize_btn': 'minimize_icon',
                             'maximize_btn': 'maximize_icon',
                             'close_btn':    'close_icon'})
        for btn_name, icon_fn in icon_map.items():
            btn = getattr(self, btn_name, None)
            if btn:
                try:
                    btn.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
                except Exception:
                    pass

    def _launch_theme_settings(self): #vers 1
        """Open the global AppSettings SettingsDialog — theme cog [⚙]."""
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

    # ── Workshop settings dialog ───────────────────────────────────────────────
    def _show_workshop_settings(self):
        """4-tab settings: Fonts / Display / Menu / About."""
        from PyQt6.QtWidgets import (
            QDialog, QVBoxLayout, QHBoxLayout, QTabWidget, QWidget,
            QGroupBox, QFormLayout, QSpinBox, QComboBox, QLabel,
            QCheckBox, QFontComboBox, QDialogButtonBox, QTextEdit
        )

        dlg = QDialog(self)
        dlg.setWindowTitle(f"{self.App_name} — Settings")
        dlg.setMinimumSize(560, 480)
        try:
            from apps.core.theme_utils import apply_dialog_theme
            apply_dialog_theme(dlg, self.main_window)
        except Exception:
            pass

        main_layout = QVBoxLayout(dlg)
        tabs = QTabWidget()

        ws = self.WS   # shorthand

        # ── TAB 1: FONTS ──────────────────────────────────────────────────
        fonts_tab = QWidget()
        fl = QVBoxLayout(fonts_tab)

        def _font_row(group_name, fam_key, sz_key, default_fam, default_sz,
                      sz_min=8, sz_max=32):
            grp = QGroupBox(group_name)
            row = QHBoxLayout(grp)
            fc = QFontComboBox()
            fc.setCurrentFont(QFont(ws.get(fam_key, default_fam)))
            row.addWidget(fc)
            sc = QSpinBox()
            sc.setRange(sz_min, sz_max)
            sc.setValue(ws.get(sz_key, default_sz))
            sc.setSuffix(" pt")
            sc.setFixedWidth(80)
            row.addWidget(sc)
            fl.addWidget(grp)
            return fc, sc

        f_title_c,  f_title_s  = _font_row("Title Font",
            "font_title_family",   "font_title_size",   "Arial",       14, 10, 32)
        f_panel_c,  f_panel_s  = _font_row("Panel / Header Font",
            "font_panel_family",   "font_panel_size",   "Arial",       10)
        f_button_c, f_button_s = _font_row("Button Font",
            "font_button_family",  "font_button_size",  "Arial",       10)
        f_info_c,   f_info_s   = _font_row("Info Bar Font",
            "font_infobar_family", "font_infobar_size", "Courier New",  9)
        fl.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        # ── TAB 2: DISPLAY ────────────────────────────────────────────────
        disp_tab = QWidget()
        dl = QVBoxLayout(disp_tab)

        btn_grp = QGroupBox("Button Display Mode")
        bg = QVBoxLayout(btn_grp)
        btn_mode = QComboBox()
        btn_mode.addItems(["Icons + Text", "Icons Only", "Text Only"])
        mode_idx = {"both": 0, "icons": 1, "text": 2}
        btn_mode.setCurrentIndex(
            mode_idx.get(ws.get("button_display_mode", "both"), 0))
        bg.addWidget(btn_mode)
        bg.addWidget(QLabel("Changes how toolbar buttons display on restart.",
                            styleSheet="color:#888; font-style:italic;"))
        dl.addWidget(btn_grp)

        sb_grp = QGroupBox("Status Bar")
        sbg = QVBoxLayout(sb_grp)
        show_sb = QCheckBox("Show status bar")
        show_sb.setChecked(bool(ws.get("show_statusbar", True)))
        sbg.addWidget(show_sb)
        dl.addWidget(sb_grp)

        sw_grp = QGroupBox("Sidebar")
        swg = QFormLayout(sw_grp)
        sw_spin = QSpinBox()
        sw_spin.setRange(60, 200)
        sw_spin.setValue(ws.get("sidebar_width", 82))
        sw_spin.setSuffix(" px")
        swg.addRow("Sidebar width:", sw_spin)
        dl.addWidget(sw_grp)

        dl.addStretch()
        tabs.addTab(disp_tab, "Display")

        # ── TAB 3: MENU ───────────────────────────────────────────────────
        menu_tab = QWidget()
        ml2 = QVBoxLayout(menu_tab)

        m_grp = QGroupBox("Menu Style")
        mg = QVBoxLayout(m_grp)
        m_style = QComboBox()
        m_style.addItems(["Dropdown  ☰  (default)", "Top menu bar"])
        m_style.setCurrentIndex(
            0 if ws.get("menu_style", "dropdown") == "dropdown" else 1)
        mg.addWidget(m_style)
        mg.addWidget(QLabel("Restart required to change menu style.",
                            styleSheet="color:#888; font-style:italic;"))
        ml2.addWidget(m_grp)

        mf_grp = QGroupBox("Menu Font Size")
        mf = QFormLayout(mf_grp)
        m_font_size = QSpinBox()
        m_font_size.setRange(7, 16)
        m_font_size.setValue(ws.get("menu_dropdown_font_size", 9))
        m_font_size.setSuffix(" pt")
        mf.addRow("Dropdown font size:", m_font_size)
        mb_height = QSpinBox()
        mb_height.setRange(18, 40)
        mb_height.setValue(ws.get("menu_bar_height", 22))
        mb_height.setSuffix(" px")
        mf.addRow("Menu bar height:", mb_height)
        ml2.addWidget(mf_grp)

        ml2.addStretch()
        tabs.addTab(menu_tab, "Menu")

        # ── TAB 4: ABOUT ─────────────────────────────────────────────────
        about_tab = QWidget()
        al = QVBoxLayout(about_tab)
        about_text = QTextEdit()
        about_text.setReadOnly(True)
        # Read author/year from subclass attrs if set, else fall back to module
        author = getattr(self, 'App_author', __author__)
        year   = getattr(self, 'App_year',   __year__)
        extra  = getattr(self, 'App_description', '')
        about_text.setHtml(
            f"<h2>{self.App_name}</h2>"
            f"<p><b>Build:</b> {self.App_build}</p>"
            f"<p><b>Template:</b> GUIWorkshop v1 — IMG Factory 1.6</p>"
            + (f"<p>{extra}</p>" if extra else "") +
            f"<hr>"
            f"<p>Copyright &copy; {year} <b>{author}</b></p>"
            f"<p>Part of <b>IMG Factory 1.6</b> — a GTA modding toolkit.</p>"
            f"<p style='color:#888;'>This software is provided as-is for GTA modding "
            f"purposes.<br>Not affiliated with Rockstar Games or Take-Two Interactive.</p>"
        )
        al.addWidget(about_text)
        tabs.addTab(about_tab, "About")

        # ── Buttons ───────────────────────────────────────────────────────
        main_layout.addWidget(tabs)
        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok |
            QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        main_layout.addWidget(btns)

        if dlg.exec() != QDialog.DialogCode.Accepted:
            return

        # ── Apply ─────────────────────────────────────────────────────────
        ws.set("font_title_family",   f_title_c.currentFont().family())
        ws.set("font_title_size",     f_title_s.value())
        ws.set("font_panel_family",   f_panel_c.currentFont().family())
        ws.set("font_panel_size",     f_panel_s.value())
        ws.set("font_button_family",  f_button_c.currentFont().family())
        ws.set("font_button_size",    f_button_s.value())
        ws.set("font_infobar_family", f_info_c.currentFont().family())
        ws.set("font_infobar_size",   f_info_s.value())
        ws.set("button_display_mode", ["both","icons","text"][btn_mode.currentIndex()])
        ws.set("show_statusbar",      show_sb.isChecked())
        ws.set("sidebar_width",       sw_spin.value())
        ws.set("menu_style",          "dropdown" if m_style.currentIndex()==0 else "topbar")
        ws.set("menu_dropdown_font_size", m_font_size.value())
        ws.set("menu_bar_height",     mb_height.value())
        ws.save()

        # Live-apply what we can without restart
        self._load_fonts_from_settings()
        if hasattr(self, "title_label"):
            self.title_label.setFont(self.title_font)
        self._status_widget.setVisible(ws.get("show_statusbar", True))
        self._sidebar_frame.setFixedWidth(ws.get("sidebar_width", 82))
        self._set_status("Settings saved.")

    # ── Menu popup ────────────────────────────────────────────────────────────
    def _on_menu_btn_clicked(self): #vers 1
        """Menu button clicked — show dropdown or toggle top bar per settings."""
        style = self.WS.get("menu_style", "dropdown")
        if style == "dropdown":
            self._show_dropdown_menu()
        else:
            # Toggle top menubar visibility
            on = not self.WS.get("show_menubar", False)
            self.WS.set("show_menubar", on)
            self.WS.save()
            if hasattr(self, "_menu_bar_container"):
                self._menu_bar_container.setVisible(on)

    def _show_dropdown_menu(self): #vers 1
        """Pop up the workshop menus as a single QMenu dropdown."""
        menu = QMenu(self)
        self._build_menus_into_qmenu(menu)
        btn = getattr(self, "menu_btn", None)
        if btn:
            menu.exec(btn.mapToGlobal(btn.rect().bottomLeft()))
        else:
            menu.exec(self.cursor().pos())

    def _show_popup_menu(self):
        """Alias for _show_dropdown_menu — kept for compatibility."""
        self._show_dropdown_menu()

    # ── Window chrome ─────────────────────────────────────────────────────────
    def showEvent(self, ev):
        super().showEvent(ev)
        if not hasattr(self, "_corner_overlay"):
            self._corner_overlay = _CornerOverlay(self)
            # Immediately give it the accent colour source
            self._corner_overlay.update_state(None, self.app_settings)
        self._corner_overlay.setGeometry(0, 0, self.width(), self.height())
        self._corner_overlay.raise_()
        self._corner_overlay.show()

    def resizeEvent(self, ev):
        super().resizeEvent(ev)
        if hasattr(self, "_corner_overlay"):
            self._corner_overlay.setGeometry(0, 0, self.width(), self.height())

    def _get_resize_corner(self, pos):
        s = self.corner_size; x, y = pos.x(), pos.y()
        w, h = self.width(), self.height()
        if x < s and y < s:     return "top-left"
        if x > w-s and y < s:   return "top-right"
        if x < s and y > h-s:   return "bottom-left"
        if x > w-s and y > h-s: return "bottom-right"
        return None

    def _update_cursor(self, corner):
        c = {
            "top-left":     Qt.CursorShape.SizeFDiagCursor,
            "top-right":    Qt.CursorShape.SizeBDiagCursor,
            "bottom-left":  Qt.CursorShape.SizeBDiagCursor,
            "bottom-right": Qt.CursorShape.SizeFDiagCursor,
        }
        self.setCursor(c.get(corner, Qt.CursorShape.ArrowCursor))

    def _handle_corner_resize(self, global_pos):
        if not self.resize_corner or not self.drag_position: return
        delta = global_pos - self.drag_position
        g = self.initial_geometry
        dx, dy = delta.x(), delta.y()
        if "right"  in self.resize_corner: self.resize(max(800, g.width()+dx), self.height())
        if "bottom" in self.resize_corner: self.resize(self.width(), max(500, g.height()+dy))
        if "left"   in self.resize_corner:
            self.setGeometry(g.x()+dx, g.y(), max(800, g.width()-dx), g.height())
        if "top"    in self.resize_corner:
            self.setGeometry(g.x(), g.y()+dy, g.width(), max(500, g.height()-dy))

    def mousePressEvent(self, ev):
        if ev.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(ev); return
        pos = ev.pos()
        self.resize_corner = self._get_resize_corner(pos)
        if self.resize_corner:
            self.resizing = True
            self.drag_position = ev.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            ev.accept(); return
        if (hasattr(self, "titlebar") and
                self.titlebar.geometry().contains(pos)):
            handle = self.windowHandle()
            if handle:
                handle.startSystemMove()
            ev.accept(); return
        super().mousePressEvent(ev)

    def mouseMoveEvent(self, ev):
        if ev.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(ev.globalPosition().toPoint())
                ev.accept(); return
        else:
            corner = self._get_resize_corner(ev.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                if hasattr(self, "_corner_overlay"):
                    self._corner_overlay.update_state(corner, self.app_settings)
            self._update_cursor(corner)
        super().mouseMoveEvent(ev)

    def mouseReleaseEvent(self, ev):
        self.dragging = False; self.resizing = False
        self.resize_corner = None
        self.setCursor(Qt.CursorShape.ArrowCursor)
        ev.accept()

    def _toggle_maximize(self):
        if self.isMaximized(): self.showNormal()
        else: self.showMaximized()

    def toggle_dock_mode(self):
        """Stub — override in subclass if dock support needed."""
        pass

    def closeEvent(self, ev):
        if self.standalone_mode:
            g = self.geometry()
            self.WS.set("window_x", g.x()); self.WS.set("window_y", g.y())
            self.WS.set("window_w", g.width()); self.WS.set("window_h", g.height())
            self.WS.save()
        self.workshop_closed.emit()
        self.window_closed.emit()
        super().closeEvent(ev)

    # ── Tool management ───────────────────────────────────────────────────────
    def _set_active_tool(self, tool: str):
        self._active_tool = tool
        for name, btn in self._draw_btns.items():
            btn.setChecked(name == tool)

    # ── Stub methods — override in subclass ───────────────────────────────────
    def _open_file(self, path=None):
        """Open a file.  Override in subclass."""
        pass

    def _save_file(self):
        """Save current file.  Override in subclass."""
        pass

    def _export_file(self):
        """Export.  Override in subclass."""
        pass

    def _import_file(self):
        """Import.  Override in subclass."""
        pass

    def _undo(self):
        """Undo last action.  Override in subclass."""
        self._set_status("Undo — not implemented in base class")

    def _redo(self):
        """Redo last undone action.  Override in subclass."""
        self._set_status("Redo — not implemented in base class")

    def _copy_item(self):
        """Copy selection.  Override in subclass."""
        pass

    def _paste_item(self):
        """Paste clipboard.  Override in subclass."""
        pass

    def _load_recent(self, path: str):
        """Load a recent file.  Override in subclass."""
        self._open_file(path)

    def _clear_recent(self):
        self.WS._data["recent_files"] = []
        self.WS.save()
        self._set_status("Recent files cleared")

    def _zoom(self, factor: float):
        """Zoom the main view.  Override in subclass."""
        pass

    def _fit(self):
        """Fit view to content.  Override in subclass."""
        pass

    def _jump(self):
        """Jump to selection.  Override in subclass."""
        pass

    def _on_list_selection_changed(self, row: int):
        """Called when left panel list selection changes.  Override."""
        pass

    def _on_tab_changed(self, idx: int):
        """Called when centre tab changes.  Override."""
        pass

    def _on_add_item(self):
        """Add button in left panel.  Override."""
        pass

    def _on_remove_item(self):
        """Remove button in left panel.  Override."""
        pass

    def _on_toolbar_action(self, action: str):
        """Generic toolbar action handler (rotate, flip, edit, etc).  Override."""
        pass

    def _show_about(self):
        """Show about/info dialog."""
        author = getattr(self, 'App_author', __author__)
        year   = getattr(self, 'App_year',   __year__)
        extra  = getattr(self, 'App_description', 'GUIWorkshop template — IMG Factory 1.6')
        QMessageBox.information(self, f"About {self.App_name}",
            f"{self.App_name}  {self.App_build}\n\n"
            f"{extra}\n\n"
            f"Copyright \u00a9 {year} {author}\n"
            f"Part of IMG Factory 1.6 — a GTA modding toolkit.")


# ─────────────────────────────────────────────────────────────────────────────
# Standalone launcher
# ─────────────────────────────────────────────────────────────────────────────

if __name__ == "__main__":
    import traceback
    print(f"GUIWorkshop template — standalone demo")
    try:
        app = QApplication(sys.argv)
        w = GUIWorkshop()
        w.App_name = "GUIWorkshop Demo"
        w.setWindowTitle("GUIWorkshop — Template Demo")
        w.resize(1300, 800)
        w.show()
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)
