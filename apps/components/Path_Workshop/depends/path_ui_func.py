#this belongs in apps/components/Path_Workshop/depends/path_ui_func.py - Version: 6
# X-Seti - October 20 2026 - IMG Factory 1.6 - Path Workshop UI

"""
Path Workshop UI - titlebar, ribbons, panels, settings, menus, theme, radar view.
"""

##Methods list -
# _is_standalone

##class RadarGridWidget: -
# __init__
# _get_ui_color
# _map_size
# _ts
# keyPressEvent
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# paintEvent
# paint_paths
# screen_xy
# resizeEvent
# screen_to_world
# set_tile
# setup
# wheelEvent
# world_to_screen

##class PathUIMixin: -
# _apply_button_font
# _apply_infobar_font
# _apply_menu_bar_style
# _apply_panel_font
# _apply_theme
# _apply_title_font
# _build_menus_into_qmenu
# _build_ribbons
# _create_right_panel
# _create_status_bar
# _create_toolbar
# _dialog_dir
# _get_tool_menu_style
# _get_ui_color
# _launch_theme_settings
# _on_menu_btn_clicked
# _refresh_icons
# _remember_dir
# _ribbon_config_path
# _set_path_tool
# _show_path_display_settings
# _set_status
# _setup_hotkeys
# _show_about
# _show_dropdown_menu
# _show_info
# _show_settings_dialog
# _show_workshop_settings
# _toggle_menubar
# _update_dock_button_visibility
# get_content_margins
# get_menu_title
# get_panel_margins
# get_tab_margins
# set_menu_orientation
# setup_ui

from PyQt6.QtWidgets import QCheckBox, QComboBox, QFrame, QHBoxLayout, QLabel, QMenu, QMessageBox, QPushButton, QScrollArea, QSizePolicy, QSpinBox, QVBoxLayout, QWidget
from PyQt6.QtCore import Qt, QRect, QRectF, QSize, QTimer
from PyQt6.QtGui import QBrush, QColor, QIcon, QImage, QKeySequence, QPainter, QPen, QPixmap, QShortcut
from apps.methods.img_factory_settings import get_user_config_dir
from apps.methods.dock_panels import add_outer_dock, make_outer_mw
from pathlib import Path

import numpy as np
from apps.components.Path_Workshop.depends.path_logic_func import GAME_PRESETS

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
App_name  = "Path Workshop"
App_build = "Oct 2026"
App_auth  = "X-Seti"
Build     = "Build 2"


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


# - Radar grid widget
class RadarGridWidget(QWidget):

    def _get_ui_color(self, key): #vers 3
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)
    """Radar tiles drawn as one map image, path nodes on top."""
    MAX_ZOOM = 200.0             # x fit; big maps (SOL) need deep zoom

    def __init__(self,parent=None): #vers 4
        super().__init__(parent)
        self._cols=8; self._count=0; self._tiles={}; self._dirty=set()
        self._alpha={}; self._scaled={}      # per tile: has transparency, (size, scaled image)
        self._names=[]
        self._zoom_factor = 1.0      # manual zoom (1.0 = fit)
        self._pan_x = 0; self._pan_y = 0
        self._workshop   = None      # set by PathWorkshop after creation
        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.WheelFocus)
        self.setSizePolicy(QSizePolicy.Policy.Expanding,QSizePolicy.Policy.Expanding)

    def setup(self,cols,count,names=None): #vers 2
        self._cols=max(1,cols); self._count=count; self._tiles={}; self._dirty=set()
        self._alpha={}; self._scaled={}
        self._names=names or [f"Tile {i}" for i in range(count)]; self.update()


    def paint_paths(self, p: QPainter): #vers 6
        """Path links and nodes over the radar image (numpy culled)."""
        ws = self._workshop
        if ws is None or not len(getattr(ws, '_pn_xy', ())):
            return
        from PyQt6.QtCore import QLineF, QPointF
        from apps.components.Path_Workshop.depends.path_nodes_func import LAYERS
        sx, sy = self.screen_xy(ws._pn_xy)
        W, H = self.width(), self.height()
        on = (sx >= -20) & (sx <= W + 20) & (sy >= -20) & (sy <= H + 20)
        shown, cls, E = ws._layer_shown(), ws._pn_cls, ws._pn_edge_arr
        X, Y = sx.tolist(), sy.tolist()
        st = ws._path_style
        px = st['node_px'] + (1 if self._zoom_factor > 2 else 0) + (3 if self._zoom_factor > 20 else 0)
        for c, key in enumerate(LAYERS):
            if not shown[c]:
                continue
            col = QColor(st[key])
            if len(E):
                sel = E[(cls[E[:, 0]] == c) & (on[E[:, 0]] | on[E[:, 1]])].tolist()
                p.setPen(QPen(col, st['line_w']))
                p.drawLines([QLineF(X[a], Y[a], X[b], Y[b]) for a, b in sel])
            ids = np.nonzero((cls == c) & on)[0].tolist()
            p.setPen(QPen(col, px + 4 if key == 'rblk' else px))
            p.drawPoints([QPointF(X[i], Y[i]) for i in ids])
        for idx, col in ((ws._selected_node, QColor(255, 60, 60)), (ws._link_from, QColor(255, 230, 40))):
            if 0 <= idx < len(X):
                p.setPen(QPen(col, 2)); p.setBrush(Qt.BrushStyle.NoBrush)
                p.drawEllipse(QPointF(X[idx], Y[idx]), 6, 6)

    def screen_xy(self, xy): #vers 1
        """World (N,2) array to widget x, y arrays."""
        x0, x1, y0, y1 = self._workshop.get_world_bounds()
        mw, mh = self._map_size()
        return (self._pan_x + (xy[:, 0] - x0) / (x1 - x0) * mw,
                self._pan_y + (y1 - xy[:, 1]) / (y1 - y0) * mh)

    def _map_size(self): #vers 1
        """Whole radar map size in screen pixels."""
        ts = self._ts()
        rows = (self._count + self._cols - 1) // self._cols if self._count else self._cols
        return self._cols * ts, rows * ts

    def world_to_screen(self, wx, wy): #vers 1
        """World units to widget pixels (north up)."""
        x0, x1, y0, y1 = self._workshop.get_world_bounds()
        mw, mh = self._map_size()
        return (self._pan_x + (wx - x0) / (x1 - x0) * mw,
                self._pan_y + (y1 - wy) / (y1 - y0) * mh)

    def _ts(self): #vers 2
        """Tile size in pixels, accounting for zoom factor."""
        if not self._count or not self._cols: return 32
        rows = (self._count + self._cols - 1) // self._cols
        base = min(max(4, self.width()//self._cols), max(4, self.height()//rows))
        return max(4, int(base * self._zoom_factor))

    def set_tile(self,idx,rgba,w,h): #vers 3
        """Store tile as RGBA8888 QImage; note real transparency for the checkerboard."""
        img = QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888).copy()
        self._tiles[idx] = img
        self._alpha[idx] = bytes(rgba[3::4]).count(255) != w * h
        self._scaled.pop(idx, None)
        self.update()

    def screen_to_world(self, pos) -> tuple: #vers 2
        """Widget pixels to world units."""
        x0, x1, y0, y1 = self._workshop.get_world_bounds()
        mw, mh = self._map_size()
        return (x0 + (pos.x() - self._pan_x) / mw * (x1 - x0),
                y1 - (pos.y() - self._pan_y) / mh * (y1 - y0))

    def paintEvent(self,ev): #vers 7
        """Tiles as one image (no grid), then path nodes."""
        p=QPainter(self)
        p.fillRect(self.rect(), self._get_ui_color('viewport_bg'))
        if not self._count:
            self.paint_paths(p); p.end(); return
        ts=self._ts(); cols=self._cols
        px0, py0 = self._pan_x, self._pan_y
        cb = max(4, ts // 8)   # checkerboard cell size relative to tile size
        chk = QPixmap(cb * 2, cb * 2)
        chk.fill(self._get_ui_color('viewport_text'))
        cp = QPainter(chk)
        cp.fillRect(0, 0, cb, cb, self._get_ui_color('border'))
        cp.fillRect(cb, cb, cb, cb, self._get_ui_color('border'))
        cp.end()
        chk_brush = QBrush(chk)
        empty = self._get_ui_color('viewport_bg')
        vis = ev.rect()
        for idx in range(self._count):
            col=idx%cols; row=idx//cols; x=px0+col*ts; y=py0+row*ts
            if x > vis.right() or y > vis.bottom() or x + ts < vis.left() or y + ts < vis.top():
                continue
            if idx in self._tiles:
                img = self._tiles[idx]
                if self._alpha.get(idx):
                    p.setBrushOrigin(x, y)
                    p.fillRect(x, y, ts, ts, chk_brush)
                if ts > img.width():                  # deep zoom: draw visible part only
                    self._scaled.pop(idx, None)
                    r = QRect(x, y, ts, ts).intersected(vis)
                    k = img.width() / ts
                    p.drawImage(QRectF(r), img, QRectF((r.x() - x) * k, (r.y() - y) * k,
                                                       r.width() * k, r.height() * k))
                    continue
                cached = self._scaled.get(idx)
                if cached is None or cached[0] != ts:
                    cached = (ts, img.scaled(ts, ts, Qt.AspectRatioMode.IgnoreAspectRatio,
                                             Qt.TransformationMode.FastTransformation))
                    self._scaled[idx] = cached
                p.drawImage(x, y, cached[1])
            else:
                p.fillRect(x, y, ts, ts, empty)
        self.paint_paths(p)
        p.end()

    def mouseMoveEvent(self,ev): #vers 6
        """Drag pans or moves a node; status shows world position."""
        ws = self._workshop
        if getattr(self, '_pan_start', None) is not None:
            d = ev.pos() - self._pan_start
            self._pan_x = self._pan_start_xy[0] + d.x()
            self._pan_y = self._pan_start_xy[1] + d.y()
            self.update()
            return
        wx, wy = self.screen_to_world(ev.pos())
        if ws and ws._drag is not None:
            ws._drag_node_to(wx, wy)
        elif ws:
            ws._set_status(f"World {wx:.1f}, {wy:.1f}")

    def keyPressEvent(self, ev): #vers 2
        """Arrows pan, Home resets, Delete removes the selected node."""
        k = ev.key()
        if k == Qt.Key.Key_Delete and self._workshop:
            self._workshop._delete_selected_node(); return
        step = max(16, min(self.width(), self.height()) // 4)
        moves = {Qt.Key.Key_Left: (step, 0), Qt.Key.Key_Right: (-step, 0),
                 Qt.Key.Key_Up: (0, step), Qt.Key.Key_Down: (0, -step)}
        if k in moves:
            self._pan_x += moves[k][0]; self._pan_y += moves[k][1]
        elif k == Qt.Key.Key_Home:
            self._zoom_factor = 1.0; self._pan_x = self._pan_y = 0
        else:
            return super().keyPressEvent(ev)
        self.update()

    def resizeEvent(self, ev): #vers 1
        """Repaint at the new tile size (some compositors skip it)."""
        super().resizeEvent(ev)
        self.update()

    def wheelEvent(self, ev): #vers 2
        """Scroll wheel: zoom in/out around cursor."""
        delta = ev.angleDelta().y()
        if delta == 0: return
        factor = 1.12 if delta > 0 else (1/1.12)
        cx, cy = ev.position().x(), ev.position().y()
        old_ts = self._ts()
        self._zoom_factor = max(0.1, min(self.MAX_ZOOM, self._zoom_factor * factor))
        new_ts = self._ts()
        self._pan_x = int(cx - (cx - self._pan_x) * new_ts / max(1, old_ts))
        self._pan_y = int(cy - (cy - self._pan_y) * new_ts / max(1, old_ts))
        self.update()

    def mousePressEvent(self,ev): #vers 7
        """Middle = pan; select/drag, add, link or zoom by tool."""
        is_left = ev.button() == Qt.MouseButton.LeftButton
        ws = self._workshop
        tool = getattr(ws, '_draw_tool', 'none') if ws else 'none'
        self.setFocus()
        if ev.button() == Qt.MouseButton.MiddleButton:
            self._pan_start = ev.pos()
            self._pan_start_xy = (self._pan_x, self._pan_y)
            return
        if is_left and ws and tool in ('none', 'link'):
            hit = ws._pick_node(ev.pos().x(), ev.pos().y())
            if tool == 'link':
                ws._link_click(hit)
            elif hit >= 0:
                ws._begin_node_drag(hit)
            else:
                ws._select_node(-1)
                self._pan_start = ev.pos()               # empty space: pan
                self._pan_start_xy = (self._pan_x, self._pan_y)
            return
        if tool == 'path_node' and is_left and ws:
            ws._add_node_at(*self.screen_to_world(ev.pos()))
            return
        if tool == 'zoom' and ws:
            factor = 1.3 if is_left else (1/1.3)
            cx, cy = ev.pos().x(), ev.pos().y()
            old_ts = self._ts()
            self._zoom_factor = max(0.1, min(self.MAX_ZOOM, self._zoom_factor * factor))
            new_ts = self._ts()
            self._pan_x = cx - int((cx - self._pan_x) * new_ts / max(1, old_ts))
            self._pan_y = cy - int((cy - self._pan_y) * new_ts / max(1, old_ts))
            self.update()
            return

    def mouseReleaseEvent(self, ev): #vers 4
        """End pan or node drag."""
        self._pan_start = None
        if self._workshop is not None:
            self._workshop._end_node_drag()


class PathUIMixin: #vers 1
    """Titlebar, ribbons, panels, settings, menus, theme."""


    def _get_ui_color(self, key): #vers 2
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)

    def _build_menus_into_qmenu(self, pm): #vers 4
        fm = pm.addMenu("File")
        fm.addAction("Load Radar (IMG / TXD)…", self._open_file)
        fm.addAction("Load Radar Image…",       self._open_radar_image)
        fm.addSeparator()
        fm.addAction("Load Paths…",             self._load_paths)
        fm.addAction("Save Paths",              self._save_paths)
        fm.addAction("Undo Path Edit",          self._undo_path_edit)
        fm.addSeparator()
        if hasattr(self, 'RAD_settings'):
            recent = self.RAD_settings.get_recent()
            if recent:
                rm = fm.addMenu("Recent Files")
                for rpath in recent:
                    act = rm.addAction(Path(rpath).name)
                    act.setToolTip(rpath)
                    act.triggered.connect(lambda checked=False, p=rpath: self._open_recent(p))
                rm.addSeparator()
                rm.addAction("Clear Recent", self._clear_recent)
        vm = pm.addMenu("View")
        vm.addAction("Zoom In (+)",    lambda: self._zoom(1.25))
        vm.addAction("Zoom Out (-)",   lambda: self._zoom(0.8))
        vm.addAction("Fit Grid",       self._fit)
        vm.addSeparator()
        vm.addAction("About Path Workshop", self._show_about)

    def get_content_margins(self): #vers 1
        return (self.contmergina,self.contmerginb,self.contmerginc,self.contmergind)

    def get_panel_margins(self): #vers 1
        return (self.panelmergina,self.panelmerginb,self.panelmerginc,self.panelmergind)

    def get_tab_margins(self): #vers 1
        return (self.tabmerginsa,self.tabmerginsb,self.tabmerginsc,self.tabmerginsd)

    #    setup_ui

        # Corner resize overlay set up in showEvent

    def setup_ui(self): #vers 2
        """Titlebar / radar view with ribbons / node docks / status bar."""
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(*self.get_content_margins())
        main_layout.setSpacing(self.setspacing)

        toolbar = self._create_toolbar()
        main_layout.addWidget(toolbar)

        self._draw_btns = {}
        outer = make_outer_mw(self.ribbon_wrap(self._create_right_panel()))
        self._outer_mw = outer
        self._outer_docks = [
            add_outer_dock(self, outer, "Path Nodes", self._create_nodes_panel(), 220),
            add_outer_dock(self, outer, "Node Properties", self._create_props_panel(), 200,
                           Qt.DockWidgetArea.RightDockWidgetArea)]
        main_layout.addWidget(outer, 1)

        self._build_ribbons()
        self.ribbon_restore_state()
        QTimer.singleShot(400, self._restore_outer_layout)
        self.window_closed.connect(self._save_outer_layout)

        self._status_bar = self._create_status_bar()
        main_layout.addWidget(self._status_bar)


    # - Toolbar

    def _create_toolbar(self): #vers 2
        self.titlebar = QFrame()
        self.titlebar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.titlebar.setFixedHeight(self.titlebarheight)
        self.titlebar.setObjectName("titlebar")
        self.titlebar.installEventFilter(self)
        self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.titlebar.setMouseTracking(True)

        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setMaximumHeight(self.toolbarheight)
        self.titlebar = self.toolbar  # alias for drag detection

        # - Theme-aware icon colour
        icon_color = self._get_icon_color()

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(*self.get_panel_margins())
        layout.setSpacing(self.panelspacing)


        self.menu_toggle_btn = QPushButton("Menu")
        self.menu_toggle_btn.setFont(self.button_font)
        self.menu_toggle_btn.setToolTip("Show menu (topbar or dropdown — set in Settings)")
        self.menu_toggle_btn.setMinimumHeight(28)
        self.menu_toggle_btn.setMaximumHeight(28)
        self.menu_toggle_btn.clicked.connect(self._on_menu_btn_clicked)
        # Menu/Settings (Aug 20 2026)
        self.menu_toggle_btn.setVisible(self.standalone_mode)
        layout.addWidget(self.menu_toggle_btn)

        # - Settings button (standalone only — docked uses right-panel button)
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(SVGIconFactory.settings_icon(20, icon_color))
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip(App_name + "Workshop Settings")
        self.settings_btn.setVisible(self.standalone_mode)
        layout.addWidget(self.settings_btn)

        layout.addSpacing(8)
        layout.addStretch()

        # - App name + version label (centre-left of toolbar)
        self._title_lbl = QLabel(f"{App_name} - {Build}")
        self._title_lbl.setStyleSheet("")
        self._title_lbl.setVisible(self.standalone_mode)
        layout.addWidget(self._title_lbl)

        layout.addStretch()

        # Game / W / H selector lives in the Grid ribbon (_build_ribbons)

        # - Info button (before Theme)
        self.info_radar_btn = QPushButton()
        self.info_radar_btn.setIcon(SVGIconFactory.info_icon(20, icon_color))
        self.info_radar_btn.setIconSize(QSize(20, 20))
        self.info_radar_btn.setFixedSize(35, 35)
        self.info_radar_btn.setToolTip("About Path Workshop")
        self.info_radar_btn.clicked.connect(self._show_about)
        layout.addWidget(self.info_radar_btn)
        self.info_radar_btn.setVisible(self.standalone_mode)

        # - Theme / Properties
        self.properties_btn = QPushButton()
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(20, icon_color))
        self.properties_btn.setIconSize(QSize(20, 20))
        self.properties_btn.setFixedSize(35, 35)
        self.properties_btn.setToolTip("Theme Settings")
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        layout.addWidget(self.properties_btn)
        self.properties_btn.setVisible(self.standalone_mode)

        # - Dock button — hidden in standalone (nothing to dock to)
        self.dock_btn = QPushButton()
        try:
            self.dock_btn.setIcon(SVGIconFactory.import_icon(20, icon_color))
            self.dock_btn.setIconSize(QSize(20, 20))
        except Exception:
            self.dock_btn.setText("⊞")
        self.dock_btn.setFixedSize(35, 35)
        self.dock_btn.setToolTip("Dock into IMG Factory")
        self.dock_btn.clicked.connect(self.toggle_dock_mode)
        self.dock_btn.setVisible(not self.standalone_mode)
        layout.addWidget(self.dock_btn)

        # - Tear-off button — only when docked, use icon not bare "T"
        if not self.standalone_mode:
            self.tearoff_btn = QPushButton()
            try:
                self.tearoff_btn.setIcon(SVGIconFactory.export_icon(20, icon_color))
                self.tearoff_btn.setIconSize(QSize(20, 20))
            except Exception:
                self.tearoff_btn.setText("↗")
            self.tearoff_btn.setFixedSize(35, 35)
            self.tearoff_btn.clicked.connect(self._toggle_tearoff)
            self.tearoff_btn.setToolTip("Tear off — open as standalone window")
            layout.addWidget(self.tearoff_btn)

        # - Window controls (standalone only)
        if self.standalone_mode:
            for attr, icon_method, slot, tip in [
                ('minimize_btn', 'minimize_icon', self.showMinimized,    "Minimize"),
                ('maximize_btn', 'maximize_icon', self._toggle_maximize, "Maximize"),
                ('close_btn',    'close_icon',    self.close,            "Close"),
            ]:
                btn = QPushButton()
                btn.setIcon(getattr(SVGIconFactory, icon_method)(20, icon_color))
                btn.setIconSize(QSize(20, 20))
                btn.setMinimumWidth(40); btn.setMaximumWidth(40); btn.setMinimumHeight(30)
                btn.clicked.connect(slot)
                btn.setToolTip(tip)
                setattr(self, attr, btn)
                layout.addWidget(btn)

        return self.toolbar

    # --- Right panel: radar view
    def _ribbon_config_path(self): #vers 1
        """Dock layout file (ribbons keep their own file)."""
        return get_user_config_dir() / 'path_workshop_docks.json'

    def _create_right_panel(self): #vers 11
        """Radar map view (background for the paths)."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        hl = QHBoxLayout(panel)
        hl.setContentsMargins(0, 0, 0, 0)
        hl.setSpacing(0)
        self._radar = RadarGridWidget()
        self._radar._workshop = self
        sc = QScrollArea()
        sc.setWidget(self._radar)
        sc.setWidgetResizable(True)
        sc.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._radar_scroll = sc
        hl.addWidget(sc, 1)
        return panel

    def _create_status_bar(self): #vers 1
        bar = QFrame()
        bar.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        bar.setFixedHeight(self.statusheight)
        hl = QHBoxLayout(bar)
        hl.setContentsMargins(*self.get_tab_margins())
        self.status_label = QLabel("Ready — no IMG loaded")
        self.status_label.setFont(self.infobar_font)
        hl.addWidget(self.status_label)
        return bar


    # Settings dialog
    def _show_workshop_settings(self): #vers 4
        """Workshop-local settings — Fonts, Display, Menu tabs."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                     QTabWidget, QWidget, QGroupBox, QFormLayout,
                                     QSpinBox, QComboBox, QLabel, QFontComboBox,
                                     QCheckBox)
        from PyQt6.QtGui import QFont

        dlg = QDialog(self)
        dlg.setWindowTitle(f"{App_name} Settings")
        dlg.setMinimumWidth(520)
        dlg.setMinimumHeight(420)
        layout = QVBoxLayout(dlg)
        tabs = QTabWidget()

        #    Fonts tab                                                          
        fonts_tab = QWidget()
        fl = QVBoxLayout(fonts_tab)

        def _font_row(label, current_font):
            grp = QGroupBox(label)
            hl  = QHBoxLayout(grp)
            combo = QFontComboBox(); combo.setCurrentFont(current_font)
            spin  = QSpinBox(); spin.setRange(7, 32); spin.setValue(current_font.pointSize())
            spin.setSuffix(" pt"); spin.setFixedWidth(70)
            hl.addWidget(combo); hl.addWidget(spin)
            return grp, combo, spin

        title_grp,  title_combo,  title_sz  = _font_row("Title Font",   self.title_font)
        panel_grp,  panel_combo,  panel_sz  = _font_row("Panel Font",   self.panel_font)
        button_grp, button_combo, button_sz = _font_row("Button Font",  self.button_font)
        info_grp,   info_combo,   info_sz   = _font_row("Info Bar Font",self.infobar_font)
        for g in [title_grp, panel_grp, button_grp, info_grp]: fl.addWidget(g)
        fl.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        #    Display tab                                                        
        disp_tab = QWidget()
        dl = QVBoxLayout(disp_tab)

        btn_grp = QGroupBox("Button Display Mode")
        bl = QVBoxLayout(btn_grp)
        mode_combo = QComboBox(); mode_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])
        mode_combo.setCurrentIndex({'both':0,'icons':1,'text':2}.get(self.button_display_mode, 0))
        bl.addWidget(mode_combo)
        dl.addWidget(btn_grp)
        dl.addStretch()
        tabs.addTab(disp_tab, "Display")

        #    Menu tab                                                           
        menu_tab = QWidget()
        ml = QVBoxLayout(menu_tab)

        game_grp = QGroupBox("Default Game Preset")
        gl = QHBoxLayout(game_grp)
        game_combo_s = QComboBox(); game_combo_s.addItems(list(GAME_PRESETS))
        game_combo_s.setCurrentText(self._game_combo.currentText())
        gl.addWidget(game_combo_s)
        ml.addWidget(game_grp)

        menu_grp = QGroupBox("Menu Style")
        mgl = QVBoxLayout(menu_grp)
        menu_style_combo = QComboBox()
        menu_style_combo.addItems(["Dropdown (button)", "Topbar (embedded)"])
        menu_style_combo.setCurrentIndex(0 if self.RAD_settings.get('menu_style') == 'dropdown' else 1)
        mgl.addWidget(menu_style_combo)
        ml.addWidget(menu_grp)
        ml.addStretch()
        tabs.addTab(menu_tab, "Menu")

        layout.addWidget(tabs)

        #    Buttons                                                            
        btn_row = QHBoxLayout(); btn_row.addStretch()

        def apply_all():
            self.title_font   = QFont(title_combo.currentFont().family(),  title_sz.value())
            self.panel_font   = QFont(panel_combo.currentFont().family(),  panel_sz.value())
            self.button_font  = QFont(button_combo.currentFont().family(), button_sz.value())
            self.infobar_font = QFont(info_combo.currentFont().family(),   info_sz.value())
            self._apply_title_font()
            self._apply_panel_font()
            self._apply_button_font()
            self._apply_infobar_font()
            self.button_display_mode = {0:'both',1:'icons',2:'text'}[mode_combo.currentIndex()]
            new_game = game_combo_s.currentText()
            if new_game != self._game_combo.currentText():
                self._on_game_changed(new_game)
            style = 'dropdown' if menu_style_combo.currentIndex() == 0 else 'topbar'
            self.RAD_settings.set('menu_style', style)
            self.RAD_settings.set('show_menubar', style == 'topbar')
            self.RAD_settings.save()
            self.set_menu_orientation(style)

        apply_btn = QPushButton("Apply"); apply_btn.clicked.connect(apply_all)
        ok_btn = QPushButton("OK"); ok_btn.setDefault(True)
        ok_btn.clicked.connect(lambda: (apply_all(), dlg.accept()))
        cancel_btn = QPushButton("Cancel"); cancel_btn.clicked.connect(dlg.reject)
        for b in [cancel_btn, apply_btn, ok_btn]: btn_row.addWidget(b)
        layout.addLayout(btn_row)
        dlg.exec()

    # - Menu options
    def _apply_menu_bar_style(self): #vers 3
        """Apply font size, height and colours to the topbar menubar.
        Uses explicit colours so it stays readable regardless of app theme.
        """
        mb = getattr(self, '_menu_bar', None)
        if not mb:
            return
        bar_h  = self.RAD_settings.get('menu_bar_height', 22)
        bar_fs = self.RAD_settings.get('menu_bar_font_size', 9)
        dd_fs  = self.RAD_settings.get('menu_dropdown_font_size', 9)

        # - Get theme colours if available, otherwise use sensible defaults
        bg   = '#2b2b2b'
        fg   = '#e0e0e0'
        sel  = '#1976d2'
        selfg = '#ffffff'
        border = '#555555'
        try:
            app_settings = getattr(self, 'app_settings', None)
            if not app_settings and self.main_window:
                app_settings = getattr(self.main_window, 'app_settings', None)
            if app_settings:
                tc = app_settings.get_theme_colors() or {}
                bg    = tc.get('bg_primary',   bg)
                fg    = tc.get('text_primary',  fg)
                sel   = tc.get('accent',        sel)
                border = tc.get('border',       border)
        except Exception:
            pass

        # - Height controlled by container — NOT by stylesheet min/max-height
        # - (stylesheet height properties override Qt layout and prevent the bar showing)
        mb.setStyleSheet(f"""
            QMenuBar {{
                background-color: {bg};
                color: {fg};
                border-bottom: 1px solid {border};
                font-size: {bar_fs}pt;
            }}
            QMenuBar::item {{
                background-color: transparent;
                padding: 2px 6px;
            }}
            QMenuBar::item:selected {{
                background-color: {sel};
                color: {selfg};
            }}
            QMenu {{
                background-color: {bg};
                color: {fg};
                border: 1px solid {border};
                font-size: {dd_fs}pt;
            }}
            QMenu::item:selected {{
                background-color: {sel};
                color: {selfg};
            }}
        """)

        # - Size the container based on settings, not isVisible()
        # - (isVisible() is False during __init__ even if widget will be shown)
        c = getattr(self, '_menu_bar_container', None)
        if c:
            show = (self.RAD_settings.get('show_menubar', False) and
                    self.RAD_settings.get('menu_style', 'dropdown') == 'topbar')
            if show:
                c.setMinimumHeight(0)
                c.setMaximumHeight(bar_h)
            # Don't touch height here if hiding — setup_ui and set_menu_orientation handle that


    def set_menu_orientation(self, style: str): #vers 4
        """Switch DP5 menu between 'topbar' (internal) and 'dropdown' (host menubar).
        Called from imgfactory Settings when the orientation radio changes.
        """
        self.RAD_settings.set('menu_style', style)
        self.RAD_settings.set('show_menubar', style == 'topbar')

        # Toggle the container (not the bare QMenuBar) so Qt doesn't promote it
        container = getattr(self, '_menu_bar_container', None) or getattr(self, '_menu_bar', None)
        if container:
            if style == 'topbar':
                container.setMinimumHeight(0)
                container.setMaximumHeight(16777215)
                container.setVisible(True)
                container.updateGeometry()
                # Re-apply style so height/font are correct
                self._apply_menu_bar_style()
            else:
                container.setVisible(False)
                container.setMinimumHeight(0)
                container.setMaximumHeight(0)
                container.setFixedHeight(0)

    def get_menu_title(self) -> str: #vers 1
        """Return menu label for imgfactory menu bar."""
        return "DP5 Paint"

    def _get_tool_menu_style(self) -> str: #vers 1
        """Read menu_style from RAD_settings."""
        return self.RAD_settings.get('menu_style', 'dropdown')

    def _on_menu_btn_clicked(self): #vers 3
        style = self.RAD_settings.get('menu_style')
        if style == 'dropdown':
            self._show_dropdown_menu()
        else:
            on = not self.RAD_settings.get('show_menubar')
            self.RAD_settings.set('show_menubar', on)
            self.RAD_settings.save()
            c = getattr(self, '_menu_bar_container', self._menu_bar if hasattr(self, '_menu_bar') else None)
            if c:
                c.setMinimumHeight(0)
                c.setMaximumHeight(16777215 if on else 0)
                c.setVisible(on)

    def _show_dropdown_menu(self): #vers 2
        """Pop up the radar menus as a single QMenu dropdown."""
        menu = QMenu(self)
        self._build_menus_into_qmenu(menu)
        btn = getattr(self, 'menu_toggle_btn', None)
        if btn:
            menu.exec(btn.mapToGlobal(btn.rect().bottomLeft()))
        else:
            menu.exec(self.cursor().pos())


    def _toggle_menubar(self, on: bool): #vers 3
        self.RAD_settings.set('show_menubar', on)
        self.RAD_settings.save()
        c = getattr(self, '_menu_bar_container', self._menu_bar if hasattr(self, '_menu_bar') else None)
        if c:
            c.setMinimumHeight(0)
            c.setMaximumHeight(16777215 if on else 0)
            c.setVisible(on)


    # - Font apply helpers (template pattern)
    def _apply_title_font(self): #vers 1
        if hasattr(self, 'title_label'): self.title_label.setFont(self.title_font)


    def _apply_panel_font(self): #vers 1
        for lbl in self.findChildren(QLabel):
            if lbl.objectName() in ('panel_header',) or lbl.text() in ("Tiles","Modified: 0"):
                lbl.setFont(self.panel_font)


    def _apply_button_font(self): #vers 1
        for btn in self.findChildren(QPushButton): btn.setFont(self.button_font)


    def _apply_infobar_font(self): #vers 2
        if hasattr(self,'status_label'): self.status_label.setFont(self.infobar_font)


    # - Docking (template pattern)
    def _update_dock_button_visibility(self): #vers 1
        if hasattr(self,'dock_btn'): self.dock_btn.setVisible(not self.is_docked)
        if hasattr(self,'tearoff_btn'): self.tearoff_btn.setVisible(self.is_docked and not self.standalone_mode)

    def _setup_hotkeys(self): #vers 3
        """Workshop-local keys (no clash with IMG Factory when docked)."""
        keys = [(QKeySequence.StandardKey.Open, self._open_file),
                (QKeySequence("Ctrl+S"), self._save_paths),
                (QKeySequence("Ctrl+Z"), self._undo_path_edit),
                (QKeySequence(Qt.Key.Key_Plus), lambda: self._zoom(1.25)),
                (QKeySequence(Qt.Key.Key_Equal), lambda: self._zoom(1.25)),
                (QKeySequence(Qt.Key.Key_Minus), lambda: self._zoom(0.8)),
                (QKeySequence("Ctrl+0"), self._fit),
                (QKeySequence(Qt.Key.Key_S), lambda: self._set_path_tool('none')),
                (QKeySequence(Qt.Key.Key_Z), lambda: self._set_path_tool('zoom')),
                (QKeySequence(Qt.Key.Key_N), lambda: self._set_path_tool('path_node')),
                (QKeySequence(Qt.Key.Key_L), lambda: self._set_path_tool('link'))]
        for seq, slot in keys:
            sc = QShortcut(seq, self)
            sc.setContext(Qt.ShortcutContext.WidgetWithChildrenShortcut)
            sc.activated.connect(slot)
        if self.main_window and hasattr(self.main_window,'log_message'):
            self.main_window.log_message(f"{App_name} hotkeys ready")


    def _refresh_icons(self): #Vers 1
        SVGIconFactory.clear_cache()
        # Re-apply theme-aware icon colour to toolbar buttons
        color = self._get_icon_color()
        icon_map = [
            ('settings_btn',    'settings_icon'),
            ('new_session_btn', 'add_icon'),
            ('clear_btn',       'delete_icon'),
            ('properties_btn',  'properties_icon'),
            ('docked_settings_btn', 'settings_icon'),
        ]
        for attr, method in icon_map:
            if hasattr(self, attr):
                btn = getattr(self, attr)
                btn.setIcon(getattr(SVGIconFactory, method)(20, color))
        # Window control buttons
        for attr, method in [('minimize_btn','minimize_icon'),
                              ('maximize_btn','maximize_icon'),
                              ('close_btn','close_icon')]:
            if hasattr(self, attr):
                getattr(self, attr).setIcon(getattr(SVGIconFactory, method)(20, color))

    def _build_ribbons(self): #vers 6
        """File / View / Grid / Path ribbons."""
        B = self.ribbon_button

        tb = self.ribbon_toolbar("File")
        self.open_btn = B(tb, "open_icon",   "Load radar IMG / TXD  (Ctrl+O)", self._open_file)
        B(tb, "import_icon", "Load radar image (PNG / JPG / BMP)", self._open_radar_image)
        B(tb, "folder_icon", "Load paths: game .dat, .ipl, nodesN.dat, tracks / flight .dat", self._load_paths)
        B(tb, "save_icon",   "Save paths  (Ctrl+S)", self._save_paths)

        tb = self.ribbon_toolbar("View")
        B(tb, "zoom_in_icon",  "Zoom in  (+)",  lambda: self._zoom(1.25))
        B(tb, "zoom_out_icon", "Zoom out  (-)", lambda: self._zoom(0.8))
        B(tb, "fit_grid_icon", "Fit grid  (Ctrl+0)", self._fit)

        tb = self.ribbon_toolbar("Grid")
        self.ribbon_label(tb, "Game")
        self._game_combo = QComboBox()
        self._game_combo.addItems(list(GAME_PRESETS))
        self._game_combo.setCurrentText("SA PC")
        self._game_combo.currentTextChanged.connect(self._on_game_changed)
        self._game_combo.setFixedSize(44, 26)
        tb.addWidget(self._game_combo)
        self._cols_spin = QSpinBox()
        self._cols_spin.setRange(1, 100); self._cols_spin.setValue(8)
        self._cols_spin.setPrefix("W "); self._cols_spin.setEnabled(False)
        self._cols_spin.valueChanged.connect(self._on_custom_changed)
        self._cols_spin.setFixedSize(100, 26)
        tb.addWidget(self._cols_spin)
        self._rows_spin = QSpinBox()
        self._rows_spin.setRange(1, 100); self._rows_spin.setValue(8)
        self._rows_spin.setPrefix("H "); self._rows_spin.setEnabled(False)
        self._rows_spin.valueChanged.connect(self._on_custom_changed)
        self._rows_spin.setFixedSize(100, 26)
        tb.addWidget(self._rows_spin)

        tb = self.ribbon_toolbar("Path")
        for icon, tip, name in (('dp_select_icon', "Select / move node, drag empty space to pan (S)", 'none'),
                                ('add_icon',       "Add node to selected group (N)",  'path_node'),
                                ('get_link_icon',  "Link: click node, then its next node (L)", 'link'),
                                ('zoom_in_icon',   "Zoom tool (Z)",      'zoom')):
            self._draw_btns[name] = B(
                tb, icon, tip, lambda checked=False, t=name: self._set_path_tool(t),
                checkable=True)
        self._draw_btns['none'].setChecked(True)
        B(tb, "delete_icon", "Delete selected node (Del)", self._delete_selected_node)
        B(tb, "undo_icon",   "Undo path edit  (Ctrl+Z)", self._undo_path_edit)
        tb = self.ribbon_toolbar("Group")
        B(tb, "trash_icon",        "Delete the selected node's whole group", self._delete_group)
        B(tb, "swap_arrows_icon",  "Reverse group traffic flow", self._reverse_group)
        B(tb, "rotate_cw_icon",    "Shift / rotate all tracks", self._shift_rotate_tracks)
        B(tb, "colour_view_icon",  "Path colours and sizes", self._show_path_display_settings)
        for text, attr in (("Cars", '_show_car'), ("Peds", '_show_ped'),
                           ("Tracks / extras", '_show_extra')):
            chk = QCheckBox(text); chk.setChecked(True)
            chk.toggled.connect(lambda on, a=attr: (setattr(self, a, on), self._radar.update()))
            tb.addWidget(chk)

    def _show_path_display_settings(self): #vers 2
        """Colours, line width and node size of path layers."""
        from PyQt6.QtWidgets import QColorDialog, QDialog, QDialogButtonBox, QDoubleSpinBox, QFormLayout
        st = dict(self._path_style)
        dlg = QDialog(self)
        dlg.setWindowTitle("Path Display")
        form = QFormLayout(dlg)
        def colour_btn(key): #vers 1
            b = QPushButton(); b.setFixedSize(60, 22)
            b.setStyleSheet(f"background:{st[key]};")
            def pick(): #vers 1
                c = QColorDialog.getColor(QColor(st[key]), dlg)
                if c.isValid():
                    st[key] = c.name(); b.setStyleSheet(f"background:{st[key]};")
            b.clicked.connect(pick)
            return b
        for key, label in (('car', "Car paths"), ('ped', "Ped paths"), ('other', "Other paths"),
                           ('trk', "Tracks / flight"), ('chase', "Chase paths"), ('rblk', "Roadblocks"),
                           ('rrr', "Car recordings")):
            form.addRow(label, colour_btn(key))
        lw = QDoubleSpinBox(); lw.setRange(0.5, 10); lw.setSingleStep(0.5); lw.setValue(st['line_w'])
        ns = QSpinBox(); ns.setRange(1, 20); ns.setValue(st['node_px'])
        form.addRow("Line width", lw); form.addRow("Node size", ns)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if not dlg.exec():
            return
        st['line_w'], st['node_px'] = lw.value(), ns.value()
        self._path_style = st
        self.RAD_settings.set('path_display', st)
        self.RAD_settings.save()
        self._radar.update()

    def _dialog_dir(self): #vers 1
        """Last folder used by any Path Workshop file dialog."""
        import os
        d = self.RAD_settings.get('last_dir') or ""
        return d if os.path.isdir(d) else ""

    def _remember_dir(self, path): #vers 1
        """Store the folder of path for the next dialog."""
        import os
        self.RAD_settings.set('last_dir', os.path.dirname(os.path.abspath(path)))
        self.RAD_settings.save()

    def _set_path_tool(self, tool): #vers 2
        """Select / add / link / zoom tool on the radar view."""
        self._draw_tool = tool
        self._link_from = -1
        for name, btn in self._draw_btns.items():
            btn.setChecked(name == tool)


    # - Status
    def _set_status(self, msg): #vers 1
        if hasattr(self,'status_label'): self.status_label.setText(msg)

    # - Theme & icon helpers (template pattern)
    def _apply_theme(self): #vers 2
        try:
            if self.app_settings:
                self.setStyleSheet(self.app_settings.get_stylesheet())
                if self.standalone_mode:                  # docked: IMG Factory applies
                    from apps.utils.app_settings_system import apply_panel_effects
                    apply_panel_effects(self, self.app_settings)
            else:
                self.setStyleSheet("QWidget{background:palette(base);color:palette(mid);} QPushButton{background:palette(button);border:1px solid palette(mid);color:palette(mid);padding:2px 4px;} QPushButton:hover{background:#4a4d50;}")
        except Exception as e:
            print(f"[{App_name}] Theme error: {e}")

    # Settings and about

    def _show_about(self): #vers 4
        """Path Workshop quick start."""
        html = ("<p style='font-size:11px; color:#888;'><b>{}</b> &middot; {} &middot; {} &middot; {}</p><hr>"
                "<h3>Quick Start</h3><ol>"
                "<li><b>Load the radar</b>: Open (Ctrl+O) an IMG / TXD with radar tiles, "
                "or <b>Load Radar Image</b> for a whole-map PNG / JPG.</li>"
                "<li><b>Load Paths</b>: game .dat, a single .ipl, nodesN.dat, or tracks / flight .dat (view only).</li>"
                "<li>Select (S) and drag nodes, Add (N) joins the selected group, "
                "Link (L) sets a node's next node, Del deletes.</li>"
                "<li>Ctrl+S saves IPL / IDE path sections and SA nodes files (backups kept).</li></ol>"
                "<h3>Keys</h3><p>+ / - zoom, Ctrl+0 fit, middle-drag or arrows pan, Home resets, Ctrl+Z undo.</p>"
                ).format(App_name, Build, App_build, App_auth)
        QMessageBox.about(self, f"{App_name} - Help", html)

    def _show_info(self): #vers 1
        lines = [
            f"<b>{App_name}</b>  Build {Build}",
            f"Author: {App_auth}  —  {App_build}",
            "",
            "GTA path editor on the radar map",
            "Supports: SA · VC · VCS · LC · LCS · SOL · Custom",
            "",
            f"IMG loaded: {Path(self._img_path).name if self._img_path else 'None'}",
            f"Tiles loaded: {len(self._tile_rgba)}",
        ]
        QMessageBox.information(self, App_name + " — Info", "\n".join(lines))


    def _show_settings_dialog(self): #vers 1  (theme button → launch global theme engine)
        try:
            from apps.utils.app_settings_system import AppSettings, SettingsDialog
            if not hasattr(self, 'app_settings') or self.app_settings is None:
                self.app_settings = AppSettings()
            dlg = SettingsDialog(self.app_settings, self)
            dlg.themeChanged.connect(lambda _: self._apply_theme())
            if dlg.exec():
                self._apply_theme()
        except Exception as e:
            QMessageBox.warning(self, "Theme Error", str(e))



    def _launch_theme_settings(self): #Vers 1
        try:
            if not APPSETTINGS_AVAILABLE:
                return
            dialog = SettingsDialog(self.app_settings, self)
            dialog.themeChanged.connect(lambda _: self._apply_theme())
            if dialog.exec():
                self._apply_theme()
        except Exception as e:
            QMessageBox.warning(self, "Theme Error", str(e))
