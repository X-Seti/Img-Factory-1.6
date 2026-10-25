#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_win_func.py - Version: 4
# X-Seti - October 14 2026 - IMG Factory 1.6 - Vehicle Workshop window base

"""
Settings store, corner overlay and GUIWorkshop window base.
"""

##class WorkshopSettings: -
# __init__
# _load
# add_recent
# get
# get_recent
# save
# set

##class GUIWorkshop: -
# __init__
# _dock_to_main
# _get_resize_corner
# _get_resize_direction
# _handle_corner_resize
# _handle_resize
# _is_on_draggable_area
# _load_fonts_from_settings
# _load_game_vehicle_img
# _setup_shortcuts
# _toggle_maximize
# _undock_from_main
# _update_cursor
# closeEvent
# get_content_margins
# get_panel_margins
# mousePressEvent
# mouseReleaseEvent
# paintEvent
# resizeEvent
# showEvent
# toggle_dock_mode

import json, os
from pathlib import Path

from PyQt6.QtCore import QPoint, QTimer, Qt, pyqtSignal
from PyQt6.QtGui import (QColor, QFont, QKeySequence, QPainter, QPolygon, QShortcut)
from PyQt6.QtWidgets import QComboBox, QPushButton, QWidget

from apps.methods.corner_handles import paint_corner_handles
from apps.methods.img_factory_settings import get_user_config_dir
from apps.components.Vehicle_Workshop.depends.vehicle_common_func import (
    APPSETTINGS_AVAILABLE, AppSettings, SVGIconFactory, ToolMenuMixin, config_key)
from apps.components.Vehicle_Workshop.depends.vehicle_layout_func import _LayoutMixin
from apps.components.Vehicle_Workshop.depends.vehicle_logic_func import _LogicStubsMixin
from apps.components.Vehicle_Workshop.depends.vehicle_toolbar_func import _ToolbarMixin


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

    def __init__(self, config_key: str = "gui_workshop"): #vers 2
        cfg = get_user_config_dir()
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

    def get(self, key, default=None):  #vers 1
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
    def __init__(self, parent=None, main_window=None): #vers 2
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
        self.contmergina = 5; self.contmerginb = 5          # COL corner strip
        self.contmerginc = 5; self.contmergind = 5; self.setspacing = 2
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
        self.setWindowIcon(SVGIconFactory.vehicle_app_icon(64))
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
    def _get_resize_corner(self, pos): #vers 2
        if self.is_docked:                     # docked: no corner resize
            return None
        s = self.corner_size; x, y = pos.x(), pos.y()
        w, h = self.width(), self.height()
        if x < s and y < s:    return "top-left"
        if x > w-s and y < s:  return "top-right"
        if x < s and y > h-s:  return "bottom-left"
        if x > w-s and y > h-s: return "bottom-right"
        return None


    def _update_cursor(self, direction): #Vers 2  #vers 1
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


    def _is_on_draggable_area(self, pos): #Vers 1  #vers 1
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


    def _toggle_maximize(self): #Vers 1  #vers 1
        if self.isMaximized(): self.showNormal()
        else: self.showMaximized()


    def toggle_dock_mode(self): #vers 2
        """Tear off to a window, or return to the IMG Factory tab."""
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

    def _undock_from_main(self): #vers 1
        """Torn-off frameless window with corner handles."""
        if self.main_window is None:
            return
        size = self.size()
        self.is_docked = False
        self.setWindowFlags(Qt.WindowType.Window | Qt.WindowType.FramelessWindowHint)
        self.resize(max(800, size.width()), max(500, size.height()))
        self.show()
        self.raise_()

    def _dock_to_main(self): #vers 2
        """Return a torn-off workshop to its IMG Factory tab."""
        from apps.methods.dock_panels import redock_to_tab
        self.is_docked = True
        if not redock_to_tab(self):          # tab gone: stay a window
            self.is_docked = False
            return


    def mouseReleaseEvent(self, event): #Vers 2  #vers 1
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    def _handle_corner_resize(self, global_pos): #Vers 1  #vers 1
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


    def paintEvent(self, event): #vers 2
        """Corner handles, COL style (methods/corner_handles.py)."""
        super().paintEvent(event)
        paint_corner_handles(self)

    def showEvent(self, event): #vers 5
        """In IMG Factory: empty list loads the game's models IMG."""
        super().showEvent(event)
        if not self.standalone_mode and not self._img_list.count() and getattr(self, '_current_img', None) is None:
            self._load_game_vehicle_img()

    def _load_game_vehicle_img(self): #vers 2
        """List vehicles from <game>/models/vehicles.img (SOL) or gta3.img."""
        from apps.methods.game_file_list import game_img
        root = getattr(self.main_window, 'game_root', '') if self.main_window else ''
        models = os.path.join(root, 'models') if root else ''
        if not os.path.isdir(models):
            return
        files = {f.lower(): f for f in os.listdir(models)}
        name = files.get('vehicles.img') or files.get('gta3.img')
        if name:
            self.load_img(game_img(self, os.path.join(models, name)))

    def resizeEvent(self, event): #vers 3
        super().resizeEvent(event)
        if hasattr(self,'size_grip'): self.size_grip.move(self.width()-16,self.height()-16)

    def closeEvent(self, event): #Vers 3  #vers 1
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
