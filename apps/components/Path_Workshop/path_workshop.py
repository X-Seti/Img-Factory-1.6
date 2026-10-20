#!/usr/bin/env python3
#this belongs in apps/components/Path_Workshop/path_workshop.py - Version: 12
# X-Seti - Apr 2026 - IMG Factory 1.6 - Path Workshop

##Methods list -
# open_path_workshop

##class RADSettings: -
# __init__
# _load
# add_recent
# get
# get_recent
# save
# set

##class PathWorkshop: -
# __init__

# Mixins: PathLogicMixin, PathUIMixin, PathWindowMixin, PathNodesMixin - depends/


import os
import sys

if sys.platform.startswith('linux'):   # xcb only exists on Linux
    os.environ['QT_QPA_PLATFORM'] = 'xcb'
    os.environ['QSG_RHI_BACKEND'] = 'opengl'

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))


import json
from PyQt6.QtWidgets import QApplication, QMessageBox, QWidget
from PyQt6.QtCore import pyqtSignal, Qt
from PyQt6.QtGui import QFont
from apps.methods.img_factory_settings import get_user_config_dir
from apps.methods.dock_panels import DockPanelsMixin
from apps.methods.ribbon_system import RibbonMixin
from typing import Dict, List, Optional
from apps.components.Path_Workshop.depends.path_logic_func import GAME_PRESETS, ImgReader, TILE_H, TILE_W, PathLogicMixin
from apps.components.Path_Workshop.depends.path_ui_func import APPSETTINGS_AVAILABLE, AppSettings, App_name, SVGIconFactory, ToolMenuMixin, PathUIMixin
from apps.components.Path_Workshop.depends.path_win_func import PathWindowMixin
from apps.components.Path_Workshop.depends.path_nodes_func import PathNodesMixin


#    Per-tool settings                                                          
class RADSettings:
    """Lightweight JSON settings for Path Workshop.
    Stored at ~/.config/imgfactory/path_workshop.json
    Completely separate from the global AppSettings/theme system.
    """
    MAX_RECENT = 10

    DEFAULTS = {
        'show_menubar':            False,
        'menu_style':              'dropdown',
        'menu_bar_font_size':      9,
        'menu_bar_height':         22,
        'menu_dropdown_font_size': 9,
        'show_statusbar':          True,
        'default_game':            'SA PC',
        'recent_files':            [],        # list of recently opened IMG paths
        'path_display':            {},        # path colours / line width / node size
        'last_dir':                '',        # last folder of any file dialog
        'window_x':                -1,
        'window_y':                -1,
        'window_w':                1400,
        'window_h':                800,
    }

    def __init__(self): #vers 2
        cfg_dir = get_user_config_dir()
        cfg_dir.mkdir(parents=True, exist_ok=True)
        self._path = cfg_dir / 'path_workshop.json'
        self._data = dict(self.DEFAULTS)
        self._load()

    def _load(self): #vers 1
        try:
            if self._path.exists():
                loaded = json.loads(self._path.read_text())
                self._data.update({k: v for k, v in loaded.items()
                                   if k in self.DEFAULTS})
        except Exception:
            pass

    def save(self): #vers 1
        try:
            self._path.write_text(json.dumps(self._data, indent=2))
        except Exception:
            pass

    def get(self, key, default=None): #vers 1
        return self._data.get(key, default if default is not None
                              else self.DEFAULTS.get(key))

    def set(self, key, value): #vers 1
        if key in self.DEFAULTS:
            self._data[key] = value

    def add_recent(self, path: str): #vers 1
        """Add path to recent files list (max MAX_RECENT entries)."""
        recents = [p for p in self._data.get('recent_files', []) if p != path]
        recents.insert(0, path)
        self._data['recent_files'] = recents[:self.MAX_RECENT]
        self.save()

    def get_recent(self): #vers 1
        """Return list of recent file paths that still exist."""
        import os
        return [p for p in self._data.get('recent_files', []) if os.path.isfile(p)]


class PathWorkshop(RibbonMixin, PathNodesMixin, PathLogicMixin, PathUIMixin, PathWindowMixin,
                   DockPanelsMixin, ToolMenuMixin, QWidget): #vers 4
    _ribbon_name = "path_workshop"
    # Bump when the set of ribbons changes (5 = Group ribbon)
    _RIBBON_LAYOUT_VERSION = 5
    _OUTER_LAYOUT_VERSION = 1      # Path Nodes / Node Properties docks

    """Path Workshop: GTA paths drawn over the radar map"""

    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    def __init__(self, parent=None, main_window=None): #vers 3
        super().__init__(parent)

        self.main_window        = main_window
        self.standalone_mode    = (main_window is None)
        self.is_docked          = not self.standalone_mode
        self.button_display_mode = 'both'
        self.dock_display_mode   = None

        # Fonts (mirrors COL Workshop)
        self.title_font   = QFont("Arial", 14)
        self.panel_font   = QFont("Arial", 10)
        self.button_font  = QFont("Arial", 10)
        self.chat_font    = QFont("Courier New", 10)
        self.button_display_mode = 'both'
        self.infobar_font = QFont("Courier New", 9)
        self.section_title_font = QFont("Arial", 11, QFont.Weight.Bold)

        # Window chrome
        self.use_system_titlebar  = False
        self.window_always_on_top = False
        self.dragging             = False
        self.drag_position        = None
        self.resizing             = False
        self.resize_corner        = None
        self.corner_size          = 20
        self.hover_corner         = None

        # AppSettings
        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        elif APPSETTINGS_AVAILABLE:
            try:
                self.app_settings = AppSettings()
            except Exception:
                self.app_settings = None
        else:
            self.app_settings = None

        if self.app_settings and hasattr(self.app_settings, 'theme_changed'):
            self.app_settings.theme_changed.connect(self._refresh_icons)

        # Per-tool settings (separate from global theme system)
        self.RAD_settings = RADSettings()
        # Restore window geometry
        if self.standalone_mode:
            wx = self.RAD_settings.get('window_x', -1)
            wy = self.RAD_settings.get('window_y', -1)
            ww = self.RAD_settings.get('window_w', 1400)
            wh = self.RAD_settings.get('window_h', 800)
            self.resize(max(800, ww), max(500, wh))
            if wx >= 0 and wy >= 0:
                self.move(wx, wy)

        # Spacing/margins (template pattern)
        self.contmergina=1; self.contmerginb=1; self.contmerginc=1; self.contmergind=1; self.setspacing=2
        self.panelmergina=5; self.panelmerginb=5; self.panelmerginc=5; self.panelmergind=5; self.panelspacing=5
        self.titlebarheight=45; self.toolbarheight=50
        self.tabmerginsa=5; self.tabmerginsb=0; self.tabmerginsc=5; self.tabmerginsd=0; self.statusheight=22
        self.buticonsizex=20; self.buticonsizey=20
        self.gadiconsizex=20; self.gadiconsizey=20
        self.iconsizex=64; self.iconsizey=64

        # Icon factory
        self.icon_factory = SVGIconFactory()

        self.setWindowTitle(App_name)
        try:
            self.setWindowIcon(SVGIconFactory.path_app_icon(64))
        except Exception:
            pass
        self.resize(1400, 800)
        self.setMinimumSize(800, 500)

        # Radar state
        self._img_reader:   Optional[ImgReader] = None
        self._iv_img = None                  # GTA IV radar.img (IMGFile, V3)
        self._texdb = None                   # mobile texture DB (SA iOS/Android radar)
        self._iv_entries: Dict[int, object] = {}
        self._img_path:     str = ""
        self._game_preset:  dict = GAME_PRESETS["SA PC"]
        self._current_idx:  int  = -1
        self._init_path_system()
        self._draw_tool: str = 'none'        # 'none' / 'zoom' / 'path_node'
        self._tw: int = TILE_W
        self._th: int = TILE_H
        self._tile_rgba:    Dict[int, bytes] = {}
        self._world_offset: tuple = (0.0, 0.0)  # (dx, dy) world repositioning
        self._tile_entries: List[dict] = []

        # Frameless in standalone (custom titlebar), widget when docked
        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        else:
            self.setWindowFlags(Qt.WindowType.Widget)

        if parent:
            p = parent.pos()
            self.move(p.x() + 50, p.y() + 80)

        self.setup_ui()
        self.setAcceptDrops(True)       # radar and path files dropped here
        self._setup_hotkeys()
        self._apply_theme()
        self._apply_preset("SA PC")



def open_path_workshop(main_window=None): #vers 1
    try:
        w = PathWorkshop(None, main_window); w.show(); return w
    except Exception as e:
        if main_window: QMessageBox.critical(main_window,App_name+" Error",str(e))
        return None

__all__ = ["PathWorkshop", "open_path_workshop"]

# Standalone entry point

if __name__ == "__main__": #Vers 1
    import traceback

    print(App_name + " starting…")
    try:
        app = QApplication(sys.argv)
        w = PathWorkshop()
        w.setWindowTitle(App_name + " – Standalone")
        w.resize(1300, 800); w.show(); sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc();sys.exit(1)
