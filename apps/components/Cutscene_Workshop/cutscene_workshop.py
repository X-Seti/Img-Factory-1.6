#!/usr/bin/env python3
#this belongs in apps/components/Cutscene_Workshop/cutscene_workshop.py - Version: 17
# X-Seti - November 02 2026 - IMG Factory 1.6 - Cutscene Workshop

"""
Cutscene Workshop - GTA III / VC / SA / IV cutscenes (anim/cuts.img): edit the
.cut script sections, the .dat camera as keys or a shot list, actor routes in
the Plan view; swap, add, export and convert the cast for in-game use.
"""

##Methods list -
# open_cutscene_workshop
#
##class CutsceneWorkshop: -
# __init__
# _build_ribbons
# setup_ui

import os
import sys

if sys.platform.startswith('linux'):           # NVIDIA on Wayland: X11 / GLX backend
    os.environ['QT_QPA_PLATFORM'] = 'xcb'
    os.environ['QSG_RHI_BACKEND'] = 'opengl'
_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
if _root not in sys.path:                      # standalone: make 'apps' importable
    sys.path.insert(0, _root)

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QApplication

from PyQt6.QtWidgets import QTabWidget, QVBoxLayout

from apps.methods.gui_workshop import GUIWorkshop
from apps.methods.ribbon_dialog import RibbonIconsMixin
from apps.methods.ribbon_system import RibbonMixin
from apps.methods.statusbar_func import register_statusbar
from apps.components.Cutscene_Workshop.depends.cutscene_cast_func import CutsceneCastMixin
from apps.components.Cutscene_Workshop.depends.cutscene_logic_func import CutsceneLogicMixin
from apps.components.Cutscene_Workshop.depends.cutscene_shots_func import CutsceneShotsMixin
from apps.components.Cutscene_Workshop.depends.cutscene_ui_func import CutsceneUIMixin

App_name = "Cutscene Workshop"


class CutsceneWorkshop(RibbonIconsMixin, RibbonMixin, CutsceneUIMixin, CutsceneLogicMixin, CutsceneCastMixin,
                       CutsceneShotsMixin, GUIWorkshop): #vers 6
    """Cutscene Workshop window: preview in the centre, list / script / camera as tear-off panels."""
    App_name = App_name
    config_key = "cutscene_workshop"
    App_description = "Cutscene scripts, camera paths and cast (III / VC / SA / IV) - IMG Factory 1.6"
    App_icon = "cutscene_app_icon"
    DROP_EXTS = ('.cut', '.dat', '.img', '.dir')
    WS_DEFAULTS = {'last_path': '', 'show_grid': True, 'render_mode': 'textured',
                   'wire': False, 'cam_zoom': -50, 'game_view': False,
                   'show_map': False, 'map_radius': 150, 'show_plan': True,
                   'model_lift': 1.0, 'timecyc': False, 'hour': 12}
    _ribbon_name = "cutscene_workshop"
    _RIBBON_LAYOUT_VERSION = 3
    _OUTER_LAYOUT_VERSION = 3

    def __init__(self, parent=None, main_window=None): #vers 2
        self._defer_setup_ui = True
        super().__init__(parent, main_window)
        self.setup_ui()

    def setup_ui(self): #vers 4
        """Ribbons over the 3D preview; Cutscenes, Script, Camera, Shots and Cast panels dock / float."""
        ml = QVBoxLayout(self)
        ml.setContentsMargins(*self.get_content_margins())
        ml.setSpacing(self.setspacing)
        ml.addWidget(self._create_toolbar())
        centre = self._cs_preview_tab()
        self._cs_tabs = QTabWidget()
        self._cs_tabs.setDocumentMode(True)
        self._cs_tabs.addTab(self._cs_script_tab(), "Sections")
        self._cs_tabs.addTab(self._cs_text_tab(), "Text")
        self._cs_tabs.currentChanged.connect(self._cs_tab_changed)
        ml.addWidget(self.ribbon_host(self._dock_layout(centre, [
            ("Cutscenes", self._create_left_panel(), 160, Qt.DockWidgetArea.LeftDockWidgetArea),
            ("Script", self._cs_tabs, 300, Qt.DockWidgetArea.RightDockWidgetArea),
            ("Camera", self._cs_camera_tab(), 300, Qt.DockWidgetArea.RightDockWidgetArea),
            ("Cast", self._create_cast_panel(), 300, Qt.DockWidgetArea.RightDockWidgetArea),
            ("Shots", self._create_shots_panel(), 300, Qt.DockWidgetArea.RightDockWidgetArea)])), 1)
        self._outer_mw.tabifyDockWidget(self._outer_docks[1], self._outer_docks[3])   # Cast beside Script
        self._outer_mw.tabifyDockWidget(self._outer_docks[2], self._outer_docks[4])   # Shots beside Camera
        self._outer_docks[1].raise_()
        self._build_ribbons()
        self._status_widget = self._create_status_bar()
        ml.addWidget(self._status_widget)
        register_statusbar(self, self._status_widget)
        self.ribbon_restore_state()
        self._inner_mw = self._ribbon_mw                 # Ribbon Manager target
        self._apply_ribbon_layout()
        from apps.methods.ui_sizes import fit_inputs
        fit_inputs(self)                                 # compact inputs

    def _build_ribbons(self): #vers 9
        """File, Script, Camera, Cast and Playback ribbons (one icon per action)."""
        B = self.ribbon_button
        tb = self.ribbon_toolbar("File")
        B(tb, "open_icon", "Open anim/cuts.img, a .cut or a .dat  (Ctrl+O)", self._open_file, text="Open")
        B(tb, "save_icon", "Save the .cut and .dat  (Ctrl+S)", self._save_file, text="Save")
        tb = self.ribbon_toolbar("Script")
        B(tb, "add_icon", "Add a row to this section", self._cs_add_row, text="+Row")
        B(tb, "delete_icon", "Remove the selected row", self._cs_del_row, text="-Row")
        tb = self.ribbon_toolbar("Camera")
        B(tb, "key_set_icon", "Add a camera key after the selected one", self._cam_add_key, text="+Key")
        B(tb, "key_delete_icon", "Remove the selected camera key", self._cam_del_key, text="-Key")
        tb = self.ribbon_toolbar("Cast")
        for icon, tip, slot, text in (
                ("cast_swap_icon", "Play the picked part with another model (preview)", self._cast_swap, "Swap"),
                ("cast_clear_icon", "Back to the original model", self._cast_clear, "Orig"),
                ("cast_add_icon", "Add a model to the .cut (SA)", self._cast_add, "+Cast"),
                ("cast_remove_icon", "Remove the picked model from the .cut (SA)", self._cast_remove, "-Cast"),
                ("cast_import_icon", "Stand-in from a DFF / TXD file", self._cast_import, "Import"),
                ("cast_export_icon", "Save the picked model's DFF / TXD as they are", self._cast_export, "Export"),
                ("cast_game_icon", "Save converted for in-game use (ped skeleton / donor car)",
                 lambda: self._cast_export_game(), "Game"),
                ("cast_send_icon", "Open in the Ped / Vehicle / Model Workshop", self._cast_send, "Send"),
                ("cast_write_icon", "Write the stand-ins into the game IMG", self._cast_apply, "Write")):
            B(tb, icon, tip, slot, text=text)
        tb = self.ribbon_toolbar("Playback")
        B(tb, "skip_start_icon", "Back to the start", lambda: self._pv_slider.setValue(0), text="|<")
        B(tb, "frame_prev_icon", "Back one frame (1/30 s)", lambda: self._cs_preview_step(-1), text="<|")
        self._pv_play = B(tb, "play_icon", "Play / pause the cutscene", self._cs_preview_play,
                          checkable=True, text="Play")
        B(tb, "frame_next_icon", "Forward one frame (1/30 s)", lambda: self._cs_preview_step(1), text="|>")
        self._pv_game_btn = B(tb, "game_view_icon", "Game view: cutscene camera as in game (4:3, zoom 0 %)",
                              self._cs_game_view, checkable=True, text="Game")
        self._pv_game_btn.setChecked(bool(self.WS.get('game_view', False)))
        self._pv_map_btn = B(tb, "map_app_icon", "Map around the scene (SA / IV: from the .cut offset)",
                             self._cs_map_toggle, checkable=True, text="Map")
        self._pv_map_btn.setChecked(bool(self.WS.get('show_map', False)))
        self._pv_plan_btn = B(tb, "plan_view_icon", "Plan view beside the 3D view: camera, view area, actor routes",
                              self._cs_plan_toggle, checkable=True, text="Plan")
        self._pv_plan_btn.setChecked(bool(self.WS.get('show_plan', True)))
        B(tb, "fit_icon", "Free camera: fit the models", self._cs_preview_centre, text="Fit")
        self._pv_settings_btn = B(tb, "settings_icon", "View settings: render, zoom, grid", self._cs_view_settings, text="Set")


def open_cutscene_workshop(main_window=None, file_path=None): #vers 2
    """Open docked in IMG Factory (tab) or standalone; file_path, else the game / last cutscenes."""
    if main_window is not None and hasattr(main_window, 'main_tab_widget'):
        ws = CutsceneWorkshop(None, main_window)
        ws.setWindowFlags(Qt.WindowType.Widget)
        idx = main_window.main_tab_widget.addTab(ws, App_name)
        main_window.main_tab_widget.setCurrentIndex(idx)
    else:
        ws = CutsceneWorkshop(main_window=main_window)
        ws.setWindowTitle(App_name)
        ws.resize(1200, 800)
    ws.show()
    from PyQt6.QtCore import QTimer
    if file_path:
        QTimer.singleShot(100, lambda: ws._open_file(file_path))
    else:
        QTimer.singleShot(100, lambda: ws._cs_autoload(getattr(main_window, 'game_root', '') or ''))
    return ws


if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = open_cutscene_workshop(None)
    sys.exit(app.exec())
