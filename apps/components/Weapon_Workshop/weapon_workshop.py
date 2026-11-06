#!/usr/bin/env python3
#this belongs in apps/components/Weapon_Workshop/weapon_workshop.py - Version: 6
# X-Seti - November 02 2026 - IMG Factory 1.6 - Weapon Workshop

"""
Weapon Workshop - weapon.dat (III / VC / SA), melee.dat and ar_stats.dat
as tables: guns, melee weapons, aim offsets, melee combos, stat values.
"""

##Methods list -
# open_weapon_workshop
#
##class WeaponWorkshop: -
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

from PyQt6.QtWidgets import QVBoxLayout

from apps.methods.gui_workshop import GUIWorkshop
from apps.methods.ribbon_dialog import RibbonIconsMixin
from apps.methods.ribbon_system import RibbonMixin
from apps.methods.statusbar_func import register_statusbar
from apps.components.Weapon_Workshop.depends.weapon_logic_func import WeaponLogicMixin
from apps.components.Weapon_Workshop.depends.weapon_model_func import WeaponModelMixin
from apps.components.Weapon_Workshop.depends.weapon_ui_func import WeaponUIMixin

App_name = "Weapon Workshop"


class WeaponWorkshop(RibbonIconsMixin, RibbonMixin, WeaponUIMixin, WeaponLogicMixin, WeaponModelMixin,
                     GUIWorkshop): #vers 4
    """Weapon Workshop window."""
    App_name = App_name
    config_key = "weapon_workshop"
    App_description = "Weapon, melee and stat data - IMG Factory 1.6"
    App_icon = "weapon_app_icon"
    _ribbon_name = "weapon_workshop"
    _RIBBON_LAYOUT_VERSION = 3
    _OUTER_LAYOUT_VERSION = 2
    DROP_EXTS = ('.dat',)
    WS_DEFAULTS = {'last_path': ''}

    def __init__(self, parent=None, main_window=None): #vers 2
        self._defer_setup_ui = True
        super().__init__(parent, main_window)
        self.setup_ui()

    def setup_ui(self): #vers 3
        """Ribbons over the tables; files, weapons and the 3D model are tear-off panels."""
        ml = QVBoxLayout(self)
        ml.setContentsMargins(*self.get_content_margins())
        ml.setSpacing(self.setspacing)
        ml.addWidget(self._create_toolbar())
        ml.addWidget(self.ribbon_host(self._dock_layout(self._create_centre_panel(), [
            ("Files", self._create_left_panel(), 150, Qt.DockWidgetArea.LeftDockWidgetArea),
            ("Weapons", self._create_weapons_panel(), 200, Qt.DockWidgetArea.LeftDockWidgetArea),
            ("Model", self._create_model_view(), 320, Qt.DockWidgetArea.RightDockWidgetArea)])), 1)
        self._build_ribbons()
        self._status_widget = self._create_status_bar()
        ml.addWidget(self._status_widget)
        register_statusbar(self, self._status_widget)
        self.ribbon_restore_state()
        self._inner_mw = self._ribbon_mw                 # Ribbon Manager target
        self._apply_ribbon_layout()
        from apps.methods.ui_sizes import fit_inputs
        fit_inputs(self)

    def _build_ribbons(self): #vers 3
        """File, Export and Import ribbons (data files; weapon DFF / TXD / COL / PNG)."""
        B = self.ribbon_button
        tb = self.ribbon_toolbar("File")
        B(tb, "open_icon", "Open a data folder or weapon.dat / melee.dat / ar_stats.dat  (Ctrl+O)",
          self._open_file, text="Open")
        B(tb, "save_icon", "Save the changed files (backups kept)  (Ctrl+S)", self._save_file, text="Save")
        for title, fn in (("Export", self._wm_export), ("Import", self._wm_import)):
            tb = self.ribbon_toolbar(title)
            way = title.lower()
            verb = "Save the selected weapon's" if way == "export" else "Replace the selected weapon's"
            B(tb, f"dff_{way}_icon", f"{verb} model (.dff)", lambda _=False, f=fn: f('dff'), text="DFF")
            B(tb, f"txd_{way}_icon", f"{verb} texture dictionary (.txd)", lambda _=False, f=fn: f('txd'), text="TXD")
            B(tb, f"col_{way}_icon", f"{verb} collision (.col), when it has one", lambda _=False, f=fn: f('col'),
              text="COL")
            if way == "export":
                B(tb, "png_export_icon", "Save every texture as PNG", self._wm_export_pngs, text="PNG")
            else:
                B(tb, "png_import_icon", "Replace the selected texture with a PNG", self._wm_import_png, text="PNG")


def open_weapon_workshop(main_window=None, file_path=None): #vers 2
    """Open docked in IMG Factory (tab) or standalone; file_path, else the game / last data folder."""
    if main_window is not None and hasattr(main_window, 'main_tab_widget'):
        ws = WeaponWorkshop(None, main_window)
        ws.setWindowFlags(Qt.WindowType.Widget)
        idx = main_window.main_tab_widget.addTab(ws, App_name)
        main_window.main_tab_widget.setCurrentIndex(idx)
    else:
        ws = WeaponWorkshop(main_window=main_window)
        ws.setWindowTitle(App_name)
        ws.resize(1200, 800)
    ws.show()
    from PyQt6.QtCore import QTimer
    if file_path:
        QTimer.singleShot(100, lambda: ws._open_file(file_path))
    else:
        QTimer.singleShot(100, lambda: ws._wp_autoload(getattr(main_window, 'game_root', '') or ''))
    return ws


if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = open_weapon_workshop(None)
    sys.exit(app.exec())
