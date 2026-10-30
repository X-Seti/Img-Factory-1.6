#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_win_func.py - Version: 5
# X-Seti - October 28 2026 - IMG Factory 1.6 - Vehicle Workshop window

"""
Vehicle-only window parts on top of methods/gui_workshop.GUIWorkshop:
identity, settings keys and Vehicle settings tab, game IMG on show, timers stopped on close.
"""

##class VehicleWinMixin: -
# _extra_settings_tab
# _load_game_vehicle_img
# closeEvent
# showEvent

import os

from PyQt6.QtWidgets import QCheckBox, QGroupBox, QHBoxLayout, QLineEdit, QPushButton, QVBoxLayout, QWidget


class VehicleWinMixin: #vers 1
    """Vehicle Workshop identity and window overrides (chrome from GUIWorkshop)."""

    App_name        = "Vehicle Workshop"
    App_icon        = "vehicle_app_icon"
    WS_DEFAULTS     = {"game_root": "", "viewport_grid": True, "viewport_cull": True}
    tabmerginsa = tabmerginsb = tabmerginsc = tabmerginsd = 2   # status bar inner margins
    panelspacing, titlebarheight = 4, 36

    def _extra_settings_tab(self, tabs): #vers 1
        """Vehicle tab: game root, viewport start options; returns the OK action."""
        ws = self.WS
        vt = QWidget()
        vl = QVBoxLayout(vt)
        game_grp = QGroupBox("GTA Game Root")
        gl = QHBoxLayout(game_grp)
        game_edit = QLineEdit(ws.get("game_root", ""))
        game_edit.setPlaceholderText("e.g. /home/user/GTASA-PC")
        browse_btn = QPushButton("Browse…")

        def _browse_game(): #vers 1
            from PyQt6.QtWidgets import QFileDialog
            d = QFileDialog.getExistingDirectory(vt, "Select GTA Game Root", game_edit.text())
            if d:
                game_edit.setText(d)
        browse_btn.clicked.connect(_browse_game)
        gl.addWidget(game_edit, 1)
        gl.addWidget(browse_btn)
        vl.addWidget(game_grp)
        vp_grp = QGroupBox("3D Viewport")
        vpl = QVBoxLayout(vp_grp)
        grid_cb = QCheckBox("Show grid on startup")
        grid_cb.setChecked(ws.get("viewport_grid", True))
        cull_cb = QCheckBox("Backface culling on startup")
        cull_cb.setChecked(ws.get("viewport_cull", True))
        vpl.addWidget(grid_cb)
        vpl.addWidget(cull_cb)
        vl.addWidget(vp_grp)
        vl.addStretch()
        tabs.addTab(vt, "Vehicle")

        def apply(): #vers 1
            ws.set("game_root", game_edit.text())
            ws.set("viewport_grid", grid_cb.isChecked())
            ws.set("viewport_cull", cull_cb.isChecked())
        return apply

    def showEvent(self, event): #vers 6
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

    def closeEvent(self, event): #vers 4
        """Stop the animation timer and texture worker, then the base close (geometry, signals)."""
        if getattr(self, '_anim_timer', None):
            self._anim_timer.stop()
            self._anim_timer = None
        if getattr(self, '_shared_txd_worker', None):
            self._shared_txd_worker.quit()
        super().closeEvent(event)
