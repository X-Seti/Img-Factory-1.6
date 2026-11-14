#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_win_func.py - Version: 9
# X-Seti - October 28 2026 - IMG Factory 1.6 - Vehicle Workshop window

"""
Vehicle-only window parts on top of methods/gui_workshop.GUIWorkshop:
identity, settings keys and Vehicle settings tab, game IMG on show, timers stopped on close.
"""

##class VehicleWinMixin: -
# _extra_settings_tabs
# _load_game_vehicle_img
# _vw_grid_button
# _vw_grid_cfg
# _vw_set_grid
# closeEvent
# showEvent

import os

from PyQt6.QtWidgets import QCheckBox, QGroupBox, QHBoxLayout, QLineEdit, QPushButton, QVBoxLayout, QWidget


class VehicleWinMixin: #vers 1
    """Vehicle Workshop identity and window overrides (chrome from GUIWorkshop)."""

    App_name        = "Vehicle Workshop"
    App_icon        = "vehicle_app_icon"
    WS_DEFAULTS     = {"game_root": "", "viewport_grid": True, "viewport_cull": True, "grid": {}}
    tabmerginsa = tabmerginsb = tabmerginsc = tabmerginsd = 2   # status bar inner margins
    panelspacing, titlebarheight = 4, 36

    def _extra_settings_tabs(self, tabs): #vers 1
        """Vehicle tab: game root, viewport start options, applied live (ModelWorkshop hook - no OK-apply return)."""
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
                ws.set("game_root", d)
        browse_btn.clicked.connect(_browse_game)
        game_edit.editingFinished.connect(lambda: ws.set("game_root", game_edit.text()))
        gl.addWidget(game_edit, 1)
        gl.addWidget(browse_btn)
        vl.addWidget(game_grp)
        vp_grp = QGroupBox("3D Viewport")
        vpl = QVBoxLayout(vp_grp)
        grid_cb = QCheckBox("Show grid on startup")
        grid_cb.setChecked(ws.get("viewport_grid", True))
        grid_cb.toggled.connect(lambda v: ws.set("viewport_grid", v))
        cull_cb = QCheckBox("Backface culling on startup")
        cull_cb.setChecked(ws.get("viewport_cull", True))
        cull_cb.toggled.connect(lambda v: ws.set("viewport_cull", v))
        vpl.addWidget(grid_cb)
        vpl.addWidget(cull_cb)
        vl.addWidget(vp_grp)
        vl.addStretch()
        tabs.addTab(vt, "Vehicle")
        from apps.methods.grid_style import grid_settings_page

        def grid_apply(cfg):  #vers 1
            self._vw_set_grid('custom', cfg=cfg)
        tabs.addTab(grid_settings_page(self, self._vw_grid_cfg(), grid_apply), "Grid")

    def _vw_grid_cfg(self) -> dict: #vers 1
        """Saved custom grid (pattern, colours, thickness, cell size)."""
        from apps.methods.grid_style import GRID_DEFAULT
        cfg = dict(GRID_DEFAULT)
        saved = self.WS.get('grid', {}) or {}
        cfg.update({k: saved[k] for k in cfg if k in saved})
        return cfg

    def _vw_set_grid(self, kind: str, path: str = '', cfg: dict = None): #vers 1
        """Grid preset or 'custom' (same styles as Ped Workshop); saved in the workshop settings."""
        from PyQt6.QtWidgets import QFileDialog
        from apps.methods.grid_style import apply_grid_custom, apply_grid_preset
        cur = cfg or self._vw_grid_cfg()
        if kind == 'texture' and not path:
            path, _ = QFileDialog.getOpenFileName(self, "Grid texture", '',
                                                  "Images (*.png *.jpg *.jpeg *.bmp *.tga)")
            if not path:
                return
        if kind == 'custom':
            apply_grid_custom(self.viewport, cur)
        else:
            apply_grid_preset(self.viewport, kind, path, int(cur.get('cell', 0)))
        self.WS.set('grid', dict(cur, kind=kind, path=path))
        self.WS.save()
        self.viewport.update()

    def _vw_grid_button(self): #vers 1
        """Grid style menu button (lines, holodeck, checker, honeycomb, texture, custom, none)."""
        from PyQt6.QtWidgets import QMenu, QToolButton
        from apps.methods.grid_style import GRID_PRESETS
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        b = QToolButton()
        b.setIcon(SVGIconFactory.grid_icon(16, self._get_icon_color()))
        b.setText("Grid")
        b.setToolTip("Grid style (Settings > Grid for colours, thickness and cell size)")
        b.setPopupMode(QToolButton.ToolButtonPopupMode.InstantPopup)
        menu = QMenu(b)
        for key, (_t, _f, _l, label) in GRID_PRESETS.items():
            menu.addAction(label, lambda k=key: self._vw_set_grid(k))
        menu.addAction("Custom (Settings)", lambda: self._vw_set_grid('custom'))
        b.setMenu(menu)
        self._vw_grid_btn = b
        saved = self.WS.get('grid', {}) or {}
        if saved.get('kind'):
            self._vw_set_grid(saved['kind'], saved.get('path', ''))
        return b

    def showEvent(self, event): #vers 8
        """Defer IMG listing until the window has painted."""
        super().showEvent(event)
        if not self.standalone_mode:
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(50, self._load_game_vehicle_img)

    def _load_game_vehicle_img(self): #vers 4
        """Open vehicles.img / gta3.img tab first, else <game>/models IMG (IMG Factory or Map Workshop root)."""
        from apps.methods.game_file_list import game_img
        mw = self.main_window
        if mw is None:
            return
        opened = [v.get('file_object') for v in (getattr(mw, 'open_files', None) or {}).values()]
        tw = getattr(mw, 'main_tab_widget', None)              # tabs hold file_object now
        if tw is not None:
            opened += [getattr(tw.widget(i), 'file_object', None) for i in range(tw.count())]
        by_name = {os.path.basename(getattr(i, 'file_path', '') or '').lower(): i
                   for i in opened if i is not None and hasattr(i, 'entries')}
        for n in ('vehicles.img', 'gta3.img'):
            if n in by_name:
                if by_name[n] is not getattr(self, '_current_img', None):
                    self.load_img(by_name[n])
                return
        if self._img_list.count():
            return
        roots = [getattr(mw, 'game_root', '') or ''] + \
                [w._game_root for w in mw.findChildren(QWidget) if getattr(w, '_game_root', '')]
        for root in roots:
            models = os.path.join(root, 'models') if root else ''
            if not os.path.isdir(models):
                continue
            files = {f.lower(): f for f in os.listdir(models)}
            name = files.get('vehicles.img') or files.get('gta3.img')
            if name:
                self._game_root = root
                self.load_img(game_img(self, os.path.join(models, name)))
                return

    def closeEvent(self, event): #vers 4
        """Stop the animation timer and texture worker, then the base close (geometry, signals)."""
        if getattr(self, '_anim_timer', None):
            self._anim_timer.stop()
            self._anim_timer = None
        if getattr(self, '_shared_txd_worker', None):
            self._shared_txd_worker.quit()
        super().closeEvent(event)
