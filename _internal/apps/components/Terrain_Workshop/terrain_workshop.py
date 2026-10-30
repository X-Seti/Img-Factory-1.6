#!/usr/bin/env python3
#this belongs in apps/components/Terrain_Workshop/terrain_workshop.py - Version: 9
# X-Seti - October 22 2026 - IMG Factory 1.6 - Terrain Workshop

"""
Terrain Workshop - Map Workshop with a terrain grid: create ground, sculpt
it, paint materials (grass, sand, rock, road, path...) with proper UV
tiling, and export DFF / COL / TXD / IDE / IPL for III, VC and SA.
"""

##Methods list -
# open_terrain_workshop
#
##class TerrainWorkshop: -
# _ribbon_config_path
# _radar_settings_path
# _titlebar_title
# closeEvent
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

from apps.components.Map_Editor.map_workshop import ModelWorkshop as MapWorkshop
from apps.components.Terrain_Workshop.depends.terrain_assets_func import TerrainAssetsMixin
from apps.components.Terrain_Workshop.depends.terrain_water_func import TerrainWaterMixin
from apps.components.Terrain_Workshop.depends.terrain_nature_func import TerrainNatureMixin
from apps.components.Terrain_Workshop.depends.terrain_export_func import TerrainExportMixin
from apps.components.Terrain_Workshop.depends.terrain_grid_func import TerrainGridMixin
from apps.components.Terrain_Workshop.depends.terrain_image_func import TerrainImageMixin
from apps.components.Terrain_Workshop.depends.terrain_material_func import TerrainMaterialMixin
from apps.components.Terrain_Workshop.depends.terrain_panel_func import TerrainPanelMixin

App_name = "Terrain Workshop"


class TerrainWorkshop(TerrainPanelMixin, TerrainAssetsMixin, TerrainNatureMixin, TerrainWaterMixin, TerrainGridMixin, TerrainImageMixin,
                      TerrainMaterialMixin, TerrainExportMixin, MapWorkshop): #vers 6
    """Map Workshop plus the terrain Environment panel (sculpt, water, paint, nature, paths, assets, setup / export)."""
    _OUTER_LAYOUT_VERSION = 23           # own dock layout (Environment panel)

    def _ribbon_config_path(self): #vers 1
        from apps.components.Map_Editor.depends.map_widgets_func import _model_workshop_config_dir
        return _model_workshop_config_dir() / 'terrain_workshop.json'

    def _radar_settings_path(self): #vers 1
        """Game root radar_settings.json, else Terrain's own file."""
        return super()._radar_settings_path() or str(self._ribbon_config_path().with_name('terrain_radar.json'))

    def _titlebar_title(self): #vers 1
        return App_name

    def setup_ui(self): #vers 4
        """Map Workshop layout, then one Environment panel (rail + pages) on the right."""
        super().setup_ui()
        from apps.methods.dock_panels import icon_only_tabs
        from apps.methods.ui_sizes import fit_inputs
        from PyQt6.QtWidgets import QDockWidget
        mw = self._outer_mw
        env = self._create_tw_env_dock()
        self._make_dock_collapsible(env, "Environment")
        mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, env)
        fit_inputs(env)
        map_right = [d for d in mw.findChildren(QDockWidget)            # Map's own right docks
                     if d.objectName() in ("Object Browser", "IPL Inst File", "IPL Controls")]
        for d in map_right:
            mw.removeDockWidget(d)
        for d in map_right:                                              # one tab group: Environment first
            mw.tabifyDockWidget(env, d)
            d.show()
        env.raise_()
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as S
        ic = self._get_icon_color()
        icons = {"Object Browser": S.box_icon(16, ic), "IPL Inst File": S.list_icon(16, ic),
                 "IPL Controls": S.settings_icon(16, ic)}
        icon_only_tabs(mw, [(env, OverlayIcons.terrain_icon(16))] +
                       [(d, icons[d.objectName()]) for d in map_right])     # space is limited
        mw.resizeDocks([env], [380], Qt.Orientation.Horizontal)
        act = getattr(self, '_terrain_act', None)                    # Map's game-world brush: not here
        if act is not None:
            act.setVisible(False)
        self.setWindowTitle(App_name)
        self._tw_status()

    def closeEvent(self, event): #vers 1
        """Ask before losing an unsaved terrain."""
        if not self._tw_confirm_discard():
            event.ignore()
            return
        super().closeEvent(event)


def open_terrain_workshop(main_window=None): #vers 1
    """Open docked in IMG Factory (tab) or standalone."""
    if main_window is not None and hasattr(main_window, 'main_tab_widget'):
        from PyQt6.QtWidgets import QVBoxLayout, QWidget
        container = QWidget()
        lay = QVBoxLayout(container)
        lay.setContentsMargins(0, 0, 0, 0)
        ws = TerrainWorkshop(container, main_window)
        ws.setWindowFlags(Qt.WindowType.Widget)
        lay.addWidget(ws)
        idx = main_window.main_tab_widget.addTab(container, App_name)
        main_window.main_tab_widget.setCurrentIndex(idx)
    else:
        ws = TerrainWorkshop(main_window=main_window)
        ws.setWindowTitle(App_name)
        ws.resize(1500, 950)
    ws.show()
    return ws


if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = open_terrain_workshop(None)
    sys.exit(app.exec())
