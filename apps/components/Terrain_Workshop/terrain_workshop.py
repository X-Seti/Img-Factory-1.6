#!/usr/bin/env python3
#this belongs in apps/components/Terrain_Workshop/terrain_workshop.py - Version: 4
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
from apps.components.Terrain_Workshop.depends.terrain_export_func import TerrainExportMixin
from apps.components.Terrain_Workshop.depends.terrain_grid_func import TerrainGridMixin
from apps.components.Terrain_Workshop.depends.terrain_image_func import TerrainImageMixin
from apps.components.Terrain_Workshop.depends.terrain_material_func import TerrainMaterialMixin

App_name = "Terrain Workshop"


class TerrainWorkshop(TerrainGridMixin, TerrainImageMixin, TerrainMaterialMixin, TerrainExportMixin,
                      MapWorkshop): #vers 2
    """Map Workshop plus terrain grid, brush, materials and export docks."""
    _OUTER_LAYOUT_VERSION = 22           # own dock layout

    def _ribbon_config_path(self): #vers 1
        from apps.components.Map_Editor.depends.map_widgets_func import _model_workshop_config_dir
        return _model_workshop_config_dir() / 'terrain_workshop.json'

    def _titlebar_title(self): #vers 1
        return App_name

    def setup_ui(self): #vers 3
        """Map Workshop layout, then Terrain / Brush / Materials / Export on the right."""
        super().setup_ui()
        from apps.methods.dock_panels import add_title_widget, icon_only_tabs
        from apps.methods.ui_sizes import fit_inputs
        mw = self._outer_mw
        docks = (self._create_tw_grid_dock(), self._create_tw_brush_dock(),
                 self._create_tw_material_dock(), self._create_tw_export_dock())
        from PyQt6.QtWidgets import QFrame, QScrollArea
        for dock, title in zip(docks, ("Terrain", "Brush", "Materials", "Export")):
            dock.setMinimumWidth(260)
            if dock is not docks[2]:                              # scroll when space is tight
                scroll = QScrollArea()
                scroll.setWidgetResizable(True)
                scroll.setFrameShape(QFrame.Shape.NoFrame)
                scroll.setWidget(dock.widget())
                dock.setWidget(scroll)
            self._make_dock_collapsible(dock, title)
            mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, dock)
            fit_inputs(dock)
        for title_widgets, dock in ((self._tw_grid_title, docks[0]), (self._tw_brush_title, docks[1]),
                                    (self._tw_mat_title, docks[2])):
            for w in title_widgets:
                add_title_widget(dock, w)
        from PyQt6.QtWidgets import QDockWidget
        map_right = [d for d in mw.findChildren(QDockWidget)            # Map's own right docks
                     if d.objectName() in ("Object Browser", "IPL Inst File", "IPL Controls")]
        for d in map_right:
            mw.removeDockWidget(d)
        mw.splitDockWidget(docks[0], docks[1], Qt.Orientation.Vertical)
        mw.splitDockWidget(docks[1], docks[2], Qt.Orientation.Vertical)
        for d in [docks[3]] + map_right:                            # one tab group: Terrain first
            mw.tabifyDockWidget(docks[0], d)
            d.show()
        docks[0].raise_()
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as S
        ic = self._get_icon_color()
        icons = {"Object Browser": S.box_icon(16, ic), "IPL Inst File": S.list_icon(16, ic),
                 "IPL Controls": S.settings_icon(16, ic)}
        icon_only_tabs(mw, [(docks[0], OverlayIcons.terrain_icon(16)), (docks[3], S.export_icon(16, ic))] +
                       [(d, icons[d.objectName()]) for d in map_right])     # space is limited
        mw.resizeDocks(list(docks[:3]), [330, 260, 360], Qt.Orientation.Vertical)
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
