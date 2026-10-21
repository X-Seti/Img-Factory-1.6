#!/usr/bin/env python3
#this belongs in apps/components/Ped_Workshop/ped_workshop.py - Version: 11
# X-Seti - October 21 2026 - IMG Factory 1.6 - Ped Workshop

"""
Ped Workshop - Model Workshop with a Peds dock (peds IDE, ped groups,
ped types) and an Animation dock (IFP playback, stick-man bones,
stop-motion keyframe editing) for GTA III, VC and SA.
"""

##Methods list -
# open_ped_workshop
#
##class PedWorkshop: -
# _default_ped_docks
# _load_ped_prefs
# _ped_dirty_items
# _ped_open_menu
# _ped_save_all
# _ribbon_config_path
# _save_ped_prefs
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

from apps.components.Model_Editor.model_workshop import ModelWorkshop
from apps.components.Ped_Workshop.depends.ped_anim_func import PedAnimMixin
from apps.components.Ped_Workshop.depends.ped_data_func import PedDataMixin
from apps.methods.img_factory_settings import get_user_config_dir

App_name = "Ped Workshop"


class PedWorkshop(PedDataMixin, PedAnimMixin, ModelWorkshop): #vers 2
    """Model Workshop plus ped data and animation docks."""
    _OUTER_LAYOUT_VERSION = 13           # own dock layout (Peds / Animation / Bones)

    def _ribbon_config_path(self): #vers 1
        """Own ribbon and dock layout file."""
        return get_user_config_dir() / 'ped_workshop.json'

    def _default_ped_docks(self, docks): #vers 2
        """No saved layout: Peds 42 %, Animation 30 %, Bones 28 % of the right column."""
        import json
        try:
            data = json.loads(self._ribbon_config_path().read_text())
            if data.get('outer_layout_version') == self._OUTER_LAYOUT_VERSION:
                return
        except Exception:
            pass
        h = max(400, self._outer_mw.height())
        self._outer_mw.resizeDocks(list(docks), [int(h * 0.42), int(h * 0.3), int(h * 0.28)],
                                   Qt.Orientation.Vertical)

    def _titlebar_title(self): #vers 1
        return App_name

    def _load_ped_prefs(self) -> dict: #vers 1
        import json
        try:
            return json.loads(self._ribbon_config_path().read_text()).get('ped_prefs', {})
        except Exception:
            return {}

    def _save_ped_prefs(self, **values): #vers 1
        """Merge values into ped_prefs of ped_workshop.json."""
        import json
        path = self._ribbon_config_path()
        try:
            data = json.loads(path.read_text()) if path.exists() else {}
        except Exception:
            data = {}
        data.setdefault('ped_prefs', {}).update(values)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(data, indent=2))

    def _ped_open_menu(self): #vers 1
        """Open: game folder (remembered), IFP, or a single DFF / TXD."""
        from PyQt6.QtWidgets import QMenu
        from PyQt6.QtGui import QCursor
        m = QMenu(self)
        m.addAction("Game folder...", lambda: self._open_ped_root(''))
        recent = self._load_ped_prefs().get('recent_roots', [])
        if recent:
            sub = m.addMenu("Recent game folders")
            for r in recent:
                sub.addAction(r, lambda p=r: self._open_ped_root(p))
        m.addSeparator()
        m.addAction("IFP animation file...", lambda: self._anim_open_ifp(''))
        m.addAction("Model / TXD file...", self._open_file)
        m.exec(QCursor.pos())

    def _ped_dirty_items(self) -> list: #vers 1
        """Names of unsaved items."""
        out = []
        if getattr(self, '_ped_ide_dirty', False):
            out.append("peds IDE")
        if getattr(self, '_ped_file_shown', None) and self._ped_file_edit.document().isModified():
            out.append(os.path.basename(self._ped_file_shown))
        if getattr(self, '_ifp_dirty', False):
            out.append(f"{self._ifp.name}.ifp" if self._ifp else "IFP")
        return out

    def _ped_save_all(self): #vers 1
        """Save every changed item: peds IDE, data file, IFP."""
        items = self._ped_dirty_items()
        if not items:
            self._set_status("Ped Workshop: nothing to save")
            return
        if getattr(self, '_ped_ide_dirty', False):
            self._save_ped_ide()
        if getattr(self, '_ped_file_shown', None) and self._ped_file_edit.document().isModified():
            self._save_ped_text(self._ped_file_shown)
        if getattr(self, '_ifp_dirty', False):
            self._anim_save_ifp()
        self._set_status("Saved: " + ", ".join(items))

    def closeEvent(self, event): #vers 1
        """Ask before losing unsaved ped edits."""
        from PyQt6.QtWidgets import QMessageBox
        items = self._ped_dirty_items()
        if items:
            r = QMessageBox.question(self, App_name, "Save changes to " + ", ".join(items) + "?",
                                     QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard
                                     | QMessageBox.StandardButton.Cancel)
            if r == QMessageBox.StandardButton.Cancel:
                event.ignore()
                return
            if r == QMessageBox.StandardButton.Save:
                self._ped_save_all()
        super().closeEvent(event)

    def setup_ui(self): #vers 10
        """Model Workshop layout, then Peds, Animation and Bones docks on the right."""
        super().setup_ui()
        mw = self._outer_mw
        docks = (self._create_peds_dock(), self._create_anim_dock(), self._create_bones_dock())
        for dock, title in zip(docks, ("Peds", "Animation", "Bones")):
            dock.setMinimumWidth(280)
            self._make_dock_collapsible(dock, title)
            mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, dock)
        mw.splitDockWidget(docks[0], docks[1], Qt.Orientation.Vertical)
        mw.splitDockWidget(docks[1], docks[2], Qt.Orientation.Vertical)
        mw.resizeDocks(list(docks), [420, 300, 280], Qt.Orientation.Vertical)
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(700, lambda d=docks: self._default_ped_docks(d))
        outer = getattr(self, '_outer_docks', None)
        if isinstance(outer, list):
            outer.extend(docks)
        for btn, slot, tip in ((getattr(self, 'open_btn', None), self._ped_open_menu,
                                "Open a game folder, IFP or model"),
                               (getattr(self, 'save_btn', None), self._ped_save_all,
                                "Save all changed ped files (backup first)")):
            if btn is not None:
                btn.clicked.disconnect()
                btn.clicked.connect(slot)
                btn.setToolTip(tip)
        from apps.methods.dock_panels import add_title_widget
        from PyQt6.QtWidgets import QAbstractSpinBox, QComboBox, QLineEdit, QSizePolicy
        tb = self._anim_tool_button
        self._ped_root_label.setStyleSheet("padding-left: 6px;")
        self._ped_search.setMinimumWidth(70)
        for w in (self._ped_root_label,          # Peds | folder | folder, open | filter | save, save all | x
                  tb('folder_icon', "Game folder: GTA III, VC, SA or IV install", lambda: self._open_ped_root('')),
                  tb('open_icon', "Open a game folder, IFP or model", self._ped_open_menu),
                  self._ped_search,
                  tb('save_icon', "Save the peds IDE (changed rows, backup first)", self._save_ped_ide),
                  tb('save_all_icon', "Save all changed ped files: peds IDE, data file, IFP",
                     self._ped_save_all)):
            add_title_widget(docks[0], w)
        lbl_lay = docks[0].titleBarWidget().layout()
        lbl_lay.setStretchFactor(self._ped_root_label, 1)
        docks[1]._title_label.hide()                         # Animation: IFP combo is the title
        for w in self._anim_title_widgets:
            add_title_widget(docks[1], w)
        docks[1].titleBarWidget().layout().setStretchFactor(self._anim_search, 1)
        self._ifp_source.setMaximumWidth(110)
        for w in self._bones_title_widgets:                  # Bones: view toggles in the title
            add_title_widget(docks[2], w)
        for dock in docks:                                   # no row taller than 26 px
            for w in dock.findChildren((QLineEdit, QComboBox, QAbstractSpinBox)):
                w.setFixedHeight(24)
            for w in dock.findChildren(QAbstractSpinBox):    # text on the row's centre line
                w.setStyleSheet("padding: 0px 4px;")
            for w in dock.findChildren(QComboBox):           # narrow docks keep their icons
                w.setSizeAdjustPolicy(QComboBox.SizeAdjustPolicy.AdjustToMinimumContentsLengthWithIcon)
                w.setMinimumContentsLength(6)
        self._ped_table.verticalHeader().setDefaultSectionSize(22)
        for w in self._rot_spins + self._pos_spins:            # pose fields shrink with the dock
            w.setMinimumWidth(44)
            w.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)
        self.setWindowTitle(App_name)


def open_ped_workshop(main_window=None, game_root: str = '', file_path: str = ''): #vers 2
    """Open docked in IMG Factory (tab) or standalone; file_path picks a data file / IFP."""
    if main_window is not None and hasattr(main_window, 'main_tab_widget'):
        from PyQt6.QtWidgets import QVBoxLayout, QWidget
        container = QWidget()
        lay = QVBoxLayout(container)
        lay.setContentsMargins(0, 0, 0, 0)
        ws = PedWorkshop(container, main_window)
        ws.setWindowFlags(Qt.WindowType.Widget)
        lay.addWidget(ws)
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        idx = main_window.main_tab_widget.addTab(container, SVGIconFactory.ped_app_icon(20), App_name)
        main_window.main_tab_widget.setCurrentIndex(idx)
    else:
        ws = PedWorkshop(main_window=main_window)       # frameless: custom title bar only
        ws.setWindowTitle(App_name)
        ws.resize(1400, 900)
    ws.show()
    if not game_root and main_window is None:
        game_root = (ws._load_ped_prefs().get('recent_roots') or [''])[0]
    if game_root and os.path.isdir(game_root):
        ws._open_ped_root(game_root)
        if file_path:
            ws._open_ped_file(file_path)
    return ws


if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = open_ped_workshop(None, sys.argv[1] if len(sys.argv) > 1 else '')
    sys.exit(app.exec())
