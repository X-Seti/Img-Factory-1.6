#this belongs in apps/methods/ribbon_system.py - Version: 1
# X-Seti - September 20 2026 - IMG Factory 1.6 - Shared ribbon (toolbar) system

"""ribbon_system.py - Ribbon toolbars for QWidget-based workshops (same
pattern as Asset Workshop / Master IDE): an inner QMainWindow holds the
workshop's content as its central widget and the tool buttons live in
movable QToolBar ribbons. Layout is saved/restored per workshop in
~/.config/imgfactory/<name>_ribbons.json (separate file so it never
clashes with the workshop's own settings json) and a saved layout from
an older ribbon structure is rejected via _RIBBON_LAYOUT_VERSION."""

##Methods list -
# RibbonMixin.ribbon_wrap
# RibbonMixin.ribbon_toolbar
# RibbonMixin.ribbon_button
# RibbonMixin.ribbon_label
# RibbonMixin.ribbon_context_menu
# RibbonMixin.ribbon_save_state
# RibbonMixin.ribbon_restore_state

import json
from pathlib import Path

from PyQt6.QtCore import Qt, QSize, QByteArray
from PyQt6.QtWidgets import QMainWindow, QToolBar, QToolButton, QMenu, QLabel


class RibbonMixin:
    """Mix into a workshop QWidget. Set _ribbon_name (file stem) and bump
    _RIBBON_LAYOUT_VERSION whenever the set of ribbons changes."""
    _ribbon_name = "workshop"
    _RIBBON_LAYOUT_VERSION = 1
    _RIBBON_ICON = 20
    _RIBBON_BTN = 30

    def ribbon_wrap(self, central) -> QMainWindow: #vers 1
        """Inner QMainWindow that hosts `central` plus the ribbons."""
        mw = QMainWindow()
        mw.setWindowFlags(Qt.WindowType.Widget)
        mw.setCentralWidget(central)
        self._ribbon_mw = mw
        return mw

    def ribbon_toolbar(self, name: str) -> QToolBar: #vers 1
        mw = self._ribbon_mw
        tb = QToolBar(name, mw)
        tb.setObjectName(name)
        tb.setIconSize(QSize(self._RIBBON_ICON, self._RIBBON_ICON))
        tb.setMovable(True)
        tb.setFloatable(True)
        tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        tb.customContextMenuRequested.connect(
            lambda pos, t=tb: self.ribbon_context_menu(t, pos))
        mw.addToolBar(Qt.ToolBarArea.TopToolBarArea, tb)
        return tb

    def ribbon_button(self, tb, icon_fn, tip, slot, checkable=False, enabled=True,
                      text=None) -> QToolButton: #vers 1
        """Icon button in a ribbon. icon_fn is an SVGIconFactory method
        name. Returns the QToolButton (setChecked/setEnabled work as on
        any button, so existing tool-state code keeps working)."""
        b = QToolButton()
        b.setFixedSize(self._RIBBON_BTN, self._RIBBON_BTN)
        b.setIconSize(QSize(self._RIBBON_ICON, self._RIBBON_ICON))
        b.setToolTip(tip)
        b.setCheckable(checkable)
        b.setEnabled(enabled)
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            color = self._get_icon_color() if hasattr(self, '_get_icon_color') else None
            b.setIcon(getattr(SVGIconFactory, icon_fn)(self._RIBBON_ICON, color))
        except Exception:
            b.setText(text or tip[:2])
        if text and b.icon().isNull():
            b.setText(text)
        b.clicked.connect(slot)
        tb.addWidget(b)
        return b

    def ribbon_label(self, tb, text: str) -> QLabel: #vers 1
        lbl = QLabel(text)
        lbl.setStyleSheet("padding: 0 4px;")
        tb.addWidget(lbl)
        return lbl

    def ribbon_context_menu(self, toolbar, pos): #vers 1
        menu = QMenu(self)
        menu.addAction("Save Ribbon Config", self.ribbon_save_state)
        menu.addSeparator()
        menu.addAction("Lock All Toolbars", lambda: self._ribbon_set_movable(False))
        menu.addAction("Unlock All Toolbars", lambda: self._ribbon_set_movable(True))
        menu.exec(toolbar.mapToGlobal(pos))

    def _ribbon_set_movable(self, movable: bool): #vers 1
        for tb in self._ribbon_mw.findChildren(QToolBar):
            tb.setMovable(movable)

    def _ribbon_path(self) -> Path: #vers 1
        return Path.home() / '.config' / 'imgfactory' / f'{self._ribbon_name}_ribbons.json'

    def ribbon_save_state(self): #vers 1
        mw = getattr(self, '_ribbon_mw', None)
        if mw is None:
            return
        try:
            path = self._ribbon_path()
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(json.dumps({
                'toolbar_state': mw.saveState(self._RIBBON_LAYOUT_VERSION).toHex().data().decode(),
                'toolbar_state_version': self._RIBBON_LAYOUT_VERSION,
            }, indent=2))
            if hasattr(self, '_set_status'):
                self._set_status("Ribbon config saved")
        except Exception as e:
            print(f"[{self._ribbon_name}] ribbon_save_state error: {e}")

    def ribbon_restore_state(self): #vers 1
        """Restore the saved layout; rejects an older ribbon structure and
        always forces every ribbon visible afterwards so a bad state can
        never leave one hidden with no way back."""
        mw = getattr(self, '_ribbon_mw', None)
        if mw is None:
            return
        try:
            path = self._ribbon_path()
            if path.exists():
                data = json.loads(path.read_text())
                if data.get('toolbar_state') and \
                        data.get('toolbar_state_version') == self._RIBBON_LAYOUT_VERSION:
                    mw.restoreState(QByteArray.fromHex(data['toolbar_state'].encode()),
                                    self._RIBBON_LAYOUT_VERSION)
        except Exception as e:
            print(f"[{self._ribbon_name}] ribbon_restore_state error: {e}")
        finally:
            for tb in mw.findChildren(QToolBar):
                tb.setVisible(True)
