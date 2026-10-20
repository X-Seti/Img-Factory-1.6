#this belongs in apps/methods/dock_panels.py - Version: 3
# X-Seti - October 14 2026 - IMG Factory 1.6 - Workshop dock panels

"""
Dock panels around a workshop's central view (Model, TXD, Map).
Workshop provides _outer_mw, _outer_docks, _OUTER_LAYOUT_VERSION,
_ribbon_config_path and section_title_font.
"""

##Methods list -
# add_outer_dock
# make_outer_mw
# redock_to_tab

##class DockPanelsMixin: -
# _make_dock_collapsible
# _restore_outer_layout
# _save_outer_layout

import json

from PyQt6.QtCore import QByteArray, Qt
from PyQt6.QtWidgets import (QDockWidget, QHBoxLayout, QLabel, QMainWindow,
                             QToolButton, QWidget)


def make_outer_mw(central): #vers 1
    """Nested QMainWindow with central widget, visible grip separators."""
    from apps.methods.grip_splitter import GripDockSeparators
    mw = QMainWindow()
    mw.setWindowFlags(Qt.WindowType.Widget)
    mw.setDockOptions(QMainWindow.DockOption.AllowNestedDocks |
                      QMainWindow.DockOption.AllowTabbedDocks)
    # local rule beats the parent theme cascade (separator vanished when docked)
    mw.setStyleSheet("QMainWindow::separator { background: palette(mid); width: 5px; height: 5px; } "
                     "QMainWindow::separator:hover { background: palette(highlight); }")
    mw._grip_docks = GripDockSeparators(mw)
    mw.setCentralWidget(central)
    return mw


def add_outer_dock(owner, mw, title, widget, min_width, area=Qt.DockWidgetArea.LeftDockWidgetArea): #vers 1
    """Movable/floatable dock with collapsible title; added to mw."""
    dock = QDockWidget(title, owner)
    dock.setObjectName(title)
    dock.setWidget(widget)
    dock.setMinimumWidth(min_width)
    dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                     QDockWidget.DockWidgetFeature.DockWidgetFloatable)
    mw.addDockWidget(area, dock)
    owner._make_dock_collapsible(dock, title)
    return dock


def redock_to_tab(ws): #vers 1
    """Put a torn-off workshop back in its IMG Factory tab."""
    tabs = getattr(getattr(ws, 'main_window', None), 'main_tab_widget', None)
    holder = ws.parentWidget()
    if tabs is None or holder is None or tabs.indexOf(holder) < 0:
        return False
    ws.setWindowFlags(Qt.WindowType.Widget)
    lay = holder.layout()
    if lay is not None and lay.indexOf(ws) < 0:
        lay.addWidget(ws)
    ws.show()
    tabs.setCurrentIndex(tabs.indexOf(holder))
    return True


class DockPanelsMixin: #vers 1
    """Collapsible dock titles and saved outer dock layout."""

    def _make_dock_collapsible(self, dock, title): #vers 6
        """Title bar: double-click collapses content; close button."""
        #TODO missing splitter between middle panel and right panel, or right panel, and newly placed "middle panel", that was moved to the right of the right panel.
        #X gadgets do not work, and right click menu recovery bar needs work.
        #I have disabled the float/dock function as this is meant to effect it's own bar open/collapse, not the surrounding bars that get collapsed.
        bar = QWidget()
        lay = QHBoxLayout(bar)
        lay.setContentsMargins(6, 2, 2, 2)
        lay.setSpacing(2)
        lbl = QLabel(title)
        lbl.setFont(self.section_title_font)
        dock._title_label = lbl        # callers can update the shown title
        lay.addWidget(lbl)
        lay.addStretch()
        close_btn = QToolButton()
        close_btn.setText("×")
        close_btn.setToolTip("Close (use the View menu or another dock's right-click menu to bring it back)")
        close_btn.setFixedSize(20, 20)
        close_btn.setAutoRaise(True)
        close_btn.clicked.connect(dock.close)
        lay.addWidget(close_btn)

        def _dbl_click(event, d=dock): #vers 1
            content = d.widget()
            if content:
                content.setVisible(not content.isVisible())
        bar.mouseDoubleClickEvent = _dbl_click
        lbl.mouseDoubleClickEvent = _dbl_click
        dock.setTitleBarWidget(bar)

    def _save_outer_layout(self): #vers 3
        """Save _outer_mw dock layout to the workshop config."""
        mw = getattr(self, '_outer_mw', None)
        if mw is None:
            return
        path = self._ribbon_config_path()
        data = json.loads(path.read_text()) if path.exists() else {}
        data['outer_layout_state'] = mw.saveState(self._OUTER_LAYOUT_VERSION).toHex().data().decode()
        data['outer_layout_version'] = self._OUTER_LAYOUT_VERSION
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(data, indent=2))

    def _restore_outer_layout(self): #vers 3
        """Restore saved dock layout; docks forced visible; width clamped to screen."""
        mw = getattr(self, '_outer_mw', None)
        if mw is None:
            return
        path = self._ribbon_config_path()
        if path.exists():
            data = json.loads(path.read_text())
            state_hex = data.get('outer_layout_state')
            if state_hex and data.get('outer_layout_version') == self._OUTER_LAYOUT_VERSION:
                mw.restoreState(QByteArray.fromHex(state_hex.encode()), self._OUTER_LAYOUT_VERSION)
        for dock in getattr(self, '_outer_docks', []):
            dock.setVisible(True)
            dock.toggleViewAction().setChecked(True)
        # saved layout from a wider screen can push the window past this one
        top = self.window()
        screen = top.screen()
        if screen is not None and top.width() > screen.availableGeometry().width():
            top.resize(screen.availableGeometry().width(), top.height())
