#this belongs in apps/methods/dock_panels.py - Version: 11
# X-Seti - October 14 2026 - IMG Factory 1.6 - Workshop dock panels

"""
Dock panels around a workshop's central view (Model, TXD, Map).
Workshop provides _outer_mw, _outer_docks, _OUTER_LAYOUT_VERSION,
_ribbon_config_path and section_title_font.
"""

##Methods list -
# add_outer_dock
# add_title_widget
# icon_only_tabs
# make_outer_mw
# redock_to_tab

##class _TabTipFilter: -
# eventFilter
#
##class DockPanelsMixin: -
# _make_dock_collapsible
# _outer_dock_menu
# _restore_outer_layout
# _save_outer_layout

import json

from PyQt6.QtCore import QByteArray, QEvent, QObject, Qt
from PyQt6.QtWidgets import (QDockWidget, QHBoxLayout, QLabel, QMainWindow, QMenu,
                             QTabBar, QToolButton, QToolTip, QWidget)


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


def add_title_widget(dock, widget, index=None): #vers 4
    """Put a widget into a collapsible dock's title bar (default: before close)."""
    from apps.methods.ui_sizes import icon_size, size
    b = size('title_button', dock)
    if isinstance(widget, QToolButton):          # fits the dock title bar
        widget.setFixedSize(b, b)
        widget.setIconSize(icon_size('title_icon', dock))
    else:
        widget.setMaximumHeight(b)
    lay = dock.titleBarWidget().layout()
    lay.insertWidget(lay.count() - 1 if index is None else index, widget)


class _TabTipFilter(QObject): #vers 1
    """Tooltips for icon-only dock tabs; hooks new tab bars."""

    def eventFilter(self, obj, event): #vers 1
        if event.type() == QEvent.Type.ChildAdded and isinstance(event.child(), QTabBar):
            event.child().installEventFilter(self)
        elif event.type() == QEvent.Type.ToolTip and isinstance(obj, QTabBar):
            from PyQt6 import sip
            i = obj.tabAt(event.pos())
            for d in self.parent().findChildren(QDockWidget):
                if i >= 0 and obj.tabData(i) == sip.unwrapinstance(d) and getattr(d, '_tab_tip', ''):
                    QToolTip.showText(event.globalPos(), d._tab_tip, obj)
                    return True
        return False


def icon_only_tabs(mw, docks_icons): #vers 1
    """Tabbed docks show only an icon; title becomes the tooltip."""
    for dock, icon in docks_icons:
        title = getattr(dock, '_title_label', None)
        dock._tab_tip = title.text() if title else dock.windowTitle()
        dock.setWindowIcon(icon)
        dock.setWindowTitle('')
        dock.toggleViewAction().setText(dock._tab_tip)       # View menu keeps the name
    if getattr(mw, '_tab_tip_filter', None) is None:
        mw._tab_tip_filter = _TabTipFilter(mw)
        mw.installEventFilter(mw._tab_tip_filter)
        for bar in mw.findChildren(QTabBar):
            bar.installEventFilter(mw._tab_tip_filter)
    from PyQt6 import sip
    ptrs = {sip.unwrapinstance(d): d for d, _i in docks_icons}
    for bar in mw.findChildren(QTabBar):                    # tabs made before the icon was set
        for i in range(bar.count()):
            d = ptrs.get(bar.tabData(i))
            if d is not None:
                bar.setTabIcon(i, d.windowIcon())


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

    def _make_dock_collapsible(self, dock, title): #vers 14
        """Title bar: double-click collapses, or re-docks when floating; close."""
        #TODO missing splitter between middle panel and right panel, or right panel, and newly placed "middle panel", that was moved to the right of the right panel.
        #X gadgets do not work, and right click menu recovery bar needs work.
        #I have disabled the float/dock function as this is meant to effect it's own bar open/collapse, not the surrounding bars that get collapsed.
        from apps.methods.ui_sizes import size
        bar = QWidget()
        bar.setFixedHeight(size('dock_title', self))  # same header height on every panel
        lay = QHBoxLayout(bar)
        lay.setContentsMargins(6, 2, 2, 2)
        lay.setSpacing(2)
        lbl = QLabel(title)
        lbl.setFont(self.section_title_font)
        dock._title_label = lbl        # callers can update the shown title
        lbl.setMinimumWidth(20)        # long titles never stop the panel getting narrow
        lay.addWidget(lbl)
        lay.addStretch()
        close_btn = QToolButton()
        close_btn.setText("×")
        close_btn.setToolTip("Close (use the View menu or another dock's right-click menu to bring it back)")
        close_btn.setFixedSize(size('title_button', self), size('title_button', self))
        close_btn.setAutoRaise(True)
        close_btn.clicked.connect(dock.close)
        dock.setFeatures(dock.features() | QDockWidget.DockWidgetFeature.DockWidgetClosable)   # [x] works
        lay.addWidget(close_btn)

        def _dbl_click(event, d=dock): #vers 2
            if d.isFloating():                  # floating: dock back
                d.setFloating(False)
                return
            content = d.widget()
            if content:
                content.setVisible(not content.isVisible())
        bar.mouseDoubleClickEvent = _dbl_click
        lbl.mouseDoubleClickEvent = _dbl_click
        for w in (bar, dock):                           # right-click title / panel: reopen panels
            w.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            w.customContextMenuRequested.connect(lambda pos, ww=w: self._outer_dock_menu(ww.mapToGlobal(pos)))
        mw = dock.parentWidget()
        if isinstance(mw, QMainWindow) and not getattr(mw, '_panel_menu', False):   # separators / empty areas
            mw._panel_menu = True
            mw.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            mw.customContextMenuRequested.connect(lambda pos, m=mw: self._outer_dock_menu(m.mapToGlobal(pos)))
        dock.setTitleBarWidget(bar)

    def _outer_dock_menu(self, global_pos, menu=None): #vers 1
        """Panel show / hide toggles (+ Ribbon Manager); added to menu, else shown at global_pos."""
        own = menu is None
        if own:
            menu = QMenu(self)
        for d in getattr(self, '_outer_docks', []):
            a = menu.addAction(d.windowTitle())
            a.setCheckable(True)
            a.setChecked(d.isVisible())
            a.toggled.connect(lambda on, dd=d: (dd.setVisible(on), dd.raise_()) if on else dd.setVisible(False))
        if hasattr(self, 'open_ribbon_manager'):
            menu.addSeparator()
            menu.addAction("Ribbon Manager...", self.open_ribbon_manager)
        if own:
            menu.exec(global_pos)

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
