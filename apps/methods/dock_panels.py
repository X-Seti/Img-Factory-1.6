#this belongs in apps/methods/dock_panels.py - Version: 16
# X-Seti - October 29 2026 - IMG Factory 1.6 - Workshop dock panels

"""
Dock panels around a workshop's central view (Model, TXD, Map).
Workshop provides _outer_mw, _outer_docks, _OUTER_LAYOUT_VERSION,
_ribbon_config_path and section_title_font. Owner may be a non-widget
layout object with main_window (IMG Factory).
"""

##Methods list -
# add_outer_dock
# add_title_widget
# icon_only_tabs
# make_outer_mw
# redock_to_tab

##class _NarrowWatch: -
# __init__
# _fit
# eventFilter
#
##class _StripLabel: -
# __init__
# paintEvent
#
##class _TabTipFilter: -
# eventFilter
#
##class DockPanelsMixin: -
# _collapse_dock
# _dock_host
# _dock_layout
# _fit_narrow_panels
# _make_dock_collapsible
# _outer_dock_menu
# _panel_flag
# _recolour_dock_icons
# _restore_outer_layout
# _save_outer_layout
# _set_panel_flag
# _set_panels_locked

import json

from PyQt6.QtCore import QByteArray, QEvent, QObject, Qt
from PyQt6.QtWidgets import (QDockWidget, QHBoxLayout, QLabel, QMainWindow, QMenu,
                             QTabBar, QToolButton, QToolTip, QVBoxLayout, QWidget)


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


def add_outer_dock(owner, mw, title, widget, min_width, area=Qt.DockWidgetArea.LeftDockWidgetArea): #vers 3
    """Movable/floatable dock with collapsible title; added to mw."""
    dock = QDockWidget(title, mw)
    dock.setObjectName(title)
    dock.setWidget(widget)
    dock.setMinimumWidth(min_width)
    dock._min_w = min_width                         # restored on expand / unlock
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


class _StripLabel(QWidget): #vers 1
    """Dock title written bottom-to-top on a collapsed panel strip."""

    def __init__(self, text, font=None, parent=None): #vers 1
        super().__init__(parent)
        self._text = text
        if font is not None:
            self.setFont(font)
        self.setMinimumHeight(self.fontMetrics().horizontalAdvance(text) + 12)

    def paintEvent(self, event): #vers 1
        from PyQt6.QtGui import QPainter
        p = QPainter(self)
        p.translate(0, self.height())
        p.rotate(-90)
        p.drawText(0, 0, self.height(), self.width(), Qt.AlignmentFlag.AlignCenter, self._text)


class _NarrowWatch(QObject): #vers 1
    """Outer window resize: collapse / expand side panels by free space."""

    def __init__(self, owner, mw): #vers 1
        super().__init__(mw)
        self._owner = owner
        self._busy = False

    def eventFilter(self, obj, event): #vers 1
        if event.type() == QEvent.Type.Resize and not self._busy:
            from PyQt6.QtCore import QTimer
            self._busy = True
            QTimer.singleShot(60, self._fit)
        return False

    def _fit(self): #vers 1
        self._busy = False
        self._owner._fit_narrow_panels()


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


class DockPanelsMixin: #vers 3
    """Collapsible dock titles and saved outer dock layout."""

    def _dock_host(self): #vers 1
        """Widget owning the docks: the workshop, or IMG Factory's window."""
        return self if isinstance(self, QWidget) else self.main_window

    def _dock_layout(self, centre, docks): #vers 2
        """Centre widget with side panels as docks: [(title, widget, min_w, area)].
        Layout saved on window_closed, else whenever a dock is moved or floated."""
        from PyQt6.QtCore import QTimer
        mw = make_outer_mw(centre)
        self._outer_mw = mw
        self._outer_docks = [add_outer_dock(self, mw, t, w, mn, area) for t, w, mn, area in docks]
        QTimer.singleShot(400, self._restore_outer_layout)
        if hasattr(self, 'window_closed'):
            self.window_closed.connect(self._save_outer_layout)
        else:                                            # panels embedded in IMG Factory
            for d in self._outer_docks:
                d.dockLocationChanged.connect(lambda _a: self._save_outer_layout())
                d.topLevelChanged.connect(lambda _f: self._save_outer_layout())
        return mw

    def _recolour_dock_icons(self): #vers 1
        """Theme change: dock close buttons take the new icon colour."""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        col = self._get_icon_color() if hasattr(self, '_get_icon_color') else None
        for d in self.findChildren(QDockWidget):
            if getattr(d, '_close_btn', None) is not None:
                d._close_btn.setIcon(SVGIconFactory.close_icon(20, col))

    def _make_dock_collapsible(self, dock, title): #vers 16
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
        dock._title_bar = bar
        lbl.setMinimumWidth(20)        # long titles never stop the panel getting narrow
        lay.addWidget(lbl)
        lay.addStretch()
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        from apps.methods.ui_sizes import icon_size
        close_btn = QToolButton()
        col = self._get_icon_color() if hasattr(self, '_get_icon_color') else None
        close_btn.setIcon(SVGIconFactory.close_icon(20, col))        # SVG, theme colour
        close_btn.setIconSize(icon_size('title_icon', self))
        close_btn.setToolTip("Close (use the View menu or another dock's right-click menu to bring it back)")
        close_btn.setFixedSize(size('title_button', self), size('title_button', self))
        dock._close_btn = close_btn
        close_btn.setAutoRaise(True)
        close_btn.clicked.connect(dock.close)
        dock.setFeatures(dock.features() | QDockWidget.DockWidgetFeature.DockWidgetClosable)   # [x] works
        lay.addWidget(close_btn)

        def _dbl_click(event, d=dock): #vers 3
            if d.isFloating():                  # floating: dock back
                d.setFloating(False)
                return
            mw_ = d.parentWidget()
            side = isinstance(mw_, QMainWindow) and mw_.dockWidgetArea(d) in (
                Qt.DockWidgetArea.LeftDockWidgetArea, Qt.DockWidgetArea.RightDockWidgetArea)
            if side:                            # side panel: collapse to an icon strip
                self._collapse_dock(d, not getattr(d, '_collapsed', False))
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

    def _outer_dock_menu(self, global_pos, menu=None): #vers 4
        """Panel show / hide toggles (+ Ribbon Manager); added to menu, else shown at global_pos."""
        own = menu is None
        if own:
            menu = QMenu(self._dock_host())
        for d in getattr(self, '_outer_docks', []):
            a = menu.addAction(d.windowTitle())
            a.setCheckable(True)
            a.setChecked(d.isVisible())
            a.toggled.connect(lambda on, dd=d: (dd.setVisible(on), dd.raise_()) if on else dd.setVisible(False))
        if getattr(self, '_outer_docks', None):
            menu.addSeparator()
            lock = menu.addAction("Lock Panels")
            lock.setCheckable(True)
            lock.setChecked(self._panel_flag('panels_locked'))
            lock.toggled.connect(self._set_panels_locked)
            narrow = menu.addAction("Collapse Panels to Icons when Narrow")
            narrow.setCheckable(True)
            narrow.setChecked(self._panel_flag('panels_autocollapse'))
            narrow.toggled.connect(lambda on: (self._set_panel_flag('panels_autocollapse', on),
                                               self._fit_narrow_panels()))
        if hasattr(self, 'open_ribbon_manager'):
            menu.addSeparator()
            menu.addAction("Ribbon Manager...", self.open_ribbon_manager)
        if hasattr(self, 'open_bindings_editor'):
            menu.addAction("Key && Mouse Bindings...", self.open_bindings_editor)
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

    def _restore_outer_layout(self): #vers 5
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
        if self._panel_flag('panels_locked'):
            self._set_panels_locked(True, save=False)
        if getattr(mw, '_narrow_watch', None) is None:
            mw._narrow_watch = _NarrowWatch(self, mw)
            mw.installEventFilter(mw._narrow_watch)
        # saved layout from a wider screen can push the window past this one
        top = self._dock_host().window()
        screen = top.screen()
        if screen is not None and top.width() > screen.availableGeometry().width():
            top.resize(screen.availableGeometry().width(), top.height())

    def _panel_flag(self, key: str) -> bool: #vers 1
        """Panel option from the workshop config (False when none)."""
        try:
            return bool(json.loads(self._ribbon_config_path().read_text()).get(key, False))
        except (AttributeError, OSError, ValueError):
            return False

    def _set_panel_flag(self, key: str, on: bool): #vers 1
        """Save a panel option to the workshop config."""
        if not hasattr(self, '_ribbon_config_path'):
            return
        path = self._ribbon_config_path()
        try:
            data = json.loads(path.read_text()) if path.exists() else {}
        except ValueError:
            data = {}
        data[key] = bool(on)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(data, indent=2))

    def _set_panels_locked(self, on: bool, save: bool = True): #vers 1
        """Lock: panels stay put at their size. Unlock: move and resize freely."""
        F = QDockWidget.DockWidgetFeature
        mw = getattr(self, '_outer_mw', None)
        for d in getattr(self, '_outer_docks', []):
            if on:
                d.setFeatures(F.DockWidgetClosable)
                side = mw is not None and mw.dockWidgetArea(d) in (
                    Qt.DockWidgetArea.LeftDockWidgetArea, Qt.DockWidgetArea.RightDockWidgetArea)
                if side:
                    d.setFixedWidth(d.width())
                else:
                    d.setFixedHeight(d.height())
            else:
                d.setFeatures(F.DockWidgetMovable | F.DockWidgetFloatable | F.DockWidgetClosable)
                d.setMinimumSize(getattr(d, '_min_w', 0), 0)
                d.setMaximumSize(16777215, 16777215)
                w = d.widget()
                if w is not None and not getattr(d, '_collapsed', False):
                    w.setMaximumSize(16777215, 16777215)      # content max size locked the separator
        if save:
            self._set_panel_flag('panels_locked', on)

    def _collapse_dock(self, dock, on: bool, auto: bool = False): #vers 1
        """Side panel to a narrow icon strip (click to expand) or back."""
        from apps.methods.ui_sizes import size
        mw = dock.parentWidget()
        if not isinstance(mw, QMainWindow) or on == getattr(dock, '_collapsed', False):
            return
        bar = getattr(dock, '_title_bar', None)
        strip_w = size('title_button', self) + 8
        if on:
            dock._expanded_w = dock.width()
            dock._content = dock.widget()
            strip = QWidget()
            lay = QVBoxLayout(strip)
            lay.setContentsMargins(2, 4, 2, 4)
            lay.setSpacing(4)
            btn = QToolButton()
            btn.setAutoRaise(True)
            btn.setToolTip(f"Expand {dock.windowTitle() or getattr(dock, '_tab_tip', '')}")
            icon = dock.windowIcon()
            if icon.isNull():
                from apps.methods.imgfactory_svg_icons import get_panel_toggle_icon
                col = self._get_icon_color() if hasattr(self, '_get_icon_color') else None
                icon = get_panel_toggle_icon(20, col)
            btn.setIcon(icon)
            btn.clicked.connect(lambda: self._collapse_dock(dock, False))
            lay.addWidget(btn, 0, Qt.AlignmentFlag.AlignHCenter)
            title = dock._title_label.text() if hasattr(dock, '_title_label') else dock.windowTitle()
            lay.addWidget(_StripLabel(title, getattr(self, 'section_title_font', None)))
            lay.addStretch()
            dock._content.hide()
            dock.setWidget(strip)
            if bar is not None:
                dock._bar_shown = [w for w in bar.findChildren(QWidget) if not w.isHidden()]
                for w in dock._bar_shown:
                    w.setVisible(False)
            dock.setMinimumWidth(strip_w)
            dock._collapsed, dock._auto_collapsed = True, auto
            mw.resizeDocks([dock], [strip_w], Qt.Orientation.Horizontal)
        else:
            strip = dock.widget()
            dock.setWidget(dock._content)
            dock._content.show()
            if strip is not None:
                strip.deleteLater()
            for w in getattr(dock, '_bar_shown', []):
                w.setVisible(True)
            dock.setMinimumWidth(getattr(dock, '_min_w', strip_w))
            dock._collapsed = dock._auto_collapsed = False
            mw.resizeDocks([dock], [max(getattr(dock, '_expanded_w', 200), dock.minimumWidth())],
                           Qt.Orientation.Horizontal)

    def _fit_narrow_panels(self): #vers 1
        """Auto option: collapse the widest side panel while the centre is cramped."""
        mw = getattr(self, '_outer_mw', None)
        if mw is None or mw.centralWidget() is None or not self._panel_flag('panels_autocollapse'):
            return
        side = [d for d in getattr(self, '_outer_docks', []) if d.isVisible() and not d.isFloating()
                and mw.dockWidgetArea(d) in (Qt.DockWidgetArea.LeftDockWidgetArea,
                                             Qt.DockWidgetArea.RightDockWidgetArea)]
        free = mw.centralWidget().width()
        open_ = [d for d in side if not getattr(d, '_collapsed', False)]
        auto = [d for d in side if getattr(d, '_auto_collapsed', False)]
        if free < 320 and open_:
            self._collapse_dock(max(open_, key=lambda d: d.width()), True, auto=True)
        elif auto:
            d = min(auto, key=lambda d: getattr(d, '_expanded_w', 200))
            if free > 320 + getattr(d, '_expanded_w', 200):
                self._collapse_dock(d, False)

