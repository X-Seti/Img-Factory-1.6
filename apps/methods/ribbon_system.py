#this belongs in apps/methods/ribbon_system.py - Version: 5
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
# RibbonMixin.ribbon_side_toolbar
# RibbonMixin.ribbon_fit_side
# RibbonMixin.ribbon_button
# RibbonMixin.ribbon_label
# RibbonMixin.ribbon_context_menu
# RibbonMixin.ribbon_save_state
# RibbonMixin.ribbon_restore_state
# RibbonMixin._ribbon_schedule_save

import json
from pathlib import Path

from PyQt6.QtCore import Qt, QSize, QByteArray
from PyQt6.QtWidgets import QMainWindow, QToolBar, QToolButton, QMenu, QLabel
from apps.methods.img_factory_settings import get_user_config_dir


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

    def ribbon_toolbar(self, name: str) -> QToolBar: #vers 2
        mw = self._ribbon_mw
        tb = QToolBar(name, mw)
        tb.setObjectName(name)
        tb.setIconSize(QSize(self._RIBBON_ICON, self._RIBBON_ICON))
        tb.setMovable(True)
        tb.setFloatable(True)
        tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        tb.customContextMenuRequested.connect(
            lambda pos, t=tb: self.ribbon_context_menu(t, pos))
        tb.topLevelChanged.connect(lambda *_: self._ribbon_schedule_save())    # float / dock
        tb.orientationChanged.connect(lambda *_: self._ribbon_schedule_save())  # moved to a side
        mw.addToolBar(Qt.ToolBarArea.TopToolBarArea, tb)
        return tb

    def _ribbon_schedule_save(self): #vers 1
        """Save the ribbon layout shortly after a ribbon moves."""
        from PyQt6.QtCore import QTimer
        t = getattr(self, '_ribbon_save_timer', None)
        if t is None:
            t = self._ribbon_save_timer = QTimer(self)
            t.setSingleShot(True)
            t.timeout.connect(self.ribbon_save_state)
        t.start(500)

    def ribbon_side_toolbar(self, name: str, widget, width: int = 26,
                            length: int = 0) -> QToolBar: #vers 2
        """Narrow ribbon holding one long widget (gauge, palette); turns with the ribbon.
        length fixes the long side (0 = stretch). Widget may define set_horizontal(bool)."""
        tb = self.ribbon_toolbar(name)
        self._ribbon_mw.addToolBar(Qt.ToolBarArea.RightToolBarArea, tb)
        tb.setStyleSheet("QToolBar { padding: 0px; spacing: 0px; }")
        tb.addWidget(widget)
        tb._side = (widget, width, length)
        tb.orientationChanged.connect(lambda _o, t=tb: self.ribbon_fit_side(t))
        self.ribbon_fit_side(tb)
        return tb

    @staticmethod
    def ribbon_fit_side(tb): #vers 1
        """Size a side ribbon's widget (tb._side = widget, width, length) for its orientation."""
        from PyQt6.QtWidgets import QSizePolicy
        widget, width, length = tb._side
        horiz = tb.orientation() == Qt.Orientation.Horizontal
        thin, fix, grow = width - 4, QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Expanding
        long_min, long_max = (length, length) if length else (60, 16777215)
        if horiz:
            widget.setMinimumSize(long_min, thin); widget.setMaximumSize(long_max, thin)
            widget.setSizePolicy(fix if length else grow, fix)
        else:
            widget.setMinimumSize(thin, long_min); widget.setMaximumSize(thin, long_max)
            widget.setSizePolicy(fix, fix if length else grow)
        if hasattr(widget, 'set_horizontal'):
            widget.set_horizontal(horiz)

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

    def _ribbon_path(self) -> Path: #vers 2
        return get_user_config_dir() / f'{self._ribbon_name}_ribbons.json'

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

    def ribbon_restore_state(self): #vers 2
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
                if hasattr(tb, '_side'):
                    self.ribbon_fit_side(tb)
