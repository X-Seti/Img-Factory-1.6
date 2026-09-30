#this belongs in apps/methods/grip_splitter.py - Version: 2
# X-Seti - Sept 29 2026 - IMG Factory 1.6 - Grip Splitter

"""
Grip Splitter - QSplitter whose handles draw the toolbar/ribbon grip,
plus saved splitter sizes for workshops.
"""

##Methods list -

##class GripSplitter: -
# __init__
# createHandle

##class GripSplitterHandle: -
# paintEvent
# sizeHint

##class SplitterSizesMixin: -
# _queue_splitter_save
# _restore_splitter_sizes
# _save_splitter_sizes
# _splitter_key

import json

from PyQt6.QtCore import Qt, QSize, QTimer
from PyQt6.QtGui import QPainter
from PyQt6.QtWidgets import QSplitter, QSplitterHandle, QStyle, QStyleOption


class GripSplitterHandle(QSplitterHandle): #vers 1
    """Splitter handle painted with the style's toolbar grip."""

    def paintEvent(self, event): #vers 1
        """Draw ribbon-style grip centred on the handle."""
        opt = QStyleOption()
        opt.initFrom(self)
        horiz = self.orientation() == Qt.Orientation.Horizontal
        r = self.rect()
        if horiz:  # vertical bar between left/right panes
            opt.rect = r.adjusted(0, r.height() // 2 - 20, 0, -(r.height() // 2 - 20))
        else:
            opt.rect = r.adjusted(r.width() // 2 - 20, 0, -(r.width() // 2 - 20), 0)
        if not horiz:
            opt.state |= QStyle.StateFlag.State_Horizontal
        if self.underMouse():
            opt.state |= QStyle.StateFlag.State_MouseOver
        p = QPainter(self)
        self.style().drawPrimitive(QStyle.PrimitiveElement.PE_IndicatorToolBarHandle, opt, p, self)
        p.end()

    def sizeHint(self): #vers 1
        """Handle thick enough to show the grip."""
        s = super().sizeHint()
        return QSize(max(s.width(), 8), max(s.height(), 8))


class GripSplitter(QSplitter): #vers 1
    """QSplitter using GripSplitterHandle for every handle."""

    def __init__(self, *args): #vers 1
        super().__init__(*args)
        self.setHandleWidth(8)

    def createHandle(self): #vers 1
        """Return grip handle for this splitter."""
        return GripSplitterHandle(self.orientation(), self)


__all__ = ['GripSplitter', 'GripSplitterHandle']


class SplitterSizesMixin: #vers 1
    """Save/restore self._main_splitter sizes in the workshop config.
    Workshop provides _ribbon_config_path, _apply_left_compact, _SPLITTER_LIST_PX."""

    _SPLITTER_LIST_PX = 220

    def _splitter_key(self): #vers 1
        """Config key; docked and standalone have different panel counts."""
        return f"splitter_sizes_{self._main_splitter.count()}"

    def _queue_splitter_save(self): #vers 1
        """Save sizes 500 ms after the last splitter drag."""
        if not hasattr(self, '_splitter_save_timer'):
            self._splitter_save_timer = QTimer(self)
            self._splitter_save_timer.setSingleShot(True)
            self._splitter_save_timer.timeout.connect(self._save_splitter_sizes)
        self._splitter_save_timer.start(500)

    def _restore_splitter_sizes(self): #vers 1
        """Restore saved sizes; default list pane _SPLITTER_LIST_PX."""
        sp = getattr(self, '_main_splitter', None)
        if sp is None:
            return
        try:
            sizes = json.loads(self._ribbon_config_path().read_text()).get(self._splitter_key())
        except (OSError, ValueError):
            sizes = None
        if not sizes or len(sizes) != sp.count():
            total = max(sp.width(), 800)
            lp = self._SPLITTER_LIST_PX
            sizes = [lp, total - lp] if sp.count() == 2 else [180, lp, total - 180 - lp]
        sp.setSizes(sizes)
        QTimer.singleShot(0, self._apply_left_compact)

    def _save_splitter_sizes(self): #vers 1
        """Write main splitter sizes to the workshop config."""
        sp = getattr(self, '_main_splitter', None)
        if sp is None:
            return
        path = self._ribbon_config_path()
        try:
            data = json.loads(path.read_text())
        except (OSError, ValueError):
            data = {}
        data[self._splitter_key()] = sp.sizes()
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(data, indent=2))
