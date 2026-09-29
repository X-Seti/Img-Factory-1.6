#this belongs in apps/methods/grip_splitter.py - Version: 1
# X-Seti - Sept 29 2026 - IMG Factory 1.6 - Grip Splitter

"""
Grip Splitter - QSplitter whose handles draw the toolbar/ribbon grip.
"""

##Methods list -

##class GripSplitter: -
# __init__
# createHandle

##class GripSplitterHandle: -
# paintEvent
# sizeHint

from PyQt6.QtCore import Qt, QSize
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
