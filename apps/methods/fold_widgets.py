#this belongs in apps/methods/fold_widgets.py - Version: 3
# X-Seti - October 24 2026 - IMG Factory 1.6 - Rows that fit narrow panels

"""
Rows that let a panel get narrow while every row stays one row:
ButtonRow drops button text (icons stay, tooltips explain), IconLabel
swaps its text for an icon.
"""

##Methods list -
# fit_height
#
##class _FitHeight: -
# eventFilter
#
##class ButtonRow: -
# __init__
# minimumSizeHint
# resizeEvent
#
##class IconLabel: -
# __init__
# _text_width
# minimumSizeHint
# resizeEvent
# sizeHint

from PyQt6.QtCore import QEvent, QObject, QSize, Qt
from PyQt6.QtWidgets import QHBoxLayout, QLabel, QWidget


class _FitHeight(QObject): #vers 1
    """Caps a widget at its layout's height whenever the layout changes."""

    def eventFilter(self, obj, ev): #vers 1
        if ev.type() == QEvent.Type.LayoutRequest and obj.layout() is not None:
            obj.setMaximumHeight(obj.layout().sizeHint().height())
        return False


def fit_height(widget): #vers 1
    """Widget never taller than its rows (a splitter gives the rest to its neighbour)."""
    widget._fit_height = _FitHeight(widget)
    widget.installEventFilter(widget._fit_height)
    if widget.layout() is not None:
        widget.setMaximumHeight(widget.layout().sizeHint().height())


class ButtonRow(QWidget): #vers 3
    """Tool buttons that drop their text when the row gets narrow."""

    def __init__(self, buttons, parent=None): #vers 1
        super().__init__(parent)
        lay = QHBoxLayout(self)
        lay.setContentsMargins(0, 0, 0, 0)
        self._buttons = buttons
        for b in buttons:
            b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            lay.addWidget(b)
        lay.addStretch(1)
        self._full = sum(b.sizeHint().width() for b in buttons) + 8 * len(buttons)

    def minimumSizeHint(self): #vers 1
        """Icon-only width, so the row never blocks dock splitters."""
        h = super().minimumSizeHint().height()
        return QSize(sum(b.iconSize().width() + 12 for b in self._buttons), h)

    def resizeEvent(self, ev): #vers 1
        style = (Qt.ToolButtonStyle.ToolButtonTextBesideIcon if ev.size().width() >= self._full
                 else Qt.ToolButtonStyle.ToolButtonIconOnly)
        for b in self._buttons:
            b.setToolButtonStyle(style)
        super().resizeEvent(ev)


class IconLabel(QLabel): #vers 1
    """Row label that shows its icon instead of the text when the row is narrow."""

    def __init__(self, text: str, icon, size: int = 16, parent=None): #vers 1
        super().__init__(text, parent)
        self._text, self._icon, self._isz = text, icon, size
        self.setToolTip(text.rstrip(':'))

    def _text_width(self) -> int: #vers 1
        return self.fontMetrics().horizontalAdvance(self._text) + 4

    def sizeHint(self): #vers 1
        """Always asks for the text width, so the label goes back to text when there is room."""
        return QSize(self._text_width(), super().sizeHint().height())

    def minimumSizeHint(self): #vers 1
        return QSize(self._isz + 2, super().minimumSizeHint().height())

    def resizeEvent(self, ev): #vers 1
        if ev.size().width() < self._text_width():
            if self.text():
                self.setPixmap(self._icon.pixmap(self._isz, self._isz))
        elif not self.text():
            self.setText(self._text)
        super().resizeEvent(ev)
