#this belongs in apps/methods/flow_layout.py - Version: 1
# X-Seti - November 07 2026 - IMG Factory 1.6 - Wrapping button row

"""
FlowLayout: widgets left to right, wrapping onto the next line when the
panel is narrow (button rows in narrow docks keep every button visible).
"""

##class FlowLayout: -
# __init__
# _do_layout
# addItem
# count
# expandingDirections
# hasHeightForWidth
# heightForWidth
# itemAt
# minimumSize
# setGeometry
# sizeHint
# takeAt

from PyQt6.QtCore import QPoint, QRect, QSize, Qt
from PyQt6.QtWidgets import QLayout


class FlowLayout(QLayout): #vers 1
    """Row of widgets that wraps."""

    def __init__(self, parent=None, spacing: int = 2): #vers 1
        super().__init__(parent)
        self._items = []
        self.setSpacing(spacing)
        self.setContentsMargins(0, 0, 0, 0)

    def addItem(self, item): #vers 1
        self._items.append(item)

    def count(self) -> int: #vers 1
        return len(self._items)

    def itemAt(self, i): #vers 1
        return self._items[i] if 0 <= i < len(self._items) else None

    def takeAt(self, i): #vers 1
        return self._items.pop(i) if 0 <= i < len(self._items) else None

    def expandingDirections(self): #vers 1
        return Qt.Orientation(0)

    def hasHeightForWidth(self) -> bool: #vers 1
        return True

    def heightForWidth(self, w: int) -> int: #vers 1
        return self._do_layout(QRect(0, 0, w, 0), True)

    def setGeometry(self, rect): #vers 1
        super().setGeometry(rect)
        self._do_layout(rect, False)

    def sizeHint(self) -> QSize: #vers 1
        w = sum(i.sizeHint().width() for i in self._items) + self.spacing() * max(0, len(self._items) - 1)
        h = max((i.sizeHint().height() for i in self._items), default=0)
        return QSize(w, h)

    def minimumSize(self) -> QSize: #vers 1
        s = QSize()
        for i in self._items:
            s = s.expandedTo(i.minimumSize())
        m = self.contentsMargins()
        return s + QSize(m.left() + m.right(), m.top() + m.bottom())

    def _do_layout(self, rect, test: bool) -> int: #vers 1
        """Place items in lines; returns the height used."""
        x, y, line = rect.x(), rect.y(), 0
        sp = self.spacing()
        for item in self._items:
            if item.widget() is not None and item.widget().isHidden():
                continue
            hint = item.sizeHint()
            if x + hint.width() > rect.right() + 1 and line > 0:
                x, y, line = rect.x(), y + line + sp, 0
            if not test:
                item.setGeometry(QRect(QPoint(x, y), hint))
            x += hint.width() + sp
            line = max(line, hint.height())
        return y + line - rect.y()
