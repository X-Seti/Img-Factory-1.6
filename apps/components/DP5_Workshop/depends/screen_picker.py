#this belongs in apps/components/DP5_Workshop/depends/screen_picker.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 screen eyedropper.

"""
Screen eyedropper: grabs every screen, shows the grab full-screen with a
magnifier; left click picks FG, right click picks BG, Esc cancels.
Wayland compositors may return a blank grab (Qt limit).
"""

##Methods list -
# pick_from_screen
#
##class ScreenPicker: -
# __init__
# _colour_at
# keyPressEvent
# mouseMoveEvent
# mousePressEvent
# paintEvent

from PyQt6.QtCore import QPoint, QRect, Qt, pyqtSignal
from PyQt6.QtGui import QColor, QCursor, QGuiApplication, QPainter, QPen, QPixmap
from PyQt6.QtWidgets import QWidget

ZOOM = 10                                                # magnifier scale
SPAN = 11                                                # magnified pixels per side


class ScreenPicker(QWidget):
    """Full-screen grab; emits picked(QColor, is_bg)."""

    picked = pyqtSignal(QColor, bool)

    def __init__(self): #vers 1
        super().__init__(None, Qt.WindowType.FramelessWindowHint | Qt.WindowType.WindowStaysOnTopHint |
                         Qt.WindowType.Tool)
        self.setAttribute(Qt.WidgetAttribute.WA_DeleteOnClose)
        geo = QRect()
        for s in QGuiApplication.screens():
            geo = geo.united(s.geometry())
        self._shot = QPixmap(geo.size())
        self._shot.fill(QColor(0, 0, 0))
        p = QPainter(self._shot)
        for s in QGuiApplication.screens():                       # each screen at its desktop offset
            p.drawPixmap(QRect(s.geometry().topLeft() - geo.topLeft(), s.geometry().size()), s.grabWindow(0))
        p.end()
        self._img = self._shot.toImage()
        self._origin = geo.topLeft()
        self.setGeometry(geo)
        self.setMouseTracking(True)
        self.setCursor(Qt.CursorShape.CrossCursor)
        self._pos = self.mapFromGlobal(QCursor.pos())

    def _colour_at(self, pos: QPoint) -> QColor: #vers 1
        if not self._img.rect().contains(pos):
            return QColor()
        return QColor(self._img.pixel(pos))

    def keyPressEvent(self, e): #vers 1
        if e.key() == Qt.Key.Key_Escape:
            self.close()

    def mouseMoveEvent(self, e): #vers 1
        self._pos = e.position().toPoint()
        self.update()

    def mousePressEvent(self, e): #vers 1
        c = self._colour_at(e.position().toPoint())
        if c.isValid():
            self.picked.emit(c, e.button() == Qt.MouseButton.RightButton)
        self.close()

    def paintEvent(self, _): #vers 1
        p = QPainter(self)
        p.drawPixmap(0, 0, self._shot)
        x, y = self._pos.x(), self._pos.y()
        size = SPAN * ZOOM
        bx = x + 20 if x + 20 + size < self.width() else x - 20 - size
        by = y + 20 if y + 20 + size + 22 < self.height() else y - 20 - size - 22
        src = QRect(x - SPAN // 2, y - SPAN // 2, SPAN, SPAN)
        p.drawPixmap(QRect(bx, by, size, size), self._shot, src)
        p.setPen(QPen(QColor(255, 255, 255), 1))
        p.drawRect(bx, by, size, size)
        mid = (SPAN // 2) * ZOOM
        p.setPen(QPen(QColor(255, 0, 0), 1))
        p.drawRect(bx + mid, by + mid, ZOOM, ZOOM)
        c = self._colour_at(self._pos)
        p.fillRect(bx, by + size, size, 22, QColor(0, 0, 0))
        if c.isValid():
            p.fillRect(bx + 3, by + size + 3, 16, 16, c)
            p.setPen(QColor(255, 255, 255))
            p.drawText(bx + 24, by + size + 16, f"{c.name()}  {c.red()},{c.green()},{c.blue()}")


def pick_from_screen(owner): #vers 1
    """Open the picker; the pick goes to owner's FG (left) or BG (right) swatch."""
    picker = ScreenPicker()
    picker.picked.connect(lambda c, bg: owner._fgbg_swatch.set_bg(c) if bg else owner._fgbg_swatch.set_fg(c))
    owner._screen_picker = picker
    picker.show()
    picker.activateWindow()
    picker.setFocus()
