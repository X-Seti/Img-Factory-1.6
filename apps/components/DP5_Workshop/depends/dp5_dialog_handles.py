#this belongs in apps/components/DP5_Workshop/depends/dp5_dialog_handles.py - Version: 1
# X-Seti - November 16 2026 - IMG Factory 1.6 - DP5 canvas handles for Perspective and Camera dialogs

"""
Canvas handles that drive the Perspective and Camera move dialogs: corner pins for the turned
brush, and start / end view boxes for the camera. The dialog stays open, its values follow the handles.
"""

##class CameraHandles: -
# __init__
# move
# paint
# press
# release

##class PasteHandles: -
# __init__
# move
# paint
# press
# release

##class PerspectiveHandles: -
# __init__
# move
# paint
# press
# release

from PyQt6.QtCore import QPoint, Qt
from PyQt6.QtGui import QColor, QPen, QPolygon


def _near(c, x, y, wp) -> bool: #vers 1
    p = c._tex_to_widget(round(x), round(y))
    return abs(p.x() - wp.x()) <= 8 and abs(p.y() - wp.y()) <= 8


def _knob(c, painter, x, y): #vers 1
    p = c._tex_to_widget(round(x), round(y))
    painter.setPen(QPen(QColor('#000000'), 1))
    painter.setBrush(QColor('#40c8d8'))
    painter.drawRect(p.x() - 4, p.y() - 4, 8, 8)


class PerspectiveHandles: #vers 1
    """Four corner pins on the turned brush."""

    def __init__(self, dlg): #vers 1
        self.d, self.drag = dlg, None

    def press(self, c, wp, tx, ty) -> bool: #vers 1
        for i, (x, y) in enumerate(self.d.quad_points()):
            if _near(c, x, y, wp):
                self.drag = i
                break
        return True

    def move(self, c, tx, ty): #vers 1
        if self.drag is not None:
            self.d.set_corner(self.drag, tx, ty)

    def release(self, c): #vers 1
        self.drag = None

    def paint(self, c, painter): #vers 1
        pts = self.d.quad_points()
        painter.setPen(QPen(QColor('#40c8d8'), 1, Qt.PenStyle.DashLine))
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.drawPolygon(QPolygon([c._tex_to_widget(round(x), round(y)) for x, y in pts]))
        for x, y in pts:
            _knob(c, painter, x, y)


class CameraHandles: #vers 1
    """Start and end view boxes: drag the end centre to scroll, a box corner to zoom."""

    def __init__(self, dlg, canvas): #vers 1
        self.d, self.c, self.drag = dlg, canvas, None

    def _geo(self): #vers 1
        w, h = self.c.tex_w, self.c.tex_h
        d = self.d
        z0, z1 = d._z0.value(), d._z1.value()
        ex, ey = w / 2.0 + d._dx.value(), h / 2.0 + d._dy.value()
        return w, h, (w / 2.0, h / 2.0, w / (2 * z0), h / (2 * z0)), (ex, ey, w / (2 * z1), h / (2 * z1))

    def _handles(self) -> dict: #vers 1
        w, h, s, e = self._geo()
        return {'z0': (s[0] + s[2], s[1] + s[3]), 'move': (e[0], e[1]), 'z1': (e[0] + e[2], e[1] + e[3])}

    def press(self, c, wp, tx, ty) -> bool: #vers 1
        for k, (x, y) in self._handles().items():
            if _near(c, x, y, wp):
                self.drag = k
                break
        return True

    def move(self, c, tx, ty): #vers 1
        w, h, s, e = self._geo()
        d = self.d
        if self.drag == 'move':
            d._dx.setValue(int(tx - w / 2.0))
            d._dy.setValue(int(ty - h / 2.0))
        elif self.drag == 'z0':
            d._z0.setValue(max(0.1, min(20.0, w / (2.0 * max(1.0, tx - s[0])))))
        elif self.drag == 'z1':
            d._z1.setValue(max(0.1, min(20.0, w / (2.0 * max(1.0, tx - e[0])))))
        c.update()

    def release(self, c): #vers 1
        self.drag = None

    def paint(self, c, painter): #vers 1
        w, h, s, e = self._geo()
        for (cx, cy, hw, hh), col in ((s, '#9aa0aa'), (e, '#40c8d8')):
            a = c._tex_to_widget(round(cx - hw), round(cy - hh))
            b = c._tex_to_widget(round(cx + hw), round(cy + hh))
            painter.setPen(QPen(QColor(col), 1, Qt.PenStyle.DashLine))
            painter.setBrush(Qt.BrushStyle.NoBrush)
            painter.drawRect(a.x(), a.y(), b.x() - a.x(), b.y() - a.y())
        a = c._tex_to_widget(round(s[0]), round(s[1]))
        b = c._tex_to_widget(round(e[0]), round(e[1]))
        painter.setPen(QPen(QColor('#40c8d8'), 1))
        painter.drawLine(a, b)
        for x, y in self._handles().values():
            _knob(c, painter, x, y)


class PasteHandles: #vers 1
    """Drag the box to place the pasted anim brush."""

    def __init__(self, dlg, canvas, bw: int, bh: int): #vers 1
        self.d, self.c, self.bw, self.bh, self.grab = dlg, canvas, bw, bh, None

    def press(self, c, wp, tx, ty) -> bool: #vers 1
        x, y = self.d._x.value(), self.d._y.value()
        if x <= tx < x + self.bw and y <= ty < y + self.bh:
            self.grab = (tx - x, ty - y)
        return True

    def move(self, c, tx, ty): #vers 1
        if self.grab:
            self.d._x.setValue(int(tx - self.grab[0]))
            self.d._y.setValue(int(ty - self.grab[1]))

    def release(self, c): #vers 1
        self.grab = None

    def paint(self, c, painter): #vers 1
        x, y = self.d._x.value(), self.d._y.value()
        a = c._tex_to_widget(x, y)
        b = c._tex_to_widget(x + self.bw, y + self.bh)
        painter.setPen(QPen(QColor('#40c8d8'), 1, Qt.PenStyle.DashLine))
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.drawRect(a.x(), a.y(), b.x() - a.x(), b.y() - a.y())
        _knob(c, painter, x, y)
