#this belongs in apps/components/DP5_Workshop/depends/rulers_widget.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 canvas rulers and guides.

"""
Pixel rulers on the top and left edges of the DP5 canvas scroll area.
Drag from a ruler onto the canvas to place a guide; drag a guide's mark
on the ruler off the canvas to remove it. Guides live on the canvas
(canvas.guides['v'] / ['h'], tex pixel edges) and tools snap to them.
"""

##Methods list -
# install_rulers
# set_rulers_visible
#
##class CanvasRuler: -
# __init__
# _canvas_geom
# _guide_at
# _tex_at
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# paintEvent
#
##class _RulerFilter: -
# __init__
# eventFilter

from PyQt6.QtCore import QEvent, QObject, QPoint, Qt
from PyQt6.QtGui import QColor, QCursor, QFont, QPainter, QPen
from PyQt6.QtWidgets import QWidget

RULER = 20                                               # ruler thickness px
GUIDE_COLOUR = QColor(0, 200, 255)
_STEPS = (1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 5000)


class CanvasRuler(QWidget):
    """One edge ruler: 'h' (top, x axis) or 'v' (left, y axis)."""

    def __init__(self, owner, orient: str, parent=None): #vers 1
        super().__init__(parent)
        self._owner = owner
        self._orient = orient
        self._drag = None                                # guide index being dragged, or 'new'
        self.setMouseTracking(True)
        self.setCursor(Qt.CursorShape.SplitVCursor if orient == 'h' else Qt.CursorShape.SplitHCursor)
        self.setToolTip("Drag onto the canvas to add a guide; drag a guide off to remove it")

    def _canvas_geom(self): #vers 1
        """(canvas origin along this axis in ruler px, pixels per tex px)."""
        c = self._owner.dp5_canvas
        origin = c.mapTo(self.parentWidget(), QPoint(0, 0))
        mine = self.mapTo(self.parentWidget(), QPoint(0, 0))
        z = max(0.01, c.zoom)
        if self._orient == 'h':
            return origin.x() - mine.x(), z * c.pixel_aspect_x
        return origin.y() - mine.y(), z

    def _tex_at(self, global_pos) -> int: #vers 1
        """Nearest tex pixel edge under a global position, on this axis."""
        c = self._owner.dp5_canvas
        p = c.mapFromGlobal(global_pos)
        z = max(0.01, c.zoom)
        if self._orient == 'h':
            return int(round(p.x() / (z * c.pixel_aspect_x)))
        return int(round(p.y() / z))

    def _guide_at(self, pos) -> int: #vers 1
        """Index of the guide mark within 4 px of pos on this ruler, or -1."""
        c = self._owner.dp5_canvas
        off, scale = self._canvas_geom()
        at = pos.x() if self._orient == 'h' else pos.y()
        key = 'v' if self._orient == 'h' else 'h'       # top ruler places vertical guides
        for i, g in enumerate(c.guides[key]):
            if abs(off + g * scale - at) <= 4:
                return i
        return -1

    def mousePressEvent(self, e): #vers 1
        if e.button() != Qt.MouseButton.LeftButton or not self._owner.dp5_canvas:
            return
        i = self._guide_at(e.position().toPoint())
        self._drag = i if i >= 0 else 'new'

    def mouseMoveEvent(self, e): #vers 1
        c = self._owner.dp5_canvas
        if self._drag is None or not c:
            self.update()
            return
        c._guide_drag = ('v' if self._orient == 'h' else 'h', self._tex_at(e.globalPosition().toPoint()))
        c.update()

    def mouseReleaseEvent(self, e): #vers 1
        c = self._owner.dp5_canvas
        if self._drag is None or not c:
            return
        key = 'v' if self._orient == 'h' else 'h'
        on_canvas = c.rect().contains(c.mapFromGlobal(e.globalPosition().toPoint()))
        if self._drag != 'new':
            del c.guides[key][self._drag]
        if on_canvas:
            limit = c.tex_w if key == 'v' else c.tex_h
            c.guides[key].append(max(0, min(limit, self._tex_at(e.globalPosition().toPoint()))))
        self._drag = None
        c._guide_drag = None
        c.update()
        self.update()

    def paintEvent(self, _): #vers 1
        c = self._owner.dp5_canvas
        p = QPainter(self)
        pal = self.palette()
        p.fillRect(self.rect(), pal.window())
        if not c:
            return
        off, scale = self._canvas_geom()
        length = self.width() if self._orient == 'h' else self.height()
        step = next((s for s in _STEPS if s * scale >= 50), _STEPS[-1])
        minor = step / 5 if step >= 5 else step
        p.setFont(QFont(self.font().family(), 7))
        p.setPen(QPen(pal.windowText().color(), 1))
        first = int(-off / scale // minor) * minor
        t = first
        while off + t * scale <= length:
            at = int(off + t * scale)
            if at >= 0:
                major = abs(t / step - round(t / step)) < 1e-6
                tick = RULER - 2 if major else RULER // 4
                if self._orient == 'h':
                    p.drawLine(at, RULER - tick, at, RULER)
                    if major:
                        p.drawText(at + 2, 9, str(int(t)))
                else:
                    p.drawLine(RULER - tick, at, RULER, at)
                    if major:
                        p.save()
                        p.translate(9, at + 2)
                        p.rotate(90)
                        p.drawText(0, 0, str(int(t)))
                        p.restore()
            t += minor
        p.setPen(QPen(GUIDE_COLOUR, 2))                  # guide marks
        for g in c.guides['v' if self._orient == 'h' else 'h']:
            at = int(off + g * scale)
            if self._orient == 'h':
                p.drawLine(at, 0, at, RULER)
            else:
                p.drawLine(0, at, RULER, at)
        cur = c.mapFromGlobal(QCursor.pos())             # cursor position marker
        if c.rect().contains(cur):
            p.setPen(QPen(QColor(255, 60, 60), 1))
            at = int(off + (cur.x() if self._orient == 'h' else cur.y()))
            if self._orient == 'h':
                p.drawLine(at, 0, at, RULER)
            else:
                p.drawLine(0, at, RULER, at)


class _RulerFilter(QObject):
    """Repaints the rulers when the canvas moves, resizes, zooms or the mouse moves."""

    def __init__(self, rulers, place, viewport, parent=None): #vers 1
        super().__init__(parent)
        self._rulers = rulers
        self._place = place
        self._viewport = viewport

    def eventFilter(self, obj, ev): #vers 1
        if obj is self._viewport and ev.type() == QEvent.Type.Resize and self._rulers[0].isVisible():
            self._place()
        if ev.type() in (QEvent.Type.Move, QEvent.Type.Resize, QEvent.Type.MouseMove, QEvent.Type.Leave):
            for r in self._rulers:
                r.update()
        return False


def install_rulers(owner): #vers 1
    """Top / left rulers in owner._canvas_scroll margins; owner._rulers."""
    sa = owner._canvas_scroll
    top = CanvasRuler(owner, 'h', sa)
    left = CanvasRuler(owner, 'v', sa)
    corner = QWidget(sa)
    corner.setAutoFillBackground(True)
    owner._rulers = (top, left, corner)

    def place():
        vp = sa.viewport().geometry()
        top.setGeometry(vp.x(), vp.y() - RULER, vp.width(), RULER)
        left.setGeometry(vp.x() - RULER, vp.y(), RULER, vp.height())
        corner.setGeometry(vp.x() - RULER, vp.y() - RULER, RULER, RULER)
    owner._place_rulers = place
    filt = _RulerFilter((top, left), place, sa.viewport(), sa)
    owner.dp5_canvas.installEventFilter(filt)
    sa.viewport().installEventFilter(filt)
    owner._ruler_filter = filt
    sa.horizontalScrollBar().valueChanged.connect(lambda _: top.update())
    sa.verticalScrollBar().valueChanged.connect(lambda _: left.update())
    set_rulers_visible(owner, owner.dp5_settings.get('show_rulers'))


def set_rulers_visible(owner, on: bool): #vers 1
    sa = owner._canvas_scroll
    sa.setViewportMargins(RULER if on else 0, RULER if on else 0, 0, 0)
    for w in owner._rulers:
        w.setVisible(bool(on))
    if on:
        owner._place_rulers()
