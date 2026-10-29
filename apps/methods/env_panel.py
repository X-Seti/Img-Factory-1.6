#this belongs in apps/methods/env_panel.py - Version: 1
# X-Seti - October 28 2026 - IMG Factory 1.6 - Environment panel widgets

"""
Shared tool-panel widgets (Planet Zoo / JWE style): an icon rail with
pages, tile grids, SETTINGS headers, segment-bar sliders and value steppers.
All colours come from the palette, so they follow the theme.
"""

##Methods list -
#
##class EnvPanel: -
# __init__
# add_page
# current
# set_page
#
##class SectionHeader: -
# __init__
#
##class SegmentSlider: -
# __init__
# _step
# mouseMoveEvent
# mousePressEvent
# paintEvent
# set_value
# value
# wheelEvent
#
##class Stepper: -
# __init__
# _show
# _step
# set_value
# value
#
##class TileGrid: -
# __init__
# _flow
# add_section
# add_tile
# clear
# current
# minimumSizeHint
# resizeEvent
# set_current
# sizeHint
# tile

from PyQt6.QtCore import QSize, Qt, pyqtSignal
from PyQt6.QtGui import QPainter
from PyQt6.QtWidgets import (QButtonGroup, QFrame, QHBoxLayout, QLabel, QScrollArea,
                             QSizePolicy, QStackedWidget, QToolButton, QVBoxLayout, QWidget)


class SectionHeader(QWidget):
    """Bold caption (SETTINGS) over a thin rule."""

    def __init__(self, text: str, parent=None): #vers 1
        super().__init__(parent)
        lay = QVBoxLayout(self)
        lay.setContentsMargins(0, 6, 0, 2)
        lay.setSpacing(2)
        lbl = QLabel(text.upper())
        f = lbl.font()
        f.setBold(True)
        lbl.setFont(f)
        line = QFrame()
        line.setFrameShape(QFrame.Shape.HLine)
        line.setStyleSheet("color: palette(mid);")
        lay.addWidget(lbl)
        lay.addWidget(line)


class TileGrid(QWidget):
    """Square icon tiles that wrap to the width; one picked at a time.
    Optional section captions split the tiles into groups."""
    picked = pyqtSignal(object)

    def __init__(self, tile: int = 64, parent=None): #vers 2
        super().__init__(parent)
        self._tile, self._gap = tile, 4
        self._items = []                                   # ('section', QLabel) / ('tile', QToolButton)
        self._group = QButtonGroup(self)
        self._group.setExclusive(True)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)

    def add_section(self, title: str): #vers 1
        lbl = QLabel(title, self)
        f = lbl.font()
        f.setItalic(True)
        lbl.setFont(f)
        lbl.show()
        self._items.append(('section', lbl))
        self._flow()

    def add_tile(self, key, icon, tip: str, icon_scale: float = 0.75) -> QToolButton: #vers 2
        b = QToolButton(self)
        b.setIcon(icon)
        b.setIconSize(QSize(int(self._tile * icon_scale), int(self._tile * icon_scale)))
        b.setFixedSize(self._tile, self._tile)
        b.setCheckable(True)
        b.setToolTip(tip)
        b.setProperty('key', key)
        b.setStyleSheet("QToolButton { border: 1px solid palette(mid); border-radius: 3px; }"
                        "QToolButton:checked { border: 2px solid palette(highlight); }")
        b.clicked.connect(lambda _c=False, k=key: self.picked.emit(k))
        b.show()
        self._group.addButton(b)
        self._items.append(('tile', b))
        self._flow()
        return b

    def clear(self): #vers 1
        for _kind, w in self._items:
            if isinstance(w, QToolButton):
                self._group.removeButton(w)
            w.setParent(None)
            w.deleteLater()
        self._items = []
        self._flow()

    def tile(self, key): #vers 1
        return next((w for kind, w in self._items if kind == 'tile' and w.property('key') == key), None)

    def set_current(self, key): #vers 1
        b = self.tile(key)
        if b is not None:
            b.setChecked(True)

    def current(self): #vers 1
        b = self._group.checkedButton()
        return b.property('key') if b is not None else None

    def _flow(self): #vers 2
        """Place tiles in as many columns as the width allows; sections take a full row."""
        step = self._tile + self._gap
        cols = max(1, (self.width() + self._gap) // step)
        x = y = 0
        for kind, w in self._items:
            if kind == 'section':
                if x:
                    x, y = 0, y + step
                h = w.sizeHint().height()
                w.setGeometry(0, y, self.width(), h)
                y += h + 2
                continue
            w.move(x * step, y)
            x += 1
            if x == cols:
                x, y = 0, y + step
        height = y + (step if x else 0)
        self.setFixedHeight(max(height - self._gap, 0))

    def sizeHint(self): #vers 1
        return QSize(self._tile * 4 + self._gap * 3, self.height())

    def minimumSizeHint(self): #vers 1
        return QSize(self._tile, 0)

    def resizeEvent(self, e): #vers 2
        super().resizeEvent(e)
        self._flow()


class SegmentSlider(QWidget):
    """‹ NAME › with a segment bar under it; click / drag the bar, wheel or arrows."""
    valueChanged = pyqtSignal(float)

    def __init__(self, name: str, lo: float, hi: float, value: float, step: float = 1.0,
                 segments: int = 10, fmt: str = "{:g}", parent=None): #vers 1
        super().__init__(parent)
        self._lo, self._hi, self._step_v, self._segments, self._fmt = lo, hi, step, segments, fmt
        self._value = value
        self.setMinimumHeight(40)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        lay = QHBoxLayout(self)
        lay.setContentsMargins(2, 0, 2, 12)
        left, right = QToolButton(), QToolButton()
        left.setArrowType(Qt.ArrowType.LeftArrow)
        right.setArrowType(Qt.ArrowType.RightArrow)
        for b in (left, right):
            b.setAutoRaise(True)
            b.setAutoRepeat(True)
        left.clicked.connect(lambda: self._step(-1))
        right.clicked.connect(lambda: self._step(1))
        self._label = QLabel()
        self._label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._label.setMinimumWidth(10)
        self._label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        f = self._label.font()
        f.setBold(True)
        self._label.setFont(f)
        self._name = name.upper()
        lay.addWidget(left)
        lay.addWidget(self._label, 1)
        lay.addWidget(right)
        self.set_value(value, emit=False)

    def value(self) -> float: #vers 1
        return self._value

    def set_value(self, v: float, emit: bool = True): #vers 1
        v = min(self._hi, max(self._lo, round(v / self._step_v) * self._step_v))
        changed = v != self._value
        self._value = v
        self._label.setText(f"{self._name}  {self._fmt.format(v)}")
        self.update()
        if emit and changed:
            self.valueChanged.emit(v)

    def _step(self, d: int): #vers 1
        self.set_value(self._value + d * self._step_v)

    def wheelEvent(self, e): #vers 1
        self._step(1 if e.angleDelta().y() > 0 else -1)

    def mousePressEvent(self, e): #vers 1
        """Click on the bar: jump to that segment."""
        if e.position().y() >= self.height() - 12:
            frac = min(1.0, max(0.0, (e.position().x() - 8) / max(1, self.width() - 16)))
            self.set_value(self._lo + frac * (self._hi - self._lo))
        super().mousePressEvent(e)

    def mouseMoveEvent(self, e): #vers 1
        if e.buttons() & Qt.MouseButton.LeftButton:
            self.mousePressEvent(e)

    def paintEvent(self, e): #vers 1
        super().paintEvent(e)
        p = QPainter(self)
        pal = self.palette()
        on, off = pal.color(pal.ColorRole.Highlight), pal.color(pal.ColorRole.Mid)
        n = self._segments
        frac = (self._value - self._lo) / max(1e-9, self._hi - self._lo)
        lit = round(frac * n)
        x0, w = 8, self.width() - 16
        seg_w = (w - (n - 1) * 3) / n
        y = self.height() - 9
        for i in range(n):
            p.fillRect(int(x0 + i * (seg_w + 3)), y, max(1, int(seg_w)), 5, on if i < lit else off)


class Stepper(QWidget):
    """Label on the left, ‹ value › on the right cycling through options."""
    changed = pyqtSignal(object)

    def __init__(self, label: str, options, current=None, parent=None): #vers 1
        super().__init__(parent)
        self._options = list(options)                      # [(value, text)]
        self._i = next((i for i, (v, _t) in enumerate(self._options) if v == current), 0)
        lay = QHBoxLayout(self)
        lay.setContentsMargins(2, 0, 2, 0)
        lay.addWidget(QLabel(label), 1)
        box = QFrame()
        box.setStyleSheet("QFrame { border: 1px solid palette(mid); border-radius: 3px; }")
        bl = QHBoxLayout(box)
        bl.setContentsMargins(0, 0, 0, 0)
        left, right = QToolButton(), QToolButton()
        left.setArrowType(Qt.ArrowType.LeftArrow)
        right.setArrowType(Qt.ArrowType.RightArrow)
        for b in (left, right):
            b.setAutoRaise(True)
            b.setStyleSheet("border: none;")
        left.clicked.connect(lambda: self._step(-1))
        right.clicked.connect(lambda: self._step(1))
        self._text = QLabel()
        self._text.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._text.setMinimumWidth(70)
        self._text.setStyleSheet("border: none; font-weight: bold;")
        bl.addWidget(left)
        bl.addWidget(self._text, 1)
        bl.addWidget(right)
        lay.addWidget(box, 1)
        self._show()

    def _show(self): #vers 1
        self._text.setText(self._options[self._i][1])

    def _step(self, d: int): #vers 1
        self._i = (self._i + d) % len(self._options)
        self._show()
        self.changed.emit(self._options[self._i][0])

    def value(self): #vers 1
        return self._options[self._i][0]

    def set_value(self, v): #vers 1
        self._i = next((i for i, (o, _t) in enumerate(self._options) if o == v), self._i)
        self._show()


class EnvPanel(QWidget):
    """Icon rail on the left, one page per rail button (title, subtitle, scrolling body)."""
    pageChanged = pyqtSignal(str)

    def __init__(self, rail_icon: int = 28, parent=None): #vers 1
        super().__init__(parent)
        lay = QHBoxLayout(self)
        lay.setContentsMargins(0, 0, 0, 0)
        lay.setSpacing(4)
        rail = QFrame()
        rail.setStyleSheet("QFrame { border-right: 1px solid palette(mid); }")
        self._rail = QVBoxLayout(rail)
        self._rail.setContentsMargins(2, 2, 2, 2)
        self._rail.setSpacing(4)
        self._rail.addStretch(1)
        self._rail_icon = rail_icon
        self._group = QButtonGroup(self)
        self._stack = QStackedWidget()
        self._keys = []
        lay.addWidget(rail)
        lay.addWidget(self._stack, 1)
        self._group.idClicked.connect(lambda i: self.set_page(self._keys[i]))

    def add_page(self, key: str, icon, title: str, subtitle: str, body: QWidget, scroll: bool = True): #vers 1
        """Rail button plus its page; returns the page body."""
        b = QToolButton()
        b.setIcon(icon)
        b.setIconSize(QSize(self._rail_icon, self._rail_icon))
        b.setFixedSize(self._rail_icon + 12, self._rail_icon + 12)
        b.setCheckable(True)
        b.setToolTip(title if not subtitle else f"{title} - {subtitle}")
        b.setStyleSheet("QToolButton { border: 1px solid transparent; border-radius: 3px; }"
                        "QToolButton:checked { border: 2px solid palette(highlight); }")
        self._group.addButton(b, len(self._keys))
        self._rail.insertWidget(self._rail.count() - 1, b)
        self._keys.append(key)
        page = QWidget()
        pl = QVBoxLayout(page)
        pl.setContentsMargins(4, 2, 4, 2)
        pl.setSpacing(2)
        head = QLabel(title.upper())
        f = head.font()
        f.setBold(True)
        f.setPointSizeF(f.pointSizeF() + 1)
        head.setFont(f)
        head.setAlignment(Qt.AlignmentFlag.AlignCenter)
        pl.addWidget(head)
        if subtitle:
            sub = QLabel(subtitle)
            pl.addWidget(sub)
        if scroll:
            area = QScrollArea()
            area.setWidgetResizable(True)
            area.setFrameShape(QFrame.Shape.NoFrame)
            area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
            area.setWidget(body)
            pl.addWidget(area, 1)
        else:
            pl.addWidget(body, 1)
        self._stack.addWidget(page)
        if len(self._keys) == 1:
            b.setChecked(True)
        return body

    def set_page(self, key: str): #vers 1
        i = self._keys.index(key)
        self._group.button(i).setChecked(True)
        self._stack.setCurrentIndex(i)
        self.pageChanged.emit(key)

    def current(self) -> str: #vers 1
        return self._keys[self._stack.currentIndex()] if self._keys else ''
