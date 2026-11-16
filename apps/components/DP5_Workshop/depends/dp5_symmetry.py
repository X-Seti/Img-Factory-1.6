#this belongs in apps/components/DP5_Workshop/depends/dp5_symmetry.py - Version: 1
# X-Seti - November 16 2026 - IMG Factory 1.6 - DP5 movable symmetry axes and HUD

"""
Symmetry axes: dashed lines with a knob each that can be dragged, and a small HUD (Off, H, V,
Quad, Centre) shown while symmetry is on. Axes default to the picture centre.
"""

##Methods list -
# _knobs
# mirror_x
# mirror_y
# sym_axes
# sym_move
# sym_paint
# sym_press
# sym_release
# sym_ui

from PyQt6.QtCore import QPoint, Qt
from PyQt6.QtGui import QColor, QPen
from PyQt6.QtWidgets import QHBoxLayout, QPushButton, QScrollArea, QWidget


def sym_axes(c) -> tuple: #vers 1
    """(x axis, y axis) in picture pixels; the centre unless dragged."""
    ax = (c.tex_w - 1) / 2.0 if c.sym_ax is None else c.sym_ax
    ay = (c.tex_h - 1) / 2.0 if c.sym_ay is None else c.sym_ay
    return ax, ay


def mirror_x(c, x: int) -> int: #vers 1
    """x mirrored about the vertical axis."""
    return round(2 * sym_axes(c)[0] - x)


def mirror_y(c, y: int) -> int: #vers 1
    """y mirrored about the horizontal axis."""
    return round(2 * sym_axes(c)[1] - y)


def _knobs(c) -> dict: #vers 1
    """Widget positions of the axis knobs for the current mode."""
    ax, ay = sym_axes(c)
    z = max(0.01, c.zoom)
    zx = z * c.pixel_aspect_x
    out = {}
    if c.symmetry_mode in ('H', 'quad'):
        out['x'] = QPoint(int((ax + 0.5) * zx), 10)
    if c.symmetry_mode in ('V', 'quad'):
        out['y'] = QPoint(10, int((ay + 0.5) * z))
    return out


def sym_paint(c, painter): #vers 1
    """Dashed axes with a knob each."""
    if c.symmetry_mode == 'off':
        return
    painter.setPen(QPen(QColor('#ff40c8'), 1, Qt.PenStyle.DashLine))
    kn = _knobs(c)
    if 'x' in kn:
        painter.drawLine(kn['x'].x(), 0, kn['x'].x(), c.height())
    if 'y' in kn:
        painter.drawLine(0, kn['y'].y(), c.width(), kn['y'].y())
    painter.setPen(QPen(QColor('#000000'), 1))
    painter.setBrush(QColor('#ff40c8'))
    for p in kn.values():
        painter.drawRect(p.x() - 5, p.y() - 5, 10, 10)


def sym_press(c, wp, tx, ty) -> bool: #vers 1
    """True when an axis knob was grabbed."""
    if c.symmetry_mode == 'off':
        return False
    for k, p in _knobs(c).items():
        if abs(p.x() - wp.x()) <= 8 and abs(p.y() - wp.y()) <= 8:
            c._sym_drag = k
            return True
    return False


def sym_move(c, tx, ty): #vers 1
    """Drag the axis to the pointer."""
    if c._sym_drag == 'x':
        c.sym_ax = max(0.0, min(c.tex_w - 1.0, float(tx)))
    else:
        c.sym_ay = max(0.0, min(c.tex_h - 1.0, float(ty)))
    c.update()


def sym_release(c): #vers 1
    """End the axis drag."""
    c._sym_drag = None


def sym_ui(c): #vers 1
    """HUD at the top left of the visible area while symmetry is on."""
    ui = getattr(c, '_sym_ui', None)
    if c.symmetry_mode == 'off':
        if ui is not None and ui.isVisible():
            ui.hide()
        return
    if ui is None:
        ui = QWidget(c)
        ui.setObjectName("symhud")
        ui.setStyleSheet("QWidget#symhud{background:rgba(60,10,48,225);border:1px solid #ff40c8;border-radius:4px;}"
                         "QPushButton{min-width:30px;padding:2px 6px;}QPushButton:checked{background:#a01c80;}")
        lay = QHBoxLayout(ui)
        lay.setContentsMargins(4, 3, 4, 3)
        lay.setSpacing(3)
        c._sym_btns = {}
        for m, text, tip in (('off', 'Off', 'Symmetry off'), ('H', 'H', 'Mirror left / right'),
                             ('V', 'V', 'Mirror up / down'), ('quad', 'Quad', 'Mirror both')):
            b = QPushButton(text)
            b.setCheckable(True)
            b.setToolTip(tip)
            b.clicked.connect(lambda _=False, mm=m: c._editor._set_symmetry(mm))
            lay.addWidget(b)
            c._sym_btns[m] = b
        b = QPushButton("Centre")
        b.setToolTip("Put the axes back at the centre")
        b.clicked.connect(lambda: (setattr(c, 'sym_ax', None), setattr(c, 'sym_ay', None), c.update()))
        lay.addWidget(b)
        c._sym_ui = ui
    for m, b in c._sym_btns.items():
        b.setChecked(m == c.symmetry_mode)
    sa = c.parent()
    while sa is not None and not isinstance(sa, QScrollArea):
        sa = sa.parent()
    org = c.mapFrom(sa.viewport(), QPoint(6, 6)) if sa else QPoint(6, 6)
    ui.adjustSize()
    if ui.pos() != org:
        ui.move(org)
    if not ui.isVisible():
        ui.show()
    ui.raise_()
