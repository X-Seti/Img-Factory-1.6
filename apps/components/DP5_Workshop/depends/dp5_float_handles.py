#this belongs in apps/components/DP5_Workshop/depends/dp5_float_handles.py - Version: 1
# X-Seti - November 16 2026 - IMG Factory 1.6 - DP5 floating selection handles

"""
Floating selection handles: eight scale handles and a rotate handle around the lifted pixels,
plus a small HUD with OK, X, Flip H and Flip V. Every change is rebuilt from the original pixels.
"""

##Methods list -
# _corners
# _derive
# _rebuild
# float_flip
# float_move
# float_paint
# float_press
# float_release
# float_ui

import math

from PyQt6.QtCore import QPoint, QRect, Qt
from PyQt6.QtGui import QColor, QPen
from PyQt6.QtWidgets import QHBoxLayout, QPushButton, QWidget

_HANDLES = ('nw', 'n', 'ne', 'e', 'se', 's', 'sw', 'w')
_SIGN = {'nw': (-1, -1), 'n': (0, -1), 'ne': (1, -1), 'e': (1, 0), 'se': (1, 1), 's': (0, 1), 'sw': (-1, 1), 'w': (-1, 0)}


def _derive(c): #vers 1
    """State for the floating buffer; starts afresh when the buffer is a different one."""
    st = getattr(c, '_fh', None)
    if st is None or st['buf'] is not c._sel_buffer:
        st = {'src': bytes(c._sel_buffer), 'sw': c._sel_buf_w, 'sh': c._sel_buf_h,
              'sx': 1.0, 'sy': 1.0, 'ang': 0.0, 'buf': c._sel_buffer, 'drag': None, 'flip': (False, False)}
        c._fh = st
    ox, oy = c._sel_float_pos
    st['cx'] = ox + c._sel_buf_w / 2.0
    st['cy'] = oy + c._sel_buf_h / 2.0
    return st


def _rebuild(c, st): #vers 1
    """Rebuild the floating buffer from the original pixels."""
    from PIL import Image
    img = Image.frombytes('RGBA', (st['sw'], st['sh']), st['src'])
    if st['flip'][0]:
        img = img.transpose(Image.Transpose.FLIP_LEFT_RIGHT)
    if st['flip'][1]:
        img = img.transpose(Image.Transpose.FLIP_TOP_BOTTOM)
    img = img.resize((max(1, round(st['sw'] * st['sx'])), max(1, round(st['sh'] * st['sy']))), Image.Resampling.NEAREST)
    if abs(st['ang']) > 0.01:
        img = img.rotate(-st['ang'], expand=True, resample=Image.Resampling.BILINEAR, fillcolor=(0, 0, 0, 0))
    c._sel_buffer = bytearray(img.tobytes())
    c._sel_buf_w, c._sel_buf_h = img.size
    st['buf'] = c._sel_buffer
    c._sel_float_pos = (round(st['cx'] - img.width / 2.0), round(st['cy'] - img.height / 2.0))
    if c._selection_rect is not None:
        c._selection_rect = QRect(c._sel_float_pos[0], c._sel_float_pos[1], img.width, img.height)
    c.update()


def _corners(c, st): #vers 1
    """Widget positions of the eight handles and the rotate handle."""
    hw, hh = st['sw'] * st['sx'] / 2.0, st['sh'] * st['sy'] / 2.0
    a = math.radians(st['ang'])
    ca, sa = math.cos(a), math.sin(a)
    out = {}
    for k, (sx, sy) in _SIGN.items():
        lx, ly = sx * hw, sy * hh
        out[k] = c._tex_to_widget(round(st['cx'] + lx * ca - ly * sa), round(st['cy'] + lx * sa + ly * ca))
    n, cen = out['n'], c._tex_to_widget(round(st['cx']), round(st['cy']))
    vx, vy = n.x() - cen.x(), n.y() - cen.y()
    ln = math.hypot(vx, vy) or 1.0
    out['rot'] = QPoint(round(n.x() + vx / ln * 22), round(n.y() + vy / ln * 22))
    return out


def float_press(c, wp) -> bool: #vers 1
    """True when a handle was grabbed."""
    if not (c._sel_floating and c._sel_buffer and c._sel_float_pos):
        return False
    st = _derive(c)
    for k, pt in _corners(c, st).items():
        if abs(pt.x() - wp.x()) <= 8 and abs(pt.y() - wp.y()) <= 8:
            st['drag'] = k
            return True
    return False


def float_move(c, tx, ty, shift: bool): #vers 1
    """Scale or rotate while a handle is dragged."""
    st = c._fh
    k = st['drag']
    dx, dy = tx - st['cx'], ty - st['cy']
    if k == 'rot':
        ang = math.degrees(math.atan2(dy, dx)) + 90.0
        st['ang'] = round(ang / 15.0) * 15.0 if shift else ang
    else:
        a = math.radians(-st['ang'])
        lx = dx * math.cos(a) - dy * math.sin(a)
        ly = dx * math.sin(a) + dy * math.cos(a)
        sx, sy = _SIGN[k]
        nx = max(0.02, abs(lx) / (st['sw'] / 2.0))
        ny = max(0.02, abs(ly) / (st['sh'] / 2.0))
        if sx and sy and shift:
            nx = ny = max(nx, ny)
        if sx:
            st['sx'] = nx
        if sy:
            st['sy'] = ny
    _rebuild(c, st)


def float_release(c): #vers 1
    """End the handle drag."""
    c._fh['drag'] = None


def float_flip(c, horizontal: bool): #vers 1
    """Mirror the floating pixels."""
    if not (c._sel_floating and c._sel_buffer):
        return
    st = _derive(c)
    i = 0 if horizontal else 1
    f = list(st['flip'])
    f[i] = not f[i]
    st['flip'] = tuple(f)
    _rebuild(c, st)


def float_paint(c, painter): #vers 1
    """Handles around the floating pixels."""
    if not (c._sel_floating and c._sel_buffer and c._sel_float_pos):
        return
    st = _derive(c)
    pts = _corners(c, st)
    painter.setPen(QPen(QColor('#ffdc00'), 1, Qt.PenStyle.DashLine))
    ring = [pts[k] for k in ('nw', 'ne', 'se', 'sw')]
    for i in range(4):
        painter.drawLine(ring[i], ring[(i + 1) % 4])
    painter.drawLine(pts['n'], pts['rot'])
    painter.setPen(QPen(QColor('#000000'), 1))
    painter.setBrush(QColor(255, 220, 0))
    for k in _HANDLES:
        painter.drawRect(pts[k].x() - 3, pts[k].y() - 3, 6, 6)
    painter.drawEllipse(pts['rot'], 5, 5)


def float_ui(c): #vers 1
    """HUD with OK, X, Flip H, Flip V while a selection floats."""
    ui = getattr(c, '_fh_ui', None)
    on = bool(c._sel_floating and c._sel_buffer and c._sel_float_pos)
    if not on:
        if ui is not None and ui.isVisible():
            ui.hide()
        return
    if ui is None:
        ui = QWidget(c)
        ui.setObjectName("fhud")
        ui.setStyleSheet("QWidget#fhud{background:rgba(60,48,10,225);border:1px solid #ffdc00;border-radius:4px;}"
                         "QPushButton{min-width:26px;padding:2px 6px;}")
        lay = QHBoxLayout(ui)
        lay.setContentsMargins(4, 3, 4, 3)
        lay.setSpacing(3)
        for text, tip, fn in (("OK", "Stamp (Enter)", lambda: c._editor._float_ok()),
                              ("X", "Cancel (Esc)", lambda: c.cancel_sel_move()),
                              ("Flip H", "Mirror left / right", lambda: float_flip(c, True)),
                              ("Flip V", "Mirror up / down", lambda: float_flip(c, False))):
            b = QPushButton(text)
            b.setToolTip(tip)
            b.clicked.connect(lambda _=False, f=fn: f())
            lay.addWidget(b)
        c._fh_ui = ui
    st = _derive(c)
    pts = _corners(c, st)
    xs = [p.x() for p in pts.values()]
    ys = [p.y() for p in pts.values()]
    ui.adjustSize()
    g, w, h = 14, ui.width(), ui.height()
    x = max(0, min(c.width() - w, min(xs)))
    if max(ys) + g + h <= c.height():
        y = max(ys) + g
    elif min(ys) - g - h >= 0:
        y = min(ys) - g - h
    else:
        x, y = max(0, min(c.width() - w, max(xs) + g)), max(0, min(c.height() - h, min(ys)))
    if ui.pos() != QPoint(x, y):
        ui.move(x, y)
    if not ui.isVisible():
        ui.show()
    ui.raise_()
