#this belongs in apps/components/DP5_Workshop/depends/dp5_pending_edit.py - Version: 1
# X-Seti - November 14 2026 - IMG Factory 1.6 - DP5 adjustable shape, gradient and crop handles

"""
Shapes, gradients and crop stay adjustable after the drag: two handles (start, end) move, dragging
inside moves both, OK / Enter keeps, X / Esc restores the picture. The HUD sits clear of the handles.
"""

##Methods list -
# _pts
# adjust_begin
# adjust_cancel
# adjust_commit
# adjust_flip
# adjust_live
# adjust_move
# adjust_paint
# adjust_press
# adjust_ui
# draw_shape

from PyQt6.QtCore import QRect, Qt
from PyQt6.QtGui import QColor, QPen
from PyQt6.QtWidgets import QHBoxLayout, QPushButton, QWidget

ADJUST_TOOLS = ('line', 'arrow', 'double_arrow', 'rect', 'filled_rect', 'circle', 'filled_circle',
                'marker_rect', 'marker_ellipse', 'triangle', 'filled_triangle', 'star', 'filled_star',
                'gradient', 'radial_gradient')
_BOXED = ('rect', 'filled_rect', 'marker_rect', 'marker_ellipse', 'triangle', 'filled_triangle', 'crop')


def draw_shape(c, tool, p0, p1, color, other=None): #vers 1
    """Draw tool from p0 to p1 on canvas c."""
    tx, ty = p1
    if tool == 'line':
        c.draw_line(p0[0], p0[1], tx, ty, color)
    elif tool == 'arrow':
        c.draw_arrow(p0[0], p0[1], tx, ty, color, thickness=c.brush_size)
    elif tool == 'double_arrow':
        c.draw_double_arrow(p0[0], p0[1], tx, ty, color, thickness=c.brush_size)
    elif tool == 'rect':
        c.draw_rect(p0[0], p0[1], tx, ty, color)
    elif tool == 'filled_rect':
        c.draw_filled_rect(p0[0], p0[1], tx, ty, color)
    elif tool == 'circle':
        c.draw_circle(p0[0], p0[1], max(1, abs(tx - p0[0])), max(1, abs(ty - p0[1])), color)
    elif tool == 'filled_circle':
        c.draw_filled_circle(p0[0], p0[1], max(1, abs(tx - p0[0])), max(1, abs(ty - p0[1])), color)
    elif tool == 'marker_rect':
        c.draw_marker_rect(p0[0], p0[1], tx, ty, color, thickness=max(2, c.brush_size))
    elif tool == 'marker_ellipse':
        c.draw_marker_ellipse(p0[0], p0[1], max(1, abs(tx - p0[0])), max(1, abs(ty - p0[1])), color,
                              thickness=max(2, c.brush_size))
    elif tool == 'triangle':
        c.draw_triangle((p0[0] + tx) // 2, p0[1], p0[0], ty, tx, ty, color)
    elif tool == 'filled_triangle':
        mx = (p0[0] + tx) // 2
        c.draw_triangle(mx, p0[1], p0[0], ty, tx, ty, color)
        c.flood_fill(mx, (p0[1] + ty) // 2, color)
    elif tool in ('star', 'filled_star'):
        r = max(abs(tx - p0[0]), abs(ty - p0[1]), 2)
        c.draw_star(p0[0], p0[1], r, r // 2, 5, color)
        if tool == 'filled_star':
            c.flood_fill(p0[0], p0[1], color)
    elif tool in ('gradient', 'radial_gradient'):
        c.apply_gradient(p0, p1, tool == 'radial_gradient', color, other)


def adjust_begin(c, tool, p0, p1, push=True, other=None): #vers 1
    """Draw the shape and keep it adjustable."""
    adjust_commit(c)
    if push:
        c._push_undo_canvas()
    c._adj = {'tool': tool, 'p0': tuple(p0), 'p1': tuple(p1), 'snap': bytes(c.rgba),
              'color': QColor(c.color), 'other': other, 'drag': None}
    adjust_live(c)
    adjust_ui(c)
    if c._editor:
        c._editor._set_status("Drag the handles to adjust, Enter / OK keeps, Esc cancels")


def adjust_live(c): #vers 2
    """Redraw the pending shape from the saved picture."""
    a = c._adj
    if a['tool'] == 'crop':
        c.update()
        return
    c.rgba[:] = a['snap']
    draw_shape(c, a['tool'], a['p0'], a['p1'], a['color'], a['other'])
    c.update()


def adjust_commit(c, ok=False): #vers 2
    """Keep the pending shape; a crop only happens when ok (OK / Enter)."""
    a = getattr(c, '_adj', None)
    if a is None:
        return
    c._adj = None
    adjust_ui(c)
    c.update()
    if a['tool'] == 'crop' and ok and c._editor:
        x0, x1 = sorted((a['p0'][0], a['p1'][0]))
        y0, y1 = sorted((a['p0'][1], a['p1'][1]))
        c._editor._crop_apply(max(0, x0), max(0, y0), min(c.tex_w, x1), min(c.tex_h, y1))


def adjust_cancel(c): #vers 1
    """Restore the picture and drop the undo step."""
    a = getattr(c, '_adj', None)
    if a is None:
        return False
    if a['tool'] != 'crop':
        c.rgba[:] = a['snap']
    c._adj = None
    ed = c._editor
    if ed and ed._undo_stack and a['tool'] != 'crop' and a.get('pushed', True):
        ed._undo_stack.pop()
        ed._history_changed()
    adjust_ui(c)
    c.update()
    return True


def adjust_flip(c): #vers 1
    """Swap the gradient colours."""
    a = c._adj
    if a and a['other'] is not None:
        a['color'], a['other'] = a['other'], a['color']
        adjust_live(c)


def _pts(c): #vers 1
    a = c._adj
    return [c._tex_to_widget(int(a[k][0]), int(a[k][1])) for k in ('p0', 'p1')]


def adjust_press(c, wp, tx, ty) -> bool: #vers 1
    """True when the press grabbed a handle or the inside of the pending shape."""
    a = getattr(c, '_adj', None)
    if a is None:
        return False
    for k, pt in zip(('p0', 'p1'), _pts(c)):
        if abs(pt.x() - wp.x()) <= 8 and abs(pt.y() - wp.y()) <= 8:
            a['drag'] = (k, (tx, ty), a['p0'], a['p1'])
            return True
    p0, p1 = _pts(c)
    r = QRect(p0, p1).normalized().adjusted(-8, -8, 8, 8)
    if r.contains(wp):
        a['drag'] = ('both', (tx, ty), a['p0'], a['p1'])
        return True
    adjust_commit(c)
    return False


def adjust_move(c, tx, ty): #vers 1
    """Drag a handle, or move both."""
    a = c._adj
    k, st, q0, q1 = a['drag']
    if k == 'both':
        dx, dy = tx - st[0], ty - st[1]
        a['p0'] = (q0[0] + dx, q0[1] + dy)
        a['p1'] = (q1[0] + dx, q1[1] + dy)
    else:
        a[k] = (tx, ty)
    adjust_live(c)
    adjust_ui(c)


def adjust_paint(c, painter): #vers 1
    """Handles, plus the box or direction line."""
    a = c._adj
    p0, p1 = _pts(c)
    if a['tool'] == 'crop':
        box = QRect(p0, p1).normalized()
        full = QRect(c._tex_to_widget(0, 0), c._tex_to_widget(c.tex_w, c.tex_h))
        shade = QColor(0, 0, 0, 150)
        painter.fillRect(QRect(full.left(), full.top(), full.width(), max(0, box.top() - full.top())), shade)
        painter.fillRect(QRect(full.left(), box.bottom(), full.width(), max(0, full.bottom() - box.bottom())), shade)
        painter.fillRect(QRect(full.left(), box.top(), max(0, box.left() - full.left()), box.height()), shade)
        painter.fillRect(QRect(box.right(), box.top(), max(0, full.right() - box.right()), box.height()), shade)
    painter.setPen(QPen(QColor('#40c8d8'), 1, Qt.PenStyle.DashLine))
    painter.setBrush(Qt.BrushStyle.NoBrush)
    if a['tool'] in _BOXED:
        painter.drawRect(QRect(p0, p1).normalized())
    else:
        painter.drawLine(p0, p1)
    painter.setBrush(QColor(255, 255, 255))
    painter.setPen(QPen(QColor('#40c8d8'), 1))
    for pt in (p0, p1):
        painter.drawRect(pt.x() - 4, pt.y() - 4, 8, 8)


def adjust_ui(c): #vers 1
    """HUD: OK, X, and Flip for gradients; placed below, above or beside the handles."""
    a = getattr(c, '_adj', None)
    ui = getattr(c, '_adj_ui', None)
    if a is None:
        if ui is not None:
            ui.hide()
        return
    if ui is None:
        ui = QWidget(c)
        ui.setObjectName("adjhud")
        ui.setStyleSheet("QWidget#adjhud{background:rgba(20,28,60,220);border:1px solid #40c8d8;border-radius:4px;}"
                         "QPushButton{min-width:26px;padding:2px 6px;}")
        lay = QHBoxLayout(ui)
        lay.setContentsMargins(4, 3, 4, 3)
        lay.setSpacing(3)
        for text, tip, fn in (("OK", "Keep (Enter)", lambda: adjust_commit(c, True)),
                              ("X", "Cancel (Esc)", lambda: adjust_cancel(c))):
            b = QPushButton(text)
            b.setToolTip(tip)
            b.clicked.connect(lambda _=False, f=fn: f())
            lay.addWidget(b)
        c._adj_flip = QPushButton("Flip")
        c._adj_flip.setToolTip("Swap the gradient colours")
        c._adj_flip.clicked.connect(lambda: adjust_flip(c))
        lay.addWidget(c._adj_flip)
        c._adj_ui = ui
    c._adj_flip.setVisible(a['other'] is not None)
    ui.adjustSize()
    p0, p1 = _pts(c)
    box = QRect(p0, p1).normalized()
    g, W, H = 14, c.width(), c.height()
    w, h = ui.width(), ui.height()
    x = max(0, min(W - w, box.left()))
    if box.bottom() + g + h <= H:
        ui.move(x, box.bottom() + g)
    elif box.top() - g - h >= 0:
        ui.move(x, box.top() - g - h)
    else:
        ui.move(max(0, min(W - w, box.right() + g)), max(0, min(H - h, box.top())))
    ui.show()
    ui.raise_()
