#this belongs in apps/methods/gizmo_paint.py - Version: 1
# X-Seti - October 14 2026 - IMG Factory 1.6 - Shared gizmo painter and style

"""
Gizmo drawing shared by COL, Model and Vehicle viewports (COL Workshop look),
plus the user's gizmo style: size, line widths, arrow size, letters.
Style is saved once in gizmo_style.json and used by every workshop.
"""

##Methods list -
# edit_gizmo_style
# gizmo_style
# paint_gizmo
# save_gizmo_style

import json
import math

from PyQt6.QtCore import QPointF, QRectF, Qt
from PyQt6.QtGui import QBrush, QColor, QFont, QPen, QPolygonF

DEFAULT_STYLE = {'size': 1.0, 'line': 3.0, 'line_active': 5.0, 'arrow': 1.0,
                 'letters': True, 'letter_size': 9}
AXIS_COLOURS = {'X': (255, 51, 51), 'Y': (51, 255, 51), 'Z': (77, 128, 255)}
GOLD = (255, 230, 25)
_style = None


def gizmo_style(): #vers 1
    """Current style dict (loaded once from gizmo_style.json)."""
    global _style
    if _style is None:
        from apps.methods.img_factory_settings import get_user_config_dir
        path = get_user_config_dir() / 'gizmo_style.json'
        _style = dict(DEFAULT_STYLE)
        if path.exists():
            _style.update(json.loads(path.read_text()))
    return _style


def save_gizmo_style(style): #vers 1
    """Store the style for every workshop."""
    global _style
    from apps.methods.img_factory_settings import get_user_config_dir
    _style = dict(DEFAULT_STYLE, **style)
    (get_user_config_dir() / 'gizmo_style.json').write_text(json.dumps(_style, indent=2))


def paint_gizmo(p, mode, centre, tips, rings, active, order=('X', 'Y', 'Z')): #vers 1
    """Draw with QPainter p. mode 'translate'|'rotate'|'scale'; centre (x, y);
    tips {'X': (x, y)}; rings {'X': [(x, y), ...]}; active 'X'|'Y'|'Z'|'U'|None;
    order = back to front."""
    st = gizmo_style()
    gx, gy = centre
    gold = QColor(*GOLD)
    font = QFont('Arial', int(st['letter_size']), QFont.Weight.Bold)
    p.setFont(font)
    for label in order:
        col = gold if active == label else QColor(*AXIS_COLOURS[label])
        width = st['line_active'] if active == label else st['line']
        if mode == 'rotate':
            pts = rings.get(label)
            if not pts:
                continue
            p.setPen(QPen(col, width)); p.setBrush(Qt.BrushStyle.NoBrush)
            p.drawPolyline(QPolygonF([QPointF(*q) for q in pts] + [QPointF(*pts[0])]))
            lx, ly = pts[len(pts) // 8]
            tx, ty = lx, ly
        else:
            tx, ty = tips[label]
            p.setPen(QPen(col, width))
            p.drawLine(QPointF(gx, gy), QPointF(tx, ty))
            p.setBrush(QBrush(col)); p.setPen(QPen(col, 1))
            arm = math.hypot(tx - gx, ty - gy) or 1.0
            if mode == 'scale':
                h = 6 * st['arrow']
                p.drawRect(QRectF(tx - h, ty - h, 2 * h, 2 * h))
            else:
                ang = math.atan2(ty - gy, tx - gx)
                aw, ah = max(10.0, arm * 0.22) * st['arrow'], max(4.0, arm * 0.08) * st['arrow']
                p.drawPolygon(QPolygonF([
                    QPointF(tx, ty),
                    QPointF(tx - aw * math.cos(ang) + ah * math.sin(ang), ty - aw * math.sin(ang) - ah * math.cos(ang)),
                    QPointF(tx - aw * math.cos(ang) - ah * math.sin(ang), ty - aw * math.sin(ang) + ah * math.cos(ang))]))
        if st['letters']:
            p.setPen(col)
            p.drawText(QPointF(tx + (9 if tx >= gx else -14), ty + (5 if ty >= gy else -3)), label)
    dot = gold if active == 'U' else QColor(255, 255, 255)
    p.setBrush(QBrush(dot)); p.setPen(QPen(dot, 1))
    r0 = 7 if mode == 'scale' else 5
    p.drawEllipse(QPointF(gx, gy), r0, r0)


def edit_gizmo_style(parent): #vers 1
    """Dialog: gizmo size, line widths, arrow size, letters. True when saved."""
    from PyQt6.QtWidgets import (QCheckBox, QDialog, QDialogButtonBox, QDoubleSpinBox,
                                 QFormLayout, QSpinBox)
    st = gizmo_style()
    dlg = QDialog(parent)
    dlg.setWindowTitle("Gizmo Settings")
    form = QFormLayout(dlg)

    def dspin(v, lo, hi, step):
        sp = QDoubleSpinBox(); sp.setRange(lo, hi); sp.setSingleStep(step); sp.setValue(float(v))
        return sp
    size = dspin(st['size'], 0.3, 4.0, 0.1)
    line = dspin(st['line'], 1.0, 12.0, 0.5)
    line_a = dspin(st['line_active'], 1.0, 16.0, 0.5)
    arrow = dspin(st['arrow'], 0.3, 4.0, 0.1)
    letters = QCheckBox("Show X / Y / Z")
    letters.setChecked(bool(st['letters']))
    lsize = QSpinBox(); lsize.setRange(6, 24); lsize.setValue(int(st['letter_size']))
    for label, w in (("Size:", size), ("Line width:", line), ("Hover line width:", line_a),
                     ("Arrow size:", arrow), ("Letters:", letters), ("Letter size:", lsize)):
        form.addRow(label, w)
    bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
                          | QDialogButtonBox.StandardButton.RestoreDefaults)
    bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)

    def defaults():
        for w, k in ((size, 'size'), (line, 'line'), (line_a, 'line_active'), (arrow, 'arrow')):
            w.setValue(DEFAULT_STYLE[k])
        letters.setChecked(DEFAULT_STYLE['letters']); lsize.setValue(DEFAULT_STYLE['letter_size'])
    bb.button(QDialogButtonBox.StandardButton.RestoreDefaults).clicked.connect(defaults)
    form.addRow(bb)
    if dlg.exec() != QDialog.DialogCode.Accepted:
        return False
    save_gizmo_style({'size': size.value(), 'line': line.value(), 'line_active': line_a.value(),
                      'arrow': arrow.value(), 'letters': letters.isChecked(),
                      'letter_size': lsize.value()})
    return True
