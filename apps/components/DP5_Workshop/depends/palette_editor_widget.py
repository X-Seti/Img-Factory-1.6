#this belongs in apps/components/DP5_Workshop/depends/palette_editor_widget.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 Palette Editor dock.

"""
Palette Editor dock: edits the swatch picked in the User Palette with
R / G / B and H / S / V sliders and a hex field. Changes go straight into
the user palette. 'Remap canvas' recolours pixels of the swatch's old
colour; 'From FG' / 'To FG' swap with the foreground colour.
"""

##Methods list -
# _apply
# _load
# _remap
# _slider_row
# _sync
# create_palette_editor_dock

from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor
from PyQt6.QtWidgets import (QCheckBox, QFrame, QGridLayout, QHBoxLayout, QLabel, QLineEdit, QPushButton,
                             QSlider, QSpinBox, QVBoxLayout)

_CHANNELS = (('R', 255), ('G', 255), ('B', 255), ('H', 359), ('S', 255), ('V', 255))


def _slider_row(grid, row: int, name: str, top: int): #vers 1
    """Label, slider and spin box in one grid row, kept in step."""
    s = QSlider(Qt.Orientation.Horizontal)
    s.setRange(0, top)
    sp = QSpinBox()
    sp.setRange(0, top)
    sp.setFixedWidth(52)
    s.valueChanged.connect(sp.setValue)
    sp.valueChanged.connect(s.setValue)
    grid.addWidget(QLabel(name), row, 0)
    grid.addWidget(s, row, 1)
    grid.addWidget(sp, row, 2)
    return s


def create_palette_editor_dock(owner): #vers 1
    from apps.components.DP5_Workshop.depends.dock_helper import make_dock
    panel = QFrame()
    panel.setObjectName("dp5_paleditor_panel")
    lay = QVBoxLayout(panel)
    lay.setContentsMargins(4, 4, 4, 4)
    lay.setSpacing(3)
    head = QHBoxLayout()
    swatch = QLabel()
    swatch.setFixedSize(40, 28)
    idx_lbl = QLabel("Pick a swatch in User Palette")
    idx_lbl.setWordWrap(True)
    head.addWidget(swatch)
    head.addWidget(idx_lbl, 1)
    lay.addLayout(head)
    grid = QGridLayout()
    grid.setSpacing(2)
    sliders = {name: _slider_row(grid, i, name, top) for i, (name, top) in enumerate(_CHANNELS)}
    lay.addLayout(grid)
    hexrow = QHBoxLayout()
    hexrow.addWidget(QLabel("Hex"))
    hex_edit = QLineEdit()
    hex_edit.setMaxLength(7)
    hexrow.addWidget(hex_edit, 1)
    lay.addLayout(hexrow)
    remap = QCheckBox("Remap canvas pixels too")
    remap.setToolTip("Pixels of the swatch's old colour change with it")
    lay.addWidget(remap)
    btns = QHBoxLayout()
    from_fg, to_fg = QPushButton("From FG"), QPushButton("To FG")
    from_fg.setToolTip("Set the swatch to the foreground colour")
    to_fg.setToolTip("Use the swatch as the foreground colour")
    btns.addWidget(from_fg)
    btns.addWidget(to_fg)
    lay.addLayout(btns)
    lay.addStretch()

    st = {'idx': -1, 'busy': False}
    owner._paled = {'state': st, 'sliders': sliders, 'hex': hex_edit, 'swatch': swatch, 'label': idx_lbl,
                    'remap': remap}
    for name, s in sliders.items():
        s.valueChanged.connect(lambda _v, n=name: _sync(owner, n))
    hex_edit.editingFinished.connect(lambda: QColor(hex_edit.text()).isValid() and
                                     _apply(owner, QColor(hex_edit.text())))
    from_fg.clicked.connect(lambda: _apply(owner, QColor(owner.dp5_canvas.color)) if owner.dp5_canvas else None)
    to_fg.clicked.connect(lambda: st['idx'] >= 0 and owner._fgbg_swatch.set_fg(
        QColor(owner._user_pal_grid._colors[st['idx']])))
    owner._user_pal_grid.color_picked.connect(lambda _c: _load(owner))
    dock = make_dock(owner, "Palette Editor", panel)
    owner._paleditor_dock = dock
    return dock


def _load(owner): #vers 1
    """Show the user palette's selected swatch in the editor."""
    ed = owner._paled
    grid = owner._user_pal_grid
    i = grid._selected
    if not 0 <= i < len(grid._colors):
        return
    ed['state']['idx'] = i
    ed['label'].setText(f"Swatch {i} of {len(grid._colors)}")
    _apply(owner, QColor(grid._colors[i]), write=False)


def _apply(owner, c: QColor, write: bool = True): #vers 1
    """Set every control to c; write it into the palette (and canvas) unless loading."""
    ed = owner._paled
    st = ed['state']
    st['busy'] = True
    h, s, v, _a = c.getHsv()
    for name, val in zip('RGBHSV', (c.red(), c.green(), c.blue(), max(0, h), s, v)):
        ed['sliders'][name].setValue(val)
    ed['hex'].setText(c.name())
    ed['swatch'].setStyleSheet(f"background:{c.name()}; border:1px solid palette(mid);")
    st['busy'] = False
    i = st['idx']
    grid = owner._user_pal_grid
    if write and 0 <= i < len(grid._colors):
        old = QColor(grid._colors[i])
        grid._colors[i] = QColor(c)
        grid.update()
        if ed['remap'].isChecked() and old.rgb() != c.rgb():
            _remap(owner, old, c)


def _sync(owner, changed: str): #vers 1
    """A slider moved: rebuild the colour from its RGB or HSV group."""
    ed = owner._paled
    if ed['state']['busy']:
        return
    v = {n: s.value() for n, s in ed['sliders'].items()}
    if changed in 'RGB':
        c = QColor(v['R'], v['G'], v['B'])
    else:
        c = QColor.fromHsv(v['H'], v['S'], v['V'])
    _apply(owner, c)


def _remap(owner, old: QColor, new: QColor): #vers 1
    """Canvas pixels exactly old RGB (alpha kept) become new; undoable."""
    import numpy as np
    cv = owner.dp5_canvas
    if not cv:
        return
    a = np.frombuffer(bytes(cv.rgba), np.uint8).reshape(cv.tex_h, cv.tex_w, 4).copy()
    hit = (a[..., 0] == old.red()) & (a[..., 1] == old.green()) & (a[..., 2] == old.blue())
    if not hit.any():
        return
    if not getattr(owner, '_paled_undo_armed', False):  # one undo step per drag
        owner._push_undo()
        owner._paled_undo_armed = True
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(600, lambda: setattr(owner, '_paled_undo_armed', False))
    a[hit, 0], a[hit, 1], a[hit, 2] = new.red(), new.green(), new.blue()
    cv.rgba = bytearray(a.tobytes())
    cv.update()
