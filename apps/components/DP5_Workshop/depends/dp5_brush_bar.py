#this belongs in apps/components/DP5_Workshop/depends/dp5_brush_bar.py - Version: 1
# X-Seti - November 16 2026 - IMG Factory 1.6 - DP5 on-canvas brush bar

"""
Brush bar: size, opacity and strength sliders that float at the bottom left of the visible
canvas for the tools that use them. Values mirror the Tool Settings ribbon.
"""

##Methods list -
# bar_ui

from PyQt6.QtCore import QPoint, Qt
from PyQt6.QtWidgets import QDoubleSpinBox, QHBoxLayout, QLabel, QScrollArea, QSlider, QWidget


def _row(lay, text, lo, hi, fn): #vers 1
    """Label + slider + value; returns (slider, value label)."""
    lbl = QLabel(text)
    s = QSlider(Qt.Orientation.Horizontal)
    s.setRange(lo, hi)
    s.setFixedWidth(90)
    val = QLabel("")
    val.setMinimumWidth(34)
    s.valueChanged.connect(fn)
    for w in (lbl, s, val):
        lay.addWidget(w)
    return s, val


def bar_ui(c): #vers 1
    """Show, hide, place and refresh the brush bar for the current tool."""
    ed = c._editor
    bar = getattr(c, '_bb_ui', None)
    on = bool(ed and ed.dp5_settings.get('brush_bar') and c.tool not in ed._TOOL_NO_SIZE
              and c.tool not in ed._TEXT_TOOLS and not c._sel_floating and c._adj is None)
    if not on:
        if bar is not None and bar.isVisible():
            bar.hide()
        return
    if bar is None:
        bar = QWidget(c)
        bar.setObjectName("brushbar")
        bar.setStyleSheet("QWidget#brushbar{background:rgba(20,40,30,215);border:1px solid #50c878;border-radius:4px;}"
                          "QLabel{color:#ddd;}")
        lay = QHBoxLayout(bar)
        lay.setContentsMargins(6, 3, 6, 3)
        lay.setSpacing(5)
        c._bb_size, c._bb_size_v = _row(lay, "Size", 1, 100, lambda v: _set_size(c, v))
        c._bb_op, c._bb_op_v = _row(lay, "Opacity", 10, 100, lambda v: _set_opacity(c, v))
        c._bb_str_l = QLabel("")
        c._bb_str = QDoubleSpinBox()
        c._bb_str.valueChanged.connect(lambda v: _set_strength(c, v))
        lay.addWidget(c._bb_str_l)
        lay.addWidget(c._bb_str)
        c._bb_ui = bar
    for s, v, val in ((c._bb_size, c._bb_size_v, c.brush_size), (c._bb_op, c._bb_op_v, round(c.opacity * 100))):
        s.blockSignals(True)
        s.setValue(int(val))
        s.blockSignals(False)
    c._bb_size_v.setText(str(c.brush_size))
    c._bb_op_v.setText(f"{round(c.opacity * 100)}%")
    info = ed._TOOL_STRENGTH_MAP.get(c.tool)
    c._bb_str.setVisible(info is not None)
    c._bb_str_l.setVisible(info is not None)
    if info:
        attr, label, lo, hi, is_float, step = info
        c._bb_str_l.setText(label)
        c._bb_str.blockSignals(True)
        c._bb_str.setDecimals(2 if is_float else 0)
        c._bb_str.setSingleStep(step)
        c._bb_str.setRange(lo, hi)
        c._bb_str.setValue(getattr(c, attr))
        c._bb_str.blockSignals(False)
    sa = c.parent()
    while sa is not None and not isinstance(sa, QScrollArea):
        sa = sa.parent()
    bar.adjustSize()
    vp = sa.viewport() if sa else c
    pos = c.mapFrom(vp, QPoint(6, max(6, vp.height() - bar.height() - 6)))
    if bar.pos() != pos:
        bar.move(pos)
    if not bar.isVisible():
        bar.show()
    bar.raise_()


def _set_size(c, v: int): #vers 1
    c.brush_size = v
    c._editor._refresh_tool_settings_ribbon()
    c.update()


def _set_opacity(c, v: int): #vers 1
    c._editor._set_opacity(v)
    c.update()


def _set_strength(c, v): #vers 1
    info = c._editor._TOOL_STRENGTH_MAP.get(c.tool)
    if info:
        setattr(c, info[0], v)
    c._editor._refresh_tool_settings_ribbon()
