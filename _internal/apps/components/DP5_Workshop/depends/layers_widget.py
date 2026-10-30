#this belongs in apps/components/DP5_Workshop/depends/layers_widget.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 Layers dock.

"""
Layers dock: layer list (top layer first, tick = visible, double-click
renames), opacity slider, Add / Delete / Up / Down / Merge down / Flatten.
Layer logic is in depends/dp5_layers.py.
"""

##Methods list -
# _btn
# _on_changed
# _on_current
# create_layers_dock
# refresh_layers

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QIcon, QImage, QPixmap
from PyQt6.QtWidgets import (QFrame, QHBoxLayout, QLabel, QListWidget, QListWidgetItem, QPushButton,
                             QSlider, QVBoxLayout)

THUMB = 32
_ROLE = Qt.ItemDataRole.UserRole


def _btn(owner, text: str, tip: str, fn): #vers 1
    b = QPushButton(text)
    b.setFont(owner.button_font)
    b.setToolTip(tip)
    b.clicked.connect(fn)
    return b


def create_layers_dock(owner): #vers 1
    from apps.components.DP5_Workshop.depends import dp5_layers as L
    from apps.components.DP5_Workshop.depends.dock_helper import make_dock
    panel = QFrame()
    panel.setObjectName("dp5_layers_panel")
    lay = QVBoxLayout(panel)
    lay.setContentsMargins(4, 4, 4, 4)
    lay.setSpacing(3)
    lw = QListWidget()
    lw.setIconSize(QSize(THUMB, THUMB))
    lw.currentRowChanged.connect(lambda r: _on_current(owner, r))
    lw.itemChanged.connect(lambda it: _on_changed(owner, it))
    lay.addWidget(lw, 1)
    row = QHBoxLayout()
    row.addWidget(QLabel("Opacity"))
    op = QSlider(Qt.Orientation.Horizontal)
    op.setRange(0, 100)
    op.setValue(100)
    op.valueChanged.connect(lambda v: owner._layers and L.set_opacity(owner, owner._layer_idx, v))
    row.addWidget(op, 1)
    op_lbl = QLabel("100%")
    op.valueChanged.connect(lambda v: op_lbl.setText(f"{v}%"))
    row.addWidget(op_lbl)
    lay.addLayout(row)
    r1, r2 = QHBoxLayout(), QHBoxLayout()
    r1.addWidget(_btn(owner, "Add", "New transparent layer above", lambda: L.add_layer(owner)))
    r1.addWidget(_btn(owner, "Delete", "Delete the active layer", lambda: L.delete_layer(owner)))
    r1.addWidget(_btn(owner, "Up", "Move layer up", lambda: L.move_layer(owner, 1)))
    r1.addWidget(_btn(owner, "Down", "Move layer down", lambda: L.move_layer(owner, -1)))
    r2.addWidget(_btn(owner, "Merge down", "Merge into the layer below", lambda: L.merge_down(owner)))
    r2.addWidget(_btn(owner, "Flatten", "Merge all visible layers", lambda: L.flatten(owner)))
    lay.addLayout(r1)
    lay.addLayout(r2)
    owner._layers_lw = lw
    owner._layers_op = op
    owner._layers_refreshing = False
    dock = make_dock(owner, "Layers", panel)
    dock.visibilityChanged.connect(lambda on: on and refresh_layers(owner))
    owner._layers_dock = dock
    return dock


def refresh_layers(owner): #vers 1
    """Rebuild the list (top first) from owner._layers."""
    lw = getattr(owner, '_layers_lw', None)
    c = owner.dp5_canvas
    if lw is None or not c:
        return
    from apps.components.DP5_Workshop.depends.dp5_layers import layer_rgba
    owner._layers_refreshing = True
    lw.clear()
    layers = owner._layers or [{'name': "Background", 'visible': True, 'opacity': 100}]
    for i in range(len(layers) - 1, -1, -1):
        Ld = layers[i]
        rgba, w, h = layer_rgba(owner, i) if owner._layers else (c.rgba, c.tex_w, c.tex_h)
        img = QImage(bytes(rgba), w, h, w * 4, QImage.Format.Format_RGBA8888)
        it = QListWidgetItem(QIcon(QPixmap.fromImage(img.scaled(THUMB, THUMB, Qt.AspectRatioMode.KeepAspectRatio))),
                             Ld['name'])
        it.setFlags(it.flags() | Qt.ItemFlag.ItemIsUserCheckable | Qt.ItemFlag.ItemIsEditable)
        it.setCheckState(Qt.CheckState.Checked if Ld['visible'] else Qt.CheckState.Unchecked)
        it.setData(_ROLE, i)
        lw.addItem(it)
    active = owner._layer_idx if owner._layers else 0
    lw.setCurrentRow(len(layers) - 1 - active)
    owner._layers_op.blockSignals(True)
    owner._layers_op.setValue(layers[active]['opacity'])
    owner._layers_op.blockSignals(False)
    owner._layers_refreshing = False


def _on_current(owner, row: int): #vers 1
    if owner._layers_refreshing or row < 0 or not owner._layers:
        return
    from apps.components.DP5_Workshop.depends.dp5_layers import select_layer
    select_layer(owner, owner._layers_lw.item(row).data(_ROLE))


def _on_changed(owner, item): #vers 1
    """Tick box = visibility, edited text = name."""
    if owner._layers_refreshing:
        return
    from apps.components.DP5_Workshop.depends.dp5_layers import set_visible
    if not owner._layers:                                # plain canvas: nothing to rename / hide
        refresh_layers(owner)
        return
    i = item.data(_ROLE)
    owner._layers[i]['name'] = item.text()
    on = item.checkState() == Qt.CheckState.Checked
    if on != owner._layers[i]['visible']:
        set_visible(owner, i, on)
