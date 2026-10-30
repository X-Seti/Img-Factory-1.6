#this belongs in apps/components/DP5_Workshop/depends/dp5_layers.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 layer stack.

"""
Layers for the DP5 canvas. ws._layers is a list, bottom first, of
{'name', 'rgba', 'w', 'h', 'opacity' (0-100), 'visible', 'undo', 'redo'};
the active layer's pixels and undo stacks live in the canvas / workshop
('rgba' None). An empty list means a plain single-image canvas.
The canvas draws the layers below and above the active one; exports
see the flattened image through flattened().
"""

##Methods list -
# _layer_image
# _over
# add_layer
# composite
# delete_layer
# flatten
# flattened
# layer_rgba
# merge_down
# move_layer
# refresh_view
# select_layer
# set_opacity
# set_visible
#
##class FlatMenu: -
# __init__
# __getattr__
# addAction
# addMenu

from collections import deque
from contextlib import contextmanager

import numpy as np
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QImage, QPainter


def _over(dst: np.ndarray, src: np.ndarray, opacity: float) -> None: #vers 1
    """Alpha-composite src over dst in place (straight alpha, uint8 h x w x 4)."""
    sa = src[..., 3:4].astype(np.float32) / 255 * opacity
    da = dst[..., 3:4].astype(np.float32) / 255
    oa = sa + da * (1 - sa)
    rgb = (src[..., :3] * sa + dst[..., :3] * da * (1 - sa)) / np.maximum(oa, 1e-6)
    dst[..., :3] = np.clip(rgb + 0.5, 0, 255).astype(np.uint8)
    dst[..., 3:4] = np.clip(oa * 255 + 0.5, 0, 255).astype(np.uint8)


def layer_rgba(ws, i: int): #vers 1
    """(rgba, w, h) of layer i; the active one comes from the canvas."""
    if i == ws._layer_idx:
        c = ws.dp5_canvas
        return c.rgba, c.tex_w, c.tex_h
    L = ws._layers[i]
    return L['rgba'], L['w'], L['h']


def composite(ws, indices=None) -> bytearray: #vers 1
    """Visible layers (or indices) flattened at the canvas size."""
    c = ws.dp5_canvas
    out = np.zeros((c.tex_h, c.tex_w, 4), np.uint8)
    for i in (range(len(ws._layers)) if indices is None else indices):
        L = ws._layers[i]
        if not L['visible']:
            continue
        rgba, w, h = layer_rgba(ws, i)
        a = np.frombuffer(bytes(rgba), np.uint8).reshape(h, w, 4)
        hh, ww = min(h, c.tex_h), min(w, c.tex_w)
        _over(out[:hh, :ww], a[:hh, :ww], L['opacity'] / 100)
    return bytearray(out.tobytes())


def _layer_image(ws, indices) -> QImage: #vers 1
    """QImage of the given visible layers drawn bottom to top (canvas size), or None."""
    idx = [i for i in indices if ws._layers[i]['visible']]
    if not idx:
        return None
    c = ws.dp5_canvas
    img = QImage(c.tex_w, c.tex_h, QImage.Format.Format_RGBA8888)
    img.fill(Qt.GlobalColor.transparent)
    p = QPainter(img)
    for i in idx:
        rgba, w, h = layer_rgba(ws, i)
        p.setOpacity(ws._layers[i]['opacity'] / 100)
        p.drawImage(0, 0, QImage(bytes(rgba), w, h, w * 4, QImage.Format.Format_RGBA8888))
    p.end()
    return img


def refresh_view(ws): #vers 1
    """Rebuild the canvas's under / over images and repaint; refresh the dock."""
    c = ws.dp5_canvas
    if not c:
        return
    if ws._layers:
        i = ws._layer_idx
        c.layer_under = _layer_image(ws, range(i))
        c.layer_over = _layer_image(ws, range(i + 1, len(ws._layers)))
        c.layer_opacity = ws._layers[i]['opacity'] / 100 if ws._layers[i]['visible'] else 0.0
    else:
        c.layer_under = c.layer_over = None
        c.layer_opacity = 1.0
    c.update()
    from apps.components.DP5_Workshop.depends.layers_widget import refresh_layers
    refresh_layers(ws)


def select_layer(ws, i: int): #vers 1
    """Make layer i active: park the canvas pixels and undo stacks, load layer i."""
    if not ws._layers or i == ws._layer_idx or not 0 <= i < len(ws._layers):
        return
    c = ws.dp5_canvas
    old = ws._layers[ws._layer_idx]
    old.update(rgba=bytearray(c.rgba), w=c.tex_w, h=c.tex_h,
               undo=deque(ws._undo_stack, maxlen=ws._undo_stack.maxlen),
               redo=deque(ws._redo_stack, maxlen=ws._redo_stack.maxlen))
    new = ws._layers[i]
    c.rgba = new['rgba']
    c.tex_w, c.tex_h = new['w'], new['h']
    ws._canvas_width, ws._canvas_height = new['w'], new['h']
    ws._undo_stack = deque(new['undo'], maxlen=ws._undo_stack.maxlen)
    ws._redo_stack = deque(new['redo'], maxlen=ws._redo_stack.maxlen)
    new['rgba'] = None
    ws._layer_idx = i
    refresh_view(ws)
    ws._history_changed()


def add_layer(ws): #vers 1
    """New transparent layer above the active one (first call makes 'Background')."""
    c = ws.dp5_canvas
    if not c:
        return
    maxlen = ws._undo_stack.maxlen
    if not ws._layers:
        ws._layers = [{'name': "Background", 'rgba': None, 'w': c.tex_w, 'h': c.tex_h, 'opacity': 100,
                       'visible': True, 'undo': deque(maxlen=maxlen), 'redo': deque(maxlen=maxlen)}]
        ws._layer_idx = 0
    n = len(ws._layers)
    ws._layers.insert(ws._layer_idx + 1, {
        'name': f"Layer {n + 1}", 'rgba': bytearray(c.tex_w * c.tex_h * 4), 'w': c.tex_w, 'h': c.tex_h,
        'opacity': 100, 'visible': True, 'undo': deque(maxlen=maxlen), 'redo': deque(maxlen=maxlen)})
    select_layer(ws, ws._layer_idx + 1)


def delete_layer(ws): #vers 1
    """Remove the active layer (the last one can't go)."""
    if len(ws._layers) < 2:
        return
    i = ws._layer_idx
    target = i - 1 if i > 0 else 1
    select_layer(ws, target)
    del ws._layers[i]
    ws._layer_idx = target if target < i else 0
    if len(ws._layers) == 1:                             # back to a plain canvas
        ws._layers = []
    refresh_view(ws)


def move_layer(ws, step: int): #vers 1
    """Move the active layer up (+1) or down (-1) the stack."""
    i, j = ws._layer_idx, ws._layer_idx + step
    if not ws._layers or not 0 <= j < len(ws._layers):
        return
    ws._layers[i], ws._layers[j] = ws._layers[j], ws._layers[i]
    ws._layer_idx = j
    refresh_view(ws)


def merge_down(ws): #vers 1
    """Composite the active layer onto the one below; that becomes active (undoable)."""
    i = ws._layer_idx
    if not ws._layers or i == 0:
        return
    top = ws._layers[i]
    merged_top = top['visible']
    rgba, w, h = layer_rgba(ws, i)
    src = np.frombuffer(bytes(rgba), np.uint8).reshape(h, w, 4)
    select_layer(ws, i - 1)
    c = ws.dp5_canvas
    ws._push_undo()
    if merged_top:
        dst = np.frombuffer(bytes(c.rgba), np.uint8).reshape(c.tex_h, c.tex_w, 4).copy()
        hh, ww = min(h, c.tex_h), min(w, c.tex_w)
        _over(dst[:hh, :ww], src[:hh, :ww], top['opacity'] / 100)
        c.rgba = bytearray(dst.tobytes())
    del ws._layers[i]
    if len(ws._layers) == 1:
        ws._layers = []
    refresh_view(ws)


def flatten(ws): #vers 1
    """All visible layers into one image (undoable as one step)."""
    if not ws._layers:
        return
    flat = composite(ws)
    c = ws.dp5_canvas
    ws._push_undo()
    c.rgba = flat
    ws._layers = []
    refresh_view(ws)


def set_opacity(ws, i: int, value: int): #vers 1
    ws._layers[i]['opacity'] = value
    refresh_view(ws)


def set_visible(ws, i: int, on: bool): #vers 1
    ws._layers[i]['visible'] = on
    refresh_view(ws)


@contextmanager
def flattened(ws): #vers 1
    """Canvas holds the flattened image for the duration (exports)."""
    c = ws.dp5_canvas
    if not c or len(ws._layers) < 2:
        yield
        return
    active, under, over, op = c.rgba, c.layer_under, c.layer_over, c.layer_opacity
    c.rgba = composite(ws)
    c.layer_under = c.layer_over = None                  # no double draw behind dialogs
    c.layer_opacity = 1.0
    try:
        yield
    finally:
        c.rgba, c.layer_under, c.layer_over, c.layer_opacity = active, under, over, op


class FlatMenu:
    """Menu wrapper: 'Save' / 'Export' actions (and everything in such submenus) run on the flattened image."""

    def __init__(self, ws, menu, flat: bool = False): #vers 1
        self._ws, self._m, self._flat = ws, menu, flat

    def addAction(self, text, slot=None): #vers 1
        if slot is None:
            return self._m.addAction(text)
        if self._flat or text.lower().startswith(('save', 'export')):
            ws = self._ws

            def run(*_args, fn=slot):
                with flattened(ws):
                    fn()
            return self._m.addAction(text, run)
        return self._m.addAction(text, slot)

    def addMenu(self, title): #vers 1
        return FlatMenu(self._ws, self._m.addMenu(title), self._flat or title.lower().startswith(('save', 'export')))

    def __getattr__(self, name): #vers 1
        return getattr(self._m, name)
