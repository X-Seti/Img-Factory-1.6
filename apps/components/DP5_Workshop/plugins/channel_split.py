#this belongs in apps/components/DP5_Workshop/plugins/channel_split.py - Version: 1
# X-Seti - October 20 2026 - IMG Factory 1.6 - DP5 Channel Split Plugin.

"""
Channel Split - splits canvas into RGB/CMYK/YPbPr greyscale tabs; combines RGB.
"""

##Methods list -
# _tab_image
# combine_rgb
# run_combine
# run_split_cmyk
# run_split_rgb
# run_split_ypbpr
# split_channels

from PIL import Image
from PyQt6.QtWidgets import (QDialog, QDialogButtonBox, QFormLayout,
                             QComboBox, QMessageBox)

PLUGIN_NAME = "Channels"

_MODES = {'RGB': ('RGB', 'RGB'), 'CMYK': ('CMYK', 'CMYK'),
          'YPbPr': ('YCbCr', ('Y', 'Pb', 'Pr'))}


def split_channels(rgba, w, h, mode): #vers 1
    """Return [(name, rgba_bytes)] greyscale image per channel."""
    pil_mode, names = _MODES[mode]
    img = Image.frombytes('RGBA', (w, h), bytes(rgba)).convert('RGB').convert(pil_mode)
    return [(n, bytearray(ch.convert('RGBA').tobytes()))
            for n, ch in zip(names, img.split())]


def combine_rgb(r, g, b, w, h): #vers 1
    """Merge three greyscale RGBA buffers into one colour RGBA."""
    chans = [Image.frombytes('RGBA', (w, h), bytes(x)).convert('L') for x in (r, g, b)]
    return bytearray(Image.merge('RGB', chans).convert('RGBA').tobytes())


def _split(ws, mode): #vers 1
    """Send each channel of active canvas to a new tab."""
    c = ws.dp5_canvas
    if not c:
        return
    w, h = c.tex_w, c.tex_h
    base = len(ws._canvas_tabs) or 1
    for name, data in split_channels(c.rgba, w, h, mode):
        ws._canvas_to_new_tab(data, w, h, f"{base}{name}")
    ws._set_status(f"Split {mode} into new tabs")


def run_split_rgb(ws): #vers 1
    _split(ws, 'RGB')


def run_split_cmyk(ws): #vers 1
    _split(ws, 'CMYK')


def run_split_ypbpr(ws): #vers 1
    _split(ws, 'YPbPr')


def _tab_image(ws, idx): #vers 1
    """Return (rgba, w, h) for tab idx, live canvas if active."""
    if idx == ws._active_canvas_tab_idx:
        c = ws.dp5_canvas
        return c.rgba, c.tex_w, c.tex_h
    st = ws._canvas_tabs[idx]['state']
    return st['rgba'], st['tex_w'], st['tex_h']


def run_combine(ws): #vers 1
    """Pick R, G, B tabs and merge into a new colour tab."""
    ws._save_current_canvas_tab()
    tabs = ws._canvas_tabs
    if len(tabs) < 3:
        QMessageBox.information(ws, "Combine RGB", "Need at least 3 canvas tabs.")
        return
    dlg = QDialog(ws)
    dlg.setWindowTitle("Combine RGB")
    form = QFormLayout(dlg)
    combos = []
    for i, lbl in enumerate(("Red", "Green", "Blue")):
        cb = QComboBox()
        cb.addItems([t['label'] for t in tabs])
        cb.setCurrentIndex(min(i, len(tabs) - 1))
        form.addRow(lbl, cb)
        combos.append(cb)
    bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                          QDialogButtonBox.StandardButton.Cancel)
    bb.accepted.connect(dlg.accept)
    bb.rejected.connect(dlg.reject)
    form.addRow(bb)
    if dlg.exec() != QDialog.DialogCode.Accepted:
        return
    imgs = [_tab_image(ws, cb.currentIndex()) for cb in combos]
    if len({(w, h) for _, w, h in imgs}) != 1:
        QMessageBox.warning(ws, "Combine RGB", "Tabs must be the same size.")
        return
    w, h = imgs[0][1], imgs[0][2]
    data = combine_rgb(imgs[0][0], imgs[1][0], imgs[2][0], w, h)
    ws._canvas_to_new_tab(data, w, h, None)
    ws._set_status("Combined RGB into new tab")


PLUGIN_ACTIONS = [("Split RGB", run_split_rgb), ("Split CMYK", run_split_cmyk),
                  ("Split YPbPr", run_split_ypbpr), (None, None),
                  ("Combine RGB…", run_combine)]
