#this belongs in apps/components/DP5_Workshop/plugins/channel_split.py - Version: 2
# X-Seti - October 20 2026 - IMG Factory 1.6 - DP5 Channel Split Plugin.

"""
Channel Split - splits canvas into RGB/CMYK/YPbPr greyscale tabs; combines RGB.
"""

##Methods list -
# _split
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


def _split(host, mode): #vers 2
    """Each channel of the active canvas to a new tab named '<tab> <channel>'."""
    img = host.image()
    if not img:
        return
    rgba, w, h = img
    label = host.active_label()
    for name, data in split_channels(rgba, w, h, mode):
        host.new_tab(data, w, h, f"{label} {name}")
    host.status(f"Split {mode} into new tabs")


def run_split_rgb(host): #vers 2
    _split(host, 'RGB')


def run_split_cmyk(host): #vers 2
    _split(host, 'CMYK')


def run_split_ypbpr(host): #vers 2
    _split(host, 'YPbPr')


def run_combine(host): #vers 2
    """Pick R, G, B tabs and merge into a new colour tab."""
    labels = host.tabs()
    if len(labels) < 3:
        QMessageBox.information(host.widget, "Combine RGB", "Need at least 3 canvas tabs.")
        return
    dlg = QDialog(host.widget)
    dlg.setWindowTitle("Combine RGB")
    form = QFormLayout(dlg)
    combos = []
    for i, lbl in enumerate(("Red", "Green", "Blue")):
        cb = QComboBox()
        cb.addItems(labels)
        cb.setCurrentIndex(min(i, len(labels) - 1))
        form.addRow(lbl, cb)
        combos.append(cb)
    bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                          QDialogButtonBox.StandardButton.Cancel)
    bb.accepted.connect(dlg.accept)
    bb.rejected.connect(dlg.reject)
    form.addRow(bb)
    if dlg.exec() != QDialog.DialogCode.Accepted:
        return
    imgs = [host.tab_image(cb.currentIndex()) for cb in combos]
    if len({(w, h) for _, w, h in imgs}) != 1:
        QMessageBox.warning(host.widget, "Combine RGB", "Tabs must be the same size.")
        return
    w, h = imgs[0][1], imgs[0][2]
    host.new_tab(combine_rgb(imgs[0][0], imgs[1][0], imgs[2][0], w, h), w, h, "RGB")
    host.status("Combined RGB into new tab")


PLUGIN_ACTIONS = [("Split RGB", run_split_rgb), ("Split CMYK", run_split_cmyk),
                  ("Split YPbPr", run_split_ypbpr), (None, None),
                  ("Combine RGB…", run_combine)]
