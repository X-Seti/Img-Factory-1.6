#this belongs in apps/components/DP5_Workshop/plugins/sprite_sheet.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 Sprite Sheet Plugin.

"""
Sprite Sheet - cuts the canvas into equal tiles (tile size or columns x
rows, with margin and spacing) and sends them to new tabs or numbered PNG
files; fully transparent / single-colour tiles can be skipped.
"""

##Methods list -
# _dialog
# _empty
# run_png
# run_tabs
# split_sheet

import os

import numpy as np
from PyQt6.QtGui import QImage
from PyQt6.QtWidgets import (QCheckBox, QComboBox, QDialog, QDialogButtonBox, QFileDialog, QFormLayout,
                             QMessageBox, QSpinBox)

PLUGIN_NAME = "Sprite Sheet"


def _empty(tile: np.ndarray) -> bool: #vers 1
    """Fully transparent, or one flat colour."""
    return not tile[..., 3].any() or bool((tile == tile[0, 0]).all())


def split_sheet(rgba, w: int, h: int, tw: int, th: int, margin: int = 0, spacing: int = 0,
                skip_empty: bool = True) -> list: #vers 1
    """[(col, row, tile RGBA array)] left to right, top to bottom."""
    a = np.frombuffer(bytes(rgba), np.uint8).reshape(h, w, 4)
    out = []
    row, y = 0, margin
    while y + th <= h:
        col, x = 0, margin
        while x + tw <= w:
            t = a[y:y + th, x:x + tw]
            if not (skip_empty and _empty(t)):
                out.append((col, row, np.ascontiguousarray(t)))
            col, x = col + 1, x + tw + spacing
        row, y = row + 1, y + th + spacing
    return out


def _dialog(host, title: str): #vers 1
    """(tw, th, margin, spacing, skip_empty) or None."""
    _rgba, w, h = host.image()
    dlg = QDialog(host.widget)
    dlg.setWindowTitle(title)
    form = QFormLayout(dlg)
    by = QComboBox()
    by.addItems(["Tile size", "Columns x rows"])
    a, b = QSpinBox(), QSpinBox()
    for s in (a, b):
        s.setRange(1, 4096)
        s.setValue(16)
    margin, spacing = QSpinBox(), QSpinBox()
    margin.setRange(0, 512)
    spacing.setRange(0, 512)
    skip = QCheckBox("Skip empty / flat tiles")
    skip.setChecked(True)

    def relabel(i):
        form.labelForField(a).setText("Columns:" if i else "Tile width:")
        form.labelForField(b).setText("Rows:" if i else "Tile height:")
    form.addRow("Cut by:", by)
    form.addRow("Tile width:", a)
    form.addRow("Tile height:", b)
    form.addRow("Margin:", margin)
    form.addRow("Spacing:", spacing)
    form.addRow(skip)
    by.currentIndexChanged.connect(relabel)
    bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
    bb.accepted.connect(dlg.accept)
    bb.rejected.connect(dlg.reject)
    form.addRow(bb)
    if dlg.exec() != QDialog.DialogCode.Accepted:
        return None
    m, s = margin.value(), spacing.value()
    if by.currentIndex():                                # columns x rows -> tile size
        tw = (w - 2 * m - s * (a.value() - 1)) // a.value()
        th = (h - 2 * m - s * (b.value() - 1)) // b.value()
    else:
        tw, th = a.value(), b.value()
    if tw < 1 or th < 1 or tw > w or th > h:
        QMessageBox.warning(host.widget, title, f"Tiles of {tw} x {th} don't fit a {w} x {h} canvas")
        return None
    return tw, th, m, s, skip.isChecked()


def run_tabs(host): #vers 1
    """Each tile to its own canvas tab."""
    if not host.image():
        return
    opts = _dialog(host, "Split sheet to tabs")
    if not opts:
        return
    rgba, w, h = host.image()
    tiles = split_sheet(rgba, w, h, *opts)
    if len(tiles) > 256 and QMessageBox.question(
            host.widget, "Split sheet", f"Open {len(tiles)} tabs?") != QMessageBox.StandardButton.Yes:
        return
    label = host.active_label()
    for col, row, t in tiles:
        host.new_tab(t.tobytes(), t.shape[1], t.shape[0], f"{label} {col},{row}")
    host.status(f"{len(tiles)} tiles of {opts[0]} x {opts[1]} opened as tabs")


def run_png(host): #vers 1
    """Each tile saved as <name>_<row>_<col>.png in a folder."""
    if not host.image():
        return
    opts = _dialog(host, "Split sheet to PNG files")
    if not opts:
        return
    folder = QFileDialog.getExistingDirectory(host.widget, "Save tiles to folder")
    if not folder:
        return
    rgba, w, h = host.image()
    stem = host.active_label().replace(' ', '_')
    tiles = split_sheet(rgba, w, h, *opts)
    for col, row, t in tiles:
        img = QImage(t.tobytes(), t.shape[1], t.shape[0], t.shape[1] * 4, QImage.Format.Format_RGBA8888)
        if not img.save(os.path.join(folder, f"{stem}_{row:03d}_{col:03d}.png"), 'PNG'):
            raise OSError(f"Can't write PNG in {folder}")
    host.status(f"{len(tiles)} tiles saved to {folder}")


PLUGIN_ACTIONS = [("Split to tabs…", run_tabs), ("Split to PNG files…", run_png)]
