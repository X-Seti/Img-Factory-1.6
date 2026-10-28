#this belongs in apps/components/DP5_Workshop/plugins/halftone.py - Version: 1
# X-Seti - October 26 2026 - IMG Factory 1.6 - DP5 Halftone Plugin.

"""
Halftone - print-style dot screens. Mono: black dots on white, sized by
darkness, at one screen angle. CMYK: four screens (C 15, M 75, Y 0, K 45
degrees) printed over each other. Dots are drawn at 2x and scaled down
(smooth edges); alpha is kept. Replaces the canvas (undo) or opens a new tab.
"""

##Methods list -
# _cmyk
# _screen
# halftone
# run

import math

import numpy as np

from PIL import Image, ImageDraw, ImageStat
from PyQt6.QtWidgets import (QCheckBox, QComboBox, QDialog, QDialogButtonBox, QFormLayout,
                             QSpinBox)

PLUGIN_NAME = "Halftone"

CMYK_ANGLES = (15, 75, 0, 45)                            # C, M, Y, K
SUPER = 2                                                # draw scale for smooth dots


def _screen(ink, cell: int, angle: float): #vers 1
    """'L' image of dots for one ink channel ('L', 255 = full ink); same size as ink."""
    w, h = ink.size
    rot = ink.rotate(angle, expand=True, fillcolor=0)
    rw, rh = rot.size
    dots = Image.new('L', (rw * SUPER, rh * SUPER), 0)
    draw = ImageDraw.Draw(dots)
    for y in range(0, rh, cell):
        for x in range(0, rw, cell):
            m = ImageStat.Stat(rot.crop((x, y, min(x + cell, rw), min(y + cell, rh)))).mean[0]
            if m < 1:
                continue
            r = cell * math.sqrt(m / 255 / math.pi) * SUPER      # dot area = ink share of the cell
            cx, cy = (x + cell / 2) * SUPER, (y + cell / 2) * SUPER
            draw.ellipse((cx - r, cy - r, cx + r, cy + r), fill=255)
    dots = dots.resize((rw, rh), Image.Resampling.LANCZOS).rotate(-angle, expand=True, fillcolor=0)
    left, top = (dots.width - w) // 2, (dots.height - h) // 2
    return dots.crop((left, top, left + w, top + h))


def _cmyk(rgb): #vers 1
    """C, M, Y, K 'L' images with black taken out of the colours (Pillow's CMYK has no K)."""
    a = 255 - np.asarray(rgb, np.int16)
    k = a.min(axis=2)
    return [Image.fromarray((ch - k).astype(np.uint8)) for ch in a.transpose(2, 0, 1)] + \
        [Image.fromarray(k.astype(np.uint8))]


def halftone(rgba, w: int, h: int, cell: int = 8, angle: float = 45, colour: bool = False): #vers 1
    """RGBA bytes -> halftoned RGBA bytes (mono or CMYK), alpha kept."""
    img = Image.frombytes('RGBA', (w, h), bytes(rgba))
    alpha = img.getchannel('A')
    rgb = img.convert('RGB')
    if colour:
        inks = _cmyk(rgb)
        out = Image.merge('CMYK', [_screen(ch, cell, a) for ch, a in zip(inks, CMYK_ANGLES)]).convert('RGB')
    else:
        ink = rgb.convert('L').point(lambda v: 255 - v)
        out = _screen(ink, cell, angle).point(lambda v: 255 - v).convert('RGB')
    out = out.convert('RGBA')
    out.putalpha(alpha)
    return bytearray(out.tobytes())


def run(host): #vers 1
    """Ask mode, cell size, angle; halftone the active canvas or into a new tab."""
    img = host.image()
    if not img:
        return
    dlg = QDialog(host.widget)
    dlg.setWindowTitle("Halftone")
    form = QFormLayout(dlg)
    mode = QComboBox()
    mode.addItems(["Mono (black dots)", "Colour (CMYK screens)"])
    cell = QSpinBox()
    cell.setRange(3, 64)
    cell.setValue(8)
    cell.setSuffix(" px")
    angle = QSpinBox()
    angle.setRange(0, 89)
    angle.setValue(45)
    angle.setSuffix(" deg")
    angle.setToolTip("Mono screen angle (CMYK uses 15 / 75 / 0 / 45)")
    mode.currentIndexChanged.connect(lambda i: angle.setEnabled(i == 0))
    new_tab = QCheckBox("Result in a new tab")
    form.addRow("Mode:", mode)
    form.addRow("Cell size:", cell)
    form.addRow("Angle:", angle)
    form.addRow(new_tab)
    bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
    bb.accepted.connect(dlg.accept)
    bb.rejected.connect(dlg.reject)
    form.addRow(bb)
    if dlg.exec() != QDialog.DialogCode.Accepted:
        return
    rgba, w, h = img
    colour = mode.currentIndex() == 1
    out = halftone(rgba, w, h, cell.value(), angle.value(), colour)
    if new_tab.isChecked():
        host.new_tab(out, w, h, f"{host.active_label()} halftone")
    else:
        host.set_image(out)
    host.status(f"Halftone ({'CMYK' if colour else 'mono'}, {cell.value()} px cells)")


PLUGIN_ACTIONS = [("Halftone…", run)]
