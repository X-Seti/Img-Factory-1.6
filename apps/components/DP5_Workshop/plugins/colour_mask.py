#this belongs in apps/components/DP5_Workshop/plugins/colour_mask.py - Version: 1
# X-Seti - October 20 2026 - IMG Factory 1.6 - DP5 Colour Mask Plugin.

"""
Colour Mask - tints a greyscale image with one chosen colour.
"""

##Methods list -
# colour_mask
# run

from PIL import Image, ImageOps
from PyQt6.QtGui import QColor
from PyQt6.QtWidgets import QColorDialog

PLUGIN_NAME = "Colour Mask"


def colour_mask(rgba, w, h, rgb): #vers 1
    """Map luminance black..white to black..rgb, keep alpha."""
    img = Image.frombytes('RGBA', (w, h), bytes(rgba))
    alpha = img.getchannel('A')
    tinted = ImageOps.colorize(img.convert('L'), (0, 0, 0), rgb).convert('RGBA')
    tinted.putalpha(alpha)
    return bytearray(tinted.tobytes())


def run(ws): #vers 1
    """Ask colour, tint active canvas with undo."""
    c = ws.dp5_canvas
    if not c:
        return
    col = QColorDialog.getColor(QColor(128, 0, 128), ws, "Colour Mask")
    if not col.isValid():
        return
    ws._push_undo()
    c.rgba = colour_mask(c.rgba, c.tex_w, c.tex_h, (col.red(), col.green(), col.blue()))
    c.update()
    ws._set_status(f"Colour mask {col.name()} applied")


PLUGIN_ACTIONS = [("Colour Mask…", run)]
