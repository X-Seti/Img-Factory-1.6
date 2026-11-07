#this belongs in apps/methods/app_fonts.py - Version: 2
# X-Seti - November 07 2026 - IMG Factory 1.6 - Bundled compact fonts

"""
Compact (narrow / condensed) fonts shipped in apps/fonts (SIL OFL, licences
beside them), loaded once into Qt so every workshop can use them on any
system. WORKSHOP_FONTS is the choice list, first = default.
"""

##Methods list -
# apply_font
# load_fonts
# set_workshop_font_family
# workshop_font_family
# workshop_font_names

import json
import os

FONT_DIR = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'fonts')

# (family, files): first is the workshop default
WORKSHOP_FONTS = [
    ("PT Sans Narrow", ("PT_Sans-Narrow-Web-Regular.ttf", "PT_Sans-Narrow-Web-Bold.ttf")),
    ("IBM Plex Sans Condensed", ("IBMPlexSansCondensed-Regular.ttf", "IBMPlexSansCondensed-Bold.ttf")),
    ("Barlow Semi Condensed", ("BarlowSemiCondensed-Regular.ttf", "BarlowSemiCondensed-Bold.ttf")),
    ("Roboto Condensed", ("RobotoCondensed[wght].ttf",)),
    ("Archivo Narrow", ("ArchivoNarrow[wght].ttf",)),
    ("Fira Sans Condensed", ("FiraSansCondensed-Regular.ttf", "FiraSansCondensed-Bold.ttf")),
]
DEFAULT_FONT = WORKSHOP_FONTS[0][0]
_CHOICE = os.path.join(os.path.expanduser('~'), '.config', 'imgfactory', 'workshop_font.json')

_loaded = None


def load_fonts() -> list: #vers 1
    """Register the bundled fonts with Qt (once); families that loaded."""
    global _loaded
    if _loaded is not None:
        return _loaded
    from PyQt6.QtGui import QFontDatabase
    _loaded = []
    for family, files in WORKSHOP_FONTS:
        ok = False
        for f in files:
            p = os.path.join(FONT_DIR, f)
            if os.path.isfile(p) and QFontDatabase.addApplicationFont(p) >= 0:
                ok = True
        if ok:
            _loaded.append(family)
    return _loaded


def workshop_font_names() -> list: #vers 1
    """Bundled font families in preference order (loaded ones only)."""
    return list(load_fonts())


def workshop_font_family() -> str: #vers 1
    """Font chosen for all workshops (Settings > Fonts > Use for all); default PT Sans Narrow."""
    try:
        with open(_CHOICE, 'r', encoding='utf-8') as f:
            return json.load(f).get('family') or DEFAULT_FONT
    except (OSError, ValueError):
        return DEFAULT_FONT


def set_workshop_font_family(family: str) -> None: #vers 1
    """Remember the font for all workshops."""
    os.makedirs(os.path.dirname(_CHOICE), exist_ok=True)
    with open(_CHOICE, 'w', encoding='utf-8') as f:
        json.dump({'family': family}, f)


def apply_font(widget, font) -> None: #vers 2
    """Font onto a workshop and every child without its own font."""
    from PyQt6.QtCore import Qt
    from PyQt6.QtWidgets import QWidget
    widget.setFont(font)
    for c in widget.findChildren(QWidget):
        if not c.testAttribute(Qt.WidgetAttribute.WA_SetFont) or c.font().family() == widget.font().family():
            c.setFont(font)
