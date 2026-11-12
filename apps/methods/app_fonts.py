#this belongs in apps/methods/app_fonts.py - Version: 6
# X-Seti - November 07 2026 - IMG Factory 1.6 - Bundled compact fonts

"""
Compact (narrow / condensed) fonts shipped in apps/fonts (SIL OFL, licences
beside them), loaded once into Qt so every workshop can use them on any
system. WORKSHOP_FONTS is the choice list, first = default.
"""

##Methods list -
# add_font_file
# apply_font
# font_css
# font_qss
# global_fonts
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
_EXTRA = os.path.join(os.path.expanduser('~'), '.config', 'imgfactory', 'extra_fonts.json')   # browsed font files

_loaded = None

# role: (App Settings id, default size); App Settings keys <id>_font_family / _size / _weight
FONT_ROLES = {'body': ('default', 10), 'title': ('title', 14), 'panel': ('panel', 10),
              'button': ('button', 10), 'info': ('infobar', 9), 'tab': ('tab', 10),
              'table': ('table', 10), 'menu': ('menu', 10), 'tooltip': ('tooltip', 9)}


def load_fonts() -> list: #vers 2
    """Register bundled and browsed fonts with Qt (once); families that loaded."""
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
    try:
        extra = json.load(open(_EXTRA))
    except (OSError, ValueError):
        extra = []
    for p in extra:
        fid = QFontDatabase.addApplicationFont(p) if os.path.isfile(p) else -1
        if fid >= 0:
            _loaded.extend(QFontDatabase.applicationFontFamilies(fid))
    return _loaded


def add_font_file(path: str) -> str: #vers 1
    """Register a .ttf / .otf, remember it for next start; its family ('' if bad)."""
    from PyQt6.QtGui import QFontDatabase
    fid = QFontDatabase.addApplicationFont(path)
    if fid < 0:
        return ''
    try:
        extra = json.load(open(_EXTRA))
    except (OSError, ValueError):
        extra = []
    if path not in extra:
        extra.append(path)
        os.makedirs(os.path.dirname(_EXTRA), exist_ok=True)
        json.dump(extra, open(_EXTRA, 'w'), indent=2)
    return QFontDatabase.applicationFontFamilies(fid)[0]


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


def global_fonts(settings: dict) -> dict: #vers 2
    """{role: QFont} from App Settings > Fonts (missing roles: workshop font)."""
    from PyQt6.QtGui import QFont
    out = {}
    for role, (key, size) in FONT_ROLES.items():
        f = QFont(settings.get(f'{key}_font_family') or workshop_font_family(),
                  int(settings.get(f'{key}_font_size') or size))
        w = settings.get(f'{key}_font_weight', 'Normal')
        f.setWeight({'Bold': QFont.Weight.Bold, 'Light': QFont.Weight.Light}.get(w, QFont.Weight.Normal))
        f.setItalic(bool(settings.get(f'{key}_font_italic', False)))
        f.setUnderline(bool(settings.get(f'{key}_font_underline', False)))
        out[role] = f
    return out


def font_css(f) -> str: #vers 1
    """QSS declarations for a QFont: family, size, weight, italic, underline."""
    wt = 'bold' if f.weight() >= 600 else ('300' if f.weight() < 400 else 'normal')
    return (f'font-family: "{f.family()}"; font-size: {f.pointSize()}pt; font-weight: {wt}; '
            f'font-style: {"italic" if f.italic() else "normal"}; '
            f'text-decoration: {"underline" if f.underline() else "none"};')


def font_qss(fonts: dict) -> str: #vers 3
    """Stylesheet rules for tabs, tables / lists, buttons, menus, tooltips (also widgets made later)."""
    rules = (('tab', 'QTabBar'), ('table', 'QTableView, QListView, QTreeView, QHeaderView'),
             ('button', 'QPushButton, QToolButton'),
             ('menu', 'QMenu, QMenuBar'), ('tooltip', 'QToolTip'))
    out = []
    for role, sel in rules:
        f = fonts.get(role)
        if f is not None:
            out.append(f'{sel} {{ {font_css(f)} }}')
    return "\n".join(out)
