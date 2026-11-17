#this belongs in apps/methods/ui_style.py - Version: 8
# X-Seti - November 17 2026 - IMG Factory 1.6 - UI style presets (Amiga Workbench)

"""
UI style options: bevelled controls and Amiga title bar, set in App Settings > Style.
"""

##Methods list -
# amiga_gadget_css
# _amiga_unpaint
# amiga_title_icon
# apply_amiga_gadget_icons
# apply_amiga_titlebar
# bevel_css
# style_settings
# _amiga_blue
# _amiga_win_settings

##class _GadgetPaint: -
# __init__
# eventFilter

from pathlib import Path

from PyQt6.QtWidgets import QHBoxLayout

from PyQt6.QtCore import QEvent, QObject, QSize, Qt
from PyQt6.QtGui import QColor, QIcon, QImage, QPainter, QPixmap

GADGET_DIR = Path(__file__).resolve().parent.parent / "icons" / "amiga"

DEFAULTS = {"ui_style": "default", "ui_bevels": False, "ui_amiga_titlebar": False}
AMIGA_THEME = "amiga_workbench"
AMIGA_BLUE = "#0055aa"


def style_settings(settings) -> dict: #vers 1
    """Style keys from a settings dict, with defaults."""
    s = settings or {}
    return {k: s.get(k, v) for k, v in DEFAULTS.items()}


def bevel_css(colors: dict, settings) -> str: #vers 2
    """Raised buttons, sunken fields, square corners, from the theme colours."""
    if not style_settings(settings)["ui_bevels"]:
        return ""
    base = QColor(colors.get("window_bg") or colors.get("bg_primary") or "#aaaaaa")
    txt = colors.get("text_primary") or "#000000"
    lite = base.lighter(165).name()
    dark = base.darker(215).name()
    return f"""
QPushButton, QToolButton, QComboBox {{ border:2px solid; border-color:{lite} {dark} {dark} {lite}; border-radius:0; }}
QPushButton:pressed, QToolButton:pressed, QPushButton:checked, QToolButton:checked {{ border-color:{dark} {lite} {lite} {dark}; }}
QLineEdit, QTextEdit, QPlainTextEdit, QSpinBox, QDoubleSpinBox, QTreeView, QTableView, QListView, QTreeWidget, QTableWidget, QListWidget {{ border:2px solid; border-color:{dark} {lite} {lite} {dark}; border-radius:0; }}
QGroupBox {{ border:2px solid; border-color:{dark} {lite} {lite} {dark}; border-radius:0; }}
QTabBar::tab {{ border:2px solid; border-color:{lite} {dark} {dark} {lite}; border-radius:0; }}
QHeaderView::section {{ color:{txt}; background:{base.name()}; border:2px solid; border-color:{lite} {dark} {dark} {lite}; padding:2px 4px; }}
QProgressBar {{ border:2px solid; border-color:{dark} {lite} {lite} {dark}; border-radius:0; }}
"""


def amiga_title_icon(kind, base, w=30, h=30) -> QIcon: #vers 3
    """Workbench title gadget drawn in code at the full button size."""
    base = QColor(base)
    ic = QIcon()
    k = max(1.0, min(w, h) / 15.0)
    for pressed, mode in ((False, QIcon.Mode.Normal), (True, QIcon.Mode.Active)):
        im = QImage(w, h, QImage.Format.Format_ARGB32)
        im.fill(QColor(0, 0, 0, 0))
        p = QPainter(im)
        lite, dark = base.lighter(170), base.darker(250)
        tl, br = (dark, lite) if pressed else (lite, dark)
        t = max(1, round(k / 2))
        p.fillRect(0, 0, w, h, base)
        p.fillRect(0, 0, w, t, tl)
        p.fillRect(0, 0, t, h, tl)
        p.fillRect(0, h - t, w, t, br)
        p.fillRect(w - t, 0, t, h, br)
        p.translate((w - 15 * k) / 2 + (k if pressed else 0), (h - 15 * k) / 2 + (k if pressed else 0))
        p.scale(k, k)
        white, black = QColor("#ffffff"), QColor("#000022")
        if kind == "close":
            p.fillRect(6, 3, 3, 9, black)
            p.fillRect(6, 3, 2, 8, white)
        elif kind == "zoom":
            p.fillRect(3, 3, 9, 9, black)
            p.fillRect(4, 4, 7, 7, base)
            p.fillRect(4, 4, 4, 4, white)
        else:
            p.fillRect(2, 2, 8, 8, black)
            p.fillRect(3, 3, 6, 6, base)
            p.fillRect(6, 6, 8, 8, black)
            p.fillRect(7, 7, 6, 6, white)
        p.end()
        ic.addPixmap(QPixmap.fromImage(im), mode)
    return ic


def amiga_gadget_css(colors: dict, settings) -> str: #vers 4
    """Workbench checkbox and combo arrow (Amiga preset); bevelled scroll bars with arrow buttons (Amiga or bevels)."""
    st = style_settings(settings)
    amiga = st["ui_style"] == "amiga"
    if not (amiga or st["ui_bevels"]):
        return ""
    d = GADGET_DIR.as_posix()
    base = QColor(colors.get("window_bg") or colors.get("bg_primary") or "#aaaaaa")
    lite, dark = base.lighter(165).name(), base.darker(215).name()
    track = base.darker(112).name()
    bev = f"border:2px solid; border-color:{lite} {dark} {dark} {lite}; border-radius:0;"
    pre = ""
    if amiga:
        pre = f"""
QCheckBox::indicator {{ width:12px; height:13px; image:url({d}/check_0.png); }}
QCheckBox::indicator:checked {{ image:url({d}/check_1.png); }}
QComboBox::down-arrow {{ image:url({d}/arrow_down_0.png); }}
QToolTip {{ background:#ffffaa; color:#000000; border:1px solid #000000; padding:2px; }}
"""
    out = pre
    for o, wh, lo, hi, mar in (("vertical", "width", "top", "bottom", "18px 0 18px 0"),
                               ("horizontal", "height", "left", "right", "0 18px 0 18px")):
        sz = "height" if o == "vertical" else "width"
        out += f"""
QScrollBar:{o} {{ {wh}:18px; background:{track}; margin:{mar}; border:none; }}
QScrollBar::handle:{o} {{ background:{base.name()}; {bev} margin:0; min-{sz}:24px; }}
QScrollBar::add-page:{o}, QScrollBar::sub-page:{o} {{ background:{track}; }}
QScrollBar::add-line:{o} {{ background:{base.name()}; {bev} {sz}:18px; subcontrol-position:{hi}; subcontrol-origin:margin; }}
QScrollBar::sub-line:{o} {{ background:{base.name()}; {bev} {sz}:18px; subcontrol-position:{lo}; subcontrol-origin:margin; }}
QScrollBar::add-line:{o}:pressed, QScrollBar::sub-line:{o}:pressed {{ border-color:{dark} {lite} {lite} {dark}; }}
"""
    out += f"""
QScrollBar::up-arrow:vertical {{ image:url({d}/arrow_up_0.png); width:14px; height:15px; }}
QScrollBar::down-arrow:vertical {{ image:url({d}/arrow_down_0.png); width:14px; height:15px; }}
QScrollBar::left-arrow:horizontal {{ image:url({d}/arrow_left_0.png); width:14px; height:15px; }}
QScrollBar::right-arrow:horizontal {{ image:url({d}/arrow_right_0.png); width:14px; height:15px; }}
"""
    return out


def _amiga_win_settings(win): #vers 1
    """(style settings, app settings object) for a window."""
    st = getattr(win, "app_settings", None) or getattr(getattr(win, "main_window", None), "app_settings", None)
    return style_settings(getattr(st, "current_settings", {})), st


def _amiga_blue(st) -> str: #vers 2
    """Workbench blue, same for every theme."""
    return AMIGA_BLUE


class _GadgetPaint(QObject): #vers 1
    """Paints a drawn gadget over the whole button, ignoring style margins."""

    def __init__(self, btn, kind, blue):
        super().__init__(btn)
        self.kind, self.blue = kind, blue
        self._cache = {}
        btn.installEventFilter(self)

    def eventFilter(self, obj, ev):
        if ev.type() != QEvent.Type.Paint:
            return False
        key = (obj.width(), obj.height(), obj.isDown(), self.kind, self.blue)
        pm = self._cache.get(key)
        if pm is None:
            ic = amiga_title_icon(self.kind, self.blue, obj.width(), obj.height())
            pm = ic.pixmap(obj.size(), QIcon.Mode.Active if obj.isDown() else QIcon.Mode.Normal)
            self._cache = {key: pm}
        p = QPainter(obj)
        p.drawPixmap(0, 0, pm)
        p.end()
        return True


def apply_amiga_gadget_icons(win) -> None: #vers 5
    """Zoom and depth on the right, close on the left, painted over the full button."""
    cur, st = _amiga_win_settings(win)
    if not cur["ui_amiga_titlebar"]:
        return
    blue = _amiga_blue(st)
    bw, bh = win._tb_dims() if hasattr(win, "_tb_dims") else (30, 30)
    for attr, name in (("maximize_btn", "zoom"), ("minimize_btn", "depth"), ("close_btn", "close")):
        b = getattr(win, attr, None)
        if b is None:
            continue
        b.setFixedSize(bw, bh)
        b.setStyleSheet("QPushButton { border:none; padding:0; }")
        g = getattr(b, "_gad", None)
        if g is None:
            b._gad = _GadgetPaint(b, name, blue)
        else:
            g.kind, g.blue = name, blue
        b.update()


def _amiga_unpaint(b) -> None: #vers 1
    g = getattr(b, "_gad", None)
    if g is not None:
        b.removeEventFilter(g)
        g.deleteLater()
        b._gad = None
        b.update()


def apply_amiga_titlebar(win) -> None: #vers 4
    """Blue Workbench title bar, close left, zoom and depth touching at right; undone when off."""
    lo = getattr(win, "titlebar_layout", None)
    btn = getattr(win, "close_btn", None)
    if lo is None or btn is None:
        return
    cur, st = _amiga_win_settings(win)
    on = bool(cur["ui_amiga_titlebar"])
    if on == getattr(win, "_amiga_tb", False):
        return
    tb = getattr(win, "toolbar", None)
    mn, mx = getattr(win, "minimize_btn", None), getattr(win, "maximize_btn", None)
    for b in (mn, mx, btn):
        if b is not None:
            lo.removeWidget(b)
    old = getattr(win, "_amiga_grp", None)
    if old is not None:
        lo.removeItem(old)
        old.deleteLater()
        win._amiga_grp = None
    if on:
        lo.insertWidget(0, btn)
        grp = QHBoxLayout()
        grp.setSpacing(0)
        grp.setContentsMargins(0, 0, 0, 0)
        for b in (mx, mn):
            if b is not None:
                grp.addWidget(b)
        lo.addLayout(grp)
        win._amiga_grp = grp
        lo.setContentsMargins(2, 2, 2, 2)
        lo.setSpacing(2)
        if tb is not None:
            blue = _amiga_blue(st)
            tb.setStyleSheet(f"#titlebar {{ background:{blue}; border:none; border-bottom:2px solid "
                             f"{QColor(blue).darker(250).name()}; }} #titlebar QLabel {{ color:#ffffff; }}")
    else:
        for b in (mn, mx, btn):
            if b is not None:
                lo.addWidget(b)
                _amiga_unpaint(b)
                b.setStyleSheet("")
                if hasattr(win, "_tb_dims"):
                    b.setFixedSize(*win._tb_dims())
                b.setIconSize(QSize(20, 20))
        lo.setContentsMargins(5, 5, 5, 5)
        lo.setSpacing(5)
        if tb is not None:
            tb.setStyleSheet("")
    win._amiga_tb = on
