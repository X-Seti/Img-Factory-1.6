#this belongs in apps/utils/depends/appsys_panel_test_func.py - Version: 1
# X-Seti - October 29 2026 - App Settings System - Panel test tab

"""
Panel Test tab: a mock window of panels, bars and gadgets drawn in the
current colours, transparency and panel effect. Click an area to edit its
colour, transparency and the panel effect; changes go through the Colors tab rows.
"""

##Methods list -
#
##class PanelTestView: -
# __init__
# _area_at
# _colour
# _rect
# leaveEvent
# mouseMoveEvent
# mousePressEvent
# paintEvent
#
##class AppsysPanelTestMixin: -
# _create_panel_test_tab
# _panel_test_alpha
# _panel_test_colour
# _panel_test_colour_set
# _panel_test_effect
# _panel_test_pick
# _panel_test_settings
# _panel_test_toggle

from PyQt6.QtCore import QRectF, Qt, pyqtSignal
from PyQt6.QtGui import QColor, QFont, QPainter, QPen
from PyQt6.QtWidgets import QComboBox, QHBoxLayout, QLabel, QVBoxLayout, QWidget

# (colour key, label, x, y, w, h) on a 100 x 70 grid; later areas draw on top
AREAS = (
    ('bg_primary',                "Window",          0,  0, 100, 70),
    ('gadgetbar_bg',              "Title bar",       0,  0, 100,  6),
    ('ribbon_bg',                 "Ribbon",          0,  6, 100,  5),
    ('hero_bg',                   "Hero banner",    22, 11,  56,  7),
    ('panel_bg',                  "Side panel",      0, 11,  21, 47),
    ('table_row_even',            "Table rows",     22, 19,  37, 39),
    ('table_row_odd',             "Table odd rows", 22, 19,  37, 39),
    ('viewport_bg',               "Viewport",       60, 19,  18, 39),
    ('splitter_color_background', "Splitter",       21, 11,   1, 47),
    ('scrollbar_background',      "Scrollbar",      58, 19,   1, 39),
    ('panel_entries',             "Button panel",   79, 11,  21, 47),
    ('button_normal',             "Buttons",        81, 14,  17,  5),
    ('dialog_bg',                 "Dialog",         30, 30,  22, 14),
    ('bg_secondary',              "Activity log",    0, 59, 100,  7),
    ('menu_bg',                   "Status bar",      0, 66, 100,  4),
)
PANEL_AREAS = ('panel_bg', 'panel_entries')               # panel effect drawn here


class PanelTestView(QWidget): #vers 1
    """Mock window; click an area to pick its colour key."""
    picked = pyqtSignal(str)

    def __init__(self, dialog): #vers 1
        super().__init__()
        self._dlg = dialog
        self._hover = None
        self._sel = None
        self.setMouseTracking(True)
        self.setMinimumSize(420, 300)

    def _rect(self, a) -> QRectF: #vers 1
        sx, sy = self.width() / 100.0, self.height() / 70.0
        return QRectF(a[2] * sx, a[3] * sy, max(2.0, a[4] * sx), max(2.0, a[5] * sy))

    def _area_at(self, pos): #vers 1
        """Top-most area under pos."""
        for a in reversed(AREAS):
            if a[0] == 'table_row_odd':
                continue                                 # picked through the row stripes
            if self._rect(a).contains(pos.toPointF()):
                if a[0] == 'table_row_even':
                    row = int((pos.y() - self._rect(a).top()) // max(1.0, self._rect(a).height() / 8))
                    return 'table_row_odd' if row % 2 else 'table_row_even'
                return a[0]
        return None

    def _colour(self, key) -> QColor: #vers 1
        """Current colour of key with its transparency."""
        return self._dlg._panel_test_colour(key)

    def paintEvent(self, _e): #vers 1
        from apps.utils.app_settings_system import AppPanelEffect
        p = QPainter(self)
        for y in range(0, self.height(), 10):            # checkerboard shows transparency
            for x in range(0, self.width(), 10):
                p.fillRect(x, y, 10, 10, QColor('#bbbbbb') if (x + y) // 10 % 2 else QColor('#ffffff'))
        cs = self._dlg._panel_test_settings()
        f = QFont(self.font())
        f.setPointSize(max(7, f.pointSize() - 1))
        p.setFont(f)
        for a in AREAS:
            r = self._rect(a)
            if a[0] == 'table_row_odd':
                h = r.height() / 8
                for i in range(1, 8, 2):
                    p.fillRect(QRectF(r.left(), r.top() + i * h, r.width(), h), self._colour('table_row_odd'))
                continue
            p.fillRect(r, self._colour(a[0]))
            if a[0] in PANEL_AREAS and cs.get('panel_effect_type', 'none') != 'none':
                p.save()
                p.setOpacity(self._colour(a[0]).alphaF())
                eff = cs['panel_effect_type']
                getattr(AppPanelEffect, {'fill': '_paint_fill', 'gradient': '_paint_gradient',
                                         'pattern': '_paint_pattern'}[eff])(p, r.toRect(), cs)
                p.restore()
            if a[0] == 'button_normal':
                p.setPen(self._colour('button_text_color'))
                p.drawText(r, Qt.AlignmentFlag.AlignCenter, "Button")
            elif r.width() > 40 and r.height() > 12:
                p.setPen(self._colour('text_primary'))
                p.drawText(r.adjusted(4, 2, -4, -2), Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignTop, a[1])
        for key, colour, width in ((self._hover, '#ffaa00', 1), (self._sel, '#ff3366', 2)):
            for a in AREAS:
                if a[0] == key or (key == 'table_row_odd' and a[0] == 'table_row_even'):
                    p.setPen(QPen(QColor(colour), width, Qt.PenStyle.DashLine))
                    p.setBrush(Qt.BrushStyle.NoBrush)
                    p.drawRect(self._rect(a).adjusted(1, 1, -1, -1))
        p.end()

    def mouseMoveEvent(self, e): #vers 1
        key = self._area_at(e.position().toPoint())
        if key != self._hover:
            self._hover = key
            label = next((a[1] for a in AREAS if a[0] == key), "")
            self.setToolTip(f"{label} ({key})" if key else "")
            self.update()

    def leaveEvent(self, _e): #vers 1
        self._hover = None
        self.update()

    def mousePressEvent(self, e): #vers 1
        key = self._area_at(e.position().toPoint())
        if key:
            self._sel = key
            self.update()
            self.picked.emit(key)


class AppsysPanelTestMixin: #vers 1
    """Panel Test tab for SettingsDialog."""

    def _create_panel_test_tab(self): #vers 1
        """Mock window left, editor for the clicked area right."""
        tab = QWidget()
        lay = QHBoxLayout(tab)
        self._panel_test_view = PanelTestView(self)
        self._panel_test_view.picked.connect(self._panel_test_pick)
        lay.addWidget(self._panel_test_view, 3)
        side = QVBoxLayout()
        self._panel_test_title = QLabel("Click an area of the window")
        self._panel_test_title.setWordWrap(True)
        side.addWidget(self._panel_test_title)
        self._panel_test_editor_box = QVBoxLayout()
        side.addLayout(self._panel_test_editor_box)
        side.addWidget(QLabel("Panel effect (side and button panels):"))
        self._panel_test_effect_combo = QComboBox()
        self._panel_test_effect_combo.addItems(["None", "Fill", "Gradient", "Pattern"])
        eff = self.app_settings.current_settings.get("panel_effect_type", "none")
        self._panel_test_effect_combo.setCurrentIndex(["none", "fill", "gradient", "pattern"].index(eff)
                                                      if eff in ("none", "fill", "gradient", "pattern") else 0)
        self._panel_test_effect_combo.currentIndexChanged.connect(self._panel_test_effect)
        side.addWidget(self._panel_test_effect_combo)
        note = QLabel("Colours and gradient stops are also on the Colors tab; "
                      "image and opacity on the Panels tab.")
        note.setWordWrap(True)
        note.setStyleSheet("font-style: italic;")
        side.addWidget(note)
        side.addStretch(1)
        lay.addLayout(side, 2)
        self._panel_test_editor = None
        return tab

    def _panel_test_settings(self) -> dict: #vers 1
        """Current settings with the (edited) theme colours merged in."""
        cs = dict(self.app_settings.current_settings)
        cs.update(self.app_settings.get_theme_colors() or {})
        for key, ed in getattr(self, 'color_editors', {}).items():
            cs[key] = ed.current_value
        return cs

    def _panel_test_colour(self, key) -> QColor: #vers 1
        """Edited colour of key (fallbacks as the stylesheet) with transparency."""
        from apps.utils.app_settings_system import color_alpha
        cs = self._panel_test_settings()
        fall = {'ribbon_bg': 'toolbar_bg', 'toolbar_bg': 'bg_secondary', 'menu_bg': 'bg_secondary',
                'dialog_bg': 'bg_primary', 'scrollbar_background': 'bg_secondary',
                'hero_bg': 'accent_primary', 'panel_entries': 'panel_bg', 'panel_bg': 'bg_secondary',
                'button_text_color': 'text_primary', 'viewport_bg': 'bg_primary'}
        k = key
        while not cs.get(k) and k in fall:
            k = fall[k]
        col = QColor(cs.get(k) or '#808080')
        a = color_alpha(self.app_settings.current_settings, key)
        if a is not None:
            col.setAlphaF(a / 100.0)
        return col

    def _panel_test_pick(self, key): #vers 1
        """Area clicked: colour row for its key."""
        from apps.utils.app_settings_system import ALPHA_COLOR_KEYS, ThemeColorEditor
        label = next((a[1] for a in AREAS if a[0] == key), key)
        self._panel_test_title.setText(f"{label}  ({key})")
        if self._panel_test_editor is not None:
            self._panel_test_editor.setParent(None)
            self._panel_test_editor.deleteLater()
        cs = self.app_settings.current_settings
        alpha = cs.get("color_alpha", {}).get(key, 100) if key in ALPHA_COLOR_KEYS else None
        ed = ThemeColorEditor(key, label, self._panel_test_colour(key).name(), self, alpha,
                              key not in cs.get("color_alpha_off", []))
        ed.colorChanged.connect(self._panel_test_colour_set)
        ed.alphaChanged.connect(self._panel_test_alpha)
        ed.alphaToggled.connect(self._panel_test_toggle)
        self._panel_test_editor_box.addWidget(ed)
        self._panel_test_editor = ed

    def _panel_test_colour_set(self, key, hex_value): #vers 1
        """Colour edited here: same path as the Colors tab row."""
        row = getattr(self, 'color_editors', {}).get(key)
        if row is not None:
            row.set_color(hex_value)
        else:
            self._on_theme_color_changed(key, hex_value)
        self._panel_test_view.update()

    def _panel_test_alpha(self, key, value): #vers 1
        """Transparency edited here."""
        self._on_color_alpha_changed(key, value)
        row = getattr(self, 'color_editors', {}).get(key)
        if row is not None:
            row.set_alpha(value)
        self._panel_test_view.update()

    def _panel_test_toggle(self, key, on): #vers 1
        """Transparency on / off here."""
        self._on_color_alpha_toggled(key, on)
        row = getattr(self, 'color_editors', {}).get(key)
        if row is not None:
            row.set_alpha_on(on)
        self._panel_test_view.update()

    def _panel_test_effect(self, i): #vers 1
        """Panel effect: kept in step with the Colors tab selector."""
        self.app_settings.current_settings["panel_effect_type"] = ["none", "fill", "gradient", "pattern"][i]
        combo = getattr(self, '_panel_effect_combo', None)
        if combo is not None and combo.currentIndex() != i:
            combo.setCurrentIndex(i)
        self._panel_test_view.update()
