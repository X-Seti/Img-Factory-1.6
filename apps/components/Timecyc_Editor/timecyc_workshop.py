#!/usr/bin/env python3
#this belongs in apps/components/Timecyc_Editor/timecyc_workshop.py - Version: 4
# X-Seti - May08 2026 - Img Factory 1.6 - Time Cycle Editor

"""
Time Cycle Editor — reads/writes GTA VC/SA timecyc.dat and timecycp.dat.
Grid: 8 weather columns x 24 time rows. Each cell = one sky/lighting preset.
Left = weather/time selector, centre = colour sliders + numeric fields,
right = live sky colour preview swatch.
"""

##Methods list -
# TimecycRow.__init__
# TimecycParser.__init__
# TimecycParser.load
# TimecycParser.save
# TimecycParser._detect_game
# TimecycParser._parse_line
# SkyPreviewWidget.__init__
# SkyPreviewWidget.set_colors
# SkyPreviewWidget.paintEvent
# TimecycWorkshop.__init__
# TimecycWorkshop._build_left_panel
# TimecycWorkshop._build_centre_panel
# TimecycWorkshop._build_right_panel
# TimecycWorkshop._open_file
# TimecycWorkshop._save_file
# TimecycWorkshop._on_cell_selected
# TimecycWorkshop._populate_fields
# TimecycWorkshop._on_field_changed
# TimecycWorkshop._update_preview
# TimecycWorkshop._build_menus_into_qmenu
# open_timecyc_editor

import sys, os, re
from pathlib import Path
from typing import List, Optional, Dict, Tuple
from dataclasses import dataclass, field

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = Path(current_dir).parents[2]
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QLabel, QLineEdit,
    QScrollArea, QGroupBox, QSpinBox, QComboBox, QPushButton,
    QFileDialog, QMessageBox, QApplication, QFormLayout, QFrame,
    QTableWidget, QTableWidgetItem, QHeaderView, QAbstractItemView,
    QSlider, QGridLayout, QSizePolicy, QMenu,
    QDialog, QDialogButtonBox, QStyle, QHeaderView, QStyleOptionHeader
)
from PyQt6.QtCore import Qt, QRect, QSize
from PyQt6.QtGui import QFont, QColor, QPainter, QBrush, QLinearGradient

from apps.components.Timecyc_Editor.depends.diffcode import GUIWorkshop
from apps.methods.ribbon_system import RibbonMixin


# Field definitions
WEATHER_NAMES_VC  = ["Sunny", "Cloudy", "Rainy", "Foggy", "ExtraSunny", "Rainy2", "ExtraColours"]
WEATHER_NAMES_GTA3 = ["ExtraS", "ExtraS2", "Sunny", "Cloudy", "Rainy", "Foggy", "ExtraS3", "ExtraS4"]
WEATHER_NAMES_SA = [
    "ExtraSunny_LA", "Sunny_LA", "ExtraSunny_Smog_LA", "Sunny_Smog_LA",
    "Cloudy_LA", "Sunny_SF", "ExtraSunny_SF", "Cloudy_SF",
    "Rainy_SF", "Foggy_SF", "Sunny_Vegas", "ExtraSunny_Vegas",
    "Cloudy_Vegas", "ExtraSunny_Country", "Sunny_Country", "Cloudy_Country",
    "Rainy_Country", "ExtraSunny_Desert", "Sunny_Desert", "Sandstorm_Desert",
    "Underwater", "ExtraColours1", "ExtraColours2"
]

TIME_LABELS = [
    "00:00","01:00","02:00","03:00","04:00","05:00",
    "06:00","07:00","08:00","09:00","10:00","11:00",
    "12:00","13:00","14:00","15:00","16:00","17:00",
    "18:00","19:00","20:00","21:00","22:00","23:00",
]

# GTA3/LC timecyc.dat field layout (40 fields)
# Header: Amb Dir SkyTop SkyBot SunCore SunCorona SunSz SprSz SprBght
#         Shdw LightShd TreeShd FarClp FogSt LightOnGround
#         LowCloudsRGB TopCloudRGB BottomCloudRGB BlurRGB WaterAlpha
GTA3_COLOUR_GROUPS = [
    ("Ambient",       0),   # [0-2]
    ("Directional",   3),   # [3-5]
    ("Sky Top",       6),   # [6-8]
    ("Sky Bottom",    9),   # [9-11]
    ("Sun Core",     12),   # [12-14]
    ("Sun Corona",   15),   # [15-17]
]
GTA3_SCALAR_FIELDS = [
    ("SunCoreSize",    18, 0, 10),
    ("SunCoronaSize",  19, 0, 10),
    ("SpriteBright",   20, 0, 10),
    ("ShadowStrength", 21, 0, 255),
    ("LightShading",   22, 0, 255),
    ("TreeShading",    23, 0, 255),
    ("FarClip",        24, 0, 3000),
    ("FogStart",       25, 0, 3000),
    ("LightOnGround",  26, 0, 10),
]
GTA3_COLOUR_GROUPS_2 = [
    ("Lower Clouds",  27),   # [27-29]
    ("Top Cloud",     30),   # [30-32]
    ("Bottom Cloud",  33),   # [33-35]
    ("Blur/Trail",    36),   # [36-38]
]

# VC/GTA3 timecyc.dat field layout (52 fields for VC, 40 for GTA3)
# Indices confirmed from GTAMods wiki + real file analysis
# [0-2]   Ambient Static RGB       [3-5]   Ambient Dynamic RGB
# [6-8]   Amb Blur Static RGB      [9-11]  Amb Blur Dynamic RGB
# [12-14] Directional RGB          [15-17] Sky Top RGB
# [18-20] Sky Bottom RGB           [21-23] Sun Core RGB
# [24-26] Sun Corona RGB           [27]    Sun Core Size (float)
# [28]    Sun Corona Size (float)  [29]    Sprite Brightness (float)
# [30]    Shadow Intensity         [31]    Light Shading
# [32]    Pole Shading             [33]    Far Clip (float)
# [34]    Fog Start (float)        [35]    Light on Ground (float)
# [36-38] Lower Clouds RGB         [39-41] Upper Clouds Top RGB
# [42-44] Upper Clouds Bottom RGB  [45-47] Blur/Trail RGB
# [48-50] Water RGB                [51]    Water Alpha

VC_COLOUR_GROUPS = [
    ("Ambient",           0),   # [0-2]  static ambient
    ("Ambient Dynamic",   3),   # [3-5]  dynamic ambient
    ("Directional",      12),   # [12-14]
    ("Sky Top",          15),   # [15-17]
    ("Sky Bottom",       18),   # [18-20]
    ("Sun Core",         21),   # [21-23]
    ("Sun Corona",       24),   # [24-26]
]

VC_SCALAR_FIELDS = [
    ("SunCoreSize",      27, 0, 10),
    ("SunCoronaSize",    28, 0, 10),
    ("SpriteBrightness", 29, 0, 10),
    ("ShadowStrength",   30, 0, 255),
    ("LightShading",     31, 0, 255),
    ("PoleShading",      32, 0, 255),
    ("FarClip",          33, 0, 3000),
    ("FogStart",         34, 0, 3000),
    ("LightOnGround",    35, 0, 10),
]

VC_COLOUR_GROUPS_2 = [
    ("Lower Clouds",     36),   # [36-38]
    ("Upper Clouds Top", 39),   # [39-41]
    ("Upper Clouds Bot", 42),   # [42-44]
    ("Blur/Trail",       45),   # [45-47]
    ("Water",            48),   # [48-50]
]


# SA timecyc.dat field layout (51 fields, 8 times per weather, 23 weathers)
# From header: Amb Amb_Obj Dir SkyTop SkyBot SunCore SunCorona SunSz SprSz SprBght
#              Shdw LightShd PoleShd FarClp FogSt LightOnGround LowClouds BottomCloud
#              WaterRGBA Alpha1 RGB1 Alpha2 RGB2 CloudAlpha
SA_COLOUR_GROUPS = [
    ("Ambient",           0),   # [0-2]
    ("Ambient Obj",       3),   # [3-5]
    ("Directional",       6),   # [6-8]
    ("Sky Top",           9),   # [9-11]
    ("Sky Bottom",       12),   # [12-14]
    ("Sun Core",         15),   # [15-17]
    ("Sun Corona",       18),   # [18-20]
]
SA_SCALAR_FIELDS = [
    ("SunCoreSize",      21, 0, 10),
    ("SunCoronaSize",    22, 0, 10),
    ("SpriteBrightness", 23, 0, 10),
    ("ShadowStrength",   24, 0, 255),
    ("LightShading",     25, 0, 255),
    ("PoleShading",      26, 0, 255),
    ("FarClip",          27, 0, 3000),
    ("FogStart",         28, 0, 3000),
    ("LightOnGround",    29, 0, 10),
]
SA_COLOUR_GROUPS_2 = [
    ("Lower Clouds",     30),   # [30-32]
    ("Bottom Cloud",     33),   # [33-35]
    ("Water",            36),   # [36-38] (39=alpha)
    ("Color Corr 1",     41),   # [41-43] (40=alpha)
    ("Color Corr 2",     45),   # [45-47] (44=alpha)
]
SA_TIME_LABELS = ["Midnight","5AM","6AM","7AM","Noon","7PM","8PM","10PM"]

# Data

@dataclass
class TimecycRow: #vers 2
    weather: int = 0
    time:    int = 0
    values:  List[int] = field(default_factory=lambda: [0] * 36)
    comment: str = ""
    raw_index: int = -1            # line index in the loaded file (-1 = none)
    orig: tuple = ()               # values as loaded, to know what changed

    @property
    def changed(self) -> bool:
        return tuple(self.values) != self.orig


def _fmt_value(v, old_token: str = "") -> str:
    if isinstance(v, float) or '.' in old_token:
        t = f"{float(v):.4f}".rstrip('0')
        return t + '0' if t.endswith('.') else t
    return str(int(v))


class TimecycParser: #vers 2
    """timecyc.dat / timecycp.dat. Keeps the original lines (section comments,
    spacing, CRLF); only rows whose values changed are rewritten, and only the
    changed numbers inside them - an untouched file saves byte-identical."""

    def __init__(self): #vers 2
        self.rows:         List[TimecycRow] = []
        self.header_lines: List[str]        = []
        self.game:         str              = 'VC'
        self.cols_per_row: int              = 33
        self._lines:       List[str]        = []
        self._eol:         str              = "\n"

    @property
    def dirty(self) -> bool:
        return any(r.changed for r in self.rows)

    def _detect_game(self, num_values: int, filename: str = '') -> str: #vers 3
        # LC/GTA3=40 fields, VC=52 fields, SA=51 fields, timecycp=52 fields (SA PSP)
        import os as _os
        if _os.path.basename(filename).lower() == 'timecycp.dat': return 'SA'
        if num_values >= 52: return 'VC'
        if num_values >= 51: return 'SA'
        if num_values >= 40: return 'GTA3'
        return 'GTA3'

    def _get_game_layout(self) -> tuple: #vers 1
        """Return (n_weathers, n_times) for current game.
        File ordering is always weather-major (all times for weather0, then weather1 etc).
        GTA3/LC: 4 weathers x 24 times = 96 rows (but we treat as 8x12 by convention)
        VC:      7 weathers x 24 times = 168 rows
        SA:      23 weathers x 8 times = 184 rows
        """
        if self.game == 'SA':   return 23, 8
        if self.game == 'GTA3': return 8, 12   # 8 logical weathers, 12 time slots
        return 7, 24  # VC

    def _parse_line(self, line: str, weather: int, time: int) -> Optional[TimecycRow]: #vers 3
        """Parse one data line into a TimecycRow."""
        s = line.strip()
        if not s or s.startswith('/'):
            return None
        comment = ""
        if '//' in s:
            idx = s.index('//')
            comment = s[idx:]
            s = s[:idx].strip()
        parts = s.split()
        if len(parts) < 10:
            return None
        try:
            values = [(float(p) if '.' in p else int(float(p))) for p in parts]
        except ValueError:
            return None
        return TimecycRow(weather=weather, time=time, values=values, comment=comment)

    def load(self, path: str, known_game: str = None) -> bool: #vers 3
        try:
            text = Path(path).read_bytes().decode("latin1")
            self._eol = "\r\n" if "\r\n" in text else "\n"
            self._lines = text.split(self._eol)
            self.rows.clear()
            self.header_lines.clear()
            first = None
            for ln in self._lines:
                s = ln.strip()
                if s and not s.startswith('/'):
                    parts = s.split('//')[0].split()
                    if len(parts) >= 10:
                        first = len(parts)
                        break
            if known_game:
                self.game = 'VC' if known_game.lower() == 'sol' else known_game.upper()
            elif first:
                self.game = self._detect_game(first, path)
            if first:
                self.cols_per_row = first
            n_weathers, n_times = self._get_game_layout()
            self.header_lines = [ln for ln in self._lines if ln.strip().startswith('/')][:3]
            row_idx = 0
            for i, ln in enumerate(self._lines):
                r = self._parse_line(ln, row_idx // n_times, row_idx % n_times)
                if r:
                    r.raw_index, r.orig = i, tuple(r.values)
                    self.rows.append(r)
                    row_idx += 1
            return True
        except Exception as ex:
            print(f"TimecycParser.load: {ex}")
            return False

    def to_text(self) -> str: #vers 1
        by_line = {r.raw_index: r for r in self.rows}
        out = []
        for i, ln in enumerate(self._lines):
            r = by_line.get(i)
            if r is None or not r.changed:
                out.append(ln)
                continue
            body, sl, cm = ln.partition('//')
            toks = re.split(r'(\s+)', body)
            slots = [k for k, t in enumerate(toks) if t and not t.isspace()]
            for k, (old, new) in enumerate(zip(r.orig, r.values)):
                if old != new and k < len(slots):
                    toks[slots[k]] = _fmt_value(new, toks[slots[k]])
            out.append(''.join(toks) + sl + cm)
        return self._eol.join(out)

    def save(self, path: str) -> bool: #vers 2
        """Atomic write (temp file in the same folder, then swap in), then
        re-baseline so `changed` is false again."""
        import tempfile
        try:
            data = self.to_text().encode("latin1", errors="replace")
            d = os.path.dirname(os.path.abspath(path))
            fd, tmp = tempfile.mkstemp(dir=d, prefix=".tcy_", suffix=".tmp")
            try:
                with os.fdopen(fd, "wb") as f:
                    f.write(data)
                if os.path.exists(path):
                    try:
                        os.chmod(tmp, os.stat(path).st_mode & 0o7777)
                    except OSError:
                        pass
                os.replace(tmp, path)
            except Exception:
                if os.path.exists(tmp):
                    os.unlink(tmp)
                raise
            self._lines = data.decode("latin1").split(self._eol)
            for r in self.rows:
                r.orig = tuple(r.values)
            return True
        except Exception as ex:
            print(f"TimecycParser.save: {ex}")
            return False

    def get_row(self, weather: int, time: int) -> Optional[TimecycRow]: #vers 1
        for r in self.rows:
            if r.weather == weather and r.time == time:
                return r
        return None


# Sky preview widget

class SkyPreviewWidget(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setMinimumHeight(120)
        self._sky_top    = QColor(10, 10, 40)
        self._sky_bot    = QColor(80, 120, 180)
        self._ambient    = QColor(60, 60, 80)
        self._sun_core   = QColor(255, 255, 200)
        self._fog_amount = 0

    def set_colors(self, sky_top: QColor, sky_bot: QColor, #vers 1
                   ambient: QColor, sun_core: QColor, fog: int = 0):
        self._sky_top    = sky_top
        self._sky_bot    = sky_bot
        self._ambient    = ambient
        self._sun_core   = sun_core
        self._fog_amount = fog
        self.update()

    def paintEvent(self, event): #vers 1
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        w, h = self.width(), self.height()

        # Sky gradient
        grad = QLinearGradient(0, 0, 0, h)
        grad.setColorAt(0.0, self._sky_top)
        grad.setColorAt(1.0, self._sky_bot)
        p.fillRect(self.rect(), QBrush(grad))

        # Sun circle
        sun_x, sun_y = int(w * 0.7), int(h * 0.3)
        p.setBrush(QBrush(self._sun_core))
        p.setPen(Qt.PenStyle.NoPen)
        p.drawEllipse(sun_x - 18, sun_y - 18, 36, 36)

        # Fog overlay
        if self._fog_amount > 0:
            fog_alpha = min(200, int(self._fog_amount * 0.8))
            fog_color = QColor(200, 210, 220, fog_alpha)
            p.fillRect(self.rect(), fog_color)

        # Ambient swatch
        p.fillRect(4, h - 22, 40, 18, self._ambient)
        p.setPen(QColor(200, 200, 200))
        p.setFont(QFont("Arial", 7))
        p.drawText(48, h - 8, "Ambient")


# Editor

class _RotatedHeaderView(QHeaderView): #vers 1
    """Horizontal header that draws section labels rotated 90° to save width."""
    def __init__(self, parent=None):
        super().__init__(Qt.Orientation.Horizontal, parent)
        self.setSectionResizeMode(QHeaderView.ResizeMode.Fixed)

    def sizeHint(self):
        s = super().sizeHint()
        return QSize(s.width(), 80)  # tall enough for rotated text

    def paintSection(self, painter, rect, logical_index): #vers 1
        painter.save()
        # Draw background using standard option
        option = QStyleOptionHeader()
        self.initStyleOption(option)
        option.rect = rect
        option.section = logical_index
        option.text = ""
        self.style().drawControl(
            QStyle.ControlElement.CE_Header, option, painter, self)
        # Draw rotated text
        text = self.model().headerData(logical_index, Qt.Orientation.Horizontal)
        if text:
            painter.translate(rect.left() + rect.width() / 2, rect.bottom() - 4)
            painter.rotate(-90)
            painter.drawText(0, 0, str(text))
        painter.restore()


class TimecycWorkshop(RibbonMixin, GUIWorkshop): #vers 2
    App_name   = "Time Cycle Workshop"
    App_build  = "Build 2"
    App_auth   = "X-Seti"
    config_key = "timecyc_editor"
    _ribbon_name = "timecyc_editor"
    # Bump when the set of ribbons changes (1 = File/Edit/Tools)
    _RIBBON_LAYOUT_VERSION = 1

    def __init__(self, main_window=None, parent=None):
        self._defer_setup_ui = True
        super().__init__(parent)
        self.main_window    = main_window
        self._parser        = TimecycParser()
        self._current_path: Optional[str]  = None
        self._current_row:  Optional[TimecycRow] = None
        self._modified      = False
        self._field_widgets: Dict[str, QWidget] = {}
        self._colour_swatches: Dict[str, QLabel] = {}
        self._blocking      = False
        self._undo_stack, self._redo_stack = [], []
        self._last_undo_key = None
        self._clip = None
        self.setup_ui()
        self.ribbon_restore_state()
        if main_window and hasattr(self, "toolbar"): self.toolbar.hide()
        self._set_status("Open a timecyc.dat file to begin")

    def _build_left_panel(self, parent: QWidget) -> QWidget: #vers 1
        w = QWidget(parent)
        lay = QVBoxLayout(w)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(4)

        # Header row: title + action buttons (collapse to icons when narrow)
        header = QHBoxLayout()
        header.setSpacing(2)
        self._grid_title_lbl = QLabel("Weather / Time Grid")
        self._grid_title_lbl.setStyleSheet("font-weight: bold;")
        header.addWidget(self._grid_title_lbl)
        header.addStretch()

        def _make_btn(text, icon_text, tooltip, callback, enabled=True):
            btn = QPushButton(text)
            btn.setToolTip(tooltip)
            btn.setEnabled(enabled)
            btn.clicked.connect(callback)
            btn.setFixedHeight(26)
            btn.setMinimumWidth(44)
            btn.setCheckable(False)
            return btn

        self._btn_convert = _make_btn("Convert", "Conv", "Convert between game formats", self._convert_dialog)
        self._btn_load    = _make_btn("Load",    "Load", "Load timecyc.dat",             self._open_file)
        self._btn_save    = _make_btn("Save",    "Save", "Save timecyc.dat",             self._save_file)
        self._btn_import  = _make_btn("Import",  "Imp", "Import from another format",   self._import_file)
        self._btn_export  = _make_btn("Export",  "Exp", "Export to another format",     self._export_file)

        # Apply SVG icons
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVG
            ic = '#cccccc'
            self._btn_convert.setIcon(_SVG.convert_icon(16, ic))
            self._btn_load.setIcon(_SVG.open_icon(16, ic))
            self._btn_save.setIcon(_SVG.save_icon(16, ic))
            self._btn_import.setIcon(_SVG.import_icon(16, ic))
            self._btn_export.setIcon(_SVG.export_icon(16, ic))
        except Exception:
            pass

        for btn in (self._btn_convert, self._btn_load, self._btn_save,
                    self._btn_import, self._btn_export):
            header.addWidget(btn)

        lay.addLayout(header)

        # Grid: rows=time, cols=weather
        self._grid = QTableWidget(24, 8)
        self._grid.setHorizontalHeader(_RotatedHeaderView(self._grid))
        self._grid.setHorizontalHeaderLabels(WEATHER_NAMES_VC)
        self._grid.setVerticalHeaderLabels(TIME_LABELS)
        self._grid.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Fixed)
        for c in range(8):
            self._grid.setColumnWidth(c, 70)
        self._grid.verticalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Fixed)
        self._grid.verticalHeader().setDefaultSectionSize(20)
        self._grid.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        self._grid.currentCellChanged.connect(self._on_cell_selected)
        lay.addWidget(self._grid)

        return w

    def _build_centre_panel(self, parent: QWidget) -> QWidget: #vers 2
        scroll = QScrollArea(parent)
        scroll.setWidgetResizable(True)
        self._form_container = QWidget()
        scroll.setWidget(self._form_container)
        self._form_layout = QVBoxLayout(self._form_container)
        self._form_layout.setContentsMargins(8, 8, 8, 8)
        self._form_layout.setSpacing(8)
        self._field_widgets.clear()
        self._build_field_groups(VC_COLOUR_GROUPS, VC_SCALAR_FIELDS, VC_COLOUR_GROUPS_2)
        return scroll

    def _build_field_groups(self, cg, sf, cg2): #vers 1
        """Populate _form_layout with colour group boxes and scalar fields."""
        lay = self._form_layout

        for group_name, r_idx in cg + cg2:
            grp = QGroupBox(group_name)
            grp_lay = QHBoxLayout(grp)
            for component, offset in [('R', 0), ('G', 1), ('B', 2)]:
                key = f"{group_name}_{component}"
                col_lay = QVBoxLayout()
                lbl = QLabel(component)
                lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
                sp = QSpinBox()
                sp.setRange(0, 255)
                sp.setFixedWidth(60)
                sp.valueChanged.connect(lambda v, k=key: self._on_field_changed(k, v))
                self._field_widgets[key] = sp
                col_lay.addWidget(lbl)
                col_lay.addWidget(sp)
                grp_lay.addLayout(col_lay)
            swatch = QLabel()
            swatch.setFixedSize(40, 40)
            swatch.setStyleSheet("background: rgb(0,0,0); border: 1px solid #555;")
            self._colour_swatches[group_name] = swatch
            grp_lay.addWidget(swatch)
            lay.addWidget(grp)

        scalar_grp = QGroupBox("Atmosphere")
        scalar_form = QFormLayout(scalar_grp)
        for fname, idx, fmin, fmax in sf:
            sp = QSpinBox()
            sp.setRange(fmin, fmax)
            sp.valueChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))
            self._field_widgets[fname] = sp
            scalar_form.addRow(QLabel(fname), sp)
        lay.addWidget(scalar_grp)

    def _build_right_panel(self, parent: QWidget) -> QWidget: #vers 1
        w = QWidget(parent)
        lay = QVBoxLayout(w)
        lay.setContentsMargins(4, 4, 4, 4)

        lay.addWidget(QLabel("Sky Preview"))
        self._sky_preview = SkyPreviewWidget()
        self._sky_preview.setMinimumHeight(160)
        lay.addWidget(self._sky_preview)

        lay.addWidget(QLabel("Current Cell"))
        self._cell_info = QLabel("—")
        self._cell_info.setWordWrap(True)
        self._cell_info.setFont(QFont("Monospace", 8))
        lay.addWidget(self._cell_info)
        lay.addStretch()
        return w

    def _create_centre_panel(self): #vers 3
        self._tc_splitter = QSplitter(Qt.Orientation.Horizontal)
        sp = self._tc_splitter
        sp.addWidget(self._build_left_panel(self))    # grid + button bar
        right = QSplitter(Qt.Orientation.Vertical)
        right.addWidget(self._build_centre_panel(self))  # colour fields
        right.addWidget(self._build_right_panel(self))   # sky preview
        right.setSizes([600, 200])
        sp.addWidget(right)
        sp.setSizes([420, 730])
        sp.splitterMoved.connect(self._on_splitter_moved)
        return sp

    def _on_splitter_moved(self, pos: int, index: int): #vers 1
        """Collapse/expand button labels as grid panel resizes."""
        if hasattr(self, '_btn_load'):
            sizes = self._tc_splitter.sizes()
            left_w = sizes[0] if sizes else 400
            self._update_action_btns(left_w < 380)

    def _get_field_groups(self): #vers 2
        """Return (colour_groups, scalar_fields, colour_groups_2) for current game."""
        game = self._parser.game if hasattr(self._parser, 'game') else 'VC'
        if game == 'SA':
            return SA_COLOUR_GROUPS, SA_SCALAR_FIELDS, SA_COLOUR_GROUPS_2
        if game == 'GTA3':
            return GTA3_COLOUR_GROUPS, GTA3_SCALAR_FIELDS, GTA3_COLOUR_GROUPS_2
        return VC_COLOUR_GROUPS, VC_SCALAR_FIELDS, VC_COLOUR_GROUPS_2

    def _rebuild_field_widgets(self): #vers 2
        """Rebuild centre panel field widgets for current game."""
        cg, sf, cg2 = self._get_field_groups()
        lay = self._form_layout
        # Clear all existing widgets from layout
        self._field_widgets.clear()
        self._colour_swatches.clear()
        while lay.count():
            item = lay.takeAt(0)
            if item.widget():
                item.widget().deleteLater()
        # Rebuild for current game
        self._build_field_groups(cg, sf, cg2)

    def _open_file(self, path=None): #vers 4
        if path is False or path is True: path = None  # Qt passes checked=bool from button signal
        if path is None:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open timecyc.dat / timecycp.dat", "",
                "DAT files (timecyc.dat timecycp.dat *.dat);;All files (*)")
        if not path: return
        parser = TimecycParser()
        if not parser.load(path):
            QMessageBox.critical(self, "Error", f"Failed to load {path}"); return
        self._parser = parser
        self._current_path = path
        self._current_row = None
        self._undo_stack.clear(); self._redo_stack.clear(); self._last_undo_key = None
        game = self._parser.game
        # Resize grid to match actual game data
        n_weathers, n_times = self._parser._get_game_layout()
        if game == 'SA':
            weathers    = WEATHER_NAMES_SA
            time_labels = SA_TIME_LABELS
        elif game == 'GTA3':
            weathers    = WEATHER_NAMES_GTA3
            time_labels = [f"{h*2:02d}:00" for h in range(n_times)]
        else:
            weathers    = WEATHER_NAMES_VC
            time_labels = TIME_LABELS
        self._grid.setRowCount(n_times)
        self._grid.setColumnCount(n_weathers)
        self._grid.setHorizontalHeaderLabels(weathers)
        self._grid.setVerticalHeaderLabels(time_labels)
        for c in range(n_weathers):
            self._grid.setColumnWidth(c, 48)
        self._rebuild_field_widgets()
        self._populate_grid()
        self._update_modified()
        self._set_status(f"Loaded {os.path.basename(path)} - {len(self._parser.rows)} rows [{game}]")

    def _save_file(self, _checked=False): #vers 4
        """Back up the existing file, then write it atomically."""
        if not self._parser.rows:
            self._set_status("Nothing to save"); return
        if not self._current_path:
            self._save_as(); return
        if not self._parser.dirty:
            self._set_status("Nothing to save"); return
        from apps.methods.file_backup import backup_file, note_change
        if os.path.exists(self._current_path):
            note_change(f"Save {os.path.basename(self._current_path)}")
            if backup_file(self._current_path) is None:
                QMessageBox.warning(self, "Save", "Backup failed - file not overwritten.")
                return
        if self._parser.save(self._current_path):
            self._update_modified()
            self._set_status(f"Saved {os.path.basename(self._current_path)}")
        else:
            QMessageBox.critical(self, "Save Error", f"Could not save to:\n{self._current_path}")

    def _save_as(self): #vers 2
        if not self._parser.rows:
            return
        p, _ = QFileDialog.getSaveFileName(
            self, "Save timecyc.dat as", self._current_path or "timecyc.dat", "DAT files (*.dat);;All files (*)")
        if not p:
            return
        from apps.methods.file_backup import backup_file, note_change
        if os.path.exists(p):
            note_change(f"Save {os.path.basename(p)}")
            if backup_file(p) is None:
                QMessageBox.warning(self, "Save", "Backup failed - file not overwritten.")
                return
        if self._parser.save(p):
            self._current_path = p
            self._update_modified()
            self._set_status(f"Saved as {os.path.basename(p)}")
        else:
            QMessageBox.critical(self, "Save Error", f"Could not save to:\n{p}")

    def _populate_grid(self): #vers 3
        n_times    = self._grid.rowCount()
        n_weathers = self._grid.columnCount()
        game = self._parser.game
        if game == 'SA':   sky_idx = 9
        elif game == 'GTA3': sky_idx = 6
        else:              sky_idx = 15  # VC
        for row in self._parser.rows:
            t, w = row.time, row.weather
            if t >= n_times or w >= n_weathers:
                continue
            vals = row.values
            if len(vals) > sky_idx + 2:
                r = max(0, min(255, int(vals[sky_idx])))
                g = max(0, min(255, int(vals[sky_idx+1])))
                b = max(0, min(255, int(vals[sky_idx+2])))
            elif len(vals) >= 3:
                r = max(0, min(255, int(vals[0])))
                g = max(0, min(255, int(vals[1])))
                b = max(0, min(255, int(vals[2])))
            else:
                r = g = b = 0
            item = self._grid.item(t, w) or QTableWidgetItem()
            item.setBackground(QColor(r, g, b))
            item.setText("")
            self._grid.setItem(t, w, item)
        # Set row heights
        for t in range(n_times):
            self._grid.setRowHeight(t, 24)

    def _on_cell_selected(self, row: int, col: int, *_): #vers 1
        r = self._parser.get_row(weather=col, time=row)
        self._current_row = r
        if r is None:
            return
        time_lbl = self._grid.verticalHeaderItem(row)
        time_str = time_lbl.text() if time_lbl else f"{row:02d}:00"
        self._cell_info.setText(f"Time: {time_str}  Weather: {col}")
        self._populate_fields(r)

    def _populate_fields(self, row: TimecycRow): #vers 2
        self._blocking = True
        vals = row.values
        cg, sf, cg2 = self._get_field_groups()

        for group_name, r_idx in cg + cg2:
            for ci, comp in enumerate(['R', 'G', 'B']):
                key = f"{group_name}_{comp}"
                w = self._field_widgets.get(key)
                if w and r_idx + ci < len(vals):
                    w.setValue(int(vals[r_idx + ci]))
            # Update swatch
            swatch = self._colour_swatches.get(group_name)
            if swatch and r_idx + 2 < len(vals):
                r2, g2, b2 = int(vals[r_idx]), int(vals[r_idx+1]), int(vals[r_idx+2])
                swatch.setStyleSheet(f"background: rgb({r2},{g2},{b2}); border: 1px solid #555;")

        for fname, idx, *_ in sf:
            w = self._field_widgets.get(fname)
            if w and idx < len(vals):
                w.setValue(int(float(vals[idx])))

        self._blocking = False
        self._update_preview(row)

    def _on_field_changed(self, key: str, value: int): #vers 1
        if self._blocking or self._current_row is None:
            return

        # Update values in current row
        vals = self._current_row.values
        self._push_undo((id(self._current_row), key))

        # Colour group field
        cg, sf, cg2 = self._get_field_groups()
        for group_name, r_idx in cg + cg2:
            for ci, comp in enumerate(['R', 'G', 'B']):
                if key == f"{group_name}_{comp}":
                    target = r_idx + ci
                    if target < len(vals):
                        vals[target] = value
                    # Update swatch
                    swatch = self._colour_swatches.get(group_name)
                    if swatch:
                        r2 = int(vals[r_idx]) if r_idx < len(vals) else 0
                        g2 = int(vals[r_idx+1]) if r_idx+1 < len(vals) else 0
                        b2 = int(vals[r_idx+2]) if r_idx+2 < len(vals) else 0
                        swatch.setStyleSheet(f"background: rgb({r2},{g2},{b2}); border: 1px solid #555;")
                    break

        # Scalar field
        for fname, idx, *_ in sf:
            if key == fname and idx < len(vals):
                vals[idx] = value
                break

        self._update_modified()
        self._update_preview(self._current_row)
        # Update grid cell colour using game-correct sky top index
        t, w2 = self._current_row.time, self._current_row.weather
        _g = self._parser.game
        _si = 9 if _g == 'SA' else (6 if _g == 'GTA3' else 15)
        if len(vals) > _si + 2:
            item = self._grid.item(t, w2) or QTableWidgetItem()
            item.setBackground(QColor(
                max(0, min(255, int(vals[_si]))),
                max(0, min(255, int(vals[_si+1]))),
                max(0, min(255, int(vals[_si+2])))))
            self._grid.setItem(t, w2, item)

    def _update_preview(self, row: TimecycRow): #vers 3
        vals = row.values
        game = self._parser.game
        def rgb(idx): return QColor(
            max(0,min(255,int(float(vals[idx]))))   if idx   < len(vals) else 0,
            max(0,min(255,int(float(vals[idx+1])))) if idx+1 < len(vals) else 0,
            max(0,min(255,int(float(vals[idx+2])))) if idx+2 < len(vals) else 0)
        def sv(idx): return int(float(vals[idx])) if idx < len(vals) else 0
        if game == 'SA':
            sky_top  = rgb(9)   # SA Sky Top  [9-11]
            sky_bot  = rgb(12)  # SA Sky Bot  [12-14]
            ambient  = rgb(0)   # SA Ambient  [0-2]
            sun_core = rgb(12)  # SA Sun Core [15-17]
            fog      = sv(28)   # SA Fog Start[28]
        elif game == 'GTA3':
            sky_top  = rgb(6)   # GTA3 Sky Top  [6-8]
            sky_bot  = rgb(9)   # GTA3 Sky Bot  [9-11]
            ambient  = rgb(0)   # GTA3 Ambient  [0-2]
            sun_core = rgb(12)  # GTA3 Sun Core [12-14]
            fog      = sv(25)   # GTA3 Fog Start[25]
        else:  # VC
            sky_top  = rgb(15)  # VC Sky Top  [15-17]
            sky_bot  = rgb(18)  # VC Sky Bot  [18-20]
            ambient  = rgb(0)   # VC Ambient  [0-2]
            sun_core = rgb(21)  # VC Sun Core [21-23]
            fog      = sv(34)   # VC Fog Start[34]
        self._sky_preview.set_colors(sky_top, sky_bot, ambient, sun_core, fog)

    # -- tools
    def _row_at(self, weather: int, time: int):
        return self._parser.get_row(weather=weather, time=time)

    def _selected_cell(self):
        r, c = self._grid.currentRow(), self._grid.currentColumn()
        return (c, r) if r >= 0 and c >= 0 else None     # (weather, time)

    def _copy_cell(self): #vers 1
        cell = self._selected_cell()
        row = self._row_at(*cell) if cell else None
        if row is None:
            self._set_status("Select a preset first")
            return
        self._clip = list(row.values)
        self._set_status("Preset copied")

    def _paste_cell(self): #vers 1
        cell = self._selected_cell()
        row = self._row_at(*cell) if cell else None
        if row is None or self._clip is None:
            self._set_status("Copy a preset, then select where to paste")
            return
        self._push_undo()
        row.values = [self._clip[i] if i < len(self._clip) else v for i, v in enumerate(row.values)]
        self._populate_grid()
        self._populate_fields(row)
        self._update_modified()
        self._set_status("Preset pasted")

    def _copy_weather(self): #vers 1
        cell = self._selected_cell()
        if not cell:
            self._set_status("Select a preset of the weather to copy")
            return
        from PyQt6.QtWidgets import QInputDialog
        n_w = self._grid.columnCount()
        names = [self._grid.horizontalHeaderItem(c).text() if self._grid.horizontalHeaderItem(c) else str(c)
                 for c in range(n_w)]
        pick, ok = QInputDialog.getItem(self, "Copy weather", f"Copy '{names[cell[0]]}' onto:", names, 0, False)
        if not ok:
            return
        dst = names.index(pick)
        if dst == cell[0]:
            return
        self._push_undo()
        n = 0
        for t in range(self._grid.rowCount()):
            a, b = self._row_at(cell[0], t), self._row_at(dst, t)
            if a is not None and b is not None:
                b.values = list(a.values)
                n += 1
        self._populate_grid()
        self._update_modified()
        self._set_status(f"Copied {n} presets from {names[cell[0]]} to {pick}")

    def _blend_times(self): #vers 1
        """Linear blend of every value between the first and last selected
        time of the current weather (select the two end presets with Ctrl+click)."""
        cell = self._selected_cell()
        sel = sorted({(i.column(), i.row()) for i in self._grid.selectedIndexes()})
        if not cell or len(sel) < 2 or len({w for w, _ in sel}) != 1:
            QMessageBox.information(self, "Blend",
                "Ctrl+click two presets of the SAME weather (the first and last time), then Blend.")
            return
        w = sel[0][0]
        t0, t1 = sel[0][1], sel[-1][1]
        a, b = self._row_at(w, t0), self._row_at(w, t1)
        if a is None or b is None or t1 - t0 < 2:
            return
        self._push_undo()
        for t in range(t0 + 1, t1):
            r = self._row_at(w, t)
            if r is None:
                continue
            f = (t - t0) / (t1 - t0)
            r.values = [type(x)(round(x + (y - x) * f, 4)) if isinstance(x, float) else int(round(x + (y - x) * f))
                        for x, y in zip(a.values, b.values)] + list(r.values[len(a.values):])
        self._populate_grid()
        self._update_modified()
        self._set_status(f"Blended {t1 - t0 - 1} preset(s) between times {t0} and {t1}")

    def _tint_dialog(self): #vers 1
        cg, sf, cg2 = self._get_field_groups()
        groups = cg + cg2
        if not self._parser.rows:
            return
        dlg = QDialog(self)
        dlg.setWindowTitle("Tint a colour group")
        fl = QFormLayout(dlg)
        gsel = QComboBox(); gsel.addItems([g for g, _ in groups])
        n_w = self._grid.columnCount()
        wsel = QComboBox(); wsel.addItem("All weathers")
        for c in range(n_w):
            it = self._grid.horizontalHeaderItem(c)
            wsel.addItem(it.text() if it else str(c))
        cur = self._selected_cell()
        if cur:
            wsel.setCurrentIndex(cur[0] + 1)
        sp = []
        for lab in ("Red", "Green", "Blue"):
            d = QSpinBox(); d.setRange(-255, 255)
            fl.addRow(f"Add {lab}:", d); sp.append(d)
        fl.insertRow(0, "Colour group:", gsel)
        fl.insertRow(1, "Weather:", wsel)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        fl.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        d = [x.value() for x in sp]
        if not any(d):
            return
        base = groups[gsel.currentIndex()][1]
        wi = wsel.currentIndex() - 1
        self._push_undo()
        n = 0
        for r in self._parser.rows:
            if wi >= 0 and r.weather != wi:
                continue
            for k in range(3):
                if base + k < len(r.values):
                    r.values[base + k] = max(0, min(255, int(r.values[base + k]) + d[k]))
            n += 1
        self._populate_grid()
        if self._current_row is not None:
            self._populate_fields(self._current_row)
        self._update_modified()
        self._set_status(f"Tinted {gsel.currentText()} on {n} preset(s)")

    def _export_file(self): #vers 2
        """Export current timecyc to a different game format."""
        if not self._parser.rows:
            self._set_status("No file loaded — nothing to export"); return
        self._convert_dialog(export_mode=True)

    def _import_file(self): #vers 2
        """Import a timecyc from a different game format and convert."""
        self._convert_dialog(import_mode=True)

    def _convert_dialog(self, export_mode=False, import_mode=False): #vers 2
        if export_mode is True and import_mode is False: export_mode = False  # Qt checked signal
        """Convert timecyc between GTA3 / VC / SA formats."""
        dlg = QDialog(self)
        dlg.setWindowTitle("Convert Timecyc Format")
        dlg.setMinimumWidth(380)
        lay = QVBoxLayout(dlg)

        src_game = self._parser.game if self._parser.rows else "VC"
        games = ["GTA3", "VC", "SA"]

        # Source
        lay.addWidget(QLabel(f"Source format: <b>{src_game}</b>"))

        # Target
        tgt_row = QHBoxLayout()
        tgt_row.addWidget(QLabel("Convert to:"))
        tgt_combo = QComboBox()
        for g in games:
            if g != src_game: tgt_combo.addItem(g)
        tgt_row.addWidget(tgt_combo)
        lay.addLayout(tgt_row)

        # Info label
        info = QLabel("")
        info.setWordWrap(True)
        info.setStyleSheet("color: #aaa; font-size: 10px;")
        lay.addWidget(info)

        def _update_info():
            tgt = tgt_combo.currentText()
            notes = {
                ("VC","SA"):  "VC→SA: field reorder, 24 times→8 slots, add color correction (zeroed).",
                ("SA","VC"):  "SA→VC: field reorder, 8 slots→24 times (interpolated), drop color correction.",
                ("GTA3","VC"):"GTA3→VC: same structure, expand from 4→7 weathers (extras zeroed).",
                ("VC","GTA3"):"VC→GTA3: truncate to 4 weathers.",
                ("GTA3","SA"):"GTA3→SA: field reorder + weather/time expansion.",
                ("SA","GTA3"):"SA→GTA3: field reorder + truncate.",
            }
            info.setText(notes.get((src_game, tgt), ""))
        tgt_combo.currentTextChanged.connect(_update_info)
        _update_info()

        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                                QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        lay.addWidget(btns)

        if dlg.exec() != QDialog.DialogCode.Accepted: return

        tgt_game = tgt_combo.currentText()
        out_path, _ = QFileDialog.getSaveFileName(
            self, f"Save as {tgt_game} timecyc", "timecyc.dat",
            "DAT files (*.dat);;All files (*)")
        if not out_path: return

        ok, msg = self._do_convert(tgt_game, out_path)
        if ok:
            self._set_status(f"Converted {src_game}→{tgt_game}: {os.path.basename(out_path)}")
        else:
            QMessageBox.critical(self, "Convert Failed", msg)

    def _do_convert(self, tgt_game: str, out_path: str): #vers 1
        """Perform field-remapping conversion between game formats."""
        src = self._parser.game
        rows = self._parser.rows
        if not rows: return False, "No data loaded"

        try:
            out_lines = []
            for row in rows:
                v = list(row.values)
                nv = self._remap_fields(v, src, tgt_game)
                out_lines.append(' '.join(str(x) for x in nv))

            with open(out_path, 'w') as f:
                f.write(f"// Converted from {src} to {tgt_game} by Timecyc Workshop\n")
                for ln in out_lines:
                    f.write(ln + '\n')
            return True, ""
        except Exception as ex:
            return False, str(ex)

    def _remap_fields(self, v: list, src: str, tgt: str) -> list: #vers 1
        """Remap field values from src game layout to tgt game layout.
        All games share: Ambient[0-2], then diverge.
        VC layout: Amb[0-2] AmbDyn[3-5] AmbBlur[6-8] AmbBlurDyn[9-11] Dir[12-14]
                   SkyTop[15-17] SkyBot[18-20] SunCore[21-23] SunCorona[24-26]
                   SunSz[27] SpriteSz[28] SpriteBright[29] Shadow[30] Light[31]
                   Pole[32] FarClip[33] FogStart[34] LightGnd[35]
                   LowCloud[36-38] UpCloudTop[39-41] UpCloudBot[42-44]
                   Blur[45-47] Water[48-50] WaterAlpha[51]
        SA layout: Amb[0-2] AmbObj[3-5] Dir[6-8] SkyTop[9-11] SkyBot[12-14]
                   SunCore[15-17] SunCorona[18-20] SunSz[21] SpriteSz[22]
                   SpriteBright[23] Shadow[24] Light[25] Pole[26]
                   FarClip[27] FogStart[28] LightGnd[29]
                   LowCloud[30-32] BottomCloud[33-35] Water[36-38] WaterAlpha[39]
                   CC1Alpha[40] CC1RGB[41-43] CC2Alpha[44] CC2RGB[45-47] CloudAlpha[48]
        """
        def g(lst, i, d=0):
            return lst[i] if i < len(lst) else d
        def rgb(lst, i):
            return [g(lst,i), g(lst,i+1), g(lst,i+2)]

        if src == 'VC' and tgt == 'SA':
            return (rgb(v,0) + rgb(v,3) +          # Amb, AmbObj (from AmbDyn)
                    rgb(v,12) +                      # Dir
                    rgb(v,15) + rgb(v,18) +          # SkyTop, SkyBot
                    rgb(v,21) + rgb(v,24) +          # SunCore, SunCorona
                    [g(v,27), g(v,28), g(v,29),     # SunSz, SpriteSz, SpriteBright
                     g(v,30), g(v,31), g(v,32),     # Shadow, Light, Pole
                     g(v,33), g(v,34), g(v,35)] +   # FarClip, FogStart, LightGnd
                    rgb(v,36) + rgb(v,39) +          # LowCloud, BottomCloud (UpCloudTop)
                    rgb(v,48) + [g(v,51)] +          # Water RGBA
                    [0, 0,0,0, 0, 0,0,0, 0])        # CC1, CC2, CloudAlpha (zeroed)

        elif src == 'SA' and tgt == 'VC':
            return (rgb(v,0) + rgb(v,3) +           # AmbStatic, AmbDyn
                    rgb(v,0) + rgb(v,3) +            # AmbBlur, AmbBlurDyn (copy ambient)
                    rgb(v,6) +                        # Dir
                    rgb(v,9) + rgb(v,12) +           # SkyTop, SkyBot
                    rgb(v,15) + rgb(v,18) +          # SunCore, SunCorona
                    [g(v,21), g(v,22), g(v,23),     # SunSz, SpriteSz, SpriteBright
                     g(v,24), g(v,25), g(v,26),     # Shadow, Light, Pole
                     g(v,27), g(v,28), g(v,29)] +   # FarClip, FogStart, LightGnd
                    rgb(v,30) + rgb(v,33) +          # LowCloud, UpCloudTop (BottomCloud)
                    [0,0,0] +                         # UpCloudBot (zeroed)
                    [0,0,0] +                         # Blur (zeroed)
                    rgb(v,36) + [g(v,39)])           # Water RGB + Alpha

        elif src == 'GTA3' and tgt == 'VC':
            # GTA3 same layout as VC but 40 fields — pad missing fields with 0
            return list(v) + [0] * (52 - len(v))

        elif src == 'VC' and tgt == 'GTA3':
            return list(v)[:40]

        elif src == 'GTA3' and tgt == 'SA':
            # GTA3→VC first, then VC→SA
            vc = list(v) + [0] * (52 - len(v))
            return self._remap_fields(vc, 'VC', 'SA')

        elif src == 'SA' and tgt == 'GTA3':
            vc = self._remap_fields(v, 'SA', 'VC')
            return vc[:40]

        return list(v)  # same game, no change

    def setup_ui(self): #vers 8
        """Titlebar / [grid | fields + sky preview] inside the ribbon host / status bar."""
        ml = QVBoxLayout(self)
        ml.setContentsMargins(*self.get_content_margins())
        ml.setSpacing(self.setspacing)
        ml.addWidget(self._create_toolbar())
        ml.addWidget(self.ribbon_wrap(self._create_centre_panel()), 1)
        self._build_ribbons()
        self._status_widget = self._create_status_bar()
        ml.addWidget(self._status_widget)
        self._status_widget.setVisible(self.WS.get("show_statusbar", True))
        self._set_action_btns_visible(False)      # the ribbons replace the old button bar

    def _create_toolbar(self): #vers 2
        tb = super()._create_toolbar()
        for name in ("open_btn", "save_btn", "export_btn", "import_btn"):
            btn = getattr(self, name, None)
            if btn:
                btn.setVisible(False)
        return tb

    def _build_ribbons(self): #vers 1
        B = self.ribbon_button
        tb = self.ribbon_toolbar("File")
        B(tb, "open_icon",   "Open timecyc.dat / timecycp.dat  (Ctrl+O)", self._open_file)
        self.save_btn = B(tb, "save_icon", "Save  (Ctrl+S) - backs up the old file first", self._save_file, enabled=False)
        B(tb, "saveas_icon", "Save As...", self._save_as)
        tb.addSeparator()
        B(tb, "convert_icon", "Convert to another game's layout...", self._convert_dialog)
        B(tb, "import_icon",  "Import from another format", self._import_file)
        B(tb, "export_icon",  "Export to another format", self._export_file)

        tb = self.ribbon_toolbar("Edit")
        B(tb, "undo_icon", "Undo  (Ctrl+Z)", self._undo)
        B(tb, "redo_icon", "Redo  (Ctrl+Y)", self._redo)
        tb.addSeparator()
        B(tb, "copy_icon",  "Copy the selected time/weather preset", self._copy_cell)
        B(tb, "paste_icon", "Paste it onto the selected preset", self._paste_cell)

        tb = self.ribbon_toolbar("Tools")
        B(tb, "convert_icon", "Tint a colour group: add R/G/B on one weather (or all)...", self._tint_dialog, text="Tint")
        B(tb, "edit_icon",    "Blend the times between two selected presets of a weather", self._blend_times, text="Blend")
        B(tb, "package_icon", "Copy the selected weather onto another weather (all times)...", self._copy_weather, text="Wthr")

    def closeEvent(self, ev): #vers 1
        if self._parser.dirty:
            r = QMessageBox.question(
                self, "Time Cycle Workshop", "Save changes before closing?",
                QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard
                | QMessageBox.StandardButton.Cancel)
            if r == QMessageBox.StandardButton.Cancel:
                ev.ignore()
                return
            if r == QMessageBox.StandardButton.Save:
                self._save_file()
                if self._parser.dirty:
                    ev.ignore()
                    return
        self.ribbon_save_state()
        super().closeEvent(ev)

    def _update_modified(self): #vers 1
        self._modified = self._parser.dirty
        if hasattr(self, 'save_btn'):
            self.save_btn.setEnabled(self._modified)

    # -- undo: snapshot of every preset's values (a run of slider moves = one step)
    def _snap(self):
        return [(r, list(r.values)) for r in self._parser.rows]

    def _push_undo(self, key=None):
        if key is not None and self._last_undo_key == key:
            return
        self._last_undo_key = key
        self._undo_stack.append(self._snap())
        del self._undo_stack[:-60]
        self._redo_stack.clear()

    def _apply_snap(self, snap):
        for r, v in snap:
            r.values = list(v)
        self._populate_grid()
        if self._current_row is not None:
            self._populate_fields(self._current_row)
        self._update_modified()

    def _undo(self): #vers 1
        if not self._undo_stack:
            self._set_status("Nothing to undo")
            return
        self._redo_stack.append(self._snap())
        self._last_undo_key = None
        self._apply_snap(self._undo_stack.pop())

    def _redo(self): #vers 1
        if not self._redo_stack:
            self._set_status("Nothing to redo")
            return
        self._undo_stack.append(self._snap())
        self._last_undo_key = None
        self._apply_snap(self._redo_stack.pop())

    def _set_action_btns_visible(self, visible: bool): #vers 1
        """Show action button bar only when docked in IMG Factory.
        In standalone the GUIWorkshop toolbar handles open/save."""
        for btn in ('_btn_convert','_btn_load','_btn_save','_btn_import','_btn_export'):
            b = getattr(self, btn, None)
            if b: b.setVisible(visible)
        # When docked, hide the plain title - buttons provide context
        if hasattr(self, '_grid_title_lbl'):
            self._grid_title_lbl.setVisible(not visible)

    def _update_action_btns(self, narrow: bool): #vers 1
        """Collapse action buttons to icons when panel is narrow (<500px)."""
        labels = {
            self._btn_convert: ("Convert", "Conv"),
            self._btn_load:    ("Load",    "Load"),
            self._btn_save:    ("Save",    "Save"),
            self._btn_import:  ("Import",  "Imp"),
            self._btn_export:  ("Export",  "Exp"),
        }
        for btn, (full, icon) in labels.items():
            btn.setText(icon if narrow else full)

    def resizeEvent(self, ev): #vers 3
        super().resizeEvent(ev)
        if hasattr(self, '_btn_load') and hasattr(self, '_tc_splitter'):
            sizes = self._tc_splitter.sizes()
            left_w = sizes[0] if sizes else self.width()
            self._update_action_btns(left_w < 380)

    def _build_menus_into_qmenu(self, pm): #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open timecyc.dat", self._open_file)
        fm.addAction("Save", self._save_file)
        fm.addSeparator()
        fm.addAction("Close", self.close)


def open_timecyc_editor(main_window=None, path: str = None): #vers 1
    app = QApplication.instance() or QApplication(sys.argv)
    w = TimecycWorkshop(main_window)
    w.resize(1200, 720)
    w.show()
    if path:
        w._open_file(path)
    return w


if __name__ == '__main__':
    app = QApplication(sys.argv)
    w = TimecycWorkshop()
    w.resize(1200, 720); w.show()
    if len(sys.argv) > 1:
        w._open_file(sys.argv[1])
    else:
        from PyQt6.QtWidgets import QFileDialog
        p,_ = QFileDialog.getOpenFileName(w,'Open timecyc.dat','','DAT files (timecyc.dat timecycp.dat *.dat);;All (*)')
        if p: w._open_file(p)
    sys.exit(app.exec())
