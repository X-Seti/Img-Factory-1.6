#!/usr/bin/env python3
#this belongs in apps/components/Timecyc_Editor/timecyc_workshop.py - Version: 3
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

import sys, os
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
    QSlider, QGridLayout, QSizePolicy, QMenu
)
from PyQt6.QtCore import Qt, QRect
from PyQt6.QtGui import QFont, QColor, QPainter, QBrush, QLinearGradient

from apps.components.Timecyc_Editor.gui_workshop import GUIWorkshop


# Field definitions
WEATHER_NAMES_VC  = ["Sunny", "Cloudy", "Rainy", "Foggy", "ExtraSunny", "Rainy2", "ExtraColours"]
WEATHER_NAMES_GTA3 = ["ExtraS", "ExtraS2", "Sunny", "Cloudy", "Rainy", "Foggy", "ExtraS3", "ExtraS4"]
WEATHER_NAMES_SA  = ["ExtraSunny", "Sunny", "Cloudy", "Rainy", "Foggy", "ExtraColors", "Hurricane", "ExtraColors2"]

TIME_LABELS = [
    "00:00","01:00","02:00","03:00","04:00","05:00",
    "06:00","07:00","08:00","09:00","10:00","11:00",
    "12:00","13:00","14:00","15:00","16:00","17:00",
    "18:00","19:00","20:00","21:00","22:00","23:00",
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
class TimecycRow: #vers 1
    weather: int = 0
    time:    int = 0
    values:  List[int] = field(default_factory=lambda: [0] * 36)
    comment: str = ""


class TimecycParser: #vers 1
    def __init__(self): #vers 1
        self.rows:         List[TimecycRow] = []
        self.header_lines: List[str]        = []
        self.game:         str              = 'VC'
        self.cols_per_row: int              = 33

    def _detect_game(self, num_values: int) -> str: #vers 1
        # LC=40 fields, VC=52 fields, SA=51 fields
        if num_values >= 52: return 'VC'
        if num_values >= 51: return 'SA'
        if num_values >= 40: return 'GTA3'
        return 'GTA3' 

    def _parse_line(self, line: str, weather: int, time: int) -> Optional[TimecycRow]: #vers 1
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
            values = [int(float(p)) for p in parts]
        except ValueError:
            return None
        row = TimecycRow(weather=weather, time=time, values=values, comment=comment)
        return row

    def load(self, path: str) -> bool: #vers 1
        try:
            self.rows.clear()
            self.header_lines.clear()
            with open(path, 'r', encoding='latin-1') as f:
                lines = [ln for ln in f]

            # Detect format from first data line
            for ln in lines:
                s = ln.strip()
                if s and not s.startswith('/'):
                    parts = s.split()
                    if len(parts) >= 10:
                        self.game = self._detect_game(len(parts))
                        self.cols_per_row = len(parts)
                        break

            # Parse rows — weather-major ordering:
            # all times for weather0, then all times for weather1, etc.
            # GTA3: 8 weathers x 12 times = 96  (2-hour steps)
            # VC:   7 weathers x 24 times = 168
            # SA:   8 weathers x 23 times = 184
            if self.game == 'GTA3':   n_times = 12
            elif self.game == 'SA':   n_times = 8    # SA: 8 time slots per weather
            else:                     n_times = 24   # VC/GTA3

            data_lines = [ln for ln in lines if ln.strip() and not ln.strip().startswith('/')]
            comment_lines = [ln for ln in lines if ln.strip().startswith('/')]
            self.header_lines = comment_lines[:3]

            row_idx = 0
            for ln in data_lines:
                weather = row_idx // n_times
                time    = row_idx % n_times
                r = self._parse_line(ln, weather, time)
                if r:
                    self.rows.append(r)
                    row_idx += 1
            return True
        except Exception as ex:
            print(f"TimecycParser.load: {ex}")
            return False

    def save(self, path: str) -> bool: #vers 1
        try:
            with open(path, 'w', encoding='latin-1') as f:
                for ln in self.header_lines:
                    f.write(ln if ln.endswith('\n') else ln + '\n')
                # Sort: time-major order (time0/weather0..7, time1/weather0..7 ...)
                ordered = sorted(self.rows, key=lambda r: (r.time, r.weather))
                for r in ordered:
                    line = ' '.join(str(v) for v in r.values)
                    if r.comment:
                        line += f'  {r.comment}'
                    f.write(line + '\n')
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

class TimecycWorkshop(GUIWorkshop): #vers 1
    App_name   = "Time Cycle Workshop"
    App_build  = "Build 2"
    App_auth   = "X-Seti"
    config_key = "timecyc_editor"

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
        self.setup_ui()
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

    def _create_centre_panel(self): #vers 2
        sp = QSplitter(Qt.Orientation.Horizontal)
        sp.addWidget(self._build_left_panel(self))    # grid (now IS the centre)
        right = QSplitter(Qt.Orientation.Vertical)
        right.addWidget(self._build_centre_panel(self))  # colour fields
        right.addWidget(self._build_right_panel(self))   # sky preview
        right.setSizes([600, 200])
        sp.addWidget(right)
        sp.setSizes([420, 730])
        return sp

    def _get_field_groups(self): #vers 1
        """Return (colour_groups, scalar_fields, colour_groups_2) for current game."""
        game = self._parser.game if hasattr(self._parser, 'game') else 'VC'
        if game == 'SA':
            return SA_COLOUR_GROUPS, SA_SCALAR_FIELDS, SA_COLOUR_GROUPS_2
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

    def _open_file(self, path=None): #vers 2
        if path is None:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open timecyc.dat / timecycp.dat", "",
                "DAT files (timecyc.dat timecycp.dat *.dat);;All files (*)")
        if not path: return
        if not self._parser.load(path):
            QMessageBox.critical(self, "Error", f"Failed to load {path}"); return
        self._current_path = path
        self._modified = False
        # timecycp.dat is always SA PSP/PAL variant
        if os.path.basename(path).lower() == 'timecycp.dat':
            self._parser.game = 'SA'
        game = self._parser.game
        # Resize grid to match actual game data
        if game == 'SA':
            # SA: 8 time slots per weather (midnight,5am,6am,7am,noon,7pm,8pm,10pm)
            n_weathers, n_times = 8, 8
            weathers   = WEATHER_NAMES_SA
            time_labels = SA_TIME_LABELS
        elif game == 'GTA3':
            n_weathers, n_times = 8, 12
            weathers   = WEATHER_NAMES_GTA3
            time_labels = [f"{h*2:02d}:00" for h in range(n_times)]
        else:  # VC: 7 weathers x 24 hours
            n_weathers, n_times = 7, 24
            weathers   = WEATHER_NAMES_VC
            time_labels = TIME_LABELS
        self._grid.setRowCount(n_times)
        self._grid.setColumnCount(n_weathers)
        self._grid.setHorizontalHeaderLabels(weathers)
        self._grid.setVerticalHeaderLabels(time_labels)
        for c in range(n_weathers):
            self._grid.setColumnWidth(c, 70)
        self._rebuild_field_widgets()
        self._populate_grid()
        self._set_status(f"Loaded {os.path.basename(path)} — {len(self._parser.rows)} rows [{game}]")

    def _save_file(self): #vers 1
        if not self._current_path:
            self._current_path, _ = QFileDialog.getSaveFileName(
                self, "Save timecyc.dat", "", "DAT files (*.dat)")
        if not self._current_path:
            return
        if self._parser.save(self._current_path):
            self._modified = False
            self._set_status(f"Saved {os.path.basename(self._current_path)}")
        else:
            QMessageBox.critical(self, "Error", "Save failed")

    def _populate_grid(self): #vers 2
        n_times    = self._grid.rowCount()
        n_weathers = self._grid.columnCount()
        for row in self._parser.rows:
            t, w = row.time, row.weather
            if t < n_times and w < n_weathers:
                r, g, b = 0, 0, 0
                if len(row.values) >= 18:
                    # Use sky top colour — SA=[9-11], VC=[15-17]
                    sky_idx = 9 if self._parser.game == 'SA' else 15
                    r, g, b = row.values[sky_idx], row.values[sky_idx+1], row.values[sky_idx+2]
                elif len(row.values) >= 3:
                    r, g, b = row.values[0], row.values[1], row.values[2]
                item = QTableWidgetItem()
                item.setBackground(QColor(r, g, b))
                item.setText("")
                self._grid.setItem(t, w, item)

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

        self._modified = True
        self._update_preview(self._current_row)
        # Update grid cell colour
        t, w2 = self._current_row.time, self._current_row.weather
        if len(vals) >= 18:
            item = self._grid.item(t, w2) or QTableWidgetItem()
            item.setBackground(QColor(int(vals[15]), int(vals[16]), int(vals[17])))
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
            sun_core = rgb(15)  # SA Sun Core [15-17]
            fog      = sv(28)   # SA Fog Start[28]
        else:  # VC/GTA3
            sky_top  = rgb(15)  # VC Sky Top  [15-17]
            sky_bot  = rgb(18)  # VC Sky Bot  [18-20]
            ambient  = rgb(0)   # VC Ambient  [0-2]
            sun_core = rgb(21)  # VC Sun Core [21-23]
            fog      = sv(34)   # VC Fog Start[34]
        self._sky_preview.set_colors(sky_top, sky_bot, ambient, sun_core, fog)

    def _export_file(self): #vers 2
        """Export current timecyc to a different game format."""
        if not self._parser.rows:
            self._set_status("No file loaded — nothing to export"); return
        self._convert_dialog(export_mode=True)

    def _import_file(self): #vers 2
        """Import a timecyc from a different game format and convert."""
        self._convert_dialog(import_mode=True)

    def _convert_dialog(self, export_mode=False, import_mode=False): #vers 1
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

    def setup_ui(self): #vers 5
        super().setup_ui()
        # Wire GUIWorkshop toolbar open/save buttons
        if hasattr(self, 'open_btn'):
            self.open_btn.clicked.connect(self._open_file)
        if hasattr(self, 'save_btn'):
            self.save_btn.clicked.connect(self._save_file)
        # Disable export/import in toolbar (handled by our button bar)
        if hasattr(self, 'export_btn'): self.export_btn.setEnabled(False)
        if hasattr(self, 'import_btn'): self.import_btn.setEnabled(False)
        # Hide left panel action buttons when standalone (show only when docked)
        self._set_action_btns_visible(bool(self.main_window))

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

    def resizeEvent(self, ev): #vers 2
        super().resizeEvent(ev)
        if hasattr(self, '_btn_load') and hasattr(self, '_main_splitter'):
            # Use left panel width from splitter
            sizes = self._main_splitter.sizes()
            left_w = sizes[0] if sizes else self.width()
            self._update_action_btns(left_w < 380)
        elif hasattr(self, '_btn_load'):
            self._update_action_btns(self.width() < 500)

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
