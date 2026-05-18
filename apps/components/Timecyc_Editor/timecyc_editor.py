#!/usr/bin/env python3
#this belongs in apps/components/Timecyc_Editor/timecyc_editor.py - Version: 2
# X-Seti - May08 2026 - Img Factory 1.6 - Time Cycle Editor

"""
Time Cycle Editor — reads/writes GTA VC/SA timecyc.dat.
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
# TimecycEditor.__init__
# TimecycEditor._build_left_panel
# TimecycEditor._build_centre_panel
# TimecycEditor._build_right_panel
# TimecycEditor._open_file
# TimecycEditor._save_file
# TimecycEditor._on_cell_selected
# TimecycEditor._populate_fields
# TimecycEditor._on_field_changed
# TimecycEditor._update_preview
# TimecycEditor._build_menus_into_qmenu
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


# ─────────────────────────────────────────────────────────────────────────────
# Field definitions
# ─────────────────────────────────────────────────────────────────────────────

WEATHER_NAMES_VC = ["ExtraS", "ExtraS2", "Sunny", "Cloudy", "Rainy", "Foggy", "ExtraS3", "ExtraS4"]
WEATHER_NAMES_SA = ["ExtraSunny", "Sunny", "Cloudy", "Rainy", "Foggy", "ExtraColors", "Hurricane", "ExtraColors2"]

TIME_LABELS = [
    "00:00","01:00","02:00","03:00","04:00","05:00",
    "06:00","07:00","08:00","09:00","10:00","11:00",
    "12:00","13:00","14:00","15:00","16:00","17:00",
    "18:00","19:00","20:00","21:00","22:00","23:00",
]

# (group_name, [(field_name, r_idx)])
# Each group occupies 3 consecutive values (R, G, B) starting at r_idx
VC_COLOUR_GROUPS = [
    ("Ambient",       0),   # R G B
    ("Directional",   3),   # R G B
    ("Sky Top",       6),   # R G B
    ("Sky Bottom",    9),   # R G B
    ("Sun Core",      12),  # R G B
    ("Sun Corona",    15),  # R G B
]

VC_SCALAR_FIELDS = [
    ("SunCoreSize",         18, 0, 255),
    ("SunCoronaSize",       19, 0, 255),
    ("SunBrightness",       20, 0, 255),
    ("ShadowStrength",      21, 0, 255),
    ("LightShadStrength",   22, 0, 255),
    ("PoleShadStrength",    23, 0, 255),
    ("FarClip",             24, 0, 2500),
    ("FogStart",            25, 0, 2500),
    ("LightsOnGroundDist",  26, 0, 2500),
]

VC_COLOUR_GROUPS_2 = [
    ("Low Clouds",    27),  # R G B
    ("Bottom Cloud",  30),  # R G B
]


# ─────────────────────────────────────────────────────────────────────────────
# Data
# ─────────────────────────────────────────────────────────────────────────────

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
        return 'SA' if num_values >= 50 else 'VC'

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
            values = [int(p) for p in parts]
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

            # Parse: rows are ordered time0/weather0..7, time1/weather0..7, ...
            data_lines = [ln for ln in lines if ln.strip() and not ln.strip().startswith('/')]
            comment_lines = [ln for ln in lines if ln.strip().startswith('/')]
            self.header_lines = comment_lines[:3]  # keep first 3 comment lines

            row_idx = 0
            for ln in data_lines:
                weather = row_idx % 8
                time    = row_idx // 8
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


# ─────────────────────────────────────────────────────────────────────────────
# Sky preview widget
# ─────────────────────────────────────────────────────────────────────────────

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


# ─────────────────────────────────────────────────────────────────────────────
# Editor
# ─────────────────────────────────────────────────────────────────────────────

class TimecycEditor(GUIWorkshop): #vers 1
    App_name   = "Time Cycle Editor"
    App_build  = "Build 1"
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
        self._set_status("Open a timecyc.dat file to begin")

    def _build_left_panel(self, parent: QWidget) -> QWidget: #vers 1
        w = QWidget(parent)
        lay = QVBoxLayout(w)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(4)

        lay.addWidget(QLabel("Weather / Time Grid"))

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

    def _build_centre_panel(self, parent: QWidget) -> QWidget: #vers 1
        scroll = QScrollArea(parent)
        scroll.setWidgetResizable(True)
        container = QWidget()
        scroll.setWidget(container)
        lay = QVBoxLayout(container)
        lay.setContentsMargins(8, 8, 8, 8)
        lay.setSpacing(8)
        self._field_widgets.clear()

        # Colour groups
        for group_name, r_idx in VC_COLOUR_GROUPS + VC_COLOUR_GROUPS_2:
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

            # Colour swatch
            swatch = QLabel()
            swatch.setFixedSize(40, 40)
            swatch.setStyleSheet("background: rgb(0,0,0); border: 1px solid #555;")
            self._colour_swatches[group_name] = swatch
            grp_lay.addWidget(swatch)
            lay.addWidget(grp)

        # Scalar fields
        scalar_grp = QGroupBox("Atmosphere")
        scalar_form = QFormLayout(scalar_grp)
        for fname, idx, fmin, fmax in VC_SCALAR_FIELDS:
            sp = QSpinBox()
            sp.setRange(fmin, fmax)
            sp.valueChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))
            self._field_widgets[fname] = sp
            scalar_form.addRow(QLabel(fname), sp)
        lay.addWidget(scalar_grp)

        return scroll

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

    def setup_ui(self): #vers 2
        super().setup_ui()

    def _create_centre_panel(self): #vers 1
        sp = QSplitter(Qt.Orientation.Horizontal)
        sp.addWidget(self._build_left_panel(self))
        sp.addWidget(self._build_centre_panel(self))
        sp.addWidget(self._build_right_panel(self))
        sp.setSizes([400, 500, 200])
        return sp

    def _open_file(self, path=None): #vers 1
        if path is None:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open timecyc.dat", "",
                "DAT files (timecyc.dat *.dat);;All files (*)")
        if not path:
            return
        if not self._parser.load(path):
            QMessageBox.critical(self, "Error", f"Failed to load {path}")
            return
        self._current_path = path
        self._modified = False
        self._populate_grid()
        weathers = WEATHER_NAMES_SA if self._parser.game == 'SA' else WEATHER_NAMES_VC
        self._grid.setHorizontalHeaderLabels(weathers)
        self._set_status(f"Loaded {os.path.basename(path)} — {len(self._parser.rows)} rows [{self._parser.game}]")

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

    def _populate_grid(self): #vers 1
        for row in self._parser.rows:
            t, w = row.time, row.weather
            if t < 24 and w < 8:
                r, g, b = 0, 0, 0
                if len(row.values) >= 3:
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
        self._cell_info.setText(f"Time: {TIME_LABELS[row]}  Weather: {col}")
        self._populate_fields(r)

    def _populate_fields(self, row: TimecycRow): #vers 1
        self._blocking = True
        vals = row.values

        for group_name, r_idx in VC_COLOUR_GROUPS + VC_COLOUR_GROUPS_2:
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

        for fname, idx, *_ in VC_SCALAR_FIELDS:
            w = self._field_widgets.get(fname)
            if w and idx < len(vals):
                w.setValue(int(vals[idx]))

        self._blocking = False
        self._update_preview(row)

    def _on_field_changed(self, key: str, value: int): #vers 1
        if self._blocking or self._current_row is None:
            return

        # Update values in current row
        vals = self._current_row.values

        # Colour group field
        for group_name, r_idx in VC_COLOUR_GROUPS + VC_COLOUR_GROUPS_2:
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
        for fname, idx, *_ in VC_SCALAR_FIELDS:
            if key == fname and idx < len(vals):
                vals[idx] = value
                break

        self._modified = True
        self._update_preview(self._current_row)
        # Update grid cell colour
        t, w2 = self._current_row.time, self._current_row.weather
        if len(vals) >= 3:
            item = self._grid.item(t, w2) or QTableWidgetItem()
            item.setBackground(QColor(int(vals[0]), int(vals[1]), int(vals[2])))
            self._grid.setItem(t, w2, item)

    def _update_preview(self, row: TimecycRow): #vers 1
        vals = row.values
        def rgb(idx): return QColor(
            int(vals[idx]) if idx < len(vals) else 0,
            int(vals[idx+1]) if idx+1 < len(vals) else 0,
            int(vals[idx+2]) if idx+2 < len(vals) else 0)
        sky_top  = rgb(6)   # Sky Top
        sky_bot  = rgb(9)   # Sky Bottom
        ambient  = rgb(0)   # Ambient
        sun_core = rgb(12)  # Sun Core
        fog      = int(vals[25]) if 25 < len(vals) else 0
        self._sky_preview.set_colors(sky_top, sky_bot, ambient, sun_core, fog)

    def _build_menus_into_qmenu(self, pm): #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open timecyc.dat", self._open_file)
        fm.addAction("Save", self._save_file)
        fm.addSeparator()
        fm.addAction("Close", self.close)


def open_timecyc_editor(main_window=None, path: str = None): #vers 1
    app = QApplication.instance() or QApplication(sys.argv)
    w = TimecycEditor(main_window)
    w.resize(1200, 720)
    w.show()
    if path:
        w._open_file(path)
    return w


if __name__ == '__main__':
    app = QApplication(sys.argv)
    w = TimecycEditor()
    w.resize(1200, 720)
    w.show()
    sys.exit(app.exec())
