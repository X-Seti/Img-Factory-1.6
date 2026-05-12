#!/usr/bin/env python3
#this belongs in apps/components/Handling_Editor/handling_editor.py - Version: 1
# X-Seti - May08 2026 - Img Factory 1.6 - Vehicle Handling Editor

"""
Vehicle Handling Editor — reads/writes GTA III/VC/SA handling.cfg.
Subclasses GUIWorkshop. Left panel = vehicle list, centre = field editor,
right = live stat bars (top speed, mass, braking, traction).
"""

##Methods list -
# HandlingEntry.__init__
# HandlingEntry.from_line
# HandlingEntry.to_line
# HandlingParser.__init__
# HandlingParser.load
# HandlingParser.save
# HandlingParser._detect_game
# HandlingEditor.__init__
# HandlingEditor._build_left_panel
# HandlingEditor._build_centre_panel
# HandlingEditor._build_right_panel
# HandlingEditor._open_file
# HandlingEditor._save_file
# HandlingEditor._on_vehicle_selected
# HandlingEditor._populate_fields
# HandlingEditor._on_field_changed
# HandlingEditor._update_stat_bars
# HandlingEditor._add_entry
# HandlingEditor._delete_entry
# HandlingEditor._duplicate_entry
# HandlingEditor._search_vehicles
# HandlingEditor._build_menus_into_qmenu
# open_handling_editor

import sys, os, re
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = Path(current_dir).parents[2]
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QLabel, QLineEdit,
    QListWidget, QListWidgetItem, QScrollArea, QFrame, QGroupBox,
    QDoubleSpinBox, QSpinBox, QComboBox, QCheckBox, QPushButton,
    QProgressBar, QFileDialog, QMessageBox, QApplication, QFormLayout,
    QTabWidget, QMenu, QSizePolicy
)
from PyQt6.QtCore import Qt, pyqtSignal
from PyQt6.QtGui import QColor, QFont

try:
    from apps.components.Tmp_Template.gui_workshop import GUIWorkshop
except ImportError:
    from apps.methods.gui_workshop import GUIWorkshop


# ─────────────────────────────────────────────────────────────────────────────
# Field definitions
# ─────────────────────────────────────────────────────────────────────────────

# (name, type, min, max, tooltip)
VC_FIELDS = [
    ("HandlingName",              "str",   "",    "",     "Internal handling ID (matches vehicles.ide)"),
    ("Mass",                      "float", 1,     50000,  "Vehicle mass in kg"),
    ("TurnMass",                  "float", 1,     50000,  "Rotational inertia"),
    ("DragMult",                  "float", 0,     10,     "Aerodynamic drag multiplier"),
    ("CentreOfMassX",             "float", -5,    5,      "Centre of mass offset X"),
    ("CentreOfMassY",             "float", -5,    5,      "Centre of mass offset Y"),
    ("CentreOfMassZ",             "float", -5,    5,      "Centre of mass offset Z"),
    ("PercentSubmerged",          "int",   0,     100,    "% of vehicle height before sinking"),
    ("TractionMultiplier",        "float", 0,     10,     "Overall grip multiplier"),
    ("TractionLoss",              "float", 0,     1,      "Grip lost when sliding (0=no loss)"),
    ("TractionBias",              "float", 0,     1,      "0=rear grip, 1=front grip"),
    ("NumberOfGears",             "int",   1,     6,      "Number of forward gears"),
    ("MaxVelocity",               "float", 0,     300,    "Top speed in m/s (~3.6x for km/h)"),
    ("EngineAcceleration",        "float", 0,     100,    "Engine force applied per gear"),
    ("EngineInertia",             "float", 0,     100,    "Throttle response lag"),
    ("DriveType",                 "char",  "",    "",     "F=front, R=rear, 4=4WD"),
    ("EngineType",                "char",  "",    "",     "P=petrol, D=diesel, E=electric"),
    ("BrakeDeceleration",         "float", 0,     100,    "Braking force"),
    ("BrakeBias",                 "float", 0,     1,      "0=rear brakes, 1=front brakes"),
    ("ABS",                       "bool",  0,     1,      "Anti-lock braking system"),
    ("SteeringLock",              "float", 0,     90,     "Max steering angle in degrees"),
    ("SuspensionForceLevel",      "float", 0,     10,     "Spring stiffness"),
    ("SuspensionDampingLevel",    "float", 0,     10,     "Damper strength"),
    ("SeatOffsetDistance",        "float", 0,     5,      "Camera seat distance"),
    ("CollisionDamageMultiplier", "float", 0,     10,     "Damage taken per collision"),
    ("MoneyValue",                "int",   0,     999999, "Base dollar value of vehicle"),
    ("SuspensionUpperLimit",      "float", -1,    1,      "Suspension travel upper bound"),
    ("SuspensionLowerLimit",      "float", -1,    1,      "Suspension travel lower bound"),
    ("SuspensionBias",            "float", 0,     1,      "0=rear suspension, 1=front"),
    ("SuspensionAntidive",        "float", 0,     1,      "Anti-dive factor under braking"),
    ("HHandlingFlags",            "hex",   "",    "",     "Handling flags (hex)"),
    ("ModelFlags",                "hex",   "",    "",     "Model flags (hex)"),
    ("HandlingFlags",             "hex",   "",    "",     "Behaviour flags (hex)"),
    ("FrontTyre",                 "int",   0,     255,    "Front wheel model ID"),
    ("FrontTyreScale",            "float", 0,     5,      "Front wheel scale"),
    ("RearTyre",                  "int",   0,     255,    "Rear wheel model ID"),
    ("RearTyreScale",             "float", 0,     5,      "Rear wheel scale"),
]

HANDLING_FLAGS = {
    0x00000001: "1G_BOOST",
    0x00000002: "2G_BOOST",
    0x00000004: "NPC_ANTI_ROLL",
    0x00000008: "NPC_NEUTRAL_HANDL",
    0x00000010: "NO_HANDBRAKE",
    0x00000020: "STEER_REARWHEELS",
    0x00000040: "HB_REARWHEEL_STEER",
    0x00000080: "ALT_STEER_OPT",
    0x00000100: "WHEEL_F_NARROW2",
    0x00000200: "WHEEL_F_NARROW",
    0x00000400: "WHEEL_F_WIDE",
    0x00000800: "WHEEL_F_WIDE2",
    0x00001000: "WHEEL_R_NARROW2",
    0x00002000: "WHEEL_R_NARROW",
    0x00004000: "WHEEL_R_WIDE",
    0x00008000: "WHEEL_R_WIDE2",
    0x00010000: "HYDRAULIC_GEOM",
    0x00020000: "HYDRAULIC_INST",
    0x00040000: "HYDRAULIC_NONE",
    0x00080000: "NOS_INST",
    0x00100000: "OFFROAD_ABILITY",
    0x00200000: "OFFROAD_ABILITY2",
    0x00400000: "HALOGEN_LIGHTS",
    0x00800000: "PROC_REARWHEEL_1ST",
    0x01000000: "USE_MAXSP_LIMIT",
    0x02000000: "LOW_RIDER",
    0x04000000: "STREET_RACER",
    0x10000000: "SWINGING_CHASSIS",
}


# ─────────────────────────────────────────────────────────────────────────────
# Data classes
# ─────────────────────────────────────────────────────────────────────────────

@dataclass
class HandlingEntry: #vers 1
    values: list = field(default_factory=list)
    comment: str = ""
    raw_line: str = ""

    @staticmethod
    def from_line(line: str) -> Optional['HandlingEntry']: #vers 1
        stripped = line.strip()
        if not stripped or stripped.startswith(';'):
            return None
        comment = ""
        if ';' in stripped:
            idx = stripped.index(';')
            comment = stripped[idx:]
            stripped = stripped[:idx].strip()
        parts = stripped.split()
        if len(parts) < 10:
            return None
        e = HandlingEntry()
        e.values = parts
        e.comment = comment
        e.raw_line = line
        return e

    def to_line(self) -> str: #vers 1
        return '\t'.join(str(v) for v in self.values) + (f'  {self.comment}' if self.comment else '') + '\n'

    @property
    def name(self) -> str: #vers 1
        return self.values[0] if self.values else ''


class HandlingParser: #vers 1
    def __init__(self): #vers 1
        self.entries: List[HandlingEntry] = []
        self.header_lines: List[str] = []
        self.game = 'VC'

    def _detect_game(self, lines: List[str]) -> str: #vers 1
        for ln in lines:
            parts = ln.strip().split()
            if len(parts) > 37:
                return 'SA'
        return 'VC'

    def load(self, path: str) -> bool: #vers 1
        try:
            with open(path, 'r', encoding='latin-1') as f:
                lines = f.readlines()
            self.game = self._detect_game([l for l in lines if not l.strip().startswith(';')])
            self.entries.clear()
            self.header_lines.clear()
            in_data = False
            for ln in lines:
                s = ln.strip()
                if not s or s.startswith(';'):
                    if not in_data:
                        self.header_lines.append(ln)
                    continue
                in_data = True
                e = HandlingEntry.from_line(ln)
                if e:
                    self.entries.append(e)
            return True
        except Exception as ex:
            print(f"HandlingParser.load error: {ex}")
            return False

    def save(self, path: str) -> bool: #vers 1
        try:
            with open(path, 'w', encoding='latin-1') as f:
                for ln in self.header_lines:
                    f.write(ln)
                for e in self.entries:
                    f.write(e.to_line())
            return True
        except Exception as ex:
            print(f"HandlingParser.save error: {ex}")
            return False


# ─────────────────────────────────────────────────────────────────────────────
# Editor widget
# ─────────────────────────────────────────────────────────────────────────────

class HandlingEditor(GUIWorkshop): #vers 1
    App_name   = "Handling Editor"
    App_build  = "Build 1"
    App_auth   = "X-Seti"
    config_key = "handling_editor"

    def __init__(self, main_window=None, parent=None):
        super().__init__(parent)
        self.main_window  = main_window
        self._parser      = HandlingParser()
        self._current_path: Optional[str] = None
        self._current_idx: int = -1
        self._modified    = False
        self._field_widgets: Dict[str, QWidget] = {}
        self._blocking    = False
        self.setup_ui()
        self._set_status("Open a handling.cfg file to begin")

    def _build_left_panel(self, parent: QWidget) -> QWidget: #vers 1
        w = QWidget(parent)
        lay = QVBoxLayout(w)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(4)

        lbl = QLabel("Vehicles")
        lbl.setFont(QFont("Arial", 9, QFont.Weight.Bold))
        lay.addWidget(lbl)

        self._search_box = QLineEdit()
        self._search_box.setPlaceholderText("Search…")
        self._search_box.textChanged.connect(self._search_vehicles)
        lay.addWidget(self._search_box)

        self._veh_list = QListWidget()
        self._veh_list.currentRowChanged.connect(self._on_vehicle_selected)
        lay.addWidget(self._veh_list)

        btn_row = QHBoxLayout()
        for label, slot in [("Add", self._add_entry), ("Del", self._delete_entry), ("Dup", self._duplicate_entry)]:
            b = QPushButton(label)
            b.setFixedHeight(24)
            b.clicked.connect(slot)
            btn_row.addWidget(b)
        lay.addLayout(btn_row)
        return w

    def _build_centre_panel(self, parent: QWidget) -> QWidget: #vers 1
        scroll = QScrollArea(parent)
        scroll.setWidgetResizable(True)
        container = QWidget()
        scroll.setWidget(container)
        self._form_layout = QFormLayout(container)
        self._form_layout.setSpacing(4)
        self._form_layout.setContentsMargins(8, 8, 8, 8)
        self._field_widgets.clear()

        for fname, ftype, fmin, fmax, tip in VC_FIELDS:
            lbl = QLabel(fname)
            lbl.setToolTip(tip)
            lbl.setFixedWidth(200)

            if ftype == 'float':
                w = QDoubleSpinBox()
                w.setRange(float(fmin), float(fmax))
                w.setDecimals(4)
                w.setSingleStep(0.01)
                w.setToolTip(tip)
                w.valueChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))
            elif ftype == 'int':
                w = QSpinBox()
                w.setRange(int(fmin), int(fmax))
                w.setToolTip(tip)
                w.valueChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))
            elif ftype == 'bool':
                w = QCheckBox()
                w.setToolTip(tip)
                w.stateChanged.connect(lambda v, n=fname: self._on_field_changed(n, int(v > 0)))
            elif ftype == 'char':
                w = QComboBox()
                if fname == 'DriveType':
                    w.addItems(['F', 'R', '4'])
                elif fname == 'EngineType':
                    w.addItems(['P', 'D', 'E'])
                w.setToolTip(tip)
                w.currentTextChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))
            elif ftype == 'hex':
                w = QLineEdit()
                w.setPlaceholderText("0x00000000")
                w.setToolTip(tip)
                w.textChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))
            else:  # str
                w = QLineEdit()
                w.setMaxLength(14)
                w.setToolTip(tip)
                w.textChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))

            self._field_widgets[fname] = w
            self._form_layout.addRow(lbl, w)

        return scroll

    def _build_right_panel(self, parent: QWidget) -> QWidget: #vers 1
        w = QWidget(parent)
        lay = QVBoxLayout(w)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(6)

        lay.addWidget(QLabel("Vehicle Stats"))

        self._stat_bars: Dict[str, QProgressBar] = {}
        stats = [
            ("Top Speed",  "MaxVelocity",          200),
            ("Mass",       "Mass",                  5000),
            ("Braking",    "BrakeDeceleration",     30),
            ("Traction",   "TractionMultiplier",    3),
            ("Engine",     "EngineAcceleration",    20),
            ("Suspension", "SuspensionForceLevel",  5),
        ]
        for label, field_name, max_val in stats:
            row = QHBoxLayout()
            l = QLabel(label)
            l.setFixedWidth(80)
            bar = QProgressBar()
            bar.setRange(0, 100)
            bar.setValue(0)
            bar.setTextVisible(True)
            bar.setFixedHeight(18)
            self._stat_bars[field_name] = (bar, max_val)
            row.addWidget(l)
            row.addWidget(bar)
            lay.addLayout(row)

        lay.addStretch()

        # Flags display
        grp = QGroupBox("Handling Flags")
        flag_lay = QVBoxLayout(grp)
        self._flag_labels: Dict[int, QLabel] = {}
        for bit, name in list(HANDLING_FLAGS.items())[:16]:
            fl = QLabel(name)
            fl.setStyleSheet("color: #888;")
            fl.setFont(QFont("Monospace", 8))
            self._flag_labels[bit] = fl
            flag_lay.addWidget(fl)
        lay.addWidget(grp)
        return w

    def setup_ui(self): #vers 1
        super().setup_ui()
        sp = QSplitter(Qt.Orientation.Horizontal)
        sp.addWidget(self._build_left_panel(self))
        sp.addWidget(self._build_centre_panel(self))
        sp.addWidget(self._build_right_panel(self))
        sp.setSizes([200, 600, 220])
        self.centre_layout.addWidget(sp)

    def _open_file(self, path=None): #vers 1
        if path is None:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open handling.cfg", "",
                "Handling files (handling.cfg *.cfg);;All files (*)")
        if not path:
            return
        if not self._parser.load(path):
            QMessageBox.critical(self, "Error", f"Failed to load {path}")
            return
        self._current_path = path
        self._modified = False
        self._refresh_list()
        self._set_status(f"Loaded {os.path.basename(path)} — {len(self._parser.entries)} vehicles  [{self._parser.game}]")

    def _save_file(self): #vers 1
        if not self._current_path:
            self._current_path, _ = QFileDialog.getSaveFileName(
                self, "Save handling.cfg", "", "Handling files (handling.cfg *.cfg)")
        if not self._current_path:
            return
        if self._parser.save(self._current_path):
            self._modified = False
            self._set_status(f"Saved {os.path.basename(self._current_path)}")
        else:
            QMessageBox.critical(self, "Error", "Save failed")

    def _refresh_list(self, filter_text: str = ""): #vers 1
        self._veh_list.clear()
        ft = filter_text.lower()
        for i, e in enumerate(self._parser.entries):
            if ft and ft not in e.name.lower():
                continue
            item = QListWidgetItem(e.name)
            item.setData(Qt.ItemDataRole.UserRole, i)
            self._veh_list.addItem(item)

    def _search_vehicles(self, text: str): #vers 1
        self._refresh_list(text)

    def _on_vehicle_selected(self, row: int): #vers 1
        item = self._veh_list.item(row)
        if item is None:
            return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None or idx >= len(self._parser.entries):
            return
        self._current_idx = idx
        self._populate_fields(self._parser.entries[idx])

    def _populate_fields(self, entry: HandlingEntry): #vers 1
        self._blocking = True
        vals = entry.values
        for i, (fname, ftype, *_) in enumerate(VC_FIELDS):
            if i >= len(vals):
                break
            w = self._field_widgets.get(fname)
            if w is None:
                continue
            v = vals[i]
            try:
                if ftype == 'float':
                    w.setValue(float(v))
                elif ftype == 'int':
                    w.setValue(int(v))
                elif ftype == 'bool':
                    w.setChecked(int(v) != 0)
                elif ftype in ('char', 'str') and hasattr(w, 'setCurrentText'):
                    w.setCurrentText(str(v))
                elif hasattr(w, 'setText'):
                    w.setText(str(v))
            except Exception:
                pass
        self._blocking = False
        self._update_stat_bars(entry)

    def _on_field_changed(self, field_name: str, value): #vers 1
        if self._blocking or self._current_idx < 0:
            return
        entry = self._parser.entries[self._current_idx]
        for i, (fname, *_) in enumerate(VC_FIELDS):
            if fname == field_name and i < len(entry.values):
                entry.values[i] = str(value)
                break
        self._modified = True
        self._update_stat_bars(entry)

    def _update_stat_bars(self, entry: HandlingEntry): #vers 1
        vals = entry.values
        field_map = {f[0]: i for i, f in enumerate(VC_FIELDS)}
        for field_name, (bar, max_val) in self._stat_bars.items():
            idx = field_map.get(field_name)
            if idx is not None and idx < len(vals):
                try:
                    v = float(vals[idx])
                    pct = min(100, int(v / max_val * 100))
                    bar.setValue(pct)
                    bar.setFormat(f"{v:.1f}")
                except Exception:
                    bar.setValue(0)
        # Update flag highlights
        hf_idx = field_map.get('HandlingFlags')
        if hf_idx and hf_idx < len(vals):
            try:
                flags = int(vals[hf_idx], 16)
                for bit, lbl in self._flag_labels.items():
                    if flags & bit:
                        lbl.setStyleSheet("color: #50e090; font-weight: bold;")
                    else:
                        lbl.setStyleSheet("color: #888;")
            except Exception:
                pass

    def _add_entry(self): #vers 1
        template = self._parser.entries[0].values[:] if self._parser.entries else ['NEWVEHICLE'] + ['0.0'] * 36
        template[0] = 'NEWVEHICLE'
        e = HandlingEntry()
        e.values = template
        self._parser.entries.append(e)
        self._refresh_list(self._search_box.text())
        self._veh_list.setCurrentRow(self._veh_list.count() - 1)
        self._modified = True

    def _delete_entry(self): #vers 1
        if self._current_idx < 0 or not self._parser.entries:
            return
        name = self._parser.entries[self._current_idx].name
        r = QMessageBox.question(self, "Delete", f"Delete {name}?")
        if r != QMessageBox.StandardButton.Yes:
            return
        self._parser.entries.pop(self._current_idx)
        self._current_idx = -1
        self._refresh_list(self._search_box.text())
        self._modified = True

    def _duplicate_entry(self): #vers 1
        if self._current_idx < 0 or not self._parser.entries:
            return
        src = self._parser.entries[self._current_idx]
        e = HandlingEntry()
        e.values = src.values[:]
        e.values[0] = src.values[0] + '_COPY'
        self._parser.entries.insert(self._current_idx + 1, e)
        self._refresh_list(self._search_box.text())
        self._modified = True

    def _build_menus_into_qmenu(self, pm): #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open handling.cfg", self._open_file)
        fm.addAction("Save", self._save_file)
        fm.addAction("Save As…", lambda: self._save_as())
        fm.addSeparator()
        fm.addAction("Close", self.close)

    def _save_as(self): #vers 1
        path, _ = QFileDialog.getSaveFileName(
            self, "Save As", "", "Handling files (handling.cfg *.cfg)")
        if path:
            self._current_path = path
            self._save_file()


def open_handling_editor(main_window=None, path: str = None): #vers 1
    app = QApplication.instance() or QApplication(sys.argv)
    w = HandlingEditor(main_window)
    w.resize(1100, 700)
    w.show()
    if path:
        w._open_file(path)
    return w


if __name__ == '__main__':
    app = QApplication(sys.argv)
    w = HandlingEditor()
    w.resize(1100, 700)
    w.show()
    sys.exit(app.exec())
