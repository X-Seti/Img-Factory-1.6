#!/usr/bin/env python3
#this belongs in apps/components/Handling_Editor/handling_editor.py - Version: 4
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

from apps.components.Handling_Editor.depends.diffcode import GUIWorkshop


#                                                                              
# Field definitions
#                                                                              

# (name, type, min, max, tooltip)
# GTA3/VC handling.cfg: 32 fields (A through AF in the file header)
# [0]Name [1]Mass [2]TurnMass [3]Drag [4-6]CentreOfMass [7]PercSub
# [8]TractionMult [9]TractionLoss [10]TractionBias [11]Gears [12]MaxVel
# [13]EngineAccel [14]DriveType [15]EngineType [16]BrakeDecel [17]BrakeBias
# [18]ABS [19]SteerLock [20]SuspForce [21]SuspDamping [22]SeatOffset
# [23]CollisionDmg [24]MoneyValue [25]SuspUpper [26]SuspLower [27]SuspBias
# [28]HandlingFlags(hex) [29]FrontLights [30]RearLights
# SA adds [31+]: extra fields, 36 total
VC_FIELDS = [
    ("HandlingName",              "str",   "",    "",     "Internal handling ID (matches vehicles.ide)"),
    ("Mass",                      "float", 1,     50000,  "Vehicle mass in kg"),
    ("TurnMass",                  "float", 1,     50000,  "Rotational inertia"),
    ("DragMult",                  "float", 0,     10,     "Aerodynamic drag multiplier"),
    ("CentreOfMassX",             "float", -10,   10,     "Centre of mass offset X"),
    ("CentreOfMassY",             "float", -10,   10,     "Centre of mass offset Y"),
    ("CentreOfMassZ",             "float", -10,   10,     "Centre of mass offset Z"),
    ("PercentSubmerged",          "int",   0,     120,    "% of vehicle height before sinking"),
    ("TractionMultiplier",        "float", 0,     5,      "Overall grip multiplier"),
    ("TractionLoss",              "float", 0,     1,      "Grip lost when sliding"),
    ("TractionBias",              "float", 0,     1,      "0=rear grip, 1=front grip"),
    ("NumberOfGears",             "int",   1,     6,      "Number of forward gears"),
    ("MaxVelocity",               "float", 0,     300,    "Top speed km/h"),
    ("EngineAcceleration",        "float", 0,     100,    "Engine force"),
    ("DriveType",                 "char",  "",    "",     "F=front R=rear 4=4WD"),
    ("EngineType",                "char",  "",    "",     "P=petrol D=diesel E=electric"),
    ("BrakeDeceleration",         "float", 0,     100,    "Braking force"),
    ("BrakeBias",                 "float", 0,     1,      "0=rear 1=front brakes"),
    ("ABS",                       "bool",  0,     1,      "Anti-lock braking"),
    ("SteeringLock",              "float", 0,     90,     "Max steering angle degrees"),
    ("SuspensionForceLevel",      "float", 0,     10,     "Spring stiffness"),
    ("SuspensionDampingLevel",    "float", 0,     10,     "Damper strength"),
    ("SeatOffsetDistance",        "float", 0,     5,      "Camera/seat distance"),
    ("CollisionDamageMultiplier", "float", 0,     10,     "Damage per collision"),
    ("MoneyValue",                "int",   0,     999999, "Vehicle dollar value"),
    ("SuspensionUpperLimit",      "float", -1,    1,      "Suspension upper travel"),
    ("SuspensionLowerLimit",      "float", -1,    1,      "Suspension lower travel"),
    ("SuspensionBias",            "float", 0,     1,      "Suspension front/rear bias"),
    ("HandlingFlags",             "hex",   "",    "",     "Behaviour flags (hex e.g. C00B)"),
    ("FrontLights",               "int",   0,     3,      "Front light type (0=long 1=small 2=big 3=tall)"),
    ("RearLights",                "int",   0,     3,      "Rear light type (0=long 1=small 2=big 3=tall)"),
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


#                                                                              
# Data classes
#                                                                              

# The file model (HandlingEntry / HandlingParser) lives in apps/methods/handling_file.py:
# byte-exact round trip, only edited vehicle lines change, CRLF/comments/SA
# special lines (% $ ! &) are kept.
from apps.methods.handling_file import HandlingEntry, HandlingParser, detect_game, header_fields
from apps.methods.ribbon_system import RibbonMixin


#                                                                              
# Editor widget
#                                                                              

class HandlingEditor(RibbonMixin, GUIWorkshop): #vers 2
    App_name   = "Handling Editor"
    App_build  = "Build 1"
    App_auth   = "X-Seti"
    config_key = "handling_editor"
    _ribbon_name = "handling_editor"
    # Bump when the set of ribbons changes (1 = File/Edit/View/Tools)
    _RIBBON_LAYOUT_VERSION = 1

    def __init__(self, main_window=None, parent=None):
        self._defer_setup_ui = True
        super().__init__(parent)
        self.main_window  = main_window
        self._parser      = HandlingParser()
        self._current_path: Optional[str] = None
        self._current_idx: int = -1
        self._modified    = False
        self._field_widgets: Dict[str, QWidget] = {}
        self._blocking    = False
        self._fields      = VC_FIELDS
        self._undo_stack, self._redo_stack = [], []
        self._ide_paths: List[str] = []
        self.setup_ui()
        self.ribbon_restore_state()
        # Hide inner toolbar chrome when docked inside IMG Factory
        if main_window and hasattr(self, 'toolbar'):
            self.toolbar.hide()
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
            b.setMinimumHeight(28)
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
        self._fill_form(VC_FIELDS)
        return scroll

    def _fields_for(self, game: str, n_values: int):
        """Columns from the loaded file's own legend (III, VC, SA and mods all
        label their columns in the header comments); numbered text fields fill
        in whatever the legend does not cover."""
        hf = header_fields(self._parser._lines) if getattr(self._parser, "_lines", None) else []
        if hf and len(hf) != n_values:
            hf = hf[:22]             # legend and data disagree (SA): trust only the shared leading columns
        base = hf[:n_values] if hf else (VC_FIELDS if game == 'VC' else VC_FIELDS[:20])
        extra = [(f"Field {i + 1}", "str", "", "", "Not labelled - edit as text")
                 for i in range(len(base), n_values)]
        return list(base) + extra

    def _fill_form(self, fields):
        self._fields = fields
        while self._form_layout.rowCount():
            self._form_layout.removeRow(0)
        self._field_widgets.clear()
        for fname, ftype, fmin, fmax, tip in fields:
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
            elif ftype == 'char' or 'DriveType' in fname or 'EngineType' in fname:
                w = QComboBox()
                if 'DriveType' in fname:
                    w.addItems(['F', 'R', '4'])
                elif 'EngineType' in fname:
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

    def setup_ui(self): #vers 3
        """Titlebar / [vehicle list | fields | stats] inside the ribbon host / status bar."""
        ml = QVBoxLayout(self)
        ml.setContentsMargins(*self.get_content_margins())
        ml.setSpacing(self.setspacing)
        ml.addWidget(self._create_toolbar())
        ml.addWidget(self.ribbon_wrap(self._create_centre_panel()), 1)
        self._build_ribbons()
        self._status_widget = self._create_status_bar()
        ml.addWidget(self._status_widget)
        self._status_widget.setVisible(self.WS.get("show_statusbar", True))

    def _create_toolbar(self): #vers 2
        """Titlebar keeps Settings / title / Undo / Info / Theme; the file
        buttons moved to the File ribbon."""
        tb = super()._create_toolbar()
        for name in ("open_btn", "save_btn", "export_btn", "import_btn"):
            btn = getattr(self, name, None)
            if btn:
                btn.setVisible(False)
        return tb

    def _build_ribbons(self): #vers 1
        B = self.ribbon_button
        tb = self.ribbon_toolbar("File")
        B(tb, "open_icon",   "Open handling.cfg  (Ctrl+O)", self._open_file)
        self.save_btn = B(tb, "save_icon", "Save  (Ctrl+S) - backs up the old file first", self._save_file, enabled=False)
        B(tb, "saveas_icon", "Save As...", self._save_as)

        tb = self.ribbon_toolbar("Edit")
        B(tb, "undo_icon", "Undo  (Ctrl+Z)", self._undo)
        B(tb, "redo_icon", "Redo  (Ctrl+Y)", self._redo)
        tb.addSeparator()
        B(tb, "add_icon",   "Add vehicle (copy of the first)", self._add_entry)
        B(tb, "trash_icon", "Delete selected vehicle", self._delete_entry)
        B(tb, "edit_icon",  "Duplicate selected vehicle", self._duplicate_entry)

        tb = self.ribbon_toolbar("Tools")
        B(tb, "check_icon",   "Check against vehicles.ide: unused handlings, missing handlings, duplicate names", self._check_vs_ide, text="Check")
        B(tb, "convert_icon", "Scale one column on every vehicle...", self._scale_column, text="Scale")
        B(tb, "folder_icon",  "Choose the vehicles.ide file(s) kept in step with renames...", self._choose_ides, text="IDEs")

    def closeEvent(self, ev): #vers 1
        if self._parser.dirty:
            r = QMessageBox.question(
                self, App_name if 'App_name' in globals() else "Handling Editor", "Save changes before closing?",
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
        if hasattr(self, "save_btn"):
            self.save_btn.setEnabled(self._modified)

    # -- undo (whole-list snapshots; a run of edits to one field is one step)
    def _snap(self):
        return [(e, list(e.values)) for e in self._parser.entries], list(self._parser.entries)

    def _push_undo(self, key=None):
        if key is not None and getattr(self, "_last_undo_key", None) == key:
            return
        self._last_undo_key = key
        self._undo_stack.append(self._snap())
        del self._undo_stack[:-60]
        self._redo_stack.clear()

    def _apply_snap(self, snap):
        vals, order = snap
        for e, v in vals:
            e.values = list(v)
        self._parser.entries = list(order)
        self._current_idx = -1
        self._refresh_list(self._search_box.text())
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

    def _create_centre_panel(self): #vers 1
        sp = QSplitter(Qt.Orientation.Horizontal)
        sp.addWidget(self._build_left_panel(self))
        sp.addWidget(self._build_centre_panel(self))
        sp.addWidget(self._build_right_panel(self))
        sp.setSizes([200, 600, 220])
        return sp

    def _open_file(self, path=None): #vers 2
        if path is None:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open handling.cfg", "",
                "Handling files (handling.cfg *.cfg);;All files (*)")
        if not path:
            return
        parser = HandlingParser()
        if not parser.load(path):
            QMessageBox.critical(self, "Error", f"Failed to load {path}")
            return
        self._parser = parser
        self._current_path = path
        self._current_idx = -1
        self._undo_stack.clear(); self._redo_stack.clear()
        self._last_undo_key = None
        n = max((len(e.values) for e in parser.entries), default=0)
        self._fill_form(self._fields_for(parser.game, n))
        self._refresh_list()
        self._update_modified()
        self._set_status(f"Loaded {os.path.basename(path)} - {len(parser.entries)} vehicles  [{parser.game}]")

    def _save_file(self): #vers 2
        """Back up the existing file, write atomically, then keep the chosen
        vehicles.ide files in step with any renamed handling."""
        if not self._current_path:
            self._save_as()
            return
        if not self._parser.dirty:
            self._set_status("Nothing to save")
            return
        renames = [(e.orig[0], e.values[0]) for e in self._parser.entries
                   if e.orig and e.orig[0] != e.values[0]]
        from apps.methods.file_backup import backup_file, note_change
        if os.path.exists(self._current_path):
            note_change(f"Save {os.path.basename(self._current_path)}")
            if backup_file(self._current_path) is None:
                QMessageBox.warning(self, "Save", "Backup failed - file not overwritten.")
                return
        if not self._parser.save(self._current_path):
            QMessageBox.critical(self, "Error", "Save failed")
            return
        msg = f"Saved {os.path.basename(self._current_path)}"
        if renames:
            msg += self._cascade_renames(renames)
        self._update_modified()
        self._set_status(msg)

    def _save_as(self): #vers 2
        path, _ = QFileDialog.getSaveFileName(
            self, "Save As", self._current_path or "", "Handling files (handling.cfg *.cfg)")
        if path:
            self._current_path = path
            self._save_file()

    # -- vehicles.ide link ("cars" rows: id, model, txd, type, HANDLING, ...)
    def _choose_ides(self): #vers 1
        paths, _ = QFileDialog.getOpenFileNames(
            self, "vehicles.ide file(s) that use these handlings", "", "IDE Files (*.ide *.IDE);;All Files (*)")
        if paths:
            self._ide_paths = list(paths)
            self._set_status(f"{len(paths)} IDE file(s) will follow handling renames")

    def _cascade_renames(self, renames) -> str: #vers 1
        """Rewrite the handling column of every 'cars' row that used a renamed
        handling (the IDE is backed up first). Returns text for the status bar."""
        if not self._ide_paths:
            r = QMessageBox.question(
                self, "vehicles.ide",
                f"{len(renames)} handling name(s) changed. Update the cars in vehicles.ide too? "
                "(you pick the IDE file(s))")
            if r == QMessageBox.StandardButton.Yes:
                self._choose_ides()
        if not self._ide_paths:
            return ""
        from apps.methods.ide_file import IDEFile
        from apps.methods.file_backup import backup_file, note_change
        table = {old.lower(): new for old, new in renames}
        touched = 0
        for p in self._ide_paths:
            f = IDEFile()
            try:
                f.load(p)
            except Exception:
                continue
            sec = f.section("cars")
            hit = 0
            for row in (sec.rows if sec else []):
                if len(row.fields) > 4 and row.fields[4].lower() in table:
                    row.fields[4] = table[row.fields[4].lower()]
                    hit += 1
            if hit:
                note_change(f"Handling rename in {os.path.basename(p)}")
                if backup_file(p) is not None:
                    f.save(p)
                    touched += 1
        return f"  |  {touched} IDE file(s) updated"

    def _handling_names(self):
        return {e.name.lower() for e in self._parser.entries}

    def _check_vs_ide(self): #vers 1
        """Report duplicate handling names, handlings no car uses, and cars
        whose handling does not exist (needs the vehicles.ide file(s))."""
        if not self._parser.entries:
            self._set_status("Open a handling.cfg first")
            return
        from collections import Counter
        from apps.methods.asset_integrity import show_integrity_dialog
        out = []
        dup = [f"   {n} x{c}" for n, c in Counter(e.name.upper() for e in self._parser.entries).items() if c > 1]
        out += [f"== Duplicate handling names: {len(dup)}"] + dup + [""]
        if not self._ide_paths:
            self._choose_ides()
        if self._ide_paths:
            from apps.methods.ide_file import IDEFile
            used, missing = {}, []
            names = self._handling_names()
            for p in self._ide_paths:
                f = IDEFile()
                try:
                    f.load(p)
                except Exception:
                    continue
                sec = f.section("cars")
                for row in (sec.rows if sec else []):
                    if len(row.fields) > 4:
                        h = row.fields[4].lower()
                        used.setdefault(h, []).append(row.fields[1])
                        if h not in names:
                            missing.append(f"   {row.fields[1]} (id {row.fields[0]}) uses '{row.fields[4]}' - not in handling.cfg")
            unused = [f"   {e.name}" for e in self._parser.entries if e.name.lower() not in used]
            out += [f"== Cars using a handling that does not exist: {len(missing)}"] + missing + [""]
            out += [f"== Handlings no car uses: {len(unused)}"] + unused
        else:
            out.append("(vehicles.ide check skipped - no IDE file chosen)")
        show_integrity_dialog(self, "\n".join(out))

    def _scale_column(self): #vers 1
        """Multiply one numeric column on every vehicle (e.g. Mass x 1.1)."""
        if not self._parser.entries:
            return
        from PyQt6.QtWidgets import QInputDialog
        names = [f[0] for f in self._fields[1:]]
        name, ok = QInputDialog.getItem(self, "Scale column", "Column:", names, 0, False)
        if not ok:
            return
        k = next(i for i, f in enumerate(self._fields) if f[0] == name)
        fac, ok = QInputDialog.getDouble(self, "Scale column", f"Multiply {name} by:", 1.0, 0.0001, 10000.0, 4)
        if not ok or fac == 1.0:
            return
        self._push_undo()
        n = 0
        for e in self._parser.entries:
            if k < len(e.values):
                try:
                    v = float(e.values[k]) * fac
                except ValueError:
                    continue
                e.values[k] = (f"{v:.4f}".rstrip('0').rstrip('.') or "0")
                n += 1
        self._refresh_list(self._search_box.text())
        self._update_modified()
        if self._current_idx >= 0:
            self._populate_fields(self._parser.entries[self._current_idx])
        self._set_status(f"Scaled {name} on {n} vehicle(s) by {fac:g}")

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
        for i, (fname, ftype, *_) in enumerate(self._fields):
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

    def _on_field_changed(self, field_name: str, value): #vers 2
        if self._blocking or self._current_idx < 0:
            return
        entry = self._parser.entries[self._current_idx]
        for i, (fname, *_) in enumerate(self._fields):
            if fname == field_name and i < len(entry.values):
                new = str(value)
                if entry.values[i] == new:
                    return
                self._push_undo((id(entry), fname))
                entry.values[i] = new
                break
        self._update_modified()
        self._update_stat_bars(entry)

    def _update_stat_bars(self, entry: HandlingEntry): #vers 1
        vals = entry.values
        field_map = {f[0]: i for i, f in enumerate(self._fields)}
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

    def _add_entry(self): #vers 2
        self._push_undo()
        template = self._parser.entries[0].values[:] if self._parser.entries else ['NEWVEHICLE'] + ['0.0'] * 36
        template[0] = 'NEWVEHICLE'
        self._parser.entries.append(HandlingEntry(template))
        self._refresh_list(self._search_box.text())
        self._veh_list.setCurrentRow(self._veh_list.count() - 1)
        self._update_modified()

    def _delete_entry(self): #vers 2
        if self._current_idx < 0 or not self._parser.entries:
            return
        name = self._parser.entries[self._current_idx].name
        if QMessageBox.question(self, "Delete", f"Delete {name}?") != QMessageBox.StandardButton.Yes:
            return
        self._push_undo()
        self._parser.entries.pop(self._current_idx)
        self._current_idx = -1
        self._refresh_list(self._search_box.text())
        self._update_modified()

    def _duplicate_entry(self): #vers 2
        if self._current_idx < 0 or not self._parser.entries:
            return
        self._push_undo()
        src = self._parser.entries[self._current_idx]
        e = src.copy_as_new()
        e.values[0] = src.values[0] + '_COPY'
        self._parser.entries.insert(self._current_idx + 1, e)
        self._refresh_list(self._search_box.text())
        self._update_modified()

    def _build_menus_into_qmenu(self, pm): #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open handling.cfg", self._open_file)
        fm.addAction("Save", self._save_file)
        fm.addAction("Save As…", self._save_as)
        fm.addSeparator()
        fm.addAction("Close", self.close)


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
