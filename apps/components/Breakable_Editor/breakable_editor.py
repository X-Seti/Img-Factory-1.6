#!/usr/bin/env python3
#this belongs in apps/components/Breakable_Editor/breakable_editor.py - Version: 1
# X-Seti - May08 2026 - Img Factory 1.6 - Breakable Objects Editor

"""
Breakable Objects Editor — reads/writes GTA VC/SA object.dat.
Sections: OBJECT/TOBJ/ANIM/TXDP/OBJS — each has physics properties.
Left panel = object list, centre = field form, right = effect preview info.
"""

##Methods list -
# ObjectEntry.__init__
# ObjectEntry.from_line
# ObjectEntry.to_line
# BreakableParser.__init__
# BreakableParser.load
# BreakableParser.save
# BreakableParser._detect_game
# BreakableEditor.__init__
# BreakableEditor._build_left_panel
# BreakableEditor._build_centre_panel
# BreakableEditor._build_right_panel
# BreakableEditor._open_file
# BreakableEditor._save_file
# BreakableEditor._on_object_selected
# BreakableEditor._populate_fields
# BreakableEditor._on_field_changed
# BreakableEditor._refresh_list
# BreakableEditor._search_objects
# BreakableEditor._filter_by_section
# BreakableEditor._add_entry
# BreakableEditor._delete_entry
# BreakableEditor._build_menus_into_qmenu
# open_breakable_editor

import sys, os
from pathlib import Path
from typing import List, Optional, Dict
from dataclasses import dataclass, field

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = Path(current_dir).parents[3]
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QLabel, QLineEdit,
    QListWidget, QListWidgetItem, QScrollArea, QGroupBox,
    QDoubleSpinBox, QSpinBox, QComboBox, QPushButton,
    QFileDialog, QMessageBox, QApplication, QFormLayout, QFrame,
    QTextEdit, QCheckBox, QMenu
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont, QColor

from gui_workshop import GUIWorkshop  # local standalone copy


# ─────────────────────────────────────────────────────────────────────────────
# Field definitions per section
# ─────────────────────────────────────────────────────────────────────────────

# VC object.dat fields (name, type, min, max, tooltip)
VC_OBJECT_FIELDS = [
    ("ModelName",           "str",   "",  "",    "Model name from IDE"),
    ("Mass",                "float", 0,   9999,  "Object mass in kg"),
    ("TurnMass",            "float", 0,   9999,  "Rotational inertia"),
    ("AirResistance",       "float", 0,   10,    "Air resistance coefficient"),
    ("ElasticityLimit",     "float", 0,   10,    "Bounce elasticity"),
    ("PercSubmerged",       "int",   0,   100,   "Submerged % before sinking"),
    ("UprootLimit",         "float", 0,   100,   "Force needed to uproot"),
    ("ColDamageEffect",     "int",   0,   255,   "Collision damage type"),
    ("FxType",              "int",   0,   255,   "Particle effect type"),
    ("FxOffsetX",           "float", -10, 10,    "FX origin X offset"),
    ("FxOffsetY",           "float", -10, 10,    "FX origin Y offset"),
    ("FxOffsetZ",           "float", -10, 10,    "FX origin Z offset"),
    ("SmashAudio",          "str",   "",  "",    "Sound effect name on smash"),
    ("CamAvoidAngle",       "float", 0,   360,   "Camera avoidance angle"),
]

# SA adds break velocity + intensity
SA_EXTRA_FIELDS = [
    ("BreakVelocity",       "float", 0,   100,   "Velocity needed to break"),
    ("BreakIntensity",      "float", 0,   100,   "Break force intensity"),
    ("FunBreakMode",        "int",   0,   3,     "SA: break mode flag"),
]

COL_DAMAGE_EFFECTS = {
    0:  "None",
    1:  "Smash",
    2:  "Jolt",
    3:  "Flatten",
    4:  "Explode",
    5:  "Bounce",
}

FX_TYPES = {
    0:  "None",
    1:  "Smoke",
    2:  "Sparks",
    3:  "Debris",
    4:  "Explosion",
    5:  "Wood chips",
    6:  "Glass shards",
    7:  "Leaves",
}

SECTIONS = ["OBJECT", "TOBJ", "ANIM", "OBJS", "TXDP"]


# ─────────────────────────────────────────────────────────────────────────────
# Data classes
# ─────────────────────────────────────────────────────────────────────────────

@dataclass
class ObjectEntry: #vers 1
    section:  str  = "OBJECT"
    values:   list = field(default_factory=list)
    comment:  str  = ""

    @staticmethod
    def from_line(line: str, section: str) -> Optional['ObjectEntry']: #vers 1
        s = line.strip()
        if not s or s.startswith('#'):
            return None
        comment = ""
        if '#' in s:
            i = s.index('#')
            comment = s[i:]
            s = s[:i].strip()
        parts = s.split()
        if len(parts) < 2:
            return None
        e = ObjectEntry(section=section, values=parts, comment=comment)
        return e

    def to_line(self) -> str: #vers 1
        return '    ' + '    '.join(str(v) for v in self.values) + \
               (f'  {self.comment}' if self.comment else '') + '\n'

    @property
    def name(self) -> str:
        return self.values[0] if self.values else ''


class BreakableParser: #vers 1
    def __init__(self): #vers 1
        self.entries:      List[ObjectEntry] = []
        self.header_lines: List[str]         = []
        self.game:         str               = 'VC'

    def _detect_game(self, entries: List[ObjectEntry]) -> str: #vers 1
        for e in entries:
            if len(e.values) > len(VC_OBJECT_FIELDS):
                return 'SA'
        return 'VC'

    def load(self, path: str) -> bool: #vers 1
        try:
            self.entries.clear()
            self.header_lines.clear()
            current_section = "OBJECT"
            with open(path, 'r', encoding='latin-1') as f:
                for ln in f:
                    s = ln.strip()
                    if not s:
                        self.header_lines.append(ln)
                        continue
                    upper = s.upper()
                    if upper in SECTIONS:
                        current_section = upper
                        self.header_lines.append(ln)
                        continue
                    if upper == 'END':
                        self.header_lines.append(ln)
                        continue
                    if s.startswith('#') and not self.entries:
                        self.header_lines.append(ln)
                        continue
                    e = ObjectEntry.from_line(ln, current_section)
                    if e:
                        self.entries.append(e)
            self.game = self._detect_game(self.entries)
            return True
        except Exception as ex:
            print(f"BreakableParser.load: {ex}")
            return False

    def save(self, path: str) -> bool: #vers 1
        try:
            # Group entries by section
            from collections import defaultdict
            by_section: Dict[str, List[ObjectEntry]] = defaultdict(list)
            for e in self.entries:
                by_section[e.section].append(e)
            with open(path, 'w', encoding='latin-1') as f:
                for ln in self.header_lines:
                    f.write(ln)
                for sec in SECTIONS:
                    if sec in by_section:
                        f.write(f'{sec}\n')
                        for e in by_section[sec]:
                            f.write(e.to_line())
                        f.write('END\n\n')
            return True
        except Exception as ex:
            print(f"BreakableParser.save: {ex}")
            return False


# ─────────────────────────────────────────────────────────────────────────────
# Editor
# ─────────────────────────────────────────────────────────────────────────────

class BreakableEditor(GUIWorkshop): #vers 1
    App_name   = "Breakable Objects Editor"
    App_build  = "Build 1"
    App_auth   = "X-Seti"
    config_key = "breakable_editor"

    def __init__(self, main_window=None, parent=None):
        self._defer_setup_ui = True
        super().__init__(parent)
        self.main_window   = main_window
        self._parser       = BreakableParser()
        self._current_path: Optional[str]  = None
        self._current_idx:  int            = -1
        self._modified      = False
        self._field_widgets: Dict[str, QWidget] = {}
        self._blocking      = False
        self._section_filter = ""
        self.setup_ui()
        self.setup_ui()
        self._set_status("Open an object.dat file to begin")

    def _build_left_panel(self, parent: QWidget) -> QWidget: #vers 1
        w = QWidget(parent)
        lay = QVBoxLayout(w)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(4)

        lay.addWidget(QLabel("Objects"))

        self._search_box = QLineEdit()
        self._search_box.setPlaceholderText("Search model name…")
        self._search_box.textChanged.connect(self._search_objects)
        lay.addWidget(self._search_box)

        # Section filter
        self._section_combo = QComboBox()
        self._section_combo.addItem("All sections")
        for s in SECTIONS:
            self._section_combo.addItem(s)
        self._section_combo.currentTextChanged.connect(self._filter_by_section)
        lay.addWidget(self._section_combo)

        self._obj_list = QListWidget()
        self._obj_list.currentRowChanged.connect(self._on_object_selected)
        lay.addWidget(self._obj_list)

        btn_row = QHBoxLayout()
        for label, slot in [("Add", self._add_entry), ("Del", self._delete_entry)]:
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

        all_fields = VC_OBJECT_FIELDS + SA_EXTRA_FIELDS
        for fname, ftype, fmin, fmax, tip in all_fields:
            lbl = QLabel(fname)
            lbl.setToolTip(tip)
            lbl.setFixedWidth(160)

            if ftype == 'float':
                w = QDoubleSpinBox()
                w.setRange(float(fmin), float(fmax))
                w.setDecimals(4)
                w.setSingleStep(0.1)
                w.setToolTip(tip)
                w.valueChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))
            elif ftype == 'int':
                w = QSpinBox()
                w.setRange(int(fmin), int(fmax))
                w.setToolTip(tip)
                w.valueChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))
            else:
                w = QLineEdit()
                w.setToolTip(tip)
                w.textChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))

            self._field_widgets[fname] = w
            self._form_layout.addRow(lbl, w)

        return scroll

    def _build_right_panel(self, parent: QWidget) -> QWidget: #vers 1
        w = QWidget(parent)
        lay = QVBoxLayout(w)
        lay.setContentsMargins(4, 4, 4, 4)

        lay.addWidget(QLabel("Effect Reference"))

        grp_dmg = QGroupBox("ColDamageEffect")
        dmg_lay = QVBoxLayout(grp_dmg)
        for k, v in COL_DAMAGE_EFFECTS.items():
            dmg_lay.addWidget(QLabel(f"  {k} = {v}"))
        lay.addWidget(grp_dmg)

        grp_fx = QGroupBox("FxType")
        fx_lay = QVBoxLayout(grp_fx)
        for k, v in FX_TYPES.items():
            fx_lay.addWidget(QLabel(f"  {k} = {v}"))
        lay.addWidget(grp_fx)

        lay.addStretch()

        self._info_box = QTextEdit()
        self._info_box.setReadOnly(True)
        self._info_box.setMaximumHeight(100)
        self._info_box.setPlaceholderText("Select an object for info")
        lay.addWidget(self._info_box)
        return w

    def setup_ui(self): #vers 1
        super().setup_ui()
        sp = QSplitter(Qt.Orientation.Horizontal)
        sp.addWidget(self._build_left_panel(self))
        sp.addWidget(self._build_centre_panel(self))
        sp.addWidget(self._build_right_panel(self))
        sp.setSizes([200, 560, 200])
        self.centre_layout.addWidget(sp)

    def _open_file(self, path=None): #vers 1
        if path is None:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open object.dat", "",
                "DAT files (object.dat *.dat);;All files (*)")
        if not path:
            return
        if not self._parser.load(path):
            QMessageBox.critical(self, "Error", f"Failed to load {path}")
            return
        self._current_path = path
        self._modified = False
        self._refresh_list()
        self._set_status(f"Loaded {os.path.basename(path)} — {len(self._parser.entries)} objects [{self._parser.game}]")

    def _save_file(self): #vers 1
        if not self._current_path:
            self._current_path, _ = QFileDialog.getSaveFileName(
                self, "Save object.dat", "", "DAT files (*.dat)")
        if not self._current_path:
            return
        if self._parser.save(self._current_path):
            self._modified = False
            self._set_status(f"Saved {os.path.basename(self._current_path)}")
        else:
            QMessageBox.critical(self, "Error", "Save failed")

    def _refresh_list(self, filter_text: str = "", section: str = ""): #vers 1
        self._obj_list.clear()
        ft = filter_text.lower()
        for i, e in enumerate(self._parser.entries):
            if ft and ft not in e.name.lower():
                continue
            if section and section != "All sections" and e.section != section:
                continue
            item = QListWidgetItem(f"[{e.section}] {e.name}")
            item.setData(Qt.ItemDataRole.UserRole, i)
            self._obj_list.addItem(item)

    def _search_objects(self, text: str): #vers 1
        self._refresh_list(text, self._section_combo.currentText())

    def _filter_by_section(self, section: str): #vers 1
        self._refresh_list(self._search_box.text(), section)

    def _on_object_selected(self, row: int): #vers 1
        item = self._obj_list.item(row)
        if item is None:
            return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None or idx >= len(self._parser.entries):
            return
        self._current_idx = idx
        entry = self._parser.entries[idx]
        self._populate_fields(entry)
        self._info_box.setPlainText(
            f"Section: {entry.section}\nModel: {entry.name}\nFields: {len(entry.values)}")

    def _populate_fields(self, entry: ObjectEntry): #vers 1
        self._blocking = True
        all_fields = VC_OBJECT_FIELDS + SA_EXTRA_FIELDS
        for i, (fname, ftype, *_) in enumerate(all_fields):
            if i >= len(entry.values):
                break
            w = self._field_widgets.get(fname)
            if w is None:
                continue
            v = entry.values[i]
            try:
                if ftype == 'float':
                    w.setValue(float(v))
                elif ftype == 'int':
                    w.setValue(int(v))
                elif hasattr(w, 'setText'):
                    w.setText(str(v))
            except Exception:
                pass
        self._blocking = False

    def _on_field_changed(self, field_name: str, value): #vers 1
        if self._blocking or self._current_idx < 0:
            return
        entry = self._parser.entries[self._current_idx]
        all_fields = VC_OBJECT_FIELDS + SA_EXTRA_FIELDS
        for i, (fname, *_) in enumerate(all_fields):
            if fname == field_name and i < len(entry.values):
                entry.values[i] = str(value)
                break
        self._modified = True

    def _add_entry(self): #vers 1
        sec = self._section_combo.currentText()
        if sec == "All sections":
            sec = "OBJECT"
        e = ObjectEntry(section=sec, values=['NEWOBJECT', '20.0', '10.0', '0.3', '0.3', '100', '0', '0', '0', '0', '0', '0', 'NULL', '0'])
        self._parser.entries.append(e)
        self._refresh_list(self._search_box.text(), self._section_combo.currentText())
        self._modified = True

    def _delete_entry(self): #vers 1
        if self._current_idx < 0:
            return
        name = self._parser.entries[self._current_idx].name
        r = QMessageBox.question(self, "Delete", f"Delete {name}?")
        if r != QMessageBox.StandardButton.Yes:
            return
        self._parser.entries.pop(self._current_idx)
        self._current_idx = -1
        self._refresh_list(self._search_box.text(), self._section_combo.currentText())
        self._modified = True

    def _build_menus_into_qmenu(self, pm): #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open object.dat", self._open_file)
        fm.addAction("Save", self._save_file)
        fm.addSeparator()
        fm.addAction("Close", self.close)


def open_breakable_editor(main_window=None, path: str = None): #vers 1
    app = QApplication.instance() or QApplication(sys.argv)
    w = BreakableEditor(main_window)
    w.resize(1000, 680)
    w.show()
    if path:
        w._open_file(path)
    return w


if __name__ == '__main__':
    app = QApplication(sys.argv)
    w = BreakableEditor()
    w.resize(1000, 680)
    w.show()
    sys.exit(app.exec())
