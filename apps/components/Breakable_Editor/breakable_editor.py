#!/usr/bin/env python3
#this belongs in apps/components/Breakable_Editor/breakable_editor.py - Version: 4
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
project_root = Path(current_dir).parents[2]
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

from apps.components.Breakable_Editor.depends.diffcode import GUIWorkshop
from apps.methods.ribbon_system import RibbonMixin


#                                                                              
# Field definitions per section
#                                                                              

# GTA3/LC/VC object.dat: 11 fields (no section headers, tab+comma separated)
# Name  Mass  TurnMass  AirRes  Elasticity  PercSub  UprootLim  ColDmg  FxType  FxOffset(x,y,z)... wait
# Actual: Name  Mass  TurnMass  AirRes  Elasticity  PercSub  UprootLim  ColDmg  FxType  SmashAudio  CamAvoid
# But file shows 11 fields: name + 10 numeric
# From GTAMods: Name, Mass, TurnMass, AirResistance, Elasticity, PercSubmerged, UprootLimit, ColDamageEffect, FxType, SmashAudio, CamAvoidAngle
VC_OBJECT_FIELDS = [
    ("ModelName",           "str",   "",   "",    "Model name from IDE"),
    ("Mass",                "float", 0,    99999, "Object mass in kg"),
    ("TurnMass",            "float", 0,    99999, "Rotational inertia"),
    ("AirResistance",       "float", 0,    10,    "Air resistance (0.99=low)"),
    ("Elasticity",          "float", 0,    10,    "Bounce elasticity"),
    ("PercSubmerged",       "float", 0,    100,   "% submerged before sinking"),
    ("UprootLimit",         "float", 0,    99999, "Force to uproot"),
    ("ColDamageEffect",     "int",   0,    255,   "Collision damage type"),
    ("FxType",              "int",   0,    255,   "Particle effect type"),
    ("SmashAudio",          "str",   "",   "",    "Sound on smash (0=none)"),
    ("CamAvoidAngle",       "float", 0,    360,   "Camera avoidance angle"),
]

# SA object.dat: 17 fields — adds FxOffset(x,y,z), BreakVelocity, BreakIntensity, BreakMode, SmashAudioName
SA_EXTRA_FIELDS = [
    ("FxOffsetX",           "float", -10,  10,    "FX origin X offset"),
    ("FxOffsetY",           "float", -10,  10,    "FX origin Y offset"),
    ("FxOffsetZ",           "float", -10,  10,    "FX origin Z offset"),
    ("BreakVelocity",       "float", 0,    100,   "Velocity needed to break"),
    ("BreakIntensity",      "float", 0,    100,   "Break force intensity"),
    ("BreakMode",           "int",   0,    3,     "Break mode flag"),
    ("SmashAudioName",      "str",   "",   "",    "SA: audio asset name"),
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


#                                                                              
# Data classes
#                                                                              

@dataclass
class ObjectEntry: #vers 2
    section:  str  = "OBJECT"
    values:   list = field(default_factory=list)
    comment:  str  = ""
    raw_index: int = -1            # line index in the loaded file (-1 = new)
    orig:     tuple = ()           # values as loaded

    @property
    def changed(self) -> bool:
        return self.raw_index < 0 or tuple(self.values) != self.orig

    def copy_as_new(self) -> "ObjectEntry":
        return ObjectEntry(self.section, list(self.values), self.comment)

    @staticmethod
    def from_line(line: str, section: str) -> Optional['ObjectEntry']: #vers 3
        """Parse one object.dat line. Handles tab, comma or space separation."""
        s = line.strip()
        if not s or s.startswith('#') or s.startswith(';') or s.startswith('*'):
            return None
        comment = ""
        for mark in ('#', ';'):
            if mark in s:
                i = s.index(mark)
                comment, s = s[i:], s[:i].strip()
        import re as _re
        parts = [p for p in _re.split(r'[,\s]+', s) if p]
        if len(parts) < 2:
            return None
        return ObjectEntry(section=section, values=parts, comment=comment)

    def new_line(self) -> str:
        return '\t'.join(str(v) for v in self.values) + (f'\t{self.comment}' if self.comment else '')

    @property
    def name(self) -> str:
        return self.values[0] if self.values else ''


def legend_fields(lines) -> list:
    """Column labels from the file's own '; (A) Object Name ...' legend (A, B, C ...
    in order; the lettered legend stops at the first non-sequential code)."""
    import re as _re
    out, want = [], 0
    for ln in lines:
        m = _re.match(r'^;\s*\(([A-Z]{1,2})\)\s*(.*)$', ln.strip())
        if not m:
            continue
        code = m.group(1)
        exp = chr(65 + want) if want < 26 else None
        if code != exp:
            if out:
                break
            continue
        label = _re.split(r'\s{2,}|\t|\[|\s\(|\s-', m.group(2).strip())[0].strip(' -:') or code
        out.append((label.replace(' ', '')[:28], code, m.group(2).strip()))
        want += 1
    return out


class BreakableParser: #vers 2
    """object.dat. Keeps the original lines (comments, CRLF, the '* ;end of file'
    terminator, VC's headerless layout); only edited rows are rewritten."""
    def __init__(self): #vers 1
        self.entries:      List[ObjectEntry] = []
        self.header_lines: List[str]         = []
        self.game:         str               = 'VC'
        self._lines:       List[str]         = []
        self._eol:         str               = "\n"
        self._count0:      int               = 0
        self._sec_at:      Dict[int, str]    = {}

    @property
    def dirty(self) -> bool:
        return len(self.entries) != self._count0 or any(e.changed for e in self.entries)

    def _detect_game(self, entries: List[ObjectEntry]) -> str: #vers 2
        """LC/VC=11 fields, SA=17 fields."""
        for e in entries:
            if len(e.values) >= 17: return 'SA'
        return 'VC'

    def load(self, path: str) -> bool: #vers 3
        try:
            text = Path(path).read_bytes().decode("latin1")
            self._parse(text)
            print(f"[BreakableParser] {len(self.entries)} entries loaded ({self.game})")
            return True
        except Exception as ex:
            print(f"BreakableParser.load: {ex}")
            return False

    def _parse(self, text: str):
        self._eol = "\r\n" if "\r\n" in text else "\n"
        self._lines = text.split(self._eol)
        self.entries, self.header_lines = [], []
        current = "OBJECT"                       # VC/III have no section headers
        for i, ln in enumerate(self._lines):
            s = ln.strip()
            if not s:
                continue
            word = s.upper().split()[0]
            if word in SECTIONS and len(s.split()) == 1:
                current = word
                continue
            if word == 'END':
                continue
            e = ObjectEntry.from_line(ln, current)
            if e:
                e.raw_index, e.orig = i, tuple(e.values)
                self.entries.append(e)
        self.game = self._detect_game(self.entries)
        self._count0 = len(self.entries)
        self.header_lines = self._lines[:min((e.raw_index for e in self.entries), default=len(self._lines))]

    def to_text(self) -> str:
        import re as _re
        alive = {e.raw_index: e for e in self.entries if e.raw_index >= 0}
        new = [e for e in self.entries if e.raw_index < 0]
        out, last, cur, last_of = [], -1, "OBJECT", {}
        for i, ln in enumerate(self._lines):
            s = ln.strip()
            word = s.upper().split()[0] if s.split() else ""
            if word in SECTIONS and len(s.split()) == 1:
                cur = word
            e = alive.get(i)
            if e is None and ObjectEntry.from_line(ln, cur) is not None:
                continue                                      # a deleted object
            if e is not None:
                if e.changed:
                    body, mk, cm = ln, "", ""
                    for mark in ('#', ';'):
                        if mark in ln:
                            k = ln.index(mark)
                            body, cm = ln[:k], ln[k:]
                            break
                    toks = _re.split(r'([,\s]+)', body)
                    slots = [k for k, t in enumerate(toks) if t and not _re.fullmatch(r'[,\s]+', t)]
                    for k, (old, nv) in enumerate(zip(e.orig, e.values)):
                        if old != nv and k < len(slots):
                            toks[slots[k]] = str(nv)
                    ln = ''.join(toks) + cm
                out.append(ln)
                last = len(out) - 1
                last_of[cur] = last
            else:
                out.append(ln)
        # new objects: after the last entry of their section, else after the last entry, else at the end
        for e in new:
            at = last_of.get(e.section, last) + 1 if (last_of or last >= 0) else len(out)
            out.insert(at, e.new_line())
            for k in list(last_of):
                if last_of[k] >= at:
                    last_of[k] += 1
            last_of[e.section] = at
            last = max(last, at)
        return self._eol.join(out)

    def save(self, path: str) -> bool: #vers 2
        """Atomic write (temp file in the same folder, then swap in), then
        re-baseline against what was written."""
        import tempfile
        try:
            text = self.to_text()
            d = os.path.dirname(os.path.abspath(path))
            fd, tmp = tempfile.mkstemp(dir=d, prefix=".obj_", suffix=".tmp")
            try:
                with os.fdopen(fd, "wb") as f:
                    f.write(text.encode("latin1", errors="replace"))
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
            keep = sorted((e for e in self.entries if e.raw_index >= 0), key=lambda e: e.raw_index) \
                + [e for e in self.entries if e.raw_index < 0]
            fresh = BreakableParser()
            fresh._parse(text)
            for old, new in zip(keep, fresh.entries):
                old.raw_index, old.orig = new.raw_index, new.orig
            self.entries = keep
            self._lines, self._count0, self.header_lines = fresh._lines, fresh._count0, fresh.header_lines
            return True
        except Exception as ex:
            print(f"BreakableParser.save: {ex}")
            return False


#                                                                              
# Editor
#                                                                              

class BreakableEditor(RibbonMixin, GUIWorkshop): #vers 2
    App_name   = "Breakable Objects Editor"
    App_build  = "Build 1"
    App_auth   = "X-Seti"
    config_key = "breakable_editor"
    _ribbon_name = "breakable_editor"
    # Bump when the set of ribbons changes (1 = File/Edit/Tools)
    _RIBBON_LAYOUT_VERSION = 1

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
        self._fields        = VC_OBJECT_FIELDS
        self._undo_stack, self._redo_stack = [], []
        self._last_undo_key = None
        self.setup_ui()
        self.ribbon_restore_state()
        #  double-call bug found and fixed (Aug 20 2026) - setup_ui()
        # was called twice in a row here, needlessly rebuilding every
        # widget a second time. The old self.toolbar.hide() that used
        # to follow it is also gone - that line unconditionally hid
        # the WHOLE toolbar frame whenever docked, silently overriding
        # this same file's own gui_workshop.py copy's real, already-
        # correct per-widget visibility logic (Menu/Settings/title/
        # Undo/Info/Theme hidden when docked, Open/Save/Export/Import
        # left genuinely visible either way - see that file's own
        # _create_toolbar for the real reasoning) - meaning that
        # earlier, more careful fix never actually took effect at all
        # while this line still existed. Per  : "Any needed
        # buttons on the title bar when docked can follow the same
        # pattern as the other tools" - removing this line is what
        # lets that already-correct pattern actually apply here now.
        self._set_status("Open an object.dat file to begin")

    def _build_left_panel(self, parent: QWidget) -> QWidget: #vers 2
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
        self._fill_form(VC_OBJECT_FIELDS)
        return scroll

    def _fill_form(self, fields): #vers 1
        self._fields = fields
        while self._form_layout.rowCount():
            self._form_layout.removeRow(0)
        self._field_widgets.clear()
        for fname, ftype, fmin, fmax, tip in fields:
            lbl = QLabel(fname)
            lbl.setToolTip(tip)
            lbl.setFixedWidth(160)
            if ftype == 'float':
                w = QDoubleSpinBox()
                w.setRange(float(fmin), float(fmax))
                w.setDecimals(4)
                w.setSingleStep(0.1)
                w.valueChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))
            elif ftype == 'int':
                w = QSpinBox()
                w.setRange(int(fmin), int(fmax))
                w.valueChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))
            else:
                w = QLineEdit()
                w.textChanged.connect(lambda v, n=fname: self._on_field_changed(n, v))
            w.setToolTip(tip)
            self._field_widgets[fname] = w
            self._form_layout.addRow(lbl, w)

    def _fields_for_file(self):
        """Labels from the file's own legend, types from the data itself; columns the
        legend does not name are numbered."""
        entries = self._parser.entries
        n = max((len(e.values) for e in entries), default=0)
        leg = legend_fields(self._parser._lines)
        out = []
        for i in range(n):
            label, tip = (leg[i][0], leg[i][2]) if i < len(leg) else (f"Field {i + 1}", "Not labelled - edit as text")
            col = [e.values[i] for e in entries if i < len(e.values)]
            def _num(v, cast):
                try:
                    cast(v)
                    return True
                except ValueError:
                    return False
            if i == 0:
                kind, lo, hi = 'str', "", ""
            elif all(_num(v, int) for v in col):
                kind, lo, hi = 'int', -2147483648, 2147483647
            elif all(_num(v, float) for v in col):
                kind, lo, hi = 'float', -1e9, 1e9
            else:
                kind, lo, hi = 'str', "", ""
            out.append((label, kind, lo, hi, tip))
        return out or VC_OBJECT_FIELDS

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

    def setup_ui(self): #vers 3
        """Titlebar / [objects | fields | reference] inside the ribbon host / status bar."""
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
        tb = super()._create_toolbar()
        for name in ("open_btn", "save_btn", "export_btn", "import_btn"):
            btn = getattr(self, name, None)
            if btn:
                btn.setVisible(False)
        return tb

    def _build_ribbons(self): #vers 1
        B = self.ribbon_button
        tb = self.ribbon_toolbar("File")
        B(tb, "open_icon",   "Open object.dat  (Ctrl+O)", self._open_file)
        self.save_btn = B(tb, "save_icon", "Save  (Ctrl+S) - backs up the old file first", self._save_file, enabled=False)
        B(tb, "saveas_icon", "Save As...", self._save_as)
        tb = self.ribbon_toolbar("Edit")
        B(tb, "undo_icon", "Undo  (Ctrl+Z)", self._undo)
        B(tb, "redo_icon", "Redo  (Ctrl+Y)", self._redo)
        tb.addSeparator()
        B(tb, "add_icon",   "Add object", self._add_entry)
        B(tb, "trash_icon", "Delete selected object", self._delete_entry)
        B(tb, "edit_icon",  "Duplicate selected object", self._duplicate_entry)
        tb = self.ribbon_toolbar("Tools")
        B(tb, "check_icon", "Check for duplicate object names", self._check_duplicates, text="Check")

    def closeEvent(self, ev): #vers 1
        if self._parser.dirty:
            r = QMessageBox.question(
                self, "Breakable Objects Editor", "Save changes before closing?",
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

    def _snap(self):
        return [(e, list(e.values)) for e in self._parser.entries], list(self._parser.entries)

    def _push_undo(self, key=None):
        if key is not None and self._last_undo_key == key:
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
        self._refresh_list(self._search_box.text(), self._section_combo.currentText())
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

    def _check_duplicates(self): #vers 1
        from collections import Counter
        from apps.methods.asset_integrity import show_integrity_dialog
        c = Counter(e.name.lower() for e in self._parser.entries)
        dup = [f"   {n} x{k}" for n, k in c.items() if k > 1]
        show_integrity_dialog(self, f"Duplicate object names: {len(dup)}\n\n" + "\n".join(dup))

    def _create_centre_panel(self): #vers 1
        sp = QSplitter(Qt.Orientation.Horizontal)
        sp.addWidget(self._build_left_panel(self))
        sp.addWidget(self._build_centre_panel(self))
        sp.addWidget(self._build_right_panel(self))
        sp.setSizes([200, 560, 200])
        return sp

    def _open_file(self, path=None): #vers 2
        if path is None or path is False or path is True:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open object.dat", "",
                "DAT files (object.dat *.dat);;All files (*)")
        if not path:
            return
        parser = BreakableParser()
        if not parser.load(path):
            QMessageBox.critical(self, "Error", f"Failed to load {path}")
            return
        self._parser = parser
        self._current_path = path
        self._current_idx = -1
        self._undo_stack.clear(); self._redo_stack.clear(); self._last_undo_key = None
        self._fill_form(self._fields_for_file())
        self._refresh_list()
        self._update_modified()
        self._set_status(f"Loaded {os.path.basename(path)} - {len(self._parser.entries)} objects [{self._parser.game}]")

    def _save_file(self): #vers 2
        """Back up the existing file, then write it atomically."""
        if not self._current_path:
            self._save_as()
            return
        if not self._parser.dirty:
            self._set_status("Nothing to save")
            return
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
            QMessageBox.critical(self, "Error", "Save failed")

    def _save_as(self): #vers 1
        p, _ = QFileDialog.getSaveFileName(self, "Save object.dat as", self._current_path or "object.dat", "DAT files (*.dat)")
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
        for i, (fname, ftype, *_) in enumerate(self._fields):
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

    def _add_entry(self): #vers 2
        self._push_undo()
        sec = self._section_combo.currentText()
        if sec == "All sections":
            sec = "OBJECT"
        if self._parser.entries:
            e = self._parser.entries[0].copy_as_new()
            e.section = sec
            e.values[0] = 'NEWOBJECT'
        else:
            e = ObjectEntry(section=sec, values=['NEWOBJECT', '20.0', '10.0', '0.3', '0.3', '100', '0', '0', '0', '0', '0'])
        self._parser.entries.append(e)
        self._refresh_list(self._search_box.text(), self._section_combo.currentText())
        self._update_modified()

    def _delete_entry(self): #vers 2
        if self._current_idx < 0:
            return
        name = self._parser.entries[self._current_idx].name
        if QMessageBox.question(self, "Delete", f"Delete {name}?") != QMessageBox.StandardButton.Yes:
            return
        self._push_undo()
        self._parser.entries.pop(self._current_idx)
        self._current_idx = -1
        self._refresh_list(self._search_box.text(), self._section_combo.currentText())
        self._update_modified()

    def _duplicate_entry(self): #vers 1
        if self._current_idx < 0:
            return
        self._push_undo()
        src = self._parser.entries[self._current_idx]
        e = src.copy_as_new()
        e.values[0] = src.values[0] + "_copy"
        self._parser.entries.insert(self._current_idx + 1, e)
        self._refresh_list(self._search_box.text(), self._section_combo.currentText())
        self._update_modified()

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
    w.resize(1000, 680); w.show()
    # No longer forces an Open dialog immediately on startup when run
    # standalone without a path argument (Aug 20 2026,  "the
    # open dialog can go, let the user decide to open the objects
    # dat") - opens genuinely empty instead, same real principle as
    # the docked launch path's own fix in imgfactory.py's own open_
    # breakable_editor - the tool's own real Open button is right
    # there whenever the person is ready to use it.
    if len(sys.argv) > 1:
        w._open_file(sys.argv[1])
    sys.exit(app.exec())
