#!/usr/bin/env python3
#this belongs in apps/components/Vehicle_Workshop/vehicle_workshop.py - Version: 2
# X-Seti - May08 2026 - Img Factory 1.6 - Vehicle Workshop

"""
Vehicle Workshop — tabbed editor for GTA III/VC/SA vehicle data files.
Tabs: Handling (handling.cfg) | Car Colours (carcols.dat) | Car Mods (carmods.dat SA)

Parsers are self-contained. Handling parser imported from Handling_Editor.
"""

##Methods list -
# CarColour.__init__
# CarColsParser.__init__
# CarColsParser.load
# CarColsParser.save
# CarColsParser._detect_game
# CarModEntry.__init__
# CarModsParser.__init__
# CarModsParser.load
# CarModsParser.save
# ColourSwatchGrid.__init__
# ColourSwatchGrid.set_palette
# ColourSwatchGrid.paintEvent
# ColourSwatchGrid.mousePressEvent
# HandlingTab.__init__
# HandlingTab._build_ui
# HandlingTab.load_file
# HandlingTab.save_file
# CarColoursTab.__init__
# CarColoursTab._build_ui
# CarColoursTab.load_file
# CarColoursTab.save_file
# CarColoursTab._on_vehicle_selected
# CarColoursTab._populate_vehicle_colours
# CarColoursTab._on_colour_index_clicked
# CarColoursTab._add_vehicle
# CarColoursTab._delete_vehicle
# CarColoursTab._edit_selected_colour
# CarColoursTab._add_pair
# CarColoursTab._remove_pair
# CarModsTab.__init__
# CarModsTab._build_ui
# CarModsTab.load_file
# CarModsTab.save_file
# CarModsTab._on_vehicle_selected
# CarModsTab._populate_mods
# CarModsTab._add_vehicle
# CarModsTab._delete_vehicle
# CarModsTab._add_mod
# CarModsTab._remove_mod
# VehicleWorkshop.__init__
# VehicleWorkshop._build_menus_into_qmenu
# VehicleWorkshop._open_file
# VehicleWorkshop._save_file
# VehicleWorkshop._save_as
# VehicleWorkshop._open_specific
# VehicleWorkshop.setup_ui
# open_vehicle_workshop

import sys, os
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = Path(current_dir).parents[2]
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QLabel, QLineEdit,
    QListWidget, QListWidgetItem, QScrollArea, QGroupBox, QTabWidget,
    QDoubleSpinBox, QSpinBox, QComboBox, QCheckBox, QPushButton,
    QFileDialog, QMessageBox, QApplication, QFormLayout, QFrame,
    QTableWidget, QTableWidgetItem, QHeaderView, QAbstractItemView,
    QColorDialog, QProgressBar, QMenu, QSizePolicy
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor, QFont, QPainter, QBrush, QPen

try:
    from apps.components.Tmp_Template.gui_workshop import GUIWorkshop
except ImportError:
    from apps.methods.gui_workshop import GUIWorkshop

try:
    from apps.methods.gl_viewport_mixin import GLViewportMixin
except ImportError:
    class GLViewportMixin: pass

try:
    from apps.components.Handling_Editor.handling_editor import (
        HandlingParser, HandlingEntry, VC_FIELDS, HANDLING_FLAGS
    )
    _HANDLING_AVAILABLE = True
except ImportError:
    _HANDLING_AVAILABLE = False


# ─────────────────────────────────────────────────────────────────────────────
# carcols.dat parser
# ─────────────────────────────────────────────────────────────────────────────

@dataclass
class CarColour: #vers 1
    r: int = 0
    g: int = 0
    b: int = 0

    def to_qcolor(self) -> QColor:
        return QColor(self.r, self.g, self.b)

    def __str__(self):
        return f"{self.r},{self.g},{self.b}"


@dataclass
class CarColEntry: #vers 1
    name:     str                    = ""
    palettes: List[Tuple[int, int]]  = field(default_factory=list)


class CarColsParser: #vers 1
    def __init__(self): #vers 1
        self.colours:      List[CarColour]   = []
        self.vehicles:     List[CarColEntry] = []
        self.header_lines: List[str]         = []
        self.game:         str               = "VC"

    def _detect_game(self, lines: List[str]) -> str: #vers 1
        for ln in lines:
            s = ln.strip()
            if s.lower().startswith("col") and not s.startswith(";"):
                if len(s.split()) > 15:
                    return "SA"
        return "VC"

    def load(self, path: str) -> bool: #vers 1
        try:
            self.colours.clear(); self.vehicles.clear(); self.header_lines.clear()
            with open(path, "r", encoding="latin-1") as f:
                lines = f.readlines()
            self.game = self._detect_game(lines)
            in_col = False
            for ln in lines:
                s = ln.strip()
                if not s or s.startswith(";"):
                    if not in_col: self.header_lines.append(ln)
                    continue
                parts = s.split()
                if not parts: continue
                # Colour palette row: "R,G,B"
                if not parts[0].lower().startswith("col") and not in_col:
                    try:
                        rgb = parts[0].split(",")
                        if len(rgb) == 3:
                            self.colours.append(CarColour(int(rgb[0]), int(rgb[1]), int(rgb[2])))
                            continue
                        elif len(parts) >= 3 and all(p.isdigit() for p in parts[:3]):
                            self.colours.append(CarColour(int(parts[0]), int(parts[1]), int(parts[2])))
                            continue
                    except (ValueError, IndexError):
                        pass
                if parts[0].lower() in ("col", "car2"):
                    in_col = True
                    if len(parts) < 2: continue
                    entry = CarColEntry(name=parts[1])
                    for pair_str in parts[2:]:
                        try:
                            pair = pair_str.split(",")
                            if len(pair) == 2:
                                entry.palettes.append((int(pair[0]), int(pair[1])))
                        except ValueError:
                            pass
                    self.vehicles.append(entry)
            return True
        except Exception as ex:
            print(f"CarColsParser.load: {ex}"); return False

    def save(self, path: str) -> bool: #vers 1
        try:
            with open(path, "w", encoding="latin-1") as f:
                for ln in self.header_lines: f.write(ln)
                for c in self.colours: f.write(f"{c.r},{c.g},{c.b}\n")
                f.write("\n")
                for v in self.vehicles:
                    pairs = "   ".join(f"{p},{s}" for p, s in v.palettes)
                    f.write(f"col   {v.name}   {pairs}\n")
            return True
        except Exception as ex:
            print(f"CarColsParser.save: {ex}"); return False


# ─────────────────────────────────────────────────────────────────────────────
# carmods.dat parser
# ─────────────────────────────────────────────────────────────────────────────

@dataclass
class CarModEntry: #vers 1
    vehicle: str       = ""
    mods:    List[str] = field(default_factory=list)


class CarModsParser: #vers 1
    def __init__(self): #vers 1
        self.entries:      List[CarModEntry] = []
        self.header_lines: List[str]         = []

    def load(self, path: str) -> bool: #vers 1
        try:
            self.entries.clear(); self.header_lines.clear()
            in_data = False
            with open(path, "r", encoding="latin-1") as f:
                for ln in f:
                    s = ln.strip()
                    if not s or s.startswith(";") or s.startswith("#"):
                        if not in_data: self.header_lines.append(ln)
                        continue
                    in_data = True
                    parts = s.split()
                    if parts:
                        self.entries.append(CarModEntry(vehicle=parts[0], mods=parts[1:]))
            return True
        except Exception as ex:
            print(f"CarModsParser.load: {ex}"); return False

    def save(self, path: str) -> bool: #vers 1
        try:
            with open(path, "w", encoding="latin-1") as f:
                for ln in self.header_lines: f.write(ln)
                for e in self.entries:
                    f.write(f"{e.vehicle}   {chr(32).join(e.mods)}\n")
            return True
        except Exception as ex:
            print(f"CarModsParser.save: {ex}"); return False


# ─────────────────────────────────────────────────────────────────────────────
# Colour swatch grid
# ─────────────────────────────────────────────────────────────────────────────

class ColourSwatchGrid(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self._palette:  List[CarColour] = []
        self._selected: int = -1
        self._cols      = 16
        self._cell      = 24
        self.setMinimumHeight(80)
        self.setCursor(Qt.CursorShape.PointingHandCursor)

    def set_palette(self, palette: List[CarColour]): #vers 1
        self._palette = palette
        rows = max(1, (len(palette) + self._cols - 1) // self._cols)
        self.setMinimumHeight(rows * self._cell + 4)
        self.update()

    def paintEvent(self, event): #vers 1
        p = QPainter(self)
        cs = self._cell
        for i, col in enumerate(self._palette):
            x = (i % self._cols) * cs
            y = (i // self._cols) * cs
            p.fillRect(x, y, cs, cs, col.to_qcolor())
            if i == self._selected:
                p.setPen(QPen(QColor(255, 255, 255), 2))
                p.drawRect(x+1, y+1, cs-2, cs-2)
            else:
                p.setPen(QPen(QColor(0, 0, 0, 60), 1))
                p.drawRect(x, y, cs, cs)
        if not self._palette:
            p.setPen(QColor(120, 120, 120))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "No palette loaded")

    def mousePressEvent(self, event): #vers 1
        cs = self._cell
        idx = (int(event.position().y()) // cs) * self._cols + (int(event.position().x()) // cs)
        if 0 <= idx < len(self._palette):
            self._selected = idx
            self.update()
            self.colour_clicked(idx)

    def colour_clicked(self, idx: int): pass  # override


# ─────────────────────────────────────────────────────────────────────────────
# Tab 1 — Handling
# ─────────────────────────────────────────────────────────────────────────────

class HandlingTab(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self._parser   = HandlingParser() if _HANDLING_AVAILABLE else None
        self._cur_idx  = -1
        self._blocking = False
        self._field_widgets: Dict[str, QWidget] = {}
        self._stat_bars:     Dict[str, tuple]   = {}
        self._flag_labels:   Dict[int, QLabel]  = {}
        self._build_ui()

    def _build_ui(self): #vers 1
        root = QHBoxLayout(self); root.setContentsMargins(4, 4, 4, 4)
        if not _HANDLING_AVAILABLE:
            root.addWidget(QLabel("Handling Editor unavailable — check Handling_Editor/handling_editor.py"))
            return
        sp = QSplitter(Qt.Orientation.Horizontal)
        # Left
        left = QWidget(); ll = QVBoxLayout(left); ll.setContentsMargins(2,2,2,2)
        ll.addWidget(QLabel("Vehicles"))
        self._search = QLineEdit(); self._search.setPlaceholderText("Search…")
        self._search.textChanged.connect(lambda t: self._refresh_list(t))
        ll.addWidget(self._search)
        self._veh_list = QListWidget()
        self._veh_list.currentRowChanged.connect(self._on_select)
        ll.addWidget(self._veh_list)
        br = QHBoxLayout()
        for lbl, fn in [("Add", self._add), ("Del", self._delete), ("Dup", self._dup)]:
            b = QPushButton(lbl); b.setFixedHeight(24); b.clicked.connect(fn); br.addWidget(b)
        ll.addLayout(br)
        sp.addWidget(left)
        # Centre
        scroll = QScrollArea(); scroll.setWidgetResizable(True)
        ctr = QWidget(); scroll.setWidget(ctr)
        self._form = QFormLayout(ctr); self._form.setSpacing(3); self._form.setContentsMargins(6,6,6,6)
        for fname, ftype, fmin, fmax, tip in VC_FIELDS:
            lbl = QLabel(fname); lbl.setToolTip(tip); lbl.setFixedWidth(200)
            if ftype == "float":
                w = QDoubleSpinBox(); w.setRange(float(fmin), float(fmax)); w.setDecimals(4); w.setSingleStep(0.01)
                w.valueChanged.connect(lambda v, n=fname: self._changed(n, v))
            elif ftype == "int":
                w = QSpinBox(); w.setRange(int(fmin), int(fmax))
                w.valueChanged.connect(lambda v, n=fname: self._changed(n, v))
            elif ftype == "bool":
                w = QCheckBox()
                w.stateChanged.connect(lambda v, n=fname: self._changed(n, int(v > 0)))
            elif ftype == "char":
                w = QComboBox()
                w.addItems(["F","R","4"] if fname == "DriveType" else ["P","D","E"])
                w.currentTextChanged.connect(lambda v, n=fname: self._changed(n, v))
            else:
                w = QLineEdit()
                w.textChanged.connect(lambda v, n=fname: self._changed(n, v))
            w.setToolTip(tip)
            self._field_widgets[fname] = w
            self._form.addRow(lbl, w)
        sp.addWidget(scroll)
        # Right
        right = QWidget(); rl = QVBoxLayout(right); rl.setContentsMargins(2,2,2,2)
        rl.addWidget(QLabel("Vehicle Stats"))
        for label, fn, mx in [("Top Speed","MaxVelocity",200),("Mass","Mass",5000),
                               ("Braking","BrakeDeceleration",30),("Traction","TractionMultiplier",3),
                               ("Engine","EngineAcceleration",20),("Suspension","SuspensionForceLevel",5)]:
            row = QHBoxLayout(); l2 = QLabel(label); l2.setFixedWidth(80)
            bar = QProgressBar(); bar.setRange(0,100); bar.setFixedHeight(18)
            self._stat_bars[fn] = (bar, mx); row.addWidget(l2); row.addWidget(bar); rl.addLayout(row)
        rl.addStretch()
        grp = QGroupBox("Active Flags"); fl = QVBoxLayout(grp)
        for bit, name in list(HANDLING_FLAGS.items())[:16]:
            lbl2 = QLabel(name); lbl2.setStyleSheet("color:#888;")
            lbl2.setFont(QFont("Monospace",8))
            self._flag_labels[bit] = lbl2; fl.addWidget(lbl2)
        rl.addWidget(grp)
        sp.addWidget(right)
        sp.setSizes([200, 600, 220])
        root.addWidget(sp)

    def load_file(self, path: str) -> bool: #vers 1
        if not self._parser: return False
        ok = self._parser.load(path)
        if ok: self._refresh_list()
        return ok

    def save_file(self, path: str) -> bool: #vers 1
        return self._parser.save(path) if self._parser else False

    def _refresh_list(self, ft: str = ""): #vers 1
        self._veh_list.clear()
        for i, e in enumerate(self._parser.entries):
            if ft and ft.lower() not in e.name.lower(): continue
            item = QListWidgetItem(e.name); item.setData(Qt.ItemDataRole.UserRole, i)
            self._veh_list.addItem(item)

    def _on_select(self, row): #vers 1
        item = self._veh_list.item(row)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None or idx >= len(self._parser.entries): return
        self._cur_idx = idx; self._populate(self._parser.entries[idx])

    def _populate(self, entry): #vers 1
        self._blocking = True
        for i, (fname, ftype, *_) in enumerate(VC_FIELDS):
            if i >= len(entry.values): break
            w = self._field_widgets.get(fname)
            if not w: continue
            try:
                v = entry.values[i]
                if ftype == "float": w.setValue(float(v))
                elif ftype == "int": w.setValue(int(v))
                elif ftype == "bool": w.setChecked(int(v) != 0)
                elif ftype == "char" and hasattr(w,"setCurrentText"): w.setCurrentText(str(v))
                elif hasattr(w,"setText"): w.setText(str(v))
            except: pass
        self._blocking = False
        self._update_stats(entry)

    def _changed(self, fname, value): #vers 1
        if self._blocking or self._cur_idx < 0: return
        entry = self._parser.entries[self._cur_idx]
        for i, (fn, *_) in enumerate(VC_FIELDS):
            if fn == fname and i < len(entry.values): entry.values[i] = str(value); break
        self._update_stats(entry)

    def _update_stats(self, entry): #vers 1
        fm = {f[0]: i for i, f in enumerate(VC_FIELDS)}
        for fn, (bar, mx) in self._stat_bars.items():
            idx = fm.get(fn)
            if idx is not None and idx < len(entry.values):
                try: v=float(entry.values[idx]); bar.setValue(min(100,int(v/mx*100))); bar.setFormat(f"{v:.1f}")
                except: bar.setValue(0)
        hf = fm.get("HandlingFlags")
        if hf and hf < len(entry.values):
            try:
                flags = int(entry.values[hf], 16)
                for bit, lbl in self._flag_labels.items():
                    lbl.setStyleSheet("color:#50e090;font-weight:bold;" if flags & bit else "color:#888;")
            except: pass

    def _add(self): #vers 1
        if not self._parser: return
        tmpl = self._parser.entries[0].values[:] if self._parser.entries else ["NEWVEHICLE"]+["0.0"]*36
        tmpl[0] = "NEWVEHICLE"
        e = HandlingEntry(); e.values = tmpl
        self._parser.entries.append(e); self._refresh_list(self._search.text())
        self._veh_list.setCurrentRow(self._veh_list.count()-1)

    def _delete(self): #vers 1
        if self._cur_idx < 0 or not self._parser: return
        name = self._parser.entries[self._cur_idx].name
        if QMessageBox.question(self,"Delete",f"Delete {name}?") != QMessageBox.StandardButton.Yes: return
        self._parser.entries.pop(self._cur_idx); self._cur_idx = -1
        self._refresh_list(self._search.text())

    def _dup(self): #vers 1
        if self._cur_idx < 0 or not self._parser: return
        src = self._parser.entries[self._cur_idx]
        e = HandlingEntry(); e.values = src.values[:]; e.values[0] = src.values[0]+"_COPY"
        self._parser.entries.insert(self._cur_idx+1, e); self._refresh_list(self._search.text())


# ─────────────────────────────────────────────────────────────────────────────
# Tab 2 — Car Colours
# ─────────────────────────────────────────────────────────────────────────────

class CarColoursTab(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self._parser  = CarColsParser()
        self._cur_veh = -1
        self._sel_col = -1
        self._build_ui()

    def _build_ui(self): #vers 1
        root = QVBoxLayout(self); root.setContentsMargins(4,4,4,4)
        sp = QSplitter(Qt.Orientation.Horizontal)
        # Left
        left = QWidget(); ll = QVBoxLayout(left); ll.setContentsMargins(2,2,2,2)
        ll.addWidget(QLabel("Vehicles"))
        self._search = QLineEdit(); self._search.setPlaceholderText("Search…")
        self._search.textChanged.connect(lambda t: self._refresh_veh_list(t))
        ll.addWidget(self._search)
        self._veh_list = QListWidget()
        self._veh_list.currentRowChanged.connect(self._on_vehicle_selected)
        ll.addWidget(self._veh_list)
        br = QHBoxLayout()
        for lbl, fn in [("Add",self._add_vehicle),("Del",self._delete_vehicle)]:
            b=QPushButton(lbl); b.setFixedHeight(24); b.clicked.connect(fn); br.addWidget(b)
        ll.addLayout(br)
        sp.addWidget(left)
        # Centre
        centre = QWidget(); cl = QVBoxLayout(centre); cl.setContentsMargins(4,4,4,4)
        cl.addWidget(QLabel("Global Colour Palette  (click to select / double-click to edit)"))
        self._pal_grid = ColourSwatchGrid()
        self._pal_grid.colour_clicked = self._on_colour_index_clicked
        pal_scroll = QScrollArea(); pal_scroll.setWidgetResizable(True)
        pal_scroll.setWidget(self._pal_grid); pal_scroll.setFixedHeight(160)
        cl.addWidget(pal_scroll)
        cl.addWidget(QLabel("Vehicle Colour Pairs"))
        self._pairs = QTableWidget(0, 3)
        self._pairs.setHorizontalHeaderLabels(["#","Primary","Secondary"])
        self._pairs.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        self._pairs.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        cl.addWidget(self._pairs)
        pr = QHBoxLayout()
        for lbl, fn in [("Add Pair",self._add_pair),("Remove Pair",self._remove_pair)]:
            b=QPushButton(lbl); b.setFixedHeight(24); b.clicked.connect(fn); pr.addWidget(b)
        cl.addLayout(pr)
        sp.addWidget(centre)
        # Right
        right = QWidget(); rl = QVBoxLayout(right); rl.setContentsMargins(4,4,4,4)
        rl.addWidget(QLabel("Selected Colour"))
        self._swatch = QFrame(); self._swatch.setFixedSize(80,80)
        self._swatch.setStyleSheet("background:rgb(0,0,0);border:2px solid #555;")
        rl.addWidget(self._swatch)
        self._col_info = QLabel("Index: —\nRGB: —"); rl.addWidget(self._col_info)
        edit_btn = QPushButton("Edit Colour"); edit_btn.clicked.connect(self._edit_selected_colour)
        rl.addWidget(edit_btn); rl.addStretch()
        self._pal_count = QLabel("0 colours"); rl.addWidget(self._pal_count)
        sp.addWidget(right)
        sp.setSizes([200,560,150]); root.addWidget(sp)

    def load_file(self, path: str) -> bool: #vers 1
        ok = self._parser.load(path)
        if ok:
            self._pal_grid.set_palette(self._parser.colours)
            self._pal_count.setText(f"{len(self._parser.colours)} colours")
            self._refresh_veh_list()
        return ok

    def save_file(self, path: str) -> bool: #vers 1
        return self._parser.save(path)

    def _refresh_veh_list(self, ft: str = ""): #vers 1
        self._veh_list.clear()
        for i, v in enumerate(self._parser.vehicles):
            if ft and ft.lower() not in v.name.lower(): continue
            item = QListWidgetItem(v.name); item.setData(Qt.ItemDataRole.UserRole, i)
            self._veh_list.addItem(item)

    def _on_vehicle_selected(self, row): #vers 1
        item = self._veh_list.item(row)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None: return
        self._cur_veh = idx; self._populate_vehicle_colours(self._parser.vehicles[idx])

    def _populate_vehicle_colours(self, entry: CarColEntry): #vers 1
        self._pairs.setRowCount(0)
        for i, (p, s) in enumerate(entry.palettes):
            self._pairs.insertRow(i)
            self._pairs.setItem(i, 0, QTableWidgetItem(f"Pair {i+1}"))
            pc = self._parser.colours[p] if p < len(self._parser.colours) else CarColour()
            pi = QTableWidgetItem(f"{p}  ({pc.r},{pc.g},{pc.b})")
            pi.setBackground(pc.to_qcolor())
            pi.setForeground(QColor(255,255,255) if (pc.r+pc.g+pc.b)<384 else QColor(0,0,0))
            self._pairs.setItem(i, 1, pi)
            sc2 = self._parser.colours[s] if s < len(self._parser.colours) else CarColour()
            si = QTableWidgetItem(f"{s}  ({sc2.r},{sc2.g},{sc2.b})")
            si.setBackground(sc2.to_qcolor()); si.setForeground(QColor(255,255,255) if (sc2.r+sc2.g+sc2.b)<384 else QColor(0,0,0))
            self._pairs.setItem(i, 2, si)

    def _on_colour_index_clicked(self, idx): #vers 1
        self._sel_col = idx
        if idx < len(self._parser.colours):
            c = self._parser.colours[idx]
            self._swatch.setStyleSheet(f"background:rgb({c.r},{c.g},{c.b});border:2px solid #555;")
            self._col_info.setText(f"Index: {idx}\nRGB: {c.r}, {c.g}, {c.b}")

    def _edit_selected_colour(self): #vers 1
        idx = self._sel_col
        if idx < 0 or idx >= len(self._parser.colours): return
        c = self._parser.colours[idx]
        col = QColorDialog.getColor(c.to_qcolor(), self, f"Edit Colour {idx}")
        if not col.isValid(): return
        self._parser.colours[idx] = CarColour(col.red(), col.green(), col.blue())
        self._pal_grid.set_palette(self._parser.colours)
        self._on_colour_index_clicked(idx)
        if self._cur_veh >= 0: self._populate_vehicle_colours(self._parser.vehicles[self._cur_veh])

    def _add_vehicle(self): #vers 1
        self._parser.vehicles.append(CarColEntry(name="NEWVEHICLE", palettes=[(0,1)]))
        self._refresh_veh_list(self._search.text())

    def _delete_vehicle(self): #vers 1
        if self._cur_veh < 0: return
        name = self._parser.vehicles[self._cur_veh].name
        if QMessageBox.question(self,"Delete",f"Delete {name}?") != QMessageBox.StandardButton.Yes: return
        self._parser.vehicles.pop(self._cur_veh); self._cur_veh = -1
        self._refresh_veh_list(self._search.text())

    def _add_pair(self): #vers 1
        if self._cur_veh < 0: return
        self._parser.vehicles[self._cur_veh].palettes.append((0,1))
        self._populate_vehicle_colours(self._parser.vehicles[self._cur_veh])

    def _remove_pair(self): #vers 1
        if self._cur_veh < 0: return
        row = self._pairs.currentRow(); v = self._parser.vehicles[self._cur_veh]
        if 0 <= row < len(v.palettes): v.palettes.pop(row); self._populate_vehicle_colours(v)


# ─────────────────────────────────────────────────────────────────────────────
# Tab 3 — Car Mods (SA only)
# ─────────────────────────────────────────────────────────────────────────────

class CarModsTab(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self._parser  = CarModsParser()
        self._cur_idx = -1
        self._build_ui()

    def _build_ui(self): #vers 1
        root = QHBoxLayout(self); root.setContentsMargins(4,4,4,4)
        sp = QSplitter(Qt.Orientation.Horizontal)
        left = QWidget(); ll = QVBoxLayout(left); ll.setContentsMargins(2,2,2,2)
        ll.addWidget(QLabel("Vehicles  (SA only)"))
        self._search = QLineEdit(); self._search.setPlaceholderText("Search…")
        self._search.textChanged.connect(lambda t: self._refresh_list(t)); ll.addWidget(self._search)
        self._veh_list = QListWidget(); self._veh_list.currentRowChanged.connect(self._on_vehicle_selected)
        ll.addWidget(self._veh_list)
        br = QHBoxLayout()
        for lbl, fn in [("Add",self._add_vehicle),("Del",self._delete_vehicle)]:
            b=QPushButton(lbl); b.setFixedHeight(24); b.clicked.connect(fn); br.addWidget(b)
        ll.addLayout(br); sp.addWidget(left)
        right = QWidget(); rl = QVBoxLayout(right); rl.setContentsMargins(4,4,4,4)
        rl.addWidget(QLabel("Mod Slots"))
        self._mod_list = QListWidget()
        self._mod_list.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        rl.addWidget(self._mod_list)
        mr = QHBoxLayout()
        self._new_mod = QLineEdit(); self._new_mod.setPlaceholderText("mod_model_name"); mr.addWidget(self._new_mod)
        for lbl, fn in [("Add",self._add_mod),("Remove",self._remove_mod)]:
            b=QPushButton(lbl); b.setFixedHeight(24); b.clicked.connect(fn); mr.addWidget(b)
        rl.addLayout(mr); sp.addWidget(right)
        sp.setSizes([250,600]); root.addWidget(sp)

    def load_file(self, path: str) -> bool: #vers 1
        ok = self._parser.load(path)
        if ok: self._refresh_list()
        return ok

    def save_file(self, path: str) -> bool: #vers 1
        return self._parser.save(path)

    def _refresh_list(self, ft: str = ""): #vers 1
        self._veh_list.clear()
        for i, e in enumerate(self._parser.entries):
            if ft and ft.lower() not in e.vehicle.lower(): continue
            item = QListWidgetItem(e.vehicle); item.setData(Qt.ItemDataRole.UserRole, i)
            self._veh_list.addItem(item)

    def _on_vehicle_selected(self, row): #vers 1
        item = self._veh_list.item(row)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None: return
        self._cur_idx = idx; self._populate_mods(self._parser.entries[idx])

    def _populate_mods(self, entry: CarModEntry): #vers 1
        self._mod_list.clear()
        for mod in entry.mods: self._mod_list.addItem(mod)

    def _add_vehicle(self): #vers 1
        self._parser.entries.append(CarModEntry(vehicle="newvehicle",mods=[]))
        self._refresh_list(self._search.text())

    def _delete_vehicle(self): #vers 1
        if self._cur_idx < 0: return
        name = self._parser.entries[self._cur_idx].vehicle
        if QMessageBox.question(self,"Delete",f"Delete {name}?") != QMessageBox.StandardButton.Yes: return
        self._parser.entries.pop(self._cur_idx); self._cur_idx = -1
        self._refresh_list(self._search.text())

    def _add_mod(self): #vers 1
        if self._cur_idx < 0: return
        name = self._new_mod.text().strip()
        if not name: return
        entry = self._parser.entries[self._cur_idx]
        entry.mods.append(name); self._populate_mods(entry); self._new_mod.clear()

    def _remove_mod(self): #vers 1
        if self._cur_idx < 0: return
        rows = sorted({i.row() for i in self._mod_list.selectedItems()}, reverse=True)
        entry = self._parser.entries[self._cur_idx]
        for r in rows:
            if 0 <= r < len(entry.mods): entry.mods.pop(r)
        self._populate_mods(entry)


# ─────────────────────────────────────────────────────────────────────────────
# Main workshop
# ─────────────────────────────────────────────────────────────────────────────

class VehicleWorkshop(GLViewportMixin, GUIWorkshop): #vers 3
    App_name   = "Vehicle Workshop"
    App_build  = "Build 2"
    App_auth   = "X-Seti"
    config_key = "vehicle_workshop"

    def __init__(self, main_window=None, parent=None):
        super().__init__(parent)
        self.main_window = main_window
        self._handling_path: Optional[str] = None
        self._carcols_path:  Optional[str] = None
        self._carmods_path:  Optional[str] = None
        self.setup_ui()
        self._set_status("Open handling.cfg, carcols.dat or carmods.dat to begin")

    def _create_centre_panel(self): #vers 1
        from PyQt6.QtWidgets import QFrame, QVBoxLayout
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        lay = QVBoxLayout(panel); lay.setContentsMargins(0,0,0,0); lay.setSpacing(0)
        self._tabs = QTabWidget()
        self._tab_handling = HandlingTab()
        self._tab_carcols  = CarColoursTab()
        self._tab_carmods  = CarModsTab()
        self._tabs.addTab(self._tab_handling, "Handling")
        self._tabs.addTab(self._tab_carcols,  "Car Colours")
        self._tabs.addTab(self._tab_carmods,  "Car Mods (SA)")
        self._tab_preview = self._create_preview_tab()
        self._tabs.addTab(self._tab_preview, "3D Preview")
        lay.addWidget(self._tabs)
        return panel

    def setup_ui(self): #vers 3
        super().setup_ui()

    def _create_preview_tab(self): #vers 1
        """Build the 3D Preview tab with DFFViewport and vehicle controls."""
        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout,
                                     QPushButton, QLabel, QComboBox, QSplitter)
        from PyQt6.QtCore import Qt
        tab = QWidget()
        lay = QVBoxLayout(tab); lay.setContentsMargins(4,4,4,4); lay.setSpacing(4)

        # Toolbar row
        bar = QHBoxLayout()
        self._vw_open_dff = QPushButton('Open DFF')
        self._vw_open_txd = QPushButton('Open TXD')
        self._vw_open_dff.clicked.connect(self._vw_pick_dff)
        self._vw_open_txd.clicked.connect(self._vw_pick_txd)
        bar.addWidget(self._vw_open_dff)
        bar.addWidget(self._vw_open_txd)
        bar.addSpacing(8)

        # Render mode
        from PyQt6.QtWidgets import QButtonGroup
        self._vw_mode_grp = QButtonGroup(tab); self._vw_mode_grp.setExclusive(True)
        for label, mode in [('Wire','wireframe'),('Solid','solid'),('Tex','textured')]:
            b = QPushButton(label); b.setCheckable(True); b.setFixedHeight(26)
            b.clicked.connect(lambda _=False, m=mode: self._vw_set_mode(m))
            self._vw_mode_grp.addButton(b); bar.addWidget(b)
            if mode == 'solid': b.setChecked(True)

        bar.addSpacing(8)
        # Assemble button
        self._vw_assemble = QPushButton('All Parts')
        self._vw_assemble.setCheckable(True)
        self._vw_assemble.toggled.connect(self._vw_toggle_assembly)
        bar.addWidget(self._vw_assemble)
        bar.addStretch()

        # Paint colour buttons
        self._vw_paint1 = QPushButton('Primary')
        self._vw_paint2 = QPushButton('Secondary')
        self._vw_paint1.setFixedHeight(26); self._vw_paint2.setFixedHeight(26)
        self._vw_paint1.clicked.connect(self._vw_pick_paint1)
        self._vw_paint2.clicked.connect(self._vw_pick_paint2)
        bar.addWidget(self._vw_paint1)
        bar.addWidget(self._vw_paint2)
        lay.addLayout(bar)

        # GL Viewport
        try:
            from apps.components.Model_Viewer.model_viewer import DFFViewport
            self._vw_viewport = DFFViewport()
            app_settings = getattr(getattr(self,'main_window',None),'app_settings',None)
            self._vw_viewport.app_settings = app_settings
        except Exception:
            self._vw_viewport = QLabel('OpenGL not available')
        lay.addWidget(self._vw_viewport, 1)

        # Status
        self._vw_status = QLabel('Open a DFF to preview')
        lay.addWidget(self._vw_status)
        return tab

    def _vw_set_mode(self, mode): #vers 1
        vp = getattr(self,'_vw_viewport',None)
        if vp and hasattr(vp,'set_render_mode'): vp.set_render_mode(mode)

    def _vw_pick_dff(self): #vers 1
        from PyQt6.QtWidgets import QFileDialog
        path,_=QFileDialog.getOpenFileName(self,'Open DFF','','DFF (*.dff);;All (*)')
        if not path: return
        self._vw_load_dff(path)

    def _vw_pick_txd(self): #vers 1
        from PyQt6.QtWidgets import QFileDialog
        path,_=QFileDialog.getOpenFileName(self,'Open TXD','','TXD (*.txd);;All (*)')
        if not path: return
        self._vw_load_txd(path)

    def _vw_load_dff(self, path: str): #vers 1
        try:
            from apps.methods.dff_parser import load_dff
            m = load_dff(path)
            if not m: return
            self._vw_model = m
            vp = getattr(self,'_vw_viewport',None)
            if vp and hasattr(vp,'load_geometry') and m.geometries:
                g = m.geometries[0]; vp.load_geometry(g, g.materials)
            self._vw_status.setText(f'{os.path.basename(path)} — {len(m.geometries)} geoms')
            # Auto-load matching TXD
            import os as _os
            for ext in ('.txd','.TXD'):
                txd=_os.path.splitext(path)[0]+ext
                if _os.path.isfile(txd): self._vw_load_txd(txd); break
        except Exception as e:
            self._vw_status.setText(f'Error: {e}')

    def _vw_load_txd(self, path: str): #vers 1
        try:
            from apps.methods.txd_parser import parse_txd
            with open(path,'rb') as f: data=f.read()
            textures=parse_txd(data)
            vp=getattr(self,'_vw_viewport',None)
            if vp and hasattr(vp,'_upload_textures') and textures:
                vp._upload_textures(textures)
                vp.update()
        except Exception as e:
            self._vw_status.setText(f'TXD error: {e}')

    def _vw_toggle_assembly(self, enabled: bool): #vers 1
        m = getattr(self,'_vw_model',None)
        vp = getattr(self,'_vw_viewport',None)
        if not m or not vp or not hasattr(vp,'set_assembly_mode'): return
        vp.set_assembly_mode(enabled)
        if enabled:
            vp.load_all_geometries(m.geometries,[g.materials for g in m.geometries],
                                   m.frames,m.atomics)
        elif m.geometries:
            vp.load_geometry(m.geometries[0],m.geometries[0].materials)

    def _vw_pick_paint1(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp=getattr(self,'_vw_viewport',None)
        if not vp or not hasattr(vp,'_paint1'): return
        p=vp._paint1
        col=QColorDialog.getColor(QColor(int(p[0]*255),int(p[1]*255),int(p[2]*255)),self)
        if col.isValid():
            vp._paint1=(col.redF(),col.greenF(),col.blueF())
            self._vw_paint1.setStyleSheet(f'background:{col.name()}')
            vp.update()

    def _vw_pick_paint2(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp=getattr(self,'_vw_viewport',None)
        if not vp or not hasattr(vp,'_paint2'): return
        p=vp._paint2
        col=QColorDialog.getColor(QColor(int(p[0]*255),int(p[1]*255),int(p[2]*255)),self)
        if col.isValid():
            vp._paint2=(col.redF(),col.greenF(),col.blueF())
            self._vw_paint2.setStyleSheet(f'background:{col.name()}')
            vp.update()

    def _open_file(self, path=None): #vers 1

        if path is None:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open Vehicle Data File", "",
                "Vehicle data (handling.cfg carcols.dat carmods.dat *.cfg *.dat);;All files (*)")
        if not path: return
        name = os.path.basename(path).lower()
        if "handling" in name:
            if self._tab_handling.load_file(path):
                self._handling_path = path
                self._tabs.setCurrentWidget(self._tab_handling)
                self._set_status(f"Handling: {os.path.basename(path)}")
        elif "carcols" in name:
            if self._tab_carcols.load_file(path):
                self._carcols_path = path
                self._tabs.setCurrentWidget(self._tab_carcols)
                self._set_status(f"Car Colours: {os.path.basename(path)}")
        elif "carmods" in name:
            if self._tab_carmods.load_file(path):
                self._carmods_path = path
                self._tabs.setCurrentWidget(self._tab_carmods)
                self._set_status(f"Car Mods: {os.path.basename(path)}")
        else:
            QMessageBox.warning(self, "Unknown", "Expected handling.cfg, carcols.dat or carmods.dat")

    def _save_file(self): #vers 1
        idx = self._tabs.currentIndex()
        if idx == 0 and self._handling_path:
            self._tab_handling.save_file(self._handling_path)
            self._set_status(f"Saved {os.path.basename(self._handling_path)}")
        elif idx == 1 and self._carcols_path:
            self._tab_carcols.save_file(self._carcols_path)
            self._set_status(f"Saved {os.path.basename(self._carcols_path)}")
        elif idx == 2 and self._carmods_path:
            self._tab_carmods.save_file(self._carmods_path)
            self._set_status(f"Saved {os.path.basename(self._carmods_path)}")
        else:
            self._save_as()

    def _save_as(self): #vers 1
        idx = self._tabs.currentIndex()
        hints = ["Handling (handling.cfg *.cfg)", "Car Colours (carcols.dat *.dat)", "Car Mods (carmods.dat *.dat)"]
        path, _ = QFileDialog.getSaveFileName(self, "Save As", "", hints[idx])
        if not path: return
        if idx == 0: self._handling_path = path; self._tab_handling.save_file(path)
        elif idx == 1: self._carcols_path = path; self._tab_carcols.save_file(path)
        elif idx == 2: self._carmods_path = path; self._tab_carmods.save_file(path)

    def _open_specific(self, kind: str): #vers 1
        f = {"handling":"Handling (handling.cfg *.cfg)",
             "carcols": "Car Colours (carcols.dat *.dat)",
             "carmods":  "Car Mods (carmods.dat *.dat)"}[kind]
        path, _ = QFileDialog.getOpenFileName(self, f"Open {kind}", "", f)
        if path: self._open_file(path)

    def _build_menus_into_qmenu(self, pm): #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open…",              self._open_file)
        fm.addAction("Save",               self._save_file)
        fm.addAction("Save As…",           self._save_as)
        fm.addSeparator()
        fm.addAction("Open handling.cfg",  lambda: self._open_specific("handling"))
        fm.addAction("Open carcols.dat",   lambda: self._open_specific("carcols"))
        fm.addAction("Open carmods.dat",   lambda: self._open_specific("carmods"))
        fm.addSeparator()
        fm.addAction("Close", self.close)


def open_vehicle_workshop(main_window=None, path: str = None): #vers 1
    app = QApplication.instance() or QApplication(sys.argv)
    w = VehicleWorkshop(main_window)
    w.resize(1200, 720)
    w.show()
    if path: w._open_file(path)
    return w


if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = VehicleWorkshop()
    w.resize(1200, 720)
    w.show()
    sys.exit(app.exec())
