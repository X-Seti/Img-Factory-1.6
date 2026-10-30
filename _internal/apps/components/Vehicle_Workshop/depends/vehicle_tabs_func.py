#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_tabs_func.py - Version: 1
# X-Seti - October 14 2026 - IMG Factory 1.6 - Vehicle Workshop data tabs

"""
Handling, Car Colours and Car Mods tabs.
"""

##class CarColour: -
# __str__
# to_qcolor

##class CarColsParser: -
# __init__
# _detect_game
# load
# save

##class CarModsParser: -
# __init__
# load
# save

##class ColourSwatchGrid: -
# __init__
# colour_clicked
# mousePressEvent
# paintEvent
# set_palette

##class HandlingTab: -
# __init__
# _add
# _build_ui
# _changed
# _delete
# _dup
# _on_select
# _populate
# _refresh_list
# _update_stats
# load_file
# save_file

##class CarColoursTab: -
# __init__
# _add_pair
# _add_vehicle
# _build_ui
# _delete_vehicle
# _edit_selected_colour
# _on_colour_index_clicked
# _on_vehicle_selected
# _populate_vehicle_colours
# _refresh_veh_list
# _remove_pair
# load_file
# save_file

##class CarModsTab: -
# __init__
# _add_mod
# _add_vehicle
# _build_ui
# _delete_vehicle
# _on_vehicle_selected
# _populate_mods
# _refresh_list
# _remove_mod
# load_file
# save_file

from typing import Dict, List

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QColor, QFont, QPainter, QPen
from PyQt6.QtWidgets import (
    QAbstractItemView, QCheckBox, QColorDialog, QComboBox, QDoubleSpinBox, QFormLayout,
    QFrame, QGroupBox, QHBoxLayout, QHeaderView, QLabel, QLineEdit, QListWidget,
    QListWidgetItem, QMessageBox, QProgressBar, QPushButton, QScrollArea, QSizePolicy,
    QSpinBox, QSplitter, QTableWidget, QTableWidgetItem, QToolButton, QVBoxLayout, QWidget)

from apps.methods.handling_file import HandlingEntry, HandlingParser, header_fields
from apps.components.Vehicle_Workshop.depends.vehicle_common_func import (
    HANDLING_FLAGS, SVGIconFactory, VC_FIELDS, _HANDLING_AVAILABLE)


# carcols.dat / carmods.dat: byte-exact models in apps/methods/vehicle_data_files.py
from apps.methods.vehicle_data_files import (
    CarColour as _CarColourBase, CarColEntry, CarModEntry, CarColsFile, CarModsFile)
from apps.methods.img_factory_settings import get_user_config_dir


class CarColour(_CarColourBase): #vers 2
    def to_qcolor(self) -> QColor:  #vers 1
        return QColor(self.r, self.g, self.b)


class CarColsParser(CarColsFile): #vers 2
    def __init__(self): #vers 1
        super().__init__(CarColour)


CarModsParser = CarModsFile


# Colour swatch grid

class ColourSwatchGrid(QWidget): #vers 1
    def __init__(self, parent=None):  #vers 1
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

    def colour_clicked(self, idx: int): pass  # override  #vers 1


# Tab 1 — Handling

class HandlingTab(QWidget): #vers 1
    def __init__(self, parent=None):  #vers 1
        super().__init__(parent)
        self._parser   = HandlingParser() if _HANDLING_AVAILABLE else None
        self._cur_idx  = -1
        self._blocking = False
        self._field_widgets: Dict[str, QWidget] = {}
        self._stat_bars:     Dict[str, tuple]   = {}
        self._flag_labels:   Dict[int, QLabel]  = {}
        self._fields = VC_FIELDS
        self._build_ui()

    def _fill_form(self, fields): #vers 1
        self._fields = fields
        while self._form.rowCount():
            self._form.removeRow(0)
        self._field_widgets.clear()
        for fname, ftype, fmin, fmax, tip in fields:
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
            elif 'DriveType' in fname or 'EngineType' in fname:
                w = QComboBox()
                w.addItems(["F", "R", "4"] if 'DriveType' in fname else ["P", "D", "E"])
                w.currentTextChanged.connect(lambda v, n=fname: self._changed(n, v))
            else:
                w = QLineEdit()
                w.textChanged.connect(lambda v, n=fname: self._changed(n, v))
            w.setToolTip(tip)
            self._field_widgets[fname] = w
            self._form.addRow(lbl, w)

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
        for lbl, tip, fn, iname in [
                ("Add", "Add entry",       self._add,    "add"),
                ("Del", "Delete entry",    self._delete, "delete"),
                ("Dup", "Duplicate entry", self._dup,    "duplicate")]:
            from PyQt6.QtWidgets import QToolButton
            b = QToolButton(); b.setFixedHeight(28); b.setToolTip(tip)
            b.setText(lbl)
            try:
                ico = getattr(SVGIconFactory, f'{iname}_icon', None)
                if ico:
                    b.setIcon(ico(16, '#ffffff'))
                    b.setIconSize(QSize(14,14))
                    b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            except Exception:
                pass
            b.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
            b.clicked.connect(fn); br.addWidget(b)
        ll.addLayout(br)
        sp.addWidget(left)

        # Centre
        scroll = QScrollArea(); scroll.setWidgetResizable(True)
        ctr = QWidget(); scroll.setWidget(ctr)
        self._form = QFormLayout(ctr); self._form.setSpacing(3); self._form.setContentsMargins(6,6,6,6)
        self._fill_form(VC_FIELDS)
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
        if ok:
            n = max((len(e.values) for e in self._parser.entries), default=0)
            hf = header_fields(self._parser._lines)
            if hf and len(hf) != n:
                hf = hf[:22]
            base = hf[:n] if hf else VC_FIELDS[:n]
            self._fill_form(list(base) + [(f"Field {i + 1}", "str", "", "", "Not labelled - edit as text")
                                          for i in range(len(base), n)])
            self._cur_idx = -1
            self._refresh_list()
        return ok

    @property
    def dirty(self) -> bool:
        return bool(self._parser and self._parser.dirty)

    def save_file(self, path: str) -> bool: #vers 2
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
        for i, (fname, ftype, *_) in enumerate(self._fields):
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
        for i, (fn, *_) in enumerate(self._fields):
            if fn == fname and i < len(entry.values): entry.values[i] = str(value); break
        self._update_stats(entry)

    def _update_stats(self, entry): #vers 1
        fm = {f[0]: i for i, f in enumerate(self._fields)}
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
        e = HandlingEntry(tmpl)
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
        e = src.copy_as_new(); e.values[0] = src.values[0]+"_COPY"
        self._parser.entries.insert(self._cur_idx+1, e); self._refresh_list(self._search.text())


# Tab 2 — Car Colours

class CarColoursTab(QWidget): #vers 1
    def __init__(self, parent=None):  #vers 1
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
        for lbl, tip, fn, iname in [("Add",'Add',self._add_vehicle,'add'),("Del",'Delete',self._delete_vehicle,'delete')]:
            from PyQt6.QtWidgets import QToolButton
            b=QToolButton(); b.setFixedHeight(28); b.setToolTip(tip); b.setText(lbl)
            try:
                ico=getattr(SVGIconFactory,f'{iname}_icon',None)
                if ico: b.setIcon(ico(14,'#ffffff')); b.setIconSize(QSize(14,14)); b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            except Exception: pass
            b.setSizePolicy(QSizePolicy.Policy.Preferred,QSizePolicy.Policy.Fixed)

            b=QPushButton(lbl); b.setFixedHeight(26); b.clicked.connect(fn); br.addWidget(b)
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
            b=QPushButton(lbl); b.setFixedHeight(28); b.clicked.connect(fn); pr.addWidget(b)
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

    @property
    def dirty(self) -> bool:
        return self._parser.dirty

    def save_file(self, path: str) -> bool: #vers 2
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


# Tab 3 — Car Mods (SA only)

class CarModsTab(QWidget): #vers 1
    def __init__(self, parent=None):  #vers 1
        super().__init__(parent)
        self._parser  = CarModsParser()
        self._cur_idx = -1
        self._build_ui()

    def _build_ui(self): #vers 1
        root = QHBoxLayout(self); root.setContentsMargins(4,4,4,4)
        sp = QSplitter(Qt.Orientation.Horizontal)
        left = QWidget(); ll = QVBoxLayout(left); ll.setContentsMargins(2,2,2,2)
        ll.addWidget(QLabel("Vehicles (SA only)"))
        self._search = QLineEdit(); self._search.setPlaceholderText("Search…")
        self._search.textChanged.connect(lambda t: self._refresh_list(t)); ll.addWidget(self._search)
        self._veh_list = QListWidget(); self._veh_list.currentRowChanged.connect(self._on_vehicle_selected)
        ll.addWidget(self._veh_list)
        br = QHBoxLayout()
        for lbl, tip, fn, iname in [("Add",'Add',self._add_vehicle,'add'),("Del",'Delete',self._delete_vehicle,'delete')]:
            from PyQt6.QtWidgets import QToolButton
            b=QToolButton(); b.setFixedHeight(28); b.setToolTip(tip); b.setText(lbl)
            try:
                ico=getattr(SVGIconFactory,f'{iname}_icon',None)
                if ico: b.setIcon(ico(14,'#ffffff')); b.setIconSize(QSize(14,14)); b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            except Exception: pass
            b.setSizePolicy(QSizePolicy.Policy.Preferred,QSizePolicy.Policy.Fixed)

            b=QPushButton(lbl); b.setFixedHeight(28); b.clicked.connect(fn); br.addWidget(b)
        ll.addLayout(br); sp.addWidget(left)
        right = QWidget(); rl = QVBoxLayout(right); rl.setContentsMargins(4,4,4,4)
        rl.addWidget(QLabel("Mod Slots"))
        self._mod_list = QListWidget()
        self._mod_list.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        rl.addWidget(self._mod_list)
        mr = QHBoxLayout()
        self._new_mod = QLineEdit(); self._new_mod.setPlaceholderText("mod_model_name"); mr.addWidget(self._new_mod)
        for lbl, fn in [("Add",self._add_mod),("Remove",self._remove_mod)]:
            b=QPushButton(lbl); b.setFixedHeight(28); b.clicked.connect(fn); mr.addWidget(b)
        rl.addLayout(mr); sp.addWidget(right)
        sp.setSizes([250,600]); root.addWidget(sp)

    def load_file(self, path: str) -> bool: #vers 1
        ok = self._parser.load(path)
        if ok: self._refresh_list()
        return ok

    @property
    def dirty(self) -> bool:
        return self._parser.dirty

    def save_file(self, path: str) -> bool: #vers 2
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


# Main workshop
