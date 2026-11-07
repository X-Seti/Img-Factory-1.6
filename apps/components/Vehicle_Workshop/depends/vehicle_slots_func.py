#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_slots_func.py - Version: 1
# X-Seti - November 06 2026 - IMG Factory 1.6 - Vehicle Workshop slots tab

"""
Slots tab: every vehicle ID of a III / VC / SA game folder with its model,
TXD, handling and colours; add (copy of a slot), swap model, remove, check.
Data: apps/methods/vehicle_slots.py.
"""

##class VehicleSlotsTab: -
# __init__
# _add
# _check
# _current
# _fill
# _img_target
# _open_root
# _pick_file
# _remove
# _save
# _swap
# load_root

import os

from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor
from PyQt6.QtWidgets import (QCheckBox, QDialog, QDialogButtonBox, QFileDialog, QFormLayout, QHBoxLayout,
                             QHeaderView, QLabel, QLineEdit, QMessageBox, QPushButton, QSpinBox,
                             QTableWidget, QTableWidgetItem, QVBoxLayout, QWidget)

_COLS = ("ID", "Model", "TXD", "Type", "Handling", "Name", "Status")


class VehicleSlotsTab(QWidget): #vers 1
    """Vehicle ID slots of one game folder."""

    def __init__(self, ws=None, root_hint=None, parent=None): #vers 1
        super().__init__(parent)
        self._ws, self._root_hint = ws, root_hint        # workshop settings, game root getter
        self.vs = None
        lay = QVBoxLayout(self)
        lay.setContentsMargins(4, 4, 4, 4)
        top = QHBoxLayout()
        self._root_lbl = QLabel("No game folder")
        top.addWidget(self._root_lbl, 1)
        for text, tip, fn in (("Folder", "Game folder (holds data/ and models/)", self._open_root),
                              ("Check", "Find missing models, TXDs, handling, colours, duplicates", self._check),
                              ("Add", "New slot copied from the selected one", self._add),
                              ("Swap", "Replace the selected slot's DFF / TXD", self._swap),
                              ("Remove", "Remove the selected slot from the data files", self._remove),
                              ("Save", "Write the changed data files (backed up as .bak)", self._save)):
            b = QPushButton(text)
            b.setToolTip(tip)
            b.clicked.connect(fn)
            top.addWidget(b)
        lay.addLayout(top)
        flt = QHBoxLayout()
        self._filter = QLineEdit()
        self._filter.setPlaceholderText("Filter by ID, model or name")
        self._filter.textChanged.connect(self._fill)
        flt.addWidget(self._filter, 1)
        self._bad_only = QCheckBox("Problems only")
        self._bad_only.toggled.connect(self._fill)
        flt.addWidget(self._bad_only)
        self._range_lbl = QLabel("")
        flt.addWidget(self._range_lbl)
        lay.addLayout(flt)
        self._table = QTableWidget(0, len(_COLS))
        self._table.setHorizontalHeaderLabels(_COLS)
        self._table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.ResizeToContents)
        self._table.horizontalHeader().setStretchLastSection(True)
        self._table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self._table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        lay.addWidget(self._table, 1)
        root = (self._ws.get('slots_root', '') if self._ws else '') or (root_hint() if root_hint else '')
        if root:
            self.load_root(root, quiet=True)

    def load_root(self, root: str, quiet: bool = False) -> bool: #vers 1
        """Read the slots of a game folder."""
        from apps.methods.vehicle_slots import VehicleSlots
        try:
            self.vs = VehicleSlots(root)
        except (ValueError, OSError) as e:
            if not quiet:
                QMessageBox.warning(self, "Vehicle slots", str(e))
            return False
        if self._ws:
            self._ws.set('slots_root', root)
            self._ws.save()
        self._root_lbl.setText(f"{root}  ({self.vs.game.upper()}, {len(self.vs.slots)} vehicles)")
        self._check()
        return True

    def _open_root(self): #vers 1
        d = QFileDialog.getExistingDirectory(self, "Game folder", self.vs.root if self.vs else '')
        if d:
            self.load_root(d)

    def _check(self): #vers 1
        if self.vs is None:
            return
        self.vs.check()
        self._fill()

    def _fill(self, *_): #vers 1
        """Table rows from the slots (filter, problems only)."""
        from apps.methods.vehicle_slots import ID_RANGES, free_ids
        t = self._table
        t.setRowCount(0)
        if self.vs is None:
            return
        ft = self._filter.text().strip().lower()
        for s in sorted(self.vs.slots, key=lambda s: s.id):
            if ft and ft not in f"{s.id} {s.model} {s.gxt}".lower():
                continue
            if self._bad_only.isChecked() and not s.problems:
                continue
            r = t.rowCount()
            t.insertRow(r)
            for c, v in enumerate((s.id, s.model, s.txd, s.type, s.handling, s.gxt,
                                   ", ".join(s.problems) or "ok")):
                it = QTableWidgetItem(str(v))
                it.setData(Qt.ItemDataRole.UserRole, s)
                if s.problems:
                    it.setForeground(QColor(220, 90, 70))
                t.setItem(r, c, it)
        lo, hi = ID_RANGES[self.vs.game]
        self._range_lbl.setText(f"standard {lo}-{hi}: {len(free_ids(self.vs.slots, lo, hi))} free")

    def _current(self): #vers 1
        it = self._table.item(self._table.currentRow(), 0)
        return it.data(Qt.ItemDataRole.UserRole) if it else None

    def _pick_file(self, title: str, ext: str) -> bytes: #vers 1
        p, _ = QFileDialog.getOpenFileName(self, title, '', f"{ext.upper()} (*.{ext})")
        if not p:
            return b''
        with open(p, 'rb') as f:
            return f.read()

    def _img_target(self) -> str: #vers 1
        """models/vehicles.img (made on first write, SA gta.dat line added), or gta3.img; '' to cancel."""
        from apps.methods.vehicle_slots import _find
        models = _find(self.vs.root, 'models') or os.path.join(self.vs.root, 'models')
        veh = _find(models, 'vehicles.img')
        if veh:
            return veh
        box = QMessageBox(self)
        box.setWindowTitle("Vehicle slots")
        box.setText("models/vehicles.img does not exist.\n\nMake it (SA: an IMG line is added to gta.dat; "
                    "III / VC need a loader for it), or write into gta3.img?")
        make = box.addButton("Make vehicles.img", QMessageBox.ButtonRole.AcceptRole)
        gta3 = box.addButton("Use gta3.img", QMessageBox.ButtonRole.AcceptRole)
        box.addButton(QMessageBox.StandardButton.Cancel)
        box.exec()
        if box.clickedButton() is make:
            self.vs.register_img()
            return os.path.join(models, 'vehicles.img')
        if box.clickedButton() is gta3:
            return _find(models, 'gta3.img') or ''
        return ''

    def _add(self): #vers 1
        """New slot from the selected one: ID, name, then its DFF / TXD."""
        from apps.methods.vehicle_slots import ID_RANGES, free_ids
        tpl = self._current()
        if self.vs is None or tpl is None:
            QMessageBox.information(self, "Vehicle slots", "Select a slot to copy first")
            return
        lo, hi = ID_RANGES[self.vs.game]
        free = free_ids(self.vs.slots, lo, hi)
        dlg = QDialog(self)
        dlg.setWindowTitle(f"New vehicle from {tpl.model}")
        form = QFormLayout(dlg)
        sid = QSpinBox()
        sid.setRange(1, 65535)
        sid.setValue(free[0] if free else max(s.id for s in self.vs.slots) + 1)
        form.addRow("ID", sid)
        name = QLineEdit()
        form.addRow("Model name", name)
        txd = QLineEdit()
        txd.setPlaceholderText("same as the model")
        form.addRow("TXD name", txd)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept)
        bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted or not name.text().strip():
            return
        try:
            slot = self.vs.add_slot(tpl, sid.value(), name.text().strip().lower(),
                                    txd.text().strip().lower() or None)
        except ValueError as e:
            QMessageBox.warning(self, "Vehicle slots", str(e))
            return
        dff = self._pick_file(f"DFF for {slot.model} (cancel: add it later)", 'dff')
        tex = self._pick_file(f"TXD for {slot.txd} (cancel: add it later)", 'txd') if dff else b''
        if dff or tex:
            img = self._img_target()
            if img:
                self.vs.set_model(img, slot, dff, tex)
        self._check()

    def _swap(self): #vers 1
        """Replace the selected slot's model / textures in its IMG."""
        s = self._current()
        if self.vs is None or s is None:
            return
        dff = self._pick_file(f"New DFF for {s.model} (cancel: keep)", 'dff')
        tex = self._pick_file(f"New TXD for {s.txd} (cancel: keep)", 'txd')
        if not dff and not tex:
            return
        imgs = self.vs.model_imgs()
        img = imgs.get(s.model.lower() + '.dff') or imgs.get(s.txd.lower() + '.txd') or self._img_target()
        if img:
            self.vs.set_model(img, s, dff, tex)
            QMessageBox.information(self, "Vehicle slots", f"{s.model}: written to {os.path.basename(img)} "
                                    "(originals in vehicle_backup/)")
        self._check()

    def _remove(self): #vers 1
        s = self._current()
        if self.vs is None or s is None:
            return
        if QMessageBox.question(self, "Vehicle slots", f"Remove {s.model} (ID {s.id}) from the IDE, handling, "
                                "colours and mods?\n\nThe model stays in its IMG. Save writes the files.") \
                != QMessageBox.StandardButton.Yes:
            return
        self.vs.remove_slot(s)
        self._check()

    def _save(self): #vers 1
        if self.vs is None:
            return
        done = self.vs.save()
        QMessageBox.information(self, "Vehicle slots", "Saved " + (", ".join(os.path.basename(p) for p in done)
                                                                   or "nothing (no changes)"))
