#this belongs in apps/components/Map_Editor/depends/map_zone_func.py - Version: 1
# X-Seti - November 02 2026 - IMG Factory 1.6 - Map Workshop zones

"""
Zones dock (table, filters) and zone edit dialog for ModelWorkshop (Map).
"""

##class MapZoneMixin: -
# _create_zones_dock
# _edit_zone
# _on_zone_row_clicked
# _refresh_zones_table
# _selected_zone_index
# _show_zones_dock

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (
    QAbstractItemView, QComboBox, QDialog, QDialogButtonBox, QDockWidget,
    QDoubleSpinBox, QFormLayout, QHBoxLayout, QLineEdit, QPushButton,
    QSpinBox, QTableWidget, QTableWidgetItem, QVBoxLayout, QWidget)

# eZoneType (III/VC/SA)
ZONE_TYPES = {0: "Navig name", 1: "Navig", 2: "Info", 3: "Map zone"}
ZONE_COLS = ["Name", "Type", "File", "Island", "Text Key",
             "Min X", "Min Y", "Min Z", "Max X", "Max Y", "Max Z"]
_BOUNDS = ('min_x', 'min_y', 'min_z', 'max_x', 'max_y', 'max_z')


class MapZoneMixin:

    def _create_zones_dock(self): #vers 1
        """Zones dock: filters, sortable table, add/edit/delete/save."""
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(2, 2, 2, 2)
        row = QHBoxLayout()
        self._zone_file_filter = QComboBox()
        self._zone_type_filter = QComboBox()
        self._zone_type_filter.addItem("All types", -1)
        for t, label in ZONE_TYPES.items():
            self._zone_type_filter.addItem(f"{t} {label}", t)
        self._zone_search = QLineEdit()
        self._zone_search.setPlaceholderText("Name / text key")
        for w in (self._zone_file_filter, self._zone_type_filter):
            w.currentIndexChanged.connect(lambda *_: self._refresh_zones_table())
            row.addWidget(w)
        self._zone_search.textChanged.connect(lambda *_: self._refresh_zones_table())
        row.addWidget(self._zone_search, 1)
        lay.addLayout(row)

        table = QTableWidget(0, len(ZONE_COLS))
        table.setHorizontalHeaderLabels(ZONE_COLS)
        table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        table.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        table.setSortingEnabled(True)
        table.verticalHeader().setVisible(False)
        if hasattr(self, '_apply_compact_table_style'):
            self._apply_compact_table_style(table)
        table.cellClicked.connect(self._on_zone_row_clicked)
        table.cellDoubleClicked.connect(lambda r, c: self._edit_zone())
        lay.addWidget(table, 1)
        self._zones_table = table

        btns = QHBoxLayout()
        for text, slot in (("Add", lambda: self._add_zone()),
                           ("Edit", lambda: self._edit_zone()),
                           ("Delete", lambda: self._delete_zone()),
                           ("Save", lambda: self._save_zones())):
            b = QPushButton(text)
            b.clicked.connect(slot)
            btns.addWidget(b)
        lay.addLayout(btns)

        dock = QDockWidget("Zones", self)
        dock.setObjectName("Zones")
        dock.setWidget(panel)
        dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                         QDockWidget.DockWidgetFeature.DockWidgetFloatable |
                         QDockWidget.DockWidgetFeature.DockWidgetClosable)
        self._zones_dock = dock
        return dock

    def _edit_zone(self, index=None): #vers 1
        """Edit all fields of one zone, undoable."""
        loader = getattr(self, '_world_loader', None)
        zones = getattr(loader, 'zones', []) if loader else []
        if index is None:
            index = self._selected_zone_index()
        if index is None or not (0 <= index < len(zones)):
            self._set_status("No zone selected")
            return
        z = zones[index]
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Edit zone - {z.get('name', '')}")
        form = QFormLayout(dlg)
        name = QLineEdit(z.get('name', '')); name.setMaxLength(7)
        ztype = QComboBox()
        for t, label in ZONE_TYPES.items():
            ztype.addItem(f"{t} {label}", t)
        ztype.setCurrentIndex(max(0, ztype.findData(int(z.get('type', 0)))))
        src = QComboBox()
        for f in sorted({q.get('source_ipl', '') for q in zones} | {z.get('source_ipl', '')}):
            src.addItem(f)
        src.setCurrentText(z.get('source_ipl', ''))
        island = QSpinBox(); island.setRange(0, 9); island.setValue(int(z.get('island', 0)))
        island.setToolTip("SA: 1 LS, 2 SF, 3 LV")
        text_key = QLineEdit(z.get('text_key', '') or ''); text_key.setMaxLength(7)
        text_key.setToolTip("SA only: GXT key shown on entry")
        form.addRow("Name", name)
        form.addRow("Type", ztype)
        form.addRow("File", src)
        form.addRow("Island", island)
        form.addRow("Text key", text_key)
        spins = {}
        for key in _BOUNDS:
            s = QDoubleSpinBox(); s.setRange(-10000.0, 10000.0); s.setDecimals(3)
            s.setValue(float(z.get(key, 0.0)))
            spins[key] = s
            form.addRow(key.replace('_', ' ').title(), s)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                              QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return

        before = dict(z)
        after = dict(z)
        after.update({'name': name.text().strip() or before.get('name', ''),
                      'type': ztype.currentData(),
                      'source_ipl': src.currentText(),
                      'island': island.value(),
                      'text_key': text_key.text().strip()})
        for key in _BOUNDS:                      # keep min below max
            after[key] = spins[key].value()
        for a, b in (('min_x', 'max_x'), ('min_y', 'max_y'), ('min_z', 'max_z')):
            if after[a] > after[b]:
                after[a], after[b] = after[b], after[a]

        def _apply(data):
            z.clear(); z.update(data)
            self._refresh_zone_box_visualization()

        _apply(after)
        self._push_map_undo(lambda: _apply(before), lambda: _apply(after),
                            f"Edit zone '{after['name']}'")
        self._set_status(f"Zone '{after['name']}' changed - not yet saved to disk")

    def _on_zone_row_clicked(self, row, col): #vers 1
        """Select the clicked zone and centre the view on it."""
        item = self._zones_table.item(row, 0)
        if item is None:
            return
        loader = getattr(self, '_world_loader', None)
        zones = getattr(loader, 'zones', []) if loader else []
        index = item.data(Qt.ItemDataRole.UserRole)
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not (0 <= index < len(zones)):
            return
        owners = getattr(vp, '_zone_box_owners', [])
        box = next((i for i, o in enumerate(owners) if o is zones[index]), None)
        if box is None:                           # zone's IPL hidden or boxes off
            self._set_status(f"Zone '{zones[index].get('name', '')}' not shown in view")
            return
        vp._selected_box = ('zone', box)
        if hasattr(self, '_on_cull_or_zone_box_picked'):
            self._on_cull_or_zone_box_picked('zone', box)

    def _refresh_zones_table(self): #vers 1
        """Fill the Zones table from loader.zones with current filters."""
        table = getattr(self, '_zones_table', None)
        if table is None:
            return
        loader = getattr(self, '_world_loader', None)
        zones = getattr(loader, 'zones', []) if loader else []

        ff = self._zone_file_filter
        want_file = ff.currentData()
        files = sorted({z.get('source_ipl', '') for z in zones})
        if [ff.itemData(i) for i in range(1, ff.count())] != files:
            ff.blockSignals(True)
            ff.clear(); ff.addItem("All files", None)
            for f in files:
                ff.addItem(f, f)
            pend = getattr(self, '_zone_pending_file', None)
            if pend:
                want_file = next((f for f in files if f.lower() == pend.lower()), want_file)
                if want_file and want_file.lower() == pend.lower():
                    self._zone_pending_file = None
            i = ff.findData(want_file)
            ff.setCurrentIndex(i if i > 0 else 0)
            ff.blockSignals(False)
            want_file = ff.currentData()
        want_type = self._zone_type_filter.currentData()
        text = self._zone_search.text().strip().lower()

        table.setSortingEnabled(False)
        table.setRowCount(0)
        for idx, z in enumerate(zones):
            if want_file and z.get('source_ipl', '').lower() != want_file.lower():
                continue
            if want_type not in (None, -1) and int(z.get('type', 0)) != want_type:
                continue
            if text and text not in z.get('name', '').lower() \
                    and text not in (z.get('text_key') or '').lower():
                continue
            r = table.rowCount()
            table.insertRow(r)
            t = int(z.get('type', 0))
            vals = [z.get('name', ''), f"{t} {ZONE_TYPES.get(t, '')}", z.get('source_ipl', ''),
                    z.get('island', 0), z.get('text_key', '') or ''] + [z.get(k, 0.0) for k in _BOUNDS]
            for c, v in enumerate(vals):
                it = QTableWidgetItem()
                if isinstance(v, (int, float)) and c != 1:
                    it.setData(Qt.ItemDataRole.DisplayRole, round(v, 3) if isinstance(v, float) else v)
                else:
                    it.setText(str(v))
                if c == 0:
                    it.setData(Qt.ItemDataRole.UserRole, idx)
                table.setItem(r, c, it)
        table.setSortingEnabled(True)
        table.resizeColumnsToContents()

    def _selected_zone_index(self): #vers 1
        """Zone index from table selection, else viewport selection."""
        table = getattr(self, '_zones_table', None)
        if table is not None and table.currentRow() >= 0:
            item = table.item(table.currentRow(), 0)
            if item is not None:
                return item.data(Qt.ItemDataRole.UserRole)
        vp = getattr(self, 'preview_widget', None)
        sel = getattr(vp, '_selected_box', None) if vp else None
        owners = getattr(vp, '_zone_box_owners', []) if vp else []
        loader = getattr(self, '_world_loader', None)
        zones = getattr(loader, 'zones', []) if loader else []
        if sel and sel[0] == 'zone' and 0 <= sel[1] < len(owners):
            return next((i for i, q in enumerate(zones) if q is owners[sel[1]]), None)
        return None

    def _show_zones_dock(self, file_name=None): #vers 1
        """Raise Zones dock, turn zone boxes on, filter to file_name."""
        dock = getattr(self, '_zones_dock', None)
        if dock is None:
            return
        dock.show(); dock.raise_()
        btn = getattr(self, '_show_zone_chk', None)
        if btn is not None and not btn.isChecked():
            btn.set_shown(True, emit=True)
        self._refresh_zones_table()
        if file_name:
            ff = self._zone_file_filter
            for i in range(ff.count()):
                if (ff.itemData(i) or '').lower() == file_name.lower():
                    ff.setCurrentIndex(i)
                    break
            else:
                self._zone_pending_file = file_name
