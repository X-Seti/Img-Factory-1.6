#this belongs in apps/components/Retro_GTA_Workshop/depends/retro_mapdata_func.py - Version: 4
# X-Seti - October 24 2026 - IMG Factory 1.6 - Retro GTA Workshop map data tab

"""
Data tab: the open map's zones, lights (GTA2), map objects (GTA2) and the
picked mission's objects (move them) as editable tables - add at the selected block, delete, apply to the map;
lights / objects drawn on the map. File ribbon: New blank map, Save as.
"""

##Methods list -
#
##class RetroMapDataMixin: -
# _create_data_tab
# _data_add
# _data_apply
# _data_delete
# _data_export_csv
# _data_import_csv
# _data_item_moved
# _data_item_picked
# _data_fill
# _data_overlay
# _data_rows
# _map_blank
# _save_map_as

import os

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (QComboBox, QFileDialog, QHBoxLayout, QHeaderView, QLabel, QMessageBox, QPushButton,
                             QTableWidget, QTableWidgetItem, QVBoxLayout, QWidget)

from apps.methods.retro_mapdata import blank_blocks, lights, objects, set_lights, set_objects, set_zones

COLUMNS = {'Zones': ('type', 'x', 'y', 'w', 'h', 'name'),
           'Lights': ('x', 'y', 'z', 'radius', 'r', 'g', 'b', 'intensity', 'shape', 'on', 'off'),
           'Objects': ('x', 'y', 'rotation', 'type'),
           'Mission objects': ('label', 'x', 'y', 'z', 'w', 'h', 'args')}
FLOATS = {'Lights': ('x', 'y', 'z', 'radius'), 'Objects': ('x', 'y'),
          'Mission objects': ('x', 'y', 'z', 'w', 'h')}
TEXT = ('name', 'label', 'args')


class RetroMapDataMixin: #vers 1
    """Zones / lights / objects editing of the Retro GTA Workshop."""

    def _create_data_tab(self) -> QWidget: #vers 3
        page = QWidget()
        lay = QVBoxLayout(page)
        top = QHBoxLayout()
        self._data_kind = QComboBox()
        self._data_kind.addItems(list(COLUMNS))
        self._data_kind.currentIndexChanged.connect(lambda _i: self._data_fill())
        self._data_info = QLabel("")
        top.addWidget(QLabel("Edit:"))
        top.addWidget(self._data_kind)
        top.addWidget(self._data_info, 1)
        lay.addLayout(top)
        self._data_table = QTableWidget(0, 0)
        self._data_table.verticalHeader().setVisible(False)
        lay.addWidget(self._data_table, 1)
        for row in ((("Add", "New row at the selected block", self._data_add),
                     ("Delete", "Remove the selected rows", self._data_delete),
                     ("Apply to map", "Write the table into the map (save the map to keep it)", self._data_apply)),
                    (("Import CSV", "Fill the table from a CSV (then Apply to map)", self._data_import_csv),
                     ("Export CSV", "Save the table as CSV", self._data_export_csv))):
            bot = QHBoxLayout()                          # two short rows: tab stays narrow
            for text, tip, fn in row:
                b = QPushButton(text)
                b.setToolTip(tip)
                b.clicked.connect(lambda _c=False, f=fn: f())
                bot.addWidget(b)
            bot.addStretch(1)
            lay.addLayout(bot)
        return page

    def _data_rows(self, kind: str) -> list: #vers 2
        m = self._map
        if kind == 'Mission objects':
            return self._mission_rows()
        if m is None:
            return []
        if kind == 'Zones':
            return [dict(z, type=z.get('sample', 0) if m.GAME.startswith('GTA1') else z['type']) for z in m.zones()]
        if not hasattr(m, 'set_chunk'):
            return []
        return lights(m) if kind == 'Lights' else objects(m)

    def _data_fill(self): #vers 2
        """Table of the picked kind from the map."""
        kind = self._data_kind.currentText()
        cols = COLUMNS[kind]
        rows = self._data_rows(kind)
        t = self._data_table
        t.setColumnCount(len(cols))
        t.setHorizontalHeaderLabels(['sample' if c == 'type' and kind == 'Zones' and self._map is not None
                                     and self._map.GAME.startswith('GTA1') else c for c in cols])
        t.horizontalHeader().setSectionResizeMode(len(cols) - 1, QHeaderView.ResizeMode.Stretch)
        t.setRowCount(len(rows))
        for r, row in enumerate(rows):
            for c, key in enumerate(cols):
                v = row.get(key, 0 if key not in TEXT else '')
                it = QTableWidgetItem(f"{v:.3f}" if isinstance(v, float) else str(v))
                if key == 'label':
                    it.setFlags(it.flags() & ~Qt.ItemFlag.ItemIsEditable)
                t.setItem(r, c, it)
        self._data_info.setText(f"{len(rows)} {kind.lower()}" if self._map is not None else "No map")
        self._data_overlay()

    def _data_overlay(self): #vers 2
        """Lights and map objects of the map drawn on the map view (src: Data tab kind, row)."""
        m, out = self._map, []
        if m is not None and hasattr(m, 'set_chunk'):
            for i, l in enumerate(lights(m)):
                out.append({'x': l['x'], 'y': l['y'], 'z': l['z'], 'w': l['radius'] * 2, 'h': l['radius'] * 2,
                            'label': 'LIGHT', 'kind': 'light', 'src': ('Lights', i),
                            'info': f"light ({l['x']:.2f}, {l['y']:.2f}) r {l['radius']:.2f}"})
            for i, o in enumerate(objects(m)):
                out.append({'x': o['x'], 'y': o['y'], 'z': 0, 'w': 0, 'h': 0, 'label': f"OBJ {o['type']}",
                            'kind': 'object', 'src': ('Objects', i),
                            'info': f"map object {o['type']} rot {o['rotation']}"})
        self._view.set_overlay(out)

    def _data_add(self): #vers 2
        kind = self._data_kind.currentText()
        if kind == 'Mission objects':
            self._set_status("Mission objects can be moved (edit x / y / z), not added")
            return
        x, y = self._view.selected or (128, 128)
        base = {'Zones': {'type': 0, 'x': x, 'y': y, 'w': 1, 'h': 1, 'name': 'new_zone'},
                'Lights': {'x': x + 0.5, 'y': y + 0.5, 'z': 2.0, 'radius': 2.0, 'r': 255, 'g': 255, 'b': 255,
                           'intensity': 128, 'shape': 0, 'on': 0, 'off': 0},
                'Objects': {'x': x + 0.5, 'y': y + 0.5, 'rotation': 0, 'type': 0}}[kind]
        t = self._data_table
        r = t.rowCount()
        t.setRowCount(r + 1)
        for c, key in enumerate(COLUMNS[kind]):
            v = base[key]
            t.setItem(r, c, QTableWidgetItem(f"{v:.3f}" if isinstance(v, float) else str(v)))
        t.scrollToBottom()

    def _data_item_picked(self, kind: str, row: int): #vers 1
        """Marker / zone clicked on the map: its row picked in the Data tab."""
        if self._data_kind.currentText() != kind:
            self._data_kind.setCurrentText(kind)
        if 0 <= row < self._data_table.rowCount():
            self._data_table.selectRow(row)
            self._data_table.scrollToItem(self._data_table.item(row, 0))
        self._set_status(f"{kind} {row} picked - drag to move, edit in the Data tab")

    def _data_item_moved(self, kind: str, row: int, dx: float, dy: float): #vers 1
        """Marker / zone dragged on the map: x / y of its row moved, then applied."""
        self._data_item_picked(kind, row)
        cols, t = COLUMNS[kind], self._data_table
        for key, d in (('x', dx), ('y', dy)):
            c = cols.index(key)
            it = t.item(row, c)
            if it is None:
                return
            v = float(it.text()) + d
            if key in FLOATS.get(kind, ()):
                it.setText(f"{max(0.0, min(255.99, v)):.3f}")
            else:
                it.setText(str(max(0, min(255, round(v)))))
        self._data_apply()
        if 0 <= row < t.rowCount():
            t.selectRow(row)

    def _data_export_csv(self): #vers 1
        import csv
        kind, t = self._data_kind.currentText(), self._data_table
        path, _ = QFileDialog.getSaveFileName(self, f"Export {kind}", f"{kind.lower().replace(' ', '_')}.csv",
                                              "CSV files (*.csv)")
        if not path:
            return
        with open(path, 'w', newline='', encoding='utf-8') as fh:
            w = csv.writer(fh)
            w.writerow(COLUMNS[kind])
            for r in range(t.rowCount()):
                w.writerow([t.item(r, c).text() if t.item(r, c) else '' for c in range(t.columnCount())])
        self._set_status(f"Exported {t.rowCount()} {kind.lower()} to {path}")

    def _data_import_csv(self): #vers 1
        """Table rows from a CSV with this kind's column names (Apply to map writes them)."""
        import csv
        kind, t = self._data_kind.currentText(), self._data_table
        path, _ = QFileDialog.getOpenFileName(self, f"Import {kind}", '', "CSV files (*.csv)")
        if not path:
            return
        cols = COLUMNS[kind]
        with open(path, newline='', encoding='utf-8') as fh:
            rows = list(csv.DictReader(fh))
        if rows and not set(cols) <= set(rows[0]):
            QMessageBox.warning(self, self.App_name, f"{kind} CSV needs columns: {', '.join(cols)}")
            return
        if kind == 'Mission objects' and len(rows) != t.rowCount():
            QMessageBox.warning(self, self.App_name, "Mission objects CSV must have the same rows (move only)")
            return
        t.setRowCount(len(rows))
        for r, row in enumerate(rows):
            for c, key in enumerate(cols):
                t.setItem(r, c, QTableWidgetItem(row[key]))
        self._set_status(f"{len(rows)} {kind.lower()} read from {path} - Apply to map to write them")

    def _data_delete(self): #vers 1
        for r in sorted({i.row() for i in self._data_table.selectedIndexes()}, reverse=True):
            self._data_table.removeRow(r)

    def _data_apply(self): #vers 2
        """Table values written into the map."""
        kind = self._data_kind.currentText()
        if self._map is None and kind != 'Mission objects':
            return
        cols, t, rows = COLUMNS[kind], self._data_table, []
        try:
            for r in range(t.rowCount()):
                row = {}
                for c, key in enumerate(cols):
                    text = t.item(r, c).text() if t.item(r, c) else '0'
                    row[key] = text if key in TEXT else (float(text) if key in FLOATS.get(kind, ()) else int(text))
                if kind == 'Zones' and self._map is not None and self._map.GAME.startswith('GTA1'):
                    row['sample'] = row['type']
                rows.append(row)
            if kind == 'Mission objects':
                self._mission_apply(rows)
                self._set_status(f"{len(rows)} mission objects updated - Save missions (Files dock) to keep them")
                return
            if kind == 'Zones':
                set_zones(self._map, rows)
            elif kind == 'Lights':
                set_lights(self._map, rows)
            else:
                set_objects(self._map, rows)
        except (ValueError, KeyError, OverflowError) as ex:
            QMessageBox.warning(self, self.App_name, f"{kind}: {ex}")
            return
        self._map_dirty = True
        self._update_title()
        self._data_fill()
        self._view.update()
        self._set_status(f"{len(rows)} {kind.lower()} written into the map - save the map to keep them")

    def _map_blank(self): #vers 1
        """Every block replaced by flat ground (zones, routes, other data kept); then Save as."""
        if self._map is None:
            self._set_status("Open a map first: its format, zones and other data are kept")
            return
        if QMessageBox.question(self, self.App_name, "Clear every block to flat ground (level 1, lid tile 1)?\n"
                                "Use Save map as afterwards to keep the original.") != QMessageBox.StandardButton.Yes:
            return
        blank_blocks(self._map)
        self._map_dirty = True
        self._block_map_opened()
        self._view.set_data(self._map, self._style)
        self._update_title()
        self._set_status("Blank map - paint lids / edit blocks, then Save map as")

    def _save_map_as(self): #vers 1
        """Write the map to a new file and keep working on that file."""
        from apps.methods.file_backup import safe_write_bytes
        if self._map is None:
            return
        ext = os.path.splitext(self._map.path)[1]
        path, _ = QFileDialog.getSaveFileName(self, "Save map as", self._map.path, f"Map (*{ext})")
        if not path:
            return
        try:
            safe_write_bytes(path, self._map.to_bytes(), f"{self.App_name}: {os.path.basename(path)}")
        except Exception as ex:
            QMessageBox.critical(self, "Save map as", str(ex))
            return
        self._map.path = path
        if hasattr(self._map, 'arch'):
            self._map.arch.path = path
            if self._style is not None and getattr(self._style, 'arch', None) is self._map.arch:
                self._style.path = path
                self._style_dirty = False
        self._map_dirty = False
        self._update_title()
        self._set_status(f"Saved {path}")
