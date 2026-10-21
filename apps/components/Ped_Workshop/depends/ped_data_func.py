#this belongs in apps/components/Ped_Workshop/depends/ped_data_func.py - Version: 7
# X-Seti - October 21 2026 - IMG Factory 1.6 - Ped Workshop ped data

"""
Peds dock for PedWorkshop: game folder, peds IDE table (edit / save),
ped data file editor (ped, pedgrp, pedstats, animgrp, clothes, shopping,
popcycle, Decision) and loading a ped's DFF + TXD.
"""

##class PedDataMixin: -
# _create_peds_dock
# _filter_ped_table
# _load_ped
# _open_ped_root
# _open_ped_file
# _ped_img_bytes
# _ped_selected
# _save_ped_ide
# _save_ped_text
# _show_ped_file

import os

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (
    QComboBox, QDockWidget, QFileDialog, QHBoxLayout, QLabel, QLineEdit, QMessageBox, QPlainTextEdit,
    QPushButton, QTableWidget, QTableWidgetItem, QTabWidget, QVBoxLayout, QWidget)

from apps.methods.ped_files import PED_COLUMNS, game_ped_paths, read_ped_ide, write_ped_ide


class PedDataMixin: #vers 1
    """Peds IDE, ped groups and ped types for PedWorkshop."""

    def _create_peds_dock(self): #vers 6
        """Game folder row, tabs: Peds table / Data files."""
        self._ped_root, self._ped_paths, self._ped_rows = '', {}, []
        self._ped_ide_dirty = False
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(3)
        tb = self._anim_tool_button
        self._ped_root_label = QLabel("No game folder")      # shown in the Peds title bar
        tabs = QTabWidget()
        self._ped_tabs = tabs
        peds = QWidget()
        pl = QVBoxLayout(peds)
        pl.setContentsMargins(0, 3, 0, 0)
        self._ped_search = QLineEdit()                        # goes in the Peds title bar
        self._ped_search.setPlaceholderText("Filter peds")
        self._ped_search.textChanged.connect(self._filter_ped_table)
        self._ped_table = QTableWidget(0, 0)
        self._ped_table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self._ped_table.setSelectionMode(QTableWidget.SelectionMode.SingleSelection)
        self._ped_table.itemSelectionChanged.connect(self._ped_selected)
        self._ped_table.itemChanged.connect(lambda _it: setattr(self, '_ped_ide_dirty', True))
        pl.addWidget(self._ped_table, 1)
        tabs.addTab(peds, "Peds")
        data = QWidget()
        dl = QVBoxLayout(data)
        dl.setContentsMargins(0, 3, 0, 0)
        row = QHBoxLayout()
        row.setSpacing(2)
        self._ped_file_combo = QComboBox()
        self._ped_file_combo.setToolTip("Ped data files found in this game folder")
        self._ped_file_combo.currentIndexChanged.connect(self._show_ped_file)
        self._ped_file_shown = None
        row.addWidget(self._ped_file_combo, 1)
        row.addWidget(tb('save_icon', "Save this file (backup first)", lambda: self._save_ped_text()))
        dl.addLayout(row)
        self._ped_file_edit = QPlainTextEdit()
        self._ped_file_edit.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        dl.addWidget(self._ped_file_edit, 1)
        tabs.addTab(data, "Data files")
        lay.addWidget(tabs, 1)
        dock = QDockWidget("Peds", self)
        dock.setObjectName("Peds")
        dock.setWidget(panel)
        self._peds_dock = dock
        return dock

    def _open_ped_root(self, root: str): #vers 4
        """Load peds IDE, pedgrp.dat, ped.dat and ped.ifp of a game folder."""
        if not root:
            root = QFileDialog.getExistingDirectory(self, "GTA game folder", self._ped_root or '')
            if not root:
                return
        paths = game_ped_paths(root)
        if not paths['ide']:
            QMessageBox.warning(self, "Ped Workshop", "No data/peds.ide or data/default.ide in that folder.")
            return
        self._ped_root, self._ped_paths = root, paths
        if hasattr(self, '_save_ped_prefs'):
            recent = [root] + [r for r in self._load_ped_prefs().get('recent_roots', []) if r != root]
            self._save_ped_prefs(recent_roots=recent[:8])
        self._ped_root_label.setText(f"{os.path.basename(os.path.normpath(root))}  ({paths['game'].upper()})")
        _lines, self._ped_rows = read_ped_ide(paths['ide'])
        cols = PED_COLUMNS[paths['game']]
        width = max([len(cols)] + [len(f) for _i, f, _c in self._ped_rows])
        t = self._ped_table
        t.blockSignals(True)
        t.clear()
        t.setColumnCount(width)
        t.setHorizontalHeaderLabels(cols + [f"Extra {i}" for i in range(width - len(cols))])
        t.setRowCount(len(self._ped_rows))
        for r, (_i, fields, _c) in enumerate(self._ped_rows):
            for c, v in enumerate(fields):
                t.setItem(r, c, QTableWidgetItem(v))
        t.resizeColumnsToContents()
        t.blockSignals(False)
        self._ped_ide_dirty = False
        self._ped_file_combo.blockSignals(True)
        self._ped_file_combo.clear()
        for label, p in paths['data_files']:
            self._ped_file_combo.addItem(label, p)
        self._ped_file_combo.blockSignals(False)
        self._show_ped_file(0)
        if hasattr(self, '_fill_ifp_sources'):
            self._fill_ifp_sources(paths)

    def _open_ped_file(self, path: str): #vers 1
        """Show a data file (or IFP) from this game folder."""
        if path.lower().endswith('.ifp') and hasattr(self, '_anim_open_ifp'):
            self._anim_open_ifp(path)
            return
        want = os.path.normcase(os.path.abspath(path))
        for i in range(self._ped_file_combo.count()):
            if os.path.normcase(os.path.abspath(self._ped_file_combo.itemData(i))) == want:
                self._ped_file_combo.setCurrentIndex(i)
                self._ped_tabs.setCurrentIndex(1)
                return

    def _filter_ped_table(self, text: str): #vers 1
        t, q = self._ped_table, text.lower()
        for r in range(t.rowCount()):
            hit = not q or any(q in (t.item(r, c).text().lower() if t.item(r, c) else '')
                               for c in range(min(3, t.columnCount())))
            t.setRowHidden(r, not hit)

    def _ped_selected(self): #vers 2
        rows = self._ped_table.selectionModel().selectedRows()
        if rows:
            r = rows[0].row()
            if self._ped_paths.get('game') == 'iv':
                self._set_status("Ped: GTA IV ped models (componentpeds) not previewed yet")
                return
            model = self._ped_table.item(r, 1).text() if self._ped_table.item(r, 1) else ''
            txd = self._ped_table.item(r, 2).text() if self._ped_table.item(r, 2) else model
            if model and model.lower() != 'null':
                self._load_ped(model, txd)

    def _ped_img_bytes(self, name: str): #vers 1
        """Bytes of an entry in the game's models/gta3.img, or None."""
        img_path = self._ped_paths.get('img')
        if not img_path:
            return None
        arc = getattr(self, '_ped_img', None)
        if arc is None or arc.file_path != img_path:
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(img_path)
            if not arc.open():
                return None
            self._ped_img = arc
        e = next((x for x in arc.entries if x.name.lower() == name.lower()), None)
        return arc.read_entry_data(e) if e is not None else None

    def _load_ped(self, model: str, txd: str): #vers 1
        """Show a ped DFF with its TXD and hand it to the animator."""
        import tempfile
        raw = self._ped_img_bytes(model + '.dff')
        if raw is None:
            self._set_status(f"Ped: {model}.dff not in models/gta3.img")
            return
        path = os.path.join(tempfile.mkdtemp(prefix='ped_'), model + '.dff')
        with open(path, 'wb') as f:
            f.write(raw)
        self._original_dff_name = model + '.dff'
        self.open_dff_file(path)
        tex = self._ped_img_bytes(txd + '.txd')
        if tex is not None:
            self._load_txd_file_from_data(tex, txd + '.txd')
        if hasattr(self, '_init_animator'):
            self._init_animator(raw)
        self._set_status(f"Ped: {model}")

    def _save_ped_ide(self): #vers 2
        """Write edited table cells back into the peds section."""
        if not self._ped_rows:
            return
        t = self._ped_table
        rows = []
        for r, (i, fields, comment) in enumerate(self._ped_rows):
            new = [t.item(r, c).text().strip() for c in range(len(fields)) if t.item(r, c)]
            rows.append((i, new, comment))
        try:
            n = write_ped_ide(self._ped_paths['ide'], rows)
        except Exception as e:
            QMessageBox.critical(self, "Save peds IDE", str(e))
            return
        self._ped_rows = rows
        self._ped_ide_dirty = False
        self._set_status(f"Peds IDE: {n} row(s) saved")

    def _show_ped_file(self, _index: int): #vers 2
        """Load the chosen ped data file; offers to save edits to the last one."""
        if self._ped_file_shown and self._ped_file_edit.document().isModified():
            name = os.path.basename(self._ped_file_shown)
            if QMessageBox.question(self, "Ped Workshop", f"Save changes to {name}?") == \
                    QMessageBox.StandardButton.Yes:
                self._save_ped_text(self._ped_file_shown)
        p = self._ped_file_combo.currentData()
        self._ped_file_edit.setPlainText(open(p, 'r', encoding='latin-1').read() if p else '')
        self._ped_file_edit.setReadOnly(not p)
        self._ped_file_edit.document().setModified(False)
        self._ped_file_shown = p

    def _save_ped_text(self, path: str = ''): #vers 3
        """Save the shown ped data file (backup first)."""
        from apps.methods.file_backup import safe_write_bytes
        p = path or self._ped_file_combo.currentData()
        if not p:
            return
        with open(p, 'rb') as f:
            nl = '\r\n' if b'\r\n' in f.read() else '\n'
        text = self._ped_file_edit.toPlainText().replace('\n', nl)
        safe_write_bytes(p, text.encode('latin-1'), f"Ped Workshop: {os.path.basename(p)}")
        self._ped_file_edit.document().setModified(False)
        self._set_status(f"Saved {os.path.basename(p)}")
