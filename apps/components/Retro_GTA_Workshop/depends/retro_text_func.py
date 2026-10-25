#this belongs in apps/components/Retro_GTA_Workshop/depends/retro_text_func.py - Version: 3
# X-Seti - October 24 2026 - IMG Factory 1.6 - Retro GTA Workshop text tab

"""
Text tab: GTA1 .fxt / GTA2 .gxt keys and strings, filter, edit, add,
delete, save (backup first).
"""

##Methods list -
#
##class RetroTextMixin: -
# _create_text_tab
# _text_add
# _text_delete
# _text_edited
# _text_export_csv
# _text_fill
# _text_filter
# _text_import_csv
# _text_list_folder
# _text_open
# _text_save

import os

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (QComboBox, QFileDialog, QHBoxLayout, QHeaderView, QInputDialog, QLabel,
                             QLineEdit, QMessageBox, QPushButton, QTableWidget, QTableWidgetItem,
                             QVBoxLayout, QWidget)

from apps.methods.retro_text import RetroText

TEXT_EXTS = ('.fxt', '.gxt')


class RetroTextMixin: #vers 1
    """Text tab of the Retro GTA Workshop."""

    def _create_text_tab(self) -> QWidget: #vers 3
        """Text file combo, filter, key / text table, add / delete / save."""
        self._text = None
        self._text_dirty = False
        page = QWidget()
        lay = QVBoxLayout(page)
        top = QHBoxLayout()
        self._text_combo = QComboBox()
        self._text_combo.setToolTip("Text files in the game folder")
        self._text_combo.activated.connect(lambda _i: self._text_open(self._text_combo.currentData()))
        opn = QPushButton("Open...")
        opn.clicked.connect(lambda _c=False: self._text_open(''))
        self._text_find = QLineEdit()
        self._text_find.setPlaceholderText("Filter keys / text")
        self._text_find.textChanged.connect(self._text_filter)
        top.addWidget(QLabel("Text:"))
        top.addWidget(self._text_combo, 1)
        top.addWidget(opn)
        top.addWidget(self._text_find, 2)
        lay.addLayout(top)
        self._text_table = QTableWidget(0, 2)
        self._text_table.setHorizontalHeaderLabels(["Key", "Text"])
        self._text_table.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.Stretch)
        self._text_table.verticalHeader().setVisible(False)
        self._text_table.itemChanged.connect(self._text_edited)
        lay.addWidget(self._text_table, 1)
        bot = QHBoxLayout()
        self._text_info = QLabel("No text file")
        add = QPushButton("Add key")
        add.clicked.connect(lambda _c=False: self._text_add())
        dele = QPushButton("Delete key")
        dele.clicked.connect(lambda _c=False: self._text_delete())
        save = QPushButton("Save text")
        save.clicked.connect(lambda _c=False: self._text_save())
        imp = QPushButton("Import CSV")
        imp.setToolTip("Update / add keys from a CSV (key, text)")
        imp.clicked.connect(lambda _c=False: self._text_import_csv())
        exp = QPushButton("Export CSV")
        exp.setToolTip("Save every key and text as CSV (UTF-8)")
        exp.clicked.connect(lambda _c=False: self._text_export_csv())
        lay.addWidget(self._text_info)
        for b in (add, dele, save):                      # two short rows: tab stays narrow
            bot.addWidget(b)
        lay.addLayout(bot)
        bot2 = QHBoxLayout()
        for b in (imp, exp):
            bot2.addWidget(b)
        lay.addLayout(bot2)
        return page

    def _text_list_folder(self, folder: str): #vers 1
        """Fill the text combo with the folder's .fxt / .gxt files."""
        self._text_combo.clear()
        for n in sorted(os.listdir(folder)):
            if n.lower().endswith(TEXT_EXTS):
                self._text_combo.addItem(n, os.path.join(folder, n))

    def _text_open(self, path: str): #vers 1
        if self._text_dirty and QMessageBox.question(
                self, self.App_name, "Discard unsaved text changes?") != QMessageBox.StandardButton.Yes:
            return
        if not path:
            path, _ = QFileDialog.getOpenFileName(self, "Open GTA1 / GTA2 text", self._data_dir or '',
                                                  "Game text (*.fxt *.gxt);;All files (*)")
            if not path:
                return
        try:
            with open(path, 'rb') as f:
                self._text = RetroText.from_bytes(f.read(), path)
        except (OSError, ValueError, UnicodeDecodeError) as ex:
            QMessageBox.warning(self, self.App_name, str(ex))
            return
        self._text_dirty = False
        self._text_fill()

    def _text_fill(self): #vers 1
        t = self._text
        tbl = self._text_table
        tbl.blockSignals(True)
        tbl.setRowCount(len(t.entries))
        for r, (k, txt) in enumerate(t.entries):
            ki = QTableWidgetItem(k)
            ki.setFlags(ki.flags() & ~Qt.ItemFlag.ItemIsEditable)
            tbl.setItem(r, 0, ki)
            tbl.setItem(r, 1, QTableWidgetItem(txt))
        tbl.blockSignals(False)
        tbl.resizeColumnToContents(0)
        self._text_filter(self._text_find.text())
        self._text_info.setText(f"{os.path.basename(t.path)}{' *' if self._text_dirty else ''}: "
                                f"{len(t.entries)} strings ({t.kind})")

    def _text_filter(self, needle: str): #vers 1
        n = needle.lower()
        tbl = self._text_table
        for r in range(tbl.rowCount()):
            hit = not n or any(n in (tbl.item(r, c).text().lower() if tbl.item(r, c) else '') for c in (0, 1))
            tbl.setRowHidden(r, not hit)

    def _text_edited(self, item): #vers 1
        if self._text is None or item.column() != 1:
            return
        self._text.set_text(item.row(), item.text())
        self._text_dirty = True
        self._text_info.setText(f"{os.path.basename(self._text.path)} *: {len(self._text.entries)} strings")

    def _text_add(self): #vers 1
        if self._text is None:
            return
        key, ok = QInputDialog.getText(self, "Add key", "New key (GTA2: 8 characters max):")
        key = key.strip()
        if not ok or not key:
            return
        if self._text.kind == 'gxt' and len(key) > 8:
            QMessageBox.warning(self, self.App_name, "GTA2 keys are 8 characters max")
            return
        if self._text.find(key) >= 0:
            QMessageBox.warning(self, self.App_name, f"Key {key} already exists")
            return
        self._text.entries.append([key, ''])
        self._text_dirty = True
        self._text_fill()
        self._text_table.scrollToBottom()

    def _text_delete(self): #vers 1
        r = self._text_table.currentRow()
        if self._text is None or r < 0:
            return
        del self._text.entries[r]
        offs = getattr(self._text, 'offs', None)
        if offs is not None and r < len(offs):
            del offs[r]
        self._text_dirty = True
        self._text_fill()

    def _text_export_csv(self): #vers 1
        import csv
        if self._text is None:
            return
        stem = os.path.splitext(os.path.basename(self._text.path))[0]
        path, _ = QFileDialog.getSaveFileName(self, "Export text", stem + '.csv', "CSV files (*.csv)")
        if not path:
            return
        with open(path, 'w', newline='', encoding='utf-8') as fh:
            w = csv.writer(fh)
            w.writerow(('key', 'text'))
            w.writerows(self._text.entries)
        self._set_status(f"Exported {len(self._text.entries)} strings to {path}")

    def _text_import_csv(self): #vers 1
        """Existing keys get the CSV text, new keys are added (GTA2: 8 characters max)."""
        import csv
        if self._text is None:
            return
        path, _ = QFileDialog.getOpenFileName(self, "Import text", '', "CSV files (*.csv)")
        if not path:
            return
        with open(path, newline='', encoding='utf-8') as fh:
            rows = [r for r in csv.reader(fh) if len(r) >= 2]
        if rows and rows[0][:2] == ['key', 'text']:
            rows = rows[1:]
        upd = add = skip = 0
        for key, txt in ((r[0].strip(), r[1]) for r in rows):
            i = self._text.find(key)
            if i >= 0:
                if self._text.entries[i][1] != txt:
                    self._text.set_text(i, txt)
                    upd += 1
            elif key and not (self._text.kind == 'gxt' and len(key) > 8):
                self._text.entries.append([key, txt])
                add += 1
            else:
                skip += 1
        if upd or add:
            self._text_dirty = True
        self._text_fill()
        self._set_status(f"Text CSV: {upd} updated, {add} added, {skip} skipped - Save text to keep")

    def _text_save(self): #vers 1
        """Write the text file (backup first)."""
        from apps.methods.file_backup import safe_write_bytes
        if self._text is None:
            return
        try:
            safe_write_bytes(self._text.path, self._text.to_bytes(),
                             f"{self.App_name}: {os.path.basename(self._text.path)}")
        except Exception as ex:
            QMessageBox.critical(self, "Save text", str(ex))
            return
        self._text_dirty = False
        self._text_fill()
        self._set_status(f"Saved {self._text.path}")
