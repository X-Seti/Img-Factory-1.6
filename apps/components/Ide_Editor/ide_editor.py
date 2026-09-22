#!/usr/bin/env python3
#this belongs in apps/components/Ide_Editor/ide_editor.py - Version: 2
from apps.app_info import App_name, App_build, App_auth
# X-Seti - September 2026 - IMG Factory 1.6 - IDE Editor

"""IDE Editor - every section of an IDE file (objs/tobj/anim/cars/peds/weap/
hier/2dfx/path/...) as an editable table. The file model (apps/methods/
ide_file.py) keeps the original text, so only edited rows change on save and
an untouched file saves byte-identical. Renaming a model or changing an ID
also updates the same file's 2dfx/path rows, and offers to update the IPL
placements that use it when saving. Every save backs the file up first."""

##Methods list -
# IDEPanel
# IDEEditor
# IDEHelpWidget
# open_ide_editor
# parse_ide_line
# validate_ide_entry
# create_ide_help_guide

import copy
import os
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional

_root = Path(__file__).resolve().parents[3]
if str(_root) not in sys.path:
    sys.path.insert(0, str(_root))

from PyQt6.QtCore import Qt, pyqtSignal
from PyQt6.QtGui import QColor, QFont
from PyQt6.QtWidgets import (
    QAbstractItemView, QApplication, QComboBox, QDialog, QDialogButtonBox, QFileDialog,
    QFormLayout, QHBoxLayout, QLabel, QLineEdit, QListWidget, QListWidgetItem, QMainWindow,
    QMessageBox, QPlainTextEdit, QScrollArea, QSpinBox, QSplitter, QTabWidget, QTableWidget,
    QTableWidgetItem, QTextEdit, QVBoxLayout, QWidget)

from apps.methods.ide_file import IDEFile, IDERow, ID_SECTIONS, NAMED_SECTIONS, column_headers
from apps.methods.ribbon_system import RibbonMixin


class IDEHelpWidget(QWidget): #vers 1
    """Built-in help guide for IDE Editor"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setup_help_ui()
    
    def setup_help_ui(self): #vers 1
        """Setup help interface"""
        layout = QVBoxLayout(self)
        
        # Help title
        title = QLabel("IDE Editor Help Guide")
        title.setFont(QFont("Arial", 14, QFont.Weight.Bold))
        layout.addWidget(title)
        
        # Scrollable help content
        scroll = QScrollArea()
        help_widget = QWidget()
        help_layout = QVBoxLayout(help_widget)
        
        # IDE Column explanations
        help_content = self.create_ide_help_content()
        help_text = QTextEdit()
        help_text.setHtml(help_content)
        help_text.setReadOnly(True)
        help_layout.addWidget(help_text)
        
        scroll.setWidget(help_widget)
        layout.addWidget(scroll)
    
    def create_ide_help_content(self) -> str: #vers 1
        """Create comprehensive IDE help content"""
        return """
        <h2>IDE (Item Definition) File Format</h2>
        <p>IDE files define object placement and properties in GTA games. Each line represents one object definition.</p>
        
        <h3>Table Columns:</h3>
        <table border="1" cellpadding="5">
        <tr><th>Column</th><th>Description</th><th>Values</th></tr>
        <tr><td><b>ID</b></td><td>Unique object identifier</td><td>0-65535 (integer)</td></tr>
        <tr><td><b>ModelName</b></td><td>DFF model filename (without .dff)</td><td>Text, max 24 chars</td></tr>
        <tr><td><b>TxdName</b></td><td>Texture dictionary name (without .txd)</td><td>Text, max 24 chars</td></tr>
        <tr><td><b>MeshCount</b></td><td>Number of meshes in model</td><td>1-10 (typical range)</td></tr>
        <tr><td><b>DrawDist</b></td><td>Draw distance in game units</td><td>50-1000+ (float)</td></tr>
        <tr><td><b>Flags</b></td><td>Object behavior flags (hex)</td><td>0x0 to 0xFFFFFFFF</td></tr>
        </table>
        
        <h3>Common Flag Values:</h3>
        <ul>
        <li><b>0x0</b> - Standard object, no special properties</li>
        <li><b>0x1</b> - Collision enabled</li>
        <li><b>0x2</b> - LOD (Level of Detail) model</li>
        <li><b>0x4</b> - Alpha transparency</li>
        <li><b>0x8</b> - Breakable object</li>
        <li><b>0x10</b> - Animated object</li>
        <li><b>0x20</b> - Damaged version</li>
        </ul>
        
        <h3>Draw Distance Guidelines:</h3>
        <ul>
        <li><b>50-100</b> - Small objects (signs, props)</li>
        <li><b>100-300</b> - Medium objects (cars, furniture)</li>
        <li><b>300-500</b> - Large objects (buildings, bridges)</li>
        <li><b>500+</b> - Massive objects (skyscrapers, landmarks)</li>
        </ul>
        
        <h3>Best Practices:</h3>
        <ul>
        <li>Keep ModelName and TxdName under 24 characters</li>
        <li>Use sequential ID numbers to avoid conflicts</li>
        <li>Match DrawDist to object importance and size</li>
        <li>Test flag combinations carefully</li>
        <li>Use LOD models for performance optimization</li>
        </ul>
        
        <h3>Editor Functions:</h3>
        <ul>
        <li><b>Sort by IDE</b> - Reorders COL/IMG lists to match IDE order</li>
        <li><b>Validate</b> - Checks for ID conflicts and invalid values</li>
        <li><b>Auto-Complete</b> - Suggests ModelName/TxdName from loaded IMG files</li>
        <li><b>Export</b> - Saves IDE file with proper formatting</li>
        <li><b>Import</b> - Loads existing IDE files for editing</li>
        </ul>
        
        <p><i>Reference: <a href="https://gtamods.com/wiki/Item_Definition">GTAMods IDE Documentation</a></i></p>
        """


class IDEPanel(QWidget): #vers 3
    """Structured IDE editor as an embeddable widget (used by the IPL Workshop's
    IDE tab and by the IDEEditor window). It has no ribbons of its own: the
    host calls its methods (open_ide_file, save_ide_file, _undo, ...)."""

    sort_by_ide_requested = pyqtSignal(list)     # model order, for the COL view
    selection_sync_requested = pyqtSignal(list)  # selected model names

    dirty_changed = pyqtSignal(bool)             # any IDE file has unsaved changes
    status_message = pyqtSignal(str)
    cascade_handler = None                       # host hook: (id_map, renames) -> status text
    usage_provider = None                        # host hook: () -> {model id: placements in loaded IPLs}

    def __init__(self, parent=None):
        super().__init__(parent)
        self.parent_window = parent
        self._files: List[IDEFile] = []
        self._fi = -1                    # current file index
        self._sec_name = ""              # current section name
        self._undo_stack, self._redo_stack = [], []
        self._ipl_paths: List[str] = []
        self._dat_path = None
        self._building = False
        self._setup_ui()

    # -- UI
    def _setup_ui(self):
        lay = QVBoxLayout(self)
        lay.setContentsMargins(0, 0, 0, 0)
        split = QSplitter(Qt.Orientation.Horizontal)
        left = QWidget()
        ll = QVBoxLayout(left)
        ll.setContentsMargins(4, 4, 4, 4)
        ll.addWidget(QLabel("IDE files"))
        self._file_list = QListWidget()
        self._file_list.currentRowChanged.connect(self._on_file_changed)
        ll.addWidget(self._file_list, 1)
        ll.addWidget(QLabel("Sections"))
        self._sec_list = QListWidget()
        self._sec_list.currentRowChanged.connect(self._on_section_changed)
        ll.addWidget(self._sec_list, 1)
        self._info = QLabel("No file loaded")
        self._info.setWordWrap(True)
        ll.addWidget(self._info)
        split.addWidget(left)

        tabs = QTabWidget()
        page = QWidget()
        pl = QVBoxLayout(page)
        pl.setContentsMargins(0, 2, 0, 0)
        self._search = QLineEdit()
        self._search.setPlaceholderText("Search any column...")
        self._search.setClearButtonEnabled(True)
        self._search.textChanged.connect(self._apply_filter)
        pl.addWidget(self._search)
        self._table = QTableWidget(0, 0)
        self._table.setAlternatingRowColors(True)
        self._table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self._table.setSortingEnabled(False)
        self._table.itemChanged.connect(self._on_cell_edited)
        self._table.itemSelectionChanged.connect(self._on_selection_changed)
        pl.addWidget(self._table, 1)
        tabs.addTab(page, "Table")
        tabs.addTab(IDEHelpWidget(), "Help")
        split.addWidget(tabs)
        split.setSizes([230, 970])

        self._status = QLabel("")
        self._status.setStyleSheet("padding: 2px 6px;")
        host = QWidget()
        hl = QVBoxLayout(host)
        hl.setContentsMargins(0, 0, 0, 0)
        hl.addWidget(split, 1)
        hl.addWidget(self._status)
        lay.addWidget(host, 1)

    def _say(self, msg: str):
        self._status.setText(msg)
        self.status_message.emit(msg)

    # -- state helpers
    def _file(self) -> Optional[IDEFile]:
        return self._files[self._fi] if 0 <= self._fi < len(self._files) else None

    def _sec(self):
        f = self._file()
        return f.section(self._sec_name) if f else None

    def _update_dirty(self):
        n = sum(1 for f in self._files if f.dirty)
        self.dirty_changed.emit(n > 0)
        for i, f in enumerate(self._files):
            it = self._file_list.item(i)
            if it:
                it.setText(("* " if f.dirty else "") + Path(f.path).name)
        self._say(f"{n} file(s) modified" if n else "No changes")

    # -- loading
    def load_ide_file(self, file_path: str) -> bool:
        """Load one IDE file (API used by IMG Factory / DAT Browser)."""
        return self._load_files([file_path], replace=True)

    def open_ide_file(self):
        paths, _ = QFileDialog.getOpenFileNames(
            self, "Open IDE file(s)", "", "IDE Files (*.ide *.IDE *.ifx);;All Files (*)")
        if paths:
            self._load_files(paths, replace=True)

    def _open_from_dat(self):
        dat, _ = QFileDialog.getOpenFileName(
            self, "Game .dat", "", "GTA DAT files (gta3.dat gta_vc.dat gta.dat gta_sol.dat gtasol.dat);;All files (*)")
        if not dat:
            return
        try:
            from apps.methods.asset_checker import find_game_asset_files
            from apps.methods.master_ide import collect_ipl_paths_from_dat
            _i, _c, ides, game = find_game_asset_files(dat)
        except Exception as e:
            QMessageBox.warning(self, App_name, f"Could not read {Path(dat).name}:\n{e}")
            return
        if not ides:
            QMessageBox.information(self, App_name, "No IDE files found from that .dat.")
            return
        self._dat_path = dat
        try:
            self._ipl_paths = list(collect_ipl_paths_from_dat(dat, game=game))
        except Exception:
            self._ipl_paths = []
        self._load_files(list(ides), replace=True)

    def _load_files(self, paths, replace=True) -> bool:
        files, failed = [], []
        for p in paths:
            f = IDEFile()
            try:
                f.load(p)
            except Exception as e:
                failed.append(f"{Path(p).name}: {e}")
                continue
            files.append(f)
        if failed:
            QMessageBox.warning(self, App_name, "Could not load:\n" + "\n".join(failed[:10]))
        if not files:
            return False
        self._files = files if replace else self._files + files
        self._undo_stack.clear(); self._redo_stack.clear()
        self._building = True
        self._file_list.clear()
        for f in self._files:
            self._file_list.addItem(Path(f.path).name)
        self._building = False
        self._file_list.setCurrentRow(0)
        self._say(f"Loaded {len(files)} IDE file(s)")
        return True

    def _on_file_changed(self, row):
        if self._building or row < 0:
            return
        self._fi = row
        self._sec_list.blockSignals(True)
        self._sec_list.clear()
        for s in self._files[row].sections:
            self._sec_list.addItem(f"{s.name}  ({len(s.rows)})")
        self._sec_list.blockSignals(False)
        pick = next((i for i, s in enumerate(self._files[row].sections) if s.rows), 0)
        self._sec_list.setCurrentRow(pick)

    def _on_section_changed(self, row):
        f = self._file()
        if not f or row < 0 or row >= len(f.sections):
            return
        self._sec_name = f.sections[row].name
        self._populate()

    def _refresh_section_counts(self):
        f = self._file()
        if not f:
            return
        self._sec_list.blockSignals(True)
        for i, s in enumerate(f.sections):
            it = self._sec_list.item(i)
            if it:
                it.setText(f"{s.name}  ({len(s.rows)})")
        self._sec_list.blockSignals(False)

    def _populate(self):
        sec = self._sec()
        self._building = True
        self._table.blockSignals(True)
        if sec is None:
            self._table.setRowCount(0)
            self._table.setColumnCount(0)
        else:
            n = max(sec.width(), 1)
            hint = min((len(r.fields) for r in sec.rows), default=0)
            self._table.setColumnCount(n)
            self._table.setHorizontalHeaderLabels(column_headers(sec.name, n, hint))
            self._table.setRowCount(len(sec.rows))
            for r, row in enumerate(sec.rows):
                for c in range(n):
                    it = QTableWidgetItem(row.fields[c] if c < len(row.fields) else "")
                    if c == 0:
                        it.setData(Qt.ItemDataRole.UserRole, r)
                    if row.changed:
                        it.setForeground(QColor("#e0a040"))
                    self._table.setItem(r, c, it)
                self._table.setRowHeight(r, 20)
            self._table.resizeColumnsToContents()
        self._table.blockSignals(False)
        self._building = False
        self._apply_filter(self._search.text())
        self._refresh_section_counts()
        self._update_dirty()
        f = self._file()
        self._info.setText(f"{Path(f.path).name}\n{self._sec_name}: {len(sec.rows) if sec else 0} rows" if f else "No file loaded")

    def _apply_filter(self, text):
        t = (text or "").lower()
        for r in range(self._table.rowCount()):
            hit = not t or any(t in (self._table.item(r, c).text().lower() if self._table.item(r, c) else "")
                               for c in range(self._table.columnCount()))
            self._table.setRowHidden(r, not hit)

    def _selected_rows(self) -> List[int]:
        out = set()
        for it in self._table.selectedItems():
            idc = self._table.item(it.row(), 0)
            if idc is not None and idc.data(Qt.ItemDataRole.UserRole) is not None:
                out.add(idc.data(Qt.ItemDataRole.UserRole))
        return sorted(out)

    def _on_selection_changed(self):
        sec = self._sec()
        if sec is None or sec.name not in NAMED_SECTIONS:
            return
        names = [sec.rows[i].fields[1] for i in self._selected_rows()
                 if i < len(sec.rows) and len(sec.rows[i].fields) > 1]
        self.selection_sync_requested.emit(names)

    # -- undo
    def _snapshot(self):
        f = self._file()
        return (f, [(s, copy.deepcopy(s.rows), s.reordered) for s in f.sections]) if f else None

    def _push_undo(self):
        snap = self._snapshot()
        if snap:
            self._undo_stack.append(snap)
            del self._undo_stack[:-40]
            self._redo_stack.clear()

    def _restore(self, snap):
        f, secs = snap
        for s, rows, ro in secs:
            s.rows, s.reordered = rows, ro
        if f is self._file():
            self._populate()

    def _undo(self):
        if not self._undo_stack:
            self._say("Nothing to undo")
            return
        self._redo_stack.append(self._snapshot())
        self._restore(self._undo_stack.pop())

    def _redo(self):
        if not self._redo_stack:
            self._say("Nothing to redo")
            return
        self._undo_stack.append(self._snapshot())
        self._restore(self._redo_stack.pop())

    # -- editing
    def _on_cell_edited(self, item):
        if self._building:
            return
        sec = self._sec()
        idc = self._table.item(item.row(), 0)
        idx = idc.data(Qt.ItemDataRole.UserRole) if idc else None
        if sec is None or idx is None or idx >= len(sec.rows):
            return
        row, col, val = sec.rows[idx], item.column(), item.text().strip()
        while len(row.fields) <= col:
            row.fields.append("")
        old = row.fields[col]
        if val == old:
            return
        if col == 0 and sec.name in ID_SECTIONS:
            try:
                int(val)
            except ValueError:
                self._table.blockSignals(True)
                item.setText(old)
                self._table.blockSignals(False)
                return
        self._push_undo()
        row.fields[col] = val
        self._cascade_in_file(sec, row, col, old, val)
        self._populate()

    def _cascade_in_file(self, sec, row, col, old, new):
        """Keep this file's own 2dfx / path rows in step with an edit of the
        ID (col 0) or model name (col 1) of a model row."""
        f = self._file()
        if sec.name not in NAMED_SECTIONS or f is None:
            return
        if col == 0:
            d = f.section("2dfx")
            for r in (d.rows if d else []):
                if r.fields and r.fields[0] == old:
                    r.fields[0] = new
            self._path_headers(f, lambda h: h.__setitem__(1, new) if h[1] == old else None)
        elif col == 1:
            oid = row.fields[0]
            self._path_headers(f, lambda h: h.__setitem__(2, new)
                               if h[1] == oid and h[2].lower() == old.lower() else None)

    @staticmethod
    def _path_headers(f, fn):
        p = f.section("path")
        for r in (p.rows if p else []):
            if len(r.fields) >= 3 and r.fields[0].lower() in ("car", "ped"):
                fn(r.fields)

    def _used_ids(self) -> set:
        ids = set()
        for f in self._files:
            for s in f.sections:
                if s.name in NAMED_SECTIONS:
                    for r in s.rows:
                        try:
                            ids.add(int(r.fields[0]))
                        except (ValueError, IndexError):
                            pass
        return ids

    def _next_free(self, after: int = -1) -> int:
        used = self._used_ids()
        n = after + 1 if after >= 0 else (max(used) + 1 if used else 0)
        while n in used:
            n += 1
        return n

    def add_ide_entry(self):
        sec = self._sec()
        if sec is None:
            self._say("Open an IDE file first")
            return
        self._push_undo()
        sel = self._selected_rows()
        if sec.rows:
            base = sec.rows[sel[-1] if sel else -1].copy_as_new()
        else:
            base = IDERow(["0", "newmodel", "newtxd", "1", "100", "0"])
        if sec.name in ID_SECTIONS and base.fields:
            try:
                base.fields[0] = str(self._next_free(int(base.fields[0]) if sel else -1))
            except ValueError:
                pass
            if len(base.fields) > 1 and sec.name in NAMED_SECTIONS:
                base.fields[1] = "newmodel"
        at = (sel[-1] + 1) if sel else len(sec.rows)
        sec.rows.insert(at, base)
        self._populate()

    def _duplicate_rows(self):
        sec, sel = self._sec(), self._selected_rows()
        if sec is None or not sel:
            return
        self._push_undo()
        used = self._used_ids()
        for i in reversed(sel):
            c = sec.rows[i].copy_as_new()
            if sec.name in NAMED_SECTIONS:
                nid = self._next_free(max(used) if used else -1)
                used.add(nid)
                c.fields[0] = str(nid)
            sec.rows.insert(i + 1, c)
        self._populate()

    def delete_ide_entry(self):
        sec, sel = self._sec(), self._selected_rows()
        if sec is None or not sel:
            return
        ids = {sec.rows[i].fields[0] for i in sel} if sec.name in NAMED_SECTIONS else set()
        f = self._file()
        extra = 0
        if ids and f:
            d = f.section("2dfx")
            extra = sum(1 for r in (d.rows if d else []) if r.fields and r.fields[0] in ids)
        msg = f"Delete {len(sel)} row(s)?"
        if extra:
            msg += f"\nTheir {extra} 2dfx row(s) in this file will be removed too."
        if QMessageBox.question(self, "Delete", msg) != QMessageBox.StandardButton.Yes:
            return
        self._push_undo()
        dead = set(sel)
        sec.rows = [r for i, r in enumerate(sec.rows) if i not in dead]
        if extra:
            d.rows = [r for r in d.rows if not (r.fields and r.fields[0] in ids)]
        self._populate()

    # -- tools
    def _run_checks(self):
        if not self._files:
            self._say("Open an IDE file first")
            return
        out, problems = [], 0
        def sect(title, rows):
            nonlocal problems
            problems += len(rows)
            out.append(f"== {title}: {len(rows)}")
            out.extend(rows[:150])
            if len(rows) > 150:
                out.append(f"   ... {len(rows) - 150} more")
            out.append("")
        seen_id, seen_nm = {}, {}
        dup_id, dup_nm, empty, long_, neg = [], [], [], [], []
        for f in self._files:
            fn = Path(f.path).name
            for s in f.sections:
                if s.name not in NAMED_SECTIONS:
                    continue
                for r in s.rows:
                    if len(r.fields) < 3:
                        continue
                    key = r.fields[0]
                    nm = r.fields[1].lower()
                    if key in seen_id:
                        dup_id.append(f"   id {key}: {r.fields[1]} ({fn}/{s.name}) and {seen_id[key]}")
                    else:
                        seen_id[key] = f"{r.fields[1]} ({fn}/{s.name})"
                    if nm in seen_nm:
                        dup_nm.append(f"   '{r.fields[1]}': id {key} ({fn}) and id {seen_nm[nm]}")
                    else:
                        seen_nm[nm] = key
                    if s.name in ("objs", "tobj", "anim") and not r.fields[2]:
                        empty.append(f"   {fn}: id {key} {r.fields[1]} has no TXD")
                    if len(r.fields[1]) > 24 or len(r.fields[2]) > 24:
                        long_.append(f"   {fn}: id {key} {r.fields[1]} / {r.fields[2]} longer than 24 characters")
                    if s.name in ("objs", "tobj", "anim"):
                        for v in r.fields[3:6]:
                            try:
                                if float(v) < 0:
                                    neg.append(f"   {fn}: id {key} {r.fields[1]} negative value {v}")
                                    break
                            except ValueError:
                                pass
        sect("Duplicate IDs", dup_id)
        sect("Duplicate model names", dup_nm)
        sect("Objects with no TXD name", empty)
        sect("Names longer than 24 characters", long_)
        sect("Negative draw distance", neg)
        from apps.methods.asset_integrity import show_integrity_dialog
        show_integrity_dialog(self, f"Problems found: {problems}\n\n" + "\n".join(out))

    def _show_free_ids(self):
        used = self._used_ids()
        if not used:
            self._say("No IDs loaded")
            return
        gaps, lo, hi = [], min(used), max(used)
        start = None
        for i in range(lo, hi + 2):
            if i not in used and i <= hi:
                start = i if start is None else start
            elif start is not None:
                gaps.append((start, i - 1))
                start = None
        lines = [f"IDs in use: {len(used)}   lowest {lo}   highest {hi}   next above highest {hi + 1}",
                 f"Free gaps inside that range: {len(gaps)}", ""]
        lines += [f"  {a}" if a == b else f"  {a} - {b}   ({b - a + 1})" for a, b in gaps[:200]]
        from apps.methods.asset_integrity import show_integrity_dialog
        show_integrity_dialog(self, "\n".join(lines))

    def _set_column(self):
        sec, sel = self._sec(), self._selected_rows()
        if sec is None or not sel:
            QMessageBox.information(self, "Set column", "Select the rows to change first.")
            return
        heads = [self._table.horizontalHeaderItem(c).text() for c in range(self._table.columnCount())]
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Set a column on {len(sel)} row(s)")
        fl = QFormLayout(dlg)
        col = QComboBox(); col.addItems(heads[1:] if len(heads) > 1 else heads)
        val = QLineEdit()
        fl.addRow("Column:", col); fl.addRow("New value:", val)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        fl.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        c = col.currentIndex() + (1 if len(heads) > 1 else 0)
        self._push_undo()
        for i in sel:
            r = sec.rows[i]
            while len(r.fields) <= c:
                r.fields.append("")
            old = r.fields[c]
            r.fields[c] = val.text().strip()
            if c == 1:
                self._cascade_in_file(sec, r, 1, old, r.fields[c])
        self._populate()

    def _renumber(self):
        sec, sel = self._sec(), self._selected_rows()
        if sec is None or sec.name not in NAMED_SECTIONS or not sel:
            QMessageBox.information(self, "Renumber", "Select model rows (objs/tobj/anim/cars/...) first.")
            return
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Renumber {len(sel)} row(s)")
        fl = QFormLayout(dlg)
        sp = QSpinBox(); sp.setRange(0, 65535); sp.setValue(self._next_free(-1))
        fl.addRow("First new ID:", sp)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        fl.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        new_ids = list(range(sp.value(), sp.value() + len(sel)))
        mine = {id(sec.rows[i]) for i in sel}
        others = set()
        for f in self._files:
            for s in f.sections:
                if s.name in NAMED_SECTIONS:
                    for r in s.rows:
                        if id(r) not in mine:
                            try:
                                others.add(int(r.fields[0]))
                            except (ValueError, IndexError):
                                pass
        clash = sorted(set(new_ids) & others)
        if clash:
            QMessageBox.warning(self, "Renumber", f"Those IDs are already used: {clash[:10]}\nNothing changed.")
            return
        self._push_undo()
        for i, nid in zip(sel, new_ids):
            r = sec.rows[i]
            old = r.fields[0]
            r.fields[0] = str(nid)
            self._cascade_in_file(sec, r, 0, old, str(nid))
        self._populate()

    def _prefix_suffix(self):
        sec, sel = self._sec(), self._selected_rows()
        if sec is None or sec.name not in NAMED_SECTIONS or not sel:
            QMessageBox.information(self, "Rename", "Select model rows first.")
            return
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Rename {len(sel)} row(s)")
        fl = QFormLayout(dlg)
        pre, suf = QLineEdit(), QLineEdit()
        fl.addRow("Prefix:", pre); fl.addRow("Suffix:", suf)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        fl.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted or not (pre.text() or suf.text()):
            return
        self._push_undo()
        for i in sel:
            r = sec.rows[i]
            old = r.fields[1]
            r.fields[1] = pre.text() + old + suf.text()
            self._cascade_in_file(sec, r, 1, old, r.fields[1])
        self._populate()

    def _sort_menu(self):
        """Sort menu for the current section (same idea as the Map Workshop's Sort menu)."""
        from PyQt6.QtWidgets import QMenu
        from PyQt6.QtGui import QCursor
        m = QMenu(self)
        for label, key, rev in (
                ("ID (low to high)", "id", False), ("ID (high to low)", "id", True),
                ("Model name A-Z", "name", False), ("Model name Z-A", "name", True),
                ("TXD name", "txd", False), ("Draw distance (far first)", "dist", True),
                ("Most placed in the loaded IPLs first", "usage", True),
                ("Unused models first (0 placements)", "unused", False)):
            m.addAction(label, lambda k=key, r=rev, lb=label: self._sort_section(k, r, lb))
        m.exec(QCursor.pos())

    def _sort_section(self, key: str = "id", reverse: bool = False, label: str = ""):
        sec = self._sec()
        if sec is None or sec.name not in ID_SECTIONS or not sec.rows:
            self._say("Pick an IDE section with model rows first")
            return
        use = None
        if key in ("usage", "unused"):
            if self.usage_provider is None:
                QMessageBox.information(self, "Sort", "Placement counts come from the loaded IPL files "
                                        "(open the IPLs in the IPL Workshop first).")
                return
            use = self.usage_provider()

        def num(r, i, default=0.0):
            try:
                return float(r.fields[i])
            except (ValueError, IndexError):
                return default

        def rid(r):
            try:
                return int(r.fields[0])
            except (ValueError, IndexError):
                return 1 << 30

        keyf = {
            "id":     lambda r: rid(r),
            "name":   lambda r: (r.fields[1].lower() if len(r.fields) > 1 else "", rid(r)),
            "txd":    lambda r: (r.fields[2].lower() if len(r.fields) > 2 else "", rid(r)),
            "dist":   lambda r: (num(r, 4), -rid(r)),
            "usage":  lambda r: (use[rid(r)], -rid(r)),
            "unused": lambda r: (use[rid(r)] != 0, rid(r)),
        }[key]
        if QMessageBox.question(self, "Sort", f"Sort '{sec.name}' by {label or key}? "
                                "Comment lines stay where they are.") != QMessageBox.StandardButton.Yes:
            return
        self._push_undo()
        sec.rows.sort(key=keyf, reverse=reverse)
        sec.reordered = True
        self._populate()
        self._say(f"Sorted {sec.name} by {label or key}")

    def _merge_ide(self):
        f = self._file()
        if f is None:
            return
        p, _ = QFileDialog.getOpenFileName(self, "Merge rows from IDE file", "", "IDE Files (*.ide *.IDE *.ifx);;All Files (*)")
        if not p:
            return
        src = IDEFile()
        try:
            src.load(p)
        except Exception as e:
            QMessageBox.warning(self, App_name, str(e))
            return
        used, added, skipped = self._used_ids(), 0, 0
        plan = []
        for s in src.sections:
            tgt = f.section(s.name)
            if tgt is None:
                continue
            for r in s.rows:
                if s.name in NAMED_SECTIONS and r.fields and r.fields[0] in {str(i) for i in used}:
                    skipped += 1
                    continue
                plan.append((tgt, r.copy_as_new()))
        if not plan:
            QMessageBox.information(self, "Merge", f"Nothing to add ({skipped} row(s) skipped: ID already used).")
            return
        if QMessageBox.question(self, "Merge", f"Add {len(plan)} row(s) from {Path(p).name} "
                                f"({skipped} skipped: ID already used)?") != QMessageBox.StandardButton.Yes:
            return
        self._push_undo()
        for tgt, r in plan:
            tgt.rows.append(r)
        self._populate()

    def has_unsaved(self) -> bool:
        return any(f.dirty for f in self._files)

    def _choose_ipls(self):
        paths, _ = QFileDialog.getOpenFileNames(
            self, "IPL files to keep in step with ID / name changes", "", "IPL Files (*.ipl *.IPL);;All Files (*)")
        if paths:
            self._ipl_paths = list(paths)
            self._say(f"{len(paths)} IPL file(s) will be updated when you save")

    def _show_stats(self):
        f = self._file()
        if not f:
            return
        lines = [f"{Path(f.path).name}"] + [f"  {s.name}: {len(s.rows)}" for s in f.sections]
        ids = sorted(int(r.fields[0]) for s in f.sections if s.name in NAMED_SECTIONS
                     for r in s.rows if r.fields and r.fields[0].lstrip('-').isdigit())
        if ids:
            lines.append(f"\nID range: {ids[0]} - {ids[-1]}")
        QMessageBox.information(self, "IDE statistics", "\n".join(lines))

    def _export_csv(self):
        sec = self._sec()
        if sec is None:
            return
        p, _ = QFileDialog.getSaveFileName(self, "Export CSV", f"{sec.name}.csv", "CSV (*.csv)")
        if p:
            Path(p).write_text("\n".join(",".join(r.fields) for r in sec.rows), encoding="utf-8")
            self._say(f"Exported {len(sec.rows)} rows")

    # -- saving
    def _cascade_ops(self):
        """(id_map, renames) from the rows changed in every dirty file."""
        id_map, renames = {}, []
        for f in self._files:
            for s in f.sections:
                if s.name not in NAMED_SECTIONS:
                    continue
                for r in s.rows:
                    if r.orig is None or len(r.orig) < 2 or len(r.fields) < 2:
                        continue
                    try:
                        oid, nid = int(r.orig[0]), int(r.fields[0])
                    except ValueError:
                        continue
                    if oid != nid:
                        id_map[oid] = nid
                    if r.orig[1] != r.fields[1]:
                        renames.append((nid, r.orig[1], r.fields[1]))
        return id_map, renames

    def _write_ide(self, f: IDEFile, path: str) -> bool:
        from apps.methods.file_backup import backup_file, note_change
        try:
            if os.path.exists(path):
                note_change(f"Save IDE {Path(path).name}")
                if backup_file(path) is None:
                    QMessageBox.warning(self, "Save", "Backup failed - file not overwritten.")
                    return False
            f.save(path)
            return True
        except Exception as e:
            QMessageBox.critical(self, "Save Error", f"{Path(path).name}:\n{e}")
            return False

    def save_ide_file(self):
        """Back up + save every changed IDE file; then keep the chosen IPL
        files in step with the ID / name changes."""
        dirty = [f for f in self._files if f.dirty]
        if not dirty:
            self._say("Nothing to save")
            return
        id_map, renames = self._cascade_ops()
        saved = [Path(f.path).name for f in dirty if self._write_ide(f, f.path)]
        msg = "Saved " + ", ".join(saved)
        if (id_map or renames) and saved:
            handled = self.cascade_handler(id_map, renames) if self.cascade_handler else None
            msg += handled if handled is not None else self._cascade_to_disk(id_map, renames)
        self._populate()
        self._say(msg)

    def _cascade_to_disk(self, id_map, renames) -> str:
        """No IPL Workshop around: rewrite the chosen IPL files on disk so the
        placements follow the ID / name changes (each IPL is backed up first)."""
        ipls = self._ipl_paths
        if not ipls:
            r = QMessageBox.question(
                self, "IPL placements",
                f"{len(id_map)} ID change(s) and {len(renames)} rename(s) were saved.\n"
                "Update the IPL placements that use them too? (you pick the IPL files)")
            if r == QMessageBox.StandardButton.Yes:
                self._choose_ipls()
                ipls = self._ipl_paths
        if not ipls:
            return ""
        from apps.methods.id_reassign import cascade_ipl_files, cascade_ipl_rename
        touched = set()
        if id_map:
            touched |= {p for p, ok in cascade_ipl_files(ipls, id_map).items() if ok}
        for nid, old, new in renames:
            touched |= {p for p, ok in cascade_ipl_rename(ipls, nid, old, new).items() if ok}
        return f"  |  {len(touched)} IPL file(s) updated"

    def save_ide_file_as(self):
        f = self._file()
        if f is None:
            return
        p, _ = QFileDialog.getSaveFileName(self, "Save IDE file as", f.path, "IDE Files (*.ide);;All Files (*)")
        if p and self._write_ide(f, p):
            self._file_list.item(self._fi).setText(Path(p).name)
            self._populate()
            self._say(f"Saved as {Path(p).name}")

    # -- compat API used by other tools
    def new_ide_file(self):
        self._files, self._fi = [], -1
        self._file_list.clear(); self._sec_list.clear(); self._table.setRowCount(0)

    def sort_col_by_ide(self):
        sec = self._sec()
        if sec is not None:
            self.sort_by_ide_requested.emit([r.fields[1] for r in sec.rows if len(r.fields) > 1])

    def log_message(self, message: str):
        self._say(message)


class IDEEditor(RibbonMixin, QDialog): #vers 3
    """Standalone IDE Editor window: an IDEPanel plus its own ribbons. (The IPL
    Workshop embeds the same panel as its IDE tab; this window stays for callers
    that open an IDE on its own.)"""

    sort_by_ide_requested = pyqtSignal(list)
    selection_sync_requested = pyqtSignal(list)
    _ribbon_name = "ide_editor"
    # Bump when the set of ribbons changes (1 = File/Edit/View/Tools)
    _RIBBON_LAYOUT_VERSION = 1

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle(f"IDE Editor - {App_name}")
        self.resize(1200, 800)
        self.panel = IDEPanel(self)
        self.panel.sort_by_ide_requested.connect(self.sort_by_ide_requested)
        self.panel.selection_sync_requested.connect(self.selection_sync_requested)
        lay = QVBoxLayout(self)
        lay.setContentsMargins(0, 0, 0, 0)
        lay.addWidget(self.ribbon_wrap(self.panel), 1)
        self._build_ribbons()
        self.panel.dirty_changed.connect(lambda d: self.save_btn.setEnabled(d))
        self.ribbon_restore_state()

    def load_ide_file(self, file_path: str) -> bool:
        return self.panel.load_ide_file(file_path)

    def _build_ribbons(self):
        P, B = self.panel, self.ribbon_button
        tb = self.ribbon_toolbar("File")
        B(tb, "open_icon",   "Open IDE file(s)...", P.open_ide_file)
        B(tb, "import_icon", "Load all IDE files of a game from its .dat...", P._open_from_dat)
        self.save_btn = B(tb, "save_icon", "Save changed IDE file(s)  (Ctrl+S)", P.save_ide_file, enabled=False)
        B(tb, "saveas_icon", "Save As...", P.save_ide_file_as)
        B(tb, "export_icon", "Export section as CSV", P._export_csv)
        tb = self.ribbon_toolbar("Edit")
        B(tb, "undo_icon", "Undo  (Ctrl+Z)", P._undo)
        B(tb, "redo_icon", "Redo  (Ctrl+Y)", P._redo)
        tb.addSeparator()
        B(tb, "add_icon",   "Add row (next free ID)", P.add_ide_entry)
        B(tb, "trash_icon", "Delete selected rows", P.delete_ide_entry)
        B(tb, "edit_icon",  "Duplicate selected rows", P._duplicate_rows)
        tb.addSeparator()
        B(tb, "search_icon", "Find", lambda: P._search.setFocus())
        tb = self.ribbon_toolbar("View")
        B(tb, "fit_grid_icon", "Fit columns", lambda: P._table.resizeColumnsToContents())
        B(tb, "info_icon", "Statistics", P._show_stats)
        tb = self.ribbon_toolbar("Tools")
        B(tb, "check_icon",   "Check: duplicate IDs / names, empty TXD, long names", P._run_checks)
        B(tb, "search_icon",  "Free IDs", P._show_free_ids, text="Free")
        tb.addSeparator()
        B(tb, "edit_icon",    "Set one column on the selected rows...", P._set_column, text="Set")
        B(tb, "convert_icon", "Renumber the selected rows from an ID...", P._renumber, text="Renum")
        B(tb, "edit_icon",    "Add a prefix / suffix to the selected model names...", P._prefix_suffix, text="Name")
        B(tb, "convert_icon", "Sort this section (ID, name, TXD, draw distance...)", P._sort_menu, text="Sort")
        tb.addSeparator()
        B(tb, "package_icon", "Merge rows from another IDE file...", P._merge_ide, text="Merge")
        B(tb, "folder_icon",  "Choose the IPL files kept in step with ID/name changes...", P._choose_ipls, text="IPLs")

    def closeEvent(self, ev):
        if self.panel.has_unsaved():
            r = QMessageBox.question(
                self, App_name, "Save changed IDE files before closing?",
                QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard
                | QMessageBox.StandardButton.Cancel)
            if r == QMessageBox.StandardButton.Cancel:
                ev.ignore()
                return
            if r == QMessageBox.StandardButton.Save:
                self.panel.save_ide_file()
                if self.panel.has_unsaved():
                    ev.ignore()
                    return
        self.ribbon_save_state()
        super().closeEvent(ev)


# Utility functions kept for older callers

def parse_ide_line(line: str) -> Optional[Dict[str, Any]]: #vers 2
    """Parse an objs-style IDE line into a dict (best effort)."""
    parts = [p.strip() for p in line.split('#')[0].split(',')]
    try:
        if len(parts) >= 6:
            return {'id': int(parts[0]), 'model': parts[1], 'txd': parts[2],
                    'meshcount': int(parts[3]), 'drawdist': float(parts[4]), 'flags': parts[5]}
        if len(parts) == 5:
            return {'id': int(parts[0]), 'model': parts[1], 'txd': parts[2],
                    'meshcount': 1, 'drawdist': float(parts[3]), 'flags': parts[4]}
    except ValueError:
        pass
    return None


def validate_ide_entry(entry: Dict[str, Any]) -> List[str]: #vers 1
    errors = []
    if entry['id'] < 0 or entry['id'] > 65535:
        errors.append("ID must be between 0 and 65535")
    if len(entry['model']) > 24:
        errors.append("Model name must be 24 characters or less")
    if len(entry['txd']) > 24:
        errors.append("TXD name must be 24 characters or less")
    if entry['meshcount'] < 1:
        errors.append("Mesh count must be at least 1")
    if entry['drawdist'] < 0:
        errors.append("Draw distance cannot be negative")
    return errors


def create_ide_help_guide() -> str: #vers 1
    return ("IDE Editor Help Guide\n\nEdit Item Definition (IDE) files. Every section is a table; "
            "only the rows you change are rewritten. See the Help tab for the field reference.")


IDETableWidget = QTableWidget      # compat alias


def open_ide_editor(parent=None): #vers 3
    """Open the IDE editor. The IDE editor now lives in the IPL Workshop (IDE
    tab, since an IPL needs its IDE list first); this opens that workshop on the
    IDE tab and returns it (it has load_ide_file() like the old window). Falls
    back to the plain window when the workshop cannot be created."""
    try:
        from apps.components.Ipl_Editor.ipl_workshop import open_ipl_workshop
        ws = open_ipl_workshop(parent)
        if ws is not None:
            ws.show_ide_tab()
            return ws
    except Exception as e:
        print(f"[IDE Editor] IPL Workshop unavailable, using the plain window: {e}")
    editor = IDEEditor(parent)
    editor.show()
    return editor


__all__ = ['IDEEditor', 'IDEPanel', 'IDEHelpWidget', 'IDETableWidget', 'open_ide_editor',
           'parse_ide_line', 'validate_ide_entry', 'create_ide_help_guide']


if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = IDEEditor()
    w.show()
    for a in sys.argv[1:]:
        if os.path.isfile(a):
            w.load_ide_file(a)
    sys.exit(app.exec())
