#!/usr/bin/env python3
#this belongs in apps/components/Hex_Editor/hex_workshop.py - Version: 7
# X-Seti - September 2026 - IMG Factory 1.6 - Hex Workshop
"""
Hex Workshop - a working hex editor for any file, with the section-tree tools of Steve-M's
RW Analyze for RenderWare streams (.dff .txd .rws ...).

  * hex view painted on demand (big files open instantly), mouse/keyboard selection,
    overwrite / insert editing in the hex or ASCII pane, undo / redo, modified-byte highlight;
  * find / find all / replace (hex, ASCII, UTF-16, integers, floats), go to (absolute,
    relative, from end), bookmarks, insert / fill / delete bytes, import / export bytes;
  * data inspector (int8..64, float, GTA fixed point, RW version stamp) at the cursor;
  * structure tree: RenderWare sections (blue complex / green data / orange empty / red faulty),
    validate, export / import / copy / paste / clear / delete a section (parent sizes kept
    right), recompute sizes, change RW version, append a file, texture names; COL models,
    IMG v2 entries and .dir listings as well;
  * compare with another file (difference ranges highlighted), hashes (CRC32/MD5/SHA1/SHA256);
  * saves are safe: timestamped backup, atomic write; an IMG entry opened for editing is
    written back to its archive.

##Methods list -
# HexWorkshop.setup_ui / _build_ribbons / load_file / load_bytes / _save_file / _save_as
# HexWorkshop find / replace / goto / bookmarks / insert / fill / import / export / compare
# show_hex_editor_for_file
# show_hex_editor_for_entry
# open_hex_workshop
"""

import json
import os
import sys
from pathlib import Path
from typing import Optional

_root = Path(__file__).resolve().parents[3]
if str(_root) not in sys.path:
    sys.path.insert(0, str(_root))

from PyQt6.QtCore import Qt, QTimer
from PyQt6.QtGui import QColor, QFont, QKeySequence, QShortcut
from PyQt6.QtWidgets import (
    QApplication, QComboBox, QFileDialog, QFormLayout, QDialog, QDialogButtonBox,
    QInputDialog, QLabel, QLineEdit, QMessageBox, QSizePolicy, QSpinBox, QSplitter, QTabWidget, QVBoxLayout,
    QWidget)

from apps.methods.ribbon_system import RibbonMixin
from apps.components.Hex_Editor.depends.diffcode import GUIWorkshop
from apps.components.Hex_Editor.hex_canvas import HexDoc, HexCanvas
from apps.components.Hex_Editor.hex_panels import (
    InspectorPanel, StructurePanel, SearchPanel, BookmarksPanel, ComparePanel, ConvertDialog, hashes)

App_name   = "Hex Workshop"
App_build  = "Build 2"
config_key = "hex_workshop"

_BIG = 48 * 1024 * 1024            # above this the structure tree is not rebuilt on every edit


class HexWorkshop(RibbonMixin, GUIWorkshop):  #vers 4
    App_name   = App_name
    App_build  = App_build
    App_auth   = "X-Seti"
    config_key = config_key
    _ribbon_name = "hex_workshop"
    # Bump when the set of ribbons changes (2 = File/Edit/Search/View/Tools)
    _RIBBON_LAYOUT_VERSION = 2

    def __init__(self, parent=None, main_window=None): #vers 3
        self._defer_setup_ui = True
        self.doc = HexDoc()
        self._file_path: Optional[str] = None
        self._save_callback = None            # set when an IMG entry is being edited
        self._entry_label = ""
        self._loading = False
        self._struct_timer = None
        super().__init__(parent, main_window)
        if self.standalone_mode:
            self.setWindowIcon(self.icon_factory.get_hex_workshop_icon(64))
        self.setup_ui()
        self.setAcceptDrops(True)
        self._set_status("Open a file to begin (File ribbon, or drop a file here)")

    # ------------------------------------------------------------------ UI
    def _create_toolbar(self):
        tb = super()._create_toolbar()
        for name in ("open_btn", "save_btn", "export_btn", "import_btn"):
            b = getattr(self, name, None)
            if b:
                b.setVisible(False)
        return tb

    def setup_ui(self): #vers 3
        ml = QVBoxLayout(self)
        ml.setContentsMargins(*self.get_content_margins())
        ml.setSpacing(self.setspacing)
        ml.addWidget(self._create_toolbar())

        self.canvas = HexCanvas(self.doc)
        self.inspector = InspectorPanel()
        self.structure = StructurePanel()
        self.search = SearchPanel()
        self.bookmarks = BookmarksPanel()
        self.compare = ComparePanel()
        self.compare.get_data = lambda: self.doc.data

        left = QWidget()
        ll = QVBoxLayout(left)
        ll.setContentsMargins(2, 2, 2, 2)
        self._info_lbl = QLabel("No file loaded")
        self._info_lbl.setWordWrap(True)
        self._info_lbl.setFont(QFont("Monospace", 9))
        self._info_lbl.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        ll.addWidget(self._info_lbl)
        ll.addWidget(self.inspector, 1)

        centre = QWidget()
        cl = QVBoxLayout(centre)
        cl.setContentsMargins(0, 0, 0, 0)
        cl.setSpacing(2)
        cl.addWidget(self.canvas, 1)
        self._pos_lbl = QLabel("")
        self._pos_lbl.setStyleSheet("padding:2px 6px;")
        self._pos_lbl.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        cl.addWidget(self._pos_lbl)

        self._tabs = QTabWidget()
        self._tabs.addTab(self.structure, "Structure")
        self._tabs.addTab(self.search, "Search")
        self._tabs.addTab(self.bookmarks, "Bookmarks")
        self._tabs.addTab(self.compare, "Compare")

        sp = QSplitter(Qt.Orientation.Horizontal)
        sp.addWidget(left)
        sp.addWidget(centre)
        sp.addWidget(self._tabs)
        sp.setStretchFactor(1, 4)
        sp.setCollapsible(1, False)
        sp.setSizes(self._load_splitter() or [300, 700, 380])
        self._splitter = sp
        self._split_timer = QTimer(self)
        self._split_timer.setSingleShot(True)
        self._split_timer.timeout.connect(self._save_splitter)
        sp.splitterMoved.connect(lambda _p, _i: self._split_timer.start(500))
        ml.addWidget(self.ribbon_wrap(sp), 1)
        self._build_ribbons()
        self._status_widget = self._create_status_bar()
        ml.addWidget(self._status_widget)
        self._status_widget.setVisible(self.WS.get("show_statusbar", True))
        self.ribbon_restore_state()

        # wiring
        self.canvas.cursor_changed.connect(self._on_cursor)
        self.canvas.selection_changed.connect(lambda a, n: self._on_cursor(self.canvas.cur))
        self.canvas.status.connect(self._set_status)
        self.doc.changed.connect(self._on_doc_changed)
        self.structure.goto.connect(self._goto_range)
        self.structure.goto.connect(self._mark_region)
        self.structure.apply_bytes.connect(self._apply_whole)
        self.search.find_requested.connect(self._find)
        self.search.all_requested.connect(self._find_all)
        self.search.replace_requested.connect(self._replace)
        self.search.goto.connect(self._goto_range)
        self.bookmarks.goto.connect(self._goto_range)
        self.bookmarks.changed.connect(self._sync_marks)
        self.compare.goto.connect(self._goto_range)
        self.compare.diffs_changed.connect(self._set_diffs)
        for keys, fn in (("Ctrl+O", self._open_file), ("Ctrl+S", self._save_file), ("Ctrl+F", self._focus_search),
                         ("Ctrl+G", self._goto_dialog), ("F3", lambda: self.search._emit_find(True)),
                         ("Shift+F3", lambda: self.search._emit_find(False)), ("Ctrl+B", self._add_bookmark)):
            QShortcut(QKeySequence(keys), self, activated=fn)

    def _splitter_path(self) -> Path: #vers 1
        """Splitter sizes file beside the other workshop settings."""
        return Path.home() / ".config" / "imgfactory" / f"{config_key}_splitter.json"

    def _load_splitter(self): #vers 1
        try:
            s = json.loads(self._splitter_path().read_text()).get("sizes")
            return s if isinstance(s, list) and len(s) == 3 and sum(s) > 0 else None
        except Exception:
            return None

    def _save_splitter(self): #vers 1
        try:
            p = self._splitter_path()
            p.parent.mkdir(parents=True, exist_ok=True)
            p.write_text(json.dumps({"sizes": self._splitter.sizes()}))
        except Exception as ex:
            print(f"[hex_workshop] splitter save error: {ex}")

    def _build_ribbons(self): #vers 2
        B, C = self.ribbon_button, self.canvas
        tb = self.ribbon_toolbar("File")
        B(tb, "open_icon",   "Open a file  (Ctrl+O)", self._open_file)
        self.save_btn = B(tb, "save_icon", "Save - backs the old file up first  (Ctrl+S)", self._save_file, enabled=False)
        B(tb, "saveas_icon", "Save As...", self._save_as)
        B(tb, "get_refresh_icon", "Revert: reload the file and drop all edits", self._revert, text="Rev")
        tb.addSeparator()
        B(tb, "import_icon", "Insert / overwrite a file's bytes at the cursor...", self._import_bytes, text="Imp")
        B(tb, "export_icon", "Save the selected bytes to a new file...", self._export_selection, text="Exp")

        tb = self.ribbon_toolbar("Edit")
        B(tb, "undo_icon", "Undo  (Ctrl+Z)", lambda: self.doc.undo())
        B(tb, "redo_icon", "Redo  (Ctrl+Y)", lambda: self.doc.redo())
        tb.addSeparator()
        B(tb, "copy_icon", "Copy as hex  (Ctrl+C)", C.copy_hex)
        B(tb, "copy_icon", "Copy as text  (Ctrl+Shift+C)", C.copy_text, text="Txt")
        B(tb, "paste_icon", "Paste  (Ctrl+V) - hex text or plain text", C.paste)
        B(tb, "trash_icon", "Delete the selection (or the byte at the cursor)", C.delete_selection)
        tb.addSeparator()
        self._ins_btn = B(tb, "edit_icon", "Insert mode: typing adds bytes instead of overwriting  (Insert key)", self._toggle_insert,
                          checkable=True, text="Ins")
        B(tb, "add_icon", "Insert N bytes at the cursor...", self._insert_dialog, text="+N")
        B(tb, "paint_icon", "Fill the selection with a byte...", self._fill_dialog, text="Fill")
        B(tb, "locate_icon", "Select all  (Ctrl+A)", C.select_all, text="All")

        tb = self.ribbon_toolbar("Search")
        B(tb, "search_icon", "Find...  (Ctrl+F)", self._focus_search)
        B(tb, "locate_icon", "Go to offset...  (Ctrl+G)", self._goto_dialog, text="Go")
        B(tb, "add_icon", "Bookmark the cursor  (Ctrl+B)", self._add_bookmark, text="Mark")

        self._ribbon_mw.addToolBarBreak()
        tb = self.ribbon_toolbar("View")
        self.ribbon_label(tb, "Bytes/row")
        self._bpr = QComboBox()
        self._bpr.addItems(["8", "16", "24", "32", "48", "64"])
        self._bpr.setCurrentText("16")
        self._bpr.setMinimumHeight(28)
        self._bpr.currentTextChanged.connect(lambda t: self.canvas.set_bytes_per_row(int(t)))
        tb.addWidget(self._bpr)
        B(tb, "info_icon", "Show offsets in decimal / hex", self._toggle_offsets, text="Dec")
        B(tb, "zoom_in_icon", "Bigger text", lambda: self._font(+1))
        B(tb, "zoom_out_icon", "Smaller text", lambda: self._font(-1))

        tb = self.ribbon_toolbar("Tools")
        B(tb, "check_icon", "Hashes of the file / selection (CRC32, MD5, SHA1, SHA256)", self._show_hashes, text="Hash")
        self._rw_btns = [
            B(tb, "convert_icon", "RenderWare: recompute every section size", self.structure.recompute, text="Size"),
            B(tb, "convert_icon", "RenderWare: change the version of every section...", self.structure.change_version, text="Ver"),
            B(tb, "package_icon", "RenderWare: append another file's sections...", self.structure.append_file, text="App"),
            B(tb, "info_icon", "RenderWare: list texture names", self.structure.show_textures, text="Tex"),
            B(tb, "export_icon", "Export the section tree as a text file...", self._export_tree, text="Tree")]
        for b in self._rw_btns:
            b.setEnabled(False)
        tb.addSeparator()
        B(tb, "convert_icon", "Convert DFF / TXD / COL up or down between GTA III, Vice City and San Andreas (also batch)...",
          self._convert_dialog, text="Conv")

    # ------------------------------------------------------------------ state
    def _on_doc_changed(self):
        self.save_btn.setEnabled(self.doc.modified)
        self._update_title()
        if self._loading:
            return
        if self._struct_timer is None:
            self._struct_timer = QTimer(self)
            self._struct_timer.setSingleShot(True)
            self._struct_timer.timeout.connect(self._refresh_structure)
        if len(self.doc) <= _BIG:
            self._struct_timer.start(400)
        self.canvas.hits = []
        self._on_cursor(self.canvas.cur)

    def _refresh_structure(self): #vers 2
        self._build_structure()
        is_rw = self.structure.mode == "rw"
        for b in self._rw_btns:
            b.setEnabled(is_rw)

    def _build_structure(self): #vers 1
        """Rebuild the structure tree; big files get IMG directory only."""
        name = os.path.basename(self._file_path or self._entry_label or "")
        if len(self.doc) > _BIG:
            head = bytes(self.doc.data[:16])
            if head[:4] == b"VER2":                     # an IMG: only the directory table is needed
                import struct as _s
                n = _s.unpack_from("<I", head, 4)[0]
                self.structure.rebuild(bytes(self.doc.data[:8 + 32 * n]), name)
            else:
                self.structure.tree.clear()
                self.structure.mode = "none"
                self.structure.kind_lbl.setText("File too large for the structure tree (over 48 MB)")
            return
        self.structure.rebuild(bytes(self.doc.data), name)

    def _update_title(self):
        name = os.path.basename(self._file_path) if self._file_path else (self._entry_label or "untitled")
        self.setWindowTitle(f"Hex Workshop - {name}{' *' if self.doc.modified else ''}")

    def _on_cursor(self, off: int):
        a, n = self.canvas.selection()
        self._pos_lbl.setText(
            f"Offset 0x{off:08X} ({off})   |   size {len(self.doc):,}   |   "
            + (f"selection 0x{a:X} - 0x{a + n - 1:X} ({n} bytes)   |   " if n else "")
            + ("INSERT" if self.canvas.insert_mode else "OVERWRITE"))
        self.inspector.update_from(self.doc.data, off)

    # ------------------------------------------------------------------ files
    def load_bytes(self, data: bytes, label: str = "", path: Optional[str] = None):
        self._loading = True
        self.doc.undo_stack.clear()
        self.doc.redo_stack.clear()
        self.doc.data[:] = data
        self.doc.version = self.doc.saved_version = 0
        self.doc.mod = []
        self._loading = False
        self.doc.changed.emit()
        self._file_path = path
        self._entry_label = label
        self.canvas.cur, self.canvas.anchor = 0, None
        self.canvas.hits, self.canvas.regions, self.canvas.diffs = [], [], []
        self.compare.clear()
        self._info_lbl.setText(f"{label or os.path.basename(path or '')}\n{len(data):,} bytes\n{path or ''}")
        self._refresh_structure()
        self.canvas.goto(0)
        self._set_status(f"Loaded {label or os.path.basename(path or '')}  ({len(data):,} bytes)")
        self._update_title()

    def load_file(self, path: str):
        try:
            with open(path, "rb") as f:
                data = f.read()
            self._save_callback = None
            self.load_bytes(data, os.path.basename(path), path)
        except Exception as ex:
            QMessageBox.critical(self, "Error", str(ex))

    def _open_file(self, path: str = None):
        if not isinstance(path, str):
            path = None
        if not self._confirm_discard():
            return
        if path is None:
            path, _ = QFileDialog.getOpenFileName(self, "Open File", "", "All files (*)")
        if path:
            self.load_file(path)

    def _confirm_discard(self) -> bool:
        if not self.doc.modified:
            return True
        r = QMessageBox.question(self, "Hex Workshop", "Save your changes first?",
                                 QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard
                                 | QMessageBox.StandardButton.Cancel)
        if r == QMessageBox.StandardButton.Cancel:
            return False
        if r == QMessageBox.StandardButton.Save:
            self._save_file()
            return not self.doc.modified
        return True

    def _revert(self):
        if self._file_path and (not self.doc.modified or QMessageBox.question(
                self, "Revert", "Drop all edits and reload the file?") == QMessageBox.StandardButton.Yes):
            self.load_file(self._file_path)

    def _save_file(self):
        data = bytes(self.doc.data)
        try:
            if self._save_callback:
                self._save_callback(data)                       # IMG entry: written back to its archive
                self._set_status(f"Saved {self._entry_label} back to its archive")
            else:
                if not self._file_path:
                    self._save_as()
                    return
                from apps.methods.file_backup import safe_write_bytes
                safe_write_bytes(self._file_path, data)
                self._set_status(f"Saved {os.path.basename(self._file_path)}")
            self.doc.mark_saved()
            self._update_title()
        except Exception as ex:
            QMessageBox.critical(self, "Save Error", str(ex))

    def _save_as(self):
        path, _ = QFileDialog.getSaveFileName(self, "Save As", self._file_path or "", "All files (*)")
        if path:
            self._file_path = path
            self._save_callback = None
            self._entry_label = os.path.basename(path)
            self._save_file()

    def closeEvent(self, ev):
        if self.doc.modified:
            r = QMessageBox.question(self, "Hex Workshop", "Save changes before closing?",
                                     QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard
                                     | QMessageBox.StandardButton.Cancel)
            if r == QMessageBox.StandardButton.Cancel:
                ev.ignore()
                return
            if r == QMessageBox.StandardButton.Save:
                self._save_file()
                if self.doc.modified:
                    ev.ignore()
                    return
        self._save_splitter()
        self.ribbon_save_state()
        super().closeEvent(ev)

    def dragEnterEvent(self, e):
        if e.mimeData().hasUrls():
            e.acceptProposedAction()

    def dropEvent(self, e):
        urls = e.mimeData().urls()
        if urls and self._confirm_discard():
            self.load_file(urls[0].toLocalFile())

    # ------------------------------------------------------------------ navigation
    def _goto_range(self, off: int, length: int = 0):
        self.canvas.goto(off, length)
        self.canvas.setFocus()

    def _goto_dialog(self):
        txt, ok = QInputDialog.getText(self, "Go to offset",
                                       "Offset: 0x1A2B (hex) or 1234 (decimal)\n+0x10 / -16 relative to the cursor, end-0x20 from the end")
        if not ok or not txt.strip():
            return
        t = txt.strip().lower().replace(" ", "")
        try:
            if t.startswith("end-"):
                off = len(self.doc) - int(t[4:], 0)
            elif t[0] in "+-":
                off = self.canvas.cur + int(t, 0)
            else:
                off = int(t, 0) if t.startswith("0x") else (int(t, 16) if any(c in "abcdef" for c in t) else int(t, 10))
        except ValueError:
            QMessageBox.warning(self, "Go to", "Could not read that offset.")
            return
        self._goto_range(max(0, min(off, len(self.doc))))

    def _focus_search(self):
        self._tabs.setCurrentWidget(self.search)
        self.search.find.setFocus()
        a, n = self.canvas.selection()
        if n and n <= 32 and not self.search.find.text():
            self.search.kind.setCurrentText("Hex bytes")
            self.search.find.setText(" ".join(f"{b:02X}" for b in self.canvas.selected_bytes()))

    def _add_bookmark(self):
        txt, ok = QInputDialog.getText(self, "Bookmark", f"Label for 0x{self.canvas.cur:08X}:")
        if ok:
            self.bookmarks.add(self.canvas.cur, txt.strip() or "bookmark")

    def _sync_marks(self):
        self.canvas.marks = dict(self.bookmarks.marks)
        self.canvas.viewport().update()

    # ------------------------------------------------------------------ search
    def _find(self, pat: bytes, forward: bool):
        d = self.doc.data
        a, n = self.canvas.selection()
        if forward:
            i = d.find(pat, (a + max(1, n)) if n else a + 1)
            if i < 0:
                i = d.find(pat, 0)
        else:
            i = d.rfind(pat, 0, max(0, a + len(pat) - 1)) if a else -1
            if i < 0:
                i = d.rfind(pat)
        if i < 0:
            self.search.info.setText("Not found")
            return
        self.search.info.setText(f"Found at 0x{i:08X}")
        self._goto_range(i, len(pat))

    def _find_all(self, pat: bytes):
        from apps.components.Hex_Editor.hex_panels import find_all
        hits = find_all(self.doc.data, pat)
        self.search.show_hits(hits, len(pat))
        self.canvas.hits = [(h, h + len(pat)) for h in hits[:50000]]
        self.canvas.viewport().update()

    def _replace(self, pat: bytes, rep: bytes, all_: bool): #vers 2
        if all_:
            n = bytes(self.doc.data).count(pat)
            if not n:
                self.search.info.setText("Nothing to replace")
                return
            self.doc.replace_all(bytes(self.doc.data).replace(pat, rep))
            self.search.info.setText(f"Replaced {n} occurrence(s)")
            return
        a, n = self.canvas.selection()
        if n == len(pat) and self.canvas.selected_bytes() == pat:
            self.canvas.anchor, self.canvas.cur = None, a
            if len(rep) == len(pat):
                self.doc.replace(a, rep)
            else:                                   # one undo step
                d = bytes(self.doc.data)
                self.doc.replace_all(d[:a] + rep + d[a + len(pat):])
            self.canvas.cur = a + len(rep)
        self._find(pat, True)

    # ------------------------------------------------------------------ edit helpers
    def _toggle_insert(self):
        self.canvas.insert_mode = self._ins_btn.isChecked()
        self._on_cursor(self.canvas.cur)

    def _insert_dialog(self):
        dlg = QDialog(self)
        dlg.setWindowTitle("Insert bytes")
        f = QFormLayout(dlg)
        cnt = QSpinBox(); cnt.setRange(1, 1 << 24); cnt.setValue(16)
        val = QLineEdit("00")
        f.addRow("How many:", cnt); f.addRow("Byte value (hex):", val)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        f.addRow(bb)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            try:
                b = int(val.text(), 16) & 255
            except ValueError:
                return
            a, n = self.canvas.selection()
            self.doc.insert(a, bytes([b]) * cnt.value())
            self.canvas.goto(a, cnt.value())

    def _fill_dialog(self):
        a, n = self.canvas.selection()
        if not n:
            QMessageBox.information(self, "Fill", "Select some bytes first.")
            return
        txt, ok = QInputDialog.getText(self, "Fill selection", f"Fill {n} byte(s) with (hex, repeats if several bytes):", text="00")
        if ok:
            try:
                pat = bytes.fromhex(txt.replace("0x", " ").replace(" ", ""))
            except ValueError:
                return
            if pat:
                self.doc.replace(a, (pat * (n // len(pat) + 1))[:n])

    def _apply_whole(self, new: bytes, label: str):
        """A structure operation produced new file bytes: one undoable step."""
        keep = self.canvas.cur
        self.doc.replace_all(new)
        self._set_status(label)
        self.canvas.goto(min(keep, len(self.doc)))

    def _import_bytes(self):
        p, _ = QFileDialog.getOpenFileName(self, "Bytes to put at the cursor", "", "All files (*)")
        if p:
            self.canvas.put_bytes(open(p, "rb").read())

    def _export_selection(self):
        a, n = self.canvas.selection()
        if not n:
            QMessageBox.information(self, "Export", "Select some bytes first.")
            return
        p, _ = QFileDialog.getSaveFileName(self, "Save selection", "selection.bin", "All files (*)")
        if p:
            from apps.methods.file_backup import safe_write_bytes
            safe_write_bytes(p, self.canvas.selected_bytes())

    def _export_tree(self):
        from apps.methods import rw_chunks as rw
        roots = rw.parse_rw(bytes(self.doc.data))
        if not roots:
            QMessageBox.information(self, "Export", "No RenderWare sections found.")
            return
        p, _ = QFileDialog.getSaveFileName(self, "Export section tree", "tree.txt", "Text (*.txt)")
        if p:
            Path(p).write_text(rw.dump_tree_text(roots), encoding="utf-8")

    def _mark_region(self, off: int, length: int): #vers 1
        """Tint the structure node picked in the tree."""
        self.canvas.regions = [(off, off + length, QColor(70, 130, 220, 60))] if length else []
        self.canvas.viewport().update()

    def _set_diffs(self, ranges):
        self.canvas.diffs = list(ranges)[:50000]
        self.canvas.viewport().update()

    def _show_hashes(self):
        a, n = self.canvas.selection()
        data = self.canvas.selected_bytes() if n else bytes(self.doc.data)
        h = hashes(data)
        QMessageBox.information(self, "Hashes", f"{'Selection' if n else 'File'}: {len(data):,} bytes\n\n"
                                + "\n".join(f"{k}: {v}" for k, v in h.items()))

    def _toggle_offsets(self):
        self.canvas.offset_hex = not self.canvas.offset_hex
        self.canvas.viewport().update()

    def _font(self, d: int):
        self.canvas.set_font_size(max(6, min(28, self.canvas.font().pointSize() + d)))

    def _convert_dialog(self):
        if getattr(self, "_conv", None) is None:
            self._conv = ConvertDialog(self)
            self._conv.convert_open.connect(lambda new, label: self._apply_whole(new, label))
        self._conv.start(bytes(self.doc.data), os.path.basename(self._file_path or self._entry_label or "open file"))


def show_hex_editor_for_file(main_window, file_path, entry_info=None):  #vers 2
    w = HexWorkshop(main_window=main_window)
    w.resize(1300, 800)
    w.show()
    w.load_file(file_path)
    return w


def show_hex_editor_for_entry(main_window, row, entry_info):  #vers 2
    """Hex-edit an IMG entry. Save writes the edited bytes back into the archive entry."""
    try:
        img = getattr(main_window, 'current_img', None)
        if not img:
            return None
        entry = img.entries[row] if hasattr(img, 'entries') else None
        if not entry:
            return None
        data = img.read_entry_data(entry) if hasattr(img, 'read_entry_data') else img.read_entry(entry)
        w = HexWorkshop(main_window=main_window)
        w.resize(1300, 800)
        w.show()
        w.load_bytes(bytes(data), entry.name)

        def _write_back(new: bytes):
            if not img.add_entry(entry.name, new):            # add_entry replaces an existing entry
                raise RuntimeError("The archive refused the new data")
            try:
                img.modified = True
            except Exception:
                pass
        w._save_callback = _write_back
        return w
    except Exception as ex:
        print(f"show_hex_editor_for_entry: {ex}")
        return None


def open_hex_workshop(main_window=None, file_path=None):  #vers 4
    """Open Hex Workshop - embedded in a tab if main_window has a tab widget, standalone otherwise."""
    mw = main_window
    if mw and hasattr(mw, 'main_tab_widget'):
        c = QWidget()
        l = QVBoxLayout(c)
        l.setContentsMargins(0, 0, 0, 0)
        w = HexWorkshop(parent=c, main_window=mw)
        l.addWidget(w)
        tw = mw.main_tab_widget
        idx = tw.addTab(c, "Hex Workshop")
        tw.setCurrentIndex(idx)
        if hasattr(mw, '_ensure_tab_area_visible'):
            mw._ensure_tab_area_visible()
        if file_path:
            w.load_file(file_path)
        return w
    app = QApplication.instance() or QApplication(sys.argv)
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
    app.setWindowIcon(SVGIconFactory.get_hex_workshop_icon(64))
    w = HexWorkshop(main_window=main_window)
    w.resize(1300, 800)
    w.show()
    if file_path:
        w.load_file(file_path)
    return w


if __name__ == "__main__":
    app = QApplication(sys.argv)
    path = sys.argv[1] if len(sys.argv) > 1 else None
    w = open_hex_workshop(file_path=path)
    sys.exit(app.exec())
