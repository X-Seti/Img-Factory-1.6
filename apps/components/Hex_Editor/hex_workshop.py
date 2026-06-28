#!/usr/bin/env python3
#this belongs in apps/components/Hex_Editor/hex_workshop.py - Version: 3
# X-Seti - May18 2026 - IMG Factory 1.6 - Hex Workshop
"""
Hex Workshop - Standalone hex editor with 3-panel display.
Based on Hex_Editor_Panel.py three-panel layout.
Uses gui_workshop.py base (local copy — do not import from Tmp_Template).

##Methods list -
# HexViewWidget.__init__
# HexViewWidget._build_panels
# HexViewWidget.load_data
# HexViewWidget.load_file
# HexViewWidget.display_hex
# HexViewWidget.goto_offset
# HexViewWidget.show_hex_context_menu
# HexViewWidget.copy_selected
# HexViewWidget.paste_hex
# StructureView.__init__
# StructureView.parse_dff
# StructureView.show_context_menu
# StructureView.goto_offset
# HexWorkshop.__init__
# HexWorkshop.setup_ui
# HexWorkshop._build_left_panel
# HexWorkshop._build_centre_panel
# HexWorkshop._build_right_panel
# HexWorkshop._open_file
# HexWorkshop._save_file
# HexWorkshop.load_file
# HexWorkshop._goto_offset
# HexWorkshop._build_menus_into_qmenu
# show_hex_editor_for_file
# show_hex_editor_for_entry
# open_hex_workshop
"""

import sys, os, struct, binascii, tempfile
from pathlib import Path
from typing import Optional

_root = Path(__file__).resolve().parents[3]
if str(_root) not in sys.path: sys.path.insert(0, str(_root))

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QLabel, QLineEdit,
    QTextEdit, QTableWidget, QTableWidgetItem, QHeaderView, QScrollArea,
    QPushButton, QFileDialog, QMessageBox, QApplication, QFrame,
    QMenu, QAbstractItemView, QToolBar, QSizePolicy
)
from PyQt6.QtCore import Qt, QMimeData, pyqtSignal
from PyQt6.QtGui import QFont, QColor, QClipboard, QKeySequence, QContextMenuEvent

try:
    from PyQt6.QtGui import QAction
except ImportError:
    from PyQt6.QtWidgets import QAction

from apps.components.Hex_Editor.gui_workshop import GUIWorkshop

App_name   = "Hex Workshop"
App_build  = "Build 1"
config_key = "hex_workshop"


# ─────────────────────────────────────────────────────────────────────────────
# 3-Panel Hex View
# ─────────────────────────────────────────────────────────────────────────────

class HexViewWidget(QWidget):  #vers 1
    """3-panel hex display: Address | Hex values | ASCII"""
    offset_selected = pyqtSignal(int)   # emitted when user clicks a row

    BYTES_PER_ROW = 16

    def __init__(self, parent=None):  #vers 1
        super().__init__(parent)
        self._data: bytes = b''
        self._build_panels()

    def _build_panels(self):  #vers 1
        layout = QHBoxLayout(self)
        layout.setContentsMargins(0,0,0,0); layout.setSpacing(0)
        sp = QSplitter(Qt.Orientation.Horizontal)

        mono = QFont("Courier New", 10)

        # Panel 1 — Address
        self.addr_panel = QTextEdit()
        self.addr_panel.setReadOnly(True); self.addr_panel.setFont(mono)
        self.addr_panel.setMaximumWidth(90)
        self.addr_panel.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.addr_panel.setStyleSheet("background:#1e1e2e; color:#888; border:none;")

        # Panel 2 — Hex
        self.hex_panel = QTextEdit()
        self.hex_panel.setReadOnly(True); self.hex_panel.setFont(mono)
        self.hex_panel.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.hex_panel.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.hex_panel.customContextMenuRequested.connect(self.show_hex_context_menu)
        self.hex_panel.setStyleSheet("background:#1e1e2e; color:#d4d4d4; border:none;")

        # Panel 3 — ASCII
        self.ascii_panel = QTextEdit()
        self.ascii_panel.setReadOnly(True); self.ascii_panel.setFont(mono)
        self.ascii_panel.setMaximumWidth(160)
        self.ascii_panel.setStyleSheet("background:#1e1e2e; color:#9cdcfe; border:none;")

        # Sync scrolling
        def _sync_scroll(val):
            self.addr_panel.verticalScrollBar().setValue(val)
            self.ascii_panel.verticalScrollBar().setValue(val)
        self.hex_panel.verticalScrollBar().valueChanged.connect(_sync_scroll)

        sp.addWidget(self.addr_panel)
        sp.addWidget(self.hex_panel)
        sp.addWidget(self.ascii_panel)
        sp.setSizes([90, 500, 160])
        layout.addWidget(sp)

    def load_data(self, data: bytes):  #vers 1
        self._data = data
        self.display_hex()

    def load_file(self, path: str) -> bool:  #vers 1
        try:
            with open(path, 'rb') as f:
                self._data = f.read()
            self.display_hex()
            return True
        except Exception as ex:
            print(f"HexViewWidget.load_file: {ex}"); return False

    def display_hex(self):  #vers 1
        addr_lines = []; hex_lines = []; ascii_lines = []
        data = self._data
        bpr  = self.BYTES_PER_ROW
        for i in range(0, len(data), bpr):
            chunk = data[i:i+bpr]
            addr_lines.append(f"{i:08X}:")
            hex_part = ' '.join(f"{b:02X}" for b in chunk)
            hex_part += '   ' * (bpr - len(chunk))   # pad short last row
            hex_lines.append(hex_part)
            ascii_part = ''.join(chr(b) if 32 <= b < 127 else '.' for b in chunk)
            ascii_lines.append(ascii_part)
        self.addr_panel.setPlainText('\n'.join(addr_lines))
        self.hex_panel.setPlainText('\n'.join(hex_lines))
        self.ascii_panel.setPlainText('\n'.join(ascii_lines))

    def goto_offset(self, offset: int):  #vers 1
        row = offset // self.BYTES_PER_ROW
        sb  = self.hex_panel.verticalScrollBar()
        line_h = self.hex_panel.fontMetrics().lineSpacing()
        sb.setValue(row * line_h)

    def show_hex_context_menu(self, pos):  #vers 1
        menu = QMenu(self)
        menu.addAction("Copy selected", self.copy_selected)
        menu.addAction("Paste hex",     self.paste_hex)
        menu.exec(self.hex_panel.mapToGlobal(pos))

    def copy_selected(self):  #vers 1
        text = self.hex_panel.textCursor().selectedText()
        QApplication.clipboard().setText(text)

    def paste_hex(self):  #vers 1
        clip = QApplication.clipboard().text().strip().replace(' ','')
        try:
            new_bytes = bytes.fromhex(clip)
            cursor = self.hex_panel.textCursor()
            row    = cursor.blockNumber()
            offset = row * self.BYTES_PER_ROW
            data   = bytearray(self._data)
            data[offset:offset+len(new_bytes)] = new_bytes
            self._data = bytes(data)
            self.display_hex()
        except Exception: pass


# ─────────────────────────────────────────────────────────────────────────────
# Structure / parse view (right panel)
# ─────────────────────────────────────────────────────────────────────────────

class StructureView(QTableWidget):  #vers 1
    goto_requested = pyqtSignal(int)

    COLS = ["Offset", "Size", "Type", "Value", "Description"]

    def __init__(self, parent=None):  #vers 1
        super().__init__(0, len(self.COLS), parent)
        self.setHorizontalHeaderLabels(self.COLS)
        self.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.ResizeToContents)
        self.horizontalHeader().setStretchLastSection(True)
        self.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self.show_context_menu)
        self.setFont(QFont("Courier New", 9))

    def parse_dff(self, data: bytes):  #vers 1
        """Parse RenderWare binary stream chunks."""
        self.setRowCount(0)
        if len(data) < 12: return
        pos = 0
        RW_TYPES = {
            0x01: "Struct",       0x02: "String",
            0x03: "Extension",    0x0E: "Texture",
            0x0F: "Material",     0x10: "Material List",
            0x11: "Frame List",   0x12: "Geometry",
            0x1A: "Clump",        0x1B: "Atomic",
            0x15: "Texture Native",
        }
        try:
            while pos + 12 <= len(data):
                chunk_type = struct.unpack_from('<I', data, pos)[0]
                chunk_size = struct.unpack_from('<I', data, pos+4)[0]
                chunk_ver  = struct.unpack_from('<I', data, pos+8)[0]
                type_name  = RW_TYPES.get(chunk_type, f"0x{chunk_type:04X}")
                row = self.rowCount(); self.insertRow(row)
                for col, val in enumerate([
                    f"0x{pos:08X}", str(chunk_size),
                    f"0x{chunk_type:04X}", f"0x{chunk_ver:08X}", type_name
                ]):
                    item = QTableWidgetItem(val)
                    item.setData(Qt.ItemDataRole.UserRole, pos)
                    self.setItem(row, col, item)
                pos += 12 + chunk_size
                if chunk_size == 0: break
        except Exception: pass

    def show_context_menu(self, pos):  #vers 1
        item = self.itemAt(pos)
        if not item: return
        offset = item.data(Qt.ItemDataRole.UserRole)
        if offset is None: return
        menu = QMenu(self)
        menu.addAction(f"Go to offset 0x{offset:08X}",
                       lambda: self.goto_requested.emit(offset))
        menu.addAction("Copy row", lambda: QApplication.clipboard().setText(
            '  '.join(self.item(item.row(), c).text() for c in range(self.columnCount()))))
        menu.exec(self.viewport().mapToGlobal(pos))

    def goto_offset(self, offset: int):  #vers 1
        for row in range(self.rowCount()):
            it = self.item(row, 0)
            if it and int(it.text(), 16) == offset:
                self.selectRow(row)
                self.scrollToItem(it)
                break


# ─────────────────────────────────────────────────────────────────────────────
# Main Workshop
# ─────────────────────────────────────────────────────────────────────────────

class HexWorkshop(GUIWorkshop):  #vers 1
    App_name   = App_name
    App_build  = App_build
    App_auth   = "X-Seti"
    config_key = config_key

    def __init__(self, parent=None, main_window=None):  #vers 1
        self._defer_setup_ui  = True
        super().__init__(parent)
        self.main_window      = main_window
        self._file_path: Optional[str] = None
        self._data:      bytes         = b''
        self._modified   = False
        self.setup_ui()
        self._set_status("Open a file to begin (File → Open…)")

    def setup_ui(self):  #vers 2
        super().setup_ui()

    def _create_centre_panel(self):  #vers 1
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel); cl.setContentsMargins(0,0,0,0)
        # Goto offset bar
        goto_widget = QWidget()
        gh = QHBoxLayout(goto_widget); gh.setContentsMargins(4,2,4,2)
        gh.addWidget(QLabel("Go to offset:"))
        self._goto_input = QLineEdit(); self._goto_input.setPlaceholderText("0x0000")
        self._goto_input.setMaximumWidth(110); self._goto_input.setFixedHeight(22)
        self._goto_input.returnPressed.connect(self._goto_offset)
        gh.addWidget(self._goto_input)
        gh.addStretch()
        self._file_size_lbl = QLabel(""); gh.addWidget(self._file_size_lbl)
        cl.addWidget(goto_widget)
        self._hex_view = HexViewWidget()
        cl.addWidget(self._hex_view, 1)
        return panel

    def _build_left_panel(self, parent):  #vers 1
        """Left: file info + offset search"""
        panel = QFrame(parent); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ll = QVBoxLayout(panel); ll.setContentsMargins(4,4,4,4); ll.setSpacing(4)
        ll.addWidget(QLabel("File Info"))
        self._info_lbl = QLabel("No file loaded")
        self._info_lbl.setFont(QFont("Courier New", 9))
        self._info_lbl.setWordWrap(True)
        ll.addWidget(self._info_lbl)
        ll.addStretch()
        return panel

    def _build_centre_panel(self, parent):  #vers 1
        """Centre: 3-panel hex view"""
        panel = QFrame(parent); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel); cl.setContentsMargins(0,0,0,0)
        self._hex_view = HexViewWidget()
        cl.addWidget(self._hex_view, 1)
        return panel

    def _build_right_panel(self, parent=None):  #vers 1
        """Right: structure / parse view"""
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        rl = QVBoxLayout(panel); rl.setContentsMargins(0,0,0,0)
        self._struct_view = StructureView()
        self._struct_view.goto_requested.connect(self._hex_view.goto_offset)
        rl.addWidget(self._struct_view, 1)
        return panel

    def load_file(self, path: str):  #vers 1
        try:
            with open(path, 'rb') as f:
                self._data = f.read()
            self._file_path = path
            self._hex_view.load_data(self._data)
            # Auto-parse DFF/TXD/COL structures
            ext = os.path.splitext(path)[1].lower()
            if ext in ('.dff', '.txd'):
                self._struct_view.parse_dff(self._data)
            self._info_lbl.setText(
                f"File: {os.path.basename(path)}\n"
                f"Size: {len(self._data):,} bytes\n"
                f"Path: {path}")
            self.setWindowTitle(f"Hex Workshop — {os.path.basename(path)}")
            self._set_status(f"Loaded: {os.path.basename(path)}  ({len(self._data):,} bytes)")
            if hasattr(self, '_file_size_lbl'):
                self._file_size_lbl.setText(f"{len(self._data):,} bytes")
            self._modified = False
        except Exception as ex:
            QMessageBox.critical(self, "Error", str(ex))

    def _open_file(self, path: str = None):  #vers 1
        if path is None:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open File", "",
                "All GTA files (*.dff *.txd *.col *.img *.dat *.ipl *.ide *.bin);;"
                "All files (*)")
        if path:
            self.load_file(path)

    def _save_file(self):  #vers 1
        if not self._file_path:
            path, _ = QFileDialog.getSaveFileName(self, "Save As", "", "All files (*)")
            if not path: return
            self._file_path = path
        try:
            with open(self._file_path, 'wb') as f:
                f.write(self._hex_view._data)
            self._set_status(f"Saved: {os.path.basename(self._file_path)}")
            self._modified = False
        except Exception as ex:
            QMessageBox.critical(self, "Save Error", str(ex))

    def _goto_offset(self):  #vers 1
        text = self._goto_input.text().strip()
        try:
            offset = int(text, 16) if text.startswith('0x') else int(text)
            self._hex_view.goto_offset(offset)
            self._struct_view.goto_offset(offset)
        except ValueError:
            self._set_status(f"Invalid offset: {text}")

    def _build_menus_into_qmenu(self, pm):  #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open…",    self._open_file)
        fm.addAction("Save",     self._save_file)
        fm.addSeparator()
        fm.addAction("Close",    self.close)
        vm = pm.addMenu("View")
        vm.addAction("Go to offset…", lambda: self._goto_input.setFocus())


# ─────────────────────────────────────────────────────────────────────────────
# Backward compat helpers (used by imgfactory + right_click_actions)
# ─────────────────────────────────────────────────────────────────────────────

def show_hex_editor_for_file(main_window, file_path, entry_info=None):  #vers 1
    w = HexWorkshop(main_window=main_window)
    w.resize(1200, 800); w.show()
    w.load_file(file_path)
    return w

def show_hex_editor_for_entry(main_window, row, entry_info):  #vers 1
    """Open hex editor for an IMG entry (extracts to temp file)."""
    try:
        img = getattr(main_window, 'current_img', None)
        if not img: return None
        entry = img.entries[row] if hasattr(img, 'entries') else None
        if not entry: return None
        data = img.read_entry(entry) if hasattr(img, 'read_entry') else b''
        with tempfile.NamedTemporaryFile(
                suffix=os.path.splitext(entry.name)[1],
                prefix=os.path.splitext(entry.name)[0]+'_',
                delete=False) as tf:
            tf.write(data); tmp_path = tf.name
        w = HexWorkshop(main_window=main_window)
        w.resize(1200, 800); w.show()
        w.load_file(tmp_path)
        return w
    except Exception as ex:
        print(f"show_hex_editor_for_entry: {ex}"); return None

def open_hex_workshop(main_window=None, file_path=None):  #vers 2
    """Open Hex Workshop - embedded in tab if main_window has tab widget, standalone otherwise"""
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
    w = HexWorkshop(main_window=main_window)
    w.resize(1200, 800); w.show()
    if file_path: w.load_file(file_path)
    return w


if __name__ == "__main__":
    app = QApplication(sys.argv)
    path = sys.argv[1] if len(sys.argv) > 1 else None
    w = open_hex_workshop(file_path=path)
    sys.exit(app.exec())
