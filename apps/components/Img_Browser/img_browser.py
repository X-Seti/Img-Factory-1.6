#!/usr/bin/env python3
#this belongs in components.Img_Browser.img_browser.py - Version: #
# X-Seti - August16 2025 - Simple Img Editor.

import sys
import os
import struct
import shutil
from pathlib import Path
from typing import List, Optional, Tuple, Dict, Any
import tempfile
import zipfile
from collections import namedtuple
from PyQt6.QtWidgets import (
    QApplication, QMainWindow, QTabWidget, QTreeWidget, QTreeWidgetItem,
    QFileDialog, QMessageBox, QVBoxLayout, QWidget, QToolBar,
    QInputDialog, QLineEdit, QHeaderView, QPushButton, QLabel, QComboBox,
    QDialog, QMenu, QToolBar
)
from PyQt6.QtCore import (
    Qt, QMimeData, QUrl, pyqtSignal, QObject, QThread, QRect
)
from PyQt6.QtGui import QActionGroup, QAction, QIcon, QDrag, QKeySequence, QShortcut, QPixmap, QPainter, QColor

from PyQt6.QtSvgWidgets import QSvgWidget

App_name = "Img Browser" # keep

# --- Import theme system ---

try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
except ImportError:
    # Fallback if not found (e.g., during dev)
    class AppSettings:
        def __init__(self):
            self.current_settings = {"theme": "App_Factory", "debug_mode": False}
        def get_stylesheet(self): return ""
        def save_settings(self): pass
    class SettingsDialog(QDialog):
        themeChanged = pyqtSignal(str)
        def __init__(self, app_settings, parent=None):
            super().__init__(parent)
            self.setWindowTitle("Settings (fallback)")

# --- Data Structures ---
IMGEntry = namedtuple('IMGEntry', ['name', 'offset', 'size', 'data_offset', 'rw_version'])


# --- IDE Parser ---
class IDEParser:
    def __init__(self, file_path: str):
        self.file_path = file_path
        self.model_names: List[str] = []
        self.load()

    def load(self): #vers 1
        self.model_names = []
        with open(self.file_path, 'r', encoding='utf-8', errors='ignore') as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'): continue
                parts = line.split(',', 2)
                if len(parts) >= 2:
                    model_name = parts[1].strip()
                    if model_name and not model_name.startswith('*'):
                        self.model_names.append(model_name)


# --- IMG Parser ---
class IMGParser:
    def __init__(self, file_path: str = None, debug_mode: bool = False): #vers 1
        self.file_path = file_path
        self.version = 0
        self.entries: List[IMGEntry] = []
        self.file_handle = None
        self.debug_mode = debug_mode
        if file_path:
            self.load(file_path)

    def _decode_name(self, name_bytes: bytes) -> str: #vers 1
        clean = name_bytes.rstrip(b'\x00')
        if not clean: return "(empty)"
        if self.debug_mode:
            return clean.hex()
        try:
            return clean.decode('utf-8', errors='replace')
        except:
            return clean.hex()

    def _parse_external_dir(self, dir_path: str, img_path: str): #vers 1
        with open(dir_path, 'rb') as f:
            num_entries = struct.unpack('<I', f.read(4))[0]
            for _ in range(num_entries):
                entry_data = f.read(32)
                if len(entry_data) < 32: break
                name = self._decode_name(entry_data[:24])
                if name == "(empty)": continue
                offset = struct.unpack('<I', entry_data[24:28])[0]
                size = struct.unpack('<I', entry_data[28:32])[0]
                self.entries.append(IMGEntry(name, offset, size, offset))
        self.file_path = img_path

    def load(self, file_path: str): #vers 2
        self.file_path = file_path
        self.entries = []
        img_path = Path(file_path)
        dir_path = img_path.with_suffix('.dir')
        if dir_path.exists():
            self.version = 2
            self._parse_external_dir(dir_path, file_path)
            return

        with open(file_path, 'rb') as f:
            version_bytes = f.read(4)
            if len(version_bytes) < 4:
                raise ValueError("Invalid IMG file")
            self.version = struct.unpack('<I', version_bytes)[0]

            if self.version == 1:
                f.seek(32)
                while True:
                    entry_data = f.read(32)
                    if len(entry_data) < 32: break
                    name = self._decode_name(entry_data[:24])
                    if name == "(empty)": break
                    offset = struct.unpack('<I', entry_data[24:28])[0]
                    size = struct.unpack('<I', entry_data[28:32])[0]
                    self.entries.append(IMGEntry(name, offset, size, offset))
            elif self.version == 2:
                num_entries = struct.unpack('<I', f.read(4))[0]
                f.seek(8)
                for _ in range(num_entries):
                    entry_data = f.read(32)
                    if len(entry_data) < 32: break
                    name = self._decode_name(entry_data[:24])
                    if name == "(empty)": continue
                    offset = struct.unpack('<I', entry_data[24:28])[0]
                    size = struct.unpack('<I', entry_data[28:32])[0]
                    dir_size = 8 + 32 * num_entries
                    data_offset = dir_size + offset
                    self.entries.append(IMGEntry(name, offset, size, data_offset))
            else:
                next_bytes = f.read(4)
                if len(next_bytes) == 4:
                    num_entries = struct.unpack('<I', next_bytes)[0]
                    if 1 <= num_entries <= 65535:
                        f.seek(8)
                        for _ in range(num_entries):
                            entry_data = f.read(32)
                            if len(entry_data) < 32: break
                            name = self._decode_name(entry_data[:24])
                            if name == "(empty)": continue
                            offset = struct.unpack('<I', entry_data[24:28])[0]
                            size = struct.unpack('<I', entry_data[28:32])[0]
                            self.entries.append(IMGEntry(name, offset, size, offset))
                    else:
                        raise ValueError(f"Unsupported IMG version: {self.version}")
                else:
                    raise ValueError(f"Unsupported IMG version: {self.version}")

    def get_file_data(self, entry: IMGEntry) -> bytes: #vers 1
        if not self.file_handle:
            self.file_handle = open(self.file_path, 'rb')
        self.file_handle.seek(entry.data_offset)
        return self.file_handle.read(entry.size)

    def close(self): #vers 1
        if self.file_handle:
            self.file_handle.close()
            self.file_handle = None

    def create_new(self, version: int = 2): #vers 1
        self.version = version
        self.entries = []
        self.file_path = None

    def add_entry(self, name: str,  bytes): #vers 1
        offset = self.entries[-1].offset + self.entries[-1].size if self.entries else 0
        self.entries.append(IMGEntry(name, offset, len(data), 0))

    def remove_entry(self, name: str): #vers 1
        self.entries = [e for e in self.entries if e.name != name]

    def rename_entry(self, old_name: str, new_name: str): #vers 1
        for i, e in enumerate(self.entries):
            if e.name == old_name:
                self.entries[i] = IMGEntry(new_name, e.offset, e.size, e.data_offset)
                break

    def save(self, file_path: str): #vers 1
        with open(file_path, 'wb') as f:
            if self.version == 1:
                f.write(struct.pack('<I', 1))
                f.write(b'\x00' * 28)
                current_offset = 0
                updated = [IMGEntry(e.name, current_offset := current_offset + e.size - e.size, e.size, 0) for e in self.entries]
                current_offset = 0
                for e in self.entries:
                    name_bytes = e.name.encode('ascii')[:24].ljust(24, b'\x00')
                    f.write(name_bytes)
                    f.write(struct.pack('<I', current_offset))
                    f.write(struct.pack('<I', e.size))
                    current_offset += e.size
                for e in self.entries:
                    f.write(self.get_file_data(e) if self.file_path else b'')
            elif self.version == 2:
                f.write(struct.pack('<I', 2))
                f.write(struct.pack('<I', len(self.entries)))
                current_offset = 0
                for e in self.entries:
                    name_bytes = e.name.encode('ascii')[:24].ljust(24, b'\x00')
                    f.write(name_bytes)
                    f.write(struct.pack('<I', current_offset))
                    f.write(struct.pack('<I', e.size))
                    current_offset += e.size
                for e in self.entries:
                    f.write(self.get_file_data(e) if self.file_path else b'')
        self.file_path = file_path


# --- Tree View ---
class IMGTreeView(QTreeWidget):
    filesDropped = pyqtSignal(list)
    filesDragged = pyqtSignal(list)

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.setHeaderLabels(["Name", "Size", "Offset", "Ext"])
        self.setSortingEnabled(True)
        self.setDragEnabled(True)
        self.setAcceptDrops(True)
        self.setSelectionMode(QTreeWidget.SelectionMode.ExtendedSelection)
        self.setAlternatingRowColors(True)
        header = self.header()
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(3, QHeaderView.ResizeMode.ResizeToContents)

    def startDrag(self, supportedActions): #vers 1
        items = self.selectedItems()
        if not items: return
        entries = [item.entry for item in items if hasattr(item, 'entry')]
        if not entries: return
        self.filesDragged.emit(entries)
        drag = QDrag(self)
        mime = QMimeData()
        temp_dir = tempfile.mkdtemp()
        urls = []
        for e in entries:
            path = Path(temp_dir) / e.name
            path.write_bytes(e.data)
            urls.append(QUrl.fromLocalFile(str(path)))
        mime.setUrls(urls)
        drag.setMimeData(mime)
        drag.exec(supportedActions)

    def dragEnterEvent(self, event): #vers 1
        if event.mimeData().hasUrls():
            files = [u.toLocalFile() for u in event.mimeData().urls() if u.isLocalFile()]
            if any(f.lower().endswith('.ide') for f in files):
                event.acceptProposedAction()
                return
            event.accept()
        else:
            super().dragEnterEvent(event)

    def dropEvent(self, event): #vers 1
        if event.mimeData().hasUrls():
            files = [u.toLocalFile() for u in event.mimeData().urls() if u.isLocalFile()]
            ide_files = [f for f in files if f.lower().endswith('.ide')]
            if ide_files:
                parent = self.parent()
                if hasattr(parent, 'sort_entries_by_ide_order'):
                    try:
                        ide_parser = IDEParser(ide_files[0])
                        parent.sort_entries_by_ide_order(ide_parser.model_names)
                    except Exception as e:
                        QMessageBox.critical(self, "Error", f"Failed to sort by IDE:\n{e}")
                event.accept()
                return
            self.filesDropped.emit(files)
            event.accept()
        else:
            super().dropEvent(event)


# --- Tab ---
class IMGTab(QWidget):
    def __init__(self, file_path: str = None, debug_mode: bool = False, parent=None):
        super().__init__(parent)
        self.file_path = file_path
        self.debug_mode = debug_mode
        self.img_parser = IMGParser(debug_mode=debug_mode)
        self.modified = False
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        self.tree_view = IMGTreeView()
        layout.addWidget(self.tree_view)
        self.tree_view.filesDropped.connect(self.on_files_dropped)
        self.tree_view.filesDragged.connect(self.on_files_dragged)
        if file_path:
            self.load_file(file_path)

    def load_file(self, file_path: str): #vers 1
        try:
            self.img_parser = IMGParser(file_path, debug_mode=self.debug_mode)
            self.file_path = file_path
            self.update_tree()
            self.modified = False
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to load IMG:\n{e}")

    def create_new(self, version: int): #vers 1
        self.img_parser.create_new(version)
        self.file_path = None
        self.update_tree()
        self.modified = True

    def update_tree(self): #vers 1
        self.tree_view.clear()
        for entry in self.img_parser.entries:
            size_str = f"{entry.size} B" if entry.size < 1024 else \
                       f"{entry.size/1024:.1f} KB" if entry.size < 1024**2 else \
                       f"{entry.size/1024**2:.1f} MB"
            ext = Path(entry.name).suffix.lower() or "(none)"
            item = QTreeWidgetItem([entry.name, size_str, f"0x{entry.offset:08X}", ext])
            item.entry = entry
            tooltip = (
                f"Name: {entry.name}\n"
                f"Extension: {ext}\n"
                f"Size: {entry.size} bytes\n"
                f"Offset: 0x{entry.offset:08X}\n"
                f"Data Offset: 0x{entry.data_offset:08X}\n"
                f"IMG Version: {self.img_parser.version}"
            )
            item.setToolTip(0, tooltip)
            self.tree_view.addTopLevelItem(item)
        main = self.window()
        if hasattr(main, 'update_status_bar'):
            main.update_status_bar(self.img_parser.version, len(self.img_parser.entries))

    def get_selected_entries(self): #vers 1
        return [item.entry for item in self.tree_view.selectedItems() if hasattr(item, 'entry')]

    def add_entries(self, files: List[str]): #vers 1
        for file_path in files:
            try:
                with open(file_path, 'rb') as f:
                    data = f.read()
                file_name = os.path.basename(file_path)
                if any(e.name == file_name for e in self.img_parser.entries):
                    QMessageBox.warning(self, "Name Conflict", f"'{file_name}' already exists.")
                    continue
                self.img_parser.add_entry(file_name, data)
                self.modified = True
            except Exception as e:
                QMessageBox.warning(self, "Warning", f"Failed to add {file_path}:\n{e}")
        self.update_tree()

    def remove_selected(self): #vers 1
        entries = self.get_selected_entries()
        if not entries: return
        msg = f"Remove {len(entries)} files?" if len(entries) > 1 else f"Remove '{entries[0].name}'?"
        if QMessageBox.question(self, "Confirm", msg, QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No) == QMessageBox.StandardButton.No:
            return
        for e in entries:
            self.img_parser.remove_entry(e.name)
        self.modified = True
        self.update_tree()

    def rename_selected(self): #vers 1
        entries = self.get_selected_entries()
        if len(entries) != 1:
            QMessageBox.warning(self, "Warning", "Select exactly one file.")
            return
        old = entries[0].name
        new, ok = QInputDialog.getText(self, "Rename", "New name:", QLineEdit.EchoMode.Normal, old)
        if not (ok and new and new != old): return
        if not new.replace('.', '').replace('_', '').replace('-', '').isalnum():
            QMessageBox.warning(self, "Invalid Name", "Use only alphanum + . _ -")
            return
        if any(e.name == new for e in self.img_parser.entries):
            QMessageBox.warning(self, "Name Conflict", "Name already exists.")
            return
        self.img_parser.rename_entry(old, new)
        self.modified = True
        self.update_tree()

    def export_selected(self, export_dir: str = None): #vers 1
        entries = self.get_selected_entries()
        if not entries: return
        if not export_dir:
            export_dir = QFileDialog.getExistingDirectory(self, "Export To")
            if not export_dir: return
        for e in entries:
            try:
                data = self.img_parser.get_file_data(e)
                (Path(export_dir) / e.name).write_bytes(data)
            except Exception as ex:
                QMessageBox.warning(self, "Export Error", f"Failed to export {e.name}:\n{ex}")

    def save(self, file_path: str = None): #vers 2
        if not file_path:
            file_path = self.file_path
        if not file_path:
            file_path, _ = QFileDialog.getSaveFileName(self, "Save IMG", "", "IMG Files (*.img)")
            if not file_path: return False
        try:
            temp = IMGParser()
            temp.version = self.img_parser.version
            temp.entries = self.img_parser.entries.copy()
            temp.file_path = self.file_path
            temp.save(file_path)
            self.file_path = file_path
            self.modified = False
            return True
        except Exception as e:
            QMessageBox.critical(self, "Save Error", f"Failed to save:\n{e}")
            return False

    def rebuild_img(self): #vers 1
        self.update_tree()
        self.modified = True

    def reload_img(self): #vers 1
        if self.file_path:
            self.load_file(self.file_path)

    def on_files_dropped(self, files: List[str]): #vers 1
        self.add_entries(files)

    def on_files_dragged(self, entries: List[IMGEntry]): #vers 1
        for e in entries:
            if not hasattr(e, 'data'):
                e.data = self.img_parser.get_file_data(e)

    def sort_entries_by_ide_order(self, ide_model_names: List[str]): #vers 1
        current = {e.name: e for e in self.img_parser.entries}
        new_entries = []
        seen = set()
        # DFFs
        for name in ide_model_names:
            dff = f"{name}.dff"
            if dff in current and dff not in seen:
                seen.add(dff)
                new_entries.append(current[dff])
        # TXDs at end
        txd_entries = []
        for name in ide_model_names:
            txd = f"{name}.txd"
            if txd in current and txd not in seen:
                seen.add(txd)
                txd_entries.append(current[txd])
        # Leftovers
        leftover = [e for e in self.img_parser.entries if e.name not in seen]
        new_entries.extend(txd_entries)
        new_entries.extend(leftover)
        self.img_parser.entries = new_entries
        self.modified = True
        self.update_tree()


# --- Main Widget ---
class MainWidget(QWidget):
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.debug_mode = False
        self.setup_ui()
        self.max_undo = 10

    def setup_ui(self): #vers 1
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        self.tab_widget = QTabWidget()
        self.tab_widget.setTabsClosable(True)
        self.tab_widget.tabCloseRequested.connect(self.close_tab)
        layout.addWidget(self.tab_widget)
        self.create_toolbar()


    def create_toolbar(self): #vers 1
        toolbar = QToolBar()
        self.layout().insertWidget(0, toolbar)
        self.toolbar = toolbar

        open_action = QAction("Open IMG", self)
        open_action.triggered.connect(self.open_img)
        toolbar.addAction(open_action)

        new_action = QAction("New IMG", self)
        new_action.triggered.connect(self.new_img)
        toolbar.addAction(new_action)

        save_action = QAction("Save", self)
        save_action.triggered.connect(self.save_current)
        toolbar.addAction(save_action)

        save_as_action = QAction("Save As", self)
        save_as_action.triggered.connect(self.save_as_current)
        toolbar.addAction(save_as_action)

        toolbar.addSeparator()

        import_action = QAction("Import Files", self)
        import_action.triggered.connect(self.import_files)
        toolbar.addAction(import_action)

        export_action = QAction("Export Selected", self)
        export_action.triggered.connect(self.export_selected)
        toolbar.addAction(export_action)

        remove_action = QAction("Remove Selected", self)
        remove_action.triggered.connect(self.remove_selected)
        toolbar.addAction(remove_action)

        rename_action = QAction("Rename Selected", self)
        rename_action.triggered.connect(self.rename_selected)
        toolbar.addAction(rename_action)

        toolbar.addSeparator()

        rebuild_action = QAction("Rebuild IMG", self)
        rebuild_action.triggered.connect(self.rebuild_current)
        toolbar.addAction(rebuild_action)

        reload_action = QAction("Reload IMG", self)
        reload_action.triggered.connect(self.reload_current)
        toolbar.addAction(reload_action)

        # === NEW: Sort by IDE ===
        sort_ide_action = QAction("Sort by IDE", self)
        sort_ide_action.triggered.connect(self.sort_by_ide)
        toolbar.addAction(sort_ide_action)

        # === Batch Operations ===
        batch_sort_action = QAction("Batch Sort by IDE", self)
        batch_sort_action.triggered.connect(self.batch_sort_by_ide)
        toolbar.addAction(batch_sort_action)

        export_ide_action = QAction("Export via IDE", self)
        export_ide_action.triggered.connect(self.export_via_ide)
        toolbar.addAction(export_ide_action)

        import_ide_action = QAction("Import via IDE", self)
        import_ide_action.triggered.connect(self.import_via_ide)
        toolbar.addAction(import_ide_action)

        remove_ide_action = QAction("Remove via IDE", self)
        remove_ide_action.triggered.connect(self.remove_via_ide)
        toolbar.addAction(remove_ide_action)

        dump_all_action = QAction("Dump All to Folder", self)
        dump_all_action.triggered.connect(self.dump_all_to_folder)
        toolbar.addAction(dump_all_action)

        toolbar.addSeparator()

        find_action = QAction("Find", self)
        find_action.triggered.connect(self.find_files)
        toolbar.addAction(find_action)

        undo_action = QAction("Undo", self)
        undo_action.triggered.connect(self.undo)
        toolbar.addAction(undo_action)


    def set_toolbar_style(self): #vers 1
        action = self.sender()
        if action and action.isChecked():
            style = action.data()
            self.toolbar.setToolButtonStyle(style)

    def _get_current_tab(self) -> Optional[IMGTab]: #vers 1
        idx = self.tab_widget.currentIndex()
        return self.tab_widget.widget(idx) if idx >= 0 else None

    def sort_by_ide(self): #vers 1
        tab = self._get_current_tab()
        if not tab or not tab.file_path: return
        img = Path(tab.file_path)
        ide_path = QFileDialog.getOpenFileName(self, "Select IDE", str(img.with_suffix('.ide')), "IDE Files (*.ide)")[0]
        if not ide_path: return
        try:
            parser = IDEParser(ide_path)
            tab.sort_entries_by_ide_order(parser.model_names)
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Sort failed:\n{e}")

    def batch_sort_by_ide(self): #vers 1
        """Sort all open IMGs using one IDE file"""
        ide_path, _ = QFileDialog.getOpenFileName(
            self, "Select IDE File", "", "IDE Files (*.ide);;All Files (*)"
        )
        if not ide_path:
            return
        try:
            ide_manager = IDEManager(ide_path)
            for i in range(self.tab_widget.count()):
                tab = self.tab_widget.widget(i)
                if isinstance(tab, IMGTab):
                    added, removed = ide_manager.apply_to_img(tab.img_parser)
                    tab.modified = True
                    tab.update_tree()
                    QMessageBox.information(
                        self, "Batch Sort Complete",
                        f"Tab {i+1}: Added {len(added)}, Removed {len(removed)}"
                    )
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to batch sort:\n{e}")


    def export_selected(self): #vers 1
        current_tab = self.current_tab()
        if current_tab:
            current_tab.export_selected()

    def export_via_ide(self): #vers 1
        tab = self._get_current_tab()
        if not tab: return
        ide_path = QFileDialog.getOpenFileName(self, "Select IDE", "", "IDE Files (*.ide)")[0]
        if not ide_path: return
        export_dir = QFileDialog.getExistingDirectory(self, "Export To")
        if not export_dir: return
        try:
            parser = IDEParser(ide_path)
            os.makedirs(export_dir, exist_ok=True)
            current = {e.name: e for e in tab.img_parser.entries}
            for name in parser.model_names:
                for ext in ['.dff', '.txd']:
                    fname = f"{name}{ext}"
                    if fname in current:
                        (Path(export_dir) / fname).write_bytes(tab.img_parser.get_file_data(current[fname]))
            QMessageBox.information(self, "Success", "Exported IDE-listed files.")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Export failed:\n{e}")

    def import_files(self): #vers 1
        current_tab = self.current_tab()
        if not current_tab:
            return
        file_paths, _ = QFileDialog.getOpenFileNames(self, "Import Files", "", "All Files (*)")
        if file_paths:
            current_tab.add_entries(file_paths)

    def import_via_ide(self): #vers 1
        tab = self._get_current_tab()
        if not tab: return
        ide_path = QFileDialog.getOpenFileName(self, "Select IDE", "", "IDE Files (*.ide)")[0]
        if not ide_path: return
        src_dir = QFileDialog.getExistingDirectory(self, "Source Folder")
        if not src_dir: return
        try:
            parser = IDEParser(ide_path)
            src_files = {f.name: f for f in Path(src_dir).iterdir() if f.is_file()}
            for name in parser.model_names:
                for ext in ['.dff', '.txd']:
                    fname = f"{name}{ext}"
                    if fname in src_files:
                        tab.img_parser.add_entry(fname, src_files[fname].read_bytes())
            tab.modified = True
            tab.update_tree()
            QMessageBox.information(self, "Success", "Imported via IDE order.")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Import failed:\n{e}")

    def remove_selected(self): #vers 1
        current_tab = self.current_tab()
        if current_tab:
            current_tab.remove_selected()

    def remove_via_ide(self): #vers 1
        tab = self._get_current_tab()
        if not tab: return
        ide_path = QFileDialog.getOpenFileName(self, "Select IDE", "", "IDE Files (*.ide)")[0]
        if not ide_path: return
        try:
            parser = IDEParser(ide_path)
            keep = {f"{n}{e}" for n in parser.model_names for e in ['.dff', '.txd']}
            tab.img_parser.entries = [e for e in tab.img_parser.entries if e.name in keep]
            tab.modified = True
            tab.update_tree()
            QMessageBox.information(self, "Success", "Removed unused files.")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Removal failed:\n{e}")

    def rename_selected(self): #vers 1
        current_tab = self.current_tab()
        if current_tab:
            current_tab.rename_selected()

    def rebuild_current(self): #vers 1
        current_tab = self.current_tab()
        if current_tab:
            current_tab.rebuild_img()

    def reload_current(self): #vers 1
        current_tab = self.current_tab()
        if current_tab:
            current_tab.reload_img()

    def find_files(self): #vers 1
        current_tab = self.current_tab()
        if not current_tab:
            return
        text, ok = QInputDialog.getText(self, "Find Files", "Search term:")
        if ok and text:
            current_tab.tree_view.clearSelection()
            matching_items = []
            for i in range(current_tab.tree_view.topLevelItemCount()):
                item = current_tab.tree_view.topLevelItem(i)
                if text.lower() in item.text(0).lower():
                    matching_items.append(item)
            if matching_items:
                matching_items[0].setSelected(True)
                current_tab.tree_view.scrollToItem(matching_items[0])
                if len(matching_items) > 1:
                    QMessageBox.information(self, "Find Results", f"Found {len(matching_items)} matches. First match is selected.")
            else:
                QMessageBox.information(self, "Find Results", "No matches found.")

    def undo(self): #vers 1
        if self.undo_stack:
            QMessageBox.information(self, "Undo", "Undo functionality would restore previous state.")

    def current_tab(self) -> Optional[IMGTab]: #vers 2
        current_index = self.tab_widget.currentIndex()
        if current_index >= 0:
            return self.tab_widget.widget(current_index)
        return None

    def dump_all_to_folder(self): #vers 1
        tab = self._get_current_tab()
        if not tab: return
        export_dir = QFileDialog.getExistingDirectory(self, "Dump To")
        if not export_dir: return
        try:
            for e in tab.img_parser.entries:
                (Path(export_dir) / e.name).write_bytes(tab.img_parser.get_file_data(e))
            QMessageBox.information(self, "Success", f"Dumped {len(tab.img_parser.entries)} files.")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Dump failed:\n{e}")

    def open_img(self): #vers 1
        paths, _ = QFileDialog.getOpenFileNames(self, "Open IMG", "", "IMG Files (*.img)")
        for p in paths:
            self.open_img_file(p)

    def open_img_file(self, path): #vers 1
        try:
            tab = IMGTab(path, debug_mode=self.debug_mode)
            self.tab_widget.addTab(tab, Path(path).name)
            self.tab_widget.setCurrentWidget(tab)
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to open:\n{e}")

    def new_img(self): #vers 2
        dialog = NewIMGDialog(self)
        if dialog.exec():
            version = dialog.get_version()
            tab = IMGTab()
            tab.create_new(version)
            self.tab_widget.addTab(tab, "New Archive")
            self.tab_widget.setCurrentWidget(tab)

    def save_current(self): #vers 1
        tab = self._get_current_tab()
        if tab: tab.save()

    def save_as_current(self): #vers 1
        tab = self._get_current_tab()
        if tab: tab.save()

    def close_tab(self, index): #vers 2
        tab = self.tab_widget.widget(index)
        if tab and tab.modified:
            r = QMessageBox.question(self, "Unsaved", "Save before closing?", QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No | QMessageBox.StandardButton.Cancel)
            if r == QMessageBox.StandardButton.Cancel: return
            elif r == QMessageBox.StandardButton.Yes and not tab.save(): return
        self.tab_widget.removeTab(index)


# --- Main Window ---
class MainWindow(QMainWindow):
    def __init__(self): #vers 1
        super().__init__()
        self.setWindowTitle("GTA IMG Browser")
        self.resize(1000, 700)
        self.setWindowIcon(self.create_icon())
        self.app_settings = AppSettings()
        self.setStyleSheet(self.app_settings.get_stylesheet())
        self.main_widget = MainWidget()
        self.setCentralWidget(self.main_widget)
        self.statusBar().showMessage("Ready")
        self.create_menus()
        QShortcut(QKeySequence.StandardKey.Open, self, self.main_widget.open_img)
        QShortcut(QKeySequence.StandardKey.Save, self, self.main_widget.save_current)
        QShortcut(QKeySequence.StandardKey.Find, self, self.main_widget.find_files)
        QShortcut(QKeySequence("Ctrl+Shift+S"), self, self.main_widget.save_as_current)
        QShortcut(QKeySequence.StandardKey.Undo, self, self.main_widget.undo)

    def create_icon(self): #vers 1
        pixmap = QPixmap(32, 32)
        pixmap.fill(Qt.GlobalColor.transparent)
        painter = QPainter(pixmap)
        painter.setPen(QColor(0, 120, 215))
        painter.setBrush(QColor(0, 120, 215, 100))
        painter.drawRect(5, 5, 22, 22)
        painter.setPen(QColor(255, 255, 255))
        painter.drawText(QRect(0, 0, 32, 32), Qt.AlignmentFlag.AlignCenter, "IMG")
        painter.end()
        return QIcon(pixmap)

    def update_status_bar(self, version: int, num_files: int): #vers 1
        self.statusBar().showMessage(f"IMG Version: {version} | {num_files} files")

    def create_menus(self): #vers 3
        """Create menu bar items"""
        view_menu = self.menuBar().addMenu("View")

        # Toolbar style group
        toolbar_style_group = QActionGroup(self)
        styles = [
            ("Icons Only", Qt.ToolButtonStyle.ToolButtonIconOnly),
            ("Text Only", Qt.ToolButtonStyle.ToolButtonTextOnly),
            ("Text Beside Icons", Qt.ToolButtonStyle.ToolButtonTextBesideIcon),
            ("Text Under Icons", Qt.ToolButtonStyle.ToolButtonTextUnderIcon),
        ]

        for label, style in styles:
            action = QAction(label, self, checkable=True)
            action.setData(style)
            action.triggered.connect(self.set_toolbar_style)
            toolbar_style_group.addAction(action)
            view_menu.addAction(action)
            if style == Qt.ToolButtonStyle.ToolButtonTextUnderIcon:
                action.setChecked(True)

        # Set initial toolbar style
        self.main_widget.toolbar.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextUnderIcon)

    def set_toolbar_style(self): #vers 2
        action = self.sender()
        if (action := self.sender()) and action.isChecked():
            self.main_widget.toolbar.setToolButtonStyle(action.data())

        #if action and action.isChecked():
            #style = action.data()
            #self.toolbar.setToolButtonStyle(style)

    def toggle_debug_mode(self): #vers 1
        self.main_widget.debug_mode = self.debug_action.isChecked()
        for i in range(self.main_widget.tab_widget.count()):
            if (tab := self.main_widget.tab_widget.widget(i)) and isinstance(tab, IMGTab):
                tab.debug_mode = self.main_widget.debug_mode
                if tab.file_path:
                    tab.load_file(tab.file_path)

    def open_settings(self): #vers 1
        dialog = SettingsDialog(self.app_settings, self)
        dialog.themeChanged.connect(lambda _: self.setStyleSheet(self.app_settings.get_stylesheet()))
        dialog.exec()

    def update_status_bar(self, version: int, num_files: int): #vers 1
        self.statusBar().showMessage(f"IMG Version: {version} | {num_files} files")

    def dragEnterEvent(self, event): #vers 1
        if event.mimeData().hasUrls(): event.accept()
        else: super().dragEnterEvent(event)

    def dropEvent(self, event): #vers 1
        for url in event.mimeData().urls():
            if url.isLocalFile() and url.toLocalFile().endswith('.img'):
                self.main_widget.open_img_file(url.toLocalFile())
        event.accept()


class IDEValidator:
    """Validate IMG entries against IDE file"""
    def __init__(self, ide_path: str, img_parser: IMGParser): #vers 1
        self.ide_parser = IDEParser(ide_path)
        self.img_parser = img_parser
        self.img_names = {e.name for e in img_parser.entries}
        self.missing_dffs = []
        self.missing_txds = []
        self.orphaned_files = []

    def validate(self) -> Dict[str, List[str]]: #vers 1
        """Run full validation"""
        self.missing_dffs = []
        self.missing_txds = []
        self.orphaned_files = []

        # Check for missing DFFs (model.dff)
        for model_name in self.ide_parser.model_names:
            dff_name = f"{model_name}.dff"
            if dff_name not in self.img_names:
                self.missing_dffs.append(dff_name)

        # Check for missing TXDs (texture.txd)
        for model_name in self.ide_parser.model_names:
            txd_name = f"{model_name}.txd"
            if txd_name not in self.img_names:
                self.missing_txds.append(txd_name)

        # Check for orphaned files (in IMG but not in IDE)
        ide_model_set = set(self.ide_parser.model_names)
        for entry in self.img_parser.entries:
            base_name = Path(entry.name).stem
            if base_name not in ide_model_set and not entry.name.endswith('.txd'):
                self.orphaned_files.append(entry.name)

        return {
            "missing_dffs": self.missing_dffs,
            "missing_txds": self.missing_txds,
            "orphaned_files": self.orphaned_files
        }

    def show_warnings(self): #vers 1
        """Show validation results as QMessageBox"""
        report = self.validate()
        messages = []

        if report["missing_dffs"]:
            messages.append(f"Missing DFFs: {len(report['missing_dffs'])}")
        if report["missing_txds"]:
            messages.append(f"Missing TXDs: {len(report['missing_txds'])}")
        if report["orphaned_files"]:
            messages.append(f"Orphaned Files: {len(report['orphaned_files'])}")

        if not messages:
            QMessageBox.information(self.img_parser, "Validation", "All files match IDE perfectly.")
            return

        msg = "\n".join(messages)
        details = ""
        if report["missing_dffs"]:
            details += "Missing DFFs:\n" + "\n".join(report["missing_dffs"][:10]) + "\n\n"
        if report["missing_txds"]:
            details += "Missing TXDs:\n" + "\n".join(report["missing_txds"][:10]) + "\n\n"
        if report["orphaned_files"]:
            details += "Orphaned Files:\n" + "\n".join(report["orphaned_files"][:10])

        QMessageBox.warning(
            self.img_parser,
            "Validation Warnings",
            msg + "\n\nSee details for full list.",
            QMessageBox.StandardButton.Ok,
            QMessageBox.StandardButton.NoButton
        )
        # For now, just print to console for dev
        print("[VALIDATION REPORT]")
        print("Missing DFFs:", report["missing_dffs"])
        print("Missing TXDs:", report["missing_txds"])
        print("Orphaned Files:", report["orphaned_files"])


class NewIMGDialog(QDialog):
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.setWindowTitle("New IMG Archive")
        layout = QVBoxLayout(self)
        version_label = QLabel("IMG Version:")
        layout.addWidget(version_label)
        self.version_combo = QComboBox()
        self.version_combo.addItem("GTA San Andreas (Version 2)", 2)
        self.version_combo.addItem("GTA III / Vice City (Version 1)", 1)
        self.version_combo.setCurrentIndex(0)
        layout.addWidget(self.version_combo)
        button_layout = QHBoxLayout()
        ok_button = QPushButton("OK")
        ok_button.clicked.connect(self.accept)
        cancel_button = QPushButton("Cancel")
        cancel_button.clicked.connect(self.reject)
        button_layout.addWidget(ok_button)
        button_layout.addWidget(cancel_button)
        layout.addLayout(button_layout)

    def get_version(self) -> int:
        return self.version_combo.currentData()


def main(): #vers 1
    app = QApplication(sys.argv)
    app.setApplicationName("GTA IMG Browser")
    window = MainWindow()
    window.setAcceptDrops(True)
    window.show()
    for arg in sys.argv[1:]:
        if Path(arg).is_file() and arg.lower().endswith('.img'):
            window.main_widget.open_img_file(arg)
    sys.exit(app.exec())

if __name__ == "__main__":
    main()
