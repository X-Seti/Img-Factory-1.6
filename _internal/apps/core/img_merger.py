#this belongs in core/img_merger.py - Version: 10
# X-Seti - March 03 2026 - IMG Factory 1.6 - IMG Merge Functions
# Credit MexUK 2007 IMG Factory 1.2
"""
IMG Factory Merge Functions
Option 1: Merge from open tabs
Option 2: Pick IMG files from disk
"""

import os
import struct
from typing import List, Dict, Any, Optional
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QFileDialog, QMessageBox, QProgressBar, QGroupBox,
    QRadioButton, QButtonGroup, QLineEdit, QListWidget,
    QListWidgetItem, QComboBox
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal
from apps.methods.img_core_classes import IMGFile, IMGVersion

##Methods list -
# merge_img_function
# merge_from_open_tabs
# merge_from_files
# _generate_unique_name

##Classes -
# MergeWorkerThread
# IMGMergeDialog


def _generate_unique_name(base_name: str, existing_names: set) -> str: #vers 1
    """Return a unique name by appending _1, _2 etc. if base_name is taken."""
    if base_name not in existing_names:
        return base_name
    name_part, _, ext = base_name.rpartition('.')
    if not name_part:
        name_part, ext = base_name, ''
    counter = 1
    while True:
        candidate = f"{name_part}_{counter}.{ext}" if ext else f"{name_part}_{counter}"
        if candidate not in existing_names:
            return candidate
        counter += 1


class MergeWorkerThread(QThread): #vers 4 Fixed
    """Worker thread for IMG merging operations."""
    progress_updated = pyqtSignal(int, str)
    merge_completed = pyqtSignal(bool, str)

    def __init__(self, source_imgs: List[str], output_path: str, options: Dict[str, Any]):
        super().__init__()
        self.source_imgs = source_imgs
        self.output_path = output_path
        self.options = options

    def run(self): #vers 2 Fixed
        """Merge all source IMGs into one output IMG."""
        try:
            self.progress_updated.emit(5, "Opening source files...")
            duplicate_strategy = self.options.get('duplicate_strategy', 'rename')
            version = self.options.get('version', IMGVersion.VERSION_2)

            all_entries = []   # list of {'name': str, 'data': bytes}
            existing_names = set()
            skipped = 0

            total = sum(
                len(IMGFile(p).entries) if IMGFile(p).open() else 0
                for p in self.source_imgs
            )
            processed = 0

            for img_path in self.source_imgs:
                src = IMGFile(img_path)
                if not src.open():
                    self.progress_updated.emit(0, f"Could not open: {os.path.basename(img_path)}")
                    continue

                # V1 data lives in .img, src.file_path points to .dir
                data_path = img_path
                if img_path.lower().endswith('.dir'):
                    data_path = img_path[:-4] + '.img'

                self.progress_updated.emit(
                    10 + int(processed * 60 / max(total, 1)),
                    f"Reading {os.path.basename(img_path)} ({len(src.entries)} entries)...")

                try:
                    fh = open(data_path, 'rb')
                except OSError as e:
                    self.progress_updated.emit(0, f"Cannot read data file: {e}")
                    src.close()
                    continue

                for entry in src.entries:
                    processed += 1
                    try:
                        fh.seek(entry.offset)
                        data = fh.read(entry.size)
                    except Exception:
                        skipped += 1
                        continue

                    if not data:
                        skipped += 1
                        continue

                    name = entry.name
                    if name in existing_names:
                        if duplicate_strategy == 'skip':
                            skipped += 1
                            continue
                        elif duplicate_strategy == 'rename':
                            name = _generate_unique_name(name, existing_names)

                    all_entries.append({'name': name, 'data': data})
                    existing_names.add(name)

                    if processed % 50 == 0:
                        self.progress_updated.emit(
                            10 + int(processed * 60 / max(total, 1)),
                            f"Processed {processed}/{total}")

                fh.close()
                src.close()

            if not all_entries:
                self.merge_completed.emit(False, "No entries could be read from source files.")
                return

            self.progress_updated.emit(75, f"Writing {len(all_entries)} entries...")

            try:
                self._write_output(all_entries, version)
            except Exception as e:
                self.merge_completed.emit(False, f"Write failed: {e}")
                return

            fsize = os.path.getsize(self.output_path)
            self.progress_updated.emit(100, "Done.")
            self.merge_completed.emit(True,
                f"Merged {len(all_entries)} entries  |  skipped {skipped}  |  "
                f"{fsize / (1024*1024):.1f} MB\n{self.output_path}")

        except Exception as e:
            self.merge_completed.emit(False, f"Merge failed: {e}")

    def _write_output(self, entries: list, version: IMGVersion): #vers 1
        """Write merged entries to output file."""
        SECTOR = 2048
        n = len(entries)

        if version == IMGVersion.VERSION_1:
            # V1: separate .dir + .img
            dir_path = self.output_path.replace('.img', '.dir')
            if not self.output_path.lower().endswith('.img'):
                dir_path = self.output_path + '.dir'

            # Build offsets (in sectors)
            current_sector = 0
            dir_data = b''
            data_parts = []
            for e in entries:
                raw = e['data']
                size_sectors = (len(raw) + SECTOR - 1) // SECTOR
                padded = raw + b'\x00' * (size_sectors * SECTOR - len(raw))
                dir_data += struct.pack('<II', current_sector, size_sectors)
                dir_data += e['name'].encode('ascii', 'replace')[:24].ljust(24, b'\x00')
                data_parts.append(padded)
                current_sector += size_sectors

            with open(dir_path, 'wb') as f:
                f.write(dir_data)
            with open(self.output_path, 'wb') as f:
                for part in data_parts:
                    f.write(part)

        else:
            # V2: single .img with VER2 header + directory embedded
            header_bytes = 8 + n * 32  # VER2(4) + count(4) + 32 per entry
            # Data starts at next sector boundary after header
            data_start_sector = (header_bytes + SECTOR - 1) // SECTOR

            current_sector = data_start_sector
            dir_data = b''
            data_parts = []
            for e in entries:
                raw = e['data']
                size_sectors = (len(raw) + SECTOR - 1) // SECTOR
                padded = raw + b'\x00' * (size_sectors * SECTOR - len(raw))
                dir_data += struct.pack('<II', current_sector, size_sectors)
                dir_data += e['name'].encode('ascii', 'replace')[:24].ljust(24, b'\x00')
                data_parts.append(padded)
                current_sector += size_sectors

            with open(self.output_path, 'wb') as f:
                f.write(b'VER2')
                f.write(struct.pack('<I', n))
                f.write(dir_data)
                # Pad to data_start
                pos = f.tell()
                if pos < data_start_sector * SECTOR:
                    f.write(b'\x00' * (data_start_sector * SECTOR - pos))
                for part in data_parts:
                    f.write(part)


class IMGMergeDialog(QDialog): #vers 4 Fixed
    """Dialog for merging IMG files."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Merge IMG Archives")
        self.setModal(True)
        self.setFixedSize(600, 480)
        self.merge_thread = None
        self._build_ui()
        from apps.core.theme_utils import apply_dialog_theme
        apply_dialog_theme(self)
        self._on_method_changed()

    def _build_ui(self): #vers 1
        layout = QVBoxLayout(self)

        # Method
        method_group = QGroupBox("Merge Method")
        ml = QVBoxLayout(method_group)
        self._method_bg = QButtonGroup(self)
        self.tabs_radio = QRadioButton("Merge from open tabs")
        self.files_radio = QRadioButton("Merge from files on disk")
        self.tabs_radio.setChecked(True)
        self._method_bg.addButton(self.tabs_radio, 1)
        self._method_bg.addButton(self.files_radio, 2)
        self.tabs_radio.toggled.connect(self._on_method_changed)
        ml.addWidget(self.tabs_radio)
        ml.addWidget(self.files_radio)
        layout.addWidget(method_group)

        # Source area (rebuilt on method change)
        self.source_group = QGroupBox("Sources")
        self.source_layout = QVBoxLayout(self.source_group)
        layout.addWidget(self.source_group)

        # Output
        out_group = QGroupBox("Output")
        out_layout = QVBoxLayout(out_group)
        path_row = QHBoxLayout()
        path_row.addWidget(QLabel("Save as:"))
        self.output_edit = QLineEdit()
        self.output_edit.setPlaceholderText("Select output path...")
        path_row.addWidget(self.output_edit)
        browse_btn = QPushButton("Browse")
        browse_btn.clicked.connect(self._browse_output)
        path_row.addWidget(browse_btn)
        out_layout.addLayout(path_row)

        opts_row = QHBoxLayout()
        opts_row.addWidget(QLabel("Duplicates:"))
        self.dup_combo = QComboBox()
        self.dup_combo.addItems(["Rename", "Skip", "Replace"])
        opts_row.addWidget(self.dup_combo)
        opts_row.addWidget(QLabel("Version:"))
        self.ver_combo = QComboBox()
        self.ver_combo.addItems(["Version 2 (SA/IV)", "Version 1 (III/VC)"])
        opts_row.addWidget(self.ver_combo)
        opts_row.addStretch()
        out_layout.addLayout(opts_row)
        layout.addWidget(out_group)

        # Progress
        self.progress_bar = QProgressBar()
        self.progress_bar.setVisible(False)
        layout.addWidget(self.progress_bar)
        self.progress_label = QLabel()
        self.progress_label.setVisible(False)
        layout.addWidget(self.progress_label)

        # Buttons
        btn_row = QHBoxLayout()
        btn_row.addStretch()
        cancel = QPushButton("Cancel")
        cancel.clicked.connect(self.reject)
        btn_row.addWidget(cancel)
        self.merge_btn = QPushButton("Start Merge")
        self.merge_btn.setDefault(True)
        self.merge_btn.clicked.connect(self._start_merge)
        btn_row.addWidget(self.merge_btn)
        layout.addLayout(btn_row)

    def _clear_source_layout(self): #vers 1
        while self.source_layout.count():
            item = self.source_layout.takeAt(0)
            if item.widget():
                item.widget().deleteLater()

    def _on_method_changed(self): #vers 1
        self._clear_source_layout()
        if self.tabs_radio.isChecked():
            self._build_tabs_source()
        else:
            self._build_files_source()

    def _build_tabs_source(self): #vers 1
        self.source_layout.addWidget(QLabel("Select 2 or more open IMG tabs:"))
        self.tabs_list = QListWidget()
        self.tabs_list.setSelectionMode(QListWidget.SelectionMode.MultiSelection)
        try:
            mw = self.parent()
            if mw and hasattr(mw, 'main_tab_widget'):
                for i in range(mw.main_tab_widget.count()):
                    tw = mw.main_tab_widget.widget(i)
                    if getattr(tw, 'file_type', '') == 'IMG' and getattr(tw, 'file_object', None):
                        fp = getattr(tw, 'file_path', None) or getattr(tw.file_object, 'file_path', None)
                        if fp:
                            n = len(tw.file_object.entries)
                            item = QListWidgetItem(f"{mw.main_tab_widget.tabText(i)}  ({n} entries)")
                            item.setData(Qt.ItemDataRole.UserRole, fp)
                            self.tabs_list.addItem(item)
        except Exception:
            pass
        if not self.tabs_list.count():
            item = QListWidgetItem("No open IMG tabs found")
            item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEnabled)
            self.tabs_list.addItem(item)
        self.source_layout.addWidget(self.tabs_list)

    def _build_files_source(self): #vers 1
        row = QHBoxLayout()
        self.files_list = QListWidget()
        row.addWidget(self.files_list)
        btns = QVBoxLayout()
        add_btn = QPushButton("Add Files...")
        add_btn.clicked.connect(self._add_files)
        btns.addWidget(add_btn)
        rem_btn = QPushButton("Remove")
        rem_btn.clicked.connect(lambda: [self.files_list.takeItem(
            self.files_list.row(i)) for i in self.files_list.selectedItems()])
        btns.addWidget(rem_btn)
        clr_btn = QPushButton("Clear")
        clr_btn.clicked.connect(self.files_list.clear)
        btns.addWidget(clr_btn)
        btns.addStretch()
        row.addLayout(btns)
        self.source_layout.addLayout(row)

    def _add_files(self): #vers 1
        paths, _ = QFileDialog.getOpenFileNames(
            self, "Select IMG Files", "", "IMG Files (*.img *.dir);;All Files (*)")
        existing = {self.files_list.item(i).data(Qt.ItemDataRole.UserRole)
                    for i in range(self.files_list.count())}
        for p in paths:
            if p not in existing:
                item = QListWidgetItem(os.path.basename(p))
                item.setData(Qt.ItemDataRole.UserRole, p)
                item.setToolTip(p)
                self.files_list.addItem(item)

    def _browse_output(self): #vers 1
        p, _ = QFileDialog.getSaveFileName(
            self, "Save Merged IMG As", "", "IMG Files (*.img);;All Files (*)")
        if p:
            self.output_edit.setText(p)

    def _start_merge(self): #vers 1
        source_files = []
        if self.tabs_radio.isChecked():
            for item in self.tabs_list.selectedItems():
                fp = item.data(Qt.ItemDataRole.UserRole)
                if fp:
                    source_files.append(fp)
        else:
            for i in range(self.files_list.count()):
                fp = self.files_list.item(i).data(Qt.ItemDataRole.UserRole)
                if fp:
                    source_files.append(fp)

        if len(source_files) < 2:
            QMessageBox.warning(self, "Too Few Sources", "Select at least 2 IMG files to merge.")
            return
        output_path = self.output_edit.text().strip()
        if not output_path:
            QMessageBox.warning(self, "No Output", "Specify an output file path.")
            return
        if os.path.exists(output_path):
            if QMessageBox.question(self, "Overwrite?",
                    f"File exists:\n{output_path}\n\nOverwrite?") != QMessageBox.StandardButton.Yes:
                return

        dup_map = {0: 'rename', 1: 'skip', 2: 'replace'}
        ver_map = {0: IMGVersion.VERSION_2, 1: IMGVersion.VERSION_1}
        options = {
            'duplicate_strategy': dup_map[self.dup_combo.currentIndex()],
            'version': ver_map[self.ver_combo.currentIndex()]
        }

        self.merge_thread = MergeWorkerThread(source_files, output_path, options)
        self.merge_thread.progress_updated.connect(self._update_progress)
        self.merge_thread.merge_completed.connect(self._on_merge_completed)
        self.progress_bar.setVisible(True)
        self.progress_label.setVisible(True)
        self.merge_btn.setEnabled(False)
        self.merge_thread.start()

    def _update_progress(self, value: int, message: str): #vers 1
        self.progress_bar.setValue(value)
        self.progress_label.setText(message)

    def _on_merge_completed(self, success: bool, message: str): #vers 1
        self.progress_bar.setVisible(False)
        self.progress_label.setVisible(False)
        self.merge_btn.setEnabled(True)
        if success:
            QMessageBox.information(self, "Merge Complete", message)
            if self.parent() and hasattr(self.parent(), '_load_img_file_in_new_tab'):
                if QMessageBox.question(self, "Open?", "Open the merged file in a new tab?") \
                        == QMessageBox.StandardButton.Yes:
                    self.parent()._load_img_file_in_new_tab(self.output_edit.text().strip())
            self.accept()
        else:
            QMessageBox.critical(self, "Merge Failed", message)


def merge_img_function(main_window): #vers 6 Fixed
    """Show merge dialog - main entry point."""
    try:
        dialog = IMGMergeDialog(main_window)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("IMG merge completed")
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Merge error: {e}")
        QMessageBox.critical(main_window, "Merge Error", str(e))


def merge_from_open_tabs(tab_paths: List[str], output_path: str, **options) -> bool: #vers 2 Fixed
    """Merge open tab IMG paths into output_path (synchronous, no UI)."""
    try:
        thread = MergeWorkerThread(tab_paths, output_path, options)
        thread.run()
        return os.path.exists(output_path)
    except Exception:
        return False


def merge_from_files(file_paths: List[str], output_path: str, **options) -> bool: #vers 2 Fixed
    """Merge file paths into output_path (synchronous, no UI)."""
    try:
        thread = MergeWorkerThread(file_paths, output_path, options)
        thread.run()
        return os.path.exists(output_path)
    except Exception:
        return False


__all__ = [
    'merge_img_function',
    'merge_from_open_tabs',
    'merge_from_files',
    'IMGMergeDialog',
    'MergeWorkerThread',
]
