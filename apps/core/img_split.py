#this belongs in core/img_split.py - Version: 4
# X-Seti - March 03 2026 - IMG Factory 1.6 - IMG Split Functions
# Credit MexUK 2007 IMG Factory 1.2
"""
IMG Factory Split Functions
Split one IMG into multiple IMGs or a folder of loose files,
grouped by file type (DFF, TXD, COL, etc.)
"""

import os
import struct
from typing import Dict, List
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QGroupBox, QLabel, QPushButton,
    QLineEdit, QComboBox, QCheckBox, QProgressBar, QFileDialog,
    QMessageBox, QWidget, QRadioButton, QButtonGroup
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal
from apps.methods.img_core_classes import IMGFile, IMGVersion

##Methods list -
# split_img
# integrate_split_functions

##Classes -
# SplitWorkerThread
# SplitDialog


class SplitWorkerThread(QThread): #vers 1
    """Worker thread for IMG split operations."""
    progress_updated = pyqtSignal(int, str)
    split_completed = pyqtSignal(bool, str, dict)

    def __init__(self, img_file, options: dict):
        super().__init__()
        self.img_file = img_file
        self.options = options

    def run(self): #vers 1
        """Execute the split operation."""
        try:
            output_dir   = self.options['output_dir']
            output_fmt   = self.options['output_format']   # 'folder', 'v1', 'v2'
            group_by     = self.options['group_by_type']
            create_index = self.options['create_index']
            SECTOR       = 2048

            entries = self.img_file.entries
            total   = len(entries)
            if not total:
                self.split_completed.emit(False, "IMG file has no entries.", {})
                return

            self.progress_updated.emit(5, f"Analysing {total} entries...")

            # Group entries by extension
            groups: Dict[str, list] = {}
            for entry in entries:
                ext = entry.name.rsplit('.', 1)[-1].upper() if '.' in entry.name else 'UNKNOWN'
                key = ext if group_by else 'all'
                groups.setdefault(key, []).append(entry)

            self.progress_updated.emit(15, f"Grouped into {len(groups)} buckets...")
            os.makedirs(output_dir, exist_ok=True)

            stats = {'groups': len(groups), 'total': total, 'written': 0, 'output_dir': output_dir}
            done = 0

            # Determine data file path (V1 .dir -> .img)
            img_data_path = getattr(self.img_file, 'file_path', '')
            if img_data_path.lower().endswith('.dir'):
                img_data_path = img_data_path[:-4] + '.img'

            for gi, (group_name, group_entries) in enumerate(groups.items()):
                pct = 15 + int(gi / len(groups) * 80)
                self.progress_updated.emit(pct, f"Writing {group_name} ({len(group_entries)} files)...")

                if output_fmt == 'folder':
                    grp_dir = os.path.join(output_dir, group_name)
                    os.makedirs(grp_dir, exist_ok=True)
                    with open(img_data_path, 'rb') as fh:
                        for entry in group_entries:
                            try:
                                fh.seek(entry.offset)
                                data = fh.read(entry.size)
                                with open(os.path.join(grp_dir, entry.name), 'wb') as out:
                                    out.write(data)
                                stats['written'] += 1
                                done += 1
                            except Exception as e:
                                self.progress_updated.emit(pct, f"Skipped {entry.name}: {e}")
                    if create_index:
                        with open(os.path.join(grp_dir, '_index.txt'), 'w') as idx:
                            idx.write(f"Group: {group_name}  |  {len(group_entries)} files\n")
                            for e in group_entries:
                                idx.write(f"{e.name}  {e.size} bytes\n")

                else:
                    version = IMGVersion.VERSION_1 if output_fmt == 'v1' else IMGVersion.VERSION_2
                    img_out = os.path.join(output_dir, f"{group_name}.img")
                    try:
                        self._write_img(img_data_path, group_entries, img_out, version, SECTOR)
                        stats['written'] += len(group_entries)
                        done += len(group_entries)
                    except Exception as e:
                        self.split_completed.emit(False, f"Failed writing {group_name}: {e}", stats)
                        return

            self.progress_updated.emit(100, "Done.")
            self.split_completed.emit(True,
                f"Split complete: {stats['written']} files across {stats['groups']} groups.",
                stats)

        except Exception as e:
            self.split_completed.emit(False, str(e), {})

    def _write_img(self, src_path: str, entries: list,
                   out_path: str, version: IMGVersion, SECTOR: int): #vers 1
        """Write a subset of entries to a new IMG file."""
        if version == IMGVersion.VERSION_1:
            dir_path = out_path[:-4] + '.dir' if out_path.lower().endswith('.img') else out_path + '.dir'
            dir_data = b''
            data_parts = []
            current_sector = 0
            with open(src_path, 'rb') as fh:
                for entry in entries:
                    fh.seek(entry.offset)
                    raw = fh.read(entry.size)
                    size_sectors = (len(raw) + SECTOR - 1) // SECTOR
                    padded = raw + b'\x00' * (size_sectors * SECTOR - len(raw))
                    dir_data += struct.pack('<II', current_sector, size_sectors)
                    dir_data += entry.name.encode('ascii', 'replace')[:24].ljust(24, b'\x00')
                    data_parts.append(padded)
                    current_sector += size_sectors
            with open(dir_path, 'wb') as f:
                f.write(dir_data)
            with open(out_path, 'wb') as f:
                for part in data_parts:
                    f.write(part)

        else:  # VERSION_2
            n = len(entries)
            header_bytes = 8 + n * 32
            data_start_sector = (header_bytes + SECTOR - 1) // SECTOR
            current_sector = data_start_sector
            dir_data = b''
            data_parts = []
            with open(src_path, 'rb') as fh:
                for entry in entries:
                    fh.seek(entry.offset)
                    raw = fh.read(entry.size)
                    size_sectors = (len(raw) + SECTOR - 1) // SECTOR
                    padded = raw + b'\x00' * (size_sectors * SECTOR - len(raw))
                    dir_data += struct.pack('<II', current_sector, size_sectors)
                    dir_data += entry.name.encode('ascii', 'replace')[:24].ljust(24, b'\x00')
                    data_parts.append(padded)
                    current_sector += size_sectors
            with open(out_path, 'wb') as f:
                f.write(b'VER2')
                f.write(struct.pack('<I', n))
                f.write(dir_data)
                pos = f.tell()
                if pos < data_start_sector * SECTOR:
                    f.write(b'\x00' * (data_start_sector * SECTOR - pos))
                for part in data_parts:
                    f.write(part)


class SplitDialog(QDialog): #vers 2 Fixed
    """Dialog for splitting an IMG file."""

    def __init__(self, main_window):
        super().__init__(main_window)
        self.main_window = main_window
        self.split_thread = None
        self.setWindowTitle("Split IMG File")
        self.setModal(True)
        self.setFixedSize(480, 300)
        self._build_ui()

    def _build_ui(self): #vers 1
        layout = QVBoxLayout(self)

        # Output dir
        dir_group = QGroupBox("Output Directory")
        dir_layout = QHBoxLayout(dir_group)
        self.dir_edit = QLineEdit(os.path.expanduser("~/IMG_Split"))
        dir_layout.addWidget(self.dir_edit)
        browse = QPushButton("Browse")
        browse.clicked.connect(self._browse_dir)
        dir_layout.addWidget(browse)
        layout.addWidget(dir_group)

        # Output format
        fmt_group = QGroupBox("Output Format")
        fmt_layout = QVBoxLayout(fmt_group)
        self._fmt_bg = QButtonGroup(self)
        self.folder_radio = QRadioButton("Loose files in sub-folders (by type)")
        self.v2_radio     = QRadioButton("Multiple IMG v2 files (one per type)")
        self.v1_radio     = QRadioButton("Multiple IMG v1 files (one per type)")
        self.folder_radio.setChecked(True)
        for rb in (self.folder_radio, self.v2_radio, self.v1_radio):
            self._fmt_bg.addButton(rb)
            fmt_layout.addWidget(rb)
        layout.addWidget(fmt_group)

        # Options
        self.index_check = QCheckBox("Create index file per group")
        self.index_check.setChecked(True)
        layout.addWidget(self.index_check)

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
        self.split_btn = QPushButton("Start Split")
        self.split_btn.setDefault(True)
        self.split_btn.clicked.connect(self._start_split)
        btn_row.addWidget(self.split_btn)
        layout.addLayout(btn_row)

    def _browse_dir(self): #vers 1
        d = QFileDialog.getExistingDirectory(self, "Select Output Directory")
        if d:
            self.dir_edit.setText(d)

    def _start_split(self): #vers 1
        output_dir = self.dir_edit.text().strip()
        if not output_dir:
            QMessageBox.warning(self, "No Output", "Please select an output directory.")
            return

        if self.folder_radio.isChecked():
            fmt = 'folder'
        elif self.v2_radio.isChecked():
            fmt = 'v2'
        else:
            fmt = 'v1'

        from apps.methods.tab_system import get_current_file_from_active_tab
        img_file, file_type = get_current_file_from_active_tab(self.main_window)
        if file_type != 'IMG' or not img_file:
            QMessageBox.warning(self, "No IMG", "No IMG file is currently active.")
            return

        options = {
            'output_dir':    output_dir,
            'output_format': fmt,
            'group_by_type': True,
            'create_index':  self.index_check.isChecked(),
        }

        self.split_thread = SplitWorkerThread(img_file, options)
        self.split_thread.progress_updated.connect(self._update_progress)
        self.split_thread.split_completed.connect(self._on_split_completed)
        self.progress_bar.setVisible(True)
        self.progress_label.setVisible(True)
        self.split_btn.setEnabled(False)
        self.split_thread.start()

    def _update_progress(self, value: int, message: str): #vers 1
        self.progress_bar.setValue(value)
        self.progress_label.setText(message)

    def _on_split_completed(self, success: bool, message: str, stats: dict): #vers 1
        self.progress_bar.setVisible(False)
        self.progress_label.setVisible(False)
        self.split_btn.setEnabled(True)
        if success:
            QMessageBox.information(self, "Split Complete", message)
            self.accept()
        else:
            QMessageBox.critical(self, "Split Failed", message)


class SplitViaThread(QThread): #vers 1
    """Extract entries matching an IDE file into a new IMG."""
    progress_updated = pyqtSignal(int, str)
    split_completed  = pyqtSignal(bool, str, dict)

    def __init__(self, img_file, ide_path: str, output_path: str, version, keep_in_source: bool):
        super().__init__()
        self.img_file       = img_file
        self.ide_path       = ide_path
        self.output_path    = output_path
        self.version        = version
        self.keep_in_source = keep_in_source

    def run(self): #vers 1
        """Parse IDE, match entries, write new IMG."""
        try:
            from apps.core.import_via import _parse_ide_sections
            SECTOR = 2048

            self.progress_updated.emit(5, "Parsing IDE file...")
            sections = _parse_ide_sections(self.ide_path)
            if not sections:
                self.split_completed.emit(False, "No model definitions found in IDE.", {})
                return

            # Collect all names from IDE (models -> .dff, textures -> .txd)
            wanted: set = set()
            for sec in sections.values():
                for m in sec['models']:
                    wanted.add(m.lower() + '.dff')
                for t in sec['textures']:
                    wanted.add(t.lower() + '.txd')

            self.progress_updated.emit(20, f"IDE defines {len(wanted)} files, scanning IMG...")

            entries = self.img_file.entries
            matched   = [e for e in entries if e.name.lower() in wanted]
            unmatched = [n for n in wanted if not any(e.name.lower() == n for e in entries)]

            if not matched:
                self.split_completed.emit(False,
                    f"No matching entries found in IMG.
Missing: {len(unmatched)}", {})
                return

            self.progress_updated.emit(40, f"Matched {len(matched)} entries, writing...")

            img_data_path = getattr(self.img_file, 'file_path', '')
            if img_data_path.lower().endswith('.dir'):
                img_data_path = img_data_path[:-4] + '.img'

            # Re-use _write_img from SplitWorkerThread
            worker = SplitWorkerThread.__new__(SplitWorkerThread)
            worker.options = {}
            try:
                worker._write_img(img_data_path, matched, self.output_path, self.version, SECTOR)
            except Exception as e:
                self.split_completed.emit(False, f"Write failed: {e}", {})
                return

            stats = {
                'matched':   len(matched),
                'missing':   len(unmatched),
                'output':    self.output_path,
            }

            self.progress_updated.emit(100, "Done.")
            self.split_completed.emit(True,
                f"Written {len(matched)} entries to {os.path.basename(self.output_path)}
"
                f"Missing from IMG: {len(unmatched)}",
                stats)

        except Exception as e:
            self.split_completed.emit(False, str(e), {})


class SplitViaDialog(QDialog): #vers 1
    """Dialog for IDE-guided IMG split."""

    def __init__(self, main_window):
        super().__init__(main_window)
        self.main_window  = main_window
        self.split_thread = None
        self.setWindowTitle("Split IMG via IDE")
        self.setModal(True)
        self.setFixedSize(500, 280)
        self._build_ui()

    def _build_ui(self): #vers 1
        layout = QVBoxLayout(self)

        # IDE file
        ide_group = QGroupBox("IDE File")
        ide_row = QHBoxLayout(ide_group)
        self.ide_edit = QLineEdit()
        self.ide_edit.setPlaceholderText("Select .ide file...")
        ide_row.addWidget(self.ide_edit)
        ide_browse = QPushButton("Browse")
        ide_browse.clicked.connect(self._browse_ide)
        ide_row.addWidget(ide_browse)
        layout.addWidget(ide_group)

        # Output IMG
        out_group = QGroupBox("Output IMG")
        out_row = QHBoxLayout(out_group)
        self.out_edit = QLineEdit()
        self.out_edit.setPlaceholderText("Save new IMG as...")
        out_row.addWidget(self.out_edit)
        out_browse = QPushButton("Browse")
        out_browse.clicked.connect(self._browse_output)
        out_row.addWidget(out_browse)
        layout.addWidget(out_group)

        # Options row
        opts = QHBoxLayout()
        opts.addWidget(QLabel("Version:"))
        from PyQt6.QtWidgets import QComboBox
        self.ver_combo = QComboBox()
        self.ver_combo.addItems(["Version 2 (SA/IV)", "Version 1 (III/VC)"])
        opts.addWidget(self.ver_combo)
        opts.addStretch()
        layout.addLayout(opts)

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
        self.split_btn = QPushButton("Start Split")
        self.split_btn.setDefault(True)
        self.split_btn.clicked.connect(self._start)
        btn_row.addWidget(self.split_btn)
        layout.addLayout(btn_row)

    def _browse_ide(self): #vers 1
        p, _ = QFileDialog.getOpenFileName(
            self, "Select IDE File", "", "IDE Files (*.ide);;All Files (*)")
        if p:
            self.ide_edit.setText(p)

    def _browse_output(self): #vers 1
        p, _ = QFileDialog.getSaveFileName(
            self, "Save Output IMG", "", "IMG Files (*.img);;All Files (*)")
        if p:
            self.out_edit.setText(p)

    def _start(self): #vers 1
        ide_path   = self.ide_edit.text().strip()
        output_path = self.out_edit.text().strip()
        if not ide_path or not os.path.exists(ide_path):
            QMessageBox.warning(self, "No IDE", "Please select a valid IDE file.")
            return
        if not output_path:
            QMessageBox.warning(self, "No Output", "Please specify an output IMG path.")
            return
        if os.path.exists(output_path):
            if QMessageBox.question(self, "Overwrite?",
                    f"File exists:
{output_path}

Overwrite?") != QMessageBox.StandardButton.Yes:
                return

        from apps.methods.tab_system import get_current_file_from_active_tab
        img_file, file_type = get_current_file_from_active_tab(self.main_window)
        if file_type != 'IMG' or not img_file:
            QMessageBox.warning(self, "No IMG", "No IMG file is currently active.")
            return

        version = IMGVersion.VERSION_1 if self.ver_combo.currentIndex() == 1 else IMGVersion.VERSION_2
        self.split_thread = SplitViaThread(img_file, ide_path, output_path, version, keep_in_source=True)
        self.split_thread.progress_updated.connect(self._update_progress)
        self.split_thread.split_completed.connect(self._on_completed)
        self.progress_bar.setVisible(True)
        self.progress_label.setVisible(True)
        self.split_btn.setEnabled(False)
        self.split_thread.start()

    def _update_progress(self, value: int, message: str): #vers 1
        self.progress_bar.setValue(value)
        self.progress_label.setText(message)

    def _on_completed(self, success: bool, message: str, stats: dict): #vers 1
        self.progress_bar.setVisible(False)
        self.progress_label.setVisible(False)
        self.split_btn.setEnabled(True)
        if success:
            QMessageBox.information(self, "Split Complete", message)
            if self.main_window and hasattr(self.main_window, '_load_img_file_in_new_tab'):
                if QMessageBox.question(self, "Open?", "Open the new IMG in a new tab?")                         == QMessageBox.StandardButton.Yes:
                    self.main_window._load_img_file_in_new_tab(self.out_edit.text().strip())
            self.accept()
        else:
            QMessageBox.critical(self, "Split Failed", message)


def split_img(main_window): #vers 3 Fixed
    """Show split dialog for the active IMG tab."""
    from apps.methods.tab_system import get_current_file_from_active_tab
    img_file, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not img_file:
        QMessageBox.warning(main_window, "No IMG", "No IMG file is currently active.")
        return
    try:
        dialog = SplitDialog(main_window)
        dialog.exec()
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Split error: {e}")


def split_img_via(main_window): #vers 1
    """Show IDE-guided split dialog for the active IMG tab."""
    from apps.methods.tab_system import get_current_file_from_active_tab
    img_file, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not img_file:
        QMessageBox.warning(main_window, "No IMG", "No IMG file is currently active.")
        return
    try:
        dialog = SplitViaDialog(main_window)
        dialog.exec()
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Split Via error: {e}")


def integrate_split_functions(main_window): #vers 3 Fixed
    """Attach split functions to main_window."""
    main_window.split_img     = lambda: split_img(main_window)
    main_window.split_img_via = lambda: split_img_via(main_window)


__all__ = [
    'split_img',
    'split_img_via',
    'integrate_split_functions',
    'SplitDialog',
    'SplitViaDialog',
    'SplitWorkerThread',
    'SplitViaThread',
]
