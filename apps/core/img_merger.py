#this belongs in core/img_merger.py - Version: 9
# X-Seti - September26 2025 - IMG Factory 1.5 - IMG Merge Functions
# Credit MexUK 2007 IMG Factory 1.2

"""
IMG Factory Merge Functions
Option 1: Merge 2 open tabs into new IMG
Option 2: Pick 2 IMG files from folder to merge
"""

import os
import struct
from typing import List, Optional, Dict, Any
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QFileDialog, QMessageBox, QProgressBar, QGroupBox,
    QRadioButton, QButtonGroup, QLineEdit, QListWidget,
    QListWidgetItem, QComboBox
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal

# Import core IMG functions
from apps.methods.img_core_classes import IMGFile, IMGVersion, IMGEntry

##Methods list -
# merge_img_function
# merge_from_open_tabs
# merge_from_files
# show_merge_dialog
# _generate_unique_name

##Classes -
# IMGMergeDialog
# MergeWorkerThread

class MergeWorkerThread(QThread): #vers 3
    """Worker thread for IMG merging operations"""
    progress_updated = pyqtSignal(int, str)
    merge_completed = pyqtSignal(bool, str)

    def __init__(self, source_imgs: List[str], output_path: str, options: Dict[str, Any]):
        super().__init__()
        self.source_imgs = source_imgs
        self.output_path = output_path
        self.options = options

    def run(self):
        """Run merge operation in thread"""
        try:
            self.progress_updated.emit(10, "Initializing merge operation...")

            # Collect all entries from source files
            all_entries = []
            existing_names = set()
            total_entries = 0
            successful_entries = 0
            skipped_entries = 0
            duplicate_strategy = self.options.get('duplicate_strategy', 'rename')

            # Count total entries
            for img_path in self.source_imgs:
                try:
                    temp_img = IMGFile(img_path)
                    if temp_img.open():
                        total_entries += len(temp_img.entries)
                        temp_img.close()
                except Exception:
                    continue

            self.progress_updated.emit(20, f"Processing {total_entries} entries from {len(self.source_imgs)} IMG files...")

            processed_entries = 0

            # Collect entries from each source IMG
            for i, img_path in enumerate(self.source_imgs):
                try:
                    source_img = IMGFile(img_path)
                    if not source_img.open():
                        self.progress_updated.emit(
                            20 + (i * 60 // len(self.source_imgs)),
                            f"Failed to open: {os.path.basename(img_path)}"
                        )
                        continue

                    self.progress_updated.emit(
                        20 + (i * 60 // len(self.source_imgs)),
                        f"Reading from: {os.path.basename(img_path)} ({len(source_img.entries)} entries)"
                    )

                    # Open file manually for reading entry data
                    with open(img_path, 'rb') as img_file:
                        # Read entries from this IMG
                        for entry in source_img.entries:
                            processed_entries += 1

                            # Handle duplicates
                            final_name = entry.name
                            if entry.name in existing_names:
                                if duplicate_strategy == 'skip':
                                    skipped_entries += 1
                                    continue
                                elif duplicate_strategy == 'rename':
                                    final_name = self._generate_unique_name(entry.name, existing_names)

                            # Read entry data from file
                            try:
                                img_file.seek(entry.offset)
                                entry_data = img_file.read(entry.size)

                                if not entry_data or len(entry_data) == 0:
                                    skipped_entries += 1
                                    continue

                                # DEBUG: Print actual size being stored
                                actual_size = len(entry_data)
                                print(f"DEBUG: {final_name} - read {actual_size} bytes")

                                # Store entry info
                                all_entries.append({
                                    'name': final_name,
                                    'data': entry_data,
                                    'size': actual_size  # Use calculated size, not len(entry_data)
                                })

                                existing_names.add(final_name)
                                successful_entries += 1

                            except Exception:
                                skipped_entries += 1
                                continue

                            # Update progress
                            if processed_entries % 50 == 0:
                                progress = 20 + (processed_entries * 60 // total_entries)
                                self.progress_updated.emit(
                                    progress,
                                    f"Processed {processed_entries}/{total_entries} entries"
                                )

                    source_img.close()

                except Exception:
                    continue

            if successful_entries == 0:
                self.merge_completed.emit(False, "No entries were successfully read")
                return

            self.progress_updated.emit(80, "Writing merged IMG file...")

            version = self.options.get('version', IMGVersion.VERSION_2)

            try:
                with open(self.output_path, 'wb') as out_file:
                    # Write version header (4 bytes)
                    if version == IMGVersion.VERSION_2:
                        out_file.write(b'VER2')
                    else:
                        out_file.write(b'VER\x01')

                    # Write number of entries (4 bytes at offset 4)
                    out_file.write(struct.pack('<I', successful_entries))

                    # Calculate where data starts (after header + directory)
                    # Header: 8 bytes (VER2 + count)
                    # Directory: 32 bytes per entry (offset 4 + size 4 + name 24)
                    data_start = 8 + (successful_entries * 32)

                    # Align to 2048 byte boundary
                    if data_start < 2048:
                        data_start = 2048

                    current_offset = data_start

                    # Build directory entries
                    directory_data = b''
                    for entry_info in all_entries:
                        # Get size from entry_info
                        size_value = entry_info.get('size', 0)

                        # DEBUG: Verify size before packing
                        print(f"WRITE: {entry_info['name']} - size={size_value}")

                        if size_value == 0:
                            print(f"WARNING: {entry_info['name']} has 0 size!")

                        # Offset (4 bytes)
                        directory_data += struct.pack('<I', current_offset)
                        # Size (4 bytes)
                        directory_data += struct.pack('<I', size_value)
                        # Name (24 bytes, null-terminated/padded)
                        name_bytes = entry_info['name'].encode('ascii', errors='replace')[:23]
                        name_bytes = name_bytes.ljust(24, b'\x00')
                        directory_data += name_bytes

                        current_offset += size_value

                    # Write directory at offset 8
                    out_file.seek(8)
                    out_file.write(directory_data)

                    # Pad to data start offset
                    out_file.seek(data_start)

                    # Write all entry data
                    for entry_info in all_entries:
                        out_file.write(entry_info['data'])



                self.progress_updated.emit(100, "Merge complete!")

                # Verify output
                if os.path.exists(self.output_path):
                    file_size = os.path.getsize(self.output_path)
                    success_msg = (
                        f"Merge completed successfully!\n\n"
                        f"Output: {os.path.basename(self.output_path)}\n"
                        f"Total entries: {successful_entries}\n"
                        f"Skipped: {skipped_entries}\n"
                        f"Size: {file_size / (1024*1024):.1f} MB"
                    )
                    self.merge_completed.emit(True, success_msg)
                else:
                    self.merge_completed.emit(False, "Output file was not created")

            except Exception as e:
                self.merge_completed.emit(False, f"Failed to write merged IMG: {str(e)}")

        except Exception as e:
            self.merge_completed.emit(False, f"Merge failed: {str(e)}")


    def merge_completed(self, success: bool, message: str):
        """Handle merge completion"""
        self.progress_bar.setVisible(False)
        self.progress_label.setVisible(False)
        self.merge_btn.setEnabled(True)

        if success:
            QMessageBox.information(self, "Merge Complete", message)

            # Ask if user wants to open the merged file
            reply = QMessageBox.question(
                self,
                "Open Merged File?",
                "Would you like to open the merged IMG file now?"
            )

            if reply == QMessageBox.StandardButton.Yes and self.parent():
                # Load the merged file in main window
                self.parent()._load_img_file_in_new_tab(self.output_path)

            self.accept()
        else:
            QMessageBox.critical(self, "Merge Failed", message)

        def _generate_unique_name(self, base_name: str, existing_names: set) -> str:
            """Generate unique name for duplicate entries"""
            if '.' in base_name:
                name_part, ext = base_name.rsplit('.', 1)
            else:
                name_part, ext = base_name, ''

            counter = 1
            while True:
                if ext:
                    new_name = f"{name_part}_{counter}.{ext}"
                else:
                    new_name = f"{name_part}_{counter}"

                if new_name not in existing_names:
                    return new_name
                counter += 1


class IMGMergeDialog(QDialog): #vers 3
    """Dialog for merging IMG files with 2 options"""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Merge IMG Archives")
        self.setModal(True)
        self.setFixedSize(600, 500)

        self.output_path = ""
        self.merge_thread = None
        self.tabs_list = None
        self.files_list = None

        self.setup_ui()

    def setup_ui(self):
        """Setup merge dialog UI"""
        layout = QVBoxLayout(self)

        # Title
        title_label = QLabel("Merge IMG Archives")
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_label.setStyleSheet("font-size: 16px; font-weight: bold; margin: 10px;")
        layout.addWidget(title_label)

        # Merge method selection
        method_group = QGroupBox("Select Merge Method")
        method_layout = QVBoxLayout(method_group)

        self.method_group = QButtonGroup()

        self.tabs_radio = QRadioButton("Option 1: Merge from Open Tabs")
        self.tabs_radio.setChecked(True)
        self.tabs_radio.toggled.connect(self.on_method_changed)
        method_layout.addWidget(self.tabs_radio)

        self.files_radio = QRadioButton("Option 2: Select IMG Files from Folder")
        self.files_radio.toggled.connect(self.on_method_changed)
        method_layout.addWidget(self.files_radio)

        self.method_group.addButton(self.tabs_radio, 1)
        self.method_group.addButton(self.files_radio, 2)

        layout.addWidget(method_group)

        # Source selection area
        self.source_group = QGroupBox("Source Selection")
        self.source_layout = QVBoxLayout(self.source_group)
        layout.addWidget(self.source_group)

        # Output selection
        output_group = QGroupBox("Output Settings")
        output_layout = QVBoxLayout(output_group)

        output_path_layout = QHBoxLayout()
        self.output_edit = QLineEdit()
        self.output_edit.setPlaceholderText("Select output path...")
        output_path_layout.addWidget(QLabel("Output File:"))
        output_path_layout.addWidget(self.output_edit)

        browse_btn = QPushButton("Browse...")
        browse_btn.clicked.connect(self.browse_output)
        output_path_layout.addWidget(browse_btn)

        output_layout.addLayout(output_path_layout)

        # Options
        options_layout = QHBoxLayout()

        options_layout.addWidget(QLabel("Duplicate Handling:"))
        self.duplicate_combo = QComboBox()
        self.duplicate_combo.addItems(["Rename duplicates", "Skip duplicates", "Replace existing"])
        options_layout.addWidget(self.duplicate_combo)

        options_layout.addWidget(QLabel("Version:"))
        self.version_combo = QComboBox()
        self.version_combo.addItems(["Version 2 (GTA SA)", "Version 1 (GTA III/VC)"])
        options_layout.addWidget(self.version_combo)

        output_layout.addLayout(options_layout)
        layout.addWidget(output_group)

        # Progress area (hidden initially)
        self.progress_bar = QProgressBar()
        self.progress_bar.setVisible(False)
        layout.addWidget(self.progress_bar)

        self.progress_label = QLabel("")
        self.progress_label.setVisible(False)
        layout.addWidget(self.progress_label)

        # Buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)

        self.merge_btn = QPushButton("Start Merge")
        self.merge_btn.clicked.connect(self.start_merge)
        self.merge_btn.setDefault(True)
        button_layout.addWidget(self.merge_btn)

        layout.addLayout(button_layout)

        # Initialize UI
        self.on_method_changed()

    def on_method_changed(self):
        """Handle merge method change"""
        # Clear ALL widgets and layouts from source_group
        while self.source_layout.count():
            item = self.source_layout.takeAt(0)
            if item.widget():
                item.widget().deleteLater()
            elif item.layout():
                # Clear nested layouts
                while item.layout().count():
                    nested = item.layout().takeAt(0)
                    if nested.widget():
                        nested.widget().deleteLater()

        # Add appropriate UI
        if self.tabs_radio.isChecked():
            self.setup_tabs_selection()
        else:
            self.setup_files_selection()

    def setup_tabs_selection(self):
        """Setup UI for selecting open tabs"""
        label = QLabel("Select 2 or more open IMG tabs to merge:")
        self.source_layout.addWidget(label)

        self.tabs_list = QListWidget()
        self.tabs_list.setSelectionMode(QListWidget.SelectionMode.MultiSelection)

        # Get open tabs
        try:
            open_tabs = self.get_open_img_tabs()
            if open_tabs:
                for tab_info in open_tabs:
                    item = QListWidgetItem(f"{tab_info['name']} ({tab_info['entries']} entries)")
                    item.setData(Qt.ItemDataRole.UserRole, tab_info['path'])
                    self.tabs_list.addItem(item)
            else:
                item = QListWidgetItem("No open IMG tabs found")
                item.setEnabled(False)
                self.tabs_list.addItem(item)
        except Exception as e:
            item = QListWidgetItem(f"Error loading tabs: {str(e)}")
            item.setEnabled(False)
            self.tabs_list.addItem(item)

        self.source_layout.addWidget(self.tabs_list)

    def setup_files_selection(self):
        """Setup UI for selecting files"""
        files_layout = QHBoxLayout()

        self.files_list = QListWidget()
        files_layout.addWidget(self.files_list)

        buttons_layout = QVBoxLayout()

        add_btn = QPushButton("Add Files...")
        add_btn.clicked.connect(self.add_files)
        buttons_layout.addWidget(add_btn)

        remove_btn = QPushButton("Remove Selected")
        remove_btn.clicked.connect(self.remove_selected_files)
        buttons_layout.addWidget(remove_btn)

        clear_btn = QPushButton("Clear All")
        clear_btn.clicked.connect(self.clear_files)
        buttons_layout.addWidget(clear_btn)

        buttons_layout.addStretch()
        files_layout.addLayout(buttons_layout)

        self.source_layout.addLayout(files_layout)

    def get_open_img_tabs(self) -> List[Dict[str, Any]]:
        """Get list of open IMG tabs from main window"""
        tabs = []

        try:
            if not self.parent():
                return tabs

            main_window = self.parent()

            if not hasattr(main_window, 'main_tab_widget'):
                return tabs

            # Iterate through all tabs
            for i in range(main_window.main_tab_widget.count()):
                tab_widget = main_window.main_tab_widget.widget(i)

                # Get file data from tab
                if hasattr(tab_widget, 'file_object') and tab_widget.file_object:
                    file_type = getattr(tab_widget, 'file_type', 'NONE')

                    # Only include IMG files
                    if file_type == 'IMG':
                        file_path = getattr(tab_widget, 'file_path', None)
                        file_object = tab_widget.file_object

                        if file_path and hasattr(file_object, 'entries'):
                            tab_name = main_window.main_tab_widget.tabText(i)
                            entry_count = len(file_object.entries)

                            tabs.append({
                                'name': tab_name,
                                'path': file_path,
                                'entries': entry_count
                            })

            return tabs

        except Exception as e:
            print(f"Error getting open tabs: {e}")
            return tabs

    def add_files(self):
        """Add IMG files to merge list"""
        files, _ = QFileDialog.getOpenFileNames(
            self, "Select IMG Files to Merge", "",
            "IMG Files (*.img);;All Files (*.*)"
        )

        for file_path in files:
            # Check if already added
            existing = False
            for i in range(self.files_list.count()):
                item = self.files_list.item(i)
                if item.data(Qt.ItemDataRole.UserRole) == file_path:
                    existing = True
                    break

            if not existing:
                item = QListWidgetItem(os.path.basename(file_path))
                item.setData(Qt.ItemDataRole.UserRole, file_path)
                item.setToolTip(file_path)
                self.files_list.addItem(item)

    def remove_selected_files(self):
        """Remove selected files from list"""
        for item in self.files_list.selectedItems():
            self.files_list.takeItem(self.files_list.row(item))

    def clear_files(self):
        """Clear all files from list"""
        self.files_list.clear()

    def browse_output(self):
        """Browse for output file"""
        file_path, _ = QFileDialog.getSaveFileName(
            self, "Save Merged IMG As", "",
            "IMG Files (*.img);;All Files (*.*)"
        )

        if file_path:
            self.output_edit.setText(file_path)

    def start_merge(self):
        """Start the merge operation"""
        try:
            # Get source files
            source_files = []

            if self.tabs_radio.isChecked():
                # Get selected tabs
                for item in self.tabs_list.selectedItems():
                    file_path = item.data(Qt.ItemDataRole.UserRole)
                    if file_path:
                        source_files.append(file_path)
            else:
                # Get selected files
                for i in range(self.files_list.count()):
                    item = self.files_list.item(i)
                    file_path = item.data(Qt.ItemDataRole.UserRole)
                    if file_path:
                        source_files.append(file_path)

            if len(source_files) < 2:
                QMessageBox.warning(self, "Invalid Selection",
                                  "Please select at least 2 IMG files to merge.")
                return

            output_path = self.output_edit.text().strip()
            if not output_path:
                QMessageBox.warning(self, "No Output", "Please specify an output file path.")
                return

            # Check if output exists
            if os.path.exists(output_path):
                reply = QMessageBox.question(self, "File Exists",
                                           f"Output file exists:\n{output_path}\n\nOverwrite?")
                if reply != QMessageBox.StandardButton.Yes:
                    return

            # Prepare merge options
            duplicate_strategies = {
                0: 'rename',
                1: 'skip',
                2: 'replace'
            }

            versions = {
                0: IMGVersion.VERSION_2,
                1: IMGVersion.VERSION_1
            }

            options = {
                'duplicate_strategy': duplicate_strategies[self.duplicate_combo.currentIndex()],
                'version': versions[self.version_combo.currentIndex()]
            }

            # Start merge thread
            self.merge_thread = MergeWorkerThread(source_files, output_path, options)
            self.merge_thread.progress_updated.connect(self.update_progress)
            self.merge_thread.merge_completed.connect(self.merge_completed)

            # Show progress UI
            self.progress_bar.setVisible(True)
            self.progress_label.setVisible(True)
            self.merge_btn.setEnabled(False)

            self.merge_thread.start()

        except Exception as e:
            QMessageBox.critical(self, "Merge Error", f"Failed to start merge: {str(e)}")

    def update_progress(self, value: int, message: str):
        """Update progress display"""
        self.progress_bar.setValue(value)
        self.progress_label.setText(message)

    def merge_completed(self, success: bool, message: str):
        """Handle merge completion"""
        self.progress_bar.setVisible(False)
        self.progress_label.setVisible(False)
        self.merge_btn.setEnabled(True)

        if success:
            QMessageBox.information(self, "Merge Complete", message)
            self.accept()
        else:
            QMessageBox.critical(self, "Merge Failed", message)


def merge_img_function(main_window): #vers 5
    """Show merge dialog and handle merging - MAIN ENTRY POINT"""
    try:
        dialog = IMGMergeDialog(main_window)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("IMG merge completed successfully")
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error in merge operation: {str(e)}")
        QMessageBox.critical(main_window, "Merge Error", f"Failed to show merge dialog: {str(e)}")


def merge_from_open_tabs(tab_paths: List[str], output_path: str, **options) -> bool: #vers 1
    """Merge IMG files from open tabs - Option 1"""
    # This would use the same logic as MergeWorkerThread but synchronously
    return False


def merge_from_files(file_paths: List[str], output_path: str, **options) -> bool: #vers 1
    """Merge IMG files from file selection - Option 2"""
    return merge_from_open_tabs(file_paths, output_path, **options)


def show_merge_dialog(parent=None) -> Optional[str]: #vers 1
    """Show merge dialog and return output path if successful"""
    try:
        dialog = IMGMergeDialog(parent)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            return dialog.output_edit.text()
        return None
    except Exception:
        return None


def _generate_unique_name(base_name: str, existing_names: set) -> str: #vers 1
    """Generate unique name for duplicate entries"""
    if '.' in base_name:
        name_part, ext = base_name.rsplit('.', 1)
    else:
        name_part, ext = base_name, ''

    counter = 1
    while True:
        if ext:
            new_name = f"{name_part}_{counter}.{ext}"
        else:
            new_name = f"{name_part}_{counter}"

        if new_name not in existing_names:
            return new_name
        counter += 1


__all__ = [
    'merge_img_function',
    'merge_from_open_tabs',
    'merge_from_files',
    'show_merge_dialog',
    'IMGMergeDialog',
    'MergeWorkerThread'
]
