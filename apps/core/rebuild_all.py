#this belongs in core/ rebuild_all.py - Version: 6
# X-Seti - August26 2025 - IMG Factory 1.5 - Batch Rebuild All Functions

import os
import threading
import time
from pathlib import Path
from typing import List, Dict, Optional, Callable
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton, QCheckBox, QListWidget, QListWidgetItem, QProgressBar, QTextEdit, QGroupBox, QFileDialog, QMessageBox, QComboBox, QSpinBox, QMessageBox, QFileDialog, QProgressDialog, QApplication
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal, QTimer

from apps.core.rebuild import rebuild_current_img_native, _perform_native_rebuild
from apps.methods.img_shared_operations import (
    create_progress_callback, validate_img_structure, cleanup_temp_files,
    log_operation_progress
)

from apps.methods.tab_system import get_current_file_from_active_tab, validate_tab_before_operation
#from apps.methods.imgcol_exists import set_context

##Methods list -
# rebuild_all_open_tabs
# rebuild_all_from_folder
# show_batch_rebuild_dialog
# _collect_open_img_tabs
# _collect_img_files_from_folder
# _validate_batch_targets
# BatchRebuildThread
# BatchRebuildDialog
# integrate_batch_rebuild_functions

class BatchRebuildThread(QThread):
    """Background thread for batch rebuild operations"""

    progress_updated = pyqtSignal(int, str, int)  # overall_progress, current_file, file_index
    file_completed = pyqtSignal(str, bool, str)   # file_path, success, message
    batch_completed = pyqtSignal(int, int, list)  # total_files, success_count, failed_files

    def __init__(self, targets: List[Dict], mode: str = "fast"):
        super().__init__()
        self.targets = targets
        self.mode = mode
        self.should_stop = False
        set_context(main_window)

    def run(self):
        """Execute batch rebuild"""
        total_files = len(self.targets)
        success_count = 0
        failed_files = []

        for i, target in enumerate(self.targets):
            if self.should_stop:
                break

            file_path = target['file_path']
            file_name = os.path.basename(file_path)

            self.progress_updated.emit(
                int((i * 100) / total_files),
                f"Rebuilding {file_name}",
                i + 1
            )

            try:
                # Perform rebuild
                success = self._rebuild_single_target(target)

                if success:
                    success_count += 1
                    self.file_completed.emit(file_path, True, "Rebuild successful")
                else:
                    failed_files.append(file_path)
                    self.file_completed.emit(file_path, False, "Rebuild failed")

            except Exception as e:
                failed_files.append(file_path)
                self.file_completed.emit(file_path, False, f"Error: {str(e)}")

        self.batch_completed.emit(total_files, success_count, failed_files)


    def _rebuild_single_target(self, target: Dict) -> bool:
        """Rebuild a single IMG target"""
        try:
            img_file = target['img_object']
            if not img_file:
                return False

            # Use the same native rebuild as single file rebuild
            return _perform_native_rebuild(img_file, self.mode, None)

        except Exception:
            return False

    def stop(self):
        """Stop the batch rebuild process"""
        self.should_stop = True


class BatchRebuildDialog(QDialog):
    """Dialog for batch rebuild operations"""

    def __init__(self, main_window):
        super().__init__(main_window)
        self.main_window = main_window
        self.targets = []
        self.rebuild_thread = None

        self.setWindowTitle("Batch Rebuild IMG Files")
        self.setModal(True)
        self.resize(600, 500)

        self._create_ui()

    def _create_ui(self):
        """Create the dialog UI"""
        layout = QVBoxLayout(self)

        # Source selection
        source_group = QGroupBox("Rebuild Source")
        source_layout = QVBoxLayout(source_group)

        self.open_tabs_radio = QCheckBox("Rebuild All Open IMG Tabs")
        self.open_tabs_radio.setChecked(True)
        source_layout.addWidget(self.open_tabs_radio)

        # Highlight status instead of folder browsing
        highlight_layout = QHBoxLayout()
        self.highlight_radio = QCheckBox("Rebuild Highlighted Files Only")
        self.highlight_status = QLabel("No highlighted files found")
        self.highlight_status.setStyleSheet("color: blue; font-weight: bold;")
        
        highlight_layout.addWidget(self.highlight_radio)
        highlight_layout.addWidget(self.highlight_status)
        source_layout.addLayout(highlight_layout)

        layout.addWidget(source_group)

        # Options
        options_group = QGroupBox("Rebuild Options")
        options_layout = QHBoxLayout(options_group)

        options_layout.addWidget(QLabel("Mode:"))
        self.mode_combo = QComboBox()
        self.mode_combo.addItems(["Fast", "Safe", "Auto"])
        options_layout.addWidget(self.mode_combo)

        options_layout.addStretch()

        options_layout.addWidget(QLabel("Max Concurrent:"))
        self.concurrent_spin = QSpinBox()
        self.concurrent_spin.setRange(1, 4)
        self.concurrent_spin.setValue(1)
        options_layout.addWidget(self.concurrent_spin)

        layout.addWidget(options_group)

        # Target files list
        targets_group = QGroupBox("Target Files")
        targets_layout = QVBoxLayout(targets_group)

        self.targets_list = QListWidget()
        targets_layout.addWidget(self.targets_list)

        refresh_btn = QPushButton("Refresh Target List")
        refresh_btn.clicked.connect(self._refresh_targets)
        targets_layout.addWidget(refresh_btn)

        layout.addWidget(targets_group)

        # Progress section
        progress_group = QGroupBox("Progress")
        progress_layout = QVBoxLayout(progress_group)

        self.overall_progress = QProgressBar()
        self.current_file_label = QLabel("Ready to start")

        progress_layout.addWidget(QLabel("Overall Progress:"))
        progress_layout.addWidget(self.overall_progress)
        progress_layout.addWidget(self.current_file_label)

        self.progress_log = QTextEdit()
        self.progress_log.setMaximumHeight(100)
        self.progress_log.setReadOnly(True)
        progress_layout.addWidget(self.progress_log)

        layout.addWidget(progress_group)

        # Buttons
        button_layout = QHBoxLayout()

        self.start_btn = QPushButton("Start Batch Rebuild")
        self.start_btn.setDefault(True)
        self.start_btn.clicked.connect(self._start_rebuild)

        self.stop_btn = QPushButton("Stop")
        self.stop_btn.setEnabled(False)
        self.stop_btn.clicked.connect(self._stop_rebuild)

        self.close_btn = QPushButton("Close")
        self.close_btn.clicked.connect(self.reject)

        button_layout.addWidget(self.start_btn)
        button_layout.addWidget(self.stop_btn)
        button_layout.addStretch()
        button_layout.addWidget(self.close_btn)

        layout.addLayout(button_layout)

        # Initial target refresh
        QTimer.singleShot(100, self._refresh_targets)



    def _refresh_targets(self):
        """Refresh the target files list"""
        self.targets_list.clear()
        self.targets = []

        if self.open_tabs_radio.isChecked():
            self.targets = _collect_open_img_tabs(self.main_window)

        elif self.highlight_radio.isChecked():
            # Get highlighted files from the current IMG table
            self.targets = self._collect_highlighted_files()
            # Update highlight status
            self.highlight_status.setText(f"Found {len(self.targets)} highlighted files")

        # Populate list widget
        for target in self.targets:
            file_name = os.path.basename(target['file_path'])
            item = QListWidgetItem(f"{file_name} ({target['source']})")
            self.targets_list.addItem(item)

        # Update button state
        self.start_btn.setEnabled(len(self.targets) > 0)
    
    def _collect_highlighted_files(self):
        """Collect highlighted files from the current IMG table"""
        targets = []
        
        # Get the current tab's IMG file and table
        if hasattr(self.main_window, 'get_current_active_tab_info'):
            tab_info = self.main_window.get_current_active_tab_info()
            if tab_info['file_object'] and tab_info['file_type'] == 'IMG' and tab_info['table_widget']:
                table = tab_info['table_widget']
                img_file = tab_info['file_object']
                
                # Get highlighted entries
                for row in range(table.rowCount()):
                    if table.item(row, 0):  # Make sure item exists
                        # Check if this row is highlighted by checking the background color
                        item = table.item(row, 0)
                        if item and item.background().color().name() in ['#90EE90', '#90ee90', '#FFFFE0', '#ffffe0']:  # Light green or light yellow
                            if row < len(img_file.entries):
                                entry = img_file.entries[row]
                                targets.append({
                                    'file_path': f"{img_file.file_path}#{entry.name}",  # Use # to indicate entry within IMG
                                    'source': 'highlighted',
                                    'entry': entry
                                })
        
        return targets

    def _start_rebuild(self):
        """Start batch rebuild process"""
        if not self.targets:
            QMessageBox.warning(self, "No Targets", "No IMG files found to rebuild")
            return

        # Validate targets
        valid_targets = _validate_batch_targets(self.targets, self.main_window)
        if not valid_targets:
            QMessageBox.critical(self, "Invalid Targets", "No valid IMG files found to rebuild")
            return

        # Get selected mode
        mode_map = {"Fast": "fast", "Safe": "safe", "Auto": "auto"}
        mode = mode_map[self.mode_combo.currentText()]

        # Start rebuild thread
        self.rebuild_thread = BatchRebuildThread(valid_targets, mode)
        self.rebuild_thread.progress_updated.connect(self._on_progress_updated)
        self.rebuild_thread.file_completed.connect(self._on_file_completed)
        self.rebuild_thread.batch_completed.connect(self._on_batch_completed)

        self.rebuild_thread.start()

        # Update UI state
        self.start_btn.setEnabled(False)
        self.stop_btn.setEnabled(True)
        self.close_btn.setEnabled(False)

        self.progress_log.append(f"Starting batch rebuild of {len(valid_targets)} files...")

    def _stop_rebuild(self):
        """Stop batch rebuild process"""
        if self.rebuild_thread:
            self.rebuild_thread.stop()
            self.progress_log.append("Stopping batch rebuild...")

    def _on_progress_updated(self, progress: int, current_file: str, file_index: int):
        """Handle progress updates"""
        self.overall_progress.setValue(progress)
        self.current_file_label.setText(f"File {file_index}/{len(self.targets)}: {current_file}")

    def _on_file_completed(self, file_path: str, success: bool, message: str):
        """Handle individual file completion"""
        file_name = os.path.basename(file_path)
        try:
            from apps.methods.imgfactory_svg_icons import get_success_icon, get_error_icon
            if success:
                status = "✅ Completed"  # Using emoji as a simple visual indicator since full SVG integration would require more complex code
            else:
                status = "❌ Failed"  # Using emoji as a simple visual indicator
        except:
            # Fallback to text-only if icon handling fails
            status = "Completed" if success else "Failed"
        self.progress_log.append(f"{status} {file_name}: {message}")

    def _on_batch_completed(self, total_files: int, success_count: int, failed_files: List[str]):
        """Handle batch completion"""
        self.overall_progress.setValue(100)
        self.current_file_label.setText("Batch rebuild completed")

        # Update UI state
        self.start_btn.setEnabled(True)
        self.stop_btn.setEnabled(False)
        self.close_btn.setEnabled(True)

        # Show completion message
        failed_count = len(failed_files)
        if failed_count == 0:
            self.progress_log.append(f"All {success_count} files rebuilt successfully!")
            QMessageBox.information(self, "Batch Complete",
                f"Successfully rebuilt {success_count} IMG files!")
        else:
            self.progress_log.append(f"Completed: {success_count} success, {failed_count} failed")

            failed_names = [os.path.basename(f) for f in failed_files[:5]]
            failed_text = "\n".join(failed_names)
            if failed_count > 5:
                failed_text += f"\n... and {failed_count - 5} more"

            QMessageBox.warning(self, "Batch Complete with Errors",
                f"Rebuilt {success_count} files successfully.\n"
                f"{failed_count} files failed:\n\n{failed_text}")


def rebuild_all_img(main_window): #vers 1
    """Rebuild all IMG files in a folder"""
    try:
        folder_path = QFileDialog.getExistingDirectory(
            main_window, "Select Folder with IMG Files", "", QFileDialog.Option.ShowDirsOnly
        )
        if not folder_path:
            return False

        # Find all IMG files
        img_files = []
        for root, dirs, files in os.walk(folder_path):
            for file in files:
                if file.lower().endswith('.img'):
                    img_files.append(os.path.join(root, file))

        if not img_files:
            QMessageBox.information(main_window, "No IMG Files", "No IMG files found in selected folder")
            return False

        # Rebuild with progress
        progress = QProgressDialog("Rebuilding IMG files...", "Cancel", 0, len(img_files), main_window)
        progress.setWindowModality(Qt.WindowModality.WindowModal)
        progress.show()
        rebuilt = 0

        for i, img_path in enumerate(img_files):
            progress.setValue(i)
            QApplication.processEvents()
            if progress.wasCanceled():
                break

            # Load IMG
            from apps.methods.img_core_classes import IMGFile
            img = IMGFile(img_path)
            if not img.open():
                continue

            # Mock tab context
            main_window.current_img = img
            # Call native rebuild
            from apps.core.rebuild import rebuild_current_img_native
            if rebuild_current_img_native(main_window):
                rebuilt += 1

        progress.close()
        QMessageBox.information(main_window, "Rebuild Complete", f"Rebuilt {rebuilt}/{len(img_files)} IMG files")
        return rebuilt > 0
    except Exception as e:
        QMessageBox.critical(main_window, "Rebuild Error", f"Error: {str(e)}")
        return False


def rebuild_all_open_tabs(main_window) -> bool:
    """Rebuild all open IMG tabs"""
    try:
        targets = _collect_open_img_tabs(main_window)
        if not targets:
            QMessageBox.information(main_window, "No Open IMGs", "No open IMG tabs found to rebuild")
            return False

        dialog = BatchRebuildDialog(main_window)
        dialog.open_tabs_radio.setChecked(True)
        dialog._refresh_targets()

        return dialog.exec() == QDialog.DialogCode.Accepted

    except Exception as e:
        log_operation_progress(main_window, "REBUILD_ALL", "Error", str(e))
        return False


def rebuild_all_from_folder(main_window) -> bool:
    """Rebuild all IMG files from a selected folder"""
    try:
        dialog = BatchRebuildDialog(main_window)
        dialog.folder_radio.setChecked(True)
        dialog.open_tabs_radio.setChecked(False)

        return dialog.exec() == QDialog.DialogCode.Accepted

    except Exception as e:
        log_operation_progress(main_window, "REBUILD_ALL", "Error", str(e))
        return False


def show_batch_rebuild_dialog(main_window) -> bool:
    """Show comprehensive batch rebuild dialog"""
    try:
        dialog = BatchRebuildDialog(main_window)
        return dialog.exec() == QDialog.DialogCode.Accepted

    except Exception as e:
        log_operation_progress(main_window, "REBUILD_ALL", "Dialog error", str(e))
        return False


def _collect_open_img_tabs(main_window) -> List[Dict]:
    """Collect IMG files from open tabs"""
    targets = []

    try:
        if not hasattr(main_window, 'open_files') or not main_window.open_files:
            return targets

        for tab_index, file_info in main_window.open_files.items():
            if file_info.get('type') == 'IMG':
                img_object = file_info.get('file_object')
                file_path = file_info.get('file_path')

                if img_object and file_path:
                    targets.append({
                        'file_path': file_path,
                        'img_object': img_object,
                        'source': f'Tab {tab_index}',
                        'tab_index': tab_index
                    })

    except Exception as e:
        log_operation_progress(main_window, "COLLECT_TABS", "Error", str(e))

    return targets


def _collect_img_files_from_folder(folder_path: str) -> List[Dict]:
    """Collect IMG files from folder"""
    targets = []

    try:
        folder = Path(folder_path)
        if not folder.exists():
            return targets

        # Find IMG files
        for img_file in folder.glob("*.img"):
            targets.append({
                'file_path': str(img_file),
                'img_object': None,  # Will need to load these
                'source': 'Folder',
                'tab_index': None
            })

    except Exception:
        pass

    return targets


def _validate_batch_targets(targets: List[Dict], main_window) -> List[Dict]:
    """Validate batch rebuild targets"""
    valid_targets = []

    for target in targets:
        try:
            # For tab-based targets, use existing object
            if target['img_object']:
                is_valid, _ = validate_img_structure(target['img_object'], main_window)
                if is_valid:
                    valid_targets.append(target)

            # For folder-based targets, need to load and validate
            elif target['file_path'] and os.path.exists(target['file_path']):
                # TODO: Load IMG file for validation
                # For now, assume valid if file exists
                valid_targets.append(target)

        except Exception:
            continue

    return valid_targets


def integrate_batch_rebuild_functions(main_window): #vers 5
    main_window.rebuild_all_img = lambda: rebuild_all_img(main_window)
    main_window.log_message("Batch rebuild functions integrated")
    return True


# Export functions
__all__ = [
    'rebuild_all_open_tabs',
    'rebuild_all_from_folder',
    'show_batch_rebuild_dialog',
    'BatchRebuildDialog',
    'integrate_batch_rebuild_functions'
]
