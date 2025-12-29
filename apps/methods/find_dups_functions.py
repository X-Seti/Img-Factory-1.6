#this belongs in methods/find_duplicates.py - Version: 1
# X-Seti - August27 2025 - IMG Factory 1.5 - Duplicate Detection Functions
# Moved from apps.components.img_manager.py

"""
Duplicate Detection Functions - Find duplicate files in IMG archives
"""

import os
import hashlib
from typing import Dict, List, Any, Tuple, Optional
from collections import defaultdict
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QTableWidget, QTableWidgetItem, QProgressBar, QMessageBox,
    QGroupBox, QCheckBox, QHeaderView
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal

##Methods list -
# find_duplicates_by_hash
# find_duplicates_by_name
# find_duplicates_by_size
# show_duplicates_dialog
# remove_duplicates_dialog

##Classes -
# DuplicateFinderDialog
# DuplicateRemovalDialog

class DuplicateFinderDialog(QDialog): #vers 1
    """Dialog for finding and managing duplicates"""
    
    def __init__(self, parent=None, duplicates_data=None):
        super().__init__(parent)
        self.setWindowTitle("Duplicate Files Found")
        self.setModal(True)
        self.setFixedSize(700, 500)
        self.duplicates_data = duplicates_data or {}
        
        self.setup_ui()
        self.populate_data()
    
    def setup_ui(self):
        """Setup duplicate finder UI"""
        layout = QVBoxLayout(self)
        
        # Title
        title_label = QLabel("🔍 Duplicate Files Analysis")
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_label.setStyleSheet("font-size: 14px; font-weight: bold; margin: 10px;")
        layout.addWidget(title_label)
        
        # Summary
        self.summary_label = QLabel("Scanning for duplicates...")
        layout.addWidget(self.summary_label)
        
        # Duplicates table
        duplicates_group = QGroupBox("Duplicate Groups")
        duplicates_layout = QVBoxLayout(duplicates_group)
        
        self.duplicates_table = QTableWidget()
        self.duplicates_table.setColumnCount(4)
        self.duplicates_table.setHorizontalHeaderLabels(["Group", "Files", "Size", "Total Waste"])
        
        header = self.duplicates_table.horizontalHeader()
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.Stretch)
        header.setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(3, QHeaderView.ResizeMode.ResizeToContents)
        
        duplicates_layout.addWidget(self.duplicates_table)
        layout.addWidget(duplicates_group)
        
        # Options
        options_group = QGroupBox("Actions")
        options_layout = QVBoxLayout(options_group)
        
        self.auto_select_checkbox = QCheckBox("Auto-select files to remove (keep first occurrence)")
        self.auto_select_checkbox.setChecked(True)
        options_layout.addWidget(self.auto_select_checkbox)
        
        layout.addWidget(options_group)
        
        # Buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()
        
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.reject)
        button_layout.addWidget(close_btn)
        
        remove_btn = QPushButton("Remove Duplicates...")
        remove_btn.clicked.connect(self.remove_duplicates)
        button_layout.addWidget(remove_btn)
        
        layout.addLayout(button_layout)
    
    def populate_data(self):
        """Populate duplicate data"""
        if not self.duplicates_data:
            self.summary_label.setText("No duplicates found.")
            return
        
        duplicate_groups = len(self.duplicates_data)
        total_files = sum(len(files) for files in self.duplicates_data.values())
        total_waste = 0
        
        # Calculate waste (size of duplicates beyond the first copy)
        for file_hash, file_names in self.duplicates_data.items():
            if len(file_names) > 1:
                # Estimate size (would need actual file sizes)
                estimated_size = 1024 * 1024  # 1MB estimate per file
                total_waste += estimated_size * (len(file_names) - 1)
        
        self.summary_label.setText(
            f"Found {duplicate_groups} duplicate groups with {total_files} files. "
            f"Estimated waste: {total_waste / (1024*1024):.1f} MB"
        )
        
        # Populate table
        self.duplicates_table.setRowCount(duplicate_groups)
        
        row = 0
        for file_hash, file_names in self.duplicates_data.items():
            if len(file_names) > 1:
                self.duplicates_table.setItem(row, 0, QTableWidgetItem(f"Group {row + 1}"))
                
                files_text = ", ".join(file_names[:3])
                if len(file_names) > 3:
                    files_text += f" (+{len(file_names) - 3} more)"
                
                self.duplicates_table.setItem(row, 1, QTableWidgetItem(files_text))
                self.duplicates_table.setItem(row, 2, QTableWidgetItem("~1MB"))  # Estimate
                self.duplicates_table.setItem(row, 3, QTableWidgetItem(f"~{len(file_names) - 1}MB"))
                
                row += 1
    
    def remove_duplicates(self):
        """Show remove duplicates confirmation"""
        if not self.duplicates_data:
            return
        
        files_to_remove = []
        
        if self.auto_select_checkbox.isChecked():
            # Auto-select: keep first occurrence, remove others
            for file_names in self.duplicates_data.values():
                if len(file_names) > 1:
                    files_to_remove.extend(file_names[1:])  # Keep first, remove rest
        
        if not files_to_remove:
            QMessageBox.information(self, "No Selection", "No files selected for removal.")
            return
        
        reply = QMessageBox.question(
            self, "Confirm Removal",
            f"Remove {len(files_to_remove)} duplicate files?\n\n"
            "This action cannot be undone without rebuilding the IMG.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
        )
        
        if reply == QMessageBox.StandardButton.Yes:
            self.accept()
            # Return list of files to remove
            self.files_to_remove = files_to_remove


def find_duplicates_by_hash(img_file, progress_callback=None) -> Dict[str, List[str]]: #vers 1
    """Find duplicate files by content hash - MOVED FROM img_manager.py"""
    try:
        hash_map = {}
        duplicates = {}
        total_entries = len(img_file.entries)
        
        for i, entry in enumerate(img_file.entries):
            try:
                # Progress callback
                if progress_callback and i % 50 == 0:
                    progress_callback(i, total_entries, f"Hashing: {entry.name}")
                
                data = entry.get_data()
                file_hash = hashlib.md5(data).hexdigest()
                
                if file_hash in hash_map:
                    if file_hash not in duplicates:
                        duplicates[file_hash] = [hash_map[file_hash]]
                    duplicates[file_hash].append(entry.name)
                else:
                    hash_map[file_hash] = entry.name
                    
            except Exception:
                continue
        
        if progress_callback:
            progress_callback(total_entries, total_entries, "Complete")
        
        return duplicates
        
    except Exception:
        return {}


def find_duplicates_by_name(img_file) -> Dict[str, List[str]]: #vers 1
    """Find duplicate files by filename"""
    try:
        name_map = defaultdict(list)
        
        for entry in img_file.entries:
            name_map[entry.name].append(entry.name)
        
        # Return only actual duplicates (more than one occurrence)
        duplicates = {name: files for name, files in name_map.items() if len(files) > 1}
        
        return duplicates
        
    except Exception:
        return {}


def find_duplicates_by_size(img_file) -> Dict[int, List[str]]: #vers 1
    """Find potential duplicate files by size"""
    try:
        size_map = defaultdict(list)
        
        for entry in img_file.entries:
            size_map[entry.size].append(entry.name)
        
        # Return only sizes with multiple files
        duplicates = {size: files for size, files in size_map.items() if len(files) > 1}
        
        return duplicates
        
    except Exception:
        return {}


def show_duplicates_dialog(main_window) -> Optional[List[str]]: #vers 1
    """Show duplicates dialog and return files to remove"""
    try:
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            QMessageBox.warning(main_window, "No IMG File", "Please open an IMG file first.")
            return None
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("🔍 Scanning for duplicates...")
        
        # Find duplicates by hash (most accurate)
        def progress_callback(current, total, message):
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Progress: {message} ({current}/{total})")
        
        duplicates = find_duplicates_by_hash(main_window.current_img, progress_callback)
        
        if not duplicates:
            QMessageBox.information(main_window, "No Duplicates", "No duplicate files found in this IMG archive.")
            return None
        
        # Show dialog
        dialog = DuplicateFinderDialog(main_window, duplicates)
        
        if dialog.exec() == QDialog.DialogCode.Accepted:
            if hasattr(dialog, 'files_to_remove'):
                return dialog.files_to_remove
        
        return None
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Duplicate detection failed: {str(e)}")
        QMessageBox.critical(main_window, "Detection Failed", f"Failed to detect duplicates:\n{str(e)}")
        return None


def remove_duplicates_dialog(main_window) -> bool: #vers 1
    """Show duplicates dialog and remove selected duplicates"""
    try:
        files_to_remove = show_duplicates_dialog(main_window)
        
        if not files_to_remove:
            return False
        
        # Remove the duplicate files
        removed_count = 0
        
        for filename in files_to_remove:
            if hasattr(main_window, 'current_img') and main_window.current_img:
                # Find and remove entry
                for entry in main_window.current_img.entries[:]:  # Copy list to avoid modification issues
                    if entry.name == filename:
                        main_window.current_img.entries.remove(entry)
                        removed_count += 1
                        break
        
        if removed_count > 0:
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"✅ Removed {removed_count} duplicate files")
            
            # Refresh table
            if hasattr(main_window, 'refresh_table'):
                main_window.refresh_table()
            
            QMessageBox.information(main_window, "Duplicates Removed", 
                                  f"Successfully removed {removed_count} duplicate files.\n\n"
                                  "Remember to rebuild the IMG to save changes.")
            return True
        else:
            QMessageBox.warning(main_window, "No Removal", "No files were removed.")
            return False
            
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Duplicate removal failed: {str(e)}")
        QMessageBox.critical(main_window, "Removal Failed", f"Failed to remove duplicates:\n{str(e)}")
        return False


# Export list for external imports
__all__ = [
    'find_duplicates_by_hash',
    'find_duplicates_by_name', 
    'find_duplicates_by_size',
    'show_duplicates_dialog',
    'remove_duplicates_dialog',
    'DuplicateFinderDialog'
]
