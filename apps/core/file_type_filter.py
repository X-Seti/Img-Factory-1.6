#this belongs in core/file_type_filter.py - version 1
# X-Seti - July10 2025 - Img Factory 1.5
# File type filtering and extraction system

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QComboBox, QLabel, 
    QPushButton, QGroupBox, QCheckBox, QSpinBox, QLineEdit,
    QFileDialog, QMessageBox, QProgressDialog, QTableWidget,
    QHeaderView, QAbstractItemView, QMenu
)
from PyQt6.QtCore import Qt, pyqtSignal, QThread
from PyQt6.QtGui import QAction
from typing import List, Dict, Any, Optional
import os
import tempfile

class FileTypeFilter(QWidget):
    """Enhanced file type filter with detailed options"""
    
    filter_changed = pyqtSignal(dict)  # Emits filter criteria
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setup_ui()
        self.file_type_stats = {}
        self.total_entries = 0
        
    def setup_ui(self):
        """Setup the filter UI"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(5, 5, 5, 5)
        
        # Main filter dropdown
        main_filter_layout = QHBoxLayout()
        main_filter_layout.addWidget(QLabel("📁 Show:"))
        
        self.main_filter_combo = QComboBox()
        self.update_filter_options()
        self.main_filter_combo.currentTextChanged.connect(self.emit_filter_changed)
        main_filter_layout.addWidget(self.main_filter_combo)
        
        # Stats label
        self.stats_label = QLabel("0 files")
        self.stats_label.setStyleSheet("color: #666; font-size: 10px;")
        main_filter_layout.addWidget(self.stats_label)
        
        layout.addLayout(main_filter_layout)
        
        # Advanced options (collapsible)
        self.advanced_group = QGroupBox("🔧 Advanced Filters")
        self.advanced_group.setCheckable(True)
        self.advanced_group.setChecked(False)
        self.setup_advanced_options()
        layout.addWidget(self.advanced_group)
        
        # Connect advanced changes
        self.advanced_group.toggled.connect(self.emit_filter_changed)
    
    def setup_advanced_options(self):
        """Setup advanced filtering options"""
        layout = QVBoxLayout(self.advanced_group)
        
        # Size filter
        size_layout = QHBoxLayout()
        size_layout.addWidget(QLabel("Size:"))
        
        self.size_filter_combo = QComboBox()
        self.size_filter_combo.addItems([
            "Any Size", "< 1 KB", "< 10 KB", "< 100 KB", "< 1 MB", 
            "> 1 MB", "> 10 MB", "Custom Range"
        ])
        self.size_filter_combo.currentTextChanged.connect(self.emit_filter_changed)
        size_layout.addWidget(self.size_filter_combo)
        
        layout.addLayout(size_layout)
        
        # RW Version filter
        version_layout = QHBoxLayout()
        version_layout.addWidget(QLabel("RW Version:"))
        
        self.version_filter_combo = QComboBox()
        self.version_filter_combo.addItems([
            "Any Version", "RW 3.3", "RW 3.4", "RW 3.5", "RW 3.6", "RW 3.7", "Unknown"
        ])
        self.version_filter_combo.currentTextChanged.connect(self.emit_filter_changed)
        version_layout.addWidget(self.version_filter_combo)
        
        layout.addLayout(version_layout)
        
        # Name pattern
        pattern_layout = QHBoxLayout()
        pattern_layout.addWidget(QLabel("Name contains:"))
        
        self.pattern_input = QLineEdit()
        self.pattern_input.setPlaceholderText("e.g., player, vehicle, weapon")
        self.pattern_input.textChanged.connect(self.emit_filter_changed)
        pattern_layout.addWidget(self.pattern_input)
        
        layout.addLayout(pattern_layout)
    
    def update_filter_options(self, file_stats: Dict[str, int] = None):
        """Update filter options based on current file statistics"""
        if file_stats:
            self.file_type_stats = file_stats
            self.total_entries = sum(file_stats.values())
        
        current_text = self.main_filter_combo.currentText()
        self.main_filter_combo.clear()
        
        # Basic options
        options = ["All Files"]
        
        # Add file type options with counts
        if self.file_type_stats:
            type_counts = [
                ("DFF", "Models", self.file_type_stats.get('DFF', 0)),
                ("TXD", "Textures", self.file_type_stats.get('TXD', 0)),
                ("COL", "Collision", self.file_type_stats.get('COL', 0)),
                ("IFP", "Animations", self.file_type_stats.get('IFP', 0)),
                ("WAV", "Audio", self.file_type_stats.get('WAV', 0)),
                ("SCM", "Scripts", self.file_type_stats.get('SCM', 0)),
                ("IPL", "Item Placement", self.file_type_stats.get('IPL', 0)),
                ("DAT", "Data", self.file_type_stats.get('DAT', 0)),
                ("CFG", "Config", self.file_type_stats.get('CFG', 0))
            ]
            
            for ext, desc, count in type_counts:
                if count > 0:
                    options.append(f"{desc} ({ext}) [{count}]")
            
            # Add "Other" for unknown types
            other_count = self.total_entries - sum(count for _, _, count in type_counts)
            if other_count > 0:
                options.append(f"Other Files [{other_count}]")
        else:
            # Default options without counts
            options.extend([
                "Models (DFF)", "Textures (TXD)", "Collision (COL)",
                "Animations (IFP)", "Audio (WAV)", "Scripts (SCM)",
                "Item Placement (IPL)", "Data (DAT)", "Other Files"
            ])
        
        self.main_filter_combo.addItems(options)
        
        # Restore selection if possible
        if current_text:
            index = self.main_filter_combo.findText(current_text)
            if index >= 0:
                self.main_filter_combo.setCurrentIndex(index)
    
    def emit_filter_changed(self):
        """Emit filter changed signal with current criteria"""
        criteria = self.get_filter_criteria()
        self.filter_changed.emit(criteria)
        
        # Update stats display
        if criteria['file_type'] == 'All':
            count = self.total_entries
        else:
            count = self.file_type_stats.get(criteria['file_type'], 0)
        
        self.stats_label.setText(f"{count} files")
    
    def get_filter_criteria(self) -> Dict[str, Any]:
        """Get current filter criteria"""
        main_filter = self.main_filter_combo.currentText()
        
        # Extract file type from selection
        if main_filter == "All Files":
            file_type = "All"
        elif "(" in main_filter and ")" in main_filter:
            # Extract extension from "Description (EXT)" format
            start = main_filter.find("(") + 1
            end = main_filter.find(")", start)
            file_type = main_filter[start:end] if start > 0 and end > start else "Unknown"
        else:
            file_type = "Unknown"
        
        criteria = {
            'file_type': file_type,
            'advanced_enabled': self.advanced_group.isChecked()
        }
        
        # Add advanced criteria if enabled
        if self.advanced_group.isChecked():
            criteria.update({
                'size_filter': self.size_filter_combo.currentText(),
                'version_filter': self.version_filter_combo.currentText(),
                'name_pattern': self.pattern_input.text().strip()
            })
        
        return criteria

class FileExtractionManager(QThread):
    """Manager for extracting files from IMG archives"""
    
    progress_update = pyqtSignal(int, str)  # progress %, status
    extraction_complete = pyqtSignal(int, str)  # count, output_dir
    extraction_error = pyqtSignal(str)  # error message
    file_extracted = pyqtSignal(str, str)  # filename, file_type
    
    def __init__(self, main_window, entries_to_extract: List, output_dir: str, extract_options: Dict):
        super().__init__()
        self.main_window = main_window
        self.entries_to_extract = entries_to_extract
        self.output_dir = output_dir
        self.extract_options = extract_options
        self.should_cancel = False
    
    def cancel_extraction(self):
        """Cancel the extraction process"""
        self.should_cancel = True
    
    def run(self):
        """Run the file extraction process"""
        try:
            total_files = len(self.entries_to_extract)
            extracted_count = 0
            
            self.progress_update.emit(0, f"Starting extraction of {total_files} files...")
            
            # Create output directory structure if needed
            if self.extract_options.get('organize_by_type', False):
                self.create_type_directories()
            
            for i, entry in enumerate(self.entries_to_extract):
                if self.should_cancel:
                    break
                
                # Update progress
                progress = int((i / total_files) * 100)
                self.progress_update.emit(progress, f"Extracting {entry.name}...")
                
                try:
                    # Extract the file
                    if self.extract_single_file(entry):
                        extracted_count += 1
                        file_type = self.get_file_type(entry.name)
                        self.file_extracted.emit(entry.name, file_type)
                    
                except Exception as e:
                    self.main_window.log_message(f"⚠️ Failed to extract {entry.name}: {str(e)}")
            
            self.progress_update.emit(100, "Extraction complete")
            self.extraction_complete.emit(extracted_count, self.output_dir)
            
        except Exception as e:
            self.extraction_error.emit(f"Extraction failed: {str(e)}")
    
    def create_type_directories(self):
        """Create subdirectories for different file types"""
        type_dirs = ['models', 'textures', 'collision', 'animations', 'audio', 'scripts', 'other']
        
        for type_dir in type_dirs:
            full_path = os.path.join(self.output_dir, type_dir)
            os.makedirs(full_path, exist_ok=True)
    
    def extract_single_file(self, entry) -> bool:
        """Extract a single file from the IMG archive"""
        try:
            # Determine output path
            if self.extract_options.get('organize_by_type', False):
                subdir = self.get_type_subdirectory(entry.name)
                output_path = os.path.join(self.output_dir, subdir, entry.name)
            else:
                output_path = os.path.join(self.output_dir, entry.name)
            
            # Create directory if needed
            os.makedirs(os.path.dirname(output_path), exist_ok=True)
            
            # Write file data
            with open(output_path, 'wb') as f:
                f.write(entry.data)
            
            return True
            
        except Exception as e:
            raise Exception(f"Failed to extract {entry.name}: {str(e)}")
    
    def get_type_subdirectory(self, filename: str) -> str:
        """Get the subdirectory for a file type"""
        ext = self.get_file_type(filename).upper()
        
        type_mapping = {
            'DFF': 'models',
            'TXD': 'textures', 
            'COL': 'collision',
            'IFP': 'animations',
            'WAV': 'audio',
            'SCM': 'scripts',
            'IPL': 'other',
            'DAT': 'other'
        }
        
        return type_mapping.get(ext, 'other')
    
    def get_file_type(self, filename: str) -> str:
        """Get file type from filename"""
        if '.' not in filename:
            return 'unknown'
        return filename.split('.')[-1].upper()

class ExtractionDialog(QWidget):
    """Dialog for configuring file extraction"""
    
    def __init__(self, main_window, selected_entries: List, parent=None):
        super().__init__(parent)
        self.main_window = main_window
        self.selected_entries = selected_entries
        self.extraction_manager = None
        self.setup_ui()
        
    def setup_ui(self):
        """Setup extraction dialog UI"""
        self.setWindowTitle("Extract Files")
        self.setMinimumSize(400, 300)
        
        layout = QVBoxLayout(self)
        
        # Summary
        summary_label = QLabel(f"Extracting {len(self.selected_entries)} files")
        summary_label.setStyleSheet("font-weight: bold; font-size: 14px;")
        layout.addWidget(summary_label)
        
        # File types summary
        type_counts = self.get_type_counts()
        summary_text = self.format_type_summary(type_counts)
        type_label = QLabel(summary_text)
        type_label.setStyleSheet("color: #666;")
        layout.addWidget(type_label)
        
        # Output directory
        dir_layout = QHBoxLayout()
        dir_layout.addWidget(QLabel("Output Directory:"))
        
        self.output_dir_input = QLineEdit()
        self.output_dir_input.setText(os.path.expanduser("~/Desktop/IMG_Extract"))
        dir_layout.addWidget(self.output_dir_input)
        
        browse_btn = QPushButton("Browse...")
        browse_btn.clicked.connect(self.browse_output_dir)
        dir_layout.addWidget(browse_btn)
        
        layout.addLayout(dir_layout)
        
        # Options
        options_group = QGroupBox("Extraction Options")
        options_layout = QVBoxLayout(options_group)
        
        self.organize_check = QCheckBox("Organize by file type (create subfolders)")
        self.organize_check.setChecked(True)
        options_layout.addWidget(self.organize_check)
        
        self.overwrite_check = QCheckBox("Overwrite existing files")
        self.overwrite_check.setChecked(True)
        options_layout.addWidget(self.overwrite_check)
        
        self.preserve_structure_check = QCheckBox("Preserve IMG structure")
        options_layout.addWidget(self.preserve_structure_check)
        
        layout.addWidget(options_group)
        
        # Progress
        self.progress_dialog = None
        
        # Buttons
        button_layout = QHBoxLayout()
        
        extract_btn = QPushButton("🚀 Start Extraction")
        extract_btn.clicked.connect(self.start_extraction)
        button_layout.addWidget(extract_btn)
        
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.close)
        button_layout.addWidget(cancel_btn)
        
        layout.addLayout(button_layout)
    
    def get_type_counts(self) -> Dict[str, int]:
        """Get count of each file type in selection"""
        counts = {}
        for entry in self.selected_entries:
            ext = entry.name.split('.')[-1].upper() if '.' in entry.name else 'Unknown'
            counts[ext] = counts.get(ext, 0) + 1
        return counts
    
    def format_type_summary(self, type_counts: Dict[str, int]) -> str:
        """Format type summary for display"""
        if not type_counts:
            return "No files selected"
        
        summary_parts = []
        for file_type, count in sorted(type_counts.items()):
            summary_parts.append(f"{count} {file_type}")
        
        return "Types: " + ", ".join(summary_parts)
    
    def browse_output_dir(self):
        """Browse for output directory"""
        dir_path = QFileDialog.getExistingDirectory(
            self, "Select Output Directory", self.output_dir_input.text()
        )
        
        if dir_path:
            self.output_dir_input.setText(dir_path)
    
    def start_extraction(self):
        """Start the file extraction process"""
        try:
            output_dir = self.output_dir_input.text().strip()
            if not output_dir:
                QMessageBox.warning(self, "Invalid Directory", "Please select an output directory.")
                return
            
            # Create output directory
            os.makedirs(output_dir, exist_ok=True)
            
            # Get extraction options
            extract_options = {
                'organize_by_type': self.organize_check.isChecked(),
                'overwrite_existing': self.overwrite_check.isChecked(),
                'preserve_structure': self.preserve_structure_check.isChecked()
            }
            
            # Create progress dialog
            self.progress_dialog = QProgressDialog("Starting extraction...", "Cancel", 0, 100, self)
            self.progress_dialog.setWindowModality(Qt.WindowModality.WindowModal)
            
            # Start extraction thread
            self.extraction_manager = FileExtractionManager(
                self.main_window, self.selected_entries, output_dir, extract_options
            )
            
            # Connect signals
            self.extraction_manager.progress_update.connect(self.update_progress)
            self.extraction_manager.extraction_complete.connect(self.extraction_finished)
            self.extraction_manager.extraction_error.connect(self.extraction_failed)
            self.extraction_manager.file_extracted.connect(self.file_extracted)
            
            # Connect cancel
            self.progress_dialog.canceled.connect(self.extraction_manager.cancel_extraction)
            
            self.extraction_manager.start()
            self.progress_dialog.show()
            
        except Exception as e:
            QMessageBox.critical(self, "Extraction Error", f"Failed to start extraction:\n{str(e)}")
    
    def update_progress(self, progress: int, status: str):
        """Update extraction progress"""
        if self.progress_dialog:
            self.progress_dialog.setValue(progress)
            self.progress_dialog.setLabelText(status)
    
    def file_extracted(self, filename: str, file_type: str):
        """Handle file extracted signal"""
        self.main_window.log_message(f"✅ Extracted: {filename} ({file_type})")
    
    def extraction_finished(self, count: int, output_dir: str):
        """Handle extraction completion"""
        if self.progress_dialog:
            self.progress_dialog.close()
        
        QMessageBox.information(
            self, "Extraction Complete", 
            f"Successfully extracted {count} files to:\n{output_dir}"
        )
        
        self.main_window.log_message(f"🎉 Extraction complete: {count} files → {output_dir}")
        self.close()
    
    def extraction_failed(self, error_msg: str):
        """Handle extraction failure"""
        if self.progress_dialog:
            self.progress_dialog.close()
        
        QMessageBox.critical(self, "Extraction Failed", error_msg)
        self.main_window.log_message(f"❌ Extraction failed: {error_msg}")

# Integration functions
def integrate_file_filtering(main_window):
    """Integrate file filtering into main window"""
    try:
        # Replace existing filter with new version
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'filter_panel'):
            # Get parent of old filter panel
            old_filter = main_window.gui_layout.filter_panel
            parent_widget = old_filter.parent()
            
            if parent_widget:
                # Create new filter
                new_filter = FileTypeFilter(parent_widget)
                
                # Replace in layout
                layout = parent_widget.layout()
                if layout:
                    layout.replaceWidget(old_filter, new_filter)
                    old_filter.deleteLater()
                    
                    # Update references
                    main_window.gui_layout.filter_panel = new_filter
                    main_window.file_filter = new_filter
                    
                    # Connect to table filtering
                    if hasattr(main_window.gui_layout, 'table'):
                        new_filter.filter_changed.connect(
                            lambda criteria: apply_file_filter(main_window, criteria)
                        )
                    
                    main_window.log_message("✅ File filtering enabled")
                    return True
        
        return False
        
    except Exception as e:
        main_window.log_message(f"❌ Failed to integrate file filtering: {str(e)}")
        return False

def apply_file_filter(main_window, criteria: Dict[str, Any]):
    """Apply filter criteria to the entries table"""
    try:
        if not hasattr(main_window.gui_layout, 'table'):
            return
        
        table = main_window.gui_layout.table
        total_rows = table.rowCount()
        visible_count = 0
        
        for row in range(total_rows):
            should_show = check_entry_matches_criteria(table, row, criteria)
            table.setRowHidden(row, not should_show)
            
            if should_show:
                visible_count += 1
        
        # Update status
        if visible_count != total_rows:
            main_window.log_message(f"🔍 Filter applied: {visible_count}/{total_rows} files visible")
        
    except Exception as e:
        main_window.log_message(f"⚠️ Filter error: {str(e)}")

def check_entry_matches_criteria(table: QTableWidget, row: int, criteria: Dict[str, Any]) -> bool:
    """Check if table entry matches filter criteria"""
    try:
        # Get entry data
        name_item = table.item(row, 0)
        type_item = table.item(row, 1)
        size_item = table.item(row, 2)
        
        if not name_item:
            return False
        
        entry_name = name_item.text()
        entry_type = type_item.text() if type_item else ""
        entry_size_text = size_item.text() if size_item else ""
        
        # Check file type
        file_type_filter = criteria.get('file_type', 'All')
        if file_type_filter != 'All':
            entry_ext = entry_name.split('.')[-1].upper() if '.' in entry_name else 'Unknown'
            if entry_ext != file_type_filter:
                return False
        
        # Check advanced criteria if enabled
        if criteria.get('advanced_enabled', False):
            # Name pattern
            pattern = criteria.get('name_pattern', '').lower()
            if pattern and pattern not in entry_name.lower():
                return False
            
            # Size filter
            size_filter = criteria.get('size_filter', 'Any Size')
            if size_filter != 'Any Size' and not check_size_filter(entry_size_text, size_filter):
                return False
            
            # Version filter (if available)
            version_filter = criteria.get('version_filter', 'Any Version')
            if version_filter != 'Any Version':
                # Would need version detection logic here
                pass
        
        return True
        
    except Exception:
        return True  # Show entry if we can't parse it

def check_size_filter(size_text: str, size_filter: str) -> bool:
    """Check if entry size matches size filter"""
    try:
        # Parse size from text like "1,234 bytes" or "1.5 KB"
        if not size_text or size_text == "Unknown":
            return size_filter == "Any Size"
        
        # Extract numeric value and unit
        import re
        match = re.search(r'([\d,\.]+)\s*(bytes?|KB|MB|GB)?', size_text, re.IGNORECASE)
        if not match:
            return True
        
        size_value = float(match.group(1).replace(',', ''))
        unit = match.group(2).upper() if match.group(2) else 'BYTES'
        
        # Convert to bytes
        multipliers = {'BYTES': 1, 'KB': 1024, 'MB': 1024*1024, 'GB': 1024*1024*1024}
        size_bytes = size_value * multipliers.get(unit, 1)
        
        # Apply filter
        if size_filter == "< 1 KB":
            return size_bytes < 1024
        elif size_filter == "< 10 KB":
            return size_bytes < 10240
        elif size_filter == "< 100 KB":
            return size_bytes < 102400
        elif size_filter == "< 1 MB":
            return size_bytes < 1048576
        elif size_filter == "> 1 MB":
            return size_bytes > 1048576
        elif size_filter == "> 10 MB":
            return size_bytes > 10485760
        
        return True
        
    except Exception:
        return True

def show_extraction_dialog(main_window, selected_entries: List):
    """Show file extraction dialog"""
    try:
        if not selected_entries:
            QMessageBox.information(main_window, "No Selection", "Please select files to extract.")
            return
        
        dialog = ExtractionDialog(main_window, selected_entries, main_window)
        dialog.show()
        
    except Exception as e:
        QMessageBox.critical(main_window, "Error", f"Failed to open extraction dialog:\n{str(e)}")

# Update file statistics for filter
def update_filter_statistics(main_window):
    """Update file type statistics for the filter"""
    try:
        if not hasattr(main_window, 'file_filter'):
            return
        
        # Count file types in current IMG
        if hasattr(main_window, 'current_img') and main_window.current_img:
            type_counts = {}
            for entry in main_window.current_img.entries:
                ext = entry.name.split('.')[-1].upper() if '.' in entry.name else 'Unknown'
                type_counts[ext] = type_counts.get(ext, 0) + 1
            
            # Update filter options
            main_window.file_filter.update_filter_options(type_counts)
        
    except Exception as e:
        main_window.log_message(f"⚠️ Error updating filter statistics: {str(e)}")

# Export functions
__all__ = [
    'FileTypeFilter',
    'FileExtractionManager', 
    'ExtractionDialog',
    'integrate_file_filtering',
    'show_extraction_dialog',
    'update_filter_statistics'
]
