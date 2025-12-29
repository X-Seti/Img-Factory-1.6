#this belongs in methods/mirror_tab_shared.py - Version: 1
# X-Seti - August19 2025 - IMG Factory 1.5 - Shared Mirror Tab Selection System

"""
Shared Mirror Tab Selection System - Universal tab selection for all operations
Used by export_via, import_via, remove_via, split_via and other multi-tab operations
Solves tab content loss issues across all functions
"""

import os
from typing import Dict, List, Optional, Tuple, Any, Callable
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QListWidget, QListWidgetItem,
    QPushButton, QMessageBox, QGroupBox, QCheckBox, QTextEdit, QComboBox,
    QProgressDialog, QFileDialog, QRadioButton, QButtonGroup, QSpinBox
)
from PyQt6.QtCore import Qt, pyqtSignal, QThread
from PyQt6.QtGui import QFont, QIcon, QPixmap

##Methods list -
# collect_all_open_tabs
# create_mirror_tab_dialog
# get_selected_entries_from_tab
# get_tab_display_info
# get_tab_file_data
# show_mirror_tab_selection
# validate_tab_for_operation

##Classes -
# MirrorTabDialog
# TabOperationThread

class MirrorTabDialog(QDialog): #vers 1
    """Universal dialog for selecting tabs for any operation"""
    
    def __init__(self, main_window, available_tabs: List[Dict[str, Any]], operation_type: str):
        super().__init__(main_window)
        self.main_window = main_window
        self.available_tabs = available_tabs
        self.operation_type = operation_type.lower()
        self.selected_tab_data = None
        self.operation_options = {}
        self.setup_ui()
        
    def setup_ui(self): #vers 1
        """Setup universal tab selection dialog UI"""
        self.setWindowTitle(f"{self.operation_type.title()} - Select Tab")
        self.setModal(True)
        self.resize(550, 500)
        
        layout = QVBoxLayout(self)
        
        # Operation info header
        header_label = QLabel(f"Multiple tabs are open. Select which tab for {self.operation_type} operation:")
        header_label.setFont(QFont("", 10, QFont.Weight.Bold))
        layout.addWidget(header_label)
        
        # Tab list widget
        self.tab_list = QListWidget()
        self.populate_tab_list()
        layout.addWidget(self.tab_list)
        
        # Selected tab info display
        info_group = QGroupBox("Selected Tab Information")
        info_layout = QVBoxLayout(info_group)
        
        self.tab_info_text = QTextEdit()
        self.tab_info_text.setMaximumHeight(100)
        self.tab_info_text.setReadOnly(True)
        info_layout.addWidget(self.tab_info_text)
        
        layout.addWidget(info_group)
        
        # Operation-specific options
        self.options_group = self.create_operation_options()
        if self.options_group:
            layout.addWidget(self.options_group)
        
        # Buttons
        button_layout = QHBoxLayout()
        
        self.action_button = QPushButton(f"{self.operation_type.title()} Selected Tab")
        self.action_button.clicked.connect(self.accept_operation)
        self.action_button.setEnabled(False)
        
        cancel_button = QPushButton("Cancel")
        cancel_button.clicked.connect(self.reject)
        
        button_layout.addWidget(self.action_button)
        button_layout.addWidget(cancel_button)
        layout.addLayout(button_layout)
        
        # Connect signals
        self.tab_list.currentItemChanged.connect(self.on_tab_selection_changed)
        
    def create_operation_options(self) -> Optional[QGroupBox]: #vers 1
        """Create operation-specific option controls"""
        options_group = QGroupBox(f"{self.operation_type.title()} Options")
        options_layout = QVBoxLayout(options_group)
        
        if self.operation_type == 'export':
            # Basic export options
            self.export_selected_only_checkbox = QCheckBox("Export selected entries only")
            self.export_selected_only_checkbox.setChecked(True)
            options_layout.addWidget(self.export_selected_only_checkbox)
            
            self.individual_files_checkbox = QCheckBox("Export as individual files")
            self.individual_files_checkbox.setChecked(True)
            options_layout.addWidget(self.individual_files_checkbox)
            
            self.preserve_names_checkbox = QCheckBox("Preserve original filenames")
            self.preserve_names_checkbox.setChecked(True)
            options_layout.addWidget(self.preserve_names_checkbox)
            
        elif self.operation_type == 'export_via':
            # Export via options
            self.use_ide_checkbox = QCheckBox("Export via IDE definitions")
            self.use_ide_checkbox.setChecked(True)
            options_layout.addWidget(self.use_ide_checkbox)
            
            self.single_files_checkbox = QCheckBox("Export as individual files")
            self.single_files_checkbox.setChecked(True)
            options_layout.addWidget(self.single_files_checkbox)
            
            self.include_textures_checkbox = QCheckBox("Include texture references")
            options_layout.addWidget(self.include_textures_checkbox)
            
        elif self.operation_type == 'import':
            # Basic import options
            self.import_files_button = QPushButton("Select Files to Import")
            self.import_files_button.clicked.connect(self.select_import_files)
            options_layout.addWidget(self.import_files_button)
            
            self.selected_files_label = QLabel("No files selected")
            options_layout.addWidget(self.selected_files_label)
            
            self.overwrite_existing_checkbox = QCheckBox("Overwrite existing entries")
            options_layout.addWidget(self.overwrite_existing_checkbox)
            
            self.auto_detect_type_checkbox = QCheckBox("Auto-detect file types")
            self.auto_detect_type_checkbox.setChecked(True)
            options_layout.addWidget(self.auto_detect_type_checkbox)
            
        elif self.operation_type == 'import_via':
            # Import via options
            self.import_mode_group = QButtonGroup()
            
            self.import_files_radio = QRadioButton("Import selected files")
            self.import_files_radio.setChecked(True)
            self.import_mode_group.addButton(self.import_files_radio)
            options_layout.addWidget(self.import_files_radio)
            
            self.import_folder_radio = QRadioButton("Import entire folder")
            self.import_mode_group.addButton(self.import_folder_radio)
            options_layout.addWidget(self.import_folder_radio)
            
            self.import_ide_radio = QRadioButton("Import via IDE file")
            self.import_mode_group.addButton(self.import_ide_radio)
            options_layout.addWidget(self.import_ide_radio)
            
            self.overwrite_checkbox = QCheckBox("Overwrite existing entries")
            options_layout.addWidget(self.overwrite_checkbox)
            
        elif self.operation_type == 'remove':
            # Basic remove options
            self.remove_selected_only_checkbox = QCheckBox("Remove selected entries only")
            self.remove_selected_only_checkbox.setChecked(True)
            options_layout.addWidget(self.remove_selected_only_checkbox)
            
            self.create_backup_checkbox = QCheckBox("Create backup before removal")
            self.create_backup_checkbox.setChecked(True)
            options_layout.addWidget(self.create_backup_checkbox)
            
            self.confirm_removal_checkbox = QCheckBox("Confirm each removal")
            options_layout.addWidget(self.confirm_removal_checkbox)
            
        elif self.operation_type == 'remove_via':
            # Remove via options
            self.remove_mode_group = QButtonGroup()
            
            self.remove_selected_radio = QRadioButton("Remove selected entries only")
            self.remove_selected_radio.setChecked(True)
            self.remove_mode_group.addButton(self.remove_selected_radio)
            options_layout.addWidget(self.remove_selected_radio)
            
            self.remove_pattern_radio = QRadioButton("Remove by name pattern")
            self.remove_mode_group.addButton(self.remove_pattern_radio)
            options_layout.addWidget(self.remove_pattern_radio)
            
            self.backup_checkbox = QCheckBox("Create backup before removal")
            self.backup_checkbox.setChecked(True)
            options_layout.addWidget(self.backup_checkbox)
            
        elif self.operation_type == 'dump':
            # Basic dump options
            self.dump_selected_only_checkbox = QCheckBox("Dump selected entries only")
            self.dump_selected_only_checkbox.setChecked(True)
            options_layout.addWidget(self.dump_selected_only_checkbox)
            
            self.dump_format_combo = QComboBox()
            self.dump_format_combo.addItems(["Individual Files", "Combined File", "Organized by Type"])
            options_layout.addWidget(QLabel("Dump format:"))
            options_layout.addWidget(self.dump_format_combo)
            
            self.include_metadata_checkbox = QCheckBox("Include file metadata")
            self.include_metadata_checkbox.setChecked(True)
            options_layout.addWidget(self.include_metadata_checkbox)
            
        elif self.operation_type == 'split_via':
            # Split via options
            self.split_method_group = QButtonGroup()
            
            self.split_size_radio = QRadioButton("Split by file size")
            self.split_size_radio.setChecked(True)
            self.split_method_group.addButton(self.split_size_radio)
            options_layout.addWidget(self.split_size_radio)
            
            self.split_count_radio = QRadioButton("Split by entry count")
            self.split_method_group.addButton(self.split_count_radio)
            options_layout.addWidget(self.split_count_radio)
            
            # Split size/count input
            split_input_layout = QHBoxLayout()
            split_input_layout.addWidget(QLabel("Target size (MB) / Entry count:"))
            self.split_value_spinbox = QSpinBox()
            self.split_value_spinbox.setRange(1, 9999)
            self.split_value_spinbox.setValue(50)
            split_input_layout.addWidget(self.split_value_spinbox)
            options_layout.addLayout(split_input_layout)
            
        elif self.operation_type == 'dump_via':
            # Dump via options
            self.dump_format_combo = QComboBox()
            self.dump_format_combo.addItems(["Individual Files", "Combined File", "Organized by Type"])
            options_layout.addWidget(QLabel("Dump format:"))
            options_layout.addWidget(self.dump_format_combo)
            
            self.include_metadata_checkbox = QCheckBox("Include file metadata")
            self.include_metadata_checkbox.setChecked(True)
            options_layout.addWidget(self.include_metadata_checkbox)
            
        else:
            # Generic options for other operations
            self.confirm_checkbox = QCheckBox(f"Confirm {self.operation_type} operation")
            self.confirm_checkbox.setChecked(True)
            options_layout.addWidget(self.confirm_checkbox)
            
        return options_group if options_layout.count() > 0 else None
    
    def select_import_files(self): #vers 1
        """Handle import file selection for basic import operation"""
        try:
            file_dialog = QFileDialog(self)
            file_dialog.setFileMode(QFileDialog.FileMode.ExistingFiles)
            file_dialog.setNameFilter("GTA Files (*.dff *.txd *.col *.ifp *.wav);;All Files (*)")
            
            if file_dialog.exec():
                selected_files = file_dialog.selectedFiles()
                if selected_files:
                    file_count = len(selected_files)
                    self.selected_files_label.setText(f"{file_count} files selected")
                    # Store selected files for later use
                    self.import_files_list = selected_files
                else:
                    self.selected_files_label.setText("No files selected")
                    self.import_files_list = []
        except Exception as e:
            self.selected_files_label.setText("Error selecting files")
            self.import_files_list = []
        
    def populate_tab_list(self): #vers 1
        """Populate the tab list with available tabs"""
        for i, tab_data in enumerate(self.available_tabs):
            item = QListWidgetItem()
            
            # Create display text based on operation and file type
            tab_name = tab_data.get('name', f'Tab {i+1}')
            file_type = tab_data.get('file_type', 'Unknown')
            entry_count = tab_data.get('entry_count', 0)
            file_path = tab_data.get('file_path', 'No file')
            selected_count = len(tab_data.get('selected_entries', []))
            
            # Operation-specific display
            if self.operation_type in ['export_via', 'dump_via'] and selected_count > 0:
                display_text = f"{tab_name} ({file_type}) - {selected_count} selected of {entry_count}"
            else:
                display_text = f"{tab_name} ({file_type}) - {entry_count} entries"
                
            item.setText(display_text)
            
            # Store tab data in item
            item.setData(Qt.ItemDataRole.UserRole, tab_data)
            
            # Add tooltip with operation context
            if self.operation_type == 'export_via':
                tooltip = f"Export from: {os.path.basename(file_path)}\nEntries: {entry_count}"
                if selected_count > 0:
                    tooltip += f"\nSelected: {selected_count}"
            elif self.operation_type == 'import_via':
                tooltip = f"Import into: {os.path.basename(file_path)}\nCurrent entries: {entry_count}"
            elif self.operation_type == 'remove_via':
                tooltip = f"Remove from: {os.path.basename(file_path)}\nSelected: {selected_count} of {entry_count}"
            elif self.operation_type == 'split_via':
                tooltip = f"Split file: {os.path.basename(file_path)}\nEntries: {entry_count}"
            else:
                tooltip = f"File: {os.path.basename(file_path)}\nEntries: {entry_count}"
                
            item.setToolTip(tooltip)
            self.tab_list.addItem(item)
            
    def on_tab_selection_changed(self, current, previous): #vers 1
        """Handle tab selection change"""
        if current:
            tab_data = current.data(Qt.ItemDataRole.UserRole)
            self.selected_tab_data = tab_data
            self.update_tab_info_display(tab_data)
            self.action_button.setEnabled(True)
        else:
            self.selected_tab_data = None
            self.action_button.setEnabled(False)
            
    def update_tab_info_display(self, tab_data: Dict[str, Any]): #vers 1
        """Update the tab information display"""
        info_text = []
        info_text.append(f"Tab Name: {tab_data.get('name', 'Unknown')}")
        info_text.append(f"File Type: {tab_data.get('file_type', 'Unknown')}")
        info_text.append(f"File Path: {tab_data.get('file_path', 'No file')}")
        info_text.append(f"Total Entries: {tab_data.get('entry_count', 0)}")
        
        if tab_data.get('file_size'):
            info_text.append(f"File Size: {tab_data['file_size']}")
            
        selected_entries = tab_data.get('selected_entries', [])
        if selected_entries:
            info_text.append(f"Selected Entries: {len(selected_entries)}")
            
        # Operation-specific info
        if self.operation_type == 'export_via' and selected_entries:
            info_text.append(f"Will export: {len(selected_entries)} selected entries")
        elif self.operation_type == 'import_via':
            info_text.append("Target for import operation")
        elif self.operation_type == 'remove_via' and selected_entries:
            info_text.append(f"Will remove: {len(selected_entries)} selected entries")
        elif self.operation_type == 'split_via':
            info_text.append("Will be split into multiple files")
            
        self.tab_info_text.setText('\n'.join(info_text))
        
    def accept_operation(self): #vers 1
        """Accept and gather operation options"""
        if not self.selected_tab_data:
            QMessageBox.warning(self, "No Selection", "Please select a tab for the operation.")
            return
            
        # Gather operation-specific options
        self.operation_options = self.gather_operation_options()
        self.accept()
        
    def gather_operation_options(self) -> Dict[str, Any]: #vers 1
        """Gather options based on operation type"""
        options = {}
        
        if self.operation_type == 'export':
            options['selected_only'] = getattr(self, 'export_selected_only_checkbox', QCheckBox()).isChecked()
            options['individual_files'] = getattr(self, 'individual_files_checkbox', QCheckBox()).isChecked()
            options['preserve_names'] = getattr(self, 'preserve_names_checkbox', QCheckBox()).isChecked()
            
        elif self.operation_type == 'export_via':
            options['use_ide'] = getattr(self, 'use_ide_checkbox', QCheckBox()).isChecked()
            options['single_files'] = getattr(self, 'single_files_checkbox', QCheckBox()).isChecked()
            options['include_textures'] = getattr(self, 'include_textures_checkbox', QCheckBox()).isChecked()
            
        elif self.operation_type == 'import':
            options['import_files'] = getattr(self, 'import_files_list', [])
            options['overwrite_existing'] = getattr(self, 'overwrite_existing_checkbox', QCheckBox()).isChecked()
            options['auto_detect_type'] = getattr(self, 'auto_detect_type_checkbox', QCheckBox()).isChecked()
            
        elif self.operation_type == 'import_via':
            if hasattr(self, 'import_files_radio') and self.import_files_radio.isChecked():
                options['import_mode'] = 'files'
            elif hasattr(self, 'import_folder_radio') and self.import_folder_radio.isChecked():
                options['import_mode'] = 'folder'
            elif hasattr(self, 'import_ide_radio') and self.import_ide_radio.isChecked():
                options['import_mode'] = 'ide'
            else:
                options['import_mode'] = 'files'
            options['overwrite'] = getattr(self, 'overwrite_checkbox', QCheckBox()).isChecked()
            
        elif self.operation_type == 'remove':
            options['selected_only'] = getattr(self, 'remove_selected_only_checkbox', QCheckBox()).isChecked()
            options['create_backup'] = getattr(self, 'create_backup_checkbox', QCheckBox()).isChecked()
            options['confirm_removal'] = getattr(self, 'confirm_removal_checkbox', QCheckBox()).isChecked()
            
        elif self.operation_type == 'remove_via':
            if hasattr(self, 'remove_selected_radio') and self.remove_selected_radio.isChecked():
                options['remove_mode'] = 'selected'
            else:
                options['remove_mode'] = 'pattern'
            options['backup'] = getattr(self, 'backup_checkbox', QCheckBox()).isChecked()
            
        elif self.operation_type == 'dump':
            options['selected_only'] = getattr(self, 'dump_selected_only_checkbox', QCheckBox()).isChecked()
            combo = getattr(self, 'dump_format_combo', QComboBox())
            options['dump_format'] = combo.currentText().lower().replace(' ', '_')
            options['include_metadata'] = getattr(self, 'include_metadata_checkbox', QCheckBox()).isChecked()
            
        elif self.operation_type == 'split_via':
            if hasattr(self, 'split_size_radio') and self.split_size_radio.isChecked():
                options['split_method'] = 'size'
            else:
                options['split_method'] = 'count'
            options['split_value'] = getattr(self, 'split_value_spinbox', QSpinBox()).value()
            
        elif self.operation_type == 'dump_via':
            combo = getattr(self, 'dump_format_combo', QComboBox())
            options['dump_format'] = combo.currentText().lower().replace(' ', '_')
            options['include_metadata'] = getattr(self, 'include_metadata_checkbox', QCheckBox()).isChecked()
            
        return options


def collect_all_open_tabs(main_window) -> List[Dict[str, Any]]: #vers 1
    """Collect information about all currently open tabs"""
    available_tabs = []
    
    try:
        if not hasattr(main_window, 'main_tab_widget'):
            return available_tabs
            
        tab_widget = main_window.main_tab_widget
        
        for i in range(tab_widget.count()):
            tab = tab_widget.widget(i)
            tab_name = tab_widget.tabText(i)
            
            # Get tab data
            tab_data = get_tab_file_data(tab, tab_name, i)
            if tab_data:
                available_tabs.append(tab_data)
                
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error collecting tabs: {str(e)}")
            
    return available_tabs


def get_tab_file_data(tab_widget, tab_name: str, tab_index: int) -> Optional[Dict[str, Any]]: #vers 1
    """Extract file data from a specific tab widget"""
    try:
        tab_data = {
            'index': tab_index,
            'name': tab_name,
            'widget': tab_widget
        }
        
        # Check for IMG file
        if hasattr(tab_widget, 'img_file') and tab_widget.img_file:
            img_file = tab_widget.img_file
            tab_data.update({
                'file_type': 'IMG',
                'file_object': img_file,
                'file_path': getattr(img_file, 'file_path', 'Unknown'),
                'entry_count': len(getattr(img_file, 'entries', [])),
                'selected_entries': get_selected_entries_from_tab(tab_widget),
                'file_size': get_file_size_display(getattr(img_file, 'file_path', ''))
            })
            
        # Check for COL file
        elif hasattr(tab_widget, 'col_file') and tab_widget.col_file:
            col_file = tab_widget.col_file
            tab_data.update({
                'file_type': 'COL',
                'file_object': col_file,
                'file_path': getattr(col_file, 'file_path', 'Unknown'),
                'entry_count': len(getattr(col_file, 'models', [])),
                'selected_entries': get_selected_entries_from_tab(tab_widget),
                'file_size': get_file_size_display(getattr(col_file, 'file_path', ''))
            })
            
        else:
            # No valid file found in this tab
            return None
            
        return tab_data
        
    except Exception as e:
        return None


def get_selected_entries_from_tab(tab_widget) -> List[int]: #vers 1
    """Get selected entries from tab's table widget"""
    selected_entries = []
    
    try:
        # Look for table widget in tab
        if hasattr(tab_widget, 'table') and tab_widget.table:
            table = tab_widget.table
            selected_rows = table.selectionModel().selectedRows()
            selected_entries = [row.row() for row in selected_rows]
            
        # Alternative: check for gui_layout table
        elif hasattr(tab_widget, 'gui_layout') and hasattr(tab_widget.gui_layout, 'table'):
            table = tab_widget.gui_layout.table
            selected_rows = table.selectionModel().selectedRows()
            selected_entries = [row.row() for row in selected_rows]
            
    except Exception:
        pass
        
    return selected_entries


def get_file_size_display(file_path: str) -> str: #vers 1
    """Get human-readable file size"""
    try:
        if file_path and os.path.exists(file_path):
            size = os.path.getsize(file_path)
            if size < 1024:
                return f"{size} B"
            elif size < 1024 * 1024:
                return f"{size / 1024:.1f} KB"
            else:
                return f"{size / (1024 * 1024):.1f} MB"
    except:
        pass
    return "Unknown"


def show_mirror_tab_selection(main_window, operation_type: str) -> Tuple[Optional[Dict[str, Any]], Dict[str, Any]]: #vers 1
    """
    Show mirror tab selection dialog for any operation
    
    Args:
        main_window: Main application window
        operation_type: Type of operation (export_via, import_via, remove_via, split_via, etc.)
        
    Returns:
        Tuple of (selected_tab_data, operation_options) or (None, {}) if cancelled
    """
    try:
        # Collect all available tabs
        available_tabs = collect_all_open_tabs(main_window)
        
        if not available_tabs:
            QMessageBox.warning(main_window, "No Files", 
                              "No IMG or COL files are currently open.")
            return None, {}
            
        # If only one tab, return it directly (no dialog needed)
        if len(available_tabs) == 1:
            tab_data = available_tabs[0]
            
            # Validate tab for operation
            if validate_tab_for_operation(tab_data, operation_type):
                main_window.log_message(f"📋 Single tab {operation_type}: {tab_data['name']}")
                return tab_data, {}
            else:
                QMessageBox.warning(main_window, "Invalid Tab", 
                                  f"The open tab is not suitable for {operation_type} operation.")
                return None, {}
            
        # Multiple tabs - show mirror selection dialog
        main_window.log_message(f"📋 Multiple tabs detected ({len(available_tabs)}), showing {operation_type} selection dialog")
        
        dialog = MirrorTabDialog(main_window, available_tabs, operation_type)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            
            selected_tab = dialog.selected_tab_data
            operation_options = dialog.operation_options
            
            main_window.log_message(f"🚀 {operation_type.title()} selected tab: {selected_tab['name']}")
            return selected_tab, operation_options
            
        else:
            main_window.log_message(f"🚫 {operation_type.title()} cancelled by user")
            return None, {}
            
    except Exception as e:
        main_window.log_message(f"❌ Mirror tab selection error: {str(e)}")
        QMessageBox.critical(main_window, "Selection Error", f"Tab selection failed: {str(e)}")
        return None, {}


def validate_tab_for_operation(tab_data: Dict[str, Any], operation_type: str) -> bool: #vers 1
    """Validate if tab is suitable for the specified operation"""
    try:
        file_type = tab_data.get('file_type', '')
        entry_count = tab_data.get('entry_count', 0)
        selected_entries = tab_data.get('selected_entries', [])
        
        if operation_type in ['export', 'export_via', 'dump']:
            # Need entries to export/dump
            return entry_count > 0
            
        elif operation_type in ['import', 'import_via']:
            # Can import into any valid file
            return file_type in ['IMG', 'COL']
            
        elif operation_type in ['remove', 'remove_via']:
            # Need selected entries to remove (or entries for pattern removal)
            if operation_type == 'remove':
                return len(selected_entries) > 0  # Basic remove needs selection
            else:
                return entry_count > 0  # Remove via can use patterns
            
        elif operation_type == 'split_via':
            # Need sufficient entries to split
            return entry_count > 1
            
        else:
            # Generic validation
            return entry_count > 0
            
    except Exception:
        return False


__all__ = [
    'show_mirror_tab_selection',
    'collect_all_open_tabs',
    'MirrorTabDialog',
    'validate_tab_for_operation',
    'get_tab_file_data'
]