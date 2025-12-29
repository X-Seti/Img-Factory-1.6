#this belongs in gui/ide_dialog.py - Version: 2
# X-Seti - September09 2025 - IMG Factory 1.5 - Universal IDE Selection Dialog

"""
Universal IDE Selection Dialog - FIXED: Added proper Choose Export Folder button connection
Used by: import_via, export_via, remove_via, split_via
Provides consistent IDE file selection and parsing across all "via" operations
"""

import os
from typing import Optional, Callable, Dict, Any
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QGroupBox, QLabel, QPushButton,
    QLineEdit, QTextEdit, QListWidget, QListWidgetItem, QMessageBox,
    QProgressBar, QCheckBox, QFileDialog, QSplitter
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal
from PyQt6.QtGui import QFont

from apps.methods.ide_parser_functions import IDEParser

##Methods list -
# create_ide_dialog
# integrate_ide_dialog

##Classes -
# IDEDialog
# IDEParseThread

class IDEParseThread(QThread):
    """Background thread for parsing IDE files"""
    
    parsing_progress = pyqtSignal(int, str)  # progress %, message
    parsing_completed = pyqtSignal(bool, str)  # success, message
    
    def __init__(self, ide_path: str):
        super().__init__()
        self.ide_path = ide_path
        self.parser = IDEParser()
        
    def run(self):
        """Run IDE parsing in background"""
        try:
            self.parsing_progress.emit(10, "Reading IDE file...")
            
            if self.parser.parse_ide_file(self.ide_path):
                self.parsing_progress.emit(100, "IDE parsing completed!")
                self.parsing_completed.emit(True, "IDE file parsed successfully")
            else:
                error_msg = "Failed to parse IDE file"
                if self.parser.parse_stats['errors']:
                    error_msg += f": {self.parser.parse_stats['errors'][0]}"
                self.parsing_completed.emit(False, error_msg)
                
        except Exception as e:
            self.parsing_completed.emit(False, f"IDE parsing error: {str(e)}")

class IDEDialog(QDialog):
    """Universal IDE selection and parsing dialog"""
    
    def __init__(self, parent=None, title="Select IDE File", operation="process"):
        super().__init__(parent)
        self.operation = operation  # "import", "export", "remove", "split", etc.
        self.ide_parser = None
        self.parse_thread = None
        
        # FIXED: Add export folder tracking
        self.export_folder = None
        self.result_choice = None
        
        self.setWindowTitle(title)
        self.setMinimumSize(600, 500)
        self.setup_ui()
        
    def setup_ui(self): #vers 1
        """Setup dialog UI"""
        layout = QVBoxLayout(self)
        
        # Create splitter for main content
        splitter = QSplitter(Qt.Orientation.Vertical)
        layout.addWidget(splitter)
        
        # IDE file selection
        self.setup_ide_selection(splitter)
        
        # Analysis results  
        self.setup_analysis_results(splitter)
        
        # Model relationships
        self.setup_relationships_view(splitter)
        
        # Progress bar
        self.progress_bar = QProgressBar()
        self.progress_bar.setVisible(False)
        layout.addWidget(self.progress_bar)
        
        # Buttons
        self.setup_buttons(layout)
        
        # Set splitter proportions
        splitter.setSizes([150, 200, 150])
        
    def setup_ide_selection(self, parent): #vers 1
        """Setup IDE file selection section"""
        ide_group = QGroupBox("IDE File Selection")
        ide_layout = QVBoxLayout(ide_group)
        
        # File path input
        file_layout = QHBoxLayout()
        
        self.ide_path_input = QLineEdit()
        self.ide_path_input.setPlaceholderText("Select an IDE file...")
        file_layout.addWidget(self.ide_path_input)
        
        self.browse_btn = QPushButton("Browse...")
        self.browse_btn.clicked.connect(self.browse_ide_file)
        file_layout.addWidget(self.browse_btn)
        
        ide_layout.addLayout(file_layout)
        
        # Parse button
        button_layout = QHBoxLayout()
        
        self.parse_btn = QPushButton("📋 Parse IDE File")
        self.parse_btn.clicked.connect(self.parse_ide_file)
        self.parse_btn.setEnabled(False)
        button_layout.addWidget(self.parse_btn)
        
        # Auto-parse checkbox
        self.auto_parse_cb = QCheckBox("Auto-parse when file selected")
        self.auto_parse_cb.setChecked(True)
        button_layout.addWidget(self.auto_parse_cb)
        
        button_layout.addStretch()
        ide_layout.addLayout(button_layout)
        
        # Status label
        self.status_label = QLabel("No IDE file selected")
        self.status_label.setStyleSheet("color: #666; font-style: italic;")
        ide_layout.addWidget(self.status_label)
        
        parent.addWidget(ide_group)
        
        # Connect file path changes
        self.ide_path_input.textChanged.connect(self.on_file_path_changed)
        
    def setup_analysis_results(self, parent): #vers 1
        """Setup analysis results section"""
        analysis_group = QGroupBox("Analysis Results")
        analysis_layout = QVBoxLayout(analysis_group)
        
        self.analysis_text = QTextEdit()
        self.analysis_text.setReadOnly(True)
        self.analysis_text.setFont(QFont("Courier", 9))
        self.analysis_text.setMaximumHeight(150)
        self.analysis_text.setPlaceholderText("IDE analysis results will appear here...")
        analysis_layout.addWidget(self.analysis_text)
        
        parent.addWidget(analysis_group)
        
    def setup_relationships_view(self, parent): #vers 1
        """Setup model relationships view"""
        relationships_group = QGroupBox("Model → Texture Relationships")
        relationships_layout = QVBoxLayout(relationships_group)
        
        self.relationships_list = QListWidget()
        self.relationships_list.setFont(QFont("Courier", 9))
        relationships_layout.addWidget(self.relationships_list)
        
        # Filter options
        filter_layout = QHBoxLayout()
        
        self.show_models_cb = QCheckBox("Show models")
        self.show_models_cb.setChecked(True)
        self.show_models_cb.toggled.connect(self.update_relationships_view)
        filter_layout.addWidget(self.show_models_cb)
        
        self.show_textures_cb = QCheckBox("Show textures")
        self.show_textures_cb.setChecked(True)
        self.show_textures_cb.toggled.connect(self.update_relationships_view)
        filter_layout.addWidget(self.show_textures_cb)
        
        filter_layout.addStretch()
        relationships_layout.addLayout(filter_layout)
        
        parent.addWidget(relationships_group)
        
    def setup_buttons(self, layout): #vers 2
        """Setup dialog buttons - FIXED: Properly connect Choose Export Folder button"""
        button_layout = QHBoxLayout()
        
        # FIXED: Only show Choose Export Folder for import/export operations
        if self.operation in ('import', 'export'):
            self.choose_folder_btn = QPushButton("Choose Export Folder")
            # FIXED: Remove hardcoded green color to match theme
            self.choose_folder_btn.setStyleSheet("QPushButton { padding: 8px; font-weight: bold; }")
            self.choose_folder_btn.setToolTip("Choose a custom folder for export destination")
            self.choose_folder_btn.clicked.connect(self.choose_export_folder)  # FIXED: Connect the button
            button_layout.addWidget(self.choose_folder_btn)

        # Operation-specific button
        operation_text = {
            'import': 'Import with IDE',
            'export': 'Export with IDE',
            'remove': 'Remove with IDE',
            'split': 'Split with IDE'
        }.get(self.operation, f'✅ Continue with IDE')
        
        self.continue_btn = QPushButton(operation_text)
        self.continue_btn.clicked.connect(self.accept)
        self.continue_btn.setEnabled(False)
        self.continue_btn.setDefault(True)
        
        self.cancel_btn = QPushButton("❌ Cancel")
        self.cancel_btn.clicked.connect(self.reject)

        button_layout.addStretch()
        button_layout.addWidget(self.continue_btn)
        button_layout.addWidget(self.cancel_btn)
        
        layout.addLayout(button_layout)

    def choose_export_folder(self): #vers 2
        """FIXED: Choose export folder dialog - properly implemented"""
        try:
            folder = QFileDialog.getExistingDirectory(
                self,
                "Select Export Folder",
                "",
                QFileDialog.Option.ShowDirsOnly
            )
            if folder:
                self.result_choice = 'choose_folder'
                self.export_folder = folder
                
                # Update the button text to show selected folder
                folder_name = os.path.basename(folder) if folder else "Export Folder"
                self.choose_folder_btn.setText(f"{folder_name}")
                self.choose_folder_btn.setToolTip(f"Export folder: {folder}")
                
                # Update status
                self.status_label.setText(f"Export folder: {folder}")
                
                # Auto-accept the dialog when folder is chosen
                if self.ide_parser:  # Only auto-accept if IDE is already parsed
                    self.accept()
                    
        except Exception as e:
            QMessageBox.critical(self, "Folder Selection Error", f"Failed to select folder: {str(e)}")

    def browse_ide_file(self): #vers 1
        """Browse for IDE file"""
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Select IDE File", "", "IDE Files (*.ide);;All Files (*)"
        )
        
        if file_path:
            self.ide_path_input.setText(file_path)
            
    def on_file_path_changed(self): #vers 1
        """Handle file path changes"""
        file_path = self.ide_path_input.text().strip()
        
        if file_path and os.path.exists(file_path):
            self.parse_btn.setEnabled(True)
            self.status_label.setText(f"{os.path.basename(file_path)}")
            self.status_label.setStyleSheet("color: #333;")
            
            # Auto-parse if enabled
            if self.auto_parse_cb.isChecked():
                self.parse_ide_file()
        else:
            self.parse_btn.setEnabled(False)
            self.continue_btn.setEnabled(False)
            self.status_label.setText("No IDE file selected")
            self.status_label.setStyleSheet("color: #666; font-style: italic;")
            
    def parse_ide_file(self): #vers 1
        """Parse selected IDE file"""
        file_path = self.ide_path_input.text().strip()
        
        if not file_path or not os.path.exists(file_path):
            QMessageBox.warning(self, "Invalid File", "Please select a valid IDE file")
            return
            
        # Disable UI during parsing
        self.parse_btn.setEnabled(False)
        self.continue_btn.setEnabled(False)
        self.progress_bar.setVisible(True)
        self.status_label.setText("⏳ Parsing IDE file...")
        
        # Start parsing thread
        self.parse_thread = IDEParseThread(file_path)
        self.parse_thread.parsing_progress.connect(self.update_parsing_progress)
        self.parse_thread.parsing_completed.connect(self.parsing_finished)
        self.parse_thread.start()
        
    def update_parsing_progress(self, progress: int, message: str): #vers 1
        """Update parsing progress"""
        self.progress_bar.setValue(progress)
        self.status_label.setText(message)
        
    def parsing_finished(self, success: bool, message: str): #vers 1
        """Handle parsing completion"""
        self.progress_bar.setVisible(False)
        self.parse_btn.setEnabled(True)
        
        if success and self.parse_thread:
            # Store parser results
            self.ide_parser = self.parse_thread.parser
            
            # Update UI with results
            self.analysis_text.setText(self.ide_parser.get_summary())
            self.status_label.setText(self.ide_parser.get_status_text())
            self.status_label.setStyleSheet("color: #2e7d32; font-weight: bold;")
            
            # Update relationships view
            self.update_relationships_view()
            
            # Enable continue button
            self.continue_btn.setEnabled(True)
            
        else:
            # Show error
            self.status_label.setText("❌ IDE parsing failed")
            self.status_label.setStyleSheet("color: #d32f2f; font-weight: bold;")
            QMessageBox.critical(self, "Parsing Error", message)
            
    def update_relationships_view(self): #vers 1
        """Update the relationships list view"""
        if not self.ide_parser:
            return
            
        self.relationships_list.clear()
        
        show_models = self.show_models_cb.isChecked()
        show_textures = self.show_textures_cb.isChecked()
        
        if show_models:
            # Show model → texture relationships
            relationships = self.ide_parser.get_model_relationships()
            for model, textures in relationships.items():
                item_text = f"🎮 {model} → {', '.join(textures)}"
                item = QListWidgetItem(item_text)
                item.setData(Qt.ItemDataRole.UserRole, {'type': 'model', 'name': model})
                self.relationships_list.addItem(item)
        
        if show_textures:
            # Show texture → models relationships  
            texture_relationships = self.ide_parser.get_texture_relationships()
            for texture, models in texture_relationships.items():
                item_text = f"{texture} ← {', '.join(models)}"
                item = QListWidgetItem(item_text)
                item.setData(Qt.ItemDataRole.UserRole, {'type': 'texture', 'name': texture})
                self.relationships_list.addItem(item)
                
    def get_ide_parser(self) -> Optional[IDEParser]: #vers 1
        """Get the parsed IDE data
        
        Returns:
            IDEParser instance if parsing succeeded, None otherwise
        """
        return self.ide_parser
    
    def get_export_folder(self) -> Optional[str]: #vers 1
        """Get the selected export folder
        
        Returns:
            Export folder path if selected, None otherwise
        """
        return self.export_folder
    
    def get_result_choice(self) -> Optional[str]: #vers 1
        """Get the user's choice result
        
        Returns:
            Result choice string ('choose_folder', etc.) or None
        """
        return self.result_choice
    
    def get_selected_relationships(self) -> Dict[str, Any]: #vers 1
        """Get selected relationships from the list
        
        Returns:
            Dict with relationship data
        """
        selected_items = self.relationships_list.selectedItems()
        relationships = {
            'models': [],
            'textures': [],
            'files': []
        }
        
        if not self.ide_parser:
            return relationships
            
        for item in selected_items:
            data = item.data(Qt.ItemDataRole.UserRole)
            if data['type'] == 'model':
                model_name = data['name']
                relationships['models'].append(model_name)
                relationships['files'].extend(self.ide_parser.get_files_for_model(model_name))
            elif data['type'] == 'texture':
                texture_name = data['name']  
                relationships['textures'].append(texture_name)
                relationships['files'].extend(self.ide_parser.get_files_for_texture(texture_name))
        
        # Remove duplicates
        relationships['files'] = list(set(relationships['files']))
        
        return relationships

def create_ide_dialog(parent=None, operation: str = "process", title: str = None) -> IDEDialog: #vers 1
    """Create IDE dialog for specific operation
    
    Args:
        parent: Parent widget
        operation: Operation type ("import", "export", "remove", "split")
        title: Custom dialog title
        
    Returns:
        IDEDialog instance
    """
    if not title:
        title_map = {
            'import': 'Import via IDE File',
            'export': 'Export via IDE File', 
            'remove': 'Remove via IDE File',
            'split': 'Split via IDE File'
        }
        title = title_map.get(operation, 'Select IDE File')
    
    return IDEDialog(parent, title, operation)

def show_ide_dialog(parent=None, operation: str = "process") -> Optional[IDEParser]: #vers 2
    """Show IDE dialog and return parser if successful - FIXED: Return dialog result info
    
    Args:
        parent: Parent widget
        operation: Operation type
        
    Returns:
        IDEParser instance if dialog accepted and parsing succeeded, None otherwise
    """
    dialog = create_ide_dialog(parent, operation)
    
    if dialog.exec() == QDialog.DialogCode.Accepted:
        # FIXED: Attach export folder info to parser for access by export_via
        parser = dialog.get_ide_parser()
        if parser:
            parser.selected_export_folder = dialog.get_export_folder()
            parser.dialog_choice = dialog.get_result_choice()
        return parser
    
    return None

def integrate_ide_dialog(main_window): #vers 1
    """Integrate IDE dialog functions into main window
    
    Args:
        main_window: Main window instance
        
    Returns:
        bool: True if integration succeeded
    """
    try:
        # Add dialog functions to main window
        main_window.create_ide_dialog = lambda op="process": create_ide_dialog(main_window, op)
        main_window.show_ide_dialog = lambda op="process": show_ide_dialog(main_window, op)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("✅ IDE dialog functions integrated")
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error integrating IDE dialog: {str(e)}")
        return False

__all__ = [
    'IDEDialog',
    'IDEParseThread',
    'create_ide_dialog',
    'show_ide_dialog', 
    'integrate_ide_dialog'
]