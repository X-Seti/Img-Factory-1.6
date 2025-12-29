#this belongs in apps/core/creator.py - Version: 17
# X-Seti - August27 2025 - IMG Factory 1.5 - IMG Creator Dialog UI Only
# Credit MexUK 2007 IMG Factory 1.2

"""
IMG Creator - Dialog UI Component Only
All creation logic moved to core/create_img.py
"""
import os
import shutil
from typing import Optional, Dict, Any, List, Tuple
from PyQt6.QtCore import QThread, pyqtSignal, Qt
from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton, 
                           QLabel, QComboBox, QSpinBox, QCheckBox, QLineEdit, 
                           QFileDialog, QProgressBar, QGroupBox, QGridLayout,
                           QMessageBox, QTextEdit, QApplication, QProgressDialog)

from PyQt6.QtGui import QFont
from pathlib import Path

# Import base creation functions from core
from apps.methods.img_core_classes import IMGVersion
from apps.core.img_formats import GameSpecificIMGDialog, IMGCreator

##Methods list -
# create_new_img_dialog
# get_default_output_path
# show_creation_dialog
#create_new_img
#detect_and_open_file
#open_file_dialog #old
#detect_file_type

##Classes -
# NewIMGDialog

#TODO - Create new tab for new created img.

class NewIMGDialog(QDialog): #vers 10
    """New IMG Creation Dialog - UI ONLY"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Create New IMG Archive")
        self.setModal(True)
        self.setFixedSize(500, 400)
        
        self.output_path = ""
        self.creation_thread = None
        
        self.setup_ui()
        self.load_default_settings()
        
    def setup_ui(self):
        """Setup dialog UI"""
        layout = QVBoxLayout(self)
        
        # Title
        title_label = QLabel("ðŸ†• Create New IMG Archive")
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_font = QFont()
        title_font.setPointSize(14)
        title_font.setBold(True)
        title_label.setFont(title_font)
        layout.addWidget(title_label)
        
        # File path group
        path_group = QGroupBox("Output File")
        path_layout = QVBoxLayout(path_group)
        
        path_row = QHBoxLayout()
        self.path_edit = QLineEdit()
        self.path_edit.setPlaceholderText("Select output path...")
        path_row.addWidget(self.path_edit)
        
        browse_btn = QPushButton("Browse...")
        browse_btn.clicked.connect(self.browse_output_path)
        path_row.addWidget(browse_btn)
        
        path_layout.addLayout(path_row)
        layout.addWidget(path_group)
        
        # Game preset group
        preset_group = QGroupBox("Game Preset")
        preset_layout = QVBoxLayout(preset_group)
        
        self.preset_combo = QComboBox()
        presets = get_available_presets()
        for code, preset in presets.items():
            self.preset_combo.addItem(f"{preset['name']} - {preset['description']}", code)
        self.preset_combo.setCurrentText("GTA San Andreas")  # Default
        self.preset_combo.currentTextChanged.connect(self.on_preset_changed)
        
        preset_layout.addWidget(self.preset_combo)
        layout.addWidget(preset_group)
        
        # Options group
        options_group = QGroupBox("Options")
        options_layout = QGridLayout(options_group)
        
        # Size
        options_layout.addWidget(QLabel("Initial Size (MB):"), 0, 0)
        self.size_spinbox = QSpinBox()
        self.size_spinbox.setRange(1, 2000)
        self.size_spinbox.setValue(200)
        options_layout.addWidget(self.size_spinbox, 0, 1)
        
        # Compression
        self.compression_checkbox = QCheckBox("Enable Compression")
        options_layout.addWidget(self.compression_checkbox, 1, 0, 1, 2)
        
        # Auto-create structure
        self.structure_checkbox = QCheckBox("Create Directory Structure")
        options_layout.addWidget(self.structure_checkbox, 2, 0, 1, 2)
        
        layout.addWidget(options_group)
        
        # Progress bar (hidden initially)
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
        
        self.create_btn = QPushButton("Create IMG")
        self.create_btn.clicked.connect(self.create_img)
        self.create_btn.setDefault(True)
        button_layout.addWidget(self.create_btn)
        
        layout.addLayout(button_layout)
    
    def load_default_settings(self):
        """Load default settings from app settings"""
        try:
            parent = self.parent()
            if hasattr(parent, 'settings'):
                settings = parent.settings
                
                # Set default output path
                default_path = settings.get('assists_folder', '/home/x2')
                self.path_edit.setText(os.path.join(default_path, "new_archive.img"))
                
                # Set default size
                default_size = settings.get('default_initial_size_mb', 200)
                self.size_spinbox.setValue(default_size)
                
                # Set default compression
                default_compression = settings.get('compression_enabled_by_default', False)
                self.compression_checkbox.setChecked(default_compression)
                
        except Exception:
            # Use fallbacks
            self.path_edit.setText("/home/x2/new_archive.img")
    
    def on_preset_changed(self):
        """Handle preset change"""
        try:
            current_data = self.preset_combo.currentData()
            if current_data:
                preset = GamePreset.get_preset(current_data)
                if preset:
                    self.size_spinbox.setValue(preset['default_size'])
                    self.compression_checkbox.setChecked(preset['compression'])
        except Exception:
            pass
    
    def browse_output_path(self):
        """Browse for output path"""
        try:
            default_dir = os.path.dirname(self.path_edit.text())
            
            file_path, _ = QFileDialog.getSaveFileName(
                self, 
                "Create New IMG Archive", 
                os.path.join(default_dir, "new_archive.img"),
                "IMG Archives (*.img);;All Files (*.*)"
            )
            
            if file_path:
                self.path_edit.setText(file_path)
        except Exception as e:
            QMessageBox.warning(self, "Browse Error", f"Error browsing for file: {str(e)}")
    
    def create_img(self):
        """Start IMG creation"""
        try:
            output_path = self.path_edit.text().strip()
            if not output_path:
                QMessageBox.warning(self, "Invalid Path", "Please specify an output path.")
                return
            
            preset_code = self.preset_combo.currentData()
            if not preset_code:
                QMessageBox.warning(self, "Invalid Preset", "Please select a game preset.")
                return
            
            # Check if file exists
            if os.path.exists(output_path):
                reply = QMessageBox.question(
                    self, "File Exists", 
                    f"File already exists:\n{output_path}\n\nOverwrite?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                    QMessageBox.StandardButton.No
                )
                if reply != QMessageBox.StandardButton.Yes:
                    return
            
            # Prepare options
            options = {
                'initial_size_mb': self.size_spinbox.value(),
                'compression_enabled': self.compression_checkbox.isChecked(),
                'create_structure': self.structure_checkbox.isChecked()
            }
            
            # Start creation thread
            self.creation_thread = IMGCreationThread(output_path, preset_code, **options)
            self.creation_thread.progress_updated.connect(self.update_progress)
            self.creation_thread.creation_finished.connect(self.creation_finished)
            
            # Show progress UI
            self.progress_bar.setVisible(True)
            self.progress_label.setVisible(True)
            self.create_btn.setEnabled(False)
            
            self.creation_thread.start()
            
        except Exception as e:
            QMessageBox.critical(self, "Creation Error", f"Error starting creation: {str(e)}")
    
    def update_progress(self, value, message):
        """Update progress display"""
        self.progress_bar.setValue(value)
        self.progress_label.setText(message)
        QApplication.processEvents()
    
    def creation_finished(self, success, message):
        """Handle creation completion"""
        try:
            self.progress_bar.setVisible(False)
            self.progress_label.setVisible(False)
            self.create_btn.setEnabled(True)
            
            if success:
                QMessageBox.information(self, "Creation Complete", message)
                self.output_path = self.path_edit.text()
                self.accept()
            else:
                QMessageBox.critical(self, "Creation Failed", message)
                
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Error handling completion: {str(e)}")


def create_new_img_dialog(parent=None) -> NewIMGDialog: #vers 1
    """Create new IMG dialog instance"""
    return NewIMGDialog(parent)


def get_default_output_path(parent=None) -> str: #vers 1
    """Get default output path from settings"""
    try:
        if parent and hasattr(parent, 'settings'):
            assists_folder = parent.settings.get('assists_folder', '/home/x2')
            return os.path.join(assists_folder, 'new_archive.img')
    except Exception:
        pass
    return '/home/x2/new_archive.img'


def show_creation_dialog(parent=None) -> Optional[str]: #vers 1
    """Show creation dialog and return created file path"""
    try:
        dialog = NewIMGDialog(parent)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            return dialog.output_path
        return None
    except Exception:
        return None

# In open.py and create.py, use tab_system directly:
def _load_img_file(main_window, file_path):
    from apps.methods.tab_system import create_tab
    tab_index = create_tab(main_window, file_path, 'IMG', None)

def create_new_img(self): #vers 5
    """Show new IMG creation dialog - FIXED: No signal connections"""
    try:
        dialog = GameSpecificIMGDialog(self)
        dialog.template_manager = self.template_manager

        # Execute dialog and check result
        if dialog.exec() == QDialog.DialogCode.Accepted:
            # Get the output path from the dialog
            if hasattr(dialog, 'output_path') and dialog.output_path:
                output_path = dialog.output_path
                self.log_message(f"Created: {os.path.basename(output_path)}")

                # Load the created IMG file in a new tab
                self._load_img_file_in_new_tab(output_path)
    except Exception as e:
        self.log_message(f"Error creating new IMG: {str(e)}")



# Export list for external imports
__all__ = [
    'NewIMGDialog',
    'create_new_img_dialog',
    'create_new_img',
    'get_default_output_path', 
    'show_creation_dialog'
]
