#this belongs in apps/components/ img_creator.py - Version: 2
# X-Seti - July16 2025 - Img Factory 1.5

"""
IMG Creator - Updated with Project Folder Integration
Uses assists_folder from settings as default output path
"""

import os
from typing import Dict, Any, Optional
from pathlib import Path
from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton, 
                           QLabel, QComboBox, QSpinBox, QCheckBox, QLineEdit, 
                           QFileDialog, QProgressBar, QGroupBox, QGridLayout,
                           QMessageBox, QTextEdit)
from PyQt6.QtCore import QThread, pyqtSignal
from PyQt6.QtGui import QFont

# Import updated core classes and version creators - img_core_classes neeeds fixing.
from apps.methods.img_core_classes import IMGFile, IMGVersion
from apps.core.img_version1 import create_version_1_img
from apps.core.img_version2 import create_version_2_img
from apps.methods.rw_versions import get_default_version_for_game
default_version = get_default_version_for_game('gtasa')

class GamePreset:
    """Game preset configurations for IMG creation"""
    
    PRESETS = {
        'gta3': {
            'name': 'GTA III',
            'code': 'gta3',
            'version': IMGVersion.VERSION_1,
            'default_size': 50,
            'compression': False,
            'description': 'GTA III format (DIR+IMG files)'
        },
        'gtavc': {
            'name': 'GTA Vice City', 
            'code': 'gtavc',
            'version': IMGVersion.VERSION_1,
            'default_size': 100,
            'compression': False,
            'description': 'Vice City format (DIR+IMG files)'
        },
        'gtasa': {
            'name': 'GTA San Andreas',
            'code': 'gtasa', 
            'version': IMGVersion.VERSION_2,
            'default_size': 200,
            'compression': False,
            'description': 'San Andreas format (Single IMG file)'
        },
        'gtaiv': {
            'name': 'GTA IV',
            'code': 'gtaiv',
            'version': IMGVersion.VERSION_2,
            'default_size': 500,
            'compression': True,
            'description': 'GTA IV format (Single IMG file with compression)'
        },
        'custom': {
            'name': 'Custom Project',
            'code': 'custom',
            'version': IMGVersion.VERSION_2,
            'default_size': 100,
            'compression': False,
            'description': 'Custom format for modding projects'
        }
    }
    
    @classmethod
    def get_preset(cls, code: str) -> Optional[Dict[str, Any]]:
        """Get preset by code"""
        return cls.PRESETS.get(code)
    
    @classmethod
    def get_all_presets(cls) -> Dict[str, Dict[str, Any]]:
        """Get all available presets"""
        return cls.PRESETS.copy()


class NewIMGDialog(QDialog):
    """Dialog for creating new IMG files - Updated with Project Folder Integration"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.selected_preset = None
        self.output_path = ""
        self.creation_thread = None
        self.main_window = parent  # Store reference to get settings
        
        self.setWindowTitle("Create New IMG File")
        self.setModal(True)
        self.setMinimumSize(500, 400)
        
        self.setup_ui()
        self.load_project_folder_default()  # NEW: Load project folder as default
    
    def setup_ui(self):
        """Setup dialog UI"""
        layout = QVBoxLayout()
        
        # Game preset selection
        preset_group = QGroupBox("Game Preset")
        preset_layout = QVBoxLayout()
        
        self.preset_combo = QComboBox()
        for code, preset in GamePreset.get_all_presets().items():
            self.preset_combo.addItem(f"{preset['name']} - {preset['description']}", code)
        
        self.preset_combo.currentTextChanged.connect(self._on_preset_changed)
        preset_layout.addWidget(self.preset_combo)
        
        preset_group.setLayout(preset_layout)
        layout.addWidget(preset_group)
        
        # Output path selection - UPDATED with project folder integration
        path_group = QGroupBox("Output Location")
        path_layout = QVBoxLayout()
        
        path_row = QHBoxLayout()
        self.path_edit = QLineEdit()
        self.path_edit.setPlaceholderText("Select output path...")
        self.browse_btn = QPushButton("Browse...")
        self.browse_btn.clicked.connect(self._browse_output_path)
        
        path_row.addWidget(self.path_edit)
        path_row.addWidget(self.browse_btn)
        path_layout.addLayout(path_row)
        
        # Project folder info label
        self.project_info_label = QLabel("📁 Using project folder from settings")
        self.project_info_label.setStyleSheet("color: #666; font-size: 9pt;")
        path_layout.addWidget(self.project_info_label)
        
        path_group.setLayout(path_layout)
        layout.addWidget(path_group)
        
        # Settings group
        settings_group = QGroupBox("IMG Settings")
        settings_layout = QGridLayout()
        
        # Version selection
        settings_layout.addWidget(QLabel("Version:"), 0, 0)
        self.version_combo = QComboBox()
        self.version_combo.addItem("Version 1 (DIR+IMG)", IMGVersion.VERSION_1)
        self.version_combo.addItem("Version 2 (Single IMG)", IMGVersion.VERSION_2)
        settings_layout.addWidget(self.version_combo, 0, 1)
        
        # Size setting
        settings_layout.addWidget(QLabel("Initial Size (MB):"), 1, 0)
        self.size_spin = QSpinBox()
        self.size_spin.setRange(1, 2000)
        self.size_spin.setValue(100)
        settings_layout.addWidget(self.size_spin, 1, 1)
        
        # Compression option
        self.compression_check = QCheckBox("Enable Compression")
        settings_layout.addWidget(self.compression_check, 2, 0, 1, 2)
        
        # Structure option
        self.structure_check = QCheckBox("Create Basic Directory Structure")
        settings_layout.addWidget(self.structure_check, 3, 0, 1, 2)
        
        settings_group.setLayout(settings_layout)
        layout.addWidget(settings_group)
        
        # Progress bar (hidden initially)
        self.progress_bar = QProgressBar()
        self.progress_bar.setVisible(False)
        layout.addWidget(self.progress_bar)
        
        # Buttons
        button_layout = QHBoxLayout()
        self.create_btn = QPushButton("Create IMG")
        self.create_btn.clicked.connect(self._create_img)
        self.create_btn.setDefault(True)
        
        self.cancel_btn = QPushButton("Cancel")
        self.cancel_btn.clicked.connect(self.reject)
        
        button_layout.addStretch()
        button_layout.addWidget(self.create_btn)
        button_layout.addWidget(self.cancel_btn)
        
        layout.addLayout(button_layout)
        self.setLayout(layout)
        
        # Set initial preset
        self._on_preset_changed()
    
    def load_project_folder_default(self):
        """Load project folder from settings as default output path"""
        try:
            if self.main_window and hasattr(self.main_window, 'settings'):
                # Try to get assists_folder from settings
                assists_folder = getattr(self.main_window.settings, 'assists_folder', None)
                if assists_folder and os.path.exists(assists_folder):
                    self.path_edit.setText(assists_folder)
                    self.project_info_label.setText(f"📁 Using project folder: {assists_folder}")
                    return
                
                # Fallback to project_folder for compatibility
                project_folder = self.main_window.settings.get('project_folder')
                if project_folder and os.path.exists(project_folder):
                    self.path_edit.setText(project_folder)
                    self.project_info_label.setText(f"📁 Using project folder: {project_folder}")
                    return
            
            # No project folder set
            self.project_info_label.setText("⚠️ No project folder set in settings")
            self.project_info_label.setStyleSheet("color: #ff9800; font-size: 9pt;")
            
        except Exception as e:
            print(f"❌ Error loading project folder: {e}")
            self.project_info_label.setText("❌ Error loading project folder")
            self.project_info_label.setStyleSheet("color: #f44336; font-size: 9pt;")
    
    def _browse_output_path(self):
        """Browse for output path"""
        current_path = self.path_edit.text() or os.path.expanduser("~")
        
        file_path, _ = QFileDialog.getSaveFileName(
            self, 
            "Save IMG File",
            current_path,
            "IMG Files (*.img);;All Files (*)"
        )
        
        if file_path:
            self.path_edit.setText(file_path)
            # Update project info to show custom path
            self.project_info_label.setText("📁 Using custom output path")
            self.project_info_label.setStyleSheet("color: #666; font-size: 9pt;")
    
    def _on_preset_changed(self):
        """Handle preset selection change"""
        current_data = self.preset_combo.currentData()
        if current_data:
            self.selected_preset = GamePreset.get_preset(current_data)
            if self.selected_preset:
                # Update UI based on preset
                self.version_combo.setCurrentText(
                    f"Version {self.selected_preset['version'].value}"
                )
                self.size_spin.setValue(self.selected_preset['default_size'])
                self.compression_check.setChecked(self.selected_preset['compression'])
    
    def _create_img(self):
        """Create IMG file"""
        # Validate inputs
        self.output_path = self.path_edit.text().strip()
        if not self.output_path:
            QMessageBox.warning(self, "Warning", "Please select an output path.")
            return
        
        if not self.selected_preset:
            QMessageBox.warning(self, "Warning", "Please select a game preset.")
            return
        
        # Get creation settings
        settings = {
            'output_path': self.output_path,
            'version': self.version_combo.currentData(),
            'size_mb': self.size_spin.value(),
            'compression': self.compression_check.isChecked(),
            'create_structure': self.structure_check.isChecked(),
            'preset_name': self.selected_preset['name'],
            'game_preset': self.selected_preset
        }
        
        # Show progress and disable UI
        self.progress_bar.setVisible(True)
        self.create_btn.setEnabled(False)
        self.cancel_btn.setEnabled(False)
        
        # Start creation thread
        self.creation_thread = IMGCreationThread(settings)
        self.creation_thread.progress_updated.connect(self._update_progress)
        self.creation_thread.creation_finished.connect(self._creation_finished)
        self.creation_thread.start()
    
    def _update_progress(self, value: int, message: str):
        """Update progress display"""
        self.progress_bar.setValue(value)
        self.progress_bar.setFormat(f"{message} ({value}%)")
    
    def _creation_finished(self, success: bool, message: str):
        """Handle creation completion"""
        self.progress_bar.setVisible(False)
        self.create_btn.setEnabled(True)
        self.cancel_btn.setEnabled(True)
        
        if success:
            QMessageBox.information(self, "Success", message)
            self.accept()
        else:
            QMessageBox.critical(self, "Error", message)


class IMGCreationThread(QThread):
    """Background thread for IMG creation"""
    
    progress_updated = pyqtSignal(int, str)
    creation_finished = pyqtSignal(bool, str)
    
    def __init__(self, settings: Dict[str, Any]):
        super().__init__()
        self.settings = settings
    
    def run(self):
        """Create IMG file in background thread"""
        try:
            output_path = self.settings['output_path']
            version = self.settings['version']
            size_mb = self.settings['size_mb']
            compression = self.settings['compression']
            create_structure = self.settings['create_structure']
            preset_name = self.settings['preset_name']
            game_preset = self.settings['game_preset']
            
            self.progress_updated.emit(10, "Initializing")
            
            # Create IMG file using appropriate version creator
            self.progress_updated.emit(30, "Creating file")
            
            creation_options = {
                'initial_size_mb': size_mb,
                'compression_enabled': compression,
                'game_preset': game_preset
            }
            
            # Use version-specific creation
            if version == IMGVersion.VERSION_1:
                success = create_version_1_img(output_path, size_mb, game_preset)
            elif version == IMGVersion.VERSION_2:
                success = create_version_2_img(output_path, size_mb, compression, game_preset)
            else:
                self.creation_finished.emit(False, f"Unsupported IMG version: {version}")
                return
            
            if not success:
                self.creation_finished.emit(False, "Failed to create IMG file structure")
                return
            
            self.progress_updated.emit(60, "Configuring")
            
            # Add basic structure if requested
            if create_structure:
                self.progress_updated.emit(80, "Creating structure")
                # TODO: Add structure creation logic here
            
            self.progress_updated.emit(100, "Complete")
            
            # Success message
            file_size = os.path.getsize(output_path) if os.path.exists(output_path) else 0
            size_str = f"{file_size / (1024*1024):.1f} MB"
            
            success_msg = (
                f"IMG archive created successfully!\n\n"
                f"Game: {preset_name}\n"
                f"File: {output_path}\n"
                f"Version: {version.name}\n"
                f"Size: {size_str}"
            )
            
            self.creation_finished.emit(True, success_msg)
            
        except Exception as e:
            self.creation_finished.emit(False, f"Creation failed: {str(e)}")


class BasicIMGCreator:
    """Static utility class for basic IMG creation"""
    
    @staticmethod
    def create_simple(output_path: str, size_mb: int = 100) -> bool:
        """Create simple IMG file with minimal configuration"""
        try:
            return create_version_2_img(output_path, size_mb)
        except Exception as e:
            print(f"❌ Error creating simple IMG: {e}")
            return False
    
    @staticmethod
    def create_with_preset(output_path: str, preset_code: str) -> bool:
        """Create IMG using game preset"""
        try:
            preset = GamePreset.get_preset(preset_code)
            if not preset:
                return False
            
            if preset['version'] == IMGVersion.VERSION_1:
                return create_version_1_img(output_path, preset['default_size'], preset)
            else:
                return create_version_2_img(output_path, preset['default_size'], 
                                          preset['compression'], preset)
        except Exception as e:
            print(f"❌ Error creating with preset: {e}")
            return False


# Factory functions for external use
def create_new_img_dialog(parent=None) -> NewIMGDialog:
    """Create new IMG dialog instance"""
    return NewIMGDialog(parent)


def get_available_presets() -> Dict[str, Dict[str, Any]]:
    """Get all available game presets"""
    return GamePreset.get_all_presets()


def create_basic_img(output_path: str, preset_code: str = 'gtasa') -> bool:
    """Quick function to create basic IMG file"""
    return BasicIMGCreator.create_with_preset(output_path, preset_code)


# Export list for external imports
__all__ = [
    'NewIMGDialog',
    'GamePreset', 
    'IMGCreationThread',
    'BasicIMGCreator',
    'create_new_img_dialog',
    'get_available_presets', 
    'create_basic_img'
]
