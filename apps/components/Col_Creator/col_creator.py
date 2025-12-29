#this belongs in components.Col_Creator.col_creator.py - Version: 2
# X-Seti - July17 2025 - IMG Factory 1.5 - COL Creator
# IMG Creator Treatment: Same structure, COL functions, IMG debug system

"""
COL Creator - Complete collision file creation with dialog system
Uses IMG debug system and follows IMG creator pattern
"""

import os
import struct
from typing import Dict, List, Optional, Any
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QFormLayout, QGridLayout, 
    QLabel, QPushButton, QLineEdit, QComboBox, QSpinBox, QDoubleSpinBox,
    QCheckBox, QProgressBar, QMessageBox, QFileDialog, QGroupBox,
    QListWidget, QListWidgetItem, QTextEdit, QTabWidget, QWidget
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal
from PyQt6.QtGui import QFont, QIcon

# Import COL classes and IMG debug system
from apps.methods.col_core_classes import COLFile, COLModel, COLVersion
from apps.debug.debug_functions import col_debug_log, debug_col_creation_process
from apps.debug.debug_functions import img_debugger

##Methods list -
# create_basic_col
# create_new_col_dialog
# create_new_col_file
# export_all_col_from_img
# get_available_col_presets
# import_col_to_current_img

##Classes -
# BasicCOLCreator
# COLCreationThread
# COLPreset
# NewCOLDialog

class COLPreset:
    """COL file presets for different games and purposes"""
    
    @staticmethod
    def get_all_presets() -> Dict[str, Dict[str, Any]]:
        """Get all available COL presets"""
        return {
            'gtasa': {
                'name': 'GTA San Andreas',
                'version': COLVersion.COL2,
                'description': 'Standard COL2 format for San Andreas',
                'default_models': 1,
                'sphere_limit': 256,
                'box_limit': 256,
                'mesh_limit': 1024,
                'compression': False
            },
            'gtavc': {
                'name': 'GTA Vice City',
                'version': COLVersion.COL2,
                'description': 'COL2 format optimized for Vice City',
                'default_models': 1,
                'sphere_limit': 128,
                'box_limit': 128,
                'mesh_limit': 512,
                'compression': False
            },
            'gta3': {
                'name': 'GTA III',
                'version': COLVersion.COL1,
                'description': 'Legacy COL1 format for GTA III',
                'default_models': 1,
                'sphere_limit': 64,
                'box_limit': 64,
                'mesh_limit': 256,
                'compression': False
            },
            'gtaiv': {
                'name': 'GTA IV',
                'version': COLVersion.COL3,
                'description': 'Advanced COL3 format for GTA IV',
                'default_models': 1,
                'sphere_limit': 512,
                'box_limit': 512,
                'mesh_limit': 2048,
                'compression': True
            },
            'custom': {
                'name': 'Custom COL',
                'version': COLVersion.COL2,
                'description': 'Custom collision file with user settings',
                'default_models': 1,
                'sphere_limit': 256,
                'box_limit': 256,
                'mesh_limit': 1024,
                'compression': False
            }
        }
    
    @staticmethod
    def get_preset(preset_code: str) -> Optional[Dict[str, Any]]:
        """Get specific preset by code"""
        presets = COLPreset.get_all_presets()
        return presets.get(preset_code)

class NewCOLDialog(QDialog):
    """Dialog for creating new COL files - Mirrors NewIMGDialog structure"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.output_path = None
        self.selected_preset = None
        self.creation_thread = None
        
        # Initialize UI
        self.setWindowTitle("Create New COL File - IMG Factory 1.5")
        self.setMinimumSize(500, 600)
        self.resize(600, 700)
        
        self._setup_ui()
        self._load_presets()
        self._connect_signals()
        
        # Debug the creation process
        if img_debugger.debug_enabled:
            debug_col_creation_process(self)
    
    def _setup_ui(self):
        """Setup the dialog UI"""
        layout = QVBoxLayout(self)
        
        # File settings section
        file_group = QGroupBox("🗂️ File Settings")
        file_layout = QFormLayout(file_group)
        
        # Output path selection
        path_layout = QHBoxLayout()
        self.path_input = QLineEdit()
        self.path_input.setPlaceholderText("Select output location...")
        self.browse_btn = QPushButton("📁 Browse")
        self.browse_btn.clicked.connect(self._browse_output_path)
        path_layout.addWidget(self.path_input)
        path_layout.addWidget(self.browse_btn)
        file_layout.addRow("Output Path:", path_layout)
        
        # File name input
        self.filename_input = QLineEdit()
        self.filename_input.setPlaceholderText("collision")
        file_layout.addRow("File Name:", self.filename_input)
        
        layout.addWidget(file_group)
        
        # COL settings section
        col_group = QGroupBox("COL Settings")
        col_layout = QFormLayout(col_group)
        
        # Version selection
        self.version_combo = QComboBox()
        for version in COLVersion:
            self.version_combo.addItem(f"COL{version.value}", version)
        self.version_combo.setCurrentIndex(1)  # Default to COL2
        col_layout.addRow("COL Version:", self.version_combo)
        
        # Model count
        self.model_count_spin = QSpinBox()
        self.model_count_spin.setRange(1, 100)
        self.model_count_spin.setValue(1)
        col_layout.addRow("Model Count:", self.model_count_spin)
        
        # Collision complexity
        self.sphere_limit_spin = QSpinBox()
        self.sphere_limit_spin.setRange(0, 1024)
        self.sphere_limit_spin.setValue(256)
        col_layout.addRow("Sphere Limit:", self.sphere_limit_spin)
        
        self.box_limit_spin = QSpinBox()
        self.box_limit_spin.setRange(0, 1024)
        self.box_limit_spin.setValue(256)
        col_layout.addRow("Box Limit:", self.box_limit_spin)
        
        self.mesh_limit_spin = QSpinBox()
        self.mesh_limit_spin.setRange(0, 4096)
        self.mesh_limit_spin.setValue(1024)
        col_layout.addRow("Mesh Limit:", self.mesh_limit_spin)
        
        # Options
        self.compression_check = QCheckBox("Enable Compression")
        col_layout.addRow("Options:", self.compression_check)
        
        layout.addWidget(col_group)
        
        # Game presets section
        preset_group = QGroupBox("🎮 Game Presets")
        preset_layout = QVBoxLayout(preset_group)
        
        self.preset_list = QListWidget()
        self.preset_list.setMaximumHeight(150)
        preset_layout.addWidget(self.preset_list)
        
        # Preset description
        self.preset_description = QTextEdit()
        self.preset_description.setMaximumHeight(80)
        self.preset_description.setReadOnly(True)
        preset_layout.addWidget(self.preset_description)
        
        layout.addWidget(preset_group)
        
        # Progress bar (initially hidden)
        self.progress_bar = QProgressBar()
        self.progress_bar.setVisible(False)
        layout.addWidget(self.progress_bar)
        
        # Buttons
        button_layout = QHBoxLayout()
        self.create_btn = QPushButton("Create COL File")
        self.create_btn.setDefault(True)
        self.cancel_btn = QPushButton("❌ Cancel")
        
        button_layout.addStretch()
        button_layout.addWidget(self.create_btn)
        button_layout.addWidget(self.cancel_btn)
        
        layout.addLayout(button_layout)
    
    def _load_presets(self):
        """Load game presets into the list"""
        presets = COLPreset.get_all_presets()
        
        for preset_code, preset_data in presets.items():
            item = QListWidgetItem(f"🎮 {preset_data['name']}")
            item.setData(Qt.ItemDataRole.UserRole, {'code': preset_code, 'data': preset_data})
            self.preset_list.addItem(item)
        
        # Select default preset
        if self.preset_list.count() > 0:
            self.preset_list.setCurrentRow(0)
            self._on_preset_selected()
    
    def _connect_signals(self):
        """Connect UI signals"""
        self.create_btn.clicked.connect(self._create_col_file)
        self.cancel_btn.clicked.connect(self.reject)
        self.preset_list.currentRowChanged.connect(self._on_preset_selected)
        self.version_combo.currentTextChanged.connect(self._on_version_changed)
    
    def _browse_output_path(self):
        """Browse for output directory"""
        directory = QFileDialog.getExistingDirectory(
            self, 
            "Select Output Directory",
            os.path.expanduser("~")
        )
        
        if directory:
            self.path_input.setText(directory)
            self.output_path = directory
    
    def _on_preset_selected(self):
        """Handle preset selection"""
        current_item = self.preset_list.currentItem()
        if not current_item:
            return
        
        preset_info = current_item.data(Qt.ItemDataRole.UserRole)
        if not preset_info:
            return
        
        self.selected_preset = preset_info['data']
        preset_code = preset_info['code']
        
        # Update description
        self.preset_description.setText(self.selected_preset['description'])
        
        # Update settings based on preset
        self.version_combo.setCurrentText(f"COL{self.selected_preset['version'].value}")
        self.model_count_spin.setValue(self.selected_preset['default_models'])
        self.sphere_limit_spin.setValue(self.selected_preset['sphere_limit'])
        self.box_limit_spin.setValue(self.selected_preset['box_limit'])
        self.mesh_limit_spin.setValue(self.selected_preset['mesh_limit'])
        self.compression_check.setChecked(self.selected_preset['compression'])
        
        img_debugger.debug(f"Selected COL preset: {preset_code}")
    
    def _on_version_changed(self):
        """Handle version change"""
        version = self.version_combo.currentData()
        img_debugger.debug(f"COL version changed to: {version}")
        
        # Adjust limits based on version
        if version == COLVersion.COL1:
            self.sphere_limit_spin.setMaximum(64)
            self.box_limit_spin.setMaximum(64)
            self.mesh_limit_spin.setMaximum(256)
            self.compression_check.setEnabled(False)
            self.compression_check.setChecked(False)
        elif version == COLVersion.COL2:
            self.sphere_limit_spin.setMaximum(256)
            self.box_limit_spin.setMaximum(256)
            self.mesh_limit_spin.setMaximum(1024)
            self.compression_check.setEnabled(True)
        else:  # COL3/COL4
            self.sphere_limit_spin.setMaximum(512)
            self.box_limit_spin.setMaximum(512)
            self.mesh_limit_spin.setMaximum(2048)
            self.compression_check.setEnabled(True)
    
    def _create_col_file(self):
        """Create the COL file"""
        # Validate inputs
        if not self.output_path:
            QMessageBox.warning(self, "Warning", "Please select an output directory.")
            return
        
        if not self.filename_input.text().strip():
            QMessageBox.warning(self, "Warning", "Please enter a filename.")
            return
        
        if not self.selected_preset:
            QMessageBox.warning(self, "Warning", "Please select a game preset.")
            return
        
        # Get creation settings
        filename = self.filename_input.text().strip()
        if not filename.endswith('.col'):
            filename += '.col'
        
        full_path = os.path.join(self.output_path, filename)
        
        settings = {
            'output_path': full_path,
            'version': self.version_combo.currentData(),
            'model_count': self.model_count_spin.value(),
            'sphere_limit': self.sphere_limit_spin.value(),
            'box_limit': self.box_limit_spin.value(),
            'mesh_limit': self.mesh_limit_spin.value(),
            'compression': self.compression_check.isChecked(),
            'preset_name': self.selected_preset['name'],
            'game_preset': self.selected_preset
        }
        
        # Debug creation settings
        img_debugger.debug_col_creation(None, **settings)
        
        # Show progress and disable UI
        self.progress_bar.setVisible(True)
        self.create_btn.setEnabled(False)
        self.cancel_btn.setEnabled(False)
        
        # Start creation thread
        self.creation_thread = COLCreationThread(settings)
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

class COLCreationThread(QThread):
    """Background thread for COL creation"""
    
    progress_updated = pyqtSignal(int, str)
    creation_finished = pyqtSignal(bool, str)
    
    def __init__(self, settings: Dict[str, Any]):
        super().__init__()
        self.settings = settings
    
    def run(self):
        """Create COL file in background thread"""
        try:
            output_path = self.settings['output_path']
            version = self.settings['version']
            model_count = self.settings['model_count']
            preset_name = self.settings['preset_name']
            
            self.progress_updated.emit(10, "Initializing")
            
            # Create COL file using appropriate version creator
            self.progress_updated.emit(30, "Creating file structure")
            
            creation_options = {
                'model_count': model_count,
                'sphere_limit': self.settings['sphere_limit'],
                'box_limit': self.settings['box_limit'],
                'mesh_limit': self.settings['mesh_limit'],
                'compression': self.settings['compression'],
                'game_preset': self.settings['game_preset']
            }
            
            img_debugger.debug(f"Creating COL file: {output_path}")
            
            # Use version-specific creation
            if version == COLVersion.COL1:
                success = create_version_1_col(output_path, creation_options)
            elif version == COLVersion.COL2:
                success = create_version_2_col(output_path, creation_options)
            elif version == COLVersion.COL3:
                success = create_version_3_col(output_path, creation_options)
            else:
                self.creation_finished.emit(False, f"Unsupported COL version: {version}")
                return
            
            self.progress_updated.emit(80, "Finalizing")
            
            if success:
                self.progress_updated.emit(100, "Complete")
                self.creation_finished.emit(True, f"COL file created successfully!\nLocation: {output_path}")
            else:
                self.creation_finished.emit(False, "Failed to create COL file")
                
        except Exception as e:
            img_debugger.error(f"COL creation error: {e}")
            self.creation_finished.emit(False, f"Error creating COL file: {str(e)}")

class BasicCOLCreator:
    """Simple COL file creation without dialog"""
    
    @staticmethod
    def create_empty_col(output_path: str, version: COLVersion = COLVersion.COL_2) -> bool:
        """Create empty COL file"""
        try:
            img_debugger.debug(f"Creating empty COL file: {output_path}")
            
            col_file = COLFile()
            col_file.file_path = output_path
            
            # Create empty model
            model = COLModel()
            model.name = "default"
            model.version = version
            
            col_file.models = [model]
            
            # Write to file
            return col_file.save()
            
        except Exception as e:
            img_debugger.error(f"Error creating empty COL: {e}")
            return False
    
    @staticmethod
    def create_with_preset(output_path: str, preset_code: str) -> bool:
        """Create COL using game preset"""
        try:
            preset = COLPreset.get_preset(preset_code)
            if not preset:
                return False
            
            options = {
                'model_count': preset['default_models'],
                'sphere_limit': preset['sphere_limit'],
                'box_limit': preset['box_limit'],
                'mesh_limit': preset['mesh_limit'],
                'compression': preset['compression'],
                'game_preset': preset
            }
            
            if preset['version'] == COLVersion.COL1:
                return create_version_1_col(output_path, options)
            elif preset['version'] == COLVersion.COL2:
                return create_version_2_col(output_path, options)
            else:
                return create_version_3_col(output_path, options)
                
        except Exception as e:
            img_debugger.error(f"Error creating with preset: {e}")
            return False

# COL creation functions (version-specific)
def create_version_1_col(output_path: str, options: Dict[str, Any]) -> bool:
    """Create COL1 format file"""
    try:
        img_debugger.debug("Creating COL1 format file")
        
        col_file = COLFile()
        col_file.file_path = output_path
        
        # Create basic COL1 structure
        for i in range(options['model_count']):
            model = COLModel()
            model.name = f"model_{i:02d}"
            model.version = COLVersion.COL1
            
            # Add basic collision elements
            # (Implementation depends on COL1 format specifics)
            
            col_file.models.append(model)
        
        return col_file.save()
        
    except Exception as e:
        img_debugger.error(f"COL1 creation error: {e}")
        return False

def create_version_2_col(output_path: str, options: Dict[str, Any]) -> bool:
    """Create COL2 format file"""
    try:
        img_debugger.debug("Creating COL2 format file")
        
        col_file = COLFile()
        col_file.file_path = output_path
        
        # Create COL2 structure with enhanced features
        for i in range(options['model_count']):
            model = COLModel()
            model.name = f"model_{i:02d}"
            model.version = COLVersion.COL2
            
            # Add collision elements based on limits
            # (Implementation depends on COL2 format specifics)
            
            col_file.models.append(model)
        
        return col_file.save()
        
    except Exception as e:
        img_debugger.error(f"COL2 creation error: {e}")
        return False

def create_version_3_col(output_path: str, options: Dict[str, Any]) -> bool:
    """Create COL3 format file"""
    try:
        img_debugger.debug("Creating COL3 format file")
        
        col_file = COLFile()
        col_file.file_path = output_path
        
        # Create COL3 structure with advanced features
        for i in range(options['model_count']):
            model = COLModel()
            model.name = f"model_{i:02d}"
            model.version = COLVersion.COL3
            
            # Add advanced collision elements
            # (Implementation depends on COL3 format specifics)
            
            col_file.models.append(model)
        
        return col_file.save()
        
    except Exception as e:
        img_debugger.error(f"COL3 creation error: {e}")
        return False

# Main interface functions
def create_new_col_dialog(parent=None) -> NewCOLDialog: #vers 1
    """Create new COL dialog instance"""
    return NewCOLDialog(parent)

def get_available_col_presets() -> Dict[str, Dict[str, Any]]: #vers 1
    """Get all available COL presets"""
    return COLPreset.get_all_presets()

def create_basic_col(output_path: str, preset_code: str = 'gtasa') -> bool: #vers 1
    """Quick function to create basic COL file"""
    return BasicCOLCreator.create_with_preset(output_path, preset_code)

def create_new_col_file(main_window, preset_code: str = 'gtasa') -> bool: #vers 1
    """Create new COL file with dialog"""
    try:
        col_debug_log(main_window, "Opening COL creation dialog", 'COL_CREATOR')
        
        dialog = NewCOLDialog(main_window)
        
        if dialog.exec() == QDialog.DialogCode.Accepted:
            if dialog.output_path:
                col_debug_log(main_window, f"COL file created: {dialog.output_path}", 'COL_CREATOR', 'SUCCESS')
                return True
        
        col_debug_log(main_window, "COL creation cancelled", 'COL_CREATOR')
        return False
        
    except Exception as e:
        col_debug_log(main_window, f"COL creation error: {e}", 'COL_CREATOR', 'ERROR')
        return False

def import_col_to_current_img(main_window) -> bool: #vers 1
    """Import COL file to current IMG"""
    try:
        col_debug_log(main_window, "Starting COL to IMG import", 'COL_IMPORT')
        
        if not main_window.current_img:
            col_debug_log(main_window, "No IMG file loaded for import", 'COL_IMPORT', 'ERROR')
            return False
        
        # File dialog for COL selection
        file_path, _ = QFileDialog.getOpenFileName(
            main_window,
            "Select COL File to Import",
            "",
            "COL Files (*.col);;All Files (*)"
        )
        
        if not file_path:
            col_debug_log(main_window, "COL import cancelled", 'COL_IMPORT')
            return False
        
        # Load COL data
        with open(file_path, 'rb') as f:
            col_data = f.read()
        
        # Add to current IMG
        col_filename = os.path.basename(file_path)
        success = main_window.current_img.add_entry(col_filename, col_data)
        
        if success:
            col_debug_log(main_window, f"COL imported successfully: {col_filename}", 'COL_IMPORT', 'SUCCESS')
            # Refresh table display
            if hasattr(main_window, 'refresh_table'):
                main_window.refresh_table()
            return True
        else:
            col_debug_log(main_window, f"Failed to import COL: {col_filename}", 'COL_IMPORT', 'ERROR')
            return False
            
    except Exception as e:
        col_debug_log(main_window, f"COL import error: {e}", 'COL_IMPORT', 'ERROR')
        return False

def export_all_col_from_img(main_window) -> bool: #vers 1
    """Export all COL files from current IMG"""
    try:
        col_debug_log(main_window, "Starting COL export from IMG", 'COL_EXPORT')
        
        if not main_window.current_img:
            col_debug_log(main_window, "No IMG file loaded for export", 'COL_EXPORT', 'ERROR')
            return False
        
        # Directory dialog for export location
        directory = QFileDialog.getExistingDirectory(
            main_window,
            "Select Export Directory for COL Files",
            os.path.expanduser("~")
        )
        
        if not directory:
            col_debug_log(main_window, "COL export cancelled", 'COL_EXPORT')
            return False
        
        # Find and export COL files
        col_count = 0
        for entry in main_window.current_img.entries:
            if entry.name.lower().endswith('.col'):
                try:
                    col_data = entry.get_data()
                    output_path = os.path.join(directory, entry.name)
                    
                    with open(output_path, 'wb') as f:
                        f.write(col_data)
                    
                    col_count += 1
                    col_debug_log(main_window, f"Exported COL: {entry.name}", 'COL_EXPORT')
                    
                except Exception as e:
                    col_debug_log(main_window, f"Failed to export {entry.name}: {e}", 'COL_EXPORT', 'ERROR')
        
        if col_count > 0:
            col_debug_log(main_window, f"Exported {col_count} COL files successfully", 'COL_EXPORT', 'SUCCESS')
            QMessageBox.information(main_window, "Export Complete", 
                f"Exported {col_count} COL files to:\n{directory}")
            return True
        else:
            col_debug_log(main_window, "No COL files found to export", 'COL_EXPORT', 'WARNING')
            QMessageBox.information(main_window, "No COL Files", 
                "No COL files found in the current IMG archive.")
            return False
            
    except Exception as e:
        col_debug_log(main_window, f"COL export error: {e}", 'COL_EXPORT', 'ERROR')
        return False

# Export list for external imports
__all__ = [
    'NewCOLDialog',
    'COLPreset', 
    'COLCreationThread',
    'BasicCOLCreator',
    'create_new_col_dialog',
    'get_available_col_presets', 
    'create_basic_col',
    'create_new_col_file',
    'import_col_to_current_img',
    'export_all_col_from_img'
]
