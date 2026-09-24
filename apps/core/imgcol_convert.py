#this belongs in apps/core/imgcol_convert.py - Version: 4
# X-Seti - September02 2025 - IMG Factory 1.5 - IMG and COL Convert Functions

"""
IMG and COL Convert Functions - Complete Implementation
Handles converting IMG formats (V1 ↔ V2) and COL format conversions
"""

import os
import shutil
from pathlib import Path
from typing import Optional, List, Dict, Any, Tuple
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QFormLayout, QLabel, QPushButton,
    QLineEdit, QMessageBox, QComboBox, QTextEdit, QGroupBox, QCheckBox,
    QDialogButtonBox, QFileDialog, QProgressDialog, QListWidget, QListWidgetItem,
    QRadioButton, QButtonGroup, QSpinBox, QApplication
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont
from apps.methods.file_validation import validate_img_file, validate_any_file, get_selected_entries_for_operation
from apps.methods.tab_system import get_current_file_from_active_tab, validate_tab_before_operation, get_current_file_type_from_tab
from apps.core.convert import convert_img_format


##Methods list -
# convert_selected
# convert_col_format
# _show_convert_dialog
# _browse_output_file
# _convert_with_fallback
# _create_conversion_backup
# integrate_imgcol_convert_functions

def convert_selected(main_window): #vers 2
    """Main convert function - handles both IMG and COL format conversions"""
    try:
        # Use same tab awareness as other core functions
        if not validate_tab_before_operation(main_window, "Convert Selected"):
            return False
        
        # Get current file type
        file_type = get_current_file_type_from_tab(main_window)
        
        if file_type == 'IMG':
            return convert_img_format(main_window)
        elif file_type == 'COL':
            return convert_col_format(main_window)
        else:
            QMessageBox.warning(main_window, "No File", "Please open an IMG or COL file first")
            return False
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Convert selected error: {str(e)}")
        QMessageBox.critical(main_window, "Convert Error", f"Convert failed: {str(e)}")
        return False


def convert_col_format(main_window): #vers 1
    """Convert COL format (placeholder for COL version conversions)"""
    try:
        # Validate tab and get file object
        if not validate_tab_before_operation(main_window, "Convert COL Format"):
            return False
        
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'COL' or not file_object:
            QMessageBox.warning(main_window, "No COL File", "Current tab does not contain a COL file")
            return False
        
        # Get current COL info
        model_count = len(getattr(file_object, 'models', []))
        file_path = getattr(file_object, 'file_path', '')
        file_size = os.path.getsize(file_path) if file_path and os.path.exists(file_path) else 0
        
        # Show convert dialog for COL
        conversion_settings = _show_convert_dialog(main_window, "COL", {
            'current_version': 'COL1',  # Placeholder
            'file_path': file_path,
            'model_count': model_count,
            'file_size': file_size
        })
        
        if not conversion_settings:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("Convert operation cancelled")
            return False
        
        target_version = conversion_settings.get('target_version', 'COL2')
        output_path = conversion_settings.get('output_path', '')
        create_backup = conversion_settings.get('create_backup', True)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"COL format conversion requested: COL1 → {target_version}")
            main_window.log_message(f"Output: {output_path}")
            main_window.log_message("COL format conversion requires COL parser integration")
        
        # Create backup if requested
        if create_backup and file_path:
            backup_success = _create_conversion_backup(main_window, file_path)
            if not backup_success:
                reply = QMessageBox.question(main_window, "Backup Failed",
                    "Failed to create backup. Continue with conversion anyway?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                    QMessageBox.StandardButton.No)
                if reply != QMessageBox.StandardButton.Yes:
                    return False
        
        # For now, show info about COL conversion (placeholder implementation)
        info_text = f"""COL format conversion is prepared but requires COL parser integration.
Current file: {os.path.basename(file_path) if file_path else 'Unknown'}
Models: {model_count}
Size: {file_size:,} bytes

Target format: {target_version}
Output: {os.path.basename(output_path) if output_path else 'Unknown'}
This functionality will be available once COL core integration is completed."""
        
        QMessageBox.information(main_window, "COL Convert", info_text)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("COL conversion dialog completed (awaiting COL core integration)")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Convert COL format error: {str(e)}")
        QMessageBox.critical(main_window, "Convert COL Format Error", f"Convert COL format failed: {str(e)}")
        return False


def _show_convert_dialog(main_window, file_type: str, file_info: Dict) -> Optional[Dict]: #vers 1
    """Show convert dialog and get conversion settings"""
    try:
        dialog = QDialog(main_window)
        dialog.setWindowTitle(f"Convert {file_type} Format")
        dialog.setModal(True)
        dialog.setMinimumWidth(500)
        
        layout = QVBoxLayout(dialog)
        
        # Current file info group
        info_group = QGroupBox(f"Current {file_type} File")
        info_layout = QVBoxLayout(info_group)
        
        file_path = file_info.get('file_path', '')
        file_name = os.path.basename(file_path) if file_path else 'Unknown'
        current_version = file_info.get('current_version', 'Unknown')
        
        info_text = f"<b>File:</b> {file_name}<br>"
        info_text += f"<b>Current Version:</b> {current_version}<br>"
        
        if file_type == 'IMG':
            entry_count = file_info.get('entry_count', 0)
            file_size = file_info.get('file_size', 0)
            info_text += f"<b>Entries:</b> {entry_count}<br>"
            info_text += f"<b>Size:</b> {file_size:,} bytes"
        else:  # COL
            model_count = file_info.get('model_count', 0)
            file_size = file_info.get('file_size', 0)
            info_text += f"<b>Models:</b> {model_count}<br>"
            info_text += f"<b>Size:</b> {file_size:,} bytes"
        
        info_label = QLabel(info_text)
        info_layout.addWidget(info_label)
        layout.addWidget(info_group)
        
        # Conversion settings group
        settings_group = QGroupBox("Conversion Settings")
        settings_layout = QVBoxLayout(settings_group)
        
        # Target version selection
        version_layout = QFormLayout()
        
        if file_type == 'IMG':
            version_combo = QComboBox()
            if current_version == 'V1':
                version_combo.addItem("Version 2 (V2)", "V2")
                version_combo.setToolTip("Convert to IMG V2 format (larger file support, streaming)")
            elif current_version == 'V2':
                version_combo.addItem("Version 1 (V1)", "V1")
                version_combo.setToolTip("Convert to IMG V1 format (classic GTA compatibility)")
            else:
                # Unknown version, offer both
                version_combo.addItem("Version 1 (V1)", "V1")
                version_combo.addItem("Version 2 (V2)", "V2")
            
            version_layout.addRow("Target Version:", version_combo)
        else:  # COL
            version_combo = QComboBox()
            version_combo.addItem("COL Version 2", "COL2")
            version_combo.addItem("COL Version 3", "COL3")
            version_combo.setToolTip("COL format conversion (requires COL parser integration)")
            version_layout.addRow("Target Version:", version_combo)
        
        settings_layout.addLayout(version_layout)
        
        # Output file selection
        output_layout = QHBoxLayout()
        output_edit = QLineEdit()
        
        # Generate default output name
        if file_path:
            base_name = os.path.splitext(file_path)[0]
            target_version = version_combo.currentData()
            default_output = f"{base_name}_converted_{target_version.lower()}.{file_type.lower()}"
            output_edit.setText(default_output)
        
        output_browse = QPushButton("Browse...")
        output_browse.clicked.connect(lambda: _browse_output_file(output_edit, file_type))
        
        output_layout.addWidget(output_edit)
        output_layout.addWidget(output_browse)
        
        output_form_layout = QFormLayout()
        output_form_layout.addRow("Output File:", output_layout)
        settings_layout.addLayout(output_form_layout)
        
        layout.addWidget(settings_group)
        
        # Conversion options group
        options_group = QGroupBox("Conversion Options")
        options_layout = QVBoxLayout(options_group)
        
        # Create backup checkbox
        backup_check = QCheckBox("Create backup of original file")
        backup_check.setChecked(True)
        backup_check.setToolTip("Create a backup copy before conversion")
        options_layout.addWidget(backup_check)
        
        # Overwrite protection
        overwrite_check = QCheckBox("Allow overwriting existing files")
        overwrite_check.setChecked(False)
        overwrite_check.setToolTip("Allow conversion to overwrite existing output files")
        options_layout.addWidget(overwrite_check)
        
        if file_type == 'IMG':
            # IMG-specific options
            compress_check = QCheckBox("Compress V2 files (if converting to V2)")
            compress_check.setChecked(False)
            compress_check.setToolTip("Apply compression when converting to IMG V2 format")
            options_layout.addWidget(compress_check)
            
            # Verify integrity
            verify_check = QCheckBox("Verify converted file integrity")
            verify_check.setChecked(True)
            verify_check.setToolTip("Verify the converted IMG can be loaded properly")
            options_layout.addWidget(verify_check)
        
        layout.addWidget(options_group)
        
        # Conversion details group (informational)
        details_group = QGroupBox("Conversion Details")
        details_layout = QVBoxLayout(details_group)
        
        if file_type == 'IMG':
            if current_version == 'V1':
                details_text = """<b>Converting V1 → V2:</b><br>• Enables streaming support<br>• Supports larger archives (&gt;2GB)<br>• Adds compression capabilities<br>• May not work with older tools"""
            elif current_version == 'V2':
                details_text = """<b>Converting V2 → V1:</b><br>• Better compatibility with classic tools<br>• Smaller overhead<br>• Limited to ~2GB archive size<br>• No streaming support"""
            else:
                details_text = """<b>IMG Format Conversion:</b><br>• V1: Classic format, maximum compatibility<br>• V2: Modern format, larger files, streaming"""
        else:  # COL
            details_text = """<b>COL Format Conversion:</b><br>• Requires COL parser integration<br>• Maintains collision data accuracy<br>• Updates format version headers"""
        
        details_label = QLabel(details_text)
        details_label.setStyleSheet("color: palette(mid); font-size: 9pt;")
        details_layout.addWidget(details_label)
        layout.addWidget(details_group)
        
        # Update output name when version changes
        def update_output_name():
            if file_path:
                base_name = os.path.splitext(file_path)[0]
                target_version = version_combo.currentData()
                new_output = f"{base_name}_converted_{target_version.lower()}.{file_type.lower()}"
                output_edit.setText(new_output)
        
        version_combo.currentTextChanged.connect(update_output_name)
        
        # Buttons
        button_box = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        button_box.accepted.connect(dialog.accept)
        button_box.rejected.connect(dialog.reject)
        layout.addWidget(button_box)
        
        # Validate before accepting
        def validate_and_accept():
            output_path = output_edit.text().strip()
            if not output_path:
                QMessageBox.warning(dialog, "No Output File", "Please specify an output file")
                return
            
            # Check if file exists and overwrite is not allowed
            if os.path.exists(output_path) and not overwrite_check.isChecked():
                QMessageBox.warning(dialog, "File Exists", "Output file already exists. Enable 'Allow overwriting' or choose a different name.")
                return
            
            # Check if trying to overwrite source file
            if os.path.abspath(output_path) == os.path.abspath(file_path):
                QMessageBox.warning(dialog, "Same File", "Cannot overwrite the source file. Please choose a different output name.")
                return
            
            dialog.accept()
        
        button_box.button(QDialogButtonBox.StandardButton.Ok).clicked.disconnect()
        button_box.button(QDialogButtonBox.StandardButton.Ok).clicked.connect(validate_and_accept)
        
        # Execute dialog
        if dialog.exec() == QDialog.DialogCode.Accepted:
            settings = {
                'target_version': version_combo.currentData(),
                'output_path': output_edit.text().strip(),
                'create_backup': backup_check.isChecked(),
                'allow_overwrite': overwrite_check.isChecked()
            }
            
            if file_type == 'IMG':
                settings['compress_v2'] = compress_check.isChecked() if 'compress_check' in locals() else False
                settings['verify_integrity'] = verify_check.isChecked() if 'verify_check' in locals() else True
            
            return settings
        
        return None
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Convert dialog error: {str(e)}")
        return None


def _browse_output_file(output_edit: QLineEdit, file_type: str): #vers 1
    """Browse for output file location"""
    try:
        if file_type == 'IMG':
            file_filter = "IMG Files (*.img);;All Files (*.*)"
        else:  # COL
            file_filter = "COL Files (*.col);;All Files (*.*)"
        
        file_path, _ = QFileDialog.getSaveFileName(
            None,
            f"Save Converted {file_type} File As",
            output_edit.text(),
            file_filter
        )
        
        if file_path:
            output_edit.setText(file_path)
    
    except Exception as e:
        print(f"Browse output file error: {str(e)}")


def _create_conversion_backup(main_window, file_path: str) -> bool: #vers 1
    """Create backup before conversion"""
    try:
        if not os.path.exists(file_path):
            return False
        
        # Create backup directory
        backup_dir = Path("conversion_backups")
        backup_dir.mkdir(exist_ok=True)
        
        # Generate backup filename
        timestamp = __import__('datetime').datetime.now().strftime("%Y%m%d_%H%M%S")
        file_name = os.path.basename(file_path)
        backup_filename = f"{file_name}_{timestamp}.backup"
        backup_path = backup_dir / backup_filename
        
        # Copy file to backup
        shutil.copy2(file_path, backup_path)
        
        # Also backup .dir file for V1 IMG files
        dir_file = file_path.replace('.img', '.dir')
        if os.path.exists(dir_file):
            backup_dir_file = backup_path.with_suffix('.dir.backup')
            shutil.copy2(dir_file, backup_dir_file)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Created conversion backup: {backup_path}")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Backup creation error: {str(e)}")
        return False


def integrate_imgcol_convert_functions(main_window) -> bool: #vers 2
    """Integrate IMG and COL convert functions into main window"""
    try:
        # Add main convert functions
        main_window.convert_selected = lambda: convert_selected(main_window)
        main_window.convert_img_format = lambda: convert_img_format(main_window)
        main_window.convert_col_format = lambda: convert_col_format(main_window)
        
        # Add aliases for different naming conventions that GUI might use
        main_window.convert_format = main_window.convert_selected
        main_window.convert_current = main_window.convert_selected
        main_window.convert_file = main_window.convert_selected
        
        if hasattr(main_window, 'log_message'):
            integration_msg = "IMG/COL convert functions integrated with tab awareness"
            main_window.log_message(integration_msg)
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Failed to integrate IMG/COL convert functions: {str(e)}")
        return False


# Export functions
__all__ = [
    'convert_selected',
    'convert_img_format',
    'convert_col_format',
    'integrate_imgcol_convert_functions'
]
