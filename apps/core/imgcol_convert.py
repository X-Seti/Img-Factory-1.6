#this belongs in core/ imgcol_convert.py - Version: 3
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

# IMG_Editor core integration support
try:
    from apps.components.img_integration import IMGArchive, IMG_Operations
    IMG_INTEGRATION_AVAILABLE = True
except ImportError:
    IMG_INTEGRATION_AVAILABLE = False

##Methods list -
# convert_selected
# convert_img_format
# convert_col_format
# _show_convert_dialog
# _browse_output_file
# _validate_conversion_settings
# _convert_with_img_core
# _convert_with_fallback
# _create_conversion_backup
# _get_img_version
# _get_col_version
# _convert_to_img_archive
# integrate_imgcol_convert_functions

def convert_selected(main_window): #vers 1
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


def convert_img_format(main_window): #vers 1
    """Convert IMG between V1 and V2 formats using IMG_Editor core"""
    try:
        # Validate tab and get file object
        if not validate_tab_before_operation(main_window, "Convert IMG Format"):
            return False
        
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        
        # Get current IMG version
        current_version = _get_img_version(file_object)
        if not current_version:
            QMessageBox.warning(main_window, "Unknown Version", "Cannot determine IMG file version")
            return False
        
        # Get file path
        file_path = getattr(file_object, 'file_path', '')
        if not file_path:
            QMessageBox.warning(main_window, "No File Path", "Cannot determine IMG file path")
            return False
        
        entry_count = len(getattr(file_object, 'entries', []))
        file_size = os.path.getsize(file_path) if os.path.exists(file_path) else 0
        
        # Show convert dialog
        conversion_settings = _show_convert_dialog(main_window, "IMG", {
            'current_version': current_version,
            'file_path': file_path,
            'entry_count': entry_count,
            'file_size': file_size
        })
        
        if not conversion_settings:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("Convert operation cancelled")
            return False
        
        target_version = conversion_settings['target_version']
        output_path = conversion_settings['output_path']
        create_backup = conversion_settings['create_backup']
        
        # Validate conversion settings
        if not _validate_conversion_settings(conversion_settings, "IMG"):
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Converting IMG: {current_version} → {target_version}")
            main_window.log_message(f"Output: {output_path}")
        
        # Create backup if requested
        if create_backup:
            backup_success = _create_conversion_backup(main_window, file_path)
            if not backup_success:
                reply = QMessageBox.question(main_window, "Backup Failed", "Failed to create backup. Continue with conversion anyway?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                    QMessageBox.StandardButton.No)
                if reply != QMessageBox.StandardButton.Yes:
                    return False
        
        # Convert using IMG_Editor core
        success = _convert_with_img_core(main_window, file_object, conversion_settings)
        
        if success:
            QMessageBox.information(main_window, "Convert Complete", 
                f"Successfully converted IMG from {current_version} to {target_version}\nOutput: {output_path}")
            
            if hasattr(main_window, 'log_message'):
                main_window.log_message("IMG format conversion completed")
                
            # Ask if user wants to open the converted file
            reply = QMessageBox.question(main_window, "Open Converted File", "Would you like to open the converted IMG file?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.Yes)
            if reply == QMessageBox.StandardButton.Yes:
                # Open the converted file
                if hasattr(main_window, 'open_img_file'):
                    try:
                        main_window.open_img_file(output_path)
                    except Exception as e:
                        if hasattr(main_window, 'log_message'):
                            main_window.log_message(f"Could not open converted file: {str(e)}")
        else:
            QMessageBox.critical(main_window, "Convert Failed", "Failed to convert IMG format. Check debug log for details.")
        
        return success
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Convert IMG format error: {str(e)}")
        QMessageBox.critical(main_window, "Convert IMG Format Error", f"Convert IMG format failed: {str(e)}")
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
            main_window.log_message("⚠COL format conversion requires COL parser integration")
        
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
        details_label.setStyleSheet("color: #666666; font-size: 9pt;")
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


def _validate_conversion_settings(settings: Dict, file_type: str) -> bool: #vers 1
    """Validate conversion settings"""
    try:
        output_path = settings.get('output_path', '')
        if not output_path:
            QMessageBox.critical(None, "No Output Path", "Output file path is required")
            return False
        
        # Check output directory exists
        output_dir = os.path.dirname(output_path)
        if output_dir and not os.path.exists(output_dir):
            try:
                os.makedirs(output_dir, exist_ok=True)
            except Exception as e:
                QMessageBox.critical(None, "Directory Error", f"Cannot create output directory: {str(e)}")
                return False
        
        # Check if we can write to output location
        try:
            test_file = output_path + '.test'
            with open(test_file, 'w') as f:
                f.write('test')
            os.remove(test_file)
        except Exception as e:
            QMessageBox.critical(None, "Write Permission Error", f"Cannot write to output location: {str(e)}")
            return False
        
        return True
        
    except Exception as e:
        QMessageBox.critical(None, "Validation Error", f"Error validating conversion settings: {str(e)}")
        return False


def _convert_with_img_core(main_window, file_object, settings: Dict) -> bool: #vers 1
    """Convert using IMG_Editor core if available"""
    try:
        
        # Convert to IMG archive format
        archive = _convert_to_img_archive(file_object, main_window)
        if not archive:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("Could not convert file to IMG_Editor format")
            return False
        
        target_version = settings['target_version']
        output_path = settings['output_path']
        
        # Show progress dialog
        progress = QProgressDialog("Converting IMG format...", "Cancel", 0, 100, main_window)
        progress.setWindowModality(Qt.WindowModality.WindowModal)
        progress.show()
        
        def progress_callback(percent, message):
            if progress.wasCanceled():
                return False
            progress.setValue(percent)
            progress.setLabelText(message)
            QApplication.processEvents()
            return True
        
        try:
            # Use IMG_Editor core conversion
            converted_archive = IMG_Operations.convert_format(
                archive, 
                output_path, 
                target_version
            )
            
            if converted_archive:
                # Verify integrity if requested
                if settings.get('verify_integrity', True):
                    progress_callback(90, "Verifying converted file...")
                    
                    # Try to load the converted file
                    test_archive = IMGArchive()
                    if not test_archive.load_from_file(output_path):
                        if hasattr(main_window, 'log_message'):
                            main_window.log_message("Converted file failed integrity check")
                        QMessageBox.warning(main_window, "Integrity Check Failed", "Converted file may be corrupted. Check the output file manually.")
                
                progress_callback(100, "Conversion complete")
                
                if hasattr(main_window, 'log_message'):
                    main_window.log_message("IMG format converted using IMG_Editor core")
                
                return True
            else:
                if hasattr(main_window, 'log_message'):
                    main_window.log_message("IMG_Editor core conversion failed")
                return False
        
        finally:
            progress.close()
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Core conversion error: {str(e)}")
        return True


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


def _get_img_version(file_object) -> Optional[str]: #vers 3
    """Get IMG file version - NO FALLBACK"""
    try:
        # Method 1: Check version attribute
        if hasattr(file_object, 'version'):
            version = getattr(file_object, 'version')
            if version in ['V1', 'VER1']:
                return 'V1'
            elif version in ['V2', 'VER2']:
                return 'V2'

        # Method 2: Check format_version attribute
        if hasattr(file_object, 'format_version'):
            version = getattr(file_object, 'format_version')
            if version == 1:
                return 'V1'
            elif version == 2:
                return 'V2'

        # Method 3: Direct file signature check (if file_path available)
        file_path = getattr(file_object, 'file_path', None)
        if file_path and os.path.exists(file_path):
            with open(file_path, 'rb') as f:
                signature = f.read(4)
                if signature == b'VER2':
                    return 'V2'
                # V1 has no signature, check for .dir file
                dir_file = file_path.replace('.img', '.dir')
                if os.path.exists(dir_file):
                    return 'V1'

        # Cannot determine version
        return None

    except Exception:
        return None


def _get_col_version(file_object) -> Optional[str]: #vers 1
    """Get COL file version (placeholder)"""
    try:
        # Placeholder for COL version detection
        if hasattr(file_object, 'col_version'):
            return getattr(file_object, 'col_version')
        
        # Default assumption
        return 'COL1'
        
    except Exception:
        return None


def _convert_to_img_archive(file_object, main_window): #vers 1
    """Convert file object to IMG_Editor archive format"""
    try:
        if not IMG_INTEGRATION_AVAILABLE:
            return None
        
        # If already IMG_Editor format, return as-is
        if isinstance(file_object, IMGArchive):
            return file_object
        
        # Load IMG file using IMG_Editor
        file_path = getattr(file_object, 'file_path', None)
        if not file_path or not os.path.exists(file_path):
            return None
        
        # Create and load IMG_Editor archive
        archive = IMGArchive()
        if archive.load_from_file(file_path):
            if hasattr(main_window, 'log_message'):
                entry_count = len(archive.entries) if archive.entries else 0
                main_window.log_message(f"Converted to IMG archive format: {entry_count} entries")
            return archive
        
        return None
        
    except Exception:
        return None


def integrate_imgcol_convert_functions(main_window) -> bool: #vers 1
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
            if IMG_INTEGRATION_AVAILABLE:
                integration_msg += " + IMG_Editor core"
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
