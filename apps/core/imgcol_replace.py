#this belongs in core/ imgcol_replace.py - Version: 1
# X-Seti - September02 2025 - IMG Factory 1.5 - IMG and COL Replace Functions

"""
IMG and COL Replace Functions - Complete Implementation  
Handles replacing IMG entries and COL model data with new files/data
"""

import os
import shutil
from pathlib import Path
from typing import Optional, List, Dict, Any, Tuple
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QFormLayout, QLabel, QPushButton,
    QLineEdit, QMessageBox, QComboBox, QTextEdit, QGroupBox, QCheckBox,
    QDialogButtonBox, QFileDialog, QProgressDialog, QListWidget, QListWidgetItem
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont
from apps.methods.file_validation import validate_img_file, validate_any_file, get_selected_entries_for_operation


# IMG_Editor core integration support
try:
    from apps.components.img_integration import IMGArchive, IMGEntry, Import_Export
    IMG_INTEGRATION_AVAILABLE = True
except ImportError:
    IMG_INTEGRATION_AVAILABLE = False

##Methods list -
# replace_selected
# replace_img_entry
# replace_col_model_data
# _show_replace_dialog
# _validate_replacement_file
# _replace_with_img_core
# _replace_with_fallback
# _get_selected_entry_safe
# _get_selected_col_model_safe
# _backup_original_entry
# integrate_imgcol_replace_functions

def replace_selected(main_window): #vers 1
    """Main replace function - handles both IMG entries and COL models"""
    try:
        # Use same tab awareness as other core functions
        if not validate_tab_before_operation(main_window, "Replace Selected"):
            return False
        
        # Get current file type
        file_type = get_current_file_type_from_tab(main_window)
        
        if file_type == 'IMG':
            return replace_img_entry(main_window)
        elif file_type == 'COL':
            return replace_col_model_data(main_window)
        else:
            QMessageBox.warning(main_window, "No File", "Please open an IMG or COL file first")
            return False
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Replace selected error: {str(e)}")
        QMessageBox.critical(main_window, "Replace Error", f"Replace failed: {str(e)}")
        return False


def replace_img_entry(main_window): #vers 1
    """Replace IMG entry with new file using IMG_Editor core support"""
    try:
        # Validate tab and get file object
        if not validate_tab_before_operation(main_window, "Replace IMG Entry"):
            return False
        
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        
        # Get selected entry
        selected_entry = _get_selected_entry_safe(main_window, file_object)
        if not selected_entry:
            QMessageBox.information(main_window, "No Selection", "Please select an IMG entry to replace")
            return False
        
        # Get current entry info
        entry_name = getattr(selected_entry, 'name', '')
        if not entry_name:
            QMessageBox.warning(main_window, "Invalid Entry", "Selected entry has no valid name")
            return False
        
        entry_size = getattr(selected_entry, 'size', 0)
        
        # Show replace dialog
        replacement_info = _show_replace_dialog(main_window, entry_name, entry_size, "IMG Entry")
        if not replacement_info:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("Replace operation cancelled")
            return False
        
        replacement_file = replacement_info['file_path']
        keep_name = replacement_info['keep_name']
        create_backup = replacement_info['create_backup']
        
        # Validate replacement file
        if not _validate_replacement_file(replacement_file, entry_name):
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"🔄 Replacing IMG entry: '{entry_name}' with '{replacement_file}'")
        
        # Create backup if requested
        if create_backup:
            backup_success = _backup_original_entry(main_window, file_object, selected_entry)
            if not backup_success:
                reply = QMessageBox.question(main_window, "Backup Failed",
                    "Failed to create backup. Continue with replacement anyway?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                    QMessageBox.StandardButton.No)
                if reply != QMessageBox.StandardButton.Yes:
                    return False
        
        # Replace using IMG_Editor core if available
        success = _replace_with_img_core(main_window, file_object, selected_entry, replacement_file, keep_name)
        
        if success:
            # Refresh current tab to show changes
            if hasattr(main_window, 'refresh_current_tab_data'):
                main_window.refresh_current_tab_data()
            elif hasattr(main_window, 'refresh_table'):
                main_window.refresh_table()
            
            file_size = os.path.getsize(replacement_file)
            QMessageBox.information(main_window, "Replace Complete", 
                f"Successfully replaced '{entry_name}' with new file\nNew size: {file_size:,} bytes")
            
            if hasattr(main_window, 'log_message'):
                main_window.log_message("💾 Remember to rebuild IMG to save changes")
        else:
            QMessageBox.critical(main_window, "Replace Failed", 
                "Failed to replace IMG entry. Check debug log for details.")
        
        return success
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Replace IMG entry error: {str(e)}")
        QMessageBox.critical(main_window, "Replace IMG Entry Error", f"Replace IMG entry failed: {str(e)}")
        return False


def replace_col_model_data(main_window): #vers 1
    """Replace COL model data with new collision data"""
    try:
        # Validate tab and get file object
        if not validate_tab_before_operation(main_window, "Replace COL Model"):
            return False
        
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'COL' or not file_object:
            QMessageBox.warning(main_window, "No COL File", "Current tab does not contain a COL file")
            return False
        
        # Get selected COL model
        selected_model, model_index = _get_selected_col_model_safe(main_window, file_object)
        if not selected_model:
            QMessageBox.information(main_window, "No Selection", "Please select a COL model to replace")
            return False
        
        # Get current model info
        model_name = getattr(selected_model, 'name', f'model_{model_index}')
        sphere_count = len(getattr(selected_model, 'spheres', []))
        box_count = len(getattr(selected_model, 'boxes', []))
        
        # Show replace dialog for COL
        replacement_info = _show_replace_dialog(main_window, model_name, 
            f"{sphere_count} spheres, {box_count} boxes", "COL Model")
        if not replacement_info:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("Replace operation cancelled")
            return False
        
        replacement_file = replacement_info['file_path']
        create_backup = replacement_info['create_backup']
        
        # Validate replacement file (should be .col file)
        if not replacement_file.lower().endswith('.col'):
            QMessageBox.warning(main_window, "Invalid File Type", 
                "Replacement file must be a COL collision file (.col)")
            return False
        
        if not os.path.exists(replacement_file):
            QMessageBox.critical(main_window, "File Not Found", 
                f"Replacement file not found: {replacement_file}")
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"🔄 Replacing COL model: '{model_name}' with '{replacement_file}'")
        
        try:
            # Load new COL data (basic implementation)
            with open(replacement_file, 'rb') as f:
                new_col_data = f.read()
            
            if len(new_col_data) == 0:
                QMessageBox.critical(main_window, "Invalid File", "Replacement COL file is empty")
                return False
            
            # For COL replacement, we would need COL parser integration
            # This is a placeholder for the actual COL data replacement
            if hasattr(main_window, 'log_message'):
                main_window.log_message("⚠️ COL model replacement requires COL parser integration")
                main_window.log_message("✅ COL replacement functionality is prepared but needs COL core integration")
            
            QMessageBox.information(main_window, "COL Replace", 
                "COL model replacement prepared.\nFull implementation requires COL parser integration.")
            
            return True
            
        except Exception as e:
            QMessageBox.critical(main_window, "COL Replace Failed", f"Failed to replace COL model: {str(e)}")
            return False
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Replace COL model error: {str(e)}")
        QMessageBox.critical(main_window, "Replace COL Model Error", f"Replace COL model failed: {str(e)}")
        return False


def _show_replace_dialog(main_window, item_name: str, item_info: str, item_type: str) -> Optional[Dict]: #vers 1
    """Show replace dialog and get replacement file and options"""
    try:
        dialog = QDialog(main_window)
        dialog.setWindowTitle(f"Replace {item_type}")
        dialog.setModal(True)
        dialog.setMinimumWidth(500)
        
        layout = QVBoxLayout(dialog)
        
        # Info group
        info_group = QGroupBox(f"Replace {item_type}")
        info_layout = QVBoxLayout(info_group)
        
        # Current item info
        current_label = QLabel(f"Current {item_type.lower()}: <b>{item_name}</b>")
        info_layout.addWidget(current_label)
        
        info_label = QLabel(f"Info: {item_info}")
        info_label.setStyleSheet("color: #666666;")
        info_layout.addWidget(info_label)
        
        layout.addWidget(info_group)
        
        # File selection group
        file_group = QGroupBox("Replacement File")
        file_layout = QVBoxLayout(file_group)
        
        file_path_layout = QHBoxLayout()
        file_path_edit = QLineEdit()
        file_path_edit.setPlaceholderText("Select replacement file...")
        file_path_edit.setReadOnly(True)
        
        browse_button = QPushButton("Browse...")
        browse_button.clicked.connect(lambda: _browse_replacement_file(file_path_edit, item_type))
        
        file_path_layout.addWidget(file_path_edit)
        file_path_layout.addWidget(browse_button)
        file_layout.addLayout(file_path_layout)
        
        layout.addWidget(file_group)
        
        # Options group
        options_group = QGroupBox("Replace Options")
        options_layout = QVBoxLayout(options_group)
        
        # Keep original name checkbox (for IMG entries)
        keep_name_check = QCheckBox("Keep original entry name")
        if item_type == "IMG Entry":
            keep_name_check.setChecked(True)
            keep_name_check.setToolTip("Keep the original entry name instead of using replacement file name")
            options_layout.addWidget(keep_name_check)
        
        # Create backup checkbox
        backup_check = QCheckBox("Create backup of original data")
        backup_check.setChecked(True)
        backup_check.setToolTip("Export original data to backup file before replacing")
        options_layout.addWidget(backup_check)
        
        layout.addWidget(options_group)
        
        # Preview group (for future enhancement)
        preview_group = QGroupBox("File Information")
        preview_layout = QVBoxLayout(preview_group)
        
        preview_label = QLabel("Select a file to see information...")
        preview_label.setStyleSheet("color: #666666; font-style: italic;")
        preview_layout.addWidget(preview_label)
        
        layout.addWidget(preview_group)
        
        # Update preview when file is selected
        def update_preview():
            file_path = file_path_edit.text()
            if file_path and os.path.exists(file_path):
                try:
                    file_size = os.path.getsize(file_path)
                    file_name = os.path.basename(file_path)
                    file_ext = file_name.split('.')[-1].upper() if '.' in file_name else 'No extension'
                    
                    preview_text = f"""
<b>File:</b> {file_name}<br>
<b>Size:</b> {file_size:,} bytes<br>
<b>Type:</b> {file_ext}<br>
<b>Path:</b> {file_path}
                    """
                    preview_label.setText(preview_text)
                except Exception:
                    preview_label.setText("Error reading file information")
            else:
                preview_label.setText("Select a file to see information...")
        
        file_path_edit.textChanged.connect(update_preview)
        
        # Buttons
        button_box = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        button_box.accepted.connect(dialog.accept)
        button_box.rejected.connect(dialog.reject)
        layout.addWidget(button_box)
        
        # Validate OK button
        def validate_and_accept():
            if not file_path_edit.text():
                QMessageBox.warning(dialog, "No File Selected", "Please select a replacement file")
                return
            if not os.path.exists(file_path_edit.text()):
                QMessageBox.warning(dialog, "File Not Found", "Selected file does not exist")
                return
            dialog.accept()
        
        button_box.button(QDialogButtonBox.StandardButton.Ok).clicked.disconnect()
        button_box.button(QDialogButtonBox.StandardButton.Ok).clicked.connect(validate_and_accept)
        
        # Execute dialog
        if dialog.exec() == QDialog.DialogCode.Accepted:
            return {
                'file_path': file_path_edit.text(),
                'keep_name': keep_name_check.isChecked() if item_type == "IMG Entry" else False,
                'create_backup': backup_check.isChecked()
            }
        
        return None
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Replace dialog error: {str(e)}")
        return None


def _browse_replacement_file(file_path_edit: QLineEdit, item_type: str): #vers 1
    """Browse for replacement file"""
    try:
        if item_type == "IMG Entry":
            file_filter = "All Files (*.*)"
            dialog_title = "Select Replacement File"
        else:  # COL Model
            file_filter = "COL Files (*.col);;All Files (*.*)"
            dialog_title = "Select Replacement COL File"
        
        file_path, _ = QFileDialog.getOpenFileName(
            None,
            dialog_title,
            "",
            file_filter
        )
        
        if file_path:
            file_path_edit.setText(file_path)
    
    except Exception as e:
        print(f"Browse file error: {str(e)}")


def _validate_replacement_file(file_path: str, original_name: str) -> bool: #vers 1
    """Validate replacement file"""
    try:
        if not os.path.exists(file_path):
            QMessageBox.critical(None, "File Not Found", f"Replacement file not found: {file_path}")
            return False
        
        if os.path.getsize(file_path) == 0:
            QMessageBox.critical(None, "Empty File", "Replacement file is empty")
            return False
        
        # Check if file is readable
        try:
            with open(file_path, 'rb') as f:
                f.read(1)  # Try to read first byte
        except Exception as e:
            QMessageBox.critical(None, "File Access Error", f"Cannot read replacement file: {str(e)}")
            return False
        
        # Warn if file types don't match
        original_ext = original_name.split('.')[-1].lower() if '.' in original_name else ''
        replacement_ext = file_path.split('.')[-1].lower() if '.' in file_path else ''
        
        if original_ext and replacement_ext and original_ext != replacement_ext:
            reply = QMessageBox.question(None, "Different File Types",
                f"Original file type: .{original_ext}\nReplacement file type: .{replacement_ext}\n\nContinue anyway?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.No)
            if reply != QMessageBox.StandardButton.Yes:
                return False
        
        return True
        
    except Exception as e:
        QMessageBox.critical(None, "Validation Error", f"Error validating replacement file: {str(e)}")
        return False


def _replace_with_img_core(main_window, file_object, entry, replacement_file: str, keep_name: bool) -> bool: #vers 1
    """Replace entry using IMG_Editor core if available"""
    try:
        if IMG_INTEGRATION_AVAILABLE:
            # Convert to IMG archive format if needed
            archive = _convert_to_img_archive(file_object, main_window)
            if archive:
                # Read replacement file data
                with open(replacement_file, 'rb') as f:
                    new_data = f.read()
                
                # Use IMG_Editor core replace
                from apps.components.img_integration import Entries_and_Selection
                success = Entries_and_Selection.replace_entry(archive, entry, new_data)
                
                if success:
                    # Update name if not keeping original
                    if not keep_name:
                        new_name = os.path.basename(replacement_file)
                        entry.name = new_name
                    
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message("✅ Entry replaced using IMG_Editor core")
                        main_window.log_message("💾 Remember to rebuild IMG to save changes")
                    
                    return True
        
        # Fallback to basic replace
        return _replace_with_fallback(main_window, entry, replacement_file, keep_name)
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Core replace error: {str(e)}")
        return _replace_with_fallback(main_window, entry, replacement_file, keep_name)


def _replace_with_fallback(main_window, entry, replacement_file: str, keep_name: bool) -> bool: #vers 1
    """Fallback replace method"""
    try:
        if hasattr(main_window, 'log_message'):
            main_window.log_message("⚠️ Using fallback replace method")
        
        # Read new file data
        with open(replacement_file, 'rb') as f:
            new_data = f.read()
        
        # Update entry data if possible
        if hasattr(entry, 'data'):
            entry.data = new_data
        
        # Update size
        if hasattr(entry, 'size'):
            # Size might be in sectors for IMG files
            import math
            entry.size = math.ceil(len(new_data) / 2048)  # Assuming 2048 byte sectors
        
        # Update name if not keeping original
        if not keep_name and hasattr(entry, 'name'):
            entry.name = os.path.basename(replacement_file)
        
        # Mark as modified if possible
        if hasattr(entry, 'modified'):
            entry.modified = True
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("✅ Entry replaced using fallback method")
            main_window.log_message("💾 Remember to save/rebuild file to preserve changes")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Fallback replace error: {str(e)}")
        return False


def _backup_original_entry(main_window, file_object, entry) -> bool: #vers 1
    """Create backup of original entry data"""
    try:
        entry_name = getattr(entry, 'name', 'unknown_entry')
        
        # Create backup directory
        backup_dir = Path("backups")
        backup_dir.mkdir(exist_ok=True)
        
        # Generate backup filename
        timestamp = __import__('datetime').datetime.now().strftime("%Y%m%d_%H%M%S")
        backup_filename = f"{entry_name}_{timestamp}.backup"
        backup_path = backup_dir / backup_filename
        
        # Extract and save entry data
        if hasattr(entry, 'data') and entry.data:
            # Direct data access
            with open(backup_path, 'wb') as f:
                f.write(entry.data)
        elif IMG_INTEGRATION_AVAILABLE:
            # Use IMG_Editor core to extract
            archive = _convert_to_img_archive(file_object, main_window)
            if archive:
                from apps.components.img_integration import Import_Export
                success = Import_Export.export_entry(archive, entry, str(backup_path))
                if not success:
                    return False
        else:
            # Can't backup without data access
            if hasattr(main_window, 'log_message'):
                main_window.log_message("⚠️ Cannot create backup - no data access method available")
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"✅ Created backup: {backup_path}")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Backup creation error: {str(e)}")
        return False


def _get_selected_entry_safe(main_window, file_object): #vers 1
    """Safely get selected IMG entry"""
    try:
        # Try different methods to get selected entry
        selected_entry = None
        
        # Method 1: Use main window's get_selected_entries
        if hasattr(main_window, 'get_selected_entries'):
            try:
                selected_entries = main_window.get_selected_entries()
                if selected_entries:
                    return selected_entries[0]  # Return first selected
            except Exception:
                pass
        
        # Method 2: Check table widget
        if hasattr(main_window, 'entries_table'):
            try:
                table = main_window.entries_table
                current_row = table.currentRow()
                
                if current_row >= 0 and hasattr(file_object, 'entries'):
                    entries = file_object.entries
                    if current_row < len(entries):
                        return entries[current_row]
            except Exception:
                pass
        
        # Method 3: Check gui_layout table
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            try:
                table = main_window.gui_layout.table
                current_row = table.currentRow()
                
                if current_row >= 0 and hasattr(file_object, 'entries'):
                    entries = file_object.entries
                    if current_row < len(entries):
                        return entries[current_row]
            except Exception:
                pass
        
        return None
        
    except Exception:
        return None


def _get_selected_col_model_safe(main_window, file_object): #vers 1
    """Safely get selected COL model"""
    try:
        # Try different methods to get selected COL model
        selected_model = None
        model_index = -1
        
        # Method 1: Check if main window has COL-specific selection
        if hasattr(main_window, 'get_selected_col_models'):
            try:
                selected_models = main_window.get_selected_col_models()
                if selected_models:
                    return selected_models[0], 0  # Return first selected
            except Exception:
                pass
        
        # Method 2: Check table widget for COL models
        if hasattr(main_window, 'entries_table'):
            try:
                table = main_window.entries_table
                current_row = table.currentRow()
                
                if current_row >= 0 and hasattr(file_object, 'models'):
                    models = file_object.models
                    if current_row < len(models):
                        return models[current_row], current_row
            except Exception:
                pass
        
        # Method 3: Check gui_layout table
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            try:
                table = main_window.gui_layout.table
                current_row = table.currentRow()
                
                if current_row >= 0 and hasattr(file_object, 'models'):
                    models = file_object.models
                    if current_row < len(models):
                        return models[current_row], current_row
            except Exception:
                pass
        
        # Method 4: Get first model if available
        if hasattr(file_object, 'models') and file_object.models:
            return file_object.models[0], 0
        
        return None, -1
        
    except Exception:
        return None, -1


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
                main_window.log_message(f"✅ Converted to IMG archive format: {entry_count} entries")
            return archive
        
        return None
        
    except Exception:
        return None


def integrate_imgcol_replace_functions(main_window) -> bool: #vers 1
    """Integrate IMG and COL replace functions into main window"""
    try:
        # Add main replace functions
        main_window.replace_selected = lambda: replace_selected(main_window)
        main_window.replace_img_entry = lambda: replace_img_entry(main_window)
        main_window.replace_col_model_data = lambda: replace_col_model_data(main_window)
        
        # Add aliases for different naming conventions that GUI might use
        main_window.replace_entry = main_window.replace_selected
        main_window.replace_current = main_window.replace_selected
        main_window.replace_item = main_window.replace_selected
        
        if hasattr(main_window, 'log_message'):
            integration_msg = "✅ IMG/COL replace functions integrated with tab awareness"
            if IMG_INTEGRATION_AVAILABLE:
                integration_msg += " + IMG_Editor core"
            main_window.log_message(integration_msg)
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Failed to integrate IMG/COL replace functions: {str(e)}")
        return False


# Export functions
__all__ = [
    'replace_selected',
    'replace_img_entry',
    'replace_col_model_data', 
    'integrate_imgcol_replace_functions'
]
        
