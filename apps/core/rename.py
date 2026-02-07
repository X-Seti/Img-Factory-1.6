#this belongs in core/ rename.py - Version: 4
# X-Seti - August24 2025 - IMG Factory 1.5 - Rename Functions for IMG and COL

"""
Comprehensive Rename Functions - UPDATED from placeholder
Handles both IMG entries and COL models
"""

import os
import re
from typing import Optional, List, Dict, Any
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QFormLayout, QLabel, QPushButton,
    QLineEdit, QMessageBox, QComboBox, QTextEdit, QGroupBox, QCheckBox
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont
from apps.methods.file_validation import validate_img_file, validate_any_file, get_selected_entries_for_operation
try:
    from apps.methods.tab_system import get_current_file_from_active_tab, validate_tab_before_operation, get_current_file_type_from_tab
except ImportError:
    # Fallback if tab_system is not available
    def validate_tab_before_operation(main_window, operation_name):
        # Simple fallback that always allows the operation
        return True
    def get_current_file_from_active_tab(main_window):
        # Fallback that returns current_img if available
        if hasattr(main_window, 'current_img'):
            return main_window.current_img, 'IMG'
        return None, None
    def get_current_file_type_from_tab(main_window):
        # Fallback that returns IMG if current_img is available
        if hasattr(main_window, 'current_img'):
            return 'IMG'
        return None

# IMG_Editor core integration support
try:
    from apps.components.img_integration import IMGArchive, IMGEntry
    IMG_INTEGRATION_AVAILABLE = True
except ImportError:
    IMG_INTEGRATION_AVAILABLE = False

##Methods list -
# rename_entry
# rename_selected_entry
# rename_img_entry
# rename_col_model
# _show_rename_dialog
# _validate_new_name
# _rename_with_img_core
# _get_selected_entry_safe
# _get_selected_col_model_safe
# integrate_rename_functions

def rename_entry(main_window): #vers 3
    """Main rename function - handles both IMG entries and COL models"""
    try:
        # Use tab-aware validation
        if not validate_tab_before_operation(main_window, "Rename Entry"):
            return False
        # Get current file type
        file_type = get_current_file_type_from_tab(main_window)
        if file_type == 'IMG':
            return rename_img_entry(main_window)
        elif file_type == 'COL':
            return rename_col_model(main_window)
        else:
            QMessageBox.warning(main_window, "No File", "Please open an IMG or COL file first")
            return False
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Rename entry error: {str(e)}")
        QMessageBox.critical(main_window, "Rename Error", f"Rename failed: {str(e)}")
        return False


def rename_selected_entry(main_window): #vers 2
    """Alias for main rename function for GUI compatibility"""
    return rename_entry(main_window)


def rename_img_entry(main_window): #vers 3
    """Rename IMG entry with validation and IMG_Editor core support"""
    try:
        # Validate tab and get file object
        if not validate_tab_before_operation(main_window, "Rename IMG Entry"):
            return False
        
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        
        # Get selected entry
        selected_entry = _get_selected_entry_safe(main_window, file_object)
        if not selected_entry:
            QMessageBox.information(main_window, "No Selection", "Please select an IMG entry to rename")
            return False
        
        # Check if the entry is pinned
        if hasattr(selected_entry, 'is_pinned') and selected_entry.is_pinned:
            reply = QMessageBox.question(
                main_window,
                "Pinned Entry",
                "This entry is currently pinned. Would you like to unpin it first to allow renaming?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.No
            )
            
            if reply == QMessageBox.StandardButton.Yes:
                # Unpin the entry first
                delattr(selected_entry, 'is_pinned')
                
                # Also update the pin file to reflect the change
                if hasattr(main_window, 'unpin_entry'):
                    entry_name = getattr(selected_entry, 'name', '')
                    if entry_name:
                        main_window.unpin_entry(entry_name)
            else:
                QMessageBox.information(main_window, "Rename Cancelled", "Entry renaming cancelled as it is pinned.")
                return False
        
        # Get current name
        current_name = getattr(selected_entry, 'name', '')
        if not current_name:
            QMessageBox.warning(main_window, "Invalid Entry", "Selected entry has no valid name")
            return False
        
        # Show rename dialog
        new_name = _show_rename_dialog(main_window, current_name, "IMG Entry")
        if not new_name or new_name == current_name:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("Rename cancelled or no change")
            return False
        
        # Validate new name
        if not _validate_new_name(new_name, "IMG"):
            return False
        
        # Check for duplicates
        if _check_duplicate_name(file_object, new_name, selected_entry):
            QMessageBox.critical(main_window, "Duplicate Name", 
                f"An entry named '{new_name}' already exists in the IMG file")
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Renaming IMG entry: '{current_name}' → '{new_name}'")
        
        # Rename using IMG_Editor core if available
        success = _rename_with_img_core(main_window, file_object, selected_entry, new_name)
        
        if success:
            # Add undo command to the undo manager
            if hasattr(main_window, 'undo_manager'):
                from apps.core.undo_system import RenameCommand
                undo_cmd = RenameCommand(selected_entry, current_name, new_name)
                main_window.undo_manager.push_command(undo_cmd)
        
            # Mark the file object as modified
            if hasattr(file_object, 'modified'):
                file_object.modified = True
            else:
                setattr(file_object, 'modified', True)
        
            # Refresh current tab to show changes
            if hasattr(main_window, 'refresh_current_tab_data'):
                main_window.refresh_current_tab_data()
            elif hasattr(main_window, 'refresh_table'):
                main_window.refresh_table()
            
            QMessageBox.information(main_window, "Rename Complete", 
                f"Successfully renamed entry to '{new_name}'")
        else:
            QMessageBox.critical(main_window, "Rename Failed", 
                "Failed to rename IMG entry. Check debug log for details.")
        
        return success
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Rename IMG entry error: {str(e)}")
        QMessageBox.critical(main_window, "Rename IMG Entry Error", f"Rename IMG entry failed: {str(e)}")
        return False


def rename_col_model(main_window): #vers 2
    """Rename COL model with validation"""
    try:
        # Validate tab and get file object
        if not validate_tab_before_operation(main_window, "Rename COL Model"):
            return False
        
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'COL' or not file_object:
            QMessageBox.warning(main_window, "No COL File", "Current tab does not contain a COL file")
            return False
        
        # Get selected COL model
        selected_model, model_index = _get_selected_col_model_safe(main_window, file_object)
        if not selected_model:
            QMessageBox.information(main_window, "No Selection", "Please select a COL model to rename")
            return False
        
        # Get current name (COL models might not have names, use index-based naming)
        current_name = getattr(selected_model, 'name', f'model_{model_index}')
        
        # Show rename dialog
        new_name = _show_rename_dialog(main_window, current_name, "COL Model")
        if not new_name or new_name == current_name:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("Rename cancelled or no change")
            return False
        
        # Validate new name
        if not _validate_new_name(new_name, "COL"):
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Renaming COL model: '{current_name}' → '{new_name}'")
        
        # Rename COL model
        try:
            if hasattr(selected_model, 'name'):
                selected_model.name = new_name
            else:
                # Add name attribute if it doesn't exist
                selected_model.name = new_name
            
            # Mark file as modified
            if hasattr(file_object, 'modified'):
                file_object.modified = True
            
            # Refresh current tab
            if hasattr(main_window, 'refresh_current_tab_data'):
                main_window.refresh_current_tab_data()
            elif hasattr(main_window, 'refresh_table'):
                main_window.refresh_table()
            
            if hasattr(main_window, 'log_message'):
                main_window.log_message("COL model renamed successfully")
                main_window.log_message("Remember to save COL file to preserve changes")
            
            QMessageBox.information(main_window, "Rename Complete", 
                f"Successfully renamed COL model to '{new_name}'")
            
            return True
            
        except Exception as e:
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"COL model rename error: {str(e)}")
            QMessageBox.critical(main_window, "Rename Failed", f"Failed to rename COL model: {str(e)}")
            return False
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Rename COL model error: {str(e)}")
        QMessageBox.critical(main_window, "Rename COL Model Error", f"Rename COL model failed: {str(e)}")
        return False


def _show_rename_dialog(main_window, current_name: str, file_type: str) -> Optional[str]: #vers 2
    """Show rename dialog with validation"""
    try:
        dialog = QDialog(main_window)
        dialog.setWindowTitle(f"Rename {file_type}")
        dialog.setModal(True)
        dialog.resize(400, 200)
        
        layout = QVBoxLayout(dialog)
        
        # Header
        header = QLabel(f"Rename {file_type}:")
        header.setStyleSheet("font-weight: bold; font-size: 14px; margin-bottom: 10px;")
        layout.addWidget(header)
        
        # Current name display
        current_group = QGroupBox("Current Name")
        current_layout = QVBoxLayout(current_group)
        
        current_label = QLabel(current_name)
        current_label.setStyleSheet("padding: 5px; background: #f0f0f0; border: 1px solid #ccc;")
        current_label.setFont(QFont("Courier", 10))  # Monospace for better readability
        current_layout.addWidget(current_label)
        
        layout.addWidget(current_group)
        
        # New name input
        new_group = QGroupBox("New Name")
        new_layout = QVBoxLayout(new_group)
        
        name_input = QLineEdit()
        name_input.setText(current_name)
        name_input.selectAll()  # Pre-select for easy editing
        name_input.setFont(QFont("Courier", 10))
        
        # Add validation hints
        if file_type == "IMG Entry":
            hint = QLabel("IMG entries: 24 characters max, avoid special characters")
        else:
            hint = QLabel("COL models: Any descriptive name")
        
        hint.setStyleSheet("color: #666; font-size: 9px; margin-top: 5px;")
        
        new_layout.addWidget(name_input)
        new_layout.addWidget(hint)
        
        layout.addWidget(new_group)
        
        # Buttons
        button_layout = QHBoxLayout()
        
        rename_btn = QPushButton("Rename")
        rename_btn.setDefault(True)
        cancel_btn = QPushButton("Cancel")
        
        button_layout.addWidget(rename_btn)
        button_layout.addWidget(cancel_btn)
        layout.addLayout(button_layout)
        
        # Results
        new_name = None
        
        def accept_rename():
            nonlocal new_name
            new_name = name_input.text().strip()
            dialog.accept()
        
        # Connect signals
        rename_btn.clicked.connect(accept_rename)
        cancel_btn.clicked.connect(dialog.reject)
        name_input.returnPressed.connect(accept_rename)  # Enter key
        
        # Focus on input
        name_input.setFocus()
        
        if dialog.exec() == QDialog.DialogCode.Accepted:
            return new_name
        
        return None
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Rename dialog error: {str(e)}")
        return None


def _validate_new_name(new_name: str, file_type: str) -> bool: #vers 2
    """Validate new name based on file type"""
    try:
        if not new_name or not new_name.strip():
            QMessageBox.critical(None, "Invalid Name", "Name cannot be empty")
            return False
        
        new_name = new_name.strip()
        
        if file_type == "IMG":
            # IMG entry validation
            if len(new_name.encode('ascii', 'replace')) > 23:  # Leave room for null terminator
                QMessageBox.critical(None, "Invalid Name", 
                    "IMG entry names must be 23 characters or less")
                return False
            
            # Check for problematic characters
            if any(ord(c) < 32 or ord(c) > 126 for c in new_name):
                QMessageBox.critical(None, "Invalid Name", 
                    "IMG entry names must use printable ASCII characters only")
                return False
            
            # Warn about potential issues
            problematic_chars = ['/', '\\', ':', '*', '?', '"', '<', '>', '|']
            if any(char in new_name for char in problematic_chars):
                reply = QMessageBox.question(None, "Potentially Problematic Name",
                    f"The name contains characters that might cause issues: {', '.join(problematic_chars)}\n\n"
                    f"Are you sure you want to use this name?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                    QMessageBox.StandardButton.No)
                
                if reply != QMessageBox.StandardButton.Yes:
                    return False
        
        elif file_type == "COL":
            # COL model validation (more lenient)
            if len(new_name) > 100:  # Reasonable limit
                QMessageBox.critical(None, "Invalid Name", 
                    "COL model names should be 100 characters or less")
                return False
        
        return True
        
    except Exception:
        return False


def _check_duplicate_name(file_object, new_name: str, current_entry) -> bool: #vers 2
    """Check for duplicate names in IMG file"""
    try:
        entries = getattr(file_object, 'entries', [])
        
        for entry in entries:
            if entry != current_entry:  # Don't compare with itself
                entry_name = getattr(entry, 'name', '')
                if entry_name.lower() == new_name.lower():
                    return True
        
        return False
        
    except Exception:
        return False


def _rename_with_img_core(main_window, file_object, entry, new_name: str) -> bool: #vers 2
    """Rename entry using IMG_Editor core if available"""
    try:
        if IMG_INTEGRATION_AVAILABLE:
            # Try using IMG_Editor core
            try:
                # Convert to IMG_Editor archive if needed
                img_archive = _convert_to_img_archive(file_object, main_window)
                if img_archive:
                    return _rename_with_img_archive(main_window, img_archive, entry, new_name)
            except Exception as e:
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"IMG core rename failed, using fallback: {str(e)}")
        
        # Fallback to direct rename
        return _rename_with_fallback_method(main_window, entry, new_name)
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Core rename error: {str(e)}")
        return False


def _rename_with_img_archive(main_window, img_archive, entry, new_name: str) -> bool: #vers 2
    """Rename using IMG_Editor archive"""
    try:
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Using IMG_Editor core for reliable rename")
        
        # Find entry in IMG archive
        entry_name = getattr(entry, 'name', '')
        img_editor_entry = None
        
        for archive_entry in img_archive.entries:
            if getattr(archive_entry, 'name', '') == entry_name:
                img_editor_entry = archive_entry
                break
        
        if not img_editor_entry:
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Entry not found in archive: {entry_name}")
            return False
        
        # Rename entry
        img_editor_entry.name = new_name
        
        # Mark as modified
        if hasattr(img_archive, 'modified'):
            img_archive.modified = True
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Renamed entry using IMG_Editor core")
            main_window.log_message("Remember to rebuild IMG to save changes")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"IMG archive rename error: {str(e)}")
        return False


def _rename_with_fallback_method(main_window, entry, new_name: str) -> bool: #vers 2
    """Fallback method to rename entry directly with undo support"""
    try:
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Using fallback method for rename")
        
        # Get old name before renaming
        old_name = getattr(entry, 'name', '')
        
        # Store original name for change detection if not already stored
        if not hasattr(entry, 'original_name'):
            entry.original_name = old_name
        
        # Create undo command before making changes
        from apps.core.undo_system import RenameCommand
        if hasattr(main_window, 'undo_manager'):
            undo_command = RenameCommand(entry, old_name, new_name)
            main_window.undo_manager.push_command(undo_command)
        
        # Directly rename the entry's name attribute
        entry.name = new_name
        
        # Mark entry as modified if there's a modified flag
        if hasattr(entry, 'modified'):
            entry.modified = True
            
        # Also set is_modified flag that save function looks for
        if hasattr(entry, 'is_modified'):
            entry.is_modified = True
        else:
            # Create the is_modified attribute if it doesn't exist
            setattr(entry, 'is_modified', True)
            
        # Mark parent object as modified if it exists
        if hasattr(entry, 'parent') and hasattr(entry.parent, 'modified'):
            entry.parent.modified = True
        elif hasattr(main_window, 'current_file') and hasattr(main_window.current_file, 'modified'):
            main_window.current_file.modified = True
        elif hasattr(main_window, 'current_img') and hasattr(main_window.current_img, 'modified'):
            main_window.current_img.modified = True
        elif hasattr(main_window, 'current_img'):
            # Also set modified flag on current_img if it exists
            setattr(main_window.current_img, 'modified', True)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Successfully renamed entry using fallback method: {old_name} -> {new_name}")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Fallback rename error: {str(e)}")
        return False


def _get_selected_entry_safe(main_window, file_object): #vers 2
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
        
        return None
        
    except Exception:
        return None


def _get_selected_col_model_safe(main_window, file_object): #vers 2
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
        
        # Method 3: Get first model if available
        if hasattr(file_object, 'models') and file_object.models:
            return file_object.models[0], 0
        
        return None, -1
        
    except Exception:
        return None, -1


def _convert_to_img_archive(file_object, main_window):
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
        from apps.components.img_integration import IMGArchive
        archive = IMGArchive()
        if archive.load_from_file(file_path):
            return archive
        
        return None
        
    except Exception:
        return None


def integrate_rename_functions(main_window) -> bool: #vers 2
    """Integrate rename functions into main window"""
    try:
        # Add main rename functions
        main_window.rename_entry = lambda: rename_entry(main_window)
        main_window.rename_selected_entry = lambda: rename_selected_entry(main_window)
        main_window.rename_img_entry = lambda: rename_img_entry(main_window)
        main_window.rename_col_model = lambda: rename_col_model(main_window)
        
        # Add aliases for different naming conventions that GUI might use
        main_window.rename_selected = main_window.rename_entry
        main_window.rename_current = main_window.rename_entry
        main_window.rename_item = main_window.rename_entry
        
        if hasattr(main_window, 'log_message'):
            integration_msg = "Rename functions integrated with tab awareness"
            if IMG_INTEGRATION_AVAILABLE:
                integration_msg += " + IMG_Editor core"
            main_window.log_message(integration_msg)
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Failed to integrate rename functions: {str(e)}")
        return False


# Export functions
__all__ = [
    'rename_entry',
    'rename_selected_entry',
    'rename_img_entry',
    'rename_col_model',
    'integrate_rename_functions'
    #'rename_txd_texture' TODO
]
