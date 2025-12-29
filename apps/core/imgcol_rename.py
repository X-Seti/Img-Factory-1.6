#this belongs in core/ imgcol_rename.py - Version: 1
# X-Seti - September02 2025 - IMG Factory 1.5 - IMG and COL Rename Functions

"""
IMG and COL Rename Functions - Complete Implementation
Handles renaming both IMG entries and COL models with validation and dialogs
"""

import os
import re
from typing import Optional, List, Dict, Any
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QFormLayout, QLabel, QPushButton,
    QLineEdit, QMessageBox, QComboBox, QTextEdit, QGroupBox, QCheckBox,
    QDialogButtonBox
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont
from apps.methods.file_validation import validate_img_file, validate_any_file, get_selected_entries_for_operation


# IMG_Editor core integration support
try:
    from apps.components.img_integration import IMGArchive, IMGEntry
    IMG_INTEGRATION_AVAILABLE = True
except ImportError:
    IMG_INTEGRATION_AVAILABLE = False

##Methods list -
# rename_selected
# rename_img_entry
# rename_col_model
# _show_rename_dialog
# _validate_new_name
# _check_duplicate_name
# _rename_with_img_core
# _rename_with_fallback
# _get_selected_entry_safe
# _get_selected_col_model_safe
# integrate_imgcol_rename_functions

def rename_selected(main_window): #vers 1
    """Main rename function - handles both IMG entries and COL models"""
    try:
        # Check if tab system is available (to fix the missing function error)
        if not hasattr(main_window, 'validate_tab_before_operation'):
            # Fallback: check if current_img exists (for non-tab mode)
            if not hasattr(main_window, 'current_img') or not main_window.current_img:
                QMessageBox.warning(main_window, "No File", "Please open an IMG or COL file first")
                return False
            # If we have current_img, proceed with IMG rename
            return rename_img_entry(main_window)
        else:
            # Use tab-aware validation
            if not main_window.validate_tab_before_operation("Rename Selected"):
                return False
            
            # Get current file type
            file_type = main_window.get_current_file_type_from_tab()
            
            if file_type == 'IMG':
                return rename_img_entry(main_window)
            elif file_type == 'COL':
                return rename_col_model(main_window)
            else:
                QMessageBox.warning(main_window, "No File", "Please open an IMG or COL file first")
                return False
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Rename selected error: {str(e)}")
        QMessageBox.critical(main_window, "Rename Error", f"Rename failed: {str(e)}")
        return False


def rename_img_entry(main_window): #vers 1
    """Rename IMG entry with validation and IMG_Editor core support"""
    try:
        # Check if tab system is available
        if hasattr(main_window, 'validate_tab_before_operation'):
            # Validate tab and get file object
            if not main_window.validate_tab_before_operation("Rename IMG Entry"):
                return False
            
            file_object, file_type = main_window.get_current_file_from_active_tab()
        else:
            # Fallback to old method for non-tab mode
            if not hasattr(main_window, 'current_img') or not main_window.current_img:
                QMessageBox.warning(main_window, "No IMG File", "No IMG file is currently loaded")
                return False
            file_object = main_window.current_img
            file_type = 'IMG'
        
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        
        # Get selected entry - handle both tab and non-tab modes
        if hasattr(main_window, 'get_selected_entries_from_active_tab'):
            selected_entries = main_window.get_selected_entries_from_active_tab()
            if selected_entries:
                selected_entry = selected_entries[0]  # Get first selected
            else:
                selected_entry = None
        else:
            # Fallback for non-tab mode
            if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
                table = main_window.gui_layout.table
                selected_items = table.selectedItems()
                if selected_items:
                    row = selected_items[0].row()
                    if hasattr(file_object, 'entries') and 0 <= row < len(file_object.entries):
                        selected_entry = file_object.entries[row]
                    else:
                        selected_entry = None
                else:
                    selected_entry = None
            else:
                selected_entry = None

        if not selected_entry:
            QMessageBox.information(main_window, "No Selection", "Please select an IMG entry to rename")
            return False
        
        # Check if entry is pinned - prevent renaming if pinned
        if hasattr(selected_entry, 'is_pinned') and selected_entry.is_pinned:
            QMessageBox.warning(main_window, "Pinned Entry", 
                "Cannot rename a pinned entry. Unpin it first.")
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
        if not _validate_new_name(new_name, "IMG", main_window):
            return False
        
        # Check for duplicates
        if _check_duplicate_name(file_object, new_name, selected_entry):
            QMessageBox.critical(main_window, "Duplicate Name", 
                f"An entry named '{new_name}' already exists in the IMG file")
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"🏷️ Renaming IMG entry: '{current_name}' → '{new_name}'")
        
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
            
            if hasattr(main_window, 'log_message'):
                main_window.log_message("💾 Remember to rebuild IMG to save changes")
        else:
            QMessageBox.critical(main_window, "Rename Failed", 
                "Failed to rename IMG entry. Check debug log for details.")
        
        return success
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Rename IMG entry error: {str(e)}")
        QMessageBox.critical(main_window, "Rename IMG Entry Error", f"Rename IMG entry failed: {str(e)}")
        return False


def rename_col_model(main_window): #vers 1
    """Rename COL model with validation"""
    try:
        # Validate tab and get file object
        if not hasattr(main_window, 'validate_tab_before_operation'):
            QMessageBox.warning(main_window, "No File", "Please open a COL file first")
            return False
        
        if not main_window.validate_tab_before_operation("Rename COL Model"):
            return False
        
        file_object, file_type = main_window.get_current_file_from_active_tab()
        
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
        if not _validate_new_name(new_name, "COL", main_window):
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"🏷️ Renaming COL model: '{current_name}' → '{new_name}'")
        
        # Rename COL model
        try:
            if hasattr(selected_model, 'name'):
                selected_model.name = new_name
            else:
                # Add name attribute if it doesn't exist
                setattr(selected_model, 'name', new_name)
            
            # Mark as modified if possible
            if hasattr(file_object, 'modified'):
                file_object.modified = True
            
            # Refresh current tab to show changes
            if hasattr(main_window, 'refresh_current_tab_data'):
                main_window.refresh_current_tab_data()
            elif hasattr(main_window, 'refresh_table'):
                main_window.refresh_table()
            
            QMessageBox.information(main_window, "Rename Complete", 
                f"Successfully renamed COL model to '{new_name}'")
            
            if hasattr(main_window, 'log_message'):
                main_window.log_message("✅ COL model renamed successfully")
                main_window.log_message("💾 Remember to save COL file to preserve changes")
            
            return True
            
        except Exception as e:
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"❌ COL rename error: {str(e)}")
            QMessageBox.critical(main_window, "COL Rename Failed", f"Failed to rename COL model: {str(e)}")
            return False
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Rename COL model error: {str(e)}")
        QMessageBox.critical(main_window, "Rename COL Model Error", f"Rename COL model failed: {str(e)}")
        return False


def _show_rename_dialog(main_window, current_name: str, item_type: str) -> Optional[str]: #vers 1
    """Show rename dialog and get new name"""
    try:
        dialog = QDialog(main_window)
        dialog.setWindowTitle(f"Rename {item_type}")
        dialog.setModal(True)
        dialog.setMinimumWidth(400)
        
        layout = QVBoxLayout(dialog)
        
        # Info group
        info_group = QGroupBox(f"Rename {item_type}")
        info_layout = QVBoxLayout(info_group)
        
        # Current name display
        current_label = QLabel(f"Current name: <b>{current_name}</b>")
        info_layout.addWidget(current_label)
        
        # New name input
        form_layout = QFormLayout()
        name_edit = QLineEdit(current_name)
        name_edit.selectAll()  # Select all text for easy replacement
        form_layout.addRow("New name:", name_edit)
        info_layout.addLayout(form_layout)
        
        layout.addWidget(info_group)
        
        # Validation info
        if item_type == "IMG Entry":
            # Get IMG name limit from settings
            img_name_limit = 23  # Default fallback
            if hasattr(main_window, 'app_settings'):
                settings = getattr(main_window.app_settings, 'current_settings', {})
                img_name_limit = settings.get('img_name_limit', 23)  # Default to 23 if not set
            
            validation_text = f"""
• Name must be valid filename (no invalid characters)
• Must include file extension (e.g., .dff, .txd, .col)
• Maximum length: {img_name_limit} characters
• Cannot duplicate existing entry names
            """
        else:  # COL Model
            validation_text = """
• Name can be any valid identifier
• Used for reference and organization
• Maximum length: 50 characters
            """
        
        validation_label = QLabel(validation_text)
        validation_label.setStyleSheet("color: #666666; font-size: 9pt;")
        layout.addWidget(validation_label)
        
        # Buttons
        button_box = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        button_box.accepted.connect(dialog.accept)
        button_box.rejected.connect(dialog.reject)
        layout.addWidget(button_box)
        
        # Focus on name input
        name_edit.setFocus()
        
        # Execute dialog
        if dialog.exec() == QDialog.DialogCode.Accepted:
            new_name = name_edit.text().strip()
            return new_name if new_name else None
        
        return None
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Rename dialog error: {str(e)}")
        return None


def _validate_new_name(new_name: str, file_type: str, main_window=None) -> bool: #vers 1
    """Validate new name based on file type"""
    try:
        if not new_name or not new_name.strip():
            QMessageBox.warning(None, "Invalid Name", "Name cannot be empty")
            return False
        
        new_name = new_name.strip()
        
        if file_type == "IMG":
            # IMG entry validation
            if len(new_name) > 24:
                QMessageBox.warning(None, "Invalid Name", "IMG entry names must be 24 characters or less")
                return False
            
            # Check for invalid characters
            invalid_chars = r'<>:"/\|?*'
            if any(char in new_name for char in invalid_chars):
                QMessageBox.warning(None, "Invalid Name", f"Name contains invalid characters: {invalid_chars}")
                return False
            
            # Should have an extension
            if '.' not in new_name:
                reply = QMessageBox.question(None, "No Extension", 
                    "The name has no file extension. Continue anyway?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                    QMessageBox.StandardButton.No)
                if reply != QMessageBox.StandardButton.Yes:
                    return False
        
        elif file_type == "COL":
            # COL model validation
            if len(new_name) > 50:
                QMessageBox.warning(None, "Invalid Name", "COL model names must be 50 characters or less")
                return False
        
        return True
        
    except Exception:
        return False


def _check_duplicate_name(file_object, new_name: str, current_entry) -> bool: #vers 1
    """Check if new name would create duplicate"""
    try:
        if hasattr(file_object, 'entries'):
            # IMG file - check entry names
            for entry in file_object.entries:
                if entry != current_entry and getattr(entry, 'name', '') == new_name:
                    return True
        
        elif hasattr(file_object, 'models'):
            # COL file - check model names
            for model in file_object.models:
                if model != current_entry and getattr(model, 'name', '') == new_name:
                    return True
        
        return False
        
    except Exception:
        return False


def _rename_with_img_core(main_window, file_object, entry, new_name: str) -> bool: #vers 1
    """Rename using IMG_Editor core if available"""
    try:
        if IMG_INTEGRATION_AVAILABLE:
            # Convert to IMG archive format if needed
            archive = _convert_to_img_archive(file_object, main_window)
            if archive and hasattr(archive, 'entries'):
                # Use IMG_Editor core rename
                from apps.components.img_integration import Entries_and_Selection
                success = Entries_and_Selection.rename_entry(archive, entry, new_name)
                
                if success and hasattr(main_window, 'log_message'):
                    main_window.log_message("✅ Entry renamed using IMG_Editor core")
                    main_window.log_message("💾 Remember to rebuild IMG to save changes")
                
                return success
        
        # Fallback to basic rename
        return _rename_with_fallback(main_window, entry, new_name)
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Core rename error: {str(e)}")
        return _rename_with_fallback(main_window, entry, new_name)


def _rename_with_fallback(main_window, entry, new_name: str) -> bool: #vers 1
    """Fallback rename method"""
    try:
        if hasattr(main_window, 'log_message'):
            main_window.log_message("⚠️ Using fallback rename method")
        
        # Direct rename
        # Store the original name before renaming
        if not hasattr(entry, 'original_name'):
            entry.original_name = entry.name
        entry.name = new_name
        
        # Mark as modified
        if not hasattr(entry, 'is_modified'):
            entry.is_modified = True
        else:
            entry.is_modified = True
            
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
            main_window.log_message("✅ Entry renamed using fallback method")
            main_window.log_message("💾 Remember to save/rebuild file to preserve changes")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Fallback rename error: {str(e)}")
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


def integrate_imgcol_rename_functions(main_window) -> bool: #vers 1
    """Integrate IMG and COL rename functions into main window"""
    try:
        # Add main rename functions
        main_window.rename_selected = lambda: rename_selected(main_window)
        main_window.rename_img_entry = lambda: rename_img_entry(main_window)
        main_window.rename_col_model = lambda: rename_col_model(main_window)
        
        # Add aliases for different naming conventions that GUI might use
        main_window.rename_entry = main_window.rename_selected
        main_window.rename_current = main_window.rename_selected
        main_window.rename_item = main_window.rename_selected
        
        if hasattr(main_window, 'log_message'):
            integration_msg = "✅ IMG/COL rename functions integrated with tab awareness"
            if IMG_INTEGRATION_AVAILABLE:
                integration_msg += " + IMG_Editor core"
            main_window.log_message(integration_msg)
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Failed to integrate IMG/COL rename functions: {str(e)}")
        return False


# Export functions
__all__ = [
    'rename_selected',
    'rename_img_entry', 
    'rename_col_model',
    'integrate_imgcol_rename_functions'
]
