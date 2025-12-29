#this belongs in core/remove.py - Version: 3
# X-Seti - September07 2025 - IMG Factory 1.5 - Remove Functions

"""
IMG Remove Functions - FIXED VERSION
Core remove functionality for IMG files using working entry operations
"""

import os
from typing import List, Optional, Any
from PyQt6.QtWidgets import (
    QMessageBox, QProgressDialog, QApplication
)
from PyQt6.QtCore import Qt

# Import working methods
from apps.methods.img_entry_operations import remove_entry_safe
from apps.methods.file_validation import validate_img_file, validate_any_file, get_selected_entries_for_operation

##Methods list -
# remove_selected_function
# remove_entries_by_name
# remove_multiple_entries
# _get_selected_entries_simple
# _get_selected_rows_from_table
# _remove_entries_direct
# _create_simple_progress_dialog
# integrate_remove_functions

def remove_selected_function(main_window, file_object, file_type): #vers 2
    """Remove selected entries - FIXED VERSION"""
    try:
        
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        
        # Get selected entries using simple method
        selected_entries = _get_selected_entries_simple(main_window, file_object)
        if not selected_entries:
            QMessageBox.information(main_window, "No Selection", "No entries selected for removal")
            return False
        
        # Show confirmation dialog
        entry_names = [getattr(entry, 'name', 'Unknown') for entry in selected_entries]
        entry_list = '\n'.join(entry_names[:10])  # Show first 10
        if len(entry_names) > 10:
            entry_list += f"\n... and {len(entry_names) - 10} more"
        
        reply = QMessageBox.question(
            main_window,
            "Confirm Removal",
            f"Remove {len(selected_entries)} selected entries?\n\n{entry_list}",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
        )
        
        if reply != QMessageBox.StandardButton.Yes:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("Remove operation cancelled by user")
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Removing {len(selected_entries)} selected entries")
        
        # Remove entries using working method
        success = _remove_entries_direct(file_object, selected_entries, main_window)
        
        if success:
            # Refresh current tab to show changes
            if hasattr(main_window, 'refresh_current_tab_data'):
                main_window.refresh_current_tab_data()
            elif hasattr(main_window, 'refresh_table'):
                main_window.refresh_table()
            elif hasattr(main_window, 'populate_entries_table'):
                main_window.populate_entries_table()
            
            # Refresh file list window to show updated entry count
            if hasattr(main_window, 'refresh_file_list'):
                main_window.refresh_file_list()
            elif hasattr(main_window, 'update_file_list'):
                main_window.update_file_list()
            elif hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'refresh_file_list'):
                main_window.gui_layout.refresh_file_list()
            
            QMessageBox.information(main_window, "Remove Complete", 
                f"Successfully removed {len(selected_entries)} entries")
        else:
            QMessageBox.critical(main_window, "Remove Failed", 
                "Failed to remove selected entries. Check debug log for details.")
        
        return success
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Remove selected error: {str(e)}")
        QMessageBox.critical(main_window, "Remove Error", f"Remove error: {str(e)}")
        return False


def remove_entries_by_name(main_window, entry_names: List[str]) -> bool: #vers 2
    """Remove entries by name programmatically - FIXED"""
    try:
        
        if file_type != 'IMG' or not file_object:
            return False
        
        if not entry_names:
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Removing {len(entry_names)} entries by name")
        
        # Use IMG Factory's native remove method if available
        removed_count = 0
        failed_count = 0
        
        for entry_name in entry_names:
            if hasattr(file_object, 'remove_entry') and callable(file_object.remove_entry):
                if file_object.remove_entry(entry_name):
                    removed_count += 1
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message(f"Removed: {entry_name}")
                else:
                    failed_count += 1
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message(f"Failed to remove: {entry_name}")
            else:
                # Fallback to remove_entry_safe function
                if remove_entry_safe(file_object, entry_name):
                    removed_count += 1
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message(f"Removed: {entry_name}")
                else:
                    failed_count += 1
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message(f"Failed to remove: {entry_name}")
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Removal complete: {removed_count} success, {failed_count} failed")
            if removed_count > 0:
                main_window.log_message("Remember to rebuild IMG to save changes")
        
        return removed_count > 0
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Remove by name error: {str(e)}")
        return False


def remove_multiple_entries(main_window, entries_to_remove: List) -> bool: #vers 2
    """Remove multiple entries programmatically - FIXED"""
    try:
        
        if file_type != 'IMG' or not file_object:
            return False
        
        if not entries_to_remove:
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Removing {len(entries_to_remove)} entries programmatically")
        
        return _remove_entries_direct(file_object, entries_to_remove, main_window)
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Remove multiple error: {str(e)}")
        return False


def _get_selected_entries_simple(main_window, file_object) -> list: #vers 2
    """Get selected entries using simple working method"""
    try:
        selected_entries = []
        
        # Method 1: Try to get selection from main window
        if hasattr(main_window, 'get_selected_entries'):
            try:
                selected_entries = main_window.get_selected_entries()
                if selected_entries:
                    return selected_entries
            except Exception:
                pass
        
        # Method 2: Try to get selected rows from table
        selected_rows = _get_selected_rows_from_table(main_window)
        if selected_rows and hasattr(file_object, 'entries') and file_object.entries:
            for row in selected_rows:
                if row < len(file_object.entries):
                    selected_entries.append(file_object.entries[row])
        
        return selected_entries
        
    except Exception:
        return []


def _get_selected_rows_from_table(main_window) -> list: #vers 2
    """Get selected row numbers from entries table"""
    try:
        selected_rows = []
        
        # Try different table attributes
        table_attrs = ['entries_table', 'table', 'gui_layout.table']
        
        for attr in table_attrs:
            try:
                if '.' in attr:
                    parts = attr.split('.')
                    table = getattr(main_window, parts[0])
                    for part in parts[1:]:
                        table = getattr(table, part)
                else:
                    table = getattr(main_window, attr)
                
                if table and hasattr(table, 'selectedItems'):
                    selected_rows = set()
                    for item in table.selectedItems():
                        selected_rows.add(item.row())
                    return list(selected_rows)
                
            except (AttributeError, TypeError):
                continue
        
        return []
        
    except Exception:
        return []


def _remove_entries_direct(file_object, entries_to_remove, main_window) -> bool: #vers 2
    """Remove entries directly using working remove_entry_safe function"""
    try:
        removed_count = 0
        failed_count = 0
        total_entries = len(entries_to_remove)
        
        # Create simple progress dialog
        progress_dialog = _create_simple_progress_dialog(main_window, total_entries)
        
        try:
            for i, entry in enumerate(entries_to_remove):
                # Update progress
                progress_dialog.setValue(i)
                entry_name = getattr(entry, 'name', f'Entry_{i}')
                progress_dialog.setLabelText(f"Removing: {entry_name}")
                
                # Use working remove_entry_safe function
                if remove_entry_safe(file_object, entry_name):
                    removed_count += 1
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message(f"Removed: {entry_name}")
                else:
                    failed_count += 1
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message(f"Failed to remove: {entry_name}")
                
                # Keep UI responsive
                QApplication.processEvents()
                
                # Check if cancelled
                if progress_dialog.wasCanceled():
                    break
            
        finally:
            progress_dialog.close()
        
        # Report results
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Removal complete: {removed_count} success, {failed_count} failed")
            if removed_count > 0:
                main_window.log_message("Remove successful - remember to rebuild IMG to save changes")
        
        return removed_count > 0
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Direct remove error: {str(e)}")
        return False


def _create_simple_progress_dialog(main_window, total_entries) -> QProgressDialog: #vers 2
    """Create simple progress dialog for remove operation"""
    try:
        progress_dialog = QProgressDialog(
            "Preparing removal...",
            "Cancel",
            0,
            total_entries,
            main_window
        )
        
        progress_dialog.setWindowTitle("Removing Entries")
        progress_dialog.setWindowModality(Qt.WindowModality.WindowModal)
        progress_dialog.setMinimumDuration(0)
        progress_dialog.setValue(0)
        
        return progress_dialog
        
    except Exception:
        # Fallback: create minimal progress dialog
        from PyQt6.QtWidgets import QProgressDialog
        progress_dialog = QProgressDialog(main_window)
        progress_dialog.setRange(0, total_entries)
        return progress_dialog


def integrate_remove_functions(main_window) -> bool: #vers 2
    """Integrate fixed remove functions into main window"""
    global file_object, file_type
    file_object = getattr(main_window, 'file_object', None)
    file_type = getattr(main_window, 'file_type', None)
    try:
        # Main remove functions
        main_window.remove_selected_function = lambda: remove_selected_function(main_window)
        main_window.remove_entries_by_name = lambda entry_names: remove_entries_by_name(main_window, entry_names)
        main_window.remove_multiple_entries = lambda entries: remove_multiple_entries(main_window, entries)
        
        # Add aliases that GUI might use
        main_window.remove_selected = main_window.remove_selected_function
        main_window.remove_selected_entries = main_window.remove_selected_function
        main_window.delete_selected = main_window.remove_selected_function
        main_window.remove_entries = main_window.remove_multiple_entries
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Fixed remove functions integrated with tab awareness")
            main_window.log_message("   • Uses working remove_entry_safe method from img_entry_operations")
            main_window.log_message("   • Supports selected entries, by name, and multiple entry removal")
            main_window.log_message("   • Remember to rebuild IMG after removal to save changes")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Remove integration failed: {str(e)}")
        return False


# Export essential functions
__all__ = [
    'remove_selected_function',
    'remove_entries_by_name',
    'remove_multiple_entries',
    'integrate_remove_functions'
]
