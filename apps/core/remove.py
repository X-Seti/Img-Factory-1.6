#this belongs in core/remove.py - Version: 7
# X-Seti - November19 2025 - IMG Factory 1.5 - Remove Functions with Proper Modification Tracking
"""
Remove Functions - Remove entries with proper modification tracking for Save Entry detection
"""

import os
from typing import List, Optional
from PyQt6.QtWidgets import QMessageBox
from apps.methods.tab_system import get_current_file_from_active_tab
from apps.methods.imgcol_exists import set_context

##Methods list -
# _get_selected_entries_simple
# _get_selected_rows_from_table
# _refresh_after_removal
# _remove_entries_with_tracking
# integrate_remove_functions
# remove_entries_by_name
# remove_multiple_entries
# remove_selected_function

def remove_selected_function(main_window): #vers 5
    """Remove selected entries with proper modification tracking - TAB AWARE"""
    try:
        set_context(main_window)
        # Get file from active tab
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        # Get selected entries
        selected_entries = _get_selected_entries_simple(main_window, file_object)
        if not selected_entries:
            QMessageBox.information(main_window, "No Selection", "No entries selected for removal")
            return False
        # Confirm removal
        reply = QMessageBox.question(
            main_window,
            "Confirm Removal",
            f"Remove {len(selected_entries)} selected entries from memory?\n"
            f"Use 'Save Entry' afterwards to save changes to disk.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
        )
        if reply != QMessageBox.StandardButton.Yes:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("Remove operation cancelled by user")
            return False
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Removing {len(selected_entries)} selected entries")
        # Remove entries with proper tracking
        success = _remove_entries_with_tracking(file_object, selected_entries, main_window)
        if success:
            # Refresh after removal
            _refresh_after_removal(main_window)
            # Inform user about saving changes
            QMessageBox.information(
                main_window,
                "Remove Complete",
                f"Successfully removed {len(selected_entries)} entries from memory.\n"
                f"Use the 'Save Entry' button to save changes to disk.\n"
                f"Changes will be lost if you reload without saving."
            )
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Removed {len(selected_entries)} entries - use Save Entry to save changes")
        return success
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Remove error: {str(e)}")
        QMessageBox.critical(main_window, "Remove Error", f"Remove failed: {str(e)}")
        return False


def remove_entries_by_name(main_window, entry_names: List[str]) -> bool: #vers 2
    """Remove entries by name with proper modification tracking - TAB AWARE"""
    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        return False
    if not entry_names:
        return False
    # Find entries by name
    entries_to_remove = []
    for entry_name in entry_names:
        for entry in file_object.entries:
            if hasattr(entry, 'name') and entry.name.lower() == entry_name.lower():
                entries_to_remove.append(entry)
                break
    if not entries_to_remove:
        if hasattr(main_window, 'log_message'):
            main_window.log_message("No matching entries found for removal")
        return False
    if hasattr(main_window, 'log_message'):
        main_window.log_message(f"Removing {len(entries_to_remove)} entries by name")
    # Remove entries with proper tracking
    success = _remove_entries_with_tracking(file_object, entries_to_remove, main_window)
    if success:
        _refresh_after_removal(main_window)
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Removed {len(entries_to_remove)} entries by name")
    return success


def remove_multiple_entries(main_window, entries_to_remove: List) -> bool: #vers 2
    """Remove multiple entries programmatically with proper modification tracking - TAB AWARE"""
    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        return False
    if not entries_to_remove:
        return False
    if hasattr(main_window, 'log_message'):
        main_window.log_message(f"Removing {len(entries_to_remove)} entries programmatically")
    # Remove entries with proper tracking
    success = _remove_entries_with_tracking(file_object, entries_to_remove, main_window)
    if success:
        _refresh_after_removal(main_window)
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Removed {len(entries_to_remove)} entries programmatically")
    return success


def _remove_entries_with_tracking(file_object, entries_to_remove: List, main_window) -> bool: #vers 2
    """Remove entries with proper modification tracking - FIXES SAVE ENTRY DETECTION"""
    if not hasattr(file_object, 'entries'):
        if hasattr(main_window, 'log_message'):
            main_window.log_message("IMG file has no entries attribute")
        return False
    # Initialize deleted_entries tracking if not exists
    if not hasattr(file_object, 'deleted_entries'):
        file_object.deleted_entries = []
    removed_count = 0
    for entry in entries_to_remove:
        entry_name = getattr(entry, 'name', str(entry))
        if entry in file_object.entries:
            # Remove from current entries list
            file_object.entries.remove(entry)
            # CRITICAL: Track the deletion for Save Entry detection
            file_object.deleted_entries.append(entry)
            removed_count += 1
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Removed: {entry_name}")
        else:
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Entry not found: {entry_name}")
    # Mark file as modified
    if removed_count > 0:
        file_object.modified = True
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Successfully removed {removed_count}/{len(entries_to_remove)} entries")
    return removed_count > 0


def _get_selected_entries_simple(main_window, file_object) -> list: #vers 3
    """Get selected entries using simple working method - TAB AWARE"""
    selected_entries = []
    # Method 1: Try to get selection from main window
    if hasattr(main_window, 'get_selected_entries'):
        selected_entries = main_window.get_selected_entries()
        if selected_entries:
            return selected_entries
    # Method 2: Try to get selected rows from table
    selected_rows = _get_selected_rows_from_table(main_window)
    if selected_rows and hasattr(file_object, 'entries') and file_object.entries:
        for row in selected_rows:
            if row < len(file_object.entries):
                selected_entries.append(file_object.entries[row])
    
    # Check if any selected entries are pinned and warn user
    pinned_entries = []
    for entry in selected_entries:
        if hasattr(entry, 'is_pinned') and entry.is_pinned:
            pinned_entries.append(entry)
    
    if pinned_entries:
        pinned_names = [getattr(entry, 'name', f'Entry_{i}') for i, entry in enumerate(pinned_entries)]
        reply = QMessageBox.question(
            main_window,
            "Pinned Entries Selected",
            f"{len(pinned_entries)} selected entry(ies) are currently pinned: {', '.join(pinned_names[:5])}{', ...' if len(pinned_names) > 5 else ''}\n\n"
            f"Would you like to unpin them first to allow removal?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
        )
        
        if reply == QMessageBox.StandardButton.Yes:
            # Unpin the entries first
            for entry in pinned_entries:
                if hasattr(entry, 'is_pinned'):
                    delattr(entry, 'is_pinned')
                    
                    # Also update the pin file to reflect the change
                    if hasattr(main_window, 'unpin_entry'):
                        entry_name = getattr(entry, 'name', '')
                        if entry_name:
                            main_window.unpin_entry(entry_name)
            
            # Return only non-pinned entries for removal
            selected_entries = [entry for entry in selected_entries if not (hasattr(entry, 'is_pinned') and entry.is_pinned)]
        else:
            # User chose not to unpin, so remove pinned entries from the selection
            selected_entries = [entry for entry in selected_entries if not (hasattr(entry, 'is_pinned') and entry.is_pinned)]
            if not selected_entries:
                QMessageBox.information(main_window, "No Entries to Remove", "All selected entries are pinned and removal was cancelled.")
                return []
    
    return selected_entries


def _get_selected_rows_from_table(main_window) -> list: #vers 2
    """Get selected row numbers from entries table"""
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
                selected_items = table.selectedItems()
                if selected_items:
                    # Get unique row numbers
                    rows = set()
                    for item in selected_items:
                        rows.add(item.row())
                    selected_rows = sorted(list(rows))
                    break
        except AttributeError:
            continue
    return selected_rows


def _refresh_after_removal(main_window): #vers 2
    """Refresh UI after removal"""
    # Refresh main table
    if hasattr(main_window, 'refresh_table'):
        main_window.refresh_table()
    # Update file list
    if hasattr(main_window, 'refresh_file_list'):
        main_window.refresh_file_list()
    # Update GUI layout
    if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'refresh_file_list'):
        main_window.gui_layout.refresh_file_list()
    # Update UI state
    if hasattr(main_window, '_update_ui_for_loaded_img'):
        main_window._update_ui_for_loaded_img()
    # Update current tab data
    if hasattr(main_window, 'refresh_current_tab_data'):
        main_window.refresh_current_tab_data()
    if hasattr(main_window, 'log_message'):
        main_window.log_message("UI refreshed after removal")


def integrate_remove_functions(main_window) -> bool: #vers 3
    """Integrate remove functions with proper modification tracking - TAB AWARE"""
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
            main_window.log_message("Remove functions integrated with proper modification tracking")
            main_window.log_message("   • Tracks deleted entries for Save Entry detection")
            main_window.log_message("   • Sets modified flag properly")
            main_window.log_message("   • Supports selected entries, by name, and multiple entry removal")
            main_window.log_message("   ✅ Tab-aware file detection")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Remove integration failed: {str(e)}")
        return False

# Export functions
__all__ = [
    'remove_selected_function',
    'remove_entries_by_name',
    'remove_multiple_entries',
    'integrate_remove_functions'
]
