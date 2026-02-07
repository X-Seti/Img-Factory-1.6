#this belongs in core/pin_entries.py - Version: 1
# X-Seti - December01 2025 - IMG Factory 1.5 - Pin Entries Function

"""
Pin Entries Function - Handles pinning of IMG entries
"""

import os
from typing import Optional, List, Dict, Any
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QFormLayout, QLabel, QPushButton,
    QLineEdit, QMessageBox, QComboBox, QTextEdit, QGroupBox, QCheckBox,
    QTableWidgetItem
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont, QIcon
from apps.methods.tab_system import get_current_file_from_active_tab, validate_tab_before_operation

##Methods list -
# pin_selected_entries
# unpin_selected_entries
# toggle_pinned_entries
# is_entry_pinned
# get_pinned_entries
# integrate_pin_functions

def _update_status_bar_pinned_info(main_window, file_object=None):
    """Update status bar with pinned entries information"""
    try:
        if not file_object and hasattr(main_window, 'current_img'):
            file_object = main_window.current_img
        elif not file_object and hasattr(main_window, 'current_file'):
            file_object = main_window.current_file
        
        if file_object and hasattr(file_object, 'entries'):
            pinned_entries = get_pinned_entries(file_object)
            pinned_count = len(pinned_entries)
            
            if pinned_count > 0:
                status_msg = f"Pinned entries: {pinned_count} | Status: Locked from future changes"
                if hasattr(main_window, 'show_permanent_status'):
                    main_window.show_permanent_status(status_msg)
                elif hasattr(main_window, 'status_label'):
                    main_window.status_label.setText(status_msg)
                elif hasattr(main_window, 'show_status'):
                    main_window.show_status(status_msg, 0)  # 0 = permanent
            else:
                if hasattr(main_window, 'set_ready_status'):
                    main_window.set_ready_status()
        else:
            if hasattr(main_window, 'set_ready_status'):
                main_window.set_ready_status()
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error updating pinned status: {str(e)}")


def pin_selected_entries(main_window): #vers 1
    """Pin selected entries to prevent changes and add pin icon in status column"""
    try:
        # Validate tab and get file object
        if not validate_tab_before_operation(main_window, "Pin Entries"):
            return False
        
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        
        # Get selected entries
        selected_items = main_window.gui_layout.table.selectedItems()
        if not selected_items:
            main_window.log_message("No entries selected to pin")
            return False
        
        # Get selected rows
        selected_rows = set(item.row() for item in selected_items)
        
        # Mark entries as pinned
        pinned_count = 0
        for row in selected_rows:
            if 0 <= row < len(file_object.entries):
                entry = file_object.entries[row]
                # Add pinned attribute to entry
                entry.is_pinned = True
                pinned_count += 1
        
        # Save pin state to a config file
        _save_pin_config(main_window, file_object)
        
        # Refresh table to show pinned status
        if hasattr(main_window, 'refresh_img_table'):
            main_window.refresh_img_table()
        elif hasattr(main_window, 'refresh_table'):
            main_window.refresh_table()
        
        main_window.log_message(f"Pinned {pinned_count} entries")
        
        # Update status bar with pinned information
        _update_status_bar_pinned_info(main_window, file_object)
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Pin entries error: {str(e)}")
        QMessageBox.critical(main_window, "Pin Error", f"Pin entries failed: {str(e)}")
        return False


def unpin_selected_entries(main_window): #vers 1
    """Unpin selected entries"""
    try:
        # Validate tab and get file object
        if not validate_tab_before_operation(main_window, "Unpin Entries"):
            return False
        
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        
        # Get selected entries
        selected_items = main_window.gui_layout.table.selectedItems()
        if not selected_items:
            main_window.log_message("No entries selected to unpin")
            return False
        
        # Get selected rows
        selected_rows = set(item.row() for item in selected_items)
        
        # Mark entries as unpinned
        unpinned_count = 0
        for row in selected_rows:
            if 0 <= row < len(file_object.entries):
                entry = file_object.entries[row]
                # Remove pinned attribute from entry
                if hasattr(entry, 'is_pinned'):
                    delattr(entry, 'is_pinned')
                unpinned_count += 1
        
        # Save pin state to a config file
        _save_pin_config(main_window, file_object)
        
        # Refresh table to show unpinned status
        if hasattr(main_window, 'refresh_img_table'):
            main_window.refresh_img_table()
        elif hasattr(main_window, 'refresh_table'):
            main_window.refresh_table()
        
        main_window.log_message(f"Unpinned {unpinned_count} entries")
        
        # Update status bar with pinned information
        _update_status_bar_pinned_info(main_window, file_object)
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Unpin entries error: {str(e)}")
        QMessageBox.critical(main_window, "Unpin Error", f"Unpin entries failed: {str(e)}")
        return False


def toggle_pinned_entries(main_window): #vers 1
    """Toggle pin status of selected entries"""
    try:
        # Validate tab and get file object
        if not validate_tab_before_operation(main_window, "Toggle Pin Entries"):
            return False
        
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        
        # Get selected entries
        selected_items = main_window.gui_layout.table.selectedItems()
        if not selected_items:
            main_window.log_message("No entries selected to toggle pin")
            return False
        
        # Get selected rows
        selected_rows = set(item.row() for item in selected_items)
        
        # Toggle pin status for entries
        pinned_count = 0
        unpinned_count = 0
        for row in selected_rows:
            if 0 <= row < len(file_object.entries):
                entry = file_object.entries[row]
                if hasattr(entry, 'is_pinned') and entry.is_pinned:
                    # Unpin
                    delattr(entry, 'is_pinned')
                    unpinned_count += 1
                else:
                    # Pin
                    entry.is_pinned = True
                    pinned_count += 1
        
        # Save pin state to a config file
        _save_pin_config(main_window, file_object)
        
        # Refresh table to show updated pin status
        if hasattr(main_window, 'refresh_img_table'):
            main_window.refresh_img_table()
        elif hasattr(main_window, 'refresh_table'):
            main_window.refresh_table()
        
        main_window.log_message(f"Toggled pin status: {pinned_count} pinned, {unpinned_count} unpinned")
        
        # Update status bar with pinned information
        _update_status_bar_pinned_info(main_window, file_object)
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Toggle pin entries error: {str(e)}")
        QMessageBox.critical(main_window, "Toggle Pin Error", f"Toggle pin entries failed: {str(e)}")
        return False


def is_entry_pinned(entry) -> bool: #vers 1
    """Check if an entry is pinned"""
    return hasattr(entry, 'is_pinned') and entry.is_pinned


def get_pinned_entries(file_object) -> List: #vers 1
    """Get all pinned entries from a file object"""
    if not hasattr(file_object, 'entries') or not file_object.entries:
        return []
    
    return [entry for entry in file_object.entries if is_entry_pinned(entry)]


def _save_pin_config(main_window, file_object):
    """Save pin state to a config file for persistence"""
    try:
        if not hasattr(file_object, 'file_path') or not file_object.file_path:
            return False
        
        import json
        import os
        
        # Get the IMG file path without extension
        img_path = file_object.file_path
        base_path = os.path.splitext(img_path)[0]
        pin_config_path = f"{base_path}.pin"
        
        # Create a list of pinned entry names
        pinned_entries = []
        if hasattr(file_object, 'entries') and file_object.entries:
            for entry in file_object.entries:
                if is_entry_pinned(entry):
                    pinned_entries.append(entry.name if hasattr(entry, 'name') else "")
        
        # Save the pinned entries to the config file
        with open(pin_config_path, 'w', encoding='utf-8') as f:
            json.dump(pinned_entries, f)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Saved pin configuration to {pin_config_path}")
        
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error saving pin config: {str(e)}")
        return False


def _load_pin_config(main_window, file_object):
    """Load pin state from a config file"""
    try:
        if not hasattr(file_object, 'file_path') or not file_object.file_path:
            return False
        
        import json
        import os
        
        # Get the IMG file path without extension
        img_path = file_object.file_path
        base_path = os.path.splitext(img_path)[0]
        pin_config_path = f"{base_path}.pin"
        
        # Check if pin config file exists
        if not os.path.exists(pin_config_path):
            return False
        
        # Load the pinned entries from the config file
        with open(pin_config_path, 'r', encoding='utf-8') as f:
            pinned_entries = json.load(f)
        
        # Mark entries as pinned if they are in the config
        if hasattr(file_object, 'entries') and file_object.entries:
            for entry in file_object.entries:
                entry_name = entry.name if hasattr(entry, 'name') else ""
                if entry_name in pinned_entries:
                    entry.is_pinned = True
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Loaded pin configuration from {pin_config_path}")
        
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error loading pin config: {str(e)}")
        return False


def mark_entry_as_modified(entry, field_changed=None):
    """Mark an entry as modified to track changes for save operations"""
    try:
        # Mark the entry as modified
        entry.is_modified = True
        
        # Set a timestamp for when it was modified
        import time
        entry.modification_time = time.time()
        
        # Track what field was changed if specified
        if field_changed:
            if not hasattr(entry, 'modified_fields'):
                entry.modified_fields = []
            if field_changed not in entry.modified_fields:
                entry.modified_fields.append(field_changed)
        
        return True
    except Exception as e:
        print(f"Error marking entry as modified: {e}")
        return False


def integrate_pin_functions(main_window) -> bool: #vers 1
    """Integrate pin functions into main window"""
    try:
        # Add pin functions
        main_window.pin_selected_entries = lambda: pin_selected_entries(main_window)
        main_window.unpin_selected_entries = lambda: unpin_selected_entries(main_window)
        main_window.toggle_pinned_entries = lambda: toggle_pinned_entries(main_window)
        
        # Add the mark_entry_as_modified function
        main_window.mark_entry_as_modified = mark_entry_as_modified
        
        # Add aliases for different naming conventions that GUI might use
        main_window.pin_entries = main_window.pin_selected_entries
        main_window.unpin_entries = main_window.unpin_selected_entries
        main_window.toggle_pin = main_window.toggle_pinned_entries
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Pin functions integrated")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Failed to integrate pin functions: {str(e)}")
        return False


def finish_pin_operations(main_window) -> bool: #vers 1
    """Complete all pending pin operations and save the pin file
    
    Args:
        main_window: Main window instance
    
    Returns:
        True if operations completed successfully
    """
    try:
        # Validate tab and get file object
        if not validate_tab_before_operation(main_window, "Finish Pin Operations"):
            return False
        
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'IMG' or not file_object:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("Current tab does not contain an IMG file")
            return False
        
        # Save pin state to config file
        success = _save_pin_config(main_window, file_object)
        
        if success and hasattr(main_window, 'log_message'):
            main_window.log_message("Finished pin operations and saved pin configuration")
        
        return success
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error finishing pin operations: {str(e)}")
        return False


# Export functions
__all__ = [
    'pin_selected_entries',
    'unpin_selected_entries',
    'toggle_pinned_entries',
    'is_entry_pinned',
    'get_pinned_entries',
    'mark_entry_as_modified',
    'integrate_pin_functions',
    'finish_pin_operations'
]