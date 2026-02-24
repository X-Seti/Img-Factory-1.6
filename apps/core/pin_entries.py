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

def pin_selected_entries(main_window): #vers 1
    """Pin selected entries to keep them visible and prevent changes"""
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
        
        # Refresh table to show pinned status
        if hasattr(main_window, 'refresh_img_table'):
            main_window.refresh_img_table()
        elif hasattr(main_window, 'refresh_table'):
            main_window.refresh_table()
        
        main_window.log_message(f"Pinned {pinned_count} entries")
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
        
        # Refresh table to show unpinned status
        if hasattr(main_window, 'refresh_img_table'):
            main_window.refresh_img_table()
        elif hasattr(main_window, 'refresh_table'):
            main_window.refresh_table()
        
        main_window.log_message(f"Unpinned {unpinned_count} entries")
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
        
        # Refresh table to show updated pin status
        if hasattr(main_window, 'refresh_img_table'):
            main_window.refresh_img_table()
        elif hasattr(main_window, 'refresh_table'):
            main_window.refresh_table()
        
        main_window.log_message(f"Toggled pin status: {pinned_count} pinned, {unpinned_count} unpinned")
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


def integrate_pin_functions(main_window) -> bool: #vers 1
    """Integrate pin functions into main window"""
    try:
        # Add pin functions
        main_window.pin_selected_entries = lambda: pin_selected_entries(main_window)
        main_window.unpin_selected_entries = lambda: unpin_selected_entries(main_window)
        main_window.toggle_pinned_entries = lambda: toggle_pinned_entries(main_window)
        
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


# Export functions
__all__ = [
    'pin_selected_entries',
    'unpin_selected_entries',
    'toggle_pinned_entries',
    'is_entry_pinned',
    'get_pinned_entries',
    'integrate_pin_functions'
]