#this belongs in core/pin_entries.py - Version: 2
# X-Seti - February04 2026 - IMG Factory 1.6 - Pin Entries and File Manager
"""
Pin Entries and File Manager - Handles pinning of IMG entries and .pin files for tracking pinned entries, dates, and metadata
"""

import json
import os
from datetime import datetime
from pathlib import Path
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
# is_entry_pinned (attribute check)
# get_pinned_entries
# integrate_pin_functions
# get_pin_file_path
# load_pin_file
# save_pin_file
# create_pin_file
# pin_entry
# unpin_entry
# update_entry_dates
# get_entry_metadata
# get_all_pinned_entries
# integrate_pin_manager
# finish_pin_operations

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
    """Toggle pin status of selected entries - click to pin/unpin based on current status"""
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
            main_window.log_message("No entries selected to pin/unpin")
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


def is_entry_pinned_attribute(entry) -> bool: #vers 1
    """Check if an entry is pinned by checking its attribute"""
    return hasattr(entry, 'is_pinned') and entry.is_pinned


def get_pinned_entries(file_object) -> List: #vers 1
    """Get all pinned entries from a file object"""
    if not hasattr(file_object, 'entries') or not file_object.entries:
        return []
    
    return [entry for entry in file_object.entries if is_entry_pinned_attribute(entry)]


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
                if is_entry_pinned_attribute(entry):
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


def get_pin_file_path(img_path: str) -> str: #vers 1
    """Get the .pin file path for an IMG file
    
    Args:
        img_path: Path to IMG file
    
    Returns:
        Path to corresponding .pin file
    """
    if not img_path:
        return None
    
    # Remove .img extension and add .pin
    base_path = os.path.splitext(img_path)[0]
    return f"{base_path}.pin"


def load_pin_file(img_path: str) -> Dict[str, Any]: #vers 1
    """Load pin file data for an IMG file
    
    Args:
        img_path: Path to IMG file
    
    Returns:
        Dictionary with pin file data, or empty structure if file doesn't exist
    """
    pin_path = get_pin_file_path(img_path)
    
    if not pin_path or not os.path.exists(pin_path):
        # Return empty structure
        return {
            "version": "2.0",
            "img_file": os.path.basename(img_path) if img_path else "",
            "last_updated": datetime.now().isoformat(),
            "entries": {}
        }
    
    try:
        with open(pin_path, 'r', encoding='utf-8') as f:
            data = json.load(f)
        
        # Migrate from v1.0 if needed
        if "version" not in data or data.get("version") == "1.0":
            data = _migrate_from_v1(data, img_path)
        
        return data
        
    except Exception as e:
        print(f"Error loading pin file {pin_path}: {e}")
        # Return empty structure on error
        return {
            "version": "2.0",
            "img_file": os.path.basename(img_path) if img_path else "",
            "last_updated": datetime.now().isoformat(),
            "entries": {}
        }


def _migrate_from_v1(old_data: Any, img_path: str) -> Dict[str, Any]: #vers 1
    """Migrate old v1.0 pin file format to v2.0
    
    Old format: Simple list of filenames or dict with simple structure
    New format: Full metadata with dates
    
    Args:
        old_data: Old format data (list or simple dict)
        img_path: Path to IMG file
    
    Returns:
        Migrated v2.0 format data
    """
    new_data = {
        "version": "2.0",
        "img_file": os.path.basename(img_path),
        "last_updated": datetime.now().isoformat(),
        "entries": {}
    }
    
    # Handle list format (just filenames)
    if isinstance(old_data, list):
        for filename in old_data:
            new_data["entries"][filename] = {
                "pinned": True,
                "creation_date": None,
                "import_date": "unknown",
                "notes": "Migrated from v1.0"
            }
    
    # Handle dict format
    elif isinstance(old_data, dict):
        for filename, entry_data in old_data.items():
            if filename in ["version", "img_file", "last_updated"]:
                continue
            
            # If entry is just a boolean or simple value
            if isinstance(entry_data, bool):
                new_data["entries"][filename] = {
                    "pinned": entry_data,
                    "creation_date": None,
                    "import_date": "unknown"
                }
            # If entry already has structure
            elif isinstance(entry_data, dict):
                # Keep existing data, add missing fields
                new_entry = {
                    "pinned": entry_data.get("pinned", True),
                    "creation_date": entry_data.get("creation_date"),
                    "import_date": entry_data.get("import_date", "unknown")
                }
                if "source_file" in entry_data:
                    new_entry["source_file"] = entry_data["source_file"]
                if "notes" in entry_data:
                    new_entry["notes"] = entry_data["notes"]
                
                new_data["entries"][filename] = new_entry
    
    return new_data


def save_pin_file(img_path: str, pin_data: Dict[str, Any]) -> bool: #vers 1
    """Save pin file data
    
    Args:
        img_path: Path to IMG file
        pin_data: Pin data dictionary to save
    
    Returns:
        True if saved successfully
    """
    pin_path = get_pin_file_path(img_path)
    
    if not pin_path:
        return False
    
    try:
        # Update last_updated timestamp
        pin_data["last_updated"] = datetime.now().isoformat()
        
        # Ensure version is set
        pin_data["version"] = "2.0"
        
        # Write to file with nice formatting
        with open(pin_path, 'w', encoding='utf-8') as f:
            json.dump(pin_data, f, indent=2, ensure_ascii=False)
        
        return True
        
    except Exception as e:
        print(f"Error saving pin file {pin_path}: {e}")
        return False


def create_pin_file(img_path: str) -> bool: #vers 1
    """Create a new empty pin file for an IMG file
    
    Args:
        img_path: Path to IMG file
    
    Returns:
        True if created successfully
    """
    pin_data = {
        "version": "2.0",
        "img_file": os.path.basename(img_path),
        "last_updated": datetime.now().isoformat(),
        "entries": {}
    }
    
    return save_pin_file(img_path, pin_data)


def is_entry_pinned(img_path: str, entry_name: str) -> bool: #vers 1
    """Check if an entry is pinned by looking up in the .pin file
    
    Args:
        img_path: Path to IMG file
        entry_name: Name of entry to check
    
    Returns:
        True if entry is pinned
    """
    pin_data = load_pin_file(img_path)
    entry = pin_data.get("entries", {}).get(entry_name, {})
    return entry.get("pinned", False)


def pin_entry(img_path: str, entry_name: str, source_file: str = None, notes: str = None) -> bool: #vers 1
    """Pin an entry and save to .pin file
    
    Args:
        img_path: Path to IMG file
        entry_name: Name of entry to pin
        source_file: Optional source file path
        notes: Optional notes about the entry
    
    Returns:
        True if pinned successfully
    """
    pin_data = load_pin_file(img_path)
    
    # Get or create entry
    if entry_name not in pin_data["entries"]:
        pin_data["entries"][entry_name] = {
            "pinned": True,
            "creation_date": None,
            "import_date": datetime.now().isoformat()
        }
    else:
        # Update existing entry
        pin_data["entries"][entry_name]["pinned"] = True
    
    # Add optional metadata
    if source_file:
        pin_data["entries"][entry_name]["source_file"] = source_file
    if notes:
        pin_data["entries"][entry_name]["notes"] = notes
    
    return save_pin_file(img_path, pin_data)


def unpin_entry(img_path: str, entry_name: str) -> bool: #vers 1
    """Unpin an entry
    
    Args:
        img_path: Path to IMG file
        entry_name: Name of entry to unpin
    
    Returns:
        True if unpinned successfully
    """
    pin_data = load_pin_file(img_path)
    
    if entry_name in pin_data["entries"]:
        pin_data["entries"][entry_name]["pinned"] = False
        return save_pin_file(img_path, pin_data)
    
    return True  # Entry doesn't exist, consider it unpinned


def update_entry_dates(img_path: str, entry_name: str, creation_date: str = None, 
                       import_date: str = None, source_file: str = None) -> bool: #vers 1
    """Update entry metadata (dates, source file)
    
    Args:
        img_path: Path to IMG file
        entry_name: Name of entry
        creation_date: ISO format creation date
        import_date: ISO format import date
        source_file: Source file path
    
    Returns:
        True if updated successfully
    """
    pin_data = load_pin_file(img_path)
    
    # Get or create entry
    if entry_name not in pin_data["entries"]:
        pin_data["entries"][entry_name] = {
            "pinned": False,
            "creation_date": creation_date,
            "import_date": import_date or datetime.now().isoformat()
        }
    else:
        # Update existing entry
        if creation_date:
            pin_data["entries"][entry_name]["creation_date"] = creation_date
        if import_date:
            pin_data["entries"][entry_name]["import_date"] = import_date
    
    if source_file:
        pin_data["entries"][entry_name]["source_file"] = source_file
    
    return save_pin_file(img_path, pin_data)


def get_entry_metadata(img_path: str, entry_name: str) -> Optional[Dict[str, Any]]: #vers 1
    """Get metadata for an entry
    
    Args:
        img_path: Path to IMG file
        entry_name: Name of entry
    
    Returns:
        Entry metadata dict or None if not found
    """
    pin_data = load_pin_file(img_path)
    return pin_data.get("entries", {}).get(entry_name)


def get_all_pinned_entries(img_path: str) -> List[str]: #vers 1
    """Get list of all pinned entry names
    
    Args:
        img_path: Path to IMG file
    
    Returns:
        List of pinned entry names
    """
    pin_data = load_pin_file(img_path)
    pinned = []
    
    for entry_name, entry_data in pin_data.get("entries", {}).items():
        if entry_data.get("pinned", False):
            pinned.append(entry_name)
    
    return pinned


def integrate_pin_manager(main_window) -> bool: #vers 1
    """Integrate PIN manager into main window
    
    Args:
        main_window: Main window instance
    
    Returns:
        True if integrated successfully
    """
    try:
        # Add methods to main window
        main_window.pin_entry = lambda entry_name, source=None, notes=None: pin_entry(
            main_window.current_img.file_path if hasattr(main_window, 'current_img') and main_window.current_img else None,
            entry_name, source, notes
        )
        
        main_window.unpin_entry = lambda entry_name: unpin_entry(
            main_window.current_img.file_path if hasattr(main_window, 'current_img') and main_window.current_img else None,
            entry_name
        )
        
        main_window.is_entry_pinned = lambda entry_name: is_entry_pinned(
            main_window.current_img.file_path if hasattr(main_window, 'current_img') and main_window.current_img else None,
            entry_name
        )
        
        main_window.load_pin_file = lambda: load_pin_file(
            main_window.current_img.file_path if hasattr(main_window, 'current_img') and main_window.current_img else None
        )
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("PIN file manager integrated")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Failed to integrate PIN manager: {str(e)}")
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
    'is_entry_pinned_attribute',
    'is_entry_pinned',
    'get_pinned_entries',
    'mark_entry_as_modified',
    'integrate_pin_functions',
    'finish_pin_operations',
    'get_pin_file_path',
    'load_pin_file',
    'save_pin_file',
    'create_pin_file',
    'pin_entry',
    'unpin_entry',
    'update_entry_dates',
    'get_entry_metadata',
    'get_all_pinned_entries',
    'integrate_pin_manager'
]