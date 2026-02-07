#this belongs in methods/pin_file_manager.py - Version: 1
# X-Seti - February04 2026 - Img Factory 1.6 - PIN File Manager
"""
PIN File Manager - Handles .pin files for tracking pinned entries, dates, and metadata
Format: JSON-based .pin files stored alongside IMG files
"""

import json
import os
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any

##Methods list -
# create_pin_file
# get_pin_file_path
# integrate_pin_manager
# is_entry_pinned
# load_pin_file
# pin_entry
# save_pin_file
# unpin_entry
# update_entry_dates

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
    """Check if an entry is pinned
    
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


def finish_pin_operations(main_window) -> bool: #vers 1
    """Complete all pending pin operations and save the pin file
    
    Args:
        main_window: Main window instance
    
    Returns:
        True if operations completed successfully
    """
    try:
        # Get current IMG file path
        img_path = None
        if hasattr(main_window, 'current_img') and main_window.current_img:
            img_path = main_window.current_img.file_path
        elif hasattr(main_window, 'current_file') and main_window.current_file:
            img_path = main_window.current_file.file_path
        
        if not img_path:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("No active IMG file to finish pin operations")
            return False
        
        # Load current pin data
        pin_data = load_pin_file(img_path)
        
        # Update the last_updated timestamp
        pin_data["last_updated"] = datetime.now().isoformat()
        
        # Save the pin file
        success = save_pin_file(img_path, pin_data)
        
        if success and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Finished pin operations and saved to {get_pin_file_path(img_path)}")
        
        return success
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error finishing pin operations: {str(e)}")
        return False


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


# Export functions
__all__ = [
    'get_pin_file_path',
    'load_pin_file',
    'save_pin_file',
    'create_pin_file',
    'is_entry_pinned',
    'pin_entry',
    'unpin_entry',
    'update_entry_dates',
    'get_entry_metadata',
    'get_all_pinned_entries',
    'integrate_pin_manager',
    'finish_pin_operations'
]
