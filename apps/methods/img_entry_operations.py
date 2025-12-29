#this belongs in methods/img_entry_operations.py - Version: 3
# X-Seti - September11 2025 - IMG Factory 1.5 - IMG Entry Operations - Ported from Modern System

"""
IMG Entry Operations - Ported working functions from modern IMG Editor system
Handles adding, removing, and managing IMG entries with proper modification tracking
"""

import os
import math
from typing import List, Optional

##Methods list -
# add_entry_safe
# add_multiple_entries
# import_file_to_img
# import_directory_to_img
# remove_entry_safe
# remove_multiple_entries
# rename_entry_safe
# integrate_entry_operations

# Constants
SECTOR_SIZE = 2048
MAX_FILENAME_LENGTH = 24

def add_entry_safe(img_archive, entry_name: str, file_data: bytes, auto_save: bool = False) -> bool:  #vers 6
    """Add entry with safe fallback - auto_save DISABLED by default - OPTIMIZED"""
    try:
        # Import debug system
        try:
            from apps.debug.debug_functions import img_debugger
        except ImportError:
            img_debugger = None
        
        # Validate inputs
        if not entry_name or not file_data:
            if img_debugger:
                img_debugger.error("Invalid entry_name or file_data provided for add_entry")
            return False
        
        # Ensure filename length is valid
        if len(entry_name.encode('ascii', errors='replace')) >= MAX_FILENAME_LENGTH:
            entry_name = entry_name[:MAX_FILENAME_LENGTH-1]  # Leave room for null terminator
            if img_debugger:
                img_debugger.debug(f"Filename truncated to: {entry_name}")
        
        # Check for duplicate entries (replace if exists)
        existing_entry = None
        if hasattr(img_archive, 'entries'):
            for i, entry in enumerate(img_archive.entries):
                if hasattr(entry, 'name') and entry.name.lower() == entry_name.lower():
                    existing_entry = entry
                    if img_debugger:
                        img_debugger.debug(f"Replacing existing entry: {entry_name}")
                    break
        
        if existing_entry:
            # Replace existing entry data
            existing_entry.data = file_data
            existing_entry.size = len(file_data)
            if hasattr(existing_entry, 'streaming_size'):
                existing_entry.streaming_size = existing_entry.size
            
            # Detect file type and RW version from data
            if hasattr(existing_entry, 'detect_rw_version'):
                existing_entry.detect_rw_version(file_data)
            
            # Mark entry as replaced for highlighting
            existing_entry.is_new_entry = True
            existing_entry.is_replaced = True
            existing_entry.modified = True
        else:
            # Create brand new entry using IMG file's add_entry method if available
            if hasattr(img_archive, 'add_entry') and callable(img_archive.add_entry):
                success = img_archive.add_entry(entry_name, file_data)
                if not success:
                    if img_debugger:
                        img_debugger.error(f"IMG file add_entry method failed for: {entry_name}")
                    return False
            else:
                # Fallback: create entry manually
                try:
                    # Import IMG classes
                    from apps.methods.img_core_classes import IMGEntry
                    
                    new_entry = IMGEntry()
                    new_entry.name = entry_name
                    new_entry.data = file_data
                    new_entry.size = len(file_data)
                    new_entry.streaming_size = new_entry.size
                    
                    # Calculate proper offset for new entry
                    new_entry.offset = _calculate_next_offset(img_archive)
                    
                    # Detect file type and RW version from data
                    if hasattr(new_entry, 'detect_rw_version'):
                        new_entry.detect_rw_version(file_data)
                    
                    # Mark as new entry for highlighting
                    new_entry.is_new_entry = True
                    new_entry.is_replaced = False
                    new_entry.modified = True
                    
                    # Add to entries list
                    if not hasattr(img_archive, 'entries'):
                        img_archive.entries = []
                    img_archive.entries.append(new_entry)
                    
                except ImportError:
                    if img_debugger:
                        img_debugger.error("Cannot import IMG classes for manual entry creation")
                    return False
        
        # Mark archive as modified
        img_archive.modified = True
        
        if img_debugger:
            img_debugger.success(f"Entry added successfully: {entry_name}")
        
        return True
        
    except Exception as e:
        if img_debugger:
            img_debugger.error(f"Failed to add entry {entry_name}: {str(e)}")
        else:
            print(f"[ERROR] add_entry_safe failed: {e}")
        return False

def _calculate_next_offset(img_file) -> int: #vers 1
    """Calculate the next available offset for a new entry - HELPER FUNCTION"""
    try:
        if not hasattr(img_file, 'entries') or not img_file.entries:
            # First entry
            version = getattr(img_file, 'version', 'V2')
            if version == 'V1':
                return 0  # Version 1 starts at beginning
            else:
                return 1  # Version 2: Reserve space for directory
        
        # Find the entry that ends the latest
        max_end = 0
        for entry in img_file.entries:
            if hasattr(entry, 'is_new_entry') and entry.is_new_entry:
                # For new entries, use a calculated end position
                entry_end = entry.offset + entry.size
            else:
                # For existing entries, use their current position
                entry_end = entry.offset + entry.size
            
            if entry_end > max_end:
                max_end = entry_end
        
        # Return next sector boundary
        return max_end  # Already in sectors
        
    except Exception:
        return 0

def add_multiple_entries(img_file, file_paths: List[str]) -> int: #vers 3
    """Add multiple entries to IMG file - PORTED FROM MODERN SYSTEM"""
    try:
        # Import debug system
        try:
            from apps.debug.debug_functions import img_debugger
        except ImportError:
            img_debugger = None
        
        if not file_paths:
            return 0
        
        added_count = 0
        
        for file_path in file_paths:
            try:
                if not os.path.exists(file_path):
                    if img_debugger:
                        img_debugger.warning(f"File not found: {file_path}")
                    continue
                
                filename = os.path.basename(file_path)
                
                # Read file data
                with open(file_path, 'rb') as f:
                    data = f.read()
                
                # Add entry
                if add_entry_safe(img_file, filename, data):
                    added_count += 1
                    if img_debugger:
                        img_debugger.info(f"Added: {filename}")
                else:
                    if img_debugger:
                        img_debugger.error(f"Failed to add: {filename}")
                        
            except Exception as e:
                if img_debugger:
                    img_debugger.error(f"Error processing {file_path}: {str(e)}")
        
        if img_debugger:
            img_debugger.success(f"Added {added_count}/{len(file_paths)} entries")
        
        return added_count
        
    except Exception as e:
        if img_debugger:
            img_debugger.error(f"Batch add failed: {e}")
        else:
            print(f"[ERROR] add_multiple_entries failed: {e}")
        return 0

def import_file_to_img(img_file, file_path: str) -> bool: #vers 3
    """Import single file into IMG - PORTED FROM MODERN SYSTEM"""
    try:
        if not os.path.exists(file_path):
            return False
        
        filename = os.path.basename(file_path)
        
        # Read file data
        with open(file_path, 'rb') as f:
            data = f.read()
        
        # Use add_entry_safe method
        return add_entry_safe(img_file, filename, data)
        
    except Exception as e:
        print(f"[ERROR] import_file_to_img failed: {e}")
        return False

def import_directory_to_img(img_file, directory_path: str, recursive: bool = False) -> int: #vers 3
    """Import directory contents into IMG - PORTED FROM MODERN SYSTEM"""
    try:
        if not os.path.exists(directory_path) or not os.path.isdir(directory_path):
            return 0
        
        imported_count = 0
        
        # Get file list
        if recursive:
            file_list = []
            for root, dirs, files in os.walk(directory_path):
                for file in files:
                    file_list.append(os.path.join(root, file))
        else:
            file_list = [
                os.path.join(directory_path, f)
                for f in os.listdir(directory_path)
                if os.path.isfile(os.path.join(directory_path, f))
            ]
        
        # Import each file
        for file_path in file_list:
            if import_file_to_img(img_file, file_path):
                imported_count += 1
        
        return imported_count
        
    except Exception as e:
        print(f"[ERROR] import_directory_to_img failed: {e}")
        return 0

def remove_entry_safe(img_file, entry_or_name) -> bool: #vers 3
    """Remove entry from IMG file with proper tracking - PORTED FROM MODERN SYSTEM"""
    try:
        # Import debug system
        try:
            from apps.debug.debug_functions import img_debugger
        except ImportError:
            img_debugger = None
        
        # Find the entry
        entry = entry_or_name
        if isinstance(entry_or_name, str):
            if hasattr(img_file, 'get_entry_by_name'):
                entry = img_file.get_entry_by_name(entry_or_name)
            else:
                # Fallback search
                entry = None
                if hasattr(img_file, 'entries'):
                    for e in img_file.entries:
                        if hasattr(e, 'name') and e.name.lower() == entry_or_name.lower():
                            entry = e
                            break
        
        if not entry or not hasattr(img_file, 'entries') or entry not in img_file.entries:
            if img_debugger:
                img_debugger.warning(f"Entry not found for removal: {entry_or_name}")
            return False
        
        # Track deletion properly
        if hasattr(entry, 'is_new_entry') and entry.is_new_entry:
            # This was a new entry that was never saved, so just remove it
            if img_debugger:
                img_debugger.debug(f"Removing new entry (not saved): {entry.name}")
        else:
            # This was an original entry from the file, so track it as deleted
            if not hasattr(img_file, 'deleted_entries'):
                img_file.deleted_entries = []
            img_file.deleted_entries.append(entry)
            if img_debugger:
                img_debugger.debug(f"Tracking deleted original entry: {entry.name}")
        
        # Remove from entries list
        img_file.entries.remove(entry)
        
        # Mark as modified
        img_file.modified = True
        
        if img_debugger:
            img_debugger.success(f"Entry removed: {entry.name}")
        
        return True
        
    except Exception as e:
        if img_debugger:
            img_debugger.error(f"Failed to remove entry: {str(e)}")
        else:
            print(f"[ERROR] remove_entry_safe failed: {e}")
        return False

def remove_multiple_entries(img_file, entries: List) -> tuple: #vers 3
    """Remove multiple entries from IMG file - PORTED FROM MODERN SYSTEM"""
    try:
        # Import debug system
        try:
            from apps.debug.debug_functions import img_debugger
        except ImportError:
            img_debugger = None
        
        if not entries:
            return 0, []
        
        success_count = 0
        failed_entries = []
        
        for entry in entries:
            if remove_entry_safe(img_file, entry):
                success_count += 1
            else:
                failed_entries.append(entry)
        
        if img_debugger:
            img_debugger.success(f"Removed {success_count}/{len(entries)} entries")
        
        return success_count, failed_entries
        
    except Exception as e:
        if img_debugger:
            img_debugger.error(f"Batch remove failed: {e}")
        else:
            print(f"[ERROR] remove_multiple_entries failed: {e}")
        return 0, entries

def rename_entry_safe(img_file, entry_or_name, new_name: str) -> bool: #vers 3
    """Rename entry in IMG file - PORTED FROM MODERN SYSTEM"""
    try:
        # Import debug system
        try:
            from apps.debug.debug_functions import img_debugger
        except ImportError:
            img_debugger = None
        
        # Find the entry
        entry = entry_or_name
        if isinstance(entry_or_name, str):
            if hasattr(img_file, 'get_entry_by_name'):
                entry = img_file.get_entry_by_name(entry_or_name)
            else:
                # Fallback search
                entry = None
                if hasattr(img_file, 'entries'):
                    for e in img_file.entries:
                        if hasattr(e, 'name') and e.name.lower() == entry_or_name.lower():
                            entry = e
                            break
        
        if not entry or not hasattr(img_file, 'entries') or entry not in img_file.entries:
            if img_debugger:
                img_debugger.warning(f"Entry not found for rename: {entry_or_name}")
            return False
        
        # Validate new name
        if not new_name or len(new_name.encode('ascii', errors='replace')) >= MAX_FILENAME_LENGTH:
            if img_debugger:
                img_debugger.error(f"Invalid new name: {new_name}")
            return False
        
        # Check for duplicate names
        if hasattr(img_file, 'get_entry_by_name'):
            existing = img_file.get_entry_by_name(new_name)
            if existing and existing != entry:
                if img_debugger:
                    img_debugger.error(f"Name already exists: {new_name}")
                return False
        
        old_name = entry.name
        entry.name = new_name
        
        # Mark as modified
        img_file.modified = True
        if hasattr(entry, 'is_new_entry'):
            entry.is_new_entry = True
        
        if img_debugger:
            img_debugger.success(f"Entry renamed: {old_name} -> {new_name}")
        
        return True
        
    except Exception as e:
        if img_debugger:
            img_debugger.error(f"Failed to rename entry: {str(e)}")
        else:
            print(f"[ERROR] rename_entry_safe failed: {e}")
        return False

def integrate_entry_operations(main_window) -> bool: #vers 3
    """Integrate entry operations functions with main window - PORTED FROM MODERN SYSTEM"""
    try:
        # Add entry operation methods to main window - FIXED: Using proper function references
        main_window.add_entry_safe = add_entry_safe
        main_window.add_multiple_entries = add_multiple_entries
        main_window.import_file_to_img = import_file_to_img
        main_window.import_directory_to_img = import_directory_to_img
        main_window.remove_entry_safe = remove_entry_safe
        main_window.remove_multiple_entries = remove_multiple_entries
        main_window.rename_entry_safe = rename_entry_safe
        
        # Add aliases that might be used elsewhere
        main_window.add_entry = main_window.add_entry_safe
        main_window.remove_entry = main_window.remove_entry_safe
        # Only set rename_entry if it doesn't already exist (to avoid overwriting user-friendly version from imgcol_rename)
        if not hasattr(main_window, 'rename_entry'):
            main_window.rename_entry = main_window.rename_entry_safe
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Entry operations integrated - PORTED from modern system")
            main_window.log_message("   • Safe entry addition with modification tracking")
            main_window.log_message("   • Proper deletion tracking for save detection")
            main_window.log_message("   • Entry renaming with validation")
            main_window.log_message("   • Batch import/export operations")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Entry operations integration failed: {str(e)}")
        return False

# Export functions
__all__ = [
    'add_entry_safe',
    'add_multiple_entries', 
    'import_file_to_img',
    'import_directory_to_img',
    'remove_entry_safe',
    'remove_multiple_entries',
    'rename_entry_safe',
    'integrate_entry_operations'
]
