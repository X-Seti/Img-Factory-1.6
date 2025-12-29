#this belongs in core/utils.py - Version: 2
# X-Seti - July15 2025 - Img Factory 1.5
# Utility functions for IMG Factory

import os
import shutil
import json
from typing import List, Optional, Dict, Any

## Method list
# get_selected_entries
# validate_import_files
# create_backup_before_import
# get_import_statistics
# get_file_type_subfolder
# parse_ide_file
# find_matching_entries
# entry_exists_in_img
# get_project_folder
# get_export_folder
# validate_img_file
# get_img_info
# log_operation
# integrate_core_functions


def get_selected_entries(main_window): #vers 3
    """Get currently selected entries from the table"""
    try:
        selected_entries = []
        
        # Try multiple ways to get the table
        table = None
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
        elif hasattr(main_window, 'table'):
            table = main_window.table
        elif hasattr(main_window, 'entries_table'):
            table = main_window.entries_table
        
        if not table:
            main_window.log_message("Could not find table widget")
            return []
        
        # Get selected rows using selection model
        selection_model = table.selectionModel()
        if selection_model:
            selected_indexes = selection_model.selectedRows()
            selected_rows = [index.row() for index in selected_indexes]
        else:
            # Fallback: Use selectedItems
            selected_rows = set()
            for item in table.selectedItems():
                selected_rows.add(item.row())
            selected_rows = list(selected_rows)
        
        # Get entries from IMG file
        if hasattr(main_window, 'current_img') and main_window.current_img and hasattr(main_window.current_img, 'entries'):
            for row in selected_rows:
                if 0 <= row < len(main_window.current_img.entries):
                    selected_entries.append(main_window.current_img.entries[row])
        
        return selected_entries
        
    except Exception as e:
        main_window.log_message(f"Error getting selected entries: {str(e)}")
        return []


def validate_import_files(files_to_import: List[str]) -> List[str]: #vers 3
    """Validate files before import"""
    valid_files = []
    
    for file_path in files_to_import:
        try:
            if os.path.exists(file_path) and os.path.getsize(file_path) > 0:
                # Try to read file
                with open(file_path, 'rb') as f:
                    f.read(10)  # Read first 10 bytes
                valid_files.append(file_path)
        except Exception:
            continue
    
    return valid_files


def create_backup_before_import(main_window) -> bool: #vers 5
    """Create backup of current IMG file before import"""
    try:
        if hasattr(main_window, 'current_img') and hasattr(main_window.current_img, 'file_path'):
            img_path = main_window.current_img.file_path
            backup_path = img_path + '.backup'
            shutil.copy2(img_path, backup_path)
            main_window.log_message(f"Backup created: {backup_path}")
            return True
    except Exception as e:
        main_window.log_message(f"Backup failed: {str(e)}")
        return False


def get_import_statistics(main_window) -> Dict[str, Any]: #vers 4
    """Get import statistics"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            total_entries = len(main_window.current_img.entries)
            
            # Count by file type
            type_counts = {}
            for entry in main_window.current_img.entries:
                entry_name = getattr(entry, 'name', '')
                ext = os.path.splitext(entry_name)[1].lower()
                type_counts[ext] = type_counts.get(ext, 0) + 1
            
            return {
                'total_entries': total_entries,
                'type_counts': type_counts
            }
    except Exception:
        pass
    
    return {'total_entries': 0, 'type_counts': {}}


def get_file_type_subfolder(filename: str) -> str: #vers 4
    """Get organized subfolder by file type"""
    ext = os.path.splitext(filename)[1].lower()
    
    if ext in ['.dff', '.3ds', '.obj']:
        return 'models'
    elif ext in ['.txd', '.png', '.jpg', '.bmp', '.tga']:
        return 'textures'
    elif ext in ['.col']:
        return 'collision'
    elif ext in ['.rrr']:
        return 'rrr'
    elif ext in ['.ipf']:
        return 'ipf'
    elif ext in ['.wav', '.mp3', '.ogg']:
        return 'audio'
    elif ext in ['.scm', '.cs']:
        return 'scripts'
    elif ext in ['.ide']:
        return 'definitions'
    elif ext in ['.dat']:
        return 'data'
    else:
        return 'other'


def parse_ide_file(ide_file_path: str) -> List[str]: #vers 3
    """Parse IDE file to get filenames"""
    filenames = []
    
    try:
        with open(ide_file_path, 'r', encoding='utf-8', errors='ignore') as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#') or line.startswith('//'):
                    continue

                # Extract filename from IDE line
                # Common IDE formats:
                # filename.dff, texdict, 100, 0, 0, 0
                # filename.txd, texdict, 100, 0, 0, 0
                parts = line.split(',')
                if parts:
                    filename = parts[0].strip().strip('"\'')
                    if filename:
                        filenames.append(filename)

    except Exception:
        pass
    
    return filenames


def find_matching_entries(main_window, entry_names: List[str]) -> List[Any]: #vers
    """Find entries by name in IMG"""
    matching_entries = []
    
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            for entry_name in entry_names:
                for img_entry in main_window.current_img.entries:
                    img_entry_name = getattr(img_entry, 'name', '')
                    if img_entry_name.lower() == entry_name.lower():
                        matching_entries.append(img_entry)
                        break
    except Exception:
        pass
    
    return matching_entries


def entry_exists_in_img(main_window, filename: str) -> bool: #vers 3
    """Check if entry already exists in IMG"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            for entry in main_window.current_img.entries:
                entry_name = getattr(entry, 'name', '')
                if entry_name.lower() == filename.lower():
                    return True
        return False
    except Exception:
        return False


def get_project_folder(main_window) -> Optional[str]: #vers 3
    """Get project folder from settings"""
    try:
        if hasattr(main_window, 'settings') and main_window.settings:
            return main_window.settings.get('project_folder')
    except Exception:
        pass
    return None


def get_export_folder(main_window) -> Optional[str]: #vers 2
    """Get export folder from settings"""
    try:
        if hasattr(main_window, 'settings') and main_window.settings:
            return main_window.settings.get('export_folder')
    except Exception:
        pass
    return None



def validate_img_file(main_window) -> bool: #vers 4
    """Validate current IMG file"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if hasattr(main_window.current_img, 'validate'):
                return main_window.current_img.validate()
            else:
                # Basic validation - check if entries exist
                return len(main_window.current_img.entries) > 0
    except Exception:
        pass
    return False


def get_img_info(main_window) -> Dict[str, Any]: #vers 5
    """Get IMG file information"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            info = {
                'file_path': getattr(main_window.current_img, 'file_path', 'Unknown'),
                'entry_count': len(main_window.current_img.entries),
                'file_size': 0,
                'type_counts': {}
            }
            
            # Get file size
            if hasattr(main_window.current_img, 'file_path'):
                try:
                    info['file_size'] = os.path.getsize(main_window.current_img.file_path)
                except Exception:
                    pass
            
            # Count by file type
            for entry in main_window.current_img.entries:
                entry_name = getattr(entry, 'name', '')
                ext = os.path.splitext(entry_name)[1].lower()
                info['type_counts'][ext] = info['type_counts'].get(ext, 0) + 1
            
            return info
    except Exception:
        pass
    
    return {'file_path': 'None', 'entry_count': 0, 'file_size': 0, 'type_counts': {}}


def log_operation(main_window, operation: str, details: str = ""): #vers 4
    """Log an operation with timestamp"""
    try:
        import datetime
        timestamp = datetime.datetime.now().strftime("%H:%M:%S")
        message = f"[{timestamp}] {operation}"
        if details:
            message += f": {details}"
        main_window.log_message(message)
    except Exception:
        # Fallback to simple logging
        main_window.log_message(f"{operation}: {details}")


def integrate_core_functions(main_window): #vers 4
    """Integrate core utility functions into main window"""
    try:
        # Add utility functions
        main_window.get_selected_entries = lambda: get_selected_entries(main_window)
        main_window.validate_import_files = validate_import_files
        main_window.create_backup_before_import = lambda: create_backup_before_import(main_window)
        main_window.get_import_statistics = lambda: get_import_statistics(main_window)
        main_window.get_file_type_subfolder = get_file_type_subfolder
        main_window.parse_ide_file = parse_ide_file
        main_window.find_matching_entries = lambda entry_names: find_matching_entries(main_window, entry_names)
        main_window.entry_exists_in_img = lambda filename: entry_exists_in_img(main_window, filename)
        main_window.get_project_folder = lambda: get_project_folder(main_window)
        main_window.get_export_folder = lambda: get_export_folder(main_window)
        main_window.validate_img = lambda: validate_img_file(main_window)
        main_window.get_img_info = lambda: get_img_info(main_window)
        main_window.format_file_size = format_file_size
        main_window.log_operation = lambda operation, details="": log_operation(main_window, operation, details)
        
        main_window.log_message("Core utility functions integrated")
        return True
        
    except Exception as e:
        main_window.log_message(f"Failed to integrate core functions: {str(e)}")
        return False


# Export functions
__all__ = [
    'get_selected_entries',
    'validate_import_files',
    'create_backup_before_import',
    'get_import_statistics',
    'get_file_type_subfolder',
    'parse_ide_file',
    'find_matching_entries',
    'entry_exists_in_img',
    'get_project_folder',
    'get_export_folder',
    'validate_img_file',
    'get_img_info',
    'log_operation',
    'integrate_core_functions'
]
