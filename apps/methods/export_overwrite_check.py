#this belongs in methods/export_overwrite_check.py - Version: 1
# X-Seti - September09 2025 - IMG Factory 1.5 - Shared Export Overwrite Check Functions

"""
Shared Export Overwrite Check Functions
Used by: export_via.py, export.py, dump.py, and other export functions
Provides consistent overwrite checking across all export operations
"""

import os
from typing import List, Dict, Tuple, Optional, Any
from PyQt6.QtWidgets import QMessageBox

##Methods list -
# check_existing_files
# filter_existing_files  
# get_output_path_for_entry
# show_overwrite_dialog

def check_existing_files(entries: List[Any], export_folder: str, export_options: Dict = None) -> List[str]: #vers 1
    """Check which files already exist in the export destination
    
    Args:
        entries: List of entry objects to check
        export_folder: Base export folder path
        export_options: Export options dict (organize_by_type, etc.)
        
    Returns:
        List of existing filenames
    """
    try:
        existing_files = []
        export_options = export_options or {}
        
        for entry in entries:
            entry_name = getattr(entry, 'name', f'entry_{len(existing_files)}')
            output_path = get_output_path_for_entry(entry_name, export_folder, export_options)
            
            if os.path.exists(output_path):
                existing_files.append(entry_name)
        
        return existing_files
        
    except Exception as e:
        print(f"Error checking existing files: {str(e)}")
        return []

def get_output_path_for_entry(entry_name: str, export_folder: str, export_options: Dict = None) -> str: #vers 1
    """Get the output path for a specific entry based on export options
    
    Args:
        entry_name: Name of the entry/file
        export_folder: Base export folder
        export_options: Export options (organize_by_type, etc.)
        
    Returns:
        Full output path for the entry
    """
    try:
        export_options = export_options or {}
        
        if export_options.get('organize_by_type', False):
            # Organize by file extension
            ext = os.path.splitext(entry_name)[1].lower()
            type_folder = {
                '.dff': 'Models', 
                '.txd': 'Textures', 
                '.col': 'Collision',
                '.wav': 'Audio',
                '.scm': 'Scripts',
                '.dat': 'Data',
                '.ipl': 'Maps',
                '.ide': 'Definitions'
            }.get(ext, 'Other')
            
            return os.path.join(export_folder, type_folder, entry_name)
        else:
            return os.path.join(export_folder, entry_name)
            
    except Exception as e:
        # Fallback to simple path
        return os.path.join(export_folder, entry_name)

def show_overwrite_dialog(main_window, existing_files: List[str], operation_name: str = "export") -> str: #vers 1
    """Show overwrite dialog for existing files
    
    Args:
        main_window: Main window for dialog parent
        existing_files: List of existing filenames
        operation_name: Name of operation (export, dump, etc.)
        
    Returns:
        User choice: 'overwrite', 'skip', or 'cancel'
    """
    try:
        if not existing_files:
            return 'overwrite'  # No files exist, proceed normally
        
        # Create list of existing files to show
        existing_list = '\n'.join(existing_files[:10])  # Show first 10
        if len(existing_files) > 10:
            existing_list += f'\n... and {len(existing_files) - 10} more files'
        
        # Create dialog
        reply = QMessageBox.question(
            main_window,
            f"Files Already Exist - {operation_name.title()}",
            f"The following {len(existing_files)} files already exist in the {operation_name} folder:\n\n"
            f"{existing_list}\n\n"
            f"What would you like to do?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No | QMessageBox.StandardButton.Cancel,
            QMessageBox.StandardButton.No
        )
        
        # Map button responses to actions
        if reply == QMessageBox.StandardButton.Yes:
            return 'overwrite'  # Overwrite existing files
        elif reply == QMessageBox.StandardButton.No:
            return 'skip'  # Skip existing files
        else:
            return 'cancel'  # Cancel operation
            
    except Exception as e:
        print(f"Error showing overwrite dialog: {str(e)}")
        return 'cancel'

def filter_existing_files(entries: List[Any], existing_files: List[str]) -> List[Any]: #vers 1
    """Filter out entries that have existing files
    
    Args:
        entries: Original list of entries
        existing_files: List of existing filenames to filter out
        
    Returns:
        Filtered list of entries (excluding existing files)
    """
    try:
        filtered_entries = []
        
        for entry in entries:
            entry_name = getattr(entry, 'name', '')
            if entry_name not in existing_files:
                filtered_entries.append(entry)
        
        return filtered_entries
        
    except Exception as e:
        print(f"Error filtering existing files: {str(e)}")
        return entries  # Return original list on error

def handle_overwrite_check(main_window, entries: List[Any], export_folder: str, 
                          export_options: Dict = None, operation_name: str = "export") -> Tuple[List[Any], bool]: #vers 1
    """Complete overwrite check workflow - all-in-one function
    
    Args:
        main_window: Main window for dialogs
        entries: List of entries to export
        export_folder: Export destination folder
        export_options: Export options dict
        operation_name: Name of operation for dialog
        
    Returns:
        Tuple of (filtered_entries, should_continue)
        - filtered_entries: Entries to process (may be filtered)
        - should_continue: Whether to continue the operation
    """
    try:
        # Check for existing files
        existing_files = check_existing_files(entries, export_folder, export_options)
        
        if not existing_files:
            # No existing files, proceed with all entries
            return entries, True
        
        # Show overwrite dialog
        user_choice = show_overwrite_dialog(main_window, existing_files, operation_name)
        
        if user_choice == 'cancel':
            # User cancelled
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"🚫 {operation_name.title()} cancelled by user")
            return [], False
            
        elif user_choice == 'skip':
            # Skip existing files
            filtered_entries = filter_existing_files(entries, existing_files)
            
            if not filtered_entries:
                QMessageBox.information(main_window, f"No Files to {operation_name.title()}", 
                    f"All files already exist. {operation_name.title()} cancelled.")
                return [], False
            
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"📝 Skipping {len(existing_files)} existing files, "
                                      f"{operation_name}ing {len(filtered_entries)} new files")
            
            return filtered_entries, True
            
        else:  # user_choice == 'overwrite'
            # Overwrite existing files, proceed with all entries
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"🔄 Overwriting {len(existing_files)} existing files")
            
            return entries, True
            
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error in overwrite check: {str(e)}")
        # On error, ask user to decide
        reply = QMessageBox.question(main_window, "Error Checking Files", 
            f"Error checking for existing files: {str(e)}\n\nContinue anyway?")
        return entries, (reply == QMessageBox.StandardButton.Yes)

# Export functions
__all__ = [
    'check_existing_files',
    'filter_existing_files',
    'get_output_path_for_entry',
    'show_overwrite_dialog',
    'handle_overwrite_check'
]