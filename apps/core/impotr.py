#this belongs in core/impotr.py - Version: 19
# X-Seti - November22 2025 - IMG Factory 1.5 - NEW IMPORT SYSTEM
"""
NEW IMPORT SYSTEM - Ground Up Rebuild
"""
import os
import re
from typing import List, Optional, Dict, Any
from PyQt6.QtWidgets import QFileDialog, QMessageBox
from apps.methods.tab_system import get_current_file_from_active_tab
from apps.methods.img_import_functions import add_multiple_files_to_img, refresh_after_import
from apps.methods.rw_versions import parse_rw_version, get_rw_version_name
from apps.methods.common_functions import sanitize_filename, detect_file_type, detect_rw_version


def import_files_function(main_window) -> bool:
    """Import multiple files via dialog - NEW SYSTEM - OPTIMIZED"""
    try:
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if file_type != 'IMG' or not file_object:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(main_window, "No IMG File", "Active tab must contain an IMG file")
            return False

        file_paths, _ = QFileDialog.getOpenFileNames(
            main_window, "Select files to import", "",
            "All Files (*);;DFF Models (*.dff);;TXD Textures (*.txd);;COL Collision (*.col);;Audio (*.wav)"
        )
        if not file_paths:
            return False

        # Track original entries for replacement detection
        original_entries = {entry.name.lower() for entry in file_object.entries if hasattr(entry, 'name')}
        
        # Import WITHOUT auto-save
        success_list, failed_list = add_multiple_files_to_img(file_object, file_paths, main_window)
        imported_count = len(success_list)

        if imported_count > 0:
            # Mark as new entries and store data for proper metadata
            imported_filenames = []
            replaced_filenames = []
            
            for entry in file_object.entries:
                if hasattr(entry, 'name'):
                    entry_name_lower = entry.name.lower()
                    
                    # Check if this is a newly imported file
                    is_new_import = False
                    for fp in success_list:
                        if sanitize_filename(os.path.basename(fp)).lower() == sanitize_filename(entry.name).lower():
                            is_new_import = True
                            break
                    
                    if is_new_import:
                        if entry_name_lower in original_entries:
                            # This is a replaced entry
                            entry.is_new_entry = True
                            entry.is_replaced = True
                            replaced_filenames.append(entry.name)
                        else:
                            # This is a truly new entry
                            entry.is_new_entry = True
                            entry.is_replaced = False
                            imported_filenames.append(entry.name)
                        
                        # Read and store data for metadata
                        if not hasattr(entry, 'data') or not entry.data:
                            for fp in success_list:
                                if sanitize_filename(os.path.basename(fp)).lower() == sanitize_filename(entry.name).lower():
                                    try:
                                        with open(fp, 'rb') as f:
                                            entry.data = f.read()
                                        # Set file type
                                        entry.file_type = detect_file_type(entry.name)
                                        # Parse RW version for table display
                                        if entry.name.lower().endswith(('.dff', '.txd')):
                                            version_val, version_name = detect_rw_version(fp)
                                            entry.rw_version = version_val
                                            entry.rw_version_name = version_name
                                        else:
                                            entry.rw_version_name = "N/A"
                                    except Exception as e:
                                        if hasattr(main_window, 'log_message'):
                                            main_window.log_message(f"Failed to read data for {entry.name}: {str(e)}")
                                        entry.rw_version_name = "Error"
                                    break

            # Track imported files for highlighting
            if hasattr(main_window, 'track_imported_files'):
                main_window.track_imported_files(imported_filenames, replaced_filenames)
            elif hasattr(main_window, '_import_highlight_manager'):
                main_window._import_highlight_manager.track_multiple_files(imported_filenames, replaced_filenames)

            # Use QTimer to defer the refresh to prevent blocking
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(0, lambda: refresh_after_import(main_window))
            
            main_window.log_message(f"Imported {imported_count} file(s) - use Save Entry to save changes")
            return True
        else:
            main_window.log_message("Import failed: No files were imported")
            return False

    except Exception as e:
        main_window.log_message(f"Import error: {str(e)}")
        return False


def import_files_with_list(main_window, file_paths: List[str]) -> bool:
    """Import from provided list - NEW SYSTEM - OPTIMIZED"""
    if not file_paths:
        return False

    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        return False

    # Track original entries for replacement detection
    original_entries = {entry.name.lower() for entry in file_object.entries if hasattr(entry, 'name')}
    
    success_list, failed_list = add_multiple_files_to_img(file_object, file_paths, main_window)
    if success_list:
        # Mark as new entries and store data
        imported_filenames = []
        replaced_filenames = []
        
        for entry in file_object.entries:
            if hasattr(entry, 'name'):
                entry_name_lower = entry.name.lower()
                
                # Check if this is a newly imported file
                is_new_import = False
                for fp in success_list:
                    if sanitize_filename(os.path.basename(fp)).lower() == sanitize_filename(entry.name).lower():
                        is_new_import = True
                        break
                
                if is_new_import:
                    if entry_name_lower in original_entries:
                        # This is a replaced entry
                        entry.is_new_entry = True
                        entry.is_replaced = True
                        replaced_filenames.append(entry.name)
                    else:
                        # This is a truly new entry
                        entry.is_new_entry = True
                        entry.is_replaced = False
                        imported_filenames.append(entry.name)
                    
                    if not hasattr(entry, 'data') or not entry.data:
                        for fp in success_list:
                            if sanitize_filename(os.path.basename(fp)).lower() == sanitize_filename(entry.name).lower():
                                try:
                                    with open(fp, 'rb') as f:
                                        entry.data = f.read()
                                    # Set file type
                                    entry.file_type = detect_file_type(entry.name)
                                    # Parse RW version
                                    if entry.name.lower().endswith(('.dff', '.txd')):
                                        version_val, version_name = detect_rw_version(fp)
                                        entry.rw_version = version_val
                                        entry.rw_version_name = version_name
                                    else:
                                        entry.rw_version_name = "N/A"
                                except Exception as e:
                                    if hasattr(main_window, 'log_message'):
                                        main_window.log_message(f"Failed to read data for {entry.name}: {str(e)}")
                                    entry.rw_version_name = "Error"
                                break
        
        # Track imported files for highlighting
        if hasattr(main_window, 'track_imported_files'):
            main_window.track_imported_files(imported_filenames, replaced_filenames)
        elif hasattr(main_window, '_import_highlight_manager'):
            main_window._import_highlight_manager.track_multiple_files(imported_filenames, replaced_filenames)
        
        # Use QTimer to defer the refresh to prevent blocking
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(0, lambda: refresh_after_import(main_window))
        
        return True
    return False


def import_folder_contents(main_window) -> bool:
    """Import folder contents - NEW SYSTEM - OPTIMIZED"""
    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        return False

    folder_path = QFileDialog.getExistingDirectory(main_window, "Select Folder to Import")
    if not folder_path:
        return False

    file_paths = []
    for root, dirs, files in os.walk(folder_path):
        for file in files:
            file_paths.append(os.path.join(root, file))

    if not file_paths:
        return False

    # Track original entries for replacement detection
    original_entries = {entry.name.lower() for entry in file_object.entries if hasattr(entry, 'name')}
    
    success_list, failed_list = add_multiple_files_to_img(file_object, file_paths, main_window)
    if success_list:
        # Mark as new entries and store data
        imported_filenames = []
        replaced_filenames = []
        
        for entry in file_object.entries:
            if hasattr(entry, 'name'):
                entry_name_lower = entry.name.lower()
                
                # Check if this is a newly imported file
                is_new_import = False
                for fp in success_list:
                    if sanitize_filename(os.path.basename(fp)).lower() == sanitize_filename(entry.name).lower():
                        is_new_import = True
                        break
                
                if is_new_import:
                    if entry_name_lower in original_entries:
                        # This is a replaced entry
                        entry.is_new_entry = True
                        entry.is_replaced = True
                        replaced_filenames.append(entry.name)
                    else:
                        # This is a truly new entry
                        entry.is_new_entry = True
                        entry.is_replaced = False
                        imported_filenames.append(entry.name)
                    
                    if not hasattr(entry, 'data') or not entry.data:
                        for fp in success_list:
                            if sanitize_filename(os.path.basename(fp)).lower() == sanitize_filename(entry.name).lower():
                                try:
                                    with open(fp, 'rb') as f:
                                        entry.data = f.read()
                                    # Set file type
                                    entry.file_type = detect_file_type(entry.name)
                                    # Parse RW version
                                    if entry.name.lower().endswith(('.dff', '.txd')):
                                        version_val, version_name = detect_rw_version(fp)
                                        entry.rw_version = version_val
                                        entry.rw_version_name = version_name
                                    else:
                                        entry.rw_version_name = "N/A"
                                except Exception as e:
                                    if hasattr(main_window, 'log_message'):
                                        main_window.log_message(f"Failed to read data for {entry.name}: {str(e)}")
                                    entry.rw_version_name = "Error"
                                break
        
        # Track imported files for highlighting
        if hasattr(main_window, 'track_imported_files'):
            main_window.track_imported_files(imported_filenames, replaced_filenames)
        elif hasattr(main_window, '_import_highlight_manager'):
            main_window._import_highlight_manager.track_multiple_files(imported_filenames, replaced_filenames)
        
        # Use QTimer to defer the refresh to prevent blocking
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(0, lambda: refresh_after_import(main_window))
        
        return True
    return False


def integrate_import_functions(main_window) -> bool:
    """Integrate NEW import functions"""
    try:
        main_window.import_files_function = lambda: import_files_function(main_window)
        main_window.import_files_with_list = lambda file_paths: import_files_with_list(main_window, file_paths)
        main_window.import_folder_contents = lambda: import_folder_contents(main_window)
        main_window.import_files = main_window.import_files_function
        main_window.import_folder = main_window.import_folder_contents

        if hasattr(main_window, 'log_message'):
            main_window.log_message("✅ NEW import system integrated")
            main_window.log_message("   • No auto-save crashes")
            main_window.log_message("   • Proper metadata detection")
            main_window.log_message("   • Filename sanitization")
            main_window.log_message("   • File type detection")
            main_window.log_message("   • RW version detection")
            main_window.log_message("   • Tab-aware only")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Import integration failed: {str(e)}")
        return False


__all__ = [
    'import_files_function',
    'import_files_with_list',
    'import_folder_contents',
    'integrate_import_functions'
]
