#this belongs in core/new_import_system.py - Version: 1
# X-Seti - November22 2025 - IMG Factory 1.5 - Complete Import System Rebuild
"""
COMPLETE IMPORT SYSTEM REBUILD - Ground-up implementation
"""

import os
import struct
from typing import List, Tuple, Optional, Dict, Any
from PyQt6.QtWidgets import QMessageBox, QFileDialog

from apps.methods.tab_system import get_current_file_from_active_tab
from apps.methods.img_core_classes import IMGEntry, IMGFile
from apps.methods.rw_versions import parse_rw_version, get_rw_version_name


##Methods list -
# import_files_dialog
# import_files_list
# import_folder_contents
# import_via_ide
# import_via_text
# _add_file_to_img
# _add_multiple_files_to_img
# _detect_file_type
# _detect_rw_version_from_data
# _refresh_ui_after_import
# integrate_new_import_system


def _add_file_to_img(img_file: IMGFile, file_path: str, auto_save: bool = False) -> bool:
    """Add single file to IMG archive with proper metadata detection"""
    try:
        if not os.path.exists(file_path):
            return False
        
        # Read file data
        with open(file_path, 'rb') as f:
            file_data = f.read()
        
        # Sanitize filename
        filename = os.path.basename(file_path)
        clean_filename = _sanitize_filename(filename)
        
        # Create new entry
        entry = IMGEntry()
        entry.name = clean_filename
        entry.size = len(file_data)
        entry._cached_data = file_data
        
        # Set reference to parent IMG file
        entry.set_img_file(img_file)
        
        # Detect file type and RW version from data
        entry.extension = clean_filename.split('.')[-1].lower() if '.' in clean_filename else 'unknown'
        entry.file_type = _detect_file_type(entry.extension)
        
        # Detect RW version if it's a RenderWare file
        if entry.file_type in ['DFF', 'TXD']:
            version_val, version_name = parse_rw_version(file_data[8:12] if len(file_data) >= 12 else b'\\x00\\x00\\x00\\x00')
            if version_val > 0:
                entry.rw_version = version_val
                entry.rw_version_name = version_name
            else:
                entry.rw_version_name = "RW File"
        else:
            entry.rw_version_name = "N/A"
        
        # Mark as new entry
        entry.is_new_entry = True
        
        # Add to IMG file entries
        img_file.entries.append(entry)
        
        # Mark IMG as modified
        img_file.modified = True
        
        # Only save if auto_save is enabled
        if auto_save and hasattr(img_file, 'save_img_file'):
            return img_file.save_img_file()
        
        return True
        
    except Exception as e:
        print(f"[ERROR] Failed to add file {file_path}: {str(e)}")
        return False


def _add_multiple_files_to_img(img_file: IMGFile, file_paths: List[str], main_window=None) -> Tuple[List[str], List[str]]:
    """Add multiple files to IMG archive"""
    success_list = []
    failed_list = []
    
    for file_path in file_paths:
        if _add_file_to_img(img_file, file_path, auto_save=False):
            success_list.append(file_path)
        else:
            failed_list.append(file_path)
    
    return success_list, failed_list


def _sanitize_filename(filename: str) -> str:
    """Clean filename to prevent corruption"""
    try:
        # Remove corrupted bytes that show as garbage in table
        clean_name = filename.replace('\x00', '').replace('\xcd', '').replace('\xff', '')
        
        # Remove control characters (except null terminator)
        clean_name = ''.join(c for c in clean_name if 32 <= ord(c) <= 126)
        
        # Limit to IMG field size
        clean_name = clean_name.strip()[:24]
        
        # Fallback if empty
        if not clean_name:
            clean_name = f"file_{len(clean_name):04d}.dat"
        
        return clean_name
        
    except Exception:
        return f"file_{len(clean_name):04d}.dat"


def _detect_file_type(extension: str) -> str:
    """Detect file type from extension"""
    ext_lower = extension.lower()
    if ext_lower == 'dff':
        return 'DFF'
    elif ext_lower == 'txd':
        return 'TXD'
    elif ext_lower == 'col':
        return 'COL'
    elif ext_lower == 'ifp':
        return 'IFP'
    elif ext_lower == 'ipl':
        return 'IPL'
    elif ext_lower == 'dat':
        return 'DAT'
    elif ext_lower == 'wav':
        return 'WAV'
    else:
        return 'UNKNOWN'


def import_files_dialog(main_window) -> bool:
    """Import multiple files via dialog"""
    try:
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Active tab must contain an IMG file")
            return False

        file_paths, _ = QFileDialog.getOpenFileNames(
            main_window, "Select files to import", "",
            "All Files (*);;DFF Models (*.dff);;TXD Textures (*.txd);;COL Collision (*.col);;Audio (*.wav)"
        )
        if not file_paths:
            return False

        # Import WITHOUT auto-save
        success_list, failed_list = _add_multiple_files_to_img(file_object, file_paths, main_window)
        imported_count = len(success_list)
        if imported_count > 0:
            main_window.log_message(f"Imported {imported_count} file(s) - use Save Entry, then Reload to see changes")
            return True


def import_files_list(main_window, file_paths: List[str]) -> bool:
    """Import from provided list"""
    if not file_paths:
        return False

    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        return False

    success_list, failed_list = _add_multiple_files_to_img(file_object, file_paths, main_window)
    if success_list:
        _refresh_ui_after_import(main_window)
        return True
    return False


def import_folder_contents(main_window) -> bool:
    """Import folder contents"""
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

    success_list, failed_list = _add_multiple_files_to_img(file_object, file_paths, main_window)
    if success_list:
        _refresh_ui_after_import(main_window)
        return True
    return False


def import_via_ide(main_window, ide_path: str, files_location: str) -> bool:
    """Import files from IDE with file searching"""
    try:
        if not os.path.exists(ide_path):
            QMessageBox.warning(main_window, "IDE Not Found", f"IDE file not found: {ide_path}")
            return False
        if not os.path.exists(files_location):
            QMessageBox.warning(main_window, "Folder Not Found", f"Files location not found: {files_location}")
            return False
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if file_type != 'IMG':
            QMessageBox.warning(main_window, "IMG Only", "Import Via IDE only works with IMG files")
            return False

        # Parse IDE file
        models = set()
        textures = set()
        try:
            with open(ide_path, 'r', encoding='utf-8', errors='ignore') as f:
                current_section = None
                for line in f:
                    line = line.strip()
                    if not line or line.startswith('#'):
                        continue
                    if line.lower() == 'objs':
                        current_section = 'objs'
                        continue
                    elif line.lower() == 'tobj':
                        current_section = 'tobj'
                        continue
                    elif line.lower() == 'end':
                        current_section = None
                        continue
                    if current_section in ['objs', 'tobj']:
                        try:
                            parts = [part.strip() for part in line.split(',')]
                            if len(parts) >= 3:
                                model_name = parts[1].strip()
                                texture_name = parts[2].strip()
                                if model_name and not model_name.isdigit() and model_name != '-1':
                                    models.add(model_name)
                                if texture_name and not texture_name.isdigit() and texture_name != '-1':
                                    textures.add(texture_name)
                        except Exception:
                            continue
        except Exception as e:
            QMessageBox.critical(main_window, "IDE Parse Error", f"Failed to parse IDE file: {str(e)}")
            return False

        if not models and not textures:
            QMessageBox.information(main_window, "No Models", "No model definitions found in IDE file")
            return False

        # Find files to import
        files_to_import = []
        # Find DFF files
        for model_name in models:
            dff_path = _find_file_in_directory(files_location, f"{model_name}.dff")
            if dff_path:
                files_to_import.append(dff_path)
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"Found: {model_name}.dff")
        # Find TXD files
        for texture_name in textures:
            txd_path = _find_file_in_directory(files_location, f"{texture_name}.txd")
            if txd_path:
                files_to_import.append(txd_path)
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"Found: {texture_name}.txd")

        if not files_to_import:
            QMessageBox.information(main_window, "No Files Found", f"No files found matching IDE definitions in: {files_location}")
            return False

        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Found {len(files_to_import)} files from IDE definitions")

        # Use the import system
        return import_files_list(main_window, files_to_import)
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"IDE import error: {str(e)}")
        return False


def import_via_text(main_window, text_path: str, base_dir: str) -> bool:
    """Import files from text list"""
    try:
        if not os.path.exists(text_path):
            QMessageBox.warning(main_window, "Text File Not Found", f"Text file not found: {text_path}")
            return False
        if not os.path.exists(base_dir):
            QMessageBox.warning(main_window, "Folder Not Found", f"Files location not found: {base_dir}")
            return False
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if file_type != 'IMG':
            QMessageBox.warning(main_window, "IMG Only", "Import Via Text only works with IMG files")
            return False
        # Read text file
        files_to_import = []
        try:
            with open(text_path, 'r', encoding='utf-8', errors='ignore') as f:
                for line in f:
                    line = line.strip()
                    if line and not line.startswith('#'):
                        # Look for file in base directory
                        file_path = os.path.join(base_dir, line)
                        if os.path.exists(file_path):
                            files_to_import.append(file_path)
                            if hasattr(main_window, 'log_message'):
                                main_window.log_message(f"Found: {line}")
                        else:
                            if hasattr(main_window, 'log_message'):
                                main_window.log_message(f"Not found: {line}")
        except Exception as e:
            QMessageBox.critical(main_window, "Text Parse Error", f"Failed to parse text file: {str(e)}")
            return False
        if not files_to_import:
            QMessageBox.information(main_window, "No Files Found", "No files found from text list")
            return False
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Found {len(files_to_import)} files from text list")
        # Use the import system
        success = import_files_list(main_window, files_to_import)
        # Force a refresh to ensure metadata is populated
        if success and hasattr(main_window, 'refresh_current_tab_data'):
            main_window.refresh_current_tab_data()
        return success
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Text import error: {str(e)}")
        return False


def _find_file_in_directory(directory: str, filename: str) -> Optional[str]:
    """Find a file in a directory (case-insensitive search)"""
    filename_lower = filename.lower()
    # Search recursively
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.lower() == filename_lower:
                return os.path.join(root, file)
    return None


def _refresh_ui_after_import(main_window) -> None: #vers 2
    """Refresh UI after import"""
    try:
        if hasattr(main_window, 'refresh_current_tab_data'):
            main_window.refresh_current_tab_data()
        elif hasattr(main_window, 'refresh_table'):
            main_window.refresh_table()
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Refresh failed: {str(e)}")


def integrate_new_import_system(main_window) -> bool:
    """Integrate the new import system"""
    try:
        # Main import functions
        main_window.import_files_dialog = lambda: import_files_dialog(main_window)
        main_window.import_files_list = lambda file_paths: import_files_list(main_window, file_paths)
        main_window.import_folder_contents = lambda: import_folder_contents(main_window)
        main_window.import_via_ide = lambda ide_path, files_location: import_via_ide(main_window, ide_path, files_location)
        main_window.import_via_text = lambda text_path, base_dir: import_via_text(main_window, text_path, base_dir)
        
        # Aliases
        main_window.import_files = main_window.import_files_dialog
        main_window.import_folder = main_window.import_folder_contents
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("NEW import system integrated")
            main_window.log_message("   • Complete ground-up rebuild")
            main_window.log_message("   • Proper metadata detection")
            main_window.log_message("   • No auto-save crashes")
            main_window.log_message("   • Tab-aware operations")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Import system integration failed: {str(e)}")
        return False


# Export functions
__all__ = [
    'import_files_dialog',
    'import_files_list', 
    'import_folder_contents',
    'import_via_ide',
    'import_via_text',
    'integrate_new_import_system'
]
