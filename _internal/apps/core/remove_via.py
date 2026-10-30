#this belongs in core/remove_via.py - Version: 8
# X-Seti - November19 2025 - IMG Factory 1.5 - Remove Via Functions with Proper Modification Tracking
"""
Remove Via Functions - Remove entries via IDE/text files with proper modification tracking

"""
import os
from typing import List
from PyQt6.QtWidgets import QFileDialog, QMessageBox
from apps.methods.tab_system import get_current_file_from_active_tab
from apps.methods.imgcol_exists import set_context

##Methods list -
# _parse_ide_file_for_removal
# _parse_text_file_list
# _refresh_after_removal_via
# _remove_entries_via_with_tracking
# integrate_remove_via_functions
# remove_via_function
# remove_via_ide_function
# remove_via_text_function

def remove_via_function(main_window): #vers 2
    """Remove entries via file selection dialog - TAB AWARE"""
    set_context(main_window)
    # Get file from active tab
    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
        return False
    # File selection dialog
    file_dialog = QFileDialog()
    file_path, _ = file_dialog.getOpenFileName(
        main_window,
        "Select file for removal list",
        "",
        "IDE Files (*.ide);;Text Files (*.txt);;All Files (*)"
    )
    if not file_path:
        return False
    if file_path.lower().endswith('.ide'):
        return remove_via_ide_function_with_path(main_window, file_path)
    else:
        return remove_via_text_function_with_path(main_window, file_path)

def remove_via_ide_function(main_window): #vers 2
    """Remove entries using IDE file selection - TAB AWARE"""
    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
        return False
    file_dialog = QFileDialog()
    ide_path, _ = file_dialog.getOpenFileName(
        main_window,
        "Select IDE file for removal",
        "",
        "IDE Files (*.ide);;All Files (*)"
    )
    if not ide_path:
        return False
    return remove_via_ide_function_with_path(main_window, ide_path)

def remove_via_ide_function_with_path(main_window, ide_path: str): #vers 2
    """Remove entries using specific IDE file path with proper modification tracking - TAB AWARE"""
    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
        return False
    if not os.path.exists(ide_path):
        QMessageBox.warning(main_window, "File Not Found", f"IDE file not found: {ide_path}")
        return False
    if hasattr(main_window, 'log_message'):
        main_window.log_message(f"Removing entries via IDE: {os.path.basename(ide_path)}")
    # Parse IDE file for entry names
    entry_names_to_remove = _parse_ide_file_for_removal(ide_path)
    if not entry_names_to_remove:
        QMessageBox.information(main_window, "No Entries", "No valid entries found in IDE file for removal")
        return False
    # Find matching entries (and track missing ones)
    entries_to_remove = []
    missing_entries = []
    existing_names = {entry.name.lower() for entry in file_object.entries}
    for entry_name in entry_names_to_remove:
        if entry_name.lower() in existing_names:
            # Find actual entry object
            for entry in file_object.entries:
                if entry.name.lower() == entry_name.lower():
                    entries_to_remove.append(entry)
                    break
        else:
            missing_entries.append(entry_name)
    if not entries_to_remove and not missing_entries:
        QMessageBox.information(main_window, "No Matches", "No entries to process")
        return False
    # Build confirmation message
    confirm_msg = ""
    if entries_to_remove:
        confirm_msg += f"Found {len(entries_to_remove)} entry(ies) to remove.\n"
    if missing_entries:
        confirm_msg += f"{len(missing_entries)} entry(ies) not found in IMG:\n"
        confirm_msg += "\n".join(missing_entries[:5])
        if len(missing_entries) > 5:
            confirm_msg += f"\n... and {len(missing_entries) - 5} more"
        confirm_msg += "\n\n"
    confirm_msg += "Continue with removal?"
    reply = QMessageBox.question(
        main_window,
        "Confirm IDE Removal",
        confirm_msg,
        QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
        QMessageBox.StandardButton.No
    )
    if reply != QMessageBox.StandardButton.Yes:
        return False
    # Remove entries with proper tracking
    success = _remove_entries_via_with_tracking(file_object, entries_to_remove, main_window)
    if success or missing_entries:
        _refresh_after_removal_via(main_window)
        # Build result message
        result_msg = ""
        if success and entries_to_remove:
            result_msg += f"Successfully removed {len(entries_to_remove)} entry(ies).\n"
        if missing_entries:
            result_msg += f"{len(missing_entries)} entry(ies) were not found in the IMG file."
        QMessageBox.information(
            main_window,
            "Remove Via IDE Complete",
            result_msg + "\n\nUse 'Save Entry' to save changes to disk."
        )
        if hasattr(main_window, 'log_message'):
            log_msg = f"Remove via IDE: "
            if entries_to_remove:
                log_msg += f"{len(entries_to_remove)} removed"
            if missing_entries:
                if entries_to_remove:
                    log_msg += ", "
                log_msg += f"{len(missing_entries)} not found"
            main_window.log_message(log_msg + " - use Save Entry to save changes")
    return success or bool(missing_entries)

def remove_via_text_function(main_window): #vers 2
    """Remove entries using text file selection - TAB AWARE"""
    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
        return False
    file_dialog = QFileDialog()
    text_path, _ = file_dialog.getOpenFileName(
        main_window,
        "Select text file for removal",
        "",
        "Text Files (*.txt);;All Files (*)"
    )
    if not text_path:
        return False
    return remove_via_text_function_with_path(main_window, text_path)

def remove_via_text_function_with_path(main_window, text_path: str): #vers 2
    """Remove entries using specific text file path with proper modification tracking - TAB AWARE"""
    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
        return False
    if not os.path.exists(text_path):
        QMessageBox.warning(main_window, "File Not Found", f"Text file not found: {text_path}")
        return False
    if hasattr(main_window, 'log_message'):
        main_window.log_message(f"Removing entries via text: {os.path.basename(text_path)}")
    # Parse text file for entry names
    entry_names_to_remove = _parse_text_file_list(text_path)
    if not entry_names_to_remove:
        QMessageBox.information(main_window, "No Entries", "No valid entries found in text file for removal")
        return False
    # Find matching entries
    entries_to_remove = []
    missing_entries = []
    existing_names = {entry.name.lower() for entry in file_object.entries}
    for entry_name in entry_names_to_remove:
        if entry_name.lower() in existing_names:
            for entry in file_object.entries:
                if entry.name.lower() == entry_name.lower():
                    entries_to_remove.append(entry)
                    break
        else:
            missing_entries.append(entry_name)
    if not entries_to_remove and not missing_entries:
        QMessageBox.information(main_window, "No Matches", "No entries to process")
        return False
    # Confirmation
    confirm_msg = ""
    if entries_to_remove:
        confirm_msg += f"Found {len(entries_to_remove)} entry(ies) to remove.\n"
    if missing_entries:
        confirm_msg += f"{len(missing_entries)} entry(ies) not found in IMG:\n"
        confirm_msg += "\n".join(missing_entries[:5])
        if len(missing_entries) > 5:
            confirm_msg += f"\n... and {len(missing_entries) - 5} more"
        confirm_msg += "\n\n"
    confirm_msg += "Continue with removal?"
    reply = QMessageBox.question(
        main_window,
        "Confirm Text Removal",
        confirm_msg,
        QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
        QMessageBox.StandardButton.No
    )
    if reply != QMessageBox.StandardButton.Yes:
        return False
    # Remove
    success = _remove_entries_via_with_tracking(file_object, entries_to_remove, main_window)
    if success or missing_entries:
        _refresh_after_removal_via(main_window)
        result_msg = ""
        if success and entries_to_remove:
            result_msg += f"Successfully removed {len(entries_to_remove)} entry(ies).\n"
        if missing_entries:
            result_msg += f"{len(missing_entries)} entry(ies) were not found in the IMG file."
        QMessageBox.information(
            main_window,
            "Remove Via Text Complete",
            result_msg + "\n\nUse 'Save Entry' to save changes to disk."
        )
        if hasattr(main_window, 'log_message'):
            log_msg = f"Remove via text: "
            if entries_to_remove:
                log_msg += f"{len(entries_to_remove)} removed"
            if missing_entries:
                if entries_to_remove:
                    log_msg += ", "
                log_msg += f"{len(missing_entries)} not found"
            main_window.log_message(log_msg + " - use Save Entry to save changes")
    return success or bool(missing_entries)

def _remove_entries_via_with_tracking(file_object, entries_to_remove: List, main_window) -> bool: #vers 2
    """Remove entries via with proper modification tracking - FIXES SAVE ENTRY DETECTION"""
    if not hasattr(file_object, 'entries'):
        return False
    if not hasattr(file_object, 'deleted_entries'):
        file_object.deleted_entries = []
    removed_count = 0
    for entry in entries_to_remove:
        if entry in file_object.entries:
            file_object.entries.remove(entry)
            file_object.deleted_entries.append(entry)
            removed_count += 1
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Removed via: {entry.name}")
    if removed_count > 0:
        file_object.modified = True
    return removed_count > 0

def _parse_ide_file_for_removal(ide_path: str) -> List[str]: #vers 1
    """Parse IDE file to extract model and texture names for removal"""
    entry_names = []
    with open(ide_path, 'r', encoding='utf-8', errors='ignore') as f:
        current_section = None
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            if not line or line.startswith('#') or line.startswith(';'):
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
                parts = [part.strip() for part in line.split(',')]
                if len(parts) >= 3:
                    model_name = parts[1].strip()
                    texture_name = parts[2].strip()
                    if model_name and not model_name.isdigit() and model_name != '-1':
                        if not model_name.lower().endswith('.dff'):
                            model_name += '.dff'
                        entry_names.append(model_name)
                    if texture_name and not texture_name.isdigit() and texture_name != '-1':
                        if not texture_name.lower().endswith('.txd'):
                            texture_name += '.txd'
                        entry_names.append(texture_name)
    return list(set(entry_names))

def _parse_text_file_list(text_path: str) -> List[str]: #vers 1
    """Parse text file for list of filenames"""
    file_list = []
    with open(text_path, 'r', encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            if line and not line.startswith('#'):
                if '/' in line or '\\' in line:
                    filename = os.path.basename(line)
                else:
                    filename = line
                if filename and '.' in filename:
                    file_list.append(filename)
    return file_list

def _refresh_after_removal_via(main_window): #vers 2
    """Refresh UI after removal via"""
    if hasattr(main_window, 'refresh_current_tab_data'):
        main_window.refresh_current_tab_data()
    elif hasattr(main_window, 'refresh_table'):
        main_window.refresh_table()
    if hasattr(main_window, 'log_message'):
        main_window.log_message("UI refreshed after removal via")

def integrate_remove_via_functions(main_window) -> bool: #vers 4
    """Integrate remove via functions with proper modification tracking - TAB AWARE"""
    try:
        main_window.remove_via_function = lambda: remove_via_function(main_window)
        main_window.remove_via_ide_function = lambda: remove_via_ide_function(main_window)
        main_window.remove_via_text_function = lambda: remove_via_text_function(main_window)
        main_window.remove_via = main_window.remove_via_function
        main_window.remove_via_ide = main_window.remove_via_ide_function
        main_window.remove_via_text = main_window.remove_via_text_function
        main_window.remove_via_entries = main_window.remove_via_function
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Remove via functions integrated")
            main_window.log_message("   • Tracks deleted entries for Save Entry detection")
            main_window.log_message("   • Supports IDE and text file removal lists")
            main_window.log_message("   • Sets modified flag properly")
            main_window.log_message("   • ✅ Accurate missing file reporting")
            main_window.log_message("   • ✅ Tab-aware file detection")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Remove via integration failed: {str(e)}")
        return False

# Export functions
__all__ = [
    'remove_via_function',
    'remove_via_ide_function',
    'remove_via_text_function',
    'integrate_remove_via_functions'
]
