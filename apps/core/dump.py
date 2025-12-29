#this belongs in core/dump.py - Version: 12
# X-Seti - September04 2025 - IMG Factory 1.5 - Clean Dump Functions

import os
import platform
import subprocess
from pathlib import Path
from typing import List, Optional
from PyQt6.QtWidgets import QMessageBox, QProgressDialog, QFileDialog, QApplication
from PyQt6.QtCore import Qt
from apps.methods.tab_system import get_current_file_from_active_tab

##Methods list -
# dump_all_function
# dump_selected_function
# _dump_entries
# _get_selected_entries
# _create_dump_directory
# _get_assists_folder
# _open_folder_in_explorer
# integrate_dump_functions


def dump_all_function(main_window): #vers 13
    """Dump all entries to folder"""
    try:
        # Get file from active tab
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        # Get all entries
        all_entries = getattr(file_object, 'entries', [])
        if not all_entries:
            QMessageBox.information(main_window, "No Entries", "IMG file contains no entries to dump")
            return False
        # Create dump directory
        dump_folder = _create_dump_directory(main_window, "IMG_DUMP_ALL")
        if not dump_folder:
            return False
        # Dump entries
        success = _dump_entries(file_object, all_entries, dump_folder, main_window)
        if success:
            QMessageBox.information(main_window, "Dump Complete",
                f"Successfully dumped {len(all_entries)} files to:\n{dump_folder}")
            _open_folder_in_explorer(dump_folder)
        else:
            QMessageBox.critical(main_window, "Dump Failed", "Failed to dump files")
        return success
    except Exception as e:
        QMessageBox.critical(main_window, "Dump Error", f"Dump error: {str(e)}")
        return False


def dump_selected_function(main_window): #vers 13 → FIXED: removed file_object,file_type params
    """Dump selected entries to folder"""
    try:
        # Get file from active tab
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        # Get selected entries
        selected_entries = _get_selected_entries(main_window, file_object)
        if not selected_entries:
            QMessageBox.information(main_window, "No Selection", "No entries selected for dump")
            return False
        # Create dump directory
        dump_folder = _create_dump_directory(main_window, "IMG_DUMP_SELECTED")
        if not dump_folder:
            return False
        # Dump entries
        success = _dump_entries(file_object, selected_entries, dump_folder, main_window)
        if success:
            QMessageBox.information(main_window, "Dump Complete",
                f"Successfully dumped {len(selected_entries)} files to:\n{dump_folder}")
            _open_folder_in_explorer(dump_folder)
        else:
            QMessageBox.critical(main_window, "Dump Failed", "Failed to dump selected files")
        return success
    except Exception as e:
        QMessageBox.critical(main_window, "Dump Error", f"Dump error: {str(e)}")
        return False


def _dump_entries(file_object, entries_to_dump, dump_folder, main_window) -> bool: #vers 12
    """Dump entries using file_object.read_entry_data()"""
    try:
        # Check if file object has read_entry_data method
        if not hasattr(file_object, 'read_entry_data'):
            return False
        # Create progress dialog
        progress_dialog = QProgressDialog("Dumping files...", "Cancel", 0, len(entries_to_dump), main_window)
        progress_dialog.setWindowTitle("Dumping Files")
        progress_dialog.setWindowModality(Qt.WindowModality.WindowModal)
        progress_dialog.setValue(0)
        progress_dialog.show()
        dumped_count = 0
        try:
            for i, entry in enumerate(entries_to_dump):
                # Update progress
                progress_dialog.setValue(i)
                entry_name = getattr(entry, 'name', f'entry_{i}')
                progress_dialog.setLabelText(f"Dumping: {entry_name}")
                QApplication.processEvents()
                if progress_dialog.wasCanceled():
                    break
                # Create output path
                output_path = os.path.join(dump_folder, entry_name)
                try:
                    # Use existing read_entry_data method
                    entry_data = file_object.read_entry_data(entry)
                    if entry_data and len(entry_data) > 0:
                        # Write to file
                        with open(output_path, 'wb') as f:
                            f.write(entry_data)
                        dumped_count += 1
                except Exception as e:
                    print(f"Failed to dump {entry_name}: {e}")
        finally:
            progress_dialog.close()
        return dumped_count > 0
    except Exception as e:
        print(f"Dump entries error: {e}")
        return False


def _get_selected_entries(main_window, file_object) -> List: #vers 12
    """Get selected entries from table"""
    try:
        # Get the table from GUI
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            return []
        table = main_window.gui_layout.table
        selected_rows = table.selectionModel().selectedRows()
        if not selected_rows:
            return []
        # Get entries based on selected rows
        selected_entries = []
        all_entries = getattr(file_object, 'entries', [])
        for selected_row in selected_rows:
            row_index = selected_row.row()
            if 0 <= row_index < len(all_entries):
                entry = all_entries[row_index]
                selected_entries.append(entry)
        return selected_entries
    except Exception as e:
        print(f"Error getting selected entries: {e}")
        return []


def _create_dump_directory(main_window, prefix="IMG_DUMP") -> Optional[str]: #vers 12
    """Create dump directory"""
    try:
        # Get assists folder
        assists_folder = _get_assists_folder()
        # Create unique dump folder name
        from datetime import datetime
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        # Try to get current file name for folder name
        try:
            current_index = main_window.main_tab_widget.currentIndex()
            tab_widget = main_window.main_tab_widget.widget(current_index)
            if hasattr(tab_widget, 'file_object'):
                file_path = getattr(tab_widget.file_object, 'file_path', '')
                if file_path:
                    file_name = os.path.splitext(os.path.basename(file_path))[0]
                    dump_folder_name = f"{prefix}_{file_name}_{timestamp}"
                else:
                    dump_folder_name = f"{prefix}_{timestamp}"
            else:
                dump_folder_name = f"{prefix}_{timestamp}"
        except:
            dump_folder_name = f"{prefix}_{timestamp}"
        dump_folder = os.path.join(assists_folder, "Dump", dump_folder_name)
        # Create directory
        os.makedirs(dump_folder, exist_ok=True)
        if os.path.exists(dump_folder):
            return dump_folder
        else:
            return None
    except Exception as e:
        print(f"Error creating dump directory: {e}")
        return None


def _get_assists_folder() -> str: #vers 12
    """Get assists folder path"""
    try:
        # Try to get from current working directory
        cwd = os.getcwd()
        assists_path = os.path.join(cwd, "Assists")
        if not os.path.exists(assists_path):
            os.makedirs(assists_path, exist_ok=True)
        return assists_path
    except:
        # Fallback to temp directory
        import tempfile
        return tempfile.gettempdir()


def _open_folder_in_explorer(folder_path: str): #vers 12
    """Open folder in system file explorer"""
    try:
        system = platform.system()
        if system == "Windows":
            os.startfile(folder_path)
        elif system == "Darwin":  # macOS
            subprocess.run(["open", folder_path])
        else:  # Linux and others
            subprocess.run(["xdg-open", folder_path])
    except Exception as e:
        print(f"Failed to open folder: {e}")


def integrate_dump_functions(main_window): #vers 12
    """Integrate dump functions into main window"""
    try:
        # Add dump methods to main window
        main_window.dump_all_function = lambda: dump_all_function(main_window)
        main_window.dump_selected_function = lambda: dump_selected_function(main_window)
        # Add all the aliases that GUI might use
        main_window.dump_all = main_window.dump_all_function
        main_window.dump_selected = main_window.dump_selected_function
        main_window.dump_all_entries = main_window.dump_all_function
        main_window.dump_selected_entries = main_window.dump_selected_function
        main_window.dump_entries = main_window.dump_selected_function
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Clean dump functions integrated")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Dump integration failed: {str(e)}")
        return False


# Export functions
__all__ = [
    'dump_all_function',
    'dump_selected_function',
    'integrate_dump_functions'
]
