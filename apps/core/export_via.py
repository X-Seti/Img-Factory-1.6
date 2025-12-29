#this belongs in core/export_via.py - Version: 10
# X-Seti - November19 2025 - IMG Factory 1.5 - Export Via Functions
"""
Export Via Functions - FIXED: No IDE dialog dependency, uses IDEParser directly
"""

import os
from typing import List, Optional
from PyQt6.QtWidgets import QMessageBox, QFileDialog, QProgressDialog, QApplication
from PyQt6.QtCore import Qt
from apps.methods.tab_system import get_current_file_from_active_tab
from apps.methods.ide_parser_functions import IDEParser
from apps.methods.img_export_entry import export_entry as export_img_entry

def export_via_function(main_window):
    """Main export via function"""
    try:
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if file_type == 'IMG':
            return _choose_ide_or_text_file(main_window)
        else:
            QMessageBox.warning(main_window, "Unsupported", "Export Via only works with IMG files")
            return False
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Export via error: {str(e)}")
        return False

def _choose_ide_or_text_file(main_window):
    """Choose IDE or text file"""
    file_path, _ = QFileDialog.getOpenFileName(
        main_window, "Select IDE or Text File", "", "IDE Files (*.ide);;Text Files (*.txt);;All Files (*)"
    )
    if not file_path:
        return False
    if file_path.lower().endswith('.ide'):
        return _export_img_via_ide(main_window, file_path)
    elif file_path.lower().endswith('.txt'):
        return _export_img_via_text(main_window, file_path)
    else:
        QMessageBox.warning(main_window, "Unsupported", "Please select .ide or .txt file")
        return False

def _export_img_via_ide(main_window, ide_path: str):
    """Export IMG entries matching IDE definitions"""
    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        QMessageBox.warning(main_window, "No IMG", "Active tab must contain an IMG file")
        return False

    # Parse IDE
    parser = IDEParser()
    if not parser.parse_ide_file(ide_path):
        QMessageBox.critical(main_window, "Parse Error", "Failed to parse IDE file")
        return False

    if not parser.models:
        QMessageBox.information(main_window, "No Models", "IDE file contains no models")
        return False

    # Build list of filenames to export
    filenames_to_export = set()
    for model in parser.models.values():
        if model.get('dff'):
            filenames_to_export.add(f"{model['dff']}")
        if model.get('txd'):
            filenames_to_export.add(f"{model['txd']}.txd")

    # Find real IMG entries that match
    matching_entries = []
    for entry in file_object.entries:
        if hasattr(entry, 'name') and entry.name in filenames_to_export:
            matching_entries.append(entry)

    if not matching_entries:
        QMessageBox.information(main_window, "No Matches", "No IMG entries match IDE definitions")
        return False

    # Choose export folder
    export_dir = QFileDialog.getExistingDirectory(main_window, "Export Folder")
    if not export_dir:
        return False

    # Export with progress
    return _export_entries_with_progress(main_window, matching_entries, export_dir)

def _export_img_via_text(main_window, text_path: str):
    """Export IMG entries listed in text file"""
    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        QMessageBox.warning(main_window, "No IMG", "Active tab must contain an IMG file")
        return False

    # Read text file
    filenames_to_export = set()
    try:
        with open(text_path, 'r', encoding='utf-8') as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith('#'):
                    filename = os.path.basename(line) if '/' in line or '\\' in line else line
                    if '.' in filename:
                        filenames_to_export.add(filename)
    except Exception as e:
        QMessageBox.critical(main_window, "Read Error", f"Failed to read text file: {str(e)}")
        return False

    if not filenames_to_export:
        QMessageBox.information(main_window, "No Files", "No valid filenames in text file")
        return False

    # Find real IMG entries
    matching_entries = []
    for entry in file_object.entries:
        if hasattr(entry, 'name') and entry.name in filenames_to_export:
            matching_entries.append(entry)

    if not matching_entries:
        QMessageBox.information(main_window, "No Matches", "No IMG entries match text list")
        return False

    export_dir = QFileDialog.getExistingDirectory(main_window, "Export Folder")
    if not export_dir:
        return False

    return _export_entries_with_progress(main_window, matching_entries, export_dir)

def _export_entries_with_progress(main_window, entries_to_export: List, export_dir: str):
    """Export entries with progress dialog"""
    try:
        progress = QProgressDialog("Exporting...", "Cancel", 0, len(entries_to_export), main_window)
        progress.setWindowModality(Qt.WindowModality.WindowModal)
        progress.show()
        QApplication.processEvents()

        exported = 0
        for i, entry in enumerate(entries_to_export):
            if progress.wasCanceled():
                break
            progress.setValue(i)
            entry_name = getattr(entry, 'name', f"entry_{i}")

            try:
                # ✅ FIXED: Use real export_entry that handles offset/size
                output_path = os.path.join(export_dir, entry_name)
                result = _export_single_entry(main_window, entry, output_path)
                if result:
                    exported += 1
            except Exception as e:
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"Export failed for {entry_name}: {str(e)}")

        progress.close()

        if exported > 0:
            QMessageBox.information(main_window, "Complete", f"Exported {exported} files to:\n{export_dir}")
            return True
        else:
            QMessageBox.warning(main_window, "Failed", "No files were exported")
            return False

    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Export error: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Export failed: {str(e)}")
        return False

def _export_single_entry(main_window, entry, output_path: str) -> bool:
    """Export single entry using IMG file object"""
    try:
        file_object, _ = get_current_file_from_active_tab(main_window)
        if not file_object or not hasattr(file_object, 'file_path'):
            return False

        # Use the fixed export_entry that reads from file using entry.offset and entry.size
        return export_img_entry(file_object, entry, output_path=output_path) is not None
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Single entry export error: {str(e)}")
        return False

def integrate_export_via_functions(main_window) -> bool:
    """Integrate export via functions"""
    try:
        main_window.export_via_function = lambda: export_via_function(main_window)
        main_window.export_via = main_window.export_via_function
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Export via functions integrated - FIXED")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Export via integration failed: {str(e)}")
        return False

__all__ = ['export_via_function', 'integrate_export_via_functions']
