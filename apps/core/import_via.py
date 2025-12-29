#this belongs in core/import_via.py - Version: 20
# X-Seti - November22 2025 - IMG Factory 1.5 - NEW Import Via Functions - Ground Up Rebuild

"""
NEW Import Via Functions - Ground Up Rebuild with enhanced functionality
"""

import os
import re
from typing import List, Optional, Dict, Any, Tuple
from PyQt6.QtWidgets import QMessageBox, QFileDialog, QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QLineEdit, QLabel, QRadioButton, QButtonGroup
from apps.methods.imgcol_exists import set_context
from apps.methods.tab_system import get_current_file_from_active_tab
from apps.methods.common_functions import sanitize_filename, detect_file_type, detect_rw_version

##Methods list -
# import_via_function
# import_via_ide_function
# import_via_text_function
# _import_files_via_ide
# _import_files_via_text
# _create_import_via_dialog
# _find_files_in_directory
# integrate_import_via_functions

def import_via_function(main_window): #vers 6
    """Main import via function with dialog - NEW SYSTEM"""
    set_context(main_window)
    # File selection dialog - import via should work with both img and col files.
    try:
        # import via dialog - import via should work with both img and col files.
        dialog_result = _create_import_via_dialog(main_window)
        if not dialog_result:
            return False
        import_type, file_path, files_location = dialog_result
        if import_type == 'ide':
            return _import_files_via_ide(main_window, file_path, files_location)
        elif import_type == 'text':
            return _import_files_via_text(main_window, file_path, files_location)
        return False
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Import Via error: {str(e)}")
        return False


def _import_files_via_ide(main_window, ide_path: str, files_location: str) -> bool: #vers 4
    """Import files from IDE with file searching - NEW SYSTEM"""
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
                                    models.add(sanitize_filename(model_name))
                                if texture_name and not texture_name.isdigit() and texture_name != '-1':
                                    textures.add(sanitize_filename(texture_name))
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
            dff_path = _find_files_in_directory(files_location, f"{model_name}.dff")
            if dff_path:
                files_to_import.append(dff_path)
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"Found: {model_name}.dff")
        # Find TXD files
        for texture_name in textures:
            txd_path = _find_files_in_directory(files_location, f"{texture_name}.txd")
            if txd_path:
                files_to_import.append(txd_path)
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"Found: {texture_name}.txd")

        if not files_to_import:
            QMessageBox.information(main_window, "No Files Found", f"No files found matching IDE definitions in: {files_location}")
            return False

        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Found {len(files_to_import)} files from IDE definitions")

        # Use the NEW import system
        if hasattr(main_window, 'import_files_with_list'):
            success = main_window.import_files_with_list(files_to_import)
            # Force a refresh to ensure metadata is populated
            if success and hasattr(main_window, 'refresh_current_tab_data'):
                main_window.refresh_current_tab_data()
            return success
        else:
            return False
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"IDE import error: {str(e)}")
        return False


def import_via_ide_function(main_window) -> bool: #vers 4
    """Direct IDE import function - NEW SYSTEM"""
    try:
        # Get IDE file
        ide_path, _ = QFileDialog.getOpenFileName(
            main_window,
            "Select IDE File", "", "IDE Files (*.ide);;All Files (*)"
        )
        if not ide_path:
            return False
        # Get files location
        files_location = QFileDialog.getExistingDirectory(
            main_window,
            "Select Folder Containing Files to Import",
            "",
            QFileDialog.Option.ShowDirsOnly
        )
        if not files_location:
            return False
        return _import_files_via_ide(main_window, ide_path, files_location)
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Import Via IDE error: {str(e)}")
        return False

def import_via_text_function(main_window) -> bool: #vers 4
    """Import files from text file list - NEW SYSTEM"""
    try:
        # File dialog for text file
        text_path, _ = QFileDialog.getOpenFileName(
            main_window,
            "Select Text File List", "", "Text Files (*.txt);;All Files (*)"
        )
        if not text_path:
            return False
        # Get files location
        files_location = QFileDialog.getExistingDirectory(
            main_window,
            "Select Folder Containing Files to Import",
            "",
            QFileDialog.Option.ShowDirsOnly
        )
        if not files_location:
            return False
        return _import_files_via_text(main_window, text_path, files_location)
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Import via text error: {str(e)}")
        return False


def _import_files_via_text(main_window, text_path: str, base_dir: str) -> bool: #vers 4
    """Import files from text list - NEW SYSTEM"""
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
                        # Look for file in base directory, sanitize the filename
                        sanitized_line = sanitize_filename(line)
                        file_path = os.path.join(base_dir, sanitized_line)
                        if os.path.exists(file_path):
                            files_to_import.append(file_path)
                            if hasattr(main_window, 'log_message'):
                                main_window.log_message(f"Found: {line}")
                        else:
                            # Try to find the file with different case or extensions
                            found_path = _find_files_in_directory(base_dir, sanitized_line)
                            if found_path:
                                files_to_import.append(found_path)
                                if hasattr(main_window, 'log_message'):
                                    main_window.log_message(f"Found (case-insensitive): {line}")
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
        # Use the NEW import system
        if hasattr(main_window, 'import_files_with_list'):
            success = main_window.import_files_with_list(files_to_import)
            # Force a refresh to ensure metadata is populated
            if success and hasattr(main_window, 'refresh_current_tab_data'):
                main_window.refresh_current_tab_data()
            return success
        else:
            return False
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Text import error: {str(e)}")
        return False

def _find_files_in_directory(directory: str, filename: str) -> Optional[str]: #vers 4
    """Find a file in a directory (case-insensitive search) - NEW SYSTEM"""
    filename_lower = sanitize_filename(filename).lower()
    # Search recursively
    for root, dirs, files in os.walk(directory):
        for file in files:
            if sanitize_filename(file).lower() == filename_lower:
                return os.path.join(root, file)
    return None

def _create_import_via_dialog(main_window): #vers 4
    """Create import via dialog - NEW SYSTEM"""
    try:
        dialog = QDialog(main_window)
        dialog.setWindowTitle("Import Via - NEW SYSTEM")
        dialog.setModal(True)
        dialog.resize(500, 300)
        layout = QVBoxLayout()
        dialog.setLayout(layout)

        # Import type selection
        type_group = QButtonGroup(dialog)
        dialog.ide_radio = QRadioButton("Import from IDE File")
        dialog.ide_radio.setChecked(True)
        type_group.addButton(dialog.ide_radio)
        layout.addWidget(dialog.ide_radio)
        dialog.text_radio = QRadioButton("Import from Text File List")
        type_group.addButton(dialog.text_radio)
        layout.addWidget(dialog.text_radio)

        # File path
        file_layout = QHBoxLayout()
        file_layout.addWidget(QLabel("File:"))
        dialog.ide_path_input = QLineEdit()
        file_layout.addWidget(dialog.ide_path_input)
        dialog.browse_file_btn = QPushButton("Browse")
        dialog.browse_file_btn.clicked.connect(lambda: _browse_file(dialog))
        file_layout.addWidget(dialog.browse_file_btn)
        layout.addLayout(file_layout)

        # Files location
        location_layout = QHBoxLayout()
        location_layout.addWidget(QLabel("Files Location:"))
        dialog.files_location_input = QLineEdit()
        location_layout.addWidget(dialog.files_location_input)
        dialog.browse_location_btn = QPushButton("Browse")
        dialog.browse_location_btn.clicked.connect(lambda: _browse_location(dialog))
        location_layout.addWidget(dialog.browse_location_btn)
        layout.addLayout(location_layout)

        # Buttons
        button_layout = QHBoxLayout()
        dialog.import_btn = QPushButton("Import")
        dialog.import_btn.setEnabled(False)
        dialog.import_btn.clicked.connect(dialog.accept)
        button_layout.addWidget(dialog.import_btn)
        dialog.cancel_btn = QPushButton("Cancel")
        dialog.cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(dialog.cancel_btn)
        layout.addLayout(button_layout)

        # Connect signals
        dialog.ide_path_input.textChanged.connect(lambda: _update_button_state(dialog))
        dialog.files_location_input.textChanged.connect(lambda: _update_button_state(dialog))

        # Show dialog
        if dialog.exec() == QDialog.DialogCode.Accepted:
            import_type = 'ide' if dialog.ide_radio.isChecked() else 'text'
            ide_path = dialog.ide_path_input.text().strip()
            files_location = dialog.files_location_input.text().strip()
            return (import_type, ide_path, files_location)
        return None
    except Exception as e:
        QMessageBox.critical(main_window, "Dialog Error", f"Failed to create import dialog: {str(e)}")
        return None

def _browse_file(dialog): #vers 2
    """Browse for file"""
    try:
        file_path, _ = QFileDialog.getOpenFileName(
            dialog,
            "Select File", "", "IDE Files (*.ide);;Text Files (*.txt);;All Files (*)")
        if file_path:
            dialog.ide_path_input.setText(file_path)
    except Exception:
        pass

def _browse_location(dialog): #vers 2
    """Browse for files location folder"""
    try:
        folder = QFileDialog.getExistingDirectory(
            dialog,
            "Select Folder Containing Files to Import",
            "",
            QFileDialog.Option.ShowDirsOnly
        )
        if folder:
            dialog.files_location_input.setText(folder)
    except Exception:
        pass

def _update_button_state(dialog): #vers 2
    """Update import button enabled state"""
    try:
        ide_path = dialog.ide_path_input.text().strip()
        files_location = dialog.files_location_input.text().strip()
        # Enable import button only if both paths are provided
        dialog.import_btn.setEnabled(bool(ide_path and files_location))
    except Exception:
        dialog.import_btn.setEnabled(False)

def integrate_import_via_functions(main_window): #vers 2
    """Integrate import via functions into main window"""
    global file_object, file_type
    file_object = getattr(main_window, 'file_object', None)
    file_type = getattr(main_window, 'file_type', None)
    try:
        # Add main import via functions
        main_window.import_via_function = lambda: import_via_function(main_window)
        main_window.import_via_ide_function = lambda: import_via_ide_function(main_window)
        main_window.import_via_text_function = lambda: import_via_text_function(main_window)
        # Add aliases that GUI might use
        main_window.import_via = main_window.import_via_function
        main_window.import_via_ide = main_window.import_via_ide_function
        main_window.import_via_text = main_window.import_via_text_function
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Import Via functions integrated")
            main_window.log_message("   • IDE file import with model/texture detection")
            main_window.log_message("   • Text file list import")
            main_window.log_message("   • Recursive file searching")
            main_window.log_message("   • FIXED: Metadata populated after import")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Import Via integration failed: {str(e)}")
        return False

# Export functions
__all__ = [
    'import_via_function',
    'import_via_ide_function',
    'import_via_text_function',
    'integrate_import_via_functions'
]
