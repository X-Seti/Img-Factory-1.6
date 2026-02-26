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
        import_type, file_path, files_location, show_stats = dialog_result
        if import_type == 'ide':
            return _import_files_via_ide(main_window, file_path, files_location, show_stats)
        elif import_type == 'text':
            return _import_files_via_text(main_window, file_path, files_location)
        return False
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Import Via error: {str(e)}")
        return False


def _parse_ide_sections(ide_path: str) -> dict: #vers 1
    """Parse IDE file and return per-section model/texture sets.
    Returns dict: { section_name: {'models': set, 'textures': set} }
    """
    MODEL_SECTIONS = {'objs', 'tobj', 'anim', 'weap', 'cars', 'peds', 'hier', 'path', '2dfx', 'txdp'}
    sections = {}
    current_section = None
    with open(ide_path, 'r', encoding='utf-8', errors='ignore') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#') or line.startswith('//'):
                continue
            low = line.lower()
            if low in MODEL_SECTIONS:
                current_section = low
                if current_section not in sections:
                    sections[current_section] = {'models': set(), 'textures': set()}
                continue
            elif low == 'end':
                current_section = None
                continue
            if current_section in MODEL_SECTIONS:
                try:
                    parts = [p.strip() for p in line.split(',')]
                    if len(parts) >= 2:
                        m = parts[1].strip()
                        if m and not m.isdigit() and m not in ('-1', ''):
                            sections[current_section]['models'].add(sanitize_filename(m))
                    if len(parts) >= 3:
                        t = parts[2].strip()
                        if t and not t.isdigit() and t not in ('-1', ''):
                            sections[current_section]['textures'].add(sanitize_filename(t))
                except Exception:
                    continue
    return sections


def _make_progress_dialog(parent, title: str, message: str):
    """Create a simple non-cancellable progress dialog for IDE operations."""
    from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QProgressBar
    from PyQt6.QtCore import Qt
    dlg = QDialog(parent)
    dlg.setWindowTitle(title)
    dlg.setModal(True)
    dlg.setWindowFlags(dlg.windowFlags() & ~Qt.WindowType.WindowCloseButtonHint)
    dlg.resize(360, 90)
    layout = QVBoxLayout(dlg)
    dlg.label = QLabel(message)
    dlg.label.setAlignment(Qt.AlignmentFlag.AlignCenter)
    layout.addWidget(dlg.label)
    dlg.bar = QProgressBar()
    dlg.bar.setRange(0, 0)  # indeterminate
    layout.addWidget(dlg.bar)
    return dlg


def _import_files_via_ide(main_window, ide_path: str, files_location: str, show_stats: bool = False) -> bool: #vers 6 Fixed
    """Import files from IDE with progress dialog and optional post-import stats."""
    from PyQt6.QtWidgets import QApplication

    progress = _make_progress_dialog(main_window, "Import Via IDE", "Parsing IDE file...")
    progress.show()
    QApplication.processEvents()

    try:
        if not os.path.exists(ide_path):
            progress.close()
            QMessageBox.warning(main_window, "IDE Not Found", f"IDE file not found: {ide_path}")
            return False
        if not os.path.exists(files_location):
            progress.close()
            QMessageBox.warning(main_window, "Folder Not Found", f"Files location not found: {files_location}")
            return False
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if file_type != 'IMG':
            progress.close()
            QMessageBox.warning(main_window, "IMG Only", "Import Via IDE only works with IMG files")
            return False

        # Stage 1: Parse IDE
        progress.label.setText("Parsing IDE file...")
        QApplication.processEvents()
        try:
            sections = _parse_ide_sections(ide_path)
        except Exception as e:
            progress.close()
            QMessageBox.critical(main_window, "IDE Parse Error", f"Failed to parse IDE file: {str(e)}")
            return False

        if not sections:
            progress.close()
            QMessageBox.information(main_window, "No Models", "No model definitions found in IDE file")
            return False

        all_models = set()
        all_textures = set()
        for sec_data in sections.values():
            all_models.update(sec_data['models'])
            all_textures.update(sec_data['textures'])

        total = len(all_models) + len(all_textures)

        # Stage 2: Search files
        progress.label.setText(f"Searching folder for {total} files...")
        progress.bar.setRange(0, total)
        progress.bar.setValue(0)
        QApplication.processEvents()

        files_to_import = []
        found_dff, missing_dff = [], []
        found_txd, missing_txd = [], []
        done = 0

        for name in sorted(all_models):
            path = _find_files_in_directory(files_location, f"{name}.dff")
            if path:
                files_to_import.append(path)
                found_dff.append(name)
            else:
                missing_dff.append(name)
            done += 1
            progress.bar.setValue(done)
            if done % 10 == 0:
                QApplication.processEvents()

        for name in sorted(all_textures):
            path = _find_files_in_directory(files_location, f"{name}.txd")
            if path:
                files_to_import.append(path)
                found_txd.append(name)
            else:
                missing_txd.append(name)
            done += 1
            progress.bar.setValue(done)
            if done % 10 == 0:
                QApplication.processEvents()

        if not files_to_import:
            progress.close()
            QMessageBox.information(main_window, "No Files Found",
                f"No files found matching IDE definitions in: {files_location}\n"
                f"Missing DFF: {len(missing_dff)}  Missing TXD: {len(missing_txd)}")
            return False

        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Found {len(files_to_import)} files "
                f"(DFF: {len(found_dff)}, TXD: {len(found_txd)}, "
                f"missing DFF: {len(missing_dff)}, missing TXD: {len(missing_txd)})")

        # Stage 3: Import
        progress.label.setText(f"Importing {len(files_to_import)} files...")
        progress.bar.setRange(0, 0)
        QApplication.processEvents()

        success = False
        if hasattr(main_window, 'import_files_with_list'):
            success = main_window.import_files_with_list(files_to_import)
            if success and hasattr(main_window, 'refresh_current_tab_data'):
                main_window.refresh_current_tab_data()

        progress.close()

        if show_stats:
            _show_import_stats_dialog(main_window, sections, found_dff, found_txd,
                                      missing_dff, missing_txd, success)
        return success

    except Exception as e:
        progress.close()
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"IDE import error: {str(e)}")
        return False


def _show_import_stats_dialog(main_window, sections: dict, found_dff: list, found_txd: list,
                               missing_dff: list, missing_txd: list, import_success: bool): #vers 1
    """Show post-import statistics dialog."""
    from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QLabel,
                                  QPushButton, QTextEdit, QTabWidget, QWidget, QFrame)
    from PyQt6.QtCore import Qt
    from PyQt6.QtGui import QFont

    dialog = QDialog(main_window)
    dialog.setWindowTitle("Import Stats")
    dialog.setModal(True)
    dialog.resize(600, 500)
    layout = QVBoxLayout(dialog)

    # Status header
    status = "Import completed" if import_success else "Import failed"
    hdr = QLabel(f"<b>{status}</b>")
    hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
    layout.addWidget(hdr)

    # Summary bar
    total_ide = sum(len(s['models']) for s in sections.values())
    total_txd_ide = sum(len(s['textures']) for s in sections.values())
    summary = QLabel(
        f"IDE entries:  DFF {total_ide}  |  TXD {total_txd_ide}    "
        f"Found:  DFF {len(found_dff)}  TXD {len(found_txd)}    "
        f"Missing:  DFF {len(missing_dff)}  TXD {len(missing_txd)}"
    )
    summary.setAlignment(Qt.AlignmentFlag.AlignCenter)
    layout.addWidget(summary)

    sep = QFrame()
    sep.setFrameShape(QFrame.Shape.HLine)
    layout.addWidget(sep)

    # Tabs
    tabs = QTabWidget()
    layout.addWidget(tabs)

    # Tab 1 - Per-section counts
    sec_widget = QWidget()
    sec_layout = QVBoxLayout(sec_widget)
    sec_text = QTextEdit()
    sec_text.setReadOnly(True)
    sec_text.setFont(QFont("Monospace", 9))
    lines = [f"{'Section':<12} {'DFF in IDE':>10} {'TXD in IDE':>10}"]
    lines.append("-" * 36)
    for sec_name, sec_data in sorted(sections.items()):
        lines.append(f"{sec_name:<12} {len(sec_data['models']):>10} {len(sec_data['textures']):>10}")
    lines.append("-" * 36)
    lines.append(f"{'TOTAL':<12} {total_ide:>10} {total_txd_ide:>10}")
    sec_text.setPlainText("\n".join(lines))
    sec_layout.addWidget(sec_text)
    tabs.addTab(sec_widget, f"Sections ({len(sections)})")

    # Tab 2 - Missing files
    miss_widget = QWidget()
    miss_layout = QVBoxLayout(miss_widget)
    miss_text = QTextEdit()
    miss_text.setReadOnly(True)
    miss_text.setFont(QFont("Monospace", 9))
    miss_lines = []
    if missing_dff:
        miss_lines.append(f"--- Missing DFF ({len(missing_dff)}) ---")
        miss_lines.extend(f"  {n}.dff" for n in sorted(missing_dff))
    if missing_txd:
        if miss_lines:
            miss_lines.append("")
        miss_lines.append(f"--- Missing TXD ({len(missing_txd)}) ---")
        miss_lines.extend(f"  {n}.txd" for n in sorted(missing_txd))
    if not miss_lines:
        miss_lines = ["All files found - nothing missing."]
    miss_text.setPlainText("\n".join(miss_lines))
    miss_layout.addWidget(miss_text)
    total_missing = len(missing_dff) + len(missing_txd)
    tabs.addTab(miss_widget, f"Missing ({total_missing})")

    # Tab 3 - Imported files
    imp_widget = QWidget()
    imp_layout = QVBoxLayout(imp_widget)
    imp_text = QTextEdit()
    imp_text.setReadOnly(True)
    imp_text.setFont(QFont("Monospace", 9))
    imp_lines = []
    if found_dff:
        imp_lines.append(f"--- Imported DFF ({len(found_dff)}) ---")
        imp_lines.extend(f"  {n}.dff" for n in sorted(found_dff))
    if found_txd:
        if imp_lines:
            imp_lines.append("")
        imp_lines.append(f"--- Imported TXD ({len(found_txd)}) ---")
        imp_lines.extend(f"  {n}.txd" for n in sorted(found_txd))
    imp_text.setPlainText("\n".join(imp_lines))
    imp_layout.addWidget(imp_text)
    total_imported = len(found_dff) + len(found_txd)
    tabs.addTab(imp_widget, f"Imported ({total_imported})")

    # Close button
    btn_layout = QHBoxLayout()
    close_btn = QPushButton("Close")
    close_btn.clicked.connect(dialog.accept)
    btn_layout.addStretch()
    btn_layout.addWidget(close_btn)
    layout.addLayout(btn_layout)

    dialog.exec()


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

def _create_import_via_dialog(main_window): #vers 5 Fixed
    """Create import via dialog with Show Import Stats checkbox."""
    from PyQt6.QtWidgets import QCheckBox
    try:
        dialog = QDialog(main_window)
        dialog.setWindowTitle("Import Via")
        dialog.setModal(True)
        dialog.resize(520, 220)
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

        # IDE / text file path
        file_layout = QHBoxLayout()
        file_layout.addWidget(QLabel("IDE File:"))
        dialog.ide_path_input = QLineEdit()
        file_layout.addWidget(dialog.ide_path_input)
        dialog.browse_file_btn = QPushButton("Browse")
        dialog.browse_file_btn.clicked.connect(lambda: _browse_file(dialog))
        file_layout.addWidget(dialog.browse_file_btn)
        layout.addLayout(file_layout)

        # Files location
        location_layout = QHBoxLayout()
        location_layout.addWidget(QLabel("Folder:"))
        dialog.files_location_input = QLineEdit()
        location_layout.addWidget(dialog.files_location_input)
        dialog.browse_location_btn = QPushButton("Browse")
        dialog.browse_location_btn.clicked.connect(lambda: _browse_location(dialog))
        location_layout.addWidget(dialog.browse_location_btn)
        layout.addLayout(location_layout)

        # Show import stats checkbox - only visible for IDE import
        dialog.stats_checkbox = QCheckBox("Show import stats")
        dialog.stats_checkbox.setChecked(True)
        layout.addWidget(dialog.stats_checkbox)

        def _toggle_stats_visibility():
            dialog.stats_checkbox.setVisible(dialog.ide_radio.isChecked())
        dialog.ide_radio.toggled.connect(_toggle_stats_visibility)

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

        if dialog.exec() == QDialog.DialogCode.Accepted:
            import_type = 'ide' if dialog.ide_radio.isChecked() else 'text'
            ide_path = dialog.ide_path_input.text().strip()
            files_location = dialog.files_location_input.text().strip()
            show_stats = dialog.stats_checkbox.isChecked() and import_type == 'ide'
            return (import_type, ide_path, files_location, show_stats)
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
