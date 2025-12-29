#this belongs in core/rebuild.py - Version: 8
# X-Seti - November19 2025 - IMG Factory 1.5 - Native Rebuild Functions
"""
Native IMG rebuild using imgfactory objects directly - NO conversion needed

"""
import os
import struct
from typing import Optional, Callable, Dict, Any
from PyQt6.QtWidgets import QMessageBox, QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton, QRadioButton, QButtonGroup
from apps.methods.tab_system import get_current_file_from_active_tab
from apps.methods.img_shared_operations import (
    create_progress_callback, get_img_version_info, validate_img_structure,
    create_temp_file_path, atomic_file_replace, write_img_header,
    write_img_directory, consolidate_img_data, cleanup_temp_files,
    log_operation_progress
)
from apps.methods.imgcol_exists import set_context

##Methods list -
# rebuild_current_img_native
# fast_rebuild_current
# safe_rebuild_current
# show_rebuild_mode_dialog
# _perform_native_rebuild
# _calculate_data_start_offset
# integrate_rebuild_functions

def rebuild_current_img_native(main_window, mode: str = "auto") -> bool: #vers 9
    """Native IMG rebuild using imgfactory objects directly - TAB AWARE"""
    try:
        set_context(main_window)
        # Get file from active tab
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if file_type != 'IMG' or not file_object:
            QMessageBox.critical(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        # Structure validation
        is_valid, validation_msg = validate_img_structure(file_object, main_window)
        if not is_valid:
            QMessageBox.critical(main_window, "Invalid IMG Structure", f"Cannot rebuild IMG:\n{validation_msg}")
            return False
        log_operation_progress(main_window, "REBUILD", "Starting native rebuild",
                             f"Mode: {mode}, File: {os.path.basename(file_object.file_path)}")
        # Perform the rebuild
        success = _perform_native_rebuild(file_object, mode, main_window)
        if success:
            # Refresh current tab to show changes
            if hasattr(main_window, 'refresh_current_tab_data'):
                main_window.refresh_current_tab_data()
            elif hasattr(main_window, 'refresh_table'):
                main_window.refresh_table()
            log_operation_progress(main_window, "REBUILD", "Completed successfully")
            QMessageBox.information(main_window, "Rebuild Complete", "IMG file rebuilt successfully using native algorithm!")
            return True
        else:
            log_operation_progress(main_window, "REBUILD", "Failed")
            QMessageBox.critical(main_window, "Rebuild Failed", "Failed to rebuild IMG file. Check activity log for details.")
            return False
    except Exception as e:
        log_operation_progress(main_window, "REBUILD", "Exception", str(e))
        QMessageBox.critical(main_window, "Rebuild Error", f"Error during rebuild: {str(e)}")
        return False


def fast_rebuild_current(main_window) -> bool:
    """Fast rebuild mode - optimized for speed"""
    return rebuild_current_img_native(main_window, mode="fast")


def safe_rebuild_current(main_window) -> bool:
    """Safe rebuild mode - includes extra validation"""
    return rebuild_current_img_native(main_window, mode="safe")


def show_rebuild_mode_dialog(main_window) -> bool:
    """Show rebuild mode selection dialog"""
    try:
        dialog = QDialog(main_window)
        dialog.setWindowTitle("Rebuild Options")
        dialog.setModal(True)
        dialog.resize(350, 200)
        layout = QVBoxLayout(dialog)
        # Title
        title = QLabel("Select Rebuild Mode:")
        title.setStyleSheet("font-weight: bold; font-size: 12px; margin-bottom: 10px;")
        layout.addWidget(title)
        # Mode selection
        mode_group = QButtonGroup(dialog)
        fast_radio = QRadioButton("Fast Rebuild")
        fast_radio.setToolTip("Quick rebuild optimized for speed")
        fast_radio.setChecked(True)  # Default selection
        mode_group.addButton(fast_radio, 0)
        layout.addWidget(fast_radio)
        safe_radio = QRadioButton("Safe Rebuild")
        safe_radio.setToolTip("Rebuild with extra validation and error checking")
        mode_group.addButton(safe_radio, 1)
        layout.addWidget(safe_radio)
        auto_radio = QRadioButton("Auto Mode")
        auto_radio.setToolTip("Automatically choose best rebuild method")
        mode_group.addButton(auto_radio, 2)
        layout.addWidget(auto_radio)
        # Buttons
        button_layout = QHBoxLayout()
        rebuild_btn = QPushButton("Rebuild")
        rebuild_btn.setDefault(True)
        cancel_btn = QPushButton("Cancel")
        button_layout.addWidget(rebuild_btn)
        button_layout.addWidget(cancel_btn)
        layout.addLayout(button_layout)
        # Connect buttons
        rebuild_btn.clicked.connect(dialog.accept)
        cancel_btn.clicked.connect(dialog.reject)
        # Show dialog and get result
        if dialog.exec() == QDialog.DialogCode.Accepted:
            selected_id = mode_group.checkedId()
            if selected_id == 0:
                return fast_rebuild_current(main_window)
            elif selected_id == 1:
                return safe_rebuild_current(main_window)
            else:
                return rebuild_current_img_native(main_window, mode="auto")
        return False
    except Exception as e:
        log_operation_progress(main_window, "REBUILD", "Dialog error", str(e))
        # Fallback to direct rebuild
        return rebuild_current_img_native(main_window)


def _perform_native_rebuild(img_file, mode: str, main_window) -> bool:
    """Core native rebuild implementation"""
    try:
        # Get IMG version and structure info
        version_info = get_img_version_info(img_file)
        entries = list(img_file.entries) if hasattr(img_file, 'entries') else []
        if not entries:
            log_operation_progress(main_window, "REBUILD", "No entries found")
            return False
        log_operation_progress(main_window, "REBUILD", "Preparation",
                             f"Version: {version_info['version']}, Entries: {len(entries)}")
        # Create progress callback
        progress_callback = create_progress_callback(main_window, "Rebuilding IMG")
        progress_callback(5, "Initializing rebuild process")
        # Create temporary file for atomic operation
        temp_path = create_temp_file_path(img_file.file_path, "rebuild")
        log_operation_progress(main_window, "REBUILD", "Created temp file", os.path.basename(temp_path))
        try:
            with open(temp_path, 'wb') as temp_file:
                progress_callback(10, "Writing header")
                # Phase 1: Write IMG header
                write_img_header(temp_file, version_info, len(entries))
                progress_callback(20, "Calculating data layout")
                # Phase 2: Calculate data start position
                data_start_offset = _calculate_data_start_offset(version_info, len(entries))
                progress_callback(30, "Writing directory")
                # Phase 3: Write directory entries
                write_img_directory(temp_file, entries, version_info, data_start_offset)
                progress_callback(50, "Consolidating file data")
                # Phase 4: Write consolidated file data
                consolidate_img_data(temp_file, entries, img_file, data_start_offset,
                                   lambda pct, msg: progress_callback(50 + pct//2, msg))
                progress_callback(95, "Finalizing")
            # Phase 5: Atomic replacement
            progress_callback(98, "Performing atomic replacement")
            success = atomic_file_replace(temp_path, img_file.file_path, main_window)
            if success:
                progress_callback(100, "Rebuild complete")
                log_operation_progress(main_window, "REBUILD", "Atomic replacement successful")
                return True
            else:
                log_operation_progress(main_window, "REBUILD", "Atomic replacement failed")
                return False
        except Exception as e:
            log_operation_progress(main_window, "REBUILD", "Write operation failed", str(e))
            # Clean up temp file on error
            try:
                if os.path.exists(temp_path):
                    os.remove(temp_path)
            except:
                pass
            return False
        finally:
            # Always clean up temp files
            cleanup_temp_files(img_file.file_path, main_window)
    except Exception as e:
        log_operation_progress(main_window, "REBUILD", "Core rebuild failed", str(e))
        return False


def _calculate_data_start_offset(version_info: Dict, entry_count: int) -> int:
    """Calculate where file data starts in the IMG"""
    try:
        # Header size
        offset = version_info['header_size']
        # Directory size (32 bytes per entry)
        offset += entry_count * version_info['entry_size']
        # Align to sector boundary (2048 bytes)
        SECTOR_SIZE = 2048
        return ((offset + SECTOR_SIZE - 1) // SECTOR_SIZE) * SECTOR_SIZE
    except Exception:
        # Safe fallback
        return 8 + (entry_count * 32)


def integrate_rebuild_functions(main_window) -> bool: #vers 4
    """Integrate native rebuild functions into main window - TAB AWARE"""
    try:
        # Main rebuild functions
        main_window.rebuild_current_img = lambda: rebuild_current_img_native(main_window)
        main_window.rebuild_img = main_window.rebuild_current_img  # Alias for GUI
        # Mode-specific functions
        main_window.fast_rebuild_current = lambda: fast_rebuild_current(main_window)
        main_window.safe_rebuild_current = lambda: safe_rebuild_current(main_window)
        main_window.show_rebuild_dialog = lambda: show_rebuild_mode_dialog(main_window)
        # Additional aliases for compatibility
        main_window.fast_rebuild = main_window.fast_rebuild_current
        main_window.safe_rebuild = main_window.safe_rebuild_current
        main_window.optimize_img = main_window.rebuild_current_img
        main_window.quick_rebuild = main_window.fast_rebuild_current
        log_operation_progress(main_window, "INTEGRATION", "Native rebuild system ready",
                             "Tab-aware, atomic operations, multi-version support")
        return True
    except Exception as e:
        log_operation_progress(main_window, "INTEGRATION", "Failed", str(e))
        return False


# Export functions
__all__ = [
    'rebuild_current_img_native',
    'fast_rebuild_current',
    'safe_rebuild_current',
    'show_rebuild_mode_dialog',
    'integrate_rebuild_functions'
]
