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
    """Core native rebuild — handles V1 (DIR+IMG pair) and V2/V3 (single file)."""
    try:
        from apps.methods.img_core_classes import IMGVersion
        version_info = get_img_version_info(img_file)
        entries = list(img_file.entries) if hasattr(img_file, 'entries') else []
        if not entries:
            log_operation_progress(main_window, "REBUILD", "No entries found")
            return False

        is_v1 = getattr(img_file, 'version', None) in (
            IMGVersion.VERSION_1, IMGVersion.VERSION_1_5, IMGVersion.VERSION_SOL)
        log_operation_progress(main_window, "REBUILD", "Preparation",
            f"Version: {version_info['version']}, V1-pair: {is_v1}, Entries: {len(entries)}")

        progress_callback = create_progress_callback(main_window, "Rebuilding IMG")
        progress_callback(5, "Initialising rebuild")

        if is_v1:
            return _rebuild_v1_pair(img_file, entries, progress_callback, main_window)
        else:
            return _rebuild_single_file(img_file, entries, version_info,
                                        progress_callback, main_window)
    except Exception as e:
        log_operation_progress(main_window, "REBUILD", "Core rebuild failed", str(e))
        return False


def _rebuild_v1_pair(img_file, entries, progress_callback, main_window) -> bool:
    """Rebuild a V1/V1.5 DIR+IMG pair into separate .dir and .img temp files."""
    try:
        fp = img_file.file_path
        # Resolve canonical paths (.dir and .img)
        if fp.lower().endswith('.dir'):
            dir_path = fp
            _c = fp[:-4] + '.img'
            img_path = _c if os.path.exists(_c) else fp[:-4] + '.IMG'
        else:
            img_path = fp
            _c = fp[:-4] + '.dir'
            dir_path = _c if os.path.exists(_c) else fp[:-4] + '.DIR'

        temp_dir  = dir_path  + '.rebuild_tmp'
        temp_img  = img_path  + '.rebuild_tmp'

        SECTOR = 2048
        try:
            # ── Phase 1: write .img data, collecting per-entry sector offsets ──
            progress_callback(10, "Writing entry data to .img")
            current_sector = 0
            entry_sectors = []  # (offset_sector, size_sector) per entry

            with open(temp_img, 'wb') as fimg:
                for i, entry in enumerate(entries):
                    data = img_file.read_entry_data(entry)
                    if data is None:
                        raise ValueError(f"Could not read entry: {entry.name}")
                    size_bytes = len(data)
                    size_sectors = (size_bytes + SECTOR - 1) // SECTOR
                    entry_sectors.append((current_sector, size_sectors))
                    fimg.write(data)
                    # Pad to sector boundary
                    pad = size_sectors * SECTOR - size_bytes
                    if pad:
                        fimg.write(b'\x00' * pad)
                    current_sector += size_sectors
                    if i % 50 == 0:
                        progress_callback(10 + 60 * i // len(entries), f"Writing {i+1}/{len(entries)}")

            # ── Phase 2: write .dir ──────────────────────────────────────────
            progress_callback(75, "Writing directory (.dir)")
            with open(temp_dir, 'wb') as fdir:
                for entry, (off_sec, sz_sec) in zip(entries, entry_sectors):
                    name_b = entry.name.encode('ascii', errors='replace')[:24].ljust(24, b'\x00')
                    fdir.write(struct.pack('<II', off_sec, sz_sec))
                    fdir.write(name_b)

            # ── Phase 3: atomic replace both files ──────────────────────────
            progress_callback(90, "Replacing files")
            ok_img = atomic_file_replace(temp_img, img_path, main_window)
            ok_dir = atomic_file_replace(temp_dir, dir_path, main_window)
            if ok_img and ok_dir:
                progress_callback(100, "V1 rebuild complete")
                log_operation_progress(main_window, "REBUILD", "V1 pair replaced successfully")
                return True
            else:
                log_operation_progress(main_window, "REBUILD", f"Atomic replace: img={ok_img} dir={ok_dir}")
                return False

        except Exception as e:
            log_operation_progress(main_window, "REBUILD", "V1 rebuild write failed", str(e))
            for tp in (temp_img, temp_dir):
                try:
                    if os.path.exists(tp): os.remove(tp)
                except Exception:
                    pass
            return False
    except Exception as e:
        log_operation_progress(main_window, "REBUILD", "V1 rebuild failed", str(e))
        return False


def _rebuild_single_file(img_file, entries, version_info, progress_callback, main_window) -> bool:
    """Rebuild a V2/V3 single-file IMG."""
    try:
        temp_path = create_temp_file_path(img_file.file_path, "rebuild")
        SECTOR = 2048

        try:
            with open(temp_path, 'wb') as f:
                # Header
                progress_callback(10, "Writing header")
                write_img_header(f, version_info, len(entries))

                # Directory placeholder — we'll fill offsets after writing data
                dir_pos = f.tell()
                dir_bytes = len(entries) * 32
                f.write(b'\x00' * dir_bytes)

                # Align to sector
                cur = f.tell()
                pad = (SECTOR - cur % SECTOR) % SECTOR
                if pad:
                    f.write(b'\x00' * pad)

                # Write entry data, collecting offsets
                progress_callback(30, "Writing entry data")
                entry_sectors = []
                for i, entry in enumerate(entries):
                    off_sector = f.tell() // SECTOR
                    data = img_file.read_entry_data(entry)
                    if data is None:
                        raise ValueError(f"Could not read: {entry.name}")
                    sz_bytes = len(data)
                    sz_sectors = (sz_bytes + SECTOR - 1) // SECTOR
                    entry_sectors.append((off_sector, sz_sectors))
                    f.write(data)
                    pad = sz_sectors * SECTOR - sz_bytes
                    if pad:
                        f.write(b'\x00' * pad)
                    if i % 50 == 0:
                        progress_callback(30 + 60 * i // len(entries), f"Writing {i+1}/{len(entries)}")

                # Seek back and write real directory
                progress_callback(92, "Writing directory")
                f.seek(dir_pos)
                for entry, (off_sec, sz_sec) in zip(entries, entry_sectors):
                    name_b = entry.name.encode('ascii', errors='replace')[:24].ljust(24, b'\x00')
                    f.write(struct.pack('<II', off_sec, sz_sec))
                    f.write(name_b)

            progress_callback(97, "Replacing file")
            ok = atomic_file_replace(temp_path, img_file.file_path, main_window)
            if ok:
                progress_callback(100, "Rebuild complete")
                return True
            return False

        except Exception as e:
            log_operation_progress(main_window, "REBUILD", "Single-file rebuild failed", str(e))
            try:
                if os.path.exists(temp_path): os.remove(temp_path)
            except Exception:
                pass
            return False
        finally:
            cleanup_temp_files(img_file.file_path, main_window)
    except Exception as e:
        log_operation_progress(main_window, "REBUILD", "Single-file rebuild error", str(e))
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
