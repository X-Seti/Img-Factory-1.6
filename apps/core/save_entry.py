#this belongs in core/save_entry.py - Version: 14
# X-Seti - September09 2025 - IMG Factory 1.5 - Save Entry Function
"""
Save Entry Function - Fixes RW address/version detection for new entries
Complete solution for proper IMG file saving with RW version analysis
"""

import os
from typing import Optional
from PyQt6.QtWidgets import QMessageBox
from apps.methods.tab_system import get_current_file_from_active_tab, validate_tab_before_operation

##Methods list -
# _detect_rw_versions_for_new_entries
# _perform_rebuild_with_rw_analysis
# _refresh_table_with_rw_data
# integrate_save_entry_function
# save_img_entry


def _detect_rw_versions_for_new_entries(img_file, main_window): #vers 1
    """Detect RW versions for new entries before saving - CRITICAL FOR RW COLUMNS"""
    if not hasattr(img_file, 'entries') or not img_file.entries:
        return
    new_entries_count = 0
    for entry in img_file.entries:
        # Check if this is a new entry with data
        if hasattr(entry, 'is_new_entry') and entry.is_new_entry and hasattr(entry, 'data') and entry.data:
            # Detect RW version from the data
            if hasattr(entry, 'detect_rw_version'):
                entry.detect_rw_version(entry.data)
                new_entries_count += 1
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"RW detected for {entry.name}: {getattr(entry, '_rw_version_name', 'Unknown')}")
    if hasattr(main_window, 'log_message') and new_entries_count > 0:
        main_window.log_message(f"RW versions detected for {new_entries_count} new entries")



def _perform_rebuild_with_rw_analysis(img_file, main_window): #vers 4
    """Perform rebuild with RW analysis - USE STANDALONE REBUILD"""
    try:
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Starting rebuild with RW analysis...")

        # Use the working rebuild system from core/rebuild.py
        from apps.core.rebuild import rebuild_current_img_native
        return rebuild_current_img_native(main_window)
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Rebuild failed: {str(e)}")
        return False


def _refresh_table_with_rw_data(main_window): #vers 1
    """Refresh table with proper RW data - FIXES EMPTY RW COLUMNS"""
    # Force full table refresh with RW data
    refresh_methods = [
        'refresh_table',
        'update_table',
        'populate_table',
        'reload_table',
        'refresh_img_table'
    ]
    refreshed = False
    for method_name in refresh_methods:
        if hasattr(main_window, method_name):
            method = getattr(main_window, method_name)
            if callable(method):
                method()
                refreshed = True
                break
    # Try GUI layout refresh
    if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
        # Force table repopulation
        main_window.gui_layout.table.clearContents()
        # Get current file and repopulate
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if file_object and hasattr(file_object, 'entries'):
            # Use populate methods that include RW detection
            if hasattr(main_window, 'populate_img_table_enhanced'):
                main_window.populate_img_table_enhanced(file_object)
            elif hasattr(main_window, 'populate_table'):
                main_window.populate_table(file_object)
        refreshed = True
    if refreshed and hasattr(main_window, 'log_message'):
        main_window.log_message("Table refreshed with RW data")
    return refreshed


def save_img_entry(main_window): #vers 1
    """Save Entry function - FIXES RW ADDRESS/VERSION DETECTION AND DELETED ENTRIES"""
    # Validate tab
    if not validate_tab_before_operation(main_window, "Save Entry"):
        return False
    file_object, file_type = get_current_file_from_active_tab(main_window)
    if file_type != 'IMG' or not file_object:
        QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
        return False
    # COMPREHENSIVE CHANGE DETECTION
    has_changes = False
    # Check modified flag
    if hasattr(file_object, 'modified') and file_object.modified:
        has_changes = True
    # Check for deleted entries
    if hasattr(file_object, 'deleted_entries') and file_object.deleted_entries:
        has_changes = True
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Changes detected: {len(file_object.deleted_entries)} deleted entries")
    # Check for new entries
    if hasattr(file_object, 'entries') and file_object.entries:
        new_count = 0
        for entry in file_object.entries:
            if hasattr(entry, 'is_new_entry') and entry.is_new_entry:
                new_count += 1
        if new_count > 0:
            has_changes = True
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Changes detected: {new_count} new entries")
    # Check for renamed entries
    renamed_count = 0
    if hasattr(file_object, 'entries') and file_object.entries:
        for entry in file_object.entries:
            # Check if any entry has been modified since last save
            if hasattr(entry, 'is_modified') and entry.is_modified:
                has_changes = True
                renamed_count += 1
            # Check if entry was renamed by comparing to original names if available
            elif hasattr(entry, 'original_name') and entry.original_name != entry.name:
                has_changes = True
                renamed_count += 1
    if renamed_count > 0:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Changes detected: {renamed_count} renamed entries")
    # Check has_new_or_modified_entries method
    if hasattr(file_object, 'has_new_or_modified_entries') and file_object.has_new_or_modified_entries():
        has_changes = True
    if not has_changes:
        # Check if any entry has been modified in the current session
        if hasattr(file_object, 'entries') and file_object.entries:
            for entry in file_object.entries:
                if (hasattr(entry, 'is_new_entry') and entry.is_new_entry) or \
                   (hasattr(entry, 'is_replaced') and entry.is_replaced) or \
                   (hasattr(entry, 'is_modified') and entry.is_modified):
                    has_changes = True
                    break
    if not has_changes:
        QMessageBox.information(main_window, "No Changes", "No changes detected. IMG file is up to date.")
        return True
    # Confirm save
    reply = QMessageBox.question(
        main_window,
        "Save Changes",
        "This will rebuild the IMG file with RW version detection.\n"
        "Do you want to continue?",
        QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
        QMessageBox.StandardButton.Yes
    )
    if reply != QMessageBox.StandardButton.Yes:
        return False
    if hasattr(main_window, 'log_message'):
        main_window.log_message(f"Saving with RW detection: {os.path.basename(file_object.file_path)}")
    # STEP 1: Detect RW versions for new entries BEFORE saving
    _detect_rw_versions_for_new_entries(file_object, main_window) #TODO needs to be for imported files.
    # STEP 2: Perform rebuild with RW analysis
    success = _perform_rebuild_with_rw_analysis(file_object, main_window)
    if success:
        # STEP 3: Clear modification flags
        if hasattr(file_object, 'clear_modification_tracking'):
            file_object.clear_modification_tracking()
        else:
            file_object.modified = False
            # Clear deleted entries
            if hasattr(file_object, 'deleted_entries'):
                file_object.deleted_entries.clear()
        # STEP 4: Refresh table with RW data
        _refresh_table_with_rw_data(main_window)
        if hasattr(main_window, 'refresh_table'):
            main_window.refresh_table()
            QMessageBox.information(main_window, "Save Complete",
            "IMG file saved successfully with RW version detection!\n All changes have been written to disk.")
            from apps.core.reload import reload_current_file
            reload_current_file(main_window)
            main_window.log_message("Auto-reloaded after save")

        if hasattr(main_window, 'log_message'):
            main_window.log_message("Save completed with RW data populated")
        return True
    else:
        QMessageBox.critical(main_window, "Save Failed",
            "Failed to save IMG file. Please check the log for details.")
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Save failed")
        return False


def integrate_save_entry_function(main_window) -> bool: #vers 1
    """Integrate save entry function using comprehensive RW detection"""
    # Replace the broken save entry function with working version
    main_window.save_img_entry = lambda: save_img_entry(main_window)
    main_window.save_entry = lambda: save_img_entry(main_window)
    main_window.save_img_changes = lambda: save_img_entry(main_window)
    main_window.save_memory_to_disk = lambda: save_img_entry(main_window)
    # Add aliases
    main_window.save_entry_function = lambda: save_img_entry(main_window)
    main_window.save_changes = lambda: save_img_entry(main_window)
    if hasattr(main_window, 'log_message'):
        main_window.log_message("Save Entry function integrated")
        main_window.log_message("   • FIXES empty RW Address/Version columns")
        main_window.log_message("   • FIXES deleted entries detection")
        main_window.log_message("   • Detects RW versions BEFORE saving")
        main_window.log_message("   • Uses rebuild with RW analysis")
        main_window.log_message("   • Proper table refresh with RW data")
    return True


# Export functions
__all__ = [
    'save_img_entry',
    'integrate_save_entry_function'
]
