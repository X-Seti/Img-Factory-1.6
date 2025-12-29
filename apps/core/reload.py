#this belongs in core/reload.py - Version: 10
# X-Seti - November16 2025 - IMG Factory 1.5 - Reload Functions - TAB AWARE
"""
Reload Functions - TAB-AWARE VERSION
Gets file from active tab, not main_window.current_img
"""

import os
from PyQt6.QtWidgets import QMessageBox
from apps.methods.tab_system import get_current_file_from_active_tab

##Methods list -
# reload_current_file
# integrate_reload_functions

def reload_current_file(main_window) -> bool: #vers 11
    """Reload file in current tab - TAB AWARE VERSION - FIXED to properly close and reopen from disk"""
    try:
        # Get file from active tab
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if not file_object:
            QMessageBox.warning(main_window, "No File", "No file is currently loaded in this tab")
            return False
        # Get file path
        if not hasattr(file_object, 'file_path'):
            QMessageBox.warning(main_window, "No File Path", "Current file has no file path")
            return False
        file_path = file_object.file_path
        if not os.path.exists(file_path):
            QMessageBox.warning(main_window, "File Not Found", f"File not found: {file_path}")
            return False
        filename = os.path.basename(file_path)
        # Ask for confirmation
        reply = QMessageBox.question(
            main_window,
            "Reload File",
            f"Reload {filename}?\nThis will discard any unsaved changes.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )
        if reply != QMessageBox.StandardButton.Yes:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("Reload cancelled by user")
            return False
        
        # Store file path before closing tab
        stored_file_path = file_path
        stored_file_type = file_type
        
        # Close the current tab
        current_tab_index = main_window.main_tab_widget.currentIndex() if hasattr(main_window, "main_tab_widget") else 0
        if hasattr(main_window, "log_message"):
            main_window.log_message(f"Closing tab {current_tab_index} before reload: {filename}")
        
        # Use close_manager to close the tab
        if hasattr(main_window, "close_manager") and main_window.close_manager:
            main_window.close_manager.close_tab(current_tab_index)
        elif hasattr(main_window, "close_tab"):
            main_window.close_tab(current_tab_index)
        
        # Now reload in NEW tab based on type
        if stored_file_type == "IMG":
            if hasattr(main_window, "load_file_unified"):
                result = main_window.load_file_unified(stored_file_path)
            elif hasattr(main_window, "_load_img_file_in_new_tab"):
                result = main_window._load_img_file_in_new_tab(stored_file_path)
            else:
                QMessageBox.critical(main_window, "Error", "No IMG load function available")
                return False
        elif stored_file_type == "COL":
            if hasattr(main_window, "load_file_unified"):
                result = main_window.load_file_unified(stored_file_path)
            else:
                QMessageBox.critical(main_window, "Error", "No COL load function available")
                return False
        else:
            QMessageBox.warning(main_window, "Unsupported Type", f"Cannot reload {stored_file_type} files")
            return False
        
        if result and hasattr(main_window, "log_message"):
            main_window.log_message(f"File reloaded in new tab: {filename}")
        return True if result else False
        
        # CLOSE TAB FIRST, then reload (opens in new tab)
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Closing tab before reload: {filename}")
        
        # Close the current tab
        current_tab_index = main_window.main_tab_widget.currentIndex() if hasattr(main_window, 'main_tab_widget') else 0
        if hasattr(main_window, 'close_manager') and main_window.close_manager:
            main_window.close_manager.close_tab(current_tab_index)
        elif hasattr(main_window, 'close_tab'):
            main_window.close_tab(current_tab_index)
        
        # Now reload based on type - will open in new tab
        if file_type == 'IMG':
            # Close the IMG file properly if it has a close method
            if file_object and hasattr(file_object, 'close'):
                file_object.close()
            # Clear current reference
            main_window.current_img = None
        elif file_type == 'COL':
            # For COL files, we should also close if possible
            if file_object and hasattr(file_object, 'close'):
                file_object.close()
            main_window.current_col = None
            
        # Also clear tab reference if using tabs
        if hasattr(main_window, 'main_tab_widget'):
            current_tab = main_window.main_tab_widget.widget(current_tab_index)
            if current_tab and hasattr(current_tab, 'file_object'):
                # Close the file in the tab as well
                if hasattr(current_tab.file_object, 'close'):
                    current_tab.file_object.close()
                current_tab.file_object = None
                current_tab.file_type = None
        
        # Now reload based on type - this will read fresh from disk
        if file_type == 'IMG':
            result = _reload_img_in_tab(main_window, file_path)
        elif file_type == 'COL':
            result = _reload_col_in_tab(main_window, file_path)
        else:
            QMessageBox.warning(main_window, "Unsupported Type", f"Cannot reload {file_type} files")
            return False
            
        if result:
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"File successfully reloaded from disk: {filename}")
        return result
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Reload error: {str(e)}")
        QMessageBox.critical(main_window, "Reload Error", f"Failed to reload file:\n{str(e)}")
        return False


def _reload_img_in_tab(main_window, file_path: str) -> bool: #vers 2
    """Reload IMG file in current tab - FIXED to properly close and reopen"""
    try:
        filename = os.path.basename(file_path)
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Reloading IMG: {filename}")
        
        # Store current tab info to restore after reload
        current_index = main_window.main_tab_widget.currentIndex() if hasattr(main_window, 'main_tab_widget') else 0
        tab_widget = main_window.main_tab_widget.widget(current_index) if hasattr(main_window, 'main_tab_widget') else None
        
        # Load new IMG instance
        from apps.methods.img_core_classes import IMGFile
        new_img = IMGFile(file_path)
        if not new_img.open():
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Failed to reload IMG: {filename}")
            return False
        
        # Update current tab's file object
        if tab_widget:
            tab_widget.file_object = new_img
            tab_widget.file_type = 'IMG'
        
        # Update main_window reference (for compatibility)
        main_window.current_img = new_img
        
        # Refresh UI - make sure to fully refresh the table
        if hasattr(main_window, 'refresh_current_tab_data'):
            main_window.refresh_current_tab_data()
        elif hasattr(main_window, 'populate_entries_table'):
            main_window.populate_entries_table()
        else:
            # Fallback: manually refresh table data
            if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
                table = main_window.gui_layout.table
                from apps.methods.populate_img_table import populate_img_entries_table
                populate_img_entries_table(main_window, new_img)
        
        entry_count = len(new_img.entries) if new_img.entries else 0
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"IMG file fresh from disk: {filename} ({entry_count} entries)")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"IMG reload error: {str(e)}")
        return False


def _reload_col_in_tab(main_window, file_path: str) -> bool: #vers 1
    """Reload COL file in current tab"""
    try:
        filename = os.path.basename(file_path)
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Reloading COL: {filename}")
        # Load new COL instance
        from apps.methods.col_core_classes import COLFile
        new_col = COLFile(file_path)
        if not new_col.load():
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Failed to reload COL: {filename}")
            return False
        # Update current tab's file object
        current_index = main_window.main_tab_widget.currentIndex()
        tab_widget = main_window.main_tab_widget.widget(current_index)
        if tab_widget:
            tab_widget.file_object = new_col
            tab_widget.file_type = 'COL'
        # Update main_window reference (for compatibility)
        main_window.current_col = new_col
        # Refresh UI
        if hasattr(main_window, 'refresh_current_tab_data'):
            main_window.refresh_current_tab_data()
        elif hasattr(main_window, 'populate_entries_table'):
            main_window.populate_entries_table()
        model_count = len(new_col.models) if hasattr(new_col, 'models') and new_col.models else 0
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"COL file fresh from disk: {filename} ({model_count} models)")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"COL reload error: {str(e)}")
        return False

def integrate_reload_functions(main_window) -> bool: #vers 9
    """Integrate reload functions - TAB AWARE"""
    try:
        # Add reload methods
        main_window.reload_current_file = lambda: reload_current_file(main_window)
        # Add aliases
        main_window.reload_file = main_window.reload_current_file
        main_window.reload_table = main_window.reload_current_file
        main_window.refresh_current_file = main_window.reload_current_file
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Reload functions integrated (tab-aware)")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Reload integration failed: {str(e)}")
        return False

# Export functions
__all__ = [
    'reload_current_file',
    'integrate_reload_functions'
]
