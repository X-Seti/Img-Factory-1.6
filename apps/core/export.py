#this belongs in core/export.py - Version: 2
# X-Seti - November19 2025 - IMG Factory 1.5 - Export Functions
"""
Export Functions - Tab-aware export with routing to IMG/COL handlers
Exports selected or all entries as individual files (no combining)
Routes to appropriate handler based on file type
"""

from PyQt6.QtWidgets import QMessageBox
from apps.methods.tab_system import get_current_file_from_active_tab
from apps.methods.img_export_functions import export_img_selected, export_img_all
from apps.methods.col_export_functions import export_col_selected, export_col_all

##Methods list -
# export_selected_function
# export_all_function
# integrate_export_functions


def export_selected_function(main_window) -> bool: #vers 2
    """Export selected entries - Tab-aware routing to IMG or COL handler
    Args:
        main_window: Main application window
    Returns:
        True if export successful, False otherwise
    """
    try:
        # Get file from active tab
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if not file_object:
            QMessageBox.warning(main_window, "No File",
                "No file is currently loaded")
            return False
        # Route to appropriate handler
        if file_type == 'IMG':
            return export_img_selected(main_window, file_object)
        elif file_type == 'COL':
            return export_col_selected(main_window, file_object)
        else:
            QMessageBox.warning(main_window, "Unsupported File Type",
                f"Export not supported for {file_type} files")
            return False
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Export selected error: {str(e)}")
        return False


def export_all_function(main_window) -> bool: #vers 2
    """Export all entries - Tab-aware routing to IMG or COL handler
    Args:
        main_window: Main application window
    Returns:
        True if export successful, False otherwise
    """
    try:
        # Get file from active tab
        file_object, file_type = get_current_file_from_active_tab(main_window)
        if not file_object:
            QMessageBox.warning(main_window, "No File",
                "No file is currently loaded")
            return False
        # Route to appropriate handler
        if file_type == 'IMG':
            return export_img_all(main_window, file_object)
        elif file_type == 'COL':
            return export_col_all(main_window, file_object)
        else:
            QMessageBox.warning(main_window, "Unsupported File Type",
                f"Export not supported for {file_type} files")
            return False
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Export all error: {str(e)}")
        return False


def integrate_export_functions(main_window) -> bool: #vers 2
    """Integrate export functions into main window with all aliases
    Args:
        main_window: Main application window
    Returns:
        True if integration successful
    """
    try:
        # Main export functions
        main_window.export_selected_function = lambda: export_selected_function(main_window)
        main_window.export_all_function = lambda: export_all_function(main_window)
        # Add all aliases that GUI might use
        main_window.export_selected = main_window.export_selected_function
        main_window.export_selected_entries = main_window.export_selected_function
        main_window.export_all_entries = main_window.export_all_function
        main_window.export_all = main_window.export_all_function
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Export functions integrated")
            main_window.log_message("   • Tab-aware file detection")
            main_window.log_message("   • Individual file export only")
            main_window.log_message("   • IMG/COL routing support")
            main_window.log_message("   • Overwrite checking")
            main_window.log_message("   ✅ No global file_object/file_type")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Export integration failed: {str(e)}")
        return False


# Export functions
__all__ = [
    'export_selected_function',
    'export_all_function',
    'integrate_export_functions'
]
