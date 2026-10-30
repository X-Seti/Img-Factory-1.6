#this belongs in methods/file_validation.py - Version: 1
# X-Seti - November15 2025 - IMG Factory 1.5 - Universal File Validation

"""
Universal File Validation - Dynamic validation for IMG, COL, TXD files
Replaces hardcoded "Current tab does not contain an IMG file" checks
Works with tab system to detect any file type
"""

from typing import Optional, Tuple, Any, List
from PyQt6.QtWidgets import QMessageBox

##Methods list -
# validate_file_for_operation
# validate_img_file
# validate_col_file
# validate_txd_file
# validate_any_file
# get_file_from_tab
# get_selected_entries_for_operation

def get_file_from_tab(main_window) -> Tuple[Optional[Any], str]: #vers 1
    """
    Get file from current tab using tab system
    
    Returns:
        (file_object, file_type) where file_type is 'IMG', 'COL', 'TXD', or 'NONE'
    """
    try:
        # Use tab system if available
        if hasattr(main_window, 'get_current_file_from_active_tab'):
            return main_window.get_current_file_from_active_tab()
        
        # Fallback to direct tab check
        if hasattr(main_window, 'main_tab_widget'):
            current_index = main_window.main_tab_widget.currentIndex()
            if current_index >= 0:
                tab_widget = main_window.main_tab_widget.widget(current_index)
                if tab_widget:
                    file_object = getattr(tab_widget, 'file_object', None)
                    file_type = getattr(tab_widget, 'file_type', 'NONE')
                    if file_object and file_type != 'NONE':
                        return file_object, file_type
        
        return None, 'NONE'
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error getting file from tab: {str(e)}")
        return None, 'NONE'


def validate_file_for_operation(main_window, operation_name: str, 
                                required_type: Optional[str] = None) -> Tuple[bool, Optional[Any], str]: #vers 1
    """
    Universal file validation for any operation
    
    Args:
        main_window: Main window instance
        operation_name: Name of operation (for error messages)
        required_type: Required file type ('IMG', 'COL', 'TXD') or None for any
        
    Returns:
        (success, file_object, file_type)
    """
    try:
        # Check if tab system exists
        if not hasattr(main_window, 'main_tab_widget'):
            QMessageBox.warning(main_window, "No Tabs", 
                f"Cannot perform {operation_name}: Tab system not available.")
            return False, None, 'NONE'
        
        # Check if any tab is active
        current_index = main_window.main_tab_widget.currentIndex()
        if current_index == -1:
            QMessageBox.warning(main_window, "No Active Tab", 
                f"Cannot perform {operation_name}: No tab is currently active.")
            return False, None, 'NONE'
        
        # Get file from tab
        file_object, file_type = get_file_from_tab(main_window)
        
        # Check if file exists
        if not file_object or file_type == 'NONE':
            QMessageBox.warning(main_window, "No File Loaded", 
                f"Cannot perform {operation_name}: No file is loaded in the active tab.")
            return False, None, 'NONE'
        
        # Check if correct type (if specified)
        if required_type and file_type != required_type:
            QMessageBox.warning(main_window, f"Wrong File Type", 
                f"Cannot perform {operation_name}: Current tab contains a {file_type} file, but {required_type} file is required.")
            return False, file_object, file_type
        
        # Success
        return True, file_object, file_type
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Validation error for {operation_name}: {str(e)}")
        return False, None, 'NONE'


def validate_img_file(main_window, operation_name: str = "operation") -> Tuple[bool, Optional[Any]]: #vers 1
    """
    Validate IMG file for operation - REPLACES hardcoded IMG checks
    
    Args:
        main_window: Main window instance
        operation_name: Operation name for error message
        
    Returns:
        (success, img_file_object)
    """
    success, file_object, file_type = validate_file_for_operation(main_window, operation_name, 'IMG')
    return success, file_object if success else None


def validate_col_file(main_window, operation_name: str = "operation") -> Tuple[bool, Optional[Any]]: #vers 1
    """
    Validate COL file for operation
    
    Args:
        main_window: Main window instance
        operation_name: Operation name for error message
        
    Returns:
        (success, col_file_object)
    """
    success, file_object, file_type = validate_file_for_operation(main_window, operation_name, 'COL')
    return success, file_object if success else None


def validate_txd_file(main_window, operation_name: str = "operation") -> Tuple[bool, Optional[Any]]: #vers 1
    """
    Validate TXD file for operation
    
    Args:
        main_window: Main window instance
        operation_name: Operation name for error message
        
    Returns:
        (success, txd_file_object)
    """
    success, file_object, file_type = validate_file_for_operation(main_window, operation_name, 'TXD')
    return success, file_object if success else None


def validate_any_file(main_window, operation_name: str = "operation") -> Tuple[bool, Optional[Any], str]: #vers 1
    """
    Validate any file type for operation
    
    Args:
        main_window: Main window instance
        operation_name: Operation name for error message
        
    Returns:
        (success, file_object, file_type)
    """
    return validate_file_for_operation(main_window, operation_name, None)


def get_selected_entries_for_operation(main_window, operation_name: str = "operation", 
                                      required_type: Optional[str] = None) -> Tuple[bool, List[Any], Optional[Any], str]: #vers 1
    """
    Get selected entries from current tab with validation
    
    Args:
        main_window: Main window instance
        operation_name: Operation name for error messages
        required_type: Required file type or None for any
        
    Returns:
        (success, selected_entries, file_object, file_type)
    """
    try:
        # Validate file
        success, file_object, file_type = validate_file_for_operation(main_window, operation_name, required_type)
        if not success:
            return False, [], None, 'NONE'
        
        # Get selected entries from tab
        selected_entries = []
        
        if hasattr(main_window, 'get_selected_entries_from_active_tab'):
            selected_entries = main_window.get_selected_entries_from_active_tab()
        else:
            # Fallback - try to get from table
            current_index = main_window.main_tab_widget.currentIndex()
            if current_index >= 0:
                tab_widget = main_window.main_tab_widget.widget(current_index)
                if tab_widget and hasattr(tab_widget, 'table_ref'):
                    table = tab_widget.table_ref
                    if table:
                        selected_rows = set()
                        for item in table.selectedItems():
                            selected_rows.add(item.row())
                        
                        if hasattr(file_object, 'entries'):
                            for row in selected_rows:
                                if row < len(file_object.entries):
                                    selected_entries.append(file_object.entries[row])
                        elif hasattr(file_object, 'models'):
                            for row in selected_rows:
                                if row < len(file_object.models):
                                    selected_entries.append(file_object.models[row])
        
        # Check if entries selected
        if not selected_entries:
            QMessageBox.warning(main_window, "No Selection", 
                f"Cannot perform {operation_name}: No entries are selected.")
            return False, [], file_object, file_type
        
        return True, selected_entries, file_object, file_type
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error getting selected entries: {str(e)}")
        return False, [], None, 'NONE'


__all__ = [
    'validate_file_for_operation',
    'validate_img_file',
    'validate_col_file', 
    'validate_txd_file',
    'validate_any_file',
    'get_file_from_tab',
    'get_selected_entries_for_operation'
]
