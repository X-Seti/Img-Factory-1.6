#this belongs in methods/column_width_manager.py - Version: 1
# X-Seti - February04 2026 - Img Factory 1.6 - Column Width Manager
"""
Column Width Manager - Handles saving and restoring table column widths
"""

from PyQt6.QtWidgets import QTableWidget, QHeaderView
from apps.methods.img_factory_settings import IMGFactorySettings
from typing import Dict, List, Optional

##Methods list -
# apply_column_widths
# get_default_column_widths
# integrate_column_width_manager
# save_column_widths
# setup_column_width_tracking

def get_default_column_widths(table_type: str = "img") -> Dict[str, int]: #vers 1
    """Get default column widths for different table types
    
    Args:
        table_type: Type of table ('img', 'col', 'txd')
    
    Returns:
        Dictionary mapping column names to default widths
    """
    defaults = {
        "img": {
            "Name": 190,
            "Type": 60,
            "Size": 90,
            "Offset": 100,
            "RW Address": 100,
            "RW Version": 100,
            "Compression": 110,
            "Status": 110
        },
        "col": {
            "Name": 200,
            "Type": 80,
            "Size": 100,
            "Bounds": 150,
            "Spheres": 100,
            "Boxes": 100,
            "Vertices": 100,
            "Faces": 100
        },
        "txd": {
            "Name": 200,
            "Format": 100,
            "Width": 80,
            "Height": 80,
            "Mipmaps": 80,
            "Size": 100
        }
    }
    
    return defaults.get(table_type, defaults["img"])


def save_column_widths(table: QTableWidget, table_type: str = "img") -> bool: #vers 1
    """Save current column widths to settings
    
    Args:
        table: QTableWidget instance
        table_type: Type of table ('img', 'col', 'txd')
    
    Returns:
        True if saved successfully
    """
    try:
        if not table:
            return False
        
        settings = IMGFactorySettings()
        widths = {}
        
        # Read current column widths
        for col in range(table.columnCount()):
            header_item = table.horizontalHeaderItem(col)
            if header_item:
                column_name = header_item.text()
                width = table.columnWidth(col)
                widths[column_name] = width
        
        # Save to settings with table type prefix
        settings_key = f"column_widths_{table_type}"
        settings.set(settings_key, widths)
        settings.save_settings()  # FIXED: Changed from settings.save()
        
        return True
        
    except Exception as e:
        print(f"Error saving column widths: {str(e)}")
        return False


def apply_column_widths(table: QTableWidget, table_type: str = "img") -> bool: #vers 1
    """Apply saved column widths to table
    
    Args:
        table: QTableWidget instance
        table_type: Type of table ('img', 'col', 'txd')
    
    Returns:
        True if applied successfully
    """
    try:
        if not table:
            return False
        
        settings = IMGFactorySettings()
        settings_key = f"column_widths_{table_type}"
        
        # Load saved widths or use defaults
        saved_widths = settings.get(settings_key, None)
        if not saved_widths:
            saved_widths = get_default_column_widths(table_type)
        
        # Apply widths to columns
        for col in range(table.columnCount()):
            header_item = table.horizontalHeaderItem(col)
            if header_item:
                column_name = header_item.text()
                if column_name in saved_widths:
                    table.setColumnWidth(col, saved_widths[column_name])
        
        return True
        
    except Exception as e:
        print(f"Error applying column widths: {str(e)}")
        return False


def setup_column_width_tracking(table: QTableWidget, main_window, table_type: str = "img") -> bool: #vers 1
    """Setup automatic column width tracking and saving
    
    Args:
        table: QTableWidget instance
        main_window: Main window instance (for logging)
        table_type: Type of table ('img', 'col', 'txd')
    
    Returns:
        True if setup successfully
    """
    try:
        if not table:
            return False
        
        header = table.horizontalHeader()
        if not header:
            return False
        
        # Connect to column resize signal
        def on_column_resized(logical_index, old_size, new_size):
            """Save column widths when user resizes columns"""
            save_column_widths(table, table_type)
            if hasattr(main_window, 'log_message'):
                column_name = table.horizontalHeaderItem(logical_index).text() if table.horizontalHeaderItem(logical_index) else f"Col{logical_index}"
                main_window.log_message(f"Column '{column_name}' resized to {new_size}px")
        
        header.sectionResized.connect(on_column_resized)
        
        # Apply saved widths immediately
        apply_column_widths(table, table_type)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Column width tracking enabled for {table_type.upper()} table")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error setting up column width tracking: {str(e)}")
        return False


def integrate_column_width_manager(main_window) -> bool: #vers 1
    """Integrate column width manager into main window
    
    Args:
        main_window: Main window instance
    
    Returns:
        True if integrated successfully
    """
    try:
        # Add methods to main window
        main_window.save_column_widths = lambda table, table_type="img": save_column_widths(table, table_type)
        main_window.apply_column_widths = lambda table, table_type="img": apply_column_widths(table, table_type)
        main_window.setup_column_width_tracking = lambda table, table_type="img": setup_column_width_tracking(table, main_window, table_type)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Column width manager integrated")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Failed to integrate column width manager: {str(e)}")
        return False


# Export functions
__all__ = [
    'get_default_column_widths',
    'save_column_widths',
    'apply_column_widths',
    'setup_column_width_tracking',
    'integrate_column_width_manager'
]
