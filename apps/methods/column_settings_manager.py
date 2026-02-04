#this belongs in methods/column_settings_manager.py - Version: 1
# X-Seti - February04 2026 - Img Factory 1.6 - Column Settings Manager
"""
Column Settings Manager - Handles column visibility, resize modes, and preferences
"""

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QGroupBox, QCheckBox, 
    QPushButton, QRadioButton, QButtonGroup, QLabel
)
from PyQt6.QtCore import Qt
from apps.methods.img_factory_settings import IMGFactorySettings

##Methods list -
# apply_column_settings
# get_default_column_settings
# integrate_column_settings_manager
# save_column_settings
# show_column_settings_dialog

##class ColumnSettingsDialog -
# __init__
# _apply_settings
# _create_buttons
# _create_resize_mode_group
# _create_ui
# _create_visibility_group
# _load_current_settings
# _reset_to_defaults

def get_default_column_settings(table_type: str = "img") -> dict: #vers 1
    """Get default column settings for different table types
    
    Args:
        table_type: Type of table ('img', 'col', 'txd')
    
    Returns:
        Dictionary with column settings
    """
    defaults = {
        "img": {
            "resize_mode": "last_stretch",  # Options: "all_fixed", "last_stretch", "name_stretch"
            "visible_columns": {
                "Name": True,
                "Type": True,
                "Size": True,
                "Offset": True,
                "RW Address": False,
                "RW Version": False,
                "Compression": True,
                "Status": True,
                "Creation Date": False,
                "Show Pinned": True
            }
        },
        "col": {
            "resize_mode": "last_stretch",
            "visible_columns": {
                "Name": True,
                "Type": True,
                "Size": True,
                "Bounds": True,
                "Spheres": False,
                "Boxes": False,
                "Vertices": True,
                "Faces": True
            }
        },
        "txd": {
            "resize_mode": "last_stretch",
            "visible_columns": {
                "Name": True,
                "Format": True,
                "Width": True,
                "Height": True,
                "Mipmaps": False,
                "Size": True
            }
        }
    }
    
    return defaults.get(table_type, defaults["img"])


def save_column_settings(table_type: str, settings: dict) -> bool: #vers 1
    """Save column settings to persistent storage
    
    Args:
        table_type: Type of table ('img', 'col', 'txd')
        settings: Settings dictionary to save
    
    Returns:
        True if saved successfully
    """
    try:
        app_settings = IMGFactorySettings()
        settings_key = f"column_settings_{table_type}"
        app_settings.set(settings_key, settings)
        app_settings.save_settings()
        return True
    except Exception as e:
        print(f"Error saving column settings: {str(e)}")
        return False


def apply_column_settings(table, main_window, table_type: str = "img") -> bool: #vers 1
    """Apply column settings to table (visibility and resize mode)
    
    Args:
        table: QTableWidget instance
        main_window: Main window for logging
        table_type: Type of table ('img', 'col', 'txd')
    
    Returns:
        True if applied successfully
    """
    try:
        from PyQt6.QtWidgets import QHeaderView
        
        # Load settings
        app_settings = IMGFactorySettings()
        settings_key = f"column_settings_{table_type}"
        settings = app_settings.get(settings_key, get_default_column_settings(table_type))
        
        header = table.horizontalHeader()
        resize_mode = settings.get("resize_mode", "last_stretch")
        visible_columns = settings.get("visible_columns", {})
        
        # Apply visibility settings
        for col in range(table.columnCount()):
            header_item = table.horizontalHeaderItem(col)
            if header_item:
                column_name = header_item.text()
                is_visible = visible_columns.get(column_name, True)
                table.setColumnHidden(col, not is_visible)
        
        # Apply resize mode
        if resize_mode == "all_fixed":
            # All columns manually resizable, no auto-stretch
            for col in range(table.columnCount()):
                header.setSectionResizeMode(col, QHeaderView.ResizeMode.Interactive)
            header.setStretchLastSection(False)
            
        elif resize_mode == "last_stretch":
            # All columns resizable, last visible column stretches
            for col in range(table.columnCount()):
                header.setSectionResizeMode(col, QHeaderView.ResizeMode.Interactive)
            header.setStretchLastSection(True)
            
        elif resize_mode == "name_stretch":
            # Name (column 0) stretches, others are interactive
            header.setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
            for col in range(1, table.columnCount()):
                header.setSectionResizeMode(col, QHeaderView.ResizeMode.Interactive)
            header.setStretchLastSection(False)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Applied {table_type.upper()} column settings: {resize_mode}")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error applying column settings: {str(e)}")
        return False


class ColumnSettingsDialog(QDialog): #vers 1
    """Dialog for configuring table column settings"""
    
    def __init__(self, main_window, table_type: str = "img", parent=None): #vers 1
        super().__init__(parent)
        self.main_window = main_window
        self.table_type = table_type
        self.setWindowTitle(f"{table_type.upper()} Column Settings")
        self.setMinimumWidth(400)
        
        self._create_ui()
        self._load_current_settings()
    
    def _create_ui(self): #vers 1
        """Create the dialog UI"""
        layout = QVBoxLayout(self)
        
        # Resize mode group
        layout.addWidget(self._create_resize_mode_group())
        
        # Column visibility group
        layout.addWidget(self._create_visibility_group())
        
        # Buttons
        layout.addLayout(self._create_buttons())
    
    def _create_resize_mode_group(self): #vers 1
        """Create resize mode selection group"""
        group = QGroupBox("Column Width Behavior")
        layout = QVBoxLayout()
        
        self.resize_group = QButtonGroup()
        
        self.all_fixed_radio = QRadioButton("All columns manually resizable (no auto-stretch)")
        self.last_stretch_radio = QRadioButton("Last column stretches to fill (recommended)")
        self.name_stretch_radio = QRadioButton("Name column stretches to fill")
        
        self.resize_group.addButton(self.all_fixed_radio, 0)
        self.resize_group.addButton(self.last_stretch_radio, 1)
        self.resize_group.addButton(self.name_stretch_radio, 2)
        
        layout.addWidget(self.all_fixed_radio)
        layout.addWidget(self.last_stretch_radio)
        layout.addWidget(self.name_stretch_radio)
        
        group.setLayout(layout)
        return group
    
    def _create_visibility_group(self): #vers 1
        """Create column visibility checkboxes"""
        group = QGroupBox("Visible Columns")
        layout = QVBoxLayout()
        
        # Get default columns for this table type
        defaults = get_default_column_settings(self.table_type)
        visible_columns = defaults.get("visible_columns", {})
        
        self.column_checkboxes = {}
        
        for column_name in visible_columns.keys():
            checkbox = QCheckBox(column_name)
            self.column_checkboxes[column_name] = checkbox
            layout.addWidget(checkbox)
        
        group.setLayout(layout)
        return group
    
    def _create_buttons(self): #vers 1
        """Create dialog buttons"""
        layout = QHBoxLayout()
        
        reset_btn = QPushButton("Reset to Defaults")
        apply_btn = QPushButton("Apply")
        ok_btn = QPushButton("OK")
        cancel_btn = QPushButton("Cancel")
        
        reset_btn.clicked.connect(self._reset_to_defaults)
        apply_btn.clicked.connect(self._apply_settings)
        ok_btn.clicked.connect(lambda: (self._apply_settings(), self.accept()))
        cancel_btn.clicked.connect(self.reject)
        
        layout.addWidget(reset_btn)
        layout.addStretch()
        layout.addWidget(apply_btn)
        layout.addWidget(ok_btn)
        layout.addWidget(cancel_btn)
        
        return layout
    
    def _load_current_settings(self): #vers 1
        """Load current settings from storage"""
        app_settings = IMGFactorySettings()
        settings_key = f"column_settings_{self.table_type}"
        settings = app_settings.get(settings_key, get_default_column_settings(self.table_type))
        
        # Set resize mode
        resize_mode = settings.get("resize_mode", "last_stretch")
        if resize_mode == "all_fixed":
            self.all_fixed_radio.setChecked(True)
        elif resize_mode == "last_stretch":
            self.last_stretch_radio.setChecked(True)
        elif resize_mode == "name_stretch":
            self.name_stretch_radio.setChecked(True)
        
        # Set column visibility
        visible_columns = settings.get("visible_columns", {})
        for column_name, checkbox in self.column_checkboxes.items():
            checkbox.setChecked(visible_columns.get(column_name, True))
    
    def _apply_settings(self): #vers 1
        """Apply settings and save"""
        # Gather settings
        resize_mode_map = {0: "all_fixed", 1: "last_stretch", 2: "name_stretch"}
        resize_mode = resize_mode_map.get(self.resize_group.checkedId(), "last_stretch")
        
        visible_columns = {}
        for column_name, checkbox in self.column_checkboxes.items():
            visible_columns[column_name] = checkbox.isChecked()
        
        settings = {
            "resize_mode": resize_mode,
            "visible_columns": visible_columns
        }
        
        # Save settings
        save_column_settings(self.table_type, settings)
        
        # Apply to current table if available
        if hasattr(self.main_window, 'gui_layout') and hasattr(self.main_window.gui_layout, 'table'):
            apply_column_settings(self.main_window.gui_layout.table, self.main_window, self.table_type)
        
        if hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"Column settings applied and saved")
    
    def _reset_to_defaults(self): #vers 1
        """Reset to default settings"""
        defaults = get_default_column_settings(self.table_type)
        
        # Reset resize mode
        resize_mode = defaults.get("resize_mode", "last_stretch")
        if resize_mode == "all_fixed":
            self.all_fixed_radio.setChecked(True)
        elif resize_mode == "last_stretch":
            self.last_stretch_radio.setChecked(True)
        elif resize_mode == "name_stretch":
            self.name_stretch_radio.setChecked(True)
        
        # Reset column visibility
        visible_columns = defaults.get("visible_columns", {})
        for column_name, checkbox in self.column_checkboxes.items():
            checkbox.setChecked(visible_columns.get(column_name, True))


def show_column_settings_dialog(main_window, table_type: str = "img"): #vers 1
    """Show column settings dialog
    
    Args:
        main_window: Main window instance
        table_type: Type of table ('img', 'col', 'txd')
    """
    dialog = ColumnSettingsDialog(main_window, table_type)
    dialog.exec()


def integrate_column_settings_manager(main_window) -> bool: #vers 1
    """Integrate column settings manager into main window
    
    Args:
        main_window: Main window instance
    
    Returns:
        True if integrated successfully
    """
    try:
        # Add methods to main window
        main_window.show_column_settings = lambda table_type="img": show_column_settings_dialog(main_window, table_type)
        main_window.apply_column_settings = lambda table, table_type="img": apply_column_settings(table, main_window, table_type)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Column settings manager integrated")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Failed to integrate column settings manager: {str(e)}")
        return False


# Export functions
__all__ = [
    'get_default_column_settings',
    'save_column_settings',
    'apply_column_settings',
    'show_column_settings_dialog',
    'integrate_column_settings_manager',
    'ColumnSettingsDialog'
]
