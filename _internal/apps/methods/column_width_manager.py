#this belongs in methods/column_width_manager.py - Version: 2
# X-Seti - February 25 2026 - Img Factory 1.6 - Column Width Manager
"""
Column Width Manager - Handles saving and restoring table column widths.
Uses main_window.app_settings.current_settings so widths persist with the app config.
"""

from PyQt6.QtWidgets import QTableWidget
from typing import Dict

##Methods list -
# apply_column_widths
# get_default_column_widths
# integrate_column_width_manager
# save_column_widths
# setup_column_width_tracking


def get_default_column_widths(table_type: str = "img") -> Dict[str, int]: #vers 1
    """Default column widths per table type."""
    defaults = {
        "img": {
            "Name": 190, "Type": 60, "Date": 140, "Size": 90,
            "Offset": 100, "RW Address": 110, "RW Version": 100,
            "Compression": 110, "Status": 110
        },
        "col": {
            "Name": 200, "Type": 80, "Size": 100, "Bounds": 150,
            "Spheres": 100, "Boxes": 100, "Vertices": 100, "Faces": 100
        },
        "txd": {
            "Name": 200, "Format": 100, "Width": 80, "Height": 80,
            "Mipmaps": 80, "Size": 100
        }
    }
    return defaults.get(table_type, defaults["img"])


def _get_settings_dict(main_window) -> dict:
    """Return the live settings dict from main_window.app_settings."""
    try:
        return main_window.app_settings.current_settings
    except Exception:
        return {}


def save_column_widths(table: QTableWidget, table_type: str, main_window) -> bool: #vers 2 Fixed
    """Save current column widths into main_window.app_settings and persist to disk."""
    try:
        if not table:
            return False
        widths = {}
        for col in range(table.columnCount()):
            h = table.horizontalHeaderItem(col)
            if h:
                widths[h.text()] = table.columnWidth(col)
        settings = _get_settings_dict(main_window)
        settings[f"column_widths_{table_type}"] = widths
        # Persist
        if hasattr(main_window, 'app_settings') and hasattr(main_window.app_settings, 'save_settings'):
            main_window.app_settings.save_settings()
        return True
    except Exception as e:
        print(f"[COLWIDTH] save error: {e}")
        return False


def apply_column_widths(table: QTableWidget, table_type: str, main_window) -> bool: #vers 2 Fixed
    """Apply saved (or default) column widths to table."""
    try:
        if not table:
            return False
        settings = _get_settings_dict(main_window)
        widths = settings.get(f"column_widths_{table_type}") or get_default_column_widths(table_type)
        for col in range(table.columnCount()):
            h = table.horizontalHeaderItem(col)
            if h and h.text() in widths:
                table.setColumnWidth(col, widths[h.text()])
        return True
    except Exception as e:
        print(f"[COLWIDTH] apply error: {e}")
        return False


def setup_column_width_tracking(table: QTableWidget, main_window, table_type: str = "img") -> bool: #vers 2 Fixed
    """Connect sectionResized to save widths; apply saved widths immediately."""
    try:
        if not table:
            return False
        header = table.horizontalHeader()
        if not header:
            return False

        def on_resized(logical_index, old_size, new_size):
            # Guard: skip zero-size and negative (happens mid-drag)
            if new_size > 0:
                save_column_widths(table, table_type, main_window)

        def on_moved(logical, old_visual, new_visual):
            # Save after a column drag completes
            save_column_widths(table, table_type, main_window)

        # Disconnect any previous connections to avoid duplicates
        try:
            header.sectionResized.disconnect()
        except Exception:
            pass
        try:
            header.sectionMoved.disconnect()
        except Exception:
            pass
        header.sectionResized.connect(on_resized)
        # NOTE: sectionMoved is NOT connected intentionally - drag-reorder causes
        # SIGSEGV in Qt when saving during the move gesture. Column move is disabled.
        header.setSectionsMovable(False)

        apply_column_widths(table, table_type, main_window)
        return True
    except Exception as e:
        print(f"[COLWIDTH] setup error: {e}")
        return False


def integrate_column_width_manager(main_window) -> bool: #vers 2 Fixed
    """Attach helpers to main_window and set up tracking on current table."""
    try:
        main_window.save_column_widths = lambda t, tt="img": save_column_widths(t, tt, main_window)
        main_window.apply_column_widths = lambda t, tt="img": apply_column_widths(t, tt, main_window)
        main_window.setup_column_width_tracking = lambda t, tt="img": setup_column_width_tracking(t, main_window, tt)

        # Set up on the current table if available
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            setup_column_width_tracking(main_window.gui_layout.table, main_window, "img")

        return True
    except Exception as e:
        print(f"[COLWIDTH] integrate error: {e}")
        return False


__all__ = [
    'get_default_column_widths',
    'save_column_widths',
    'apply_column_widths',
    'setup_column_width_tracking',
    'integrate_column_width_manager'
]
