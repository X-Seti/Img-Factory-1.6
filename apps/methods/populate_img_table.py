#this belongs in methods/populate_img_table.py - Version: 10
# X-Seti - November18 2025 - IMG Factory 1.5
"""
IMG Table Population
"""
import os
from typing import Any, List, Optional
from PyQt6.QtWidgets import QTableWidgetItem, QTableWidget, QHeaderView
from PyQt6.QtCore import Qt
from apps.debug.debug_functions import img_debugger
try:
    from apps.utils.img_debug_logger import img_debugger
except ImportError:
    class DummyDebugger:
        def debug(self, msg): print(f"DEBUG: {msg}")
        def error(self, msg): print(f"ERROR: {msg}")
        def info(self, msg): print(f"INFO: {msg}")
    img_debugger = DummyDebugger()

##Methods list - imported from tables_structure
# reset_table_styling
# setup_table_for_img_data
# setup_table_structure
##Methods list -
# create_img_table_item
# format_img_entry_size
# get_img_entry_type
# populate_table_row_minimal
# populate_table_with_img_data_minimal
# refresh_img_table
# update_img_table_selection_info

def reset_table_styling(main_window): #vers 2
    """Completely reset table styling to default using IMG debug system"""
    try:
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            img_debugger.warning("No table widget available for styling reset")
            return False
        table = main_window.gui_layout.table
        header = table.horizontalHeader()
        table.setStyleSheet("")
        header.setStyleSheet("")
        table.setObjectName("")
        table.setAlternatingRowColors(True)
        table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        table.setSelectionMode(QTableWidget.SelectionMode.ExtendedSelection)
        header.setStretchLastSection(True)
        header.setSectionsClickable(True)
        header.setSortIndicatorShown(True)
        header.setSectionsMovable(False)
        main_window.log_message("Table styling completely reset")
        img_debugger.debug("Table styling reset to default")
        return True
    except Exception as e:
        error_msg = f"Error resetting table styling: {str(e)}"
        main_window.log_message(f"Error: {error_msg}")
        img_debugger.error(error_msg)
        return False

def setup_table_for_img_data(table: QTableWidget) -> bool: #vers 3
    """Setup table structure for IMG file data"""
    try:
        img_headers = ["Name", "Type", "Size", "Offset", "RW Address", "RW Version", "Compression", "Status"]
        table.setColumnCount(8)
        table.setHorizontalHeaderLabels(img_headers)
        table.setColumnWidth(0, 190)  # Name
        table.setColumnWidth(1, 60)   # Type
        table.setColumnWidth(2, 90)   # Size
        table.setColumnWidth(3, 100)  # Offset
        table.setColumnWidth(4, 100)  # RW Address
        table.setColumnWidth(5, 100)  # RW Version
        table.setColumnWidth(6, 110)  # Compression
        table.setColumnWidth(7, 110)  # Status
        header = table.horizontalHeader()
        header.setSectionsMovable(True)
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        for col in range(1, 8):
            header.setSectionResizeMode(col, QHeaderView.ResizeMode.Interactive)
        table.setSortingEnabled(True)
        img_debugger.debug("Table structure setup for IMG data")
        return True
    except Exception as e:
        img_debugger.error(f"Error setting up IMG table structure: {e}")
        return False

class IMGTablePopulator:
    """Handles IMG table population with minimal processing to prevent freezing"""
    def __init__(self, main_window):
        self.main_window = main_window

    def populate_table_with_img_data(self, img_file: Any) -> bool: #vers 9
        """Populate table with IMG entry data - MINIMAL VERSION to prevent freezing"""
        try:
            if not img_file or not hasattr(img_file, 'entries'):
                img_debugger.error("Invalid IMG file for table population")
                return False
            table = self.get_table_reference()
            if not table:
                img_debugger.error("No table found for IMG population")
                return False
            table.setColumnCount(8)
            table.setHorizontalHeaderLabels([
                "Name", "Type", "Size", "Offset", "RW Address", "RW Version", "Compression", "Status"
            ])
            table.setColumnWidth(0, 190)
            table.setColumnWidth(1, 60)
            table.setColumnWidth(2, 90)
            table.setColumnWidth(3, 100)
            table.setColumnWidth(4, 100)
            table.setColumnWidth(5, 100)
            table.setColumnWidth(6, 110)
            table.setColumnWidth(7, 110)
            header = table.horizontalHeader()
            header.setSectionsMovable(True)
            header.setStretchLastSection(False)
            for col in range(8):
                header.setSectionResizeMode(col, QHeaderView.ResizeMode.Stretch)
            entries = img_file.entries
            if not entries:
                img_debugger.info("No entries found in IMG file")
                return True
            table.setRowCount(len(entries))
            img_debugger.debug(f"Populating table with {len(entries)} entries")
            for row, entry in enumerate(entries):
                self.populate_table_row_minimal(table, row, entry)
            img_debugger.info(f"Table populated with {len(entries)} entries")
            return True
        except Exception as e:
            img_debugger.error(f"Error populating IMG table: {str(e)}")
            return False

    def populate_table_row_minimal(self, table: Any, row: int, entry: Any): #vers 5
        """Populate single table row with MINIMAL processing - keep all 8 columns"""
        try:
            # Check if this entry should be highlighted - OPTIMIZED
            is_highlighted = False
            highlight_type = None
            if hasattr(entry, 'is_new_entry') and entry.is_new_entry:
                is_highlighted = True
                if hasattr(entry, 'is_replaced') and entry.is_replaced:
                    highlight_type = "replaced"
                else:
                    highlight_type = "imported"
            
            # Check if entry is pinned
            is_pinned = hasattr(entry, 'is_pinned') and entry.is_pinned
            
            # Create items with minimal processing
            name_text = str(entry.name) if hasattr(entry, 'name') else f"Entry_{row}"
            name_item = self.create_img_table_item(name_text, is_highlighted, highlight_type, is_pinned)
            table.setItem(row, 0, name_item)
            
            entry_type = self.get_img_entry_type_simple(entry)
            type_item = self.create_img_table_item(entry_type, is_highlighted, highlight_type, is_pinned)
            table.setItem(row, 1, type_item)
            
            size_text = self.format_img_entry_size_simple(entry)
            size_item = self.create_img_table_item(size_text, is_highlighted, highlight_type, is_pinned)
            table.setItem(row, 2, size_item)
            
            offset_text = f"0x{entry.offset:08X}" if hasattr(entry, 'offset') else "N/A"
            offset_item = self.create_img_table_item(offset_text, is_highlighted, highlight_type, is_pinned)
            table.setItem(row, 3, offset_item)
            
            rw_address_text = self.get_rw_address_light(entry)
            rw_address_item = self.create_img_table_item(rw_address_text, is_highlighted, highlight_type, is_pinned)
            table.setItem(row, 4, rw_address_item)
            
            version_text = self.get_rw_version_light(entry)
            version_item = self.create_img_table_item(version_text, is_highlighted, highlight_type, is_pinned)
            table.setItem(row, 5, version_item)
            
            info_text = self.get_compression_info(entry)
            info_item = self.create_img_table_item(info_text, is_highlighted, highlight_type, is_pinned)
            table.setItem(row, 6, info_item)
            
            status_text = self.get_info_light(entry)
            status_item = self.create_img_table_item(status_text, is_highlighted, highlight_type, is_pinned)
            table.setItem(row, 7, status_item)
        except Exception as e:
            img_debugger.error(f"Error populating table row {row}: {str(e)}")
            table.setItem(row, 0, self.create_img_table_item(f"Error_{row}"))

    def get_img_entry_type_simple(self, entry: Any) -> str: #vers 2
        """Get entry type - SIMPLE extension extraction, no heavy processing"""
        try:
            if hasattr(entry, 'name') and '.' in entry.name:
                return entry.name.split('.')[-1].upper()
            else:
                return "UNKNOWN"
        except Exception:
            return "UNKNOWN"

    def format_img_entry_size_simple(self, entry: Any) -> str: #vers 2
        """Format entry size - SIMPLE formatting, no processing"""
        try:
            if hasattr(entry, 'size') and entry.size > 0:
                size = entry.size
            elif hasattr(entry, 'actual_size') and entry.actual_size > 0:
                size = entry.actual_size
            else:
                return "0 B"
            if size < 1024:
                return f"{size} B"
            elif size < 1024 * 1024:
                return f"{size/1024:.1f} KB"
            elif size < 1024 * 1024 * 1024:
                return f"{size/(1024*1024):.1f} MB"
            else:
                return f"{size/(1024*1024*1024):.1f} GB"
        except Exception:
            return "0 B"

    def get_rw_address_light(self, entry: Any) -> str: #vers 2
        """Get RW address - LIGHT processing, no file reading"""
        try:
            if hasattr(entry, 'rw_version') and entry.rw_version > 0:
                return f"0x{entry.rw_version:08X}"
            else:
                entry_type = self.get_img_entry_type_simple(entry)
                if entry_type in ['DFF', 'TXD']:
                    return "RW File"
                else:
                    return "N/A"
        except Exception:
            return "N/A"

    def get_rw_version_light(self, entry: Any) -> str: #vers 3
        """Get RW version - IMPROVED processing with better detection"""
        try:
            # First, try to get existing RW version info
            if hasattr(entry, 'rw_version_name') and entry.rw_version_name:
                if entry.rw_version_name not in ["Unknown", "", "N/A", "Error"]:
                    return entry.rw_version_name
            # Check for numeric RW version
            if hasattr(entry, 'rw_version') and entry.rw_version and entry.rw_version != 0:
                from apps.methods.rw_versions import get_rw_version_name
                version_name = get_rw_version_name(entry.rw_version)
                if version_name and version_name not in ["Unknown", "", "N/A", "Error"]:
                    return version_name
            # If no existing info, try to detect from entry type
            entry_type = self.get_img_entry_type_simple(entry)
            if entry_type in ['DFF', 'TXD']:
                # For DFF/TXD files, try to detect RW version from data if possible
                if hasattr(entry, '_cached_data') and entry._cached_data:
                    from apps.methods.rw_versions import parse_rw_version
                    if len(entry._cached_data) >= 12:
                        version_info = parse_rw_version(entry._cached_data[8:12])
                        if version_info and 'version' in version_info:
                            version_val = version_info['version']
                            version_name = get_rw_version_name(version_val)
                            if version_name and version_name not in ["Unknown", "", "N/A", "Error"]:
                                # Cache the detected version
                                entry.rw_version = version_val
                                entry.rw_version_name = version_name
                                return version_name
                return "RW File"
            elif entry_type == 'COL':
                return "COL File"
            elif entry_type in ['IPL', 'IDE', 'DAT']:
                return f"{entry_type} File"
            else:
                return "Unknown"
        except Exception:
            return "Unknown"

    def get_compression_info(self, entry: Any) -> str: #vers 2
        """Get compression info only"""
        try:
            if hasattr(entry, 'compression_type') and entry.compression_type:
                if str(entry.compression_type).upper() != 'NONE':
                    return str(entry.compression_type)
            return "None"
        except Exception:
            return "None"

    def get_info_light(self, entry: Any) -> str: #vers 4
        """Get entry info - LIGHT processing, no heavy detection"""
        try:
            info_parts = []
            if hasattr(entry, 'is_new_entry') and entry.is_new_entry:
                info_parts.append("Imported")
            elif hasattr(entry, 'is_replaced') and entry.is_replaced:
                info_parts.append("Replaced")
            else:
                info_parts.append("Original")
            
            # Check if entry is pinned
            if hasattr(entry, 'is_pinned') and entry.is_pinned:
                info_parts.append("Pinned")
            
            entry_type = self.get_img_entry_type_simple(entry)
            if entry_type in ['DFF']:
                info_parts.append("Model")
            elif entry_type in ['TXD']:
                info_parts.append("Texture")
            elif entry_type in ['COL']:
                info_parts.append("Collision")
            return " • ".join(info_parts) if info_parts else "Original"  # ✅ CLEAN SEPARATOR
        except Exception:
            return "Original"

    def create_img_table_item(self, text: str, is_highlighted: bool = False, highlight_type: str = None, is_pinned: bool = False) -> QTableWidgetItem: #vers 6
        """Create table item with optional highlighting and pin icon - OPTIMIZED"""
        try:
            from PyQt6.QtWidgets import QTableWidgetItem
            from PyQt6.QtGui import QColor, QBrush, QFont, QIcon
            from PyQt6.QtCore import Qt
            
            item = QTableWidgetItem(str(text))
            
            # Apply pinned icon if needed
            if is_pinned:
                # Import the SVG icon factory and add lock icon
                from apps.methods.imgfactory_svg_icons import SVGIconFactory
                lock_icon = SVGIconFactory.lock_icon(size=16, color=None)
                item.setIcon(lock_icon)
                item.setToolTip(item.toolTip() + " | Pinned" if item.toolTip() else "Pinned")
            
            # Apply highlighting if needed - OPTIMIZED
            if is_highlighted and highlight_type:
                if highlight_type == "imported":
                    # Light green background for newly imported files
                    item.setBackground(QBrush(QColor(200, 255, 200)))  # Light green
                    item.setForeground(QBrush(QColor(0, 100, 0)))      # Dark green text
                    # Make text bold - OPTIMIZED
                    font = item.font()
                    font.setBold(True)
                    item.setFont(font)
                    item.setToolTip(item.toolTip() + " | Recently imported file" if item.toolTip() else "Recently imported file")
                    
                elif highlight_type == "replaced":
                    # Light yellow background for replaced files
                    item.setBackground(QBrush(QColor(255, 255, 200)))  # Light yellow
                    item.setForeground(QBrush(QColor(150, 100, 0)))    # Dark orange text
                    # Make text bold - OPTIMIZED
                    font = item.font()
                    font.setBold(True)
                    item.setFont(font)
                    item.setToolTip(item.toolTip() + " | Recently replaced file" if item.toolTip() else "Recently replaced file")
                    
            return item
        except Exception:
            return QTableWidgetItem("Error")

    def get_table_reference(self): #vers 2
        """Get table reference from main window"""
        try:
            if hasattr(self.main_window, 'gui_layout') and hasattr(self.main_window.gui_layout, 'table'):
                return self.main_window.gui_layout.table
            elif hasattr(self.main_window, 'table'):
                return self.main_window.table
            else:
                img_debugger.error("No table found in main window")
                return None
        except Exception as e:
            img_debugger.error(f"Error getting table reference: {str(e)}")
            return None

# Standalone functions for compatibility
def populate_img_table(table, img_file) -> bool: #vers 3
    """Standalone function for IMG table population - MINIMAL VERSION"""
    try:
        class DummyWindow:
            def __init__(self, table):
                self.gui_layout = type('obj', (object,), {'table': table})
        dummy_window = DummyWindow(table)
        populator = IMGTablePopulator(dummy_window)
        return populator.populate_table_with_img_data(img_file)
    except Exception as e:
        img_debugger.error(f"Error in standalone populate_img_table: {e}")
        if table:
            table.setRowCount(0)
        return False

def clear_img_table(main_window) -> bool: #vers 2
    """Clear IMG table contents"""
    try:
        populator = IMGTablePopulator(main_window)
        table = populator.get_table_reference()
        if table:
            table.setRowCount(0)
            table.clearContents()
            return True
        else:
            return False
    except Exception as e:
        img_debugger.error(f"Error clearing IMG table: {str(e)}")
        return False

def refresh_img_table(main_window) -> bool: #vers 2
    """Refresh the IMG table with current data"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            populator = IMGTablePopulator(main_window)
            return populator.populate_table_with_img_data(main_window.current_img)
        else:
            clear_img_table(main_window)
            return False
    except Exception as e:
        img_debugger.error(f"Error refreshing IMG table: {str(e)}")
        return False

def install_img_table_populator(main_window): #vers 2
    """Install IMG table populator into main window"""
    try:
        main_window.populate_img_table = lambda img_file: populate_img_table(
            main_window.gui_layout.table if hasattr(main_window, 'gui_layout') else None,
            img_file
        )
        main_window.clear_img_table = lambda: clear_img_table(main_window)
        main_window.refresh_img_table = lambda: refresh_img_table(main_window)
        img_debugger.info("Minimal IMG table populator installed")
        return True
    except Exception as e:
        img_debugger.error(f"Error installing IMG table populator: {str(e)}")
        return False

def update_img_table_selection_info(main_window) -> bool: #vers 2
    """Update selection info for IMG table"""
    try:
        populator = IMGTablePopulator(main_window)
        table = populator.get_table_reference()
        if not table:
            return False
        selected_rows = len(table.selectionModel().selectedRows())
        total_rows = table.rowCount()
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Selected {selected_rows} of {total_rows} entries")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error updating selection info: {str(e)}")
        return False

# Export functions
__all__ = [
    'IMGTablePopulator',
    'populate_img_table',
    'refresh_img_table',
    'clear_img_table',
    'install_img_table_populator',
    'update_img_table_selection_info'
]
