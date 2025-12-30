#this belongs in methods/col_table.py - Version: 1
# X-Seti - December18 2025 - Col Workshop - COL Table Population
"""
COL Table Population - Display COL models in table widget
"""

import os
from PyQt6.QtWidgets import QTableWidgetItem
from PyQt6.QtCore import Qt

# Optional debug - use print if not available
try:
    from apps.debug.debug_functions import img_debugger
except ImportError:
    class SimpleDebugger:
        def debug(self, msg): print(f"DEBUG: {msg}")
        def error(self, msg): print(f"ERROR: {msg}")
        def warning(self, msg): print(f"WARN: {msg}")
        def success(self, msg): print(f"SUCCESS: {msg}")
    img_debugger = SimpleDebugger()

##Methods list -
# populate_col_table
# setup_col_table_structure

def setup_col_table_structure(main_window) -> bool: #vers 1
    """Setup table columns for COL data"""
    try:
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            img_debugger.error("No table widget available")
            return False
        
        table = main_window.gui_layout.table
        
        # COL columns
        col_headers = ["Model Name", "Type", "Version", "Size", "Spheres", "Boxes", "Vertices", "Faces"]
        table.setColumnCount(len(col_headers))
        table.setHorizontalHeaderLabels(col_headers)
        
        # Column widths
        table.setColumnWidth(0, 200)  # Model Name
        table.setColumnWidth(1, 80)   # Type
        table.setColumnWidth(2, 80)   # Version
        table.setColumnWidth(3, 100)  # Size
        table.setColumnWidth(4, 80)   # Spheres
        table.setColumnWidth(5, 80)   # Boxes
        table.setColumnWidth(6, 80)   # Vertices
        table.setColumnWidth(7, 80)   # Faces
        
        table.setSortingEnabled(True)
        
        img_debugger.debug("COL table structure ready")
        return True
        
    except Exception as e:
        img_debugger.error(f"Table setup error: {str(e)}")
        return False

def populate_col_table(main_window, col_file) -> bool: #vers 1
    """Populate table with COL models"""
    try:
        if not col_file or not hasattr(col_file, 'models') or not col_file.models:
            img_debugger.warning("No COL data")
            return False
        
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            img_debugger.error("No table widget")
            return False
        
        table = main_window.gui_layout.table
        models = col_file.models
        
        # Set rows
        table.setRowCount(len(models))
        
        for row, model in enumerate(models):
            # Model Name
            name = model.name if model.name else f"Model_{row+1}"
            item = QTableWidgetItem(name)
            item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            table.setItem(row, 0, item)
            
            # Type
            item = QTableWidgetItem("COL")
            item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            table.setItem(row, 1, item)
            
            # Version
            version_text = f"COL{model.version.value}"
            item = QTableWidgetItem(version_text)
            item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            table.setItem(row, 2, item)
            
            # Size (placeholder - calculate later if needed)
            item = QTableWidgetItem("N/A")
            item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            table.setItem(row, 3, item)
            
            # Spheres
            item = QTableWidgetItem(str(len(model.spheres)))
            item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            table.setItem(row, 4, item)
            
            # Boxes
            item = QTableWidgetItem(str(len(model.boxes)))
            item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            table.setItem(row, 5, item)
            
            # Vertices
            item = QTableWidgetItem(str(len(model.vertices)))
            item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            table.setItem(row, 6, item)
            
            # Faces
            item = QTableWidgetItem(str(len(model.faces)))
            item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            table.setItem(row, 7, item)
        
        img_debugger.success(f"Table populated: {len(models)} models")
        return True
        
    except Exception as e:
        img_debugger.error(f"Table population error: {str(e)}")
        return False

__all__ = ['setup_col_table_structure', 'populate_col_table']
