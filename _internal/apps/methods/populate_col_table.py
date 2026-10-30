#this belongs in apps/methods/populate_col_table.py - Version: 7
# X-Seti - September28 2026 - IMG Factory 1.6 - COL Table Population Methods
"""
COL Table Population Methods - shows COL file models in IMG Factory tables.
Single home for COL table setup, filling, loading and info bar.
"""

import os
from PyQt6.QtWidgets import QTableWidget, QTableWidgetItem

from apps.debug.debug_functions import img_debugger
from apps.methods.col_workshop_loader import COLFile

##Methods list -
# _table_for
# col_row_values
# load_col_file_object
# load_col_file_safely
# populate_col_table
# populate_table_with_col_data_debug
# setup_col_table_structure
# update_col_info_bar_enhanced
# validate_col_file

COL_HEADERS = ["Model Name", "Type", "Version", "Size", "Spheres", "Boxes", "Vertices", "Faces"]
COL_WIDTHS = [200, 80, 80, 100, 80, 80, 80, 80]


def _table_for(main_window, table=None): #vers 1
    """Given table, else active tab table."""
    if table is not None:
        return table
    from apps.methods.export_shared import get_active_table
    return get_active_table(main_window)


def col_row_values(model, row=0): #vers 1
    """Eight display strings for one COL model row."""
    header = getattr(model, 'header', None)
    name = getattr(model, 'name', None) or getattr(header, 'name', None) or f"Model_{row+1}"

    version = getattr(model, 'version', None)
    if version is None:
        version = getattr(header, 'version', None)
    if hasattr(version, 'value'):
        version_text = f"COL{version.value}"
    else:
        version_text = str(version) if version else "Unknown"

    spheres = len(getattr(model, 'spheres', None) or [])
    boxes = len(getattr(model, 'boxes', None) or [])
    verts = len(getattr(model, 'vertices', None) or [])
    faces = len(getattr(model, 'faces', None) or [])

    size = getattr(model, 'model_size', 0) or 0
    if not size:
        size = spheres * 20 + boxes * 28 + verts * 12 + faces * 16
    if size == 0:
        size_text = "0B LOD"
    elif size > 1024:
        size_text = f"{size // 1024}KB"
    else:
        size_text = f"{size}B"

    return [str(name), "COL", version_text, size_text,
            str(spheres), str(boxes), str(verts), str(faces)]


def setup_col_table_structure(main_window, table=None): #vers 3
    """Set COL columns, widths and sorting on the table."""
    try:
        table = _table_for(main_window, table)
        if table is None:
            img_debugger.error("No table widget available")
            return False
        table.setColumnCount(len(COL_HEADERS))
        table.setHorizontalHeaderLabels(COL_HEADERS)
        for col, width in enumerate(COL_WIDTHS):
            table.setColumnWidth(col, width)
        table.setSortingEnabled(True)
        return True
    except Exception as e:
        img_debugger.error(f"Error setting up COL table structure: {str(e)}")
        return False


def populate_col_table(main_window, col_file, table=None): #vers 4
    """Fill table rows with COL model data."""
    try:
        if not col_file or not getattr(col_file, 'models', None):
            img_debugger.warning("No COL data to populate")
            return False
        table = _table_for(main_window, table)
        if table is None:
            img_debugger.error("No table widget available")
            return False

        models = col_file.models
        sorting = table.isSortingEnabled()
        table.setSortingEnabled(False)
        table.setRowCount(len(models))
        for row, model in enumerate(models):
            for col, text in enumerate(col_row_values(model, row)):
                table.setItem(row, col, QTableWidgetItem(text))
        table.setSortingEnabled(sorting)

        img_debugger.success(f"COL table populated with {len(models)} models")
        return True
    except Exception as e:
        img_debugger.error(f"Error populating COL table: {str(e)}")
        return False


def populate_table_with_col_data_debug(main_window, col_file, table=None): #vers 3
    """Set up COL columns then fill rows on the active table."""
    table = _table_for(main_window, table)
    if not setup_col_table_structure(main_window, table):
        return False
    return populate_col_table(main_window, col_file, table)


def validate_col_file(main_window, file_path): #vers 2
    """Check COL file exists, is readable and not tiny."""
    if not os.path.exists(file_path):
        img_debugger.error(f"COL file not found: {file_path}")
        return False
    if not os.access(file_path, os.R_OK):
        img_debugger.error(f"Cannot read COL file: {file_path}")
        return False
    if os.path.getsize(file_path) < 32:
        img_debugger.error(f"COL file too small: {file_path}")
        return False
    return True


def load_col_file_object(main_window, file_path): #vers 3
    """Load and return COLFile, or None."""
    try:
        col_file = COLFile()
        if col_file.load_from_file(file_path):
            img_debugger.success(f"COL file loaded: {len(col_file.models)} models")
            return col_file
        img_debugger.error(f"Failed to load COL file: {getattr(col_file, 'load_error', 'unknown error')}")
        return None
    except Exception as e:
        img_debugger.error(f"Error loading COL file: {str(e)}")
        return None


def update_col_info_bar_enhanced(main_window, col_file, file_path): #vers 3
    """Show COL totals in the main window info bar."""
    try:
        info_bar = getattr(getattr(main_window, 'gui_layout', None), 'info_bar', None)
        if not info_bar or not col_file or not hasattr(col_file, 'models'):
            return False
        models = col_file.models
        total = lambda attr: sum(len(getattr(m, attr, None) or []) for m in models)
        info_bar.setText(
            f"{os.path.basename(file_path)} | {len(models)} models | {total('spheres')} spheres | "
            f"{total('boxes')} boxes | {total('vertices')} vertices | {total('faces')} faces")
        return True
    except Exception as e:
        img_debugger.error(f"COL info bar update failed: {str(e)}")
        return False


def load_col_file_safely(main_window, file_path): #vers 3
    """Load COL file into a new tab and show its models."""
    try:
        if not validate_col_file(main_window, file_path):
            return False
        col_file = load_col_file_object(main_window, file_path)
        if col_file is None:
            return False

        from apps.methods.tab_system import create_tab
        tab_index = create_tab(main_window, file_path=file_path, file_type='COL', file_object=col_file)
        tab_widget = main_window.main_tab_widget.widget(tab_index)
        table = getattr(tab_widget, 'table_ref', None)
        if table is None:
            tables = tab_widget.findChildren(QTableWidget)
            table = tables[-1] if tables else None
        if table is None:
            img_debugger.error("No table found in new COL tab")
            return False

        populate_table_with_col_data_debug(main_window, col_file, table)
        main_window.current_col = col_file
        update_col_info_bar_enhanced(main_window, col_file, file_path)
        img_debugger.success(f"COL file loaded: {os.path.basename(file_path)}")
        return True
    except Exception as e:
        import traceback
        img_debugger.error(f"Error loading COL file: {str(e)}")
        img_debugger.error(traceback.format_exc())
        return False


__all__ = [
    'COL_HEADERS',
    'col_row_values',
    'load_col_file_object',
    'load_col_file_safely',
    'populate_col_table',
    'populate_table_with_col_data_debug',
    'setup_col_table_structure',
    'update_col_info_bar_enhanced',
    'validate_col_file',
]
