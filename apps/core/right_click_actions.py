#this belongs in core/right_click_actions.py - Version: 5
# X-Seti - August07 2025 - IMG Factory 1.5 - Complete Right-Click Actions
# Combined: Basic copying + Advanced file operations + Extraction functionality

"""
Complete Right-Click Actions - Unified context menu system
Combines basic clipboard operations with advanced file-specific actions
"""

import os
import tempfile
from typing import Optional, List, Any
from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QApplication, QMenu, QTableWidget, QWidget, QMessageBox
from PyQt6.QtGui import QAction
from apps.core.rename import rename_entry

##Methods list -
# analyze_col_from_table
# copy_file_summary
# copy_filename_only
# copy_table_cell
# copy_table_column_data
# copy_table_row
# copy_table_selection
# edit_col_from_table
# edit_ide_file
# get_selected_entries_for_extraction
# integrate_right_click_actions
# setup_table_context_menu
# show_context_menu
# show_dff_info
# view_ide_definitions
# view_txd_textures

def setup_table_context_menu(main_window): #vers 3
    """Setup comprehensive right-click context menu for the main table"""
    print("=" * 60)
    print("DEBUG: setup_table_context_menu CALLED")
    print(f"DEBUG: main_window type: {type(main_window)}")
    print(f"DEBUG: has gui_layout: {hasattr(main_window, 'gui_layout')}")
    if hasattr(main_window, 'gui_layout'):
        print(f"DEBUG: gui_layout type: {type(main_window.gui_layout)}")
        print(f"DEBUG: has table: {hasattr(main_window.gui_layout, 'table')}")
        if hasattr(main_window.gui_layout, 'table'):
            print(f"DEBUG: table type: {type(main_window.gui_layout.table)}")
            print(f"DEBUG: table is None: {main_window.gui_layout.table is None}")
    print("=" * 60)
    try:
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            print("DEBUG: ✓ Context menu policy set to CustomContextMenu")
            table.customContextMenuRequested.connect(lambda pos: show_context_menu(main_window, pos))
            print("DEBUG: ✓ Signal connected to show_context_menu")
            main_window.log_message("Table right-click context menu enabled")
            print("DEBUG: ✓ Setup completed successfully, returning True")
            return True
        else:
            main_window.log_message("Table not available for context menu setup")
            return False
    except Exception as e:
        main_window.log_message(f"Error setting up context menu: {str(e)}")
        return False

def show_context_menu(main_window, position): #vers 6
    """Right-click context menu — grouped by function with submenus per file type."""
    try:
        from apps.methods.export_shared import get_active_table
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        icons = SVGIconFactory()

        # Get theme colour for icons
        icon_color = '#e0e0e0'
        if hasattr(main_window, 'app_settings') and main_window.app_settings:
            colors = main_window.app_settings.get_theme_colors()
            icon_color = colors.get('text_primary', icon_color)

        table = get_active_table(main_window) or main_window.gui_layout.table
        item  = table.itemAt(position)
        if not item:
            return

        menu_parent = main_window if isinstance(main_window, QWidget) else table
        menu = QMenu(menu_parent)
        row  = item.row()
        col  = item.column()
        header_item = table.horizontalHeaderItem(col)
        column_name = header_item.text() if header_item else f"Column {col}"

        # Resolve entry
        entry      = None
        entry_type = ""
        img = getattr(main_window, 'current_img', None)
        if hasattr(main_window, 'get_current_file_from_active_tab'):
            file_object, file_type = main_window.get_current_file_from_active_tab()
            if file_type == 'IMG' and file_object and 0 <= row < len(file_object.entries):
                entry = file_object.entries[row]
        elif img and 0 <= row < len(img.entries):
            entry = img.entries[row]
        if entry:
            entry_type = entry.name.rsplit('.', 1)[-1].upper() if '.' in entry.name else ""

        has_selection = bool(table.selectedItems())

        # ── 1. FILE-TYPE SUBMENUS ─────────────────────────────────────
        if entry_type == 'DFF':
            from apps.methods.imgfactory_svg_icons import get_txd_workshop_icon
            dff_menu = menu.addMenu("DFF Model")
            dff_menu.setIcon(icons.info_icon(color=icon_color))
            a = QAction(icons.info_icon(color=icon_color), "Model Info", menu_parent)
            a.triggered.connect(lambda: show_dff_info(main_window, row))
            dff_menu.addAction(a)
            a = QAction(icons.search_icon(color=icon_color), "Texture List", menu_parent)
            a.triggered.connect(lambda: show_dff_texture_list(main_window, row))
            dff_menu.addAction(a)
            a = QAction("View in Model Workshop", menu_parent)
            a.setEnabled(False)
            dff_menu.addAction(a)
            menu.addSeparator()

        elif entry_type == 'TXD':
            from apps.methods.imgfactory_svg_icons import get_txd_workshop_icon
            txd_menu = menu.addMenu("TXD Textures")
            txd_menu.setIcon(get_txd_workshop_icon(color=icon_color))
            a = QAction(get_txd_workshop_icon(color=icon_color), "Open in TXD Workshop", menu_parent)
            if hasattr(main_window, 'open_txd_workshop_for_entry'):
                a.triggered.connect(lambda: main_window.open_txd_workshop_for_entry(row))
            else:
                a.setEnabled(False)
            txd_menu.addAction(a)
            a = QAction(icons.search_icon(color=icon_color), "View Textures", menu_parent)
            a.triggered.connect(lambda: view_txd_textures(main_window, row))
            txd_menu.addAction(a)
            menu.addSeparator()

        elif entry_type == 'COL':
            from apps.methods.imgfactory_svg_icons import get_col_workshop_icon
            col_menu = menu.addMenu("COL Collision")
            col_menu.setIcon(get_col_workshop_icon(color=icon_color))
            a = QAction(get_col_workshop_icon(color=icon_color), "Open in COL Workshop", menu_parent)
            a.triggered.connect(lambda: edit_col_from_table(main_window, row))
            col_menu.addAction(a)
            a = QAction(icons.info_icon(color=icon_color), "Analyze", menu_parent)
            a.triggered.connect(lambda: analyze_col_from_table(main_window, row))
            col_menu.addAction(a)
            menu.addSeparator()

        elif entry_type == 'IDE':
            from apps.methods.imgfactory_svg_icons import get_ide_editor_icon
            ide_menu = menu.addMenu("IDE Definitions")
            ide_menu.setIcon(get_ide_editor_icon(color=icon_color))
            a = QAction(icons.search_icon(color=icon_color), "View Definitions", menu_parent)
            a.triggered.connect(lambda: view_ide_definitions(main_window, row))
            ide_menu.addAction(a)
            a = QAction(icons.edit_icon(color=icon_color), "Edit File", menu_parent)
            a.triggered.connect(lambda: edit_ide_file(main_window, row))
            ide_menu.addAction(a)
            menu.addSeparator()

        # ── 2. EXTRACT / EXPORT ───────────────────────────────────────
        if has_selection and hasattr(main_window, 'extract_selected_files'):
            a = QAction(icons.open_icon(color=icon_color), "Extract Selected", menu_parent)
            a.triggered.connect(main_window.extract_selected_files)
            menu.addAction(a)

        if hasattr(main_window, 'extract_all_files'):
            a = QAction(icons.package_icon(color=icon_color), "Extract All", menu_parent)
            a.triggered.connect(main_window.extract_all_files)
            menu.addAction(a)

        if hasattr(main_window, 'export_selected'):
            a = QAction(icons.save_icon(color=icon_color), "Export", menu_parent)
            a.triggered.connect(main_window.export_selected)
            menu.addAction(a)

        menu.addSeparator()

        # ── 3. ENTRY OPERATIONS ───────────────────────────────────────
        if has_selection and hasattr(main_window, 'rename_entry'):
            a = QAction(icons.edit_icon(color=icon_color), "Rename", menu_parent)
            a.setShortcut("F2")
            a.triggered.connect(main_window.rename_entry)
            menu.addAction(a)

        if has_selection:
            if hasattr(main_window, 'remove_selected_function'):
                a = QAction(icons.trash_icon(color=icon_color), "Remove", menu_parent)
                a.setShortcut("Del")
                a.triggered.connect(main_window.remove_selected_function)
                menu.addAction(a)
            elif hasattr(main_window, 'remove_selected'):
                a = QAction(icons.trash_icon(color=icon_color), "Remove", menu_parent)
                a.setShortcut("Del")
                a.triggered.connect(main_window.remove_selected)
                menu.addAction(a)

        if has_selection and hasattr(main_window, '_move_entries_up'):
            a = QAction("Move Up", menu_parent)
            a.setShortcut("Ctrl+Up")
            a.triggered.connect(main_window._move_entries_up)
            menu.addAction(a)
            a = QAction("Move Down", menu_parent)
            a.setShortcut("Ctrl+Down")
            a.triggered.connect(main_window._move_entries_down)
            menu.addAction(a)

        if has_selection and hasattr(main_window, 'move_selected_file'):
            a = QAction(icons.folder_icon(color=icon_color), "Move to Folder", menu_parent)
            a.triggered.connect(main_window.move_selected_file)
            menu.addAction(a)

        menu.addSeparator()

        # ── 4. CLIPBOARD ─────────────────────────────────────────────
        copy_menu = menu.addMenu("Copy")
        copy_menu.setIcon(icons.copy_icon(color=icon_color))
        a = QAction(f"Cell ({column_name})", menu_parent)
        a.triggered.connect(lambda: copy_table_cell(main_window, row, col))
        copy_menu.addAction(a)
        a = QAction("Row", menu_parent)
        a.triggered.connect(lambda: copy_table_row(main_window, row))
        copy_menu.addAction(a)
        a = QAction("Name", menu_parent)
        a.triggered.connect(lambda: copy_entry_name(main_window, row))
        copy_menu.addAction(a)
        a = QAction("Info", menu_parent)
        a.triggered.connect(lambda: copy_entry_info(main_window, row))
        copy_menu.addAction(a)
        if len(table.selectedItems()) > 1:
            a = QAction(f"Selection ({len(table.selectedItems())} items)", menu_parent)
            a.triggered.connect(lambda: copy_table_selection(main_window))
            copy_menu.addAction(a)

        menu.addSeparator()

        # ── 5. TOOLS ─────────────────────────────────────────────────
        if has_selection and hasattr(main_window, 'analyze_selected_file'):
            a = QAction(icons.rw_scan_icon(color=icon_color), "Analyze File", menu_parent)
            a.triggered.connect(main_window.analyze_selected_file)
            menu.addAction(a)

        if has_selection and hasattr(main_window, 'show_hex_editor_selected'):
            a = QAction(icons.editer_icon(color=icon_color), "Hex Editor", menu_parent)
            a.triggered.connect(main_window.show_hex_editor_selected)
            menu.addAction(a)

        if has_selection and hasattr(main_window, 'toggle_pinned_entries'):
            from apps.methods.imgfactory_svg_icons import get_pin_icon
            a = QAction(get_pin_icon(color=icon_color), "Toggle Pin", menu_parent)
            a.triggered.connect(main_window.toggle_pinned_entries)
            menu.addAction(a)

        menu.exec(table.viewport().mapToGlobal(position))

    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error showing context menu: {e}")
        import traceback; traceback.print_exc()


def copy_table_cell(main_window, row: int, col: int): #vers 1
    """Copy single table cell to clipboard"""
    try:
        table = main_window.gui_layout.table
        item = table.item(row, col)
        
        if item:
            text = item.text()
            QApplication.clipboard().setText(text)
            
            header_item = table.horizontalHeaderItem(col)
            column_name = header_item.text() if header_item else f"Column {col}"
            main_window.log_message(f"Copied {column_name}: '{text}'")
        else:
            main_window.log_message("No data in selected cell")
            
    except Exception as e:
        main_window.log_message(f"Copy cell error: {str(e)}")

def copy_table_row(main_window, row: int): #vers 1
    """Copy entire table row to clipboard"""
    try:
        table = main_window.gui_layout.table
        
        row_data = []
        for col in range(table.columnCount()):
            item = table.item(row, col)
            if item:
                row_data.append(item.text())
            else:
                row_data.append("")
        
        text = "\t".join(row_data)
        QApplication.clipboard().setText(text)
        
        filename = row_data[0] if row_data else f"Row {row}"
        main_window.log_message(f"Copied row: {filename}")
        
    except Exception as e:
        main_window.log_message(f"Copy row error: {str(e)}")

def copy_table_row_as_lines(main_window, row: int): #vers 1
    """Copy entire table row to clipboard as separate lines (Issue #3 fix)"""
    try:
        table = main_window.gui_layout.table
        
        row_data = []
        for col in range(table.columnCount()):
            item = table.item(row, col)
            if item:
                row_data.append(item.text())
            else:
                row_data.append("")
        
        # Join with newlines instead of tabs (as separate lines)
        text = "\n".join(row_data)
        QApplication.clipboard().setText(text)
        
        filename = row_data[0] if row_data else f"Row {row}"
        main_window.log_message(f"Copied row as lines: {filename}")
        
    except Exception as e:
        main_window.log_message(f"Copy row as lines error: {str(e)}")

def copy_table_column_data(main_window, col: int): #vers 1
    """Copy entire column data to clipboard"""
    try:
        table = main_window.gui_layout.table
        
        header_item = table.horizontalHeaderItem(col)
        column_name = header_item.text() if header_item else f"Column {col}"
        
        column_data = []
        for row in range(table.rowCount()):
            item = table.item(row, col)
            if item:
                column_data.append(item.text())
            else:
                column_data.append("")
        
        text = "\n".join(column_data)
        QApplication.clipboard().setText(text)
        
        main_window.log_message(f"Copied column '{column_name}': {table.rowCount()} entries")
        
    except Exception as e:
        main_window.log_message(f"Copy column error: {str(e)}")

def copy_table_selection(main_window): #vers 1
    """Copy selected table items to clipboard"""
    try:
        table = main_window.gui_layout.table
        selected_items = table.selectedItems()
        
        if not selected_items:
            main_window.log_message("No items selected")
            return
        
        # Group by rows
        rows_data = {}
        for item in selected_items:
            row = item.row()
            col = item.column()
            if row not in rows_data:
                rows_data[row] = {}
            rows_data[row][col] = item.text()
        
        # Format as table
        lines = []
        for row in sorted(rows_data.keys()):
            row_data = rows_data[row]
            ordered_values = []
            for col in sorted(row_data.keys()):
                ordered_values.append(row_data[col])
            lines.append("\t".join(ordered_values))
        
        text = "\n".join(lines)
        QApplication.clipboard().setText(text)
        
        main_window.log_message(f"Copied selection: {len(selected_items)} items from {len(rows_data)} rows")
        
    except Exception as e:
        main_window.log_message(f"Copy selection error: {str(e)}")

def copy_selected_text_from_cell(main_window, row: int, col: int): #vers 1
    """Copy selected text from current cell to clipboard"""
    try:
        table = main_window.gui_layout.table
        item = table.item(row, col)
        
        if item:
            # Get the QTableWidgetItem
            cell_widget = table.cellWidget(row, col) if table.cellWidget(row, col) else None
            
            if cell_widget and hasattr(cell_widget, 'selectedText') and callable(getattr(cell_widget, 'selectedText')):
                # If there's a custom widget with selected text
                selected_text = cell_widget.selectedText()
            else:
                # For standard QTableWidgetItem, we need to handle text selection differently
                # Since standard QTableWidgetItem doesn't support partial text selection,
                # we'll just copy the full text of the cell
                selected_text = item.text()
                
                # However, if the user wants to copy only selected text, they would need
                # to select the text in an editable context. For read-only tables,
                # we'll just copy the whole cell content
                main_window.log_message(f"Note: Full cell content copied. Partial text selection not supported in read-only table.")
            
            if selected_text:
                from PyQt6.QtWidgets import QApplication
                QApplication.clipboard().setText(selected_text)
                main_window.log_message(f"Copied selected text: '{selected_text[:50]}{'...' if len(selected_text) > 50 else ''}'")
            else:
                # If no specific text was selected, copy the full cell content
                full_text = item.text()
                QApplication.clipboard().setText(full_text)
                main_window.log_message(f"Copied full cell content: '{full_text[:50]}{'...' if len(full_text) > 50 else ''}'")
        else:
            main_window.log_message("No data in selected cell")
            
    except Exception as e:
        main_window.log_message(f"Copy selected text error: {str(e)}")

def copy_filename_only(main_window, row: int): #vers 1
    """Copy filename without extension from first column"""
    try:
        table = main_window.gui_layout.table
        item = table.item(row, 0)
        
        if item:
            full_name = item.text()
            if '.' in full_name:
                filename_only = '.'.join(full_name.split('.')[:-1])
            else:
                filename_only = full_name
                
            QApplication.clipboard().setText(filename_only)
            main_window.log_message(f"Copied filename: '{filename_only}'")
        else:
            main_window.log_message("No filename found")
            
    except Exception as e:
        main_window.log_message(f"Copy filename error: {str(e)}")

def copy_file_summary(main_window, row: int): #vers 1
    """Copy formatted file information summary"""
    try:
        table = main_window.gui_layout.table
        
        # Get all column headers
        headers = []
        for col in range(table.columnCount()):
            header_item = table.horizontalHeaderItem(col)
            if header_item:
                headers.append(header_item.text())
            else:
                headers.append(f"Column_{col}")
        
        # Get row data
        row_data = []
        for col in range(table.columnCount()):
            item = table.item(row, col)
            if item:
                row_data.append(item.text())
            else:
                row_data.append("N/A")
        
        # Create formatted summary
        summary_lines = ["=== File Information ==="]
        for i, (header, data) in enumerate(zip(headers, row_data)):
            summary_lines.append(f"{header}: {data}")
        
        text = "\n".join(summary_lines)
        QApplication.clipboard().setText(text)
        
        filename = row_data[0] if row_data else "Unknown"
        main_window.log_message(f"Copied file summary for: {filename}")
        
    except Exception as e:
        main_window.log_message(f"Copy summary error: {str(e)}")

# FILE-TYPE SPECIFIC ACTIONS
def edit_col_from_table(main_window, row: int): #vers 1
    """Edit COL file from table row"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                
                # Check if COL editor is available
                if hasattr(main_window, 'open_col_editor'):
                    main_window.open_col_editor(entry)
                else:
                    main_window.log_message("COL editor not available")
    except Exception as e:
        main_window.log_message(f"COL edit error: {str(e)}")

def analyze_col_from_table(main_window, row: int): #vers 1
    """Analyze COL file from table row"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                
                # Check if COL analyzer is available
                if hasattr(main_window, 'analyze_col_file'):
                    main_window.analyze_col_file(entry)
                else:
                    main_window.log_message("COL analyzer not available")
    except Exception as e:
        main_window.log_message(f"COL analysis error: {str(e)}")

def edit_ide_file(main_window, row: int): #vers 1
    """Edit IDE file from table row"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                
                # Check if IDE editor is available
                if hasattr(main_window, 'open_ide_editor'):
                    main_window.open_ide_editor(entry)
                else:
                    main_window.log_message("IDE editor not available")
    except Exception as e:
        main_window.log_message(f"IDE edit error: {str(e)}")

def view_ide_definitions(main_window, row: int): #vers 1
    """View IDE definitions from table row"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                
                # Check if IDE viewer is available
                if hasattr(main_window, 'view_ide_definitions'):
                    main_window.view_ide_definitions(entry)
                else:
                    main_window.log_message("IDE viewer not available")
    except Exception as e:
        main_window.log_message(f"IDE view error: {str(e)}")

def show_dff_info(main_window, row: int): #vers 1
    """Show DFF model information"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                
                # Check if DFF info viewer is available
                if hasattr(main_window, 'show_dff_info'):
                    main_window.show_dff_info(entry)
                else:
                    main_window.log_message("DFF info viewer not available")
    except Exception as e:
        main_window.log_message(f"DFF info error: {str(e)}")

def view_txd_textures(main_window, row: int): #vers 1
    """View TXD textures from table row"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                
                # Check if TXD viewer is available
                if hasattr(main_window, 'view_txd_textures'):
                    main_window.view_txd_textures(entry)
                else:
                    main_window.log_message("TXD viewer not available")
    except Exception as e:
        main_window.log_message(f"TXD view error: {str(e)}")

# EXTRACTION SUPPORT
def get_selected_entries_for_extraction(main_window) -> List: #vers 1
    """Get currently selected entries for extraction"""
    try:
        entries = []
        
        if not hasattr(main_window.gui_layout, 'table') or not hasattr(main_window, 'current_img') or not main_window.current_img:
            return entries
        
        table = main_window.gui_layout.table
        
        # Get selected rows
        selected_rows = set()
        for item in table.selectedItems():
            selected_rows.add(item.row())
        
        # Get corresponding entries
        for row in selected_rows:
            if row < len(main_window.current_img.entries):
                entries.append(main_window.current_img.entries[row])
        
        return entries
        
    except Exception as e:
        main_window.log_message(f"Error getting selected entries: {str(e)}")
        return []

def integrate_right_click_actions(main_window): #vers 3
    """Main integration function - call this from imgfactory.py"""
    try:
        success = setup_table_context_menu(main_window)
        if success:
            main_window.log_message("Complete right-click actions integrated successfully")
            
            # Add convenience method to main window
            main_window.setup_table_right_click = lambda: setup_table_context_menu(main_window)
            
        return success
        
    except Exception as e:
        main_window.log_message(f"Right-click integration error: {str(e)}")
        return False

# Additional functions needed for context menu
def show_dff_texture_list(main_window, row): #vers 3
    """Extract DFF from IMG and show texture list dialog with TXD checker.
    Cross-references DAT Browser xref to find the IDE-declared TXD name.
    """
    try:
        img = getattr(main_window, 'current_img', None)
        if not img or not img.entries or not (0 <= row < len(img.entries)):
            return
        entry = img.entries[row]
        if not entry.name.lower().endswith('.dff'):
            return

        dff_data = img.read_entry_data(entry)
        if not dff_data:
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Could not read {entry.name}")
            return

        # Look up IDE-declared TXD name from DAT Browser xref
        ide_txd_name = None
        xref = getattr(main_window, 'xref', None)
        if xref and hasattr(xref, 'model_map'):
            stem = entry.name.rsplit('.', 1)[0].lower()
            ide_obj = xref.model_map.get(stem)
            if ide_obj and getattr(ide_obj, 'txd_name', None):
                ide_txd_name = ide_obj.txd_name

        from apps.gui.dff_texlist_dialog import show_dff_texlist_dialog
        show_dff_texlist_dialog(main_window, entry.name, dff_data,
                                ide_txd_name=ide_txd_name)

    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"DFF texture list error: {e}")


def show_dff_model_viewer(main_window, row):
    """Show DFF model in viewer - needed for context menu"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                entry_info = {
                    'name': entry.name,
                    'is_dff': entry.name.lower().endswith('.dff'),
                    'size': entry.size,
                    'offset': entry.offset
                }
                
                if entry_info['is_dff']:
                    # Import from comprehensive fix if available
                    try:
                        from apps.components.Img_Factory.comprehensive_fix import show_dff_model_viewer as dff_viewer_func
                        dff_viewer_func(main_window, row, entry_info)
                    except ImportError:
                        # Fallback implementation
                        from PyQt6.QtWidgets import QMessageBox
                        QMessageBox.information(main_window, "DFF Model Viewer", 
                                              f"DFF Model Viewer for: {entry.name}\n\n"
                                              f"Note: 3D model viewer functionality would be implemented here.\n"
                                              f"This would load and display the DFF model in a 3D viewport.")
                else:
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.warning(main_window, "DFF Model Viewer", 
                                      "Selected file is not a DFF file")
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error showing DFF model viewer: {str(e)}")


def get_selected_entry_info(main_window, row): #vers 1
    """Get information about selected entry"""
    try:
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            return None
        
        if row < 0 or row >= len(main_window.current_img.entries):
            return None
        
        entry = main_window.current_img.entries[row]
        return {
            'entry': entry,
            'name': entry.name,
            'is_col': entry.name.lower().endswith('.col'),
            'is_dff': entry.name.lower().endswith('.dff'),
            'is_txd': entry.name.lower().endswith('.txd'),
            'size': entry.size,
            'offset': entry.offset
        }
    except Exception as e:
        main_window.log_message(f"Error getting entry info: {str(e)}")
        return None


def edit_col_from_img_entry(main_window, row): #vers 2
    """Edit COL file from IMG entry - WORKING VERSION"""
    try:
        entry_info = get_selected_entry_info(main_window, row)
        if not entry_info or not entry_info['is_col']:
            main_window.log_message("Selected entry is not a COL file")
            return False
        
        entry = entry_info['entry']
        main_window.log_message(f"Opening COL editor for: {entry.name}")
        
        # Use methods from col_operations
        from apps.methods.col_operations import extract_col_from_img_entry, create_temporary_col_file, cleanup_temporary_file
        
        # Extract COL data
        extraction_result = extract_col_from_img_entry(main_window, row)
        if not extraction_result:
            main_window.log_message("Failed to extract COL data")
            return False
        
        col_data, entry_name = extraction_result
        
        # Create temporary COL file
        temp_path = create_temporary_col_file(col_data, entry_name)
        if not temp_path:
            main_window.log_message("Failed to create temporary COL file")
            return False
        
        try:
            # Import and open COL editor
            from apps.components.Col_Editor.col_editor import COLEditorDialog
            
            editor = COLEditorDialog(main_window)
            
            # Load the temporary COL file
            if editor.load_col_file(temp_path):
                editor.setWindowTitle(f"COL Editor - {entry.name}")
                editor.show()  # Use show() instead of exec() for non-modal
                main_window.log_message(f"COL editor opened for: {entry.name}")
                return True
            else:
                main_window.log_message("Failed to load COL file in editor")
                return False
                
        finally:
            # Clean up temporary file
            cleanup_temporary_file(temp_path)
        
    except ImportError:
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(main_window, "COL Editor", 
            "COL editor component not available. Please check components.Col_Editor.col_workshop.py")
        return False
    except Exception as e:
        main_window.log_message(f"Error editing COL file: {str(e)}")
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.critical(main_window, "Error", f"Failed to edit COL file: {str(e)}")
        return False


def view_col_collision(main_window, row): #vers 2
    """View COL collision - WORKING VERSION"""
    try:
        entry_info = get_selected_entry_info(main_window, row)
        if not entry_info or not entry_info['is_col']:
            main_window.log_message("Selected entry is not a COL file")
            return False
        
        entry = entry_info['entry']
        main_window.log_message(f"Viewing COL collision for: {entry.name}")
        
        # Use methods from col_operations
        from apps.methods.col_operations import extract_col_from_img_entry, get_col_basic_info
        
        # Extract COL data
        extraction_result = extract_col_from_img_entry(main_window, row)
        if not extraction_result:
            main_window.log_message("Failed to extract COL data")
            return False
        
        col_data, entry_name = extraction_result
        
        # Get basic info
        basic_info = get_col_basic_info(col_data)
        
        if 'error' in basic_info:
            main_window.log_message(f"COL analysis error: {basic_info['error']}")
            return False
        
        # Build info display
        from apps.methods.img_core_classes import format_file_size
        info_text = f"COL File: {entry.name}\\n"
        info_text += f"Size: {format_file_size(len(col_data))}\\n"
        info_text += f"Version: {basic_info.get('version', 'Unknown')}\\n"
        info_text += f"Models: {basic_info.get('model_count', 0)}\\n"
        info_text += f"Signature: {basic_info.get('signature', b'Unknown')}\\n"
        
        # Show info dialog
        from apps.gui.col_dialogs import show_col_info_dialog
        show_col_info_dialog(main_window, info_text, f"COL Collision Info - {entry.name}")
        
        main_window.log_message(f"COL collision viewed for: {entry.name}")
        return True
        
    except ImportError:
        main_window.log_message("COL operations not available")
        return False
    except Exception as e:
        main_window.log_message(f"Error viewing COL collision: {str(e)}")
        return False


def analyze_col_from_img_entry(main_window, row): #vers 2
    """Analyze COL file from IMG entry - WORKING VERSION"""
    try:
        entry_info = get_selected_entry_info(main_window, row)
        if not entry_info or not entry_info['is_col']:
            main_window.log_message("Selected entry is not a COL file")
            return False
        
        entry = entry_info['entry']
        main_window.log_message(f"Analyzing COL file: {entry.name}")
        
        # Use methods from col_operations
        from apps.methods.col_operations import extract_col_from_img_entry, validate_col_data, create_temporary_col_file, cleanup_temporary_file, get_col_detailed_analysis
        
        # Extract COL data
        extraction_result = extract_col_from_img_entry(main_window, row)
        if not extraction_result:
            main_window.log_message("Failed to extract COL data")
            return False
        
        col_data, entry_name = extraction_result
        
        # Validate COL data
        validation_result = validate_col_data(col_data)
        
        # Get detailed analysis if possible
        temp_path = create_temporary_col_file(col_data, entry_name)
        analysis_data = {}
        
        if temp_path:
            try:
                detailed_analysis = get_col_detailed_analysis(temp_path)
                if 'error' not in detailed_analysis:
                    analysis_data.update(detailed_analysis)
            finally:
                cleanup_temporary_file(temp_path)
        
        # Combine validation and analysis data
        final_analysis = {
            'size': len(col_data),
            **analysis_data,
            **validation_result
        }
        
        # Show analysis dialog
        from apps.gui.col_dialogs import show_col_analysis_dialog
        show_col_analysis_dialog(main_window, final_analysis, entry.name)
        
        main_window.log_message(f"COL analysis completed for: {entry.name}")
        return True
        
    except ImportError:
        main_window.log_message("COL analysis components not available")
        return False
    except Exception as e:
        main_window.log_message(f"Error analyzing COL file: {str(e)}")
        return False


def edit_col_collision(main_window, row): #vers 2
    """Edit COL collision - WORKING VERSION (alias for edit_col_from_img_entry)"""
    return edit_col_from_img_entry(main_window, row)


def edit_dff_model(main_window, row): #vers 1
    """Edit DFF model"""
    main_window.log_message(f"Edit DFF model from row {row} - not yet implemented")


def edit_txd_textures(main_window, row): #vers 1
    """Edit TXD textures"""
    main_window.log_message(f"Edit TXD textures from row {row} - not yet implemented")


def view_dff_model(main_window, row): #vers 1
    """View DFF model"""
    main_window.log_message(f"View DFF model from row {row} - not yet implemented")


def view_txd_textures(main_window, row): #vers 1
    """View TXD textures"""
    main_window.log_message(f"View TXD textures from row {row} - not yet implemented")


def replace_selected_entry(main_window, row): #vers 1
    """Replace selected entry"""
    main_window.log_message(f"Replace entry from row {row} - not yet implemented")


def show_entry_properties(main_window, row): #vers 1
    """Show entry properties"""
    entry_info = get_selected_entry_info(main_window, row)
    if entry_info:
        from apps.methods.img_core_classes import format_file_size
        props_text = f"Entry Properties:\\n\\n"
        props_text += f"Name: {entry_info['name']}\\n"
        props_text += f"Size: {format_file_size(entry_info['size'])}\\n"
        props_text += f"Offset: 0x{entry_info['offset']:08X}\\n"
        props_text += f"Type: {'COL' if entry_info['is_col'] else 'DFF' if entry_info['is_dff'] else 'TXD' if entry_info['is_txd'] else 'Other'}\\n"
        
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(main_window, "Entry Properties", props_text)
        main_window.log_message(f"Properties shown for: {entry_info['name']}")
    else:
        main_window.log_message(f"Unable to get properties for row {row}")


def move_file(main_window, row, entry_info):
    """
    Move selected file to a new location
    """
    try:
        # Get current entry
        entry = entry_info['entry']
        current_name = entry.name
        
        # Show dialog to select destination
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        dest_dir = QFileDialog.getExistingDirectory(
            main_window,
            "Select Destination Directory",
            ""
        )
        
        if dest_dir:
            # For IMG entries, we can't actually move files since they're inside the IMG
            # Instead, we can rename to change the path-like structure
            QMessageBox.information(main_window, "Move Operation", 
                                  f"Moving '{current_name}' to '{dest_dir}'\\n\\n"
                                  f"Note: In IMG files, entries are virtual and cannot be moved to different directories.\\n"
                                  f"You can rename the entry to reflect a new path structure if needed.")
            
    except Exception as e:
        main_window.log_message(f"Error moving file: {str(e)}")


def move_selected_file(main_window):
    """
    Move selected file (when no specific row selected)
    """
    try:
        # Get selected items from table
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            selected_items = table.selectedItems()
            if selected_items:
                row = selected_items[0].row()
                entry_info = get_selected_entry_info(main_window, row)
                if entry_info:
                    move_file(main_window, row, entry_info)
    except Exception as e:
        main_window.log_message(f"Error moving selected file: {str(e)}")


def analyze_file(main_window, row, entry_info):
    """
    Analyze selected file
    """
    try:
        entry = entry_info['entry']
        name = entry.name
        
        # Determine file type and perform appropriate analysis
        if entry_info['is_col']:
            # Use existing COL analysis functionality
            analyze_col_from_img_entry(main_window, row)
        elif entry_info['is_dff']:
            # DFF analysis
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(main_window, "DFF Analysis", 
                                  f"DFF Analysis for: {name}\\n\\n"
                                  f"Size: {entry.size} bytes\\n"
                                  f"Offset: 0x{entry.offset:08X}\\n"
                                  f"Type: DFF Model File")
        elif entry_info['is_txd']:
            # TXD analysis
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(main_window, "TXD Analysis", 
                                  f"TXD Analysis for: {name}\\n\\n"
                                  f"Size: {entry.size} bytes\\n"
                                  f"Offset: 0x{entry.offset:08X}\\n"
                                  f"Type: Texture Dictionary File")
        else:
            # Generic analysis
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(main_window, "File Analysis", 
                                  f"Analysis for: {name}\\n\\n"
                                  f"Size: {entry.size} bytes\\n"
                                  f"Offset: 0x{entry.offset:08X}\\n"
                                  f"Type: Generic IMG Entry")
        
    except Exception as e:
        main_window.log_message(f"Error analyzing file: {str(e)}")


def analyze_selected_file(main_window):
    """
    Analyze selected file (when no specific row selected)
    """
    try:
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            selected_items = table.selectedItems()
            if selected_items:
                row = selected_items[0].row()
                entry_info = get_selected_entry_info(main_window, row)
                if entry_info:
                    analyze_file(main_window, row, entry_info)
    except Exception as e:
        main_window.log_message(f"Error analyzing selected file: {str(e)}")


def show_hex_editor(main_window, row, entry_info):
    """
    Show hex editor for selected file
    """
    try:
        # Import the hex editor module
        from apps.components.Hex_Editor import show_hex_editor_for_entry
        
        # Use the new hex editor implementation
        show_hex_editor_for_entry(main_window, row, entry_info)
        
    except Exception as e:
        main_window.log_message(f"Error showing hex editor: {str(e)}")
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.critical(main_window, "Error", f"Could not open hex editor:\\n{str(e)}")


def show_hex_editor_selected(main_window):
    """
    Show hex editor for selected file (when no specific row selected)
    """
    try:
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            selected_items = table.selectedItems()
            if selected_items:
                row = selected_items[0].row()
                entry_info = get_selected_entry_info(main_window, row)
                if entry_info:
                    show_hex_editor(main_window, row, entry_info)
    except Exception as e:
        main_window.log_message(f"Error showing hex editor for selected: {str(e)}")


def copy_entry_name(main_window, row):
    """
    Copy entry name to clipboard
    """
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                from PyQt6.QtWidgets import QApplication
                clipboard = QApplication.clipboard()
                clipboard.setText(entry.name)
                main_window.log_message(f"Copied name: {entry.name}")
    except Exception as e:
        main_window.log_message(f"Error copying entry name: {str(e)}")


def copy_entry_info(main_window, row):
    """
    Copy entry info to clipboard
    """
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                info_text = f"Name: {entry.name}\\nSize: {entry.size}\\nOffset: 0x{entry.offset:08X}"
                from PyQt6.QtWidgets import QApplication
                clipboard = QApplication.clipboard()
                clipboard.setText(info_text)
                main_window.log_message(f"Copied info for: {entry.name}")
    except Exception as e:
        main_window.log_message(f"Error copying entry info: {str(e)}")


def get_selected_entry_info(main_window, row):
    """
    Get entry information for a given row
    """
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                return {
                    'entry': entry,
                    'name': entry.name,
                    'is_col': entry.name.lower().endswith('.col'),
                    'is_dff': entry.name.lower().endswith('.dff'),
                    'is_txd': entry.name.lower().endswith('.txd'),
                    'size': entry.size,
                    'offset': entry.offset
                }
        return None
    except Exception:
        return None


# Export main functions
__all__ = [
    'setup_table_context_menu',
    'show_context_menu', 
    'copy_table_cell',
    'copy_table_row',
    'copy_table_column_data',
    'copy_table_selection',
    'copy_selected_text_from_cell',
    'copy_filename_only',
    'copy_file_summary',
    'edit_col_from_table',
    'analyze_col_from_table',
    'edit_ide_file',
    'view_ide_definitions',
    'show_dff_info',
    'view_txd_textures',
    'get_selected_entries_for_extraction',
    'integrate_right_click_actions',
    'show_dff_texture_list',
    'show_dff_model_viewer',
    'get_selected_entry_info',
    'edit_col_from_img_entry',
    'view_col_collision',
    'analyze_col_from_img_entry',
    'edit_col_collision',
    'edit_dff_model',
    'edit_txd_textures',
    'view_dff_model',
    'view_txd_textures',
    'replace_selected_entry',
    'show_entry_properties',
    'move_file',
    'move_selected_file',
    'analyze_file',
    'analyze_selected_file',
    'show_hex_editor',
    'show_hex_editor_selected',
    'copy_entry_name',
    'copy_entry_info',
    'get_selected_entry_info'
]