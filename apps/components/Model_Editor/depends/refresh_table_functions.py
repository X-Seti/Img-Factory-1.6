#this belongs in methods/refresh_table_functions.py - Version: 2
# X-Seti - September07 2025 - IMG Factory 1.5 - Refresh Table Functions FIXED

"""
Refresh Table Function - FIXED VERSION
Simple refresh functionality for IMG and COL tables that actually works
"""

##Methods list -
# refresh_table
# _refresh_img_table
# _refresh_col_table
# _clear_table
# integrate_refresh_table

def refresh_table(main_window): #vers 3
    """Refresh the current table - works for IMG or COL files - FIXED - TAB AWARE"""
    try:
        # Use tab-aware approach to get current file
        if hasattr(main_window, 'get_current_file_from_active_tab'):
            file_object, file_type = main_window.get_current_file_from_active_tab()
            if file_object and file_type == 'IMG':
                # IMG file is loaded - refresh IMG table
                return _refresh_img_table(main_window)
            elif file_object and file_type == 'COL':
                # COL file is loaded - refresh COL table
                return _refresh_col_table(main_window)
            else:
                # No file loaded - clear table
                return _clear_table(main_window)
        else:
            # Fallback to old method
            if hasattr(main_window, 'current_img') and main_window.current_img:
                # IMG file is loaded - refresh IMG table
                return _refresh_img_table(main_window)
            elif hasattr(main_window, 'current_col') and main_window.current_col:
                # COL file is loaded - refresh COL table
                return _refresh_col_table(main_window)
            else:
                # No file loaded - clear table
                return _clear_table(main_window)
            
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error refreshing table: {str(e)}")
        return False


def _refresh_img_table(main_window) -> bool: #vers 6
    """Refresh IMG table with current IMG data - PRESERVES SELECTION AND STATE"""
    try:
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Refreshing IMG table...")
        
        img_file = main_window.current_img
        if not img_file or not hasattr(img_file, 'entries'):
            if hasattr(main_window, 'log_message'):
                main_window.log_message("No IMG entries to refresh")
            return _clear_table(main_window)
        
        # Preserve current selection and scroll position
        preserved_selection = []
        current_scroll_pos = 0
        current_row = -1
        
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            # Store current selection
            selected_items = table.selectedItems()
            for item in selected_items:
                preserved_selection.append((item.row(), item.column()))
            
            # Store current scroll position
            current_scroll_pos = table.verticalScrollBar().value()
            current_row = table.currentRow()
        
        # Import RW detection functions
        try:
            from apps.methods.img_detection import detect_entry_file_type_and_version
            from apps.methods.rw_versions import get_rw_version_name
            rw_detection_available = True
        except ImportError:
            rw_detection_available = False
        
        # Direct table update with proper 8-column structure
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            
            if table and img_file.entries:
                from PyQt6.QtWidgets import QTableWidgetItem
                
                # Disable updates temporarily to prevent UI freezing
                table.setUpdatesEnabled(False)
                
                try:
                    # Clear and set row count
                    table.setRowCount(len(img_file.entries))
                    
                    # Set correct 8-column structure for IMG files
                    if table.columnCount() != 8:
                        table.setColumnCount(8)
                        headers = ["Name", "Type", "Size", "Offset", "RW Address", "RW Version", "Compression", "Status"]
                        table.setHorizontalHeaderLabels(headers)
                        
                        # Set column widths to match populate_img_table
                        table.setColumnWidth(0, 190)  # Name
                        table.setColumnWidth(1, 60)   # Type
                        table.setColumnWidth(2, 90)   # Size
                        table.setColumnWidth(3, 100)  # Offset
                        table.setColumnWidth(4, 100)  # RW Address
                        table.setColumnWidth(5, 100)  # RW Version
                        table.setColumnWidth(6, 110)  # Compression
                        table.setColumnWidth(7, 110)  # Status
                    
                    # Populate rows with correct 8-column data
                    for row, entry in enumerate(img_file.entries):
                        try:
                            # Column 0: Name
                            name = str(getattr(entry, 'name', f'entry_{row}')).strip()
                            table.setItem(row, 0, QTableWidgetItem(name))
                            
                            # Column 1: Type (file extension)
                            if '.' in name:
                                file_ext = name.split('.')[-1].upper()
                            else:
                                file_ext = 'UNK'
                            table.setItem(row, 1, QTableWidgetItem(file_ext))
                            
                            # Column 2: Size
                            size = getattr(entry, 'size', 0)
                            if size > 1024 * 1024:
                                size_text = f"{size / (1024 * 1024):.1f} MB"
                            elif size > 1024:
                                size_text = f"{size / 1024:.1f} KB"
                            else:
                                size_text = f"{size} bytes"
                            table.setItem(row, 2, QTableWidgetItem(size_text))
                            
                            # Column 3: Offset
                            offset = getattr(entry, 'offset', 0)
                            offset_text = f"0x{offset:08X}"
                            table.setItem(row, 3, QTableWidgetItem(offset_text))
                            
                            # Column 4: RW Address (for RW files)
                            rw_address_text = "N/A"
                            if file_ext in ['DFF', 'TXD']:
                                # For RW files, this could be the section address
                                if hasattr(entry, 'rw_section_type'):
                                    rw_address_text = f"0x{entry.rw_section_type:08X}"
                                else:
                                    rw_address_text = "0x00000000"
                            table.setItem(row, 4, QTableWidgetItem(rw_address_text))
                            
                            # Column 5: RW Version (THE IMPORTANT ONE)
                            rw_version_text = "N/A"
                            if rw_detection_available and file_ext in ['DFF', 'TXD']:
                                # Try to detect RW version
                                try:
                                    if detect_entry_file_type_and_version(entry, img_file):
                                        if hasattr(entry, 'rw_version_name') and entry.rw_version_name:
                                            rw_version_text = entry.rw_version_name
                                        elif hasattr(entry, 'rw_version') and entry.rw_version > 0:
                                            rw_version_text = get_rw_version_name(entry.rw_version)
                                except Exception:
                                    # If detection fails, try to get existing value
                                    if hasattr(entry, 'rw_version_name') and entry.rw_version_name:
                                        rw_version_text = entry.rw_version_name
                                    elif hasattr(entry, 'rw_version') and entry.rw_version > 0:
                                        rw_version_text = get_rw_version_name(entry.rw_version)
                            table.setItem(row, 5, QTableWidgetItem(rw_version_text))
                            
                            # Column 6: Compression
                            compression_text = "None"
                            if hasattr(entry, 'compression_type'):
                                if entry.compression_type and str(entry.compression_type) != "NONE":
                                    compression_text = str(entry.compression_type)
                            table.setItem(row, 6, QTableWidgetItem(compression_text))
                            
                            # Column 7: Status
                            status_text = "Ready"
                            if hasattr(entry, 'is_new_entry') and entry.is_new_entry:
                                status_text = "New"
                            elif hasattr(entry, 'is_replaced') and entry.is_replaced:
                                status_text = "Modified"
                            table.setItem(row, 7, QTableWidgetItem(status_text))
                            
                        except Exception as row_error:
                            # Create safe fallback row on any error
                            table.setItem(row, 0, QTableWidgetItem(f"Entry_{row}"))
                            table.setItem(row, 1, QTableWidgetItem("ERR"))
                            table.setItem(row, 2, QTableWidgetItem("0 B"))
                            table.setItem(row, 3, QTableWidgetItem("0x00000000"))
                            table.setItem(row, 4, QTableWidgetItem("Error"))
                            table.setItem(row, 5, QTableWidgetItem("Error"))
                            table.setItem(row, 6, QTableWidgetItem("Error"))
                            table.setItem(row, 7, QTableWidgetItem("Error"))
                            continue
                
                finally:
                    # Re-enable updates
                    table.setUpdatesEnabled(True)
                    table.update()
                
                # Restore selection after refresh
                if preserved_selection and table.rowCount() > 0:
                    table.clearSelection()
                    for row, col in preserved_selection:
                        if row < table.rowCount() and col < table.columnCount():
                            table.selectRow(row)  # Select the entire row
                
                # Restore scroll position and current row
                if current_scroll_pos > 0:
                    table.verticalScrollBar().setValue(current_scroll_pos)
                if current_row >= 0 and current_row < table.rowCount():
                    table.setCurrentCell(current_row, 0)
                
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"IMG table refreshed with 8 columns + RW detection - {len(img_file.entries)} entries")
                return True
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Could not refresh IMG table - no table widget found")
        return False
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error refreshing IMG table: {str(e)}")
        return False


def _refresh_col_table(main_window) -> bool: #vers 3
    """Refresh COL table with current COL data - PRESERVES SELECTION AND STATE"""
    try:
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Refreshing COL table...")
        
        col_file = main_window.current_col
        if not col_file:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("No COL file to refresh")
            return _clear_table(main_window)
        
        # Preserve current selection and scroll position
        preserved_selection = []
        current_scroll_pos = 0
        current_row = -1
        
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            # Store current selection
            selected_items = table.selectedItems()
            for item in selected_items:
                preserved_selection.append((item.row(), item.column()))
            
            # Store current scroll position
            current_scroll_pos = table.verticalScrollBar().value()
            current_row = table.currentRow()
        
        # Method 1: Use existing COL refresh method if available
        if hasattr(main_window, 'refresh_col_table') and callable(main_window.refresh_col_table):
            try:
                main_window.refresh_col_table()
                if hasattr(main_window, 'log_message'):
                    main_window.log_message("COL table refreshed via existing method")
                
                # Restore selection after refresh
                if preserved_selection and table.rowCount() > 0:
                    table.clearSelection()
                    for row, col in preserved_selection:
                        if row < table.rowCount() and col < table.columnCount():
                            table.selectRow(row)  # Select the entire row
                
                # Restore scroll position and current row
                if current_scroll_pos > 0:
                    table.verticalScrollBar().setValue(current_scroll_pos)
                if current_row >= 0 and current_row < table.rowCount():
                    table.setCurrentCell(current_row, 0)
                
                return True
            except Exception as e:
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"Existing COL refresh failed: {str(e)}")
        
        # Method 2: Manual COL table refresh
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            
            if table and (hasattr(col_file, 'models') or hasattr(col_file, 'entries')):
                from PyQt6.QtWidgets import QTableWidgetItem
                
                # Disable updates temporarily to prevent UI freezing
                table.setUpdatesEnabled(False)
                
                try:
                    # Get COL data
                    if hasattr(col_file, 'models') and col_file.models:
                        items = col_file.models
                        table.setRowCount(len(items))
                        
                        # Ensure we have the right number of columns for COL
                        if table.columnCount() < 3:
                            table.setColumnCount(3)
                            headers = ["Name", "Type", "Info"]
                            table.setHorizontalHeaderLabels(headers)
                        
                        for row, model in enumerate(items):
                            try:
                                # Name
                                name = getattr(model, 'name', f'model_{row}')
                                table.setItem(row, 0, QTableWidgetItem(str(name)))
                                
                                # Type
                                table.setItem(row, 1, QTableWidgetItem('COL'))
                                
                                # Info
                                spheres_count = len(getattr(model, 'spheres', []))
                                boxes_count = len(getattr(model, 'boxes', []))
                                faces_count = len(getattr(model, 'faces', []))
                                info = f"Spheres: {spheres_count}, Boxes: {boxes_count}, Faces: {faces_count}"
                                table.setItem(row, 2, QTableWidgetItem(info))
                                
                            except Exception as row_error:
                                if hasattr(main_window, 'log_message'):
                                    main_window.log_message(f"Error refreshing COL row {row}: {str(row_error)}")
                                continue
                        
                        # Resize columns to content
                        table.resizeColumnsToContents()
                        
                        if hasattr(main_window, 'log_message'):
                            main_window.log_message(f"COL table refreshed - {len(items)} models")
                        return True
                    
                    elif hasattr(col_file, 'entries') and col_file.entries:
                        # Handle COL files with entries instead of models
                        entries = col_file.entries
                        table.setRowCount(len(entries))
                        
                        for row, entry in enumerate(entries):
                            try:
                                name = getattr(entry, 'name', f'entry_{row}')
                                table.setItem(row, 0, QTableWidgetItem(str(name)))
                                table.setItem(row, 1, QTableWidgetItem('COL'))
                                
                                size = getattr(entry, 'size', 0)
                                size_text = f"{size} bytes" if size > 0 else "Unknown"
                                table.setItem(row, 2, QTableWidgetItem(size_text))
                                
                            except Exception as row_error:
                                if hasattr(main_window, 'log_message'):
                                    main_window.log_message(f"Error refreshing COL entry row {row}: {str(row_error)}")
                                continue
                        
                        table.resizeColumnsToContents()
                        
                        if hasattr(main_window, 'log_message'):
                            main_window.log_message(f"COL table refreshed - {len(entries)} entries")
                        return True
                finally:
                    # Re-enable updates
                    table.setUpdatesEnabled(True)
                    table.update()
                
                # Restore selection after refresh
                if preserved_selection and table.rowCount() > 0:
                    table.clearSelection()
                    for row, col in preserved_selection:
                        if row < table.rowCount() and col < table.columnCount():
                            table.selectRow(row)  # Select the entire row
                
                # Restore scroll position and current row
                if current_scroll_pos > 0:
                    table.verticalScrollBar().setValue(current_scroll_pos)
                if current_row >= 0 and current_row < table.rowCount():
                    table.setCurrentCell(current_row, 0)
                
                return True
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Could not refresh COL table - no available methods")
        return False
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error refreshing COL table: {str(e)}")
        return False


def _clear_table(main_window) -> bool: #vers 2
    """Clear the table when no file is loaded - FIXED"""
    try:
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            if table:
                table.setRowCount(0)
                table.clearContents()
                
                if hasattr(main_window, 'log_message'):
                    main_window.log_message("Table cleared - no file loaded")
                return True
        
        # Also try to clear other possible table references
        if hasattr(main_window, 'entries_table'):
            table = main_window.entries_table
            if table:
                table.setRowCount(0)
                table.clearContents()
                return True
                
        return False
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error clearing table: {str(e)}")
        return False


def integrate_refresh_table(main_window): #vers 3
    """Integrate refresh table function into main window - FIXED"""
    try:
        # Add refresh_table method to main window with multiple aliases
        main_window.refresh_table = lambda: refresh_table(main_window)
        main_window.update_list = lambda: refresh_table(main_window)  # For "Update List" button
        main_window.refresh_entries = lambda: refresh_table(main_window)
        main_window.update_table = lambda: refresh_table(main_window)
        
        # Also add methods for file list updates (for remove operations)
        main_window.refresh_file_list = lambda: refresh_table(main_window)
        main_window.update_file_list = lambda: refresh_table(main_window)
        main_window.refresh_current_tab_data = lambda: refresh_table(main_window)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Fixed refresh table function integrated with all aliases")
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error integrating refresh table: {str(e)}")
        return False


# Export functions
__all__ = [
    'refresh_table',
    'integrate_refresh_table'
]
