#this belongs in core/col_viewer_integration.py - Version: 3
# X-Seti - October22 2025 - IMG Factory 1.5 - COL Viewer Integration

"""
COL Viewer Integration - Connects new clean COL viewer to IMG Factory
Adds "View COL (3D)" option to right-click context menu
"""

import os
import tempfile
from PyQt6.QtWidgets import QDialog, QVBoxLayout, QMessageBox
from PyQt6.QtCore import Qt

##Methods list -
# add_view_col_3d_to_context_menu
# cleanup_temp_col_file
# extract_col_to_temp
# integrate_col_viewer
# open_col_viewer_dialog

def extract_col_to_temp(main_window, entry) -> str: #vers 1
    """Extract COL entry to temporary file"""
    try:
        # Create temp file
        temp_dir = tempfile.gettempdir()
        temp_path = os.path.join(temp_dir, f"imgfactory_{entry.name}")
        
        # Extract data
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            return None
        
        img = main_window.current_img
        
        # Read entry data
        with open(img.file_path, 'rb') as f:
            f.seek(entry.offset * 2048)
            data = f.read(entry.size * 2048)
        
        # Write to temp file
        with open(temp_path, 'wb') as f:
            f.write(data)
        
        return temp_path
        
    except Exception as e:
        main_window.log_message(f"Failed to extract COL: {str(e)}")
        return None

def cleanup_temp_col_file(temp_path: str): #vers 1
    """Clean up temporary COL file"""
    try:
        if temp_path and os.path.exists(temp_path):
            os.remove(temp_path)
    except:
        pass

def open_col_viewer_dialog(main_window, row: int) -> bool: #vers 1
    """Open COL viewer dialog for selected entry"""
    try:
        # Get entry info
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            main_window.log_message("No IMG file loaded")
            return False
        
        if row < 0 or row >= len(main_window.current_img.entries):
            main_window.log_message("Invalid entry selection")
            return False
        
        entry = main_window.current_img.entries[row]
        
        # Check if COL file
        if not entry.name.lower().endswith('.col'):
            main_window.log_message("Selected file is not a COL file")
            return False
        
        main_window.log_message(f"Opening 3D viewer for: {entry.name}")
        
        # Extract to temp file
        temp_path = extract_col_to_temp(main_window, entry)
        if not temp_path:
            return False
        
        try:
            # Import COL viewer
            from apps.components.col_viewer.col_viewer import COLViewerWidget
            
            # Create dialog
            dialog = QDialog(main_window)
            dialog.setWindowTitle(f"COL Viewer - {entry.name}")
            dialog.setMinimumSize(800, 600)
            dialog.resize(1000, 700)
            
            # Setup layout
            layout = QVBoxLayout(dialog)
            layout.setContentsMargins(0, 0, 0, 0)
            
            # Create viewer widget
            viewer = COLViewerWidget(dialog)
            viewer.main_window = main_window
            layout.addWidget(viewer)
            
            # Apply theme
            viewer.apply_theme()
            
            # Load COL file
            if not viewer.load_col_file(temp_path):
                QMessageBox.warning(
                    dialog,
                    "COL Viewer",
                    f"Failed to load COL file: {entry.name}\n\nThe file may be corrupted or in an unsupported format."
                )
                cleanup_temp_col_file(temp_path)
                return False
            
            # Show dialog
            dialog.exec()
            
            main_window.log_message(f"COL viewer closed: {entry.name}")
            
        finally:
            # Cleanup temp file
            cleanup_temp_col_file(temp_path)
        
        return True
        
    except ImportError as e:
        QMessageBox.critical(
            main_window,
            "COL Viewer Error",
            f"COL Viewer component not found!\n\nError: {str(e)}\n\nMake sure components/col_viewer/ exists."
        )
        main_window.log_message(f"COL viewer import failed: {str(e)}")
        return False
        
    except Exception as e:
        QMessageBox.critical(
            main_window,
            "COL Viewer Error",
            f"Failed to open COL viewer:\n\n{str(e)}"
        )
        main_window.log_message(f"COL viewer error: {str(e)}")
        return False

def add_view_col_3d_to_context_menu(main_window) -> bool: #vers 1
    """Add 'View COL (3D)' option to right-click context menu"""
    try:
        # Patch the context_menu_event function
        from gui import gui_context
        
        # Store original function
        if not hasattr(gui_context, '_original_context_menu'):
            gui_context._original_context_menu = gui_context.context_menu_event
        
        def context_menu_with_viewer(main_window, event): #vers 1
            """Enhanced context menu with COL 3D viewer option"""
            try:
                if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
                    return
                
                table = main_window.gui_layout.table
                item = table.itemAt(event.pos())
                if not item:
                    return
                
                row = item.row()
                
                # Get entry info
                from gui.gui_context import get_selected_entry_info
                entry_info = get_selected_entry_info(main_window, row)
                if not entry_info:
                    return
                
                # Create context menu
                from PyQt6.QtWidgets import QMenu
                from PyQt6.QtGui import QAction
                menu = QMenu(table)
                
                # Add COL 3D viewer option FIRST if COL file
                if entry_info['is_col']:
                    view_3d_action = QAction("View COL (3D)", table)
                    view_3d_action.triggered.connect(lambda: open_col_viewer_dialog(main_window, row))
                    menu.addAction(view_3d_action)
                    menu.addSeparator()
                
                # Call original context menu function to add other options
                gui_context._original_context_menu(main_window, event)
                
            except Exception as e:
                main_window.log_message(f"Context menu error: {str(e)}")
        
        # Replace function
        gui_context.context_menu_event = context_menu_with_viewer
        
        main_window.log_message("COL 3D viewer added to context menu")
        return True
        
    except Exception as e:
        main_window.log_message(f"Failed to add COL viewer to context menu: {str(e)}")
        return False

def integrate_col_viewer(main_window) -> bool: #vers 1
    """Main integration function - call this from imgfactory.py"""
    try:
        # Add to context menu
        if not add_view_col_3d_to_context_menu(main_window):
            return False
        
        # Add convenience method to main window
        main_window.open_col_3d_viewer = lambda row: open_col_viewer_dialog(main_window, row)
        
        main_window.log_message("COL 3D viewer integration complete")
        return True
        
    except Exception as e:
        main_window.log_message(f"COL viewer integration failed: {str(e)}")
        return False


# Export functions
__all__ = [
    'integrate_col_viewer',
    'open_col_viewer_dialog',
    'add_view_col_3d_to_context_menu'
]
