#this belongs in methods/update_ui_for_loaded_img.py - Version: 6
# X-Seti - August13 2025 - IMG Factory 1.5 - UI Update for Loaded IMG - VISIBILITY FIXED

"""
IMG Factory UI Update for Loaded IMG
Standalone method to update UI when IMG file is loaded - FIXED VERSION WITH VISIBILITY
"""

import os
from PyQt6.QtWidgets import QTableWidgetItem

##Methods list -
# update_ui_for_loaded_img
# integrate_update_ui_for_loaded_img

def update_ui_for_loaded_img(main_window): #vers 6
    """Update UI when IMG file is loaded - FIXED VERSION WITH VISIBILITY"""
    try:
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("update_ui_for_loaded_img called but no current_img")
            return False

        # Update window title
        file_name = os.path.basename(main_window.current_img.file_path)
        main_window.setWindowTitle(f"IMG Factory 1.5 - {file_name}")

        # Populate table with IMG entries using STANDALONE method
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            # Import and use the standalone function from apps.methods.
            from apps.methods.populate_img_table import populate_img_table

            # Setup IMG table structure first
            table = main_window.gui_layout.table
            table.setColumnCount(6)
            table.setHorizontalHeaderLabels([
                "Name", "Type", "Size", "Offset", "RW Version", "Info"
            ])
            # Set IMG column widths
            table.setColumnWidth(0, 200)  # Name
            table.setColumnWidth(1, 80)   # Type
            table.setColumnWidth(2, 100)  # Size
            table.setColumnWidth(3, 100)  # Offset
            table.setColumnWidth(4, 120)  # RW Version
            table.setColumnWidth(5, 150)  # Info

            # Clear table before populating (preserve headers)
            table.setRowCount(0)

            # Populate table
            populate_img_table(main_window.gui_layout.table, main_window.current_img)
            
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Table populated with {len(main_window.current_img.entries)} entries")
        else:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("GUI layout or table not available")

        # Update status - FIXED: Properly hide progress and update status
        if hasattr(main_window, 'gui_layout'):
            entry_count = len(main_window.current_img.entries) if main_window.current_img.entries else 0
            
            # CRITICAL: Hide progress bar completely - multiple methods
            if hasattr(main_window.gui_layout, 'hide_progress'):
                main_window.gui_layout.hide_progress()
            
            if hasattr(main_window.gui_layout, 'progress_bar') and main_window.gui_layout.progress_bar is not None:
                main_window.gui_layout.progress_bar.setVisible(False)
                main_window.gui_layout.progress_bar.setValue(0)
            
            # Update IMG info if method exists
            if hasattr(main_window.gui_layout, 'update_img_info'):
                main_window.gui_layout.update_img_info(f"IMG: {file_name}")

        # VISIBILITY FIXES - Force all GUI components to be visible after loading
        if hasattr(main_window, 'gui_layout'):
            # Force main splitter visibility
            if hasattr(main_window.gui_layout, 'main_splitter'):
                main_window.gui_layout.main_splitter.setVisible(True)
                main_window.gui_layout.main_splitter.show()
            
            # Force left vertical splitter visibility  
            if hasattr(main_window.gui_layout, 'left_vertical_splitter'):
                main_window.gui_layout.left_vertical_splitter.setVisible(True)
                main_window.gui_layout.left_vertical_splitter.show()
            
            # Force tab widget visibility (CRITICAL - this was often hidden)
            if hasattr(main_window.gui_layout, 'tab_widget'):
                tab_widget = main_window.gui_layout.tab_widget
                tab_widget.setVisible(True)
                tab_widget.show()
                
                # Make sure current tab is visible
                current_tab = tab_widget.currentWidget()
                if current_tab:
                    current_tab.setVisible(True)
                    current_tab.show()
            
            # Force table visibility and all its parents
            if hasattr(main_window.gui_layout, 'table'):
                table = main_window.gui_layout.table
                table.setVisible(True)
                table.show()
                
                # Force all table parents to be visible
                parent = table.parent()
                level = 0
                while parent and level < 5:
                    parent.setVisible(True)
                    parent.show()
                    parent = parent.parent()
                    level += 1
            
            # Force log widget visibility
            if hasattr(main_window.gui_layout, 'log'):
                main_window.gui_layout.log.setVisible(True)
                main_window.gui_layout.log.show()
        
        # Force central widget visibility
        central_widget = main_window.centralWidget()
        if central_widget:
            central_widget.setVisible(True)
            central_widget.show()
        
        # Force main window visibility
        main_window.setVisible(True)
        main_window.show()
        
        # Force repaint everything
        main_window.repaint()
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'main_splitter'):
            main_window.gui_layout.main_splitter.repaint()

        if hasattr(main_window, 'log_message'):
            main_window.log_message("IMG UI updated successfully with visibility fix")
        
        return True

    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error updating UI for loaded IMG: {str(e)}")
        
        # Even if there's an error, try basic visibility fix
        try:
            if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
                main_window.gui_layout.table.setVisible(True)
                main_window.gui_layout.table.show()
            main_window.setVisible(True)
            main_window.show()
        except:
            pass
        
        return False

def integrate_update_ui_for_loaded_img(main_window): #vers 2
    """Integrate the standalone update_ui_for_loaded_img method into main window"""
    try:
        # Add method to main window
        main_window._update_ui_for_loaded_img = lambda: update_ui_for_loaded_img(main_window)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Enhanced update_ui_for_loaded_img integrated with visibility fix")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error integrating update_ui_for_loaded_img: {str(e)}")
        return False

# Export functions
__all__ = [
    'update_ui_for_loaded_img',
    'integrate_update_ui_for_loaded_img'
]
