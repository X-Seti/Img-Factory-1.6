#this belongs in core/close.py - Version: 11
# X-Seti - September27 2025 - IMG Factory 1.5 - Close Functions Only

"""
IMG Factory Close Functions
ONLY handles closing and clearing - NO tab creation
"""

import os
from PyQt6.QtWidgets import QWidget, QVBoxLayout

##Class IMGCloseManager -
# __init__
# close_current_file
# close_all_tabs
# close_tab
# _clear_current_tab
# _clear_all_tables_in_tab

##Functions -
# close_all_img
# close_img_file
# setup_close_manager
# install_close_functions

def close_all_img(main_window): #vers 3
    """Close all IMG files - Wrapper for close_all_tabs"""
    try:
        if hasattr(main_window, 'close_manager') and main_window.close_manager:
            main_window.close_manager.close_all_tabs()
        else:
            main_window.log_message("Close manager not available")
    except Exception as e:
        main_window.log_message(f"Error in close_all_img: {str(e)}")

def close_img_file(main_window): #vers 6
    """Close current file (IMG or COL or TXD)"""
    try:
        if hasattr(main_window, 'close_manager') and main_window.close_manager:
            main_window.close_manager.close_current_file()
        else:
            main_window.log_message("Close manager not available")
    except Exception as e:
        main_window.log_message(f"Error in close_img_file: {str(e)}")

def setup_close_manager(main_window): #vers 4
    """Setup close manager for main window"""
    main_window.close_manager = IMGCloseManager(main_window)
    return main_window.close_manager

def install_close_functions(main_window): #vers 4
    """Install close functions as methods on main window"""
    close_manager = setup_close_manager(main_window)

    main_window.close_img_file = lambda: close_img_file(main_window)
    main_window.close_all_img = lambda: close_all_img(main_window)
    main_window.close_all_tabs = close_manager.close_all_tabs
    main_window._clear_current_tab = close_manager._clear_current_tab

    main_window.log_message("Close functions installed")
    return close_manager

class IMGCloseManager:
    """Manages ONLY close operations - NO tab creation"""

    def __init__(self, main_window): #vers 1
        """Initialize with reference to main window"""
        self.main_window = main_window
        self.log_message = main_window.log_message

    def close_current_file(self): #vers 3
        """Close current file (IMG/COL/TXD)"""
        try:
            current_index = self.main_window.main_tab_widget.currentIndex()

            if self.main_window.main_tab_widget.count() > 1:
                if hasattr(self.main_window, 'close_tab'):
                    self.main_window.close_tab(current_index)
                else:
                    self.close_tab(current_index)
            else:
                self._clear_current_tab()

        except Exception as e:
            self.log_message(f"Error closing file: {str(e)}")

    def close_all_tabs(self): #vers 5
        """Close all tabs"""
        try:
            tab_count = self.main_window.main_tab_widget.count()
            self.log_message(f"Closing all {tab_count} tabs")

            for i in range(tab_count - 1, -1, -1):
                if self.main_window.main_tab_widget.count() > 1:
                    if hasattr(self.main_window, 'close_tab'):
                        self.main_window.close_tab(i)
                    else:
                        self.close_tab(i)
                else:
                    self._clear_current_tab()
                    break

            # If no tabs left, let tab_functions.py handle creating one
            if self.main_window.main_tab_widget.count() == 0:
                self.log_message("All tabs closed - tab system will create new one")

            # Clear references
            self.main_window.current_img = None
            if hasattr(self.main_window, 'current_col'):
                self.main_window.current_col = None
            if hasattr(self.main_window, 'current_txd'):
                self.main_window.current_txd = None
            self.main_window._update_ui_for_no_img()

            self.log_message("All tabs closed")

        except Exception as e:
            self.log_message(f"Error closing all tabs: {str(e)}")

    def close_tab(self, index): #vers 6
        """Close tab at index"""
        if self.main_window.main_tab_widget.count() <= 1:
            self._clear_current_tab()
            return

        try:
            tab_widget = self.main_window.main_tab_widget.widget(index)
            tab_name = self.main_window.main_tab_widget.tabText(index)

            if tab_widget:
                self._clear_all_tables_in_tab(tab_widget)

            # Remove tab - this triggers currentChanged signal automatically
            self.main_window.main_tab_widget.removeTab(index)
            self.log_message(f"Closed tab: {tab_name}")

            # DON'T manually call update_references - let the signal do it
            # The currentChanged signal will trigger switch_tab which calls update_references

        except Exception as e:
            self.log_message(f"Error closing tab {index}: {str(e)}")

    def _clear_current_tab(self): #vers 2
        """Clear current tab contents"""
        try:
            current_index = self.main_window.main_tab_widget.currentIndex()

            if hasattr(self.main_window, 'clear_tab'):
                self.main_window.clear_tab(current_index)
                return

            tab_widget = self.main_window.main_tab_widget.widget(current_index)
            if tab_widget:
                tab_widget.file_object = None
                tab_widget.file_type = 'NONE'
                tab_widget.file_path = None

            self.main_window.current_img = None
            if hasattr(self.main_window, 'current_col'):
                self.main_window.current_col = None
            if hasattr(self.main_window, 'current_txd'):
                self.main_window.current_txd = None

            self.main_window.main_tab_widget.setTabText(current_index, "No File")
            self.main_window._update_ui_for_no_img()

            self.log_message("Current tab cleared")

        except Exception as e:
            self.log_message(f"Error clearing current tab: {str(e)}")

    def _clear_all_tables_in_tab(self, tab_widget): #vers 1
        """Clear all table data in a tab widget"""
        try:
            from PyQt6.QtWidgets import QTableWidget
            tables = tab_widget.findChildren(QTableWidget)
            for table in tables:
                table.clear()
                table.setRowCount(0)
        except Exception as e:
            self.log_message(f"Error clearing tables: {str(e)}")


__all__ = [
    'IMGCloseManager',
    'close_img_file',
    'close_all_img',
    'setup_close_manager',
    'install_close_functions'
]
