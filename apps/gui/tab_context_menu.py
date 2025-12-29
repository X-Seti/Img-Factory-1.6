#this belongs in gui/tab_context_menu.py - Version: 1
# X-Seti - October22 2025 - IMG Factory 1.5 - Tab Context Menu

"""
Tab Context Menu - Right-click menu for IMG/COL tabs
Provides tab management operations: close, rename, navigate
"""

from PyQt6.QtWidgets import QMenu, QInputDialog, QWidget
from PyQt6.QtGui import QAction
from PyQt6.QtCore import Qt

##Methods list -
# close_other_tabs
# rename_tab
# setup_tab_context_menu
# show_tab_context_menu

def setup_tab_context_menu(main_window): #vers 1
    """Setup right-click context menu for tabs"""
    try:
        if not hasattr(main_window, 'main_tab_widget'):
            return False
        
        tab_bar = main_window.main_tab_widget.tabBar()
        tab_bar.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        tab_bar.customContextMenuRequested.connect(
            lambda pos: show_tab_context_menu(main_window, pos)
        )
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Tab context menu enabled")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error setting up tab context menu: {e}")
        return False

def show_tab_context_menu(main_window, position): #vers 1
    """Show context menu for tabs"""
    try:
        tab_bar = main_window.main_tab_widget.tabBar()
        tab_index = tab_bar.tabAt(position)
        
        if tab_index < 0:
            return
        
        menu = QMenu(tab_bar)
        
        # Close this tab
        close_action = QAction("Close Tab", menu)
        close_action.triggered.connect(lambda: main_window.close_tab(tab_index))
        menu.addAction(close_action)
        
        # Close other tabs (only if more than 1 tab)
        if main_window.main_tab_widget.count() > 1:
            close_others_action = QAction("Close Other Tabs", menu)
            close_others_action.triggered.connect(lambda: close_other_tabs(main_window, tab_index))
            menu.addAction(close_others_action)
        
        # Close all tabs
        close_all_action = QAction("Close All Tabs", menu)
        close_all_action.triggered.connect(main_window.close_all_tabs)
        menu.addAction(close_all_action)
        
        menu.addSeparator()
        
        # Rename tab
        rename_action = QAction("Rename Tab", menu)
        rename_action.triggered.connect(lambda: rename_tab(main_window, tab_index))
        menu.addAction(rename_action)
        
        menu.addSeparator()
        
        # Show full path
        tab_text = tab_bar.tabText(tab_index)
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if hasattr(main_window.current_img, 'file_path'):
                full_path_action = QAction(f"{main_window.current_img.file_path}", menu)
                full_path_action.setEnabled(False)
                menu.addAction(full_path_action)
        
        menu.exec(tab_bar.mapToGlobal(position))
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error showing tab context menu: {e}")
        print(f"Tab context menu error: {e}")

def close_other_tabs(main_window, keep_index): #vers 1
    """Close all tabs except the specified one"""
    try:
        tab_widget = main_window.main_tab_widget
        
        # Close tabs after keep_index first (in reverse)
        for i in range(tab_widget.count() - 1, keep_index, -1):
            if hasattr(main_window, 'close_tab'):
                main_window.close_tab(i)
            else:
                tab_widget.removeTab(i)
        
        # Close tabs before keep_index (in reverse)
        for i in range(keep_index - 1, -1, -1):
            if hasattr(main_window, 'close_tab'):
                main_window.close_tab(i)
            else:
                tab_widget.removeTab(i)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Closed other tabs")
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error closing other tabs: {e}")

def rename_tab(main_window, tab_index): #vers 1
    """Rename tab"""
    try:
        tab_widget = main_window.main_tab_widget
        current_name = tab_widget.tabText(tab_index)
        
        new_name, ok = QInputDialog.getText(
            main_window,
            "Rename Tab",
            "Enter new tab name:",
            text=current_name
        )
        
        if ok and new_name:
            tab_widget.setTabText(tab_index, new_name)
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Renamed tab to: {new_name}")
            
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error renaming tab: {e}")


# Export functions
__all__ = [
    'setup_tab_context_menu',
    'show_tab_context_menu',
    'close_other_tabs',
    'rename_tab'
]
