#this belongs in gui/tab_context_menu.py - Version: 1
# X-Seti - October22 2025 - IMG Factory 1.5 - Tab Context Menu

"""
Tab Context Menu - Right-click menu for IMG/COL tabs
Provides tab management operations: close, rename, navigate
"""

from PyQt6.QtWidgets import QMenu, QInputDialog, QWidget
from PyQt6.QtGui import QFont
try:
    from PyQt6.QtGui import QAction
except ImportError:
    from PyQt6.QtWidgets import QAction
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
        
        from apps.methods.tab_system import close_tab as _close_tab

        # Close this tab
        close_action = QAction("Close Tab", menu)
        close_action.triggered.connect(lambda: _close_tab(main_window, tab_index))
        menu.addAction(close_action)

        # Close other tabs (only if more than 1 tab)
        if main_window.main_tab_widget.count() > 1:
            close_others_action = QAction("Close Other Tabs", menu)
            close_others_action.triggered.connect(
                lambda: close_other_tabs(main_window, tab_index))
            menu.addAction(close_others_action)

        # Close all tabs
        close_all_action = QAction("Close All Tabs", menu)
        if hasattr(main_window, 'close_all_tabs'):
            close_all_action.triggered.connect(main_window.close_all_tabs)
        else:
            from apps.core.close import close_all_tabs as _close_all
            close_all_action.triggered.connect(lambda: _close_all(main_window))
        menu.addAction(close_all_action)
        
        menu.addSeparator()
        
        # Rename tab
        rename_action = QAction("Rename Tab", menu)
        rename_action.triggered.connect(lambda: rename_tab(main_window, tab_index))
        menu.addAction(rename_action)
        
        # ── Type-specific actions ────────────────────────────────
        tab_widget = main_window.main_tab_widget.widget(tab_index)
        file_type  = getattr(tab_widget, 'file_type', None)
        file_path  = getattr(tab_widget, 'file_path', None)

        if file_type == 'COL' and file_path:
            menu.addSeparator()
            send_col_action = QAction("Send to Col Workshop", menu)
            def _send_to_col_workshop(fp=file_path):
                try:
                    # Switch to existing Col Workshop tab for this file if open
                    tw = main_window.main_tab_widget
                    for i in range(tw.count()):
                        w = tw.widget(i)
                        from apps.components.Col_Editor.col_workshop import COLWorkshop
                        workshops = w.findChildren(COLWorkshop) if w else []
                        if workshops:
                            wk = workshops[0]
                            if getattr(wk, 'current_file_path', None) == fp:
                                tw.setCurrentIndex(i)
                                return
                    # Otherwise open a new Col Workshop tab
                    from apps.components.Col_Editor.col_workshop import open_col_workshop
                    open_col_workshop(main_window, fp)
                except Exception as e:
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message(f"Col Workshop error: {e}")
            send_col_action.triggered.connect(_send_to_col_workshop)
            menu.addAction(send_col_action)

        menu.addSeparator()

        # Show full path
        tab_text = tab_bar.tabText(tab_index)
        if file_path:
            full_path_action = QAction(file_path, menu)
            full_path_action.setEnabled(False)
            menu.addAction(full_path_action)
        elif hasattr(main_window, 'current_img') and main_window.current_img:
            if hasattr(main_window.current_img, 'file_path'):
                full_path_action = QAction(main_window.current_img.file_path, menu)
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
        from apps.methods.tab_system import close_tab as _close_tab
        for i in range(tab_widget.count() - 1, keep_index, -1):
            _close_tab(main_window, i)

        # Close tabs before keep_index (in reverse)
        for i in range(keep_index - 1, -1, -1):
            _close_tab(main_window, i)
        
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
