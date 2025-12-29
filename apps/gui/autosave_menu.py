#this belongs in gui/autosave_menu.py - Version: 1
# X-Seti - August07 2025 - IMG Factory 1.5 - Auto-Save Menu Option

"""
Auto-Save Menu Option - Add to IMG menu
Allows users to toggle auto-save on/off for import operations
"""

from PyQt6.QtWidgets import QMessageBox
from PyQt6.QtGui import QAction
from PyQt6.QtCore import Qt

def add_autosave_menu_option(main_window): #vers 1
    """Add auto-save toggle option to IMG menu"""
    try:
        # Find IMG menu in menubar
        menubar = main_window.menuBar()
        img_menu = None
        
        for action in menubar.actions():
            if action.text() == "IMG" or action.text() == "&IMG":
                img_menu = action.menu()
                break
        
        if not img_menu:
            # Create IMG menu if it doesn't exist
            img_menu = menubar.addMenu("&IMG")
        
        # Add separator before auto-save option
        img_menu.addSeparator()
        
        # Create auto-save toggle action
        autosave_action = QAction("Auto-Save After Import", main_window)
        autosave_action.setCheckable(True)
        autosave_action.setChecked(False)  # Default: OFF (to prevent crashes)
        autosave_action.setStatusTip("Automatically save IMG file after importing files")
        
        # Connect to toggle function
        autosave_action.triggered.connect(lambda checked: toggle_autosave(main_window, checked))
        
        # Add to menu
        img_menu.addAction(autosave_action)
        
        # Store reference
        main_window.autosave_action = autosave_action
        main_window.autosave_enabled = False
        
        main_window.log_message("✅ Auto-save menu option added to IMG menu")
        return True
        
    except Exception as e:
        main_window.log_message(f"❌ Failed to add auto-save menu option: {str(e)}")
        return False

def toggle_autosave(main_window, enabled): #vers 1
    """Toggle auto-save setting"""
    try:
        main_window.autosave_enabled = enabled
        
        if enabled:
            # Warn user about potential crashes
            reply = QMessageBox.question(
                main_window,
                "Enable Auto-Save?",
                "⚠️ WARNING: Auto-save may cause crashes during import.\n\n"
                "It's recommended to keep auto-save disabled and use the 'Save Entry' button manually after importing.\n\n"
                "Enable auto-save anyway?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.No
            )
            
            if reply == QMessageBox.StandardButton.No:
                # User chose not to enable - revert the setting
                main_window.autosave_action.setChecked(False)
                main_window.autosave_enabled = False
                main_window.log_message("❌ Auto-save disabled (user choice)")
                return
            
            main_window.log_message("⚠️ Auto-save ENABLED - may cause crashes!")
        else:
            main_window.log_message("✅ Auto-save disabled - manual save required")
        
    except Exception as e:
        main_window.log_message(f"❌ Error toggling auto-save: {str(e)}")

def is_autosave_enabled(main_window) -> bool: #vers 1
    """Check if auto-save is enabled"""
    return getattr(main_window, 'autosave_enabled', False)

def integrate_autosave_menu(main_window): #vers 1
    """Integrate auto-save menu into main window"""
    try:
        success = add_autosave_menu_option(main_window)
        if success:
            main_window.log_message("✅ Auto-save menu integrated")
        return success
        
    except Exception as e:
        main_window.log_message(f"❌ Auto-save menu integration failed: {str(e)}")
        return False

# Export functions
__all__ = [
    'add_autosave_menu_option',
    'toggle_autosave', 
    'is_autosave_enabled',
    'integrate_autosave_menu'
]