#this belongs in gui/gui_backend.py - Version: 22
# X-Seti - August07 2025 - IMG Factory 1.5 - Clean GUI Backend - No Fallbacks

"""
Clean GUI Backend - Working functions only, no fallback code
Button display management and action handling for IMG Factory 1.5
"""

from enum import Enum
from PyQt6.QtWidgets import QPushButton, QWidget
from PyQt6.QtCore import Qt, pyqtSignal
from PyQt6.QtGui import QFont, QIcon
from typing import Optional, Dict, Any, List, Callable

##Methods list -
# create_adaptive_button
# handle_action  
# log_message
# set_button_display_mode

##Classes -
# ButtonDisplayMode
# GUIBackend

class ButtonDisplayMode(Enum):
    """Button display modes"""
    TEXT_ONLY = 1
    ICONS_ONLY = 2
    ICONS_WITH_TEXT = 3

class GUIBackend:
    """Clean GUI Backend - working functions only"""
    
    def __init__(self, main_window): #vers 1
        """Initialize GUI backend with main window reference"""
        self.main_window = main_window
        self.button_display_mode = ButtonDisplayMode.ICONS_WITH_TEXT
        self.img_buttons = []
        self.entry_buttons = []
        self.options_buttons = []

    def create_adaptive_button(self, label, action_type=None, icon=None, color=None, bold=False): #vers 1
        """Create adaptive button that works with current display mode"""
        btn = QPushButton(label)
        
        # Store button data for mode switching
        btn.full_text = label
        btn.short_text = self._get_short_text(label)
        btn.action_type = action_type
        btn.icon_name = icon
        
        # Apply styling
        if color:
            btn.setStyleSheet(f"background-color: {color};")
        
        if bold:
            font = btn.font()
            font.setBold(True)
            btn.setFont(font)
        
        # Set initial display based on current mode
        self._update_button_display(btn)
        
        return btn
    
    def set_button_display_mode(self, mode): #vers 1
        """Set button display mode and update all buttons"""
        if mode not in [ButtonDisplayMode.TEXT_ONLY, ButtonDisplayMode.ICONS_ONLY, ButtonDisplayMode.ICONS_WITH_TEXT]:
            return False
        
        self.button_display_mode = mode
        self._update_all_button_displays()
        return True
    
    def _update_all_button_displays(self): #vers 1
        """Update display for all buttons based on current mode"""
        all_buttons = self.img_buttons + self.entry_buttons + self.options_buttons
        
        for btn in all_buttons:
            self._update_button_display(btn)
    
    def _update_button_display(self, btn): #vers 1
        """Update individual button display based on mode"""
        if not hasattr(btn, 'full_text') or not hasattr(btn, 'short_text'):
            return
        
        if self.button_display_mode == ButtonDisplayMode.TEXT_ONLY:
            btn.setText(btn.short_text)
            btn.setIcon(QIcon())  # Remove icon
        elif self.button_display_mode == ButtonDisplayMode.ICONS_ONLY:
            btn.setText("")
            if hasattr(btn, 'icon_name'):
                btn.setIcon(QIcon.fromTheme(btn.icon_name))
        else:  # ICONS_WITH_TEXT
            btn.setText(btn.full_text)
            if hasattr(btn, 'icon_name'):
                btn.setIcon(QIcon.fromTheme(btn.icon_name))


    def _open_col_editor(self): #vers 1
        """Open COL editor using working implementation"""
        try:
            from gui.gui_context import open_col_editor_dialog
            return open_col_editor_dialog(self.main_window)
        except Exception as e:
            self.main_window.log_message(f"❌ COL editor error: {str(e)}")

    def _show_table_context_menu(self, position): #vers 1
        """Show context menu for table"""
        try:
            if hasattr(self.main_window, 'table'):
                # Get current file info
                current_type = self._get_current_file_type()
                current_file = self._get_current_file()

                if current_file and hasattr(self.main_window, 'create_file_info_popup'):
                    menu = self.main_window.create_file_info_popup(current_type, current_file.get('path', ''), current_file.get('object'))
                    menu.exec(self.main_window.table.mapToGlobal(position))
        except Exception as e:
            self.log_message(f"❌ Error showing context menu: {str(e)}")

    def _get_current_file_type(self): #vers 1
        """Get currently selected file type"""
        try:
            if hasattr(self.main_window, 'main_type_tabs'):
                index = self.main_window.main_type_tabs.currentIndex()
                return ["IMG", "COL", "TXD"][index] if 0 <= index < 3 else "IMG"
            return "IMG"
        except Exception:
            return "IMG"

    def _get_current_file(self): #vers 1
        """Get current file information"""
        try:
            if hasattr(self.main_window, 'current_img') and self.main_window.current_img:
                return {
                    'path': getattr(self.main_window.current_img, 'file_path', 'Unknown'),
                    'object': self.main_window.current_img
                }
            return None
        except Exception:
            return None

    def create_button(self, label, action_type=None, icon=None, bold=False): #vers 1
        """Legacy method for compatibility"""
        return self.create_adaptive_button(label, action_type, icon, None, bold)

# Export classes
__all__ = [
    'GUIBackend',
    'ButtonDisplayMode'
]
