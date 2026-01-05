#this belongs in components/File_Editor/file_dirtree_backend.py - Version: 3
# X-Seti - January03 2025 - IMG Factory 1.6 - File Browser Backend Classes

"""
FILE BROWSER BACKEND CLASSES
Contains dialog classes and helper functions for the file browser
Separated from main file to keep under size limits
NO EMOJIS - Uses SVG icons from imgfactory_svg_icons.py
"""

import os
import datetime
from typing import Dict, List, Optional
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QFormLayout, QTabWidget,
    QWidget, QLabel, QPushButton, QCheckBox, QSpinBox, QComboBox,
    QTextBrowser, QListWidget, QMessageBox, QGroupBox, QLineEdit
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont, QIcon

# SVG Icons
from apps.methods.imgfactory_svg_icons import (
    get_folder_icon, get_file_icon, get_img_file_icon,
    get_txd_file_icon, get_col_file_icon, get_edit_icon,
    get_view_icon, get_settings_icon, get_image_icon
)

##Methods list -
# format_file_size_backend
# get_file_attributes_backend
# get_file_type_display_backend
# get_file_type_icon_backend
# get_folder_size_quick_backend

##Classes -
# BrowserSettingsDialog
# FilePropertiesDialog
# FileSearchDialog

class BrowserSettingsDialog(QDialog):
    """Browser settings dialog"""
    
    def __init__(self, current_settings, parent=None): #vers 1
        super().__init__(parent)
        self.setWindowTitle("Browser Settings")
        self.setModal(True)
        self.setFixedSize(400, 500)
        self.current_settings = current_settings.copy()
        self.setup_ui()
        
    def setup_ui(self): #vers 1
        """Setup settings dialog UI"""
        layout = QVBoxLayout(self)
        
        # Tab widget for different setting categories
        tabs = QTabWidget()
        
        # General tab
        general_tab = QWidget()
        general_layout = QFormLayout(general_tab)
        
        self.show_hidden_check = QCheckBox()
        self.show_hidden_check.setChecked(self.current_settings.get('show_hidden', False))
        general_layout.addRow("Show hidden files:", self.show_hidden_check)
        
        self.show_extensions_check = QCheckBox()
        self.show_extensions_check.setChecked(self.current_settings.get('show_extensions', True))
        general_layout.addRow("Show file extensions:", self.show_extensions_check)
        
        tabs.addTab(general_tab, "General")
        
        # View tab
        view_tab = QWidget()
        view_layout = QFormLayout(view_tab)
        
        self.view_mode_combo = QComboBox()
        self.view_mode_combo.addItems(["Tree", "List", "Grid"])
        view_layout.addRow("View mode:", self.view_mode_combo)
        
        self.font_size_spin = QSpinBox()
        self.font_size_spin.setRange(8, 16)
        self.font_size_spin.setValue(self.current_settings.get('font_size', 11))
        view_layout.addRow("Font size:", self.font_size_spin)
        
        tabs.addTab(view_tab, "View")
        
        layout.addWidget(tabs)
        
        # Buttons
        button_layout = QHBoxLayout()
        
        ok_btn = QPushButton("OK")
        ok_btn.clicked.connect(self.accept)
        button_layout.addWidget(ok_btn)
        
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)
        
        layout.addLayout(button_layout)
        
    def get_settings(self) -> dict: #vers 1
        """Get current settings from dialog"""
        return {
            'show_hidden': self.show_hidden_check.isChecked(),
            'show_extensions': self.show_extensions_check.isChecked(),
            'view_mode': self.view_mode_combo.currentText().lower(),
            'font_size': self.font_size_spin.value()
        }


class FilePropertiesDialog(QDialog):
    """File properties dialog"""
    
    def __init__(self, file_path: str, parent=None): #vers 1
        super().__init__(parent)
        self.file_path = file_path
        self.setWindowTitle("File Properties")
        self.setModal(True)
        self.setFixedSize(450, 400)
        self.setup_ui()
        
    def setup_ui(self): #vers 1
        """Setup properties dialog UI"""
        layout = QVBoxLayout(self)
        
        # File info
        info_group = QGroupBox("File Information")
        info_layout = QFormLayout(info_group)
        
        try:
            stats = os.stat(self.file_path)
            
            info_layout.addRow("Name:", QLabel(os.path.basename(self.file_path)))
            info_layout.addRow("Path:", QLabel(os.path.dirname(self.file_path)))
            info_layout.addRow("Size:", QLabel(format_file_size_backend(stats.st_size)))
            info_layout.addRow("Type:", QLabel(get_file_type_display_backend(
                os.path.splitext(self.file_path)[1])))
            
            modified_time = datetime.datetime.fromtimestamp(stats.st_mtime)
            info_layout.addRow("Modified:", QLabel(modified_time.strftime("%Y-%m-%d %H:%M:%S")))
            
            info_layout.addRow("Attributes:", QLabel(get_file_attributes_backend(self.file_path)))
            
        except Exception as e:
            info_layout.addRow("Error:", QLabel(str(e)))
        
        layout.addWidget(info_group)
        
        # Close button
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        layout.addWidget(close_btn)


class FileSearchDialog(QDialog):
    """File search dialog"""
    
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.setWindowTitle("Search Files")
        self.setModal(True)
        self.setFixedSize(500, 400)
        self.setup_ui()
        
    def setup_ui(self): #vers 1
        """Setup search dialog UI"""
        layout = QVBoxLayout(self)
        
        # Search input
        search_layout = QHBoxLayout()
        search_layout.addWidget(QLabel("Search for:"))
        
        self.search_input = QLineEdit()
        search_layout.addWidget(self.search_input)
        
        search_btn = QPushButton("Search")
        search_btn.clicked.connect(self.perform_search)
        search_layout.addWidget(search_btn)
        
        layout.addLayout(search_layout)
        
        # Results list
        self.results_list = QListWidget()
        layout.addWidget(self.results_list)
        
        # Close button
        button_layout = QHBoxLayout()
        
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        button_layout.addWidget(close_btn)
        
        layout.addLayout(button_layout)
        
    def perform_search(self): #vers 1
        """Perform file search"""
        self.results_list.addItem("Search functionality not yet implemented")


# Helper functions moved from main file
def format_file_size_backend(size: int) -> str: #vers 1
    """Format file size for display with better precision"""
    if size == 0:
        return "0 B"
    
    for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
        if size < 1024:
            if unit == 'B':
                return f"{size:,} {unit}"
            else:
                return f"{size:.1f} {unit}"
        size /= 1024
    return f"{size:.1f} PB"


def get_file_type_icon_backend(file_ext: str) -> QIcon: #vers 2
    """Get SVG icon for file type - Returns QIcon, not emoji string"""
    icon_map = {
        '.img': get_img_file_icon,
        '.dir': get_file_icon,
        '.ide': get_edit_icon,
        '.ipl': get_view_icon,
        '.dat': get_file_icon,
        '.dff': get_image_icon,
        '.txd': get_txd_file_icon,
        '.col': get_col_file_icon,
        '.cfg': get_settings_icon,
        '.txt': get_edit_icon,
        '.py': get_edit_icon,
        '.json': get_view_icon,
        '.log': get_view_icon
    }
    icon_func = icon_map.get(file_ext, get_file_icon)
    return icon_func()


def get_file_type_display_backend(file_ext: str) -> str: #vers 1
    """Get display name for file type"""
    type_map = {
        '.img': 'IMG Archive',
        '.dir': 'Directory File',
        '.ide': 'Item Definition',
        '.ipl': 'Item Placement',
        '.dat': 'Data File',
        '.dff': '3D Model',
        '.txd': 'Texture Dictionary',
        '.col': 'Collision File',
        '.cfg': 'Configuration',
        '.txt': 'Text File',
        '.py': 'Python Script',
        '.json': 'JSON Data',
        '.log': 'Log File'
    }
    return type_map.get(file_ext, 'Unknown File')


def get_file_attributes_backend(file_path: str) -> str: #vers 1
    """Get file attributes string"""
    try:
        import stat
        attrs = []
        st = os.stat(file_path)
        
        # Check permissions
        if st.st_mode & stat.S_IRUSR:
            attrs.append('R')
        if st.st_mode & stat.S_IWUSR:
            attrs.append('W')
        if st.st_mode & stat.S_IXUSR:
            attrs.append('X')
            
        # Check if hidden (starts with .)
        if os.path.basename(file_path).startswith('.'):
            attrs.append('H')
            
        return ''.join(attrs) if attrs else '-'
    except:
        return ''


def get_folder_size_quick_backend(folder_path: str) -> int: #vers 1
    """Get folder size quickly (just immediate files, not recursive)"""
    try:
        total_size = 0
        for item in os.listdir(folder_path):
            item_path = os.path.join(folder_path, item)
            if os.path.isfile(item_path):
                total_size += os.path.getsize(item_path)
        return total_size
    except (PermissionError, OSError):
        return 0


__all__ = [
    'BrowserSettingsDialog',
    'FilePropertiesDialog', 
    'FileSearchDialog',
    'format_file_size_backend',
    'get_file_type_icon_backend',
    'get_file_type_display_backend',
    'get_file_attributes_backend',
    'get_folder_size_quick_backend'
]
