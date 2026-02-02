#this belongs in core/file_dirtree_backend.py - Version: 2
# X-Seti - August14 2025 - IMG Factory 1.5 - File Browser Backend Classes

"""
FILE BROWSER BACKEND CLASSES
Contains dialog classes and helper functions for the file browser
Separated from main file to keep under size limits
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
from PyQt6.QtGui import QFont

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
    
    def __init__(self, current_settings, parent=None):
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
        
        self.font_size_spin = QSpinBox()
        self.font_size_spin.setRange(8, 24)
        self.font_size_spin.setValue(self.current_settings.get('font_size', 13))
        general_layout.addRow("Font size:", self.font_size_spin)
        
        self.max_depth_spin = QSpinBox()
        self.max_depth_spin.setRange(1, 10)
        self.max_depth_spin.setValue(self.current_settings.get('max_depth', 4))
        general_layout.addRow("Tree depth:", self.max_depth_spin)
        
        tabs.addTab(general_tab, "General")
        
        # View tab
        view_tab = QWidget()
        view_layout = QFormLayout(view_tab)
        
        self.view_mode_combo = QComboBox()
        self.view_mode_combo.addItems(["tree", "list", "details"])
        self.view_mode_combo.setCurrentText(self.current_settings.get('view_mode', 'tree'))
        view_layout.addRow("Default view mode:", self.view_mode_combo)
        
        self.sort_mode_combo = QComboBox()
        self.sort_mode_combo.addItems(["name", "size", "date", "type"])
        self.sort_mode_combo.setCurrentText(self.current_settings.get('sort_mode', 'name'))
        view_layout.addRow("Default sort mode:", self.sort_mode_combo)
        
        self.auto_expand_check = QCheckBox()
        self.auto_expand_check.setChecked(self.current_settings.get('auto_expand', True))
        view_layout.addRow("Auto-expand first level:", self.auto_expand_check)
        
        tabs.addTab(view_tab, "View")
        
        layout.addWidget(tabs)
        
        # Dialog buttons
        button_layout = QHBoxLayout()
        
        ok_btn = QPushButton("OK")
        ok_btn.clicked.connect(self.accept)
        button_layout.addWidget(ok_btn)
        
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)
        
        layout.addLayout(button_layout)
        
    def get_settings(self): #vers 1
        """Get updated settings"""
        return {
            'show_hidden': self.show_hidden_check.isChecked(),
            'show_extensions': self.show_extensions_check.isChecked(),
            'font_size': self.font_size_spin.value(),
            'view_mode': self.view_mode_combo.currentText(),
            'sort_mode': self.sort_mode_combo.currentText(),
            'max_depth': self.max_depth_spin.value(),
            'auto_expand': self.auto_expand_check.isChecked(),
        }


class FilePropertiesDialog(QDialog):
    """File properties dialog"""
    
    def __init__(self, file_path, parent=None):
        super().__init__(parent)
        self.file_path = file_path
        self.setWindowTitle(f"Properties - {os.path.basename(file_path)}")
        self.setModal(True)
        self.setFixedSize(350, 400)
        self.setup_ui()
        
    def setup_ui(self): #vers 1
        """Setup properties dialog UI"""
        layout = QVBoxLayout(self)
        
        # File icon and name
        header_layout = QHBoxLayout()
        
        # Icon (placeholder)
        icon_label = QLabel("📄")
        icon_label.setFont(QFont("", 24))
        header_layout.addWidget(icon_label)
        
        # File name
        name_label = QLabel(os.path.basename(self.file_path))
        name_label.setFont(QFont("", 12, QFont.Weight.Bold))
        header_layout.addWidget(name_label)
        
        header_layout.addStretch()
        layout.addLayout(header_layout)
        
        # Properties
        props_group = QGroupBox("Properties")
        props_layout = QFormLayout(props_group)
        
        # Basic info
        props_layout.addRow("Location:", QLabel(os.path.dirname(self.file_path)))
        
        if os.path.isfile(self.file_path):
            size = os.path.getsize(self.file_path)
            props_layout.addRow("Size:", QLabel(format_file_size_backend(size)))
            
        # Dates
        if os.path.exists(self.file_path):
            mod_time = os.path.getmtime(self.file_path)
            mod_date = datetime.datetime.fromtimestamp(mod_time)
            props_layout.addRow("Modified:", QLabel(mod_date.strftime('%Y-%m-%d %H:%M:%S')))
            
        layout.addWidget(props_group)
        
        layout.addStretch()
        
        # Close button
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        layout.addWidget(close_btn)

class FileSearchDialog(QDialog):
    """File search dialog"""
    
    def __init__(self, search_path, parent=None):
        super().__init__(parent)
        self.search_path = search_path
        self.setWindowTitle("Search Files")
        self.setModal(True)
        self.setFixedSize(500, 400)
        self.setup_ui()
        
    def setup_ui(self): #vers 1
        """Setup search dialog UI"""
        layout = QVBoxLayout(self)
        
        # Search criteria
        criteria_group = QGroupBox("Search Criteria")
        criteria_layout = QFormLayout(criteria_group)
        
        self.name_edit = QLineEdit()
        self.name_edit.setPlaceholderText("*.txt, *.py, etc.")
        criteria_layout.addRow("File name:", self.name_edit)
        
        self.content_edit = QLineEdit()
        self.content_edit.setPlaceholderText("Text to search for")
        criteria_layout.addRow("Content:", self.content_edit)
        
        layout.addWidget(criteria_group)
        
        # Search button
        search_btn = QPushButton("🔍 Search")
        search_btn.clicked.connect(self.perform_search)
        layout.addWidget(search_btn)
        
        # Results
        results_group = QGroupBox("Search Results")
        results_layout = QVBoxLayout(results_group)
        
        self.results_list = QListWidget()
        results_layout.addWidget(self.results_list)
        
        layout.addWidget(results_group)
        
        # Dialog buttons
        button_layout = QHBoxLayout()
        
        ok_btn = QPushButton("OK")
        ok_btn.clicked.connect(self.accept)
        button_layout.addWidget(ok_btn)
        
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)
        
        layout.addLayout(button_layout)
        
    def perform_search(self): #vers 1
        """Perform file search"""
        # Implementation would search for files
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


def get_file_type_icon_backend(file_ext: str) -> str: #vers 1
    """Get icon for file type"""
    icon_map = {
        '.img': '💽',
        '.dir': '📋',
        '.ide': '📝',
        '.ipl': '📍',
        '.dat': '📄',
        '.dff': '🎭',
        '.txd': '🖼️',
        '.col': '🛡️',
        '.cfg': '⚙️',
        '.txt': '📝',
        '.py': '🐍',
        '.json': '📊',
        '.log': '📊'
    }
    return icon_map.get(file_ext, '📄')


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