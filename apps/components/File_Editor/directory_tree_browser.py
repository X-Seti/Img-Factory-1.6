#this belongs in gui/directory_tree_browser.py - Version: 1
# X-Seti - January07 2026 - IMG Factory 1.6 - Complete Directory Tree Browser

"""
COMPLETE DIRECTORY TREE BROWSER
Single unified file browser with full functionality
NO fallback code - works or doesn't work
"""

import os
import shutil
import subprocess
import platform
import fnmatch
from typing import Dict, List, Optional
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QTreeWidget, QTreeWidgetItem,
    QMenuBar, QMenu, QToolBar, QPushButton, QLineEdit, QLabel, QMessageBox,
    QSplitter, QTextEdit, QGroupBox, QInputDialog, QDialog, QFormLayout,
    QCheckBox, QListWidget, QFileDialog
)
from PyQt6.QtCore import Qt, pyqtSignal, QSettings
from PyQt6.QtGui import QAction, QFont

# SVG Icons
from apps.methods.imgfactory_svg_icons import (
    get_folder_icon, get_file_icon, get_img_file_icon,
    get_txd_file_icon, get_col_file_icon, get_refresh_icon,
    get_close_icon, get_view_icon, get_edit_icon, get_image_icon,
    get_copy_icon, get_paste_icon, get_cut_icon, get_rename_icon,
    get_back_icon, get_forward_icon, get_up_icon, get_home_icon,
    get_search_icon, get_properties_icon, get_new_folder_icon,
    get_trash_icon, get_settings_icon
)

##Methods list -
# apply_browser_styling
# browse_directory
# copy_files
# copy_path_to_clipboard
# count_tree_items
# create_info_panel
# create_menubar
# create_new_folder
# create_toolbar
# cut_files
# delete_selected
# file_contains_text
# get_file_type_display
# get_file_type_icon
# integrate_directory_tree_browser
# is_dark_theme
# load_browser_settings
# navigate_back
# navigate_forward
# navigate_home
# navigate_to_address
# navigate_up
# on_item_clicked
# on_item_double_clicked
# open_in_explorer
# paste_files
# populate_tree
# populate_tree_recursive
# refresh_browser
# rename_selected
# save_browser_settings
# search_files
# setup_connections
# setup_edit_menu
# setup_file_menu
# setup_settings_menu
# setup_tools_menu
# setup_tree_view
# setup_ui
# show_context_menu
# show_file_properties
# show_file_search_dialog
# update_file_info

class DirectoryTreeBrowser(QWidget):
    """Complete directory tree browser widget"""
    
    # Signals
    file_selected = pyqtSignal(str)
    file_opened = pyqtSignal(str)
    directory_changed = pyqtSignal(str)
    
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.current_path = None
        self.clipboard_files = []
        self.clipboard_operation = None
        self.navigation_history = []
        self.history_index = -1
        self.browser_settings = self.load_browser_settings()
        self.setup_ui()
        self.setup_connections()
        
    def setup_ui(self): #vers 1
        """Setup complete browser UI"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        
        # Menu bar
        self.menubar = self.create_menubar()
        layout.addWidget(self.menubar)
        
        # Toolbar - ICONS ONLY, no text
        self.toolbar = self.create_toolbar()
        layout.addWidget(self.toolbar)
        
        # Address bar
        address_layout = QHBoxLayout()
        address_layout.addWidget(QLabel("Location:"))
        
        self.address_bar = QLineEdit()
        self.address_bar.setPlaceholderText("Enter path...")
        self.address_bar.returnPressed.connect(self.navigate_to_address)
        address_layout.addWidget(self.address_bar)
        
        go_btn = QPushButton("Go")
        go_btn.clicked.connect(self.navigate_to_address)
        go_btn.setMaximumHeight(30)
        address_layout.addWidget(go_btn)
        
        layout.addLayout(address_layout)

        # Path label - ONE LINE above tree
        self.path_label = QLabel("No directory loaded")
        #self.path_label.setStyleSheet("padding: 3px 5px; background-color: #2a2a2a; color: #ffffff;")
        #layout.addWidget(self.path_label)
        
        # Main browser area
        splitter = QSplitter(Qt.Orientation.Horizontal)
        
        # Left: Directory tree
        self.tree = QTreeWidget()
        self.tree.setHeaderLabel("Directory Structure")
        self.setup_tree_view()
        #self.apply_browser_styling()
        splitter.addWidget(self.tree)
        
        # Right: File info
        info_widget = self.create_info_panel()
        splitter.addWidget(info_widget)
        
        #splitter.setSizes([700])
        layout.addWidget(splitter)
        

    def setup_tree_view(self): #vers 1
        """Setup tree widget"""
        self.tree.setAlternatingRowColors(True)
        self.tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.tree.customContextMenuRequested.connect(self.show_context_menu)
        self.tree.itemClicked.connect(self.on_item_clicked)
        self.tree.itemDoubleClicked.connect(self.on_item_double_clicked)
        

    def apply_browser_styling(self): #vers 1
        """Apply theme-aware styling"""
        if self.is_dark_theme():
            stylesheet = """
                QTreeWidget {
                    background-color: #1e1e1e;
                    color: #ffffff;
                    border: 1px solid #3a3a3a;
                }
                QTreeWidget::item:selected {
                    background-color: #3a7ca8;
                }
                QTreeWidget::item:hover {
                    background-color: #2a2a2a;
                }
            """
        else:
            stylesheet = """
                QTreeWidget {
                    background-color: #ffffff;
                    color: #000000;
                    border: 1px solid #cccccc;
                }
                QTreeWidget::item:selected {
                    background-color: #0078d7;
                    color: #ffffff;
                }
                QTreeWidget::item:hover {
                    background-color: #e5f3ff;
                }
            """
        self.tree.setStyleSheet(stylesheet)
        

    def is_dark_theme(self) -> bool: #vers 1
        """Detect if dark theme is active"""
        bg_color = self.palette().color(self.backgroundRole())
        return bg_color.lightness() < 128
        

    def setup_connections(self): #vers 1
        """Setup signal connections"""
        pass
        

    def create_menubar(self): #vers 1
        """Create menu bar"""
        menubar = QMenuBar()
        
        # File menu
        file_menu = menubar.addMenu("File")
        self.setup_file_menu(file_menu)
        
        # Edit menu
        edit_menu = menubar.addMenu("Edit")
        self.setup_edit_menu(edit_menu)
        
        # Tools menu
        tools_menu = menubar.addMenu("Tools")
        self.setup_tools_menu(tools_menu)
        
        # Settings menu
        settings_menu = menubar.addMenu("Settings")
        self.setup_settings_menu(settings_menu)
        
        return menubar
        

    def setup_file_menu(self, menu): #vers 1
        """Setup File menu"""
        new_folder_action = QAction("New Folder", self)
        new_folder_action.setIcon(get_new_folder_icon())
        new_folder_action.triggered.connect(self.create_new_folder)
        menu.addAction(new_folder_action)
        
        menu.addSeparator()
        
        properties_action = QAction("Properties", self)
        properties_action.setIcon(get_properties_icon())
        properties_action.triggered.connect(self.show_file_properties)
        menu.addAction(properties_action)
        

    def setup_edit_menu(self, menu): #vers 1
        """Setup Edit menu"""
        cut_action = QAction("Cut", self)
        cut_action.setIcon(get_cut_icon())
        cut_action.triggered.connect(self.cut_files)
        menu.addAction(cut_action)
        
        copy_action = QAction("Copy", self)
        copy_action.setIcon(get_copy_icon())
        copy_action.triggered.connect(self.copy_files)
        menu.addAction(copy_action)
        
        paste_action = QAction("Paste", self)
        paste_action.setIcon(get_paste_icon())
        paste_action.triggered.connect(self.paste_files)
        menu.addAction(paste_action)
        
        menu.addSeparator()
        
        delete_action = QAction("Delete", self)
        delete_action.setIcon(get_trash_icon())
        delete_action.triggered.connect(self.delete_selected)
        menu.addAction(delete_action)
        
        rename_action = QAction("Rename", self)
        rename_action.setIcon(get_rename_icon())
        rename_action.triggered.connect(self.rename_selected)
        menu.addAction(rename_action)
        

    def setup_tools_menu(self, menu): #vers 1
        """Setup Tools menu"""
        search_action = QAction("Search Files", self)
        search_action.setIcon(get_search_icon())
        search_action.triggered.connect(self.show_file_search_dialog)
        menu.addAction(search_action)
        

    def setup_settings_menu(self, menu): #vers 1
        """Setup Settings menu"""
        refresh_action = QAction("Refresh", self)
        refresh_action.setIcon(get_refresh_icon())
        refresh_action.triggered.connect(self.refresh_browser)
        menu.addAction(refresh_action)
        

    def create_toolbar(self): #vers 1
        """Create toolbar - ICONS ONLY"""
        toolbar = QWidget()
        layout = QHBoxLayout(toolbar)
        layout.setContentsMargins(2, 2, 2, 2)
        layout.setSpacing(2)
        
        # Navigation buttons - ICON ONLY
        back_btn = QPushButton()
        back_btn.setIcon(get_back_icon())
        back_btn.setToolTip("Back")
        back_btn.setMaximumSize(32, 32)
        back_btn.clicked.connect(self.navigate_back)
        layout.addWidget(back_btn)
        
        forward_btn = QPushButton()
        forward_btn.setIcon(get_forward_icon())
        forward_btn.setToolTip("Forward")
        forward_btn.setMaximumSize(32, 32)
        forward_btn.clicked.connect(self.navigate_forward)
        layout.addWidget(forward_btn)
        
        up_btn = QPushButton()
        up_btn.setIcon(get_up_icon())
        up_btn.setToolTip("Up")
        up_btn.setMaximumSize(32, 32)
        up_btn.clicked.connect(self.navigate_up)
        layout.addWidget(up_btn)
        
        home_btn = QPushButton()
        home_btn.setIcon(get_home_icon())
        home_btn.setToolTip("Home")
        home_btn.setMaximumSize(32, 32)
        home_btn.clicked.connect(self.navigate_home)
        layout.addWidget(home_btn)
        
        layout.addSpacing(10)
        
        # File operations - ICON ONLY
        new_folder_btn = QPushButton()
        new_folder_btn.setIcon(get_new_folder_icon())
        new_folder_btn.setToolTip("New Folder")
        new_folder_btn.setMaximumSize(32, 32)
        new_folder_btn.clicked.connect(self.create_new_folder)
        layout.addWidget(new_folder_btn)
        
        copy_btn = QPushButton()
        copy_btn.setIcon(get_copy_icon())
        copy_btn.setToolTip("Copy")
        copy_btn.setMaximumSize(32, 32)
        copy_btn.clicked.connect(self.copy_files)
        layout.addWidget(copy_btn)
        
        paste_btn = QPushButton()
        paste_btn.setIcon(get_paste_icon())
        paste_btn.setToolTip("Paste")
        paste_btn.setMaximumSize(32, 32)
        paste_btn.clicked.connect(self.paste_files)
        layout.addWidget(paste_btn)
        
        delete_btn = QPushButton()
        delete_btn.setIcon(get_trash_icon())
        delete_btn.setToolTip("Delete")
        delete_btn.setMaximumSize(32, 32)
        delete_btn.clicked.connect(self.delete_selected)
        layout.addWidget(delete_btn)
        
        layout.addStretch()
        
        # Search and refresh - ICON ONLY
        search_btn = QPushButton()
        search_btn.setIcon(get_search_icon())
        search_btn.setToolTip("Search")
        search_btn.setMaximumSize(32, 32)
        search_btn.clicked.connect(self.show_file_search_dialog)
        layout.addWidget(search_btn)
        
        refresh_btn = QPushButton()
        refresh_btn.setIcon(get_refresh_icon())
        refresh_btn.setToolTip("Refresh")
        refresh_btn.setMaximumSize(32, 32)
        refresh_btn.clicked.connect(self.refresh_browser)
        layout.addWidget(refresh_btn)
        
        return toolbar
        

    def create_info_panel(self): #vers 1
        """Create file info panel"""
        widget = QGroupBox("File Information")
        layout = QVBoxLayout(widget)

        self.info_text = QTextEdit()
        self.info_text.setReadOnly(True)
        layout.addWidget(self.info_text)
        
        return widget
        

    def browse_directory(self, path: str): #vers 1
        """Browse to specific directory"""
        if os.path.exists(path) and os.path.isdir(path):
            self.current_path = path
            self.address_bar.setText(path)
            self.path_label.setText(f"Root: {path}")
            self.populate_tree(path)
            self.directory_changed.emit(path)
            
            # Add to navigation history
            if self.history_index < len(self.navigation_history) - 1:
                self.navigation_history = self.navigation_history[:self.history_index + 1]
            self.navigation_history.append(path)
            self.history_index = len(self.navigation_history) - 1
            
            self.log_message(f"Browsing: {path}")
            

    def populate_tree(self, root_path: str): #vers 1
        """Populate tree with directory contents"""
        try:
            self.tree.clear()
            
            root_item = QTreeWidgetItem(self.tree)
            root_item.setText(0, os.path.basename(root_path) or root_path)
            root_item.setData(0, Qt.ItemDataRole.UserRole, root_path)
            root_item.setIcon(0, get_folder_icon())
            
            self.populate_tree_recursive(root_item, root_path, max_depth=3)
            
            root_item.setExpanded(True)
            
            # Auto-expand first level
            for i in range(min(root_item.childCount(), 5)):
                child = root_item.child(i)
                if child and child.childCount() > 0:
                    child.setExpanded(True)
            
            total_items = self.count_tree_items(root_item)
            self.log_message(f"Loaded: {root_path} ({total_items} items)")
            
        except Exception as e:
            self.log_message(f"Error populating tree: {str(e)}")
            

    def count_tree_items(self, item): #vers 1
        """Count total items in tree"""
        count = 1
        for i in range(item.childCount()):
            count += self.count_tree_items(item.child(i))
        return count
            

    def populate_tree_recursive(self, parent_item: QTreeWidgetItem,
                               dir_path: str, max_depth: int = 3,
                               current_depth: int = 0): #vers 1
        """Recursively populate tree"""
        if current_depth >= max_depth:
            return
            
        try:
            all_items = os.listdir(dir_path)
            
            # Filter hidden files
            if not self.browser_settings.get('show_hidden', False):
                filtered_items = [item for item in all_items if not item.startswith('.')]
            else:
                filtered_items = all_items[:]
            
            # Separate directories and files
            directories = []
            files = []
            
            for item in filtered_items:
                item_path = os.path.join(dir_path, item)
                try:
                    if os.path.isdir(item_path):
                        directories.append(item)
                    elif os.path.isfile(item_path):
                        files.append(item)
                except (PermissionError, OSError):
                    continue
            
            # Sort
            directories.sort(key=str.lower)
            files.sort(key=str.lower)
            
            # Add directories
            for directory in directories:
                item_path = os.path.join(dir_path, directory)
                tree_item = QTreeWidgetItem(parent_item)
                tree_item.setText(0, directory)
                tree_item.setData(0, Qt.ItemDataRole.UserRole, item_path)
                tree_item.setIcon(0, get_folder_icon())
                
                # Recursively populate
                self.populate_tree_recursive(tree_item, item_path, max_depth, current_depth + 1)
            
            # Add files
            for file in files:
                item_path = os.path.join(dir_path, file)
                tree_item = QTreeWidgetItem(parent_item)
                tree_item.setText(0, file)
                tree_item.setData(0, Qt.ItemDataRole.UserRole, item_path)
                
                # Set file icon
                file_ext = os.path.splitext(file)[1].lower()
                icon = self.get_file_type_icon(file_ext)
                tree_item.setIcon(0, icon)
                
        except Exception:
            pass
            

    def get_file_type_icon(self, file_ext: str): #vers 1
        """Get SVG icon for file type"""
        icon_map = {
            '.img': get_img_file_icon,
            '.txd': get_txd_file_icon,
            '.col': get_col_file_icon,
            '.dff': get_image_icon,
            '.ide': get_edit_icon,
            '.ipl': get_view_icon,
            '.dat': get_file_icon,
        }
        icon_func = icon_map.get(file_ext, get_file_icon)
        return icon_func()
        

    def get_file_type_display(self, file_ext: str) -> str: #vers 1
        """Get display name for file type"""
        type_map = {
            '.img': 'IMG Archive',
            '.txd': 'Texture Dictionary',
            '.col': 'Collision File',
            '.dff': '3D Model',
            '.ide': 'Item Definition',
            '.ipl': 'Item Placement',
            '.dat': 'Data File',
        }
        return type_map.get(file_ext, 'File')
        

    def on_item_clicked(self, item, column): #vers 1
        """Handle item click"""
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if file_path and os.path.isfile(file_path):
            self.update_file_info(file_path)
            self.file_selected.emit(file_path)
            

    def on_item_double_clicked(self, item, column): #vers 1
        """Handle item double-click"""
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        
        if os.path.isdir(file_path):
            self.browse_directory(file_path)
        elif os.path.isfile(file_path):
            self.file_opened.emit(file_path)
            

    def update_file_info(self, file_path: str): #vers 1
        """Update file info panel"""
        try:
            stats = os.stat(file_path)
            file_ext = os.path.splitext(file_path)[1]
            
            info = f"File: {os.path.basename(file_path)}\n"
            info += f"Type: {self.get_file_type_display(file_ext)}\n"
            info += f"Size: {stats.st_size:,} bytes\n"
            info += f"Path: {file_path}\n"
            
            self.info_text.setText(info)
        except Exception as e:
            self.info_text.setText(f"Error: {str(e)}")
            

    def show_context_menu(self, position): #vers 1
        """Show context menu"""
        item = self.tree.itemAt(position)
        if not item:
            return
            
        menu = QMenu(self)
        
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        
        # Open
        if os.path.isfile(file_path):
            open_action = QAction("Open", self)
            open_action.setIcon(get_folder_icon())
            open_action.triggered.connect(lambda: self.file_opened.emit(file_path))
            menu.addAction(open_action)
            
        menu.addSeparator()
        
        # Copy/Cut/Delete/Rename
        copy_action = QAction("Copy", self)
        copy_action.setIcon(get_copy_icon())
        copy_action.triggered.connect(self.copy_files)
        menu.addAction(copy_action)
        
        cut_action = QAction("Cut", self)
        cut_action.setIcon(get_cut_icon())
        cut_action.triggered.connect(self.cut_files)
        menu.addAction(cut_action)
        
        paste_action = QAction("Paste", self)
        paste_action.setIcon(get_paste_icon())
        paste_action.triggered.connect(self.paste_files)
        menu.addAction(paste_action)
        
        menu.addSeparator()
        
        delete_action = QAction("Delete", self)
        delete_action.setIcon(get_trash_icon())
        delete_action.triggered.connect(self.delete_selected)
        menu.addAction(delete_action)
        
        rename_action = QAction("Rename", self)
        rename_action.setIcon(get_rename_icon())
        rename_action.triggered.connect(self.rename_selected)
        menu.addAction(rename_action)
        
        menu.addSeparator()
        
        # Copy path
        copy_path_action = QAction("Copy Path", self)
        copy_path_action.setIcon(get_copy_icon())
        copy_path_action.triggered.connect(lambda: self.copy_path_to_clipboard(file_path))
        menu.addAction(copy_path_action)
        
        # Open in explorer
        explorer_action = QAction("Open in File Manager", self)
        explorer_action.setIcon(get_folder_icon())
        explorer_action.triggered.connect(lambda: self.open_in_explorer(file_path))
        menu.addAction(explorer_action)
        
        menu.addSeparator()
        
        # Properties
        props_action = QAction("Properties", self)
        props_action.setIcon(get_properties_icon())
        props_action.triggered.connect(self.show_file_properties)
        menu.addAction(props_action)
        
        menu.exec(self.tree.mapToGlobal(position))
        

    # Navigation methods
    def navigate_back(self): #vers 1
        """Navigate back in history"""
        if self.history_index > 0:
            self.history_index -= 1
            path = self.navigation_history[self.history_index]
            self.current_path = path
            self.address_bar.setText(path)
            self.path_label.setText(f"Root: {path}")
            self.populate_tree(path)
        

    def navigate_forward(self): #vers 1
        """Navigate forward in history"""
        if self.history_index < len(self.navigation_history) - 1:
            self.history_index += 1
            path = self.navigation_history[self.history_index]
            self.current_path = path
            self.address_bar.setText(path)
            self.path_label.setText(f"Root: {path}")
            self.populate_tree(path)
        

    def navigate_up(self): #vers 1
        """Navigate to parent directory"""
        if self.current_path:
            parent = os.path.dirname(self.current_path)
            if parent != self.current_path:
                self.browse_directory(parent)
                

    def navigate_home(self): #vers 1
        """Navigate to home directory"""
        home_path = os.path.expanduser("~")
        self.browse_directory(home_path)
        

    def navigate_to_address(self): #vers 1
        """Navigate to address bar path"""
        path = self.address_bar.text()
        if os.path.exists(path):
            self.browse_directory(path)
        else:
            QMessageBox.warning(self, "Invalid Path", f"Path does not exist: {path}")
            

    # File operations
    def create_new_folder(self): #vers 1
        """Create new folder"""
        if not self.current_path:
            QMessageBox.warning(self, "No Directory", "Navigate to a directory first")
            return
            
        name, ok = QInputDialog.getText(self, "New Folder", "Folder name:")
        if ok and name:
            new_path = os.path.join(self.current_path, name)
            try:
                os.makedirs(new_path, exist_ok=False)
                self.refresh_browser()
                self.log_message(f"Created folder: {name}")
            except FileExistsError:
                QMessageBox.warning(self, "Exists", f"Folder '{name}' already exists")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to create folder: {str(e)}")
                

    def copy_files(self): #vers 1
        """Copy selected files to clipboard"""
        selected_items = self.tree.selectedItems()
        if selected_items:
            self.clipboard_files = [item.data(0, Qt.ItemDataRole.UserRole) for item in selected_items]
            self.clipboard_operation = 'copy'
            self.log_message(f"Copied {len(self.clipboard_files)} item(s)")
            

    def cut_files(self): #vers 1
        """Cut selected files to clipboard"""
        selected_items = self.tree.selectedItems()
        if selected_items:
            self.clipboard_files = [item.data(0, Qt.ItemDataRole.UserRole) for item in selected_items]
            self.clipboard_operation = 'cut'
            self.log_message(f"Cut {len(self.clipboard_files)} item(s)")
            

    def paste_files(self): #vers 1
        """Paste files from clipboard"""
        if not self.clipboard_files or not self.current_path:
            return
            
        try:
            for src in self.clipboard_files:
                if not os.path.exists(src):
                    continue
                    
                dest = os.path.join(self.current_path, os.path.basename(src))
                
                if self.clipboard_operation == 'copy':
                    if os.path.isdir(src):
                        shutil.copytree(src, dest)
                    else:
                        shutil.copy2(src, dest)
                elif self.clipboard_operation == 'cut':
                    shutil.move(src, dest)
                    
            self.refresh_browser()
            self.log_message(f"Pasted {len(self.clipboard_files)} item(s)")
            
            if self.clipboard_operation == 'cut':
                self.clipboard_files = []
                
        except Exception as e:
            QMessageBox.critical(self, "Paste Error", f"Error: {str(e)}")
            

    def delete_selected(self):#vers 1
        """Delete selected files"""
        selected_items = self.tree.selectedItems()
        if not selected_items:
            return
            
        reply = QMessageBox.question(
            self, "Delete",
            f"Delete {len(selected_items)} item(s)?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )
        
        if reply == QMessageBox.StandardButton.Yes:
            try:
                for item in selected_items:
                    file_path = item.data(0, Qt.ItemDataRole.UserRole)
                    if os.path.isdir(file_path):
                        shutil.rmtree(file_path)
                    else:
                        os.remove(file_path)
                        
                self.refresh_browser()
                self.log_message(f"Deleted {len(selected_items)} item(s)")
            except Exception as e:
                QMessageBox.critical(self, "Delete Error", f"Error: {str(e)}")
                

    def rename_selected(self): #vers 1
        """Rename selected file"""
        selected_items = self.tree.selectedItems()
        if not selected_items:
            return
            
        item = selected_items[0]
        old_path = item.data(0, Qt.ItemDataRole.UserRole)
        old_name = os.path.basename(old_path)
        
        new_name, ok = QInputDialog.getText(self, "Rename", "New name:", text=old_name)
        if ok and new_name and new_name != old_name:
            try:
                new_path = os.path.join(os.path.dirname(old_path), new_name)
                os.rename(old_path, new_path)
                self.refresh_browser()
                self.log_message(f"Renamed: {old_name} → {new_name}")
            except Exception as e:
                QMessageBox.critical(self, "Rename Error", f"Error: {str(e)}")
                

    def copy_path_to_clipboard(self, file_path: str): #vers 1
        """Copy file path to clipboard"""
        try:
            from PyQt6.QtWidgets import QApplication
            clipboard = QApplication.clipboard()
            clipboard.setText(file_path)
            self.log_message(f"Path copied: {file_path}")
        except Exception as e:
            self.log_message(f"Error copying path: {str(e)}")
            

    def open_in_explorer(self, file_path: str): #vers 1
        """Open file in system file manager"""
        try:
            system = platform.system()
            if system == "Linux":
                subprocess.Popen(['xdg-open', os.path.dirname(file_path)])
            elif system == "Darwin":
                subprocess.Popen(['open', '-R', file_path])
            elif system == "Windows":
                subprocess.run(['explorer', '/select,', file_path])
        except Exception as e:
            self.log_message(f"Error opening file manager: {str(e)}")
            

    def show_file_properties(self): #vers 1
        """Show file properties"""
        selected_items = self.tree.selectedItems()
        if not selected_items:
            return
            
        file_path = selected_items[0].data(0, Qt.ItemDataRole.UserRole)
        try:
            stats = os.stat(file_path)
            info = f"File: {os.path.basename(file_path)}\n"
            info += f"Path: {file_path}\n"
            info += f"Size: {stats.st_size:,} bytes\n"
            QMessageBox.information(self, "Properties", info)
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Could not get properties: {str(e)}")
            

    def show_file_search_dialog(self): #vers 1
        """Show file search dialog"""
        dialog = QDialog(self)
        dialog.setWindowTitle("Search Files")
        dialog.setModal(True)
        #dialog.setFixedSize(600, 500)
        
        layout = QVBoxLayout(dialog)
        
        # Search options
        options_layout = QHBoxLayout()
        
        filename_input = QLineEdit()
        filename_input.setPlaceholderText("Filename pattern (e.g., *.img)")
        options_layout.addWidget(QLabel("Filename:"))
        options_layout.addWidget(filename_input)
        
        layout.addLayout(options_layout)
        
        # Exact match
        exact_match = QCheckBox("Exact match only")
        layout.addWidget(exact_match)
        
        # Results
        results_list = QListWidget()
        layout.addWidget(results_list)
        
        # Buttons
        button_layout = QHBoxLayout()
        
        search_btn = QPushButton("Search")
        search_btn.setIcon(get_search_icon())
        button_layout.addWidget(search_btn)
        
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.accept)
        button_layout.addWidget(close_btn)
        
        layout.addLayout(button_layout)
        
        # Search function
        def perform_search():
            results_list.clear()
            pattern = filename_input.text().strip()
            
            if not pattern:
                QMessageBox.warning(dialog, "No Pattern", "Enter a filename pattern")
                return
            
            results = self.search_files(self.current_path, pattern, exact_match.isChecked())
            
            if results:
                for file_path in results:
                    results_list.addItem(file_path)
            else:
                results_list.addItem("No files found")
        
        search_btn.clicked.connect(perform_search)
        
        dialog.exec()
        

    def search_files(self, search_path: str, pattern: str, exact_match: bool) -> List[str]: #vers 1
        """Search for files by name"""
        found_files = []
        
        try:
            for root, dirs, files in os.walk(search_path):
                for filename in files:
                    if exact_match:
                        if filename.lower() == pattern.lower():
                            found_files.append(os.path.join(root, filename))
                    else:
                        if fnmatch.fnmatch(filename.lower(), pattern.lower()):
                            found_files.append(os.path.join(root, filename))
        except Exception as e:
            self.log_message(f"Search error: {str(e)}")
        
        return found_files
        

    def file_contains_text(self, file_path: str, search_text: str) -> bool: #vers 1
        """Check if file contains text"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                return search_text.lower() in content.lower()
        except Exception:
            return False
            

    def refresh_browser(self): #vers 1
        """Refresh current view"""
        if self.current_path:
            self.populate_tree(self.current_path)
            self.log_message("Refreshed")
            

    def load_browser_settings(self) -> dict: #vers 1
        """Load browser settings"""
        settings = QSettings("IMG_Factory", "FileBrowser")
        return {
            'show_hidden': settings.value('show_hidden', False, type=bool),
        }
        

    def save_browser_settings(self): #vers 1
        """Save browser settings"""
        settings = QSettings("IMG_Factory", "FileBrowser")
        for key, value in self.browser_settings.items():
            settings.setValue(key, value)
            

    def log_message(self, message: str): #vers 1
        """Log message"""
        print(f"[DirectoryBrowser] {message}")


def integrate_directory_tree_browser(main_window): #vers 1
    """Integrate directory browser into main window"""
    try:
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'tab_widget'):
            main_window.log_message("Tab widget not found")
            return False

        tab_widget = main_window.gui_layout.tab_widget
        
        # Find Directory Tree tab
        directory_tab_index = -1
        if tab_widget and hasattr(tab_widget, 'count'):
            for i in range(tab_widget.count()):
                if "Directory Tree" in tab_widget.tabText(i):
                    directory_tab_index = i
                    break
        
        if directory_tab_index == -1:
            main_window.log_message("Directory Tree tab not found")
            return False

        # Get existing tab
        existing_tab = tab_widget.widget(directory_tab_index)
        if not existing_tab:
            return False

        # Clear existing layout
        existing_layout = existing_tab.layout()
        if existing_layout:
            while existing_layout.count():
                child = existing_layout.takeAt(0)
                if child.widget():
                    child.widget().deleteLater()
        else:
            from PyQt6.QtWidgets import QVBoxLayout
            existing_layout = QVBoxLayout(existing_tab)

        # Create browser
        directory_browser = DirectoryTreeBrowser(main_window)
        
        # Connect signals
        if hasattr(main_window, 'load_file_unified'):
            directory_browser.file_opened.connect(main_window.load_file_unified)
        
        directory_browser.log_message = main_window.log_message
        
        # Add to tab
        existing_layout.addWidget(directory_browser)
        
        # Update tab
        tab_widget.setTabText(directory_tab_index, "Directory Tree")
        tab_widget.setTabIcon(directory_tab_index, get_folder_icon())
        
        # Store reference
        main_window.directory_tree = directory_browser
        
        # Auto-load saved game root
        settings = QSettings("IMG-Factory", "IMG-Factory")
        saved_root = settings.value("game_root", "", type=str)
        if saved_root and os.path.exists(saved_root):
            directory_browser.browse_directory(saved_root)
            main_window.log_message(f"Loaded saved game root: {saved_root}")
        
        main_window.log_message("Directory browser integrated")
        return True

    except Exception as e:
        main_window.log_message(f"Error integrating directory browser: {str(e)}")
        return False


__all__ = [
    'DirectoryTreeBrowser',
    'integrate_directory_tree_browser'
]
