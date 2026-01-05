#this belongs in components/File_Editor/file_dirtree_browser.py - Version: 3
# X-Seti - January03 2025 - IMG Factory 1.6 - Complete File Directory Tree Browser

"""
COMPLETE FILE DIRECTORY TREE BROWSER
Comprehensive file browser with Edit, View, and Settings menus
Similar to Caja/Dolphin file managers but integrated with IMG Factory
Backend classes moved to file_dirtree_backend.py for size optimization
NO EMOJIS - Uses SVG icons from imgfactory_svg_icons.py
"""

import os
import shutil
import subprocess
import platform
from typing import Dict, List, Optional, Tuple
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QGridLayout, QTreeWidget, QTreeWidgetItem,
    QMenuBar, QMenu, QToolBar, QPushButton, QLineEdit, QLabel, QMessageBox,
    QSplitter, QTextEdit, QGroupBox, QComboBox, QCheckBox, QSpinBox,
    QDialog, QFormLayout, QTabWidget, QSlider, QRadioButton, QButtonGroup,
    QFileDialog, QInputDialog, QProgressDialog, QListWidget, QTableWidget
)
from PyQt6.QtCore import Qt, pyqtSignal, QThread, pyqtSlot, QTimer, QSettings
from PyQt6.QtGui import QAction, QIcon, QFont, QKeySequence, QActionGroup

# Import backend classes and functions
from apps.components.File_Editor.file_dirtree_backend import (
    BrowserSettingsDialog, FilePropertiesDialog, FileSearchDialog,
    format_file_size_backend, get_file_type_icon_backend, 
    get_file_type_display_backend, get_file_attributes_backend,
    get_folder_size_quick_backend
)

# SVG Icons
from apps.methods.imgfactory_svg_icons import (
    get_folder_icon, get_file_icon, get_img_file_icon,
    get_txd_file_icon, get_col_file_icon, get_refresh_icon,
    get_close_icon, get_checkmark_icon, get_view_icon,
    get_error_icon, get_search_icon, get_settings_icon,
    get_edit_icon, get_image_icon, get_copy_icon,
    get_paste_icon, get_cut_icon, get_rename_icon,
    get_back_icon, get_forward_icon, get_up_icon,
    get_home_icon, get_terminal_icon, get_tools_icon,
    get_link_icon, get_calculator_icon, get_tree_icon,
    get_properties_icon, get_new_folder_icon, get_new_file_icon,
    get_trash_icon, get_add_icon, get_remove_icon
)

##Methods list -
# apply_browser_styling
# apply_settings
# browse_directory
# calculate_folder_size
# copy_files
# count_tree_items
# create_context_menu
# create_directory
# create_file_browser_widget
# create_info_panel
# create_menubar
# create_new_file
# create_new_folder
# create_toolbar
# cut_files
# delete_files
# delete_selected
# edit_file
# edit_selected
# get_browser_settings
# get_file_size_for_sort
# handle_browser_settings
# invert_selection
# load_browser_settings
# log_message
# move_files
# navigate_back
# navigate_forward
# navigate_home
# navigate_to_address
# navigate_up
# on_item_clicked
# on_item_double_clicked
# on_view_combo_changed
# open_command_prompt
# open_file_properties
# open_selected
# open_terminal
# open_with_dialog
# paste_files
# populate_tree
# populate_tree_recursive
# refresh_browser
# refresh_view
# rename_file
# rename_selected
# reset_browser_settings
# save_browser_settings
# select_all_files
# set_sort_mode
# set_view_mode
# setup_browser_menubar
# setup_browser_toolbar
# setup_connections
# setup_edit_menu
# setup_file_menu
# setup_settings_menu
# setup_tools_menu
# setup_tree_view
# setup_ui
# show_browser_settings
# show_context_menu
# show_default_apps
# show_file_associations
# show_file_properties
# show_file_search_dialog
# show_properties
# show_search_dialog
# toggle_extensions
# toggle_hidden_files
# update_file_info

##Classes -
# FileBrowserWidget

class FileBrowserWidget(QWidget):
    """Complete file browser widget with full menu system"""
    
    # Signals
    file_selected = pyqtSignal(str)
    file_opened = pyqtSignal(str)
    directory_changed = pyqtSignal(str)
    
    def __init__(self, parent=None): #vers 2
        super().__init__(parent)
        self.current_path = None
        self.clipboard_files = []
        self.clipboard_operation = None
        self.browser_settings = self.load_browser_settings()
        self.setup_ui()
        self.setup_connections()
        

    def setup_ui(self): #vers 3
        """Setup complete file browser UI"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        # Menu bar
        self.menubar = self.create_menubar()
        layout.addWidget(self.menubar)

        # Toolbar
        self.toolbar = self.create_toolbar()
        layout.addWidget(self.toolbar)

        # Path label - MOVED HERE (below toolbar, above tree)
        self.path_label = QLabel("No directory loaded")
        self.path_label.setStyleSheet("padding: 5px; background-color: #2a2a2a; color: #ffffff;")
        layout.addWidget(self.path_label)

        # Address bar
        address_layout = QHBoxLayout()
        address_layout.addWidget(QLabel("Location:"))

        self.address_bar = QLineEdit()
        self.address_bar.setPlaceholderText("Enter path or browse...")
        self.address_bar.returnPressed.connect(self.navigate_to_address)
        address_layout.addWidget(self.address_bar)

        go_btn = QPushButton("Go")
        go_btn.clicked.connect(self.navigate_to_address)
        go_btn.setMaximumHeight(25)
        address_layout.addWidget(go_btn)

        layout.addLayout(address_layout)

        # Main browser area
        splitter = QSplitter(Qt.Orientation.Horizontal)

        # Left: Directory tree
        self.tree = QTreeWidget()
        self.tree.setHeaderLabel("Directory Structure")
        self.setup_tree_view()
        self.apply_browser_styling()
        splitter.addWidget(self.tree)

        # Right: File info
        info_widget = self.create_info_panel()
        splitter.addWidget(info_widget)

        splitter.setSizes([600, 300])
        layout.addWidget(splitter)
        # NO path_label here - removed from bottom


    def show_file_search_dialog(self): #vers 2
        """Show enhanced file search dialog"""
        dialog = QDialog(self)
        dialog.setWindowTitle("Search Files")
        dialog.setModal(True)
        dialog.setFixedSize(600, 500)

        layout = QVBoxLayout(dialog)

        # Search options
        options_group = QGroupBox("Search Options")
        options_layout = QFormLayout(options_group)

        # Filename search
        filename_input = QLineEdit()
        filename_input.setPlaceholderText("e.g., *.img, player*.dff")
        options_layout.addRow("Filename pattern:", filename_input)

        # Content search
        content_input = QLineEdit()
        content_input.setPlaceholderText("Search text inside files")
        options_layout.addRow("Content contains:", content_input)

        # Search path
        path_input = QLineEdit()
        path_input.setText(self.current_path or "")
        options_layout.addRow("Search in:", path_input)

        # Options
        case_sensitive = QCheckBox("Case sensitive")
        options_layout.addRow("", case_sensitive)

        recursive = QCheckBox("Include subdirectories")
        recursive.setChecked(True)
        options_layout.addRow("", recursive)

        layout.addWidget(options_group)

        # Results
        results_group = QGroupBox("Search Results")
        results_layout = QVBoxLayout(results_group)

        results_list = QListWidget()
        results_layout.addWidget(results_list)

        layout.addWidget(results_group)

        # Buttons
        button_layout = QHBoxLayout()

        search_btn = QPushButton("Search")
        search_btn.setIcon(get_search_icon())
        button_layout.addWidget(search_btn)

        open_btn = QPushButton("Open Selected")
        open_btn.setIcon(get_folder_icon())
        open_btn.setEnabled(False)
        button_layout.addWidget(open_btn)

        button_layout.addStretch()

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.accept)
        button_layout.addWidget(close_btn)

        layout.addLayout(button_layout)

        # Search function
        def perform_search():
            results_list.clear()
            filename_pattern = filename_input.text().strip()
            content_text = content_input.text().strip()
            search_path = path_input.text().strip()

            if not search_path or not os.path.exists(search_path):
                QMessageBox.warning(dialog, "Invalid Path", "Search path does not exist")
                return

            if not filename_pattern and not content_text:
                QMessageBox.warning(dialog, "No Search Criteria", "Enter filename pattern or content to search")
                return

            results_list.addItem("Searching...")

            # Perform search
            found_files = self.search_files(
                search_path,
                filename_pattern,
                content_text,
                case_sensitive.isChecked(),
                recursive.isChecked()
            )

            results_list.clear()

            if found_files:
                for file_path in found_files:
                    results_list.addItem(file_path)
                results_list.addItem(f"\n--- Found {len(found_files)} file(s) ---")
                open_btn.setEnabled(True)
            else:
                results_list.addItem("No files found")
                open_btn.setEnabled(False)

        # Open selected file
        def open_selected():
            selected = results_list.currentItem()
            if selected and not selected.text().startswith("---"):
                file_path = selected.text()
                if os.path.isfile(file_path):
                    self.file_opened.emit(file_path)
                    dialog.accept()

        # Double-click to open
        results_list.itemDoubleClicked.connect(lambda item: open_selected())

        search_btn.clicked.connect(perform_search)
        open_btn.clicked.connect(open_selected)

        dialog.exec()


    def search_files(self, search_path: str, filename_pattern: str,
                    content_text: str, case_sensitive: bool, recursive: bool) -> List[str]: #vers 1
        """Search for files by name and/or content"""
        import fnmatch

        found_files = []

        try:
            if recursive:
                # Walk through all subdirectories
                for root, dirs, files in os.walk(search_path):
                    for filename in files:
                        file_path = os.path.join(root, filename)

                        # Check filename pattern
                        if filename_pattern:
                            pattern = filename_pattern if case_sensitive else filename_pattern.lower()
                            name = filename if case_sensitive else filename.lower()

                            if not fnmatch.fnmatch(name, pattern):
                                continue

                        # Check file content
                        if content_text:
                            if self.file_contains_text(file_path, content_text, case_sensitive):
                                found_files.append(file_path)
                            elif not filename_pattern:
                                # Only content search, file didn't match
                                continue
                        else:
                            # Only filename search, already matched
                            found_files.append(file_path)
            else:
                # Only search current directory
                for filename in os.listdir(search_path):
                    file_path = os.path.join(search_path, filename)

                    if not os.path.isfile(file_path):
                        continue

                    # Check filename pattern
                    if filename_pattern:
                        pattern = filename_pattern if case_sensitive else filename_pattern.lower()
                        name = filename if case_sensitive else filename.lower()

                        if not fnmatch.fnmatch(name, pattern):
                            continue

                    # Check file content
                    if content_text:
                        if self.file_contains_text(file_path, content_text, case_sensitive):
                            found_files.append(file_path)
                        elif not filename_pattern:
                            continue
                    else:
                        found_files.append(file_path)

        except Exception as e:
            self.log_message(f"Search error: {str(e)}")

        return found_files


    def file_contains_text(self, file_path: str, search_text: str, case_sensitive: bool) -> bool: #vers 1
        """Check if file contains specific text"""
        try:
            # Only search text files (skip binary files)
            text_extensions = {'.txt', '.log', '.cfg', '.ini', '.dat', '.ide', '.ipl',
                            '.py', '.json', '.xml', '.html', '.css', '.js', '.md'}

            file_ext = os.path.splitext(file_path)[1].lower()

            if file_ext not in text_extensions:
                return False

            # Read file content
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()

                if case_sensitive:
                    return search_text in content
                else:
                    return search_text.lower() in content.lower()

        except Exception:
            return False


    def create_toolbar(self): #vers 2
        """Create navigation toolbar with search"""
        toolbar = QWidget()
        layout = QHBoxLayout(toolbar)
        layout.setContentsMargins(4, 2, 2, 4)

        # Navigation buttons
        back_btn = QPushButton("Back")
        back_btn.setIcon(get_back_icon())
        back_btn.setMaximumHeight(25)
        back_btn.clicked.connect(self.navigate_back)
        layout.addWidget(back_btn)

        forward_btn = QPushButton("Forward")
        forward_btn.setIcon(get_forward_icon())
        forward_btn.setMaximumHeight(25)
        forward_btn.clicked.connect(self.navigate_forward)
        layout.addWidget(forward_btn)

        up_btn = QPushButton("Up")
        up_btn.setIcon(get_up_icon())
        up_btn.setMaximumHeight(25)
        up_btn.clicked.connect(self.navigate_up)
        layout.addWidget(up_btn)

        home_btn = QPushButton("Home")
        home_btn.setIcon(get_home_icon())
        home_btn.setMaximumHeight(25)
        home_btn.clicked.connect(self.navigate_home)
        layout.addWidget(home_btn)

        layout.addStretch()

        # Search button
        search_btn = QPushButton("Search")
        search_btn.setIcon(get_search_icon())
        search_btn.setMaximumHeight(25)
        search_btn.clicked.connect(self.show_file_search_dialog)
        layout.addWidget(search_btn)

        # Refresh button
        refresh_btn = QPushButton("Refresh")
        refresh_btn.setIcon(get_refresh_icon())
        refresh_btn.setMaximumHeight(25)
        refresh_btn.clicked.connect(self.refresh_browser)
        layout.addWidget(refresh_btn)

        return toolbar

    def setup_tree_view(self): #vers 1
        """Setup tree widget"""
        self.tree.setAlternatingRowColors(True)
        self.tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.tree.customContextMenuRequested.connect(self.show_context_menu)
        self.tree.itemClicked.connect(self.on_item_clicked)
        self.tree.itemDoubleClicked.connect(self.on_item_double_clicked)
        
    def apply_browser_styling(self): #vers 1
        """Apply theme-aware styling"""
        pass
        
    def setup_connections(self): #vers 1
        """Setup signal connections"""
        pass
        
    def create_menubar(self): #vers 1
        """Create menu bar with File, Edit, View, Tools, Settings"""
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
        
        new_file_action = QAction("New File", self)
        new_file_action.setIcon(get_new_file_icon())
        new_file_action.triggered.connect(self.create_new_file)
        menu.addAction(new_file_action)
        
        menu.addSeparator()
        
        properties_action = QAction("Properties", self)
        properties_action.setIcon(get_properties_icon())
        properties_action.triggered.connect(self.show_file_properties)
        menu.addAction(properties_action)
        
    def setup_edit_menu(self, menu): #vers 1
        """Setup Edit menu"""
        cut_action = QAction("Cut", self)
        cut_action.setIcon(get_cut_icon())
        cut_action.setShortcut(QKeySequence.StandardKey.Cut)
        cut_action.triggered.connect(self.cut_files)
        menu.addAction(cut_action)
        
        copy_action = QAction("Copy", self)
        copy_action.setIcon(get_copy_icon())
        copy_action.setShortcut(QKeySequence.StandardKey.Copy)
        copy_action.triggered.connect(self.copy_files)
        menu.addAction(copy_action)
        
        paste_action = QAction("Paste", self)
        paste_action.setIcon(get_paste_icon())
        paste_action.setShortcut(QKeySequence.StandardKey.Paste)
        paste_action.triggered.connect(self.paste_files)
        menu.addAction(paste_action)
        
        menu.addSeparator()
        
        delete_action = QAction("Delete", self)
        delete_action.setIcon(get_trash_icon())
        delete_action.setShortcut(QKeySequence.StandardKey.Delete)
        delete_action.triggered.connect(self.delete_selected)
        menu.addAction(delete_action)
        
        rename_action = QAction("Rename", self)
        rename_action.setIcon(get_rename_icon())
        rename_action.setShortcut(QKeySequence("F2"))
        rename_action.triggered.connect(self.rename_selected)
        menu.addAction(rename_action)
        
        menu.addSeparator()
        
        select_all_action = QAction("Select All", self)
        select_all_action.setShortcut(QKeySequence.StandardKey.SelectAll)
        select_all_action.triggered.connect(self.select_all_files)
        menu.addAction(select_all_action)
        
    def setup_tools_menu(self, menu): #vers 1
        """Setup Tools menu"""
        search_action = QAction("Search Files", self)
        search_action.setIcon(get_search_icon())
        search_action.triggered.connect(self.show_file_search_dialog)
        menu.addAction(search_action)
        
        menu.addSeparator()
        
        terminal_action = QAction("Open Terminal", self)
        terminal_action.setIcon(get_terminal_icon())
        terminal_action.triggered.connect(self.open_terminal)
        menu.addAction(terminal_action)
        
        calc_size_action = QAction("Calculate Folder Size", self)
        calc_size_action.setIcon(get_calculator_icon())
        calc_size_action.triggered.connect(self.calculate_folder_size)
        menu.addAction(calc_size_action)
        
    def setup_settings_menu(self, menu): #vers 1
        """Setup Settings menu"""
        preferences_action = QAction("Browser Preferences", self)
        preferences_action.setIcon(get_settings_icon())
        preferences_action.triggered.connect(self.show_browser_settings)
        menu.addAction(preferences_action)
        
        menu.addSeparator()
        
        default_apps_action = QAction("Default Applications", self)
        default_apps_action.setIcon(get_tools_icon())
        default_apps_action.triggered.connect(self.show_default_apps)
        menu.addAction(default_apps_action)
        
        associations_action = QAction("File Associations", self)
        associations_action.setIcon(get_link_icon())
        associations_action.triggered.connect(self.show_file_associations)
        menu.addAction(associations_action)
        
        menu.addSeparator()
        
        reset_action = QAction("Reset to Defaults", self)
        reset_action.setIcon(get_refresh_icon())
        reset_action.triggered.connect(self.reset_browser_settings)
        menu.addAction(reset_action)
        
    def create_toolbar(self): #vers 1
        """Create navigation toolbar"""
        toolbar = QWidget()
        layout = QHBoxLayout(toolbar)
        layout.setContentsMargins(4, 2, 2, 4)
        
        # Navigation buttons
        back_btn = QPushButton("Back")
        back_btn.setIcon(get_back_icon())
        back_btn.setMaximumHeight(25)
        back_btn.clicked.connect(self.navigate_back)
        layout.addWidget(back_btn)
        
        forward_btn = QPushButton("Forward")
        forward_btn.setIcon(get_forward_icon())
        forward_btn.setMaximumHeight(25)
        forward_btn.clicked.connect(self.navigate_forward)
        layout.addWidget(forward_btn)
        
        up_btn = QPushButton("Up")
        up_btn.setIcon(get_up_icon())
        up_btn.setMaximumHeight(25)
        up_btn.clicked.connect(self.navigate_up)
        layout.addWidget(up_btn)
        
        home_btn = QPushButton("Home")
        home_btn.setIcon(get_home_icon())
        home_btn.setMaximumHeight(25)
        home_btn.clicked.connect(self.navigate_home)
        layout.addWidget(home_btn)
        
        layout.addStretch()
        
        refresh_btn = QPushButton("Refresh")
        refresh_btn.setIcon(get_refresh_icon())
        refresh_btn.setMaximumHeight(25)
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
            self.populate_tree(path)
            self.directory_changed.emit(path)
            self.log_message(f"Browsing: {path}")
            
    def populate_tree(self, root_path: str): #vers 1
        """Populate tree with directory contents"""
        try:
            self.tree.clear()
            
            root_item = QTreeWidgetItem(self.tree)
            root_item.setText(0, os.path.basename(root_path) or root_path)
            root_item.setData(0, Qt.ItemDataRole.UserRole, root_path)
            root_item.setIcon(0, get_folder_icon())
            
            self.populate_tree_recursive(root_item, root_path, max_depth=4)
            
            root_item.setExpanded(True)
            
            # Auto-expand first level
            for i in range(root_item.childCount()):
                child = root_item.child(i)
                if child and child.childCount() > 0:
                    child.setExpanded(True)
            
            total_items = self.count_tree_items(root_item)
            self.log_message(f"File browser populated: {root_path} ({total_items} items)")
            
        except Exception as e:
            self.log_message(f"Error populating tree: {str(e)}")
            
    def count_tree_items(self, item): #vers 1
        """Count total items in tree recursively"""
        count = 1
        for i in range(item.childCount()):
            count += self.count_tree_items(item.child(i))
        return count
            
    def populate_tree_recursive(self, parent_item: QTreeWidgetItem,
                               dir_path: str,
                               max_depth: int = 4,
                               current_depth: int = 0): #vers 1
        """Recursively populate tree structure"""
        if current_depth >= max_depth:
            return
            
        try:
            all_items = os.listdir(dir_path)
            
            # Filter hidden files if needed
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
            sort_mode = self.browser_settings.get('sort_mode', 'name')
            if sort_mode == 'name':
                directories.sort(key=str.lower)
                files.sort(key=str.lower)
            
            # Add directories first
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
                
                # Set file type icon
                file_ext = os.path.splitext(file)[1].lower()
                icon = get_file_type_icon_backend(file_ext)
                tree_item.setIcon(0, icon)
                
        except Exception as e:
            pass
            
    def get_file_size_for_sort(self, file_path: str) -> int: #vers 1
        """Get file size for sorting"""
        try:
            return os.path.getsize(file_path)
        except:
            return 0
            
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
            info = f"File: {os.path.basename(file_path)}\n"
            info += f"Path: {file_path}\n"
            info += f"Size: {stats.st_size:,} bytes\n"
            self.info_text.setText(info)
        except Exception as e:
            self.info_text.setText(f"Error: {str(e)}")
            
    def show_context_menu(self, position): #vers 1
        """Show context menu"""
        item = self.tree.itemAt(position)
        if not item:
            return
            
        menu = QMenu(self)
        
        open_action = QAction("Open", self)
        open_action.setIcon(get_folder_icon())
        open_action.triggered.connect(self.open_selected)
        menu.addAction(open_action)
        
        menu.addSeparator()
        
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
        
        menu.addSeparator()
        
        properties_action = QAction("Properties", self)
        properties_action.setIcon(get_properties_icon())
        properties_action.triggered.connect(self.show_file_properties)
        menu.addAction(properties_action)
        
        menu.exec(self.tree.mapToGlobal(position))
        
    # Navigation methods
    def navigate_back(self): #vers 1
        """Navigate back"""
        self.log_message("Navigate back (not implemented)")
        
    def navigate_forward(self): #vers 1
        """Navigate forward"""
        self.log_message("Navigate forward (not implemented)")
        
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
            QMessageBox.warning(self, "No Directory", "Please select a directory first.")
            return
            
        name, ok = QInputDialog.getText(self, "New Folder", "Folder name:")
        if ok and name:
            new_path = os.path.join(self.current_path, name)
            try:
                os.makedirs(new_path, exist_ok=False)
                self.refresh_view()
                self.log_message(f"Created folder: {name}")
            except FileExistsError:
                QMessageBox.warning(self, "Folder Exists", f"A folder named '{name}' already exists.")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to create folder: {str(e)}")
                
    def create_new_file(self): #vers 1
        """Create new file"""
        if not self.current_path:
            QMessageBox.warning(self, "No Directory", "Please select a directory first.")
            return
            
        name, ok = QInputDialog.getText(self, "New File", "File name:")
        if ok and name:
            new_path = os.path.join(self.current_path, name)
            try:
                with open(new_path, 'w') as f:
                    f.write("")
                self.refresh_view()
                self.log_message(f"Created file: {name}")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to create file: {str(e)}")
                
    def cut_files(self): #vers 1
        """Cut selected files"""
        selected_items = self.tree.selectedItems()
        if selected_items:
            self.clipboard_files = [item.data(0, Qt.ItemDataRole.UserRole) for item in selected_items]
            self.clipboard_operation = 'cut'
            self.log_message(f"Cut {len(self.clipboard_files)} item(s)")
            
    def copy_files(self): #vers 1
        """Copy selected files"""
        selected_items = self.tree.selectedItems()
        if selected_items:
            self.clipboard_files = [item.data(0, Qt.ItemDataRole.UserRole) for item in selected_items]
            self.clipboard_operation = 'copy'
            self.log_message(f"Copied {len(self.clipboard_files)} item(s)")
            
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
                    
            self.refresh_view()
            self.log_message(f"Pasted {len(self.clipboard_files)} item(s)")
            
            if self.clipboard_operation == 'cut':
                self.clipboard_files = []
                
        except Exception as e:
            QMessageBox.critical(self, "Paste Error", f"Error pasting files: {str(e)}")
            
    def delete_selected(self): #vers 1
        """Delete selected files"""
        selected_items = self.tree.selectedItems()
        if not selected_items:
            return
            
        reply = QMessageBox.question(
            self, "Delete Files",
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
                        
                self.refresh_view()
                self.log_message(f"Deleted {len(selected_items)} item(s)")
            except Exception as e:
                QMessageBox.critical(self, "Delete Error", f"Error deleting files: {str(e)}")
                
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
                self.refresh_view()
                self.log_message(f"Renamed: {old_name} -> {new_name}")
            except Exception as e:
                QMessageBox.critical(self, "Rename Error", f"Error renaming: {str(e)}")
                
    def open_selected(self): #vers 1
        """Open selected file"""
        selected_items = self.tree.selectedItems()
        if selected_items:
            file_path = selected_items[0].data(0, Qt.ItemDataRole.UserRole)
            if os.path.isfile(file_path):
                self.file_opened.emit(file_path)
            elif os.path.isdir(file_path):
                self.browse_directory(file_path)
                
    def select_all_files(self): #vers 1
        """Select all items"""
        self.tree.selectAll()
        
    def refresh_view(self): #vers 1
        """Refresh current view"""
        if self.current_path:
            self.populate_tree(self.current_path)
            
    def refresh_browser(self): #vers 1
        """Refresh browser"""
        self.refresh_view()
        self.log_message("Browser refreshed")
        
    # Settings methods
    def load_browser_settings(self) -> dict: #vers 1
        """Load browser settings"""
        settings = QSettings("IMG_Factory", "FileBrowser")
        return {
            'show_hidden': settings.value('show_hidden', False, type=bool),
            'show_extensions': settings.value('show_extensions', True, type=bool),
            'view_mode': settings.value('view_mode', 'tree', type=str),
            'sort_mode': settings.value('sort_mode', 'name', type=str),
            'font_size': settings.value('font_size', 11, type=int),
        }
        
    def save_browser_settings(self): #vers 1
        """Save browser settings"""
        settings = QSettings("IMG_Factory", "FileBrowser")
        for key, value in self.browser_settings.items():
            settings.setValue(key, value)
        self.log_message("Browser settings saved")
        
    def show_browser_settings(self): #vers 1
        """Show browser settings dialog"""
        QMessageBox.information(self, "Browser Settings", "Settings dialog not yet implemented.")
        
    def show_default_apps(self): #vers 1
        """Show default applications dialog"""
        QMessageBox.information(self, "Default Applications", "Default applications configuration not yet implemented.")
        
    def show_file_associations(self): #vers 1
        """Show file associations dialog"""
        QMessageBox.information(self, "File Associations", "File associations configuration not yet implemented.")
        
    def reset_browser_settings(self): #vers 1
        """Reset browser settings to defaults"""
        reply = QMessageBox.question(
            self, "Reset Settings",
            "Are you sure you want to reset all browser settings to defaults?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )
        
        if reply == QMessageBox.StandardButton.Yes:
            self.browser_settings = {
                'show_hidden': False,
                'show_extensions': True,
                'view_mode': 'tree',
                'sort_mode': 'name',
                'font_size': 11,
            }
            self.save_browser_settings()
            self.refresh_view()
            self.log_message("Browser settings reset to defaults")
            
    def show_file_properties(self): #vers 1
        """Show file properties"""
        selected_items = self.tree.selectedItems()
        if not selected_items:
            return
            
        file_path = selected_items[0].data(0, Qt.ItemDataRole.UserRole)
        try:
            stats = os.stat(file_path)
            info = f"File: {os.path.basename(file_path)}\n"
            info += f"Full Path: {file_path}\n"
            info += f"Size: {stats.st_size:,} bytes\n"
            QMessageBox.information(self, "File Properties", info)
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Could not get properties: {str(e)}")
            
    def show_file_search_dialog(self): #vers 1
        """Show file search dialog"""
        QMessageBox.information(self, "Search", "Search functionality not yet implemented.")
        
    def open_terminal(self): #vers 1
        """Open terminal in current directory"""
        if not self.current_path:
            return
            
        try:
            system = platform.system()
            if system == "Linux":
                subprocess.Popen(['x-terminal-emulator'], cwd=self.current_path)
            elif system == "Darwin":
                subprocess.Popen(['open', '-a', 'Terminal', self.current_path])
            elif system == "Windows":
                subprocess.Popen(['cmd'], cwd=self.current_path)
        except Exception as e:
            self.log_message(f"Error opening terminal: {str(e)}")
            
    def open_command_prompt(self): #vers 1
        """Open command prompt"""
        self.open_terminal()
        
    def calculate_folder_size(self): #vers 1
        """Calculate folder size"""
        selected_items = self.tree.selectedItems()
        if not selected_items:
            return
            
        file_path = selected_items[0].data(0, Qt.ItemDataRole.UserRole)
        if not os.path.isdir(file_path):
            return
            
        try:
            total_size = 0
            for dirpath, dirnames, filenames in os.walk(file_path):
                for filename in filenames:
                    filepath = os.path.join(dirpath, filename)
                    try:
                        total_size += os.path.getsize(filepath)
                    except:
                        pass
                        
            QMessageBox.information(self, "Folder Size", f"Total size: {total_size:,} bytes")
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Could not calculate size: {str(e)}")
            
    def log_message(self, message: str): #vers 1
        """Log message"""
        print(f"[FileBrowser] {message}")


# Integration function
def create_file_browser_widget(main_window): #vers 1
    """Create file browser widget for integration"""
    try:
        browser_widget = FileBrowserWidget(main_window)
        
        if hasattr(main_window, 'load_file_unified'):
            browser_widget.file_opened.connect(main_window.load_file_unified)
        
        if hasattr(main_window, 'log_message'):
            browser_widget.log_message = main_window.log_message
        
        return browser_widget
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error creating file browser: {str(e)}")
        return None


__all__ = [
    'FileBrowserWidget',
    'create_file_browser_widget'
]
