#this belongs in gui/directory_tree_system.py - Version: 3
# X-Seti - January03 2025 - IMG Factory 1.6 - Interactive Directory Tree System

"""
INTERACTIVE DIRECTORY TREE SYSTEM
Replaces the placeholder "Directory Tree" tab with full functionality.
Parses game directories and provides context-sensitive file operations.
NO EMOJIS - Uses SVG icons from imgfactory_svg_icons.py
"""

import os
from typing import Dict, List, Optional, Tuple
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QTreeWidget, QTreeWidgetItem,
    QToolBar, QPushButton, QLineEdit, QLabel, QMenu, QMessageBox,
    QSplitter, QTextEdit, QGroupBox, QComboBox, QCheckBox
)
from PyQt6.QtCore import Qt, pyqtSignal, QThread, pyqtSlot
from PyQt6.QtGui import QAction, QIcon, QFont

# SVG Icons
from apps.methods.imgfactory_svg_icons import (
    get_folder_icon, get_file_icon, get_img_file_icon,
    get_txd_file_icon, get_col_file_icon, get_refresh_icon,
    get_close_icon, get_checkmark_icon, get_view_icon,
    get_error_icon, get_search_icon, get_settings_icon,
    get_edit_icon, get_image_icon, get_copy_icon,
    get_tree_icon, get_properties_icon
)

##Methods list -
# analyze_directory
# apply_tree_styling
# copy_path_to_clipboard
# create_directory_tree_widget
# create_simple_directory_tree_widget
# get_file_context_actions
# get_file_type_display
# get_file_type_icon
# handle_tree_item_click
# handle_tree_item_double_click
# integrate_directory_tree_system
# is_dark_theme
# open_in_explorer
# parse_game_directory_structure
# populate_directory_tree
# populate_tree
# populate_tree_recursive
# refresh_tree
# setup_connections
# setup_directory_tree_context_menu
# setup_directory_tree_toolbar
# setup_ui
# show_context_menu
# update_directory_stats
# update_directory_tree_info

class DirectoryTreeWidget(QWidget):
    """Interactive directory tree widget for game file navigation"""
    
    # Signals
    file_selected = pyqtSignal(str)
    img_file_requested = pyqtSignal(str)
    text_file_requested = pyqtSignal(str)
    directory_changed = pyqtSignal(str)
    
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.current_root = None
        self.project_folder = None
        self.game_root = None
        self.setup_ui()
        self.setup_connections()
        
    def setup_ui(self): #vers 1
        """Setup directory tree UI"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(5, 5, 5, 5)
        
        # Toolbar
        self.toolbar = self.create_toolbar()
        layout.addWidget(self.toolbar)
        
        # Create splitter for tree and info
        splitter = QSplitter(Qt.Orientation.Horizontal)
        
        # Left: Directory tree
        tree_widget = QWidget()
        tree_layout = QVBoxLayout(tree_widget)
        tree_layout.setContentsMargins(0, 0, 0, 0)
        
        # Tree widget
        self.tree = QTreeWidget()
        self.tree.setHeaderLabel("Directory Structure")
        self.tree.itemClicked.connect(self.handle_tree_item_click)
        self.tree.itemDoubleClicked.connect(self.handle_tree_item_double_click)
        self.tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.tree.customContextMenuRequested.connect(self.show_context_menu)
        
        # Enhanced styling for better visibility
        self.tree.setAlternatingRowColors(True)
        self.tree.setRootIsDecorated(True)
        self.tree.setIndentation(20)
        
        # Apply theme styling
        self.apply_tree_styling()
        
        tree_layout.addWidget(self.tree)
        splitter.addWidget(tree_widget)
        
        # Right: File info panel
        info_widget = QGroupBox("File Information")
        info_layout = QVBoxLayout(info_widget)
        
        self.info_text = QTextEdit()
        self.info_text.setReadOnly(True)
        self.info_text.setMaximumHeight(200)
        info_layout.addWidget(self.info_text)
        
        splitter.addWidget(info_widget)
        splitter.setSizes([600, 200])
        
        layout.addWidget(splitter)
        
        # Path label at bottom
        self.path_label = QLabel("No directory loaded")
        self.path_label.setStyleSheet("padding: 5px; background-color: #2a2a2a; color: #ffffff;")
        layout.addWidget(self.path_label)
        
    def setup_connections(self): #vers 1
        """Setup signal connections"""
        pass
        
    def create_toolbar(self): #vers 1
        """Create toolbar for directory navigation"""
        toolbar = QToolBar()
        toolbar.setMovable(False)
        
        # Refresh button
        refresh_action = QAction("Refresh", self)
        refresh_action.setIcon(get_refresh_icon())
        refresh_action.triggered.connect(self.refresh_tree)
        toolbar.addAction(refresh_action)
        
        toolbar.addSeparator()
        
        # Search field
        toolbar.addWidget(QLabel(" Search: "))
        self.search_field = QLineEdit()
        self.search_field.setPlaceholderText("Filter files...")
        self.search_field.setMaximumWidth(200)
        toolbar.addWidget(self.search_field)
        
        return toolbar
        
    def apply_tree_styling(self): #vers 1
        """Apply theme-aware styling to tree"""
        # Detect if dark theme
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
        # Check background color
        bg_color = self.palette().color(self.backgroundRole())
        return bg_color.lightness() < 128
        
    def populate_tree(self, root_path: str): #vers 1
        """Populate tree with directory structure"""
        try:
            self.tree.clear()
            self.current_root = root_path
            
            # Create root item
            root_item = QTreeWidgetItem(self.tree)
            root_item.setText(0, os.path.basename(root_path) or root_path)
            root_item.setData(0, Qt.ItemDataRole.UserRole, root_path)
            root_item.setIcon(0, get_folder_icon())
            
            # Populate recursively
            self.populate_tree_recursive(root_item, root_path)
            
            # Expand root
            root_item.setExpanded(True)
            
            # Update path label
            self.path_label.setText(f"Root: {root_path}")
            
            # Update statistics
            self.update_directory_stats(root_path)
            
            self.log_message(f"Directory tree populated: {root_path}")
            
        except Exception as e:
            self.log_message(f"Error populating tree: {str(e)}")
            
    def populate_tree_recursive(self, parent_item: QTreeWidgetItem, 
                               dir_path: str, max_depth: int = 2, 
                               current_depth: int = 0): #vers 1
        """Recursively populate tree structure"""
        if current_depth >= max_depth:
            return
            
        try:
            for item in sorted(os.listdir(dir_path)):
                item_path = os.path.join(dir_path, item)
                
                tree_item = QTreeWidgetItem(parent_item)
                tree_item.setText(0, item)
                tree_item.setData(0, Qt.ItemDataRole.UserRole, item_path)
                
                if os.path.isdir(item_path):
                    # Directory
                    tree_item.setIcon(0, get_folder_icon())
                    
                    # Recursively add subdirectories
                    self.populate_tree_recursive(tree_item, item_path, 
                                               max_depth, current_depth + 1)
                else:
                    # File
                    file_ext = os.path.splitext(item)[1].lower()
                    icon = self.get_file_type_icon(file_ext)
                    tree_item.setIcon(0, icon)
                    
        except PermissionError:
            pass
        except Exception as e:
            self.log_message(f"Error reading directory: {str(e)}")
            
    def get_file_type_icon(self, file_ext: str) -> QIcon: #vers 2
        """Get SVG icon for file type"""
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
            '.log': get_view_icon
        }
        icon_func = icon_map.get(file_ext, get_file_icon)
        return icon_func()
        
    def get_file_type_display(self, file_ext: str) -> str: #vers 1
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
            '.log': 'Log File'
        }
        return type_map.get(file_ext, 'File')
        
    def handle_tree_item_click(self, item: QTreeWidgetItem, column: int): #vers 1
        """Handle tree item click"""
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        
        if os.path.isfile(file_path):
            # Update info panel
            file_size = os.path.getsize(file_path)
            file_ext = os.path.splitext(file_path)[1]
            file_type = self.get_file_type_display(file_ext)
            
            info = f"File: {os.path.basename(file_path)}"
            info += f"Path: {file_path}"
            info += f"Type: {file_type}"
            info += f"Size: {file_size:,} bytes"
            
            self.info_text.setText(info)
            self.file_selected.emit(file_path)
            
    def handle_tree_item_double_click(self, item: QTreeWidgetItem, column: int): #vers 1
        """Handle tree item double-click"""
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        
        if os.path.isfile(file_path):
            file_ext = os.path.splitext(file_path)[1].lower()
            
            if file_ext == '.img':
                self.img_file_requested.emit(file_path)
            else:
                self.text_file_requested.emit(file_path)
                
    def show_context_menu(self, position): #vers 1
        """Show context menu for selected item"""
        item = self.tree.itemAt(position)
        if not item:
            return
            
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        
        menu = QMenu(self)
        
        # Open in explorer
        explore_action = QAction("Open in Explorer", self)
        explore_action.setIcon(get_folder_icon())
        explore_action.triggered.connect(lambda: self.open_in_explorer(file_path))
        menu.addAction(explore_action)
        
        # Copy path
        copy_path_action = QAction("Copy Path", self)
        copy_path_action.setIcon(get_copy_icon())
        copy_path_action.triggered.connect(lambda: self.copy_path_to_clipboard(file_path))
        menu.addAction(copy_path_action)
        
        menu.addSeparator()
        
        # Properties
        props_action = QAction("Properties", self)
        props_action.setIcon(get_properties_icon())
        props_action.triggered.connect(lambda: self.show_file_properties(file_path))
        menu.addAction(props_action)
        
        menu.exec(self.tree.mapToGlobal(position))
        
    def open_in_explorer(self, file_path: str): #vers 1
        """Open file path in system explorer"""
        try:
            import subprocess
            import platform
            
            system = platform.system()
            if system == "Windows":
                subprocess.run(["explorer", "/select,", file_path])
            elif system == "Darwin":
                subprocess.run(["open", "-R", file_path])
            else:
                subprocess.run(["xdg-open", os.path.dirname(file_path)])
                
        except Exception as e:
            self.log_message(f"Error opening explorer: {str(e)}")
            
    def copy_path_to_clipboard(self, file_path: str): #vers 1
        """Copy file path to clipboard"""
        try:
            from PyQt6.QtWidgets import QApplication
            clipboard = QApplication.clipboard()
            clipboard.setText(file_path)
            self.log_message(f"Path copied: {file_path}")
        except Exception as e:
            self.log_message(f"Error copying path: {str(e)}")
            

    def show_file_properties(self, file_path: str): #vers 1
        """Show file properties dialog"""
        try:
            stats = os.stat(file_path)
            info = f"File: {os.path.basename(file_path)}\n"
            info += f"Full Path: {file_path}\n"
            info += f"Size: {stats.st_size:,} bytes\n"

            QMessageBox.information(self, "File Properties", info)
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Could not get properties: {str(e)}")


    def refresh_tree(self): #vers 1
        """Refresh the directory tree"""
        if self.current_root:
            self.populate_tree(self.current_root)
            self.log_message("Directory tree refreshed")
        else:
            self.log_message("No directory to refresh")
            
    def update_directory_stats(self, root_path: str): #vers 1
        """Update directory statistics"""
        try:
            file_count = 0
            dir_count = 0

            for root, dirs, files in os.walk(root_path):
                file_count += len(files)
                dir_count += len(dirs)

            stats = f"Directories: {dir_count} | Files: {file_count}"
            self.log_message(stats)

        except Exception as e:
            self.log_message(f"Error calculating stats: {str(e)}")


def create_directory_tree_widget(main_window): #vers 4
    """Create comprehensive file browser widget for main window"""
    try:
        # Import the new comprehensive file browser
        from apps.components.File_Editor.file_dirtree_browser import create_file_browser_widget
        
        # Create the comprehensive file browser instead of simple tree
        browser_widget = create_file_browser_widget(main_window)
        
        if not browser_widget:
            main_window.log_message("Failed to create file browser widget")
            return None
        
        # Connect signals to main window
        if hasattr(main_window, 'load_file_unified'):
            browser_widget.file_opened.connect(main_window.load_file_unified)
        elif hasattr(main_window, 'open_img_file'):
            browser_widget.file_opened.connect(main_window.open_img_file)
        else:
            # Fallback: create a wrapper function
            def load_file_wrapper(file_path):
                try:
                    main_window.log_message(f"Loading file from browser: {file_path}")
                    if hasattr(main_window, '_load_img_file_in_new_tab'):
                        main_window._load_img_file_in_new_tab(file_path)
                    elif hasattr(main_window, 'load_file_unified'):
                        main_window.load_file_unified(file_path)
                    else:
                        main_window.log_message(f"No suitable file loading method found")
                except Exception as e:
                    main_window.log_message(f"Error loading file from browser: {str(e)}")
            
            browser_widget.file_opened.connect(load_file_wrapper)
        
        # Connect directory change signal
        browser_widget.directory_changed.connect(
            lambda path: main_window.log_message(f"Directory changed: {path}")
        )
        
        # Set up initial directory if game root is available
        if hasattr(main_window, 'game_root') and main_window.game_root:
            browser_widget.browse_directory(main_window.game_root)
        
        main_window.log_message("Comprehensive file browser widget created")
        return browser_widget
        
    except ImportError as e:
        main_window.log_message(f"File browser import failed: {str(e)}")
        main_window.log_message("Falling back to simple directory tree")
        
        # Fallback to simple tree if comprehensive browser fails
        return create_simple_directory_tree_widget(main_window)
        
    except Exception as e:
        main_window.log_message(f"Error creating file browser: {str(e)}")
        return None


def create_simple_directory_tree_widget(main_window): #vers 2
    """Create simple directory tree widget as fallback"""
    try:
        tree_widget = DirectoryTreeWidget(main_window)
        
        # Connect signals to main window
        if hasattr(main_window, 'load_file_unified'):
            tree_widget.img_file_requested.connect(main_window.load_file_unified)
        elif hasattr(main_window, 'open_img_file'):
            tree_widget.img_file_requested.connect(main_window.open_img_file)
        else:
            # Fallback: create a wrapper function
            def load_file_wrapper(file_path):
                try:
                    main_window.log_message(f"Loading file from directory tree: {file_path}")
                    if hasattr(main_window, '_load_img_file_in_new_tab'):
                        main_window._load_img_file_in_new_tab(file_path)
                    elif hasattr(main_window, 'load_file_unified'):
                        main_window.load_file_unified(file_path)
                    else:
                        main_window.log_message(f"No suitable file loading method found")
                except Exception as e:
                    main_window.log_message(f"Error loading file from directory tree: {str(e)}")
            
            tree_widget.img_file_requested.connect(load_file_wrapper)
        
        # Connect text file signal (if needed)
        if hasattr(main_window, 'open_text_file'):
            tree_widget.text_file_requested.connect(main_window.open_text_file)
        
        tree_widget.log_message = main_window.log_message
        
        main_window.log_message("Simple directory tree widget created (fallback)")
        return tree_widget
        
    except Exception as e:
        main_window.log_message(f"Error creating simple directory tree: {str(e)}")
        return None


def integrate_directory_tree_system(main_window): #vers 4
    """Replace placeholder with functional directory tree - PRESERVE TAB STRUCTURE"""
    try:
        # Find the directory tree tab in gui_layout
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'tab_widget'):
            main_window.log_message("Tab widget not found for directory tree integration")
            return False

        tab_widget = main_window.gui_layout.tab_widget

        # Find the "Directory Tree" tab (should be index 1)
        directory_tab_index = -1

        # Add None check before counting
        if tab_widget and hasattr(tab_widget, 'count'):
            for i in range(tab_widget.count()):
                if "Directory Tree" in tab_widget.tabText(i):
                    directory_tab_index = i
                    break
        else:
            main_window.log_message("Tab widget is None or has no count method")
            return False

        if directory_tab_index == -1:
            main_window.log_message("Directory Tree tab not found")
            return False

        # Get the existing tab widget at this index (to replace its contents)
        existing_tab = tab_widget.widget(directory_tab_index)
        if not existing_tab:
            main_window.log_message("Directory Tree tab widget not found")
            return False

        # Clear the existing tab contents but keep the tab
        existing_layout = existing_tab.layout()
        if existing_layout:
            # Remove all widgets from existing layout
            while existing_layout.count():
                child = existing_layout.takeAt(0)
                if child.widget():
                    child.widget().deleteLater()
        else:
            # Create new layout if none exists
            from PyQt6.QtWidgets import QVBoxLayout
            existing_layout = QVBoxLayout(existing_tab)

        # Create new functional directory tree
        directory_tree = create_directory_tree_widget(main_window)
        if not directory_tree:
            return False

        # Add the directory tree to the existing tab (replace placeholder)
        existing_layout.addWidget(directory_tree)

        # Update the tab text and icon
        tab_widget.setTabText(directory_tab_index, "Directory Tree")
        tab_widget.setTabIcon(directory_tab_index, get_tree_icon())

        # Store reference for later use
        main_window.directory_tree = directory_tree

        main_window.log_message("Directory tree system integrated (tab structure preserved)")
        main_window.log_message("Use 'Set Game Root' in File menu to browse GTA directories")

        return True

    except Exception as e:
        main_window.log_message(f"Error integrating directory tree: {str(e)}")
        return False


    def update_directory_tree_info(main_window): #vers 1
        """Update directory tree info when game root is set"""
        try:
            if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'tab_widget'):
                return False

            tab_widget = main_window.gui_layout.tab_widget
            directory_tab = tab_widget.widget(1)

            if directory_tab:
                text_widgets = directory_tab.findChildren(QTextEdit)
                if text_widgets:
                    text_widget = text_widgets[0]
                    if hasattr(main_window, 'game_root'):
                        text_widget.setText(f"Game Root: {main_window.game_root}\n\nDetected: GTA SA installation\n\nDirectory tree functionality will be implemented here.\n\nFor now, you can:\n• Load IMG files via File → Open\n• Use the project folder system for exports")
                    else:
                        text_widget.setText("No game root set.\n\nUse File → Set Game Root Folder to select your GTA installation.")

            return True

        except Exception as e:
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Error updating directory tree: {str(e)}")
            return False

__all__ = [
    'DirectoryTreeWidget',
    'create_directory_tree_widget',
    'integrate_directory_tree_system',
    'update_directory_tree_info'
]
