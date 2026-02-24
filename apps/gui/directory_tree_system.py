#this belongs in gui/directory_tree_system.py - Version: 2
# X-Seti - August10 2025 - IMG Factory 1.5 - Interactive Directory Tree System

"""
INTERACTIVE DIRECTORY TREE SYSTEM
Replaces the placeholder "Directory Tree" tab with full functionality.
Parses game directories and provides context-sensitive file operations.
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

##Methods list -
# analyze_directory
# apply_tree_styling
# copy_path_to_clipboard
# create_directory_tree_widget
# get_file_context_actions
# get_file_type_icon
# handle_tree_item_click
# handle_tree_item_double_click
# integrate_directory_tree_system
# is_dark_theme
# open_in_explorer
# parse_game_directory_structure
# populate_directory_tree
# refresh_tree
# setup_directory_tree_context_menu
# setup_directory_tree_toolbar
# show_context_menu
# update_directory_tree_info

class DirectoryTreeWidget(QWidget):
    """Interactive directory tree widget for game file navigation"""
    
    # Signals
    file_selected = pyqtSignal(str)  # file_path
    img_file_requested = pyqtSignal(str)  # img_path
    text_file_requested = pyqtSignal(str)  # text_path
    directory_changed = pyqtSignal(str)  # directory_path
    
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
        
        # Apply theme-aware styling
        self.apply_tree_styling()
        
        tree_layout.addWidget(self.tree)
        
        splitter.addWidget(tree_widget)
        
        # Right: Information panel
        info_widget = QWidget()
        info_layout = QVBoxLayout(info_widget)
        info_layout.setContentsMargins(5, 5, 5, 5)
        
        # Selected file info
        self.selected_file_label = QLabel("No file selected")
        self.selected_file_label.setFont(QFont("", 10, QFont.Weight.Bold))
        info_layout.addWidget(self.selected_file_label)
        
        self.file_path_label = QLabel("")
        self.file_path_label.setWordWrap(True)
        info_layout.addWidget(self.file_path_label)
        
        self.file_type_label = QLabel("")
        info_layout.addWidget(self.file_type_label)
        
        self.file_size_label = QLabel("")
        info_layout.addWidget(self.file_size_label)
        
        # Action buttons
        button_layout = QVBoxLayout()
        
        self.load_img_btn = QPushButton("Load IMG")
        self.load_img_btn.setEnabled(False)
        self.load_img_btn.clicked.connect(self.load_selected_img)
        button_layout.addWidget(self.load_img_btn)
        
        self.edit_text_btn = QPushButton("📝 Edit Text")
        self.edit_text_btn.setEnabled(False)
        self.edit_text_btn.clicked.connect(self.edit_selected_text)
        button_layout.addWidget(self.edit_text_btn)
        
        self.explore_btn = QPushButton("🗂️ Explore")
        self.explore_btn.setEnabled(False)
        self.explore_btn.clicked.connect(self.explore_selected)
        button_layout.addWidget(self.explore_btn)
        
        info_layout.addLayout(button_layout)
        
        # Directory statistics
        self.stats_label = QLabel("Directory statistics will appear here")
        self.stats_label.setWordWrap(True)
        self.stats_label.setStyleSheet("font-size: 10px; color: #666;")
        info_layout.addWidget(self.stats_label)
        
        info_layout.addStretch()
        splitter.addWidget(info_widget)
        
        # Set splitter proportions (tree 70%, info 30%)
        splitter.setSizes([700, 300])
        
    def apply_tree_styling(self): #vers 2
        """Apply theme-aware styling using IMG Factory theme system"""
        try:
            # Get theme colors from main window's app_settings
            theme_colors = self.get_theme_colors()
            
            if not theme_colors:
                # Fallback to basic styling if no theme available
                self.apply_fallback_styling()
                return
            
            # Use theme colors for styling
            tree_style = f"""
                QTreeWidget {{
                    background-color: {theme_colors.get('bg_secondary', '#ffffff')};
                    color: {theme_colors.get('text_primary', '#333333')};
                    font-size: 13px;
                    font-family: 'Segoe UI', Arial, sans-serif;
                    border: 1px solid {theme_colors.get('border', '#cccccc')};
                    outline: none;
                    selection-background-color: {theme_colors.get('accent_primary', '#FFECEE')};
                    alternate-background-color: {theme_colors.get('bg_tertiary', '#f0f0f0')};
                }}
                QTreeWidget::item {{
                    padding: 4px;
                    border: none;
                    color: {theme_colors.get('text_primary', '#333333')};
                    min-height: 20px;
                }}
                QTreeWidget::item:hover {{
                    background-color: {theme_colors.get('button_hover', '#e8e8e8')};
                    color: {theme_colors.get('text_primary', '#333333')};
                }}
                QTreeWidget::item:selected {{
                    background-color: {theme_colors.get('accent_primary', '#FFECEE')};
                    color: #ffffff;
                }}
                QTreeWidget::item:selected:hover {{
                    background-color: {theme_colors.get('accent_secondary', '#FFECEE')};
                    color: #ffffff;
                }}
                QTreeWidget::branch {{
                    background-color: transparent;
                }}
                QTreeWidget::branch:hover {{
                    background-color: {theme_colors.get('button_hover', '#e8e8e8')};
                }}
                QHeaderView::section {{
                    background-color: {theme_colors.get('panel_bg', '#f5f5f5')};
                    color: {theme_colors.get('text_primary', '#333333')};
                    padding: 6px;
                    border: 1px solid {theme_colors.get('border', '#cccccc')};
                    font-weight: bold;
                }}
            """
            
            self.tree.setStyleSheet(tree_style)
            self.log_message(f"🎨 Applied theme styling to directory tree")
            
        except Exception as e:
            self.log_message(f"❌ Error applying theme styling: {str(e)}")
            self.apply_fallback_styling()
            
    def get_theme_colors(self): #vers 1
        """Get theme colors from main window's app_settings"""
        try:
            # Try to get theme colors from parent main window
            main_window = self.get_main_window()
            if main_window and hasattr(main_window, 'app_settings'):
                app_settings = main_window.app_settings
                
                # Use the app_settings method to get current theme colors
                if hasattr(app_settings, 'get_theme_colors'):
                    return app_settings.get_theme_colors()
                elif hasattr(app_settings, 'get_theme'):
                    theme = app_settings.get_theme()
                    return theme.get('colors', {})
                    
            return {}
            
        except Exception as e:
            self.log_message(f"❌ Error getting theme colors: {str(e)}")
            return {}
            
    def get_main_window(self): #vers 1
        """Get the main window from parent hierarchy"""
        try:
            parent = self.parent()
            while parent:
                # Look for IMGFactory main window
                if hasattr(parent, 'app_settings') or 'IMGFactory' in str(type(parent)):
                    return parent
                parent = parent.parent()
            return None
        except:
            return None
            
    def apply_fallback_styling(self): #vers 1
        """Apply fallback styling when theme system is not available"""
        try:
            # Simple fallback styling
            fallback_style = """
                QTreeWidget {
                    background-color: #fafafa;
                    color: #333333;
                    font-size: 13px;
                    font-family: 'Segoe UI', Arial, sans-serif;
                    border: 1px solid #cccccc;
                    outline: none;
                    selection-background-color: #FFECEE;
                    alternate-background-color: #f0f0f0;
                }
                QTreeWidget::item {
                    padding: 4px;
                    border: none;
                    min-height: 20px;
                }
                QTreeWidget::item:hover {
                    background-color: #e8e8e8;
                }
                QTreeWidget::item:selected {
                    background-color: #FFECEE;
                    color: #ffffff;
                }
                QHeaderView::section {
                    background-color: #f5f5f5;
                    color: #333333;
                    padding: 6px;
                    border: 1px solid #cccccc;
                    font-weight: bold;
                }
            """
            
            self.tree.setStyleSheet(fallback_style)
            self.log_message("🎨 Applied fallback styling to directory tree")
            
        except Exception as e:
            self.log_message(f"❌ Error applying fallback styling: {str(e)}")
        
    def setup_connections(self): #vers 1
        """Setup signal connections"""
        pass
        
    def create_toolbar(self): #vers 1
        """Create directory tree toolbar"""
        toolbar = QWidget()
        layout = QHBoxLayout(toolbar)
        layout.setContentsMargins(0, 0, 0, 0)
        
        # Game root button
        self.set_root_btn = QPushButton("🎮 Set Game Root")
        self.set_root_btn.clicked.connect(self.set_game_root)
        layout.addWidget(self.set_root_btn)
        
        # Refresh button
        self.refresh_btn = QPushButton("🔄 Refresh")
        self.refresh_btn.clicked.connect(self.refresh_tree)
        layout.addWidget(self.refresh_btn)
        
        # Path display
        self.path_label = QLabel("No root set")
        self.path_label.setStyleSheet("font-style: italic; color: #666;")
        layout.addWidget(self.path_label)
        
        layout.addStretch()
        return toolbar
        
    def set_game_root(self): #vers 1
        """Set game root directory"""
        try:
            from PyQt6.QtWidgets import QFileDialog
            directory = QFileDialog.getExistingDirectory(
                self, "Select GTA Game Root Directory"
            )
            
            if directory:
                self.game_root = directory
                self.current_root = directory
                self.path_label.setText(f"Root: {directory}")
                self.populate_tree(directory)
                self.log_message(f"🎮 Game root set: {directory}")
                
        except Exception as e:
            self.log_message(f"❌ Error setting game root: {str(e)}")
            
    def populate_tree(self, root_path: str): #vers 1
        """Populate directory tree"""
        try:
            self.tree.clear()
            
            if not os.path.exists(root_path):
                self.log_message(f"❌ Directory does not exist: {root_path}")
                return
                
            # Create root item
            root_item = QTreeWidgetItem(self.tree)
            root_item.setText(0, os.path.basename(root_path) or root_path)
            root_item.setData(0, Qt.ItemDataRole.UserRole, root_path)
            
            # Populate children
            self.populate_tree_recursive(root_item, root_path, max_depth=3)
            
            # Expand root
            root_item.setExpanded(True)
            
            # Update stats
            self.update_directory_stats(root_path)
            
            self.log_message(f"🌳 Directory tree populated: {root_path}")
            
        except Exception as e:
            self.log_message(f"❌ Error populating tree: {str(e)}")
            
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
                    # Directory icon
                    tree_item.setText(0, f"📁 {item}")
                    
                    # Recursively add subdirectories
                    self.populate_tree_recursive(tree_item, item_path, 
                                               max_depth, current_depth + 1)
                else:
                    # File icon based on extension
                    file_ext = os.path.splitext(item)[1].lower()
                    icon = self.get_file_type_icon(file_ext)
                    tree_item.setText(0, f"{icon} {item}")
                    
        except PermissionError:
            # Skip directories we can't access
            pass
        except Exception as e:
            self.log_message(f"❌ Error reading directory: {str(e)}")
            
    def get_file_type_icon(self, file_ext: str) -> str: #vers 1
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
            '.log': '📊'
        }
        return icon_map.get(file_ext, '📄')
        
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
        return type_map.get(file_ext, 'Unknown File')
        
    def format_file_size(self, size: int) -> str: #vers 1
        """Format file size for display"""
        for unit in ['B', 'KB', 'MB', 'GB']:
            if size < 1024:
                return f"{size:.1f} {unit}"
            size /= 1024
        return f"{size:.1f} TB"
        
    def handle_tree_item_click(self, item: QTreeWidgetItem, column: int): #vers 1
        """Handle tree item click"""
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if file_path:
            self.file_selected.emit(file_path)
            self.update_file_info(file_path)
            
    def handle_tree_item_double_click(self, item: QTreeWidgetItem, column: int): #vers 1
        """Handle tree item double click"""
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if file_path and os.path.isfile(file_path):
            file_ext = os.path.splitext(file_path)[1].lower()
            
            if file_ext == '.img':
                self.img_file_requested.emit(file_path)
            elif file_ext in ['.ide', '.ipl', '.dat', '.txt']:
                self.text_file_requested.emit(file_path)
                
    def load_selected_img(self): #vers 1
        """Load selected IMG file"""
        try:
            selection = self.tree.currentItem()
            if selection:
                file_path = selection.data(0, Qt.ItemDataRole.UserRole)
                if file_path and file_path.lower().endswith('.img'):
                    self.img_file_requested.emit(file_path)
                    
        except Exception as e:
            self.log_message(f"❌ Error loading IMG: {str(e)}")
            
    def edit_selected_text(self): #vers 1
        """Edit selected text file"""
        try:
            selection = self.tree.currentItem()
            if selection:
                file_path = selection.data(0, Qt.ItemDataRole.UserRole)
                if file_path:
                    file_ext = os.path.splitext(file_path)[1].lower()
                    if file_ext in ['.ide', '.ipl', '.dat', '.txt']:
                        self.text_file_requested.emit(file_path)
                        
        except Exception as e:
            self.log_message(f"❌ Error editing text: {str(e)}")
            
    def explore_selected(self): #vers 1
        """Open selected path in system explorer"""
        try:
            selection = self.tree.currentItem()
            if selection:
                file_path = selection.data(0, Qt.ItemDataRole.UserRole)
                if file_path:
                    self.open_in_explorer(file_path)
                    
        except Exception as e:
            self.log_message(f"❌ Error exploring: {str(e)}")
            
    def update_file_info(self, file_path: str): #vers 1
        """Update file information display"""
        try:
            self.selected_file_label.setText(f"{os.path.basename(file_path)}")
            self.file_path_label.setText(f"Path: {file_path}")
            
            if os.path.isfile(file_path):
                # File info
                size = os.path.getsize(file_path)
                self.file_size_label.setText(f"Size: {self.format_file_size(size)}")
                
                file_ext = os.path.splitext(file_path)[1].lower()
                self.file_type_label.setText(f"Type: {self.get_file_type_display(file_ext)}")
                
                # Enable appropriate buttons
                self.load_img_btn.setEnabled(file_ext == '.img')
                self.edit_text_btn.setEnabled(file_ext in ['.ide', '.ipl', '.dat'])
                self.explore_btn.setEnabled(True)
                
            else:
                # Directory info
                self.file_type_label.setText("Type: 📁 Directory")
                self.file_size_label.setText("Size: Directory")
                
                self.load_img_btn.setEnabled(False)
                self.edit_text_btn.setEnabled(False)
                self.explore_btn.setEnabled(True)
                
        except Exception as e:
            self.log_message(f"❌ Error updating file info: {str(e)}")
            
    def update_directory_stats(self, dir_path: str): #vers 1
        """Update directory statistics"""
        try:
            stats = self.analyze_directory(dir_path)
            
            stats_text = f"""
📊 Directory Analysis:
• IMG Files: {stats['img_files']}
• Text Files: {stats['text_files']} (IDE: {stats['ide_files']}, IPL: {stats['ipl_files']}, DAT: {stats['dat_files']})
• Model Files: {stats['dff_files']} DFF
• Texture Files: {stats['txd_files']} TXD
• Collision Files: {stats['col_files']} COL
• Total Files: {stats['total_files']}
• Total Size: {self.format_file_size(stats['total_size'])}
            """.strip()
            
            self.stats_label.setText(stats_text)
            
        except Exception as e:
            self.stats_label.setText(f"Error analyzing directory: {str(e)}")
            
    def analyze_directory(self, dir_path: str) -> Dict[str, int]: #vers 1
        """Analyze directory contents"""
        stats = {
            'img_files': 0, 'ide_files': 0, 'ipl_files': 0, 'dat_files': 0,
            'dff_files': 0, 'txd_files': 0, 'col_files': 0, 'text_files': 0,
            'total_files': 0, 'total_size': 0
        }
        
        try:
            for root, dirs, files in os.walk(dir_path):
                for file in files:
                    file_path = os.path.join(root, file)
                    file_ext = os.path.splitext(file)[1].lower()
                    
                    stats['total_files'] += 1
                    
                    try:
                        stats['total_size'] += os.path.getsize(file_path)
                    except:
                        pass
                    
                    if file_ext == '.img':
                        stats['img_files'] += 1
                    elif file_ext == '.ide':
                        stats['ide_files'] += 1
                        stats['text_files'] += 1
                    elif file_ext == '.ipl':
                        stats['ipl_files'] += 1
                        stats['text_files'] += 1
                    elif file_ext == '.dat':
                        stats['dat_files'] += 1
                        stats['text_files'] += 1
                    elif file_ext == '.dff':
                        stats['dff_files'] += 1
                    elif file_ext == '.txd':
                        stats['txd_files'] += 1
                    elif file_ext == '.col':
                        stats['col_files'] += 1
                        
        except Exception as e:
            self.log_message(f"❌ Error analyzing directory: {str(e)}")
            
        return stats
        
    def show_context_menu(self, position): #vers 1
        """Show context menu for tree item"""
        item = self.tree.itemAt(position)
        if not item:
            return
            
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if not file_path:
            return
            
        menu = QMenu(self)
        
        if os.path.isfile(file_path):
            file_ext = os.path.splitext(file_path)[1].lower()
            
            if file_ext == '.img':
                load_action = menu.addAction("Load IMG File")
                load_action.triggered.connect(lambda: self.img_file_requested.emit(file_path))
                
            elif file_ext in ['.ide', '.ipl', '.dat', '.txt']:
                edit_action = menu.addAction("📝 Edit Text File")
                edit_action.triggered.connect(lambda: self.text_file_requested.emit(file_path))
                
        # Common actions
        menu.addSeparator()
        
        explore_action = menu.addAction("🗂️ Open in Explorer")
        explore_action.triggered.connect(lambda: self.open_in_explorer(file_path))
        
        copy_path_action = menu.addAction("📋 Copy Path")
        copy_path_action.triggered.connect(lambda: self.copy_path_to_clipboard(file_path))
        
        menu.exec(self.tree.mapToGlobal(position))
        
    def open_in_explorer(self, file_path: str): #vers 1
        """Open file path in system explorer"""
        try:
            import subprocess
            import platform
            
            system = platform.system()
            if system == "Windows":
                subprocess.run(["explorer", "/select,", file_path])
            elif system == "Darwin":  # macOS
                subprocess.run(["open", "-R", file_path])
            else:  # Linux
                subprocess.run(["xdg-open", os.path.dirname(file_path)])
                
        except Exception as e:
            self.log_message(f"❌ Error opening explorer: {str(e)}")
            
    def copy_path_to_clipboard(self, file_path: str): #vers 1
        """Copy file path to clipboard"""
        try:
            from PyQt6.QtWidgets import QApplication
            clipboard = QApplication.clipboard()
            clipboard.setText(file_path)
            self.log_message(f"📋 Path copied: {file_path}")
        except Exception as e:
            self.log_message(f"❌ Error copying path: {str(e)}")
            
    def refresh_tree(self): #vers 1
        """Refresh the directory tree"""
        if self.current_root:
            self.populate_tree(self.current_root)
            self.log_message("🔄 Directory tree refreshed")
        else:
            self.log_message("⚠️ No directory to refresh")
            
    def log_message(self, message: str): #vers 1
        """Log message (to be connected to main window)"""
        print(f"[DirectoryTree] {message}")


def create_directory_tree_widget(main_window): #vers 3
    """Create comprehensive file browser widget for main window"""
    try:
        # Import the new comprehensive file browser
        from apps.core.file_dirtree_browser import create_file_browser_widget
        
        # Create the comprehensive file browser instead of simple tree
        browser_widget = create_file_browser_widget(main_window)
        
        if not browser_widget:
            main_window.log_message("❌ Failed to create file browser widget")
            return None
        
        # Connect signals to main window - FIXED: Use correct method names
        if hasattr(main_window, 'load_file_unified'):
            browser_widget.file_opened.connect(main_window.load_file_unified)
        elif hasattr(main_window, 'open_img_file'):
            browser_widget.file_opened.connect(main_window.open_img_file)
        else:
            # Fallback: create a wrapper function
            def load_file_wrapper(file_path):
                try:
                    main_window.log_message(f"Loading file from browser: {file_path}")
                    # Try different loading methods
                    if hasattr(main_window, '_load_img_file_in_new_tab'):
                        main_window._load_img_file_in_new_tab(file_path)
                    elif hasattr(main_window, 'load_file_unified'):
                        main_window.load_file_unified(file_path)
                    else:
                        main_window.log_message(f"❌ No suitable file loading method found")
                except Exception as e:
                    main_window.log_message(f"❌ Error loading file from browser: {str(e)}")
            
            browser_widget.file_opened.connect(load_file_wrapper)
        
        # Connect directory change signal
        browser_widget.directory_changed.connect(
            lambda path: main_window.log_message(f"📁 Directory changed: {path}")
        )
        
        # Set up initial directory if game root is available
        if hasattr(main_window, 'game_root') and main_window.game_root:
            browser_widget.browse_directory(main_window.game_root)
        
        main_window.log_message("✅ Comprehensive file browser widget created")
        return browser_widget
        
    except ImportError as e:
        main_window.log_message(f"❌ File browser import failed: {str(e)}")
        main_window.log_message("⚠️ Falling back to simple directory tree")
        
        # Fallback to simple tree if comprehensive browser fails
        return create_simple_directory_tree_widget(main_window)
        
    except Exception as e:
        main_window.log_message(f"❌ Error creating file browser: {str(e)}")
        return None


def create_simple_directory_tree_widget(main_window): #vers 1
    """Create simple directory tree widget as fallback"""
    try:
        tree_widget = DirectoryTreeWidget(main_window)
        
        # Connect signals to main window - FIXED: Use correct method names
        if hasattr(main_window, 'load_file_unified'):
            tree_widget.img_file_requested.connect(main_window.load_file_unified)
        elif hasattr(main_window, 'open_img_file'):
            tree_widget.img_file_requested.connect(main_window.open_img_file)
        else:
            # Fallback: create a wrapper function
            def load_file_wrapper(file_path):
                try:
                    main_window.log_message(f"Loading file from directory tree: {file_path}")
                    # Try different loading methods
                    if hasattr(main_window, '_load_img_file_in_new_tab'):
                        main_window._load_img_file_in_new_tab(file_path)
                    elif hasattr(main_window, 'load_file_unified'):
                        main_window.load_file_unified(file_path)
                    else:
                        main_window.log_message(f"❌ No suitable file loading method found")
                except Exception as e:
                    main_window.log_message(f"❌ Error loading file from directory tree: {str(e)}")
            
            tree_widget.img_file_requested.connect(load_file_wrapper)
        
        # Connect text file signal (if needed)
        if hasattr(main_window, 'open_text_file'):
            tree_widget.text_file_requested.connect(main_window.open_text_file)
        
        tree_widget.log_message = main_window.log_message
        
        main_window.log_message("✅ Simple directory tree widget created (fallback)")
        return tree_widget
        
    except Exception as e:
        main_window.log_message(f"❌ Error creating simple directory tree: {str(e)}")
        return None


def integrate_directory_tree_system(main_window): #vers 3
    """Replace placeholder with functional directory tree - PRESERVE TAB STRUCTURE"""
    try:
        # Find the directory tree tab in gui_layout
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'tab_widget'):
            main_window.log_message("❌ Tab widget not found for directory tree integration")
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
            main_window.log_message("❌ Tab widget is None or has no count method")
            return False

        if directory_tab_index == -1:
            main_window.log_message("❌ Directory Tree tab not found")
            return False

        # Get the existing tab widget at this index (to replace its contents)
        existing_tab = tab_widget.widget(directory_tab_index)
        if not existing_tab:
            main_window.log_message("❌ Directory Tree tab widget not found")
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

        # Update the tab text with icon
        tab_widget.setTabText(directory_tab_index, "🌳 Directory Tree")

        # Store reference for later use
        main_window.directory_tree = directory_tree

        main_window.log_message("✅ Directory tree system integrated (tab structure preserved)")
        main_window.log_message("🌳 Use 'Set Game Root' in File menu to browse GTA directories")

        return True

    except Exception as e:
        main_window.log_message(f"❌ Error integrating directory tree: {str(e)}")
        return False


def update_directory_tree_info(main_window): #vers 1
    """Update directory tree info when game root is set"""
    try:
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'tab_widget'):
            return False
            
        tab_widget = main_window.gui_layout.tab_widget
        directory_tab = tab_widget.widget(1)  # Directory tree is tab 1
        
        if directory_tab:
            # Find the text widget and update it
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
            main_window.log_message(f"❌ Error updating directory tree: {str(e)}")
        return False


__all__ = [
    'DirectoryTreeWidget',
    'create_directory_tree_widget',
    'integrate_directory_tree_system',
    'update_directory_tree_info'
]
