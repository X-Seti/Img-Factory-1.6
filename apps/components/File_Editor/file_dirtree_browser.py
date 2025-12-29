#this belongs in core/file_dirtree_browser.py - Version: 2
# X-Seti - August10 2025 - IMG Factory 1.5 - Complete File Directory Tree Browser

"""
COMPLETE FILE DIRECTORY TREE BROWSER
Comprehensive file browser with Edit, View, and Settings menus
Similar to Caja/Dolphin file managers but integrated with IMG Factory
Backend classes moved to file_dirtree_backend.py for size optimization
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
from apps.core.file_dirtree_backend import (
    BrowserSettingsDialog, FilePropertiesDialog, FileSearchDialog, format_file_size_backend, get_file_type_icon_backend, get_file_type_display_backend, get_file_attributes_backend, get_folder_size_quick_backend
)

##Methods list -
# apply_browser_styling
# apply_settings
# browse_directory
# calculate_folder_size
# copy_files
# create_browser_settings_dialog
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
# setup_view_menu
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
# FilePropertiesDialog
# FileSearchDialog

class FileBrowserWidget(QWidget):
    """Complete file browser widget with full menu system"""
    
    # Signals
    file_selected = pyqtSignal(str)
    file_opened = pyqtSignal(str)
    directory_changed = pyqtSignal(str)
    
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.current_path = None
        self.clipboard_files = []
        self.clipboard_operation = None  # 'copy' or 'cut'
        self.browser_settings = self.load_browser_settings()
        self.setup_ui()
        self.setup_connections()
        
    def setup_ui(self): #vers 1
        """Setup complete file browser UI"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        
        # Menu bar
        self.menubar = self.create_menubar()
        layout.addWidget(self.menubar)
        
        # Toolbar
        self.toolbar = self.create_toolbar()
        layout.addWidget(self.toolbar)
        
        # Address bar
        address_layout = QHBoxLayout()
        address_layout.addWidget(QLabel("📁 Location:"))
        
        font = QFont()
        font.setPointSize(9)  # Set font size to 9pt

        self.address_bar = QLineEdit()
        self.address_bar.setPlaceholderText("Enter path or browse...")
        self.address_bar.returnPressed.connect(self.navigate_to_address)
        address_layout.addWidget(self.address_bar)
        
        go_btn = QPushButton("Go")
        go_btn.clicked.connect(self.navigate_to_address)
        go_btn.setMaximumHeight(25)
        go_btn.setFont(font)
        address_layout.addWidget(go_btn)
        
        layout.addLayout(address_layout)
        
        # Main browser area
        splitter = QSplitter(Qt.Orientation.Horizontal)
        
        # Left: Directory tree
        self.tree = QTreeWidget()
        self.tree.setHeaderLabel("Directory Structure")
        self.setup_tree_view()
        
        # Apply theme styling
        self.apply_browser_styling()
        
        splitter.addWidget(self.tree)
        
        # Right: File info and actions
        info_widget = self.create_info_panel()
        splitter.addWidget(info_widget)
        
        # Set splitter proportions
        splitter.setSizes([710, 200])
        
        layout.addWidget(splitter)

    def update_file_info(self, file_path): #vers 1
        """Update file information panel"""
        try:
            if not file_path or not os.path.exists(file_path):
                return

            # Update file info labels
            file_name = os.path.basename(file_path)
            self.file_name_label.setText(file_name)
            self.file_path_label.setText(file_path)

            # Get file stats
            stat = os.stat(file_path)
            file_size = stat.st_size

            # Update info labels
            if os.path.isfile(file_path):
                file_ext = os.path.splitext(file_path)[1].lower()
                self.file_type_label.setText(get_file_type_display_backend(file_ext))
                self.file_size_label.setText(format_file_size_backend(file_size))
            else:
                self.file_type_label.setText("Folder")
                # Calculate folder size (quick scan)
                folder_size = get_folder_size_quick_backend(file_path)
                self.file_size_label.setText(f"{format_file_size_backend(folder_size)} (quick scan)")

            # Update date
            import datetime
            mod_time = datetime.datetime.fromtimestamp(stat.st_mtime)
            self.file_date_label.setText(mod_time.strftime('%Y-%m-%d %H:%M:%S'))

            # Enable action buttons
            self.open_btn.setEnabled(True)
            self.edit_btn.setEnabled(os.path.isfile(file_path))
            self.copy_btn.setEnabled(True)
            self.delete_btn.setEnabled(True)

        except Exception as e:
            self.log_message(f"❌ Error updating file info: {str(e)}")


    def get_main_window(self): #vers 1
        """Get main window reference"""
        parent = self.parent()
        while parent:
            if hasattr(parent, 'log_message') and hasattr(parent, 'current_img'):
                return parent
            parent = parent.parent()
        return None


    def create_menubar(self): #vers 1
        """Create comprehensive menu bar"""
        menubar = QMenuBar()
        
        # File menu
        file_menu = menubar.addMenu("📁 &File")
        self.setup_file_menu(file_menu)
        
        # Edit menu
        edit_menu = menubar.addMenu("✏️ &Edit")
        self.setup_edit_menu(edit_menu)
        
        # View menu
        view_menu = menubar.addMenu("👁️ &View")
        self.setup_view_menu(view_menu)
        
        # Tools menu
        tools_menu = menubar.addMenu("🔧 &Tools")
        self.setup_tools_menu(tools_menu)
        
        # Settings menu
        settings_menu = menubar.addMenu("⚙️ &Settings")
        self.setup_settings_menu(settings_menu)
        
        return menubar
        
    def setup_file_menu(self, menu): #vers 1
        """Setup File menu"""
        # New folder
        new_folder_action = QAction("📁 New &Folder", self)
        new_folder_action.setShortcut("Ctrl+Shift+N")
        new_folder_action.triggered.connect(self.create_new_folder)
        menu.addAction(new_folder_action)
        
        # New file
        new_file_action = QAction("New &File", self)
        new_file_action.setShortcut("Ctrl+N")
        new_file_action.triggered.connect(self.create_new_file)
        menu.addAction(new_file_action)
        
        menu.addSeparator()
        
        # Open
        open_action = QAction("&Open", self)
        open_action.setShortcut("Enter")
        open_action.triggered.connect(self.open_selected)
        menu.addAction(open_action)
        
        # Open with
        open_with_action = QAction("🔧 Open &With...", self)
        open_with_action.triggered.connect(self.open_with_dialog)
        menu.addAction(open_with_action)
        
        menu.addSeparator()
        
        # Properties
        properties_action = QAction("📋 P&roperties", self)
        properties_action.setShortcut("Alt+Enter")
        properties_action.triggered.connect(self.show_properties)
        menu.addAction(properties_action)
        
    def setup_edit_menu(self, menu): #vers 1
        """Setup Edit menu with all basic file operations"""
        # Cut
        cut_action = QAction("✂️ Cu&t", self)
        cut_action.setShortcut("Ctrl+X")
        cut_action.triggered.connect(self.cut_files)
        menu.addAction(cut_action)
        
        # Copy
        copy_action = QAction("📋 &Copy", self)
        copy_action.setShortcut("Ctrl+C")
        copy_action.triggered.connect(self.copy_files)
        menu.addAction(copy_action)
        
        # Paste
        paste_action = QAction("📌 &Paste", self)
        paste_action.setShortcut("Ctrl+V")
        paste_action.triggered.connect(self.paste_files)
        menu.addAction(paste_action)
        
        menu.addSeparator()
        
        # Rename
        rename_action = QAction("✏️ &Rename", self)
        rename_action.setShortcut("F2")
        rename_action.triggered.connect(self.rename_selected)
        menu.addAction(rename_action)
        
        # Delete
        delete_action = QAction("🗑️ &Delete", self)
        delete_action.setShortcut("Delete")
        delete_action.triggered.connect(self.delete_selected)
        menu.addAction(delete_action)
        
        menu.addSeparator()
        
        # Select all
        select_all_action = QAction("📑 Select &All", self)
        select_all_action.setShortcut("Ctrl+A")
        select_all_action.triggered.connect(self.select_all_files)
        menu.addAction(select_all_action)
        
        # Invert selection
        invert_action = QAction("🔄 &Invert Selection", self)
        invert_action.setShortcut("Ctrl+I")
        invert_action.triggered.connect(self.invert_selection)
        menu.addAction(invert_action)
        
    def setup_view_menu(self, menu): #vers 1
        """Setup View menu with display options"""
        # View modes
        view_group = QActionGroup(self)
        
        tree_view_action = QAction("🌳 &Tree View", self)
        tree_view_action.setCheckable(True)
        tree_view_action.setChecked(True)
        tree_view_action.triggered.connect(lambda: self.set_view_mode('tree'))
        view_group.addAction(tree_view_action)
        menu.addAction(tree_view_action)
        
        list_view_action = QAction("📋 &List View", self)
        list_view_action.setCheckable(True)
        list_view_action.triggered.connect(lambda: self.set_view_mode('list'))
        view_group.addAction(list_view_action)
        menu.addAction(list_view_action)
        
        details_view_action = QAction("📊 &Details View", self)
        details_view_action.setCheckable(True)
        details_view_action.triggered.connect(lambda: self.set_view_mode('details'))
        view_group.addAction(details_view_action)
        menu.addAction(details_view_action)
        
        menu.addSeparator()
        
        # Show options
        show_hidden_action = QAction("👁️ Show &Hidden Files", self)
        show_hidden_action.setCheckable(True)
        show_hidden_action.setChecked(self.browser_settings.get('show_hidden', False))
        show_hidden_action.triggered.connect(self.toggle_hidden_files)
        menu.addAction(show_hidden_action)
        
        show_extensions_action = QAction("📝 Show File &Extensions", self)
        show_extensions_action.setCheckable(True)
        show_extensions_action.setChecked(self.browser_settings.get('show_extensions', True))
        show_extensions_action.triggered.connect(self.toggle_extensions)
        menu.addAction(show_extensions_action)
        
        menu.addSeparator()
        
        # Sorting
        sort_menu = menu.addMenu("🔽 &Sort By")
        
        sort_group = QActionGroup(self)
        
        sort_name_action = QAction("&Name", self)
        sort_name_action.setCheckable(True)
        sort_name_action.setChecked(True)
        sort_name_action.triggered.connect(lambda: self.set_sort_mode('name'))
        sort_group.addAction(sort_name_action)
        sort_menu.addAction(sort_name_action)
        
        sort_size_action = QAction("📏 &Size", self)
        sort_size_action.setCheckable(True)
        sort_size_action.triggered.connect(lambda: self.set_sort_mode('size'))
        sort_group.addAction(sort_size_action)
        sort_menu.addAction(sort_size_action)
        
        sort_date_action = QAction("📅 &Date Modified", self)
        sort_date_action.setCheckable(True)
        sort_date_action.triggered.connect(lambda: self.set_sort_mode('date'))
        sort_group.addAction(sort_date_action)
        sort_menu.addAction(sort_date_action)
        
        sort_type_action = QAction("🏷️ &Type", self)
        sort_type_action.setCheckable(True)
        sort_type_action.triggered.connect(lambda: self.set_sort_mode('type'))
        sort_group.addAction(sort_type_action)
        sort_menu.addAction(sort_type_action)
        
        menu.addSeparator()
        
        # Refresh
        refresh_action = QAction("🔄 &Refresh", self)
        refresh_action.setShortcut("F5")
        refresh_action.triggered.connect(self.refresh_view)
        menu.addAction(refresh_action)
        
    def setup_tools_menu(self, menu): #vers 1
        """Setup Tools menu"""
        # Search
        search_action = QAction("🔍 &Search Files...", self)
        search_action.setShortcut("Ctrl+F")
        search_action.triggered.connect(self.show_search_dialog)
        menu.addAction(search_action)
        
        menu.addSeparator()
        
        # Terminal here
        terminal_action = QAction("💻 Open &Terminal Here", self)
        terminal_action.setShortcut("Ctrl+Alt+T")
        terminal_action.triggered.connect(self.open_terminal)
        menu.addAction(terminal_action)
        
        # Command prompt (Windows)
        if platform.system() == "Windows":
            cmd_action = QAction("⚫ Open Command &Prompt Here", self)
            cmd_action.triggered.connect(self.open_command_prompt)
            menu.addAction(cmd_action)
        
        menu.addSeparator()
        
        # Calculate folder size
        calc_size_action = QAction("📊 Calculate Folder &Size", self)
        calc_size_action.triggered.connect(self.calculate_folder_size)
        menu.addAction(calc_size_action)
        
    def setup_settings_menu(self, menu): #vers 1
        """Setup Settings menu"""
        # Browser preferences
        preferences_action = QAction("⚙️ Browser &Preferences...", self)
        preferences_action.triggered.connect(self.show_browser_settings)
        menu.addAction(preferences_action)
        
        menu.addSeparator()
        
        # Default applications
        default_apps_action = QAction("🔧 &Default Applications...", self)
        default_apps_action.triggered.connect(self.show_default_apps)
        menu.addAction(default_apps_action)
        
        # File associations
        associations_action = QAction("🔗 File &Associations...", self)
        associations_action.triggered.connect(self.show_file_associations)
        menu.addAction(associations_action)
        
        menu.addSeparator()
        
        # Reset settings
        reset_action = QAction("🔄 &Reset to Defaults", self)
        reset_action.triggered.connect(self.reset_browser_settings)
        menu.addAction(reset_action)
        
    def create_toolbar(self): #vers 1
        """Create navigation toolbar"""
        toolbar = QWidget()
        layout = QHBoxLayout(toolbar)
        layout.setContentsMargins(4, 2, 2, 4)
        font = QFont()
        font.setPointSize(9)  # Set font size to 9pt


        # Navigation buttons
        back_btn = QPushButton("⬅️ Back")
        back_btn.setMaximumHeight(25)
        back_btn.clicked.connect(self.navigate_back)
        back_btn.setFont(font)
        layout.addWidget(back_btn)
        
        forward_btn = QPushButton("➡️ Forward")
        forward_btn.setMaximumHeight(25)
        forward_btn.clicked.connect(self.navigate_forward)
        forward_btn.setFont(font)
        layout.addWidget(forward_btn)
        
        up_btn = QPushButton("⬆️ Up")
        up_btn.setMaximumHeight(25)
        up_btn.clicked.connect(self.navigate_up)
        up_btn.setFont(font)
        layout.addWidget(up_btn)
        
        home_btn = QPushButton("🏠 Home")
        home_btn.setMaximumHeight(25)
        home_btn.clicked.connect(self.navigate_home)
        home_btn.setFont(font)
        layout.addWidget(home_btn)
        
        layout.addWidget(QLabel("|"))
        
        # Quick actions
        new_folder_btn = QPushButton("📁 New Folder")
        new_folder_btn.setMaximumHeight(25)
        new_folder_btn.clicked.connect(self.create_new_folder)
        new_folder_btn.setFont(font)
        layout.addWidget(new_folder_btn)
        
        delete_btn = QPushButton("🗑️ Delete")
        delete_btn.setMaximumHeight(25)
        delete_btn.clicked.connect(self.delete_selected)
        delete_btn.setFont(font)
        layout.addWidget(delete_btn)
        
        layout.addStretch()
        
        # View options
        view_combo = QComboBox()
        view_combo.addItems(["🌳 Tree", "📋 List", "📊 Details"])
        view_combo.setMaximumHeight(25)
        view_combo.currentTextChanged.connect(self.on_view_combo_changed)
        layout.addWidget(view_combo)
        
        return toolbar
        
    def create_info_panel(self): #vers 1
        """Create file information panel"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        font = QFont()
        font.setPointSize(9)  # Set font size to 9pt
        widget.setMaximumHeight(710)

        # Directory statistics
        stats_group = QGroupBox("📊 Directory Statistics")
        stats_layout = QVBoxLayout(stats_group)

        self.stats_label = QLabel("Select a directory to see statistics")
        self.stats_label.setWordWrap(True)
        stats_layout.addWidget(self.stats_label)
        layout.addWidget(stats_group)

        layout.addStretch()

        # Selected file info
        self.info_group = QGroupBox("File Information")
        #self.info_group.setMaximumHeight(200)
        info_layout = QVBoxLayout(self.info_group)
        info_layout.setSpacing(0)
        info_layout.setContentsMargins(4, 4, 4, 4)  # Add margins for the group


        # Create font for smaller labels
        small_font = QFont("", 9)

        self.file_name_label = QLabel("No file selected")
        self.file_name_label.setFont(QFont("", 10, QFont.Weight.Bold))
        info_layout.addWidget(self.file_name_label)

        # Path label - limit to 3 lines
        self.file_path_label = QLabel("")
        self.file_path_label.setWordWrap(True)
        self.file_path_label.setMaximumHeight(60)  # ~3 lines at 9pt font
        self.file_path_label.setFont(small_font)
        info_layout.addWidget(self.file_path_label)

        self.file_type_label = QLabel("")
        self.file_type_label.setFont(small_font)
        self.file_type_label.setMaximumHeight(45)
        info_layout.addWidget(self.file_type_label)

        self.file_size_label = QLabel("")
        self.file_size_label.setFont(small_font)
        self.file_size_label.setMaximumHeight(45)
        info_layout.addWidget(self.file_size_label)

        self.file_date_label = QLabel("")
        self.file_date_label.setFont(small_font)
        self.file_date_label.setMaximumHeight(45)
        info_layout.addWidget(self.file_date_label)
        
        layout.addWidget(self.info_group)

        # Action buttons in 2x3 grid
        actions_group = QGroupBox("⚡ Quick Actions")
        actions_layout = QGridLayout(actions_group)
        actions_layout.setSpacing(3)  # ← Changed from 4 to 3
        actions_layout.setContentsMargins(6, 6, 6, 6)  # Add margins for better appearance

        # Row 1
        self.open_btn = QPushButton("Open")
        self.open_btn.setEnabled(False)
        self.open_btn.setMinimumHeight(25)
        self.open_btn.setFont(font)
        self.open_btn.clicked.connect(self.open_selected)
        actions_layout.addWidget(self.open_btn, 0, 0)

        self.edit_btn = QPushButton("✏️ Edit")
        self.edit_btn.setEnabled(False)
        self.edit_btn.setMinimumHeight(25)
        self.edit_btn.setFont(font)
        self.edit_btn.clicked.connect(self.edit_selected)
        actions_layout.addWidget(self.edit_btn, 0, 1)

        # Row 2
        self.copy_btn = QPushButton("📋 Copy")
        self.copy_btn.setEnabled(False)
        self.copy_btn.setMinimumHeight(25)
        self.copy_btn.setFont(font)
        self.copy_btn.clicked.connect(self.copy_files)
        actions_layout.addWidget(self.copy_btn, 1, 0)

        self.delete_btn = QPushButton("🗑️ Delete")
        self.delete_btn.setEnabled(False)
        self.delete_btn.setFont(font)
        self.delete_btn.setMinimumHeight(25)
        self.delete_btn.clicked.connect(self.delete_selected)
        actions_layout.addWidget(self.delete_btn, 1, 1)

        # Row 3 - Two spare buttons
        self.spare1_btn = QPushButton("🔧 Tools")
        self.spare1_btn.setEnabled(False)
        self.spare1_btn.setMinimumHeight(25)
        self.spare1_btn.setFont(font)
        actions_layout.addWidget(self.spare1_btn, 2, 0)

        self.spare2_btn = QPushButton("⭐ Extra")
        self.spare2_btn.setEnabled(False)
        self.spare2_btn.setMinimumHeight(25)
        self.spare2_btn.setFont(font)
        actions_layout.addWidget(self.spare2_btn, 2, 1)

        layout.addWidget(actions_group)

        return widget
        
    def setup_tree_view(self): #vers 2
        """Setup tree view configuration with table-like columns"""
        # Set up multiple columns like the IMG file window
        column_headers = ["Name", "Type", "Size", "Date Modified", "Attributes", "Path"]
        self.tree.setHeaderLabels(column_headers)
        
        # Configure columns
        self.tree.setAlternatingRowColors(True)
        self.tree.setRootIsDecorated(True)
        self.tree.setIndentation(20)
        self.tree.setSelectionMode(self.tree.SelectionMode.ExtendedSelection)
        self.tree.setSortingEnabled(True)
        
        # Set column widths (similar to IMG file window)
        header = self.tree.header()
        header.setSectionResizeMode(0, header.ResizeMode.Interactive)    # Name - expandable
        header.setSectionResizeMode(1, header.ResizeMode.ResizeToContents)  # Type
        header.setSectionResizeMode(2, header.ResizeMode.ResizeToContents)  # Size  
        header.setSectionResizeMode(3, header.ResizeMode.ResizeToContents)  # Date
        header.setSectionResizeMode(4, header.ResizeMode.ResizeToContents)  # Attributes
        header.setSectionResizeMode(5, header.ResizeMode.Stretch)          # Path
        
        # Set initial column widths
        self.tree.setColumnWidth(0, 250)  # Name
        self.tree.setColumnWidth(1, 80)   # Type
        self.tree.setColumnWidth(2, 100)  # Size
        self.tree.setColumnWidth(3, 150)  # Date
        self.tree.setColumnWidth(4, 80)   # Attributes
        self.tree.setColumnWidth(5, 200)  # Path
        
        # Connect signals
        self.tree.itemClicked.connect(self.on_item_clicked)
        self.tree.itemDoubleClicked.connect(self.on_item_double_clicked)
        self.tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.tree.customContextMenuRequested.connect(self.show_context_menu)
        
    def setup_connections(self): #vers 1
        """Setup signal connections"""
        pass
        
    # Navigation methods
    def navigate_to_address(self): #vers 1
        """Navigate to address bar path"""
        path = self.address_bar.text().strip()
        if path and os.path.exists(path):
            self.browse_directory(path)
        else:
            QMessageBox.warning(self, "Invalid Path", f"The path '{path}' does not exist.")
            
    def navigate_back(self): #vers 1
        """Navigate back in history"""
        # Implementation for back navigation
        self.log_message("⬅️ Navigate back (not implemented)")
        
    def navigate_forward(self): #vers 1
        """Navigate forward in history"""
        # Implementation for forward navigation
        self.log_message("➡️ Navigate forward (not implemented)")
        
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
        
    # File operations
    def create_new_folder(self): #vers 1
        """Create new folder in current directory"""
        if not self.current_path:
            QMessageBox.warning(self, "No Directory", "Please select a directory first.")
            return
            
        name, ok = QInputDialog.getText(self, "New Folder", "Folder name:")
        if ok and name:
            new_path = os.path.join(self.current_path, name)
            try:
                os.makedirs(new_path, exist_ok=False)
                self.refresh_view()
                self.log_message(f"📁 Created folder: {name}")
            except FileExistsError:
                QMessageBox.warning(self, "Folder Exists", f"A folder named '{name}' already exists.")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to create folder: {str(e)}")
                
    def create_new_file(self): #vers 1
        """Create new file in current directory"""
        if not self.current_path:
            QMessageBox.warning(self, "No Directory", "Please select a directory first.")
            return
            
        name, ok = QInputDialog.getText(self, "New File", "File name:")
        if ok and name:
            new_path = os.path.join(self.current_path, name)
            try:
                with open(new_path, 'w') as f:
                    f.write("")  # Create empty file
                self.refresh_view()
                self.log_message(f"Created file: {name}")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to create file: {str(e)}")
                
    def cut_files(self): #vers 1
        """Cut selected files to clipboard"""
        selected_items = self.tree.selectedItems()
        if selected_items:
            self.clipboard_files = [item.data(0, Qt.ItemDataRole.UserRole) for item in selected_items]
            self.clipboard_operation = 'cut'
            self.log_message(f"✂️ Cut {len(self.clipboard_files)} item(s)")
            
    def copy_files(self): #vers 1
        """Copy selected files to clipboard"""
        selected_items = self.tree.selectedItems()
        if selected_items:
            self.clipboard_files = [item.data(0, Qt.ItemDataRole.UserRole) for item in selected_items]
            self.clipboard_operation = 'copy'
            self.log_message(f"📋 Copied {len(self.clipboard_files)} item(s)")
            
    def paste_files(self): #vers 1
        """Paste files from clipboard"""
        if not self.clipboard_files or not self.current_path:
            return
            
        try:
            for file_path in self.clipboard_files:
                if not os.path.exists(file_path):
                    continue
                    
                dest_path = os.path.join(self.current_path, os.path.basename(file_path))
                
                if self.clipboard_operation == 'copy':
                    if os.path.isdir(file_path):
                        shutil.copytree(file_path, dest_path)
                    else:
                        shutil.copy2(file_path, dest_path)
                elif self.clipboard_operation == 'cut':
                    shutil.move(file_path, dest_path)
                    
            self.refresh_view()
            operation = "moved" if self.clipboard_operation == 'cut' else "copied"
            self.log_message(f"📌 Successfully {operation} {len(self.clipboard_files)} item(s)")
            
            if self.clipboard_operation == 'cut':
                self.clipboard_files = []
                
        except Exception as e:
            QMessageBox.critical(self, "Paste Error", f"Failed to paste files: {str(e)}")
            
    def delete_selected(self): #vers 1
        """Delete selected files"""
        selected_items = self.tree.selectedItems()
        if not selected_items:
            return
            
        file_paths = [item.data(0, Qt.ItemDataRole.UserRole) for item in selected_items]
        
        reply = QMessageBox.question(
            self, "Confirm Delete",
            f"Are you sure you want to delete {len(file_paths)} item(s)?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
        )
        
        if reply == QMessageBox.StandardButton.Yes:
            try:
                for file_path in file_paths:
                    if os.path.isdir(file_path):
                        shutil.rmtree(file_path)
                    else:
                        os.remove(file_path)
                        
                self.refresh_view()
                self.log_message(f"🗑️ Deleted {len(file_paths)} item(s)")
                
            except Exception as e:
                QMessageBox.critical(self, "Delete Error", f"Failed to delete files: {str(e)}")
                
    def rename_selected(self): #vers 1
        """Rename selected file"""
        selected_items = self.tree.selectedItems()
        if len(selected_items) != 1:
            QMessageBox.warning(self, "Rename", "Please select exactly one item to rename.")
            return
            
        item = selected_items[0]
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        current_name = os.path.basename(file_path)
        
        new_name, ok = QInputDialog.getText(self, "Rename", "New name:", text=current_name)
        if ok and new_name and new_name != current_name:
            try:
                new_path = os.path.join(os.path.dirname(file_path), new_name)
                os.rename(file_path, new_path)
                self.refresh_view()
                self.log_message(f"✏️ Renamed '{current_name}' to '{new_name}'")
            except Exception as e:
                QMessageBox.critical(self, "Rename Error", f"Failed to rename file: {str(e)}")
                
    # View methods
    def refresh_view(self): #vers 1
        """Refresh current view"""
        if self.current_path:
            self.browse_directory(self.current_path)
            
    def populate_tree(self, root_path): #vers 2
        """Populate tree with directory contents - IMPROVED: Show more items"""
        try:
            self.tree.clear()
            
            if not os.path.exists(root_path):
                self.log_message(f"❌ Directory does not exist: {root_path}")
                return
                
            # Create root item
            root_item = QTreeWidgetItem(self.tree)
            root_item.setText(0, os.path.basename(root_path) or root_path)
            root_item.setData(0, Qt.ItemDataRole.UserRole, root_path)
            
            # Get max depth from settings (default to 4 for more visibility)
            max_depth = self.browser_settings.get('max_depth', 4)
            
            # Populate children with increased depth
            self.populate_tree_recursive(root_item, root_path, max_depth=max_depth)
            
            # Expand root and first level for better visibility
            root_item.setExpanded(True)
            
            # Auto-expand first level if setting enabled
            if self.browser_settings.get('auto_expand', True):
                for i in range(root_item.childCount()):
                    child = root_item.child(i)
                    if child and child.text(0).startswith("📁"):  # Directory
                        child.setExpanded(True)
            
            # Update current path
            self.current_path = root_path
            self.address_bar.setText(root_path)
            
            # Update status with item count
            total_items = self.count_tree_items(root_item)
            self.log_message(f"🗂️ File browser populated: {root_path} ({total_items} items)")
            
        except Exception as e:
            self.log_message(f"❌ Error populating tree: {str(e)}")
            
    def count_tree_items(self, item): #vers 1
        """Count total items in tree recursively"""
        count = 1  # Count this item
        for i in range(item.childCount()):
            count += self.count_tree_items(item.child(i))
        return count
            
    def populate_tree_recursive(self, parent_item: QTreeWidgetItem, 
                               dir_path: str, max_depth: int = 2, 
                               current_depth: int = 0): #vers 4
        """Recursively populate tree with enhanced error handling"""
        if current_depth >= max_depth:
            return
            
        try:
            # Get all items in directory with better error handling
            try:
                all_items = os.listdir(dir_path)
            except PermissionError:
                # Add permission denied placeholder
                error_item = QTreeWidgetItem(parent_item)
                error_item.setText(0, "🔒 Permission Denied")
                error_item.setText(1, "Error")
                error_item.setText(2, "")
                error_item.setText(3, "")
                error_item.setText(4, "")
                error_item.setText(5, dir_path)
                return
            except (OSError, FileNotFoundError) as e:
                # Add error placeholder
                error_item = QTreeWidgetItem(parent_item)
                error_item.setText(0, f"❌ Error: {str(e)[:20]}...")
                error_item.setText(1, "Error")
                error_item.setText(2, "")
                error_item.setText(3, "")
                error_item.setText(4, "")
                error_item.setText(5, dir_path)
                return
            
            # Filter hidden files if needed (but show all regular files)
            filtered_items = []
            if not self.browser_settings.get('show_hidden', False):
                filtered_items = [item for item in all_items if not item.startswith('.')]
            else:
                filtered_items = all_items[:]  # Show all items including hidden
            
            # Separate directories and files with enhanced debugging
            directories = []
            files = []
            
            for item in filtered_items:
                item_path = os.path.join(dir_path, item)
                try:
                    # More explicit file type checking
                    if os.path.isdir(item_path):
                        directories.append(item)
                    elif os.path.isfile(item_path):
                        files.append(item)
                    else:
                        # Log what we're skipping
                        if current_depth == 0:  # Only log at root level
                            self.log_message(f"⚠️ Skipping special item: {item} (not file or directory)")
                except (PermissionError, OSError) as e:
                    if current_depth == 0:
                        self.log_message(f"⚠️ Cannot access: {item} ({str(e)})")
                    continue
            
            # Sort items safely
            try:
                sort_mode = self.browser_settings.get('sort_mode', 'name')
                if sort_mode == 'name':
                    directories.sort(key=str.lower)
                    files.sort(key=str.lower)
                elif sort_mode == 'size':
                    files.sort(key=lambda x: self.get_file_size_for_sort(os.path.join(dir_path, x)))
            except Exception:
                # If sorting fails, just use alphabetical
                directories.sort()
                files.sort()
                
            # Debug logging for file visibility issues
            if current_depth == 0:  # Only log for root level to avoid spam
                self.log_message(f"📊 Found: {len(directories)} folders, {len(files)} files in {os.path.basename(dir_path)}")
            
            # Add directories first
            for directory in directories:
                item_path = os.path.join(dir_path, directory)
                
                try:
                    tree_item = QTreeWidgetItem(parent_item)
                    tree_item.setData(0, Qt.ItemDataRole.UserRole, item_path)
                    
                    # Column 0: Name with icon
                    tree_item.setText(0, f"📁 {directory}")
                    
                    # Column 1: Type
                    tree_item.setText(1, "Folder")
                    
                    # Column 2: Size
                    try:
                        folder_size = get_folder_size_quick_backend(item_path)
                        tree_item.setText(2, format_file_size_backend(folder_size) if folder_size > 0 else "")
                    except:
                        tree_item.setText(2, "")
                    
                    # Column 3: Date Modified
                    try:
                        import datetime
                        mod_time = os.path.getmtime(item_path)
                        mod_date = datetime.datetime.fromtimestamp(mod_time)
                        tree_item.setText(3, mod_date.strftime('%Y-%m-%d %H:%M'))
                    except:
                        tree_item.setText(3, "")
                    
                    # Column 4: Attributes
                    try:
                        attrs = get_file_attributes_backend(item_path)
                        tree_item.setText(4, attrs)
                    except:
                        tree_item.setText(4, "")
                    
                    # Column 5: Path
                    tree_item.setText(5, item_path)
                    
                    # Add subdirectories with error handling
                    if current_depth < max_depth - 1:
                        try:
                            self.populate_tree_recursive(tree_item, item_path, 
                                                       max_depth, current_depth + 1)
                        except Exception as e:
                            # Add error item as child if recursion fails
                            error_child = QTreeWidgetItem(tree_item)
                            error_child.setText(0, f"❌ Error: {str(e)[:20]}...")
                            error_child.setText(1, "Error")
                            error_child.setText(2, "")
                            error_child.setText(3, "")
                            error_child.setText(4, "")
                            error_child.setText(5, item_path)
                            
                    directories_added += 1
                except Exception as e:
                    # If we can't create the tree item, skip this directory
                    if current_depth == 0:
                        self.log_message(f"❌ Failed to add directory: {directory} ({str(e)})")
                    continue
            
            # Add files with detailed logging
            files_added = 0
            for file in files:
                item_path = os.path.join(dir_path, file)
                
                try:
                    tree_item = QTreeWidgetItem(parent_item)
                    tree_item.setData(0, Qt.ItemDataRole.UserRole, item_path)
                    
                    # Column 0: Name with icon
                    file_ext = os.path.splitext(file)[1].lower()
                    icon = get_file_type_icon_backend(file_ext)
                    
                    if self.browser_settings.get('show_extensions', True):
                        tree_item.setText(0, f"{icon} {file}")
                    else:
                        name_without_ext = os.path.splitext(file)[0]
                        tree_item.setText(0, f"{icon} {name_without_ext}")
                    
                    # Column 1: Type
                    tree_item.setText(1, get_file_type_display_backend(file_ext))
                    
                    # Column 2: Size
                    try:
                        size = os.path.getsize(item_path)
                        tree_item.setText(2, format_file_size_backend(size))
                    except:
                        tree_item.setText(2, "")
                    
                    # Column 3: Date Modified
                    try:
                        import datetime
                        mod_time = os.path.getmtime(item_path)
                        mod_date = datetime.datetime.fromtimestamp(mod_time)
                        tree_item.setText(3, mod_date.strftime('%Y-%m-%d %H:%M'))
                    except:
                        tree_item.setText(3, "")
                    
                    # Column 4: Attributes
                    try:
                        attrs = get_file_attributes_backend(item_path)
                        tree_item.setText(4, attrs)
                    except:
                        tree_item.setText(4, "")
                    
                    # Column 5: Path
                    tree_item.setText(5, item_path)
                    
                    files_added += 1
                    
                except Exception as e:
                    # If we can't create the tree item, skip this file
                    if current_depth == 0:
                        self.log_message(f"❌ Failed to add file: {file} ({str(e)})")
                    continue
                    
            # Final debug report
            if current_depth == 0:
                self.log_message(f"✅ Successfully added: {directories_added} folders, {files_added} files")
                    
        except Exception as e:
            # Final catch-all error handling
            self.log_message(f"❌ Error reading directory {dir_path}: {str(e)}")
            error_item = QTreeWidgetItem(parent_item)
            error_item.setText(0, f"❌ Fatal Error: {str(e)[:20]}...")
            error_item.setText(1, "Error")
            error_item.setText(2, "")
            error_item.setText(3, "")
            error_item.setText(4, "")
            error_item.setText(5, dir_path)
            
    def browse_directory(self, path): #vers 1
        """Browse to specified directory"""
        try:
            if os.path.exists(path):
                self.current_path = path
                self.address_bar.setText(path)
                self.populate_tree(path)
                self.directory_changed.emit(path)
                self.status_bar.setText(f"📁 {path}")
        except Exception as e:
            self.log_message(f"❌ Error browsing directory: {str(e)}")
        
    # Settings and configuration
    def load_browser_settings(self): #vers 2
        """Load browser settings with better defaults"""
        settings = QSettings("IMG Factory", "File Browser")
        return {
            'show_hidden': settings.value('show_hidden', False, type=bool),
            'show_extensions': settings.value('show_extensions', True, type=bool),
            'view_mode': settings.value('view_mode', 'tree', type=str),
            'sort_mode': settings.value('sort_mode', 'name', type=str),
            'font_size': settings.value('font_size', 13, type=int),
            'max_depth': settings.value('max_depth', 4, type=int),  # Show more levels
            'auto_expand': settings.value('auto_expand', True, type=bool),
        }
        
    def save_browser_settings(self): #vers 1
        """Save browser settings"""
        settings = QSettings("IMG Factory", "File Browser")
        for key, value in self.browser_settings.items():
            settings.setValue(key, value)
            
    def show_browser_settings(self): #vers 1
        """Show browser settings dialog"""
        dialog = BrowserSettingsDialog(self.browser_settings, self)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            self.browser_settings = dialog.get_settings()
            self.save_browser_settings()
            self.apply_settings()
            
    def apply_settings(self): #vers 1
        """Apply current settings to browser"""
        # Apply view mode, font size, etc.
        self.apply_browser_styling()
        self.refresh_view()
        
    def apply_browser_styling(self): #vers 4
        """Apply styling with professional table appearance and vertical grid lines"""
        try:
            # Get theme colors from main window if available
            theme_colors = self.get_theme_colors()
            
            if theme_colors:
                # Apply theme colors to tree with professional table styling
                tree_style = f"""
                    QTreeWidget {{
                        background-color: {theme_colors.get('bg_secondary', '#ffffff')};
                        color: {theme_colors.get('text_primary', '#333333')};
                        font-size: {self.browser_settings.get('font_size', 13)}px;
                        font-family: 'Segoe UI', Arial, sans-serif;
                        border: 1px solid {theme_colors.get('border', '#cccccc')};
                        outline: none;
                        selection-background-color: {theme_colors.get('accent_primary', '#FFECEE')};
                        alternate-background-color: {theme_colors.get('bg_tertiary', '#f0f0f0')};
                        gridline-color: {theme_colors.get('border', '#e0e0e0')};
                        show-decoration-selected: 1;
                    }}
                    QTreeWidget::item {{
                        padding: 3px 4px;
                        border: none;
                        min-height: 22px;
                        border-bottom: 1px solid {theme_colors.get('border', '#e0e0e0')};
                        border-right: 1px solid {theme_colors.get('border', '#e0e0e0')};
                    }}
                    QTreeWidget::item:hover {{
                        background-color: {theme_colors.get('button_hover', '#e8e8e8')};
                    }}
                    QTreeWidget::item:selected {{
                        background-color: {theme_colors.get('accent_primary', '#FFECEE')};
                        color: #ffffff;
                    }}
                    QTreeWidget::item:selected:hover {{
                        background-color: {theme_colors.get('accent_secondary', '#FFECEE')};
                        color: #ffffff;
                    }}
                    QHeaderView {{
                        background-color: {theme_colors.get('panel_bg', '#f5f5f5')};
                    }}
                    QHeaderView::section {{
                        background-color: {theme_colors.get('panel_bg', '#f5f5f5')};
                        color: {theme_colors.get('text_primary', '#333333')};
                        padding: 6px 4px;
                        border: 1px solid {theme_colors.get('border', '#cccccc')};
                        border-left: none;
                        border-top: none;
                        font-weight: bold;
                        font-size: {self.browser_settings.get('font_size', 13)}px;
                    }}
                    QHeaderView::section:first {{
                        border-left: 1px solid {theme_colors.get('border', '#cccccc')};
                    }}
                    QHeaderView::section:hover {{
                        background-color: {theme_colors.get('button_hover', '#e8e8e8')};
                    }}
                """
                self.tree.setStyleSheet(tree_style)
            else:
                # Fallback styling with professional table appearance
                self.tree.setStyleSheet(f"""
                    QTreeWidget {{
                        font-size: {self.browser_settings.get('font_size', 13)}px;
                        font-family: 'Segoe UI', Arial, sans-serif;
                        alternate-background-color: #f8f9fa;
                        background-color: #ffffff;
                        gridline-color: #e0e0e0;
                        border: 1px solid #cccccc;
                        show-decoration-selected: 1;
                    }}
                    QTreeWidget::item {{
                        padding: 3px 4px;
                        min-height: 22px;
                        border-bottom: 1px solid #e0e0e0;
                        border-right: 1px solid #e0e0e0;
                    }}
                    QTreeWidget::item:hover {{
                        background-color: #e8e8e8;
                    }}
                    QTreeWidget::item:selected {{
                        background-color: #FFECEE;
                        color: #ffffff;
                    }}
                    QHeaderView {{
                        background-color: #f5f5f5;
                    }}
                    QHeaderView::section {{
                        background-color: #f5f5f5;
                        padding: 6px 4px;
                        border: 1px solid #cccccc;
                        border-left: none;
                        border-top: none;
                        font-weight: bold;
                    }}
                    QHeaderView::section:first {{
                        border-left: 1px solid #cccccc;
                    }}
                    QHeaderView::section:hover {{
                        background-color: #e8e8e8;
                    }}
                """)
                
        except Exception as e:
            self.log_message(f"❌ Error applying browser styling: {str(e)}")

    def format_file_size(self, size: int) -> str: #vers 2
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
            

    def get_theme_colors(self): #vers 1
        """Get theme colors from main window"""
        try:
            # Navigate up to find main window with theme system
            parent = self.parent()
            while parent:
                if hasattr(parent, 'app_settings'):
                    app_settings = parent.app_settings
                    if hasattr(app_settings, 'get_theme_colors'):
                        return app_settings.get_theme_colors()
                    elif hasattr(app_settings, 'get_theme'):
                        theme = app_settings.get_theme()
                        return theme.get('colors', {})
                parent = parent.parent()
            return {}
        except Exception:
            return {}
        
    # Event handlers
    def on_item_clicked(self, item, column): #vers 1
        """Handle item click"""
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if file_path:
            self.file_selected.emit(file_path)
            self.update_file_info(file_path)
            
    def on_item_double_clicked(self, item, column): #vers 1
        """Handle item double click"""
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if file_path:
            if os.path.isdir(file_path):
                self.browse_directory(file_path)
            else:
                self.file_opened.emit(file_path)
                
    def on_view_combo_changed(self, text): #vers 1
        """Handle view mode combo change"""
        if "Tree" in text:
            self.set_view_mode('tree')
        elif "List" in text:
            self.set_view_mode('list')
        elif "Details" in text:
            self.set_view_mode('details')
            
    def show_context_menu(self, position): #vers 1
        """Show context menu"""
        item = self.tree.itemAt(position)
        if not item:
            return
            
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if not file_path:
            return
            
        menu = self.create_context_menu(file_path)
        menu.exec(self.tree.mapToGlobal(position))
        
    def create_context_menu(self, file_path): #vers 2
        """Create comprehensive context menu for file - ENHANCED"""
        menu = QMenu(self)
        
        file_ext = os.path.splitext(file_path)[1].lower()
        is_file = os.path.isfile(file_path)
        is_dir = os.path.isdir(file_path)
        
        if is_file:
            # === FILE-SPECIFIC ACTIONS ===
            
            # IMG Factory specific files
            if file_ext == '.img':
                load_img_action = menu.addAction("💽 Load in IMG Factory")
                load_img_action.triggered.connect(lambda: self.file_opened.emit(file_path))
                
                analyze_action = menu.addAction("🔍 Analyze IMG Structure")
                analyze_action.triggered.connect(lambda: self.analyze_img_file(file_path))
                menu.addSeparator()
                
            elif file_ext == '.col':
                load_col_action = menu.addAction("Load COL File")
                load_col_action.triggered.connect(lambda: self.file_opened.emit(file_path))
                
                view_col_action = menu.addAction("👁️ View COL Models")
                view_col_action.triggered.connect(lambda: self.view_col_file(file_path))
                menu.addSeparator()
                
            elif file_ext == '.txd':
                view_textures_action = menu.addAction("View Textures")
                view_textures_action.triggered.connect(lambda: self.view_txd_file(file_path))
                menu.addSeparator()
                
            elif file_ext == '.dff':
                view_model_action = menu.addAction("🎭 View 3D Model")
                view_model_action.triggered.connect(lambda: self.view_dff_file(file_path))
                menu.addSeparator()
                
            elif file_ext in ['.ide', '.ipl', '.dat']:
                edit_text_action = menu.addAction("📝 Edit Text File")
                edit_text_action.triggered.connect(lambda: self.edit_file(file_path))
                
                view_definitions_action = menu.addAction("👁️ View Definitions")
                view_definitions_action.triggered.connect(lambda: self.view_text_file(file_path))
                menu.addSeparator()
            
            # Generic file actions
            open_action = menu.addAction("Open")
            open_action.triggered.connect(lambda: self.file_opened.emit(file_path))
            
            open_with_action = menu.addAction("🔧 Open With...")
            open_with_action.triggered.connect(self.open_with_dialog)
            
            edit_action = menu.addAction("✏️ Edit")
            edit_action.triggered.connect(lambda: self.edit_file(file_path))
            
            menu.addSeparator()
            
        elif is_dir:
            # === DIRECTORY-SPECIFIC ACTIONS ===
            
            open_action = menu.addAction("Open Folder")
            open_action.triggered.connect(lambda: self.browse_directory(file_path))
            
            open_new_window_action = menu.addAction("🗂️ Open in New Window")
            open_new_window_action.triggered.connect(lambda: self.open_in_new_window(file_path))
            
            menu.addSeparator()
            
            # Quick scan actions
            scan_imgs_action = menu.addAction("💽 Scan for IMG Files")
            scan_imgs_action.triggered.connect(lambda: self.scan_for_file_type(file_path, '.img'))
            
            scan_models_action = menu.addAction("🎭 Scan for Models")
            scan_models_action.triggered.connect(lambda: self.scan_for_file_type(file_path, '.dff'))
            
            scan_textures_action = menu.addAction("Scan for Textures")
            scan_textures_action.triggered.connect(lambda: self.scan_for_file_type(file_path, '.txd'))
            
            menu.addSeparator()
        
        # === COMMON ACTIONS ===
        
        # Clipboard operations
        copy_action = menu.addAction("📋 Copy")
        copy_action.triggered.connect(self.copy_files)
        
        cut_action = menu.addAction("✂️ Cut")
        cut_action.triggered.connect(self.cut_files)
        
        if self.clipboard_files:
            paste_action = menu.addAction("📌 Paste")
            paste_action.triggered.connect(self.paste_files)
            
        menu.addSeparator()
        
        # File operations
        rename_action = menu.addAction("✏️ Rename")
        rename_action.triggered.connect(self.rename_selected)
        
        delete_action = menu.addAction("🗑️ Delete")
        delete_action.triggered.connect(self.delete_selected)
        
        menu.addSeparator()
        
        # Copy path options
        copy_path_menu = menu.addMenu("📋 Copy Path")
        
        copy_full_path_action = copy_path_menu.addAction("📍 Full Path")
        copy_full_path_action.triggered.connect(lambda: self.copy_path_to_clipboard(file_path))
        
        copy_filename_action = copy_path_menu.addAction("Filename Only")
        copy_filename_action.triggered.connect(lambda: self.copy_filename_to_clipboard(file_path))
        
        copy_parent_action = copy_path_menu.addAction("📁 Parent Directory")
        copy_parent_action.triggered.connect(lambda: self.copy_path_to_clipboard(os.path.dirname(file_path)))
        
        menu.addSeparator()
        
        # System integration
        explore_action = menu.addAction("🗂️ Open in Explorer")
        explore_action.triggered.connect(lambda: self.open_in_explorer(file_path))
        
        # IMG Factory text editor for supported files
        if file_ext in ['.ide', '.ipl', '.dat', '.cfg', '.txt', '.log']:
            notepad_action = menu.addAction("📝 Edit in IMG Factory Editor")
            notepad_action.triggered.connect(lambda: self.open_in_notepad(file_path))
        else:
            # System text editor for other files
            cmd_edit_action = menu.addAction("💻 Edit with System Editor")
            cmd_edit_action.triggered.connect(lambda: self.edit_file(file_path))
        
        terminal_action = menu.addAction("💻 Open Terminal Here")
        terminal_action.triggered.connect(lambda: self.open_terminal_at_path(file_path))
        
        menu.addSeparator()
        
        # Properties and info
        properties_action = menu.addAction("📋 Properties")
        properties_action.triggered.connect(lambda: self.show_file_properties(file_path))
        
        return menu
        
    # Enhanced action methods for context menu
    def analyze_img_file(self, file_path): #vers 1
        """Analyze IMG file structure"""
        QMessageBox.information(
            self, "IMG Analysis", 
            f"IMG file analysis for:\n{file_path}\n\nThis would show:\n• Entry count\n• File version\n• Compression status\n• Corruption check"
        )
        
    def view_col_file(self, file_path): #vers 1
        """View COL file models"""
        QMessageBox.information(
            self, "COL Viewer", 
            f"COL model viewer for:\n{file_path}\n\nThis would show:\n• Model count\n• Collision shapes\n• Bounding boxes\n• Version info"
        )
        
    def view_txd_file(self, file_path): #vers 1
        """View TXD textures"""
        QMessageBox.information(
            self, "TXD Viewer", 
            f"Texture viewer for:\n{file_path}\n\nThis would show:\n• Texture list\n• Resolution info\n• Format details\n• Preview images"
        )
        
    def view_dff_file(self, file_path): #vers 1
        """View DFF 3D model"""
        QMessageBox.information(
            self, "DFF Viewer", 
            f"3D model viewer for:\n{file_path}\n\nThis would show:\n• Mesh info\n• Material count\n• Bone structure\n• LOD levels"
        )
        
    def view_text_file(self, file_path): #vers 1
        """View text file definitions"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read(1000)  # First 1000 chars
            
            QMessageBox.information(
                self, f"Text File Preview - {os.path.basename(file_path)}", 
                f"Preview of {file_path}:\n\n{content}{'...' if len(content) >= 1000 else ''}"
            )
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Could not read file: {str(e)}")
        
    def copy_filename_to_clipboard(self, file_path): #vers 1
        """Copy just the filename to clipboard"""
        filename = os.path.basename(file_path)
        from PyQt6.QtWidgets import QApplication
        QApplication.clipboard().setText(filename)
        self.log_message(f"📋 Filename copied: {filename}")
        
    def open_in_new_window(self, dir_path): #vers 1
        """Open directory in new file manager window"""
        try:
            import subprocess
            import platform
            
            system = platform.system()
            if system == "Windows":
                subprocess.run(["explorer", dir_path], shell=False)
            elif system == "Darwin":  # macOS
                subprocess.run(["open", dir_path])
            else:  # Linux
                subprocess.run(["xdg-open", dir_path])
                
            self.log_message(f"🗂️ Opened in new window: {dir_path}")
        except Exception as e:
            self.log_message(f"❌ Error opening new window: {str(e)}")
            
    def scan_for_file_type(self, dir_path, file_ext): #vers 1
        """Scan directory for specific file type"""
        try:
            count = 0
            files_found = []
            
            for root, dirs, files in os.walk(dir_path):
                for file in files:
                    if file.lower().endswith(file_ext):
                        count += 1
                        files_found.append(os.path.join(root, file))
                        if count >= 20:  # Limit for display
                            break
                if count >= 20:
                    break
            
            file_list = '\n'.join([os.path.basename(f) for f in files_found[:10]])
            if count > 10:
                file_list += f'\n... and {count - 10} more'
                
            QMessageBox.information(
                self, f"File Scan Results - {file_ext.upper()}", 
                f"Found {count} {file_ext.upper()} files in:\n{dir_path}\n\nFiles found:\n{file_list}"
            )
            
        except Exception as e:
            QMessageBox.warning(self, "Scan Error", f"Error scanning directory: {str(e)}")
            
    def open_in_notepad(self, file_path): #vers 1
        """Open file in system text editor"""
        try:
            import subprocess
            import platform
            
            system = platform.system()
            if system == "Windows":
                subprocess.run(["notepad.exe", file_path])
            elif system == "Darwin":  # macOS
                subprocess.run(["open", "-a", "TextEdit", file_path])
            else:  # Linux
                subprocess.run(["gedit", file_path])
                
        except Exception as e:
            self.log_message(f"❌ Error opening in text editor: {str(e)}")
            
    def open_terminal_at_path(self, file_path): #vers 1
        """Open terminal at file location"""
        try:
            terminal_path = os.path.dirname(file_path) if os.path.isfile(file_path) else file_path
            
            import subprocess
            import platform
            
            system = platform.system()
            if system == "Windows":
                subprocess.run(["cmd", "/c", "start", "cmd", "/k", f"cd /d \"{terminal_path}\""])
            elif system == "Darwin":  # macOS
                subprocess.run(["open", "-a", "Terminal", terminal_path])
            else:  # Linux
                subprocess.run(["gnome-terminal", "--working-directory", terminal_path])
                
            self.log_message(f"💻 Terminal opened at: {terminal_path}")
        except Exception as e:
            self.log_message(f"❌ Error opening terminal: {str(e)}")
        
    # Utility methods
    def update_file_info(self, file_path): #vers 1
        """Update file information display"""
        try:
            file_name = os.path.basename(file_path)
            self.file_name_label.setText(f"{file_name}")
            self.file_path_label.setText(f"Path: {file_path}")
            
            if os.path.isfile(file_path):
                # File info
                size = os.path.getsize(file_path)
                self.file_size_label.setText(f"Size: {self.format_file_size(size)}")
                
                file_ext = os.path.splitext(file_path)[1].lower()
                self.file_type_label.setText(f"Type: {self.get_file_type_display_backend(file_ext)}")
                
                # Modification date
                import datetime
                mod_time = os.path.getmtime(file_path)
                mod_date = datetime.datetime.fromtimestamp(mod_time)
                self.file_date_label.setText(f"Modified: {mod_date.strftime('%Y-%m-%d %H:%M')}")
                
                # Enable file actions
                self.open_btn.setEnabled(True)
                self.edit_btn.setEnabled(file_ext in ['.txt', '.py', '.json', '.cfg'])
                self.copy_btn.setEnabled(True)
                self.delete_btn.setEnabled(True)
                
            else:
                # Directory info
                self.file_type_label.setText("Type: 📁 Directory")
                self.file_size_label.setText("Size: Directory")
                self.file_date_label.setText("")
                
                # Enable directory actions
                self.open_btn.setEnabled(True)
                self.edit_btn.setEnabled(False)
                self.copy_btn.setEnabled(True)
                self.delete_btn.setEnabled(True)
                
        except Exception as e:
            self.log_message(f"❌ Error updating file info: {str(e)}")
            

    # Additional action methods
    def open_selected(self): #vers 1
        """Open selected file or directory"""
        selected_items = self.tree.selectedItems()
        if selected_items:
            file_path = selected_items[0].data(0, Qt.ItemDataRole.UserRole)
            if os.path.isdir(file_path):
                self.browse_directory(file_path)
            else:
                self.file_opened.emit(file_path)
                
    def open_with_dialog(self): #vers 1
        """Show open with dialog"""
        selected_items = self.tree.selectedItems()
        if not selected_items:
            return
            
        file_path = selected_items[0].data(0, Qt.ItemDataRole.UserRole)
        if not os.path.isfile(file_path):
            return
            
        # Simple implementation - could be enhanced with actual app selection
        QMessageBox.information(
            self, "Open With", 
            f"Open with dialog for:\n{file_path}\n\nThis feature will be implemented with system app detection."
        )
                
    def edit_selected(self): #vers 1
        """Edit selected file"""
        selected_items = self.tree.selectedItems()
        if selected_items:
            file_path = selected_items[0].data(0, Qt.ItemDataRole.UserRole)
            self.edit_file(file_path)
            
    def show_properties(self): #vers 1
        """Show properties of selected file"""
        selected_items = self.tree.selectedItems()
        if selected_items:
            file_path = selected_items[0].data(0, Qt.ItemDataRole.UserRole)
            self.show_file_properties(file_path)
            
    def edit_file(self, file_path): #vers 2
        """Edit file with IMG Factory text editor"""
        try:
            file_ext = os.path.splitext(file_path)[1].lower()
            
            # Use IMG Factory text editor for supported files
            if file_ext in ['.ide', '.ipl', '.dat', '.cfg', '.txt', '.log']:
                from apps.core.notepad import open_text_file_in_editor
                main_window = self.get_main_window()
                editor = open_text_file_in_editor(file_path, main_window)
                if editor:
                    self.log_message(f"📝 Opened in IMG Factory Text Editor: {os.path.basename(file_path)}")
                    return
                    
            # Fallback to system editor for other files
            if platform.system() == "Windows":
                os.startfile(file_path)
            elif platform.system() == "Darwin":  # macOS
                subprocess.run(["open", file_path])
            else:  # Linux
                subprocess.run(["xdg-open", file_path])
                
            self.log_message(f"📝 Opened with system editor: {os.path.basename(file_path)}")
            
        except Exception as e:
            QMessageBox.critical(self, "Edit Error", f"Failed to open file for editing: {str(e)}")
            
    def open_in_notepad(self, file_path): #vers 2
        """Open file in IMG Factory text editor"""
        try:
            from apps.core.notepad import open_text_file_in_editor
            main_window = self.get_main_window()
            editor = open_text_file_in_editor(file_path, main_window)
            if editor:
                self.log_message(f"📝 Opened in IMG Factory Text Editor: {os.path.basename(file_path)}")
            else:
                raise Exception("Failed to create text editor")
        except Exception as e:
            self.log_message(f"❌ Error opening in text editor: {str(e)}")
            
    def show_file_properties(self, file_path): #vers 1
        """Show file properties dialog"""
        dialog = FilePropertiesDialog(file_path, self)
        dialog.exec()
        
    def log_message(self, message): #vers 1
        """Log message to status bar and parent"""
        # Send message to main window log instead of local status bar
        main_window = self.get_main_window()
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(message)
        else:
            print(f"[FileBrowser] {message}")
            
    # View mode methods
    def set_view_mode(self, mode): #vers 1
        """Set view mode (tree, list, details)"""
        self.browser_settings['view_mode'] = mode
        self.log_message(f"👁️ View mode: {mode}")
        # Implementation would switch between different view widgets
        
    def set_sort_mode(self, mode): #vers 1
        """Set sort mode (name, size, date, type)"""
        self.browser_settings['sort_mode'] = mode
        self.refresh_view()
        self.log_message(f"🔽 Sort by: {mode}")
        
    def toggle_hidden_files(self, show): #vers 1
        """Toggle showing hidden files"""
        self.browser_settings['show_hidden'] = show
        self.refresh_view()
        self.log_message(f"👁️ Hidden files: {'shown' if show else 'hidden'}")
        
    def toggle_extensions(self, show): #vers 1
        """Toggle showing file extensions"""
        self.browser_settings['show_extensions'] = show
        self.refresh_view()
        self.log_message(f"📝 Extensions: {'shown' if show else 'hidden'}")
        
    # Tool methods
    def open_terminal(self): #vers 1
        """Open terminal in current directory"""
        if not self.current_path:
            return
            
        try:
            if platform.system() == "Windows":
                subprocess.run(["cmd", "/c", "start", "cmd"], cwd=self.current_path)
            elif platform.system() == "Darwin":  # macOS
                subprocess.run(["open", "-a", "Terminal", self.current_path])
            else:  # Linux
                subprocess.run(["gnome-terminal", "--working-directory", self.current_path])
        except Exception as e:
            self.log_message(f"❌ Failed to open terminal: {str(e)}")
            
    def open_command_prompt(self): #vers 1
        """Open command prompt (Windows)"""
        if not self.current_path or platform.system() != "Windows":
            return
            
        try:
            subprocess.run(["cmd", "/k", f"cd /d {self.current_path}"])
        except Exception as e:
            self.log_message(f"❌ Failed to open command prompt: {str(e)}")
            
    def show_search_dialog(self): #vers 1
        """Show file search dialog"""
        dialog = FileSearchDialog(self.current_path, self)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            # Handle search results
            pass
            
    def calculate_folder_size(self): #vers 1
        """Calculate and display folder size"""
        selected_items = self.tree.selectedItems()
        if not selected_items:
            return
            
        folder_path = selected_items[0].data(0, Qt.ItemDataRole.UserRole)
        if not os.path.isdir(folder_path):
            QMessageBox.information(self, "Folder Size", "Please select a folder.")
            return
            
        # This would show a progress dialog and calculate size
        QMessageBox.information(self, "Folder Size", "Folder size calculation not yet implemented.")
        
    # Selection methods
    def select_all_files(self): #vers 1
        """Select all files in current view"""
        self.tree.selectAll()
        
    def invert_selection(self): #vers 1
        """Invert current selection"""
        # Implementation would invert selection
        self.log_message("🔄 Selection inverted")
        
    # Settings methods
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
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
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
            self.apply_settings()
            self.log_message("🔄 Browser settings reset to defaults")

# Integration functions
def create_file_browser_widget(main_window): #vers 1
    """Create file browser widget for integration"""
    try:
        browser_widget = FileBrowserWidget(main_window)
        
        # Connect signals if main window has appropriate methods
        if hasattr(main_window, 'load_file_unified'):
            browser_widget.file_opened.connect(main_window.load_file_unified)
        
        if hasattr(main_window, 'log_message'):
            browser_widget.log_message = main_window.log_message
        
        return browser_widget
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error creating file browser: {str(e)}")
        return None


__all__ = [
    'FileBrowserWidget',
    'BrowserSettingsDialog', 
    'FilePropertiesDialog',
    'FileSearchDialog',
    'create_file_browser_widget'
]
