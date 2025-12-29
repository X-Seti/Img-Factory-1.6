#this belongs in components/File_Browser/dolphin_dialog.py - Version: 1
# X-Seti - October22 2025 - IMG Factory 1.5 - Dolphin Style File Browser

"""
Dolphin Style File Browser - Custom themed file dialog
Replaces native Qt dialogs with fully themed browser supporting:
- Single/Multi selection
- Create folder, rename, delete
- Theme integration from IMG Factory
- SVG icons
"""

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTreeWidget, QTreeWidgetItem,
    QLabel, QPushButton, QLineEdit, QComboBox, QSplitter, QMenu,
    QMessageBox, QInputDialog, QToolBar, QWidget, QHeaderView
)
from PyQt6.QtCore import Qt, pyqtSignal, QDir, QFileInfo, QSize
from PyQt6.QtGui import QIcon, QPixmap, QPainter, QColor, QFont
from PyQt6.QtSvg import QSvgRenderer
import os
import datetime

##Methods list -
# __init__
# _add_place
# _add_project_folders
# _add_storage_devices
# _add_tree_item
# _apply_colors
# _apply_default_styling
# _apply_filter
# _apply_theme_styling
# _change_view_mode
# _create_address_bar
# _create_archive_icon
# _create_back_icon
# _create_bottom_bar
# _create_cancel_icon
# _create_collision_icon
# _create_delete_icon
# _create_desktop_icon
# _create_document_icon
# _create_download_icon
# _create_drive_icon
# _create_edit_icon
# _create_export_icon
# _create_file_icon
# _create_file_tree
# _create_folder_icon
# _create_forward_icon
# _create_home_icon
# _create_image_icon
# _create_import_icon
# _create_info_panel
# _create_model_icon
# _create_new_folder
# _create_open_icon
# _create_places_sidebar
# _create_properties_icon
# _create_refresh_icon
# _create_save_icon
# _create_svg_icon
# _create_text_icon
# _create_texture_icon
# _create_toolbar
# _create_up_icon
# _delete_item
# _format_file_size
# _get_project_folder_icon
# _get_file_details
# _get_file_icon
# _go_back
# _go_forward
# _go_home
# _go_up
# _handle_action_button
# _item_double_clicked
# _load_directory
# _load_directory_silent
# _navigate_to_address
# _parse_filter
# _place_clicked
# _populate_filter_combo
# _refresh_directory
# _rename_item
# _selection_changed
# _setup_dialog_properties
# _setup_ui
# _show_context_menu
# _show_properties
# _update_info_panel
# _update_preview
# accept
# get_existing_directory
# get_open_filename
# get_open_filenames
# get_save_filename
# get_selected_path
# get_selected_paths
# reject

##Classes -
# DolphinFileDialog

class DolphinFileDialog(QDialog): #vers 1
    """Custom file dialog with Dolphin-style interface"""

    # Signals
    path_selected = pyqtSignal(str)
    paths_selected = pyqtSignal(list)

    def __init__(self, parent=None, mode='open', multi_select=False, file_filter="All Files (*.*)"): #vers 1
        """
        Initialize Dolphin file dialog

        Args:
            parent: Parent widget
            mode: 'open', 'save', 'import', or 'export'
            multi_select: Allow multiple file selection
            file_filter: File extension filter
        """
        super().__init__(parent)

        self.parent_window = parent
        self.mode = mode
        self.multi_select = multi_select
        self.file_filter = file_filter
        self.current_path = QDir.homePath()
        self.selected_items = []

        # Setup dialog
        self._setup_dialog_properties()


    def _setup_dialog_properties(self): #vers 1
        """Setup dialog window properties"""
        # Set title based on mode
        titles = {
            'open': 'Open File',
            'save': 'Save File',
            'import': 'Import Files',
            'export': 'Export Files'
        }
        self.setWindowTitle(titles.get(self.mode, 'Browse'))

        # Set size
        self.setMinimumSize(900, 600)
        self.resize(1000, 650)
        self.setModal(True)


    def get_selected_path(self): #vers 1
        """Get single selected file path"""
        if self.selected_items:
            return self.selected_items[0]
        return None


    def get_selected_paths(self): #vers 1
        """Get all selected file paths"""
        return self.selected_items


    def accept(self): #vers 1
        """Handle dialog accept"""
        if self.selected_items:
            if self.multi_select:
                self.paths_selected.emit(self.selected_items)
            else:
                self.path_selected.emit(self.selected_items[0])
        super().accept()


    def reject(self): #vers 1
        """Handle dialog cancel"""
        self.selected_items = []
        super().reject()


    def _setup_ui(self): #vers 1
        """Setup main UI layout"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Toolbar at top
        self.toolbar = self._create_toolbar()
        layout.addWidget(self.toolbar)

        # Address bar
        address_widget = self._create_address_bar()
        layout.addWidget(address_widget)

        # Main content area with splitter
        self.main_splitter = QSplitter(Qt.Orientation.Horizontal)

        # Left: Places sidebar
        self.places_widget = self._create_places_sidebar()
        self.main_splitter.addWidget(self.places_widget)

        # Center: File tree/list
        self.file_tree = self._create_file_tree()
        self.main_splitter.addWidget(self.file_tree)

        # Right: Preview/info panel
        self.info_panel = self._create_info_panel()
        self.main_splitter.addWidget(self.info_panel)

        # Set splitter proportions (20% | 50% | 30%)
        self.main_splitter.setStretchFactor(0, 2)
        self.main_splitter.setStretchFactor(1, 5)
        self.main_splitter.setStretchFactor(2, 3)

        layout.addWidget(self.main_splitter)

        # Bottom: Filename input and buttons
        bottom_widget = self._create_bottom_bar()
        layout.addWidget(bottom_widget)

        # Apply theme
        self._apply_theme_styling()

        # Load initial directory
        self._load_directory(self.current_path)


    def _create_toolbar(self): #vers 1
        """Create top toolbar with navigation and action buttons"""
        toolbar = QToolBar()
        toolbar.setMovable(False)
        toolbar.setIconSize(QSize(20, 20))

        # Back button
        self.back_btn = QPushButton()
        self.back_btn.setIcon(self._create_back_icon())
        self.back_btn.setToolTip("Go Back")
        self.back_btn.setFixedSize(32, 32)
        self.back_btn.clicked.connect(self._go_back)
        self.back_btn.setEnabled(False)
        toolbar.addWidget(self.back_btn)

        # Forward button
        self.forward_btn = QPushButton()
        self.forward_btn.setIcon(self._create_forward_icon())
        self.forward_btn.setToolTip("Go Forward")
        self.forward_btn.setFixedSize(32, 32)
        self.forward_btn.clicked.connect(self._go_forward)
        self.forward_btn.setEnabled(False)
        toolbar.addWidget(self.forward_btn)

        # Up/Parent button
        self.up_btn = QPushButton()
        self.up_btn.setIcon(self._create_up_icon())
        self.up_btn.setToolTip("Go to Parent Directory")
        self.up_btn.setFixedSize(32, 32)
        self.up_btn.clicked.connect(self._go_up)
        toolbar.addWidget(self.up_btn)

        # Refresh button
        self.refresh_btn = QPushButton()
        self.refresh_btn.setIcon(self._create_refresh_icon())
        self.refresh_btn.setToolTip("Refresh")
        self.refresh_btn.setFixedSize(32, 32)
        self.refresh_btn.clicked.connect(self._refresh_directory)
        toolbar.addWidget(self.refresh_btn)

        toolbar.addSeparator()

        # Home button
        self.home_btn = QPushButton()
        self.home_btn.setIcon(self._create_home_icon())
        self.home_btn.setToolTip("Go Home")
        self.home_btn.setFixedSize(32, 32)
        self.home_btn.clicked.connect(self._go_home)
        toolbar.addWidget(self.home_btn)

        toolbar.addSeparator()

        # View mode combo
        self.view_mode = QComboBox()
        self.view_mode.addItems(["Details", "Icons", "List"])
        self.view_mode.setCurrentIndex(0)
        self.view_mode.currentIndexChanged.connect(self._change_view_mode)
        toolbar.addWidget(self.view_mode)

        toolbar.addSeparator()

        # Create folder button
        self.new_folder_btn = QPushButton()
        self.new_folder_btn.setIcon(self._create_folder_icon())
        self.new_folder_btn.setToolTip("Create New Folder")
        self.new_folder_btn.setFixedSize(32, 32)
        self.new_folder_btn.clicked.connect(self._create_new_folder)
        toolbar.addWidget(self.new_folder_btn)

        # History tracking
        self.history = []
        self.history_index = -1

        return toolbar

    def _create_address_bar(self): #vers 1
        """Create address bar with path navigation"""
        widget = QWidget()
        layout = QHBoxLayout(widget)
        layout.setContentsMargins(5, 5, 5, 5)

        # Location icon + label
        location_label = QLabel("Location:")
        layout.addWidget(location_label)

        # Path input
        self.address_input = QLineEdit()
        self.address_input.setPlaceholderText("Enter path or browse...")
        self.address_input.setText(self.current_path)
        self.address_input.returnPressed.connect(self._navigate_to_address)
        layout.addWidget(self.address_input)

        # Go button
        go_btn = QPushButton("Go")
        go_btn.setFixedWidth(60)
        go_btn.clicked.connect(self._navigate_to_address)
        layout.addWidget(go_btn)

        # Filter combo
        self.filter_combo = QComboBox()
        self.filter_combo.setMinimumWidth(150)
        self._populate_filter_combo()
        self.filter_combo.currentIndexChanged.connect(self._apply_filter)
        layout.addWidget(self.filter_combo)

        return widget


    def _populate_filter_combo(self): #vers 1
        """Populate file filter dropdown"""
        filters = [
            "All Files (*.*)",
            "IMG Archives (*.img)",
            "TXD Textures (*.txd)",
            "DFF Models (*.dff)",
            "COL Collision (*.col)",
            "Images (*.png *.jpg *.bmp *.dds)",
            "Archives (*.zip *.rar *.7z)"
        ]

        # Add custom filter if provided
        if self.file_filter and self.file_filter not in filters:
            filters.insert(0, self.file_filter)

        self.filter_combo.addItems(filters)

        # Set current filter
        if self.file_filter:
            index = self.filter_combo.findText(self.file_filter)
            if index >= 0:
                self.filter_combo.setCurrentIndex(index)


    def _add_project_folders(self): #vers 1
        """Add project folders from IMG Factory settings"""
        try:
            # Try to get project folders from parent window settings
            if hasattr(self.parent_window, 'app_settings'):
                settings = self.parent_window.app_settings.current_settings

                project_folders = {
                    'GTA Folder': settings.get('working_gta_folder', ''),
                    'Assists': settings.get('assists_folder', ''),
                    'Textures': settings.get('textures_folder', ''),
                    'Collisions': settings.get('collisions_folder', ''),
                    'Generics': settings.get('generics_folder', ''),
                    'Water': settings.get('water_folder', ''),
                    'Radar': settings.get('radar_folder', ''),
                    'Game Art': settings.get('gameart_folder', ''),
                    'Peds': settings.get('peds_folder', ''),
                    'Vehicles': settings.get('vehicles_folder', ''),
                    'Weapons': settings.get('weapons_folder', '')
                }

                # Add folders that exist
                for name, path in project_folders.items():
                    if path and os.path.exists(path):
                        icon = self._get_project_folder_icon(name)
                        item = QTreeWidgetItem(self.project_tree)
                        item.setText(0, name)
                        item.setData(0, Qt.ItemDataRole.UserRole, path)
                        item.setIcon(0, icon)
            else:
                # Standalone mode - show message
                item = QTreeWidgetItem(self.project_tree)
                item.setText(0, "No project loaded")
                item.setDisabled(True)

        except Exception as e:
            print(f"Project folders error: {e}")


    def _get_project_folder_icon(self, folder_name): #vers 1
        """Get appropriate icon for project folder type"""
        folder_name_lower = folder_name.lower()

        if 'texture' in folder_name_lower:
            return self._create_texture_icon()
        elif 'collision' in folder_name_lower:
            return self._create_collision_icon()
        elif 'vehicle' in folder_name_lower or 'ped' in folder_name_lower:
            return self._create_model_icon()
        elif 'weapon' in folder_name_lower:
            return self._create_file_icon()
        elif 'art' in folder_name_lower or 'radar' in folder_name_lower:
            return self._create_image_icon()
        elif 'gta' in folder_name_lower:
            return self._create_home_icon()
        else:
            return self._create_folder_icon()


    def _add_storage_devices(self): #vers 2
        """Add storage devices to devices tree"""
        drives = QDir.drives()

        for drive in drives:
            drive_path = drive.absolutePath()
            drive_name = drive_path.replace("/", "").replace("\\", "") or "Root"

            # Create drive item in devices tree
            item = QTreeWidgetItem(self.devices_tree)
            item.setText(0, f"Drive {drive_name}")
            item.setData(0, Qt.ItemDataRole.UserRole, drive_path)
            item.setIcon(0, self._create_drive_icon())

    def _create_places_sidebar(self): #vers 2
        """Create left sidebar with common places and devices"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        layout.setContentsMargins(5, 5, 5, 5)

        # Places label
        places_label = QLabel("Places")
        places_label.setStyleSheet("font-weight: bold; font-size: 11px; padding: 5px 0px;")
        layout.addWidget(places_label)

        # Places tree
        self.places_tree = QTreeWidget()
        self.places_tree.setHeaderHidden(True)
        self.places_tree.setMaximumWidth(200)
        self.places_tree.itemClicked.connect(self._place_clicked)

        # Add common places
        self._add_place("Home", QDir.homePath(), self._create_home_icon())
        self._add_place("Desktop", QDir.homePath() + "/Desktop", self._create_desktop_icon())
        self._add_place("Documents", QDir.homePath() + "/Documents", self._create_document_icon())
        self._add_place("Downloads", QDir.homePath() + "/Downloads", self._create_download_icon())
        self._add_place("Pictures", QDir.homePath() + "/Pictures", self._create_image_icon())

        layout.addWidget(self.places_tree)

        # Devices label
        devices_label = QLabel("Devices")
        devices_label.setStyleSheet("font-weight: bold; font-size: 11px; padding: 5px 0px;")
        layout.addWidget(devices_label)

        # Devices tree
        self.devices_tree = QTreeWidget()
        self.devices_tree.setHeaderHidden(True)
        self.devices_tree.setMaximumWidth(200)
        self.devices_tree.itemClicked.connect(self._place_clicked)

        # Add storage devices
        self._add_storage_devices()

        layout.addWidget(self.devices_tree)

        # Project Folders label
        project_label = QLabel("Project Folders")
        project_label.setStyleSheet("font-weight: bold; font-size: 11px; padding: 5px 0px;")
        layout.addWidget(project_label)

        # Project folders tree
        self.project_tree = QTreeWidget()
        self.project_tree.setHeaderHidden(True)
        self.project_tree.setMaximumWidth(200)
        self.project_tree.itemClicked.connect(self._place_clicked)

        # Add project folders from settings
        self._add_project_folders()

        layout.addWidget(self.project_tree)

        layout.addStretch()

        return widget


    def _add_place(self, name, path, icon): #vers 1
        """Add a place to sidebar"""
        item = QTreeWidgetItem(self.places_tree)
        item.setText(0, name)
        item.setData(0, Qt.ItemDataRole.UserRole, path)
        item.setIcon(0, icon)
        return item


    def _add_storage_devices(self): #vers 1
        """Add storage devices to places"""
        drives = QDir.drives()

        for drive in drives:
            drive_path = drive.absolutePath()
            drive_name = drive_path.replace("/", "").replace("\\", "") or "Root"

            # Create drive item
            item = QTreeWidgetItem(self.places_tree)
            item.setText(0, f"Drive {drive_name}")
            item.setData(0, Qt.ItemDataRole.UserRole, drive_path)
            item.setIcon(0, self._create_drive_icon())


    def _create_file_tree(self): #vers 1
        """Create main file/folder tree view"""
        self.tree = QTreeWidget()

        # Set headers
        headers = ["Name", "Size", "Type", "Date Modified"]
        self.tree.setHeaderLabels(headers)

        # Configure tree
        self.tree.setRootIsDecorated(False)
        self.tree.setAlternatingRowColors(True)
        self.tree.setSortingEnabled(True)
        self.tree.setSelectionMode(
            QTreeWidget.SelectionMode.ExtendedSelection if self.multi_select
            else QTreeWidget.SelectionMode.SingleSelection
        )

        # Set column widths
        self.tree.setColumnWidth(0, 300)  # Name
        self.tree.setColumnWidth(1, 100)  # Size
        self.tree.setColumnWidth(2, 120)  # Type
        self.tree.setColumnWidth(3, 150)  # Date

        # Connect signals
        self.tree.itemDoubleClicked.connect(self._item_double_clicked)
        self.tree.itemSelectionChanged.connect(self._selection_changed)
        self.tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.tree.customContextMenuRequested.connect(self._show_context_menu)

        return self.tree


    def _load_directory(self, path): #vers 1
        """Load directory contents into tree"""
        self.tree.clear()
        self.current_path = path
        self.address_input.setText(path)

        # Update history
        if not self.history or self.history[self.history_index] != path:
            # Remove forward history if we navigate from middle
            if self.history_index < len(self.history) - 1:
                self.history = self.history[:self.history_index + 1]
            self.history.append(path)
            self.history_index = len(self.history) - 1

        # Update navigation buttons
        self.back_btn.setEnabled(self.history_index > 0)
        self.forward_btn.setEnabled(self.history_index < len(self.history) - 1)
        self.up_btn.setEnabled(QDir(path).cdUp())

        # Get directory info
        dir_info = QDir(path)

        # Get current filter
        filter_text = self.filter_combo.currentText()
        name_filters = self._parse_filter(filter_text)

        # Set filters
        dir_info.setNameFilters(name_filters)
        dir_info.setFilter(QDir.Filter.AllEntries | QDir.Filter.NoDotAndDotDot)
        dir_info.setSorting(QDir.SortFlag.DirsFirst | QDir.SortFlag.Name | QDir.SortFlag.IgnoreCase)

        # Add parent directory (..) if not at root
        if dir_info.cdUp():
            parent_item = QTreeWidgetItem(self.tree)
            parent_item.setText(0, "..")
            parent_item.setIcon(0, self._create_up_icon())
            parent_item.setData(0, Qt.ItemDataRole.UserRole, dir_info.absolutePath())
            dir_info.cd(path)  # Go back to current

        # Load entries
        entries = dir_info.entryInfoList()

        for entry in entries:
            self._add_tree_item(entry)


    def _add_tree_item(self, file_info): #vers 1
        """Add file/folder item to tree"""
        item = QTreeWidgetItem(self.tree)

        # Name
        item.setText(0, file_info.fileName())
        item.setData(0, Qt.ItemDataRole.UserRole, file_info.absoluteFilePath())

        # Icon
        if file_info.isDir():
            item.setIcon(0, self._create_folder_icon())
        else:
            item.setIcon(0, self._get_file_icon(file_info.suffix()))

        # Size
        if file_info.isFile():
            size = file_info.size()
            item.setText(1, self._format_file_size(size))
            item.setData(1, Qt.ItemDataRole.UserRole, size)  # For sorting
        else:
            item.setText(1, "--")

        # Type
        if file_info.isDir():
            item.setText(2, "Folder")
        else:
            suffix = file_info.suffix().upper()
            item.setText(2, f"{suffix} File" if suffix else "File")

        # Date modified
        modified = file_info.lastModified().toString("yyyy-MM-dd HH:mm")
        item.setText(3, modified)

        return item


    def _create_info_panel(self): #vers 1
        """Create right info/preview panel"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        layout.setContentsMargins(5, 5, 5, 5)

        # Info label
        info_label = QLabel("Information")
        info_label.setStyleSheet("font-weight: bold; font-size: 11px;")
        layout.addWidget(info_label)

        # Preview area
        self.preview_label = QLabel("No selection")
        self.preview_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.preview_label.setMinimumHeight(200)
        self.preview_label.setMaximumHeight(250)
        self.preview_label.setStyleSheet("border: 1px solid #cccccc; background: #f5f5f5;")
        layout.addWidget(self.preview_label)

        # File details
        details_group = QWidget()
        details_layout = QVBoxLayout(details_group)
        details_layout.setSpacing(8)

        self.name_label = QLabel("Name: --")
        self.type_label = QLabel("Type: --")
        self.size_label = QLabel("Size: --")
        self.modified_label = QLabel("Modified: --")
        self.path_label = QLabel("Path: --")
        self.path_label.setWordWrap(True)

        details_layout.addWidget(self.name_label)
        details_layout.addWidget(self.type_label)
        details_layout.addWidget(self.size_label)
        details_layout.addWidget(self.modified_label)
        details_layout.addWidget(self.path_label)

        layout.addWidget(details_group)
        layout.addStretch()

        return widget


    def _create_info_panel(self): #vers 1
        """Create right info/preview panel"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        layout.setContentsMargins(5, 5, 5, 5)

        # Info label
        info_label = QLabel("Information")
        info_label.setStyleSheet("font-weight: bold; font-size: 11px;")
        layout.addWidget(info_label)

        # Preview area
        self.preview_label = QLabel("No selection")
        self.preview_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.preview_label.setMinimumHeight(200)
        self.preview_label.setMaximumHeight(250)
        self.preview_label.setStyleSheet("border: 1px solid #cccccc; background: #f5f5f5;")
        layout.addWidget(self.preview_label)

        # File details
        details_group = QWidget()
        details_layout = QVBoxLayout(details_group)
        details_layout.setSpacing(8)

        self.name_label = QLabel("Name: --")
        self.type_label = QLabel("Type: --")
        self.size_label = QLabel("Size: --")
        self.modified_label = QLabel("Modified: --")
        self.path_label = QLabel("Path: --")
        self.path_label.setWordWrap(True)

        details_layout.addWidget(self.name_label)
        details_layout.addWidget(self.type_label)
        details_layout.addWidget(self.size_label)
        details_layout.addWidget(self.modified_label)
        details_layout.addWidget(self.path_label)

        layout.addWidget(details_group)
        layout.addStretch()

        return widget

    def _update_info_panel(self, file_path): #vers 1
        """Update info panel with file details"""
        if not file_path or not os.path.exists(file_path):
            self.preview_label.setText("No selection")
            self.name_label.setText("Name: --")
            self.type_label.setText("Type: --")
            self.size_label.setText("Size: --")
            self.modified_label.setText("Modified: --")
            self.path_label.setText("Path: --")
            return

        file_info = QFileInfo(file_path)

        # Update details
        self.name_label.setText(f"Name: {file_info.fileName()}")

        if file_info.isDir():
            self.type_label.setText("Type: Folder")
            self.size_label.setText("Size: --")
        else:
            suffix = file_info.suffix().upper()
            self.type_label.setText(f"Type: {suffix} File" if suffix else "Type: File")
            self.size_label.setText(f"Size: {self._format_file_size(file_info.size())}")

        modified = file_info.lastModified().toString("yyyy-MM-dd HH:mm:ss")
        self.modified_label.setText(f"Modified: {modified}")
        self.path_label.setText(f"Path: {file_path}")

        # Update preview
        self._update_preview(file_path, file_info)

    def _update_preview(self, file_path, file_info): #vers 2
        """Update preview image or file info"""
        if file_info.isDir():
            self.preview_label.setText("Folder")
            self.preview_label.setPixmap(QPixmap())
            return

        # Check if image file
        image_extensions = ['.png', '.jpg', '.jpeg', '.bmp', '.gif', '.dds']
        suffix = file_info.suffix().lower()

        if f".{suffix}" in image_extensions:
            try:
                pixmap = QPixmap(file_path)
                if not pixmap.isNull():
                    # Scale to fit preview
                    scaled = pixmap.scaled(
                        self.preview_label.size(),
                        Qt.AspectRatioMode.KeepAspectRatio,
                        Qt.TransformationMode.SmoothTransformation
                    )
                    self.preview_label.setPixmap(scaled)
                    return
            except:
                pass

        # Use file command to get detailed info
        file_details = self._get_file_details(file_path)
        if file_details:
            self.preview_label.setText(file_details)
            self.preview_label.setWordWrap(True)
            self.preview_label.setAlignment(Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignLeft)
            self.preview_label.setStyleSheet("padding: 10px; font-size: 10px;")
        else:
            # Fallback: show file type
            self.preview_label.setText(f"{file_info.suffix().upper()}\nFile")

        self.preview_label.setPixmap(QPixmap())

    def _get_file_details(self, file_path): #vers 1
        """Get detailed file information using system commands"""
        import subprocess
        import platform

        system = platform.system()
        details = []

        try:
            if system == "Linux":
                # Use 'file' command
                result = subprocess.run(['file', '-b', file_path],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0:
                    details.append(result.stdout.strip())

            elif system == "Windows":
                # Use PowerShell Get-Item
                ps_cmd = f'Get-Item "{file_path}" | Select-Object -ExpandProperty VersionInfo | Format-List'
                result = subprocess.run(['powershell', '-Command', ps_cmd],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0 and result.stdout.strip():
                    details.append(result.stdout.strip()[:200])  # Limit output
                else:
                    # Fallback: basic info
                    details.append(f"Windows file\n{os.path.splitext(file_path)[1].upper()} format")

            elif system == "Darwin":  # macOS
                # Use 'file' command (available on macOS)
                result = subprocess.run(['file', '-b', file_path],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0:
                    details.append(result.stdout.strip())

                # Also try mdls for additional metadata
                result = subprocess.run(['mdls', '-name', 'kMDItemContentType', file_path],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0:
                    content_type = result.stdout.strip().split('=')[-1].strip().strip('"')
                    if content_type and content_type != '(null)':
                        details.append(f"Type: {content_type}")

            return "\n".join(details) if details else None

        except Exception as e:
            print(f"File details error: {e}")
            return None

    def _get_file_details(self, file_path): #vers 1
        """Get detailed file information using system commands"""
        import subprocess
        import platform

        system = platform.system()
        details = []

        try:
            if system == "Linux":
                # Use 'file' command
                result = subprocess.run(['file', '-b', file_path],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0:
                    details.append(result.stdout.strip())

            elif system == "Windows":
                # Use PowerShell Get-Item
                ps_cmd = f'Get-Item "{file_path}" | Select-Object -ExpandProperty VersionInfo | Format-List'
                result = subprocess.run(['powershell', '-Command', ps_cmd],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0 and result.stdout.strip():
                    details.append(result.stdout.strip()[:200])  # Limit output
                else:
                    # Fallback: basic info
                    details.append(f"Windows file\n{os.path.splitext(file_path)[1].upper()} format")

            elif system == "Darwin":  # macOS
                # Use 'file' command (available on macOS)
                result = subprocess.run(['file', '-b', file_path],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0:
                    details.append(result.stdout.strip())

                # Also try mdls for additional metadata
                result = subprocess.run(['mdls', '-name', 'kMDItemContentType', file_path],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0:
                    content_type = result.stdout.strip().split('=')[-1].strip().strip('"')
                    if content_type and content_type != '(null)':
                        details.append(f"Type: {content_type}")

            return "\n".join(details) if details else None

        except Exception as e:
            print(f"File details error: {e}")
            return None

    def _create_bottom_bar(self): #vers 1
        """Create bottom bar with filename input and action buttons"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        layout.setContentsMargins(5, 5, 5, 5)

        # Filename input row (for save mode)
        if self.mode in ['save', 'export']:
            filename_layout = QHBoxLayout()

            filename_label = QLabel("Filename:")
            filename_layout.addWidget(filename_label)

            self.filename_input = QLineEdit()
            self.filename_input.setPlaceholderText("Enter filename...")
            filename_layout.addWidget(self.filename_input)

            layout.addLayout(filename_layout)

        # Selection info and buttons
        button_layout = QHBoxLayout()

        # Selection count label
        self.selection_label = QLabel("No items selected")
        button_layout.addWidget(self.selection_label)

        button_layout.addStretch()

        # Action buttons based on mode
        if self.mode == 'open':
            self.action_btn = QPushButton("Open")
            self.action_btn.setIcon(self._create_open_icon())
        elif self.mode == 'save':
            self.action_btn = QPushButton("Save")
            self.action_btn.setIcon(self._create_save_icon())
        elif self.mode == 'import':
            self.action_btn = QPushButton("Import")
            self.action_btn.setIcon(self._create_import_icon())
        elif self.mode == 'export':
            self.action_btn = QPushButton("Export")
            self.action_btn.setIcon(self._create_export_icon())

        self.action_btn.setFixedWidth(100)
        self.action_btn.setFixedHeight(32)
        self.action_btn.clicked.connect(self._handle_action_button)
        self.action_btn.setEnabled(False)
        button_layout.addWidget(self.action_btn)

        # Cancel button
        cancel_btn = QPushButton("Cancel")
        cancel_btn.setIcon(self._create_cancel_icon())
        cancel_btn.setFixedWidth(100)
        cancel_btn.setFixedHeight(32)
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)

        layout.addLayout(button_layout)

        return widget

    def _handle_action_button(self): #vers 1
        """Handle main action button click"""
        if self.mode in ['save', 'export']:
            # Get filename from input
            if hasattr(self, 'filename_input'):
                filename = self.filename_input.text().strip()
                if not filename:
                    QMessageBox.warning(self, "No Filename", "Please enter a filename.")
                    return

                # Build full path
                full_path = os.path.join(self.current_path, filename)

                # Check if file exists
                if os.path.exists(full_path):
                    reply = QMessageBox.question(
                        self,
                        "File Exists",
                        f"File '{filename}' already exists.\nOverwrite?",
                        QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                    )
                    if reply == QMessageBox.StandardButton.No:
                        return

                self.selected_items = [full_path]
        else:
            # Get selected items from tree
            selected = self.tree.selectedItems()
            if not selected:
                QMessageBox.warning(self, "No Selection", "Please select a file or folder.")
                return

            self.selected_items = []
            for item in selected:
                path = item.data(0, Qt.ItemDataRole.UserRole)
                if path and path != "..":
                    self.selected_items.append(path)

        # Accept dialog
        self.accept()

    def _selection_changed(self): #vers 1
        """Handle tree selection change"""
        selected = self.tree.selectedItems()
        count = len(selected)

        # Update selection label
        if count == 0:
            self.selection_label.setText("No items selected")
            self.action_btn.setEnabled(False)
        elif count == 1:
            self.selection_label.setText("1 item selected")
            self.action_btn.setEnabled(True)

            # Update info panel
            item = selected[0]
            path = item.data(0, Qt.ItemDataRole.UserRole)
            if path and path != "..":
                self._update_info_panel(path)
        else:
            self.selection_label.setText(f"{count} items selected")
            self.action_btn.setEnabled(True)
            self.preview_label.setText(f"{count} items\nselected")
            self.preview_label.setPixmap(QPixmap())

    def _item_double_clicked(self, item, column): #vers 1
        """Handle tree item double click"""
        path = item.data(0, Qt.ItemDataRole.UserRole)

        if not path:
            return

        # Check if it's parent directory (..)
        if item.text(0) == "..":
            self._go_up()
            return

        file_info = QFileInfo(path)

        # If directory, navigate into it
        if file_info.isDir():
            self._load_directory(path)
        else:
            # If file and in open mode, accept selection
            if self.mode in ['open', 'import']:
                self.selected_items = [path]
                self.accept()

    def _go_back(self): #vers 1
        """Navigate to previous directory in history"""
        if self.history_index > 0:
            self.history_index -= 1
            path = self.history[self.history_index]
            self._load_directory_silent(path)

    def _go_forward(self): #vers 1
        """Navigate to next directory in history"""
        if self.history_index < len(self.history) - 1:
            self.history_index += 1
            path = self.history[self.history_index]
            self._load_directory_silent(path)

    def _go_up(self): #vers 1
        """Navigate to parent directory"""
        dir_info = QDir(self.current_path)
        if dir_info.cdUp():
            self._load_directory(dir_info.absolutePath())

    def _go_home(self): #vers 1
        """Navigate to home directory"""
        self._load_directory(QDir.homePath())

    def _refresh_directory(self): #vers 1
        """Refresh current directory"""
        self._load_directory_silent(self.current_path)

    def _navigate_to_address(self): #vers 1
        """Navigate to path entered in address bar"""
        path = self.address_input.text().strip()

        if not path:
            return

        if os.path.exists(path) and os.path.isdir(path):
            self._load_directory(path)
        else:
            QMessageBox.warning(self, "Invalid Path", f"Path does not exist:\n{path}")
            self.address_input.setText(self.current_path)

    def _place_clicked(self, item, column): #vers 1
        """Handle places sidebar click"""
        path = item.data(0, Qt.ItemDataRole.UserRole)
        if path and os.path.exists(path):
            self._load_directory(path)

    def _load_directory_silent(self, path): #vers 1
        """Load directory without adding to history"""
        old_index = self.history_index
        self._load_directory(path)
        self.history_index = old_index

    def _change_view_mode(self, index): #vers 1
        """Change view mode (Details/Icons/List)"""
        # TODO: Implement different view modes
        # For now, only details view is implemented
        pass


    def _show_context_menu(self, position): #vers 1
        """Show context menu for file operations"""
        item = self.tree.itemAt(position)

        menu = QMenu(self)

        if item:
            path = item.data(0, Qt.ItemDataRole.UserRole)
            file_info = QFileInfo(path)

            # Open action
            open_action = menu.addAction(self._create_open_icon(), "Open")
            open_action.triggered.connect(lambda: self._item_double_clicked(item, 0))

            menu.addSeparator()

            # Rename action
            rename_action = menu.addAction(self._create_edit_icon(), "Rename")
            rename_action.triggered.connect(lambda: self._rename_item(item))

            # Delete action
            delete_action = menu.addAction(self._create_delete_icon(), "Delete")
            delete_action.triggered.connect(lambda: self._delete_item(item))

            menu.addSeparator()

            # Properties action
            props_action = menu.addAction(self._create_properties_icon(), "Properties")
            props_action.triggered.connect(lambda: self._show_properties(item))
        else:
            # Empty space context menu
            new_folder_action = menu.addAction(self._create_folder_icon(), "New Folder")
            new_folder_action.triggered.connect(self._create_new_folder)

            menu.addSeparator()

            refresh_action = menu.addAction(self._create_refresh_icon(), "Refresh")
            refresh_action.triggered.connect(self._refresh_directory)

        menu.exec(self.tree.viewport().mapToGlobal(position))


    def _create_new_folder(self): #vers 1
        """Create new folder in current directory"""
        folder_name, ok = QInputDialog.getText(
            self,
            "New Folder",
            "Enter folder name:",
            QLineEdit.EchoMode.Normal,
            "New Folder"
        )

        if ok and folder_name:
            new_path = os.path.join(self.current_path, folder_name)

            if os.path.exists(new_path):
                QMessageBox.warning(self, "Folder Exists", f"Folder '{folder_name}' already exists.")
                return

            try:
                os.makedirs(new_path)
                self._refresh_directory()

                if hasattr(self.parent_window, 'log_message'):
                    self.parent_window.log_message(f"Created folder: {folder_name}")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to create folder:\n{str(e)}")


    def _rename_item(self, item): #vers 1
        """Rename selected file or folder"""
        old_path = item.data(0, Qt.ItemDataRole.UserRole)
        old_name = item.text(0)

        new_name, ok = QInputDialog.getText(
            self,
            "Rename",
            "Enter new name:",
            QLineEdit.EchoMode.Normal,
            old_name
        )

        if ok and new_name and new_name != old_name:
            new_path = os.path.join(os.path.dirname(old_path), new_name)

            if os.path.exists(new_path):
                QMessageBox.warning(self, "Name Exists", f"'{new_name}' already exists.")
                return

            try:
                os.rename(old_path, new_path)
                self._refresh_directory()

                if hasattr(self.parent_window, 'log_message'):
                    self.parent_window.log_message(f"Renamed: {old_name} → {new_name}")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to rename:\n{str(e)}")


    def _delete_item(self, item): #vers 1
        """Delete selected file or folder"""
        path = item.data(0, Qt.ItemDataRole.UserRole)
        name = item.text(0)

        file_info = QFileInfo(path)
        item_type = "folder" if file_info.isDir() else "file"

        reply = QMessageBox.question(
            self,
            "Confirm Delete",
            f"Delete {item_type} '{name}'?\nThis action cannot be undone.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            try:
                if file_info.isDir():
                    import shutil
                    shutil.rmtree(path)
                else:
                    os.remove(path)

                self._refresh_directory()

                if hasattr(self.parent_window, 'log_message'):
                    self.parent_window.log_message(f"Deleted: {name}")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to delete:\n{str(e)}")


    def _show_properties(self, item): #vers 1
        """Show file/folder properties dialog"""
        path = item.data(0, Qt.ItemDataRole.UserRole)
        file_info = QFileInfo(path)

        props_text = f"Name: {file_info.fileName()}\n"
        props_text += f"Path: {file_info.absolutePath()}\n"
        props_text += f"Type: {'Folder' if file_info.isDir() else 'File'}\n"

        if file_info.isFile():
            props_text += f"Size: {self._format_file_size(file_info.size())}\n"

        props_text += f"Modified: {file_info.lastModified().toString('yyyy-MM-dd HH:mm:ss')}\n"
        props_text += f"Permissions: {'Read' if file_info.isReadable() else ''}"
        props_text += f"{' Write' if file_info.isWritable() else ''}"
        props_text += f"{' Execute' if file_info.isExecutable() else ''}"

        QMessageBox.information(self, "Properties", props_text)


    def _parse_filter(self, filter_text): #vers 1
        """Parse filter text into name filters list"""
        # Extract patterns from filter text like "Images (*.png *.jpg)"
        import re
        patterns = re.findall(r'\*\.\w+', filter_text)

        if patterns:
            return patterns
        else:
            return ["*"]  # All files

    def _format_file_size(self, size): #vers 1
        """Format file size in human readable format"""
        for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
            if size < 1024.0:
                return f"{size:.1f} {unit}"
            size /= 1024.0
        return f"{size:.1f} PB"

    def _get_file_icon(self, extension): #vers 1
        """Get appropriate icon for file extension"""
        ext = extension.lower()

        # Image files
        if ext in ['png', 'jpg', 'jpeg', 'bmp', 'gif', 'dds']:
            return self._create_image_icon()

        # Archive files
        elif ext in ['img', 'zip', 'rar', '7z']:
            return self._create_archive_icon()

        # Model files
        elif ext in ['dff', 'obj', 'fbx']:
            return self._create_model_icon()

        # Texture files
        elif ext == 'txd':
            return self._create_texture_icon()

        # Collision files
        elif ext == 'col':
            return self._create_collision_icon()

        # Text files
        elif ext in ['txt', 'log', 'ini', 'cfg']:
            return self._create_text_icon()

        # Default file icon
        else:
            return self._create_file_icon()

    def _apply_filter(self): #vers 1
        """Apply current file filter"""
        self._refresh_directory()

    def _apply_theme_styling(self): #vers 2
        """Apply IMG Factory theme to dialog"""
        try:
            # Get theme colors from parent
            if hasattr(self.parent_window, 'app_settings'):
                theme_name = self.parent_window.app_settings.current_settings.get("theme", "IMG_Factory")
                theme_data = self.parent_window.app_settings.themes.get(theme_name, {})
                colors = theme_data.get('colors', {})

                if colors:
                    self._apply_colors(colors)
                    return
        except Exception as e:
            print(f"Theme error: {e}")

        # Detect system theme
        from PyQt6.QtWidgets import QApplication
        from PyQt6.QtGui import QPalette

        palette = QApplication.palette()
        is_dark = palette.color(QPalette.ColorRole.Window).lightness() < 128

        if is_dark:
            self._apply_dark_fallback()
        else:
            self._apply_light_fallback()

    def _apply_dark_fallback(self): #vers 2
        """Apply dark theme fallback for standalone mode"""
        colors = {
            'bg_primary': '#2b2b2b',
            'bg_secondary': '#3c3c3c',
            'bg_tertiary': '#4a4a4a',
            'panel_bg': '#333333',
            'text_primary': '#ffffff',
            'text_secondary': '#cccccc',
            'text_accent': '#60A5FA',
            'accent_primary': '#FFECEE',
            'accent_secondary': '#FFD4D9',
            'border': '#666666',
            'button_normal': '#404040',
            'button_hover': '#505050',
            'button_pressed': '#303030',
            'selection_background': '#0078d4',
            'selection_text': '#ffffff',
            'table_row_even': '#2b2b2b',
            'table_row_odd': '#353535'
        }
        self._apply_colors(colors)

    def _apply_light_fallback(self): #vers 2
        """Apply light theme fallback for standalone mode"""
        colors = {
            'bg_primary': '#ffffff',
            'bg_secondary': '#f5f5f5',
            'bg_tertiary': '#e9ecef',
            'panel_bg': '#f0f0f0',
            'text_primary': '#000000',
            'text_secondary': '#666666',
            'text_accent': '#0066cc',
            'accent_primary': '#0078d4',
            'accent_secondary': '#0A7Ad4',
            'border': '#cccccc',
            'button_normal': '#e0e0e0',
            'button_hover': '#d0d0d0',
            'button_pressed': '#b0b0b0',
            'selection_background': '#0078d4',
            'selection_text': '#ffffff',
            'table_row_even': '#ffffff',
            'table_row_odd': '#f8f9fa'
        }
        self._apply_colors(colors)


    def _apply_colors(self, colors): #vers 2
        """Apply theme colors to dialog components"""
        bg_primary = colors.get('bg_primary', '#ffffff')
        bg_secondary = colors.get('bg_secondary', '#f5f5f5')
        bg_tertiary = colors.get('bg_tertiary', '#e9ecef')
        panel_bg = colors.get('panel_bg', '#f0f0f0')
        text_primary = colors.get('text_primary', '#000000')
        text_secondary = colors.get('text_secondary', '#666666')
        text_accent = colors.get('text_accent', '#0066cc')
        accent = colors.get('accent_primary', '#0078d4')
        accent_secondary = colors.get('accent_secondary', '#0A7Ad4')
        border = colors.get('border', '#cccccc')
        button_normal = colors.get('button_normal', '#e0e0e0')
        button_hover = colors.get('button_hover', '#d0d0d0')
        button_pressed = colors.get('button_pressed', '#b0b0b0')
        selection_bg = colors.get('selection_background', '#0078d4')
        selection_text = colors.get('selection_text', '#ffffff')
        table_row_even = colors.get('table_row_even', bg_primary)
        table_row_odd = colors.get('table_row_odd', bg_secondary)

        dialog_style = f"""
            QDialog {{
                background-color: {bg_primary};
                color: {text_primary};
            }}
            QTreeWidget {{
                background-color: {bg_primary};
                alternate-background-color: {bg_secondary};
                border: 1px solid {border};
                color: {text_primary};
                selection-background-color: {selection_bg};
                selection-color: {selection_text};
            }}
            QTreeWidget::item {{
                padding: 4px;
            }}
            QTreeWidget::item:hover {{
                background-color: {bg_secondary};
            }}
            QTreeWidget::item:alternate {{
                background-color: {table_row_odd};
            }}
            QHeaderView::section {{
                background-color: {bg_secondary};
                color: {text_primary};
                border: 1px solid {border};
                padding: 5px;
                font-weight: bold;
            }}
            QPushButton {{
                background-color: {button_normal};
                color: {text_primary};
                border: 1px solid {border};
                border-radius: 4px;
                padding: 5px 10px;
            }}
            QPushButton:hover {{
                background-color: {button_hover};
            }}
            QPushButton:pressed {{
                background-color: {button_pressed};
            }}
            QPushButton:disabled {{
                background-color: {bg_secondary};
                color: {text_secondary};
            }}
            QLineEdit, QComboBox {{
                background-color: {bg_primary};
                color: {text_primary};
                border: 1px solid {border};
                border-radius: 3px;
                padding: 4px;
            }}
            QLineEdit:focus, QComboBox:focus {{
                border: 1px solid {accent};
            }}
            QLabel {{
                color: {text_primary};
            }}
            QToolBar {{
                background-color: {bg_secondary};
                border-bottom: 1px solid {border};
                spacing: 3px;
                padding: 2px;
            }}
            QWidget {{
                background-color: {panel_bg};
            }}
        """

        self.setStyleSheet(dialog_style)


    def _apply_default_styling(self): #vers 2
        """Apply default styling if theme not available"""
        default_style = """
            QDialog {
                background-color: #ffffff;
                color: #000000;
            }
            QTreeWidget {
                background-color: #ffffff;
                alternate-background-color: #f5f5f5;
                border: 1px solid #cccccc;
                color: #000000;
                selection-background-color: #0078d4;
                selection-color: #ffffff;
            }
            QTreeWidget::item {
                padding: 4px;
            }
            QTreeWidget::item:hover {
                background-color: #e8e8e8;
            }
            QTreeWidget::item:selected {
                background-color: #0078d4;
                color: #ffffff;
            }
            QHeaderView::section {
                background-color: #f5f5f5;
                color: #000000;
                border: 1px solid #cccccc;
                padding: 5px;
                font-weight: bold;
            }
            QPushButton {
                background-color: #e0e0e0;
                color: #000000;
                border: 1px solid #999999;
                border-radius: 4px;
                padding: 5px 10px;
            }
            QPushButton:hover {
                background-color: #d0d0d0;
            }
            QPushButton:pressed {
                background-color: #b0b0b0;
            }
            QLineEdit, QComboBox {
                background-color: #ffffff;
                color: #000000;
                border: 1px solid #cccccc;
                border-radius: 3px;
                padding: 4px;
            }
            QLabel {
                color: #000000;
            }
        """
        self.setStyleSheet(default_style)

    def _create_svg_icon(self, svg_data, size=20): #vers 1
        """Convert SVG data to QIcon"""
        from PyQt6.QtCore import QSize
        from PyQt6.QtGui import QPixmap, QPainter
        from PyQt6.QtSvg import QSvgRenderer

        renderer = QSvgRenderer(svg_data)
        pixmap = QPixmap(QSize(size, size))
        pixmap.fill(Qt.GlobalColor.transparent)

        painter = QPainter(pixmap)
        renderer.render(painter)
        painter.end()

        return QIcon(pixmap)

    def _create_folder_icon(self): #vers 1
        """Folder icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-7l-2-2H5a2 2 0 00-2 2z"
                stroke="currentColor" stroke-width="2" stroke-linejoin="round" fill="none"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_file_icon(self): #vers 1
        """Generic file icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M14 2v6h6" stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_image_icon(self): #vers 1
        """Image file icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <rect x="3" y="3" width="18" height="18" rx="2"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <circle cx="8.5" cy="8.5" r="1.5" fill="currentColor"/>
            <path d="M21 15l-5-5L5 21" stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_archive_icon(self): #vers 1
        """Archive file icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M21 16V8a2 2 0 00-1-1.73l-7-4a2 2 0 00-2 0l-7 4A2 2 0 003 8v8a2 2 0 001 1.73l7 4a2 2 0 002 0l7-4A2 2 0 0021 16z"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M3.27 6.96L12 12.01l8.73-5.05M12 22.08V12"
                stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_model_icon(self): #vers 1
        """3D model file icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M12 2L2 7l10 5 10-5-10-5z" stroke="currentColor" stroke-width="2"
                fill="none" stroke-linejoin="round"/>
            <path d="M2 17l10 5 10-5M2 12l10 5 10-5"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linejoin="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_texture_icon(self): #vers 1
        """Texture file icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <rect x="3" y="3" width="7" height="7" fill="currentColor" opacity="0.3"/>
            <rect x="14" y="3" width="7" height="7" fill="currentColor" opacity="0.6"/>
            <rect x="3" y="14" width="7" height="7" fill="currentColor" opacity="0.6"/>
            <rect x="14" y="14" width="7" height="7" fill="currentColor" opacity="0.3"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_collision_icon(self): #vers 1
        """Collision file icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="9" stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M12 2v20M2 12h20" stroke="currentColor" stroke-width="1" opacity="0.5"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_text_icon(self): #vers 1
        """Text file icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M14 2v6h6M16 13H8M16 17H8M10 9H8"
                stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_back_icon(self): #vers 1
        """Back navigation icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M19 12H5M12 19l-7-7 7-7"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_forward_icon(self): #vers 1
        """Forward navigation icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M5 12h14M12 5l7 7-7 7"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_up_icon(self): #vers 1
        """Up/Parent directory icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M12 19V5M5 12l7-7 7 7"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_refresh_icon(self): #vers 1
        """Refresh icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M21.5 2v6h-6M2.5 22v-6h6M2 11.5a10 10 0 0118.8-4.3M22 12.5a10 10 0 01-18.8 4.2"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_home_icon(self): #vers 1
        """Home icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M3 9l9-7 9 7v11a2 2 0 01-2 2H5a2 2 0 01-2-2V9z"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M9 22V12h6v10" stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_desktop_icon(self): #vers 1
        """Desktop icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <rect x="2" y="3" width="20" height="14" rx="2"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M8 21h8M12 17v4" stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_document_icon(self): #vers 1
        """Document icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M14 2v6h6M16 13H8M16 17H8M10 9H8"
                stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_download_icon(self): #vers 1
        """Download/Downloads folder icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="7 10 12 15 17 10"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="15" x2="12" y2="3"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_drive_icon(self): #vers 1
        """Hard drive icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <rect x="2" y="6" width="20" height="12" rx="2"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M6 12h.01M10 12h.01" stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_open_icon(self): #vers 1
        """Open icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-7l-2-2H5a2 2 0 00-2 2z"
                stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_save_icon(self): #vers 1
        """Save icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M19 21H5a2 2 0 01-2-2V5a2 2 0 012-2h11l5 5v11a2 2 0 01-2 2z"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M17 21v-8H7v8M7 3v5h8"
                stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_import_icon(self): #vers 1
        """Import icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="7 10 12 15 17 10"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="15" x2="12" y2="3"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_export_icon(self): #vers 1
        """Export icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="17 8 12 3 7 8"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="3" x2="12" y2="15"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_cancel_icon(self): #vers 1
        """Cancel/Close icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="10"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <line x1="15" y1="9" x2="9" y2="15"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="9" y1="9" x2="15" y2="15"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_edit_icon(self): #vers 1
        """Edit/Rename icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M11 4H4a2 2 0 00-2 2v14a2 2 0 002 2h14a2 2 0 002-2v-7"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M18.5 2.5a2.121 2.121 0 013 3L12 15l-4 1 1-4 9.5-9.5z"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_delete_icon(self): #vers 1
        """Delete icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <polyline points="3 6 5 6 21 6"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M19 6v14a2 2 0 01-2 2H7a2 2 0 01-2-2V6m3 0V4a2 2 0 012-2h4a2 2 0 012 2v2"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)

    def _create_properties_icon(self): #vers 1
        """Properties/Info icon SVG"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="10"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M12 16v-4M12 8h.01"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._create_svg_icon(svg_data)


@staticmethod
def get_open_filename(parent=None, caption="Open File", directory="", file_filter="All Files (*.*)"): #vers 1
    """Static method to show open file dialog"""
    dialog = DolphinFileDialog(parent, mode='open', multi_select=False, file_filter=file_filter)

    if directory:
        dialog.current_path = directory
        dialog._load_directory(directory)

    if dialog.exec() == QDialog.DialogCode.Accepted:
        return dialog.get_selected_path()
    return None

@staticmethod
def get_open_filenames(parent=None, caption="Open Files", directory="", file_filter="All Files (*.*)"): #vers 1
    """Static method to show open multiple files dialog"""
    dialog = DolphinFileDialog(parent, mode='open', multi_select=True, file_filter=file_filter)

    if directory:
        dialog.current_path = directory
        dialog._load_directory(directory)

    if dialog.exec() == QDialog.DialogCode.Accepted:
        return dialog.get_selected_paths()
    return []

@staticmethod
def get_save_filename(parent=None, caption="Save File", directory="", file_filter="All Files (*.*)"): #vers 1
    """Static method to show save file dialog"""
    dialog = DolphinFileDialog(parent, mode='save', multi_select=False, file_filter=file_filter)

    if directory:
        dialog.current_path = directory
        dialog._load_directory(directory)

    if dialog.exec() == QDialog.DialogCode.Accepted:
        return dialog.get_selected_path()
    return None

@staticmethod
def get_existing_directory(parent=None, caption="Select Directory", directory=""): #vers 1
    """Static method to show directory selection dialog"""
    dialog = DolphinFileDialog(parent, mode='open', multi_select=False, file_filter="")
    dialog.setWindowTitle(caption)

    if directory:
        dialog.current_path = directory
        dialog._load_directory(directory)

    # Modify to only allow folder selection
    dialog.tree.itemDoubleClicked.disconnect()
    dialog.tree.itemDoubleClicked.connect(lambda item, col: dialog._load_directory(item.data(0, Qt.ItemDataRole.UserRole)) if QFileInfo(item.data(0, Qt.ItemDataRole.UserRole)).isDir() else None)

    if dialog.exec() == QDialog.DialogCode.Accepted:
        return dialog.current_path
    return None


# Test code - Remove before production
if __name__ == "__main__":
    from PyQt6.QtWidgets import QApplication
    import sys

    app = QApplication(sys.argv)

    # Test open dialog
    dialog = DolphinFileDialog(None, mode='open', multi_select=False, file_filter="All Files (*.*)")
    dialog._setup_ui()

    if dialog.exec() == QDialog.DialogCode.Accepted:
        print("Selected:", dialog.get_selected_path())
    else:
        print("Cancelled")

    sys.exit()
