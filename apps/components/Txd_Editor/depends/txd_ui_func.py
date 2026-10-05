#this belongs in apps/components/Txd_Editor/depends/txd_ui_func.py - Version: 8
# X-Seti - September30 2026 - IMG Factory 1.6 - TXD Workshop UI

"""
TXD Workshop UI - panels, ribbons, fonts, icons, view modes, hotkeys, status.
"""

##class TXDUIMixin: -
# _add_checkerboard_background
# _apply_button_font
# _apply_button_mode_to_button
# _apply_icon_scale
# _apply_infobar_font
# _apply_panel_font
# _apply_theme
# _apply_title_font
# _build_menus_into_qmenu
# _build_toolbars
# _connect_texture_table_signals
# _create_left_panel
# _create_middle_panel
# _create_right_panel
# _create_toolbar
# _enable_name_edit
# _filter_txd_list
# _focus_search
# _gamepad_saved
# _gamepad_step
# _get_icon_color
# get_menu_title
# _get_ui_color
# _initialize_features
# keyPressEvent
# _pan_preview
# _pick_background_color
# _push_status_to_img_factory
# _refresh_icons
# _restore_toolbar_state
# _save_toolbar_state
# _set_icon_display_mode
# _set_selection_buttons_enabled
# _set_status
# _set_tiled_preview
# _set_transform_buttons_enabled
# _setup_hotkeys
# _setup_status_indicators
# setup_ui
# _show_alpha_view
# _show_normal_view
# _show_overlay_view
# _show_split_view
# _show_txd_search
# switch_texture_view
# _toggle_checkerboard
# _toggle_gamepad
# _toolbar_context_menu
# _update_all_buttons
# _update_status_indicators
# _update_table_display
# _update_texture_info

from PyQt6.QtCore import QSize, Qt, QTimer
from PyQt6.QtGui import QColor, QFont, QIcon, QImage, QPainter, QPixmap
from PyQt6.QtWidgets import QAbstractItemView, QColorDialog, QComboBox, QFrame, QHBoxLayout, QLabel, QLineEdit, QListWidget, QMenu, QMessageBox, QPushButton, QTabWidget, QTableWidget, QVBoxLayout, QWidget
from apps.methods.img_factory_settings import get_user_config_dir
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.txd_dialogs import ZoomablePreview
from apps.methods.grip_splitter import GripSplitter

App_name = "Txd Workshop"
App_build = "39"
DEBUG_STANDALONE = False


class TXDUIMixin: #vers 1
    """UI methods for TXDWorkshop."""

    def _get_ui_color(self, key): #vers 2
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)

    def get_menu_title(self) -> str: #vers 1
        """Return menu label for imgfactory menu bar."""
        return "TXD"

    def _build_menus_into_qmenu(self, parent_menu): #vers 1
        """Populate parent_menu with TXD Workshop actions for imgfactory injection."""
        from PyQt6.QtGui import QAction

        # File
        fm = parent_menu.addMenu("File")
        fm.addAction("Open TXD…",           self._open_txd_file if hasattr(self, '_open_txd_file') else lambda: None)
        fm.addAction("Save TXD",             self._save_txd_file)
        fm.addAction("Save TXD As…",         self._save_as_txd_file)
        fm.addSeparator()
        fm.addAction("New TXD",              self._create_new_txd)
        fm.addSeparator()
        fm.addAction("Close TXD",            lambda: None)

        # Texture
        tm = parent_menu.addMenu("Texture")
        tm.addAction("Import Texture…",      self._import_textures)
        tm.addAction("Export Selected…",     self.export_selected_texture)
        tm.addAction("Export All…",          self.export_all_textures)
        tm.addSeparator()
        tm.addAction("Convert Format…",      self._show_convert_dialog if hasattr(self, '_show_convert_dialog') else lambda: None)

        # Tools
        tools = parent_menu.addMenu("Tools")
        tools.addAction("Colour Adjustments…", self._open_colour_adjust)
        tools.addAction("Seamless Tool…",       self._open_seamless_tool)
        tools.addAction("Snow Effect…",         self._open_snow_tool)
        tools.addSeparator()
        tools.addAction("Tiled Preview 1x1",    lambda: self._set_tiled_preview(1))
        tools.addAction("Tiled Preview 2x2",    lambda: self._set_tiled_preview(2))
        tools.addAction("Tiled Preview 3x3",    lambda: self._set_tiled_preview(3))
        tools.addSeparator()
        tools.addAction("Alpha Coverage…",      self._open_alpha_coverage)

        # View
        vm = parent_menu.addMenu("View")
        vm.addAction("TXD Info",             self._show_txd_info)

    def setup_ui(self): #vers 10
        """Setup the main UI layout"""
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(5, 5, 5, 5)
        main_layout.setSpacing(5)

        # Toolbar - hidden when embedded in main window tab
        toolbar = self._create_toolbar()
        self._workshop_toolbar = toolbar
        if not self.standalone_mode:
            toolbar.setVisible(False)
        main_layout.addWidget(toolbar)

        # Tab bar for multiple TXD files
        self.txd_tabs = QTabWidget()
        self.txd_tabs.setTabsClosable(True)
        self.txd_tabs.tabCloseRequested.connect(self._close_txd_tab)
        self.txd_tabs.currentChanged.connect(self._switch_txd_tab)

        # Create initial tab with main content
        initial_tab = QWidget()
        tab_layout = QVBoxLayout(initial_tab)
        tab_layout.setContentsMargins(0, 0, 0, 0)


        # Main splitter
        main_splitter = GripSplitter(Qt.Orientation.Horizontal)

        # Create all panels first
        left_panel = self._create_left_panel()
        middle_panel = self._create_middle_panel()
        right_panel = self._create_right_panel()

        # Add panels to splitter based on mode
        if left_panel is not None:  # IMG Factory mode
            main_splitter.addWidget(left_panel)
            main_splitter.addWidget(middle_panel)
            main_splitter.addWidget(right_panel)
            # Set proportions (2:3:5)
            main_splitter.setStretchFactor(0, 2)
            main_splitter.setStretchFactor(1, 3)
            main_splitter.setStretchFactor(2, 5)
        else:  # Standalone mode
            main_splitter.addWidget(middle_panel)
            main_splitter.addWidget(right_panel)
            # Set proportions (1:1)
            main_splitter.setStretchFactor(0, 1)
            main_splitter.setStretchFactor(1, 1)

        self._main_splitter = main_splitter
        self._main_splitter.splitterMoved.connect(self._on_splitter_moved)
        main_layout.addWidget(main_splitter)
        QTimer.singleShot(0, self._restore_splitter_sizes)

        # Apply themed icons now UI is fully built
        self._refresh_icons()

        # Connect signals AFTER texture_table is created
        self._connect_texture_table_signals()

        # NEW: Status bar at bottom with texture info
        #self.status_bar = self._create_status_bar()
        #main_layout.addWidget(self.status_bar)

        # Status indicators - hidden when embedded in main window tab, or
        # if the user has turned off "Show TXD Workshop status bar"
        if hasattr(self, '_setup_status_indicators'):
            status_frame = self._setup_status_indicators()
            if not self.standalone_mode or not getattr(self, 'show_status_bar', True):
                status_frame.setVisible(False)
            main_layout.addWidget(status_frame)

    def _initialize_features(self): #vers 3
        """Initialize all features after UI setup"""
        try:
            self._apply_theme()
            self._update_status_indicators()

            if hasattr(self, 'format_filter'):
                self.format_filter.setCurrentIndex(0)
            if hasattr(self, 'size_filter'):
                self.size_filter.setCurrentIndex(0)
            if hasattr(self, 'alpha_filter'):
                self.alpha_filter.setCurrentIndex(0)

            self._clear_texture_search()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("TXD Workshop features initialized")

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Feature init error: {str(e)}")

    def _update_all_buttons(self): #vers 4
        """Update all buttons to match display mode"""
        buttons_to_update = [
            # Toolbar buttons
            ('open_img_btn', 'Open IMG'),
            ('open_txd_btn', 'Open TXD'),
            ('save_txd_btn', 'Save TXD'),
            ('import_btn', 'Import'),
            ('export_btn', 'Export'),
            ('export_all_btn', 'Export All'),
            ('switch_btn', 'Switch'),
            ('props_btn', 'Prop'),
            ('info_btn', 'I'),
            ('undo_btn', 'Undo'),
            ('paint_btn', 'Paint'),
            ('build_from_dff_btn', 'Build from DFF'),
            # Transform buttons
            ('flip_vert_btn', 'Flip Vertical'),
            ('flip_horz_btn', 'Flip Horizontal'),
            ('rotate_cw_btn', 'Rotate 90° CW"'),
            ('rotate_ccw_btn', 'Rotate 90° CCW"'),
            ('copy_btn', 'Copy'),
            ('paste_btn', 'Paste'),
            ('convert_btn', 'Convert'),
            # Manage buttons
            ('create_texture_btn', 'Create'),
            ('delete_texture_btn', 'Delete'),
            ('duplicate_texture_btn', 'Duplicate'),
            # Effects buttons
            ('filters_btn', 'Filters'),
            ('paint_btn', 'Paint'),
            ('check_dff_btn', 'Check Dff'),
            # Format/Size buttons
            ('bitdepth_btn', 'Bit Depth'),
            ('resize_btn', 'Resize'),
            ('upscale_btn', 'Upscale'),
            ('compress_btn', 'Compress'),
            ('uncompress_btn', 'Uncompress'),
            # Mipmap buttons
            ('show_mipmaps_btn', 'View'),
            ('create_mipmaps_btn', 'Create'),
            ('remove_mipmaps_btn', 'Remove'),
            # Bumpmap buttons
            ('view_bumpmap_btn', 'View'),
            ('export_bumpmap_btn', 'Export'),
            ('import_bumpmap_btn', 'Import'),
        ]

        # Toggle panel visibility based on mode
        self._update_transform_text_panel_visibility()

        for btn_name, btn_text in buttons_to_update:
            if hasattr(self, btn_name):
                button = getattr(self, btn_name)
                self._apply_button_mode_to_button(button, btn_text)
        self._update_dock_button_visibility()

    def _apply_button_mode_to_button(self, button, text): #vers 8
        """Apply display mode via shared helper."""
        from apps.methods.button_mode import apply_button_mode_to_button
        apply_button_mode_to_button(button, text, self.button_display_mode)

    def _create_toolbar(self): #vers 13
        """Create toolbar - FIXED: Hide drag button when docked, ensure buttons visible"""
        self.titlebar = QFrame()
        self.titlebar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.titlebar.setFixedHeight(45)
        self.titlebar.setObjectName("titlebar")

        # Install event filter for drag detection
        self.titlebar.installEventFilter(self)
        self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.titlebar.setMouseTracking(True)

        self.layout = QHBoxLayout(self.titlebar)
        self.layout.setContentsMargins(5, 5, 5, 5)
        self.layout.setSpacing(5)

        # Get icon color from theme
        icon_color = self._get_icon_color()

        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setMaximumHeight(50)

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(5)

        # Settings button
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(self.icon_factory.settings_icon(color=self._get_icon_color()))
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip("Workshop Settings")
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        # App title in center
        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(self.title_label)

        layout.addStretch()
        #layout.addStretch()

        # Only show "Open IMG" button if NOT standalone
        if not self.standalone_mode:
            self.open_img_btn = QPushButton("OpenIMG")
            self.open_img_btn.setFont(self.button_font)
            self.open_img_btn.setIcon(self.icon_factory.folder_icon(color=self._get_icon_color()))
            self.open_img_btn.setIconSize(QSize(20, 20))
            self.open_img_btn.clicked.connect(self.open_img_archive)
            layout.addWidget(self.open_img_btn)

        self.open_txd_btn = QPushButton("Open")
        self.open_txd_btn.setFont(self.button_font)
        self.open_txd_btn.setIcon(self.icon_factory.file_icon(color=self._get_icon_color()))
        self.open_txd_btn.setIconSize(QSize(20, 20))
        self.open_txd_btn.clicked.connect(self.open_txd_file)
        layout.addWidget(self.open_txd_btn)

        self.save_txd_btn = QPushButton("Save")
        self.save_txd_btn.setFont(self.button_font)
        self.save_txd_btn.setIcon(self.icon_factory.save_icon(color=self._get_icon_color()))
        self.save_txd_btn.setIconSize(QSize(20, 20))
        self.save_txd_btn.clicked.connect(self.save_txd_file)
        self.save_txd_btn.setEnabled(False)
        layout.addWidget(self.save_txd_btn)

        self.export_all_btn = QPushButton("Extract")
        self.export_all_btn.setFont(self.button_font)
        self.export_all_btn.setIcon(self.icon_factory.package_icon(color=self._get_icon_color()))
        self.export_all_btn.setIconSize(QSize(20, 20))
        self.export_all_btn.clicked.connect(self.export_all_textures)
        self.export_all_btn.setEnabled(False)
        layout.addWidget(self.export_all_btn)

        self.undo_btn = QPushButton()
        self.undo_btn.setFont(self.button_font)
        self.undo_btn.setIcon(self.icon_factory.undo_icon(color=self._get_icon_color()))
        self.undo_btn.setText("Undo")
        self.undo_btn.setIconSize(QSize(20, 20))
        self.undo_btn.clicked.connect(self._undo_last_action)
        self.undo_btn.setEnabled(False)
        self.undo_btn.setToolTip("Undo last change")
        layout.addWidget(self.undo_btn)
        self._undo_buttons = [self.undo_btn]

        layout.addSpacing(10)

        # Info button
        self.info_btn = QPushButton("")
        self.info_btn.setText("")  # CHANGED from "Info"
        self.info_btn.setIcon(self.icon_factory.info_icon(color=self._get_icon_color()))
        self.info_btn.setMinimumWidth(40)
        self.info_btn.setMaximumWidth(40)
        self.info_btn.setMinimumHeight(30)
        self.info_btn.setToolTip("Information")
        self.info_btn.setIconSize(QSize(20, 20))
        self.info_btn.setFixedWidth(35)
        self.info_btn.clicked.connect(self._show_txd_info)
        layout.addWidget(self.info_btn)

        # Properties/Theme button
        self.properties_btn = QPushButton()
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(24, icon_color))
        self.properties_btn.setToolTip("Theme")
        self.properties_btn.setFixedSize(35, 35)
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        self.properties_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.properties_btn.customContextMenuRequested.connect(self._show_settings_context_menu)
        layout.addWidget(self.properties_btn)

        # Dock button [D]
        self.dock_btn = QPushButton("D")
        #self.dock_btn.setFont(self.button_font)
        self.dock_btn.setMinimumWidth(40)
        self.dock_btn.setMaximumWidth(40)
        self.dock_btn.setMinimumHeight(30)
        self.dock_btn.setToolTip("Dock")
        self.dock_btn.clicked.connect(self.toggle_dock_mode)
        layout.addWidget(self.dock_btn)

        # Tear-off button [T] - only in IMG Factory mode
        if not self.standalone_mode:
            self.tearoff_btn = QPushButton("T")
            #self.tearoff_btn.setFont(self.button_font)
            self.tearoff_btn.setMinimumWidth(40)
            self.tearoff_btn.setMaximumWidth(40)
            self.tearoff_btn.setMinimumHeight(30)
            self.tearoff_btn.clicked.connect(self._toggle_tearoff)
            self.tearoff_btn.setToolTip("TXD Workshop - Tearoff window")
            layout.addWidget(self.tearoff_btn)

        # Window controls
        self.minimize_btn = QPushButton()
        self.minimize_btn.setIcon(self.icon_factory.minimize_icon(color=self._get_icon_color()))
        self.minimize_btn.setIconSize(QSize(20, 20))
        self.minimize_btn.setMinimumWidth(40)
        self.minimize_btn.setMaximumWidth(40)
        self.minimize_btn.setMinimumHeight(30)
        self.minimize_btn.clicked.connect(self.showMinimized)
        self.minimize_btn.setToolTip("Minimize Window") # click tab to restore
        layout.addWidget(self.minimize_btn)

        self.maximize_btn = QPushButton()
        self.maximize_btn.setIcon(self.icon_factory.maximize_icon(color=self._get_icon_color()))
        self.maximize_btn.setIconSize(QSize(20, 20))
        self.maximize_btn.setMinimumWidth(40)
        self.maximize_btn.setMaximumWidth(40)
        self.maximize_btn.setMinimumHeight(30)
        self.maximize_btn.clicked.connect(self._toggle_maximize)
        self.maximize_btn.setToolTip("Maximize/Restore Window")
        layout.addWidget(self.maximize_btn)

        self.close_btn = QPushButton()
        self.close_btn.setIcon(self.icon_factory.close_icon(color=self._get_icon_color()))
        self.close_btn.setIconSize(QSize(20, 20))
        self.close_btn.setMinimumWidth(40)
        self.close_btn.setMaximumWidth(40)
        self.close_btn.setMinimumHeight(30)
        self.close_btn.clicked.connect(self.close)
        self.close_btn.setToolTip("Close Window") # closes tab
        layout.addWidget(self.close_btn)

        return self.toolbar

    def _create_left_panel(self): #vers 5
        """Create left panel - TXD file list (only in IMG Factory mode)"""
        # In standalone mode, don't create this panel
        if self.standalone_mode:
            self.txd_list_widget = None  # Explicitly set to None
            return None

        # Only create panel in IMG Factory mode
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(200)
        panel.setMaximumWidth(300)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)

        # Header row with search button
        hdr_row = QHBoxLayout()
        self._txd_list_header = QLabel("TXD Files")
        self._txd_list_header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        hdr_row.addWidget(self._txd_list_header)
        hdr_row.addStretch()
        self.txd_search_btn = QPushButton()
        self.txd_search_btn.setFixedSize(24, 24)
        self.txd_search_btn.setIcon(SVGIconFactory.search_icon(16, self._get_icon_color()))
        self.txd_search_btn.setIconSize(QSize(16, 16))
        self.txd_search_btn.setToolTip("Search TXD files")
        self.txd_search_btn.clicked.connect(self._show_txd_search)
        hdr_row.addWidget(self.txd_search_btn)
        layout.addLayout(hdr_row)

        # Search box (hidden by default)
        self.txd_search_box = QLineEdit()
        self.txd_search_box.setPlaceholderText("Search TXD files...")
        self.txd_search_box.setVisible(False)
        self.txd_search_box.textChanged.connect(self._filter_txd_list)
        layout.addWidget(self.txd_search_box)

        self.txd_list_widget = QListWidget()
        self.txd_list_widget.setAlternatingRowColors(True)
        self.txd_list_widget.itemClicked.connect(self._on_txd_selected)
        layout.addWidget(self.txd_list_widget)

        return panel

    def _create_middle_panel(self): #vers 9
        """Create middle panel - Texture list with mini toolbar shown in docked mode."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(250)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(4)

        # Header label
        self._textures_header = QLabel("Textures")
        self._textures_header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        layout.addWidget(self._textures_header)

        #    Mini toolbar: 4 icon buttons — only shown when docked          
        # (toolbar has these too; in docked mode the toolbar is hidden)
        icon_color = self._get_icon_color()
        self._middle_btn_row = QFrame()
        btn_layout = QHBoxLayout(self._middle_btn_row)
        btn_layout.setContentsMargins(0, 0, 0, 0)
        btn_layout.setSpacing(3)

        self.open_txd_btn = QPushButton("Open")
        self.open_txd_btn.setFont(self.button_font)
        self.open_txd_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.open_txd_btn.setIconSize(QSize(20, 20))
        self.open_txd_btn.setToolTip("Open TXD file (Ctrl+O)")
        self.open_txd_btn.clicked.connect(self.open_txd_file)
        btn_layout.addWidget(self.open_txd_btn)

        self.save_txd_btn = QPushButton("Save")
        self.save_txd_btn.setFont(self.button_font)
        self.save_txd_btn.setIcon(self.icon_factory.save_icon(color=icon_color))
        self.save_txd_btn.setIconSize(QSize(20, 20))
        self.save_txd_btn.setToolTip("Save TXD file (Ctrl+S)")
        self.save_txd_btn.clicked.connect(self.save_txd_file)
        self.save_txd_btn.setEnabled(False)
        btn_layout.addWidget(self.save_txd_btn)

        self.export_all_btn = QPushButton("Extract")
        self.export_all_btn.setFont(self.button_font)
        self.export_all_btn.setIcon(self.icon_factory.package_icon(color=icon_color))
        self.export_all_btn.setIconSize(QSize(20, 20))
        self.export_all_btn.setToolTip("Export all textures")
        self.export_all_btn.clicked.connect(self.export_all_textures)
        self.export_all_btn.setEnabled(False)
        btn_layout.addWidget(self.export_all_btn)

        self.undo_btn = QPushButton()
        self.undo_btn.setFont(self.button_font)
        self.undo_btn.setIcon(self.icon_factory.undo_icon(color=icon_color))
        self.undo_btn.setIconSize(QSize(20, 20))
        self.undo_btn.setToolTip("Undo last change")
        self.undo_btn.clicked.connect(self._undo_last_action)
        self.undo_btn.setEnabled(False)
        btn_layout.addWidget(self.undo_btn)
        self._undo_buttons.append(self.undo_btn)

        btn_layout.addStretch()
        layout.addWidget(self._middle_btn_row)
        self._middle_compact_btns = [(self.open_txd_btn, "Open"), (self.save_txd_btn, "Save"),
                                     (self.export_all_btn, "Extract")]

        # Only show mini toolbar when docked (standalone toolbar already has these)
        self._middle_btn_row.setVisible(self.is_docked and not self.standalone_mode)

        #    Texture table                                                  
        self.texture_table = QTableWidget()
        self.texture_table.setColumnCount(2)
        self.texture_table.setHorizontalHeaderLabels(["Preview", "Details"])
        self.texture_table.horizontalHeader().setStretchLastSection(True)
        self.texture_table.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.texture_table.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)   # Shift/Ctrl multi-select
        self.texture_table.setAlternatingRowColors(True)
        self.texture_table.itemSelectionChanged.connect(self._on_texture_selected)
        self.texture_table.setIconSize(QSize(64, 64))
        layout.addWidget(self.texture_table)

        return panel

    def _create_right_panel(self): #vers 13
        """Right panel using QMainWindow + QToolBar for native docking.
        Same system as Model/COL Workshop - QMainWindow handles toolbar
        placement, row stacking, floating, and save/restore natively,
        replacing the old DockableToolbar panels."""
        icon_color = self._get_icon_color()

        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(250)
        self._right_panel_ref = panel   # used by visibility method
        has_bumpmap = False
        outer_layout = QVBoxLayout(panel)
        outer_layout.setContentsMargins(4, 4, 4, 4)
        outer_layout.setSpacing(3)

        from PyQt6.QtWidgets import QMainWindow
        inner_mw = QMainWindow()
        inner_mw.setWindowFlags(Qt.WindowType.Widget)
        inner_mw.setDockOptions(
            QMainWindow.DockOption.AllowNestedDocks |
            QMainWindow.DockOption.AllowTabbedDocks)
        self._inner_mw = inner_mw

        self.preview_widget = ZoomablePreview(self)
        inner_mw.setCentralWidget(self.preview_widget)

        self._build_toolbars(inner_mw, icon_color)
        self._apply_custom_icons()

        outer_layout.addWidget(inner_mw, stretch=1)

        from PyQt6.QtCore import QTimer as _QTimer
        _QTimer.singleShot(400, self._restore_toolbar_state)

        self.window_closed.connect(self._save_toolbar_state)

        return panel

    def _set_status(self, msg: str): #vers 1
        """Write msg to the status label (whichever one exists). Was called
        in a few places already but never defined - added to match every
        other workshop's pattern."""
        if hasattr(self, 'status_label'):
            self.status_label.setText(msg)
        elif hasattr(self, 'status_bar') and hasattr(self.status_bar, 'showMessage'):
            self.status_bar.showMessage(msg, 3000)
        else:
            print(f"[TXD] {msg}")

    def _build_toolbars(self, mw: 'QMainWindow', icon_color: str): #vers 13
        """Build all QToolBar instances using QAction (Model/COL Workshop
        pattern). Replaces the old DockableToolbar-based
        _create_transform_icon_panel/_create_transform_text_panel/
        _create_preview_controls panels. attr= names match the original
        QPushButton names exactly so _set_transform_buttons_enabled(),
        _set_selection_buttons_enabled(), _on_texture_selected() and
        _refresh_icons() elsewhere in this file keep working unchanged -
        QAction supports setEnabled()/setIcon()/setText() same as
        QPushButton."""
        from PyQt6.QtWidgets import QToolBar
        from PyQt6.QtGui import QAction
        _saved_px = 20
        try:
            import json
            from pathlib import Path
            _saved_px = json.loads(
                (get_user_config_dir()/'txd_workshop.json').read_text()
            ).get('icon_scale', 20)
        except Exception:
            pass
        icon_size = QSize(_saved_px, _saved_px)
        pw = self.preview_widget
        self._ribbon_actions = []

        def _tb(name, area=Qt.ToolBarArea.TopToolBarArea): #vers 1
            tb = QToolBar(name, mw)
            tb.setObjectName(name)
            tb.setIconSize(icon_size)
            tb.setMovable(True)
            tb.setFloatable(True)
            tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            tb.customContextMenuRequested.connect(
                lambda pos, t=tb: self._toolbar_context_menu(t, pos))
            mw.addToolBar(area, tb)
            return tb

        def _act(tb, name, icon_fn, callback=None, checkable=False,
                 checked=False, attr=None, enabled=True): #vers 1
            try:
                icon = icon_fn(color=icon_color)
            except Exception:
                icon = self.icon_factory.settings_icon(color=icon_color)
            act = QAction(icon, name, mw)
            act.setToolTip(name)
            act.setCheckable(checkable)
            act.setEnabled(enabled)
            if checkable:
                act.setChecked(checked)
            if callback:
                if checkable:
                    act.toggled.connect(callback)
                else:
                    act.triggered.connect(callback)
            tb.addAction(act)
            self._ribbon_actions.append({
                'action': act, 'toolbar': tb, 'name': name,
                'icon_fn': icon_fn, 'checkable': checkable,
            })
            if attr:
                setattr(self, attr, act)
            return act

        #    Ribbon 1: Transform                                            
        tb_xform = _tb("Transform")
        _act(tb_xform, "Flip Vertical",   self.icon_factory.flip_vert_icon,
             self._flip_vertical,   enabled=False, attr='flip_vert_btn')
        _act(tb_xform, "Flip Horizontal", self.icon_factory.flip_horz_icon,
             self._flip_horizontal, enabled=False, attr='flip_horz_btn')
        _act(tb_xform, "Rotate CW",       self.icon_factory.rotate_cw_icon,
             self._rotate_clockwise,        enabled=False, attr='rotate_cw_btn')
        _act(tb_xform, "Rotate CCW",      self.icon_factory.rotate_ccw_icon,
             self._rotate_counterclockwise, enabled=False, attr='rotate_ccw_btn')
        tb_xform.addSeparator()
        _act(tb_xform, "Copy",  self.icon_factory.copy_icon,
             self._copy_texture,  enabled=False, attr='copy_btn')
        _act(tb_xform, "Paste", self.icon_factory.paste_icon,
             self._paste_texture, enabled=False, attr='paste_btn')
        tb_xform.addSeparator()
        _act(tb_xform, "Create",    self.icon_factory.create_texture_icon,
             self._create_new_texture_entry, attr='create_texture_btn')
        _act(tb_xform, "Delete",    self.icon_factory.delete_texture_icon,
             self._delete_texture,     enabled=False, attr='delete_texture_btn')
        _act(tb_xform, "Duplicate", self.icon_factory.duplicate_icon,
             self._duplicate_texture,  enabled=False, attr='duplicate_texture_btn')
        tb_xform.addSeparator()
        _act(tb_xform, "Paint", self.icon_factory.paint_texture_icon,
             self._open_paint_editor, enabled=False, attr='paint_btn')
        _act(tb_xform, "Check DFF",      self.icon_factory.analyze_icon,
             self._check_txd_vs_dff,    attr='check_dff_btn')
        _act(tb_xform, "Build from DFF", self.icon_factory.build_from_dff_icon,
             self._build_txd_from_dff,  attr='build_from_dff_btn')
        _act(tb_xform, "Filters", self.icon_factory.filter_icon,
             self._open_filters_dialog, enabled=False, attr='filters_btn')
        tb_xform.addSeparator()
        _act(tb_xform, "Switch",        self.icon_factory.switch_view_icon,
             self.switch_texture_view,      enabled=False, attr='switch_btn')
        _act(tb_xform, "Invert Alpha",  self.icon_factory.invert_alpha_icon,
             self._toggle_alpha_invert,     enabled=False, attr='invert_btn')
        _act(tb_xform, "Generate Alpha",self.icon_factory.paint_icon,
             self._generate_alpha_mask,     enabled=False, attr='gen_alpha_btn')
        _act(tb_xform, "Properties",    self.icon_factory.properties_icon,
             self.show_properties,          enabled=False, attr='props_btn')

        #    Ribbon 2: Navigation                                           
        tb_nav = _tb("Navigation", Qt.ToolBarArea.RightToolBarArea)
        _act(tb_nav, "Zoom In",       self.icon_factory.zoom_in_icon,  pw.zoom_in)
        _act(tb_nav, "Zoom Out",      self.icon_factory.zoom_out_icon, pw.zoom_out)
        _act(tb_nav, "Reset View",    self.icon_factory.view_reset_icon,    pw.reset_view)
        _act(tb_nav, "Fit to Window", self.icon_factory.fit_icon,      pw.fit_to_window)
        tb_nav.addSeparator()
        _act(tb_nav, "Pan Up",    self.icon_factory.arrow_up_icon,    lambda: self._pan_preview(0, -20))
        _act(tb_nav, "Pan Down",  self.icon_factory.arrow_down_icon,  lambda: self._pan_preview(0, 20))
        _act(tb_nav, "Pan Left",  self.icon_factory.arrow_left_icon,  lambda: self._pan_preview(-20, 0))
        _act(tb_nav, "Pan Right", self.icon_factory.arrow_right_icon, lambda: self._pan_preview(20, 0))
        tb_nav.addSeparator()
        _act(tb_nav, "Pick Background", self.icon_factory.color_picker_icon,
             self._pick_background_color)
        _act(tb_nav, "Resize Texture",  self.icon_factory._resize_icon,
             self._resize_texture, attr='resize_texture_btn')
        tb_nav.addSeparator()
        _act(tb_nav, "Game Controller (PS5)", self.icon_factory.controller_icon,
             self._toggle_gamepad, checkable=True, attr='gamepad_btn')
        self.gamepad_btn.setChecked(self._gamepad_saved())

        #    Ribbon 3: Effects                                              
        tb_fx = _tb("Effects", Qt.ToolBarArea.RightToolBarArea)
        _act(tb_fx, "Colour Adjustments…", self.icon_factory.knob_icon,
             self._open_colour_adjust)
        _act(tb_fx, "Seamless Tool…",      self.icon_factory.seamless_icon,
             self._open_seamless_tool)
        _act(tb_fx, "Snow Effect…",        self.icon_factory.snow_icon,
             self._open_snow_tool)
        _act(tb_fx, "Alpha Coverage…",     self.icon_factory.alpha_coverage_icon,
             self._open_alpha_coverage)
        tb_fx.addSeparator()
        _act(tb_fx, "Checkerboard", self.icon_factory.checkerboard_icon,
             lambda: pw.set_checkerboard_background())
        _act(tb_fx, "Black Background", self.icon_factory.stop_icon,
             lambda: pw.set_background_color(QColor(0, 0, 0)))
        _act(tb_fx, "White Background", self.icon_factory.stop_icon,
             lambda: pw.set_background_color(QColor(255, 255, 255)))

        #    Ribbon 4: Name                                                 
        # Replaces the old info_group QGroupBox (name/alpha fields + format/
        # bitdepth/resize/compress buttons) which duplicated itself between
        # icons-mode and text-mode with several latent bugs (undefined
        # 'texture' var, import_btn/export_btn never created in icons mode).
        # One set of widgets now, QToolBar handles icons/text/both natively.
        tb_name = _tb("Name", Qt.ToolBarArea.RightToolBarArea)

        self.info_name = QLineEdit()
        self.info_name.setPlaceholderText("Click to edit...")
        self.info_name.setReadOnly(True)
        self.info_name.setMinimumWidth(130)
        self.info_name.setStyleSheet("padding: 2px; border: 1px solid palette(mid);")
        self.info_name.returnPressed.connect(self._save_texture_name)
        self.info_name.editingFinished.connect(self._save_texture_name)
        self.info_name.mousePressEvent = lambda e: self._enable_name_edit(e, False)
        tb_name.addWidget(self.info_name)

        self.alpha_label = QLabel("Alpha:")
        self.alpha_label.setStyleSheet("color: red;")
        self.alpha_label.setVisible(False)
        self._alpha_label_action = tb_name.addWidget(self.alpha_label)

        self.info_alpha_name = QLineEdit()
        self.info_alpha_name.setPlaceholderText("Click to edit...")
        self.info_alpha_name.setReadOnly(True)
        self.info_alpha_name.setMinimumWidth(100)
        self.info_alpha_name.setStyleSheet(
            "color: palette(windowText); padding: 2px; border: 1px solid palette(mid);")
        self.info_alpha_name.returnPressed.connect(self._save_alpha_name)
        self.info_alpha_name.editingFinished.connect(self._save_alpha_name)
        self.info_alpha_name.mousePressEvent = lambda e: self._enable_name_edit(e, True)
        self.info_alpha_name.setVisible(False)
        self._info_alpha_name_action = tb_name.addWidget(self.info_alpha_name)

        #    Ribbon 5: Format                                               
        tb_format = _tb("Format", Qt.ToolBarArea.RightToolBarArea)

        self.format_combo = QComboBox()
        self.format_combo.addItems(["DXT1", "DXT3", "DXT5", "ARGB8888",
                                     "ARGB1555", "ARGB4444", "RGB888", "RGB565"])
        self.format_combo.currentTextChanged.connect(self._change_format)
        self.format_combo.setEnabled(False)
        self.format_combo.setMaximumWidth(100)
        tb_format.addWidget(self.format_combo)

        self.info_bitdepth = QLabel("[32bit]")
        self.info_bitdepth.setMinimumWidth(50)
        tb_format.addWidget(self.info_bitdepth)
        tb_format.addSeparator()

        _act(tb_format, "Change Bit Depth", self.icon_factory._bitdepth_icon,
             self._change_bit_depth,  enabled=False, attr='bitdepth_btn')
        _act(tb_format, "Resize Texture",   self.icon_factory._resize_icon,
             self._resize_texture,    enabled=False, attr='resize_btn')
        _act(tb_format, "AI Upscale",       self.icon_factory._upscale_icon,
             self._upscale_texture,   enabled=False, attr='upscale_btn')
        _act(tb_format, "Compress",         self.icon_factory.compress_icon,
             self._compress_texture,  enabled=False, attr='compress_btn')
        _act(tb_format, "Uncompress",       self.icon_factory.uncompress_icon,
             self._uncompress_texture,enabled=False, attr='uncompress_btn')
        _act(tb_format, "Convert Format",   self.icon_factory.format_convert_icon,
             self._convert_texture,   enabled=False, attr='convert_btn')
        tb_format.addSeparator()
        _act(tb_format, "Import",
             self.icon_factory.import_icon, self._import_textures,
             enabled=True, attr='import_btn')
        _act(tb_format, "Export",
             self.icon_factory.export_icon, self.export_selected_texture,
             enabled=False, attr='export_btn')

        #    Ribbon 6: Mipmaps                                              
        tb_mips = _tb("Mipmaps", Qt.ToolBarArea.RightToolBarArea)

        self.info_format = QLabel("Mipmaps:")
        self.info_format.setMinimumWidth(60)
        tb_mips.addWidget(self.info_format)

        _act(tb_mips, "View Mipmaps",   self.icon_factory.view_icon,
             self._open_mipmap_manager,  enabled=False, attr='show_mipmaps_btn')
        _act(tb_mips, "Generate Mipmaps", self.icon_factory.add_icon,
             self._create_mipmaps_dialog, enabled=False, attr='create_mipmaps_btn')
        _act(tb_mips, "Remove Mipmaps", self.icon_factory.delete_icon,
             self._remove_mipmaps,       enabled=False, attr='remove_mipmaps_btn')
        tb_mips.addSeparator()

        self.info_format_b = QLabel("Bumpmaps:")
        self.info_format_b.setMinimumWidth(70)
        tb_mips.addWidget(self.info_format_b)

        _act(tb_mips, "Manage Bumpmaps", self.icon_factory.manage_icon,
             self._view_bumpmap,   enabled=False, attr='view_bumpmap_btn')
        _act(tb_mips, "Export Bumpmap",  self.icon_factory.export_icon,
             self._export_bumpmap, enabled=False, attr='export_bumpmap_btn')
        _act(tb_mips, "Import Bumpmap",  self.icon_factory.import_icon,
             self._import_bumpmap, enabled=False, attr='import_bumpmap_btn')

        # Store toolbar refs
        self._tb_transform = tb_xform
        self._tb_nav        = tb_nav
        self._tb_effects     = tb_fx
        self._tb_name        = tb_name
        self._tb_format      = tb_format
        self._tb_mipmaps     = tb_mips

        # Compat lists for _refresh_icons's tip_to_icon walk
        self._preview_ctrl_view_btns = [e['action'] for e in self._ribbon_actions
                                         if e['toolbar'] in (tb_nav, tb_fx)]
        self._preview_ctrl_tool_btns = []
        self._preview_ctrl_sep       = None

        # Apply current icons-vs-text display mode
        self._update_transform_text_panel_visibility()

    def _toolbar_context_menu(self, toolbar, pos): #vers 2
        """Right-click context menu on any toolbar."""
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)

        size_menu = menu.addMenu("Icon Size")
        from PyQt6.QtWidgets import QSlider, QWidgetAction
        slider = QSlider(Qt.Orientation.Horizontal)
        slider.setRange(14, 40)
        slider.setSingleStep(2)
        try:
            import json
            from pathlib import Path
            data = json.loads((get_user_config_dir()/'txd_workshop.json').read_text())
            slider.setValue(data.get('icon_scale', 20))
        except Exception:
            slider.setValue(20)
        slider.valueChanged.connect(self._apply_icon_scale)
        wa = QWidgetAction(menu)
        wa.setDefaultWidget(slider)
        size_menu.addAction(wa)

        menu.addSeparator()
        menu.addAction("Ribbon Manager...", self.open_ribbon_manager)
        menu.addSeparator()
        from PyQt6.QtWidgets import QToolBar as _QTB
        menu.addAction("Lock All Toolbars",
            lambda: [tb.setMovable(False)
                     for tb in self._inner_mw.findChildren(_QTB)])
        menu.addAction("Unlock All Toolbars",
            lambda: [tb.setMovable(True)
                     for tb in self._inner_mw.findChildren(_QTB)])
        menu.exec(toolbar.mapToGlobal(pos))

    def _apply_icon_scale(self, px: int): #vers 2
        """Apply icon size to all toolbars live and persist it."""
        mw = getattr(self, '_inner_mw', None)
        if mw:
            from PyQt6.QtWidgets import QToolBar
            from PyQt6.QtCore import QSize as _QS
            for tb in mw.findChildren(QToolBar):
                tb.setIconSize(_QS(px, px))
        try:
            import json
            from pathlib import Path
            path = get_user_config_dir() / 'txd_workshop.json'
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['icon_scale'] = px
            path.write_text(json.dumps(data, indent=2))
        except Exception:
            pass

    def _save_toolbar_state(self): #vers 3
        """Save QMainWindow toolbar state to txd_workshop.json."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        try:
            import json
            from pathlib import Path
            path = get_user_config_dir() / 'txd_workshop.json'
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['toolbar_state'] = mw.saveState(self._RIBBON_LAYOUT_VERSION).toHex().data().decode()
            data['toolbar_state_version'] = self._RIBBON_LAYOUT_VERSION
            path.write_text(json.dumps(data, indent=2))
            self._set_status("Ribbon config saved")
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("TXD Workshop: Ribbon config saved")
        except Exception as _e:
            print(f"[TXDWorkshop] _save_toolbar_state error: {_e}")

    def _restore_toolbar_state(self): #vers 4
        """Restore QMainWindow toolbar state from txd_workshop.json.
        Uses an explicit layout version - bumped whenever ribbons are
        added/removed/renamed - so a stale save from an older ribbon
        layout is cleanly rejected instead of silently failing to
        restore (Qt's own toolbar-name hashing does this invisibly and
        without any way to detect success/failure)."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        try:
            import json
            from pathlib import Path
            from PyQt6.QtCore import QByteArray
            path = get_user_config_dir() / 'txd_workshop.json'
            if not path.exists():
                return
            data = json.loads(path.read_text())
            state_hex = data.get('toolbar_state')
            saved_version = data.get('toolbar_state_version')
            if state_hex and saved_version == self._RIBBON_LAYOUT_VERSION:
                ok = mw.restoreState(QByteArray.fromHex(state_hex.encode()),
                                      self._RIBBON_LAYOUT_VERSION)
                if ok:
                    self._set_status("Ribbon config loaded")
                    if self.main_window and hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("TXD Workshop: Ribbon config loaded")
                else:
                    print("[TXDWorkshop] _restore_toolbar_state: restoreState() returned False")
            elif state_hex:
                print(f"[TXDWorkshop] Saved ribbon layout is from an older version "
                      f"({saved_version} != {self._RIBBON_LAYOUT_VERSION}) - skipping, "
                      f"will save fresh on next change.")
        except Exception as _e:
            print(f"[TXDWorkshop] _restore_toolbar_state error: {_e}")
        finally:
            # Safety net: restoreState() can leave a ribbon fully hidden
            # (e.g. if it was saved mid-drag, floating off-screen, or
            # squeezed out) with no way for the user to bring it back -
            # there's no "closed" state exposed anywhere for these ribbons,
            # so force every one of them visible no matter what happened
            # above. Only position/floating/row is meant to be restorable,
            # never full visibility.
            for tb in (getattr(self, '_tb_transform', None),
                       getattr(self, '_tb_nav', None),
                       getattr(self, '_tb_effects', None),
                       getattr(self, '_tb_name', None),
                       getattr(self, '_tb_format', None),
                       getattr(self, '_tb_mipmaps', None)):
                if tb is not None:
                    tb.setVisible(True)
                    tb.toggleViewAction().setChecked(True)

    def _pan_preview(self, dx, dy): #vers 2
        """Pan preview by dx, dy pixels - FIXED"""
        if hasattr(self, 'preview_widget') and self.preview_widget:
            self.preview_widget.pan(dx, dy)

    def _pick_background_color(self): #vers 2
        """Open color picker for background"""
        pw = self.preview_widget
        start = pw.bg_color if pw.bg_color is not None else pw._get_ui_color('viewport_bg')
        color = QColorDialog.getColor(start, self, "Pick Background Color")
        if color.isValid():
            self.preview_widget.set_background_color(color)

    def _apply_title_font(self): #vers 1
        """Apply title font to title bar labels"""
        if hasattr(self, 'title_font'):
            # Find all title labels
            for label in self.findChildren(QLabel):
                if label.objectName() == "title_label":
                    label.setFont(self.title_font)

    def _apply_panel_font(self): #vers 1
        """Apply panel font to info panels and labels"""
        if hasattr(self, 'panel_font'):
            # Apply to info labels (Mipmaps, Bumpmaps, status labels)
            for label in self.findChildren(QLabel):
                if any(x in label.text() for x in ["Mipmaps:", "Bumpmaps:", "Status:", "Type:", "Format:"]):
                    label.setFont(self.panel_font)

    def _apply_button_font(self): #vers 1
        """Apply button font to all buttons"""
        if hasattr(self, 'button_font'):
            for button in self.findChildren(QPushButton):
                button.setFont(self.button_font)

    def _apply_infobar_font(self): #vers 1
        """Apply fixed-width font to info bar at bottom"""
        if hasattr(self, 'infobar_font'):
            if hasattr(self, 'info_bar'):
                self.info_bar.setFont(self.infobar_font)

    def _set_icon_display_mode(self, mode: str): #vers 1
        """Set button display mode: both, icons or text."""
        if mode != self.button_display_mode:
            self.button_display_mode = mode
            self._update_all_buttons()

    def _get_icon_color(self): #vers 3
        """Get icon colour from current theme — returns text_primary.
        Falls back to main_window app_settings if own settings not loaded."""
        as_ = (self.app_settings
               or getattr(getattr(self, 'main_window', None), 'app_settings', None))
        if as_:
            try:
                colors = as_.get_theme_colors() or {}
                return colors.get('text_primary', '#cccccc')
            except Exception:
                pass
        return '#cccccc'

    def _refresh_icons(self): #vers 8
        """Refresh all button icons after theme change."""
        SVGIconFactory.clear_cache()
        c = self._get_icon_color()
        SVGIconFactory.set_theme_color(c)

        _icon_map = [
            # Title bar / toolbar
            ('open_btn',            'open_icon'),
            ('save_btn',            'save_icon'),
            ('saveall_btn',         'saveas_icon'),
            ('export_all_btn',      'package_icon'),
            ('undo_btn',            'undo_icon'),
            ('info_btn',            'info_icon'),
            ('settings_btn',        'settings_icon'),
            ('minimize_btn',        'minimize_icon'),
            ('maximize_btn',        'maximize_icon'),
            ('close_btn',           'close_icon'),
            ('open_img_btn',        'folder_icon'),
            ('txd_search_btn',      'search_icon'),
            # Docked mode mini toolbar
            ('open_txd_btn',        'open_icon'),
            ('save_txd_btn',        'save_icon'),
            # Left transform toolbar (icon grid)
            ('flip_vert_btn',       'flip_vert_icon'),
            ('flip_horz_btn',       'flip_horz_icon'),
            ('rotate_cw_btn',       'rotate_cw_icon'),
            ('rotate_ccw_btn',      'rotate_ccw_icon'),
            ('copy_btn',            'copy_icon'),
            ('paste_btn',           'paste_icon'),
            ('create_texture_btn',  'create_texture_icon'),
            ('delete_texture_btn',  'delete_texture_icon'),
            ('duplicate_texture_btn','duplicate_icon'),
            ('paint_btn',           'paint_texture_icon'),
            ('check_dff_btn',       'analyze_icon'),
            ('build_from_dff_btn',  'build_from_dff_icon'),
            ('filters_btn',         'filter_icon'),
            ('switch_btn',          'switch_view_icon'),
            ('invert_btn',          'invert_alpha_icon'),
            ('gen_alpha_btn',       'paint_icon'),
            ('props_btn',           'properties_icon'),
            # Info panel buttons
            ('import_btn',          'import_icon'),
            ('export_btn',          'export_icon'),
            ('convert_btn',         'format_convert_icon'),
            ('properties_btn',      'settings_icon'),
            ('analyze_btn',         'analyze_icon'),
            # Mipmap row
            ('create_mipmaps_btn',  'add_icon'),
            ('remove_mipmaps_btn',  'delete_icon'),
            ('show_mipmaps_btn',    'view_icon'),
            ('compress_btn',        'compress_icon'),
            ('uncompress_btn',      'uncompress_icon'),
            ('upscale_btn',         'upscale_icon'),
            # Bumpmap row
            ('import_bumpmap_btn',  'import_icon'),
            ('export_bumpmap_btn',  'export_icon'),
            # Right preview bar
            ('resize_texture_btn',  '_resize_icon'),
        ]
        for attr, method in _icon_map:
            btn = getattr(self, attr, None)
            if btn is None:
                continue
            fn = getattr(self.icon_factory, method, None)
            if fn is None:
                continue
            try:
                btn.setIcon(fn(color=c))
            except TypeError:
                try:
                    btn.setIcon(fn())
                except Exception:
                    pass

        # Refresh right preview bar icons on theme change
        try:
            c2 = self._get_icon_color()
            tip_to_icon = {
                'Zoom In': 'zoom_in_icon', 'Zoom Out': 'zoom_out_icon',
                'Reset View': 'view_reset_icon', 'Fit to Window': 'fit_icon',
                'Pan Up': 'arrow_up_icon', 'Pan Down': 'arrow_down_icon',
                'Pan Left': 'arrow_left_icon', 'Pan Right': 'arrow_right_icon',
                'Pick Background': 'color_picker_icon',
                'Resize Texture': '_resize_icon',
                'Checkerboard': 'checkerboard_icon',
                'Colour Adjustments…': 'knob_icon',
                'Seamless Tool…': 'seamless_icon',
                'Snow Effect…': 'snow_icon',
                'Alpha Coverage…': 'alpha_coverage_icon',
            }
            for btn in getattr(self, '_preview_ctrl_view_btns', []):
                fn_name = tip_to_icon.get(btn.toolTip())
                if fn_name:
                    fn = getattr(self.icon_factory, fn_name, None)
                    if fn:
                        try:
                            btn.setIcon(fn(color=c2))
                        except Exception:
                            pass
        except Exception:
            pass

        # Update middle btn row visibility (may have changed docked state)
        if hasattr(self, '_middle_btn_row'):
            self._middle_btn_row.setVisible(
                self.is_docked and not self.standalone_mode)
        self._apply_custom_icons()

    def _apply_theme(self): #vers 6
        """Apply global app theme — uses QApplication stylesheet set by app_settings."""
        try:
            mw = getattr(self, 'main_window', None)
            app_settings = None
            if hasattr(self, 'app_settings') and self.app_settings:
                app_settings = self.app_settings
            elif mw and hasattr(mw, 'app_settings'):
                app_settings = mw.app_settings

            if app_settings and hasattr(app_settings, 'get_stylesheet'):
                # Apply to QApplication so all widgets inherit it
                from PyQt6.QtWidgets import QApplication
                ss = app_settings.get_stylesheet()
                if ss:
                    QApplication.instance().setStyleSheet(ss)
            # Clear any widget-level override so we inherit from QApplication
            self.setStyleSheet("")
            if app_settings:                    # panel effects, image and transparency
                from apps.utils.app_settings_system import apply_panel_effects
                apply_panel_effects(self, app_settings)
        except Exception as e:
            print(f"Theme application error: {e}")

    def _setup_status_indicators(self): #vers 5
        """Setup status indicators with texture info and visible resize button"""
        self.status_frame = QFrame()
        self.status_frame.setFixedHeight(24)
        self.status_layout = QHBoxLayout(self.status_frame)
        self.status_layout.setContentsMargins(5, 0, 5, 0)

        self.status_textures = QLabel("Textures: 0")
        self.status_layout.addWidget(self.status_textures)

        self.status_selected = QLabel("Selected: None")
        self.status_layout.addWidget(self.status_selected)

        self.status_size = QLabel("TXD Size: Unknown")
        self.status_layout.addWidget(self.status_size)

        self.status_layout.addStretch()

        self.status_modified = QLabel("")
        self.status_layout.addWidget(self.status_modified)

        # NEW: Texture dimension info
        self.info_size = QLabel("Size: -")
        self.status_layout.addWidget(self.info_size)

        # NEW: Texture format info
        self.format_status_label = QLabel("Format: -")
        self.status_layout.addWidget(self.format_status_label)

        # Add visible resize button with icon
        self.resize_grip_btn = QPushButton()
        self.resize_grip_btn.setIcon(self.icon_factory._resize_icon(color=self._get_icon_color()))
        self.resize_grip_btn.setIconSize(QSize(20, 20))
        self.resize_grip_btn.setFixedSize(20, 20)
        self.resize_grip_btn.setToolTip("Drag to resize window")
        self.resize_grip_btn.setStyleSheet("""
            QPushButton {
                background-color: transparent;
                border: none;
                padding: 0px;
            }
            QPushButton:hover {
                background-color: palette(mid);
            }
        """)
        # Make it act like a resize grip
        self.resize_grip_btn.setCursor(Qt.CursorShape.SizeFDiagCursor)
        self.status_layout.addWidget(self.resize_grip_btn)

        return self.status_frame

    def _update_status_indicators(self): #vers 3
        """Update status indicators"""
        if hasattr(self, 'status_textures'):
            self.status_textures.setText(f"Textures: {len(self.texture_list)}")
        # Update middle panel header with texture count
        hdr = getattr(self, '_textures_header', None)
        if hdr:
            n = len(self.texture_list)
            hdr.setText(f"Textures  ({n})" if n else "Textures")

        if hasattr(self, 'status_selected'):
            if self.selected_texture:
                name = self.selected_texture.get('name', 'Unknown')
                self.status_selected.setText(f"Selected: {name}")
            else:
                self.status_selected.setText("Selected: None")

        if hasattr(self, 'status_size'):
            if self.current_txd_data:
                size_kb = len(self.current_txd_data) / 1024
                self.status_size.setText(f"TXD Size: {size_kb:.1f} KB")
            else:
                self.status_size.setText("TXD Size: Unknown")

        if hasattr(self, 'status_modified'):
            if self.windowTitle().endswith("*"):
                self.status_modified.setText("MODIFIED")
                self.status_modified.setStyleSheet("color: orange; font-weight: bold;")
            else:
                self.status_modified.setText("")
                self.status_modified.setStyleSheet("")

        self._push_status_to_img_factory()

    def _push_status_to_img_factory(self): #vers 1
        """Relay this workshop's status bar text to IMG Factory's own status
        bar when docked. Controlled by the 'Relay status to IMG Factory'
        setting - useful on its own, and doubly so when 'Show status bar'
        is turned off, since the info still needs to surface somewhere."""
        if self.standalone_mode:
            return
        if not getattr(self, 'relay_status_to_img_factory', True):
            return
        mw = getattr(self, 'main_window', None)
        if not mw or not hasattr(mw, 'show_status'):
            return
        parts = []
        for attr in ('status_textures', 'status_selected', 'status_size'):
            lbl = getattr(self, attr, None)
            if lbl:
                parts.append(lbl.text())
        if parts:
            try:
                mw.show_status("TXD Workshop - " + "  |  ".join(parts))
            except Exception:
                pass

    def _enable_name_edit(self, event, is_alpha): #vers 1
        """Enable name editing on click"""
        if is_alpha:
            self.info_alpha_name.setReadOnly(False)
            self.info_alpha_name.selectAll()
            self.info_alpha_name.setFocus()
        else:
            self.info_name.setReadOnly(False)
            self.info_name.selectAll()
            self.info_name.setFocus()

    def _connect_texture_table_signals(self): #vers 1
        """Connect texture table signals for mipmap manager"""
        # Double-click to open mipmap manager
        self.texture_table.itemDoubleClicked.connect(self._on_texture_table_double_click)

    def _set_transform_buttons_enabled(self, enabled: bool): #vers 1
        """Enable/disable transform buttons in BOTH icon and text panels.
        The text panel's _btn() calls overwrite self.flip_vert_btn etc, so when
        the icon panel is visible (narrow mode) those self.X refs point to hidden
        text-panel buttons. Fix: enable all QPushButtons in the icon panel too.
        """
        # Text panel buttons (via self.X refs)
        transform_attrs = [
            'flip_vert_btn', 'flip_horz_btn', 'rotate_cw_btn', 'rotate_ccw_btn',
            'copy_btn', 'delete_texture_btn', 'duplicate_texture_btn',
            'filters_btn', 'paint_btn', 'switch_btn', 'gen_alpha_btn',
            'props_btn', 'convert_btn', 'compress_btn', 'uncompress_btn',
            'resize_btn', 'upscale_btn', 'bitdepth_btn',
        ]
        for attr in transform_attrs:
            btn = getattr(self, attr, None)
            if btn is not None:
                btn.setEnabled(enabled)

        # Icon panel buttons — find by walking the panel's children
        icon_panel = getattr(self, '_transform_icon_panel_ref', None)
        if icon_panel:
            from PyQt6.QtWidgets import QPushButton
            for btn in icon_panel.findChildren(QPushButton):
                btn.setEnabled(enabled)

    def _set_selection_buttons_enabled(self, enabled: bool): #vers 1
        """Enable/disable buttons that need a texture selected."""
        self._set_transform_buttons_enabled(enabled)
        for attr in ('export_btn', 'switch_btn', 'invert_btn',
                     'gen_alpha_btn', 'props_btn'):
            btn = getattr(self, attr, None)
            if btn is not None:
                btn.setEnabled(enabled)

    def _gamepad_saved(self): #vers 1
        """Saved controller on/off from txd_workshop.json."""
        import json
        try:
            return bool(json.loads(self._ribbon_config_path().read_text()).get('gamepad_enabled'))
        except (OSError, ValueError):
            return False

    def _toggle_gamepad(self, on): #vers 1
        """Start or stop the PS5 / game controller; remembered in txd_workshop.json."""
        import json
        pad = getattr(self, '_gamepad', None)
        if on and pad is None:
            from apps.methods.gamepad_input import GamepadPoller
            pad = GamepadPoller(self)
            pad.connected.connect(lambda n: self._set_status(
                f"Controller connected: {n}" if n else "Controller disconnected"))
            try:
                pad.start()
            except ImportError:
                QMessageBox.warning(self, "Game Controller",
                                    "Controller support needs pygame 2:\n\npip install pygame")
                self.gamepad_btn.setChecked(False)
                return
            pad.state.connect(self._gamepad_step)
            self._gamepad = pad
            self._pad_zoom_t = 0.0
            self._set_status("Controller on: left stick pan, L2/R2 zoom, D-pad texture")
        elif not on and pad is not None:
            pad.stop()
            self._gamepad = None
        path = self._ribbon_config_path()
        try:
            data = json.loads(path.read_text())
        except (OSError, ValueError):
            data = {}
        data['gamepad_enabled'] = bool(on and getattr(self, '_gamepad', None) is not None)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(data, indent=2))

    def _gamepad_step(self, st): #vers 1
        """Controller: pan, zoom, pick texture, reset, flip, rotate, view mode, tabs."""
        pw = self.preview_widget
        dt = st['dt']
        if st['lx'] or st['ly']:
            pw.pan(int(-st['lx'] * 600 * dt), int(-st['ly'] * 600 * dt))
        zoom = st['rt'] - st['lt'] - st['ry']
        self._pad_zoom_t = getattr(self, '_pad_zoom_t', 0.0) + dt
        if abs(zoom) > 0.2 and self._pad_zoom_t >= 0.15:
            self._pad_zoom_t = 0.0
            pw.zoom_in() if zoom > 0 else pw.zoom_out()
        pressed = st['pressed']
        rows = self.texture_table.rowCount()
        if rows and ('up' in pressed or 'down' in pressed):
            row = self.texture_table.currentRow()
            row = (row + (1 if 'down' in pressed else -1)) % rows if row >= 0 else 0
            self.texture_table.selectRow(row)
        if 'a' in pressed and rows:
            self.texture_table.selectRow(max(0, self.texture_table.currentRow()))
        if 'b' in pressed:
            pw.reset_view()
        if self.selected_texture:
            if 'x' in pressed:
                self._flip_horizontal()
            if 'y' in pressed:
                self.switch_texture_view()
        if ('l1' in pressed or 'r1' in pressed) and self.txd_tabs.count() > 1:
            step = 1 if 'r1' in pressed else -1
            self.txd_tabs.setCurrentIndex((self.txd_tabs.currentIndex() + step) % self.txd_tabs.count())

    def switch_texture_view(self): #vers 5
        """Cycle through view modes with [Inv] enabled for Alpha AND Overlay"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        tex_name = self.selected_texture.get('name', '')
        has_alpha = self.selected_texture.get('has_alpha', False)

        # Get current state for this texture
        current_state = self.texture_view_states.get(tex_name, 0)

        # Cycle to next state
        next_state = (current_state + 1) % 4

        # Skip alpha/both/overlay states if no alpha
        if not has_alpha and next_state > 0:
            QMessageBox.information(self, "No Alpha Channel",
                "This texture has no alpha channel.\n\n"
                "Use the [+] button to generate an alpha mask,\n"
                "or Import -> Import Alpha Channel to add one.")
            next_state = 0

        # Save state for this texture
        self.texture_view_states[tex_name] = next_state
        self._current_view_state = next_state

        # Update display
        self._update_texture_info(self.selected_texture)

        # Update button text
        state_labels = ["Normal", "Alpha", "Both", "Overlay"]
        self.switch_btn.setText(state_labels[next_state])

        # MODIFIED: Enable [Inv] for Alpha (1) OR Overlay (3)
        self.invert_btn.setEnabled((next_state == 1 or next_state == 3) and has_alpha)

        # Log message
        view_names = {
            0: "Normal View",
            1: "Alpha Mask View",
            2: "Split View (Normal | Alpha)",
            3: "Overlay View (Normal over Alpha)"
        }

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"Switched to {view_names[next_state]}")

    def _show_txd_search(self): #vers 1
        """Toggle TXD search box visibility."""
        if hasattr(self, 'txd_search_box'):
            visible = not self.txd_search_box.isVisible()
            self.txd_search_box.setVisible(visible)
            if visible:
                self.txd_search_box.setFocus()
            else:
                self.txd_search_box.clear()

    def _filter_txd_list(self, text: str): #vers 1
        """Filter TXD list by search text."""
        if not hasattr(self, 'txd_list_widget'): return
        for i in range(self.txd_list_widget.count()):
            item = self.txd_list_widget.item(i)
            item.setHidden(bool(text) and text.lower() not in item.text().lower())

    def _update_table_display(self): #vers 2
        """Update the middle panel table display after edits"""
        if not self.selected_texture:
            return

        row = self.texture_table.currentRow()
        if row < 0 or row >= len(self.texture_list):
            return

        tex = self.selected_texture

        # Rebuild details text with compression status
        details = f"Name: {tex['name']}\n"

        # Add alpha name if texture has alpha
        if tex.get('has_alpha', False):
            alpha_name = tex.get('alpha_name', tex['name'] + 'a')
            details += f"Alpha: {alpha_name}\n"

        if tex['width'] > 0:
            details += f"Size: {tex['width']}x{tex['height']}\n"

        # Show format with compression status
        fmt = tex['format']
        if 'DXT' in fmt:
            details += f"Format: {fmt} (Compressed)\n"
        else:
            details += f"Format: {fmt} (Uncompressed)\n"

        details += f"Alpha: {'Yes' if tex.get('has_alpha', False) else 'No'}"

        # Update text (col 1)
        details_item = self.texture_table.item(row, 1)
        if details_item:
            details_item.setText(details)

        # Also refresh thumbnail (col 0) so flip/rotate/filters show immediately
        rgba = tex.get('rgba_data')
        if rgba:
            thumb = self._create_thumbnail(rgba, tex['width'], tex['height'])
            thumb_item = self.texture_table.item(row, 0)
            if thumb_item and thumb:
                thumb_item.setIcon(QIcon(thumb))

    def _update_texture_info(self, texture): #vers 9
        """Update texture display with 4-state view support and checkerboard"""
        if not texture:
            self.info_name.setText("")
            self.info_alpha_name.setText("")
            self.info_alpha_name.setVisible(False)
            if hasattr(self, '_info_alpha_name_action'):
                self._info_alpha_name_action.setVisible(False)
            if hasattr(self, 'alpha_label'):
                self.alpha_label.setVisible(False)
            if hasattr(self, '_alpha_label_action'):
                self._alpha_label_action.setVisible(False)
            if hasattr(self, 'preview_widget'):
                self.preview_widget.setText("No texture selected")
            return

        # Set name
        name = texture.get('name', 'Unknown')
        self.info_name.setText(name)

        # Set alpha name if has alpha
        has_alpha = texture.get('has_alpha', False)
        if has_alpha:
            alpha_name = texture.get('alpha_name', name + 'a')
            self.info_alpha_name.setText(alpha_name)
            self.info_alpha_name.setVisible(True)
            if hasattr(self, '_info_alpha_name_action'):
                self._info_alpha_name_action.setVisible(True)
            if hasattr(self, 'alpha_label'):
                self.alpha_label.setVisible(True)
            if hasattr(self, '_alpha_label_action'):
                self._alpha_label_action.setVisible(True)
        else:
            self.info_alpha_name.setText("")
            self.info_alpha_name.setVisible(False)
            if hasattr(self, '_info_alpha_name_action'):
                self._info_alpha_name_action.setVisible(False)
            if hasattr(self, 'alpha_label'):
                self.alpha_label.setVisible(False)
            if hasattr(self, '_alpha_label_action'):
                self._alpha_label_action.setVisible(False)

        # Update size info WITH FILE SIZE
        width = texture.get('width', 0)
        height = texture.get('height', 0)
        rgba_data = texture.get('rgba_data', b'')
        file_size_kb = len(rgba_data) / 1024 if rgba_data else 0

        if hasattr(self, 'info_size'):
            self.info_size.setText(f"Size: {width}x{height}, {file_size_kb:.1f}KB")

        # Update format
        fmt = texture.get('format', 'Unknown')
        if hasattr(self, 'format_status_label'):
            self.format_status_label.setText(f"Format: {fmt}")

        # Update bit depth label - derive from format, not raw header byte
        fmt_depth = {
            'ARGB8888': 32, 'RGB888': 24,
            'RGB565': 16, 'ARGB1555': 16, 'ARGB4444': 16, 'RGB555': 16,
            'DXT1': 4, 'DXT2': 8, 'DXT3': 8, 'DXT4': 8, 'DXT5': 8,
            'PAL8': 8, 'PAL4': 4, 'LUM8': 8, 'A8L8': 16,
        }
        fmt_str = texture.get('format', '')
        depth = fmt_depth.get(fmt_str, texture.get('depth', 32))
        if hasattr(self, 'info_bitdepth'):
            self.info_bitdepth.setText(f"[{depth}bit]")

        # Get current view state
        tex_name = texture.get('name', '')
        view_state = self.texture_view_states.get(tex_name, 0)
        self._current_view_state = view_state

        # Update preview based on view state
        if hasattr(self, 'preview_widget') and rgba_data:

            if view_state == 0:  # Normal view
                self._show_normal_view(rgba_data, width, height)

            elif view_state == 1:  # Alpha mask view
                if has_alpha:
                    self._show_alpha_view(rgba_data, width, height)
                else:
                    self.preview_widget.setText("No alpha channel")

            elif view_state == 2:  # Split view (side-by-side)
                if has_alpha:
                    self._show_split_view(rgba_data, width, height)
                else:
                    self.preview_widget.setText("No alpha channel")

            elif view_state == 3:  # Overlay view
                if has_alpha:
                    self._show_overlay_view(rgba_data, width, height)
                else:
                    self.preview_widget.setText("No alpha channel")

    def _show_normal_view(self, rgba_data, width, height): #vers 3
        """Display normal texture - background handled by ZoomablePreview paintEvent"""
        self._preview_buffer = bytes(rgba_data)
        image = QImage(self._preview_buffer, width, height, width * 4, QImage.Format.Format_RGBA8888)
        pixmap = QPixmap.fromImage(image)
        self.preview_widget.set_pixmap(pixmap)

    def _show_alpha_view(self, rgba_data, width, height): #vers 2
        """Display alpha channel as grayscale with optional invert"""
        alpha_data = self._extract_alpha_channel(rgba_data)

        # Apply invert if enabled
        if self._invert_alpha:
            alpha_data = self._invert_grayscale(alpha_data)

        self._preview_buffer = bytes(alpha_data)  # keep ref to prevent GC
        image = QImage(self._preview_buffer, width, height, width * 4, QImage.Format.Format_RGBA8888)
        pixmap = QPixmap.fromImage(image)
        self.preview_widget.setPixmap(pixmap)

    def _show_split_view(self, rgba_data, width, height): #vers 1
        """Display normal and alpha side-by-side"""
        combined_width = width * 2
        combined_image = QImage(combined_width, height, QImage.Format.Format_RGBA8888)
        combined_image.fill(Qt.GlobalColor.black)

        painter = QPainter(combined_image)

        # Left: Normal
        normal_img = QImage(rgba_data, width, height, width * 4, QImage.Format.Format_RGBA8888)
        if self._show_checkerboard:
            normal_img = self._add_checkerboard_background(normal_img)
        painter.drawImage(0, 0, normal_img)

        # Right: Alpha mask
        alpha_data = self._extract_alpha_channel(rgba_data)
        alpha_img = QImage(alpha_data, width, height, width * 4, QImage.Format.Format_RGBA8888)
        painter.drawImage(width, 0, alpha_img)

        painter.end()

        pixmap = QPixmap.fromImage(combined_image)
        self.preview_widget.setPixmap(pixmap)

    def _show_overlay_view(self, rgba_data, width, height): #vers 2
        """Display normal over alpha with adjustable opacity - SUPPORTS INVERT"""
        # Create base alpha visualization
        alpha_data = self._extract_alpha_channel(rgba_data)

        # MODIFIED: Apply invert if enabled
        if self._invert_alpha:
            alpha_data = self._invert_grayscale(alpha_data)

        base_img = QImage(alpha_data, width, height, width * 4, QImage.Format.Format_RGBA8888)

        # Create normal image with adjusted opacity
        normal_img = QImage(rgba_data, width, height, width * 4, QImage.Format.Format_RGBA8888)

        # Composite images
        result = QImage(width, height, QImage.Format.Format_ARGB32)
        result.fill(Qt.GlobalColor.transparent)

        painter = QPainter(result)
        painter.drawImage(0, 0, base_img)
        painter.setOpacity(self._overlay_opacity / 100.0)
        painter.drawImage(0, 0, normal_img)
        painter.end()

        if self._show_checkerboard:
            result = self._add_checkerboard_background(result)

        pixmap = QPixmap.fromImage(result)
        self.preview_widget.setPixmap(pixmap)

    def _add_checkerboard_background(self, image): #vers 1
        """Add checkerboard pattern behind transparent areas"""
        result = QImage(image.size(), QImage.Format.Format_ARGB32)

        painter = QPainter(result)

        # Draw checkerboard
        size = self._checkerboard_size
        color1 = self._get_ui_color('border')
        color2 = self._get_ui_color('viewport_text')

        for y in range(0, image.height(), size):
            for x in range(0, image.width(), size):
                color = color1 if ((x // size) + (y // size)) % 2 == 0 else color2
                painter.fillRect(x, y, size, size, color)

        # Draw image on top
        painter.drawImage(0, 0, image)
        painter.end()

        return result

    def _toggle_checkerboard(self): #vers 2
        """Toggle checkerboard background display"""
        self._show_checkerboard = not self._show_checkerboard

        # Sync to preview widget background mode
        if hasattr(self, 'preview_widget'):
            if self._show_checkerboard:
                self.preview_widget.set_checkerboard_background()
            else:
                self.preview_widget.set_background_color(self.preview_widget.bg_color)

        # Refresh current texture
        if self.selected_texture:
            self._update_texture_info(self.selected_texture)

        if self.main_window and hasattr(self.main_window, 'log_message'):
            status = "enabled" if self._show_checkerboard else "disabled"
            self.main_window.log_message(f"Checkerboard background {status}")

    def _set_tiled_preview(self, n: int): #vers 1
        """Switch preview tiling: 1x1, 2x2, 3x3."""
        # Update cycle button label/tooltip
        self._tile_n = n
        if hasattr(self, '_tile_btn'):
            self._tile_btn.setToolTip(
                f"{n}x{n} tiled preview — click to cycle")
        # Update preview widget if it supports tiling
        if hasattr(self, 'preview_widget') and hasattr(self.preview_widget, 'set_tile'):
            self.preview_widget.set_tile(n)
        else:
            # Fallback: re-render with tiling via PIL
            rgba, w, h, _ = self._get_current_rgba()
            if rgba and n > 1:
                try:
                    from PIL import Image
                    img = Image.frombytes('RGBA', (w, h), rgba)
                    tiled = Image.new('RGBA', (w * n, h * n))
                    for y in range(n):
                        for x in range(n):
                            tiled.paste(img, (x * w, y * h))
                    from PyQt6.QtGui import QImage, QPixmap
                    td = tiled.tobytes()
                    qi = QImage(td, tiled.width, tiled.height,
                                tiled.width * 4, QImage.Format.Format_RGBA8888)
                    pm = QPixmap.fromImage(qi)
                    if hasattr(self, 'preview_widget'):
                        self.preview_widget.setPixmap(pm.scaled(
                            self.preview_widget.size(),
                            Qt.AspectRatioMode.KeepAspectRatio,
                            Qt.TransformationMode.SmoothTransformation))
                except Exception as e:
                    if hasattr(self, 'status_label'):
                        self.status_label.setText(f"Tiled preview error: {e}")
            elif n == 1:
                t = getattr(self, 'selected_texture', None)
                if t:
                    try: self._update_texture_info(t)
                    except Exception: pass

    def _focus_search(self): #vers 1
        """Focus search input via Ctrl+F"""
        if hasattr(self, 'search_input'):
            self.search_input.setFocus()
            self.search_input.selectAll()

    def keyPressEvent(self, event): #vers 1
        """Handle keyboard shortcuts"""
        from PyQt6.QtCore import Qt

        # D key - Dock/Undock toggle
        if event.key() == Qt.Key.Key_D and not event.modifiers():
            self.toggle_dock_mode()
            event.accept()
            return

        # T key - Tear out (same as undock)
        if event.key() == Qt.Key.Key_T and not event.modifiers():
            if self.is_docked:
                self._undock_from_main()
            event.accept()
            return

        super().keyPressEvent(event)

    def _setup_hotkeys(self): #vers 4
        """Plasma6-style keyboard shortcuts, wired to TXD methods."""
        from PyQt6.QtGui import QShortcut, QKeySequence

        def _key(attr, seq, slot): #vers 1
            sc = QShortcut(QKeySequence(seq), self)
            sc.activated.connect(slot)
            setattr(self, attr, sc)

        # File
        _key('hotkey_open',       QKeySequence.StandardKey.Open,    self.open_txd_file)
        _key('hotkey_save',       QKeySequence.StandardKey.Save,    self._save_txd_file)
        _key('hotkey_force_save', "Alt+Shift+S",                    self._force_save_txd)
        _key('hotkey_save_as',    QKeySequence.StandardKey.SaveAs,  self._save_as_txd_file)
        _key('hotkey_close',      QKeySequence.StandardKey.Close,   self.close)
        # Edit
        _key('hotkey_undo',       QKeySequence.StandardKey.Undo,    self._undo_last_action)
        _key('hotkey_copy',       QKeySequence.StandardKey.Copy,    self._copy_texture)
        _key('hotkey_paste',      QKeySequence.StandardKey.Paste,   self._paste_texture)
        _key('hotkey_delete',     QKeySequence.StandardKey.Delete,  self._delete_texture)
        _key('hotkey_duplicate',  "Ctrl+D",                         self._duplicate_texture)
        _key('hotkey_rename',     "F2",                             self._rename_texture_shortcut)
        # Texture
        _key('hotkey_import',     "Ctrl+I",                         self._import_normal_texture)
        _key('hotkey_export',     "Ctrl+E",                         self.export_selected_texture)
        _key('hotkey_export_all', "Ctrl+Shift+E",                   self.export_all_textures)
        # View
        _key('hotkey_refresh',    QKeySequence.StandardKey.Refresh, self._reload_texture_table)
        _key('hotkey_properties', "Alt+Return",                     self.show_properties)
        _key('hotkey_settings',   QKeySequence.StandardKey.Preferences, self._show_settings_dialog)
        _key('hotkey_find',       QKeySequence.StandardKey.Find,    self._focus_search)
        # Help
        _key('hotkey_help',       QKeySequence.StandardKey.HelpContents, self._show_txd_info)

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Hotkeys initialized (Plasma6 standard)")
