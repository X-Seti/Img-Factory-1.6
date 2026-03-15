#belongs in gui/gui_layout_custom.py - Version 6
# X-Seti - February04 2026 - Img Factory 1.6 - Custom UI Module

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QTabWidget, QFrame,
    QGroupBox, QPushButton, QLabel, QCheckBox
)
from PyQt6.QtCore import Qt, QObject, pyqtSignal, QSize, QTimer
from PyQt6.QtGui import QIcon
from .gui_layout import IMGFactoryGUILayout
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.imgfactory_svg_icons import (
    get_add_icon, get_open_icon, get_refresh_icon, get_close_icon,
    get_save_icon, get_export_icon, get_import_icon, get_remove_icon,
    get_edit_icon, get_view_icon, get_search_icon, get_settings_icon,
    get_rebuild_icon, get_undobar_icon, get_undo_icon, get_redo_icon
)

from apps.gui.gui_menu_custom import CustomMenuManager, show_popup_menu_at_button

# Temporary 3D viewport placeholder
class COL3DViewport(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setMinimumSize(400, 400)
        layout = QVBoxLayout(self)
        label = QLabel("3D Viewport - Placeholder")
        label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(label)
    def set_current_file(self, col_file): pass
    def set_view_options(self, **options): pass

VIEWPORT_AVAILABLE = False  # 3D viewport not yet implemented

# Add root directory to path
App_name = "Img Factory"
vers = "1.6"
DEBUG_STANDALONE = False

class TitleBarEventFilter(QObject):
    """Event filter for custom title bar to handle dragging"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.start_pos = None
    
    def eventFilter(self, obj, event):
        # Implement drag functionality if needed
        # For now, just return False to let events pass through normally
        return False

# Import AppSettings
try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    APPSETTINGS_AVAILABLE = False
    print("Warning: AppSettings not available")


class IMGFactoryGUILayoutCustom(IMGFactoryGUILayout):
    """Custom UI version of IMGFactoryGUILayout with modern theme and layout"""

    def __init__(self, main_window):
        super().__init__(main_window)

        # Load setting: use system titlebar by default
        self.use_system_titlebar = True
        if hasattr(self.main_window, 'app_settings'):
            self.use_system_titlebar = self.main_window.app_settings.current_settings.get('use_system_titlebar', False)

        # Apply initial window flags
        self._apply_window_flags()

        # ONLY set window-specific stuff in standalone mode

        self.window_always_on_top = False

        # Corner resize variables for standalone
        self.dragging = False
        self.drag_position = None
        self.resizing = False
        self.resize_corner = None
        self.corner_size = 20
        self.resize_margin = 10  # Edge margin for resize detection
        self.hover_corner = None
        self._initialize_features()
        self.enable_debug_check = False
        self.custom_menu_manager = CustomMenuManager(main_window)
        main_window.custom_menu_manager = self.custom_menu_manager


    @property
    def window_context(self): #vers 1
        """Get the appropriate window context for method calls

        Returns self.main_window if available (when embedded in IMG Factory),
        otherwise returns self (when running standalone)
        """
        return self.main_window if self.main_window is not None else self


    def apply_ui_mode(self, ui_mode: str, show_toolbar: bool, show_status_bar: bool, show_menu_bar: bool): #vers 4
        """Apply UI mode: 'system' or 'custom'"""
        # STORE MODE EARLY
        self.ui_mode = ui_mode

        # If toolbar exists, apply visibility immediately
        if hasattr(self, 'titlebar') and self.titlebar:
            self.titlebar.setVisible(True)  # Always visible

            # Control window buttons based on mode
            show_controls = (ui_mode == 'custom')
            if hasattr(self, 'minimize_btn'):
                self.minimize_btn.setVisible(show_controls)
            if hasattr(self, 'maximize_btn'):
                self.maximize_btn.setVisible(show_controls)
            if hasattr(self, 'close_btn'):
                self.close_btn.setVisible(show_controls)

        # Menu bar visibility
        if hasattr(self.main_window, 'menuBar') and callable(self.main_window.menuBar):
            menu_bar = self.main_window.menuBar()
            if menu_bar:
                menu_bar.setVisible(show_menu_bar)

        # Status bar visibility
        if hasattr(self.main_window, 'statusBar') and callable(self.main_window.statusBar):
            status_bar = self.main_window.statusBar()
            if status_bar:
                status_bar.setVisible(show_status_bar)


    def create_main_ui_with_splitters(self, parent_layout): #vers 6
        """Create the main UI with toolbar for custom layout - TOOLBAR NOW IN _create_ui"""
        # NO LONGER CREATE TOOLBAR HERE - it's handled in imgfactory._create_ui

        # Create temp widget for main content
        temp_widget = QWidget()
        temp_layout = QVBoxLayout(temp_widget)
        temp_layout.setContentsMargins(0, 0, 0, 0)

        # Call parent method to create the main UI content
        super().create_main_ui_with_splitters(temp_layout)

        # Add the main content directly to parent layout
        parent_layout.addWidget(temp_widget)


        # NOW apply ui_mode visibility settings AFTER toolbar exists
        ui_mode = getattr(self, 'ui_mode', 'system')
        if ui_mode == 'custom':
            # Show window controls in custom mode
            if hasattr(self, 'minimize_btn'):
                self.minimize_btn.setVisible(True)
            if hasattr(self, 'maximize_btn'):
                self.maximize_btn.setVisible(True)
            if hasattr(self, 'close_btn'):
                self.close_btn.setVisible(True)
        else:
            # Hide window controls in system mode
            if hasattr(self, 'minimize_btn'):
                self.minimize_btn.setVisible(False)
            if hasattr(self, 'maximize_btn'):
                self.maximize_btn.setVisible(False)
            if hasattr(self, 'close_btn'):
                self.close_btn.setVisible(False)




    def _apply_window_flags(self): #vers 1
        """Apply window flags based on system/custom UI setting"""
        if not hasattr(self.main_window, 'setWindowFlags') or not hasattr(self.main_window, 'show'):
            return
        current_geometry = self.main_window.geometry()
        was_visible = self.main_window.isVisible()
        if self.use_system_titlebar:
            self.main_window.setWindowFlags(
                Qt.WindowType.Window |
                Qt.WindowType.WindowMinimizeButtonHint |
                Qt.WindowType.WindowMaximizeButtonHint |
                Qt.WindowType.WindowCloseButtonHint
            )
        else:
            self.main_window.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        self.main_window.setGeometry(current_geometry)
        if was_visible:
            self.main_window.show()


    def toggle_maximize_restore(self):
        if self.main_window.isMaximized():
            self.main_window.showNormal()
        else:
            self.main_window.showMaximized()


    def _show_workshop_settings(self): #vers 4
        """Show IMG Factory Settings dialog - calls proper settings module"""
        try:
            from apps.methods.imgfactory_ui_settings import show_imgfactory_settings_dialog
            show_imgfactory_settings_dialog(self.main_window)
        except Exception as e:
            self.main_window.log_message(f"Error opening settings: {str(e)}")
            import traceback
            traceback.print_exc()


    def _create_toolbar(self):

        apply_btn.clicked.connect(apply_settings)
        button_layout.addWidget(apply_btn)

        # OK button (apply and close)
        ok_btn = QPushButton("OK")
        ok_btn.setStyleSheet("""
            QPushButton {
                background: #0078d4;
                color: white;
                padding: 8px 20px;
                font-weight: bold;
                font-size: 11px;
            }
            QPushButton:hover {
                background: #1984d8;
            }
        """)
        ok_btn.setDefault(True)

        def ok_clicked():
            apply_settings()
            dialog.accept()

        ok_btn.clicked.connect(ok_clicked)
        button_layout.addWidget(ok_btn)

        layout.addLayout(button_layout)

        # Show dialog
        dialog.exec()


    def _create_toolbar(self): #vers 7
        """Create toolbar - ALL BUTTONS CONNECTED"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        from PyQt6.QtGui import QFont

        if not hasattr(self, 'title_font'):
            self.title_font = QFont("Arial", 14)
        if not hasattr(self, 'button_font'):
            self.button_font = QFont("Arial", 10)
        if not hasattr(self, 'icon_factory'):
            self.icon_factory = SVGIconFactory()
        if not hasattr(globals(), 'App_name'):
            from apps.components.Img_Factory.imgfactory import App_name

        self.titlebar = QWidget()
        self.titlebar.setFixedHeight(45)
        self.titlebar.setObjectName("titlebar")

        # Install event filter for drag detection
        self.titlebar_event_filter = TitleBarEventFilter()
        self.titlebar.installEventFilter(self.titlebar_event_filter)
        self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.titlebar.setMouseTracking(True)

        layout = QHBoxLayout(self.titlebar)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(5)

        # Get icon color from theme text_primary
        icon_color = "#000000"
        if hasattr(self, 'main_window') and hasattr(self.main_window, 'app_settings'):
            current_theme = self.main_window.app_settings.current_settings.get("theme", "default")
            theme_colors = self.main_window.app_settings.get_theme_colors(current_theme)
            if theme_colors:
                icon_color = theme_colors.get('text_primary', '#000000')

        # M Menu button - BEFORE settings
        self.menu_btn = QPushButton()
        self.menu_btn.setFont(self.button_font)
        self.menu_btn.setIcon(self.icon_factory.menu_m_icon(20, icon_color))
        self.menu_btn.setText("Menu")
        self.menu_btn.setIconSize(QSize(20, 20))
        self.menu_btn.clicked.connect(self._show_popup_menu)
        self.menu_btn.setToolTip("Main Menu")
        layout.addWidget(self.menu_btn)

        # Settings button - PREVENT DUPLICATE CONNECTIONS
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(self.icon_factory.settings_icon(20, icon_color))
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))

        try:
            self.settings_btn.clicked.disconnect()
        except (TypeError, RuntimeError):
            pass

        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip("Img Factory Settings")
        layout.addWidget(self.settings_btn)

        # RW Reference button
        self.rw_ref_btn = QPushButton()
        self.rw_ref_btn.setFont(self.button_font)
        self.rw_ref_btn.setIcon(SVGIconFactory.info_icon(20, icon_color))
        self.rw_ref_btn.setText("RW Ref")
        self.rw_ref_btn.setIconSize(QSize(20, 20))
        self.rw_ref_btn.clicked.connect(self._show_rw_reference)
        self.rw_ref_btn.setToolTip("RenderWare Format Reference (versions, sections, texture formats, platforms)")
        layout.addWidget(self.rw_ref_btn)

        layout.addStretch()

        # App title in center
        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(self.title_label)

        layout.addStretch()

    # = TAB NAVIGATION BUTTONS

        #TODO buttons and connect signals needs to be fixed.
        #'create_new_img': lambda: create_new_img(self.main_window),
        #'open_img_file': lambda: open_file_dialog(self.main_window),
        #'reload_table': lambda: reload_current_file(self.main_window),


        # File Entries button - CONNECTED
        #self.file_open_btn = QPushButton()
        #self.file_open_btn.setFont(self.button_font)
        #self.file_open_btn.setIcon(self.icon_factory.open_icon())
        #self.file_open_btn.setText("Open")
        #self.file_open_btn.setIconSize(QSize(20, 20))
        #self.file_open_btn.setToolTip("Open file (Ctrl+O)")
        #self.file_open_btn.clicked.connect(lambda: self.open_img_file)
        #self.file_open_btn.clicked.connect(self.open_img_file)
        #layout.addWidget(self.file_open_btn)

        # Directory Tree button - CONNECTED
        #self.file_save_btn = QPushButton()
        #self.file_save_btn.setFont(self.button_font)
        #self.file_save_btn.setIcon(self.icon_factory.save_icon())
        #self.file_save_btn.setText("Save")
        #self.file_save_btn.setIconSize(QSize(20, 20))
        #self.file_save_btn.setToolTip("Save File (Ctrl+S)")
        #self.file_save_btn.clicked.connect(self.save_img_entry)
        #self.file_save_btn.clicked.connect(lambda: self.show_project_manager_dialog)

        #layout.addWidget(self.file_save_btn)

        # Search button - CONNECTED (renamed from "Search Results")
        #self.file_rebuild_btn = QPushButton()
        #self.file_rebuild_btn.setFont(self.button_font)
        #self.file_rebuild_btn.setIcon(self.icon_factory.rebuild_icon())
        #self.file_rebuild_btn.setText("Rebuild")
        #self.file_rebuild_btn.setIconSize(QSize(20, 20))
        #self.file_rebuild_btn.setToolTip("Rebuild (Ctrl+R)")
        #self.file_rebuild_btn.clicked.connect(lambda: self.main_window.search_manager.show_search_dialog())
        #self.file_rebuild_btn.clicked.connect(self.rebuild_img)
        #layout.addWidget(self.file_rebuild_btn)


        layout.addStretch()

        # ==================== ACTION BUTTONS ====================

        # Undo button - CONNECTED
        self.undo_btn = QPushButton()
        self.undo_btn.setFont(self.button_font)
        self.undo_btn.setIcon(self.icon_factory.undo_icon(20, icon_color))
        self.undo_btn.setText("")
        self.undo_btn.setIconSize(QSize(20, 20))
        self.undo_btn.setMinimumWidth(40)
        self.undo_btn.setMaximumWidth(40)
        self.undo_btn.setMinimumHeight(30)
        self.undo_btn.setToolTip("Undo last change")
        self.undo_btn.setEnabled(False)

        # Connect to undo manager if available
        if hasattr(self.main_window, 'undo_manager'):
            self.undo_btn.clicked.connect(self.main_window.undo_manager.undo)

        layout.addWidget(self.undo_btn)

        # Info button - CONNECTED
        self.info_btn = QPushButton()
        self.info_btn.setIcon(self.icon_factory.info_icon(20, icon_color))
        self.info_btn.setIconSize(QSize(20, 20))
        self.info_btn.setMinimumWidth(40)
        self.info_btn.setMaximumWidth(40)
        self.info_btn.setMinimumHeight(30)
        self.info_btn.setToolTip("Application Information")
        self.info_btn.clicked.connect(self._show_imgfactory_info)
        layout.addWidget(self.info_btn)

        # Properties/Theme button - CONNECTED
        self.properties_btn = QPushButton()
        self.properties_btn.setFont(self.button_font)
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(24, icon_color))
        self.properties_btn.setIconSize(QSize(20, 20))
        self.properties_btn.setMinimumWidth(40)
        self.properties_btn.setMaximumWidth(40)
        self.properties_btn.setMinimumHeight(30)
        self.properties_btn.setToolTip("Theme Settings")
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        self.properties_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.properties_btn.customContextMenuRequested.connect(self._show_settings_context_menu)
        layout.addWidget(self.properties_btn)

        # AI Workshop button
        self.ai_btn = QPushButton()
        self.ai_btn.setFont(self.button_font)
        self.ai_btn.setIcon(SVGIconFactory.ai_icon(24, icon_color))
        self.ai_btn.setText("AI")
        self.ai_btn.setIconSize(QSize(24, 24))
        self.ai_btn.setMinimumWidth(55)
        self.ai_btn.setMaximumWidth(55)
        self.ai_btn.setMinimumHeight(30)
        self.ai_btn.setToolTip("AI Workshop – Local LLM chat via Ollama\nLeft-click: dock into tab\nRight-click: open standalone")
        self.ai_btn.clicked.connect(lambda: self.main_window.open_ai_workshop_docked())
        self.ai_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.ai_btn.customContextMenuRequested.connect(self._show_ai_context_menu)
        layout.addWidget(self.ai_btn)

        #self.debug_btn = QPushButton()
        #self.debug_btn.setFont(self.button_font)
        #self.debug_btn.setText("DEBUG")
        #self.debug_btn.setIconSize(QSize(20, 20))
        #self.debug_btn.setToolTip("Show tab debug info")
        #self.debug_btn.clicked.connect(self._debug_tabs)
        #layout.addWidget(self.debug_btn)

        # ==================== WINDOW CONTROLS (Custom UI only) ====================

        ui_mode = getattr(self, 'ui_mode', 'system')

        if ui_mode == 'custom':
            self.minimize_btn = QPushButton()
            self.minimize_btn.setIcon(self.icon_factory.minimize_icon(20, icon_color))
            self.minimize_btn.setIconSize(QSize(20, 20))
            self.minimize_btn.setMinimumWidth(40)
            self.minimize_btn.setMaximumWidth(40)
            self.minimize_btn.setMinimumHeight(30)
            self.minimize_btn.clicked.connect(self.main_window.showMinimized)
            self.minimize_btn.setToolTip("Minimize Window")
            layout.addWidget(self.minimize_btn)

            self.maximize_btn = QPushButton()
            self.maximize_btn.setIcon(self.icon_factory.maximize_icon(20, icon_color))
            self.maximize_btn.setIconSize(QSize(20, 20))
            self.maximize_btn.setMinimumWidth(40)
            self.maximize_btn.setMaximumWidth(40)
            self.maximize_btn.setMinimumHeight(30)
            self.maximize_btn.clicked.connect(self.toggle_maximize_restore)
            self.maximize_btn.setToolTip("Maximize/Restore Window")
            layout.addWidget(self.maximize_btn)

            self.close_btn = QPushButton()
            self.close_btn.setIcon(self.icon_factory.close_icon(20, icon_color))
            self.close_btn.setIconSize(QSize(20, 20))
            self.close_btn.setMinimumWidth(40)
            self.close_btn.setMaximumWidth(40)
            self.close_btn.setMinimumHeight(30)
            self.close_btn.clicked.connect(self.main_window.close)
            self.close_btn.setToolTip("Close Window")
            layout.addWidget(self.close_btn)

        return self.titlebar

    def _show_popup_menu(self): #vers 1
        """Show main popup menu"""
        try:
            if hasattr(self, 'menu_btn'):
                show_popup_menu_at_button(self.main_window, self.menu_btn)
            else:
                self.custom_menu_manager.show_popup_menu()
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Menu error: {str(e)}")


    def _create_left_three_section_panel(self): #vers 3
        """Create left panel with 3 sections: File Window, Status Window"""
        left_container = QWidget()
        left_layout = QVBoxLayout(left_container)
        left_layout.setContentsMargins(3, 3, 3, 3)
        left_layout.setSpacing(0)  # No spacing - splitter handles this

        # Create vertical splitter for the sections
        self.left_vertical_splitter = QSplitter(Qt.Orientation.Vertical)

        # 1. MIDDLE: File Window (table with sub-tabs)
        file_window = self._create_file_window()
        self.left_vertical_splitter.addWidget(file_window)

        # 2. BOTTOM: Status Window (log and status)
        #status_window = self.create_status_window()
        #self.left_vertical_splitter.addWidget(status_window)

        # Set section proportions: File(760px), Status(200px)
        self.left_vertical_splitter.setSizes([760, 200])

        # Prevent sections from collapsing completely
        self.left_vertical_splitter.setCollapsible(0, True)  # File window
        self.left_vertical_splitter.setCollapsible(1, True)  # Status window

        # Apply theme styling to vertical splitter
        self._apply_vertical_splitter_theme()

        left_layout.addWidget(self.left_vertical_splitter)
        return left_container


    def _switch_to_tab(self, index: int): #vers 5
        """Switch to the specified tab in the main tab widget"""
        try:
            # Check debug flag
            debug_enabled = False
            if hasattr(self.main_window, 'img_settings'):
                debug_enabled = self.main_window.img_settings.get("enable_debug", False)

            if debug_enabled:
                print(f"\n=== SWITCH TO TAB {index} ===")
                print(f"Has main_tab_widget: {hasattr(self.main_window, 'main_tab_widget')}")
                print(f"Has gui_layout: {hasattr(self.main_window, 'gui_layout')}")

            # Try main_tab_widget first (created in _create_ui)
            if hasattr(self.main_window, 'main_tab_widget') and self.main_window.main_tab_widget:
                tab_widget = self.main_window.main_tab_widget

                if debug_enabled:
                    print(f"Using main_tab_widget, count: {tab_widget.count()}")

                # Special handling for Directory Tree (index 1)
                if index == 1:
                    if not hasattr(self.main_window, 'directory_tree'):
                        from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser
                        integrate_directory_tree_browser(self.main_window)
                    
                    # Ensure the directory tree is properly populated with the game root
                    if hasattr(self.main_window, 'directory_tree') and hasattr(self.main_window, 'game_root'):
                        if hasattr(self.main_window.directory_tree, 'browse_directory'):
                            self.main_window.directory_tree.browse_directory(self.main_window.game_root)

                if index < tab_widget.count():
                    tab_widget.setCurrentIndex(index)
                    tab_name = tab_widget.tabText(index)

                    if debug_enabled:
                        print(f"✓ Switched to tab {index}: {tab_name}")

                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message(f"Switched to: {tab_name}")
                else:
                    if debug_enabled:
                        print(f"✗ Tab {index} doesn't exist (only {tab_widget.count()} tabs)")
                        
                    # Try to handle the case where the tab doesn't exist but we want to show the content
                    if index == 0:  # File entries
                        self._switch_to_file_entries()
                    elif index == 1:  # Directory tree
                        self._switch_to_directory_tree()
                    elif index == 2:  # Search
                        self._switch_to_search()
                return

            # Try gui_layout.tab_widget as fallback
            if hasattr(self.main_window, 'gui_layout'):
                if hasattr(self.main_window.gui_layout, 'tab_widget') and self.main_window.gui_layout.tab_widget:
                    tab_widget = self.main_window.gui_layout.tab_widget

                    if debug_enabled:
                        print(f"Using gui_layout.tab_widget, count: {tab_widget.count()}")

                    if index < tab_widget.count():
                        tab_widget.setCurrentIndex(index)

                        if debug_enabled:
                            print(f"✓ Switched to tab {index}")
                    else:
                        # If the tab doesn't exist, try our button-based functions
                        if index == 0:  # File entries
                            self._switch_to_file_entries()
                        elif index == 1:  # Directory tree
                            self._switch_to_directory_tree()
                        elif index == 2:  # Search
                            self._switch_to_search()
                    return

            if debug_enabled:
                print("✗ No valid tab widget found!")

            # As a final fallback, try to use our button-based switching functions
            if index == 0:  # File entries
                self._switch_to_file_entries()
            elif index == 1:  # Directory tree
                self._switch_to_directory_tree()
            elif index == 2:  # Search
                self._switch_to_search()

        except Exception as e:
            if debug_enabled:
                print(f"✗ Error switching to tab {index}: {str(e)}")
                import traceback
                traceback.print_exc()


    def _switch_to_file_entries(self): #vers 2
        """Switch to current IMG file tab (last active tab > 0)"""
        try:
            if not hasattr(self.main_window, 'main_tab_widget') or not self.main_window.main_tab_widget:
                self.main_window.log_message("No tab widget available")
                return

            tab_widget = self.main_window.main_tab_widget
            current_index = tab_widget.currentIndex()

            # If already on an IMG tab (> 0), stay there
            if current_index > 0:
                tab_name = tab_widget.tabText(current_index)
                self.main_window.log_message(f"Already on: {tab_name}")
                return

            # Find last active IMG tab or first IMG tab
            if tab_widget.count() > 1:
                # Switch to Tab 1 (first IMG tab)
                tab_widget.setCurrentIndex(1)
                tab_name = tab_widget.tabText(1)
                self.main_window.log_message(f"→ Switched to: {tab_name}")
            else:
                self.main_window.log_message("No IMG files loaded")

        except Exception as e:
            self.main_window.log_message(f"Error switching to file entries: {str(e)}")
            import traceback
            traceback.print_exc()


    def _switch_to_directory_tree(self): #vers 12
        """Switch to Directory Tree - setup once, then block button"""
        try:
            # If already setup, block second press
            if hasattr(self.main_window, '_dirtree_setup_complete'):
                self.main_window.log_message("Directory Tree already loaded")
                return

            # Check game root
            if not hasattr(self.main_window, 'game_root') or not self.main_window.game_root:
                from PyQt6.QtWidgets import QMessageBox
                reply = QMessageBox.question(
                    self.main_window,
                    "No Project Configured",
                    "No project is currently configured.\n\nWould you like to open the Project Manager now?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                )
                if reply == QMessageBox.StandardButton.Yes:
                    from apps.components.Project_Manager.project_manager import show_project_manager_dialog
                    show_project_manager_dialog(self.main_window)
                return

            # ONE-TIME SETUP: Create directory tree
            if not hasattr(self.main_window, 'directory_tree'):
                from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser
                if not integrate_directory_tree_browser(self.main_window):
                    self.main_window.log_message("Failed to load Directory Tree")
                    return

            # Place in file window
            if hasattr(self.main_window.gui_layout, 'middle_vertical_splitter'):
                splitter = self.main_window.gui_layout.middle_vertical_splitter
                if splitter and splitter.count() > 0:
                    file_window = splitter.widget(0)
                    layout = file_window.layout()
                    if layout:
                        layout.addWidget(self.main_window.directory_tree)
                        self.main_window.log_message("✓ Directory tree setup complete")

            # Browse to game root
            if hasattr(self.main_window.directory_tree, 'browse_directory'):
                self.main_window.directory_tree.browse_directory(self.main_window.game_root)

            # Update Tab 0 label to "Dir Tree"
            if hasattr(self.main_window, 'main_tab_widget'):
                self.main_window.main_tab_widget.setTabText(0, "Dir Tree")

            # Mark setup as complete BEFORE showing
            self.main_window._dirtree_setup_complete = True

            # Show directory tree, hide table
            if hasattr(self.main_window.gui_layout, 'table'):
                self.main_window.gui_layout.table.hide()
            self.main_window.directory_tree.show()

            # Switch to Tab 0
            if hasattr(self.main_window, 'main_tab_widget'):
                self.main_window.main_tab_widget.setCurrentIndex(0)
                self.main_window.log_message("→ Directory Tree")

        except Exception as e:
            self.main_window.log_message(f"Error: {str(e)}")
            import traceback
            traceback.print_exc()


    def _ensure_tab_0_for_directory_tree(self): #vers 1
        """Ensure Tab 0 exists and contains directory tree widget"""
        try:
            if not hasattr(self.main_window, 'main_tab_widget') or not self.main_window.main_tab_widget:
                return False

            tab_widget = self.main_window.main_tab_widget

            # Check if Tab 0 exists
            if tab_widget.count() == 0:
                # Create Tab 0
                from PyQt6.QtWidgets import QWidget, QVBoxLayout
                tab_0 = QWidget()
                tab_0_layout = QVBoxLayout(tab_0)
                tab_0_layout.setContentsMargins(0, 0, 0, 0)

                # Add directory tree if it exists
                if hasattr(self.main_window, 'directory_tree'):
                    tab_0_layout.addWidget(self.main_window.directory_tree)

                tab_widget.insertTab(0, tab_0, "Dir Tree")
                self.main_window.log_message("Created Tab 0 for Directory Tree")
                return True
            else:
                # Tab 0 exists - ensure it has directory tree
                tab_0 = tab_widget.widget(0)
                if tab_0 and hasattr(self.main_window, 'directory_tree'):
                    # Check if directory tree is already in tab 0
                    if self.main_window.directory_tree.parent() != tab_0:
                        layout = tab_0.layout()
                        if not layout:
                            from PyQt6.QtWidgets import QVBoxLayout
                            layout = QVBoxLayout(tab_0)
                            layout.setContentsMargins(0, 0, 0, 0)
                        layout.addWidget(self.main_window.directory_tree)

                    # Update tab label
                    tab_widget.setTabText(0, "Dir Tree")
                    return True
            return False
        except Exception as e:
            self.main_window.log_message(f"Error ensuring Tab 0: {str(e)}")
            return False


    def _switch_to_search(self): #vers 1
        """Show search dialog and log results to status window and log tab"""
        try:
            if hasattr(self.main_window, 'search_manager') and self.main_window.search_manager:
                # Connect search results to log system
                if hasattr(self.main_window.search_manager, 'search_results_signal'):
                    # Connect search results to both status window and log
                    def handle_search_results(results):
                        self.main_window.log_message(f"Search completed: Found {len(results)} results")
                        if hasattr(self, 'log') and self.log:
                            self.log.append(f"[SEARCH] Found {len(results)} results")
                    
                    self.main_window.search_manager.search_results_signal.connect(handle_search_results)
                
                self.main_window.search_manager.show_search_dialog()
            else:
                # Try to create a search dialog directly
                from apps.core.gui_search import ASearchDialog
                search_dialog = ASearchDialog(self.main_window)
                search_dialog.exec()
                
                self.main_window.log_message("→ Showing Search Dialog")
            
            # If we have a main tab widget, try to switch to the search tab if it exists
            if hasattr(self.main_window, 'main_tab_widget') and self.main_window.main_tab_widget:
                tab_widget = self.main_window.main_tab_widget
                for i in range(tab_widget.count()):
                    if "Search" in tab_widget.tabText(i):
                        tab_widget.setCurrentIndex(i)
                        self.main_window.log_message("→ Switched to Search Tab")
                        return
        except Exception as e:
            self.main_window.log_message(f"Error: {str(e)}")


    def _log_button_press(self, button_name: str, action_func=None): #vers 1
        """Wrapper to log button presses before executing action"""
        def wrapper():
            try:
                self.main_window.log_message(f"[BUTTON] {button_name}")
                if action_func:
                    action_func()
            except Exception as e:
                self.main_window.log_message(f"[ERROR] {button_name}: {str(e)}")
        return wrapper


    def _toggle_log_visibility(self): #vers 3
        """Toggle extended activity log visibility including search results, imports, exports, and removals"""
        try:
            if not hasattr(self, 'log') or not self.log:
                self.main_window.log_message("Log widget not available")
                return

            is_visible = self.log.isVisible()
            self.log.setVisible(not is_visible)

            if not is_visible:
                self.main_window.log_message("=" * 60)
                self.main_window.log_message("EXTENDED ACTIVITY LOG - All operations tracked")
                self.main_window.log_message("Includes: Search Results | Imports | Exports | Removals | System Events")
                self.main_window.log_message("=" * 60)
                if hasattr(self, 'log_btn'):
                    self.log_btn.setText("Hide Log")
            else:
                if hasattr(self, 'log_btn'):
                    self.log_btn.setText("Log")

        except Exception as e:
            self.main_window.log_message(f"Error: {str(e)}")


    def handle_set_assists_project_folder(self): #vers 1
        """Handle Set Project folder (Assists) menu action - Creates Assists folder with Models, Maps, Collisions, Textures, Other"""
        try:
            from PyQt6.QtWidgets import QFileDialog, QMessageBox
            import os
            
            # Start from user's home directory
            start_dir = os.path.expanduser("~")
            
            # Select base directory where "Assists" folder will be created
            base_folder = QFileDialog.getExistingDirectory(
                self.main_window,
                "Select Base Directory for Assists Folder",
                start_dir,
                QFileDialog.Option.ShowDirsOnly
            )
            
            if base_folder:
                # Create the main "Assists" folder
                assists_folder = os.path.join(base_folder, "Assists")
                
                # Create the Assists folder and subfolders
                subfolders = ["Models", "Maps", "Collisions", "Textures", "Other"]
                
                os.makedirs(assists_folder, exist_ok=True)
                self.main_window.log_message(f"Created Assists folder: {assists_folder}")
                
                for subfolder in subfolders:
                    subfolder_path = os.path.join(assists_folder, subfolder)
                    os.makedirs(subfolder_path, exist_ok=True)
                    self.main_window.log_message(f"Created subfolder: {subfolder_path}")
                
                # Store the assists folder path for other functions
                if not hasattr(self.main_window, 'assists_folder'):
                    self.main_window.assists_folder = assists_folder
                
                # Show success message
                QMessageBox.information(
                    self.main_window,
                    "Assists Project Folder Created",
                    f"Assists project folder structure created successfully:\n{assists_folder}\n\nSubfolders created:\n• Models/\n• Maps/\n• Collisions/\n• Textures/\n• Other/"
                )
                
                self.main_window.log_message(f"Assists project folder ready: {assists_folder}")
                
            else:
                self.main_window.log_message("Assists project folder creation cancelled")
                
        except Exception as e:
            self.main_window.log_message(f"Error creating Assists project folder: {str(e)}")





    def _show_rw_scan_dialog(self): #vers 1
        """Show RW version scan dialog: lists all detected versions, offers force-rescan."""
        try:
            from PyQt6.QtWidgets import (
                QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QLabel,
                QTableWidget, QTableWidgetItem, QHeaderView, QProgressDialog,
                QAbstractItemView
            )
            from PyQt6.QtCore import Qt, QTimer
            from PyQt6.QtGui import QColor, QPalette

            mw = self.main_window
            img_file = getattr(mw, 'current_img', None)

            if not img_file or not img_file.entries:
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.information(mw, "RW Scan", "No IMG file loaded.")
                return

            from apps.methods.rw_versions import get_rw_version_name, is_valid_rw_version

            # ── collect stats ────────────────────────────────────────
            def _collect(entries):
                """Return (rows, version_summary) from current entry list."""
                rows = []
                version_counts = {}
                for entry in entries:
                    ext = getattr(entry, 'extension', '').upper()
                    if ext not in ('DFF', 'TXD'):
                        continue
                    size = getattr(entry, 'size', 0)
                    rv   = getattr(entry, 'rw_version', 0)
                    rvn  = getattr(entry, 'rw_version_name', '')
                    if rv and is_valid_rw_version(rv):
                        label = rvn if rvn and rvn not in ('Unknown', '', 'N/A') \
                                    else get_rw_version_name(rv)
                        hex_v = f"0x{rv:08X}"
                    elif size == 0:
                        label, hex_v = "Empty", "—"
                    else:
                        label, hex_v = "Unknown", "—"
                    rows.append((entry.name, ext, hex_v, label))
                    version_counts[label] = version_counts.get(label, 0) + 1
                return rows, version_counts

            # ── build dialog ─────────────────────────────────────────
            dlg = QDialog(mw)
            dlg.setWindowTitle("RW Version Scan")
            dlg.setMinimumSize(700, 480)
            dlg.resize(750, 520)

            pal   = dlg.palette()
            c_bg  = pal.color(QPalette.ColorRole.Base).name()
            c_alt = pal.color(QPalette.ColorRole.AlternateBase).name()
            c_txt = pal.color(QPalette.ColorRole.Text).name()
            c_hdr = pal.color(QPalette.ColorRole.Button).name()
            c_sel = pal.color(QPalette.ColorRole.Highlight).name()

            dlg.setStyleSheet(f"""
                QDialog        {{ background:{c_bg}; color:{c_txt}; }}
                QTableWidget   {{ background:{c_bg}; color:{c_txt};
                                  gridline-color:{c_hdr};
                                  selection-background-color:{c_sel}; }}
                QHeaderView::section {{ background:{c_hdr}; color:{c_txt};
                                        padding:4px; border:none; }}
                QPushButton    {{ background:{c_hdr}; color:{c_txt};
                                  padding:4px 12px; border-radius:3px; }}
                QPushButton:hover {{ background:{c_sel}; }}
                QLabel         {{ color:{c_txt}; }}
            """)

            vlay = QVBoxLayout(dlg)
            vlay.setSpacing(6)

            # Title + summary row
            title_lbl = QLabel()
            title_lbl.setStyleSheet("font-weight:bold; font-size:12px;")
            vlay.addWidget(title_lbl)

            summary_lbl = QLabel()
            summary_lbl.setWordWrap(True)
            vlay.addWidget(summary_lbl)

            # Table: Name | Type | Version hex | Version name
            table = QTableWidget()
            table.setColumnCount(4)
            table.setHorizontalHeaderLabels(["Name", "Type", "RW Hex", "RW Version"])
            table.horizontalHeader().setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
            table.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
            table.horizontalHeader().setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
            table.horizontalHeader().setSectionResizeMode(3, QHeaderView.ResizeMode.ResizeToContents)
            table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
            table.setAlternatingRowColors(True)
            table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
            table.verticalHeader().setVisible(False)
            vlay.addWidget(table)

            def _populate(entries):
                rows, vcounts = _collect(entries)
                table.setRowCount(len(rows))
                unknown_count = 0
                for r, (name, ext, hex_v, label) in enumerate(rows):
                    table.setItem(r, 0, QTableWidgetItem(name))
                    table.setItem(r, 1, QTableWidgetItem(ext))
                    table.setItem(r, 2, QTableWidgetItem(hex_v))
                    table.setItem(r, 3, QTableWidgetItem(label))
                    if label == "Unknown":
                        unknown_count += 1
                        for c in range(4):
                            item = table.item(r, c)
                            if item:
                                item.setForeground(QColor(pal.color(QPalette.ColorRole.PlaceholderText)))

                dff_txd_total = sum(vcounts.values())
                title_lbl.setText(
                    f"RW Version Scan — {os.path.basename(img_file.file_path)}"
                    f"  ({dff_txd_total} DFF/TXD entries)"
                )
                parts = [f"{v}: {n}" for v, n in sorted(vcounts.items(), key=lambda x: -x[1])]
                summary_lbl.setText("  |  ".join(parts) if parts else "No DFF/TXD entries.")
                return unknown_count

            import os
            unknown_before = _populate(img_file.entries)

            # Bottom buttons
            hlay = QHBoxLayout()

            status_lbl = QLabel("")
            status_lbl.setStyleSheet("font-style:italic;")
            hlay.addWidget(status_lbl, 1)

            rescan_btn = QPushButton("⟳  Force Rescan All")
            rescan_btn.setToolTip("Re-read every DFF/TXD entry from disk and detect RW version")

            def _do_rescan():
                rescan_btn.setEnabled(False)
                status_lbl.setText("Scanning…")
                dlg.repaint()

                dff_txd_entries = [
                    e for e in img_file.entries
                    if getattr(e, 'extension', '').upper() in ('DFF', 'TXD')
                ]
                total = len(dff_txd_entries)
                found = 0

                for e in dff_txd_entries:
                    e.rw_version = 0
                    e.rw_version_name = ''
                    e._version_detected = False
                    try:
                        e.detect_file_type_and_version()
                        if getattr(e, 'rw_version', 0):
                            found += 1
                    except Exception:
                        pass

                unknown_after = _populate(img_file.entries)

                # Refresh the main IMG table too
                try:
                    from apps.methods.export_shared import get_active_table
                    active_table = get_active_table(mw)
                    if active_table and hasattr(mw, 'gui_layout'):
                        populator = getattr(mw.gui_layout, 'table_populator', None)
                        if populator is None:
                            from apps.methods.populate_img_table import IMGTablePopulator
                            populator = IMGTablePopulator(mw)
                        # Refresh only the RW Version column (col 5) for efficiency
                        from apps.methods.populate_img_table import IMGTablePopulator
                        pop = IMGTablePopulator(mw)
                        for row, entry in enumerate(img_file.entries):
                            if row >= active_table.rowCount():
                                break
                            ext = getattr(entry, 'extension', '').upper()
                            if ext in ('DFF', 'TXD'):
                                ver_text = pop.get_rw_version_light(entry)
                                from PyQt6.QtWidgets import QTableWidgetItem as QTI
                                item = QTI(ver_text)
                                active_table.setItem(row, 5, item)
                except Exception:
                    pass

                status_lbl.setText(
                    f"Done — {found}/{total} entries resolved, "
                    f"{unknown_after} still unknown."
                )
                rescan_btn.setEnabled(True)

            rescan_btn.clicked.connect(_do_rescan)
            hlay.addWidget(rescan_btn)

            close_btn = QPushButton("Close")
            close_btn.clicked.connect(dlg.accept)
            hlay.addWidget(close_btn)

            vlay.addLayout(hlay)

            if unknown_before > 0:
                status_lbl.setText(
                    f"{unknown_before} entries have no detected RW version — "
                    "click Rescan to re-read from disk."
                )

            dlg.exec()

        except Exception as e:
            try:
                self.main_window.log_message(f"RW Scan dialog error: {e}")
            except Exception:
                pass

    def _show_rw_reference(self): #vers 2
        """Show comprehensive GTA format reference — IMG, RW, TXD, COL"""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QTabWidget, QTextEdit, QLabel
        from PyQt6.QtGui import QFont
        from PyQt6.QtCore import Qt

        dialog = QDialog(self.main_window)
        dialog.setWindowTitle("GTA File Format Reference — IMG Factory 1.6")
        dialog.setMinimumWidth(860)
        dialog.setMinimumHeight(700)
        dialog.resize(900, 740)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(8)
        layout.setContentsMargins(10, 10, 10, 10)

        header = QLabel("GTA File Format Reference  —  IMG Factory 1.6")
        header.setFont(QFont("Arial", 13, QFont.Weight.Bold))
        header.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(header)

        sub = QLabel("Documents all researched format details for IMG archives, RenderWare sections/versions, TXD textures, and COL collision files.")
        sub.setWordWrap(True)
        sub.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(sub)

        tabs = QTabWidget()
        layout.addWidget(tabs)

        def make_text(html: str) -> QTextEdit:
            t = QTextEdit()
            t.setReadOnly(True)
            t.setHtml(html)
            return t

        # Derive all colours from the live Qt palette so the dialog is correct
        # on every theme (light, dark, system) with zero hardcoded hex values.
        from PyQt6.QtGui import QPalette
        from PyQt6.QtWidgets import QApplication
        pal = QApplication.instance().palette()

        def _hex(role):
            return pal.color(role).name()

        c_bg     = _hex(QPalette.ColorRole.Base)
        c_bg2    = _hex(QPalette.ColorRole.AlternateBase)
        c_text   = _hex(QPalette.ColorRole.Text)
        c_text2  = _hex(QPalette.ColorRole.PlaceholderText)
        c_accent = _hex(QPalette.ColorRole.Highlight)
        c_border = _hex(QPalette.ColorRole.Mid)
        c_th_bg  = _hex(QPalette.ColorRole.Button)
        c_th_txt = _hex(QPalette.ColorRole.ButtonText)

        # Status colours: try app_settings first, fall back to palette-relative
        _tc = {}
        if hasattr(self.main_window, 'app_settings'):
            _theme = self.main_window.app_settings.current_settings.get('theme', 'IMG_Factory')
            _tc = self.main_window.app_settings.get_theme_colors(_theme) or {}
        # If the theme dict has explicit success/error/warning use them;
        # otherwise derive safe semantic colours from the highlight palette colour
        # (these are the only three values that can't be cleanly inferred from QPalette).
        c_ok   = _tc.get('success', '')  or _hex(QPalette.ColorRole.Link)
        c_err  = _tc.get('error',   '')  or _hex(QPalette.ColorRole.LinkVisited)
        c_warn = _tc.get('warning', '')  or _hex(QPalette.ColorRole.Highlight)

        css = (
            "<style>"
            f"body{{font-family:monospace;font-size:11px;background:{c_bg};color:{c_text};}}"
            f"h3{{color:{c_accent};margin-bottom:4px;}}"
            f"h4{{color:{c_accent};margin:6px 0 2px 0;}}"
            "table{border-collapse:collapse;width:100%;}"
            f"th{{background:{c_th_bg};color:{c_th_txt};padding:3px 6px;text-align:left;border-bottom:1px solid {c_border};}}"
            f"td{{padding:2px 6px;border-bottom:1px solid {c_border};color:{c_text};background:{c_bg};}}"
            f"tr:nth-child(even) td{{background:{c_bg2};}}"
            f".note{{color:{c_text2};font-style:italic;}}"
            f".ok{{color:{c_ok};font-weight:bold;}}"
            f".no{{color:{c_err};}}"
            f".wip{{color:{c_warn};}}"
            "</style>"
        )

        # ── IMG Archive ───────────────────────────────────────────────────────
        img_html = css + """
<h3>IMG Archive Formats</h3>
<h4>Version 1 — GTA III / VC PC (.img + .dir pair)</h4>
<p>The .dir is a flat list of 32-byte entry records. The .img is raw concatenated data aligned to 2048-byte sectors.</p>
<table><tr><th>Offset</th><th>Size</th><th>Field</th></tr>
<tr><td>0</td><td>4</td><td>Sector offset (uint32 LE) × 2048 = byte offset in .img</td></tr>
<tr><td>4</td><td>4</td><td>Sector size (uint32 LE) × 2048 = byte size of entry</td></tr>
<tr><td>8</td><td>24</td><td>Filename — null-padded; Xbox builds have garbage bytes after the null terminator</td></tr></table>

<h4>Version 1.5 — Extended V1 (large archives)</h4>
<p>Same record layout as V1 but detected by IMG file >2GB or name field with no null terminator. Extended addressing for up to 4GB and long filenames.</p>

<h4>Version 2 — GTA SA PC + iOS/Android ports (self-contained .img)</h4>
<p>No separate .dir — the directory is embedded in the .img file itself. Used by GTA SA PC and all War Drum Studios iOS/Android ports.</p>
<table><tr><th>Offset</th><th>Size</th><th>Field</th></tr>
<tr><td>0</td><td>4</td><td>Magic: "VER2" = 0x32524556 (LE)</td></tr>
<tr><td>4</td><td>4</td><td>Entry count (uint32 LE)</td></tr>
<tr><td>8</td><td>32×N</td><td>Entry records (same 32-byte layout as V1)</td></tr></table>

<h4>Version 3 — GTA IV (AES-256 ECB encrypted)</h4>
<p>Header begins with a 16-byte AES block. Magic visible only after decryption. Not fully supported in IMG Factory 1.6 (read-only / partial).</p>

<h4>Sector Addressing</h4>
<table><tr><th>Rule</th><th>Value</th></tr>
<tr><td>Sector size</td><td>2048 bytes (0x800) — all PC, iOS, Android builds</td></tr>
<tr><td>PS2 / PSP sector size</td><td>512 bytes (0x200)</td></tr>
<tr><td>iOS / Android GTA3/VC (.dir+.img)</td><td>512 bytes (0x200) — detected automatically as V1_MOBILE</td></tr>
<tr><td>Byte offset</td><td>sector_offset × sector_size</td></tr>
<tr><td>Alignment on rebuild</td><td>Entries padded to next sector boundary</td></tr></table>

<h4>iOS / Android Releases (War Drum Studios ports)</h4>
<table><tr><th>Game</th><th>iOS</th><th>Android</th><th>IMG Format</th><th>IMG Factory</th></tr>
<tr><td>GTA III</td><td class="ok">Dec 2011</td><td class="ok">Dec 2011</td><td>.dir+.img pair, <b>512-byte sectors</b> (V1_MOBILE)</td><td class="ok">Loads OK</td></tr>
<tr><td>GTA Vice City</td><td class="ok">Dec 2012</td><td class="ok">Dec 2012</td><td>.dir+.img pair, <b>512-byte sectors</b> (V1_MOBILE)</td><td class="ok">Loads OK</td></tr>
<tr><td>GTA San Andreas</td><td class="ok">Dec 2013</td><td class="ok">Dec 2013</td><td>VER2, 2048-byte sectors</td><td class="ok">Loads OK</td></tr>
<tr><td>GTA Liberty City Stories</td><td class="ok">Jun 2015</td><td class="ok">Jun 2015</td><td>VER2 — internal format under investigation</td><td class="wip">Fails — investigating</td></tr>
<tr><td>GTA Vice City Stories</td><td class="no">Never released</td><td class="no">Never released</td><td>PSP/PS2 only</td><td class="no">N/A</td></tr></table>
<p class="note">GTA VC iOS confirmed loads OK (VER2 + D3D8 TXD, same as PC). LCS iOS failure likely relates to PSP-derived MDL asset format (RW 0x35001) or non-standard sector layout inherited from PSP version — needs a file sample to confirm.</p>

<h4>Xbox V1 Name Corruption</h4>
<p>Xbox DIR entries use a 24-byte name field. After the null terminator, remaining bytes contain arbitrary non-null garbage (e.g. 0x77 0x78 = 'wx' appended to ".txd"). IMG Factory uses a known-extension whitelist to truncate at the real extension boundary.</p>
<p class="note"><b>Known extensions:</b> dff txd col ifp ipl dat wav ide zon ped grp cut cnf img dir scm mp3 ogg fxp bmp png jpg spl rrr rdb rsc</p>

<h4>Special Entry Display Rules</h4>
<table><tr><th>Condition</th><th>RW Version column</th><th>RW Address column</th></tr>
<tr><td>Entry size == 0 and type DFF/TXD</td><td><b>Empty</b></td><td><b>Empty</b></td></tr>
<tr><td>Valid RW version read</td><td>Version string</td><td>Hex offset</td></tr>
<tr><td>No valid RW version</td><td>Unknown / file-type label</td><td>Hex offset</td></tr></table>
"""

        # ── RW Versions ───────────────────────────────────────────────────────
        ver_html = css + """
<h3>RenderWare Section Header</h3>
<p>Every RW chunk starts with a 12-byte header — all fields little-endian uint32:</p>
<table><tr><th>Offset</th><th>Field</th></tr>
<tr><td>0</td><td>Section type ID</td></tr>
<tr><td>4</td><td>Section data size (bytes following this header)</td></tr>
<tr><td>8</td><td>RW version</td></tr></table>

<h4>Version Encoding — Format 1: Plain Integer (GTA III PC, pre-retail)</h4>
<p>Raw 3-nibble integer <b>0xMmP</b>. Range 0x300–0x3FF accepted by IMG Factory.</p>
<table><tr><th>Value</th><th>Version</th><th>Confirmed file</th></tr>
<tr><td>0x00000300</td><td>3.0.0</td><td>GTA III PC earliest builds</td></tr>
<tr><td>0x00000304</td><td>3.0.4</td><td>GTAElift.DFF</td></tr>
<tr><td>0x00000310</td><td>3.1.0</td><td>GTA III PC retail DFF/TXD (most files)</td></tr></table>

<h4>Version Encoding — Format 2: Old Compact (SDK 3.0–3.7)</h4>
<p><b>0xMm0P</b> (5 hex digits). Used by SDK-linked tools and exports.</p>
<table><tr><th>Value</th><th>Version</th><th>Game</th></tr>
<tr><td>0x30000</td><td>3.0.0.0</td><td></td></tr>
<tr><td>0x31001</td><td>3.1.0.1</td><td>GTA III (canonical SDK)</td></tr>
<tr><td>0x33002</td><td>3.3.0.2</td><td>GTA VC (canonical SDK)</td></tr>
<tr><td>0x34001</td><td>3.4.0.1</td><td>Manhunt / GTA SOL</td></tr>
<tr><td>0x34003</td><td>3.4.0.3</td><td>GTA VC late</td></tr>
<tr><td>0x36003</td><td>3.6.0.3</td><td>GTA SA / Bully</td></tr>
<tr><td>0x37002</td><td>3.7.0.2</td><td>SA Mobile / PSP</td></tr></table>

<h4>Version Encoding — Format 3: Packed Platform Form</h4>
<p>Discriminator: <b>(v &amp; 0xFFFF) == 0xFFFF</b> and high word 0x0400–0x1C03.</p>
<table><tr><th>Value</th><th>Version</th><th>Platform</th></tr>
<tr><td>0x0401FFFF</td><td>2.0.0.1</td><td>GTA III early TXD (pre-retail)</td></tr>
<tr><td>0x0800FFFF</td><td>3.0.0.0</td><td>GTA III PS2</td></tr>
<tr><td>0x0C00FFFF</td><td>3.1.0.0</td><td>GTA III / VC PC</td></tr>
<tr><td>0x0C02FFFF</td><td>3.1.0.2</td><td>GTA III PC / GTA VC PS2</td></tr>
<tr><td>0x1000FFFF</td><td>3.2.0.0</td><td>GTA III PC</td></tr>
<tr><td>0x1003FFFF</td><td>3.2.0.3</td><td>GTA III PC TXD</td></tr>
<tr><td>0x1402FFFF</td><td>3.3.0.2</td><td>GTA III / VC PS2</td></tr>
<tr><td>0x1403FFFF</td><td>3.4.0.3</td><td>GTA VC late PC</td></tr>
<tr><td>0x1400FFFF</td><td>3.4.0.0</td><td><b>GTA III / VC Xbox</b></td></tr>
<tr><td>0x1803FFFF</td><td>3.6.0.3</td><td>GTA SA PC</td></tr>
<tr><td>0x1C020037</td><td>3.7.0.2</td><td>SA Mobile / PSP (special case)</td></tr></table>
<p class="note">Xbox RW headers may have a 4-byte prefix before the standard 12-byte header. IMG Factory scans offsets 0, 4, and 8 for valid version values to handle this.</p>

<h4>Core Section Type IDs</h4>
<table><tr><th>ID</th><th>Name</th><th>Used in</th></tr>
<tr><td>0x0001</td><td>Struct</td><td>All — raw binary payload block</td></tr>
<tr><td>0x0002</td><td>String</td><td>Frame names, material names</td></tr>
<tr><td>0x0003</td><td>Extension</td><td>Plugin data container (Hanim, skin, etc.)</td></tr>
<tr><td>0x0006</td><td>Texture</td><td>DFF — material texture reference</td></tr>
<tr><td>0x0007</td><td>Material</td><td>DFF — colour, texture, flags</td></tr>
<tr><td>0x0008</td><td>Material List</td><td>DFF — container for all materials</td></tr>
<tr><td>0x000E</td><td>Atomic</td><td>DFF — render unit linking geometry to frame</td></tr>
<tr><td>0x0014</td><td>Frame List</td><td>DFF — bone/frame hierarchy</td></tr>
<tr><td>0x0015</td><td>Geometry</td><td>DFF — mesh (vertices, UVs, vertex colours)</td></tr>
<tr><td>0x0016</td><td>Texture Dictionary</td><td>TXD — root container</td></tr>
<tr><td>0x0018</td><td>Texture Native</td><td>TXD — one texture entry</td></tr>
<tr><td>0x001A</td><td>Clump</td><td>DFF — root container</td></tr>
<tr><td>0x0253F2F7</td><td>Hanim PLG</td><td>Extension: bone animation</td></tr>
<tr><td>0x0253F2FB</td><td>Skin PLG</td><td>Extension: skinning weights</td></tr>
<tr><td>0x0253F2FE</td><td>Delta Morph PLG</td><td>Extension: morph targets</td></tr></table>
"""

        # ── TXD Format ────────────────────────────────────────────────────────
        txd_html = css + """
<h3>TXD — Texture Dictionary</h3>
<h4>Container Structure</h4>
<table><tr><th>Section</th><th>Contents</th></tr>
<tr><td>0x0016 Texture Dictionary</td><td>Root</td></tr>
<tr><td>&nbsp;&nbsp;└ 0x0001 Struct</td><td>GTA III/VC: tex_count uint32 &nbsp;|&nbsp; SA: tex_count uint16 + device_id uint16</td></tr>
<tr><td>&nbsp;&nbsp;└ 0x0018 Texture Native ×N</td><td>One per texture</td></tr>
<tr><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;└ 0x0001 Struct</td><td>All texture binary data (see struct below)</td></tr>
<tr><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;└ 0x0003 Extension</td><td>Usually empty in GTA files</td></tr></table>

<h4>Texture Native Struct Fields</h4>
<table><tr><th>Field</th><th>Size</th><th>GTA III/VC D3D8</th><th>GTA SA D3D9</th><th>Xbox (id=5)</th></tr>
<tr><td>platform_id</td><td>4</td><td>8</td><td>9</td><td>5</td></tr>
<tr><td>filter_flags</td><td>4</td><td>uint32</td><td>uint32</td><td>uint32</td></tr>
<tr><td>name</td><td>32</td><td>texture name</td><td>texture name</td><td>texture name</td></tr>
<tr><td>mask_name</td><td>32</td><td>alpha mask name</td><td>alpha mask name</td><td>alpha mask name</td></tr>
<tr><td>raster_format</td><td>4</td><td>format flags</td><td>format flags</td><td>format flags</td></tr>
<tr><td>d3d_format / FourCC</td><td>4</td><td>DXT FourCC</td><td>D3DFORMAT enum</td><td>Xbox-specific</td></tr>
<tr><td>width / height</td><td>2+2</td><td>uint16 each</td><td>uint16 each</td><td>uint16 each</td></tr>
<tr><td>depth</td><td>1</td><td>bits per pixel</td><td>bits per pixel</td><td>bits per pixel</td></tr>
<tr><td>mipmap_count</td><td>1</td><td>uint8</td><td>uint8</td><td>uint8</td></tr>
<tr><td>raster_type</td><td>1</td><td>uint8</td><td>uint8</td><td>uint8</td></tr>
<tr><td>compression</td><td>1</td><td>0=none / 1/3/5=DXT</td><td>uint8</td><td>Xbox compression byte</td></tr>
<tr><td>has_alpha</td><td>1</td><td>uint8</td><td>uint8</td><td>uint8</td></tr>
<tr><td>data_size</td><td>4</td><td>DXT formats only (per mip)</td><td>every mip level</td><td>ONE total for all mips</td></tr></table>

<h4>Pixel Format Detection Priority (IMG Factory)</h4>
<ol>
<li>PAL8 / PAL4 flags in raster_format</li>
<li>Xbox (platform_id=5) — compression byte map (see Platforms tab)</li>
<li>D3D FourCC: 0x31545844=DXT1 &nbsp; 0x33545844=DXT3 &nbsp; 0x35545844=DXT5</li>
<li>GTA III/VC non-Xbox: platform_prop 1=DXT1, 3=DXT3, 5=DXT5</li>
<li>SA D3D9 format map: 21=ARGB8888, 22=XRGB8888, 23=RGB565, 25=ARGB1555, 26=ARGB4444, 50=LUM8</li>
<li>GTA III/VC raster map: 0x0100=ARGB1555, 0x0200=RGB565, 0x0300=ARGB4444, 0x0500=ARGB8888, 0x0600=RGB888</li>
</ol>

<h4>Xbox Compression Byte Map (platform_id = 5)</h4>
<table><tr><th>Byte</th><th>Format</th><th>Notes</th></tr>
<tr><td>0x00</td><td>Raw ARGB8888</td><td>Uncompressed 32-bit</td></tr>
<tr><td>0x0B</td><td>LIN_DXT1</td><td>Linear DXT1</td></tr>
<tr><td>0x0C</td><td>DXT1 swizzled</td><td>Morton-order tiled</td></tr>
<tr><td>0x0E</td><td>DXT3 swizzled</td><td>Morton-order tiled</td></tr>
<tr><td>0x0F</td><td>LIN_DXT3</td><td>Linear DXT3 — e.g. fonts.txd</td></tr>
<tr><td>0x10</td><td>DXT5 swizzled</td><td>Morton-order tiled</td></tr>
<tr><td>0x11</td><td>LIN_DXT5</td><td>Linear DXT5</td></tr></table>
<p class="note">All Xbox variants decoded via PIL DDS header injection — no manual Morton unswizzle required.</p>

<h4>Platform IDs</h4>
<table><tr><th>ID</th><th>Platform</th></tr>
<tr><td>5</td><td>Xbox (GTA III / VC Xbox)</td></tr>
<tr><td>8</td><td>D3D8 — GTA III / VC PC</td></tr>
<tr><td>9</td><td>D3D9 — GTA SA PC</td></tr>
<tr><td>6</td><td>PlayStation 2 (VRAM swizzle — not supported)</td></tr></table>
"""

        # ── COL Format ────────────────────────────────────────────────────────
        col_html = css + """
<h3>COL — Collision Format</h3>

<h4>File Structure — Multiple Models Concatenated</h4>
<p>A .col file is a sequence of model blocks, each starting with an 8-byte header:</p>
<table><tr><th>Offset</th><th>Size</th><th>Field</th></tr>
<tr><td>0</td><td>4</td><td>FourCC / version magic (see below)</td></tr>
<tr><td>4</td><td>4</td><td>Model data size in bytes (uint32 LE) — NOT including this 8-byte header</td></tr>
<tr><td>8</td><td>N</td><td>Model data (name, bounding box, counts, primitives)</td></tr></table>

<h4>Version Magic Bytes</h4>
<table><tr><th>Bytes</th><th>Version</th><th>Game</th></tr>
<tr><td>COLL (0x434F4C4C)</td><td>COL1</td><td>GTA III</td></tr>
<tr><td>COL\x02</td><td>COL2</td><td>GTA Vice City</td></tr>
<tr><td>COL\x03</td><td>COL3</td><td>GTA San Andreas</td></tr>
<tr><td>COL\x04</td><td>COL4</td><td>Extended (rare)</td></tr></table>

<h4>Model Data Layout — COL1 (GTA III)</h4>
<table><tr><th>Field</th><th>Size</th><th>Notes</th></tr>
<tr><td>Model name</td><td>22 bytes</td><td>ASCII, null-padded</td></tr>
<tr><td>Model ID</td><td>2</td><td>uint16 LE</td></tr>
<tr><td>Bounding radius</td><td>4</td><td>float32</td></tr>
<tr><td>Bounding center</td><td>12</td><td>3× float32 (x,y,z)</td></tr>
<tr><td>Bounding min</td><td>12</td><td>3× float32</td></tr>
<tr><td>Bounding max</td><td>12</td><td>3× float32</td></tr>
<tr><td>num_spheres</td><td>4</td><td>uint32 LE</td></tr>
<tr><td>num_boxes</td><td>4</td><td>uint32 LE</td></tr>
<tr><td>num_vertices</td><td>4</td><td>uint32 LE</td></tr>
<tr><td>num_faces</td><td>4</td><td>uint32 LE — may be garbage, recalculated from file size</td></tr>
<tr><td>Sphere data</td><td>24 × N</td><td>center(12) + radius(4) + material(4) + flags(4)</td></tr>
<tr><td>Box data</td><td>32 × N</td><td>min(12) + max(12) + material(4) + flags(4)</td></tr>
<tr><td>Vertex data</td><td>12 × N</td><td>position (3× float32)</td></tr>
<tr><td>Face data</td><td>14 × N</td><td>indices(6: 3×uint16) + mat_id(2) + light(2) + flags(4)</td></tr></table>

<h4>Model Data Layout — COL2/COL3 (VC / SA)</h4>
<table><tr><th>Field</th><th>Size</th><th>Difference from COL1</th></tr>
<tr><td>Model name</td><td>22 bytes</td><td>Same</td></tr>
<tr><td>Model ID</td><td>2</td><td>Same</td></tr>
<tr><td>Bounding min/max/center/radius</td><td>40</td><td>Order: min(12), max(12), center(12), radius(4)</td></tr>
<tr><td>Counts order</td><td>16</td><td><b>spheres, boxes, faces, vertices</b> (faces and vertices swapped vs COL1)</td></tr>
<tr><td>Sphere data</td><td>20 × N</td><td>center(12) + radius(4) + material(4) — no flags field</td></tr>
<tr><td>Box data</td><td>28 × N</td><td>min(12) + max(12) + material(4) — no flags field</td></tr>
<tr><td>Face data</td><td>12 × N</td><td>indices(6) + mat_id(2) + light(2) + <b>padding(2)</b> instead of flags(4)</td></tr></table>

<h4>Garbage Face Count Fix</h4>
<p>COL1 files sometimes store a corrupt face count (e.g. 3,226,344,957 instead of 46). IMG Factory detects values exceeding 1,000,000 or 10× the calculated count and recalculates from remaining bytes:</p>
<p class="note">calculated_faces = (remaining_bytes_after_vertices) ÷ face_size</p>
"""

        # ── Status ────────────────────────────────────────────────────────────
        status_html = css + """
<h3>IMG Factory 1.6 — Implementation Status</h3>

<h4>IMG Archive</h4>
<table><tr><th>Format</th><th>Read</th><th>Write</th></tr>
<tr><td>Version 1 — GTA III / VC PC</td><td class="ok">✓</td><td class="ok">✓</td></tr>
<tr><td>Version 1 — GTA III / VC Xbox</td><td class="ok">✓</td><td class="ok">✓</td></tr>
<tr><td>Version 1.5 — Extended</td><td class="ok">✓</td><td class="wip">~</td></tr>
<tr><td>Version 2 — GTA SA PC</td><td class="ok">✓</td><td class="ok">✓</td></tr>
<tr><td>Version 1 Mobile — GTA III iOS / Android (512-byte sectors)</td><td class="ok">✓</td><td class="no">✗</td></tr>
<tr><td>Version 1 Mobile — GTA VC iOS / Android (512-byte sectors)</td><td class="ok">✓</td><td class="no">✗</td></tr>
<tr><td>Version 2 — GTA SA iOS / Android</td><td class="ok">✓</td><td class="ok">✓</td></tr>
<tr><td>Version 2 — GTA LCS iOS / Android</td><td class="wip">~</td><td class="no">✗</td></tr>
<tr><td>Version 3 — GTA IV encrypted</td><td class="wip">~</td><td class="no">✗</td></tr></table>
<p class="note">GTA VCS was never released on iOS or Android (PSP/PS2 only).</p>

<h4>TXD Texture Formats</h4>
<table><tr><th>Format</th><th>Read</th><th>Write</th><th>Notes</th></tr>
<tr><td>GTA III/VC  DXT1/3/5</td><td class="ok">✓</td><td class="ok">✓</td><td>D3D8 FourCC</td></tr>
<tr><td>GTA III/VC  PAL8</td><td class="ok">✓</td><td class="ok">✓</td><td>256-colour palette</td></tr>
<tr><td>GTA III/VC  ARGB1555 / RGB565 / ARGB4444</td><td class="ok">✓</td><td class="ok">✓</td><td>16-bit formats</td></tr>
<tr><td>GTA III/VC  ARGB8888 / RGB888</td><td class="ok">✓</td><td class="ok">✓</td><td>32-bit uncompressed</td></tr>
<tr><td>GTA SA      DXT1/3/5</td><td class="ok">✓</td><td class="ok">✓</td><td>D3D9 per-mip data_size</td></tr>
<tr><td>GTA SA      ARGB8888 / XRGB8888</td><td class="ok">✓</td><td class="ok">✓</td><td>d3d_fmt 21/22</td></tr>
<tr><td>Xbox        LIN_DXT1/3/5 + swizzled variants</td><td class="ok">✓</td><td class="wip">~</td><td>compression bytes 0x0B–0x11</td></tr>
<tr><td>Xbox        Raw ARGB8888</td><td class="ok">✓</td><td class="wip">~</td><td>compression byte 0x00</td></tr>
<tr><td>PS2         VRAM swizzle</td><td class="no">✗</td><td class="no">✗</td><td>Not targeted</td></tr></table>

<h4>RW Version Detection</h4>
<table><tr><th>Range</th><th>Status</th><th>Notes</th></tr>
<tr><td>0x300–0x3FF  plain integer</td><td class="ok">✓</td><td>GTA III PC pre-packed (0x304=GTAElift, 0x310=retail)</td></tr>
<tr><td>0x0401FFFF  early packed</td><td class="ok">✓</td><td>GTA III pre-retail TXD</td></tr>
<tr><td>0x0800FFFF–0x1C03FFFF  packed</td><td class="ok">✓</td><td>All standard packed variants incl. Xbox 0x1400FFFF</td></tr>
<tr><td>0x30000–0x3FFFF  compact</td><td class="ok">✓</td><td>SDK compact format</td></tr>
<tr><td>0x1C020037  SA Mobile</td><td class="ok">✓</td><td>Special case</td></tr>
<tr><td>Xbox header offset scan</td><td class="ok">✓</td><td>Scans offsets 0/4/8 for Xbox-prefixed layouts</td></tr>
<tr><td>0-byte entry guard</td><td class="ok">✓</td><td>Shows "Empty" instead of attempting read</td></tr></table>

<h4>COL Collision</h4>
<table><tr><th>Version</th><th>Read</th><th>Write</th><th>Notes</th></tr>
<tr><td>COL1 — GTA III</td><td class="ok">✓</td><td class="ok">✓</td><td>Garbage face count auto-corrected</td></tr>
<tr><td>COL2 — GTA VC</td><td class="ok">✓</td><td class="ok">✓</td><td></td></tr>
<tr><td>COL3 — GTA SA</td><td class="ok">✓</td><td class="ok">✓</td><td></td></tr>
<tr><td>COL4 — Extended</td><td class="wip">~</td><td class="wip">~</td><td>Rare — partial support</td></tr></table>
<p class="note">~ = partial / read-only / in progress</p>
"""

        tabs.addTab(make_text(img_html),    "IMG Archive")
        tabs.addTab(make_text(ver_html),    "RW Versions & Sections")
        tabs.addTab(make_text(txd_html),    "TXD Format")
        tabs.addTab(make_text(col_html),    "COL Format")
        tabs.addTab(make_text(status_html), "Status")

        close_btn = QPushButton("Close")
        close_btn.setMaximumWidth(100)
        close_btn.clicked.connect(dialog.accept)
        btn_row = QHBoxLayout()
        btn_row.addStretch()
        btn_row.addWidget(close_btn)
        layout.addLayout(btn_row)

        dialog.exec()


    def _show_imgfactory_info(self): #vers 1
        """Show IMG Factory application information dialog"""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton, QTextEdit
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        try:
            from apps.components.Img_Factory.imgfactory import App_name, App_auth, App_build
        except:
            App_name = "IMG Factory"
            App_auth = "X-Seti"
            App_build = "1.6"

        dialog = QDialog(self.main_window)
        dialog.setWindowTitle("About IMG Factory")
        dialog.setMinimumWidth(500)
        dialog.setMinimumHeight(600)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(15)
        layout.setContentsMargins(20, 20, 20, 20)

        # Title
        title = QLabel(f"{App_name} {App_build}")
        title_font = QFont("Arial", 18, QFont.Weight.Bold)
        title.setFont(title_font)
        title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(title)

        # Author
        author = QLabel(f"by {App_auth}")
        author_font = QFont("Arial", 12)
        author.setFont(author_font)
        author.setAlignment(Qt.AlignmentFlag.AlignCenter)
        author.setStyleSheet("color: palette(placeholder-text);")
        layout.addWidget(author)

        # Description
        description = QTextEdit()
        description.setReadOnly(True)
        description.setMaximumHeight(200)
        description.setHtml("""
            <h3>Professional IMG Archive Manager</h3>
            <p><b>IMG Factory</b> is a comprehensive tool for managing Grand Theft Auto game archives.</p>

            <p><b>Features:</b></p>
            <ul>
                <li>IMG, COL, TXD, DFF file support</li>
                <li>Import, export, and batch operations</li>
                <li>Built-in editors for collision and texture files</li>
                <li>IDE integration for proper file ordering</li>
                <li>Template system for quick project setup</li>
                <li>Undo/Redo system</li>
                <li>Custom UI modes</li>
            </ul>

            <p><b>Based on:</b> Original IMG Factory by MexUK (2007)</p>
            <p><b>Python Edition:</b> Complete rewrite in Python/PyQt6</p>
        """)
        layout.addWidget(description)

        # Version info
        version_info = QLabel(f"Version: {App_build} | Python Edition")
        version_info.setAlignment(Qt.AlignmentFlag.AlignCenter)
        version_info.setStyleSheet("color: palette(placeholder-text); font-size: 10px;")
        layout.addWidget(version_info)

        layout.addStretch()

        # Close button
        close_btn = QPushButton("Close")
        close_btn.setFixedWidth(100)
        close_btn.clicked.connect(dialog.accept)

        btn_layout = QHBoxLayout()
        btn_layout.addStretch()
        btn_layout.addWidget(close_btn)
        btn_layout.addStretch()

        layout.addLayout(btn_layout)

        dialog.exec()

    def show_search_dialog(self):  # vers 1
        """Show the search dialog"""
        try:
            # Create and show the search dialog
            search_dialog = ASearchDialog(self.main_window)
            search_dialog.exec()
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Search dialog error: {str(e)}")


    def find_next(self):  # vers 1
        """Find next occurrence of search term"""
        try:
            if hasattr(self.main_window, 'search_manager'):
                self.main_window.search_manager.find_next()
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Find Next: Search manager not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Find Next error: {str(e)}")


    def find_previous(self):  # vers 1
        """Find previous occurrence of search term"""
        try:
            if hasattr(self.main_window, 'search_manager'):
                self.main_window.search_manager.find_previous()
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Find Previous: Search manager not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Find Previous error: {str(e)}")


    def replace_dialog(self):  # vers 1
        """Show replace dialog"""
        try:
            if hasattr(self.main_window, 'search_manager'):
                # Create and show the replace dialog
                from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QLineEdit, QPushButton, QCheckBox
                dialog = QDialog(self.main_window)
                dialog.setWindowTitle("Replace Text")
                dialog.setModal(True)

                layout = QVBoxLayout()

                # Search text
                search_layout = QHBoxLayout()
                search_layout.addWidget(QLabel("Find:"))
                search_input = QLineEdit()
                search_layout.addWidget(search_input)
                layout.addLayout(search_layout)

                # Replace text
                replace_layout = QHBoxLayout()
                replace_layout.addWidget(QLabel("Replace with:"))
                replace_input = QLineEdit()
                replace_layout.addWidget(replace_input)
                layout.addLayout(replace_layout)

                # Options
                case_sensitive_check = QCheckBox("Case sensitive")
                layout.addWidget(case_sensitive_check)

                regex_check = QCheckBox("Regular expression")
                layout.addWidget(regex_check)

                # Buttons
                button_layout = QHBoxLayout()
                replace_btn = QPushButton("Replace")
                replace_all_btn = QPushButton("Replace All")
                cancel_btn = QPushButton("Cancel")

                def on_replace():
                    search_text = search_input.text().strip()
                    replace_text = replace_input.text()
                    if not search_text:
                        return

                    options = {
                        'case_sensitive': case_sensitive_check.isChecked(),
                        'regex': regex_check.isChecked(),
                        'search_mode': 'replace_one',
                        'replace_text': replace_text
                    }

                    # Perform search first to find matches
                    self.main_window.search_manager.perform_search(search_text, options)

                def on_replace_all():
                    search_text = search_input.text().strip()
                    replace_text = replace_input.text()
                    if not search_text:
                        return

                    options = {
                        'case_sensitive': case_sensitive_check.isChecked(),
                        'regex': regex_check.isChecked(),
                        'search_mode': 'replace_all',
                        'replace_text': replace_text
                    }

                    # Perform replace all
                    self.main_window.search_manager.perform_search(search_text, options)
                    dialog.accept()

                replace_btn.clicked.connect(on_replace)
                replace_all_btn.clicked.connect(on_replace_all)
                cancel_btn.clicked.connect(dialog.reject)

                button_layout.addWidget(replace_btn)
                button_layout.addWidget(replace_all_btn)
                button_layout.addStretch()
                button_layout.addWidget(cancel_btn)

                layout.addLayout(button_layout)
                dialog.setLayout(layout)
                dialog.exec()
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Replace Dialog: Search manager not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Replace Dialog error: {str(e)}")


    def advanced_search(self):  # vers 1
        """Show advanced search options"""
        try:
            if hasattr(self.main_window, 'search_manager'):
                self.main_window.search_manager.show_search_dialog()
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Advanced Search: Search manager not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Advanced Search error: {str(e)}")


    def clear_search(self):  # vers 1
        """Clear current search"""
        try:
            if hasattr(self.main_window, 'search_manager'):
                self.main_window.search_manager._clear_search()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Search cleared")
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Clear Search: Search manager not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Clear Search error: {str(e)}")


    def move_entries_up(self):  # vers 3
        """Move selected entries up - updates file_object.entries and pushes undo."""
        try:
            from apps.methods.export_shared import get_active_table
            from apps.methods.tab_system import get_current_file_from_active_tab
            from apps.methods.img_entry_operations import move_entries_in_file
            self.table = get_active_table(self.main_window) or self.table
            if not self.table or not self.table.selectedItems():
                self.main_window.log_message("No entries selected to move")
                return
            rows = sorted(set(i.row() for i in self.table.selectedItems()))
            file_object, _ = get_current_file_from_active_tab(self.main_window)
            if not file_object:
                return
            if move_entries_in_file(self.main_window, file_object, rows, -1):
                from apps.core.undo_system import refresh_after_undo
                refresh_after_undo(self.main_window)
                # Re-select moved rows
                new_start = min(rows) - 1
                self.table.clearSelection()
                for i in range(len(rows)):
                    for col in range(self.table.columnCount()):
                        item = self.table.item(new_start + i, col)
                        if item:
                            item.setSelected(True)
                self.main_window.log_message(f"{len(rows)} entries moved up")
        except Exception as e:
            self.main_window.log_message(f"Move up error: {str(e)}")

    def move_entries_down(self):  # vers 3
        """Move selected entries down - updates file_object.entries and pushes undo."""
        try:
            from apps.methods.export_shared import get_active_table
            from apps.methods.tab_system import get_current_file_from_active_tab
            from apps.methods.img_entry_operations import move_entries_in_file
            self.table = get_active_table(self.main_window) or self.table
            if not self.table or not self.table.selectedItems():
                self.main_window.log_message("No entries selected to move")
                return
            rows = sorted(set(i.row() for i in self.table.selectedItems()))
            file_object, _ = get_current_file_from_active_tab(self.main_window)
            if not file_object:
                return
            if move_entries_in_file(self.main_window, file_object, rows, 1):
                from apps.core.undo_system import refresh_after_undo
                refresh_after_undo(self.main_window)
                new_start = min(rows) + 1
                self.table.clearSelection()
                for i in range(len(rows)):
                    for col in range(self.table.columnCount()):
                        item = self.table.item(new_start + i, col)
                        if item:
                            item.setSelected(True)
                self.main_window.log_message(f"{len(rows)} entries moved down")
        except Exception as e:
            self.main_window.log_message(f"Move down error: {str(e)}")

    def _debug_tabs(self): #vers 1
        """Debug tab structure - TEMPORARY"""
        msg = "=== TAB DEBUG ===\n"

        # Check main_tab_widget
        if hasattr(self.main_window, 'main_tab_widget'):
            mtw = self.main_window.main_tab_widget
            msg += f"main_tab_widget: {mtw}\n"
            msg += f"  Count: {mtw.count()}\n"
            for i in range(mtw.count()):
                msg += f"  Tab {i}: {mtw.tabText(i)}\n"
        else:
            msg += "main_tab_widget: NOT FOUND\n"

        msg += "\n"

        # Check gui_layout.tab_widget
        if hasattr(self.main_window, 'gui_layout'):
            if hasattr(self.main_window.gui_layout, 'tab_widget'):
                gtw = self.main_window.gui_layout.tab_widget
                msg += f"gui_layout.tab_widget: {gtw}\n"
                msg += f"  Count: {gtw.count()}\n"
                for i in range(gtw.count()):
                    msg += f"  Tab {i}: {gtw.tabText(i)}\n"
            else:
                msg += "gui_layout.tab_widget: NOT FOUND\n"
        else:
            msg += "gui_layout: NOT FOUND\n"

        print(msg)
        if hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(msg)


    def _launch_theme_settings(self): #vers 1
        """Launch theme settings dialog"""
        try:
            if hasattr(self.main_window, 'app_settings'):
                from apps.utils.app_settings_system import SettingsDialog
                dialog = SettingsDialog(self.main_window.app_settings, self.main_window)
                dialog.exec()
            else:
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.information(
                    self.main_window,
                    "Theme Settings",
                    "Theme settings not available"
                )
        except Exception as e:
            print(f"Error launching theme settings: {str(e)}")


    def _show_settings_context_menu(self, pos): #vers 1
        """Show context menu for settings button"""
        from PyQt6.QtWidgets import QMenu

        menu = QMenu(self.main_window)

        # Theme action
        theme_action = menu.addAction("Theme Settings")
        theme_action.triggered.connect(self._launch_theme_settings)

        # IMG Factory settings action
        imgfactory_action = menu.addAction("IMG Factory Settings")
        imgfactory_action.triggered.connect(self._show_workshop_settings)

        menu.exec(self.properties_btn.mapToGlobal(pos))


    def _show_ai_context_menu(self, pos): #vers 1
        """Right-click menu for AI Workshop button"""
        from PyQt6.QtWidgets import QMenu

        menu = QMenu(self.main_window)

        docked_action = menu.addAction("Open AI Workshop (Docked Tab)")
        docked_action.triggered.connect(lambda: self.main_window.open_ai_workshop_docked())

        standalone_action = menu.addAction("Open AI Workshop (Standalone Window)")
        standalone_action.triggered.connect(lambda: self.main_window.open_ai_workshop_standalone())

        menu.exec(self.ai_btn.mapToGlobal(pos))


        """Apply always on top window flag"""
        current_flags = self.windowFlags()

        if self.window_always_on_top:
            new_flags = current_flags | Qt.WindowType.WindowStaysOnTopHint
        else:
            new_flags = current_flags & ~Qt.WindowType.WindowStaysOnTopHint

        if new_flags != current_flags:
            # Save state
            current_geometry = self.geometry()
            was_visible = self.isVisible()

            self.setWindowFlags(new_flags)

            self.setGeometry(current_geometry)
            if was_visible:
                self.show()


    def _scan_available_locales(self): #vers 2
        """Scan locale folder and return list of available languages"""
        import os
        import configparser

        locales = []
        locale_path = os.path.join(os.path.dirname(__file__), 'locale')

        if not os.path.exists(locale_path):
            # Easter egg: Amiga Workbench 3.1 style error
            self._show_amiga_locale_error()
            # Return default English
            return [("English", "en", None)]

        try:
            for filename in os.listdir(locale_path):
                if filename.endswith('.lang'):
                    filepath = os.path.join(locale_path, filename)

                    try:
                        config = configparser.ConfigParser()
                        config.read(filepath, encoding='utf-8')

                        if 'Metadata' in config:
                            lang_name = config['Metadata'].get('LanguageName', 'Unknown')
                            lang_code = config['Metadata'].get('LanguageCode', 'unknown')
                            locales.append((lang_name, lang_code, filepath))

                    except Exception as e:
                        if self.main_window and hasattr(self.main_window, 'log_message'):
                            self.window_context.log_message(f"Failed to load locale {filename}: {e}")

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.window_context.log_message(f"Locale scan error: {e}")

        locales.sort(key=lambda x: x[0])

        if not locales:
            locales = [("English", "en", None)]

        return locales


    def _show_amiga_locale_error(self): #vers 2
        """Show Amiga Workbench 3.1 style error dialog"""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton, QHBoxLayout
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle("Workbench Request")
        dialog.setFixedSize(450, 150)

        # Amiga Workbench styling
        dialog.setStyleSheet("""
            QDialog {
                background-color: #aaaaaa;
                border: 2px solid #ffffff;
            }
            QLabel {
                color: #000000;
                background-color: #aaaaaa;
            }
            QPushButton {
                background-color: #8899aa;
                color: #000000;
                border: 2px outset #ffffff;
                padding: 5px 15px;
                min-width: 80px;
            }
            QPushButton:pressed {
                border: 2px inset #555555;
            }
        """)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(15)
        layout.setContentsMargins(20, 20, 20, 20)

        # Amiga Topaz font style
        amiga_font = QFont("Courier", 10, QFont.Weight.Normal)

        # Error message
        message = QLabel("Workbench 3.1 installer\n\nPlease insert Local disk in any drive")
        message.setFont(amiga_font)
        message.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(message)

        layout.addStretch()

        # Button layout
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        # Retry and Cancel buttons (Amiga style)
        retry_btn = QPushButton("Retry")
        retry_btn.setFont(amiga_font)
        retry_btn.clicked.connect(dialog.accept)
        button_layout.addWidget(retry_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.setFont(amiga_font)
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        button_layout.addStretch()
        layout.addLayout(button_layout)

        dialog.exec()


# - Docking functions

    def _update_dock_button_visibility(self): #vers 2
        """Show/hide dock and tearoff buttons based on docked state"""
        if hasattr(self, 'dock_btn'):
            # Hide D button when docked, show when standalone
            self.dock_btn.setVisible(not self.is_docked)

        if hasattr(self, 'tearoff_btn'):
            # T button only visible when docked and not in standalone mode
            self.tearoff_btn.setVisible(self.is_docked and not self.standalone_mode)


    def toggle_dock_mode(self): #vers 2
        """Toggle between docked and standalone mode"""
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

        self._update_dock_button_visibility()


    def _dock_to_main(self): #vers 9
        """Dock handled by overlay system in imgfactory - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                self.show()
                self.raise_()
                return

            # For proper docking, we need to be called from imgfactory
            # This method should be handled by imgfactory's overlay system
            if self.main_window and hasattr(self.main_window, App_name + '_docked'):
                # If available, use the main window's docking system
                self.main_window.open_col_workshop_docked()
            else:
                # Fallback: just show the window
                self.show()
                self.raise_()

            # Update dock state
            self.is_docked = True
            self._update_dock_button_visibility()

            if hasattr(self.main_window, 'log_message'):
                self.window_context.log_message(f"{App_name} docked to main window")


        except Exception as e:
            print(f"Error docking: {str(e)}")
            self.show()


    def _undock_from_main(self): #vers 4
        """Undock from overlay mode to standalone window - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                # Switch from overlay to normal window
                self.setWindowFlags(Qt.WindowType.Window)
                self.is_overlay = False
                self.overlay_table = None

            # Set proper window flags for standalone mode
            self.setWindowFlags(Qt.WindowType.Window)

            # Ensure proper size when undocking
            if hasattr(self, 'original_size'):
                self.resize(self.original_size)
            else:
                self.resize(1000, 700)  # Reasonable default size

            self.is_docked = False
            self._update_dock_button_visibility()

            self.show()
            self.raise_()

            if hasattr(self.main_window, 'log_message'):
                self.window_context.log_message(f"{App_name} undocked to standalone")

        except Exception as e:
            print(f"Error undocking: {str(e)}")
            # Fallback
            self.setWindowFlags(Qt.WindowType.Window)
            self.show()


    def _apply_button_mode(self, dialog): #vers 1
        """Apply button display mode"""
        mode_index = self.button_mode_combo.currentIndex()
        mode_map = {0: 'both', 1: 'icons', 2: 'text'}

        new_mode = mode_map[mode_index]

        if new_mode != self.button_display_mode:
            self.button_display_mode = new_mode
            self._update_all_buttons()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                mode_names = {0: 'Icons + Text', 1: 'Icons Only', 2: 'Text Only'}
                self.window_context.log_message(f"✨ Button style: {mode_names[mode_index]}")

        dialog.close()


# - Window functionality

    def _initialize_features(self): #vers 5
        """Initialize all features after UI setup - including enhanced logging"""
        try:
            self._apply_theme()
            self._update_status_indicators()
            
            # Connect various system events to the extended log
            self._connect_extended_logging()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.window_context.log_message("All features initialized")
                self.window_context.log_message("Extended logging system active")
                
                # Check if directory tree autoload is enabled
                autoload_tree = True
                if hasattr(self.main_window, 'app_settings'):
                    from apps.methods.img_factory_settings import IMGFactorySettings
                    settings = IMGFactorySettings()
                    autoload_tree = settings.get('autoload_directory_tree', True)
                
                if autoload_tree:
                    self.window_context.log_message("Directory tree autoload enabled - switching in 100ms")
                    # Automatically switch to directory tree view at startup
                    QTimer.singleShot(100, self._switch_to_directory_tree)
                else:
                    self.window_context.log_message("Directory tree autoload disabled in settings")

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.window_context.log_message(f"Feature init error: {str(e)}")
            import traceback
            traceback.print_exc()

    
    def _connect_extended_logging(self): #vers 1
        """Connect various system events to the extended log system"""
        try:
            # Connect search results to log
            if hasattr(self.main_window, 'search_manager') and self.main_window.search_manager:
                if hasattr(self.main_window.search_manager, 'search_results_signal'):
                    def log_search_results(results):
                        self.main_window.log_message(f"[SEARCH RESULT] Found {len(results)} items")
                        
                    self.main_window.search_manager.search_results_signal.connect(log_search_results)
            
            # Connect import/export events if available
            if hasattr(self.main_window, 'import_manager'):
                def log_import(event_data):
                    self.main_window.log_message(f"[IMPORT] {event_data}")
                
                def log_export(event_data):
                    self.main_window.log_message(f"[EXPORT] {event_data}")
                
                # Connect import/export signals if they exist
                if hasattr(self.main_window.import_manager, 'import_signal'):
                    self.main_window.import_manager.import_signal.connect(log_import)
                if hasattr(self.main_window, 'export_signal'):
                    self.main_window.export_signal.connect(log_export)
            
            # Connect file removal events
            def log_removal(file_path):
                self.main_window.log_message(f"[REMOVAL] Removed: {file_path}")
            
            # This would connect to actual removal signals when they exist in the system
            self.main_window.log_removal_handler = log_removal

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.window_context.log_message(f"Logging connection error: {str(e)}")


    def _is_on_draggable_area(self, pos): #vers 7
        """Check if position is on draggable titlebar area

        Args:
            pos: Position in titlebar coordinates (from eventFilter)

        Returns:
            True if position is on titlebar but not on any button
        """
        if not hasattr(self, 'titlebar'):
            print("[DRAG] No titlebar attribute")
            return False

        # Verify pos is within titlebar bounds
        if not self.titlebar.rect().contains(pos):
            print(f"[DRAG] Position {pos} outside titlebar rect {self.titlebar.rect()}")
            return False

        # Check if clicking on any button - if so, NOT draggable
        for widget in self.titlebar.findChildren(QPushButton):
            if widget.isVisible():
                # Get button geometry in titlebar coordinates
                button_rect = widget.geometry()
                if button_rect.contains(pos):
                    print(f"[DRAG] Clicked on button: {widget.toolTip()}")
                    return False

        # Not on any button = draggable
        print(f"[DRAG] On draggable area at {pos}")
        return True


# - From the fixed gui - move, drag

    def _update_all_buttons(self): #vers 4
        """Update all buttons to match display mode"""
        buttons_to_update = [
            # Toolbar buttons
            ('open_btn', 'Open'),
            ('save_btn', 'Save'),
            ('save_col_btn', 'Save TXD'),
        ]

        # Adjust transform panel width based on mode
        if hasattr(self, 'nav_icon_panel'):
            if self.button_display_mode == 'icons':
                self.nav_icon_panel.setMaximumWidth(50)
            else:
                self.nav_text_panel.setMaximumWidth(200)

        for btn_name, btn_text in buttons_to_update:
            if hasattr(self, btn_name):
                button = getattr(self, btn_name)
                self._apply_button_mode_to_button(button, btn_text)
        self._update_dock_button_visibility()


    def _toggle_maximize(self): #vers 1
        """Toggle window maximize state"""
        if self.main_window.isMaximized():
            self.main_window.showNormal()
        else:
            self.main_window.showMaximized()


    def refresh_icons(self, color: str): #vers 1
        """Refresh all toolbar SVG icons using the given color (text_primary from theme)"""
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            if hasattr(self, 'menu_btn'):
                self.menu_btn.setIcon(SVGIconFactory.menu_m_icon(20, color))
            if hasattr(self, 'settings_btn'):
                self.settings_btn.setIcon(SVGIconFactory.settings_icon(20, color))
            if hasattr(self, 'undo_btn'):
                self.undo_btn.setIcon(SVGIconFactory.undo_icon(20, color))
            if hasattr(self, 'info_btn'):
                self.info_btn.setIcon(SVGIconFactory.info_icon(20, color))
            if hasattr(self, 'properties_btn'):
                self.properties_btn.setIcon(SVGIconFactory.properties_icon(24, color))
            if hasattr(self, 'minimize_btn'):
                self.minimize_btn.setIcon(SVGIconFactory.minimize_icon(20, color))
            if hasattr(self, 'maximize_btn'):
                self.maximize_btn.setIcon(SVGIconFactory.maximize_icon(20, color))
            if hasattr(self, 'close_btn'):
                self.close_btn.setIcon(SVGIconFactory.close_icon(20, color))
            print(f"Custom toolbar icons refreshed with color: {color}")
        except Exception as e:
            print(f"refresh_icons failed: {e}")

    def closeEvent(self, event): #vers 1
        """Handle close event"""
        self.window_closed.emit()
        event.accept()



# Export functions
__all__ = [
    'IMGFactoryGUILayoutCustom', '_create_toolbar', '_show_workshop_settings'
]


# ─────────────────────────────────────────────────────────────────────────────
# Tool Taskbar
# ─────────────────────────────────────────────────────────────────────────────

class ToolTaskbar(QWidget):  # vers 1
    """Compact taskbar showing icons for every open tool/workshop.

    Sits between the titlebar and the file-list window.  Each button shows
    the tool's icon + a short label.  Clicking raises the tool's window (or
    re-opens it if it was closed).  Buttons appear when a tool is registered
    and disappear when it is closed.

    Usage
    -----
    bar = ToolTaskbar(main_window, parent_widget)
    bar.register("txd",   "TXD Workshop", txd_icon, txd_window_or_callable)
    bar.register("col",   "COL Workshop", col_icon, col_window_or_callable)
    bar.register("ai",    "AI Workshop",  ai_icon,  ai_window_or_callable)
    bar.unregister("txd")   # removes button
    """

    def __init__(self, main_window, parent=None):
        super().__init__(parent)
        self._main_window = main_window
        self._tools: dict = {}   # key -> {label, icon, target, btn}
        self.setFixedHeight(32)
        self.setObjectName("tool_taskbar")

        self._layout = QHBoxLayout(self)
        self._layout.setContentsMargins(4, 2, 4, 2)
        self._layout.setSpacing(3)

        # Separator line feel — small label on the far left
        self._sep_label = QLabel("Tools:")
        self._sep_label.setFixedWidth(38)
        self._sep_label.setStyleSheet(
            "color: palette(mid); font-size: 10px; font-style: italic;")
        self._layout.addWidget(self._sep_label)

        self._layout.addStretch()

        # Start hidden — shows itself as soon as the first tool registers
        self.setVisible(False)

    # ── Public API ────────────────────────────────────────────────────────────

    def register(self, key: str, label: str, icon,
                 target, tooltip: str = "") -> None:
        """Add or update a tool button.

        Parameters
        ----------
        key     : unique string identifier (e.g. 'txd', 'col', 'ai')
        label   : short display label (≤10 chars recommended)
        icon    : QIcon
        target  : QWidget to raise/show  OR  callable to open
        tooltip : optional full tooltip text
        """
        if key in self._tools:
            self.unregister(key)

        btn = QPushButton()
        btn.setIcon(icon)
        btn.setText(label)
        btn.setIconSize(QSize(18, 18))
        btn.setFixedHeight(26)
        btn.setMaximumWidth(100)
        btn.setFlat(False)
        btn.setToolTip(tooltip or label)
        btn.setStyleSheet("""
            QPushButton {
                border-radius: 4px;
                padding: 1px 6px;
                font-size: 11px;
            }
            QPushButton:hover  { background: palette(highlight); }
            QPushButton:pressed{ background: palette(dark); }
        """)

        def _click():
            t = self._tools.get(key, {}).get("target")
            if t is None:
                return
            if callable(t):
                t()
            elif isinstance(t, QWidget):
                if not t.isVisible():
                    t.show()
                t.raise_()
                t.activateWindow()

        btn.clicked.connect(_click)

        # Insert before the trailing stretch
        # Find stretch position
        count = self._layout.count()
        self._layout.insertWidget(count - 1, btn)

        self._tools[key] = {
            "label":  label,
            "icon":   icon,
            "target": target,
            "btn":    btn,
        }
        self.setVisible(True)
        self._update_sep_label()

    def unregister(self, key: str) -> None:
        """Remove a tool button."""
        if key not in self._tools:
            return
        btn = self._tools[key]["btn"]
        self._layout.removeWidget(btn)
        btn.deleteLater()
        del self._tools[key]
        if not self._tools:
            self.setVisible(False)
        self._update_sep_label()

    def update_target(self, key: str, target) -> None:
        """Update the window/callable for an existing tool button."""
        if key in self._tools:
            self._tools[key]["target"] = target

    def is_registered(self, key: str) -> bool:
        return key in self._tools

    def _update_sep_label(self):
        self._sep_label.setVisible(bool(self._tools))

    def apply_theme(self, colors: dict) -> None:
        """Re-style from theme colour dict."""
        bg   = colors.get("bg_secondary", "#252525")
        brd  = colors.get("border",       "#3a3a3a")
        self.setStyleSheet(
            f"QWidget#tool_taskbar {{"
            f"  background:{bg};"
            f"  border-bottom:1px solid {brd};"
            f"}}")

