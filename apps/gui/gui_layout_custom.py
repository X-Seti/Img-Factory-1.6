#belongs in gui/gui_layout_custom.py - Version 1
# X-Seti - January12 2026 - Img Factory 1.6 - Custom UI Module

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QTabWidget, QFrame,
    QGroupBox, QPushButton, QLabel, QCheckBox
)
from PyQt6.QtCore import Qt, QObject, pyqtSignal, QSize
from PyQt6.QtGui import QIcon
from .gui_layout import IMGFactoryGUILayout
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.imgfactory_svg_icons import (
    get_add_icon, get_open_icon, get_refresh_icon, get_close_icon,
    get_save_icon, get_export_icon, get_import_icon, get_remove_icon,
    get_edit_icon, get_view_icon, get_search_icon, get_settings_icon,
    get_rebuild_icon, get_undobar_icon, get_undo_icon, get_redo_icon
)


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

        #self.setWindowTitle(App_name + ": No File")
        #self.resize(1400, 800)
        self.window_always_on_top = False
        #self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

        # Corner resize variables for standalone
        self.dragging = False
        self.drag_position = None
        self.resizing = False
        self.resize_corner = None
        self.corner_size = 20
        self.hover_corner = None
        self._initialize_features()

    @property
    def window_context(self): #vers 1
        """Get the appropriate window context for method calls

        Returns self.main_window if available (when embedded in IMG Factory),
        otherwise returns self (when running standalone)
        """
        return self.main_window if self.main_window is not None else self

    def apply_ui_mode(self, ui_mode: str, show_toolbar: bool, show_status_bar: bool, show_menu_bar: bool):
        """Apply UI mode: 'system' or 'custom'"""
        # Since this is a layout component, we need to work with the main window
        # Store the UI mode preference for later use
        self.ui_mode = ui_mode
        
        # Handle custom titlebar if needed
        if ui_mode == "custom":
            # For custom UI mode, we need to set up the custom titlebar
            if not hasattr(self, '_custom_titlebar') or self._custom_titlebar is None:
                self._custom_titlebar = self._create_toolbar()
                # Connect window control buttons to main window methods
                if hasattr(self, 'minimize_btn'):
                    try:
                        self.minimize_btn.clicked.disconnect()
                    except:
                        pass  # Ignore if no connections exist
                    self.minimize_btn.clicked.connect(self.main_window.showMinimized)
                if hasattr(self, 'maximize_btn'):
                    try:
                        self.maximize_btn.clicked.disconnect()
                    except:
                        pass  # Ignore if no connections exist
                    self.maximize_btn.clicked.connect(self.main_window.showMaximized)
                if hasattr(self, 'close_btn'):
                    try:
                        self.close_btn.clicked.disconnect()
                    except:
                        pass  # Ignore if no connections exist
                    self.close_btn.clicked.connect(self.main_window.close)
            
            # Add the custom titlebar to the main window's layout at the top if not already added
            if self._custom_titlebar:
                # Check if the titlebar is already in the layout to avoid duplicates
                central_widget = self.main_window.centralWidget()
                if central_widget and central_widget.layout():
                    main_layout = central_widget.layout()
                    # Check if titlebar is already the first widget
                    if main_layout.itemAt(0) and main_layout.itemAt(0).widget() != self._custom_titlebar:
                        # Remove from any existing position first to avoid issues
                        self._custom_titlebar.setParent(None)
                        # Insert at the top
                        main_layout.insertWidget(0, self._custom_titlebar)
        
        else:
            # For system UI mode, ensure custom titlebar is hidden if it exists
            if hasattr(self, '_custom_titlebar') and self._custom_titlebar:
                self._custom_titlebar.hide()
        
        # Apply visibility settings for menu bar and status bar after handling custom titlebar
        if hasattr(self.main_window, 'menuBar') and callable(self.main_window.menuBar):
            menu_bar = self.main_window.menuBar()
            if menu_bar:
                menu_bar.setVisible(show_menu_bar)
                
                # In custom mode, make sure menu appears under the custom titlebar
                if ui_mode == "custom":
                    # Force the menu to be visible even in custom mode if requested
                    menu_bar.setVisible(show_menu_bar)
        
        if hasattr(self.main_window, 'statusBar') and callable(self.main_window.statusBar):
            status_bar = self.main_window.statusBar()
            if status_bar:
                status_bar.setVisible(show_status_bar)


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


    def _create_custom_titlebar(self):
        """Create a minimal custom title bar using only existing SVG icons"""
        from PyQt6.QtWidgets import QFrame, QHBoxLayout, QLabel, QPushButton
        from PyQt6.QtCore import Qt, QSize
        from apps.methods.imgfactory_svg_icons import (
            get_settings_icon, get_open_icon, get_save_icon,
            get_search_icon, get_view_icon, get_close_icon
        )

        titlebar = QFrame()
        titlebar.setFixedHeight(30)
        titlebar.setStyleSheet("background: #2b2b2b; border-bottom: 1px solid #444;")
        layout = QHBoxLayout(titlebar)
        layout.setContentsMargins(8, 0, 8, 0)

        # Settings button
        settings_btn = QPushButton()
        settings_btn.setIcon(get_settings_icon())
        settings_btn.setFixedSize(24, 24)
        settings_btn.clicked.connect(lambda: self.main_window.show_imgfactory_settings())
        layout.addWidget(settings_btn)
        layout.addSpacing(10)

        # App title
        title_label = QLabel("Img Factory")
        title_label.setStyleSheet("color: white; font-weight: bold;")
        layout.addWidget(title_label)
        layout.addStretch()

        # Tab-like buttons (using available icons)
        tabs = [
            ("File Entries", get_open_icon),
            ("Directory Tree", get_view_icon),
            ("Search Results", get_search_icon)
        ]
        for text, icon_func in tabs:
            btn = QPushButton(text)
            btn.setIcon(icon_func())
            btn.setIconSize(QSize(16, 16))
            btn.setStyleSheet("""
                QPushButton {
                    color: white;
                    padding: 2px 6px;
                    border: 1px solid #555;
                    border-radius: 3px;
                    background: #3a3a3a;
                    font-size: 9pt;
                }
                QPushButton:hover {
                    background: #4a4a4a;
                }
            """)
            layout.addWidget(btn)

        layout.addStretch()

        # Window controls (text fallback — no minimize/maximize SVGs defined)
        min_btn = QPushButton("_")
        max_btn = QPushButton("□")
        close_btn = QPushButton("✕")
        for btn in [min_btn, max_btn, close_btn]:
            btn.setFixedSize(24, 24)
            btn.setStyleSheet("QPushButton { color: white; background: transparent; }")
        min_btn.clicked.connect(self.main_window.showMinimized)
        max_btn.clicked.connect(lambda: self.main_window.showNormal() if self.main_window.isMaximized() else self.main_window.showMaximized())
        close_btn.clicked.connect(self.main_window.close)
        layout.addWidget(min_btn)
        layout.addWidget(max_btn)
        layout.addWidget(close_btn)

        return titlebar


    def toggle_maximize_restore(self):
        if self.main_window.isMaximized():
            self.main_window.showNormal()
        else:
            self.main_window.showMaximized()


    def _create_toolbar(self): #vers 2 = keep style and theme.
        """Create toolbar - FIXED: Hide drag button when docked, ensure buttons visible"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        from PyQt6.QtGui import QFont

        # Initialize required variables if they don't exist
        if not hasattr(self, 'title_font'):
            self.title_font = QFont("Arial", 14)
        if not hasattr(self, 'button_font'):
            self.button_font = QFont("Arial", 10)
        if not hasattr(self, 'icon_factory'):
            self.icon_factory = SVGIconFactory()
        if not hasattr(globals(), 'App_name'):
            from apps.components.Img_Factory.imgfactory import App_name

        # Create the toolbar widget (this will be the custom titlebar)
        self.titlebar = QWidget()
        self.titlebar.setFixedHeight(45)
        self.titlebar.setObjectName("titlebar")

        # Install event filter for drag detection
        self.titlebar_event_filter = TitleBarEventFilter()
        self.titlebar.installEventFilter(self.titlebar_event_filter)
        self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.titlebar.setMouseTracking(True)

        # Create layout for the titlebar
        self.layout = QHBoxLayout(self.titlebar)
        self.layout.setContentsMargins(5, 5, 5, 5)
        self.layout.setSpacing(5)

        # Get icon color from theme
        icon_color = "#ffffff"  # Default white for dark theme

        # Settings button
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(self.icon_factory.settings_icon())
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))
        
        # Disconnect any existing connections to prevent duplicates
        try:
            self.settings_btn.clicked.disconnect()
        except TypeError:
            # No connections to disconnect, which is fine
            pass
        
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip("Img Factory Settings")
        self.layout.addWidget(self.settings_btn)

        self.layout.addStretch()

        # App title in center
        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.layout.addWidget(self.title_label)

        self.layout.addStretch()
        self.layout.addStretch()

        # Main Filelist window
        # Connect tab buttons to switch tabs
        self.f_entries_btn = QPushButton()
        self.f_entries_btn.setFont(self.button_font)
        self.f_entries_btn.setIcon(self.icon_factory.package_icon())
        self.f_entries_btn.setText("File Entries")
        self.f_entries_btn.setIconSize(QSize(20, 20))
        self.f_entries_btn.setShortcut("Ctrl+tab")
        if hasattr(self, 'button_display_mode') and self.button_display_mode == 'icons':
            self.f_entries_btn.setFixedSize(40, 40)
        self.f_entries_btn.setToolTip("File Entries (Ctrl+tab)")
        # Connect to switch to File Entries tab (index 0)
        self.f_entries_btn.clicked.connect(lambda: self._switch_to_tab(0))
        self.layout.addWidget(self.f_entries_btn)

        # File Browser
        self.dirtree_btn = QPushButton()
        self.dirtree_btn.setFont(self.button_font)
        self.dirtree_btn.setIcon(self.icon_factory.folder_icon())
        self.dirtree_btn.setText("Directory Tree")
        self.dirtree_btn.setIconSize(QSize(20, 20))
        self.dirtree_btn.setShortcut("Ctrl+tab")
        if hasattr(self, 'button_display_mode') and self.button_display_mode == 'icons':
            self.dirtree_btn.setFixedSize(40, 40)
        self.dirtree_btn.setToolTip("Directory Tree (Ctrl+tab)")
        # Connect to switch to Directory Tree tab (index 1)
        self.dirtree_btn.clicked.connect(lambda: self._switch_to_tab(1))
        self.layout.addWidget(self.dirtree_btn)

        # Search Results - Search function results page.
        self.s_results_btn = QPushButton()
        self.s_results_btn.setFont(self.button_font)
        self.s_results_btn.setIcon(self.icon_factory.search_icon())
        self.s_results_btn.setText("Search Results")
        self.s_results_btn.setIconSize(QSize(20, 20))
        self.s_results_btn.setShortcut("Ctrl+tab")
        if hasattr(self, 'button_display_mode') and self.button_display_mode == 'icons':
            self.s_results_btn.setFixedSize(40, 40)
        self.s_results_btn.setToolTip("Search Results (Ctrl+tab)")
        # Connect to switch to Search Results tab (index 2)
        self.s_results_btn.clicked.connect(lambda: self._switch_to_tab(2))
        self.layout.addWidget(self.s_results_btn)

        self.undo_btn = QPushButton()
        self.undo_btn.setFont(self.button_font)
        self.undo_btn.setIcon(self.icon_factory.undo_icon())
        self.undo_btn.setText("Undo")
        self.undo_btn.setIconSize(QSize(20, 20))
        #self.undo_btn.clicked.connect(self._undo_last_action)
        self.undo_btn.setEnabled(False)
        self.undo_btn.setToolTip("Undo last change")
        self.layout.addWidget(self.undo_btn)

        # Info button
        self.info_btn = QPushButton("")
        self.info_btn.setText("")  # CHANGED from "Info"
        self.info_btn.setIcon(self.icon_factory.info_icon())
        self.info_btn.setMinimumWidth(40)
        self.info_btn.setMaximumWidth(40)
        self.info_btn.setMinimumHeight(30)
        self.info_btn.setToolTip("Information")

        self.info_btn.setIconSize(QSize(20, 20))
        self.info_btn.setFixedWidth(35)
        self.info_btn.clicked.connect(self._show_imgfactory_info)
        self.layout.addWidget(self.info_btn)

        # Properties/Theme button
        self.properties_btn = QPushButton()
        self.properties_btn.setFont(self.button_font)
        self.properties_btn.setIcon(self.icon_factory.properties_icon(24, icon_color))
        self.properties_btn.setToolTip("Theme")
        self.properties_btn.setFixedSize(35, 35)
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        self.properties_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.properties_btn.customContextMenuRequested.connect(self._show_settings_context_menu)
        self.layout.addWidget(self.properties_btn)

        # Window controls
        self.minimize_btn = QPushButton()
        self.minimize_btn.setIcon(self.icon_factory.minimize_icon())
        self.minimize_btn.setIconSize(QSize(20, 20))
        self.minimize_btn.setMinimumWidth(40)
        self.minimize_btn.setMaximumWidth(40)
        self.minimize_btn.setMinimumHeight(30)
        self.minimize_btn.clicked.connect(self.main_window.showMinimized)
        self.minimize_btn.setToolTip("Minimize Window") # click tab to restore
        self.layout.addWidget(self.minimize_btn)

        self.maximize_btn = QPushButton()
        self.maximize_btn.setIcon(self.icon_factory.maximize_icon())
        self.maximize_btn.setIconSize(QSize(20, 20))
        self.maximize_btn.setMinimumWidth(40)
        self.maximize_btn.setMaximumWidth(40)
        self.maximize_btn.setMinimumHeight(30)
        self.maximize_btn.clicked.connect(self.toggle_maximize_restore)
        self.maximize_btn.setToolTip("Maximize/Restore Window")
        self.layout.addWidget(self.maximize_btn)

        self.close_btn = QPushButton()
        self.close_btn.setIcon(self.icon_factory.close_icon())
        self.close_btn.setIconSize(QSize(20, 20))
        self.close_btn.setMinimumWidth(40)
        self.close_btn.setMaximumWidth(40)
        self.close_btn.setMinimumHeight(30)
        self.close_btn.clicked.connect(self.main_window.close)
        self.close_btn.setToolTip("Close Window") # closes tab
        self.layout.addWidget(self.close_btn)

        return self.titlebar


    def show_workshop_settings(self): #vers 1
        """Show complete workshop settings dialog"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                    QTabWidget, QWidget, QGroupBox, QFormLayout,
                                    QSpinBox, QComboBox, QSlider, QLabel, QCheckBox,
                                    QFontComboBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + "Settings")
        dialog.setMinimumWidth(650)
        dialog.setMinimumHeight(650)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # TAB 1: FONTS (FIRST TAB)

        fonts_tab = QWidget()
        fonts_layout = QVBoxLayout(fonts_tab)

        # Default Font
        default_font_group = QGroupBox("Default Font")
        default_font_layout = QHBoxLayout()

        default_font_combo = QFontComboBox()
        default_font_combo.setCurrentFont(self.font())
        default_font_layout.addWidget(default_font_combo)

        default_font_size = QSpinBox()
        default_font_size.setRange(8, 24)
        default_font_size.setValue(self.font().pointSize())
        default_font_size.setSuffix(" pt")
        default_font_size.setFixedWidth(80)
        default_font_layout.addWidget(default_font_size)

        default_font_group.setLayout(default_font_layout)
        fonts_layout.addWidget(default_font_group)

        # Title Font
        title_font_group = QGroupBox("Title Font")
        title_font_layout = QHBoxLayout()

        title_font_combo = QFontComboBox()
        if hasattr(self, 'title_font'):
            title_font_combo.setCurrentFont(self.title_font)
        else:
            title_font_combo.setCurrentFont(QFont("Arial", 14))
        title_font_layout.addWidget(title_font_combo)

        title_font_size = QSpinBox()
        title_font_size.setRange(10, 32)
        title_font_size.setValue(getattr(self, 'title_font', QFont("Arial", 14)).pointSize())
        title_font_size.setSuffix(" pt")
        title_font_size.setFixedWidth(80)
        title_font_layout.addWidget(title_font_size)

        title_font_group.setLayout(title_font_layout)
        fonts_layout.addWidget(title_font_group)

        # Panel Font
        panel_font_group = QGroupBox("Panel Headers Font")
        panel_font_layout = QHBoxLayout()

        panel_font_combo = QFontComboBox()
        if hasattr(self, 'panel_font'):
            panel_font_combo.setCurrentFont(self.panel_font)
        else:
            panel_font_combo.setCurrentFont(QFont("Arial", 10))
        panel_font_layout.addWidget(panel_font_combo)

        panel_font_size = QSpinBox()
        panel_font_size.setRange(8, 18)
        panel_font_size.setValue(getattr(self, 'panel_font', QFont("Arial", 10)).pointSize())
        panel_font_size.setSuffix(" pt")
        panel_font_size.setFixedWidth(80)
        panel_font_layout.addWidget(panel_font_size)

        panel_font_group.setLayout(panel_font_layout)
        fonts_layout.addWidget(panel_font_group)

        # Button Font
        button_font_group = QGroupBox("Button Font")
        button_font_layout = QHBoxLayout()

        button_font_combo = QFontComboBox()
        if hasattr(self, 'button_font'):
            button_font_combo.setCurrentFont(self.button_font)
        else:
            button_font_combo.setCurrentFont(QFont("Arial", 10))
        button_font_layout.addWidget(button_font_combo)

        button_font_size = QSpinBox()
        button_font_size.setRange(8, 16)
        button_font_size.setValue(getattr(self, 'button_font', QFont("Arial", 10)).pointSize())
        button_font_size.setSuffix(" pt")
        button_font_size.setFixedWidth(80)
        button_font_layout.addWidget(button_font_size)

        button_font_group.setLayout(button_font_layout)
        fonts_layout.addWidget(button_font_group)

        # Info Bar Font
        infobar_font_group = QGroupBox("Info Bar Font")
        infobar_font_layout = QHBoxLayout()

        infobar_font_combo = QFontComboBox()
        if hasattr(self, 'infobar_font'):
            infobar_font_combo.setCurrentFont(self.infobar_font)
        else:
            infobar_font_combo.setCurrentFont(QFont("Courier New", 9))
        infobar_font_layout.addWidget(infobar_font_combo)

        infobar_font_size = QSpinBox()
        infobar_font_size.setRange(7, 14)
        infobar_font_size.setValue(getattr(self, 'infobar_font', QFont("Courier New", 9)).pointSize())
        infobar_font_size.setSuffix(" pt")
        infobar_font_size.setFixedWidth(80)
        infobar_font_layout.addWidget(infobar_font_size)

        infobar_font_group.setLayout(infobar_font_layout)
        fonts_layout.addWidget(infobar_font_group)

        fonts_layout.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        # TAB 2: DISPLAY SETTINGS

        display_tab = QWidget()
        display_layout = QVBoxLayout(display_tab)

        # Button display mode
        button_group = QGroupBox("Button Display Mode")
        button_layout = QVBoxLayout()

        button_mode_combo = QComboBox()
        button_mode_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])
        current_mode = getattr(self, 'button_display_mode', 'both')
        mode_map = {'both': 0, 'icons': 1, 'text': 2}
        button_mode_combo.setCurrentIndex(mode_map.get(current_mode, 0))
        button_layout.addWidget(button_mode_combo)

        button_hint = QLabel("Changes how toolbar buttons are displayed")
        button_hint.setStyleSheet("color: #888; font-style: italic;")
        button_layout.addWidget(button_hint)

        button_group.setLayout(button_layout)
        display_layout.addWidget(button_group)

        # Table display
        table_group = QGroupBox("Surface List Display")
        table_layout = QVBoxLayout()

        show_thumbnails = QCheckBox("Show Surface types")
        show_thumbnails.setChecked(True)
        table_layout.addWidget(show_thumbnails)

        show_warnings = QCheckBox("Show warning icons for suspicious files")
        show_warnings.setChecked(True)
        show_warnings.setToolTip("Shows surface types")
        table_layout.addWidget(show_warnings)

        table_group.setLayout(table_layout)
        display_layout.addWidget(table_group)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")


        # TAB 3: placeholder
        # TAB 4: PERFORMANCE

        perf_tab = QWidget()
        perf_layout = QVBoxLayout(perf_tab)

        perf_group = QGroupBox("Performance Settings")
        perf_form = QFormLayout()

        preview_quality = QComboBox()
        preview_quality.addItems(["Low (Fast)", "Medium", "High (Slow)"])
        preview_quality.setCurrentIndex(1)
        perf_form.addRow("Preview Quality:", preview_quality)

        thumb_size = QSpinBox()
        thumb_size.setRange(32, 128)
        thumb_size.setValue(64)
        thumb_size.setSuffix(" px")
        perf_form.addRow("Thumbnail Size:", thumb_size)

        perf_group.setLayout(perf_form)
        perf_layout.addWidget(perf_group)

        # Caching
        cache_group = QGroupBox("Caching")
        cache_layout = QVBoxLayout()

        enable_cache = QCheckBox("Enable surface preview caching")
        enable_cache.setChecked(True)
        cache_layout.addWidget(enable_cache)

        cache_hint = QLabel("Caching improves performance but uses more memory")
        cache_hint.setStyleSheet("color: #888; font-style: italic;")
        cache_layout.addWidget(cache_hint)

        cache_group.setLayout(cache_layout)
        perf_layout.addWidget(cache_group)

        perf_layout.addStretch()
        tabs.addTab(perf_tab, "Performance")

        # TAB 5: PREVIEW SETTINGS (LAST TAB)

        preview_tab = QWidget()
        preview_layout = QVBoxLayout(preview_tab)

        # Zoom Settings
        zoom_group = QGroupBox("Zoom Settings")
        zoom_form = QFormLayout()

        zoom_spin = QSpinBox()
        zoom_spin.setRange(10, 500)
        zoom_spin.setValue(int(getattr(self, 'zoom_level', 1.0) * 100))
        zoom_spin.setSuffix("%")
        zoom_form.addRow("Default Zoom:", zoom_spin)

        zoom_group.setLayout(zoom_form)
        preview_layout.addWidget(zoom_group)

        # Background Settings
        bg_group = QGroupBox("Background Settings")
        bg_layout = QVBoxLayout()

        # Background mode
        bg_mode_layout = QFormLayout()
        bg_mode_combo = QComboBox()
        bg_mode_combo.addItems(["Solid Color", "Odd And Even", "Checkerboard", "Grid"])
        current_bg_mode = getattr(self, 'background_mode', 'solid')
        mode_idx = {"solid": 0, "oddeven": 1, "checkerboard": 2, "checker": 2, "grid": 3}.get(current_bg_mode, 0)
        bg_mode_combo.setCurrentIndex(mode_idx)
        bg_mode_layout.addRow("Background Mode:", bg_mode_combo)
        bg_layout.addLayout(bg_mode_layout)

        bg_layout.addSpacing(10)

        # Checkerboard size #TODO size of the file list display
        cbc_label = QLabel("Grid Colomns Size:")
        bg_layout.addWidget(cbc_label)

        cbc_layout = QHBoxLayout()
        cbc_slider = QSlider(Qt.Orientation.Horizontal)
        cbc_slider.setMinimum(4)
        cbc_slider.setMaximum(64)
        cbc_slider.setValue(getattr(self, '_colomn_size', 16))
        cbc_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        cbc_slider.setTickInterval(8)
        cbc_layout.addWidget(cbc_slider)

        cbc_spin = QSpinBox()
        cbc_spin.setMinimum(4)
        cbc_spin.setMaximum(64)
        cbc_spin.setValue(getattr(self, '_checkerboardcolumn_size', 16))
        cbc_spin.setSuffix(" px")
        cbc_spin.setFixedWidth(80)
        cbc_layout.addWidget(cbc_spin)

        bg_layout.addLayout(cbc_layout)

        cbr_label = QLabel("Grid Row Size:")
        bg_layout.addWidget(cbr_label)

        cbr_layout = QHBoxLayout()
        cbr_slider = QSlider(Qt.Orientation.Horizontal)
        cbr_slider.setMinimum(4)
        cbr_slider.setMaximum(64)
        cbr_slider.setValue(getattr(self, '_row_size', 30))
        cbr_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        cbr_slider.setTickInterval(8)
        cbr_layout.addWidget(cbr_slider)

        cbr_spin = QSpinBox()
        cbr_spin.setMinimum(4)
        cbr_spin.setMaximum(64)
        cbr_spin.setValue(getattr(self, '_checkerboardrow_size', 16))
        cbr_spin.setSuffix(" px")
        cbr_spin.setFixedWidth(80)
        cbc_layout.addWidget(cbr_spin)

        bg_layout.addLayout(cbc_layout)

        bg_group.setLayout(bg_layout)
        preview_layout.addWidget(bg_group)

        # Overlay Settings = TODO function needs work.
        overlay_group = QGroupBox("Button Sizes Settings") #Fallback if app_settings_system doesn't exist'
        overlay_layout = QVBoxLayout()

        overlay_label = QLabel("Button height - width size:")
        overlay_layout.addWidget(overlay_label)

        opacity_layout = QHBoxLayout()
        opacity_slider = QSlider(Qt.Orientation.Horizontal)
        opacity_slider.setMinimum(0)
        opacity_slider.setMaximum(100)
        opacity_slider.setValue(getattr(self, '_overlay_opacity', 50))
        opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        opacity_slider.setTickInterval(10)
        opacity_layout.addWidget(opacity_slider)

        opacity_spin = QSpinBox()
        opacity_spin.setMinimum(0)
        opacity_spin.setMaximum(100)
        opacity_spin.setValue(getattr(self, '_overlay_opacity', 50))
        opacity_spin.setSuffix(" %")
        opacity_spin.setFixedWidth(80)
        opacity_layout.addWidget(opacity_spin)

        overlay_layout.addLayout(opacity_layout)

        # Connect opacity controls
        #opacity_slider.valueChanged.connect(opacity_spin.setValue)
        #opacity_spin.valueChanged.connect(opacity_slider.setValue)

        # Hint
        opacity_hint = QLabel("0")
        opacity_hint.setStyleSheet("color: #888; font-style: italic; font-size: 10px;")
        overlay_layout.addWidget(opacity_hint)

        overlay_group.setLayout(overlay_layout)
        preview_layout.addWidget(overlay_group)

        preview_layout.addStretch()
        tabs.addTab(preview_tab, "Preview")

        # Add tabs to dialog
        layout.addWidget(tabs)

        # BUTTONS

        btn_layout = QHBoxLayout()
        btn_layout.addStretch()

        # Apply button
        apply_btn = QPushButton("Apply Settings")
        apply_btn.setStyleSheet("""
            QPushButton {
                background: #0078d4;
                color: white;
                padding: 10px 24px;
                font-weight: bold;
                border-radius: 4px;
                font-size: 13px;
            }
            QPushButton:hover {
                background: #1984d8;
            }
        """)


    def _show_workshop_settings(self): #vers 2
        """Show complete workshop settings dialog with custom UI toggle"""
        # Prevent duplicate dialogs
        if hasattr(self, '_settings_dialog_open') and self._settings_dialog_open:
            return  # Already open, ignore duplicate call
        
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                     QTabWidget, QWidget, QGroupBox, QFormLayout,
                                     QSpinBox, QComboBox, QSlider, QLabel, QCheckBox,
                                     QFontComboBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont
        
        self._settings_dialog_open = True
        dialog = QDialog(self.main_window)
        dialog.setWindowTitle("Img Factory Settings")
        dialog.setMinimumWidth(650)
        dialog.setMinimumHeight(650)
        
        # Make sure to reset the flag when dialog closes
        def cleanup_on_close():
            if hasattr(self, '_settings_dialog_open'):
                self._settings_dialog_open = False
        
    def _switch_to_tab(self, index: int):
        """Switch to the specified tab in the main tab widget"""
        try:
            # Check if the main window has a tab widget and it's accessible
            if hasattr(self.main_window, 'gui_layout') and hasattr(self.main_window.gui_layout, 'tab_widget'):
                tab_widget = self.main_window.gui_layout.tab_widget
                if tab_widget and index < tab_widget.count():
                    tab_widget.setCurrentIndex(index)
            elif hasattr(self, 'tab_widget') and self.tab_widget and index < self.tab_widget.count():
                # Fallback to local tab widget if available
                self.tab_widget.setCurrentIndex(index)
        except Exception as e:
            print(f"Error switching to tab {index}: {str(e)}")

    def _show_workshop_settings(self): #vers 2
        """Show complete workshop settings dialog with custom UI toggle"""
        # Prevent duplicate dialogs
        if hasattr(self, '_settings_dialog_open') and self._settings_dialog_open:
            return  # Already open, ignore duplicate call
        
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                     QTabWidget, QWidget, QGroupBox, QFormLayout,
                                     QSpinBox, QComboBox, QSlider, QLabel, QCheckBox,
                                     QFontComboBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont
        
        self._settings_dialog_open = True
        dialog = QDialog(self.main_window)
        dialog.setWindowTitle("Img Factory Settings")
        dialog.setMinimumWidth(650)
        dialog.setMinimumHeight(650)
        
        # Make sure to reset the flag when dialog closes
        def cleanup_on_close():
            if hasattr(self, '_settings_dialog_open'):
                self._settings_dialog_open = False
        
        dialog.finished.connect(cleanup_on_close)
        
        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # TAB 1: FONTS (FIRST TAB)
        fonts_tab = QWidget()
        fonts_layout = QVBoxLayout(fonts_tab)

        # Default Font
        default_font_group = QGroupBox("Default Font")
        default_font_layout = QHBoxLayout()
        default_font_combo = QFontComboBox()
        default_font_combo.setCurrentFont(self.font())
        default_font_layout.addWidget(default_font_combo)
        default_font_size = QSpinBox()
        default_font_size.setRange(8, 24)
        default_font_size.setValue(self.font().pointSize())
        default_font_size.setSuffix(" pt")
        default_font_size.setFixedWidth(80)
        default_font_layout.addWidget(default_font_size)
        default_font_group.setLayout(default_font_layout)
        fonts_layout.addWidget(default_font_group)

        # Title Font
        title_font_group = QGroupBox("Title Font")
        title_font_layout = QHBoxLayout()
        title_font_combo = QFontComboBox()
        if hasattr(self, 'title_font'):
            title_font_combo.setCurrentFont(self.title_font)
        else:
            title_font_combo.setCurrentFont(QFont("Arial", 14))
        title_font_layout.addWidget(title_font_combo)
        title_font_size = QSpinBox()
        title_font_size.setRange(10, 32)
        title_font_size.setValue(getattr(self, 'title_font', QFont("Arial", 14)).pointSize())
        title_font_size.setSuffix(" pt")
        title_font_size.setFixedWidth(80)
        title_font_layout.addWidget(title_font_size)
        title_font_group.setLayout(title_font_layout)
        fonts_layout.addWidget(title_font_group)

        # Panel Font
        panel_font_group = QGroupBox("Panel Headers Font")
        panel_font_layout = QHBoxLayout()
        panel_font_combo = QFontComboBox()
        if hasattr(self, 'panel_font'):
            panel_font_combo.setCurrentFont(self.panel_font)
        else:
            panel_font_combo.setCurrentFont(QFont("Arial", 10))
        panel_font_layout.addWidget(panel_font_combo)
        panel_font_size = QSpinBox()
        panel_font_size.setRange(8, 18)
        panel_font_size.setValue(getattr(self, 'panel_font', QFont("Arial", 10)).pointSize())
        panel_font_size.setSuffix(" pt")
        panel_font_size.setFixedWidth(80)
        panel_font_layout.addWidget(panel_font_size)
        panel_font_group.setLayout(panel_font_layout)
        fonts_layout.addWidget(panel_font_group)

        # Button Font
        button_font_group = QGroupBox("Button Font")
        button_font_layout = QHBoxLayout()
        button_font_combo = QFontComboBox()
        if hasattr(self, 'button_font'):
            button_font_combo.setCurrentFont(self.button_font)
        else:
            button_font_combo.setCurrentFont(QFont("Arial", 10))
        button_font_layout.addWidget(button_font_combo)
        button_font_size = QSpinBox()
        button_font_size.setRange(8, 16)
        button_font_size.setValue(getattr(self, 'button_font', QFont("Arial", 10)).pointSize())
        button_font_size.setSuffix(" pt")
        button_font_size.setFixedWidth(80)
        button_font_layout.addWidget(button_font_size)
        button_font_group.setLayout(button_font_layout)
        fonts_layout.addWidget(button_font_group)

        # Info Bar Font
        infobar_font_group = QGroupBox("Info Bar Font")
        infobar_font_layout = QHBoxLayout()
        infobar_font_combo = QFontComboBox()
        if hasattr(self, 'infobar_font'):
            infobar_font_combo.setCurrentFont(self.infobar_font)
        else:
            infobar_font_combo.setCurrentFont(QFont("Courier New", 9))
        infobar_font_layout.addWidget(infobar_font_combo)
        infobar_font_size = QSpinBox()
        infobar_font_size.setRange(7, 14)
        infobar_font_size.setValue(getattr(self, 'infobar_font', QFont("Courier New", 9)).pointSize())
        infobar_font_size.setSuffix(" pt")
        infobar_font_size.setFixedWidth(80)
        infobar_font_layout.addWidget(infobar_font_size)
        infobar_font_group.setLayout(infobar_font_layout)
        fonts_layout.addWidget(infobar_font_group)
        fonts_layout.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        # TAB 2: DISPLAY SETTINGS
        display_tab = QWidget()
        display_layout = QVBoxLayout(display_tab)

        # Button display mode
        button_group = QGroupBox("Button Display Mode")
        button_layout = QVBoxLayout()
        button_mode_combo = QComboBox()
        button_mode_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])
        current_mode = getattr(self, 'button_display_mode', 'icons_with_text')
        mode_map = {'icons_with_text': 0, 'icons_only': 1, 'text_only': 2}
        button_mode_combo.setCurrentIndex(mode_map.get(current_mode, 0))
        button_layout.addWidget(button_mode_combo)
        button_hint = QLabel("Changes how toolbar buttons are displayed")
        button_hint.setStyleSheet("color: #888; font-style: italic;")
        button_layout.addWidget(button_hint)
        button_group.setLayout(button_layout)
        display_layout.addWidget(button_group)

        # Window chrome mode
        window_chrome_group = QGroupBox("Window Decorations")
        window_chrome_layout = QVBoxLayout()
        use_system_ui_check = QCheckBox("Use system window decorations")
        use_system_ui_check.setChecked(self.use_system_titlebar)
        window_chrome_layout.addWidget(use_system_ui_check)
        window_chrome_group.setLayout(window_chrome_layout)
        display_layout.addWidget(window_chrome_group)

        # Table display
        table_group = QGroupBox("Surface List Display")
        table_layout = QVBoxLayout()
        show_thumbnails = QCheckBox("Show Surface types")
        show_thumbnails.setChecked(True)
        table_layout.addWidget(show_thumbnails)
        show_warnings = QCheckBox("Show warning icons for suspicious files")
        show_warnings.setChecked(True)
        show_warnings.setToolTip("Shows surface types")
        table_layout.addWidget(show_warnings)
        table_group.setLayout(table_layout)
        display_layout.addWidget(table_group)
        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")

        # Add tabs to dialog
        layout.addWidget(tabs)

        # BUTTONS
        btn_layout = QHBoxLayout()
        btn_layout.addStretch()

        # Apply button
        apply_btn = QPushButton("Apply Settings")
        apply_btn.setStyleSheet("""
        QPushButton {
            background: #0078d4;
            color: white;
            padding: 10px 24px;
            font-weight: bold;
            border-radius: 4px;
            font-size: 13px;
        }
        QPushButton:hover {
            background: #1984d8;
        }
        """)

        def apply_settings():
            # Apply fonts to UI
            self.setFont(QFont(default_font_combo.currentFont().family(),
                               default_font_size.value()))
            self.title_font = QFont(title_font_combo.currentFont().family(),
                                    title_font_size.value())
            self.panel_font = QFont(panel_font_combo.currentFont().family(),
                                    panel_font_size.value())
            self.button_font = QFont(button_font_combo.currentFont().family(),
                                     button_font_size.value())
            self.infobar_font = QFont(infobar_font_combo.currentFont().family(),
                                      infobar_font_size.value())

            # Apply button display mode
            mode_map = {0: 'icons_with_text', 1: 'icons_only', 2: 'text_only'}
            self.button_display_mode = mode_map[button_mode_combo.currentIndex()]

            # Apply window decoration setting
            self.use_system_titlebar = use_system_ui_check.isChecked()
            self._apply_window_flags()

            # Save setting
            if hasattr(self.main_window, 'app_settings'):
                self.main_window.app_settings.current_settings['use_system_titlebar'] = self.use_system_titlebar
                self.main_window.app_settings.save_settings()

            # Refresh display
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Workshop settings updated successfully")
        apply_btn.clicked.connect(apply_settings)
        btn_layout.addWidget(apply_btn)

        # Close button
        close_btn = QPushButton("Close")
        close_btn.setStyleSheet("padding: 10px 24px; font-size: 13px;")
        close_btn.clicked.connect(dialog.close)
        btn_layout.addWidget(close_btn)
        layout.addLayout(btn_layout)

        # Show dialog
        dialog.exec()

    def _show_imgfactory_info(self):
        """Show information dialog about Img Factory"""
        from PyQt6.QtWidgets import QMessageBox
        from apps.components.Img_Factory.imgfactory import App_name, App_auth
        QMessageBox.information(
            self, 
            "About Img Factory", 
            f"{App_name}\n\nApplication Information System\n\nDeveloped by: {App_auth}"
        )

    def _launch_theme_settings(self):
        """Launch theme settings dialog"""
        from apps.utils.app_settings_system import SettingsDialog
        if hasattr(self.main_window, 'app_settings'):
            dialog = SettingsDialog(self.main_window, self.main_window.app_settings)
            dialog.exec()

    def _show_settings_context_menu(self, position):
        """Show context menu for settings button"""
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        
        # Add theme settings action
        theme_action = menu.addAction("Theme Settings")
        theme_action.triggered.connect(self._launch_theme_settings)
        
        # Add about action
        about_action = menu.addAction("About")
        about_action.triggered.connect(self._show_imgfactory_info)
        
        # Show menu at cursor position
        menu.exec(self.mapToGlobal(position))

    def _apply_always_on_top(self): #vers 1
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

    def _initialize_features(self): #vers 3
        """Initialize all features after UI setup"""
        try:
            self._apply_theme()
            self._update_status_indicators()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.window_context.log_message("All features initialized")

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.window_context.log_message(f"Feature init error: {str(e)}")


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

    def paintEvent(self, event): #vers 2
        """Paint corner resize triangles"""
        super().paintEvent(event)

        from PyQt6.QtGui import QPainter, QColor, QPen, QBrush, QPainterPath

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Colors
        normal_color = QColor(100, 100, 100, 150)
        hover_color = QColor(150, 150, 255, 200)

        w = self.width()
        h = self.height()
        grip_size = 8  # Make corners visible (8x8px)
        size = self.corner_size

        # Define corner triangles
        corners = {
            'top-left': [(0, 0), (size, 0), (0, size)],
            'top-right': [(w, 0), (w-size, 0), (w, size)],
            'bottom-left': [(0, h), (size, h), (0, h-size)],
            'bottom-right': [(w, h), (w-size, h), (w, h-size)]
        }
        corners2 = {
            "top-left": [(0, grip_size), (0, 0), (grip_size, 0)],
            "top-right": [(w-grip_size, 0), (w, 0), (w, grip_size)],
            "bottom-left": [(0, h-grip_size), (0, h), (grip_size, h)],
            "bottom-right": [(w-grip_size, h), (w, h), (w, h-grip_size)]
        }

        # Get theme colors for corner indicators
        if self.app_settings:
            theme_colors = self.app_settings.get_theme_colors()
            accent_color = QColor(theme_colors.get('accent_primary', '#1976d2'))
            accent_color.setAlpha(180)
        else:
            accent_color = QColor(100, 150, 255, 180)

        hover_color = QColor(accent_color)
        hover_color.setAlpha(255)

        # Draw all corners with hover effect
        for corner_name, points in corners.items():
            path = QPainterPath()
            path.moveTo(points[0][0], points[0][1])
            path.lineTo(points[1][0], points[1][1])
            path.lineTo(points[2][0], points[2][1])
            path.closeSubpath()

            # Use hover color if mouse is over this corner
            color = hover_color if self.hover_corner == corner_name else accent_color

            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(color))
            painter.drawPath(path)

        painter.end()


    def _get_resize_corner(self, pos): #vers 3
        """Determine which corner is under mouse position"""
        size = self.corner_size; w = self.width(); h = self.height()

        if pos.x() < size and pos.y() < size:
            return "top-left"
        if pos.x() > w - size and pos.y() < size:
            return "top-right"
        if pos.x() < size and pos.y() > h - size:
            return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:
            return "bottom-right"

        return None


    def mousePressEvent(self, event): #vers 8
        """Handle ALL mouse press - dragging and resizing"""
        if event.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(event)
            return

        pos = event.pos()

        # Check corner resize FIRST
        self.resize_corner = self._get_resize_corner(pos)
        if self.resize_corner:
            self.resizing = True
            self.drag_position = event.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            event.accept()
            return

        # Check if on titlebar
        if hasattr(self, 'titlebar') and self.titlebar.geometry().contains(pos):
            titlebar_pos = self.titlebar.mapFromParent(pos)
            if self._is_on_draggable_area(titlebar_pos):
                self.windowHandle().startSystemMove()
                event.accept()
                return

        super().mousePressEvent(event)


    def mouseMoveEvent(self, event): #vers 4
        """Handle mouse move for resizing and hover effects

        Window dragging is handled by eventFilter to avoid conflicts
        """
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept()
                return
        else:
            # Update hover state and cursor
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()  # Trigger repaint for hover effect
            self._update_cursor(corner)

        # Let parent handle everything else
        super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event): #vers 2
        """Handle mouse release"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = False
            self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    def _handle_corner_resize(self, global_pos): #vers 2
        """Handle window resizing from corners"""
        if not self.resize_corner or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.initial_geometry

        min_width = 800
        min_height = 600

        # Calculate new geometry based on corner
        if self.resize_corner == "top-left":
            new_x = geometry.x() + delta.x()
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() - delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, new_y, new_width, new_height)

        elif self.resize_corner == "top-right":
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() - delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(geometry.x(), new_y, new_width, new_height)

        elif self.resize_corner == "bottom-left":
            new_x = geometry.x() + delta.x()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() + delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, geometry.y(), new_width, new_height)

        elif self.resize_corner == "bottom-right":
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() + delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.resize(new_width, new_height)


    def _get_resize_direction(self, pos): #vers 1
        """Determine resize direction based on mouse position"""
        rect = self.rect()
        margin = self.resize_margin

        left = pos.x() < margin
        right = pos.x() > rect.width() - margin
        top = pos.y() < margin
        bottom = pos.y() > rect.height() - margin

        if left and top:
            return "top-left"
        elif right and top:
            return "top-right"
        elif left and bottom:
            return "bottom-left"
        elif right and bottom:
            return "bottom-right"
        elif left:
            return "left"
        elif right:
            return "right"
        elif top:
            return "top"
        elif bottom:
            return "bottom"

        return None


    def _update_cursor(self, direction): #vers 1
        """Update cursor based on resize direction"""
        if direction == "top" or direction == "bottom":
            self.setCursor(Qt.CursorShape.SizeVerCursor)
        elif direction == "left" or direction == "right":
            self.setCursor(Qt.CursorShape.SizeHorCursor)
        elif direction == "top-left" or direction == "bottom-right":
            self.setCursor(Qt.CursorShape.SizeFDiagCursor)
        elif direction == "top-right" or direction == "bottom-left":
            self.setCursor(Qt.CursorShape.SizeBDiagCursor)
        else:
            self.setCursor(Qt.CursorShape.ArrowCursor)


    def _handle_resize(self, global_pos): #vers 1
        """Handle window resizing"""
        if not self.resize_direction or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.frameGeometry()

        min_width = 800
        min_height = 600

        # Handle horizontal resizing
        if "left" in self.resize_direction:
            new_width = geometry.width() - delta.x()
            if new_width >= min_width:
                geometry.setLeft(geometry.left() + delta.x())
        elif "right" in self.resize_direction:
            new_width = geometry.width() + delta.x()
            if new_width >= min_width:
                geometry.setRight(geometry.right() + delta.x())

        # Handle vertical resizing
        if "top" in self.resize_direction:
            new_height = geometry.height() - delta.y()
            if new_height >= min_height:
                geometry.setTop(geometry.top() + delta.y())
        elif "bottom" in self.resize_direction:
            new_height = geometry.height() + delta.y()
            if new_height >= min_height:
                geometry.setBottom(geometry.bottom() + delta.y())

        self.setGeometry(geometry)
        self.drag_position = global_pos


    def resizeEvent(self, event): #vers 1
        '''Keep resize grip in bottom-right corner'''
        super().resizeEvent(event)
        if hasattr(self, 'size_grip'):
            self.size_grip.move(self.width() - 16, self.height() - 16)


    def mouseDoubleClickEvent(self, event): #vers 2
        """Handle double-click - maximize/restore

        Handled here instead of eventFilter for better control
        """
        if event.button() == Qt.MouseButton.LeftButton:
            # Convert to titlebar coordinates if needed
            if hasattr(self, 'titlebar'):
                titlebar_pos = self.titlebar.mapFromParent(event.pos())
                if self._is_on_draggable_area(titlebar_pos):
                    self._toggle_maximize()
                    event.accept()
                    return

        super().mouseDoubleClickEvent(event)


    def closeEvent(self, event): #vers 1
        """Handle close event"""
        self.window_closed.emit()
        event.accept()



# Export functions
__all__ = [
    'IMGFactoryGUILayoutCustom', '_create_toolbar', '_show_workshop_settings'
]
