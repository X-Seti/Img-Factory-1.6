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


class TitleBarEventFilter(QObject):
    """Event filter for custom title bar to handle dragging"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.start_pos = None
    
    def eventFilter(self, obj, event):
        # Implement drag functionality if needed
        # For now, just return False to let events pass through normally
        return False


class IMGFactoryGUILayoutCustom(IMGFactoryGUILayout):
    """Custom UI version of IMGFactoryGUILayout with modern theme and layout"""

    def __init__(self, main_window):
        super().__init__(main_window)
        # Load setting: use system titlebar by default
        self.use_system_titlebar = True
        if hasattr(self.main_window, 'app_settings'):
            self.use_system_titlebar = self.main_window.app_settings.current_settings.get('use_system_titlebar', True)
        # Apply initial window flags
        self._apply_window_flags()

    def apply_ui_mode(self, ui_mode: str, show_toolbar: bool, show_status_bar: bool, show_menu_bar: bool):
        """Apply UI mode: 'system' or 'custom'"""
        # Since this is a layout component, we need to work with the main window
        # Store the UI mode preference for later use
        self.ui_mode = ui_mode
        
        # Apply visibility settings for toolbar, status bar, and menu bar
        if hasattr(self.main_window, 'menuBar') and callable(self.main_window.menuBar):
            menu_bar = self.main_window.menuBar()
            if menu_bar:
                menu_bar.setVisible(show_menu_bar)
        
        if hasattr(self.main_window, 'statusBar') and callable(self.main_window.statusBar):
            status_bar = self.main_window.statusBar()
            if status_bar:
                status_bar.setVisible(show_status_bar)
        
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
        else:
            # For system UI mode, ensure custom titlebar is hidden if it exists
            if hasattr(self, '_custom_titlebar') and self._custom_titlebar:
                self._custom_titlebar.hide()


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
        self.f_entries_btn = QPushButton()
        self.f_entries_btn.setFont(self.button_font)
        self.f_entries_btn.setIcon(self.icon_factory.package_icon())
        self.f_entries_btn.setText("File Entries")
        self.f_entries_btn.setIconSize(QSize(20, 20))
        self.f_entries_btn.setShortcut("Ctrl+tab")
        if hasattr(self, 'button_display_mode') and self.button_display_mode == 'icons':
            self.f_entries_btn.setFixedSize(40, 40)
        self.f_entries_btn.setToolTip("File Entries (Ctrl+tab)")
        #self.f_entries_btn.clicked.connect(self.f_entries_file)
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
        self.dirtree_btn.setEnabled(False)  # Enable when modified
        self.dirtree_btn.setToolTip("Directory Tree (Ctrl+tab)")
        #self.dirtree_btn.clicked.connect(self._save_file)
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
        self.s_results_btn.setEnabled(False)  # Enable when modified
        self.s_results_btn.setToolTip("Search Results (Ctrl+tab)")
        #self.s_results_btn.clicked.connect(self._saveall_file)
        self.layout.addWidget(self.s_results_btn)

        self.undo_btn = QPushButton()
        self.undo_btn.setFont(self.button_font)
        self.undo_btn.setIcon(self.icon_factory.undobar_icon())
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


# Export functions
__all__ = [
    'IMGFactoryGUILayoutCustom', '_create_toolbar', '_show_workshop_settings'
]
