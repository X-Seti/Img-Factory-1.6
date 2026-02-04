#belongs in gui/gui_layout_custom.py - Version 5
# X-Seti - January12 2026 - Img Factory 1.6 - Custom UI Module

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


    def _show_workshop_settings(self): #vers 3
        """Show complete workshop settings dialog - FIXED DUPLICATION"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                    QTabWidget, QWidget, QGroupBox, QFormLayout,
                                    QSpinBox, QComboBox, QSlider, QLabel, QCheckBox,
                                    QFontComboBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self.main_window)
        dialog.setWindowTitle("Img Factory Settings")
        dialog.setMinimumWidth(650)
        dialog.setMinimumHeight(650)
        dialog.setModal(True)

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
        default_font_combo.setCurrentFont(self.main_window.font())
        default_font_layout.addWidget(default_font_combo)

        default_font_size = QSpinBox()
        default_font_size.setRange(8, 24)
        default_font_size.setValue(self.main_window.font().pointSize())
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
        debug_tab = QWidget()
        debug_layout = QVBoxLayout(debug_tab)

        debug_group = QGroupBox("Debug Options")
        debug_group_layout = QVBoxLayout()

        self.enable_debug_check = QCheckBox("Enable debug output to console")
        if hasattr(self.main_window, 'img_settings'):
            self.enable_debug_check.setChecked(self.main_window.img_settings.get("enable_debug", False))  # ADD self.
        else:
            self.enable_debug_check.setChecked(False)

        debug_group_layout.addWidget(self.enable_debug_check)

        debug_hint = QLabel("Shows diagnostic messages in console for troubleshooting")
        debug_hint.setStyleSheet("color: #888; font-style: italic; font-size: 10px;")
        debug_group_layout.addWidget(debug_hint)

        debug_group.setLayout(debug_group_layout)
        debug_layout.addWidget(debug_group)  # Add group to tab layout

        debug_layout.addStretch()
        tabs.addTab(debug_tab, "Debug")
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
        

    def _show_workshop_settings(self): #vers 6
        """Show complete workshop settings dialog - FULL VERSION"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                    QTabWidget, QWidget, QGroupBox, QFormLayout,
                                    QSpinBox, QComboBox, QSlider, QLabel, QCheckBox,
                                    QFontComboBox, QGridLayout, QRadioButton, QButtonGroup,
                                    QMessageBox, QFrame)
        from PyQt6.QtCore import Qt, QSize
        from PyQt6.QtGui import QFont

        dialog = QDialog(self.main_window)
        dialog.setWindowTitle("Img Factory Settings")
        dialog.setMinimumWidth(750)
        dialog.setMinimumHeight(650)
        dialog.setModal(True)

        layout = QVBoxLayout(dialog)
        layout.setContentsMargins(10, 10, 10, 10)

        # Create tabbed interface
        tabs = QTabWidget()

        # ==================== TAB 1: APPEARANCE ====================
        appearance_tab = QWidget()
        appearance_layout = QVBoxLayout(appearance_tab)

        # Tab Sizing Group
        tab_size_group = QGroupBox("Tab Bar Sizing")
        tab_size_layout = QGridLayout(tab_size_group)

        tab_size_layout.addWidget(QLabel("Main Type Tabs Height:"), 0, 0)
        main_tab_height_spin = QSpinBox()
        main_tab_height_spin.setRange(18, 50)
        main_tab_height_spin.setValue(22)
        main_tab_height_spin.setSuffix(" px")
        main_tab_height_spin.setToolTip("Height for IMG/COL/TXD type tabs")
        tab_size_layout.addWidget(main_tab_height_spin, 0, 1)

        tab_size_layout.addWidget(QLabel("File Tabs Height:"), 1, 0)
        individual_tab_height_spin = QSpinBox()
        individual_tab_height_spin.setRange(18, 45)
        individual_tab_height_spin.setValue(22)
        individual_tab_height_spin.setSuffix(" px")
        individual_tab_height_spin.setToolTip("Height for individual file tabs")
        tab_size_layout.addWidget(individual_tab_height_spin, 1, 1)

        tab_size_layout.addWidget(QLabel("Tab Minimum Width:"), 2, 0)
        tab_min_width_spin = QSpinBox()
        tab_min_width_spin.setRange(80, 200)
        tab_min_width_spin.setValue(100)
        tab_min_width_spin.setSuffix(" px")
        tab_size_layout.addWidget(tab_min_width_spin, 2, 1)

        tab_size_layout.addWidget(QLabel("Tab Padding:"), 3, 0)
        tab_padding_spin = QSpinBox()
        tab_padding_spin.setRange(1, 16)
        tab_padding_spin.setValue(1)
        tab_padding_spin.setSuffix(" px")
        tab_size_layout.addWidget(tab_padding_spin, 3, 1)

        tab_size_group.setLayout(tab_size_layout)
        appearance_layout.addWidget(tab_size_group)

        # UI Mode Group
        ui_mode_group = QGroupBox("UI Mode")
        ui_mode_layout = QVBoxLayout(ui_mode_group)

        ui_mode_button_group = QButtonGroup(dialog)
        system_ui_radio = QRadioButton("System UI - Standard window with menu bar")
        custom_ui_radio = QRadioButton("Custom UI - Toolbar with integrated controls")

        ui_mode_button_group.addButton(system_ui_radio, 0)
        ui_mode_button_group.addButton(custom_ui_radio, 1)

        # Load current mode
        if hasattr(self.main_window, 'img_settings'):
            current_mode = self.main_window.img_settings.get("ui_mode", "system")
            if current_mode == "custom":
                custom_ui_radio.setChecked(True)
            else:
                system_ui_radio.setChecked(True)
        else:
            system_ui_radio.setChecked(True)

        ui_mode_layout.addWidget(system_ui_radio)
        ui_mode_layout.addWidget(custom_ui_radio)
        ui_mode_group.setLayout(ui_mode_layout)
        appearance_layout.addWidget(ui_mode_group)

        # Visibility Options
        visibility_group = QGroupBox("Visibility Options")
        visibility_layout = QVBoxLayout(visibility_group)

        show_toolbar_check = QCheckBox("Show toolbar")
        show_toolbar_check.setChecked(True)
        visibility_layout.addWidget(show_toolbar_check)

        show_status_bar_check = QCheckBox("Show status bar")
        show_status_bar_check.setChecked(True)
        visibility_layout.addWidget(show_status_bar_check)

        show_menu_bar_check = QCheckBox("Show menu bar (System UI only)")
        show_menu_bar_check.setChecked(True)
        visibility_layout.addWidget(show_menu_bar_check)

        visibility_group.setLayout(visibility_layout)
        appearance_layout.addWidget(visibility_group)

        appearance_layout.addStretch()
        tabs.addTab(appearance_tab, "Appearance")

        # ==================== TAB 2: FONTS ====================
        fonts_tab = QWidget()
        fonts_layout = QVBoxLayout(fonts_tab)

        # Default Font
        default_font_group = QGroupBox("Default Font")
        default_font_layout = QGridLayout()

        default_font_combo = QFontComboBox()
        default_font_combo.setCurrentFont(self.main_window.font())
        default_font_layout.addWidget(QLabel("Font:"), 0, 0)
        default_font_layout.addWidget(default_font_combo, 0, 1, 1, 2)

        default_font_layout.addWidget(QLabel("Size:"), 1, 0)
        default_font_size = QSpinBox()
        default_font_size.setRange(8, 24)
        default_font_size.setValue(self.main_window.font().pointSize())
        default_font_size.setSuffix(" pt")
        default_font_layout.addWidget(default_font_size, 1, 1)

        default_font_bold = QCheckBox("Bold")
        default_font_italic = QCheckBox("Italic")
        default_font_layout.addWidget(default_font_bold, 1, 2)
        default_font_layout.addWidget(default_font_italic, 1, 3)

        default_font_group.setLayout(default_font_layout)
        fonts_layout.addWidget(default_font_group)

        # Title Font
        title_font_group = QGroupBox("Title Font")
        title_font_layout = QGridLayout()

        title_font_combo = QFontComboBox()
        if hasattr(self.main_window, 'title_font'):
            title_font_combo.setCurrentFont(self.main_window.title_font)
        else:
            title_font_combo.setCurrentFont(QFont("Arial", 14))
        title_font_layout.addWidget(QLabel("Font:"), 0, 0)
        title_font_layout.addWidget(title_font_combo, 0, 1, 1, 2)

        title_font_layout.addWidget(QLabel("Size:"), 1, 0)
        title_font_size = QSpinBox()
        title_font_size.setRange(10, 32)
        title_font_size.setValue(getattr(self.main_window, 'title_font', QFont("Arial", 14)).pointSize())
        title_font_size.setSuffix(" pt")
        title_font_layout.addWidget(title_font_size, 1, 1)

        title_font_bold = QCheckBox("Bold")
        title_font_italic = QCheckBox("Italic")
        title_font_layout.addWidget(title_font_bold, 1, 2)
        title_font_layout.addWidget(title_font_italic, 1, 3)

        title_font_group.setLayout(title_font_layout)
        fonts_layout.addWidget(title_font_group)

        # Panel Font
        panel_font_group = QGroupBox("Panel Headers Font")
        panel_font_layout = QGridLayout()

        panel_font_combo = QFontComboBox()
        if hasattr(self.main_window, 'panel_font'):
            panel_font_combo.setCurrentFont(self.main_window.panel_font)
        else:
            panel_font_combo.setCurrentFont(QFont("Arial", 10))
        panel_font_layout.addWidget(QLabel("Font:"), 0, 0)
        panel_font_layout.addWidget(panel_font_combo, 0, 1, 1, 2)

        panel_font_layout.addWidget(QLabel("Size:"), 1, 0)
        panel_font_size = QSpinBox()
        panel_font_size.setRange(8, 18)
        panel_font_size.setValue(getattr(self.main_window, 'panel_font', QFont("Arial", 10)).pointSize())
        panel_font_size.setSuffix(" pt")
        panel_font_layout.addWidget(panel_font_size, 1, 1)

        panel_font_bold = QCheckBox("Bold")
        panel_font_italic = QCheckBox("Italic")
        panel_font_layout.addWidget(panel_font_bold, 1, 2)
        panel_font_layout.addWidget(panel_font_italic, 1, 3)

        panel_font_group.setLayout(panel_font_layout)
        fonts_layout.addWidget(panel_font_group)

        # Button Font
        button_font_group = QGroupBox("Button Font")
        button_font_layout = QGridLayout()

        button_font_combo = QFontComboBox()
        if hasattr(self.main_window, 'button_font'):
            button_font_combo.setCurrentFont(self.main_window.button_font)
        else:
            button_font_combo.setCurrentFont(QFont("Arial", 10))
        button_font_layout.addWidget(QLabel("Font:"), 0, 0)
        button_font_layout.addWidget(button_font_combo, 0, 1, 1, 2)

        button_font_layout.addWidget(QLabel("Size:"), 1, 0)
        button_font_size = QSpinBox()
        button_font_size.setRange(8, 18)
        button_font_size.setValue(getattr(self.main_window, 'button_font', QFont("Arial", 10)).pointSize())
        button_font_size.setSuffix(" pt")
        button_font_layout.addWidget(button_font_size, 1, 1)

        button_font_bold = QCheckBox("Bold")
        button_font_italic = QCheckBox("Italic")
        button_font_layout.addWidget(button_font_bold, 1, 2)
        button_font_layout.addWidget(button_font_italic, 1, 3)

        button_font_group.setLayout(button_font_layout)
        fonts_layout.addWidget(button_font_group)

        fonts_layout.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        # ==================== TAB 3: BUTTONS ====================
        buttons_tab = QWidget()
        buttons_layout = QVBoxLayout(buttons_tab)

        # Button Display Mode
        display_group = QGroupBox("Button Display Mode")
        display_layout = QVBoxLayout(display_group)

        display_button_group = QButtonGroup(dialog)
        icons_text_radio = QRadioButton("Show Icons and Text")
        icons_only_radio = QRadioButton("Show Icons Only")
        text_only_radio = QRadioButton("Show Text Only")

        display_button_group.addButton(icons_text_radio, 0)
        display_button_group.addButton(icons_only_radio, 1)
        display_button_group.addButton(text_only_radio, 2)

        icons_text_radio.setChecked(True)

        display_layout.addWidget(icons_text_radio)
        display_layout.addWidget(icons_only_radio)
        display_layout.addWidget(text_only_radio)

        display_group.setLayout(display_layout)
        buttons_layout.addWidget(display_group)

        # Button Sizing
        button_size_group = QGroupBox("Button Sizing")
        button_size_layout = QGridLayout(button_size_group)

        button_size_layout.addWidget(QLabel("Button Height:"), 0, 0)
        button_height_spin = QSpinBox()
        button_height_spin.setRange(24, 48)
        button_height_spin.setValue(32)
        button_height_spin.setSuffix(" px")
        button_size_layout.addWidget(button_height_spin, 0, 1)

        button_size_layout.addWidget(QLabel("Icon Size:"), 1, 0)
        icon_size_spin = QSpinBox()
        icon_size_spin.setRange(12, 32)
        icon_size_spin.setValue(16)
        icon_size_spin.setSuffix(" px")
        button_size_layout.addWidget(icon_size_spin, 1, 1)

        button_size_layout.addWidget(QLabel("Horizontal Spacing:"), 2, 0)
        h_spacing_spin = QSpinBox()
        h_spacing_spin.setRange(2, 20)
        h_spacing_spin.setValue(6)
        h_spacing_spin.setSuffix(" px")
        button_size_layout.addWidget(h_spacing_spin, 2, 1)

        button_size_layout.addWidget(QLabel("Vertical Spacing:"), 3, 0)
        v_spacing_spin = QSpinBox()
        v_spacing_spin.setRange(2, 20)
        v_spacing_spin.setValue(8)
        v_spacing_spin.setSuffix(" px")
        button_size_layout.addWidget(v_spacing_spin, 3, 1)

        button_size_group.setLayout(button_size_layout)
        buttons_layout.addWidget(button_size_group)

        # Button Theme
        button_theme_group = QGroupBox("Button Theme")
        button_theme_layout = QVBoxLayout(button_theme_group)

        pastel_check = QCheckBox("Enable pastel effect for buttons")
        pastel_check.setChecked(True)
        button_theme_layout.addWidget(pastel_check)

        high_contrast_check = QCheckBox("High contrast buttons")
        high_contrast_check.setChecked(False)
        button_theme_layout.addWidget(high_contrast_check)

        rounded_corners_check = QCheckBox("Rounded corners")
        rounded_corners_check.setChecked(True)
        button_theme_layout.addWidget(rounded_corners_check)

        button_theme_group.setLayout(button_theme_layout)
        buttons_layout.addWidget(button_theme_group)

        buttons_layout.addStretch()
        tabs.addTab(buttons_tab, "Buttons")

        # ==================== TAB 4: ADVANCED ====================
        advanced_tab = QWidget()
        advanced_layout = QVBoxLayout(advanced_tab)

        # File Handling
        file_group = QGroupBox("File Handling")
        file_layout = QVBoxLayout()

        auto_save_check = QCheckBox("Auto-save after import")
        auto_save_check.setChecked(True)
        file_layout.addWidget(auto_save_check)

        auto_reload_check = QCheckBox("Auto-reload after import (reload from disk)")
        auto_reload_check.setChecked(False)
        file_layout.addWidget(auto_reload_check)

        load_ide_check = QCheckBox("Load IDE file automatically if found with IMG")
        load_ide_check.setChecked(False)
        file_layout.addWidget(load_ide_check)

        file_group.setLayout(file_layout)
        advanced_layout.addWidget(file_group)

        # Backup Settings
        backup_group = QGroupBox("Backup Settings")
        backup_layout = QGridLayout()

        auto_backup_check = QCheckBox("Enable automatic backups")
        auto_backup_check.setChecked(False)
        backup_layout.addWidget(auto_backup_check, 0, 0, 1, 2)

        backup_layout.addWidget(QLabel("Backup count:"), 1, 0)
        backup_count_spin = QSpinBox()
        backup_count_spin.setRange(1, 10)
        backup_count_spin.setValue(3)
        backup_count_spin.setEnabled(False)
        backup_layout.addWidget(backup_count_spin, 1, 1)

        def toggle_backup_count(checked):
            backup_count_spin.setEnabled(checked)

        auto_backup_check.toggled.connect(toggle_backup_count)

        backup_group.setLayout(backup_layout)
        advanced_layout.addWidget(backup_group)

        # Recent Files
        recent_group = QGroupBox("Recent Files")
        recent_layout = QHBoxLayout()
        recent_layout.addWidget(QLabel("Recent files limit:"))
        recent_files_spin = QSpinBox()
        recent_files_spin.setRange(5, 50)
        recent_files_spin.setValue(10)
        recent_layout.addWidget(recent_files_spin)
        recent_layout.addStretch()
        recent_group.setLayout(recent_layout)
        advanced_layout.addWidget(recent_group)

        # Window Settings
        window_group = QGroupBox("Window Settings")
        window_layout = QVBoxLayout()

        remember_size_check = QCheckBox("Remember window size")
        remember_size_check.setChecked(True)
        window_layout.addWidget(remember_size_check)

        remember_pos_check = QCheckBox("Remember window position")
        remember_pos_check.setChecked(True)
        window_layout.addWidget(remember_pos_check)

        window_group.setLayout(window_layout)
        advanced_layout.addWidget(window_group)

        advanced_layout.addStretch()
        tabs.addTab(advanced_tab, "Advanced")

        # ==================== TAB 5: PREVIEW ====================
        preview_tab = QWidget()
        preview_layout = QVBoxLayout(preview_tab)

        # Button Preview
        preview_group = QGroupBox("Button Preview")
        preview_inner_layout = QVBoxLayout()

        preview_label = QLabel("Preview buttons will appear here based on your settings")
        preview_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        preview_label.setStyleSheet("padding: 20px; background: #f0f0f0; border: 1px solid #ccc;")
        preview_inner_layout.addWidget(preview_label)

        preview_group.setLayout(preview_inner_layout)
        preview_layout.addWidget(preview_group)

        # Overlay Settings
        overlay_group = QGroupBox("Preview Overlay Settings")
        overlay_layout = QVBoxLayout()

        overlay_layout.addWidget(QLabel("Overlay opacity:"))

        opacity_controls = QHBoxLayout()
        opacity_slider = QSlider(Qt.Orientation.Horizontal)
        opacity_slider.setMinimum(0)
        opacity_slider.setMaximum(100)
        opacity_slider.setValue(50)
        opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        opacity_slider.setTickInterval(10)
        opacity_controls.addWidget(opacity_slider)

        opacity_spin = QSpinBox()
        opacity_spin.setMinimum(0)
        opacity_spin.setMaximum(100)
        opacity_spin.setValue(50)
        opacity_spin.setSuffix(" %")
        opacity_spin.setFixedWidth(80)
        opacity_controls.addWidget(opacity_spin)

        # Connect opacity controls
        opacity_slider.valueChanged.connect(opacity_spin.setValue)
        opacity_spin.valueChanged.connect(opacity_slider.setValue)

        overlay_layout.addLayout(opacity_controls)

        opacity_hint = QLabel("Adjust transparency for preview overlays")
        opacity_hint.setStyleSheet("color: #888; font-style: italic; font-size: 10px;")
        overlay_layout.addWidget(opacity_hint)

        overlay_group.setLayout(overlay_layout)
        preview_layout.addWidget(overlay_group)

        preview_layout.addStretch()
        tabs.addTab(preview_tab, "Preview")

        # Add tabs to dialog
        layout.addWidget(tabs)

        # ==================== BUTTONS ====================
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        # Reset button
        reset_btn = QPushButton("Reset to Defaults")
        reset_btn.setStyleSheet("""
            QPushButton {
                padding: 8px 16px;
                font-size: 11px;
            }
        """)

        def reset_to_defaults():
            reply = QMessageBox.question(
                dialog,
                "Reset Settings",
                "Are you sure you want to reset all settings to defaults?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
            )
            if reply == QMessageBox.StandardButton.Yes:
                # Reset all controls to defaults
                main_tab_height_spin.setValue(22)
                individual_tab_height_spin.setValue(20)
                tab_min_width_spin.setValue(100)
                tab_padding_spin.setValue(1)
                system_ui_radio.setChecked(True)
                show_toolbar_check.setChecked(True)
                show_status_bar_check.setChecked(True)
                show_menu_bar_check.setChecked(True)
                icons_text_radio.setChecked(True)
                button_height_spin.setValue(32)
                icon_size_spin.setValue(16)
                h_spacing_spin.setValue(6)
                v_spacing_spin.setValue(8)
                pastel_check.setChecked(True)
                high_contrast_check.setChecked(False)
                rounded_corners_check.setChecked(True)
                auto_save_check.setChecked(True)
                auto_backup_check.setChecked(False)
                recent_files_spin.setValue(10)

        reset_btn.clicked.connect(reset_to_defaults)
        button_layout.addWidget(reset_btn)

        # Cancel button
        cancel_btn = QPushButton("Cancel")
        cancel_btn.setStyleSheet("""
            QPushButton {
                padding: 8px 16px;
                font-size: 11px;
            }
        """)
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        # Apply button
        apply_btn = QPushButton("Apply")
        apply_btn.setStyleSheet("""
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

        def apply_settings():
            """Apply all settings"""
            try:
                # Apply tab sizing
                main_tab_height = main_tab_height_spin.value()
                individual_tab_height = individual_tab_height_spin.value()
                tab_min_width = tab_min_width_spin.value()
                tab_padding = tab_padding_spin.value()

                # Apply to main tab widget
                if hasattr(self.main_window, 'main_tab_widget'):
                    self.main_window.main_tab_widget.setStyleSheet(f"""
                        QTabBar::tab {{
                            height: {individual_tab_height}px;
                            min-height: {individual_tab_height}px;
                            max-height: {individual_tab_height}px;
                            min-width: {tab_min_width}px;
                            padding: {tab_padding}px 12px;
                        }}
                    """)

                # Apply to main type tabs if exists
                if hasattr(self.main_window, 'main_type_tabs'):
                    self.main_window.main_type_tabs.setStyleSheet(f"""
                        QTabBar::tab {{
                            height: {main_tab_height}px;
                            min-height: {main_tab_height}px;
                            max-height: {main_tab_height}px;
                            min-width: {tab_min_width}px;
                            padding: {tab_padding}px 12px;
                        }}
                    """)

                # Save all settings to img_settings
                if hasattr(self.main_window, 'img_settings'):
                    settings = self.main_window.img_settings

                    # UI Mode
                    ui_mode = "custom" if custom_ui_radio.isChecked() else "system"
                    settings.set("ui_mode", ui_mode)
                    settings.set("show_toolbar", show_toolbar_check.isChecked())
                    settings.set("show_status_bar", show_status_bar_check.isChecked())
                    settings.set("show_menu_bar", show_menu_bar_check.isChecked())

                    # Tab sizing
                    settings.set("main_type_tab_height", main_tab_height)
                    settings.set("individual_tab_height", individual_tab_height)
                    settings.set("tab_min_width", tab_min_width)
                    settings.set("tab_padding", tab_padding)

                    # Button settings
                    settings.set("button_height", button_height_spin.value())
                    settings.set("button_icon_size", icon_size_spin.value())
                    settings.set("button_spacing_horizontal", h_spacing_spin.value())
                    settings.set("button_spacing_vertical", v_spacing_spin.value())
                    settings.set("use_pastel_buttons", pastel_check.isChecked())
                    settings.set("high_contrast_buttons", high_contrast_check.isChecked())

                    # File handling
                    settings.set("auto_save_on_import", auto_save_check.isChecked())
                    settings.set("auto_reload_on_import", auto_reload_check.isChecked())
                    settings.set("load_ide_with_img", load_ide_check.isChecked())

                    # Backups
                    settings.set("auto_backup", auto_backup_check.isChecked())
                    settings.set("backup_count", backup_count_spin.value())

                    # Recent files
                    settings.set("recent_files_limit", recent_files_spin.value())

                    # Window
                    settings.set("remember_window_size", remember_size_check.isChecked())
                    settings.set("remember_window_position", remember_pos_check.isChecked())

                    #settings.set("enable_debug", enable_debug_check.isChecked())

                    settings.save_settings()

                # Apply fonts
                if hasattr(self.main_window, 'setFont'):
                    new_font = default_font_combo.currentFont()
                    new_font.setPointSize(default_font_size.value())
                    new_font.setBold(default_font_bold.isChecked())
                    new_font.setItalic(default_font_italic.isChecked())
                    self.main_window.setFont(new_font)

                # Apply title font
                if hasattr(self.main_window, 'title_font'):
                    title_font = title_font_combo.currentFont()
                    title_font.setPointSize(title_font_size.value())
                    title_font.setBold(title_font_bold.isChecked())
                    title_font.setItalic(title_font_italic.isChecked())
                    self.main_window.title_font = title_font

                # Apply panel font
                if hasattr(self.main_window, 'panel_font'):
                    panel_font = panel_font_combo.currentFont()
                    panel_font.setPointSize(panel_font_size.value())
                    panel_font.setBold(panel_font_bold.isChecked())
                    panel_font.setItalic(panel_font_italic.isChecked())
                    self.main_window.panel_font = panel_font

                # Apply button font
                if hasattr(self.main_window, 'button_font'):
                    btn_font = button_font_combo.currentFont()
                    btn_font.setPointSize(button_font_size.value())
                    btn_font.setBold(button_font_bold.isChecked())
                    btn_font.setItalic(button_font_italic.isChecked())
                    self.main_window.button_font = btn_font

                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Settings applied successfully")

                QMessageBox.information(dialog, "Success", "Settings applied!\n\nNote: Some changes require restart to take full effect.")

            except Exception as e:
                QMessageBox.warning(dialog, "Error", f"Failed to apply settings:\n{str(e)}")

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

        # Get icon color from theme
        icon_color = "#ffffff"

        # M Menu button - BEFORE settings
        self.menu_btn = QPushButton()
        self.menu_btn.setFont(self.button_font)
        self.menu_btn.setIcon(self.icon_factory.menu_m_icon())
        self.menu_btn.setText("Menu")
        self.menu_btn.setIconSize(QSize(20, 20))
        self.menu_btn.clicked.connect(self._show_popup_menu)
        self.menu_btn.setToolTip("Main Menu")
        layout.addWidget(self.menu_btn)

        # Settings button - PREVENT DUPLICATE CONNECTIONS
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(self.icon_factory.settings_icon())
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))

        try:
            self.settings_btn.clicked.disconnect()
        except (TypeError, RuntimeError):
            pass

        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip("Img Factory Settings")
        layout.addWidget(self.settings_btn)

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
        self.undo_btn.setIcon(self.icon_factory.undo_icon())
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
        self.info_btn.setIcon(self.icon_factory.info_icon())
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
            self.minimize_btn.setIcon(self.icon_factory.minimize_icon())
            self.minimize_btn.setIconSize(QSize(20, 20))
            self.minimize_btn.setMinimumWidth(40)
            self.minimize_btn.setMaximumWidth(40)
            self.minimize_btn.setMinimumHeight(30)
            self.minimize_btn.clicked.connect(self.main_window.showMinimized)
            self.minimize_btn.setToolTip("Minimize Window")
            layout.addWidget(self.minimize_btn)

            self.maximize_btn = QPushButton()
            self.maximize_btn.setIcon(self.icon_factory.maximize_icon())
            self.maximize_btn.setIconSize(QSize(20, 20))
            self.maximize_btn.setMinimumWidth(40)
            self.maximize_btn.setMaximumWidth(40)
            self.maximize_btn.setMinimumHeight(30)
            self.maximize_btn.clicked.connect(self.toggle_maximize_restore)
            self.maximize_btn.setToolTip("Maximize/Restore Window")
            layout.addWidget(self.maximize_btn)

            self.close_btn = QPushButton()
            self.close_btn.setIcon(self.icon_factory.close_icon())
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
                self.main_window.log_message("Directory Tree already loaded - use 'Dir Tree' tab")
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
        author.setStyleSheet("color: #666;")
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
        version_info.setStyleSheet("color: #888; font-size: 10px;")
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


    def move_entries_up(self):  # vers 1
        """Move selected entries up in the table"""
        try:
            if self.table and self.table.selectedItems():
                # Get selected rows
                selected_items = self.table.selectedItems()
                selected_rows = sorted(set(item.row() for item in selected_items))

                # Check if any selected rows are already at the top
                if 0 in selected_rows:
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Cannot move entries up: some are already at top")
                    return

                # Store data for selected rows
                selected_data = []
                for row in selected_rows:
                    row_data = []
                    for col in range(self.table.columnCount()):
                        item = self.table.item(row, col)
                        if item:
                            row_data.append(item.text())
                        else:
                            row_data.append("")
                    selected_data.append(row_data)

                # Remove selected rows from the table (in reverse order to maintain indices)
                for row in sorted(selected_rows, reverse=True):
                    self.table.removeRow(row)

                # Calculate new positions (move up by 1)
                new_start_pos = min(selected_rows) - 1
                if new_start_pos < 0:
                    new_start_pos = 0

                # Insert rows at new positions
                for i, row_data in enumerate(selected_data):
                    insert_row = new_start_pos + i
                    self.table.insertRow(insert_row)
                    for j, cell_data in enumerate(row_data):
                        self.table.setItem(insert_row, j, QTableWidgetItem(cell_data))

                # Re-select the moved rows
                self.table.clearSelection()
                for i in range(len(selected_data)):
                    for col in range(self.table.columnCount()):
                        item = self.table.item(new_start_pos + i, col)
                        if item:
                            item.setSelected(True)

                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{len(selected_data)} entries moved up")
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("No selected entries to move or table not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Move entries up error: {str(e)}")


    def move_entries_down(self):  # vers 1
        """Move selected entries down in the table"""
        try:
            if self.table and self.table.selectedItems():
                # Get selected rows
                selected_items = self.table.selectedItems()
                selected_rows = sorted(set(item.row() for item in selected_items))

                # Check if any selected rows are already at the bottom
                if max(selected_rows) >= self.table.rowCount() - 1:
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Cannot move entries down: some are already at bottom")
                    return

                # Store data for selected rows
                selected_data = []
                for row in reversed(selected_rows):  # Process in reverse to maintain indices when removing
                    row_data = []
                    for col in range(self.table.columnCount()):
                        item = self.table.item(row, col)
                        if item:
                            row_data.append(item.text())
                        else:
                            row_data.append("")
                    selected_data.insert(0, row_data)  # Insert at beginning to maintain order

                # Remove selected rows from the table (in reverse order to maintain indices)
                for row in sorted(selected_rows, reverse=True):
                    self.table.removeRow(row)

                # Calculate new positions (move down by 1)
                new_start_pos = min(selected_rows) + 1

                # Insert rows at new positions
                for i, row_data in enumerate(selected_data):
                    insert_row = new_start_pos + i
                    self.table.insertRow(insert_row)
                    for j, cell_data in enumerate(row_data):
                        self.table.setItem(insert_row, j, QTableWidgetItem(cell_data))

                # Re-select the moved rows
                self.table.clearSelection()
                for i in range(len(selected_data)):
                    for col in range(self.table.columnCount()):
                        item = self.table.item(new_start_pos + i, col)
                        if item:
                            item.setSelected(True)

                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{len(selected_data)} entries moved down")
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("No selected entries to move or table not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Move entries down error: {str(e)}")


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

    def _initialize_features(self): #vers 4
        """Initialize all features after UI setup - including enhanced logging"""
        try:
            self._apply_theme()
            self._update_status_indicators()
            
            # Connect various system events to the extended log
            self._connect_extended_logging()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.window_context.log_message("All features initialized")
                self.window_context.log_message("Extended logging system active")
                
                # Automatically switch to directory tree view at startup
                QTimer.singleShot(100, self._switch_to_directory_tree)  # Delay to ensure UI is fully loaded

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.window_context.log_message(f"Feature init error: {str(e)}")

    
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


    def closeEvent(self, event): #vers 1
        """Handle close event"""
        self.window_closed.emit()
        event.accept()



# Export functions
__all__ = [
    'IMGFactoryGUILayoutCustom', '_create_toolbar', '_show_workshop_settings'
]
