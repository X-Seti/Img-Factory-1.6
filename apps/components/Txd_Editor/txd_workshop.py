#!/usr/bin/env python3
#this belongs in apps/components/Txd_Editor/txd_workshop.py - Version: 43
# X-Seti - September30 2026 - IMG Factory 1.6 - TXD Workshop

"""
TXD Workshop - main window: init, settings, docking, help, theme, tabs.
Everything else lives in depends/ mixins.
"""

import os
import sys
from pathlib import Path
# Force X11/GLX backend for NVIDIA on Wayland (Linux only)
if sys.platform.startswith('linux'):
    os.environ['QT_QPA_PLATFORM'] = 'xcb'
    os.environ['QSG_RHI_BACKEND'] = 'opengl'
    os.environ['LIBGL_ALWAYS_SOFTWARE'] = '0'  # Use hardware acceleration
_root = Path(__file__).resolve().parents[3]
if str(_root) not in sys.path: sys.path.insert(0, str(_root))

from PyQt6.QtCore import QPoint, Qt, pyqtSignal
from PyQt6.QtWidgets import QApplication, QLabel, QMessageBox, QWidget
from apps.components.Txd_Editor.depends.txd_logic_func import TXDLogicMixin
from apps.components.Txd_Editor.depends.txd_ui_func import App_name, DEBUG_STANDALONE, TXDUIMixin
from apps.components.Txd_Editor.depends.txd_win_func import TXDWindowMixin
from apps.debug.debug_functions import img_debugger
from apps.gui.tool_menu_mixin import ToolMenuMixin
from apps.gui.txd_context_menu import setup_txd_context_menu
from apps.methods.img_factory_settings import get_user_config_dir
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.ribbon_dialog import RibbonIconsMixin
from apps.methods.grip_splitter import SplitterSizesMixin

##Methods list -
# open_txd_workshop

##class TXDWorkshop: -
# _close_txd_tab
# closeEvent
# _dock_to_main
# __init__
# _launch_theme_settings
# _ribbon_config_path
# _show_settings_dialog
# _show_shaders_dialog
# _show_workshop_settings
# _switch_txd_tab
# toggle_dock_mode
# _toggle_tearoff
# _undock_from_main
# _update_dock_button_visibility

##Mixins (depends/) -
# TXDWindowMixin - txd_win_func.py
# TXDUIMixin - txd_ui_func.py
# TXDLogicMixin - txd_logic_func.py
# RibbonIconsMixin - methods/ribbon_dialog.py
# SplitterSizesMixin - methods/grip_splitter.py
# Shared windows - methods/txd_dialogs.py, DXT encoders - methods/txd_dxt_encode.py


class TXDWorkshop(TXDWindowMixin, TXDUIMixin, TXDLogicMixin, RibbonIconsMixin, SplitterSizesMixin, ToolMenuMixin, QWidget): #vers 7
    """TXD Workshop - Main texture editing window"""

    workshop_closed = pyqtSignal()
    window_closed = pyqtSignal()

    # Bump whenever the set of ribbon toolbars changes (added/removed/
    # renamed) so a saved layout from an older structure is cleanly
    # rejected by _restore_toolbar_state instead of Qt silently failing
    # to restore it. History: 1 = Transform/Nav/Effects only,
    # 2 = added merged Name/Format + Mipmaps, 3 = split Name/Format apart.
    _RIBBON_LAYOUT_VERSION = 3
    _SPLITTER_LIST_PX = 250     # texture list minimum width


    def __init__(self, parent=None, main_window=None): #vers 12
        """Initialize TXD Workshop"""
        if DEBUG_STANDALONE and main_window is None:
            print(App_name + " Initializing ...")

        super().__init__(parent)
        self.main_window = main_window
        self.setWindowTitle(App_name)
        self.setWindowIcon(SVGIconFactory.txd_workshop_icon())
        self.icon_factory = SVGIconFactory()

        # Initialize app_settings for theme support
        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        else:
            self.app_settings = None
        if self.app_settings and hasattr(self.app_settings, 'theme_changed'):
            self.app_settings.theme_changed.connect(self._refresh_icons)

        self.current_img = None
        self.current_txd_data = None
        self.current_txd_name = None
        self.txd_list = []
        self.texture_list = []
        self.selected_texture = None
        self.undo_stack = []
        self.button_display_mode = 'icons'
        self.current_txd_path = None
        self.save_to_source_location = True
        self.last_save_directory = None
        self._load_settings()
        self.texture_view_states = {}
        self._current_view_state = 0

        # Set default fonts
        from PyQt6.QtGui import QFont
        default_font = QFont("Fira Sans Condensed", 14)
        self.setFont(default_font)
        self.title_font = QFont("Arial", 14)
        self.panel_font = QFont("Arial", 10)
        self.button_font = QFont("Arial", 10)
        self.infobar_font = QFont("Courier New", 9)

        # Preview settings
        self._show_checkerboard = True
        self._checkerboard_size = 16
        self._overlay_opacity = 50
        self._invert_alpha = False
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)
        #self.background_color = self._get_ui_color('viewport_bg')
        self.background_mode = 'solid'
        self.placeholder_text = "No texture"
        self.setMinimumSize(200, 200)
        self.info_bitdepth = QLabel("[32bit]")

        # Texture import/export settings
        self.dimension_limiting_enabled = False
        self.splash_screen_mode = False
        self.custom_max_dimension = 4096

        # Texture naming settings
        self.name_limit_enabled = True
        self.max_texture_name_length = 32

        # Format support flags
        self.iff_import_enabled = True
        self.splash_formats_enabled = True

        # Export preferences
        self.export_target_game = "auto"
        self.export_target_platform = "pc"

        # TXD version tracking
        self.txd_version_id = 0
        self.txd_device_id = 0
        self.txd_version_str = "Unknown"
        self.txd_platform_name = "Unknown"
        self.txd_game = "Unknown"
        self.txd_capabilities = {}

        # Detect standalone mode FIRST
        self.standalone_mode = (main_window is None)

        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        else:
            # FIXED: Create AppSettings for standalone mode
            try:
                from apps.utils.app_settings_system import AppSettings
                self.app_settings = AppSettings()
                img_debugger.debug("AppSettings initialized for standalone COL Workshop")
            except Exception as e:
                img_debugger.warning(f"Could not initialize AppSettings: {e}")
                self.app_settings = None


        # Docking state
        self.is_docked = (main_window is not None)
        self.dock_widget = None
        self.is_overlay = False
        self.overlay_table = None
        self.overlay_tab_index = -1

        self.setWindowTitle(App_name +": No File")
        self.resize(1400, 800)
        self.use_system_titlebar = False
        self.window_always_on_top = False

        # Window flags
        self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

        self._initialize_features()

        # Corner resize variables
        self.dragging = False
        self.drag_position = None
        self.resizing = False
        self.resize_corner = None
        self.corner_size = 20
        self.hover_corner = None

        if parent:
            parent_pos = parent.pos()
            self.move(parent_pos.x() + 50, parent_pos.y() + 80)

        self.txd_tabs = []
        self.current_tab_index = 0

        # Setup UI FIRST
        self.setup_ui()
        self.setAcceptDrops(True)       # .txd / .img / images dropped here

        # THEN setup context menu
        setup_txd_context_menu(self)

        # Setup hotkeys
        self._setup_hotkeys()

        # Apply theme ONCE at the end
        self._apply_theme()

        # Enable mouse tracking
        self.setMouseTracking(True)

        if DEBUG_STANDALONE and self.standalone_mode:
            print(App_name + " initialized")


# - Settings Reusable

    def _show_workshop_settings(self): #vers 5
        """Show complete workshop settings dialog"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                    QTabWidget, QWidget, QGroupBox, QFormLayout,
                                    QSpinBox, QComboBox, QSlider, QLabel, QCheckBox,
                                    QFontComboBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name +" Settings")
        dialog.setMinimumWidth(650)
        dialog.setMinimumHeight(550)

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
        table_group = QGroupBox("Texture List Display")
        table_layout = QVBoxLayout()

        show_thumbnails = QCheckBox("Show texture thumbnails")
        show_thumbnails.setChecked(True)
        table_layout.addWidget(show_thumbnails)

        show_warnings = QCheckBox("Show warning icons for suspicious textures")
        show_warnings.setChecked(True)
        show_warnings.setToolTip("Shows warning icon if normal and alpha appear identical")
        table_layout.addWidget(show_warnings)

        table_group.setLayout(table_layout)
        display_layout.addWidget(table_group)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")

        # TAB 3: EXPORT SETTINGS

        export_tab = QWidget()
        export_layout = QVBoxLayout(export_tab)

        # Default export format
        export_format_group = QGroupBox("Default Export Format")
        export_format_layout = QVBoxLayout()

        format_combo = QComboBox()
        format_combo.addItems(["PNG", "TGA", "BMP", "DDS"])
        format_combo.setCurrentText(getattr(self, 'default_export_format', 'PNG'))
        export_format_layout.addWidget(format_combo)

        format_hint = QLabel("PNG recommended for best quality and compatibility")
        format_hint.setStyleSheet("color: #888; font-style: italic;")
        export_format_layout.addWidget(format_hint)

        export_format_group.setLayout(export_format_layout)
        export_layout.addWidget(export_format_group)

        # Export options
        export_options_group = QGroupBox("Export Options")
        export_options_layout = QVBoxLayout()

        preserve_alpha = QCheckBox("Preserve alpha channel when exporting")
        preserve_alpha.setChecked(True)
        export_options_layout.addWidget(preserve_alpha)

        export_mipmaps = QCheckBox("Export mipmaps as separate files")
        export_mipmaps.setChecked(False)
        export_mipmaps.setToolTip("Saves each mipmap level as texture_mip0.png, texture_mip1.png, etc.")
        export_options_layout.addWidget(export_mipmaps)

        auto_folder = QCheckBox("Auto-create subfolders by texture name")
        auto_folder.setChecked(False)
        export_options_layout.addWidget(auto_folder)

        export_options_group.setLayout(export_options_layout)
        export_layout.addWidget(export_options_group)

        # Target game/platform
        target_group = QGroupBox("Export Target")
        target_layout = QFormLayout()

        game_combo = QComboBox()
        game_combo.addItems(["Auto Detect", "GTA III", "GTA Vice City", "GTA San Andreas", "Manhunt"])
        target_layout.addRow("Target Game:", game_combo)

        platform_combo = QComboBox()
        platform_combo.addItems(["PC", "Xbox", "PS2", "Android", "Multi-platform"])
        target_layout.addRow("Target Platform:", platform_combo)

        target_group.setLayout(target_layout)
        export_layout.addWidget(target_group)

        export_layout.addStretch()
        tabs.addTab(export_tab, "Export")

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

        enable_cache = QCheckBox("Enable texture preview caching")
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
        bg_mode_combo.addItems(["Solid Color", "Checkerboard", "Grid"])
        current_bg_mode = getattr(self, 'background_mode', 'solid')
        mode_idx = {"solid": 0, "checkerboard": 1, "checker": 1, "grid": 2}.get(current_bg_mode, 0)
        bg_mode_combo.setCurrentIndex(mode_idx)
        bg_mode_layout.addRow("Background Mode:", bg_mode_combo)
        bg_layout.addLayout(bg_mode_layout)

        bg_layout.addSpacing(10)

        # Checkerboard size
        cb_label = QLabel("Checkerboard Size:")
        bg_layout.addWidget(cb_label)

        cb_layout = QHBoxLayout()
        cb_slider = QSlider(Qt.Orientation.Horizontal)
        cb_slider.setMinimum(4)
        cb_slider.setMaximum(64)
        cb_slider.setValue(getattr(self, '_checkerboard_size', 16))
        cb_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        cb_slider.setTickInterval(8)
        cb_layout.addWidget(cb_slider)

        cb_spin = QSpinBox()
        cb_spin.setMinimum(4)
        cb_spin.setMaximum(64)
        cb_spin.setValue(getattr(self, '_checkerboard_size', 16))
        cb_spin.setSuffix(" px")
        cb_spin.setFixedWidth(80)
        cb_layout.addWidget(cb_spin)

        bg_layout.addLayout(cb_layout)

        # Connect checkerboard controls
        cb_slider.valueChanged.connect(cb_spin.setValue)
        cb_spin.valueChanged.connect(cb_slider.setValue)

        # Hint
        cb_hint = QLabel("Smaller = tighter pattern, larger = bigger squares")
        cb_hint.setStyleSheet("color: #888; font-style: italic; font-size: 10px;")
        bg_layout.addWidget(cb_hint)

        bg_group.setLayout(bg_layout)
        preview_layout.addWidget(bg_group)

        # Overlay Settings
        overlay_group = QGroupBox("Overlay View Settings")
        overlay_layout = QVBoxLayout()

        overlay_label = QLabel("Overlay Opacity (Normal over Alpha):")
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
        opacity_slider.valueChanged.connect(opacity_spin.setValue)
        opacity_spin.valueChanged.connect(opacity_slider.setValue)

        # Hint
        opacity_hint = QLabel("0% = Only alpha visible, 100% = Only normal visible")
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
                background: palette(highlight);
                color: white;
                padding: 10px 24px;
                font-weight: bold;
                border-radius: 4px;
                font-size: 13px;
            }
            QPushButton:hover {
                background: palette(highlight);
            }
        """)

        def apply_settings():  #vers 1
            # FONTS
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

            # Apply fonts to UI
            self._apply_title_font()
            self._apply_panel_font()
            self._apply_button_font()
            self._apply_infobar_font()

            # DISPLAY
            mode_map = {0: 'both', 1: 'icons', 2: 'text'}
            self.button_display_mode = mode_map[button_mode_combo.currentIndex()]

            # EXPORT
            self.default_export_format = format_combo.currentText()

            # PREVIEW
            self.zoom_level = zoom_spin.value() / 100.0

            bg_modes = ['solid', 'checkerboard', 'grid']
            self.background_mode = bg_modes[bg_mode_combo.currentIndex()]

            self._checkerboard_size = cb_spin.value()
            self._overlay_opacity = opacity_spin.value()

            # Update preview widget
            if hasattr(self, 'preview_widget'):
                if self.background_mode == 'checkerboard':
                    self.preview_widget.set_checkerboard_background()
                    self.preview_widget._checkerboard_size = self._checkerboard_size
                else:
                    self.preview_widget.set_background_color(self.preview_widget.bg_color)

            # Apply button display mode
            if hasattr(self, '_update_all_buttons'):
                self._update_all_buttons()

            # Refresh display
            if self.selected_texture:
                self._update_texture_info(self.selected_texture)

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


# - Docking functions

    def _update_dock_button_visibility(self): #vers 1
        """Show/hide dock and tearoff buttons based on docked state"""
        if hasattr(self, 'dock_btn'):
            # Hide D button when docked, show when standalone
            self.dock_btn.setVisible(not self.is_docked)

        if hasattr(self, 'tearoff_btn'):
            # T button only visible when docked and not in standalone mode
            self.tearoff_btn.setVisible(self.is_docked and not self.standalone_mode)


    def toggle_dock_mode(self): #vers 1
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
            if hasattr(self, '_middle_btn_row'):
                self._middle_btn_row.setVisible(True)

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} docked to main window")


        except Exception as e:
            img_debugger.error(f"Error docking: {str(e)}")
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
            if hasattr(self, '_middle_btn_row'):
                self._middle_btn_row.setVisible(False)

            self.show()
            self.raise_()

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} undocked to standalone")

        except Exception as e:
            img_debugger.error(f"Error undocking: {str(e)}")
            # Fallback
            self.setWindowFlags(Qt.WindowType.Window)
            self.show()


    def _ribbon_config_path(self): #vers 1
        """Ribbon layout, presets and custom icons file."""
        return get_user_config_dir() / 'txd_workshop.json'


    def _toggle_tearoff(self): #vers 2
        """Toggle tear-off state (merge back to IMG Factory) - IMPROVED"""
        try:
            if self.is_docked:
                # Undock from main window
                self._undock_from_main()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{App_name} torn off from main window")
            else:
                # Dock back to main window
                self._dock_to_main()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{App_name} docked back to main window")

        except Exception as e:
            img_debugger.error(f"Error toggling tear-off: {str(e)}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Tear-off Error", f"Could not toggle tear-off state:\n{str(e)}")


    def _launch_theme_settings(self): #vers 2
        """Launch theme engine from app_settings_system"""
        try:
            from apps.utils.app_settings_system import AppSettings, SettingsDialog

            # Get or create app_settings
            if not hasattr(self, 'app_settings') or self.app_settings is None:
                self.app_settings = AppSettings()
                if not hasattr(self.app_settings, 'current_settings'):
                    img_debugger.error("AppSettings failed to initialize")
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.warning(self, "Error", "Could not initialize theme system")
                    return

            # Launch settings dialog
            dialog = SettingsDialog(self.app_settings, self)

            # Connect theme change signal to apply theme
            dialog.themeChanged.connect(lambda theme: self._apply_theme())

            if dialog.exec():
                # Apply theme after dialog closes
                self._apply_theme()
                img_debugger.success("Theme settings applied")
                if hasattr(self, 'main_window') and self.main_window:
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Theme settings updated")

        except Exception as e:
            img_debugger.error(f"Theme settings error: {e}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Theme Error", f"Could not load theme system:\n{e}")


    def _show_settings_dialog(self): #vers 6
        """Show comprehensive settings dialog with all tabs including hotkeys"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                                    QWidget, QLabel, QPushButton, QGroupBox,
                                    QCheckBox, QSpinBox, QFormLayout, QScrollArea,
                                    QKeySequenceEdit, QComboBox, QMessageBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QKeySequence

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        dialog.setMinimumWidth(700)
        dialog.setMinimumHeight(600)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # === DISPLAY TAB ===
        display_tab = QWidget()
        display_layout = QVBoxLayout(display_tab)

        # Thumbnail settings
        thumb_group = QGroupBox("Thumbnail Display")
        thumb_layout = QVBoxLayout()

        thumb_size_layout = QHBoxLayout()
        thumb_size_layout.addWidget(QLabel("Thumbnail size:"))
        thumb_size_spin = QSpinBox()
        thumb_size_spin.setRange(32, 256)
        thumb_size_spin.setValue(self.thumbnail_size if hasattr(self, 'thumbnail_size') else 64)
        thumb_size_spin.setSuffix(" px")
        thumb_size_layout.addWidget(thumb_size_spin)
        thumb_size_layout.addStretch()
        thumb_layout.addLayout(thumb_size_layout)

        thumb_group.setLayout(thumb_layout)
        display_layout.addWidget(thumb_group)

        # Table display settings
        table_group = QGroupBox("Table Display")
        table_layout = QVBoxLayout()

        row_height_layout = QHBoxLayout()
        row_height_layout.addWidget(QLabel("Row height:"))
        row_height_spin = QSpinBox()
        row_height_spin.setRange(50, 200)
        row_height_spin.setValue(getattr(self, 'table_row_height', 100))
        row_height_spin.setSuffix(" px")
        row_height_layout.addWidget(row_height_spin)
        row_height_layout.addStretch()
        table_layout.addLayout(row_height_layout)

        show_grid_check = QCheckBox("Show grid lines")
        show_grid_check.setChecked(getattr(self, 'show_grid_lines', True))
        table_layout.addWidget(show_grid_check)

        table_group.setLayout(table_layout)
        display_layout.addWidget(table_group)

        # Status bar settings
        status_group = QGroupBox("Status Bar")
        status_layout = QVBoxLayout()

        show_status_bar_check = QCheckBox("Show " + App_name + " status bar")
        show_status_bar_check.setChecked(getattr(self, 'show_status_bar', True))
        show_status_bar_check.setToolTip(
            "The bottom row showing texture count, selection, and size")
        status_layout.addWidget(show_status_bar_check)

        relay_status_check = QCheckBox("Relay status to IMG Factory status bar when docked")
        relay_status_check.setChecked(getattr(self, 'relay_status_to_img_factory', True))
        relay_status_check.setToolTip(
            "Pushes texture count/selection/size into IMG Factory's own\n"
            "status bar - useful on its own, and worth keeping on if you\n"
            "turn off this workshop's own status bar above")
        status_layout.addWidget(relay_status_check)

        status_group.setLayout(status_layout)
        display_layout.addWidget(status_group)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")

        # === PREVIEW TAB ===
        preview_tab = QWidget()
        preview_layout = QVBoxLayout(preview_tab)

        # Preview window settings
        preview_window_group = QGroupBox("Preview Window")
        preview_window_layout = QVBoxLayout()

        show_preview_check = QCheckBox("Show preview window by default")
        show_preview_check.setChecked(getattr(self, 'show_preview_default', True))
        show_preview_check.setToolTip("Automatically open preview when selecting textures")
        preview_window_layout.addWidget(show_preview_check)

        auto_refresh_check = QCheckBox("Auto-refresh preview on selection")
        auto_refresh_check.setChecked(getattr(self, 'auto_refresh_preview', True))
        auto_refresh_check.setToolTip("Update preview immediately when clicking textures")
        preview_window_layout.addWidget(auto_refresh_check)

        preview_window_group.setLayout(preview_window_layout)
        preview_layout.addWidget(preview_window_group)

        # Preview size settings
        preview_size_group = QGroupBox("Preview Size")
        preview_size_layout = QVBoxLayout()

        preview_width_layout = QHBoxLayout()
        preview_width_layout.addWidget(QLabel("Default width:"))
        preview_width_spin = QSpinBox()
        preview_width_spin.setRange(200, 1920)
        preview_width_spin.setValue(getattr(self, 'preview_width', 512))
        preview_width_spin.setSuffix(" px")
        preview_width_layout.addWidget(preview_width_spin)
        preview_width_layout.addStretch()
        preview_size_layout.addLayout(preview_width_layout)

        preview_height_layout = QHBoxLayout()
        preview_height_layout.addWidget(QLabel("Default height:"))
        preview_height_spin = QSpinBox()
        preview_height_spin.setRange(200, 1080)
        preview_height_spin.setValue(getattr(self, 'preview_height', 512))
        preview_height_spin.setSuffix(" px")
        preview_height_layout.addWidget(preview_height_spin)
        preview_height_layout.addStretch()
        preview_size_layout.addLayout(preview_height_layout)

        preview_size_group.setLayout(preview_size_layout)
        preview_layout.addWidget(preview_size_group)

        # Preview background
        preview_bg_group = QGroupBox("Preview Background")
        preview_bg_layout = QVBoxLayout()

        bg_combo = QComboBox()
        bg_combo.addItems(["Checkerboard", "Black", "White", "Gray", "Custom Color"])
        bg_combo.setCurrentText(getattr(self, 'preview_background', 'Checkerboard'))
        preview_bg_layout.addWidget(bg_combo)

        bg_hint = QLabel("Checkerboard helps visualize alpha transparency")
        bg_hint.setStyleSheet("color: #888; font-style: italic;")
        preview_bg_layout.addWidget(bg_hint)

        preview_bg_group.setLayout(preview_bg_layout)
        preview_layout.addWidget(preview_bg_group)

        # Preview zoom
        preview_zoom_group = QGroupBox("Preview Zoom")
        preview_zoom_layout = QVBoxLayout()

        fit_to_window_check = QCheckBox("Fit to window by default")
        fit_to_window_check.setChecked(getattr(self, 'preview_fit_to_window', True))
        preview_zoom_layout.addWidget(fit_to_window_check)

        smooth_zoom_check = QCheckBox("Use smooth scaling")
        smooth_zoom_check.setChecked(getattr(self, 'preview_smooth_scaling', True))
        smooth_zoom_check.setToolTip("Better quality but slower for large textures")
        preview_zoom_layout.addWidget(smooth_zoom_check)

        preview_zoom_group.setLayout(preview_zoom_layout)
        preview_layout.addWidget(preview_zoom_group)

        preview_layout.addStretch()
        tabs.addTab(preview_tab, "Preview")

        # === EXPORT TAB ===
        export_tab = QWidget()
        export_layout = QVBoxLayout(export_tab)

        # Export format
        format_group = QGroupBox("Default Export Format")
        format_layout = QVBoxLayout()

        format_combo = QComboBox()
        format_combo.addItems(["PNG", "TGA", "BMP", "DDS"])
        format_combo.setCurrentText(getattr(self, 'default_export_format', 'PNG'))
        format_layout.addWidget(format_combo)

        format_hint = QLabel("PNG recommended for best quality and compatibility")
        format_hint.setStyleSheet("color: #888; font-style: italic;")
        format_layout.addWidget(format_hint)

        format_group.setLayout(format_layout)
        export_layout.addWidget(format_group)

        # Export options
        export_options_group = QGroupBox("Export Options")
        export_options_layout = QVBoxLayout()

        preserve_alpha_check = QCheckBox("Preserve alpha channel when exporting")
        preserve_alpha_check.setChecked(getattr(self, 'export_preserve_alpha', True))
        export_options_layout.addWidget(preserve_alpha_check)

        export_mipmaps_check = QCheckBox("Export mipmaps as separate files")
        export_mipmaps_check.setChecked(getattr(self, 'export_mipmaps_separate', False))
        export_mipmaps_check.setToolTip("Save each mipmap level as texture_name_mip0.png, etc.")
        export_options_layout.addWidget(export_mipmaps_check)

        create_subfolders_check = QCheckBox("Create subfolders when exporting all")
        create_subfolders_check.setChecked(getattr(self, 'export_create_subfolders', False))
        create_subfolders_check.setToolTip("Organize exports into folders by TXD name")
        export_options_layout.addWidget(create_subfolders_check)

        export_options_group.setLayout(export_options_layout)
        export_layout.addWidget(export_options_group)

        # Compatibility note
        compat_label = QLabel(
            "Note: TXD files use RenderWare format. Exported textures are converted to standard image formats."
        )
        compat_label.setWordWrap(True)
        compat_label.setStyleSheet("padding: 10px; background-color: palette(mid); border-radius: 4px;")
        export_layout.addWidget(compat_label)

        export_layout.addStretch()
        tabs.addTab(export_tab, "Export")

        # === IMPORT TAB ===
        import_tab = QWidget()
        import_layout = QVBoxLayout(import_tab)

        # Import behavior
        import_behavior_group = QGroupBox("Import Behavior")
        import_behavior_layout = QVBoxLayout()

        auto_name_check = QCheckBox("Auto-name textures from filename")
        auto_name_check.setChecked(getattr(self, 'import_auto_name', True))
        auto_name_check.setToolTip("Use image filename as texture name")
        import_behavior_layout.addWidget(auto_name_check)

        replace_check = QCheckBox("Replace existing textures with same name")
        replace_check.setChecked(getattr(self, 'import_replace_existing', False))
        import_behavior_layout.addWidget(replace_check)

        auto_format_check = QCheckBox("Automatically select best format")
        auto_format_check.setChecked(getattr(self, 'import_auto_format', True))
        auto_format_check.setToolTip("Choose DXT1/DXT5 based on alpha channel")
        import_behavior_layout.addWidget(auto_format_check)

        import_behavior_group.setLayout(import_behavior_layout)
        import_layout.addWidget(import_behavior_group)

        # Import format
        import_format_group = QGroupBox("Default Import Format")
        import_format_layout = QVBoxLayout()

        import_format_combo = QComboBox()
        import_format_combo.addItems(["DXT1", "DXT3", "DXT5", "ARGB8888", "RGB888"])
        import_format_combo.setCurrentText(getattr(self, 'default_import_format', 'DXT1'))
        import_format_layout.addWidget(import_format_combo)

        format_note = QLabel("DXT1: No alpha, best compression\nDXT5: With alpha, good compression\nARGB8888: Uncompressed, best quality")
        format_note.setStyleSheet("color: #888; font-style: italic;")
        import_format_layout.addWidget(format_note)

        import_format_group.setLayout(import_format_layout)
        import_layout.addWidget(import_format_group)

        import_layout.addStretch()
        tabs.addTab(import_tab, "Import")

        # === TEXTURE CONSTRAINTS TAB ===
        constraints_tab = QWidget()
        constraints_layout = QVBoxLayout(constraints_tab)

        # Dimension constraints
        dimension_group = QGroupBox("Dimension Constraints")
        dimension_layout = QVBoxLayout()

        dimension_check = QCheckBox("Enforce power-of-2 dimensions")
        dimension_check.setChecked(getattr(self, 'dimension_limiting_enabled', True))
        dimension_check.setToolTip("Enforce sizes like 256, 512, 1024, 2048")
        dimension_layout.addWidget(dimension_check)

        splash_check = QCheckBox("Allow splash screen dimensions")
        splash_check.setChecked(getattr(self, 'splash_screen_mode', False))
        splash_check.setToolTip("Allow non-power-of-2 sizes like 1280x720, 720x576, 640x480")
        dimension_layout.addWidget(splash_check)

        max_dim_layout = QHBoxLayout()
        max_dim_layout.addWidget(QLabel("Maximum dimension:"))
        max_dim_spin = QSpinBox()
        max_dim_spin.setRange(256, 8192)
        max_dim_spin.setValue(getattr(self, 'custom_max_dimension', 2048))
        max_dim_spin.setSingleStep(256)
        max_dim_spin.setToolTip("Maximum width/height for imported textures")
        max_dim_layout.addWidget(max_dim_spin)
        max_dim_layout.addStretch()
        dimension_layout.addLayout(max_dim_layout)

        dimension_group.setLayout(dimension_layout)
        constraints_layout.addWidget(dimension_group)

        # Texture naming
        naming_group = QGroupBox("Texture Naming")
        naming_layout = QVBoxLayout()

        name_limit_check = QCheckBox("Enable name length limit")
        name_limit_check.setChecked(getattr(self, 'name_limit_enabled', True))
        name_limit_check.setToolTip("Enforce maximum texture name length")
        naming_layout.addWidget(name_limit_check)

        char_limit_layout = QHBoxLayout()
        char_limit_layout.addWidget(QLabel("Maximum characters:"))
        char_limit_spin = QSpinBox()
        char_limit_spin.setRange(8, 64)
        char_limit_spin.setValue(getattr(self, 'max_texture_name_length', 32))
        char_limit_spin.setToolTip("RenderWare default is 32 characters")
        char_limit_layout.addWidget(char_limit_spin)
        char_limit_layout.addStretch()
        naming_layout.addLayout(char_limit_layout)

        naming_group.setLayout(naming_layout)
        constraints_layout.addWidget(naming_group)

        # Format support
        format_support_group = QGroupBox("Format Support")
        format_support_layout = QVBoxLayout()

        iff_check = QCheckBox("Enable IFF (Amiga) format import")
        iff_check.setChecked(getattr(self, 'iff_import_enabled', False))
        iff_check.setToolTip("Support for Amiga IFF/ILBM image format")
        format_support_layout.addWidget(iff_check)

        format_support_group.setLayout(format_support_layout)
        constraints_layout.addWidget(format_support_group)

        constraints_layout.addStretch()
        tabs.addTab(constraints_tab, "Constraints")

        # === KEYBOARD SHORTCUTS TAB ===
        hotkeys_tab = QWidget()
        hotkeys_layout = QVBoxLayout(hotkeys_tab)

        # Add scroll area for hotkeys
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)

        # File Operations Group
        file_group = QGroupBox("File Operations")
        file_form = QFormLayout()

        hotkey_edit_open = QKeySequenceEdit(self.hotkey_open.key() if hasattr(self, 'hotkey_open') else QKeySequence.StandardKey.Open)
        file_form.addRow("Open TXD:", hotkey_edit_open)

        hotkey_edit_save = QKeySequenceEdit(self.hotkey_save.key() if hasattr(self, 'hotkey_save') else QKeySequence.StandardKey.Save)
        file_form.addRow("Save TXD:", hotkey_edit_save)

        hotkey_edit_force_save = QKeySequenceEdit(self.hotkey_force_save.key() if hasattr(self, 'hotkey_force_save') else QKeySequence("Alt+Shift+S"))
        force_save_layout = QHBoxLayout()
        force_save_layout.addWidget(hotkey_edit_force_save)
        force_save_hint = QLabel("(Force save even if unmodified)")
        force_save_hint.setStyleSheet("color: #888; font-style: italic;")
        force_save_layout.addWidget(force_save_hint)
        file_form.addRow("Force Save:", force_save_layout)

        hotkey_edit_save_as = QKeySequenceEdit(self.hotkey_save_as.key() if hasattr(self, 'hotkey_save_as') else QKeySequence.StandardKey.SaveAs)
        file_form.addRow("Save As:", hotkey_edit_save_as)

        hotkey_edit_close = QKeySequenceEdit(self.hotkey_close.key() if hasattr(self, 'hotkey_close') else QKeySequence.StandardKey.Close)
        file_form.addRow("Close:", hotkey_edit_close)

        file_group.setLayout(file_form)
        scroll_layout.addWidget(file_group)

        # Edit Operations Group
        edit_group = QGroupBox("Edit Operations")
        edit_form = QFormLayout()

        hotkey_edit_undo = QKeySequenceEdit(self.hotkey_undo.key() if hasattr(self, 'hotkey_undo') else QKeySequence.StandardKey.Undo)
        edit_form.addRow("Undo:", hotkey_edit_undo)

        hotkey_edit_copy = QKeySequenceEdit(self.hotkey_copy.key() if hasattr(self, 'hotkey_copy') else QKeySequence.StandardKey.Copy)
        edit_form.addRow("Copy Texture:", hotkey_edit_copy)

        hotkey_edit_paste = QKeySequenceEdit(self.hotkey_paste.key() if hasattr(self, 'hotkey_paste') else QKeySequence.StandardKey.Paste)
        edit_form.addRow("Paste Texture:", hotkey_edit_paste)

        hotkey_edit_delete = QKeySequenceEdit(self.hotkey_delete.key() if hasattr(self, 'hotkey_delete') else QKeySequence.StandardKey.Delete)
        edit_form.addRow("Delete:", hotkey_edit_delete)

        hotkey_edit_duplicate = QKeySequenceEdit(self.hotkey_duplicate.key() if hasattr(self, 'hotkey_duplicate') else QKeySequence("Ctrl+D"))
        edit_form.addRow("Duplicate:", hotkey_edit_duplicate)

        hotkey_edit_rename = QKeySequenceEdit(self.hotkey_rename.key() if hasattr(self, 'hotkey_rename') else QKeySequence("F2"))
        edit_form.addRow("Rename:", hotkey_edit_rename)

        edit_group.setLayout(edit_form)
        scroll_layout.addWidget(edit_group)

        # Texture Operations Group
        texture_group = QGroupBox("Texture Operations")
        texture_form = QFormLayout()

        hotkey_edit_import = QKeySequenceEdit(self.hotkey_import.key() if hasattr(self, 'hotkey_import') else QKeySequence("Ctrl+I"))
        texture_form.addRow("Import Texture:", hotkey_edit_import)

        hotkey_edit_export = QKeySequenceEdit(self.hotkey_export.key() if hasattr(self, 'hotkey_export') else QKeySequence("Ctrl+E"))
        texture_form.addRow("Export Texture:", hotkey_edit_export)

        hotkey_edit_export_all = QKeySequenceEdit(self.hotkey_export_all.key() if hasattr(self, 'hotkey_export_all') else QKeySequence("Ctrl+Shift+E"))
        texture_form.addRow("Export All:", hotkey_edit_export_all)

        texture_group.setLayout(texture_form)
        scroll_layout.addWidget(texture_group)

        # View Operations Group
        view_group = QGroupBox("View Operations")
        view_form = QFormLayout()

        hotkey_edit_refresh = QKeySequenceEdit(self.hotkey_refresh.key() if hasattr(self, 'hotkey_refresh') else QKeySequence.StandardKey.Refresh)
        view_form.addRow("Refresh:", hotkey_edit_refresh)

        hotkey_edit_properties = QKeySequenceEdit(self.hotkey_properties.key() if hasattr(self, 'hotkey_properties') else QKeySequence("Alt+Return"))
        view_form.addRow("Properties:", hotkey_edit_properties)

        hotkey_edit_find = QKeySequenceEdit(self.hotkey_find.key() if hasattr(self, 'hotkey_find') else QKeySequence.StandardKey.Find)
        view_form.addRow("Find/Search:", hotkey_edit_find)

        hotkey_edit_help = QKeySequenceEdit(self.hotkey_help.key() if hasattr(self, 'hotkey_help') else QKeySequence.StandardKey.HelpContents)
        view_form.addRow("Help:", hotkey_edit_help)

        view_group.setLayout(view_form)
        scroll_layout.addWidget(view_group)

        scroll_layout.addStretch()

        scroll.setWidget(scroll_widget)
        hotkeys_layout.addWidget(scroll)

        # Reset to defaults button
        reset_layout = QHBoxLayout()
        reset_layout.addStretch()
        reset_hotkeys_btn = QPushButton("Reset to Plasma6 Defaults")

        def reset_hotkeys():  #vers 1
            reply = QMessageBox.question(dialog, "Reset Hotkeys",
                "Reset all keyboard shortcuts to Plasma6 defaults?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)

            if reply == QMessageBox.StandardButton.Yes:
                hotkey_edit_open.setKeySequence(QKeySequence.StandardKey.Open)
                hotkey_edit_save.setKeySequence(QKeySequence.StandardKey.Save)
                hotkey_edit_force_save.setKeySequence(QKeySequence("Alt+Shift+S"))
                hotkey_edit_save_as.setKeySequence(QKeySequence.StandardKey.SaveAs)
                hotkey_edit_close.setKeySequence(QKeySequence.StandardKey.Close)
                hotkey_edit_undo.setKeySequence(QKeySequence.StandardKey.Undo)
                hotkey_edit_copy.setKeySequence(QKeySequence.StandardKey.Copy)
                hotkey_edit_paste.setKeySequence(QKeySequence.StandardKey.Paste)
                hotkey_edit_delete.setKeySequence(QKeySequence.StandardKey.Delete)
                hotkey_edit_duplicate.setKeySequence(QKeySequence("Ctrl+D"))
                hotkey_edit_rename.setKeySequence(QKeySequence("F2"))
                hotkey_edit_import.setKeySequence(QKeySequence("Ctrl+I"))
                hotkey_edit_export.setKeySequence(QKeySequence("Ctrl+E"))
                hotkey_edit_export_all.setKeySequence(QKeySequence("Ctrl+Shift+E"))
                hotkey_edit_refresh.setKeySequence(QKeySequence.StandardKey.Refresh)
                hotkey_edit_properties.setKeySequence(QKeySequence("Alt+Return"))
                hotkey_edit_find.setKeySequence(QKeySequence.StandardKey.Find)
                hotkey_edit_help.setKeySequence(QKeySequence.StandardKey.HelpContents)

        reset_hotkeys_btn.clicked.connect(reset_hotkeys)
        reset_layout.addWidget(reset_hotkeys_btn)
        hotkeys_layout.addLayout(reset_layout)

        tabs.addTab(hotkeys_tab, "Keyboard Shortcuts")

        # Add tabs widget to main layout
        layout.addWidget(tabs)

        # Dialog buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        def apply_settings(close_dialog=False):  #vers 2
            """Apply all settings"""
            # Apply display settings
            self.thumbnail_size = thumb_size_spin.value()
            self.table_row_height = row_height_spin.value()
            self.show_grid_lines = show_grid_check.isChecked()

            self.show_status_bar = show_status_bar_check.isChecked()
            self.relay_status_to_img_factory = relay_status_check.isChecked()
            if hasattr(self, 'status_frame'):
                self.status_frame.setVisible(
                    self.show_status_bar and self.standalone_mode)
            self._push_status_to_img_factory()

            # Apply preview settings
            self.show_preview_default = show_preview_check.isChecked()
            self.auto_refresh_preview = auto_refresh_check.isChecked()
            self.preview_width = preview_width_spin.value()
            self.preview_height = preview_height_spin.value()
            self.preview_background = bg_combo.currentText()
            self.preview_fit_to_window = fit_to_window_check.isChecked()
            self.preview_smooth_scaling = smooth_zoom_check.isChecked()

            # Apply export settings
            self.default_export_format = format_combo.currentText()
            self.export_preserve_alpha = preserve_alpha_check.isChecked()
            self.export_mipmaps_separate = export_mipmaps_check.isChecked()
            self.export_create_subfolders = create_subfolders_check.isChecked()

            # Apply import settings
            self.import_auto_name = auto_name_check.isChecked()
            self.import_replace_existing = replace_check.isChecked()
            self.import_auto_format = auto_format_check.isChecked()
            self.default_import_format = import_format_combo.currentText()

            # Apply constraint settings
            self.dimension_limiting_enabled = dimension_check.isChecked()
            self.splash_screen_mode = splash_check.isChecked()
            self.custom_max_dimension = max_dim_spin.value()
            self.name_limit_enabled = name_limit_check.isChecked()
            self.max_texture_name_length = char_limit_spin.value()
            self.iff_import_enabled = iff_check.isChecked()

            # Apply hotkeys
            if hasattr(self, 'hotkey_open'):
                self.hotkey_open.setKey(hotkey_edit_open.keySequence())
            if hasattr(self, 'hotkey_save'):
                self.hotkey_save.setKey(hotkey_edit_save.keySequence())
            if hasattr(self, 'hotkey_force_save'):
                self.hotkey_force_save.setKey(hotkey_edit_force_save.keySequence())
            if hasattr(self, 'hotkey_save_as'):
                self.hotkey_save_as.setKey(hotkey_edit_save_as.keySequence())
            if hasattr(self, 'hotkey_close'):
                self.hotkey_close.setKey(hotkey_edit_close.keySequence())
            if hasattr(self, 'hotkey_undo'):
                self.hotkey_undo.setKey(hotkey_edit_undo.keySequence())
            if hasattr(self, 'hotkey_copy'):
                self.hotkey_copy.setKey(hotkey_edit_copy.keySequence())
            if hasattr(self, 'hotkey_paste'):
                self.hotkey_paste.setKey(hotkey_edit_paste.keySequence())
            if hasattr(self, 'hotkey_delete'):
                self.hotkey_delete.setKey(hotkey_edit_delete.keySequence())
            if hasattr(self, 'hotkey_duplicate'):
                self.hotkey_duplicate.setKey(hotkey_edit_duplicate.keySequence())
            if hasattr(self, 'hotkey_rename'):
                self.hotkey_rename.setKey(hotkey_edit_rename.keySequence())
            if hasattr(self, 'hotkey_import'):
                self.hotkey_import.setKey(hotkey_edit_import.keySequence())
            if hasattr(self, 'hotkey_export'):
                self.hotkey_export.setKey(hotkey_edit_export.keySequence())
            if hasattr(self, 'hotkey_export_all'):
                self.hotkey_export_all.setKey(hotkey_edit_export_all.keySequence())
            if hasattr(self, 'hotkey_refresh'):
                self.hotkey_refresh.setKey(hotkey_edit_refresh.keySequence())
            if hasattr(self, 'hotkey_properties'):
                self.hotkey_properties.setKey(hotkey_edit_properties.keySequence())
            if hasattr(self, 'hotkey_find'):
                self.hotkey_find.setKey(hotkey_edit_find.keySequence())
            if hasattr(self, 'hotkey_help'):
                self.hotkey_help.setKey(hotkey_edit_help.keySequence())

            # Refresh UI with new settings
            if hasattr(self, '_reload_texture_table'):
                self._reload_texture_table()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Settings applied")

            if close_dialog:
                dialog.accept()

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(lambda: apply_settings(close_dialog=False))
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(lambda: apply_settings(close_dialog=True))
        button_layout.addWidget(ok_btn)

        layout.addLayout(button_layout)

        dialog.exec()


    def _show_shaders_dialog(self): #vers 2
        """Show viewport shader presets — applies display-only visual effects."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout,
            QLabel, QPushButton, QButtonGroup, QRadioButton)

        dlg = QDialog(self)
        dlg.setWindowTitle("Preview Shaders")
        dlg.setFixedSize(300, 220)
        lay = QVBoxLayout(dlg)
        lay.addWidget(QLabel("<b>Display shader (preview only, does not modify texture)</b>"))

        presets = [
            ("None",       "normal"),
            ("Greyscale",  "greyscale"),
            ("Sepia",      "sepia"),
            ("Invert",     "invert"),
            ("Sharpen",    "sharpen"),
        ]
        grp = QButtonGroup(dlg)
        current = getattr(self, '_preview_shader', 'normal')
        for label, key in presets:
            rb = QRadioButton(label)
            rb.setChecked(key == current)
            rb.toggled.connect(lambda checked, k=key: (
                setattr(self, '_preview_shader', k) or (
                    self._update_texture_info(self.selected_texture)
                    if checked and self.selected_texture else None
                )
            ))
            grp.addButton(rb)
            lay.addWidget(rb)

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dlg.accept)
        lay.addWidget(close_btn)
        dlg.exec()


    # STUB: dock_btn/tearoff_btn icons do not yet update on theme change
    # Remaining: dock_btn, tearoff_btn, colour swatch buttons, some info-panel
    # labels/combos, and any icons created via _svg_to_icon() rather than
    # icon_factory. Address in a future pass.




    def _close_txd_tab(self, index): #vers 2
        """Close TXD tab"""
        if self.txd_tabs.count() <= 1:
            QMessageBox.warning(self, "Cannot Close", "Cannot close the last tab")
            return
        if not self._confirm_discard():
            return
        self.txd_tabs.removeTab(index)

    def closeEvent(self, event): #vers 1
        """Ask before closing with unsaved changes."""
        if not self._confirm_discard():
            event.ignore()
            return
        super().closeEvent(event)


    def _switch_txd_tab(self, index): #vers 1
        """Switch to different TXD tab"""
        if index < 0:
            return

        tab_name = self.txd_tabs.tabText(index)
        tab_widget = self.txd_tabs.widget(index)
        if tab_widget and hasattr(tab_widget, 'refresh'):
            tab_widget.refresh()

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"Switched to tab: {tab_name}")




def open_txd_workshop(main_window, img_path=None): #vers 6
    """Open TXD Workshop - embedded in tab if main_window has tab widget, standalone otherwise"""
    try:
        from PyQt6.QtWidgets import QVBoxLayout, QWidget

        # Standalone mode - no main window or no tab widget
        if not main_window or not hasattr(main_window, 'main_tab_widget'):
            workshop = TXDWorkshop(None, main_window)
            workshop.setWindowFlags(Qt.WindowType.Window)
            if img_path:
                if img_path.lower().endswith('.txd'):
                    workshop.open_txd_file(img_path)
                else:
                    workshop.load_from_img_archive(img_path)
            elif main_window:
                # No explicit file - load from current IMG if available, so
                # opening TXD Workshop with an IMG already open picks it up
                # automatically, same as when a path is given explicitly.
                img = getattr(main_window, 'current_img', None)
                if img:
                    fp = getattr(img, 'file_path', '') or ''
                    if fp and os.path.isfile(fp):
                        workshop.load_from_img_archive(fp)
            workshop.resize(1200, 800)
            workshop.show()
            return workshop

        # Embedded mode - add as tab
        import os
        tab_container = QWidget()
        tab_layout = QVBoxLayout(tab_container)
        tab_layout.setContentsMargins(0, 0, 0, 0)

        workshop = TXDWorkshop(tab_container, main_window)
        workshop.setWindowFlags(Qt.WindowType.Widget)
        tab_layout.addWidget(workshop)

        if img_path:
            if img_path.lower().endswith('.txd'):
                workshop.open_txd_file(img_path)
            else:
                workshop.load_from_img_archive(img_path)
        elif main_window:
            # No explicit file - load from current IMG if available
            img = getattr(main_window, 'current_img', None)
            if img:
                fp = getattr(img, 'file_path', '') or ''
                if fp and os.path.isfile(fp):
                    workshop.load_from_img_archive(fp)

        tab_label = os.path.splitext(os.path.basename(img_path))[0] if img_path else App_name
        # Strip GTA streaming suffix (e.g. barracks_hli9ksta -> barracks)
        import re as _re
        tab_label = _re.sub(r'_[a-z0-9]{6,12}$', '', tab_label)
        try:
            from apps.methods.imgfactory_svg_icons import get_txd_file_icon
            icon = get_txd_file_icon()
            idx = main_window.main_tab_widget.addTab(tab_container, icon, tab_label)
        except Exception:
            idx = main_window.main_tab_widget.addTab(tab_container, tab_label)
        main_window.main_tab_widget.setCurrentIndex(idx)
        if hasattr(main_window, '_ensure_tab_area_visible'):
            main_window._ensure_tab_area_visible()

        workshop.show()
        return workshop

    except Exception as e:
        if main_window:
            QMessageBox.critical(main_window, "Error", f"Failed to open {App_name}: {str(e)}")
        else:
            print(f"Failed to open {App_name}: {str(e)}")
        return None



if __name__ == "__main__":
    import sys
    import traceback

    print("Starting " + App_name + "...")

    try:
        app = QApplication(sys.argv)
        print("QApplication created")

        workshop = TXDWorkshop()
        print(App_name + " instance created")

        workshop.setWindowTitle(App_name + " - Standalone")
        workshop.resize(1200, 800)
        workshop.show()
        print("Window shown, entering event loop")
        print(f"Window visible: {workshop.isVisible()}")
        print(f"Window geometry: {workshop.geometry()}")

        sys.exit(app.exec())

    except Exception as e:
        img_debugger.error(f"{e}")
        traceback.print_exc()
        sys.exit(1)
