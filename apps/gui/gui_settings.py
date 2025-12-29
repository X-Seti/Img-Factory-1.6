#this belongs in gui/ gui_settings.py - version 2
# X-Seti - July05 2025 - IMG Factory 1.5 - Complete GUI Settings with Tab Height Controls
# Credit MexUK 2007 Img Factory 1.2

#!/usr/bin/env python3
"""
IMG Factory GUI Settings Dialog
Comprehensive GUI customization options
"""

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTabWidget, QWidget,
    QGroupBox, QLabel, QSpinBox, QCheckBox, QComboBox, 
    QPushButton, QSlider, QColorDialog, QFontDialog,
    QMessageBox, QGridLayout, QFrame, QButtonGroup,
    QRadioButton, QLineEdit, QTextEdit, QListWidget,
    QListWidgetItem, QSplitter, QScrollArea, QFormLayout,
    QApplication
)
from pathlib import Path
from PyQt6.QtCore import Qt, pyqtSignal
from PyQt6.QtGui import QFont, QColor, QPalette, QIcon
import os

print(f"[DEBUG] gui_settings calling: with args={Path}")

class GUISettingsDialog(QDialog):
    """Comprehensive GUI Settings Dialog"""
    
    settings_changed = pyqtSignal()
    theme_changed = pyqtSignal(str)
    
    def __init__(self, app_settings, parent=None):
        super().__init__(parent)
        self.app_settings = app_settings
        self.setWindowTitle("🖥️ GUI Settings - IMG Factory 1.5")
        self.setMinimumSize(600, 500)
        self.resize(700, 600)
        
        self._create_ui()
        self._load_current_settings()
        #integrate_settings_menu(self)
        #integrate_color_ui_system(self)

    def _safe_load_icon(self, icon_name):
        """Safely load icon with fallback - ADDED TO FIX QIcon.fromTheme issues"""
        try:
            # Try theme icon first
            theme_icon = QIcon.fromTheme(icon_name)
            if theme_icon and not theme_icon.isNull():
                return theme_icon
        except Exception:
            pass
        
        # Try local icon paths
        try:
            icon_paths = [
                f"icons/{icon_name}.png",
                f"gui/icons/{icon_name}.png",
                f"resources/icons/{icon_name}.png"
            ]
            
            for path in icon_paths:
                if os.path.exists(path):
                    local_icon = QIcon(path)
                    if local_icon and not local_icon.isNull():
                        return local_icon
        except Exception:
            pass
        
        # Return empty icon as fallback
        return QIcon()
    
    def _create_ui(self):
        """Create the settings UI with tabs"""
        layout = QVBoxLayout(self)
        
        # Tab widget for different setting categories
        self.tab_widget = QTabWidget()
        
        # Create tabs
        self.tab_widget.addTab(self._create_appearance_tab(), "🎨 Appearance")
        self.tab_widget.addTab(self._create_layout_tab(), "📐 Layout")
        self.tab_widget.addTab(self._create_tabs_tab(), "📑 Tabs")
        self.tab_widget.addTab(self._create_fonts_tab(), "🔤 Fonts")
        self.tab_widget.addTab(self._create_icons_tab(), "Icons")
        self.tab_widget.addTab(self._create_behavior_tab(), "⚙️ Behavior")
        self.tab_widget.addTab(self._create_game_format_tab(), "🎮 Game Format")
        
        layout.addWidget(self.tab_widget)
        
        # Button bar
        button_layout = QHBoxLayout()
        
        # Reset to defaults
        reset_btn = QPushButton("🔄 Reset to Defaults")
        reset_btn.clicked.connect(self._reset_to_defaults)
        button_layout.addWidget(reset_btn)
        
        button_layout.addStretch()
        
        # Cancel
        cancel_btn = QPushButton("❌ Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)
        
        # Apply
        apply_btn = QPushButton("✓ Apply")
        apply_btn.clicked.connect(self._apply_settings)
        button_layout.addWidget(apply_btn)
        
        # OK
        ok_btn = QPushButton("✓ OK")
        ok_btn.clicked.connect(self._save_and_close)
        ok_btn.setDefault(True)
        button_layout.addWidget(ok_btn)
        
        layout.addLayout(button_layout)
    
    def _create_appearance_tab(self) -> QWidget:
        """Create appearance settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Theme selection
        theme_group = QGroupBox("🎨 Theme Selection")
        theme_layout = QVBoxLayout(theme_group)
        
        self.theme_combo = QComboBox()
        self.theme_combo.addItems([
            "IMG Factory Default",
            "Dark Theme", 
            "Light Professional",
            "GTA San Andreas",
            "GTA Vice City",
            "LCARS (Star Trek)",
            "Cyberpunk 2077",
            "Matrix",
            "Synthwave",
            "Amiga Workbench"
        ])
        theme_layout.addWidget(QLabel("Select Theme:"))
        theme_layout.addWidget(self.theme_combo)
        
        layout.addWidget(theme_group)
        
        # Color customization
        color_group = QGroupBox("🌈 Color Customization")
        color_layout = QGridLayout(color_group)
        
        # Background color
        color_layout.addWidget(QLabel("Background Color:"), 0, 0)
        self.bg_color_btn = QPushButton("Choose Color...")
        self.bg_color_btn.clicked.connect(lambda: self._choose_color('background'))
        color_layout.addWidget(self.bg_color_btn, 0, 1)
        
        # Text color
        color_layout.addWidget(QLabel("Text Color:"), 1, 0)
        self.text_color_btn = QPushButton("Choose Color...")
        self.text_color_btn.clicked.connect(lambda: self._choose_color('text'))
        color_layout.addWidget(self.text_color_btn, 1, 1)
        
        # Accent color
        color_layout.addWidget(QLabel("Accent Color:"), 2, 0)
        self.accent_color_btn = QPushButton("Choose Color...")
        self.accent_color_btn.clicked.connect(lambda: self._choose_color('accent'))
        color_layout.addWidget(self.accent_color_btn, 2, 1)
        
        layout.addWidget(color_group)
        
        # Visual effects
        effects_group = QGroupBox("✨ Visual Effects")
        effects_layout = QVBoxLayout(effects_group)
        
        self.animations_check = QCheckBox("Enable animations")
        self.shadows_check = QCheckBox("Enable shadows")
        self.transparency_check = QCheckBox("Enable transparency effects")
        self.rounded_corners_check = QCheckBox("Rounded corners")
        
        effects_layout.addWidget(self.animations_check)
        effects_layout.addWidget(self.shadows_check)
        effects_layout.addWidget(self.transparency_check)
        effects_layout.addWidget(self.rounded_corners_check)
        
        layout.addWidget(effects_group)
        layout.addStretch()
        
        return widget
    
    def _create_layout_tab(self) -> QWidget:
        """Create layout settings tab - REVERTED to original without tab settings"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Panel sizes
        panel_group = QGroupBox("📏 Panel Sizes")
        panel_layout = QGridLayout(panel_group)
        
        # Left panel width
        panel_layout.addWidget(QLabel("Left Panel Width:"), 0, 0)
        self.left_panel_spin = QSpinBox()
        self.left_panel_spin.setRange(200, 800)
        self.left_panel_spin.setValue(600)
        self.left_panel_spin.setSuffix(" px")
        panel_layout.addWidget(self.left_panel_spin, 0, 1)
        
        # Right panel width
        panel_layout.addWidget(QLabel("Right Panel Width:"), 1, 0)
        self.right_panel_spin = QSpinBox()
        self.right_panel_spin.setRange(180, 600)
        self.right_panel_spin.setValue(280)
        self.right_panel_spin.setSuffix(" px")
        panel_layout.addWidget(self.right_panel_spin, 1, 1)
        
        # Table row height
        panel_layout.addWidget(QLabel("Table Row Height:"), 2, 0)
        self.row_height_spin = QSpinBox()
        self.row_height_spin.setRange(20, 60)
        self.row_height_spin.setValue(25)
        self.row_height_spin.setSuffix(" px")
        panel_layout.addWidget(self.row_height_spin, 2, 1)
        
        layout.addWidget(panel_group)
        
        # Spacing and margins
        spacing_group = QGroupBox("📐 Spacing & Margins")
        spacing_layout = QGridLayout(spacing_group)
        
        # Widget spacing
        spacing_layout.addWidget(QLabel("Widget Spacing:"), 0, 0)
        self.widget_spacing_spin = QSpinBox()
        self.widget_spacing_spin.setRange(2, 20)
        self.widget_spacing_spin.setValue(5)
        self.widget_spacing_spin.setSuffix(" px")
        spacing_layout.addWidget(self.widget_spacing_spin, 0, 1)
        
        # Layout margins
        spacing_layout.addWidget(QLabel("Layout Margins:"), 1, 0)
        self.layout_margins_spin = QSpinBox()
        self.layout_margins_spin.setRange(0, 30)
        self.layout_margins_spin.setValue(5)
        self.layout_margins_spin.setSuffix(" px")
        spacing_layout.addWidget(self.layout_margins_spin, 1, 1)
        
        layout.addWidget(spacing_group)
        
        # Window behavior
        window_group = QGroupBox("🪟 Window Behavior")
        window_layout = QVBoxLayout(window_group)
        
        self.remember_size_check = QCheckBox("Remember window size")
        self.remember_position_check = QCheckBox("Remember window position")
        self.maximize_on_startup_check = QCheckBox("Maximize on startup")
        self.always_on_top_check = QCheckBox("Keep window always on top")
        
        window_layout.addWidget(self.remember_size_check)
        window_layout.addWidget(self.remember_position_check)
        window_layout.addWidget(self.maximize_on_startup_check)
        window_layout.addWidget(self.always_on_top_check)
        
        layout.addWidget(window_group)
        layout.addStretch()
        
        return widget
    
    def _create_tabs_tab(self) -> QWidget:
        """Create dedicated tabs settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Tab Settings Group - DEDICATED TAB
        tab_group = QGroupBox("📑 Tab Configuration")
        tab_layout = QGridLayout(tab_group)
        
        # Main tab height
        tab_layout.addWidget(QLabel("Main Tab Height:"), 0, 0)
        self.main_tab_height_spin = QSpinBox()
        self.main_tab_height_spin.setRange(20, 60)
        self.main_tab_height_spin.setValue(35)
        self.main_tab_height_spin.setSuffix(" px")
        self.main_tab_height_spin.setToolTip("Height of the main IMG/COL/TXD tabs")
        tab_layout.addWidget(self.main_tab_height_spin, 0, 1)
        
        # Individual tab height
        tab_layout.addWidget(QLabel("Individual Tab Height:"), 1, 0)
        self.individual_tab_height_spin = QSpinBox()
        self.individual_tab_height_spin.setRange(16, 30)
        self.individual_tab_height_spin.setValue(24)
        self.individual_tab_height_spin.setSuffix(" px")
        self.individual_tab_height_spin.setToolTip("Height of individual tab buttons")
        tab_layout.addWidget(self.individual_tab_height_spin, 1, 1)
        
        # Tab font size
        tab_layout.addWidget(QLabel("Tab Font Size:"), 2, 0)
        self.tab_font_size_spin = QSpinBox()
        self.tab_font_size_spin.setRange(7, 14)
        self.tab_font_size_spin.setValue(9)
        self.tab_font_size_spin.setSuffix(" pt")
        self.tab_font_size_spin.setToolTip("Font size for tab text")
        tab_layout.addWidget(self.tab_font_size_spin, 2, 1)
        
        # Tab padding
        tab_layout.addWidget(QLabel("Tab Padding:"), 3, 0)
        self.tab_padding_spin = QSpinBox()
        self.tab_padding_spin.setRange(2, 12)
        self.tab_padding_spin.setValue(4)
        self.tab_padding_spin.setSuffix(" px")
        self.tab_padding_spin.setToolTip("Padding inside tab buttons")
        tab_layout.addWidget(self.tab_padding_spin, 3, 1)
        
        # Tab container height
        tab_layout.addWidget(QLabel("Tab Container Height:"), 4, 0)
        self.tab_container_height_spin = QSpinBox()
        self.tab_container_height_spin.setRange(30, 80)
        self.tab_container_height_spin.setValue(30)
        self.tab_container_height_spin.setSuffix(" px")
        self.tab_container_height_spin.setToolTip("Total height of the tab container section")
        tab_layout.addWidget(self.tab_container_height_spin, 4, 1)
        
        layout.addWidget(tab_group)
        
        # Tab Style Presets
        style_group = QGroupBox("🎨 Tab Style Presets")
        style_layout = QVBoxLayout(style_group)
        
        # Style selector
        style_selector_layout = QHBoxLayout()
        style_selector_layout.addWidget(QLabel("Quick Style:"))
        self.tab_style_combo = QComboBox()
        self.tab_style_combo.addItems(["Compact", "Standard", "Large"])
        self.tab_style_combo.setCurrentText("Compact")
        self.tab_style_combo.setToolTip("Preset tab size configurations")
        style_selector_layout.addWidget(self.tab_style_combo)
        style_selector_layout.addStretch()
        
        style_layout.addLayout(style_selector_layout)
        
        # Style descriptions
        style_descriptions = QTextEdit()
        style_descriptions.setMaximumHeight(80)
        style_descriptions.setReadOnly(True)
        style_descriptions.setPlainText(
            "• Compact: Space-saving tabs (35px height, 9pt font)\n"
            "• Standard: Balanced appearance (45px height, 10pt font)\n"
            "• Large: Accessibility-friendly (55px height, 11pt font)"
        )
        style_layout.addWidget(style_descriptions)
        
        layout.addWidget(style_group)
        
        # Preview and Actions
        actions_group = QGroupBox("🛠️ Actions")
        actions_layout = QVBoxLayout(actions_group)
        
        # Preview button
        preview_btn = QPushButton("👀 Preview Tab Changes")
        preview_btn.clicked.connect(self._preview_tab_changes)
        preview_btn.setMinimumHeight(35)
        actions_layout.addWidget(preview_btn)
        
        # Reset to defaults button
        reset_tabs_btn = QPushButton("🔄 Reset Tab Settings to Defaults")
        reset_tabs_btn.clicked.connect(self._reset_tab_settings)
        reset_tabs_btn.setMinimumHeight(35)
        actions_layout.addWidget(reset_tabs_btn)
        
        layout.addWidget(actions_group)
        
        # Connect tab style combo to update spinboxes
        self.tab_style_combo.currentTextChanged.connect(self._apply_tab_style_preset)
        
        layout.addStretch()
        
        return widget
    
    def _create_fonts_tab(self) -> QWidget:
        """Create fonts settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Font categories
        font_group = QGroupBox("🔤 Font Settings")
        font_layout = QGridLayout(font_group)
        
        # Main interface font
        font_layout.addWidget(QLabel("Main Interface Font:"), 0, 0)
        self.main_font_btn = QPushButton("Select Font...")
        self.main_font_btn.clicked.connect(lambda: self._choose_font('main'))
        font_layout.addWidget(self.main_font_btn, 0, 1)
        
        # Table font
        font_layout.addWidget(QLabel("Table Font:"), 1, 0)
        self.table_font_btn = QPushButton("Select Font...")
        self.table_font_btn.clicked.connect(lambda: self._choose_font('table'))
        font_layout.addWidget(self.table_font_btn, 1, 1)
        
        # Menu font
        font_layout.addWidget(QLabel("Menu Font:"), 2, 0)
        self.menu_font_btn = QPushButton("Select Font...")
        self.menu_font_btn.clicked.connect(lambda: self._choose_font('menu'))
        font_layout.addWidget(self.menu_font_btn, 2, 1)
        
        layout.addWidget(font_group)
        
        # Font size scaling
        size_group = QGroupBox("📏 Font Size Scaling")
        size_layout = QVBoxLayout(size_group)
        
        size_layout.addWidget(QLabel("Global Font Size Scale:"))
        self.font_scale_slider = QSlider(Qt.Orientation.Horizontal)
        self.font_scale_slider.setRange(75, 150)
        self.font_scale_slider.setValue(100)
        self.font_scale_slider.valueChanged.connect(self._update_font_scale_label)
        
        self.font_scale_label = QLabel("100%")
        self.font_scale_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        
        scale_layout = QHBoxLayout()
        scale_layout.addWidget(QLabel("75%"))
        scale_layout.addWidget(self.font_scale_slider)
        scale_layout.addWidget(QLabel("150%"))
        
        size_layout.addLayout(scale_layout)
        size_layout.addWidget(self.font_scale_label)
        
        layout.addWidget(size_group)
        
        # Text options
        text_group = QGroupBox("📝 Text Options")
        text_layout = QVBoxLayout(text_group)
        
        self.antialiasing_check = QCheckBox("Enable font antialiasing")
        self.bold_headers_check = QCheckBox("Bold table headers")
        self.monospace_numbers_check = QCheckBox("Use monospace for numbers")
        
        text_layout.addWidget(self.antialiasing_check)
        text_layout.addWidget(self.bold_headers_check)
        text_layout.addWidget(self.monospace_numbers_check)
        
        layout.addWidget(text_group)
        layout.addStretch()
        
        return widget
    
    def _create_icons_tab(self) -> QWidget:
        """Create icons settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Icon display
        display_group = QGroupBox("Icon Display")
        display_layout = QVBoxLayout(display_group)
        
        self.show_menu_icons_check = QCheckBox("Show icons in menus")
        self.show_toolbar_icons_check = QCheckBox("Show toolbar icons")
        self.show_button_icons_check = QCheckBox("Show icons on buttons")
        self.show_file_type_icons_check = QCheckBox("Show file type icons in table")
        
        display_layout.addWidget(self.show_menu_icons_check)
        display_layout.addWidget(self.show_toolbar_icons_check)
        display_layout.addWidget(self.show_button_icons_check)
        display_layout.addWidget(self.show_file_type_icons_check)
        
        layout.addWidget(display_group)
        
        # Icon size
        size_group = QGroupBox("📏 Icon Sizes")
        size_layout = QGridLayout(size_group)
        
        # Menu icon size
        size_layout.addWidget(QLabel("Menu Icon Size:"), 0, 0)
        self.menu_icon_size_combo = QComboBox()
        self.menu_icon_size_combo.addItems(["16px", "20px", "24px", "32px"])
        size_layout.addWidget(self.menu_icon_size_combo, 0, 1)
        
        # Toolbar icon size
        size_layout.addWidget(QLabel("Toolbar Icon Size:"), 1, 0)
        self.toolbar_icon_size_combo = QComboBox()
        self.toolbar_icon_size_combo.addItems(["16px", "20px", "24px", "32px", "48px"])
        size_layout.addWidget(self.toolbar_icon_size_combo, 1, 1)
        
        # Button icon size
        size_layout.addWidget(QLabel("Button Icon Size:"), 2, 0)
        self.button_icon_size_combo = QComboBox()
        self.button_icon_size_combo.addItems(["16px", "20px", "24px", "32px"])
        size_layout.addWidget(self.button_icon_size_combo, 2, 1)
        
        layout.addWidget(size_group)
        
        # Icon style
        style_group = QGroupBox("🎨 Icon Style")
        style_layout = QVBoxLayout(style_group)
        
        self.icon_style_group = QButtonGroup()
        
        style_auto = QRadioButton("Automatic (match theme)")
        style_mono = QRadioButton("Monochrome")
        style_color = QRadioButton("Full color")
        style_outline = QRadioButton("Outline style")
        
        self.icon_style_group.addButton(style_auto, 0)
        self.icon_style_group.addButton(style_mono, 1)
        self.icon_style_group.addButton(style_color, 2)
        self.icon_style_group.addButton(style_outline, 3)
        
        style_layout.addWidget(style_auto)
        style_layout.addWidget(style_mono)
        style_layout.addWidget(style_color)
        style_layout.addWidget(style_outline)
        
        layout.addWidget(style_group)
        layout.addStretch()
        
        return widget
    
    def _create_behavior_tab(self) -> QWidget:
        """Create behavior settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Interface behavior
        interface_group = QGroupBox("🖱️ Interface Behavior")
        interface_layout = QVBoxLayout(interface_group)
        
        self.double_click_open_check = QCheckBox("Double-click to open files")
        self.single_click_select_check = QCheckBox("Single-click to select")
        self.hover_preview_check = QCheckBox("Show preview on hover")
        self.auto_resize_columns_check = QCheckBox("Auto-resize table columns")
        
        interface_layout.addWidget(self.double_click_open_check)
        interface_layout.addWidget(self.single_click_select_check)
        interface_layout.addWidget(self.hover_preview_check)
        interface_layout.addWidget(self.auto_resize_columns_check)
        
        layout.addWidget(interface_group)
        
        # Performance
        performance_group = QGroupBox("⚡ Performance")
        performance_layout = QVBoxLayout(performance_group)
        
        self.lazy_loading_check = QCheckBox("Enable lazy loading for large files")
        self.cache_thumbnails_check = QCheckBox("Cache file thumbnails")
        self.preload_common_files_check = QCheckBox("Preload common file types")
        
        performance_layout.addWidget(self.lazy_loading_check)
        performance_layout.addWidget(self.cache_thumbnails_check)
        performance_layout.addWidget(self.preload_common_files_check)
        
        layout.addWidget(performance_group)
        
        # Notifications
        notifications_group = QGroupBox("🔔 Notifications")
        notifications_layout = QVBoxLayout(notifications_group)
        
        self.show_notifications_check = QCheckBox("Show system notifications")
        self.sound_notifications_check = QCheckBox("Play sound for notifications")
        self.progress_notifications_check = QCheckBox("Show progress notifications")
        
        notifications_layout.addWidget(self.show_notifications_check)
        notifications_layout.addWidget(self.sound_notifications_check)
        notifications_layout.addWidget(self.progress_notifications_check)
        
        layout.addWidget(notifications_group)
        
        # Game Format Settings
        format_group = QGroupBox("🎮 Game Format Settings")
        format_layout = QVBoxLayout(format_group)
        
        format_layout.addWidget(QLabel("IMG Name Length Limit:"))
        self.img_name_format_combo = QComboBox()
        self.img_name_format_combo.addItems([
            "GTA III/VC (23 chars)", 
            "GTASA (32 chars)", 
            "GTASOL (32 chars)"
        ])
        self.img_name_format_combo.setToolTip("Set maximum character limit for IMG entry names based on game format")
        format_layout.addWidget(self.img_name_format_combo)
        
        layout.addWidget(format_group)
        layout.addStretch()
        
        return widget
    
    def _save_and_close(self):
        """Save settings and close dialog"""
        self._save_settings()
        self.settings_changed.emit()
        self.accept()
    
    def _create_appearance_tab(self) -> QWidget:
        """Create appearance settings tab with both theme options and button display options"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Theme selection
        theme_group = QGroupBox("🎨 Theme Selection")
        theme_layout = QVBoxLayout(theme_group)
        
        self.theme_combo = QComboBox()
        self.theme_combo.addItems([
            "IMG Factory Default",
            "Dark Theme", 
            "Light Professional",
            "GTA San Andreas",
            "GTA Vice City",
            "LCARS (Star Trek)",
            "Cyberpunk 2077",
            "Matrix",
            "Synthwave",
            "Amiga Workbench"
        ])
        theme_layout.addWidget(QLabel("Select Theme:"))
        theme_layout.addWidget(self.theme_combo)
        
        layout.addWidget(theme_group)
        
        # Button display options
        button_display_group = QGroupBox("ButtonTitles & Icons")
        button_display_layout = QVBoxLayout(button_display_group)
        
        self.button_display_group = QButtonGroup()
        
        self.both_radio = QRadioButton("Show Both Icons and Text")
        self.icons_radio = QRadioButton("Show Icons Only")
        self.text_radio = QRadioButton("Show Text Only")
        
        self.button_display_group.addButton(self.both_radio, 0)
        self.button_display_group.addButton(self.icons_radio, 1)
        self.button_display_group.addButton(self.text_radio, 2)
        
        button_display_layout.addWidget(self.both_radio)
        button_display_layout.addWidget(self.icons_radio)
        button_display_layout.addWidget(self.text_radio)
        
        layout.addWidget(button_display_group)
        
        # Color customization
        color_group = QGroupBox("🌈 Color Customization")
        color_layout = QGridLayout(color_group)
        
        # Background color
        color_layout.addWidget(QLabel("Background Color:"), 0, 0)
        self.bg_color_btn = QPushButton("Choose Color...")
        self.bg_color_btn.clicked.connect(lambda: self._choose_color('background'))
        color_layout.addWidget(self.bg_color_btn, 0, 1)
        
        # Text color
        color_layout.addWidget(QLabel("Text Color:"), 1, 0)
        self.text_color_btn = QPushButton("Choose Color...")
        self.text_color_btn.clicked.connect(lambda: self._choose_color('text'))
        color_layout.addWidget(self.text_color_btn, 1, 1)
        
        # Accent color
        color_layout.addWidget(QLabel("Accent Color:"), 2, 0)
        self.accent_color_btn = QPushButton("Choose Color...")
        self.accent_color_btn.clicked.connect(lambda: self._choose_color('accent'))
        color_layout.addWidget(self.accent_color_btn, 2, 1)
        
        layout.addWidget(color_group)
        
        # Visual effects
        effects_group = QGroupBox("✨ Visual Effects")
        effects_layout = QVBoxLayout(effects_group)
        
        self.animations_check = QCheckBox("Enable animations")
        self.shadows_check = QCheckBox("Enable shadows")
        self.transparency_check = QCheckBox("Enable transparency effects")
        self.rounded_corners_check = QCheckBox("Rounded corners")
        
        effects_layout.addWidget(self.animations_check)
        effects_layout.addWidget(self.shadows_check)
        effects_layout.addWidget(self.transparency_check)
        effects_layout.addWidget(self.rounded_corners_check)
        
        layout.addWidget(effects_group)
        layout.addStretch()
        
        return widget
    def _load_current_settings(self):
        """Load current settings into the UI"""
        # Load theme
        current_theme = self.app_settings.current_settings.get("theme", "IMG Factory Default")
        theme_index = self.theme_combo.findText(current_theme)
        if theme_index >= 0:
            self.theme_combo.setCurrentIndex(theme_index)
        
        # Load button display mode
        button_mode = self.app_settings.current_settings.get("button_display_mode", "both")
        if button_mode == "icons":
            self.icons_radio.setChecked(True)
        elif button_mode == "text":
            self.text_radio.setChecked(True)
        else:  # "both"
            self.both_radio.setChecked(True)
        
        # Load visual effects settings
        self.animations_check.setChecked(self.app_settings.current_settings.get("enable_animations", True))
        self.shadows_check.setChecked(self.app_settings.current_settings.get("enable_shadows", True))
        self.transparency_check.setChecked(self.app_settings.current_settings.get("enable_transparency", True))
        self.rounded_corners_check.setChecked(self.app_settings.current_settings.get("enable_rounded_corners", True))

    def _apply_settings(self):
        """Apply settings temporarily without saving"""
        # Apply theme
        new_theme = self.theme_combo.currentText()
        self.app_settings.current_settings["theme"] = new_theme
        
        # Apply button display mode
        if self.icons_radio.isChecked():
            button_mode = "icons"
        elif self.text_radio.isChecked():
            button_mode = "text"
        else:  # both
            button_mode = "both"
        self.app_settings.current_settings["button_display_mode"] = button_mode
        
        # Apply visual effects settings
        self.app_settings.current_settings["enable_animations"] = self.animations_check.isChecked()
        self.app_settings.current_settings["enable_shadows"] = self.shadows_check.isChecked()
        self.app_settings.current_settings["enable_transparency"] = self.transparency_check.isChecked()
        self.app_settings.current_settings["enable_rounded_corners"] = self.rounded_corners_check.isChecked()
        
        # Apply changes to the main window
        self._apply_theme_to_main_window()
        
        # Emit signal to notify about changes
        self.settings_changed.emit()
        if new_theme != self.app_settings.current_settings.get("theme", "IMG Factory Default"):
            self.theme_changed.emit(new_theme)

    def _apply_theme_to_main_window(self):
        """Apply theme settings to the main window"""
        # Get the main window from parent
        parent = self.parent()
        if parent:
            # Apply unified button theme
            from apps.gui.unified_button_theme import apply_unified_button_theme
            apply_unified_button_theme(parent, self.app_settings)
            
            # Apply button display mode
            self._apply_button_display_mode()
            
            # Apply stylesheet
            stylesheet = self.app_settings.get_stylesheet()
            parent.setStyleSheet(stylesheet)
            
            # Force update to ensure visual changes are applied
            parent.update()
            QApplication.processEvents()  # Process any pending events

    def _apply_button_display_mode(self):
        """Apply button display mode to all buttons in the main window"""
        parent = self.parent()
        if parent and hasattr(parent, "right_panel"):
            # Update button appearance in right panel
            self._update_buttons_in_panel(parent.right_panel)
            
            # Update button appearance in left panel if exists
            if hasattr(parent, "left_panel"):
                self._update_buttons_in_panel(parent.left_panel)
        
        # Refresh the parent to ensure visual updates
        if parent:
            parent.update()

    def _update_buttons_in_panel(self, panel):
        """Update all buttons in a panel according to the display mode"""
        if not panel:
            return
            
        # Get the button display mode
        button_mode = self.app_settings.current_settings.get("button_display_mode", "both")
        
        # Find all buttons in the panel and update them
        buttons = panel.findChildren(QPushButton)
        for btn in buttons:
            if button_mode == "icons":
                btn.setText("")  # Show only icons
                btn.setMinimumWidth(32)  # Make it more square-like for icons
            elif button_mode == "text":
                # Restore text if available (we need to track original text)
                if hasattr(btn, "_original_text"):
                    btn.setText(btn._original_text)
                btn.setIcon(QIcon())  # Remove icon
                btn.setMinimumWidth(80)  # Make wider for text
            else:  # "both"
                # Restore both text and icon if they were originally present
                if hasattr(btn, "_original_text"):
                    btn.setText(btn._original_text)
                # Don't remove the icon in 'both' mode, keep existing icon
                btn.setMinimumWidth(80)  # Standard width

    def _save_settings(self):
        """Save settings permanently"""
        # Apply current settings first
        # Note: _apply_settings() is called again here even if already called from Apply button
        # This ensures all UI values are captured when OK is pressed directly
        self._apply_settings()
        # Save to app settings file
        self.app_settings.save_settings()

    def _reset_to_defaults(self):
        """Reset settings to defaults"""
        # Reset theme
        self.theme_combo.setCurrentIndex(0)
        
        # Reset button display mode
        self.both_radio.setChecked(True)
        
        # Reset visual effects
        self.animations_check.setChecked(True)
        self.shadows_check.setChecked(True)
        self.transparency_check.setChecked(True)
        self.rounded_corners_check.setChecked(True)

    def _choose_color(self, color_type):
        """Open color dialog to choose a color"""
        color = QColorDialog.getColor()
        if color.isValid():
            # Store the chosen color in app settings
            color_key = f"{color_type}_color"
            self.app_settings.current_settings[color_key] = color.name()
            # Apply the theme
            self._apply_theme_to_main_window()

    def _choose_font(self, font_type):
        """Open font dialog to choose a font"""
        current_font = QFont()
        font, ok = QFontDialog.getFont(current_font, self)
        if ok:
            # Store font settings
            self.app_settings.current_settings[f"{font_type}_font_family"] = font.family()
            self.app_settings.current_settings[f"{font_type}_font_size"] = font.pointSize()
            self.app_settings.current_settings[f"{font_type}_font_weight"] = font.weight()
            # Apply the theme
            self._apply_theme_to_main_window()

    def _update_font_scale_label(self, value):
        """Update font scale label"""
        self.font_scale_label.setText(f"{value}%")

    def _preview_tab_changes(self):
        """Preview tab changes"""
        # This would apply temporary tab changes for preview
        pass

    def _reset_tab_settings(self):
        """Reset tab settings to defaults"""
        self.main_tab_height_spin.setValue(35)
        self.individual_tab_height_spin.setValue(24)
        self.tab_font_size_spin.setValue(9)
        self.tab_padding_spin.setValue(4)
        self.tab_container_height_spin.setValue(30)
        self.tab_style_combo.setCurrentText("Compact")

    def _apply_tab_style_preset(self, preset):
        """Apply tab style preset"""
        if preset == "Compact":
            self.main_tab_height_spin.setValue(35)
            self.individual_tab_height_spin.setValue(24)
            self.tab_font_size_spin.setValue(9)
            self.tab_padding_spin.setValue(4)
        elif preset == "Standard":
            self.main_tab_height_spin.setValue(45)
            self.individual_tab_height_spin.setValue(28)
            self.tab_font_size_spin.setValue(10)
            self.tab_padding_spin.setValue(6)
        elif preset == "Large":
            self.main_tab_height_spin.setValue(55)
            self.individual_tab_height_spin.setValue(32)
            self.tab_font_size_spin.setValue(11)
            self.tab_padding_spin.setValue(8)
