#this belongs in gui/gui_settings.py - Version: 4
# X-Seti - December30 2025 - IMG Factory 1.6 - Consolidated Settings Dialog

"""
Consolidated GUI Settings Dialog - Buttons, GUI, and Fonts configuration
"""

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTabWidget, QWidget,
    QGroupBox, QLabel, QSpinBox, QCheckBox, QComboBox, 
    QPushButton, QSlider, QColorDialog, QFontDialog,
    QMessageBox, QGridLayout, QRadioButton, QButtonGroup,
    QApplication
)
from PyQt6.QtCore import Qt, pyqtSignal
from PyQt6.QtGui import QFont, QColor

##Methods list -
# __init__
# _apply_button_display_mode
# _apply_settings
# _apply_tab_heights
# _apply_theme_to_main_window
# _choose_color
# _choose_font
# _create_buttons_tab
# _create_fonts_tab
# _create_gui_tab
# _create_ui
# _load_current_settings
# _reset_to_defaults
# _save_and_close
# _save_settings
# _update_buttons_in_panel
# _update_font_scale_label

class SettingsDialog(QDialog): #vers 4
    """Consolidated Settings Dialog with 3 tabs"""
    
    settings_changed = pyqtSignal()
    theme_changed = pyqtSignal(str)
    
    def __init__(self, app_settings, parent=None): #vers 4
        super().__init__(parent)
        self.app_settings = app_settings
        self.setWindowTitle("IMG Factory 1.6 - Settings")
        self.setMinimumSize(600, 550)
        self.resize(650, 600)
        
        self._create_ui()
        self._load_current_settings()

    def _create_ui(self): #vers 4
        """Create the settings UI with 3 tabs"""
        layout = QVBoxLayout(self)
        
        # Tab widget
        self.tab_widget = QTabWidget()
        
        # Create 4 tabs
        self.tab_widget.addTab(self._create_buttons_tab(), "Buttons")
        self.tab_widget.addTab(self._create_gui_tab(), "GUI")
        self.tab_widget.addTab(self._create_ui_tab(), "UI")
        self.tab_widget.addTab(self._create_fonts_tab(), "Fonts")
        
        layout.addWidget(self.tab_widget)
        
        # Button bar
        button_layout = QHBoxLayout()
        
        # Reset to defaults
        reset_btn = QPushButton("Reset to Defaults")
        reset_btn.clicked.connect(self._reset_to_defaults)
        button_layout.addWidget(reset_btn)
        
        button_layout.addStretch()
        
        # Cancel
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)
        
        # Apply
        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(self._apply_settings)
        button_layout.addWidget(apply_btn)
        
        # OK
        ok_btn = QPushButton("OK")
        ok_btn.clicked.connect(self._save_and_close)
        ok_btn.setDefault(True)
        button_layout.addWidget(ok_btn)
        
        layout.addLayout(button_layout)
    
    def _create_buttons_tab(self): #vers 4
        """Create buttons settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Button Spacing
        spacing_group = QGroupBox("Button Spacing")
        spacing_layout = QGridLayout(spacing_group)
        
        # Vertical spacing
        spacing_layout.addWidget(QLabel("Vertical Spacing:"), 0, 0)
        self.btn_spacing_v_spin = QSpinBox()
        self.btn_spacing_v_spin.setRange(0, 20)
        self.btn_spacing_v_spin.setValue(8)
        self.btn_spacing_v_spin.setSuffix(" px")
        spacing_layout.addWidget(self.btn_spacing_v_spin, 0, 1)
        
        # Horizontal spacing
        spacing_layout.addWidget(QLabel("Horizontal Spacing:"), 1, 0)
        self.btn_spacing_h_spin = QSpinBox()
        self.btn_spacing_h_spin.setRange(0, 20)
        self.btn_spacing_h_spin.setValue(6)
        self.btn_spacing_h_spin.setSuffix(" px")
        spacing_layout.addWidget(self.btn_spacing_h_spin, 1, 1)
        
        layout.addWidget(spacing_group)
        
        # Button Height
        height_group = QGroupBox("Button Height")
        height_layout = QGridLayout(height_group)
        
        height_layout.addWidget(QLabel("Button Height:"), 0, 0)
        self.btn_height_spin = QSpinBox()
        self.btn_height_spin.setRange(20, 60)
        self.btn_height_spin.setValue(32)
        self.btn_height_spin.setSuffix(" px")
        height_layout.addWidget(self.btn_height_spin, 0, 1)
        
        layout.addWidget(height_group)
        
        # Tab Heights
        tab_group = QGroupBox("Tab Heights")
        tab_layout = QGridLayout(tab_group)
        
        # Main type tabs (IMG/COL/TXD)
        tab_layout.addWidget(QLabel("Main Type Tabs (IMG/COL/TXD):"), 0, 0)
        self.main_type_tab_height_spin = QSpinBox()
        self.main_type_tab_height_spin.setRange(25, 60)
        self.main_type_tab_height_spin.setValue(35)
        self.main_type_tab_height_spin.setSuffix(" px")
        self.main_type_tab_height_spin.setToolTip("Height for IMG, COL, TXD tabs")
        tab_layout.addWidget(self.main_type_tab_height_spin, 0, 1)
        
        # Individual file tabs
        tab_layout.addWidget(QLabel("Individual File Tabs:"), 1, 0)
        self.individual_tab_height_spin = QSpinBox()
        self.individual_tab_height_spin.setRange(20, 45)
        self.individual_tab_height_spin.setValue(28)
        self.individual_tab_height_spin.setSuffix(" px")
        self.individual_tab_height_spin.setToolTip("Height for individual opened file tabs")
        tab_layout.addWidget(self.individual_tab_height_spin, 1, 1)
        
        layout.addWidget(tab_group)
        
        # Pastel Theme
        pastel_group = QGroupBox("Button Theme")
        pastel_layout = QVBoxLayout(pastel_group)
        
        self.pastel_check = QCheckBox("Enable Pastel effect for buttons")
        pastel_layout.addWidget(self.pastel_check)
        
        self.high_contrast_check = QCheckBox("High contrast buttons")
        pastel_layout.addWidget(self.high_contrast_check)
        
        layout.addWidget(pastel_group)
        
        # Display Mode
        display_group = QGroupBox("Button Display Mode")
        display_layout = QVBoxLayout(display_group)
        
        self.display_mode_group = QButtonGroup(self)
        
        self.icons_text_radio = QRadioButton("Show Icons and Text")
        self.icons_only_radio = QRadioButton("Show Icons Only")
        self.text_only_radio = QRadioButton("Show Text Only")
        
        self.display_mode_group.addButton(self.icons_text_radio, 0)
        self.display_mode_group.addButton(self.icons_only_radio, 1)
        self.display_mode_group.addButton(self.text_only_radio, 2)
        
        display_layout.addWidget(self.icons_text_radio)
        display_layout.addWidget(self.icons_only_radio)
        display_layout.addWidget(self.text_only_radio)
        
        layout.addWidget(display_group)
        
        # Button Sizes
        size_group = QGroupBox("Icon and Text Sizes")
        size_layout = QGridLayout(size_group)
        
        size_layout.addWidget(QLabel("Icon Size:"), 0, 0)
        self.icon_size_spin = QSpinBox()
        self.icon_size_spin.setRange(12, 32)
        self.icon_size_spin.setValue(16)
        self.icon_size_spin.setSuffix(" px")
        size_layout.addWidget(self.icon_size_spin, 0, 1)
        
        size_layout.addWidget(QLabel("Text Size:"), 1, 0)
        self.text_size_spin = QSpinBox()
        self.text_size_spin.setRange(7, 14)
        self.text_size_spin.setValue(9)
        self.text_size_spin.setSuffix(" pt")
        size_layout.addWidget(self.text_size_spin, 1, 1)
        
        layout.addWidget(size_group)
        
        layout.addStretch()
        
        return widget
    
    def _create_gui_tab(self): #vers 4
        """Create GUI settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Theme selection
        theme_group = QGroupBox("Theme")
        theme_layout = QVBoxLayout(theme_group)
        
        self.theme_combo = QComboBox()
        self.theme_combo.addItems([
            "IMG Factory Default",
            "Dark Theme", 
            "Light Professional",
            "GTA San Andreas",
            "GTA Vice City"
        ])
        theme_layout.addWidget(QLabel("Select Theme:"))
        theme_layout.addWidget(self.theme_combo)
        
        layout.addWidget(theme_group)
        
        # Transparency
        transparency_group = QGroupBox("Transparency")
        transparency_layout = QVBoxLayout(transparency_group)
        
        transparency_layout.addWidget(QLabel("Window Transparency:"))
        self.transparency_slider = QSlider(Qt.Orientation.Horizontal)
        self.transparency_slider.setRange(50, 100)
        self.transparency_slider.setValue(100)
        self.transparency_slider.valueChanged.connect(lambda v: self.transparency_label.setText(f"{v}%"))
        
        self.transparency_label = QLabel("100%")
        self.transparency_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        
        transparency_layout.addWidget(self.transparency_slider)
        transparency_layout.addWidget(self.transparency_label)
        
        layout.addWidget(transparency_group)
        
        # Visual Effects
        effects_group = QGroupBox("Visual Effects")
        effects_layout = QVBoxLayout(effects_group)
        
        self.animations_check = QCheckBox("Enable animations")
        self.shadows_check = QCheckBox("Enable shadows")
        self.transparency_effects_check = QCheckBox("Enable transparency effects")
        self.rounded_corners_check = QCheckBox("Rounded corners")
        
        effects_layout.addWidget(self.animations_check)
        effects_layout.addWidget(self.shadows_check)
        effects_layout.addWidget(self.transparency_effects_check)
        effects_layout.addWidget(self.rounded_corners_check)
        
        layout.addWidget(effects_group)
        
        layout.addStretch()
        
        return widget
    
    def _create_ui_tab(self): #vers 1
        """Create UI settings tab for custom vs system UI"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # UI Mode Selection
        ui_mode_group = QGroupBox("UI Mode")
        ui_mode_layout = QVBoxLayout(ui_mode_group)
        
        self.ui_mode_group = QButtonGroup(self)
        
        self.system_ui_radio = QRadioButton("System UI [Settings] [Title] [ ] [_][X]")
        self.custom_ui_radio = QRadioButton("Custom UI [Settings] Title = Img Factory [Open][Save][Extract][undo][i][*] [ ] [_][X]")
        
        self.ui_mode_group.addButton(self.system_ui_radio, 0)
        self.ui_mode_group.addButton(self.custom_ui_radio, 1)
        
        ui_mode_layout.addWidget(self.system_ui_radio)
        ui_mode_layout.addWidget(self.custom_ui_radio)
        
        layout.addWidget(ui_mode_group)
        
        # Additional UI settings
        appearance_group = QGroupBox("Appearance")
        appearance_layout = QVBoxLayout(appearance_group)
        
        self.show_toolbar_check = QCheckBox("Show toolbar buttons")
        self.show_status_bar_check = QCheckBox("Show status bar")
        self.show_menu_bar_check = QCheckBox("Show menu bar")
        
        appearance_layout.addWidget(self.show_toolbar_check)
        appearance_layout.addWidget(self.show_status_bar_check)
        appearance_layout.addWidget(self.show_menu_bar_check)
        
        layout.addWidget(appearance_group)
        
        layout.addStretch()
        
        return widget
    
    def _create_fonts_tab(self): #vers 4
        """Create fonts settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Font categories
        font_group = QGroupBox("Font Settings")
        font_layout = QGridLayout(font_group)
        
        # Main interface font
        font_layout.addWidget(QLabel("Main Interface:"), 0, 0)
        self.main_font_btn = QPushButton("Select Font...")
        self.main_font_btn.clicked.connect(lambda: self._choose_font('main'))
        font_layout.addWidget(self.main_font_btn, 0, 1)
        
        # Table font
        font_layout.addWidget(QLabel("Table:"), 1, 0)
        self.table_font_btn = QPushButton("Select Font...")
        self.table_font_btn.clicked.connect(lambda: self._choose_font('table'))
        font_layout.addWidget(self.table_font_btn, 1, 1)
        
        # Menu font
        font_layout.addWidget(QLabel("Menu:"), 2, 0)
        self.menu_font_btn = QPushButton("Select Font...")
        self.menu_font_btn.clicked.connect(lambda: self._choose_font('menu'))
        font_layout.addWidget(self.menu_font_btn, 2, 1)
        
        # Button font
        font_layout.addWidget(QLabel("Buttons:"), 3, 0)
        self.button_font_btn = QPushButton("Select Font...")
        self.button_font_btn.clicked.connect(lambda: self._choose_font('button'))
        font_layout.addWidget(self.button_font_btn, 3, 1)
        
        # Tab font
        font_layout.addWidget(QLabel("Tabs:"), 4, 0)
        self.tab_font_btn = QPushButton("Select Font...")
        self.tab_font_btn.clicked.connect(lambda: self._choose_font('tab'))
        font_layout.addWidget(self.tab_font_btn, 4, 1)
        
        layout.addWidget(font_group)
        
        # Font size scaling
        size_group = QGroupBox("Global Font Scale")
        size_layout = QVBoxLayout(size_group)
        
        size_layout.addWidget(QLabel("Font Size Scale:"))
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
        
        layout.addStretch()
        
        return widget

    def _load_current_settings(self): #vers 4
        """Load current settings into UI"""
        settings = self.app_settings.current_settings
        
        # Buttons tab
        self.btn_spacing_v_spin.setValue(settings.get('button_spacing_vertical', 8))
        self.btn_spacing_h_spin.setValue(settings.get('button_spacing_horizontal', 6))
        self.btn_height_spin.setValue(settings.get('button_height', 32))
        self.main_type_tab_height_spin.setValue(settings.get('main_type_tab_height', 35))
        self.individual_tab_height_spin.setValue(settings.get('individual_tab_height', 28))
        self.pastel_check.setChecked(settings.get('use_pastel_buttons', True))
        self.high_contrast_check.setChecked(settings.get('high_contrast_buttons', False))
        self.icon_size_spin.setValue(settings.get('button_icon_size', 16))
        self.text_size_spin.setValue(settings.get('button_text_size', 9))
        
        # Button display mode
        button_mode = settings.get('button_display_mode', 'both')
        if button_mode == 'icons_only':
            self.icons_only_radio.setChecked(True)
        elif button_mode == 'text_only':
            self.text_only_radio.setChecked(True)
        else:
            self.icons_text_radio.setChecked(True)
        
        # GUI tab
        theme = settings.get('theme', 'IMG_Factory')
        theme_index = self.theme_combo.findText(theme, Qt.MatchFlag.MatchContains)
        if theme_index >= 0:
            self.theme_combo.setCurrentIndex(theme_index)
        
        self.transparency_slider.setValue(settings.get('window_transparency', 100))
        self.animations_check.setChecked(settings.get('animations_enabled', True))
        self.shadows_check.setChecked(settings.get('shadows_enabled', True))
        self.transparency_effects_check.setChecked(settings.get('transparency_effects', True))
        self.rounded_corners_check.setChecked(settings.get('rounded_corners', True))
        
        # Fonts tab
        self.font_scale_slider.setValue(settings.get('font_scale', 100))
        
        # UI tab
        ui_mode = settings.get('ui_mode', 'system')
        if ui_mode == 'custom':
            self.custom_ui_radio.setChecked(True)
        else:
            self.system_ui_radio.setChecked(True)
        
        self.show_toolbar_check.setChecked(settings.get('show_toolbar', True))
        self.show_status_bar_check.setChecked(settings.get('show_status_bar', True))
        self.show_menu_bar_check.setChecked(settings.get('show_menu_bar', True))

    def _apply_settings(self): #vers 4
        """Apply settings without closing"""
        settings = self.app_settings.current_settings
        
        # Buttons settings
        settings['button_spacing_vertical'] = self.btn_spacing_v_spin.value()
        settings['button_spacing_horizontal'] = self.btn_spacing_h_spin.value()
        settings['button_height'] = self.btn_height_spin.value()
        settings['main_type_tab_height'] = self.main_type_tab_height_spin.value()
        settings['individual_tab_height'] = self.individual_tab_height_spin.value()
        settings['use_pastel_buttons'] = self.pastel_check.isChecked()
        settings['high_contrast_buttons'] = self.high_contrast_check.isChecked()
        settings['button_icon_size'] = self.icon_size_spin.value()
        settings['button_text_size'] = self.text_size_spin.value()
        
        # Button display mode
        if self.icons_only_radio.isChecked():
            settings['button_display_mode'] = 'icons_only'
        elif self.text_only_radio.isChecked():
            settings['button_display_mode'] = 'text_only'
        else:
            settings['button_display_mode'] = 'both'
        
        # GUI settings
        settings['theme'] = self.theme_combo.currentText()
        settings['window_transparency'] = self.transparency_slider.value()
        settings['animations_enabled'] = self.animations_check.isChecked()
        settings['shadows_enabled'] = self.shadows_check.isChecked()
        settings['transparency_effects'] = self.transparency_effects_check.isChecked()
        settings['rounded_corners'] = self.rounded_corners_check.isChecked()
        
        # Font settings
        settings['font_scale'] = self.font_scale_slider.value()
        
        # UI settings
        if self.custom_ui_radio.isChecked():
            settings['ui_mode'] = 'custom'
        else:
            settings['ui_mode'] = 'system'
        
        settings['show_toolbar'] = self.show_toolbar_check.isChecked()
        settings['show_status_bar'] = self.show_status_bar_check.isChecked()
        settings['show_menu_bar'] = self.show_menu_bar_check.isChecked()
        
        # Apply changes to main window
        self._apply_theme_to_main_window()
        self._apply_button_display_mode()
        self._apply_tab_heights()
        self._apply_ui_mode()
        
        # Emit signal
        self.settings_changed.emit()
        
        parent = self.parent()
        if parent and hasattr(parent, 'log_message'):
            parent.log_message("Settings applied")

    def _save_and_close(self): #vers 4
        """Save settings and close dialog"""
        self._apply_settings()
        self.app_settings.save_settings()
        self.accept()

    def _reset_to_defaults(self): #vers 4
        """Reset all settings to defaults"""
        # Buttons
        self.btn_spacing_v_spin.setValue(8)
        self.btn_spacing_h_spin.setValue(6)
        self.btn_height_spin.setValue(32)
        self.main_type_tab_height_spin.setValue(35)
        self.individual_tab_height_spin.setValue(28)
        self.pastel_check.setChecked(True)
        self.high_contrast_check.setChecked(False)
        self.icons_text_radio.setChecked(True)
        self.icon_size_spin.setValue(16)
        self.text_size_spin.setValue(9)
        
        # GUI
        self.theme_combo.setCurrentIndex(0)
        self.transparency_slider.setValue(100)
        self.animations_check.setChecked(True)
        self.shadows_check.setChecked(True)
        self.transparency_effects_check.setChecked(True)
        self.rounded_corners_check.setChecked(True)
        
        # Fonts
        self.font_scale_slider.setValue(100)

    def _choose_font(self, font_type): #vers 4
        """Open font dialog"""
        current_font = QFont()
        font, ok = QFontDialog.getFont(current_font, self)
        if ok:
            self.app_settings.current_settings[f"{font_type}_font_family"] = font.family()
            self.app_settings.current_settings[f"{font_type}_font_size"] = font.pointSize()
            self.app_settings.current_settings[f"{font_type}_font_weight"] = font.weight()
            self._apply_theme_to_main_window()

    def _choose_color(self, color_type): #vers 4
        """Open color dialog"""
        color = QColorDialog.getColor()
        if color.isValid():
            color_key = f"{color_type}_color"
            self.app_settings.current_settings[color_key] = color.name()
            self._apply_theme_to_main_window()

    def _update_font_scale_label(self, value): #vers 4
        """Update font scale label"""
        self.font_scale_label.setText(f"{value}%")

    def _apply_theme_to_main_window(self): #vers 4
        """Apply theme to main window"""
        parent = self.parent()
        if parent and hasattr(self.app_settings, 'get_stylesheet'):
            stylesheet = self.app_settings.get_stylesheet()
            parent.setStyleSheet(stylesheet)
            parent.update()
            QApplication.processEvents()

    def _apply_button_display_mode(self): #vers 4
        """Apply button display mode"""
        parent = self.parent()
        if parent and hasattr(parent, "gui_layout") and hasattr(parent.gui_layout, "create_right_panel_with_pastel_buttons"):
            parent.gui_layout.create_right_panel_with_pastel_buttons()
        if parent:
            parent.update()

    def _apply_tab_heights(self): #vers 4
        """Apply tab height settings to main window"""
        parent = self.parent()
        if not parent:
            return
        
        # Apply main type tab heights (IMG/COL/TXD)
        main_tab_height = self.main_type_tab_height_spin.value()
        if hasattr(parent, 'main_type_tabs'):
            parent.main_type_tabs.setStyleSheet(f"""
                QTabBar::tab {{
                    height: {main_tab_height}px;
                    min-height: {main_tab_height}px;
                    max-height: {main_tab_height}px;
                }}
            """)
        
        # Apply individual file tab heights
        individual_tab_height = self.individual_tab_height_spin.value()
        if hasattr(parent, 'gui_layout') and hasattr(parent.gui_layout, 'tab_widget'):
            parent.gui_layout.tab_widget.setStyleSheet(f"""
                QTabBar::tab {{
                    height: {individual_tab_height}px;
                    min-height: {individual_tab_height}px;
                    max-height: {individual_tab_height}px;
                }}
            """)

    def _update_buttons_in_panel(self, panel): #vers 4
        """Update buttons in panel"""
        if not panel:
            return
        
        from PyQt6.QtWidgets import QPushButton
        from PyQt6.QtGui import QIcon
        
        button_mode = self.app_settings.current_settings.get("button_display_mode", "both")
        buttons = panel.findChildren(QPushButton)
        
        for btn in buttons:
            if button_mode == "icons_only":
                btn.setText("")
                btn.setMinimumWidth(32)
            elif button_mode == "text_only":
                if hasattr(btn, "_original_text"):
                    btn.setText(btn._original_text)
                btn.setIcon(QIcon())
                btn.setMinimumWidth(80)
            else:
                if hasattr(btn, "_original_text"):
                    btn.setText(btn._original_text)
                btn.setMinimumWidth(80)

    def _apply_ui_mode(self): #vers 1
        """Apply UI mode settings to main window"""
        parent = self.parent()
        if not parent:
            return
        
        # Get the UI mode settings
        ui_mode = self.app_settings.current_settings.get('ui_mode', 'system')
        show_toolbar = self.app_settings.current_settings.get('show_toolbar', True)
        show_status_bar = self.app_settings.current_settings.get('show_status_bar', True)
        show_menu_bar = self.app_settings.current_settings.get('show_menu_bar', True)
        
        # Apply UI mode to main window
        if hasattr(parent, 'apply_ui_mode'):
            parent.apply_ui_mode(ui_mode, show_toolbar, show_status_bar, show_menu_bar)
        else:
            # Fallback implementation if the method doesn't exist yet
            if hasattr(parent, 'menuBar') and callable(parent.menuBar):
                menu_bar = parent.menuBar()
                menu_bar.setVisible(show_menu_bar)
            
            if hasattr(parent, 'statusBar') and callable(parent.statusBar):
                status_bar = parent.statusBar()
                status_bar.setVisible(show_status_bar)
