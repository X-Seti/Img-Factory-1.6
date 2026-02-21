#this belongs in methods/imgfactory_ui_settings.py - Version: 4
# X-Seti - February04 2026 - IMG Factory 1.6 - IMG Factory Settings Dialog

"""
IMG Factory Settings Dialog - Application-specific settings with UI mode toggle
Handles IMG Factory-only settings separate from global theme settings
"""

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QGroupBox, QCheckBox, QPushButton, QHBoxLayout,
    QSpinBox, QLabel, QComboBox, QFontComboBox, QTabWidget, QWidget,
    QMessageBox, QRadioButton, QButtonGroup
)
from PyQt6.QtCore import QSize
from PyQt6.QtGui import QFont
from apps.methods.img_factory_settings import IMGFactorySettings

##Methods list -
# show_imgfactory_settings_dialog

##Classes -
# IMGFactorySettingsDialog
# __init__
# _apply_settings
# _create_advanced_tab
# _create_buttons
# _create_file_window_tab
# _create_general_tab
# _create_interface_tab
# _create_ui
# _create_ui_tab
# _reset_settings
# _save_and_close
# _save_settings

class IMGFactorySettingsDialog(QDialog): #vers 1
    """IMG Factory-specific settings dialog with 4 tabs"""

    def __init__(self, main_window, parent=None): #vers 1
        super().__init__(parent)
        self.main_window = main_window
        self.img_settings = IMGFactorySettings()

        self.setWindowTitle("IMG Factory Settings")
        self.setMinimumWidth(500)
        self.setMinimumHeight(400)

        self._create_ui()

    def _create_ui(self): #vers 2
        """Create the settings UI with 5 tabs"""
        main_layout = QVBoxLayout(self)

        # Create tabbed interface
        tabs = QTabWidget()

        # Add 5 tabs
        tabs.addTab(self._create_general_tab(), "General")
        tabs.addTab(self._create_interface_tab(), "Interface")
        tabs.addTab(self._create_ui_tab(), "UI")
        tabs.addTab(self._create_file_window_tab(), "File Window")
        tabs.addTab(self._create_advanced_tab(), "Advanced")

        main_layout.addWidget(tabs)

        # Add buttons
        main_layout.addLayout(self._create_buttons())

    def _create_general_tab(self): #vers 1
        """Create General settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)

        # Import/Export Group
        import_group = QGroupBox("Import/Export Behavior")
        import_layout = QVBoxLayout()

        self.auto_save_cb = QCheckBox("Auto-save after import")
        self.auto_save_cb.setChecked(self.img_settings.get("auto_save_on_import", True))
        import_layout.addWidget(self.auto_save_cb)

        self.auto_reload_cb = QCheckBox("Auto-reload after import (reload from disk)")
        self.auto_reload_cb.setChecked(self.img_settings.get("auto_reload_on_import", False))
        import_layout.addWidget(self.auto_reload_cb)

        import_group.setLayout(import_layout)
        layout.addWidget(import_group)

        # IDE Integration Group
        ide_group = QGroupBox("IDE Integration")
        ide_layout = QVBoxLayout()

        self.load_ide_cb = QCheckBox("Load IDE file automatically if found with IMG")
        self.load_ide_cb.setChecked(self.img_settings.get("load_ide_with_img", False))
        ide_layout.addWidget(self.load_ide_cb)

        ide_pref_layout = QHBoxLayout()
        ide_pref_layout.addWidget(QLabel("Preferred IDE tool:"))
        self.ide_combo = QComboBox()
        self.ide_combo.addItems(["Text Editor", "IDE Workshop"])
        self.ide_combo.setCurrentText(self.img_settings.get("preferred_ide_name", "TXD Workshop"))
        ide_pref_layout.addWidget(self.ide_combo)
        ide_layout.addLayout(ide_pref_layout)

        ide_group.setLayout(ide_layout)
        layout.addWidget(ide_group)

        # Window Behavior Group
        window_group = QGroupBox("Window Behavior")
        window_layout = QVBoxLayout()

        self.remember_size_cb = QCheckBox("Remember window size")
        self.remember_size_cb.setChecked(self.img_settings.get("remember_window_size", True))
        window_layout.addWidget(self.remember_size_cb)

        self.remember_pos_cb = QCheckBox("Remember window position")
        self.remember_pos_cb.setChecked(self.img_settings.get("remember_window_position", True))
        window_layout.addWidget(self.remember_pos_cb)

        window_group.setLayout(window_layout)
        layout.addWidget(window_group)

        layout.addStretch()
        return widget

    def _create_interface_tab(self): #vers 1
        """Create Interface settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)

        # Button Spacing Group
        button_group = QGroupBox("Button Layout")
        button_layout = QVBoxLayout()

        h_spacing_layout = QHBoxLayout()
        h_spacing_layout.addWidget(QLabel("Horizontal spacing:"))
        self.h_spacing_spin = QSpinBox()
        self.h_spacing_spin.setRange(0, 50)
        self.h_spacing_spin.setValue(self.img_settings.get("button_horizontal_spacing", 10))
        self.h_spacing_spin.setSuffix(" px")
        h_spacing_layout.addWidget(self.h_spacing_spin)
        h_spacing_layout.addStretch()
        button_layout.addLayout(h_spacing_layout)

        v_spacing_layout = QHBoxLayout()
        v_spacing_layout.addWidget(QLabel("Vertical spacing:"))
        self.v_spacing_spin = QSpinBox()
        self.v_spacing_spin.setRange(0, 50)
        self.v_spacing_spin.setValue(self.img_settings.get("button_vertical_spacing", 10))
        self.v_spacing_spin.setSuffix(" px")
        v_spacing_layout.addWidget(self.v_spacing_spin)
        v_spacing_layout.addStretch()
        button_layout.addLayout(v_spacing_layout)

        button_group.setLayout(button_layout)
        layout.addWidget(button_group)

        # Font Settings Group
        font_group = QGroupBox("Font Settings")
        font_layout = QVBoxLayout()

        self.use_custom_font_cb = QCheckBox("Use custom font settings")
        self.use_custom_font_cb.setChecked(self.img_settings.get("use_custom_font", False))
        font_layout.addWidget(self.use_custom_font_cb)

        font_family_layout = QHBoxLayout()
        font_family_layout.addWidget(QLabel("Font:"))
        self.font_combo = QFontComboBox()
        self.font_combo.setCurrentFont(QFont(self.img_settings.get("font_family", "Segoe UI")))
        self.font_combo.setEnabled(self.use_custom_font_cb.isChecked())
        font_family_layout.addWidget(self.font_combo)
        font_layout.addLayout(font_family_layout)

        font_size_layout = QHBoxLayout()
        font_size_layout.addWidget(QLabel("Size:"))
        self.font_size_spin = QSpinBox()
        self.font_size_spin.setRange(6, 24)
        self.font_size_spin.setValue(self.img_settings.get("font_size", 9))
        self.font_size_spin.setSuffix(" pt")
        self.font_size_spin.setEnabled(self.use_custom_font_cb.isChecked())
        font_size_layout.addWidget(self.font_size_spin)
        font_size_layout.addStretch()
        font_layout.addLayout(font_size_layout)

        font_style_layout = QHBoxLayout()
        self.font_bold_cb = QCheckBox("Bold")
        self.font_bold_cb.setChecked(self.img_settings.get("font_bold", False))
        self.font_bold_cb.setEnabled(self.use_custom_font_cb.isChecked())
        font_style_layout.addWidget(self.font_bold_cb)

        self.font_italic_cb = QCheckBox("Italic")
        self.font_italic_cb.setChecked(self.img_settings.get("font_italic", False))
        self.font_italic_cb.setEnabled(self.use_custom_font_cb.isChecked())
        font_style_layout.addWidget(self.font_italic_cb)
        font_style_layout.addStretch()
        font_layout.addLayout(font_style_layout)

        # Enable/disable font controls based on checkbox
        def toggle_font_controls(checked):
            self.font_combo.setEnabled(checked)
            self.font_size_spin.setEnabled(checked)
            self.font_bold_cb.setEnabled(checked)
            self.font_italic_cb.setEnabled(checked)

        self.use_custom_font_cb.toggled.connect(toggle_font_controls)

        font_group.setLayout(font_layout)
        layout.addWidget(font_group)

        # Tab Settings Group
        tab_group = QGroupBox("Tab Settings")
        tab_layout = QVBoxLayout()

        tab_height_layout = QHBoxLayout()
        tab_height_layout.addWidget(QLabel("Tab height:"))
        self.tab_height_spin = QSpinBox()
        self.tab_height_spin.setRange(16, 60)
        self.tab_height_spin.setValue(self.img_settings.get("tab_height", 24))
        self.tab_height_spin.setSuffix(" px")
        tab_height_layout.addWidget(self.tab_height_spin)
        tab_height_layout.addStretch()
        tab_layout.addLayout(tab_height_layout)

        tab_width_layout = QHBoxLayout()
        tab_width_layout.addWidget(QLabel("Min tab width:"))
        self.tab_min_width_spin = QSpinBox()
        self.tab_min_width_spin.setRange(40, 300)
        self.tab_min_width_spin.setValue(self.img_settings.get("tab_min_width", 100))
        self.tab_min_width_spin.setSuffix(" px")
        tab_width_layout.addWidget(self.tab_min_width_spin)
        tab_width_layout.addStretch()
        tab_layout.addLayout(tab_width_layout)

        tab_style_layout = QHBoxLayout()
        tab_style_layout.addWidget(QLabel("Tab style:"))
        self.tab_style_combo = QComboBox()
        self.tab_style_combo.addItems(["default", "rounded", "square"])
        self.tab_style_combo.setCurrentText(self.img_settings.get("tab_style", "default"))
        tab_style_layout.addWidget(self.tab_style_combo)
        tab_style_layout.addStretch()
        tab_layout.addLayout(tab_style_layout)

        tab_pos_layout = QHBoxLayout()
        tab_pos_layout.addWidget(QLabel("Tab position:"))
        self.tab_position_combo = QComboBox()
        self.tab_position_combo.addItems(["top", "bottom"])
        self.tab_position_combo.setCurrentText(self.img_settings.get("tab_position", "top"))
        tab_pos_layout.addWidget(self.tab_position_combo)
        tab_pos_layout.addStretch()
        tab_layout.addLayout(tab_pos_layout)

        tab_group.setLayout(tab_layout)
        layout.addWidget(tab_group)

        layout.addStretch()
        return widget

    def _create_ui_tab(self): #vers 3
        """Create UI mode settings tab with SVG icon previews"""
        widget = QWidget()
        layout = QVBoxLayout(widget)

        # UI Mode Selection
        ui_mode_group = QGroupBox("UI Mode")
        ui_mode_layout = QVBoxLayout(ui_mode_group)

        self.ui_mode_button_group = QButtonGroup(self)

        # System UI option
        system_container = QWidget()
        system_layout = QVBoxLayout(system_container)
        system_layout.setContentsMargins(0, 0, 0, 10)

        self.system_ui_radio = QRadioButton("System UI")
        system_layout.addWidget(self.system_ui_radio)

        system_desc = QLabel("Standard window with menu bar")
        system_desc.setStyleSheet("color: #888; font-size: 10px; margin-left: 25px;")
        system_layout.addWidget(system_desc)

        system_preview = QLabel("[Settings] [Title] [ ] [_] [X]")
        system_preview.setStyleSheet("font-family: monospace; color: #aaa; margin-left: 25px; padding: 5px;")
        system_layout.addWidget(system_preview)

        ui_mode_layout.addWidget(system_container)

        # Custom UI option with SVG icons
        custom_container = QWidget()
        custom_layout = QVBoxLayout(custom_container)
        custom_layout.setContentsMargins(0, 0, 0, 10)

        self.custom_ui_radio = QRadioButton("Custom UI (COL Workshop Style)")
        custom_layout.addWidget(self.custom_ui_radio)

        custom_desc = QLabel("Toolbar with action buttons in title bar")
        custom_desc.setStyleSheet("color: #888; font-size: 10px; margin-left: 25px;")
        custom_layout.addWidget(custom_desc)

        # Create icon preview with actual SVG icon buttons
        icon_preview = QWidget()
        icon_layout = QHBoxLayout(icon_preview)
        icon_layout.setContentsMargins(25, 5, 0, 5)
        icon_layout.setSpacing(3)

        try:
            from apps.methods.imgfactory_svg_icons import (
                get_settings_icon, get_open_icon, get_save_icon,
                get_extract_icon, get_undo_icon, get_info_icon,
                get_minimize_icon, get_maximize_icon, get_close_icon
            )
            from PyQt6.QtCore import QSize

            # Settings button with icon
            settings_btn = QPushButton()
            settings_btn.setIcon(get_settings_icon())
            settings_btn.setIconSize(QSize(16, 16))
            settings_btn.setFixedSize(24, 24)
            settings_btn.setFlat(True)
            settings_btn.setToolTip("Settings")
            icon_layout.addWidget(settings_btn)

            settings_text = QLabel("Settings")
            settings_text.setStyleSheet("color: #aaa; font-size: 10px;")
            icon_layout.addWidget(settings_text)

            icon_layout.addSpacing(5)

            # Separator
            sep1 = QLabel("|")
            sep1.setStyleSheet("color: #555;")
            icon_layout.addWidget(sep1)

            icon_layout.addSpacing(5)

            # Title
            title_label = QLabel("Img Factory")
            title_label.setStyleSheet("color: #fff; font-weight: bold;")
            icon_layout.addWidget(title_label)

            icon_layout.addSpacing(5)

            # Separator
            sep2 = QLabel("|")
            sep2.setStyleSheet("color: #555;")
            icon_layout.addWidget(sep2)

            icon_layout.addSpacing(5)

            # Action buttons with SVG icons
            action_icons = [
                (get_open_icon(), "Open"),
                (get_save_icon(), "Save"),
                (get_extract_icon(), "Extract"),
                (get_undo_icon(), "Undo"),
                (get_info_icon(), "Info"),
                (get_settings_icon(), "Options")
            ]

            for icon_func, tooltip in action_icons:
                btn = QPushButton()
                btn.setIcon(icon_func)
                btn.setIconSize(QSize(16, 16))
                btn.setFixedSize(24, 24)
                btn.setFlat(True)
                btn.setToolTip(tooltip)
                icon_layout.addWidget(btn)

            icon_layout.addSpacing(5)

            # Separator
            sep3 = QLabel("|")
            sep3.setStyleSheet("color: #555;")
            icon_layout.addWidget(sep3)

            icon_layout.addSpacing(5)

            # Window controls with icons
            try:
                control_icons = [
                    (get_minimize_icon(), "Minimize"),
                    (get_maximize_icon(), "Maximize"),
                    (get_close_icon(), "Close")
                ]

                for icon_func, tooltip in control_icons:
                    btn = QPushButton()
                    btn.setIcon(icon_func)
                    btn.setIconSize(QSize(14, 14))
                    btn.setFixedSize(22, 22)
                    btn.setFlat(True)
                    btn.setToolTip(tooltip)
                    icon_layout.addWidget(btn)
            except:
                # Fallback text controls if icons not available
                controls = ["_", "□", "X"]
                for ctrl in controls:
                    ctrl_label = QLabel(ctrl)
                    ctrl_label.setStyleSheet("color: #aaa; font-size: 12px;")
                    icon_layout.addWidget(ctrl_label)

        except ImportError:
            # Fallback if icons not available
            fallback_label = QLabel("[Settings] Img Factory [Open][Save][Extract][Undo][i][*] [ ][_][X]")
            fallback_label.setStyleSheet("font-family: monospace; color: #aaa;")
            icon_layout.addWidget(fallback_label)

        icon_layout.addStretch()
        custom_layout.addWidget(icon_preview)

        ui_mode_layout.addWidget(custom_container)

        # Add buttons to button group
        self.ui_mode_button_group.addButton(self.system_ui_radio, 0)
        self.ui_mode_button_group.addButton(self.custom_ui_radio, 1)

        # Load current setting
        current_ui_mode = self.img_settings.get("ui_mode", "system")
        if current_ui_mode == "custom":
            self.custom_ui_radio.setChecked(True)
        else:
            self.system_ui_radio.setChecked(True)

        ui_mode_group.setLayout(ui_mode_layout)
        layout.addWidget(ui_mode_group)

        # Additional UI settings
        appearance_group = QGroupBox("Appearance")
        appearance_layout = QVBoxLayout(appearance_group)

        self.show_toolbar_check = QCheckBox("Show toolbar buttons")
        self.show_toolbar_check.setChecked(self.img_settings.get("show_toolbar", True))
        appearance_layout.addWidget(self.show_toolbar_check)

        self.show_status_bar_check = QCheckBox("Show status bar")
        self.show_status_bar_check.setChecked(self.img_settings.get("show_status_bar", True))
        appearance_layout.addWidget(self.show_status_bar_check)

        self.show_menu_bar_check = QCheckBox("Show menu bar")
        self.show_menu_bar_check.setChecked(self.img_settings.get("show_menu_bar", True))
        appearance_layout.addWidget(self.show_menu_bar_check)

        appearance_group.setLayout(appearance_layout)
        layout.addWidget(appearance_group)

        layout.addStretch()
        return widget

    def _create_file_window_tab(self): #vers 1
        """Create File Window settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)

        # Column Settings Group
        column_group = QGroupBox("Table Column Settings")
        column_layout = QVBoxLayout()

        info_label = QLabel("Configure which columns are visible and how they resize")
        info_label.setWordWrap(True)
        column_layout.addWidget(info_label)

        # IMG Table button
        img_button = QPushButton("IMG Table Columns...")
        img_button.clicked.connect(lambda: self._show_column_settings("img"))
        column_layout.addWidget(img_button)

        # COL Table button
        col_button = QPushButton("COL Table Columns...")
        col_button.clicked.connect(lambda: self._show_column_settings("col"))
        column_layout.addWidget(col_button)

        # TXD Table button
        txd_button = QPushButton("TXD Table Columns...")
        txd_button.clicked.connect(lambda: self._show_column_settings("txd"))
        column_layout.addWidget(txd_button)

        column_group.setLayout(column_layout)
        layout.addWidget(column_group)

        # Directory Tree Settings Group
        tree_group = QGroupBox("Directory Tree")
        tree_layout = QVBoxLayout()

        self.autoload_tree_cb = QCheckBox("Auto-load directory tree on startup")
        self.autoload_tree_cb.setChecked(self.img_settings.get("autoload_directory_tree", True))
        tree_layout.addWidget(self.autoload_tree_cb)

        tree_group.setLayout(tree_layout)
        layout.addWidget(tree_group)

        # PIN File Settings Group
        pin_group = QGroupBox("PIN File Settings")
        pin_layout = QVBoxLayout()

        self.enable_pin_files_cb = QCheckBox("Enable .pin files for tracking pinned entries and dates")
        self.enable_pin_files_cb.setChecked(self.img_settings.get("enable_pin_files", True))
        pin_layout.addWidget(self.enable_pin_files_cb)

        self.auto_create_pin_cb = QCheckBox("Auto-create .pin file on first import")
        self.auto_create_pin_cb.setChecked(self.img_settings.get("auto_create_pin", True))
        pin_layout.addWidget(self.auto_create_pin_cb)

        pin_group.setLayout(pin_layout)
        layout.addWidget(pin_group)

        layout.addStretch()
        return widget

    def _show_column_settings(self, table_type: str): #vers 1
        """Show column settings dialog for specific table type"""
        try:
            from apps.methods.column_settings_manager import show_column_settings_dialog
            show_column_settings_dialog(self.main_window, table_type)
        except Exception as e:
            print(f"Error showing column settings: {str(e)}")

    def _create_advanced_tab(self): #vers 1
        """Create Advanced settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)

        # File Handling Group
        file_group = QGroupBox("File Handling")
        file_layout = QVBoxLayout()

        recent_files_layout = QHBoxLayout()
        recent_files_layout.addWidget(QLabel("Recent files limit:"))
        self.recent_files_spin = QSpinBox()
        self.recent_files_spin.setRange(5, 50)
        self.recent_files_spin.setValue(self.img_settings.get("recent_files_limit", 10))
        recent_files_layout.addWidget(self.recent_files_spin)
        recent_files_layout.addStretch()
        file_layout.addLayout(recent_files_layout)

        self.auto_backup_cb = QCheckBox("Create automatic backups")
        self.auto_backup_cb.setChecked(self.img_settings.get("auto_backup", False))
        file_layout.addWidget(self.auto_backup_cb)

        backup_count_layout = QHBoxLayout()
        backup_count_layout.addWidget(QLabel("Number of backups:"))
        self.backup_count_spin = QSpinBox()
        self.backup_count_spin.setRange(1, 10)
        self.backup_count_spin.setValue(self.img_settings.get("backup_count", 3))
        self.backup_count_spin.setEnabled(self.auto_backup_cb.isChecked())
        backup_count_layout.addWidget(self.backup_count_spin)
        backup_count_layout.addStretch()
        file_layout.addLayout(backup_count_layout)

        self.auto_backup_cb.toggled.connect(self.backup_count_spin.setEnabled)

        file_group.setLayout(file_layout)
        layout.addWidget(file_group)

        layout.addStretch()
        return widget

    def _create_buttons(self): #vers 1
        """Create button layout"""
        button_layout = QHBoxLayout()

        reset_btn = QPushButton("Reset to Defaults")
        reset_btn.clicked.connect(self._reset_settings)
        button_layout.addWidget(reset_btn)

        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(self._apply_settings)
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.clicked.connect(self._save_and_close)
        ok_btn.setDefault(True)
        button_layout.addWidget(ok_btn)

        return button_layout

    def _save_settings(self): #vers 1
        """Save all settings to file"""
        # General tab
        self.img_settings.set("auto_save_on_import", self.auto_save_cb.isChecked())
        self.img_settings.set("auto_reload_on_import", self.auto_reload_cb.isChecked())
        self.img_settings.set("load_ide_with_img", self.load_ide_cb.isChecked())
        self.img_settings.set("preferred_ide_name", self.ide_combo.currentText())
        self.img_settings.set("remember_window_size", self.remember_size_cb.isChecked())
        self.img_settings.set("remember_window_position", self.remember_pos_cb.isChecked())

        # Interface tab
        self.img_settings.set("button_horizontal_spacing", self.h_spacing_spin.value())
        self.img_settings.set("button_vertical_spacing", self.v_spacing_spin.value())
        self.img_settings.set("use_custom_font", self.use_custom_font_cb.isChecked())
        self.img_settings.set("font_family", self.font_combo.currentFont().family())
        self.img_settings.set("font_size", self.font_size_spin.value())
        self.img_settings.set("font_bold", self.font_bold_cb.isChecked())
        self.img_settings.set("font_italic", self.font_italic_cb.isChecked())

        # UI tab
        if self.custom_ui_radio.isChecked():
            self.img_settings.set("ui_mode", "custom")
        else:
            self.img_settings.set("ui_mode", "system")

        self.img_settings.set("show_toolbar", self.show_toolbar_check.isChecked())
        self.img_settings.set("show_status_bar", self.show_status_bar_check.isChecked())
        self.img_settings.set("show_menu_bar", self.show_menu_bar_check.isChecked())

        # File Window tab
        self.img_settings.set("autoload_directory_tree", self.autoload_tree_cb.isChecked())
        self.img_settings.set("enable_pin_files", self.enable_pin_files_cb.isChecked())
        self.img_settings.set("auto_create_pin", self.auto_create_pin_cb.isChecked())

        # Tab settings
        self.img_settings.set("tab_height", self.tab_height_spin.value())
        self.img_settings.set("tab_min_width", self.tab_min_width_spin.value())
        self.img_settings.set("tab_style", self.tab_style_combo.currentText())
        self.img_settings.set("tab_position", self.tab_position_combo.currentText())

        # Advanced tab
        self.img_settings.set("recent_files_limit", self.recent_files_spin.value())
        self.img_settings.set("auto_backup", self.auto_backup_cb.isChecked())
        self.img_settings.set("backup_count", self.backup_count_spin.value())

        self.img_settings.save_settings()

    def _apply_settings(self): #vers 1
        """Apply settings without closing dialog"""
        self._save_settings()

        # Apply UI mode to main window
        if hasattr(self.main_window, 'apply_ui_mode'):
            ui_mode = "custom" if self.custom_ui_radio.isChecked() else "system"
            show_toolbar = self.show_toolbar_check.isChecked()
            show_status_bar = self.show_status_bar_check.isChecked()
            show_menu_bar = self.show_menu_bar_check.isChecked()
            self.main_window.apply_ui_mode(ui_mode, show_toolbar, show_status_bar, show_menu_bar)

        # Apply other immediate changes
        if hasattr(self.main_window, 'apply_app_settings'):
            temp_img_settings = IMGFactorySettings()
            if not hasattr(self.main_window, 'img_settings'):
                self.main_window.img_settings = temp_img_settings
            else:
                self.main_window.img_settings.current_settings = temp_img_settings.current_settings
            self.main_window.apply_app_settings()

        if hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("IMG Factory settings applied")

        # Apply tab settings
        try:
            from apps.methods.tab_settings_apply import apply_tab_settings
            apply_tab_settings(self.main_window, self.img_settings)
        except Exception as e:
            print(f"Tab settings apply failed: {e}")

        QMessageBox.information(self, "Settings Applied", "Settings have been applied successfully.")

    def _save_and_close(self): #vers 1
        """Save settings and close dialog"""
        self._save_settings()
        self._apply_settings()
        self.accept()

    def _reset_settings(self): #vers 1
        """Reset to default settings"""
        reply = QMessageBox.question(
            self,
            "Reset Settings",
            "Are you sure you want to reset all settings to defaults?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            self.img_settings.reset_to_defaults()
            self.accept()
            # Reopen dialog to show defaults
            show_imgfactory_settings_dialog(self.main_window)


def show_imgfactory_settings_dialog(main_window): #vers 1
    """Show IMG Factory settings dialog - main entry point"""
    try:
        dialog = IMGFactorySettingsDialog(main_window)
        dialog.exec()
    except Exception as e:
        QMessageBox.warning(
            main_window,
            "Error",
            f"Failed to open IMG Factory Settings: {str(e)}"
        )
