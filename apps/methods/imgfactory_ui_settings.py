#this belongs in methods/imgfactory_ui_settings.py - Version: 4
# X-Seti - February04 2026 - IMG Factory 1.6 - IMG Factory Settings Dialog

"""
IMG Factory Settings Dialog - Application-specific settings with UI mode toggle
Handles IMG Factory-only settings separate from global theme settings
"""

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QGroupBox, QCheckBox, QPushButton, QHBoxLayout,
    QSpinBox, QLabel, QComboBox, QFontComboBox, QTabWidget, QWidget,
    QMessageBox, QRadioButton, QButtonGroup, QSlider
)
from PyQt6.QtCore import QSize, Qt
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

class IMGFactorySettingsDialog(QDialog): #vers 2
    """IMG Factory-specific settings dialog — non-modal standalone window."""

    def __init__(self, main_window, parent=None): #vers 2
        # Use no parent so dialog is a true top-level window (movable anywhere)
        super().__init__(None)
        self.main_window = main_window
        self.img_settings = getattr(main_window, 'img_settings', None) or IMGFactorySettings()

        self.setWindowTitle("IMG Factory Settings")
        self.setWindowFlags(
            Qt.WindowType.Window |
            Qt.WindowType.WindowCloseButtonHint |
            Qt.WindowType.WindowMinimizeButtonHint |
            Qt.WindowType.WindowMaximizeButtonHint
        )
        self.setMinimumWidth(540)
        self.setMinimumHeight(440)
        self.resize(696, 624)

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
        tabs.addTab(self._create_right_panel_tab(), "Right Panel")
        tabs.addTab(self._create_debug_log_tab(), "Debug Log")

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

    def _create_interface_tab(self): #vers 2
        """Interface settings — compact 2-3 items per row."""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        layout.setSpacing(8)

        def spin(attr, label, lo, hi, val, suffix=''):
            w = QSpinBox(); w.setRange(lo, hi); w.setValue(val)
            if suffix: w.setSuffix(suffix)
            w.setMaximumWidth(80)
            setattr(self, attr, w)
            return QLabel(label), w

        def combo(attr, label, items, current):
            w = QComboBox(); w.addItems(items); w.setCurrentText(current)
            setattr(self, attr, w)
            return QLabel(label), w

        def row(*widgets):
            h = QHBoxLayout(); h.setSpacing(8)
            for w in widgets: h.addWidget(w)
            h.addStretch()
            return h

        # ── Button Layout ──────────────────────────────────────────────────
        btn_grp = QGroupBox("Button Layout")
        btn_lay = QVBoxLayout(btn_grp); btn_lay.setSpacing(4)
        lh, self.h_spacing_spin = spin('h_spacing_spin', 'H spacing:', 0, 50,
            self.img_settings.get('button_horizontal_spacing', 10), ' px')
        lv, self.v_spacing_spin = spin('v_spacing_spin', 'V spacing:', 0, 50,
            self.img_settings.get('button_vertical_spacing', 10), ' px')
        btn_lay.addLayout(row(lh, self.h_spacing_spin, lv, self.v_spacing_spin))
        layout.addWidget(btn_grp)

        # ── Font Settings ──────────────────────────────────────────────────
        fnt_grp = QGroupBox("Font Settings")
        fnt_lay = QVBoxLayout(fnt_grp); fnt_lay.setSpacing(4)

        self.use_custom_font_cb = QCheckBox("Use custom font")
        self.use_custom_font_cb.setChecked(self.img_settings.get('use_custom_font', False))
        fnt_lay.addWidget(self.use_custom_font_cb)

        font_row = QHBoxLayout(); font_row.setSpacing(8)
        font_row.addWidget(QLabel("Font:"))
        self.font_combo = QFontComboBox()
        self.font_combo.setCurrentFont(QFont(self.img_settings.get('font_family', 'Segoe UI')))
        self.font_combo.setEnabled(self.use_custom_font_cb.isChecked())
        font_row.addWidget(self.font_combo, 1)
        font_row.addWidget(QLabel("Size:"))
        self.font_size_spin = QSpinBox(); self.font_size_spin.setRange(6, 24)
        self.font_size_spin.setValue(self.img_settings.get('font_size', 9))
        self.font_size_spin.setSuffix(' pt'); self.font_size_spin.setMaximumWidth(70)
        self.font_size_spin.setEnabled(self.use_custom_font_cb.isChecked())
        font_row.addWidget(self.font_size_spin)
        self.font_bold_cb = QCheckBox("Bold")
        self.font_bold_cb.setChecked(self.img_settings.get('font_bold', False))
        self.font_bold_cb.setEnabled(self.use_custom_font_cb.isChecked())
        font_row.addWidget(self.font_bold_cb)
        self.font_italic_cb = QCheckBox("Italic")
        self.font_italic_cb.setChecked(self.img_settings.get('font_italic', False))
        self.font_italic_cb.setEnabled(self.use_custom_font_cb.isChecked())
        font_row.addWidget(self.font_italic_cb)
        fnt_lay.addLayout(font_row)

        def toggle_font_controls(checked):
            for w in [self.font_combo, self.font_size_spin,
                      self.font_bold_cb, self.font_italic_cb]:
                w.setEnabled(checked)
        self.use_custom_font_cb.toggled.connect(toggle_font_controls)
        layout.addWidget(fnt_grp)

        # ── Tab Settings ───────────────────────────────────────────────────
        tab_grp = QGroupBox("Tab Settings")
        tab_lay = QVBoxLayout(tab_grp); tab_lay.setSpacing(4)

        # Row 1: height + min-width
        lth, self.tab_height_spin = spin('tab_height_spin', 'Height:', 16, 60,
            self.img_settings.get('tab_height', 24), ' px')
        ltw, self.tab_min_width_spin = spin('tab_min_width_spin', 'Min width:', 40, 300,
            self.img_settings.get('tab_min_width', 100), ' px')
        tab_lay.addLayout(row(lth, self.tab_height_spin, ltw, self.tab_min_width_spin))

        # Row 2: style + content + position
        lts, self.tab_style_combo = combo('tab_style_combo', 'Style:',
            ['default', 'rounded', 'square'],
            self.img_settings.get('tab_style', 'default'))
        self.tab_style_combo.setMaximumWidth(100)
        mode_map = {'both': 0, 'icon': 1, 'text': 2}
        self.tab_content_combo = QComboBox()
        self.tab_content_combo.addItems(['Icon + Text', 'Icon Only', 'Text Only'])
        self.tab_content_combo.setCurrentIndex(
            mode_map.get(self.img_settings.get('tab_content_mode', 'both'), 0))
        self.tab_content_combo.setMaximumWidth(100)
        ltp, self.tab_position_combo = combo('tab_position_combo', 'Position:',
            ['top', 'bottom'], self.img_settings.get('tab_position', 'top'))
        self.tab_position_combo.setMaximumWidth(80)
        tab_lay.addLayout(row(lts, self.tab_style_combo,
                              QLabel('Content:'), self.tab_content_combo,
                              ltp, self.tab_position_combo))
        layout.addWidget(tab_grp)

        # ── Workshop Panel Collapse ─────────────────────────────────────────
        col_grp = QGroupBox("Workshop Panel Collapse")
        col_lay = QVBoxLayout(col_grp); col_lay.setSpacing(4)
        desc = QLabel("Side panels switch from text+icon to icon-only\n"
                      "when the right panel width drops below this value.")
        desc.setStyleSheet("color: #888; font-size: 10px;")
        col_lay.addWidget(desc)

        slider_row = QHBoxLayout(); slider_row.setSpacing(8)
        slider_row.addWidget(QLabel("Threshold:"))
        self.collapse_slider = QSlider(Qt.Orientation.Horizontal)
        self.collapse_slider.setRange(200, 900)
        self.collapse_slider.setSingleStep(10)
        self.collapse_slider.setValue(self.img_settings.get('panel_collapse_threshold', 550))
        slider_row.addWidget(self.collapse_slider)
        self.collapse_label = QLabel(f"{self.collapse_slider.value()} px")
        self.collapse_label.setMinimumWidth(55)
        slider_row.addWidget(self.collapse_label)
        self.collapse_slider.valueChanged.connect(
            lambda v: self.collapse_label.setText(f"{v} px"))
        col_lay.addLayout(slider_row)
        hint_row = QHBoxLayout()
        hint_row.addWidget(QLabel("200 px (always icons)"))
        hint_row.addStretch()
        hint_row.addWidget(QLabel("900 px (always text)"))
        col_lay.addLayout(hint_row)
        layout.addWidget(col_grp)

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
        self._original_ui_mode = self.img_settings.get("ui_mode", "system")

        # System UI option
        system_container = QWidget()
        system_layout = QVBoxLayout(system_container)
        system_layout.setContentsMargins(0, 0, 0, 10)

        self.system_ui_radio = QRadioButton("System UI")
        system_layout.addWidget(self.system_ui_radio)

        # Restart required notice
        from PyQt6.QtWidgets import QLabel as _QL
        self._restart_notice = _QL("⚠  Restart required for UI mode changes to take effect")
        self._restart_notice.setStyleSheet("color: #f90; font-size: 10px; padding: 4px;")
        self._restart_notice.setVisible(False)

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

        # Connect radios to show restart notice
        def _on_mode_changed():
            if hasattr(self, '_restart_notice') and hasattr(self, '_original_ui_mode'):
                new_mode = "custom" if self.custom_ui_radio.isChecked() else "system"
                self._restart_notice.setVisible(new_mode != self._original_ui_mode)
        self.system_ui_radio.toggled.connect(_on_mode_changed)
        self.custom_ui_radio.toggled.connect(_on_mode_changed)

        if hasattr(self, '_restart_notice'):
            ui_mode_layout.addWidget(self._restart_notice)

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

        # ── IMG Factory menu orientation ─────────────────────────────────
        from PyQt6.QtWidgets import QGroupBox as _GB, QVBoxLayout as _VL, QRadioButton as _RB

        img_orient_group = _GB("IMG Factory — Menu Orientation")
        img_orient_layout = _VL(img_orient_group)
        img_orient = self.img_settings.get("img_menu_orientation", "topbar")
        self.img_menu_topbar_radio   = _RB("Topbar  (menubar below titlebar)")
        self.img_menu_dropdown_radio = _RB("Dropdown  (Menu button popup)")
        self.img_menu_topbar_radio.setChecked(img_orient == "topbar")
        self.img_menu_dropdown_radio.setChecked(img_orient != "topbar")
        img_orient_layout.addWidget(self.img_menu_topbar_radio)
        img_orient_layout.addWidget(self.img_menu_dropdown_radio)
        img_orient_group.setLayout(img_orient_layout)
        appearance_layout.addWidget(img_orient_group)



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

    def _create_right_panel_tab(self): #vers 1
        """Right Panel — button height, spacing, display mode for the IMG/Entries/Options sections."""
        from PyQt6.QtWidgets import QSlider
        widget = QWidget()
        layout = QVBoxLayout(widget)
        layout.setSpacing(8)

        # Read from app_settings (where gui_layout reads them)
        app_s = getattr(self.main_window, 'app_settings', None)
        cs = app_s.current_settings if app_s else {}

        # ── Button Size & Spacing ──────────────────────────────────────────
        size_grp = QGroupBox("Button Size && Spacing")
        size_lay = QVBoxLayout(size_grp); size_lay.setSpacing(6)

        def spin_row(label, attr, lo, hi, val, suffix=' px'):
            from PyQt6.QtWidgets import QSpinBox
            row = QHBoxLayout()
            row.addWidget(QLabel(label))
            sp = QSpinBox(); sp.setRange(lo, hi); sp.setValue(val)
            sp.setSuffix(suffix); sp.setMaximumWidth(80)
            setattr(self, attr, sp)
            row.addWidget(sp); row.addStretch()
            return row

        size_lay.addLayout(spin_row("Button height:",
            'rp_btn_height', 16, 60, cs.get('button_height', 32)))
        size_lay.addLayout(spin_row("Vertical spacing:",
            'rp_v_spacing', 0, 30, cs.get('button_spacing_vertical', 8)))
        size_lay.addLayout(spin_row("Horizontal spacing:",
            'rp_h_spacing', 0, 30, cs.get('button_spacing_horizontal', 6)))
        layout.addWidget(size_grp)

        # ── Button Display Mode ────────────────────────────────────────────
        mode_grp = QGroupBox("Button Display Mode")
        mode_lay = QVBoxLayout(mode_grp); mode_lay.setSpacing(4)
        cur_mode = cs.get('button_display_mode', 'text_only')
        self.rp_mode_icon   = QRadioButton("Icon only")
        self.rp_mode_text   = QRadioButton("Text only")
        self.rp_mode_both   = QRadioButton("Icon + Text")
        self.rp_mode_icon.setChecked(cur_mode == 'icon_only')
        self.rp_mode_text.setChecked(cur_mode == 'text_only')
        self.rp_mode_both.setChecked(cur_mode == 'both')
        for rb in (self.rp_mode_icon, self.rp_mode_text, self.rp_mode_both):
            mode_lay.addWidget(rb)
        layout.addWidget(mode_grp)

        layout.addStretch()
        return widget

    def _save_right_panel_tab(self): #vers 1
        """Save right panel settings back to app_settings and apply live."""
        app_s = getattr(self.main_window, 'app_settings', None)
        if not app_s:
            return
        if hasattr(self, 'rp_btn_height'):
            app_s.current_settings['button_height'] = self.rp_btn_height.value()
        if hasattr(self, 'rp_v_spacing'):
            app_s.current_settings['button_spacing_vertical'] = self.rp_v_spacing.value()
        if hasattr(self, 'rp_h_spacing'):
            app_s.current_settings['button_spacing_horizontal'] = self.rp_h_spacing.value()
        mode = ('icon_only' if getattr(self, 'rp_mode_icon', None) and self.rp_mode_icon.isChecked()
                else 'both' if getattr(self, 'rp_mode_both', None) and self.rp_mode_both.isChecked()
                else 'text_only')
        app_s.current_settings['button_display_mode'] = mode
        app_s.save_settings()
        # Apply live
        try:
            gl = getattr(self.main_window, 'gui_layout', None)
            if gl:
                gl.button_display_mode = mode
                if hasattr(gl, '_update_all_buttons_display_mode'):
                    gl._update_all_buttons_display_mode()
        except Exception:
            pass

    def _create_debug_log_tab(self): #vers 2
        """Create Debug Log tab - output destinations + per-feature toggles."""
        from PyQt6.QtWidgets import QGridLayout, QScrollArea, QGroupBox
        widget = QWidget()
        layout = QVBoxLayout(widget)

        # --- Master switch ---
        master_group = QGroupBox("Debug Mode")
        master_layout = QVBoxLayout(master_group)
        self._debug_mode_cb = QCheckBox("Enable debug logging")
        self._debug_mode_cb.setChecked(self.img_settings.get('debug_mode', False))
        master_layout.addWidget(self._debug_mode_cb)
        layout.addWidget(master_group)

        # --- Output destinations ---
        dest_group = QGroupBox("Output Destinations")
        dest_layout = QVBoxLayout(dest_group)
        self._debug_out_terminal = QCheckBox("Terminal (stdout)")
        self._debug_out_file     = QCheckBox("Log file  (" + self.img_settings.get('log_file_path', 'imgfactory_activity.log') + ")")
        self._debug_out_activity = QCheckBox("Activity window")
        self._debug_out_terminal.setChecked(self.img_settings.get('debug_output_terminal', True))
        self._debug_out_file.setChecked(self.img_settings.get('debug_output_file', False))
        self._debug_out_activity.setChecked(self.img_settings.get('debug_output_activity', True))
        for cb in (self._debug_out_terminal, self._debug_out_file, self._debug_out_activity):
            dest_layout.addWidget(cb)
        layout.addWidget(dest_group)

        # --- Per-feature toggles ---
        feat_group = QGroupBox("Log Features  (only active when debug mode is on)")
        feat_layout = QGridLayout(feat_group)
        self._debug_log_checks = {}
        _entries = [
            ('import_via',  'Import Via'),
            ('export_via',  'Export Via'),
            ('rename',      'Rename'),
            ('remove',      'Remove'),
            ('pin',         'Pin / Unpin'),
            ('open',        'Open / Load'),
            ('save',        'Save'),
            ('rebuild',     'Rebuild'),
            ('extract',     'Extract'),
            ('col',         'COL Operations'),
            ('split',       'Split'),
            ('merge',       'Merge'),
        ]
        enabled = self.img_settings.get('debug_log_functions', [])
        for i, (key, label) in enumerate(_entries):
            cb = QCheckBox(label)
            cb.setChecked(key in enabled)
            self._debug_log_checks[key] = cb
            feat_layout.addWidget(cb, i // 2, i % 2)
        layout.addWidget(feat_group)

        layout.addStretch()
        return widget

    def _save_settings(self): #vers 2 Fixed
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

        # IMG Factory menu orientation — live apply
        img_orient = "topbar" if getattr(self, 'img_menu_topbar_radio', None) and self.img_menu_topbar_radio.isChecked() else "dropdown"
        self.img_settings.set("img_menu_orientation", img_orient)
        try:
            if hasattr(self.main_window, 'set_img_menu_orientation'):
                self.main_window.set_img_menu_orientation(img_orient)
        except Exception:
            pass

        # File Window tab
        self.img_settings.set("autoload_directory_tree", self.autoload_tree_cb.isChecked())
        self.img_settings.set("enable_pin_files", self.enable_pin_files_cb.isChecked())
        self.img_settings.set("auto_create_pin", self.auto_create_pin_cb.isChecked())

        # Tab settings
        self.img_settings.set("tab_height", self.tab_height_spin.value())
        self.img_settings.set("tab_min_width", self.tab_min_width_spin.value())
        self.img_settings.set("tab_style", self.tab_style_combo.currentText())
        mode_rev = {0: "both", 1: "icon", 2: "text"}
        self.img_settings.set("tab_content_mode",
            mode_rev.get(self.tab_content_combo.currentIndex(), "both"))
        self.img_settings.set("tab_position", self.tab_position_combo.currentText())

        # Panel collapse threshold
        if hasattr(self, 'collapse_slider'):
            self.img_settings.set("panel_collapse_threshold", self.collapse_slider.value())

        # Advanced tab
        self.img_settings.set("recent_files_limit", self.recent_files_spin.value())
        self.img_settings.set("auto_backup", self.auto_backup_cb.isChecked())
        self.img_settings.set("backup_count", self.backup_count_spin.value())

        # Debug Log tab
        if hasattr(self, '_debug_mode_cb'):
            self.img_settings.set('debug_mode', self._debug_mode_cb.isChecked())
        if hasattr(self, '_debug_out_terminal'):
            self.img_settings.set('debug_output_terminal', self._debug_out_terminal.isChecked())
            self.img_settings.set('debug_output_file',     self._debug_out_file.isChecked())
            self.img_settings.set('debug_output_activity', self._debug_out_activity.isChecked())
        if hasattr(self, '_debug_log_checks'):
            self.img_settings.set('debug_log_functions',
                [k for k, cb in self._debug_log_checks.items() if cb.isChecked()])
        # Re-sync the live debugger
        try:
            from apps.debug.debug_functions import img_debugger
            img_debugger.set_main_window(self.main_window)
        except Exception:
            pass

        # Right Panel tab
        self._save_right_panel_tab()

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

    def _save_and_close(self): #vers 2
        """Save settings, handle restart if UI mode changed, then close."""
        self._save_settings()
        self._apply_settings()
        self.accept()

        # Check if UI mode changed — requires restart
        new_mode = "custom" if self.custom_ui_radio.isChecked() else "system"
        if new_mode != self._original_ui_mode:
            self._prompt_restart()

    def _prompt_restart(self): #vers 1
        """If files are open, ask user to save first, then restart."""
        mw = self.main_window

        # Check for open files
        open_files = getattr(mw, 'open_files', {})
        has_open = bool(open_files)

        if has_open:
            reply = QMessageBox.question(
                mw,
                "Restart Required",
                ("UI mode has changed — a restart is required.\n\n"
                 "You have files open. Please save all work before restarting.\n\n"
                 "Save all and restart now?"),
                QMessageBox.StandardButton.Yes |
                QMessageBox.StandardButton.No |
                QMessageBox.StandardButton.Cancel,
                QMessageBox.StandardButton.Cancel
            )
            if reply == QMessageBox.StandardButton.Cancel:
                return
            if reply == QMessageBox.StandardButton.Yes:
                # Try to save all open files
                try:
                    if hasattr(mw, 'save_all_open_files'):
                        mw.save_all_open_files()
                    elif hasattr(mw, 'save_img_file'):
                        mw.save_img_file()
                except Exception as _se:
                    QMessageBox.warning(mw, "Save Error",
                        f"Could not save all files: {_se}\n\nPlease save manually then restart.")
                    return
        else:
            reply = QMessageBox.question(
                mw,
                "Restart Required",
                "UI mode has changed — a restart is required to apply changes.\n\nRestart now?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.Yes
            )
            if reply != QMessageBox.StandardButton.Yes:
                return

        self._do_restart(mw)

    def _do_restart(self, mw): #vers 1
        """Restart the application."""
        import sys, os
        from PyQt6.QtWidgets import QApplication
        try:
            QApplication.quit()
            os.execv(sys.executable, [sys.executable] + sys.argv)
        except Exception as e:
            QMessageBox.critical(mw, "Restart Failed",
                f"Could not restart automatically: {e}\n\nPlease close and reopen IMG Factory.")

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



def get_collapse_threshold(main_window=None) -> int:
    """Read panel_collapse_threshold from IMG Factory settings.
    Falls back to 550 if settings unavailable."""
    try:
        settings = None
        if main_window and hasattr(main_window, 'img_settings'):
            settings = main_window.img_settings
        if settings is None:
            from apps.methods.img_factory_settings import IMGFactorySettings
            settings = IMGFactorySettings()
        return settings.get_panel_collapse_threshold()
    except Exception:
        return 550

def show_imgfactory_settings_dialog(main_window): #vers 2
    """Show IMG Factory settings dialog — non-modal, movable, reused if open."""
    try:
        # Reuse existing instance if already open
        existing = getattr(main_window, '_imgfactory_settings_dialog', None)
        if existing is not None:
            try:
                if existing.isVisible():
                    existing.raise_()
                    existing.activateWindow()
                    return
            except RuntimeError:
                pass  # C++ object deleted

        dialog = IMGFactorySettingsDialog(main_window)
        main_window._imgfactory_settings_dialog = dialog
        dialog.setAttribute(Qt.WidgetAttribute.WA_DeleteOnClose, False)
        dialog.show()
        dialog.raise_()
    except Exception as e:
        QMessageBox.warning(
            main_window,
            "Error",
            f"Failed to open IMG Factory Settings: {str(e)}"
        )
