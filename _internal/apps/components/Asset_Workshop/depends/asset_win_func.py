#this belongs in apps/components/Asset_Workshop/depends/asset_win_func.py - Version: 2
# X-Seti - October 29 2026 - IMG Factory 1.6 - Asset Workshop window

"""
Asset Workshop window: titlebar, settings, hotkeys, drag and drop.
Mixed into AssetWorkshop (asset_workshop.py) ahead of TXDWorkshop.
"""

##class AssetWinMixin: -
# _config_path
# _create_status_bar
# _dropped_files
# _get_ui_color
# _is_on_draggable_area
# _refresh_icons
# _setup_hotkeys
# _show_settings_context_menu
# _show_settings_hotkeys
# _show_workshop_settings
# _titlebar_actions
# _titlebar_info
# _toggle_upscale_native
# _update_all_buttons
# dragEnterEvent
# dropEvent
# get_menu_title
# resizeEvent

from PyQt6.QtWidgets import QPushButton


from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.asset_checker import check_assets


App_name = "Asset Workshop"


class AssetWinMixin: #vers 1
    """Asset Workshop window: titlebar, settings, hotkeys, drag and drop."""

    def _get_ui_color(self, key): #vers 2
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)

    def get_menu_title(self) -> str: #vers 1
        """Return menu label for imgfactory menu bar."""
        return "TXD"

    def _dropped_files(self, event): #vers 1
        """Local .dat/.img/.col/.ide/.txd paths in a drag."""
        md = event.mimeData()
        if not md.hasUrls():
            return []
        exts = ('.dat', '.img', '.col', '.ide', '.txd')
        return [u.toLocalFile() for u in md.urls()
                if u.isLocalFile() and u.toLocalFile().lower().endswith(exts)]

    def dragEnterEvent(self, event): #vers 1
        """Accept game asset files."""
        if self._dropped_files(event):
            event.acceptProposedAction()
        else:
            event.ignore()

    def dropEvent(self, event): #vers 1
        """.dat loads a game; img/col/ide are checked; .txd opens."""
        paths = self._dropped_files(event)
        if not paths:
            event.ignore()
            return
        event.acceptProposedAction()
        by = lambda ext: [p for p in paths if p.lower().endswith(ext)]
        dats = by('.dat')
        if dats:
            self._load_from_dat_path(dats[0])
            return
        imgs, cols, ides = by('.img'), by('.col'), by('.ide')
        if imgs or cols or ides:
            self._all_img_paths, self._all_col_paths, self._all_ide_paths = [], [], []
            self.load_result(check_assets(img_path=imgs, col_path=cols, ide_path=ides))
        for p in by('.txd'):
            self.open_txd_file(p)

    def _config_path(self): #vers 3
        return self._ribbon_config_path()

    def _create_status_bar(self): #vers 6
        """Create bottom status bar - single line compact"""
        from PyQt6.QtWidgets import QFrame, QHBoxLayout, QLabel

        status_bar = QFrame()
        status_bar.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        status_bar.setFixedHeight(22)

        layout = QHBoxLayout(status_bar)
        layout.setContentsMargins(5, 0, 5, 0)
        layout.setSpacing(15)

        # Left: Ready
        self.status_label = QLabel("Ready")
        layout.addWidget(self.status_label)

        # TXD info
        self.status_txd_info = QLabel("TXD: None")
        layout.addWidget(self.status_txd_info)

        layout.addStretch()

        # Right: Size and Format
        self.info_size = QLabel("Size: -")
        layout.addWidget(self.info_size)

        self.format_status_label = QLabel("Format: -")
        layout.addWidget(self.format_status_label)

        return status_bar

    def _show_workshop_settings(self): #vers 7
        """Show workshop settings dialog - Fonts/Display/Preview only,
        Export and Performance tabs removed (values were never read
        anywhere, and Texture List Display referenced a table that
        no longer exists)."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                    QTabWidget, QWidget, QGroupBox, QFormLayout,
                                    QSpinBox, QComboBox, QSlider, QLabel, QCheckBox,
                                    QFontComboBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
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

        # Asset Check
        asset_check_group = QGroupBox("Asset Check")
        asset_check_layout = QVBoxLayout()

        auto_gta3_img_chk = QCheckBox("Automatically search for gta3.img")
        auto_gta3_img_chk.setChecked(getattr(self, 'auto_find_gta3_img', True))
        auto_gta3_img_chk.setToolTip(
            "GTA III/VC's gta3.img is hard-coded into the engine - the "
            ".dat never declares it. When Load from .dat finds no IMG "
            "at all, fall back to game_root/models/gta3.img.")
        asset_check_layout.addWidget(auto_gta3_img_chk)

        asset_check_group.setLayout(asset_check_layout)
        display_layout.addWidget(asset_check_group)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")

        # TAB 3: PREVIEW SETTINGS (LAST TAB)

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
        from apps.methods.ui_sizes import add_sizes_tab       # own sizes or global
        add_sizes_tab(self, tabs)
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

            # ASSET CHECK
            self.auto_find_gta3_img = auto_gta3_img_chk.isChecked()

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
                self.main_window.log_message(App_name + " settings updated successfully")

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

    def _is_on_draggable_area(self, pos): #vers 3
        """Check if position is on draggable toolbar area (stretch space, not buttons)"""
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

    def resizeEvent(self, event): #vers 2
        """Keep resize grip in corner; auto-collapse text panel when narrow."""
        super().resizeEvent(event)
        if hasattr(self, 'size_grip'):
            self.size_grip.move(self.width() - 16, self.height() - 16)
        self._update_transform_text_panel_visibility()

    def _titlebar_actions(self, layout): #vers 2
        """Asset titlebar: OpenIMG (docked), Undo."""
        if not self.standalone_mode:
            self.open_img_btn = self._tb_button('folder_icon', "OpenIMG", "Open an IMG archive",
                                                self.open_img_archive)
            layout.addWidget(self.open_img_btn)
        self.undo_btn = self._tb_button('undo_icon', "Undo", "Undo last change", self._undo_last_action)
        self.undo_btn.setEnabled(False)
        layout.addWidget(self.undo_btn)
        self._save_buttons = []                          # no Save button; TXD code updates these lists
        self._undo_buttons = [self.undo_btn]

    def _titlebar_info(self): #vers 1
        """Info button: asset info."""
        self._show_txd_info()

    def _show_settings_context_menu(self, pos): #vers 2
        """Show context menu for Settings button"""
        from PyQt6.QtWidgets import QMenu

        menu = QMenu(self)

        # Move window action
        move_action = menu.addAction("Move Window")
        move_action.triggered.connect(self._enable_move_mode)

        # Maximize window action
        max_action = menu.addAction("Maximize Window")
        max_action.triggered.connect(self._toggle_maximize)

        # Minimize action
        min_action = menu.addAction("Minimize")
        min_action.triggered.connect(self.showMinimized)

        menu.addSeparator()

        # Upscale Native action
        upscale_action = menu.addAction("Upscale Native")
        upscale_action.setCheckable(True)
        upscale_action.setChecked(False)
        upscale_action.triggered.connect(self._toggle_upscale_native)

        # Shaders action
        shaders_action = menu.addAction("Shaders")
        shaders_action.triggered.connect(self._show_shaders_dialog)

        menu.addSeparator()

        # Icon display mode submenu — auto-compact handled by resizeEvent
        display_menu = menu.addMenu("Button Display")

        icons_text_action = display_menu.addAction("Icons & Text")
        icons_text_action.setCheckable(True)
        icons_text_action.setChecked(self.button_display_mode == 'both')
        icons_text_action.triggered.connect(lambda: self._set_icon_display_mode('both'))

        icons_only_action = display_menu.addAction("Icons Only")
        icons_only_action.setCheckable(True)
        icons_only_action.setChecked(self.button_display_mode == 'icons')
        icons_only_action.triggered.connect(lambda: self._set_icon_display_mode('icons'))

        text_only_action = display_menu.addAction("Text Only")
        text_only_action.setCheckable(True)
        text_only_action.setChecked(self.button_display_mode == 'text')
        text_only_action.triggered.connect(lambda: self._set_icon_display_mode('text'))

        # Show menu at button position
        menu.exec(self.properties_btn.mapToGlobal(pos))

    def _toggle_upscale_native(self): #vers 1
        """Toggle upscale native resolution"""
        # Placeholder for upscale native functionality
        print("Upscale Native toggled")

    def _refresh_icons(self): #vers 13
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
            ('gen_alpha_btn',       'generate_alpha_icon'),
            ('props_btn',           'texture_properties_icon'),
            # Info panel buttons
            ('import_btn',          'texture_import_icon'),
            ('export_btn',          'texture_export_icon'),
            ('convert_btn',         'format_convert_icon'),
            ('properties_btn',      'settings_icon'),
            ('analyze_btn',         'analyze_icon'),
            # Mipmap row
            ('create_mipmaps_btn',  'generate_mipmaps_icon'),
            ('remove_mipmaps_btn',  'remove_mipmaps_icon'),
            ('show_mipmaps_btn',    'view_icon'),
            ('compress_btn',        'compress_icon'),
            ('uncompress_btn',      'uncompress_icon'),
            ('upscale_btn',         'upscale_icon'),
            # Bumpmap row
            ('import_bumpmap_btn',  'bumpmap_imp_icon'),
            ('export_bumpmap_btn',  'bumpmap_ext_icon'),
            ('view_bumpmap_btn',    'manage_bumpmaps_icon'),
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

        # Update middle btn row visibility (may have changed docked state)
        if hasattr(self, '_middle_btn_row'):
            self._middle_btn_row.setVisible(
                self.is_docked and not self.standalone_mode)
        self._apply_custom_icons()

    def _setup_hotkeys(self): #vers 3
        """Setup Plasma6-style keyboard shortcuts for TXD Workshop - checks for existing methods"""
        from PyQt6.QtGui import QShortcut, QKeySequence

        # === FILE OPERATIONS ===

        # Open TXD (Ctrl+O)
        self.hotkey_open = QShortcut(QKeySequence.StandardKey.Open, self)
        if hasattr(self, 'open_txd_file'):
            self.hotkey_open.activated.connect(self.open_txd_file)
        elif hasattr(self, '_open_txd_file'):
            self.hotkey_open.activated.connect(self._open_txd_file)

        # Save TXD (Ctrl+S)
        self.hotkey_save = QShortcut(QKeySequence.StandardKey.Save, self)
        if hasattr(self, '_save_txd_file'):
            self.hotkey_save.activated.connect(self._save_txd_file)
        elif hasattr(self, 'save_txd_file'):
            self.hotkey_save.activated.connect(self.save_txd_file)

        # Force Save TXD (Alt+Shift+S)
        self.hotkey_force_save = QShortcut(QKeySequence("Alt+Shift+S"), self)
        if not hasattr(self, '_force_save_txd'):
            # Create force save method inline if it doesn't exist
            def force_save():  #vers 1
                if not self.texture_list:
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.warning(self, "No Textures", "No textures to save")
                    return
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Force save triggered (Alt+Shift+S)")
                # Call save regardless of modified state
                if hasattr(self, '_save_txd_file'):
                    self._save_txd_file()
                elif hasattr(self, 'save_txd_file'):
                    self.save_txd_file()
            self.hotkey_force_save.activated.connect(force_save)
        else:
            self.hotkey_force_save.activated.connect(self._force_save_txd)

        # Save As (Ctrl+Shift+S)
        self.hotkey_save_as = QShortcut(QKeySequence.StandardKey.SaveAs, self)
        if hasattr(self, '_save_as_txd_file'):
            self.hotkey_save_as.activated.connect(self._save_as_txd_file)
        elif hasattr(self, 'save_as_txd_file'):
            self.hotkey_save_as.activated.connect(self.save_as_txd_file)
        elif hasattr(self, '_save_txd_file'):
            self.hotkey_save_as.activated.connect(self._save_txd_file)

        # Close (Ctrl+W)
        self.hotkey_close = QShortcut(QKeySequence.StandardKey.Close, self)
        self.hotkey_close.activated.connect(self.close)

        # === EDIT OPERATIONS ===

        # Undo (Ctrl+Z)
        self.hotkey_undo = QShortcut(QKeySequence.StandardKey.Undo, self)
        if hasattr(self, '_undo_last_action'):
            self.hotkey_undo.activated.connect(self._undo_last_action)
        elif hasattr(self, 'undo_last_action'):
            self.hotkey_undo.activated.connect(self.undo_last_action)
        # else: not implemented yet, no connection

        # Copy (Ctrl+C)
        self.hotkey_copy = QShortcut(QKeySequence.StandardKey.Copy, self)
        if hasattr(self, '_copy_texture'):
            self.hotkey_copy.activated.connect(self._copy_texture)
        elif hasattr(self, 'copy_texture'):
            self.hotkey_copy.activated.connect(self.copy_texture)

        # Paste (Ctrl+V)
        self.hotkey_paste = QShortcut(QKeySequence.StandardKey.Paste, self)
        if hasattr(self, '_paste_texture'):
            self.hotkey_paste.activated.connect(self._paste_texture)
        elif hasattr(self, 'paste_texture'):
            self.hotkey_paste.activated.connect(self.paste_texture)

        # Delete (Delete)
        self.hotkey_delete = QShortcut(QKeySequence.StandardKey.Delete, self)
        if hasattr(self, '_delete_texture'):
            self.hotkey_delete.activated.connect(self._delete_texture)
        elif hasattr(self, 'delete_texture'):
            self.hotkey_delete.activated.connect(self.delete_texture)

        # Duplicate (Ctrl+D)
        self.hotkey_duplicate = QShortcut(QKeySequence("Ctrl+D"), self)
        if hasattr(self, '_duplicate_texture'):
            self.hotkey_duplicate.activated.connect(self._duplicate_texture)
        elif hasattr(self, 'duplicate_texture'):
            self.hotkey_duplicate.activated.connect(self.duplicate_texture)

        # Rename (F2)
        self.hotkey_rename = QShortcut(QKeySequence("F2"), self)
        if not hasattr(self, '_rename_texture_shortcut'):
            # Create rename shortcut method inline
            def rename_shortcut():  #vers 1
                if not self.selected_texture:
                    return
                # Focus the name input field if it exists
                if hasattr(self, 'info_name'):
                    self.info_name.setReadOnly(False)
                    self.info_name.selectAll()
                    self.info_name.setFocus()
            self.hotkey_rename.activated.connect(rename_shortcut)
        else:
            self.hotkey_rename.activated.connect(self._rename_texture_shortcut)

        # === TEXTURE OPERATIONS ===

        # Import Texture (Ctrl+I)
        self.hotkey_import = QShortcut(QKeySequence("Ctrl+I"), self)
        if hasattr(self, '_import_normal_texture'):
            self.hotkey_import.activated.connect(self._import_normal_texture)
        elif hasattr(self, 'import_normal_texture'):
            self.hotkey_import.activated.connect(self.import_normal_texture)
        elif hasattr(self, 'import_textures'):
            self.hotkey_import.activated.connect(self.import_textures)

        # Export Texture (Ctrl+E)
        self.hotkey_export = QShortcut(QKeySequence("Ctrl+E"), self)
        if hasattr(self, 'export_selected_texture'):
            self.hotkey_export.activated.connect(self.export_selected_texture)
        elif hasattr(self, '_export_selected_texture'):
            self.hotkey_export.activated.connect(self._export_selected_texture)
        elif hasattr(self, 'export_texture'):
            self.hotkey_export.activated.connect(self.export_texture)

        # Export All (Ctrl+Shift+E)
        self.hotkey_export_all = QShortcut(QKeySequence("Ctrl+Shift+E"), self)
        if hasattr(self, 'export_all_textures'):
            self.hotkey_export_all.activated.connect(self.export_all_textures)
        elif hasattr(self, '_export_all_textures'):
            self.hotkey_export_all.activated.connect(self._export_all_textures)

        # === VIEW OPERATIONS ===

        # Refresh (F5)
        self.hotkey_refresh = QShortcut(QKeySequence.StandardKey.Refresh, self)
        if hasattr(self, '_reload_texture_table'):
            self.hotkey_refresh.activated.connect(self._reload_texture_table)
        elif hasattr(self, 'reload_texture_table'):
            self.hotkey_refresh.activated.connect(self.reload_texture_table)
        elif hasattr(self, 'refresh'):
            self.hotkey_refresh.activated.connect(self.refresh)

        # Properties (Alt+Enter)
        self.hotkey_properties = QShortcut(QKeySequence("Alt+Return"), self)
        if hasattr(self, '_show_detailed_info'):
            self.hotkey_properties.activated.connect(self._show_detailed_info)
        elif hasattr(self, '_show_texture_info'):
            self.hotkey_properties.activated.connect(self._show_texture_info)

        # Settings (Ctrl+,)
        self.hotkey_settings = QShortcut(QKeySequence.StandardKey.Preferences, self)
        if hasattr(self, '_show_settings_dialog'):
            self.hotkey_settings.activated.connect(self._show_settings_dialog)
        elif hasattr(self, 'show_settings_dialog'):
            self.hotkey_settings.activated.connect(self.show_settings_dialog)
        elif hasattr(self, '_show_settings_hotkeys'):
            self.hotkey_settings.activated.connect(self._show_settings_hotkeys)

        # === NAVIGATION ===

        # Select All (Ctrl+A) - reserved for future
        self.hotkey_select_all = QShortcut(QKeySequence.StandardKey.SelectAll, self)
        # Not connected - reserved for future multi-select

        # Find (Ctrl+F)
        self.hotkey_find = QShortcut(QKeySequence.StandardKey.Find, self)
        if not hasattr(self, '_focus_search'):
            # Create focus search method inline
            def focus_search():  #vers 1
                if hasattr(self, 'search_input'):
                    self.search_input.setFocus()
                    self.search_input.selectAll()
            self.hotkey_find.activated.connect(focus_search)
        else:
            self.hotkey_find.activated.connect(self._focus_search)

        # === HELP ===

        # Help (F1)
        self.hotkey_help = QShortcut(QKeySequence.StandardKey.HelpContents, self)

        if hasattr(self, 'show_help'):
            self.hotkey_help.activated.connect(self.show_help)

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Hotkeys initialized (Plasma6 standard)")

    def _show_settings_hotkeys(self): #vers 2
        """Key & Mouse Bindings editor (methods/input_bindings_dialog.py)."""
        self.open_bindings_editor()
