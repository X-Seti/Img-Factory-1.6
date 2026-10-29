#this belongs in apps/components/Map_Editor/depends/map_settings_func.py - Version: 8
# X-Seti - October 14 2026 - IMG Factory 1.6 - Map Workshop settings

"""
Workshop settings tabs, fonts, theme, hotkeys, About for ModelWorkshop (Map).
"""

##class MapSettingsMixin: -
# _apply_button_font
# _apply_infobar_font
# _apply_panel_font
# _apply_theme
# _apply_title_font
# _build_workshop_settings_tabs
# _enable_move_mode
# _get_icon_color
# _get_icon_set
# _get_ui_color
# _launch_theme_settings
# _make_world_map_icon
# _set_icon_display_mode
# _setup_hotkeys
# _show_about
# _show_settings_context_menu
# _show_settings_dialog
# _show_settings_hotkeys
# _show_shaders_dialog
# _show_workshop_settings
# _texture_limits
# _toggle_upscale_native

import os

from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor, QIcon, QPainter, QPixmap
from PyQt6.QtWidgets import (
    QApplication, QColorDialog, QDoubleSpinBox, QFileDialog, QGridLayout, QHBoxLayout,
    QLabel, QLineEdit, QMessageBox, QPushButton, QScrollArea, QVBoxLayout)

from apps.methods.statusbar_func import add_statusbar_checkbox, register_statusbar
from apps.components.Map_Editor.depends.map_widgets_func import (
    App_build, App_name, _model_workshop_config_dir)


class MapSettingsMixin: #vers 1
    """Workshop settings tabs, fonts, theme, hotkeys, About."""

    def _texture_limits(self): #vers 1
        """Per-format texture limits {group: (over, down_to)} from settings."""
        ms = self.map_settings
        limits = {}
        if ms.get('texture_downscale_enabled'):
            limits['rw'] = (ms.get('texture_downscale_threshold'), ms.get('texture_downscale_target'))
        for g in ('iv', 'stories', 'nif'):
            if ms.get(f'tex_limit_{g}_on'):
                limits[g] = (ms.get(f'tex_limit_{g}_over'), ms.get(f'tex_limit_{g}_to'))
        return limits

    def _build_workshop_settings_tabs(self): #vers 15
        """Build the workshop settings QTabWidget (Fonts/Display/
        Performance/Preview/Loading/Map Assets/Navigation) and the
        Apply callback that reads all their widgets back and
        persists the values - factored out of _show_workshop_settings
        (Aug 15 2026)"""
        from PyQt6.QtWidgets import (QTabWidget, QWidget, QGroupBox, QFormLayout,
                                    QSpinBox, QComboBox, QSlider, QLabel, QCheckBox,
                                    QFontComboBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

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
        button_hint.setStyleSheet("color: palette(placeholderText); font-style: italic;")
        button_layout.addWidget(button_hint)

        button_group.setLayout(button_layout)
        display_layout.addWidget(button_group)

        # IPL Controls display style (Aug 20 2026,  "we
        # could add a toggle in settings, Show IPL Controls = as
        # [Buttons] or ribbon icons") - a separate, real toggle from
        # the general "Button Display Mode" combo just above, since
        # that one controls this app's own main toolbars, not IPL
        # Controls' own overlay toggle buttons specifically.
        ipl_style_grp = QGroupBox("IPL Controls Display")
        ipl_style_layout = QVBoxLayout()
        ipl_icon_only_chk = QCheckBox("Show as ribbon icons (instead of text buttons)")
        ipl_icon_only_chk.setChecked(bool(self.map_settings.get('ipl_controls_icon_only')))
        ipl_icon_only_chk.setToolTip(
            "Each overlay toggle button in IPL Controls falls back to\n"
            "its own text label on its own if it doesn't have real icon\n"
            "artwork yet, so this is safe to try even before every\n"
            "button has one.")
        def _live_ipl_controls_style(checked): #vers 1
            self.map_settings.set('ipl_controls_icon_only', checked)
            self._apply_ipl_controls_display_style()
        ipl_icon_only_chk.toggled.connect(_live_ipl_controls_style)
        ipl_style_layout.addWidget(ipl_icon_only_chk)
        ipl_style_grp.setLayout(ipl_style_layout)
        display_layout.addWidget(ipl_style_grp)

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
        add_statusbar_checkbox(self, display_layout)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")

        # TAB: RENDER (Aug 16 2026)
        render_tab_outer = QWidget()
        render_tab_outer_layout = QVBoxLayout(render_tab_outer)
        render_tab_outer_layout.setContentsMargins(0, 0, 0, 0)
        render_scroll = QScrollArea()
        render_scroll.setWidgetResizable(True)
        render_tab = QWidget()
        render_layout = QVBoxLayout(render_tab)
        render_layout.setSpacing(6)
        render_scroll.setWidget(render_tab)
        render_tab_outer_layout.addWidget(render_scroll)
        pw = getattr(self, 'preview_widget', None)

        style_grp = QGroupBox("Object Rendering")
        sg = QHBoxLayout(style_grp)
        sg.addWidget(QLabel("Style:"))
        style_combo = QComboBox()
        style_combo.addItems(["Wireframe", "Solid", "Textured"])
        mapping = {"wireframe": "Wireframe", "solid": "Solid", "textured": "Textured"}
        cur_mode = getattr(pw, '_mode', 'solid') if pw else 'solid'
        style_combo.setCurrentText(mapping.get(cur_mode, "Solid"))
        sg.addWidget(style_combo)
        render_layout.addWidget(style_grp)

        bg_grp = QGroupBox("Background")
        bg = QHBoxLayout(bg_grp)
        if pw is not None:
            override = getattr(pw, '_bg_color_override', None)
            r, g, b = override if override is not None else (
                pw._get_ui_color('bg_panel').red(),
                pw._get_ui_color('bg_panel').green(),
                pw._get_ui_color('bg_panel').blue())
        else:
            r, g, b = 30, 30, 30
        bg_preview = QPushButton("  ")
        bg_preview.setFixedSize(60, 28)
        bg_preview.setStyleSheet(f"background-color: rgb({r},{g},{b});")
        def _pick_bg():  #vers 1
            c = QColorDialog.getColor(QColor(r, g, b), tabs, "Background Colour")
            if c.isValid():
                bg_preview.setStyleSheet(f"background-color: {c.name()};")
                bg_preview.setProperty("chosen", (c.red(), c.green(), c.blue()))
        bg_preview.clicked.connect(_pick_bg)
        bg.addWidget(QLabel("Colour:"))
        bg.addWidget(bg_preview)
        reset_btn = QPushButton("Use Theme")
        reset_btn.clicked.connect(lambda: bg_preview.setProperty("reset_to_theme", True))
        bg.addWidget(reset_btn)
        render_layout.addWidget(bg_grp)

        path_grp = QGroupBox("Path Lines")
        path_form = QFormLayout(path_grp)
        saved_path_color = self.map_settings.get('path_line_color') or (255, 0, 0)
        pr, pg_, pb = saved_path_color
        path_preview = QPushButton("  ")
        path_preview.setFixedSize(60, 24)
        path_preview.setStyleSheet(f"background-color: rgb({pr},{pg_},{pb});")
        def _pick_path_color():  #vers 1
            c = QColorDialog.getColor(QColor(pr, pg_, pb), tabs, "Path Line Colour")
            if c.isValid():
                path_preview.setStyleSheet(f"background-color: {c.name()};")
                path_preview.setProperty("chosen", (c.red(), c.green(), c.blue()))
        path_preview.clicked.connect(_pick_path_color)

        saved_node_color = self.map_settings.get('path_node_color') or (255, 204, 0)
        npr, npg, npb = saved_node_color
        node_preview = QPushButton("  ")
        node_preview.setFixedSize(60, 24)
        node_preview.setStyleSheet(f"background-color: rgb({npr},{npg},{npb});")
        def _pick_node_color():  #vers 1
            c = QColorDialog.getColor(QColor(npr, npg, npb), tabs, "Path Node Colour")
            if c.isValid():
                node_preview.setStyleSheet(f"background-color: {c.name()};")
                node_preview.setProperty("chosen", (c.red(), c.green(), c.blue()))
        node_preview.clicked.connect(_pick_node_color)

        line_thickness_spin = QDoubleSpinBox()
        line_thickness_spin.setRange(0.1, 20.0)
        line_thickness_spin.setSingleStep(0.1)
        line_thickness_spin.setValue(self.map_settings.get('path_line_thickness') or 1.2)

        node_size_spin = QDoubleSpinBox()
        node_size_spin.setRange(0.1, 30.0)
        node_size_spin.setSingleStep(0.5)
        node_size_spin.setValue(self.map_settings.get('path_node_size') or 3.5)

        dat_row = QHBoxLayout()
        dat_row.addWidget(QLabel("Line:")); dat_row.addWidget(path_preview)
        dat_row.addWidget(QLabel("Node:")); dat_row.addWidget(node_preview)
        dat_row.addWidget(QLabel("Thickness:")); dat_row.addWidget(line_thickness_spin)
        dat_row.addWidget(QLabel("Size:")); dat_row.addWidget(node_size_spin)
        path_form.addRow("Dat Nodes:", dat_row)

        saved_track_color = self.map_settings.get('track_color') or (191, 191, 204)
        tr, tg, tb = saved_track_color
        track_preview = QPushButton("  ")
        track_preview.setFixedSize(60, 24)
        track_preview.setStyleSheet(f"background-color: rgb({tr},{tg},{tb});")
        def _pick_track_color():  #vers 1
            c = QColorDialog.getColor(QColor(tr, tg, tb), tabs, "Track Colour")
            if c.isValid():
                track_preview.setStyleSheet(f"background-color: {c.name()};")
                track_preview.setProperty("chosen", (c.red(), c.green(), c.blue()))
        track_preview.clicked.connect(_pick_track_color)
        track_thickness_spin = QDoubleSpinBox()
        track_thickness_spin.setRange(0.1, 20.0)
        track_thickness_spin.setSingleStep(0.1)
        track_thickness_spin.setValue(self.map_settings.get('track_line_thickness') or 1.5)
        track_row = QHBoxLayout()
        track_row.addWidget(QLabel("Colour:")); track_row.addWidget(track_preview)
        track_row.addWidget(QLabel("Thickness:")); track_row.addWidget(track_thickness_spin)
        path_form.addRow("Tracks:", track_row)

        saved_airtrain_color = self.map_settings.get('airtrain_color') or (230, 153, 51)
        atr, atg, atb = saved_airtrain_color
        airtrain_preview = QPushButton("  ")
        airtrain_preview.setFixedSize(60, 24)
        airtrain_preview.setStyleSheet(f"background-color: rgb({atr},{atg},{atb});")
        def _pick_airtrain_color():  #vers 1
            c = QColorDialog.getColor(QColor(atr, atg, atb), tabs, "Airtrain/Plane Colour")
            if c.isValid():
                airtrain_preview.setStyleSheet(f"background-color: {c.name()};")
                airtrain_preview.setProperty("chosen", (c.red(), c.green(), c.blue()))
        airtrain_preview.clicked.connect(_pick_airtrain_color)
        airtrain_thickness_spin = QDoubleSpinBox()
        airtrain_thickness_spin.setRange(0.1, 20.0)
        airtrain_thickness_spin.setSingleStep(0.1)
        airtrain_thickness_spin.setValue(self.map_settings.get('airtrain_line_thickness') or 1.2)
        airtrain_row = QHBoxLayout()
        airtrain_row.addWidget(QLabel("Colour:")); airtrain_row.addWidget(airtrain_preview)
        airtrain_row.addWidget(QLabel("Thickness:")); airtrain_row.addWidget(airtrain_thickness_spin)
        path_form.addRow("Airtrain / Plane:", airtrain_row)

        render_layout.addWidget(path_grp)

        boxes_grp = QGroupBox("Cull / Zone / Occlusion Boxes")
        boxes_form = QFormLayout(boxes_grp)

        saved_cull_color = self.map_settings.get('cull_box_color') or (255, 217, 51)
        clr, clg, clb = saved_cull_color
        cull_preview = QPushButton("  ")
        cull_preview.setFixedSize(60, 28)
        cull_preview.setStyleSheet(f"background-color: rgb({clr},{clg},{clb});")
        def _pick_cull_color():  #vers 1
            c = QColorDialog.getColor(QColor(clr, clg, clb), tabs, "Cull Box Colour")
            if c.isValid():
                cull_preview.setStyleSheet(f"background-color: {c.name()};")
                cull_preview.setProperty("chosen", (c.red(), c.green(), c.blue()))
        cull_preview.clicked.connect(_pick_cull_color)
        boxes_form.addRow("Cull Colour:", cull_preview)

        saved_zone_color = self.map_settings.get('zone_box_color') or (77, 179, 255)
        zlr, zlg, zlb = saved_zone_color
        zone_preview = QPushButton("  ")
        zone_preview.setFixedSize(60, 28)
        zone_preview.setStyleSheet(f"background-color: rgb({zlr},{zlg},{zlb});")
        def _pick_zone_color():  #vers 1
            c = QColorDialog.getColor(QColor(zlr, zlg, zlb), tabs, "Zone Box Colour")
            if c.isValid():
                zone_preview.setStyleSheet(f"background-color: {c.name()};")
                zone_preview.setProperty("chosen", (c.red(), c.green(), c.blue()))
        zone_preview.clicked.connect(_pick_zone_color)
        boxes_form.addRow("Zone Colour:", zone_preview)

        saved_occl_color = self.map_settings.get('occl_box_color') or (255, 102, 204)
        olr, olg, olb = saved_occl_color
        occl_preview = QPushButton("  ")
        occl_preview.setFixedSize(60, 28)
        occl_preview.setStyleSheet(f"background-color: rgb({olr},{olg},{olb});")
        def _pick_occl_color():  #vers 1
            c = QColorDialog.getColor(QColor(olr, olg, olb), tabs, "Occlusion Box Colour")
            if c.isValid():
                occl_preview.setStyleSheet(f"background-color: {c.name()};")
                occl_preview.setProperty("chosen", (c.red(), c.green(), c.blue()))
        occl_preview.clicked.connect(_pick_occl_color)
        boxes_form.addRow("Occlusion Colour:", occl_preview)

        axis_colors_chk = QCheckBox("Colour box faces by axis (X=green, Y=red, Z=blue)")
        axis_colors_chk.setChecked(bool(self.map_settings.get('box_axis_colors')))
        axis_colors_chk.setToolTip(
            "Overrides each box type's own colour above with a fixed\n"
            "per-face scheme instead, so orientation is easy to read\n"
            "while resizing/editing a box - which face is which axis\n"
            "at a glance, regardless of the box's own type colour.")
        boxes_form.addRow(axis_colors_chk)

        unique_colors_chk = QCheckBox("Unique colour per box")
        unique_colors_chk.setChecked(bool(self.map_settings.get('box_unique_colors')))
        unique_colors_chk.setToolTip(
            "Each individual cull/zone/occlusion box gets its own\n"
            "colour from a fixed palette instead of every box of the\n"
            "same type sharing one colour - makes adjacent/overlapping\n"
            "zones easy to tell apart. Takes a back seat to axis-\n"
            "coloured faces above if both are on at once.")
        boxes_form.addRow(unique_colors_chk)

        no_clip_chk = QCheckBox("No-clip box resizing")
        no_clip_chk.setChecked(bool(self.map_settings.get('no_clip_boxes')))
        no_clip_chk.setToolTip(
            "While dragging a cull/zone box's own corner to resize it\n"
            "(right-click Cull or Zon to turn on box edit mode), a\n"
            "resize that would make the box overlap any other loaded\n"
            "cull/zone box is rejected - the box holds its last good\n"
            "size instead of jumping into the other one.")
        boxes_form.addRow(no_clip_chk)

        # Grid & Radar Tiles (Aug 20 2026)
        radar_grp = QGroupBox("Grid && Radar Tiles")
        radar_form = QFormLayout(radar_grp)

        pw_for_quick = getattr(self, 'preview_widget', None)
        quick_show_grid_chk = QCheckBox("Show Grid")
        quick_show_grid_chk.setChecked(bool(getattr(pw_for_quick, '_show_grid', True)))
        quick_show_grid_chk.setToolTip("Quick on/off for the whole reference grid, regardless of style.")
        def _on_quick_show_grid(checked): #vers 1
            vp = getattr(self, 'preview_widget', None)
            if vp is not None and hasattr(vp, 'set_show_grid'):
                vp.set_show_grid(checked)
        quick_show_grid_chk.toggled.connect(_on_quick_show_grid)

        quick_show_timecyc_chk = QCheckBox("Show Timecyc")
        quick_show_timecyc_chk.setChecked(bool(getattr(pw_for_quick, '_timecyc_playing', False)))
        quick_show_timecyc_chk.setToolTip(
            "Quick on/off for the timecyc day/night effect - same real\n"
            "switch as the [Tcyc] toolbar button, same row as [2DFX]/[Tobj].")
        def _on_quick_show_timecyc(checked): #vers 1
            vp = getattr(self, 'preview_widget', None)
            if vp is not None and hasattr(vp, 'set_timecyc_playing'):
                vp.set_timecyc_playing(checked)
            tcyc_btn = getattr(self, '_tcyc_chk', None)
            if tcyc_btn is not None:
                tcyc_btn.set_shown(checked, emit=False)
        quick_show_timecyc_chk.toggled.connect(_on_quick_show_timecyc)

        quick_row = QHBoxLayout()
        quick_row.addWidget(quick_show_grid_chk)
        quick_row.addWidget(quick_show_timecyc_chk)
        radar_form.addRow(quick_row)

        grid_type_combo = QComboBox()
        grid_type_items = [
            ('lines',            'Lines (default)'),
            # ('squares',          'Squares (blue fill)'),  # Temporarily disabled (Aug 20 2026,  "remove the square (blue fill) entry, temp comment it out if possible. So that function doesn't affect testing") - kept as its own entry, since my own request was to comment it out rather than delete it, in case it's needed again later.
            ('dashed',           'Marching ants (dashed)'),
            ('dots',             'Dots only'),
            ('honeycomb',        'Honeycomb (hexagons)'),
            ('honeycomb_dashed', 'Marching ants honeycomb'),
            ('none',             'Hide grid'),
        ]
        for key, label in grid_type_items:
            grid_type_combo.addItem(label, key)
        current_grid_type = self.map_settings.get('grid_type')
        if current_grid_type == 'squares':
            # Force back to the default while Squares is disabled
            # above, rather than silently falling through to index 0
            # and leaving the real saved setting still pointing at a
            # style that's no longer selectable at all.
            current_grid_type = 'lines'
        idx = next((i for i, (k, _) in enumerate(grid_type_items) if k == current_grid_type), 0)
        grid_type_combo.setCurrentIndex(idx)
        grid_type_combo.setToolTip(
            "The whole viewport's own reference grid style, used\n"
            "wherever the grid is shown at all - the interactive\n"
            "view and generated radar tiles alike. To switch the\n"
            "grid off completely, use the Show Grid quick toggle\n"
            "above instead - this only picks the style used\n"
            "whenever the grid is actually on.\n\n"
            "Not the same as the Preview tab's own Background Mode\n"
            "'Grid' choice - that's a flat, 2D fill pattern behind\n"
            "the scene, unrelated despite the shared name.")
        radar_form.addRow("Grid style:", grid_type_combo)

        grid_show_lines_chk = QCheckBox("Show grid outline lines")
        grid_show_lines_chk.setChecked(bool(self.map_settings.get('grid_show_lines', True)))
        grid_show_lines_chk.setToolTip(
            "Separate from the grid style itself - turn off to show\n"
            "just the Squares fill/texture on its own, with no line\n"
            "outlines drawn on top of it. To hide the grid entirely\n"
            "instead, use the Show Grid quick toggle above.")
        radar_form.addRow(grid_show_lines_chk)

        grid_bg_color_btn = QPushButton()
        grid_bg_color = tuple(self.map_settings.get('grid_bg_color'))
        grid_bg_color_btn.setStyleSheet(f"background-color: rgb{grid_bg_color};")
        def _pick_grid_bg_color(): #vers 1
            c = QColorDialog.getColor(QColor(*grid_bg_color_btn.property('rgb') or grid_bg_color), self)
            if c.isValid():
                grid_bg_color_btn.setProperty('rgb', (c.red(), c.green(), c.blue()))
                grid_bg_color_btn.setStyleSheet(f"background-color: rgb({c.red()},{c.green()},{c.blue()});")
        grid_bg_color_btn.setProperty('rgb', grid_bg_color)
        grid_bg_color_btn.clicked.connect(_pick_grid_bg_color)
        radar_form.addRow("Grid background color:", grid_bg_color_btn)

        grid_line_color_btn = QPushButton()
        grid_line_color = tuple(self.map_settings.get('grid_line_color'))
        grid_line_color_btn.setStyleSheet(f"background-color: rgb{grid_line_color};")
        def _pick_grid_line_color(): #vers 1
            c = QColorDialog.getColor(QColor(*grid_line_color_btn.property('rgb') or grid_line_color), self)
            if c.isValid():
                grid_line_color_btn.setProperty('rgb', (c.red(), c.green(), c.blue()))
                grid_line_color_btn.setStyleSheet(f"background-color: rgb({c.red()},{c.green()},{c.blue()});")
        grid_line_color_btn.setProperty('rgb', grid_line_color)
        grid_line_color_btn.clicked.connect(_pick_grid_line_color)
        radar_form.addRow("Grid line/dot color:", grid_line_color_btn)

        grid_line_size_spin = QSpinBox()
        grid_line_size_spin.setRange(4, 10)
        grid_line_size_spin.setValue(int(self.map_settings.get('grid_line_size')))
        radar_form.addRow("Grid line/dot size (px):", grid_line_size_spin)

        grid_spacing_spin = QSpinBox()
        grid_spacing_spin.setRange(1, 50)
        grid_spacing_spin.setValue(int(self.map_settings.get('grid_spacing') or 5))
        grid_spacing_spin.setToolTip("Used by 'Locked' mode only - divides zoom distance to get cell size.")
        radar_form.addRow("Grid spacing (Locked mode):", grid_spacing_spin)

        grid_fixed_step_spin = QSpinBox()
        grid_fixed_step_spin.setRange(1, 5000)
        grid_fixed_step_spin.setValue(int(self.map_settings.get('grid_fixed_step') or 200))
        grid_fixed_step_spin.setToolTip(
            "Used by 'Resize with models' mode only - real world units\n"
            "per grid cell, a genuine map-scale value (unlike Grid\n"
            "spacing above, which is only a divisor for Locked mode).")
        radar_form.addRow("Grid cell size (Resize-with-models mode):", grid_fixed_step_spin)

        grid_cell_count_combo = QComboBox()
        _cell_count_options = [12, 24, 36, 48, 60, 72, 84, 96, 108, 120]
        for n in _cell_count_options:
            grid_cell_count_combo.addItem(str(n), n)
        saved_cell_count = int(self.map_settings.get('grid_cell_count') or 24)
        _cell_count_idx = _cell_count_options.index(saved_cell_count) if saved_cell_count in _cell_count_options else 1
        grid_cell_count_combo.setCurrentIndex(_cell_count_idx)
        grid_cell_count_combo.setToolTip(
            "Total grid diameter in cells, across every grid style\n"
            "(not just honeycomb) - how far the grid extends before\n"
            "it stops, independent of cell size.")
        radar_form.addRow("Grid cell count:", grid_cell_count_combo)

        grid_scale_mode_combo = QComboBox()
        grid_scale_mode_combo.addItem("Locked (constant on-screen size)", "zoom_relative")
        grid_scale_mode_combo.addItem("Resize with models (fixed world size)", "fixed")
        grid_scale_mode_combo.addItem("Radar tiles (matches game's radar grid)", "radar_tiles")
        saved_scale_mode = self.map_settings.get('grid_scale_mode') or 'zoom_relative'
        _scale_mode_idx = {'zoom_relative': 0, 'fixed': 1, 'radar_tiles': 2}.get(saved_scale_mode, 0)
        grid_scale_mode_combo.setCurrentIndex(_scale_mode_idx)
        grid_scale_mode_combo.setToolTip(
            "Locked: the grid always looks the same size on screen,\n"
            "its world-unit cell size shrinks/grows as you zoom.\n"
            "Resize with models: the grid's cell size is fixed in\n"
            "world units, so it scales with zoom the same way models do.\n"
            "Radar tiles: grid cells match the currently loaded game's\n"
            "own real radar tile grid (12x12 SA, 8x8 VC/III, 36x36 SOL)\n"
            "and stay anchored to the map's real origin, not the camera -\n"
            "so tile boundaries line up with actually exported radar tiles.")
        radar_form.addRow("Grid zoom behaviour:", grid_scale_mode_combo)

        grid_fill_mode_combo = QComboBox()
        grid_fill_mode_combo.addItem("Colour", "color")
        grid_fill_mode_combo.addItem("Image/Texture (tiled)", "texture")
        saved_fill_mode = self.map_settings.get('squares_fill_mode') or 'color'
        grid_fill_mode_combo.setCurrentIndex(1 if saved_fill_mode == 'texture' else 0)

        grid_texture_path_edit = QLineEdit(self.map_settings.get('squares_texture_path') or '')
        grid_texture_path_edit.setReadOnly(True)
        grid_texture_browse_btn = QPushButton("Browse…")
        def _browse_grid_texture(): #vers 1
            path, _ = QFileDialog.getOpenFileName(
                self, "Choose Grid Texture", "", "Images (*.png *.jpg *.jpeg *.bmp *.tga)")
            if path:
                grid_texture_path_edit.setText(path)
        grid_texture_browse_btn.clicked.connect(_browse_grid_texture)
        grid_texture_row = QHBoxLayout()
        grid_texture_row.addWidget(grid_fill_mode_combo)
        grid_texture_row.addWidget(grid_texture_path_edit)
        grid_texture_row.addWidget(grid_texture_browse_btn)
        radar_form.addRow("Use image/Texture:", grid_texture_row)

        grid_tile_size_spin = QSpinBox()
        grid_tile_size_spin.setRange(64, 1024)
        grid_tile_size_spin.setSingleStep(64)
        grid_tile_size_spin.setValue(int(self.map_settings.get('squares_tile_size') or 256))
        radar_form.addRow("Texture tile size:", grid_tile_size_spin)

        grid_texture_alpha_spin = QDoubleSpinBox()
        grid_texture_alpha_spin.setRange(0.0, 1.0)
        grid_texture_alpha_spin.setSingleStep(0.05)
        grid_texture_alpha_spin.setValue(float(self.map_settings.get('squares_texture_alpha') or 0.5))
        grid_texture_alpha_spin.setToolTip("0.0 = fully transparent, 1.0 = fully opaque")
        radar_form.addRow("Texture opacity:", grid_texture_alpha_spin)

        radar_show_grid_chk = QCheckBox("Show reference grid in generated tiles")
        radar_show_grid_chk.setChecked(bool(self.map_settings.get('radar_tiles_show_grid')))
        radar_show_grid_chk.setToolTip(
            "Include the viewport's reference grid lines in\n"
            "generated radar/minimap tiles. Off by default.")
        radar_form.addRow(radar_show_grid_chk)

        # radar_tex_layer_chk removed (Aug 20 2026,  "moving
        # the Radar settings from the settings, to the button would
        # follow the pattern of the other buttons, leaving the
        # settings, for those buttons in the map_workshop settings") -
        # the simple on/off toggle this checkbox used to be now lives
        # on the [Radar] button itself (_on_show_radar_tex_layer_
        # toggled), matching every other overlay's own pattern.

        grid_hide_over_radar_chk = QCheckBox("Hide grid over radar tiles")
        grid_hide_over_radar_chk.setChecked(bool(self.map_settings.get('grid_hide_over_radar_tiles')))
        grid_hide_over_radar_chk.setToolTip(
            "Only takes effect while the radar tex layer above is on -\n"
            "hides grid lines specifically within the map's own real\n"
            "area, while still showing them outside it (e.g. as scale\n"
            "reference in the void beyond the map edge).")
        radar_form.addRow(grid_hide_over_radar_chk)

        grid_reset_btn = QPushButton("Reset Grid to Defaults")
        grid_reset_btn.setToolTip(
            "Resets every grid control on this tab back to its\n"
            "original default - easy to enter a large cell size or\n"
            "count and end up with something unusable.")
        def _reset_grid_defaults(): #vers 1
            grid_type_combo.setCurrentIndex(0)   # 'lines'
            grid_bg_color_btn.setProperty('rgb', (51, 128, 230))
            grid_bg_color_btn.setStyleSheet("background-color: rgb(51,128,230);")
            grid_line_color_btn.setProperty('rgb', (76, 76, 102))
            grid_line_color_btn.setStyleSheet("background-color: rgb(76,76,102);")
            grid_line_size_spin.setValue(4)
            grid_spacing_spin.setValue(5)
            grid_fixed_step_spin.setValue(200)
            grid_cell_count_combo.setCurrentIndex(1)   # 24
            grid_scale_mode_combo.setCurrentIndex(0)   # 'zoom_relative'
            grid_fill_mode_combo.setCurrentIndex(0)    # 'color'
            grid_texture_path_edit.setText('')
            grid_tile_size_spin.setValue(256)
            grid_texture_alpha_spin.setValue(0.5)
            grid_show_lines_chk.setChecked(True)
            grid_hide_over_radar_chk.setChecked(False)
        grid_reset_btn.clicked.connect(_reset_grid_defaults)
        radar_form.addRow(grid_reset_btn)

        env_grp = QGroupBox("Environment")
        env_form = QFormLayout(env_grp)

        skybox_path_edit = QLineEdit(self.map_settings.get('skybox_path') or '')
        skybox_path_edit.setReadOnly(True)
        skybox_browse_btn = QPushButton("Browse…")
        def _browse_skybox(): #vers 1
            path, _ = QFileDialog.getOpenFileName(
                self, "Choose Skybox/Skydome Image", "", "Images (*.png *.jpg *.jpeg *.bmp *.tga)")
            if path:
                skybox_path_edit.setText(path)
        skybox_browse_btn.clicked.connect(_browse_skybox)
        skybox_row = QHBoxLayout()
        skybox_row.addWidget(skybox_path_edit)
        skybox_row.addWidget(skybox_browse_btn)
        env_form.addRow("Skybox / Skydome:", skybox_row)

        timecyc_path_edit = QLineEdit(self.map_settings.get('timecyc_path') or '')
        timecyc_path_edit.setReadOnly(True)
        timecyc_path_edit.setPlaceholderText("Auto-detected next to the loaded game's own data folder, if found")
        timecyc_browse_btn = QPushButton("Browse…")
        def _browse_timecyc(): #vers 2
            path, _ = QFileDialog.getOpenFileName(
                self, "Choose Timecyc File", "", "Timecyc (*.dat)")
            if path:
                timecyc_path_edit.setText(path)
                vp = getattr(self, 'preview_widget', None)
                if vp is not None and hasattr(vp, 'set_timecyc_path'):
                    known_game = getattr(getattr(self, '_world_loader', None), 'game', None)
                    vp.set_timecyc_path(path, known_game=known_game)
        timecyc_browse_btn.clicked.connect(_browse_timecyc)
        timecyc_row = QHBoxLayout()
        timecyc_row.addWidget(timecyc_path_edit)
        timecyc_row.addWidget(timecyc_browse_btn)
        env_form.addRow("Use timecyc file:", timecyc_row)

        sky_flip_chk = QCheckBox("Flip sky gradient (zenith/horizon)")
        sky_flip_chk.setChecked(bool(self.map_settings.get('sky_gradient_flipped')))
        sky_flip_chk.setToolTip(
            "In case the sky's own real vertical orientation still\n"
            "looks backwards - swaps which colour sits at the zenith\n"
            "versus near the horizon.")
        env_form.addRow(sky_flip_chk)

        water_grp = QGroupBox("Water Display")
        water_form = QFormLayout(water_grp)

        water_path_edit = QLineEdit()
        loader_for_water = getattr(self, '_world_loader', None)
        water_loaded = bool(getattr(loader_for_water, 'water_shapes', None) or getattr(loader_for_water, 'waterpro', None))
        water_path_display = "Loaded (water.dat/waterpro.dat)" if water_loaded else ''
        water_path_edit.setText(water_path_display)
        water_path_edit.setReadOnly(True)
        water_path_edit.setPlaceholderText("Not found - Browse to point at it directly")
        water_browse_btn = QPushButton("Browse…")
        water_browse_btn.setToolTip(
            "Per  : \"the waterpro.dat is in gameroot/data/waterpro.\n"
            "dat; if it's not found, ask for it, [browse] with the path\n"
            "to where the waterpro.dat is.\" Manually point at a real\n"
            "water.dat or waterpro.dat if auto-detection (the game's\n"
            "own WATER directive, then gameroot/data/, then the active\n"
            "project's own game root) didn't find one.")
        def _browse_water_file(): #vers 1
            path, _ = QFileDialog.getOpenFileName(
                self, "Choose water.dat or waterpro.dat", "", "Water data (*.dat)")
            if not path:
                return
            loader = getattr(self, '_world_loader', None)
            if loader is None:
                QMessageBox.warning(self, "No World Loaded",
                    "Load a world first, then Browse for its own water.dat/waterpro.dat.")
                return
            from apps.methods.gta_dat_parser import parse_water_dat, parse_waterpro_dat
            result = parse_waterpro_dat(path)
            if result is not None:
                loader.waterpro = result
            else:
                shapes = parse_water_dat(path)
                if shapes:
                    loader.water_shapes = shapes
                else:
                    QMessageBox.warning(self, "Water File",
                        f"Couldn't parse {os.path.basename(path)} as either a real water.dat or waterpro.dat.")
                    return
            water_path_edit.setText(f"Loaded ({os.path.basename(path)}, manually chosen)")
            self._refresh_water_visualization()
        water_browse_btn.clicked.connect(_browse_water_file)
        water_path_row = QHBoxLayout()
        water_path_row.addWidget(water_path_edit)
        water_path_row.addWidget(water_browse_btn)
        water_form.addRow("Water file:", water_path_row)

        water_style_combo = QComboBox()
        water_style_items = [
            ('fill',     'Flat fill (default)'),
            ('lines',    'Outline lines'),
            ('dots',     'Dots'),
            ('hexagons', 'Hexagons'),
            ('texture',  'Custom texture (tiled)'),
        ]
        for key, label in water_style_items:
            water_style_combo.addItem(label, key)
        saved_water_style = self.map_settings.get('water_display_style') or 'fill'
        idx = next((i for i, (k, _) in enumerate(water_style_items) if k == saved_water_style), 0)
        water_style_combo.setCurrentIndex(idx)
        water_form.addRow("Water style:", water_style_combo)

        water_texture_path_edit = QLineEdit(self.map_settings.get('water_texture_path') or '')
        water_texture_path_edit.setReadOnly(True)
        water_texture_browse_btn = QPushButton("Browse…")
        def _browse_water_texture(): #vers 1
            path, _ = QFileDialog.getOpenFileName(
                self, "Choose Water Texture", "", "Images (*.png *.jpg *.jpeg *.bmp *.tga)")
            if path:
                water_texture_path_edit.setText(path)
        water_texture_browse_btn.clicked.connect(_browse_water_texture)
        water_texture_row = QHBoxLayout()
        water_texture_row.addWidget(water_texture_path_edit)
        water_texture_row.addWidget(water_texture_browse_btn)
        water_form.addRow("Custom texture:", water_texture_row)

        water_tile_size_spin = QSpinBox()
        water_tile_size_spin.setRange(64, 1024)
        water_tile_size_spin.setSingleStep(64)
        water_tile_size_spin.setValue(int(self.map_settings.get('water_tile_size') or 256))
        water_form.addRow("Texture tile size:", water_tile_size_spin)

        water_hide_outside_chk = QCheckBox("Hide water outside map boundary")
        water_hide_outside_chk.setChecked(bool(self.map_settings.get('water_hide_outside_map')))
        water_hide_outside_chk.setToolTip(
            "Skips water cells outside the loaded map's own area,\n"
            "instead of showing them stretched across the void.")
        water_form.addRow(water_hide_outside_chk)

        render_layout.addWidget(boxes_grp)
        render_layout.addWidget(radar_grp)
        # water_grp (the old, settings-driven group) disconnected (Aug
        # 20 2026, re-applied per my own "get water working"
        # priority) - widget still built above (harmless, just not
        # shown) so this can be reused or fully removed later once
        # the new preload-driven water is confirmed solid.
        # render_layout.addWidget(water_grp)

        water2_grp = QGroupBox("Water")
        water2_form = QFormLayout(water2_grp)
        water2_use_texture_chk = QCheckBox("Use water texture (if preloaded)")
        water2_use_texture_chk.setChecked(bool(self.map_settings.get('water2_use_texture')))
        water2_use_texture_chk.setToolTip(
            "Off: plain flat colour fill.\n"
            "On: uses the game's water texture if preloaded, "
            "falling back to a custom file or tex/ asset.")
        water2_form.addRow(water2_use_texture_chk)

        water2_tex_path_edit = QLineEdit(self.map_settings.get('water2_custom_texture_path') or '')
        water2_tex_path_edit.setReadOnly(True)
        water2_tex_path_edit.setPlaceholderText("Auto: the loaded game's own particle.txd water texture")
        water2_tex_browse_btn = QPushButton("Browse...")
        def _browse_water2_texture(): #vers 2
            """ fix (Aug 20 2026,  "the option settings
            path for using other water textures") - a manually-chosen
            custom texture is a deliberate, explicit choice, so it
            takes priority over the automatic particle.txd extraction
            - clears the in-game RGBA source (set_water2_texture_rgba
            (None,...)) so _ensure_water2_texture's own precedence
            rule correctly falls through to this file path instead.
            Uses set_water2_texture_path (updates just the texture)
            rather than set_water2_data with an empty cells list,
            which would have wiped out the already-loaded water grid
            instead of leaving it alone."""
            path, _ = QFileDialog.getOpenFileName(
                self, "Choose Water Texture", "", "Images (*.png *.jpg *.jpeg *.bmp *.tga)")
            if path:
                water2_tex_path_edit.setText(path)
                self.map_settings.set('water2_custom_texture_path', path)
                vp = getattr(self, 'preview_widget', None)
                if vp is not None:
                    if hasattr(vp, 'set_water2_texture_rgba'):
                        vp.set_water2_texture_rgba(None, 0, 0)
                    if hasattr(vp, 'set_water2_texture_path'):
                        vp.set_water2_texture_path(path)
        water2_tex_browse_btn.clicked.connect(_browse_water2_texture)
        water2_tex_row = QHBoxLayout()
        water2_tex_row.addWidget(water2_tex_path_edit)
        water2_tex_row.addWidget(water2_tex_browse_btn)
        water2_form.addRow("Custom texture:", water2_tex_row)

        water2_height_spin = QDoubleSpinBox()
        water2_height_spin.setRange(-500.0, 500.0)
        water2_height_spin.setSingleStep(1.0)
        water2_height_spin.setDecimals(1)
        water2_height_spin.setValue(float(self.map_settings.get('water2_height_offset')))
        water2_height_spin.setToolTip(
            "Added to every water cell's height. Turn on [Radar] "
            "to compare against the radar layer while adjusting.")
        water2_form.addRow("Height offset:", water2_height_spin)

        water2_alpha_spin = QDoubleSpinBox()
        water2_alpha_spin.setRange(0.0, 1.0)
        water2_alpha_spin.setSingleStep(0.05)
        water2_alpha_spin.setDecimals(2)
        water2_alpha_spin.setValue(float(self.map_settings.get('water2_alpha')))
        water2_alpha_spin.setToolTip("Same real request as Height offset above - water's own transparency.")
        water2_form.addRow("Transparency:", water2_alpha_spin)

        water2_x_spin = QDoubleSpinBox()
        water2_x_spin.setRange(-4000.0, 4000.0)
        water2_x_spin.setSingleStep(10.0)
        water2_x_spin.setDecimals(1)
        water2_x_spin.setValue(float(self.map_settings.get('water2_x_offset')))
        water2_x_spin.setToolTip(
            "X offset applied to every water cell. Matches water_"
            "workshop.py's own \"World coordinate offset\".")
        water2_form.addRow("X offset:", water2_x_spin)

        water2_y_spin = QDoubleSpinBox()
        water2_y_spin.setRange(-4000.0, 4000.0)
        water2_y_spin.setSingleStep(10.0)
        water2_y_spin.setDecimals(1)
        water2_y_spin.setValue(float(self.map_settings.get('water2_y_offset')))
        water2_y_spin.setToolTip("Same real request as X offset above.")
        water2_form.addRow("Y offset:", water2_y_spin)

        water2_mode_combo = QComboBox()
        for label, key in (("VC only", 'vc'), ("All games", 'all'), ("Off", 'off')):
            water2_mode_combo.addItem(label, key)
        water2_mode_combo.setCurrentIndex(
            max(0, water2_mode_combo.findData(self._water2_offset_mode())))
        water2_mode_combo.setToolTip(
            "Radar/water fix - when the X/Y offset applies.\n"
            "VC only: PC Vice City (-400 X).  Off: LCS ports, LC, SA.")
        water2_form.addRow("Radar/water fix:", water2_mode_combo)

        #  fix (Aug 20 2026,  "Change it by 20+ on the
        # height in settings, doesn't update the view") - these two
        # only ever applied on Apply/OK before; live updates as the
        # spinbox itself changes make far more sense for a "not sure
        # by how much, need to see it interactively" adjustment like
        # this one, rather than needing a separate Apply click after
        # every nudge.
        def _live_water2_height(value): #vers 1
            vp_live = getattr(self, 'preview_widget', None)
            if vp_live is not None and hasattr(vp_live, 'set_water2_height_offset'):
                vp_live.set_water2_height_offset(value)
        def _live_water2_alpha(value): #vers 1
            vp_live = getattr(self, 'preview_widget', None)
            if vp_live is not None and hasattr(vp_live, 'set_water2_alpha'):
                vp_live.set_water2_alpha(value)
        def _live_water2_x(value): #vers 1
            vp_live = getattr(self, 'preview_widget', None)
            if vp_live is not None and hasattr(vp_live, 'set_water2_x_offset'):
                vp_live.set_water2_x_offset(value)
        def _live_water2_y(value): #vers 1
            vp_live = getattr(self, 'preview_widget', None)
            if vp_live is not None and hasattr(vp_live, 'set_water2_y_offset'):
                vp_live.set_water2_y_offset(value)
        water2_height_spin.valueChanged.connect(_live_water2_height)
        water2_alpha_spin.valueChanged.connect(_live_water2_alpha)
        water2_x_spin.valueChanged.connect(_live_water2_x)
        water2_y_spin.valueChanged.connect(_live_water2_y)
        def _live_water2_mode(_idx): #vers 2
            vp_live = getattr(self, 'preview_widget', None)
            if vp_live is not None and hasattr(vp_live, 'set_water2_offset_mode'):
                vp_live.set_water2_offset_mode(water2_mode_combo.currentData())
        water2_mode_combo.currentIndexChanged.connect(_live_water2_mode)

        render_layout.addWidget(water2_grp)

        # Audio Streams (Aug 20 2026,  "i can send you the
        # sounds, would that help") - real, working extraction from
        # SA's own real "audio stream" file format (AMBIENCE, GENRL,
        # radio station files) confirmed against my own real,
        # uploaded AMBIENCE file (documented XOR key + track header
        # from GTAMods, verified via ffprobe: extracted tracks are
        # fully valid Ogg Vorbis, probe_score=100).
        audio_grp = QGroupBox("Audio Streams")
        audio_form = QFormLayout(audio_grp)
        audio_extract_btn = QPushButton("Extract Tracks...")
        audio_extract_btn.setToolTip(
            "Extract every track from an SA audio stream file (e.g.\n"
            "AMBIENCE, GENRL) to numbered .ogg files in depends/\n"
            "auzo_sounds/. Rename matches by zone name or sound_id.")
        def _extract_audio_stream(): #vers 1
            path, _ = QFileDialog.getOpenFileName(
                self, "Choose SA Audio Stream File (e.g. AMBIENCE, GENRL)", "", "All Files (*)")
            if not path:
                return
            try:
                from apps.methods.audioparser import extract_all_tracks
            except ImportError as e:
                self._set_status(f"Couldn't load the audio stream decoder: {e}")
                return
            out_dir = self._app_asset_folder('auzo_sounds')
            self._set_status(f"Extracting tracks from {os.path.basename(path)}...")
            QApplication.processEvents()
            try:
                written = extract_all_tracks(path, out_dir)
            except Exception as e:
                self._set_status(f"Failed to extract audio stream tracks: {e}")
                return
            self._set_status(
                f"Extracted {len(written)} track(s) from {os.path.basename(path)} "
                f"to {out_dir} - listen and rename to match Auzo zones.")
        audio_extract_btn.clicked.connect(_extract_audio_stream)
        audio_form.addRow(audio_extract_btn)
        render_layout.addWidget(audio_grp)

        render_layout.addWidget(env_grp)
        render_layout.addStretch()
        tabs.addTab(render_tab_outer, "Render")

        # - World tab (Oct 12 2026): grid/radar, water, environment, audio
        world_scroll = QScrollArea()
        world_scroll.setWidgetResizable(True)
        world_tab = QWidget()
        world_layout = QVBoxLayout(world_tab)
        world_layout.setSpacing(6)
        for _grp in (radar_grp, water2_grp, env_grp, audio_grp):
            world_layout.addWidget(_grp)
        world_layout.addStretch()
        world_scroll.setWidget(world_tab)
        tabs.addTab(world_scroll, "World")

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
        cache_hint.setStyleSheet("color: palette(placeholderText); font-style: italic;")
        cache_layout.addWidget(cache_hint)

        cache_group.setLayout(cache_layout)
        perf_layout.addWidget(cache_group)

        perf_layout.addStretch()

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
        bg_mode_combo.setToolTip(
            "The viewport's own background FILL, behind everything\n"
            "else - not the 3D reference grid overlay (Lines/Squares/\n"
            "Dashed/Dots, on the Render tab's own Grid & Radar Tiles\n"
            "group) - this option's own 'Grid' choice is a flat, 2D\n"
            "checker-style pattern behind the scene, a different,\n"
            "unrelated feature despite the shared name.")
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
        #cb_slider.valueChanged.connect(cb_spin.setValue)
        #cb_spin.valueChanged.connect(cb_slider.setValue)

        # Hint
        cb_hint = QLabel("Smaller = tighter pattern, larger = bigger squares")
        cb_hint.setStyleSheet("color: palette(placeholderText); font-style: italic; font-size: 10px;")
        bg_layout.addWidget(cb_hint)

        bg_group.setLayout(bg_layout)
        preview_layout.addWidget(bg_group)

        # Overlay Settings
        overlay_group = QGroupBox("Overlay View Settings")
        overlay_layout = QVBoxLayout()

        overlay_label = QLabel("Overlay Opacity (Wireframe over mesh):")
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
        opacity_hint.setStyleSheet("color: palette(placeholderText); font-style: italic; font-size: 10px;")
        overlay_layout.addWidget(opacity_hint)

        overlay_group.setLayout(overlay_layout)
        preview_layout.addWidget(overlay_group)

        preview_layout.addStretch()
        for _grp in (zoom_group, bg_group, overlay_group):   # Preview merged into Display
            display_layout.insertWidget(display_layout.count() - 1, _grp)

        # - Loading tab (Aug 1 2026)
        loading_tab = QWidget()
        ld_lay = QVBoxLayout(loading_tab)
        ld_form = QFormLayout()
        ld_form.setSpacing(8)
        ld_grid = QGridLayout()          # short checkboxes two per row
        ld_grid.setHorizontalSpacing(24)

        load_streams_chk = QCheckBox("Load Text plus Binary IPL set")
        load_streams_chk.setChecked(self.map_settings.get('load_text_plus_binary_ipl_set'))
        load_streams_chk.setToolTip(
            "When loading a text IPL, also automatically load all of\n"
            "its known associated binary stream files - many text IPLs\n"
            "are mostly LOD content, with the normal-detail models\n"
            "living in their streams instead.")

        preload_img_chk = QCheckBox("Preload IMG archives on DAT load")
        preload_img_chk.setChecked(self.map_settings.get('preload_img_on_dat_load'))
        preload_img_chk.setToolTip(
            "When loading a game's main .dat file (gta3.dat/gta_vc.dat/\n"
            "gta.dat), sequentially read through every IMG archive it\n"
            "references once, letting the OS cache them in RAM ahead\n"
            "of time. Trades a longer upfront wait for faster per-IPL\n"
            "model/texture loading afterward - off by default since\n"
            "it's not always a net win.")

        show_load_options_chk = QCheckBox("Show Load Options dialog on world load")
        show_load_options_chk.setChecked(self.map_settings.get('show_load_options_dialog', True))
        show_load_options_chk.setToolTip(
            "The popup shown right after a world's IPL list is\n"
            "discovered, letting you pick specific IPLs to load right\n"
            "away (with model/texture options) instead of staying\n"
            "fully lazy. Turn off to skip it and always stay lazy -\n"
            "load individual IPLs later via their eye icon in IPL\n"
            "Sections, the same as picking \"Load from .dat file\" in\n"
            "that dialog would.")

        auto_load_last_world_chk = QCheckBox("Auto-load last world on startup")
        auto_load_last_world_chk.setChecked(self.map_settings.get('auto_load_last_world', True))
        auto_load_last_world_chk.setToolTip(
            "Automatically reloads the most recently used game .dat\n"
            "file when Map Workshop opens.")

        auto_dismiss_summary_chk = QCheckBox("Auto-dismiss the load summary dialog")
        auto_dismiss_summary_chk.setChecked(self.map_settings.get('auto_dismiss_summary_dialog', False))
        auto_dismiss_summary_row = QHBoxLayout()
        auto_dismiss_seconds_spin = QSpinBox()
        auto_dismiss_seconds_spin.setRange(1, 60)
        auto_dismiss_seconds_spin.setValue(int(self.map_settings.get('auto_dismiss_summary_seconds', 10)))
        auto_dismiss_seconds_spin.setSuffix(" s")
        auto_dismiss_summary_chk.setToolTip(
            "Automatically clicks OK on the load summary dialog\n"
            "after the countdown below.")
        auto_dismiss_summary_row.addWidget(auto_dismiss_summary_chk)
        auto_dismiss_summary_row.addWidget(QLabel("after:"))
        auto_dismiss_summary_row.addWidget(auto_dismiss_seconds_spin)
        ld_form.addRow(auto_dismiss_summary_row)

        verbose_loading_chk = QCheckBox("Show Full Loading Models (Debug)")
        verbose_loading_chk.setChecked(self.map_settings.get('show_verbose_loading_dialog'))
        for _i, _chk in enumerate((load_streams_chk, preload_img_chk, show_load_options_chk,
                                   auto_load_last_world_chk)):
            ld_grid.addWidget(_chk, _i // 2, _i % 2)
        ld_lay.addLayout(ld_grid)
        verbose_loading_chk.setToolTip(
            "Show a scrolling debug dialog listing every model as it\n"
            "loads, one line per instance.")
        ld_form.addRow(verbose_loading_chk)
        import os as _os
        _cores = _os.cpu_count() or 1
        workers_spin = QSpinBox()
        workers_spin.setRange(0, _cores)
        workers_spin.setSpecialValueText(f"Auto ({max(1, _cores - 2)})")
        workers_spin.setValue(int(self.map_settings.get('load_workers') or 0))
        workers_spin.setToolTip(
            f"Worker processes that parse DFF/TXD files in parallel while an IPL loads.\n"
            f"This system has {_cores} CPU cores. 1 = one at a time, Auto = cores - 2.")
        ld_form.addRow(f"Parallel load workers ({_cores} cores):", workers_spin)

        ld_lay.addLayout(ld_form)
        ld_lay.addStretch()
        for _grp in (perf_group, cache_group):   # Performance merged into Loading
            ld_lay.insertWidget(ld_lay.count() - 1, _grp)
        tabs.addTab(loading_tab, "Loading")

        # - Textures tab (Oct 12 2026): per-format limits on load
        textures_tab = QWidget()
        tx_lay = QVBoxLayout(textures_tab)
        tex_grp = QGroupBox("Texture size limit - reload the world after changing")
        tx_grid = QGridLayout(tex_grp)
        for _c, _h in enumerate(("Format", "Reduce", "Anything over", "Down to")):
            tx_grid.addWidget(QLabel(f"<b>{_h}</b>"), 0, _c)

        def _limit_spin(value, top): #vers 1
            sp = QSpinBox()
            sp.setRange(16, top)
            sp.setSingleStep(16)
            sp.setValue(int(value))
            return sp

        downscale_chk = QCheckBox()
        downscale_chk.setChecked(self.map_settings.get('texture_downscale_enabled'))
        downscale_threshold_spin = _limit_spin(self.map_settings.get('texture_downscale_threshold'), 4096)
        downscale_target_spin = _limit_spin(self.map_settings.get('texture_downscale_target'), 2048)
        tex_rows = {'rw': (downscale_chk, downscale_threshold_spin, downscale_target_spin)}
        for g in ('iv', 'stories', 'nif'):
            chk = QCheckBox()
            chk.setChecked(bool(self.map_settings.get(f'tex_limit_{g}_on')))
            tex_rows[g] = (chk, _limit_spin(self.map_settings.get(f'tex_limit_{g}_over'), 4096),
                           _limit_spin(self.map_settings.get(f'tex_limit_{g}_to'), 2048))
        for r, (g, title, tip) in enumerate((
                ('rw', "RenderWare", "TXD and DFF: PC, Xbox, PS2, PSP, mobile"),
                ('iv', "GTA IV", "WTD, and textures inside WDR / WFT"),
                ('stories', "LCS / VCS", "CHK / XTX textures for MDL models"),
                ('nif', "Bully (Gamebryo)", "NFT texture packs for NIF models")), 1):
            lbl = QLabel(title)
            lbl.setToolTip(tip)
            tx_grid.addWidget(lbl, r, 0)
            for c, w in enumerate(tex_rows[g], 1):
                w.setToolTip(tip)
                tx_grid.addWidget(w, r, c)
        tx_grid.setColumnStretch(4, 1)
        tx_lay.addWidget(tex_grp)
        keep_dxt_chk = QCheckBox("Keep DXT textures compressed (graphics card decodes, 4-8x less memory)")
        keep_dxt_chk.setChecked(bool(self.map_settings.get('tex_keep_dxt')))
        keep_dxt_chk.setToolTip("RW PC and GTA IV DXT1/3/5 textures. Reload the world after changing.")
        tx_lay.addWidget(keep_dxt_chk)
        load_generic_btn = QPushButton("Load Generic.txd Manually")
        load_generic_btn.setToolTip(
            "Manually (re)load generic.txd - the shared texture archive\n"
            "many models reference without having their own dedicated\n"
            "TXD. Not normally needed - generic.ide's referenced TXDs\n"
            "(including generic.txd itself) are already preloaded\n"
            "automatically whenever a world loads. Tries the game's\n"
            "indexed IMG archives first (e.g. gta3.img, which every game\n"
            "always loads via the exe), then falls back to {game root}/\n"
            "models/generic.txd as a loose file.")
        load_generic_btn.clicked.connect(self._on_load_generic_txd_clicked)
        tx_lay.addWidget(load_generic_btn)
        tx_lay.addStretch()

        lod_test_grp = QGroupBox("LOD Test")
        lod_test_form = QFormLayout(lod_test_grp)
        lod_test_radius_spin = QSpinBox()
        lod_test_radius_spin.setRange(1, 10000)
        lod_test_radius_spin.setSingleStep(50)
        lod_test_radius_spin.setValue(int(self.map_settings.get('lod_test_circle_radius')))
        lod_test_radius_spin.setToolTip(
            "Radius of the circle drawn around the mouse cursor in LOD\n"
            "Test mode - models inside switch to one detail level,\n"
            "models outside switch to the other (see the LOD Test\n"
            "checkbox in IPL Controls).")
        lod_test_form.addRow("Circle radius:", lod_test_radius_spin)

        # - Save Points tab (Sep 23 2026)
        sp_tab = QWidget()
        sp_lay = QVBoxLayout(sp_tab)
        sp_form = QFormLayout()
        sp_enabled_chk = QCheckBox("Automatic save points")
        sp_enabled_chk.setChecked(bool(self.map_settings.get('savepoints_enabled')))
        sp_enabled_chk.setToolTip("Snapshot unsaved map edits on a timer.\n"
                                  "Game files are not touched; restore from the IPL list menu.")
        sp_form.addRow(sp_enabled_chk)
        sp_interval_spin = QSpinBox()
        sp_interval_spin.setRange(1, 240)
        sp_interval_spin.setSuffix(" min")
        sp_interval_spin.setValue(int(self.map_settings.get('savepoints_interval_min') or 10))
        sp_form.addRow("Every:", sp_interval_spin)
        sp_keep_spin = QSpinBox()
        sp_keep_spin.setRange(1, 500)
        sp_keep_spin.setValue(int(self.map_settings.get('savepoints_keep') or 20))
        sp_keep_spin.setToolTip("Oldest automatic save points beyond this are deleted")
        sp_form.addRow("Keep automatic:", sp_keep_spin)
        sp_lay.addLayout(sp_form)
        sp_btn_row = QHBoxLayout()
        sp_now_btn = QPushButton("Create Save Point Now")
        sp_now_btn.clicked.connect(lambda: self._create_savepoint("Manual"))
        sp_restore_btn = QPushButton("Restore...")
        sp_restore_btn.clicked.connect(self._restore_savepoint_dialog)
        sp_btn_row.addWidget(sp_now_btn)
        sp_btn_row.addWidget(sp_restore_btn)
        sp_lay.addLayout(sp_btn_row)
        sp_lay.addStretch()

        # - Navigation tab (Aug 1 2026)
        nav_tab = QWidget()
        nav_lay = QVBoxLayout(nav_tab)
        nav_vp = getattr(self, 'preview_widget', None)
        nav_lay.addWidget(QLabel("Mouse Sensitivity"))
        nav_row = QHBoxLayout()
        nav_slider = QSlider(Qt.Orientation.Horizontal)
        nav_slider.setRange(10, 300)   # 0.1x to 3.0x, as integer tenths
        nav_slider.setValue(int(getattr(nav_vp, '_mouse_sensitivity', 1.0) * 100) if nav_vp else 100)
        nav_value_lbl = QLabel(f"{nav_slider.value() / 100:.1f}x")
        def _on_nav_sens_change(v):  #vers 1
            if nav_vp is not None:
                nav_vp._mouse_sensitivity = v / 100
            nav_value_lbl.setText(f"{v / 100:.1f}x")
        nav_slider.valueChanged.connect(_on_nav_sens_change)
        nav_row.addWidget(nav_slider)
        nav_row.addWidget(nav_value_lbl)
        nav_lay.addLayout(nav_row)

        nav_lay.addWidget(QLabel("Go-to-Object Zoom Distance"))
        nav_zoom_row = QHBoxLayout()
        nav_zoom_spin = QDoubleSpinBox()
        nav_zoom_spin.setRange(1.0, 500.0)
        nav_zoom_spin.setDecimals(1)
        nav_zoom_spin.setSingleStep(5.0)
        nav_zoom_spin.setValue(getattr(self, '_goto_zoom_distance', 40.0))
        nav_zoom_spin.setToolTip(
            "How far the camera sits from an object when double-clicking\n"
            "it in the viewport (or picking it another way) - lower is\n"
            "closer/more zoomed in.")
        def _on_nav_zoom_change(v): #vers 1
            self._goto_zoom_distance = v
        nav_zoom_spin.valueChanged.connect(_on_nav_zoom_change)
        nav_zoom_row.addWidget(nav_zoom_spin)
        nav_lay.addLayout(nav_zoom_row)

        # Zoom to cursor (Aug 18 2026)
        zoom_cursor_chk = QCheckBox("Zoom to mouse cursor")
        zoom_cursor_chk.setChecked(bool(self.map_settings.get('zoom_to_cursor')))
        zoom_cursor_chk.setToolTip(
            "When zooming with the mouse wheel, zoom toward wherever\n"
            "the cursor is instead of always toward the current pan\n"
            "centre - move the cursor to an area first, then zoom in\n"
            "to pull in on that specific spot.")
        nav_lay.addWidget(zoom_cursor_chk)

        # Auto-highlight on hover (Aug 19 2026)
        hover_highlight_chk = QCheckBox("Auto-highlight object under cursor")
        hover_highlight_chk.setChecked(bool(self.map_settings.get('auto_highlight_hover')))
        hover_highlight_chk.setToolTip(
            "Highlights whatever instance the mouse is currently over\n"
            "in the 3D view, even without clicking. Right-click a\n"
            "highlighted instance for a quick options menu (Info,\n"
            "Show Textures). Currently instances only, not path nodes\n"
            "or other viewport elements yet.")
        nav_lay.addWidget(hover_highlight_chk)

        # View from above when selecting a model (Sep 5 2026, per
        #  : "when selecting a model in map workshop, can we
        # change the camera view, have a setting, view from 0, +200
        # so we dont view the model from the bottom, we see it from
        # the top instead")
        focus_above_chk = QCheckBox("View from above when selecting a model")
        focus_above_chk.setChecked(bool(self.map_settings.get('focus_from_above')))
        focus_above_chk.setToolTip(
            "When selecting/double-clicking a model, reset the camera\n"
            "to look straight down from above instead of keeping\n"
            "whatever angle it already happened to be at (which can\n"
            "end up looking up at the model from underneath).")
        nav_lay.addWidget(focus_above_chk)

        focus_above_dist_row = QHBoxLayout()
        focus_above_dist_row.addWidget(QLabel("From-above height:"))
        focus_above_dist_spin = QDoubleSpinBox()
        focus_above_dist_spin.setRange(10.0, 2000.0)
        focus_above_dist_spin.setSingleStep(10.0)
        focus_above_dist_spin.setValue(float(self.map_settings.get('focus_from_above_dist', 200.0)))
        focus_above_dist_spin.setEnabled(focus_above_chk.isChecked())
        focus_above_chk.toggled.connect(focus_above_dist_spin.setEnabled)
        focus_above_dist_row.addWidget(focus_above_dist_spin)
        nav_lay.addLayout(focus_above_dist_row)

        # IPL Object Editor always-on-top (Sep 5 2026,  "the
        # IPL file editor should stay on top, with a settings toggle
        # option")
        ipl_editor_on_top_chk = QCheckBox("Keep IPL Object Editor on top")
        ipl_editor_on_top_chk.setChecked(bool(self.map_settings.get('ipl_editor_always_on_top')))
        ipl_editor_on_top_chk.setToolTip(
            "Keeps the IPL Object Editor window above other windows,\n"
            "including outside this app, instead of it being able to\n"
            "get covered by whatever you click on next.")
        nav_lay.addWidget(ipl_editor_on_top_chk)

        # VC-layout IPL stems (Sep 5 2026,  "LC, MLL, VC are
        # still in VC format... a loading toggle to adjust ipl loading
        # patterns")
        vc_layout_row = QHBoxLayout()
        vc_layout_row.addWidget(QLabel("Load as VC format (stems):"))
        vc_layout_ipl_stems_edit = QLineEdit(str(self.map_settings.get('vc_layout_ipl_stems', '')))
        vc_layout_ipl_stems_edit.setPlaceholderText("e.g. lc,mll,vc")
        vc_layout_ipl_stems_edit.setToolTip(
            "Comma-separated IPL filename stems (no extension) to\n"
            "parse using VC's own field layout instead of this\n"
            "world's normal one - for sub-city IPLs (e.g. SOL's own\n"
            "LC/MLL/VC ones) that are still genuinely VC-format even\n"
            "though the rest of the world isn't.")
        vc_layout_row.addWidget(vc_layout_ipl_stems_edit)
        nav_lay.addLayout(vc_layout_row)

        nav_lay.addWidget(QLabel(""))
        nav_lay.addWidget(QLabel("—  Game Controller  —"))
        pad_chk = QCheckBox("Use game controller (PS5 / PS4 / Xbox, needs pygame)")
        pad_chk.setChecked(bool(self.map_settings.get('gamepad_enabled')))
        pad_chk.setToolTip(
            "Right stick orbit, L2/R2 zoom, left stick pan / move grab.\n"
            "Cross select, grab, drop | Square add to selection | Circle cancel / clear\n"
            "Triangle Move/Rotate | L1/R1 constraint | D-pad Z or 15 deg\n"
            "Options edge snap | Create duplicate | Touchpad drop to ground | L3 fine")
        nav_lay.addWidget(pad_chk)
        pad_dz_row = QHBoxLayout()
        pad_dz_row.addWidget(QLabel("Stick deadzone:"))
        pad_dz_spin = QDoubleSpinBox()
        pad_dz_spin.setRange(0.0, 0.5)
        pad_dz_spin.setSingleStep(0.01)
        pad_dz_spin.setValue(float(self.map_settings.get('gamepad_deadzone') or 0.15))
        pad_dz_row.addWidget(pad_dz_spin)
        pad_dz_row.addStretch()
        nav_lay.addLayout(pad_dz_row)

        nav_lay.addStretch()
        sp_grp = QGroupBox("Save Points")   # Save Points + LOD Test merged here
        QVBoxLayout(sp_grp).addWidget(sp_tab)
        nav_lay.insertWidget(nav_lay.count() - 1, sp_grp)
        nav_lay.insertWidget(nav_lay.count() - 1, lod_test_grp)
        tabs.addTab(textures_tab, "Textures")
        tabs.addTab(nav_tab, "Navigation")

        # TAB 8: KEYS & MOUSE - shortcuts, 3D mouse buttons, camera keys (buttons/<tool>.json)
        from apps.methods.input_bindings_dialog import add_bindings_tab
        bindings_save = add_bindings_tab(self, tabs)

        def apply_settings():  #vers 3
            # Loading / Map Assets tabs saved first (Aug 1 2026),
            # before any of this function's other, pre-existing logic
            # runs - that logic has at least one confirmed broken
            # reference (self.format_combo, unrelated to this work,
            # a leftover from an "Export" section whose widget was
            # never actually built) that raises before reaching the
            # bottom of this function, which would otherwise prevent
            # these settings from ever saving too. Uses the real
            # MapSettings persistence mechanism (self.map_settings),
            # unlike the rest of this dialog's own ad-hoc self.xxx =
            # ... attributes, since that's what IPL Controls and the
            # rest of the app actually read these settings from.
            self.map_settings.set('load_text_plus_binary_ipl_set', load_streams_chk.isChecked())
            self.map_settings.set('preload_img_on_dat_load', preload_img_chk.isChecked())
            self.map_settings.set('show_load_options_dialog', show_load_options_chk.isChecked())
            self.map_settings.set('auto_load_last_world', auto_load_last_world_chk.isChecked())
            self.map_settings.set('auto_dismiss_summary_dialog', auto_dismiss_summary_chk.isChecked())
            self.map_settings.set('auto_dismiss_summary_seconds', auto_dismiss_seconds_spin.value())
            self.map_settings.set('water2_use_texture', water2_use_texture_chk.isChecked())
            vp_for_water2 = getattr(self, 'preview_widget', None)
            if vp_for_water2 is not None and hasattr(vp_for_water2, 'set_water2_use_texture'):
                vp_for_water2.set_water2_use_texture(water2_use_texture_chk.isChecked())
            self.map_settings.set('water2_height_offset', water2_height_spin.value())
            if vp_for_water2 is not None and hasattr(vp_for_water2, 'set_water2_height_offset'):
                vp_for_water2.set_water2_height_offset(water2_height_spin.value())
            self.map_settings.set('water2_alpha', water2_alpha_spin.value())
            if vp_for_water2 is not None and hasattr(vp_for_water2, 'set_water2_alpha'):
                vp_for_water2.set_water2_alpha(water2_alpha_spin.value())
            self.map_settings.set('water2_x_offset', water2_x_spin.value())
            if vp_for_water2 is not None and hasattr(vp_for_water2, 'set_water2_x_offset'):
                vp_for_water2.set_water2_x_offset(water2_x_spin.value())
            self.map_settings.set('water2_y_offset', water2_y_spin.value())
            if vp_for_water2 is not None and hasattr(vp_for_water2, 'set_water2_y_offset'):
                vp_for_water2.set_water2_y_offset(water2_y_spin.value())
            self._write_water_offsets({'x': water2_x_spin.value(), 'y': water2_y_spin.value(),
                                       'z': water2_height_spin.value()})
            mode = water2_mode_combo.currentData()
            self.map_settings.set('water2_offset_mode', mode)
            self.map_settings.set('water2_offset_vc_only', mode == 'vc')
            if vp_for_water2 is not None and hasattr(vp_for_water2, 'set_water2_offset_mode'):
                vp_for_water2.set_water2_offset_mode(mode)
            self.map_settings.set('show_verbose_loading_dialog',   verbose_loading_chk.isChecked())
            self.map_settings.set('load_workers', workers_spin.value())
            self.map_settings.set('texture_downscale_enabled',   downscale_chk.isChecked())
            self.map_settings.set('texture_downscale_threshold', downscale_threshold_spin.value())
            self.map_settings.set('texture_downscale_target',    downscale_target_spin.value())
            self.map_settings.set('tex_keep_dxt', keep_dxt_chk.isChecked())
            for g in ('iv', 'stories', 'nif'):
                chk, over, to = tex_rows[g]
                self.map_settings.set(f'tex_limit_{g}_on', chk.isChecked())
                self.map_settings.set(f'tex_limit_{g}_over', over.value())
                self.map_settings.set(f'tex_limit_{g}_to', to.value())
            self.map_settings.set('lod_test_circle_radius', float(lod_test_radius_spin.value()))
            vp2 = getattr(self, 'preview_widget', None)
            if vp2 is not None and hasattr(vp2, '_lod_test_radius'):
                vp2._lod_test_radius = float(lod_test_radius_spin.value())
            self.map_settings.set('zoom_to_cursor', zoom_cursor_chk.isChecked())
            if vp2 is not None and hasattr(vp2, 'set_zoom_to_cursor'):
                vp2.set_zoom_to_cursor(zoom_cursor_chk.isChecked())
            self.map_settings.set('auto_highlight_hover', hover_highlight_chk.isChecked())
            if vp2 is not None and hasattr(vp2, 'set_hover_highlight_enabled'):
                vp2.set_hover_highlight_enabled(hover_highlight_chk.isChecked())
            self.map_settings.set('focus_from_above', focus_above_chk.isChecked())
            self.map_settings.set('focus_from_above_dist', float(focus_above_dist_spin.value()))
            self.map_settings.set('ipl_editor_always_on_top', ipl_editor_on_top_chk.isChecked())
            self._apply_ipl_editor_on_top_setting()
            self.map_settings.set('vc_layout_ipl_stems', vc_layout_ipl_stems_edit.text().strip())

            bindings_save()                                   # buttons/<tool>.json

            self.map_settings.save()
            vp = getattr(self, 'preview_widget', None)
            if vp is not None and hasattr(vp, 'set_texture_downscale_settings'):
                vp.set_texture_downscale_settings(
                    downscale_chk.isChecked(),
                    downscale_threshold_spin.value(),
                    downscale_target_spin.value())
            if getattr(self, '_model_cache', None) is not None:   # all formats
                if self._model_cache.set_texture_limit(self._texture_limits(),
                                                       self.map_settings.get('tex_keep_dxt')):
                    self._reload_world_textures()             # new limit applies now, not next load

            # Render tab (Aug 16 2026)
            if vp is not None:
                s = style_combo.currentText()
                rev = {"Wireframe": "wireframe", "Solid": "solid", "Textured": "textured"}
                vp.set_render_mode(rev.get(s, "solid"))
                if bg_preview.property("reset_to_theme"):
                    vp.set_checkerboard_background()
                else:
                    chosen = bg_preview.property("chosen")
                    if chosen:
                        vp.set_background_color(chosen)
            path_chosen = path_preview.property("chosen")
            if path_chosen:
                self.map_settings.set('path_line_color', path_chosen)
                if vp is not None and hasattr(vp, 'set_path_line_color'):
                    pcr, pcg, pcb = path_chosen
                    vp.set_path_line_color(pcr / 255.0, pcg / 255.0, pcb / 255.0)
            node_chosen = node_preview.property("chosen")
            if node_chosen:
                self.map_settings.set('path_node_color', node_chosen)
                if vp is not None and hasattr(vp, 'set_path_node_color'):
                    ncr, ncg, ncb = node_chosen
                    vp.set_path_node_color(ncr / 255.0, ncg / 255.0, ncb / 255.0)
            self.map_settings.set('path_line_thickness', line_thickness_spin.value())
            if vp is not None and hasattr(vp, 'set_path_line_thickness'):
                vp.set_path_line_thickness(line_thickness_spin.value())
            self.map_settings.set('path_node_size', node_size_spin.value())
            if vp is not None and hasattr(vp, 'set_path_node_size'):
                vp.set_path_node_size(node_size_spin.value())

            track_chosen = track_preview.property("chosen")
            if track_chosen:
                self.map_settings.set('track_color', track_chosen)
                if vp is not None and hasattr(vp, 'set_track_color'):
                    tcr, tcg, tcb = track_chosen
                    vp.set_track_color(tcr / 255.0, tcg / 255.0, tcb / 255.0)
            self.map_settings.set('track_line_thickness', track_thickness_spin.value())
            if vp is not None and hasattr(vp, 'set_track_line_thickness'):
                vp.set_track_line_thickness(track_thickness_spin.value())

            airtrain_chosen = airtrain_preview.property("chosen")
            if airtrain_chosen:
                self.map_settings.set('airtrain_color', airtrain_chosen)
                if vp is not None and hasattr(vp, 'set_airtrain_color'):
                    acr, acg, acb = airtrain_chosen
                    vp.set_airtrain_color(acr / 255.0, acg / 255.0, acb / 255.0)
            self.map_settings.set('airtrain_line_thickness', airtrain_thickness_spin.value())
            if vp is not None and hasattr(vp, 'set_airtrain_line_thickness'):
                vp.set_airtrain_line_thickness(airtrain_thickness_spin.value())
            cull_chosen = cull_preview.property("chosen")
            if cull_chosen:
                self.map_settings.set('cull_box_color', cull_chosen)
                if vp is not None and hasattr(vp, 'set_cull_box_color'):
                    ccr, ccg, ccb = cull_chosen
                    vp.set_cull_box_color(ccr / 255.0, ccg / 255.0, ccb / 255.0)
            zone_chosen = zone_preview.property("chosen")
            if zone_chosen:
                self.map_settings.set('zone_box_color', zone_chosen)
                if vp is not None and hasattr(vp, 'set_zone_box_color'):
                    zcr, zcg, zcb = zone_chosen
                    vp.set_zone_box_color(zcr / 255.0, zcg / 255.0, zcb / 255.0)
            occl_chosen = occl_preview.property("chosen")
            if occl_chosen:
                self.map_settings.set('occl_box_color', occl_chosen)
                if vp is not None and hasattr(vp, 'set_occl_box_color'):
                    ocr, ocg, ocb = occl_chosen
                    vp.set_occl_box_color(ocr / 255.0, ocg / 255.0, ocb / 255.0)
            self.map_settings.set('box_axis_colors', axis_colors_chk.isChecked())
            if vp is not None and hasattr(vp, 'set_box_axis_colors'):
                vp.set_box_axis_colors(axis_colors_chk.isChecked())
            self.map_settings.set('box_unique_colors', unique_colors_chk.isChecked())
            if vp is not None and hasattr(vp, 'set_box_unique_colors'):
                vp.set_box_unique_colors(unique_colors_chk.isChecked())
            self.map_settings.set('no_clip_boxes', no_clip_chk.isChecked())
            if vp is not None and hasattr(vp, 'set_no_clip_boxes'):
                vp.set_no_clip_boxes(no_clip_chk.isChecked())
            self.map_settings.set('radar_tiles_show_grid', radar_show_grid_chk.isChecked())
            self.map_settings.set('gamepad_enabled', pad_chk.isChecked())
            self.map_settings.set('gamepad_deadzone', pad_dz_spin.value())
            self._apply_gamepad_setting()
            self.map_settings.set('savepoints_enabled', sp_enabled_chk.isChecked())
            self.map_settings.set('savepoints_interval_min', sp_interval_spin.value())
            self.map_settings.set('savepoints_keep', sp_keep_spin.value())
            if hasattr(self, '_savepoint_timer'):
                self._apply_savepoint_settings()
            grid_type_key = grid_type_combo.currentData()
            self.map_settings.set('grid_type', grid_type_key)
            if vp is not None and hasattr(vp, 'set_grid_type'):
                vp.set_grid_type(grid_type_key)

            grid_bg_rgb = grid_bg_color_btn.property('rgb') or grid_bg_color
            grid_line_rgb = grid_line_color_btn.property('rgb') or grid_line_color
            grid_size = grid_line_size_spin.value()
            self.map_settings.set('grid_bg_color', tuple(grid_bg_rgb))
            self.map_settings.set('grid_line_color', tuple(grid_line_rgb))
            self.map_settings.set('grid_line_size', grid_size)
            if vp is not None and hasattr(vp, 'set_grid_colors'):
                vp.set_grid_colors(tuple(grid_bg_rgb), tuple(grid_line_rgb))
            if vp is not None and hasattr(vp, 'set_grid_line_size'):
                vp.set_grid_line_size(grid_size)

            grid_spacing_val = grid_spacing_spin.value()
            self.map_settings.set('grid_spacing', grid_spacing_val)
            if vp is not None and hasattr(vp, 'set_grid_spacing'):
                vp.set_grid_spacing(grid_spacing_val)

            grid_fixed_step_val = grid_fixed_step_spin.value()
            self.map_settings.set('grid_fixed_step', grid_fixed_step_val)
            if vp is not None and hasattr(vp, 'set_grid_fixed_step'):
                vp.set_grid_fixed_step(grid_fixed_step_val)

            grid_cell_count_val = grid_cell_count_combo.currentData()
            self.map_settings.set('grid_cell_count', grid_cell_count_val)
            if vp is not None and hasattr(vp, 'set_grid_cell_count'):
                vp.set_grid_cell_count(grid_cell_count_val)

            hide_over_radar_val = grid_hide_over_radar_chk.isChecked()
            self.map_settings.set('grid_hide_over_radar_tiles', hide_over_radar_val)
            if vp is not None and hasattr(vp, 'set_grid_hide_over_radar_tiles'):
                vp.set_grid_hide_over_radar_tiles(hide_over_radar_val)

            grid_scale_mode_val = grid_scale_mode_combo.currentData()
            self.map_settings.set('grid_scale_mode', grid_scale_mode_val)
            if vp is not None and hasattr(vp, 'set_grid_scale_mode'):
                vp.set_grid_scale_mode(grid_scale_mode_val)
            if grid_scale_mode_val == 'radar_tiles' and vp is not None and hasattr(vp, 'set_radar_grid_extent'):
                from apps.methods.gta_dat_parser import RADAR_GRID_PRESETS
                loader = getattr(self, '_world_loader', None)
                game_key = getattr(loader, 'game', 'sa') if loader is not None else 'sa'
                preset = RADAR_GRID_PRESETS.get(game_key, RADAR_GRID_PRESETS['sa'])
                grid_size = preset['grid_size']
                tile_size = grid_size / preset['tiles_per_side']
                vp.set_radar_grid_extent(tile_size, grid_size / 2.0)

            fill_mode = grid_fill_mode_combo.currentData()
            tex_path = grid_texture_path_edit.text()
            tile_size = grid_tile_size_spin.value()
            self.map_settings.set('squares_fill_mode', fill_mode)
            self.map_settings.set('squares_texture_path', tex_path)
            self.map_settings.set('squares_tile_size', tile_size)
            if vp is not None and hasattr(vp, 'set_squares_fill'):
                vp.set_squares_fill(fill_mode, tuple(grid_bg_rgb), tex_path, tile_size)

            texture_alpha_val = grid_texture_alpha_spin.value()
            self.map_settings.set('squares_texture_alpha', texture_alpha_val)
            if vp is not None and hasattr(vp, 'set_squares_texture_alpha'):
                vp.set_squares_texture_alpha(texture_alpha_val)

            show_lines_val = grid_show_lines_chk.isChecked()
            self.map_settings.set('grid_show_lines', show_lines_val)
            if vp is not None and hasattr(vp, 'set_grid_show_lines'):
                vp.set_grid_show_lines(show_lines_val)

            skybox_path_val = skybox_path_edit.text()
            self.map_settings.set('skybox_path', skybox_path_val)
            if vp is not None and hasattr(vp, 'set_skybox_path'):
                vp.set_skybox_path(skybox_path_val)

            timecyc_path_val = timecyc_path_edit.text()
            self.map_settings.set('timecyc_path', timecyc_path_val)

            sky_flip_val = sky_flip_chk.isChecked()
            self.map_settings.set('sky_gradient_flipped', sky_flip_val)
            if vp is not None and hasattr(vp, 'set_sky_gradient_flipped'):
                vp.set_sky_gradient_flipped(sky_flip_val)

            try:
                # Adjusted for COL Wireframe, Mesh
                default_font_family = default_font_combo.currentFont().family()
                default_font_size_v = default_font_size.value()
                self.setFont(QFont(default_font_family, default_font_size_v))
                title_font_family = title_font_combo.currentFont().family()
                title_font_size_v = title_font_size.value()
                self.title_font = QFont(title_font_family, title_font_size_v)
                panel_font_family = panel_font_combo.currentFont().family()
                panel_font_size_v = panel_font_size.value()
                self.panel_font = QFont(panel_font_family, panel_font_size_v)
                button_font_family = button_font_combo.currentFont().family()
                button_font_size_v = button_font_size.value()
                self.button_font = QFont(button_font_family, button_font_size_v)
                infobar_font_family = infobar_font_combo.currentFont().family()
                infobar_font_size_v = infobar_font_size.value()
                self.infobar_font = QFont(infobar_font_family, infobar_font_size_v)

                # Apply fonts to UI
                self._apply_title_font()
                self._apply_panel_font()
                self._apply_button_font()
                self._apply_infobar_font()

                mode_map = {0: 'both', 1: 'icons', 2: 'text'}
                self.button_display_mode = mode_map[button_mode_combo.currentIndex()]

                # EXPORT
                # self.format_combo was never actually created anywhere (Aug 1 2026)
                format_combo = getattr(self, 'format_combo', None)
                if format_combo is not None:
                    self.default_export_format = format_combo.currentText()

                # PREVIEW
                self.zoom_level = zoom_spin.value() / 100.0

                bg_modes = ['solid', 'checkerboard', 'grid']
                self.background_mode = bg_modes[bg_mode_combo.currentIndex()]

                self._checkerboard_size = cb_spin.value()
                self._overlay_opacity = opacity_spin.value()

                #  persistence for everything just applied above (Aug 20 2026)
                self.map_settings.set('default_font_family', default_font_family)
                self.map_settings.set('default_font_size', default_font_size_v)
                self.map_settings.set('title_font_family', title_font_family)
                self.map_settings.set('title_font_size', title_font_size_v)
                self.map_settings.set('panel_font_family', panel_font_family)
                self.map_settings.set('panel_font_size', panel_font_size_v)
                self.map_settings.set('button_font_family', button_font_family)
                self.map_settings.set('button_font_size', button_font_size_v)
                self.map_settings.set('infobar_font_family', infobar_font_family)
                self.map_settings.set('infobar_font_size', infobar_font_size_v)
                self.map_settings.set('button_display_mode', self.button_display_mode)
                self.map_settings.set('zoom_level', self.zoom_level)
                self.map_settings.set('background_mode', self.background_mode)
                self.map_settings.set('checkerboard_size', self._checkerboard_size)
                self.map_settings.set('overlay_opacity', self._overlay_opacity)
                self.map_settings.save()

                # Update preview widget
                if hasattr(self, 'preview_widget'):
                    # Aug 1 2026
                    self.preview_widget.set_checkerboard_background()
                    if self.background_mode == 'checkerboard':
                        self.preview_widget._checkerboard_size = self._checkerboard_size

                # Apply button display mode
                if hasattr(self, '_update_all_buttons'):
                    self._update_all_buttons()

                # Refresh display

                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Workshop settings updated successfully")
            except Exception as e:
                print(f"[Settings] Some pre-existing settings could not be applied: {e}")

        from apps.methods.ui_sizes import add_sizes_tab             # own sizes or global
        add_sizes_tab(self, tabs)
        return tabs, apply_settings

    def _show_workshop_settings(self, initial_tab=None): #vers 3
        """Show complete workshop settings dialog - standalone-only
        path now (Aug 15 2026)"""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QPushButton

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        # Widened from 650 (Aug 16 2026)
        dialog.setMinimumWidth(850)
        dialog.setMinimumHeight(550)

        layout = QVBoxLayout(dialog)

        tabs, apply_settings = self._build_workshop_settings_tabs()
        tabs.setUsesScrollButtons(False)
        if initial_tab:
            for i in range(tabs.count()):
                if tabs.tabText(i) == initial_tab:
                    tabs.setCurrentIndex(i)
                    break
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


# - Rest of the logic for the panels

    def _apply_title_font(self): #vers 1
        """Apply title font to title bar labels"""
        if hasattr(self, 'title_font'):
            # Find all title labels
            for label in self.findChildren(QLabel):
                if label.objectName() == "title_label" or "🗺️" in label.text():
                    label.setFont(self.title_font)


    def _apply_panel_font(self): #vers 1
        """Apply panel font to info panels and labels"""
        if hasattr(self, 'panel_font'):
            # Apply to info labels (Mipmaps, Bumpmaps, status labels)
            for label in self.findChildren(QLabel):
                if any(x in label.text() for x in ["Mipmaps:", "Bumpmaps:", "Status:", "Type:", "Format:"]):
                    label.setFont(self.panel_font)


    def _apply_button_font(self): #vers 1
        """Apply button font to all buttons"""
        if hasattr(self, 'button_font'):
            for button in self.findChildren(QPushButton):
                button.setFont(self.button_font)


    def _apply_infobar_font(self): #vers 1
        """Apply fixed-width font to info bar at bottom"""
        if hasattr(self, 'infobar_font'):
            if hasattr(self, 'info_bar'):
                self.info_bar.setFont(self.infobar_font)


# - Marker 6


    def _launch_theme_settings(self): #vers 2
        """Launch theme engine from app_settings_system"""
        try:
            from apps.utils.app_settings_system import AppSettings, SettingsDialog

            # Get or create app_settings
            if not hasattr(self, 'app_settings') or self.app_settings is None:
                self.app_settings = AppSettings()
                if not hasattr(self.app_settings, 'current_settings'):
                    print("AppSettings failed to initialize")
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
                print("Theme settings applied")
                if hasattr(self, 'main_window') and self.main_window:
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Theme settings updated")

        except Exception as e:
            print(f"Theme settings error: {e}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Theme Error", f"Could not load theme system:\n{e}")



    def _show_settings_dialog(self): #vers 8
        """Show comprehensive settings dialog with all tabs including hotkeys"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                                    QWidget, QLabel, QPushButton, QGroupBox,
                                    QCheckBox, QSpinBox, QFormLayout, QScrollArea,
                                    QKeySequenceEdit, QComboBox, QMessageBox)
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
        show_preview_check.setToolTip("Automatically open preview when selecting surface")
        preview_window_layout.addWidget(show_preview_check)

        auto_refresh_check = QCheckBox("Auto-refresh preview on selection")
        auto_refresh_check.setChecked(getattr(self, 'auto_refresh_preview', True))
        auto_refresh_check.setToolTip("Update preview immediately when clicking surface")
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
        bg_combo.addItems(["Black", "White", "Gray", "Custom Color"])
        bg_combo.setCurrentText(getattr(self, 'preview_background', 'Checkerboard'))
        preview_bg_layout.addWidget(bg_combo)

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
        smooth_zoom_check.setToolTip("Better quality but slower for large model mesh")
        preview_zoom_layout.addWidget(smooth_zoom_check)

        preview_zoom_group.setLayout(preview_zoom_layout)
        preview_layout.addWidget(preview_zoom_group)

        preview_layout.addStretch()
        tabs.addTab(preview_tab, "Preview")

        # === EXPORT TAB ===
        export_tab = QWidget()
        export_layout = QVBoxLayout(export_tab)

        # Export format
        format_group = QGroupBox("Default Collision Export Format")
        format_layout = QVBoxLayout()

        format_combo = QComboBox()
        format_combo.addItems(["COL", "COL2", "COL3", "CST", "3DS"])
        format_combo.setCurrentText(getattr(self, 'default_export_format', 'COL'))
        format_layout.addWidget(format_combo)
        format_hint = QLabel("COL recommended for GTAIII/VC, COL2 for SA")
        format_hint.setStyleSheet("color: palette(placeholderText); font-style: italic;")
        format_layout.addWidget(format_hint)

        format_group.setLayout(format_layout)
        export_layout.addWidget(format_group)

        # Export options
        export_options_group = QGroupBox("Export Options")
        export_options_layout = QVBoxLayout()

        preserve_shadow_check = QCheckBox("Preserve Shadow Mesh when exporting")
        preserve_shadow_check.setChecked(getattr(self, 'export_preserve_shadow', True))
        export_options_layout.addWidget(preserve_shadow_check)

        export_shadowm_check = QCheckBox("Export shadow as separate files")
        export_shadowm_check.setChecked(getattr(self, 'export_shadow_separate', False))
        export_shadowm_check.setToolTip("Save each shadow map as _shadow.col, etc.")
        export_options_layout.addWidget(export_shadowm_check)

        create_subfolders_check = QCheckBox("Create subfolders when exporting all")
        create_subfolders_check.setChecked(getattr(self, 'export_create_subfolders', False))
        create_subfolders_check.setToolTip("Organize exports into folders by col type")
        export_options_layout.addWidget(create_subfolders_check)

        export_options_group.setLayout(export_options_layout)
        export_layout.addWidget(export_options_group)

        # Compatibility note
        compat_label = QLabel(
            "Export settings apply to the active model. "
            "COL export uses the format set in the Format combo. "
            "OBJ export includes vertex positions and face indices."
        )
        compat_label.setWordWrap(True)
        compat_label.setStyleSheet("padding: 10px; background-color: palette(mid); border-radius: 4px;")
        export_layout.addWidget(compat_label)

        # - Texture Sources
        tex_src_group = QGroupBox("Texture Sources")
        tex_src_layout = QVBoxLayout(tex_src_group)

        tex_src_layout.addWidget(QLabel(
            "texlist/ folder: pre-exported PNG/IFF/TGA textures.\nFallback when TXD not found in any open IMG."))

        texlist_row = QHBoxLayout()
        texlist_row.addWidget(QLabel("texlist/ folder:"))
        texlist_edit = QLineEdit()
        texlist_edit.setPlaceholderText("Browse to set, or leave blank for auto-discover")
        texlist_edit.setText(getattr(self, '_texlist_folder', '') or '')
        texlist_edit.setFixedHeight(26)
        texlist_row.addWidget(texlist_edit, 1)

        texlist_browse = QPushButton("…")
        texlist_browse.setFixedSize(28, 24)
        texlist_browse.setToolTip("Browse for texlist/ root folder")
        texlist_browse.clicked.connect(lambda: (
            (folder := __import__('PyQt6.QtWidgets', fromlist=['QFileDialog'])
                       .QFileDialog.getExistingDirectory(
                           dialog, "Select texlist/ folder",
                           texlist_edit.text() or __import__('os').path.expanduser('~'))),
            texlist_edit.setText(folder) if folder else None))
        texlist_row.addWidget(texlist_browse)
        tex_src_layout.addLayout(texlist_row)

        auto_discover_lbl = QLabel(
            "If blank: auto-discovers texlist/ next to the loaded DFF file.")
        auto_discover_lbl.setStyleSheet("color: palette(mid); font-style: italic;")
        tex_src_layout.addWidget(auto_discover_lbl)

        export_layout.addWidget(tex_src_group)

        export_layout.addStretch()
        tabs.addTab(export_tab, "Export")

        # === IMPORT TAB ===
        import_tab = QWidget()
        import_layout = QVBoxLayout(import_tab)

        # Import behavior
        import_behavior_group = QGroupBox("Import Behavior")
        import_behavior_layout = QVBoxLayout()

        replace_check = QCheckBox("Replace existing collision with same name")
        replace_check.setChecked(getattr(self, 'import_replace_existing', False))
        import_behavior_layout.addWidget(replace_check)

        auto_format_check = QCheckBox("Automatically select best format")
        auto_format_check.setChecked(getattr(self, 'import_auto_format', True))
        auto_format_check.setToolTip("Choose COL/COL3 based on collision version")
        import_behavior_layout.addWidget(auto_format_check)

        import_behavior_group.setLayout(import_behavior_layout)
        import_layout.addWidget(import_behavior_group)

        # Import format
        import_format_group = QGroupBox("Default Collision Format")
        import_format_layout = QVBoxLayout()

        import_format_combo = QComboBox()
        import_format_combo.addItems(["COL", "COL2", "COL3", "CST", "3DS"])
        import_format_combo.setCurrentText(getattr(self, 'default_import_format', 'COL'))
        import_format_layout.addWidget(import_format_combo)

        format_note = QLabel("COL2/COL3: compression\n, COL type 1 always Uncompressed")
        format_note.setStyleSheet("color: palette(placeholderText); font-style: italic;")
        import_format_layout.addWidget(format_note)

        import_format_group.setLayout(import_format_layout)
        import_layout.addWidget(import_format_group)

        import_layout.addStretch()
        tabs.addTab(import_tab, "Import")

        # === Collision CONSTRAINTS TAB ===
        constraints_tab = QWidget()
        constraints_layout = QVBoxLayout(constraints_tab)

        # Collision naming
        naming_group = QGroupBox("Collision Naming")
        naming_layout = QVBoxLayout()

        name_limit_check = QCheckBox("Enable name length limit")
        name_limit_check.setChecked(getattr(self, 'name_limit_enabled', True))
        name_limit_check.setToolTip("Enforce maximum Collision name length")
        naming_layout.addWidget(name_limit_check)

        char_limit_layout = QHBoxLayout()
        char_limit_layout.addWidget(QLabel("Maximum characters:"))
        char_limit_spin = QSpinBox()
        char_limit_spin.setRange(8, 64)
        char_limit_spin.setValue(getattr(self, 'max_collision_name_length', 32))
        char_limit_spin.setToolTip("RenderWare default is 32 characters")
        char_limit_layout.addWidget(char_limit_spin)
        char_limit_layout.addStretch()
        naming_layout.addLayout(char_limit_layout)

        naming_group.setLayout(naming_layout)
        constraints_layout.addWidget(naming_group)

        # Format support
        format_support_group = QGroupBox("Format Support")
        format_support_layout = QVBoxLayout()

        format_support_group.setLayout(format_support_layout)
        constraints_layout.addWidget(format_support_group)

        constraints_layout.addStretch()
        tabs.addTab(constraints_tab, "Constraints")

        # === KEYS & MOUSE TAB (methods/input_bindings_dialog.py) ===
        from apps.methods.input_bindings_dialog import add_bindings_tab
        bindings_save = add_bindings_tab(self, tabs)

        # Add tabs widget to main layout
        layout.addWidget(tabs)

        # Dialog buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        def apply_settings(close_dialog=False):  #vers 1
            """Apply all settings"""
            # Apply display settings
            self.thumbnail_size = thumb_size_spin.value()
            self.table_row_height = row_height_spin.value()
            self.show_grid_lines = show_grid_check.isChecked()

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
            self.export_preserve_shadow = preserve_shadow_check.isChecked()
            self.export_shadow_separate = export_shadowm_check.isChecked()
            self.export_create_subfolders = create_subfolders_check.isChecked()
            # Texture Sources — texlist folder
            new_texlist = texlist_edit.text().strip()
            if new_texlist != getattr(self, '_texlist_folder', ''):
                self._texlist_folder = new_texlist
                self._save_texlist_setting()

            # Apply import settings
            self.import_replace_existing = replace_check.isChecked()
            self.import_auto_format = auto_format_check.isChecked()
            self.default_import_format = import_format_combo.currentText()

            # Apply constraint settings
            self.name_limit_enabled = name_limit_check.isChecked()
            self.max_surface_name_length = char_limit_spin.value()

            bindings_save()                                   # buttons/<tool>.json

            # Refresh UI with new settings
            if hasattr(self, '_reload_surface_table'):
                self._reload_surface_table()

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

        # Icon display mode submenu (icon-only mode uses _update_tex_btn_compact)
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


    def _set_icon_display_mode(self, mode: str): #vers 1
        """Set button display mode: both, icons or text."""
        if mode != self.button_display_mode:
            self.button_display_mode = mode
            self._update_all_buttons()
    def _enable_move_mode(self): #vers 2
        """Enable move window mode using system move"""
        handle = self.windowHandle()
        if handle and hasattr(handle, 'startSystemMove'):
            handle.startSystemMove()

    def _toggle_upscale_native(self): #vers 1
        """Toggle upscale native resolution"""
        # Placeholder for upscale native functionality
        print("Upscale Native toggled")

    def _show_shaders_dialog(self): #vers 2
        """Viewport render style presets."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QLabel,
                                      QRadioButton, QButtonGroup, QPushButton)
        dlg = QDialog(self)
        dlg.setWindowTitle("Viewport Render Style")
        dlg.setFixedSize(260, 200)
        lay = QVBoxLayout(dlg)
        lay.addWidget(QLabel("<b>Render style (viewport display only)</b>"))
        presets = [("Wireframe", "wireframe"), ("Solid", "solid"),
                   ("Painted (material colours)", "painted")]
        grp = QButtonGroup(dlg)
        current = getattr(self, '_render_mode', 'wireframe')
        for label, key in presets:
            rb = QRadioButton(label)
            rb.setChecked(key == current)
            def _set(checked, k=key):  #vers 1
                if checked:
                    self._render_mode = k
                    pw = getattr(self, 'preview_widget', None)
                    if pw and hasattr(pw, '_refresh'):
                        pw._refresh()
            rb.toggled.connect(_set)
            grp.addButton(rb)
            lay.addWidget(rb)
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dlg.accept)
        lay.addWidget(close_btn)
        dlg.exec()


    def _get_icon_color(self): #vers 3
        """Get icon colour from current theme — returns text_primary.
        Falls back to main_window app_settings if own settings not loaded."""
        as_ = (self.app_settings
               or getattr(getattr(self, 'main_window', None), 'app_settings', None))
        if as_:
            try:
                colors = as_.get_theme_colors() or {}
                return colors.get('text_primary', '#cccccc')
            except Exception:
                pass
        return '#cccccc'

    _WORLD_MAP_SVG = """<svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
  <circle cx="32" cy="32" r="30" fill="#1a5f9e"/>
  <circle cx="32" cy="32" r="30" fill="none" stroke="#0d3a63" stroke-width="1.5"/>
  <clipPath id="mw-globe-clip"><circle cx="32" cy="32" r="30"/></clipPath>
  <g clip-path="url(#mw-globe-clip)">
    <path d="M2,26 Q10,20 18,24 Q26,20 32,25 Q38,21 46,25 Q54,22 62,27
             L62,38 Q54,34 46,37 Q38,33 32,37 Q26,34 18,36 Q10,32 2,37 Z"
          fill="#4d8f3a"/>
    <path d="M4,23 Q12,26 20,22 Q28,27 34,23 Q42,27 50,22 Q58,25 60,22
             L60,29 Q52,26 44,30 Q36,26 30,30 Q22,26 14,29 Q8,27 4,29 Z"
          fill="#7fb54a"/>
    <path d="M6,32 Q14,29 22,33 Q30,30 36,34 Q44,30 52,33 Q58,31 60,33
             L60,40 Q52,37 44,40 Q36,36 30,40 Q22,37 14,39 Q8,38 6,40 Z"
          fill="#c8c141"/>
    <ellipse cx="16" cy="21" rx="5" ry="3" fill="#a9cf4c"/>
    <ellipse cx="41" cy="40" rx="6" ry="3" fill="#8fbf4f"/>
    <ellipse cx="53" cy="24" rx="4" ry="2.5" fill="#d8d24a"/>
  </g>
</svg>"""

    @staticmethod
    def _make_world_map_icon(): #vers 2
        """ multi-color SVG Earth icon (blue ocean, green-to-yellow
        continent shapes near the equator)"""
        from PyQt6.QtSvg import QSvgRenderer
        icon = QIcon()
        renderer = QSvgRenderer(MapSettingsMixin._WORLD_MAP_SVG.encode())
        for size in (16, 24, 32, 48, 64):
            pix = QPixmap(size, size)
            pix.fill(Qt.GlobalColor.transparent)
            painter = QPainter(pix)
            painter.setRenderHint(QPainter.RenderHint.Antialiasing)
            renderer.render(painter)
            painter.end()
            icon.addPixmap(pix)
        return icon

    def _get_ui_color(self, key): #vers 2
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)


    def _get_icon_set(self) -> str: #vers 2
        """Return active icon set: 'default' or '3dsmax'.
        Read from model_workshop.json 'icon_set' key."""
        try:
            import json
            path = self._ribbon_config_path()
            if path.exists():
                return json.loads(path.read_text()).get('icon_set', 'default')
        except Exception:
            pass
        return 'default'



    def _apply_theme(self): #vers 8
        """Apply theme — matches COL/TXD pattern exactly.
        Sets QApplication stylesheet globally, clears own override to inherit."""
        try:
            mw = getattr(self, 'main_window', None)
            app_settings = None
            if hasattr(self, 'app_settings') and self.app_settings:
                app_settings = self.app_settings
            elif mw and hasattr(mw, 'app_settings'):
                app_settings = mw.app_settings

            if app_settings and hasattr(app_settings, 'get_stylesheet'):
                from PyQt6.QtWidgets import QApplication
                ss = app_settings.get_stylesheet()
                if ss:
                    QApplication.instance().setStyleSheet(ss)
            # Clear widget-level override — inherit from QApplication like COL/TXD
            self.setStyleSheet("")
        except Exception as e:
            print(f"Theme application error: {e}")






# ----- Render functions

    def _setup_hotkeys(self): #vers 4
        """Setup Plasma6-style keyboard shortcuts for this application - checks for existing methods"""
        from PyQt6.QtGui import QShortcut, QKeySequence

        # === FILE OPERATIONS ===

        # Open col (Ctrl+O)
        self.hotkey_open = QShortcut(QKeySequence.StandardKey.Open, self)
        if hasattr(self, 'open_col_file'):
            self.hotkey_open.activated.connect(self.open_col_file)
        elif hasattr(self, '_open_col_file'):
            self.hotkey_open.activated.connect(self._open_col_file)

        # Save col (Ctrl+S)
        self.hotkey_save = QShortcut(QKeySequence.StandardKey.Save, self)
        self.hotkey_save.activated.connect(self._save_map_or_model)

        # Force Save col (Alt+Shift+S)
        self.hotkey_force_save = QShortcut(QKeySequence("Alt+Shift+S"), self)
        if not hasattr(self, '_force_save_col'):
            # Create force save method inline if it doesn't exist
            def force_save():  #vers 1
                if not self.collision_list:
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.warning(self, "No Collision", "No Collision to save")
                    return
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Force save triggered (Alt+Shift+S)")
                # Call save regardless of modified state
                if hasattr(self, '_save_col_file'):
                    self._save_col_file()

            self.hotkey_force_save.activated.connect(force_save)
        else:
            self.hotkey_force_save.activated.connect(self._force_save_col)

        # Save As (Ctrl+Shift+S)
        self.hotkey_save_as = QShortcut(QKeySequence.StandardKey.SaveAs, self)
        if hasattr(self, '_save_as_col_file'):
            self.hotkey_save_as.activated.connect(self._save_as_col_file)
        elif hasattr(self, '_save_col_file'):
            self.hotkey_save_as.activated.connect(self._save_col_file)

        # Close (Ctrl+W)
        self.hotkey_close = QShortcut(QKeySequence.StandardKey.Close, self)
        self.hotkey_close.activated.connect(self.close)

        # === EDIT OPERATIONS ===

        # Undo (Ctrl+Z)
        #  fix (Aug 20 2026,  "Undo/redo for mapping
        # changes") - was only ever wired to the older, separate
        # paint/material undo system (_undo_last_action, from the
        # collision-paint context) - the newer, general map-edit undo
        # stack (_map_undo/_map_redo/_push_map_undo, Aug 18 2026,
        # already wired to Position/Rotation/Scale nudges) had no
        # keyboard shortcut of its own at all. Tries map undo first
        # (the more general, always-relevant context for the main
        # Map Workshop view) and falls back to the older paint undo
        # only when the map undo stack is empty, so both real systems
        # stay reachable rather than one silently shadowing the other.
        self.hotkey_undo = QShortcut(QKeySequence.StandardKey.Undo, self)
        self.hotkey_undo.activated.connect(self._on_ctrl_z_pressed)

        # Redo (Ctrl+Y / Ctrl+Shift+Z, platform-appropriate via
        # StandardKey) - real fix, same request as above - no keyboard
        # shortcut existed for map redo at all before this, only
        # Shift+click on the Undo button.
        self.hotkey_redo = QShortcut(QKeySequence.StandardKey.Redo, self)
        self.hotkey_redo.activated.connect(self._map_redo)

        # Copy (Ctrl+C)
        self.hotkey_copy = QShortcut(QKeySequence.StandardKey.Copy, self)
        if hasattr(self, '_copy_collision'):
            self.hotkey_copy.activated.connect(self._copy_surface)


        # Paste (Ctrl+V)
        self.hotkey_paste = QShortcut(QKeySequence.StandardKey.Paste, self)
        if hasattr(self, '_paste_collision'):
            self.hotkey_paste.activated.connect(self._paste_surface)


        # Delete (Delete)
        self.hotkey_delete = QShortcut(QKeySequence.StandardKey.Delete, self)
        self.hotkey_delete.activated.connect(self._delete_selected_instances)
        # Drop selected objects to the ground (End)
        self.hotkey_drop_ground = QShortcut(QKeySequence("End"), self)
        self.hotkey_drop_ground.activated.connect(self._drop_selected_to_ground)


        # Duplicate (Ctrl+D)
        self.hotkey_duplicate = QShortcut(QKeySequence("Ctrl+D"), self)
        self.hotkey_duplicate.activated.connect(self._duplicate_selected)

        # Camera bookmarks: Ctrl+1..9 go, Ctrl+Shift+1..9 save
        self._camera_bookmark_keys = []
        for slot in range(1, 10):
            go = QShortcut(QKeySequence(f"Ctrl+{slot}"), self)
            go.activated.connect(lambda s=slot: self._goto_camera_bookmark(s))
            sv = QShortcut(QKeySequence(f"Ctrl+Shift+{slot}"), self)
            sv.activated.connect(lambda s=slot: self._save_camera_bookmark(s))
            self._camera_bookmark_keys += [go, sv]


        # Rename (F2)
        self.hotkey_rename = QShortcut(QKeySequence("F2"), self)
        if not hasattr(self, '_rename_collsion_shortcut'):
            # Create rename shortcut method inline
            def rename_shortcut():  #vers 1
                # Focus the name input field if it exists
                if hasattr(self, 'info_name'):
                    self.info_name.setReadOnly(False)
                    self.info_name.selectAll()
                    self.info_name.setFocus()
            self.hotkey_rename.activated.connect(rename_shortcut)
        else:
            self.hotkey_rename.activated.connect(self._rename_shadow_shortcut)

        # === Collision OPERATIONS ===

        # Export Collision (Ctrl+E)
        self.hotkey_export = QShortcut(QKeySequence("Ctrl+E"), self)
        if hasattr(self, 'export_selected_collision'):
            self.hotkey_export.activated.connect(self.export_selected_surface)

        # Export All (Ctrl+Shift+E)
        self.hotkey_export_all = QShortcut(QKeySequence("Ctrl+Shift+E"), self)
        if hasattr(self, 'export_all_collision'):
            self.hotkey_export_all.activated.connect(self.export_all_surfaces)


        # === VIEW OPERATIONS ===

        # Refresh (F5)d_btn
        self.hotkey_refresh = QShortcut(QKeySequence.StandardKey.Refresh, self)
        if hasattr(self, '_reload_surface_table'):
            self.hotkey_refresh.activated.connect(self._reload_surface_table)
        elif hasattr(self, 'reload_surface_table'):
            self.hotkey_refresh.activated.connect(self.reload_surface_table)
        elif hasattr(self, 'refresh'):
            self.hotkey_refresh.activated.connect(self.refresh)

        # Properties (Alt+Enter)
        self.hotkey_properties = QShortcut(QKeySequence("Alt+Return"), self)
        if hasattr(self, '_show_detailed_info'):
            self.hotkey_properties.activated.connect(self._show_detailed_info)
        elif hasattr(self, '_show_surface_info'):
            self.hotkey_properties.activated.connect(self._show_surface_info)

        # Settings (Ctrl+,)
        self.hotkey_settings = QShortcut(QKeySequence.StandardKey.Preferences, self)
        if hasattr(self, '_show_settings_dialog'):
            self.hotkey_settings.activated.connect(self._show_settings_dialog)
        elif hasattr(self, 'show_settings_dialog'):
            self.hotkey_settings.activated.connect(self.show_settings_dialog)
        elif hasattr(self, '_show_settings_hotkeys'):
            self.hotkey_settings.activated.connect(self._show_settings_hotkeys)

        # === NAVIGATION ===

        # Select All (Ctrl+A)
        self.hotkey_select_all = QShortcut(QKeySequence.StandardKey.SelectAll, self)
        self.hotkey_select_all.activated.connect(self._select_all_models)

        # Invert Selection (Ctrl+I)
        from PyQt6.QtGui import QKeySequence as _KS
        self.hotkey_invert = QShortcut(_KS("Ctrl+I"), self)
        self.hotkey_invert.activated.connect(self._invert_selection)

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

    def _show_about(self): #vers 1
        """Show Model Workshop about dialog."""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton, QTextEdit
        from PyQt6.QtGui import QFont
        from PyQt6.QtCore import Qt
        dlg = QDialog(self)
        dlg.setWindowTitle(f"About {App_name}")
        dlg.setMinimumWidth(480); dlg.setMinimumHeight(360)
        lay = QVBoxLayout(dlg); lay.setSpacing(10)

        hdr = QLabel(f"{App_name}  —  Build {App_build}")
        hdr.setFont(QFont('Arial', 14, QFont.Weight.Bold))
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        lay.addWidget(hdr)

        lay.addWidget(QLabel(f"Author: X-Seti  |  Part of IMG Factory 1.6",
                             alignment=Qt.AlignmentFlag.AlignCenter))

        info = QTextEdit(); info.setReadOnly(True)
        info.setHtml("""
            <b>Model Workshop Capabilities:</b><br><br>
            <b>✓ DFF Support:</b> GTA III / VC / SA RenderWare DFF model files<br>
            <b>✓ COL Support:</b> COL1 (III/VC) COL2/3/4 (SA) collision files<br>
            <b>✓ Viewport:</b> QPainter 2D + OpenGL 3D (GL toggle)<br>
            <b>✓ Editing:</b> Geometry, materials, UV, normals, vertex colours<br>
            <b>✓ Export:</b> OBJ, COL, texture extraction<br>
            <b>✓ IDE Integration:</b> Link to DAT Browser IDE entries<br>
            <b>✓ TXD:</b> Auto-load matching TXD for textured preview<br><br>
            <b>Shortcuts:</b><br>
            Ctrl+O: Open  |  Ctrl+S: Save  |  Ctrl+D: Duplicate object  |  End: Drop to ground  |  Ctrl+1..9 / Ctrl+Shift+1..9: Camera bookmarks<br>
            W / E: Move / Rotate gizmo  |  Tab / Shift+Tab: X, Y, XY, Z constraint  |  drag selected object to move<br>
            Ctrl+T: Open TXD  |  Ctrl+Z: Undo  |  Delete: Remove
        """)
        lay.addWidget(info, 1)

        btn = QPushButton('Close'); btn.clicked.connect(dlg.accept)
        lay.addWidget(btn)
        dlg.exec()
