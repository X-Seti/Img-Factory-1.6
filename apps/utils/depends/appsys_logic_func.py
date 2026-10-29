#this belongs in apps/utils/depends/appsys_logic_func.py - Version: 11
# X-Seti - October 20 2026 - IMG Factory 1.6 - Settings Dialog Logic

"""
Settings dialog apply, load, save, colours, fonts, themes.
Mixin for SettingsDialog in apps/utils/app_settings_system.py.
"""

##class AppsysLogicMixin: -
# _apply_button_sizes
# _apply_demo_theme
# _apply_gadget_styles_to_dialog
# _apply_palette_color
# _apply_picked_color
# _apply_quick_theme
# _apply_settings
# _browse_background_image
# _browse_panel_bg_image
# _build_btn_stylesheet
# _change_preview_scope
# _clear_background_image
# _clear_debug_log
# _collect_current_button_colors
# _collect_gadget_styles
# _collect_settings_contributions
# _get_default_button_colors
# _get_dialog_settings
# _load_current_settings
# _load_font_settings
# _load_panel_controls
# _load_theme_colors
# _lock_all_colors
# _ok_clicked
# _on_btn_style_changed
# _on_button_color_changed
# _on_button_theme_type_changed
# _on_color_alpha_changed
# _on_color_alpha_toggled
# _on_font_changed
# _on_global_bri_changed
# _on_global_hue_changed
# _on_global_sat_changed
# _on_size_changed
# _apply_theme_change
# _on_theme_changed
# _on_theme_color_changed
# _pick_button_color
# _pick_panel_colour
# _preview_button_colors
# _preview_font_changes
# _preview_theme_instantly
# _push_undo
# _random_theme
# _random_theme_from_picker
# _refresh_panel_previews
# _refresh_themes
# _reset_all_fonts
# _reset_all_settings
# _reset_button_colors_to_defaults
# _reset_demo_theme
# _reset_gadget_styles
# _reset_global_sliders
# _reset_to_defaults
# _save_current_theme
# _save_font_settings
# _save_theme_as
# _select_color_for_component
# _select_color_for_gadget
# _select_shadow_color
# _show_effect_preview_popup
# _show_full_preview
# _toggle_instant_apply
# _undo_color_change
# _unlock_all_colors
# _update_btn_style_preview
# _update_gadget_preview
# _update_preview_stats
# _update_theme_info
# apply_global_hsb_to_all_colors
# get_stylesheet
# refresh_themes_in_dialog

import json
import os
import random
from pathlib import Path

from PyQt6.QtCore import (Qt, pyqtSignal, QTimer, QThread, pyqtSlot, QRect, QDateTime,
                          QByteArray, QPoint, QSize)
from PyQt6.QtGui import QFont, QColor, QPixmap, QPainter, QCursor, QIcon
from PyQt6.QtWidgets import (
    QSizePolicy, QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout,
    QGridLayout, QButtonGroup, QRadioButton, QLabel, QPushButton,
    QComboBox, QCheckBox, QSpinBox, QMenu, QSlider, QSplitter,
    QGroupBox, QTabWidget, QDialog, QMessageBox,
    QFileDialog, QColorDialog, QFontDialog, QTextEdit,
    QTableWidget, QTableWidgetItem, QHeaderView, QFontComboBox,
    QScrollArea, QFrame, QLineEdit, QListWidget
)


class AppsysLogicMixin: #vers 1
    """Settings dialog apply, load, save, colours, fonts, themes."""

    def _apply_quick_theme(self, theme_name: str):
        """Apply quick theme with animation effect"""
        self.demo_theme_combo.setCurrentText(theme_name)
        self._apply_demo_theme(theme_name)

        # Animate button click
        sender = self.sender()
        if sender:
            original_text = sender.text()
            sender.setText(f"Applied!")
            QTimer.singleShot(1000, lambda: sender.setText(original_text))


    def _push_undo(self, color_key, old_value): #vers 1
        """Push a single colour change onto the undo stack."""
        if not hasattr(self, '_color_history'):
            self._color_history = []
        self._color_history.append((color_key, old_value))
        limit = getattr(self, '_history_limit', 30)
        if len(self._color_history) > limit:
            self._color_history.pop(0)
        # Update undo button tooltip
        btn = getattr(self, '_dialog_undo_btn', None)
        if btn:
            btn.setEnabled(True)
            btn.setToolTip(f"Undo — revert {color_key} ({len(self._color_history)} steps)")


    def _undo_color_change(self): #vers 1
        """Undo the last colour change — pops the history stack."""
        if not hasattr(self, '_color_history') or not self._color_history:
            return
        color_key, old_value = self._color_history.pop()
        # Restore in editor
        editor = self.color_editors.get(color_key)
        if editor:
            editor.set_color(old_value)
        # Restore in modified_colors
        self._modified_colors[color_key] = old_value
        # Update undo button
        btn = getattr(self, '_dialog_undo_btn', None)
        if btn:
            remaining = len(self._color_history)
            btn.setEnabled(remaining > 0)
            if remaining > 0:
                next_key, next_val = self._color_history[-1]
                btn.setToolTip(f"Undo — revert {next_key} ({remaining} steps)")
            else:
                btn.setToolTip("Nothing to undo")


    def _reset_all_settings(self): #vers 1
        """Reset all settings to defaults."""
        from PyQt6.QtWidgets import QMessageBox
        r = QMessageBox.question(self, "Reset Settings",
            "Reset ALL settings to defaults?\nThis cannot be undone.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if r == QMessageBox.StandardButton.Yes:
            self.app_settings.current_settings.update(
                self.app_settings.default_settings.copy())


    def _random_theme(self):
        """Apply random theme"""
        import random
        themes = list(self.app_settings.themes.keys())
        current = self.demo_theme_combo.currentText()
        themes.remove(current)  # Don't pick the same theme

        random_theme = random.choice(themes)
        self.demo_theme_combo.setCurrentText(random_theme)
        self._apply_demo_theme(random_theme)

        self.demo_log.append(f"Random theme: {random_theme}")


    def get_stylesheet(self): #vers 4
        """Generate complete stylesheet for current theme"""
        colors = self.get_theme_colors()
        return self.app_settings._generate_stylesheet(colors)


    def _random_theme_from_picker(self): #vers 1
        """Apply random theme from color picker tab"""
        import random

        # Get all available themes
        themes = list(self.app_settings.themes.keys())

        # Get current theme
        current = self.theme_selector_combo.currentData()

        # Remove current theme from options
        if current in themes:
            themes.remove(current)

        if not themes:
            QMessageBox.information(
                self,
                "No Other Themes",
                "No other themes available to randomize."
            )
            return

        # Pick random theme
        random_theme = random.choice(themes)

        # Find and set the theme in combo box
        for i in range(self.theme_selector_combo.count()):
            if self.theme_selector_combo.itemData(i) == random_theme:
                self.theme_selector_combo.setCurrentIndex(i)
                break

        # Show notification
        theme_display_name = self.app_settings.themes[random_theme].get("name", random_theme)
        QMessageBox.information(
            self,
            "Random Theme Applied",
            f"Applied theme: {theme_display_name}"
        )


    def _apply_palette_color(self, color): #vers 1
        """Apply palette color to selected element"""
        selected_data = self.selected_element_combo.currentData()
        if selected_data and selected_data in self.color_editors:
            self.color_editors[selected_data].set_color(color)


    def _update_gadget_preview(self): #vers 3
        """Update live preview with current gadget settings"""
        if not hasattr(self, 'preview_frame'):
            return

        # Get current theme colors
        theme_colors = self.app_settings.get_theme_colors()

        # Collect gadget settings
        button_radius = self.button_radius_slider.value()
        button_height = self.button_height_spin.value()
        button_h_padding = self.button_h_padding_spin.value()
        button_v_padding = self.button_v_padding_spin.value()

        slider_height = self.slider_height_spin.value()
        slider_handle_size = self.slider_handle_size.value()
        slider_handle_radius = self.slider_handle_radius.value()

        checkbox_size = self.checkbox_size_spin.value()
        checkbox_radius = self.checkbox_radius_slider.value()

        scrollbar_width = self.scrollbar_width_slider.value()
        scrollbar_radius = self.scrollbar_handle_radius.value()

        splitter_width = self.splitter_width_slider.value()
        splitter_radius = self.splitter_radius_slider.value()
        show_grip = self.splitter_show_grip.isChecked()

        panel_opacity = self.panel_opacity_slider.value()
        enable_animations = self.enable_animations.isChecked()

        # Build button stylesheet
        button_style = f"""
        QPushButton {{
            background-color: {theme_colors.get('button_normal', '#e0e0e0')};
            color: {theme_colors.get('text_primary', '#000000')};
            border: 1px solid {theme_colors.get('border', '#cccccc')};
            border-radius: {button_radius}px;
            padding: {button_v_padding}px {button_h_padding}px;
            min-height: {button_height}px;
            font-weight: bold;
        }}
        QPushButton:hover {{
            background-color: {theme_colors.get('button_hover', '#d0d0d0')};
        }}
        QPushButton:pressed {{
            background-color: {theme_colors.get('button_pressed', '#b0b0b0')};
        }}
        """

        # Apply to preview buttons
        if hasattr(self, 'preview_btn_normal'):
            self.preview_btn_normal.setStyleSheet(button_style)

        if hasattr(self, 'preview_btn_primary'):
            primary_style = button_style.replace(
                theme_colors.get('button_normal', '#e0e0e0'),
                theme_colors.get('action_export', '#e8f5e8')
            )
            self.preview_btn_primary.setStyleSheet(primary_style)

        if hasattr(self, 'preview_btn_danger'):
            danger_style = button_style.replace(
                theme_colors.get('button_normal', '#e0e0e0'),
                theme_colors.get('action_remove', '#ffebee')
            )
            self.preview_btn_danger.setStyleSheet(danger_style)

        # Build slider stylesheet
        slider_style = f"""
        QSlider::groove:horizontal {{
            height: {slider_height}px;
            background: {theme_colors.get('bg_tertiary', '#e0e0e0')};
            border: 1px solid {theme_colors.get('border', '#d0d0d0')};
            border-radius: {slider_height // 2}px;
        }}
        QSlider::handle:horizontal {{
            background: {theme_colors.get('accent_primary', '#0078d4')};
            border: 1px solid {theme_colors.get('accent_secondary', '#0066b8')};
            width: {slider_handle_size}px;
            height: {slider_handle_size}px;
            margin: -{(slider_handle_size - slider_height) // 2}px 0;
            border-radius: {slider_handle_radius}px;
        }}
        QSlider::handle:horizontal:hover {{
            background: {theme_colors.get('accent_secondary', '#0066b8')};
        }}
        """

        if hasattr(self, 'preview_slider_h'):
            self.preview_slider_h.setStyleSheet(slider_style)

        # Build checkbox stylesheet
        checkbox_style = f"""
        QCheckBox::indicator {{
            width: {checkbox_size}px;
            height: {checkbox_size}px;
            border: 2px solid {theme_colors.get('border', '#d0d0d0')};
            border-radius: {checkbox_radius}px;
            background-color: {theme_colors.get('bg_primary', '#ffffff')};
        }}
        QCheckBox::indicator:checked {{
            background-color: {theme_colors.get('accent_primary', '#0078d4')};
            border-color: {theme_colors.get('accent_primary', '#0078d4')};
            image: url(data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMTIiIGhlaWdodD0iMTIiIHZpZXdCb3g9IjAgMCAxMiAxMiIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj48cGF0aCBkPSJNMTAuNSAyLjVMNC41IDguNSAyIDYiIHN0cm9rZT0id2hpdGUiIHN0cm9rZS13aWR0aD0iMiIgZmlsbD0ibm9uZSIvPjwvc3ZnPg==);
        }}
        QCheckBox::indicator:hover {{
            border-color: {theme_colors.get('accent_primary', '#0078d4')};
        }}
        """

        if hasattr(self, 'preview_checkbox1'):
            self.preview_checkbox1.setStyleSheet(checkbox_style)
        if hasattr(self, 'preview_checkbox2'):
            self.preview_checkbox2.setStyleSheet(checkbox_style)

        # Build scrollbar stylesheet
        scrollbar_style = f"""
        QScrollBar:vertical {{
            width: {scrollbar_width}px;
            background-color: {theme_colors.get('scrollbar_background', '#f0f0f0')};
            border: 1px solid {theme_colors.get('scrollbar_border', '#d0d0d0')};
        }}
        QScrollBar::handle:vertical {{
            background-color: {theme_colors.get('scrollbar_handle', '#c0c0c0')};
            border-radius: {scrollbar_radius}px;
            min-height: 20px;
        }}
        QScrollBar::handle:vertical:hover {{
            background-color: {theme_colors.get('scrollbar_handle_hover', '#a0a0a0')};
        }}
        QScrollBar::handle:vertical:pressed {{
            background-color: {theme_colors.get('scrollbar_handle_pressed', '#909090')};
        }}
        QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
            height: 0px;
        }}
        """

        if hasattr(self, 'preview_text'):
            self.preview_text.setStyleSheet(scrollbar_style)

        # Build splitter stylesheet
        grip_dots = ""
        if show_grip:
            grip_dots = f"""
            QSplitter::handle:horizontal {{
                background: qlineargradient(x1:0, y1:0, x2:1, y2:0,
                    stop:0.4 {theme_colors.get('bg_secondary', '#f0f0f0')},
                    stop:0.5 {theme_colors.get('border', '#d0d0d0')},
                    stop:0.6 {theme_colors.get('bg_secondary', '#f0f0f0')});
            }}
            """
        else:
            grip_dots = ""

        splitter_style = f"""
        QSplitter::handle:horizontal {{
            background-color: {theme_colors.get('bg_secondary', '#f0f0f0')};
            border: 1px solid {theme_colors.get('border', '#d0d0d0')};
            width: {splitter_width}px;
            border-radius: {splitter_radius}px;
            margin: 1px;
        }}
        {grip_dots}
        QSplitter::handle:horizontal:hover {{
            background-color: {theme_colors.get('bg_tertiary', '#e0e0e0')};
        }}
        """

        if hasattr(self, 'preview_splitter'):
            self.preview_splitter.setStyleSheet(splitter_style)

        # Mark as modified
        self._gadget_modified = True


    def _reset_gadget_styles(self): #vers 3
        """Reset gadget styles to defaults and update preview"""
        self.button_shape_combo.setCurrentText("Rounded")
        self.button_radius_slider.setValue(4)
        self.button_height_spin.setValue(30)
        self.button_h_padding_spin.setValue(12)
        self.button_v_padding_spin.setValue(6)

        self.slider_height_spin.setValue(8)
        self.slider_handle_size.setValue(16)
        self.slider_handle_radius.setValue(8)

        self.checkbox_size_spin.setValue(18)
        self.checkbox_radius_slider.setValue(3)

        self.scrollbar_width_slider.setValue(12)
        self.scrollbar_handle_radius.setValue(3)

        self.splitter_width_slider.setValue(8)
        self.splitter_radius_slider.setValue(3)
        self.splitter_show_grip.setChecked(True)

        self.panel_opacity_slider.setValue(95)
        self.enable_animations.setChecked(True)

        # Update preview
        self._update_gadget_preview()


    def _collect_gadget_styles(self): #vers 2
        """Collect current gadget style settings"""
        return {
            "button_shape": self.button_shape_combo.currentText(),
            "button_border_radius": self.button_radius_slider.value(),
            "button_min_height": self.button_height_spin.value(),
            "button_padding_horizontal": self.button_h_padding_spin.value(),
            "button_padding_vertical": self.button_v_padding_spin.value(),

            "slider_height": self.slider_height_spin.value(),
            "slider_handle_size": self.slider_handle_size.value(),
            "slider_handle_radius": self.slider_handle_radius.value(),

            "checkbox_size": self.checkbox_size_spin.value(),
            "checkbox_border_radius": self.checkbox_radius_slider.value(),

            "scrollbar_width": self.scrollbar_width_slider.value(),
            "scrollbar_handle_radius": self.scrollbar_handle_radius.value(),

            "splitter_handle_width": self.splitter_width_slider.value(),
            "splitter_border_radius": self.splitter_radius_slider.value(),
            "splitter_show_grip": self.splitter_show_grip.isChecked(),

            "panel_opacity": self.panel_opacity_slider.value(),
            "enable_animations": self.enable_animations.isChecked()
        }


    def _browse_background_image(self, target): #vers 2
        """Browse for background image (panel, primary, button)"""
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            f"Select {target.capitalize()} Background Image",
            "",
            "Images (*.png *.jpg *.jpeg *.bmp);;All Files (*)"
        )

        if file_path:
            getattr(self, f"{target}_bg_path").setText(file_path)


    def _clear_background_image(self, target): #vers 2
        """Clear background image"""
        getattr(self, f"{target}_bg_path").clear()


    def _apply_gadget_styles_to_dialog(self, styles): #vers 1
        """Apply gadget styles to current dialog for preview"""
        # This would generate and apply stylesheet based on gadget settings
        # Implementation depends on how you want to apply these styles
        pass


    def _on_btn_style_changed(self, style_key): #vers 1
        """Store selected button style and refresh preview."""
        self.app_settings.current_settings["button_style"] = style_key
        self._update_btn_style_preview()


    def _apply_button_sizes(self): #vers 3
        """Apply titlebar icon size now and auto-save to JSON
        so other apps (COL, TXD, Model etc.) pick up the change next launch."""
        cs = self.app_settings.current_settings
        ico_sz = cs.get("titlebar_icon_size",      20)

        # Menu / Settings icons follow the setting, as in workshops
        from PyQt6.QtCore import QSize
        for attr in ("_dialog_menu_btn", "_dialog_settings_btn"):
            btn = getattr(self, attr, None)
            if btn:
                btn.setIconSize(QSize(ico_sz, ico_sz))

        # Auto-save so all other apps see the new sizes on next launch
        try:
            self.app_settings.save_settings()
        except Exception:
            pass


    def _update_btn_style_preview(self): #vers 1
        """Apply current button_style to the preview buttons."""
        if not hasattr(self, "_preview_btns"):
            return
        style_key = self.app_settings.current_settings.get("button_style", "flat")
        for btn, base_color in self._preview_btns:
            btn.setStyleSheet(self._build_btn_stylesheet(style_key, base_color))


    def _build_btn_stylesheet(self, style: str, base: str) -> str: #vers 1
        """Generate a QPushButton stylesheet for the given style + base colour."""
        from PyQt6.QtGui import QColor
        c = QColor(base)
        light = c.lighter(140).name()
        dark  = c.darker(130).name()
        mid   = c.name()
        shine = QColor(255, 255, 255, 80)

        if style == "flat":
            return f"QPushButton {{ background: {mid}; border: 1px solid {dark}; border-radius: 3px; }}"

        elif style == "gradient_h":
            return (f"QPushButton {{ background: qlineargradient(x1:0,y1:0,x2:1,y2:0,"
                    f"stop:0 {light}, stop:1 {dark}); border: 1px solid {dark}; border-radius:3px; }}")

        elif style == "gradient_v":
            return (f"QPushButton {{ background: qlineargradient(x1:0,y1:0,x2:0,y2:1,"
                    f"stop:0 {light}, stop:1 {dark}); border: 1px solid {dark}; border-radius:3px; }}")

        elif style == "gradient_45":
            return (f"QPushButton {{ background: qlineargradient(x1:0,y1:0,x2:1,y2:1,"
                    f"stop:0 {light}, stop:0.5 {mid}, stop:1 {dark}); "
                    f"border: 1px solid {dark}; border-radius:3px; }}")

        elif style == "banded":
            # Win ME style — highlight band at ~30%
            return (f"QPushButton {{ background: qlineargradient(x1:0,y1:0,x2:0,y2:1,"
                    f"stop:0 {light}, stop:0.25 white, stop:0.30 {light},"
                    f"stop:0.31 {mid}, stop:1 {dark}); "
                    f"border: 1px solid {dark}; border-radius:2px; }}")

        elif style == "zen":
            return (f"QPushButton {{ background: {mid}; border: 1px solid {dark}; "
                    f"border-radius: 6px; "
                    f"padding: 2px; }}"
                    f"QPushButton:hover {{ background: {light}; }}")

        elif style == "indented":
            return (f"QPushButton {{ background: {dark}; "
                    f"border-top: 2px solid {QColor(base).darker(160).name()}; "
                    f"border-left: 2px solid {QColor(base).darker(160).name()}; "
                    f"border-bottom: 1px solid {light}; "
                    f"border-right: 1px solid {light}; border-radius:2px; }}")

        elif style == "bump":
            return (f"QPushButton {{ background: qlineargradient(x1:0,y1:0,x2:0,y2:1,"
                    f"stop:0 {light}, stop:1 {dark}); "
                    f"border-top: 2px solid {light}; "
                    f"border-left: 2px solid {light}; "
                    f"border-bottom: 2px solid {dark}; "
                    f"border-right: 2px solid {dark}; border-radius:2px; }}")

        elif style == "amiga_wb":
            return (f"QPushButton {{ background: qlineargradient(x1:0,y1:0,x2:0,y2:1,"
                    f"stop:0 white, stop:0.15 {light}, stop:0.85 {dark}, stop:1 black); "
                    f"color: white; border: 1px solid black; border-radius:1px; }}")

        elif style == "half_shine":
            return (f"QPushButton {{ background: qlineargradient(x1:0,y1:0,x2:0,y2:1,"
                    f"stop:0 white, stop:0.48 {light}, stop:0.49 {mid}, stop:1 {mid}); "
                    f"border: 1px solid {dark}; border-radius:3px; }}")

        elif style == "shadow_dark":
            return (f"QPushButton {{ background: {mid}; "
                    f"border-top: 2px solid {light}; "
                    f"border-left: 2px solid {light}; "
                    f"border-bottom: 2px solid {QColor(base).darker(180).name()}; "
                    f"border-right: 2px solid {QColor(base).darker(180).name()}; "
                    f"border-radius:2px; }}")

        return f"QPushButton {{ background: {mid}; }}"


    def _get_default_button_colors(self): #vers 3
        """Get default button colors based on current theme (light/dark)"""
        # Detect light/dark from bg_primary colour luminance
        try:
            from PyQt6.QtGui import QColor
            tc = self.app_settings.get_theme_colors() or {}
            bg = tc.get("bg_primary", "#1a1a2e")
            is_light = QColor(bg).lightness() > 128
        except Exception:
            is_light = False

        if is_light:
            # Light theme - Pastel colors
            return {
                "open": "#E3F2FD",
                "close": "#FFF3E0",
                "close_all": "#FFF3E0",
                "rebuild": "#E8F5E8",
                "save_entry": "#E8F5E8",
                "rebuild_all": "#E8F5E8",
                "merge": "#F3E5F5",
                "split": "#F3E5F5",
                "convert": "#FFF8E1",
                "import": "#E1F5FE",
                "import_via": "#E1F5FE",
                "export": "#E0F2F1",
                "export_via": "#E0F2F1",
                "remove": "#FFEBEE",
                "remove_all": "#FFEBEE",
                "update": "#F9FBE7",
                "quick_export": "#E0F2F1",
                "pin": "#FCE4EC",
                "col_edit": "#FFE0B2",
                "txd_edit": "#E8EAF6",
                "dff_edit": "#F1F8E9",
                "ipf_edit": "#FFF3E0",
                "ipl_edit": "#FFEBEE",
                "ide_edit": "#E0F2F1",
                "dat_edit": "#F3E5F5",
                "zons_edit": "#E1F5FE",
                "weap_edit": "#FFF8E1",
                "vehi_edit": "#E8F5E8",
                "radar_map": "#FCE4EC",
                "paths_map": "#F9FBE7",
                "waterpro": "#E3F2FD"
            }
        else:
            # Dark theme - Solid darker colors
            return {
                "open": "#1a3a52",
                "close": "#4a3d2d",
                "close_all": "#4a3d2d",
                "rebuild": "#2d4a2d",
                "save_entry": "#2d4a2d",
                "rebuild_all": "#2d4a2d",
                "merge": "#3d2d4a",
                "split": "#3d2d4a",
                "convert": "#4a4a2d",
                "import": "#1a3a52",
                "import_via": "#1a3a52",
                "export": "#1a4a44",
                "export_via": "#1a4a44",
                "remove": "#4a2d2d",
                "remove_all": "#4a2d2d",
                "update": "#4a4a2d",
                "quick_export": "#1a4a44",
                "pin": "#4a2d3d",
                "col_edit": "#4a3d2d",
                "txd_edit": "#2d2d4a",
                "dff_edit": "#3a4a2d",
                "ipf_edit": "#4a4a2d",
                "ipl_edit": "#4a2d2d",
                "ide_edit": "#1a4a44",
                "dat_edit": "#3d2d4a",
                "zons_edit": "#1a3a52",
                "weap_edit": "#4a4a2d",
                "vehi_edit": "#2d4a2d",
                "radar_map": "#4a2d3d",
                "paths_map": "#4a4a2d",
                "waterpro": "#1a3a52"
            }


    def _on_button_color_changed(self, button_key, hex_color, preview_label): #vers 1
        """Handle button color changes"""
        # Update preview
        preview_label.setStyleSheet(f"background-color: {hex_color}; border: 1px solid #999;")

        # Store in settings
        if not hasattr(self, '_modified_button_colors'):
            self._modified_button_colors = {}
        self._modified_button_colors[button_key] = hex_color


    def _pick_button_color(self, color_input): #vers 1
        """Open color picker for button"""
        current_color = QColor(color_input.text())
        color = QColorDialog.getColor(current_color, self, "Pick Button Color")
        if color.isValid():
            color_input.setText(color.name())


    def _on_button_theme_type_changed(self, theme_type): #vers 2
        """No-op — theme type now derived from _is_dark_theme()."""
        pass


    def _toggle_instant_apply(self, enabled: bool):
        """Enhanced instant apply toggle"""
        if enabled:
            current_theme = self.demo_theme_combo.currentText()
            self._apply_demo_theme(current_theme)
            self.preview_status.setText("Instant apply: ON")
            self.demo_log.append("Instant apply enabled")
        else:
            self.preview_status.setText("Instant apply: OFF")
            self.demo_log.append("Instant apply disabled")


    def _change_preview_scope(self, scope: str):
        """Change preview scope"""
        self.demo_log.append(f"Preview scope: {scope}")
        current_theme = self.demo_theme_combo.currentText()
        self._apply_demo_theme(current_theme)


    def _update_theme_info(self):
        """Update theme information display"""
        current_theme = self.demo_theme_combo.currentText()
        if current_theme in self.app_settings.themes:
            theme_data = self.app_settings.themes[current_theme]

            info_text = f"""
            <b>{theme_data.get('name', current_theme)}</b><br>
            <i>{theme_data.get('description', 'No description available')}</i><br><br>

            <b>Colors:</b><br>
            • Primary: {theme_data['colors'].get('accent_primary', 'N/A')}<br>
            • Background: {theme_data['colors'].get('bg_primary', 'N/A')}<br>
            • Text: {theme_data['colors'].get('text_primary', 'N/A')}<br>

            <b>Category:</b> {theme_data.get('category', 'Standard')}<br>
            <b>Author:</b> {theme_data.get('author', 'Unknown')}
            """

            if hasattr(self, 'theme_info_label'):
                self.theme_info_label.setText(info_text)


    def _reset_button_colors_to_defaults(self): #vers 1
        """Reset all button colors to theme defaults"""
        reply = QMessageBox.question(
            self, "Reset Button Colors",
            "Reset all button colors to default values?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            default_colors = self._get_default_button_colors()

            for panel_id, editors in self.button_color_editors.items():
                for button_action, editor_dict in editors.items():
                    default_color = default_colors.get(button_action, "#E3F2FD")
                    editor_dict['input'].setText(default_color)

            QMessageBox.information(self, "Reset Complete", "Button colors reset to defaults")


    def _preview_button_colors(self): #vers 1
        """Preview button color changes"""
        QMessageBox.information(
            self, "Button Preview",
            "Button color preview will be applied when you click 'Apply'.\n\n"
            "Current button colors have been updated in the editors."
        )


    def _collect_current_button_colors(self): #vers 1
        """Collect current button panel colors for theme saving"""
        button_panels = {
            "img_files": [],
            "file_entries": [],
            "editing_options": []
        }

        # Check if we have button color editors
        if not hasattr(self, 'button_color_editors'):
            return button_panels

        # Determine if we're using light or dark colors
        is_light = getattr(self, 'button_theme_type_combo', None)
        is_light = is_light.currentText() == "Light Theme Buttons" if is_light else True

        # Get button definitions
        button_definitions = {
            "img_files": [
                ("Open", "open"), ("Close", "close"), ("Close All", "close_all"),
                ("Rebuild", "rebuild"), ("Save Entry...", "save_entry"),
                ("Rebuild All", "rebuild_all"), ("Merge", "merge"),
                ("Split", "split"), ("Convert", "convert")
            ],
            "file_entries": [
                ("Import", "import"), ("Import via", "import_via"),
                ("Export", "export"), ("Export via", "export_via"),
                ("Remove", "remove"), ("Remove All", "remove_all"),
                ("Refresh", "update"), ("Quick Export", "quick_export"),
                ("Pin selected", "pin")
            ],
            "editing_options": [
                ("Col Edit", "col_edit"), ("Txd Edit", "txd_edit"),
                ("Dff Edit", "dff_edit"), ("Ipf Edit", "ipf_edit"),
                ("IPL Edit", "ipl_edit"), ("IDE Edit", "ide_edit"),
                ("Dat Edit", "dat_edit"), ("Zons Edit", "zons_edit"),
                ("Weap Edit", "weap_edit"), ("Vehi Edit", "vehi_edit"),
                ("Radar Map", "radar_map"), ("Paths Map", "paths_map"),
                ("Waterpro", "waterpro")
            ]
        }

        # Collect colors from editors
        for panel_id, button_list in button_definitions.items():
            for button_text, button_action in button_list:
                # Get color from editor or settings
                color = "#E3F2FD"  # Default

                if panel_id in self.button_color_editors:
                    if button_action in self.button_color_editors[panel_id]:
                        editor = self.button_color_editors[panel_id][button_action]
                        color = editor['input'].text()

                # Add to button panel
                button_panels[panel_id].append({
                    "text": button_text,
                    "action": button_action,
                    "icon": f"edit-{button_action}",  # Icon placeholder
                    "color": color
                })

        return button_panels


    def _update_preview_stats(self):
        """Update preview statistics"""
        if hasattr(self, 'stats_labels'):
            current_count = int(self.stats_labels["Preview Changes:"].text()) + 1
            self.stats_labels["Preview Changes:"].setText(str(current_count))
            self.stats_labels["Last Applied:"].setText(self.demo_theme_combo.currentText())


    def _show_full_preview(self):
        """Show full preview window"""
        QMessageBox.information(self, "Full Preview",
            "Full preview window would open here!\n\n"
            "This would show a complete App Factory interface\n"
            "with the selected theme applied.")


    def _preview_theme_instantly(self, theme_name: str):
        """Enhanced instant preview with better feedback"""
        if hasattr(self, 'auto_preview_check') and self.auto_preview_check.isChecked():
            self._apply_demo_theme(theme_name)
            self._update_theme_info()
            self._update_preview_stats()

            # Update status
            self.preview_status.setText(f"Previewing: {theme_name}")
            self.demo_log.append(f"Theme preview: {theme_name}")


    def _apply_demo_theme(self, theme_name: str): #vers 1
        """Apply theme to demo elements"""
        if theme_name not in self.app_settings.themes:
            return

        # Temporarily update settings for preview
        self.app_settings.current_settings["theme"] = theme_name

        # Apply theme stylesheet
        stylesheet = self.app_settings.get_stylesheet()

        # Apply to demo widgets if instant apply enabled
        if hasattr(self, 'instant_apply_check') and self.instant_apply_check.isChecked():
            self.setStyleSheet(stylesheet)
            self.themeChanged.emit(theme_name)

        if hasattr(self, 'demo_log'):
            self.demo_log.append(f"Previewing: {theme_name}")


    def _reset_demo_theme(self): #vers 1
        """Reset to original theme"""
        original = getattr(self, '_original_theme', self.app_settings.current_settings["theme"])
        if hasattr(self, 'demo_theme_combo'):
            self.demo_theme_combo.setCurrentText(original)
        self._apply_demo_theme(original)


    def _on_font_changed(self, font_id, property_type, value): #vers 1
        """Handle font property changes - updates current_settings"""
        if property_type == 'family':
            self.app_settings.current_settings[f'{font_id}_font_family'] = value
        elif property_type == 'size':
            self.app_settings.current_settings[f'{font_id}_font_size'] = value
        elif property_type == 'weight':
            self.app_settings.current_settings[f'{font_id}_font_weight'] = value

        # Mark as modified
        if not hasattr(self, '_fonts_modified'):
            self._fonts_modified = True


    def _preview_font_changes(self): #vers 1
        """Preview font changes in dialog"""
        try:
            # Apply default font to see immediate changes
            if 'default' in self.font_controls:
                family = self.font_controls['default']['family'].currentFont().family()
                size = self.font_controls['default']['size'].value()
                weight = self.font_controls['default']['weight'].currentText()

                font = QFont(family, size)
                if weight == "Bold":
                    font.setWeight(QFont.Weight.Bold)
                elif weight == "Light":
                    font.setWeight(QFont.Weight.Light)

                self.setFont(font)

            # Show preview info
            preview_text = "Font Preview Applied!\n\n"
            for font_id, controls in self.font_controls.items():
                family = controls['family'].currentFont().family()
                size = controls['size'].value()
                weight = controls['weight'].currentText()
                preview_text += f"{controls['group'].title()}: {family} {size}pt {weight}\n"

            QMessageBox.information(
                self,
                "Font Preview",
                preview_text + "\nClick 'Apply' to save changes to appfactory.settings.json"
            )
        except Exception as e:
            QMessageBox.warning(self, "Preview Error", f"Could not preview fonts:\n{e}")


    def _on_size_changed(self, key, value): #vers 2
        """Store a size rule and preview it in this dialog."""
        from apps.methods.ui_sizes import DEFAULTS, refresh_all, set_overrides
        sizes = dict(self.app_settings.current_settings.get('ui_sizes', {}))
        if value == DEFAULTS[key]:
            sizes.pop(key, None)
        else:
            sizes[key] = value
        self.app_settings.current_settings['ui_sizes'] = sizes
        set_overrides(sizes)
        self.setStyleSheet(self.app_settings.get_stylesheet())
        refresh_all()                                   # open workshops follow live

    def _reset_all_fonts(self): #vers 1
        """Reset all fonts to their default values"""
        reply = QMessageBox.question(
            self,
            "Reset Fonts",
            "Reset all fonts to their default values?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            for font_id, controls in self.font_controls.items():
                # Reset to defaults
                default_family = controls['default_family']
                default_size = controls['default_size']

                controls['family'].setCurrentFont(QFont(default_family))
                controls['size'].setValue(default_size)
                controls['weight'].setCurrentText('Normal')

                # Update settings
                self.app_settings.current_settings[f'{font_id}_font_family'] = default_family
                self.app_settings.current_settings[f'{font_id}_font_size'] = default_size
                self.app_settings.current_settings[f'{font_id}_font_weight'] = 'Normal'

            QMessageBox.information(self, "Fonts Reset", "All fonts reset to default values")


    def _load_font_settings(self): #vers 1
        """Load font settings from appfactory.settings.json into controls"""
        font_ids = ['default', 'title', 'panel', 'button', 'menu', 'infobar', 'table', 'tooltip']

        for font_id in font_ids:
            if font_id not in self.font_controls:
                continue

            controls = self.font_controls[font_id]

            # Load family
            family = self.app_settings.current_settings.get(
                f'{font_id}_font_family',
                controls['default_family']
            )
            controls['family'].setCurrentFont(QFont(family))

            # Load size
            size = self.app_settings.current_settings.get(
                f'{font_id}_font_size',
                controls['default_size']
            )
            controls['size'].setValue(size)

            # Load weight
            weight = self.app_settings.current_settings.get(
                f'{font_id}_font_weight',
                'Normal'
            )
            controls['weight'].setCurrentText(weight)


    def _save_font_settings(self): #vers 1
        """Save font settings from controls to app settings and appfactory.settings.json"""
        for font_id, controls in self.font_controls.items():
            family = controls['family'].currentFont().family()
            size = controls['size'].value()
            weight = controls['weight'].currentText()

            self.app_settings.current_settings[f'{font_id}_font_family'] = family
            self.app_settings.current_settings[f'{font_id}_font_size'] = size
            self.app_settings.current_settings[f'{font_id}_font_weight'] = weight

        # Save to appfactory.settings.json
        self.app_settings.save_settings()


    def _refresh_panel_previews(self): #vers 2
        """Refresh all PanelPreviewWidget instances in the dialog."""
        for attr in ("_fill_preview", "_hero_preview", "_grad_preview",
                     "_pat_preview", "_copper_preview", "_img_preview",
                     "_trans_preview", "_effect_preview", "_hero_side_preview"):
            pw = getattr(self, attr, None)
            if pw:
                pw.refresh()


    def _show_effect_preview_popup(self, mode: str): #vers 1
        """Show a larger popup preview of the given panel effect mode."""
        from apps.utils.app_settings_system import PanelPreviewWidget
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel as _QL
        d = QDialog(self)
        d.setWindowTitle(f"Preview — {mode.title()}")
        d.resize(400, 200)
        lay = QVBoxLayout(d)
        pw = PanelPreviewWidget(self, mode)
        pw.setMinimumHeight(160)
        lay.addWidget(pw)
        note = _QL("Close window to dismiss")
        note.setStyleSheet("color: palette(placeholderText); font-size: 8pt;")
        lay.addWidget(note)
        d.exec()


    def _pick_panel_colour(self, key: str, btn: "QPushButton"): #vers 2
        """Open colour dialog, store result, refresh all panel previews."""
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        current = self.app_settings.current_settings.get(key, "#1a1a2e")
        colour = QColorDialog.getColor(QColor(current), self, f"Pick colour — {key}")
        if colour.isValid():
            hex_val = colour.name()
            self.app_settings.current_settings[key] = hex_val
            btn.setStyleSheet(f"background:{hex_val};")
            # Refresh all panel previews
            for attr in ("_fill_preview", "_hero_preview", "_grad_preview",
                         "_pat_preview", "_copper_preview", "_img_preview",
                         "_trans_preview"):
                pw = getattr(self, attr, None)
                if pw:
                    pw.refresh()


    def _on_color_alpha_changed(self, key, value): #vers 1
        """Store a colour's transparency; 100 removes it."""
        alpha = dict(self.app_settings.current_settings.get("color_alpha", {}))
        if value >= 100:
            alpha.pop(key, None)
        else:
            alpha[key] = int(value)
        self.app_settings.current_settings["color_alpha"] = alpha

    def _on_color_alpha_toggled(self, key, on): #vers 1
        """Transparency on / off for a colour; the value is kept."""
        off = set(self.app_settings.current_settings.get("color_alpha_off", []))
        (off.discard if on else off.add)(key)
        self.app_settings.current_settings["color_alpha_off"] = sorted(off)


    def _load_panel_controls(self): #vers 7
        """Show current panel image, transparency and colour alpha settings."""
        from apps.utils.app_settings_system import ALPHA_COLOR_KEYS
        cs = self.app_settings.current_settings
        for key, ed in getattr(self, 'color_editors', {}).items():
            if key in ALPHA_COLOR_KEYS:
                ed.set_alpha(cs.get("color_alpha", {}).get(key, 100))
                ed.set_alpha_on(key not in cs.get("color_alpha_off", []))
        if hasattr(self, '_panel_img_path'):
            self._panel_img_path.setText(cs.get("panel_bg_image", ""))
            self._panel_img_mode.setCurrentIndex(int(cs.get("panel_bg_image_mode", 5)))
            self._panel_img_opacity.setValue(int(cs.get("panel_bg_image_opacity", 100)))
            self._panel_img_all.setChecked(bool(cs.get("panel_bg_image_all", False)))
            self._panel_img_titlebar.setCurrentIndex(int(cs.get("image_titlebar", 0)))
        for key in ("titlebar_opacity", "panel_opacity", "button_opacity", "widget_opacity",
                    "tabs_opacity", "splitter_opacity"):
            sl = getattr(self, f"_{key}_slider", None)
            if sl is not None:
                sl.setValue(int(cs.get(key, 100)))
        if hasattr(self, '_see_through_chk'):
            self._see_through_chk.setChecked(bool(cs.get("see_through", False)))
            self._viewport_solid_chk.setChecked(bool(cs.get("viewport_solid", True)))


    def _browse_panel_bg_image(self): #vers 1
        """Browse for panel background image."""
        from PyQt6.QtWidgets import QFileDialog
        path, _ = QFileDialog.getOpenFileName(
            self, "Select Background Image", "",
            "Images (*.png *.jpg *.jpeg *.bmp *.webp *.gif)")
        if path:
            self._panel_img_path.setText(path)
            self.app_settings.current_settings["panel_bg_image"] = path


    def _select_color_for_gadget(self, gadget_type): #vers 1
        """Open color dialog to select color for specific gadget type"""
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        
        current_color = QColor(255, 255, 255)  # Default white
        color = QColorDialog.getColor(current_color, self, f"Select {gadget_type.replace('_', ' ').title()} Color")
        
        if color.isValid():
            color_name = color.name()
            # Update the button text to show selected color
            button_map = {
                "string_bg": self.string_bg_color,
                "string_text": self.string_text_color,
                "string_border": self.string_border_color,
                "gauge_bg": self.gauge_bg_color,
                "gauge_fill": self.gauge_fill_color,
                "gauge_border": self.gauge_border_color,
                "scale_bg": self.scale_bg_color,
                "scale_handle": self.scale_handle_color,
                "colorfield_bg": self.colorfield_bg_color,
                "colorfield_border": self.colorfield_border_color,
                "list_bg": self.list_bg_color,
                "list_text": self.list_text_color,
                "list_select": self.list_select_color,
                "numeric_bg": self.numeric_bg_color,
                "numeric_text": self.numeric_text_color,
                "knob_bg": self.knob_bg_color,
                "knob_handle": self.knob_handle_color,
                "levelmeter_bg": self.levelmeter_bg_color,
                "levelmeter_fill": self.levelmeter_fill_color,
                "radio_bg": self.radio_bg_color,
                "radio_text": self.radio_text_color,
                "cycle_bg": self.cycle_bg_color,
                "cycle_text": self.cycle_text_color,
                "palette_bg": self.palette_bg_color,
                "palette_border": self.palette_border_color,
                "popstring_bg": self.popstring_bg_color,
                "popstring_text": self.popstring_text_color,
            }
            
            if gadget_type in button_map:
                button_map[gadget_type].setText(f"Selected: {color_name}")
            
            # Store the color for later use
            setattr(self, f"{gadget_type}_color", color_name)
            print(f"{gadget_type} color selected: {color_name}")


    def _select_shadow_color(self): #vers 1
        """Open color dialog to select shadow color"""
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        
        current_color = QColor(0, 0, 0, 128)  # Default: black with 50% opacity
        color = QColorDialog.getColor(current_color, self, "Select Shadow Color")
        
        if color.isValid():
            # For now, just store the color for later use
            self.shadow_color = color.name()
            self.shadow_color_button.setText(f"Selected: {color.name()}")
            print(f"Shadow color selected: {color.name()}")


    def _select_color_for_component(self, component_type): #vers 2
        """Open color dialog to select color for specific UI component type"""
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        
        current_color = QColor(255, 255, 255)  # Default white
        color = QColorDialog.getColor(current_color, self, f"Select {component_type.replace('_', ' ').title()} Color")
        
        if color.isValid():
            color_name = color.name()
            # Update the button text to show selected color
            button_map = {
                "group_bg": self.group_bg_color,
                "group_border": self.group_border_color,
                "scrollbar_bg": self.scrollbar_bg_color,
                "scrollbar_handle": self.scrollbar_handle_color,
                "listview_bg": self.listview_bg_color,
                "listview_text": self.listview_text_color,
                "listview_select": self.listview_select_color,
                "register_bg": self.register_bg_color,
                "register_text": self.register_text_color,
                "virtgroup_bg": self.virtgroup_bg_color,
                "virtgroup_border": self.virtgroup_border_color,
                "scrollgroup_bg": self.scrollgroup_bg_color,
                "scrollgroup_border": self.scrollgroup_border_color,
                "popobject_bg": self.popobject_bg_color,
                "popobject_border": self.popobject_border_color,
            }
            
            if component_type in button_map:
                button_map[component_type].setStyleSheet(f"background-color: {color_name};")
                button_map[component_type].setToolTip(color_name)

            # Store the color for later use (the _color attribute is the swatch button)
            setattr(self, f"{component_type}_color_value", color_name)
            print(f"{component_type} color selected: {color_name}")


    def _on_global_hue_changed(self, value): #vers 2
        """Handle global hue slider change - applies to ALL colors"""
        self.global_hue_value.setText(str(value))
        hue_shift = value
        sat_shift = self.global_sat_slider.value()
        bri_shift = self.global_bri_slider.value()
        self.apply_global_hsb_to_all_colors(hue_shift, sat_shift, bri_shift)


    def _on_global_sat_changed(self, value): #vers 2
        """Handle global saturation slider change - applies to ALL colors"""
        self.global_sat_value.setText(str(value))
        hue_shift = self.global_hue_slider.value()
        sat_shift = value
        bri_shift = self.global_bri_slider.value()
        self.apply_global_hsb_to_all_colors(hue_shift, sat_shift, bri_shift)


    def _on_global_bri_changed(self, value): #vers 2
        """Handle global brightness slider change - applies to ALL colors"""
        self.global_bri_value.setText(str(value))
        hue_shift = self.global_hue_slider.value()
        sat_shift = self.global_sat_slider.value()
        bri_shift = value
        self.apply_global_hsb_to_all_colors(hue_shift, sat_shift, bri_shift)


    def apply_global_hsb_to_all_colors(self, hue_shift, sat_shift, bri_shift): #vers 3
        """Apply HSB adjustments to ALL theme colors globally - respects locks"""
        from apps.utils.app_settings_system import hsl_to_rgb, rgb_to_hsl
        if not hasattr(self, 'color_editors'):
            return

        if not hasattr(self, 'theme_selector_combo'):
            return

        current_theme = self.theme_selector_combo.currentData()
        if not current_theme:
            return

        original_colors = self.app_settings.get_theme_colors(current_theme)

        # Apply adjustments to each color
        for color_key, original_hex in original_colors.items():
            if color_key not in self.color_editors:
                continue

            editor = self.color_editors[color_key]

            # SKIP if locked
            if editor.is_locked:
                continue

            # Convert to HSL
            h, s, l = rgb_to_hsl(original_hex)

            # Apply shifts
            h = (h + hue_shift) % 360
            s = max(0, min(100, s + sat_shift))
            l = max(0, min(100, l + bri_shift))

            # Convert back to hex
            new_hex = hsl_to_rgb(h, s, l)

            # Update the color editor
            editor.update_color(new_hex)

            # Store modified color
            if not hasattr(self, '_modified_colors'):
                self._modified_colors = {}
            self._modified_colors[color_key] = new_hex


    def _reset_global_sliders(self): #vers 1
        """Reset global HSB sliders to default (0)"""
        self.global_hue_slider.setValue(0)
        self.global_sat_slider.setValue(0)
        self.global_bri_slider.setValue(0)

        current_theme = self.theme_selector_combo.currentData()
        if current_theme:
            original_colors = self.app_settings.get_theme_colors(current_theme)
            self._modified_colors = original_colors.copy()

            for color_key, editor in self.color_editors.items():
                if color_key in original_colors:
                    editor.update_color(original_colors[color_key])


    def _lock_all_colors(self): #vers 1
        """Lock all color editors"""
        if hasattr(self, 'color_editors'):
            for editor in self.color_editors.values():
                editor.set_locked(True)


    def _unlock_all_colors(self): #vers 1
        """Unlock all color editors"""
        if hasattr(self, 'color_editors'):
            for editor in self.color_editors.values():
                editor.set_locked(False)


    def _on_theme_changed(self, theme_name): #vers 5
        """Theme picked: colour editors now, restyle once the selection settles (150 ms)."""
        theme_key = None
        for key, data in self.app_settings.themes.items():
            if data.get("name", key) == theme_name:
                theme_key = key
                break
        if not theme_key:
            return
        self._load_theme_colors(theme_key)
        self._pending_theme = theme_key
        timer = getattr(self, '_theme_timer', None)
        if timer is None:
            from PyQt6.QtCore import QTimer
            timer = self._theme_timer = QTimer(self)
            timer.setSingleShot(True)
            timer.timeout.connect(self._apply_theme_change)
        timer.start(150)                                 # scrolling the list: only the last one restyles

    def _apply_theme_change(self): #vers 1
        """One restyle: whole app when live apply is on, else this dialog only."""
        from PyQt6.QtWidgets import QApplication
        theme_key = self._pending_theme
        self.app_settings.apply_theme_effects(theme_key)
        self._load_panel_controls()
        live = hasattr(self, 'instant_apply_check') and self.instant_apply_check.isChecked()
        if live:
            self.app_settings.current_settings["theme"] = theme_key
        stylesheet = self.app_settings.get_stylesheet()
        if live:
            self.setStyleSheet("")                       # inherits the app stylesheet
            QApplication.instance().setStyleSheet(stylesheet)
        else:
            self.setStyleSheet(stylesheet)               # preview in this dialog only
            return
        # Refresh IMG Factory-specific widgets (taskbar, table, icons)
        mw = self.parent()
        if mw is None:                                   # non-modal: find the main window
            for w in QApplication.topLevelWidgets():
                if hasattr(w, 'app_settings') and hasattr(w, 'gui_layout'):
                    mw = w
                    break
        if mw and hasattr(mw, 'app_settings'):
            colors = mw.app_settings.get_theme_colors() or {}
            icon_color = colors.get('text_primary', '#cccccc')
            if hasattr(mw, 'gui_layout'):
                if hasattr(mw.gui_layout, 'refresh_icons'):
                    mw.gui_layout.refresh_icons(icon_color)
                if hasattr(mw.gui_layout, 'apply_table_theme'):
                    mw.gui_layout.apply_table_theme()
            if hasattr(mw, 'tool_taskbar'):
                mw.tool_taskbar.apply_theme(colors)
        self.themeChanged.emit(theme_key)


    def _load_theme_colors(self, theme_key): #vers 3
        """Load colors for selected theme into editors — clears undo history."""
        if hasattr(self, "_color_history"): self._color_history.clear()
        _ub = getattr(self, "_dialog_undo_btn", None)
        if _ub: _ub.setEnabled(False); _ub.setToolTip("Nothing to undo")
        if theme_key in self.app_settings.themes:
            colors = self.app_settings.themes[theme_key].get("colors", {})

            for color_key, editor in self.color_editors.items():
                color_value = colors.get(color_key, "#ffffff")
                editor.set_color(color_value)


    def _on_theme_color_changed(self, color_key, hex_value): #vers 2
        """Handle individual color changes — push undo snapshot first."""
        if not hasattr(self, '_modified_colors'):
            self._modified_colors = {}

        # Push current state to undo history before applying change
        old_val = self._modified_colors.get(color_key)
        if old_val is None:
            # First change for this key — get current editor value
            editor = self.color_editors.get(color_key)
            old_val = editor.color_input.text() if editor else '#ffffff'
        if old_val != hex_value:
            self._push_undo(color_key, old_val)

        self._modified_colors[color_key] = hex_value


    def _apply_picked_color(self): #vers 1
        """Apply picked color to selected element"""
        picked_color = self.color_picker.current_color
        selected_data = self.selected_element_combo.currentData()

        if selected_data and picked_color:
            if selected_data in self.color_editors:
                self.color_editors[selected_data].set_color(picked_color)

            if hasattr(self, 'demo_log'):
                element_name = self.selected_element_combo.currentText()
                self.demo_log.append(f"Applied {picked_color} to {element_name}")


    def _refresh_themes(self): #vers 1
        """Refresh themes from disk"""
        current_theme = self.theme_selector_combo.currentData()
        self.app_settings.refresh_themes()

        self.theme_selector_combo.clear()
        for theme_key, theme_data in self.app_settings.themes.items():
            display_name = theme_data.get("name", theme_key)
            self.theme_selector_combo.addItem(display_name, theme_key)

        index = self.theme_selector_combo.findData(current_theme)
        if index >= 0:
            self.theme_selector_combo.setCurrentIndex(index)


    def refresh_themes_in_dialog(self): #vers 1
        """Refresh themes in settings dialog"""
        if hasattr(self, 'demo_theme_combo'):
            current_theme = self.demo_theme_combo.currentText()
            self.app_settings.refresh_themes()

            self.demo_theme_combo.clear()
            for theme_key in self.app_settings.themes.keys():
                self.demo_theme_combo.addItem(theme_key)

            index = self.demo_theme_combo.findText(current_theme)
            if index >= 0:
                self.demo_theme_combo.setCurrentIndex(index)

            if hasattr(self, 'demo_log'):
                self.demo_log.append(f"Refreshed: {len(self.app_settings.themes)} themes")


    def _load_current_settings(self): #vers 6
        """Load current settings into UI"""
        # Set theme
        if hasattr(self, 'theme_selector_combo'):
            current_theme = self.app_settings.current_settings.get("theme", "App_Factory")
            index = self.theme_selector_combo.findData(current_theme)
            if index >= 0:
                self.theme_selector_combo.setCurrentIndex(index)

       # Backward compatibility: migrate old font_family/font_size to default_font_*
        if 'font_family' in self.app_settings.current_settings and 'default' in self.font_controls:
            old_family = self.app_settings.current_settings.get('font_family', 'Segoe UI')
            old_size = self.app_settings.current_settings.get('font_size', 9)

            if 'default_font_family' not in self.app_settings.current_settings:
                self.app_settings.current_settings['default_font_family'] = old_family
                self.app_settings.current_settings['default_font_size'] = old_size
                self.font_controls['default']['family'].setCurrentFont(QFont(old_family))
                self.font_controls['default']['size'].setValue(old_size)

        # Load font settings
        if hasattr(self, 'font_controls'):
            self._load_font_settings()

        # Set interface settings
        if hasattr(self, 'font_family_combo'):
            self.font_family_combo.setCurrentText(
                self.app_settings.current_settings.get("font_family", "Segoe UI")
            )
        if hasattr(self, 'font_size_spin'):
            self.font_size_spin.setValue(
                self.app_settings.current_settings.get("font_size", 9)
            )
        if hasattr(self, 'tooltips_check'):
            self.tooltips_check.setChecked(
                self.app_settings.current_settings.get("show_tooltips", True)
            )
        if hasattr(self, 'menu_icons_check'):
            self.menu_icons_check.setChecked(
                self.app_settings.current_settings.get("show_menu_icons", True)
            )
        if hasattr(self, 'button_icons_check'):
            self.button_icons_check.setChecked(
                self.app_settings.current_settings.get("show_button_icons", False)
            )

            # Button display mode
        if hasattr(self, 'button_display_combo'):
            current_mode = self.app_settings.current_settings.get("button_display_mode", "both")
            mode_map = {"both": 0, "icons": 1, "text": 2, "auto": 3}
            self.button_display_combo.setCurrentIndex(mode_map.get(current_mode, 0))

        # Window controls
        if hasattr(self, 'custom_gadgets_check'):
            self.custom_gadgets_check.setChecked(
                self.app_settings.current_settings.get("use_custom_gadgets", False)
            )
        if hasattr(self, 'corner_resize_check'):
            self.corner_resize_check.setChecked(
                self.app_settings.current_settings.get("enable_corner_resize", True)
            )

        # Interface settings
        if hasattr(self, 'tooltips_check'):
            self.tooltips_check.setChecked(
                self.app_settings.current_settings.get("show_tooltips", True)
            )
        if hasattr(self, 'menu_icons_check'):
            self.menu_icons_check.setChecked(
                self.app_settings.current_settings.get("show_menu_icons", True)
            )
        if hasattr(self, 'use_svg_icons_check'):
            self.use_svg_icons_check.setChecked(
                self.app_settings.current_settings.get("use_svg_icons", True)
            )


    def _get_dialog_settings(self): #vers 3
        """Collect all settings from dialog controls"""
        settings = {}

        # Get theme from demo tab if available
        if hasattr(self, 'demo_theme_combo'):
            settings["theme"] = self.demo_theme_combo.currentText()
        elif hasattr(self, 'theme_selector_combo'):
            theme_data = self.theme_selector_combo.currentData()
            if theme_data:
                settings["theme"] = theme_data
        else:
            settings["theme"] = self.app_settings.current_settings["theme"]

        # Font settings
        if hasattr(self, 'font_family_combo'):
            settings["font_family"] = self.font_family_combo.currentText()
        if hasattr(self, 'font_size_spin'):
            settings["font_size"] = self.font_size_spin.value()

        # Interface settings
        if hasattr(self, 'tooltips_check'):
            settings["show_tooltips"] = self.tooltips_check.isChecked()
        if hasattr(self, 'menu_icons_check'):
            settings["show_menu_icons"] = self.menu_icons_check.isChecked()
        if hasattr(self, 'button_icons_check'):
            settings["show_button_icons"] = self.button_icons_check.isChecked()

        # Button display mode
        if hasattr(self, 'button_display_combo'):
            mode_index = self.button_display_combo.currentIndex()
            mode_map = {0: "both", 1: "icons", 2: "text", 3: "auto"}
            settings["button_display_mode"] = mode_map.get(mode_index, "both")

        # Tab content mode
        if hasattr(self, 'tab_content_mode_combo') and self.tab_content_mode_combo:
            tab_map = {0: "both", 1: "icon", 2: "text"}
            settings["tab_content_mode"] = tab_map.get(
                self.tab_content_mode_combo.currentIndex(), "both")

        # Window controls
        if hasattr(self, 'custom_gadgets_check'):
            settings["use_custom_gadgets"] = self.custom_gadgets_check.isChecked()
        if hasattr(self, 'corner_resize_check'):
            settings["enable_corner_resize"] = self.corner_resize_check.isChecked()

        # Interface settings
        if hasattr(self, 'tooltips_check'):
            settings["show_tooltips"] = self.tooltips_check.isChecked()
        if hasattr(self, 'menu_icons_check'):
            settings["show_menu_icons"] = self.menu_icons_check.isChecked()
        if hasattr(self, 'use_svg_icons_check'):
            settings["use_svg_icons"] = self.use_svg_icons_check.isChecked()

        # Button & titlebar sizes
        for attr, key in (
            ("_tb_height_spin",   "titlebar_button_height"),
            ("_tb_btn_size_spin", "titlebar_button_size"),
            ("_tb_icon_spin",     "titlebar_icon_size"),
            ("_btn_height_spin",  "button_size"),
        ):
            spin = getattr(self, attr, None)
            if spin:
                settings[key] = spin.value()

        # Panel effect + button style
        if hasattr(self, "_panel_effect_combo") and self._panel_effect_combo:
            settings["panel_effect_type"] =                 ["none","fill","gradient","pattern"][self._panel_effect_combo.currentIndex()]
        if hasattr(self, "_btn_style_combo"):
            settings["button_style"] = self._btn_style_combo.currentData()
        if hasattr(self, "_tint_enabled_cb") and self._tint_enabled_cb:
            settings["use_pastel_buttons"] = self._tint_enabled_cb.isChecked()

        # Debug settings
        if hasattr(self, 'pin_warn_popup_check'):
            settings["pin_warn_popup"] = self.pin_warn_popup_check.isChecked()
        if hasattr(self, 'pin_warn_log_check'):
            settings["pin_warn_log"] = self.pin_warn_log_check.isChecked()
        if hasattr(self, 'rename_notify_popup_check'):
            settings["rename_notify_popup"] = self.rename_notify_popup_check.isChecked()
        if hasattr(self, 'rename_notify_log_check'):
            settings["rename_notify_log"] = self.rename_notify_log_check.isChecked()
        if hasattr(self, 'debug_enabled_check'):
            settings["debug_mode"] = self.debug_enabled_check.isChecked()
        if hasattr(self, 'debug_level_combo'):
            settings["debug_level"] = self.debug_level_combo.currentText()
        if hasattr(self, 'debug_categories'):
            enabled_categories = []
            for category, checkbox in self.debug_categories.items():
                if checkbox.isChecked():
                    enabled_categories.append(category)
            settings["debug_categories"] = enabled_categories

        return settings


    def _collect_settings_contributions(self): #vers 1
        """Find any docked tool currently open as a tab in the main
        window that exposes get_settings_contribution()"""
        contributions = []
        mw = getattr(self, 'main_window', None)
        tab_widget = getattr(mw, 'main_tab_widget', None)
        if tab_widget is None:
            return contributions
        seen_ids = set()
        for i in range(tab_widget.count()):
            page = tab_widget.widget(i)
            if page is None:
                continue
            candidates = [page] + page.findChildren(QWidget)
            for w in candidates:
                if id(w) in seen_ids:
                    continue
                get_contrib = getattr(w, 'get_settings_contribution', None)
                if get_contrib is None or not callable(get_contrib):
                    continue
                seen_ids.add(id(w))
                try:
                    extra_tabs, apply_fn = get_contrib()
                except Exception as e:
                    print(f"[Settings] get_settings_contribution failed for {w}: {e}")
                    continue
                contributions.extend(extra_tabs)
                if callable(apply_fn):
                    self._extra_apply_callbacks.append(apply_fn)
        return contributions


    def _apply_settings(self): #vers 8
        """Apply settings permanently and save to appfactory.settings.json AND theme files"""
        from apps.utils.app_settings_system import apply_panel_effects
        new_settings = self._get_dialog_settings()
        old_theme = self.app_settings.current_settings["theme"]
        old_custom_gadgets = self.app_settings.current_settings.get("use_custom_gadgets", False)

        # Update settings
        self.app_settings.current_settings.update(new_settings)

        # Save font settings if modified
        if hasattr(self, 'font_controls'):
            self._save_font_settings()

        # Save all modified colors, button colors and settings to theme JSON
        modified = {}
        if hasattr(self, '_modified_colors') and self._modified_colors:
            modified.update(self._modified_colors)
        if hasattr(self, '_modified_button_colors') and self._modified_button_colors:
            modified.update(self._modified_button_colors)
        if hasattr(self, 'handle_style_combo'):
            modified['handle_style'] = self.handle_style_combo.currentText()
        if modified:
            current_theme = self.app_settings.current_settings["theme"]
            if current_theme in self.app_settings.themes:
                if "colors" not in self.app_settings.themes[current_theme]:
                    self.app_settings.themes[current_theme]["colors"] = {}
                self.app_settings.themes[current_theme]["colors"].update(modified)

                theme_data = self.app_settings.themes[current_theme]
                success = self.app_settings.save_theme(current_theme, theme_data)

                if success:
                    print(f"Saved {len(modified)} color changes to theme: {current_theme}")
                else:
                    print(f"Failed to save theme: {current_theme}")
                    QMessageBox.warning(
                        self,
                        "Theme Save Warning",
                        f"Settings applied but could not save theme file.\n"
                        f"Changes may be lost on restart.\n\n"
                        f"Check file permissions for themes/{current_theme}.json"
                    )

        # Save settings to appfactory.settings.json
        self.app_settings.save_settings()

        # Apply theme and update icons
        stylesheet = self.app_settings.get_stylesheet()
        self.setStyleSheet(stylesheet)

        # Emit signals
        if new_settings["theme"] != old_theme:
            self.themeChanged.emit(new_settings["theme"])

        # Check if custom gadgets setting changed
        new_custom_gadgets = new_settings.get("use_custom_gadgets", False)
        if old_custom_gadgets != new_custom_gadgets:
            QMessageBox.information(
                self,
                "Restart Required",
                "Window gadget changes will take effect after restarting the application."
            )

        # Re-apply stylesheet to dialog so colour changes show immediately
        try:
            self.setStyleSheet(self.app_settings.get_stylesheet())
        except Exception:
            pass

        self.settingsChanged.emit()
        owner = self.parent()                            # IMG Factory: button display mode now
        if hasattr(owner, '_apply_button_display_mode_from_settings'):
            owner._apply_button_display_mode_from_settings()

        # Apply panel effects and button/progressbar styles to main window,
        # then every other open top-level window (standalone / torn-off workshops)
        try:
            mw = getattr(self, 'main_window', None) or self.parent()
            if mw and hasattr(mw, 'setStyleSheet'):
                mw.setStyleSheet(self.app_settings.get_stylesheet())   # before effects: they append rules
                apply_panel_effects(mw, self.app_settings)
            from PyQt6.QtWidgets import QApplication, QDialog
            for top in QApplication.topLevelWidgets():
                if (top is not mw and top.isVisible() and not isinstance(top, QDialog)
                        and top.parent() is None and top.findChildren(QWidget)):
                    apply_panel_effects(top, self.app_settings)
            # Always re-apply to the dialog itself
            self.setStyleSheet(self.app_settings.get_stylesheet())
        except Exception as _pe:
            print(f"Panel effects error: {_pe}")

        # Apply any settings contributed by docked tools (Map
        # Workshop, etc.)
        for extra_apply in getattr(self, '_extra_apply_callbacks', []):
            try:
                extra_apply()
            except Exception as e:
                print(f"[Settings] A docked tool's settings apply failed: {e}")

        QMessageBox.information(
            self,
            "Applied",
            f"Settings saved successfully!\n\n"
            f"Active theme: {new_settings['theme']}\n"
            f"Colors modified: {len(self._modified_colors) if hasattr(self, '_modified_colors') else 0}"
        )

        # Clear modified colors after saving
        if hasattr(self, '_modified_colors'):
            self._modified_colors = {}


    def _reset_to_defaults(self): #vers 1
        """Reset to default settings"""
        reply = QMessageBox.question(
            self,
            "Reset to Defaults",
            "This will reset all settings to their default values.\n\nAre you sure?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            self.app_settings.current_settings = self.app_settings.default_settings.copy()
            self._load_current_settings()
            QMessageBox.information(self, "Reset", "Settings reset to defaults")


    def _clear_debug_log(self): #vers 1
        """Clear the activity log"""
        if hasattr(self.parent(), 'gui_layout') and hasattr(self.parent().gui_layout, 'log'):
            self.parent().gui_layout.log.clear()
            if hasattr(self.parent(), 'log_message'):
                self.parent().log_message("Debug log cleared")
        else:
            QMessageBox.information(self, "Clear Log", "Activity log cleared (if available).")


    def _ok_clicked(self): #vers 1
        """OK button clicked"""
        self._apply_settings()
        self.accept()


    def _save_current_theme(self): #vers 3
        """Save modifications to the currently selected theme file in themes/"""
        from apps.utils.app_settings_system import THEME_EFFECT_KEYS, store_panel_image
        current_theme_key = self.theme_selector_combo.currentData()

        if not current_theme_key:
            QMessageBox.warning(self, "No Theme", "No theme selected to save.")
            return

        # Get current theme data as base (deep copy to preserve everything)
        import copy
        theme_data = copy.deepcopy(self.app_settings.themes.get(current_theme_key, {}))

        # Update only the color values from editors (preserve all other color section data)
        if "colors" not in theme_data:
            theme_data["colors"] = {}

        for color_key, editor in self.color_editors.items():
            theme_data["colors"][color_key] = editor.color_input.text()

        # Save panel effect, image and transparency settings into theme
        cs = self.app_settings.current_settings
        for key in THEME_EFFECT_KEYS:
            if key in cs:
                theme_data[key] = cs[key]
        if theme_data.get("panel_bg_image"):
            theme_data["panel_bg_image"] = store_panel_image(theme_data["panel_bg_image"],
                                                             self.app_settings.themes_dir)

        # Collect gadget styles if modified
        if hasattr(self, '_gadget_modified') and self._gadget_modified:
            gadget_styles = self._collect_gadget_styles()
            theme_data["styles"] = gadget_styles

        # Save theme to themes/ directory
        success = self.app_settings.save_theme_to_file(current_theme_key, theme_data)

        if success:
            # Update in-memory theme
            self.app_settings.themes[current_theme_key] = theme_data

            theme_file = self.app_settings.themes_dir / f"{current_theme_key}.json"
            QMessageBox.information(
                self, "Saved",
                f"Theme '{current_theme_key}' saved successfully!\n\nFile: {theme_file}"
            )
        else:
            QMessageBox.warning(
                self, "Save Failed",
                f"Failed to save theme '{current_theme_key}' to themes/ directory."
            )


    def _save_theme_as(self): #vers 8
        """Save current theme as a new theme with file dialog - PRESERVES ALL DATA"""
        from apps.utils.app_settings_system import THEME_EFFECT_KEYS, store_panel_image
        from PyQt6.QtWidgets import QInputDialog, QFileDialog, QMessageBox
        import json
        import copy
        import os

        # Ask for new theme name
        theme_name, ok = QInputDialog.getText(
            self,
            "Save Theme As",
            "Theme Naming Guidelines:\n"
            "• Dark themes: Add _Dark suffix (e.g., Ocean_Dark)\n"
            "• Light themes: Add _Light suffix (e.g., Ocean_Light)\n\n"
            "Enter new theme name:",
            text="My_Custom_Theme"
        )

        if not ok or not theme_name:
            return

        # Create safe filename
        safe_filename = "".join(c if c.isalnum() or c in (' ', '_', '-') else '_' for c in theme_name)
        safe_filename = safe_filename.replace(' ', '_').lower()

        # Get current theme as base (DEEP COPY preserves all nested structures)
        current_theme_key = self.theme_selector_combo.currentData()
        base_theme_data = self.app_settings.themes.get(current_theme_key, {})
        theme_data = copy.deepcopy(base_theme_data)

        # Update only the color values from editors (preserve layout params, labels, etc.)
        if "colors" not in theme_data:
            theme_data["colors"] = {}

        for color_key, editor in self.color_editors.items():
            # Only update actual color values, preserve everything else in colors section
            if color_key in theme_data["colors"] or color_key in self.theme_colors:
                theme_data["colors"][color_key] = editor.color_input.text()

        # Save panel effect, image and transparency settings at theme root level
        cs = self.app_settings.current_settings
        for key in THEME_EFFECT_KEYS:
            if key in cs:
                theme_data[key] = cs[key]

        # Update theme metadata
        theme_data.update({
            "name": theme_name,
            "description": f"Custom theme created from {self.theme_selector_combo.currentText()}",
            "category": "Custom",
            "author": "User",
            "version": "1.0"
        })

        # Collect gadget styles if modified
        if hasattr(self, '_gadget_modified') and self._gadget_modified:
            gadget_styles = self._collect_gadget_styles()
            theme_data["styles"] = gadget_styles

        # Show file dialog
        themes_dir = str(self.app_settings.themes_dir)
        os.makedirs(themes_dir, exist_ok=True)
        default_path = os.path.join(themes_dir, f"{safe_filename}.json")

        file_path, _ = QFileDialog.getSaveFileName(
            self,
            "Save Theme As",
            default_path,
            "JSON Theme Files (*.json);;All Files (*)"
        )

        if not file_path:
            return

        # Ensure .json extension
        if not file_path.endswith('.json'):
            file_path += '.json'
        if theme_data.get("panel_bg_image"):                 # image travels with the json
            theme_data["panel_bg_image"] = store_panel_image(theme_data["panel_bg_image"],
                                                             os.path.dirname(file_path))

        # Save theme file
        try:
            with open(file_path, 'w', encoding='utf-8') as f:
                json.dump(theme_data, f, indent=2, ensure_ascii=False)

            # Extract filename without extension for theme key
            theme_key = os.path.splitext(os.path.basename(file_path))[0]

            # Update in-memory themes
            self.app_settings.themes[theme_key] = theme_data

            # Count components for feedback
            color_count = len(theme_data.get('colors', {}))
            menu_items = theme_data.get('menus', {}).get('menu_items', {})
            menu_count = sum(len(items) for items in menu_items.values()) if menu_items else 0
            button_panels = theme_data.get('button_panels', {})
            button_count = sum(len(panel) for panel in button_panels.values()) if button_panels else 0
            font_count = len(theme_data.get('fonts', {}))
            has_styles = "Yes" if "styles" in theme_data else "No"

            QMessageBox.information(
                self, "Theme Saved",
                f"Theme '{theme_name}' saved successfully!\n\n"
                f"File: {file_path}\n\n"
                f"Components saved:\n"
                f"  • Colors section: {color_count} entries\n"
                f"  • Menu items: {menu_count}\n"
                f"  • Button panels: {button_count} buttons\n"
                f"  • Font definitions: {font_count}\n"
                f"  • Widget styles: {has_styles}"
            )

        except Exception as e:
            QMessageBox.warning(
                self, "Save Failed",
                f"Failed to save theme '{theme_name}'.\n\nError: {str(e)}"
            )

