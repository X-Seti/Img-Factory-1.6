#this belongs in methods/img_factory_settings.py - Version: 2
# X-Seti - December31 2025 - IMG Factory 1.6
"""
IMG Factory-specific settings dialog
Handles application-specific settings with UI
"""
import json
import os
from pathlib import Path
from typing import Dict, Any
from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QGroupBox, QCheckBox,
                            QPushButton, QHBoxLayout, QSpinBox, QLabel,
                            QComboBox, QFontComboBox, QTabWidget, QWidget,
                            QMessageBox)
from PyQt6.QtGui import QFont

class IMGFactorySettings(QDialog):
    def __init__(self, parent_window=None):
        super().__init__(parent_window)
        self.parent_window = parent_window
        self.settings_file = Path.home() / ".config" / "img-factory" / "app_settings.json"
        self.settings_file.parent.mkdir(parents=True, exist_ok=True)

        # Default settings
        self.defaults = {
            # Import/Export behavior
            "auto_save_on_import": True,
            "auto_reload_on_import": False,

            # IDE Integration
            "load_ide_with_img": False,
            "preferred_ide_name": "TXD Workshop",  # or "IDE Workshop"

            # Button Layout
            "button_horizontal_spacing": 10,
            "button_vertical_spacing": 10,

            # Font Settings
            "use_custom_font": False,
            "font_family": "Segoe UI",
            "font_size": 9,
            "font_bold": False,
            "font_italic": False,

            # Window behavior
            "remember_window_size": True,
            "remember_window_position": True,
            "last_window_width": 1200,
            "last_window_height": 800,
            "last_window_x": -1,  # -1 means center
            "last_window_y": -1,

            # File handling
            "recent_files_limit": 10,
            "auto_backup": False,
            "backup_count": 3,
            
            # Button display mode
            "button_display_mode": "both",  # "both", "icons", "text"
        }

        self.current_settings = self.load_settings()
        
        self.setWindowTitle("IMG Factory Settings")
        self.setMinimumWidth(500)
        self.setMinimumHeight(400)
        
        self._setup_ui()

    def _setup_ui(self):
        """Setup the user interface"""
        main_layout = QVBoxLayout(self)

        # Create tabbed interface
        tabs = QTabWidget()

        # ===== TAB 1: General =====
        general_tab = QWidget()
        general_layout = QVBoxLayout(general_tab)

        # Import/Export Group
        import_group = QGroupBox("Import/Export Behavior")
        import_layout = QVBoxLayout()

        self.auto_save_cb = QCheckBox("Auto-save after import")
        self.auto_save_cb.setChecked(self.current_settings.get("auto_save_on_import", True))
        import_layout.addWidget(self.auto_save_cb)

        self.auto_reload_cb = QCheckBox("Auto-reload after import (reload from disk)")
        self.auto_reload_cb.setChecked(self.current_settings.get("auto_reload_on_import", False))
        import_layout.addWidget(self.auto_reload_cb)

        import_group.setLayout(import_layout)
        general_layout.addWidget(import_group)

        # IDE Integration Group
        ide_group = QGroupBox("IDE Integration")
        ide_layout = QVBoxLayout()

        self.load_ide_cb = QCheckBox("Load IDE file automatically if found with IMG")
        self.load_ide_cb.setChecked(self.current_settings.get("load_ide_with_img", False))
        ide_layout.addWidget(self.load_ide_cb)

        ide_pref_layout = QHBoxLayout()
        ide_pref_layout.addWidget(QLabel("Preferred IDE tool:"))
        self.ide_combo = QComboBox()
        self.ide_combo.addItems(["TXD Workshop", "IDE Workshop"])
        self.ide_combo.setCurrentText(self.current_settings.get("preferred_ide_name", "TXD Workshop"))
        ide_pref_layout.addWidget(self.ide_combo)
        ide_layout.addLayout(ide_pref_layout)

        ide_group.setLayout(ide_layout)
        general_layout.addWidget(ide_group)

        # Window Behavior Group
        window_group = QGroupBox("Window Behavior")
        window_layout = QVBoxLayout()

        self.remember_size_cb = QCheckBox("Remember window size")
        self.remember_size_cb.setChecked(self.current_settings.get("remember_window_size", True))
        window_layout.addWidget(self.remember_size_cb)

        self.remember_pos_cb = QCheckBox("Remember window position")
        self.remember_pos_cb.setChecked(self.current_settings.get("remember_window_position", True))
        window_layout.addWidget(self.remember_pos_cb)

        window_group.setLayout(window_layout)
        general_layout.addWidget(window_group)

        general_layout.addStretch()
        tabs.addTab(general_tab, "General")

        # ===== TAB 2: Interface =====
        interface_tab = QWidget()
        interface_layout = QVBoxLayout(interface_tab)

        # Button Display Mode Group
        button_mode_group = QGroupBox("Button Display Mode")
        button_mode_layout = QVBoxLayout()

        button_mode_layout.addWidget(QLabel("Show buttons as:"))
        self.button_mode_combo = QComboBox()
        self.button_mode_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])
        
        # Set current selection based on saved setting
        mode = self.current_settings.get("button_display_mode", "both")
        mode_map = {"both": 0, "icons": 1, "text": 2}
        self.button_mode_combo.setCurrentIndex(mode_map.get(mode, 0))
        
        button_mode_layout.addWidget(self.button_mode_combo)
        button_mode_group.setLayout(button_mode_layout)
        interface_layout.addWidget(button_mode_group)

        # Button Spacing Group
        button_group = QGroupBox("Button Layout")
        button_layout = QVBoxLayout()

        h_spacing_layout = QHBoxLayout()
        h_spacing_layout.addWidget(QLabel("Horizontal spacing:"))
        self.h_spacing_spin = QSpinBox()
        self.h_spacing_spin.setRange(0, 50)
        self.h_spacing_spin.setValue(self.current_settings.get("button_horizontal_spacing", 10))
        self.h_spacing_spin.setSuffix(" px")
        h_spacing_layout.addWidget(self.h_spacing_spin)
        h_spacing_layout.addStretch()
        button_layout.addLayout(h_spacing_layout)

        v_spacing_layout = QHBoxLayout()
        v_spacing_layout.addWidget(QLabel("Vertical spacing:"))
        self.v_spacing_spin = QSpinBox()
        self.v_spacing_spin.setRange(0, 50)
        self.v_spacing_spin.setValue(self.current_settings.get("button_vertical_spacing", 10))
        self.v_spacing_spin.setSuffix(" px")
        v_spacing_layout.addWidget(self.v_spacing_spin)
        v_spacing_layout.addStretch()
        button_layout.addLayout(v_spacing_layout)

        button_group.setLayout(button_layout)
        interface_layout.addWidget(button_group)

        # Font Settings Group
        font_group = QGroupBox("Font Settings")
        font_layout = QVBoxLayout()

        self.use_custom_font_cb = QCheckBox("Use custom font settings")
        self.use_custom_font_cb.setChecked(self.current_settings.get("use_custom_font", False))
        font_layout.addWidget(self.use_custom_font_cb)

        font_family_layout = QHBoxLayout()
        font_family_layout.addWidget(QLabel("Font:"))
        self.font_combo = QFontComboBox()
        self.font_combo.setCurrentFont(QFont(self.current_settings.get("font_family", "Segoe UI")))
        self.font_combo.setEnabled(self.use_custom_font_cb.isChecked())
        font_family_layout.addWidget(self.font_combo)
        font_layout.addLayout(font_family_layout)

        font_size_layout = QHBoxLayout()
        font_size_layout.addWidget(QLabel("Size:"))
        self.font_size_spin = QSpinBox()
        self.font_size_spin.setRange(6, 24)
        self.font_size_spin.setValue(self.current_settings.get("font_size", 9))
        self.font_size_spin.setSuffix(" pt")
        self.font_size_spin.setEnabled(self.use_custom_font_cb.isChecked())
        font_size_layout.addWidget(self.font_size_spin)
        font_size_layout.addStretch()
        font_layout.addLayout(font_size_layout)

        font_style_layout = QHBoxLayout()
        self.font_bold_cb = QCheckBox("Bold")
        self.font_bold_cb.setChecked(self.current_settings.get("font_bold", False))
        self.font_bold_cb.setEnabled(self.use_custom_font_cb.isChecked())
        font_style_layout.addWidget(self.font_bold_cb)

        self.font_italic_cb = QCheckBox("Italic")
        self.font_italic_cb.setChecked(self.current_settings.get("font_italic", False))
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
        interface_layout.addWidget(font_group)

        interface_layout.addStretch()
        tabs.addTab(interface_tab, "Interface")

        # ===== TAB 3: Advanced =====
        advanced_tab = QWidget()
        advanced_layout = QVBoxLayout(advanced_tab)

        # File Handling Group
        file_group = QGroupBox("File Handling")
        file_layout = QVBoxLayout()

        recent_files_layout = QHBoxLayout()
        recent_files_layout.addWidget(QLabel("Recent files limit:"))
        self.recent_files_spin = QSpinBox()
        self.recent_files_spin.setRange(5, 50)
        self.recent_files_spin.setValue(self.current_settings.get("recent_files_limit", 10))
        recent_files_layout.addWidget(self.recent_files_spin)
        recent_files_layout.addStretch()
        file_layout.addLayout(recent_files_layout)

        self.auto_backup_cb = QCheckBox("Create automatic backups")
        self.auto_backup_cb.setChecked(self.current_settings.get("auto_backup", False))
        file_layout.addWidget(self.auto_backup_cb)

        backup_count_layout = QHBoxLayout()
        backup_count_layout.addWidget(QLabel("Number of backups:"))
        self.backup_count_spin = QSpinBox()
        self.backup_count_spin.setRange(1, 10)
        self.backup_count_spin.setValue(self.current_settings.get("backup_count", 3))
        self.backup_count_spin.setEnabled(self.auto_backup_cb.isChecked())
        backup_count_layout.addWidget(self.backup_count_spin)
        backup_count_layout.addStretch()
        file_layout.addLayout(backup_count_layout)

        self.auto_backup_cb.toggled.connect(self.backup_count_spin.setEnabled)

        file_group.setLayout(file_layout)
        advanced_layout.addWidget(file_group)

        advanced_layout.addStretch()
        tabs.addTab(advanced_tab, "Advanced")

        # Add tabs to main layout
        main_layout.addWidget(tabs)

        # Buttons
        button_layout = QHBoxLayout()

        self.reset_btn = QPushButton("Reset to Defaults")
        self.ok_btn = QPushButton("OK")
        self.apply_btn = QPushButton("Apply")
        self.cancel_btn = QPushButton("Cancel")

        def save_settings():
            """Save all settings"""
            # General tab
            self.current_settings["auto_save_on_import"] = self.auto_save_cb.isChecked()
            self.current_settings["auto_reload_on_import"] = self.auto_reload_cb.isChecked()
            self.current_settings["load_ide_with_img"] = self.load_ide_cb.isChecked()
            self.current_settings["preferred_ide_name"] = self.ide_combo.currentText()
            self.current_settings["remember_window_size"] = self.remember_size_cb.isChecked()
            self.current_settings["remember_window_position"] = self.remember_pos_cb.isChecked()

            # Interface tab
            # Map combo box index back to setting value
            mode_index = self.button_mode_combo.currentIndex()
            mode_map = {0: "both", 1: "icons", 2: "text"}
            self.current_settings["button_display_mode"] = mode_map.get(mode_index, "both")
            
            self.current_settings["button_horizontal_spacing"] = self.h_spacing_spin.value()
            self.current_settings["button_vertical_spacing"] = self.v_spacing_spin.value()
            self.current_settings["use_custom_font"] = self.use_custom_font_cb.isChecked()
            self.current_settings["font_family"] = self.font_combo.currentFont().family()
            self.current_settings["font_size"] = self.font_size_spin.value()
            self.current_settings["font_bold"] = self.font_bold_cb.isChecked()
            self.current_settings["font_italic"] = self.font_italic_cb.isChecked()

            # Advanced tab
            self.current_settings["recent_files_limit"] = self.recent_files_spin.value()
            self.current_settings["auto_backup"] = self.auto_backup_cb.isChecked()
            self.current_settings["backup_count"] = self.backup_count_spin.value()

            self.save_settings()

        def apply_settings():
            """Apply settings without closing"""
            save_settings()
            # Apply immediate changes if needed
            self.apply_settings_to_parent()
            QMessageBox.information(self, "Settings Applied", "Settings have been applied successfully.")

        def reset_settings():
            """Reset to defaults"""
            reply = QMessageBox.question(self, "Reset Settings",
                                        "Are you sure you want to reset all settings to defaults?",
                                        QMessageBox.StandardButton.StandardButton.Yes | QMessageBox.StandardButton.StandardButton.No)
            if reply == QMessageBox.StandardButton.StandardButton.Yes:
                self.reset_to_defaults()
                # Reload UI with defaults
                self._reload_ui_from_settings()
                # Apply settings to parent
                self.apply_settings_to_parent()

        self.reset_btn.clicked.connect(reset_settings)
        self.ok_btn.clicked.connect(lambda: (save_settings(), self.accept()))
        self.apply_btn.clicked.connect(apply_settings)
        self.cancel_btn.clicked.connect(self.reject)

        button_layout.addWidget(self.reset_btn)
        button_layout.addStretch()
        button_layout.addWidget(self.ok_btn)
        button_layout.addWidget(self.apply_btn)
        button_layout.addWidget(self.cancel_btn)

        main_layout.addLayout(button_layout)

    def _reload_ui_from_settings(self):
        """Reload UI elements from current settings"""
        # General tab
        self.auto_save_cb.setChecked(self.current_settings.get("auto_save_on_import", True))
        self.auto_reload_cb.setChecked(self.current_settings.get("auto_reload_on_import", False))
        self.load_ide_cb.setChecked(self.current_settings.get("load_ide_with_img", False))
        self.ide_combo.setCurrentText(self.current_settings.get("preferred_ide_name", "TXD Workshop"))
        self.remember_size_cb.setChecked(self.current_settings.get("remember_window_size", True))
        self.remember_pos_cb.setChecked(self.current_settings.get("remember_window_position", True))

        # Interface tab
        mode = self.current_settings.get("button_display_mode", "both")
        mode_map = {"both": 0, "icons": 1, "text": 2}
        self.button_mode_combo.setCurrentIndex(mode_map.get(mode, 0))
        
        self.h_spacing_spin.setValue(self.current_settings.get("button_horizontal_spacing", 10))
        self.v_spacing_spin.setValue(self.current_settings.get("button_vertical_spacing", 10))
        self.use_custom_font_cb.setChecked(self.current_settings.get("use_custom_font", False))
        self.font_combo.setCurrentFont(QFont(self.current_settings.get("font_family", "Segoe UI")))
        self.font_size_spin.setValue(self.current_settings.get("font_size", 9))
        self.font_bold_cb.setChecked(self.current_settings.get("font_bold", False))
        self.font_italic_cb.setChecked(self.current_settings.get("font_italic", False))

        # Advanced tab
        self.recent_files_spin.setValue(self.current_settings.get("recent_files_limit", 10))
        self.auto_backup_cb.setChecked(self.current_settings.get("auto_backup", False))
        self.backup_count_spin.setValue(self.current_settings.get("backup_count", 3))

    def load_settings(self) -> Dict[str, Any]:
        """Load settings from file or return defaults"""
        if self.settings_file.exists():
            try:
                with open(self.settings_file, 'r') as f:
                    loaded = json.load(f)
                    # Merge with defaults to ensure all keys exist
                    settings = self.defaults.copy()
                    settings.update(loaded)
                    return settings
            except Exception as e:
                print(f"Error loading IMG Factory settings: {e}")
                return self.defaults.copy()
        return self.defaults.copy()

    def save_settings(self) -> bool:
        """Save current settings to file"""
        try:
            with open(self.settings_file, 'w') as f:
                json.dump(self.current_settings, f, indent=4)
            return True
        except Exception as e:
            print(f"Error saving IMG Factory settings: {e}")
            return False

    def get(self, key: str, default=None):
        """Get a setting value"""
        return self.current_settings.get(key, default)

    def set(self, key: str, value: Any):
        """Set a setting value"""
        self.current_settings[key] = value

    def reset_to_defaults(self):
        """Reset all settings to defaults"""
        self.current_settings = self.defaults.copy()
        self.save_settings()

    def apply_settings_to_parent(self):
        """Apply settings changes to the parent window"""
        if self.parent_window is None:
            return
            
        # Apply button display mode if it has changed
        button_mode = self.current_settings.get("button_display_mode", "both")
        
        # Apply to GUI layout if available
        if hasattr(self.parent_window, 'gui_layout') and hasattr(self.parent_window.gui_layout, 'set_button_display_mode'):
            # Map the settings mode to the GUI mode
            mode_mapping = {
                'both': 'icons_with_text',    # Icons + Text
                'icons': 'icons_only',        # Icons Only  
                'text': 'text_only'           # Text Only
            }
            gui_mode = mode_mapping.get(button_mode, 'icons_with_text')
            self.parent_window.gui_layout.set_button_display_mode(gui_mode)
