#this belongs in methods/img_factory_settings.py - Version: 2
# X-Seti - December31 2025 - IMG Factory 1.6
"""
IMG Factory-specific settings manager
Handles application-specific settings separate from global theme settings
"""
import json
import os
from pathlib import Path
from typing import Dict, Any

class IMGFactorySettings:
    def __init__(self):
        self.settings_file = Path.home() / ".config" / "img-factory" / "app_settings.json"
        self.settings_file.parent.mkdir(parents=True, exist_ok=True)

        # Default settings
        self.defaults = {
            # Import/Export behavior
            "auto_save_on_import": True,
            "auto_reload_on_import": False,

            # IDE Integration
            "load_ide_with_img": False,
            "preferred_ide_name": "TXD Workshop",

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
            "last_window_x": -1,
            "last_window_y": -1,

            # UI Mode (NEW)
            "ui_mode": "system",
            "show_toolbar": True,
            "show_status_bar": True,
            "show_menu_bar": True,

            # File handling
            "recent_files_limit": 10,
            "auto_backup": False,
            "backup_count": 3,
        }

        self.current_settings = self.load_settings()

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
                print(f"Error loading settings: {e}")
                return self.defaults.copy()
        return self.defaults.copy()

    def save_settings(self):
        """Save current settings to file"""
        try:
            with open(self.settings_file, 'w') as f:
                json.dump(self.current_settings, f, indent=4)
        except Exception as e:
            print(f"Error saving settings: {e}")

    def get(self, key: str, default=None) -> Any:
        """Get a setting value"""
        return self.current_settings.get(key, default)

    def set(self, key: str, value: Any):
        """Set a setting value"""
        self.current_settings[key] = value

    def reset_to_defaults(self):
        """Reset all settings to defaults"""
        self.current_settings = self.defaults.copy()
        self.save_settings()
