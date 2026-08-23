#this belongs in methods/img_factory_settings.py - Version: 4
# X-Seti - December31 2025 - IMG Factory 1.6
"""
IMG Factory-specific settings manager
Handles application-specific settings separate from global theme settings
"""
import json
import os
from pathlib import Path
from typing import Dict, Any

def _img_factory_config_dir() -> Path: #vers 1
    """The one, correct location for IMG Factory's own app_settings.json
    - a dedicated config/ subfolder alongside imgfactory.py itself
    (Aug 20 2026, per Keith: "lets fix map workshop and img factory
    first since we're working on those" - same real fix already
    applied to Map/Model Workshop's own config just before this, for
    the same underlying reason: "all config files would need to be
    with the own app folder" for a standalone deployment to be truly
    self-contained and portable, rather than scattered into the
    running user's own home directory).

    This module (apps/methods/img_factory_settings.py) is a shared
    helper, not the real app file itself - imgfactory.py (apps/
    components/Img_Factory/imgfactory.py) is - so this can't just use
    this module's own __file__ the way Map Workshop's identical fix
    could (that fix's own settings class lives directly inside the
    one file it configures). Navigates from this file's own known,
    fixed location (apps/methods/) up to apps/, then down into
    components/Img_Factory/config/ - reliable as long as that
    directory relationship holds, which it structurally always does
    for this app.

    Falls back to the old ~/.config/img-factory location only if the
    app's own folder genuinely isn't writable (a real, if less
    common, possibility - e.g. a read-only system install) - settings
    simply won't travel with the app folder in that one specific
    case, which is still strictly better than every config-writing
    call in this app failing outright."""
    cfg_dir = Path(__file__).resolve().parent.parent / 'components' / 'Img_Factory' / 'config'
    try:
        cfg_dir.mkdir(parents=True, exist_ok=True)
        probe = cfg_dir / '.write_test'
        probe.write_text('')
        probe.unlink()
        return cfg_dir
    except Exception as e:
        print(f"[IMGFactorySettings] App folder not writable ({e}), "
              f"falling back to ~/.config/img-factory")
        fallback = Path.home() / '.config' / 'img-factory'
        fallback.mkdir(parents=True, exist_ok=True)
        return fallback


class IMGFactorySettings:
    def __init__(self):
        self.settings_file = _img_factory_config_dir() / 'app_settings.json'

        # Default settings
        self.defaults = {
            # Import/Export behavior
            "auto_save_on_import": True,
            "auto_reload_on_import": False,

            # Debug / Logging
            "debug_mode": False,
            "debug_output_terminal": True,
            "debug_output_file": False,
            "debug_output_activity": True,
            "log_to_file": False,
            "log_file_path": "imgfactory_activity.log",
            "debug_log_functions": [],

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

            # Panel collapse threshold (pixels) — workshop side panels
            # switch from text+icon buttons to icon-only below this right-panel width
            "panel_collapse_threshold": 550,

            # Tab Settings
            "tab_height": 24,
            "tab_min_width": 100,
            "tab_style": "default",
            "tab_position": "top",

            # File handling
            "recent_files_limit": 10,
            "auto_backup": False,
            "backup_count": 3,

            # Last active project (IMG Factory's own record - restored at
            # startup so game_root doesn't need Project Manager to be
            # opened manually every launch)
            "last_project_name": "",
            "last_game_root": "",
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
        """Save current settings to file. Atomic write (Aug 20 2026) -
        temp file in the same directory, then os.replace over the
        real path - a crash/interruption mid-write can now only ever
        leave the OLD file intact or the NEW one fully written, never
        a half-written, corrupt file in between the two (same real
        fix already applied to Map Workshop's own MapSettings for the
        identical reason - a torn write here would silently revert
        every IMG Factory setting at once on the next launch, not
        just this one file's own data)."""
        try:
            tmp_path = self.settings_file.with_suffix('.json.tmp')
            tmp_path.write_text(json.dumps(self.current_settings, indent=4))
            os.replace(tmp_path, self.settings_file)
        except Exception as e:
            print(f"Error saving settings: {e}")

    def get(self, key: str, default=None) -> Any:
        """Get a setting value"""
        return self.current_settings.get(key, default)

    def set(self, key: str, value: Any) -> None:
        """Set a setting value"""
        self.current_settings[key] = value

    def get_panel_collapse_threshold(self) -> int:
        """Return the right-panel width at which side buttons collapse to icons."""
        return int(self.current_settings.get("panel_collapse_threshold", 550))

    def reset_to_defaults(self):
        """Reset all settings to defaults"""
        self.current_settings = self.defaults.copy()
        self.save_settings()


def get_img_factory_qsettings(): #vers 1
    """A single, shared, app-folder-relative QSettings instance for
    IMG Factory's own window geometry/splitter state/game_root (Aug
    20 2026, per Keith: "lets fix map workshop and img factory first"
    - same real reasoning as _img_factory_config_dir just above).

    Real, confirmed mess this consolidates: imgfactory.py had FOUR
    separate QSettings(...) call sites using TWO different (org, app)
    name pairs - QSettings("IMG-Factory", "IMG-Factory") for game_root,
    QSettings("XSeti", "IMGFactory") for window geometry/splitter
    state - each pair creating its own, separate native-location
    settings file (on Linux, a separate ~/.config/<org>/<app>.conf
    each), on top of the several OTHER differently-named "IMG
    Factory"-ish QSettings organizations already found scattered
    elsewhere in this app (a real, wider audit still to come). These
    two specific ones are consolidated into one shared .ini file here
    since their own keys never actually collide (game_root vs
    geometry/splitter_state), reducing both the file count and the
    chance of yet another accidentally-different name being
    introduced later.

    QSettings.Format.IniFormat with an explicit file path bypasses
    Qt's own native (org, app) -> OS-standard-location lookup
    entirely - this is what actually makes it app-folder-relative
    rather than just picking a differently-worded org/app pair that
    would still resolve to some other ~/.config subfolder."""
    from PyQt6.QtCore import QSettings
    ini_path = _img_factory_config_dir() / 'img_factory_state.ini'
    return QSettings(str(ini_path), QSettings.Format.IniFormat)
