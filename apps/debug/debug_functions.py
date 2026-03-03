# this belongs in apps/debug/debug_functions.py - version 6
# X-Seti - March 03 2026 - IMG Factory 1.6 - Debug Functions
"""
Unified debug/logging system for IMG Factory.
Single entry point: debug_log(main_window, feature_key, msg, level)
Output destinations controlled by Settings > Debug Log tab:
  - Terminal (stdout)
  - Log file  (imgfactory_activity.log or custom path)
  - Activity window (main_window.log_message)
Feature keys map to per-feature toggles in the same tab.
"""

import os
import sys
import time
import inspect
import traceback
from typing import Any, Optional
from pathlib import Path

##Methods list -
# debug_log
# set_debug_main_window
# is_feature_enabled
# is_col_debug_enabled
# set_col_debug_enabled
# toggle_col_debug
# col_debug_log
# integrate_col_debug_with_main_window
# apply_all_fixes_and_improvements
# integrate_all_improvements
# install_search_manager
# fix_search_dialog
# install_debug_control_system
# setup_debug_convenience_methods
# create_debug_menu
# add_status_indicators
# integrate_enhanced_debug_error

##Classes -
# IMGDebugger

# ---------------------------------------------------------------------------
# Feature keys - must match keys used in imgfactory_ui_settings.py
# and debug_log_functions list in img_factory_settings
# ---------------------------------------------------------------------------
FEATURE_KEYS = [
    'import_via',
    'export_via',
    'rename',
    'remove',
    'pin',
    'open',
    'save',
    'rebuild',
    'extract',
    'col',
    'split',
    'merge',
]

_col_debug_enabled = False


class IMGDebugger: #vers 2
    """Unified debug system. Routes messages to terminal, log file, and/or
    activity window depending on Settings > Debug Log output toggles."""

    def __init__(self, log_file: str = "imgfactory_activity.log"):
        self.log_file      = log_file
        self.debug_enabled = False   # master switch (off by default)
        self.log_to_console  = True
        self.log_to_file     = False
        self.log_to_activity = False
        self._main_window    = None

        # Stats
        self.error_count   = 0
        self.warning_count = 0

    def set_main_window(self, main_window): #vers 1
        """Attach the main window so activity-window logging works."""
        self._main_window = main_window
        self._sync_from_settings()

    def _sync_from_settings(self): #vers 1
        """Read output-destination and debug_enabled flags from img_settings."""
        try:
            mw = self._main_window
            if not mw:
                return
            s = getattr(mw, 'img_settings', None)
            if not s:
                return
            self.debug_enabled   = s.get('debug_mode', False)
            self.log_to_console  = s.get('debug_output_terminal', True)
            self.log_to_file     = s.get('debug_output_file', False)
            self.log_to_activity = s.get('debug_output_activity', True)
            self.log_file        = s.get('log_file_path', 'imgfactory_activity.log')
        except Exception:
            pass

    def _enabled_features(self): #vers 1
        """Return the set of enabled feature keys from img_settings."""
        try:
            mw = self._main_window
            if not mw:
                return set()
            s = getattr(mw, 'img_settings', None)
            if not s:
                return set()
            return set(s.get('debug_log_functions', []))
        except Exception:
            return set()

    def _write(self, level: str, feature: str, message: str): #vers 1
        """Write a formatted message to enabled outputs."""
        ts  = time.strftime('%H:%M:%S')
        tag = f"[{feature.upper()}]" if feature else ""
        line = f"[{ts}] {level}{tag} {message}"

        if self.log_to_console:
            print(line)

        if self.log_to_file:
            try:
                with open(self.log_file, 'a', encoding='utf-8') as f:
                    f.write(line + '\n')
            except Exception:
                pass

        if self.log_to_activity and self._main_window:
            try:
                if hasattr(self._main_window, 'log_message'):
                    self._main_window.log_message(line)
            except Exception:
                pass

        if level.startswith('ERROR'):
            self.error_count += 1
        elif level.startswith('WARNING'):
            self.warning_count += 1

    def log(self, level: str, message: str): #vers 2
        """Generic log — no feature gate."""
        if not self.debug_enabled:
            return
        self._write(level, '', message)

    def debug(self, message: str): #vers 2
        self.log('DEBUG   ', message)

    def info(self, message: str): #vers 2
        self.log('INFO    ', message)

    def warning(self, message: str): #vers 2
        self.log('WARNING ', message)

    def error(self, message: str): #vers 2
        self.log('ERROR   ', message)

    def success(self, message: str): #vers 2
        self.log('SUCCESS ', message)

    def feature(self, feature_key: str, message: str, level: str = 'DEBUG'): #vers 1
        """Log only if feature_key is in the enabled debug_log_functions list."""
        if not self.debug_enabled:
            return
        if feature_key not in self._enabled_features():
            return
        self._write(f'{level:<8}', feature_key, message)

    def get_debug_summary(self) -> str: #vers 2
        return (f"Errors: {self.error_count}  "
                f"Warnings: {self.warning_count}  "
                f"Log: {self.log_file}")


# ---------------------------------------------------------------------------
# Global singleton
# ---------------------------------------------------------------------------
img_debugger = IMGDebugger()


# ---------------------------------------------------------------------------
# Public convenience functions
# ---------------------------------------------------------------------------

def set_debug_main_window(main_window): #vers 1
    """Call once from imgfactory.py after startup to attach main window."""
    img_debugger.set_main_window(main_window)


def debug_log(main_window, feature_key: str, msg: str, level: str = 'DEBUG'): #vers 1
    """Single call point for all feature debug output.

    Usage:
        from apps.debug.debug_functions import debug_log
        debug_log(main_window, 'import_via', f"Importing {filename}")

    Output goes to the destinations enabled in Settings > Debug Log,
    only if feature_key is ticked in the feature list.
    """
    # Keep debugger in sync with live main_window
    if img_debugger._main_window is not main_window and main_window is not None:
        img_debugger.set_main_window(main_window)
    img_debugger.feature(feature_key, msg, level)


def is_feature_enabled(main_window, feature_key: str) -> bool: #vers 1
    """Return True if the feature key is ticked in Settings > Debug Log."""
    try:
        s = getattr(main_window, 'img_settings', None)
        if not s:
            return False
        return feature_key in s.get('debug_log_functions', [])
    except Exception:
        return False


# ---------------------------------------------------------------------------
# COL debug helpers (kept for backward compatibility)
# ---------------------------------------------------------------------------

def is_col_debug_enabled() -> bool: #vers 2
    global _col_debug_enabled
    return _col_debug_enabled and img_debugger.debug_enabled


def set_col_debug_enabled(enabled: bool): #vers 2
    global _col_debug_enabled
    _col_debug_enabled = enabled


def toggle_col_debug() -> bool: #vers 2
    set_col_debug_enabled(not is_col_debug_enabled())
    return is_col_debug_enabled()


def col_debug_log(main_window, message: str, category: str = 'COL', level: str = 'DEBUG'): #vers 2
    """COL-specific debug log — routes through unified system."""
    debug_log(main_window, 'col', f"[{category}] {message}", level)


def integrate_col_debug_with_main_window(main_window): #vers 2
    """Attach COL debug helpers to main window."""
    try:
        main_window.enable_col_debug  = lambda: set_col_debug_enabled(True)
        main_window.disable_col_debug = lambda: set_col_debug_enabled(False)
        main_window.toggle_col_debug  = toggle_col_debug
        main_window.is_col_debug_enabled = is_col_debug_enabled
        set_col_debug_enabled(False)
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"COL debug integration error: {e}")
        return False


# ---------------------------------------------------------------------------
# Integration / setup helpers (called from imgfactory.py startup)
# ---------------------------------------------------------------------------

def apply_all_fixes_and_improvements(main_window): #vers 2
    """Apply startup fixes."""
    try:
        set_debug_main_window(main_window)
        install_search_manager(main_window)
        fix_search_dialog(main_window)
        install_debug_control_system(main_window)
        integrate_col_debug_with_main_window(main_window)
        setup_debug_convenience_methods(main_window)
        create_debug_menu(main_window)
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Debug setup error: {e}")
        return False


def integrate_all_improvements(main_window): #vers 2
    """Main integration entry point."""
    try:
        result = apply_all_fixes_and_improvements(main_window)
        add_status_indicators(main_window)
        return result
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Integration error: {e}")
        return False


def install_search_manager(main_window): #vers 2
    try:
        from apps.core.guisearch import install_search_system
        return install_search_system(main_window)
    except Exception:
        return False


def fix_search_dialog(main_window): #vers 2
    try:
        if not hasattr(main_window, 'show_search_dialog'):
            def show_search_dialog():
                try:
                    from apps.core.dialogs import show_search_dialog as _sd
                    _sd(main_window)
                except ImportError:
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.information(main_window, "Search",
                        "Use Ctrl+F for quick search.")
            main_window.show_search_dialog = show_search_dialog
        return True
    except Exception:
        return False


def install_debug_control_system(main_window): #vers 2
    try:
        def show_debug_settings():
            try:
                from apps.utils.app_settings_system import SettingsDialog
                if hasattr(main_window, 'app_settings'):
                    SettingsDialog(main_window.app_settings, main_window).exec()
            except Exception:
                pass
        main_window.show_debug_settings = show_debug_settings
        return True
    except Exception:
        return False


def setup_debug_convenience_methods(main_window): #vers 2
    try:
        def show_debug_info():
            from PyQt6.QtWidgets import QMessageBox
            info = (f"Debug enabled: {img_debugger.debug_enabled}\n"
                    f"Terminal: {img_debugger.log_to_console}\n"
                    f"Log file: {img_debugger.log_to_file}  ({img_debugger.log_file})\n"
                    f"Activity window: {img_debugger.log_to_activity}\n"
                    f"Errors: {img_debugger.error_count}  "
                    f"Warnings: {img_debugger.warning_count}")
            QMessageBox.information(main_window, "Debug Information", info)

        def view_debug_log():
            from PyQt6.QtWidgets import QMessageBox
            if os.path.exists(img_debugger.log_file):
                os.system(f"xdg-open {img_debugger.log_file}")
            else:
                QMessageBox.information(main_window, "Debug Log", "No log file found.")

        main_window.show_debug_info = show_debug_info
        main_window.view_debug_log  = view_debug_log
        return True
    except Exception:
        return False


def create_debug_menu(main_window): #vers 2
    try:
        from PyQt6.QtWidgets import QMenu
        from PyQt6.QtGui import QKeySequence
        menu_bar = main_window.menuBar()
        debug_menu = None
        for action in menu_bar.actions():
            if 'debug' in action.text().lower():
                debug_menu = action.menu()
                break
        if not debug_menu:
            debug_menu = menu_bar.addMenu("Debug")
        debug_menu.clear()

        a = debug_menu.addAction("Debug Information")
        a.triggered.connect(lambda: main_window.show_debug_info()
                            if hasattr(main_window, 'show_debug_info') else None)

        b = debug_menu.addAction("View Log File")
        b.triggered.connect(lambda: main_window.view_debug_log()
                            if hasattr(main_window, 'view_debug_log') else None)

        debug_menu.addSeparator()

        c = debug_menu.addAction("Debug Settings")
        c.triggered.connect(lambda: main_window.show_debug_settings()
                            if hasattr(main_window, 'show_debug_settings') else None)

        col = debug_menu.addAction("Toggle COL Debug")
        col.triggered.connect(toggle_col_debug)
        col.setShortcut(QKeySequence("Ctrl+Shift+D"))

        return True
    except Exception:
        return False


def add_status_indicators(main_window): #vers 2
    """Add debug status label to status bar."""
    try:
        from PyQt6.QtWidgets import QLabel
        from PyQt6.QtCore import QTimer
        lbl = QLabel("Debug: off")
        lbl.setToolTip("Debug system — configure in Settings > Debug Log")

        def _update():
            if img_debugger.debug_enabled:
                lbl.setText("Debug: on")
            else:
                lbl.setText("Debug: off")

        _update()
        main_window.statusBar().addPermanentWidget(lbl)
        t = QTimer(main_window)
        t.timeout.connect(_update)
        t.start(5000)
        return True
    except Exception:
        return False


def integrate_enhanced_debug_error(main_window): #vers 2
    """Install sys.excepthook for uncaught exceptions."""
    try:
        def _hook(exc_type, exc_value, exc_tb):
            img_debugger.error(
                f"Uncaught {exc_type.__name__}: {exc_value}\n"
                + "".join(traceback.format_tb(exc_tb)))
            sys.__excepthook__(exc_type, exc_value, exc_tb)
        sys.excepthook = _hook
        return True
    except Exception:
        return False


# ---------------------------------------------------------------------------
# Decorator
# ---------------------------------------------------------------------------

def trace_function(func): #vers 2
    """Decorator — traces call/result when debug_enabled."""
    def wrapper(*args, **kwargs):
        if not img_debugger.debug_enabled:
            return func(*args, **kwargs)
        t0 = time.time()
        img_debugger.debug(f"CALL {func.__name__}")
        try:
            result = func(*args, **kwargs)
            img_debugger.debug(
                f"DONE {func.__name__} ({time.time()-t0:.3f}s)")
            return result
        except Exception as e:
            img_debugger.error(f"EXCEPTION in {func.__name__}: {e}")
            raise
    return wrapper


__all__ = [
    'IMGDebugger',
    'img_debugger',
    'debug_log',
    'set_debug_main_window',
    'is_feature_enabled',
    'is_col_debug_enabled',
    'set_col_debug_enabled',
    'toggle_col_debug',
    'col_debug_log',
    'integrate_col_debug_with_main_window',
    'apply_all_fixes_and_improvements',
    'integrate_all_improvements',
    'install_search_manager',
    'fix_search_dialog',
    'install_debug_control_system',
    'setup_debug_convenience_methods',
    'create_debug_menu',
    'add_status_indicators',
    'integrate_enhanced_debug_error',
    'trace_function',
    'FEATURE_KEYS',
]
