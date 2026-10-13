#this belongs in methods/progressbar.py - Version: 1
# X-Seti - August05 2025 - IMG Factory 1.5 - Unified Progress Bar System

"""
Unified Progress Bar System
Centralizes all progress bar functionality across IMG Factory
"""

from typing import Optional, Any
from PyQt6.QtCore import QTimer

##Methods list -
# get_progress_manager
# hide_progress
# integrate_progress_system
# reset_progress
# show_progress
# update_progress

##Classes -
# ProgressManager

_recursion_guard = False

class ProgressManager:
    """Centralized progress management for IMG Factory"""

    def __init__(self, main_window): #vers 1
        """Initialize with main window reference"""
        self.main_window = main_window
        self.current_operation = None
        self.is_active = False

        # Auto-reset timer for stale progress
        self.auto_reset_timer = QTimer()
        self.auto_reset_timer.setSingleShot(True)
        self.auto_reset_timer.timeout.connect(self._auto_reset)


    def show_progress(self, value: int = 0, text: str = "Working...", auto_reset_ms: int = 30000): #vers 1
        """Show progress bar with standardized interface

        Args:
            value: Progress value (0-100)
            text: Status message
            auto_reset_ms: Auto-reset timeout in milliseconds (0 = disabled)
        """
        try:
            self.is_active = True
            self.current_operation = text

            # Try main window progress system first (gui/status_bar.py)
            if hasattr(self.main_window, 'show_progress'):
                self.main_window.show_progress(text, 0, 100)
                if hasattr(self.main_window, 'update_progress'):
                    self.main_window.update_progress(value, text)

            # Fallback to gui_layout progress if available
            elif hasattr(self.main_window, 'gui_layout') and hasattr(self.main_window.gui_layout, 'show_progress'):
                self.main_window.gui_layout.show_progress(value, text)

            # Final fallback to status bar
            elif hasattr(self.main_window, 'statusBar'):
                self.main_window.statusBar().showMessage(f"{text} ({value}%)")

            # Log progress for debugging
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Progress: {value}% - {text}")

            # Set auto-reset timer if requested
            if auto_reset_ms > 0:
                self.auto_reset_timer.start(auto_reset_ms)

        except Exception as e:
            print(f"Progress show error: {e}")


    def update_progress(self, value: int, text: str = None): #vers 1
        """Update progress value and optional text

        Args:
            value: Progress value (0-100)
            text: Optional new status message
        """
        try:
            if not self.is_active:
                return

            # Update current operation text if provided
            if text:
                self.current_operation = text

            # Try main window progress system first
            if hasattr(self.main_window, 'update_progress'):
                self.main_window.update_progress(value, text)

            # Fallback to gui_layout
            elif hasattr(self.main_window, 'gui_layout') and hasattr(self.main_window.gui_layout, 'show_progress'):
                current_text = text or self.current_operation or "Working..."
                self.main_window.gui_layout.show_progress(value, current_text)

            # Final fallback to status bar
            elif hasattr(self.main_window, 'statusBar'):
                current_text = text or self.current_operation or "Working..."
                self.main_window.statusBar().showMessage(f"{current_text} ({value}%)")

        except Exception as e:
            print(f"Progress update error: {e}")


    def hide_progress(self, final_text: str = "Ready"): #vers 1
        """Hide progress bar and reset to ready state

        Args:
            final_text: Final status message to display
        """
        try:
            self.is_active = False
            self.current_operation = None

            # Stop auto-reset timer
            if self.auto_reset_timer.isActive():
                self.auto_reset_timer.stop()

            # Try main window hide method first
            if hasattr(self.main_window, 'hide_progress'):
                self.main_window.hide_progress()
                if hasattr(self.main_window, 'show_permanent_status'):
                    self.main_window.show_permanent_status(final_text)

            # Fallback to gui_layout with -1 value (reset signal)
            elif hasattr(self.main_window, 'gui_layout') and hasattr(self.main_window.gui_layout, 'show_progress'):
                self.main_window.gui_layout.show_progress(-1, final_text)

            # Final fallback to status bar
            elif hasattr(self.main_window, 'statusBar'):
                self.main_window.statusBar().showMessage(final_text)

            # Log completion
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Progress completed: {final_text}")

        except Exception as e:
            print(f"Progress hide error: {e}")


    def reset_progress(self): #vers 1
        """Reset progress to ready state (alias for hide_progress)"""
        self.hide_progress("Ready")


    def _auto_reset_old(self): #vers 1
        """Auto-reset progress if it's been active too long"""
        if self.is_active:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Progress auto-reset due to timeout")
            self.hide_progress("Ready")


    def _auto_reset(self):  # vers 6
        global _recursion_guard
        if _recursion_guard:
            return
        try:
            _recursion_guard = True
            if hasattr(self, '_progress_bar') and self._progress_bar:
                self._progress_bar.setValue(0)
                self._progress_bar.setVisible(False)
            if hasattr(self, '_status_label') and self._status_label:
                self._status_label.setText("Ready")
        finally:
            _recursion_guard = False


# Global progress manager instance
_progress_manager: Optional[ProgressManager] = None

def get_progress_manager(main_window) -> ProgressManager:
    """Get or create the global progress manager"""
    global _progress_manager
    if _progress_manager is None:
        _progress_manager = ProgressManager(main_window)
    return _progress_manager

def show_progress(main_window, value: int = 0, text: str = "Working...", auto_reset_ms: int = 30000):
    """Show progress - Unified function for all IMG Factory operations

    Args:
        main_window: Main window instance
        value: Progress value (0-100)
        text: Status message
        auto_reset_ms: Auto-reset timeout in milliseconds (0 = disabled)
    """
    manager = get_progress_manager(main_window)
    manager.show_progress(value, text, auto_reset_ms)

def update_progress(main_window, value: int, text: str = None):
    """Update progress - Unified function for all IMG Factory operations

    Args:
        main_window: Main window instance
        value: Progress value (0-100)
        text: Optional new status message
    """
    manager = get_progress_manager(main_window)
    manager.update_progress(value, text)

def hide_progress(main_window, final_text: str = "Ready"):
    """Hide progress - Unified function for all IMG Factory operations

    Args:
        main_window: Main window instance
        final_text: Final status message to display
    """
    manager = get_progress_manager(main_window)
    manager.hide_progress(final_text)

def reset_progress(main_window):
    """Reset progress to ready state - Unified function

    Args:
        main_window: Main window instance
    """
    manager = get_progress_manager(main_window)
    manager.reset_progress()

def integrate_progress_system(main_window) -> bool:
    """Integrate unified progress system into main window

    Args:
        main_window: Main window instance

    Returns:
        bool: True if integration successful
    """
    try:
        # Create progress manager
        manager = get_progress_manager(main_window)

        # Add convenience methods to main window
        main_window.show_progress_unified = lambda value=0, text="Working...", auto_reset=30000: show_progress(main_window, value, text, auto_reset)
        main_window.update_progress_unified = lambda value, text=None: update_progress(main_window, value, text)
        main_window.hide_progress_unified = lambda final_text="Ready": hide_progress(main_window, final_text)
        main_window.reset_progress_unified = lambda: reset_progress(main_window)

        # Store manager reference
        main_window._progress_manager = manager

        if hasattr(main_window, 'log_message'):
            main_window.log_message("Unified progress system integrated")

        return True

    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Progress system integration failed: {str(e)}")
        return False

def refresh_after_import(main_window) -> None:
    try:
        if hasattr(main_window, 'refresh_current_tab_data'):
            main_window.refresh_current_tab_data()
        elif hasattr(main_window, 'refresh_table'):
            main_window.refresh_table()
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Refresh failed: {str(e)}")


# Export functions
__all__ = [
    'ProgressManager',
    'show_progress',
    'update_progress',
    'hide_progress',
    'reset_progress',
    'get_progress_manager',
    'integrate_progress_system'
]
