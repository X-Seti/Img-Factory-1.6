#this belongs in gui/ log_panel.py - Version: 3
# X-Seti - JULY04 2025 - IMG Factory 1.5 - Enhanced Log Panel with Copy Button

#!/usr/bin/env python3
"""
IMG Factory Log Panel - Activity Log and Message Display
Handles the activity log, error messages, and status updates
"""

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QTextEdit, QGroupBox, 
    QPushButton, QComboBox, QLabel, QCheckBox, QScrollBar
)
from PyQt6.QtCore import Qt, QTimer, pyqtSignal
from PyQt6.QtGui import QTextCursor, QFont, QColor
from datetime import datetime
from enum import Enum


class LogLevel(Enum):
    """Log message levels"""
    DEBUG = "DEBUG"
    INFO = "INFO"
    WARNING = "WARNING"  
    ERROR = "ERROR"
    SUCCESS = "SUCCESS"


class LogMessage:
    """Container for log messages"""
    
    def __init__(self, message, level=LogLevel.INFO, timestamp=None):
        self.message = message
        self.level = level
        self.timestamp = timestamp or datetime.now()
    
    def __str__(self):
        time_str = self.timestamp.strftime("%H:%M:%S")
        return f"[{time_str}] {self.level.value}: {self.message}"


class IMGFactoryLogWidget(QTextEdit):
    """Custom log widget with formatting and filtering"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self._setup_widget()
        self._messages = []
        self._filtered_levels = set()
        self._max_messages = 1000
    
    def _setup_widget(self):
        """Setup widget properties"""
        self.setReadOnly(True)
        self.setMaximumHeight(200)
        self.setMinimumHeight(100)
        
        # Font setup
        font = QFont("Consolas", 9)
        font.setStyleHint(QFont.StyleHint.Monospace)
        self.setFont(font)
        
        # Styling
        self.setStyleSheet("""
            QTextEdit {
                background-color: #1e1e1e;
                color: #faaaff;
                border: 1px solid #444444;
                border-radius: 4px;
                padding: 4px;
            }
        """)
    
    def add_message(self, message, level=LogLevel.INFO):
        """Add a message to the log"""
        log_msg = LogMessage(message, level)
        self._messages.append(log_msg)
        
        # Limit message history
        if len(self._messages) > self._max_messages:
            self._messages = self._messages[-self._max_messages:]
        
        # Add to display if not filtered
        if level not in self._filtered_levels:
            self._append_formatted_message(log_msg)
    
    def _append_formatted_message(self, log_msg):
        """Append formatted message to display"""
        # Get color for log level
        color = self._get_level_color(log_msg.level)
        
        # Format timestamp
        time_str = log_msg.timestamp.strftime("%H:%M:%S")
        
        # Create formatted HTML
        html = f"""
        <span style="color: #888888;">[{time_str}]</span>
        <span style="color: {color}; font-weight: bold;">{log_msg.level.value}:</span>
        <span style="color: #ffffff;">{log_msg.message}</span>
        """
        
        # Append to end and scroll
        cursor = self.textCursor()
        cursor.movePosition(QTextCursor.MoveOperation.End)
        cursor.insertHtml(html + "<br>")
        
        # Auto-scroll to bottom
        scrollbar = self.verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum())
    
    def _get_level_color(self, level):
        """Get color for log level"""
        colors = {
            LogLevel.DEBUG: "#888888",
            LogLevel.INFO: "#87CEEB",
            LogLevel.WARNING: "#FFD700",
            LogLevel.ERROR: "#FF6B6B",
            LogLevel.SUCCESS: "#98FB98"
        }
        return colors.get(level, "#ffffff")
    
    def set_level_filter(self, levels_to_hide):
        """Set which log levels to hide"""
        self._filtered_levels = set(levels_to_hide)
        self._refresh_display()
    
    def _refresh_display(self):
        """Refresh the display with current filter"""
        self.clear()
        for msg in self._messages:
            if msg.level not in self._filtered_levels:
                self._append_formatted_message(msg)
    
    def clear_log(self):
        """Clear all messages"""
        self._messages.clear()
        self.clear()
    
    def get_last_lines(self, count=2):
        """Get the last N lines of log messages"""
        if not self._messages:
            return ""
        
        # Get last N messages
        last_messages = self._messages[-count:] if len(self._messages) >= count else self._messages
        
        # Format as text
        lines = []
        for msg in last_messages:
            time_str = msg.timestamp.strftime("%H:%M:%S")
            lines.append(f"[{time_str}] {msg.level.value}: {msg.message}")
        
        return "\n".join(lines)

    def copy_last_lines_to_clipboard(self, count=2):
        """Copy last N lines to clipboard"""
        from PyQt6.QtWidgets import QApplication
        
        text = self.get_last_lines(count)
        if text:
            QApplication.clipboard().setText(text)
            return True
        return False
    
    def save_log(self, filename):
        """Save log to file"""
        try:
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(f"IMG Factory Log Export - {datetime.now()}\n")
                f.write("=" * 50 + "\n\n")
                
                for msg in self._messages:
                    f.write(str(msg) + "\n")
                
            return True
        except Exception as e:
            print(f"Failed to save log: {e}")
            return False


class LogControlPanel(QWidget):
    """Control panel for log filtering and management"""
    
    level_filter_changed = pyqtSignal(list)
    clear_requested = pyqtSignal()
    copy_last_lines_requested = pyqtSignal()
    save_requested = pyqtSignal()
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self._create_ui()
    
    def _create_ui(self):
        """Create control panel UI"""
        layout = QHBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(5)
        
        # Level filter checkboxes
        layout.addWidget(QLabel("Show:"))
        
        self.level_checkboxes = {}
        for level in LogLevel:
            checkbox = QCheckBox(level.value)
            checkbox.setChecked(True)
            checkbox.toggled.connect(self._on_filter_changed)
            self.level_checkboxes[level] = checkbox
            layout.addWidget(checkbox)
        
        layout.addStretch()
        
        # Control buttons
        clear_btn = QPushButton("🗑️ Clear")
        clear_btn.setToolTip("Clear all log messages")
        clear_btn.setMaximumWidth(80)
        clear_btn.clicked.connect(self.clear_requested.emit)
        layout.addWidget(clear_btn)
        
        # Copy Last 2 Lines button
        copy_btn = QPushButton("📋 Copy")
        copy_btn.setToolTip("Copy last 2 lines to clipboard")
        copy_btn.setMaximumWidth(80)
        copy_btn.clicked.connect(self.copy_last_lines_requested.emit)
        layout.addWidget(copy_btn)
        
        save_btn = QPushButton("💾 Save")
        save_btn.setToolTip("Save log to file")
        save_btn.setMaximumWidth(80)
        save_btn.clicked.connect(self.save_requested.emit)
        layout.addWidget(save_btn)
    
    def _on_filter_changed(self):
        """Handle filter checkbox changes"""
        hidden_levels = []
        for level, checkbox in self.level_checkboxes.items():
            if not checkbox.isChecked():
                hidden_levels.append(level)
        
        self.level_filter_changed.emit(hidden_levels)


class StatusMessageManager:
    """Manages temporary status messages that auto-clear"""
    
    def __init__(self, log_widget):
        self.log_widget = log_widget
        self._status_timer = QTimer()
        self._status_timer.setSingleShot(True)
        self._status_timer.timeout.connect(self._clear_status)
        self._current_status = None
    
    def show_status(self, message, duration=3000):
        """Show a temporary status message"""
        self._current_status = message
        self.log_widget.add_message(f"Status: {message}", LogLevel.INFO)
        
        if duration > 0:
            self._status_timer.start(duration)
    
    def _clear_status(self):
        """Clear current status message"""
        if self._current_status:
            self.log_widget.add_message(f"Status cleared: {self._current_status}", LogLevel.DEBUG)
            self._current_status = None


class ProgressLogger:
    """Helper for logging progress of long operations"""
    
    def __init__(self, log_widget, operation_name):
        self.log_widget = log_widget
        self.operation_name = operation_name
        self.start_time = datetime.now()
        self.last_progress = 0
    
    def __enter__(self):
        self.log_widget.add_message(f"Starting {self.operation_name}...", LogLevel.INFO)
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        duration = datetime.now() - self.start_time
        if exc_type is None:
            self.log_widget.add_message(
                f"Completed {self.operation_name} in {duration.total_seconds():.1f}s", 
                LogLevel.SUCCESS
            )
        else:
            self.log_widget.add_message(
                f"Failed {self.operation_name}: {exc_val}", 
                LogLevel.ERROR
            )
    
    def progress(self, current, total, item_name=""):
        """Log progress update"""
        percentage = int((current / total) * 100) if total > 0 else 0
        
        # Only log every 10% to avoid spam
        if percentage >= self.last_progress + 10:
            self.last_progress = percentage
            msg = f"{self.operation_name} progress: {percentage}% ({current}/{total})"
            if item_name:
                msg += f" - {item_name}"
            self.log_widget.add_message(msg, LogLevel.INFO)


def create_log_panel(main_window):
    """Create the complete log panel"""
    group = QGroupBox("📜 Activity Log")
    layout = QVBoxLayout(group)
    layout.setSpacing(5)
    
    # Create log widget
    main_window.log_widget = IMGFactoryLogWidget()
    layout.addWidget(main_window.log_widget)
    
    # Create control panel
    control_panel = LogControlPanel()
    layout.addWidget(control_panel)
    
    # Connect signals
    control_panel.level_filter_changed.connect(main_window.log_widget.set_level_filter)
    control_panel.clear_requested.connect(main_window.log_widget.clear_log)
    control_panel.save_requested.connect(lambda: _save_log_dialog(main_window))
    
    # Connect copy signal
    def copy_last_lines():
        if main_window.log_widget.copy_last_lines_to_clipboard(2):
            main_window.log_widget.add_message("📋 Last 2 lines copied to clipboard", LogLevel.INFO)
        else:
            main_window.log_widget.add_message("❌ No lines to copy", LogLevel.WARNING)
    
    control_panel.copy_last_lines_requested.connect(copy_last_lines)
    
    # Add convenience methods to main window
    main_window.log_message = lambda msg: main_window.log_widget.add_message(msg, LogLevel.INFO)
    main_window.log_warning = lambda msg: main_window.log_widget.add_message(msg, LogLevel.WARNING)
    main_window.log_error = lambda msg: main_window.log_widget.add_message(msg, LogLevel.ERROR)
    main_window.log_success = lambda msg: main_window.log_widget.add_message(msg, LogLevel.SUCCESS)
    main_window.log_debug = lambda msg: main_window.log_widget.add_message(msg, LogLevel.DEBUG)
    
    # Add initial welcome message
    main_window.log_message("IMG Factory 1.5 initialized")
    main_window.log_message("Ready to work with IMG archives")
    
    return group


def _save_log_dialog(main_window):
    """Show save log dialog"""
    from PyQt6.QtWidgets import QFileDialog
    
    filename, _ = QFileDialog.getSaveFileName(
        main_window,
        "Save Activity Log",
        f"imgfactory_log_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt",
        "Text Files (*.txt);;All Files (*)"
    )
    
    if filename:
        if main_window.log_widget.save_log(filename):
            main_window.log_success(f"Log saved to: {filename}")
        else:
            main_window.log_error("Failed to save log file")


# Helper functions for integration
def setup_logging_for_main_window(main_window):
    """Setup logging methods for main window"""
    if not hasattr(main_window, 'log_widget'):
        # Create a simple log widget if none exists
        main_window.log_widget = IMGFactoryLogWidget()
    
    # Add logging methods
    main_window.log_message = lambda msg: main_window.log_widget.add_message(msg, LogLevel.INFO)
    main_window.log_warning = lambda msg: main_window.log_widget.add_message(msg, LogLevel.WARNING) 
    main_window.log_error = lambda msg: main_window.log_widget.add_message(msg, LogLevel.ERROR)
    main_window.log_success = lambda msg: main_window.log_widget.add_message(msg, LogLevel.SUCCESS)
    main_window.log_debug = lambda msg: main_window.log_widget.add_message(msg, LogLevel.DEBUG)
    
    # Status message manager
    main_window.status_manager = StatusMessageManager(main_window.log_widget)
    main_window.show_status = main_window.status_manager.show_status
    
    # Progress logger factory
    main_window.create_progress_logger = lambda name: ProgressLogger(main_window.log_widget, name)


# Integration with existing log systems
def integrate_with_existing_logging(main_window):
    """Integrate with any existing logging in the main window"""
    # If main window already has log methods, preserve them
    existing_methods = ['log_message', 'log_error', 'log_warning']
    
    for method_name in existing_methods:
        if hasattr(main_window, method_name):
            # Backup existing method
            setattr(main_window, f'_original_{method_name}', getattr(main_window, method_name))
    
    # Setup new logging
    setup_logging_for_main_window(main_window)
