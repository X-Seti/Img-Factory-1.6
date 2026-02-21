#this belongs in gui/status_bar.py - Version: 15
# X-Seti - Aug06 2025 - IMG Factory 1.5 - Universal Theme Status Bar
# UPDATED: Removed StatusBarTheme class, now uses AppSettings universal theme system
# PRESERVES: 100% of existing functionality - all widgets, methods, and features

"""
IMG Factory Status Bar - Universal Theme Integration
Status bar using AppSettings universal theme system instead of separate StatusBarTheme
Preserves ALL existing functionality while using universal theme colors
"""

from PyQt6.QtWidgets import (
    QStatusBar, QLabel, QProgressBar, QPushButton, QWidget, 
    QHBoxLayout, QSizePolicy
)
from PyQt6.QtCore import Qt, QTimer, pyqtSignal
from PyQt6.QtGui import QPixmap, QIcon
from datetime import datetime

##Methods list -
# _add_status_methods
# add_col_status_info
# apply_universal_status_theme
# copy_status_to_clipboard
# create_status_bar
# create_status_context_menu
# format_operation_time
# integrate_with_existing_status_bar

def create_enhanced_status_bar(main_window):
    """Create enhanced status bar with universal theme support - ALIAS for compatibility"""
    return create_status_bar(main_window)


def create_status_bar(main_window):
    """Create status bar with universal theme support"""
    # Create status bar
    status_bar = QStatusBar()
    main_window.setStatusBar(status_bar)
    
    # Create status manager
    main_window.status_manager = StatusBarManager(status_bar)
    
    # Main status message (left side)  
    main_window.status_label = QLabel("Ready")
    status_bar.addWidget(main_window.status_label)
    
    # Create progress widget
    main_window.progress_widget = ProgressWidget()
    status_bar.addWidget(main_window.progress_widget)
    
    # Create selection status widget
    main_window.selection_status_widget = SelectionStatusWidget()
    status_bar.addWidget(main_window.selection_status_widget)
    
    # Create operation status widget
    main_window.operation_status_widget = OperationStatusWidget()
    status_bar.addPermanentWidget(main_window.operation_status_widget)
    
    # Create IMG status widget (right side)
    main_window.img_status_widget = IMGStatusWidget()
    status_bar.addPermanentWidget(main_window.img_status_widget)
    
    # Add convenience methods to main window
    _add_status_methods(main_window)
    
    # Apply universal theme if app_settings available
    if hasattr(main_window, 'app_settings'):
        apply_universal_status_theme(status_bar, main_window.app_settings)
    
    return status_bar


def apply_universal_status_theme(status_bar, app_settings):
    """Apply universal theme to status bar using AppSettings colors"""
    try:
        # Get theme colors from AppSettings
        colors = app_settings.get_theme_colors()
        
        if not colors:
            # Fallback to basic styling if no theme colors available
            status_bar.setStyleSheet("""
                QStatusBar {
                    background-color: #f0f0f0;
                    border-top: 1px solid #d0d0d0;
                    color: #333333;
                }
                QStatusBar::item {
                    border: none;
                }
            """)
            return
        
        # Build stylesheet using universal theme colors
        stylesheet = f"""
            QStatusBar {{
                background-color: {colors.get('bg_secondary', '#f8f9fa')};
                border-top: 1px solid {colors.get('border', '#dee2e6')};
                color: {colors.get('text_secondary', '#495057')};
                font-size: 9pt;
            }}
            
            QStatusBar::item {{
                border: none;
                margin: 2px;
            }}
            
            QLabel {{
                color: {colors.get('text_secondary', '#495057')};
                padding: 2px 4px;
            }}
            
            QProgressBar {{
                border: 1px solid {colors.get('border', '#dee2e6')};
                border-radius: 2px;
                background-color: {colors.get('bg_tertiary', '#e9ecef')};
                text-align: center;
                min-width: 100px;
                max-height: 16px;
            }}
            
            QProgressBar::chunk {{
                background-color: {colors.get('accent_primary', '#1976d2')};
                border-radius: 1px;
            }}
            
            QPushButton {{
                background-color: {colors.get('button_normal', '#e0e0e0')};
                border: 1px solid {colors.get('border', '#cccccc')};
                border-radius: 2px;
                padding: 2px 6px;
                color: {colors.get('text_primary', '#000000')};
                font-size: 8pt;
            }}
            
            QPushButton:hover {{
                background-color: {colors.get('button_hover', '#d0d0d0')};
            }}
            
            QPushButton:pressed {{
                background-color: {colors.get('button_pressed', '#c0c0c0')};
            }}
        """
        
        status_bar.setStyleSheet(stylesheet)
        
    except Exception as e:
        print(f"Warning: Failed to apply universal theme to status bar: {e}")
        # Apply basic fallback styling
        status_bar.setStyleSheet("""
            QStatusBar {
                background-color: #f0f0f0;
                border-top: 1px solid #d0d0d0;
                color: #333333;
            }
            QStatusBar::item {
                border: none;
            }
        """)


def _add_status_methods(main_window):
    """Add status bar convenience methods to main window - PRESERVED 100%"""
    
    def show_status(message, timeout=0):
        """Show status message"""
        main_window.status_manager.show_message(message, timeout)
    
    def show_permanent_status(message):
        """Show permanent status message"""
        main_window.status_manager.show_permanent_message(message)
    
    def show_progress(text="Working...", minimum=0, maximum=100):
        """Show progress bar"""
        main_window.progress_widget.show_progress(text, minimum, maximum)
    
    def update_progress(value, text=None):
        """Update progress"""
        main_window.progress_widget.update_progress(value, text)
    
    def hide_progress():
        """Hide progress bar"""
        main_window.progress_widget.hide_progress()
    
    def update_img_status(img_file=None, filename="", entry_count=0, file_size=0, version="Unknown"):
        """Update IMG file status"""
        main_window.img_status_widget.update_img_status(img_file, filename, entry_count, file_size, version)
    
    def set_ready_status():
        """Set status to ready"""
        show_permanent_status("Ready")
        main_window.img_status_widget._reset_status()
    
    def set_operation_status(status, message=""):
        """Set operation status - PRESERVED CRITICAL FUNCTION"""
        if status == "idle":
            main_window.operation_status_widget.set_idle()
        elif status == "working":
            main_window.operation_status_widget.set_working(message)
        elif status == "success":
            main_window.operation_status_widget.set_success(message)
        elif status == "error":
            main_window.operation_status_widget.set_error(message)
    
    # Attach ALL methods to main window
    main_window.show_status = show_status
    main_window.show_permanent_status = show_permanent_status
    main_window.show_progress = show_progress
    main_window.update_progress = update_progress
    main_window.hide_progress = hide_progress
    main_window.update_img_status = update_img_status
    main_window.set_ready_status = set_ready_status
    main_window.set_operation_status = set_operation_status


class StatusBarManager:
    """Manages status bar components and updates - PRESERVED 100%"""
    
    def __init__(self, status_bar):
        self.status_bar = status_bar
        self._permanent_status = "Ready"
        self._status_timer = QTimer()
        self._status_timer.setSingleShot(True)
        self._status_timer.timeout.connect(self._clear_temporary_status)
    
    def show_message(self, message, timeout=0):
        """Show a message in the status bar"""
        self.status_bar.showMessage(message, timeout)
        
        if timeout > 0:
            self._status_timer.start(timeout)
    
    def show_permanent_message(self, message):
        """Show a permanent message"""
        self._permanent_status = message
        self.status_bar.showMessage(message)
    
    def _clear_temporary_status(self):
        """Clear temporary status and restore permanent status"""
        self.status_bar.showMessage(self._permanent_status)


class ProgressWidget(QWidget):
    """Progress bar widget for status bar with cancel button - PRESERVED 100%"""
    
    cancel_requested = pyqtSignal()
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self._create_ui()
        self.hide()
    
    def _create_ui(self):
        """Create progress UI"""
        layout = QHBoxLayout(self)
        layout.setContentsMargins(2, 2, 2, 2)
        layout.setSpacing(5)
        
        # Progress label
        self.progress_label = QLabel("Working...")
        self.progress_label.setMinimumWidth(100)
        layout.addWidget(self.progress_label)
        
        # Progress bar
        self.progress_bar = QProgressBar()
        self.progress_bar.setMinimumWidth(200)
        self.progress_bar.setMaximumHeight(16)
        layout.addWidget(self.progress_bar)
        
        # Cancel button
        self.cancel_btn = QPushButton("X")
        self.cancel_btn.setMaximumWidth(20)
        self.cancel_btn.setMaximumHeight(16)
        self.cancel_btn.setToolTip("Cancel operation")
        self.cancel_btn.clicked.connect(self.cancel_requested.emit)
        layout.addWidget(self.cancel_btn)
    
    def show_progress(self, text="Working...", minimum=0, maximum=100):
        """Show progress bar"""
        self.progress_label.setText(text)
        self.progress_bar.setRange(minimum, maximum)
        self.progress_bar.setValue(minimum)
        self.show()
    
    def update_progress(self, value, text=None):
        """Update progress"""
        self.progress_bar.setValue(value)
        if text:
            self.progress_label.setText(text)
    
    def hide_progress(self):
        """Hide progress bar"""
        self.hide()
        self.progress_bar.setValue(0)
        self.progress_label.setText("Working...")


class SelectionStatusWidget(QWidget):
    """Widget showing current selection information - PRESERVED 100%"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self._create_ui()
        self.update_selection(0, 0)
    
    def _create_ui(self):
        """Create selection status UI"""
        layout = QHBoxLayout(self)
        layout.setContentsMargins(5, 2, 5, 2)
        layout.setSpacing(5)
        
        # Selection icon
        
        
        # Selection text
        self.selection_label = QLabel("0 of 0 selected")
        layout.addWidget(self.selection_label)
    
    def update_selection(self, selected_count, total_count):
        """Update selection display"""
        self.selection_label.setText(f"{selected_count} of {total_count} selected")


class OperationStatusWidget(QWidget):
    """Widget showing current operation status - PRESERVED 100%"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self._create_ui()
        self.set_idle()
    
    def _create_ui(self):
        """Create operation status UI"""
        layout = QHBoxLayout(self)
        layout.setContentsMargins(5, 2, 5, 2)
        layout.setSpacing(5)

        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        self._svg = SVGIconFactory

        # Status icon button (flat, non-interactive)
        self.status_icon = QPushButton()
        self.status_icon.setFlat(True)
        self.status_icon.setFixedSize(16, 16)
        self.status_icon.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        layout.addWidget(self.status_icon)

        # Status text
        self.status_label = QLabel("Idle")
        layout.addWidget(self.status_label)

    def set_idle(self):
        """Set idle status"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        self.status_icon.setIcon(SVGIconFactory.info_icon(14))
        self.status_label.setText("Idle")

    def set_working(self, message="Working"):
        """Set working status"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        self.status_icon.setIcon(SVGIconFactory.reset_icon(14))
        self.status_label.setText(message)

    def set_success(self, message="Success"):
        """Set success status"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        self.status_icon.setIcon(SVGIconFactory.get_checkmark_icon(14))
        self.status_label.setText(message)
        QTimer.singleShot(3000, self.set_idle)

    def set_error(self, message="Error"):
        """Set error status"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        self.status_icon.setIcon(SVGIconFactory.close_icon(14))
        self.status_label.setText(message)
        QTimer.singleShot(5000, self.set_idle)


class IMGStatusWidget(QWidget):
    """Widget showing IMG file status information - PRESERVED 100%"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self._create_ui()
        self._reset_status()
    
    def _create_ui(self):
        """Create IMG status UI"""
        layout = QHBoxLayout(self)
        layout.setContentsMargins(5, 2, 5, 2)
        layout.setSpacing(8)

        # File icon (SVG)
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        self.file_icon = QPushButton()
        self.file_icon.setFlat(True)
        self.file_icon.setFixedSize(16, 16)
        self.file_icon.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self.file_icon.setIcon(SVGIconFactory.file_icon(14))
        layout.addWidget(self.file_icon)

        # File name
        self.file_label = QLabel("No File")
        layout.addWidget(self.file_label)
        
        # Separator
        layout.addWidget(QLabel("|"))
        
        # Entry count
        self.entries_label = QLabel("Entries: 0")
        layout.addWidget(self.entries_label)
        
        # Separator
        layout.addWidget(QLabel("|"))
        
        # File size
        self.size_label = QLabel("Size: 0 B")
        layout.addWidget(self.size_label)
        
        # Separator
        layout.addWidget(QLabel("|"))
        
        # Version
        self.version_label = QLabel("Version: Unknown")
        layout.addWidget(self.version_label)
    
    def update_img_status(self, img_file=None, filename="", entry_count=0, file_size=0, version="Unknown"):
        """Update IMG file status display"""
        if img_file:
            # Extract info from IMG file object
            filename = filename or (img_file.file_path if hasattr(img_file, 'file_path') else "Unknown")
            entry_count = len(img_file.entries) if hasattr(img_file, 'entries') and img_file.entries else 0
            file_size = img_file.file_size if hasattr(img_file, 'file_size') else 0
            version = str(img_file.version) if hasattr(img_file, 'version') else "Unknown"
        
        # Update display
        if filename:
            import os
            short_name = os.path.basename(filename) if filename else "No File"
            self.file_label.setText(short_name)
        else:
            self.file_label.setText("No File")
        
        self.entries_label.setText(f"Entries: {entry_count}")
        self.size_label.setText(f"Size: {self._format_file_size(file_size)}")
        self.version_label.setText(f"Version: {version}")
    
    def _reset_status(self):
        """Reset to default status"""
        self.file_label.setText("No File")
        self.entries_label.setText("Entries: 0")
        self.size_label.setText("Size: 0 B")
        self.version_label.setText("Version: Unknown")
    
    def _format_file_size(self, size_bytes):
        """Format file size for display"""
        if size_bytes == 0:
            return "0 B"
        elif size_bytes < 1024:
            return f"{size_bytes} B"
        elif size_bytes < 1024**2:
            return f"{size_bytes/1024:.1f} KB"
        elif size_bytes < 1024**3:
            return f"{size_bytes/(1024**2):.1f} MB"
        else:
            return f"{size_bytes/(1024**3):.1f} GB"


def add_col_status_info(img_factory_instance):
    """Add COL-specific status information - PRESERVED 100%"""
    # Store original status update method
    if hasattr(img_factory_instance, 'update_status_bar'):
        original_update_status = img_factory_instance.update_status_bar

        def enhanced_update_status_bar():
            # Call original method
            original_update_status()

            # Add COL-specific info
            if hasattr(img_factory_instance, 'current_img') and img_factory_instance.current_img:
                col_count = 0
                for entry in img_factory_instance.current_img.entries:
                    if entry.name.lower().endswith('.col'):
                        col_count += 1

                if col_count > 0:
                    current_status = img_factory_instance.statusBar().currentMessage()
                    enhanced_status = f"{current_status} | COL Files: {col_count}"
                    img_factory_instance.statusBar().showMessage(enhanced_status)

        # Replace the method
        img_factory_instance.update_status_bar = enhanced_update_status_bar


# Integration helpers - PRESERVED 100%
def integrate_with_existing_status_bar(main_window):
    """Integrate with existing status bar if present"""
    if hasattr(main_window, 'statusBar') and main_window.statusBar():
        # Use existing status bar
        existing_bar = main_window.statusBar()
        
        # Add our components to existing bar
        main_window.status_manager = StatusBarManager(existing_bar)
        
        # Add our widgets if space available
        main_window.progress_widget = ProgressWidget()
        existing_bar.addWidget(main_window.progress_widget)
        
        main_window.img_status_widget = IMGStatusWidget()
        existing_bar.addPermanentWidget(main_window.img_status_widget)
        
        # Add methods
        _add_status_methods(main_window)
        
        # Apply universal theme if available
        if hasattr(main_window, 'app_settings'):
            apply_universal_status_theme(existing_bar, main_window.app_settings)
        
        return existing_bar
    else:
        # Create new status bar
        return create_status_bar(main_window)


# Utility functions - PRESERVED 100%
def format_operation_time(seconds):
    """Format operation time for display"""
    if seconds < 1:
        return f"{seconds*1000:.0f}ms"
    elif seconds < 60:
        return f"{seconds:.1f}s"
    else:
        minutes = int(seconds // 60)
        remaining_seconds = seconds % 60
        return f"{minutes}m {remaining_seconds:.1f}s"


def create_status_context_menu(main_window):
    """Create context menu for status bar"""
    from PyQt6.QtWidgets import QMenu
    
    def show_context_menu(position):
        menu = QMenu(main_window)
        
        # Copy status action
        copy_action = menu.addAction("📋 Copy Status")
        copy_action.triggered.connect(lambda: copy_status_to_clipboard(main_window))
        
        menu.addSeparator()
        
        # Clear status action
        clear_action = menu.addAction("🗑️ Clear Status")
        clear_action.triggered.connect(lambda: main_window.show_permanent_status("Ready"))

        # Show log action
        if hasattr(main_window, 'log_widget'):
            log_action = menu.addAction("📜 Show Log")
        
        menu.exec(main_window.statusBar().mapToGlobal(position))
    
    # Connect context menu
    main_window.statusBar().setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
    main_window.statusBar().customContextMenuRequested.connect(show_context_menu)


def copy_status_to_clipboard(main_window):
    """Copy current status information to clipboard"""
    from PyQt6.QtWidgets import QApplication
    
    status_info = []
    
    # Main status
    if hasattr(main_window, 'statusBar') and main_window.statusBar():
        status_info.append(f"Status: {main_window.statusBar().currentMessage()}")
    
    # IMG status
    if hasattr(main_window, 'img_status_widget'):
        widget = main_window.img_status_widget
        status_info.extend([
            f"File: {widget.file_label.text()}",
            f"{widget.entries_label.text()}",
            f"{widget.size_label.text()}",
            f"{widget.version_label.text()}"
        ])
    
    # Selection status
    if hasattr(main_window, 'selection_status_widget'):
        status_info.append(f"Selection: {main_window.selection_status_widget.selection_label.text()}")
    
    clipboard_text = "\n".join(status_info)
    QApplication.clipboard().setText(clipboard_text)
    
    if hasattr(main_window, 'show_status'):
        main_window.show_status("Status copied to clipboard", 2000)