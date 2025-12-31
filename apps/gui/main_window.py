#this belongs in gui/ main_window.py - Version: 58
# X-Seti - July12 2025 - Img Factory 1.5
# Credit MexUK 2007 Img Factory 1.2

#!/usr/bin/env python3
"""
IMG Factory Main Window - Clean Implementation
Main window setup and layout management
"""
import sys
from typing import Optional
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, 
    QApplication, QMenuBar, QStatusBar, QMainWindow
)
from PyQt6.QtCore import Qt, pyqtSignal
from PyQt6.QtGui import QAction, QKeySequence

# Import consolidated GUI components
try:
    from .panel_manager import PanelManager  # Fixed import location
    from .tear_off import TearOffPanelManager, setup_tear_off_system
    from .log_panel import create_log_panel, setup_logging_for_main_window
    from .status_bar import create_status_bar
except ImportError as e:
    print(f"Import error in main_window: {e}")
    # Fallback - create minimal stubs
    def create_right_panel_with_pastel_buttons(main_window):
        return QWidget()
    def create_control_panel(main_window):
        return QWidget()
    def create_log_panel(main_window):
        return QWidget()
    def create_status_bar(main_window):
        return QStatusBar()
    class PanelManager:
        def __init__(self, main_window): pass
    class TearOffPanelManager:
        def __init__(self, main_window): pass

# Add an undo manager for handling undo/redo operations
class UndoManager:
    def __init__(self, main_window):
        self.main_window = main_window
        self.undo_stack = []
        self.redo_stack = []
        
    def push_command(self, command, description=""):
        """Push a command to the undo stack"""
        self.undo_stack.append((command, description))
        self.redo_stack.clear()  # Clear redo stack when new command is added
        
    def undo(self):
        """Execute the last command from the undo stack"""
        if self.undo_stack:
            command, description = self.undo_stack.pop()
            self.redo_stack.append((command, description))
            
            # Execute the undo command
            if callable(command):
                command()
                
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Undo: {description}")
        else:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("No operations to undo")
    
    def redo(self):
        """Execute the last command from the redo stack"""
        if self.redo_stack:
            command, description = self.redo_stack.pop()
            self.undo_stack.append((command, description))
            
            # Execute the redo command
            if callable(command):
                command()
                
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Redo: {description}")
        else:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("No operations to redo")


class IMGFactoryMainWindow(QMainWindow):
    """
    Clean IMG Factory main window implementation
    Handles layout and basic window management
    """
    
    # Signals for communication
    file_opened = pyqtSignal(str)  # file_path
    file_closed = pyqtSignal()
    entries_changed = pyqtSignal()  # img entries modified
    selection_changed = pyqtSignal()  # table selection changed
    
    def __init__(self, parent=None):
        super().__init__(parent)
        
        # Initialize components
        self.central_widget = None
        self.left_panel = None
        self.right_panel = None
        self.log_panel = None
        self.status_bar = None
        self.splitter = None
        
        # Initialize undo manager
        self.undo_manager = UndoManager(self)
        
        # Panel managers
        self.panel_manager = None
        self.tearoff_manager = None
        
        # Setup basic window
        self.setup_window()
        
        # Create layout
        self.setup_layout()
        
        # Setup panels
        self.setup_panels()
        
        # Initialize UI mode based on settings
        self.initialize_ui_mode()
        
        # Show window
        self.show()
    

    def initialize_ui_mode(self):
        """Initialize UI mode based on settings after setup is complete"""
        try:
            # Check if app_settings is available and apply UI mode
            if hasattr(self, 'app_settings') and hasattr(self.app_settings, 'current_settings'):
                ui_mode = self.app_settings.current_settings.get('ui_mode', 'system')
                show_toolbar = self.app_settings.current_settings.get('show_toolbar', True)
                show_status_bar = self.app_settings.current_settings.get('show_status_bar', True)
                show_menu_bar = self.app_settings.current_settings.get('show_menu_bar', True)
                
                self.apply_ui_mode(ui_mode, show_toolbar, show_status_bar, show_menu_bar)
            else:
                # Default to system UI mode if no settings available
                self.apply_ui_mode('system', True, True, True)
        except Exception as e:
            self.log_message(f"Error initializing UI mode: {str(e)}")
            # Fallback to system UI mode
            self.apply_ui_mode('system', True, True, True)

    def save_img_entry(self):
        """Save IMG entry - placeholder method"""
        try:
            # This would be implemented based on your specific IMG saving logic
            self.log_message("Save IMG entry function called")
            # Add your actual save logic here
        except Exception as e:
            self.log_message(f"Error saving IMG entry: {str(e)}")

    def export_selected(self):
        """Export selected entries - placeholder method"""
        try:
            self.log_message("Export selected function called")
            # Add your actual export logic here
        except Exception as e:
            self.log_message(f"Error exporting selected: {str(e)}")

    def export_via(self):
        """Export via function - placeholder method"""
        try:
            self.log_message("Export via function called")
            # Add your actual export via logic here
        except Exception as e:
            self.log_message(f"Error exporting via: {str(e)}")

    def quick_export(self):
        """Quick export function - placeholder method"""
        try:
            self.log_message("Quick export function called")
            # Add your actual quick export logic here
        except Exception as e:
            self.log_message(f"Error quick exporting: {str(e)}")

    def dump_all(self):
        """Dump all entries - placeholder method"""
        try:
            self.log_message("Dump all function called")
            # Add your actual dump logic here
        except Exception as e:
            self.log_message(f"Error dumping all: {str(e)}")

    def import_files_via(self):
        """Import files via IDE or folder"""
        try:
            from apps.core.importer import import_via_function
            import_via_function(self)
        except Exception as e:
            self.log_message(f"Import via error: {str(e)}")

    def remove_via_entries(self):
        """Remove entries via IDE file"""
        try:
            from apps.core.remove import remove_via_entries_function
            remove_via_entries_function(self)
        except Exception as e:
            self.log_message(f"Remove via error: {str(e)}")

    def dump_entries(self):
        """Dump all entries"""
        try:
            from apps.core.exporter import dump_all_function
            dump_all_function(self)
        except Exception as e:
            self.log_message(f"Dump error: {str(e)}")

    def export_selected_via(self):
        """Export selected via IDE"""
        try:
            from apps.core.exporter import export_via_function
            export_via_function(self)
        except Exception as e:
            self.log_message(f"Export via error: {str(e)}")

    def quick_export_selected(self):
        """Quick export to project folder"""
        try:
            from apps.core.exporter import quick_export_function
            quick_export_function(self)
        except Exception as e:
            self.log_message(f"Quick export error: {str(e)}")

    def setup_window(self):
        """Setup basic window properties"""
        self.setWindowTitle("IMG Factory 1.5")
        self.setMinimumSize(1000, 700)
        self.resize(1400, 900)
        
        # Center window
        screen = QApplication.primaryScreen().geometry()
        size = self.geometry()
        self.move(
            (screen.width() - size.width()) // 2,
            (screen.height() - size.height()) // 2
        )
        
        # Initially set to system UI mode
        self.is_custom_ui_mode = False
        self.custom_title_bar = None
    
    def setup_layout(self):
        """Setup main window layout"""
        # Create central widget
        self.central_widget = QWidget()
        self.setCentralWidget(self.central_widget)
        
        # Create main layout
        main_layout = QHBoxLayout(self.central_widget)
        main_layout.setContentsMargins(5, 5, 5, 5)
        main_layout.setSpacing(5)
        
        # Create splitter for resizable panels
        self.splitter = QSplitter(Qt.Orientation.Horizontal)
        main_layout.addWidget(self.splitter)
        
        # Create left panel (main content area)
        self.left_panel = QWidget()
        self.left_panel.setMinimumWidth(500)
        
        # Create right panel (controls)
        self.right_panel = create_right_panel_with_pastel_buttons(self)
        self.right_panel.setMaximumWidth(300)
        self.right_panel.setMinimumWidth(250)
        
        # Add to splitter
        self.splitter.addWidget(self.left_panel)
        self.splitter.addWidget(self.right_panel)
        
        # Set splitter proportions (70% left, 30% right)
        self.splitter.setSizes([700, 300])
    
    def setup_panels(self):
        """Setup panel management system"""
        try:
            # Create panel manager
            self.panel_manager = PanelManager(self)
            
            # Create tear-off panel manager
            self.tearoff_manager = TearOffPanelManager(self)
            
            # Create status bar
            self.status_bar = create_status_bar(self)
            self.setStatusBar(self.status_bar)
            
            # Setup logging
            setup_logging_for_main_window(self)
            
            # Add context menu to the table if it exists
            if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'table'):
                from .gui_context import add_img_context_menu_to_entries_table
                add_img_context_menu_to_entries_table(self)
            
            # Set up keyboard shortcuts for undo/redo
            undo_shortcut = QKeySequence('Ctrl+Z')
            redo_shortcut = QKeySequence('Ctrl+Y')
            
            undo_action = QAction('Undo', self)
            undo_action.setShortcut(undo_shortcut)
            undo_action.triggered.connect(self.undo_manager.undo)
            self.addAction(undo_action)
            
            redo_action = QAction('Redo', self)
            redo_action.setShortcut(redo_shortcut)
            redo_action.triggered.connect(self.undo_manager.redo)
            self.addAction(redo_action)
            
            self.log_message("✅ Panel system initialized")
            
        except Exception as e:
            print(f"Error setting up panels: {e}")

    def apply_ui_mode(self, ui_mode, show_toolbar=True, show_status_bar=True, show_menu_bar=True):
        """Apply UI mode settings - custom or system UI"""
        if ui_mode == 'custom':
            self._apply_custom_ui_mode()
        else:
            self._apply_system_ui_mode()
        
        # Apply visibility settings
        if hasattr(self, 'statusBar') and callable(self.statusBar):
            status_bar = self.statusBar()
            if status_bar:
                status_bar.setVisible(show_status_bar)
        
        # For menu bar, we need to create one if it doesn't exist
        menu_bar = self.menuBar()
        if menu_bar:
            menu_bar.setVisible(show_menu_bar)

    def _apply_custom_ui_mode(self):
        """Apply custom UI mode with buttons in title bar"""
        if not self.is_custom_ui_mode:
            # Switch to custom UI mode
            self.is_custom_ui_mode = True
            self._create_custom_title_bar()
            self.log_message("Custom UI mode activated")
        else:
            # Already in custom UI mode, just update
            self._update_custom_title_bar()

    def _apply_system_ui_mode(self):
        """Apply system UI mode with standard title bar"""
        if self.is_custom_ui_mode:
            # Switch back to system UI mode
            self.is_custom_ui_mode = False
            self._remove_custom_title_bar()
            self.log_message("System UI mode activated")


    def _create_custom_title_bar(self): #vers 2
        """Create a custom title bar with buttons - COMPLETE WORKING VERSION"""
        try:
            from PyQt6.QtWidgets import QWidget, QHBoxLayout, QPushButton, QLabel
            from PyQt6.QtCore import Qt, QSize
            from apps.methods.imgfactory_svg_icons import (
                get_open_icon, get_save_icon, get_extract_icon,
                get_undo_icon, get_info_icon, get_settings_icon
            )

            # Remove standard title bar
            self.setWindowFlag(Qt.WindowType.FramelessWindowHint, True)

            # Create custom title bar widget if it doesn't exist
            if self.custom_title_bar is None:
                self.custom_title_bar = QWidget()
                self.custom_title_bar.setFixedHeight(35)
                self.custom_title_bar.setStyleSheet("""
                    QWidget {
                        background-color: #2d2d30;
                        border-bottom: 1px solid #3e3e42;
                    }
                """)

                layout = QHBoxLayout(self.custom_title_bar)
                layout.setContentsMargins(8, 2, 8, 2)
                layout.setSpacing(5)

                # Settings button
                settings_btn = QPushButton()
                settings_btn.setIcon(get_settings_icon())
                settings_btn.setIconSize(QSize(16, 16))
                settings_btn.setText("Settings")
                settings_btn.setFixedHeight(28)
                settings_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #4a7abc;
                        color: white;
                        border: none;
                        border-radius: 3px;
                        padding: 4px 10px;
                        font-size: 11px;
                    }
                    QPushButton:hover {
                        background-color: #5a8acc;
                    }
                """)
                settings_btn.clicked.connect(self._show_settings_dialog)
                layout.addWidget(settings_btn)

                # Separator
                sep1 = QLabel("|")
                sep1.setStyleSheet("color: #555;")
                layout.addWidget(sep1)

                # Title
                title_label = QLabel("Img Factory 1.6")
                title_label.setStyleSheet("color: #cccccc; font-weight: bold; font-size: 12px;")
                layout.addWidget(title_label)

                # Separator
                sep2 = QLabel("|")
                sep2.setStyleSheet("color: #555;")
                layout.addWidget(sep2)

                # Action buttons
                open_btn = QPushButton()
                open_btn.setIcon(get_open_icon())
                open_btn.setIconSize(QSize(16, 16))
                open_btn.setToolTip("Open")
                open_btn.setFixedSize(28, 28)
                open_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #3e3e42;
                        border: none;
                        border-radius: 3px;
                    }
                    QPushButton:hover {
                        background-color: #505050;
                    }
                """)
                layout.addWidget(open_btn)

                save_btn = QPushButton()
                save_btn.setIcon(get_save_icon())
                save_btn.setIconSize(QSize(16, 16))
                save_btn.setToolTip("Save")
                save_btn.setFixedSize(28, 28)
                save_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #3e3e42;
                        border: none;
                        border-radius: 3px;
                    }
                    QPushButton:hover {
                        background-color: #505050;
                    }
                """)
                layout.addWidget(save_btn)

                extract_btn = QPushButton()
                extract_btn.setIcon(get_extract_icon())
                extract_btn.setIconSize(QSize(16, 16))
                extract_btn.setToolTip("Extract")
                extract_btn.setFixedSize(28, 28)
                extract_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #3e3e42;
                        border: none;
                        border-radius: 3px;
                    }
                    QPushButton:hover {
                        background-color: #505050;
                    }
                """)
                layout.addWidget(extract_btn)

                undo_btn = QPushButton()
                undo_btn.setIcon(get_undo_icon())
                undo_btn.setIconSize(QSize(16, 16))
                undo_btn.setToolTip("Undo")
                undo_btn.setFixedSize(28, 28)
                undo_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #3e3e42;
                        border: none;
                        border-radius: 3px;
                    }
                    QPushButton:hover {
                        background-color: #505050;
                    }
                """)
                layout.addWidget(undo_btn)

                info_btn = QPushButton()
                info_btn.setIcon(get_info_icon())
                info_btn.setIconSize(QSize(16, 16))
                info_btn.setToolTip("Info")
                info_btn.setFixedSize(28, 28)
                info_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #3e3e42;
                        border: none;
                        border-radius: 3px;
                    }
                    QPushButton:hover {
                        background-color: #505050;
                    }
                """)
                layout.addWidget(info_btn)

                # Spacer
                layout.addStretch()

                # Window control buttons
                minimize_btn = QPushButton("_")
                minimize_btn.setFixedSize(28, 28)
                minimize_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #3e3e42;
                        color: #cccccc;
                        border: none;
                        border-radius: 3px;
                    }
                    QPushButton:hover {
                        background-color: #505050;
                    }
                """)
                minimize_btn.clicked.connect(self.showMinimized)
                layout.addWidget(minimize_btn)

                maximize_btn = QPushButton("□")
                maximize_btn.setFixedSize(28, 28)
                maximize_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #3e3e42;
                        color: #cccccc;
                        border: none;
                        border-radius: 3px;
                    }
                    QPushButton:hover {
                        background-color: #505050;
                    }
                """)
                maximize_btn.clicked.connect(self._toggle_maximize)
                layout.addWidget(maximize_btn)

                close_btn = QPushButton("X")
                close_btn.setFixedSize(28, 28)
                close_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #bc5a5a;
                        color: white;
                        border: none;
                        border-radius: 3px;
                    }
                    QPushButton:hover {
                        background-color: #dd6a6a;
                    }
                """)
                close_btn.clicked.connect(self.close)
                layout.addWidget(close_btn)

                # Insert title bar at top of main window
                central_widget = self.centralWidget()
                if central_widget and central_widget.layout():
                    main_layout = central_widget.layout()
                    main_layout.insertWidget(0, self.custom_title_bar)

                self.log_message("Custom title bar created and added")
            else:
                # Show existing custom title bar
                self.custom_title_bar.show()

        except Exception as e:
            self.log_message(f"Error creating custom title bar: {str(e)}")
            import traceback
            traceback.print_exc()


    def _remove_custom_title_bar(self):
        """Remove the custom title bar and return to system UI"""
        if self.custom_title_bar:
            self.custom_title_bar.hide()
            self.log_message("Custom title bar removed")

    def _update_custom_title_bar(self):
        """Update the custom title bar"""
        if self.custom_title_bar:
            self.custom_title_bar.show()

    def _show_settings_dialog(self):
        """Show settings dialog"""
        try:
            from apps.gui.gui_settings import SettingsDialog
            if hasattr(self, 'app_settings'):
                dialog = SettingsDialog(self.app_settings, self)
                dialog.exec()
        except Exception as e:
            self.log_message(f"Error showing settings: {str(e)}")

    def _show_info_dialog(self):
        """Show info dialog"""
        try:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "Info", "IMG Factory 1.5\nCustom UI Mode Active")
        except Exception as e:
            self.log_message(f"Error showing info dialog: {str(e)}")

    def _toggle_maximize(self):
        """Toggle window maximize state"""
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()

    def get_left_panel(self) -> QWidget:
        """Get the left panel for content"""
        return self.left_panel
    
    def get_right_panel(self) -> QWidget:
        """Get the right panel for controls"""
        return self.right_panel
    
    def closeEvent(self, event):
        """Handle window close event"""
        try:
            # Save panel settings if manager exists
            if self.tearoff_manager:
                self.tearoff_manager._save_panel_settings()
            
            # Save window geometry
            # TODO: Implement settings system
            
            self.log_message("Main window closed")
            event.accept()
            
        except Exception as e:
            print(f"Error during close: {e}")
            event.accept()


def create_main_window() -> IMGFactoryMainWindow:
    """Create and return main window instance"""
    return IMGFactoryMainWindow()


# Export main classes
__all__ = [
    'IMGFactoryMainWindow',
    'create_main_window'
]
