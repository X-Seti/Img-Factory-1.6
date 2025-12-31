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
        
        # Show window
        self.show()
    

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

    def _create_custom_title_bar(self):
        """Create a custom title bar with buttons"""
        try:
            from PyQt6.QtWidgets import QWidget, QHBoxLayout, QLabel, QPushButton
            from PyQt6.QtCore import Qt
            from apps.methods.imgfactory_svg_icons import get_open_icon, get_save_icon, get_undo_icon, get_info_icon, get_settings_icon, get_extract_icon
            
            # Create a custom title bar widget
            if self.custom_title_bar is None:
                self.custom_title_bar = QWidget()
                self.custom_title_bar.setFixedHeight(30)
                self.custom_title_bar.setStyleSheet("""
                    background-color: #3a3a3a;
                    border-bottom: 1px solid #555;
                """)
                
                layout = QHBoxLayout(self.custom_title_bar)
                layout.setContentsMargins(5, 2, 5, 2)
                layout.setSpacing(3)
                
                # Add Settings button
                settings_btn = QPushButton("Settings")
                settings_btn.setFixedSize(60, 24)
                settings_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #4a7abc;
                        color: white;
                        border: 1px solid #2a4a7a;
                        border-radius: 3px;
                        font-size: 11px;
                        font-weight: bold;
                    }
                    QPushButton:hover {
                        background-color: #5a8add;
                    }
                """)
                settings_btn.clicked.connect(self._show_settings_dialog)
                layout.addWidget(settings_btn)
                
                # Add title label
                title_label = QLabel("Title = Img Factory")
                title_label.setStyleSheet("color: white; font-weight: bold;")
                layout.addWidget(title_label)
                
                # Add Open button
                open_btn = QPushButton("Open")
                open_btn.setFixedSize(50, 24)
                open_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #5a8abc;
                        color: white;
                        border: 1px solid #2a4a7a;
                        border-radius: 3px;
                        font-size: 11px;
                    }
                    QPushButton:hover {
                        background-color: #6a9add;
                    }
                """)
                from apps.core.open import open_file_dialog
                open_btn.clicked.connect(lambda: open_file_dialog(self))
                layout.addWidget(open_btn)
                
                # Add Save button
                save_btn = QPushButton("Save")
                save_btn.setFixedSize(50, 24)
                save_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #5a8abc;
                        color: white;
                        border: 1px solid #2a4a7a;
                        border-radius: 3px;
                        font-size: 11px;
                    }
                    QPushButton:hover {
                        background-color: #6a9add;
                    }
                """)
                save_btn.clicked.connect(lambda: self.save_img_entry())
                layout.addWidget(save_btn)
                
                # Add Extract button
                extract_btn = QPushButton("Extract")
                extract_btn.setFixedSize(60, 24)
                extract_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #5a8abc;
                        color: white;
                        border: 1px solid #2a4a7a;
                        border-radius: 3px;
                        font-size: 11px;
                    }
                    QPushButton:hover {
                        background-color: #6a9add;
                    }
                """)
                from apps.core.extract import extract_textures_function
                extract_btn.clicked.connect(lambda: extract_textures_function(self))
                layout.addWidget(extract_btn)
                
                # Add Undo button
                undo_btn = QPushButton("undo")
                undo_btn.setFixedSize(50, 24)
                undo_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #5a8abc;
                        color: white;
                        border: 1px solid #2a4a7a;
                        border-radius: 3px;
                        font-size: 11px;
                    }
                    QPushButton:hover {
                        background-color: #6a9add;
                    }
                """)
                undo_btn.clicked.connect(self.undo_manager.undo)
                layout.addWidget(undo_btn)
                
                # Add Info button
                info_btn = QPushButton("i")
                info_btn.setFixedSize(25, 24)
                info_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #5a8abc;
                        color: white;
                        border: 1px solid #2a4a7a;
                        border-radius: 3px;
                        font-size: 11px;
                        font-weight: bold;
                    }
                    QPushButton:hover {
                        background-color: #6a9add;
                    }
                """)
                info_btn.clicked.connect(lambda: self.log_message("Info button clicked"))
                layout.addWidget(info_btn)
                
                # Add Maximize button
                maximize_btn = QPushButton("*")
                maximize_btn.setFixedSize(25, 24)
                maximize_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #5a8abc;
                        color: white;
                        border: 1px solid #2a4a7a;
                        border-radius: 3px;
                        font-size: 11px;
                        font-weight: bold;
                    }
                    QPushButton:hover {
                        background-color: #6a9add;
                    }
                """)
                maximize_btn.clicked.connect(self._toggle_maximize)
                layout.addWidget(maximize_btn)
                
                # Add Minimize button
                minimize_btn = QPushButton("_")
                minimize_btn.setFixedSize(25, 24)
                minimize_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #5a8abc;
                        color: white;
                        border: 1px solid #2a4a7a;
                        border-radius: 3px;
                        font-size: 11px;
                        font-weight: bold;
                    }
                    QPushButton:hover {
                        background-color: #6a9add;
                    }
                """)
                minimize_btn.clicked.connect(self.showMinimized)
                layout.addWidget(minimize_btn)
                
                # Add Close button
                close_btn = QPushButton("X")
                close_btn.setFixedSize(25, 24)
                close_btn.setStyleSheet("""
                    QPushButton {
                        background-color: #bc5a5a;
                        color: white;
                        border: 1px solid #7a2a2a;
                        border-radius: 3px;
                        font-size: 11px;
                        font-weight: bold;
                    }
                    QPushButton:hover {
                        background-color: #dd6a6a;
                    }
                """)
                close_btn.clicked.connect(self.close)
                layout.addWidget(close_btn)
                
                # Insert the custom title bar at the top of the central widget
                central_widget = self.centralWidget()
                if central_widget:
                    # Get the current layout and add the title bar
                    current_layout = central_widget.layout()
                    if current_layout:
                        # We need to restructure the layout to include the title bar
                        # For now, just log that the custom title bar was created
                        self.log_message("Custom title bar created")
            else:
                # Show the existing custom title bar
                self.custom_title_bar.show()
                
        except Exception as e:
            self.log_message(f"Error creating custom title bar: {str(e)}")

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
