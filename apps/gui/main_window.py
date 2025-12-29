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
