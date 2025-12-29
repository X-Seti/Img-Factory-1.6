#this belongs in gui/col_gui_integration.py - Version: 2
# X-Seti - July17 2025 - IMG Factory 1.5 - COL GUI Integration
# Uses IMG debug system and integrates COL tabs into main tab system

"""
COL GUI Integration - Complete integration of COL functionality into IMG Factory GUI
Moved from apps.components.col_tabs_functions.py to match IMG pattern
Uses IMG debug system throughout
"""

import os
from typing import Optional, Dict, Any
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QTableWidget, QTableWidgetItem,
    QLabel, QPushButton, QGroupBox, QListWidget, QListWidgetItem, QTabWidget,
    QHeaderView, QAbstractItemView, QTextEdit, QProgressBar, QStatusBar
)
from PyQt6.QtCore import Qt, pyqtSignal, QTimer
from PyQt6.QtGui import QAction, QIcon, QFont

# Import COL components with IMG debug system
from apps.methods.col_core_classes import COLFile, COLModel
from apps.debug.debug_functions import col_debug_log, is_col_debug_enabled
from apps.debug.debug_functions import img_debugger

##Methods list -
# add_col_tab_to_main_widget
# create_col_status_indicator  
# integrate_col_gui_into_imgfactory
# setup_col_gui_integration
# setup_col_tab_for_file
# setup_col_table_structure
# update_col_tab_info

##Classes -
# COLStatusIndicator
# COLToolsButtonGroup

class COLStatusIndicator(QLabel):
    """Status indicator for COL operations in status bar"""
    
    def __init__(self, parent=None):
        super().__init__("COL Ready", parent)
        self.setStyleSheet("color: #4CAF50; font-weight: bold; padding: 2px 8px;")
        self.setToolTip("COL system status")
        
        # Update timer
        self.update_timer = QTimer()
        self.update_timer.timeout.connect(self.update_status)
        self.update_timer.start(3000)  # Update every 3 seconds
    
    def update_status(self):
        """Update status display"""
        try:
            if is_col_debug_enabled():
                self.setText("🔴 COL Debug")
                self.setStyleSheet("color: #f44336; font-weight: bold; padding: 2px 8px;")
            else:
                self.setText("COL Ready")
                self.setStyleSheet("color: #4CAF50; font-weight: bold; padding: 2px 8px;")
        except:
            self.setText("COL Ready")

class COLToolsButtonGroup(QWidget):
    """Group of COL tool buttons for right panel integration"""
    
    col_editor_requested = pyqtSignal()
    col_batch_requested = pyqtSignal()
    col_analyzer_requested = pyqtSignal()
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setup_ui()
        self.connect_signals()
    
    def setup_ui(self):
        """Setup the button group UI"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(5, 5, 5, 5)
        
        # COL Tools group
        col_group = QGroupBox("COL Tools")
        col_layout = QVBoxLayout(col_group)
        
        # COL Editor button
        self.col_editor_btn = QPushButton("🔧 COL Editor")
        self.col_editor_btn.setToolTip("Open COL Editor\nCreate and edit collision files")
        self.col_editor_btn.clicked.connect(self.col_editor_requested.emit)
        col_layout.addWidget(self.col_editor_btn)
        
        # Secondary tools in horizontal layout
        tools_layout = QHBoxLayout()
        
        self.batch_btn = QPushButton("⚙️ Batch")
        self.batch_btn.setToolTip("COL Batch Processor\nProcess multiple COL files at once")
        self.batch_btn.clicked.connect(self.col_batch_requested.emit)
        tools_layout.addWidget(self.batch_btn)
        
        self.analyzer_btn = QPushButton("📊 Analyze")
        self.analyzer_btn.setToolTip("COL Analyzer\nAnalyze COL file structure and quality")
        self.analyzer_btn.clicked.connect(self.col_analyzer_requested.emit)
        tools_layout.addWidget(self.analyzer_btn)
        
        col_layout.addLayout(tools_layout)
        layout.addWidget(col_group)
    
    def connect_signals(self):
        """Connect button signals to actions"""
        # These will be connected to main window methods during integration
        pass

def setup_col_tab_for_file(main_window, file_path: str) -> Optional[int]: #vers 1
    """Setup or reuse tab for COL file - MOVED from apps.components.col_tabs_functions.py"""
    try:
        col_debug_log(main_window, f"Setting up COL tab for: {file_path}", 'COL_TABS')
        
        current_index = main_window.main_tab_widget.currentIndex()

        # Check if current tab is empty
        if not hasattr(main_window, 'open_files') or current_index not in main_window.open_files:
            col_debug_log(main_window, "Using current tab for COL file", 'COL_TABS')
        else:
            col_debug_log(main_window, "Creating new tab for COL file", 'COL_TABS')
            if hasattr(main_window, 'close_manager'):
                main_window.create_tab()
                current_index = main_window.main_tab_widget.currentIndex()
            else:
                col_debug_log(main_window, "Close manager not available", 'COL_TABS', 'WARNING')
                return None

        # Setup tab info
        file_name = os.path.basename(file_path)
        file_name_clean = file_name[:-4] if file_name.lower().endswith('.col') else file_name
        tab_name = f"{file_name_clean}"

        # Store tab info
        if not hasattr(main_window, 'open_files'):
            main_window.open_files = {}

        main_window.open_files[current_index] = {
            'type': 'COL',
            'file_path': file_path,
            'file_object': None,
            'tab_name': tab_name
        }

        # Update tab name
        main_window.main_tab_widget.setTabText(current_index, tab_name)
        
        col_debug_log(main_window, f"COL tab setup complete: {tab_name}", 'COL_TABS', 'SUCCESS')
        return current_index

    except Exception as e:
        col_debug_log(main_window, f"Error setting up COL tab: {str(e)}", 'COL_TABS', 'ERROR')
        return None

def setup_col_table_structure(main_window): #vers 1
    """Setup table structure for COL data display - MOVED from apps.components.""
    try:
        col_debug_log(main_window, "Setting up COL table structure", 'COL_TABLE')
        
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            col_debug_log(main_window, "Main table not available", 'COL_TABLE', 'WARNING')
            return False

        table = main_window.gui_layout.table

        # Configure table for COL data (7 columns)
        table.setColumnCount(7)
        table.setHorizontalHeaderLabels([
            "Model", "Type", "Surfaces", "Vertices", "Faces", "Collision", "Status"
        ])

        # Configure column widths for COL data
        header = table.horizontalHeader()
        header.setSectionsMovable(True)
        header.setSectionResizeMode(QHeaderView.ResizeMode.Interactive)
        
        # Set COL-specific column widths
        table.setColumnWidth(0, 150)  # Model name
        table.setColumnWidth(1, 80)   # Type (COL1/2/3/4)
        table.setColumnWidth(2, 100)  # Surfaces count
        table.setColumnWidth(3, 100)  # Vertices count
        table.setColumnWidth(4, 100)  # Faces count
        table.setColumnWidth(5, 120)  # Collision types
        table.setColumnWidth(6, 80)   # Status

        # Apply COL-specific styling
        table.setAlternatingRowColors(True)
        table.setStyleSheet("""
            QTableWidget {
                background-color: #f8f9fa;
                alternate-background-color: #e3f2fd;
                selection-background-color: #2196f3;
                gridline-color: #ddd;
            }
            QHeaderView::section {
                background-color: #1976d2;
                color: white;
                font-weight: bold;
                border: 1px solid #0d47a1;
                padding: 4px;
            }
        """)
        
        col_debug_log(main_window, "COL table structure configured", 'COL_TABLE', 'SUCCESS')
        return True
        
    except Exception as e:
        col_debug_log(main_window, f"Error setting up COL table structure: {str(e)}", 'COL_TABLE', 'ERROR')
        return False

def add_col_tab_to_main_widget(main_window): #vers 1
    """Add COL tab to the main interface - UNIFIED with IMG tabs"""
    try:
        col_debug_log(main_window, "Integrating COL tabs into main tab widget", 'COL_INTEGRATION')

        # COL files now use the SAME main_tab_widget as IMG files
        # No separate COL tab widget - unified system
        
        if not hasattr(main_window, 'main_tab_widget'):
            col_debug_log(main_window, "Main tab widget not available", 'COL_INTEGRATION', 'ERROR')
            return False

        # Add COL-specific menu items to main window
        if hasattr(main_window, 'menuBar') and main_window.menuBar():
            # Find or create Tools menu
            tools_menu = None
            for action in main_window.menuBar().actions():
                if action.text() == "Tools":
                    tools_menu = action.menu()
                    break
            
            if not tools_menu:
                tools_menu = main_window.menuBar().addMenu("Tools")

            # Add COL submenu
            col_menu = tools_menu.addMenu("COL Tools")
            
            # COL actions
            open_col_action = col_menu.addAction("Open COL File...")
            open_col_action.triggered.connect(lambda: open_col_file_dialog(main_window))
            
            create_col_action = col_menu.addAction("Create New COL...")
            create_col_action.triggered.connect(lambda: create_col_editor_action(main_window))
            
            col_debug_log(main_window, "COL menu integrated into main menu bar", 'COL_INTEGRATION')

        col_debug_log(main_window, "COL tab integration complete", 'COL_INTEGRATION', 'SUCCESS')
        return True
        
    except Exception as e:
        col_debug_log(main_window, f"Error adding COL tab integration: {str(e)}", 'COL_INTEGRATION', 'ERROR')
        return False

def create_col_status_indicator(main_window): #vers 1
    """Create COL status indicator for status bar"""
    try:
        col_debug_log(main_window, "Creating COL status indicator", 'COL_GUI')
        
        col_status = COLStatusIndicator()
        
        # Add to status bar if available
        if hasattr(main_window, 'statusBar') and main_window.statusBar():
            main_window.statusBar().addPermanentWidget(col_status)
            main_window.col_status_indicator = col_status
            col_debug_log(main_window, "COL status indicator added to status bar", 'COL_GUI', 'SUCCESS')
            return col_status
        else:
            col_debug_log(main_window, "Status bar not available for COL indicator", 'COL_GUI', 'WARNING')
            return None
            
    except Exception as e:
        col_debug_log(main_window, f"Error creating COL status indicator: {str(e)}", 'COL_GUI', 'ERROR')
        return None

def update_col_tab_info(main_window, file_path: str, stats: Dict[str, Any] = None): #vers 1
    """Update COL tab information"""
    try:
        col_debug_log(main_window, f"Updating COL tab info for: {file_path}", 'COL_TABS')
        
        current_index = main_window.main_tab_widget.currentIndex()
        
        # Update tab title
        file_name = os.path.basename(file_path)
        file_name_clean = file_name[:-4] if file_name.lower().endswith('.col') else file_name
        
        if stats and 'model_count' in stats:
            tab_name = f"{file_name_clean} ({stats['model_count']})"
        else:
            tab_name = f"{file_name_clean}"
        
        main_window.main_tab_widget.setTabText(current_index, tab_name)
        
        # Update window title
        main_window.setWindowTitle(f"IMG Factory 1.5 - {file_name} (COL)")
        
        # Update status indicator if available
        if hasattr(main_window, 'col_status_indicator') and main_window.col_status_indicator:
            if stats and 'model_count' in stats:
                main_window.col_status_indicator.setText(f"COL ({stats['model_count']} models)")
            else:
                main_window.col_status_indicator.setText("COL Loaded")
        
        col_debug_log(main_window, f"COL tab info updated: {tab_name}", 'COL_TABS', 'SUCCESS')
        return True
        
    except Exception as e:
        col_debug_log(main_window, f"Error updating COL tab info: {str(e)}", 'COL_TABS', 'ERROR')
        return False

def setup_col_gui_integration(main_window): #vers 1
    """Setup COL GUI integration with main window - MAIN ENTRY POINT"""
    try:
        col_debug_log(main_window, "Starting COL GUI integration", 'COL_INTEGRATION')
        
        # 1. Add COL tab integration to main widget
        tab_success = add_col_tab_to_main_widget(main_window)
        
        # 2. Create COL status indicator
        status_success = create_col_status_indicator(main_window) is not None
        
        # 3. Add COL methods to main window
        main_window.setup_col_tab_for_file = lambda file_path: setup_col_tab_for_file(main_window, file_path)
        main_window.setup_col_table_structure = lambda: setup_col_table_structure(main_window)
        main_window.update_col_tab_info = lambda file_path, stats=None: update_col_tab_info(main_window, file_path, stats)
        
        # 4. Import helper functions for menu integration
        from gui.gui_context import open_col_file_dialog, open_col_editor_dialog, open_col_batch_proc_dialog
        
        def open_col_file_dialog_wrapper():
            """Wrapper for COL file dialog"""
            try:
                open_col_file_dialog(main_window)
            except Exception as e:
                col_debug_log(main_window, f"COL file dialog error: {e}", 'COL_GUI', 'ERROR')
        
        def create_col_editor_action():
            """Wrapper for COL editor"""
            try:
                open_col_editor_dialog(main_window)
            except Exception as e:
                col_debug_log(main_window, f"COL editor error: {e}", 'COL_GUI', 'ERROR')
        
        # Add wrapped functions to main window
        main_window.open_col_file_dialog = open_col_file_dialog_wrapper
        main_window.create_col_editor_action = create_col_editor_action
        
        overall_success = tab_success and status_success
        
        if overall_success:
            col_debug_log(main_window, "COL GUI integration completed successfully", 'COL_INTEGRATION', 'SUCCESS')
            main_window.log_message("✅ COL GUI integrated into main interface")
        else:
            col_debug_log(main_window, "COL GUI integration completed with issues", 'COL_INTEGRATION', 'WARNING')
            main_window.log_message("⚠️ COL GUI integration had some issues")
        
        return overall_success
        
    except Exception as e:
        col_debug_log(main_window, f"COL GUI integration failed: {str(e)}", 'COL_INTEGRATION', 'ERROR')
        main_window.log_message(f"❌ COL GUI integration error: {str(e)}")
        return False

def integrate_col_gui_into_imgfactory(img_factory_window): #vers 1
    """
    Main integration function - Integrate COL GUI components into IMG Factory
    REPLACES separate COL tab system with unified IMG+COL system
    """
    try:
        col_debug_log(img_factory_window, "Integrating COL GUI into IMG Factory", 'COL_INTEGRATION')
        
        # Use the unified setup function
        success = setup_col_gui_integration(img_factory_window)
        
        if success:
            # Add COL tools to right panel if available
            try:
                # Find right panel in gui_layout
                if hasattr(img_factory_window, 'gui_layout') and hasattr(img_factory_window.gui_layout, 'right_panel'):
                    right_panel = img_factory_window.gui_layout.right_panel
                    layout = right_panel.layout()
                    
                    if layout:
                        # Create COL tools group
                        col_tools = COLToolsButtonGroup()
                        
                        # Connect signals to main window methods
                        col_tools.col_editor_requested.connect(img_factory_window.create_col_editor_action)
                        col_tools.col_batch_requested.connect(lambda: open_col_batch_proc_dialog(img_factory_window))
                        col_tools.col_analyzer_requested.connect(lambda: col_debug_log(img_factory_window, "COL analyzer not yet implemented", 'COL_GUI'))
                        
                        # Add to layout (before stretch if present)
                        count = layout.count()
                        if count > 0:
                            last_item = layout.itemAt(count - 1)
                            if hasattr(last_item, 'spacerItem') and last_item.spacerItem():
                                layout.insertWidget(count - 1, col_tools)
                            else:
                                layout.addWidget(col_tools)
                        else:
                            layout.addWidget(col_tools)
                        
                        # Store reference
                        img_factory_window.col_tools = col_tools
                        col_debug_log(img_factory_window, "COL tools added to right panel", 'COL_INTEGRATION')
                        
            except Exception as e:
                col_debug_log(img_factory_window, f"Failed to add COL tools to right panel: {e}", 'COL_INTEGRATION', 'WARNING')
        
        col_debug_log(img_factory_window, f"COL GUI integration result: {success}", 'COL_INTEGRATION')
        return success
        
    except Exception as e:
        col_debug_log(img_factory_window, f"Error integrating COL GUI: {e}", 'COL_INTEGRATION', 'ERROR')
        if hasattr(img_factory_window, 'log_message'):
            img_factory_window.log_message(f"❌ COL GUI integration error: {str(e)}")
        return False

# Convenience function for easy integration
def setup_col_gui(img_factory_window): #vers 1
    """
    Easy setup function for COL GUI integration
    Call this from your main IMG Factory window after UI creation
    """
    return integrate_col_gui_into_imgfactory(img_factory_window)

# Helper functions imported from gui_context
def open_col_file_dialog(main_window):
    """Import from gui_context"""
    from gui.gui_context import open_col_file_dialog as context_open_col
    context_open_col(main_window)

def create_col_editor_action(main_window):
    """Import from gui_context"""
    from gui.gui_context import open_col_editor_dialog
    open_col_editor_dialog(main_window)

# Export main functions
__all__ = [
    'COLStatusIndicator',
    'COLToolsButtonGroup',
    'add_col_tab_to_main_widget',
    'create_col_status_indicator',
    'integrate_col_gui_into_imgfactory',
    'setup_col_gui',
    'setup_col_gui_integration', 
    'setup_col_tab_for_file',
    'setup_col_table_structure',
    'update_col_tab_info'
]
