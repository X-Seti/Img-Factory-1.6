#this belongs in components/col_functions.py - Version: 8
# X-Seti - July20 2025 - IMG Factory 1.5 - COL Functions
"""
COL Functions - Integration and UI components
Provides COL integration functions, widgets, and menu actions
"""

import os
from typing import Optional
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QListWidget, QListWidgetItem,
    QLabel, QTextEdit, QPushButton, QFileDialog, QMessageBox
)
from PyQt6.QtCore import Qt, QTimer

# Import IMG debug system and COL classes
from apps.debug.debug_functions import img_debugger
from apps.methods.col_core_classes import COLFile, COLModel
from apps.debug.debug_functions import col_debug_log

##Methods list -
# add_col_context_menu_to_entries_table
# add_col_tools_menu
# analyze_col_from_img_entry
# cancel_col_loading
# create_new_col_file
# edit_col_from_img_entry
# export_all_col_from_img
# import_col_to_current_img
# integrate_complete_col_system
# open_col_file_dialog
# setup_col_integration_full
# setup_complete_col_integration
# setup_delayed_col_integration
# setup_threaded_col_loading

##Classes -
# COLListWidget
# COLModelDetailsWidget

class COLListWidget(QWidget):
    """Widget for displaying COL model list"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setup_ui()
    
    def setup_ui(self): #vers 1
        """Setup COL list UI"""
        layout = QVBoxLayout(self)
        
        # Title
        title_label = QLabel("COL Models")
        layout.addWidget(title_label)
        
        # List widget
        self.list_widget = QListWidget()
        layout.addWidget(self.list_widget)
        
        # Buttons
        button_layout = QHBoxLayout()
        
        self.analyze_btn = QPushButton("Analyze")
        self.analyze_btn.clicked.connect(self.analyze_selected)
        button_layout.addWidget(self.analyze_btn)
        
        self.export_btn = QPushButton("Export")
        self.export_btn.clicked.connect(self.export_selected)
        button_layout.addWidget(self.export_btn)
        
        button_layout.addStretch()
        layout.addLayout(button_layout)
    
    def populate_models(self, col_file: COLFile): #vers 1
        """Populate list with COL models"""
        self.list_widget.clear()
        
        if not col_file or not col_file.models:
            return
        
        for i, model in enumerate(col_file.models):
            item_text = f"{model.name} (v{model.version.value})"
            item = QListWidgetItem(item_text)
            item.setData(Qt.ItemDataRole.UserRole, i)
            self.list_widget.addItem(item)
    
    def analyze_selected(self): #vers 1
        """Analyze selected model"""
        current_item = self.list_widget.currentItem()
        if current_item:
            model_index = current_item.data(Qt.ItemDataRole.UserRole)
            img_debugger.debug(f"Analyzing COL model {model_index}")
    
    def export_selected(self): #vers 1
        """Export selected model"""
        current_item = self.list_widget.currentItem()
        if current_item:
            model_index = current_item.data(Qt.ItemDataRole.UserRole)
            img_debugger.debug(f"Exporting COL model {model_index}")

class COLModelDetailsWidget(QWidget):
    """Widget for displaying COL model details"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setup_ui()
    
    def setup_ui(self): #vers 1
        """Setup model details UI"""
        layout = QVBoxLayout(self)
        
        # Title
        title_label = QLabel("Model Details")
        layout.addWidget(title_label)
        
        # Details text
        self.details_text = QTextEdit()
        self.details_text.setReadOnly(True)
        layout.addWidget(self.details_text)
    
    def show_model_details(self, model: COLModel): #vers 1
        """Show details for COL model"""
        if not model:
            self.details_text.clear()
            return
        
        details = []
        details.append(f"Name: {model.name}")
        details.append(f"Version: COL {model.version.value}")
        details.append(f"Model ID: {model.model_id}")
        details.append("")
        details.append(f"Collision Data:")
        details.append(f"  Spheres: {len(model.spheres)}")
        details.append(f"  Boxes: {len(model.boxes)}")
        details.append(f"  Vertices: {len(model.vertices)}")
        details.append(f"  Faces: {len(model.faces)}")
        details.append("")
        
        if model.bounding_box:
            bbox = model.bounding_box
            details.append(f"Bounding Box:")
            details.append(f"  Center: ({bbox.center.x:.2f}, {bbox.center.y:.2f}, {bbox.center.z:.2f})")
            details.append(f"  Radius: {bbox.radius:.2f}")
        
        self.details_text.setPlainText("\n".join(details))

def add_col_tools_menu(main_window): #vers 1
    """Add COL tools menu to main window using IMG debug system"""
    try:
        if not hasattr(main_window, 'menuBar') or not main_window.menuBar():
            col_debug_log(main_window, "No menu bar available for COL tools menu", 'COL_MENU', 'ERROR')
            return False
        
        from PyQt6.QtGui import QAction
        
        menubar = main_window.menuBar()
        
        # Create COL menu
        col_menu = menubar.addMenu("🔧 COL")
        
        # File operations
        open_col_action = QAction("Open COL File", main_window)
        open_col_action.setShortcut("Ctrl+Shift+O")
        open_col_action.triggered.connect(lambda: open_col_file_dialog(main_window))
        col_menu.addAction(open_col_action)
        
        new_col_action = QAction("🆕 New COL File", main_window)
        new_col_action.triggered.connect(lambda: create_new_col_file(main_window))
        col_menu.addAction(new_col_action)
        
        col_menu.addSeparator()
        
        # COL Editor
        editor_action = QAction("✏️ COL Editor", main_window)
        editor_action.setShortcut("Ctrl+E")
        editor_action.triggered.connect(lambda: open_col_editor(main_window))
        col_menu.addAction(editor_action)
        
        col_menu.addSeparator()
        
        # Batch operations
        from apps.methods.col_utilities import open_col_batch_processor, analyze_col_file_dialog
        
        batch_process_action = QAction("⚙️ Batch Processor", main_window)
        batch_process_action.triggered.connect(lambda: open_col_batch_processor(main_window))
        col_menu.addAction(batch_process_action)
        
        analyze_action = QAction("📊 Analyze COL", main_window)
        analyze_action.triggered.connect(lambda: analyze_col_file_dialog(main_window))
        col_menu.addAction(analyze_action)
        
        col_menu.addSeparator()
        
        # Import/Export
        import_to_img_action = QAction("📥 Import to IMG", main_window)
        import_to_img_action.triggered.connect(lambda: import_col_to_current_img(main_window))
        col_menu.addAction(import_to_img_action)
        
        export_from_img_action = QAction("📤 Export from IMG", main_window)
        export_from_img_action.triggered.connect(lambda: export_all_col_from_img(main_window))
        col_menu.addAction(export_from_img_action)
        
        # Store reference to COL menu
        main_window.col_menu = col_menu
        
        col_debug_log(main_window, "COL tools menu added successfully", 'COL_MENU', 'SUCCESS')
        return True
        
    except Exception as e:
        col_debug_log(main_window, f"Error adding COL tools menu: {e}", 'COL_MENU', 'ERROR')
        return False


def open_col_file_dialog(main_window): #vers 4
    """Open COL file dialog using IMG debug system"""
    try:
        file_path, _ = QFileDialog.getOpenFileName(
            main_window, "Open COL File", "", "COL Files (*.col);;All Files (*)"
        )

        if file_path:
            # Use the COL loading system
            from apps.methods.col_parsing_functions import load_col_file_safely
            success = load_col_file_safely(main_window, file_path)

            if success:
                img_debugger.success(f"COL file opened: {os.path.basename(file_path)}")
            else:
                img_debugger.error(f"Failed to open COL file: {os.path.basename(file_path)}")

            return success

        return False

    except Exception as e:
        img_debugger.error(f"Error opening COL file dialog: {str(e)}")
        return False


def create_new_col_file(main_window): #vers 1
    """Create new COL file using IMG debug system"""
    try:
        file_path, _ = QFileDialog.getSaveFileName(
            main_window,
            "Create New COL File",
            "",
            "COL Files (*.col);;All Files (*)"
        )

        if file_path:
            # Create basic COL1 file structure
            col_header = struct.pack('<4sI22sH', b'COLL', 32, b'new_collision\x00' * 22, 0)

            with open(file_path, 'wb') as f:
                f.write(col_header)
                # Add minimal COL1 data structure
                f.write(b'\x00' * 32)  # Basic collision data

            QMessageBox.information(main_window, "Success", f"Created new COL file: {os.path.basename(file_path)}")
            img_debugger.success(f"Created new COL file: {os.path.basename(file_path)}")
            return True

    except Exception as e:
        img_debugger.error(f"Failed to create COL file: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Failed to create COL file: {str(e)}")
        return False


def open_col_editor(main_window): #vers 1
    """Open COL editor using IMG debug system"""
    try:
        col_debug_log(main_window, "Opening COL editor", 'COL_EDITOR')
        
        from apps.methods.col_utilities import open_col_editor
        open_col_editor(main_window)
        
    except Exception as e:
        col_debug_log(main_window, f"Error opening COL editor: {e}", 'COL_EDITOR', 'ERROR')


def import_col_to_current_img(main_window): #vers 2
    """Import COL file to current IMG using IMG debug system"""
    try:
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            QMessageBox.warning(main_window, "No IMG", "Please open an IMG file first")
            return False

        file_path, _ = QFileDialog.getOpenFileName(
            main_window, "Import COL File to IMG", "", "COL Files (*.col);;All Files (*)"
        )

        if file_path:
            with open(file_path, 'rb') as f:
                col_data = f.read()

            # Validate it's a COL file
            from apps.components.col_integration_main import detect_col_version_from_data
            analysis = detect_col_version_from_data(col_data)
            if not analysis:
                QMessageBox.warning(main_window, "Invalid File", "Selected file is not a valid COL file")
                return False

            entry_name = os.path.basename(file_path)
            main_window.current_img.add_entry(entry_name, col_data)

            QMessageBox.information(main_window, "Success", f"COL imported as {entry_name}")
            img_debugger.success(f"COL imported to IMG: {entry_name}")

            # Refresh the entries table
            if hasattr(main_window, 'populate_entries_table'):
                main_window.populate_entries_table()

            return True

    except Exception as e:
        img_debugger.error(f"Failed to import COL to IMG: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Failed to import COL: {str(e)}")
        return False


def export_all_col_from_img(main_window): #vers 3
    """Export all COL files from current IMG using IMG debug system"""
    try:
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            QMessageBox.warning(main_window, "No IMG", "Please open an IMG file first")
            return False

        output_dir = QFileDialog.getExistingDirectory(
            main_window, "Export COL Files to Directory"
        )

        if output_dir:
            exported_count = 0
            failed_count = 0

            # Find and export all COL files
            for entry in main_window.current_img.entries:
                if entry.name.lower().endswith('.col'):
                    try:
                        output_path = os.path.join(output_dir, entry.name)
                        entry.extract_to_file(output_path)
                        exported_count += 1
                        img_debugger.debug(f"Exported: {entry.name}")
                    except Exception as e:
                        failed_count += 1
                        img_debugger.warning(f"Failed to export {entry.name}: {e}")

            # Show results
            if exported_count > 0:
                message = f"Exported {exported_count} COL files to {output_dir}"
                if failed_count > 0:
                    message += f"\n{failed_count} files failed to export"
                QMessageBox.information(main_window, "Export Complete", message)
                img_debugger.success(f"COL export complete: {exported_count} files")
            else:
                QMessageBox.information(main_window, "No COL Files", "No COL files found in current IMG")
                img_debugger.warning("No COL files found to export")

            return exported_count > 0

    except Exception as e:
        img_debugger.error(f"Failed to export COL files: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Failed to export COL files: {str(e)}")
        return False


def edit_col_from_img_entry(main_window, row): #vers 3
    """Edit COL file from IMG entry using IMG debug system"""
    try:
        # Get entry from row
        if hasattr(main_window, 'current_img') and main_window.current_img:
            entries = main_window.current_img.entries
            if row < len(entries):
                entry = entries[row]

                # Use the function from col_integration_main
                from apps.components.col_integration_main import edit_col_from_img_entry
                edit_col_from_img_entry(main_window, row)
                return True

        img_debugger.warning("Invalid row or no IMG loaded for COL editing")
        return False

    except Exception as e:
        img_debugger.error(f"Failed to edit COL from IMG entry: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Failed to edit COL: {str(e)}")
        return False


def analyze_col_from_img_entry(main_window, row): #vers 1
    """Analyze COL file from IMG entry using IMG debug system"""
    try:
        # Get entry from row
        if hasattr(main_window, 'current_img') and main_window.current_img:
            entries = main_window.current_img.entries
            if row < len(entries):
                entry = entries[row]

                # Use the function from col_integration_main
                from apps.components.col_integration_main import analyze_col_from_img_entry
                analyze_col_from_img_entry(main_window, row)
                return True

        img_debugger.warning("Invalid row or no IMG loaded for COL analysis")
        return False

    except Exception as e:
        img_debugger.error(f"Failed to analyze COL from IMG entry: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Failed to analyze COL: {str(e)}")
        return False


def setup_threaded_col_loading(main_window): #vers 1
    """Setup threaded COL loading using IMG debug system"""
    try:
        col_debug_log(main_window, "Setting up threaded COL loading", 'COL_THREADING')
        
        from apps.components.col_loader import COLBackgroundLoader
        
        # Create background loader
        col_loader = COLBackgroundLoader()
        
        # Connect signals
        if hasattr(main_window, '_on_col_loaded'):
            col_loader.load_complete.connect(main_window._on_col_loaded)
        
        if hasattr(main_window, '_on_load_progress'):
            col_loader.progress_update.connect(main_window._on_load_progress)
        
        # Store reference
        main_window.col_background_loader = col_loader
        
        col_debug_log(main_window, "Threaded COL loading setup complete", 'COL_THREADING', 'SUCCESS')
        return True
        
    except Exception as e:
        col_debug_log(main_window, f"Error setting up threaded COL loading: {e}", 'COL_THREADING', 'ERROR')
        return False

def cancel_col_loading(main_window): #vers 1
    """Cancel COL loading process using IMG debug system"""
    try:
        if hasattr(main_window, 'col_background_loader'):
            main_window.col_background_loader.stop_loading()
            col_debug_log(main_window, "COL loading cancelled", 'COL_THREADING')
        
    except Exception as e:
        col_debug_log(main_window, f"Error cancelling COL loading: {e}", 'COL_THREADING', 'ERROR')

def setup_col_integration_full(main_window): #vers 1
    """Main COL integration entry point with threaded loading using IMG debug system"""
    try:
        col_debug_log(main_window, "Starting full COL integration for IMG interface", 'COL_INTEGRATION')

        # Setup threaded loading
        setup_threaded_col_loading(main_window)

        # Add COL tools menu to existing menu bar
        if hasattr(main_window, 'menuBar') and main_window.menuBar():
            add_col_tools_menu(main_window)
            col_debug_log(main_window, "COL tools menu added", 'COL_INTEGRATION')

        # Add COL context menu items to existing entries table
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            add_col_context_menu_to_entries_table(main_window)
            col_debug_log(main_window, "COL context menu added to entries table", 'COL_INTEGRATION')

        # Mark integration as completed
        main_window._col_integration_active = True

        col_debug_log(main_window, "Full COL integration completed successfully", 'COL_INTEGRATION', 'SUCCESS')
        return True

    except Exception as e:
        col_debug_log(main_window, f"Full COL integration failed: {e}", 'COL_INTEGRATION', 'ERROR')
        return False

def setup_complete_col_integration(main_window): #vers 1
    """Complete COL integration setup - main entry point using IMG debug system"""
    try:
        col_debug_log(main_window, "Starting complete COL system integration", 'COL_INTEGRATION')
        
        # Check settings for initial debug state
        try:
            if hasattr(main_window, 'app_settings') and hasattr(main_window.app_settings, 'debug_enabled'):
                from apps.debug.debug_functions import set_col_debug_enabled
                set_col_debug_enabled(main_window.app_settings.debug_enabled)
        except:
            pass
        
        # Setup full integration
        return setup_col_integration_full(main_window)
        
    except Exception as e:
        col_debug_log(main_window, f"Complete COL integration failed: {e}", 'COL_INTEGRATION', 'ERROR')
        return False

def integrate_complete_col_system(main_window): #vers 1
    """Placeholder for COL integration during init - DEPRECATED"""
    # Just set a flag that COL integration is needed
    main_window._col_integration_needed = True
    col_debug_log(main_window, "COL integration marked for later setup", 'COL_INTEGRATION')

def setup_delayed_col_integration(main_window): #vers 1
    """Setup COL integration after GUI is fully ready"""
    try:
        # Use a timer to delay until GUI is ready
        def try_setup():
            if setup_col_integration_full(main_window):
                # Success - stop trying
                return
            else:
                # Retry in 100ms
                QTimer.singleShot(100, try_setup)
        
        # Start the retry process
        QTimer.singleShot(100, try_setup)
        
    except Exception as e:
        col_debug_log(main_window, f"Error setting up delayed COL integration: {str(e)}", 'COL_INTEGRATION')

# Export main classes and functions
__all__ = [
    'COLListWidget', 
    'COLModelDetailsWidget',
    'add_col_tools_menu',
    'add_col_context_menu_to_entries_table',
    'open_col_file_dialog',
    'create_new_col_file',
    'open_col_editor',
    'import_col_to_current_img',
    'export_all_col_from_img',
    'edit_col_from_img_entry',
    'analyze_col_from_img_entry',
    'setup_threaded_col_loading',
    'cancel_col_loading',
    'setup_col_integration_full',
    'setup_complete_col_integration',
    'integrate_complete_col_system',
    'setup_delayed_col_integration'
]
        
