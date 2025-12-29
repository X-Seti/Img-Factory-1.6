#this belongs in components/col_loader.py - Version: 22
# X-Seti - July23 2025 - IMG Factory 1.5 - COL Threaded Loader
"""
COL Threaded Loader
"""

import os
from typing import Optional, asynchronously
from PyQt6.QtWidgets import QTableWidgetItem
from PyQt6.QtCore import QThread, pyqtSignal

# Import IMG debug system and COL classes
from apps.debug.debug_functions import img_debugger
from apps.methods.col_core_classes import COLFile

##Functions list -
# _load_with_progress
# cancel_load
# load_col_file_async
# on_load_complete
# on_load_error
# on_model_loaded
# on_progress
# progress_load
# run
# create_table_item
# load_col_file_object
# load_col_file_safely
# populate_col_table
# setup_col_table_structure
# update_col_info_bar
# validate_col_file

##Classes list -
# COLBackgroundLoader


def load_col_file_safely(main_window, file_path: str) -> bool: #vers 8
    """Load COL file safely with proper validation and tab management"""
    try:
        img_debugger.debug(f"Loading COL: {os.path.basename(file_path)}")

        # Use threaded loader for better UX
        #from apps.components.col_loader import load_col_file_threaded
        loader = load_col_file_threaded(main_window, file_path)

        # Return True if loader started successfully
        return loader is not None

    except Exception as e:
        img_debugger.error(f"Error in COL loading: {str(e)}")
        return False

def load_col_file_object(main_window, file_path: str) -> Optional[COLFile]: #vers 8
    """Load COL file and return COL object directly"""
    try:
        if not validate_col_file(main_window, file_path):
            return None

        col_file = COLFile(file_path)

        if col_file.load_from_file(file_path):
            if col_file.load_error:
                error_details = col_file.load_error
                img_debugger.error(f"COL file load error: {error_details}")
                main_window.log_message(f"❌ Failed to load COL file: {error_details}")
                return None

            model_count = len(col_file.models) if hasattr(col_file, 'models') else 0
            img_debugger.success(f"COL file loaded: {model_count} models")
            return col_file

        except Exception as e:
            img_debugger.error(f"Error loading COL file: {str(e)}")
            return None

def validate_col_file(main_window, file_path: str) -> bool: #vers 8
    """Validate COL file before loading"""
    try:
        if not os.path.exists(file_path):
            img_debugger.error(f"COL file not found: {file_path}")
            return False

        if not os.access(file_path, os.R_OK):
            img_debugger.error(f"Cannot read COL file: {file_path}")
            return False

        file_size = os.path.getsize(file_path)
        if file_size < 32:
            img_debugger.error(f"COL file too small ({file_size} bytes)")
            return False

        # Check COL signature
        try:
            with open(file_path, 'rb') as f:
                signature = f.read(4)
                if signature not in [b'COLL', b'COL\x02', b'COL\x03', b'COL\x04']:
                    img_debugger.error(f"Invalid COL signature: {signature}")
                    return False
        except Exception as e:
            img_debugger.error(f"Error reading COL signature: {str(e)}")
            return False

        img_debugger.debug(f"COL file validation passed: {os.path.basename(file_path)}")
        return True

    except Exception as e:
        img_debugger.error(f"Error validating COL file: {str(e)}")
        return False

def setup_col_table_structure(main_window) -> bool: #vers 8
    """Setup table structure for COL data"""
    try:
        # Try to use methods version first
        try:
            from apps.methods.populate_col_table import setup_col_table_structure as methods_setup_structure
            return methods_setup_structure(main_window)
        except ImportError:
            img_debugger.warning("methods.populate_col_table not available, using fallback")

        # Fallback implementation
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            img_debugger.error("GUI layout or table not found")
            return False

        table = main_window.gui_layout.table

        # COL-specific columns
        col_headers = ["Model Name", "Type", "Version", "Size", "Spheres", "Boxes", "Faces", "Info"]
        table.setColumnCount(len(col_headers))
        table.setHorizontalHeaderLabels(col_headers)

        # Set column widths
        table.setColumnWidth(0, 200)  # Model Name
        table.setColumnWidth(1, 80)   # Type
        table.setColumnWidth(2, 80)   # Version
        table.setColumnWidth(3, 100)  # Size
        table.setColumnWidth(4, 80)   # Spheres
        table.setColumnWidth(5, 80)   # Boxes
        table.setColumnWidth(6, 80)   # Faces
        table.setColumnWidth(7, 150)  # Info

        img_debugger.debug("COL table structure setup complete")
        return True

    except Exception as e:
        img_debugger.error(f"Error setting up COL table structure: {str(e)}")
        return False

def populate_col_table(main_window, col_file: Any) -> bool: #vers 8
    """Populate table with COL data"""
    try:
        # Try to use methods version first
        try:
            from apps.methods.populate_col_table import populate_table_with_col_data_debug
            return populate_table_with_col_data_debug(main_window, col_file)
        except ImportError:
            img_debugger.warning("methods.populate_col_table not available, using fallback")

        # Fallback implementation
        if not col_file or not hasattr(col_file, 'models'):
            img_debugger.error("Invalid COL file for table population")
            return False

        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            img_debugger.error("GUI layout or table not found")
            return False

        table = main_window.gui_layout.table
        table.setRowCount(len(col_file.models))

        for row, model in enumerate(col_file.models):
            # Model Name
            name_item = create_table_item(model.name or f"Model_{row+1}")
            table.setItem(row, 0, name_item)

            # Type
            type_item = create_table_item("COL")
            table.setItem(row, 1, type_item)

            # Version
            version_item = create_table_item(str(getattr(model, 'col_version', 1)))
            table.setItem(row, 2, version_item)

            # Size (estimate)
            size_item = create_table_item("N/A")
            table.setItem(row, 3, size_item)

            # Stats
            stats = model.get_total_stats()
            table.setItem(row, 4, create_table_item(str(stats['spheres'])))
            table.setItem(row, 5, create_table_item(str(stats['boxes'])))
            table.setItem(row, 6, create_table_item(str(stats['faces'])))

            # Info
            info_text = f"V{model.col_version} Model"
            table.setItem(row, 7, create_table_item(info_text))

        img_debugger.success(f"COL table populated with {len(col_file.models)} models")
        return True

    except Exception as e:
        img_debugger.error(f"Error populating COL table: {str(e)}")
        return False

def create_table_item(text: str) -> QTableWidgetItem: #vers 8
    """Create a table widget item with proper formatting"""
    item = QTableWidgetItem(str(text))
    item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
    return item

def update_col_info_bar(main_window, col_file: Any, file_path: str): #vers 8
    """Update info bar with COL file information"""
    try:
        # Try to use GUI version first
        try:
            from gui.gui_infobar import update_col_info_bar_enhanced
            update_col_info_bar_enhanced(main_window, col_file, file_path)
            return
        except ImportError:
            img_debugger.warning("gui.gui_infobar not available, using fallback")

        # Fallback info bar update
        if hasattr(main_window, 'status_bar') and hasattr(main_window.status_bar, 'showMessage'):
            file_name = os.path.basename(file_path)
            model_count = len(col_file.models) if hasattr(col_file, 'models') else 0
            message = f"COL: {file_name} - {model_count} models"
            main_window.status_bar.showMessage(message)

        img_debugger.debug(f"COL info bar updated: {os.path.basename(file_path)}")

    except Exception as e:
        img_debugger.error(f"Error updating COL info bar: {str(e)}")


class COLBackgroundLoader(QThread):
    """Background COL file loader with progress reporting"""

    # Signals for progress and completion
    progress_update = pyqtSignal(int, str)  # progress %, status message
    model_loaded = pyqtSignal(int, str)     # model count, model name
    load_complete = pyqtSignal(object)      # COLFile object
    load_error = pyqtSignal(str)            # error message

    def __init__(self, file_path: str = None, parent=None): #vers 2
        super().__init__(parent)
        self.file_path = file_path
        self.col_file = None
        self._should_stop = False

        if file_path:  # Only log if file_path is provided
            img_debugger.debug(f"🔧 COL background loader created for: {os.path.basename(file_path)}")

    
    def cancel_load(self): #vers 1
        """Cancel the loading operation"""
        self._should_stop = True
        img_debugger.debug("🛑 COL loading cancelled by user")
    
    def run(self): #vers 1
        """Main thread execution - load COL file with progress"""
        try:
            self.progress_update.emit(10, "Initializing COL loader...")
            
            if self._should_stop:
                return
            
            # Create COL file object
            self.col_file = COLFile(self.file_path)
            
            self.progress_update.emit(25, "Reading COL file data...")
            
            if self._should_stop:
                return
            
            # Load with progress monitoring
            success = self._load_with_progress()
            
            if self._should_stop:
                return
            
            self.progress_update.emit(100, "COL loading complete")
            
            if success:
                self.load_complete.emit(self.col_file)
                img_debugger.success(f"COL file loaded successfully: {os.path.basename(self.file_path)}")
            else:
                error_msg = self.col_file.load_error or "Unknown loading error"
                self.load_error.emit(error_msg)
                img_debugger.error(f"COL file loading failed: {error_msg}")
                
        except Exception as e:
            error_msg = f"COL loading exception: {str(e)}"
            self.load_error.emit(error_msg)
            img_debugger.error(error_msg)
    

    def setup_threaded_col_loading_safe(main_window): #vers 1
        """Setup threaded COL loading with error handling"""
        try:
            # Add threading setup methods to main window
            main_window.load_col_file_async = lambda file_path: load_col_file_async(main_window, file_path)

            img_debugger.debug("[COL-COL_THREADING] Threaded COL loading setup complete")
            return True

        except Exception as e:
            img_debugger.error(f"[COL-COL_THREADING] Error setting up threaded COL loading: {e}")
            return False


    def _load_with_progress(self) -> bool: #vers 1
        """Load COL file with progress monitoring"""
        try:
            # Wrap the COL file's load method to provide progress feedback
            def progress_load():
                self.progress_update.emit(50, "Parsing COL structure...")
                
                result = self.col_file.load()
                
                if result and self.col_file.models:
                    self.progress_update.emit(75, f"Processing {len(self.col_file.models)} models...")
                    
                    # Report individual models
                    for i, model in enumerate(self.col_file.models):
                        if self._should_stop:
                            return False
                        
                        model_name = model.name or f"Model_{i+1}"
                        progress = 75 + (20 * (i + 1) // len(self.col_file.models))
                        self.progress_update.emit(progress, f"Processing {model_name}...")
                        self.model_loaded.emit(i+1, model_name)
                        
                        # Small delay to prevent UI lockup
                        self.msleep(5)
                
                return result
            
            # Replace the load method temporarily
            original_load = self.col_file.load
            self.col_file.load = progress_load
            
            try:
                return self.col_file.load()
            finally:
                # Restore original load method
                self.col_file.load = original_load
            
        except Exception as e:
            self.progress_update.emit(0, f"Loading error: {str(e)}")
            return False

def load_col_file_async(main_window, file_path: str): #vers 1
    """Load COL file asynchronously with progress feedback"""
    
    # Create progress dialog
    progress_dialog = None
    if hasattr(main_window.gui_layout, 'show_progress'):
        main_window.gui_layout.show_progress(0, "Loading COL file...")
    
    # Create background loader
    loader = COLBackgroundLoader(file_path, main_window)
    
    # Connect signals
    def on_progress(progress, status):
        """Update progress display"""
        if hasattr(main_window.gui_layout, 'update_progress'):
            main_window.gui_layout.update_progress(progress, status)
        
        # Also log to main window if available
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"COL Load: {status}")
        
        img_debugger.debug(f"📈 COL Load Progress: {progress}% - {status}")
    
    def on_model_loaded(count, name):
        """Report model loading"""
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Loaded model {count}: {name}")
        
        img_debugger.debug(f"COL Model {count} loaded: {name}")
    
    def on_load_complete(col_file):
        """Handle successful load completion"""
        try:
            # Hide progress
            if hasattr(main_window.gui_layout, 'hide_progress'):
                main_window.gui_layout.hide_progress()
            
            # Update main window with loaded COL
            main_window.current_col = col_file
            
            # Populate table with COL data
            if hasattr(main_window, 'populate_col_table'):
                main_window.populate_col_table(col_file)
            
            # Update COL tab if available
            if hasattr(main_window, 'col_list_widget'):
                main_window.col_list_widget.set_col_file(col_file)
            
            # Update info bar
            if hasattr(main_window, 'update_col_info_bar'):
                main_window.update_col_info_bar(col_file, file_path)
            
            # Update status
            models_count = len(col_file.models)
            total_objects = sum(len(m.spheres) + len(m.boxes) for m in col_file.models)
            
            status_msg = f"COL loaded: {models_count} models, {total_objects} collision objects"
            
            if hasattr(main_window, 'update_status'):
                main_window.update_status(status_msg)
            elif hasattr(main_window, 'log_message'):
                main_window.log_message(f"✅ {status_msg}")
            
            img_debugger.success(f"✅ COL integration complete: {status_msg}")
            
        except Exception as e:
            img_debugger.error(f"❌ Error in COL load completion: {str(e)}")
            on_load_error(f"Integration error: {str(e)}")
    
    def on_load_error(error_message):
        """Handle load error"""
        try:
            # Hide progress
            if hasattr(main_window.gui_layout, 'hide_progress'):
                main_window.gui_layout.hide_progress()
            
            # Show error message
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.critical(
                main_window, 
                "COL Load Error", 
                f"Failed to load COL file:\n{error_message}"
            )
            
            # Log error
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"❌ COL load failed: {error_message}")
            
            img_debugger.error(f"❌ COL load failed: {error_message}")
            
        except Exception as e:
            img_debugger.error(f"❌ Error in COL error handler: {str(e)}")
    
    # Connect all signals
    loader.progress_update.connect(on_progress)
    loader.model_loaded.connect(on_model_loaded)
    loader.load_complete.connect(on_load_complete)
    loader.load_error.connect(on_load_error)
    
    # Store loader reference to prevent garbage collection
    main_window._col_loader = loader
    
    # Start loading
    loader.start()
    
    img_debugger.debug(f"🚀 Async COL loading started: {os.path.basename(file_path)}")
    
    return loader

# Export functions and classes
__all__ = [
    'COLBackgroundLoader',
    'load_col_file_async'
]
