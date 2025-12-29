#this belongs in methods/dragdrop_functions.py - Version: 1
# X-Seti - September04 2025 - IMG Factory 1.5 - Drag and Drop Support

"""
Drag and Drop Support - Shared Functions
Universal drag-and-drop functionality for IMG Factory
Supports: Files IN/OUT, Folders IN/OUT, IMG entries, Desktop integration
"""

import os
import mimetypes
from pathlib import Path
from typing import List, Optional, Dict, Any, Callable
from PyQt6.QtWidgets import QWidget, QApplication
from PyQt6.QtCore import Qt, QMimeData, QUrl, pyqtSignal
from PyQt6.QtGui import QDragEnterEvent, QDropEvent, QDrag, QPixmap, QPainter

##Methods list -
# setup_drag_drop_widget
# handle_drag_enter
# handle_drop_event
# create_drag_operation
# get_dropped_files
# get_dropped_folders
# filter_supported_files
# create_file_drag_data
# create_folder_drag_data
# extract_entries_to_temp
# setup_main_window_drag_drop
# setup_table_drag_drop
# DragDropHandler (class)

##Classes -
# DragDropHandler

class DragDropHandler:
    """Universal drag-and-drop handler for IMG Factory"""
    
    def __init__(self, main_window):
        self.main_window = main_window
        self.supported_extensions = {
            '.dff', '.txd', '.col', '.ipl', '.ide', '.dat', '.cfg',
            '.img', '.dir', '.zip', '.rar', '.7z', '.txt', '.ini'
        }
        self.drag_callbacks = {}
        self.drop_callbacks = {}
    
    def register_drop_callback(self, target_type: str, callback: Callable): #vers 1
        """Register callback for drop events"""
        self.drop_callbacks[target_type] = callback
    
    def register_drag_callback(self, source_type: str, callback: Callable): #vers 1
        """Register callback for drag start events"""
        self.drag_callbacks[source_type] = callback
    
    def handle_drag_enter(self, event: QDragEnterEvent) -> bool: #vers 1
        """Handle drag enter event"""
        try:
            mime_data = event.mimeData()
            
            # Check for files/folders
            if mime_data.hasUrls():
                urls = mime_data.urls()
                valid_items = 0
                
                for url in urls:
                    if url.isLocalFile():
                        file_path = url.toLocalFile()
                        
                        if os.path.isfile(file_path):
                            # Check file extension
                            ext = Path(file_path).suffix.lower()
                            if ext in self.supported_extensions:
                                valid_items += 1
                        elif os.path.isdir(file_path):
                            # Folders are always accepted
                            valid_items += 1
                
                if valid_items > 0:
                    event.acceptProposedAction()
                    return True
            
            # Check for text data (file lists, etc.)
            if mime_data.hasText():
                text_data = mime_data.text()
                if self._is_valid_file_list(text_data):
                    event.acceptProposedAction()
                    return True
            
            event.ignore()
            return False
            
        except Exception as e:
            self._log_debug(f"Drag enter error: {e}")
            event.ignore()
            return False
    
    def handle_drop_event(self, event: QDropEvent, target_type: str = "main") -> bool: #vers 1
        """Handle drop event"""
        try:
            mime_data = event.mimeData()
            
            # Get dropped items
            dropped_files = []
            dropped_folders = []
            
            if mime_data.hasUrls():
                for url in mime_data.urls():
                    if url.isLocalFile():
                        file_path = url.toLocalFile()
                        
                        if os.path.isfile(file_path):
                            dropped_files.append(file_path)
                        elif os.path.isdir(file_path):
                            dropped_folders.append(file_path)
            
            # Process dropped items
            total_processed = 0
            
            if dropped_files:
                # Filter supported files
                supported_files = self._filter_supported_files(dropped_files)
                if supported_files:
                    callback = self.drop_callbacks.get('files', self._default_file_drop)
                    if callback(supported_files, target_type):
                        total_processed += len(supported_files)
            
            if dropped_folders:
                callback = self.drop_callbacks.get('folders', self._default_folder_drop)
                if callback(dropped_folders, target_type):
                    total_processed += len(dropped_folders)
            
            if total_processed > 0:
                event.acceptProposedAction()
                self._log_debug(f"Drop successful: {total_processed} items processed")
                return True
            else:
                event.ignore()
                return False
                
        except Exception as e:
            self._log_debug(f"Drop event error: {e}")
            event.ignore()
            return False
    
    def create_file_drag(self, file_paths: List[str], widget: QWidget) -> bool: #vers 1
        """Create drag operation for files"""
        try:
            # Create mime data
            mime_data = QMimeData()
            
            # Add file URLs
            urls = []
            for file_path in file_paths:
                if os.path.exists(file_path):
                    urls.append(QUrl.fromLocalFile(file_path))
            
            if not urls:
                return False
            
            mime_data.setUrls(urls)
            
            # Add text representation
            text_data = "\n".join(file_paths)
            mime_data.setText(text_data)
            
            # Create drag object
            drag = QDrag(widget)
            drag.setMimeData(mime_data)
            
            # Set drag pixmap (optional)
            pixmap = self._create_drag_pixmap(file_paths)
            if pixmap:
                drag.setPixmap(pixmap)
            
            # Execute drag
            drop_action = drag.exec(Qt.DropAction.CopyAction | Qt.DropAction.MoveAction)
            
            return drop_action != Qt.DropAction.IgnoreAction
            
        except Exception as e:
            self._log_debug(f"File drag error: {e}")
            return False
    
    def extract_entries_for_drag(self, entries: List, temp_folder: str) -> List[str]: #vers 1
        """Extract IMG entries to temp folder for drag operations"""
        try:
            extracted_files = []
            
            # Get current file object
            from apps.methods.tab_system import get_current_file_from_active_tab
            file_object, file_type = get_current_file_from_active_tab(self.main_window)
            
            if file_type != 'IMG' or not file_object:
                return []
            
            # Check if we can read entry data
            if not hasattr(file_object, 'read_entry_data'):
                return []
            
            # Create temp folder
            os.makedirs(temp_folder, exist_ok=True)
            
            for entry in entries:
                try:
                    entry_name = getattr(entry, 'name', 'unknown')
                    
                    # Read entry data
                    entry_data = file_object.read_entry_data(entry)
                    
                    if entry_data:
                        # Write to temp file
                        temp_file_path = os.path.join(temp_folder, entry_name)
                        with open(temp_file_path, 'wb') as f:
                            f.write(entry_data)
                        
                        extracted_files.append(temp_file_path)
                        
                except Exception as e:
                    self._log_debug(f"Failed to extract {entry_name}: {e}")
            
            return extracted_files
            
        except Exception as e:
            self._log_debug(f"Extract entries error: {e}")
            return []
    
    def _filter_supported_files(self, file_paths: List[str]) -> List[str]: #vers 1
        """Filter files by supported extensions"""
        supported_files = []
        
        for file_path in file_paths:
            ext = Path(file_path).suffix.lower()
            if ext in self.supported_extensions:
                supported_files.append(file_path)
        
        return supported_files
    
    def _is_valid_file_list(self, text_data: str) -> bool: #vers 1
        """Check if text data contains valid file list"""
        try:
            lines = text_data.strip().split('\n')
            valid_files = 0
            
            for line in lines:
                line = line.strip()
                if os.path.exists(line):
                    valid_files += 1
            
            return valid_files > 0
            
        except:
            return False
    
    def _create_drag_pixmap(self, file_paths: List[str]) -> Optional[QPixmap]: #vers 1
        """Create drag pixmap for visual feedback"""
        try:
            # Simple pixmap showing file count
            pixmap = QPixmap(100, 30)
            pixmap.fill(Qt.GlobalColor.transparent)
            
            painter = QPainter(pixmap)
            painter.setRenderHint(QPainter.RenderHint.Antialiasing)
            
            # Draw background
            painter.fillRect(0, 0, 100, 30, Qt.GlobalColor.lightGray)
            
            # Draw text
            painter.setPen(Qt.GlobalColor.black)
            text = f"{len(file_paths)} files"
            painter.drawText(10, 20, text)
            
            painter.end()
            return pixmap
            
        except Exception as e:
            self._log_debug(f"Create drag pixmap error: {e}")
            return None
    
    def _default_file_drop(self, file_paths: List[str], target_type: str) -> bool: #vers 1
        """Default file drop handler"""
        try:
            self._log_debug(f"Default file drop: {len(file_paths)} files to {target_type}")
            
            # Try to import files if we have an active IMG
            if hasattr(self.main_window, 'import_multiple_files'):
                return self.main_window.import_multiple_files(file_paths)
            
            return False
            
        except Exception as e:
            self._log_debug(f"Default file drop error: {e}")
            return False
    
    def _default_folder_drop(self, folder_paths: List[str], target_type: str) -> bool: #vers 1
        """Default folder drop handler"""
        try:
            self._log_debug(f"Default folder drop: {len(folder_paths)} folders to {target_type}")
            
            # Try to import folder contents
            if hasattr(self.main_window, 'import_folder_contents'):
                for folder_path in folder_paths:
                    # Set folder for import and trigger
                    if hasattr(self.main_window, '_set_import_folder'):
                        self.main_window._set_import_folder(folder_path)
                    return self.main_window.import_folder_contents()
            
            return False
            
        except Exception as e:
            self._log_debug(f"Default folder drop error: {e}")
            return False
    
    def _log_debug(self, message: str): #vers 1
        """Debug logging"""
        if hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"ðŸŽ¯ [DRAG-DROP] {message}")
        else:
            print(f"[DRAG-DROP] {message}")


def setup_drag_drop_widget(widget: QWidget, handler: DragDropHandler, target_type: str = "main") -> bool: #vers 1
    """Setup drag-and-drop for a widget"""
    try:
        # Enable drag and drop
        if widget and hasattr(widget, 'setAcceptDrops'):
            widget.setAcceptDrops(True)
        
        # Store handler and target type
        widget._drag_drop_handler = handler
        widget._target_type = target_type
        
        # Override drag/drop events
        original_drag_enter = widget.dragEnterEvent if hasattr(widget, 'dragEnterEvent') else lambda e: None
        original_drop = widget.dropEvent if hasattr(widget, 'dropEvent') else lambda e: None
        
        def new_drag_enter_event(event):
            if handler.handle_drag_enter(event):
                return
            original_drag_enter(event)
        
        def new_drop_event(event):
            if handler.handle_drop_event(event, target_type):
                return
            original_drop(event)
        
        widget.dragEnterEvent = new_drag_enter_event
        widget.dropEvent = new_drop_event
        
        return True
        
    except Exception as e:
        print(f"Setup drag-drop widget error: {e}")
        return False


def setup_main_window_drag_drop(main_window) -> bool: #vers 1
    """Setup drag-and-drop for main window"""
    try:
        # Create handler
        handler = DragDropHandler(main_window)
        
        # Setup main window
        setup_drag_drop_widget(main_window, handler, "main_window")
        
        # Setup GUI layout widget if available
        if hasattr(main_window, 'gui_layout'):
            setup_drag_drop_widget(main_window.gui_layout, handler, "gui_layout")
            
            # Setup table widget if available
            if hasattr(main_window.gui_layout, 'table'):
                setup_drag_drop_widget(main_window.gui_layout.table, handler, "table")
        
        # Store handler in main window
        main_window._drag_drop_handler = handler
        
        # Add drag-drop methods to main window
        main_window.create_file_drag = lambda files, widget=None: handler.create_file_drag(files, widget or main_window)
        main_window.extract_entries_for_drag = lambda entries, temp_folder: handler.extract_entries_for_drag(entries, temp_folder)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("ðŸŽ¯ Drag-and-drop support initialized")
        
        return True
        
    except Exception as e:
        print(f"Setup main window drag-drop error: {e}")
        return False


def setup_table_drag_drop(table_widget, main_window) -> bool: #vers 1
    """Setup drag-and-drop specifically for table widget"""
    try:
        if not hasattr(main_window, '_drag_drop_handler'):
            # Create handler if it doesn't exist
            handler = DragDropHandler(main_window)
            main_window._drag_drop_handler = handler
        else:
            handler = main_window._drag_drop_handler
        
        # Setup table drag-and-drop
        setup_drag_drop_widget(table_widget, handler, "table")
        
        # Register table-specific callbacks
        def table_file_drop(file_paths, target_type):
            """Import files when dropped on table"""
            if hasattr(main_window, 'import_multiple_files'):
                return main_window.import_multiple_files(file_paths)
            return False
        
        def table_folder_drop(folder_paths, target_type):
            """Import folder contents when dropped on table"""  
            if hasattr(main_window, 'import_folder_contents'):
                for folder_path in folder_paths:
                    # This could be enhanced to set the folder path
                    pass
                return main_window.import_folder_contents()
            return False
        
        handler.register_drop_callback('files', table_file_drop)
        handler.register_drop_callback('folders', table_folder_drop)
        
        return True
        
    except Exception as e:
        print(f"Setup table drag-drop error: {e}")
        return False


def integrate_drag_drop_system(main_window) -> bool: #vers 1
    """Integrate complete drag-and-drop system"""
    try:
        # Setup main window drag-drop
        success = setup_main_window_drag_drop(main_window)
        
        if success and hasattr(main_window, 'log_message'):
            main_window.log_message("âœ… Complete drag-and-drop system integrated")
            main_window.log_message("   â€¢ Files: Drag files onto IMG Factory to import")
            main_window.log_message("   â€¢ Folders: Drag folders onto IMG Factory to import contents") 
            main_window.log_message("   â€¢ Entries: Drag IMG entries to desktop to extract")
            main_window.log_message("   â€¢ Cross-platform: Windows Explorer, Dolphin, Caja supported")
        
        return success
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"âŒ Drag-drop integration failed: {str(e)}")
        return False


# Export functions
__all__ = [
    'DragDropHandler',
    'setup_drag_drop_widget', 
    'setup_main_window_drag_drop',
    'setup_table_drag_drop',
    'integrate_drag_drop_system'
]
