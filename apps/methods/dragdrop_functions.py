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
                img_archive = None
                if hasattr(self.main_window, 'main_tab_widget'):
                    tab = self.main_window.main_tab_widget.currentWidget()
                    img_archive = getattr(tab, 'file_object', None)
                if img_archive:
                    return self.main_window.import_multiple_files(img_archive, file_paths)
            
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


# --- Tree <-> Tree and Tree <-> Table drag/drop ---
# setup_tree_drag_drop
# setup_table_entry_drag
# _tree_drag_start
# _tree_drop_event
# _table_drag_start
# _table_drop_from_tree

from PyQt6.QtWidgets import QTreeWidget, QTableWidget, QAbstractItemView
from PyQt6.QtCore import QPoint
import shutil


def setup_tree_drag_drop(tree_widget, main_window, side='left'): #vers 1
    """Enable drag FROM and drop TO a dir tree panel"""
    tree_widget.setDragEnabled(True)
    tree_widget.setAcceptDrops(True)
    tree_widget.setDropIndicatorShown(True)
    tree_widget.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
    tree_widget.setDragDropMode(QAbstractItemView.DragDropMode.DragDrop)

    def start_drag(supported_actions):
        selected = tree_widget.selectedItems()
        if not selected:
            return
        paths = []
        for item in selected:
            path = item.data(0, Qt.ItemDataRole.UserRole)
            if path and os.path.exists(path):
                paths.append(path)
        if not paths:
            return
        mime = QMimeData()
        urls = [QUrl.fromLocalFile(p) for p in paths]
        mime.setUrls(urls)
        mime.setText('\n'.join(paths))
        mime.setData('application/x-imgfactory-tree', side.encode())
        drag = QDrag(tree_widget)
        drag.setMimeData(mime)
        drag.exec(Qt.DropAction.CopyAction | Qt.DropAction.MoveAction)

    def _clear_hover(item):
        if item:
            item.setBackground(0, item._orig_bg if hasattr(item, '_orig_bg') else item.background(0))

    def drag_enter(event):
        if event.mimeData().hasUrls() or event.mimeData().hasText():
            event.acceptProposedAction()
        else:
            event.ignore()

    _hovered = [None]  # mutable ref for nested functions

    def drag_move(event):
        mime = event.mimeData()
        if not (mime.hasUrls() or mime.hasText()):
            event.ignore()
            return

        # Auto-scroll near edges - map to viewport coords
        viewport = tree_widget.viewport()
        pos = tree_widget.mapTo(viewport, event.position().toPoint())
        vh = viewport.height()
        scroll_zone = 40
        sb = tree_widget.verticalScrollBar()
        if pos.y() < scroll_zone:
            sb.setValue(sb.value() - 12)
        elif pos.y() > vh - scroll_zone:
            sb.setValue(sb.value() + 12)

        item = tree_widget.itemAt(tree_widget.viewport().mapFrom(
            tree_widget, event.position().toPoint()))

        # Clear previous hover
        if _hovered[0] and _hovered[0] is not item:
            prev = _hovered[0]
            if hasattr(prev, '_orig_bg'):
                prev.setBackground(0, prev._orig_bg)
            _hovered[0] = None

        # Highlight folder under cursor only
        if item:
            path = item.data(0, Qt.ItemDataRole.UserRole)
            if path and os.path.isdir(path):
                if not hasattr(item, '_orig_bg'):
                    item._orig_bg = item.background(0)
                from PyQt6.QtGui import QColor
                from PyQt6.QtWidgets import QApplication
                palette = QApplication.palette()
                highlight = palette.color(palette.ColorRole.Highlight)
                highlight.setAlpha(120)
                item.setBackground(0, highlight)
                _hovered[0] = item
                event.acceptProposedAction()
                return

        event.acceptProposedAction()

    def drag_leave(event):
        if _hovered[0]:
            prev = _hovered[0]
            if hasattr(prev, '_orig_bg'):
                prev.setBackground(0, prev._orig_bg)
            _hovered[0] = None

    def drop_event(event):
        # Clear hover on drop
        if _hovered[0]:
            prev = _hovered[0]
            if hasattr(prev, '_orig_bg'):
                prev.setBackground(0, prev._orig_bg)
            _hovered[0] = None

        mime = event.mimeData()
        dst_item = tree_widget.itemAt(event.position().toPoint())
        if dst_item:
            dst_path = dst_item.data(0, Qt.ItemDataRole.UserRole)
        else:
            dst_path = None

        if not dst_path or not os.path.isdir(dst_path):
            event.ignore()
            return

        paths = []
        if mime.hasUrls():
            paths = [u.toLocalFile() for u in mime.urls() if u.isLocalFile()]
        elif mime.hasText():
            paths = [p for p in mime.text().split('\n') if os.path.exists(p)]

        if not paths:
            event.ignore()
            return

        # Open GTA files directly instead of copy/move
        openable = {'.img', '.col', '.txd'}
        to_open = [p for p in paths if os.path.splitext(p)[1].lower() in openable]
        to_transfer = [p for p in paths if p not in to_open]

        if to_open:
            mw = getattr(getattr(tree_widget, '_browser', None), 'main_window', None)
            if mw and hasattr(mw, 'load_file_unified'):
                for p in to_open:
                    mw.load_file_unified(p)
            event.acceptProposedAction()
            if not to_transfer:
                return

        if not to_transfer:
            return
        paths = to_transfer

        # Ask Copy or Move
        from PyQt6.QtWidgets import QMessageBox
        msg = QMessageBox(tree_widget)
        msg.setWindowTitle("Copy or Move?")
        msg.setText(f"Drop {len(paths)} item(s) into:\n{dst_path}")
        copy_btn = msg.addButton("Copy", QMessageBox.ButtonRole.AcceptRole)
        move_btn = msg.addButton("Move", QMessageBox.ButtonRole.ActionRole)
        msg.addButton("Cancel", QMessageBox.ButtonRole.RejectRole)
        msg.exec()
        clicked = msg.clickedButton()
        if clicked is None or clicked.text() == "Cancel":
            event.ignore()
            return
        do_move = (clicked is move_btn)

        done = 0
        for src in paths:
            try:
                dst = os.path.join(dst_path, os.path.basename(src))
                if do_move:
                    shutil.move(src, dst)
                elif os.path.isdir(src):
                    shutil.copytree(src, dst)
                else:
                    shutil.copy2(src, dst)
                done += 1
            except Exception as e:
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"{'Move' if do_move else 'Copy'} error: {e}")

        action = 'Moved' if do_move else 'Copied'
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"{action} {done} item(s) → {dst_path}")

        if done and hasattr(tree_widget, '_browser'):
            browser = tree_widget._browser
            dests = [os.path.join(dst_path, os.path.basename(p)) for p in paths]
            if hasattr(browser, 'undo_stack'):
                browser.undo_stack.append({'action': 'move' if do_move else 'copy', 'paths': dests})
                if hasattr(browser, 'redo_stack'):
                    browser.redo_stack.clear()

        event.acceptProposedAction()

        if hasattr(tree_widget, '_browser') and hasattr(tree_widget._browser, 'browse_directory'):
            tree_widget._browser.browse_directory(dst_path)

    tree_widget.startDrag = start_drag
    tree_widget.dragEnterEvent = drag_enter
    tree_widget.dragMoveEvent = drag_move
    tree_widget.dragLeaveEvent = drag_leave
    tree_widget.dropEvent = drop_event


def setup_table_entry_drag(table_widget, main_window): #vers 1
    """Enable drag FROM table (IMG/COL entries) and drop TO table (import files)"""
    table_widget.setDragEnabled(True)
    table_widget.setAcceptDrops(True)
    table_widget.setDropIndicatorShown(True)
    table_widget.setDragDropMode(QAbstractItemView.DragDropMode.DragDrop)

    def start_drag(supported_actions):
        selected = table_widget.selectedItems()
        if not selected:
            return
        rows = list({item.row() for item in selected})
        names = []
        for row in rows:
            name_item = table_widget.item(row, 0)
            if name_item:
                names.append(name_item.text())
        if not names:
            return

        # Try to extract entries to temp dir for desktop drag
        urls = []
        img_archive = None
        if hasattr(main_window, 'main_tab_widget'):
            tab = main_window.main_tab_widget.currentWidget()
            img_archive = getattr(tab, 'file_object', None)

        if img_archive and hasattr(img_archive, 'entries'):
            import tempfile
            tmp_dir = tempfile.mkdtemp(prefix='imgfactory_drag_')
            for entry in img_archive.entries:
                name = getattr(entry, 'name', '') or getattr(entry, 'filename', '')
                if name in names:
                    try:
                        out_path = os.path.join(tmp_dir, name)
                        if hasattr(img_archive, 'extract_entry'):
                            img_archive.extract_entry(entry, out_path)
                        elif hasattr(entry, 'data') and entry.data:
                            with open(out_path, 'wb') as f:
                                f.write(entry.data)
                        if os.path.exists(out_path):
                            urls.append(QUrl.fromLocalFile(out_path))
                    except Exception as e:
                        print(f"Drag extract error: {e}")

        mime = QMimeData()
        mime.setData('application/x-imgfactory-entries', '\n'.join(names).encode())
        mime.setText('\n'.join(names))
        if urls:
            mime.setUrls(urls)
        drag = QDrag(table_widget)
        drag.setMimeData(mime)
        drag.exec(Qt.DropAction.CopyAction)

    def drag_enter(event):
        mime = event.mimeData()
        if mime.hasUrls() or mime.hasText():
            event.acceptProposedAction()
        else:
            event.ignore()

    def drag_move(event):
        if event.mimeData().hasUrls() or event.mimeData().hasText():
            event.acceptProposedAction()
        else:
            event.ignore()

    def drop_event(event):
        mime = event.mimeData()
        paths = []
        if mime.hasUrls():
            paths = [u.toLocalFile() for u in mime.urls() if u.isLocalFile()]
        elif mime.hasText() and not mime.hasFormat('application/x-imgfactory-entries'):
            paths = [p for p in mime.text().split('\n') if os.path.exists(p)]

        if paths and hasattr(main_window, 'import_multiple_files'):
            # Get img_archive from current tab
            img_archive = None
            if hasattr(main_window, 'main_tab_widget'):
                tab = main_window.main_tab_widget.currentWidget()
                img_archive = getattr(tab, 'file_object', None)
            if img_archive is None:
                if hasattr(main_window, 'log_message'):
                    main_window.log_message("No open archive to import into")
                event.ignore()
                return
            main_window.import_multiple_files(img_archive, paths)
            event.acceptProposedAction()
        else:
            event.ignore()

    table_widget.startDrag = start_drag
    table_widget.dragEnterEvent = drag_enter
    table_widget.dragMoveEvent = drag_move
    table_widget.dropEvent = drop_event


def setup_tree_as_extract_target(tree_widget, main_window): #vers 2
    """Allow IMG/COL table entries to be dropped onto dir tree to extract them"""
    original_drop = getattr(tree_widget, 'dropEvent', None)
    _hovered = [None]

    def _apply_hover(item):
        if item and not hasattr(item, '_orig_bg'):
            item._orig_bg = item.background(0)
        if item:
            from PyQt6.QtGui import QColor
            from PyQt6.QtWidgets import QApplication
            highlight = QApplication.palette().color(QApplication.palette().ColorRole.Highlight)
            highlight.setAlpha(120)
            item.setBackground(0, highlight)

    def _clear_hover():
        if _hovered[0]:
            prev = _hovered[0]
            if hasattr(prev, '_orig_bg'):
                prev.setBackground(0, prev._orig_bg)
            _hovered[0] = None

    def drag_enter(event):
        if event.mimeData().hasUrls() or event.mimeData().hasFormat('application/x-imgfactory-entries'):
            event.acceptProposedAction()
        else:
            event.ignore()

    def drag_move(event):
        mime = event.mimeData()
        if not (mime.hasUrls() or mime.hasFormat('application/x-imgfactory-entries')):
            event.ignore()
            return
        item = tree_widget.itemAt(event.position().toPoint())
        if _hovered[0] and _hovered[0] is not item:
            _clear_hover()
        if item:
            path = item.data(0, Qt.ItemDataRole.UserRole)
            if path and os.path.isdir(path):
                _apply_hover(item)
                _hovered[0] = item
        event.acceptProposedAction()

    def drag_leave(event):
        _clear_hover()

    def drop_event(event):
        _clear_hover()
        mime = event.mimeData()

        # Handle entry extraction drop from table
        if mime.hasFormat('application/x-imgfactory-entries'):
            dst_item = tree_widget.itemAt(event.position().toPoint())
            dst_path = None
            if dst_item:
                dst_path = dst_item.data(0, Qt.ItemDataRole.UserRole)
            if not dst_path or not os.path.isdir(dst_path):
                event.ignore()
                return

            entry_names = mime.data('application/x-imgfactory-entries').data().decode().split('\n')
            entry_names = [n for n in entry_names if n]

            # Find active tab and extract
            extracted = 0
            if hasattr(main_window, 'main_tab_widget'):
                tab = main_window.main_tab_widget.currentWidget()
                file_object = getattr(tab, 'file_object', None)
                if file_object and hasattr(file_object, 'entries'):
                    for entry in file_object.entries:
                        name = getattr(entry, 'name', '') or getattr(entry, 'filename', '')
                        if name in entry_names:
                            try:
                                out_path = os.path.join(dst_path, name)
                                if hasattr(file_object, 'extract_entry'):
                                    file_object.extract_entry(entry, out_path)
                                elif hasattr(entry, 'data') and entry.data:
                                    with open(out_path, 'wb') as f:
                                        f.write(entry.data)
                                extracted += 1
                            except Exception as e:
                                if hasattr(main_window, 'log_message'):
                                    main_window.log_message(f"Extract error: {e}")

            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Extracted {extracted} entry(s) → {dst_path}")
            event.acceptProposedAction()
            return

        # Fall through to normal file drop
        if original_drop:
            original_drop(event)

    tree_widget.setAcceptDrops(True)
    tree_widget.dragEnterEvent = drag_enter
    tree_widget.dragMoveEvent = drag_move
    tree_widget.dragLeaveEvent = drag_leave
    tree_widget.dropEvent = drop_event
