# this belongs in components/unified_signal_handler.py - Version: 1
# X-Seti - July02 2025 - Single signal handler for all Img Factory 1.5 components

from PyQt6.QtWidgets import QTableWidget, QTableWidgetItem
from PyQt6.QtCore import QObject, pyqtSignal
from typing import Optional, Callable, Dict, Any, List


class UnifiedSignalHandler(QObject):
    """
    Single signal handler for all table operations throughout the project
    Eliminates duplicate signal handlers and prevents conflicts
    """
    
    # Single set of signals used by all components
    selection_changed = pyqtSignal(int, str)  # count, table_name
    item_double_clicked = pyqtSignal(str, int, str)  # table_name, row, filename
    status_update_requested = pyqtSignal(str)  # status_message
    
    def __init__(self, parent=None):
        super().__init__(parent)
        
        # Registry of connected tables and their callbacks
        self._table_registry: Dict[str, Dict[str, Any]] = {}
        
        # Prevent duplicate connections
        self._connected_tables: set = set()
        
        # Last known selection state to prevent duplicate updates
        self._last_selections: Dict[str, List[int]] = {}
    
    def connect_table_signals(self, table_name: str, table_widget: QTableWidget,
                            parent_instance=None, 
                            selection_callback: Optional[Callable] = None,
                            double_click_callback: Optional[Callable] = None) -> bool:
        """
        Connect table signals using the unified handler
        Replaces all individual _on_selection_changed and on_selection_changed methods
        """
        
        if table_name in self._connected_tables:
            print(f"Warning: Table '{table_name}' signals already connected")
            return False
        
        # Disconnect any existing signals to prevent conflicts
        try:
            table_widget.itemSelectionChanged.disconnect()
            table_widget.itemDoubleClicked.disconnect()
        except TypeError:
            # No signals connected yet, that's fine
            pass
        
        # Store registration info
        self._table_registry[table_name] = {
            'table': table_widget,
            'parent': parent_instance,
            'selection_callback': selection_callback,
            'double_click_callback': double_click_callback
        }
        
        # Connect to unified handlers
        table_widget.itemSelectionChanged.connect(
            lambda: self._handle_selection_changed(table_name)
        )
        table_widget.itemDoubleClicked.connect(
            lambda item: self._handle_item_double_clicked(table_name, item)
        )
        
        # Initialize selection tracking
        self._last_selections[table_name] = []
        
        # Mark as connected
        self._connected_tables.add(table_name)
        
        return True
    
    def _handle_selection_changed(self, table_name: str):
        """
        Single selection change handler for ALL tables
        Replaces all _on_selection_changed and on_selection_changed methods
        """
        
        if table_name not in self._table_registry:
            return
        
        table_info = self._table_registry[table_name]
        table = table_info['table']
        
        # Get current selection
        selected_rows = [index.row() for index in table.selectionModel().selectedRows()]
        selection_count = len(selected_rows)
        
        # Check if selection actually changed
        if selected_rows == self._last_selections.get(table_name, []):
            return  # No change, skip processing
        
        # Update tracking
        self._last_selections[table_name] = selected_rows
        
        # Create status message
        if selection_count == 0:
            status_message = "Ready"
        elif selection_count == 1:
            status_message = "1 entry selected"
        else:
            status_message = f"{selection_count} entries selected"
        
        # Update status through parent if it has the method
        parent = table_info['parent']
        if parent:
            # Try different status update methods that exist in the codebase
            if hasattr(parent, 'status_label') and hasattr(parent.status_label, 'setText'):
                parent.status_label.setText(status_message)
            elif hasattr(parent, 'update_status'):
                parent.update_status(status_message)
            elif hasattr(parent, 'log_message'):
                parent.log_message(f"Selection: {status_message}")
        
        # Call custom selection callback if provided
        selection_callback = table_info['selection_callback']
        if selection_callback and callable(selection_callback):
            try:
                selection_callback(selected_rows, selection_count)
            except Exception as e:
                print(f"Error in selection callback for {table_name}: {e}")
        
        # Emit unified signal
        self.selection_changed.emit(selection_count, table_name)
        self.status_update_requested.emit(status_message)
    
    def _handle_item_double_clicked(self, table_name: str, item: QTableWidgetItem):
        """
        Single double-click handler for ALL tables
        Replaces all _on_item_double_clicked and on_item_double_clicked methods
        """
        
        if not item or table_name not in self._table_registry:
            return
        
        table_info = self._table_registry[table_name]
        parent = table_info['parent']
        
        row = item.row()
        filename = item.text()
        
        # Standard logging if parent supports it
        if parent and hasattr(parent, 'log_message'):
            parent.log_message(f"Double-clicked {table_name}: {filename}")
        
        # Call custom double-click callback if provided
        double_click_callback = table_info['double_click_callback']
        if double_click_callback and callable(double_click_callback):
            try:
                double_click_callback(row, filename, item)
            except Exception as e:
                print(f"Error in double-click callback for {table_name}: {e}")
        
        # Emit unified signal
        self.item_double_clicked.emit(table_name, row, filename)
    
    def disconnect_table_signals(self, table_name: str) -> bool:
        """Disconnect signals for a specific table"""
        
        if table_name not in self._connected_tables:
            return False
        
        if table_name in self._table_registry:
            table = self._table_registry[table_name]['table']
            
            # Disconnect signals
            try:
                table.itemSelectionChanged.disconnect()
                table.itemDoubleClicked.disconnect()
            except TypeError:
                pass  # Already disconnected
            
            # Clean up tracking
            del self._table_registry[table_name]
            if table_name in self._last_selections:
                del self._last_selections[table_name]
        
        self._connected_tables.discard(table_name)
        return True
    
    def get_current_selection(self, table_name: str) -> List[int]:
        """Get current selection for a table"""
        return self._last_selections.get(table_name, [])
    
    def get_selection_count(self, table_name: str) -> int:
        """Get selection count for a table"""
        return len(self._last_selections.get(table_name, []))
    
    def clear_selection(self, table_name: str) -> bool:
        """Programmatically clear selection for a table"""
        if table_name not in self._table_registry:
            return False
        
        table = self._table_registry[table_name]['table']
        table.clearSelection()
        return True
    
    def select_rows(self, table_name: str, rows: List[int]) -> bool:
        """Programmatically select specific rows"""
        if table_name not in self._table_registry:
            return False
        
        table = self._table_registry[table_name]['table']
        table.clearSelection()
        
        for row in rows:
            if 0 <= row < table.rowCount():
                table.selectRow(row)
        
        return True
    
    def is_table_connected(self, table_name: str) -> bool:
        """Check if table signals are connected"""
        return table_name in self._connected_tables
    
    def get_connected_tables(self) -> List[str]:
        """Get list of connected table names"""
        return list(self._connected_tables)
    
    def reconnect_all_signals(self):
        """Reconnect all signals (useful after theme changes etc)"""
        # Store current registry
        current_registry = self._table_registry.copy()
        
        # Disconnect all
        for table_name in list(self._connected_tables):
            self.disconnect_table_signals(table_name)
        
        # Reconnect all
        for table_name, info in current_registry.items():
            self.connect_table_signals(
                table_name,
                info['table'],
                info['parent'],
                info['selection_callback'],
                info['double_click_callback']
            )


# Global instance - use this throughout the project
signal_handler = UnifiedSignalHandler()


def connect_table_signals(table_name: str, table_widget: QTableWidget,
                        parent_instance=None,
                        selection_callback: Optional[Callable] = None,
                        double_click_callback: Optional[Callable] = None) -> bool:
    """
    Global function to connect table signals
    Use this instead of manual signal connections in all files
    
    Example usage:
    connect_table_signals("main", self.entries_table, self)
    connect_table_signals("col", self.col_table, self, self.custom_selection_handler)
    """
    return signal_handler.connect_table_signals(
        table_name, table_widget, parent_instance, 
        selection_callback, double_click_callback
    )


def disconnect_table_signals(table_name: str) -> bool:
    """
    Global function to disconnect table signals
    Use this for cleanup
    """
    return signal_handler.disconnect_table_signals(table_name)


def get_selection_count(table_name: str) -> int:
    """
    Global function to get selection count
    Use this instead of manual selection model queries
    """
    return signal_handler.get_selection_count(table_name)


def clear_table_selection(table_name: str) -> bool:
    """
    Global function to clear table selection
    """
    return signal_handler.clear_selection(table_name)