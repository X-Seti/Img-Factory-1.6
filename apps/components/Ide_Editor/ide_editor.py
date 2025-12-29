#this belongs in components/IDE_Editor.py - Version: 1
# X-Seti - August14 2025 - IMG Factory 1.5 - IDE Editor

"""
IDE Editor - Item Definition Editor with built-in help guide
Provides complete IDE editing capabilities with model viewer and property editor
Includes comprehensive help system explaining all IDE properties and functions
"""

import os
import sys
from typing import Optional, List, Dict, Any, Tuple
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QSplitter, QTabWidget,
    QTableWidget, QTableWidgetItem, QHeaderView, QPushButton,
    QLabel, QLineEdit, QSpinBox, QDoubleSpinBox, QComboBox,
    QGroupBox, QCheckBox, QTextEdit, QFileDialog, QMessageBox,
    QStatusBar, QMenuBar, QToolBar, QFormLayout, QScrollArea,
    QWidget, QProgressBar, QTreeWidget, QTreeWidgetItem, QMenu,
    QApplication
)
from PyQt6.QtCore import Qt, pyqtSignal, QTimer
from PyQt6.QtGui import QAction, QFont, QIcon, QPixmap, QKeySequence, QShortcut

# Add project root to path for standalone execution
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(current_dir)
sys.path.insert(0, project_root)

# Import with fallback for standalone execution
try:
    from apps.debug.debug_functions import img_debugger
except ImportError:
    from img_debug_functions import img_debugger

##Methods list -
# create_ide_help_guide
# parse_ide_line
# validate_ide_entry

##Classes -
# IDEEditor
# IDEHelpWidget
# IDETableWidget

class IDEHelpWidget(QWidget): #vers 1
    """Built-in help guide for IDE Editor"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setup_help_ui()
    
    def setup_help_ui(self): #vers 1
        """Setup help interface"""
        layout = QVBoxLayout(self)
        
        # Help title
        title = QLabel("IDE Editor Help Guide")
        title.setFont(QFont("Arial", 14, QFont.Weight.Bold))
        layout.addWidget(title)
        
        # Scrollable help content
        scroll = QScrollArea()
        help_widget = QWidget()
        help_layout = QVBoxLayout(help_widget)
        
        # IDE Column explanations
        help_content = self.create_ide_help_content()
        help_text = QTextEdit()
        help_text.setHtml(help_content)
        help_text.setReadOnly(True)
        help_layout.addWidget(help_text)
        
        scroll.setWidget(help_widget)
        layout.addWidget(scroll)
    
    def create_ide_help_content(self) -> str: #vers 1
        """Create comprehensive IDE help content"""
        return """
        <h2>IDE (Item Definition) File Format</h2>
        <p>IDE files define object placement and properties in GTA games. Each line represents one object definition.</p>
        
        <h3>Table Columns:</h3>
        <table border="1" cellpadding="5">
        <tr><th>Column</th><th>Description</th><th>Values</th></tr>
        <tr><td><b>ID</b></td><td>Unique object identifier</td><td>0-65535 (integer)</td></tr>
        <tr><td><b>ModelName</b></td><td>DFF model filename (without .dff)</td><td>Text, max 24 chars</td></tr>
        <tr><td><b>TxdName</b></td><td>Texture dictionary name (without .txd)</td><td>Text, max 24 chars</td></tr>
        <tr><td><b>MeshCount</b></td><td>Number of meshes in model</td><td>1-10 (typical range)</td></tr>
        <tr><td><b>DrawDist</b></td><td>Draw distance in game units</td><td>50-1000+ (float)</td></tr>
        <tr><td><b>Flags</b></td><td>Object behavior flags (hex)</td><td>0x0 to 0xFFFFFFFF</td></tr>
        </table>
        
        <h3>Common Flag Values:</h3>
        <ul>
        <li><b>0x0</b> - Standard object, no special properties</li>
        <li><b>0x1</b> - Collision enabled</li>
        <li><b>0x2</b> - LOD (Level of Detail) model</li>
        <li><b>0x4</b> - Alpha transparency</li>
        <li><b>0x8</b> - Breakable object</li>
        <li><b>0x10</b> - Animated object</li>
        <li><b>0x20</b> - Damaged version</li>
        </ul>
        
        <h3>Draw Distance Guidelines:</h3>
        <ul>
        <li><b>50-100</b> - Small objects (signs, props)</li>
        <li><b>100-300</b> - Medium objects (cars, furniture)</li>
        <li><b>300-500</b> - Large objects (buildings, bridges)</li>
        <li><b>500+</b> - Massive objects (skyscrapers, landmarks)</li>
        </ul>
        
        <h3>Best Practices:</h3>
        <ul>
        <li>Keep ModelName and TxdName under 24 characters</li>
        <li>Use sequential ID numbers to avoid conflicts</li>
        <li>Match DrawDist to object importance and size</li>
        <li>Test flag combinations carefully</li>
        <li>Use LOD models for performance optimization</li>
        </ul>
        
        <h3>Editor Functions:</h3>
        <ul>
        <li><b>Sort by IDE</b> - Reorders COL/IMG lists to match IDE order</li>
        <li><b>Validate</b> - Checks for ID conflicts and invalid values</li>
        <li><b>Auto-Complete</b> - Suggests ModelName/TxdName from loaded IMG files</li>
        <li><b>Export</b> - Saves IDE file with proper formatting</li>
        <li><b>Import</b> - Loads existing IDE files for editing</li>
        </ul>
        
        <p><i>Reference: <a href="https://gtamods.com/wiki/Item_Definition">GTAMods IDE Documentation</a></i></p>
        """

class IDETableWidget(QTableWidget): #vers 1
    """Enhanced table widget for IDE entries"""
    
    ide_selection_changed = pyqtSignal(list)  # Emits selected model names
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setup_ide_table()
        
    def setup_ide_table(self): #vers 1
        """Setup IDE table structure"""
        # Define columns
        headers = ["ID", "ModelName", "TxdName", "MeshCount", "DrawDist", "Flags"]
        self.setColumnCount(len(headers))
        self.setHorizontalHeaderLabels(headers)
        
        # Set column widths
        self.setColumnWidth(0, 80)   # ID
        self.setColumnWidth(1, 180)  # ModelName
        self.setColumnWidth(2, 180)  # TxdName
        self.setColumnWidth(3, 100)  # MeshCount
        self.setColumnWidth(4, 100)  # DrawDist
        self.setColumnWidth(5, 120)  # Flags
        
        # Configure table properties
        self.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self.setSelectionMode(QTableWidget.SelectionMode.ExtendedSelection)
        self.setAlternatingRowColors(True)
        self.setSortingEnabled(True)
        
        # Enable context menu
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self.show_context_menu)
        
        # Connect selection changed signal
        self.selectionModel().selectionChanged.connect(self.on_selection_changed)
    
    def show_context_menu(self, position): #vers 1
        """Show right-click context menu"""
        if self.itemAt(position) is None:
            return
        
        menu = QMenu(self)
        
        # Edit actions
        copy_action = QAction("Copy", self)
        copy_action.setShortcut("Ctrl+C")
        copy_action.triggered.connect(self.copy_selection)
        menu.addAction(copy_action)
        
        paste_action = QAction("Paste", self)
        paste_action.setShortcut("Ctrl+V")
        paste_action.triggered.connect(self.paste_selection)
        menu.addAction(paste_action)
        
        menu.addSeparator()
        
        # Entry actions
        add_action = QAction("Add Entry", self)
        add_action.triggered.connect(self.add_entry)
        menu.addAction(add_action)
        
        delete_action = QAction("Delete Entry", self)
        delete_action.setShortcut("Delete")
        delete_action.triggered.connect(self.delete_entry)
        menu.addAction(delete_action)
        
        menu.addSeparator()
        
        # Selection actions
        select_all_action = QAction("Select All", self)
        select_all_action.setShortcut("Ctrl+A")
        select_all_action.triggered.connect(self.selectAll)
        menu.addAction(select_all_action)
        
        menu.addSeparator()
        
        # Validation
        validate_action = QAction("Validate Entry", self)
        validate_action.triggered.connect(self.validate_selected)
        menu.addAction(validate_action)
        
        menu.exec(self.mapToGlobal(position))
    
    def copy_selection(self): #vers 1
        """Copy selected entries to clipboard"""
        selected_rows = []
        for item in self.selectedItems():
            if item.row() not in selected_rows:
                selected_rows.append(item.row())
        
        if not selected_rows:
            return
        
        # Build clipboard text
        clipboard_data = []
        for row in sorted(selected_rows):
            row_data = []
            for col in range(self.columnCount()):
                item = self.item(row, col)
                row_data.append(item.text() if item else "")
            clipboard_data.append("\t".join(row_data))
        
        clipboard_text = "\n".join(clipboard_data)
        QApplication.clipboard().setText(clipboard_text)
    
    def paste_selection(self): #vers 1
        """Paste entries from clipboard"""
        clipboard_text = QApplication.clipboard().text()
        if not clipboard_text:
            return
        
        # Parse clipboard data
        lines = clipboard_text.strip().split('\n')
        current_row = self.currentRow()
        if current_row < 0:
            current_row = self.rowCount()
        
        for i, line in enumerate(lines):
            parts = line.split('\t')
            if len(parts) >= 6:
                row = current_row + i
                if row >= self.rowCount():
                    self.setRowCount(row + 1)
                
                for col, text in enumerate(parts[:6]):
                    self.setItem(row, col, QTableWidgetItem(text))
    
    def add_entry(self): #vers 1
        """Add new entry"""
        if self.parent() and hasattr(self.parent(), 'add_ide_entry'):
            self.parent().add_ide_entry()
    
    def delete_entry(self): #vers 1
        """Delete selected entry"""
        if self.parent() and hasattr(self.parent(), 'delete_ide_entry'):
            self.parent().delete_ide_entry()
    
    def validate_selected(self): #vers 1
        """Validate selected entries"""
        selected_rows = []
        for item in self.selectedItems():
            if item.row() not in selected_rows:
                selected_rows.append(item.row())
        
        if not selected_rows:
            return
        
        errors = []
        for row in selected_rows:
            id_item = self.item(row, 0)
            model_item = self.item(row, 1)
            
            if id_item and model_item:
                try:
                    entry_id = int(id_item.text())
                    model_name = model_item.text()
                    
                    if entry_id < 0 or entry_id > 65535:
                        errors.append(f"Row {row+1}: Invalid ID {entry_id}")
                    
                    if len(model_name) > 24:
                        errors.append(f"Row {row+1}: Model name too long")
                        
                except ValueError:
                    errors.append(f"Row {row+1}: Invalid ID format")
        
        if errors:
            QMessageBox.warning(self, "Validation Errors", "\n".join(errors))
        else:
            QMessageBox.information(self, "Validation", "Selected entries are valid!")
    
    def on_selection_changed(self): #vers 1
        """Handle selection changes and emit model names"""
        selected_models = []
        for row in range(self.rowCount()):
            if self.selectionModel().isRowSelected(row, self.rootIndex()):
                model_item = self.item(row, 1)  # ModelName column
                if model_item:
                    selected_models.append(model_item.text())
        
        self.ide_selection_changed.emit(selected_models)

class IDEEditor(QDialog): #vers 1
    """Main IDE Editor dialog"""
    
    # Signals for integration
    sort_by_ide_requested = pyqtSignal(list)  # Emits model order
    selection_sync_requested = pyqtSignal(list)  # Emits selected models
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.parent_window = parent
        self.ide_data = []
        self.current_file_path = None
        self.setup_ui()
        
    def setup_ui(self): #vers 1
        """Setup IDE Editor interface"""
        self.setWindowTitle("IDE Editor - IMG Factory 1.5")
        self.setGeometry(100, 100, 1200, 800)
        
        # Main layout with proper margins
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(5, 5, 5, 5)
        main_layout.setSpacing(5)
        
        # Top toolbar with dropdown menu
        toolbar_layout = QHBoxLayout()
        
        # Title label
        title_label = QLabel("IDE Entries:")
        title_label.setFont(QFont("Arial", 12, QFont.Weight.Bold))
        toolbar_layout.addWidget(title_label)
        
        toolbar_layout.addStretch()  # Push dropdown to right
        
        # Dropdown menu button on top right
        self.menu_button = QPushButton("⚙️ Menu")
        self.menu_button.setMinimumSize(100, 30)
        self.menu_button.clicked.connect(self.show_dropdown_menu)
        toolbar_layout.addWidget(self.menu_button)
        
        main_layout.addLayout(toolbar_layout)
        
        # Main vertical splitter
        splitter = QSplitter(Qt.Orientation.Vertical)
        
        # Top panel - IDE table (takes most space)
        top_panel = self.create_table_panel()
        splitter.addWidget(top_panel)
        
        # Bottom panel - Status/log window (5 lines height)
        bottom_panel = self.create_status_panel()
        splitter.addWidget(bottom_panel)
        
        # Set splitter proportions (top gets most space, bottom is small)
        splitter.setSizes([650, 100])
        main_layout.addWidget(splitter)
        
        # Status bar
        self.status_bar = QStatusBar()
        main_layout.addWidget(self.status_bar)
        self.status_bar.showMessage("IDE Editor ready")
        
        img_debugger.debug("IDE Editor UI setup complete")
    
    def show_dropdown_menu(self): #vers 1
        """Show dropdown menu from top-right button"""
        menu = QMenu(self)
        
        # File operations
        file_menu = menu.addMenu("📁 File")
        
        new_action = QAction("🆕 New IDE", self)
        new_action.setShortcut("Ctrl+N")
        new_action.triggered.connect(self.new_ide_file)
        file_menu.addAction(new_action)
        
        load_action = QAction("Load IDE", self)
        load_action.setShortcut("Ctrl+O")
        load_action.triggered.connect(self.open_ide_file)
        file_menu.addAction(load_action)
        
        file_menu.addSeparator()
        
        save_action = QAction("💾 Save", self)
        save_action.setShortcut("Ctrl+S")
        save_action.triggered.connect(self.save_ide_file)
        file_menu.addAction(save_action)
        
        save_as_action = QAction("💾 Save As...", self)
        save_as_action.triggered.connect(self.save_ide_file_as)
        file_menu.addAction(save_as_action)
        
        # Edit operations
        edit_menu = menu.addMenu("✏️ Edit")
        
        copy_action = QAction("📋 Copy", self)
        copy_action.setShortcut("Ctrl+C")
        copy_action.triggered.connect(self.copy_selection)
        edit_menu.addAction(copy_action)
        
        paste_action = QAction("📌 Paste", self)
        paste_action.setShortcut("Ctrl+V")
        paste_action.triggered.connect(self.paste_selection)
        edit_menu.addAction(paste_action)
        
        edit_menu.addSeparator()
        
        add_action = QAction("➕ Add Entry", self)
        add_action.setShortcut("Insert")
        add_action.triggered.connect(self.add_ide_entry)
        edit_menu.addAction(add_action)
        
        delete_action = QAction("🗑️ Delete Entry", self)
        delete_action.setShortcut("Delete")
        delete_action.triggered.connect(self.delete_ide_entry)
        edit_menu.addAction(delete_action)
        
        # View operations
        view_menu = menu.addMenu("👁️ View")
        
        help_action = QAction("❓ Help Guide", self)
        help_action.setShortcut("F1")
        help_action.triggered.connect(self.show_help_guide)
        view_menu.addAction(help_action)
        
        view_menu.addSeparator()
        
        validate_action = QAction("✅ Validate IDE", self)
        validate_action.setShortcut("F5")
        validate_action.triggered.connect(self.validate_ide)
        view_menu.addAction(validate_action)
        
        # Sort operations
        sort_menu = menu.addMenu("🔄 Sort")
        
        sort_id_action = QAction("Sort by ID", self)
        sort_id_action.triggered.connect(self.sort_by_id)
        sort_menu.addAction(sort_id_action)
        
        sort_name_action = QAction("Sort by Name", self)
        sort_name_action.triggered.connect(self.sort_by_name)
        sort_menu.addAction(sort_name_action)
        
        sort_menu.addSeparator()
        
        sort_img_col_action = QAction("🔗 Sort IMG/COL by IDE", self)
        sort_img_col_action.triggered.connect(self.sort_col_by_ide)
        sort_menu.addAction(sort_img_col_action)
        
        # Show menu at button position
        button_pos = self.menu_button.mapToGlobal(self.menu_button.rect().bottomLeft())
        menu.exec(button_pos)
    
    def create_menu_bar(self): #vers 1
        """Create menu bar - REMOVED, using dropdown instead"""
        pass  # No longer needed, using dropdown menu
    
    def _create_file_menu(self): #vers 1
        """Create File menu"""
        file_menu = self.menu_bar.addMenu("&File")
        new_action = QAction("&New IDE", self)
        new_action.setShortcut("Ctrl+N")
        new_action.triggered.connect(self.new_ide_file)
        file_menu.addAction(new_action)
        
        load_action = QAction("&Load IDE", self)
        load_action.setShortcut("Ctrl+O")
        load_action.triggered.connect(self.open_ide_file)
        file_menu.addAction(load_action)
        
        file_menu.addSeparator()
        
        save_action = QAction("&Save", self)
        save_action.setShortcut("Ctrl+S")
        save_action.triggered.connect(self.save_ide_file)
        file_menu.addAction(save_action)
        
        save_as_action = QAction("Save &As...", self)
        save_as_action.setShortcut("Ctrl+Shift+S")
        save_as_action.triggered.connect(self.save_ide_file_as)
        file_menu.addAction(save_as_action)
        
        file_menu.addSeparator()
        
        export_action = QAction("&Export to Project", self)
        export_action.triggered.connect(self.export_to_project)
        file_menu.addAction(export_action)
        
        file_menu.addSeparator()
        
        close_action = QAction("&Close", self)
        close_action.setShortcut("Ctrl+W")
        close_action.triggered.connect(self.close)
        file_menu.addAction(close_action)
    
    def _create_edit_menu(self): #vers 1
        """Create Edit menu"""
        edit_menu = self.menu_bar.addMenu("&Edit")
        
        copy_action = QAction("&Copy", self)
        copy_action.setShortcut("Ctrl+C")
        copy_action.triggered.connect(self.copy_selection)
        edit_menu.addAction(copy_action)
        
        paste_action = QAction("&Paste", self)
        paste_action.setShortcut("Ctrl+V")
        paste_action.triggered.connect(self.paste_selection)
        edit_menu.addAction(paste_action)
        
        edit_menu.addSeparator()
        
        find_action = QAction("&Find", self)
        find_action.setShortcut("Ctrl+F")
        find_action.triggered.connect(self.find_entry)
        edit_menu.addAction(find_action)
        
        edit_menu.addSeparator()
        
        undo_action = QAction("&Undo", self)
        undo_action.setShortcut("Ctrl+Z")
        undo_action.triggered.connect(self.undo_action)
        edit_menu.addAction(undo_action)
        
        edit_menu.addSeparator()
        
        add_entry_action = QAction("&Add Entry", self)
        add_entry_action.setShortcut("Insert")
        add_entry_action.triggered.connect(self.add_ide_entry)
        edit_menu.addAction(add_entry_action)
        
        delete_entry_action = QAction("&Delete Entry", self)
        delete_entry_action.setShortcut("Delete")
        delete_entry_action.triggered.connect(self.delete_ide_entry)
        edit_menu.addAction(delete_entry_action)
    
    def _create_view_menu(self): #vers 1
        """Create View menu"""
        view_menu = self.menu_bar.addMenu("&View")
        
        # Help guide dropdown
        help_action = QAction("Show &Help Guide", self)
        help_action.setShortcut("F1")
        help_action.triggered.connect(self.show_help_guide)
        view_menu.addAction(help_action)
        
        view_menu.addSeparator()
        
        # Text size submenu
        text_size_menu = view_menu.addMenu("Text &Size")
        
        small_text_action = QAction("&Small", self)
        small_text_action.triggered.connect(lambda: self.set_text_size(8))
        text_size_menu.addAction(small_text_action)
        
        normal_text_action = QAction("&Normal", self)
        normal_text_action.triggered.connect(lambda: self.set_text_size(10))
        text_size_menu.addAction(normal_text_action)
        
        large_text_action = QAction("&Large", self)
        large_text_action.triggered.connect(lambda: self.set_text_size(12))
        text_size_menu.addAction(large_text_action)
        
        view_menu.addSeparator()
        
        validate_action = QAction("&Validate IDE", self)
        validate_action.setShortcut("F5")
        validate_action.triggered.connect(self.validate_ide)
        view_menu.addAction(validate_action)
    
    def _create_sort_menu(self): #vers 1
        """Create Sort menu"""
        sort_menu = self.menu_bar.addMenu("&Sort")
        
        sort_by_id_action = QAction("Sort by &ID", self)
        sort_by_id_action.triggered.connect(self.sort_by_id)
        sort_menu.addAction(sort_by_id_action)
        
        sort_by_name_action = QAction("Sort by &Name", self)
        sort_by_name_action.triggered.connect(self.sort_by_name)
        sort_menu.addAction(sort_by_name_action)
        
        sort_menu.addSeparator()
        
        sort_img_col_action = QAction("Sort IDE with &IMG/COL files", self)
        sort_img_col_action.triggered.connect(self.sort_col_by_ide)
        sort_menu.addAction(sort_img_col_action)
    
    def create_table_panel(self) -> QWidget: #vers 1
        """Create table panel with right-click support"""
        panel = QWidget()
        layout = QVBoxLayout(panel)
        layout.setContentsMargins(0, 0, 0, 0)
        
        # Create IDE table
        self.ide_table = IDETableWidget()
        layout.addWidget(self.ide_table)
        
        # Connect signals
        self.ide_table.ide_selection_changed.connect(self.on_ide_selection_changed)
        
        # Add keyboard shortcuts
        self.setup_keyboard_shortcuts()
        
        return panel
    
    def setup_keyboard_shortcuts(self): #vers 1
        """Setup keyboard shortcuts for IDE table"""
        # Copy shortcut
        copy_shortcut = QShortcut(QKeySequence("Ctrl+C"), self.ide_table)
        copy_shortcut.activated.connect(self.copy_selection)
        
        # Paste shortcut  
        paste_shortcut = QShortcut(QKeySequence("Ctrl+V"), self.ide_table)
        paste_shortcut.activated.connect(self.paste_selection)
        
        # Delete shortcut
        delete_shortcut = QShortcut(QKeySequence("Delete"), self.ide_table)
        delete_shortcut.activated.connect(self.delete_ide_entry)
        
        # Select all shortcut
        select_all_shortcut = QShortcut(QKeySequence("Ctrl+A"), self.ide_table)
        select_all_shortcut.activated.connect(self.ide_table.selectAll)
        
        # Add entry shortcut
        add_shortcut = QShortcut(QKeySequence("Insert"), self.ide_table)
        add_shortcut.activated.connect(self.add_ide_entry)
        
        # Help shortcut
        help_shortcut = QShortcut(QKeySequence("F1"), self)
        help_shortcut.activated.connect(self.show_help_guide)
        
        # Validate shortcut
        validate_shortcut = QShortcut(QKeySequence("F5"), self)
        validate_shortcut.activated.connect(self.validate_ide)
    
    def create_status_panel(self) -> QWidget: #vers 1
        """Create bottom status/log panel (5 lines height)"""
        panel = QWidget()
        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)
        
        # Status label
        status_label = QLabel("Status/Log Window:")
        status_label.setFont(QFont("Arial", 9, QFont.Weight.Bold))
        layout.addWidget(status_label)
        
        # Log text area (readonly, 5 lines max)
        self.log_text = QTextEdit()
        self.log_text.setReadOnly(True)
        self.log_text.setMaximumHeight(80)  # Approximately 5 lines
        self.log_text.setFont(QFont("Consolas", 9))
        self.log_text.append("IDE Editor initialized")
        layout.addWidget(self.log_text)
        
        return panel
    
    def show_help_guide(self): #vers 1
        """Show help guide in dropdown dialog"""
        help_dialog = QDialog(self)
        help_dialog.setWindowTitle("IDE Editor Help Guide")
        help_dialog.setGeometry(200, 200, 800, 600)
        
        layout = QVBoxLayout(help_dialog)
        
        # Create help widget
        help_widget = IDEHelpWidget()
        layout.addWidget(help_widget)
        
        # Close button
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(help_dialog.close)
        layout.addWidget(close_btn)
        
        help_dialog.exec()
    
    def set_text_size(self, size: int): #vers 1
        """Set table text size"""
        font = self.ide_table.font()
        font.setPointSize(size)
        self.ide_table.setFont(font)
        self.log_message(f"Text size set to {size}pt")
    
    def copy_selection(self): #vers 1
        """Copy selected entries to clipboard"""
        selected_rows = self.ide_table.selectionModel().selectedRows()
        if not selected_rows:
            self.log_message("No entries selected for copying")
            return
        
        # Build clipboard text
        clipboard_data = []
        for index in selected_rows:
            row = index.row()
            if row < len(self.ide_data):
                entry = self.ide_data[row]
                line = f"{entry['id']}, {entry['model']}, {entry['txd']}, {entry['meshcount']}, {entry['drawdist']}, {entry['flags']}"
                clipboard_data.append(line)
        
        clipboard_text = "\n".join(clipboard_data)
        QApplication.clipboard().setText(clipboard_text)
        self.log_message(f"Copied {len(clipboard_data)} entries to clipboard")
    
    def paste_selection(self): #vers 1
        """Paste entries from clipboard"""
        clipboard_text = QApplication.clipboard().text()
        if not clipboard_text:
            self.log_message("Clipboard is empty")
            return
        
        # Parse clipboard data
        lines = clipboard_text.strip().split('\n')
        pasted_count = 0
        
        for line in lines:
            entry = parse_ide_line(line)
            if entry:
                self.ide_data.append(entry)
                pasted_count += 1
        
        if pasted_count > 0:
            self.populate_ide_table()
            self.log_message(f"Pasted {pasted_count} entries from clipboard")
        else:
            self.log_message("No valid IDE entries found in clipboard")
    
    def find_entry(self): #vers 1
        """Find/search entries"""
        # Placeholder for find functionality
        self.log_message("Find function - not yet implemented")
    
    def undo_action(self): #vers 1
        """Undo last action"""
        # Placeholder for undo functionality
        self.log_message("Undo function - not yet implemented")
    
    def save_ide_file_as(self): #vers 1
        """Save IDE file with new name"""
        file_path, _ = QFileDialog.getSaveFileName(
            self, "Save IDE File As", "", "IDE Files (*.ide);;All Files (*)")
        if file_path:
            self.current_file_path = file_path
            self.save_ide_file()
    
    def export_to_project(self): #vers 1
        """Export IDE to project"""
        # Placeholder for project export
        self.log_message("Export to project - not yet implemented")
    
    def sort_by_id(self): #vers 1
        """Sort entries by ID"""
        self.ide_data.sort(key=lambda x: x['id'])
        self.populate_ide_table()
        self.log_message("Sorted by ID")
    
    def sort_by_name(self): #vers 1
        """Sort entries by model name"""
        self.ide_data.sort(key=lambda x: x['model'].lower())
        self.populate_ide_table()
        self.log_message("Sorted by model name")
    
    def log_message(self, message: str): #vers 1
        """Add message to log window"""
        if hasattr(self, 'log_text'):
            self.log_text.append(f"• {message}")
            # Auto-scroll to bottom
            scrollbar = self.log_text.verticalScrollBar()
            scrollbar.setValue(scrollbar.maximum())
    
    def new_ide_file(self): #vers 1
        """Create new IDE file"""
        self.ide_data.clear()
        self.ide_table.setRowCount(0)
        self.current_file_path = None
        self.status_bar.showMessage("New IDE file created")
        img_debugger.debug("New IDE file created")
    
    def open_ide_file(self): #vers 1
        """Open IDE file"""
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Open IDE File", "", "IDE Files (*.ide);;All Files (*)")
        
        if file_path:
            self.load_ide_file(file_path)
    
    def load_ide_file(self, file_path: str) -> bool: #vers 1
        """Load IDE file from path"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                lines = f.readlines()
            
            self.ide_data.clear()
            self.ide_table.setRowCount(0)
            
            for line_num, line in enumerate(lines):
                line = line.strip()
                if line and not line.startswith('#') and not line.startswith('end'):
                    entry = parse_ide_line(line)
                    if entry:
                        self.ide_data.append(entry)
            
            self.populate_ide_table()
            self.current_file_path = file_path
            self.status_bar.showMessage(f"Loaded: {os.path.basename(file_path)} ({len(self.ide_data)} entries)")
            img_debugger.success(f"IDE file loaded: {len(self.ide_data)} entries")
            return True
            
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to load IDE file:\n{str(e)}")
            img_debugger.error(f"Error loading IDE file: {str(e)}")
            return False
    
    def save_ide_file(self): #vers 1
        """Save IDE file"""
        if not self.current_file_path:
            file_path, _ = QFileDialog.getSaveFileName(
                self, "Save IDE File", "", "IDE Files (*.ide);;All Files (*)")
            if not file_path:
                return
            self.current_file_path = file_path
        
        try:
            with open(self.current_file_path, 'w', encoding='utf-8') as f:
                f.write("# Generated by IMG Factory 1.5 IDE Editor\n")
                f.write("objs\n")
                
                for entry in self.ide_data:
                    line = f"{entry['id']}, {entry['model']}, {entry['txd']}, {entry['meshcount']}, {entry['drawdist']}, {entry['flags']}\n"
                    f.write(line)
                
                f.write("end\n")
            
            self.status_bar.showMessage(f"Saved: {os.path.basename(self.current_file_path)}")
            img_debugger.success("IDE file saved successfully")
            
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to save IDE file:\n{str(e)}")
            img_debugger.error(f"Error saving IDE file: {str(e)}")
    
    def populate_ide_table(self): #vers 1
        """Populate table with IDE data"""
        self.ide_table.setRowCount(len(self.ide_data))
        
        for row, entry in enumerate(self.ide_data):
            self.ide_table.setItem(row, 0, QTableWidgetItem(str(entry['id'])))
            self.ide_table.setItem(row, 1, QTableWidgetItem(entry['model']))
            self.ide_table.setItem(row, 2, QTableWidgetItem(entry['txd']))
            self.ide_table.setItem(row, 3, QTableWidgetItem(str(entry['meshcount'])))
            self.ide_table.setItem(row, 4, QTableWidgetItem(str(entry['drawdist'])))
            self.ide_table.setItem(row, 5, QTableWidgetItem(entry['flags']))
    
    def add_ide_entry(self): #vers 1
        """Add new IDE entry"""
        # Create new entry with default values
        new_entry = {
            'id': self.get_next_id(),
            'model': 'newmodel',
            'txd': 'newtxd',
            'meshcount': 1,
            'drawdist': 100.0,
            'flags': '0x0'
        }
        
        self.ide_data.append(new_entry)
        self.populate_ide_table()
        
        # Select the new entry
        new_row = len(self.ide_data) - 1
        self.ide_table.selectRow(new_row)
    
    def delete_ide_entry(self): #vers 1
        """Delete selected IDE entry"""
        current_row = self.ide_table.currentRow()
        if current_row >= 0:
            del self.ide_data[current_row]
            self.populate_ide_table()
    
    def get_next_id(self) -> int: #vers 1
        """Get next available ID"""
        if not self.ide_data:
            return 1
        
        max_id = max(entry['id'] for entry in self.ide_data)
        return max_id + 1
    
    def validate_ide(self): #vers 1
        """Validate IDE entries"""
        errors = []
        warnings = []
        
        ids_used = set()
        for i, entry in enumerate(self.ide_data):
            # Check for duplicate IDs
            if entry['id'] in ids_used:
                errors.append(f"Row {i+1}: Duplicate ID {entry['id']}")
            ids_used.add(entry['id'])
            
            # Check model name length
            if len(entry['model']) > 24:
                warnings.append(f"Row {i+1}: Model name too long ({len(entry['model'])} chars)")
            
            # Check draw distance
            if entry['drawdist'] < 10:
                warnings.append(f"Row {i+1}: Very low draw distance ({entry['drawdist']})")
        
        # Show validation results
        if errors or warnings:
            msg = "Validation Results:\n\n"
            if errors:
                msg += "ERRORS:\n" + "\n".join(errors) + "\n\n"
            if warnings:
                msg += "WARNINGS:\n" + "\n".join(warnings)
            QMessageBox.warning(self, "Validation", msg)
        else:
            QMessageBox.information(self, "Validation", "IDE file is valid!")
    
    def sort_col_by_ide(self): #vers 1
        """Sort COL editor by IDE order"""
        model_order = [entry['model'] for entry in self.ide_data]
        self.sort_by_ide_requested.emit(model_order)
        self.status_bar.showMessage("Sort by IDE order requested")
    
    def sync_selection_with_col(self): #vers 1
        """Sync selection with COL editor"""
        selected_rows = self.ide_table.selectionModel().selectedRows()
        selected_models = []
        
        for index in selected_rows:
            row = index.row()
            if row < len(self.ide_data):
                selected_models.append(self.ide_data[row]['model'])
        
        self.selection_sync_requested.emit(selected_models)
        self.status_bar.showMessage(f"Selection sync: {len(selected_models)} models")
    
    def on_ide_selection_changed(self, selected_models: List[str]): #vers 1
        """Handle IDE selection changes"""
        self.status_bar.showMessage(f"Selected: {len(selected_models)} models")
        # Auto-sync with COL editor if enabled
        self.selection_sync_requested.emit(selected_models)

# Utility functions

def parse_ide_line(line: str) -> Optional[Dict[str, Any]]: #vers 1
    """Parse single IDE line"""
    try:
        # Remove comments and extra whitespace
        line = line.split('#')[0].strip()
        if not line:
            return None
        
        # Split by comma and clean up
        parts = [part.strip() for part in line.split(',')]
        if len(parts) < 6:
            return None
        
        return {
            'id': int(parts[0]),
            'model': parts[1],
            'txd': parts[2],
            'meshcount': int(parts[3]),
            'drawdist': float(parts[4]),
            'flags': parts[5]
        }
        
    except (ValueError, IndexError) as e:
        img_debugger.warning(f"Error parsing IDE line: {line} - {e}")
        return None

def validate_ide_entry(entry: Dict[str, Any]) -> List[str]: #vers 1
    """Validate single IDE entry"""
    errors = []
    
    if entry['id'] < 0 or entry['id'] > 65535:
        errors.append("ID must be between 0 and 65535")
    
    if len(entry['model']) > 24:
        errors.append("Model name must be 24 characters or less")
    
    if len(entry['txd']) > 24:
        errors.append("TXD name must be 24 characters or less")
    
    if entry['meshcount'] < 1:
        errors.append("Mesh count must be at least 1")
    
    if entry['drawdist'] < 0:
        errors.append("Draw distance cannot be negative")
    
    return errors

def create_ide_help_guide() -> str: #vers 1
    """Create IDE help guide content"""
    return """
    IDE Editor Help Guide
    
    This editor allows you to modify Item Definition (IDE) files that control
    object placement and properties in GTA games.
    
    Use the built-in help tab for complete documentation of all fields and functions.
    """

# Export main class
def open_ide_editor(parent=None) -> IDEEditor: #vers 1
    """Open IDE Editor dialog"""
    editor = IDEEditor(parent)
    editor.show()
    return editor

__all__ = [
    'IDEEditor',
    'IDEHelpWidget', 
    'IDETableWidget',
    'open_ide_editor',
    'parse_ide_line',
    'validate_ide_entry',
    'create_ide_help_guide'
]