#this belongs in gui/ imgcol_tearoff.py - Version: 2
# X-Seti - September26 2025 - Img Factory 1.5

"""
IMG, COL Tear-Off Window System - Reusable for all editors
Creates detachable windows for IMG, COL and other files with essential functions
"""

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
    QPushButton, QLabel, QSplitter, QTextEdit, QDialog, QFrame,
    QGroupBox, QMessageBox, QHeaderView, QAbstractItemView, QMenu
)
from PyQt6.QtCore import Qt, pyqtSignal, QSize
from PyQt6.QtGui import QIcon, QFont, QCursor, QAction, QTextCursor
from apps.gui.tear_off import TearOffPanel

##Methods list -
# create_img_tearoff_window
# create_col_tearoff_window
# create_editor_tearoff_window
# integrate_tearoff_with_col_editor
# integrate_tearoff_with_ide_editor
# integrate_tearoff_with_txd_editor
# _setup_table_widget
# _setup_button_panel
# _setup_log_panel

##class IMGCOLTearOffWindow: -
# __init__
# setup_ui
# setup_img_table
# setup_col_table
# _create_button_panel
# _create_log_area
# _setup_connections
# _import_files
# _export_selected
# _remove_selected
# _rebuild_archive
# _refresh_table
# _close_window
# _edit_collision
# _analyze_collision
# update_table_data
# log_message
# get_selected_entries
# _apply_theme
# closeEvent

def create_img_tearoff_window(main_window, title="IMG Archive"): #vers 1
    """Create tear-off window for IMG files"""
    window = IMGCOLTearOffWindow(main_window, "img", title)
    window.setup_img_table()
    return window

def create_col_tearoff_window(main_window, title="COL Collision"): #vers 1
    """Create tear-off window for COL files"""
    window = IMGCOLTearOffWindow(main_window, "col", title)
    window.setup_col_table()
    return window

def create_editor_tearoff_window(main_window, editor_type, title): #vers 1
    """Create tear-off window for specific editors (TXD, DFF, IDE, etc.)"""
    window = IMGCOLTearOffWindow(main_window, editor_type, title)
    # Setup appropriate table based on editor type
    if editor_type in ["txd", "dff", "ifp"]:
        window.setup_img_table()
    elif editor_type in ["col"]:
        window.setup_col_table()
    else:
        window.setup_img_table()  # Default to IMG table
    return window

def _setup_table_widget(table): #vers 1
    """Configure table widget with standard settings"""
    table.setAlternatingRowColors(True)
    table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
    table.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
    table.setSortingEnabled(True)
    table.verticalHeader().setVisible(False)
    table.horizontalHeader().setStretchLastSection(True)

def _setup_button_panel(parent, window_type): #vers 1
    """Create button panel based on window type"""
    button_frame = QFrame()
    button_layout = QHBoxLayout(button_frame)
    button_layout.setContentsMargins(5, 5, 5, 5)

    buttons = []

    if window_type == "img":
        button_configs = [
            ("Import", "import_files", "Import files into archive"),
            ("📤 Export", "export_selected", "Export selected entries"),
            ("🗑️ Remove", "remove_selected", "Remove selected entries"),
            ("🔄 Rebuild", "rebuild_archive", "Rebuild archive")
        ]
    elif window_type == "col":
        button_configs = [
            ("Import", "import_files", "Import COL files"),
            ("📤 Export", "export_selected", "Export selected COL data"),
            ("✏️ Edit", "edit_collision", "Edit collision data"),
            ("🔍 Analyze", "analyze_collision", "Analyze collision")
        ]
    else:
        button_configs = [
            ("Import", "import_files", "Import files"),
            ("📤 Export", "export_selected", "Export selected"),
            ("🗑️ Remove", "remove_selected", "Remove selected"),
            ("🔄 Refresh", "refresh_table", "Refresh table")
        ]

    for text, action, tooltip in button_configs:
        btn = QPushButton(text)
        btn.setToolTip(tooltip)
        btn.setMinimumHeight(32)
        btn.setObjectName(action)
        buttons.append(btn)
        button_layout.addWidget(btn)

    return button_frame, buttons

def _setup_log_panel(parent): #vers 1
    """Create log panel for tear-off window"""
    log_frame = QFrame()
    log_layout = QVBoxLayout(log_frame)
    log_layout.setContentsMargins(5, 5, 5, 5)

    log_text = QTextEdit()
    log_text.setReadOnly(True)
    log_text.setMaximumHeight(100)
    log_layout.addWidget(log_text)

    status_label = QLabel("Ready")
    log_layout.addWidget(status_label)

    return log_frame, log_text, status_label


# Integration functions for existing editors
def integrate_tearoff_with_col_editor(main_window): #vers 1
    """Add tear-off button to COL editor"""
    if hasattr(main_window, 'col_editor'):
        # Add tear-off functionality to existing COL editor
        pass

def integrate_tearoff_with_ide_editor(main_window): #vers 1
    """Add tear-off button to IDE editor"""
    if hasattr(main_window, 'ide_editor'):
        # Add tear-off functionality to existing IDE editor
        pass

def integrate_tearoff_with_txd_editor(main_window): #vers 2
    """Add tear-off button to TXD Workshop"""
    try:
        # Check if TXD Workshop is open
        if hasattr(main_window, 'txd_workshop') and main_window.txd_workshop:
            # Add tearoff button to TXD Workshop toolbar
            tearoff_btn = QPushButton("↗️ Tearoff")
            tearoff_btn.setToolTip("Detach TXD Workshop window")
            tearoff_btn.clicked.connect(lambda: _tearoff_txd_workshop(main_window))

            # Add to workshop toolbar if it has one
            if hasattr(main_window.txd_workshop, 'toolbar'):
                toolbar_layout = main_window.txd_workshop.toolbar.layout()
                if toolbar_layout:
                    toolbar_layout.addWidget(tearoff_btn)

            if hasattr(main_window, 'log_message'):
                main_window.log_message("✅ TXD Workshop tearoff button added")
            return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ TXD Workshop tearoff error: {str(e)}")

    return False

def _tearoff_txd_workshop(main_window): #vers 1
    """Tearoff TXD Workshop to separate window"""
    try:
        if hasattr(main_window, 'txd_workshop') and main_window.txd_workshop:
            # TXD Workshop is already a separate window, just raise it
            main_window.txd_workshop.raise_()
            main_window.txd_workshop.activateWindow()

            if hasattr(main_window, 'log_message'):
                main_window.log_message("📱 TXD Workshop is already a separate window")
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ TXD Workshop tearoff error: {str(e)}")


class IMGCOLTearOffWindow(QWidget): #vers 1
    """Tear-off window for IMG/COL archives"""

    # ... [ALL EXISTING CLASS METHODS REMAIN EXACTLY THE SAME] ...

    def __init__(self, parent_window, window_type, title="Archive Viewer"): #vers 1
        super().__init__()
        self.parent_window = parent_window
        self.window_type = window_type
        self.current_entries = []

        self.setWindowTitle(title)
        self.resize(800, 600)
        self.setup_ui()

    def setup_ui(self): #vers 1
        """Setup the tear-off window UI"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(5, 5, 5, 5)

        # Info bar
        self.info_label = QLabel(f"{self.window_type.upper()}: No file")
        self.info_label.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        layout.addWidget(self.info_label)

        # Main table
        self.table = QTableWidget()
        _setup_table_widget(self.table)
        layout.addWidget(self.table)

        # Button panel
        button_frame, self.buttons = _setup_button_panel(self, self.window_type)
        layout.addWidget(button_frame)

        # Log panel
        log_frame, self.log_text, self.status_label = _setup_log_panel(self)
        layout.addWidget(log_frame)

        # Setup connections
        self._setup_connections()

        # Apply theme
        self._apply_theme()

    def setup_img_table(self): #vers 1
        """Setup table for IMG entries"""
        self.table.setColumnCount(6)
        self.table.setHorizontalHeaderLabels(["Name", "Type", "Size", "Offset", "RW Version", "Status"])

    def setup_col_table(self): #vers 1
        """Setup table for COL entries"""
        self.table.setColumnCount(6)
        self.table.setHorizontalHeaderLabels(["Model", "Spheres", "Boxes", "Faces", "Vertices", "Status"])

    def _create_button_panel(self): #vers 1
        """Create button panel (legacy method - kept for compatibility)"""
        button_frame, buttons = _setup_button_panel(self, self.window_type)
        return button_frame

    def _create_log_area(self): #vers 1
        """Create log area (legacy method - kept for compatibility)"""
        log_frame, log_text, status_label = _setup_log_panel(self)
        return log_frame

    def _setup_connections(self): #vers 1
        """Connect button signals"""
        for btn in self.buttons:
            action = btn.objectName()
            if action == "import_files":
                btn.clicked.connect(self._import_files)
            elif action == "export_selected":
                btn.clicked.connect(self._export_selected)
            elif action == "remove_selected":
                btn.clicked.connect(self._remove_selected)
            elif action == "rebuild_archive":
                btn.clicked.connect(self._rebuild_archive)
            elif action == "refresh_table":
                btn.clicked.connect(self._refresh_table)
            elif action == "edit_collision":
                btn.clicked.connect(self._edit_collision)
            elif action == "analyze_collision":
                btn.clicked.connect(self._analyze_collision)

    def _import_files(self): #vers 1
        """Import files"""
        self.log_message("Import files")
        if hasattr(self.parent_window, 'import_files'):
            self.parent_window.import_files()
        else:
            self.log_message("❌ Import function not available in parent")

    def _export_selected(self): #vers 1
        """Export selected entries"""
        selected = self.get_selected_entries()
        if not selected:
            self.log_message("❌ No entries selected")
            return

        self.log_message(f"📤 Exporting {len(selected)} entries")
        if hasattr(self.parent_window, 'export_selected'):
            self.parent_window.export_selected()
        else:
            self.log_message("❌ Export function not available in parent")

    def _remove_selected(self): #vers 1
        """Remove selected entries"""
        selected = self.get_selected_entries()
        if not selected:
            self.log_message("❌ No entries selected")
            return

        reply = QMessageBox.question(
            self,
            "Remove Entries",
            f"Remove {len(selected)} selected entries?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            self.log_message(f"🗑️ Removing {len(selected)} entries")
            if hasattr(self.parent_window, 'remove_selected'):
                self.parent_window.remove_selected()
            else:
                self.log_message("❌ Remove function not available in parent")

    def _rebuild_archive(self): #vers 1
        """Rebuild archive"""
        self.log_message("🔄 Rebuilding archive")
        if hasattr(self.parent_window, 'rebuild_img'):
            self.parent_window.rebuild_img()
        else:
            self.log_message("❌ Rebuild function not available in parent")

    def _refresh_table(self): #vers 1
        """Refresh table data"""
        self.log_message("🔄 Refreshing table")
        if hasattr(self.parent_window, 'refresh_table'):
            self.parent_window.refresh_table()
        elif hasattr(self.parent_window, 'reload_current_file'):
            self.parent_window.reload_current_file()
        else:
            self.log_message("❌ Refresh function not available in parent")

    def _close_window(self): #vers 1
        """Close tear-off window"""
        self.log_message("❌ Closing tear-off window")
        self.close()

    def _edit_collision(self): #vers 1
        """Edit collision data (COL specific)"""
        if self.window_type != "col":
            return

        self.log_message("✏️ Opening COL editor")
        try:
            from gui.gui_context import open_col_editor_dialog
            open_col_editor_dialog(self.parent_window)
        except Exception as e:
            self.log_message(f"❌ COL editor error: {str(e)}")

    def _analyze_collision(self): #vers 1
        """Analyze collision data (COL specific)"""
        if self.window_type != "col":
            return

        self.log_message("🔍 Analyzing collision")
        # Add collision analysis logic here

    def update_table_data(self, entries_data): #vers 1
        """Update table with new data"""
        self.table.setRowCount(0)
        self.current_entries = entries_data

        for entry in entries_data:
            row = self.table.rowCount()
            self.table.insertRow(row)

            for col, key in enumerate(entry.keys()):
                item = QTableWidgetItem(str(entry[key]))
                self.table.setItem(row, col, item)

    def log_message(self, message): #vers 1
        """Log message to log panel"""
        self.log_text.append(message)
        self.log_text.moveCursor(self.log_text.textCursor().End)
        self.status_label.setText(message.replace('📋 ', '').replace('❌ ', '').replace('✅ ', ''))

    def get_selected_entries(self): #vers 1
        """Get list of selected entries"""
        selected_rows = []
        for item in self.table.selectedItems():
            row = item.row()
            if row not in selected_rows:
                selected_rows.append(row)

        return [self.current_entries[row] for row in selected_rows if row < len(self.current_entries)]

    def _apply_theme(self): #vers 1
        """Apply theme from parent window"""
        try:
            if hasattr(self.parent_window, 'app_settings'):
                theme_name = self.parent_window.app_settings.current_settings.get("theme", "default")
                # Apply theme styling here if needed
                self.log_message(f"🎨 Applied theme: {theme_name}")
        except Exception as e:
            self.log_message(f"❌ Theme error: {str(e)}")

    def closeEvent(self, event): #vers 1
        """Handle window close event"""
        self.log_message("👋 Tear-off window closing")
        event.accept()
