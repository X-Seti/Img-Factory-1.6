#this belongs in methods/populate_img_table.py - Version: 10
# X-Seti - November18 2025 - IMG Factory 1.5
"""
IMG Table Population
"""
import os
from typing import Any, List, Optional
from PyQt6.QtWidgets import QTableWidgetItem, QTableWidget, QHeaderView, QAbstractItemView
from PyQt6.QtCore import Qt, QItemSelectionModel
from PyQt6.QtGui import QMouseEvent
from apps.debug.debug_functions import img_debugger


#                                                                              
# DragSelectTableWidget
#                                                                              

class DragSelectTableWidget(QTableWidget): #vers 1
    """QTableWidget subclass that supports click-and-drag row selection.

    Holding the left mouse button and moving up or down continuously extends
    (or shrinks) the row selection — identical to how file managers behave.
    Works with Shift+Click and Ctrl+Click as usual.
    """

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._drag_selecting = False
        self._drag_anchor_row = -1
        # Sensible defaults — callers can override after construction
        self.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        self.setDragEnabled(False)
        self.setDragDropMode(QAbstractItemView.DragDropMode.NoDragDrop)
        self.setMouseTracking(False)  # only track while button held

    def mousePressEvent(self, event: QMouseEvent): #vers 1
        if event.button() == Qt.MouseButton.LeftButton:
            item = self.itemAt(event.pos())
            if item is not None:
                self._drag_anchor_row = item.row()
                self._drag_selecting = True
                self.setMouseTracking(True)
        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent): #vers 1
        if not self._drag_selecting:
            super().mouseMoveEvent(event)
            return
        if not (event.buttons() & Qt.MouseButton.LeftButton):
            self._drag_selecting = False
            self.setMouseTracking(False)
            return

        item = self.itemAt(event.pos())
        if item is None:
            # Clamp to first/last row when cursor leaves the table
            y = event.pos().y()
            current_row = 0 if y < 0 else self.rowCount() - 1
        else:
            current_row = item.row()

        anchor = self._drag_anchor_row
        if anchor < 0 or current_row < 0:
            return

        top    = min(anchor, current_row)
        bottom = max(anchor, current_row)
        cols   = self.columnCount()

        sm = self.selectionModel()
        # Clear only if no modifier held
        mods = event.modifiers()
        if not (mods & Qt.KeyboardModifier.ControlModifier):
            sm.clearSelection()

        for row in range(top, bottom + 1):
            for col in range(cols):
                idx = self.model().index(row, col)
                sm.select(idx, QItemSelectionModel.SelectionFlag.Select)

    def mouseReleaseEvent(self, event: QMouseEvent): #vers 1
        if event.button() == Qt.MouseButton.LeftButton:
            self._drag_selecting = False
            self.setMouseTracking(False)
        super().mouseReleaseEvent(event)

##Methods list - imported from tables_structure
# reset_table_styling
# setup_table_for_img_data
# setup_table_structure
##Methods list -
# create_img_table_item
# format_img_entry_size
# get_img_entry_type
# populate_table_row_minimal
# populate_table_with_img_data_minimal
# refresh_img_table
# update_img_table_selection_info

def reset_table_styling(main_window): #vers 2
    """Completely reset table styling to default using IMG debug system"""
    try:
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            img_debugger.warning("No table widget available for styling reset")
            return False
        table = main_window.gui_layout.table
        header = table.horizontalHeader()
        table.setObjectName("")
        table.setAlternatingRowColors(True)
        table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        table.setSelectionMode(QTableWidget.SelectionMode.ExtendedSelection)
        table.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        table.setStyleSheet("""
            QTableWidget { outline: 0; }
            QTableWidget::item { outline: 0; border: none; }
            QTableWidget::item:hover {
                background-color: rgba(100, 150, 255, 0.15);
            }
            QTableWidget::item:selected {
                background-color: rgba(70, 130, 230, 0.7);
            }
            QTableWidget::item:selected:hover {
                background-color: rgba(90, 150, 250, 0.8);
            }
        """)
        header.setStretchLastSection(True)
        header.setSectionsClickable(True)
        header.setSortIndicatorShown(True)
        header.setSectionsMovable(False)
        main_window.log_message("Table styling reset")
        img_debugger.debug("Table styling reset to default")
        return True
    except Exception as e:
        error_msg = f"Error resetting table styling: {str(e)}"
        main_window.log_message(f"Error: {error_msg}")
        img_debugger.error(error_msg)
        return False

def setup_table_for_img_data(table: QTableWidget) -> bool: #vers 4
    """Setup table structure for IMG file data.
    Column 8 (COL) is always present but hidden by default;
    hybrid load reveals it via table.setColumnHidden(8, False).
    """
    try:
        img_headers = ["Name", "Type", "Size", "Offset", "RW Address", "RW Version",
                       "Encoding", "Status", "COL", "IDE Model", "IDE TXD"]
        table.setColumnCount(11)
        table.setHorizontalHeaderLabels(img_headers)
        table.setColumnWidth(0, 190)  # Name
        table.setColumnWidth(1, 60)   # Type
        table.setColumnWidth(2, 90)   # Size
        table.setColumnWidth(3, 100)  # Offset
        table.setColumnWidth(4, 100)  # RW Address
        table.setColumnWidth(5, 100)  # RW Version
        table.setColumnWidth(6, 110)  # Encoding
        table.setColumnWidth(7, 110)  # Status
        table.setColumnWidth(8, 160)  # COL
        table.setColumnWidth(9, 160)  # IDE Model
        table.setColumnWidth(10, 120) # IDE TXD
        table.setColumnHidden(8, True)   # hidden until hybrid load
        table.setColumnHidden(9, True)   # hidden until xref loads
        table.setColumnHidden(10, True)  # hidden until xref loads
        header = table.horizontalHeader()
        header.setSectionsMovable(True)
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        for col in range(1, 11):
            header.setSectionResizeMode(col, QHeaderView.ResizeMode.Interactive)
        table.setSortingEnabled(True)
        img_debugger.debug("Table structure setup for IMG data")
        return True
    except Exception as e:
        img_debugger.error(f"Error setting up IMG table structure: {e}")
        return False

class IMGTablePopulator:
    """Handles IMG table population with minimal processing to prevent freezing"""
    def __init__(self, main_window):
        self.main_window = main_window


    def _get_ui_color(self, key): #vers 1
        """Return theme-aware QColor. No hardcoded colors - everything via app_settings."""
        from PyQt6.QtGui import QColor
        try:
            app_settings = getattr(self, 'app_settings', None) or \
                getattr(getattr(self, 'main_window', None), 'app_settings', None)
            if app_settings and hasattr(app_settings, 'get_ui_color'):
                return app_settings.get_ui_color(key)
        except Exception:
            pass
        pal = self.palette()
        if key == 'viewport_bg':
            return pal.color(pal.ColorRole.Base)
        if key == 'viewport_text':
            return pal.color(pal.ColorRole.PlaceholderText)
        if key == 'border':
            return pal.color(pal.ColorRole.Mid)
        return pal.color(pal.ColorRole.WindowText)

    def populate_table_with_img_data(self, img_file: Any) -> bool: #vers 9
        """Populate table with IMG entry data - MINIMAL VERSION to prevent freezing"""
        try:
            if not img_file or not hasattr(img_file, 'entries'):
                img_debugger.error("Invalid IMG file for table population")
                return False
            table = self.get_table_reference()
            if not table:
                img_debugger.error("No table found for IMG population")
                return False
            table.setColumnCount(11)
            table.setHorizontalHeaderLabels([
                "Name", "Type", "Size", "Offset", "RW Address", "RW Version",
                "Encoding", "Status", "COL", "IDE Model", "IDE TXD"
            ])
            table.setColumnWidth(0, 190)
            table.setColumnWidth(1, 60)
            table.setColumnWidth(2, 90)
            table.setColumnWidth(3, 100)
            table.setColumnWidth(4, 100)
            table.setColumnWidth(5, 100)
            table.setColumnWidth(6, 110)
            table.setColumnWidth(7, 110)
            table.setColumnWidth(8, 160)
            table.setColumnHidden(8, True)   # hidden until hybrid load
            header = table.horizontalHeader()
            header.setSectionsMovable(True)
            header.setStretchLastSection(True)
            for col in range(10):
                header.setSectionResizeMode(col, QHeaderView.ResizeMode.Interactive)
            entries = img_file.entries
            if not entries:
                img_debugger.info("No entries found in IMG file")
                return True
            table.setRowCount(len(entries))
            img_debugger.debug(f"Populating table with {len(entries)} entries")
            for row, entry in enumerate(entries):
                self.populate_table_row_minimal(table, row, entry)
            img_debugger.info(f"Table populated with {len(entries)} entries")

            # Apply DAT cross-reference tooltips — pick xref matching this IMG's game root
            try:
                mw = self.main_window
                xref = None
                # Try per-root lookup first, then dat_browser.xref, then main_window.xref
                xref_by_root = getattr(mw, 'xref_by_root', {})
                if xref_by_root and self.img_file:
                    import os as _os
                    img_path = getattr(self.img_file, 'file_path', '') or getattr(self.img_file, 'path', '')
                    if img_path:
                        img_abs = _os.path.abspath(img_path)
                        best_root, best_len = None, 0
                        for game_root in xref_by_root:
                            gr = _os.path.normcase(_os.path.abspath(game_root))
                            ip = _os.path.normcase(img_abs)
                            try:
                                common = _os.path.commonpath([gr, ip])
                                if _os.path.normcase(common) == gr and len(gr) > best_len:
                                    best_root, best_len = game_root, len(gr)
                            except ValueError:
                                pass
                        if best_root:
                            xref = xref_by_root[best_root]
                if not xref:
                    dat_browser = getattr(mw, "dat_browser", None)
                    xref = getattr(dat_browser, "xref", None) if dat_browser else None
                if not xref:
                    xref = getattr(mw, 'xref', None)
                if xref:
                    from apps.methods.populate_img_table import apply_xref_tooltips
                    hits = apply_xref_tooltips(table, xref)
                    if hits and hasattr(mw, "log_message"):
                        mw.log_message(f"XRef tooltips: {hits} entries cross-referenced")
            except Exception as e:
                img_debugger.error(f"XRef tooltip error: {e}")

            return True
        except Exception as e:
            img_debugger.error(f"Error populating IMG table: {str(e)}")
            return False

    def populate_table_row_minimal(self, table: Any, row: int, entry: Any): #vers 4
        """Populate single table row with MINIMAL processing - keep all 8 columns"""
        try:
            # Check if this entry should be highlighted - OPTIMIZED
            is_highlighted = False
            highlight_type = None
            if hasattr(entry, 'is_new_entry') and entry.is_new_entry:
                is_highlighted = True
                if hasattr(entry, 'is_replaced') and entry.is_replaced:
                    highlight_type = "replaced"
                else:
                    highlight_type = "imported"
            
            # Create items with minimal processing
            name_text = str(entry.name) if hasattr(entry, 'name') else f"Entry_{row}"
            name_item = self.create_img_table_item(name_text, is_highlighted, highlight_type)
            table.setItem(row, 0, name_item)
            
            entry_type = self.get_img_entry_type_simple(entry)
            type_item = self.create_img_table_item(entry_type, is_highlighted, highlight_type)
            table.setItem(row, 1, type_item)
            
            size_text = self.format_img_entry_size_simple(entry)
            size_item = self.create_img_table_item(size_text, is_highlighted, highlight_type)
            table.setItem(row, 2, size_item)
            
            offset_text = f"0x{entry.offset:08X}" if hasattr(entry, 'offset') else "N/A"
            offset_item = self.create_img_table_item(offset_text, is_highlighted, highlight_type)
            table.setItem(row, 3, offset_item)
            
            rw_address_text = self.get_rw_address_light(entry)
            rw_address_item = self.create_img_table_item(rw_address_text, is_highlighted, highlight_type)
            table.setItem(row, 4, rw_address_item)
            
            version_text = self.get_rw_version_light(entry)
            version_item = self.create_img_table_item(version_text, is_highlighted, highlight_type)
            table.setItem(row, 5, version_item)
            
            info_text = self.get_compression_info(entry)
            info_item = self.create_img_table_item(info_text, is_highlighted, highlight_type)
            table.setItem(row, 6, info_item)
            
            status_text = self.get_info_light(entry)
            status_item = self.create_img_table_item(status_text, is_highlighted, highlight_type)
            table.setItem(row, 7, status_item)
        except Exception as e:
            img_debugger.error(f"Error populating table row {row}: {str(e)}")
            table.setItem(row, 0, self.create_img_table_item(f"Error_{row}"))

    def get_img_entry_type_simple(self, entry: Any) -> str: #vers 2
        """Get entry type - SIMPLE extension extraction, no heavy processing"""
        try:
            if hasattr(entry, 'name') and '.' in entry.name:
                return entry.name.split('.')[-1].upper()
            else:
                return "UNKNOWN"
        except Exception:
            return "UNKNOWN"

    def format_img_entry_size_simple(self, entry: Any) -> str: #vers 2
        """Format entry size - SIMPLE formatting, no processing"""
        try:
            if hasattr(entry, 'size') and entry.size > 0:
                size = entry.size
            elif hasattr(entry, 'actual_size') and entry.actual_size > 0:
                size = entry.actual_size
            else:
                return "0 B"
            if size < 1024:
                return f"{size} B"
            elif size < 1024 * 1024:
                return f"{size/1024:.1f} KB"
            elif size < 1024 * 1024 * 1024:
                return f"{size/(1024*1024):.1f} MB"
            else:
                return f"{size/(1024*1024*1024):.1f} GB"
        except Exception:
            return "0 B"

    def get_rw_address_light(self, entry: Any) -> str: #vers 4
        """Get RW address — shows the raw RW version value as a hex address."""
        try:
            from apps.methods.rw_versions import is_valid_rw_version
            entry_type = self.get_img_entry_type_simple(entry)
            if entry_type in ['DFF', 'TXD'] and hasattr(entry, 'size') and entry.size == 0:
                return "Empty"
            v = getattr(entry, 'rw_version', 0)
            if v and is_valid_rw_version(v):
                return f"0x{v:08X}"
            if entry_type in ['DFF', 'TXD']:
                return "N/A"
            return "N/A"
        except Exception:
            return "N/A"

    def get_rw_version_light(self, entry: Any) -> str: #vers 6
        """Get RW version - validates version is in known RW range before accepting"""
        try:
            from apps.methods.rw_versions import get_rw_version_name, is_valid_rw_version, parse_rw_version
            # 0-byte entries have no data to parse - show as Empty, not Unknown
            entry_type = self.get_img_entry_type_simple(entry)
            if entry_type in ['DFF', 'TXD']:
                if hasattr(entry, 'size') and entry.size == 0:
                    return "Empty"
            # Use validated cached rw_version (only trust it if it's a known-valid version)
            if hasattr(entry, 'rw_version') and entry.rw_version and is_valid_rw_version(entry.rw_version):
                if hasattr(entry, 'rw_version_name') and entry.rw_version_name not in ["Unknown", "", "N/A", "Error"]:
                    return entry.rw_version_name
                return get_rw_version_name(entry.rw_version)
            # Try to detect from cached data or read first 16 bytes from IMG
            if entry_type in ['DFF', 'TXD']:
                data = getattr(entry, '_cached_data', None)
                if not data and hasattr(entry, 'offset') and entry.size >= 12:
                    try:
                        import os as _os
                        img_file = getattr(entry, '_img_file', None)
                        # Resolve the actual data file (handles .dir -> .img)
                        fp = getattr(img_file, 'file_path', None) if img_file else None
                        if fp:
                            if fp.lower().endswith('.dir'):
                                for _ext in ('.img', '.IMG'):
                                    _c = fp[:-4] + _ext
                                    if _os.path.exists(_c):
                                        fp = _c; break
                            with open(fp, 'rb') as _f:
                                _f.seek(entry.offset)
                                data = _f.read(16)
                    except Exception:
                        pass
                if data and len(data) >= 12:
                    for base in (0, 4, 8):
                        off = base + 8
                        if len(data) >= off + 4:
                            v, _ = parse_rw_version(data[off:off+4])
                            if is_valid_rw_version(v):
                                name = get_rw_version_name(v)
                                entry.rw_version = v
                                entry.rw_version_name = name
                                return name
                return "RW File"
            elif entry_type == 'COL':
                return "COL File"
            elif entry_type in ['IPL', 'IDE', 'DAT']:
                return f"{entry_type} File"
            else:
                return "Unknown"
        except Exception:
            return "Unknown"

    def get_compression_info(self, entry: Any) -> str: #vers 3
        """Get encoding info - compression + encryption"""
        try:
            parts = []
            # Encryption takes priority in display
            if hasattr(entry, 'is_encrypted') and entry.is_encrypted:
                enc = getattr(entry, 'encryption_type', None)
                if enc and str(enc.value).lower() != 'none':
                    parts.append(f'Enc:{enc.value}')
                else:
                    parts.append('Encrypted')
            # Compression
            if hasattr(entry, 'compression_type') and entry.compression_type:
                ct = str(entry.compression_type.value).upper()
                if ct not in ('NONE', 'UNKNOWN', ''):
                    parts.append(ct)
            return ' + '.join(parts) if parts else 'None'
        except Exception:
            return 'None'


    def get_info_light(self, entry: Any) -> str: #vers 4
        """Get entry info - LIGHT processing, no heavy detection"""
        try:
            info_parts = []
            if hasattr(entry, 'is_new_entry') and entry.is_new_entry:
                info_parts.append("Imported")
            elif hasattr(entry, 'is_replaced') and entry.is_replaced:
                info_parts.append("Replaced")
            else:
                info_parts.append("Original")
            
            # Check if entry is pinned
            if hasattr(entry, 'is_pinned') and entry.is_pinned:
                info_parts.append("Pinned")
            
            entry_type = self.get_img_entry_type_simple(entry)
            if entry_type in ['DFF']:
                info_parts.append("Model")
            elif entry_type in ['TXD']:
                info_parts.append("Texture")
            elif entry_type in ['COL']:
                info_parts.append("Collision")
            return " • ".join(info_parts) if info_parts else "Original"  # ✅ CLEAN SEPARATOR
        except Exception:
            return "Original"

    def create_img_table_item(self, text: str, is_highlighted: bool = False, highlight_type: str = None) -> QTableWidgetItem: #vers 5
        """Create table item with optional highlighting and pin icon - OPTIMIZED"""
        try:
            from PyQt6.QtWidgets import QTableWidgetItem
            from PyQt6.QtGui import QColor, QBrush, QFont
            from PyQt6.QtCore import Qt
            
            item = QTableWidgetItem(str(text))
            
            # Apply highlighting if needed - OPTIMIZED
            if is_highlighted and highlight_type:
                if highlight_type == "imported":
                    # Light green background for newly imported files
                    item.setBackground(QBrush(QColor(200, 255, 200)))  # Light green
                    item.setForeground(QBrush(QColor(0, 100, 0)))      # Dark green text
                    # Make text bold - OPTIMIZED
                    font = item.font()
                    font.setBold(True)
                    item.setFont(font)
                    item.setToolTip("Recently imported file")
                    
                elif highlight_type == "replaced":
                    # Light yellow background for replaced files
                    item.setBackground(QBrush(QColor(255, 255, 200)))  # Light yellow
                    item.setForeground(QBrush(QColor(150, 100, 0)))    # Dark orange text
                    # Make text bold - OPTIMIZED
                    font = item.font()
                    font.setBold(True)
                    item.setFont(font)
                    item.setToolTip("Recently replaced file")
                    
            return item
        except Exception:
            return QTableWidgetItem("Error")

    def get_table_reference(self): #vers 2
        """Get table reference from main window"""
        try:
            if hasattr(self.main_window, 'gui_layout') and hasattr(self.main_window.gui_layout, 'table'):
                return self.main_window.gui_layout.table
            elif hasattr(self.main_window, 'table'):
                return self.main_window.table
            else:
                img_debugger.error("No table found in main window")
                return None
        except Exception as e:
            img_debugger.error(f"Error getting table reference: {str(e)}")
            return None

# Standalone functions for compatibility
def populate_img_table(table, img_file) -> bool: #vers 3
    """Standalone function for IMG table population - MINIMAL VERSION"""
    try:
        class DummyWindow:
            def __init__(self, table):
                self.gui_layout = type('obj', (object,), {'table': table})
        dummy_window = DummyWindow(table)
        populator = IMGTablePopulator(dummy_window)
        return populator.populate_table_with_img_data(img_file)
    except Exception as e:
        img_debugger.error(f"Error in standalone populate_img_table: {e}")
        if table:
            table.setRowCount(0)
        return False

def clear_img_table(main_window) -> bool: #vers 2
    """Clear IMG table contents"""
    try:
        populator = IMGTablePopulator(main_window)
        table = populator.get_table_reference()
        if table:
            table.setRowCount(0)
            table.clearContents()
            return True
        else:
            return False
    except Exception as e:
        img_debugger.error(f"Error clearing IMG table: {str(e)}")
        return False

def refresh_img_table(main_window) -> bool: #vers 2
    """Refresh the IMG table with current data"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            populator = IMGTablePopulator(main_window)
            return populator.populate_table_with_img_data(main_window.current_img)
        else:
            clear_img_table(main_window)
            return False
    except Exception as e:
        img_debugger.error(f"Error refreshing IMG table: {str(e)}")
        return False

def install_img_table_populator(main_window): #vers 2
    """Install IMG table populator into main window"""
    try:
        main_window.populate_img_table = lambda img_file: populate_img_table(
            main_window.gui_layout.table if hasattr(main_window, 'gui_layout') else None,
            img_file
        )
        main_window.clear_img_table = lambda: clear_img_table(main_window)
        main_window.refresh_img_table = lambda: refresh_img_table(main_window)
        img_debugger.info("Minimal IMG table populator installed")
        return True
    except Exception as e:
        img_debugger.error(f"Error installing IMG table populator: {str(e)}")
        return False

def update_img_table_selection_info(main_window) -> bool: #vers 2
    """Update selection info for IMG table"""
    try:
        populator = IMGTablePopulator(main_window)
        table = populator.get_table_reference()
        if not table:
            return False
        selected_rows = len(table.selectionModel().selectedRows())
        total_rows = table.rowCount()
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Selected {selected_rows} of {total_rows} entries")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error updating selection info: {str(e)}")
        return False

# Export functions
def populate_col_column(table, paired: list) -> int: #vers 1
    """Fill the hidden COL column (index 8) from a hybrid-load pairing list
    and make the column visible.

    Args:
        table:  QTableWidget already populated with IMG entries
        paired: list of (dff_entry, col_source_str or None) from open_hybrid_load

    Returns the number of DFF rows that received a COL match.

    Column text per row:
      DFF with COL found  ->  "✓ stem  (source)"  or  "✓ stem"  green-ish text
      DFF with no COL     ->  "✗ missing"          red-ish text
      Non-DFF row         ->  ""                   (blank)
    """
    from PyQt6.QtGui import QColor
    from PyQt6.QtWidgets import QTableWidgetItem
    from PyQt6.QtCore import Qt

    if not table or not paired:
        return 0

    # Build a quick name -> col_source map from the pairing list
    pair_map = {}
    for entry, source in paired:
        name = getattr(entry, "name", "") or ""
        pair_map[name.lower()] = source  # source is str or None

    hits = 0
    for row in range(table.rowCount()):
        name_item = table.item(row, 0)
        if not name_item:
            continue
        name = name_item.text().lower()
        col_item = QTableWidgetItem()
        col_item.setFlags(Qt.ItemFlag.ItemIsEnabled | Qt.ItemFlag.ItemIsSelectable)

        if name.endswith(".dff"):
            source = pair_map.get(name)
            if source is not None:
                # Show just the stem + source archive if external
                stem = name.rsplit(".", 1)[0]
                # source already contains "(archive.col)" suffix if external
                display = f"✓ {source}"
                col_item.setText(display)
                col_item.setForeground(QColor("#4CAF50"))   # green
                hits += 1
            else:
                col_item.setText("✗ missing")
                col_item.setForeground(QColor("#F44336"))   # red
        # non-DFF rows: leave blank

        table.setItem(row, 8, col_item)

    table.setColumnHidden(8, False)
    return hits



def _set_cell(table, row: int, col: int, text: str): #vers 1
    """Set text in a table cell, creating the item if needed.
    Silently skips if col is out of range."""
    if col >= table.columnCount():
        return
    from PyQt6.QtWidgets import QTableWidgetItem
    item = table.item(row, col)
    if item is None:
        item = QTableWidgetItem()
        table.setItem(row, col, item)
    item.setText(str(text))


def apply_xref_status(table, xref) -> int: #vers 2
    """Update the Status column with IDE/COLFILE membership info.
    Detects Status column by header name — works for both table layouts
    (with or without Date column).
    Returns number of rows updated.
    """
    if not table or not xref:
        return 0

    from PyQt6.QtGui import QColor, QBrush
    model_map  = getattr(xref, 'model_map',  {})
    txd_stems  = getattr(xref, 'txd_stems',  set())
    col_stems  = getattr(xref, 'col_stems',  set())

    green  = QColor(0, 160, 0)
    red    = QColor(180, 0, 0)
    grey   = self._get_ui_color('viewport_text')

    # Detect column indices by header name — robust to layout differences
    STATUS = IDE_MODEL = IDE_TXD = -1
    for c in range(table.columnCount()):
        h = table.horizontalHeaderItem(c)
        if not h:
            continue
        t = h.text()
        if t == 'Status':
            STATUS = c
        elif t == 'IDE Model':
            IDE_MODEL = c
        elif t == 'IDE TXD':
            IDE_TXD = c

    if STATUS < 0:
        return 0

    updated = 0
    for row in range(table.rowCount()):
        name_item = table.item(row, 0)
        if not name_item:
            continue
        name = name_item.text()
        if '.' not in name:
            continue
        stem = name.rsplit('.', 1)[0].lower()
        ext  = name.rsplit('.', 1)[1].lower()

        if ext == 'dff':
            obj = model_map.get(stem)
            if obj:
                txd  = getattr(obj, 'txd_name', '')
                text = 'In IDE' + (f' — {txd}' if txd and txd.lower() != 'null' else '')
                color = green
                # Fill hidden IDE columns
                if IDE_MODEL >= 0: _set_cell(table, row, IDE_MODEL, getattr(obj, 'model_name', stem))
                if IDE_TXD >= 0:   _set_cell(table, row, IDE_TXD,   txd if txd and txd.lower() != 'null' else '')
            else:
                text  = 'Not in IDE'
                color = red

        elif ext == 'txd':
            if stem in txd_stems:
                text  = 'In IDE'
                color = green
                if IDE_MODEL >= 0: _set_cell(table, row, IDE_MODEL, stem)
                if IDE_TXD >= 0:   _set_cell(table, row, IDE_TXD,   stem)
            else:
                text  = 'Orphan TXD'
                color = grey

        elif ext == 'col':
            if stem in col_stems:
                text  = 'In COLFILE'
                color = green
            else:
                text  = 'Not in COLFILE'
                color = red

        else:
            continue

        status_item = table.item(row, STATUS)
        if status_item is None:
            from PyQt6.QtWidgets import QTableWidgetItem
            status_item = QTableWidgetItem()
            table.setItem(row, STATUS, status_item)

        # Preserve any existing prefix (Imported / Replaced / Original)
        existing = status_item.text()
        prefix = ''
        for tag in ('Imported', 'Replaced', 'Original', 'Pinned'):
            if tag in existing:
                prefix = tag + ' • '
                break
        status_item.setText(prefix + text)
        status_item.setForeground(QBrush(color))
        updated += 1

    return updated

def apply_xref_tooltips(table, xref) -> int: #vers 2
    """Apply DAT cross-reference tooltips to every row in an IMG table.

    Sets the tooltip on ALL cells of each matching row so the info bubble
    appears wherever the cursor lands on the row, not only on the Name cell.
    Returns the number of rows that received a non-empty tooltip.

    Args:
        table: QTableWidget populated by _populate_real_img_table
        xref:  GTAWorldXRef built from build_xref(loader)
    """
    if not table or not xref:
        return 0
    hits = 0
    col_count = table.columnCount()
    for row in range(table.rowCount()):
        name_item = table.item(row, 0)
        if not name_item:
            continue
        tip = xref.tooltip_for(name_item.text())
        if tip:
            # Set on every column so hover anywhere on the row shows the bubble
            for col in range(col_count):
                cell = table.item(row, col)
                if cell:
                    cell.setToolTip(tip)
            hits += 1
    return hits


__all__ = [
    'DragSelectTableWidget',
    'IMGTablePopulator',
    'populate_img_table',
    'refresh_img_table',
    'clear_img_table',
    'install_img_table_populator',
    'update_img_table_selection_info',
    'apply_xref_status',
    'apply_xref_tooltips',
    'populate_col_column',
]
