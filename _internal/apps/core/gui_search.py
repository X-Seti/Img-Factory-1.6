#this belongs in gui/ gui_search.py - Version: 1
# X-Seti - July13 2025 - IMG Factory 1.5 - Consolidated Search Functions

"""
GUI Search Functions - Consolidated
All search functionality in one place to avoid conflicts and duplication
"""

from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QLabel, QLineEdit, 
                            QCheckBox, QComboBox, QPushButton, QGroupBox, QMessageBox)
from PyQt6.QtCore import QTimer, pyqtSignal
from PyQt6.QtGui import QShortcut, QKeySequence
from typing import Optional, Dict, Any, List
import re


class SearchManager:
    """Main search manager for IMG Factory"""
    
    def __init__(self, main_window):
        self.main_window = main_window
        self.search_timer = QTimer()
        self.search_timer.setSingleShot(True)
        self.search_timer.timeout.connect(self._perform_live_search)
        self.current_matches = []
        self.current_match_index = -1
        self.last_search_text = ""
        
    def setup_search_functionality(self):
        """Setup all search functionality"""
        try:
            # Connect search input if exists
            self._connect_search_input()
            
            # Setup keyboard shortcuts
            self._setup_shortcuts()
            
            self.main_window.log_message("Search functionality setup complete")
            return True
            
        except Exception as e:
            self.main_window.log_message(f"Search setup error: {e}")
            return False
    
    def _setup_shortcuts(self):
        """Setup search keyboard shortcuts"""
        try:
            from .shortcuts import setup_search_shortcuts
            setup_search_shortcuts(self.main_window)
            self.main_window.log_message("Search shortcuts setup complete")
        except Exception as e:
            self.main_window.log_message(f"Search shortcuts setup error: {e}")
    
    def _connect_search_input(self):
        """Connect search input widget"""
        search_input = self._find_search_input()
        
        if search_input:
            # Disconnect any existing connections
            try:
                search_input.textChanged.disconnect()
                search_input.returnPressed.disconnect()
            except:
                pass
            
            # Connect to our handlers
            search_input.textChanged.connect(self._on_search_text_changed)
            search_input.returnPressed.connect(self._perform_live_search)
            
            self.main_window.log_message("Search input connected")
        else:
            self.main_window.log_message("Search input not found")
    
    def _find_search_input(self):
        """Find search input widget in various locations"""
        # Try gui_layout first
        if hasattr(self.main_window, 'gui_layout'):
            if hasattr(self.main_window.gui_layout, 'search_input'):
                return self.main_window.gui_layout.search_input
            elif hasattr(self.main_window.gui_layout, 'filter_input'):
                return self.main_window.gui_layout.filter_input
        
        # Try main window direct
        if hasattr(self.main_window, 'filter_input'):
            return self.main_window.filter_input
        elif hasattr(self.main_window, 'search_input'):
            return self.main_window.search_input
            
        return None

    def _on_search_text_changed(self, text):
        """Handle search text changes with debouncing"""
        self.search_timer.stop()
        if text.strip():
            self.search_timer.start(300)  # 300ms delay
        else:
            self._clear_search()
    
    def _perform_live_search(self):
        """Perform live search as user types"""
        try:
            search_text = self._get_search_text()
            
            if not search_text:
                self._clear_search()
                return
            
            # Get file type filter
            file_type = self._get_file_type_filter()
            
            # Create search options for live search
            options = {
                'case_sensitive': False,
                'whole_word': False,
                'regex': False,
                'file_type': file_type
            }
            
            # Perform search
            self.perform_search(search_text, options)
            
        except Exception as e:
            self.main_window.log_message(f"Live search error: {e}")
    
    def _get_search_text(self):
        """Get current search text"""
        search_input = self._find_search_input()
        return search_input.text().strip() if search_input else ""
    
    def _get_file_type_filter(self):
        """Get current file type filter"""
        if hasattr(self.main_window, 'filter_combo'):
            return self.main_window.filter_combo.currentText()
        elif hasattr(self.main_window, 'gui_layout') and hasattr(self.main_window.gui_layout, 'filter_combo'):
            return self.main_window.gui_layout.filter_combo.currentText()
        return "All Files"
    
    def perform_search(self, search_text, options=None):
        """Main search function - works with both live search and dialog search"""
        try:
            if not self.main_window.current_img or not self.main_window.current_img.entries:
                self.main_window.log_message("No IMG file loaded for search")
                return []
            
            # Default options
            if options is None:
                options = {
                    'case_sensitive': False,
                    'whole_word': False,
                    'regex': False,
                    'file_type': 'All Files'
                }
            
            # Perform search
            matches = self._find_matches(search_text, options)
            self.current_matches = matches
            self.current_match_index = -1
            self.last_search_text = search_text
            
            # Update table selection
            self._highlight_matches(matches)
            
            # Log results
            if matches:
                # Send detailed logging to main window
                self.main_window.log_message(f"Found {len(matches)} matches for '{search_text}'")
                # Show first few matches in the activity window
                for idx in matches[:10]:  # Show first 10 matches
                    entry_name = self.main_window.current_img.entries[idx].name
                    self.main_window.log_message(f"  Entry {idx}: {entry_name}")
                if len(matches) > 10:
                    self.main_window.log_message(f"  ... and {len(matches) - 10} more matches")

                if matches:
                    self._select_first_match()
            else:
                self.main_window.log_message(f"No matches found for '{search_text}'")
            
            return matches
            
        except Exception as e:
            self.main_window.log_message(f"Search error: {e}")
            return []
    
    def _find_matches(self, search_text, options):
        """Find matching entries based on search criteria"""
        matches = []
        
        # Check if this is a replace operation
        search_mode = options.get('search_mode', 'search')
        replace_text = options.get('replace_text', '')
        
        # Prepare search text
        if not options.get('case_sensitive', False):
            search_text = search_text.lower()
        
        for i, entry in enumerate(self.main_window.current_img.entries):
            entry_name = entry.name
            
            # Apply case sensitivity
            if not options.get('case_sensitive', False):
                entry_name = entry_name.lower()
            
            # File type filter
            if not self._matches_file_type(entry.name, options.get('file_type', 'All Files')):
                continue
            
            # Check if matches search criteria
            if self._matches_search_criteria(entry_name, search_text, options):
                matches.append(i)
        
        # Handle replace operations
        if search_mode == 'replace_all' and replace_text:
            self._perform_replace_all(matches, search_text, replace_text, options)
        elif search_mode == 'replace_one' and replace_text:
            # This would be handled by the UI to replace the currently selected item
            # For now, just log the operation
            self.main_window.log_message(f"Replace one operation would replace next match of '{search_text}' with '{replace_text}'")
        
        return matches

    def _perform_replace_all(self, matches, search_text, replace_text, options):
        """Perform replace all operation on matched entries"""
        try:
            count = 0
            for match_idx in matches:
                entry = self.main_window.current_img.entries[match_idx]
                old_name = entry.name
                
                # Apply the replacement based on options
                if options.get('regex', False):
                    import re
                    flags = 0 if options.get('case_sensitive', False) else re.IGNORECASE
                    try:
                        new_name = re.sub(search_text, replace_text, old_name, flags=flags)
                    except re.error:
                        self.main_window.log_message(f"Invalid regex pattern: {search_text}")
                        continue
                else:
                    if options.get('case_sensitive', False):
                        new_name = old_name.replace(search_text, replace_text)
                    else:
                        # Case-insensitive replacement
                        import re
                        pattern = re.escape(search_text)
                        new_name = re.sub(pattern, replace_text, old_name, flags=re.IGNORECASE)
                
                # Only update if name actually changed
                if new_name != old_name:
                    entry.name = new_name
                    count += 1
                    
                    # Update the table display if possible
                    if (hasattr(self.main_window, 'gui_layout') and 
                        hasattr(self.main_window.gui_layout, 'table')):
                        table = self.main_window.gui_layout.table
                        if match_idx < table.rowCount():
                            table.item(match_idx, 0).setText(new_name)
            
            self.main_window.log_message(f"Replace All: {count} entries updated")
            QMessageBox.information(
                self.main_window, 
                "Replace Complete", 
                f"Replace All completed: {count} entries updated"
            )
            
            # Mark the IMG as modified
            if hasattr(self.main_window.current_img, 'modified'):
                self.main_window.current_img.modified = True
                
        except Exception as e:
            self.main_window.log_message(f"Error during replace all: {str(e)}")
            QMessageBox.critical(
                self.main_window, 
                "Replace Error", 
                f"An error occurred during replace: {str(e)}"
            )
    
    def _matches_file_type(self, filename, file_type):
        """Check if file matches the selected file type filter"""
        if file_type == 'All Files':
            return True
        
        extension = filename.split('.')[-1].upper() if '.' in filename else ''
        
        type_mapping = {
            'Models (DFF)': 'DFF',
            'Textures (TXD)': 'TXD',
            'Collision (COL)': 'COL',
            'Animation (IFP)': 'IFP',
            'Audio (WAV)': 'WAV',
            'Scripts (SCM)': 'SCM'
        }
        
        return extension == type_mapping.get(file_type, '')
    
    def _matches_search_criteria(self, entry_name, search_text, options):
        """Check if entry name matches search criteria"""
        if options.get('regex', False):
            try:
                return bool(re.search(search_text, entry_name))
            except re.error:
                return False
        elif options.get('whole_word', False):
            pattern = r'\b' + re.escape(search_text) + r'\b'
            return bool(re.search(pattern, entry_name))
        else:
            return search_text in entry_name
    
    def _highlight_matches(self, matches):
        """Highlight matching entries in table"""
        try:
            if not hasattr(self.main_window, 'gui_layout') or not hasattr(self.main_window.gui_layout, 'table'):
                return
            
            table = self.main_window.gui_layout.table
            table.clearSelection()
            
            for row in matches:
                if row < table.rowCount():
                    table.selectRow(row)
            
        except Exception as e:
            self.main_window.log_message(f"Highlight error: {e}")
    
    def _select_first_match(self):
        """Select and scroll to first match"""
        try:
            if self.current_matches and hasattr(self.main_window, 'gui_layout'):
                table = self.main_window.gui_layout.table
                first_match = self.current_matches[0]
                if first_match < table.rowCount():
                    table.scrollToItem(table.item(first_match, 0))
                    self.current_match_index = 0
        except Exception as e:
            self.main_window.log_message(f"Select first match error: {e}")
    
    def _clear_search(self):
        """Clear search results"""
        try:
            if hasattr(self.main_window, 'gui_layout') and hasattr(self.main_window.gui_layout, 'table'):
                self.main_window.gui_layout.table.clearSelection()
            
            self.current_matches = []
            self.current_match_index = -1
            
        except Exception as e:
            self.main_window.log_message(f"Clear search error: {e}")
    
    def find_next(self):
        """Find next match"""
        if not self.current_matches:
            if self.last_search_text:
                # Re-perform last search
                self._perform_live_search()
            return
        
        if self.current_match_index < len(self.current_matches) - 1:
            self.current_match_index += 1
        else:
            self.current_match_index = 0  # Wrap around
        
        self._jump_to_current_match()
    
    def find_previous(self):
        """Find previous match"""
        if not self.current_matches:
            if self.last_search_text:
                # Re-perform last search
                self._perform_live_search()
            return
        
        if self.current_match_index > 0:
            self.current_match_index -= 1
        else:
            self.current_match_index = len(self.current_matches) - 1  # Wrap around
        
        self._jump_to_current_match()
    
    def _jump_to_current_match(self):
        """Jump to current match in table"""
        try:
            if (self.current_matches and 
                0 <= self.current_match_index < len(self.current_matches) and
                hasattr(self.main_window, 'gui_layout')):
                
                table = self.main_window.gui_layout.table
                match_row = self.current_matches[self.current_match_index]
                
                if match_row < table.rowCount():
                    table.clearSelection()
                    table.selectRow(match_row)
                    table.scrollToItem(table.item(match_row, 0))
                    
                    self.main_window.log_message(
                        f"🔍 Match {self.current_match_index + 1} of {len(self.current_matches)}"
                    )
                    
        except Exception as e:
            self.main_window.log_message(f"Jump to match error: {e}")
    
    def show_search_dialog(self):
        """Show advanced search dialog"""
        try:
            dialog = ASearchDialog(self.main_window)
            dialog.search_requested.connect(self.perform_search)
            dialog.exec()
            
        except Exception as e:
            self.main_window.log_message(f"Search dialog error: {e}")


class ASearchDialog(QDialog):
    """Search/filter dialog — filters the active IMG table in real time."""

    search_requested = pyqtSignal(str, dict)

    # Extension groups
    EXT_GROUPS = {
        "All Files":         [],
        "Models (.dff)":     ["dff"],
        "Textures (.txd)":   ["txd"],
        "Collision (.col)":  ["col"],
        "Animation (.ifp)":  ["ifp"],
        "Audio (.wav/.mp3)": ["wav", "mp3", "ogg"],
        "Scripts (.scm)":    ["scm", "cs"],
        "Images (.png/.bmp)":["png", "bmp", "jpg", "jpeg"],
        "Data (.dat/.cfg)":  ["dat", "cfg", "ide", "ipl"],
    }

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Search / Filter IMG")
        self.setModal(False)          # non-modal so user can scroll table
        self.resize(460, 380)
        self._matches = []
        self._match_idx = -1
        self._setup_ui()
        from apps.core.theme_utils import apply_dialog_theme
        apply_dialog_theme(self)
        # Filter on type-change (extension combo), Enter key, or Find button
        # Note: textChanged NOT connected — user presses Find or Enter to search
        self.ext_combo.currentIndexChanged.connect(self._apply_filter)

    def _setup_ui(self):
        layout = QVBoxLayout(self)
        layout.setSpacing(6)

        #    Search row with Find button                                     
        row1 = QHBoxLayout()
        row1.addWidget(QLabel("Search:"))
        self.search_input = QLineEdit()
        self.search_input.setPlaceholderText("Name, extension, type…")
        self.search_input.setClearButtonEnabled(True)
        self.search_input.returnPressed.connect(self._apply_filter)
        row1.addWidget(self.search_input, 1)
        self.find_btn = QPushButton("Find")
        self.find_btn.setDefault(True)
        self.find_btn.setFixedWidth(60)
        self.find_btn.clicked.connect(self._apply_filter)
        row1.addWidget(self.find_btn)
        layout.addLayout(row1)

        #    Extension/type filter                                           
        row2 = QHBoxLayout()
        row2.addWidget(QLabel("Type:"))
        self.ext_combo = QComboBox()
        self.ext_combo.addItems(list(self.EXT_GROUPS.keys()))
        row2.addWidget(self.ext_combo, 1)
        layout.addLayout(row2)

        #    Options                                                         
        opt_row = QHBoxLayout()
        self.case_chk  = QCheckBox("Case sensitive")
        self.regex_chk = QCheckBox("Regex")
        opt_row.addWidget(self.case_chk)
        opt_row.addWidget(self.regex_chk)
        opt_row.addStretch()
        layout.addLayout(opt_row)

        #    Results summary                                                 
        self.result_lbl = QLabel("Enter search text or select type and click Find")
        self.result_lbl.setStyleSheet("font-style: italic; color: palette(mid);")
        layout.addWidget(self.result_lbl)

        #    Nav + action buttons                                            
        btn_row = QHBoxLayout()
        self.prev_btn  = QPushButton("◀ Prev")
        self.next_btn  = QPushButton("Next ▶")
        self.clear_btn = QPushButton("Clear")
        self.close_btn = QPushButton("Close")
        self.prev_btn.setEnabled(False)
        self.next_btn.setEnabled(False)
        self.prev_btn.clicked.connect(self._prev_match)
        self.next_btn.clicked.connect(self._next_match)
        self.clear_btn.clicked.connect(self._clear)
        self.close_btn.clicked.connect(self.close)
        for b in [self.prev_btn, self.next_btn, self.clear_btn, self.close_btn]:
            btn_row.addWidget(b)
        layout.addLayout(btn_row)

        self.search_input.setFocus()

    #    Filter logic                                                        

    def _on_text_changed(self, text):
        self._apply_filter()

    def _apply_filter(self):
        mw = self.parent()
        if not mw: return

        # Get current table
        table = self._get_table(mw)
        if table is None:
            self.result_lbl.setText("No IMG table visible — open an IMG file first")
            self.result_lbl.setStyleSheet("color: palette(placeholderText); font-style: italic;")
            return
        if table.rowCount() == 0:
            self.result_lbl.setText("Table is empty — no entries loaded")
            self.result_lbl.setStyleSheet("color: palette(placeholderText); font-style: italic;")
            return

        text    = self.search_input.text().strip()
        ext_key = self.ext_combo.currentText()
        exts    = self.EXT_GROUPS.get(ext_key, [])
        case    = self.case_chk.isChecked()
        use_re  = self.regex_chk.isChecked()

        if use_re and text:
            try:
                flags = 0 if case else re.IGNORECASE
                pattern = re.compile(text, flags)
            except re.error:
                self.result_lbl.setText("Invalid regex")
                return
        else:
            pattern = None

        matches = []
        total   = table.rowCount()

        for row in range(total):
            # Get filename from first text column
            name = ""
            for col in range(table.columnCount()):
                item = table.item(row, col)
                if item and item.text().strip():
                    name = item.text().strip()
                    break

            # Extension filter
            if exts:
                ext = name.rsplit(".", 1)[-1].lower() if "." in name else ""
                if ext not in exts:
                    table.setRowHidden(row, True)
                    continue

            # Text filter
            if text:
                haystack = name if case else name.lower()
                needle   = text if case else text.lower()
                if pattern:
                    hit = bool(pattern.search(name))
                else:
                    hit = needle in haystack
                if not hit:
                    table.setRowHidden(row, True)
                    continue

            table.setRowHidden(row, False)
            matches.append(row)

        self._matches = matches
        self._match_idx = 0 if matches else -1

        visible = len(matches)
        if not text and not exts:
            self.result_lbl.setText(f"{total} entries")
            self.result_lbl.setStyleSheet("font-style: italic; color: palette(mid);")
        elif visible:
            self.result_lbl.setText(f"Showing {visible} of {total} entries")
            self.result_lbl.setStyleSheet("color: palette(windowText); font-weight: bold;")
        else:
            self.result_lbl.setText(f"No matches in {total} entries")
            self.result_lbl.setStyleSheet("color: palette(windowText); font-weight: bold;")

        has_nav = len(matches) > 1
        self.prev_btn.setEnabled(has_nav)
        self.next_btn.setEnabled(has_nav)

        # Scroll to first match
        if matches:
            table.scrollToItem(table.item(matches[0], 0))
            table.selectRow(matches[0])

        self.search_requested.emit(text, {"ext": ext_key})

    def _next_match(self):
        if not self._matches: return
        self._match_idx = (self._match_idx + 1) % len(self._matches)
        self._scroll_to_match()

    def _prev_match(self):
        if not self._matches: return
        self._match_idx = (self._match_idx - 1) % len(self._matches)
        self._scroll_to_match()

    def _scroll_to_match(self):
        mw = self.parent()
        table = self._get_table(mw)
        if table and 0 <= self._match_idx < len(self._matches):
            row = self._matches[self._match_idx]
            table.scrollToItem(table.item(row, 0))
            table.selectRow(row)

    def _clear(self):
        self.search_input.clear()
        self.ext_combo.setCurrentIndex(0)
        mw = self.parent()
        table = self._get_table(mw)
        if table:
            for row in range(table.rowCount()):
                table.setRowHidden(row, False)
        self.result_lbl.setText("Type to filter…")
        self.result_lbl.setStyleSheet("font-style: italic; color: palette(mid);")
        self.prev_btn.setEnabled(False)
        self.next_btn.setEnabled(False)

    def _get_table(self, mw):
        """Get the currently visible IMG table from the active tab."""
        if mw is None: return None

        # Primary: main_tab_widget (IMG Factory uses this name)
        tw = getattr(mw, 'main_tab_widget', None) or getattr(mw, 'tab_widget', None)
        if tw:
            tab = tw.currentWidget()
            if tab:
                # table_ref is how IMG Factory stores the table on each tab
                t = getattr(tab, 'table_ref', None)
                if t and hasattr(t, 'rowCount'): return t
                # Fallback: findChildren
                from PyQt6.QtWidgets import QTableWidget
                tables = [w for w in tab.findChildren(QTableWidget)
                          if w.rowCount() > 0]
                if tables: return tables[0]

        # Also check gui_layout.table (sync'd on tab switch)
        gl = getattr(mw, 'gui_layout', None)
        if gl:
            t = getattr(gl, 'table', None)
            if t and hasattr(t, 'rowCount'): return t

        # Last resort — direct attributes
        for attr in ('table_widget', 'entry_table', 'img_table', 'table'):
            t = getattr(mw, attr, None)
            if t and hasattr(t, 'rowCount'): return t

        return None

    def closeEvent(self, event):
        # Restore all hidden rows when dialog is closed
        self._clear()
        super().closeEvent(event)

    def update_results(self, match_count, total_entries):
        self._apply_filter()


# Legacy support functions for backward compatibility
def setup_search_functionality(main_window):
    """Legacy function - redirects to SearchManager"""
    if not hasattr(main_window, 'search_manager'):
        main_window.search_manager = SearchManager(main_window)
    return main_window.search_manager.setup_search_functionality()


def perform_search(main_window, search_text=None, options=None):
    """Legacy function - redirects to SearchManager"""
    if not hasattr(main_window, 'search_manager'):
        main_window.search_manager = SearchManager(main_window)
    
    if search_text is None:
        search_text = main_window.search_manager._get_search_text()
    
    return main_window.search_manager.perform_search(search_text, options)


def show_search_dialog(main_window):
    """Legacy function - redirects to SearchManager"""
    if not hasattr(main_window, 'search_manager'):
        main_window.search_manager = SearchManager(main_window)
    main_window.search_manager.show_search_dialog()


# Quick setup function for main window
def install_search_system(main_window):
    """Install complete search system on main window"""
    try:
        # Create search manager
        main_window.search_manager = SearchManager(main_window)
        
        # Setup functionality
        success = main_window.search_manager.setup_search_functionality()
        
        # Add convenience methods to main window
        main_window.show_search_dialog = main_window.search_manager.show_search_dialog
        main_window.perform_search = main_window.search_manager.perform_search
        main_window.find_next = main_window.search_manager.find_next
        main_window.find_previous = main_window.search_manager.find_previous
        
        if success:
            main_window.log_message("✅ Complete search system installed")
        
        return success
        
    except Exception as e:
        main_window.log_message(f"❌ Search system installation error: {e}")
        return False

__all__ = [
    'ASearchDialog',
    'SearchManager'
]
