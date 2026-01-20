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
                self.main_window.log_message(f"🔍 Found {len(matches)} matches for '{search_text}'")
                # Show first few matches in the activity window
                for idx in matches[:10]:  # Show first 10 matches
                    entry_name = self.main_window.current_img.entries[idx].name
                    self.main_window.log_message(f"  📄 Entry {idx}: {entry_name}")
                if len(matches) > 10:
                    self.main_window.log_message(f"  ... and {len(matches) - 10} more matches")
                    
                if matches:
                    self._select_first_match()
            else:
                self.main_window.log_message(f"❌ No matches found for '{search_text}'")
            
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
    """Advanced search dialog with options"""
    
    search_requested = pyqtSignal(str, dict)
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.matches = []
        self.current_match = -1
        self.setWindowTitle("Advanced Search")
        self.setModal(True)
        self.resize(400, 300)
        self._setup_ui()
    
    def _setup_ui(self):
        """Setup dialog UI"""
        layout = QVBoxLayout(self)

        # Search criteria group
        search_group = QGroupBox("Search Criteria")
        search_layout = QVBoxLayout(search_group)

        # Search text
        text_layout = QHBoxLayout()
        text_layout.addWidget(QLabel("Search for:"))
        self.search_input = QLineEdit()
        self.search_input.setPlaceholderText("Enter search text...")
        text_layout.addWidget(self.search_input)
        search_layout.addLayout(text_layout)

        # Search options
        options_layout = QVBoxLayout()

        self.case_sensitive_check = QCheckBox("Case sensitive")
        options_layout.addWidget(self.case_sensitive_check)

        self.whole_word_check = QCheckBox("Whole word only")
        options_layout.addWidget(self.whole_word_check)

        self.regex_check = QCheckBox("Regular expression")
        options_layout.addWidget(self.regex_check)

        search_layout.addLayout(options_layout)

        # File type filter
        type_layout = QHBoxLayout()
        type_layout.addWidget(QLabel("File type:"))
        self.type_combo = QComboBox()
        self.type_combo.addItems([
            "All Files", "Models (DFF)", "Textures (TXD)",
            "Collision (COL)", "Animation (IFP)", "Audio (WAV)", "Scripts (SCM)"
        ])
        type_layout.addWidget(self.type_combo)
        search_layout.addLayout(type_layout)

        layout.addWidget(search_group)

        # Results area
        results_group = QGroupBox("Search Results")
        results_layout = QVBoxLayout(results_group)

        self.results_label = QLabel("Enter search criteria and click Find")
        self.results_label.setStyleSheet("color: #666666; font-style: italic;")
        results_layout.addWidget(self.results_label)

        layout.addWidget(results_group)

        # Buttons
        button_layout = QHBoxLayout()

        self.find_btn = QPushButton("Find")
        self.find_btn.clicked.connect(self._do_search)
        self.find_btn.setDefault(True)
        button_layout.addWidget(self.find_btn)

        self.find_next_btn = QPushButton("Find Next")
        self.find_next_btn.clicked.connect(self._find_next)
        self.find_next_btn.setEnabled(False)
        button_layout.addWidget(self.find_next_btn)

        button_layout.addStretch()

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.close)
        button_layout.addWidget(close_btn)

        layout.addLayout(button_layout)

        # Focus on search input
        self.search_input.setFocus()
    
    def _do_search(self):
        """Perform search"""
        search_text = self.search_input.text().strip()
        if not search_text:
            QMessageBox.warning(self, "Search", "Please enter search text.")
            return
        
        options = {
            'case_sensitive': self.case_sensitive_check.isChecked(),
            'whole_word': self.whole_word_check.isChecked(),
            'regex': self.regex_check.isChecked(),
            'file_type': self.type_combo.currentText()
        }
        
        # Emit search request
        self.search_requested.emit(search_text, options)
        
        # Enable find next button
        self.find_next_btn.setEnabled(True)
        
        # Update results display
        self.results_label.setText(f"Search performed for: '{search_text}'")
    
    def _find_next(self):
        """Find next match"""
        if hasattr(self.parent(), 'search_manager'):
            self.parent().search_manager.find_next()
    
    def update_results(self, match_count, total_entries):
        """Update results display"""
        if match_count > 0:
            self.results_label.setText(f"Found {match_count} matches out of {total_entries} entries")
            self.results_label.setStyleSheet("color: #006600; font-weight: bold;")
        else:
            self.results_label.setText("No matches found")
            self.results_label.setStyleSheet("color: #CC0000; font-weight: bold;")


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
