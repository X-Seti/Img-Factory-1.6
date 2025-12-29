#this belongs in core/notepad.py - Version: 1
# X-Seti - August10 2025 - IMG Factory 1.5 - Integrated Text Editor

"""
IMG FACTORY TEXT EDITOR
Notepad++-style text editor integrated with IMG Factory
Specialized for GTA modding files (.ide, .ipl, .dat, .cfg, etc.)
"""

import os
import re
from typing import Dict, List, Optional, Tuple
from PyQt6.QtWidgets import (
    QMainWindow, QWidget, QVBoxLayout, QHBoxLayout, QTextEdit, QMenuBar,
    QMenu, QToolBar, QPushButton, QLabel, QLineEdit, QDialog, QMessageBox,
    QFileDialog, QTabWidget, QSplitter, QListWidget, QTreeWidget, QTreeWidgetItem,
    QCheckBox, QSpinBox, QComboBox, QGroupBox, QFormLayout, QTextBrowser,
    QStatusBar, QProgressBar, QSizePolicy, QFrame
)
from PyQt6.QtCore import Qt, pyqtSignal, QTimer, QThread, QSettings, QSize
from PyQt6.QtGui import (
    QFont, QFontMetrics, QSyntaxHighlighter, QTextCharFormat, QColor,
    QAction, QKeySequence, QTextCursor, QTextDocument, QPalette, QIcon
)

##Methods list -
# apply_syntax_highlighting
# auto_complete_text
# create_gta_text_editor
# find_and_replace
# format_gta_file
# get_gta_syntax_rules
# open_text_file_in_editor
# save_current_file
# setup_editor_menus
# setup_editor_toolbar
# validate_gta_syntax

##Classes -
# GTASyntaxHighlighter
# IMGFactoryTextEditor
# FindReplaceDialog
# GotoLineDialog

class GTASyntaxHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for GTA modding files"""
    
    def __init__(self, parent=None, file_type="ide"): #vers 1
        super().__init__(parent)
        self.file_type = file_type.lower()
        self.setup_highlighting_rules()
        
    def setup_highlighting_rules(self): #vers 1
        """Setup syntax highlighting rules for different GTA file types"""
        self.highlighting_rules = []
        
        # Common patterns
        self.setup_common_rules()
        
        # File-specific patterns
        if self.file_type == 'ide':
            self.setup_ide_rules()
        elif self.file_type == 'ipl':
            self.setup_ipl_rules()
        elif self.file_type == 'dat':
            self.setup_dat_rules()
        elif self.file_type == 'cfg':
            self.setup_cfg_rules()
            
    def setup_common_rules(self): #vers 1
        """Setup common highlighting rules"""
        # Comments (# and //)
        comment_format = QTextCharFormat()
        comment_format.setForeground(QColor(0, 128, 0))
        comment_format.setFontItalic(True)
        self.add_rule(r'#[^\r\n]*', comment_format)
        self.add_rule(r'//[^\r\n]*', comment_format)
        
        # Numbers
        number_format = QTextCharFormat()
        number_format.setForeground(QColor(0, 0, 255))
        self.add_rule(r'\b\d+\.?\d*\b', number_format)
        
        # Negative numbers
        self.add_rule(r'\b-\d+\.?\d*\b', number_format)
        
        # Strings in quotes
        string_format = QTextCharFormat()
        string_format.setForeground(QColor(163, 21, 21))
        self.add_rule(r'"[^"]*"', string_format)
        
    def setup_ide_rules(self): #vers 1
        """Setup IDE file specific rules"""
        # IDE keywords
        keyword_format = QTextCharFormat()
        keyword_format.setForeground(QColor(128, 0, 128))
        keyword_format.setFontWeight(QFont.Weight.Bold)
        
        ide_keywords = [
            'objs', 'tobj', 'weap', 'hier', 'cars', 'peds', 'path', 'anim'
        ]
        
        for keyword in ide_keywords:
            pattern = rf'\b{keyword}\b'
            self.add_rule(pattern, keyword_format)
            
        # Model names (usually .dff files)
        model_format = QTextCharFormat()
        model_format.setForeground(QColor(0, 128, 128))
        self.add_rule(r'\b\w+\.dff\b', model_format)
        
        # Texture names (usually .txd files)  
        texture_format = QTextCharFormat()
        texture_format.setForeground(QColor(128, 128, 0))
        self.add_rule(r'\b\w+\.txd\b', texture_format)
        
    def setup_ipl_rules(self): #vers 1
        """Setup IPL file specific rules"""
        # IPL sections
        section_format = QTextCharFormat()
        section_format.setForeground(QColor(128, 0, 128))
        section_format.setFontWeight(QFont.Weight.Bold)
        
        ipl_sections = [
            'inst', 'pick', 'cull', 'path', 'grge', 'enex', 'cars', 'jump'
        ]
        
        for section in ipl_sections:
            pattern = rf'\b{section}\b'
            self.add_rule(pattern, section_format)
            
    def setup_dat_rules(self): #vers 1
        """Setup DAT file specific rules"""
        # File paths
        path_format = QTextCharFormat()
        path_format.setForeground(QColor(0, 128, 128))
        self.add_rule(r'\b\w+[\\/]\w+.*\b', path_format)
        
        # IMG references
        img_format = QTextCharFormat()
        img_format.setForeground(QColor(128, 0, 0))
        img_format.setFontWeight(QFont.Weight.Bold)
        self.add_rule(r'\b\w+\.img\b', img_format)
        
    def setup_cfg_rules(self): #vers 1
        """Setup CFG file specific rules"""
        # Configuration keys
        key_format = QTextCharFormat()
        key_format.setForeground(QColor(0, 0, 128))
        key_format.setFontWeight(QFont.Weight.Bold)
        self.add_rule(r'^\s*\w+\s*=', key_format)
        
    def add_rule(self, pattern: str, format: QTextCharFormat): #vers 1
        """Add highlighting rule"""
        self.highlighting_rules.append((re.compile(pattern), format))
        
    def highlightBlock(self, text: str): #vers 1
        """Apply highlighting to text block"""
        for pattern, format in self.highlighting_rules:
            for match in pattern.finditer(text):
                start, end = match.span()
                self.setFormat(start, end - start, format)


class FindReplaceDialog(QDialog):
    """Find and replace dialog"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Find and Replace")
        self.setModal(True)
        self.setFixedSize(400, 200)
        self.setup_ui()
        
    def setup_ui(self): #vers 1
        """Setup find/replace UI"""
        layout = QVBoxLayout(self)
        
        # Find section
        find_layout = QHBoxLayout()
        find_layout.addWidget(QLabel("Find:"))
        self.find_edit = QLineEdit()
        find_layout.addWidget(self.find_edit)
        layout.addLayout(find_layout)
        
        # Replace section
        replace_layout = QHBoxLayout()
        replace_layout.addWidget(QLabel("Replace:"))
        self.replace_edit = QLineEdit()
        replace_layout.addWidget(self.replace_edit)
        layout.addLayout(replace_layout)
        
        # Options
        options_layout = QHBoxLayout()
        self.case_sensitive_check = QCheckBox("Case sensitive")
        self.whole_word_check = QCheckBox("Whole word")
        options_layout.addWidget(self.case_sensitive_check)
        options_layout.addWidget(self.whole_word_check)
        layout.addLayout(options_layout)
        
        # Buttons
        button_layout = QHBoxLayout()
        
        find_next_btn = QPushButton("Find Next")
        find_next_btn.clicked.connect(self.find_next)
        button_layout.addWidget(find_next_btn)
        
        replace_btn = QPushButton("Replace")
        replace_btn.clicked.connect(self.replace_current)
        button_layout.addWidget(replace_btn)
        
        replace_all_btn = QPushButton("Replace All")
        replace_all_btn.clicked.connect(self.replace_all)
        button_layout.addWidget(replace_all_btn)
        
        layout.addLayout(button_layout)
        
    def find_next(self): #vers 1
        """Find next occurrence"""
        # Implementation would search in parent text editor
        pass
        
    def replace_current(self): #vers 1
        """Replace current selection"""
        # Implementation would replace in parent text editor
        pass
        
    def replace_all(self): #vers 1
        """Replace all occurrences"""
        # Implementation would replace all in parent text editor
        pass


class GotoLineDialog(QDialog):
    """Go to line dialog"""
    
    def __init__(self, parent=None, max_line=1):
        super().__init__(parent)
        self.max_line = max_line
        self.setWindowTitle("Go to Line")
        self.setModal(True)
        self.setFixedSize(300, 120)
        self.setup_ui()
        
    def setup_ui(self): #vers 1
        """Setup goto line UI"""
        layout = QVBoxLayout(self)
        
        # Line input
        input_layout = QHBoxLayout()
        input_layout.addWidget(QLabel("Line number:"))
        self.line_edit = QSpinBox()
        self.line_edit.setRange(1, self.max_line)
        input_layout.addWidget(self.line_edit)
        layout.addLayout(input_layout)
        
        # Info
        info_label = QLabel(f"(1 - {self.max_line})")
        info_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(info_label)
        
        # Buttons
        button_layout = QHBoxLayout()
        
        ok_btn = QPushButton("Go")
        ok_btn.clicked.connect(self.accept)
        button_layout.addWidget(ok_btn)
        
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)
        
        layout.addLayout(button_layout)
        
    def get_line_number(self): #vers 1
        """Get selected line number"""
        return self.line_edit.value()


class IMGFactoryTextEditor(QMainWindow):
    """Main text editor window for IMG Factory"""
    
    # Signals
    file_saved = pyqtSignal(str)
    file_closed = pyqtSignal(str)
    
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.parent_window = parent
        self.current_files = {}  # tab_index: file_info
        self.settings = QSettings("IMG Factory", "Text Editor")
        self.setup_ui()
        self.setup_menus()
        self.setup_toolbar()
        self.setup_status_bar()
        self.load_settings()
        
    def setup_ui(self): #vers 1
        """Setup main UI"""
        self.setWindowTitle("IMG Factory Text Editor")
        self.setGeometry(200, 200, 1000, 700)
        
        # Central widget with tabs
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        
        layout = QVBoxLayout(central_widget)
        layout.setContentsMargins(2, 2, 2, 2)
        
        # Tab widget for multiple files
        self.tab_widget = QTabWidget()
        self.tab_widget.setTabsClosable(True)
        self.tab_widget.tabCloseRequested.connect(self.close_tab)
        self.tab_widget.currentChanged.connect(self.on_tab_changed)
        
        layout.addWidget(self.tab_widget)
        
        # If no tabs, show welcome message
        self.show_welcome_tab()
        
    def setup_menus(self): #vers 1
        """Setup menu bar"""
        menubar = self.menuBar()
        
        # File menu
        file_menu = menubar.addMenu("📁 &File")
        
        new_action = QAction("&New", self)
        new_action.setShortcut("Ctrl+N")
        new_action.triggered.connect(self.new_file)
        file_menu.addAction(new_action)
        
        open_action = QAction("&Open...", self)
        open_action.setShortcut("Ctrl+O")
        open_action.triggered.connect(self.open_file)
        file_menu.addAction(open_action)
        
        file_menu.addSeparator()
        
        save_action = QAction("💾 &Save", self)
        save_action.setShortcut("Ctrl+S")
        save_action.triggered.connect(self.save_current_file)
        file_menu.addAction(save_action)
        
        save_as_action = QAction("💾 Save &As...", self)
        save_as_action.setShortcut("Ctrl+Shift+S")
        save_as_action.triggered.connect(self.save_as)
        file_menu.addAction(save_as_action)
        
        file_menu.addSeparator()
        
        close_action = QAction("❌ &Close", self)
        close_action.setShortcut("Ctrl+W")
        close_action.triggered.connect(self.close_current_tab)
        file_menu.addAction(close_action)
        
        # Edit menu
        edit_menu = menubar.addMenu("✏️ &Edit")
        
        undo_action = QAction("⤴️ &Undo", self)
        undo_action.setShortcut("Ctrl+Z")
        undo_action.triggered.connect(self.undo)
        edit_menu.addAction(undo_action)
        
        redo_action = QAction("⤵️ &Redo", self)
        redo_action.setShortcut("Ctrl+Y")
        redo_action.triggered.connect(self.redo)
        edit_menu.addAction(redo_action)
        
        edit_menu.addSeparator()
        
        find_action = QAction("🔍 &Find...", self)
        find_action.setShortcut("Ctrl+F")
        find_action.triggered.connect(self.show_find_dialog)
        edit_menu.addAction(find_action)
        
        goto_action = QAction("🎯 &Go to Line...", self)
        goto_action.setShortcut("Ctrl+G")
        goto_action.triggered.connect(self.show_goto_dialog)
        edit_menu.addAction(goto_action)
        
        # View menu
        view_menu = menubar.addMenu("👁️ &View")
        
        zoom_in_action = QAction("🔍+ Zoom &In", self)
        zoom_in_action.setShortcut("Ctrl++")
        zoom_in_action.triggered.connect(self.zoom_in)
        view_menu.addAction(zoom_in_action)
        
        zoom_out_action = QAction("🔍- Zoom &Out", self)
        zoom_out_action.setShortcut("Ctrl+-")
        zoom_out_action.triggered.connect(self.zoom_out)
        view_menu.addAction(zoom_out_action)
        
        # Tools menu
        tools_menu = menubar.addMenu("🔧 &Tools")
        
        validate_action = QAction("✅ &Validate GTA Syntax", self)
        validate_action.triggered.connect(self.validate_syntax)
        tools_menu.addAction(validate_action)
        
        format_action = QAction("📐 &Format Document", self)
        format_action.triggered.connect(self.format_document)
        tools_menu.addAction(format_action)
        
    def setup_toolbar(self): #vers 1
        """Setup toolbar"""
        toolbar = self.addToolBar("Main")
        toolbar.setMovable(False)
        
        # File operations
        new_btn = QPushButton("New")
        new_btn.clicked.connect(self.new_file)
        toolbar.addWidget(new_btn)
        
        open_btn = QPushButton("Open")
        open_btn.clicked.connect(self.open_file)
        toolbar.addWidget(open_btn)
        
        save_btn = QPushButton("💾 Save")
        save_btn.clicked.connect(self.save_current_file)
        toolbar.addWidget(save_btn)
        
        toolbar.addSeparator()
        
        # Text operations
        find_btn = QPushButton("🔍 Find")
        find_btn.clicked.connect(self.show_find_dialog)
        toolbar.addWidget(find_btn)
        
        toolbar.addSeparator()
        
        # GTA tools
        validate_btn = QPushButton("✅ Validate")
        validate_btn.clicked.connect(self.validate_syntax)
        toolbar.addWidget(validate_btn)
        
    def setup_status_bar(self): #vers 1
        """Setup status bar"""
        self.status_bar = self.statusBar()
        
        # Line/column indicator
        self.line_col_label = QLabel("Line: 1, Column: 1")
        self.status_bar.addPermanentWidget(self.line_col_label)
        
        # File type indicator
        self.file_type_label = QLabel("Plain Text")
        self.status_bar.addPermanentWidget(self.file_type_label)
        
    def show_welcome_tab(self): #vers 1
        """Show welcome tab when no files are open"""
        if self.tab_widget.count() == 0:
            welcome_widget = QTextBrowser()
            welcome_widget.setHtml("""
                <h2>🎮 IMG Factory Text Editor</h2>
                <p>Specialized text editor for GTA modding files</p>
                <ul>
                    <li><b>📝 IDE Files:</b> Item definitions with syntax highlighting</li>
                    <li><b>📍 IPL Files:</b> Item placement files</li>
                    <li><b>DAT Files:</b> Data files and configurations</li>
                    <li><b>⚙️ CFG Files:</b> Configuration files</li>
                </ul>
                <p><b>Quick Start:</b></p>
                <ul>
                    <li>Open a file (Ctrl+O)</li>
                    <li>Create new file (Ctrl+N)</li>
                    <li>🔍 Find text (Ctrl+F)</li>
                    <li>✅ Validate syntax (F7)</li>
                </ul>
            """)
            
            self.tab_widget.addTab(welcome_widget, "Welcome")
            
    def new_file(self): #vers 1
        """Create new file"""
        editor = QTextEdit()
        editor.setFont(QFont("Consolas", 11))
        
        # Add line numbers and syntax highlighting
        self.setup_editor_features(editor)
        
        tab_index = self.tab_widget.addTab(editor, "Untitled")
        self.tab_widget.setCurrentIndex(tab_index)
        
        # Store file info
        self.current_files[tab_index] = {
            'file_path': None,
            'is_modified': False,
            'file_type': 'txt'
        }
        
        # Remove welcome tab if it exists
        if self.tab_widget.count() > 1 and self.tab_widget.tabText(0) == "Welcome":
            self.tab_widget.removeTab(0)
            
    def open_file(self, file_path=None): #vers 1
        """Open file in editor"""
        if not file_path:
            file_path, _ = QFileDialog.getOpenFileName(
                self, "Open File", "", 
                "GTA Files (*.ide *.ipl *.dat *.cfg);;Text Files (*.txt);;All Files (*.*)"
            )
            
        if file_path:
            self.open_text_file(file_path)
            
    def open_text_file(self, file_path): #vers 2
        """Open specific text file"""
        try:
            # Check if file is already open
            for tab_index, file_info in self.current_files.items():
                if file_info.get('file_path') == file_path:
                    self.tab_widget.setCurrentIndex(tab_index)
                    return
                    
            # Read file content
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                
            # Create editor
            editor = QTextEdit()
            editor.setFont(QFont("Consolas", 11))
            editor.setPlainText(content)
            
            # Setup editor features
            file_ext = os.path.splitext(file_path)[1][1:].lower()
            self.setup_editor_features(editor, file_ext)
            
            # Add tab
            file_name = os.path.basename(file_path)
            tab_index = self.tab_widget.addTab(editor, file_name)
            self.tab_widget.setCurrentIndex(tab_index)
            
            # Store file info
            self.current_files[tab_index] = {
                'file_path': file_path,
                'is_modified': False,
                'file_type': file_ext
            }
            
            # Remove welcome tab if it exists
            if self.tab_widget.count() > 1 and self.tab_widget.tabText(0) == "Welcome":
                self.tab_widget.removeTab(0)
                
            self.status_bar.showMessage(f"Opened: {file_path}", 3000)
            
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Could not open file:\n{str(e)}")
            
    def setup_editor_features(self, editor, file_type="txt"): #vers 1
        """Setup editor features like syntax highlighting"""
        # Apply syntax highlighting
        if file_type in ['ide', 'ipl', 'dat', 'cfg']:
            highlighter = GTASyntaxHighlighter(editor.document(), file_type)
            
        # Track text changes
        editor.textChanged.connect(lambda: self.on_text_changed())
        
        # Track cursor position
        editor.cursorPositionChanged.connect(self.update_cursor_position)
        
        # Enable line wrap
        editor.setLineWrapMode(QTextEdit.LineWrapMode.WidgetWidth)
        
    def on_text_changed(self): #vers 1
        """Handle text changes"""
        current_index = self.tab_widget.currentIndex()
        if current_index in self.current_files:
            self.current_files[current_index]['is_modified'] = True
            
            # Add asterisk to tab title
            current_title = self.tab_widget.tabText(current_index)
            if not current_title.endswith('*'):
                self.tab_widget.setTabText(current_index, current_title + '*')
                
    def update_cursor_position(self): #vers 1
        """Update cursor position in status bar"""
        current_editor = self.get_current_editor()
        if current_editor:
            cursor = current_editor.textCursor()
            line = cursor.blockNumber() + 1
            column = cursor.columnNumber() + 1
            self.line_col_label.setText(f"Line: {line}, Column: {column}")
            
    def get_current_editor(self): #vers 1
        """Get current text editor"""
        current_widget = self.tab_widget.currentWidget()
        if isinstance(current_widget, QTextEdit):
            return current_widget
        return None
        
    def save_current_file(self): #vers 1
        """Save current file"""
        current_index = self.tab_widget.currentIndex()
        if current_index not in self.current_files:
            return
            
        file_info = self.current_files[current_index]
        file_path = file_info.get('file_path')
        
        if not file_path:
            self.save_as()
            return
            
        editor = self.get_current_editor()
        if editor:
            try:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(editor.toPlainText())
                    
                # Update file info
                self.current_files[current_index]['is_modified'] = False
                
                # Remove asterisk from tab title
                current_title = self.tab_widget.tabText(current_index)
                if current_title.endswith('*'):
                    self.tab_widget.setTabText(current_index, current_title[:-1])
                    
                self.status_bar.showMessage(f"Saved: {file_path}", 3000)
                self.file_saved.emit(file_path)
                
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Could not save file:\n{str(e)}")
                
    def save_as(self): #vers 1
        """Save file as"""
        file_path, _ = QFileDialog.getSaveFileName(
            self, "Save As", "",
            "GTA Files (*.ide *.ipl *.dat *.cfg);;Text Files (*.txt);;All Files (*.*)"
        )
        
        if file_path:
            current_index = self.tab_widget.currentIndex()
            self.current_files[current_index]['file_path'] = file_path
            
            # Update tab title
            file_name = os.path.basename(file_path)
            self.tab_widget.setTabText(current_index, file_name)
            
            self.save_current_file()
            
    def close_tab(self, index): #vers 1
        """Close tab with save prompt if modified"""
        if index in self.current_files:
            file_info = self.current_files[index]
            
            if file_info.get('is_modified', False):
                file_name = self.tab_widget.tabText(index)
                reply = QMessageBox.question(
                    self, "Save Changes",
                    f"Save changes to {file_name}?",
                    QMessageBox.StandardButton.Save | 
                    QMessageBox.StandardButton.Discard | 
                    QMessageBox.StandardButton.Cancel,
                    QMessageBox.StandardButton.Save
                )
                
                if reply == QMessageBox.StandardButton.Save:
                    self.tab_widget.setCurrentIndex(index)
                    self.save_current_file()
                elif reply == QMessageBox.StandardButton.Cancel:
                    return
                    
            # Emit signal if file was open
            if file_info.get('file_path'):
                self.file_closed.emit(file_info['file_path'])
                
            del self.current_files[index]
            
        self.tab_widget.removeTab(index)
        
        # Show welcome tab if no files open
        if self.tab_widget.count() == 0:
            self.show_welcome_tab()
            
    def close_current_tab(self): #vers 1
        """Close current tab"""
        current_index = self.tab_widget.currentIndex()
        if current_index >= 0:
            self.close_tab(current_index)
            
    def on_tab_changed(self, index): #vers 1
        """Handle tab change"""
        if index in self.current_files:
            file_info = self.current_files[index]
            file_type = file_info.get('file_type', 'txt')
            
            type_names = {
                'ide': 'IDE File',
                'ipl': 'IPL File', 
                'dat': 'DAT File',
                'cfg': 'CFG File',
                'txt': 'Text File'
            }
            
            self.file_type_label.setText(type_names.get(file_type, 'Unknown'))
            
    # Editor operations
    def undo(self): #vers 1
        """Undo text change"""
        editor = self.get_current_editor()
        if editor:
            editor.undo()
            
    def redo(self): #vers 1
        """Redo text change"""
        editor = self.get_current_editor()
        if editor:
            editor.redo()
            
    def zoom_in(self): #vers 1
        """Increase font size"""
        editor = self.get_current_editor()
        if editor:
            font = editor.font()
            font.setPointSize(font.pointSize() + 1)
            editor.setFont(font)
            
    def zoom_out(self): #vers 1
        """Decrease font size"""
        editor = self.get_current_editor()
        if editor:
            font = editor.font()
            if font.pointSize() > 8:
                font.setPointSize(font.pointSize() - 1)
                editor.setFont(font)
                
    def show_find_dialog(self): #vers 1
        """Show find and replace dialog"""
        dialog = FindReplaceDialog(self)
        dialog.exec()
        
    def show_goto_dialog(self): #vers 1
        """Show go to line dialog"""
        editor = self.get_current_editor()
        if editor:
            line_count = editor.document().blockCount()
            dialog = GotoLineDialog(self, line_count)
            if dialog.exec() == QDialog.DialogCode.Accepted:
                line_number = dialog.get_line_number()
                cursor = editor.textCursor()
                cursor.movePosition(QTextCursor.MoveOperation.Start)
                cursor.movePosition(QTextCursor.MoveOperation.Down, 
                                  QTextCursor.MoveMode.MoveAnchor, line_number - 1)
                editor.setTextCursor(cursor)
                editor.ensureCursorVisible()
                
    def validate_syntax(self): #vers 1
        """Validate GTA file syntax"""
        current_index = self.tab_widget.currentIndex()
        if current_index not in self.current_files:
            return
            
        file_info = self.current_files[current_index]
        file_type = file_info.get('file_type', 'txt')
        
        if file_type not in ['ide', 'ipl', 'dat', 'cfg']:
            QMessageBox.information(self, "Validation", "Syntax validation is only available for GTA files (.ide, .ipl, .dat, .cfg)")
            return
            
        editor = self.get_current_editor()
        if editor:
            content = editor.toPlainText()
            errors = self.validate_gta_syntax(content, file_type)
            
            if errors:
                error_text = "\n".join([f"Line {line}: {error}" for line, error in errors[:10]])
                if len(errors) > 10:
                    error_text += f"\n... and {len(errors) - 10} more errors"
                    
                QMessageBox.warning(self, "Syntax Errors", f"Found {len(errors)} syntax errors:\n\n{error_text}")
            else:
                QMessageBox.information(self, "Validation", "✅ No syntax errors found!")
                
    def validate_gta_syntax(self, content: str, file_type: str) -> List[Tuple[int, str]]: #vers 1
        """Validate GTA file syntax and return list of errors"""
        errors = []
        lines = content.split('\n')
        
        for line_num, line in enumerate(lines, 1):
            line = line.strip()
            if not line or line.startswith('#') or line.startswith('//'):
                continue
                
            # Basic validation based on file type
            if file_type == 'ide':
                errors.extend(self.validate_ide_line(line, line_num))
            elif file_type == 'ipl':
                errors.extend(self.validate_ipl_line(line, line_num))
            elif file_type == 'dat':
                errors.extend(self.validate_dat_line(line, line_num))
                
        return errors
        
    def validate_ide_line(self, line: str, line_num: int) -> List[Tuple[int, str]]: #vers 1
        """Validate IDE file line"""
        errors = []
        parts = line.split(',')
        
        if len(parts) < 3:
            errors.append((line_num, "IDE lines should have at least 3 comma-separated values"))
            return errors
            
        # Check if first value is numeric (object ID)
        try:
            int(parts[0].strip())
        except ValueError:
            errors.append((line_num, "First value should be a numeric object ID"))
            
        # Check for .dff and .txd references
        if len(parts) >= 2:
            model_name = parts[1].strip()
            if model_name and not model_name.endswith('.dff'):
                errors.append((line_num, "Model name should end with .dff"))
                
        if len(parts) >= 3:
            texture_name = parts[2].strip()
            if texture_name and not texture_name.endswith('.txd'):
                errors.append((line_num, "Texture name should end with .txd"))
                
        return errors
        
    def validate_ipl_line(self, line: str, line_num: int) -> List[Tuple[int, str]]: #vers 1
        """Validate IPL file line"""
        errors = []
        parts = line.split(',')
        
        if len(parts) < 7:
            errors.append((line_num, "IPL lines should have at least 7 comma-separated values"))
            return errors
            
        # Check numeric values (ID, position, rotation)
        numeric_indices = [0, 2, 3, 4, 5, 6, 7]  # ID, X, Y, Z, RX, RY, RZ
        
        for i in numeric_indices:
            if i < len(parts):
                try:
                    float(parts[i].strip())
                except ValueError:
                    errors.append((line_num, f"Value at position {i+1} should be numeric"))
                    
        return errors
        
    def validate_dat_line(self, line: str, line_num: int) -> List[Tuple[int, str]]: #vers 1
        """Validate DAT file line"""
        errors = []
        
        # Check for valid file paths
        if '\\' in line or '/' in line:
            # Looks like a file path
            if not any(line.lower().endswith(ext) for ext in ['.img', '.ide', '.ipl', '.dat', '.dff', '.txd', '.col']):
                errors.append((line_num, "File path should reference a valid GTA file type"))
                
        return errors
        
    def format_document(self): #vers 1
        """Format current document"""
        current_index = self.tab_widget.currentIndex()
        if current_index not in self.current_files:
            return
            
        file_info = self.current_files[current_index]
        file_type = file_info.get('file_type', 'txt')
        
        editor = self.get_current_editor()
        if editor:
            content = editor.toPlainText()
            formatted_content = self.format_gta_file(content, file_type)
            
            if formatted_content != content:
                editor.setPlainText(formatted_content)
                self.status_bar.showMessage("Document formatted", 2000)
            else:
                self.status_bar.showMessage("Document already properly formatted", 2000)
                
    def format_gta_file(self, content: str, file_type: str) -> str: #vers 1
        """Format GTA file content"""
        lines = content.split('\n')
        formatted_lines = []
        
        for line in lines:
            stripped = line.strip()
            
            if not stripped or stripped.startswith('#') or stripped.startswith('//'):
                # Keep comments and empty lines as-is
                formatted_lines.append(stripped)
                continue
                
            if file_type in ['ide', 'ipl']:
                # Format comma-separated values
                parts = [part.strip() for part in stripped.split(',')]
                formatted_line = ', '.join(parts)
                formatted_lines.append(formatted_line)
            else:
                # Just strip whitespace for other file types
                formatted_lines.append(stripped)
                
        return '\n'.join(formatted_lines)
        
    def load_settings(self): #vers 1
        """Load editor settings"""
        # Window geometry
        geometry = self.settings.value("geometry")
        if geometry:
            self.restoreGeometry(geometry)
            
        # Font settings
        font_family = self.settings.value("font_family", "Consolas")
        font_size = self.settings.value("font_size", 11, type=int)
        
        # Apply to current editors
        for i in range(self.tab_widget.count()):
            widget = self.tab_widget.widget(i)
            if isinstance(widget, QTextEdit):
                widget.setFont(QFont(font_family, font_size))
                
    def save_settings(self): #vers 1
        """Save editor settings"""
        self.settings.setValue("geometry", self.saveGeometry())
        
        # Save font settings from current editor
        editor = self.get_current_editor()
        if editor:
            font = editor.font()
            self.settings.setValue("font_family", font.family())
            self.settings.setValue("font_size", font.pointSize())
            
    def closeEvent(self, event): #vers 1
        """Handle window close event"""
        # Check for unsaved files
        unsaved_files = []
        for tab_index, file_info in self.current_files.items():
            if file_info.get('is_modified', False):
                file_name = self.tab_widget.tabText(tab_index)
                unsaved_files.append(file_name)
                
        if unsaved_files:
            reply = QMessageBox.question(
                self, "Unsaved Changes",
                f"Save changes to {len(unsaved_files)} file(s)?\n\n" + "\n".join(unsaved_files),
                QMessageBox.StandardButton.Save | 
                QMessageBox.StandardButton.Discard | 
                QMessageBox.StandardButton.Cancel,
                QMessageBox.StandardButton.Save
            )
            
            if reply == QMessageBox.StandardButton.Save:
                # Save all modified files
                for tab_index, file_info in self.current_files.items():
                    if file_info.get('is_modified', False):
                        self.tab_widget.setCurrentIndex(tab_index)
                        self.save_current_file()
            elif reply == QMessageBox.StandardButton.Cancel:
                event.ignore()
                return
                
        self.save_settings()
        event.accept()


# Integration functions for IMG Factory
def create_gta_text_editor(parent_window=None): #vers 1
    """Create and return GTA text editor instance"""
    editor = IMGFactoryTextEditor(parent_window)
    return editor


def open_text_file_in_editor(file_path: str, parent_window=None): #vers 1
    """Open text file in IMG Factory text editor"""
    try:
        # Check if editor window already exists
        if hasattr(parent_window, '_text_editor') and parent_window._text_editor:
            editor = parent_window._text_editor
            if not editor.isVisible():
                editor.show()
            editor.raise_()
            editor.activateWindow()
        else:
            # Create new editor window
            editor = create_gta_text_editor(parent_window)
            if parent_window:
                parent_window._text_editor = editor
            editor.show()
            
        # Open the file
        editor.open_text_file(file_path)
        
        return editor
        
    except Exception as e:
        if parent_window and hasattr(parent_window, 'log_message'):
            parent_window.log_message(f"❌ Error opening text editor: {str(e)}")
        else:
            print(f"Error opening text editor: {str(e)}")
        return None


def integrate_text_editor_with_browser(browser_widget): #vers 1
    """Integrate text editor with file browser"""
    try:
        # Replace the edit_file method in browser
        original_edit_file = browser_widget.edit_file
        
        def enhanced_edit_file(file_path):
            # Use IMG Factory text editor for supported files
            file_ext = os.path.splitext(file_path)[1].lower()
            if file_ext in ['.ide', '.ipl', '.dat', '.cfg', '.txt']:
                parent_window = browser_widget.get_main_window()
                open_text_file_in_editor(file_path, parent_window)
            else:
                # Fall back to original method for other files
                original_edit_file(file_path)
                
        browser_widget.edit_file = enhanced_edit_file
        
        # Update context menu text editor action
        def enhanced_open_in_notepad(file_path):
            parent_window = browser_widget.get_main_window()
            open_text_file_in_editor(file_path, parent_window)
            
        browser_widget.open_in_notepad = enhanced_open_in_notepad
        
        return True
        
    except Exception as e:
        if hasattr(browser_widget, 'log_message'):
            browser_widget.log_message(f"❌ Error integrating text editor: {str(e)}")
        return False


__all__ = [
    'IMGFactoryTextEditor',
    'GTASyntaxHighlighter',
    'FindReplaceDialog',
    'GotoLineDialog',
    'create_gta_text_editor',
    'open_text_file_in_editor',
    'integrate_text_editor_with_browser'
]