#this belongs in gui/col_workshop_dialogs.py - Version: 3
# X-Seti - December18 2025 - Col Workshop - Loading Dialogs
"""
COL Workshop Dialogs - Progress and error dialogs for loading
"""

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, 
    QPushButton, QProgressBar, QTextEdit, QListWidget,
    QDockWidget, QToolButton, QWidget
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont, QColor

##Classes -
# COLLoadErrorDialog
# COLLoadProgressDialog
# DetachedPartsDock (__init__, _apply_on_top, _build_title_bar, _refresh, _reattach,
#                    _reattach_all, _selected, _set_on_top)
##Functions -
# show_detached_parts_dock

class COLLoadProgressDialog(QDialog): #vers 1
    """Progress dialog for COL file loading"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Loading COL File")
        self.setModal(True)
        self.setMinimumWidth(500)
        self.setMinimumHeight(400)
        
        layout = QVBoxLayout(self)
        
        # File label
        self.file_label = QLabel("Loading...")
        self.file_label.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        layout.addWidget(self.file_label)
        
        # Status label
        self.status_label = QLabel("Initializing...")
        layout.addWidget(self.status_label)
        
        # Progress bar
        self.progress_bar = QProgressBar()
        self.progress_bar.setMinimum(0)
        self.progress_bar.setMaximum(100)
        layout.addWidget(self.progress_bar)
        
        # Model count label
        self.model_label = QLabel("Models: 0")
        layout.addWidget(self.model_label)
        
        # Loaded models list
        list_label = QLabel("Loaded Models:")
        list_label.setFont(QFont("Arial", 9, QFont.Weight.Bold))
        layout.addWidget(list_label)
        
        self.model_list = QListWidget()
        self.model_list.setMaximumHeight(150)
        layout.addWidget(self.model_list)
        
        # Warnings section
        warn_label = QLabel("Warnings:")
        warn_label.setFont(QFont("Arial", 9, QFont.Weight.Bold))
        layout.addWidget(warn_label)
        
        self.warnings_text = QTextEdit()
        self.warnings_text.setReadOnly(True)
        self.warnings_text.setMaximumHeight(80)
        self.warnings_text.setStyleSheet("QTextEdit { color: palette(windowText); }")
        layout.addWidget(self.warnings_text)
        
        # Cancel button
        btn_layout = QHBoxLayout()
        btn_layout.addStretch()
        self.cancel_btn = QPushButton("Cancel")
        self.cancel_btn.clicked.connect(self.reject)
        btn_layout.addWidget(self.cancel_btn)
        layout.addLayout(btn_layout)
    
    def set_file_name(self, name: str): #vers 1
        """Set file name"""
        self.file_label.setText(f"Loading: {name}")
    
    def set_status(self, status: str): #vers 1
        """Set status message"""
        self.status_label.setText(status)
    
    def set_progress(self, value: int): #vers 1
        """Set progress value (0-100)"""
        self.progress_bar.setValue(value)
    
    def set_model_count(self, count: int): #vers 1
        """Set model count"""
        self.model_label.setText(f"Models loaded: {count}")
    
    def add_model(self, name: str): #vers 1
        """Add model to loaded list"""
        self.model_list.addItem(name)
        # Auto-scroll to bottom
        self.model_list.scrollToBottom()
    
    def add_warning(self, warning: str): #vers 1
        """Add warning message"""
        current = self.warnings_text.toPlainText()
        if current:
            self.warnings_text.setPlainText(current + "\n" + warning)
        else:
            self.warnings_text.setPlainText(warning)
        # Auto-scroll to bottom
        cursor = self.warnings_text.textCursor()
        cursor.movePosition(cursor.MoveOperation.End)
        self.warnings_text.setTextCursor(cursor)


class COLLoadErrorDialog(QDialog): #vers 1
    """Error dialog for corrupt COL models"""
    
    IGNORE = 1
    IGNORE_ALL = 2
    SKIP = 3
    SKIP_ALL = 4
    ABORT = 5
    
    def __init__(self, model_name: str, error_msg: str, remaining: int, parent=None):
        super().__init__(parent)
        self.setWindowTitle("COL Loading Error")
        self.setModal(True)
        self.setMinimumWidth(500)
        self.result_action = self.ABORT
        
        layout = QVBoxLayout(self)
        
        # Error message
        error_label = QLabel("Error loading COL model:")
        error_label.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        layout.addWidget(error_label)
        
        # Model name
        name_label = QLabel(f"Model: {model_name}")
        layout.addWidget(name_label)
        
        # Error details
        details = QTextEdit()
        details.setPlainText(error_msg)
        details.setReadOnly(True)
        details.setMaximumHeight(100)
        layout.addWidget(details)
        
        # Remaining count
        remaining_label = QLabel(f"Remaining models: {remaining}")
        layout.addWidget(remaining_label)
        
        # Buttons
        btn_layout = QHBoxLayout()
        
        ignore_btn = QPushButton("Ignore")
        ignore_btn.setToolTip("Log error but try to continue parsing this model")
        ignore_btn.clicked.connect(lambda: self.finish(self.IGNORE))
        btn_layout.addWidget(ignore_btn)
        
        ignore_all_btn = QPushButton("Ignore All")
        ignore_all_btn.setToolTip("Continue parsing all models, ignore all errors")
        ignore_all_btn.clicked.connect(lambda: self.finish(self.IGNORE_ALL))
        btn_layout.addWidget(ignore_all_btn)
        
        skip_btn = QPushButton("Skip")
        skip_btn.setToolTip("Skip this model and continue")
        skip_btn.clicked.connect(lambda: self.finish(self.SKIP))
        btn_layout.addWidget(skip_btn)
        
        skip_all_btn = QPushButton("Skip All")
        skip_all_btn.setToolTip("Skip all corrupted models without asking")
        skip_all_btn.clicked.connect(lambda: self.finish(self.SKIP_ALL))
        btn_layout.addWidget(skip_all_btn)
        
        abort_btn = QPushButton("Abort")
        abort_btn.setToolTip("Stop loading and return")
        abort_btn.clicked.connect(lambda: self.finish(self.ABORT))
        btn_layout.addWidget(abort_btn)
        
        layout.addLayout(btn_layout)
    
    def finish(self, action: int): #vers 1
        """Set result and close"""
        self.result_action = action
        self.accept()
    
    def get_action(self) -> int: #vers 1
        """Get user's choice"""
        return self.result_action


__all__ = ['COLLoadProgressDialog', 'COLLoadErrorDialog']


class DetachedPartsDock(QDockWidget): #vers 1
    """Detached parts dock (COL / Model Workshop): click selects, Reattach joins back.
    parts_fn() -> [(label, face_ids)]; select_fn(face_ids); reattach_fn(face_ids)."""

    def __init__(self, parent, parts_fn, select_fn, reattach_fn, icon_color=None): #vers 1
        super().__init__("Detached Parts", parent)
        self.setObjectName("Detached Parts")
        self.setMinimumWidth(220)
        self.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                         QDockWidget.DockWidgetFeature.DockWidgetFloatable |
                         QDockWidget.DockWidgetFeature.DockWidgetClosable)
        self._parts_fn, self._select_fn, self._reattach_fn = parts_fn, select_fn, reattach_fn
        self._on_top = False
        body = QWidget()
        lay = QVBoxLayout(body)
        lay.setContentsMargins(4, 4, 4, 4)
        self._list = QListWidget()
        self._list.currentRowChanged.connect(lambda r: self._select_fn(self._parts[r][1])
                                             if 0 <= r < len(self._parts) else None)
        lay.addWidget(self._list)
        row = QHBoxLayout()
        for text, slot in (("Reattach", self._reattach), ("Reattach All", self._reattach_all),
                           ("Refresh", self._refresh)):
            b = QPushButton(text)
            b.clicked.connect(slot)
            row.addWidget(b)
        lay.addLayout(row)
        self.setWidget(body)
        self._build_title_bar(icon_color)
        self.topLevelChanged.connect(self._apply_on_top)
        self._refresh()

    def _build_title_bar(self, icon_color): #vers 1
        """Title, stay-on-top pin, float toggle, close; double-click collapses."""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory, get_pin_icon
        bar = QWidget()
        lay = QHBoxLayout(bar)
        lay.setContentsMargins(6, 2, 2, 2)
        lay.setSpacing(2)
        lbl = QLabel("Detached Parts")
        font = getattr(self.parent(), 'section_title_font', None)
        if font is not None:
            lbl.setFont(font)
        lay.addWidget(lbl)
        lay.addStretch()
        self._pin_btn = QToolButton()
        self._pin_btn.setIcon(get_pin_icon(16, icon_color))
        self._pin_btn.setCheckable(True)
        self._pin_btn.setAutoRaise(True)
        self._pin_btn.setToolTip("Stay on top (when floating)")
        self._pin_btn.toggled.connect(self._set_on_top)
        float_btn = QToolButton()
        float_btn.setIcon(SVGIconFactory.get_tearoff_icon(16, icon_color))
        float_btn.setAutoRaise(True)
        float_btn.setToolTip("Float / dock")
        float_btn.clicked.connect(lambda: self.setFloating(not self.isFloating()))
        close_btn = QToolButton()
        close_btn.setIcon(SVGIconFactory.close_icon(16, icon_color))
        close_btn.setAutoRaise(True)
        close_btn.setToolTip("Close")
        close_btn.clicked.connect(self.close)
        for b in (self._pin_btn, float_btn, close_btn):
            b.setFixedSize(20, 20)
            lay.addWidget(b)

        def _dbl_click(event):
            body = self.widget()
            if body:
                body.setVisible(not body.isVisible())
        bar.mouseDoubleClickEvent = _dbl_click
        lbl.mouseDoubleClickEvent = _dbl_click
        self.setTitleBarWidget(bar)

    def _set_on_top(self, on): #vers 1
        self._on_top = bool(on)
        self._apply_on_top(self.isFloating())

    def _apply_on_top(self, floating): #vers 1
        """Stay-on-top flag only applies to the floating window."""
        if not floating:
            return
        self.setWindowFlag(Qt.WindowType.WindowStaysOnTopHint, self._on_top)
        self.show()

    def _refresh(self): #vers 1
        self._parts = list(self._parts_fn())
        self._list.blockSignals(True)
        self._list.clear()
        for label, faces in self._parts:
            self._list.addItem(f"{label}  ({len(faces)} faces)")
        self._list.blockSignals(False)
        if not self._parts:
            self._list.addItem("No detached parts")

    def _selected(self): #vers 1
        r = self._list.currentRow()
        return self._parts[r][1] if 0 <= r < len(self._parts) else None

    def _reattach(self): #vers 1
        faces = self._selected()
        if faces:
            self._reattach_fn(faces)
            self._refresh()

    def _reattach_all(self): #vers 1
        for _label, faces in list(self._parts):
            self._reattach_fn(faces)
            self._refresh()
            if not self._parts:
                break


def show_detached_parts_dock(owner, main_window, parts_fn, select_fn, reattach_fn, icon_color=None): #vers 1
    """Create once on owner._detached_dock, dock right of main_window, refresh, show."""
    dock = getattr(owner, '_detached_dock', None)
    if dock is None:
        dock = owner._detached_dock = DetachedPartsDock(owner, parts_fn, select_fn,
                                                        reattach_fn, icon_color)
        main_window.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, dock)
    dock._refresh()
    dock.show(); dock.raise_()
    return dock
