#this belongs in apps/methods/input_bindings_dialog.py - Version: 2
# X-Seti - October 28 2026 - IMG Factory 1.6 - Key and mouse bindings editor

"""
Editor for buttons/<tool>.json: shortcuts, 3D view mouse buttons, camera keys.
Used as a dialog, or as a Settings tab via add_bindings_tab.
"""

##Methods list -
# add_bindings_tab
# key_display_name
#
##class KeyCaptureButton: -
# __init__
# _start_capture
# _update_text
# binding
# keyPressEvent
#
##class InputBindingsWidget: -
# __init__
# _build_camera
# _build_keys
# _build_mouse
# _filter
# _mouse_changed
# data
# reset_defaults
# save
#
##class InputBindingsDialog: -
# __init__

import sys
from collections import Counter

from PyQt6.QtCore import Qt, pyqtSignal
from PyQt6.QtGui import QKeySequence, QShortcut
from PyQt6.QtWidgets import (QComboBox, QDialog, QDialogButtonBox, QFormLayout, QGroupBox, QHBoxLayout,
                             QHeaderView, QKeySequenceEdit, QLabel, QLineEdit, QMessageBox, QPushButton,
                             QTableWidget, QTableWidgetItem, QVBoxLayout, QWidget)

from apps.methods.input_bindings import MOUSE_DEFAULTS, MOUSE_ROLES


def key_display_name(key: int, is_numpad: bool) -> str: #vers 1
    """Qt key int + numpad flag as text ('Numpad 4')."""
    name = QKeySequence(key).toString() or f"Key {key}"
    return f"Numpad {name}" if is_numpad else name


class KeyCaptureButton(QPushButton):
    """Shows a bound key; click, then press a key to rebind (Esc cancels)."""
    key_captured = pyqtSignal(int, bool)

    def __init__(self, key: int, is_numpad: bool, parent=None): #vers 1
        super().__init__(parent)
        self._key, self._is_numpad, self._capturing = key, is_numpad, False
        self._update_text()
        self.clicked.connect(self._start_capture)

    def _update_text(self): #vers 1
        self.setText(key_display_name(self._key, self._is_numpad))

    def _start_capture(self): #vers 1
        self._capturing = True
        self.setText("Press a key…")
        self.grabKeyboard()

    def keyPressEvent(self, event): #vers 1
        if not self._capturing:
            super().keyPressEvent(event)
            return
        self._capturing = False
        self.releaseKeyboard()
        if event.key() != Qt.Key.Key_Escape:
            self._key = event.key()
            self._is_numpad = bool(event.modifiers() & Qt.KeyboardModifier.KeypadModifier)
            self.key_captured.emit(self._key, self._is_numpad)
        self._update_text()

    def binding(self) -> dict: #vers 1
        return {'key': self._key, 'numpad': self._is_numpad}


class InputBindingsWidget(QWidget):
    """Shortcuts table, mouse button roles and camera keys for one tool."""

    def __init__(self, owner, parent=None): #vers 1
        super().__init__(parent)
        self._owner = owner
        if not hasattr(owner, '_bindings_defaults'):
            owner._bindings_apply()
        self._saved = owner._bindings_load()
        lay = QVBoxLayout(self)
        lay.addWidget(QLabel(f"Saved to buttons/{owner._bindings_path().name}"))
        self._build_keys(lay)
        self._viewports = owner._bindings_viewports()
        if self._viewports:
            row = QHBoxLayout()
            self._build_mouse(row)
            self._build_camera(row)
            lay.addLayout(row)

    def _build_keys(self, lay): #vers 1
        self._search = QLineEdit()
        self._search.setPlaceholderText("Filter actions...")
        self._search.textChanged.connect(self._filter)
        lay.addWidget(self._search)
        acts = self._owner._binding_actions()
        self._table = QTableWidget(len(acts), 2)
        self._table.setHorizontalHeaderLabels(["Action", "Shortcut"])
        self._table.horizontalHeader().setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        self._table.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        self._table.verticalHeader().setVisible(False)
        self._edits = {}
        ribbon_sc = getattr(self._owner, '_bindings_ribbon_sc', {})
        for r, (bid, label, target) in enumerate(acts):
            it = QTableWidgetItem(label)
            it.setFlags(it.flags() & ~Qt.ItemFlag.ItemIsEditable)
            it.setToolTip(bid)
            self._table.setItem(r, 0, it)
            sc = target if isinstance(target, QShortcut) else ribbon_sc.get(bid)
            cur = sc.key().toString() if sc else ''
            ed = QKeySequenceEdit(QKeySequence(cur))
            ed.setClearButtonEnabled(True)
            ed.setMaximumSequenceLength(1)
            self._table.setCellWidget(r, 1, ed)
            self._edits[bid] = ed
        lay.addWidget(self._table, 1)

    def _filter(self, text): #vers 1
        t = text.lower()
        for r in range(self._table.rowCount()):
            self._table.setRowHidden(r, t not in self._table.item(r, 0).text().lower())

    def _build_mouse(self, row): #vers 1
        grp = QGroupBox("3D view mouse buttons")
        form = QFormLayout(grp)
        roles = dict(MOUSE_DEFAULTS, **self._saved.get('mouse', {}))
        self._mouse = {}
        for role, label in MOUSE_ROLES.items():
            cb = QComboBox()
            cb.addItems(["left", "middle", "right"])
            cb.setCurrentText(roles[role])
            cb.setProperty('prev', roles[role])
            cb.currentTextChanged.connect(lambda v, r=role: self._mouse_changed(r, v))
            form.addRow(label + ":", cb)
            self._mouse[role] = cb
        row.addWidget(grp)

    def _mouse_changed(self, role, value): #vers 1
        """Keep one role per button: swap with the role that had it."""
        cb = self._mouse[role]
        old = cb.property('prev')
        for r, other in self._mouse.items():
            if r != role and other.currentText() == value:
                other.blockSignals(True)
                other.setCurrentText(old)
                other.setProperty('prev', old)
                other.blockSignals(False)
        cb.setProperty('prev', value)

    def _build_camera(self, row): #vers 2
        vp_mod = sys.modules[type(self._viewports[0]).__module__]   # loaded: a 3D view exists
        DEFAULT_KEY_BINDINGS, KEY_BINDING_LABELS = vp_mod.DEFAULT_KEY_BINDINGS, vp_mod.KEY_BINDING_LABELS
        grp = QGroupBox("3D camera keys")
        form = QFormLayout(grp)
        live = getattr(self._viewports[0], '_key_bindings', None) or DEFAULT_KEY_BINDINGS
        self._camera = {}
        for action, default in DEFAULT_KEY_BINDINGS.items():
            spec = live.get(action, default)
            btn = KeyCaptureButton(spec['key'], spec['numpad'])
            form.addRow(KEY_BINDING_LABELS.get(action, action) + ":", btn)
            self._camera[action] = btn
        row.addWidget(grp)

    def reset_defaults(self): #vers 2
        """Every field back to the tool's built-in keys and buttons."""
        defaults = self._owner._bindings_defaults
        for bid, ed in self._edits.items():
            ed.setKeySequence(QKeySequence(defaults.get(bid, '')))
        for role, cb in getattr(self, '_mouse', {}).items():
            cb.blockSignals(True)
            cb.setCurrentText(MOUSE_DEFAULTS[role])
            cb.setProperty('prev', MOUSE_DEFAULTS[role])
            cb.blockSignals(False)
        if getattr(self, '_camera', None):
            DEFAULT_KEY_BINDINGS = sys.modules['apps.methods.dff_viewport'].DEFAULT_KEY_BINDINGS
            for action, btn in self._camera.items():
                btn._key, btn._is_numpad = DEFAULT_KEY_BINDINGS[action]['key'], DEFAULT_KEY_BINDINGS[action]['numpad']
                btn._update_text()

    def data(self) -> dict: #vers 1
        d = {'keys': {bid: ed.keySequence().toString() for bid, ed in self._edits.items()}}
        if getattr(self, '_mouse', None):
            d['mouse'] = {r: cb.currentText() for r, cb in self._mouse.items()}
        if getattr(self, '_camera', None):
            d['camera'] = {a: b.binding() for a, b in self._camera.items()}
        return d

    def save(self) -> bool: #vers 1
        """Write and apply; asks first when a shortcut is used twice."""
        d = self.data()
        dup = [k for k, n in Counter(v for v in d['keys'].values() if v).items() if n > 1]
        if dup and QMessageBox.question(
                self, "Key Bindings", "Shortcut used more than once: " + ", ".join(dup) +
                "\nOnly one of each will work. Save anyway?") != QMessageBox.StandardButton.Yes:
            return False
        try:
            self._owner._bindings_save(d)
        except OSError as e:
            QMessageBox.warning(self, "Key Bindings", f"Can't write buttons file:\n{e}")
            return False
        self._saved = d
        return True


class InputBindingsDialog(QDialog):
    """Key & Mouse Bindings: Save / Reset / Close."""

    def __init__(self, owner, parent=None): #vers 1
        super().__init__(parent)
        self.setWindowTitle("Key & Mouse Bindings")
        self.resize(640, 560)
        from apps.core.theme_utils import apply_dialog_theme
        apply_dialog_theme(self, getattr(owner, 'main_window', None))
        lay = QVBoxLayout(self)
        self.editor = InputBindingsWidget(owner, self)
        lay.addWidget(self.editor, 1)
        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Save |
                                QDialogButtonBox.StandardButton.RestoreDefaults |
                                QDialogButtonBox.StandardButton.Close)
        btns.button(QDialogButtonBox.StandardButton.Save).clicked.connect(
            lambda: self.editor.save() and self.accept())
        btns.button(QDialogButtonBox.StandardButton.RestoreDefaults).clicked.connect(self.editor.reset_defaults)
        btns.rejected.connect(self.reject)
        lay.addWidget(btns)


def add_bindings_tab(owner, tabs, title="Keys && Mouse"): #vers 1
    """Add the editor as a Settings tab; returns the save callable for OK."""
    w = QWidget()
    lay = QVBoxLayout(w)
    editor = InputBindingsWidget(owner, w)
    lay.addWidget(editor, 1)
    reset = QPushButton("Restore Defaults")
    reset.clicked.connect(editor.reset_defaults)
    lay.addWidget(reset, 0, Qt.AlignmentFlag.AlignRight)
    tabs.addTab(w, title)
    return editor.save
