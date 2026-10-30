#this belongs in apps/gui/batch_convert_dialog.py - Version: 2
# X-Seti - October 13 2026 - IMG Factory 1.6 - Model batch convert dialog

"""
[Source dir] -> [Dest dir] model batch converter (any supported model to DFF / OBJ).
"""

##Methods list -
# open_batch_convert
#
##class BatchConvertDialog: -
# __init__
# _browse
# _run

import os

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (QApplication, QCheckBox, QComboBox, QDialog, QFileDialog, QFormLayout,
                             QHBoxLayout, QLineEdit, QListWidget, QProgressDialog, QPushButton,
                             QVBoxLayout, QWidget)

from apps.methods.model_convert import CONVERT_TARGETS, SOURCE_EXTS, batch_convert


class BatchConvertDialog(QDialog):
    """Convert every model in a folder to the chosen format."""

    def __init__(self, parent=None, src_dir='', dst_dir=''): #vers 2
        super().__init__(parent)
        self.setWindowTitle("Batch Convert Models")
        self.resize(640, 520)
        lay = QVBoxLayout(self)
        form = QFormLayout()
        self._src = QLineEdit(src_dir)
        self._dst = QLineEdit(dst_dir)
        for label, edit in (("Source dir:", self._src), ("Dest dir:", self._dst)):
            row = QWidget()
            rl = QHBoxLayout(row); rl.setContentsMargins(0, 0, 0, 0)
            rl.addWidget(edit, 1)
            b = QPushButton("...")
            b.setFixedWidth(32)
            b.clicked.connect(lambda _=False, e=edit: self._browse(e))
            rl.addWidget(b)
            form.addRow(label, row)
        fmt = QWidget()
        fl = QHBoxLayout(fmt); fl.setContentsMargins(0, 0, 0, 0)
        self._ext_chk = {}
        for ext in SOURCE_EXTS:
            c = QCheckBox(ext[1:].upper())
            c.setChecked(ext != '.obj')
            self._ext_chk[ext] = c
            fl.addWidget(c)
        fl.addStretch(1)
        form.addRow("From:", fmt)
        self._target = QComboBox()
        self._target.addItems(CONVERT_TARGETS)
        form.addRow("To:", self._target)
        self._recursive = QCheckBox("Include sub folders")
        self._recursive.setChecked(True)
        form.addRow("", self._recursive)
        lay.addLayout(form)
        self._log = QListWidget()
        lay.addWidget(self._log, 1)
        btns = QHBoxLayout()
        btns.addStretch(1)
        go = QPushButton("Convert")
        go.clicked.connect(self._run)
        close = QPushButton("Close")
        close.clicked.connect(self.accept)
        btns.addWidget(go); btns.addWidget(close)
        lay.addLayout(btns)
        try:
            from apps.core.theme_utils import apply_dialog_theme
            apply_dialog_theme(self, parent)
        except Exception:
            pass

    def _browse(self, edit): #vers 1
        d = QFileDialog.getExistingDirectory(self, "Select folder", edit.text())
        if d:
            edit.setText(d)

    def _run(self): #vers 1
        """Convert, listing each file's result."""
        src, dst = self._src.text().strip(), self._dst.text().strip()
        exts = [e for e, c in self._ext_chk.items() if c.isChecked()]
        if not (os.path.isdir(src) and dst and exts):
            self._log.addItem("Pick a source folder, a destination folder and at least one format.")
            return
        if os.path.abspath(src) == os.path.abspath(dst):
            self._log.addItem("Destination must differ from the source folder.")
            return
        prog = QProgressDialog("Converting...", "Cancel", 0, 100, self)
        prog.setWindowModality(Qt.WindowModality.WindowModal)

        def _cb(done, total, name):
            prog.setMaximum(max(1, total)); prog.setValue(done)
            prog.setLabelText(name)
            QApplication.processEvents()
            return not prog.wasCanceled()
        res = batch_convert(src, dst, exts, self._target.currentText(), self._recursive.isChecked(), _cb)
        prog.close()
        self._log.clear()
        for rel, ok, msg in res:
            self._log.addItem(f"{'OK  ' if ok else 'FAIL'}  {rel}" + (f"  - {msg}" if msg else ""))
        n = sum(ok for _, ok, _ in res)
        self._log.addItem(f"{n} of {len(res)} converted to {dst}")
        self._log.scrollToBottom()


def open_batch_convert(parent=None, src_dir='', dst_dir=''): #vers 1
    """Show the batch convert dialog."""
    dlg = BatchConvertDialog(parent, src_dir, dst_dir)
    dlg.exec()
