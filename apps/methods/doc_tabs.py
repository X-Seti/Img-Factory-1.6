#this belongs in apps/methods/doc_tabs.py - Version: 1
# X-Seti - October 12 2026 - IMG Factory 1.6 - Standalone file tabs

"""
Standalone workshop file tabs: title bar drop-down to switch, open in a
new tab and close. Docked workshops use IMG Factory tabs instead.
A workshop provides _doc_state, _doc_restore, _doc_open, _doc_has_file,
_doc_reset, _doc_current_path, _doc_new and _DOC_KIND / _DOC_FILTER.
"""

##class DocTabsMixin: -
# _build_doc_tabs_btn
# _close_doc
# _doc_opened
# _fill_doc_menu
# _open_file_new_tab
# _open_new_doc
# _refresh_doc_menu
# _switch_doc

import os

from PyQt6.QtWidgets import QFileDialog, QMenu, QMessageBox, QToolButton


class DocTabsMixin: #vers 1
    """Several open files in one standalone workshop window."""

    _DOC_KIND = "file"
    _DOC_FILTER = "All Files (*)"

    def _build_doc_tabs_btn(self, layout): #vers 1
        """Title bar drop-down button (hidden until a file is open standalone)."""
        self.doc_tabs_btn = QToolButton()
        self.doc_tabs_btn.setFont(self.button_font)
        self.doc_tabs_btn.setPopupMode(QToolButton.ToolButtonPopupMode.InstantPopup)
        self.doc_tabs_btn.setMenu(QMenu(self.doc_tabs_btn))
        self.doc_tabs_btn.menu().aboutToShow.connect(self._fill_doc_menu)
        self.doc_tabs_btn.setToolTip(f"Open {self._DOC_KIND} files - switch, open in new tab, close")
        self.doc_tabs_btn.setVisible(False)
        layout.addWidget(self.doc_tabs_btn)

    def _doc_opened(self): #vers 1
        """First file opened standalone becomes tab 1."""
        if getattr(self, '_doc_idx', -1) < 0:
            self._docs, self._doc_idx = [{}], 0
        self._refresh_doc_menu()

    def _open_new_doc(self, file_path): #vers 1
        """Open a file in a new tab, keeping the current one (docked: new IMG Factory tab)."""
        if not self.standalone_mode and hasattr(self, '_doc_new_window'):
            return self._doc_new_window(file_path)
        if not self._doc_has_file():
            return self._doc_open(file_path)
        if getattr(self, '_doc_idx', -1) < 0:              # file loaded another way (IMG entry)
            self._docs, self._doc_idx = [{}], 0
        prev = self._doc_idx
        self._docs[prev] = self._doc_state()
        self._docs.append({})
        self._doc_idx = len(self._docs) - 1
        self._doc_new()
        if self._doc_open(file_path):
            self._refresh_doc_menu()
            return True
        self._docs.pop()
        self._switch_doc(prev, save_current=False)
        return False

    def _switch_doc(self, index, save_current=True): #vers 1
        """Show another open standalone file."""
        docs = getattr(self, '_docs', [])
        if not 0 <= index < len(docs):
            return
        if save_current and 0 <= self._doc_idx < len(docs):
            docs[self._doc_idx] = self._doc_state()
        self._doc_idx = index
        self._doc_restore(docs[index])
        self._refresh_doc_menu()

    def _close_doc(self): #vers 1
        """Close the current standalone tab (unsaved edits in it are lost)."""
        docs = getattr(self, '_docs', [])
        if not docs:
            return
        name = os.path.basename(self._doc_current_path() or 'file')
        if QMessageBox.question(self, "Close Tab", f"Close {name}? Unsaved changes are lost.",
                                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No) \
                != QMessageBox.StandardButton.Yes:
            return
        del docs[self._doc_idx]
        self._doc_idx = -1
        if docs:
            self._switch_doc(0, save_current=False)
            return
        self._doc_reset()
        self._refresh_doc_menu()

    def _refresh_doc_menu(self): #vers 1
        """Drop-down shown standalone with files open; text is current name."""
        btn = getattr(self, 'doc_tabs_btn', None)
        if btn is None:
            return
        docs = getattr(self, '_docs', [])
        btn.setVisible(bool(self.standalone_mode and docs))
        cur = os.path.basename(self._doc_current_path() or '')
        btn.setText(f"{cur}  ({self._doc_idx + 1}/{len(docs)})" if docs else "")

    def _fill_doc_menu(self): #vers 1
        """Build the drop-down when it opens: files (current ticked), new tab, close."""
        menu = self.doc_tabs_btn.menu()
        menu.clear()
        docs = getattr(self, '_docs', [])
        for i in range(len(docs)):
            path = self._doc_current_path() if i == self._doc_idx else docs[i].get('path')
            act = menu.addAction(os.path.basename(path or 'untitled'))
            act.setCheckable(True)
            act.setChecked(i == self._doc_idx)
            act.triggered.connect(lambda _=False, k=i: self._switch_doc(k))
        menu.addSeparator()
        menu.addAction("Open in New Tab...", self._open_file_new_tab)
        menu.addAction("Close Tab", self._close_doc)

    def _open_file_new_tab(self): #vers 1
        """Pick files and open each in a new standalone tab."""
        paths, _ = QFileDialog.getOpenFileNames(self, f"Open {self._DOC_KIND} in New Tab", "",
                                                self._DOC_FILTER)
        for path in paths:
            self._open_new_doc(path)
