#this belongs in apps/components/Master_Ide/img_col_reorder_dialog.py - Version: 1
# X-Seti - September 12 2026 - IMG Factory 1.6 - IMG/COL Reorder Dialog

"""img_col_reorder_dialog.py - real UI for img_col_reorder.py's own
physical archive reorder (Sep 12 2026, the final step of Keith's
own original 7-step Master IDE plan: "IMG/COL physical reorder —
rebuild those archives to match the new order once the ID/IDE side
is solid"). Preview is always required before Apply is enabled -
this rewrites real binary game archives, not IDE text, so the
confirmation step says so plainly rather than treating it like
every other lighter-weight operation in this app."""

##Methods list -
# IMGColReorderDialog

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QListWidget, QPushButton,
    QFileDialog, QMessageBox,
)


class IMGColReorderDialog(QDialog): #vers 1
    def __init__(self, parent, master_ide_result): #vers 1
        super().__init__(parent)
        self.result = master_ide_result
        self.pending_files = []   # (path, 'img'|'col')
        self.preview_done = False
        self.setWindowTitle("IMG / COL Physical Reorder")
        self.resize(560, 480)
        self._build_ui()

    def _build_ui(self): #vers 1
        lay = QVBoxLayout(self)

        warn = QLabel(
            "This physically rebuilds real IMG/COL archive files - not IDE text. "
            "Each file is backed up first, but this is a real binary rewrite. "
            "Preview before Apply.")
        warn.setWordWrap(True)
        warn.setStyleSheet("font-weight: bold;")
        lay.addWidget(warn)

        add_row = QHBoxLayout()
        add_img_btn = QPushButton("Add IMG File(s)...")
        add_img_btn.clicked.connect(self._on_add_img)
        add_row.addWidget(add_img_btn)
        add_col_btn = QPushButton("Add COL File(s)...")
        add_col_btn.clicked.connect(self._on_add_col)
        add_row.addWidget(add_col_btn)
        remove_btn = QPushButton("Remove Selected")
        remove_btn.clicked.connect(self._on_remove_selected)
        add_row.addWidget(remove_btn)
        add_row.addStretch()
        lay.addLayout(add_row)

        self.file_list = QListWidget()
        self.file_list.setSelectionMode(QListWidget.SelectionMode.ExtendedSelection)
        lay.addWidget(self.file_list)

        self.summary_label = QLabel("Add file(s), then Preview.")
        self.summary_label.setWordWrap(True)
        lay.addWidget(self.summary_label)

        self.unmatched_list = QListWidget()
        lay.addWidget(self.unmatched_list)

        btn_row = QHBoxLayout()
        preview_btn = QPushButton("Preview")
        preview_btn.clicked.connect(self._on_preview)
        btn_row.addWidget(preview_btn)
        self.apply_btn = QPushButton("Apply (rewrites real files)")
        self.apply_btn.setEnabled(False)
        self.apply_btn.clicked.connect(self._on_apply)
        btn_row.addWidget(self.apply_btn)
        btn_row.addStretch()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.reject)
        btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)

    def _refresh_file_list(self): #vers 1
        self.file_list.clear()
        for path, kind in self.pending_files:
            self.file_list.addItem(f"[{kind.upper()}] {os.path.basename(path)}")
        self.preview_done = False
        self.apply_btn.setEnabled(False)

    def _on_add_img(self): #vers 1
        paths, _ = QFileDialog.getOpenFileNames(self, "Select IMG file(s)", "", "IMG Files (*.img)")
        for p in paths:
            if (p, 'img') not in self.pending_files:
                self.pending_files.append((p, 'img'))
        self._refresh_file_list()

    def _on_add_col(self): #vers 1
        paths, _ = QFileDialog.getOpenFileNames(self, "Select COL file(s)", "", "COL Files (*.col)")
        for p in paths:
            if (p, 'col') not in self.pending_files:
                self.pending_files.append((p, 'col'))
        self._refresh_file_list()

    def _on_remove_selected(self): #vers 1
        rows = sorted({self.file_list.row(i) for i in self.file_list.selectedItems()}, reverse=True)
        for r in rows:
            del self.pending_files[r]
        self._refresh_file_list()

    def _on_preview(self): #vers 1
        from apps.methods.img_col_reorder import (
            build_id_by_name, plan_img_reorder, plan_col_reorder)
        from apps.methods.img_core_classes import IMGFile
        from apps.methods.col_core_classes import COLFile

        if not self.pending_files:
            return
        id_by_name = build_id_by_name(self.result)
        self.unmatched_list.clear()
        total_moved, total_unmatched, errors = 0, 0, []

        for path, kind in self.pending_files:
            try:
                if kind == 'img':
                    img_file = IMGFile(path)
                    if not img_file.open():
                        errors.append(f"{os.path.basename(path)}: could not open")
                        continue
                    new_entries, unmatched = plan_img_reorder(img_file.entries, id_by_name)
                    moved = sum(1 for a, b in zip(img_file.entries, new_entries) if a is not b)
                else:
                    col_file = COLFile()
                    if not col_file.load_from_file(path):
                        errors.append(f"{os.path.basename(path)}: could not open")
                        continue
                    new_models, unmatched = plan_col_reorder(col_file.models, id_by_name)
                    moved = sum(1 for a, b in zip(col_file.models, new_models) if a is not b)
                total_moved += moved
                total_unmatched += len(unmatched)
                for name in unmatched:
                    self.unmatched_list.addItem(f"{os.path.basename(path)}: {name} (no real declared ID)")
            except Exception as e:
                errors.append(f"{os.path.basename(path)}: {e}")

        parts = [f"{total_moved} real entrie(s)/model(s) would move", f"{total_unmatched} unmatched name(s)"]
        if errors:
            parts.append(f"{len(errors)} file(s) failed to open: {'; '.join(errors)}")
        self.summary_label.setText(", ".join(parts) + ".")
        self.preview_done = not errors
        self.apply_btn.setEnabled(self.preview_done and bool(self.pending_files))

    def _on_apply(self): #vers 1
        if not self.preview_done:
            return
        reply = QMessageBox.warning(
            self, "Apply IMG/COL Reorder",
            f"This rewrites {len(self.pending_files)} real binary archive file(s). "
            f"Each is backed up first, but this is irreversible without that backup. "
            f"Continue?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return

        from apps.methods.img_col_reorder import apply_img_reorder, apply_col_reorder
        results = []
        for path, kind in self.pending_files:
            fn = apply_img_reorder if kind == 'img' else apply_col_reorder
            success, unmatched = fn(path, self.result)
            results.append((os.path.basename(path), success, len(unmatched)))

        lines = []
        for name, success, unmatched_count in results:
            status = "OK" if success else "FAILED"
            lines.append(f"{name}: {status} ({unmatched_count} unmatched name(s))")
        QMessageBox.information(self, "IMG/COL Reorder Complete", "\n".join(lines))
        self.accept()
