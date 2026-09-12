#this belongs in apps/methods/id_shift_dialog.py - Version: 1
# X-Seti - September 12 2026 - IMG Factory 1.6 - ID Shift Dialog

"""id_shift_dialog.py - the real UI for id_reassign.py's own plan/
apply/cascade engine (Sep 12 2026, step 5 of Keith's own Master IDE
build order - "Move + ID reassignment + cascading"). Mark a block's
start/end ID and an offset, preview the plan (real conflict check
against every ID outside the block, no partial application), then
apply - writes the touched real IDE source file(s) and cascades
into any real IPL files given, all backed up first."""

##Methods list -
# IDShiftDialog

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QFormLayout, QLabel, QSpinBox,
    QPushButton, QListWidget, QFileDialog, QMessageBox, QGroupBox,
)

from apps.methods.id_reassign import plan_id_shift, apply_id_shift, cascade_ipl_files
from apps.methods.master_ide_edit import write_source_file


class IDShiftDialog(QDialog): #vers 1
    def __init__(self, parent, result): #vers 1
        super().__init__(parent)
        self.result = result
        self.plan = None
        self.setWindowTitle("Move / Reassign ID Block")
        self.resize(560, 480)
        self._build_ui()

    def _build_ui(self): #vers 1
        lay = QVBoxLayout(self)

        form = QFormLayout()
        self.start_spin = QSpinBox()
        self.start_spin.setRange(0, 999999)
        form.addRow("Start ID:", self.start_spin)
        self.end_spin = QSpinBox()
        self.end_spin.setRange(0, 999999)
        form.addRow("End ID:", self.end_spin)
        self.offset_spin = QSpinBox()
        self.offset_spin.setRange(-999999, 999999)
        form.addRow("Shift by:", self.offset_spin)
        lay.addLayout(form)

        ipl_group = QGroupBox("IPL files to cascade into (optional)")
        ipl_lay = QVBoxLayout(ipl_group)
        self.ipl_list = QListWidget()
        ipl_lay.addWidget(self.ipl_list)
        ipl_btn_row = QHBoxLayout()
        add_ipl_btn = QPushButton("Add IPL files...")
        add_ipl_btn.clicked.connect(self._on_add_ipl_files)
        ipl_btn_row.addWidget(add_ipl_btn)
        remove_ipl_btn = QPushButton("Remove Selected")
        remove_ipl_btn.clicked.connect(self._on_remove_ipl_files)
        ipl_btn_row.addWidget(remove_ipl_btn)
        ipl_btn_row.addStretch()
        ipl_lay.addLayout(ipl_btn_row)
        lay.addWidget(ipl_group)

        self.preview_label = QLabel("Set a range and offset, then Preview.")
        self.preview_label.setWordWrap(True)
        lay.addWidget(self.preview_label)

        btn_row = QHBoxLayout()
        preview_btn = QPushButton("Preview")
        preview_btn.clicked.connect(self._on_preview)
        btn_row.addWidget(preview_btn)
        self.apply_btn = QPushButton("Apply")
        self.apply_btn.setEnabled(False)
        self.apply_btn.clicked.connect(self._on_apply)
        btn_row.addWidget(self.apply_btn)
        btn_row.addStretch()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.reject)
        btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)

    def _on_add_ipl_files(self): #vers 1
        paths, _ = QFileDialog.getOpenFileNames(
            self, "Select IPL file(s)", "", "IPL Files (*.ipl);;All files (*)")
        for p in paths:
            self.ipl_list.addItem(p)

    def _on_remove_ipl_files(self): #vers 1
        for item in self.ipl_list.selectedItems():
            self.ipl_list.takeItem(self.ipl_list.row(item))

    def _ipl_paths(self): #vers 1
        return [self.ipl_list.item(i).text() for i in range(self.ipl_list.count())]

    def _on_preview(self): #vers 1
        start, end, offset = self.start_spin.value(), self.end_spin.value(), self.offset_spin.value()
        if start > end:
            self.preview_label.setText("Start ID must not be greater than End ID.")
            self.apply_btn.setEnabled(False)
            return
        if offset == 0:
            self.preview_label.setText("Shift by must not be 0.")
            self.apply_btn.setEnabled(False)
            return
        self.plan = plan_id_shift(self.result, start, end, offset)
        if not self.plan.moved:
            self.preview_label.setText(f"No real entries found in ID range {start}-{end}.")
            self.apply_btn.setEnabled(False)
            return
        if self.plan.conflicts:
            lines = [f"CONFLICTS - {len(self.plan.conflicts)} real ID(s) would collide with an "
                     f"existing entry outside the moved block. Nothing will be applied:"]
            for new_id, name, source in self.plan.conflicts[:15]:
                lines.append(f"  new ID {new_id} already used by {name} ({os.path.basename(source)})")
            if len(self.plan.conflicts) > 15:
                lines.append(f"  ...and {len(self.plan.conflicts) - 15} more")
            self.preview_label.setText("\n".join(lines))
            self.apply_btn.setEnabled(False)
        else:
            self.preview_label.setText(
                f"OK - {len(self.plan.moved)} real entrie(s) would shift by {offset:+d} "
                f"(range becomes {start + offset}-{end + offset}). No conflicts.")
            self.apply_btn.setEnabled(True)

    def _on_apply(self): #vers 1
        if not self.plan or not self.plan.ok:
            return
        reply = QMessageBox.question(
            self, "Apply ID Shift",
            f"This will back up and rewrite every real source IDE file involved, "
            f"and any IPL files listed. Continue?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return

        touched_basenames = apply_id_shift(self.result, self.plan)
        if not touched_basenames:
            QMessageBox.warning(self, "Apply Failed", "No files were touched - nothing applied.")
            return

        write_failures = []
        for basename in touched_basenames:
            source_path = next(
                (p for p in self.result.source_files if os.path.basename(p) == basename), None)
            if not source_path or not write_source_file(self.result, source_path):
                write_failures.append(basename)

        ipl_paths = self._ipl_paths()
        ipl_results = cascade_ipl_files(ipl_paths, self.plan.id_map) if ipl_paths else {}
        ipl_changed = [p for p, changed in ipl_results.items() if changed]
        ipl_unchanged = [p for p, changed in ipl_results.items() if not changed]

        summary = [f"IDE file(s) written: {', '.join(touched_basenames)}"]
        if write_failures:
            summary.append(f"FAILED to write: {', '.join(write_failures)}")
        if ipl_changed:
            summary.append(f"IPL file(s) updated: {', '.join(os.path.basename(p) for p in ipl_changed)}")
        if ipl_unchanged:
            summary.append(f"IPL file(s) with no matching IDs: "
                            f"{', '.join(os.path.basename(p) for p in ipl_unchanged)}")
        QMessageBox.information(self, "ID Shift Applied", "\n".join(summary))
        self.accept()
