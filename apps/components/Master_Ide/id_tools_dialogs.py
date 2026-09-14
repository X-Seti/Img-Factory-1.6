#this belongs in apps/components/Master_Ide/id_tools_dialogs.py - Version: 1
# X-Seti - September 12 2026 - IMG Factory 1.6 - ID Tools Dialogs

"""id_tools_dialogs.py - real UI for id_reassign.py's own Add ID/
Remove ID/Delete ID/gap-report/compact-gaps/swap-IDs/usage-lookup/
batch-rename/insert-relocate engine (Sep 12 2026, per Keith: "can we
complete those 4 things next" - wiring the already-built, already-
tested backend into real dialogs, step 1 of that 4-item list)."""

##Methods list -
# AddIDDialog
# RemoveDeleteIDDialog
# IDUtilitiesDialog
# InsertRelocateDialog

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QFormLayout, QLabel, QSpinBox,
    QPushButton, QListWidget, QMessageBox, QTabWidget, QWidget, QLineEdit,
    QComboBox,
)

from apps.methods.id_reassign import (
    plan_add_ids, apply_add_ids, plan_collapse_free_ids, apply_collapse_free_ids,
    plan_delete_and_collapse, apply_delete_and_collapse, find_free_id_gaps,
    plan_compact_all_gaps, apply_id_shift, plan_swap_ids, find_usages,
    plan_prefix_suffix_rename, apply_prefix_suffix_rename,
    plan_insert_relocation, apply_insert_relocation,
)


class AddIDDialog(QDialog): #vers 1
    """Reserve N free ID slots right after a given ID (Sep 12 2026,
    per Keith: "Add would create xN of ID's from the selected line...
    shift everything after by 1000+")."""
    def __init__(self, parent, result): #vers 1
        super().__init__(parent)
        self.result = result
        self.plan = None
        self.setWindowTitle("Add ID")
        self.resize(420, 220)
        lay = QVBoxLayout(self)

        form = QFormLayout()
        self.after_spin = QSpinBox()
        self.after_spin.setRange(0, 999999)
        form.addRow("After ID:", self.after_spin)
        self.count_spin = QSpinBox()
        self.count_spin.setRange(1, 999999)
        self.count_spin.setValue(100)
        form.addRow("Reserve count:", self.count_spin)
        lay.addLayout(form)

        self.preview_label = QLabel("Set an ID and count, then Preview.")
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

    def _on_preview(self): #vers 1
        self.plan = plan_add_ids(self.result, self.after_spin.value(), self.count_spin.value())
        if self.plan.conflicts:
            lines = [f"CONFLICTS - {len(self.plan.conflicts)} real ID(s) would collide. Nothing applied:"]
            for new_id, name, source in self.plan.conflicts[:15]:
                lines.append(f"  {new_id} already used by {name} ({os.path.basename(source)})")
            self.preview_label.setText("\n".join(lines))
            self.apply_btn.setEnabled(False)
        elif not self.plan.moved:
            self.preview_label.setText(
                f"OK - nothing exists above ID {self.after_spin.value()} to shift. "
                f"The {self.count_spin.value()} new ID(s) are already free.")
            self.apply_btn.setEnabled(False)
        else:
            self.preview_label.setText(
                f"OK - {len(self.plan.moved)} real entrie(s) shift up by "
                f"{self.count_spin.value()} to open the new space. No conflicts.")
            self.apply_btn.setEnabled(True)

    def _on_apply(self): #vers 1
        if not self.plan:
            return
        reply = QMessageBox.question(
            self, "Add ID", "This backs up and rewrites every real file involved. Continue?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
        touched = apply_add_ids(self.result, self.plan)
        if touched is None:
            QMessageBox.warning(self, "Add ID Failed", "Plan had real conflicts - nothing applied.")
            return
        if not self._write_touched(touched):
            return
        QMessageBox.information(self, "Add ID", f"Reserved {self.count_spin.value()} ID(s). "
                                                  f"File(s) written: {', '.join(touched) or '(none needed)'}")
        self.accept()

    def _write_touched(self, touched): #vers 1
        from apps.methods.master_ide_edit import write_source_file
        failures = []
        for basename in touched:
            source_path = next((p for p in self.result.source_files
                                 if os.path.basename(p) == basename), None)
            if not source_path or not write_source_file(self.result, source_path):
                failures.append(basename)
        if failures:
            QMessageBox.warning(self, "Write Failed", f"Failed to write: {', '.join(failures)}")
            return False
        return True


class RemoveDeleteIDDialog(QDialog): #vers 1
    """Collapse free ID slots (safe - refuses on any real assigned
    entry) or explicitly delete-and-collapse through them (Sep 12
    2026, per Keith: "if there are 5 free ID's and I delete 6 of
    them, it won't delete what isn't free... If the 6th is assigned
    I get a warning, then the reverse happens, until i want to
    remove lines altogether")."""
    def __init__(self, parent, result): #vers 1
        super().__init__(parent)
        self.result = result
        self.scan_plan = None
        self.setWindowTitle("Remove / Delete ID")
        self.resize(480, 320)
        lay = QVBoxLayout(self)

        form = QFormLayout()
        self.start_spin = QSpinBox()
        self.start_spin.setRange(0, 999999)
        form.addRow("Start ID:", self.start_spin)
        self.count_spin = QSpinBox()
        self.count_spin.setRange(1, 999999)
        self.count_spin.setValue(6)
        form.addRow("Count to remove:", self.count_spin)
        lay.addLayout(form)

        self.preview_label = QLabel("Set a start ID and count, then Scan.")
        self.preview_label.setWordWrap(True)
        lay.addWidget(self.preview_label)

        btn_row = QHBoxLayout()
        scan_btn = QPushButton("Scan")
        scan_btn.clicked.connect(self._on_scan)
        btn_row.addWidget(scan_btn)
        self.collapse_free_btn = QPushButton("Collapse Free Only")
        self.collapse_free_btn.setEnabled(False)
        self.collapse_free_btn.setToolTip(
            "Safe - only removes the free slots actually found, leaves any "
            "real assigned entry untouched.")
        self.collapse_free_btn.clicked.connect(self._on_collapse_free)
        btn_row.addWidget(self.collapse_free_btn)
        self.delete_through_btn = QPushButton("Delete Models && Collapse")
        self.delete_through_btn.setEnabled(False)
        self.delete_through_btn.setToolTip(
            "Destructive - actually removes any real assigned model(s) found "
            "in range, then collapses the whole range.")
        self.delete_through_btn.clicked.connect(self._on_delete_through)
        btn_row.addWidget(self.delete_through_btn)
        btn_row.addStretch()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.reject)
        btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)

    def _on_scan(self): #vers 1
        start, count = self.start_spin.value(), self.count_spin.value()
        self.scan_plan = plan_collapse_free_ids(self.result, start, count)
        if self.scan_plan.fully_free:
            self.preview_label.setText(
                f"OK - all {count} ID(s) from {start} are genuinely free. "
                f"Safe to collapse.")
            self.collapse_free_btn.setEnabled(True)
            self.delete_through_btn.setEnabled(False)
        else:
            free_n = len(self.scan_plan.free_ids_found)
            self.preview_label.setText(
                f"BLOCKED at ID {self.scan_plan.blocking_id} - {self.scan_plan.blocking_name} "
                f"({os.path.basename(self.scan_plan.blocking_source)}). Only {free_n} of {count} "
                f"were genuinely free.\n\nCollapse Free Only will remove just those {free_n}. "
                f"Delete Models && Collapse will actually delete every real assigned entry in "
                f"the full range and collapse all {count}.")
            self.collapse_free_btn.setEnabled(free_n > 0)
            self.delete_through_btn.setEnabled(True)

    def _on_collapse_free(self): #vers 1
        start = self.start_spin.value()
        plan = self.scan_plan
        if not plan.fully_free:
            # Fall back to only what was actually found free.
            plan = plan_collapse_free_ids(self.result, start, len(self.scan_plan.free_ids_found))
        reply = QMessageBox.question(
            self, "Collapse Free IDs",
            f"Collapse {len(plan.free_ids_found)} genuinely free ID(s)? A backup is made "
            f"before writing.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
        touched = apply_collapse_free_ids(self.result, plan)
        if touched is None:
            QMessageBox.warning(self, "Failed", "Plan was not fully free - nothing applied.")
            return
        if not self._write_touched(touched):
            return
        QMessageBox.information(self, "Collapsed", f"File(s) written: {', '.join(touched) or '(none needed)'}")
        self.accept()

    def _on_delete_through(self): #vers 1
        from apps.methods.id_reassign import plan_delete_and_collapse as _plan_del
        start, count = self.start_spin.value(), self.count_spin.value()
        to_delete = _plan_del(self.result, start, count)
        if to_delete:
            lines = [f"{len(to_delete)} real model(s) will be PERMANENTLY DELETED:"]
            lines += [f"  ID {mid}: {name} ({os.path.basename(src)})" for mid, name, src in to_delete]
            msg = "\n".join(lines) + "\n\nThen the whole range collapses. Continue?"
        else:
            msg = f"No real assigned entries in range - this is just a plain collapse of {count} free ID(s). Continue?"
        reply = QMessageBox.warning(
            self, "Delete Models && Collapse", msg,
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
        result = apply_delete_and_collapse(self.result, start, count, to_delete)
        if result is None:
            QMessageBox.warning(self, "Failed", "Could not apply - nothing written.")
            return
        removed_files, shift_touched = result
        all_touched = sorted(set(removed_files) | set(shift_touched))
        if not self._write_touched(all_touched):
            return
        QMessageBox.information(self, "Deleted && Collapsed",
            f"Removed {len(to_delete)} model(s). File(s) written: {', '.join(all_touched) or '(none needed)'}")
        self.accept()

    def _write_touched(self, touched): #vers 1
        from apps.methods.master_ide_edit import write_source_file
        failures = []
        for basename in touched:
            source_path = next((p for p in self.result.source_files
                                 if os.path.basename(p) == basename), None)
            if not source_path or not write_source_file(self.result, source_path):
                failures.append(basename)
        if failures:
            QMessageBox.warning(self, "Write Failed", f"Failed to write: {', '.join(failures)}")
            return False
        return True


class IDUtilitiesDialog(QDialog): #vers 1
    """Gap report, compact all gaps, swap two IDs, usage lookup, and
    batch prefix/suffix rename - the lighter-weight follow-up use
    cases (Sep 12 2026, per Keith: "can you think of any other use
    cases")."""
    def __init__(self, parent, result): #vers 1
        super().__init__(parent)
        self.result = result
        self.setWindowTitle("ID Utilities")
        self.resize(560, 480)
        lay = QVBoxLayout(self)
        self.tabs = QTabWidget()
        lay.addWidget(self.tabs)
        self.tabs.addTab(self._build_gap_tab(), "Gap Report / Compact")
        self.tabs.addTab(self._build_swap_tab(), "Swap IDs")
        self.tabs.addTab(self._build_usage_tab(), "Find Usages")
        self.tabs.addTab(self._build_rename_tab(), "Batch Rename")
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.reject)
        close_row = QHBoxLayout()
        close_row.addStretch()
        close_row.addWidget(close_btn)
        lay.addLayout(close_row)

    def _write_touched(self, touched): #vers 1
        from apps.methods.master_ide_edit import write_source_file
        failures = []
        for basename in touched:
            source_path = next((p for p in self.result.source_files
                                 if os.path.basename(p) == basename), None)
            if not source_path or not write_source_file(self.result, source_path):
                failures.append(basename)
        if failures:
            QMessageBox.warning(self, "Write Failed", f"Failed to write: {', '.join(failures)}")
            return False
        return True

    def _build_gap_tab(self): #vers 1
        w = QWidget()
        lay = QVBoxLayout(w)
        form = QFormLayout()
        self.gap_min = QSpinBox(); self.gap_min.setRange(0, 999999)
        self.gap_max = QSpinBox(); self.gap_max.setRange(0, 999999); self.gap_max.setValue(9999)
        form.addRow("From:", self.gap_min)
        form.addRow("To:", self.gap_max)
        lay.addLayout(form)
        self.gap_list = QListWidget()
        lay.addWidget(self.gap_list, 1)
        btn_row = QHBoxLayout()
        report_btn = QPushButton("Find Gaps")
        report_btn.clicked.connect(self._on_find_gaps)
        btn_row.addWidget(report_btn)
        self.compact_btn = QPushButton("Compact All Gaps in Range")
        self.compact_btn.clicked.connect(self._on_compact_all)
        btn_row.addWidget(self.compact_btn)
        btn_row.addStretch()
        lay.addLayout(btn_row)
        return w

    def _on_find_gaps(self): #vers 1
        gaps = find_free_id_gaps(self.result, self.gap_min.value(), self.gap_max.value())
        self.gap_list.clear()
        if not gaps:
            self.gap_list.addItem("No free gaps in this range.")
            return
        for lo, hi in gaps:
            size = hi - lo + 1
            self.gap_list.addItem(f"{lo}-{hi}  ({size} free)")

    def _on_compact_all(self): #vers 1
        plan = plan_compact_all_gaps(self.result, self.gap_min.value(), self.gap_max.value())
        if not plan.moved:
            QMessageBox.information(self, "Compact All Gaps", "No gaps to compact in this range.")
            return
        reply = QMessageBox.question(
            self, "Compact All Gaps",
            f"Close every gap in {self.gap_min.value()}-{self.gap_max.value()}? "
            f"{len(plan.moved)} real entrie(s) will shift down. A backup is made first.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
        touched = apply_id_shift(self.result, plan)
        if not touched and plan.moved:
            QMessageBox.warning(self, "Failed", "Could not apply - nothing written.")
            return
        if not self._write_touched(touched):
            return
        QMessageBox.information(self, "Compacted", f"File(s) written: {', '.join(touched) or '(none needed)'}")
        self._on_find_gaps()

    def _build_swap_tab(self): #vers 1
        w = QWidget()
        lay = QVBoxLayout(w)
        form = QFormLayout()
        self.swap_a = QSpinBox(); self.swap_a.setRange(0, 999999)
        self.swap_b = QSpinBox(); self.swap_b.setRange(0, 999999)
        form.addRow("ID A:", self.swap_a)
        form.addRow("ID B:", self.swap_b)
        lay.addLayout(form)
        self.swap_label = QLabel("")
        self.swap_label.setWordWrap(True)
        lay.addWidget(self.swap_label)
        lay.addStretch()
        swap_btn = QPushButton("Swap")
        swap_btn.clicked.connect(self._on_swap)
        lay.addWidget(swap_btn)
        return w

    def _on_swap(self): #vers 1
        id_a, id_b = self.swap_a.value(), self.swap_b.value()
        plan = plan_swap_ids(self.result, id_a, id_b)
        if not plan.moved:
            self.swap_label.setText(f"Both {id_a} and {id_b} must already be assigned real entries.")
            return
        reply = QMessageBox.question(
            self, "Swap IDs", f"Swap what occupies {id_a} and {id_b}?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
        touched = apply_id_shift(self.result, plan)
        if not touched:
            self.swap_label.setText("Failed - nothing written.")
            return
        if not self._write_touched(touched):
            return
        self.swap_label.setText(f"Swapped. File(s) written: {', '.join(touched)}")

    def _build_usage_tab(self): #vers 1
        w = QWidget()
        lay = QVBoxLayout(w)
        form = QFormLayout()
        self.usage_id_edit = QLineEdit()
        self.usage_id_edit.setPlaceholderText("Leave blank to search by name only")
        form.addRow("Model ID:", self.usage_id_edit)
        self.usage_name_edit = QLineEdit()
        self.usage_name_edit.setPlaceholderText("Leave blank to search by ID only")
        form.addRow("Model name:", self.usage_name_edit)
        lay.addLayout(form)
        self.usage_list = QListWidget()
        lay.addWidget(self.usage_list, 1)
        find_btn = QPushButton("Find Usages")
        find_btn.clicked.connect(self._on_find_usages)
        lay.addWidget(find_btn)
        return w

    def _on_find_usages(self): #vers 1
        id_text = self.usage_id_edit.text().strip()
        name_text = self.usage_name_edit.text().strip()
        model_id = int(id_text) if id_text.isdigit() else None
        usages = find_usages(self.result, model_id=model_id, model_name=name_text or None)
        self.usage_list.clear()
        if not usages:
            self.usage_list.addItem("No real usages found.")
            return
        for u in usages:
            self.usage_list.addItem(
                f"[{u['section']}] ID {u['model_id']}  {u['model_name']}  "
                f"txd={u['txd_name']}  ({u['source_ide']})")

    def _build_rename_tab(self): #vers 1
        w = QWidget()
        lay = QVBoxLayout(w)
        form = QFormLayout()
        self.rename_from = QSpinBox(); self.rename_from.setRange(0, 999999)
        self.rename_to = QSpinBox(); self.rename_to.setRange(0, 999999)
        form.addRow("From ID:", self.rename_from)
        form.addRow("To ID:", self.rename_to)
        self.prefix_edit = QLineEdit()
        self.suffix_edit = QLineEdit()
        form.addRow("Prefix:", self.prefix_edit)
        form.addRow("Suffix:", self.suffix_edit)
        lay.addLayout(form)
        self.rename_label = QLabel("")
        self.rename_label.setWordWrap(True)
        lay.addWidget(self.rename_label)
        self.rename_plan = None
        btn_row = QHBoxLayout()
        preview_btn = QPushButton("Preview")
        preview_btn.clicked.connect(self._on_rename_preview)
        btn_row.addWidget(preview_btn)
        self.rename_apply_btn = QPushButton("Apply")
        self.rename_apply_btn.setEnabled(False)
        self.rename_apply_btn.clicked.connect(self._on_rename_apply)
        btn_row.addWidget(self.rename_apply_btn)
        btn_row.addStretch()
        lay.addLayout(btn_row)
        return w

    def _on_rename_preview(self): #vers 1
        ids = list(range(self.rename_from.value(), self.rename_to.value() + 1))
        self.rename_plan = plan_prefix_suffix_rename(
            self.result, ids, self.prefix_edit.text(), self.suffix_edit.text())
        if not self.rename_plan.renames:
            self.rename_label.setText("No real entries in that ID range.")
            self.rename_apply_btn.setEnabled(False)
        elif self.rename_plan.name_conflicts or self.rename_plan.internal_duplicates:
            lines = [f"CONFLICTS - nothing will be applied:"]
            for name, mid, cid, csrc in self.rename_plan.name_conflicts[:10]:
                lines.append(f"  {name} (from ID {mid}) already used by ID {cid} ({os.path.basename(csrc)})")
            for name, a, b in self.rename_plan.internal_duplicates[:10]:
                lines.append(f"  ID {a} and ID {b} would both become {name}")
            self.rename_label.setText("\n".join(lines))
            self.rename_apply_btn.setEnabled(False)
        else:
            self.rename_label.setText(f"OK - {len(self.rename_plan.renames)} real entrie(s) will be renamed.")
            self.rename_apply_btn.setEnabled(True)

    def _on_rename_apply(self): #vers 1
        if not self.rename_plan:
            return
        reply = QMessageBox.question(
            self, "Batch Rename", f"Rename {len(self.rename_plan.renames)} entries? "
                                   f"A backup is made before writing each file.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
        touched = apply_prefix_suffix_rename(self.result, self.rename_plan)
        if not touched:
            QMessageBox.warning(self, "Failed", "Could not apply - nothing written.")
            return
        QMessageBox.information(self, "Renamed", f"File(s) written: {', '.join(touched)}")
        self.rename_apply_btn.setEnabled(False)


class InsertRelocateDialog(QDialog): #vers 1
    """Relocate an already-loaded file's entries (e.g. just added
    via Insert IDE File, possibly from a different game) onto a
    fresh target range, with a real name-collision check (Sep 12
    2026, per Keith: "insert ID, moves other ide files into that
    area... would need a model name check")."""
    def __init__(self, parent, result): #vers 1
        super().__init__(parent)
        self.result = result
        self.plan = None
        self.setWindowTitle("Insert & Relocate File")
        self.resize(480, 320)
        lay = QVBoxLayout(self)

        form = QFormLayout()
        self.file_combo = QComboBox()
        self.file_combo.addItems([os.path.basename(p) for p in result.source_files])
        form.addRow("Relocate file:", self.file_combo)
        self.target_spin = QSpinBox()
        self.target_spin.setRange(0, 999999)
        form.addRow("Target start ID:", self.target_spin)
        lay.addLayout(form)

        self.preview_label = QLabel("Pick a file and target ID, then Preview.")
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

    def _on_preview(self): #vers 1
        if not self.result.source_files:
            return
        source_path = self.result.source_files[self.file_combo.currentIndex()]
        self.plan = plan_insert_relocation(self.result, source_path, self.target_spin.value())
        if not self.plan.moved:
            self.preview_label.setText("That file has no real objs/tobj entries to relocate.")
            self.apply_btn.setEnabled(False)
        elif self.plan.id_conflicts or self.plan.name_conflicts:
            lines = ["CONFLICTS - nothing will be applied:"]
            for new_id, name, src in self.plan.id_conflicts[:10]:
                lines.append(f"  ID {new_id} already used by {name} ({os.path.basename(src)})")
            for name, old_id, existing_id, existing_src in self.plan.name_conflicts[:10]:
                lines.append(f"  Name '{name}' (from old ID {old_id}) already used by "
                             f"ID {existing_id} ({os.path.basename(existing_src)})")
            self.preview_label.setText("\n".join(lines))
            self.apply_btn.setEnabled(False)
        else:
            self.preview_label.setText(
                f"OK - {len(self.plan.moved)} real entrie(s) will relocate to "
                f"{self.target_spin.value()}-{self.target_spin.value() + len(self.plan.moved) - 1}. "
                f"No conflicts.")
            self.apply_btn.setEnabled(True)

    def _on_apply(self): #vers 1
        if not self.plan:
            return
        reply = QMessageBox.question(
            self, "Insert & Relocate", "This backs up and rewrites the relocated file. Continue?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
        touched = apply_insert_relocation(self.result, self.plan)
        if not touched:
            QMessageBox.warning(self, "Failed", "Could not apply - nothing written.")
            return
        from apps.methods.master_ide_edit import write_source_file
        failures = []
        for basename in touched:
            source_path = next((p for p in self.result.source_files
                                 if os.path.basename(p) == basename), None)
            if not source_path or not write_source_file(self.result, source_path):
                failures.append(basename)
        if failures:
            QMessageBox.warning(self, "Write Failed", f"Failed to write: {', '.join(failures)}")
            return
        QMessageBox.information(self, "Relocated", f"File(s) written: {', '.join(touched)}")
        self.accept()
