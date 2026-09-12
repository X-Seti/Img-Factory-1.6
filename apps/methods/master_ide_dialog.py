#this belongs in apps/methods/master_ide_dialog.py - Version: 9

##Methods list -
# MasterIDEDialog
# _InsertTextDialog
# _AddEntryDialog
# show_master_ide
# show_master_ide_from_dat

"""master_ide_dialog.py - the real UI for master_ide.py's own step-1 merge (Sep 5 2026)"""

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QTableWidget, QTableWidgetItem,
    QPushButton, QFileDialog, QMessageBox, QWidget, QListWidget,
)
from PyQt6.QtCore import Qt

from apps.methods.master_ide import (
    load_master_ide, write_master_ide, collect_ide_paths_from_dat)
from apps.methods.master_ide_edit import rename_entry, add_entry, remove_entry, write_source_file
from apps.methods.file_backup import backup_file


class MasterIDEDialog(QDialog): #vers 8
    def __init__(self, parent, result, source_paths, game=None): #vers 2
        super().__init__(parent)
        self.result = result
        self.source_paths = source_paths
        self.game = game
        self.setWindowTitle("Master IDE")
        self.resize(820, 560)
        self._build_ui()
        self._refresh_top()
        self._populate()

    def _build_ui(self): #vers 2
        self._lay = QVBoxLayout(self)
        lay = self._lay

        self._top = QHBoxLayout()
        lay.addLayout(self._top)
        self._warn_lbl = None
        self._error_lbls = []

        self.table = QTableWidget()
        self.table.setColumnCount(4)
        self.table.setHorizontalHeaderLabels(["ID", "Model", "TXD", "Source IDE"])
        self.table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self.table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self.table.horizontalHeader().setStretchLastSection(True)
        from PyQt6.QtWidgets import QMenu
        self.table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.table.customContextMenuRequested.connect(self._on_table_context_menu)
        lay.addWidget(self.table, 1)

        btn_row = QHBoxLayout()
        load_dat_btn = QPushButton("Load from .dat...")
        load_dat_btn.clicked.connect(self._on_load_from_dat)
        btn_row.addWidget(load_dat_btn)
        insert_file_btn = QPushButton("Insert IDE File...")
        insert_file_btn.clicked.connect(self._on_insert_ide_file)
        btn_row.addWidget(insert_file_btn)
        insert_text_btn = QPushButton("Insert Text...")
        insert_text_btn.clicked.connect(self._on_insert_text)
        btn_row.addWidget(insert_text_btn)
        remove_file_btn = QPushButton("Remove File...")
        remove_file_btn.clicked.connect(self._on_remove_file)
        btn_row.addWidget(remove_file_btn)
        add_entry_btn = QPushButton("Add Entry...")
        add_entry_btn.clicked.connect(self._on_add_entry)
        btn_row.addWidget(add_entry_btn)
        id_shift_btn = QPushButton("Move / Reassign ID Block...")
        id_shift_btn.clicked.connect(self._on_id_shift)
        btn_row.addWidget(id_shift_btn)
        save_btn = QPushButton("Save as Master IDE...")
        save_btn.clicked.connect(self._on_save)
        btn_row.addWidget(save_btn)
        btn_row.addStretch()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)

    def _refresh_top(self): #vers 2
        """Rebuild the header info/warning/error labels - called on\n        init and again after a Load from .dat swaps self.result."""
        while self._top.count():
            item = self._top.takeAt(0)
            if item.widget():
                item.widget().deleteLater()
        names = ", ".join(os.path.basename(p) for p in self.result.source_files)
        visible_count = sum(len(v) for section, v in self.result.objects_by_section.items()
                             if section not in self.result.raw_section_lines)
        self._top.addWidget(QLabel(f"Merged: {names} ({visible_count} object(s))"))
        self._top.addStretch()

        for lbl in getattr(self, '_extra_lbls', []):
            self._lay.removeWidget(lbl)
            lbl.deleteLater()
        self._extra_lbls = []

        self._add_warning_row(
            self.result.collisions,
            lambda n: f"WARNING: {n} real ID collision(s) found - see highlighted "
                      f"rows below. Resolve these before using this as a real combined file.",
            lambda c: f"ID {c.model_id}: " + ", ".join(f"{n} ({os.path.basename(s)})" for n, s in c.entries),
            "ID Collisions")
        self._add_warning_row(
            self.result.name_collisions,
            lambda n: f"WARNING: {n} model name(s) declared under more than one ID - "
                      f"which ID wins depends on file load order.",
            lambda c: f"{c.model_name}: " + ", ".join(f"ID {i} ({os.path.basename(s)})" for i, s in c.entries),
            "Name Collisions")
        self._add_warning_row(
            self.result.redefinitions,
            lambda n: f"WARNING: {n} ID+name pair(s) redefined differently across files - "
                      f"which definition wins depends on file load order.",
            lambda r: f"ID {r.model_id} {r.model_name}: " +
                      ", ".join(f"txd={t} section={s} ({os.path.basename(src)})" for t, s, src in r.entries),
            "Redefinitions")
        self._add_warning_row(
            self.result.out_of_range,
            lambda n: f"WARNING: {n} object ID(s) fall outside the target game's "
                      f"real supported range.",
            lambda o: f"ID {o.model_id} {o.model_name} (valid {o.min_id}-{o.max_id}, "
                      f"{os.path.basename(o.source_ide)})",
            "Out of Range")
        self._add_warning_row(
            self.result.file_range_violations,
            lambda n: f"WARNING: {n} object ID(s) fall outside their own SOL "
                      f"source file's documented ID block.",
            lambda v: f"ID {v.model_id} {v.model_name} ({os.path.basename(v.source_ide)}, "
                      f"expected {v.expected_min}-{v.expected_max})",
            "SOL File Range Violations")

        for err in self.result.errors:
            lbl = QLabel(f"Error: {err}")
            self._lay.insertWidget(1, lbl)
            self._extra_lbls.append(lbl)

    def _add_warning_row(self, items, text_fn, line_fn, popup_title): #vers 1
        """One short warning line + a Details button, for any of the
        4 real checks - keeps the header short instead of dumping
        every entry inline (Sep 12 2026 - same lesson as Asset
        Checker's own window-stretching fix)."""
        if not items:
            return
        container = QWidget()
        row = QHBoxLayout(container)
        row.setContentsMargins(0, 0, 0, 0)
        warn = QLabel(text_fn(len(items)))
        warn.setStyleSheet("font-weight: bold;")
        row.addWidget(warn)
        from PyQt6.QtWidgets import QPushButton
        details_btn = QPushButton("Details...")
        details_btn.clicked.connect(
            lambda: self._show_details_popup(popup_title, [line_fn(i) for i in items]))
        row.addWidget(details_btn)
        row.addStretch()
        self._lay.insertWidget(1, container)
        self._extra_lbls.append(container)

    def _show_details_popup(self, title, lines): #vers 1
        """Plain listing popup for warning details - manually closed,
        not auto-timed, since these need reviewing/acting on (unlike
        Asset Checker's purely informational checked-files list)."""
        popup = QDialog(self)
        popup.setWindowTitle(title)
        popup.resize(480, 400)
        v = QVBoxLayout(popup)
        lst = QListWidget()
        lst.addItems(lines)
        v.addWidget(lst)
        close_btn_row = QHBoxLayout()
        close_btn_row.addStretch()
        from PyQt6.QtWidgets import QPushButton
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(popup.close)
        close_btn_row.addWidget(close_btn)
        v.addLayout(close_btn_row)
        popup.exec()

    def _on_load_from_dat(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Select GTA .dat file", "",
            "GTA DAT files (gta3.dat gta_vc.dat gta.dat gta_sol.dat gtasol.dat);;All files (*.dat)")
        if not path:
            return
        ide_paths, game = collect_ide_paths_from_dat(path)
        if not ide_paths:
            QMessageBox.warning(self, "Master IDE",
                f"No IDE files found from:\n{path}")
            return
        self.result = load_master_ide(ide_paths, game=game)
        self.source_paths = ide_paths
        self.game = game
        self._refresh_top()
        self._populate()

    def _on_insert_ide_file(self): #vers 1
        """Add one more real .ide file to the current merge (Sep 12
        2026, per Keith: "insert text or an ide file, that gets
        merged in") - reruns load_master_ide against the expanded
        file list so it's grouped/checked exactly the same as any
        other loaded file."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Select IDE file to insert", "", "IDE Files (*.ide);;All files (*)")
        if not path:
            return
        if path in self.source_paths:
            QMessageBox.information(self, "Master IDE", "That file is already loaded.")
            return
        self.source_paths.append(path)
        self._reload_after_edit()

    def _on_insert_text(self): #vers 1
        """Paste raw IDE-format text (e.g. "objs\\n...\\nend") and
        either append it to an already-loaded real file or save it
        as a brand-new one - either way it's real text on disk
        before being merged, never held only in memory."""
        dlg = _InsertTextDialog(self, self.source_paths)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        text, target = dlg.values()
        if not text.strip():
            return

        if target == "__new__":
            path, _ = QFileDialog.getSaveFileName(
                self, "Save New IDE File", "", "IDE Files (*.ide)")
            if not path:
                return
            try:
                with open(path, "w", encoding="ascii", errors="ignore") as f:
                    f.write(text)
            except Exception as e:
                QMessageBox.warning(self, "Insert Text Failed", f"Could not write {path}:\n{e}")
                return
            self.source_paths.append(path)
        else:
            if backup_file(target) is None:
                QMessageBox.warning(self, "Insert Text Failed",
                    f"Could not back up before writing:\n{target}")
                return
            try:
                with open(target, "a", encoding="ascii", errors="ignore") as f:
                    f.write("\n" + text if not text.startswith("\n") else text)
            except Exception as e:
                QMessageBox.warning(self, "Insert Text Failed", f"Could not append to {target}:\n{e}")
                return

        self._reload_after_edit()

    def _on_remove_file(self): #vers 1
        """Unload one real loaded file from this merge session -
        does NOT delete or modify the real file on disk, just
        removes it from the current merge scope."""
        if not self.source_paths:
            return
        from PyQt6.QtWidgets import QInputDialog
        names = [os.path.basename(p) for p in self.source_paths]
        name, ok = QInputDialog.getItem(
            self, "Remove File", "Unload which loaded file from this merge?",
            names, 0, False)
        if not ok or not name:
            return
        reply = QMessageBox.question(
            self, "Remove File",
            f"Unload {name} from this merge? The real file on disk is not touched.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
        self.source_paths = [p for p in self.source_paths if os.path.basename(p) != name]
        if not self.source_paths:
            QMessageBox.warning(self, "Master IDE", "At least one file must stay loaded.")
            self.source_paths = [p for p in self.result.source_files]
            return
        self._reload_after_edit()

    def _populate(self): #vers 3
        base = self.palette().color(self.palette().currentColorGroup(),
                                     self.palette().ColorRole.Base)
        from PyQt6.QtGui import QColor
        collision_tint = QColor(
            min(255, base.red() + 40), max(0, base.green() - 25), max(0, base.blue() - 25))
        flagged_ids = {c.model_id for c in self.result.collisions}
        flagged_ids |= {mid for nc in self.result.name_collisions for mid, _ in nc.entries}
        flagged_ids |= {r.model_id for r in self.result.redefinitions}
        flagged_ids |= {o.model_id for o in self.result.out_of_range}
        flagged_ids |= {v.model_id for v in self.result.file_range_violations}

        rows = []   # each entry: ('entry', model_id, model_name, txd_name, source_ide) or ('header'|'end'|'blank', section)
        for section in ("objs", "tobj"):
            objs = self.result.objects_by_section.get(section)
            if not objs:
                continue
            if rows:
                rows.append(("blank", None))
            rows.append(("header", section))
            for obj in objs:
                rows.append(("entry", obj.model_id, obj.model_name, obj.txd_name,
                             os.path.basename(obj.source_ide)))
            rows.append(("end", section))

        self.table.setRowCount(len(rows))
        header_bg = self.palette().color(self.palette().currentColorGroup(),
                                          self.palette().ColorRole.Mid)
        for row, entry in enumerate(rows):
            kind = entry[0]
            if kind in ("header", "end"):
                text = entry[1] if kind == "header" else "end"
                item = QTableWidgetItem(text)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable & ~Qt.ItemFlag.ItemIsSelectable)
                font = item.font()
                font.setBold(True)
                item.setFont(font)
                item.setBackground(header_bg)
                self.table.setItem(row, 0, item)
                self.table.setSpan(row, 0, 1, 4)
                item.setData(Qt.ItemDataRole.UserRole, "marker")
            elif kind == "blank":
                item = QTableWidgetItem("")
                item.setFlags(Qt.ItemFlag.NoItemFlags)
                self.table.setItem(row, 0, item)
                self.table.setSpan(row, 0, 1, 4)
                item.setData(Qt.ItemDataRole.UserRole, "marker")
            else:
                _kind, model_id, model_name, txd_name, source_ide = entry
                has_flag = model_id in flagged_ids
                for col, val in enumerate([str(model_id), model_name, txd_name, source_ide]):
                    item = QTableWidgetItem(val)
                    item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                    if has_flag:
                        item.setBackground(collision_tint)
                    if col == 0:
                        item.setData(Qt.ItemDataRole.UserRole, "entry")
                    self.table.setItem(row, col, item)

    def _resolve_source_path(self, basename_or_path): #vers 1
        """Resolve a real full source path from result.source_files
        matching by basename - table rows only ever display a
        basename (obj.source_ide is basename-only, see master_ide_
        edit.py's own docstring on this pre-existing convention)."""
        target = os.path.basename(basename_or_path)
        for p in self.result.source_files:
            if os.path.basename(p) == target:
                return p
        return None

    def _reload_after_edit(self): #vers 1
        """Re-run load_master_ide against the same real source files
        so the merged view/checks reflect a just-written edit."""
        self.result = load_master_ide(self.source_paths, game=self.game)
        self._refresh_top()
        self._populate()

    def _on_table_context_menu(self, pos): #vers 3
        """Rename/Remove operate on the real full selection (Sep 12
        2026, per Keith). Marker rows (section header/end/blank
        spacer, from the file-shaped layout) are never real entries -
        skipped entirely rather than treated as selectable targets.
        Right-clicking a row outside the current selection replaces
        the selection with just that row. Rename only ever applies
        to exactly one selected row; Remove applies to every
        selected real entry row."""
        row = self.table.rowAt(pos.y())
        if row < 0:
            return
        id_item = self.table.item(row, 0)
        if id_item is None or id_item.data(Qt.ItemDataRole.UserRole) != "entry":
            return
        selected_rows = sorted({idx.row() for idx in self.table.selectionModel().selectedRows()})
        if row not in selected_rows:
            self.table.selectRow(row)
            selected_rows = [row]

        targets = []
        for r in selected_rows:
            item0 = self.table.item(r, 0)
            if item0 is None or item0.data(Qt.ItemDataRole.UserRole) != "entry":
                continue
            model_id = int(item0.text())
            source_ide = self.table.item(r, 3).text()
            targets.append((model_id, source_ide))
        if not targets:
            return

        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        rename_act = menu.addAction("Rename...")
        rename_act.setEnabled(len(targets) == 1)
        remove_label = "Remove" if len(targets) == 1 else f"Remove ({len(targets)})"
        remove_act = menu.addAction(remove_label)
        action = menu.exec(self.table.viewport().mapToGlobal(pos))
        if action == rename_act:
            model_id, source_ide = targets[0]
            self._on_rename_row(model_id, source_ide)
        elif action == remove_act:
            self._on_remove_rows(targets)

    def _on_rename_row(self, model_id, source_ide): #vers 1
        from PyQt6.QtWidgets import QInputDialog
        new_name, ok = QInputDialog.getText(self, "Rename Entry", "New model name:")
        if not ok or not new_name.strip():
            return
        source_path = self._resolve_source_path(source_ide)
        err = rename_entry(self.result, model_id, new_name.strip(), source_ide=source_path)
        if err:
            QMessageBox.warning(self, "Rename Failed", err)
            return
        if not write_source_file(self.result, source_path):
            QMessageBox.warning(self, "Rename Failed",
                f"Renamed in memory but could not write:\n{source_path}")
            return
        self._reload_after_edit()

    def _on_remove_rows(self, targets): #vers 2
        """Remove every given (model_id, source_ide) target - one
        confirmation for the whole batch, one write per real
        touched file (not per entry)."""
        if not targets:
            return
        if len(targets) == 1:
            model_id, source_ide = targets[0]
            prompt = f"Remove ID {model_id} from {os.path.basename(self._resolve_source_path(source_ide))}?"
        else:
            prompt = f"Remove {len(targets)} selected entries?"
        reply = QMessageBox.question(
            self, "Remove Entries", f"{prompt} A backup is made before writing each file.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return

        failures = []
        touched_paths = set()
        for model_id, source_ide in targets:
            source_path = self._resolve_source_path(source_ide)
            err = remove_entry(self.result, model_id, source_path)
            if err:
                failures.append(f"ID {model_id}: {err}")
            else:
                touched_paths.add(source_path)

        write_failures = [p for p in touched_paths if not write_source_file(self.result, p)]
        if failures or write_failures:
            msg = []
            if failures:
                msg.append("Failed to remove: " + "; ".join(failures))
            if write_failures:
                msg.append("Failed to write: " + ", ".join(os.path.basename(p) for p in write_failures))
            QMessageBox.warning(self, "Remove Failed", "\n".join(msg))
        self._reload_after_edit()

    def _on_add_entry(self): #vers 1
        dlg = _AddEntryDialog(self, self.result.source_files)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        values = dlg.values()
        source_path = self._resolve_source_path(values['source_ide'])
        extra = {'draw_dist': values['draw_dist'], 'flags': values['flags']}
        if values['section'] == 'tobj':
            extra['time_on'] = values['time_on']
            extra['time_off'] = values['time_off']
        err = add_entry(self.result, values['section'], values['model_id'],
                         values['model_name'], values['txd_name'], source_path, extra=extra)
        if err:
            QMessageBox.warning(self, "Add Entry Failed", err)
            return
        if not write_source_file(self.result, source_path):
            QMessageBox.warning(self, "Add Entry Failed",
                f"Added in memory but could not write:\n{source_path}")
            return
        self._reload_after_edit()

    def _on_id_shift(self): #vers 2
        from apps.methods.id_shift_dialog import IDShiftDialog
        dlg = IDShiftDialog(self, self.result, game=self.game)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            self._reload_after_edit()

    def _on_save(self): #vers 3
        total_flags = (len(self.result.collisions) + len(self.result.name_collisions) +
                       len(self.result.redefinitions) + len(self.result.out_of_range) +
                       len(self.result.file_range_violations))
        if total_flags:
            parts = []
            if self.result.collisions:
                parts.append(f"{len(self.result.collisions)} ID collision(s)")
            if self.result.name_collisions:
                parts.append(f"{len(self.result.name_collisions)} name collision(s)")
            if self.result.redefinitions:
                parts.append(f"{len(self.result.redefinitions)} redefinition(s)")
            if self.result.out_of_range:
                parts.append(f"{len(self.result.out_of_range)} out-of-range ID(s)")
            if self.result.file_range_violations:
                parts.append(f"{len(self.result.file_range_violations)} SOL file-range violation(s)")
            reply = QMessageBox.warning(
                self, "Issues Found",
                f"{', '.join(parts)} found across the merged files. Saving now will "
                f"keep everything as-is (nothing is renumbered by this step). "
                f"Continue anyway?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
            if reply != QMessageBox.StandardButton.Yes:
                return

        path, _ = QFileDialog.getSaveFileName(self, "Save Master IDE", "", "IDE Files (*.ide)")
        if not path:
            return
        if write_master_ide(self.result, path):
            QMessageBox.information(self, "Saved", f"Master IDE saved to:\n{path}")
        else:
            QMessageBox.warning(self, "Save Failed", "Could not write the master IDE file.")


class _InsertTextDialog(QDialog): #vers 1
    """Small form for Insert Text - a plain text box for pasting raw
    real IDE-format text, plus where it goes: appended to an already
    loaded real file, or saved as a brand-new one (Sep 12 2026, per
    Keith: "insert text or an ide file, that gets merged in")."""
    def __init__(self, parent, source_paths): #vers 1
        super().__init__(parent)
        self.setWindowTitle("Insert Text")
        self.resize(520, 420)
        from PyQt6.QtWidgets import QTextEdit, QComboBox, QFormLayout
        self._source_paths = source_paths
        lay = QVBoxLayout(self)

        lay.addWidget(QLabel("Paste real IDE-format text (e.g. \"objs\\n<id>, <model>, "
                              "<txd>, <dist>, <flags>\\nend\"):"))
        self.text_edit = QTextEdit()
        lay.addWidget(self.text_edit, 1)

        form = QFormLayout()
        self.target_combo = QComboBox()
        self.target_combo.addItem("New file...", "__new__")
        for p in source_paths:
            self.target_combo.addItem(f"Append to {os.path.basename(p)}", p)
        form.addRow("Add to:", self.target_combo)
        lay.addLayout(form)

        btn_row = QHBoxLayout()
        ok_btn = QPushButton("Insert")
        ok_btn.clicked.connect(self.accept)
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        btn_row.addWidget(ok_btn)
        btn_row.addWidget(cancel_btn)
        lay.addLayout(btn_row)

    def values(self): #vers 1
        return self.text_edit.toPlainText(), self.target_combo.currentData()


class _AddEntryDialog(QDialog): #vers 1
    """Small form for Add Entry - section/ID/name/txd/draw_dist/
    flags(+time_on/time_off for tobj)/source file, only objs/tobj
    supported (see master_ide_edit.py's own docstring)."""
    def __init__(self, parent, source_files): #vers 1
        super().__init__(parent)
        self.setWindowTitle("Add Entry")
        from PyQt6.QtWidgets import QFormLayout, QComboBox, QLineEdit, QSpinBox, QDoubleSpinBox
        self._source_files = source_files
        lay = QFormLayout(self)

        self.section_combo = QComboBox()
        self.section_combo.addItems(["objs", "tobj"])
        self.section_combo.currentTextChanged.connect(self._on_section_changed)
        lay.addRow("Section:", self.section_combo)

        self.id_spin = QSpinBox()
        self.id_spin.setRange(0, 999999)
        lay.addRow("Model ID:", self.id_spin)

        self.name_edit = QLineEdit()
        lay.addRow("Model name:", self.name_edit)

        self.txd_edit = QLineEdit()
        lay.addRow("TXD name:", self.txd_edit)

        self.dist_spin = QDoubleSpinBox()
        self.dist_spin.setRange(0, 99999)
        self.dist_spin.setValue(250)
        lay.addRow("Draw distance:", self.dist_spin)

        self.flags_spin = QSpinBox()
        self.flags_spin.setRange(0, 999999)
        lay.addRow("Flags:", self.flags_spin)

        self.time_on_spin = QSpinBox()
        self.time_on_spin.setRange(0, 23)
        self.time_on_row_label = QLabel("Time on:")
        lay.addRow(self.time_on_row_label, self.time_on_spin)

        self.time_off_spin = QSpinBox()
        self.time_off_spin.setRange(0, 23)
        self.time_off_row_label = QLabel("Time off:")
        lay.addRow(self.time_off_row_label, self.time_off_spin)

        self.source_combo = QComboBox()
        self.source_combo.addItems([os.path.basename(p) for p in source_files])
        lay.addRow("Source file:", self.source_combo)

        btn_row = QHBoxLayout()
        ok_btn = QPushButton("Add")
        ok_btn.clicked.connect(self.accept)
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        btn_row.addWidget(ok_btn)
        btn_row.addWidget(cancel_btn)
        lay.addRow(btn_row)

        self._on_section_changed(self.section_combo.currentText())

    def _on_section_changed(self, section): #vers 1
        show_time = (section == "tobj")
        self.time_on_spin.setVisible(show_time)
        self.time_on_row_label.setVisible(show_time)
        self.time_off_spin.setVisible(show_time)
        self.time_off_row_label.setVisible(show_time)

    def values(self): #vers 1
        return {
            'section': self.section_combo.currentText(),
            'model_id': self.id_spin.value(),
            'model_name': self.name_edit.text().strip(),
            'txd_name': self.txd_edit.text().strip(),
            'draw_dist': self.dist_spin.value(),
            'flags': self.flags_spin.value(),
            'time_on': self.time_on_spin.value(),
            'time_off': self.time_off_spin.value(),
            'source_ide': self._source_files[self.source_combo.currentIndex()],
        }


def show_master_ide(main_window, ide_paths, game: str = None): #vers 2
    """Entry point - loads and merges the given real .ide files and
    shows the result. ide_paths can be a single path or a list."""
    if isinstance(ide_paths, str):
        ide_paths = [ide_paths]
    result = load_master_ide(ide_paths, game=game)
    dlg = MasterIDEDialog(main_window, result, ide_paths, game=game)
    dlg.exec()


def show_master_ide_from_dat(main_window, dat_path: str): #vers 1
    """Entry point - resolves every real IDE a game's .dat loads
    (default.dat + main dat), merges, and shows the result."""
    ide_paths, game = collect_ide_paths_from_dat(dat_path)
    if not ide_paths:
        QMessageBox.warning(main_window, "Master IDE",
            f"No IDE files found from:\n{dat_path}")
        return
    show_master_ide(main_window, ide_paths, game=game)
