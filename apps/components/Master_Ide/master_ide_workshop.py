#this belongs in apps/components/Master_Ide/master_ide_workshop.py - Version: 9
# X-Seti - September 17 2026 - IMG Factory 1.6 - Master IDE Workshop

"""master_ide_workshop.py - Master IDE as its own standalone,
dockable workshop. Same dual-mode pattern every workshop here uses
(open_col_workshop's own shape): embeds as a tab if main_window has
one, real standalone floating window otherwise, registers in the
tool taskbar. The button row is a set of icon ribbon toolbars
(File/Entries/Tools/Filters) docked to an inner QMainWindow, same
pattern as Asset Workshop's own ribbons, instead of one wide row of
text buttons."""

##Methods list -
# MasterIDEWorkshop
# _InsertTextDialog
# _AddEntryDialog
# open_master_ide_workshop

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QTableWidget, QTableWidgetItem,
    QPushButton, QFileDialog, QMessageBox, QWidget, QListWidget, QCheckBox,
    QAbstractItemView, QMainWindow, QToolBar, QSpinBox, QComboBox,
)
from PyQt6.QtCore import Qt, QSize
from PyQt6.QtGui import QAction

from apps.methods.master_ide import (
    load_master_ide, write_master_ide, collect_ide_paths_from_dat)
from apps.methods.master_ide_edit import rename_entry, add_entry, remove_entry, write_source_file
from apps.methods.file_backup import backup_file
from apps.methods.imgfactory_svg_icons import SVGIconFactory


class _MasterIDETable(QTableWidget): #vers 1
    """QTableWidget with drag-move reinterpreted as a real splice-
    move (Sep 12 2026, per Keith's own drag UI request: "click and
    drag to scroll... hold down left click to drag those entries to
    another location within the list"). Qt's own InternalMove drag/
    drop machinery already handles the press-hold-drag-vs-click
    distinction and the visual drop indicator/auto-scroll; dropEvent
    is overridden to redirect the actual DATA operation to
    drop_callback(selected_rows, target_row, drop_position) instead
    of letting Qt physically rearrange widget rows itself - the
    workshop always rebuilds via its own _populate() after a real
    splice-move is applied, never from Qt's own row shuffle."""
    def __init__(self, *args, **kwargs): #vers 1
        super().__init__(*args, **kwargs)
        self.drop_callback = None
        self.setDragEnabled(True)
        self.setAcceptDrops(True)
        self.setDragDropMode(QAbstractItemView.DragDropMode.InternalMove)
        self.setDragDropOverwriteMode(False)

    def dropEvent(self, event): #vers 1
        target_index = self.indexAt(event.position().toPoint())
        target_row = target_index.row() if target_index.isValid() else self.rowCount() - 1
        drop_pos = self.dropIndicatorPosition()
        selected_rows = sorted({idx.row() for idx in self.selectionModel().selectedRows()})
        event.ignore()   # never let Qt physically rearrange rows itself
        if self.drop_callback:
            self.drop_callback(selected_rows, target_row, drop_pos)


class MasterIDEWorkshop(QWidget): #vers 9
    # Bump whenever the set of ribbon toolbars changes, so a saved
    # layout from an older structure is cleanly rejected instead of
    # silently failing to restore. History: 1 = File/Entries/Tools/
    # Filters ribbons (replaced the old DockableToolbar button row).
    _RIBBON_LAYOUT_VERSION = 1

    def __init__(self, parent, main_window=None): #vers 1
        super().__init__(parent)
        self.main_window = main_window
        self.result = None
        self.source_paths = []
        self.game = None
        self.dat_path = None   # last real .dat used, for reloading with a changed ignore-files option
        self.ignore_base_files = True
        self.ignore_id_range = None   # (min, max) or None - display/check filter only, never touches saved data
        self._tab_container = None
        self._build_ui()
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(400, self._restore_toolbar_state)

    def load_ide_paths(self, ide_paths, game=None): #vers 1
        """Load from a plain real .ide path list (single-file or
        multi-file right-click entry points)."""
        if isinstance(ide_paths, str):
            ide_paths = [ide_paths]
        self.dat_path = None
        self.source_paths = list(ide_paths)
        self.game = game
        self._reload_after_edit()

    def load_from_dat(self, dat_path): #vers 1
        """Load by resolving every real IDE a game's .dat loads."""
        self.dat_path = dat_path
        ide_paths, game = collect_ide_paths_from_dat(
            dat_path, ignore_base_files=self.ignore_base_files)
        if not ide_paths:
            QMessageBox.warning(self, "Master IDE", f"No IDE files found from:\n{dat_path}")
            return
        self.source_paths = ide_paths
        self.game = game
        self._reload_after_edit()

    def _build_ui(self): #vers 2
        self._lay = QVBoxLayout(self)

        self._top = QHBoxLayout()
        self._lay.addLayout(self._top)
        self._extra_lbls = []

        self.table = _MasterIDETable()
        self.table.setColumnCount(4)
        self.table.setHorizontalHeaderLabels(["ID", "Model", "TXD", "Source IDE"])
        self.table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self.table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self.table.horizontalHeader().setStretchLastSection(True)
        self.table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.table.customContextMenuRequested.connect(self._on_table_context_menu)
        self.table.drop_callback = self._on_splice_drop
        self._entry_rows = []   # (row_index, model_id, section) for every real entry row, built by _populate

        self._inner_mw = QMainWindow()
        self._inner_mw.setWindowFlags(Qt.WindowType.Widget)
        self._inner_mw.setCentralWidget(self.table)
        self._build_toolbars()
        self._lay.addWidget(self._inner_mw, 1)

        self.status_bar = QLabel()
        self.status_bar.setStyleSheet("padding: 2px 4px;")
        self._lay.addWidget(self.status_bar)

    def _build_toolbars(self): #vers 1
        """Ribbon toolbars replacing the old DockableToolbar button
        row - too many text buttons in one bar, so they're grouped
        into icon ribbons (File/Entries/Tools/Filters) the same way
        every other workshop's ribbon is built."""
        icon_color = None
        icon_size = QSize(20, 20)
        icons = SVGIconFactory()
        mw = self._inner_mw

        def _tb(name): #vers 1
            tb = QToolBar(name, mw)
            tb.setObjectName(name)
            tb.setIconSize(icon_size)
            tb.setMovable(True)
            tb.setFloatable(True)
            tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            tb.customContextMenuRequested.connect(
                lambda pos, t=tb: self._toolbar_context_menu(t, pos))
            mw.addToolBar(Qt.ToolBarArea.TopToolBarArea, tb)
            return tb

        def _act(tb, name, icon_fn, callback): #vers 1
            act = QAction(icon_fn(color=icon_color), name, mw)
            act.setToolTip(name)
            act.triggered.connect(callback)
            tb.addAction(act)
            return act

        #    Ribbon: File
        tb_file = _tb("File")
        _act(tb_file, "Load from .dat...", icons.folder_icon, self._on_load_from_dat)
        _act(tb_file, "Insert IDE File...", icons.import_icon, self._on_insert_ide_file)
        _act(tb_file, "Remove File...", icons.trash_icon, self._on_remove_file)
        tb_file.addSeparator()
        _act(tb_file, "Save as Master IDE...", icons.saveas_icon, self._on_save)

        #    Ribbon: Entries
        tb_entries = _tb("Entries")
        _act(tb_entries, "Insert Text...", icons.edit_icon, self._on_insert_text)
        _act(tb_entries, "Add Entry...", icons.add_icon, self._on_add_entry)
        _act(tb_entries, "Insert && Relocate File...", icons.package_icon, self._on_insert_relocate)
        tb_entries.addSeparator()
        _act(tb_entries, "Add ID...", icons.add_id_icon, self._on_add_id)
        _act(tb_entries, "Remove / Delete ID...", icons.remove_id_icon, self._on_remove_delete_id)
        _act(tb_entries, "Move / Reassign ID Block...", icons.convert_icon, self._on_id_shift)

        #    Ribbon: Tools
        tb_tools = _tb("Tools")
        _act(tb_tools, "ID Utilities...", icons.id_utilities_icon, self._on_id_utilities)
        _act(tb_tools, "TXD Duplicate Check...", icons.txd_dedup_icon, self._on_txd_dedup)
        _act(tb_tools, "IMG / COL Physical Reorder...", icons.database_icon, self._on_img_col_reorder)

        #    Ribbon: Filters
        tb_filters = _tb("Filters")
        self.ignore_base_chk = QCheckBox("Ignore default.ide/gta3.ide")
        self.ignore_base_chk.setChecked(self.ignore_base_files)
        self.ignore_base_chk.setToolTip(
            "Skip the base engine's own default.ide/gta3.ide (peds/cars/"
            "wheels/weapons/hier only) when loading from a .dat, so ID "
            "counting/reassignment starts from the first real world "
            "(generic) IDE file instead.")
        self.ignore_base_chk.toggled.connect(self._on_ignore_base_toggled)
        tb_filters.addWidget(self.ignore_base_chk)
        tb_filters.addSeparator()

        self.ignore_range_chk = QCheckBox("Ignore ID range")
        self.ignore_range_chk.setToolTip(
            "Hide this range from the table and from every check - "
            "display/check only, never touches what Save as Master "
            "IDE actually writes (Sep 12 2026, per Keith: 'ignore "
            "0-1932 so it checks everything after').")
        tb_filters.addWidget(self.ignore_range_chk)
        self.ignore_range_from = QSpinBox()
        self.ignore_range_from.setRange(0, 999999)
        tb_filters.addWidget(self.ignore_range_from)
        tb_filters.addWidget(QLabel("to"))
        self.ignore_range_to = QSpinBox()
        self.ignore_range_to.setRange(0, 999999)
        self.ignore_range_to.setValue(1932)
        tb_filters.addWidget(self.ignore_range_to)
        _act(tb_filters, "Apply", icons.check_icon, self._on_apply_ignore_range)

    def _toolbar_context_menu(self, toolbar, pos): #vers 1
        """Right-click context menu on any ribbon toolbar."""
        from PyQt6.QtWidgets import QMenu, QToolBar as _QTB
        menu = QMenu(self)
        menu.addAction("Save Ribbon Config", self._save_toolbar_state)
        menu.addSeparator()
        menu.addAction("Lock All Toolbars",
            lambda: [tb.setMovable(False)
                     for tb in self._inner_mw.findChildren(_QTB)])
        menu.addAction("Unlock All Toolbars",
            lambda: [tb.setMovable(True)
                     for tb in self._inner_mw.findChildren(_QTB)])
        menu.exec(toolbar.mapToGlobal(pos))

    def _save_toolbar_state(self): #vers 1
        """Save the ribbon layout to master_ide.json."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        try:
            import json
            from pathlib import Path
            path = Path.home() / '.config' / 'imgfactory' / 'master_ide.json'
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['toolbar_state'] = mw.saveState(self._RIBBON_LAYOUT_VERSION).toHex().data().decode()
            data['toolbar_state_version'] = self._RIBBON_LAYOUT_VERSION
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(json.dumps(data, indent=2))
            self.status_bar.setText("Ribbon config saved")
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Master IDE: Ribbon config saved")
        except Exception as e:
            print(f"[Master IDE] _save_toolbar_state error: {e}")

    def _restore_toolbar_state(self): #vers 1
        """Restore the ribbon layout from master_ide.json - rejects a
        saved layout from an older ribbon structure (see
        _RIBBON_LAYOUT_VERSION) instead of silently failing."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        try:
            import json
            from pathlib import Path
            from PyQt6.QtCore import QByteArray
            path = Path.home() / '.config' / 'imgfactory' / 'master_ide.json'
            if not path.exists():
                return
            data = json.loads(path.read_text())
            state_hex = data.get('toolbar_state')
            saved_version = data.get('toolbar_state_version')
            if state_hex and saved_version == self._RIBBON_LAYOUT_VERSION:
                mw.restoreState(QByteArray.fromHex(state_hex.encode()),
                                 self._RIBBON_LAYOUT_VERSION)
        except Exception as e:
            print(f"[Master IDE] _restore_toolbar_state error: {e}")
        finally:
            # Safety net: restoreState() can leave a ribbon fully
            # hidden with no way to bring it back - force every one
            # visible no matter what happened above.
            from PyQt6.QtWidgets import QToolBar as _QTB
            for tb in mw.findChildren(_QTB):
                tb.setVisible(True)
                tb.toggleViewAction().setChecked(True)

    def closeEvent(self, event): #vers 1
        try:
            self._save_toolbar_state()
        except Exception:
            pass
        super().closeEvent(event)

    def _on_ignore_base_toggled(self, checked): #vers 1
        self.ignore_base_files = checked
        if self.dat_path:
            self.load_from_dat(self.dat_path)

    def _on_apply_ignore_range(self): #vers 1
        if self.ignore_range_chk.isChecked():
            lo, hi = self.ignore_range_from.value(), self.ignore_range_to.value()
            if lo > hi:
                QMessageBox.warning(self, "Master IDE", "'From' must not be greater than 'to'.")
                return
            self.ignore_id_range = (lo, hi)
        else:
            self.ignore_id_range = None
        if self.result:
            self._refresh_top()
            self._populate()

    def _id_ignored(self, model_id): #vers 1
        if not self.ignore_id_range:
            return False
        lo, hi = self.ignore_id_range
        return lo <= model_id <= hi

    def _filter_ignored_single(self, items): #vers 1
        """Drop items whose own .model_id falls in the ignored
        range - for check types with exactly one real id per item
        (collisions/redefinitions/out_of_range/file_range_violations)."""
        if not self.ignore_id_range:
            return items
        return [i for i in items if not self._id_ignored(i.model_id)]

    def _filter_ignored_multi(self, items): #vers 1
        """Drop items only when EVERY id in their .entries falls in
        the ignored range - a collision straddling the boundary
        still needs the non-ignored side flagged (name_collisions
        only, since its entries are (model_id, source) pairs)."""
        if not self.ignore_id_range:
            return items
        return [i for i in items if not all(self._id_ignored(mid) for mid, _ in i.entries)]

    def _refresh_top(self): #vers 3
        """Rebuild the header info/warning/error labels."""
        while self._top.count():
            item = self._top.takeAt(0)
            if item.widget():
                item.widget().deleteLater()
        visible_count = sum(
            1 for section, objs in self.result.objects_by_section.items()
            if section not in self.result.raw_section_lines
            for obj in objs if not self._id_ignored(obj.model_id))
        range_note = f" - ignoring {self.ignore_id_range[0]}-{self.ignore_id_range[1]}" \
            if self.ignore_id_range else ""
        num_files = len(self.result.source_files)
        self._top.addWidget(QLabel(
            f"Merged: {num_files} file{'s' if num_files != 1 else ''} "
            f"({visible_count} object(s){range_note})"))
        merged_files_combo = QComboBox()
        merged_files_combo.addItems(
            os.path.basename(p) for p in self.result.source_files)
        merged_files_combo.setToolTip("Every real source file merged into this view")
        self._top.addWidget(merged_files_combo)
        self._top.addStretch()

        for lbl in getattr(self, '_extra_lbls', []):
            self._lay.removeWidget(lbl)
            lbl.deleteLater()
        self._extra_lbls = []

        self._add_warning_row(
            self._filter_ignored_single(self.result.collisions),
            lambda n: f"WARNING: {n} real ID collision(s) found - see highlighted "
                      f"rows below. Resolve these before using this as a real combined file.",
            lambda c: f"ID {c.model_id}: " + ", ".join(f"{n} ({os.path.basename(s)})" for n, s in c.entries),
            "ID Collisions")
        self._add_warning_row(
            self._filter_ignored_multi(self.result.name_collisions),
            lambda n: f"WARNING: {n} model name(s) declared under more than one ID - "
                      f"which ID wins depends on file load order.",
            lambda c: f"{c.model_name}: " + ", ".join(f"ID {i} ({os.path.basename(s)})" for i, s in c.entries),
            "Name Collisions")
        self._add_warning_row(
            self._filter_ignored_single(self.result.redefinitions),
            lambda n: f"WARNING: {n} ID+name pair(s) redefined differently across files - "
                      f"which definition wins depends on file load order.",
            lambda r: f"ID {r.model_id} {r.model_name}: " +
                      ", ".join(f"txd={t} section={s} ({os.path.basename(src)})" for t, s, src in r.entries),
            "Redefinitions")
        self._add_warning_row(
            self._filter_ignored_single(self.result.out_of_range),
            lambda n: f"WARNING: {n} object ID(s) fall outside the target game's "
                      f"real supported range.",
            lambda o: f"ID {o.model_id} {o.model_name} (valid {o.min_id}-{o.max_id}, "
                      f"{os.path.basename(o.source_ide)})",
            "Out of Range")
        self._add_warning_row(
            self._filter_ignored_single(self.result.file_range_violations),
            lambda n: f"WARNING: {n} object ID(s) fall outside their own SOL "
                      f"source file's documented ID block.",
            lambda v: f"ID {v.model_id} {v.model_name} ({os.path.basename(v.source_ide)}, "
                      f"expected {v.expected_min}-{v.expected_max})",
            "SOL File Range Violations")

        for err in self.result.errors:
            lbl = QLabel(f"Error: {err}")
            self._lay.insertWidget(1, lbl)
            self._extra_lbls.append(lbl)

        self._refresh_status_bar()

    def _refresh_status_bar(self): #vers 2
        """IDs used / free within the engine's own ID range - "free"
        is real remaining capacity in that range, not a count of
        literal gaps between used IDs. Uses this app's own stored
        ID_RANGES as a default only - the real engine limit varies
        and isn't asserted here. When an ID range is being ignored,
        both used and capacity exclude it, so free reflects only
        the range actually being checked."""
        try:
            from apps.methods.gta_dat_parser import GTAGame
            min_id, max_id = GTAGame.ID_RANGES.get(self.game, (0, 32767))
        except Exception:
            min_id, max_id = 0, 32767

        used_ids = {obj.model_id for section in ("objs", "tobj")
                    for obj in self.result.objects_by_section.get(section, [])}
        used_in_range = sum(1 for i in used_ids if min_id <= i <= max_id and not self._id_ignored(i))
        capacity = max_id - min_id + 1
        if self.ignore_id_range:
            lo, hi = self.ignore_id_range
            overlap_lo, overlap_hi = max(min_id, lo), min(max_id, hi)
            if overlap_lo <= overlap_hi:
                capacity -= (overlap_hi - overlap_lo + 1)
        free = max(0, capacity - used_in_range)
        range_note = f", ignoring {self.ignore_id_range[0]}-{self.ignore_id_range[1]}" \
            if self.ignore_id_range else ""
        self.status_bar.setText(
            f"IDs used: {used_in_range}  |  Free: {free}  "
            f"(range {min_id}-{max_id}{range_note}, this app's default for the detected game)")

    def _add_warning_row(self, items, text_fn, line_fn, popup_title): #vers 1
        """One short warning line + a Details button, for any of the
        checks - keeps the header short instead of dumping every
        entry inline."""
        if not items:
            return
        container = QWidget()
        row = QHBoxLayout(container)
        row.setContentsMargins(0, 0, 0, 0)
        warn = QLabel(text_fn(len(items)))
        warn.setStyleSheet("font-weight: bold;")
        row.addWidget(warn)
        details_btn = QPushButton("Details...")
        details_btn.clicked.connect(
            lambda: self._show_details_popup(popup_title, [line_fn(i) for i in items]))
        row.addWidget(details_btn)
        row.addStretch()
        self._lay.insertWidget(1, container)
        self._extra_lbls.append(container)

    def _show_details_popup(self, title, lines): #vers 1
        """Plain listing popup for warning details - manually closed."""
        popup = QDialog(self)
        popup.setWindowTitle(title)
        popup.resize(480, 400)
        v = QVBoxLayout(popup)
        lst = QListWidget()
        lst.addItems(lines)
        v.addWidget(lst)
        close_btn_row = QHBoxLayout()
        close_btn_row.addStretch()
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
        self.load_from_dat(path)

    def _on_insert_ide_file(self): #vers 1
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

        rows = []
        self._entry_rows = []
        for section in ("objs", "tobj"):
            objs = [o for o in (self.result.objects_by_section.get(section) or [])
                    if not self._id_ignored(o.model_id)]
            if not objs:
                continue
            if rows:
                rows.append(("blank", None))
            rows.append(("header", section))
            prev_source = None
            for obj in objs:
                source_name = os.path.basename(obj.source_ide)
                if source_name != prev_source:
                    # Real ID-sorted order interleaves different real
                    # source files (Sep 12 2026, per Keith: "mark the
                    # beginning on the ID file, so you know you have
                    # scroll past airport.ide and are now looking at
                    # airportn.ide") - a divider row whenever the
                    # real source changes, not just at file load.
                    rows.append(("source_change", source_name))
                    prev_source = source_name
                rows.append(("entry", obj.model_id, obj.model_name, obj.txd_name, source_name))
                self._entry_rows.append((len(rows) - 1, obj.model_id, section))
            rows.append(("end", section))

        self.table.setRowCount(len(rows))
        header_bg = self.palette().color(self.palette().currentColorGroup(),
                                          self.palette().ColorRole.Mid)
        source_bg = self.palette().color(self.palette().currentColorGroup(),
                                          self.palette().ColorRole.AlternateBase)
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
            elif kind == "source_change":
                item = QTableWidgetItem(f"\u25b8 {entry[1]}")
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable & ~Qt.ItemFlag.ItemIsSelectable)
                font = item.font()
                font.setItalic(True)
                item.setFont(font)
                item.setBackground(source_bg)
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
        target = os.path.basename(basename_or_path)
        for p in self.result.source_files:
            if os.path.basename(p) == target:
                return p
        return None

    def _reload_after_edit(self): #vers 1
        self.result = load_master_ide(self.source_paths, game=self.game)
        self._refresh_top()
        self._populate()

    def _on_table_context_menu(self, pos): #vers 1
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

    def _on_rename_row(self, model_id, source_ide): #vers 3
        """Rename cascades into every real IPL "inst"/"cars" line
        placing this same ID+old-name, so the IPL's own model-name
        field stays matching the IDE (Sep 17 2026, per Keith: "if
        IDE modelnames are changed, IPL modelnames need to also
        match"). Also cascades into this same IDE file's own "path"
        section header (GTA III/VC only - see id_reassign.py's own
        note). IPL cascade only runs when the game was loaded via
        .dat (self.dat_path known) - without a game root there's no
        real IPL file list to derive."""
        from PyQt6.QtWidgets import QInputDialog
        new_name, ok = QInputDialog.getText(self, "Rename Entry", "New model name:")
        if not ok or not new_name.strip():
            return
        new_name = new_name.strip()
        source_path = self._resolve_source_path(source_ide)

        old_name = None
        for section in ("objs", "tobj", "anim"):
            for obj in self.result.objects_by_section.get(section, []):
                if obj.model_id == model_id and os.path.basename(obj.source_ide) == os.path.basename(source_path):
                    old_name = obj.model_name
                    break
            if old_name:
                break

        err = rename_entry(self.result, model_id, new_name, source_ide=source_path)
        if err:
            QMessageBox.warning(self, "Rename Failed", err)
            return
        if not write_source_file(self.result, source_path):
            QMessageBox.warning(self, "Rename Failed",
                f"Renamed in memory but could not write:\n{source_path}")
            return

        if old_name:
            try:
                from apps.methods.id_reassign import cascade_path_rename
                cascade_path_rename([source_path], model_id, old_name, new_name)
            except Exception as e:
                QMessageBox.warning(self, "Path Cascade Failed",
                    f"Renamed in IDE, but could not cascade into the 'path' section:\n{e}")
            if self.dat_path:
                try:
                    from apps.methods.master_ide import collect_ipl_paths_from_dat
                    from apps.methods.id_reassign import cascade_ipl_rename
                    ipl_paths = collect_ipl_paths_from_dat(self.dat_path, game=self.game)
                    cascade_ipl_rename(ipl_paths, model_id, old_name, new_name)
                except Exception as e:
                    QMessageBox.warning(self, "IPL Cascade Failed",
                        f"Renamed in IDE, but could not cascade into IPL files:\n{e}")

        self._reload_after_edit()

    def _on_remove_rows(self, targets): #vers 3
        """Removing an ID orphans its own 2dfx effects, any real IPL
        placements, and (GTA III/VC only) any "path" block
        referencing it (Sep 17 2026, per Keith: "when changing ID's
        in the IDE, other entries need to be accounted for, like
        2dfx... in the cascade"). Warns with the affected usages
        before removing, then cascades the deletion into all three
        once the IDE removal itself succeeds."""
        if not targets:
            return
        model_ids = {model_id for model_id, _ in targets}

        from apps.methods.id_reassign import (
            find_usages, find_ipl_usages, find_path_usages,
            cascade_delete_2dfx, remove_ipl_lines, remove_path_blocks)

        dfx_usages = []
        for model_id in model_ids:
            dfx_usages.extend(u for u in find_usages(self.result, model_id=model_id)
                               if u['section'] == '2dfx')

        source_paths = {self._resolve_source_path(source_ide) for _, source_ide in targets}
        path_usages = []
        for model_id in model_ids:
            path_usages.extend(find_path_usages(list(source_paths), model_id))

        ipl_paths = []
        ipl_usages = []
        if self.dat_path:
            try:
                from apps.methods.master_ide import collect_ipl_paths_from_dat
                ipl_paths = collect_ipl_paths_from_dat(self.dat_path, game=self.game)
                for model_id in model_ids:
                    ipl_usages.extend(find_ipl_usages(ipl_paths, model_id))
            except Exception:
                ipl_paths, ipl_usages = [], []

        if len(targets) == 1:
            model_id, source_ide = targets[0]
            prompt = f"Remove ID {model_id} from {os.path.basename(self._resolve_source_path(source_ide))}?"
        else:
            prompt = f"Remove {len(targets)} selected entries?"
        warn_lines = []
        if dfx_usages:
            warn_lines.append(f"\n{len(dfx_usages)} 2dfx effect(s) reference these IDs and will be removed too.")
        if path_usages:
            warn_lines.append(f"\n{len(path_usages)} 'path' block(s) reference these IDs and will be removed too.")
        if ipl_usages:
            files = sorted({os.path.basename(u['ipl_path']) for u in ipl_usages})
            warn_lines.append(f"\n{len(ipl_usages)} IPL placement(s) in {', '.join(files)} "
                               f"reference these IDs and will be removed too.")
        reply = QMessageBox.question(
            self, "Remove Entries",
            f"{prompt} A backup is made before writing each file.{''.join(warn_lines)}",
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

        removed_paths = touched_paths - set(write_failures)
        if removed_paths:
            # Every loaded file, not just the ones written for the
            # objs/tobj removal itself - a deleted model's 2dfx
            # effects (or, GTA III/VC, its "path" block) can live in
            # a separate loaded file (e.g. SOL's GAME_LC.IFX never
            # gets an objs/tobj write of its own, so it was never in
            # removed_paths).
            if dfx_usages:
                cascade_delete_2dfx(self.result.source_files, model_ids)
            if path_usages:
                remove_path_blocks(self.result.source_files, model_ids)
            if ipl_paths and ipl_usages:
                remove_ipl_lines(ipl_paths, model_ids)

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

    def _resolve_drop_target(self, target_row, drop_position): #vers 1
        """Real target ID a drop lands at - AboveItem drops before
        that row's own ID, BelowItem/OnItem drops after it. Dropping
        on a marker row (header/end/blank/source-change divider) or
        past the end of the table resolves to the nearest real entry
        row instead of failing."""
        if target_row < 0 or target_row >= self.table.rowCount():
            return (self._entry_rows[-1][1] + 1) if self._entry_rows else None
        item = self.table.item(target_row, 0)
        if item and item.data(Qt.ItemDataRole.UserRole) == "entry":
            target_id = int(item.text())
            if drop_position == QAbstractItemView.DropIndicatorPosition.BelowItem:
                return target_id + 1
            return target_id
        for r in range(target_row, self.table.rowCount()):
            it = self.table.item(r, 0)
            if it and it.data(Qt.ItemDataRole.UserRole) == "entry":
                return int(it.text())
        for r in range(target_row, -1, -1):
            it = self.table.item(r, 0)
            if it and it.data(Qt.ItemDataRole.UserRole) == "entry":
                return int(it.text()) + 1
        return None

    def _on_splice_drop(self, selected_rows, target_row, drop_position): #vers 2
        """Real drag-move (Sep 12 2026, per Keith: "hold down left
        click to drag those entries to another location within the
        list"). Validates the dragged selection is one contiguous
        real block within a single section before touching anything -
        a scattered or mixed-section selection is refused outright,
        never guessed into a "best effort" range."""
        from apps.methods.id_reassign import (
            validate_contiguous_selection, plan_splice_move, apply_id_shift_and_write, cascade_ipl_files)

        result = validate_contiguous_selection(self._entry_rows, set(selected_rows))
        if isinstance(result, str):
            QMessageBox.warning(self, "Move Entries", result)
            return
        move_start, move_end, _section = result

        target_start = self._resolve_drop_target(target_row, drop_position)
        if target_start is None:
            return

        plan = plan_splice_move(self.result, move_start, move_end, target_start)
        if not plan.moved:
            QMessageBox.information(self, "Move Entries",
                "Nothing to move - the drop target falls inside the selection itself.")
            return

        moved_count = move_end - move_start + 1
        displaced_count = len(plan.moved) - moved_count
        reply = QMessageBox.question(
            self, "Move Entries",
            f"Move {moved_count} entrie(s) (ID {move_start}-{move_end}) to start at "
            f"{target_start}? {displaced_count} other real entrie(s) will shift to make "
            f"room. A backup is made before writing.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return

        touched = apply_id_shift_and_write(self.result, plan)
        if not touched:
            QMessageBox.warning(self, "Move Failed", "Could not apply/write - nothing applied.")
            return
        if plan.id_map and self.dat_path:
            try:
                from apps.methods.master_ide import collect_ipl_paths_from_dat
                ipl_paths = collect_ipl_paths_from_dat(self.dat_path, game=self.game)
                cascade_ipl_files(ipl_paths, plan.id_map)
            except Exception:
                pass
        self._reload_after_edit()

    def _on_id_shift(self): #vers 2
        from apps.methods.id_shift_dialog import IDShiftDialog
        dlg = IDShiftDialog(self, self.result, game=self.game, dat_path=self.dat_path)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            self._reload_after_edit()

    def _on_add_id(self): #vers 2
        from apps.components.Master_Ide.id_tools_dialogs import AddIDDialog
        dlg = AddIDDialog(self, self.result, game=self.game, dat_path=self.dat_path)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            self._reload_after_edit()

    def _on_remove_delete_id(self): #vers 2
        from apps.components.Master_Ide.id_tools_dialogs import RemoveDeleteIDDialog
        dlg = RemoveDeleteIDDialog(self, self.result, game=self.game, dat_path=self.dat_path)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            self._reload_after_edit()

    def _on_id_utilities(self): #vers 2
        from apps.components.Master_Ide.id_tools_dialogs import IDUtilitiesDialog
        dlg = IDUtilitiesDialog(self, self.result, game=self.game, dat_path=self.dat_path)
        dlg.exec()
        self._reload_after_edit()

    def _on_insert_relocate(self): #vers 2
        from apps.components.Master_Ide.id_tools_dialogs import InsertRelocateDialog
        dlg = InsertRelocateDialog(self, self.result, game=self.game, dat_path=self.dat_path)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            self._reload_after_edit()

    def _on_txd_dedup(self): #vers 1
        from apps.components.Master_Ide.txd_dedup_dialog import TXDDedupDialog
        dlg = TXDDedupDialog(self, master_ide_result=self.result)
        dlg.exec()
        self._reload_after_edit()

    def _on_img_col_reorder(self): #vers 1
        from apps.components.Master_Ide.img_col_reorder_dialog import IMGColReorderDialog
        dlg = IMGColReorderDialog(self, self.result)
        dlg.exec()

    def _on_save(self): #vers 1
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
    """Small form for Insert Text - paste raw real IDE-format text,
    plus where it goes: appended to an already loaded real file, or
    saved as a brand-new one."""
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


def open_master_ide_workshop(main_window, ide_paths=None, dat_path=None, game=None): #vers 1
    """Entry point - real dual-mode pattern every workshop here uses
    (matches open_col_workshop's own shape): embeds as a tab if
    main_window has one, real standalone floating window otherwise,
    registers in the tool taskbar. Pass either ide_paths (a single
    path or list) or dat_path (resolves the whole game), not both."""
    try:
        if not main_window or not hasattr(main_window, 'main_tab_widget'):
            workshop = MasterIDEWorkshop(None, main_window)
            workshop.setWindowFlags(Qt.WindowType.Window)
            if dat_path:
                workshop.load_from_dat(dat_path)
            elif ide_paths:
                workshop.load_ide_paths(ide_paths, game=game)
            workshop.setWindowTitle("Master IDE")
            workshop.resize(1000, 650)
            workshop.show()
            return workshop

        tab_container = QWidget()
        tab_layout = QVBoxLayout(tab_container)
        tab_layout.setContentsMargins(0, 0, 0, 0)

        workshop = MasterIDEWorkshop(tab_container, main_window)
        workshop._tab_container = tab_container
        tab_layout.addWidget(workshop)

        if dat_path:
            workshop.load_from_dat(dat_path)
        elif ide_paths:
            workshop.load_ide_paths(ide_paths, game=game)

        tab_label = "Master IDE"
        try:
            from apps.methods.imgfactory_svg_icons import get_asset_checker_icon
            icon = get_asset_checker_icon()
            idx = main_window.main_tab_widget.addTab(tab_container, icon, tab_label)
        except Exception:
            idx = main_window.main_tab_widget.addTab(tab_container, tab_label)
        main_window.main_tab_widget.setCurrentIndex(idx)
        if hasattr(main_window, '_ensure_tab_area_visible'):
            main_window._ensure_tab_area_visible()

        _register_master_ide_taskbar(tab_container, main_window)
        return workshop
    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error opening Master IDE: {e}")
        return None


def _register_master_ide_taskbar(widget, main_window): #vers 1
    """Register or activate the Master IDE button in the real tool
    taskbar, matching Asset Checker's own convention."""
    try:
        tb = getattr(main_window, 'tool_taskbar', None)
        if not tb:
            return
        if 'master_ide' not in tb._tools:
            from apps.methods.imgfactory_svg_icons import get_asset_checker_icon
            icon = get_asset_checker_icon(16)
            tb.register('master_ide', 'Master IDE', icon, widget, 'Master IDE')
        else:
            tb._tools['master_ide']['target'] = widget
        if hasattr(tb, '_set_exclusive_active'):
            tb._set_exclusive_active('master_ide')
    except Exception:
        pass


if __name__ == "__main__": #vers 1
    """Standalone launcher (Sep 17 2026, per Keith: "make Master IDE
    standalone" - it had none before, unlike Asset Workshop's own).
    Opens genuinely empty, ready for its own Load from .dat/Insert
    IDE File buttons - no pre-loaded data assumed."""
    import sys
    import traceback
    from PyQt6.QtWidgets import QApplication

    print("Starting Master IDE")

    try:
        app = QApplication(sys.argv)
        workshop = MasterIDEWorkshop(None)
        workshop.setWindowTitle("Master IDE - Standalone")
        workshop.resize(1000, 650)
        workshop.show()
        sys.exit(app.exec())
    except Exception as e:
        traceback.print_exc()
        sys.exit(1)
