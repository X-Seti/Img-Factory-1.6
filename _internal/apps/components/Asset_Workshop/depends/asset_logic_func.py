#this belongs in apps/components/Asset_Workshop/depends/asset_logic_func.py - Version: 2
# X-Seti - October 29 2026 - IMG Factory 1.6 - Asset Workshop logic

"""
Asset Workshop logic: asset check, exclusions, cross reference, TXD / IMG updates.
Mixed into AssetWorkshop (asset_workshop.py) ahead of TXDWorkshop.
"""

##class AssetLogicMixin: -
# _all_checked_names
# _apply_exclusions
# _display_xtx_texture
# _exclusions_key
# _export_all_textures
# _export_selected_texture
# _export_text_report
# _get_selected_checker_names
# _load_exclusions
# _load_from_dat_path
# _on_exclude_selected
# _on_export_full_report
# _on_integrity_check
# _on_load_from_dat
# _on_manage_exclusions
# _on_master_ide
# _on_master_ide_ribbon
# _open_chk_file
# _open_txd_file
# _open_xtx_file
# _pick_target_img
# _real_diffs
# _reload_result
# _rename_texture
# _save_as_new_img
# _save_exclusions
# _show_detailed_info
# _show_texture_info
# _xref_add_missing_col
# _xref_add_missing_dff
# _xref_copy_cell
# _xref_copy_row
# _xref_open_txd_workshop
# copy_texture
# delete_texture
# duplicate_texture
# export_texture
# import_normal_texture
# import_textures
# load_result
# paste_texture
# refresh
# reload_texture_table
# save_as_txd_file
# show_help
# show_settings_dialog
# undo_last_action

import os
from PyQt6.QtWidgets import (QApplication, QWidget, QVBoxLayout,
    QHBoxLayout, QListWidget, QDialog, QListWidgetItem, QLabel, QPushButton, QFileDialog, QMessageBox,  QAbstractItemView, QInputDialog
)
from PyQt6.QtCore import Qt


from apps.methods.asset_checker import check_assets, find_game_asset_files


App_name = "Asset Workshop"


class AssetLogicMixin: #vers 1
    """Asset Workshop logic: asset check, exclusions, cross reference, TXD / IMG updates."""

    def load_result(self, result): #vers 4
        """Populate Asset Check with a real AssetCheckResult, building
        it into the right panel's own viewport on first use - ribbons
        stay visible, left panel stays disabled. Excluded names/files
        and the ID-range filter (Sep 17 2026, per Keith: "exclude
        files... img, col, ide should ignore them" / "start listing
        from a given ID") are stripped from the result right here,
        once, so every view/diff downstream (all pure functions of
        img_names/col_names/ide_names) simply never sees them.

        The FULL, never-filtered path lists are captured once on the
        first real load only (never overwritten by a later reload)
        - excluding a whole file re-runs check_assets() with that
        file's path dropped from the input list, which would
        otherwise permanently lose it from self.result.*_paths and
        make excluding it irreversible, and would also drift
        _exclusions_key() out from under any names/files already
        saved against the original full set."""
        self.result = result
        if not (self._all_img_paths or self._all_col_paths or self._all_ide_paths):
            self._all_img_paths = list(result.img_paths)
            self._all_col_paths = list(result.col_paths)
            self._all_ide_paths = list(result.ide_paths)
        self.excluded_names, self.excluded_files, self.ignore_id_min, self.ignore_id_max = \
            self._load_exclusions()
        self._apply_exclusions(self.result)
        if self._checker_tab is None:
            self._build_checker_tab()
        self._refresh_checker_summary()
        self._populate_columns_view()
        self._populate_merged_view()
        self._populate_cross_reference_view()
        self._viewport_stack.setCurrentWidget(self._checker_tab)
        all_checked = self._all_checked_names()
        if len(all_checked) > 3:
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(200, lambda: self._show_checked_files_popup(all_checked))

    def _all_checked_names(self): #vers 1
        """Every real checked filename, one per file."""
        names = []
        if self.result.img_path:
            if "," in self.result.img_path:
                names.extend(n.strip() for n in self.result.img_path.split(","))
            else:
                names.append(os.path.basename(self.result.img_path))
        if self.result.col_path:
            if "," in self.result.col_path:
                names.extend(n.strip() for n in self.result.col_path.split(","))
            else:
                names.append(os.path.basename(self.result.col_path))
        if self.result.ide_path:
            names.extend(n.strip() for n in self.result.ide_path.split(","))
        return names

    def _real_diffs(self, extra_count, missing_count, source_label): #vers 1
        diffs = []
        if extra_count:
            diffs.append((
                f"+{extra_count}",
                f"{extra_count} {source_label} entr{'y' if extra_count == 1 else 'ies'} "
                f"not declared anywhere in IDE",
                lambda: self._show_diff_popup(
                    self.result.img_extra_over_ide if source_label == "IMG" else self.result.col_extra_over_ide,
                    f"{source_label} entries not in IDE")))
        if missing_count:
            diffs.append((
                f"-{missing_count}",
                f"{missing_count} IDE entr{'y' if missing_count == 1 else 'ies'} "
                f"with no matching {source_label} file",
                lambda: self._show_diff_popup(
                    self.result.missing_from_img if source_label == "IMG" else self.result.missing_from_col,
                    f"IDE entries missing from {source_label}")))
        return diffs

    def _on_export_full_report(self): #vers 1
        """Export every real mismatch across IMG/COL/IDE to one
        plain text file."""
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Full Report", "asset_report.txt", "Text Files (*.txt)")
        if not path:
            return
        r = self.result
        lines = [f"Asset Workshop Report", f"Checked: {', '.join(self._all_checked_names())}", ""]

        def _section(title, names):
            lines.append(f"{title} ({len(names)}):")
            lines.extend(f"  {n}" for n in sorted(names))
            lines.append("")

        if r.img_path and r.ide_path:
            _section("IMG entries not declared in IDE", r.img_extra_over_ide)
            _section("IDE entries missing from IMG", r.missing_from_img)
        if r.col_path and r.ide_path:
            _section("COL entries not declared in IDE", r.col_extra_over_ide)
            _section("IDE entries missing from COL", r.missing_from_col)
        if r.ide_path:
            _section("Names not found in IDE at all", r.not_in_ide)

        try:
            with open(path, "w", encoding="utf-8", errors="ignore") as f:
                f.write("\n".join(lines))
            QMessageBox.information(self, "Export Full Report", f"Saved to:\n{path}")
        except Exception as e:
            QMessageBox.warning(self, "Export Failed", str(e))

    def _export_text_report(self, title: str, lines: list): #vers 1
        """Write a plain text report of the given entries."""
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Report", f"{title}.txt", "Text Files (*.txt)")
        if not path:
            return
        try:
            with open(path, "w", encoding="utf-8", errors="ignore") as f:
                f.write(f"{title}\n{len(lines)} entr{'y' if len(lines) == 1 else 'ies'}\n\n")
                f.write("\n".join(lines))
            QMessageBox.information(self, "Export Report", f"Saved to:\n{path}")
        except Exception as e:
            QMessageBox.warning(self, "Export Failed", str(e))

    def _on_master_ide_ribbon(self): #vers 1
        """Ribbon entry point for Master IDE - routes through the same
        checker view dropdown as the 4-Column/Merged/Cross-Reference
        views, so ribbon and dropdown stay in sync."""
        idx = self.checker_view_combo.findText("Master IDE...")
        if idx >= 0:
            self.checker_view_combo.setCurrentIndex(idx)
        else:
            self._on_master_ide()

    def _on_master_ide(self): #vers 2
        """Open Master IDE Workshop for the same real IDE file(s), or
        with nothing pre-loaded if Asset Workshop itself has no real
        IDE path yet (Sep 17 2026 - previously this passed [None] as
        ide_paths when standalone with nothing loaded, which loaded
        nothing useful anyway; opening it genuinely empty instead
        lets its own Load from .dat/Insert IDE File buttons work)."""
        from apps.components.Master_Ide.master_ide_workshop import open_master_ide_workshop
        paths = self.result.ide_paths or ([self.result.ide_path] if self.result.ide_path else None)
        open_master_ide_workshop(self.main_window, ide_paths=paths)

    def _on_load_from_dat(self): #vers 2
        """Load a whole game's real IMG/COL/IDE via its .dat, same
        resolution open_asset_workshop's own dat_path branch uses -
        needed standalone, where nothing else can hand this a path."""
        dat_path, _ = QFileDialog.getOpenFileName(
            self, "Load from .dat", "",
            "GTA DAT files (gta3.dat gta_vc.dat gta.dat gta_sol.dat "
            "gtasol.dat gta_quick.dat);;All files (*.dat)")
        if dat_path:
            self._load_from_dat_path(dat_path)

    def _load_from_dat_path(self, dat_path): #vers 1
        """Check a whole game from its .dat file."""
        img_path, col_path, ide_paths, game = find_game_asset_files(
            dat_path, auto_find_gta3_img=getattr(self, 'auto_find_gta3_img', True))
        if not img_path and not col_path and not ide_paths:
            QMessageBox.warning(self, App_name,
                f"Could not find any real IMG/COL/IDE files from:\n{dat_path}")
            return
        result = check_assets(img_path=img_path, col_path=col_path, ide_path=ide_paths, game=game)
        self._all_img_paths, self._all_col_paths, self._all_ide_paths = [], [], []   # new game - recapture
        self.dat_path, self._dat_game = dat_path, game
        self.load_result(result)

    def _on_integrity_check(self): #vers 2
        """Read-only consistency report: orphaned IPL/2dfx/path, duplicate
        IDs/names, TXD usage (info only), ID gaps. Uses ALL ide files
        (ignoring Exclude Files) so nothing shows as falsely orphaned."""
        from apps.methods.master_ide import load_master_ide, collect_ipl_paths_from_dat
        from apps.methods.asset_integrity import check_integrity, format_report, show_integrity_dialog
        r = self.result
        ides = list(self._all_ide_paths or (r.ide_paths if r else [])
                    or ([r.ide_path] if r and r.ide_path else []))
        if not ides:
            QMessageBox.information(self, App_name, "No IDE files loaded.")
            return
        dat = getattr(self, 'dat_path', None)
        game = getattr(self, '_dat_game', None)
        if dat:
            ipls = collect_ipl_paths_from_dat(dat, game=game)
        else:
            ipls = [os.path.splitext(p)[0] + ext for p in ides for ext in (".ipl", ".IPL")
                    if os.path.isfile(os.path.splitext(p)[0] + ext)]
        rep = check_integrity(load_master_ide(ides, game=game), ipls)
        show_integrity_dialog(self, format_report(rep))

    def _xref_copy_cell(self, item): #vers 1
        QApplication.clipboard().setText(item.text())

    def _xref_copy_row(self, row): #vers 1
        values = [self.xref_table.item(row, c).text() if self.xref_table.item(row, c) else ""
                   for c in range(self.xref_table.columnCount())]
        QApplication.clipboard().setText("\t".join(values))

    def _xref_open_txd_workshop(self): #vers 1
        """Open the real TXD Workshop (not the local duplicate) for
        the target IMG holding the missing texture."""
        try:
            from apps.components.Txd_Editor.txd_workshop import open_txd_workshop as _open_real_txd_workshop
            target = self._pick_target_img()
            if target:
                _open_real_txd_workshop(self.main_window, target)
        except Exception:
            pass

    def _pick_target_img(self): #vers 1
        """Resolve which real loaded IMG a write should target - ask
        explicitly when several are loaded (e.g. SOL's own
        game_vc.img/game_sa.img/etc)."""
        paths = self.result.img_paths
        if not paths:
            return None
        if len(paths) == 1:
            return paths[0]
        names = [os.path.basename(p) for p in paths]
        name, ok = QInputDialog.getItem(
            self, "Select Target IMG", "Multiple real IMG archives are loaded - add to which one?",
            names, 0, False)
        if not ok or not name:
            return None
        return next((p for p in paths if os.path.basename(p) == name), None)

    def _xref_add_missing_dff(self, model_name): #vers 1
        """Browse to a real external .dff and add it to the target
        IMG under this exact model name."""
        target = self._pick_target_img()
        if not target:
            return
        path, _ = QFileDialog.getOpenFileName(
            self, f"Select external DFF for {model_name}", "", "DFF Files (*.dff)")
        if not path:
            return
        try:
            from apps.methods.img_core_classes import IMGFile
            with open(path, "rb") as f:
                data = f.read()
            img_file = IMGFile(target)
            if not img_file.open():
                QMessageBox.warning(self, "Add File Failed", f"Could not open:\n{target}")
                return
            if not img_file.add_entry(f"{model_name}.dff", data):
                QMessageBox.warning(self, "Add File Failed",
                    "add_entry() (including its own save) returned False.")
                return
        except Exception as e:
            QMessageBox.warning(self, "Add File Failed", str(e))
            return
        self._reload_result()

    def _xref_add_missing_col(self, model_name): #vers 1
        """Browse to a real external .col and merge its model(s)
        into the currently loaded standalone col file. Embedded COL
        data inside an IMG isn't supported here yet."""
        if not self.result.col_paths:
            QMessageBox.information(self, "Add File Failed",
                "This game's real COL data is embedded inside the IMG itself, "
                "not a standalone .col file - adding to embedded IMG collision "
                "data isn't supported yet.")
            return
        standalone_path = self.result.col_paths[0]

        path, _ = QFileDialog.getOpenFileName(
            self, f"Select external COL for {model_name}", "", "COL Files (*.col)")
        if not path:
            return
        try:
            from apps.methods.col_workshop_loader import COLFile
            from apps.methods.file_backup import backup_file

            external = COLFile()
            if not external.load_from_file(path):
                QMessageBox.warning(self, "Add File Failed", f"Could not parse:\n{path}")
                return
            if not external.models:
                QMessageBox.warning(self, "Add File Failed", "External COL has no real models.")
                return

            target = COLFile()
            target.load_from_file(standalone_path)   # OK if this is empty/new
            target.models.extend(external.models)

            if backup_file(standalone_path) is None:
                QMessageBox.warning(self, "Add File Failed",
                    f"Could not back up before writing:\n{standalone_path}")
                return
            if not target.save_to_file(standalone_path):
                QMessageBox.warning(self, "Add File Failed", "COLFile.save_to_file() returned False.")
                return
        except Exception as e:
            QMessageBox.warning(self, "Add File Failed", str(e))
            return
        self._reload_result()

    def _reload_result(self): #vers 2
        """Re-run check_assets against the FULL original path lists
        (minus any excluded whole files) and repopulate every view."""
        excluded = {f.lower() for f in self.excluded_files}

        def _filt(paths):
            kept = [p for p in paths if os.path.basename(p).lower() not in excluded]
            return kept or None

        result = check_assets(img_path=_filt(self._all_img_paths),
                               col_path=_filt(self._all_col_paths),
                               ide_path=_filt(self._all_ide_paths))
        self.load_result(result)

    def _apply_exclusions(self, result): #vers 2
        """Strip excluded names, plus any name whose IDE ID falls
        outside the ID range, from the three base sets every other
        view/diff on AssetCheckResult is a pure function of. Names
        with no IDE ID (IMG/COL only) are unaffected by the ID range."""
        to_exclude = set(self.excluded_names)
        if self.ignore_id_min is not None or self.ignore_id_max is not None:
            lo = self.ignore_id_min if self.ignore_id_min is not None else float('-inf')
            hi = self.ignore_id_max if self.ignore_id_max is not None else float('inf')
            for name, mid in result.ide_id_by_name.items():
                if not (lo <= mid <= hi):
                    to_exclude.add(name)
        if not to_exclude:
            return
        result.img_names -= to_exclude
        result.col_names -= to_exclude
        result.ide_names -= to_exclude
        for name in to_exclude:
            result.ide_id_by_name.pop(name, None)
            result.ide_txd_by_name.pop(name, None)
            result.ide_txd_display_by_name.pop(name, None)

    def _exclusions_key(self): #vers 2
        """Stable key for the ORIGINAL full img/col/ide file set, so
        exclusions never bleed between projects and never drift when
        a whole file is excluded."""
        import hashlib
        paths = (getattr(self, '_all_img_paths', None) or self.result.img_paths or []) + \
                (getattr(self, '_all_col_paths', None) or self.result.col_paths or []) + \
                (getattr(self, '_all_ide_paths', None) or self.result.ide_paths or [])
        names = sorted(set(os.path.basename(p) for p in paths))
        if not names:
            return "default"
        return hashlib.md5(",".join(names).encode()).hexdigest()[:12]

    def _load_exclusions(self): #vers 1
        """Returns (names, files, id_min, id_max)."""
        try:
            import json
            data = json.loads(self._config_path().read_text())
            entry = data.get('excluded_names', {}).get(self._exclusions_key())
            if not entry:
                return set(), set(), None, None
            return (set(entry.get('names', [])), set(entry.get('excluded_files', [])),
                    entry.get('id_min'), entry.get('id_max'))
        except Exception:
            return set(), set(), None, None

    def _save_exclusions(self): #vers 1
        try:
            import json
            path = self._config_path()
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            all_excl = data.setdefault('excluded_names', {})
            key = self._exclusions_key()
            if (self.excluded_names or self.excluded_files
                    or self.ignore_id_min is not None or self.ignore_id_max is not None):
                files = sorted(set(os.path.basename(p) for p in
                                   self._all_img_paths + self._all_col_paths + self._all_ide_paths))
                all_excl[key] = {
                    'names': sorted(self.excluded_names),
                    'excluded_files': sorted(self.excluded_files),
                    'id_min': self.ignore_id_min, 'id_max': self.ignore_id_max,
                    'files': files,
                }
            else:
                all_excl.pop(key, None)
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(json.dumps(data, indent=2))
        except Exception:
            pass

    def _get_selected_checker_names(self): #vers 1
        """Every real name currently selected across the checker's
        own list/table views - id_list resolves via ide_list's own
        same-index pairing (both built from the same sorted_ide_names
        loop); xref_table resolves via self.result.all_names since
        its own "IDE Model Name" column is blank for anything not
        declared in IDE."""
        names = set()
        for lst in (getattr(self, 'ide_list', None), getattr(self, 'img_list', None),
                    getattr(self, 'col_list', None)):
            if lst is None:
                continue
            for item in lst.selectedItems():
                text = item.text().strip()
                if text:
                    names.add(text.lower())
        id_list = getattr(self, 'id_list', None)
        ide_list = getattr(self, 'ide_list', None)
        if id_list is not None and ide_list is not None:
            for item in id_list.selectedItems():
                row = id_list.row(item)
                paired = ide_list.item(row)
                if paired and paired.text().strip():
                    names.add(paired.text().strip().lower())
        merged_table = getattr(self, 'merged_table', None)
        if merged_table is not None:
            for idx in merged_table.selectionModel().selectedRows():
                item = merged_table.item(idx.row(), 0)
                if item and item.text().strip():
                    names.add(item.text().strip().lower())
        xref_table = getattr(self, 'xref_table', None)
        if xref_table is not None and self.result:
            all_names = self.result.all_names
            for idx in xref_table.selectionModel().selectedRows():
                row = idx.row()
                if 0 <= row < len(all_names):
                    names.add(all_names[row])
        return names

    def _on_exclude_selected(self): #vers 1
        """Exclude every real name currently selected in any checker
        view - it stops showing anywhere and stops being counted as
        missing/extra, and the exclusion persists across sessions
        for this same img/col/ide file set."""
        names = self._get_selected_checker_names()
        if not names:
            QMessageBox.information(self, "Exclude Files",
                "Select one or more entries in the 4-Column, Merged, or "
                "Cross-Reference view first.")
            return
        self.excluded_names |= names
        self._save_exclusions()
        self._reload_result()
        self._set_status(f"Excluded {len(names)} name(s)")

    def _on_manage_exclusions(self): #vers 2
        """Names / Files / ID Range tabs. Files excludes a whole loaded
        .ide/.img/.col (e.g. vehicles.ide, peds.ide, weapons.ide);
        ID Range hides IDE entries outside [from, to] - leave a side
        unchecked for open-ended ("show everything after ID 300")."""
        from PyQt6.QtWidgets import QTabWidget, QCheckBox, QSpinBox
        dialog = QDialog(self)
        dialog.setWindowTitle("Manage Exclusions")
        dialog.resize(420, 460)
        layout = QVBoxLayout(dialog)
        tabs = QTabWidget()
        layout.addWidget(tabs)

        # Names
        names_tab = QWidget()
        nl = QVBoxLayout(names_tab)
        nl.addWidget(QLabel(f"{len(self.excluded_names)} name(s) excluded:"))
        names_lst = QListWidget()
        names_lst.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        names_lst.addItems(sorted(self.excluded_names))
        nl.addWidget(names_lst)
        rm_btn = QPushButton("Remove Selected")

        def _remove_names(): #vers 1
            for item in names_lst.selectedItems():
                self.excluded_names.discard(item.text())
                names_lst.takeItem(names_lst.row(item))
        rm_btn.clicked.connect(_remove_names)
        nl.addWidget(rm_btn)
        tabs.addTab(names_tab, "Names")

        # Files
        files_tab = QWidget()
        fl = QVBoxLayout(files_tab)
        fl.addWidget(QLabel("Checked files are excluded from every view:"))
        files_lst = QListWidget()
        all_paths = self._all_ide_paths + self._all_img_paths + self._all_col_paths
        for base in sorted({os.path.basename(p) for p in all_paths}, key=str.lower):
            item = QListWidgetItem(base)
            item.setFlags(item.flags() | Qt.ItemFlag.ItemIsUserCheckable)
            item.setCheckState(Qt.CheckState.Checked if base.lower() in
                               {f.lower() for f in self.excluded_files} else Qt.CheckState.Unchecked)
            files_lst.addItem(item)
        fl.addWidget(files_lst)
        tabs.addTab(files_tab, "Files")

        # ID range
        id_tab = QWidget()
        il = QVBoxLayout(id_tab)
        il.addWidget(QLabel("Only list IDE entries inside this ID range:"))
        min_row = QHBoxLayout()
        min_chk = QCheckBox("From ID")
        min_chk.setChecked(self.ignore_id_min is not None)
        min_spin = QSpinBox(); min_spin.setRange(0, 999999)
        min_spin.setValue(self.ignore_id_min if self.ignore_id_min is not None else 0)
        min_row.addWidget(min_chk); min_row.addWidget(min_spin); min_row.addStretch()
        il.addLayout(min_row)
        max_row = QHBoxLayout()
        max_chk = QCheckBox("To ID")
        max_chk.setChecked(self.ignore_id_max is not None)
        max_spin = QSpinBox(); max_spin.setRange(0, 999999)
        max_spin.setValue(self.ignore_id_max if self.ignore_id_max is not None else 32767)
        max_row.addWidget(max_chk); max_row.addWidget(max_spin); max_row.addStretch()
        il.addLayout(max_row)
        il.addWidget(QLabel("Entries with no IDE ID (IMG/COL only) are unaffected."))
        il.addStretch()
        tabs.addTab(id_tab, "ID Range")

        btn_row = QHBoxLayout()
        clear_btn = QPushButton("Clear All")

        def _clear_all(): #vers 1
            self.excluded_names.clear()
            self.excluded_files.clear()
            self.ignore_id_min = self.ignore_id_max = None
            self._save_exclusions()
            self._reload_result()
            dialog.accept()
        clear_btn.clicked.connect(_clear_all)
        btn_row.addWidget(clear_btn)
        btn_row.addStretch()
        apply_btn = QPushButton("Apply")

        def _apply(): #vers 1
            self.excluded_files = {files_lst.item(i).text().lower()
                                   for i in range(files_lst.count())
                                   if files_lst.item(i).checkState() == Qt.CheckState.Checked}
            self.ignore_id_min = min_spin.value() if min_chk.isChecked() else None
            self.ignore_id_max = max_spin.value() if max_chk.isChecked() else None
            if (self.ignore_id_min is not None and self.ignore_id_max is not None
                    and self.ignore_id_min > self.ignore_id_max):
                QMessageBox.warning(dialog, "Manage Exclusions", "'From' must not exceed 'To'.")
                return
            self._save_exclusions()
            self._reload_result()
            dialog.accept()
        apply_btn.clicked.connect(_apply)
        btn_row.addWidget(apply_btn)
        close_btn = QPushButton("Cancel")
        close_btn.clicked.connect(lambda: (setattr(self, 'excluded_names', self._load_exclusions()[0]), dialog.reject()))
        btn_row.addWidget(close_btn)
        layout.addLayout(btn_row)
        dialog.exec()

    def _save_as_new_img(self, new_txd_data): #vers 1
        """Save as new IMG file when rebuild is needed"""
        try:
            file_path, _ = QFileDialog.getSaveFileName(
                self, "Save IMG with Large Textures",
                self.current_img.file_path.replace('.img', '_hd.img'),
                "IMG Files (*.img);;All Files (*)"
            )

            if file_path:
                # Update TXD data
                for entry in self.current_img.entries:
                    if entry.name == self.current_txd_name:
                        entry.data = new_txd_data
                        entry.size = len(new_txd_data)
                        break

                # Save as new file
                self.current_img.save_as(file_path)

                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Saved as new IMG: {file_path}")

                return True

            return False

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Save as new error: {str(e)}")
            return False

    def _export_all_textures(self, *a, **kw): return self.export_all_textures(*a, **kw)  #vers 1

    def _export_selected_texture(self, *a, **kw): return self.export_selected_texture(*a, **kw)  #vers 1

    def _open_txd_file(self, *a, **kw): return self.open_txd_file(*a, **kw)  #vers 3

    def copy_texture(self, *a, **kw): return self._copy_texture(*a, **kw)  #vers 1

    def delete_texture(self, *a, **kw): return self._delete_texture(*a, **kw)  #vers 1

    def duplicate_texture(self, *a, **kw): return self._duplicate_texture(*a, **kw)  #vers 1

    def export_texture(self, *a, **kw): return self.export_selected_texture(*a, **kw)  #vers 1

    def import_normal_texture(self, *a, **kw): return self._import_textures(*a, **kw)  #vers 1

    def import_textures(self, *a, **kw): return self._import_textures(*a, **kw)  #vers 1

    def paste_texture(self, *a, **kw): return self._paste_texture(*a, **kw)  #vers 1

    def refresh(self, *a, **kw): return self._reload_texture_table(*a, **kw)  #vers 1

    def reload_texture_table(self, *a, **kw): return self._reload_texture_table(*a, **kw)  #vers 1

    def save_as_txd_file(self, *a, **kw): return self._save_as_txd_file(*a, **kw)  #vers 1

    def undo_last_action(self, *a, **kw): return self._undo_last_action(*a, **kw)  #vers 1

    def _show_detailed_info(self, *a, **kw): pass  #vers 1

    def _show_texture_info(self, *a, **kw): pass  #vers 1

    def show_help(self, *a, **kw): pass  #vers 1

    def show_settings_dialog(self, *a, **kw): pass  #vers 1

    def _rename_texture(self, alpha=False): #vers 2
        """Rename texture or alpha name and mark as modified"""
        from PyQt6.QtWidgets import QInputDialog

        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        current_name = self.selected_texture.get('name', 'texture')

        if alpha:
            if not self.selected_texture.get('has_alpha', False):
                QMessageBox.information(self, "No Alpha", "This texture does not have an alpha channel")
                return

            alpha_name = self.selected_texture.get('alpha_name', current_name + 'a')
            new_name, ok = QInputDialog.getText(self, "Rename Alpha", "Enter alpha name:", text=alpha_name)
            if ok and new_name and new_name != alpha_name:
                self._save_undo_state("Rename alpha")
                self.selected_texture['alpha_name'] = new_name
                if hasattr(self, 'info_alpha_name'):
                    self.info_alpha_name.setText(f"Alpha: {new_name}")
                self._update_table_display()
                self._mark_as_modified()  # Mark as modified
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Alpha renamed to: {new_name}")
        else:
            new_name, ok = QInputDialog.getText(self, "Rename Texture", "Enter texture name:", text=current_name)
            if ok and new_name and new_name != current_name:
                self._save_undo_state("Rename texture")
                self.selected_texture['name'] = new_name
                if hasattr(self, 'info_name'):
                    self.info_name.setText(f"Name: {new_name}")
                self._update_table_display()
                self._mark_as_modified()  # Mark as modified
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Texture renamed: {current_name} -> {new_name}")

    def _open_xtx_file(self, file_path: str): #vers 1
        """Open a VCS PS2/PC XTX palettized texture and display it in the workshop."""
        try:
            from apps.methods.xtx_reader import read_xtx, xtx_to_qimage
            info = read_xtx(file_path)
            if info['error']:
                QMessageBox.warning(self, "XTX Error",
                    f"Failed to read XTX file:\n{info['error']}")
                return

            name = os.path.basename(file_path)
            w, h = info['width'], info['height']

            # Build a fake texture list entry compatible with the existing display
            qimg = xtx_to_qimage(file_path)
            if qimg is None:
                QMessageBox.warning(self, "XTX Error", "Could not decode XTX texture")
                return

            from PyQt6.QtGui import QPixmap
            pixmap = QPixmap.fromImage(qimg)

            # Display in the texture preview
            self._display_xtx_texture(name, pixmap, info)

            self.current_txd_path = file_path
            self.current_txd_name = name
            self.setWindowTitle(App_name + f": {name} [XTX — VCS PS2 Palettized {w}×{h}]")
            self._log(f"Opened XTX: {name} ({w}x{h}, 256-colour indexed)")

        except Exception as e:
            QMessageBox.critical(self, "XTX Error", f"Failed to open XTX:\n{str(e)}")

    def _open_chk_file(self, file_path: str): #vers 3
        """Open a GTA III PS2 CHK splash texture file."""
        try:
            from apps.methods.chk_parser import load_chk
            tex = load_chk(file_path)
            if not tex:
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.warning(self, "CHK Error", f"Failed to parse CHK file:\n{file_path}")
                return

            name = tex['name']
            self.setWindowTitle(App_name + f": {name}.CHK [{tex['width']}×{tex['height']}]")
            self._set_status(f"Opened CHK: {name}  "
                             f"{tex['width']}×{tex['height']}  8bpp palettised")

            # Display as a single-texture list
            self.texture_list = [tex]
            if hasattr(self, 'texture_table'):
                self.texture_table.setRowCount(0)

            self._add_texture_to_table(tex)
            self.selected_texture = tex
            self._update_texture_panel_visibility()

            # Selecting the row drives the normal preview/info path
            if hasattr(self, 'texture_table') and self.texture_table.rowCount():
                self.texture_table.selectRow(0)
                self._on_texture_selected()

        except Exception as e:
            import traceback; traceback.print_exc()
            print(f"[{App_name}] CHK error: {e}")

    def _display_xtx_texture(self, name: str, pixmap, info: dict): #vers 1
        """Show an XTX texture in the workshop preview area."""
        try:
            w, h = info['width'], info['height']

            # Clear existing texture list and add XTX entry
            if hasattr(self, 'txd_list_widget'):
                self.txd_list_widget.clear()
                item = QListWidgetItem(
                    f"{name}  [{w}×{h}]  XTX/PSMT8-256col"
                )
                self.txd_list_widget.addItem(item)
                self.txd_list_widget.setCurrentRow(0)

            # Show in preview
            if hasattr(self, 'texture_preview'):
                scaled = pixmap.scaled(
                    self.texture_preview.width(),
                    self.texture_preview.height(),
                    Qt.AspectRatioMode.KeepAspectRatio,
                    Qt.TransformationMode.SmoothTransformation,
                )
                self.texture_preview.setPixmap(scaled)

            # Update info labels if present
            for attr, val in [
                ('texture_name_label',   name),
                ('texture_size_label',   f"{w} × {h} pixels"),
                ('texture_format_label', "PSMT8 (8-bit palette-indexed)"),
                ('texture_depth_label',  "8 bpp → 256 colours"),
                ('texture_alpha_label',  "Yes (PS2 alpha 0–128 range)"),
            ]:
                lbl = getattr(self, attr, None)
                if lbl and hasattr(lbl, 'setText'):
                    lbl.setText(val)

            # Store for potential export
            self._xtx_pixmap = pixmap
            self._xtx_info   = info

        except Exception as e:
            self._log(f"XTX display error: {e}")
