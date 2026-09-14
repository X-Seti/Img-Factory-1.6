#this belongs in apps/components/Asset_Workshop/asset_workshop.py - Version: 4
# X-Seti - September 12 2026 - IMG Factory 1.6 - Asset Workshop

"""asset_workshop.py - Asset Checker."""

##Methods list -
# AssetWorkshop
# open_asset_workshop

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QListWidget, QTableWidget,
    QTableWidgetItem, QStackedWidget, QComboBox, QSplitter, QWidget, QMenu,
    QMessageBox, QPushButton,
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor

from apps.methods.asset_checker import check_assets, find_sibling_asset_files, find_game_asset_files
from apps.components.Asset_Workshop.dockable_toolbar import DockableToolbar


class AssetWorkshop(QWidget): #vers 4
    def __init__(self, parent, main_window=None): #vers 1
        super().__init__(parent)
        self.main_window = main_window
        self.result = None
        self._tab_container = None
        self._build_ui()

    def load_result(self, result): #vers 1
        """Populate the workshop with an already-computed real
        AssetCheckResult (Sep 12 2026 - construct-then-load, matching
        Master IDE Workshop's own convention)."""
        self.result = result
        self._refresh_summary()
        self._populate_columns_view()
        self._populate_merged_view()
        self._populate_cross_reference_view()
        all_checked = self._all_checked_names()
        if len(all_checked) > 3:
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(200, lambda: self._show_checked_files_popup(all_checked))

    def _all_checked_names(self): #vers 2
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

    def _show_checked_files_popup(self, names): #vers 1
        """Small non-modal popup listing every checked file, one per
        line, auto-closing after 5 seconds."""
        from PyQt6.QtCore import QTimer
        popup = QDialog(self)
        popup.setWindowTitle(f"Checked files ({len(names)})")
        v = QVBoxLayout(popup)
        lst = QListWidget()
        lst.addItems(names)
        v.addWidget(lst)
        popup.resize(360, 400)
        popup.setModal(False)
        popup.show()
        QTimer.singleShot(5000, popup.close)

    def _build_ui(self): #vers 1
        self._lay = QVBoxLayout(self)

        self.toolbar = DockableToolbar(self, self, settings_key='asset_workshop_toolbar_layout')
        toolbar_bar = QWidget()
        self._top = QHBoxLayout(toolbar_bar)
        self._top.setContentsMargins(2, 2, 2, 2)
        self.toolbar.set_content(toolbar_bar)
        self._lay.addWidget(self.toolbar)

        self.stack = QStackedWidget()
        self._lay.addWidget(self.stack, 1)

        columns_widget = QWidget()
        columns_lay = QHBoxLayout(columns_widget)
        splitter = QSplitter(Qt.Orientation.Horizontal)
        columns_lay.addWidget(splitter)

        self.id_list = self._make_column(splitter, "ID", None)
        self.ide_list = self._make_column(splitter, "IDE entry list", None)
        self.img_list = self._make_column(splitter, "IMG archive", None)
        self.col_list = self._make_column(splitter, "COL archive", None)
        self.error_list = self._make_column(splitter, "Error list", None)
        self.stack.addWidget(columns_widget)

        self._sync_lists = [self.id_list, self.ide_list, self.img_list, self.col_list]
        self._sync_guard = False
        for lst in self._sync_lists:
            lst.verticalScrollBar().valueChanged.connect(self._on_sync_scroll)

        self.merged_table = QTableWidget()
        self.merged_table.setColumnCount(3)
        self.merged_table.setHorizontalHeaderLabels(["Model Name", "Source", "Status"])
        self.merged_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self.merged_table.horizontalHeader().setStretchLastSection(True)
        self.stack.addWidget(self.merged_table)

        self.xref_table = QTableWidget()
        self.xref_table.setColumnCount(6)
        self.xref_table.setHorizontalHeaderLabels(
            ["ID", "DFF", "COL", "IDE Model Name", "Texture entry", "Errors"])
        self.xref_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self.xref_table.horizontalHeader().setStretchLastSection(True)
        self.xref_table.setSortingEnabled(True)
        self.xref_table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.xref_table.customContextMenuRequested.connect(self._xref_context_menu)
        self.stack.addWidget(self.xref_table)

    def _refresh_summary(self): #vers 1
        """Rebuild the toolbar's own content row - real summary +
        view selector + action buttons, recomputed against
        self.result each time (Sep 12 2026, adapted from the old
        modal dialog's one-time _build_ui row into a refreshable
        toolbar row)."""
        while self._top.count():
            item = self._top.takeAt(0)
            if item.widget():
                item.widget().deleteLater()

        all_checked = self._all_checked_names()
        if not all_checked:
            summary = "No sibling files found"
        elif len(all_checked) <= 3:
            summary = "Checked: " + ", ".join(all_checked)
        else:
            summary = (f"Checked: {', '.join(all_checked[:2])} "
                       f"+{len(all_checked) - 2} more")
        self._top.addWidget(QLabel(summary))
        if len(all_checked) > 3:
            show_files_btn = QPushButton("Show list")
            show_files_btn.clicked.connect(
                lambda: self._show_checked_files_popup(all_checked))
            self._top.addWidget(show_files_btn)
        self._top.addStretch()
        self._top.addWidget(QLabel("View:"))
        self.view_combo = QComboBox()
        self.view_combo.addItems(["4-Column View", "Merged View", "Cross-Reference Table"])
        self.view_combo.currentIndexChanged.connect(self._on_view_changed)
        self._top.addWidget(self.view_combo)
        if self.result.ide_path:
            master_ide_btn = QPushButton("Master IDE...")
            master_ide_btn.clicked.connect(self._on_master_ide)
            self._top.addWidget(master_ide_btn)

        ide_count = len(self.result.ide_names)
        img_extra_count = len(self.result.img_extra_over_ide) if self.result.img_path and self.result.ide_path else 0
        img_missing_count = len(self.result.missing_from_img) if self.result.img_path and self.result.ide_path else 0
        col_extra_count = len(self.result.col_extra_over_ide) if self.result.col_path and self.result.ide_path else 0
        col_missing_count = len(self.result.missing_from_col) if self.result.col_path and self.result.ide_path else 0
        self._set_column_header(self.ide_list, "IDE entry list", ide_count)
        self._set_column_header(self.img_list, "IMG archive", ide_count,
            diffs=self._real_diffs(img_extra_count, img_missing_count, "IMG"))
        self._set_column_header(self.col_list, "COL archive", ide_count,
            diffs=self._real_diffs(col_extra_count, col_missing_count, "COL"))

    def _set_column_header(self, lst, title, count, diffs=None): #vers 1
        """Rebuild one column's own header row (label + diff
        buttons) - stored on the list widget itself at creation
        time (see _make_column) so it can be refreshed here."""
        header_lay = lst.property("header_layout")
        if header_lay is None:
            return
        while header_lay.count():
            item = header_lay.takeAt(0)
            if item.widget():
                item.widget().deleteLater()
        label_text = title if count is None else f"{title} ({count})"
        header_lbl = QLabel(label_text)
        bright = self.palette().color(self.palette().currentColorGroup(),
                                       self.palette().ColorRole.BrightText)
        header_lbl.setStyleSheet(f"color: {bright.name()}; font-weight: bold;")
        header_lay.addWidget(header_lbl)
        for label, tooltip, on_click in (diffs or []):
            diff_btn = QPushButton(label)
            diff_btn.setFlat(True)
            diff_btn.setCursor(Qt.CursorShape.PointingHandCursor)
            diff_btn.setStyleSheet(
                "text-decoration: underline; padding: 0px 3px; font-size: 11px;")
            diff_btn.setMaximumWidth(diff_btn.fontMetrics().horizontalAdvance(label) + 10)
            diff_btn.setToolTip(tooltip)
            if on_click:
                diff_btn.clicked.connect(on_click)
            header_lay.addWidget(diff_btn)
        header_lay.addStretch()

    def _make_column(self, splitter, title, count, diffs=None): #vers 1
        container = QWidget()
        container.setMinimumWidth(150)
        v = QVBoxLayout(container)
        v.setContentsMargins(2, 2, 2, 2)
        header_row = QHBoxLayout()
        v.addLayout(header_row)
        lst = QListWidget()
        lst.setAlternatingRowColors(True)
        lst.setProperty("header_layout", header_row)
        v.addWidget(lst)
        splitter.addWidget(container)
        self._set_column_header(lst, title, count, diffs)
        return lst

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

    def _on_sync_scroll(self, value): #vers 1
        if self._sync_guard:
            return
        self._sync_guard = True
        try:
            for lst in self._sync_lists:
                if lst.verticalScrollBar().value() != value:
                    lst.verticalScrollBar().setValue(value)
        finally:
            self._sync_guard = False

    def _show_diff_popup(self, names, title): #vers 1
        from PyQt6.QtWidgets import QApplication, QHBoxLayout as _QHBoxLayout
        dlg = QDialog(self)
        dlg.setWindowTitle(title)
        dlg.resize(360, 400)
        v = QVBoxLayout(dlg)
        v.addWidget(QLabel(f"{len(names)} entr{'y' if len(names) == 1 else 'ies'}:"))
        lst = QListWidget()
        lst.addItems(sorted(names))
        v.addWidget(lst)
        btn_row = _QHBoxLayout()
        copy_one_btn = QPushButton("Copy Selected")
        copy_one_btn.clicked.connect(
            lambda: QApplication.clipboard().setText(
                lst.currentItem().text() if lst.currentItem() else ""))
        copy_all_btn = QPushButton("Copy All")
        copy_all_btn.clicked.connect(
            lambda: QApplication.clipboard().setText("\n".join(sorted(names))))
        btn_row.addWidget(copy_one_btn)
        btn_row.addWidget(copy_all_btn)
        btn_row.addStretch()
        v.addLayout(btn_row)
        dlg.exec()

    def _on_view_changed(self, index): #vers 1
        self.stack.setCurrentIndex(index)

    def _on_master_ide(self): #vers 1
        """Open Master IDE Workshop for the same real IDE file(s)."""
        from apps.components.Master_Ide.master_ide_workshop import open_master_ide_workshop
        paths = self.result.ide_paths or [self.result.ide_path]
        open_master_ide_workshop(self.main_window, ide_paths=paths)

    def _populate_columns_view(self): #vers 1
        r = self.result

        def _ide_order_key(name):
            return (r.ide_id_by_name.get(name, float('inf')), name)
        self.img_list.clear()
        self.col_list.clear()
        self.ide_list.clear()
        self.id_list.clear()
        self.error_list.clear()
        self.img_list.addItems(sorted(r.img_names, key=_ide_order_key))
        self.col_list.addItems(sorted(r.col_names, key=_ide_order_key))
        sorted_ide_names = sorted(r.ide_names, key=lambda n: r.ide_id_by_name.get(n, 0))
        self.ide_list.addItems(sorted_ide_names)
        self.id_list.addItems(str(r.ide_id_by_name.get(name, "")) for name in sorted_ide_names)

        errors = []
        for name in sorted(r.missing_from_col):
            errors.append(f"Missing in COL: {name}")
        for name in sorted(r.missing_from_img):
            errors.append(f"Missing in IMG: {name}")
        for name in sorted(r.not_in_ide):
            errors.append(f"Not found in IDE: {name}")
        if not errors:
            errors.append("No mismatches found among the checked sources.")
        self.error_list.addItems(errors)

    def _populate_merged_view(self): #vers 1
        base = self.palette().color(self.palette().currentColorGroup(),
                                     self.palette().ColorRole.Base)
        shades = {
            'IMG': base.lighter(112),
            'COL': base,
            'IDE': base.darker(108),
        }
        rows = []
        for name in self.result.all_names:
            if name in self.result.img_names:
                rows.append((name, 'IMG', self.result.status_for(name)))
            if name in self.result.col_names:
                rows.append((name, 'COL', self.result.status_for(name)))
            if name in self.result.ide_names:
                rows.append((name, 'IDE', self.result.status_for(name)))

        self.merged_table.setRowCount(len(rows))
        for row, (name, source, status) in enumerate(rows):
            for col, val in enumerate([name, source, status]):
                item = QTableWidgetItem(val)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                item.setBackground(shades[source])
                self.merged_table.setItem(row, col, item)

    def _populate_cross_reference_view(self): #vers 1
        base = self.palette().color(self.palette().currentColorGroup(),
                                     self.palette().ColorRole.Base)
        error_tint = QColor(
            min(255, base.red() + 40), max(0, base.green() - 25), max(0, base.blue() - 25))

        rows = self.result.cross_reference_rows()
        self.xref_table.setSortingEnabled(False)
        self.xref_table.setRowCount(len(rows))
        for row, values in enumerate(rows):
            has_error = values[5] != "OK"
            for col, val in enumerate(values):
                item = QTableWidgetItem(val)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                if has_error:
                    item.setBackground(error_tint)
                self.xref_table.setItem(row, col, item)
        self.xref_table.setSortingEnabled(True)

    def _xref_context_menu(self, pos): #vers 2
        item = self.xref_table.itemAt(pos)
        if item is None:
            return
        row = item.row()
        menu = QMenu(self)

        copy_cell_act = menu.addAction("Copy Cell")
        copy_cell_act.triggered.connect(lambda: self._xref_copy_cell(item))
        copy_row_act = menu.addAction("Copy Row")
        copy_row_act.triggered.connect(lambda: self._xref_copy_row(row))

        texture_item = self.xref_table.item(row, 4)
        if (texture_item and "(missing)" in texture_item.text()
                and self.result.img_path):
            menu.addSeparator()
            txd_act = menu.addAction("Open in TXD Workshop to add missing texture")
            txd_act.triggered.connect(lambda: self._xref_open_txd_workshop())

        model_item = self.xref_table.item(row, 3)
        errors_item = self.xref_table.item(row, 5)
        model_name = model_item.text() if model_item else ""
        errors_text = errors_item.text() if errors_item else ""
        if model_name and "Missing DFF" in errors_text and self.result.img_path:
            menu.addSeparator()
            add_dff_act = menu.addAction("Add file externally... (DFF)")
            add_dff_act.triggered.connect(lambda: self._xref_add_missing_dff(model_name))
        if model_name and "Missing COL" in errors_text:
            menu.addSeparator()
            add_col_act = menu.addAction("Add file externally... (COL)")
            add_col_act.triggered.connect(lambda: self._xref_add_missing_col(model_name))

        menu.exec(self.xref_table.viewport().mapToGlobal(pos))

    def _xref_copy_cell(self, item): #vers 1
        from PyQt6.QtWidgets import QApplication
        QApplication.clipboard().setText(item.text())

    def _xref_copy_row(self, row): #vers 1
        from PyQt6.QtWidgets import QApplication
        values = [self.xref_table.item(row, c).text() if self.xref_table.item(row, c) else ""
                   for c in range(self.xref_table.columnCount())]
        QApplication.clipboard().setText("\t".join(values))

    def _xref_open_txd_workshop(self): #vers 2
        try:
            from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
            target = self._pick_target_img()
            if target:
                open_txd_workshop(self.main_window, target)
        except Exception:
            pass

    def _pick_target_img(self): #vers 1
        """Resolve which real loaded IMG a write should target - the
        only one if there's just one, or ask explicitly if several
        (Sep 12 2026, real fix: a whole-game check can have many
        real IMG archives, e.g. SOL's own game_vc.img/game_sa.img/
        etc - self.result.img_path is a DISPLAY string in that case,
        never a real path to open directly)."""
        paths = self.result.img_paths
        if not paths:
            return None
        if len(paths) == 1:
            return paths[0]
        from PyQt6.QtWidgets import QInputDialog
        names = [os.path.basename(p) for p in paths]
        name, ok = QInputDialog.getItem(
            self, "Select Target IMG", "Multiple real IMG archives are loaded - add to which one?",
            names, 0, False)
        if not ok or not name:
            return None
        return next((p for p in paths if os.path.basename(p) == name), None)

    def _xref_add_missing_dff(self, model_name): #vers 3
        """Browse to a real external .dff and add it to the real
        loaded IMG under this exact model name (Sep 12 2026, per
        Keith: "if it says missing file, have the ability to add it
        externally"). add_entry()'s own default auto_save=True
        already saves internally (via save_img_file, which makes
        its own real .backup copy first) and returns that success -
        no separate save() call needed. Asks which real IMG to
        target when more than one is loaded - self.result.img_path
        is a display string, never a real path to open directly."""
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
        into the currently loaded STANDALONE col file. Real COL data
        embedded inside gta3.img itself isn't supported here yet -
        that would mean rewriting the IMG's own binary entry, a
        bigger, riskier operation than appending to a plain
        standalone .col file."""
        if not self.result.col_paths:
            QMessageBox.information(self, "Add File Failed",
                "This game's real COL data is embedded inside gta3.img itself, "
                "not a standalone .col file - adding to embedded IMG collision "
                "data isn't supported yet.")
            return
        standalone_path = self.result.col_paths[0]

        path, _ = QFileDialog.getOpenFileName(
            self, f"Select external COL for {model_name}", "", "COL Files (*.col)")
        if not path:
            return
        try:
            from apps.methods.col_core_classes import COLFile
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

    def _reload_result(self): #vers 3
        """Re-run check_assets against the same real paths and
        repopulate every view - used after any Add File Externally
        write. Only ide_paths/col_paths/img_paths (the real usable
        lists, never the display-string ide_path/col_path/img_path)
        are used - a col_path or img_path display string can
        literally read "gta3.img (embedded COL entries)", not a
        real file check_assets could open."""
        result = check_assets(img_path=self.result.img_paths or None,
                               col_path=self.result.col_paths or None,
                               ide_path=self.result.ide_paths or None)
        self.load_result(result)



def open_asset_workshop(main_window, clicked_path: str = None, dat_path: str = None,
                         game: str = None) -> AssetWorkshop: #vers 1
    """Entry point - real dual-mode pattern every workshop here uses:
    embeds as a tab if main_window has one, real standalone floating
    window otherwise, registered in the tool taskbar. Pass either
    clicked_path (finds real sibling files by shared stem) or
    dat_path (resolves a whole game's gta3.img/COL/all-IDE), not
    both."""
    if clicked_path:
        img_path, col_path, ide_path = find_sibling_asset_files(clicked_path)
        ext = os.path.splitext(clicked_path)[1].lower()
        if ext == '.img':
            img_path = clicked_path
        elif ext == '.col':
            col_path = clicked_path
        elif ext == '.ide':
            ide_path = clicked_path
        result = check_assets(img_path=img_path, col_path=col_path, ide_path=ide_path, game=game)
        tab_label = os.path.splitext(os.path.basename(clicked_path))[0]
    elif dat_path:
        img_path, col_path, ide_paths, game = find_game_asset_files(dat_path)
        if not img_path and not col_path and not ide_paths:
            QMessageBox.warning(main_window, "Asset Workshop",
                f"Could not find any real IMG/COL/IDE files from:\n{dat_path}")
            return None
        result = check_assets(img_path=img_path, col_path=col_path, ide_path=ide_paths, game=game)
        tab_label = os.path.splitext(os.path.basename(dat_path))[0]
    else:
        result = check_assets()
        tab_label = "Asset Workshop"

    try:
        if not main_window or not hasattr(main_window, 'main_tab_widget'):
            workshop = AssetWorkshop(None, main_window)
            workshop.load_result(result)
            workshop.setWindowFlags(Qt.WindowType.Window)
            workshop.setWindowTitle("Asset Workshop")
            workshop.resize(980, 560)
            workshop.show()
            return workshop

        tab_container = QWidget()
        tab_layout = QVBoxLayout(tab_container)
        tab_layout.setContentsMargins(0, 0, 0, 0)

        workshop = AssetWorkshop(tab_container, main_window)
        workshop._tab_container = tab_container
        tab_layout.addWidget(workshop)
        workshop.load_result(result)

        try:
            from apps.methods.imgfactory_svg_icons import get_asset_checker_icon
            icon = get_asset_checker_icon()
            idx = main_window.main_tab_widget.addTab(tab_container, icon, f"Assets: {tab_label}")
        except Exception:
            idx = main_window.main_tab_widget.addTab(tab_container, f"Assets: {tab_label}")
        main_window.main_tab_widget.setCurrentIndex(idx)
        if hasattr(main_window, '_ensure_tab_area_visible'):
            main_window._ensure_tab_area_visible()

        _register_asset_workshop_taskbar(tab_container, main_window)
        return workshop
    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error opening Asset Workshop: {e}")
        return None


def _register_asset_workshop_taskbar(widget, main_window): #vers 1
    """Register or activate the Asset Workshop button in the real
    tool taskbar."""
    try:
        tb = getattr(main_window, 'tool_taskbar', None)
        if not tb:
            return
        if 'asset_workshop' not in tb._tools:
            from apps.methods.imgfactory_svg_icons import get_asset_checker_icon
            icon = get_asset_checker_icon(16)
            tb.register('asset_workshop', 'Assets', icon, widget, 'Asset Workshop')
        else:
            tb._tools['asset_workshop']['target'] = widget
        if hasattr(tb, '_set_exclusive_active'):
            tb._set_exclusive_active('asset_workshop')
    except Exception:
        pass
