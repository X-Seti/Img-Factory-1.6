#this belongs in apps/methods/asset_checker_dialog.py - Version: 7

##Methods list -
# AssetCheckerDialog
# show_asset_checker
# show_asset_checker_from_dat
# _present_asset_check_result
# _register_asset_checker_taskbar
# _all_checked_names
# _show_checked_files_popup
# _on_master_ide

"""asset_checker_dialog.py - the real UI for asset_checker.py's own
cross-referencing"""

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QListWidget, QTableWidget,
    QTableWidgetItem, QStackedWidget, QComboBox, QSplitter, QWidget, QMenu,
    QMessageBox,
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor

from apps.methods.asset_checker import check_assets, find_sibling_asset_files, find_game_asset_files


class AssetCheckerDialog(QDialog): #vers 6
    def __init__(self, parent, result): #vers 4
        super().__init__(parent)
        self.result = result
        self.setWindowTitle("Asset Checker")
        self.resize(980, 560)
        self._build_ui()
        self._populate_columns_view()
        self._populate_merged_view()
        self._populate_cross_reference_view()
        all_checked = self._all_checked_names()
        if len(all_checked) > 3:
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(200, lambda: self._show_checked_files_popup(all_checked))

    def _all_checked_names(self): #vers 1
        """Every real checked filename, one per file (Sep 12 2026 -
        a long comma-joined "Checked: ..." string in one label was
        stretching the window off-screen for whole-game checks with
        many IDE files)."""
        names = []
        if self.result.img_path:
            names.append(os.path.basename(self.result.img_path))
        if self.result.col_path:
            # col_path is either one real full path or an already
            # basename-joined string for the merged multi-file case
            # (SOL gta3 split COL / whole-game check).
            if "," in self.result.col_path:
                names.extend(n.strip() for n in self.result.col_path.split(","))
            else:
                names.append(os.path.basename(self.result.col_path))
        if self.result.ide_path:
            names.extend(n.strip() for n in self.result.ide_path.split(","))
        return names

    def _show_checked_files_popup(self, names): #vers 1
        """Small non-modal popup listing every checked file, one per
        line, auto-closing after 5 seconds (Sep 12 2026, per Keith:
        "a timed popup window listing those line by line, then 5
        seconds close")."""
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

    def _build_ui(self): #vers 2
        lay = QVBoxLayout(self)

        top_row = QHBoxLayout()
        all_checked = self._all_checked_names()
        if not all_checked:
            summary = "No sibling files found"
        elif len(all_checked) <= 3:
            summary = "Checked: " + ", ".join(all_checked)
        else:
            summary = (f"Checked: {', '.join(all_checked[:2])} "
                       f"+{len(all_checked) - 2} more")
        top_row.addWidget(QLabel(summary))
        if len(all_checked) > 3:
            from PyQt6.QtWidgets import QPushButton
            show_files_btn = QPushButton("Show list")
            show_files_btn.clicked.connect(
                lambda: self._show_checked_files_popup(all_checked))
            top_row.addWidget(show_files_btn)
        top_row.addStretch()
        top_row.addWidget(QLabel("View:"))
        self.view_combo = QComboBox()
        self.view_combo.addItems(["4-Column View", "Merged View", "Cross-Reference Table"])
        self.view_combo.currentIndexChanged.connect(self._on_view_changed)
        top_row.addWidget(self.view_combo)
        if self.result.ide_path:
            from PyQt6.QtWidgets import QPushButton
            master_ide_btn = QPushButton("Master IDE...")
            master_ide_btn.clicked.connect(self._on_master_ide)
            top_row.addWidget(master_ide_btn)
        lay.addLayout(top_row)

        self.stack = QStackedWidget()
        lay.addWidget(self.stack, 1)

        columns_widget = QWidget()
        columns_lay = QHBoxLayout(columns_widget)
        splitter = QSplitter(Qt.Orientation.Horizontal)
        columns_lay.addWidget(splitter)

        ide_count = len(self.result.ide_names)
        img_extra_count = len(self.result.img_extra_over_ide) if self.result.img_path and self.result.ide_path else 0
        img_missing_count = len(self.result.missing_from_img) if self.result.img_path and self.result.ide_path else 0
        col_extra_count = len(self.result.col_extra_over_ide) if self.result.col_path and self.result.ide_path else 0
        col_missing_count = len(self.result.missing_from_col) if self.result.col_path and self.result.ide_path else 0

        self.id_list = self._make_column(splitter, "ID", None)
        self.ide_list = self._make_column(splitter, "IDE entry list", ide_count)
        self.img_list = self._make_column(
            splitter, "IMG archive", ide_count,
            diffs=self._real_diffs(img_extra_count, img_missing_count, "IMG"))
        self.col_list = self._make_column(
            splitter, "COL archive", ide_count,
            diffs=self._real_diffs(col_extra_count, col_missing_count, "COL"))
        self.error_list = self._make_column(splitter, "Error list", None)
        self.stack.addWidget(columns_widget)

        # Locked scrolling across ID/IDE/IMG/COL, Errors excluded (Sep 5 2026)
        self._sync_lists = [self.id_list, self.ide_list, self.img_list, self.col_list]
        self._sync_guard = False
        for lst in self._sync_lists:
            lst.verticalScrollBar().valueChanged.connect(self._on_sync_scroll)

        # --- merged view ---
        self.merged_table = QTableWidget()
        self.merged_table.setColumnCount(3)
        self.merged_table.setHorizontalHeaderLabels(["Model Name", "Source", "Status"])
        self.merged_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self.merged_table.horizontalHeader().setStretchLastSection(True)
        self.stack.addWidget(self.merged_table)

        # --- cross-reference table view ---
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

    def _make_column(self, splitter, title, count, diffs=None): #vers 8
        """count is the base number shown in parentheses (Sep 5 2026)"""
        container = QWidget()
        container.setMinimumWidth(150)   # wider columns (Sep 5 2026)
        v = QVBoxLayout(container)
        v.setContentsMargins(2, 2, 2, 2)
        header_row = QHBoxLayout()
        label_text = title if count is None else f"{title} ({count})"
        header_lbl = QLabel(label_text)
        # Lighter theme-aware header text
        bright = self.palette().color(self.palette().currentColorGroup(),
                                       self.palette().ColorRole.BrightText)
        header_lbl.setStyleSheet(f"color: {bright.name()}; font-weight: bold;")
        header_row.addWidget(header_lbl)
        for label, tooltip, on_click in (diffs or []):
            from PyQt6.QtWidgets import QPushButton
            diff_btn = QPushButton(label)
            diff_btn.setFlat(True)
            diff_btn.setCursor(Qt.CursorShape.PointingHandCursor)
            # Compact padding (Sep 5 2026)
            diff_btn.setStyleSheet(
                "text-decoration: underline; padding: 0px 3px; font-size: 11px;")
            diff_btn.setMaximumWidth(diff_btn.fontMetrics().horizontalAdvance(label) + 10)
            diff_btn.setToolTip(tooltip)
            if on_click:
                diff_btn.clicked.connect(on_click)
            header_row.addWidget(diff_btn)
        header_row.addStretch()
        v.addLayout(header_row)
        lst = QListWidget()
        # Alternating row colours (Sep 5 2026)
        lst.setAlternatingRowColors(True)
        v.addWidget(lst)
        splitter.addWidget(container)
        return lst

    def _real_diffs(self, extra_count, missing_count, source_label): #vers 2
        """Build the real, independently-accurate diff button list for
        one source column - a "+N" button only if there are real
        extras, a "-M" button only if there are real missing entries,
        both at once if both are genuinely true."""
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
        """Keep ID/IDE/IMG/COL scrolled together (Sep 5 2026)"""
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
        """Small popup listing the real specific names behind a "+N"/
        "-N" header diff (Sep 5 2026)"""
        from PyQt6.QtWidgets import QApplication, QPushButton, QHBoxLayout as _QHBoxLayout
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

    def _on_master_ide(self): #vers 3
        """Open Master IDE for the same real IDE file(s)."""
        from apps.components.Master_Ide.master_ide_workshop import open_master_ide_workshop
        paths = self.result.ide_paths or [self.result.ide_path]
        open_master_ide_workshop(self.parent(), ide_paths=paths)

    def _populate_columns_view(self): #vers 4
        r = self.result
        # IMG/COL now sort by the same IDE-ID-driven order as ID/IDE

        def _ide_order_key(name):
            return (r.ide_id_by_name.get(name, float('inf')), name)
        self.img_list.addItems(sorted(r.img_names, key=_ide_order_key))
        self.col_list.addItems(sorted(r.col_names, key=_ide_order_key))
        # ID numeric order by default (Sep 5 2026)
        sorted_ide_names = sorted(r.ide_names, key=lambda n: r.ide_id_by_name.get(n, 0))
        self.ide_list.addItems(sorted_ide_names)
        # ID column aligned to the same sorted order as IDE (Sep 5 026)
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
        """Each model name gets one row per source it's actually
        found in (Sep 5 2026)"""
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
        """One row per real model name, in the real column order
        asked for (Sep 5 2026): ID | DFF | COL | IDE Model Name |
        Texture entry | Errors - see AssetCheckResult.cross_reference_
        rows' own docstring for how each column is actually derived.
        Rows with a real error get a subtle red tint blended from the
        current theme's own base colour, so problems stand out without
        hardcoding a fixed hex value."""
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

    def _xref_context_menu(self, pos): #vers 1
        """Right-click menu on the cross-reference table (Sep 5 2026)"""
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

        menu.exec(self.xref_table.viewport().mapToGlobal(pos))

    def _xref_copy_cell(self, item): #vers 1
        from PyQt6.QtWidgets import QApplication
        QApplication.clipboard().setText(item.text())

    def _xref_copy_row(self, row): #vers 1
        from PyQt6.QtWidgets import QApplication
        values = [self.xref_table.item(row, c).text() if self.xref_table.item(row, c) else ""
                   for c in range(self.xref_table.columnCount())]
        QApplication.clipboard().setText("\t".join(values))

    def _xref_open_txd_workshop(self): #vers 1
        try:
            from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
            open_txd_workshop(self.parent(), self.result.img_path)
        except Exception:
            pass


def show_asset_checker(main_window, clicked_path: str, game: str = None): #vers 2
    """Entry point for the real right-click action - finds the other
    real sibling files sharing clicked_path's own base stem, cross-
    references them, and shows the result. Safe to call even if only
    1 of the 3 real files exists."""
    img_path, col_path, ide_path = find_sibling_asset_files(clicked_path)
    ext = os.path.splitext(clicked_path)[1].lower()
    if ext == '.img':
        img_path = clicked_path
    elif ext == '.col':
        col_path = clicked_path
    elif ext == '.ide':
        ide_path = clicked_path

    result = check_assets(img_path=img_path, col_path=col_path, ide_path=ide_path, game=game)
    tab_label = os.path.splitext(os.path.basename(clicked_path))[0] if clicked_path else "Asset Checker"
    return _present_asset_check_result(main_window, result, tab_label)


def show_asset_checker_from_dat(main_window, dat_path: str): #vers 1
    """Entry point for the Intro page tile (no file context of its
    own) - resolves a whole game's real gta3.img/COL/all-IDE files
    from its main .dat and cross-references them (Sep 12 2026, per
    Keith: "asset check needs all 3 img col ide, so i'd ask for the
    game gta_vc.dat, gta3.dat... to load another gta modding
    project")."""
    img_path, col_path, ide_paths, game = find_game_asset_files(dat_path)
    if not img_path and not col_path and not ide_paths:
        QMessageBox.warning(main_window, "Asset Checker",
            f"Could not find any real IMG/COL/IDE files from:\n{dat_path}")
        return
    result = check_assets(img_path=img_path, col_path=col_path,
                           ide_path=ide_paths, game=game)
    tab_label = os.path.splitext(os.path.basename(dat_path))[0]
    return _present_asset_check_result(main_window, result, tab_label)


def _present_asset_check_result(main_window, result, tab_label: str): #vers 1
    """Shared display logic - real embedded tab when main_window has
    a tab system, standalone modal dialog otherwise."""
    if main_window and hasattr(main_window, 'main_tab_widget'):
        from PyQt6.QtWidgets import QVBoxLayout, QWidget

        tab_container = QWidget()
        tab_layout = QVBoxLayout(tab_container)
        tab_layout.setContentsMargins(0, 0, 0, 0)

        dlg = AssetCheckerDialog(tab_container, result)
        dlg.setWindowFlags(Qt.WindowType.Widget)
        tab_layout.addWidget(dlg)

        try:
            from apps.methods.imgfactory_svg_icons import get_asset_checker_icon
            icon = get_asset_checker_icon()
            idx = main_window.main_tab_widget.addTab(tab_container, icon, f"Assets: {tab_label}")
        except Exception:
            idx = main_window.main_tab_widget.addTab(tab_container, f"Assets: {tab_label}")
        main_window.main_tab_widget.setCurrentIndex(idx)
        if hasattr(main_window, '_ensure_tab_area_visible'):
            main_window._ensure_tab_area_visible()

        _register_asset_checker_taskbar(tab_container, main_window)
        return dlg

    dlg = AssetCheckerDialog(main_window, result)
    dlg.exec()
    return dlg


def _register_asset_checker_taskbar(widget, main_window): #vers 1
    """Register or activate the Asset Checker button in the real
    tool taskbar (Sep 5 2026)"""
    try:
        tb = getattr(main_window, 'tool_taskbar', None)
        if not tb:
            return
        if 'asset_checker' not in tb._tools:
            from apps.methods.imgfactory_svg_icons import get_asset_checker_icon
            icon = get_asset_checker_icon(16)
            tb.register('asset_checker', 'Assets', icon, widget, 'Asset Checker')
        else:
            tb._tools['asset_checker']['target'] = widget
        if hasattr(tb, '_set_exclusive_active'):
            tb._set_exclusive_active('asset_checker')
    except Exception:
        pass
