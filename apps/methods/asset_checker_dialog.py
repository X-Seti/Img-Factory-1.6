#this belongs in apps/methods/asset_checker_dialog.py - Version: 2

##Methods list -
# AssetCheckerDialog
# show_asset_checker

"""asset_checker_dialog.py - the real UI for asset_checker.py's own
cross-referencing (Sep 5 2026, per Keith: "As 3 columns IMG archive |
COL archive | IDE entry list | Error list... And another layout to
show IMG, COL and IDE as 3 different lines, each with its own shade
but theme-aware" + his own follow-up: "we could have a txd 4th
column... the ide file ID for the 1st column, dff for the 2rd, col,
3rd, ide modelname 4th, texture entry, then errors" + his own later
confirmed 4-column redesign: "ID | ide (2453) | Img (2453) +1 | col
(2453) +1 | Errors... Clicking the +1 shows the filename, with the
option to copy the filename... have the ability to lock the scroll
across all 4... except the Errors column"). Three switchable layouts
sharing one AssetCheckResult:

- 4-column view: ID | IDE entry list | IMG archive | COL archive |
  Error list. ID and IDE scroll-locked together with IMG/COL (Errors
  excluded). IMG/COL headers show IDE's own real count as their base
  number plus a signed, clickable +N/-N diff from it (+ when that
  source has extras IDE doesn't declare, - when IDE declares things
  that source is missing) - clicking it lists the real specific names
  involved, with copy options. Equal counts don't guarantee matching
  sets; the Error list still does the real, full comparison.
- Merged view: each model name gets one row per source it's actually
  found in, each row tinted from the current theme's own palette.
- Cross-reference table: one row per model (ID/DFF/COL/IDE Model
  Name/Texture entry/Errors), checking whether each IDE entry's own
  declared texture actually exists in the IMG, with a right-click
  menu (copy cell/row, open TXD Workshop for a missing texture)."""

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QListWidget, QTableWidget,
    QTableWidgetItem, QStackedWidget, QComboBox, QSplitter, QWidget, QMenu,
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor

from apps.methods.asset_checker import check_assets, find_sibling_asset_files


class AssetCheckerDialog(QDialog): #vers 3
    def __init__(self, parent, result): #vers 3
        super().__init__(parent)
        self.result = result
        self.setWindowTitle("Asset Checker")
        self.resize(980, 560)
        self._build_ui()
        self._populate_columns_view()
        self._populate_merged_view()
        self._populate_cross_reference_view()

    def _build_ui(self): #vers 1
        lay = QVBoxLayout(self)

        top_row = QHBoxLayout()
        names = []
        if self.result.img_path:
            names.append(os.path.basename(self.result.img_path))
        if self.result.col_path:
            # col_path is either one real full path (basename it, same
            # as img/ide) or an already-basename-joined string for the
            # merged multi-file case (Sep 5 2026, SOL gta3 split COL) -
            # only basename() the single-path case.
            col_display = (self.result.col_path if "," in self.result.col_path
                            else os.path.basename(self.result.col_path))
            names.append(col_display)
        if self.result.ide_path:
            names.append(os.path.basename(self.result.ide_path))
        top_row.addWidget(QLabel("Checked: " + ", ".join(names) if names else "No sibling files found"))
        top_row.addStretch()
        top_row.addWidget(QLabel("View:"))
        self.view_combo = QComboBox()
        self.view_combo.addItems(["4-Column View", "Merged View", "Cross-Reference Table"])
        self.view_combo.currentIndexChanged.connect(self._on_view_changed)
        top_row.addWidget(self.view_combo)
        lay.addLayout(top_row)

        self.stack = QStackedWidget()
        lay.addWidget(self.stack, 1)

        # --- 4-column view ---
        # New ID column + IDE-relative header diffs (Sep 5 2026, per
        # Keith's own confirmed design: "ID | ide (2453) | Img (2453)
        # +1 | col (2453) +1 | Errors... does that make logical sense
        # to you, and what if there are more items in the ide, then
        # col or img" -> "perfect"). Diff can run either way (+N when
        # a source has extras IDE doesn't declare, -N when IDE
        # declares things that source is missing) - both directions
        # use the SAME real IDE count as their base number, so they're
        # directly comparable at a glance; clicking the diff opens the
        # specific real names involved. Equal counts don't guarantee
        # matching sets (see this dialog's own README on that) - the
        # Error list still does the real, full comparison regardless.
        columns_widget = QWidget()
        columns_lay = QHBoxLayout(columns_widget)
        splitter = QSplitter(Qt.Orientation.Horizontal)
        columns_lay.addWidget(splitter)

        ide_count = len(self.result.ide_names)
        img_diff = (len(self.result.img_names) - ide_count
                    if self.result.img_path and self.result.ide_path else None)
        col_diff = (len(self.result.col_names) - ide_count
                    if self.result.col_path and self.result.ide_path else None)

        self.id_list = self._make_column(splitter, "ID", None)
        self.ide_list = self._make_column(splitter, "IDE entry list", ide_count)
        self.img_list = self._make_column(
            splitter, "IMG archive", ide_count, img_diff,
            lambda: self._show_diff_popup(
                self.result.img_extra_over_ide if img_diff > 0 else self.result.missing_from_img,
                "IMG entries not in IDE" if img_diff > 0 else "IDE entries missing from IMG"))
        self.col_list = self._make_column(
            splitter, "COL archive", ide_count, col_diff,
            lambda: self._show_diff_popup(
                self.result.col_extra_over_ide if col_diff > 0 else self.result.missing_from_col,
                "COL entries not in IDE" if col_diff > 0 else "IDE entries missing from COL"))
        self.error_list = self._make_column(splitter, "Error list", None)
        self.stack.addWidget(columns_widget)

        # Locked scrolling across ID/IDE/IMG/COL, Errors excluded (Sep
        # 5 2026, per Keith: "have the ability to lock the scroll
        # across all 4... except the Errors column").
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

    def _make_column(self, splitter, title, count, diff=None, on_diff_click=None): #vers 2
        """count is the base number shown in parentheses (Sep 5 2026,
        always the real IDE count for IMG/COL columns, per Keith's own
        confirmed design), diff is the signed real difference from it
        (None or 0 means no diff shown). on_diff_click opens the real
        popup listing which specific names make up that difference."""
        container = QWidget()
        v = QVBoxLayout(container)
        v.setContentsMargins(2, 2, 2, 2)
        header_row = QHBoxLayout()
        label_text = title if count is None else f"{title} ({count})"
        header_row.addWidget(QLabel(label_text))
        if diff:
            from PyQt6.QtWidgets import QPushButton
            diff_btn = QPushButton(f"{'+' if diff > 0 else ''}{diff}")
            diff_btn.setFlat(True)
            diff_btn.setCursor(Qt.CursorShape.PointingHandCursor)
            diff_btn.setStyleSheet("text-decoration: underline;")
            if on_diff_click:
                diff_btn.clicked.connect(on_diff_click)
            header_row.addWidget(diff_btn)
        header_row.addStretch()
        v.addLayout(header_row)
        lst = QListWidget()
        v.addWidget(lst)
        splitter.addWidget(container)
        return lst

    def _on_sync_scroll(self, value): #vers 1
        """Keep ID/IDE/IMG/COL scrolled together (Sep 5 2026, per
        Keith's own confirmed design) - guarded against re-entrant
        signal loops, since setting one list's scrollbar would
        otherwise re-trigger this same handler for that list too."""
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
        "-N" header diff (Sep 5 2026, per Keith: "Clicking the +1
        shows the filename, with the option to copy the filename")."""
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

    def _populate_columns_view(self): #vers 2
        r = self.result
        self.img_list.addItems(sorted(r.img_names))
        self.col_list.addItems(sorted(r.col_names))
        sorted_ide_names = sorted(r.ide_names)
        self.ide_list.addItems(sorted_ide_names)
        # ID column aligned to the same sorted order as IDE (Sep 5
        # 2026, per Keith: "ID column is the object ID shown in the
        # IDE file, with that I can just lookup the ID in the real
        # ide file, to match the model").
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
        found in (Sep 5 2026, per Keith: "show IMG, COL and IDE as 3
        different lines, each with its own shade but theme-aware") -
        shading comes from the current palette's own base colour,
        lightened/darkened per source rather than a fixed hex value,
        so it still fits whatever theme is active."""
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
        """One row per real model name, in the real column order Keith
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
        """Right-click menu on the cross-reference table (Sep 5 2026,
        per Keith: "with right click options to edit that table, add
        the missing txd, rename, delete, copy and paste cell names").
        Copy cell/row is always available. "Open in TXD Workshop" only
        shows for a row whose own Texture entry is genuinely missing
        (per Keith's own confirmed answer: open the real workshop so
        he can add a real texture there himself, not an automated
        write) - opens TXD Workshop against this checker's own real
        img_path, the same archive the missing texture would need to
        go into.

        Rename/Delete-with-backup-undo and the game-wide IPL scan for
        model-name uniqueness are real, separate, larger pieces (the
        former edits real files, the latter needs scanning every real
        .dat file for every real IPL) - not implemented yet, scoped
        as their own follow-up rather than rushed in here."""
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


def show_asset_checker(main_window, clicked_path: str, game: str = None): #vers 1
    """Entry point for the real right-click action - finds the other
    real sibling files sharing clicked_path's own base stem, cross-
    references them, and shows the result dialog. Safe to call even
    if only 1 of the 3 real files exists."""
    img_path, col_path, ide_path = find_sibling_asset_files(clicked_path)
    ext = os.path.splitext(clicked_path)[1].lower()
    if ext == '.img':
        img_path = clicked_path
    elif ext == '.col':
        col_path = clicked_path
    elif ext == '.ide':
        ide_path = clicked_path

    result = check_assets(img_path=img_path, col_path=col_path, ide_path=ide_path, game=game)
    dlg = AssetCheckerDialog(main_window, result)
    dlg.exec()
