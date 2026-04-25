#this belongs in apps/core/img_ide_find.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - IMG vs IDE Find Functions
"""
IMG vs IDE Find Functions
Scan the loaded IMG against the xref and highlight/report entries
that are missing from or present in the IDE/COLFILE data.
"""

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
    QPushButton, QLabel, QHeaderView, QAbstractItemView
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor

from apps.core.theme_utils import apply_dialog_theme

##Methods list -
# find_not_in_ide
# find_orphan_txd
# find_orphan_col
# find_all_col
# find_all_dff_in_ide
# highlight_rows
# show_find_results_dialog
# _get_table_and_xref


STATUS_COL = 7   # Status column index in IMG table


def _get_table_and_xref(main_window): #vers 1
    """Return (table, xref) for the active IMG tab, or (None, None)."""
    try:
        from apps.methods.export_shared import get_active_table
        table = get_active_table(main_window)
        if not table:
            tw = getattr(main_window, 'main_tab_widget', None)
            if tw:
                tab = tw.currentWidget()
                table = getattr(tab, 'table_ref', None)
        xref = getattr(main_window, 'xref', None)
        return table, xref
    except Exception:
        return None, None


def find_not_in_ide(main_window): #vers 1
    """Find DFF and TXD entries not present in any IDE file."""
    table, xref = _get_table_and_xref(main_window)
    if not table:
        return
    results = []
    for row in range(table.rowCount()):
        item = table.item(row, 0)
        if not item:
            continue
        name = item.text()
        ext  = name.rsplit('.', 1)[-1].lower() if '.' in name else ''
        if ext not in ('dff', 'txd'):
            continue
        tip = xref.tooltip_for(name) if xref else ''
        if not tip or 'orphan' in tip.lower() or 'not found' in tip.lower():
            results.append((row, name, ext.upper(), 'Not in IDE'))
    _show_and_highlight(main_window, table, results,
                        "Not in IDE — DFF / TXD",
                        ["Row", "Name", "Type", "Status"])


def find_orphan_txd(main_window): #vers 1
    """Find TXD entries not referenced by any IDE object."""
    table, xref = _get_table_and_xref(main_window)
    if not table:
        return
    results = []
    txd_stems = getattr(xref, 'txd_stems', set()) if xref else set()
    for row in range(table.rowCount()):
        item = table.item(row, 0)
        if not item:
            continue
        name = item.text()
        if not name.lower().endswith('.txd'):
            continue
        stem = os.path.splitext(name.lower())[0]
        if stem not in txd_stems:
            results.append((row, name, 'TXD', 'Orphan — not referenced by IDE'))
    _show_and_highlight(main_window, table, results,
                        "Orphan TXD — not referenced by any IDE object",
                        ["Row", "Name", "Type", "Status"])


def find_orphan_col(main_window): #vers 1
    """Find COL entries not listed in any COLFILE directive."""
    table, xref = _get_table_and_xref(main_window)
    if not table:
        return
    results = []
    col_stems = getattr(xref, 'col_stems', set()) if xref else set()
    for row in range(table.rowCount()):
        item = table.item(row, 0)
        if not item:
            continue
        name = item.text()
        if not name.lower().endswith('.col'):
            continue
        stem = os.path.splitext(name.lower())[0]
        if stem not in col_stems:
            results.append((row, name, 'COL', 'Not in COLFILE'))
    _show_and_highlight(main_window, table, results,
                        "COL entries not in any COLFILE directive",
                        ["Row", "Name", "Type", "Status"])


def find_all_col(main_window): #vers 1
    """Find and highlight all COL entries in the IMG."""
    table, xref = _get_table_and_xref(main_window)
    if not table:
        return
    col_stems = getattr(xref, 'col_stems', set()) if xref else set()
    results = []
    for row in range(table.rowCount()):
        item = table.item(row, 0)
        if not item:
            continue
        name = item.text()
        if not name.lower().endswith('.col'):
            continue
        stem = os.path.splitext(name.lower())[0]
        status = 'In COLFILE' if stem in col_stems else 'Not in COLFILE'
        results.append((row, name, 'COL', status))
    _show_and_highlight(main_window, table, results,
                        "All COL entries",
                        ["Row", "Name", "Type", "COLFILE Status"])


def find_all_dff_in_ide(main_window): #vers 1
    """Find all DFF entries and show their IDE status."""
    table, xref = _get_table_and_xref(main_window)
    if not table:
        return
    model_map = getattr(xref, 'model_map', {}) if xref else {}
    results = []
    for row in range(table.rowCount()):
        item = table.item(row, 0)
        if not item:
            continue
        name = item.text()
        if not name.lower().endswith('.dff'):
            continue
        stem = os.path.splitext(name.lower())[0]
        obj  = model_map.get(stem)
        if obj:
            txd  = getattr(obj, 'txd_name', '?')
            ide  = getattr(obj, 'source_ide', '?')
            status = f"In IDE — txd:{txd}  [{ide}]"
        else:
            status = 'Not in IDE'
        results.append((row, name, 'DFF', status))
    _show_and_highlight(main_window, table, results,
                        "All DFF entries — IDE status",
                        ["Row", "Name", "Type", "IDE Status"])


def _show_and_highlight(main_window, table, results, title, headers): #vers 1
    """Show results dialog and highlight matching rows in the IMG table."""
    if not results:
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(main_window, title, "No matching entries found.")
        return

    # Highlight rows in table
    highlight_rows(table, [r[0] for r in results])

    # Show results dialog
    dlg = show_find_results_dialog(main_window, title, headers, results)
    dlg.exec()


def highlight_rows(table, row_indices: list): #vers 1
    """Select the given rows in the table and scroll to first."""
    table.clearSelection()
    for row in row_indices:
        table.selectRow(row)
    if row_indices:
        table.scrollToItem(table.item(row_indices[0], 0))


def show_find_results_dialog(main_window, title: str,
                             headers: list, results: list) -> QDialog: #vers 1
    """Show a resizable results table dialog."""
    dlg = QDialog(main_window)
    dlg.setWindowTitle(title)
    dlg.setMinimumSize(700, 450)
    layout = QVBoxLayout(dlg)

    lbl = QLabel(f"{len(results)} result(s)")
    layout.addWidget(lbl)

    tbl = QTableWidget()
    tbl.setColumnCount(len(headers))
    tbl.setHorizontalHeaderLabels(headers)
    tbl.setRowCount(len(results))
    tbl.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
    tbl.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
    tbl.setAlternatingRowColors(True)
    hdr = tbl.horizontalHeader()
    hdr.setSectionResizeMode(0, QHeaderView.ResizeMode.ResizeToContents)
    hdr.setSectionResizeMode(1, QHeaderView.ResizeMode.Stretch)
    for i in range(2, len(headers)):
        hdr.setSectionResizeMode(i, QHeaderView.ResizeMode.ResizeToContents)

    green = QColor(0,160,0)  # semantic: valid
    red   = QColor(180,0,0)  # semantic: invalid
    grey  = self._get_ui_color('viewport_text')

    for row, data in enumerate(results):
        for col, val in enumerate(data):
            item = QTableWidgetItem(str(val))
            if col == len(headers) - 1:  # Status column
                v = str(val).lower()
                item.setForeground(
                    green if 'in ide' in v or 'in col' in v
                    else grey if 'orphan' in v
                    else red)
            tbl.setItem(row, col, item)

    layout.addWidget(tbl)

    btn_row = QHBoxLayout()
    btn_row.addStretch()
    close_btn = QPushButton("Close")
    close_btn.clicked.connect(dlg.accept)
    btn_row.addWidget(close_btn)
    layout.addLayout(btn_row)

    apply_dialog_theme(dlg)
    return dlg
