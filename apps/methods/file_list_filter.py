#this belongs in apps/methods/file_list_filter.py - Version: 1
# X-Seti - October 21 2026 - IMG Factory 1.6 - Files pane type filter

"""
Files pane (Map / Model Workshop): counts of the file types present, a type
filter showing only those types, and the search box, applied together.
"""

##Methods list -
# apply_file_filter
# file_type_counts
# file_types_header
# make_type_filter
# refresh_type_filter

import os

from PyQt6.QtWidgets import QComboBox

_LABELS = {'.dff': 'Models', '.txd': 'TXD', '.col': 'COL'}


def file_type_counts(lw) -> dict: #vers 1
    """{'.dff': n, ...} for the list widget's items (only types present)."""
    counts = {}
    for i in range(lw.count()):
        ext = os.path.splitext(lw.item(i).text())[1].lower()
        counts[ext] = counts.get(ext, 0) + 1
    return counts


def file_types_header(counts: dict) -> str: #vers 1
    """'Models: 3  COL: 48' for the types present; 'No files loaded' when empty."""
    parts = [f"{_LABELS.get(e, e[1:].upper())}: {n}" for e, n in counts.items() if n]
    return "  ".join(parts) if parts else "No files loaded"


def make_type_filter(ws) -> QComboBox: #vers 1
    """Type filter combo for ws.col_list_widget; hidden until two types are present."""
    combo = QComboBox()
    combo.setToolTip("Show only this file type")
    combo.setFixedHeight(18)
    combo.setVisible(False)
    combo.currentIndexChanged.connect(lambda _i, w=ws: apply_file_filter(w))
    ws._file_type_combo = combo
    return combo


def refresh_type_filter(ws): #vers 1
    """After the list is filled: header counts and the combo's available types."""
    lw = getattr(ws, 'col_list_widget', None)
    if lw is None:
        return
    counts = file_type_counts(lw)
    hdr = getattr(ws, '_left_panel_header', None)
    if hdr is not None:
        hdr.setText(file_types_header(counts))
    combo = getattr(ws, '_file_type_combo', None)
    if combo is not None:
        combo.blockSignals(True)
        combo.clear()
        combo.addItem(f"All ({lw.count()})", '')
        for ext, n in counts.items():
            combo.addItem(f"{_LABELS.get(ext, ext[1:].upper())} ({n})", ext)
        combo.blockSignals(False)
        combo.setVisible(len(counts) > 1)
    apply_file_filter(ws)


def apply_file_filter(ws): #vers 1
    """Hide items not matching the search text or the chosen type."""
    lw = getattr(ws, 'col_list_widget', None)
    if lw is None:
        return
    box = getattr(ws, '_model_search_box', None)
    text = box.text().lower() if box is not None and box.isVisible() else ''
    combo = getattr(ws, '_file_type_combo', None)
    ext = combo.currentData() if combo is not None and combo.count() else ''
    for i in range(lw.count()):
        item = lw.item(i)
        name = item.text().lower()
        item.setHidden(bool(text and text not in name) or bool(ext and not name.endswith(ext)))
