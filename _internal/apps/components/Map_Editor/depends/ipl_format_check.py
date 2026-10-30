#!/usr/bin/env python3
#this belongs in apps/components/Map_Editor/depends/ipl_format_check.py - Version: 2
# X-Seti - September25 2026 - IMG Factory 1.6 - IPL format checker

"""
IPL format checker - reports each IPL's INST layout (VC / SA / III), scale
values and malformed lines; repairs lines and converts III / VC / SA layouts.
"""

##Methods list -
# _inst_lines
# _line_problems
# rewrite_ipl
# scan_ipl_format

##class IPLFormatDialog: -
# __init__
# _add_files
# _add_folder
# _add_loaded
# _add_paths
# _apply
# _fill_row
# _show_problems

import os

from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTableWidget,
    QTableWidgetItem, QPushButton, QPlainTextEdit, QFileDialog, QMessageBox,
    QAbstractItemView, QHeaderView, QLabel)

TARGETS = {'to_vc': 'vc', 'to_sa': 'sa', 'to_iii': 'gta3'}


def _inst_lines(lines): #vers 1
    """Yield (index, fields) for every data line inside inst sections."""
    in_inst = False
    for i, raw in enumerate(lines):
        s = raw.split('#')[0].strip()
        low = s.lower()
        if not in_inst:
            in_inst = (low == 'inst')
            continue
        if low == 'end':
            in_inst = False
            continue
        if s:
            yield i, [p.strip() for p in s.split(',')]


def _line_problems(parts, rec, layout): #vers 2
    """Reasons a single inst line looks wrong, empty list if fine."""
    if rec is None:
        return [f"unrecognised line ({len(parts)} fields)"]
    probs = [rec['note']] if rec['note'] else []
    if layout and rec['layout'] != layout:
        probs.append(f"{rec['layout']} line in {layout} file")
    if any(abs(float(v)) > 20000 for v in rec['pos']):
        probs.append("position out of range")
    mag = sum(float(v) ** 2 for v in rec['rot'])
    if not 0.9 < mag < 1.1:
        probs.append(f"rotation not unit length ({mag:.2f})")
    if rec['scale']:
        sc = [float(v) for v in rec['scale']]
        if sc not in ([0.0] * 3, [1.0] * 3) and (min(sc) <= 0 or max(sc) > 50):
            probs.append(f"odd scale {', '.join(rec['scale'])}")
    return probs


def scan_ipl_format(path): #vers 2
    """Layout, line count, scale counts and problem lines for one IPL."""
    from apps.methods.gta_dat_parser import parse_inst_record
    with open(path, 'r', encoding='latin-1', newline='') as f:
        lines = f.readlines()
    rows = [(i, parts, parse_inst_record(parts)) for i, parts in _inst_lines(lines)]
    counts = {}
    for _i, _p, rec in rows:
        if rec:
            counts[rec['layout']] = counts.get(rec['layout'], 0) + 1
    layout = max(counts, key=counts.get) if counts else ''
    res = {'path': path, 'layout': layout, 'lines': len(rows),
           'zero_scale': 0, 'unit_scale': 0, 'interiors': set(), 'problems': []}
    for i, parts, rec in rows:
        if rec and rec['scale']:
            fs = [float(v) for v in rec['scale']]
            res['zero_scale'] += fs == [0.0] * 3
            res['unit_scale'] += fs == [1.0] * 3
        if rec and rec['layout'] != 'III':
            res['interiors'].add(rec['int'])
        for p in _line_problems(parts, rec, layout):
            res['problems'].append((i + 1, p, lines[i].rstrip('\r\n')))
    return res


def rewrite_ipl(path, action, zero_scale_to_one=True): #vers 2
    """Rewrite inst lines: to_vc / to_sa / to_iii / repair / scale_1 / scale_0. Returns lines changed."""
    from apps.methods.gta_dat_parser import parse_inst_record, format_inst_record
    from apps.components.Map_Editor.map_workshop import _write_ipl_lines
    with open(path, 'r', encoding='latin-1', newline='') as f:
        lines = f.readlines()
    layout = scan_ipl_format(path)['layout']
    own = {'VC': 'vc', 'SA': 'sa', 'III': 'gta3'}.get(layout, 'vc')
    changed = 0
    for i, parts in list(_inst_lines(lines)):
        rec = parse_inst_record(parts)
        if rec is None:
            continue
        eol = '\r\n' if lines[i].endswith('\r\n') else '\n'
        if action in TARGETS:
            new = format_inst_record(rec, TARGETS[action], zero_scale_to_one)
        elif action == 'repair':
            new = format_inst_record(rec, own, zero_scale_to_one=False)
        elif action in ('scale_1', 'scale_0') and rec['scale']:
            src, dst = (0.0, '1') if action == 'scale_1' else (1.0, '0')
            if [float(v) for v in rec['scale']] != [src] * 3:
                continue
            rec = dict(rec, scale=[dst] * 3)
            new = format_inst_record(rec, 'vc' if rec['layout'] != 'III' else 'gta3',
                                     zero_scale_to_one=False)
        else:
            continue
        if new != parts:
            lines[i] = ', '.join(new) + eol
            changed += 1
    if changed:
        _write_ipl_lines(path, lines)          # timestamped backup + atomic write
    return changed


class IPLFormatDialog(QDialog):
    """Check IPL layouts and scales; fix scale or convert SA/VC layout."""

    COLS = ["File", "Layout", "Inst lines", "Scale 0,0,0", "Scale 1,1,1", "Interiors", "Problems"]

    def __init__(self, parent=None, loaded_paths=None): #vers 1
        super().__init__(parent)
        self.setWindowTitle("IPL Format Checker")
        self.resize(900, 560)
        self._loaded = list(loaded_paths or [])
        self._results = []
        lay = QVBoxLayout(self)
        add = QHBoxLayout()
        for text, fn in (("Add Loaded IPLs", self._add_loaded), ("Add Files...", self._add_files),
                         ("Add Folder...", self._add_folder)):
            b = QPushButton(text); b.clicked.connect(fn); add.addWidget(b)
        add.addStretch()
        lay.addLayout(add)
        self._table = QTableWidget(0, len(self.COLS))
        self._table.setHorizontalHeaderLabels(self.COLS)
        self._table.verticalHeader().setVisible(False)
        self._table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self._table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self._table.horizontalHeader().setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        self._table.itemSelectionChanged.connect(self._show_problems)
        lay.addWidget(self._table, 2)
        lay.addWidget(QLabel("Problem lines in selected file:"))
        self._problems = QPlainTextEdit(); self._problems.setReadOnly(True)
        lay.addWidget(self._problems, 1)
        act = QHBoxLayout()
        act.addWidget(QLabel("Selected files:"))
        for text, action, tip in (
                ("Repair lines", 'repair', "Fix swapped interior/name and extra fields, keep layout"),
                ("Scale 0 -> 1", 'scale_1', "Set 0,0,0 scale to 1,1,1"),
                ("Scale 1 -> 0", 'scale_0', "Set 1,1,1 scale to 0,0,0"),
                ("To VC", 'to_vc', "Any layout -> VC: interior added (SA flag bits dropped), scale 1,1,1, SA LOD removed"),
                ("To SA", 'to_sa', "Any layout -> SA: scale removed, LOD index kept or -1"),
                ("To LC/III", 'to_iii', "Any layout -> GTA III: interior removed, scale 1,1,1")):
            b = QPushButton(text); b.setToolTip(tip)
            b.clicked.connect(lambda _=False, a=action: self._apply(a))
            act.addWidget(b)
        act.addStretch()
        close = QPushButton("Close"); close.clicked.connect(self.accept)
        act.addWidget(close)
        lay.addLayout(act)
        if self._loaded:
            self._add_loaded()

    def _add_loaded(self): #vers 1
        self._add_paths(self._loaded)

    def _add_files(self): #vers 1
        paths, _ = QFileDialog.getOpenFileNames(self, "Add IPL files", "", "IPL Files (*.ipl);;All Files (*)")
        self._add_paths(paths)

    def _add_folder(self): #vers 1
        d = QFileDialog.getExistingDirectory(self, "Add IPL folder")
        if d:
            self._add_paths([os.path.join(r, f) for r, _, fs in os.walk(d)
                             for f in fs if f.lower().endswith('.ipl')])

    def _add_paths(self, paths): #vers 1
        have = {r['path'] for r in self._results}
        for p in sorted(paths):
            if p in have or not os.path.isfile(p):
                continue
            try:
                res = scan_ipl_format(p)
            except OSError:
                continue
            self._results.append(res)
            self._table.insertRow(self._table.rowCount())
            self._fill_row(self._table.rowCount() - 1, res)

    def _fill_row(self, row, res): #vers 1
        vals = [os.path.basename(res['path']), res['layout'] or '-', res['lines'],
                res['zero_scale'], res['unit_scale'],
                ', '.join(sorted(res['interiors'])[:6]), len(res['problems'])]
        for c, v in enumerate(vals):
            it = QTableWidgetItem(str(v))
            if c == 0:
                it.setToolTip(res['path'])
            self._table.setItem(row, c, it)

    def _show_problems(self): #vers 1
        rows = sorted({i.row() for i in self._table.selectedItems()})
        out = []
        for r in rows:
            res = self._results[r]
            for ln, why, text in res['problems']:
                out.append(f"{os.path.basename(res['path'])}:{ln}  {why}\n    {text}")
        self._problems.setPlainText("\n".join(out) or "No problems in selected file(s).")

    def _apply(self, action): #vers 1
        rows = sorted({i.row() for i in self._table.selectedItems()})
        if not rows:
            QMessageBox.information(self, "IPL Format Checker", "Select one or more files first.")
            return
        if QMessageBox.question(self, "IPL Format Checker",
                f"Rewrite {len(rows)} file(s)? A timestamped backup of each is kept.") \
                != QMessageBox.StandardButton.Yes:
            return
        total = 0
        for r in rows:
            path = self._results[r]['path']
            total += rewrite_ipl(path, action)
            self._results[r] = scan_ipl_format(path)
            self._fill_row(r, self._results[r])
        self._show_problems()
        QMessageBox.information(self, "IPL Format Checker",
            f"{total} line(s) changed. Reload changed IPLs to see them in the viewport.")
