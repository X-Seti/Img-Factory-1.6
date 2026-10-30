#!/usr/bin/env python3
#this belongs in apps/components/Map_Editor/depends/map_workflow.py - Version: 1
# X-Seti - September23 2026 - IMG Factory 1.6 - Map Workshop diff view and mod package export

"""
Map Workshop workflow - diff of edited IPLs against last load/save or a
save point, and export of changed files as a mod package (folder + zip).
"""

##Methods list -
# diff_keys
# export_mod_package
# package_name
# _fmt

##class MapDiffDialog: -
# __init__
# _activate
# _run

import os
import time
import zipfile
from collections import Counter

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (QComboBox, QDialog, QHBoxLayout, QLabel, QPushButton, QTreeWidget,
                             QTreeWidgetItem, QVBoxLayout)


def diff_keys(base, cur): #vers 1
    """Instance state tuples -> (changed [(old, new)], added, removed), paired by model ID."""
    b, c = Counter(base), Counter(cur)
    removed = list((b - c).elements())
    added = list((c - b).elements())
    changed, left_add = [], []
    pool = {}
    for r in removed:
        pool.setdefault(r[0], []).append(r)
    for a in added:
        lst = pool.get(a[0])
        if lst:
            changed.append((lst.pop(0), a))
        else:
            left_add.append(a)
    left_rem = [r for lst in pool.values() for r in lst]
    return changed, left_add, left_rem


def _fmt(k): #vers 1
    return f"{k[1]} (id {k[0]}) at ({k[3]:.2f}, {k[4]:.2f}, {k[5]:.2f})"


class MapDiffDialog(QDialog):
    """Per-IPL list of changed / added / removed objects; double-click centres on it."""

    def __init__(self, workshop): #vers 1
        super().__init__(workshop)
        self.ws = workshop
        self.setWindowTitle("Map Changes")
        self.resize(820, 560)
        lay = QVBoxLayout(self)
        row = QHBoxLayout()
        row.addWidget(QLabel("Compare with:"))
        self.src = QComboBox()
        self.src.addItem("Last load / save", None)
        from apps.components.Map_Editor.depends.map_changes import list_savepoints
        from datetime import datetime
        for sp in list_savepoints(workshop._savepoint_dir()):
            self.src.addItem(f"Save point {datetime.fromtimestamp(sp['created']):%Y-%m-%d %H:%M:%S} "
                             f"{sp['label']}", sp['path'])
        row.addWidget(self.src, 1)
        b = QPushButton("Refresh")
        b.clicked.connect(self._run)
        row.addWidget(b)
        lay.addLayout(row)
        self.info = QLabel("")
        lay.addWidget(self.info)
        self.tree = QTreeWidget()
        self.tree.setHeaderLabels(["IPL / change", "Detail"])
        self.tree.itemDoubleClicked.connect(self._activate)
        lay.addWidget(self.tree, 1)
        self.src.currentIndexChanged.connect(lambda _i: self._run())
        self._run()

    def _run(self): #vers 1
        from apps.components.Map_Editor.depends.map_changes import (
            _inst_key, _freeze, CATEGORIES, read_savepoint, state_from_json)
        ws = self.ws
        loader = ws._world_loader
        ws._refresh_dirty_ipls()
        path = self.src.currentData()
        cur_by = {}
        for i in loader.instances:
            cur_by.setdefault(i.source_ipl, []).append(_inst_key(i))
        other_changed = {}
        if path is None:
            names = sorted(ws._dirty_ipls)
            src = getattr(ws, '_inst_baseline_src', {})
            base_by = {}
            for key, k in ws._inst_baseline.items():
                if src.get(key) in ws._dirty_ipls:
                    base_by.setdefault(src[key], []).append(k)
        else:
            snap = state_from_json(read_savepoint(path).get('ipls', {}))
            names = sorted(snap)
            base_by = {n: [_inst_key(i) for i in snap[n].get('inst', [])] for n in names}
            for n in names:
                diff = []
                for cat, attr in CATEGORIES[1:]:
                    now = [o for o in getattr(loader, attr, None) or []
                           if (o.get('source_ipl') if isinstance(o, dict) else o.source_ipl) == n]
                    if _freeze(now) != _freeze(snap[n].get(cat, [])):
                        diff.append(cat)
                other_changed[n] = diff
        self.tree.clear()
        totals = [0, 0, 0]
        for n in names:
            changed, added, removed = diff_keys(base_by.get(n, []), cur_by.get(n, []))
            top = QTreeWidgetItem([n, f"{len(changed)} changed, {len(added)} added, {len(removed)} removed"])
            for old, new in changed:
                it = QTreeWidgetItem(["changed", f"{_fmt(old)}  ->  ({new[3]:.2f}, {new[4]:.2f}, {new[5]:.2f})"
                                      + ("" if old[6:10] == new[6:10] else "  rotated")])
                it.setData(0, Qt.ItemDataRole.UserRole, (n, new))
                top.addChild(it)
            for a in added:
                it = QTreeWidgetItem(["added", _fmt(a)])
                it.setData(0, Qt.ItemDataRole.UserRole, (n, a))
                top.addChild(it)
            for r in removed:
                top.addChild(QTreeWidgetItem(["removed", _fmt(r)]))
            cats = other_changed.get(n)
            if path is None and not (changed or added or removed):
                top.addChild(QTreeWidgetItem(["sections", "cull / zone / path / garage / enex / occl / auzo edits"]))
            elif cats:
                top.addChild(QTreeWidgetItem(["sections", ", ".join(cats) + " differ"]))
            totals[0] += len(changed); totals[1] += len(added); totals[2] += len(removed)
            self.tree.addTopLevelItem(top)
        self.tree.expandAll()
        self.tree.resizeColumnToContents(0)
        self.info.setText(f"{len(names)} IPL(s): {totals[0]} changed, {totals[1]} added, {totals[2]} removed")

    def _activate(self, item, _col): #vers 1
        """Double-click: centre on the current object matching this row."""
        d = item.data(0, Qt.ItemDataRole.UserRole)
        if not d:
            return
        from apps.components.Map_Editor.depends.map_changes import _inst_key
        n, key = d
        for i in self.ws._world_loader.instances:
            if i.source_ipl == n and _inst_key(i) == key:
                self.ws._center_on_instance(i)
                return


def export_mod_package(folder, name, files, readme_lines, make_zip=True) -> str: #vers 1
    """Write {relative path: bytes} into folder/name (+ .zip); returns the package folder."""
    root = os.path.join(folder, name)
    for rel, data in files.items():
        p = os.path.join(root, rel)
        os.makedirs(os.path.dirname(p), exist_ok=True)
        with open(p, 'wb') as f:
            f.write(data)
    with open(os.path.join(root, 'readme.txt'), 'w', encoding='utf-8') as f:
        f.write("\n".join(readme_lines) + "\n")
    if make_zip:
        with zipfile.ZipFile(root + '.zip', 'w', zipfile.ZIP_DEFLATED) as z:
            for base, _dirs, fns in os.walk(root):
                for fn in fns:
                    full = os.path.join(base, fn)
                    z.write(full, os.path.relpath(full, folder))
    return root


def package_name(game) -> str: #vers 1
    return f"mod_{game}_{time.strftime('%Y%m%d_%H%M%S')}"
