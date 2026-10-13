#!/usr/bin/env python3
#this belongs in apps/components/Map_Editor/depends/map_checks.py - Version: 1
# X-Seti - September23 2026 - IMG Factory 1.6 - Map Workshop integrity checks

"""
Map Workshop integrity checks - LOD links, IDE ID conflicts, missing assets,
instance / model-ID limits, plus the Map Checks dialog that shows them.
"""

##Methods list -
# check_lod_links
# free_id_ranges
# id_conflicts
# instance_counts
# missing_assets
# scan_ide_ids

##class MapChecksDialog: -
# __init__
# _activate
# _build_assets_tab
# _build_ids_tab
# _build_limits_tab
# _build_lod_tab
# _fill
# _fix_lod
# _run_assets
# _run_ids
# _run_limits
# _run_lod
# _tab
# _tree

import os
from collections import defaultdict

from PyQt6.QtCore import Qt
from PyQt6.QtGui import QBrush, QColor
from PyQt6.QtWidgets import (QDialog, QHBoxLayout, QLabel, QPushButton, QSpinBox, QTabWidget,
                             QTreeWidget, QTreeWidgetItem, QVBoxLayout, QWidget, QFormLayout)

ID_SECTIONS = ('objs', 'tobj', 'anim', 'cars', 'peds', 'weap', 'hier', 'tanm')

# Engine array sizes (re3 / reVC source; SA from its exe). Editable in the dialog.
DEFAULT_LIMITS = {
    'gta3': {'model_infos': 5500, 'buildings': 5500},
    'vc':   {'model_infos': 6500, 'buildings': 7000},
    'sa':   {'model_infos': 20000, 'buildings': 13000},
    'sol':  {'model_infos': 20000, 'buildings': 13000},
}


def check_lod_links(instances, lod_parent): #vers 1
    """[(inst, problem, lod_index)] for LOD indices that don't resolve cleanly."""
    by_ipl = defaultdict(list)
    for i in instances:
        by_ipl[i.source_ipl].append(i)
    out = []
    for i in instances:
        if i.lod_index is None or i.lod_index < 0:
            continue
        parent = by_ipl.get(lod_parent(i.source_ipl), [])
        if i.lod_index >= len(parent):
            out.append((i, f"index {i.lod_index} past end of {lod_parent(i.source_ipl)} ({len(parent)})", i.lod_index))
            continue
        t = parent[i.lod_index]
        if t is i:
            out.append((i, "points at itself", i.lod_index))
        elif not t.model_name.lower().startswith('lod'):
            out.append((i, f"target {t.model_name} is not a LOD model (warning)", i.lod_index))
    return out


def scan_ide_ids(ide_paths): #vers 1
    """{model_id: [(name, file, line, section)]} over every ID-carrying IDE section."""
    ids = defaultdict(list)
    for path in ide_paths:
        try:
            with open(path, 'rb') as f:
                lines = f.read().decode('latin-1').splitlines()
        except OSError:
            continue
        sec = None
        for n, raw in enumerate(lines, 1):
            line = raw.split('#')[0].strip()
            if not line:
                continue
            low = line.lower()
            if low == 'end':
                sec = None
                continue
            if sec is None and ',' not in line:
                sec = low
                continue
            if sec in ID_SECTIONS:
                parts = [p.strip() for p in line.split(',')]
                try:
                    mid = int(parts[0])
                except (ValueError, IndexError):
                    continue
                ids[mid].append((parts[1] if len(parts) > 1 else '', os.path.basename(path), n, sec))
    return ids


def id_conflicts(ids): #vers 1
    """(duplicate IDs, names defined under more than one ID)."""
    dup = {k: v for k, v in ids.items() if len(v) > 1}
    names = defaultdict(set)
    for k, v in ids.items():
        for name, *_ in v:
            names[name.lower()].add(k)
    multi = {n: sorted(s) for n, s in names.items() if len(s) > 1 and n}
    return dup, multi


def free_id_ranges(used, max_id, min_len=1): #vers 1
    """[(first, last)] runs of unused IDs below max_id."""
    out, start = [], None
    for k in range(max_id):
        if k not in used:
            if start is None:
                start = k
        elif start is not None:
            if k - start >= min_len:
                out.append((start, k - 1))
            start = None
    if start is not None and max_id - start >= min_len:
        out.append((start, max_id - 1))
    return out


def missing_assets(objects, cache): #vers 1
    """[(obj, missing list)] for IDE objects whose DFF / TXD / COL isn't indexed."""
    out = []
    for o in objects:
        if o.section not in ('objs', 'tobj', 'anim'):
            continue
        miss = []
        if not cache.is_dff_indexed(o.model_name):
            miss.append('DFF')
        if o.txd_name and o.txd_name.lower() != 'null' and not cache.is_txd_indexed(o.txd_name):
            miss.append('TXD')
        if not cache.is_col_indexed(o.model_name):
            miss.append('COL')
        if miss:
            out.append((o, miss))
    return out


def instance_counts(instances): #vers 1
    """{ipl name: instance count} in load order."""
    c = {}
    for i in instances:
        c[i.source_ipl] = c.get(i.source_ipl, 0) + 1
    return c


class MapChecksDialog(QDialog):
    """Four tabs: LOD links, IDE IDs, missing assets, limits. Double-click jumps to the object."""

    def __init__(self, workshop): #vers 1
        super().__init__(workshop)
        self.ws = workshop
        self.setWindowTitle("Map Checks")
        self.resize(820, 560)
        lay = QVBoxLayout(self)
        self.tabs = QTabWidget()
        lay.addWidget(self.tabs)
        self._build_lod_tab()
        self._build_ids_tab()
        self._build_assets_tab()
        self._build_limits_tab()

    def _tree(self, headers): #vers 1
        t = QTreeWidget()
        t.setHeaderLabels(headers)
        t.setRootIsDecorated(False)
        t.setUniformRowHeights(True)
        t.itemDoubleClicked.connect(self._activate)
        return t

    def _tab(self, title, tree, buttons): #vers 1
        w = QWidget()
        v = QVBoxLayout(w)
        row = QHBoxLayout()
        info = QLabel("")
        row.addWidget(info, 1)
        for text, fn in buttons:
            b = QPushButton(text)
            b.clicked.connect(fn)
            row.addWidget(b)
        v.addLayout(row)
        v.addWidget(tree, 1)
        self.tabs.addTab(w, title)
        return info

    def _fill(self, tree, rows, payloads=None, warn_col=None): #vers 1
        tree.clear()
        for k, r in enumerate(rows):
            it = QTreeWidgetItem([str(x) for x in r])
            if payloads is not None:
                it.setData(0, Qt.ItemDataRole.UserRole, payloads[k])
            if warn_col is not None and 'warning' not in str(r[warn_col]):
                it.setForeground(warn_col, QBrush(QColor(230, 70, 70)))
            tree.addTopLevelItem(it)
        for c in range(tree.columnCount()):
            tree.resizeColumnToContents(c)

    def _activate(self, item, _col): #vers 1
        """Double-click: select the object in the viewport, or jump to its IDE line."""
        p = item.data(0, Qt.ItemDataRole.UserRole)
        if p is None:
            return
        if isinstance(p, tuple):
            self.ws._jump_to_ide_line(p[0], p[1])
        else:
            self.ws._center_on_instance(p)

    # LOD
    def _build_lod_tab(self): #vers 1
        self.lod_tree = self._tree(["Object", "IPL", "LOD index", "Problem"])
        self.lod_info = self._tab("LOD links", self.lod_tree,
                                  [("Check", self._run_lod), ("Clear broken links", self._fix_lod)])

    def _run_lod(self): #vers 1
        loader = self.ws._world_loader
        res = check_lod_links(loader.instances, self.ws._lod_parent_ipl)
        self._lod_res = res
        self._fill(self.lod_tree, [(i.model_name, i.source_ipl, idx, p) for i, p, idx in res],
                   [i for i, _p, _x in res], warn_col=3)
        broken = sum(1 for _i, p, _x in res if 'warning' not in p)
        self.lod_info.setText(f"{broken} broken, {len(res) - broken} warning(s)")

    def _fix_lod(self): #vers 1
        """Set broken (not warning) links to -1, undoable."""
        res = getattr(self, '_lod_res', None)
        if res is None:
            self._run_lod()
            res = self._lod_res
        fix = [(i, i.lod_index) for i, p, _x in res if 'warning' not in p]
        if not fix:
            return

        def _apply(k):
            for i, old in fix:
                i.lod_index = (old, -1)[k]
        _apply(1)
        self.ws._push_map_undo(lambda: _apply(0), lambda: _apply(1), f"Clear {len(fix)} broken LOD link(s)")
        self._run_lod()

    # IDs
    def _build_ids_tab(self): #vers 1
        self.id_tree = self._tree(["ID", "Name", "File", "Line", "Section / note"])
        self.id_info = self._tab("IDE IDs", self.id_tree, [("Check", self._run_ids)])

    def _run_ids(self): #vers 1
        loader = self.ws._world_loader
        paths = [p for _ph, et, p, ok in loader.load_log if et == "IDE" and ok]
        ids = scan_ide_ids(paths)
        dup, multi = id_conflicts(ids)
        rows, pay = [], []
        for mid in sorted(dup):
            for name, fn, ln, sec in dup[mid]:
                rows.append((mid, name, fn, ln, f"{sec} - duplicate ID"))
                pay.append((fn, ln))
        for name in sorted(multi):
            rows.append((", ".join(map(str, multi[name])), name, "", "", "same name, several IDs"))
            pay.append(None)
        game = getattr(loader, 'game', 'sa')
        cap = self.ws.map_settings.get(f'limit_{game}_model_infos') or DEFAULT_LIMITS.get(game, {}).get('model_infos', 20000)
        used = set(ids)
        placed = {i.model_id for i in loader.instances}
        unused = sorted(k for k in used if k not in placed)
        free = free_id_ranges(used, cap, min_len=10)
        for a, b in free[:40]:
            rows.append((f"{a}-{b}", "", "", "", f"free ({b - a + 1} IDs)"))
            pay.append(None)
        self._fill(self.id_tree, rows, pay)
        self.id_info.setText(f"{len(dup)} duplicate ID(s), {len(multi)} name(s) with several IDs, "
                             f"{len(unused)} defined but not placed, highest ID {max(used) if used else 0} / {cap - 1}")

    # assets
    def _build_assets_tab(self): #vers 1
        self.asset_tree = self._tree(["ID", "Model", "TXD", "Missing", "IDE"])
        self.asset_info = self._tab("Missing assets", self.asset_tree, [("Check", self._run_assets)])

    def _run_assets(self): #vers 1
        loader = self.ws._world_loader
        cache = getattr(self.ws, '_model_cache', None)
        if cache is None:
            self.asset_info.setText("Model cache not ready - load a world with its IMG archives first")
            return
        res = missing_assets(loader.objects.values(), cache)
        self._fill(self.asset_tree, [(o.model_id, o.model_name, o.txd_name, " ".join(m),
                                      f"{o.source_ide}:{o.line_no}") for o, m in res],
                   [(o.source_ide, o.line_no) for o, _m in res])
        self.asset_info.setText(f"{len(res)} object(s) with missing files")

    # limits
    def _build_limits_tab(self): #vers 1
        w = QWidget()
        v = QVBoxLayout(w)
        form = QFormLayout()
        game = getattr(self.ws._world_loader, 'game', 'sa')
        d = DEFAULT_LIMITS.get(game, DEFAULT_LIMITS['sa'])
        self.lim_models = QSpinBox(); self.lim_models.setRange(1, 1000000)
        self.lim_models.setValue(int(self.ws.map_settings.get(f'limit_{game}_model_infos') or d['model_infos']))
        self.lim_build = QSpinBox(); self.lim_build.setRange(1, 10000000)
        self.lim_build.setValue(int(self.ws.map_settings.get(f'limit_{game}_buildings') or d['buildings']))
        form.addRow(f"Model IDs ({game}):", self.lim_models)
        form.addRow("Building pool (approx. - dynamic objects count elsewhere):", self.lim_build)
        v.addLayout(form)
        row = QHBoxLayout()
        self.lim_info = QLabel("")
        row.addWidget(self.lim_info, 1)
        b = QPushButton("Check")
        b.clicked.connect(self._run_limits)
        row.addWidget(b)
        v.addLayout(row)
        self.lim_tree = self._tree(["IPL / stream", "Instances", "Share of pool"])
        v.addWidget(self.lim_tree, 1)
        self.tabs.addTab(w, "Limits")

    def _run_limits(self): #vers 1
        loader = self.ws._world_loader
        game = getattr(loader, 'game', 'sa')
        self.ws.map_settings.set(f'limit_{game}_model_infos', self.lim_models.value())
        self.ws.map_settings.set(f'limit_{game}_buildings', self.lim_build.value())
        counts = instance_counts(loader.instances)
        total = sum(counts.values())
        pool = self.lim_build.value()
        rows = sorted(counts.items(), key=lambda kv: -kv[1])
        self._fill(self.lim_tree, [(n, c, f"{100.0 * c / pool:.1f}%") for n, c in rows])
        top = max((i.model_id for i in loader.instances), default=0)
        pct = 100.0 * total / pool
        self.lim_info.setText(f"{total} instances ({pct:.0f}% of {pool}), highest placed ID {top} of "
                              f"{self.lim_models.value() - 1}" + ("  - OVER LIMIT" if pct > 100 or top >= self.lim_models.value() else ""))
        self.lim_info.setStyleSheet("color: rgb(230,70,70);" if pct > 90 else "")
