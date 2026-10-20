#this belongs in apps/components/Path_Workshop/depends/path_nodes_func.py - Version: 9
# X-Seti - October 19 2026 - IMG Factory 1.6 - Path Workshop node logic

"""
Path node loading, tree, properties, editing, undo and save.
IPL (VC/SOL) and IDE (III) path groups, SA nodesN.dat, tracks.
Chase paths (III), roadblocks and car recordings (SA), LCS/VCS game.dtz paths read only.
"""

##Methods list -
# _add_node_at
# _begin_node_drag
# _build_path_records
# _create_nodes_panel
# _create_props_panel
# _delete_group
# _deferred_rebuild
# _delete_selected_node
# _drag_node_to
# _end_node_drag
# _fill_tree_item
# _ide_set_world
# _init_path_system
# _link_click
# _load_path_file
# _load_paths
# _mark_dirty
# _node_label
# _on_group_edited
# _on_prop_edited
# _on_tree_expanded
# _on_tree_selected
# _layer_shown
# _pick_node
# _populate_nodes_tree
# _pn_visible
# _prop_widget
# _push_undo
# _reverse_group
# _save_paths
# _select_group
# _select_node
# _shift_rotate_tracks
# _show_node_props
# _snapshot
# _tree_item_for
# _undo_path_edit

##class PathRec: -
# __init__

import os

import numpy as np

from PyQt6.QtCore import Qt, QTimer
from PyQt6.QtWidgets import (QDoubleSpinBox, QFileDialog, QFormLayout, QFrame, QGridLayout,
                             QLabel, QLineEdit, QMessageBox, QProgressDialog,
                             QApplication, QSpinBox, QTreeWidget, QTreeWidgetItem,
                             QVBoxLayout, QWidget)

# editable fields per kind: (attr, label, kind, lo, hi)
_FIELDS = {
    'ipl': [('node_type', "Type", int, 0, 2), ('next_id', "Next", int, -1, 11),
            ('cross', "+Road", int, 0, 1),
            ('x', "X", float, -9000, 9000), ('y', "Y", float, -9000, 9000),
            ('z', "Z", float, -2000, 2000), ('median', "Median", float, -500, 500),
            ('left', "Left", int, 0, 15), ('right', "Right", int, 0, 15),
            ('flag1', "Flag 1", int, -999999, 999999), ('flag2', "Flag 2", int, -999999, 999999),
            ('flag3', "Flag 3", float, 0, 255)],
    'ide': [('node_type', "Type", int, 0, 2), ('next_id', "Next", int, -1, 11),
            ('is_crossroad', "+Road", int, 0, 1),
            ('x_rel', "X rel", float, -99999, 99999), ('y_rel', "Y rel", float, -99999, 99999),
            ('z_rel', "Z rel", float, -99999, 99999), ('median', "Median", float, -500, 500),
            ('left', "Left", int, 0, 15), ('right', "Right", int, 0, 15)],
    'sa':  [('x', "X", float, -4095, 4095), ('y', "Y", float, -4095, 4095),
            ('z', "Z", float, -4095, 4095), ('path_width', "Width", float, 0, 31),
            ('flood_fill', "Flood", int, 0, 255), ('flags', "Flags", 'hex', 0, 0)],
}
_FIELDS['trk'] = [('x', "X", float, -9000, 9000), ('y', "Y", float, -9000, 9000),
                  ('z', "Z", float, -2000, 2000)]
_FIELDS['dtz'] = []                     # LCS/VCS compiled paths: view only
_GROUP_FIELDS = {
    'ipl': [('header_a', "G-Type", int, -1, 99), ('header_b', "G-Value", int, -1, 999999)],
    'ide': [('group_type', "G-Type", str, 0, 0), ('model_id', "Model ID", int, 0, 999999),
            ('model_name', "Model name", str, 0, 0)],
}
_VIEW_ONLY = ('chase', 'rblk', 'dtz', 'rrr')
LAYERS = ('car', 'ped', 'other', 'trk', 'chase', 'rblk', 'rrr')
_LAYER = {False: 0, True: 1, None: 2, 'trk': 3, 'chase': 4, 'rblk': 5, 'rrr': 6}   # rec.ped -> layer
PATH_STYLE_DEFAULTS = {'car': '#50b4ff', 'ped': '#5adc6e', 'other': '#ffaa28', 'trk': '#c8c8d2',
                       'chase': '#ff50dc', 'rblk': '#ff3030', 'rrr': '#ffd23c',
                       'line_w': 1.5, 'node_px': 3}
_PRESET_FOR_GAME = {'gta3': "III PC", 'vc': "VC PC", 'sa': "SA PC", 'sol': "SOL"}


class PathRec: #vers 1
    """One drawable node: world position plus its source object."""
    __slots__ = ('kind', 'obj', 'group', 'gi', 'inst', 'x', 'y', 'z', 'ped', 'key')

    def __init__(self, kind, obj, group, gi, x, y, z, ped, key, inst=None): #vers 1
        self.kind, self.obj, self.group, self.gi = kind, obj, group, gi
        self.x, self.y, self.z, self.ped, self.key, self.inst = x, y, z, ped, key, inst


class PathNodesMixin:
    """Mixed into PathWorkshop."""

    def _init_path_system(self): #vers 3
        self._loader = None
        self._paths_dat = ""
        self._pn = []                 # PathRec list
        self._pn_edges = []           # (i, j) record pairs
        self._pn_key = {}             # key -> first record index
        self._selected_node = -1
        self._link_from = -1
        self._drag = None             # (index, snapshot) while dragging
        self._undo_stack = []
        self._dirty = set()           # ('ipl'|'ide', basename) / ('sa', area)
        self._sa_changed = {}         # area -> {id: SAPathNode}
        self._show_car = True
        self._show_ped = True
        self._show_extra = True       # tracks, chase, roadblocks
        self._pn_xy = np.zeros((0, 2)); self._pn_cls = np.zeros(0, np.int8); self._pn_edit = np.zeros(0, bool)
        self._pn_edge_arr = np.zeros((0, 2), np.int64)
        self._path_style = dict(PATH_STYLE_DEFAULTS, **(self.RAD_settings.get('path_display') or {}))

    # - Panels

    def _create_nodes_panel(self): #vers 1
        """Left dock: path files, groups and nodes."""
        panel = QFrame()
        vl = QVBoxLayout(panel)
        vl.setContentsMargins(*self.get_panel_margins())
        self._nodes_tree = QTreeWidget()
        self._nodes_tree.setHeaderLabels(["Path", "Info"])
        self._nodes_tree.setColumnWidth(0, 170)
        self._nodes_tree.itemExpanded.connect(self._on_tree_expanded)
        self._nodes_tree.itemSelectionChanged.connect(self._on_tree_selected)
        vl.addWidget(self._nodes_tree, 1)
        self._nodes_count = QLabel("No paths loaded")
        vl.addWidget(self._nodes_count)
        return panel

    def _create_props_panel(self): #vers 2
        """Right dock: editable fields of the selected node."""
        panel = QFrame()
        vl = QVBoxLayout(panel)
        vl.setContentsMargins(*self.get_panel_margins())
        self._props_title = QLabel("No node selected")
        vl.addWidget(self._props_title)
        self._props_host = QWidget()
        self._props_grid = QGridLayout(self._props_host)   # label, field x3 per row
        self._props_grid.setContentsMargins(0, 0, 0, 0)
        self._props_grid.setHorizontalSpacing(4)
        for c in (1, 3, 5):
            self._props_grid.setColumnStretch(c, 1)
        vl.addWidget(self._props_host)
        vl.addStretch(1)
        return panel

    # - Loading

    def _load_paths(self, path=None): #vers 6
        """Load paths; several files at once; Shift+Load adds to the loaded set."""
        from apps.methods.gta_dat_parser import (GTAWorldLoader, dat_game_root,
                                                 detect_game_from_dat_filename)
        shift = QApplication.keyboardModifiers() & Qt.KeyboardModifier.ShiftModifier
        append = bool(shift) and self._loader is not None
        if not append and not self._confirm_discard_paths():
            return
        if path:
            paths = list(path) if isinstance(path, (list, tuple)) else [path]   # drop: list
        else:
            paths, _ = QFileDialog.getOpenFileNames(
                self, "Load Paths - Shift+click picks several (e.g. SA paths.ipl..paths5.ipl)",
                self._dialog_dir(),
                "Path files (gta3.dat gta_vc.dat gta.dat gta_sol.dat gtasol.dat *.ipl nodes*.dat "
                "tracks*.dat flight*.dat spath*.dat chase*.dat roadblox.dat *.img *.rrr *.dtz);;All files (*)")
        if not paths:
            return
        self._remember_dir(paths[0])
        path = paths[0]
        game = detect_game_from_dat_filename(path) if len(paths) == 1 else None
        if game and append:
            self._set_status("Shift+Load adds path files - a game .dat replaces, load it without Shift")
            return
        if game:
            prog = QProgressDialog("Loading paths…", None, 0, 0, self)
            prog.setWindowModality(Qt.WindowModality.WindowModal)
            prog.show(); QApplication.processEvents()
            loader = GTAWorldLoader(game)
            ok = loader.load_from_dat(path, dat_game_root(path, game))
            prog.close()
            if not ok:
                QMessageBox.warning(self, "Load Paths", "\n".join(loader.stats.errors[:10]) or path)
                return
        else:
            loader = self._loader if append else None
            for p in paths:
                loader = self._load_path_file(p, loader) or loader
            if loader is None:
                return
            game = loader.game
        self._loader, self._paths_dat = loader, path
        self._file_abs = {os.path.basename(p).lower(): p for _ph, t, p, good in loader.load_log
                          if t in ("IPL", "IDE", "TRACKS") and good}
        if not append:
            self._undo_stack.clear(); self._dirty.clear(); self._sa_changed.clear()
        self._selected_node = self._link_from = -1
        preset = _PRESET_FOR_GAME.get(game)
        if loader.stories:                             # (game, platform) from game.dtz
            sg, sp = loader.stories
            preset = f"{sg.upper()} {'PSP' if sp == 'psp' else 'PC'}"
        if preset and self._game_combo.currentText() != preset:
            if self._tile_rgba:
                self._set_status(f"Radar is not {preset} - load the matching radar")
            else:
                self._on_game_changed(preset)
        self._build_path_records()
        self._populate_nodes_tree()
        self._radar.update()
        names = ", ".join(os.path.basename(p) for p in paths[:4]) + (" …" if len(paths) > 4 else "")
        self._set_status(f"{'Added' if append else 'Loaded'} {names} - {len(self._pn)} path nodes")

    def _load_path_file(self, path, loader=None): #vers 7
        """One path file into loader (new one if None), game from the Game preset."""
        import re
        from apps.methods.gta_dat_parser import GTAWorldLoader, IPLParser
        from apps.methods.sa_path_parser import load_all_nodes_dat_from_dir
        name = os.path.basename(path).lower()
        preset = self._game_combo.currentText()
        game = next((g for g, p in _PRESET_FOR_GAME.items() if p == preset), 'vc')
        is_nodes = re.match(r'nodes\d+\.dat$', name)
        if is_nodes or name in ('roadblox.dat',) or name.endswith(('.img', '.rrr')):
            game = 'sa'
        loader = loader or GTAWorldLoader(game)
        if name.endswith('.dtz'):                      # LCS / VCS compiled paths
            from apps.methods.stories_dtz import find_stories_root
            root = find_stories_root(path)
            if not root:
                QMessageBox.warning(self, "Load Paths", f"No LCS / VCS game layout around:\n{path}")
                return None
            loader.load_stories(root)
        elif name.endswith('.ipl'):
            parser = IPLParser(game)
            if not parser.parse(path):
                QMessageBox.warning(self, "Load Paths", f"Could not read {name}")
                return None
            loader.paths += parser.paths
            loader.load_log.append(("file", "IPL", path, True))
            if not parser.paths:
                self._set_status(f"{name}: no path section")
        elif is_nodes:
            loader.sa_nodes.update(load_all_nodes_dat_from_dir(os.path.dirname(path)))
        elif name.endswith('.img'):                    # gta3.img nodesN.dat / carrec.img .rrr
            from apps.methods.img_core_classes import IMGFile
            from apps.methods.sa_path_parser import (find_nodes_dat_in_img, load_carrec_img,
                                                     load_nodes_dat_from_img_entry)
            arc = IMGFile(path)
            arc.open()
            for area, entry in find_nodes_dat_in_img(arc).items():
                pf = load_nodes_dat_from_img_entry(arc, entry, area)
                if pf is not None:
                    loader.sa_nodes[area] = pf
            loader.car_recordings.update(load_carrec_img(path))
            if not loader.sa_nodes and not loader.car_recordings:
                QMessageBox.warning(self, "Load Paths", f"No nodesN.dat or .rrr inside {name}")
                return None
        elif name.endswith('.rrr'):                    # one extracted car recording
            from apps.methods.sa_path_parser import parse_rrr
            with open(path, 'rb') as fh:
                loader.car_recordings[os.path.basename(path)] = parse_rrr(fh.read(), os.path.basename(path))
        elif re.match(r'(tracks|flight|spath)\w*\.dat$', name):
            pts = loader._parse_tracks_file(path, os.path.basename(path))
            if pts:
                loader.tracks[os.path.basename(path)] = pts
                loader.load_log.append(("file", "TRACKS", path, True))
        elif re.match(r'chase\d+\.dat$', name):
            loader.load_chase_dat(os.path.dirname(os.path.dirname(path)))   # data/paths/chaseN
        elif name == 'roadblox.dat':
            loader.load_sa_roadblox(os.path.dirname(os.path.dirname(path)))
            loader.sa_nodes.update(load_all_nodes_dat_from_dir(os.path.dirname(path)))
        else:
            QMessageBox.warning(self, "Load Paths",
                                f"Not a path file:\n{path}\n\nUse the game .dat, an .ipl, "
                                "nodesN.dat, gta3.img / carrec.img, .rrr, game.dtz, tracks / flight / chase .dat or roadblox.dat")
            return None
        return loader

    def _confirm_discard_paths(self): #vers 1
        """Ask before dropping unsaved path edits."""
        if not self._dirty:
            return True
        r = QMessageBox.question(self, "Unsaved Paths", "Discard unsaved path edits?")
        return r == QMessageBox.StandardButton.Yes

    def _build_path_records(self): #vers 6
        """Flatten all loaded path data into PathRec + edges."""
        from apps.methods.path_nodes import rotate_vector_by_quaternion
        recs, edges, keys = [], [], {}
        ld = self._loader
        if ld is None:
            self._pn, self._pn_edges, self._pn_key = recs, edges, keys
            self._pn_xy = np.zeros((0, 2)); self._pn_cls = np.zeros(0, np.int8); self._pn_edit = np.zeros(0, bool)
            self._pn_edge_arr = np.zeros((0, 2), np.int64)
            return
        for g in ld.paths:                                   # VC / SOL IPL, LCS/VCS dtz
            idx = {}
            dtz = g.source_ipl.endswith('.dtz')
            kind = 'dtz' if dtz else 'ipl'
            ped = g.header_a == (1 if dtz else 0)            # IPL 0 = ped; dtz 0 = car
            for i, n in enumerate(g.nodes):
                if n.node_type == 0:
                    continue
                k = (kind, id(g), i)
                idx[i] = len(recs); keys.setdefault(k, len(recs))
                recs.append(PathRec(kind, n, g, i, n.x, n.y, n.z, ped, k))
            for i, n in enumerate(g.nodes):
                if i in idx and n.next_id in idx:
                    edges.append((idx[i], idx[n.next_id]))
        by_model = {}
        for inst in ld.instances:
            by_model.setdefault(inst.model_id, []).append(inst)
        for g in ld.ide_paths:                               # III IDE
            ped = g.group_type.lower() == 'ped'
            for inst in by_model.get(g.model_id, []):
                idx = {}
                for i, n in enumerate(g.nodes):
                    if n.node_type == 0:
                        continue
                    rx, ry, rz = rotate_vector_by_quaternion(
                        n.x_rel / 16.0, n.y_rel / 16.0, n.z_rel / 16.0,
                        -inst.rot_x, -inst.rot_y, -inst.rot_z, inst.rot_w)
                    k = ('ide', id(g), i)
                    idx[i] = len(recs); keys.setdefault(k, len(recs))
                    recs.append(PathRec('ide', n, g, i, inst.pos_x + rx, inst.pos_y + ry,
                                        inst.pos_z + rz, ped, k, inst))
                for i, n in enumerate(g.nodes):
                    if i in idx and n.next_id in idx:
                        edges.append((idx[i], idx[n.next_id]))
        sa_idx, pairs = {}, set()
        for area, pf in ld.sa_nodes.items():                 # SA nodesN.dat
            for ped, lst in ((False, pf.vehicle_nodes), (True, pf.ped_nodes)):
                for i, n in enumerate(lst):
                    k = ('sa', area, ped, i)
                    sa_idx[k] = keys[k] = len(recs)
                    recs.append(PathRec('sa', n, pf, i, n.x, n.y, n.z, ped, k))
        for area, pf in ld.sa_nodes.items():
            for ped, lst in ((False, pf.vehicle_nodes), (True, pf.ped_nodes)):
                for i, n in enumerate(lst):
                    a = sa_idx[('sa', area, ped, i)]
                    for li in range(n.link_id, n.link_id + n.link_count):
                        if not 0 <= li < len(pf.links):
                            continue
                        lk = pf.links[li]
                        tf = ld.sa_nodes.get(lk.area_id)
                        if tf is None:
                            continue
                        tid = lk.node_id - len(tf.vehicle_nodes) if ped else lk.node_id
                        b = sa_idx.get(('sa', lk.area_id, ped, tid))
                        if b is not None and b != a:
                            pairs.add((min(a, b), max(a, b)))
        edges += sorted(pairs)
        for name, pts in ld.tracks.items():                  # tracks / flight / spath
            base = len(recs)
            for i, t in enumerate(pts):
                k = ('trk', name, i)
                keys[k] = len(recs)
                recs.append(PathRec('trk', t, name, i, t.x, t.y, t.z, 'trk', k))
            edges += [(base + i, base + i + 1) for i in range(len(pts) - 1)]
        for name, frames in ld.chase_paths.items():          # III chase (view only)
            base = len(recs)
            for i, c in enumerate(frames):
                recs.append(PathRec('chase', c, name, i, c.pos_x, c.pos_y, c.pos_z, 'chase', None))
            edges += [(base + i, base + i + 1) for i in range(len(frames) - 1)     # skip teleports
                      if abs(frames[i].pos_x - frames[i + 1].pos_x) + abs(frames[i].pos_y - frames[i + 1].pos_y) < 50]
        for name, frames in ld.car_recordings.items():       # SA car recordings .rrr (view only)
            base = len(recs)
            for i, c in enumerate(frames):
                recs.append(PathRec('rrr', c, name, i, c.pos_x, c.pos_y, c.pos_z, 'rrr', None))
            edges += [(base + i, base + i + 1) for i in range(len(frames) - 1)
                      if abs(frames[i].pos_x - frames[i + 1].pos_x) + abs(frames[i].pos_y - frames[i + 1].pos_y) < 50]
        for rb in ld.sa_roadblocks:                          # SA roadblocks (view only)
            j = keys.get(('sa', rb.area_id, False, rb.node_id))
            if j is not None:
                n = recs[j]
                recs.append(PathRec('rblk', rb, None, 0, n.x, n.y, n.z, 'rblk', None))
        self._pn, self._pn_edges, self._pn_key = recs, edges, keys
        self._pn_xy = np.array([(r.x, r.y) for r in recs], float).reshape(-1, 2)
        self._pn_cls = np.array([_LAYER[r.ped] for r in recs], np.int8)
        self._pn_edit = np.array([r.kind not in _VIEW_ONLY for r in recs], bool)
        self._pn_edge_arr = np.array(edges, np.int64).reshape(-1, 2)
        if hasattr(self, '_nodes_count'):
            self._nodes_count.setText(f"{len(recs)} nodes, {len(edges)} links")

    # - Tree

    def _node_label(self, kind, n, i): #vers 1
        """Tree text for one node."""
        if kind == 'sa':
            return f"Node {n.node_id}", f"{n.x:.1f}, {n.y:.1f}, {n.z:.1f}"
        return f"Node {i}", f"type {n.node_type} next {n.next_id}"

    def _populate_nodes_tree(self): #vers 4
        """Top level: IPL files, IDE models, SA areas (nodes filled on expand)."""
        t = self._nodes_tree
        t.clear()
        ld = self._loader
        if ld is None:
            return
        def lazy(parent, text, info, data): #vers 1
            it = QTreeWidgetItem(parent, [text, info])
            it.setData(0, Qt.ItemDataRole.UserRole, data)
            QTreeWidgetItem(it, [""])                       # expand marker
            return it
        if ld.paths:
            top = QTreeWidgetItem(t, ["IPL paths", f"{len(ld.paths)} groups"])
            files = {}
            for g in ld.paths:
                files.setdefault(g.source_ipl, []).append(g)
            for name, groups in files.items():
                fi = QTreeWidgetItem(top, [name, f"{len(groups)} groups"])
                for gn, g in enumerate(groups):
                    real = sum(1 for n in g.nodes if n.node_type)
                    lazy(fi, f"Group {gn}", f"({g.header_a}, {g.header_b}) {real} nodes", ('grp', id(g)))
        if ld.ide_paths:
            top = QTreeWidgetItem(t, ["IDE paths (III)", f"{len(ld.ide_paths)} groups"])
            for g in ld.ide_paths:
                lazy(top, g.model_name, f"{g.group_type} id {g.model_id}", ('grp', id(g)))
        if ld.tracks:
            top = QTreeWidgetItem(t, ["Tracks / flight", f"{len(ld.tracks)} files"])
            for name, pts in ld.tracks.items():
                lazy(top, name, f"{len(pts)} points", ('trkfile', name))
        if ld.chase_paths:
            top = QTreeWidgetItem(t, ["Chase paths (view)", f"{len(ld.chase_paths)} files"])
            for name, frames in ld.chase_paths.items():
                QTreeWidgetItem(top, [name, f"{len(frames)} frames"])
        if ld.sa_roadblocks:
            QTreeWidgetItem(t, ["Roadblocks (view)", f"{len(ld.sa_roadblocks)}"])
        if ld.car_recordings:
            top = QTreeWidgetItem(t, ["Car recordings (view)", f"{len(ld.car_recordings)} .rrr"])
            for name, frames in sorted(ld.car_recordings.items()):
                secs = frames[-1].time_ms / 1000 if frames else 0
                QTreeWidgetItem(top, [name, f"{len(frames)} frames, {secs:.1f}s"])
        if ld.sa_nodes:
            top = QTreeWidgetItem(t, ["SA nodes", f"{len(ld.sa_nodes)} areas"])
            for area in sorted(ld.sa_nodes):
                pf = ld.sa_nodes[area]
                ai = QTreeWidgetItem(top, [f"Area {area}", ""])
                lazy(ai, "Vehicle", f"{len(pf.vehicle_nodes)} nodes", ('sa', area, False))
                lazy(ai, "Ped", f"{len(pf.ped_nodes)} nodes", ('sa', area, True))

    def _fill_tree_item(self, it): #vers 3
        """Replace an expand marker with node rows."""
        if it.childCount() != 1 or it.child(0).text(0):
            return
        it.takeChild(0)
        data = it.data(0, Qt.ItemDataRole.UserRole)
        ld = self._loader
        if data[0] == 'trkfile':
            for i, p in enumerate(ld.tracks[data[1]]):
                c = QTreeWidgetItem(it, [f"Point {i}", f"{p.x:.1f}, {p.y:.1f}, {p.z:.1f}"])
                c.setData(0, Qt.ItemDataRole.UserRole, ('trk', data[1], i))
            return
        if data[0] == 'grp':
            g = next(g for g in list(ld.paths) + list(ld.ide_paths) if id(g) == data[1])
            kind = ('dtz' if g.source_ipl.endswith('.dtz') else 'ipl') if g in ld.paths else 'ide'
            for i, n in enumerate(g.nodes):
                c = QTreeWidgetItem(it, list(self._node_label(kind, n, i)))
                c.setData(0, Qt.ItemDataRole.UserRole, (kind, id(g), i))
        else:
            _s, area, ped = data
            pf = ld.sa_nodes[area]
            for i, n in enumerate(pf.ped_nodes if ped else pf.vehicle_nodes):
                c = QTreeWidgetItem(it, list(self._node_label('sa', n, i)))
                c.setData(0, Qt.ItemDataRole.UserRole, ('sa', area, ped, i))

    def _on_tree_expanded(self, it): #vers 1
        self._fill_tree_item(it)

    def _on_tree_selected(self): #vers 3
        """Tree node clicked: select and centre it on the map."""
        items = self._nodes_tree.selectedItems()
        if not items:
            return
        key = items[0].data(0, Qt.ItemDataRole.UserRole)
        if key is None or key[0] not in ('ipl', 'ide', 'sa', 'trk', 'dtz') or key not in self._pn_key:
            return
        self._select_node(self._pn_key[key], from_tree=True)
        r = self._pn[self._selected_node]
        sx, sy = self._radar.world_to_screen(r.x, r.y)
        self._radar._pan_x += int(self._radar.width() / 2 - sx)
        self._radar._pan_y += int(self._radar.height() / 2 - sy)
        self._radar.update()

    def _tree_item_for(self, key): #vers 3
        """Tree item of a node key, filling parents as needed."""
        t = self._nodes_tree
        want = {'ipl': ('grp', key[1]), 'ide': ('grp', key[1]), 'dtz': ('grp', key[1]),
                'trk': ('trkfile', key[1])}.get(
            key[0], ('sa', key[1], key[2]))
        stack = [t.topLevelItem(i) for i in range(t.topLevelItemCount())]
        while stack:
            it = stack.pop()
            if it.data(0, Qt.ItemDataRole.UserRole) == want:
                self._fill_tree_item(it)
                for c in range(it.childCount()):
                    if it.child(c).data(0, Qt.ItemDataRole.UserRole) == key:
                        return it.child(c)
                return None
            stack += [it.child(c) for c in range(it.childCount())]
        return None

    # - Selection and properties

    def _pn_visible(self, r): #vers 3
        if r.ped in ('trk', 'chase', 'rblk'):
            return self._show_extra
        if r.ped is None:
            return True
        return self._show_ped if r.ped else self._show_car

    def _layer_shown(self): #vers 2
        """Visible flag per layer (LAYERS order)."""
        e = self._show_extra
        return np.array([self._show_car, self._show_ped, True, e, e, e, e])

    def _pick_node(self, sx, sy, radius=8): #vers 5
        """Nearest visible editable node within radius pixels, or -1."""
        if not len(self._pn_xy):
            return -1
        px, py = self._radar.screen_xy(self._pn_xy)
        d = (px - sx) ** 2 + (py - sy) ** 2
        ok = self._layer_shown()[self._pn_cls] & self._pn_edit & (d <= radius * radius)
        if not ok.any():
            return -1
        d[~ok] = np.inf
        return int(np.argmin(d))

    def _select_node(self, idx, from_tree=False): #vers 1
        self._selected_node = idx
        self._show_node_props()
        if not from_tree and idx >= 0:
            it = self._tree_item_for(self._pn[idx].key)
            if it is not None:
                self._nodes_tree.blockSignals(True)
                self._nodes_tree.setCurrentItem(it)
                self._nodes_tree.scrollToItem(it)
                self._nodes_tree.blockSignals(False)
        self._radar.update()

    def _show_node_props(self): #vers 5
        """Rebuild the compact properties grid (3 fields per row)."""
        g = self._props_grid
        while g.count():                             # field may be emitting: defer delete
            w = g.takeAt(0).widget()
            if w is not None:
                w.blockSignals(True); w.hide(); w.deleteLater()
        if not 0 <= self._selected_node < len(self._pn):
            self._props_title.setText("No node selected")
            return
        r = self._pn[self._selected_node]
        src = {'ipl': lambda: r.group.source_ipl, 'ide': lambda: r.group.source_ide,
               'sa': lambda: f"nodes{r.group.area_id}.dat", 'trk': lambda: r.group,
               'dtz': lambda: r.group.source_ipl + " (view only)"}[r.kind]()
        self._props_title.setText(f"{src}  node {r.gi}")
        cells = []                                   # (label, widget) in reading order
        for attr, label, typ, lo, hi in _FIELDS[r.kind]:
            w = self._prop_widget(typ, lo, hi, getattr(r.obj, attr))
            w.editingFinished.connect(lambda a=attr, ww=w, t=typ: self._on_prop_edited(a, ww, t))
            cells.append((label, w))
        row = (len(cells) + 2) // 3
        for i, (label, w) in enumerate(cells):
            g.addWidget(QLabel(label), i // 3, (i % 3) * 2)
            g.addWidget(w, i // 3, (i % 3) * 2 + 1)
        for i, (attr, label, typ, lo, hi) in enumerate(_GROUP_FIELDS.get(r.kind, [])):
            w = self._prop_widget(typ, lo, hi, getattr(r.group, attr))
            w.editingFinished.connect(lambda a=attr, ww=w, t=typ: self._on_group_edited(a, ww, t))
            g.addWidget(QLabel(label), row, i * 2)
            g.addWidget(w, row, i * 2 + 1)
        row += 1
        g.addWidget(QLabel("World"), row, 0)
        g.addWidget(QLabel(f"{r.x:.2f}, {r.y:.2f}, {r.z:.2f}"), row, 1, 1, 5)
        if r.kind == 'sa':
            g.addWidget(QLabel("Links"), row + 1, 0)
            g.addWidget(QLabel(str(r.obj.link_count)), row + 1, 1)

    def _prop_widget(self, typ, lo, hi, val): #vers 1
        """Compact editor for one property value."""
        if typ is float:
            w = QDoubleSpinBox(); w.setDecimals(3); w.setRange(lo, hi); w.setValue(val)
        elif typ is int:
            w = QSpinBox(); w.setRange(lo, hi); w.setValue(int(val))
        elif typ == 'hex':
            w = QLineEdit(f"{val:08X}")
        else:
            w = QLineEdit(str(val))
        w.setMinimumWidth(82 if typ is float else 48)   # fits -9999.999 / 2-3 digits
        return w

    def _on_prop_edited(self, attr, w, typ): #vers 2
        """Apply one edited field to the selected node."""
        if not 0 <= self._selected_node < len(self._pn):
            return
        r = self._pn[self._selected_node]
        if typ == 'hex':
            try:
                val = int(w.text(), 16)
            except ValueError:
                return
            val = (val & ~0xF) | (r.obj.flags & 0xF)        # link count bits kept
        else:
            val = w.value()
        if getattr(r.obj, attr) == val:
            return
        self._push_undo([self._snapshot(r.obj)])
        setattr(r.obj, attr, val)
        self._mark_dirty(r)
        QTimer.singleShot(0, self._deferred_rebuild)   # not inside the field's own signal

    def _on_group_edited(self, attr, w, typ): #vers 2
        """Apply an edited group header field."""
        g = self._selected_group()
        if g is None:
            return
        val = w.value() if typ is int else w.text().strip()
        if getattr(g, attr) == val or val == "":
            return
        self._push_undo([(g, {attr: getattr(g, attr)})])
        setattr(g, attr, val)
        self._mark_dirty(self._pn[self._selected_node])
        self._set_status(f"Group {attr} = {val}")
        self._tree_stale = True
        QTimer.singleShot(0, self._deferred_rebuild)

    def _deferred_rebuild(self): #vers 1
        """Rebuild after a field edit, keeping whatever is selected now."""
        key = self._pn[self._selected_node].key if 0 <= self._selected_node < len(self._pn) else None
        if getattr(self, '_tree_stale', False):
            self._tree_stale = False
            self._populate_nodes_tree()
        self._rebuild_keep_selection(key)

    def _rebuild_keep_selection(self, key): #vers 1
        """Rebuild records and redraw, keeping the selected node."""
        self._build_path_records()
        self._selected_node = self._pn_key.get(key, -1)
        self._show_node_props()
        it = self._tree_item_for(key) if key else None
        if it is not None:
            it.setText(1, self._node_label(key[0], self._pn[self._selected_node].obj,
                                           key[-1])[1] if self._selected_node >= 0 else "")
        self._radar.update()

    # - Editing

    def _snapshot(self, obj): #vers 2
        kind = ('sa' if hasattr(obj, 'flood_fill') else 'ide' if hasattr(obj, 'x_rel')
                else 'trk' if hasattr(obj, 'source_file') else 'ipl')
        return (obj, {a: getattr(obj, a) for a, *_ in _FIELDS[kind]})

    def _push_undo(self, entry): #vers 1
        self._undo_stack.append(entry)
        del self._undo_stack[:-200]

    def _undo_path_edit(self): #vers 2
        """Undo the last path edit (fields, new group)."""
        if not self._undo_stack:
            self._set_status("Nothing to undo")
            return
        for item in reversed(self._undo_stack.pop()):
            if item[0] == 'group':
                item[1].remove(item[2])
            elif item[0] == 'ungroup':
                item[1].insert(item[2], item[3])
            else:
                obj, vals = item
                for a, v in vals.items():
                    setattr(obj, a, v)
        key = self._pn[self._selected_node].key if 0 <= self._selected_node < len(self._pn) else None
        self._populate_nodes_tree()
        self._rebuild_keep_selection(key)
        self._set_status("Undone")

    def _mark_dirty(self, r): #vers 2
        if r.kind == 'ipl':
            self._dirty.add(('ipl', r.group.source_ipl))
        elif r.kind == 'ide':
            self._dirty.add(('ide', r.group.source_ide))
        elif r.kind == 'trk':
            self._dirty.add(('trk', r.group))
        else:
            self._dirty.add(('sa', r.group.area_id))
            self._sa_changed.setdefault(r.group.area_id, {})[id(r.obj)] = r.obj

    def _ide_set_world(self, r, wx, wy): #vers 1
        """Set a III IDE node's relative offset from a world point."""
        from apps.methods.path_nodes import rotate_vector_by_quaternion
        i = r.inst
        dx, dy, dz = wx - i.pos_x, wy - i.pos_y, r.z - i.pos_z
        rx, ry, rz = rotate_vector_by_quaternion(dx, dy, dz, i.rot_x, i.rot_y, i.rot_z, i.rot_w)
        r.obj.x_rel, r.obj.y_rel, r.obj.z_rel = rx * 16.0, ry * 16.0, rz * 16.0

    def _begin_node_drag(self, idx): #vers 1
        self._select_node(idx)
        self._drag = (idx, self._snapshot(self._pn[idx].obj))

    def _drag_node_to(self, wx, wy): #vers 2
        if self._drag is None:
            return
        r = self._pn[self._drag[0]]
        r.x, r.y = wx, wy
        self._pn_xy[self._drag[0]] = (wx, wy)
        if r.kind == 'ide':
            self._ide_set_world(r, wx, wy)
        else:
            r.obj.x, r.obj.y = wx, wy
        self._radar.update()

    def _end_node_drag(self): #vers 1
        if self._drag is None:
            return
        idx, snap = self._drag
        self._drag = None
        r = self._pn[idx]
        if any(getattr(r.obj, a) != v for a, v in snap[1].items()):
            self._push_undo([snap])
            self._mark_dirty(r)
            self._rebuild_keep_selection(r.key)
            self._set_status(f"Moved node to {r.x:.1f}, {r.y:.1f}")

    def _add_node_at(self, wx, wy): #vers 2
        """Fill a free slot of the selected group, else start a new group."""
        from apps.methods.gta_dat_parser import PathGroup, PathNode
        if not 0 <= self._selected_node < len(self._pn):
            self._set_status("Select a node first - new node joins its group")
            return
        sel = self._pn[self._selected_node]
        if sel.kind in ('sa', 'trk'):
            self._set_status("SA nodes / tracks: move and edit only (adding not supported)")
            return
        g = sel.group
        slot = next((i for i, n in enumerate(g.nodes) if n.node_type == 0), None)
        undo = []
        if slot is None:
            if sel.kind == 'ide':
                self._set_status("IDE group full (12 nodes)")
                return
            nodes = [PathNode(node_type=0, next_id=-1, x=0.0, y=0.0, z=0.0) for _ in range(12)]
            ng = PathGroup(header_a=g.header_a, header_b=g.header_b, nodes=nodes,
                           source_ipl=g.source_ipl)
            lst = self._loader.paths
            lst.insert(lst.index(g) + 1, ng)
            undo.append(('group', lst, ng))
            g, slot, link_to = ng, 0, -1
        else:
            link_to = sel.gi
        n = g.nodes[slot]
        undo.append(self._snapshot(n))
        n.node_type, n.next_id = 2, link_to
        key = (sel.kind, id(g), slot)
        if sel.kind == 'ide':
            tmp = PathRec('ide', n, g, slot, wx, wy, sel.z, sel.ped, key, sel.inst)
            self._ide_set_world(tmp, wx, wy)
        else:
            n.x, n.y, n.z = wx, wy, sel.z
        self._push_undo(undo)
        self._mark_dirty(sel)
        self._populate_nodes_tree()
        self._build_path_records()
        self._select_node(self._pn_key.get(key, -1))
        self._set_status(f"Added node {slot} at {wx:.1f}, {wy:.1f}")

    def _link_click(self, idx): #vers 2
        """Link tool: first click picks, second sets next node."""
        if idx < 0:
            return
        if self._link_from < 0:
            self._link_from = idx
            self._select_node(idx)
            self._set_status("Link: click the node it leads to")
            return
        a, b = self._pn[self._link_from], self._pn[idx]
        self._link_from = -1
        if a.kind in ('sa', 'trk') or b.kind in ('sa', 'trk'):
            self._set_status("SA / track links: not editable")
        elif a.group is not b.group or a is b:
            self._set_status("Link within one group only")
        else:
            self._push_undo([self._snapshot(a.obj)])
            a.obj.next_id = b.gi
            self._mark_dirty(a)
            self._rebuild_keep_selection(a.key)
            self._set_status(f"Node {a.gi} -> {b.gi}")
        self._radar.update()

    def _delete_selected_node(self): #vers 2
        """Null the selected node and unlink nodes pointing at it."""
        if not 0 <= self._selected_node < len(self._pn):
            return
        r = self._pn[self._selected_node]
        if r.kind in ('sa', 'trk'):
            self._set_status("SA nodes / tracks: delete not supported")
            return
        undo = [self._snapshot(r.obj)]
        for n in r.group.nodes:
            if n is not r.obj and n.next_id == r.gi:
                undo.append(self._snapshot(n))
                n.next_id = -1
        self._push_undo(undo)
        r.obj.node_type, r.obj.next_id = 0, -1
        self._mark_dirty(r)
        self._selected_node = -1
        self._build_path_records()
        self._show_node_props()
        self._radar.update()
        self._set_status(f"Deleted node {r.gi}")

    def _selected_group(self): #vers 1
        """IPL/IDE group of the selected node, or None."""
        if not 0 <= self._selected_node < len(self._pn):
            return None
        r = self._pn[self._selected_node]
        return r.group if r.kind in ('ipl', 'ide') else None

    def _delete_group(self): #vers 1
        """Remove the selected node's whole path group."""
        g = self._selected_group()
        if g is None:
            self._set_status("Select an IPL / IDE path node first")
            return
        r = self._pn[self._selected_node]
        lst = self._loader.paths if r.kind == 'ipl' else self._loader.ide_paths
        i = lst.index(g)
        lst.remove(g)
        self._push_undo([('ungroup', lst, i, g)])
        self._mark_dirty(r)
        self._selected_node = -1
        self._populate_nodes_tree()
        self._build_path_records()
        self._show_node_props()
        self._radar.update()
        self._set_status("Deleted path group")

    def _reverse_group(self): #vers 1
        """Reverse traffic flow: every next-link points the other way."""
        g = self._selected_group()
        if g is None:
            self._set_status("Select an IPL / IDE path node first")
            return
        n = len(g.nodes)
        incoming = {}
        for nd in g.nodes:
            if nd.node_type and 0 <= nd.next_id < n and g.nodes[nd.next_id].node_type:
                incoming[nd.next_id] = incoming.get(nd.next_id, 0) + 1
        if any(c > 1 for c in incoming.values()):
            self._set_status("Group branches (node with 2+ incoming links) - cannot reverse")
            return
        new_next = [-1] * n
        for i, nd in enumerate(g.nodes):
            if nd.node_type and 0 <= nd.next_id < n and g.nodes[nd.next_id].node_type:
                new_next[nd.next_id] = i
        r = self._pn[self._selected_node]
        self._push_undo([self._snapshot(nd) for nd in g.nodes])
        for i, nd in enumerate(g.nodes):
            if nd.node_type:
                nd.next_id = new_next[i]
        self._mark_dirty(r)
        self._populate_nodes_tree()
        self._rebuild_keep_selection(r.key)
        self._set_status("Reversed group traffic flow")

    def _shift_rotate_tracks(self): #vers 1
        """Move and/or rotate every loaded track point."""
        import math
        from PyQt6.QtWidgets import QDialog, QDialogButtonBox
        tracks = self._loader.tracks if self._loader else {}
        if not tracks:
            self._set_status("No tracks loaded")
            return
        dlg = QDialog(self)
        dlg.setWindowTitle("Shift / Rotate Tracks")
        form = QFormLayout(dlg)
        spins = {}
        for key, label, lo, hi in (('dx', "Shift X", -9000, 9000), ('dy', "Shift Y", -9000, 9000),
                                   ('dz', "Shift Z", -2000, 2000), ('ang', "Rotate (deg)", -360, 360),
                                   ('px', "Pivot X", -9000, 9000), ('py', "Pivot Y", -9000, 9000)):
            sp = QDoubleSpinBox(); sp.setRange(lo, hi); sp.setDecimals(3)
            form.addRow(label, sp); spins[key] = sp
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if not dlg.exec():
            return
        v = {k: sp.value() for k, sp in spins.items()}
        rad = math.radians(v['ang'])
        ca, sa = math.cos(rad), math.sin(rad)
        undo = []
        for name, pts in tracks.items():
            for p in pts:
                undo.append(self._snapshot(p))
                x, y = p.x - v['px'], p.y - v['py']
                p.x = v['px'] + x * ca - y * sa + v['dx']
                p.y = v['py'] + x * sa + y * ca + v['dy']
                p.z += v['dz']
            self._dirty.add(('trk', name))
        self._push_undo(undo)
        key = self._pn[self._selected_node].key if 0 <= self._selected_node < len(self._pn) else None
        self._rebuild_keep_selection(key)
        self._set_status(f"Moved {len(undo)} track points")

    # - Save

    def _save_paths(self): #vers 2
        """Write changed IPL/IDE path sections, SA nodes and tracks files."""
        from apps.methods.path_nodes import (ide_path_section_lines, ipl_path_section_lines,
                                             write_sa_nodes, write_text_path_section,
                                             write_tracks_file)
        if not self._dirty:
            self._set_status("No path changes to save")
            return
        ld, done, errors = self._loader, [], []
        for kind, name in sorted(self._dirty, key=str):
            try:
                if kind == 'sa':
                    pf = ld.sa_nodes[name]
                    write_sa_nodes(pf, self._sa_changed.get(name, {}).values())
                    done.append(f"nodes{name}.dat")
                    continue
                path = self._file_abs.get(name.lower())
                if not path:
                    errors.append(f"{name}: file path unknown")
                    continue
                if kind == 'trk':
                    write_tracks_file(path, ld.tracks[name])
                    done.append(name)
                    continue
                if kind == 'ipl':
                    lines = ipl_path_section_lines([g for g in ld.paths if g.source_ipl == name])
                else:
                    lines = ide_path_section_lines([g for g in ld.ide_paths if g.source_ide == name])
                write_text_path_section(path, lines)
                done.append(name)
            except Exception as e:
                errors.append(f"{name}: {e}")
        self._dirty.clear(); self._sa_changed.clear()
        if errors:
            QMessageBox.warning(self, "Save Paths", "\n".join(errors))
        self._set_status(f"Saved paths: {', '.join(done) or 'none'}")
