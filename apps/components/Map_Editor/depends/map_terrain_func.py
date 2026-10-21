#this belongs in apps/components/Map_Editor/depends/map_terrain_func.py - Version: 1
# X-Seti - October 21 2026 - IMG Factory 1.6 - Map Workshop terrain tools

"""
Terrain sculpt tools for ModelWorkshop (Map): brush dock, live DFF / COL
vertex edits on ground models, undo, and save back into the IMG / COL.
"""

##class TerrainTool: -
# __init__
# _apply
# _commit
# _ground_like
# _mesh
# _push
# _targets
# _xform
# hover
# pick
# press
# release
# ring
# save
# tick
# undo
# vp
#
##class MapTerrainMixin: -
# _create_terrain_dock
# _on_terrain_mode
# _save_terrain
# _set_terrain_brush
# _terrain_param
# _terrain_status
# _terrain_undo
# _toggle_terrain

import numpy as np

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtWidgets import (
    QButtonGroup, QCheckBox, QDockWidget, QDoubleSpinBox, QGridLayout, QHBoxLayout,
    QLabel, QMessageBox, QPushButton, QSlider, QToolButton, QVBoxLayout, QWidget)

from apps.methods.terrain_brush import (
    MODES, brush_heights, gl_rotation, ramp_heights, ray_mesh_hit, vertex_normals)

_RING_COLOUR = {'raise': (0.4, 0.9, 0.4), 'lower': (0.95, 0.5, 0.3), 'smooth': (0.4, 0.8, 1.0),
                'ramp': (1.0, 0.85, 0.2)}
_LABELS = {'raise': "Raise", 'lower': "Lower", 'flatten': "Flatten", 'level': "Level to height",
           'smooth': "Smooth", 'roughen': "Roughen", 'ramp': "Ramp (drag A to B)"}


class TerrainTool: #vers 1
    """Brush state and vertex edits for Map Workshop's viewport."""

    def __init__(self, ws): #vers 1
        self.ws = ws
        self.mode, self.radius, self.intensity = 'raise', 20.0, 1.0
        self.falloff, self.slope, self.height = 0.5, 75.0, 0.0
        self.ground_only, self.edit_col = True, True
        self.meshes = {}             # model key -> arrays + list refs
        self.dirty = set()
        self.undo_stack = []
        self._hit = None             # (world point, entry)
        self._stroke = None          # model key -> (V copy, CV copy)
        self._press = None
        self._ramp = None

    @property
    def vp(self):
        return self.ws.preview_widget

    def _mesh(self, entry): #vers 1
        """Cached numpy arrays for an entry's model; None if not editable."""
        key = entry.get('model_key')
        verts, tris = entry.get('vertices'), entry.get('triangles')
        if not isinstance(key, str) or ':' in key or not verts or not tris:
            return None
        m = self.meshes.get(key)
        if m is not None and m['vlist'] is verts:
            return m if m['src'] else None
        cache = getattr(self.ws, '_model_cache', None)
        src = cache.geometry_source(key) if cache is not None else None
        V = np.asarray(verts, dtype=np.float64).reshape(-1, 3)
        cv = entry.get('col_vertices') or []
        m = {'key': key, 'vlist': verts, 'nlist': entry.get('normals'), 'src': src,
             'V': V, 'T': np.asarray([t[:3] for t in tris], dtype=np.int64),
             'cvlist': cv, 'CV': np.asarray(cv, dtype=np.float64).reshape(-1, 3)}
        m['ground'] = self._ground_like(V)
        self.meshes[key] = m
        return m if src else None

    @staticmethod
    def _ground_like(V): #vers 1
        """Wide and low models count as ground (not buildings / props)."""
        ext = V.max(axis=0) - V.min(axis=0)
        return max(ext[0], ext[1]) >= 20.0 and ext[2] <= 0.6 * max(ext[0], ext[1])

    def _xform(self, entry): #vers 1
        p = np.asarray(entry.get('pos', (0, 0, 0)), dtype=np.float64)
        M = gl_rotation(self.vp._quat_to_gl_matrix(*entry.get('rot', (0, 0, 0, 1))))
        s = np.asarray(entry.get('scale') or (1, 1, 1), dtype=np.float64)
        return p, M, np.where(np.abs(s) < 1e-6, 1.0, s)

    def pick(self, mx, my): #vers 1
        """(world point, entry) of the nearest editable surface under the mouse."""
        ray = self.vp._pick_ray(mx, my)
        if ray is None:
            return None
        o, d = np.asarray(ray[0]), np.asarray(ray[1])
        best = None
        for e in self.vp._world_instances or []:
            if not e.get('vertices'):
                continue
            p = np.asarray(e.get('pos', (0, 0, 0)), dtype=np.float64)
            r = self.vp._entry_radius(e)
            t0 = float((p - o) @ d)
            if t0 + r < 0 or np.sum((o + d * t0 - p) ** 2) > r * r:
                continue
            m = self._mesh(e)
            if m is None or (self.ground_only and not m['ground']):
                continue
            p, M, s = self._xform(e)
            lo, ld = ((o - p) @ M) / s, (d @ M) / s
            t = ray_mesh_hit(lo, ld, m['V'], m['T'])
            if t is None:
                continue
            w = ((lo + ld * t) * s) @ M.T + p
            dist = float(np.linalg.norm(w - o))
            if best is None or dist < best[0]:
                best = (dist, w, e)
        return (best[1], best[2]) if best else None

    def _targets(self, centre, reach): #vers 1
        """One editable entry per model within reach of centre (XY)."""
        out = {}
        for e in self.vp._world_instances or []:
            p = e.get('pos')
            if p is None or not e.get('vertices'):
                continue
            dxy = float(np.hypot(p[0] - centre[0], p[1] - centre[1]))
            if dxy > reach + self.vp._entry_radius(e):
                continue
            m = self._mesh(e)
            if m is None or (self.ground_only and not m['ground']):
                continue
            if m['key'] not in out or dxy < out[m['key']][0]:
                out[m['key']] = (dxy, e, m)
        return [(e, m) for _d, e, m in out.values()]

    def _apply(self, func, centre, reach): #vers 1
        """Run func(world points) -> new z on model and COL vertices."""
        targets = self._targets(centre, reach)
        changed = []
        for arr, lst in (('V', 'vlist'), ('CV', 'cvlist')):
            if arr == 'CV' and not self.edit_col:
                break
            parts, P = [], []
            for e, m in targets:
                if not len(m[arr]):
                    continue
                p, M, s = self._xform(e)
                W = (m[arr] * s) @ M.T + p
                idx = np.nonzero(np.hypot(W[:, 0] - centre[0], W[:, 1] - centre[1]) <= reach)[0]
                if len(idx):
                    parts.append((m, M, s, idx))
                    P.append(W[idx])
            if not parts:
                continue
            P = np.concatenate(P)
            dz = func(P) - P[:, 2]
            at = 0
            for m, M, s, idx in parts:
                step = dz[at:at + len(idx)]
                at += len(idx)
                if not np.any(np.abs(step) > 1e-6):
                    continue
                if m['key'] not in self._stroke:
                    self._stroke[m['key']] = (m['V'].copy(), m['CV'].copy())
                m[arr][idx] += step[:, None] * M[2, :] / s
                self._push(m, arr, lst, idx)
                changed.append(m['key'])
        if changed:
            self.dirty.update(changed)
            self.vp.refresh_world_models(changed)

    @staticmethod
    def _push(m, arr, lst, idx): #vers 1
        """Copy edited rows into the viewport's shared vertex list."""
        rows, out = m[arr], m[lst]
        for i in idx.tolist():
            out[i] = (rows[i, 0], rows[i, 1], rows[i, 2])

    def hover(self, mx, my): #vers 1
        self._hit = self.pick(mx, my)
        if self._ramp is not None and self._hit is not None:
            self._ramp[1] = self._hit[0]

    def press(self, mx, my, mods): #vers 1
        hit = self.pick(mx, my)
        if hit is None:
            return
        if self.mode == 'level' and mods & Qt.KeyboardModifier.ControlModifier:
            self.ws._terrain_param('height', float(hit[0][2]))      # Ctrl+click samples
            return
        self._hit, self._press, self._stroke = hit, hit[0], {}
        if self.mode == 'ramp':
            self._ramp = [hit[0], hit[0]]
        else:
            self.tick()

    def tick(self): #vers 1
        if self._stroke is None or self._hit is None or self.mode == 'ramp':
            return
        c = self._hit[0]
        strength = self.intensity * self.radius * 0.01
        target = self.height if self.mode == 'level' else float(self._press[2])
        self._apply(lambda P: brush_heights(self.mode, P, c, self.radius, self.falloff,
                                            strength, self.slope, target), c, self.radius)

    def release(self): #vers 1
        if self._stroke is None:
            return
        if self._ramp is not None:
            a, b = self._ramp
            mid = (a + b) / 2.0
            reach = float(np.hypot(*(b - a)[:2])) / 2.0 + self.radius
            self._apply(lambda P: ramp_heights(P, a, b, self.radius, self.falloff, self.slope),
                        mid, reach)
            self._ramp = None
        if self._stroke:
            for key in self._stroke:
                self._commit(self.meshes[key])
            self.undo_stack.append(self._stroke)
            del self.undo_stack[:-30]
        self._stroke = None
        self.ws._terrain_status()

    def _commit(self, m): #vers 1
        """Write arrays into the cached DFF / COL models and normals."""
        cache = self.ws._model_cache
        dff = cache.get_geometry(m['key'])
        at = 0
        for g in dff.geometries:
            n = len(g.vertices)
            Vg = m['V'][at:at + n]
            for v, row in zip(g.vertices, Vg.tolist()):
                v.x, v.y, v.z = row
            if g.normals and len(g.normals) == n:
                Tg = np.asarray([(t.v1, t.v2, t.v3) for t in g.triangles], dtype=np.int64).reshape(-1, 3)
                for nv, row in zip(g.normals, vertex_normals(Vg, Tg).tolist()):
                    nv.x, nv.y, nv.z = row
            at += n
        nl = m['nlist']
        if nl is not None and len(nl) == len(m['V']):
            nl[:] = [tuple(r) for r in vertex_normals(m['V'], m['T']).tolist()]
        col = cache.get_collision(m['key']) if len(m['CV']) else None
        if col is not None and len(col.vertices) == len(m['CV']):
            for v, row in zip(col.vertices, m['CV'].tolist()):
                v.x, v.y, v.z = row
            lo, hi, bd = m['CV'].min(axis=0), m['CV'].max(axis=0), col.bounds
            bd.min.x, bd.min.y, bd.min.z = np.minimum(lo, (bd.min.x, bd.min.y, bd.min.z)).tolist()
            bd.max.x, bd.max.y, bd.max.z = np.maximum(hi, (bd.max.x, bd.max.y, bd.max.z)).tolist()
            c = (np.asarray((bd.min.x, bd.min.y, bd.min.z)) + (bd.max.x, bd.max.y, bd.max.z)) / 2.0
            bd.center.x, bd.center.y, bd.center.z = c.tolist()
            bd.radius = float(max(bd.radius, np.linalg.norm(m['CV'] - c, axis=1).max()))

    def undo(self): #vers 1
        """Restore the last stroke; False when nothing to undo."""
        if not self.undo_stack:
            return False
        stroke = self.undo_stack.pop()
        for key, (V, CV) in stroke.items():
            m = self.meshes[key]
            m['V'][:], m['CV'][:] = V, CV
            self._push(m, 'V', 'vlist', np.arange(len(V)))
            self._push(m, 'CV', 'cvlist', np.arange(len(CV)))
            self._commit(m)
        self.vp.refresh_world_models(stroke)
        return True

    def ring(self): #vers 1
        if self._hit is None:
            return None
        line = [tuple(self._ramp[0]), tuple(self._ramp[1])] if self._ramp else None
        return (tuple(self._hit[0]), self.radius,
                _RING_COLOUR.get(self.mode, (1.0, 1.0, 1.0)), line)

    def save(self): #vers 1
        """Write edited DFFs (and COLs) back; returns (saved, problems)."""
        from apps.methods.dff_parser import DFFWriter
        from apps.methods.path_nodes import write_back
        from apps.methods.col_splice import tag_models
        cache = self.ws._model_cache
        saved, problems, cols = [], [], {}
        for key in sorted(self.dirty):
            m = self.meshes.get(key)
            try:
                dff = cache.get_geometry(key)
                raw = DFFWriter.write(dff)
                write_back(m['src'], lambda _old, r=raw: r)
                dff.raw = raw
                cache._opened_img_files.pop(m['src'].rpartition(':')[0], None)
                skipped = [f"{key}: {r}" for r in DFFWriter.last_report if r.startswith('not written')]
                problems += skipped
                if not skipped:
                    saved.append(key)
            except Exception as e:
                problems.append(f"{key}.dff: {e}")
            if self.edit_col and len(m['CV']):
                cs = cache.collision_source(key)
                if cs is not None:
                    cols[cs[1]] = cs[0]
        for src, col_file in cols.items():
            try:
                data = col_file.to_bytes()
                write_back(src, lambda _old, d=data: d)
                col_file.splice_info = tag_models(col_file.models, data)
                cache._opened_img_files.pop(src.rpartition(':')[0], None)
            except Exception as e:
                problems.append(f"{src}: {e}")
        self.dirty -= set(saved)
        return saved, problems


class MapTerrainMixin: #vers 1
    """Terrain dock and mode for ModelWorkshop (Map)."""

    def _create_terrain_dock(self): #vers 1
        """Brush grid, settings sliders, Undo / Save."""
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons
        self._terrain = TerrainTool(self)
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(6, 6, 6, 6)
        grid = QGridLayout()
        self._terrain_brushes = QButtonGroup(panel)
        for i, mode in enumerate(MODES):
            b = QToolButton()
            b.setIcon(getattr(OverlayIcons, f"terrain_{mode}_icon")(28))
            b.setIconSize(QSize(28, 28))
            b.setCheckable(True)
            b.setToolTip(_LABELS[mode] + ("\nCtrl+click: sample height" if mode == 'level' else ""))
            b.setChecked(mode == 'raise')
            b.clicked.connect(lambda _c, md=mode: self._set_terrain_brush(md))
            self._terrain_brushes.addButton(b)
            grid.addWidget(b, i // 4, i % 4)
        lay.addLayout(grid)
        lay.addWidget(QLabel("Settings"))
        self._terrain_sliders = {}
        for name, lo, hi, val, scale in (('radius', 2, 200, 20, 1), ('intensity', 1, 100, 10, 10),
                                         ('falloff', 0, 100, 50, 100), ('slope', 5, 89, 75, 1)):
            row = QHBoxLayout()
            row.addWidget(QLabel(name.capitalize()), 1)
            sl = QSlider(Qt.Orientation.Horizontal)
            sl.setRange(lo, hi)
            sl.setValue(val)
            lbl = QLabel(f"{val / scale:g}")
            lbl.setMinimumWidth(34)
            sl.valueChanged.connect(lambda v, n=name, sc=scale, lb=lbl: (
                lb.setText(f"{v / sc:g}"), setattr(self._terrain, n, v / sc), self._terrain_status()))
            row.addWidget(sl, 2)
            row.addWidget(lbl)
            lay.addLayout(row)
            self._terrain_sliders[name] = sl
        row = QHBoxLayout()
        row.addWidget(QLabel("Height"), 1)
        self._terrain_height = QDoubleSpinBox()
        self._terrain_height.setRange(-1000.0, 3000.0)
        self._terrain_height.setDecimals(2)
        self._terrain_height.setToolTip("Level to height target (Ctrl+click ground to sample)")
        self._terrain_height.valueChanged.connect(lambda v: setattr(self._terrain, 'height', v))
        row.addWidget(self._terrain_height, 2)
        lay.addLayout(row)
        ground = QCheckBox("Ground models only")
        ground.setToolTip("Skip buildings and props (tall or small models)")
        ground.setChecked(True)
        ground.toggled.connect(lambda on: setattr(self._terrain, 'ground_only', on))
        col = QCheckBox("Update collision (COL)")
        col.setChecked(True)
        col.toggled.connect(lambda on: setattr(self._terrain, 'edit_col', on))
        lay.addWidget(ground)
        lay.addWidget(col)
        row = QHBoxLayout()
        undo_btn = QPushButton("Undo")
        undo_btn.setToolTip("Undo last terrain stroke")
        undo_btn.clicked.connect(self._terrain_undo)
        save_btn = QPushButton("Save terrain")
        save_btn.setIcon(OverlayIcons.save_texture_icon(18))
        save_btn.setToolTip("Write edited DFF / COL entries back into their IMG")
        save_btn.clicked.connect(self._save_terrain)
        row.addWidget(undo_btn)
        row.addWidget(save_btn)
        lay.addLayout(row)
        self._terrain_info = QLabel("")
        self._terrain_info.setWordWrap(True)
        lay.addWidget(self._terrain_info)
        lay.addStretch(1)
        dock = QDockWidget("Terrain", self)
        dock.setObjectName("Terrain")
        dock.setWidget(panel)
        dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                         QDockWidget.DockWidgetFeature.DockWidgetFloatable |
                         QDockWidget.DockWidgetFeature.DockWidgetClosable)
        dock.visibilityChanged.connect(self._on_terrain_mode)
        self._terrain_dock = dock
        return dock

    def _on_terrain_mode(self, on): #vers 1
        """Dock shown: brush owns the left mouse; hidden: normal picking."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_brush_tool'):
            vp.set_brush_tool(self._terrain if on else None)
        act = getattr(self, '_terrain_act', None)
        if act is not None and act.isChecked() != bool(on):
            act.setChecked(bool(on))
        if on:
            self._terrain_status()

    def _set_terrain_brush(self, mode): #vers 1
        self._terrain.mode = mode
        self._terrain_status()

    def _terrain_param(self, name, value): #vers 1
        """Set a brush value from code (keeps the widgets in step)."""
        if name == 'height':
            self._terrain_height.setValue(value)
        self._terrain_status()

    def _terrain_status(self): #vers 1
        t = self._terrain
        n = len(t.dirty)
        self._terrain_info.setText(f"{_LABELS[t.mode]}  -  {n} model(s) edited, not saved"
                                   if n else _LABELS[t.mode])

    def _terrain_undo(self): #vers 1
        if self._terrain.undo():
            self._terrain_status()
        else:
            self._set_status("Terrain: nothing to undo")

    def _save_terrain(self): #vers 1
        """Save edited ground models; report problems."""
        if not self._terrain.dirty:
            self._set_status("Terrain: no edits to save")
            return
        saved, problems = self._terrain.save()
        msg = f"Terrain: saved {len(saved)} model(s)"
        self._set_status(msg)
        mw = getattr(self, 'main_window', None)
        if mw is not None and hasattr(mw, 'log_message'):
            mw.log_message(msg + (f", {len(problems)} problem(s)" if problems else ""))
        if problems:
            QMessageBox.warning(self, "Save terrain", "\n".join(problems[:30]))
        self._terrain_status()

    def _toggle_terrain(self, on): #vers 1
        """Ribbon Terrain tools: show and raise the dock, or hide it."""
        dock = getattr(self, '_terrain_dock', None)
        if dock is None:
            return
        dock.setVisible(bool(on))
        if on:
            dock.raise_()
