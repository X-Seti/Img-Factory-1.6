#this belongs in apps/components/Terrain_Workshop/depends/terrain_grid_func.py - Version: 11
# X-Seti - October 22 2026 - IMG Factory 1.6 - Terrain Workshop grid, brush and view

"""
Terrain Workshop grid: new / open / save a terrain project, sculpt and paint
brushes on the height grid, and the tiles shown in the viewport.
"""

##class GridBrush: -
# __init__
# _apply
# _cells
# _touch
# cancel_path
# finish_path
# hover
# pick
# press
# release
# ring
# t
# tick
# undo
#
##class TerrainGridMixin: -
# _build_tw_sculpt_page
# _build_tw_setup_page
# _tw_confirm_discard
# _tw_new
# _tw_open
# _tw_refresh_tiles
# _tw_save
# _tw_set_mode
# _tw_set_opacity
# _tw_show
# _tw_slider
# _tw_status
# _tw_tool_button
# _tw_undo

import math
import os
import time

import numpy as np

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QApplication
from PyQt6.QtWidgets import (
    QDoubleSpinBox, QFileDialog, QGridLayout,
    QHBoxLayout, QLabel, QMessageBox, QPushButton, QSlider, QToolButton, QVBoxLayout, QWidget)

from apps.methods.terrain_brush import (brush_heights, path_heights, path_profile, path_project,
                                        ramp_heights, spline_points)
from apps.methods.terrain_mesh import (cell_centres, grid_normals, load_terrain, new_terrain,
                                       save_terrain, tile_mesh, tile_ranges)

MODES = ('raise', 'lower', 'flatten', 'level', 'smooth', 'roughen', 'ramp', 'paint')
_LABELS = {'raise': "Raise", 'lower': "Lower", 'flatten': "Flatten", 'level': "Level to height",
           'smooth': "Smooth", 'roughen': "Roughen", 'ramp': "Ramp (drag A to B)",
           'paint': "Paint the selected material", 'place': "Place the picked object",
           'water': "Add water at the water level", 'dry': "Remove water",
           'path': "Path: click points, double-click / Enter finishes, Esc cancels",
           'nature': "Nature: paint the land type and scatter its plants"}
_RING = {'raise': (0.4, 0.9, 0.4), 'lower': (0.95, 0.5, 0.3), 'smooth': (0.4, 0.8, 1.0),
         'ramp': (1.0, 0.85, 0.2), 'paint': (1.0, 1.0, 1.0), 'place': (0.3, 0.9, 1.0),
         'water': (0.3, 0.6, 1.0), 'dry': (0.85, 0.7, 0.45), 'path': (1.0, 0.75, 0.3),
         'nature': (0.5, 0.95, 0.4)}


class GridBrush: #vers 5
    """Viewport brush for the terrain grid (sculpt heights, paint cells)."""

    def __init__(self, ws): #vers 2
        self.ws = ws
        self.mode, self.radius, self.intensity = 'raise', 12.0, 1.0
        self.falloff, self.slope, self.height = 0.5, 75.0, 0.0
        self.paint_mat = 0
        self.water_level = 0.0                                 # world Z for 'water'
        self.path_width, self.path_blend = 8.0, 6.0            # path strip: full width, edge blend
        self.path_pts, self._path_click = [], 0.0
        self.undo_stack = []
        self._hit = self._press = self._stroke = self._ramp = None

    @property
    def t(self): #vers 1
        return self.ws._tw_terrain

    def pick(self, mx, my): #vers 1
        """Ground point under the mouse: march the ray, then bisect."""
        t = self.t
        ray = self.ws.preview_widget._pick_ray(mx, my)
        if t is None or ray is None:
            return None
        o, d = np.asarray(ray[0]), np.asarray(ray[1])
        w, h = t.size()
        step = t.cell * 0.5
        far = max(w, h) * 4 + abs(o[2]) * 4 + 100.0
        prev, s = None, 0.0
        while s < far:
            p = o + d * s
            inside = (t.origin[0] <= p[0] <= t.origin[0] + w and t.origin[1] <= p[1] <= t.origin[1] + h)
            if inside and p[2] <= t.height_at(p[0], p[1]):
                a, b = (prev if prev is not None else s - step), s
                for _ in range(12):
                    m = (a + b) / 2
                    q = o + d * m
                    if q[2] <= t.height_at(q[0], q[1]):
                        b = m
                    else:
                        a = m
                q = o + d * b
                return np.array([q[0], q[1], t.height_at(q[0], q[1])])
            prev, s = s, s + step
        return None

    def hover(self, mx, my): #vers 1
        self._hit = self.pick(mx, my)
        if self._ramp is not None and self._hit is not None:
            self._ramp[1] = self._hit

    def press(self, mx, my, mods): #vers 5
        hit = self.pick(mx, my)
        if hit is None:
            return
        if self.mode == 'path':                                # one point per click, double-click ends
            now = time.monotonic()
            if now - self._path_click < QApplication.doubleClickInterval() / 1000.0 and len(self.path_pts) >= 2:
                self.finish_path()
            else:
                self.path_pts.append(hit)
                self.ws._tw_path_keys()
            self._path_click = now
            return
        if self.mode == 'place':                               # Assets page: one object per click
            self.ws._tw_place_asset(hit)
            return
        if self.mode == 'level' and mods & Qt.KeyboardModifier.ControlModifier:
            self.ws._tw_height.setValue(float(hit[2]))          # Ctrl+click samples
            return
        if self.mode == 'water' and mods & Qt.KeyboardModifier.ControlModifier:
            self.ws._tw_water_z.setValue(float(hit[2]))         # Ctrl+click samples
            return
        self._hit, self._press = hit, hit
        self._stroke = (self.t.heights.copy(), self.t.mats.copy(), self.t.water.copy(), [])   # [] placed objects
        self._dirty = set()
        if self.mode == 'ramp':
            self._ramp = [hit, hit]
        else:
            self.tick()

    def _cells(self, centre, reach): #vers 1
        """Index window of grid points within reach of centre."""
        t = self.t
        i0 = max(0, int((centre[0] - reach - t.origin[0]) / t.cell))
        i1 = min(t.nx, int((centre[0] + reach - t.origin[0]) / t.cell) + 1)
        j0 = max(0, int((centre[1] - reach - t.origin[1]) / t.cell))
        j1 = min(t.ny, int((centre[1] + reach - t.origin[1]) / t.cell) + 1)
        return i0, i1, j0, j1

    def _apply(self, func, centre, reach): #vers 1
        """Run func on the grid points near centre; note touched tiles."""
        t = self.t
        i0, i1, j0, j1 = self._cells(centre, reach)
        if i1 < i0 or j1 < j0:
            return
        pts = t.grid_points()[j0:j1 + 1, i0:i1 + 1].reshape(-1, 3)
        z = func(pts) - t.origin[2]
        t.heights[j0:j1 + 1, i0:i1 + 1] = z.reshape(j1 - j0 + 1, i1 - i0 + 1)
        self._touch(i0, i1, j0, j1)

    def _touch(self, i0, i1, j0, j1): #vers 1
        tc = self.t.tile_cells
        for ty in range(max(0, (j0 - 1) // tc), (j1 + 1) // tc + 1):
            for tx in range(max(0, (i0 - 1) // tc), (i1 + 1) // tc + 1):
                self._dirty.add((tx, ty))

    def tick(self): #vers 3
        if self._stroke is None or self._hit is None or self.mode == 'ramp':
            return
        c = self._hit
        t = self.t
        if self.mode == 'nature':
            self.ws._tw_nature_tick(c, self._stroke[3])
        elif self.mode in ('paint', 'water', 'dry'):
            cc = cell_centres(t)
            mask = np.hypot(cc[..., 0] - c[0], cc[..., 1] - c[1]) <= self.radius
            if not mask.any():
                return
            if self.mode == 'paint':
                t.mats[mask] = self.paint_mat
                jj, ii = np.nonzero(mask)
                self._touch(ii.min(), ii.max() + 1, jj.min(), jj.max() + 1)
            else:
                t.water[mask] = self.water_level if self.mode == 'water' else np.nan
                self.ws._tw_refresh_water()
                return
        else:
            strength = self.intensity * self.radius * 0.01
            target = self.height if self.mode == 'level' else float(self._press[2])
            self._apply(lambda P: brush_heights(self.mode, P, c, self.radius, self.falloff,
                                                strength, self.slope, target), c, self.radius)
        self.ws._tw_refresh_tiles(self._dirty)
        self._dirty = set()

    def release(self): #vers 1
        if self._stroke is None:
            return
        if self._ramp is not None:
            a, b = self._ramp
            mid = (a + b) / 2.0
            reach = float(np.hypot(*(b - a)[:2])) / 2.0 + self.radius
            self._apply(lambda P: ramp_heights(P, a, b, self.radius, self.falloff, self.slope), mid, reach)
            self.ws._tw_refresh_tiles(self._dirty)
            self._ramp = None
        self.undo_stack.append(self._stroke)
        del self.undo_stack[:-40]
        self._stroke = None
        self.ws._tw_dirty = True
        self.ws._tw_status()

    def undo(self) -> bool: #vers 3
        if not self.undo_stack:
            return False
        self.t.heights, self.t.mats, self.t.water, placed = self.undo_stack.pop()
        if placed:
            self.ws._tw_remove_placed(placed)
        self.ws._tw_refresh_tiles(None)
        self.ws._tw_refresh_water()
        return True

    def ring(self): #vers 3
        if self._hit is None:
            return None
        if self.mode == 'place':
            return (tuple(self._hit), 1.0, _RING['place'], None)
        if self.mode == 'path':
            line = None
            if self.path_pts:
                curve = spline_points(self.path_pts + [self._hit], 2.0)
                z = [self.t.height_at(x, y) for x, y in curve]
                pts = [(float(x), float(y), zz) for (x, y), zz in zip(curve, z)]
                line = [p for a, b in zip(pts, pts[1:]) for p in (a, b)]
            return (tuple(self._hit), self.path_width / 2.0, _RING['path'], line)
        line = [tuple(self._ramp[0]), tuple(self._ramp[1])] if self._ramp else None
        return (tuple(self._hit), self.radius, _RING.get(self.mode, (1.0, 1.0, 1.0)), line)


    def finish_path(self): #vers 2
        """Bake the drawn path: paint its material, level across, smooth along."""
        pts, self.path_pts = self.path_pts, []
        self.ws._tw_path_keys()
        t = self.t
        if t is None or len(pts) < 2:
            return
        line = spline_points(pts, max(0.5, t.cell / 2.0))
        prof = path_profile(line, t.height_at, max(self.path_width * 2.0, 4.0 * t.cell), self.slope)
        half = self.path_width / 2.0
        lo, hi = line.min(0) - half - self.path_blend, line.max(0) + half + self.path_blend
        self.undo_stack.append((t.heights.copy(), t.mats.copy(), t.water.copy(), []))
        del self.undo_stack[:-40]
        self._dirty = set()
        i0, i1, j0, j1 = self._cells((lo + hi) / 2.0, float(max(hi - lo)) / 2.0)
        P = t.grid_points()[j0:j1 + 1, i0:i1 + 1].reshape(-1, 3)
        z = path_heights(P, line, prof, half, self.path_blend) - t.origin[2]
        t.heights[j0:j1 + 1, i0:i1 + 1] = z.reshape(j1 - j0 + 1, i1 - i0 + 1)
        cc = cell_centres(t)[j0:j1, i0:i1].reshape(-1, 2)
        if len(cc):
            d = path_project(cc, line)[0].reshape(j1 - j0, i1 - i0)
            t.mats[j0:j1, i0:i1][d <= half] = self.paint_mat
        self._touch(i0, i1, j0, j1)
        self.ws._tw_refresh_tiles(self._dirty)
        self._dirty = set()
        self.ws._tw_dirty = True
        self.ws._tw_status()

    def cancel_path(self): #vers 1
        """Drop the points of the path being drawn."""
        self.path_pts = []
        self.ws._tw_path_keys()


class TerrainGridMixin: #vers 1
    """Terrain project, sculpt / setup pages and tile display for TerrainWorkshop."""

    def _build_tw_setup_page(self): #vers 7
        """Setup page: project buttons, new terrain size (two columns), opacity, info."""
        from apps.methods.ui_sizes import rule_label, size
        self._tw_terrain, self._tw_path, self._tw_dirty = None, '', False
        self._tw_brush = GridBrush(self)
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(2, 2, 2, 2)
        lay.setSpacing(2)
        tb = self._tw_tool_button
        self._tw_guide_btn = tb('guide_layer_icon', "Show the guide picture on the ground\n"
                                "(off: show the painted materials)",
                                lambda: self._tw_toggle_guide(self._tw_guide_btn.isChecked()))
        self._tw_guide_btn.setCheckable(True)
        self._tw_guide, self._tw_guide_on = None, False
        self._tw_radar_btn = tb('view_icon', "Show the game radar map under the ground",
                                lambda: self._tw_radar_layer(self._tw_radar_btn.isChecked()))
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons
        self._tw_radar_btn.setIcon(OverlayIcons.radar_icon(size('title_icon', self)))
        self._tw_radar_btn.setCheckable(True)
        tools = QHBoxLayout()
        tools.setSpacing(2)
        for w in (tb('terrain_image_icon', "New terrain from an image or the game's radar map", self._tw_from_image),
                  self._tw_guide_btn, self._tw_radar_btn,
                  tb('settings_icon', "Align the radar: move, rotate, scale", self._show_radar_align_dialog),
                  tb('open_icon', "Open a terrain project (.terrain.json)", self._tw_open),
                  tb('save_icon', "Save the terrain project", lambda: self._tw_save(False)),
                  tb('saveas_icon', "Save the terrain project as...", lambda: self._tw_save(True))):
            tools.addWidget(w)
        tools.addStretch(1)
        lay.addLayout(tools)
        from apps.methods.env_panel import SectionHeader
        lay.addWidget(SectionHeader("New terrain"))
        grid = QGridLayout()
        grid.setHorizontalSpacing(4)
        grid.setVerticalSpacing(2)
        self._tw_new_fields = {}
        for n, (key, label, tip, lo, hi, val, step) in enumerate((
                ('width', "Width", "Terrain width in metres", 8, 6000, 256, 16),
                ('depth', "Depth", "Terrain depth in metres", 8, 6000, 256, 16),
                ('cell', "Cell", "Grid cell size in metres", 0.5, 32, 4, 0.5),
                ('tile', "Tile", "Tile size in metres (one model per tile)", 16, 512, 128, 16),
                ('ox', "X", "World origin X", -6000, 6000, 0, 10),
                ('oy', "Y", "World origin Y", -6000, 6000, 0, 10),
                ('oz', "Z", "Ground height (world Z)", -200, 1000, 10, 1))):
            sp = QDoubleSpinBox()
            sp.setRange(lo, hi)
            sp.setSingleStep(step)
            sp.setDecimals(1)
            sp.setValue(val)
            sp.setSuffix(" m")
            sp.setToolTip(tip)
            sp.setFixedWidth(size('number_width', self))
            r, c = n // 2, (n % 2) * 2
            grid.addWidget(rule_label(label, tip, self), r, c, Qt.AlignmentFlag.AlignVCenter)
            grid.addWidget(sp, r, c + 1, Qt.AlignmentFlag.AlignVCenter)
            self._tw_new_fields[key] = sp
        grid.setColumnStretch(4, 1)
        lay.addLayout(grid)
        create = QPushButton("Create new terrain")
        create.setFixedHeight(size('action_button', self))
        create.setToolTip("Flat ground with these settings, painted with the first material")
        create.clicked.connect(self._tw_new)
        lay.addWidget(create)
        orow = QHBoxLayout()
        orow.setSpacing(4)
        self._tw_opacity = 0.8                          # slightly see-through ground
        op = QSlider(Qt.Orientation.Horizontal)
        op.setRange(55, 100)
        op.setValue(80)
        op.setFixedHeight(size('input', self))
        op.setToolTip("Terrain opacity - see the game world through the ground")
        op_lbl = QLabel("80%")
        op_lbl.setFixedWidth(34)
        op.valueChanged.connect(lambda v: op_lbl.setText(f"{v}%"))
        op.sliderReleased.connect(lambda: self._tw_set_opacity(op.value() / 100.0))
        orow.addWidget(rule_label("Opacity", op.toolTip(), self))
        orow.addWidget(op, 1)
        orow.addWidget(op_lbl)
        lay.addLayout(orow)
        self._tw_info = QLabel("No terrain")
        self._tw_info.setWordWrap(True)
        lay.addWidget(self._tw_info)
        lay.addWidget(SectionHeader("Export"))
        lay.addWidget(self._build_tw_export_section())
        lay.addStretch(1)
        return panel

    def _tw_slider(self, name: str, label: str = ''): #vers 2
        """Segment slider for a brush setting; every copy (sculpt / paint / paths) stays in step."""
        from apps.methods.env_panel import SegmentSlider
        lo, hi, step, fmt = {'radius': (1, 200, 1, "{:g} m"), 'intensity': (0.1, 10, 0.1, "{:.1f}"),
                             'falloff': (0, 1, 0.05, "{:.2f}"), 'slope': (1, 89, 1, "{:g}\u00b0"),
                             'path_width': (1, 60, 0.5, "{:g} m"), 'path_blend': (0, 40, 1, "{:g} m")}[name]
        sl = SegmentSlider(label or name, lo, hi, getattr(self._tw_brush, name), step, fmt=fmt)
        sl.setToolTip({'radius': "Brush radius in metres", 'intensity': "Strength per stroke",
                       'falloff': "Soft edge: 0 hard, 1 soft", 'slope': "Ramp / path: steepest slope in degrees",
                       'path_width': "Path width in metres", 'path_blend': "Ground blend past the path edge"}[name])
        peers = self._tw_sliders.setdefault(name, [])
        peers.append(sl)

        def changed(v, me=sl, n=name):  #vers 1
            setattr(self._tw_brush, n, v)
            for other in self._tw_sliders[n]:
                if other is not me:
                    other.set_value(v, emit=False)
        sl.valueChanged.connect(changed)
        return sl

    def _build_tw_sculpt_page(self): #vers 4
        """Sculpt page: tool tiles, SETTINGS sliders, level height, undo."""
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons
        from apps.methods.env_panel import SectionHeader, TileGrid
        from apps.methods.ui_sizes import rule_label, size
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(2, 2, 2, 2)
        lay.setSpacing(2)
        self._tw_sliders = {}
        self._tw_sculpt_tiles = TileGrid(size('picker_tile', self) + 8)
        for mode in MODES[:-1]:                                     # 'paint' lives on its own page
            self._tw_sculpt_tiles.add_tile(mode, getattr(OverlayIcons, f"terrain_{mode}_icon")(48),
                                           _LABELS[mode] + ("\nCtrl+click: sample height" if mode == 'level' else ""))
        self._tw_sculpt_tiles.set_current('raise')
        self._tw_sculpt_tiles.picked.connect(self._tw_set_mode)
        lay.addWidget(self._tw_sculpt_tiles)
        lay.addWidget(SectionHeader("Settings"))
        for name in ('radius', 'intensity', 'falloff', 'slope'):
            lay.addWidget(self._tw_slider(name))
        row = QHBoxLayout()
        row.setSpacing(4)
        self._tw_height = QDoubleSpinBox()
        self._tw_height.setRange(-500, 3000)
        self._tw_height.setDecimals(2)
        self._tw_height.setSuffix(" m")
        self._tw_height.setFixedWidth(size('number_width', self))
        tip = "Level to height target (Ctrl+click the ground to sample)"
        self._tw_height.setToolTip(tip)
        self._tw_height.valueChanged.connect(lambda v: setattr(self._tw_brush, 'height', v))
        row.addWidget(rule_label("Level height", tip, self))
        row.addWidget(self._tw_height)
        row.addStretch(1)
        row.addWidget(self._tw_tool_button('undo_icon', "Undo the last stroke", self._tw_undo))
        lay.addLayout(row)
        lay.addStretch(1)
        return panel

    def _tw_tool_button(self, icon: str, tip: str, slot): #vers 3
        """Title bar sized themed icon button."""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        from apps.methods.ui_sizes import icon_size, size
        b = QToolButton()
        b.setIcon(getattr(SVGIconFactory, icon)(size('title_icon', self), self._get_icon_color()))
        b.setIconSize(icon_size('title_icon', self))
        b.setFixedSize(size('title_button', self), size('title_button', self))
        b.setAutoRaise(True)
        b.setToolTip(tip)
        b.clicked.connect(lambda _c=False: slot())
        return b

    def _tw_set_opacity(self, value: float): #vers 1
        """Ground opacity (0.55 - 1.0); redraws the tiles."""
        self._tw_opacity = value
        if self._tw_terrain is not None and hasattr(self, '_tw_entries'):
            self._tw_refresh_tiles(None)

    def _tw_set_mode(self, mode): #vers 4
        """Brush mode; sculpt modes are remembered for the Sculpt page."""
        if mode != 'path' and self._tw_brush.path_pts:
            self._tw_brush.cancel_path()                      # unfinished path dropped
        self._tw_brush.mode = mode
        if mode in MODES[:-1]:
            self._tw_last_sculpt = mode
        self._tw_status()

    def _tw_new(self): #vers 3
        """Create a flat terrain from the settings."""
        if not self._tw_confirm_discard():
            return
        f = {k: sp.value() for k, sp in self._tw_new_fields.items()}
        cells = (f['width'] / f['cell']) * (f['depth'] / f['cell'])
        if cells > 600000:
            QMessageBox.warning(self, "New terrain", f"{int(cells):,} cells is too many - use a bigger cell size.")
            return
        self._tw_terrain = new_terrain(f['width'], f['depth'], f['cell'], (f['ox'], f['oy'], f['oz']),
                                       f['tile'], "terrain")
        self._tw_guide = None                         # plain ground: no guide picture
        self._tw_guide_textures()
        self._tw_toggle_guide(False)
        self._tw_path, self._tw_dirty = '', True
        self._tw_brush.undo_stack.clear()
        self._tw_assets_push()                           # no placed objects on a new terrain
        self._tw_show(fit=True)

    def _tw_confirm_discard(self) -> bool: #vers 1
        if not self._tw_dirty or self._tw_terrain is None:
            return True
        r = QMessageBox.question(self, "Terrain Workshop", "Discard the unsaved terrain?")
        return r == QMessageBox.StandardButton.Yes

    def _tw_open(self): #vers 3
        if not self._tw_confirm_discard():
            return
        path, _ = QFileDialog.getOpenFileName(self, "Open terrain", self._tw_path or '',
                                              "Terrain projects (*.terrain.json);;All files (*)")
        if not path:
            return
        try:
            self._tw_terrain, lib = load_terrain(path)
        except Exception as e:
            QMessageBox.warning(self, "Open terrain", str(e))
            return
        if lib:
            self._tw_set_library(lib)
        self._tw_guide = None
        if self._tw_terrain.guide:                    # reload the guide picture
            try:
                self._tw_guide = self._tw_load_guide(self._tw_terrain.guide)
            except Exception as e:
                self._set_status(f"Guide picture not loaded: {e}")
        self._tw_guide_textures()
        self._tw_path, self._tw_dirty = path, False
        self._tw_brush.undo_stack.clear()
        self._tw_toggle_guide(self._tw_guide is not None)
        self._tw_assets_push()                           # its placed objects into the world
        self._tw_show(fit=True)

    def _tw_save(self, save_as: bool): #vers 2
        t = self._tw_terrain
        if t is None:
            return
        path = self._tw_path
        if save_as or not path:
            path, _ = QFileDialog.getSaveFileName(self, "Save terrain", path or 'terrain.terrain.json',
                                                  "Terrain projects (*.terrain.json)")
            if not path:
                return
            if not path.endswith('.terrain.json'):
                path = os.path.splitext(path)[0] + '.terrain.json'
        t.assets = self._tw_assets_collect()
        save_terrain(t, path, self._tw_library)
        self._tw_path, self._tw_dirty = path, False
        self._tw_status()
        self._set_status(f"Saved {os.path.basename(path)}")

    def _tw_undo(self): #vers 1
        if not self._tw_brush.undo():
            self._set_status("Terrain: nothing to undo")
        self._tw_status()

    def _tw_show(self, fit=False): #vers 3
        """Put every tile in the viewport, brush on, textured view."""
        vp = self.preview_widget
        if vp._mode == 'solid':                       # solid mode draws the ground white
            vp.set_render_mode('textured')
        keep = [e for e in (vp._world_instances or []) if not e.get('terrain')]
        self._tw_entries = {}
        vp._tw_keys = set()
        self._tw_upload_textures()
        self._tw_refresh_tiles(None, keep, fit)
        self._tw_refresh_water()
        vp.set_brush_tool(self._tw_brush)
        self._tw_status()

    def _tw_refresh_tiles(self, tiles=None, keep=None, fit=False): #vers 3
        """Rebuild tile entries (all, or the given (tx, ty) set) and redraw."""
        from apps.methods.dff_classes import RGBA, Material
        alpha = int(255 * getattr(self, '_tw_opacity', 1.0))
        t, vp = self._tw_terrain, self.preview_widget
        if t is None:
            return
        pts, nrm = t.grid_points(), grid_normals(t)
        lib = self._tw_library
        changed = []
        for tx, ty, i0, j0 in tile_ranges(t):
            if tiles is not None and (tx, ty) not in tiles:
                continue
            mesh = tile_mesh(t, i0, j0, lambda m: lib[min(m, len(lib) - 1)]['uv_size'], pts, nrm)
            if getattr(self, '_tw_guide_on', False):      # guide picture across the tile
                ci0, cj0, ci1, cj1 = mesh['cells']
                x0, y1 = (ci0 - i0) * t.cell, (cj1 - j0) * t.cell
                w, h = (ci1 - ci0) * t.cell, (cj1 - cj0) * t.cell
                uvs = [((x - x0) / w, (y1 - y) / h) for x, y, _z in mesh['vertices']]
                remap = {m: 0 for m in mesh['used']}
                mat = Material(texture_name=f"tguide_{tx}_{ty}")
                mat.color = RGBA(255, 255, 255, alpha)
                mat.colour = mat.color
                mats = [mat]
            else:
                uvs = mesh['uvs']
                remap = {m: k for k, m in enumerate(mesh['used'])}
                mats = []
                for m in mesh['used']:
                    mat = Material(texture_name=lib[min(m, len(lib) - 1)]['texture'])
                    mat.color = RGBA(255, 255, 255, alpha)    # opacity setting
                    mat.colour = mat.color                    # viewport reads .colour
                    mats.append(mat)
            key = f"terrain:{tx}_{ty}"
            entry = {'vertices': mesh['vertices'], 'normals': mesh['normals'], 'uvs': uvs,
                     'triangles': [(a, b, c, remap[m]) for a, b, c, m in mesh['triangles']],
                     'materials': mats,
                     'prelit': [], 'geom_flags': 0x02 | 0x04 | 0x10 | 0x20 | 0x40,
                     'pos': mesh['pos'], 'rot': (0.0, 0.0, 0.0, 1.0), 'scale': (1.0, 1.0, 1.0),
                     'model_key': key, 'terrain': True}
            old = self._tw_entries.get(key)
            if old is not None and keep is None:
                old.clear()
                old.update(entry)                     # same dict object stays in the viewport list
            else:
                self._tw_entries[key] = entry
            changed.append(key)
        if keep is not None:
            vp.set_world_instances(keep + list(self._tw_entries.values()), auto_fit=fit)
        else:
            vp.refresh_world_models(changed)
            radius_cache = vp.__dict__.get('_radius_cache', {})
            for k in changed:
                radius_cache.pop(k, None)

    def _tw_status(self): #vers 2
        t = self._tw_terrain
        if t is None:
            self._tw_info.setText("No terrain - set the size and Create, or Open a project")
            return
        w, h = t.size()
        tiles = len(tile_ranges(t))
        name = os.path.basename(self._tw_path) if self._tw_path else "unsaved"
        self._tw_info.setText(f"{name}{' *' if self._tw_dirty else ''}\n{w:g} x {h:g} m, "
                              f"{t.nx} x {t.ny} cells of {t.cell:g} m, {tiles} tiles\n"
                              f"Brush: {_LABELS[self._tw_brush.mode]}"
                              + ("\nShowing the guide picture" if getattr(self, '_tw_guide_on', False) else ""))
