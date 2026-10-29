#this belongs in apps/components/Terrain_Workshop/depends/terrain_water_func.py - Version: 1
# X-Seti - October 28 2026 - IMG Factory 1.6 - Terrain Workshop water

"""
Water page: paint water at a level onto terrain cells, shown as a plane.
Write puts it in the loaded game's water file (backup first): SA water.dat
quads, III / VC waterpro.dat cells. Old water inside the terrain is replaced.
"""

##Methods list -
# water_rects
#
##class TerrainWaterMixin: -
# _build_tw_water_page
# _tw_refresh_water
# _tw_water_info_update
# _tw_water_target
# _tw_write_sa_water
# _tw_write_water
# _tw_write_waterpro

import math
import os

import numpy as np
from PyQt6.QtWidgets import QDoubleSpinBox, QHBoxLayout, QLabel, QPushButton, QVBoxLayout, QWidget

WATER_KEY = "terrain:water"
WATER_TEX = "tw_water"


def water_rects(water: np.ndarray) -> list: #vers 1
    """Wet cells merged into (i0, j0, i1, j1, z) rectangles of one level."""
    ny, nx = water.shape
    used = np.isnan(water)
    out = []
    for j, i in zip(*np.nonzero(~used)):
        if used[j, i]:
            continue
        z = water[j, i]
        i1 = i + 1
        while i1 < nx and not used[j, i1] and water[j, i1] == z:
            i1 += 1
        j1 = j + 1
        while j1 < ny and not used[j1, i:i1].any() and (water[j1, i:i1] == z).all():
            j1 += 1
        used[j:j1, i:i1] = True
        out.append((int(i), int(j), int(i1), int(j1), float(z)))
    return out


class TerrainWaterMixin: #vers 1
    """Water level brush and game water file writer for TerrainWorkshop."""

    def _build_tw_water_page(self): #vers 1
        """Add / Remove tiles, radius, water level, Write to game."""
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons
        from apps.methods.env_panel import SectionHeader, TileGrid
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as S
        from apps.methods.ui_sizes import rule_label, size
        page = QWidget()
        lay = QVBoxLayout(page)
        lay.setContentsMargins(2, 2, 2, 2)
        lay.setSpacing(2)
        self._tw_water_tiles = TileGrid(size('picker_tile', self) + 8)
        self._tw_water_tiles.add_tile('water', OverlayIcons.water_icon(48),
                                      "Add water at the water level\nCtrl+click: sample ground height")
        self._tw_water_tiles.add_tile('dry', S.dp_eraser_icon(48, self._get_icon_color()), "Remove water")
        self._tw_water_tiles.set_current('water')
        self._tw_water_tiles.picked.connect(self._tw_set_mode)
        lay.addWidget(self._tw_water_tiles)
        lay.addWidget(SectionHeader("Settings"))
        lay.addWidget(self._tw_slider('radius'))
        row = QHBoxLayout()
        self._tw_water_z = QDoubleSpinBox()
        self._tw_water_z.setRange(-100, 1000)
        self._tw_water_z.setDecimals(2)
        self._tw_water_z.setSuffix(" m")
        self._tw_water_z.setFixedWidth(size('number_width', self))
        tip = "World Z of painted water (Ctrl+click the ground to sample)"
        self._tw_water_z.setToolTip(tip)
        self._tw_water_z.valueChanged.connect(lambda v: setattr(self._tw_brush, 'water_level', v))
        row.addWidget(rule_label("Water level", tip, self))
        row.addWidget(self._tw_water_z)
        row.addStretch(1)
        lay.addLayout(row)
        lay.addWidget(SectionHeader("Game"))
        self._tw_water_btn = QPushButton("Write to game water file")
        self._tw_water_btn.setToolTip("Replace the game's water inside the terrain with the painted water (backup first)")
        self._tw_water_btn.clicked.connect(self._tw_write_water)
        lay.addWidget(self._tw_water_btn)
        self._tw_water_info = QLabel()
        self._tw_water_info.setWordWrap(True)
        lay.addWidget(self._tw_water_info)
        lay.addStretch(1)
        self._tw_water_info_update()
        return page

    def _tw_water_target(self): #vers 1
        """('sa' | 'waterpro', path) for the loaded world, or (None, reason)."""
        ld = getattr(self, '_world_loader', None)
        if ld is None:
            return None, "Load a game world to write its water file"
        game = getattr(ld, 'game', '')
        if game == 'sa':
            path = self._water_dat_path()
            return ('sa', path) if path else (None, "No water.dat in the loaded world")
        if game in ('gta3', 'vc'):
            kind, target = self._water_levels_target()
            return ('waterpro', target) if kind == 'file' else (None, "No waterpro.dat in the loaded world")
        return None, f"Water writing not supported for {game or 'this game'}"

    def _tw_water_info_update(self): #vers 1
        """Target file line under the Write button."""
        if not hasattr(self, '_tw_water_info'):
            return
        kind, target = self._tw_water_target()
        self._tw_water_btn.setEnabled(kind is not None)
        self._tw_water_info.setText(f"Writes {os.path.basename(target)}" if kind else target)

    def _tw_refresh_water(self): #vers 1
        """Painted water as one translucent plane entry in the viewport."""
        from apps.methods.dff_classes import RGBA, Material
        from apps.methods.terrain_materials import placeholder_texture
        t, vp = self._tw_terrain, self.preview_widget
        if t is None or not hasattr(self, '_tw_entries'):
            return
        rects = water_rects(t.water)
        old = self._tw_entries.get(WATER_KEY)
        if not rects:
            if old is not None:
                del self._tw_entries[WATER_KEY]
                vp.set_world_instances([e for e in vp._world_instances if e is not old],
                                       auto_fit=False, clear_display_lists=False)
            return
        c = t.cell
        verts, norms, uvs, tris = [], [], [], []
        for i0, j0, i1, j1, z in rects:
            k = len(verts)
            for i, j in ((i0, j0), (i1, j0), (i1, j1), (i0, j1)):
                verts.append((i * c, j * c, z))
                norms.append((0.0, 0.0, 1.0))
                uvs.append((i * c / 16.0, -j * c / 16.0))
            tris += [(k, k + 1, k + 2, 0), (k, k + 2, k + 3, 0)]
        mat = Material(texture_name=WATER_TEX)
        mat.color = RGBA(255, 255, 255, 150)
        mat.colour = mat.color
        entry = {'vertices': verts, 'normals': norms, 'uvs': uvs, 'triangles': tris, 'materials': [mat],
                 'prelit': [], 'geom_flags': 0x02 | 0x04 | 0x10 | 0x20 | 0x40,
                 'pos': (t.origin[0], t.origin[1], 0.0), 'rot': (0.0, 0.0, 0.0, 1.0),
                 'scale': (1.0, 1.0, 1.0), 'model_key': WATER_KEY, 'terrain': True}
        if old is not None:
            old.clear()
            old.update(entry)
            vp.refresh_world_models([WATER_KEY])
            vp.__dict__.get('_radius_cache', {}).pop(WATER_KEY, None)
            return
        vp._upload_textures([placeholder_texture(WATER_TEX, (40, 105, 175), 6)], additive=True)
        self._tw_entries[WATER_KEY] = entry
        vp.set_world_instances(list(vp._world_instances or []) + [entry],
                               auto_fit=False, clear_display_lists=False)

    def _tw_write_water(self): #vers 1
        """Write button: painted water into the game's water file."""
        t = self._tw_terrain
        if t is None:
            self._set_status("No terrain")
            return
        kind, target = self._tw_water_target()
        if kind is None:
            self._set_status(target)
            return
        try:
            msg = (self._tw_write_sa_water if kind == 'sa' else self._tw_write_waterpro)(target)
        except Exception as e:
            self._set_status(f"Water not written: {e}")
            return
        self._set_status(msg)

    def _tw_write_sa_water(self, path: str) -> str: #vers 1
        """SA water.dat: quads inside the terrain replaced by painted rectangles."""
        from apps.components.Water_Editor.water_workshop import SaWaterParser
        from apps.methods.file_backup import safe_write_bytes
        t = self._tw_terrain
        p = SaWaterParser()
        p.load(path)
        if p.is_iv():
            raise ValueError("GTA IV water.dat")
        x0, y0 = t.origin[0], t.origin[1]
        x1, y1 = x0 + t.nx * t.cell, y0 + t.ny * t.cell
        tmpl = p.quads[0] if p.quads else None                # wave / flow values copied from the file
        rest = list(tmpl['corners'][0]['f'][1:]) if tmpl else [0.0, 0.0, 0.0, 0.0]
        flag = tmpl.get('flag') if tmpl else 1
        def inside(q):
            return all(x0 - 0.01 <= c['x'] <= x1 + 0.01 and y0 - 0.01 <= c['y'] <= y1 + 0.01 for c in q['corners'])
        keep = [q for q in p.quads if not inside(q)]
        removed = len(p.quads) - len(keep)
        rects = water_rects(t.water)
        for i0, j0, i1, j1, z in rects:
            xa, xb, ya, yb = x0 + i0 * t.cell, x0 + i1 * t.cell, y0 + j0 * t.cell, y0 + j1 * t.cell
            keep.append({'corners': [{'x': x, 'y': y, 'f': [z] + rest}
                                     for x, y in ((xa, ya), (xb, ya), (xa, yb), (xb, yb))],
                         'flag': flag, 'extra': []})
        p.quads = keep
        safe_write_bytes(path, p.to_text().encode("latin1", errors="replace"),
                         f"Terrain Workshop: {os.path.basename(path)}")
        return f"{os.path.basename(path)}: {len(rects)} quads written, {removed} replaced (backup kept)"

    def _tw_write_waterpro(self, path: str) -> str: #vers 1
        """III / VC waterpro.dat: visible and physical cells inside the terrain set from painted water."""
        from apps.components.Water_Editor.water_workshop import WaterproParser
        from apps.methods.file_backup import safe_write_bytes
        from apps.methods.gta_dat_parser import RADAR_GRID_PRESETS, WATER_GRID_PRESETS
        t = self._tw_terrain
        wp = WaterproParser()
        wp.load(path)
        gw = wp.grid_w
        if gw % 6 == 0:
            raise ValueError("tiled (SOL) waterpro.dat")
        game = self._world_loader.game
        preset = WATER_GRID_PRESETS.get(game) or RADAR_GRID_PRESETS.get(game, RADAR_GRID_PRESETS['vc'])
        grid_size = preset['grid_size'] * self._water_scale()     # same layout as the Map view
        half = grid_size / 2.0
        x0, y0 = t.origin[0], t.origin[1]
        x1, y1 = x0 + t.nx * t.cell, y0 + t.ny * t.cell
        levels = wp.water_level_data
        added = []

        def level_index(z):
            n = wp.water_levels_count
            for k in range(n):
                if abs(levels[k] - z) < 0.05:
                    return k
            if n < wp.WATER_LEVELS:
                levels[n] = z
                wp.water_levels_count = n + 1
                added.append(z)
                return n
            return min(range(n), key=lambda k: abs(levels[k] - z))

        cells = 0
        for grid, g in ((wp.vis_grid, gw), (wp.phys_grid, gw * 2)):
            cell = grid_size / g
            r0 = max(0, math.ceil((x0 + half) / cell - 0.5))
            r1 = min(g - 1, math.ceil((x1 + half) / cell - 0.5) - 1)
            c0 = max(0, math.ceil((y0 + half) / cell - 0.5))
            c1 = min(g - 1, math.ceil((y1 + half) / cell - 0.5) - 1)
            for r in range(r0, r1 + 1):                          # row drives x, column y
                i = min(t.nx - 1, int((-half + (r + 0.5) * cell - x0) / t.cell))
                for c in range(c0, c1 + 1):
                    j = min(t.ny - 1, int((-half + (c + 0.5) * cell - y0) / t.cell))
                    z = t.water[j, i]
                    grid[r * g + c] = 128 if np.isnan(z) else level_index(float(z))
                    cells += 1
        safe_write_bytes(path, wp.to_bytes(), f"Terrain Workshop: {os.path.basename(path)}")
        return (f"{os.path.basename(path)}: {cells} cells set"
                + (f", {len(added)} level(s) added" if added else "") + " (backup kept)")
