#this belongs in apps/components/Terrain_Workshop/depends/terrain_assets_func.py - Version: 2
# X-Seti - October 28 2026 - IMG Factory 1.6 - Terrain Workshop assets browser

"""
Assets page: the loaded game's objects (IDE) by category with thumbnails
and search; click a tile, then click the ground to place it (random
rotation, align to surface, angle snap). Placed objects live in the world
as 'terrain_assets.ipl' (move / delete with the Map tools), are saved in
the terrain project and written to the exported IPL.
"""

##Methods list -
# asset_category
#
##class TerrainAssetsMixin: -
# _build_tw_assets_page
# _save_ipl_in_place
# _tw_asset_instance
# _tw_asset_picked
# _tw_assets_collect
# _tw_assets_fill
# _tw_assets_push
# _tw_assets_refresh
# _tw_place_asset
# _tw_texture_colour
# _tw_thumb_tick

import math
import random

from PyQt6.QtCore import QTimer
from PyQt6.QtGui import QIcon, QPixmap
from PyQt6.QtWidgets import QLabel, QLineEdit, QListWidget, QVBoxLayout, QWidget

TERRAIN_IPL = "terrain_assets.ipl"
SHOW_MAX = 300                                            # tiles per view (search narrows)
CATEGORIES = (
    ("Foliage", ('tree', 'palm', 'bush', 'veg', 'plant', 'grass', 'flower', 'hedge', 'shrub', 'cactus',
                 'pine', 'oak', 'fern', 'weed', 'foliage', 'leaf')),
    ("Rocks", ('rock', 'boulder', 'cliff', 'stone', 'pebble')),
    ("Lights", ('lamp', 'light', 'neon', 'lantern')),
    ("Fences & walls", ('fence', 'wall', 'barrier', 'railing', 'gate', 'bollard', 'hedge')),
    ("Signs", ('sign', 'billboard', 'poster')),
    ("Street furniture", ('bench', 'bin', 'hydrant', 'phone', 'busstop', 'bus_stop', 'meter', 'trash',
                          'dumpster', 'table', 'chair', 'mailbox', 'post')),
    ("Roads & bridges", ('road', 'bridge', 'tunnel', 'junc', 'kerb', 'curb', 'rail', 'track', 'ramp')),
    ("Water", ('water', 'pier', 'jetty', 'dock', 'buoy')),
    ("Buildings", ('bld', 'build', 'house', 'shop', 'apart', 'tower', 'office', 'hotel', 'church',
                   'warehouse', 'garage', 'barn', 'shack', 'hut', 'store', 'factory')),
)


def asset_category(name: str) -> str: #vers 1
    """Category for a model name (keywords), else Other."""
    n = name.lower()
    for title, words in CATEGORIES:
        if any(w in n for w in words):
            return title
    return "Other"


class TerrainAssetsMixin: #vers 1
    """Assets browser and placement for TerrainWorkshop."""

    def _build_tw_assets_page(self): #vers 1
        """Search, categories, thumbnail tiles, placement SETTINGS."""
        from apps.methods.env_panel import SectionHeader, Stepper, TileGrid, SegmentSlider
        from apps.methods.ui_sizes import size
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(2, 2, 2, 2)
        lay.setSpacing(2)
        self._tw_asset_search = QLineEdit()
        self._tw_asset_search.setPlaceholderText("Search model name or ID")
        self._tw_asset_search.setClearButtonEnabled(True)
        self._tw_asset_search.textChanged.connect(lambda _t: self._tw_assets_fill())
        lay.addWidget(self._tw_asset_search)
        self._tw_asset_cats = QListWidget()
        self._tw_asset_cats.setFixedHeight(size('input', self) * 5)
        self._tw_asset_cats.currentRowChanged.connect(lambda _r: self._tw_assets_fill())
        lay.addWidget(self._tw_asset_cats)
        self._tw_asset_note = QLabel()
        self._tw_asset_note.setWordWrap(True)
        self._tw_asset_note.setStyleSheet("font-style: italic;")
        lay.addWidget(self._tw_asset_note)
        self._tw_asset_tiles = TileGrid(size('picker_tile', self) + 24)
        self._tw_asset_tiles.picked.connect(self._tw_asset_picked)
        lay.addWidget(self._tw_asset_tiles)
        lay.addWidget(SectionHeader("Settings"))
        self._tw_asset_rand = Stepper("Random rotation", [(False, "OFF"), (True, "ON")], True)
        self._tw_asset_align = Stepper("Align to surface", [(False, "OFF"), (True, "ON")], False)
        self._tw_asset_snap = SegmentSlider("Angle snap", 0, 90, 0, 15, segments=6, fmt="{:g}°")
        self._tw_asset_snap.setToolTip("Rotation steps (0 = free)")
        for w in (self._tw_asset_rand, self._tw_asset_align, self._tw_asset_snap):
            lay.addWidget(w)
        lay.addStretch(1)
        self._tw_asset_model = None
        self._tw_thumb_queue, self._tw_thumb_cache, self._tw_tex_avg = [], {}, {}
        self._tw_thumb_timer = QTimer(self)
        self._tw_thumb_timer.timeout.connect(self._tw_thumb_tick)
        self._tw_assets_refresh()
        return panel

    def _tw_assets_refresh(self): #vers 1
        """Categories with counts from the loaded game's objects."""
        loader = getattr(self, '_world_loader', None)
        objs = getattr(loader, 'objects', None) or []
        objs = list(objs.values()) if isinstance(objs, dict) else list(objs)
        self._tw_asset_objs = [o for o in objs if getattr(o, 'model_name', '')]
        counts = {}
        for o in self._tw_asset_objs:
            c = asset_category(o.model_name)
            counts[c] = counts.get(c, 0) + 1
        cats = self._tw_asset_cats
        cats.blockSignals(True)
        cats.clear()
        cats.addItem(f"All  ({len(self._tw_asset_objs)})")
        cats.item(0).setData(256, "All")
        for title in [t for t, _w in CATEGORIES] + ["Other"]:
            if counts.get(title):
                cats.addItem(f"{title}  ({counts[title]})")
                cats.item(cats.count() - 1).setData(256, title)
        cats.setCurrentRow(0)
        cats.blockSignals(False)
        self._tw_assets_fill()

    def _tw_assets_fill(self): #vers 2
        """Tiles for the category / search (first SHOW_MAX); thumbnails render in the background."""
        tiles = self._tw_asset_tiles
        tiles.clear()
        self._tw_thumb_queue = []
        if not self._tw_asset_objs:
            self._tw_asset_note.setText("Load a game world (Map tools: Load) to browse its objects.")
            return
        item = self._tw_asset_cats.currentItem()
        cat = item.data(256) if item is not None else "All"
        q = self._tw_asset_search.text().strip().lower()
        hits = [o for o in self._tw_asset_objs
                if (cat == "All" or asset_category(o.model_name) == cat)
                and (not q or q in o.model_name.lower() or q == str(o.model_id))]
        hits.sort(key=lambda o: o.model_name.lower())
        self._tw_asset_note.setText(f"{len(hits)} objects" + (f" - first {SHOW_MAX} shown, search to narrow"
                                                              if len(hits) > SHOW_MAX else ""))
        blank = QIcon()
        for o in hits[:SHOW_MAX]:
            b = tiles.add_tile(o.model_id, self._tw_thumb_cache.get(o.model_name.lower(), blank),
                               f"{o.model_name}  (ID {o.model_id}, {o.txd_name}.txd)", 0.95)
            if o.model_name.lower() not in self._tw_thumb_cache:
                b.setText(o.model_name[:10])
                self._tw_thumb_queue.append((tiles, o))
        if self._tw_asset_model is not None:
            tiles.set_current(self._tw_asset_model)
        if self._tw_thumb_queue:
            self._tw_thumb_timer.start(0)

    def _tw_texture_colour(self, txd: str): #vers 1
        """name -> average (r, g, b) of that texture in txd (cached)."""
        def colour(name):
            key = (txd, name.lower())
            if key not in self._tw_tex_avg:
                from apps.methods.model_thumbnail import texture_average
                texs = self._model_cache.get_textures(txd) or {} if getattr(self, '_model_cache', None) else {}
                self._tw_tex_avg[key] = texture_average(texs.get(name.lower()))
            return self._tw_tex_avg[key]
        return colour

    def _tw_thumb_tick(self): #vers 2
        """Render a few queued (tile grid, object) thumbnails per tick (UI stays responsive)."""
        from apps.methods.model_thumbnail import render_model_thumbnail
        cache = getattr(self, '_model_cache', None)
        for _ in range(4):
            if not self._tw_thumb_queue:
                self._tw_thumb_timer.stop()
                return
            grid, o = self._tw_thumb_queue.pop(0)
            b = grid.tile(o.model_id)
            if b is None:
                continue
            icon = self._tw_thumb_cache.get(o.model_name.lower())
            if icon is not None:                               # made for another grid already
                b.setText("")
                b.setIcon(icon)
                continue
            model = cache.get_geometry(o.model_name) if cache is not None else None
            if model is None:
                b.setText(o.model_name[:10] + "\n(no model)")
                continue
            img = render_model_thumbnail(model, b.width() - 4, self._tw_texture_colour(o.txd_name))
            icon = QIcon(QPixmap.fromImage(img))
            self._tw_thumb_cache[o.model_name.lower()] = icon
            b.setText("")
            b.setIcon(icon)

    def _tw_asset_picked(self, model_id): #vers 1
        """Object picked: next ground click places it."""
        self._tw_asset_model = model_id
        self._tw_set_mode('place')
        o = self._world_loader.get_object(model_id)
        self._set_status(f"Click the ground to place {o.model_name}" if o else "")

    def _tw_place_asset(self, hit): #vers 2
        """Place the picked object at hit (rotation from the SETTINGS); undoable."""
        loader = getattr(self, '_world_loader', None)
        obj = loader.get_object(self._tw_asset_model) if loader and self._tw_asset_model is not None else None
        if obj is None:
            self._set_status("Pick an object on the Assets page first")
            return
        inst = self._tw_asset_instance(obj, hit, self._tw_asset_rand.value(), self._tw_asset_align.value(),
                                       self._tw_asset_snap.value())
        self._add_instances([inst], f"Placed {obj.model_name}")
        self._tw_dirty = True
        self._tw_status()

    def _tw_asset_instance(self, obj, hit, rand: bool, align: bool, snap: float): #vers 1
        """IPLInstance of obj at hit in the terrain IPL (yaw random / snapped, tilt to ground)."""
        from apps.components.Map_Editor.depends.map_widgets_func import euler_degrees_to_quat
        from apps.methods.gta_dat_parser import IPLInstance
        yaw = random.uniform(0, 360) if rand else 0.0
        if snap:
            yaw = round(yaw / snap) * snap
        rx = ry = 0.0
        if align:                                              # tilt with the ground
            t, d = self._tw_terrain, self._tw_terrain.cell
            x, y = float(hit[0]), float(hit[1])
            nx = t.height_at(x - d, y) - t.height_at(x + d, y)
            ny = t.height_at(x, y - d) - t.height_at(x, y + d)
            nz = 2 * d
            rx, ry = -math.degrees(math.atan2(ny, nz)), math.degrees(math.atan2(nx, nz))
        q = self._conjugate_rotation_for_game(*euler_degrees_to_quat(rx, ry, yaw))   # IPL stores the conjugate
        return IPLInstance(model_id=obj.model_id, model_name=obj.model_name, interior=0,
                           pos_x=float(hit[0]), pos_y=float(hit[1]), pos_z=float(hit[2]),
                           rot_x=q[0], rot_y=q[1], rot_z=q[2], rot_w=q[3],
                           lod_index=-1, source_ipl=TERRAIN_IPL, line_no=0)

    def _tw_assets_collect(self) -> list: #vers 1
        """Placed objects (as now in the world, after any Map-tool edits) for the project / export."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return list(getattr(self._tw_terrain, 'assets', []))
        return [{'id': i.model_id, 'name': i.model_name, 'pos': [i.pos_x, i.pos_y, i.pos_z],
                 'rot': [i.rot_x, i.rot_y, i.rot_z, i.rot_w]}
                for i in loader.instances if i.source_ipl == TERRAIN_IPL]

    def _tw_assets_push(self): #vers 1
        """Opened project: its placed objects into the loaded world (replacing earlier ones)."""
        from apps.methods.gta_dat_parser import IPLInstance
        loader = getattr(self, '_world_loader', None)
        assets = getattr(self._tw_terrain, 'assets', []) if self._tw_terrain is not None else []
        if loader is None:
            if assets:
                self._set_status(f"{len(assets)} placed objects kept - load the game world to see them")
            return
        loader.instances[:] = [i for i in loader.instances if i.source_ipl != TERRAIN_IPL]
        for a in assets:
            x, y, z = a['pos']
            qx, qy, qz, qw = a['rot']
            loader.instances.append(IPLInstance(model_id=a['id'], model_name=a['name'], interior=0,
                                                pos_x=x, pos_y=y, pos_z=z, rot_x=qx, rot_y=qy, rot_z=qz,
                                                rot_w=qw, lod_index=-1, source_ipl=TERRAIN_IPL, line_no=0))
        self._refresh_after_instance_change()

    def _save_ipl_in_place(self, ipl_name, confirm=True): #vers 1
        """Map save: placed terrain objects are saved with the terrain project, not a game IPL."""
        if ipl_name == TERRAIN_IPL:
            self._tw_dirty = True
            self._mark_ipl_saved(ipl_name)
            self._tw_status()
            return True
        return super()._save_ipl_in_place(ipl_name, confirm)
