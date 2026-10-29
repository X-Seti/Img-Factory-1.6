#this belongs in apps/components/Terrain_Workshop/depends/terrain_nature_func.py - Version: 1
# X-Seti - October 28 2026 - IMG Factory 1.6 - Terrain Workshop nature

"""
Nature page: land types (Nature, Arid & Barren, Wetland) paint a ground
material and scatter matching game plants / rocks; Plants scatters one
picked plant. Radius, intensity (density) and obstructions (skip paths,
water and spots near other objects).
"""

##Methods list -
# land_type_models
#
##class TerrainNatureMixin: -
# _build_tw_nature_page
# _tw_land_icon
# _tw_nature_fill
# _tw_nature_picked
# _tw_nature_tick
# _tw_remove_placed

import random

import numpy as np
from PyQt6.QtCore import QPointF, QRectF, Qt
from PyQt6.QtGui import QColor, QFont, QIcon, QImage, QPainter, QPixmap
from PyQt6.QtWidgets import QLabel, QVBoxLayout, QWidget

from apps.components.Terrain_Workshop.depends.terrain_assets_func import asset_category
from apps.components.Terrain_Workshop.depends.terrain_material_func import PATH_GROUPS

# (section, ((key, label, ground material, plant name keywords, spacing m), ...))
LAND_TYPES = (
    ("Nature", (
        ('grassland', "Grassland", "Grass", ('grass', 'weed', 'flower', 'fern'), 4.0),
        ('meadow', "Meadow", "Dry grass", ('flower', 'grass', 'bush', 'shrub'), 5.0),
        ('woodland', "Woodland", "Grass", ('tree', 'pine', 'oak', 'bush', 'fern'), 9.0))),
    ("Arid & Barren", (
        ('desert', "Desert", "Sand", ('cactus', 'dead', 'rock'), 14.0),
        ('scrub', "Scrub", "Dry grass", ('bush', 'shrub', 'cactus', 'dry'), 7.0),
        ('rocky', "Rocky", "Rock", ('rock', 'boulder', 'stone'), 10.0),
        ('gravel', "Gravel", "Gravel", ('rock', 'pebble', 'stone'), 12.0))),
    ("Wetland", (
        ('marsh', "Marsh", "Wet land", ('reed', 'grass', 'weed'), 4.0),
        ('swamp', "Swamp", "Wet land", ('tree', 'reed', 'bush'), 8.0),
        ('riverbank', "Riverbank", "Dirt", ('reed', 'bush', 'rock'), 6.0),
        ('mud', "Mud", "Wet land", ('weed', 'reed'), 10.0))),
)
PLANTS_MAX = 60                                          # plant tiles shown


def land_type_models(objects: list, words) -> list: #vers 1
    """Foliage / rock objects whose name has one of the keywords (no LODs)."""
    out = []
    for o in objects:
        n = o.model_name.lower()
        if n.startswith('lod') or asset_category(n) not in ("Foliage", "Rocks"):
            continue
        if any(w in n for w in words):
            out.append(o)
    return out


class TerrainNatureMixin: #vers 1
    """Nature brush (land types and single plants) for TerrainWorkshop."""

    def _build_tw_nature_page(self): #vers 1
        """Land type and Plants tiles, radius / intensity, obstructions."""
        from apps.methods.env_panel import SectionHeader, Stepper, TileGrid
        from apps.methods.ui_sizes import size
        page = QWidget()
        lay = QVBoxLayout(page)
        lay.setContentsMargins(2, 2, 2, 2)
        lay.setSpacing(2)
        self._tw_nature_tiles = TileGrid(size('picker_tile', self) + 8)
        self._tw_nature_tiles.picked.connect(self._tw_nature_picked)
        lay.addWidget(self._tw_nature_tiles)
        self._tw_nature_note = QLabel()
        self._tw_nature_note.setWordWrap(True)
        self._tw_nature_note.setStyleSheet("font-style: italic;")
        lay.addWidget(self._tw_nature_note)
        lay.addWidget(SectionHeader("Settings"))
        lay.addWidget(self._tw_slider('radius'))
        lay.addWidget(self._tw_slider('intensity', "Density"))
        self._tw_nature_obst = Stepper("Obstructions", [(False, "OFF"), (True, "ON")], True)
        self._tw_nature_obst.setToolTip("ON: no plants on paths, water or near other objects")
        lay.addWidget(self._tw_nature_obst)
        lay.addStretch(1)
        self._tw_nature_pick = None
        self._tw_nature_acc = 0.0
        return page

    def _tw_nature_fill(self): #vers 1
        """Land type tiles (ground texture icons) and the world's plants."""
        tiles = self._tw_nature_tiles
        tiles.clear()
        names = {m['name'].lower(): i for i, m in enumerate(self._tw_library)}
        for section, types in LAND_TYPES:
            tiles.add_section(section)
            for key, label, mat, words, _sp in types:
                i = names.get(mat.lower())
                icon = self._tw_land_icon(self._tw_library[i] if i is not None else None, label, words)
                tiles.add_tile(key, icon, f"{label}: {mat} ground, plants: {', '.join(words)}", 0.9)
        objs = getattr(self, '_tw_asset_objs', [])
        plants = sorted((o for o in objs if asset_category(o.model_name) == "Foliage"
                         and not o.model_name.lower().startswith('lod')), key=lambda o: o.model_name.lower())
        self._tw_nature_note.setText("" if objs else "Load a game world to scatter its plants "
                                                     "(land types still paint the ground).")
        if plants:
            tiles.add_section("Plants")
            blank = QIcon()
            for o in plants[:PLANTS_MAX]:
                b = tiles.add_tile(o.model_id, self._tw_thumb_cache.get(o.model_name.lower(), blank),
                                   f"{o.model_name}  (ID {o.model_id})", 0.95)
                if o.model_name.lower() not in self._tw_thumb_cache:
                    b.setText(o.model_name[:10])
                    self._tw_thumb_queue.append((tiles, o))
            if self._tw_thumb_queue:
                self._tw_thumb_timer.start(0)
        if self._tw_nature_pick is not None and tiles.tile(self._tw_nature_pick) is not None:
            tiles.set_current(self._tw_nature_pick)

    def _tw_land_icon(self, mat, label: str, words) -> QIcon: #vers 1
        """Ground texture with plant / rock marks and the land type's name."""
        from apps.methods.ui_sizes import size
        n = size('picker_tile', self)
        img = QImage(n, n, QImage.Format.Format_ARGB32)
        img.fill(QColor(90, 90, 90))
        p = QPainter(img)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        if mat is not None:
            tex = self._tw_texture(mat)
            src = QImage(tex['rgba_data'], tex['width'], tex['height'], tex['width'] * 4,
                         QImage.Format.Format_RGBA8888)
            p.drawImage(QRectF(0, 0, n, n), src)
        rnd = random.Random(label)
        kind = words[0]
        for _ in range(5):
            x, y = rnd.uniform(0.15, 0.85) * n, rnd.uniform(0.25, 0.7) * n
            s = n * rnd.uniform(0.09, 0.14)
            p.setPen(Qt.PenStyle.NoPen)
            if kind in ('tree', 'bush', 'flower'):           # trunk + canopy
                p.setBrush(QColor(90, 60, 30))
                p.drawRect(QRectF(x - s * 0.12, y, s * 0.24, s * 0.9))
                p.setBrush(QColor(*((200, 80, 140) if kind == 'flower' else (30, 110, 40))))
                p.drawEllipse(QPointF(x, y), s * 0.7, s * 0.6)
            elif kind in ('rock', 'cactus'):                  # rock / cactus blobs
                p.setBrush(QColor(*((60, 120, 60) if kind == 'cactus' else (120, 116, 110))))
                p.drawEllipse(QPointF(x, y), s * (0.35 if kind == 'cactus' else 0.7), s * 0.55)
            else:                                            # grass / reeds / weeds: blades
                p.setPen(QColor(*((120, 140, 60) if kind == 'reed' else (60, 140, 50))))
                for dx in (-0.3, 0.0, 0.3):
                    p.drawLine(QPointF(x + dx * s, y + s * 0.6), QPointF(x + dx * s * 1.6, y - s * 0.6))
        f = QFont(p.font())
        f.setPixelSize(max(8, n // 6))
        f.setBold(True)
        p.setFont(f)
        r = QRectF(0, n * 0.74, n, n * 0.26)
        p.setPen(QColor(0, 0, 0))
        p.drawText(r.translated(1, 1), Qt.AlignmentFlag.AlignCenter, label)
        p.setPen(QColor(255, 255, 255))
        p.drawText(r, Qt.AlignmentFlag.AlignCenter, label)
        p.end()
        return QIcon(QPixmap.fromImage(img))

    def _tw_nature_picked(self, key): #vers 1
        """Land type (str key) or plant (model ID) picked: nature brush."""
        self._tw_nature_pick = key
        self._tw_set_mode('nature')

    def _tw_nature_tick(self, centre, placed: list): #vers 1
        """One brush tick: paint the land type's ground, scatter its plants."""
        t, b = self._tw_terrain, self._tw_brush
        key = self._tw_nature_pick
        if key is None:
            return
        words, spacing, mat, single = (), 6.0, None, None
        for _s, types in LAND_TYPES:
            for k, _label, m, w, sp in types:
                if k == key:
                    words, spacing, mat = w, sp, m
        if not isinstance(key, str):
            single = self._world_loader.get_object(key) if getattr(self, '_world_loader', None) else None
            spacing = 6.0
        lib = [m['name'].lower() for m in self._tw_library]
        paths = {i for i, n in enumerate(lib) if any(w in n for _g, ws in PATH_GROUPS for w in ws)}
        from apps.methods.terrain_mesh import cell_centres
        cc = cell_centres(t)
        inside = np.hypot(cc[..., 0] - centre[0], cc[..., 1] - centre[1]) <= b.radius
        if mat is not None and mat.lower() in lib and inside.any():
            free = inside & ~np.isin(t.mats, list(paths)) if self._tw_nature_obst.value() else inside
            t.mats[free] = lib.index(mat.lower())                  # paths kept when obstructions are on
            jj, ii = np.nonzero(inside)
            b._touch(ii.min(), ii.max() + 1, jj.min(), jj.max() + 1)
        models = [single] if single is not None else land_type_models(getattr(self, '_tw_asset_objs', []), words)
        if not models:
            return
        self._tw_nature_acc += b.intensity * 0.25                  # attempts per tick (density)
        tries, self._tw_nature_acc = int(self._tw_nature_acc), self._tw_nature_acc % 1.0
        if not tries:
            return
        loader = self._world_loader
        obst = self._tw_nature_obst.value()
        if obst:
            near = [(i.pos_x, i.pos_y) for i in loader.instances
                    if abs(i.pos_x - centre[0]) < b.radius + spacing and abs(i.pos_y - centre[1]) < b.radius + spacing]
        new = []
        x0, y0 = t.origin[0], t.origin[1]
        for _ in range(tries):
            a, r = random.uniform(0, 6.2832), b.radius * random.random() ** 0.5
            x, y = centre[0] + r * np.cos(a), centre[1] + r * np.sin(a)
            i, j = int((x - x0) / t.cell), int((y - y0) / t.cell)
            if not (0 <= i < t.nx and 0 <= j < t.ny):
                continue
            if obst:
                if int(t.mats[j, i]) in paths or not np.isnan(t.water[j, i]):
                    continue
                if any((x - px) ** 2 + (y - py) ** 2 < spacing * spacing for px, py in near):
                    continue
                near.append((x, y))
            obj = random.choice(models)
            new.append(self._tw_asset_instance(obj, (x, y, t.height_at(x, y)), True, False, 0))
        if new:
            loader.instances.extend(new)
            placed.extend(new)
            self._refresh_after_instance_change()

    def _tw_remove_placed(self, placed: list): #vers 1
        """Undo of a nature stroke: its scattered objects out of the world."""
        ids = {id(i) for i in placed}
        loader = self._world_loader
        loader.instances[:] = [i for i in loader.instances if id(i) not in ids]
        self._refresh_after_instance_change()
