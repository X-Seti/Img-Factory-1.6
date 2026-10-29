#this belongs in apps/components/Terrain_Workshop/depends/terrain_material_func.py - Version: 5
# X-Seti - October 22 2026 - IMG Factory 1.6 - Terrain Workshop materials

"""
Painting / Paths pages: the terrain material library (ground, grass, sand, rock,
stone, dirt, wet land, road, path...), texture from a PNG, a game TXD or a
generated placeholder, UV size in metres and the COL surface per game.
"""

##class TerrainMaterialMixin: -
# _build_tw_paint_page
# _build_tw_paths_page
# _tw_add_material
# _tw_game_texture
# _tw_mat_changed
# _tw_mat_selected
# _tw_mat_row
# _tw_material_icon
# _tw_path_keys
# _tw_path_material
# _tw_pick_png
# _tw_pick_txd
# _tw_remove_material
# _tw_set_library
# _tw_texture
# _tw_texture_changed
# _tw_upload_textures

from PyQt6.QtCore import Qt
from PyQt6.QtGui import QIcon, QImage, QKeySequence, QPixmap, QShortcut
from PyQt6.QtWidgets import (
    QComboBox, QDoubleSpinBox, QFileDialog, QGridLayout, QHBoxLayout, QInputDialog, QLabel, QLineEdit,
    QMessageBox, QVBoxLayout, QWidget)

from apps.methods.terrain_materials import default_library, material_texture

PATH_GROUPS = (('Roads', ('road', 'tarmac', 'asphalt', 'street')),               # Paths page sections
               ('Rail', ('rail', 'tram')),
               ('Footpaths', ('path', 'pavement', 'sidewalk', 'cobble', 'brick', 'gravel', 'board', 'track')))


class TerrainMaterialMixin: #vers 1
    """Material library: Painting and Paths pages for TerrainWorkshop."""

    def _build_tw_paint_page(self): #vers 4
        """Painting page: material tiles, brush SETTINGS, the material's own settings."""
        from apps.methods.col_materials import COLGame, get_materials_for_version
        from apps.methods.env_panel import SectionHeader, TileGrid
        from apps.methods.ui_sizes import rule_label, size
        self._tw_library = default_library()
        self._tw_tex_cache = {}
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(2, 2, 2, 2)
        lay.setSpacing(2)
        self._tw_mat_tiles = TileGrid(size('picker_tile', self) + 8)
        self._tw_mat_tiles.picked.connect(self._tw_mat_selected)
        lay.addWidget(self._tw_mat_tiles)
        lay.addWidget(SectionHeader("Settings"))
        for name in ('radius', 'intensity'):
            lay.addWidget(self._tw_slider(name))
        lay.addWidget(SectionHeader("Material"))
        tb = self._tw_tool_button
        tools = QHBoxLayout()
        tools.setSpacing(2)
        for w in (tb('add_icon', "Add a material", self._tw_add_material),
                  tb('delete_icon', "Remove the selected material", self._tw_remove_material),
                  tb('folder_icon', "Texture from a PNG / image file", self._tw_pick_png),
                  tb('open_icon', "Texture from a game TXD (loaded IMGs)", self._tw_pick_txd)):
            tools.addWidget(w)
        tools.addStretch(1)
        lay.addLayout(tools)
        grid = QGridLayout()
        grid.setHorizontalSpacing(4)
        grid.setVerticalSpacing(2)
        self._tw_mat_name = QLineEdit()
        self._tw_mat_name.editingFinished.connect(self._tw_mat_changed)
        self._tw_mat_uv = QDoubleSpinBox()
        self._tw_mat_uv.setRange(0.5, 200.0)
        self._tw_mat_uv.setSuffix(" m")
        self._tw_mat_uv.setFixedWidth(size('number_width', self))
        self._tw_mat_uv.valueChanged.connect(self._tw_mat_changed)
        self._tw_mat_sa, self._tw_mat_vc = QComboBox(), QComboBox()
        for combo, game in ((self._tw_mat_sa, COLGame.SA), (self._tw_mat_vc, COLGame.VC)):
            names = []
            for m in get_materials_for_version(game):
                n = m[1]
                if n not in names:
                    names.append(n)
            combo.addItems(names)
            combo.currentIndexChanged.connect(self._tw_mat_changed)
        self._tw_mat_src = QLineEdit()
        self._tw_mat_src.setReadOnly(True)
        for r, (label, tip, w) in enumerate((
                ("Name", "Material name", self._tw_mat_name),
                ("UV size", "Size of one texture repeat on the ground", self._tw_mat_uv),
                ("SA", "San Andreas COL surface", self._tw_mat_sa),
                ("III / VC", "GTA III / Vice City COL surface", self._tw_mat_vc),
                ("Texture", "Texture source: PNG, game TXD or generated colour", self._tw_mat_src))):
            w.setToolTip(tip)
            w.setFixedHeight(size('input', self))
            grid.addWidget(rule_label(label, tip, self), r, 0, Qt.AlignmentFlag.AlignVCenter)
            grid.addWidget(w, r, 1, Qt.AlignmentFlag.AlignLeft if w is self._tw_mat_uv else Qt.AlignmentFlag(0))
        grid.setColumnStretch(1, 1)
        lay.addLayout(grid)
        lay.addStretch(1)
        return panel

    def _build_tw_paths_page(self): #vers 2
        """Paths page: road / rail / footpath tiles; drawn strips with width, blend, max slope."""
        from apps.methods.env_panel import SectionHeader, TileGrid
        from apps.methods.ui_sizes import size
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(2, 2, 2, 2)
        lay.setSpacing(2)
        self._tw_path_tiles = TileGrid(size('picker_tile', self) + 8)
        self._tw_path_tiles.picked.connect(self._tw_path_material)
        lay.addWidget(self._tw_path_tiles)
        lay.addWidget(SectionHeader("Settings"))
        lay.addWidget(self._tw_slider('path_width', "Width"))
        lay.addWidget(self._tw_slider('path_blend', "Edge blend"))
        lay.addWidget(self._tw_slider('slope', "Max slope"))
        note = QLabel("Click points along the path; double-click or Enter finishes, Esc cancels. "
                      "The ground is levelled across and smoothed along it.")
        note.setWordWrap(True)
        note.setStyleSheet("font-style: italic;")
        lay.addWidget(note)
        lay.addStretch(1)
        vp = self.preview_widget
        self._tw_path_enter = [QShortcut(QKeySequence(k), vp) for k in (Qt.Key.Key_Return, Qt.Key.Key_Enter)]
        for sc in self._tw_path_enter:
            sc.activated.connect(lambda: self._tw_brush.finish_path())
        self._tw_path_esc = QShortcut(QKeySequence(Qt.Key.Key_Escape), vp)
        self._tw_path_esc.activated.connect(lambda: self._tw_brush.cancel_path())
        self._tw_path_keys()
        return panel

    def _tw_path_keys(self): #vers 1
        """Enter / Esc belong to the path only while one is being drawn."""
        on = bool(self._tw_brush.path_pts)
        for sc in self._tw_path_enter + [self._tw_path_esc]:
            sc.setEnabled(on)
        self.preview_widget.update()

    def _tw_path_material(self, row): #vers 2
        """Paths tile picked: draw paths with that material."""
        self._tw_mat_tiles.set_current(row)
        self._tw_mat_selected(row)
        self._tw_set_mode('path')

    def _tw_mat_row(self) -> int: #vers 1
        """Selected material index, or -1."""
        row = self._tw_mat_tiles.current()
        return row if row is not None else -1

    def _tw_texture(self, mat) -> dict: #vers 1
        """Texture dict for a material (cached by texture name + source)."""
        key = (mat['texture'], mat.get('source'), mat.get('path'), mat.get('txd'), mat.get('txd_texture'))
        tex = self._tw_tex_cache.get(key)
        if tex is None:
            tex = material_texture(mat, self._tw_game_texture)
            self._tw_tex_cache[key] = tex
        return tex

    def _tw_game_texture(self, txd: str, name: str): #vers 1
        """A texture from the game's indexed TXDs (Map Workshop model cache)."""
        cache = getattr(self, '_model_cache', None)
        if cache is None:
            return None
        texs = cache.get_textures(txd) or {}
        return texs.get(name.lower())

    def _tw_material_icon(self, mat) -> QIcon: #vers 3
        """Tile icon from the material texture."""
        from apps.methods.ui_sizes import size
        tex = self._tw_texture(mat)
        img = QImage(tex['rgba_data'], tex['width'], tex['height'], tex['width'] * 4,
                     QImage.Format.Format_RGBA8888)
        t = size('picker_tile', self)
        return QIcon(QPixmap.fromImage(img.scaled(t, t)))

    def _tw_set_library(self, lib): #vers 5
        """Show a library as tiles (Painting, road / path ones on Paths, Nature ground icons)."""
        self._tw_library = lib
        self._tw_mat_tiles.clear()
        icons = [self._tw_material_icon(mat) for mat in lib]
        for i, mat in enumerate(lib):
            self._tw_mat_tiles.add_tile(i, icons[i], mat['name'], 0.9)
        paths = getattr(self, '_tw_path_tiles', None)
        if paths is not None:                                   # Roads / Rail / Footpaths
            paths.clear()
            placed = set()
            for title, words in PATH_GROUPS:
                rows = [i for i, m in enumerate(lib) if i not in placed
                        and any(w in m['name'].lower() for w in words)]
                if rows:
                    paths.add_section(title)
                for i in rows:
                    paths.add_tile(i, icons[i], lib[i]['name'], 0.9)
                    placed.add(i)
        brush = getattr(self, '_tw_brush', None)
        row = min(brush.paint_mat if brush else 0, len(lib) - 1)
        self._tw_mat_tiles.set_current(row)
        self._tw_mat_selected(row)
        if hasattr(self, '_tw_nature_tiles'):
            self._tw_nature_fill()

    def _tw_mat_selected(self, row): #vers 2
        """Pick the paint material; show its settings."""
        if not 0 <= row < len(self._tw_library):
            return
        mat = self._tw_library[row]
        brush = getattr(self, '_tw_brush', None)
        if brush is not None:
            brush.paint_mat = row
        for w in (self._tw_mat_name, self._tw_mat_uv, self._tw_mat_sa, self._tw_mat_vc):
            w.blockSignals(True)
        self._tw_mat_name.setText(mat['name'])
        self._tw_mat_uv.setValue(mat.get('uv_size', 8.0))
        self._tw_mat_sa.setCurrentText(mat.get('surface_sa', ''))
        self._tw_mat_vc.setCurrentText(mat.get('surface_vc', ''))
        for w in (self._tw_mat_name, self._tw_mat_uv, self._tw_mat_sa, self._tw_mat_vc):
            w.blockSignals(False)
        src = mat.get('source', 'placeholder')
        self._tw_mat_src.setText({'png': mat.get('path', ''), 'txd': f"{mat.get('txd')}.txd: {mat.get('txd_texture')}"}
                                 .get(src, "Generated colour"))

    def _tw_mat_changed(self, *_a): #vers 3
        """Store edited settings; UV size redraws the ground."""
        row = self._tw_mat_row()
        if not 0 <= row < len(self._tw_library):
            return
        mat = self._tw_library[row]
        uv_old = mat.get('uv_size')
        mat['name'] = self._tw_mat_name.text().strip() or mat['name']
        mat['uv_size'] = self._tw_mat_uv.value()
        mat['surface_sa'] = self._tw_mat_sa.currentText()
        mat['surface_vc'] = self._tw_mat_vc.currentText()
        self._tw_mat_tiles.tile(row).setToolTip(mat['name'])
        self._tw_dirty = True
        if uv_old != mat['uv_size'] and getattr(self, '_tw_terrain', None) is not None:
            self._tw_refresh_tiles(None)

    def _tw_add_material(self): #vers 2
        name, ok = QInputDialog.getText(self, "Add material", "Name:")
        if not ok or not name.strip():
            return
        if len(self._tw_library) >= 255:
            QMessageBox.warning(self, "Add material", "255 materials is the limit.")
            return
        tex = ''.join(c for c in name.lower() if c.isalnum() or c == '_')[:20] or 'material'
        self._tw_library.append({'name': name.strip(), 'texture': tex, 'source': 'placeholder',
                                 'colour': [128, 128, 128], 'noise': 20, 'uv_size': 8.0,
                                 'surface_sa': 'Default', 'surface_vc': 'Default'})
        self._tw_brush.paint_mat = len(self._tw_library) - 1
        self._tw_set_library(self._tw_library)
        self._tw_dirty = True

    def _tw_remove_material(self): #vers 2
        """Remove a material not used by the terrain."""
        row = self._tw_mat_row()
        t = getattr(self, '_tw_terrain', None)
        if row < 0 or len(self._tw_library) <= 1:
            return
        if t is not None and (t.mats == row).any():
            QMessageBox.information(self, "Remove material", "This material is painted on the terrain - paint over it first.")
            return
        del self._tw_library[row]
        if t is not None:
            t.mats[t.mats > row] -= 1
        self._tw_set_library(self._tw_library)
        self._tw_dirty = True
        if t is not None:
            self._tw_upload_textures()
            self._tw_refresh_tiles(None)

    def _tw_pick_png(self): #vers 2
        row = self._tw_mat_row()
        if row < 0:
            return
        path, _ = QFileDialog.getOpenFileName(self, "Texture image", '',
                                              "Images (*.png *.jpg *.jpeg *.bmp *.tga);;All files (*)")
        if not path:
            return
        mat = self._tw_library[row]
        mat.update(source='png', path=path)
        self._tw_texture_changed(row)

    def _tw_pick_txd(self): #vers 2
        """Pick 'txd_name / texture' from the loaded game's TXDs."""
        row = self._tw_mat_row()
        if row < 0:
            return
        cache = getattr(self, '_model_cache', None)
        if cache is None:
            QMessageBox.information(self, "Game texture", "Load a game (or IMG) in this workshop first.")
            return
        text, ok = QInputDialog.getText(self, "Game texture", "TXD name / texture name (e.g. gta3 / grass):")
        if not ok or '/' not in text:
            return
        txd, name = (p.strip() for p in text.split('/', 1))
        if self._tw_game_texture(txd, name) is None:
            QMessageBox.warning(self, "Game texture", f"{name} not found in {txd}.txd")
            return
        self._tw_library[row].update(source='txd', txd=txd, txd_texture=name)
        self._tw_texture_changed(row)

    def _tw_texture_changed(self, row): #vers 2
        """New texture for a material: icon, GL upload, redraw."""
        mat = self._tw_library[row]
        self._tw_tex_cache = {k: v for k, v in self._tw_tex_cache.items() if k[0] != mat['texture']}
        icon = self._tw_material_icon(mat)
        self._tw_mat_tiles.tile(row).setIcon(icon)
        path_tile = self._tw_path_tiles.tile(row)
        if path_tile is not None:
            path_tile.setIcon(icon)
        self._tw_mat_selected(row)
        self._tw_dirty = True
        vp = self.preview_widget
        name = mat['texture'].lower()
        if name in getattr(vp, '_tex_ids', {}):
            vp.makeCurrent()
            from OpenGL.GL import glDeleteTextures
            glDeleteTextures([vp._tex_ids.pop(name)])
            vp.doneCurrent()
        self._tw_upload_textures()
        if getattr(self, '_tw_terrain', None) is not None:
            self._tw_refresh_tiles(None)

    def _tw_upload_textures(self): #vers 1
        """All library textures into the viewport."""
        self.preview_widget._upload_textures([self._tw_texture(m) for m in self._tw_library], additive=True)
