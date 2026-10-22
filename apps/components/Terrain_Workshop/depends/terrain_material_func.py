#this belongs in apps/components/Terrain_Workshop/depends/terrain_material_func.py - Version: 2
# X-Seti - October 22 2026 - IMG Factory 1.6 - Terrain Workshop materials

"""
Materials dock: the terrain material library (ground, grass, sand, rock,
stone, dirt, wet land, road, path...), texture from a PNG, a game TXD or a
generated placeholder, UV size in metres and the COL surface per game.
"""

##class TerrainMaterialMixin: -
# _create_tw_material_dock
# _tw_add_material
# _tw_game_texture
# _tw_mat_changed
# _tw_mat_selected
# _tw_material_icon
# _tw_pick_png
# _tw_pick_txd
# _tw_remove_material
# _tw_set_library
# _tw_texture
# _tw_texture_changed
# _tw_upload_textures

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QIcon, QImage, QPixmap
from PyQt6.QtWidgets import (
    QComboBox, QDockWidget, QDoubleSpinBox, QFileDialog, QGridLayout, QInputDialog, QLineEdit,
    QListWidget, QListWidgetItem, QMessageBox, QVBoxLayout, QWidget)

from apps.methods.terrain_materials import default_library, material_texture


class TerrainMaterialMixin: #vers 1
    """Material library dock for TerrainWorkshop."""

    def _create_tw_material_dock(self): #vers 3
        """Material tile grid (paint pick) and the selected material's settings."""
        from apps.methods.col_materials import COLGame, get_materials_for_version
        from apps.methods.ui_sizes import rule_label, size
        self._tw_library = default_library()
        self._tw_tex_cache = {}
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(6, 4, 6, 4)
        lay.setSpacing(2)
        tile = size('picker_tile', self)
        self._tw_mat_list = QListWidget()
        self._tw_mat_list.setViewMode(QListWidget.ViewMode.IconMode)
        self._tw_mat_list.setIconSize(QSize(tile, tile))
        self._tw_mat_list.setGridSize(QSize(tile + 6, tile + 6))
        self._tw_mat_list.setMovement(QListWidget.Movement.Static)
        self._tw_mat_list.setResizeMode(QListWidget.ResizeMode.Adjust)
        self._tw_mat_list.setWrapping(True)
        self._tw_mat_list.setSpacing(0)
        self._tw_mat_list.setStyleSheet("QListWidget::item:selected { border: 2px solid palette(highlight); }")
        self._tw_mat_list.setToolTip("Click a material to paint with it")
        self._tw_mat_list.currentRowChanged.connect(self._tw_mat_selected)
        lay.addWidget(self._tw_mat_list, 1)
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
            grid.addWidget(rule_label(label, tip, self), r, 0)
            grid.addWidget(w, r, 1, Qt.AlignmentFlag.AlignLeft if w is self._tw_mat_uv else Qt.AlignmentFlag(0))
        grid.setColumnStretch(1, 1)
        lay.addLayout(grid)
        dock = QDockWidget("Materials", self)
        dock.setObjectName("TerrainMaterials")
        dock.setWidget(panel)
        tb = self._tw_tool_button
        self._tw_mat_title = [tb('add_icon', "Add a material", self._tw_add_material),
                              tb('delete_icon', "Remove the selected material", self._tw_remove_material),
                              tb('folder_icon', "Texture from a PNG / image file", self._tw_pick_png),
                              tb('open_icon', "Texture from a game TXD (loaded IMGs)", self._tw_pick_txd)]
        self._tw_mat_dock = dock
        self._tw_set_library(self._tw_library)
        return dock

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

    def _tw_set_library(self, lib): #vers 2
        """Show a library as tiles; keeps the paint material in range."""
        self._tw_library = lib
        self._tw_mat_list.blockSignals(True)
        self._tw_mat_list.clear()
        for mat in lib:
            item = QListWidgetItem(self._tw_material_icon(mat), '')
            item.setToolTip(mat['name'])
            self._tw_mat_list.addItem(item)
        self._tw_mat_list.blockSignals(False)
        brush = getattr(self, '_tw_brush', None)
        row = min(brush.paint_mat if brush else 0, len(lib) - 1)
        self._tw_mat_list.setCurrentRow(row)

    def _tw_mat_selected(self, row): #vers 1
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

    def _tw_mat_changed(self, *_a): #vers 2
        """Store edited settings; UV size redraws the ground."""
        row = self._tw_mat_list.currentRow()
        if not 0 <= row < len(self._tw_library):
            return
        mat = self._tw_library[row]
        uv_old = mat.get('uv_size')
        mat['name'] = self._tw_mat_name.text().strip() or mat['name']
        mat['uv_size'] = self._tw_mat_uv.value()
        mat['surface_sa'] = self._tw_mat_sa.currentText()
        mat['surface_vc'] = self._tw_mat_vc.currentText()
        self._tw_mat_list.item(row).setToolTip(mat['name'])
        self._tw_dirty = True
        if uv_old != mat['uv_size'] and getattr(self, '_tw_terrain', None) is not None:
            self._tw_refresh_tiles(None)

    def _tw_add_material(self): #vers 1
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
        self._tw_set_library(self._tw_library)
        self._tw_mat_list.setCurrentRow(len(self._tw_library) - 1)
        self._tw_dirty = True

    def _tw_remove_material(self): #vers 1
        """Remove a material not used by the terrain."""
        row = self._tw_mat_list.currentRow()
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

    def _tw_pick_png(self): #vers 1
        row = self._tw_mat_list.currentRow()
        if row < 0:
            return
        path, _ = QFileDialog.getOpenFileName(self, "Texture image", '',
                                              "Images (*.png *.jpg *.jpeg *.bmp *.tga);;All files (*)")
        if not path:
            return
        mat = self._tw_library[row]
        mat.update(source='png', path=path)
        self._tw_texture_changed(row)

    def _tw_pick_txd(self): #vers 1
        """Pick 'txd_name / texture' from the loaded game's TXDs."""
        row = self._tw_mat_list.currentRow()
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

    def _tw_texture_changed(self, row): #vers 1
        """New texture for a material: icon, GL upload, redraw."""
        mat = self._tw_library[row]
        self._tw_tex_cache = {k: v for k, v in self._tw_tex_cache.items() if k[0] != mat['texture']}
        self._tw_mat_list.item(row).setIcon(self._tw_material_icon(mat))
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
