#this belongs in apps/components/Terrain_Workshop/depends/terrain_image_func.py - Version: 5
# X-Seti - October 22 2026 - IMG Factory 1.6 - Terrain Workshop image / radar base

"""
Flat base terrain from a picture (image file or the game's radar map at
true world position). The picture is a guide layer on the ground and its
colours can pre-paint materials; sculpt it with the Brush tools after.
"""

##class TerrainImageMixin: -
# _tw_build_from_picture
# _tw_from_image
# _tw_guide_textures
# _tw_load_guide
# _tw_pick_guide_file
# _tw_game_changed
# _tw_radar_available
# _tw_radar_layer
# _tw_toggle_guide

import os

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (
    QCheckBox, QComboBox, QDialog, QDialogButtonBox, QDoubleSpinBox, QFileDialog, QGridLayout,
    QHBoxLayout, QLabel, QLineEdit, QMessageBox, QVBoxLayout)

from apps.methods.terrain_image import (classify_cells, guide_tile_texture, load_image_rgba,
                                        radar_extent, radar_tiles_from_img, stitch_radar)
from apps.methods.terrain_mesh import new_terrain, tile_ranges

_RADAR_GAMES = (("San Andreas", 'sa'), ("Vice City", 'vc'), ("GTA III", 'gta3'), ("State of Liberty", 'sol'))


class TerrainImageMixin: #vers 1
    """Terrain from image / radar, and the guide picture layer."""

    def _tw_from_image(self): #vers 1
        """Dialog: picture source, size, cells; then build the flat terrain."""
        from apps.methods.ui_sizes import rule_label, size
        if not self._tw_confirm_discard():
            return
        dlg = QDialog(self)
        dlg.setWindowTitle("Terrain from image / radar")
        lay = QVBoxLayout(dlg)
        grid = QGridLayout()
        grid.setHorizontalSpacing(4)
        grid.setVerticalSpacing(2)
        src = QComboBox()
        src.addItems(["Image file", "Game radar map"])
        path = QLineEdit()
        path.setPlaceholderText("Image file (png, jpg, bmp, tga)")
        img_path = QLineEdit()
        img_path.setPlaceholderText("gta3.img with radar tiles (blank: loaded game)")
        game = QComboBox()
        for label, key in _RADAR_GAMES:
            game.addItem(label, key)
        tb = self._tw_tool_button
        prow, irow = QHBoxLayout(), QHBoxLayout()
        prow.addWidget(path, 1)
        prow.addWidget(tb('folder_icon', "Choose the image",
                          lambda: self._tw_pick_guide_file(path, "Images (*.png *.jpg *.jpeg *.bmp *.tga)")))
        irow.addWidget(img_path, 1)
        irow.addWidget(tb('folder_icon', "Choose the IMG archive",
                          lambda: self._tw_pick_guide_file(img_path, "IMG archives (*.img)")))

        def spin(lo, hi, val, step, tip, suffix=" m"): #vers 1
            sp = QDoubleSpinBox()
            sp.setRange(lo, hi)
            sp.setDecimals(1)
            sp.setSingleStep(step)
            sp.setValue(val)
            sp.setSuffix(suffix)
            sp.setToolTip(tip)
            sp.setFixedWidth(size('number_width', self))
            return sp
        width = spin(16, 12000, 512, 16, "World width the picture covers")
        depth = spin(16, 12000, 512, 16, "World depth the picture covers")
        ox = spin(-12000, 12000, 0, 10, "West edge (world X)")
        oy = spin(-12000, 12000, 0, 10, "South edge (world Y)")
        cell = spin(0.5, 64, 4, 0.5, "Grid cell size")
        tile = spin(16, 1000, 128, 16, "Tile size (one model per tile)")
        oz = spin(-200, 1000, 10, 1, "Ground height (world Z)")
        paint = QCheckBox("Pre-paint materials from the picture colours")
        paint.setChecked(True)
        info = QLabel()
        rows = (("Source", "Image file or the game's radar tiles", src),
                ("Image", "Picture file", prow), ("Game", "Radar grid of this game", game),
                ("IMG", "Archive holding radarNN.txd", irow))
        for r, (label, tip, w) in enumerate(rows):
            grid.addWidget(rule_label(label, tip, self), r, 0)
            (grid.addLayout if isinstance(w, QHBoxLayout) else grid.addWidget)(w, r, 1, 1, 3)
        for r, pair in enumerate(((("Width", width), ("Depth", depth)), (("X", ox), ("Y", oy)),
                                  (("Cell", cell), ("Tile", tile)), (("Z", oz),)), start=len(rows)):
            for c, (label, w) in enumerate(pair):
                grid.addWidget(rule_label(label, w.toolTip(), self), r, c * 2)
                grid.addWidget(w, r, c * 2 + 1, Qt.AlignmentFlag.AlignLeft)
        grid.setColumnStretch(3, 1)                       # pairs sit close together
        lay.addLayout(grid)
        lay.addWidget(paint)
        lay.addWidget(info)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept)
        bb.rejected.connect(dlg.reject)
        lay.addWidget(bb)
        radar_w = (game, img_path)
        image_w = (path, width, depth, ox, oy)

        def refresh(*_a): #vers 1
            radar = src.currentIndex() == 1
            for w in radar_w:
                w.setEnabled(radar)
            for w in image_w:
                w.setEnabled(not radar)
            if radar:
                x0, y0, s, _per = radar_extent(game.currentData())
                for sp, v in ((width, s), (depth, s), (ox, x0), (oy, y0)):
                    sp.blockSignals(True)
                    sp.setValue(v)
                    sp.blockSignals(False)
                if cell.value() < s / 400:                 # keep the cell count sensible
                    cell.setValue(s / 300)
                    tile.setValue(s / (2 * _per))
            n = (width.value() / cell.value()) * (depth.value() / cell.value())
            info.setText(f"{int(n):,} cells" + ("  - too many, use a bigger cell" if n > 600000 else ""))
        for w in (src, game):
            w.currentIndexChanged.connect(refresh)
        for w in (width, depth, cell):
            w.valueChanged.connect(refresh)
        refresh()
        from apps.methods.ui_sizes import fit_inputs
        fit_inputs(dlg)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        if src.currentIndex() == 1:
            guide = {'radar': game.currentData(), 'img': img_path.text().strip()}
        else:
            guide = {'image': path.text().strip()}
        self._tw_build_from_picture(guide, width.value(), depth.value(), cell.value(), tile.value(),
                                    (ox.value(), oy.value(), oz.value()), paint.isChecked())

    def _tw_pick_guide_file(self, edit, filt): #vers 1
        p, _ = QFileDialog.getOpenFileName(self, "Choose file", edit.text() or '', filt + ";;All files (*)")
        if p:
            edit.setText(p)

    def _tw_load_guide(self, guide: dict): #vers 2
        """Picture (h, w, 4) for a guide source; raises if it can't be read."""
        if 'image' in guide:
            return load_image_rgba(guide['image'])
        g = guide['radar']
        _x, _y, _s, per = radar_extent(g)
        if guide.get('img'):
            tiles = radar_tiles_from_img(guide['img'], g)
        elif getattr(self, '_model_cache', None) is not None:
            tiles = self._load_radar_tex_tiles(g)
        else:
            raise IOError("No IMG chosen and no game loaded in this workshop.")
        if not any(tiles):
            raise IOError("No radar tiles found (radar00.txd ...).")
        self._tw_radar_tiles, self._tw_radar_game = tiles, g     # also shown as the radar layer
        return stitch_radar(tiles, per)

    def _tw_build_from_picture(self, guide, width, depth, cell, tile, origin, paint): #vers 2
        """Flat terrain covering the picture; guide layer on; optional pre-paint."""
        if (width / cell) * (depth / cell) > 600000:
            QMessageBox.warning(self, "Terrain from image", "Too many cells - use a bigger cell size.")
            return
        try:
            image = self._tw_load_guide(guide)
        except Exception as e:
            QMessageBox.warning(self, "Terrain from image", str(e))
            return
        name = os.path.splitext(os.path.basename(guide.get('image', '')))[0] or f"radar_{guide.get('radar')}"
        t = new_terrain(width, depth, cell, origin, tile, name)
        t.guide = guide
        if paint:
            t.mats = classify_cells(image, t.ny, t.nx, self._tw_library)
        self._tw_terrain, self._tw_guide = t, image
        self._tw_path, self._tw_dirty = '', True
        self._tw_brush.undo_stack.clear()
        self._tw_guide_textures()
        radar = 'radar' in guide
        self._tw_toggle_guide(not radar)                 # radar: real radar layer under the ground
        self._tw_show(fit=True)
        if radar:
            self._tw_radar_layer(True)

    def _tw_guide_textures(self): #vers 1
        """Upload one guide texture per tile (old ones dropped)."""
        vp, t = self.preview_widget, self._tw_terrain
        ids = getattr(vp, '_tex_ids', {})
        old = [ids.pop(k) for k in [k for k in ids if k.startswith('tguide_')]]
        if old:
            from OpenGL.GL import glDeleteTextures
            vp.makeCurrent()
            glDeleteTextures(old)
            vp.doneCurrent()
        if t is None or getattr(self, '_tw_guide', None) is None:
            return
        texs = []
        for tx, ty, i0, j0 in tile_ranges(t):
            i1, j1 = min(i0 + t.tile_cells, t.nx), min(j0 + t.tile_cells, t.ny)
            frac = (i0 / t.nx, 1 - j1 / t.ny, i1 / t.nx, 1 - j0 / t.ny)   # image top = north
            texs.append(guide_tile_texture(self._tw_guide, frac, f"tguide_{tx}_{ty}"))
        vp._upload_textures(texs, additive=True)

    def _tw_toggle_guide(self, on: bool): #vers 3
        """Show the guide picture on the ground (or the painted materials)."""
        on = bool(on) and getattr(self, '_tw_guide', None) is not None
        self._tw_guide_on = on
        if on and self.preview_widget._mode == 'solid':   # picture needs textured view
            self.preview_widget.set_render_mode('textured')
        btn = getattr(self, '_tw_guide_btn', None)
        if btn is not None:
            btn.blockSignals(True)
            btn.setChecked(on)
            btn.blockSignals(False)
        if self._tw_terrain is not None and hasattr(self, '_tw_entries'):   # tiles already shown
            self._tw_refresh_tiles(None)
            self._tw_status()

    def _tw_radar_available(self, game: str) -> bool: #vers 1
        """Radar tiles for game: from From image / radar, or the loaded game is that game."""
        if getattr(self, '_tw_radar_game', None) == game and getattr(self, '_tw_radar_tiles', None):
            return True
        loaded = getattr(getattr(self, '_world_loader', None), 'game', None)
        return getattr(self, '_model_cache', None) is not None and loaded == game

    def _tw_game_changed(self, *_a): #vers 1
        """Export game picked: the radar layer follows it, or turns off when that game's radar
        isn't available (button disabled until it is)."""
        game = self._tw_game.currentData()
        ok = self._tw_radar_available(game)
        btn = self._tw_radar_btn
        btn.setEnabled(ok)
        btn.setToolTip(f"Show the {self._tw_game.currentText()} radar map under the ground" if ok else
                       f"No {self._tw_game.currentText()} radar: load that game, or use From image / radar")
        if btn.isChecked():
            self._tw_radar_layer(ok)

    def _tw_radar_layer(self, on: bool): #vers 2
        """Radar map of the export game under the terrain (move / rotate / scale with Align)."""
        game = self._tw_game.currentData()
        tiles = getattr(self, '_tw_radar_tiles', None) if getattr(self, '_tw_radar_game', None) == game else None
        if on and not tiles:
            if not self._tw_radar_available(game):
                QMessageBox.information(self, "Radar layer", f"No {self._tw_game.currentText()} radar: "
                                        "use From image / radar, or load that game in this workshop first.")
                on = False
            else:
                tiles = self._load_radar_tex_tiles(game)
                self._tw_radar_tiles, self._tw_radar_game = tiles, game
        self.preview_widget.set_radar_tex_layer(on, tiles if on else None, game)
        self._apply_radar_settings()
        btn = getattr(self, '_tw_radar_btn', None)
        if btn is not None:
            btn.blockSignals(True)
            btn.setChecked(on)
            btn.blockSignals(False)
