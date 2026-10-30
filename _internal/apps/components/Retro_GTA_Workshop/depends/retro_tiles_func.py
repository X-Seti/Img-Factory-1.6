#this belongs in apps/components/Retro_GTA_Workshop/depends/retro_tiles_func.py - Version: 11
# X-Seti - October 23 2026 - IMG Factory 1.6 - Retro GTA Workshop tiles and sprites

"""
Tiles dock (every style tile; pick one to paint lids or set the edited
face, send it to DP5 Workshop and back) and the Sprites tab (sprite
groups of the style, enlarged preview, PNG export).
"""

##class _WheelZoom: -
# __init__
# eventFilter
#
##class RetroTilesMixin: -
# _create_sprites_tab
# _create_tiles_dock
# _export_items
# _export_png
# _export_txd
# _fill_sprites
# _fill_tiles
# _hover_enlarge
# _palette_apply
# _palette_back_from_dp5
# _palette_export
# _palette_image
# _palette_import
# _palette_to_dp5
# _rgba_pixmap
# _sprite_back_from_dp5
# _sprite_export
# _sprite_import
# _sprite_picked
# _sprite_to_dp5
# _sprite_zoom_by
# _tile_back_from_dp5
# _tile_export
# _tile_import
# _tile_picked
# _tile_to_dp5
# _tile_use_on_face

import numpy as np

from PyQt6.QtCore import QEvent, QObject, QSize, Qt
from PyQt6.QtGui import QIcon, QImage, QPixmap
from PyQt6.QtWidgets import (QComboBox, QFileDialog, QHBoxLayout, QLabel, QListView, QListWidget,
                             QListWidgetItem, QMessageBox, QPushButton, QScrollArea, QSplitter, QVBoxLayout, QWidget)


class _WheelZoom(QObject): #vers 1
    """Wheel on a viewport: zoom callback (+1 / -1); middle click: fit (0)."""

    def __init__(self, cb, parent=None): #vers 1
        super().__init__(parent)
        self._cb = cb

    def eventFilter(self, _obj, ev): #vers 1
        if ev.type() == QEvent.Type.Wheel:
            self._cb(1 if ev.angleDelta().y() > 0 else -1)
            return True
        if ev.type() == QEvent.Type.MouseButtonPress and ev.button() == Qt.MouseButton.MiddleButton:
            self._cb(0)
            return True
        return False


class RetroTilesMixin: #vers 1
    """Tile picker and sprite browser for RetroGTAWorkshop."""

    @staticmethod
    def _rgba_pixmap(a: np.ndarray) -> QPixmap: #vers 1
        a = np.ascontiguousarray(a)
        h, w = a.shape[:2]
        return QPixmap.fromImage(QImage(a.data, w, h, w * 4, QImage.Format.Format_RGBA8888).copy())

    def _create_tiles_dock(self) -> QWidget: #vers 5
        """Tile grid; Use on face / Edit in DP5; Import / Export PNG."""
        self._paint_tile = None
        page = QWidget()
        lay = QVBoxLayout(page)
        lay.setContentsMargins(4, 4, 4, 4)
        self._tile_list = QListWidget()
        self._tile_list.setViewMode(QListView.ViewMode.IconMode)
        self._tile_list.setIconSize(QSize(48, 48))
        self._tile_list.setGridSize(QSize(58, 66))
        self._tile_list.setResizeMode(QListView.ResizeMode.Adjust)
        self._tile_list.setMovement(QListView.Movement.Static)
        self._tile_list.currentRowChanged.connect(self._tile_picked)
        self._tile_list.itemDoubleClicked.connect(lambda _it: self._tile_use_on_face())
        self._hover_enlarge(self._tile_list, lambda it: self._style.tile_rgba(self._tile_list.row(it)))
        lay.addWidget(self._tile_list, 1)
        from PyQt6.QtWidgets import QToolButton
        from apps.methods.fold_widgets import ButtonRow
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as I
        ic = self._get_icon_color()
        btns = []                                        # one row; icons only when narrow
        for label, icon, slot, tip in (("Use on face", I.check_icon, self._tile_use_on_face,
                                        "Put this tile on the face picked in the Block panel (double-click too)"),
                                       ("Edit in DP5", I.paint_icon, self._tile_to_dp5,
                                        "Paint this tile in DP5 Workshop; Send back writes it into the style"),
                                       ("Import", I.import_icon, self._tile_import,
                                        "Replace this tile from a PNG of its size"),
                                       ("Export", I.export_icon, self._tile_export, "Save this tile as PNG")):
            b = QToolButton()
            b.setText(label)
            b.setIcon(icon(16, ic))
            b.setToolTip(tip)
            b.clicked.connect(lambda _c=False, s=slot: s())
            btns.append(b)
        lay.addWidget(ButtonRow(btns))
        return page

    def _hover_enlarge(self, lst, rgba_of): #vers 2
        """Hovering an icon of lst shows it 4x larger beside the cursor (hidden on leave)."""
        from PyQt6.QtCore import QEvent, QObject
        from PyQt6.QtGui import QCursor
        pop = QLabel(self, Qt.WindowType.ToolTip)
        pop.setStyleSheet("border: 1px solid palette(mid); background: palette(base);")   # theme

        def show(it):
            if self._style is None or it is None:
                return
            a = rgba_of(it)
            pm = self._rgba_pixmap(a)
            k = max(1, min(4, 256 // max(a.shape[:2])))
            pop.setPixmap(pm.scaled(pm.width() * k, pm.height() * k))
            pop.adjustSize()
            p = QCursor.pos()
            pop.move(p.x() + 16, p.y() + 16)
            pop.show()

        class _Leave(QObject):
            def eventFilter(self, _o, ev): #vers 1
                if ev.type() in (QEvent.Type.Leave, QEvent.Type.MouseButtonPress):
                    pop.hide()
                return False
        lst.setMouseTracking(True)
        lst.itemEntered.connect(show)
        lst._hover_filter = _Leave(lst)
        lst.viewport().installEventFilter(lst._hover_filter)

    def _fill_tiles(self): #vers 1
        """One icon per style tile."""
        self._tile_list.clear()
        if self._style is None:
            return
        for n in range(self._style.tile_count()):
            it = QListWidgetItem(QIcon(self._rgba_pixmap(self._style.tile_rgba(n))), str(n))
            self._tile_list.addItem(it)

    def _tile_picked(self, row: int): #vers 1
        """Tile picked: it is the paint tile."""
        if row < 0:
            return
        self._paint_tile = row
        self._set_status(f"Tile {row} picked - Paint tool puts it on lids")

    def _tile_use_on_face(self): #vers 2
        """Picked tile onto the face shown in the Block panel."""
        if self._paint_tile is None or getattr(self, '_cell', None) is None:
            self._set_status("Pick a block on the map and a tile first")
            return
        from apps.components.Retro_GTA_Workshop.depends.retro_block_func import FACES
        face = FACES[self._face_combo.currentIndex()]
        n = self._style.global_face(face, self._paint_tile)
        if n is None:
            self._set_status(f"Tile {self._paint_tile} can't go on a {face} face in this game")
            return
        self._tile_spin.setValue(n)                           # editor writes the face

    def _tile_import(self): #vers 1
        """Replace the picked tile from a PNG (nearest palette colours)."""
        from apps.methods.retro_palette import png_rgba
        n = self._paint_tile
        if n is None or self._style is None:
            self._set_status("Pick a tile first")
            return
        path, _ = QFileDialog.getOpenFileName(self, f"Import tile {n}", self._data_dir or '', "PNG images (*.png)")
        if not path:
            return
        try:
            a = png_rgba(path)
        except ValueError as ex:
            self._set_status(str(ex))
            return
        self._tile_back_from_dp5({'tile': n}, a.tobytes(), a.shape[1], a.shape[0])

    def _tile_export(self): #vers 1
        n = self._paint_tile
        if n is None or self._style is None:
            self._set_status("Pick a tile first")
            return
        path, _ = QFileDialog.getSaveFileName(self, "Export tile", f"tile_{n:03d}.png", "PNG images (*.png)")
        if path and self._rgba_pixmap(self._style.tile_rgba(n)).save(path, 'PNG'):
            self._set_status(f"Tile {n} saved: {path}")

    def _tile_to_dp5(self): #vers 2
        """Send the picked tile to DP5 Workshop."""
        from apps.methods.dp5_link import send_textures_to_dp5
        n = self._paint_tile
        if n is None or self._style is None:
            return
        a = self._style.tile_rgba(n)
        tex = {'name': f"tile_{n}", 'width': a.shape[1], 'height': a.shape[0], 'tile': n,
               'rgba_data': a.tobytes()}
        if send_textures_to_dp5(self, [tex], self.App_name, self._tile_back_from_dp5):
            self._set_status(f"Tile {n} sent to DP5 Workshop")

    def _tile_back_from_dp5(self, tex, rgba, w, h): #vers 2
        """DP5 Send back: pixels into the style tile (nearest colours of its palette)."""
        n = tex['tile']
        th, tw = self._style.tile_rgba(n).shape[:2]
        if (w, h) != (tw, th):
            self._set_status(f"DP5: tiles stay {tw} x {th} - not written")
            return
        self._style.set_tile_rgba(n, np.frombuffer(bytes(rgba), np.uint8).reshape(th, tw, 4))
        self._style_dirty = True
        self._tile_list.item(n).setIcon(QIcon(self._rgba_pixmap(self._style.tile_rgba(n))))
        self._view.clear_cache()                           # tile redrawn everywhere
        self._update_title()
        self._set_status(f"Tile {n} updated from DP5 - Save style to keep")

    def _export_items(self) -> list: #vers 1
        """(name, sub folder, RGBA array) for every tile and sprite of the style."""
        st = self._style
        out = [(f"tile_{n:03d}", "tiles", st.tile_rgba(n)) for n in range(st.tile_count())]
        bases = st.sprite_bases()
        names = list(bases)
        for i, g in enumerate(names):
            end = bases[names[i + 1]] if i + 1 < len(names) else st.sprite_count()
            for n in range(bases[g], end):
                out.append((f"{g.replace(' ', '_')}_{n - bases[g]:03d}", f"sprites/{g}", st.sprite_rgba(n)))
        return out

    def _export_png(self): #vers 1
        """All tiles and sprites as PNG files (tiles/, sprites/<group>/)."""
        import os
        if self._style is None:
            return
        folder = QFileDialog.getExistingDirectory(self, "Export tiles and sprites to folder", self._data_dir or '')
        if not folder:
            return
        stem = os.path.splitext(os.path.basename(self._style.path))[0]
        n = 0
        for name, sub, a in self._export_items():
            d = os.path.join(folder, stem, sub)
            os.makedirs(d, exist_ok=True)
            if a.size and self._rgba_pixmap(a).save(os.path.join(d, name + '.png'), 'PNG'):
                n += 1
        self._set_status(f"Exported {n} PNGs to {os.path.join(folder, stem)}")

    def _export_txd(self): #vers 2
        """All tiles and sprites in one TXD (32-bit, SA / D3D8 layout) for TXD Workshop."""
        import os
        from apps.methods.file_backup import safe_write_bytes
        from apps.methods.txd_splice import build_txd
        if self._style is None:
            return
        stem = os.path.splitext(os.path.basename(self._style.path))[0]
        path, _ = QFileDialog.getSaveFileName(self, "Export TXD", os.path.join(self._data_dir or '', stem + '.txd'),
                                              "TXD files (*.txd)")
        if not path:
            return
        from apps.methods.retro_mesh import rgba_txd_entry
        texs = [rgba_txd_entry(name, a) for name, _sub, a in self._export_items() if a.size]
        try:
            safe_write_bytes(path, build_txd(texs, 0x1803FFFF, platform=8), f"Retro GTA: {stem}.txd")
        except Exception as ex:
            self._set_status(f"Export TXD failed: {ex}")
            return
        self._set_status(f"Exported {len(texs)} textures to {path}")

    def _palette_image(self) -> np.ndarray: #vers 1
        """All style palettes as one RGBA image: a row per palette, a pixel per colour (256 wide)."""
        st = self._style
        img = np.zeros((st.palette_count(), 256, 4), np.uint8)
        for i in range(len(img)):
            a = st.palette_rgba(i)
            img[i, :len(a)] = a
        return img

    def _palette_apply(self, img: np.ndarray) -> int: #vers 1
        """Changed palette rows of an edited palette image written into the style; returns the count."""
        st = self._style
        now = self._palette_image()
        if img.shape[:2] != now.shape[:2]:
            raise ValueError(f"Palette image must be {now.shape[1]} x {now.shape[0]}")
        rows = [i for i in range(len(now)) if not np.array_equal(img[i], now[i])]
        for i in rows:
            st.set_palette_rgba(i, img[i])
        if rows:
            self._style_dirty = True
            self._view.clear_cache()
            self._fill_tiles()
            self._fill_sprites()
            self._update_title()
        return len(rows)

    def _palette_export(self): #vers 1
        import os
        if self._style is None:
            return
        stem = os.path.splitext(os.path.basename(self._style.path))[0]
        path, _ = QFileDialog.getSaveFileName(self, "Export palettes", stem + "_palettes.png", "PNG images (*.png)")
        if path and self._rgba_pixmap(self._palette_image()).save(path, 'PNG'):
            self._set_status(f"Palettes saved: {path} (a row per palette, a pixel per colour)")

    def _palette_import(self): #vers 1
        """Edited palette PNG back into the style (changed rows only)."""
        from apps.methods.retro_palette import png_rgba
        if self._style is None:
            return
        path, _ = QFileDialog.getOpenFileName(self, "Import palettes", '', "PNG images (*.png)")
        if not path:
            return
        try:
            n = self._palette_apply(png_rgba(path))
        except ValueError as ex:
            QMessageBox.warning(self, self.App_name, str(ex))
            return
        self._set_status(f"{n} palettes changed - Save style to keep")

    def _palette_to_dp5(self): #vers 1
        """Palette image to DP5 Workshop; Send back writes the changed palettes."""
        from apps.methods.dp5_link import send_textures_to_dp5
        if self._style is None:
            return
        a = self._palette_image()
        tex = {'name': "palettes", 'width': a.shape[1], 'height': a.shape[0], 'rgba_data': a.tobytes()}
        if send_textures_to_dp5(self, [tex], self.App_name, self._palette_back_from_dp5):
            self._set_status("Palettes sent to DP5 Workshop (a row per palette)")

    def _palette_back_from_dp5(self, _tex, rgba, w, h): #vers 1
        try:
            n = self._palette_apply(np.frombuffer(bytes(rgba), np.uint8)[:w * h * 4].reshape(h, w, 4))
        except ValueError as ex:
            self._set_status(f"DP5: {ex}")
            return
        self._set_status(f"{n} palettes changed from DP5 - Save style to keep")

    def _create_sprites_tab(self) -> QWidget: #vers 3
        """Sprite group combo, sprite icons, enlarged preview, PNG export."""
        page = QWidget()
        lay = QHBoxLayout(page)
        left = QWidget()
        ll = QVBoxLayout(left)
        ll.setContentsMargins(0, 0, 0, 0)
        self._sprite_group = QComboBox()
        self._sprite_group.currentIndexChanged.connect(lambda _i: self._fill_sprites())
        ll.addWidget(self._sprite_group)
        self._sprite_list = QListWidget()
        self._sprite_list.setViewMode(QListView.ViewMode.IconMode)
        self._sprite_list.setIconSize(QSize(48, 48))
        self._sprite_list.setGridSize(QSize(60, 68))
        self._sprite_list.setResizeMode(QListView.ResizeMode.Adjust)
        self._sprite_list.setMovement(QListView.Movement.Static)
        self._sprite_list.currentItemChanged.connect(lambda it, _p: self._sprite_picked(it))
        ll.addWidget(self._sprite_list, 1)
        right = QWidget()
        rl = QVBoxLayout(right)
        self._sprite_info = QLabel("")
        self._sprite_prev = QLabel()
        self._sprite_prev.setAlignment(Qt.AlignmentFlag.AlignCenter)
        scroll = QScrollArea()
        scroll.setWidget(self._sprite_prev)
        scroll.setWidgetResizable(True)
        self._sprite_zoom = 0                          # 0 = fit, else pixels per sprite pixel
        self._zoom_filter = _WheelZoom(self._sprite_zoom_by, scroll)
        scroll.viewport().installEventFilter(self._zoom_filter)
        scroll.setToolTip("Wheel: zoom in / out, middle click: fit")
        imp = QPushButton("Import")
        imp.setToolTip("Replace the sprite from a PNG of the same size")
        imp.clicked.connect(lambda _c=False: self._sprite_import())
        exp = QPushButton("Export")
        exp.setToolTip("Save the sprite as a PNG")
        exp.clicked.connect(lambda _c=False: self._sprite_export())
        dp5 = QPushButton("Edit in DP5")
        dp5.setToolTip("Paint the sprite in DP5 Workshop; Send back writes it into the style")
        dp5.clicked.connect(lambda _c=False: self._sprite_to_dp5())
        rl.addWidget(self._sprite_info)
        rl.addWidget(scroll, 1)
        btns = QHBoxLayout()
        for b in (imp, exp, dp5):
            btns.addWidget(b)
        rl.addLayout(btns)
        sp = QSplitter()
        sp.addWidget(left)
        sp.addWidget(right)
        sp.setSizes([500, 400])
        lay.addWidget(sp)
        return page

    def _fill_sprites(self): #vers 1
        """Icons for the sprites of the picked group."""
        self._sprite_list.clear()
        st = self._style
        if st is None or not st.sprite_count():
            return
        if self._sprite_group.count() == 0:
            self._sprite_group.blockSignals(True)
            for name in st.sprite_bases():
                self._sprite_group.addItem(name.replace('_', ' ').title(), name)
            self._sprite_group.blockSignals(False)
        bases = st.sprite_bases()
        names = list(bases)
        g = self._sprite_group.currentData() or names[0]
        i = names.index(g)
        end = bases[names[i + 1]] if i + 1 < len(names) else st.sprite_count()
        for n in range(bases[g], end):
            it = QListWidgetItem(QIcon(self._rgba_pixmap(st.sprite_rgba(n))), str(n))
            it.setData(Qt.ItemDataRole.UserRole, n)
            self._sprite_list.addItem(it)

    def _sprite_picked(self, item): #vers 2
        if item is None:
            return
        n = item.data(Qt.ItemDataRole.UserRole)
        a = self._style.sprite_rgba(n)
        pm = self._rgba_pixmap(a)
        k = self._sprite_zoom or max(1, min(6, 384 // max(a.shape[:2])))
        self._sprite_prev.setPixmap(pm.scaled(pm.width() * k, pm.height() * k))
        self._sprite_info.setText(f"Sprite {n}: {a.shape[1]} x {a.shape[0]}   zoom {k}x")

    def _sprite_zoom_by(self, step: int): #vers 1
        """Wheel zoom of the sprite preview (0 step: back to fit)."""
        it = self._sprite_list.currentItem()
        if it is None:
            return
        a = self._style.sprite_rgba(it.data(Qt.ItemDataRole.UserRole))
        cur = self._sprite_zoom or max(1, min(6, 384 // max(a.shape[:2])))
        self._sprite_zoom = 0 if step == 0 else max(1, min(16, cur + step))
        self._sprite_picked(it)

    def _sprite_import(self): #vers 2
        """Replace the picked sprite from a PNG of its size (nearest palette colours)."""
        from apps.methods.retro_palette import png_rgba
        it = self._sprite_list.currentItem()
        if it is None or self._style is None:
            return
        n = it.data(Qt.ItemDataRole.UserRole)
        path, _ = QFileDialog.getOpenFileName(self, f"Import sprite {n}", '', "PNG images (*.png)")
        if not path:
            return
        try:
            a = png_rgba(path)
        except ValueError as ex:
            self._set_status(str(ex))
            return
        self._sprite_back_from_dp5({'sprite': n}, a.tobytes(), a.shape[1], a.shape[0])

    def _sprite_to_dp5(self): #vers 1
        """Send the picked sprite to DP5 Workshop."""
        from apps.methods.dp5_link import send_textures_to_dp5
        it = self._sprite_list.currentItem()
        if it is None or self._style is None:
            return
        n = it.data(Qt.ItemDataRole.UserRole)
        a = np.ascontiguousarray(self._style.sprite_rgba(n))
        tex = {'name': f"sprite_{n}", 'width': a.shape[1], 'height': a.shape[0], 'sprite': n,
               'rgba_data': a.tobytes()}
        if send_textures_to_dp5(self, [tex], self.App_name, self._sprite_back_from_dp5):
            self._set_status(f"Sprite {n} sent to DP5 Workshop")

    def _sprite_back_from_dp5(self, tex, rgba, w, h): #vers 1
        """DP5 Send back: pixels into the style sprite (nearest colours of its palette)."""
        n = tex['sprite']
        sh, sw = self._style.sprite_rgba(n).shape[:2]
        if (w, h) != (sw, sh):
            self._set_status(f"DP5: sprite {n} stays {sw} x {sh} - not written")
            return
        try:
            self._style.set_sprite_rgba(n, np.frombuffer(bytes(rgba), np.uint8)[:w * h * 4].reshape(h, w, 4))
        except ValueError as ex:
            self._set_status(str(ex))
            return
        self._style_dirty = True
        self._fill_sprites()
        self._update_title()
        self._set_status(f"Sprite {n} written - save the style to keep it")

    def _sprite_export(self): #vers 1
        it = self._sprite_list.currentItem()
        if it is None:
            return
        n = it.data(Qt.ItemDataRole.UserRole)
        path, _ = QFileDialog.getSaveFileName(self, "Export sprite", f"sprite_{n}.png", "PNG images (*.png)")
        if path and self._rgba_pixmap(self._style.sprite_rgba(n)).save(path, 'PNG'):
            self._set_status(f"Sprite {n} saved: {path}")
