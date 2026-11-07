#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_skin_func.py - Version: 1
# X-Seti - November 07 2026 - IMG Factory 1.6 - Vehicle Workshop skins

"""
Vehicle skins: the car's TXD textures go to DP5 Workshop and back, a PNG
replaces a texture or adds a new one, textures export as PNG, and the TXD
saves to a file or back into the IMG it came from (original kept in
vehicle_backup/ beside the IMG).
"""

##Methods list -
# _pow2

##class VehicleSkinMixin: -
# _skin_add
# _skin_bar
# _skin_export
# _skin_from_dp5
# _skin_img_entry
# _skin_item
# _skin_menu
# _skin_png
# _skin_replace
# _skin_save
# _skin_selected
# _skin_set_textures
# _skin_to_dp5
# _skin_update

import math
import os

from PyQt6.QtCore import Qt
from PyQt6.QtGui import QIcon, QImage, QPixmap
from PyQt6.QtWidgets import (QFileDialog, QHBoxLayout, QInputDialog, QListWidgetItem, QMenu, QMessageBox,
                             QToolButton, QWidget)

_ROLE = Qt.ItemDataRole.UserRole


def _pow2(n: int) -> int: #vers 1
    """Nearest power of two, 4 to 1024."""
    return min(1024, max(4, 1 << round(math.log2(max(n, 1)))))


class VehicleSkinMixin: #vers 1
    """Texture skin tools for Vehicle Workshop."""

    def _skin_bar(self) -> QWidget: #vers 1
        """Buttons under the texture list."""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as I
        bar = QWidget()
        lay = QHBoxLayout(bar)
        lay.setContentsMargins(0, 0, 0, 0)
        lay.setSpacing(2)
        for text, icon, tip, fn in (("DP5", 'dp5_workshop_icon', "Paint the selected textures in DP5 Workshop",
                                     self._skin_to_dp5),
                                    ("Replace", 'import_icon', "Replace the selected texture with a PNG",
                                     self._skin_replace),
                                    ("Add", 'add_icon', "Add a PNG as a new texture", self._skin_add),
                                    ("Export", 'export_icon', "Save the selected textures as PNG",
                                     self._skin_export),
                                    ("Save", 'save_icon', "Save the TXD: file or back into its IMG",
                                     self._skin_save)):
            b = QToolButton()
            b.setText(text)
            b.setToolTip(tip)
            try:
                b.setIcon(getattr(I, icon)())
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            except Exception:
                pass
            b.clicked.connect(fn)
            lay.addWidget(b)
        lay.addStretch(1)
        self._tex_list.setSelectionMode(self._tex_list.SelectionMode.ExtendedSelection)
        self._tex_list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._tex_list.customContextMenuRequested.connect(self._skin_menu)
        return bar

    def _skin_set_textures(self, textures: list, data: bytes, path: str): #vers 1
        """The car's own TXD (editable); list items carry their texture."""
        from apps.methods.txd_splice import tag_loaded_texture
        for t in textures:
            tag_loaded_texture(t)
        self._skin_textures, self._skin_bytes, self._skin_path = textures, data, path
        self._skin_dirty = False
        for i in range(min(self._tex_list.count(), len(textures))):
            self._tex_list.item(i).setData(_ROLE, i)          # index: Qt copies dicts

    def _skin_selected(self) -> list: #vers 1
        """Selected textures of the car's TXD (all when none selected)."""
        own = getattr(self, '_skin_textures', None) or []
        sel = [own[it.data(_ROLE)] for it in self._tex_list.selectedItems()
               if isinstance(it.data(_ROLE), int) and it.data(_ROLE) < len(own)]
        return sel or list(own)

    def _skin_item(self, tex: dict): #vers 1
        """List item showing a texture (made when missing)."""
        idx = next(i for i, t in enumerate(self._skin_textures) if t is tex)
        for i in range(self._tex_list.count()):
            if self._tex_list.item(i).data(_ROLE) == idx:
                return self._tex_list.item(i)
        it = QListWidgetItem()
        it.setData(_ROLE, idx)
        self._tex_list.addItem(it)
        return it

    def _skin_update(self, tex: dict): #vers 1
        """Thumbnail, label and viewport after a pixel change."""
        w, h, rgba = tex['width'], tex['height'], tex['rgba_data']
        it = self._skin_item(tex)
        it.setText(f"{tex['name']}  {w}\xd7{h}  *")
        img = QImage(rgba, w, h, w * 4, QImage.Format.Format_RGBA8888).copy()
        it.setIcon(QIcon(QPixmap.fromImage(img).scaled(32, 32, Qt.AspectRatioMode.KeepAspectRatio,
                                                       Qt.TransformationMode.SmoothTransformation)))
        self.viewport.drop_textures([tex['name']])
        self.viewport._upload_textures([tex], additive=True)
        self.viewport.update()
        self._skin_dirty = True

    def _skin_png(self, title: str): #vers 1
        """(rgba, w, h, has_alpha) from a picked image; None when cancelled."""
        p, _ = QFileDialog.getOpenFileName(self, title, '', "Images (*.png *.bmp *.jpg *.jpeg *.tga)")
        if not p:
            return None
        img = QImage(p)
        if img.isNull():
            QMessageBox.warning(self, "Vehicle skin", f"Not an image: {os.path.basename(p)}")
            return None
        img = img.convertToFormat(QImage.Format.Format_RGBA8888)
        rgba = bytes(img.constBits().asstring(img.sizeInBytes()))
        return rgba, img.width(), img.height(), img.hasAlphaChannel()

    def _skin_to_dp5(self): #vers 1
        from apps.methods.dp5_link import send_textures_to_dp5
        texs = self._skin_selected()
        if not texs:
            self._set_status("Load a vehicle TXD first")
            return
        try:
            n = send_textures_to_dp5(self, texs, "Vehicle Workshop", self._skin_from_dp5)
        except ImportError:
            self._set_status("DP5 Workshop is not part of this build - use Export / Replace")
            return
        self._set_status(f"{n} texture(s) sent to DP5 - Send back returns them here")

    def _skin_from_dp5(self, tex, rgba, w, h): #vers 1
        """DP5 Send back: painted pixels into the texture."""
        from apps.methods.dp5_link import texture_from_dp5
        if tex not in (getattr(self, '_skin_textures', None) or []) or texture_from_dp5(tex, rgba, w, h) is None:
            self._set_status("DP5: that texture is no longer loaded here")
            return
        self._skin_update(tex)
        self._set_status(f"{tex['name']} painted in DP5 - Save to keep")

    def _skin_replace(self): #vers 1
        """PNG into the selected texture (scaled to its size)."""
        own = getattr(self, '_skin_textures', None) or []
        sel = [own[it.data(_ROLE)] for it in self._tex_list.selectedItems()
               if isinstance(it.data(_ROLE), int) and it.data(_ROLE) < len(own)]
        if len(sel) != 1:
            self._set_status("Select one texture of the car's TXD to replace")
            return
        got = self._skin_png(f"Image for {sel[0]['name']}")
        if not got:
            return
        rgba, w, h, alpha = got
        tex = sel[0]
        if (w, h) != (tex['width'], tex['height']):
            img = QImage(rgba, w, h, w * 4, QImage.Format.Format_RGBA8888).scaled(
                tex['width'], tex['height'], Qt.AspectRatioMode.IgnoreAspectRatio,
                Qt.TransformationMode.SmoothTransformation)
            rgba, w, h = bytes(img.constBits().asstring(img.sizeInBytes())), img.width(), img.height()
        from apps.methods.dp5_link import texture_from_dp5
        texture_from_dp5(tex, rgba, w, h)
        if alpha:
            tex['has_alpha'] = True
        self._skin_update(tex)
        self._set_status(f"{tex['name']} replaced - Save to keep")

    def _skin_add(self): #vers 1
        """PNG as a new texture of the car's TXD."""
        if getattr(self, '_skin_textures', None) is None:
            self._set_status("Load a vehicle TXD first")
            return
        got = self._skin_png("Image for the new texture")
        if not got:
            return
        rgba, w, h, alpha = got
        name, ok = QInputDialog.getText(self, "New texture", "Texture name (as the model's material uses it):")
        name = name.strip()
        if not ok or not name:
            return
        if any(t['name'].lower() == name.lower() for t in self._skin_textures):
            QMessageBox.warning(self, "Vehicle skin", f"{name} is already in the TXD - use Replace")
            return
        nw, nh = _pow2(w), _pow2(h)                              # power of two for the game
        if (nw, nh) != (w, h):
            img = QImage(rgba, w, h, w * 4, QImage.Format.Format_RGBA8888).scaled(
                nw, nh, Qt.AspectRatioMode.IgnoreAspectRatio, Qt.TransformationMode.SmoothTransformation)
            rgba, w, h = bytes(img.constBits().asstring(img.sizeInBytes())), nw, nh
        tex = {'name': name, 'alpha_name': '', 'width': w, 'height': h, 'depth': 32, 'rgba_data': rgba,
               'has_alpha': alpha, 'format': 'DXT3' if alpha else 'DXT1', 'mipmap_levels': [],
               'compressed_data': b''}
        self._skin_textures.append(tex)
        self._skin_update(tex)
        self._set_status(f"{name} added ({w}\xd7{h}) - Save to keep")

    def _skin_export(self): #vers 1
        texs = self._skin_selected()
        if not texs:
            return
        d = QFileDialog.getExistingDirectory(self, "Export textures to")
        if not d:
            return
        for t in texs:
            QImage(t['rgba_data'], t['width'], t['height'], t['width'] * 4,
                   QImage.Format.Format_RGBA8888).save(os.path.join(d, t['name'] + '.png'))
        self._set_status(f"{len(texs)} PNG(s) saved to {d}")

    def _skin_img_entry(self): #vers 1
        """(IMG, entry) holding this TXD, or (None, None)."""
        img = getattr(self, '_current_img', None)
        name = os.path.basename(getattr(self, '_skin_path', '') or '').lower()
        if img is None or not name:
            return None, None
        e = next((x for x in getattr(img, 'entries', []) if x.name.lower() == name), None)
        return (img, e) if e is not None else (None, None)

    def _skin_save(self): #vers 1
        """TXD back into its IMG (original backed up) or to a file."""
        from apps.methods.txd_splice import txd_from_textures
        texs = getattr(self, '_skin_textures', None)
        if not texs:
            self._set_status("No vehicle TXD loaded")
            return
        try:
            data = txd_from_textures(texs, getattr(self, '_skin_bytes', None))
        except Exception as e:
            QMessageBox.warning(self, "Vehicle skin", f"TXD not built: {e}")
            return
        img, entry = self._skin_img_entry()
        if img is not None:
            box = QMessageBox(self)
            box.setWindowTitle("Save TXD")
            box.setText(f"Write {entry.name} back into {os.path.basename(getattr(img, 'file_path', 'the IMG'))}?")
            into = box.addButton("Into IMG", QMessageBox.ButtonRole.AcceptRole)
            as_file = box.addButton("Save as file", QMessageBox.ButtonRole.AcceptRole)
            box.addButton(QMessageBox.StandardButton.Cancel)
            box.exec()
            if box.clickedButton() is into:
                bak = os.path.join(os.path.dirname(img.file_path), 'vehicle_backup')
                keep = os.path.join(bak, entry.name)
                if not os.path.exists(keep):
                    os.makedirs(bak, exist_ok=True)
                    with open(keep, 'wb') as f:
                        f.write(img.read_entry_data(entry))
                if not img.add_entry(entry.name, data):
                    QMessageBox.warning(self, "Vehicle skin", f"{entry.name}: not written")
                    return
                self._skin_bytes, self._skin_dirty = data, False
                self._set_status(f"{entry.name} saved into the IMG (original in vehicle_backup/)")
                return
            if box.clickedButton() is not as_file:
                return
        start = self._skin_path or os.path.join(getattr(self, '_last_dir', ''), 'vehicle.txd')
        p, _ = QFileDialog.getSaveFileName(self, "Save TXD", start, "TXD (*.txd)")
        if not p:
            return
        with open(p, 'wb') as f:
            f.write(data)
        self._skin_bytes, self._skin_dirty = data, False
        self._set_status(f"Saved {os.path.basename(p)}")

    def _skin_menu(self, pos): #vers 1
        m = QMenu(self)
        m.addAction("Paint in DP5 Workshop", self._skin_to_dp5)
        m.addAction("Replace with image...", self._skin_replace)
        m.addAction("Add image as texture...", self._skin_add)
        m.addAction("Export as PNG...", self._skin_export)
        m.addSeparator()
        m.addAction("Save TXD...", self._skin_save)
        m.exec(self._tex_list.mapToGlobal(pos))
