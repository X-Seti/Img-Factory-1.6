#this belongs in apps/components/Weapon_Workshop/depends/weapon_model_func.py - Version: 1
# X-Seti - November 02 2026 - IMG Factory 1.6 - Weapon Workshop models

"""
Weapon Workshop models: weapon list from default.ide, 3D view, textures with
thumbnails; import / export of DFF, TXD, texture PNGs and COL (when one
exists) into the game's gta3.img.
"""

##Methods list -
#
##class WeaponModelMixin: -
# _create_model_view
# _create_weapons_panel
# _wm_entry
# _wm_export
# _wm_export_pngs
# _wm_import
# _wm_import_png
# _wm_load_game
# _wm_select
# _wm_selected_name
# _wm_source

import os

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QIcon, QImage, QPixmap
from PyQt6.QtWidgets import (QFileDialog, QLabel, QListWidget, QListWidgetItem, QMessageBox, QSplitter,
                             QVBoxLayout, QWidget)


class WeaponModelMixin: #vers 1
    """Weapon models, textures and collision for WeaponWorkshop (IMG entries or III / loose files)."""

    def _create_weapons_panel(self): #vers 1
        """Weapon model list over the selected model's textures."""
        w = QWidget()
        lay = QVBoxLayout(w)
        lay.setContentsMargins(2, 2, 2, 2)
        self._wm_lbl = QLabel("Weapon models load with the game folder")
        self._wm_lbl.setWordWrap(True)
        lay.addWidget(self._wm_lbl)
        sp = QSplitter(Qt.Orientation.Vertical)
        self._wm_list = QListWidget()
        self._wm_list.currentRowChanged.connect(self._wm_select)
        sp.addWidget(self._wm_list)
        self._wm_tex = QListWidget()
        self._wm_tex.setIconSize(QSize(48, 48))
        self._wm_tex.setToolTip("Textures of the selected weapon (Import PNG replaces the selected one)")
        sp.addWidget(self._wm_tex)
        sp.setSizes([300, 200])
        lay.addWidget(sp, 1)
        self._wm_arc, self._wm_rows, self._wm_cols, self._wm_root = None, [], None, ''
        return w

    def _create_model_view(self): #vers 1
        """Host for the 3D view (made on first model)."""
        w = QWidget()
        self._wm_view_host = QVBoxLayout(w)
        self._wm_view_host.setContentsMargins(0, 0, 0, 0)
        self._wm_vp = None
        return w

    def _wm_load_game(self, game_root: str): #vers 1
        """Weapon list from default.ide; models from models/gta3.img (III / VC through gta3.dir)."""
        from apps.methods.img_core_classes import IMGFile
        from apps.methods.weapon_models import _find, weapon_models
        models = _find(game_root, 'models')
        path = _find(models, 'gta3.dir') or _find(models, 'gta3.img')
        self._wm_rows = weapon_models(game_root)
        self._wm_root = game_root
        self._wm_list.clear()
        self._wm_cols = None
        if not self._wm_rows:
            self._wm_arc = None
            self._wm_lbl.setText(f"No weapon models in {game_root}")
            return
        arc = IMGFile(path) if path else None
        self._wm_arc = arc if arc is not None and arc.open() else None
        self._wm_lbl.setText(f"{len(self._wm_rows)} weapons" + (f", {os.path.basename(path)}" if self._wm_arc else ""))
        for wid, model, txd in self._wm_rows:
            self._wm_list.addItem(f"{wid}  {model}" + (f"  ({txd})" if txd.lower() != model.lower() else ""))

    def _wm_entry(self, name: str, ext: str): #vers 1
        """IMG entry name+ext (case-insensitive), or None."""
        if self._wm_arc is None:
            return None
        want = (name + ext).lower()
        return next((e for e in self._wm_arc.entries if e.name.lower() == want), None)

    def _wm_selected_name(self): #vers 1
        """(model, txd) of the selected weapon, or (None, None)."""
        row = self._wm_list.currentRow()
        if not (0 <= row < len(self._wm_rows)):
            return None, None
        return self._wm_rows[row][1], self._wm_rows[row][2]

    def _wm_source(self, kind: str): #vers 1
        """(bytes, write(new bytes), label) of the selected weapon's dff / txd / col, or None."""
        from apps.methods.file_backup import safe_write_bytes
        from apps.methods.weapon_models import _find
        model, txd = self._wm_selected_name()
        if model is None:
            return None
        arc, root = self._wm_arc, self._wm_root
        models = _find(root, 'models')

        def img_write(e):  #vers 1
            return lambda new: arc.add_entry(e.name, new)           # replaces the entry

        def file_write(path):  #vers 1
            return lambda new: safe_write_bytes(path, new, f"{self.App_name}: {os.path.basename(path)}") or True
        if kind in ('dff', 'txd'):
            name = model if kind == 'dff' else txd
            e = self._wm_entry(name, '.' + kind)
            if e is not None:
                return arc.read_entry_data(e), img_write(e), f"{e.name} in {os.path.basename(arc.file_path)}"
            path = (_find(_find(models, 'generic'), 'weapons.dff') if kind == 'dff'   # III: one file
                    else _find(models, name + '.txd'))
            if path:
                with open(path, 'rb') as f:
                    return f.read(), file_write(path), os.path.basename(path) + " (shared file)"
            return None
        from apps.methods.col_splice import _rec_name, split_records
        from apps.methods.weapon_models import col_index, replace_col_record
        path = _find(_find(models, 'coll'), 'weapons.col')
        if path:
            with open(path, 'rb') as f:
                data = f.read()
            if any(_rec_name(r) == model.lower() for r in split_records(data)[0]):
                return data, file_write(path), os.path.basename(path)
        if arc is not None:
            if self._wm_cols is None:
                self._wm_cols = col_index(arc)
            e = self._wm_cols.get(model.lower())
            if e is not None:
                return arc.read_entry_data(e), img_write(e), f"{e.name} in {os.path.basename(arc.file_path)}"
        return None

    def _wm_select(self, row: int): #vers 1
        """Show the weapon in 3D (III: its part of weapons.dff) and its textures as thumbnails."""
        from apps.methods.dff_parser import parse_model_any
        from apps.methods.txd_parser import parse_txd
        model, txd = self._wm_selected_name()
        if model is None:
            return
        if self._wm_vp is None:
            from apps.methods.dff_viewport import DFFViewport
            self._wm_vp = DFFViewport()
            self._wm_view_host.addWidget(self._wm_vp)
        vp = self._wm_vp
        vp.clear_textures()
        self._wm_tex.clear()
        src = self._wm_source('txd')
        texs = parse_txd(src[0]) if src else []
        if texs:
            vp._upload_textures(texs)
        for x in texs:
            item = QListWidgetItem(f"{x['name']}  {x['width']}x{x['height']} {x.get('format', '')}")
            rgba = x.get('rgba_data')
            if rgba:
                img = QImage(bytes(rgba), x['width'], x['height'], QImage.Format.Format_RGBA8888).copy()
                item.setIcon(QIcon(QPixmap.fromImage(img)))
            item.setData(Qt.ItemDataRole.UserRole, x['name'])
            self._wm_tex.addItem(item)
        src = self._wm_source('dff')
        dff = parse_model_any(src[0], model) if src else None
        if dff is None or not dff.geometries:
            vp.load_all_geometries([], [], [], [])
            self._set_status(f"{model}: no model found")
            return
        atomics = dff.atomics
        names = {i: (f.name or '').strip().lower() for i, f in enumerate(dff.frames)}
        own = [a for a in atomics if names.get(a.frame_index, '').split('_l')[0] == model.lower()]
        if own and len(own) < len(atomics):                  # III weapons.dff: this weapon's atomics
            atomics = own
        vp.set_render_mode('textured')
        vp.load_all_geometries(dff.geometries, [g.materials for g in dff.geometries], dff.frames, atomics)
        vp.fit_to_window()
        col = self._wm_source('col')
        self._set_status(f"{model}: {src[2]}, {len(texs)} textures" + (f", collision in {col[2]}" if col else ""))

    def _wm_export(self, kind: str): #vers 1
        """Save the selected weapon's .dff / .txd / .col (its own model record) to a file."""
        model, txd = self._wm_selected_name()
        src = self._wm_source(kind)
        if src is None:
            QMessageBox.information(self, self.App_name, f"{model}: no {kind.upper()} found")
            return
        data = src[0]
        if kind == 'col':
            from apps.methods.col_splice import _rec_name, split_records
            data = next(r for r in split_records(data)[0] if _rec_name(r) == model.lower())
        name = txd if kind == 'txd' else model
        path, _ = QFileDialog.getSaveFileName(self, f"Export {name}.{kind}", f"{name}.{kind}", f"*.{kind}")
        if path:
            from apps.methods.file_backup import safe_write_bytes
            safe_write_bytes(path, bytes(data), f"{self.App_name}: export {name}.{kind}")
            self._set_status(f"Exported {os.path.basename(path)} from {src[2]}")

    def _wm_import(self, kind: str): #vers 1
        """Replace the selected weapon's .dff / .txd / .col model record where it lives."""
        model, txd = self._wm_selected_name()
        src = self._wm_source(kind)
        if src is None:
            QMessageBox.information(self, self.App_name, f"{model}: no {kind.upper()} found to replace")
            return
        name = txd if kind == 'txd' else model
        path, _ = QFileDialog.getOpenFileName(self, f"Import over {src[2]}", "", f"*.{kind}")
        if not path:
            return
        with open(path, 'rb') as f:
            new = f.read()
        if kind == 'col':
            from apps.methods.col_splice import _rec_name, split_records
            from apps.methods.weapon_models import replace_col_record
            recs = split_records(new)[0]
            rec = next((r for r in recs if _rec_name(r) == model.lower()), recs[0] if len(recs) == 1 else None)
            if rec is None:
                QMessageBox.warning(self, self.App_name, f"{os.path.basename(path)}: no {model} model inside")
                return
            new = replace_col_record(src[0], model, rec)
        if QMessageBox.question(self, self.App_name, f"Replace {src[2]} with {os.path.basename(path)}?") \
                != QMessageBox.StandardButton.Yes:
            return
        if not src[1](new):
            QMessageBox.warning(self, self.App_name, f"Could not write {src[2]}")
            return
        self._wm_cols = None
        self._wm_select(self._wm_list.currentRow())
        self._set_status(f"Replaced {name}.{kind} in {src[2]}")

    def _wm_export_pngs(self): #vers 1
        """Every texture of the selected weapon's TXD as PNGs in a folder."""
        from apps.methods.weapon_models import export_textures_png
        src = self._wm_source('txd')
        if src is None:
            return
        folder = QFileDialog.getExistingDirectory(self, f"Export textures of {src[2]} to")
        if folder:
            n = export_textures_png(src[0], folder)
            self._set_status(f"{n} textures saved to {folder}")

    def _wm_import_png(self): #vers 1
        """Replace the selected texture with a PNG (TXD re-written where it lives)."""
        from apps.methods.weapon_models import replace_texture
        src = self._wm_source('txd')
        item = self._wm_tex.currentItem()
        if src is None or item is None:
            QMessageBox.information(self, self.App_name, "Pick a weapon and one of its textures first")
            return
        tex = item.data(Qt.ItemDataRole.UserRole)
        path, _ = QFileDialog.getOpenFileName(self, f"PNG for {tex}", tex + '.png', "Images (*.png)")
        if not path:
            return
        data = replace_texture(src[0], tex, path)
        if QMessageBox.question(self, self.App_name, f"Replace {tex} in {src[2]}?") != QMessageBox.StandardButton.Yes:
            return
        if not src[1](data):
            QMessageBox.warning(self, self.App_name, f"Could not write {src[2]}")
            return
        self._wm_select(self._wm_list.currentRow())
        self._set_status(f"{tex} replaced in {src[2]}")
