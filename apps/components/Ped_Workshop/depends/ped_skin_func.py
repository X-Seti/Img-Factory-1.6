#this belongs in apps/components/Ped_Workshop/depends/ped_skin_func.py - Version: 1
# X-Seti - October 23 2026 - IMG Factory 1.6 - Ped Workshop skinning window

"""
Skinning window: Weights tab heat-maps the selected bone's skin weights and
paints them in the viewport (saved in place into the DFF / gta3.img);
Texture / UV tab shows a ped texture with its UV layout, exports it as PNG
and sends the texture to DP5 Workshop to paint.
"""

##class WeightBrush: -
# __init__
# hover
# press
# release
# ring
# tick
#
##class PedSkinMixin: -
# _skin_close
# _skin_paint
# _skin_refresh
# _skin_save
# _skin_set_weight
# _skin_show_weights
# _skin_undo
# _skin_uv_draw
# _skin_uv_export
# _skin_uv_to_dp5
# _skin_window

import os

import numpy as np

from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor, QImage, QPainter, QPen, QPixmap
from PyQt6.QtWidgets import (QCheckBox, QComboBox, QDialog, QDoubleSpinBox, QFileDialog, QFormLayout,
                             QHBoxLayout, QLabel, QMessageBox, QPushButton, QRadioButton, QScrollArea,
                             QSlider, QTabWidget, QVBoxLayout, QWidget)


class WeightBrush: #vers 1
    """Viewport brush: left drag paints the bone weight, Shift removes."""

    def __init__(self, ws): #vers 1
        self.ws, self.pos, self.down, self.remove = ws, None, False, False

    def ring(self): #vers 1
        return None                                         # heat colours show the result

    def hover(self, x, y): #vers 1
        self.pos = (x, y)

    def press(self, x, y, mods): #vers 1
        self.pos, self.down = (x, y), True
        self.remove = bool(mods & Qt.KeyboardModifier.ShiftModifier) or self.ws._skin_remove.isChecked()
        self.ws._skin_undo_stack.append({gi: (sk['indices'].copy(), sk['weights'].copy())
                                         for gi, sk in self.ws._animator.sk.skins.items()})
        self.tick()

    def tick(self): #vers 1
        if self.down and self.pos is not None:
            self.ws._skin_paint(*self.pos, self.remove)

    def release(self): #vers 1
        self.down = False


class PedSkinMixin: #vers 1
    """Skinning window for PedWorkshop."""

    def _skin_window(self): #vers 1
        """Open (or raise) the skinning window: Weights and Texture / UV tabs."""
        dlg = getattr(self, '_skin_dlg', None)
        if dlg is not None:
            self._skin_refresh()
            dlg.show()
            dlg.raise_()
            return
        self._skin_undo_stack, self._skin_dirty = [], False
        dlg = QDialog(self)
        dlg.setWindowTitle("Skinning")
        dlg.resize(460, 560)
        dlg.finished.connect(lambda _r: self._skin_close())
        tabs = QTabWidget(dlg)
        QVBoxLayout(dlg).addWidget(tabs)
        page = QWidget()                                    # Weights
        form = QFormLayout(page)
        self._skin_bone_lbl = QLabel("Pick a bone in the Bones list or the viewport")
        self._skin_view_chk = QCheckBox("Show weights (heat map)")
        self._skin_view_chk.toggled.connect(self._skin_show_weights)
        self._skin_paint_chk = QCheckBox("Paint weights (left drag, Shift removes)")
        self._skin_paint_chk.toggled.connect(lambda on: (self._skin_view_chk.setChecked(True) if on else None,
                                                         self.preview_widget.set_brush_tool(
                                                             WeightBrush(self) if on else None)))
        add, self._skin_remove = QRadioButton("Add"), QRadioButton("Remove")
        add.setChecked(True)
        mode = QHBoxLayout()
        mode.addWidget(add)
        mode.addWidget(self._skin_remove)
        mode.addStretch(1)
        self._skin_radius = QDoubleSpinBox()
        self._skin_radius.setRange(0.01, 0.5)
        self._skin_radius.setSingleStep(0.01)
        self._skin_radius.setValue(0.06)
        self._skin_radius.setSuffix(" m")
        self._skin_strength = QSlider(Qt.Orientation.Horizontal)
        self._skin_strength.setRange(1, 100)
        self._skin_strength.setValue(15)
        self._skin_strength.setToolTip("Weight change per brush tick (%)")
        btns = QHBoxLayout()
        for label, slot, tip in (("Undo", self._skin_undo, "Undo the last stroke"),
                                 ("Save weights", self._skin_save, "Write the weights into the DFF (backup first)")):
            b = QPushButton(label)
            b.setToolTip(tip)
            b.clicked.connect(lambda _c=False, s=slot: s())
            btns.addWidget(b)
        legend = QLabel("0 blue - green - yellow - red 1")
        form.addRow("Bone:", self._skin_bone_lbl)
        form.addRow(self._skin_view_chk)
        form.addRow(self._skin_paint_chk)
        form.addRow("Brush:", mode)
        form.addRow("Radius:", self._skin_radius)
        form.addRow("Strength:", self._skin_strength)
        form.addRow("Weights:", legend)
        form.addRow(btns)
        tabs.addTab(page, "Weights")
        page = QWidget()                                    # Texture / UV
        lay = QVBoxLayout(page)
        row = QHBoxLayout()
        self._skin_tex_combo = QComboBox()
        self._skin_tex_combo.currentIndexChanged.connect(lambda _i: self._skin_uv_draw())
        self._skin_tex_chk = QCheckBox("Texture")
        self._skin_tex_chk.setChecked(True)
        self._skin_tex_chk.toggled.connect(lambda _on: self._skin_uv_draw())
        row.addWidget(self._skin_tex_combo, 1)
        row.addWidget(self._skin_tex_chk)
        lay.addLayout(row)
        self._skin_uv_lbl = QLabel()
        self._skin_uv_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        scroll = QScrollArea()
        scroll.setWidget(self._skin_uv_lbl)
        scroll.setWidgetResizable(True)
        lay.addWidget(scroll, 1)
        row = QHBoxLayout()
        for label, slot, tip in (("Export UV PNG", self._skin_uv_export, "Texture with the UV layout, to paint outside"),
                                 ("Edit in DP5", self._skin_uv_to_dp5, "Paint this texture in DP5 Workshop (Send back returns it)")):
            b = QPushButton(label)
            b.setToolTip(tip)
            b.clicked.connect(lambda _c=False, s=slot: s())
            row.addWidget(b)
        lay.addLayout(row)
        tabs.addTab(page, "Texture / UV")
        self._bone_list.currentRowChanged.connect(lambda _r: self._skin_show_weights(self._skin_view_chk.isChecked()))
        self._skin_dlg = dlg
        self._skin_refresh()
        dlg.show()

    def _skin_refresh(self): #vers 1
        """New ped: texture list, UV view and weight view follow it."""
        dlg = getattr(self, '_skin_dlg', None)
        if dlg is None:
            return
        self._skin_undo_stack, self._skin_dirty = [], False
        self._skin_tex_combo.blockSignals(True)
        self._skin_tex_combo.clear()
        for t in getattr(self, '_mod_textures', None) or []:
            self._skin_tex_combo.addItem(t.get('name', '?'), t)
        self._skin_tex_combo.blockSignals(False)
        self._skin_show_weights(self._skin_view_chk.isChecked())
        self._skin_uv_draw()

    def _skin_close(self): #vers 1
        """Window closed: weight view and brush off."""
        self._skin_paint_chk.setChecked(False)
        self._skin_view_chk.setChecked(False)

    def _skin_show_weights(self, on: bool): #vers 1
        """Heat-map the selected bone's weights (or normal view)."""
        an = self._animator
        fi = self._bone_list.currentRow()
        if an is None or not an.sk.skins:
            self._skin_bone_lbl.setText("No skinned ped open (III peds are segmented)")
            if an is not None:
                an.weight_frame = -1
            return
        name = an.sk.frames[fi]['name'].strip() if 0 <= fi < len(an.sk.frames) else ''
        has_bone = fi in an.sk.bone_of_frame()
        self._skin_bone_lbl.setText(name if has_bone else f"{name or 'none'} (not a skin bone)")
        an.weight_frame = fi if on and has_bone else -1
        self._anim_render()

    def _skin_paint(self, x: int, y: int, remove: bool): #vers 1
        """One brush tick at a viewport pixel."""
        an, vp = self._animator, self.preview_widget
        fi = an.weight_frame if an is not None else -1
        ray = vp._pick_ray(x, y) if fi >= 0 else None
        if ray is None:
            return
        o, d = np.asarray(ray[0], float), np.asarray(ray[1], float)
        d /= np.linalg.norm(d) or 1.0
        r, step = self._skin_radius.value(), self._skin_strength.value() / 100.0
        b = an.sk.bone_of_frame()[fi]
        parent_b = an.sk.bone_of_frame().get(an.sk.frames[fi]['parent'], b)
        hits = []
        for gi, (P, _n) in an.mesh_points(an.pose()).items():
            if gi not in an.sk.skins:
                continue
            rel = P - o
            t = rel @ d
            dist = np.linalg.norm(rel - np.outer(t, d), axis=1)
            for vi in np.nonzero(dist < r)[0]:
                hits.append((t[vi], gi, vi, dist[vi]))
        if not hits:
            return
        front = min(h[0] for h in hits) + r * 1.5            # nearest surface only
        for t, gi, vi, dist in hits:
            if t <= front:
                sk = an.sk.skins[gi]
                cur = float(sk['weights'][vi][sk['indices'][vi] == b].sum())
                delta = step * (1.0 - dist / r)
                self._skin_set_weight(sk, vi, b, cur - delta if remove else cur + delta, parent_b)
        self._skin_dirty = True
        self._anim_render()

    @staticmethod
    def _skin_set_weight(sk: dict, vi: int, b: int, new: float, parent_b: int): #vers 1
        """Set bone b's weight on a vertex; other influences rescaled to sum 1."""
        idx, w = sk['indices'][vi], sk['weights'][vi]
        new = max(0.0, min(1.0, new))
        slots = np.nonzero(idx == b)[0]
        if len(slots):
            k = slots[0]
            w[slots[1:]] = 0.0
        elif new > 0.0:
            k = int(np.argmin(w))                           # weakest slot takes the new bone
            idx[k], w[k] = b, 0.0
        else:
            return
        rest = w.sum() - w[k]
        if rest <= 1e-6 and new < 1.0:                      # sole influence: parent takes the rest
            j = next((s for s in range(4) if s != k and idx[s] == parent_b), None)
            if j is None:
                j = next(s for s in range(4) if s != k)
                idx[j] = parent_b
            w[j], rest = 1.0, 1.0
        w[k] = new
        for j in range(4):
            if j != k and rest > 1e-6:
                w[j] *= (1.0 - new) / rest

    def _skin_undo(self): #vers 1
        """Undo the last weight stroke."""
        if not self._skin_undo_stack or self._animator is None:
            return
        for gi, (idx, w) in self._skin_undo_stack.pop().items():
            self._animator.sk.skins[gi]['indices'][:] = idx
            self._animator.sk.skins[gi]['weights'][:] = w
        self._anim_render()

    def _skin_save(self): #vers 1
        """Weights into the DFF file; from gta3.img: also into the IMG."""
        from apps.methods.dff_skin import write_skin_weights
        from apps.methods.file_backup import safe_write_bytes
        src = getattr(self, '_ped_dff_src', None)
        if self._animator is None or src is None or not self._animator.sk.skins:
            self._set_status("Skinning: no skinned ped to save")
            return
        path, raw = src
        data = write_skin_weights(raw, self._animator.sk.skins)
        try:
            safe_write_bytes(path, data, f"Skin weights: {os.path.basename(path)}")
            arc = getattr(self, '_ped_img', None)
            name = os.path.basename(path).lower()
            entry = next((e for e in arc.entries if e.name.lower() == name), None) if arc else None
            in_img = entry is not None and os.path.basename(os.path.dirname(path)).startswith('ped_')
            if in_img:                                      # opened from gta3.img (temp copy)
                if not arc.add_entry(entry.name, data):
                    raise IOError(f"Could not write {entry.name} into {arc.file_path}")
        except Exception as ex:
            QMessageBox.warning(self, "Save weights", str(ex))
            return
        self._ped_dff_src = (path, data)
        self._skin_dirty = False
        self._set_status(f"Skin weights saved: {os.path.basename(path)}"
                         + (" and gta3.img" if in_img else ""))

    def _skin_uv_draw(self): #vers 1
        """Texture (or black) with the UV wireframe of the meshes using it."""
        tex = self._skin_tex_combo.currentData()
        if tex is None:
            self._skin_uv_lbl.setText("No textures loaded")
            return
        w, h = max(1, tex.get('width', 0)), max(1, tex.get('height', 0))
        side = 512 if max(w, h) < 512 else max(w, h)
        img = QImage(side, side * h // w, QImage.Format.Format_ARGB32)
        img.fill(QColor(0, 0, 0))
        p = QPainter(img)
        rgba = tex.get('rgba_data')
        if self._skin_tex_chk.isChecked() and rgba and len(rgba) >= w * h * 4:
            src = QImage(bytes(rgba[:w * h * 4]), w, h, w * 4, QImage.Format.Format_RGBA8888)
            p.drawImage(img.rect(), src)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        p.setPen(QPen(QColor(0, 230, 255), 1))
        iw, ih = img.width(), img.height()
        tname = tex.get('name', '').lower()
        model = getattr(self, '_current_dff_model', None)
        for g in (model.geometries if model is not None else []):
            mats = [i for i, m in enumerate(g.materials) if (getattr(m, 'texture_name', '') or '').lower() == tname]
            if not mats or not g.uv_layers:
                continue
            uv = g.uv_layers[0]
            for t in g.triangles:
                if t.material_id in mats:
                    pts = [uv[i] for i in (t.v1, t.v2, t.v3)]
                    for a, c in ((0, 1), (1, 2), (2, 0)):
                        p.drawLine(int(pts[a].u * iw), int(pts[a].v * ih), int(pts[c].u * iw), int(pts[c].v * ih))
        p.end()
        self._skin_uv_img = img
        self._skin_uv_lbl.setPixmap(QPixmap.fromImage(img))

    def _skin_uv_export(self): #vers 1
        """Save the texture + UV layout as PNG."""
        img = getattr(self, '_skin_uv_img', None)
        if img is None:
            return
        name = self._skin_tex_combo.currentText() or 'texture'
        path, _ = QFileDialog.getSaveFileName(self, "Export UV layout", f"{name}_uv.png", "PNG images (*.png)")
        if path and img.save(path, 'PNG'):
            self._set_status(f"UV layout saved: {path}")

    def _skin_uv_to_dp5(self): #vers 1
        """Send this texture to DP5; Send back refreshes the UV view."""
        from apps.methods.dp5_link import send_textures_to_dp5
        tex = self._skin_tex_combo.currentData()
        if tex is None:
            return

        def back(t, rgba, w, h):
            self._texture_back_from_dp5(t, rgba, w, h)
            self._skin_uv_draw()
        if send_textures_to_dp5(self, [tex], self._titlebar_title(), back):
            self._set_status(f"{tex.get('name')} sent to DP5 Workshop")
