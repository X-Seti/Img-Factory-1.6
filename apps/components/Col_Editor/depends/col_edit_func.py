#this belongs in apps/components/Col_Editor/depends/col_edit_func.py - Version: 1
# X-Seti - Sept 30 2026 - IMG Factory 1.6 - COL Workshop edit tools

"""
COL Workshop edit tools - viewport selection edits: detach, extract, delete,
weld, fill hole, box/sphere/mesh conversion, scale, centre, merge files.
Geometry maths lives in apps/methods/col_mesh_ops.py.
"""

##class COLEditMixin: -
# _ask_copy_or_move
# _edit_box_to_mesh
# _edit_centre_origin
# _edit_delete_faces
# _edit_detach
# _edit_faces_to_box
# _edit_faces_to_sphere
# _edit_fill_hole
# _edit_gamepad_saved
# _edit_scale_dialog
# _edit_selection
# _edit_selection_to_file
# _edit_selection_to_model
# _edit_sphere_to_mesh
# _edit_toggle_gamepad
# _edit_toggle_vertex_mode
# _edit_weld
# _merge_col_files
# _mesh_edited
# _pick_items

import os

from PyQt6.QtWidgets import (QCheckBox, QDialog, QDialogButtonBox, QDoubleSpinBox,
                             QFileDialog, QFormLayout, QInputDialog, QMessageBox)

from apps.methods import col_mesh_ops as ops


class COLEditMixin: #vers 1
    """Viewport edit tools for COLWorkshop; each edit is undoable."""

    def _mesh_edited(self, model, msg, reselect=True): #vers 1
        """Refresh lists and viewport after an edit; mark file unsaved."""
        idx = self.current_col_file.models.index(model)
        self._populate_collision_list()
        self._populate_compact_col_list()
        if reselect:
            self._select_model_by_row(idx)
        else:                                     # keep viewport selection, re-mark list row
            for lw in (self.col_compact_list, self.collision_list):
                if idx < lw.rowCount():
                    lw.blockSignals(True)
                    lw.selectRow(idx)
                    lw.setCurrentCell(idx, 0)
                    lw.blockSignals(False)
            self.preview_widget.update()
        if hasattr(self, 'save_btn'):
            self.save_btn.setEnabled(True)
        self._set_status(f"{msg} - not saved yet")

    def _edit_selection(self, need_faces=True): #vers 1
        """(model, index, selected face ids) or None after telling the user why."""
        model = self._get_selected_model()
        if model is None:
            QMessageBox.warning(self, "No Selection", "Select a collision model first.")
            return None
        faces = set(self.preview_widget._selected_faces)
        if need_faces and not faces:
            QMessageBox.information(self, "No Faces Selected",
                                    "Click faces in the viewport first (Ctrl+click adds).")
            return None
        return model, self.current_col_file.models.index(model), faces

    def _ask_copy_or_move(self, title): #vers 1
        """'copy', 'move' or None."""
        box = QMessageBox(self)
        box.setWindowTitle(title)
        box.setText("Copy the selected faces, or move them out of this model?")
        copy_btn = box.addButton("Copy", QMessageBox.ButtonRole.AcceptRole)
        move_btn = box.addButton("Move", QMessageBox.ButtonRole.ActionRole)
        box.addButton(QMessageBox.StandardButton.Cancel)
        box.exec()
        return {copy_btn: 'copy', move_btn: 'move'}.get(box.clickedButton())

    def _pick_items(self, title, items): #vers 1
        """Choose one item index from a list (or all with -1); None when cancelled."""
        if len(items) == 1:
            return 0
        labels = ["All"] + items
        choice, ok = QInputDialog.getItem(self, title, "Convert:", labels, 0, False)
        if not ok:
            return None
        return labels.index(choice) - 1

    def _edit_toggle_vertex_mode(self, checked): #vers 1
        """Viewport vertex selection mode (for weld) on/off."""
        self.preview_widget.set_select_mode('vertex' if checked else 'face')
        self._set_status("Vertex select: click vertices, Ctrl+click adds" if checked
                         else "Face select")

    def _edit_detach(self): #vers 1
        """Split selected faces from their neighbours so they move freely."""
        sel = self._edit_selection()
        if not sel: return
        model, idx, faces = sel
        self._push_undo(idx, "Detach faces")
        n = ops.detach_faces(model, faces)
        self._mesh_edited(model, f"Detached {len(faces)} face(s), {n} vertex(es) split", reselect=False)

    def _edit_selection_to_model(self): #vers 1
        """Selected faces to a new model in this file (copy or move)."""
        sel = self._edit_selection()
        if not sel: return
        model, idx, faces = sel
        how = self._ask_copy_or_move("Selection to New Model")
        if not how: return
        name, ok = QInputDialog.getText(self, "New Model", "Model name:", text=f"{model.name[:17]}_part")
        if not ok or not name.strip(): return
        new = ops.extract_faces(model, faces, name.strip()[:22])
        if how == 'move':
            self._push_undo(idx, "Move faces to new model")
            ops.delete_faces(model, faces)
            ops.recalc_bounds(model)
        self.current_col_file.models.insert(idx + 1, new)
        self._mesh_edited(new, f"{how.title()} {len(faces)} face(s) to new model '{new.name}'")

    def _edit_selection_to_file(self): #vers 1
        """Selected faces saved as a new one-model COL file (copy or move)."""
        from apps.methods.col_workshop_parser import COLWriter
        sel = self._edit_selection()
        if not sel: return
        model, idx, faces = sel
        how = self._ask_copy_or_move("Save Selection as COL")
        if not how: return
        start = os.path.dirname(self.current_file_path or '')
        path, _ = QFileDialog.getSaveFileName(self, "Save Selection as COL",
                                              os.path.join(start, f"{model.name}_part.col"),
                                              "COL Files (*.col)")
        if not path: return
        new = ops.extract_faces(model, faces, os.path.splitext(os.path.basename(path))[0][:22])
        with open(path, 'wb') as fh:
            fh.write(COLWriter.write_model(new))
        if how == 'move':
            self._push_undo(idx, "Move faces to file")
            ops.delete_faces(model, faces)
            ops.recalc_bounds(model)
            self._mesh_edited(model, f"Moved {len(faces)} face(s) to {os.path.basename(path)}")
        else:
            self._set_status(f"Copied {len(faces)} face(s) to {os.path.basename(path)}")

    def _edit_delete_faces(self): #vers 1
        """Delete selected faces (unused vertices removed)."""
        sel = self._edit_selection()
        if not sel: return
        model, idx, faces = sel
        self._push_undo(idx, "Delete faces")
        ops.delete_faces(model, faces)
        ops.recalc_bounds(model)
        self._mesh_edited(model, f"Deleted {len(faces)} face(s)")

    def _edit_weld(self): #vers 1
        """Join the selected vertices (vertex mode) into one."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        verts = set(self.preview_widget._selected_verts)
        if len(verts) < 2:
            QMessageBox.information(self, "Weld", "Turn on Vertex Select Mode and pick 2 or more vertices.")
            return
        self._push_undo(idx, "Weld vertices")
        gone = ops.weld_vertices(model, verts)
        ops.recalc_bounds(model)
        self.preview_widget._selected_verts = set()
        self._mesh_edited(model, f"Welded {len(verts)} vertices, {gone} collapsed face(s) removed")

    def _edit_fill_hole(self): #vers 1
        """Fill holes surrounded by the selected faces."""
        sel = self._edit_selection()
        if not sel: return
        model, idx, faces = sel
        self._push_undo(idx, "Fill hole")
        added = ops.fill_holes(model, faces)
        if not added:
            self._undo_last_action()
            QMessageBox.information(self, "Fill Hole",
                                    "No closed hole found. Select all the faces around the hole.")
            return
        self._mesh_edited(model, f"Filled hole: {added} face(s) added")

    def _edit_box_to_mesh(self): #vers 1
        """Replace a box (or all) with triangles."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        if not model.boxes:
            QMessageBox.information(self, "Box to Mesh", f"'{model.name}' has no boxes."); return
        k = self._pick_items("Box to Mesh", [f"Box {i} (material {b.material_id})" for i, b in enumerate(model.boxes)])
        if k is None: return
        self._push_undo(idx, "Box to mesh")
        for i in (range(len(model.boxes) - 1, -1, -1) if k < 0 else [k]):
            ops.box_to_mesh(model, i)
        self._mesh_edited(model, "Box converted to mesh")

    def _edit_sphere_to_mesh(self): #vers 1
        """Replace a sphere (or all) with an 80-face mesh."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        if not model.spheres:
            QMessageBox.information(self, "Sphere to Mesh", f"'{model.name}' has no spheres."); return
        k = self._pick_items("Sphere to Mesh", [f"Sphere {i} (r {s.radius:.2f}, material {s.material_id})"
                                                for i, s in enumerate(model.spheres)])
        if k is None: return
        self._push_undo(idx, "Sphere to mesh")
        for i in (range(len(model.spheres) - 1, -1, -1) if k < 0 else [k]):
            ops.sphere_to_mesh(model, i)
        self._mesh_edited(model, "Sphere converted to mesh")

    def _edit_faces_to_box(self): #vers 1
        """Replace selected faces with one box around them."""
        sel = self._edit_selection()
        if not sel: return
        model, idx, faces = sel
        self._push_undo(idx, "Faces to box")
        ops.faces_to_box(model, faces)
        ops.recalc_bounds(model)
        self._mesh_edited(model, f"{len(faces)} face(s) replaced by a box")

    def _edit_faces_to_sphere(self): #vers 1
        """Replace selected faces with one enclosing sphere."""
        sel = self._edit_selection()
        if not sel: return
        model, idx, faces = sel
        self._push_undo(idx, "Faces to sphere")
        ops.faces_to_sphere(model, faces)
        ops.recalc_bounds(model)
        self._mesh_edited(model, f"{len(faces)} face(s) replaced by a sphere")

    def _edit_scale_dialog(self): #vers 1
        """Scale the selected faces, or the whole model, by X/Y/Z factors."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, faces = sel
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Scale {'selected faces' if faces else model.name}")
        form = QFormLayout(dlg)
        uni = QCheckBox("Uniform")
        uni.setChecked(True)
        spins = []
        for axis in "XYZ":
            sp = QDoubleSpinBox()
            sp.setRange(0.01, 100.0); sp.setDecimals(3); sp.setSingleStep(0.1); sp.setValue(1.0)
            form.addRow(f"{axis}:", sp)
            spins.append(sp)

        def _sync(v):  #vers 1
            if uni.isChecked():
                for other in spins[1:]:
                    other.setValue(v)
        spins[0].valueChanged.connect(_sync)
        uni.toggled.connect(lambda on: [o.setEnabled(not on) for o in spins[1:]])
        for o in spins[1:]:
            o.setEnabled(False)
        form.addRow(uni)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        sx, sy, sz = (sp.value() for sp in spins)
        self._push_undo(idx, "Scale")
        ops.scale(model, faces, sx, sy, sz, ops.selection_centre(model, faces))
        ops.recalc_bounds(model)
        self._mesh_edited(model, f"Scaled {'selection' if faces else model.name} by {sx:g}, {sy:g}, {sz:g}",
                          reselect=not faces)

    def _edit_centre_origin(self): #vers 1
        """Move the whole model so its bounds centre is at 0,0,0."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        ops.recalc_bounds(model)
        c = model.bounds.center
        self._push_undo(idx, "Centre to origin")
        ops.translate(model, set(), -c.x, -c.y, -c.z)
        ops.recalc_bounds(model)
        self._mesh_edited(model, f"Centred {model.name} (moved {-c.x:.2f}, {-c.y:.2f}, {-c.z:.2f})")

    def _edit_gamepad_saved(self): #vers 1
        """Saved controller on/off from col_workshop.json."""
        import json
        from apps.methods.img_factory_settings import get_user_config_dir
        try:
            return bool(json.loads((get_user_config_dir() / 'col_workshop.json').read_text()).get('gamepad_enabled'))
        except (OSError, ValueError):
            return False

    def _edit_toggle_gamepad(self, on): #vers 1
        """Start or stop the PS5 / game controller; remembered in col_workshop.json."""
        import json
        from apps.methods.img_factory_settings import get_user_config_dir
        vp = self.preview_widget
        pad = getattr(self, '_gamepad', None)
        if on and pad is None:
            from apps.methods.gamepad_input import GamepadPoller
            pad = GamepadPoller(self)
            pad.connected.connect(lambda n: self._set_status(
                f"Controller connected: {n}" if n else "Controller disconnected"))
            try:
                pad.start()
            except ImportError:
                QMessageBox.warning(self, "Game Controller",
                                    "Controller support needs pygame 2:\n\npip install pygame")
                self.gamepad_btn.setChecked(False)
                return
            self._gamepad = pad
            vp.set_gamepad(pad)
            self._set_status("Controller on: Cross select/grab, right stick orbit, L2/R2 zoom")
        elif not on and pad is not None:
            pad.stop()
            vp.set_gamepad(None)
            self._gamepad = None
        path = get_user_config_dir() / 'col_workshop.json'
        try:
            data = json.loads(path.read_text())
        except (OSError, ValueError):
            data = {}
        data['gamepad_enabled'] = bool(on and self._gamepad is not None)
        path.write_text(json.dumps(data, indent=2))

    def _merge_col_files(self): #vers 1
        """Pick COL files; all their models are added to the open file."""
        paths, _ = QFileDialog.getOpenFileNames(self, "Merge COL Files",
                                                os.path.dirname(self.current_file_path or ''),
                                                "COL Files (*.col)")
        if paths:
            self._add_models_from_files(paths)


__all__ = ['COLEditMixin']
