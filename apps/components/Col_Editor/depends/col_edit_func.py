#this belongs in apps/components/Col_Editor/depends/col_edit_func.py - Version: 9
# X-Seti - Sept 30 2026 - IMG Factory 1.6 - COL Workshop edit tools

"""
COL Workshop edit tools - viewport selection edits: detach, extract, delete,
weld, fill hole, box/sphere/mesh conversion, optimise, scale, centre, merge files,
vertex tools: select, position, create face, split, delete, mirror;
hide, lock, select by material, LOD copy, shadow copy, clear, bounds,
face groups, lighting, light view, VC to SA materials, region select modes,
duplicate check, batch conversion, CST/3DS/X/DFF import, CST export, attach to DFF,
COL from DFF mesh, surfaces from DFF textures.
Geometry maths lives in apps/methods/col_mesh_ops.py.
"""

##class COLEditMixin: -
# _ask_copy_or_move
# _edit_add_face
# _edit_attach_to_dff
# _edit_batch_convert
# _edit_box_to_mesh
# _edit_centre_origin
# _edit_clear_face_groups
# _edit_clear_parts
# _edit_col_from_dff
# _edit_copy_as_lod
# _edit_delete_faces
# _edit_delete_isolated
# _edit_delete_vertices
# _edit_detach
# _edit_duplicate_check
# _edit_export_cst
# _edit_face_groups
# _edit_faces_to_box
# _edit_faces_to_sphere
# _edit_fill_hole
# _edit_gamepad_saved
# _edit_hide_selected
# _edit_import_exchange
# _edit_light_view
# _edit_lighting
# _edit_mesh_from_shadow
# _edit_mirror
# _edit_optimise
# _edit_optimum_bounds
# _edit_region_circle
# _edit_region_window
# _edit_scale_dialog
# _edit_select_material
# _edit_selection
# _edit_selection_to_file
# _edit_selection_to_model
# _edit_show_face_groups
# _edit_show_vertices
# _edit_sphere_to_mesh
# _edit_split_faces
# _edit_surfaces_from_dff
# _edit_toggle_gamepad
# _edit_toggle_lock
# _edit_toggle_vertex_mode
# _edit_tools_menu
# _edit_unhide_all
# _edit_vc_to_sa
# _edit_vertex_position
# _edit_verts_all
# _edit_verts_invert
# _edit_verts_none
# _edit_verts_to_faces
# _edit_weld
# _merge_col_files
# _mesh_edited
# _pick_items
# _vertex_selection

import os

from PyQt6.QtWidgets import (QCheckBox, QDialog, QDialogButtonBox, QDoubleSpinBox,
                             QFileDialog, QFormLayout, QInputDialog, QMessageBox,
                             QRadioButton, QSpinBox)

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

    def _edit_selection(self, need_faces=True): #vers 2
        """(model, index, face ids) or None; vertex mode uses faces inside selection."""
        model = self._get_selected_model()
        if model is None:
            QMessageBox.warning(self, "No Selection", "Select a collision model first.")
            return None
        pw = self.preview_widget
        faces = (ops.faces_of_vertices(model, pw._selected_verts) if pw._select_mode == 'vertex'
                 else set(pw._selected_faces))
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
        """Viewport vertex selection mode on/off."""
        self.preview_widget.set_select_mode('vertex' if checked else 'face')
        self._set_status("Vertex select: click, Ctrl+click adds, drag box selects" if checked
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

    def _edit_scale_dialog(self): #vers 2
        """Scale the selected faces, or the whole model, by X/Y/Z factors."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        faces, vs = self.preview_widget._edit_sets()
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Scale {'selection' if faces or vs else model.name}")
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
        ops.scale(model, faces, sx, sy, sz, ops.selection_centre(model, faces, vs), vs)
        ops.recalc_bounds(model)
        self._mesh_edited(model, f"Scaled {'selection' if faces or vs else model.name} by {sx:g}, {sy:g}, {sz:g}",
                          reselect=not (faces or vs))

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

    def _vertex_selection(self, least=1): #vers 1
        """(model, index, vertex ids) in vertex mode, else None after a message."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return None
        vs = set(self.preview_widget._selected_verts)
        if self.preview_widget._select_mode != 'vertex' or len(vs) < least:
            QMessageBox.information(self, "Vertices",
                                    f"Turn on Vertex Select Mode and pick {least} or more vertices.")
            return None
        return sel[0], sel[1], vs

    def _edit_verts_all(self): #vers 1
        """Select every vertex of the current model."""
        pw = self.preview_widget
        if pw._model is None: return
        pw._selected_verts = set(range(len(pw._model.vertices)))
        pw.update()

    def _edit_verts_none(self): #vers 1
        """Clear the vertex selection."""
        self.preview_widget._selected_verts = set()
        self.preview_widget.update()

    def _edit_verts_invert(self): #vers 1
        """Invert the vertex selection."""
        pw = self.preview_widget
        if pw._model is None: return
        pw._selected_verts = set(range(len(pw._model.vertices))) - pw._selected_verts
        pw.update()

    def _edit_verts_to_faces(self): #vers 1
        """Switch to face mode selecting faces inside the vertex selection."""
        sel = self._vertex_selection()
        if not sel: return
        model, _, vs = sel
        faces = ops.faces_of_vertices(model, vs)
        self.vertex_mode_btn.setChecked(False)
        self.preview_widget._selected_faces = faces
        self.preview_widget.update()
        self._set_status(f"{len(faces)} face(s) selected")

    def _edit_vertex_position(self): #vers 1
        """Type X/Y/Z for one vertex, or move a selection (absolute centre or relative)."""
        sel = self._vertex_selection()
        if not sel: return
        model, idx, vs = sel
        cx, cy, cz = ops.selection_centre(model, vert_ids=vs)
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Position of {len(vs)} vertex(es)")
        form = QFormLayout(dlg)
        rel = QCheckBox("Relative (move by)")
        spins = []
        for axis, val in zip("XYZ", (cx, cy, cz)):
            sp = QDoubleSpinBox()
            sp.setRange(-100000.0, 100000.0); sp.setDecimals(4); sp.setSingleStep(0.1); sp.setValue(val)
            form.addRow(f"{axis}:", sp)
            spins.append(sp)
        rel.toggled.connect(lambda on: [sp.setValue(0.0 if on else v)
                                        for sp, v in zip(spins, (cx, cy, cz))])
        form.addRow(rel)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        x, y, z = (sp.value() for sp in spins)
        dx, dy, dz = (x, y, z) if rel.isChecked() else (x - cx, y - cy, z - cz)
        self._push_undo(idx, "Vertex position")
        ops.translate(model, set(), dx, dy, dz, vert_ids=vs)
        ops.recalc_bounds(model)
        self._mesh_edited(model, f"Moved {len(vs)} vertex(es) by {dx:.3f}, {dy:.3f}, {dz:.3f}", reselect=False)

    def _edit_add_face(self): #vers 1
        """New face from exactly three selected vertices."""
        sel = self._vertex_selection(3)
        if not sel: return
        model, idx, vs = sel
        if len(vs) != 3:
            QMessageBox.information(self, "Create Face", "Select exactly 3 vertices.")
            return
        self._push_undo(idx, "Create face")
        fi = ops.add_face(model, *sorted(vs))
        self._mesh_edited(model, f"Created face {fi}", reselect=False)

    def _edit_split_faces(self): #vers 1
        """Add a centre vertex to each selected face (face becomes three)."""
        sel = self._edit_selection()
        if not sel: return
        model, idx, faces = sel
        self._push_undo(idx, "Split faces")
        new = ops.split_faces(model, faces)
        self.preview_widget._selected_faces = set()
        self._mesh_edited(model, f"Split {len(faces)} face(s), {len(new)} vertex(es) added", reselect=False)

    def _edit_delete_vertices(self): #vers 1
        """Delete selected vertices and the faces using them."""
        sel = self._vertex_selection()
        if not sel: return
        model, idx, vs = sel
        self._push_undo(idx, "Delete vertices")
        gone = ops.delete_vertices(model, vs)
        ops.recalc_bounds(model)
        self.preview_widget._selected_verts = set()
        self._mesh_edited(model, f"Deleted {len(vs)} vertex(es), {gone} face(s) removed", reselect=False)

    def _edit_mirror(self): #vers 1
        """Mirror the selection (or whole model) on chosen axes."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        faces, vs = self.preview_widget._edit_sets()
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Mirror {'selection' if faces or vs else model.name}")
        form = QFormLayout(dlg)
        boxes = [QCheckBox(f"{a} axis") for a in "XYZ"]
        boxes[0].setChecked(True)
        for b in boxes:
            form.addRow(b)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        axes = ''.join(a for a, b in zip("XYZ", boxes) if b.isChecked())
        if not axes: return
        self._push_undo(idx, "Mirror")
        ops.mirror(model, faces, axes, ops.selection_centre(model, faces, vs), vs)
        ops.recalc_bounds(model)
        self._mesh_edited(model, f"Mirrored {'selection' if faces or vs else model.name} on {axes}",
                          reselect=not (faces or vs))

    def _edit_hide_selected(self): #vers 1
        """Hide selected faces (not drawn or picked)."""
        sel = self._edit_selection()
        if not sel: return
        pw = self.preview_widget
        pw._hidden_faces |= sel[2]
        pw._selected_faces = set()
        pw.update()
        self._set_status(f"{len(pw._hidden_faces)} face(s) hidden")

    def _edit_unhide_all(self): #vers 1
        """Show all hidden faces."""
        self.preview_widget._hidden_faces = set()
        self.preview_widget.update()
        self._set_status("All faces shown")

    def _edit_toggle_lock(self, checked): #vers 1
        """Selection lock: clicks keep the current selection (Space)."""
        self.preview_widget._sel_lock = bool(checked)
        self._set_status("Selection locked" if checked else "Selection unlocked")

    def _edit_select_material(self): #vers 1
        """Select every face using the selected faces' materials, or a picked one."""
        from apps.methods.col_materials import get_material_name, COLGame
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, _, faces = sel
        mats = {ops._mat(model.faces[i]) for i in faces if i < len(model.faces)}
        if not mats:
            game = COLGame.VC if getattr(model.version, 'value', 3) == 1 else COLGame.SA
            used = sorted({ops._mat(f) for f in model.faces})
            if not used: return
            labels = [f"{m} - {get_material_name(m, game)}" for m in used]
            pick, ok = QInputDialog.getItem(self, "Select by Material", "Material:", labels, 0, False)
            if not ok: return
            mats = {used[labels.index(pick)]}
        pw = self.preview_widget
        if pw._select_mode == 'vertex':
            self.vertex_mode_btn.setChecked(False)
        pw._selected_faces = ops.faces_by_material(model, mats) - pw._hidden_faces
        pw.update()
        self._set_status(f"{len(pw._selected_faces)} face(s) with material {', '.join(map(str, sorted(mats)))}")

    def _edit_copy_as_lod(self): #vers 1
        """Insert a LOD copy of the selected model after it."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        new = ops.copy_as_lod(model)
        self.current_col_file.models.insert(idx + 1, new)
        self._mesh_edited(new, f"Added LOD copy '{new.name}'")

    def _edit_mesh_from_shadow(self): #vers 1
        """Replace the collision mesh with a copy of the shadow mesh."""
        import copy
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        if not getattr(model, 'shadow_faces', None):
            QMessageBox.information(self, "No Shadow Mesh", f"{model.name} has no shadow mesh.")
            return
        if model.faces and QMessageBox.question(self, "Mesh from Shadow",
                f"Replace the {len(model.faces)} face collision mesh with the shadow mesh?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No) != QMessageBox.StandardButton.Yes:
            return
        self._push_undo(idx, "Mesh from shadow")
        model.vertices = copy.deepcopy(model.shadow_vertices)
        model.faces = copy.deepcopy(model.shadow_faces)
        ops.recalc_bounds(model)
        self._mesh_edited(model, f"Collision mesh copied from shadow: {len(model.faces)} face(s)")

    def _edit_clear_parts(self): #vers 1
        """Clear mesh, spheres, boxes or shadow mesh of the selected model."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Clear - {model.name}")
        form = QFormLayout(dlg)
        parts = [("mesh", f"Mesh ({len(model.faces)} faces)"), ("spheres", f"Spheres ({len(model.spheres)})"),
                 ("boxes", f"Boxes ({len(model.boxes)})"),
                 ("shadow", f"Shadow mesh ({len(getattr(model, 'shadow_faces', []) or [])} faces)")]
        boxes = {k: QCheckBox(t) for k, t in parts}
        for cb in boxes.values():
            form.addRow(cb)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        chosen = {k: cb.isChecked() for k, cb in boxes.items()}
        if not any(chosen.values()): return
        self._push_undo(idx, "Clear parts")
        ops.clear_parts(model, **chosen)
        ops.recalc_bounds(model)
        self._mesh_edited(model, "Cleared " + ", ".join(k for k, v in chosen.items() if v))

    def _edit_delete_isolated(self): #vers 1
        """Remove vertices not used by any face."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        self._push_undo(idx, "Delete isolated vertices")
        n = ops.delete_isolated_vertices(model)
        self._mesh_edited(model, f"Removed {n} isolated vertex(es)", reselect=False)

    def _edit_optimum_bounds(self): #vers 1
        """Tightest bounding box and sphere for the selected model."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        old = float(model.bounds.radius)
        self._push_undo(idx, "Optimum bounds")
        ops.optimum_bounds(model)
        self._mesh_edited(model, f"Bounds optimised: radius {old:.3f} to {float(model.bounds.radius):.3f}",
                          reselect=False)

    def _edit_face_groups(self): #vers 1
        """Generate face groups (faces re-sorted spatially) for COL2/3 models."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        if getattr(model.version, 'value', 1) < 2:
            QMessageBox.information(self, "Face Groups", "Face groups need COL2 or COL3.")
            return
        per, ok = QInputDialog.getInt(self, "Generate Face Groups", "Max faces per group:", 50, 8, 1000)
        if not ok: return
        self._push_undo(idx, "Generate face groups")
        n = ops.generate_face_groups(model, per)
        self.preview_widget._selected_faces = set()
        self._mesh_edited(model, f"{n} face group(s) for {len(model.faces)} faces", reselect=False)

    def _edit_clear_face_groups(self): #vers 1
        """Remove face groups from the selected model."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        if not model.face_groups: return
        self._push_undo(idx, "Clear face groups")
        model.face_groups = []
        self._mesh_edited(model, "Face groups cleared", reselect=False)

    def _edit_show_face_groups(self, checked): #vers 1
        """Draw face group boxes in the viewport."""
        self.preview_widget._show_face_groups = bool(checked)
        self.preview_widget.update()

    def _edit_light_view(self, checked): #vers 1
        """Colour faces by day light instead of material."""
        self.preview_widget._light_view = bool(checked)
        self.preview_widget.update()

    def _edit_lighting(self): #vers 1
        """Generate face lighting from a directional light (CE II style)."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Generate Lighting - {model.name}")
        form = QFormLayout(dlg)
        fields = {}
        for key, label, lo, hi, val in [("intensity", "Light intensity %:", 0, 100, 100),
                                        ("azimuth", "Azimuth:", 0, 359, 45),
                                        ("altitude", "Altitude:", -90, 90, 45),
                                        ("ambient", "Ambient light %:", 0, 100, 30),
                                        ("night", "Night level %:", 0, 100, 50)]:
            sp = QSpinBox(); sp.setRange(lo, hi); sp.setValue(val)
            form.addRow(label, sp); fields[key] = sp
        direc = QCheckBox("Use directional light source"); direc.setChecked(True)
        form.addRow(direc)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        v = {k: sp.value() for k, sp in fields.items()}
        self._push_undo(idx, "Generate lighting")
        ops.generate_lighting(model, v['intensity'] / 100, v['azimuth'], v['altitude'],
                              v['ambient'] / 100, direc.isChecked(), v['night'] / 100)
        self._mesh_edited(model, f"Lighting generated for {len(model.faces)} face(s)", reselect=False)

    def _edit_vc_to_sa(self): #vers 1
        """Convert GTA3/VC surface materials to SA equivalents."""
        from apps.methods.col_materials import COLGame
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        self._push_undo(idx, "VC to SA materials")
        n = ops.convert_materials(model, COLGame.VC, COLGame.SA)
        self._mesh_edited(model, f"{n} surface(s) converted VC to SA", reselect=False)

    def _edit_region_circle(self, checked): #vers 1
        """Region select shape: circle when on, rectangle when off."""
        self.preview_widget._region_circle = bool(checked)
        self._set_status("Region select: circle" if checked else "Region select: rectangle")

    def _edit_region_window(self, checked): #vers 1
        """Face region select: window (all corners inside) when on, crossing when off."""
        self.preview_widget._region_crossing = not checked
        self._set_status("Face region: window" if checked else "Face region: crossing")

    def _edit_duplicate_check(self): #vers 1
        """List models sharing a name; optionally remove later copies."""
        models = getattr(self.current_col_file, 'models', None)
        if not models: return
        seen, dups = {}, []
        for i, m in enumerate(models):
            key = m.name.lower()
            if key in seen:
                dups.append(i)
            else:
                seen[key] = i
        if not dups:
            QMessageBox.information(self, "Duplicate Check", "No duplicate model names.")
            return
        names = sorted({models[i].name for i in dups})
        shown = "\n".join(names[:30]) + ("\n..." if len(names) > 30 else "")
        if QMessageBox.question(self, "Duplicate Check",
                f"{len(dups)} duplicate model(s):\n{shown}\n\nRemove the later copies (first kept)?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No) != QMessageBox.StandardButton.Yes:
            return
        for i in reversed(dups):
            del models[i]
        self._populate_collision_list()
        self._populate_compact_col_list()
        self._select_model_by_row(0)
        if hasattr(self, 'save_btn'):
            self.save_btn.setEnabled(True)
        self._set_status(f"Removed {len(dups)} duplicate model(s) - not saved yet")

    def _edit_batch_convert(self): #vers 1
        """Convert many COL files: version, VC to SA, lighting, bounds, optimise."""
        from PyQt6.QtWidgets import QComboBox
        from apps.methods.col_workshop_loader import COLFile
        from apps.methods.col_workshop_parser import COLWriter
        from apps.methods.col_workshop_classes import COLVersion
        from apps.methods.col_materials import COLGame
        files, _ = QFileDialog.getOpenFileNames(self, "Batch Conversion - COL files",
                                                os.path.dirname(self.current_file_path or ''),
                                                "COL Files (*.col)")
        if not files: return
        out_dir = QFileDialog.getExistingDirectory(self, "Batch Conversion - output folder",
                                                   os.path.dirname(files[0]))
        if not out_dir: return
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Batch Conversion - {len(files)} file(s)")
        form = QFormLayout(dlg)
        ver = QComboBox()
        for label, v in (("Keep", None), ("COL1", COLVersion.COL_1), ("COL2", COLVersion.COL_2),
                         ("COL3", COLVersion.COL_3)):
            ver.addItem(label, v)
        form.addRow("Output version:", ver)
        opts = {k: QCheckBox(t) for k, t in (("light", "Generate lighting (default settings)"),
                                             ("vcsa", "Material conversion VC to SA"),
                                             ("bounds", "Minimize bounding volumes"),
                                             ("optimise", "Optimize (clean, isolated vertices, face groups)"))}
        for cb in opts.values():
            form.addRow(cb)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        target = ver.currentData()
        on = {k: cb.isChecked() for k, cb in opts.items()}
        done, failed = 0, []
        for path in files:
            cf = COLFile()
            if not cf.load(path) or not cf.models:
                failed.append(os.path.basename(path)); continue
            for m in cf.models:
                old_game = COLGame.VC if getattr(m.version, 'value', 1) == 1 else COLGame.SA
                if on['vcsa'] and old_game == COLGame.VC:
                    ops.convert_materials(m, COLGame.VC, COLGame.SA)
                if target is not None:
                    m.version = target
                if on['optimise']:
                    ops.clean_mesh(m)
                    ops.delete_isolated_vertices(m)
                    if getattr(m.version, 'value', 1) >= 2:
                        ops.generate_face_groups(m)
                if on['light']:
                    ops.generate_lighting(m)
                ops.recalc_bounds(m)
                if on['bounds']:
                    ops.optimum_bounds(m)
            with open(os.path.join(out_dir, os.path.basename(path)), 'wb') as fh:
                fh.write(COLWriter.write_file(cf.models))
            done += 1
        msg = f"Batch conversion: {done} file(s) written to {out_dir}"
        if failed:
            msg += f"; failed: {', '.join(failed)}"
        self._set_status(msg)
        QMessageBox.information(self, "Batch Conversion", msg)

    def _edit_export_cst(self): #vers 1
        """Save the selected model as a collision script (CST2)."""
        from apps.methods.col_exchange import write_cst2
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model = sel[0]
        start = os.path.dirname(self.current_file_path or '')
        path, _ = QFileDialog.getSaveFileName(self, "Export Collision Script",
                                              os.path.join(start, f"{model.name}.cst"),
                                              "Collision Scripts (*.cst)")
        if not path: return
        with open(path, 'w', encoding='utf-8') as fh:
            fh.write(write_cst2(model))
        self._set_status(f"Exported {model.name} to {os.path.basename(path)}")

    def _edit_import_exchange(self): #vers 1
        """New model from a CST, 3DS, .X file or the collision inside a DFF."""
        from apps.methods import col_exchange as ex
        from apps.methods.col_workshop_classes import COLVersion
        from apps.methods.col_workshop_parser import COLParser
        if not self.current_col_file:
            QMessageBox.warning(self, "No File", "Load a COL file first.")
            return
        path, _ = QFileDialog.getOpenFileName(
            self, "Import Collision", os.path.dirname(self.current_file_path or ''),
            "All Supported (*.cst *.3ds *.x *.dff);;Collision Scripts (*.cst);;"
            "3DS Models (*.3ds);;DirectX Models (*.x);;GTA Models (*.dff)")
        if not path: return
        name = os.path.splitext(os.path.basename(path))[0][:22]
        cur = self._get_selected_model()
        ver = cur.version if cur is not None else COLVersion.COL_3
        ext = os.path.splitext(path)[1].lower()
        if ext == '.cst':
            with open(path, encoding='utf-8', errors='replace') as fh:
                model = ex.model_from_parts(ex.read_cst(fh.read()), name, ver)
        elif ext in ('.3ds', '.x'):
            if ext == '.3ds':
                with open(path, 'rb') as fh:
                    vs, fs = ex.read_3ds_mesh(fh.read())
            else:
                with open(path, encoding='utf-8', errors='replace') as fh:
                    vs, fs = ex.read_x_mesh(fh.read())
            model = ex.model_from_parts({'Vertex': [list(v) for v in vs], 'Face': [list(f) for f in fs]}, name, ver)
        else:
            with open(path, 'rb') as fh:
                raw = ex.dff_collision(fh.read())
            if not raw:
                QMessageBox.information(self, "Import", f"{os.path.basename(path)} has no embedded collision.")
                return
            model, _ = COLParser().parse_model(raw, 0)
            if model is None:
                QMessageBox.warning(self, "Import", "Embedded collision could not be read.")
                return
        self.current_col_file.models.append(model)
        self._mesh_edited(model, f"Imported '{model.name}': {len(model.vertices)}V {len(model.faces)}F "
                                 f"{len(model.spheres)}S {len(model.boxes)}B")

    def _edit_attach_to_dff(self): #vers 1
        """Embed the selected model in a DFF (SA clump Collision Model section)."""
        from apps.methods.col_exchange import attach_col_to_dff
        from apps.methods.col_workshop_parser import COLWriter
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model = sel[0]
        start = os.path.dirname(self.current_file_path or '')
        src, _ = QFileDialog.getOpenFileName(self, "Attach to DFF", start, "GTA Models (*.dff)")
        if not src: return
        dst, _ = QFileDialog.getSaveFileName(self, "Save DFF with collision", src, "GTA Models (*.dff)")
        if not dst: return
        with open(src, 'rb') as fh:
            dff = fh.read()
        try:
            out = attach_col_to_dff(dff, COLWriter.write_model(model))
        except ValueError as e:
            QMessageBox.warning(self, "Attach to DFF", str(e))
            return
        with open(dst, 'wb') as fh:
            fh.write(out)
        self._set_status(f"Attached {model.name} to {os.path.basename(dst)}")

    def _edit_col_from_dff(self): #vers 1
        """New COL model per DFF from its render mesh; surfaces from texture names."""
        from apps.methods import col_exchange as ex
        from apps.methods.dff_parser import load_dff
        from apps.methods.col_materials import material_from_texture, COLGame
        from apps.methods.col_workshop_classes import COLVersion
        from apps.methods.col_workshop_loader import COLFile
        paths, _ = QFileDialog.getOpenFileNames(self, "COL from DFF",
                                                os.path.dirname(self.current_file_path or ''),
                                                "GTA Models (*.dff)")
        if not paths: return
        dlg = QDialog(self)
        dlg.setWindowTitle(f"COL from DFF - {len(paths)} file(s)")
        form = QFormLayout(dlg)
        skip = QCheckBox("Skip LOD and damage parts (_vlo, _lod, _dam)"); skip.setChecked(True)
        surf = QCheckBox("Surfaces from texture names"); surf.setChecked(True)
        opt = QCheckBox("Clean mesh (weld, drop degenerate faces)"); opt.setChecked(True)
        for cb in (skip, surf, opt):
            form.addRow(cb)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        if not self.current_col_file:
            self.current_col_file = COLFile()
            self.current_col_file.models = []
        cur = self._get_selected_model()
        ver = cur.version if cur is not None else COLVersion.COL_3
        game = COLGame.VC if ver == COLVersion.COL_1 else COLGame.SA
        added, unknown, last = 0, set(), None
        for path in paths:
            dff = load_dff(path)
            if dff is None: continue
            verts, tris = ex.dff_triangles(dff, skip.isChecked())
            if not tris: continue
            rows = []
            for a, b, c, tex in tris:
                m = material_from_texture(tex, game) if surf.isChecked() else None
                if m is None and surf.isChecked() and tex:
                    unknown.add(tex)
                rows.append([a, b, c, m or 0])
            name = os.path.splitext(os.path.basename(path))[0]
            model = ex.model_from_parts({'Vertex': [list(v) for v in verts], 'Face': rows}, name, ver)
            if opt.isChecked():
                ops.clean_mesh(model)
                ops.recalc_bounds(model)
            self.current_col_file.models.append(model)
            added += 1; last = model
        if not last:
            QMessageBox.information(self, "COL from DFF", "No mesh found in the chosen DFF file(s).")
            return
        msg = f"Created {added} COL model(s) from DFF"
        if unknown:
            msg += f"; textures with no surface match (Default): {', '.join(sorted(unknown)[:12])}"
        self._mesh_edited(last, msg)

    def _edit_surfaces_from_dff(self): #vers 1
        """Set the selected model's face surfaces from a DFF's texture names."""
        from apps.methods import col_exchange as ex
        from apps.methods.dff_parser import load_dff
        from apps.methods.col_materials import COLGame
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        if not model.faces:
            QMessageBox.information(self, "Surfaces from DFF", f"{model.name} has no mesh faces.")
            return
        path, _ = QFileDialog.getOpenFileName(self, "Surfaces from DFF texture map",
                                              os.path.dirname(self.current_file_path or ''),
                                              "GTA Models (*.dff)")
        if not path: return
        dff = load_dff(path)
        if dff is None:
            QMessageBox.warning(self, "Surfaces from DFF", "DFF could not be read.")
            return
        verts, tris = ex.dff_triangles(dff, True)
        game = COLGame.VC if getattr(model.version, 'value', 3) == 1 else COLGame.SA
        self._push_undo(idx, "Surfaces from DFF")
        n = ex.surfaces_from_triangles(model, verts, tris, game)
        self._mesh_edited(model, f"Surfaces set on {n} of {len(model.faces)} face(s) from "
                                 f"{os.path.basename(path)}", reselect=False)

    def _edit_show_vertices(self, checked): #vers 1
        """Red vertex dots in normal (face) mode."""
        self.preview_widget._show_verts = bool(checked)
        self.preview_widget.update()

    def _edit_tools_menu(self, menu): #vers 1
        """Key tools for right-click menus (viewport and model list)."""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as IF
        ic = self._get_icon_color()
        menu.addAction(IF.surfaceedit_icon(20, ic), "Edit Model...", self._open_surface_edit_dialog)
        vm = menu.addAction(IF.vertex_select_icon(20, ic), "Vertex Select Mode")
        vm.setCheckable(True)
        vm.setChecked(self.preview_widget._select_mode == 'vertex')
        vm.toggled.connect(self.vertex_mode_btn.setChecked)
        sv = menu.addAction(IF.show_vertices_icon(20, ic), "Show Vertices")
        sv.setCheckable(True)
        sv.setChecked(self.preview_widget._show_verts)
        sv.toggled.connect(self.show_verts_btn.setChecked)
        menu.addSeparator()
        menu.addAction(IF.hide_faces_icon(20, ic), "Hide Selected Faces", self._edit_hide_selected)
        menu.addAction(IF.unhide_faces_icon(20, ic), "Unhide All Faces", self._edit_unhide_all)
        menu.addAction(IF.select_material_icon(20, ic), "Select by Material", self._edit_select_material)
        menu.addSeparator()
        menu.addAction(IF.filter_icon(20, ic), "Optimise Mesh...", self._edit_optimise)
        menu.addAction(IF.optimum_bounds_icon(20, ic), "Optimum Bounds", self._edit_optimum_bounds)
        menu.addAction(IF.face_groups_icon(20, ic), "Generate Face Groups...", self._edit_face_groups)
        menu.addAction(IF.lighting_icon(20, ic), "Generate Lighting...", self._edit_lighting)
        menu.addAction(IF.copy_lod_icon(20, ic), "Copy as LOD", self._edit_copy_as_lod)
        menu.addSeparator()
        all_menu = menu.addMenu("All Tools")
        self._build_full_menu(all_menu)

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

    def _edit_optimise(self): #vers 1
        """Reduce face count: clean (lossless), merge flat areas, decimate (lossy)."""
        sel = self._edit_selection(need_faces=False)
        if not sel: return
        model, idx, _ = sel
        dlg = QDialog(self)
        dlg.setWindowTitle("Optimise Mesh")
        form = QFormLayout(dlg)
        clean = QCheckBox("Clean (lossless): weld duplicates, drop zero-area / duplicate faces")
        clean.setChecked(True)
        tol = QDoubleSpinBox(); tol.setDecimals(4); tol.setRange(0.0001, 0.1); tol.setValue(0.001)
        merge = QCheckBox("Merge flat areas (lossless): same material, same plane")
        merge.setChecked(True)
        ang = QDoubleSpinBox(); ang.setRange(0.1, 10.0); ang.setValue(0.5); ang.setSuffix(" deg")
        dec = QCheckBox("Decimate (lossy): collapse edges, material borders kept")
        pct = QSpinBox(); pct.setRange(5, 95); pct.setValue(50); pct.setSuffix(" % of faces")
        pct.setEnabled(False); dec.toggled.connect(pct.setEnabled)
        one = QRadioButton(f"Selected model ({model.name})"); one.setChecked(True)
        every = QRadioButton(f"All {len(self.current_col_file.models)} models in file")
        for w in (clean, merge, dec):
            w.setToolTip(w.text())
        form.addRow(clean); form.addRow("Weld tolerance:", tol)
        form.addRow(merge); form.addRow("Flat angle:", ang)
        form.addRow(dec); form.addRow("Keep:", pct)
        form.addRow(one); form.addRow(every)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        models = list(self.current_col_file.models) if every.isChecked() else [model]
        before = after = 0
        for m in models:
            if not m.faces:
                continue
            self._push_undo(self.current_col_file.models.index(m), "Optimise mesh")
            before += len(m.faces)
            if clean.isChecked():
                ops.clean_mesh(m, tol.value())
            if merge.isChecked():
                ops.merge_coplanar(m, ang.value())
            if dec.isChecked():
                ops.decimate(m, pct.value() / 100.0)
            ops.recalc_bounds(m)
            after += len(m.faces)
        cut = (before - after) * 100.0 / before if before else 0.0
        self._mesh_edited(model, f"Optimised {len(models)} model(s): {before} -> {after} faces (-{cut:.0f}%)")

    def _merge_col_files(self): #vers 1
        """Pick COL files; all their models are added to the open file."""
        paths, _ = QFileDialog.getOpenFileNames(self, "Merge COL Files",
                                                os.path.dirname(self.current_file_path or ''),
                                                "COL Files (*.col)")
        if paths:
            self._add_models_from_files(paths)


__all__ = ['COLEditMixin']
