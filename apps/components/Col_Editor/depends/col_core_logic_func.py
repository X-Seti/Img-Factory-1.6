#this belongs in apps/components/Col_Editor/depends/col_core_logic_func.py - Version: 6
# X-Seti - Sept 29 2026 - IMG Factory 1.6 - COL Workshop core logic

"""
COL Workshop core logic - file load/save, import/export, model edits, undo, surface.dat.
"""

##class COLCoreLogicMixin: -
# _add_models_from_files
# _analyze_collision
# _apply_name_edit
# _apply_settings
# _build_col_from_txd
# _change_format
# _compress_col
# _compress_surface
# _convert_surface
# _copy_model_to_clipboard
# _copy_surface
# _create_new_surface
# _create_shadow_mesh
# _delete_selected_model
# _delete_surface
# dragEnterEvent
# dragMoveEvent
# dropEvent
# _dropped_files
# _duplicate_selected_model
# _duplicate_surface
# export_all
# export_all_surfaces
# _export_col_data
# _export_col_model
# export_selected
# export_selected_surface
# _export_via_ide
# _force_save_col
# _get_selected_model
# _import_col_data
# _import_replace_col_model
# _import_selected
# _import_surface
# _import_via_ide
# _invert_selection
# _is_model_pinned
# load_from_img_archive
# _load_img_col_list
# _open_col_file
# open_col_file
# _open_col_from_img_entry
# _open_file
# open_img_archive
# _open_surface_edit_dialog
# _open_surface_type_dialog
# _paste_model_from_clipboard
# _paste_surface
# _pick_col_from_current_img
# _push_undo
# _remove_shadow
# _remove_shadow_mesh
# _remove_via_ide
# _rename_col_model
# _save_as_col_file
# save_col_file
# _save_col_file
# _save_file
# _save_file_as
# _save_settings
# _saveall_file
# _select_all_models
# shadow_dialog
# _shadow_mesh_changed
# _show_shadow_mesh
# showEvent
# _sort_models
# _sort_models_desc
# _surf_add
# _surf_changed
# _surf_delete
# _surf_dup
# _surf_on_select
# _surf_open
# _surf_populate
# _surf_refresh_list
# _surf_save
# _toggle_pin_selected
# _uncompress_col
# _uncompress_surface
# _undo_last_action

import os
from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QFileDialog, QListWidgetItem, QMessageBox, QTableWidgetItem
from apps.components.Col_Editor.depends.col_setup_ui_func import App_name, _SURFACE_FIELDS, _SurfaceEntry
from apps.methods.col_workshop_classes import COLHeader

class COLCoreLogicMixin: #vers 1
    """File, model and surface.dat operations for COLWorkshop."""

    def _delete_selected_model(self): #vers 3
        """Delete selected collision model(s) — uses currentRow() for reliability."""
        if not self.current_col_file: return
        models = getattr(self.current_col_file, 'models', [])
        if not models: return

        lw = (self.col_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
              else self.collision_list)

        # Collect selected indices (highest first so deletion doesn't shift lower rows)
        indices = sorted({i.row() for i in lw.selectionModel().selectedRows()
                          if 0 <= i.row() < len(models)}, reverse=True)
        cr = lw.currentRow()
        if 0 <= cr < len(models):
            indices = sorted(set(indices) | {cr}, reverse=True)
        if not indices: return

        from PyQt6.QtWidgets import QMessageBox
        if len(indices) == 1:
            name = models[indices[0]].name
            if QMessageBox.question(self, "Delete", f"Delete '{name}'?",
               QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
               ) != QMessageBox.StandardButton.Yes:
                return
        else:
            names = ", ".join(models[i].name for i in sorted(indices)[:10]) + (" ..." if len(indices) > 10 else "")
            if QMessageBox.question(self, "Delete",
               f"Delete {len(indices)} collision models?\n\n{names}",
               QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
               ) != QMessageBox.StandardButton.Yes:
                return

        for idx in indices:
            del models[idx]
        self._populate_collision_list()
        self._populate_compact_col_list()
        if hasattr(self, 'save_btn'):
            self.save_btn.setEnabled(True)
        self._set_status(f"Deleted {len(indices)} model(s) - not saved yet")

    def _duplicate_selected_model(self): #vers 2
        model = self._get_selected_model()   # visible list, delegate-safe
        if model is None: return
        idx = self.current_col_file.models.index(model)
        import copy
        m = copy.deepcopy(self.current_col_file.models[idx])
        m.name = m.name + "_copy"
        self.current_col_file.models.insert(idx+1, m)
        self._populate_collision_list()
        self._populate_compact_col_list()
        self._select_model_by_row(idx+1)

    def _copy_model_to_clipboard(self): #vers 2
        model = self._get_selected_model()   # visible list, delegate-safe
        if model is None: return
        idx = self.current_col_file.models.index(model)
        import copy
        self._clipboard_model = copy.deepcopy(self.current_col_file.models[idx])
        if hasattr(self, 'paste_btn') and self.paste_btn:
            self.paste_btn.setEnabled(True)

    def _paste_model_from_clipboard(self): #vers 2
        if not hasattr(self, '_clipboard_model') or not self._clipboard_model: return
        if not self.current_col_file: return
        import copy
        m = copy.deepcopy(self._clipboard_model)
        m.name = m.name + "_paste"
        self.current_col_file.models.append(m)
        self._populate_collision_list()
        self._populate_compact_col_list()
        self._select_model_by_row(len(self.current_col_file.models) - 1)

    def _get_selected_model(self): #vers 4
        """Return the currently selected COLModel or None.
        Uses currentRow() on the active (visible) list widget — more reliable
        than selectedRows() which can fail with custom delegates."""
        if not self.current_col_file:
            return None
        models = getattr(self.current_col_file, 'models', [])
        if not models:
            return None

        # Only check the VISIBLE list — the hidden one may have stale state
        if getattr(self, '_col_view_mode', 'detail') == 'detail':
            lw = getattr(self, 'col_compact_list', None)
        else:
            lw = getattr(self, 'collision_list', None)

        if lw is None:
            return None

        # currentRow() is always reliable; selectedRows() can fail with delegates
        row = lw.currentRow()
        if row < 0:
            # Fallback: try selectedRows
            rows = lw.selectionModel().selectedRows()
            if rows:
                row = rows[0].row()
        if 0 <= row < len(models):
            return models[row]
        return None

    def _create_new_surface(self): #vers 1
        """Add a new empty COL model to the loaded file."""
        if not self.current_col_file:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "No File", "Load a COL file first.")
            return
        from apps.methods.col_workshop_classes import COLModel, COLHeader, COLBounds, COLVersion
        from apps.methods.col_workshop_classes import Vector3
        hdr = COLHeader(fourcc=b'COLL', size=0, name='new_model',
                        model_id=0, version=COLVersion.COL_1)
        bnd = COLBounds(radius=1.0, center=Vector3(0,0,0),
                        min=Vector3(-1,-1,-1), max=Vector3(1,1,1))
        model = COLModel(header=hdr, bounds=bnd,
                         spheres=[], boxes=[], vertices=[], faces=[])
        self.current_col_file.models.append(model)
        self._populate_compact_col_list()
        self._populate_collision_list()
        new_row = len(self.current_col_file.models) - 1
        active = (self.col_compact_list
                  if self._col_view_mode == 'detail' else self.collision_list)
        if active.rowCount() > new_row:
            active.selectRow(new_row)

    def _build_col_from_txd(self): #vers 4
        """Create stub COL models for each texture name in a loaded TXD."""
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        from apps.methods.col_workshop_loader import COLFile
        from apps.methods.col_workshop_classes import COLModel, COLVersion, COLBounds
        txd_path, _ = QFileDialog.getOpenFileName(
            self, "Select TXD file", "", "TXD Files (*.txd);;All Files (*)")
        if not txd_path:
            return
        try:
            import os
            # Extract texture names from TXD (scan for null-terminated strings after each 0x15 chunk)
            import struct
            names = []
            with open(txd_path, 'rb') as f:
                data = f.read()
            pos = 0
            while pos < len(data) - 12:
                t, s, v = struct.unpack_from('<III', data, pos)
                if t == 0x15 and pos + 12 + 12 < len(data):
                    body = pos + 12 + 12
                    name = data[body+8:body+40].rstrip(b'').decode('ascii','ignore').strip()
                    if name:
                        names.append(name)
                pos += 12 + s if s > 0 else 1
            if not names:
                QMessageBox.warning(self, "No Textures", "No texture names found in TXD.")
                return
            if not self.current_col_file:
                from apps.methods.col_workshop_loader import COLFile
                self.current_col_file = COLFile()
                self.current_col_file.models = []
            added = 0
            for name in names:
                hdr = COLHeader(fourcc=b'COL2', size=0, name=name,
                                model_id=0, version=COLVersion.COL_2)
                bounds = COLBounds(radius=1.73, center=(0.0, 0.0, 0.0),
                                   min=(-1.0, -1.0, -1.0), max=(1.0, 1.0, 1.0))
                m = COLModel(header=hdr, bounds=bounds,
                             spheres=[], boxes=[], vertices=[], faces=[])
                self.current_col_file.models.append(m)
                added += 1
            self._populate_collision_list()
            self._populate_compact_col_list()
            msg = f"Created {added} stub COL model(s) from {os.path.basename(txd_path)}"
            self._set_status(msg)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(msg)
        except Exception as e:
            QMessageBox.critical(self, "Error", str(e))

    def _convert_surface(self): #vers 4
        """Convert selected model version; per-surface GTA3/VC <-> SA mapping table."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QLabel, QComboBox,
                                     QPushButton, QMessageBox, QCheckBox, QTableWidget,
                                     QTableWidgetItem, QHeaderView)
        from apps.methods.col_workshop_classes import COLVersion
        from apps.methods.col_materials import (convert_material_id, convert_piece_flag,
                                                get_material_name, get_materials_for_version, COLGame)
        model = self._get_selected_model()
        if not model:
            QMessageBox.warning(self, "No Selection", "Select a collision model first.")
            return
        current = model.version
        items = list(model.spheres) + list(model.boxes) + list(model.faces)
        used = {}
        for it in items:
            used[int(it.material_id)] = used.get(int(it.material_id), 0) + 1
        game_of = lambda v: COLGame.VC if v == COLVersion.COL_1 else COLGame.SA

        dlg = QDialog(self)
        dlg.setWindowTitle(f"Convert COL version - {model.name}")
        dlg.resize(560, 420)
        lay = QVBoxLayout(dlg)
        lay.addWidget(QLabel(f"Current version: <b>{current.name}</b>"))
        row = QHBoxLayout()
        row.addWidget(QLabel("Convert to:"))
        combo = QComboBox()
        for v in (COLVersion.COL_1, COLVersion.COL_2, COLVersion.COL_3):
            if v != current:
                combo.addItem(v.name, v)
        row.addWidget(combo, 1)
        lay.addLayout(row)
        restore = QCheckBox("Restore original SA surfaces where known")
        restore.setChecked(True)
        lay.addWidget(restore)
        table = QTableWidget(0, 3)
        table.setHorizontalHeaderLabels(["Used", "Surface now", "Becomes"])
        table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        table.verticalHeader().setVisible(False)
        lay.addWidget(table, 1)
        note = QLabel("")
        note.setWordWrap(True)
        lay.addWidget(note)
        maps = {}

        def _refresh():  #vers 1
            target = combo.currentData()
            src, dst = game_of(current), game_of(target)
            cross = src != dst
            has_stash = any(hasattr(it, '_sa_material') for it in items)
            restore.setVisible(cross and dst == COLGame.SA and has_stash)
            table.setVisible(cross)
            table.setRowCount(0)
            maps.clear()
            if cross:
                choices = get_materials_for_version(dst, include_vehicle=True)
                for mid in sorted(used):
                    r = table.rowCount()
                    table.insertRow(r)
                    table.setItem(r, 0, QTableWidgetItem(str(used[mid])))
                    table.setItem(r, 1, QTableWidgetItem(f"{mid}  {get_material_name(mid, src)}"))
                    box = QComboBox()
                    for cid, cname, _ in choices:
                        box.addItem(f"{cid}  {cname}", cid)
                    box.setCurrentIndex(max(0, box.findData(convert_material_id(mid, src, dst))))
                    table.setCellWidget(r, 2, box)
                    maps[mid] = box
            msgs = []
            if cross and dst == COLGame.VC:
                msgs.append(f"GTA3/VC has {len(get_materials_for_version(COLGame.VC, True))} surfaces, "
                            f"SA has {len(get_materials_for_version(COLGame.SA, True))}: "
                            "several SA surfaces share one VC surface. Originals are remembered "
                            "until the workshop closes.")
            if current.value >= 2 and target == COLVersion.COL_1:
                msgs.append("Face groups, shadow mesh and lines are not kept.")
            note.setText("\n".join(msgs))
        combo.currentIndexChanged.connect(_refresh)
        _refresh()

        btns = QHBoxLayout()
        ok = QPushButton("Convert"); cancel = QPushButton("Cancel")
        btns.addStretch(); btns.addWidget(ok); btns.addWidget(cancel)
        lay.addLayout(btns)
        cancel.clicked.connect(dlg.reject)

        def _do():  #vers 3
            target = combo.currentData()
            src, dst = game_of(current), game_of(target)
            if src != dst:
                use_stash = not restore.isHidden() and restore.isChecked()
                for it in items:
                    old = int(it.material_id)
                    if dst == COLGame.VC:
                        it._sa_material = old
                    if use_stash and hasattr(it, '_sa_material'):
                        it.material = it._sa_material
                    else:
                        it.material = maps[old].currentData() if old in maps else old
                    if dst == COLGame.SA and hasattr(it, '_sa_material'):
                        del it._sa_material
                    if hasattr(it, 'flag'):
                        it.flag = convert_piece_flag(int(it.flag or 0), src, dst)
            model.version = target
            self._populate_collision_list()
            self._populate_compact_col_list()
            if hasattr(self, 'save_btn'):
                self.save_btn.setEnabled(True)
            self._set_status(f"Converted {model.name} to {target.name} - not saved yet")
            dlg.accept()
        ok.clicked.connect(_do)
        dlg.exec()

    def _create_shadow_mesh(self): #vers 3
        """Create shadow mesh as a copy of the main collision mesh (COL3)."""
        from PyQt6.QtWidgets import QMessageBox
        import copy
        model = self._get_selected_model()
        if not model:
            QMessageBox.warning(self, "No Selection", "Select a collision model first.")
            return
        if not model.vertices or not model.faces:
            QMessageBox.warning(self, "No Mesh", f"'{model.name}' has no vertex/face data.")
            return
        from apps.methods.col_workshop_classes import COLVersion
        if getattr(model.version, 'value', 0) < 3:
            reply = QMessageBox.question(self, "Upgrade to COL3",
                "Shadow mesh requires COL3. Upgrade this model?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
            if reply != QMessageBox.StandardButton.Yes:
                return
        if model.shadow_faces and QMessageBox.question(self, "Replace Shadow Mesh",
                f"'{model.name}' already has a shadow mesh. Replace it with a copy of the mesh?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No) != QMessageBox.StandardButton.Yes:
            return
        self._push_undo(self.current_col_file.models.index(model), "Create shadow mesh")
        if getattr(model.version, 'value', 0) < 3:
            model.version = COLVersion.COL_3
        model.shadow_vertices = copy.deepcopy(model.vertices)
        model.shadow_faces = copy.deepcopy(model.faces)
        self._shadow_mesh_changed(model, f"Shadow mesh created for {model.name}: "
                                  f"{len(model.shadow_vertices)}V {len(model.shadow_faces)}F")

    def _remove_shadow_mesh(self): #vers 3
        """Remove shadow mesh data from the selected COL model."""
        from PyQt6.QtWidgets import QMessageBox
        model = self._get_selected_model()
        if not model:
            QMessageBox.warning(self, "No Selection", "Select a collision model first.")
            return
        if not model.shadow_faces:
            QMessageBox.information(self, "No Shadow Mesh", f"{model.name} has no shadow mesh.")
            return
        if QMessageBox.question(self, "Remove Shadow Mesh",
                f"Remove shadow mesh from '{model.name}'?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No) != QMessageBox.StandardButton.Yes:
            return
        self._push_undo(self.current_col_file.models.index(model), "Remove shadow mesh")
        model.shadow_vertices = []
        model.shadow_faces = []
        self._shadow_mesh_changed(model, f"Removed shadow mesh from {model.name}")

    def _shadow_mesh_changed(self, model, msg): #vers 1
        """Refresh lists, viewport and save button after a shadow edit."""
        idx = self.current_col_file.models.index(model)
        self._populate_collision_list()
        self._populate_compact_col_list()
        self._select_model_by_row(idx)
        if hasattr(self, 'save_btn'):
            self.save_btn.setEnabled(True)
        self._set_status(msg + " - not saved yet")
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(msg)

    def _compress_col(self): #vers 2
        """Mark COL file for compressed output (sets flags on export)."""
        from PyQt6.QtWidgets import QMessageBox
        if not self.current_col_file:
            QMessageBox.warning(self, "No File", "No COL file loaded.")
            return
        QMessageBox.information(self, "COL Compression",
            "COL files do not use zlib/LZO compression internally.\n\n"
            "To reduce size: remove unused models, clear shadow meshes,\n"
            "or reduce vertex/face counts in the mesh editor.")

    def _uncompress_col(self): #vers 2
        """Reload COL file (parses fresh from disk, clears in-memory edits)."""
        from PyQt6.QtWidgets import QMessageBox
        if not self.current_col_file or not getattr(self, 'current_file_path', None):
            QMessageBox.warning(self, "No File", "No COL file loaded.")
            return
        reply = QMessageBox.question(self, "Reload File",
            "Reload from disk? Unsaved changes will be lost.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply == QMessageBox.StandardButton.Yes:
            self._open_file(self.current_file_path)

    def _surf_open(self, path: str = None): #vers 1
        if not path:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open surface.dat", "",
                "Surface data (surface.dat *.dat);;All files (*)")
        if not path:
            return
        if self._surf_parser.load(path):
            self._surf_path = path
            self._surf_refresh_list()
            self._surf_status.setText(
                f"{os.path.basename(path)}  —  {len(self._surf_parser.entries)} surfaces  [{self._surf_parser.game}]")
        else:
            QMessageBox.critical(self, "Error", f"Failed to load {path}")

    def _surf_save(self): #vers 1
        path = getattr(self, '_surf_path', None)
        if not path:
            path, _ = QFileDialog.getSaveFileName(self, "Save surface.dat", "", "DAT files (*.dat)")
        if path and self._surf_parser.save(path):
            self._surf_status.setText(f"Saved {os.path.basename(path)}")

    def _surf_refresh_list(self, ft: str = ""): #vers 1
        self._surf_list.clear()
        for i, e in enumerate(self._surf_parser.entries):
            if ft and ft.lower() not in e.values[0].lower():
                continue
            item = QListWidgetItem(e.values[0] if e.values else f"surface_{i}")
            item.setData(Qt.ItemDataRole.UserRole, i)
            self._surf_list.addItem(item)

    def _surf_on_select(self, row: int): #vers 1
        item = self._surf_list.item(row)
        if not item:
            return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None or idx >= len(self._surf_parser.entries):
            return
        self._surf_cur_idx = idx
        self._surf_populate(self._surf_parser.entries[idx])

    def _surf_populate(self, entry): #vers 1
        self._surf_blocking = True
        for i, (fname, ftype, *_) in enumerate(_SURFACE_FIELDS):
            if i >= len(entry.values):
                break
            w = self._surf_widgets.get(fname)
            if not w:
                continue
            try:
                v = entry.values[i]
                if ftype == 'float':   w.setValue(float(v))
                elif ftype == 'int':   w.setValue(int(v))
                elif ftype == 'bool':  w.setChecked(int(v) != 0)
                elif hasattr(w,'setText'): w.setText(str(v))
            except Exception:
                pass
        self._surf_blocking = False

    def _surf_changed(self, fname: str, value): #vers 1
        if self._surf_blocking or self._surf_cur_idx < 0:
            return
        entry = self._surf_parser.entries[self._surf_cur_idx]
        for i, (fn, *_) in enumerate(_SURFACE_FIELDS):
            if fn == fname and i < len(entry.values):
                entry.values[i] = str(value)
                break

    def _surf_add(self): #vers 1
        tmpl = self._surf_parser.entries[0].values[:] if self._surf_parser.entries \
               else ['NEWSURFACE','0.9','0.9','0','0','1','0.1','0','ROAD','1','1','1','0']
        tmpl[0] = 'NEWSURFACE'
        e = _SurfaceEntry(); e.values = tmpl
        self._surf_parser.entries.append(e)
        self._surf_refresh_list(self._surf_search.text())
        self._surf_list.setCurrentRow(self._surf_list.count()-1)

    def _surf_delete(self): #vers 1
        if self._surf_cur_idx < 0:
            return
        name = self._surf_parser.entries[self._surf_cur_idx].values[0]
        if QMessageBox.question(self, "Delete", f"Delete {name}?") != QMessageBox.StandardButton.Yes:
            return
        self._surf_parser.entries.pop(self._surf_cur_idx)
        self._surf_cur_idx = -1
        self._surf_refresh_list(self._surf_search.text())

    def _surf_dup(self): #vers 1
        if self._surf_cur_idx < 0:
            return
        src = self._surf_parser.entries[self._surf_cur_idx]
        e = _SurfaceEntry(); e.values = src.values[:]
        e.values[0] = src.values[0] + '_COPY'
        self._surf_parser.entries.insert(self._surf_cur_idx + 1, e)
        self._surf_refresh_list(self._surf_search.text())

    def _load_img_col_list(self): #vers 3
        """Load COL files from IMG archive"""
        try:
            # Safety check for standalone mode
            if self.standalone_mode or not hasattr(self, 'col_list_widget') or self.col_list_widget is None:
                return

            self.col_list_widget.clear()
            self.col_list = []

            if not self.current_img:
                return

            for entry in self.current_img.entries:
                if entry.name.lower().endswith('.col'):
                    self.col_list.append(entry)
                    item = QListWidgetItem(entry.name)
                    item.setData(Qt.ItemDataRole.UserRole, entry)
                    size_kb = entry.size / 1024
                    item.setToolTip(f"{entry.name}\nSize: {size_kb:.1f} KB")
                    self.col_list_widget.addItem(item)

            hdr = getattr(self, '_col_list_header', None)
            if hdr:
                hdr.setText(f"COL Files  ({len(self.col_list)})")
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Found {len(self.col_list)} COL files")
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error loading COL list: {str(e)}")

    def _apply_settings(self, dialog): #vers 6
        """Apply settings from dialog"""
        from PyQt6.QtGui import QFont

        # Store font settings
        self.title_font = QFont(self.title_font_combo.currentFont().family(), self.title_font_size.value())
        self.panel_font = QFont(self.panel_font_combo.currentFont().family(), self.panel_font_size.value())
        self.button_font = QFont(self.button_font_combo.currentFont().family(), self.button_font_size.value())
        self.infobar_font = QFont(self.infobar_font_combo.currentFont().family(), self.infobar_font_size.value())

        # Apply fonts to specific elements
        self._apply_title_font()
        self._apply_panel_font()
        self._apply_button_font()
        self._apply_infobar_font()

        # Apply button display mode
        mode_map = ["icons", "text", "both"]
        new_mode = mode_map[self.settings_display_combo.currentIndex()]
        if new_mode != self.button_display_mode:
            self.button_display_mode = new_mode
            self._update_all_buttons()

        # Locale setting (would need implementation)
        locale_text = self.settings_locale_combo.currentText()

    def _open_file(self): #vers 1
        """Open file dialog and load COL file"""
        try:
            file_path, _ = QFileDialog.getOpenFileName(
                self,
                "Open COL File",
                "",
                "COL Files (*.col);;All Files (*)"
            )

            if file_path:
                self.open_col_file(file_path)

        except Exception as e:
            print(f"Error in open file dialog: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to open file:\n{str(e)}")

    def _push_undo(self, model_index, description=""): #vers 1
        """Deep-copy model[model_index] onto undo stack before any edit."""
        import copy
        if not self.current_col_file:
            return
        models = getattr(self.current_col_file, 'models', [])
        if model_index < 0 or model_index >= len(models):
            return
        self.undo_stack.append({
            'description': description,
            'model_index': model_index,
            'model_data':  copy.deepcopy(models[model_index]),
        })
        if len(self.undo_stack) > 50:
            self.undo_stack.pop(0)
        if hasattr(self, 'undo_col_btn'):
            self.undo_col_btn.setEnabled(True)
        # Also enable the paint toolbar undo button if paint mode active
        if hasattr(self, 'paint_undo_btn') and getattr(self, 'paint_toolbar', None)                 and self.paint_toolbar.isVisible():
            self.paint_undo_btn.setEnabled(True)

    def _undo_last_action(self): #vers 2
        """Restore the last deep-copied model from the undo stack."""
        try:
            if not self.undo_stack:
                return
            entry = self.undo_stack.pop()
            idx   = entry['model_index']
            saved = entry['model_data']
            desc  = entry.get('description', '')
            if self.current_col_file:
                models = getattr(self.current_col_file, 'models', [])
                if idx < len(models):
                    models[idx] = saved
                    self._populate_collision_list()
                    self._populate_compact_col_list()
                    if hasattr(self, 'preview_widget'):
                        self.preview_widget.set_current_model(saved, idx)
            if hasattr(self, 'undo_col_btn'):
                self.undo_col_btn.setEnabled(bool(self.undo_stack))
            msg = f"Undo: {desc}" if desc else "Undo applied"
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(msg)
        except Exception as e:
            print(f"Undo error: {e}")

    def _select_all_models(self): #vers 1
        """Select all entries in the active list (Ctrl+A)."""
        lw = (self.col_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
              else self.collision_list)
        lw.selectAll()

    def _invert_selection(self): #vers 1
        """Invert the current selection."""
        lw = (self.col_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
              else self.collision_list)
        selected = {i.row() for i in lw.selectionModel().selectedRows()}
        lw.clearSelection()
        lw.setSelectionMode(lw.selectionMode())  # keep mode
        for r in range(lw.rowCount()):
            if r not in selected:
                lw.selectRow(r)  # QTableWidget multi-select needs blockSignals trick
        # Proper invert via selection model
        from PyQt6.QtCore import QItemSelection, QItemSelectionModel
        sel_model = lw.selectionModel()
        full = QItemSelection()
        full.select(lw.model().index(0, 0),
                    lw.model().index(lw.rowCount()-1, lw.columnCount()-1))
        sel_model.select(full, QItemSelectionModel.SelectionFlag.Toggle)

    def _sort_models(self, key: str = 'name'): #vers 1
        """Sort collision models in place by key: 'name','version','faces','boxes','spheres','vertices'."""
        if not self.current_col_file: return
        models = getattr(self.current_col_file, 'models', [])
        if not models: return

        def sort_key(m):  #vers 1
            if key == 'name':     return (getattr(m,'name','') or '').lower()
            if key == 'version':  return getattr(getattr(m,'version',None),'value',0)
            if key == 'faces':    return len(getattr(m,'faces',[]))
            if key == 'boxes':    return len(getattr(m,'boxes',[]))
            if key == 'spheres':  return len(getattr(m,'spheres',[]))
            if key == 'vertices': return len(getattr(m,'vertices',[]))
            return 0

        models.sort(key=sort_key)
        self._populate_collision_list()
        self._populate_compact_col_list()
        self._set_status(f"Sorted by {key}.")

    def _sort_models_desc(self, key: str): #vers 1
        """Sort descending (largest first)."""
        if not self.current_col_file: return
        models = getattr(self.current_col_file, 'models', [])
        def k(m):  #vers 1
            return len(getattr(m, key, []))
        models.sort(key=k, reverse=True)
        self._populate_collision_list()
        self._populate_compact_col_list()
        self._set_status(f"Sorted by {key} (descending).")

    def _toggle_pin_selected(self): #vers 1
        """Toggle pin (edit-lock) on selected models. Pinned models can't be deleted/renamed."""
        if not self.current_col_file: return
        lw = (self.col_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
              else self.collision_list)
        indices = sorted({i.row() for i in lw.selectionModel().selectedRows()})
        cr = lw.currentRow()
        if 0 <= cr: indices = sorted(set(indices) | {cr})
        models = self.current_col_file.models

        if not hasattr(self, '_pinned_models'):
            self._pinned_models = set()

        pinned_now = 0
        for idx in indices:
            if idx < len(models):
                name = getattr(models[idx], 'name', f'model_{idx}')
                if idx in self._pinned_models:
                    self._pinned_models.discard(idx)
                else:
                    self._pinned_models.add(idx)
                    pinned_now += 1

        # Refresh both lists to show pin state
        self._populate_collision_list()
        self._populate_compact_col_list()
        if pinned_now:
            self._set_status(f"Pinned {pinned_now} model(s) — protected from editing.")
        else:
            self._set_status("Unpinned selected model(s).")

    def _is_model_pinned(self, row: int) -> bool: #vers 1
        """Return True if model at row is pinned."""
        return row in getattr(self, '_pinned_models', set())

    def _import_via_ide(self): #vers 1
        """Import COL entries referenced by the currently loaded IDE file."""
        from PyQt6.QtWidgets import QMessageBox, QFileDialog
        # Try to get IDE file path from main window DAT browser
        ide_path = None
        if self.main_window and hasattr(self.main_window, 'current_ide_path'):
            ide_path = self.main_window.current_ide_path

        if not ide_path:
            ide_path, _ = QFileDialog.getOpenFileName(
                self, "Select IDE File", "",
                "IDE Files (*.ide);;All Files (*)")
        if not ide_path:
            return

        try:
            # Parse IDE to get model names
            names = []
            with open(ide_path, 'r', errors='ignore') as f:
                in_objs = False
                for line in f:
                    line = line.strip()
                    if line.lower() in ('objs', 'tobj', 'anim'):
                        in_objs = True; continue
                    if line == 'end':
                        in_objs = False; continue
                    if in_objs and line and not line.startswith('#'):
                        parts = line.split(',')
                        if len(parts) >= 2:
                            names.append(parts[1].strip().lower())

            if not names:
                QMessageBox.information(self, "IDE Import",
                    "No model names found in IDE file.")
                return

            # Find matching models in the current COL file
            if not self.current_col_file:
                QMessageBox.warning(self, "No COL File", "Load a COL file first.")
                return

            models = self.current_col_file.models
            matched = [m for m in models
                       if (getattr(m,'name','') or '').lower() in names]

            QMessageBox.information(self, "IDE Import",
                f"IDE has {len(names)} model names.\n"
                f"{len(matched)} matching collision models found in current COL file.\n\n"
                f"Showing matched models in list.")

            # Select matched rows
            lw = (self.col_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
                  else self.collision_list)
            lw.clearSelection()
            name_set = {(getattr(m,'name','') or '').lower() for m in matched}
            for i, model in enumerate(models):
                if (getattr(model,'name','') or '').lower() in name_set:
                    lw.selectRow(i)

            self._set_status(f"IDE: {len(matched)}/{len(names)} models matched.")

        except Exception as e:
            QMessageBox.critical(self, "IDE Import Error", str(e))

    def _remove_via_ide(self): #vers 1
        """Remove collision models NOT referenced by an IDE file (cleanup)."""
        from PyQt6.QtWidgets import QMessageBox, QFileDialog
        if not self.current_col_file:
            QMessageBox.warning(self, "No COL File", "Load a COL file first.")
            return

        ide_path, _ = QFileDialog.getOpenFileName(
            self, "Select IDE to remove unref'd COL models", "",
            "IDE Files (*.ide);;All Files (*)")
        if not ide_path:
            return

        try:
            names = set()
            with open(ide_path, 'r', errors='ignore') as f:
                in_objs = False
                for line in f:
                    line = line.strip()
                    if line.lower() in ('objs', 'tobj', 'anim'):
                        in_objs = True; continue
                    if line == 'end':
                        in_objs = False; continue
                    if in_objs and line and not line.startswith('#'):
                        parts = line.split(',')
                        if len(parts) >= 2:
                            names.add(parts[1].strip().lower())

            models = self.current_col_file.models
            to_remove = [i for i, m in enumerate(models)
                         if (getattr(m,'name','') or '').lower() not in names]

            if not to_remove:
                QMessageBox.information(self, "Remove via IDE",
                    "All COL models are referenced by the IDE — nothing to remove.")
                return

            example_names = [models[i].name for i in to_remove[:5]]
            reply = QMessageBox.question(self, "Remove via IDE",
                f"Remove {len(to_remove)} unreferenced model(s)?\n\n"
                f"Examples: {', '.join(example_names)}"
                + (" ..." if len(to_remove) > 5 else ""),
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
            if reply != QMessageBox.StandardButton.Yes:
                return

            for i in sorted(to_remove, reverse=True):
                del models[i]
            self._populate_collision_list()
            self._populate_compact_col_list()
            self._set_status(f"Removed {len(to_remove)} unreferenced model(s).")

        except Exception as e:
            QMessageBox.critical(self, "Remove via IDE Error", str(e))

    def _export_via_ide(self): #vers 1
        """Export only COL models referenced by an IDE file."""
        from PyQt6.QtWidgets import QMessageBox, QFileDialog
        import os
        from apps.methods.col_workshop_writer import save_col_file

        if not self.current_col_file:
            QMessageBox.warning(self, "No COL File", "Load a COL file first.")
            return

        ide_path, _ = QFileDialog.getOpenFileName(
            self, "Select IDE File for Export", "",
            "IDE Files (*.ide);;All Files (*)")
        if not ide_path:
            return

        out_path, _ = QFileDialog.getSaveFileName(
            self, "Save filtered COL archive", "",
            "COL Files (*.col);;All Files (*)")
        if not out_path:
            return

        try:
            names = set()
            with open(ide_path, 'r', errors='ignore') as f:
                in_objs = False
                for line in f:
                    line = line.strip()
                    if line.lower() in ('objs', 'tobj', 'anim'):
                        in_objs = True; continue
                    if line == 'end':
                        in_objs = False; continue
                    if in_objs and line and not line.startswith('#'):
                        parts = line.split(',')
                        if len(parts) >= 2:
                            names.add(parts[1].strip().lower())

            matched = [m for m in self.current_col_file.models
                       if (getattr(m,'name','') or '').lower() in names]

            if not matched:
                QMessageBox.warning(self, "No Matches",
                    "No COL models matched the IDE entries.")
                return

            if save_col_file(matched, out_path):
                msg = (f"Exported {len(matched)} IDE-referenced model(s) to:\n"
                       f"{os.path.basename(out_path)}")
                self._set_status(msg)
                QMessageBox.information(self, "Export via IDE", msg)
            else:
                QMessageBox.warning(self, "Export Failed", "Could not write output file.")

        except Exception as e:
            QMessageBox.critical(self, "Export via IDE Error", str(e))

    def _import_col_data(self): #vers 2
        """Import one or more COL models from .col file(s) into the current archive."""
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        if not self.current_col_file:
            # No file loaded yet — open the files directly
            self._open_file()
            return

        paths, _ = QFileDialog.getOpenFileNames(
            self, "Import COL File(s)", "",
            "COL Files (*.col);;All Files (*)")
        if not paths:
            return

        from apps.methods.col_workshop_loader import COLFile
        added = 0
        for path in paths:
            cf = COLFile()
            if cf.load_from_file(path):
                for model in cf.models:
                    self.current_col_file.models.append(model)
                    added += 1
            else:
                print(f"Import failed: {path}")

        if added:
            self._populate_collision_list()
            self._populate_compact_col_list()
            # Select last added
            last = len(self.current_col_file.models) - 1
            active = (self.col_compact_list
                      if getattr(self,'_col_view_mode','list')=='detail'
                      else self.collision_list)
            if active.rowCount() > last:
                active.selectRow(last)
            msg = f"Imported {added} model(s) from {len(paths)} file(s)."
            self._set_status(msg)
            if self.main_window and hasattr(self.main_window,'log_message'):
                self.main_window.log_message(msg)
        else:
            QMessageBox.warning(self, "Import", "No models could be imported.")

    def _export_col_data(self): #vers 2
        """Extract/export selected COL models (or all) to individual .col files."""
        import os
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        from apps.methods.col_workshop_writer import save_col_file

        if not self.current_col_file:
            QMessageBox.warning(self, "Export", "No COL file loaded.")
            return
        models = getattr(self.current_col_file, 'models', [])
        if not models:
            QMessageBox.warning(self, "Export", "No collision models to export.")
            return

        # Determine selection from the VISIBLE list
        if getattr(self, '_col_view_mode', 'detail') == 'detail':
            lw = getattr(self, 'col_compact_list', None)
        else:
            lw = getattr(self, 'collision_list', None)

        indices = set()
        if lw is not None:
            for idx in lw.selectionModel().selectedRows():
                if idx.row() < len(models):
                    indices.add(idx.row())
            # Also include currentRow() in case selectedRows() missed it
            cr = lw.currentRow()
            if 0 <= cr < len(models):
                indices.add(cr)
        if not indices:
            indices = set(range(len(models)))
        indices = sorted(indices)

        if len(indices) == 1:
            model = models[indices[0]]
            safe = (getattr(model,'name','model') or 'model').replace(' ','_')
            out, _ = QFileDialog.getSaveFileName(
                self, "Export COL Model", safe + '.col', "COL Files (*.col);;All Files (*)")
            if not out: return
            ok = save_col_file([model], out)
            msg = f"Exported {os.path.basename(out)}" if ok else "Export failed."
            ok_count = 1 if ok else 0
        else:
            folder = QFileDialog.getExistingDirectory(
                self, f"Extract {len(indices)} COL models to folder")
            if not folder: return
            ok_count = 0
            for i in indices:
                model = models[i]
                safe = (getattr(model,'name',f'model_{i}') or f'model_{i}').replace(' ','_')
                out  = os.path.join(folder, safe.lower() + '.col')
                base, ext = os.path.splitext(out)
                n = 1
                while os.path.exists(out):
                    out = f"{base}_{n}{ext}"; n += 1
                if save_col_file([model], out):
                    ok_count += 1
            msg = f"Extracted {ok_count} of {len(indices)} model(s) to {folder}"
            ok  = ok_count > 0

        self._set_status(msg)
        if self.main_window and hasattr(self.main_window,'log_message'):
            self.main_window.log_message(msg)
        if ok:
            QMessageBox.information(self, "Extract Complete", msg)
        else:
            QMessageBox.warning(self, "Extract Failed", msg)

    def _save_file(self): #vers 3
        """Save current COL file — serialises all models via COLWriter."""
        if not self.current_col_file:
            QMessageBox.warning(self, "Save", "No COL file loaded to save")
            return

        if not self.current_file_path:
            self._save_file_as()
            return

        models = getattr(self.current_col_file, 'models', [])
        if not models:
            QMessageBox.warning(self, "Save", "No models to save.")
            return

        damaged = getattr(self.current_col_file, 'damaged_records', 0)
        if damaged and QMessageBox.question(
                self, "Save COL",
                f"{damaged} damaged record(s) could not be read when this file was opened "
                "and will not be written.\n\nSave anyway?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.No) != QMessageBox.StandardButton.Yes:
            return
        info = getattr(self.current_col_file, 'splice_info', None)
        if info is None and any(getattr(m, '_orig_record', None) is not None for m in models) is False \
                and getattr(self.current_col_file, 'raw_data', None):
            # the file could not be matched model-by-model, so data this editor does not
            # model (COL2/3 lines, face groups, shadow mesh...) cannot be kept
            if QMessageBox.question(
                    self, "Save COL",
                    "This COL file cannot be saved without dropping data the editor does not model "
                    "(suspension lines, face groups, shadow mesh...).\n\nSave anyway?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                    QMessageBox.StandardButton.No) != QMessageBox.StandardButton.Yes:
                return
        try:
            from apps.methods.col_workshop_parser import COLWriter
            from apps.methods.col_splice import build_col_bytes
            raw = build_col_bytes(models, COLWriter, info)
        except Exception as e:
            import traceback
            traceback.print_exc()
            QMessageBox.critical(self, "Save COL", f"Nothing was written:\n{e}")
            return

        try:
            from apps.methods.file_backup import safe_write_bytes      # backup first, atomic write
            safe_write_bytes(self.current_file_path, raw)
            # Re-tag against what was just written so a second Save starts from it
            self.current_col_file.raw_data = raw
            try:
                from apps.methods.col_splice import tag_models
                self.current_col_file.splice_info = tag_models(models, raw)
            except Exception:
                self.current_col_file.splice_info = None
            fname = os.path.basename(self.current_file_path)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Saved COL: {fname} ({len(models)} models, {len(raw):,} bytes)")
            self._set_status(f"Saved: {fname}")
        except Exception as e:
            QMessageBox.critical(self, "Write Error", str(e))

    def _save_file_as(self): #vers 1
        """Save As dialog"""
        try:
            file_path, _ = QFileDialog.getSaveFileName(
                self,
                "Save COL File As",
                "",
                "COL Files (*.col);;All Files (*)"
            )

            if file_path:
                self.current_file_path = file_path
                self.current_col_file.file_path = file_path
                self._save_file()

        except Exception as e:
            print(f"Error in save as dialog: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to save file:\n{str(e)}")

    def _save_settings(self): #vers 1
        """Save settings to config file"""
        import json

        settings_file = os.path.join(
            os.path.dirname(__file__),
            'col_workshop_settings.json'
        )

        try:
            settings = {
                'save_to_source_location': self.save_to_source_location,
                'last_save_directory': self.last_save_directory
            }

            with open(settings_file, 'w') as f:
                json.dump(settings, indent=2, fp=f)
        except Exception as e:
            print(f"Failed to save settings: {e}")

    def _dropped_files(self, event): #vers 1
        """Local .col/.img paths carried by a drag event."""
        md = event.mimeData()
        if not md.hasUrls():
            return []
        return [u.toLocalFile() for u in md.urls()
                if u.isLocalFile() and u.toLocalFile().lower().endswith(('.col', '.img'))]

    def _add_models_from_files(self, paths): #vers 1
        """Append every model from the given .col files to the open file."""
        from PyQt6.QtWidgets import QMessageBox
        from apps.methods.col_workshop_loader import COLFile
        added, failed = 0, []
        for path in paths:
            cf = COLFile()
            if cf.load_from_file(path) and cf.models:
                for m in cf.models:
                    m._orphans_after = []       # keep only the model records
                self.current_col_file.models.extend(cf.models)
                added += len(cf.models)
            else:
                failed.append(os.path.basename(path))
        if added:
            self._populate_collision_list()
            self._populate_compact_col_list()
            last = len(self.current_col_file.models) - 1
            active = (self.col_compact_list
                      if getattr(self, '_col_view_mode', 'list') == 'detail'
                      else self.collision_list)
            if active.rowCount() > last:
                active.selectRow(last)
            if hasattr(self, 'save_btn'):
                self.save_btn.setEnabled(True)
            msg = f"Added {added} model(s) from {len(paths) - len(failed)} file(s) - not saved yet"
            self._set_status(msg)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(msg)
        if failed:
            QMessageBox.warning(self, "Add COL", "Could not read:\n" + "\n".join(failed))

    def open_col_file(self, file_path): #vers 4
        """Open standalone COL file - supports COL1, COL2, COL3"""
        try:
            from apps.methods.col_workshop_loader import COLFile

            # Create and load COL file
            # col_file = COLFile()
            # col_file.load_from_file(file_path)

            #from apps.methods.col_workshop_loader import load_col_with_progress
            #col_file = load_col_with_progress(file_path, self)

            #if not col_file:  # Just check if None
            #    return False

            # Large file warning + progress feedback
            import os as _os
            _fsize = _os.path.getsize(file_path)
            _fsize_mb = _fsize / 1024 / 1024
            if _fsize > 512 * 1024 * 1024:  # > 512 MB
                from PyQt6.QtWidgets import QMessageBox
                reply = QMessageBox.question(
                    self, "Large COL File",
                    f"{os.path.basename(file_path)} is {_fsize_mb:.0f} MB.\n\n"
                    "Loading uses memory-mapped I/O to minimise RAM usage, "
                    "but parsing may take 30–60 seconds for very large archives.\n\n"
                    "Continue?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
                if reply != QMessageBox.StandardButton.Yes:
                    return False

            # Show busy cursor for large files
            if _fsize > 32 * 1024 * 1024:
                from PyQt6.QtWidgets import QApplication
                from PyQt6.QtCore import Qt
                QApplication.setOverrideCursor(Qt.CursorShape.WaitCursor)

            col_file = COLFile(debug=(_fsize_mb > 64))
            try:
                if not col_file.load(file_path):
                    return False
            finally:
                if _fsize > 32 * 1024 * 1024:
                    QApplication.restoreOverrideCursor()

            # Store loaded file
            self.current_col_file = col_file
            self.current_file_path = file_path

            # Update window title with model count
            model_count = len(col_file.models) if hasattr(col_file, 'models') else 0
            version_str = f"COL ({model_count} models)"
            self.setWindowTitle(f"{App_name} - {os.path.basename(file_path)} - {version_str}")

            # Populate UI — compact view is default, also populate detail table
            self._populate_compact_col_list()
            self._populate_collision_list()

            # Select first model by default
            active_list = (self.col_compact_list
                          if self._col_view_mode == 'detail'
                          else self.collision_list)
            if active_list.rowCount() > 0:
                active_list.selectRow(0)
                self._select_model_by_row(0)


            # Enable all buttons that require a loaded file
            # Transform buttons: use helper to cover BOTH icon and text panels
            self._set_col_buttons_enabled(True)
            for btn_name in [
                'save_btn', 'save_col_btn', 'saveall_btn',
                'export_col_btn', 'export_all_btn', 'export_btn',
                'import_btn', 'undo_btn', 'undo_col_btn',
                'create_surface_btn', 'paste_btn',
            ]:
                btn = getattr(self, btn_name, None)
                if btn:
                    btn.setEnabled(True)


            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Loaded COL: {os.path.basename(file_path)} ({model_count} models)")
            damaged = getattr(self.current_col_file, 'damaged_records', 0)
            if damaged:
                self._set_status(f"{damaged} damaged record(s) skipped while loading")

            print(f"Opened COL file: {file_path} with {model_count} models")
            return True

        except Exception as e:
            print(f"Error opening COL file: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to open COL file:\n{str(e)}")
            return False

    def _pick_col_from_current_img(self): #vers 1
        """Pick a COL entry from the IMG currently loaded in IMG Factory and open it."""
        try:
            # Get the main IMG Factory window and its loaded IMG
            mw = self.main_window
            img = getattr(mw, 'current_img', None) if mw else None

            if not img or not getattr(img, 'entries', None):
                QMessageBox.information(self, "No IMG Loaded",
                    "No IMG archive is currently open in IMG Factory.\n"
                    "Open an IMG file first, then use From IMG.")
                return

            col_entries = [e for e in img.entries
                           if getattr(e, 'name', '').lower().endswith('.col')]

            if not col_entries:
                QMessageBox.information(self, "No COL Entries",
                    f"No .col entries found in {os.path.basename(img.file_path)}.")
                return

            # Show a picker dialog
            from PyQt6.QtWidgets import QDialog, QListWidget, QDialogButtonBox, QVBoxLayout, QLabel
            dlg = QDialog(self)
            dlg.setWindowTitle(f"Pick COL — {os.path.basename(img.file_path)}")
            dlg.setMinimumSize(320, 400)
            v = QVBoxLayout(dlg)
            v.addWidget(QLabel(f"{len(col_entries)} COL entries in {os.path.basename(img.file_path)}:"))
            lst = QListWidget()
            for e in col_entries:
                lst.addItem(e.name)
            lst.setCurrentRow(0)
            v.addWidget(lst)
            btns = QDialogButtonBox(
                QDialogButtonBox.StandardButton.Open |
                QDialogButtonBox.StandardButton.Cancel)
            btns.accepted.connect(dlg.accept)
            btns.rejected.connect(dlg.reject)
            v.addWidget(btns)
            lst.doubleClicked.connect(dlg.accept)

            if dlg.exec() != QDialog.DialogCode.Accepted:
                return

            row = lst.currentRow()
            if row < 0:
                return

            entry = col_entries[row]
            self._open_col_from_img_entry(img, entry)

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to pick COL from IMG:\n{e}")

    def _open_col_from_img_entry(self, img, entry): #vers 1
        """Extract a COL entry from an IMGFile and load it into the workshop."""
        try:
            import tempfile
            data = img.read_entry_data(entry)
            if not data:
                QMessageBox.warning(self, "Extract Failed",
                    f"Could not extract {entry.name} from IMG.")
                return

            stem = os.path.splitext(entry.name)[0]
            tmp = tempfile.NamedTemporaryFile(
                delete=False, suffix='.col', prefix=stem + '_')
            tmp.write(data)
            tmp.close()

            self.open_col_file(tmp.name)
            # Retitle with original name
            self.setWindowTitle(
                f"COL Workshop — {entry.name} (from {os.path.basename(img.file_path)})")
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"COL Workshop: opened {entry.name} from {os.path.basename(img.file_path)}")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to open {entry.name}:\n{e}")

    def open_img_archive(self): #vers 1
        """Open file dialog to select an IMG archive and load COL entries from it"""
        try:
            file_path, _ = QFileDialog.getOpenFileName(
                self,
                "Open IMG Archive",
                "",
                "IMG Archives (*.img);;All Files (*)"
            )
            if file_path:
                self.load_from_img_archive(file_path)
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to open IMG:\n{str(e)}")

    def load_from_img_archive(self, img_path): #vers 2
        """Load all COL entries from an IMG archive and populate the collision list"""
        try:
            from apps.methods.img_core_classes import IMGFile
            from apps.methods.col_workshop_loader import COLFile

            img = IMGFile(img_path)
            img.open()
            self.current_img = img

            img_name = os.path.basename(img_path)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Scanning {img_name} for COL entries...")

            col_entries = [e for e in img.entries
                           if getattr(e, 'name', '').lower().endswith('.col')]

            if not col_entries:
                QMessageBox.information(self, "No COL Files",
                    f"No .col entries found in {img_name}")
                return False

            # Populate the compact left-panel list with COL entries
            # col_compact_list is the QTableWidget visible in default view
            if hasattr(self, 'col_compact_list'):
                tbl = self.col_compact_list
                tbl.setRowCount(0)
                for i, entry in enumerate(col_entries):
                    tbl.insertRow(i)
                    cell = QTableWidgetItem(entry.name)
                    cell.setData(Qt.ItemDataRole.UserRole, entry)
                    tbl.setItem(i, 0, cell)
                    # Blank detail cell so delegate doesn't crash
                    tbl.setItem(i, 1, QTableWidgetItem(""))
                    tbl.setRowHeight(i, 22)
                self._img_col_entries = col_entries   # store for selection handler

            count = len(col_entries)
            hdr = getattr(self, '_col_list_header', None)
            if hdr:
                hdr.setText(f"COL Files  ({count})")
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Loaded {count} COL entr{'ies' if count != 1 else 'y'} from {img_name}")

            self.setWindowTitle(f"COL Workshop: {img_name}")
            return True

        except Exception as e:
            print(f"Error loading from IMG archive: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to load from IMG:\n{str(e)}")
            return False

    def _analyze_collision(self): #vers 2
        """Analyze current COL file"""
        try:
            if not self.current_col_file or not self.current_file_path:
                QMessageBox.warning(self, "Analyze", "No COL file loaded to analyze")
                return

            # Import analysis functions
            from apps.methods.col_operations import get_col_detailed_analysis
            from apps.gui.col_dialogs import show_col_analysis_dialog

            # Get detailed analysis
            analysis_data = get_col_detailed_analysis(self.current_file_path)

            if 'error' in analysis_data:
                QMessageBox.warning(self, "Analysis Error", f"Analysis failed:\n{analysis_data['error']}")
                return

            # Show analysis dialog
            show_col_analysis_dialog(self, analysis_data, os.path.basename(self.current_file_path))

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Analyzed COL: {os.path.basename(self.current_file_path)}")

        except Exception as e:
            print(f"Error analyzing file: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to analyze file:\n{str(e)}")

    def _rename_col_model(self, model, row): #vers 2
        """Rename a collision model entry in the list."""
        try:
            from PyQt6.QtWidgets import QInputDialog
            old_name = getattr(model, 'name', f'Model_{row}')
            new_name, ok = QInputDialog.getText(
                self, "Rename Model", "New model name:", text=old_name)
            if not ok or not new_name.strip():
                return
            new_name = new_name.strip()
            model.name = new_name
            # Update table cell
            name_item = self.collision_list.item(row, 0)
            if name_item:
                name_item.setText(new_name)
            # Mark file as modified
            if hasattr(self, 'save_btn'):
                self.save_btn.setEnabled(True)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Renamed model {row}: '{old_name}' to '{new_name}'")
        except Exception as e:
            QMessageBox.critical(self, "Rename Error", str(e))

    def _export_col_model(self, model, row): #vers 2
        """Export a single collision model as a standalone COL file."""
        try:
            model_name = getattr(model, 'name', f'model_{row}')
            default_name = model_name.lower().replace(' ', '_') + '.col'
            file_path, _ = QFileDialog.getSaveFileName(
                self, "Export COL Model",
                default_name, "COL Files (*.col);;All Files (*)")
            if not file_path:
                return

            # A COL file with just this model: its original record (edited values patched
            # in) or a freshly written one - never the old lossy COLWriter.
            from apps.methods.col_workshop_parser import COLWriter
            from apps.methods.col_splice import build_col_bytes
            data = build_col_bytes([model], COLWriter, {"head": [], "tail": b""})
            from apps.methods.file_backup import safe_write_bytes
            safe_write_bytes(file_path, data)

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Exported model '{model_name}' to {os.path.basename(file_path)}")
            QMessageBox.information(self, "Export OK",
                f"Model '{model_name}' exported to:\n{file_path}")
        except Exception as e:
            QMessageBox.critical(self, "Export Error", str(e))

    def _import_replace_col_model(self, row): #vers 2
        """Replace a collision model entry from an external COL file."""
        try:
            file_path, _ = QFileDialog.getOpenFileName(
                self, "Import COL to Replace Model",
                "", "COL Files (*.col);;All Files (*)")
            if not file_path:
                return

            from apps.methods.col_workshop_loader import COLFile
            new_col = COLFile()
            if not new_col.load(file_path):
                QMessageBox.warning(self, "Import Failed",
                    f"Could not load {os.path.basename(file_path)}")
                return

            new_models = getattr(new_col, 'models', [])
            if not new_models:
                QMessageBox.warning(self, "No Models",
                    f"No collision models found in {os.path.basename(file_path)}")
                return

            # If multiple models in the source, ask which one to use
            src_model = new_models[0]
            if len(new_models) > 1:
                from PyQt6.QtWidgets import QInputDialog
                names = [f"[{i}] {getattr(m,'name',f'Model_{i}')}"
                         for i, m in enumerate(new_models)]
                choice, ok = QInputDialog.getItem(
                    self, "Choose Model",
                    f"{len(new_models)} models in file — pick one to import:",
                    names, 0, False)
                if not ok:
                    return
                idx = names.index(choice)
                src_model = new_models[idx]

            old_name = getattr(
                self.current_col_file.models[row], 'name', f'Model_{row}')
            src_model.name = old_name  # preserve existing name
            self.current_col_file.models[row] = src_model

            # Refresh the table row
            self._populate_collision_list()
            self._populate_compact_col_list()
            self.collision_list.selectRow(row)

            if hasattr(self, 'save_btn'):
                self.save_btn.setEnabled(True)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Replaced model {row} ('{old_name}') from "
                    f"{os.path.basename(file_path)}")
        except Exception as e:
            QMessageBox.critical(self, "Import Error", str(e))

    def _open_surface_type_dialog(self): #vers 2
        """Show surface material type picker for selected model."""
        model = self._get_selected_model()   # visible list, delegate-safe
        if model is None: return
        idx = self.current_col_file.models.index(model)
        model = self.current_col_file.models[idx]
        types = {0:"Default",1:"Tarmac",2:"Gravel",3:"Grass",4:"Sand",5:"Water",
                 6:"Metal",7:"Wood",8:"Concrete",63:"Obstacle"}
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QListWidget, QDialogButtonBox
        dlg = QDialog(self); dlg.setWindowTitle(f"Surface Type — {model.name}")
        lay = QVBoxLayout(dlg)
        lst = QListWidget()
        for k,v in types.items(): lst.addItem(f"{k:3d}  {v}")
        lay.addWidget(lst)
        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept); btns.rejected.connect(dlg.reject)
        lay.addWidget(btns)
        dlg.exec()

    def _open_surface_edit_dialog(self): #vers 2
        """Open the COL Mesh Editor for the currently selected model."""
        try:
            from apps.components.Col_Editor.col_mesh_editor import open_col_mesh_editor
            open_col_mesh_editor(self, parent=self)
        except Exception as e:
            import traceback; traceback.print_exc()
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Mesh Editor Error", str(e))

    def _show_shadow_mesh(self): #vers 3
        """Toggle the shadow mesh overlay in the viewport."""
        from PyQt6.QtWidgets import QMessageBox
        model = self._get_selected_model()
        if not model:
            QMessageBox.warning(self, "No Selection", "Select a collision model first.")
            return
        pw = self.preview_widget
        if not model.shadow_faces:
            pw.set_show_shadow(False)
            self._set_status(f"'{model.name}' has no shadow mesh (COL3 only) - use Create Shadow Mesh")
            return
        pw.set_show_shadow(not pw._show_shadow)
        state = "shown" if pw._show_shadow else "hidden"
        self._set_status(f"Shadow mesh {state}: {model.name} - "
                         f"{len(model.shadow_vertices)}V {len(model.shadow_faces)}F")

    def _compress_surface(self, *_, **__): return self._compress_col()  #vers 1

    def _copy_surface(self, *_, **__): return self._copy_model_to_clipboard()  #vers 1

    def _delete_surface(self, *_, **__): return self._delete_selected_model()  #vers 1

    def _duplicate_surface(self, *_, **__): return self._duplicate_selected_model()  #vers 1

    def _force_save_col(self, *_, **__): return self._save_file()  #vers 1

    def _import_selected(self, *_, **__): #vers 2
        """Replace the selected model from a COL file."""
        model = self._get_selected_model()
        if model is None:
            QMessageBox.information(self, "Import", "Select a collision model first.")
            return
        self._import_replace_col_model(self.current_col_file.models.index(model))

    def _import_surface(self, *_, **__): return self._import_col_data()  #vers 1

    def _open_col_file(self, *_, **__): return self._open_file()  #vers 1


    def _paste_surface(self, *_, **__): return self._paste_model_from_clipboard()  #vers 1

    def _remove_shadow(self, *_, **__): return self._remove_shadow_mesh()  #vers 1

    def _save_as_col_file(self, *_, **__): return self._save_file_as()  #vers 2

    def _save_col_file(self, *_, **__): return self._save_file()  #vers 1

    def _saveall_file(self, *_, **__): return self._save_file()  #vers 1

    def _uncompress_surface(self, *_, **__): return self._uncompress_col()  #vers 1

    def export_all(self, *_, **__): return self._export_col_data()  #vers 1

    def export_all_surfaces(self, *_, **__): return self._export_col_data()  #vers 1

    def export_selected(self, *_, **__): #vers 2
        """Export the selected model as its own COL file."""
        model = self._get_selected_model()
        if model is None:
            QMessageBox.information(self, "Export", "Select a collision model first.")
            return
        self._export_col_model(model, self.current_col_file.models.index(model))

    def export_selected_surface(self, *_, **__): return self.export_selected()  #vers 2

    def save_col_file(self, *a, **kw): return self._save_file(*a, **kw)  #vers 1

    def shadow_dialog(self, *_, **__): return self._create_shadow_mesh()  #vers 2

    def _change_format(self, text): #vers 2
        """Format combo sets the default export format."""
        self.default_export_format = text

    def _apply_name_edit(self): #vers 1
        """Commit name field edit to the selected model."""
        self.info_name.setReadOnly(True)
        model = self._get_selected_model()
        new_name = self.info_name.text().strip()[:22]
        if model is None or not new_name or new_name == model.name:
            return
        idx = self.current_col_file.models.index(model)
        self._push_undo(idx)
        old_name = model.name
        model.name = new_name
        self._populate_collision_list()
        self._select_model_by_row(idx)
        if hasattr(self, 'save_btn'):
            self.save_btn.setEnabled(True)
        self._set_status(f"Renamed {old_name} to {new_name} - not saved yet")


    def dragEnterEvent(self, event): #vers 1
        """Accept .col and .img files."""
        if self._dropped_files(event):
            event.acceptProposedAction()
        else:
            event.ignore()

    def dragMoveEvent(self, event): #vers 1
        """Keep accepting while over the workshop."""
        self.dragEnterEvent(event)

    def dropEvent(self, event): #vers 3
        """Drop .col/.img: empty workshop opens it; loaded one asks add/new tab."""
        import sys
        open_col_workshop = sys.modules[type(self).__module__].open_col_workshop  # avoids circular import
        paths = self._dropped_files(event)
        if not paths:
            event.ignore()
            return
        event.acceptProposedAction()
        loaded = bool(getattr(self.current_col_file, 'models', None))
        tw = getattr(self.main_window, 'main_tab_widget', None)
        if loaded:
            from PyQt6.QtWidgets import QMessageBox
            cols = [p for p in paths if p.lower().endswith('.col')]
            names = ", ".join(os.path.basename(p) for p in paths[:3]) + (" ..." if len(paths) > 3 else "")
            box = QMessageBox(self)
            box.setWindowTitle("Dropped COL")
            box.setText(f"{names}\n\nAdd to the open file, or open in a new tab?")
            add_btn = box.addButton("Add to current", QMessageBox.ButtonRole.AcceptRole) if cols else None
            new_btn = box.addButton("Open in new tab", QMessageBox.ButtonRole.ActionRole) if tw is not None else None
            box.addButton(QMessageBox.StandardButton.Cancel)
            box.exec()
            clicked = box.clickedButton()
            if add_btn is not None and clicked is add_btn:
                self._add_models_from_files(cols)
                for path in paths:
                    if path.lower().endswith('.img') and tw is not None:
                        open_col_workshop(self.main_window, path)
            elif new_btn is not None and clicked is new_btn:
                for path in paths:
                    open_col_workshop(self.main_window, path)
            return
        first, rest = paths[0], paths[1:]
        if first.lower().endswith('.img'):
            self.load_from_img_archive(first)
        else:
            self.open_col_file(first)
        if tw is not None and tw.indexOf(self.parentWidget()) >= 0:
            tw.setTabText(tw.indexOf(self.parentWidget()), os.path.splitext(os.path.basename(first))[0])
        if rest and tw is not None:
            for path in rest:
                open_col_workshop(self.main_window, path)

    def showEvent(self, event): #vers 1
        """When COL workshop becomes visible, try to populate from loaded IMG."""
        super().showEvent(event)
        if (not self.standalone_mode and
                hasattr(self, 'col_list_widget') and
                self.col_list_widget is not None and
                self.col_list_widget.count() == 0):
            # Try to get current IMG from main window
            if self.main_window and hasattr(self.main_window, 'current_img'):
                img = self.main_window.current_img
                if img and img != getattr(self, 'current_img', None):
                    self.current_img = img
                    self._load_img_col_list()
