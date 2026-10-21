#this belongs in apps/components/Model_Editor/depends/model_core_logic_func.py - Version: 8
# X-Seti - October 12 2026 - IMG Factory 1.6 - Model Workshop file logic

"""
Open, import, export, save, IDE/IMG links and COL data for ModelWorkshop.
"""

##class ModelCoreLogicMixin: -
# _analyze_collision
# _batch_convert
# _convert_surface
# _copy_surface
# _create_new_surface
# _delete_surface
# _dff_to_col_surfaces
# _doc_current_path
# _doc_has_file
# _doc_new
# _doc_new_window
# _doc_open
# _doc_reset
# _doc_restore
# _doc_state
# _duplicate_surface
# export_all
# export_all_surfaces
# _export_col_data
# _export_col_model
# _export_dff_obj
# _export_model_menu
# _export_not_implemented
# export_selected
# export_selected_surface
# _export_via_ide
# _extract_col_from_img
# _find_in_ide
# _force_save_col
# _get_ide_db
# _get_xref
# _import_col_data
# _import_model
# _import_obj
# _import_replace_col_model
# _import_surface
# _import_via_ide
# load_from_img_archive
# _load_game_model_list
# _load_img_col_list
# _lookup_ide_for_dff
# _lookup_ide_from_db
# _model_item_entry
# open_col_file
# _open_col_file
# _open_col_from_img_entry
# open_dff_file
# _open_dff_standalone
# _open_file
# _open_path
# open_img_archive
# _open_imgs
# _open_mipmap_manager
# _paste_surface
# _pick_col_from_current_img
# _populate_left_panel_from_img
# _populate_left_panel_from_open_imgs
# refresh
# _reload_surface_table
# reload_surface_table
# _remove_via_ide
# _rename_col_model
# _rename_shadow_shortcut
# _save_as_col_file
# _save_col_file
# save_col_file
# _save_file
# _save_file_as
# _save_in_place
# _save_model_as
# _saveall_file
# _show_dff
# _show_shadow_mesh
# _show_surface_info
# _sync_open_imgs

import os
from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QAbstractItemView, QApplication, QComboBox, QDialog, QFileDialog, QHBoxLayout, QLabel, QLineEdit, QListWidget, QListWidgetItem, QMenu, QMessageBox, QPushButton, QTableWidget, QTableWidgetItem, QVBoxLayout
from apps.components.Model_Editor.depends.model_setup_ui_func import App_name

class ModelCoreLogicMixin: #vers 1
    """Open, import, export, save, IDE/IMG links and COL data for ModelWorkshop."""


    def _create_new_surface(self): #vers 1
        """Add a new empty COL model to the loaded file."""
        if not getattr(self, "current_col_file", None):
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
        active = (self.mod_compact_list
                  if self._col_view_mode == 'detail' else self.collision_list)
        if active.rowCount() > new_row:
            active.selectRow(new_row)


    def _dff_to_col_surfaces(self, single=True): #vers 2
        """Generate COL from DFF model — maps texture names to COL surface types.
        single=True: one COL model from currently loaded DFF.
        single=False: batch — pick a directory of DFF files.
        """
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QLabel,
            QTableWidget, QTableWidgetItem, QComboBox, QPushButton, QFileDialog,
            QMessageBox, QProgressDialog, QCheckBox, QHeaderView, QAbstractItemView)
        from PyQt6.QtCore import Qt

        # - GTA surface type map
        # texture name keyword -> (surface_id, label)
        SURFACE_MAP = [
            # Roads / Paths
            (["road","tarmac","asphalt","highway","freeway","motorway"], (0,  "Default/Road")),
            (["concrete","cement","pavement","sidewalk","curb","kerb"],  (2,  "Concrete")),
            (["sand","dirt","gravel","earth","soil","dust"],             (3,  "Sand")),
            (["grass","lawn","turf","verge","meadow"],                   (4,  "Grass")),
            (["water","sea","ocean","river","lake","pool"],              (5,  "Water")),
            (["wood","plank","board","timber","pier","crate","box"],     (6,  "Wood")),
            (["metal","steel","iron","tin","sheet","corrugated"],        (7,  "Metal")),
            (["grate","grid","mesh","fence","wire"],                     (8,  "Metal (grate)")),
            (["roof","tile","slate","shingle"],                          (9,  "Roof tile")),
            (["glass","window","pane","windscreen"],                     (10, "Glass")),
            (["mud","swamp","marsh"],                                    (11, "Mud")),
            (["rubber","tyre","tire","wheel"],                           (12, "Rubber")),
            (["plastic","pvc","fibreglass","fibreglass"],                (13, "Plastic")),
            (["rock","stone","boulder","cliff","granite","limestone"],   (14, "Rock")),
            (["marble","polished","floor","tile"],                       (15, "Marble")),
            (["carpet","rug","mat"],                                     (16, "Carpet")),
            (["leaves","bush","shrub","hedge","foliage","plant"],       (17, "Foliage")),
            (["snow","ice","frost","frozen"],                            (20, "Snow/Ice")),
            (["wood_chips","sawdust","bark"],                            (21, "Wood chips")),
        ]

        def _guess_surface(tex_name: str) -> int:  #vers 1
            low = tex_name.lower()
            for keywords, (sid, _) in SURFACE_MAP:
                if any(kw in low for kw in keywords):
                    return sid
            return 0  # default

        def _surface_label(sid: int) -> str:  #vers 2
            for _, (s, lbl) in SURFACE_MAP:
                if s == sid:
                    return lbl
            return f"Surface {sid}"

        # - Collect DFF files
        dff_paths = []
        if single:
            # Use currently loaded DFF if available
            cur = getattr(self, '_current_dff_path', None)
            if not cur:
                cur, _ = QFileDialog.getOpenFileName(
                    self, "Select DFF File", "", "DFF Files (*.dff);;All Files (*)")
            if cur:
                dff_paths = [cur]
        else:
            # Batch: pick directory
            d = QFileDialog.getExistingDirectory(self, "Select DFF Directory")
            if d:
                import os
                dff_paths = [os.path.join(d, f) for f in os.listdir(d)
                             if f.lower().endswith('.dff')]
                dff_paths.sort()

        if not dff_paths:
            return

        # - Parse DFFs and collect material info
        from apps.methods.dff_parser import DFFParser
        all_models = []  # list of (dff_basename, [(tex_name, surface_id), ...], geom)

        prog = QProgressDialog("Parsing DFF files…", "Cancel", 0, len(dff_paths), self)
        prog.setWindowModality(Qt.WindowModality.WindowModal)
        for i, path in enumerate(dff_paths):
            prog.setValue(i)
            if prog.wasCanceled():
                return
            try:
                import os
                p = DFFParser(path)
                p.parse()
                for geom in p.geometries:
                    mats = []
                    for mat in geom.materials:
                        tex = getattr(mat, 'texture_name', '') or ''
                        sid = _guess_surface(tex)
                        mats.append([tex, sid])
                    all_models.append((os.path.basename(path), mats, geom))
            except Exception as e:
                print(f"[DFF→COL] {path}: {e}")
        prog.setValue(len(dff_paths))

        if not all_models:
            QMessageBox.warning(self, "No Models", "No valid DFF models found.")
            return

        # - Surface assignment dialog
        dlg = QDialog(self)
        dlg.setWindowTitle(f"DFF → COL Surface Assignment ({len(all_models)} model(s))")
        dlg.resize(700, 500)
        lo = QVBoxLayout(dlg)

        lo.addWidget(QLabel(
            "Texture names have been matched to GTA surface types. "
            "Adjust any assignments before generating the COL."))

        # Table: DFF name | Texture | Surface ID | Label
        tbl = QTableWidget(0, 4)
        tbl.setHorizontalHeaderLabels(["Model", "Texture", "Surface ID", "Surface Type"])
        tbl.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        tbl.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        lo.addWidget(tbl, 1)

        row_data = []  # (model_name, tex_name, surface_id_ref, geom)
        for model_name, mats, geom in all_models:
            for tex, sid in mats:
                row = tbl.rowCount(); tbl.insertRow(row)
                tbl.setItem(row, 0, QTableWidgetItem(model_name))
                tbl.setItem(row, 1, QTableWidgetItem(tex or "(no texture)"))
                sp = QComboBox()
                for _, (s_id, s_lbl) in SURFACE_MAP:
                    sp.addItem(f"{s_id} — {s_lbl}", s_id)
                # Select current
                for i in range(sp.count()):
                    if sp.itemData(i) == sid:
                        sp.setCurrentIndex(i); break
                tbl.setCellWidget(row, 2, sp)
                tbl.setItem(row, 3, QTableWidgetItem(_surface_label(sid)))
                sp.currentIndexChanged.connect(
                    lambda _, r=row, s=sp: tbl.item(r, 3).setText(
                        _surface_label(s.currentData())))
                row_data.append((model_name, tex, sp, geom))

        # Options
        opts_row = QHBoxLayout()
        col_version = QComboBox()
        col_version.addItems(["COL 1 (GTA III/VC)", "COL 2 (GTA SA)", "COL 3 (GTA SA extended)"])
        col_version.setCurrentIndex(1)
        opts_row.addWidget(QLabel("COL version:")); opts_row.addWidget(col_version)
        use_mesh = QCheckBox("Include mesh faces")
        use_mesh.setChecked(True)
        opts_row.addWidget(use_mesh)
        opts_row.addStretch()
        lo.addLayout(opts_row)

        btn_row = QHBoxLayout()
        ok_btn = QPushButton("Generate COL"); ok_btn.setDefault(True)
        cancel_btn = QPushButton("Cancel")
        ok_btn.clicked.connect(dlg.accept); cancel_btn.clicked.connect(dlg.reject)
        btn_row.addStretch(); btn_row.addWidget(ok_btn); btn_row.addWidget(cancel_btn)
        lo.addLayout(btn_row)

        if dlg.exec() != QDialog.DialogCode.Accepted:
            return

        # - Build COL models
        from apps.methods.col_workshop_loader import COLFile
        from apps.methods.col_workshop_classes import (
            COLModel, COLHeader, COLVersion, COLBounds, COLFace, COLVertex)
        from apps.methods.col_workshop_classes import Vector3
        import os

        col_ver_map = [COLVersion.COL_1, COLVersion.COL_2, COLVersion.COL_3]
        col_ver = col_ver_map[col_version.currentIndex()]
        fourcc = {COLVersion.COL_1: b'COLL', COLVersion.COL_2: b'COL2',
                  COLVersion.COL_3: b'COL3'}[col_ver]

        if not getattr(self, "current_col_file", None):
            self.current_col_file = COLFile()
            self.current_col_file.models = []

        added = 0
        for model_name, tex_name, sp_widget, geom in row_data:
            surface_id = sp_widget.currentData() or 0
            bs = getattr(geom, 'bounding_sphere', None)
            r = float(getattr(bs, 'radius', 0.0) or 1.73)
            c = getattr(bs, 'center', None)
            cx, cy, cz = (c.x, c.y, c.z) if c is not None else (0.0, 0.0, 0.0)
            hdr = COLHeader(fourcc=fourcc, size=0, name=os.path.splitext(model_name)[0][:21],
                            model_id=0, version=col_ver)
            bnd = COLBounds(radius=r, center=Vector3(cx, cy, cz),
                            min=Vector3(cx - r, cy - r, cz - r), max=Vector3(cx + r, cy + r, cz + r))
            verts, faces = [], []
            if use_mesh.isChecked() and getattr(geom, 'vertices', None):
                verts = [COLVertex(v.x, v.y, v.z) for v in geom.vertices]
                faces = [COLFace(t.v1, t.v2, t.v3, surface_id, 0, 0, 0)
                         for t in getattr(geom, 'triangles', [])]
            m = COLModel(header=hdr, bounds=bnd, spheres=[], boxes=[],
                         vertices=verts, faces=faces)
            self.current_col_file.models.append(m)
            added += 1

        self._populate_collision_list()
        self._populate_compact_col_list()
        msg = (f"Generated {added} COL model(s) from "
               f"{'DFF' if single else str(len(dff_paths)) + ' DFFs'}")
        self._set_status(msg)
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(msg)



    def _convert_surface(self): #vers 2
        """Convert selected model to a different COL version."""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QComboBox, QPushButton, QMessageBox
        from apps.methods.col_workshop_classes import COLVersion
        model = self._get_selected_model()
        if not model:
            QMessageBox.warning(self, "No Selection", "Select a collision model first.")
            return
        current = getattr(model.version, 'name', str(model.version))
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Convert COL version — {model.name}")
        dlg.setFixedSize(300, 130)
        lay = QVBoxLayout(dlg)
        lay.addWidget(QLabel(f"Current version: <b>{current}</b>"))
        lay.addWidget(QLabel("Convert to:"))
        combo = QComboBox()
        ver_map = {'COL_1': COLVersion.COL_1, 'COL_2': COLVersion.COL_2, 'COL_3': COLVersion.COL_3}
        [combo.addItem(k) for k in ver_map if k != current]
        lay.addWidget(combo)
        btns = QHBoxLayout()
        ok = QPushButton("Convert"); cancel = QPushButton("Cancel")
        btns.addStretch(); btns.addWidget(ok); btns.addWidget(cancel)
        lay.addLayout(btns)
        cancel.clicked.connect(dlg.reject)
        def _do():  #vers 1
            model.version = ver_map[combo.currentText()]
            self._populate_collision_list()
            self._populate_compact_col_list()
            self._set_status(f"Converted {model.name} to {combo.currentText()}")
            dlg.accept()
        ok.clicked.connect(_do)
        dlg.exec()


    def _show_shadow_mesh(self): #vers 3
        """Show shadow mesh info for selected model."""
        from PyQt6.QtWidgets import QMessageBox
        model = self._get_selected_model()
        if not model:
            QMessageBox.warning(self, "No Selection", "Select a collision model first.")
            return
        sv = len(getattr(model, 'shadow_vertices', []))
        sf = len(getattr(model, 'shadow_faces', []))
        if sv == 0 and sf == 0:
            QMessageBox.information(self, "Shadow Mesh",
                f"'{model.name}' has no shadow mesh data.\n\n"
                "COL3+ models can have a separate low-poly shadow collision mesh.")
        else:
            QMessageBox.information(self, "Shadow Mesh",
                f"Model: {model.name}\n"
                f"Shadow vertices: {sv}\n"
                f"Shadow faces:    {sf}\n\n"
                "Shadow mesh is included in COL3 export.")


    # - Method aliases — drop Qt signal args (*_, **__) before forwarding
    # These are called from Qt signals that pass e.g. bool(checked) as arg.
    # Target methods take only self, so we must not forward the signal args.

    def _copy_surface(self, *_, **__): #vers 1
        return self._copy_model_to_clipboard()

    def _delete_surface(self, *_, **__): #vers 1
        return self._delete_selected_model()

    def _duplicate_surface(self, *_, **__): #vers 1
        return self._duplicate_selected_model()

    def _force_save_col(self, *_, **__): #vers 1
        return self._save_file()


    def _import_surface(self, *_, **__): #vers 1
        return self._import_col_data()

    def _open_col_file(self, *_, **__): #vers 1
        return self._open_file()

    def _open_mipmap_manager(self, *_, **__): #vers 1
        return self._show_shadow_mesh()

    def _paste_surface(self, *_, **__): #vers 1
        return self._paste_model_from_clipboard()

    def _reload_surface_table(self, *_, **__): #vers 1
        return self._populate_collision_list()


    def _save_as_col_file(self, *_, **__): #vers 1
        return self._save_file()

    def _save_col_file(self, *_, **__): #vers 1
        return self._save_file()

    def _saveall_file(self, *_, **__): #vers 1
        return self._save_file()


    def export_all(self, *_, **__): #vers 1
        return self._export_col_data()

    def export_all_surfaces(self, *_, **__): #vers 1
        return self._export_col_data()

    def export_selected(self, *_, **__): #vers 1
        return self._export_col_data()

    def export_selected_surface(self, *_, **__): #vers 1
        return self._export_col_data()

    def refresh(self, *_, **__): #vers 1
        return self._populate_collision_list()

    def reload_surface_table(self, *_, **__): #vers 1
        return self._populate_collision_list()

    def save_col_file(self, *a, **kw): #vers 1
        return self._save_file(*a, **kw)
    def _rename_shadow_shortcut(self, *a, **kw): pass  #vers 1
    def _show_surface_info(self, *a, **kw): pass  #vers 1


    def _load_img_col_list(self): #vers 2
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

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"📋 Found {len(self.txd_list)} COL files")
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error loading COL list: {str(e)}")



# - Marker 7


#------ Col functions

    def _import_model(self): #vers 1
        """Import model from external format (MDL, OBJ, FBX…)."""
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        path, _ = QFileDialog.getOpenFileName(
            self, "Import Model",
            os.path.dirname(getattr(self, '_current_dff_path', '') or ''),
            "Model Files (*.obj *.mdl *.fbx *.3ds *.dae);;OBJ (*.obj);;MDL (*.mdl);;All Files (*)")
        if not path:
            return
        ext = os.path.splitext(path)[1].lower()
        if ext == '.obj':
            self._import_obj(path)
        else:
            QMessageBox.information(self, "Import",
                f"Import of {ext.upper()} format is not yet implemented.\n"
                f"Supported: OBJ")

    def _import_obj(self, path: str): #vers 4
        """Open a Wavefront OBJ as a model (one geometry per object / group)."""
        from apps.methods.model_convert import obj_to_model
        try:
            model = obj_to_model(path)
        except Exception as e:
            QMessageBox.critical(self, "OBJ Import Error", str(e))
            return
        if model is None:
            QMessageBox.warning(self, "OBJ Import", "No geometry found in OBJ file.")
            return
        self.current_col_file = None
        self._show_dff(path, model)
        self._set_status(f"OBJ opened: {os.path.basename(path)} - Save As to write DFF")

    def _export_model_menu(self): #vers 2
        """Show export format menu — DFF geometry + COL + other formats."""
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        menu.addSection("Geometry")
        menu.addAction("Export as OBJ (Wavefront)",    self._export_dff_obj)
        menu.addAction("Export as 3DS (3D Studio)",
            lambda: self._export_not_implemented("3DS"))
        menu.addAction("Export as FBX",
            lambda: self._export_not_implemented("FBX"))
        menu.addSection("Collision")
        menu.addAction("Export as COL (collision)",    self._export_col_data)
        menu.addSection("Other")
        menu.addAction("Export as CST (Crysis)",
            lambda: self._export_not_implemented("CST"))
        menu.addAction("Export as OBS (OpenBVE)",
            lambda: self._export_not_implemented("OBS"))
        # Pop up below whichever button called us
        btn = getattr(self, 'export_col_btn', None)
        if btn:
            menu.exec(btn.mapToGlobal(btn.rect().bottomLeft()))
        else:
            menu.exec(self.cursor().pos())

    def _export_not_implemented(self, fmt: str): #vers 1
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(self, f"Export {fmt}",
            f"{fmt} export is not yet implemented.")

    def _open_dff_standalone(self): #vers 6
        """Open DFF + optionally TXD. Remembers last directory."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout,
                                     QLabel, QLineEdit, QPushButton, QCheckBox)

        # Persist last-used directory across calls
        _settings_key = '_mw_last_open_dir'
        start_dir = getattr(self, _settings_key, '') or os.path.dirname(
            getattr(self, '_current_dff_path', '') or '')

        dlg = QDialog(self)
        dlg.setWindowTitle("Open DFF Model")
        dlg.setMinimumWidth(520)
        lay = QVBoxLayout(dlg)
        lay.setSpacing(8)

        def _row(label_text, placeholder):  #vers 1
            row = QHBoxLayout()
            lbl = QLabel(label_text); lbl.setFixedWidth(44)
            edit = QLineEdit(); edit.setPlaceholderText(placeholder)
            btn = QPushButton("…"); btn.setFixedWidth(28)
            row.addWidget(lbl); row.addWidget(edit, 1); row.addWidget(btn)
            lay.addLayout(row)
            return edit, btn

        dff_edit, dff_btn = _row("DFF:", "Select a DFF model file…")
        txd_edit, txd_btn = _row("TXD:", "Select matching TXD (optional)…")

        auto_txd_cb = QCheckBox("Auto-find TXD with same name in same folder")
        auto_txd_cb.setChecked(True)
        lay.addWidget(auto_txd_cb)

        def _pick_dff():  #vers 1
            p, _ = QFileDialog.getOpenFileName(
                dlg, "Open Model", start_dir,
                "Models (*.dff *.mdl *.wdr *.wft *.nif *.obj);;DFF Models (*.dff);;All Files (*)")
            if p:
                dff_edit.setText(p)
                setattr(self, _settings_key, os.path.dirname(p))
                if auto_txd_cb.isChecked():
                    for ext in ('.txd', '.TXD', '.nft', '.wtd', '.chk', '.xtx'):
                        txd_guess = os.path.splitext(p)[0] + ext
                        if os.path.isfile(txd_guess):
                            txd_edit.setText(txd_guess)
                            break

        def _pick_txd():  #vers 2
            start = os.path.dirname(dff_edit.text()) or start_dir
            p, _ = QFileDialog.getOpenFileName(
                dlg, "Open TXD", start, "Texture Files (*.txd *.wtd *.nft *.xtx *.chk);;TXD Files (*.txd);;All Files (*)")
            if p:
                txd_edit.setText(p)

        dff_btn.clicked.connect(_pick_dff)
        txd_btn.clicked.connect(_pick_txd)

        btn_row = QHBoxLayout()
        ok_btn = QPushButton("Open"); ok_btn.setDefault(True)
        cancel_btn = QPushButton("Cancel")
        btn_row.addStretch()
        btn_row.addWidget(ok_btn); btn_row.addWidget(cancel_btn)
        lay.addLayout(btn_row)
        ok_btn.clicked.connect(dlg.accept)
        cancel_btn.clicked.connect(dlg.reject)

        if dlg.exec() != QDialog.DialogCode.Accepted:
            return

        dff_path = dff_edit.text().strip()
        txd_path = txd_edit.text().strip()
        if not dff_path:
            return

        setattr(self, _settings_key, os.path.dirname(dff_path))
        self.open_dff_file(dff_path)   # also runs _lookup_ide_for_dff internally
        if txd_path and os.path.isfile(txd_path):
            self._load_txd_file(txd_path)
        elif auto_txd_cb.isChecked():
            # No same-folder-same-name TXD was found/specified - fall back
            # to searching open IMGs (IDE-linked name via _current_ide_obj,
            # which open_dff_file's _lookup_ide_for_dff call just
            # populated). Loads directly into this workshop's own scene -
            # _open_txd_smart() would be wrong here, it opens the separate
            # TXD Workshop tool instead.
            self._auto_load_txd_from_imgs()


    _DOC_KIND = "Model"
    _DOC_FILTER = ("Model/Collision/Texture Files (*.dff *.mdl *.wdr *.wft *.nif *.obj *.col *.txd *.wtd *.nft *.chk *.xtx *.img);;"
                   "Models (*.dff *.mdl *.wdr *.wft *.nif);;COL Files (*.col);;TXD Files (*.txd);;IMG Archives (*.img);;"
                   "All Files (*)")

    def _open_file(self): #vers 7
        """Open file dialog: models, COL, TXD and IMG."""
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Open Model / Collision / Texture File", "", self._DOC_FILTER)
        if file_path:
            self._open_path(file_path)

    def _open_path(self, file_path): #vers 2
        """Open one model, COL, TXD or IMG path (dialog and drag & drop)."""
        try:
            ext = os.path.splitext(file_path)[1].lower()
            from apps.methods.dff_parser import MODEL_EXTS
            if ext == '.obj':
                self._import_obj(file_path)
            elif ext in MODEL_EXTS:
                self.open_dff_file(file_path)
                # Matching TXD into this scene: IDE name, same folder or open IMGs
                self._auto_load_txd_from_imgs()
            elif ext in ('.txd', '.wtd', '.nft', '.chk', '.xtx'):
                mw = getattr(self, 'main_window', None)
                if mw and hasattr(mw, 'open_txd_workshop_docked'):
                    mw.open_txd_workshop_docked(file_path=file_path)
                else:
                    from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
                    open_txd_workshop(self, file_path)
            elif ext == '.img':
                self.load_from_img_archive(file_path)
            else:
                self.open_col_file(file_path)
        except Exception as e:
            print(f"Error opening file: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to open file:\n{str(e)}")

    def _save_model_as(self): #vers 2
        """Save the open model as DFF (III / VC / SA) or OBJ."""
        from apps.methods.model_convert import CONVERT_TARGETS, TARGETS, save_model
        model = getattr(self, '_current_dff_model', None)
        if model is None:
            QMessageBox.warning(self, "Save As", "Open a model first.")
            return
        src = getattr(self, '_current_dff_path', '') or ''
        base = os.path.splitext(src)[0] if src else os.path.join(os.path.expanduser('~'), 'model')
        filters = [f"{lab} (*{'.obj' if TARGETS[lab][0] == 'obj' else '.dff'})" for lab in CONVERT_TARGETS]
        path, chosen = QFileDialog.getSaveFileName(self, "Save Model As", base + ".dff", ";;".join(filters))
        if not path:
            return
        label = chosen.rsplit(' (', 1)[0] if chosen else CONVERT_TARGETS[0]
        ext = '.obj' if TARGETS[label][0] == 'obj' else '.dff'
        if not path.lower().endswith(ext):
            path += ext
        try:
            from apps.methods.file_backup import backup_file
            if os.path.exists(path):
                backup_file(path)
            report = save_model(model, path, label)
        except Exception as e:
            QMessageBox.critical(self, "Save As", f"{os.path.basename(path)}:\n{e}")
            return
        if ext == '.dff' and TARGETS[label][1] == 0x36003 and not src.lower().endswith('.dff'):
            self._current_dff_path = path                 # now a DFF: Save writes here
            model.source_path, model.raw = path, None
        failed = [r for r in report if r.startswith("not written")]
        self._set_status(f"Saved {os.path.basename(path)} as {label}")
        if failed:
            QMessageBox.information(self, "Save As", "Saved. Not written:\n\n" + "\n".join(failed))

    def _save_in_place(self, model, path): #vers 3
        """Write edits back into the open .nif / .mdl / .wdr / .wft (backup first)."""
        from apps.methods.model_convert import TARGETS, save_model
        from apps.methods.file_backup import backup_file
        kind = os.path.splitext(path)[1].lower()[1:]
        title = f"Save {kind.upper()}"
        try:
            backup_file(path)
            report = save_model(model, path, next(lab for lab, (k, _v) in TARGETS.items() if k == kind))
        except Exception as e:
            QMessageBox.critical(self, title, f"{os.path.basename(path)}:\n{e}")
            return
        self._mesh_dirty = False
        failed = [r for r in report if r.startswith("not written")]
        self._set_status(f"Saved {os.path.basename(path)}: " + ("; ".join(r for r in report if not r.startswith('not')) or "no changes"))
        if failed:
            QMessageBox.information(self, title, "Saved. Not written:\n\n" + "\n".join(failed))

    def _batch_convert(self): #vers 1
        """Batch convert a folder of models (source dir -> dest dir)."""
        from apps.gui.batch_convert_dialog import open_batch_convert
        src = os.path.dirname(getattr(self, '_current_dff_path', '') or '')
        open_batch_convert(self, src)

    #    File tab hooks for DocTabsMixin (apps/methods/doc_tabs.py)

    def _doc_state(self): #vers 1
        """Snapshot of the open file: model / COL, undo, row, textures."""
        lw = getattr(self, 'mod_compact_list', None)
        return {'path': self._doc_current_path(),
                'model': getattr(self, '_current_dff_model', None),
                'dff_path': getattr(self, '_current_dff_path', None),
                'col_file': getattr(self, 'current_col_file', None),
                'col_path': getattr(self, 'current_file_path', None),
                'undo': getattr(self, '_mesh_undo', []), 'dirty': getattr(self, '_mesh_dirty', False),
                'row': max(0, lw.currentRow()) if lw is not None else 0,
                'textures': getattr(self, '_mod_textures', []),
                'txd_path': getattr(self, '_current_txd_path', None),
                'txd_bytes': getattr(self, '_current_txd_bytes', None)}

    def _doc_restore(self, st): #vers 1
        """Show a saved tab state (model or COL, its textures, selected row)."""
        self._mesh_undo, self._mesh_dirty = st['undo'], st['dirty']
        self._mod_textures = st['textures']
        self._current_txd_path, self._current_txd_bytes = st['txd_path'], st['txd_bytes']
        self.current_col_file, self.current_file_path = st['col_file'], st['col_path']
        vp = self.preview_widget
        vp.clear_textures()
        if st['col_file'] is None and st['model'] is not None:
            self._show_dff(st['dff_path'], st['model'])
            if self.mod_compact_list.rowCount() > st['row']:
                self.mod_compact_list.selectRow(st['row'])
        else:
            self._current_dff_model, self._current_dff_path = st['model'], st['dff_path']
            self._populate_compact_col_list()
            self._populate_collision_list()
        self._populate_texture_list()
        if self._mod_textures:
            vp._upload_textures(self._mod_textures)
            vp.set_render_mode('textured')
        vp.update()

    def _doc_open(self, file_path): #vers 1
        """Open into the current (new) tab; True when a model or COL loaded."""
        before = (getattr(self, '_current_dff_model', None), getattr(self, 'current_col_file', None))
        self._open_path(file_path)
        return (getattr(self, '_current_dff_model', None), getattr(self, 'current_col_file', None)) != before

    def _doc_has_file(self): #vers 1
        return bool(getattr(self, '_current_dff_model', None) or getattr(self, 'current_col_file', None))

    def _doc_current_path(self): #vers 1
        if getattr(self, 'current_col_file', None) is not None:     # cleared when a model opens
            return getattr(self, 'current_file_path', None)
        return getattr(self, '_current_dff_path', None)

    def _doc_new(self): #vers 1
        """Fresh per-file state before a new tab opens."""
        self._mesh_undo, self._mesh_dirty = [], False
        self._mod_textures, self._current_txd_path, self._current_txd_bytes = [], None, None
        self._populate_texture_list()
        self.preview_widget.clear_textures()

    def _doc_reset(self): #vers 1
        """Empty workshop after the last tab closes."""
        self._doc_new()
        self._current_dff_model = self._current_dff_path = None
        self.current_col_file = self.current_file_path = None
        self.mod_compact_list.setRowCount(0)
        tree = getattr(self, '_frame_tree', None)
        if tree is not None:
            tree.clear()
        vp = self.preview_widget
        vp.load_all_geometries([], [], [], [])
        vp._triangles = []
        vp.update()
        self.setWindowTitle(App_name)

    def _doc_new_window(self, file_path): #vers 1
        """Docked: open the file in a new IMG Factory tab."""
        import sys
        sys.modules[type(self).__module__].open_model_workshop(self.main_window, file_path)

        # - IDE / TXD linking


    def _get_ide_db(self): #vers 2
        """Return IDEDatabase — prefers the one built by DAT Browser on load
        (stored on main_window.ide_db), falls back to filesystem scan."""
        try:
            from apps.methods.gta_dat_parser import IDEDatabase, GTAGame
        except ImportError:
            return None
        mw = getattr(self,'main_window',None)

        # Primary: DAT Browser already built the DB — use it directly
        mw_db = getattr(mw, 'ide_db', None)
        if mw_db and getattr(mw_db, '_loaded', False) and mw_db.model_map:
            return mw_db

        # Also check DAT Browser widget's own _ide_db
        db_widget = getattr(mw, 'dat_browser', None)
        if db_widget:
            db_from_widget = getattr(db_widget, '_ide_db', None)
            if db_from_widget and db_from_widget.model_map:
                return db_from_widget
        img_path = ''

        # Priority 1: source IMG path stored at workshop-open time
        src_img = getattr(self, '_source_img_path', '') or ''
        if src_img and __import__('os').path.isfile(src_img):
            img_path = src_img

        # Priority 2: current_img on workshop or main_window
        ci = (getattr(self,'current_img',None) or
              (getattr(mw,'current_img',None) if mw else None))
        if ci: img_path = getattr(ci,'file_path','') or ''

        # Priority 3: current DFF path (skip /tmp — extracted DFFs lose context)
        if not img_path:
            _dff = getattr(self,'_current_dff_path','') or ''
            if _dff and not _dff.startswith('/tmp'):
                img_path = _dff

        # Try: left panel selected item (col_list_widget / img entries)
        if not img_path and mw and hasattr(mw,'main_tab_widget'):
            tw = mw.main_tab_widget
            for i in range(tw.count()):
                w = tw.widget(i)
                if w and getattr(w,'file_type','')=='IMG':
                    fp = getattr(w,'file_path','') or getattr(
                        getattr(w,'file_object',None),'file_path','')
                    if fp: img_path=fp; break

        # Try: any open IMG tab
        if not img_path and mw and hasattr(mw,'main_tab_widget'):
            tw = mw.main_tab_widget
            for i in range(tw.count()):
                w = tw.widget(i)
                fp = getattr(w,'file_path','') if w else ''
                if fp and fp.lower().endswith('.img'):
                    img_path=fp; break

        if not img_path:
            return None
        # Walk up until we find a data/ or DATA/ sibling (case-insensitive)
        # Also accept any folder that contains .ide files directly
        candidate = os.path.dirname(img_path)
        game_root = candidate   # default: use img folder
        for _ in range(6):
            try:
                entries = os.listdir(candidate)
                # Case-insensitive match for data/ subfolder
                has_data = any(
                    e.lower() == 'data' and
                    os.path.isdir(os.path.join(candidate, e))
                    for e in entries)
                # Or: folder directly contains .ide files
                has_ide = any(e.lower().endswith('.ide') for e in entries)
                if has_data or has_ide:
                    game_root = candidate; break
            except OSError:
                pass
            parent = os.path.dirname(candidate)
            if parent == candidate:
                break
            candidate = parent
        if getattr(self,'_ide_db_root','')==game_root and self._ide_db:
            return self._ide_db
        db = IDEDatabase(GTAGame.VC)
        loaded = db.load_folder(game_root, recurse=True)

        # If nothing found in game_root, also try the direct IMG folder
        # (handles cases where IDE files sit alongside the IMG)
        if loaded == 0:
            img_dir = os.path.dirname(img_path) if img_path else ''
            if img_dir and img_dir != game_root:
                loaded = db.load_folder(img_dir, recurse=True)
                if loaded > 0:
                    game_root = img_dir  # update for cache key

        self._ide_db=db; self._ide_db_root=game_root
        if mw and hasattr(mw,'log_message'):
            if loaded:
                mw.log_message(
                    f"IDE DB: {loaded} objects from {game_root} "
                    f"({len(db.source_files)} IDE files)  max_id={db.max_id}")
            else:
                mw.log_message(
                    f"IDE DB: no .ide files found under {game_root}  "
                    f"(tried img_path={img_path})")
        return db

    def _lookup_ide_from_db(self, stem: str): #vers 1
        """Look up a model in the standalone IDEDatabase fallback."""
        db = self._get_ide_db()
        return db.lookup(stem) if db else None

    def _get_xref(self): #vers 1
        """Return GTAWorldXRef from DAT Browser if loaded, else None."""
        mw = getattr(self, 'main_window', None)
        if mw is None:
            return None
        # Try dat_browser.xref first, then mw.xref directly
        db = getattr(mw, 'dat_browser', None)
        xref = getattr(db, 'xref', None) if db else None
        if xref is None:
            xref = getattr(mw, 'xref', None)
        return xref

    def _lookup_ide_for_dff(self, dff_path: str): #vers 2
        """Look up IDEObject for a DFF file path via DAT Browser xref.
        Updates info panel labels and enables/disables TXD/IDE buttons.
        Handles /tmp paths where DFF was extracted with a random suffix."""
        raw_stem = os.path.splitext(os.path.basename(dff_path))[0]

        # If workshop has the original DFF entry name (set at open time), use it
        orig = getattr(self, '_original_dff_name', '') or ''
        if orig:
            stem = os.path.splitext(orig)[0].lower()
        else:
            stem = raw_stem.lower()

        xref = self._get_xref()

        # Reset labels
        for lbl, val in [
            ('info_ide_section', '—'),
            ('info_model_id',    'ID: —'),
            ('info_txd_name',    '—'),
        ]:
            w = getattr(self, lbl, None)
            if w: w.setText(val)
        for btn in ('load_txd_btn', 'find_in_ide_btn'):
            b = getattr(self, btn, None)
            if b: b.setEnabled(False)

        if xref is None:
            # Try standalone IDEDatabase fallback — scan game root for .ide files
            obj = self._lookup_ide_from_db(stem)
            if obj is None:
                # Progressive stem fallback
                parts = stem.split('_')
                for n in range(len(parts)-1, 0, -1):
                    obj = self._lookup_ide_from_db('_'.join(parts[:n]))
                    if obj:
                        break
            if obj:
                self._current_ide_obj = obj
                self._ide_txd_name    = obj.txd_name or ''
                if hasattr(self, 'info_ide_section'):
                    self.info_ide_section.setText(obj.section or 'object')
                if hasattr(self, 'info_model_id'):
                    self.info_model_id.setText(f"ID: {obj.model_id}")
                if hasattr(self, 'info_txd_name'):
                    self.info_txd_name.setText(obj.txd_name or '—')
                if hasattr(self, 'load_txd_btn') and obj.txd_name:
                    self.load_txd_btn.setEnabled(True)
                # Auto-load TXD immediately
                if obj.txd_name and obj.txd_name.lower() not in ('null',''):
                    from PyQt6.QtCore import QTimer as _QTIDB
                    _QTIDB.singleShot(0, lambda txd=obj.txd_name:
                        self._auto_load_txd_from_imgs(txd))
                return obj
            if hasattr(self, 'info_ide_section'):
                self.info_ide_section.setText('No DAT loaded')
            return None

        obj = xref.model_map.get(stem)
        if obj is None:
            # Try progressively shorter stems (e.g. airportwall_2_2 → airportwall_2 → airportwall)
            parts = stem.split('_')
            for n in range(len(parts)-1, 0, -1):
                shorter = '_'.join(parts[:n])
                obj = xref.model_map.get(shorter)
                if obj:
                    break
        if obj is None:
            if hasattr(self, 'info_ide_section'):
                self.info_ide_section.setText('Not in IDE')
            return None

        # Populate labels
        _section_label = {
            'objs': 'Static', 'tobj': 'Timed', 'cars': 'Vehicle',
            'peds': 'Ped', 'weap': 'Weapon', 'hier': 'Hierarchy',
            'anim': 'Animated', 'tanm': 'Timed Anim',
        }
        section = _section_label.get(obj.section, obj.section or 'Object')
        ide_file = os.path.basename(obj.source_ide) if obj.source_ide else '?'

        if hasattr(self, 'info_ide_section'):
            self.info_ide_section.setText(section)
            self.info_ide_section.setToolTip(f"{ide_file} — section [{obj.section}]")
        if hasattr(self, 'info_model_id'):
            self.info_model_id.setText(f"ID: {obj.model_id}")
        if hasattr(self, 'info_txd_name'):
            txd = obj.txd_name or '—'
            self.info_txd_name.setText(txd)
            # Check if TXD is findable
            in_xref = obj.txd_name and obj.txd_name.lower() in xref.txd_stems
            self.info_txd_name.setToolTip(
                f"{txd}.txd — {'found in IMG' if in_xref else 'not found in IMG'}")

        # Enable buttons
        if hasattr(self, 'load_txd_btn') and obj.txd_name and obj.txd_name.lower() not in ('null', ''):
            self.load_txd_btn.setEnabled(True)
        if hasattr(self, 'find_in_ide_btn'):
            self.find_in_ide_btn.setEnabled(True)

        self._current_ide_obj = obj
        # Auto-load TXD immediately after IDE lookup — no user click needed
        if obj and obj.txd_name and obj.txd_name.lower() not in ('null',''):
            self._ide_txd_name = obj.txd_name
            from PyQt6.QtCore import QTimer as _QTIDE
            _QTIDE.singleShot(0, lambda txd=obj.txd_name:
                self._auto_load_txd_from_imgs(txd))
        # Refresh the IDE label in the material editor dialog if it's open
        fn = getattr(self, '_ide_label_refresh_fn', None)
        if fn and callable(fn):
            from PyQt6.QtCore import QTimer as _QTLBL
            _QTLBL.singleShot(50, fn)
        return obj

    def _find_in_ide(self): #vers 2
        """Switch to DAT Browser tab and highlight this model's IDE entry."""
        obj = getattr(self, '_current_ide_obj', None)
        if not obj:
            self._set_status("No IDE entry linked — load a model from an IMG via DAT Browser first")
            return
        mw = getattr(self, 'main_window', None)
        if not mw:
            return
        # Try to find and activate DAT browser tab
        db = getattr(mw, 'dat_browser', None)
        if db and hasattr(db, '_search_bar'):
            # Switch to dat browser tab
            if hasattr(mw, 'tab_widget'):
                for i in range(mw.tab_widget.count()):
                    if mw.tab_widget.widget(i) is db or                        db in mw.tab_widget.widget(i).findChildren(type(db)):
                        mw.tab_widget.setCurrentIndex(i)
                        break
            # Set search to model name and trigger
            try:
                db._search_bar.setText(obj.model_name)
                db._search_bar.returnPressed.emit()
            except Exception:
                pass
        self._set_status(
            f"IDE: {obj.model_name}  ID={obj.model_id}  "
            f"TXD={obj.txd_name}  section={obj.section}  "
            f"source={os.path.basename(obj.source_ide or '')}")

    def open_dff_file(self, file_path: str): #vers 3
        """Open any model format (DFF, PS2/PSP/mobile DFF, MDL, WDR/WFT, NIF)."""
        self.current_col_file = None   # clear COL mode
        # Hide COL format combo when DFF is loaded
        if hasattr(self, 'format_combo'):
            self.format_combo.setVisible(False)
        try:
            from apps.methods.dff_parser import parse_model_any
            with open(file_path, 'rb') as f:
                data = f.read()
            model = parse_model_any(data, file_path)
            if model is None:
                QMessageBox.warning(self, "Model Error",
                    "Not a supported model (DFF, MDL, WDR, WFT, NIF) or failed to parse.")
                return
            self._show_dff(file_path, model)
        except Exception as e:
            import traceback; traceback.print_exc()
            QMessageBox.critical(self, "DFF Error", f"Failed to open DFF:\n{e}")

    def _show_dff(self, file_path, model): #vers 2
        """Make model current and fill the viewport, tree, table and toolbars."""
        try:
            self._current_dff_path = file_path
            self._current_dff_model = model
            name = os.path.basename(file_path)
            self.setWindowTitle(f"{App_name}: {name} "
                                f"[{model.geometry_count} geometries, "
                                f"{model.frame_count} frames]")
            # Enable OBJ export toolbar button
            if hasattr(self, 'export_obj_btn'):
                self.export_obj_btn.setEnabled(True)
            self._set_status(f"Opened DFF: {name}  — "
                             f"{model.geometry_count} geometries, "
                             f"{model.frame_count} frames, "
                             f"{model.atomic_count} atomics")
            # Populate mesh list + viewport
            self._display_dff_model(model)
            # Populate frame/bone hierarchy tree
            self._populate_frame_tree(model)
            # Populate detail table with geometry info
            self._populate_dff_detail_table(model)
            # Look up IDE entry → populate TXD/IDE link row
            self._lookup_ide_for_dff(file_path)
            # Enable DFF-mode toolbar buttons
            self._enable_dff_toolbar(True)
            # Restore saved viewport light settings
            self._load_viewport_light_settings()
            self._doc_opened()
        except Exception as e:
            import traceback; traceback.print_exc()
            QMessageBox.critical(self, "DFF Error", f"Failed to open DFF:\n{e}")


    def _export_dff_obj(self): #vers 2
        """Export the open model to Wavefront OBJ + MTL."""
        from apps.methods.model_convert import model_to_obj
        model = getattr(self, '_current_dff_model', None)
        if not model or not model.geometries:
            QMessageBox.warning(self, "No Model", "Open a model first.")
            return
        base = os.path.splitext(os.path.basename(getattr(self, '_current_dff_path', '') or 'model.dff'))[0]
        out_path, _ = QFileDialog.getSaveFileName(self, "Export OBJ", base + ".obj",
                                                  "Wavefront OBJ (*.obj);;All Files (*)")
        if not out_path:
            return
        try:
            v, f = model_to_obj(model, out_path)
        except Exception as e:
            QMessageBox.critical(self, "OBJ Export Error", str(e))
            return
        self._set_status(f"Exported OBJ: {os.path.basename(out_path)}  ({v:,}v {f:,}f)")

    # - IDE-linked operations

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
            if not getattr(self, "current_col_file", None):
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
            lw = (self.mod_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
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
        if not getattr(self, "current_col_file", None):
            QMessageBox.warning(self, "No COL File", "Load a COL file first.")
            return

        ide_path, _ = QFileDialog.getOpenFileName(
            self, "Select IDE to remove unref'd models", "",
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
                    "All models are referenced by the IDE — nothing to remove.")
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
        """Export only models referenced by an IDE file."""
        from PyQt6.QtWidgets import QMessageBox, QFileDialog
        import os
        from apps.methods.col_workshop_writer import save_col_file

        if not getattr(self, "current_col_file", None):
            QMessageBox.warning(self, "No COL File", "Load a COL file first.")
            return

        ide_path, _ = QFileDialog.getOpenFileName(
            self, "Select IDE File for Export", "",
            "IDE Files (*.ide);;All Files (*)")
        if not ide_path:
            return

        out_path, _ = QFileDialog.getSaveFileName(
            self, "Save filtered img archive", "",
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
                    "No models matched the IDE entries.")
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

    def _import_col_data(self): #vers 3
        """Import one or more COL models from .col file(s) into the current archive."""
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        if not getattr(self, "current_col_file", None):
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
            active = (self.mod_compact_list
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

        if not getattr(self, "current_col_file", None):
            QMessageBox.warning(self, "Export", "No dff file loaded.")
            return
        models = getattr(self.current_col_file, 'models', [])
        if not models:
            QMessageBox.warning(self, "Export", "No models to export.")
            return

        # Determine selection from the VISIBLE list
        if getattr(self, '_col_view_mode', 'detail') == 'detail':
            lw = getattr(self, 'mod_compact_list', None)
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
                self, "Export DFF Model", safe + '.dff', "DFF Files (*.dff);;All Files (*)")
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
                out  = os.path.join(folder, safe.lower() + '.dff')
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

    def _save_file(self): #vers 10
        """Save current DFF model. Falls back to Save As if no path set."""
        from PyQt6.QtWidgets import QFileDialog
        dff_model = getattr(self, '_current_dff_model', None)
        dff_path  = getattr(self, '_current_dff_path', '') or ''

        if not dff_model:
            QMessageBox.warning(self, "Save", "No DFF model loaded to save.")
            return
        if dff_path.lower().endswith(('.nif', '.mdl', '.wdr', '.wft')):
            return self._save_in_place(dff_model, dff_path)
        if dff_path and not dff_path.lower().endswith('.dff'):
            return self._save_model_as()                # OBJ / other: pick a format

        if not dff_path:
            return self._save_model_as()                # built from scratch: pick format

        try:
            from apps.methods.dff_parser import DFFWriter
            from apps.methods.file_backup import safe_write_bytes
            raw = DFFWriter.write(dff_model)
            report = list(DFFWriter.last_report)
            safe_write_bytes(dff_path, raw, "model_workshop")     # backup + atomic write
            src = next((st for st in getattr(self, '_open_models', {}).values()
                        if st['path'] == dff_path), None)
            if src is not None and src.get('img') is not None:   # opened from an IMG: write back
                if not src['img'].add_entry(src['entry'].name, raw):
                    raise IOError(f"{src['entry'].name}: archive refused the data")
                src['orig'] = raw
            self._current_dff_path = dff_path
            fname = os.path.basename(dff_path)
            self._set_status(f"Saved: {fname}")
            mw = getattr(self, 'main_window', None)
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(f"Saved DFF: {fname} ({len(raw):,} bytes)")
            self._mesh_dirty = False
            failed = [r for r in report if r.startswith("not written")]
            done = [r for r in report if not r.startswith("not written")]
            if done:
                self._set_status(f"Saved: {fname} - " + "; ".join(done))
            if failed:
                QMessageBox.information(self, "Save DFF",
                    f"{fname} saved. These edits could not be written:\n\n" + "\n".join(failed))
        except NotImplementedError as e:
            QMessageBox.information(self, "Save DFF", str(e))
        except Exception as e:
            QMessageBox.critical(self, "Save Error", str(e))


    def _save_file_as(self): #vers 2
        """Save As: model formats when a model is open, else COL."""
        if getattr(self, '_current_dff_model', None) is not None and not getattr(self, 'current_col_file', None):
            return self._save_model_as()
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


    def open_col_file(self, file_path): #vers 4
        """Open standalone COL file - supports COL1, COL2, COL3"""
        self._dff_adapters = []   # clear DFF mode
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
            if hasattr(self, 'format_combo'):
                self.format_combo.setVisible(True)
            self.current_file_path = file_path
            self._enable_dff_toolbar(False)   # switch to COL mode

            # Update window title with model count
            model_count = len(col_file.models) if hasattr(col_file, 'models') else 0
            version_str = f"COL ({model_count} models)"
            self.setWindowTitle(f"{App_name} - {os.path.basename(file_path)} - {version_str}")

            # Populate UI — compact view is default, also populate detail table
            self._populate_compact_col_list()
            self._populate_collision_list()

            # Select first model by default
            active_list = (self.mod_compact_list
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

            self._doc_opened()
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


    def load_from_img_archive(self, img_path): #vers 5
        """Load IMG archive — populates left panel with all DFF/COL/TXD entries."""
        try:
            from apps.methods.img_core_classes import IMGFile
            img = IMGFile(img_path)
            img.open()
            self.current_img = img
            self._panel_img_ids = None          # one chosen archive, no tab sync
            self._current_img_path = img_path

            img_name = os.path.basename(img_path)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Model Workshop: scanning {img_name}…")

            model_exts = ('.dff', '.col', '.txd')
            model_entries = [e for e in img.entries
                             if getattr(e, 'name', '').lower().endswith(model_exts)]

            lw = getattr(self, 'col_list_widget', None)
            if lw is not None:
                lw.clear()
                for entry in model_entries:
                    name = entry.name
                    ext  = os.path.splitext(name)[1].lower()
                    item = QListWidgetItem(name)
                    item.setData(Qt.ItemDataRole.UserRole, entry)
                    from PyQt6.QtGui import QColor
                    if ext == '.dff':
                        item.setForeground(QColor('#4db6ac'))
                    elif ext == '.col':
                        item.setForeground(QColor('#ef5350'))
                    elif ext == '.txd':
                        item.setForeground(QColor('#ffa726'))
                    lw.addItem(item)

                # Update header counts
                n_dff = sum(1 for e in model_entries if e.name.lower().endswith('.dff'))
                n_txd = sum(1 for e in model_entries if e.name.lower().endswith('.txd'))
                n_col = sum(1 for e in model_entries if e.name.lower().endswith('.col'))
                from apps.methods.file_list_filter import refresh_type_filter
                refresh_type_filter(self)                  # header counts + type filter

            n_dff = sum(1 for e in model_entries if e.name.lower().endswith('.dff'))
            n_txd = sum(1 for e in model_entries if e.name.lower().endswith('.txd'))
            n_col = sum(1 for e in model_entries if e.name.lower().endswith('.col'))
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Model Workshop: {img_name} — "
                    f"{n_dff} DFF, {n_col} COL, {n_txd} TXD")

            self.setWindowTitle(f"Model Workshop: {img_name}")
            return True

        except Exception as e:
            import traceback; traceback.print_exc()
            QMessageBox.critical(self, "Error", f"Failed to load from IMG:\n{e}")
            return False


    def _analyze_collision(self): #vers 1
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
                self.main_window.log_message(f"✅ Analyzed COL: {os.path.basename(self.current_file_path)}")

        except Exception as e:
            print(f"Error analyzing file: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to analyze file:\n{str(e)}")


    def _populate_left_panel_from_img(self, img): #vers 7
        """Populate the left panel list from an already-open IMGFile object.
        Also stores img as self.current_img so _extract_col_from_img can read entries."""
        lw = getattr(self, 'col_list_widget', None)
        if lw is None:
            return
        try:
            lw.count()  # raises RuntimeError if C++ object deleted
        except RuntimeError:
            return
        # Store so clicking entries can extract data
        self.current_img = img
        self._entry_img = {}                    # entry id -> source IMG (multi-IMG panel)

        lw.clear()
        model_exts = ('.dff', '.col', '.txd')
        n_dff = n_txd = n_col = 0
        from PyQt6.QtGui import QColor
        for entry in getattr(img, 'entries', []):
            name = getattr(entry, 'name', '')
            if not name.lower().endswith(model_exts):
                continue
            ext = os.path.splitext(name)[1].lower()
            item = QListWidgetItem(name)
            item.setData(Qt.ItemDataRole.UserRole, entry)
            if ext == '.dff':
                item.setForeground(QColor('#4db6ac')); n_dff += 1
            elif ext == '.col':
                item.setForeground(QColor('#ef5350')); n_col += 1
            elif ext == '.txd':
                item.setForeground(QColor('#ffa726')); n_txd += 1
            lw.addItem(item)

        from apps.methods.file_list_filter import refresh_type_filter
        refresh_type_filter(self)                  # header counts + type filter
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(
                f"Model Workshop: {lw.count()} entries in left panel")
        return

    def _open_imgs(self): #vers 1
        """IMG archives open as IMG Factory tabs (plus current_img)."""
        mw = self.main_window
        out = []
        for info in (getattr(mw, 'open_files', {}) or {}).values() if mw else []:
            obj = info.get('file_object') if isinstance(info, dict) else None
            if obj is not None and hasattr(obj, 'entries') and \
                    str(info.get('file_path', '')).lower().endswith('.img') and obj not in out:
                out.append(obj)
        ci = getattr(mw, 'current_img', None) if mw else None
        if ci is not None and hasattr(ci, 'entries') and ci not in out:
            out.insert(0, ci)
        return out

    def _populate_left_panel_from_open_imgs(self): #vers 3
        """Files panel: every game DFF/TXD/COL when a game folder is set, else open IMG tabs."""
        mw = self.main_window
        root = getattr(mw, 'game_root', '') if mw else ''
        if root and os.path.isdir(root):
            self._load_game_model_list(root, rescan=False)
            return
        imgs = self._open_imgs()
        self._panel_img_ids = tuple(id(i) for i in imgs)
        if not imgs:
            return
        self._populate_left_panel_from_img(imgs[0])
        lw = self.col_list_widget
        if len(imgs) == 1:
            return
        from PyQt6.QtGui import QColor
        colours = {'.dff': '#4db6ac', '.col': '#ef5350', '.txd': '#ffa726'}
        counts = {'.dff': 0, '.txd': 0, '.col': 0}
        lw.clear()
        for img in imgs:
            arc = os.path.basename(getattr(img, 'file_path', '') or '')
            for entry in getattr(img, 'entries', []):
                ext = os.path.splitext(getattr(entry, 'name', ''))[1].lower()
                if ext not in colours:
                    continue
                item = QListWidgetItem(entry.name)
                item.setData(Qt.ItemDataRole.UserRole, entry)
                item.setToolTip(arc)
                item.setForeground(QColor(colours[ext]))
                self._entry_img[id(entry)] = img
                counts[ext] += 1
                lw.addItem(item)
        from apps.methods.file_list_filter import refresh_type_filter
        refresh_type_filter(self)                  # header counts + type filter
        self._left_panel_header.setText(self._left_panel_header.text() + f"  - {len(imgs)} IMGs")

    def _sync_open_imgs(self): #vers 1
        """Refresh the files panel when IMG tabs were opened or closed."""
        if getattr(self, '_panel_img_ids', None) is None:
            return                              # panel holds one chosen archive
        if tuple(id(i) for i in self._open_imgs()) != self._panel_img_ids:
            self._populate_left_panel_from_open_imgs()

    def _load_game_model_list(self, root, rescan=True): #vers 2
        """List every DFF/TXD/COL in the game: loose files and IMG entries."""
        from PyQt6.QtGui import QColor
        from apps.methods.game_file_list import fill_game_list, game_list_excludes, scan_game_files
        exts = ('.dff', '.txd', '.col')
        if rescan or getattr(self, '_game_model_items', None) is None or self._game_model_root != root:
            self._game_model_items = scan_game_files(self, root, exts)
        self._game_model_root = root
        self._panel_img_ids = None              # game list, no tab sync
        lw = self.col_list_widget
        shown, hidden = fill_game_list(lw, self._game_model_items,
                                       game_list_excludes(self._ribbon_config_path()),
                                       getattr(self, '_game_list_show_excluded', False))
        colours = {'.dff': '#4db6ac', '.col': '#ef5350', '.txd': '#ffa726'}
        counts = dict.fromkeys(exts, 0)
        for i in range(lw.count()):
            it = lw.item(i)
            ext = os.path.splitext(it.text())[1].lower()
            counts[ext] += 1
            if not it.toolTip().endswith("(excluded)"):
                it.setForeground(QColor(colours[ext]))
        from apps.methods.file_list_filter import refresh_type_filter
        refresh_type_filter(self)                  # header counts + type filter
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"Model Workshop: {shown} file(s) in {root} ({hidden} excluded)")

    def _model_item_entry(self, item): #vers 1
        """IMG entry of a list item (game list items resolve their IMG); None for loose files."""
        from apps.methods.game_file_list import game_img
        src = item.data(Qt.ItemDataRole.UserRole)
        if isinstance(src, dict):
            if src.get('kind') != 'img':
                return None
            img = game_img(self, src['img'])
            entry = next((e for e in img.entries if e.name == src['name']), None)
            if entry is not None:
                self.__dict__.setdefault('_entry_img', {})[id(entry)] = img
            return entry
        return src

    def _extract_col_from_img(self, entry): #vers 3
        """Read an entry's data from the IMG it was listed from."""
        try:
            img = getattr(self, '_entry_img', {}).get(id(entry)) or self.current_img
            if not img:
                return None
            return img.read_entry_data(entry)
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Extract error: {str(e)}")
            return None


    def _rename_col_model(self, model, row): #vers 1
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
                    f"Renamed model {row}: '{old_name}' → '{new_name}'")
        except Exception as e:
            QMessageBox.critical(self, "Rename Error", str(e))

    def _export_col_model(self, model, row): #vers 1
        """Export a single collision model as a standalone COL file."""
        try:
            model_name = getattr(model, 'name', f'model_{row}')
            default_name = model_name.lower().replace(' ', '_') + '.col'
            file_path, _ = QFileDialog.getSaveFileName(
                self, "Export COL Model",
                default_name, "COL Files (*.col);;All Files (*)")
            if not file_path:
                return

            # Build a minimal COL file containing just this model
            from apps.methods.col_workshop_loader import COLFile
            out = COLFile()
            # this model's original record (edits patched in) or a freshly written one
            from apps.methods.col_workshop_parser import COLWriter
            from apps.methods.col_splice import build_col_bytes
            from apps.methods.file_backup import safe_write_bytes
            safe_write_bytes(file_path, build_col_bytes([model], COLWriter, {"head": [], "tail": b""}))

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Exported model '{model_name}' → {os.path.basename(file_path)}")
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
