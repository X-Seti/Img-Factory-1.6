#this belongs in apps/components/Model_Editor/depends/model_edit_func.py - Version: 12
# X-Seti - October 12 2026 - IMG Factory 1.6 - Model Workshop editing

"""
Geometry edits, primitives, paint, materials, undo for ModelWorkshop.
"""

##class ModelEditMixin: -
# _add_geometry_to_dff
# _align_dialog
# _apply_to_selected_faces_paint
# _build_primitive
# _compute_face_shade
# _copy_model_to_clipboard
# _create_primitive_dialog
# _delete_selected_model
# _dff_analyze
# _dff_append_geometry
# _dff_copy_geometry
# _dff_delete_geometry
# _dff_duplicate_geometry
# _dff_flip_x
# _dff_flip_y
# _dff_get_geometries
# _dff_get_selected_geom_idx
# _dff_paste_geometry
# _dff_rotate_ccw
# _dff_rotate_cw
# _dff_transform_vertices
# _edit_model
# _duplicate_selected_model
# _exit_paint_mode
# _extrude_dialog
# _mesh_add_face
# _mesh_after_edit
# _mesh_centre_origin
# _mesh_copy_as_lod
# _mesh_copy_or_move
# _mesh_delete_faces
# _mesh_delete_vertices
# _mesh_detach
# _mesh_detached_dialog
# _mesh_detached_groups
# _mesh_fill_hole
# _mesh_frame_name
# _mesh_gizmo_moved
# _mesh_gizmo_settings
# _mesh_gizmo_xform
# _mesh_hide_selected
# _mesh_light_view
# _mesh_lighting
# _mesh_menu
# _mesh_move_dialog
# _mesh_optimise
# _mesh_optimum_bounds
# _mesh_preview
# _mesh_push_undo
# _mesh_record_detached
# _mesh_region_circle
# _mesh_region_window
# _mesh_rotate_dialog
# _mesh_run
# _mesh_scale_dialog
# _mesh_sel
# _mesh_select_material
# _mesh_select_verts
# _mesh_selection_to_file
# _mesh_selection_to_geometry
# _mesh_set_face_mode
# _mesh_show
# _mesh_show_vertices
# _mesh_split
# _mesh_target
# _mesh_toggle_gamepad
# _mesh_toggle_lock
# _mesh_undo_last
# _mesh_unhide_all
# _mesh_uv_dialog
# _mesh_verts_to_faces
# _mesh_weld
# _mirror_dialog
# _on_paint_mode_exited
# _on_painted_face
# _open_dff_material_list
# _open_paint_editor
# _open_paint_mat_popup
# _paint_cycle_mat
# _paste_model_from_clipboard
# _push_undo
# _set_paint_tool
# _undo_last_action
# _write_geometry_dff

import os
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont
from PyQt6.QtWidgets import QComboBox, QDialog, QFileDialog, QFrame, QHBoxLayout, QLabel, QLineEdit, QListWidget, QListWidgetItem, QMenu, QMessageBox, QPushButton, QSplitter, QVBoxLayout, QWidget
from apps.components.Model_Editor.depends.model_list_func import _DFFGeometryAdapter

class ModelEditMixin: #vers 1
    """Geometry edits, primitives, paint, materials, undo for ModelWorkshop."""



    # - Stub implementations (log until fully implemented)


    def _delete_selected_model(self): #vers 2
        """Delete selected collision model(s) — uses currentRow() for reliability."""
        if not getattr(self, "current_col_file", None): return
        models = getattr(self.current_col_file, 'models', [])
        if not models: return

        lw = (self.mod_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
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
            if QMessageBox.question(self, "Delete",
               f"Delete {len(indices)} collision models?",
               QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
               ) != QMessageBox.StandardButton.Yes:
                return

        for idx in indices:
            del models[idx]
        self._populate_collision_list()
        self._populate_compact_col_list()
        self._set_status(f"Deleted {len(indices)} model(s).")


    def _duplicate_selected_model(self): #vers 1
        rows = self.collision_list.selectionModel().selectedRows()
        if not rows or not self.current_col_file: return
        row = rows[0].row()
        item = self.collision_list.item(row, 1)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None: return
        import copy
        m = copy.deepcopy(self.current_col_file.models[idx])
        m.name = m.name + "_copy"
        self.current_col_file.models.insert(idx+1, m)
        self._populate_collision_list()
        self.collision_list.selectRow(row+1)


    def _copy_model_to_clipboard(self): #vers 1
        rows = self.collision_list.selectionModel().selectedRows()
        if not rows or not self.current_col_file: return
        row = rows[0].row()
        item = self.collision_list.item(row, 1)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None: return
        import copy
        self._clipboard_model = copy.deepcopy(self.current_col_file.models[idx])
        if hasattr(self, 'paste_btn') and self.paste_btn:
            self.paste_btn.setEnabled(True)


    def _paste_model_from_clipboard(self): #vers 1
        if not hasattr(self, '_clipboard_model') or not self._clipboard_model: return
        if not getattr(self, "current_col_file", None): return
        import copy
        m = copy.deepcopy(self._clipboard_model)
        m.name = m.name + "_paste"
        self.current_col_file.models.append(m)
        self._populate_collision_list()
        self.collision_list.selectRow(self.collision_list.rowCount()-1)




    def _open_dff_material_list(self): #vers 9

        """Material Editor — 3ds Max style.
        Left: grid of material slots (thumbnail sphere-style preview + name).
        Right: properties for the selected material.
        Compact, everything in one window."""

        from PyQt6.QtWidgets import (
            QDialog, QVBoxLayout, QHBoxLayout, QSplitter, QScrollArea,
            QGridLayout, QLabel, QPushButton, QLineEdit, QComboBox,
            QFrame, QFormLayout)
        from PyQt6.QtGui import QColor, QPixmap, QImage, QFont, QPainter, QBrush, QPen, QRadialGradient, QPolygonF
        from PyQt6.QtCore import Qt as _Qt, QSize as _QS

        model = getattr(self, '_current_dff_model', None)

        if not model:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "Material Editor", "Load a DFF file first.")
            return

        vp = getattr(self, 'preview_widget', None)
        tex_cache = getattr(vp, '_tex_cache', {}) if vp else {}
        ide_obj = getattr(self, '_current_ide_obj', None)
        dff_name = os.path.splitext(
            os.path.basename(getattr(self, '_current_dff_path', '') or 'model.dff'))[0]
        ic = self._get_icon_color()

        # - Collect all materials

        all_mats = []   # (gi, mi, mat, geom)
        geoms = getattr(model, 'geometries', [])

        if not geoms:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "Material Editor", "Loaded DFF has no geometries.")
            return

        for gi, geom in enumerate(geoms):
            # FIX: Check for materials attribute and non-empty list
            if hasattr(geom, 'materials') and geom.materials:
                for mi, mat in enumerate(geom.materials):
                    all_mats.append((gi, mi, mat, geom))

        if not all_mats:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "Material Editor", "No materials found in DFF geometries.")
            return

        dlg = QDialog(self)
        dlg.setWindowTitle(f"Material Editor  {dff_name}")
        dlg.resize(860, 540)
        dlg.setMinimumSize(640, 400)
        root = QVBoxLayout(dlg)
        root.setSpacing(4)
        root.setContentsMargins(6, 6, 6, 6)

        # - Top: IDE info bar

        info_bar = QHBoxLayout()
        model_lbl = QLabel(f"<b>{dff_name}</b>")
        model_lbl.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        info_bar.addWidget(model_lbl)

        if ide_obj:
            draw    = ide_obj.extra.get('draw_dist', ide_obj.extra.get('dist1', '—'))
            ide_src = os.path.basename(ide_obj.source_ide or '') or '?'
            ide_lbl = QLabel(
                f"  ID:{ide_obj.model_id}  TXD:{ide_obj.txd_name or '—'}"
                f"  Draw:{draw}  [{ide_obj.section}]  {ide_src}")

            ide_lbl.setStyleSheet("color:palette(windowText); font-size:10px; font-family:monospace;")
        else:
            ide_lbl = QLabel("  IDE: Not found — load a game via DAT Browser")
            ide_lbl.setStyleSheet("color:palette(placeholderText); font-size:10px;")

        info_bar.addWidget(ide_lbl, 1)

        n_cached = sum(1 for _,_,m,_ in all_mats
                       if (getattr(m,'texture_name','') or '').strip().lower() in tex_cache)
        bc = 'palette(link)' if n_cached==len(all_mats) and all_mats else \
             'palette(mid)' if n_cached > 0 else 'palette(placeholderText)'

        badge = QLabel(f"{n_cached}/{len(all_mats)} cached")
        badge.setStyleSheet(f"color:{bc}; font-weight:bold; font-size:10px;")
        info_bar.addWidget(badge)
        root.addLayout(info_bar)
        sep = QFrame(); sep.setFrameShape(QFrame.Shape.HLine)
        sep.setStyleSheet("color:palette(mid);"); root.addWidget(sep)

        # - Main splitter

        splitter = QSplitter(_Qt.Orientation.Horizontal)
        root.addWidget(splitter, 1)

        # - LEFT: Material grid (Max-style)

        left_frame = QFrame()
        left_lay   = QVBoxLayout(left_frame)
        left_lay.setContentsMargins(0,0,0,0)
        left_lay.setSpacing(4)
        grid_hdr = QHBoxLayout()
        grid_hdr.setSpacing(4)
        density_lbl = QLabel("Grid:")
        density_lbl.setFont(self.panel_font)
        grid_hdr.addWidget(density_lbl)
        _n_cols = [3]

        _col_btns = []
        for nc in (3, 4, 5, 6):
            b = QPushButton(str(nc))
            b.setFixedSize(24, 20)
            b.setFont(self.panel_font)
            b.setCheckable(True)
            b.setChecked(nc == _n_cols[0])
            _col_btns.append(b)
            def _set_cols(checked, n=nc, all_b=_col_btns):  #vers 1
                if not checked: return
                _n_cols[0] = n
                for ob in all_b: ob.setChecked(ob.text() == str(n))
                _rebuild_grid()
            b.toggled.connect(_set_cols)
            grid_hdr.addWidget(b)

        grid_hdr.addSpacing(1)
        grid_hdr.addStretch()
        left_lay.addLayout(grid_hdr)
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        grid_container = [None]
        left_lay.addWidget(scroll, 1)
        N_ROWS = 6           # grid is always N_ROWS rows; extras = empty placeholder spheres
        _selected_row = [0]
        _slot_btns    = []

        def _get_slot_size(n_cols):  #vers 1
            sizes = {3: (88,104), 4: (72,86), 5: (62,76), 6: (54,66)}
            return sizes.get(n_cols, (72,86))

        def _rebuild_grid():  #vers 1
            nc   = _n_cols[0]
            total = nc * N_ROWS          # always nc columns × 6 rows
            sw, sh = _get_slot_size(nc)
            pix_w  = sw - 8

            new_w = QWidget()
            gl    = QGridLayout(new_w)
            gl.setSpacing(3)
            gl.setContentsMargins(3,3,3,3)

            # Reparent real slots first
            for s, _ in _slot_btns:
                s.setParent(new_w)
            scroll.setWidget(new_w)
            grid_container[0] = new_w

            # Place real material slots
            for i, (gi, mi, mat, geom) in enumerate(all_mats):
                if i >= total: break
                slot, pix_lbl = _slot_btns[i]
                slot.setFixedSize(sw, sh)
                pix_lbl.setFixedSize(pix_w, pix_w)
                pix_lbl.setPixmap(_make_slot_pix(mat, geom, pix_w))
                slot.setVisible(True)
                gl.addWidget(slot, i // nc, i % nc)

            # Fill remaining cells with empty placeholder slots
            for j in range(len(all_mats), total):
                row, col = j // nc, j % nc
                empty = QFrame()
                empty.setFixedSize(sw, sh)
                empty.setStyleSheet(
                    "QFrame { border: 1px dashed palette(mid); border-radius: 4px; "
                    "background: palette(base); }")
                # Grey sphere placeholder
                ph = QLabel()
                ph.setFixedSize(pix_w, pix_w)
                ph.setAlignment(_Qt.AlignmentFlag.AlignCenter)
                ph_pix = QPixmap(pix_w, pix_w)
                ph_pix.fill(_Qt.GlobalColor.transparent)
                _pp = QPainter(ph_pix)
                _pp.setRenderHint(QPainter.RenderHint.Antialiasing)
                from PyQt6.QtGui import QRadialGradient as _RG
                cx, cy, r = pix_w//2, pix_w//2, pix_w//2 - 3
                _g = _RG(cx, cy, r)
                _g.setColorAt(0, QColor(80,80,90,60))
                _g.setColorAt(1, QColor(40,40,50,40))
                from PyQt6.QtGui import QBrush as _QB
                _pp.setBrush(_QB(_g)); _pp.setPen(_Qt.PenStyle.NoPen)
                _pp.drawEllipse(cx-r, cy-r, 2*r, 2*r)
                _pp.end()
                ph.setPixmap(ph_pix)
                el = QVBoxLayout(empty)
                el.setContentsMargins(4,4,4,4); el.setSpacing(2)
                el.addWidget(ph, 0, _Qt.AlignmentFlag.AlignCenter)
                sl = QLabel(f"{j+1}")
                sl.setFont(QFont("Arial", 6))
                sl.setAlignment(_Qt.AlignmentFlag.AlignCenter)
                sl.setStyleSheet("color: palette(placeholderText);")
                el.addWidget(sl)
                gl.addWidget(empty, row, col)

            gl.setColumnStretch(nc, 1)

        SLOT_W, SLOT_H = 88, 104
        N_COLS = 5
        _slot_shape = ['sphere']

        def _make_slot_pix(mat, geom, size=80, shape=None):  #vers 1
            shape = shape or _slot_shape[0]
            pix = QPixmap(size, size)
            pix.fill(_Qt.GlobalColor.transparent)
            p2 = QPainter(pix)
            p2.setRenderHint(QPainter.RenderHint.Antialiasing)
            tname = (getattr(mat,'texture_name','') or '').strip().lower()
            tex   = (tex_cache.get(tname) or tex_cache.get(tname.split('.')[0]))
            c     = getattr(mat,'colour',None) or getattr(mat,'color',None)
            base  = QColor(c.r,c.g,c.b) if c else self._get_ui_color('viewport_text')
            pad   = 4

            if shape == 'flat':
                if tex and isinstance(tex, QImage):
                    scaled = tex.scaled(size-pad*2, size-pad*2,
                                        _Qt.AspectRatioMode.IgnoreAspectRatio,
                                        _Qt.TransformationMode.SmoothTransformation)
                    p2.drawImage(pad, pad, scaled)

                else:
                    p2.fillRect(pad, pad, size-pad*2, size-pad*2, base)
                p2.setPen(QPen(QColor(80,80,80,120), 1))
                p2.drawRect(pad, pad, size-pad*2-1, size-pad*2-1)

            elif shape == 'cube':
                from PyQt6.QtCore import QPointF
                m = size // 2
                top = [QPointF(m, pad), QPointF(size-pad, m//2+pad),
                       QPointF(m, m), QPointF(pad, m//2+pad)]
                front = [QPointF(pad, m//2+pad), QPointF(m, m),
                         QPointF(m, size-pad), QPointF(pad, size-pad-m//2)]
                right = [QPointF(m, m), QPointF(size-pad, m//2+pad),
                         QPointF(size-pad, size-pad-m//2), QPointF(m, size-pad)]

                def _face_brush(darken=0):  #vers 1
                    if tex and isinstance(tex, QImage):
                        sc = tex.scaled(size, size, _Qt.AspectRatioMode.IgnoreAspectRatio,
                                        _Qt.TransformationMode.SmoothTransformation)
                        b = QBrush(sc)
                        if darken:
                            b2 = QBrush(QColor(0,0,0,darken))
                            return b, b2
                        return b, None
                    else:
                        col = QColor(base.red()-darken, base.green()-darken,
                                     base.blue()-darken)
                        return QBrush(col), None

                p2.setPen(QPen(QColor(30,30,30,150), 0.8))

                for face, dk in [(top,0),(front,40),(right,70)]:
                    fb, overlay = _face_brush(dk)
                    poly = QPolygonF(face)
                    p2.setBrush(fb); p2.drawPolygon(poly)
                    if overlay:
                        p2.setBrush(overlay); p2.drawPolygon(poly)

            else:  # sphere
                cx, cy, r = size//2, size//2, size//2 - 2

                if tex and isinstance(tex, QImage):
                    p2.setClipRect(0,0,size,size)
                    brush = QBrush(tex.scaled(size,size,
                                _Qt.AspectRatioMode.IgnoreAspectRatio,
                                _Qt.TransformationMode.SmoothTransformation))
                    p2.setBrush(brush); p2.setPen(_Qt.PenStyle.NoPen)
                    p2.drawEllipse(cx-r, cy-r, 2*r, 2*r)
                else:
                    p2.setBrush(QBrush(base)); p2.setPen(_Qt.PenStyle.NoPen)
                    p2.drawEllipse(cx-r, cy-r, 2*r, 2*r)
                grad = QRadialGradient(cx-r//3, cy-r//3, r)
                grad.setColorAt(0, QColor(255,255,255,90 if tex else 100))
                grad.setColorAt(0.5, QColor(0,0,0,0))
                grad.setColorAt(1, QColor(0,0,0,130))
                p2.setBrush(QBrush(grad)); p2.drawEllipse(cx-r,cy-r,2*r,2*r)
            p2.end()
            return pix

        def _refresh_slot(i):  #vers 1
            if i < len(_slot_btns) and i < len(all_mats):
                _,lbl = _slot_btns[i]
                gi2,mi2,mat2,geom2 = all_mats[i]
                lbl.setPixmap(_make_slot_pix(mat2, geom2))

        def _refresh_all_slots_shaped():  #vers 1
            for i in range(len(_slot_btns)):
                _refresh_slot(i)

        def _slot_context_menu(slot_idx, global_pos):  #vers 1
            from PyQt6.QtWidgets import QMenu
            menu = QMenu()
            for label, shape in [("Sphere", "sphere"),
                                 ("Cube",   "cube"),
                                 ("Flat",   "flat")]:
                act = menu.addAction(label)
                act.setCheckable(True)
                act.setChecked(_slot_shape[0] == shape)

                def _set(checked, s=shape):  #vers 1
                    _slot_shape[0] = s
                    _refresh_all_slots_shaped()
                act.triggered.connect(_set)

            menu.addSeparator()
            menu.addAction("Reload TXD from IMGs").triggered.connect(
                lambda: (self._auto_load_txd_from_imgs(), _refresh_all_slots_shaped()))

            menu.addAction("Open in TXD Workshop").triggered.connect(self._open_linked_txd)
            menu.exec(global_pos)

        for idx, (gi, mi, mat, geom) in enumerate(all_mats):
            tname  = (getattr(mat,'texture_name','') or '').strip()
            in_cache = tname.lower() in tex_cache if tname else False
            slot = QFrame()
            slot.setFixedSize(SLOT_W, SLOT_H)
            slot.setCursor(_Qt.CursorShape.PointingHandCursor)
            slot_lay = QVBoxLayout(slot)
            slot_lay.setContentsMargins(3,3,3,3); slot_lay.setSpacing(2)
            pix_lbl = QLabel()
            pix_lbl.setFixedSize(80,80)
            pix_lbl.setAlignment(_Qt.AlignmentFlag.AlignCenter)
            pix_lbl.setPixmap(_make_slot_pix(mat, geom))
            _raw = next((t for t in getattr(self,'_mod_textures',[])
                         if t.get('name','').lower()==tname.lower()), None)
            if _raw:
                pix_lbl._tex_raw = _raw
                pix_lbl._hover_wnd = [None]

                def _enter(ev, lb=pix_lbl, ws=self):  #vers 1
                    ws._show_tex_hover(lb, getattr(lb,'_tex_raw',None))

                def _leave(ev, lb=pix_lbl, ws=self):  #vers 1
                    ws._hide_tex_hover(lb)
                pix_lbl.enterEvent = _enter
                pix_lbl.leaveEvent = _leave
            slot_lay.addWidget(pix_lbl, 0, _Qt.AlignmentFlag.AlignCenter)
            border = 'palette(link)' if in_cache else ('palette(mid)' if not tname else 'palette(placeholderText)')
            name_short = (tname[:12]+'…') if len(tname)>12 else (tname or '(none)')
            name_lbl = QLabel(name_short)
            name_lbl.setFont(QFont("Arial", 7))
            name_lbl.setAlignment(_Qt.AlignmentFlag.AlignCenter)
            # FIX: Theme-aware styling
            name_lbl.setStyleSheet("color:palette(windowText);")
            name_lbl.setToolTip(f"Mat {mi}  Texture: {tname or '(none)'}")
            slot_lay.addWidget(name_lbl)
            slot._idx = idx

            def _select(ev=None, i=idx, s=slot):  #vers 1
                _selected_row[0] = i
                for j,(sl,_) in enumerate(_slot_btns):
                    sl.setStyleSheet(
                        "QFrame{border:2px solid palette(highlight);border-radius:3px;background:palette(alternateBase);}"
                        if j==i else
                        "QFrame{border:1px solid palette(mid);border-radius:3px;background:transparent;}")
                _on_slot_selected(i)

            def _slot_mouse(ev, i=idx, s=slot):  #vers 1
                from PyQt6.QtCore import Qt as _Qt2
                if ev.button() == _Qt2.MouseButton.LeftButton:
                    _select(i=i, s=s)
                elif ev.button() == _Qt2.MouseButton.RightButton:
                    _slot_context_menu(i, ev.globalPosition().toPoint())
            slot.mousePressEvent = _slot_mouse
            slot.setStyleSheet("QFrame{border:1px solid palette(mid);border-radius:3px;background:transparent;}")
            _slot_btns.append((slot, pix_lbl))
        _rebuild_grid()

        # TXD action buttons below grid
        txd_row = QHBoxLayout(); txd_row.setSpacing(3)

        def _qb_txd(label, tip, slot_fn, icon_fn=None):  #vers 1
            b = QPushButton(); b.setFixedHeight(26); b.setFixedWidth(26)
            b.setFont(self.button_font)
            if icon_fn:
                try: b.setIcon(getattr(self.icon_factory,icon_fn)(color=ic)); b.setIconSize(_QS(20,20))
                except Exception: b.setText(label[0])
            else: b.setText(label[0])
            b.setToolTip(f"{label}: {tip}"); b.clicked.connect(slot_fn); return b

        txd_row.addWidget(_qb_txd("Load TXD","Load a TXD file",self._load_txd_into_workshop,"open_icon"))
        txd_row.addWidget(_qb_txd("Auto-find","Search open IMGs for linked TXD",
                                  lambda: (self._auto_load_txd_from_imgs(), _refresh_all_slots()),"reset_icon"))
        txd_row.addWidget(_qb_txd("Texlist","Scan texlist/ folder",
                                  lambda: (self._auto_load_from_texlist(getattr(self,'_ide_txd_name','')),
                                           _refresh_all_slots()),"folder_icon"))
        txd_row.addWidget(_qb_txd("Swap TXD","Change IDE TXD set",
                                  lambda: swap_combo.setFocus(),"convert_icon"))
        txd_row.addStretch()
        left_lay.addLayout(txd_row)
        splitter.addWidget(left_frame)

        # - RIGHT: Property panel

        right_frame = QFrame(); right_frame.setMinimumWidth(280)
        right_lay   = QVBoxLayout(right_frame)
        right_lay.setContentsMargins(4,4,4,4); right_lay.setSpacing(6)
        hdr_lbl = QLabel("Select a material slot")
        hdr_lbl.setFont(QFont("Arial",9,QFont.Weight.Bold))
        right_lay.addWidget(hdr_lbl)
        form = QFormLayout()
        form.setSpacing(8)
        tex_edit = QLineEdit()
        tex_edit.setEnabled(False)
        tex_edit.setPlaceholderText("texture_name")
        cache_lbl = QLabel("-")
        cache_lbl.setFixedWidth(80)
        load_btn = QPushButton("Load TXD…")
        load_btn.setFixedHeight(26)
        try: load_btn.setIcon(self.icon_factory.open_icon(color=ic)); load_btn.setIconSize(_QS(20,20))
        except Exception: pass

        load_btn.clicked.connect(self._load_txd_into_workshop)
        tr = QHBoxLayout(); tr.addWidget(tex_edit,1); tr.addWidget(cache_lbl); tr.addWidget(load_btn)
        form.addRow("Texture:", tr)

        def _update_cache_lbl(txt):  #vers 1
            t = txt.strip().lower()
            in_c = t in tex_cache
            cache_lbl.setText("in cache" if in_c else ("not found" if t else "-"))
            # FIX: Theme-aware color for cache status
            cache_lbl.setStyleSheet("color:palette(link);" if in_c else ("color:palette(text);" if t else "color:palette(placeholderText);"))

        tex_edit.textChanged.connect(_update_cache_lbl)
        ide_edit_lbl = QLabel()
        ide_edit_lbl.setVisible(False)
        reload_btn = QPushButton(); reload_btn.setFixedSize(26,26)

        try: reload_btn.setIcon(self.icon_factory.reset_icon(color=ic)); reload_btn.setIconSize(_QS(20,20))

        except Exception: reload_btn.setText("↻")
        reload_btn.setToolTip("Reload IDE entry (click to refresh top bar)")
        swap_combo = QComboBox()
        swap_combo.setEditable(True)
        swap_combo.setEnabled(False)

        if getattr(self,'_ide_txd_name',''):
            swap_combo.addItem(self._ide_txd_name)

        apply_btn = QPushButton("Apply")
        apply_btn.setFixedHeight(26)
        apply_btn.setEnabled(False)
        swap_row = QHBoxLayout()
        swap_row.addWidget(swap_combo, 1)
        swap_row.addWidget(apply_btn)
        swap_row.addWidget(reload_btn)

        form.addRow("Use TXD:", swap_row)
        _vp = getattr(self, 'preview_widget', None)
        chosen_color = [_vp._get_ui_color('viewport_text') if _vp and hasattr(_vp, '_get_ui_color') else __import__('PyQt6.QtGui', fromlist=['QColor']).QColor(200, 200, 200)]
        col_swatch = QLabel()
        col_swatch.setFixedSize(32,26)
        col_swatch.setFrameShape(QFrame.Shape.Box)
        pick_btn = QPushButton("Pick…")
        pick_btn.setFixedHeight(26)
        pick_btn.setEnabled(False)

        def _pick():  #vers 1
            from PyQt6.QtWidgets import QColorDialog
            c2 = QColorDialog.getColor(chosen_color[0], dlg, "Pick Colour")
            if c2.isValid():
                chosen_color[0] = c2
                col_swatch.setStyleSheet(f"background:{c2.name()};border:1px solid palette(mid);")

        pick_btn.clicked.connect(_pick)
        col_row = QHBoxLayout(); col_row.addWidget(col_swatch); col_row.addWidget(pick_btn); col_row.addStretch()
        form.addRow("Colour:", col_row)
        uv_lbl = QLabel("—"); uv_lbl.setStyleSheet("font-size:10px;color:palette(link);")
        form.addRow("UV Layer:", uv_lbl)

        def _import_tex():  #vers 1
            from PyQt6.QtWidgets import QFileDialog
            path,_ = QFileDialog.getOpenFileName(dlg,"Import Texture", getattr(self,'_texlist_folder','') or '', "Images (*.png *.jpg *.bmp *.tga);;All Files (*)")

            if not path: return
            try:
                qi = QImage(path)
                if qi.isNull(): return
                qi = qi.convertToFormat(QImage.Format.Format_RGBA8888)
                bdata = qi.bits().asarray(qi.width()*qi.height()*4)
                tname_cur = tex_edit.text().strip() or os.path.splitext(os.path.basename(path))[0]
                new_tex = {'name':tname_cur,'width':qi.width(),'height':qi.height(),
                           'format':'RGBA8888','mipmaps':1,'rgba_data':bytes(bdata)}
                lst = getattr(self,'_mod_textures',[])
                existing = next((t for t in lst if t.get('name','').lower()==tname_cur.lower()),None)
                if existing: existing.update(new_tex)
                else: lst.append(new_tex); self._mod_textures = lst
                if vp: vp._tex_cache[tname_cur.lower()] = qi; vp.update()
                _update_cache_lbl(tname_cur)
                _refresh_all_slots()

            except Exception as ex:

                if self.main_window and hasattr(self.main_window,'log_message'):
                    self.main_window.log_message(f"Import tex error: {ex}")

        def _export_tex():  #vers 1
            from PyQt6.QtWidgets import QFileDialog
            tname_cur = tex_edit.text().strip()

            if not tname_cur: return
            qi = (vp._tex_cache.get(tname_cur.lower()) if vp else None)

            if qi is None:
                raw = next((t for t in getattr(self,'_mod_textures',[])

                if t.get('name','').lower()==tname_cur.lower()),None)

                if raw:
                    rd=raw.get('rgba_data',b''); tw,th=raw.get('width',0),raw.get('height',0)
                    if rd and tw and th:
                        qi = QImage(rd[:tw*th*4],tw,th,tw*4,QImage.Format.Format_RGBA8888)

            if qi is None: return
            path,_ = QFileDialog.getSaveFileName(dlg,"Export Texture", os.path.join(getattr(self,'_texlist_folder','') or '',tname_cur+'.png'), "PNG (*.png);;BMP (*.bmp)")

            if path: qi.save(path)

        _tex_btns = []

        def _qb26_tex(label, tip, fn, icon_fn):  #vers 1
            b = QPushButton(label); b.setFixedHeight(26); b.setFont(self.button_font)
            try: b.setIcon(getattr(self.icon_factory,icon_fn)(color=ic)); b.setIconSize(_QS(16,16))
            except Exception: pass
            b.setToolTip(tip); b.clicked.connect(fn)
            b._full_label = label
            _tex_btns.append(b)
            return b

        io_row = QHBoxLayout(); io_row.setSpacing(4)
        io_row.addWidget(_qb26_tex("Import","Import image as texture",_import_tex,"import_icon"))
        io_row.addWidget(_qb26_tex("Export","Export texture to file",_export_tex,"export_icon"))
        io_row.addWidget(_qb26_tex("Save","Apply edits to texture cache", lambda: (self._populate_texture_list(), vp.load_textures(getattr(self,'_mod_textures',[]))

        if vp else None, vp.update()
        if vp else None),"save_icon"))

        io_row.addStretch()
        form.addRow("Textures:", io_row)
        pw = getattr(self, 'preview_widget', None)
        prev_row = QHBoxLayout(); prev_row.setSpacing(3)
        _prev_icons = {'solid':'solid_icon','textured':'texture_icon',
                        'semi':'semi_icon','wire':'wireframe_icon'}
        _prev_btns = []
        for style, label in [('solid','Solid'),('textured','Texture'),
                              ('semi','Semi'),('wire','Wire')]:
            b = QPushButton(label)
            b.setFixedHeight(26)
            b.setMinimumWidth(26); b.setMaximumWidth(80)
            b.setToolTip(f"{label} render mode")
            try:
                ico_fn = _prev_icons.get(style)
                if ico_fn:
                    b.setIcon(getattr(self.icon_factory, ico_fn)(color=ic))
                    b.setIconSize(_QS(16, 16))
            except Exception:
                pass
            if pw:
                rev = {'wire': 'wireframe', 'semi': 'solid', 'solid': 'solid', 'textured': 'textured'}
                b.clicked.connect(lambda _=False, s=style, p=pw: p.set_render_mode(rev.get(s, 'solid')))
            prev_row.addWidget(b)
            _prev_btns.append((b, label))
        prev_row.addStretch()
        form.addRow("Preview:", prev_row)
        right_lay.addLayout(form)
        right_lay.addStretch()
        ok_row = QHBoxLayout(); ok_row.addStretch()
        ok_btn = QPushButton("OK"); ok_btn.setFixedHeight(26)
        cancel_btn = QPushButton("Cancel")
        cancel_btn.setFixedHeight(26)
        close_btn  = QPushButton("Close")
        close_btn.setFixedHeight(26)
        ok_row.addWidget(ok_btn)
        ok_row.addWidget(cancel_btn)
        ok_row.addWidget(close_btn)
        right_lay.addLayout(ok_row)
        splitter.addWidget(right_frame)
        splitter.setSizes([380, 480])

        # - Wire IDE refresh
        def _refresh_ide():  #vers 1
            obj = self._current_ide_obj
            if not obj:
                xr = self._get_xref()
                if xr:
                    stem = os.path.splitext(getattr(self,'_original_dff_name','') or
                                            os.path.basename(getattr(self,'_current_dff_path','') or ''))[0].lower()
                    obj = xr.model_map.get(stem)
                    if obj:
                        draw = obj.extra.get('draw_dist',obj.extra.get('dist1','—'))
                        ide_edit_lbl.setText(
                            f"{obj.model_id}  {obj.model_name}  TXD:{obj.txd_name}  "
                            f"Draw:{draw}  [{obj.section}]")
                        swap_combo.setCurrentText(obj.txd_name or '')
                        swap_combo.setEnabled(True); apply_btn.setEnabled(True)
                        self._current_ide_obj = obj
                    else:
                        ide_edit_lbl.setText("Not found")

            self._ide_label_refresh_fn = _refresh_ide

        reload_btn.clicked.connect(_refresh_ide)

        def _apply_swap():  #vers 1
            new_txd = swap_combo.currentText().strip()
            if new_txd:
                self._ide_txd_name = new_txd
                from PyQt6.QtCore import QTimer as _QTs
                _QTs.singleShot(0, lambda t=new_txd: self._auto_load_txd_from_imgs(t))

        apply_btn.clicked.connect(_apply_swap)

        # - Slot selection → properties
        _current_mat = [None]

        def _refresh_all_slots():  #vers 1
            for i,(gi,mi,mat,geom) in enumerate(all_mats):
                if i < len(_slot_btns):
                    _,pix_lbl = _slot_btns[i]
                    pix_lbl.setPixmap(_make_slot_pix(mat, geom))

        def _on_slot_selected(i):  #vers 1
            if i >= len(all_mats): return

            gi, mi, mat, geom = all_mats[i]
            _current_mat[0] = mat
            tname = (getattr(mat,'texture_name','') or '').strip()
            c = getattr(mat,'colour',None) or getattr(mat,'color',None)
            uv_count = len(geom.uv_layers[0]) if geom.uv_layers else 0
            hdr_lbl.setText(f"Mat {mi}  —  {tname or '(no texture)'}")
            tex_edit.setText(tname); tex_edit.setEnabled(True)
            pick_btn.setEnabled(True)
            _update_cache_lbl(tname)

            if c:
                chosen_color[0] = QColor(c.r,c.g,c.b)
                col_swatch.setStyleSheet(
                    f"background:{chosen_color[0].name()};border:1px solid palette(mid);")
            uv_lbl.setText(f"✓ {uv_count} UV coords" if uv_count else "—")
            uv_lbl.setStyleSheet(
                "color:palette(link);font-size:10px;" if uv_count else "color:palette(placeholderText);font-size:10px;")
            _refresh_ide()

        def _apply_and_close():  #vers 1
            mat = _current_mat[0]

            if mat:
                mat.texture_name = tex_edit.text().strip()
                c2 = chosen_color[0]
                mc = getattr(mat,'colour',None) or getattr(mat,'color',None)
                if mc: mc.r=c2.red(); mc.g=c2.green(); mc.b=c2.blue()
                if vp: vp.update()
            dlg.accept()

        ok_btn.clicked.connect(_apply_and_close)
        cancel_btn.clicked.connect(dlg.reject)
        close_btn.clicked.connect(dlg.accept)
        # Select first slot

        if all_mats:
            _slot_btns[0][0].setStyleSheet(
                "QFrame{border:2px solid palette(highlight);border-radius:3px;background:palette(alternateBase);}")
            _on_slot_selected(0)
            _refresh_ide()
        dlg.exec()


    def _open_paint_editor(self): #vers 5
        """Enter paint mode — works on DFF model materials or COL faces.
        When a DFF is loaded, opens the Material List for texture painting.
        When a COL is loaded, enters face-paint mode for surface types."""
        vp = getattr(self, 'preview_widget', None)

        # DFF mode: open material list dialog for texture assignment
        dff = getattr(self, '_current_dff_model', None)
        if dff is not None:
            self._open_dff_material_list()
            return

        # COL mode: enter face-paint mode
        if not getattr(self, "current_col_file", None):
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "No Model",
                "Load a DFF or COL file first.")
            return
        model = self._get_selected_model()
        if model is None:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "No Model Selected",
                "Select a collision model in the list first.")
            return
        if not getattr(model, 'faces', []):
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "No Mesh Faces",
                f"'{model.name}' has no mesh faces to paint.")
            return

        if not vp:
            return

        models    = getattr(self.current_col_file, 'models', [])
        model_idx = models.index(model) if model in models else -1

        # Use last active mat or default 0
        mat_id  = getattr(self, '_paint_active_mat', 0)

        # Push one undo snapshot on entry
        if model_idx >= 0:
            self._push_undo(model_idx, f"Enter paint mode")

        # Cache the full material list for this model's version
        try:
            from apps.methods.col_materials import get_materials_for_version, COLGame
            ver     = getattr(getattr(model,'version',None),'value',3) if model else 3
            game    = COLGame.VC if ver == 1 else COLGame.SA
            self._paint_mat_list = get_materials_for_version(game, include_procedural=True)
        except Exception:
            self._paint_mat_list = [(i, f"Material {i}", "808080") for i in range(64)]

        # Set current index into the list
        mat_ids = [m[0] for m in self._paint_mat_list]
        self._paint_mat_idx = mat_ids.index(mat_id) if mat_id in mat_ids else 0
        mat_id = self._paint_mat_list[self._paint_mat_idx][0]
        self._paint_active_mat = mat_id

        # Enter viewport paint mode
        vp.set_paint_mode(True, mat_id)
        vp.on_face_selected = self._on_painted_face
        vp._paint_material  = mat_id
        vp.update()  # draw overlay immediately

        # Update paint button to show exit state
        for btn in self._find_all_paint_btns():
            try: btn.clicked.disconnect()
            except: pass
            btn.clicked.connect(self._exit_paint_mode)
            btn.setText("[ ] Exit Paint")
            btn.setStyleSheet("color: palette(windowText); font-weight:bold;")

        self._set_status(
            "Paint mode - click faces to paint  |  ◀▶ change material  "
            "|  Shift+drag to select  |  Esc to exit")


    def _open_paint_mat_popup(self): #vers 2
        """Searchable material popup anchored below the mat chip.
        Closes on item click, X button, or focus loss."""
        from PyQt6.QtWidgets import (QListWidget, QListWidgetItem, QFrame,
                                     QVBoxLayout, QHBoxLayout, QLineEdit,
                                     QPushButton)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QColor

        lst = getattr(self, '_paint_mat_list', [])
        if not lst: return

        vp = getattr(self, 'preview_widget', None)
        if not vp: return

        # Close any existing popup
        old = getattr(self, '_mat_popup', None)
        if old:
            try: old.hide(); old.deleteLater()
            except: pass
            self._mat_popup = None

        popup = QFrame(vp)
        popup.setFrameStyle(QFrame.Shape.StyledPanel)
        popup.setStyleSheet(
            "QFrame { background:palette(base); border:1px solid #ff8c00; border-radius:4px; }"
            "QListWidget { background:palette(base); color:palette(windowText); border:none; }"
            "QListWidget::item { padding:2px 4px; }"
            "QListWidget::item:hover { background:palette(base); }"
            "QListWidget::item:selected { background:#ff8c00; color:#000; }"
            "QLineEdit { background:palette(base); color:palette(windowText); border:1px solid palette(mid); "
            "            border-radius:3px; padding:2px 4px; }"
            "QPushButton { background:transparent; color:#ff6b35; border:none; "
            "              font-weight:bold; font-size:14px; }"
            "QPushButton:hover { color:#ff3300; }"
        )

        lay = QVBoxLayout(popup)
        lay.setContentsMargins(6, 4, 6, 6)
        lay.setSpacing(4)

        # Header: search + X
        hdr = QHBoxLayout()
        search = QLineEdit()
        search.setPlaceholderText("Filter materials…")
        search.setFixedHeight(26)
        hdr.addWidget(search)
        close_btn = QPushButton("✕")
        close_btn.setFixedSize(22, 22)
        close_btn.setToolTip("Close")
        hdr.addWidget(close_btn)
        lay.addLayout(hdr)

        lw = QListWidget()
        lw.setFixedHeight(220)
        lw.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        lay.addWidget(lw)

        ws = self

        def _close():  #vers 1
            popup.hide()
            popup.deleteLater()
            ws._mat_popup = None

        close_btn.clicked.connect(_close)

        def _populate(flt=""):  #vers 1
            lw.clear()
            for mid, name, hex_col in lst:
                if flt and flt.lower() not in name.lower()                         and flt not in str(mid):
                    continue
                item = QListWidgetItem(f"  {mid:3d}  {name}")
                item.setData(Qt.ItemDataRole.UserRole, mid)
                c = QColor(f"#{hex_col}")
                item.setBackground(
                    QColor(max(0,c.red()//4), max(0,c.green()//4),
                           min(255, c.blue()//4 + 15)))
                item.setForeground(c.lighter(200))
                lw.addItem(item)
            # Scroll to current material
            cur_id = getattr(ws, '_paint_active_mat', 0)
            for i in range(lw.count()):
                if lw.item(i).data(Qt.ItemDataRole.UserRole) == cur_id:
                    lw.setCurrentRow(i)
                    lw.scrollToItem(lw.item(i))
                    break

        _populate()
        search.textChanged.connect(_populate)

        def _pick(item):  #vers 1
            mid = item.data(Qt.ItemDataRole.UserRole)
            if mid is None: return
            mat_ids = [m[0] for m in lst]
            ws._paint_mat_idx    = mat_ids.index(mid) if mid in mat_ids else 0
            ws._paint_active_mat = mid
            vp._paint_material   = mid
            vp.update()
            _close()

        lw.itemClicked.connect(_pick)
        lw.itemDoubleClicked.connect(_pick)

        # Position: anchored below the mat chip, aligned to right edge
        W = vp.width()
        _MARGIN = 8; _MAT_W = 200; _ROW1_Y = 4; _CHIP_H = 26; _ARW = 22
        pw = 200   # popup width
        px = W - _MAT_W - _MARGIN + _ARW     # left-align with mat name chip
        py = _ROW1_Y + _CHIP_H + 4           # just below mat row
        # Keep inside viewport
        px = max(4, min(px, W - pw - 4))
        popup.move(px, py)
        popup.resize(pw, 264)
        popup.show()
        popup.raise_()
        self._mat_popup = popup
        search.setFocus()


    def _apply_to_selected_faces_paint(self): #vers 1
        """Apply current paint material to all selected faces (F key in paint mode)."""
        vp = getattr(self, 'preview_widget', None)
        if not vp: return
        sel = sorted(getattr(vp, '_selected_faces', set()))
        if not sel:
            self._set_status("No faces selected — click or drag to select faces first")
            return
        model = self._get_selected_model()
        if not model: return
        models = getattr(self.current_col_file, 'models', [])
        mi = models.index(model) if model in models else -1
        mat_id = getattr(self, '_paint_active_mat', 0)
        if mi >= 0:
            self._push_undo(mi, f"Paint {mat_id} → {len(sel)} selected faces")
        for fi in sel:
            if fi < len(model.faces):
                f = model.faces[fi]
                if hasattr(f.material, 'material_id'):
                    f.material.material_id = mat_id
                else:
                    f.material = mat_id
        vp.update()
        self._set_status(f"Applied material {mat_id} to {len(sel)} selected face(s)")

    def _paint_cycle_mat(self, delta: int): #vers 1
        """Cycle active paint material by delta steps (+1 next / -1 prev)."""
        lst = getattr(self, '_paint_mat_list', [])
        if not lst: return
        idx = getattr(self, '_paint_mat_idx', 0)
        idx = (idx + delta) % len(lst)
        self._paint_mat_idx   = idx
        mat_id, name, hex_col = lst[idx]
        self._paint_active_mat = mat_id
        vp = getattr(self, 'preview_widget', None)
        if vp:
            vp._paint_material = mat_id
            vp.update()
        self._set_status(f"Paint material: {mat_id} — {name}")


    def _on_painted_face(self, face_index, face): #vers 2
        """Called by viewport when a face is painted. Status update only —
        undo state was pushed before entering paint mode."""
        mat_id = self._paint_active_mat if hasattr(self, '_paint_active_mat') else 0
        self._set_status(f"Painted face {face_index} → material {mat_id}  [Esc to exit]")

    def _set_paint_tool(self, mode: str): #vers 1
        """Switch active paint tool: 'paint' | 'dropper' | 'fill'."""
        self._current_paint_tool = mode
        vp = getattr(self, 'preview_widget', None)
        if vp:
            vp._tool_mode = mode
            if mode == 'dropper':
                from PyQt6.QtCore import Qt
                vp.setCursor(Qt.CursorShape.PointingHandCursor)
            elif mode == 'fill':
                from PyQt6.QtCore import Qt
                vp.setCursor(Qt.CursorShape.CrossCursor)
            else:
                from PyQt6.QtCore import Qt
                vp.setCursor(Qt.CursorShape.CrossCursor)
        # Update button check states
        for btn, name in [
            (getattr(self, 'tool_paint_btn', None), 'paint'),
            (getattr(self, 'tool_dropper_btn', None), 'dropper'),
            (getattr(self, 'tool_fill_btn', None), 'fill'),
        ]:
            if btn:
                btn.setChecked(name == mode)
        tool_names = {'paint': 'Paint', 'dropper': 'Dropper (pick material)', 'fill': 'Fill (same material)'}
        self._set_status(f"Tool: {tool_names.get(mode, mode)}")

    def _exit_paint_mode(self): #vers 3
        """Exit paint mode — hide toolbar, restore paint button."""
        # Close material popup if open
        old_popup = getattr(self, '_mat_popup', None)
        if old_popup:
            try: old_popup.hide(); old_popup.deleteLater()
            except: pass
            self._mat_popup = None

        vp = getattr(self, 'preview_widget', None)
        if vp and getattr(vp, '_paint_mode', False):
            vp.set_paint_mode(False)
            vp.on_face_selected = None

        # Hide QWidget paint bar if it was created, refresh viewport
        tb = getattr(self, 'paint_toolbar', None)
        if tb:
            tb.hide()
        vp = getattr(self, 'preview_widget', None)
        if vp:
            vp.update()

        # Reset paint button in both icon and text panels
        for btn in self._find_all_paint_btns():
            try: btn.clicked.disconnect()
            except: pass
            btn.clicked.connect(self._open_paint_editor)
            btn.setText("Paint")
            btn.setStyleSheet("")
            btn.setChecked(False) if btn.isCheckable() else None

        self._set_status("Paint mode exited.")

    def _on_paint_mode_exited(self): #vers 1
        """Called by viewport Escape key — sync button state."""
        self._exit_paint_mode()

    def _dff_get_geometries(self): #vers 1
        """Return geometry list from current DFF model or None."""
        model = getattr(self, '_current_dff_model', None)
        if not model:
            QMessageBox.warning(self, "No DFF", "No DFF loaded.")
            return None
        geoms = getattr(model, 'geometries', [])
        if not geoms:
            QMessageBox.warning(self, "No Geometry", "DFF has no geometry.")
            return None
        return geoms

    def _dff_get_selected_geom_idx(self): #vers 1
        """Return index of selected geometry in the Models list, or 0."""
        lw = getattr(self, 'mod_compact_list', None) or getattr(self, 'collision_list', None)
        if lw:
            row = lw.currentRow()
            if row >= 0:
                return row
        return 0

    def _dff_transform_vertices(self, fn): #vers 2
        """Apply fn(x,y,z)->x,y,z to every vertex in every geometry. Refreshes viewport."""
        from apps.methods.dff_classes import Vector3
        geoms = self._dff_get_geometries()
        if not geoms:
            return
        for geom in geoms:
            verts = getattr(geom, 'vertices', None)
            if not verts:
                continue
            new_verts = []
            for v in verts:
                nx, ny, nz = fn(v.x, v.y, v.z)
                new_verts.append(Vector3(nx, ny, nz))
            geom.vertices = new_verts
        # Rebuild adapters so viewport sees updated data
        dff_model = getattr(self, '_current_dff_model', None)
        if dff_model and hasattr(self, '_dff_adapters'):
            atomics = getattr(dff_model, 'atomics', [])
            frames  = getattr(dff_model, 'frames', [])
            for i, geom in enumerate(dff_model.geometries):
                atomic = next((a for a in atomics if a.geometry_index == i), None)
                from apps.components.Model_Editor.model_workshop import _DFFGeometryAdapter
                self._dff_adapters[i] = _DFFGeometryAdapter(geom, i, dff_model=dff_model, atomic=atomic)
        vp = getattr(self, 'preview_widget', None)
        if vp:
            row = self._dff_get_selected_geom_idx()
            if 0 <= row < len(getattr(self, '_dff_adapters', [])):
                vp.set_current_model(self._dff_adapters[row], row)
            vp.update()
        self._set_status("Geometry transformed.")

    def _dff_flip_x(self): #vers 1
        """Flip DFF geometry on X axis (left/right)."""
        self._dff_transform_vertices(lambda x, y, z: (-x, y, z))

    def _dff_flip_y(self): #vers 1
        """Flip DFF geometry on Y axis (up/down)."""
        self._dff_transform_vertices(lambda x, y, z: (x, -y, z))

    def _dff_rotate_cw(self): #vers 1
        """Rotate DFF geometry 90° clockwise around Z axis."""
        self._dff_transform_vertices(lambda x, y, z: (y, -x, z))

    def _dff_rotate_ccw(self): #vers 1
        """Rotate DFF geometry 90° counter-clockwise around Z axis."""
        self._dff_transform_vertices(lambda x, y, z: (-y, x, z))

    def _dff_analyze(self): #vers 1
        """Show DFF mesh statistics dialog."""
        model = getattr(self, '_current_dff_model', None)
        if not model:
            QMessageBox.warning(self, "No DFF", "No DFF loaded.")
            return
        geoms   = getattr(model, 'geometries', [])
        frames  = getattr(model, 'frames',     [])
        atomics = getattr(model, 'atomics',    [])
        total_v = sum(len(getattr(g, 'vertices', [])) for g in geoms)
        total_t = sum(len(getattr(g, 'triangles', [])) for g in geoms)
        total_m = sum(len(getattr(g, 'materials', [])) for g in geoms)
        name = getattr(self, '_original_dff_name', None) or \
               getattr(self, '_current_dff_path', 'unknown')
        import os as _os
        name = _os.path.basename(name)
        lines = [
            f"File:       {name}",
            f"Geometries: {len(geoms)}",
            f"Frames:     {len(frames)}",
            f"Atomics:    {len(atomics)}",
            f"Vertices:   {total_v:,}",
            f"Triangles:  {total_t:,}",
            f"Materials:  {total_m}",
        ]
        for i, g in enumerate(geoms):
            vs = len(getattr(g, 'vertices',  []))
            ts = len(getattr(g, 'triangles', []))
            ms = len(getattr(g, 'materials', []))
            lines.append(f"\nGeometry [{i}]:  {vs}v  {ts}t  {ms} mats")
        QMessageBox.information(self, "DFF Analysis", "\n".join(lines))

    def _dff_copy_geometry(self): #vers 1
        """Copy selected geometry to internal clipboard."""
        geoms = self._dff_get_geometries()
        if not geoms:
            return
        import copy
        idx = self._dff_get_selected_geom_idx()
        idx = min(idx, len(geoms)-1)
        self._dff_clipboard_geom = copy.deepcopy(geoms[idx])
        b = getattr(self, 'paste_btn', None)
        if b:
            b.setEnabled(True)
        self._set_status(f"Geometry [{idx}] copied.")

    def _dff_paste_geometry(self): #vers 2
        """Paste geometry from clipboard into current DFF."""
        if not hasattr(self, '_dff_clipboard_geom') or self._dff_clipboard_geom is None:
            QMessageBox.warning(self, "Paste", "Nothing in clipboard.")
            return
        model = getattr(self, '_current_dff_model', None)
        if not model:
            return
        import copy
        geoms = getattr(model, 'geometries', [])
        geoms.append(copy.deepcopy(self._dff_clipboard_geom))
        model.geometries = geoms
        # Rebuild full display so adapters are recreated
        self._display_dff_model(model)
        self._set_status("Geometry pasted.")

    def _dff_delete_geometry(self): #vers 1
        """Delete selected geometry from DFF."""
        geoms = self._dff_get_geometries()
        if not geoms:
            return
        idx = self._dff_get_selected_geom_idx()
        idx = min(idx, len(geoms)-1)
        if len(geoms) == 1:
            QMessageBox.warning(self, "Delete", "Cannot delete the only geometry.")
            return
        reply = QMessageBox.question(self, "Delete Geometry",
            f"Delete geometry [{idx}]? This cannot be undone.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
        del geoms[idx]
        model = getattr(self, '_current_dff_model', None)
        self._populate_dff_detail_table(model)
        vp = getattr(self, 'preview_widget', None)
        if vp:
            vp._model = model
            vp.update()
        self._set_status(f"Geometry [{idx}] deleted.")

    def _dff_duplicate_geometry(self): #vers 2
        """Duplicate selected geometry (own frame + atomic, saved with the DFF)."""
        import copy
        geoms = self._dff_get_geometries()
        if not geoms:
            return
        idx = min(self._dff_get_selected_geom_idx(), len(geoms) - 1)
        self._dff_append_geometry(copy.deepcopy(geoms[idx]), idx,
                                  f"{self._mesh_frame_name(idx)[:18]}_copy", f"Geometry [{idx}] duplicated")

    def _mirror_dialog(self): #vers 2
        """Mirror selection (or whole geometry) on chosen axes."""
        from PyQt6.QtWidgets import QInputDialog
        from apps.methods import col_mesh_ops as ops
        axes, ok = QInputDialog.getItem(self, "Mirror", "Axes:", ["X", "Y", "Z", "XY", "XZ", "YZ", "XYZ"], 0, False)
        if not ok:
            return
        faces, verts = self._mesh_sel()
        tgt = self._mesh_target()
        if tgt is None:
            return
        pivot = ops.selection_centre(self._mesh_preview(tgt[1]), faces, verts)
        self._mesh_run(f"Mirror {axes}", ops.mirror, faces, axes, pivot, verts or None)

    def _align_dialog(self): #vers 2
        """Flatten selected vertices/faces onto their average on one axis."""
        from PyQt6.QtWidgets import QInputDialog
        from apps.methods import col_mesh_ops as ops
        faces, verts = self._mesh_sel()
        if not faces and not verts:
            self._set_status("Align: select vertices or faces first")
            return
        axis, ok = QInputDialog.getItem(self, "Align (flatten)", "Axis:", ["X", "Y", "Z"], 2, False)
        if not ok:
            return
        tgt = self._mesh_target()
        if tgt is None:
            return
        k = 'xyz'.index(axis.lower())
        c = ops.selection_centre(self._mesh_preview(tgt[1]), faces, verts)[k]
        sc = [1.0, 1.0, 1.0]; sc[k] = 0.0
        pivot = [0.0, 0.0, 0.0]; pivot[k] = c
        self._mesh_run(f"Align {axis}", ops.scale, faces, sc[0], sc[1], sc[2], tuple(pivot), verts or None)

    # - Primitive creation

    def _create_primitive_dialog(self): #vers 1
        """Show dialog to create a primitive shape (box or sphere) as a new DFF geometry."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QFormLayout,
            QComboBox, QSpinBox, QDoubleSpinBox, QDialogButtonBox, QGroupBox)

        dlg = QDialog(self)
        dlg.setWindowTitle("Create Primitive")
        dlg.setMinimumWidth(300)
        lay = QVBoxLayout(dlg)
        form = QFormLayout()

        type_combo = QComboBox()
        type_combo.addItems(["Box", "Sphere", "Cylinder", "Plane"])
        form.addRow("Shape:", type_combo)

        # Dimensions
        dim_grp = QGroupBox("Dimensions")
        dim_lay = QFormLayout(dim_grp)
        sx = QDoubleSpinBox(); sx.setRange(0.01, 1000); sx.setValue(1.0); sx.setSingleStep(0.5)
        sy = QDoubleSpinBox(); sy.setRange(0.01, 1000); sy.setValue(1.0); sy.setSingleStep(0.5)
        sz = QDoubleSpinBox(); sz.setRange(0.01, 1000); sz.setValue(1.0); sz.setSingleStep(0.5)
        dim_lay.addRow("Width (X):",  sx)
        dim_lay.addRow("Height (Y):", sy)
        dim_lay.addRow("Depth (Z):",  sz)
        lay.addLayout(form)
        lay.addWidget(dim_grp)

        # Subdivisions
        sub_grp = QGroupBox("Subdivisions")
        sub_lay = QFormLayout(sub_grp)
        seg_x = QSpinBox(); seg_x.setRange(1, 64); seg_x.setValue(1)
        seg_y = QSpinBox(); seg_y.setRange(1, 64); seg_y.setValue(1)
        seg_z = QSpinBox(); seg_z.setRange(1, 64); seg_z.setValue(1)
        sub_lay.addRow("Segments X:", seg_x)
        sub_lay.addRow("Segments Y:", seg_y)
        sub_lay.addRow("Segments Z:", seg_z)
        lay.addWidget(sub_grp)

        # Hide irrelevant fields for sphere
        def _on_type_changed(t):  #vers 1
            is_box = t in ("Box", "Plane")
            seg_z.setEnabled(is_box)
            sz.setEnabled(t != "Plane")
        type_combo.currentTextChanged.connect(_on_type_changed)

        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok |
            QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        lay.addWidget(btns)

        if dlg.exec() != QDialog.DialogCode.Accepted:
            return

        shape  = type_combo.currentText()
        w, h, d = sx.value(), sy.value(), sz.value()
        nx, ny, nz = seg_x.value(), seg_y.value(), seg_z.value()

        try:
            verts, tris = self._build_primitive(shape, w, h, d, nx, ny, nz)
            self._add_geometry_to_dff(verts, tris, name=f"{shape.lower()}_01")
            self._set_status(
                f"Created {shape}: {w:.2f}×{h:.2f}×{d:.2f}  "
                f"segs {nx}×{ny}×{nz}  "
                f"({len(verts)} verts, {len(tris)} tris)")
        except Exception as e:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Primitive Error", str(e))

    def _extrude_dialog(self): #vers 1
        """Prompt for extrude distance and apply to the current face/poly
        selection in the viewport. Positive = outward, negative = inward
        (push-in, e.g. a recessed door/window/vent)."""
        from PyQt6.QtWidgets import QInputDialog, QMessageBox

        vp = getattr(self, 'preview_widget', None)
        if vp is None:
            return

        mode = getattr(vp, '_select_mode', 'face')
        if mode not in ('face', 'poly'):
            QMessageBox.information(self, "Extrude",
                "Switch to Face or Polygon select mode and select one or "
                "more faces first.")
            return

        if not getattr(vp, '_selected_faces', None):
            QMessageBox.information(self, "Extrude",
                "Select one or more faces first, then Extrude.")
            return

        distance, ok = QInputDialog.getDouble(
            self, "Extrude", "Distance (negative = push in):",
            0.5, -1000.0, 1000.0, 3)
        if not ok or distance == 0.0:
            return

        try:
            success = vp._extrude_selected_faces(distance)
            if success:
                self._set_status(f"Extruded {len(vp._selected_faces)} face(s) by {distance:.3f}")
            else:
                QMessageBox.warning(self, "Extrude",
                    "Nothing to extrude — load a DFF and select faces first.")
        except Exception as e:
            QMessageBox.warning(self, "Extrude Error", str(e))

    def _build_primitive(self, shape: str,
                          w: float, h: float, d: float,
                          nx: int, ny: int, nz: int):  #vers 1
        """Build vertex + triangle lists for a primitive.
        Returns (verts: list[(x,y,z)], tris: list[(i,j,k)])."""
        verts, tris = [], []

        if shape == "Box":
            # Generate a subdivided box
            # Each face: (nx×ny), (nx×nz), (ny×nz) quads → 2 tris each
            def _face_quad(p0, pu, pv, su, sv):  #vers 1
                """Add a subdivided quad face."""
                base = len(verts)
                for iv in range(sv + 1):
                    for iu in range(su + 1):
                        x = p0[0] + pu[0]*iu/su + pv[0]*iv/sv
                        y = p0[1] + pu[1]*iu/su + pv[1]*iv/sv
                        z = p0[2] + pu[2]*iu/su + pv[2]*iv/sv
                        verts.append((x, y, z))
                for iv in range(sv):
                    for iu in range(su):
                        i0 = base + iv*(su+1) + iu
                        i1 = i0 + 1
                        i2 = i0 + (su+1)
                        i3 = i2 + 1
                        tris.append((i0, i1, i2))
                        tris.append((i1, i3, i2))

            hw, hh, hd = w/2, h/2, d/2
            # +Y top
            _face_quad((-hw, hh, -hd), (w,0,0), (0,0,d), nx, nz)
            # -Y bottom
            _face_quad((-hw,-hh,  hd), (w,0,0), (0,0,-d), nx, nz)
            # +Z front
            _face_quad((-hw,-hh,  hd), (w,0,0), (0,h,0), nx, ny)
            # -Z back
            _face_quad((-hw,-hh, -hd), (w,0,0), (0,h,0), nx, ny)
            # -X left
            _face_quad((-hw,-hh, -hd), (0,0,d), (0,h,0), nz, ny)
            # +X right
            _face_quad(( hw,-hh,  hd), (0,0,-d), (0,h,0), nz, ny)

        elif shape == "Sphere":
            import math
            rings  = max(ny, 2)
            slices = max(nx * 2, 4)
            # Poles
            verts.append((0,  h/2, 0))  # top
            verts.append((0, -h/2, 0))  # bottom
            top_idx, bot_idx = 0, 1
            # Ring vertices
            for ri in range(1, rings):
                phi = math.pi * ri / rings
                y   = (h/2) * math.cos(phi)
                r   = (w/2) * math.sin(phi)
                for si in range(slices):
                    theta = 2 * math.pi * si / slices
                    verts.append((r * math.cos(theta), y, r * math.sin(theta)))
            ring_base = 2
            def _ri(ring, seg): return ring_base + ring * slices + (seg % slices)  #vers 1
            # Cap triangles
            for si in range(slices):
                tris.append((top_idx, _ri(0, si+1), _ri(0, si)))
                tris.append((bot_idx, _ri(rings-2, si), _ri(rings-2, si+1)))
            # Body quads
            for ri in range(rings - 2):
                for si in range(slices):
                    tris.append((_ri(ri,si), _ri(ri,si+1), _ri(ri+1,si)))
                    tris.append((_ri(ri,si+1), _ri(ri+1,si+1), _ri(ri+1,si)))

        elif shape == "Plane":
            hw, hd = w/2, d/2
            for iz in range(nz + 1):
                for ix in range(nx + 1):
                    verts.append((-hw + w*ix/nx, 0, -hd + d*iz/nz))
            for iz in range(nz):
                for ix in range(nx):
                    i0 = iz*(nx+1) + ix
                    i1 = i0 + 1
                    i2 = i0 + (nx+1)
                    i3 = i2 + 1
                    tris.append((i0, i1, i2))
                    tris.append((i1, i3, i2))

        elif shape == "Cylinder":
            import math
            slices = max(nx * 4, 8)
            hw, hh = w/2, h/2
            # Top + bottom centre
            verts.append((0,  hh, 0)); top_c = 0
            verts.append((0, -hh, 0)); bot_c = 1
            # Top ring
            top_base = len(verts)
            for si in range(slices):
                theta = 2*math.pi*si/slices
                verts.append((hw*math.cos(theta), hh, hw*math.sin(theta)))
            # Bottom ring
            bot_base = len(verts)
            for si in range(slices):
                theta = 2*math.pi*si/slices
                verts.append((hw*math.cos(theta), -hh, hw*math.sin(theta)))
            # Body quads + caps
            for si in range(slices):
                nsi = (si+1) % slices
                t0, t1 = top_base+si, top_base+nsi
                b0, b1 = bot_base+si, bot_base+nsi
                tris.append((t0, t1, b0))
                tris.append((t1, b1, b0))
                tris.append((top_c, t1, t0))
                tris.append((bot_c, b0, b1))

        return verts, tris

    def _add_geometry_to_dff(self, verts: list, tris: list,
                              name: str = "primitive"): #vers 2
        """Add a new geometry to the current DFF model from raw verts+tris.
        If no DFF model is loaded yet, creates a new blank one first so
        primitives can be built completely from scratch.
        Builds a real Geometry/Triangle/Vector3 (not a duck-typed stand-in)
        so the result is identical to a loaded DFF and saves correctly."""
        import math
        from apps.methods.dff_classes import (
            DFFModel, Geometry, Triangle, Vector3, TexCoord, Frame, Atomic,
            BoundingSphere)

        model = getattr(self, '_current_dff_model', None)
        if model is None:
            # Nothing loaded — create a brand new empty model with one
            # root frame so the primitive has somewhere to attach.
            model = DFFModel()
            model.frames.append(Frame(name="root", parent_index=-1))
            self._current_dff_model = model
            self._current_dff_path = None
            self.current_col_file = None
            if hasattr(self, 'format_combo'):
                self.format_combo.setVisible(False)

        geom = Geometry()
        geom.vertices = [Vector3(x, y, z) for (x, y, z) in verts]
        geom.triangles = [Triangle(v1=i0, v2=i1, v3=i2, material_id=0)
                           for (i0, i1, i2) in tris]
        geom.uv_layers = [[TexCoord(0.0, 0.0) for _ in verts]]

        # Per-vertex normals: accumulate face normals, then normalise
        vert_normals = [[0.0, 0.0, 0.0] for _ in verts]
        for i0, i1, i2 in tris:
            v0, v1, v2 = verts[i0], verts[i1], verts[i2]
            ax, ay, az = v1[0]-v0[0], v1[1]-v0[1], v1[2]-v0[2]
            bx, by, bz = v2[0]-v0[0], v2[1]-v0[1], v2[2]-v0[2]
            nx = ay*bz - az*by
            ny = az*bx - ax*bz
            nz = ax*by - ay*bx
            ln = math.sqrt(nx*nx + ny*ny + nz*nz) or 1.0
            nx, ny, nz = nx/ln, ny/ln, nz/ln
            for idx in (i0, i1, i2):
                vert_normals[idx][0] += nx
                vert_normals[idx][1] += ny
                vert_normals[idx][2] += nz
        normals = []
        for nx, ny, nz in vert_normals:
            ln = math.sqrt(nx*nx + ny*ny + nz*nz) or 1.0
            normals.append(Vector3(nx/ln, ny/ln, nz/ln))
        geom.normals = normals

        # Bounding sphere — centre + radius enclosing all verts
        if verts:
            cx = sum(v[0] for v in verts) / len(verts)
            cy = sum(v[1] for v in verts) / len(verts)
            cz = sum(v[2] for v in verts) / len(verts)
            radius = max(math.sqrt((v[0]-cx)**2 + (v[1]-cy)**2 + (v[2]-cz)**2)
                         for v in verts)
            geom.bounding_sphere = BoundingSphere(center=Vector3(cx, cy, cz),
                                                   radius=radius)

        geom_index = len(model.geometries)
        model.geometries.append(geom)

        # New geometry needs a frame + atomic to attach to so the frame
        # hierarchy / Model Viewer can see it; attach to the root frame.
        model.atomics.append(Atomic(frame_index=0, geometry_index=geom_index))

        # Refresh viewport / mesh list
        self._display_dff_model(model)
        self._update_select_mode_availability()
        mw = self.main_window
        if mw and hasattr(mw, 'log_message'):
            mw.log_message(
                f"Primitive '{name}': {len(verts)} verts, {len(tris)} tris added to DFF")


    # Lighting is applied in paintEvent per-face using dot(normal, light_dir)
    # This modifies the material colour before drawing solid/semi faces.
    # For textured faces a semi-transparent shading overlay is applied.

    def _compute_face_shade(self, v0, v1, v2, ambient=0.30,
                            light=(0.5, 0.5, 0.8)): #vers 3
        # light=(0.5, 0.5, 0.8) = upper-front-right, suits GTA camera angles
        """Return a shade factor [0..1] for a triangle via Lambertian diffuse.
        v0/v1/v2 are (x,y,z) tuples in view space.
        light = normalised direction toward light source."""
        import math
        ax, ay, az = v1[0]-v0[0], v1[1]-v0[1], v1[2]-v0[2]
        bx, by, bz = v2[0]-v0[0], v2[1]-v0[1], v2[2]-v0[2]
        nx = ay*bz - az*by
        ny = az*bx - ax*bz
        nz = ax*by - ay*bx
        ln = math.sqrt(nx*nx + ny*ny + nz*nz)
        if ln < 1e-6:
            return ambient
        nx, ny, nz = nx/ln, ny/ln, nz/ln
        lx, ly, lz = light[0], light[1], light[2]
        ll = math.sqrt(lx*lx + ly*ly + lz*lz) or 1.0
        dot = max(0.0, nx*lx/ll + ny*ly/ll + nz*lz/ll)
        return min(1.0, ambient + (1.0 - ambient) * dot)

    def _push_undo(self, model_index, description=""): #vers 1
        """Deep-copy model[model_index] onto undo stack before any edit."""
        import copy
        if not getattr(self, "current_col_file", None):
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

    def _undo_last_action(self): #vers 3
        """Restore the last DFF mesh edit, else the last COL model from the undo stack."""
        try:
            if getattr(self, '_mesh_undo', None) and not getattr(self, 'current_col_file', None):
                self._mesh_undo_last()
                return
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


    #    DFF mesh editing (Oct 12 2026): col_mesh_ops on DFF geometry

    def _mesh_target(self): #vers 1
        """(index, Geometry) of the geometry selected in the list, or None."""
        model = getattr(self, '_current_dff_model', None)
        if not model or not model.geometries:
            self._set_status("Mesh edit: open a model first")
            return None
        idx = max(0, self._dff_get_selected_geom_idx())
        return (idx, model.geometries[idx]) if idx < len(model.geometries) else None

    def _mesh_preview(self, geom): #vers 1
        """Read-only mesh view of a geometry (for centres and lookups)."""
        from apps.methods.dff_mesh_ops import geometry_to_mesh
        return geometry_to_mesh(geom)

    def _mesh_sel(self): #vers 1
        """(face ids, vertex ids) selected in the viewport."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None:
            return set(), set()
        mode = getattr(vp, '_select_mode', 'object')
        if mode == 'vertex':
            return set(), set(vp._selected_verts)
        if mode in ('face', 'poly'):
            return set(vp._selected_faces), set()
        return set(), set()

    def _mesh_show(self, idx, geom, fit=False, keep_sel=False): #vers 4
        """Show one geometry in the viewport; selection kept if indices still valid."""
        detached = set().union(*[g for g in self._mesh_detached_groups(geom)]) if \
            getattr(geom, 'detached_groups', None) else set()
        for vp in self._live_viewports():
            vp.load_geometry(geom, geom.materials, fit=fit)
            vp.set_detached_faces(detached)
            if not keep_sel:
                vp._selected_verts.clear(); vp._selected_edges.clear(); vp._selected_faces.clear()
            vp.update()
        dock = getattr(self, '_detached_dock', None)
        if dock is not None and dock.isVisible():
            dock._refresh()

    def _mesh_push_undo(self, idx, geom, label): #vers 1
        """Save a geometry copy (None = undo removes appended geometry idx)."""
        import copy
        if not hasattr(self, '_mesh_undo'):
            self._mesh_undo = []
        self._mesh_undo.append((idx, copy.deepcopy(geom) if geom is not None else None, label))
        del self._mesh_undo[:-50]

    def _mesh_run(self, label, op, *args, need=None, **kwargs): #vers 3
        """Undo point, run a col_mesh_ops op on the geometry, refresh views."""
        import copy
        from apps.methods.dff_mesh_ops import apply_mesh_op
        tgt = self._mesh_target()
        if tgt is None:
            return None
        idx, geom = tgt
        faces, verts = self._mesh_sel()
        if need == 'faces' and not faces:
            self._set_status(f"{label}: select faces first (Face mode)")
            return None
        if need == 'verts' and not verts:
            self._set_status(f"{label}: select vertices first (Vertex mode)")
            return None
        counts = (len(geom.vertices), len(geom.triangles))
        self._mesh_push_undo(idx, geom, label)
        try:
            result = apply_mesh_op(geom, op, *args, **kwargs)
        except Exception as e:
            idx_, old, _l = self._mesh_undo.pop()
            self._current_dff_model.geometries[idx_] = old
            QMessageBox.warning(self, label, f"{label} failed:\n{e}")
            return None
        self._mesh_after_edit(idx, geom, f"{label}: {len(geom.vertices)} verts, {len(geom.triangles)} faces",
                              keep_sel=counts == (len(geom.vertices), len(geom.triangles)))
        return result

    def _mesh_after_edit(self, idx, geom, msg, keep_sel=False): #vers 2
        """Refresh viewport, tables and dirty state after a mesh change."""
        self._mesh_dirty = True
        self._mesh_show(idx, geom, keep_sel=keep_sel)
        model = self._current_dff_model
        if hasattr(self, '_populate_dff_detail_table'):
            self._populate_dff_detail_table(model)
        tbl = getattr(self, 'mod_compact_list', None)
        if tbl is not None and idx < tbl.rowCount():
            it = tbl.item(idx, 1)
            if it is not None:
                it.setText(f"{geom.vertex_count}v  {geom.triangle_count}t  {geom.material_count}m")
        if hasattr(self, 'undo_btn'):
            self.undo_btn.setEnabled(True)
        self._set_status(msg)

    def _mesh_undo_last(self): #vers 2
        """Restore the geometry saved before the last mesh edit, or drop an added one."""
        idx, old, label = self._mesh_undo.pop()
        model = self._current_dff_model
        if model and old is None and idx == len(model.geometries) - 1:
            model.geometries.pop()
            for a in [a for a in model.atomics if a.geometry_index == idx]:
                model.atomics.remove(a)
                if a.frame_index == len(model.frames) - 1:
                    model.frames.pop()
            self._display_dff_model(model)
            self._set_status(f"Undo: {label}")
        elif model and old is not None and idx < len(model.geometries):
            model.geometries[idx] = old
            self._mesh_after_edit(idx, old, f"Undo: {label}")

    def _mesh_gizmo_moved(self, vert_ids, dx, dy, dz): #vers 2
        """Viewport gizmo drag finished: move vertices, or faces (connected / detach / extrude)."""
        from apps.methods import col_mesh_ops as ops
        vp = self.preview_widget
        faces, _v = self._mesh_sel()
        mode = getattr(vp, '_mesh_face_mode', 'connected')
        if faces and mode != 'connected':
            if self._mesh_run(f"Move faces ({mode})", ops.move_faces, faces, mode, dx, dy, dz) is not None \
                    and mode == 'detach':
                self._mesh_record_detached(faces)
            for v in self._live_viewports():
                v._selected_faces.clear(); v._selected_faces.update(faces); v.update()
            return
        self._mesh_run("Move (gizmo)", ops.translate, set(), dx, dy, dz, set(vert_ids))

    def _mesh_gizmo_xform(self, kind, vert_ids, axis, value, pivot): #vers 1
        """Viewport gizmo rotate (degrees) or scale (factor) finished; undoable."""
        from apps.methods import col_mesh_ops as ops
        ids = set(vert_ids)
        if kind == 'rotate':
            self._mesh_run(f"Rotate {axis.upper()} {value:+.1f} (gizmo)", ops.rotate, set(), axis.upper(),
                           value, pivot, ids)
        else:
            f = [value if axis in ('u', a) else 1.0 for a in 'xyz']
            self._mesh_run(f"Scale {'all' if axis == 'u' else axis.upper()} x{value:.2f} (gizmo)",
                           ops.scale, set(), *f, pivot, ids)

    def _dff_append_geometry(self, geom, src_idx, name, label): #vers 1
        """Append geom with its own frame (copy of src_idx's) and atomic; undoable."""
        import copy
        from apps.methods.dff_classes import Atomic, Frame
        model = self._current_dff_model
        src = next((a for a in model.atomics if a.geometry_index == src_idx), None)
        frame = copy.deepcopy(model.frames[src.frame_index]) if src and src.frame_index < len(model.frames) \
            else Frame(parent_index=0 if model.frames else -1)
        frame.name = name[:23]
        geom.added = True
        model.geometries.append(geom)
        model.frames.append(frame)
        model.atomics.append(Atomic(len(model.frames) - 1, len(model.geometries) - 1,
                                    src.flags if src else 5))
        self._mesh_push_undo(len(model.geometries) - 1, None, label)
        self._mesh_dirty = True
        self._display_dff_model(model)
        if hasattr(self, 'undo_btn'):
            self.undo_btn.setEnabled(True)
        self._set_status(f"{label}: '{frame.name}' ({len(geom.vertices)} verts, {len(geom.triangles)} faces)")

    def _mesh_frame_name(self, idx): #vers 1
        """Frame name of a geometry (via its atomic)."""
        model = self._current_dff_model
        a = next((a for a in model.atomics if a.geometry_index == idx), None)
        return model.frames[a.frame_index].name if a and a.frame_index < len(model.frames) else f"geom{idx}"

    def _mesh_copy_or_move(self, title): #vers 1
        """'copy', 'move' or None."""
        from PyQt6.QtWidgets import QInputDialog
        how, ok = QInputDialog.getItem(self, title, "Selected faces:", ["Copy", "Move"], 0, False)
        return how.lower() if ok else None

    def _mesh_selection_to_geometry(self): #vers 1
        """Selected faces to a new geometry + frame + atomic (copy or move)."""
        from PyQt6.QtWidgets import QInputDialog
        from apps.methods import col_mesh_ops as ops
        from apps.methods.dff_mesh_ops import extract_geometry
        tgt = self._mesh_target()
        faces, _v = self._mesh_sel()
        if tgt is None or not faces:
            self._set_status("Selection to geometry: select faces first (Face mode)"); return
        idx, geom = tgt
        how = self._mesh_copy_or_move("Selection to New Geometry")
        if not how: return
        name, ok = QInputDialog.getText(self, "New Geometry", "Frame name:",
                                        text=f"{self._mesh_frame_name(idx)[:18]}_part")
        if not ok or not name.strip(): return
        new = extract_geometry(geom, faces)
        if how == 'move':
            self._mesh_run("Move faces to new geometry", ops.delete_faces, faces, need='faces')
        self._dff_append_geometry(new, idx, name.strip(), f"{how.title()} {len(faces)} face(s) to geometry")

    def _mesh_selection_to_file(self): #vers 1
        """Selected faces saved as a new one-geometry DFF (copy or move)."""
        from apps.methods import col_mesh_ops as ops
        from apps.methods.dff_mesh_ops import extract_geometry
        tgt = self._mesh_target()
        faces, _v = self._mesh_sel()
        if tgt is None or not faces:
            self._set_status("Save selection: select faces first (Face mode)"); return
        idx, geom = tgt
        how = self._mesh_copy_or_move("Save Selection as DFF")
        if not how: return
        start = os.path.dirname(getattr(self, '_current_dff_path', None) or '')
        path, _ = QFileDialog.getSaveFileName(self, "Save Selection as DFF",
                                              os.path.join(start, f"{self._mesh_frame_name(idx)}_part.dff"),
                                              "DFF Files (*.dff)")
        if not path: return
        self._write_geometry_dff(path, extract_geometry(geom, faces))
        if how == 'move':
            self._mesh_run(f"Move faces to {os.path.basename(path)}", ops.delete_faces, faces, need='faces')
        else:
            self._set_status(f"Copied {len(faces)} face(s) to {os.path.basename(path)}")

    def _write_geometry_dff(self, path, geom): #vers 1
        """Write one geometry as a standalone DFF (root frame named after the file)."""
        from apps.methods.dff_classes import Atomic, DFFModel, Frame
        from apps.methods.dff_patch import build_dff
        stamp = getattr(self._current_dff_model, 'rw_version', 0) or 0x1803FFFF
        name = os.path.splitext(os.path.basename(path))[0][:23]
        solo = DFFModel(frames=[Frame(name=name)], geometries=[geom], atomics=[Atomic(0, 0, 5)])
        with open(path, 'wb') as fh:
            fh.write(build_dff(solo, stamp))

    def _mesh_copy_as_lod(self): #vers 1
        """LOD copy of the geometry (optionally decimated): new geometry or LOD DFF file."""
        import copy
        from PyQt6.QtWidgets import QDialogButtonBox, QFormLayout, QSpinBox
        from apps.methods import col_mesh_ops as ops
        from apps.methods.dff_mesh_ops import apply_mesh_op
        tgt = self._mesh_target()
        if tgt is None: return
        idx, geom = tgt
        dlg = QDialog(self)
        dlg.setWindowTitle("Copy as LOD")
        form = QFormLayout(dlg)
        keep = QSpinBox(); keep.setRange(5, 100); keep.setValue(50); keep.setSuffix(" %")
        form.addRow("Keep faces:", keep)
        dest = QComboBox(); dest.addItems(["New geometry (_l1 frame)", "LOD DFF file"])
        form.addRow("Copy to:", dest)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted: return
        new = copy.deepcopy(geom)
        if keep.value() < 100:
            target = max(4, int(len(new.triangles) * keep.value() / 100))
            apply_mesh_op(new, ops.clean_mesh)          # weld seams so edges can collapse
            apply_mesh_op(new, ops.decimate, min(1.0, target / max(1, len(new.triangles))))
        name = self._mesh_frame_name(idx)
        if dest.currentIndex() == 0:
            base = name[:-3] if name.lower()[-3:] in ('_l0', '_hi') else name
            self._dff_append_geometry(new, idx, f"{base}_l1", "Copy as LOD")
            return
        stem = os.path.splitext(os.path.basename(getattr(self, '_current_dff_path', None) or name))[0]
        start = os.path.dirname(getattr(self, '_current_dff_path', None) or '')
        path, _ = QFileDialog.getSaveFileName(self, "Save LOD DFF",
                                              os.path.join(start, f"LOD{stem[3:]}.dff"), "DFF Files (*.dff)")
        if path:
            self._write_geometry_dff(path, new)
            self._set_status(f"LOD saved: {os.path.basename(path)} ({len(new.triangles)} faces)")

    def _mesh_optimum_bounds(self): #vers 1
        """Tight bounding sphere for the geometry (undoable)."""
        from apps.methods.dff_mesh_ops import optimum_sphere
        tgt = self._mesh_target()
        if tgt is None: return
        idx, geom = tgt
        self._mesh_push_undo(idx, geom, "Optimum bounds")
        optimum_sphere(geom)
        self._mesh_after_edit(idx, geom, f"Bounding sphere radius {geom.bounding_sphere.radius:.3f}", keep_sel=True)

    def _mesh_lighting(self): #vers 1
        """Generate prelit vertex colours from a directional light."""
        from PyQt6.QtWidgets import QCheckBox, QDialogButtonBox, QFormLayout, QSpinBox
        from apps.methods.dff_mesh_ops import prelight_geometry
        tgt = self._mesh_target()
        if tgt is None: return
        idx, geom = tgt
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Generate Lighting - {self._mesh_frame_name(idx)}")
        form = QFormLayout(dlg)
        fields = {}
        for key, label, lo, hi, val in [("intensity", "Light intensity %:", 0, 100, 100),
                                        ("azimuth", "Azimuth:", 0, 359, 45),
                                        ("altitude", "Altitude:", -90, 90, 45),
                                        ("ambient", "Ambient light %:", 0, 100, 30)]:
            sp = QSpinBox(); sp.setRange(lo, hi); sp.setValue(val)
            form.addRow(label, sp); fields[key] = sp
        direc = QCheckBox("Use directional light source"); direc.setChecked(True)
        form.addRow(direc)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted: return
        v = {k: sp.value() for k, sp in fields.items()}
        self._mesh_push_undo(idx, geom, "Generate lighting")
        prelight_geometry(geom, v['intensity'] / 100, v['azimuth'], v['altitude'],
                          v['ambient'] / 100, direc.isChecked())
        self._mesh_after_edit(idx, geom, f"Prelight generated for {len(geom.vertices)} vertices", keep_sel=True)
        if hasattr(self, '_mesh_light_act'):
            self._mesh_light_act.setChecked(True)

    def _mesh_light_view(self, checked): #vers 1
        """Show prelit vertex colours in the viewports."""
        for vp in self._live_viewports():
            vp.set_prelight(bool(checked))

    def _mesh_region_circle(self, checked): #vers 1
        """Region select shape: circle when on, rectangle when off."""
        for vp in self._live_viewports():
            vp._region_circle = bool(checked)
        self._set_status("Region select: circle" if checked else "Region select: rectangle")

    def _mesh_region_window(self, checked): #vers 1
        """Face region select: window (all corners inside) when on, crossing when off."""
        for vp in self._live_viewports():
            vp._region_crossing = not checked
        self._set_status("Face region: window" if checked else "Face region: crossing")

    def _edit_model(self): #vers 1
        """Start mesh editing on the selected geometry (Face mode)."""
        if not getattr(self, '_current_dff_model', None):
            QMessageBox.information(self, "Edit Model", "Open a model (DFF / MDL / WDR / NIF) first.")
            return
        self._set_select_mode('face')
        idx = max(0, self._dff_get_selected_geom_idx())
        self._set_status(f"Editing geometry [{idx}]: click faces (Ctrl adds, drag a box), drag the gizmo, "
                         "right-click for mesh tools. Object Select finishes.")

    def _mesh_hide_selected(self): #vers 1
        """Hide selected faces in the live viewports (not drawn or picked)."""
        faces, _v = self._mesh_sel()
        if not faces:
            self._set_status("Hide: select faces first (Face mode)"); return
        for vp in self._live_viewports():
            vp._hidden_faces |= faces
            vp._selected_faces.clear()
            vp.update()
        self._set_status(f"{len(self.preview_widget._hidden_faces)} face(s) hidden")

    def _mesh_unhide_all(self): #vers 1
        """Show all hidden faces."""
        for vp in self._live_viewports():
            vp._hidden_faces = set()
            vp.update()
        self._set_status("All faces shown")

    def _mesh_toggle_lock(self, checked): #vers 1
        """Selection lock: clicks keep the current selection (Space)."""
        for vp in self._live_viewports():
            vp._selection_locked = bool(checked)
        self._set_status("Selection locked" if checked else "Selection unlocked")

    def _mesh_toggle_gamepad(self, on): #vers 1
        """Start or stop the game controller; remembered in model_workshop.json."""
        from apps.methods.gamepad_input import toggle_workshop_gamepad
        toggle_workshop_gamepad(self, on, 'model_workshop.json', getattr(self, '_mesh_gamepad_act', None))

    def _mesh_delete_faces(self): #vers 1
        from apps.methods import col_mesh_ops as ops
        faces, _v = self._mesh_sel()
        self._mesh_run("Delete faces", ops.delete_faces, faces, need='faces')

    def _mesh_detach(self): #vers 2
        from apps.methods import col_mesh_ops as ops
        faces, _v = self._mesh_sel()
        if self._mesh_run("Detach", ops.detach_faces, faces, need='faces') is not None:
            self._mesh_record_detached(faces)

    def _mesh_record_detached(self, faces): #vers 1
        """Remember detached faces on the geometry (orange edges, Detached Parts list)."""
        tgt = self._mesh_target()
        if tgt is None or not faces:
            return
        idx, geom = tgt
        groups = [g for g in self._mesh_detached_groups(geom) if not g & set(faces)]
        geom.detached_groups = groups + [(frozenset(faces), len(geom.triangles))]
        self._mesh_show(idx, geom, keep_sel=True)

    def _mesh_detached_groups(self, geom): #vers 1
        """Valid detached face groups (dropped once faces were deleted)."""
        n = len(geom.triangles)
        return [g for g, count in getattr(geom, 'detached_groups', []) if n >= count and max(g) < n]

    def _mesh_detached_dialog(self): #vers 2
        """List detached parts: click selects them, Reattach joins them back."""
        from apps.gui.col_workshop_dialogs import show_detached_parts_dock
        from apps.methods import col_mesh_ops as ops

        def parts():
            tgt = self._mesh_target()
            return [(f"Part {i + 1}", sorted(g)) for i, g in
                    enumerate(self._mesh_detached_groups(tgt[1]))] if tgt else []

        def select(faces):
            self._set_select_mode('face')
            for vp in self._live_viewports():
                vp._selected_faces.clear(); vp._selected_faces.update(faces); vp.update()

        def reattach(faces):
            tgt = self._mesh_target()
            if tgt is None:
                return
            n = self._mesh_run("Reattach", ops.reattach_faces, set(faces))
            idx, geom = self._mesh_target()
            geom.detached_groups = [(g, c) for g, c in getattr(geom, 'detached_groups', [])
                                    if set(g) != set(faces)]
            self._mesh_show(idx, geom, keep_sel=True)
            self._set_status(f"Reattached {n or 0} vertex(es)")
        show_detached_parts_dock(self, self._outer_mw, parts, select, reattach,
                                 self._get_icon_color())

    def _mesh_split(self): #vers 1
        from apps.methods import col_mesh_ops as ops
        faces, _v = self._mesh_sel()
        self._mesh_run("Split faces", ops.split_faces, faces, need='faces')

    def _mesh_fill_hole(self): #vers 1
        from apps.methods import col_mesh_ops as ops
        faces, _v = self._mesh_sel()
        self._mesh_run("Fill hole", ops.fill_holes, faces, need='faces')

    def _mesh_gizmo_settings(self): #vers 1
        """Shared gizmo style (size, lines, arrows, letters) for every workshop."""
        from apps.methods.gizmo_paint import edit_gizmo_style
        if edit_gizmo_style(self):
            for vp in self._live_viewports():
                vp.update()

    def _mesh_set_face_mode(self, mode): #vers 1
        """Face move mode shared by every viewport chip and the ribbon toggles."""
        self._mesh_face_mode = mode
        for vp in self._live_viewports():
            vp.set_mesh_face_mode(mode, notify=False)
        for m in ('connected', 'detach', 'extrude'):
            act = getattr(self, f'_mesh_fm_{m}_act', None)
            if act is not None and act.isChecked() != (m == mode):
                act.blockSignals(True); act.setChecked(m == mode); act.blockSignals(False)
        self._set_status(f"Face move: {mode}")

    def _mesh_uv_dialog(self): #vers 1
        """UV map selected faces (or all): projection, fit 0-1, scale, rotate, offset."""
        from PyQt6.QtWidgets import (QCheckBox, QComboBox, QDialog, QDialogButtonBox,
                                     QDoubleSpinBox, QFormLayout, QSpinBox)
        from apps.methods.dff_mesh_ops import uv_map
        tgt = self._mesh_target()
        if tgt is None:
            return
        geom = tgt[1]
        if not geom.uv_layers:
            self._set_status("UV Map: geometry has no UV layer")
            return
        faces, _v = self._mesh_sel()
        dlg = QDialog(self)
        dlg.setWindowTitle(f"UV Map - {len(faces)} face(s)" if faces else "UV Map - whole geometry")
        form = QFormLayout(dlg)
        proj = QComboBox()
        for label, key in (("Keep current UVs", 'keep'), ("Box", 'box'), ("Planar X", 'planar_x'),
                           ("Planar Y", 'planar_y'), ("Planar Z", 'planar_z')):
            proj.addItem(label, key)
        form.addRow("Projection:", proj)
        layer = QSpinBox(); layer.setRange(1, len(geom.uv_layers))
        form.addRow("UV layer:", layer)
        fit = QCheckBox("Fit to 0-1 (resize)")
        form.addRow(fit)

        def spin(val, lo, hi, step):
            sp = QDoubleSpinBox(); sp.setRange(lo, hi); sp.setDecimals(4)
            sp.setSingleStep(step); sp.setValue(val)
            return sp
        su, sv = spin(1.0, -1000, 1000, 0.1), spin(1.0, -1000, 1000, 0.1)
        ou, ov = spin(0.0, -1000, 1000, 0.05), spin(0.0, -1000, 1000, 0.05)
        rot = spin(0.0, -360, 360, 15)
        for label, w in (("Scale U:", su), ("Scale V:", sv), ("Offset U:", ou),
                         ("Offset V:", ov), ("Rotate:", rot)):
            form.addRow(label, w)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        n = self._mesh_run("UV Map", uv_map, faces, proj.currentData(), su.value(), sv.value(),
                           ou.value(), ov.value(), rot.value(), fit.isChecked(), layer.value() - 1)
        if n is not None:
            self._set_status(f"UV Map: {n} vertex UV(s) set")

    def _mesh_weld(self): #vers 1
        from apps.methods import col_mesh_ops as ops
        _f, verts = self._mesh_sel()
        if len(verts) < 2:
            self._set_status("Weld: select two or more vertices")
            return
        self._mesh_run("Weld", ops.weld_vertices, verts, need='verts')

    def _mesh_delete_vertices(self): #vers 1
        from apps.methods import col_mesh_ops as ops
        _f, verts = self._mesh_sel()
        self._mesh_run("Delete vertices", ops.delete_vertices, verts, need='verts')

    def _mesh_add_face(self): #vers 1
        from apps.methods import col_mesh_ops as ops
        _f, verts = self._mesh_sel()
        if len(verts) != 3:
            self._set_status("Add face: select exactly three vertices")
            return
        a, b, c = sorted(verts)
        self._mesh_run("Add face", ops.add_face, a, b, c, need='verts')

    def _mesh_move_dialog(self): #vers 2
        """Position of the selection (or whole geometry): absolute X/Y/Z of its centre, or move by."""
        from PyQt6.QtWidgets import QCheckBox, QDialog, QDialogButtonBox, QDoubleSpinBox, QFormLayout
        from apps.methods import col_mesh_ops as ops
        tgt = self._mesh_target()
        if tgt is None:
            return
        faces, verts = self._mesh_sel()
        cx, cy, cz = ops.selection_centre(self._mesh_preview(tgt[1]), faces or None, verts or None)
        dlg = QDialog(self)
        n = len(verts) or len(faces)
        dlg.setWindowTitle(f"Position of {n} {'vertex(es)' if verts else 'face(s)'}" if n else "Position of geometry")
        form = QFormLayout(dlg)
        spins = []
        for axis, val in zip("XYZ", (cx, cy, cz)):
            sp = QDoubleSpinBox()
            sp.setRange(-100000.0, 100000.0); sp.setDecimals(4); sp.setSingleStep(0.1); sp.setValue(val)
            form.addRow(f"{axis}:", sp)
            spins.append(sp)
        rel = QCheckBox("Relative (move by)")
        rel.toggled.connect(lambda on: [sp.setValue(0.0 if on else v) for sp, v in zip(spins, (cx, cy, cz))])
        form.addRow(rel)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept); bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        x, y, z = (sp.value() for sp in spins)
        dx, dy, dz = (x, y, z) if rel.isChecked() else (x - cx, y - cy, z - cz)
        if dx or dy or dz:
            self._mesh_run("Move", ops.translate, faces, dx, dy, dz, verts or None)

    def _mesh_show_vertices(self, checked): #vers 1
        """Vertex dots on the model in every edit mode."""
        for vp in self._live_viewports():
            vp._show_verts = bool(checked)
            vp.update()

    def _mesh_scale_dialog(self): #vers 1
        """Scale selection (or whole geometry) about its centre."""
        from PyQt6.QtWidgets import QInputDialog
        from apps.methods import col_mesh_ops as ops
        f, ok = QInputDialog.getDouble(self, "Scale", "Factor:", 1.0, 0.001, 1000.0, 3)
        if not ok or f == 1.0:
            return
        faces, verts = self._mesh_sel()
        tgt = self._mesh_target()
        if tgt is None:
            return
        pivot = ops.selection_centre(self._mesh_preview(tgt[1]), faces, verts)
        self._mesh_run(f"Scale x{f:g}", ops.scale, faces, f, f, f, pivot, verts or None)

    def _mesh_rotate_dialog(self): #vers 1
        """Rotate selection (or whole geometry) about its centre."""
        from PyQt6.QtWidgets import QInputDialog
        from apps.methods import col_mesh_ops as ops
        axis, ok = QInputDialog.getItem(self, "Rotate", "Axis:", ["X", "Y", "Z"], 2, False)
        if not ok:
            return
        deg, ok = QInputDialog.getDouble(self, "Rotate", "Degrees:", 90.0, -360.0, 360.0, 2)
        if not ok or deg == 0.0:
            return
        faces, verts = self._mesh_sel()
        tgt = self._mesh_target()
        if tgt is None:
            return
        pivot = ops.selection_centre(self._mesh_preview(tgt[1]), faces, verts)
        self._mesh_run(f"Rotate {axis} {deg:g}", ops.rotate, faces, axis, deg, pivot, verts or None)

    def _mesh_centre_origin(self): #vers 1
        """Move the geometry so its centre sits on the origin."""
        from apps.methods import col_mesh_ops as ops
        tgt = self._mesh_target()
        if tgt is None:
            return
        cx, cy, cz = ops.selection_centre(self._mesh_preview(tgt[1]))
        self._mesh_run("Centre origin", ops.translate, set(), -cx, -cy, -cz)

    def _mesh_select_verts(self, how): #vers 1
        """Vertex selection: all, none, invert, or from selected faces."""
        vp = getattr(self, 'preview_widget', None)
        tgt = self._mesh_target()
        if vp is None or tgt is None:
            return
        n = len(tgt[1].vertices)
        if how == 'from_faces':
            sel = {i for fi in vp._selected_faces if fi < len(tgt[1].triangles)
                   for i in (tgt[1].triangles[fi].v1, tgt[1].triangles[fi].v2, tgt[1].triangles[fi].v3)}
            self._set_select_mode('vertex')
            vp._selected_verts |= sel
        elif how == 'all':
            vp._selected_verts = set(range(n))
        elif how == 'none':
            vp._selected_verts.clear()
        else:
            vp._selected_verts = set(range(n)) - set(vp._selected_verts)
        vp._notify_selection_changed(); vp.update()

    def _mesh_verts_to_faces(self): #vers 1
        """Select faces whose three vertices are all selected."""
        from apps.methods import col_mesh_ops as ops
        vp = getattr(self, 'preview_widget', None)
        tgt = self._mesh_target()
        if vp is None or tgt is None:
            return
        faces = ops.faces_of_vertices(self._mesh_preview(tgt[1]), vp._selected_verts)
        self._set_select_mode('face')
        vp._selected_faces |= faces
        vp._notify_selection_changed(); vp.update()

    def _mesh_select_material(self): #vers 1
        """Select every face using the materials of the selected faces."""
        from apps.methods import col_mesh_ops as ops
        vp = getattr(self, 'preview_widget', None)
        tgt = self._mesh_target()
        if vp is None or tgt is None or not vp._selected_faces:
            self._set_status("Select by material: select a face first")
            return
        geom = tgt[1]
        mats = {geom.triangles[i].material_id for i in vp._selected_faces if i < len(geom.triangles)}
        vp._selected_faces |= ops.faces_by_material(self._mesh_preview(geom), mats)
        vp._notify_selection_changed(); vp.update()

    def _mesh_optimise(self, how): #vers 1
        """Clean (seams kept), merge flat areas, decimate, or drop unused vertices."""
        from PyQt6.QtWidgets import QInputDialog
        from apps.methods import col_mesh_ops as ops
        if how == 'clean':
            self._mesh_run("Clean", ops.clean_mesh, keep_seams=True)
        elif how == 'merge':
            self._mesh_run("Merge flat areas", ops.merge_coplanar)
        elif how == 'isolated':
            self._mesh_run("Delete isolated vertices", ops.delete_isolated_vertices)
        else:
            r, ok = QInputDialog.getDouble(self, "Decimate", "Keep this fraction of faces:", 0.5, 0.05, 0.95, 2)
            if ok:
                self._mesh_run(f"Decimate {r:g}", ops.decimate, r)

    def _mesh_menu(self, menu=None): #vers 5
        """Mesh edit menu (ribbon button and viewport right-click)."""
        own = menu is None
        menu = menu or QMenu(self)
        menu.addAction("Gizmo Settings...", self._mesh_gizmo_settings)
        menu.addSeparator()
        sel = menu.addMenu("Select")
        sel.addAction("All Vertices", lambda: self._mesh_select_verts('all'))
        sel.addAction("No Vertices", lambda: self._mesh_select_verts('none'))
        sel.addAction("Invert Vertices", lambda: self._mesh_select_verts('invert'))
        sel.addAction("Vertices of Selected Faces", lambda: self._mesh_select_verts('from_faces'))
        sel.addAction("Faces of Selected Vertices", self._mesh_verts_to_faces)
        sel.addAction("Faces with Same Material", self._mesh_select_material)
        fm = menu.addMenu("Faces")
        fm.addAction("Delete Faces", self._mesh_delete_faces)
        fm.addAction("Detach", self._mesh_detach)
        fm.addAction("Detached Parts...", self._mesh_detached_dialog)
        fm.addAction("Split (centre vertex)", self._mesh_split)
        fm.addAction("Fill Hole", self._mesh_fill_hole)
        vm = menu.addMenu("Vertices")
        vm.addAction("Weld", self._mesh_weld)
        vm.addAction("Delete Vertices", self._mesh_delete_vertices)
        vm.addAction("Add Face (3 vertices)", self._mesh_add_face)
        tm = menu.addMenu("Transform")
        tm.addAction("Position / Move...", self._mesh_move_dialog)
        tm.addAction("Rotate...", self._mesh_rotate_dialog)
        tm.addAction("Scale...", self._mesh_scale_dialog)
        tm.addAction("Mirror...", self._mirror_dialog)
        tm.addAction("Align (flatten)...", self._align_dialog)
        tm.addAction("Centre on Origin", self._mesh_centre_origin)
        om = menu.addMenu("Optimise")
        om.addAction("Clean (keep UV seams)", lambda: self._mesh_optimise('clean'))
        om.addAction("Merge Flat Areas", lambda: self._mesh_optimise('merge'))
        om.addAction("Decimate...", lambda: self._mesh_optimise('decimate'))
        om.addAction("Delete Isolated Vertices", lambda: self._mesh_optimise('isolated'))
        gm = menu.addMenu("Geometry")
        gm.addAction("Selection to New Geometry...", self._mesh_selection_to_geometry)
        gm.addAction("Save Selection as DFF...", self._mesh_selection_to_file)
        gm.addAction("Copy as LOD...", self._mesh_copy_as_lod)
        gm.addAction("Optimum Bounds", self._mesh_optimum_bounds)
        gm.addAction("Generate Lighting...", self._mesh_lighting)
        menu.addSeparator()
        menu.addAction("Undo Mesh Edit", self._undo_last_action).setEnabled(bool(getattr(self, '_mesh_undo', None)))
        if own:
            from PyQt6.QtGui import QCursor
            menu.exec(QCursor.pos())
        return menu
