#this belongs in apps/components/Map_Editor/depends/map_model_func.py - Version: 5
# X-Seti - October 14 2026 - IMG Factory 1.6 - Map Workshop models

"""
Model, COL, DFF, texture and IDE handling (Model Workshop base) for ModelWorkshop (Map).
"""

##class MapModelMixin: -
# _add_textures_from_txd
# _align_dialog
# _analyze_collision
# _apply_to_selected_faces_paint
# _auto_load_from_texlist
# _auto_load_txd_from_imgs
# _browse_texlist_folder
# _build_txd_from_textures
# _compute_face_shade
# _convert_surface
# _copy_model_info
# _copy_model_to_clipboard
# _copy_surface
# _copy_text_to_clipboard
# _create_new_surface
# _create_texture_panel
# _cycle_view_render_style
# _delete_selected_model
# _delete_surface
# _dff_analyze
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
# _dff_to_col_surfaces
# _dff_transform_vertices
# _display_dff_model
# _draw_col_model
# _duplicate_selected_model
# _duplicate_surface
# _enable_dff_toolbar
# _enable_name_edit
# _exit_paint_mode
# export_all
# export_all_surfaces
# _export_col_data
# _export_col_model
# _export_dff_obj
# _export_model_menu
# _export_not_implemented
# export_selected
# export_selected_surface
# _export_textures_as_png
# _export_via_ide
# _extract_col_from_img
# _filter_model_list
# _find_all_paint_btns
# _find_in_ide
# _force_save_col
# _generate_collision_thumbnail
# _get_ide_db
# _get_selected_model
# _get_xref
# _hide_tex_hover
# _import_col_data
# _import_model
# _import_obj
# _import_replace_col_model
# _import_surface
# _import_via_ide
# _invert_selection
# _is_model_pinned
# load_from_img_archive
# _load_iff_as_qimage
# _load_img_col_list
# _load_texlist_setting
# _load_txd_file
# _load_txd_file_from_data
# _load_txd_into_workshop
# _lookup_ide_for_dff
# _lookup_ide_from_db
# _mirror_dialog
# _on_col_selected
# _on_collision_selected
# _on_compact_col_selected
# _on_dff_geom_selected
# _on_dff_geom_selected_tbl
# _on_frame_tree_clicked
# _on_paint_mode_exited
# _on_painted_face
# _on_snap_vertices_clicked
# _on_tex_selected
# _open_col_file
# open_col_file
# _open_col_from_img_entry
# open_dff_file
# _open_dff_material_list
# _open_dff_standalone
# _open_file
# _open_gl_viewer
# open_img_archive
# _open_linked_txd
# _open_mipmap_manager
# _open_paint_editor
# _open_paint_mat_popup
# _open_txd_combined
# _open_txd_smart
# _paint_cycle_mat
# _parse_txd_lightweight
# _pass_textures_to_dp5_workshop
# _pass_textures_to_txd_workshop
# _paste_model_from_clipboard
# _paste_surface
# _pick_col_from_current_img
# _populate_collision_list
# _populate_compact_col_list
# _populate_dff_detail_table
# _populate_frame_tree
# _populate_tex_thumbnails
# _populate_texture_list
# _project_model_2d
# _push_undo
# refresh
# _regenerate_all_thumbnails
# reload_surface_table
# _reload_surface_table
# _remove_selected_textures
# _remove_via_ide
# _rename_col_model
# _render_collision_preview
# _save_as_col_file
# save_col_file
# _save_col_file
# _save_file
# _save_file_as
# _save_settings
# _save_texlist_setting
# _save_textures_as_txd
# _saveall_file
# _select_all_models
# _select_model_by_row
# _set_col_buttons_enabled
# _set_paint_tool
# _set_select_mode
# _set_status
# _set_thumbnail_view
# _show_collision_context_menu
# _show_dff_geometry
# _show_dff_material_context_menu
# _show_model_details
# _show_model_search
# _show_shadow_mesh
# _show_sort_menu
# _show_tex_hover
# _sort_models
# _sort_models_desc
# _start_thumbnail_spin
# _stop_thumbnail_spin
# _tbl_item
# _tex_context_menu
# _texture_back_from_dp5
# _tick_thumbnail_spin
# _toggle_backface_cull
# _toggle_col_view
# _toggle_front_only_paint
# _toggle_pin_selected
# _toggle_tex_view
# _undo_last_action
# _update_select_mode_availability
# _wire_col_buttons
# _wire_dff_buttons

import os

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QCursor, QFont
from PyQt6.QtWidgets import (
    QAbstractItemView, QFileDialog, QFrame, QHBoxLayout, QLabel, QLineEdit, QListWidgetItem,
    QMenu, QMessageBox, QPushButton, QTableWidget, QTableWidgetItem, QVBoxLayout, QWidget)

from apps.components.Map_Editor.depends.map_widgets_func import (
    App_name, _DFFGeometryAdapter, _model_workshop_config_dir)


class MapModelMixin: #vers 1
    """Model, COL, DFF, texture and IDE handling (Model Workshop base)."""


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



    def _cycle_view_render_style(self): #vers 3
        """Cycle viewport render: wireframe -> solid -> textured.
        Applied to every visible viewport (single or all 4 quad panes)."""
        pw = getattr(self, 'preview_widget', None)
        if not pw: return
        modes = ['wireframe', 'solid', 'textured']
        cur = getattr(pw, '_mode', 'solid')
        nxt = modes[(modes.index(cur) + 1) % len(modes)] if cur in modes else 'solid'
        for vp in self._live_viewports():
            if hasattr(vp, 'set_render_mode'):
                vp.set_render_mode(nxt)
            else:
                vp._mode = nxt
                vp.update()


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

    def _get_selected_model(self): #vers 4
        """Return the currently selected COLModel or None.
        Uses currentRow() on the active (visible) list widget — more reliable
        than selectedRows() which can fail with custom delegates."""
        if not getattr(self, "current_col_file", None):
            return None
        models = getattr(self.current_col_file, 'models', [])
        if not models:
            return None

        # Only check the VISIBLE list — the hidden one may have stale state
        if getattr(self, '_col_view_mode', 'detail') == 'detail':
            lw = getattr(self, 'mod_compact_list', None)
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

    def _set_status(self, msg: str): #vers 2
        """Write msg to the status label (whichever one exists)."""
        if hasattr(self, 'status_label'):
            self.status_label.setText(msg)
        elif hasattr(self, 'status_bar') and hasattr(self.status_bar, 'showMessage'):
            self.status_bar.showMessage(msg, 3000)
        else:
            print(f"[Map Workshop] {msg}")

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


    def _set_thumbnail_view(self, yaw, pitch, label="Custom"): #vers 1
        """Change the view angle for all thumbnails and regenerate them."""
        self._thumb_yaw   = float(yaw)
        self._thumb_pitch = float(pitch)
        self._stop_thumbnail_spin()
        self._regenerate_all_thumbnails()
        if hasattr(self, 'main_window') and self.main_window:
            self.main_window.log_message(f"Thumbnail view: {label}")


    def _regenerate_all_thumbnails(self): #vers 1
        """Redraw every thumbnail in both lists at current _thumb_yaw/pitch."""
        if not getattr(self, "current_col_file", None):
            return
        models = getattr(self.current_col_file, 'models', [])
        # Compact list
        for row in range(self.mod_compact_list.rowCount()):
            item = self.mod_compact_list.item(row, 0)
            if item and row < len(models):
                thumb = self._generate_collision_thumbnail(
                    models[row], 64, 64,
                    yaw=self._thumb_yaw, pitch=self._thumb_pitch)
                item.setData(Qt.ItemDataRole.DecorationRole, thumb)
                item.setData(Qt.ItemDataRole.UserRole + 1, True)
        # Detail list
        for row in range(self.collision_list.rowCount()):
            item = self.collision_list.item(row, 0)
            if item and row < len(models):
                thumb = self._generate_collision_thumbnail(
                    models[row], 64, 64,
                    yaw=self._thumb_yaw, pitch=self._thumb_pitch)
                item.setData(Qt.ItemDataRole.DecorationRole, thumb)
                item.setData(Qt.ItemDataRole.UserRole + 1, True)


    def _start_thumbnail_spin(self, row, model): #vers 1
        """Start slowly rotating the thumbnail of the selected row."""
        self._stop_thumbnail_spin()
        self._spin_row   = row
        self._spin_model = model
        self._spin_yaw   = 0.0
        # Random slow axis: yaw + slight pitch drift
        import random
        self._spin_dyaw   = random.uniform(0.8, 1.4)
        self._spin_dpitch = random.uniform(-0.3, 0.3)
        self._spin_pitch  = random.uniform(-20.0, 20.0)
        from PyQt6.QtCore import QTimer
        self._spin_timer = QTimer(self)
        self._spin_timer.setInterval(50)   # 20 fps
        self._spin_timer.timeout.connect(self._tick_thumbnail_spin)
        self._spin_timer.start()


    def _stop_thumbnail_spin(self): #vers 1
        """Stop any running thumbnail rotation."""
        t = getattr(self, '_spin_timer', None)
        if t:
            t.stop()
            t.deleteLater()
            self._spin_timer = None
        self._spin_row   = None
        self._spin_model = None


    def _tick_thumbnail_spin(self): #vers 1
        """Advance the spin angle and update the thumbnail."""
        model = getattr(self, '_spin_model', None)
        row   = getattr(self, '_spin_row',   None)
        if model is None or row is None:
            self._stop_thumbnail_spin()
            return
        # Advance angles
        self._spin_yaw   = (self._spin_yaw + self._spin_dyaw) % 360
        self._spin_pitch = max(-35.0, min(35.0,
            self._spin_pitch + self._spin_dpitch))
        # Flip pitch direction at limits
        if abs(self._spin_pitch) >= 35.0:
            self._spin_dpitch *= -1

        # Only spin if model has geometry
        has_geo = (getattr(model, 'vertices', []) or
                   getattr(model, 'spheres',  []) or
                   getattr(model, 'boxes',    []))
        if not has_geo:
            self._stop_thumbnail_spin()
            return

        # Render thumbnail at current angle
        thumb = self._generate_collision_thumbnail(
            model, 64, 64,
            yaw=self._spin_yaw, pitch=self._spin_pitch)

        # [T] view no longer has thumbnails — spin does nothing visible there
        # The viewport itself rotates via _yaw/_pitch so just stop the timer
        self._stop_thumbnail_spin()


    def _enable_name_edit(self, event, is_alpha): #vers 1
        """Enable name editing on click"""
        self.info_name.setReadOnly(False)
        self.info_name.selectAll()
        self.info_name.setFocus()




    def _enable_dff_toolbar(self, dff_mode: bool): #vers 3
        """Switch left toolbar between DFF mode and COL mode.
        DFF mode: enables paint, material, render-select, prelighting, flip, rotate, analyze, copy, delete, duplicate.
        COL mode: enables COL-specific buttons, disables DFF ones."""
        # Buttons only relevant for COL — hide entirely in DFF mode
        col_only = [
            'create_surface_btn',
        ]
        # Buttons that work in BOTH modes (flip/rotate/analyze/copy/paste/delete/duplicate)
        shared_btns = [
            'flip_vert_btn', 'flip_horz_btn', 'rotate_cw_btn', 'rotate_ccw_btn',
            'analyze_btn', 'copy_btn', 'paste_btn', 'delete_surface_btn',
            'duplicate_surface_btn',
        ]
        # Buttons shared but context-switches
        dff_btns = ['paint_btn', 'surface_type_btn']

        for attr in col_only:
            b = getattr(self, attr, None)
            if b:
                b.setEnabled(not dff_mode)
                b.setVisible(not dff_mode)

        for attr in shared_btns:
            b = getattr(self, attr, None)
            if b:
                b.setEnabled(True)
                b.setVisible(True)

        for attr in dff_btns:
            b = getattr(self, attr, None)
            if b: b.setEnabled(dff_mode)

        # Rewire shared buttons for correct mode
        if dff_mode:
            self._wire_dff_buttons()
        else:
            self._wire_col_buttons()

        # DFF-only toolbar buttons (backface, front-paint, primitive, etc.)
        # V/E/F/P select buttons are gated separately by
        # _update_select_mode_availability (need an actual model, not just
        # DFF-vs-COL mode) so they're excluded from this blanket pass.
        _select_btn_attrs = {'_sel_vert_btn', '_sel_edge_btn',
                              '_sel_face_btn', '_sel_poly_btn'}
        for btn in getattr(self, '_dff_only_toolbar_btns', []):
            is_select_btn = any(btn is getattr(self, a, None) for a in _select_btn_attrs)
            btn.setVisible(dff_mode)
            if not is_select_btn:
                btn.setEnabled(dff_mode)
        self._update_select_mode_availability()

        # Prelighting row — shown in DFF mode
        for attr in ('prelight_apply_btn', 'prelight_setup_btn'):
            b = getattr(self, attr, None)
            if b:
                b.setEnabled(dff_mode)

        # Bottom info_format label
        lbl = getattr(self, 'info_format', None)
        if lbl:
            lbl.setText("Prelight: " if dff_mode else "Format: ")

        # Update tooltips for DFF context
        if dff_mode:
            if hasattr(self, 'paint_btn'):
                self.paint_btn.setToolTip(
                    "Open Material List — assign textures to DFF geometry")
            if hasattr(self, 'surface_type_btn'):
                self.surface_type_btn.setToolTip(
                    "Material Editor — view materials, textures, IDE info, swap TXD")
            if hasattr(self, 'flip_vert_btn'):
                self.flip_vert_btn.setToolTip("Flip DFF geometry on Y axis")
            if hasattr(self, 'flip_horz_btn'):
                self.flip_horz_btn.setToolTip("Flip DFF geometry on X axis")
            if hasattr(self, 'rotate_cw_btn'):
                self.rotate_cw_btn.setToolTip("Rotate DFF geometry 90° clockwise (Z axis)")
            if hasattr(self, 'rotate_ccw_btn'):
                self.rotate_ccw_btn.setToolTip("Rotate DFF geometry 90° counter-clockwise (Z axis)")
            if hasattr(self, 'analyze_btn'):
                self.analyze_btn.setToolTip("Analyze DFF — verts, tris, materials, frames")
            if hasattr(self, 'copy_btn'):
                self.copy_btn.setToolTip("Copy selected geometry to clipboard")
            if hasattr(self, 'paste_btn'):
                self.paste_btn.setToolTip("Paste geometry from clipboard")
            if hasattr(self, 'delete_surface_btn'):
                self.delete_surface_btn.setToolTip("Delete selected geometry from DFF")
            if hasattr(self, 'duplicate_surface_btn'):
                self.duplicate_surface_btn.setToolTip("Duplicate selected geometry")

    # - Render / selection mode

    def _wire_col_buttons(self): #vers 1
        """Reconnect shared toolbar buttons to COL-mode handlers."""
        pairs = [
            ('flip_vert_btn',       lambda: self.preview_widget and self.preview_widget.flip_vertical()),
            ('flip_horz_btn',       lambda: self.preview_widget and self.preview_widget.flip_horizontal()),
            ('rotate_cw_btn',       lambda: self.preview_widget and self.preview_widget.rotate_cw()),
            ('rotate_ccw_btn',      lambda: self.preview_widget and self.preview_widget.rotate_ccw()),
            ('analyze_btn',         self._analyze_collision),
            ('copy_btn',            self._copy_surface),
            ('paste_btn',           self._paste_surface),
            ('delete_surface_btn',  self._delete_surface),
            ('duplicate_surface_btn', self._duplicate_surface),
        ]
        for attr, fn in pairs:
            b = getattr(self, attr, None)
            if b:
                try: b.clicked.disconnect()
                except Exception: pass
                b.clicked.connect(fn)

    def _wire_dff_buttons(self): #vers 1
        """Connect shared toolbar buttons to DFF-mode handlers."""
        pairs = [
            ('flip_vert_btn',       self._dff_flip_y),
            ('flip_horz_btn',       self._dff_flip_x),
            ('rotate_cw_btn',       self._dff_rotate_cw),
            ('rotate_ccw_btn',      self._dff_rotate_ccw),
            ('analyze_btn',         self._dff_analyze),
            ('copy_btn',            self._dff_copy_geometry),
            ('paste_btn',           self._dff_paste_geometry),
            ('delete_surface_btn',  self._dff_delete_geometry),
            ('duplicate_surface_btn', self._dff_duplicate_geometry),
        ]
        for attr, fn in pairs:
            b = getattr(self, attr, None)
            if b:
                try: b.clicked.disconnect()
                except Exception: pass
                b.clicked.connect(fn)

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

    def _dff_duplicate_geometry(self): #vers 1
        """Duplicate selected geometry in DFF."""
        geoms = self._dff_get_geometries()
        if not geoms:
            return
        import copy
        model = getattr(self, '_current_dff_model', None)
        idx = self._dff_get_selected_geom_idx()
        idx = min(idx, len(geoms)-1)
        dup = copy.deepcopy(geoms[idx])
        geoms.insert(idx+1, dup)
        model.geometries = geoms
        self._populate_dff_detail_table(model)
        vp = getattr(self, 'preview_widget', None)
        if vp:
            vp._model = model
            vp.update()
        self._set_status(f"Geometry [{idx}] duplicated.")

    def _on_snap_vertices_clicked(self): #vers 1
        """Snap Vertices button handler (Aug 20 2026,  "the
        biggest problem sometimes with making models is sometimes
        there are gaps, so we need a snap function") - delegates to
        the viewport's own real _snap_vertices, then reports the real
        result."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, '_snap_vertices'):
            return
        moved = vp._snap_vertices()
        if moved:
            self._set_status(f"Snapped {moved} vertex/vertices to close gaps")
        else:
            self._set_status("Snap Vertices: nothing close enough to snap")

    def _mirror_dialog(self): #vers 1
        """STUB: Mirror selected geometry across a chosen axis."""
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(self, "Mirror",
            "Mirror is not yet implemented.\n"
            "Coming in a future build.")

    def _align_dialog(self): #vers 2
        """Align menu: selected objects to last selected, distribute, drop."""
        n = len(self._selected_instances())
        menu = QMenu(self)
        for ax in 'xyz':
            menu.addAction(f"Align {ax.upper()} to last selected",
                           lambda a=ax: self._align_selected(a)).setEnabled(n > 1)
        menu.addAction("Distribute evenly", self._distribute_selected).setEnabled(n > 2)
        menu.addAction(f"Drop to Ground ({n})", self._drop_selected_to_ground).setEnabled(n > 0)
        if not n:
            menu.addSeparator()
            menu.addAction("Select objects in the world view first").setEnabled(False)
        menu.exec(QCursor.pos())

    def _set_select_mode(self, mode: str): #vers 3
        """Set viewport selection mode: vertex / edge / face / poly / object.
        Applied to every visible viewport (single or all 4 quad panes).
        Switching modes clears the other modes' selection sets, so there is
        no stale hidden selection left over when the user switches back."""
        for vp in self._live_viewports():
            vp._select_mode = mode
            if mode != 'vertex':
                vp._selected_verts.clear()
            if mode != 'edge':
                vp._selected_edges.clear()
            if mode not in ('face', 'poly'):
                vp._selected_faces.clear()
            vp.update()
        self._set_status(f"Select mode: {mode}")
        mw = self.main_window
        if mw and hasattr(mw, 'log_message'):
            mw.log_message(f"Model Workshop: select mode → {mode}")

    def _update_select_mode_availability(self): #vers 2
        """Enable vertex/edge/face/poly select buttons only when there is
        an actual model (DFF geometry or COL surfaces) to select from.
        Called after load, after creating a primitive from scratch, and
        whenever the active model is cleared."""
        has_dff = bool(getattr(self, '_current_dff_model', None) and
                       getattr(self._current_dff_model, 'geometries', None))
        has_col = bool(getattr(self, 'current_col_file', None) and
                       getattr(self.current_col_file, 'models', None))
        has_model = has_dff or has_col
        for attr in ('_sel_vert_btn', '_sel_edge_btn',
                     '_sel_face_btn', '_sel_poly_btn'):
            b = getattr(self, attr, None)
            if b:
                b.setEnabled(has_model)
        if not has_model:
            for vp in self._live_viewports():
                vp._select_mode = 'object'
                vp._selected_verts.clear()
                vp._selected_edges.clear()
                vp._selected_faces.clear()
                vp.update()
            grp = getattr(self, '_select_mode_group', None)
            if grp:
                grp.setExclusive(False)
                for attr in ('_sel_vert_btn', '_sel_edge_btn',
                             '_sel_face_btn', '_sel_poly_btn'):
                    b = getattr(self, attr, None)
                    if b: b.setChecked(False)
                grp.setExclusive(True)

    def _toggle_backface_cull(self): #vers 2
        """Toggle backface culling — when ON only the front face is visible/selectable."""
        vp = getattr(self, 'preview_widget', None)
        if not vp:
            return
        cull_on = not getattr(vp, '_backface_cull', True)
        vp.set_backface_cull(cull_on)
        state = "on (front only)" if cull_on else "off (front+back)"
        self._set_status(f"Backface cull: {state}")

    def _toggle_front_only_paint(self): #vers 1
        """Toggle front-face-only paint — prevents painting geometry behind the current view."""
        vp = getattr(self, 'preview_widget', None)
        if not vp:
            return
        current = getattr(vp, '_front_only_paint', False)
        vp._front_only_paint = not current
        btn = getattr(self, '_front_paint_btn', None)
        if btn:
            btn.setChecked(not current)
        self._set_status(
            f"Front-only paint: {'ON — only visible faces painted' if not current else 'OFF'}")


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
                self.main_window.log_message(f"Found {len(self.txd_list)} COL files")
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

    def _import_obj(self, path: str): #vers 3
        """Import Wavefront OBJ as a new DFF geometry."""
        from PyQt6.QtWidgets import QMessageBox
        verts, uvs, normals, faces = [], [], [], []
        try:
            with open(path, 'r', errors='replace') as fh:
                for line in fh:
                    t = line.split()
                    if not t: continue
                    if t[0] == 'v'  and len(t) >= 4:
                        verts.append((float(t[1]), float(t[2]), float(t[3])))
                    elif t[0] == 'vt' and len(t) >= 3:
                        uvs.append((float(t[1]), float(t[2])))
                    elif t[0] == 'vn' and len(t) >= 4:
                        normals.append((float(t[1]), float(t[2]), float(t[3])))
                    elif t[0] == 'f'  and len(t) >= 4:
                        def _idx(s): return int(s.split('/')[0]) - 1  #vers 1
                        faces.append((_idx(t[1]), _idx(t[2]), _idx(t[3])))
        except Exception as e:
            QMessageBox.critical(self, "OBJ Import Error", str(e))
            return
        if not verts or not faces:
            QMessageBox.warning(self, "OBJ Import", "No geometry found in OBJ file.")
            return
        # Build a minimal DFF geometry and add to current model or create new
        fname = os.path.basename(path)
        self._set_status(f"OBJ imported: {fname} ({len(verts)} verts, {len(faces)} faces)")
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(
                f"Imported OBJ: {fname} — {len(verts)} vertices, {len(faces)} faces")
        QMessageBox.information(self, "OBJ Import",
            f"Imported: {fname}\n{len(verts)} vertices, {len(faces)} faces\n"
            f"Note: OBJ geometry loaded as preview. DFF write-back in next session.")

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

    def _open_txd_combined(self, _checked=False): #vers 1
        """Open TXD — smart loader for DFF+TXD workflow.
        Shift+click always browses. Otherwise tries:
          1. asset_db lookup (if DB built)
          2. Auto-find from open IMGs using IDE txd_name
          3. Browse for file
        """
        from PyQt6.QtWidgets import QApplication
        shift_held = bool(QApplication.keyboardModifiers() &
                          __import__('PyQt6.QtCore', fromlist=['Qt']).Qt.KeyboardModifier.ShiftModifier)

        if not shift_held:
            # Try smart auto-load first
            txd_name = (getattr(self, '_ide_txd_name', '') or '').strip()
            if not txd_name:
                # Try from current DFF materials
                model = getattr(self, '_current_dff_model', None)
                if model and hasattr(model, 'geometries') and model.geometries:
                    geom = model.geometries[0]
                    mats = getattr(geom, 'materials', [])
                    if mats:
                        txd_name = getattr(mats[0], 'texture_name', '') or ''
                        if txd_name:
                            # strip to stem
                            import os
                            txd_name = os.path.splitext(txd_name)[0]

            if txd_name:
                # Try DB first
                mw = self.main_window
                db = getattr(mw, 'asset_db', None) if mw else None
                ok = False
                if db:
                    try:
                        row = db.find_img_entry(txd_name + '.txd')
                        if row:
                            from apps.methods.img_core_classes import IMGFile
                            import os
                            arc = IMGFile(row['source_path']); arc.open()
                            entry = next((e for e in arc.entries
                                if e.name.lower() == row['entry_name'].lower()), None)
                            if entry:
                                data = arc.read_entry_data(entry)
                                if data:
                                    self._load_txd_file_from_data(data, txd_name + '.txd')
                                    if mw and hasattr(mw, 'log_message'):
                                        mw.log_message(
                                            f"TXD loaded via DB: {txd_name}.txd "
                                            f"from {os.path.basename(row['source_path'])}")
                                    ok = True
                    except Exception:
                        pass
                if not ok:
                    ok = self._auto_load_txd_from_imgs(txd_name)
                if ok:
                    return
                # Auto-find failed — fall through to browse

        # Browse for TXD file
        self._load_txd_into_workshop()


    def _open_gl_viewer(self): #vers 1
        """Launch the OpenGL model viewer with current DFF/TXD if loaded."""
        try:
            from apps.components.Model_Viewer.model_viewer import open_model_viewer
            mw = getattr(self, 'main_window', None)
            dff_path = getattr(self, '_current_dff_path', None)
            txd_path = getattr(self, '_current_txd_path', None)
            win, viewer = open_model_viewer(mw, dff_path, txd_path)
            # Keep reference so window isn't GC'd
            self._gl_viewer_win = win
        except Exception as e:
            import traceback; traceback.print_exc()
            self._set_status(f"GL Viewer error: {e}")

    def _open_dff_standalone(self): #vers 5
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
                dlg, "Open DFF", start_dir,
                "DFF Models (*.dff);;DFF + TXD (*.dff *.txd);;All Files (*)")
            if p:
                dff_edit.setText(p)
                setattr(self, _settings_key, os.path.dirname(p))
                if auto_txd_cb.isChecked():
                    for ext in ('.txd', '.TXD'):
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


    def _open_file(self): #vers 5
        """Open file dialog — supports DFF (model), COL (collision), and
        TXD (texture dictionary) files."""
        try:
            file_path, _ = QFileDialog.getOpenFileName(
                self,
                "Open Model / Collision / Texture / Map File",
                "",
                "All Supported Files (*.dff *.col *.txd *.dat);;"
                "Model/Collision/Texture Files (*.dff *.col *.txd);;"
                "DFF Models (*.dff);;COL Files (*.col);;TXD Files (*.txd);;"
                "GTA Map (*.dat gta3.dat gta_vc.dat gta.dat gta_sol.dat gtasol.dat gta_quick.dat);;"
                "All Files (*)"
            )
            if not file_path:
                return
            ext = os.path.splitext(file_path)[1].lower()
            if ext == '.dff':
                self.open_dff_file(file_path)
                # Auto-load matching TXD into this workshop's own scene -
                # IDE-linked name (via _current_ide_obj, just populated by
                # open_dff_file's internal _lookup_ide_for_dff call), same
                # folder, or open IMGs. _open_txd_smart() would be wrong
                # here, it opens the separate TXD Workshop tool instead of
                # loading textures into the current scene.
                self._auto_load_txd_from_imgs()
            elif ext == '.txd':
                mw = getattr(self, 'main_window', None)
                if mw and hasattr(mw, 'open_txd_workshop_docked'):
                    mw.open_txd_workshop_docked(file_path=file_path)
                else:
                    from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
                    open_txd_workshop(self, file_path)
            elif ext == '.dat':
                self._load_game_dat_file(file_path)
            else:
                self.open_col_file(file_path)
        except Exception as e:
            print(f"Error in open file dialog: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to open file:\n{str(e)}")

    # - Texture management

    def _create_texture_panel(self): #vers 6
        """Collapsible texture panel in middle column.
        Shows textures loaded from TXD files.
        Toggle between List view (table) and Thumbnail view (64x64 grid).
        Click a thumbnail to see it at up to 128x128 in a popup."""
        icon_color = self._get_icon_color()

        self._tex_panel = QFrame()
        self._tex_panel.setFrameStyle(QFrame.Shape.StyledPanel)
        self._tex_panel.setVisible(False)   # hidden until TXD loaded
        lay = QVBoxLayout(self._tex_panel)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(3)

        # - Header row (count + view toggle only - the dock's own title
        # bar already shows "Textures", a second label here was duplicate)
        self._tex_count_lbl = QLabel("0 textures")       # shown in the dock title bar
        self._tex_count_lbl.setFont(self.panel_font)

        # View toggle: list ⇔ thumbnails
        self._tex_view_mode = 'list'
        self._tex_view_btn = QPushButton("⊞ Thumbnails")    # shown in the dock title bar
        self._tex_view_btn.setFixedHeight(20)
        self._tex_view_btn.setFont(self.panel_font)
        self._tex_view_btn.setToolTip(
            "Toggle between list view and 64×64 thumbnail grid")
        self._tex_view_btn.clicked.connect(self._toggle_tex_view)

        # - Button row
        btn_row = QHBoxLayout()
        btn_row.setSpacing(3)

        # Collapse/expand toggle - hides Load/Texlist/WShop/Save to
        # reclaim space, independent of the icon-only/icon+text compacting.
        self._tex_btn_row_collapsed = False
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        self.tex_btn_collapse_btn = QPushButton()
        self.tex_btn_collapse_btn.setIcon(SVGIconFactory.arrow_up_icon(16, icon_color))
        self.tex_btn_collapse_btn.setFixedSize(26, 26)
        self.tex_btn_collapse_btn.setToolTip("Collapse/expand this toolbar")
        self.tex_btn_collapse_btn.clicked.connect(self._toggle_tex_btn_row_collapsed)
        btn_row.addWidget(self.tex_btn_collapse_btn)

        # tex buttons: adaptive — show label when panel wide enough, icon-only when narrow
        self._tex_btns_meta = []   # [(attr, label, icon_method)] for compact update

        def _tbtn(attr, text, icon_method, tip, slot, enabled=True):  #vers 2
            b = QPushButton(text)
            b.setFont(self.button_font)
            try:
                b.setIcon(getattr(self.icon_factory, icon_method)(color=icon_color))
                b.setIconSize(QSize(18, 18))
            except Exception:
                pass
            b.setFixedHeight(26)
            b.setMinimumWidth(28)   # icon-only minimum
            b.setToolTip(tip)
            b.setEnabled(enabled)
            b.clicked.connect(slot)
            b._tex_label = text
            setattr(self, attr, b)
            btn_row.addWidget(b)
            self._tex_btns_meta.append((attr, text, icon_method))
            return b

        # Adaptive: _update_tex_btn_compact() reduces to icon-only when panel < 260px
        _tbtn('tex_load_btn',    'Load',   'open_icon',
              'Load a TXD file into this workshop',
              self._load_txd_into_workshop)
        _tbtn('tex_browse_btn',  'Texlist',    'folder_icon',  # folder_icon is appropriate
              'Browse texlist/ folder and import individual textures',
              self._browse_texlist_folder)
        _tbtn('tex_pass_btn',    'WShop',  'export_icon',  # export_icon used; txd_workshop_icon if added later
              'Send all textures to TXD Workshop for editing/rebuilding',
              self._pass_textures_to_txd_workshop, enabled=False)
        _tbtn('tex_dp5_btn',     'DP5',    'dp5_workshop_icon',
              'Send the selected textures (or all) to DP5 Workshop to paint',
              self._pass_textures_to_dp5_workshop, enabled=False)
        _tbtn('tex_save_btn',    'Save',   'save_icon',
              'Save current textures as a new TXD file',
              self._save_textures_as_txd, enabled=False)
        btn_row.addStretch()
        lay.addLayout(btn_row)

        # - Stacked: List view (table) + Thumbnail view (scroll area)
        from PyQt6.QtWidgets import QStackedWidget, QScrollArea, QGridLayout
        self._tex_stack = QStackedWidget()
        lay.addWidget(self._tex_stack, stretch=1)

        # Page 0: Table list
        self._tex_list = QTableWidget()
        self._tex_list.setColumnCount(4)
        self._tex_list.setHorizontalHeaderLabels(["", "Name", "Size", "Format"])
        self._tex_list.horizontalHeader().setStretchLastSection(True)
        self._tex_list.horizontalHeader().setFixedHeight(26)
        self._tex_list.setColumnWidth(0, 56)  # thumbnail
        self._tex_list.setColumnWidth(2, 72)  # size
        self._tex_list.setColumnWidth(3, 56)  # format
        self._tex_list.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self._tex_list.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)
        self._tex_list.setAlternatingRowColors(True)
        self._tex_list.setIconSize(QSize(32, 32))
        self._tex_list.verticalHeader().setDefaultSectionSize(52)
        self._tex_list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._tex_list.customContextMenuRequested.connect(self._tex_context_menu)
        self._tex_list.itemSelectionChanged.connect(self._on_tex_selected)
        self._tex_stack.addWidget(self._tex_list)   # index 0

        # Page 1: Thumbnail grid
        self._tex_scroll = QScrollArea()
        self._tex_scroll.setWidgetResizable(True)
        self._tex_scroll.setFrameShape(QFrame.Shape.NoFrame)
        self._tex_grid_widget = QWidget()
        self._tex_grid_layout = QGridLayout(self._tex_grid_widget)
        self._tex_grid_layout.setContentsMargins(4, 4, 4, 4)
        self._tex_grid_layout.setSpacing(6)
        self._tex_scroll.setWidget(self._tex_grid_widget)
        self._tex_stack.addWidget(self._tex_scroll)   # index 1

        # Internal storage
        self._mod_textures = []
        self._texlist_path = ''

        # Hook the panel's own resize event directly - self.resizeEvent()
        # only fires when the whole ModelWorkshop widget's outer bounding
        # box changes, but dragging the Models/viewport dock divider only
        # changes this panel's own width, so relying solely on
        # self.resizeEvent() left the compact check stuck at whatever it
        # computed on first layout and never re-evaluated afterwards.
        _orig_tex_resize = self._tex_panel.resizeEvent
        def _tex_panel_resize(event, _orig=_orig_tex_resize):  #vers 1
            _orig(event)
            self._update_tex_btn_compact()
        self._tex_panel.resizeEvent = _tex_panel_resize

        return self._tex_panel


    def _toggle_tex_view(self): #vers 1
        """Switch texture panel between list view and 64×64 thumbnail grid."""
        if getattr(self, '_tex_view_mode', 'list') == 'list':
            self._tex_view_mode = 'thumb'
            try:
                from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVGT
                self._tex_view_btn.setIcon(_SVGT._list_icon(color=self._get_icon_color()))
                self._tex_view_btn.setIconSize(QSize(16,16))
            except Exception:
                pass
            self._tex_view_btn.setText("List")
            self._tex_view_btn.setToolTip("Switch to list view")
            stack = getattr(self, '_tex_stack', None)
            if stack:
                stack.setCurrentIndex(1)
            self._populate_tex_thumbnails()
        else:
            self._tex_view_mode = 'list'
            try:
                from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVGT
                self._tex_view_btn.setIcon(_SVGT.fit_grid_icon(color=self._get_icon_color()))
                self._tex_view_btn.setIconSize(QSize(16,16))
            except Exception:
                pass
            self._tex_view_btn.setText("Thumbnails")
            self._tex_view_btn.setToolTip("Switch to 64×64 thumbnail grid")
            stack = getattr(self, '_tex_stack', None)
            if stack:
                stack.setCurrentIndex(0)

    def _populate_tex_thumbnails(self): #vers 1
        """Fill the thumbnail grid with 64×64 previews of all loaded textures.
        Each thumbnail is a clickable label — click opens a larger popup."""
        from PyQt6.QtWidgets import QLabel, QVBoxLayout, QWidget
        from PyQt6.QtGui import QImage, QPixmap, QColor, QFont
        from PyQt6.QtCore import Qt as _Qt

        grid   = getattr(self, '_tex_grid_layout', None)
        if grid is None:
            return

        # Clear previous thumbnails
        while grid.count():
            item = grid.takeAt(0)
            if item and item.widget():
                item.widget().deleteLater()

        textures = getattr(self, '_mod_textures', [])
        vp       = getattr(self, 'preview_widget', None)
        tc       = getattr(vp, '_tex_cache', {}) if vp else {}

        THUMB_SIZE = 64
        # Fit as many columns as the scroll area width allows (min 2)
        scroll_w = getattr(self, '_tex_scroll', None)
        avail_w  = scroll_w.viewport().width() if scroll_w else 220
        COL_COUNT = max(2, avail_w // (THUMB_SIZE + 10))

        for i, tex in enumerate(textures):
            name     = tex.get('name', f'tex_{i}')
            rgba     = tex.get('rgba_data', b'')
            w        = tex.get('width',  0)
            h        = tex.get('height', 0)
            fmt      = tex.get('format', '?')
            in_cache = name.lower() in tc

            # Build 64×64 pixmap
            pix = QPixmap(THUMB_SIZE, THUMB_SIZE)
            pix.fill(QColor(40, 40, 40))
            if rgba and w > 0 and h > 0 and len(rgba) >= w * h * 4:
                try:
                    qimg = QImage(rgba[:w*h*4], w, h,
                                  w * 4, QImage.Format.Format_RGBA8888)
                    pix = QPixmap.fromImage(qimg).scaled(
                        THUMB_SIZE, THUMB_SIZE,
                        _Qt.AspectRatioMode.KeepAspectRatio,
                        _Qt.TransformationMode.SmoothTransformation)
                    # Pad to square
                    if pix.width() != THUMB_SIZE or pix.height() != THUMB_SIZE:
                        padded = QPixmap(THUMB_SIZE, THUMB_SIZE)
                        padded.fill(self._get_ui_color('viewport_bg'))
                        from PyQt6.QtGui import QPainter
                        p = QPainter(padded)
                        x = (THUMB_SIZE - pix.width())  // 2
                        y = (THUMB_SIZE - pix.height()) // 2
                        p.drawPixmap(x, y, pix)
                        p.end()
                        pix = padded
                except Exception:
                    pass

            # Container widget for thumb + name label
            cell = QWidget()
            cell.setFixedSize(THUMB_SIZE + 4, THUMB_SIZE + 18)
            cell_lay = QVBoxLayout(cell)
            cell_lay.setContentsMargins(2, 2, 2, 2)
            cell_lay.setSpacing(1)

            # Clickable image label
            img_lbl = QLabel()
            img_lbl.setFixedSize(THUMB_SIZE, THUMB_SIZE)
            img_lbl.setPixmap(pix)
            img_lbl.setAlignment(_Qt.AlignmentFlag.AlignCenter)
            img_lbl.setCursor(
                __import__('PyQt6.QtCore', fromlist=['Qt']).Qt.CursorShape.PointingHandCursor)
            # Cache indicator border
            border_col = 'palette(link)' if in_cache else 'palette(mid)'
            img_lbl.setStyleSheet(
                f"border: 2px solid {border_col}; background: palette(base);")
            img_lbl.setToolTip(
                f"{name}\n{w}×{h}  {fmt}\n"
                f"{'✓ in texture cache' if in_cache else '✗ not in cache'}\n"
                "Click to view full size")

            # Capture tex for lambda
            _tex = tex
            img_lbl._tex_raw  = _tex
            img_lbl._hover_wnd = [None]
            def _enter_g(ev, lbl=img_lbl, ws=self):  #vers 1
                ws._show_tex_hover(lbl, getattr(lbl,'_tex_raw',None))
            def _leave_g(ev, lbl=img_lbl, ws=self):  #vers 1
                ws._hide_tex_hover(lbl)
            img_lbl.enterEvent = _enter_g
            img_lbl.leaveEvent = _leave_g
            cell_lay.addWidget(img_lbl)

            # Name label (truncated)
            name_lbl = QLabel(name[:12] + '…' if len(name) > 12 else name)
            name_lbl.setFont(QFont("Arial", 7))
            name_lbl.setAlignment(_Qt.AlignmentFlag.AlignCenter)
            name_lbl.setStyleSheet("color: palette(windowText);")
            cell_lay.addWidget(name_lbl)

            grid.addWidget(cell, i // COL_COUNT, i % COL_COUNT)

        # Fill remaining cells with stretch
        grid.setColumnStretch(COL_COUNT, 1)


    def _show_tex_hover(self, anchor_widget, tex: dict): #vers 1
        """Show a frameless texture preview that follows the mouse into the popup.
        Disappears when mouse leaves both the thumbnail and the popup itself."""
        if not tex:
            return
        from PyQt6.QtWidgets import QLabel, QVBoxLayout, QWidget
        from PyQt6.QtGui import QImage, QPixmap, QColor, QFont, QCursor
        from PyQt6.QtCore import Qt as _Qt

        # Dismiss any existing hover window
        self._hide_tex_hover(anchor_widget)

        name = tex.get('name', 'texture')
        rgba = tex.get('rgba_data', b'')
        w    = tex.get('width',  0)
        h    = tex.get('height', 0)
        fmt  = tex.get('format', '?')
        MAX_D = 192

        popup = QWidget(None,
            _Qt.WindowType.Tool |
            _Qt.WindowType.FramelessWindowHint |
            _Qt.WindowType.WindowStaysOnTopHint)
        popup.setAttribute(_Qt.WidgetAttribute.WA_DeleteOnClose)
        popup.setStyleSheet(
            "QWidget { background:palette(base); border:1px solid palette(mid); border-radius:3px; }"
            "QLabel  { color:#ccc; background:transparent; }")

        v = QVBoxLayout(popup)
        v.setContentsMargins(6, 6, 6, 6)
        v.setSpacing(4)

        img_lbl = QLabel()
        img_lbl.setAlignment(_Qt.AlignmentFlag.AlignCenter)
        pix = QPixmap(MAX_D, MAX_D)
        pix.fill(QColor(40, 40, 55))
        if rgba and w > 0 and h > 0 and len(rgba) >= w*h*4:
            try:
                qi = QImage(rgba[:w*h*4], w, h,
                            w*4, QImage.Format.Format_RGBA8888)
                if w > MAX_D or h > MAX_D:
                    qi = qi.scaled(MAX_D, MAX_D,
                        _Qt.AspectRatioMode.KeepAspectRatio,
                        _Qt.TransformationMode.SmoothTransformation)
                pix = QPixmap.fromImage(qi)
            except Exception:
                pass
        img_lbl.setPixmap(pix)
        img_lbl.setFixedSize(pix.width(), pix.height())
        v.addWidget(img_lbl)

        info = QLabel(f"{name}   {w}×{h}   {fmt}")
        info.setFont(QFont("Arial", 8))
        v.addWidget(info)

        popup.adjustSize()
        pos = QCursor.pos()
        popup.move(pos.x() + 12, pos.y() + 8)

        # Hide when mouse leaves the popup
        popup.leaveEvent = lambda ev: popup.close()
        # Also close on click
        popup.mousePressEvent = lambda ev: popup.close()

        popup.show()
        anchor_widget._hover_wnd[0] = popup

    def _hide_tex_hover(self, anchor_widget): #vers 1
        """Close the hover popup if open."""
        holder = getattr(anchor_widget, '_hover_wnd', [None])
        w = holder[0] if holder else None
        if w is not None:
            try:
                w.close()
            except Exception:
                pass
            holder[0] = None


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


    def _auto_load_txd_from_imgs(self, txd_stem: str = '') -> bool: #vers 2
        """Search all open IMG tabs and current_img for txd_stem.txd.
        Extracts and loads it into the texture cache. Returns True if found.
        Falls back to the current DFF's own filename (same-folder-same-name
        convention) if no IDE-linked name is available either."""
        mw = self.main_window
        if not txd_stem:
            # Try to get txd_stem from IDE link
            txd_stem = (getattr(self, '_ide_txd_name', '') or '').strip().lower()
            if not txd_stem:
                # No IDE link either - fall back to the DFF's own filename
                # stem (same-folder-same-name convention).
                dff_path = getattr(self, '_current_dff_path', '') or ''
                if dff_path:
                    txd_stem = os.path.splitext(os.path.basename(dff_path))[0].lower()
                if not txd_stem:
                    return False

        txd_stem_lo = txd_stem.lower().split('.')[0]  # strip extension if present
        txd_filename = txd_stem_lo + '.txd'

        # Same-folder-as-DFF direct disk check first (cheap, common case)
        dff_path = getattr(self, '_current_dff_path', '') or ''
        if dff_path:
            dff_dir = os.path.dirname(dff_path)
            try:
                for fname in os.listdir(dff_dir):
                    if fname.lower() == txd_filename:
                        self._load_txd_file(os.path.join(dff_dir, fname))
                        if mw and hasattr(mw, 'log_message'):
                            mw.log_message(f"Auto-loaded TXD: {fname} (same folder as DFF)")
                        return True
            except OSError:
                pass

        # Gather all candidate IMG sources
        img_candidates   = []   # IMGFile objects
        img_path_search  = []   # raw paths to open directly if needed

        # 1. current_img on workshop or main_window
        ci = getattr(self, 'current_img', None) or (
             getattr(mw, 'current_img', None) if mw else None)

        if mw and hasattr(mw, 'log_message'):
            mw.log_message(
                f"[TXD lookup] looking for '{txd_filename}' - "
                f"self.current_img={getattr(self, 'current_img', None)!r}, "
                f"mw.current_img={getattr(mw, 'current_img', None)!r}, "
                f"entries={len(getattr(ci, 'entries', []) or [])}")
        if ci:
            img_candidates.append(ci)

        # 2. Source IMG path stored when DFF was extracted
        src_img = getattr(self, '_source_img_path', '') or ''
        if src_img and os.path.isfile(src_img):
            img_path_search.append(src_img)

        # 3. All open IMG tabs
        if mw and hasattr(mw, 'main_tab_widget'):
            tw = mw.main_tab_widget
            for i in range(tw.count()):
                w = tw.widget(i)
                if w and getattr(w, 'file_type', '') == 'IMG':
                    fo = getattr(w, 'file_object', None)
                    if fo and fo is not ci:
                        img_candidates.append(fo)
                # Also check file_path of any tab
                fp = getattr(w, 'file_path', '') if w else ''
                if fp and fp.lower().endswith('.img') and fp not in img_path_search:
                    img_path_search.append(fp)

        # 4. Search by opening the source IMG directly (for extracted DFFs)
        seen_paths = {getattr(img, 'file_path', '') for img in img_candidates}
        for ipath in img_path_search:
            if ipath not in seen_paths:
                try:
                    from apps.methods.img_core_classes import IMGFile
                    arc = IMGFile(ipath)
                    arc.open()
                    img_candidates.append(arc)
                    seen_paths.add(ipath)
                except Exception:
                    pass

        for img in img_candidates:
            entries = getattr(img, 'entries', [])
            entry = next(
                (e for e in entries if e.name.lower() == txd_filename),
                None)
            if entry:
                try:
                    data = img.read_entry_data(entry)
                    if data:
                        self._load_txd_file_from_data(data, txd_filename)
                        if mw and hasattr(mw, 'log_message'):
                            mw.log_message(
                                f"Auto-loaded TXD: {txd_filename} "
                                f"from {os.path.basename(img.file_path)}")
                        return True
                except Exception as e:
                    if mw and hasattr(mw, 'log_message'):
                        mw.log_message(f"TXD extract error: {e}")

        if mw and hasattr(mw, 'log_message'):
            mw.log_message(
                f"TXD not in any open IMG: {txd_filename} — trying texlist/")
        # Fallback: scan texlist/ folder for pre-exported image files
        return self._auto_load_from_texlist(txd_stem_lo)


    def _auto_load_from_texlist(self, txd_stem: str = '') -> bool: #vers 1
        """Scan the configured texlist/ folder recursively for image files
        matching the textures needed by the current DFF model.
        Supports PNG, IFF/ILBM, TGA, BMP.  Returns True if at least one
        texture was loaded into the viewport cache."""
        txd_stem_lo = (txd_stem or '').lower().split('.')[0]
        texlist_root = getattr(self, '_texlist_folder', '') or ''

        # Auto-discover texlist/ relative to the current DFF or TXD file
        if not texlist_root or not os.path.isdir(texlist_root):
            for attr in ('_current_dff_path', '_current_txd_path'):
                base = getattr(self, attr, '') or ''
                if not base:
                    continue
                base_dir = os.path.dirname(base)
                for rel in ('texlist',
                            os.path.join('..', 'texlist'),
                            os.path.join('..', '..', 'texlist')):
                    cand = os.path.normpath(os.path.join(base_dir, rel))
                    if os.path.isdir(cand):
                        texlist_root = cand
                        break
                if texlist_root:
                    break

        if not texlist_root or not os.path.isdir(texlist_root):
            mw = self.main_window
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(
                    "texlist: no folder set — use 'Set texlist…' to configure")
            return False

        mw  = self.main_window
        pw  = getattr(self, 'preview_widget', None)
        extensions = ('.png', '.iff', '.tga', '.bmp', '.jpg', '.jpeg')

        # Collect texture names we are looking for from the current model
        needed = set()
        for tex in getattr(self, '_mod_textures', []):
            n = (tex.get('name') or '').lower().strip('\x00').strip()
            if n:
                needed.add(n)

        # When _mod_textures is empty accept anything under the txd_stem subfolder
        use_stem_only = not needed
        if use_stem_only and not txd_stem_lo:
            return False

        found_count = 0

        for dirpath, _dirs, filenames in os.walk(texlist_root):
            # For named-structure dumps, prefer the txd_stem subfolder
            rel_dir = os.path.relpath(dirpath, texlist_root).lower()
            in_stem_folder = (
                rel_dir == '.' or
                rel_dir == txd_stem_lo or
                txd_stem_lo in rel_dir.split(os.sep))

            for fname in filenames:
                ext = os.path.splitext(fname)[1].lower()
                if ext not in extensions:
                    continue
                tex_name = os.path.splitext(fname)[0].lower()

                if use_stem_only:
                    if not in_stem_folder:
                        continue
                else:
                    if tex_name not in needed:
                        continue

                fpath = os.path.join(dirpath, fname)
                try:
                    if ext == '.iff':
                        qimg = self._load_iff_as_qimage(fpath)
                    else:
                        from PyQt6.QtGui import QImage
                        qimg = QImage(fpath)

                    if qimg is None or qimg.isNull():
                        continue

                    from PyQt6.QtGui import QImage as _QI
                    if qimg.format() != _QI.Format.Format_RGBA8888:
                        qimg = qimg.convertToFormat(_QI.Format.Format_RGBA8888)

                    if pw and hasattr(pw, '_tex_cache'):
                        pw._tex_cache[tex_name] = qimg.copy()
                    found_count += 1

                    if mw and hasattr(mw, 'log_message'):
                        mw.log_message(f"texlist: {fname} -> {tex_name}")

                except Exception as e:
                    if mw and hasattr(mw, 'log_message'):
                        mw.log_message(f"texlist load error {fname}: {e}")

        if found_count:
            if pw:
                pw.set_render_mode('textured')
                pw.update()
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(
                    f"texlist: {found_count} texture(s) loaded "
                    f"from {texlist_root}")
        else:
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(
                    f"texlist: no matching textures in {texlist_root}")

        return found_count > 0

    def _load_iff_as_qimage(self, path: str): #vers 1
        """Load an Amiga IFF ILBM file and return a QImage (RGBA8888), or None.
        Supports 24-bit true colour and 8-bit CMAP-indexed ILBM files."""
        import struct
        from PyQt6.QtGui import QImage
        try:
            data = open(path, 'rb').read()
            if len(data) < 12 or data[:4] != b'FORM' or data[8:12] != b'ILBM':
                return None
            i = 12
            bmhd = body = cmap = None
            while i < len(data) - 8:
                tag  = data[i:i+4]
                size = struct.unpack_from('>I', data, i+4)[0]
                chunk = data[i+8:i+8+size]
                if   tag == b'BMHD': bmhd = struct.unpack_from('>HHhhBBBBHBBhh', chunk)
                elif tag == b'CMAP': cmap = chunk
                elif tag == b'BODY': body = chunk
                i += 8 + size + (size % 2)
                if i >= len(data):
                    break

            if not bmhd or body is None:
                return None

            w, h, n_planes = bmhd[0], bmhd[1], bmhd[4]
            row_bytes = (w + 15) // 16 * 2

            # 24-bit true colour (R8 G8 B8 interleaved bitplanes)
            if n_planes == 24:
                rgba = bytearray(w * h * 4)
                bp = 0
                for y in range(h):
                    planes = []
                    for _ in range(24):
                        planes.append(body[bp:bp+row_bytes])
                        bp += row_bytes
                    for x in range(w):
                        bx  = x // 8
                        bit = 0x80 >> (x % 8)
                        r = sum(1<<p for p in range(8) if bx<len(planes[p])   and planes[p][bx]   & bit)
                        g = sum(1<<p for p in range(8) if bx<len(planes[8+p]) and planes[8+p][bx] & bit)
                        b = sum(1<<p for p in range(8) if bx<len(planes[16+p])and planes[16+p][bx]& bit)
                        off = (y*w+x)*4
                        rgba[off]=r; rgba[off+1]=g; rgba[off+2]=b; rgba[off+3]=255
                return QImage(bytes(rgba), w, h, w*4,
                              QImage.Format.Format_RGBA8888).copy()

            # 8-bit indexed with CMAP palette
            if n_planes == 8 and cmap:
                pal = [(cmap[j*3], cmap[j*3+1], cmap[j*3+2])
                       for j in range(len(cmap)//3)]
                rgba = bytearray(w * h * 4)
                bp = 0
                for y in range(h):
                    planes = [body[bp+p*row_bytes:bp+p*row_bytes+row_bytes]
                              for p in range(8)]
                    bp += 8 * row_bytes
                    for x in range(w):
                        bx  = x // 8
                        bit = 0x80 >> (x % 8)
                        pv  = sum(1<<p for p in range(8)
                                  if bx<len(planes[p]) and planes[p][bx] & bit)
                        r, g, b = pal[pv] if pv < len(pal) else (0, 0, 0)
                        off = (y*w+x)*4
                        rgba[off]=r; rgba[off+1]=g; rgba[off+2]=b; rgba[off+3]=255
                return QImage(bytes(rgba), w, h, w*4,
                              QImage.Format.Format_RGBA8888).copy()

            return None
        except Exception:
            return None


    def _save_texlist_setting(self): #vers 1
        """Persist the texlist folder path to model_workshop.json (see _model_workshop_config_dir)"""
        import json
        cfg_dir = str(_model_workshop_config_dir())
        os.makedirs(cfg_dir, exist_ok=True)
        try:
            p = os.path.join(cfg_dir, 'model_workshop.json')
            data = {}
            if os.path.isfile(p):
                data = json.load(open(p))
            data['texlist_folder'] = getattr(self, '_texlist_folder', '')
            json.dump(data, open(p, 'w'), indent=2)
        except Exception:
            pass

    def _load_texlist_setting(self): #vers 1
        """Load the texlist folder path from model_workshop.json (see _model_workshop_config_dir)"""
        import json
        p = str(_model_workshop_config_dir() / 'model_workshop.json')
        if os.path.isfile(p):
            try:
                data = json.load(open(p))
                self._texlist_folder = data.get('texlist_folder', '')
                return
            except Exception:
                pass
        self._texlist_folder = ''


    def _load_txd_into_workshop(self): #vers 2
        """Open a TXD file and load its textures into Model Workshop."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Load TXD",
            os.path.dirname(getattr(self, '_current_dff_path', '')),
            "Texture Files (*.txd *.wtd *.nft *.xtx *.chk);;TXD Files (*.txd);;All Files (*)")
        if not path:
            return
        self._load_txd_file(path)

    def _parse_txd_lightweight(self, data: bytes) -> list: #vers 5
        """Parse TXD data via txd_parser (all platforms, TXD Workshop reader)."""
        try:
            from apps.methods.txd_parser import parse_txd
            return parse_txd(data)
        except Exception as e:
            print(f"_parse_txd_lightweight error: {e}")
            return []

    def _load_txd_file(self, path: str): #vers 4
        """Parse a TXD file, populate texture panel, and feed textures into viewport."""
        try:
            with open(path, 'rb') as f:
                data = f.read()
            textures = self._parse_txd_lightweight(data)
            if not textures:
                QMessageBox.warning(self, "TXD",
                    f"No textures found in {os.path.basename(path)}")
                return
            from apps.methods.txd_splice import tag_loaded_texture
            for t in textures:
                t.setdefault('alpha_name', t.get('mask', ''))
                tag_loaded_texture(t)
            self._mod_textures = textures
            self._current_txd_path = path
            self._current_txd_bytes = data
            self._populate_texture_list()
            self._tex_panel.setVisible(True)
            from PyQt6.QtCore import QTimer as _QTTex
            _QTTex.singleShot(0, self._update_tex_btn_compact)   # visibility alone doesn't fire resizeEvent; defer so layout has settled
            txd_stem = os.path.splitext(os.path.basename(path))[0]
            if hasattr(self, 'info_txd_name'):
                self.info_txd_name.setText(txd_stem)
            self._set_status(
                f"Loaded TXD: {os.path.basename(path)} "
                f"— {len(textures)} textures")
            # Push textures into the 3D viewport cache for textured rendering
            pw = getattr(self, 'preview_widget', None)
            if pw and hasattr(pw, '_upload_textures'):
                pw._upload_textures(textures)
                pw.set_render_mode('textured')
                pw.update()
            elif pw and hasattr(pw, 'load_textures'):
                pw.load_textures(textures)
                if len(textures) > 0:
                    pw.set_render_mode('textured')
        except Exception as e:
            import traceback; traceback.print_exc()
            QMessageBox.critical(self, "TXD Error",
                f"Failed to load TXD:\n{e}")

    def _add_textures_from_txd(self, path: str): #vers 2
        """Add textures from a TXD file to existing texture list (no replace)."""
        try:
            with open(path, 'rb') as f:
                data = f.read()
            new_texs = self._parse_txd_lightweight(data)
            if not new_texs:
                return
            # Merge — skip duplicates by name
            existing = {t['name'].lower() for t in self._mod_textures}
            added = 0
            for tex in new_texs:
                if tex['name'].lower() not in existing:
                    self._mod_textures.append(tex)
                    existing.add(tex['name'].lower())
                    added += 1
            self._populate_texture_list()
            self._set_status(f"Added {added} texture(s) from {os.path.basename(path)}")
        except Exception as e:
            print(f"Add textures error: {e}")

    def _populate_texture_list(self): #vers 3
        """Fill the texture QTableWidget from self._mod_textures."""
        from PyQt6.QtGui import QImage, QPixmap, QIcon
        tbl = self._tex_list
        tbl.setRowCount(0)
        tbl.setIconSize(QSize(48, 48))
        tbl.verticalHeader().setDefaultSectionSize(52)

        for i, tex in enumerate(self._mod_textures):
            row = tbl.rowCount()
            tbl.insertRow(row)

            w = tex.get('width',  0)
            h = tex.get('height', 0)
            name = tex.get('name', '?')
            fmt  = tex.get('format', '?')
            mips = tex.get('mip_count', 1)
            rgba = tex.get('rgba_data', b'')

            # Thumbnail
            thumb_item = QTableWidgetItem()
            thumb_item.setTextAlignment(
                Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignVCenter)
            if rgba and w > 0 and h > 0:
                try:
                    qimg = QImage(rgba, w, h, w * 4, QImage.Format.Format_RGBA8888)
                    pix  = QPixmap.fromImage(qimg).scaled(
                        48, 48,
                        Qt.AspectRatioMode.KeepAspectRatio,
                        Qt.TransformationMode.SmoothTransformation)
                    thumb_item.setIcon(QIcon(pix))
                except Exception:
                    thumb_item.setText("?")
            else:
                thumb_item.setText("—")

            # Name + number as tooltip
            name_item = QTableWidgetItem(name)
            name_item.setToolTip(
                f"#{i+1}  {name}\n{w}×{h}  {fmt}  {mips} mip(s)")

            size_item = QTableWidgetItem(f"{w}×{h}")
            fmt_item  = QTableWidgetItem(fmt)

            tbl.setItem(row, 0, thumb_item)
            tbl.setItem(row, 1, name_item)
            tbl.setItem(row, 2, size_item)
            tbl.setItem(row, 3, fmt_item)

        count = len(self._mod_textures)
        if hasattr(self, '_tex_count_lbl'):
            self._tex_count_lbl.setText(f"{count} texture{'s' if count != 1 else ''}")
        has = count > 0
        for btn in ('tex_pass_btn', 'tex_dp5_btn', 'tex_save_btn'):
            b = getattr(self, btn, None)
            if b: b.setEnabled(has)
        # Refresh thumbnail grid if it's currently visible
        if getattr(self, '_tex_view_mode', 'list') == 'thumb':
            self._populate_tex_thumbnails()

    def _browse_texlist_folder(self): #vers 2
        """Browse a Texlist folder — shows all TXDs, lets user add individual textures."""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QTreeWidget, QTreeWidgetItem
        folder = QFileDialog.getExistingDirectory(
            self, "Select Texlist Folder",
            self._texlist_path or os.path.expanduser('~'))
        if not folder:
            return
        self._texlist_path = folder

        # Scan folder for TXD files
        txd_files = []
        for root, dirs, files in os.walk(folder):
            dirs.sort()
            for fn in sorted(files):
                if fn.lower().endswith('.txd'):
                    txd_files.append(os.path.join(root, fn))

        if not txd_files:
            QMessageBox.information(self, "Texlist", "No TXD files found in that folder.")
            return

        # Show browser dialog
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Texlist Browser — {os.path.basename(folder)}")
        dlg.resize(600, 500)
        dlg_lay = QVBoxLayout(dlg)

        info = QLabel(f"{len(txd_files)} TXD files found in {os.path.basename(folder)}")
        info.setFont(self.panel_font)
        dlg_lay.addWidget(info)

        # Search bar
        search_row = QHBoxLayout()
        search_edit = QLineEdit()
        search_edit.setPlaceholderText("Filter by name…")
        search_row.addWidget(search_edit)
        dlg_lay.addLayout(search_row)

        # Tree: TXD file → texture names
        tree = QTreeWidget()
        tree.setHeaderLabels(["TXD / Texture", "Size", "Format"])
        tree.setColumnWidth(0, 280)
        tree.setAlternatingRowColors(True)
        tree.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        dlg_lay.addWidget(tree, stretch=1)

        # Populate tree
        def _populate(filter_text=''):  #vers 1
            tree.clear()
            ft = filter_text.lower()
            for txd_path in txd_files:
                txd_name = os.path.basename(txd_path)
                if ft and ft not in txd_name.lower():
                    continue
                file_item = QTreeWidgetItem([txd_name, '', ''])
                file_item.setData(0, Qt.ItemDataRole.UserRole, ('file', txd_path))
                file_item.setFont(0, QFont("Arial", 9, QFont.Weight.Bold))
                # Lazy-load: parse on expand
                placeholder = QTreeWidgetItem(['Loading…'])
                file_item.addChild(placeholder)
                tree.addTopLevelItem(file_item)

        _populate()
        search_edit.textChanged.connect(_populate)

        def _on_expand(item):  #vers 1
            if item.childCount() == 1 and item.child(0).text(0) == 'Loading…':
                item.removeChild(item.child(0))
                txd_path = (item.data(0, Qt.ItemDataRole.UserRole) or (None, None))[1]
                if not txd_path:
                    return
                try:
                    with open(txd_path, 'rb') as f:
                        data = f.read()
                    texs = self._parse_txd_lightweight(data)
                    for tex in (texs or []):
                        child = QTreeWidgetItem([
                            tex.get('name', '?'),
                            f"{tex.get('width',0)}×{tex.get('height',0)}",
                            tex.get('format', '?'),
                        ])
                        child.setData(0, Qt.ItemDataRole.UserRole, ('tex', txd_path, tex['name']))
                        item.addChild(child)
                except Exception as e:
                    item.addChild(QTreeWidgetItem([f"Error: {e}"]))

        tree.itemExpanded.connect(_on_expand)

        # Buttons
        btn_row = QHBoxLayout()
        add_file_btn = QPushButton("Add whole TXD")
        add_file_btn.setToolTip("Add all textures from selected TXD file(s)")
        add_tex_btn  = QPushButton("Add selected textures")
        add_tex_btn.setToolTip("Add only the selected texture(s)")
        cancel_btn   = QPushButton("Close")
        btn_row.addWidget(add_file_btn)
        btn_row.addWidget(add_tex_btn)
        btn_row.addStretch()
        btn_row.addWidget(cancel_btn)
        dlg_lay.addLayout(btn_row)
        cancel_btn.clicked.connect(dlg.close)

        def _add_whole_txd():  #vers 1
            added_any = False
            for item in tree.selectedItems():
                role = item.data(0, Qt.ItemDataRole.UserRole)
                if role and role[0] == 'file':
                    self._add_textures_from_txd(role[1])
                    added_any = True
            if added_any:
                dlg.close()

        def _add_selected_textures():  #vers 1
            # Group by TXD file, parse once, add matching names
            by_file = {}
            for item in tree.selectedItems():
                role = item.data(0, Qt.ItemDataRole.UserRole)
                if role and role[0] == 'tex':
                    _, txd_path, tex_name = role
                    by_file.setdefault(txd_path, []).append(tex_name.lower())
            if not by_file:
                return
            for txd_path, names in by_file.items():
                try:
                    with open(txd_path, 'rb') as f:
                        data = f.read()
                    texs = self._parse_txd_lightweight(data) or []
                    existing = {t['name'].lower() for t in self._mod_textures}
                    for tex in texs:
                        if tex['name'].lower() in names and tex['name'].lower() not in existing:
                            self._mod_textures.append(tex)
                            existing.add(tex['name'].lower())
                except Exception as e:
                    print(f"Texlist add error: {e}")
            self._populate_texture_list()
            self._tex_panel.setVisible(True)
            from PyQt6.QtCore import QTimer as _QTTex
            _QTTex.singleShot(0, self._update_tex_btn_compact)   # visibility alone doesn't fire resizeEvent; defer so layout has settled
            dlg.close()

        add_file_btn.clicked.connect(_add_whole_txd)
        add_tex_btn.clicked.connect(_add_selected_textures)
        dlg.exec()

    def _on_tex_selected(self): #vers 1
        """Texture row selected — show preview in status."""
        rows = self._tex_list.selectionModel().selectedRows()
        if not rows:
            return
        idx = rows[0].row()
        if 0 <= idx < len(self._mod_textures):
            tex = self._mod_textures[idx]
            self._set_status(
                f"Texture: {tex.get('name','?')}  "
                f"{tex.get('width',0)}×{tex.get('height',0)}  "
                f"{tex.get('format','?')}  "
                f"{tex.get('mipmaps',1)} mip(s)")

    def _tex_context_menu(self, pos): #vers 2
        """Right-click context menu on texture list."""
        from PyQt6.QtWidgets import QMenu
        rows = self._tex_list.selectionModel().selectedRows()
        menu = QMenu(self)
        if rows:
            menu.addAction("Remove selected texture(s)",
                           self._remove_selected_textures)
            menu.addAction("Export texture(s) as PNG…",
                           self._export_textures_as_png)
            menu.addSeparator()
        menu.addAction("Load TXD…",        self._load_txd_into_workshop)
        menu.addAction("Browse Texlist…",  self._browse_texlist_folder)
        if self._mod_textures:
            menu.addSeparator()
            menu.addAction("Pass to TXD Workshop", self._pass_textures_to_txd_workshop)
            menu.addAction("Pass to DP5 Workshop", self._pass_textures_to_dp5_workshop)
            menu.addAction("Save as TXD…",         self._save_textures_as_txd)
        menu.exec(self._tex_list.viewport().mapToGlobal(pos))

    def _remove_selected_textures(self): #vers 1
        rows = sorted(
            {r.row() for r in self._tex_list.selectionModel().selectedRows()},
            reverse=True)
        for r in rows:
            if 0 <= r < len(self._mod_textures):
                del self._mod_textures[r]
        self._populate_texture_list()

    def _export_textures_as_png(self): #vers 1
        """Export selected textures to PNG files."""
        rows = {r.row() for r in self._tex_list.selectionModel().selectedRows()}
        if not rows:
            return
        folder = QFileDialog.getExistingDirectory(self, "Export Textures to Folder")
        if not folder:
            return
        from PyQt6.QtGui import QImage
        exported = 0
        for idx in rows:
            if not (0 <= idx < len(self._mod_textures)):
                continue
            tex = self._mod_textures[idx]
            try:
                w, h = tex.get('width', 0), tex.get('height', 0)
                data = tex.get('pixel_data') or tex.get('compressed_data', b'')
                if data and w and h and 'DXT' not in tex.get('format', ''):
                    qimg = QImage(data[:w*h*4], w, h, QImage.Format.Format_RGBA8888)
                    out = os.path.join(folder, tex.get('name', f'tex_{idx}') + '.png')
                    qimg.save(out)
                    exported += 1
            except Exception as e:
                print(f"Export tex error: {e}")
        self._set_status(f"Exported {exported} texture(s) to {folder}")

    def _pass_textures_to_dp5_workshop(self): #vers 1
        """Selected textures (or all) into DP5 Workshop; DP5's Send back returns them here."""
        from apps.methods.dp5_link import send_textures_to_dp5
        if not self._mod_textures:
            return
        rows = sorted({i.row() for i in self._tex_list.selectionModel().selectedRows()})
        texs = [self._mod_textures[r] for r in rows if r < len(self._mod_textures)] or list(self._mod_textures)
        sent = send_textures_to_dp5(self, texs, self._titlebar_title(), self._texture_back_from_dp5)
        self._set_status(f"Passed {sent} texture(s) to DP5 Workshop")

    def _texture_back_from_dp5(self, tex, rgba, w, h): #vers 1
        """DP5 Send back: painted pixels into the texture, list and viewport."""
        from apps.methods.dp5_link import texture_from_dp5
        if tex not in self._mod_textures or texture_from_dp5(tex, rgba, w, h) is None:
            self._set_status("DP5: texture no longer loaded here")
            return
        self._populate_texture_list()
        self.preview_widget._upload_textures(self._mod_textures)  # re-upload all (replaces the old one)
        self.preview_widget.update()
        self._set_status(f"{tex.get('name')} updated from DP5 - Save TXD to keep")

    def _pass_textures_to_txd_workshop(self): #vers 1
        """Send current textures to TXD Workshop for editing."""
        if not self._mod_textures:
            return
        mw = getattr(self, 'main_window', None)

        # Build a minimal TXD in memory from self._mod_textures
        txd_data = self._build_txd_from_textures()

        if txd_data and mw and hasattr(mw, 'open_txd_workshop_docked'):
            # Write to a temp file and open
            import tempfile
            stem = os.path.splitext(
                os.path.basename(getattr(self, '_current_dff_path', 'model')))[0]
            tmp_dir = tempfile.mkdtemp()
            tmp_path = os.path.join(tmp_dir, f'{stem}.txd')
            with open(tmp_path, 'wb') as _f: _f.write(txd_data)
            mw.open_txd_workshop_docked(file_path=tmp_path)
            self._set_status(
                f"Passed {len(self._mod_textures)} texture(s) to TXD Workshop")
        elif txd_data:
            # Standalone mode — open TXD Workshop window
            import tempfile
            stem = os.path.splitext(
                os.path.basename(getattr(self, '_current_dff_path', 'model')))[0]
            tmp_dir = tempfile.mkdtemp()
            tmp_path = os.path.join(tmp_dir, f'{stem}.txd')
            with open(tmp_path, 'wb') as _f: _f.write(txd_data)
            try:
                from apps.components.Txd_Editor.txd_workshop import TXDWorkshop
                txd_win = TXDWorkshop(main_window=None)
                txd_win.setWindowTitle(f"TXD Workshop — {stem}.txd")
                txd_win.show(); txd_win.resize(1000, 700)
                txd_win.open_txd_file(tmp_path)
                if not hasattr(self, '_standalone_txd_windows'):
                    self._standalone_txd_windows = []
                self._standalone_txd_windows.append(txd_win)
            except Exception as e:
                QMessageBox.critical(self, "TXD Error", f"Failed to open TXD Workshop:\n{e}")
        else:
            QMessageBox.warning(self, "TXD", "Could not build TXD from current textures.")

    def _build_txd_from_textures(self): #vers 3
        """TXD bytes from self._mod_textures; untouched textures keep their bytes."""
        from apps.methods.txd_splice import txd_from_textures
        try:
            return txd_from_textures(self._mod_textures, getattr(self, '_current_txd_bytes', None))
        except Exception as e:
            print(f"TXD build error: {e}")
            return None

    def _save_textures_as_txd(self): #vers 1
        """Save current textures as a new TXD file."""
        if not self._mod_textures:
            return
        stem = os.path.splitext(
            os.path.basename(getattr(self, '_current_dff_path', 'model')))[0]
        path, _ = QFileDialog.getSaveFileName(
            self, "Save TXD",
            os.path.join(os.path.dirname(
                getattr(self, '_current_dff_path', '')), stem + '.txd'),
            "TXD Files (*.txd);;All Files (*)")
        if not path:
            return
        data = self._build_txd_from_textures()
        if data:
            with open(path, 'wb') as f:
                f.write(data)
            self._set_status(f"Saved TXD: {os.path.basename(path)}")
        else:
            QMessageBox.warning(self, "TXD", "Could not build TXD from current textures.")

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

    def _open_txd_smart(self): #vers 3
        """Open TXD — tries in order:
        1. IDE-linked TXD name → search current IMG (case-insensitive)
        2. DFF filename stem → search same folder as DFF (case-insensitive)
        3. DFF filename stem → search game_root recursively (case-insensitive)
        4. Ask user via file dialog, or skip
        Works the same whether docked or standalone."""
        import os, glob
        from PyQt6.QtWidgets import QFileDialog, QMessageBox

        mw  = getattr(self, 'main_window', None)
        dff_path = getattr(self, '_current_dff_path', '') or ''

        # Build candidate TXD names to search for (case-insensitive)
        candidates = []
        obj = getattr(self, '_current_ide_obj', None)
        if obj and getattr(obj, 'txd_name', ''):
            candidates.append(obj.txd_name.lower())
        if dff_path:
            candidates.append(os.path.splitext(os.path.basename(dff_path))[0].lower())
        candidates = list(dict.fromkeys(candidates))   # dedupe, preserve order

        def _open(path):  #vers 1
            if mw and hasattr(mw, 'open_txd_workshop_docked'):
                mw.open_txd_workshop_docked(file_path=path)
            else:
                from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
                open_txd_workshop(self, path)

        def _name_matches(filename, candidate):  #vers 1
            return os.path.splitext(filename.lower())[0] == candidate

        # 1. Search current IMG
        img = (getattr(mw, 'current_img', None) if mw else None) or \
              getattr(self, 'current_img', None)
        if img:
            for cand in candidates:
                for entry in getattr(img, 'entries', []):
                    if _name_matches(entry.name, cand):
                        if mw and hasattr(mw, 'open_txd_workshop_docked'):
                            mw.open_txd_workshop_docked(txd_name=entry.name)
                        return

        # 2. Same folder as DFF — case-insensitive
        if dff_path:
            dff_dir = os.path.dirname(dff_path)
            for fname in os.listdir(dff_dir):
                if fname.lower().endswith('.txd'):
                    stem = os.path.splitext(fname.lower())[0]
                    if stem in candidates:
                        _open(os.path.join(dff_dir, fname))
                        return

        # 3. Recursive game_root search
        xref = self._get_xref() if hasattr(self, '_get_xref') else None
        game_root = getattr(xref, 'game_root', '') if xref else ''
        if not game_root and mw:
            game_root = getattr(mw, '_game_root', '') or ''
        if game_root:
            for cand in candidates:
                for fpath in glob.glob(
                        os.path.join(game_root, '**', '*.txd'), recursive=True):
                    if os.path.splitext(os.path.basename(fpath).lower())[0] == cand:
                        _open(fpath)
                        return

        # 4. Not found — ask user or skip
        cand_str = ' / '.join(f"{c}.txd" for c in candidates) or 'TXD'
        ans = QMessageBox.question(
            self, "TXD Not Found",
            f"Could not automatically find '{cand_str}'.\n"
            "Browse for it, or skip?",
            QMessageBox.StandardButton.Open |
            QMessageBox.StandardButton.Cancel,
            QMessageBox.StandardButton.Open)
        if ans == QMessageBox.StandardButton.Open:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open TXD",
                os.path.dirname(dff_path) if dff_path else '',
                "Texture Files (*.txd *.wtd *.nft *.xtx *.chk);;TXD Files (*.txd);;All Files (*)")
            if path:
                _open(path)

    def _open_linked_txd(self): #vers 2

        """Open the IDE-linked TXD in TXD Workshop."""
        obj = getattr(self, '_current_ide_obj', None)
        if not obj or not obj.txd_name:
            return
        txd_name = obj.txd_name.lower()
        mw = getattr(self, 'main_window', None)

        # Try via main_window.open_txd_workshop_docked (IMG mode)
        if mw and hasattr(mw, 'open_txd_workshop_docked'):
            # Check if TXD is in the current IMG
            img = getattr(mw, 'current_img', None) or getattr(self, 'current_img', None)
            if img:
                for entry in getattr(img, 'entries', []):
                    if entry.name.lower() == txd_name + '.txd':
                        mw.open_txd_workshop_docked(txd_name=txd_name + '.txd')
                        return
            # Try standalone via xref game_root
            xref = self._get_xref()
            game_root = getattr(xref, 'game_root', '') if xref else ''
            if game_root:
                # Search for the TXD file on disk
                import glob
                pattern = os.path.join(game_root, '**', txd_name + '.txd')
                matches = glob.glob(pattern, recursive=True)
                if matches:
                    mw.open_txd_workshop_docked(file_path=matches[0])
                    return

        # Fallback: open file dialog pre-filtered to TXD
        from PyQt6.QtWidgets import QFileDialog
        path, _ = QFileDialog.getOpenFileName(
            self, f"Open TXD for {obj.txd_name}",
            getattr(self, '_current_dff_path', ''),
            "Texture Files (*.txd *.wtd *.nft *.xtx *.chk);;TXD Files (*.txd);;All Files (*)")
        if path and mw and hasattr(mw, 'open_txd_workshop_docked'):
            mw.open_txd_workshop_docked(file_path=path)
        elif path:
            from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
            open_txd_workshop(self, path)

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

    def open_dff_file(self, file_path: str): #vers 1
        """Open and display a GTA DFF model file."""
        self.current_col_file = None   # clear COL mode
        # Hide COL format combo when DFF is loaded
        if hasattr(self, 'format_combo'):
            self.format_combo.setVisible(False)
        try:
            from apps.methods.dff_parser import load_dff, detect_dff
            with open(file_path, 'rb') as f:
                data = f.read()
            if not detect_dff(data):
                QMessageBox.warning(self, "Invalid DFF",
                    "File does not appear to be a valid RenderWare DFF model.")
                return
            model = load_dff(file_path)
            if model is None:
                QMessageBox.warning(self, "DFF Error", "Failed to parse DFF file.")
                return
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
        except Exception as e:
            import traceback; traceback.print_exc()
            QMessageBox.critical(self, "DFF Error", f"Failed to open DFF:\n{e}")

    def _populate_dff_detail_table(self, model): #vers 1
        """Fill the detail table (collision_list) with DFF geometry info."""
        try:
            tbl = self.collision_list
            tbl.setRowCount(0)
            for i, geom in enumerate(model.geometries):
                atomic = next((a for a in model.atomics if a.geometry_index == i), None)
                frame_name = model.get_frame_name(atomic.frame_index) if atomic else f"geom_{i}"
                row = tbl.rowCount(); tbl.insertRow(row)
                tbl.setItem(row, 0, self._tbl_item(frame_name))
                # Determine pixel storage mode from depth (from adapter knowledge)
                fmt = "PSMT8" if not geom.normals and not geom.colors else "Mesh"
                tbl.setItem(row, 1, self._tbl_item("DFF Geometry"))
                rw_ver = f"0x{model.rw_version:08X}" if model.rw_version else "—"
                tbl.setItem(row, 2, self._tbl_item(rw_ver))
                est_size = geom.vertex_count * 12 + geom.triangle_count * 8
                tbl.setItem(row, 3, self._tbl_item(f"{est_size // 1024:.1f} KB"))
                tbl.setItem(row, 4, self._tbl_item(str(geom.vertex_count)))
                tbl.setItem(row, 5, self._tbl_item(str(geom.triangle_count)))
                tbl.setItem(row, 6, self._tbl_item(str(geom.material_count)))
                tbl.setItem(row, 7, self._tbl_item(str(len(geom.uv_layers))))
            tbl.resizeColumnsToContents()
        except Exception as e:
            print(f"DFF detail table error: {e}")

    def _tbl_item(self, text: str): #vers 1
        """Helper: create a non-editable QTableWidgetItem."""
        from PyQt6.QtWidgets import QTableWidgetItem
        from PyQt6.QtCore import Qt
        item = QTableWidgetItem(str(text))
        item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
        return item

    def _populate_frame_tree(self, model): #vers 1
        """Populate the frame/bone hierarchy tree widget."""
        tree = getattr(self, '_frame_tree', None)
        if tree is None:
            return
        tree.clear()
        tree.setHeaderLabels(["Frame", "Parent", "Pos"])

        from PyQt6.QtWidgets import QTreeWidgetItem
        items = {}
        for i, frame in enumerate(model.frames):
            name = frame.name or f"Frame_{i}"
            pos = frame.position
            pos_str = f"({pos.x:.2f}, {pos.y:.2f}, {pos.z:.2f})"
            parent_str = str(frame.parent_index) if frame.parent_index >= 0 else "—"
            item = QTreeWidgetItem([name, parent_str, pos_str])
            item.setData(0, 0x0100, i)  # store frame index
            items[i] = item
            if frame.parent_index < 0 or frame.parent_index not in items:
                tree.addTopLevelItem(item)
            else:
                items[frame.parent_index].addChild(item)
        tree.expandAll()
        tree.resizeColumnToContents(0)

        # Show the frame tree panel
        panel = getattr(self, '_frame_tree_panel', None)
        if panel:
            panel.setVisible(True)

    def _on_frame_tree_clicked(self, item, column): #vers 1
        """When user clicks a frame in the tree, highlight the associated geometry."""
        frame_idx = item.data(0, 0x0100)
        if frame_idx is None:
            return
        model = getattr(self, '_current_dff_model', None)
        if not model:
            return
        # Find geometry linked to this frame via atomics
        atomic = next((a for a in model.atomics if a.frame_index == frame_idx), None)
        if atomic is not None:
            geom_idx = atomic.geometry_index
            adapters = getattr(self, '_dff_adapters', [])
            if 0 <= geom_idx < len(adapters):
                self._show_dff_geometry(geom_idx)
                # Also select the row in the compact list
                self.mod_compact_list.selectRow(geom_idx)
            frame = model.frames[frame_idx]
            name = frame.name or f"Frame_{frame_idx}"
            pos = frame.position
            self._set_status(
                f"Frame [{frame_idx}]: '{name}'  "
                f"pos=({pos.x:.3f}, {pos.y:.3f}, {pos.z:.3f})  "
                f"parent={frame.parent_index}")

    def _display_dff_model(self, model): #vers 3
        """Populate the workshop UI with a loaded DFF model.
        Fills the QTableWidget mesh list and shows first geometry in 3D viewport."""
        try:
            if not hasattr(self, 'mod_compact_list'):
                return

            tbl = self.mod_compact_list
            tbl.setRowCount(0)
            self._dff_adapters = []   # keep adapters alive

            for i, geom in enumerate(model.geometries):
                atomic = next((a for a in model.atomics if a.geometry_index == i), None)
                frame_name = model.get_frame_name(atomic.frame_index) if atomic else f"geom_{i}"
                adapter = _DFFGeometryAdapter(geom, i, dff_model=model, atomic=atomic)
                self._dff_adapters.append(adapter)

                row = tbl.rowCount()
                tbl.insertRow(row)

                # Col 0: name
                name_item = QTableWidgetItem(f"[{i}] {frame_name}")
                name_item.setData(Qt.ItemDataRole.UserRole, i)
                name_item.setToolTip(f"{geom.vertex_count}v / {geom.triangle_count}t / {geom.material_count}mat")
                tbl.setItem(row, 0, name_item)

                # Col 1: stats
                stats_item = QTableWidgetItem(
                    f"{geom.vertex_count}v  {geom.triangle_count}t  {geom.material_count}m")
                stats_item.setData(Qt.ItemDataRole.UserRole, i)
                tbl.setItem(row, 1, stats_item)

            # Disconnect ALL existing handlers, then connect DFF-specific one
            for handler in (self._on_compact_col_selected,
                            self._on_dff_geom_selected,
                            self._on_dff_geom_selected_tbl):
                try:
                    tbl.itemSelectionChanged.disconnect(handler)
                except Exception:
                    pass
            tbl.itemSelectionChanged.connect(self._on_dff_geom_selected_tbl)

            # selectRow(0) fires the signal which calls _show_dff_geometry(0)
            if tbl.rowCount() > 0:
                tbl.selectRow(0)
            elif self._dff_adapters:
                # No table rows somehow — show manually
                self._show_dff_geometry(0)

        except Exception as e:
            import traceback; traceback.print_exc()
            print(f"DFF display error: {e}")

    def _on_dff_geom_selected_tbl(self): #vers 1
        """QTableWidget selection changed → update viewport."""
        tbl = self.mod_compact_list
        rows = tbl.selectionModel().selectedRows()
        if rows:
            row = rows[0].row()
            item = tbl.item(row, 0)
            if item:
                idx = item.data(Qt.ItemDataRole.UserRole)
                if idx is not None:
                    self._show_dff_geometry(int(idx))

    def _on_dff_geom_selected(self, row: int): #vers 1
        """Called when user selects a geometry in the mesh list."""
        self._show_dff_geometry(row)

    def _show_dff_geometry(self, index: int): #vers 3
        """Push a DFF geometry into the GL viewport using full frame hierarchy."""
        adapters = getattr(self, '_dff_adapters', [])\
        
        model = getattr(self, '_current_dff_model', None)
        pw = getattr(self, 'preview_widget', None)
        if not pw: return

        if model and model.frames and model.atomics:
            # Full assembly — correct UV mapping and world positions
            pw.load_all_geometries(
                model.geometries, [g.materials for g in model.geometries],
                model.frames, model.atomics)
        elif model and index < len(model.geometries):
            g = model.geometries[index]
            pw.load_geometry(g, g.materials)
        pw.update()

        if model and index < len(model.geometries):
            geom = model.geometries[index]
            self._set_status(
                f"Geometry [{index}]: {len(geom.vertices)} vertices, "
                f"{geom.triangle_count} triangles, "
                f"{geom.material_count} materials"
            )


    def _export_dff_obj(self): #vers 1
        """Export the currently loaded DFF model to Wavefront OBJ + MTL."""
        model = getattr(self, '_current_dff_model', None)
        if not model or not model.geometries:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "No Model", "Open a DFF file first.")
            return
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        import os

        base_name = os.path.splitext(
            os.path.basename(getattr(self, '_current_dff_path', 'model.dff'))
        )[0]
        out_path, _ = QFileDialog.getSaveFileName(
            self, "Export OBJ", base_name + ".obj",
            "Wavefront OBJ (*.obj);;All Files (*)")
        if not out_path:
            return

        mtl_path = os.path.splitext(out_path)[0] + ".mtl"
        mtl_name = os.path.basename(mtl_path)

        try:
            lines_obj = [
                f"# Exported by {App_name}",
                f"# Source: {getattr(self, '_current_dff_path', 'unknown')}",
                f"mtllib {mtl_name}",
                "",
            ]
            lines_mtl = [f"# Material library — {App_name}", ""]

            vertex_offset = 1   # OBJ is 1-indexed
            uv_offset     = 1

            for geom_idx, geom in enumerate(model.geometries):
                # Find frame name via atomics
                atomic = next(
                    (a for a in model.atomics if a.geometry_index == geom_idx), None)
                frame_name = (model.get_frame_name(atomic.frame_index)
                              if atomic else f"geometry_{geom_idx}")
                safe_name = frame_name.replace(' ', '_').replace('/', '_')

                lines_obj.append(f"o {safe_name}")

                # Vertices
                for v in geom.vertices:
                    lines_obj.append(f"v {v.x:.6f} {v.y:.6f} {v.z:.6f}")

                # Normals
                has_normals = bool(geom.normals)
                if has_normals:
                    for n in geom.normals:
                        lines_obj.append(f"vn {n.x:.6f} {n.y:.6f} {n.z:.6f}")

                # UV coordinates (first layer only)
                has_uvs = bool(geom.uv_layers)
                if has_uvs:
                    for uv in geom.uv_layers[0]:
                        lines_obj.append(f"vt {uv.u:.6f} {uv.v:.6f}")

                # Materials
                for mat_idx, mat in enumerate(geom.materials):
                    mat_id = f"{safe_name}_mat{mat_idx}"
                    lines_mtl.append(f"newmtl {mat_id}")
                    r = mat.color.r / 255.0
                    g = mat.color.g / 255.0
                    b = mat.color.b / 255.0
                    lines_mtl.append(f"Kd {r:.4f} {g:.4f} {b:.4f}")
                    lines_mtl.append(f"Ka {mat.ambient:.4f} {mat.ambient:.4f} {mat.ambient:.4f}")
                    lines_mtl.append(f"Ks {mat.specular:.4f} {mat.specular:.4f} {mat.specular:.4f}")
                    lines_mtl.append(f"d 1.0")
                    if mat.texture_name:
                        lines_mtl.append(f"map_Kd {mat.texture_name}.png")
                    lines_mtl.append("")

                # Faces — grouped by material
                tris_by_mat = {}
                for tri in geom.triangles:
                    tris_by_mat.setdefault(tri.material_id, []).append(tri)

                for mat_id_idx, tris in sorted(tris_by_mat.items()):
                    if geom.materials and mat_id_idx < len(geom.materials):
                        mat_obj_name = f"{safe_name}_mat{mat_id_idx}"
                        lines_obj.append(f"usemtl {mat_obj_name}")
                    else:
                        lines_obj.append("usemtl default")

                    for tri in tris:
                        v1 = tri.v1 + vertex_offset
                        v2 = tri.v2 + vertex_offset
                        v3 = tri.v3 + vertex_offset
                        if has_uvs and has_normals:
                            uv1 = tri.v1 + uv_offset
                            uv2 = tri.v2 + uv_offset
                            uv3 = tri.v3 + uv_offset
                            lines_obj.append(
                                f"f {v1}/{uv1}/{v1} {v2}/{uv2}/{v2} {v3}/{uv3}/{v3}")
                        elif has_uvs:
                            uv1 = tri.v1 + uv_offset
                            uv2 = tri.v2 + uv_offset
                            uv3 = tri.v3 + uv_offset
                            lines_obj.append(
                                f"f {v1}/{uv1} {v2}/{uv2} {v3}/{uv3}")
                        elif has_normals:
                            lines_obj.append(
                                f"f {v1}//{v1} {v2}//{v2} {v3}//{v3}")
                        else:
                            lines_obj.append(f"f {v1} {v2} {v3}")

                vertex_offset += len(geom.vertices)
                if has_uvs:
                    uv_offset += len(geom.uv_layers[0])
                lines_obj.append("")

            # Write files
            with open(out_path, 'w') as f:
                f.write("\n".join(lines_obj))
            with open(mtl_path, 'w') as f:
                f.write("\n".join(lines_mtl))

            total_v = sum(len(g.vertices) for g in model.geometries)
            total_f = sum(len(g.triangles) for g in model.geometries)
            QMessageBox.information(
                self, "OBJ Exported",
                f"Exported {len(model.geometries)} geometr{'y' if len(model.geometries)==1 else 'ies'}\n"
                f"{total_v:,} vertices  {total_f:,} triangles\n"
                f"→ {os.path.basename(out_path)}\n"
                f"→ {os.path.basename(mtl_path)}")
            self._set_status(
                f"Exported OBJ: {os.path.basename(out_path)}  "
                f"({total_v:,}v {total_f:,}f)")

        except Exception as e:
            import traceback; traceback.print_exc()
            QMessageBox.critical(self, "OBJ Export Error", str(e))

    def _toggle_col_view(self): #vers 1
        """Toggle between detail table and compact thumbnail+name list."""
        if self._col_view_mode == 'list':
            self._col_view_mode = 'detail'
            self.collision_list.setVisible(False)
            self.mod_compact_list.setVisible(True)
            self.col_view_toggle_btn.setText("[=]")
            self.col_view_toggle_btn.setToolTip("Switch to detail table view")
            if (self.mod_compact_list.rowCount() == 0
                    and self.collision_list.rowCount() > 0
                    and self.current_col_file):
                self._populate_compact_col_list()
        else:
            self._col_view_mode = 'list'
            self.mod_compact_list.setVisible(False)
            self.collision_list.setVisible(True)
            self.col_view_toggle_btn.setText("[T]")
            self.col_view_toggle_btn.setToolTip("Switch to compact thumbnail view")

    def _populate_compact_col_list(self): #vers 1
        """Fill compact two-column list (icon + name/version/counts)."""
        try:
            self.mod_compact_list.setRowCount(0)
            models = getattr(self.current_col_file, 'models', [])
            for i, model in enumerate(models):
                self.mod_compact_list.insertRow(i)

                # Col 0: real collision thumbnail
                icon_item = QTableWidgetItem()
                pm = self._generate_collision_thumbnail(model, 64, 64,
                                yaw=self._thumb_yaw, pitch=self._thumb_pitch)
                icon_item.setData(Qt.ItemDataRole.DecorationRole, pm)
                self.mod_compact_list.setItem(i, 0, icon_item)

                # Col 1: name + stats
                name = getattr(model, 'name', '') or f'model_{i}'
                ver  = getattr(model, 'version', None)
                ver_str = ver.name if hasattr(ver, 'name') else str(ver) if ver else '?'
                spheres = len(getattr(model, 'spheres',  []))
                boxes   = len(getattr(model, 'boxes',    []))
                verts   = len(getattr(model, 'vertices', []))
                faces   = len(getattr(model, 'faces',    []))

                line1 = name
                line2 = "Version: 1" + ver_str
                line3 = "Spheres: " + str(spheres) + "  Boxes: " + str(boxes)
                line4 = "Verts: "   + str(verts)   + "  Faces: " + str(faces)
                details = line1 + "\n" + line2 + "\n" + line3 + "\n" + line4

                det_item = QTableWidgetItem(details)
                det_item.setToolTip(details)
                self.mod_compact_list.setItem(i, 1, det_item)
                self.mod_compact_list.setRowHeight(i, 72)

            self.mod_compact_list.setColumnWidth(0, 72)
        except Exception as e:
            print("_populate_compact_col_list error: " + str(e))

    def _on_compact_col_selected(self): #vers 5
        """Handle compact [=] list selection — routes to DFF or COL handler.
        Aug 1 2026: also routes to _on_ipl_model_row_selected when
        self._ipl_models_mode is set (the Models dock is currently
        listing an IPL's referenced models, not a DFF/COL file's own
        parts)"""
        try:
            rows = self.mod_compact_list.selectionModel().selectedRows()
            if not rows:
                return
            row = rows[0].row()
            if getattr(self, '_ipl_models_mode', False):
                self._on_ipl_model_row_selected(row)
                return
            # DFF mode: _dff_adapters set when a DFF is loaded
            if getattr(self, '_dff_adapters', None):
                self._on_dff_geom_selected_tbl()
            else:
                self._select_model_by_row(row)
        except Exception as e:
            print("_on_compact_col_selected error: " + str(e))

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

    # - Selection helpers

    def _select_all_models(self): #vers 1
        """Select all entries in the active list (Ctrl+A)."""
        lw = (self.mod_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
              else self.collision_list)
        lw.selectAll()

    def _invert_selection(self): #vers 1
        """Invert the current selection."""
        lw = (self.mod_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
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

    # - Sort

    def _sort_models(self, key: str = 'name'): #vers 1
        """Sort collision models in place by key: 'name','version','faces','boxes','spheres','vertices'."""
        if not getattr(self, "current_col_file", None): return
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

    def _show_sort_menu(self): #vers 1
        """Show sort options popup."""
        from PyQt6.QtWidgets import QMenu
        m = QMenu(self)
        m.addAction("Sort by Name (A→Z)",     lambda: self._sort_models('name'))
        m.addAction("Sort by Version",         lambda: self._sort_models('version'))
        m.addAction("Sort by Faces (most)",    lambda: self._sort_models_desc('faces'))
        m.addAction("Sort by Boxes (most)",    lambda: self._sort_models_desc('boxes'))
        m.addAction("Sort by Spheres (most)",  lambda: self._sort_models_desc('spheres'))
        m.addAction("Sort by Vertices (most)", lambda: self._sort_models_desc('vertices'))
        m.exec(self.cursor().pos())

    def _sort_models_desc(self, key: str): #vers 1
        """Sort descending (largest first)."""
        if not getattr(self, "current_col_file", None): return
        models = getattr(self.current_col_file, 'models', [])
        def k(m):  #vers 1
            return len(getattr(m, key, []))
        models.sort(key=k, reverse=True)
        self._populate_collision_list()
        self._populate_compact_col_list()
        self._set_status(f"Sorted by {key} (descending).")

    # - Pin / lock entries from editing

    def _toggle_pin_selected(self): #vers 1
        """Toggle pin (edit-lock) on selected models. Pinned models can't be deleted/renamed."""
        if not getattr(self, "current_col_file", None): return
        lw = (self.mod_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
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

    def _save_file(self): #vers 3
        """Save current DFF model. Falls back to Save As if no path set."""
        from PyQt6.QtWidgets import QFileDialog
        dff_model = getattr(self, '_current_dff_model', None)
        dff_path  = getattr(self, '_current_dff_path', '') or ''

        if not dff_model:
            QMessageBox.warning(self, "Save", "No DFF model loaded to save.")
            return

        if not dff_path:
            path, _ = QFileDialog.getSaveFileName(
                self, "Save DFF", '',
                "DFF Files (*.dff);;MDL Files (*.mdl);;All Files (*)")
            if not path:
                return
            dff_path = path

        try:
            from apps.methods.dff_parser import DFFWriter
            raw = DFFWriter.write(dff_model)
            with open(dff_path, 'wb') as f:
                f.write(raw)
            self._current_dff_path = dff_path
            fname = os.path.basename(dff_path)
            self._set_status(f"Saved: {fname}")
            mw = getattr(self, 'main_window', None)
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(f"Saved DFF: {fname} ({len(raw):,} bytes)")
        except NotImplementedError:
            QMessageBox.information(self, "Save DFF",
                "DFF round-trip save is not yet fully implemented.\n"
                "Use Export → OBJ for now.")
        except Exception as e:
            QMessageBox.critical(self, "Save Error", str(e))


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
            os.path.dirname(os.path.dirname(__file__)),   # Map_Editor/
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


    def open_col_file(self, file_path): #vers 3
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
                self.main_window.log_message(f"✅ Loaded COL: {os.path.basename(file_path)} ({model_count} models)")

            print(f"Opened COL file: {file_path} with {model_count} models")
            return True

        except Exception as e:
            print(f"Error opening COL file: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to open COL file:\n{str(e)}")
            return False


    def _find_all_paint_btns(self): #vers 1
        """Return all paint buttons from both icon and text panels."""
        btns = []
        b = getattr(self, 'paint_btn', None)
        if b: btns.append(b)
        # Walk icon panel for any other paint_btn that was overwritten
        ip = getattr(self, '_transform_icon_panel_ref', None)
        if ip:
            from PyQt6.QtWidgets import QPushButton
            for child in ip.findChildren(QPushButton):
                if child.toolTip() and 'paint' in child.toolTip().lower()                    and child not in btns:
                    btns.append(child)
        return btns

    def _set_col_buttons_enabled(self, enabled: bool): #vers 1
        """Enable/disable all transform buttons in BOTH icon and text panels.
        The text panel overwrites self.X refs, so when the icon panel is visible
        (narrow mode) those refs point to hidden buttons. Walk the icon panel too.
        """
        col_btn_attrs = [
            'flip_vert_btn', 'flip_horz_btn', 'rotate_cw_btn', 'rotate_ccw_btn',
            'analyze_btn', 'copy_btn', 'delete_surface_btn', 'duplicate_surface_btn',
            'paint_btn', 'surface_type_btn', 'surface_edit_btn', 'build_from_txd_btn',
            'show_shadow_btn', 'create_shadow_btn', 'remove_shadow_btn',
            'compress_btn', 'uncompress_btn', 'switch_btn', 'convert_btn',
        ]
        for attr in col_btn_attrs:
            btn = getattr(self, attr, None)
            if btn is not None:
                btn.setEnabled(enabled)
        icon_panel = getattr(self, '_transform_icon_panel_ref', None)
        if icon_panel:
            from PyQt6.QtWidgets import QPushButton
            for btn in icon_panel.findChildren(QPushButton):
                btn.setEnabled(enabled)

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



    def _load_txd_file_from_data(self, data: bytes, name: str): #vers 1
        """Load TXD from raw bytes into the texture panel."""
        try:
            import tempfile
            tmp_dir = tempfile.mkdtemp()
            tmp_path = os.path.join(tmp_dir, name if name.lower().endswith('.txd') else os.path.splitext(name)[0] + '.txd')
            with open(tmp_path, 'wb') as _f: _f.write(data)
            self._load_txd_file(tmp_path)
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"TXD load error: {e}")

    def _show_model_search(self): #vers 1
        """Toggle search box in left panel."""
        box = getattr(self, '_model_search_box', None)
        if not box:
            return
        visible = not box.isVisible()
        box.setVisible(visible)
        if visible:
            box.setFocus()
        else:
            box.clear()

    def _filter_model_list(self, text: str): #vers 2
        """Filter left panel list by search text."""
        from apps.methods.file_list_filter import apply_file_filter
        apply_file_filter(self)                        # search text + type filter together

    def _on_col_selected(self, item): #vers 3
        """Handle entry selection from left panel — routes by extension."""
        try:
            entry = item.data(Qt.ItemDataRole.UserRole)
            if not entry:
                return
            name = entry.name.lower()
            data = self._extract_col_from_img(entry)
            if not data:
                return
            if name.endswith('.dff'):
                import tempfile, os as _os
                tmp_dir = tempfile.mkdtemp()
                tmp_path = _os.path.join(tmp_dir, entry.name)
                with open(tmp_path, 'wb') as _f: _f.write(data)
                self.open_dff_file(tmp_path)
            elif name.endswith('.col'):
                import tempfile, os as _os
                self.current_col_data = data
                self.current_col_name = entry.name
                tmp_path = _os.path.join(tempfile.mkdtemp(), entry.name)
                with open(tmp_path, 'wb') as _f: _f.write(data)
                self.open_col_file(tmp_path)
            elif name.endswith('.txd'):
                self._load_txd_file_from_data(data, entry.name)
        except Exception as e:
            import traceback; traceback.print_exc()
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error selecting entry: {e}")


    def _extract_col_from_img(self, entry): #vers 2
        """Extract TXD data from IMG entry"""
        try:
            if not self.current_img:
                return None
            return self.current_img.read_entry_data(entry)
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Extract error: {str(e)}")
            return None





    def _project_model_2d(self, model, width, height, padding=8,
                          yaw=0.0, pitch=0.0,
                          flip_h=False, flip_v=False): #vers 3
        """Project COL model geometry to 2D canvas using yaw/pitch rotation."""
        import math
        def _rot(pts3):  #vers 1
            result = []
            yr = math.radians(yaw)
            pr = math.radians(pitch)
            cy, sy = math.cos(yr), math.sin(yr)
            cp, sp = math.cos(pr), math.sin(pr)
            for x, y, z in pts3:
                # yaw around Z axis
                rx = x*cy - y*sy
                ry = x*sy + y*cy
                rz = z
                # pitch around X axis
                rx2 = rx
                ry2 = ry*cp - rz*sp
                rz2 = ry*sp + rz*cp
                result.append((rx2, ry2))  # project onto screen plane
            return result

        def _pts3(model):  #vers 1
            def vc(v):  #vers 1
                if hasattr(v,'position'): return (v.position.x,v.position.y,v.position.z)
                return (v.x,v.y,v.z)
            def sc(s):  #vers 1
                c = s.center
                if hasattr(c,'x'): return (c.x,c.y,c.z)
                return (c[0],c[1],c[2])
            def bc(b, mn):  #vers 1
                pt = (b.min_point if mn else b.max_point) if hasattr(b,'min_point') else (b.min if mn else b.max)
                if hasattr(pt,'x'): return (pt.x,pt.y,pt.z)
                return (pt[0],pt[1],pt[2])
            pts = []
            for s in getattr(model,'spheres',[]): x,y,z=sc(s); r=s.radius; pts+=[(x-r,y-r,z-r),(x+r,y+r,z+r)]
            for b in getattr(model,'boxes',  []): pts+=[bc(b,True),bc(b,False)]
            for v in getattr(model,'vertices',[]): pts.append(vc(v))
            return pts

        pts_3d = _pts3(model)
        pts_2d = _rot(pts_3d) if pts_3d else []
        if not pts_2d:
            return 1.0, width//2, height//2, []
        xs = [p[0] for p in pts_2d]
        ys = [p[1] for p in pts_2d]
        mn_x, mx_x = min(xs), max(xs)
        mn_y, mx_y = min(ys), max(ys)
        rng_x = mx_x - mn_x or 1.0
        rng_y = mx_y - mn_y or 1.0
        scale = min((width - padding*2) / rng_x, (height - padding*2) / rng_y)
        cx = (mn_x + mx_x) / 2
        cy = (mn_y + mx_y) / 2
        ox = width  / 2 - cx * scale
        oy = height / 2 - cy * scale

        result = []
        for px, py in pts_2d:
            sx = px * scale + ox
            sy = py * scale + oy
            if flip_h: sx = width - sx
            if flip_v: sy = height - sy
            result.append((sx, sy))
        return scale, ox, oy, result

    def _draw_col_model(self, painter, model, width, height, padding=4,
                       yaw=0.0, pitch=0.0,
                       flip_h=False, flip_v=False): #vers 3
        """Draw COL model onto a QPainter — used by both thumbnail and preview."""
        from PyQt6.QtGui import QPen, QBrush, QColor
        from PyQt6.QtCore import QRectF, QPointF
        import math

        scale, ox, oy, _ = self._project_model_2d(
            model, width, height, padding,
            yaw=yaw, pitch=pitch, flip_h=flip_h, flip_v=flip_v)

        yr = math.radians(yaw);  cy, sy = math.cos(yr), math.sin(yr)
        pr = math.radians(pitch); cp, sp = math.cos(pr), math.sin(pr)

        def _to2d(x, y, z):  #vers 1
            rx  = x*cy - y*sy
            ry  = x*sy + y*cy
            rx2 = rx
            ry2 = ry*cp - z*sp
            sx = rx2 * scale + ox
            sy2 = ry2 * scale + oy
            if flip_h: sx  = width  - sx
            if flip_v: sy2 = height - sy2
            return sx, sy2

        def _get3(obj):  #vers 1
            if hasattr(obj,'x'):        return obj.x, obj.y, obj.z
            elif hasattr(obj,'position'): return obj.position.x, obj.position.y, obj.position.z
            else: return float(obj[0]), float(obj[1]), float(obj[2])

        def proj_pt(obj):  #vers 1
            return _to2d(*_get3(obj))

        def wx(v): return (width  - (v*scale+ox)) if flip_h else (v*scale+ox)  #vers 1
        def wy(v): return (height - (v*scale+oy)) if flip_v else (v*scale+oy)  #vers 1

        # Mesh faces — filled triangles (grey)
        verts = getattr(model, 'vertices', [])
        faces = getattr(model, 'faces', [])
        if verts and faces:
            painter.setPen(QPen(QColor(120, 180, 120, 180), 0.5))
            painter.setBrush(QBrush(QColor(60, 120, 60, 80)))
            from PyQt6.QtGui import QPolygonF
            for face in faces:
                idx = getattr(face, 'vertex_indices', None)
                if idx is None:
                    fa = getattr(face, 'a', None)
                    if fa is not None:
                        idx = (fa, face.b, face.c)
                if idx and len(idx) == 3:
                    try:
                        p0x, p0y = proj_pt(verts[idx[0]])
                        p1x, p1y = proj_pt(verts[idx[1]])
                        p2x, p2y = proj_pt(verts[idx[2]])
                        poly = QPolygonF([
                            QPointF(p0x, p0y),
                            QPointF(p1x, p1y),
                            QPointF(p2x, p2y),
                        ])
                        painter.drawPolygon(poly)
                    except (IndexError, AttributeError):
                        pass

        # Boxes — yellow outline
        painter.setPen(QPen(QColor(220, 180, 50), max(1.0, scale * 0.05)))
        painter.setBrush(QBrush(QColor(220, 180, 50, 40)))
        for box in getattr(model, 'boxes', []):
            bmin_obj = box.min_point if hasattr(box, 'min_point') else box.min
            bmax_obj = box.max_point if hasattr(box, 'max_point') else box.max
            x1, y1 = proj_pt(bmin_obj)
            x2, y2 = proj_pt(bmax_obj)
            painter.drawRect(QRectF(min(x1,x2), min(y1,y2),
                                    abs(x2-x1) or 2, abs(y2-y1) or 2))

        # Spheres — cyan outline
        painter.setPen(QPen(QColor(80, 200, 220), max(1.0, scale * 0.05)))
        painter.setBrush(QBrush(QColor(80, 200, 220, 40)))
        for sph in getattr(model, 'spheres', []):
            r = sph.radius * scale
            cx, cy = proj_pt(sph.center)
            painter.drawEllipse(QRectF(cx - r, cy - r, r * 2 or 2, r * 2 or 2))

    def _generate_collision_thumbnail(self, model, width=64, height=64,
                                      yaw=0.0, pitch=0.0): #vers 2
        """Generate a small QPixmap thumbnail of a COL model."""
        from PyQt6.QtGui import QPixmap, QPainter, QPen
        pixmap = QPixmap(width, height)
        pixmap.fill(self._get_ui_color('viewport_bg'))
        has_data = (getattr(model, 'spheres', []) or
                    getattr(model, 'boxes', []) or
                    getattr(model, 'vertices', []))
        if not has_data:
            painter = QPainter(pixmap)
            painter.setPen(QPen(self._get_ui_color('viewport_text'), 1))
            painter.drawLine(4, 4, width-4, height-4)
            painter.drawLine(width-4, 4, 4, height-4)
            painter.end()
            return pixmap
        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        self._draw_col_model(painter, model, width, height, padding=4,
                             yaw=yaw, pitch=pitch)
        painter.end()
        return pixmap

    def _render_collision_preview(self, model, width=400, height=400,
                                  yaw=0.0, pitch=0.0,
                                  flip_h=False, flip_v=False): #vers 3
        """Render a full-size QPixmap preview of a COL model.
        yaw/pitch are Euler angles in degrees for free rotation.
        """
        from PyQt6.QtGui import QPixmap, QPainter, QColor, QFont
        from PyQt6.QtCore import Qt
        pixmap = QPixmap(width, height)
        pixmap.fill(self._get_ui_color('viewport_bg'))
        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        has_data = (getattr(model, 'spheres', []) or
                    getattr(model, 'boxes', []) or
                    getattr(model, 'vertices', []))

        if not has_data:
            painter.setPen(self._get_ui_color('viewport_text'))
            painter.setFont(QFont('Arial', 11))
            painter.drawText(pixmap.rect(), Qt.AlignmentFlag.AlignCenter, "No geometry data")
            painter.end()
            return pixmap

        self._draw_col_model(painter, model, width, height, padding=20,
                            yaw=yaw, pitch=pitch,
                            flip_h=flip_h, flip_v=flip_v)

        # Legend
        painter.setFont(QFont('Arial', 8))
        y = height - 52
        for color, label in [
            (QColor(60, 120, 60),   f"Mesh  F:{len(getattr(model,'faces',[]))} V:{len(getattr(model,'vertices',[]))}"),
            (QColor(220, 180, 50),  f"Boxes  {len(getattr(model,'boxes',[]))}"),
            (QColor(80, 200, 220),  f"Spheres  {len(getattr(model,'spheres',[]))}"),
        ]:
            painter.setPen(color)
            painter.drawText(6, y, label)
            y += 14

        # Model name
        name = getattr(model, 'name', '')
        if name:
            painter.setPen(self._get_ui_color('border'))
            painter.setFont(QFont('Arial', 9))
            painter.drawText(6, 14, name)

        painter.end()
        return pixmap

    def _on_collision_selected(self): #vers 8
        """Handle [T] detail table selection."""
        try:
            rows = self.collision_list.selectionModel().selectedRows()
            if rows:
                self._select_model_by_row(rows[0].row())
        except Exception as e:
            print("_on_collision_selected error: " + str(e))

    def _select_model_by_row(self, row): #vers 4
        """Load model by row index into preview — COL mode only."""
        try:
            if getattr(self, '_dff_adapters', None):
                return   # DFF mode — handled by _on_dff_geom_selected_tbl
            if not getattr(self, 'current_col_file', None):
                return
            models = getattr(self.current_col_file, 'models', [])
            if row < 0 or row >= len(models):
                return
            model = models[row]
            model_name = getattr(model, 'name', f'Model_{row}')

            # Debug counts
            nb = len(getattr(model,'boxes',[]));  ns = len(getattr(model,'spheres',[]))
            nv = len(getattr(model,'vertices',[])); nf = len(getattr(model,'faces',[]))
            print(f"SELECT [{row}] {model_name}: V={nv} F={nf} B={nb} S={ns}")

            # Name field
            if hasattr(self, 'info_name'):
                self.info_name.setText(model_name)

            # Push model into viewport
            pw = getattr(self, 'preview_widget', None)
            if pw and getattr(model, 'geometries', None):   # COL models have none
                pw.set_current_model(model, row)

            # Spin thumbnail in detail list
            self._start_thumbnail_spin(row, model)

        except Exception as e:
            import traceback; traceback.print_exc()
            print(f"_select_model_by_row error: {e}")


    def _show_collision_context_menu(self, position): #vers 5
        """Right-click on mod_compact_list — DFF material/texture mode or COL mode."""
        sender = self.sender()
        source_list = self.mod_compact_list if sender is self.mod_compact_list else self.collision_list
        item = source_list.itemAt(position)
        row = source_list.row(item) if item else -1

        # - DFF mode: show texture/material menu
        if getattr(self, '_dff_adapters', None):
            self._show_dff_material_context_menu(position, source_list, row)
            return

        # - COL mode: existing collision menu
        col_file = getattr(self, 'current_col_file', None)
        models = getattr(col_file, 'models', []) if col_file else []
        model  = models[row] if 0 <= row < len(models) else None
        menu   = QMenu(self)

        view_menu = menu.addMenu("Thumbnail View")
        axes = [("Top  (XY — Z up)", 0, 0), ("Front (XZ — Y fwd)", 0, 90),
                ("Side  (YZ — X right)", 90, 0), ("Isometric", 45, 35),
                ("Bottom", 0, 180), ("Back", 180, 90)]
        for label, yaw, pitch in axes:
            is_current = (abs(self._thumb_yaw - yaw) < 0.5 and
                          abs(self._thumb_pitch - pitch) < 0.5)
            act = view_menu.addAction(("✓ " if is_current else "    ") + label)
            act.triggered.connect(
                lambda _=False, y=yaw, p=pitch, l=label:
                    self._set_thumbnail_view(y, p, l))

        if model is not None:
            menu.addSeparator()
            menu.addAction("Show Details").triggered.connect(
                lambda: self._show_model_details(model, row))
            menu.addAction("Copy Info to Clipboard").triggered.connect(
                lambda: self._copy_model_info(model, row))
            menu.addSeparator()
            menu.addAction("Rename Model…").triggered.connect(
                lambda: self._rename_col_model(model, row))
            menu.addSeparator()
            menu.addAction("Export Model as COL…").triggered.connect(
                lambda: self._export_col_model(model, row))
            menu.addAction("Replace with COL file…").triggered.connect(
                lambda: self._import_replace_col_model(row))
            menu.addSeparator()
            is_pinned = self._is_model_pinned(row)
            menu.addAction(
                "📌 Unpin" if is_pinned else "📌 Pin").triggered.connect(
                    self._toggle_pin_selected)

        menu.addSeparator()
        menu.addAction("Select All  [Ctrl+A]",     self._select_all_models)
        menu.addAction("Invert Selection  [Ctrl+I]", self._invert_selection)
        menu.addAction("Sort…",                     self._show_sort_menu)
        menu.addSeparator()
        ide_menu = menu.addMenu("IDE Operations")
        ide_menu.addAction("Import matched by IDE…",      self._import_via_ide)
        ide_menu.addAction("Export matched by IDE…",      self._export_via_ide)
        ide_menu.addAction("Remove unreferenced by IDE…", self._remove_via_ide)
        menu.exec(source_list.mapToGlobal(position))

    def _show_dff_material_context_menu(self, position, source_list, row): #vers 1
        """DFF mode right-click — texture and material channel operations."""
        menu = QMenu(self)
        adapter  = (self._dff_adapters[row]
                    if self._dff_adapters and 0 <= row < len(self._dff_adapters)
                    else None)
        model    = getattr(self, '_current_dff_model', None)
        geom     = adapter._geometry if adapter else None

        # - Geometry info header
        if adapter:
            info = menu.addAction(
                f"Geometry [{row}]: {adapter.vertex_count}v  "
                f"{adapter.face_count}f  {len(geom.materials) if geom else 0} materials")
            info.setEnabled(False)
            menu.addSeparator()

        # - Texture channels
        tex_menu = menu.addMenu("Texture Channels")
        if geom and geom.materials:
            for mi, mat in enumerate(geom.materials):
                tname = getattr(mat, 'texture_name', '') or '(no texture)'
                in_cache = tname.lower() in getattr(
                    self.preview_widget, '_tex_cache', {})
                status = '✓' if in_cache else '✗'
                mat_act = tex_menu.addAction(
                    f"{status}  Mat {mi}: {tname}")
                mat_act.setEnabled(False)
            tex_menu.addSeparator()
        load_txd_act = tex_menu.addAction("Load TXD for this geometry…")
        load_txd_act.triggered.connect(self._load_txd_into_workshop)

        auto_act = tex_menu.addAction("Auto-find TXD from open IMGs…")
        auto_act.triggered.connect(lambda: self._auto_load_txd_from_imgs(
            getattr(self, '_ide_txd_name', '') or
            (geom.materials[0].texture_name if geom and geom.materials else '')))

        # - Render mode
        menu.addSeparator()
        render_menu = menu.addMenu("Render Mode")
        pw = getattr(self, 'preview_widget', None)
        cur_style = getattr(pw, '_mode', 'solid') if pw else 'solid'
        for style, label in [('wireframe', 'Wireframe'),
                              ('solid',    'Solid'),
                              ('textured', 'Textured')]:
            tick = '✓ ' if cur_style == style else '    '
            act = render_menu.addAction(tick + label)
            if pw:
                act.triggered.connect(
                    lambda _=False, s=style, p=pw: p.set_render_mode(s))

        # - UV info
        if geom:
            menu.addSeparator()
            uv_layers = getattr(geom, 'uv_layers', [])
            uv_info = menu.addAction(
                f"UV layers: {len(uv_layers)}  "
                f"({'has UVs' if uv_layers else 'no UVs, textured mode will not work'})")
            uv_info.setEnabled(False)

        # - Export
        menu.addSeparator()
        if model:
            menu.addAction("Export DFF as OBJ / MTL…").triggered.connect(
                self._export_dff_obj)

        menu.exec(source_list.mapToGlobal(position))


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
            out.models = [model]
            if hasattr(out, 'save'):
                if not out.save(file_path):
                    QMessageBox.warning(self, "Export Failed",
                        "Could not save COL model — save() returned False.")
                    return
            else:
                # Fallback: write the raw bytes of the model
                raw = getattr(model, '_raw_bytes', None)
                if raw:
                    with open(file_path, 'wb') as f:
                        f.write(raw)
                else:
                    QMessageBox.warning(self, "Export Failed",
                        "No serialisation method available for this model.")
                    return

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


    def _show_model_details(self, model, index): #vers 1
        """Show detailed model information dialog"""
        from PyQt6.QtWidgets import QDialog, QTextEdit, QVBoxLayout, QPushButton

        dialog = QDialog(self)
        dialog.setWindowTitle(f"Model Details - {model.name}")
        dialog.setMinimumSize(500, 400)

        layout = QVBoxLayout(dialog)

        # Create detailed info text
        info_text = f"""Model: {model.name}
    Index: {index}
    Version: 1{model.version.name if hasattr(model.version, 'name') else model.version}

    Bounding Box:
    Center: ({model.bounding_box.center.x:.3f}, {model.bounding_box.center.y:.3f}, {model.bounding_box.center.z:.3f})
    Min: ({model.bounding_box.min.x:.3f}, {model.bounding_box.min.y:.3f}, {model.bounding_box.min.z:.3f})
    Max: ({model.bounding_box.max.x:.3f}, {model.bounding_box.max.y:.3f}, {model.bounding_box.max.z:.3f})
    Radius: {model.bounding_box.radius:.3f}

    Collision Data:
    Spheres: {len(model.spheres)}
    Boxes: {len(model.boxes)}
    Vertices: {len(model.vertices)}
    Faces: {len(model.faces)}

    """

        # Add first 3 vertices if available
        if len(model.vertices) > 0:
            info_text += "\nVertices:\n"
            for i in range(min(30000, len(model.vertices))):
                v = model.vertices[i]
                if hasattr(v, 'position'):
                    info_text += f"  [{i}] ({v.position.x:.3f}, {v.position.y:.3f}, {v.position.z:.3f})\n"
                else:
                    info_text += f"  [{i}] ({v.x:.3f}, {v.y:.3f}, {v.z:.3f})\n"

        # Add material info from faces
        if len(model.faces) > 0:
            materials = set()
            for face in model.faces:
                if hasattr(face, 'material'):
                    mat_id = face.material.material_id if hasattr(face.material, 'material_id') else face.material
                    materials.add(mat_id)
            info_text += f"\nUnique Materials: {len(materials)}\n"
            info_text += f"Material IDs: {sorted(materials)}\n"

        text_edit = QTextEdit()
        text_edit.setPlainText(info_text)
        text_edit.setReadOnly(True)
        layout.addWidget(text_edit)

        # Copy button
        copy_btn = QPushButton("Copy to Clipboard")
        copy_btn.clicked.connect(lambda: self._copy_text_to_clipboard(info_text))
        layout.addWidget(copy_btn)

        # Close button
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.accept)
        layout.addWidget(close_btn)

        dialog.exec()


    def _copy_model_info(self, model, index): #vers 1
        """Copy model info to clipboard"""
        info = f"{model.name} | S:{len(model.spheres)} B:{len(model.boxes)} V:{len(model.vertices)} F:{len(model.faces)}"
        self._copy_text_to_clipboard(info)
        if hasattr(self, 'status_bar'):
            self.status_bar.showMessage("Model info copied to clipboard", 2000)


    def _copy_text_to_clipboard(self, text): #vers 1
        """Copy text to system clipboard"""
        from PyQt6.QtWidgets import QApplication
        clipboard = QApplication.clipboard()
        clipboard.setText(text)


    def _populate_collision_list(self): #vers 7
        """Populate [T] detail table — 8 columns, icon badges on counts > 0."""
        try:
            self.collision_list.setRowCount(0)
            if not self.current_col_file or not hasattr(self.current_col_file, 'models'):
                return

            # Build icon pixmaps once (16px, themed colour)
            icon_color = self._get_icon_color()
            from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVG
            from PyQt6.QtGui import QPixmap

            def _px(icon_fn, color=None):  #vers 1
                """Get 14px QPixmap from an SVG icon factory method."""
                try:
                    ico = icon_fn(size=14, color=color or icon_color)
                    return ico.pixmap(14, 14)
                except Exception:
                    return QPixmap()

            sphere_px = _px(_SVG.sphere_icon, '#50c8e0')   # cyan
            box_px    = _px(_SVG.box_icon,    '#dcb432')   # yellow
            face_px   = _px(_SVG.mesh_icon,   '#6496dc')   # blue
            # No dedicated vert icon — draw a tiny dot pixmap inline
            vert_px   = QPixmap(14, 14)
            from PyQt6.QtGui import QPainter, QColor, QBrush
            from PyQt6.QtCore import QRectF
            vert_px.fill(QColor(0, 0, 0, 0))
            vp = QPainter(vert_px)
            vp.setRenderHint(QPainter.RenderHint.Antialiasing)
            vp.setBrush(QBrush(QColor(100, 200, 120)))
            vp.setPen(QColor(100, 200, 120))
            for ox, oy in [(2,2),(8,2),(5,9)]:
                vp.drawEllipse(QRectF(ox, oy, 4, 4))
            vp.end()

            icon_map = {4: sphere_px, 5: box_px, 6: vert_px, 7: face_px}

            models = self.current_col_file.models
            self.collision_list.setUpdatesEnabled(False)

            for i, model in enumerate(models):
                name     = getattr(model, 'name', '') or f'model_{i}'
                version  = getattr(model, 'version', None)
                ver_str  = version.name if hasattr(version,'name') else str(version) if version else '?'
                ver_short = ver_str.replace('COL_','COL').replace('COLVersion.','')
                # Type = fourcc string, Version = game target label
                header   = getattr(model, 'header', None)
                fourcc   = getattr(header, 'fourcc', b'') if header else b''
                try:    type_str = fourcc.decode('ascii').rstrip('\x00')
                except Exception: type_str = str(fourcc)
                ver_label = {'COL1':'GTA III/VC','COL2':'SA PS2',
                             'COL3':'SA PC/Xbox','COL4':'SA (unused)'
                             }.get(ver_short, ver_short)
                spheres  = len(getattr(model, 'spheres',  []))
                boxes    = len(getattr(model, 'boxes',    []))
                vertices = len(getattr(model, 'vertices', []))
                faces    = len(getattr(model, 'faces',    []))
                bounds   = getattr(model, 'bounds', None)
                radius   = getattr(bounds, 'radius', 0.0) if bounds else 0.0

                row = self.collision_list.rowCount()
                self.collision_list.insertRow(row)

                def _item(text, col=None, idx=None):  #vers 1
                    it = QTableWidgetItem(str(text))
                    it.setFlags(it.flags() & ~Qt.ItemFlag.ItemIsEditable)
                    it.setTextAlignment(Qt.AlignmentFlag.AlignCenter |
                                        Qt.AlignmentFlag.AlignVCenter)
                    if idx is not None:
                        it.setData(Qt.ItemDataRole.UserRole, idx)
                    # Add icon if count > 0 and we have a pixmap for this col
                    if col in icon_map and isinstance(text, int) and text > 0:
                        it.setIcon(QIcon(icon_map[col]))
                    return it

                from PyQt6.QtGui import QIcon

                name_it = QTableWidgetItem(name)
                name_it.setFlags(name_it.flags() & ~Qt.ItemFlag.ItemIsEditable)
                name_it.setTextAlignment(Qt.AlignmentFlag.AlignLeft |
                                         Qt.AlignmentFlag.AlignVCenter)
                name_it.setData(Qt.ItemDataRole.UserRole, i)

                self.collision_list.setItem(row, 0, name_it)
                self.collision_list.setItem(row, 1, _item(type_str))
                self.collision_list.setItem(row, 2, _item(ver_label))
                self.collision_list.setItem(row, 3, _item(f"{radius:.2f}"))
                self.collision_list.setItem(row, 4, _item(spheres,  col=4))
                self.collision_list.setItem(row, 5, _item(boxes,    col=5))
                self.collision_list.setItem(row, 6, _item(vertices, col=6))
                self.collision_list.setItem(row, 7, _item(faces,    col=7))
                self.collision_list.setRowHeight(row, 22)

            # Column widths
            self.collision_list.setIconSize(QSize(14, 14))
            hdr = self.collision_list.horizontalHeader()
            hdr.resizeSection(0, 160)
            for c in range(1, 8):
                hdr.resizeSection(c, 68)
            hdr.setStretchLastSection(True)

            self.collision_list.setUpdatesEnabled(True)
            self.collision_list.viewport().update()

        except Exception as e:
            import traceback; traceback.print_exc()
            print(f"Error populating collision table: {str(e)}")
