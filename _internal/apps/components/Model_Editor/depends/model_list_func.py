#this belongs in apps/components/Model_Editor/depends/model_list_func.py - Version: 5
# X-Seti - October 12 2026 - IMG Factory 1.6 - Model Workshop model list

"""
Model list, frame tree, selection, sorting, thumbnails and details.
"""

##class _DFFGeometryAdapter: -
# __init__
# face_count
# materials
# vertex_count
#
##class _ModelListDelegate: -
# paint
# sizeHint
#
##class ModelListMixin: -
# _close_open_model
# _copy_model_info
# _copy_text_to_clipboard
# _display_dff_model
# _draw_col_model
# _filter_model_list
# _focus_search
# _generate_collision_thumbnail
# _get_selected_model
# _invert_selection
# _is_model_pinned
# _mark_model_item
# _model_is_dirty
# _on_col_selected
# _on_collision_selected
# _on_compact_col_selected
# _on_dff_geom_selected
# _on_dff_geom_selected_tbl
# _on_frame_tree_clicked
# _on_model_list_menu
# _populate_collision_list
# _populate_compact_col_list
# _populate_dff_detail_table
# _populate_frame_tree
# _project_model_2d
# _regenerate_all_thumbnails
# _render_collision_preview
# _select_all_models
# _select_model_by_row
# _set_thumbnail_view
# _show_collision_context_menu
# _show_detailed_info
# _show_dff_geometry
# _show_dff_material_context_menu
# _show_model_details
# _show_model_search
# _show_sort_menu
# _sort_models
# _sort_models_desc
# _start_thumbnail_spin
# _stop_thumbnail_spin
# _tbl_item
# _tick_thumbnail_spin
# _toggle_col_view
# _toggle_pin_selected

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QFont, QIcon
from PyQt6.QtWidgets import QApplication, QDialog, QMenu, QMessageBox, QPushButton, QStyledItemDelegate, QTableWidgetItem, QVBoxLayout
from apps.methods.imgfactory_svg_icons import SVGIconFactory

class _DFFGeometryAdapter:
    """Adapts a DFF Geometry for use with DFFViewport.

    The viewport expects model.vertices (objects with .x .y .z) and
    model.faces (objects with .vertex_indices or .a .b .c).
    DFF Geometry has .vertices (Vector3) and .triangles (Triangle with .v1 .v2 .v3).
    Vertices are transformed to world space using the frame hierarchy.
    """

    class _FaceAdapter:
        """Wraps a DFF Triangle so the viewport can read it as a COL face."""
        __slots__ = ('vertex_indices', 'material', 'a', 'b', 'c')
        def __init__(self, tri):  #vers 1
            self.vertex_indices = (tri.v1, tri.v2, tri.v3)
            self.a = tri.v1
            self.b = tri.v2
            self.c = tri.v3
            self.material = tri.material_id



    def __init__(self, geometry, geometry_index: int = 0, dff_model=None, atomic=None): #vers 2
        self._geometry = geometry
        self.faces     = [self._FaceAdapter(t) for t in geometry.triangles]
        self.spheres   = []
        self.boxes     = []
        self.name      = f"geometry_{geometry_index}"

        # Use raw local-space vertices — world transform not needed for single-geom preview
        self.vertices = list(geometry.vertices)

    @property
    def vertex_count(self):   return len(self.vertices)  #vers 1
    @property
    def face_count(self):     return len(self.faces)  #vers 1

    @property
    def materials(self): #vers 2
        """Expose DFF geometry materials for use by the viewport renderer."""
        return getattr(self._geometry, 'materials', [])

    def __repr__(self): #vers 1
        return f"<_DFFGeometryAdapter {self.name} {self.vertex_count}v {self.face_count}f>"


class _ModelListDelegate(QStyledItemDelegate):
    """Word-wrapping delegate for the COL compact list Details column."""
    def paint(self, painter, option, index): #vers 1
        if index.column() != 1:
            super().paint(painter, option, index)
            return
        from PyQt6.QtWidgets import QStyle, QApplication
        from PyQt6.QtCore import Qt
        # Background
        QApplication.style().drawPrimitive(
            QStyle.PrimitiveElement.PE_PanelItemViewItem, option, painter)
        text = index.data(Qt.ItemDataRole.DisplayRole) or ''
        painter.save()
        painter.setClipRect(option.rect)
        r = option.rect.adjusted(4, 4, -4, -4)
        painter.setPen(option.palette.text().color())
        painter.setFont(option.font)
        painter.drawText(r, Qt.TextFlag.TextWordWrap | Qt.AlignmentFlag.AlignTop, text)
        painter.restore()


    def sizeHint(self, option, index): #vers 1
        if index.column() != 1:
            return super().sizeHint(option, index)
        from PyQt6.QtCore import Qt, QSize
        text = index.data(Qt.ItemDataRole.DisplayRole) or ''
        fm = option.fontMetrics
        # Measure the text in a column ~160px wide
        w = 160
        r = fm.boundingRect(0, 0, w, 9999,
            Qt.TextFlag.TextWordWrap | Qt.AlignmentFlag.AlignTop, text)
        return QSize(w, max(72, r.height() + 12))


class ModelListMixin: #vers 1
    """Model list, frame tree, selection, sorting, thumbnails and details."""


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



    def _focus_search(self, *a, **kw): pass  #vers 1
    def _show_detailed_info(self, *a, **kw): pass  #vers 1


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

    def _show_dff_geometry(self, index: int): #vers 4
        """Push a DFF geometry into the GL viewport using full frame hierarchy."""
        adapters = getattr(self, '_dff_adapters', [])\
        
        model = getattr(self, '_current_dff_model', None)
        pw = getattr(self, 'preview_widget', None)
        if not pw: return

        sub = getattr(pw, '_select_mode', 'object') != 'object'
        if model and index < len(model.geometries) and sub:     # mesh editing: one geometry
            g = model.geometries[index]
            for vp in self._live_viewports():
                vp.load_geometry(g, g.materials)
                vp._selected_verts.clear(); vp._selected_edges.clear(); vp._selected_faces.clear()
        elif model and model.frames and model.atomics:
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

    def _on_compact_col_selected(self): #vers 4
        """Handle compact [=] list selection — routes to DFF or COL handler."""
        try:
            rows = self.mod_compact_list.selectionModel().selectedRows()
            if not rows:
                return
            row = rows[0].row()
            # DFF mode: _dff_adapters set when a DFF is loaded
            if getattr(self, '_dff_adapters', None):
                self._on_dff_geom_selected_tbl()
            else:
                self._select_model_by_row(row)
        except Exception as e:
            print("_on_compact_col_selected error: " + str(e))

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

    def _on_col_selected(self, item): #vers 6
        """Handle entry selection from left panel — routes by extension."""
        try:
            src = item.data(Qt.ItemDataRole.UserRole)
            if isinstance(src, dict) and src.get('kind') == 'file':
                self._open_path(src['path'])    # loose game file
                return
            entry = self._model_item_entry(item)
            if not entry:
                return
            name = entry.name.lower()
            opened = self.__dict__.setdefault('_open_models', {})
            if name.endswith('.dff') and name in opened:          # already open: swap back, no reparse
                st = opened[name]
                self._show_dff(st['path'], st['model'])
                return
            data = self._extract_col_from_img(entry)
            if not data:
                return
            if name.endswith('.dff'):
                import tempfile, os as _os
                tmp_dir = tempfile.mkdtemp()
                tmp_path = _os.path.join(tmp_dir, entry.name)
                with open(tmp_path, 'wb') as _f: _f.write(data)
                self.open_dff_file(tmp_path)
                if getattr(self, '_current_dff_path', None) == tmp_path:
                    opened[name] = {'path': tmp_path, 'model': self._current_dff_model,
                                    'orig': data, 'entry': entry,
                                    'img': getattr(self, '_entry_img', {}).get(id(entry)) or self.current_img}
                    self._mark_model_item(name, True)
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


    def _mark_model_item(self, key, opened): #vers 2
        """Open icon + tooltip on list items whose model is open."""
        lw = getattr(self, 'col_list_widget', None)
        if lw is None:
            return
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        for i in range(lw.count()):
            it = lw.item(i)
            if it is not None and it.text().lower() == key:
                it.setIcon(SVGIconFactory.open_icon(16) if opened else QIcon())
                it.setToolTip("Open - click to switch back, right-click to close" if opened else "")

    def _model_is_dirty(self, st): #vers 1
        """True if the model differs from the bytes it was opened with."""
        from apps.methods.dff_parser import DFFWriter
        try:
            return DFFWriter.write(st['model']) != st['orig']
        except Exception:
            return True

    def _on_model_list_menu(self, pos): #vers 2
        """Right-click on the model list: close models; game list exclusions."""
        lw = self.col_list_widget
        it = lw.itemAt(pos)
        opened = getattr(self, '_open_models', {})
        key = it.text().lower() if it else ''
        root = getattr(self, '_game_model_root', None)
        if root and getattr(self, '_panel_img_ids', 0) is None and key not in opened:
            from apps.methods.game_file_list import game_list_menu
            game_list_menu(self, lw, pos, self._ribbon_config_path(),
                           lambda rescan: self._load_game_model_list(root, rescan))
            return
        m = QMenu(self)
        a = m.addAction("Close model", lambda: self._close_open_model(key))
        a.setEnabled(key in opened)
        b = m.addAction(f"Close all open models ({len(opened)})",
                        lambda: [self._close_open_model(k) for k in list(opened)])
        b.setEnabled(bool(opened))
        m.exec(lw.viewport().mapToGlobal(pos))

    def _close_open_model(self, key): #vers 1
        """Close one open model; Save / Cancel / Close when it has unsaved edits."""
        opened = getattr(self, '_open_models', {})
        st = opened.get(key)
        if st is None:
            return True
        if self._model_is_dirty(st):
            r = QMessageBox.question(
                self, "Close model", f"{key} has unsaved changes.",
                QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Cancel
                | QMessageBox.StandardButton.Close)
            if r == QMessageBox.StandardButton.Cancel:
                return False
            if r == QMessageBox.StandardButton.Save:
                self._show_dff(st['path'], st['model'])
                self._save_file()
                if self._model_is_dirty(st):
                    return False
        opened.pop(key, None)
        self._mark_model_item(key, False)
        if getattr(self, '_current_dff_model', None) is st['model']:
            nxt = next(iter(opened.values()), None)
            if nxt is not None:
                self._show_dff(nxt['path'], nxt['model'])
            else:
                self._current_dff_model = None
                self._current_dff_path = None
                self._set_status(f"Closed {key}")
        return True





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

    def _show_dff_material_context_menu(self, position, source_list, row): #vers 2
        """DFF mode right-click — texture and material channel operations."""
        menu = QMenu(self)
        menu.addAction("Edit Model (mesh edit)…", self._edit_model)
        menu.addSeparator()
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
