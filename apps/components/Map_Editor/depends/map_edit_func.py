#this belongs in apps/components/Map_Editor/depends/map_edit_func.py - Version: 1
# X-Seti - October 14 2026 - IMG Factory 1.6 - Map Workshop instance editing

"""
Object browser, instance edit, undo, gizmo, camera bookmarks, gamepad for ModelWorkshop (Map).
"""

##class MapEditMixin: -
# _add_instance_of_model
# _add_instances
# _align_selected
# _apply_gamepad_setting
# _apply_ipl_editor_on_top_setting
# _apply_vc_layout_ipl_stems
# _camera_bookmarks_menu
# _center_on_instance
# _create_object_browser_dock
# _cycle_model_instance
# _delete_all_instances_of_model
# _delete_camera_bookmark
# _delete_instances
# _delete_selected_instances
# _distribute_selected
# _drop_selected_to_ground
# _duplicate_selected
# _goto_camera_bookmark
# _lod_parent_ipl
# _map_redo
# _map_undo
# _new_instance
# _on_ctrl_z_pressed
# _on_gizmo_moved
# _on_gizmo_rotated
# _on_instance_edited
# _on_instance_row_selected
# _on_model_dropped
# _on_object_browser_add_clicked
# _on_object_browser_column_resized
# _on_object_browser_column_visibility_toggled
# _on_object_browser_context_menu
# _on_object_browser_delete_clicked
# _on_object_browser_header_context_menu
# _on_object_browser_rename_clicked
# _on_object_browser_tab_changed
# _on_object_mode_changed
# _on_object_row_double_clicked
# _on_object_row_selected
# _on_object_search_changed
# _on_pad_snap_toggled
# _on_undo_ribbon_clicked
# _on_viewport_selection
# _populate_instance_list
# _populate_object_browser
# _push_map_undo
# _recompute_object_browser_model_width
# _refresh_after_instance_change
# _rename_camera_bookmark
# _rename_object
# _render_scale
# _save_camera_bookmark
# _select_instances
# _selected_instances
# _show_instance_edit_panel
# _target_ipl_for_new
# _toggle_instance_favourite
# _update_object_browser_action_buttons
# _world_key

import os

from PyQt6.QtCore import QPoint, QSize, QTimer, Qt
from PyQt6.QtWidgets import (
    QAbstractItemView, QApplication, QDockWidget, QHBoxLayout, QHeaderView, QInputDialog,
    QLabel, QMenu, QMessageBox, QVBoxLayout, QWidget)

from apps.components.Map_Editor.depends.map_widgets_func import (
    _InstanceEditPanel, _InstanceTableModel, _ObjectBrowserModel, _SizeAdaptiveStackedWidget)


class MapEditMixin: #vers 1
    """Object browser, instance edit, undo, gizmo, camera bookmarks, gamepad."""

    # --- Object Browser (ported from map_workshop_old_version.py) ---

    def _on_object_browser_column_resized(self, logical_index, old_size, new_size): #vers 1
        """Persist the user's column widths for Object Browser whenever
        they resize a column, so it doesn't reset to defaults on next
        open. Column 2 (Model) is captured too even though it stays
        Stretch-managed and isn't restored on the next load - harmless
        to store, simpler than special-casing it out."""
        view = getattr(self, '_object_browser_view', None)
        if view is None:
            return
        widths = [view.columnWidth(c) for c in range(view.model().columnCount())]
        self.map_settings.set('object_browser_column_widths', widths)
        self.map_settings.save()

    def _on_object_browser_header_context_menu(self, pos): #vers 1
        """Right-click the Object Browser header to hide/show individual
        columns."""
        view = getattr(self, '_object_browser_view', None)
        if view is None:
            return
        model = view.model()
        header = view.horizontalHeader()
        menu = QMenu(header)
        for col in range(model.columnCount()):
            label = model.headerData(col, Qt.Orientation.Horizontal) or f"Column {col}"
            act = menu.addAction(str(label))
            act.setCheckable(True)
            act.setChecked(not view.isColumnHidden(col))
            act.triggered.connect(
                lambda checked, c=col: self._on_object_browser_column_visibility_toggled(c, checked))
        menu.exec(header.mapToGlobal(pos))

    def _on_object_browser_column_visibility_toggled(self, col, visible): #vers 2
        view = getattr(self, '_object_browser_view', None)
        if view is None:
            return
        view.setColumnHidden(col, not visible)
        hidden = [c for c in range(view.model().columnCount()) if view.isColumnHidden(c)]
        self.map_settings.set('object_browser_hidden_columns', hidden)
        self.map_settings.save()
        self._recompute_object_browser_model_width()

    def _recompute_object_browser_model_width(self): #vers 1
        """Explicitly set the Model column."""
        view = getattr(self, '_object_browser_view', None)
        if view is None:
            return
        model = view.model()
        if model is None:
            return
        other_width = sum(view.columnWidth(c) for c in range(model.columnCount())
                          if c != 2 and not view.isColumnHidden(c))
        available = view.viewport().width()
        model_width = max(80, available - other_width)
        view.setColumnWidth(2, model_width)

    def _create_object_browser_dock(self): #vers 2
        """Object Browser dock - Add/Delete/Rename icon row, search +
        filter (All/Most Used/Favourites/Generic) over the loaded
        object catalog. Double-click a row to toggle its favourite
        status (persisted to map_settings)."""
        from PyQt6.QtWidgets import QTableView, QLineEdit, QPushButton, QButtonGroup, QToolButton
        from apps.methods.imgfactory_svg_icons import get_add_icon, get_trash_icon, get_rename_icon

        OBJECT_BROWSER_ICON_SIZE = 18
        OBJECT_BROWSER_BUTTON_W = 18
        OBJECT_BROWSER_BUTTON_H = 18
        panel = QWidget()

        main_layout = QHBoxLayout(panel)
        main_layout.setContentsMargins(2, 2, 2, 2)

        left_panel = QWidget()
        left_layout = QVBoxLayout(left_panel)
        left_layout.setContentsMargins(2, 2, 2, 2)
        main_layout.addWidget(left_panel)

        icon_color = self._get_icon_color()

        top_row_widget = QWidget()
        top_row = QHBoxLayout(top_row_widget)
        top_row.setContentsMargins(0, 0, 0, 0)

        group = QButtonGroup(panel)
        group.setExclusive(True)
        self._object_mode_buttons = {}

        # IMG, DAT, IDE, IPL, then All/Most Used/Favourites/Generic.
        tab_colors = {'img': '#c060e0', 'dat': '#e0a030', 'ide': '#4090e0', 'ipl': '#40b060'}
        tab_labels = {'img': "IMG", 'dat': "DAT", 'ide': "IDE", 'ipl': "IPL"}
        self._object_browser_tab_buttons = {}
        for tab_key in ('img', 'dat', 'ide', 'ipl'):
            btn = QToolButton()
            btn.setText(tab_labels[tab_key])
            btn.setStyleSheet(f"QToolButton {{ color: {tab_colors[tab_key]}; font-weight: bold; }}")
            btn.setToolTip(f"{tab_labels[tab_key]} tab")
            btn.setCheckable(True)
            btn.setMaximumWidth(28)
            btn.setFixedHeight(OBJECT_BROWSER_BUTTON_H)
            btn.clicked.connect(lambda checked, k=tab_key: self._on_object_browser_tab_changed(k))
            group.addButton(btn)
            top_row.addWidget(btn)
            self._object_browser_tab_buttons[tab_key] = btn

        mode_icon_shapes = {'all': 'list_all', 'most_used': 'bars_most_used',
                            'favourites': 'star_filled', 'generic': 'box_generic'}
        mode_labels = {'all': "All", 'most_used': "Most Used",
                      'favourites': "Favourites", 'generic': "Generic"}
        self._object_mode_button_text_widths = {}
        for mode in ('all', 'most_used', 'favourites', 'generic'):
            icon = self._render_variant_icon(mode_icon_shapes[mode], None,
                                             OBJECT_BROWSER_ICON_SIZE, icon_color,
                                             has_menu=False)
            btn = QToolButton()
            btn.setIcon(icon)
            btn.setText(mode_labels[mode])
            btn.setToolTip(mode_labels[mode])
            btn.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
            btn.setCheckable(True)
            btn.setChecked(mode == 'all')
            btn.setFixedHeight(OBJECT_BROWSER_BUTTON_H)
            btn.setMinimumWidth(OBJECT_BROWSER_ICON_SIZE + 8)   # icon-only size floor - without
                                                                # this the icon+text minimumSizeHint
                                                                # (~80-100px per button) forces the
                                                                # whole dock's minimum width way up,
                                                                # blocking narrower resizing entirely
            btn.clicked.connect(lambda checked, m=mode: self._on_object_mode_changed(m))
            group.addButton(btn)
            top_row.addWidget(btn)
            self._object_mode_buttons[mode] = btn
            # Estimated width needed in icon+text mode, computed directly

            from PyQt6.QtGui import QFontMetrics
            text_w = QFontMetrics(btn.font()).horizontalAdvance(mode_labels[mode])
            self._object_mode_button_text_widths[mode] = OBJECT_BROWSER_ICON_SIZE + text_w + 16

        # Add/Delete/Rename icon row - previously these actions only

        action_row_widget = QWidget()
        action_row = QHBoxLayout(action_row_widget)
        action_row.setContentsMargins(0, 0, 0, 0)
        self._ob_add_btn = QPushButton(get_add_icon(OBJECT_BROWSER_ICON_SIZE, icon_color), "")
        self._ob_add_btn.setToolTip("Add Instance Here - place another copy of the\n"
                                    "selected model at the origin")
        self._ob_add_btn.clicked.connect(self._on_object_browser_add_clicked)
        self._ob_del_btn = QPushButton(get_trash_icon(18, icon_color), "")
        self._ob_del_btn.setToolTip("Delete All Instances of the selected model")
        self._ob_del_btn.clicked.connect(self._on_object_browser_delete_clicked)
        self._ob_rename_btn = QPushButton(get_rename_icon(18, icon_color), "")
        self._ob_rename_btn.setToolTip("Rename the selected object")
        self._ob_rename_btn.clicked.connect(self._on_object_browser_rename_clicked)
        for btn in (self._ob_add_btn, self._ob_del_btn, self._ob_rename_btn):
            btn.setFixedSize(OBJECT_BROWSER_BUTTON_W, OBJECT_BROWSER_BUTTON_H)
            btn.setEnabled(False)
            action_row.addWidget(btn)

        ipl_settings_btn = QToolButton()
        try:
            ipl_settings_btn.setIcon(self.icon_factory.settings_icon(color=icon_color))
        except Exception:
            pass
        ipl_settings_btn.setIconSize(QSize(16, 16))
        ipl_settings_btn.setFixedSize(OBJECT_BROWSER_BUTTON_W, OBJECT_BROWSER_BUTTON_H)
        ipl_settings_btn.setToolTip("Open App Settings (includes Map Workshop's own settings)")
        ipl_settings_btn.clicked.connect(self._on_ipl_controls_settings_clicked)
        ipl_settings_btn.setVisible(self.is_docked and not self.standalone_mode)
        action_row.addWidget(ipl_settings_btn)
        self._ipl_controls_settings_btn = ipl_settings_btn

        # Only shown when docked - in standalone mode the titlebar's own
        # Add/Del/Rename icons cover this, so showing both would be redundant
        action_row_widget.setVisible(not self.standalone_mode)
        self._ob_action_row_widget = action_row_widget
        top_row.addWidget(action_row_widget)

        self._ob_top_row_widget = top_row_widget
        top_row_widget.installEventFilter(self)
        QTimer.singleShot(0, self._update_mode_button_style)
        left_layout.addWidget(top_row_widget)

        view = QTableView()
        from PyQt6.QtGui import QFontMetrics
        OBJECT_BROWSER_ROW_HEIGHT = QFontMetrics(view.font()).height() + 2   # text height + 2px
        view.verticalHeader().setMinimumSectionSize(OBJECT_BROWSER_ROW_HEIGHT)
        view.verticalHeader().setDefaultSectionSize(
            OBJECT_BROWSER_ROW_HEIGHT
        )
        header = view.horizontalHeader()
        header.setFixedHeight(QFontMetrics(header.font()).height() + 2)
        header.setMinimumSectionSize(16)   # Qt's own default was silently clamping the narrow ★/ID widths upward
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.Interactive)
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.Interactive)
        # Model (col 2) used to be QHeaderView.ResizeMode.Stretch, but
        # that didn't reliably recalculate after hiding/showing other
        # columns (confirmed by direct measurement, not just assumed) -
        # explicit, directly-computed width instead (see
        # _recompute_object_browser_model_width), recalculated on
        # setup, column visibility changes, and view resize.
        header.setSectionResizeMode(2, QHeaderView.ResizeMode.Interactive)
        header.sectionResized.connect(self._on_object_browser_column_resized)
        header.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        header.customContextMenuRequested.connect(self._on_object_browser_header_context_menu)
        view.verticalHeader().setVisible(False)
        view.setEditTriggers(QTableView.EditTrigger.NoEditTriggers)
        view.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        view.setDragEnabled(True)                 # drag a model into the viewport to place it
        view.setDragDropMode(QAbstractItemView.DragDropMode.DragOnly)
        view.doubleClicked.connect(self._on_object_row_double_clicked)
        view.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        view.customContextMenuRequested.connect(self._on_object_browser_context_menu)
        view.installEventFilter(self)
        model = _ObjectBrowserModel()
        view.setModel(model)
        # Narrow defaults for ★/ID
        view.setColumnWidth(0, 20)
        view.setColumnWidth(1, 45)
        saved_widths = self.map_settings.get('object_browser_column_widths') or []
        for col, width in enumerate(saved_widths):
            if col < model.columnCount() and col != 2:   # 2 (Model) is computed, not restored
                header.setSectionResizeMode(col, QHeaderView.ResizeMode.Interactive)
                view.setColumnWidth(col, width)
        hidden_cols = self.map_settings.get('object_browser_hidden_columns') or []
        for col in hidden_cols:
            if col < model.columnCount():
                view.setColumnHidden(col, True)
        QTimer.singleShot(0, self._recompute_object_browser_model_width)
        sel_model = view.selectionModel()
        if sel_model is not None:
            sel_model.currentRowChanged.connect(
                lambda cur, prev: self._on_object_row_selected(cur.row()))
        self._object_browser_view = view
        self._object_browser_model = model

        # Stacked content area - page 0 is the Object Browser table
        # itself (All/Most Used/Favourites/Generic modes all share this
        # one page, just filtering its rows); pages 1-4 are the tabs
        # merged in from the former standalone Editing Panel dock.
        content_stack = _SizeAdaptiveStackedWidget()
        content_stack.addWidget(view)
        content_stack.addWidget(self._create_ide_tab())
        content_stack.addWidget(self._create_ipl_tab())
        content_stack.addWidget(self._create_dat_tab())
        content_stack.addWidget(self._create_img_tab())
        left_layout.addWidget(content_stack)
        self._object_browser_content_stack = content_stack

        dock = QDockWidget("Object Browser", self)
        dock.setObjectName("Object Browser")
        dock.setWidget(panel)
        dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                        QDockWidget.DockWidgetFeature.DockWidgetFloatable |
                        QDockWidget.DockWidgetFeature.DockWidgetClosable)

        title_bar = QWidget()
        title_lay = QHBoxLayout(title_bar)
        title_lay.setContentsMargins(2, 2, 2, 2)
        title_label = QLabel("Object Browser")
        title_lay.addWidget(title_label)
        search = QLineEdit()
        search.setPlaceholderText("Search objects…")
        search.setMinimumWidth(40)
        search.textChanged.connect(self._on_object_search_changed)
        title_lay.addWidget(search, 1)
        self._object_search_edit = search
        dock.setTitleBarWidget(title_bar)

        self._object_browser_dock = dock
        return dock

    def _populate_object_browser(self, loader): #vers 2
        """Fill the Object Browser from a completed load - instance
        counts per model (for Most Used) computed once here, not
        recomputed on every filter change."""
        model = getattr(self, '_object_browser_model', None)
        if model is None:
            return
        model.set_model_cache(getattr(self, '_model_cache', None))
        from collections import Counter
        counts = getattr(self, '_object_instance_counts', None)
        up_to = getattr(self, '_object_instance_count_up_to', 0)
        total = len(loader.instances)
        if counts is None or up_to > total:
            counts = Counter(inst.model_id for inst in loader.instances)
            up_to = total
        elif up_to < total:
            counts.update(inst.model_id for inst in loader.instances[up_to:])
            up_to = total
        self._object_instance_counts = counts
        self._object_instance_count_up_to = up_to
        favourites = self.map_settings.get('favourite_objects') or []
        model.set_objects(list(loader.objects.values()), counts, favourites)

    def _on_object_search_changed(self, text): #vers 1
        model = getattr(self, '_object_browser_model', None)
        if model is not None:
            model.set_search(text)

    def _on_object_mode_changed(self, mode): #vers 2
        model = getattr(self, '_object_browser_model', None)
        if model is not None:
            model.set_mode(mode)
        stack = getattr(self, '_object_browser_content_stack', None)
        if stack is not None:
            stack.setCurrentIndex(0)   # table page - IMG/DAT/IDE/IPL buttons show pages 1-4

    def _on_object_browser_tab_changed(self, tab_key): #vers 3
        """IMG/DAT/IDE/IPL button clicked - switches the shared content
        stack to that tab's page instead of the Object Browser table."""
        stack = getattr(self, '_object_browser_content_stack', None)
        if stack is None:
            return
        page_index = {'ide': 1, 'ipl': 2, 'dat': 3, 'img': 4}.get(tab_key)
        if page_index is not None:
            stack.setCurrentIndex(page_index)
        row_widget = getattr(self, '_object_browser_tab_rows', {}).get(tab_key)
        if row_widget is not None:
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(0, lambda w=row_widget: self._update_button_row_collapse(w))

        dock = getattr(self, '_ipl_inst_file_dock', None)
        title_lbl = getattr(dock, '_title_label', None) if dock is not None else None
        if tab_key == 'ide':
            self._ipl_inst_file_mode = 'ide'
            if title_lbl is not None:
                title_lbl.setText("IDE Objects")
            ide_table = getattr(self, '_ide_tab_table', None)
            ide_row = ide_table.currentRow() if ide_table is not None else -1
            if ide_row >= 0:
                self._refresh_ide_objects_panel(ide_row)
            else:
                table = getattr(self, '_ipl_inst_file_table', None)
                if table is not None:
                    table.setRowCount(0)
        else:
            self._ipl_inst_file_mode = 'ipl'
            if title_lbl is not None:
                title_lbl.setText("IPL File Display")
            table = getattr(self, '_ipl_inst_file_table', None)
            if table is not None and table.columnCount() != 13:
                table.setColumnCount(13)
                table.setHorizontalHeaderLabels([
                    "ID", "Model", "Int",
                    "Pos X", "Pos Y", "Pos Z",
                    "Scale X", "Scale Y", "Scale Z",
                    "Rot X", "Rot Y", "Rot Z", "Rot W"])
            self._refresh_ipl_inst_file_panel()

    def _on_object_row_selected(self, row): #vers 2
        """Selecting a model row in the (now merged) Object Browser
        centres the camera + shows the edit panel on that model's
        FIRST placement, if it has any - with Prev/Next cycling stored
        for models with multiple instances. Models with zero instances
        (defined in the IDE but never placed) show identity/IDE/2DFX/
        TOBJ info in the panel without any placement-specific data,
        since there's no instance to show it for. Also enables/disables
        the Add/Delete/Rename icon row based on selection state."""
        if row < 0:
            self._selected_object_model_id = None
            self._update_object_browser_action_buttons()
            return
        model = self._object_browser_model
        obj = model.object_at(row)
        if obj is None:
            self._selected_object_model_id = None
            self._update_object_browser_action_buttons()
            return
        self._selected_object_model_id = obj.model_id
        self._update_object_browser_action_buttons()
        loader = getattr(self, '_world_loader', None)
        instances = loader.get_instances_for_model(obj.model_id) if loader else []
        if not instances:
            self._current_model_instances = []
            self._current_instance_index = 0
            return
        self._current_model_instances = instances
        self._current_instance_index = 0
        self._center_on_instance(instances[0], nav_info=(0, len(instances)))

    def _update_object_browser_action_buttons(self): #vers 1
        """Enable/disable the Add/Delete/Rename icon buttons to match
        the current selection - Add and Rename work regardless of
        instance count, Delete additionally needs at least one
        instance to remove."""
        mid = getattr(self, '_selected_object_model_id', None)
        has_selection = mid is not None
        self._ob_add_btn.setEnabled(has_selection)
        self._ob_rename_btn.setEnabled(has_selection)
        instance_count = 0
        if has_selection:
            model = self._object_browser_model
            instance_count = model._instance_counts.get(mid, 0)
        self._ob_del_btn.setEnabled(has_selection and instance_count > 0)

    def _on_object_browser_add_clicked(self): #vers 1
        mid = getattr(self, '_selected_object_model_id', None)
        if mid is not None:
            self._add_instance_of_model(mid)
            self._update_object_browser_action_buttons()

    def _on_object_browser_delete_clicked(self): #vers 1
        mid = getattr(self, '_selected_object_model_id', None)
        if mid is not None:
            self._delete_all_instances_of_model(mid)
            self._update_object_browser_action_buttons()

    def _on_object_browser_rename_clicked(self): #vers 1
        mid = getattr(self, '_selected_object_model_id', None)
        if mid is not None:
            self._rename_object(mid)

    def _on_object_browser_context_menu(self, pos): #vers 2
        """Right-click a row for Favourites, Rename, Add Instance, and
        Delete All Instances."""
        view = self._object_browser_view
        index = view.indexAt(pos)
        if not index.isValid():
            return
        model = self._object_browser_model
        obj = model.object_at(index.row())
        if obj is None:
            return

        menu = QMenu(view)
        is_fav = obj.model_id in model._favourites
        fav_act = menu.addAction("Remove from Favourites" if is_fav else "Add to Favourites")
        fav_act.triggered.connect(lambda checked=False, mid=obj.model_id:
                                  self._toggle_instance_favourite(mid))
        menu.addSeparator()
        rename_act = menu.addAction("Rename Object…")
        rename_act.triggered.connect(lambda checked=False, mid=obj.model_id:
                                     self._rename_object(mid))
        add_act = menu.addAction("Add Instance Here…")
        add_act.triggered.connect(lambda checked=False, mid=obj.model_id:
                                  self._add_instance_of_model(mid))
        instance_count = model._instance_counts.get(obj.model_id, 0)
        del_act = menu.addAction(f"Delete All Instances ({instance_count})")
        del_act.setEnabled(instance_count > 0)
        del_act.triggered.connect(lambda checked=False, mid=obj.model_id:
                                  self._delete_all_instances_of_model(mid))
        menu.exec(view.viewport().mapToGlobal(pos))

    def _rename_object(self, model_id): #vers 2
        """Rename an object's model_name - IN MEMORY ONLY for now
        (updates the IDEObject held by the current GTAWorldLoader and
        refreshes Object Browser's display). Does NOT yet write the
        new name back to the actual IDE file, or update GTA3/VC's
        text-format IPL lines that redundantly store the name (SA's
        format doesn't) - that needs real file-writing infrastructure,
        tracked separately.

        Undoable now (Aug 20 2026,  "Undo/redo for mapping
        changes") - was explicitly called out in TODO.md as an in-
        memory-only Object Browser action that isn't undoable yet."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        obj = loader.get_object(model_id)
        if obj is None:
            return
        new_name, ok = QInputDialog.getText(
            self, "Rename Object", "New name:", text=obj.model_name)
        if not ok or not new_name.strip():
            return
        new_name = new_name.strip()
        old_name = obj.model_name

        def _apply(name):
            obj.model_name = name
            for inst in getattr(self, '_all_instances', []):
                if inst.model_id == model_id:
                    inst.model_name = name
            self._object_browser_model._recompute()

        def _undo():
            _apply(old_name)

        def _redo():
            _apply(new_name)

        _apply(new_name)
        self._push_map_undo(_undo, _redo, f"Rename '{old_name}' to '{new_name}'")
        self._set_status(f"Renamed object {model_id} to '{new_name}' "
                         f"(in memory only - not yet written to disk)")

    def _on_object_row_double_clicked(self, index): #vers 1
        """Double-click a row to toggle its favourite status."""
        model = getattr(self, '_object_browser_model', None)
        if model is None:
            return
        obj = model.object_at(index.row())
        if obj is None:
            return
        new_favourites = model.toggle_favourite(obj.model_id)
        self.map_settings.set('favourite_objects', new_favourites)
        self.map_settings.save()

    # --- Instance List / Instance Edit Panel (ported from map_workshop_old_version.py) ---

    def _cycle_model_instance(self, direction): #vers 1
        """Prev (-1) / Next (+1) through the currently-selected model's
        placements, wrapping around at either end."""
        instances = getattr(self, '_current_model_instances', [])
        if not instances:
            return
        idx = (getattr(self, '_current_instance_index', 0) + direction) % len(instances)
        self._current_instance_index = idx
        self._center_on_instance(instances[idx], nav_info=(idx, len(instances)))

    def _add_instance_of_model(self, model_id): #vers 3
        """Add a new placement of an existing model at the view centre,
        on the ground, in the target IPL (selected IPL row, or asked).
        Saved with Ctrl+S like any other edit.

        Undoable now (Aug 20 2026,  "Undo/redo for mapping
        changes") - was explicitly called out in TODO.md as an in-
        memory-only Object Browser action that isn't undoable yet."""
        loader = getattr(self, '_world_loader', None)
        obj = loader.get_object(model_id) if loader else None
        if obj is None:
            return
        vp = getattr(self, 'preview_widget', None)
        cx, cy = (-vp._pan_x, -vp._pan_y) if vp is not None else (0.0, 0.0)
        gz = vp.ground_z_below(cx, cy, 5000.0) if vp is not None and hasattr(vp, 'ground_z_below') else None
        new_inst = self._new_instance(model_id, (cx, cy, gz if gz is not None else 0.0))
        if new_inst is None:
            return

        def _do_add():
            self._all_instances = getattr(self, '_all_instances', [])
            self._all_instances.append(new_inst)
            if loader is not None:
                loader.instances.append(new_inst)
            counts = getattr(self._object_browser_model, '_instance_counts', {})
            counts[model_id] = counts.get(model_id, 0) + 1
            self._object_browser_model._recompute()
            self._apply_ipl_visibility_filter(clear_display_lists=False)

        def _do_remove():
            self._all_instances = [i for i in getattr(self, '_all_instances', []) if i is not new_inst]
            if loader is not None:
                loader.instances[:] = [i for i in loader.instances if i is not new_inst]
            counts = getattr(self._object_browser_model, '_instance_counts', {})
            counts[model_id] = max(0, counts.get(model_id, 0) - 1)
            self._object_browser_model._recompute()
            self._apply_ipl_visibility_filter(clear_display_lists=False)

        _do_add()
        self._push_map_undo(_do_remove, _do_add, f"Add instance of '{obj.model_name}'")
        self._center_on_instance(new_inst)
        self._set_status(f"Added '{obj.model_name}' to {new_inst.source_ipl} at the view centre "
                         f"- Ctrl+S to save")

    def _delete_all_instances_of_model(self, model_id): #vers 2
        """Remove every placement of a model.

        Undoable now (Aug 20 2026,  "Undo/redo for mapping
        changes") - was explicitly called out in TODO.md as an in-
        memory-only Object Browser action that isn't undoable yet."""
        loader = getattr(self, '_world_loader', None)
        all_inst = getattr(self, '_all_instances', [])
        removed_instances = [i for i in all_inst if i.model_id == model_id]
        removed = len(removed_instances)
        if removed == 0:
            return
        confirm = QMessageBox.question(
            self, "Delete All Instances",
            f"Remove all {removed} placement(s) of this model from the "
            f"currently loaded world?\n\n"
            f"This only removes the placements - the object definition, "
            f"collision, and model/texture files are left untouched.\n\n"
            f"(In-memory only for now - not yet written back to the "
            f"actual IPL file(s) on disk.)",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if confirm != QMessageBox.StandardButton.Yes:
            return

        def _do_remove():
            self._all_instances = [i for i in getattr(self, '_all_instances', [])
                                   if i.model_id != model_id]
            if loader is not None:
                loader.instances[:] = [i for i in loader.instances if i.model_id != model_id]
            counts = getattr(self._object_browser_model, '_instance_counts', {})
            counts.pop(model_id, None)
            self._object_browser_model._recompute()
            self._apply_ipl_visibility_filter(clear_display_lists=False)

        def _do_restore():
            self._all_instances = getattr(self, '_all_instances', [])
            self._all_instances.extend(removed_instances)
            if loader is not None:
                loader.instances.extend(removed_instances)
            counts = getattr(self._object_browser_model, '_instance_counts', {})
            counts[model_id] = removed
            self._object_browser_model._recompute()
            self._apply_ipl_visibility_filter(clear_display_lists=False)

        _do_remove()
        self._push_map_undo(_do_restore, _do_remove, f"Delete {removed} instance(s)")
        self._set_status(f"Removed {removed} placement(s) "
                         f"(in memory only - not yet written to disk)")



    def _center_on_instance(self, inst, nav_info=None): #vers 4
        """Centre all three World View panes' cameras on an instance,
        show an XYZ gizmo at its position, and show/update its edit
        panel - the shared behaviour for both single- and double-
        clicking an Instance List row, and (since the Object Browser
        merge) selecting a model row with one or more placements.
        nav_info, if given, is (current_index, total_count) for
        Prev/Next cycling through a model's other placements.

        focus_from_above (Sep 5 2026,  "can we change the
        camera view, have a setting, view from 0, +200 so we dont
        view the model from the bottom, we see it from the top
        instead") - when on, also resets yaw/pitch/dist to a fixed
        from-above angle rather than leaving whatever angle the
        camera already happened to be at, which could be looking up
        from underneath the model."""
        from_above = self.map_settings.get('focus_from_above', False)
        focus_dist = self.map_settings.get('focus_from_above_dist', 200.0)
        for pane in getattr(self, '_world_panes', []):
            pane._pan_x = -inst.pos_x
            pane._pan_y = -inst.pos_y
            if from_above:
                pane._yaw = 0.0
                pane._pitch = 89.0
                pane._dist = focus_dist
            pane.set_gizmo_target(inst)
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_gizmo_target'):
            vp.set_gizmo_target(inst)
        self._show_instance_edit_panel(inst, nav_info)

    def _show_instance_edit_panel(self, inst, nav_info=None): #vers 5
        """Show the non-modal IPL Object Editor window for one instance.
        Plain top-level window: floating QDockWidget came up black on Wayland."""
        win = getattr(self, '_instance_edit_panel', None)
        if win is None:
            win = _InstanceEditPanel(self, self)
            win.setWindowFlags(Qt.WindowType.Window)
            win.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
            win.setAutoFillBackground(True)
            self._instance_edit_panel = win
            self._apply_ipl_editor_on_top_setting()
            win.resize(620, 400)
            top_level = self.window()
            win.move(top_level.mapToGlobal(top_level.rect().topLeft()) + QPoint(8, 8))
        win.show_for_instance(inst, getattr(self, '_world_loader', None), nav_info,
                              getattr(self, '_model_cache', None))
        win.show()
        win.raise_()
        win.activateWindow()

    def _apply_ipl_editor_on_top_setting(self): #vers 2
        """Apply ipl_editor_always_on_top to the IPL Object Editor window."""
        win = getattr(self, '_instance_edit_panel', None)
        if win is None:
            return
        on_top = self.map_settings.get('ipl_editor_always_on_top', False)
        flags = win.windowFlags()
        if bool(flags & Qt.WindowType.WindowStaysOnTopHint) == on_top:
            return
        was_visible = win.isVisible()
        win.setWindowFlag(Qt.WindowType.WindowStaysOnTopHint, on_top)
        if was_visible:
            win.show()

    def _apply_vc_layout_ipl_stems(self, loader): #vers 1
        """Populate loader.vc_layout_ipl_stems from the vc_layout_ipl_
        stems setting (Sep 5 2026,  "LC, MLL, VC are still
        in VC format... a loading toggle to adjust ipl loading
        patterns") - a comma-separated list of IPL filename stems
        (no extension, case-insensitive) to parse using VC's own
        field layout instead of the world's own default game. Empty
        setting means no change to normal loading behaviour."""
        raw = self.map_settings.get('vc_layout_ipl_stems', '') or ''
        stems = {s.strip().lower() for s in raw.split(',') if s.strip()}
        loader.vc_layout_ipl_stems = stems

    # -- object operations: select, duplicate, delete, rotate, ground, align, place (Sep 23 2026)
    def _selected_instances(self): #vers 1
        """Viewport multi-selection, else the Object Editor's instance."""
        vp = getattr(self, 'preview_widget', None)
        sel = vp.selected_instances() if vp is not None and hasattr(vp, 'selected_instances') else []
        if sel:
            return sel
        panel = getattr(self, '_instance_edit_panel', None)
        inst = getattr(panel, '_inst', None) if panel is not None else None
        return [inst] if inst is not None else []

    def _on_viewport_selection(self, insts, primary): #vers 1
        """Viewport selection changed: Object Editor follows the primary object."""
        if primary is not None:
            self._show_instance_edit_panel(primary)
        self._set_status(f"{len(insts)} object(s) selected" if insts else "Selection cleared")

    def _select_instances(self, insts): #vers 1
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_selection'):
            vp.set_selection(insts, insts[-1] if insts else None, notify=False)
        if insts:
            self._show_instance_edit_panel(insts[-1])

    def _lod_parent_ipl(self, ipl_name): #vers 1
        """IPL whose instance list a lod_index points into (SA streams -> parent text IPL)."""
        for parent, streams in getattr(self, '_ipl_names_with_binary_stream', {}).items():
            if any(en == ipl_name for _a, en in streams):
                return parent
        return ipl_name

    def _refresh_after_instance_change(self): #vers 1
        """Rebuild lists, counts and the viewport after instances were added/removed."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        self._all_instances = list(loader.instances)
        self._object_instance_counts = None
        self._populate_object_browser(loader)
        self._populate_instance_list(loader)
        self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=False)

    def _add_instances(self, insts, label): #vers 1
        """Append new instances (end of their IPL keeps SA LOD indices valid); undoable."""
        loader = getattr(self, '_world_loader', None)
        if loader is None or not insts:
            return

        def _add():
            loader.instances.extend(insts)
            self._refresh_after_instance_change()
            self._select_instances(list(insts))

        def _remove():
            ids = {id(i) for i in insts}
            loader.instances[:] = [i for i in loader.instances if id(i) not in ids]
            self._refresh_after_instance_change()
            self._select_instances([])

        _add()
        self._push_map_undo(_remove, _add, label)
        self._set_status(label)

    def _delete_instances(self, insts): #vers 1
        """Remove instances, re-pointing SA lod_index links that shift; undoable."""
        loader = getattr(self, '_world_loader', None)
        if loader is None or not insts:
            return
        dead = {id(i) for i in insts}
        affected = {self._lod_parent_ipl(i.source_ipl) for i in insts}
        remap = {}                                   # parent ipl -> {old idx: new idx}
        for p in affected:
            old = [i for i in loader.instances if i.source_ipl == p]
            m, n = {}, 0
            for k, i in enumerate(old):
                if id(i) in dead:
                    m[k] = -1
                else:
                    m[k] = n
                    n += 1
            remap[p] = m
        lod_changes = []
        for i in loader.instances:
            if id(i) in dead or i.lod_index < 0:
                continue
            p = self._lod_parent_ipl(i.source_ipl)
            if p in remap:
                new = remap[p].get(i.lod_index, -1)
                if new != i.lod_index:
                    lod_changes.append((i, i.lod_index, new))
        positions = [(k, i) for k, i in enumerate(loader.instances) if id(i) in dead]

        def _do():
            loader.instances[:] = [i for i in loader.instances if id(i) not in dead]
            for i, _o, n in lod_changes:
                i.lod_index = n
            self._refresh_after_instance_change()
            self._select_instances([])

        def _undo():
            for k, i in positions:
                loader.instances.insert(k, i)
            for i, o, _n in lod_changes:
                i.lod_index = o
            self._refresh_after_instance_change()
            self._select_instances([i for _k, i in positions])

        _do()
        self._push_map_undo(_undo, _do, f"Delete {len(insts)} object(s)")
        msg = f"Deleted {len(insts)} object(s)"
        if lod_changes:
            msg += f"; {len(lod_changes)} LOD link(s) re-pointed"
        self._set_status(msg)

    def _delete_selected_instances(self): #vers 1
        insts = self._selected_instances()
        if insts and QMessageBox.question(self, "Delete", f"Delete {len(insts)} object(s)?") \
                == QMessageBox.StandardButton.Yes:
            self._delete_instances(insts)

    def _duplicate_selected(self): #vers 1
        """Ctrl+D: clone the selected objects in place; the clones become the selection."""
        import dataclasses
        src = self._selected_instances()
        if not src:
            self._set_status("Duplicate: select an object first")
            return
        clones = [dataclasses.replace(i, line_no=0, raw_line="") for i in src]
        self._add_instances(clones, f"Duplicated {len(clones)} object(s)")

    def _on_gizmo_moved(self, insts, dx, dy, dz): #vers 2
        """Gizmo move finished: apply to every dragged instance, undoable."""
        olds = [(i, (i.pos_x, i.pos_y, i.pos_z)) for i in insts]

        def _apply(sign):
            for i, (x, y, z) in olds:
                i.pos_x, i.pos_y, i.pos_z = x + dx * sign, y + dy * sign, z + dz * sign
                self._on_instance_edited(i)
            panel = getattr(self, '_instance_edit_panel', None)
            if panel is not None and getattr(panel, '_inst', None) is not None:
                panel._refresh_position_spins()

        _apply(1)
        self._push_map_undo(lambda: _apply(0), lambda: _apply(1),
                            f"Move {len(insts)} object(s) ({dx:+.2f}, {dy:+.2f}, {dz:+.2f})")
        self._set_status(f"Moved {len(insts)} object(s) by ({dx:.2f}, {dy:.2f}, {dz:.2f})")

    def _on_gizmo_rotated(self, insts, axis, angle, pivot): #vers 1
        """Gizmo rotate finished: rotate orientations and positions about the pivot, undoable."""
        vp = self.preview_widget
        q = vp._quat_axis(axis, angle)
        olds = [(i, (i.pos_x, i.pos_y, i.pos_z), (i.rot_x, i.rot_y, i.rot_z, i.rot_w)) for i in insts]
        news = []
        for i, pos, rot in olds:
            eff = self._conjugate_rotation_for_game(*rot)
            ne = vp._quat_mul(q, eff)
            off = vp._quat_rotate(q, (pos[0] - pivot[0], pos[1] - pivot[1], pos[2] - pivot[2]))
            news.append((i, (pivot[0] + off[0], pivot[1] + off[1], pivot[2] + off[2]),
                         self._conjugate_rotation_for_game(*ne)))

        def _apply(state):
            for i, pos, rot in state:
                i.pos_x, i.pos_y, i.pos_z = pos
                i.rot_x, i.rot_y, i.rot_z, i.rot_w = rot
                self._on_instance_edited(i)
            panel = getattr(self, '_instance_edit_panel', None)
            if panel is not None and getattr(panel, '_inst', None) is not None:
                panel._refresh_position_spins()
                panel._refresh_rotation_spins()

        _apply(news)
        self._push_map_undo(lambda: _apply(olds), lambda: _apply(news),
                            f"Rotate {len(insts)} object(s) {angle:+.1f} deg about {axis.upper()}")
        self._set_status(f"Rotated {len(insts)} object(s) {angle:+.1f} deg about {axis.upper()}")

    def _drop_selected_to_ground(self): #vers 1
        """End key: set each selected object's Z onto the surface below it."""
        vp = getattr(self, 'preview_widget', None)
        insts = self._selected_instances()
        if vp is None or not insts:
            self._set_status("Drop to ground: select an object first")
            return
        moves, missed = [], 0
        for i in insts:
            z = vp.ground_z_below(i.pos_x, i.pos_y, i.pos_z + 0.05, exclude=insts)
            if z is None:
                missed += 1
            elif abs(z - i.pos_z) > 1e-4:
                moves.append((i, i.pos_z, z))
        if moves:
            def _apply(k):
                for i, old, new in moves:
                    i.pos_z = (old, new)[k]
                    self._on_instance_edited(i)
            _apply(1)
            self._push_map_undo(lambda: _apply(0), lambda: _apply(1), f"Drop {len(moves)} object(s) to ground")
        self._set_status(f"Dropped {len(moves)} object(s) to ground"
                         + (f"; {missed} had nothing below" if missed else ""))

    def _align_selected(self, axis): #vers 1
        """Line selected objects up with the primary (last selected) on one axis."""
        insts = self._selected_instances()
        if len(insts) < 2:
            self._set_status("Align: select two or more objects")
            return
        attr = 'pos_' + axis
        target = getattr(insts[-1], attr)
        olds = [(i, getattr(i, attr)) for i in insts]

        def _apply(k):
            for i, old in olds:
                setattr(i, attr, (old, target)[k])
                self._on_instance_edited(i)
        _apply(1)
        self._push_map_undo(lambda: _apply(0), lambda: _apply(1), f"Align {len(insts)} on {axis.upper()}")
        self._set_status(f"Aligned {len(insts)} object(s) on {axis.upper()}")

    def _distribute_selected(self): #vers 1
        """Space selected objects evenly on the line between the two furthest apart."""
        insts = self._selected_instances()
        if len(insts) < 3:
            self._set_status("Distribute: select three or more objects")
            return
        pts = [(i.pos_x, i.pos_y, i.pos_z) for i in insts]
        a, b = max(((p, q) for p in pts for q in pts),
                   key=lambda pq: sum((pq[0][k] - pq[1][k]) ** 2 for k in range(3)))
        d = (b[0] - a[0], b[1] - a[1], b[2] - a[2])
        l2 = sum(c * c for c in d) or 1.0
        order = sorted(insts, key=lambda i: ((i.pos_x - a[0]) * d[0] + (i.pos_y - a[1]) * d[1]
                                             + (i.pos_z - a[2]) * d[2]) / l2)
        n = len(order) - 1
        olds = [(i, (i.pos_x, i.pos_y, i.pos_z)) for i in order]
        news = [(i, (a[0] + d[0] * k / n, a[1] + d[1] * k / n, a[2] + d[2] * k / n))
                for k, i in enumerate(order)]

        def _apply(state):
            for i, p in state:
                i.pos_x, i.pos_y, i.pos_z = p
                self._on_instance_edited(i)
        _apply(news)
        self._push_map_undo(lambda: _apply(olds), lambda: _apply(news), f"Distribute {len(insts)} objects")
        self._set_status(f"Distributed {len(insts)} object(s) evenly")

    def _target_ipl_for_new(self): #vers 1
        """IPL for new objects: selected IPL row, else primary object's IPL, else ask."""
        table = getattr(self, '_ipl_sections_table', None)
        if table is not None and table.currentRow() >= 0:
            it = table.item(table.currentRow(), 0)
            name = it.data(Qt.ItemDataRole.UserRole) if it is not None else None
            if name and name in getattr(self, '_ipl_baseline', {}):
                return name
        sel = self._selected_instances()
        if sel:
            return sel[-1].source_ipl
        names = sorted(getattr(self, '_ipl_baseline', {}))
        if not names:
            return None
        pick, ok = QInputDialog.getItem(self, "Place Object", "Add to IPL:", names, 0, False)
        return pick if ok else None

    def _new_instance(self, model_id, pos): #vers 1
        """IPLInstance for model_id at pos in the target IPL, or None."""
        loader = getattr(self, '_world_loader', None)
        obj = loader.get_object(model_id) if loader else None
        ipl = self._target_ipl_for_new() if obj is not None else None
        if ipl is None:
            return None
        from apps.methods.gta_dat_parser import IPLInstance
        return IPLInstance(model_id=model_id, model_name=obj.model_name, interior=0,
                           pos_x=pos[0], pos_y=pos[1], pos_z=pos[2],
                           rot_x=0.0, rot_y=0.0, rot_z=0.0, rot_w=1.0,
                           lod_index=-1, source_ipl=ipl, line_no=0)

    def _on_model_dropped(self, model_id, pos): #vers 1
        """Object Browser model dropped on the viewport: place it there."""
        inst = self._new_instance(model_id, pos)
        if inst is not None:
            self._add_instances([inst], f"Placed {inst.model_name} in {inst.source_ipl}")

    @staticmethod
    def _render_scale(inst): #vers 1
        """Scale to draw with; (0,0,0) means no scale (converted VC/SOL IPLs)."""
        s = (inst.scale_x, inst.scale_y, inst.scale_z)
        return (1.0, 1.0, 1.0) if s == (0.0, 0.0, 0.0) else s

    def _on_instance_edited(self, inst): #vers 4
        """Called by _InstanceEditPanel."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'update_instance_transform'):
            pos = (inst.pos_x, inst.pos_y, inst.pos_z)
            rot = self._effective_rotation(inst)
            scale = self._render_scale(inst)
            if vp.update_instance_transform(inst, pos, rot, scale):

                loader = getattr(self, '_world_loader', None)
                if loader is not None and any(
                        g.model_id == inst.model_id for g in getattr(loader, 'ide_paths', [])):
                    self._refresh_path_visualization()
                return
        self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=False)

    def _on_ctrl_z_pressed(self): #vers 1
        """Ctrl+Z's own real handler (Aug 20 2026,  "Undo/
        redo for mapping changes") - tries the general map-edit undo
        stack first (the more relevant context for the main Map
        Workshop view), falling back to the older, separate paint/
        material undo system (_undo_last_action, from the collision-
        paint context) only when the map undo stack is empty - keeps
        both real systems reachable from the same real shortcut,
        rather than one silently shadowing the other."""
        if getattr(self, 'map_undo_stack', []):
            self._map_undo()
        elif hasattr(self, '_undo_last_action'):
            self._undo_last_action()
        else:
            self._set_status("Nothing to undo")

    def _on_undo_ribbon_clicked(self): #vers 1
        """Undo ribbon button (Aug 21 2026,  "we need an
        undo button /ribbon icon") - the real, map-undo-aware handler
        (_InstanceEditPanel's own _on_undo_clicked, wired to Ctrl+Z/
        Ctrl+Y earlier this session) was previously only reachable
        from inside the Item Editor Dialog's own [Apply][Undo][Close]
        [Save] row, which only opens by double-clicking an instance -
        genuinely not reachable at all otherwise. A real, separate
        handler here (that panel's own version calls self._workshop.
        _map_undo/_map_redo, since it's a different class - this one
        is already the workshop, so calls its own methods directly)
        rather than reusing that one - same real left-click=undo,
        Shift+click=redo behavior."""
        if QApplication.keyboardModifiers() & Qt.KeyboardModifier.ShiftModifier:
            self._map_redo()
        else:
            self._on_ctrl_z_pressed()

    def _push_map_undo(self, undo_fn, redo_fn, description=""): #vers 2
        """Record one undoable map edit (Aug 18 2026)."""
        self.map_undo_stack = getattr(self, 'map_undo_stack', [])
        self.map_redo_stack = getattr(self, 'map_redo_stack', [])
        self.map_undo_stack.append(
            {'undo': undo_fn, 'redo': redo_fn, 'description': description})
        if len(self.map_undo_stack) > 100:
            self.map_undo_stack.pop(0)
        self.map_redo_stack.clear()
        QTimer.singleShot(0, self._refresh_dirty_ipls)

    def _map_undo(self): #vers 2
        """Reverse the most recent map edit."""
        stack = getattr(self, 'map_undo_stack', [])
        if not stack:
            self._set_status("Nothing to undo")
            return
        entry = stack.pop()
        try:
            entry['undo']()
        except Exception as e:
            self._set_status(f"Undo failed: {e}")
            return
        redo_stack = self.map_redo_stack = getattr(self, 'map_redo_stack', [])
        redo_stack.append(entry)
        QTimer.singleShot(0, self._refresh_dirty_ipls)
        desc = entry.get('description', '')
        self._set_status(f"Undo: {desc}" if desc else "Undo applied")

    def _map_redo(self): #vers 1
        """Reapply the most recently undone map edit."""
        stack = getattr(self, 'map_redo_stack', [])
        if not stack:
            self._set_status("Nothing to redo")
            return
        entry = stack.pop()
        try:
            entry['redo']()
        except Exception as e:
            self._set_status(f"Redo failed: {e}")
            return
        undo_stack = self.map_undo_stack = getattr(self, 'map_undo_stack', [])
        undo_stack.append(entry)
        desc = entry.get('description', '')
        self._set_status(f"Redo: {desc}" if desc else "Redo applied")


    def _toggle_instance_favourite(self, model_id): #vers 1
        """Add/remove a model_id from favourite_objects."""
        favourites = set(self.map_settings.get('favourite_objects') or [])
        if model_id in favourites:
            favourites.discard(model_id)
        else:
            favourites.add(model_id)
        self.map_settings.set('favourite_objects', sorted(favourites))
        self.map_settings.save()
        ob_model = getattr(self, '_object_browser_model', None)
        if ob_model is not None:
            ob_model._favourites = favourites
            ob_model._recompute()

    def _populate_instance_list(self, loader): #vers 2
        """Point the Instance List view at a completed GTAWorldLoader
        load's instances."""
        view = getattr(self, '_instance_table', None)
        if view is None:
            return
        model = _InstanceTableModel(loader.instances, loader, changed_fn=self._inst_changed)
        view.setModel(model)
        sel_model = view.selectionModel()
        if sel_model is not None:
            sel_model.currentRowChanged.connect(
                lambda cur, prev: self._on_instance_row_selected(model, cur.row()))

    def _on_instance_row_selected(self, model, row): #vers 4
        """Single-click (or keyboard navigation) centres the camera,
        shows an XYZ gizmo, and shows/updates the object edit panel for
        the newly-selected instance - same as double-click; both share
        _center_on_instance()."""
        if row < 0:
            return
        inst = model.instance_at(row)
        if inst is None:
            return
        self._center_on_instance(inst)

    def _world_key(self): #vers 1
        return os.path.abspath(getattr(self, '_loaded_dat_path', '') or getattr(self, '_game_root', '') or 'world')

    def _save_camera_bookmark(self, slot): #vers 1
        """Ctrl+Shift+1..9: store the camera in a slot for this world."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None:
            return
        allb = dict(self.map_settings.get('camera_bookmarks') or {})
        mine = dict(allb.get(self._world_key(), {}))
        old = mine.get(str(slot))
        name = old[5] if old else f"Bookmark {slot}"
        mine[str(slot)] = [vp._pan_x, vp._pan_y, vp._dist, vp._yaw, vp._pitch, name]
        allb[self._world_key()] = mine
        self.map_settings.set('camera_bookmarks', allb)
        self._set_status(f"Camera saved to slot {slot} (Ctrl+{slot} to return)")

    def _goto_camera_bookmark(self, slot): #vers 1
        """Ctrl+1..9: jump the camera to a saved slot."""
        vp = getattr(self, 'preview_widget', None)
        b = (self.map_settings.get('camera_bookmarks') or {}).get(self._world_key(), {}).get(str(slot))
        if vp is None or not b:
            self._set_status(f"No camera bookmark in slot {slot} (Ctrl+Shift+{slot} saves one)")
            return
        vp._pan_x, vp._pan_y, vp._dist, vp._yaw, vp._pitch = b[:5]
        if getattr(vp, '_projection', '') == 'ortho':
            vp.makeCurrent(); vp.resizeGL(vp.width(), vp.height())
        vp.update()
        self._set_status(f"Camera: {b[5]}")

    def _camera_bookmarks_menu(self, parent_menu): #vers 1
        """Submenu listing slots with go / rename / delete."""
        m = parent_menu.addMenu("Camera Bookmarks")
        mine = (self.map_settings.get('camera_bookmarks') or {}).get(self._world_key(), {})
        for slot in range(1, 10):
            b = mine.get(str(slot))
            sub = m.addMenu(f"{slot}: {b[5] if b else '(empty)'}")
            sub.addAction("Go (Ctrl+%d)" % slot, lambda s=slot: self._goto_camera_bookmark(s)).setEnabled(bool(b))
            sub.addAction("Save current view here (Ctrl+Shift+%d)" % slot, lambda s=slot: self._save_camera_bookmark(s))
            sub.addAction("Rename...", lambda s=slot: self._rename_camera_bookmark(s)).setEnabled(bool(b))
            sub.addAction("Delete", lambda s=slot: self._delete_camera_bookmark(s)).setEnabled(bool(b))
        return m

    def _rename_camera_bookmark(self, slot): #vers 1
        allb = dict(self.map_settings.get('camera_bookmarks') or {})
        mine = dict(allb.get(self._world_key(), {}))
        b = mine.get(str(slot))
        if not b:
            return
        name, ok = QInputDialog.getText(self, "Camera Bookmark", "Name:", text=b[5])
        if ok and name.strip():
            mine[str(slot)] = b[:5] + [name.strip()]
            allb[self._world_key()] = mine
            self.map_settings.set('camera_bookmarks', allb)

    def _delete_camera_bookmark(self, slot): #vers 1
        allb = dict(self.map_settings.get('camera_bookmarks') or {})
        mine = dict(allb.get(self._world_key(), {}))
        mine.pop(str(slot), None)
        allb[self._world_key()] = mine
        self.map_settings.set('camera_bookmarks', allb)

    def _apply_gamepad_setting(self): #vers 1
        """Start or stop the game controller from map settings."""
        vp = getattr(self, 'preview_widget', None)
        pad = getattr(self, '_gamepad', None)
        want = bool(self.map_settings.get('gamepad_enabled'))
        if not want or vp is None or not hasattr(vp, 'set_gamepad'):
            if pad is not None:
                pad.stop()
                vp.set_gamepad(None)
                self._gamepad = None
            return
        if pad is None:
            from apps.methods.gamepad_input import GamepadPoller
            pad = GamepadPoller(self, deadzone=float(self.map_settings.get('gamepad_deadzone') or 0.15))
            pad.connected.connect(lambda n: self._set_status(
                f"Controller connected: {n}" if n else "Controller disconnected"))
            try:
                pad.start()
            except ImportError:
                QMessageBox.warning(self, "Game Controller",
                                    "Controller support needs pygame 2:\n\npip install pygame")
                self.map_settings.set('gamepad_enabled', False)
                return
            self._gamepad = pad
            vp.set_gamepad(pad)
        pad.deadzone = float(self.map_settings.get('gamepad_deadzone') or 0.15)

    def _on_pad_snap_toggled(self, on): #vers 1
        """Options button toggled edge snap: keep the ribbon button in step."""
        act = getattr(self, '_snap_edge_act', None)
        if act is not None:
            act.blockSignals(True)
            act.setChecked(on)
            act.blockSignals(False)
        self._set_status(f"Edge snap {'on' if on else 'off'}")
