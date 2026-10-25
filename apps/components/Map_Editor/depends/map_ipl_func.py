#this belongs in apps/components/Map_Editor/depends/map_ipl_func.py - Version: 3
# X-Seti - October 14 2026 - IMG Factory 1.6 - Map Workshop IPL files

"""
IPL/IDE/DAT/IMG tabs, IPL save, convert, shift/rotate, savepoints for ModelWorkshop (Map).
"""

##class MapIplMixin: -
# _add_ipl_section_type_checkboxes
# _apply_alt_timecyc
# _apply_dirty_highlight
# _apply_ide_raw
# _apply_instance_fields
# _apply_savepoint_settings
# _auto_savepoint
# _auto_switch_ipl_tab_if_empty
# _binary_stream_source
# _build_ipl_text_lines
# _check_ide_raw
# _confirm_discard_ipls
# _create_dat_tab
# _create_img_tab
# _create_ipl_tab
# _create_savepoint
# _ensure_ipl_loaded
# _export_changed_ipls
# _export_mod_package
# _export_selected_ipl
# _forget_ipl_baseline
# _ide_abs_path
# _ide_raw_line
# _import_ide_files
# _import_ipl_files
# _init_change_tracking
# _insert_stream_into_display_order
# _inst_changed
# _ipl_section_has_data
# _is_stories_obj
# _load_binary_ipl_stream
# _load_selected_ipl_sections
# _map_export_menu
# _map_import_menu
# _map_loader_or_warn
# _map_open_menu
# _mark_ipl_saved
# _move_ipl_section
# _on_drag_ipl_context_menu
# _on_hover_context_menu
# _on_ipl_click_for_move_or_rotate
# _on_ipl_data_type_changed
# _on_ipl_dragged
# _on_ipl_mode_button_clicked
# _on_ipl_section_cell_clicked
# _on_ipl_section_cell_double_clicked
# _on_ipl_sections_column_resized
# _on_ipl_sections_context_menu
# _on_ipl_sections_selection_changed
# _on_ipl_selection_changed
# _on_ipl_tab_changed
# _on_ipl_tab_close_clicked
# _on_ipl_tab_open_clicked
# _on_ipl_tab_open_file_clicked
# _on_show_tobj_toggled
# _on_tcyc_toggled
# _parse_ide_raw
# _populate_ipl_sections
# _populate_wpl_table
# _popup_under
# _prompt_rotate_ipl_coordinates
# _prompt_shift_ipl_coordinates
# _rebase_ipls_dialog
# _rebuild_ipl_sections_rows
# _refresh_dat_tab
# _refresh_dirty_ipls
# _refresh_img_tab
# _regen_changed_radar_tiles
# _restore_savepoint_dialog
# _rotate_all_tracks
# _rotate_all_water
# _rotate_ipl_coordinates
# _save_all_ipls
# _save_ipl_as_game
# _save_ipl_data_as_binary
# _save_ipl_data_as_full
# _save_ipl_data_as_text
# _save_ipl_in_place
# _save_map_or_model
# _save_pending_ide
# _savepoint_dir
# _scan_binary_ipls_in_img_archives
# _shift_all_tracks
# _shift_all_water
# _shift_ipl_coordinates
# _show_alt_timecyc_menu
# _show_engine_load_log
# _show_map_checks
# _show_map_diff
# _show_script_placements
# _stories_ide_line
# _style_ipl_name_item
# _toggle_ipl_section
# _unload_all_ipl_sections
# _unload_ipl_section
# _verify_binary_ipl_parser
# _write_ipl_as_game
# _write_ipl_error_log
# _write_ipl_in_place
# _write_stories_instances
# confirm_close

import math
import os

from PyQt6.QtCore import QSize, QTimer, Qt
from PyQt6.QtGui import QBrush, QColor, QCursor
from PyQt6.QtWidgets import (
    QAbstractItemView, QApplication, QCheckBox, QComboBox, QDialog, QDoubleSpinBox,
    QFileDialog, QHBoxLayout, QHeaderView, QInputDialog, QLabel, QMenu, QMessageBox,
    QPushButton, QTabWidget, QTableWidget, QTableWidgetItem, QVBoxLayout, QWidget)

from apps.components.Map_Editor.depends.map_widgets_func import (
    _IplLoadDialog, _model_workshop_config_dir, _write_ipl_lines, euler_degrees_to_quat,
    quat_to_euler_degrees)


class MapIplMixin: #vers 1
    """IPL/IDE/DAT/IMG tabs, IPL save, convert, shift/rotate, savepoints."""

    def _create_ipl_tab(self): #vers 1
        """[IPL] tab content, matching MooMapper's own "Item Placement"
        tab - the former standalone IPL Sections dock's table moves
        here unchanged, plus an Open/Close/New/Delete button row and
        INST/CULL/ZONE/PATH radio buttons at the bottom (only INST/
        CULL/ZONE are real - PATH isn't parsed anywhere in this
        project yet, honestly stubbed rather than guessed at without
        real sample data to verify against, the same caution applied
        to the VC/GTA3 inst-format work earlier).

        Column order is eye-icon first, then name."""
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(1, 1, 1, 1)
        from apps.methods.imgfactory_svg_icons import get_remove_icon, get_file_icon, get_add_icon, get_close_icon
        title_row_widget = QWidget()
        title_row_widget.setObjectName("IPL_title_row")
        title_row = QHBoxLayout(title_row_widget)
        title_row.setContentsMargins(0, 0, 0, 0)
        label = QLabel("IPL Sections")
        label.setStyleSheet("font-weight: bold;")
        title_row.addWidget(label)
        sm_buttonheight = 20
        _compact_18 = "padding: 0px 4px; margin: 0px; border-width: 1px;"

        icon_color = self._get_icon_color()
        open_btn = QPushButton(get_add_icon(sm_buttonheight, icon_color), "Open")
        open_btn.setToolTip("Load the selected IPL's content on demand -\n"
                            "same as clicking its eye icon to show it") # TODO this needs to aupport zon files.
        open_btn.setIconSize(QSize(18, 18))
        open_btn.setFixedHeight(18)
        open_btn.setStyleSheet(_compact_18)
        open_btn.clicked.connect(self._on_ipl_tab_open_clicked)
        close_btn = QPushButton(get_close_icon(sm_buttonheight, icon_color), "Close")
        close_btn.setToolTip("Hide the selected IPL - same as clicking\n"
                             "its eye icon to hide it (its data stays\n"
                             "loaded, just not shown)")
        close_btn.setIconSize(QSize(18, 18))
        close_btn.setFixedHeight(18)
        close_btn.setStyleSheet(_compact_18)
        close_btn.clicked.connect(self._on_ipl_tab_close_clicked)
        new_btn = QPushButton(get_file_icon(sm_buttonheight, icon_color), "New")
        new_btn.setToolTip("STUB - creating a brand new, empty IPL file\n"
                           "on disk isn't built yet")
        new_btn.setIconSize(QSize(18, 18))
        new_btn.setFixedHeight(18)
        new_btn.setStyleSheet(_compact_18)
        new_btn.setEnabled(False)
        delete_btn = QPushButton(get_remove_icon(sm_buttonheight, icon_color), "Delete")
        delete_btn.setToolTip("STUB - deleting an IPL file from disk isn't\n"
                              "built yet (no write-back infrastructure exists\n"
                              "for any file type in Map Workshop yet)")
        delete_btn.setIconSize(QSize(18, 18))
        delete_btn.setFixedHeight(18)
        delete_btn.setStyleSheet(_compact_18)
        delete_btn.setEnabled(False)
        # Open Zone File... (Aug 16 2026)

        open_file_btn = QPushButton(get_add_icon(sm_buttonheight, icon_color), "")
        # TODO this needs to be combined with the first open function, that can support ipl, ide, and zon files.

        open_file_btn.setToolTip(
            "Load one or more standalone .ipl or .zon files (info.zon,\n"
            "map.zon, navig.zon, cull.ipl, etc.) - they'll appear in\n"
            "this list like any other IPL file, viewable the same way.")
        open_file_btn.setIconSize(QSize(18, 18))
        open_file_btn.setFixedHeight(18)
        open_file_btn.setStyleSheet(_compact_18)
        open_file_btn.clicked.connect(self._on_ipl_tab_open_file_clicked)
        for b in (open_file_btn, close_btn, new_btn, delete_btn):
            title_row.addWidget(b)
        title_row.addStretch()
        lay.addWidget(title_row_widget)
        self._register_collapsible_button_row(
            title_row_widget, [(open_file_btn, "Open"), (close_btn, "Close"),
                                (new_btn, "New"), (delete_btn, "Delete")])
        if not hasattr(self, '_object_browser_tab_rows'):
            self._object_browser_tab_rows = {}
        self._object_browser_tab_rows['ipl'] = title_row_widget

        content_container = QWidget()
        content_lay = QVBoxLayout(content_container)
        content_lay.setContentsMargins(0, 0, 0, 0)

        table = QTableWidget(0, 3)
        table.setHorizontalHeaderLabels(["", "IPL File", "Format"])
        header = table.horizontalHeader()
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.Fixed)
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.Interactive)
        header.setSectionResizeMode(2, QHeaderView.ResizeMode.Interactive)
        header.setStretchLastSection(False)
        table.setColumnWidth(0, 18)
        saved_widths = self.map_settings.get('ipl_sections_column_widths') or []
        if len(saved_widths) >= 3:
            table.setColumnWidth(1, saved_widths[1])
            table.setColumnWidth(2, saved_widths[2])
        elif len(saved_widths) >= 2:
            table.setColumnWidth(1, saved_widths[1])
            table.setColumnWidth(2, 110)
        else:
            table.setColumnWidth(1, 200)
            table.setColumnWidth(2, 110)
        header.sectionResized.connect(self._on_ipl_sections_column_resized)
        self._apply_compact_table_style(table)
        table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        table.setShowGrid(False)

        table.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        table.setToolTip("Toggle which IPL files' placements are shown in World View")
        table.cellClicked.connect(self._on_ipl_section_cell_clicked)

        table.cellDoubleClicked.connect(self._on_ipl_section_cell_double_clicked)
        table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        table.customContextMenuRequested.connect(self._on_ipl_sections_context_menu)

        table.itemSelectionChanged.connect(self._on_ipl_sections_selection_changed)
        content_lay.addWidget(table)
        self._ipl_sections_table = table

        placeholder = QLabel("No world loaded yet")
        #placeholder.setStyleSheet("color: palette(mid);")
        content_lay.addWidget(placeholder)
        self._ipl_sections_placeholder = placeholder
        placeholder.setVisible(True)
        table.setVisible(False)

        self._ipl_data_type = 'inst'

        lay.addWidget(content_container)
        self._make_section_collapsible(label, content_container)
        return panel

    def _populate_ipl_sections(self, loader): #vers 6
        """Fill the IPL Sections panel from a completed load - one row
        per IPL, in the user's saved display order (ipl_sections_order)
        if any, with new/unrecognised names (a different world/mod)
        appended alphabetically after the known ones rather than losing
        the saved order entirely."""
        table = getattr(self, '_ipl_sections_table', None)
        placeholder = getattr(self, '_ipl_sections_placeholder', None)
        if table is None:
            return

        self._all_instances = list(loader.instances)

        icon_color = self._get_icon_color()
        icon_sz = 16
        self._eye_open_icon = self._render_variant_icon('eye_visible', None, icon_sz, icon_color, has_menu=False)
        self._eye_closed_icon = self._render_variant_icon('eye_hidden', None, icon_sz, icon_color, has_menu=False)

        # display name (as it'll appear in the table / match inst.source_ipl
        # once loaded) -> lowercase stem (the key load_ipl_by_name expects)
        self._ipl_display_to_stem = {}
        if getattr(loader, 'lazy_ipl_loading', False):
            for stem, entry in loader.available_ipls.items():
                display_name = os.path.basename(entry.abs_path)
                self._ipl_display_to_stem[display_name] = stem
            current_names = set(self._ipl_display_to_stem.keys())
            # Nothing is actually loaded yet - every IPL starts hidden/
            # unloaded, matching MooMapper's own default state, rather
            # than showing everything as already visible.
            self._hidden_ipls = set(current_names)
        else:
            current_names = {inst.source_ipl for inst in loader.instances}
            self._hidden_ipls = set()

        saved_order = self.map_settings.get('ipl_sections_order') or []
        ordered = [n for n in saved_order if n in current_names]
        remaining = sorted(current_names - set(ordered))
        self._ipl_display_order = ordered + remaining

        self._scan_binary_ipls_in_img_archives(loader)

        self._rebuild_ipl_sections_rows()

        if placeholder is not None:
            placeholder.setVisible(False)
        table.setVisible(True)

    def _scan_binary_ipls_in_img_archives(self, loader): #vers 4
        """Scan every indexed IMG archive (gta3.img etc.) for .ipl-
        extension entries, associating binary ones with their parent
        text IPL rather than listing them as independent entries."""
        self._binary_ipl_names = set()
        self._ipl_names_with_binary_stream = {}       # text display name -> [(archive_path, entry_name), ...]
        self._standalone_binary_ipl_archives = {}      # display name -> archive_path
        get_img_paths = getattr(loader, 'get_img_paths', None)
        if get_img_paths is None:
            return
        from apps.methods.gta_dat_parser import detect_ipl_format
        try:
            from apps.methods.img_core_classes import IMGFile
        except Exception:
            return

        # stem (lowercase, no extension) -> text IPL's own display name
        text_stems = {}
        for display_name in list(self._ipl_display_to_stem.keys()):
            stem = os.path.splitext(display_name)[0].lower()
            text_stems[stem] = display_name

        found_count = 0
        associated_count = 0
        model_cache = getattr(self, '_model_cache', None)
        for img_path in get_img_paths():
            try:
                # Reuse the model_cache's already-opened archive (Aug 1 2026)
                img = model_cache._opened_img_files.get(img_path) if model_cache else None
                if img is None:
                    img = IMGFile(img_path)
                    if not img.open():
                        continue
                    if model_cache is not None:
                        model_cache._opened_img_files[img_path] = img
                for entry in img.entries:
                    name = getattr(entry, 'name', '') or ''
                    is_wpl = name.lower().endswith('.wpl')   # GTA IV, always binary
                    if not (is_wpl or name.lower().endswith('.ipl')):
                        continue
                    if not is_wpl:
                        try:
                            head = img.read_entry_data(entry)[:64]
                        except Exception:
                            continue
                        if detect_ipl_format(head) != 'binary':
                            continue
                    found_count += 1
                    entry_stem = os.path.splitext(name)[0].lower()

                    # Match the exact stem, "{parent}_streamN" or IV "{parent}_strbigN"
                    parent_stem = entry_stem
                    for tag in ('_stream', '_strbig'):
                        if tag in entry_stem:
                            parent_stem = entry_stem.rsplit(tag, 1)[0]
                            break
                    parent_display = text_stems.get(entry_stem) or text_stems.get(parent_stem)

                    if parent_display is not None:
                        self._ipl_names_with_binary_stream.setdefault(parent_display, []).append((img_path, name))
                        associated_count += 1
                        continue

                    # No matching text IPL - genuinely standalone binary entry
                    display_name = name
                    if display_name in self._ipl_display_to_stem:
                        continue
                    stem = f"img:{os.path.basename(img_path)}:{name}".lower()
                    self._ipl_display_to_stem[display_name] = stem
                    self._binary_ipl_names.add(display_name)
                    self._standalone_binary_ipl_archives[display_name] = img_path
                    if display_name not in self._ipl_display_order:
                        self._ipl_display_order.append(display_name)
                    if display_name not in self._hidden_ipls:
                        self._hidden_ipls.add(display_name)
            except Exception:
                continue
        if found_count:
            standalone_count = found_count - associated_count
            self._set_status(
                f"Found {found_count} binary IPL(s) in indexed IMG archives "
                f"({associated_count} matched to a parent text IPL, {standalone_count} standalone)")

    def _rebuild_ipl_sections_rows(self): #vers 4
        """(Re)build every row from self._ipl_display_order - shared by
        the initial populate and by _move_ipl_section, so reordering
        doesn't duplicate the row-construction logic.."""
        order = self._ipl_display_order
        non_zon = [n for n in order if not n.lower().endswith('.zon')]
        zon_only = [n for n in order if n.lower().endswith('.zon')]
        if zon_only and order != non_zon + zon_only:
            self._ipl_display_order = non_zon + zon_only

        table = self._ipl_sections_table
        table.setRowCount(len(self._ipl_display_order))
        hidden = getattr(self, '_hidden_ipls', set())
        loader = getattr(self, '_world_loader', None)
        from apps.methods.gta_dat_parser import detect_ipl_format
        format_cache = self._ipl_format_cache = getattr(self, '_ipl_format_cache', {})

        stream_to_parent = {}
        for parent, streams in getattr(self, '_ipl_names_with_binary_stream', {}).items():
            for _archive, stream_name in streams:
                stream_to_parent[stream_name] = parent
        for row, ipl_name in enumerate(self._ipl_display_order):
            is_hidden = ipl_name in hidden
            eye_item = QTableWidgetItem()
            eye_item.setIcon(self._eye_closed_icon if is_hidden else self._eye_open_icon)
            eye_item.setTextAlignment(Qt.AlignmentFlag.AlignCenter)
            eye_item.setToolTip(f"Show {ipl_name}" if is_hidden else f"Hide {ipl_name}")
            eye_item.setData(Qt.ItemDataRole.UserRole, ipl_name)
            table.setItem(row, 0, eye_item)

            if ipl_name in stream_to_parent:
                is_loaded_display = ipl_name in getattr(self, '_loaded_binary_ipls', set())
                display_text = "    " + ipl_name + (" (Loaded)" if is_loaded_display else "")
                is_loaded = is_loaded_display
            else:
                stem_for_load_check = getattr(self, '_ipl_display_to_stem', {}).get(ipl_name)
                is_text_loaded = (loader is not None and stem_for_load_check is not None
                                  and stem_for_load_check in loader.loaded_ipls)
                display_text = ipl_name + (" (Loaded)" if is_text_loaded else "")
                is_loaded = is_text_loaded
            name_item = QTableWidgetItem(display_text)
            self._style_ipl_name_item(name_item, is_hidden, is_loaded)
            table.setItem(row, 1, name_item)

            if ipl_name in format_cache:
                fmt_text, fmt_tooltip = format_cache[ipl_name]
            else:
                fmt_text = ""
                fmt_tooltip = ""
                if ipl_name in getattr(self, '_binary_ipl_names', set()) or \
                        ipl_name in getattr(self, '_loaded_binary_ipls', set()):
                    fmt_text = os.path.splitext(ipl_name)[0]
                    fmt_tooltip = ipl_name
                elif ipl_name in getattr(self, '_ipl_names_with_binary_stream', {}):
                    stream_entries = sorted(self._ipl_names_with_binary_stream[ipl_name], key=lambda t: t[1])
                    stream_names = [name for _archive, name in stream_entries]
                    first_name = os.path.splitext(stream_names[0])[0]
                    remaining = stream_names[1:]
                    fmt_text = f"{first_name} +{len(remaining)}" if remaining else first_name
                    if remaining:
                        fmt_tooltip = "\n".join(remaining)
                else:
                    stem = getattr(self, '_ipl_display_to_stem', {}).get(ipl_name)
                    entry = loader.available_ipls.get(stem) if (loader and stem) else None
                    if entry is not None and entry.exists:
                        try:
                            with open(entry.abs_path, 'rb') as f:
                                head = f.read(64)
                            if detect_ipl_format(head) == 'binary':
                                fmt_text = "Binary IPL"
                        except Exception:
                            pass
                # Only cache once genuinely resolved (Aug 16 2026)
                if fmt_text:
                    format_cache[ipl_name] = (fmt_text, fmt_tooltip)
            fmt_item = QTableWidgetItem(fmt_text)
            fmt_item.setTextAlignment(Qt.AlignmentFlag.AlignCenter)
            if fmt_tooltip:
                fmt_item.setToolTip(fmt_tooltip)
            table.setItem(row, 2, fmt_item)
        self._apply_dirty_highlight()

    def _move_ipl_section(self, ipl_name, direction): #vers 1
        """Move one IPL section up (-1) or down (+1) in the display
        order, persisting the new order so it survives reloads."""
        order = getattr(self, '_ipl_display_order', None)
        if not order or ipl_name not in order:
            return
        idx = order.index(ipl_name)
        new_idx = idx + direction
        if not (0 <= new_idx < len(order)):
            return  # already at that end
        order[idx], order[new_idx] = order[new_idx], order[idx]
        self._rebuild_ipl_sections_rows()
        self.map_settings.set('ipl_sections_order', order)
        self.map_settings.save()

    def _on_ipl_sections_context_menu(self, pos): #vers 7
        """Right-click a row for Move Up/Down/Load Selected - explicit
        menu actions rather than drag-and-drop, since QTableWidget's
        built-in InternalMove drag-drop is a known source of subtle
        bugs with multi-column rows (data can end up split across the
        wrong rows/columns) - explicit actions are simple and
        reliable."""
        table = self._ipl_sections_table
        index = table.indexAt(pos)
        if not index.isValid():
            return
        item = table.item(index.row(), 0)
        if item is None:
            return
        ipl_name = item.data(Qt.ItemDataRole.UserRole)
        order = getattr(self, '_ipl_display_order', [])
        idx = order.index(ipl_name) if ipl_name in order else -1
        loader = getattr(self, '_world_loader', None)
        stem = getattr(self, '_ipl_display_to_stem', {}).get(ipl_name)
        is_loaded = (stem is not None and loader is not None and stem in loader.loaded_ipls) \
            or ipl_name in getattr(self, '_loaded_binary_ipls', set())
        menu = QMenu(table)
        is_hidden = ipl_name in getattr(self, '_hidden_ipls', set())
        vis_act = menu.addAction("Load (Show)" if is_hidden else "Hide")
        vis_act.triggered.connect(
            lambda checked=False, r=index.row(): self._on_ipl_section_cell_clicked(r, 0))

        # Load Selected (Aug 1 2026)
        unload_act = menu.addAction("Unload (Remove)")
        unload_act.setToolTip(
            "Remove this IPL's loaded data from memory entirely - distinct\n"
            "from Hide, which only stops it from being drawn; the data stays\n"
            "loaded either way. Unloading lets it be loaded again fresh later.")
        unload_act.setEnabled(is_loaded)
        unload_act.triggered.connect(
            lambda checked=False, n=ipl_name: self._unload_ipl_section(n))

        selected_rows = sorted({r.row() for r in table.selectionModel().selectedRows()})
        if len(selected_rows) > 1:
            load_act = menu.addAction(f"Load Selected ({len(selected_rows)})")
            load_act.triggered.connect(
                lambda checked=False, rows=selected_rows: self._load_selected_ipl_sections(rows))

        # Select All / Deselect All / Load All / Unload All (Aug 19 2026)
        menu.addSeparator()
        select_all_act = menu.addAction("Select All")
        select_all_act.triggered.connect(table.selectAll)
        deselect_all_act = menu.addAction("Deselect All")
        deselect_all_act.triggered.connect(table.clearSelection)
        load_all_act = menu.addAction("Load All")
        load_all_act.triggered.connect(
            lambda checked=False: self._load_selected_ipl_sections(range(table.rowCount())))
        unload_all_act = menu.addAction("Unload All")
        unload_all_act.triggered.connect(self._unload_all_ipl_sections)

        menu.addSeparator()
        preload_act = menu.addAction("Preload Game Data Files…")
        preload_act.setToolTip(
            "Browse the game's own data folder directly and load real,\n"
            "non-IPL files like waterpro.dat/water.dat/timecyc.dat -\n"
            "see my own real \"can't seem to find it otherwise\"\n"
            "request for why this exists alongside the IPL list above.")
        preload_act.triggered.connect(self._show_preload_dialog)

        stream_entries = getattr(self, '_ipl_names_with_binary_stream', {}).get(ipl_name)
        if stream_entries:
            stream_menu = menu.addMenu("Load Binary Stream")
            for archive_path, entry_name in sorted(stream_entries, key=lambda t: t[1]):
                act = stream_menu.addAction(entry_name)
                act.triggered.connect(
                    lambda checked=False, ap=archive_path, en=entry_name:
                        self._load_binary_ipl_stream(ap, en))
            verify_menu = menu.addMenu("Verify Binary Parser")
            for archive_path, entry_name in sorted(stream_entries, key=lambda t: t[1]):
                act = verify_menu.addAction(entry_name)
                act.triggered.connect(
                    lambda checked=False, ap=archive_path, en=entry_name:
                        self._verify_binary_ipl_parser(ap, en))
        elif ipl_name in getattr(self, '_binary_ipl_names', set()):
            archive_path = getattr(self, '_standalone_binary_ipl_archives', {}).get(ipl_name)
            if archive_path:
                load_bin_act = menu.addAction("Load Binary Stream")
                load_bin_act.triggered.connect(
                    lambda checked=False, ap=archive_path, en=ipl_name:
                        self._load_binary_ipl_stream(ap, en))

                verify_act = menu.addAction("Verify Binary Parser")
                verify_act.triggered.connect(
                    lambda checked=False, ap=archive_path, en=ipl_name:
                        self._verify_binary_ipl_parser(ap, en))

        # Save Binary IPL as Text (Aug 1 2026)
        if ipl_name in getattr(self, '_loaded_binary_ipls', set()):
            save_text_act = menu.addAction("Save Binary IPL as Text...")
            save_text_act.triggered.connect(
                lambda checked=False, n=ipl_name: self._save_ipl_data_as_text(n))

        #TODO; When working with SA files, have the ability to click on a text ipl, convert to binary.ipl.
        #save options, save to img file, save to desktop.

        save_act = menu.addAction("Save IPL")
        save_act.setToolTip("Write edits back to the original file (backup first)")
        save_act.setEnabled(is_loaded)
        save_act.triggered.connect(
            lambda checked=False, n=ipl_name: self._save_ipl_in_place(n))
        save_all_act = menu.addAction(f"Save All Changed IPLs ({len(getattr(self, '_dirty_ipls', ()))})")
        save_all_act.setEnabled(bool(getattr(self, '_dirty_ipls', None)))
        save_all_act.triggered.connect(lambda checked=False: self._save_all_ipls())
        sp_menu = menu.addMenu("Save Points")
        sp_create = sp_menu.addAction("Create Save Point")
        sp_create.setEnabled(bool(getattr(self, '_dirty_ipls', None)))
        sp_create.triggered.connect(lambda checked=False: self._create_savepoint("Manual"))
        sp_menu.addAction("Restore Save Point...").triggered.connect(
            lambda checked=False: self._restore_savepoint_dialog())
        menu.addAction("Map Checks...").triggered.connect(
            lambda checked=False: self._show_map_checks())
        menu.addAction("IPL Format Checker...").triggered.connect(
            lambda checked=False: self._show_ipl_format_checker())
        menu.addAction("Script Placements (main.scm)...").triggered.connect(
            lambda checked=False: self._show_script_placements())
        menu.addAction("Engine Load Log...").triggered.connect(
            lambda checked=False: self._show_engine_load_log())
        menu.addAction("Show Changes (Diff)...").triggered.connect(
            lambda checked=False: self._show_map_diff())
        menu.addAction("Export Changes as Mod Package...").triggered.connect(
            lambda checked=False: self._export_mod_package())
        self._camera_bookmarks_menu(menu)

        if getattr(loader, 'game', None) == 'sa':            # binary IPL is SA only
            savebin_act = menu.addAction("Save Text as Binary IPL...")
            savebin_act.setEnabled(is_loaded)
            savebin_act.triggered.connect(
                lambda checked=False, n=ipl_name: self._save_ipl_data_as_binary(n))

        conv_act = menu.addAction("Convert to Game Format...")
        conv_act.setEnabled(is_loaded)
        conv_act.triggered.connect(lambda checked=False, n=ipl_name: self._save_ipl_as_game(n))
        rb_names = [table.item(r, 0).data(Qt.ItemDataRole.UserRole)
                    for r in sorted({x.row() for x in table.selectionModel().selectedRows()})
                    if table.item(r, 0) is not None] or [ipl_name]
        rebase_act = menu.addAction(f"Rebase Area ({len(rb_names)} IPL)...")
        rebase_act.triggered.connect(lambda checked=False, ns=rb_names: self._rebase_ipls_dialog(ns))

        # Save IPL Data As... / Unload (Aug 16 2026)
        save_full_act = menu.addAction("Save IPL Data As...")
        save_full_act.setEnabled(is_loaded)
        save_full_act.triggered.connect(
            lambda checked=False, n=ipl_name: self._save_ipl_data_as_full(n))

        menu.addSeparator()

        shift_act = menu.addAction("Shift Coordinates...")
        shift_act.setToolTip(
            "Move every position this IPL holds (instances, cull/zone/\n"
            "occlusion boxes, path nodes, garages, entrances/exits) by a\n"
            "fixed offset - for aligning converted/ported map data to its\n"
            "real intended location as a single rigid-body move.")
        shift_act.setEnabled(is_loaded)
        shift_act.triggered.connect(
            lambda checked=False, n=ipl_name: self._prompt_shift_ipl_coordinates(n))

        rotate_act = menu.addAction("Rotate Coordinates...")
        rotate_act.setToolTip(
            "Rotote every position this IPL holds (instances, cull/zone/\n"
            "occlusion boxes, path nodes, garages, entrances/exits")
        rotate_act.setEnabled(is_loaded)
        rotate_act.triggered.connect(
            lambda checked=False, n=ipl_name: self._prompt_rotate_ipl_coordinates(n))

        menu.addSeparator()
        up_act = menu.addAction("Move Up")
        up_act.setEnabled(idx > 0)
        up_act.triggered.connect(lambda checked=False, n=ipl_name: self._move_ipl_section(n, -1))
        down_act = menu.addAction("Move Down")
        down_act.setEnabled(0 <= idx < len(order) - 1)
        down_act.triggered.connect(lambda checked=False, n=ipl_name: self._move_ipl_section(n, 1))
        menu.exec(table.viewport().mapToGlobal(pos))

    def _unload_ipl_section(self, ipl_name): #vers 2
        """Actually remove an IPL's loaded content from memory -
        freeing it up, distinct from Hide (which only filters what's
        drawn; the data stays loaded either way)"""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        if not self._confirm_discard_ipls([ipl_name], "Unload"):
            return
        self._forget_ipl_baseline([ipl_name])
        stem = getattr(self, '_ipl_display_to_stem', {}).get(ipl_name)

        loader.instances[:] = [i for i in loader.instances if i.source_ipl != ipl_name]
        loader.culls[:] = [c for c in getattr(loader, 'culls', [])
                           if getattr(c, 'source_ipl', None) != ipl_name]
        loader.zones[:] = [z for z in getattr(loader, 'zones', [])
                           if z.get('source_ipl') != ipl_name]
        loader.paths[:] = [p for p in getattr(loader, 'paths', []) if p.source_ipl != ipl_name]
        loader.grges[:] = [g for g in getattr(loader, 'grges', []) if g.source_ipl != ipl_name]
        loader.enexes[:] = [e for e in getattr(loader, 'enexes', []) if e.source_ipl != ipl_name]
        loader.occls[:] = [o for o in getattr(loader, 'occls', []) if o.source_ipl != ipl_name]

        if stem is not None:
            loader.loaded_ipls.discard(stem)
        loaded_binary = getattr(self, '_loaded_binary_ipls', None)
        if loaded_binary is not None:
            loaded_binary.discard(ipl_name)

        self._all_instances = list(loader.instances)
        self._hidden_ipls.add(ipl_name)
        self._populate_object_browser(loader)
        self._rebuild_ipl_sections_rows()
        self._apply_ipl_visibility_filter()
        self._set_status(f"Unloaded {ipl_name}")

    def _unload_all_ipl_sections(self): #vers 2
        """Unload every currently-loaded IPL at once (Aug 19 2026)"""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        display_names = list(getattr(self, '_ipl_display_to_stem', {}).keys())
        loaded_names = [n for n in display_names if n not in getattr(self, '_hidden_ipls', set())]
        if not loaded_names:
            self._set_status("No loaded IPLs to unload")
            return
        if not self._confirm_discard_ipls(loaded_names, "Unload All"):
            return
        self._forget_ipl_baseline(loaded_names)
        loaded_set = set(loaded_names)

        loader.instances[:] = [i for i in loader.instances if i.source_ipl not in loaded_set]
        loader.culls[:] = [c for c in getattr(loader, 'culls', [])
                           if getattr(c, 'source_ipl', None) not in loaded_set]
        loader.zones[:] = [z for z in getattr(loader, 'zones', [])
                           if z.get('source_ipl') not in loaded_set]
        loader.paths[:] = [p for p in getattr(loader, 'paths', []) if p.source_ipl not in loaded_set]
        loader.grges[:] = [g for g in getattr(loader, 'grges', []) if g.source_ipl not in loaded_set]
        loader.enexes[:] = [e for e in getattr(loader, 'enexes', []) if e.source_ipl not in loaded_set]
        loader.occls[:] = [o for o in getattr(loader, 'occls', []) if o.source_ipl not in loaded_set]

        stem_map = getattr(self, '_ipl_display_to_stem', {})
        for name in loaded_names:
            stem = stem_map.get(name)
            if stem is not None:
                loader.loaded_ipls.discard(stem)
        loaded_binary = getattr(self, '_loaded_binary_ipls', None)
        if loaded_binary is not None:
            loaded_binary.difference_update(loaded_set)

        self._all_instances = list(loader.instances)
        self._hidden_ipls.update(loaded_set)
        self._populate_object_browser(loader)
        self._rebuild_ipl_sections_rows()
        self._apply_ipl_visibility_filter()
        self._set_status(f"Unloaded {len(loaded_names)} IPL(s)")

    def _shift_ipl_coordinates(self, ipl_name, dx, dy, dz, include_types=None, _record_undo=True): #vers 3
        """Translate every real position an IPL's loaded data holds
        by a fixed (dx,dy,dz) offset - added so converted/ported map
        data.

        Undoable now (Aug 20 2026,  "Undo/redo for mapping
        changes") - was a real, significant gap: moving/rotating a
        whole IPL section (potentially many instances plus cull/zone/
        path/grge/enex/occl entries all at once) had no undo at all
        before this, despite this method's own caller (DFFViewport's
        own whole-IPL drag-end handler) already carrying a comment
        about "avoiding a pointless undo-stack entry" - the entry
        itself was never actually being pushed. _record_undo=False is
        used internally by the undo/redo closures below themselves, so
        applying the inverse shift to undo doesn't itself get pushed
        as a second, new undo entry."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        want = include_types if include_types is not None else \
            {'inst', 'cull', 'zone', 'path', 'grge', 'enex', 'occl'}
        moved = 0
        if 'inst' in want:
            for inst in loader.instances:
                if inst.source_ipl == ipl_name:
                    inst.pos_x += dx; inst.pos_y += dy; inst.pos_z += dz
                    moved += 1
        if 'cull' in want:
            for c in getattr(loader, 'culls', []):
                if getattr(c, 'source_ipl', None) == ipl_name:
                    c.center_x += dx; c.center_y += dy; c.center_z += dz
                    c.x1 += dx; c.y1 += dy; c.z1 += dz
                    c.x2 += dx; c.y2 += dy; c.z2 += dz
                    moved += 1
        if 'zone' in want:
            for z in getattr(loader, 'zones', []):
                if z.get('source_ipl') == ipl_name:
                    z['min_x'] += dx; z['min_y'] += dy; z['min_z'] += dz
                    z['max_x'] += dx; z['max_y'] += dy; z['max_z'] += dz
                    moved += 1
        if 'path' in want:
            for p in getattr(loader, 'paths', []):
                if p.source_ipl == ipl_name:
                    for node in p.nodes:
                        node.x += dx; node.y += dy; node.z += dz
                    moved += 1
        if 'grge' in want:
            for g in getattr(loader, 'grges', []):
                if g.source_ipl == ipl_name:
                    g.x1 += dx; g.y1 += dy; g.z1 += dz
                    g.front_x += dx; g.front_y += dy
                    g.x2 += dx; g.y2 += dy; g.z2 += dz
                    moved += 1
        if 'enex' in want:
            for e in getattr(loader, 'enexes', []):
                if e.source_ipl == ipl_name:
                    e.enter_x += dx; e.enter_y += dy; e.enter_z += dz
                    e.exit_x += dx; e.exit_y += dy; e.exit_z += dz
                    moved += 1
        if 'occl' in want:
            for o in getattr(loader, 'occls', []):
                if o.source_ipl == ipl_name:
                    o.mid_x += dx; o.mid_y += dy; o.bottom_z += dz
                    moved += 1

        self._all_instances = list(loader.instances)
        self._apply_ipl_visibility_filter()
        self._set_status(
            f"Shifted {ipl_name} by ({dx:g}, {dy:g}, {dz:g}) - {moved} entries moved")

        if _record_undo and moved:
            def _undo():
                self._shift_ipl_coordinates(ipl_name, -dx, -dy, -dz, want, _record_undo=False)

            def _redo():
                self._shift_ipl_coordinates(ipl_name, dx, dy, dz, want, _record_undo=False)

            self._push_map_undo(_undo, _redo, f"Shift {ipl_name} by ({dx:g}, {dy:g}, {dz:g})")

    def _rotate_ipl_coordinates(self, ipl_name, pivot_x, pivot_y, angle_deg, include_types=None, _record_undo=True): #vers 3
        """Rotate every real position an IPL's loaded data holds
        around a given (pivot_x, pivot_y) point, by angle_deg around
        the vertical (Z) axis (Aug 19 2026).

        Undoable now (Aug 20 2026,  "Undo/redo for mapping
        changes") - same real gap and same real fix as _shift_ipl_
        coordinates' own docstring describes. _record_undo=False is
        used internally by the undo/redo closures below themselves."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        want = include_types if include_types is not None else \
            {'inst', 'cull', 'zone', 'path', 'grge', 'enex', 'occl'}
        rad = math.radians(angle_deg)
        cos_a, sin_a = math.cos(rad), math.sin(rad)

        def rotate_xy(x, y):
            dx, dy = x - pivot_x, y - pivot_y
            return pivot_x + dx * cos_a - dy * sin_a, pivot_y + dx * sin_a + dy * cos_a

        moved = 0
        if 'inst' in want:
            for inst in loader.instances:
                if inst.source_ipl != ipl_name:
                    continue
                inst.pos_x, inst.pos_y = rotate_xy(inst.pos_x, inst.pos_y)
                ex, ey, ez, ew = self._conjugate_rotation_for_game(
                    inst.rot_x, inst.rot_y, inst.rot_z, inst.rot_w)
                roll, pitch, yaw = quat_to_euler_degrees(ex, ey, ez, ew)
                nx, ny, nz, nw = euler_degrees_to_quat(roll, pitch, yaw + angle_deg)
                inst.rot_x, inst.rot_y, inst.rot_z, inst.rot_w = \
                    self._conjugate_rotation_for_game(nx, ny, nz, nw)
                moved += 1

        if 'cull' in want:
            for c in getattr(loader, 'culls', []):
                if getattr(c, 'source_ipl', None) != ipl_name:
                    continue
                half_w = (c.x2 - c.x1) / 2.0
                half_h = (c.y2 - c.y1) / 2.0
                c.center_x, c.center_y = rotate_xy(c.center_x, c.center_y)
                c.x1, c.y1 = c.center_x - half_w, c.center_y - half_h
                c.x2, c.y2 = c.center_x + half_w, c.center_y + half_h
                moved += 1

        if 'zone' in want:
            for z in getattr(loader, 'zones', []):
                if z.get('source_ipl') != ipl_name:
                    continue
                half_w = (z['max_x'] - z['min_x']) / 2.0
                half_h = (z['max_y'] - z['min_y']) / 2.0
                cx, cy = rotate_xy((z['min_x'] + z['max_x']) / 2.0, (z['min_y'] + z['max_y']) / 2.0)
                z['min_x'], z['min_y'] = cx - half_w, cy - half_h
                z['max_x'], z['max_y'] = cx + half_w, cy + half_h
                moved += 1

        if 'path' in want:
            for p in getattr(loader, 'paths', []):
                if p.source_ipl != ipl_name:
                    continue
                for node in p.nodes:
                    node.x, node.y = rotate_xy(node.x, node.y)
                moved += 1

        if 'grge' in want:
            for g in getattr(loader, 'grges', []):
                if g.source_ipl != ipl_name:
                    continue
                half_w = (g.x2 - g.x1) / 2.0
                half_h = (g.y2 - g.y1) / 2.0
                cx, cy = rotate_xy((g.x1 + g.x2) / 2.0, (g.y1 + g.y2) / 2.0)
                g.x1, g.y1 = cx - half_w, cy - half_h
                g.x2, g.y2 = cx + half_w, cy + half_h
                g.front_x, g.front_y = rotate_xy(g.front_x, g.front_y)
                moved += 1

        if 'enex' in want:
            for e in getattr(loader, 'enexes', []):
                if e.source_ipl != ipl_name:
                    continue
                e.enter_x, e.enter_y = rotate_xy(e.enter_x, e.enter_y)
                e.exit_x, e.exit_y = rotate_xy(e.exit_x, e.exit_y)
                e.enter_angle += angle_deg
                e.exit_angle += angle_deg
                moved += 1

        if 'occl' in want:
            for o in getattr(loader, 'occls', []):
                if o.source_ipl != ipl_name:
                    continue
                o.mid_x, o.mid_y = rotate_xy(o.mid_x, o.mid_y)
                o.rotation += angle_deg
                moved += 1

        self._all_instances = list(loader.instances)
        self._apply_ipl_visibility_filter()
        self._set_status(
            f"Rotated {ipl_name} by {angle_deg:g}\u00b0 around ({pivot_x:g}, {pivot_y:g}) - {moved} entries moved")

        if _record_undo and moved:
            def _undo():
                self._rotate_ipl_coordinates(
                    ipl_name, pivot_x, pivot_y, -angle_deg, want, _record_undo=False)

            def _redo():
                self._rotate_ipl_coordinates(
                    ipl_name, pivot_x, pivot_y, angle_deg, want, _record_undo=False)

            self._push_map_undo(_undo, _redo, f"Rotate {ipl_name} by {angle_deg:g}\u00b0")

    def _shift_all_tracks(self, dx, dy, dz): #vers 1
        """Translate every loaded train track waypoint by a fixed
        offset (Aug 19 2026)"""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return 0
        moved = 0
        for waypoints in getattr(loader, 'tracks', {}).values():
            for wp in waypoints:
                wp.x += dx; wp.y += dy; wp.z += dz
                moved += 1
        if moved:
            self._refresh_track_visualization()
        return moved

    def _rotate_all_tracks(self, pivot_x, pivot_y, angle_deg): #vers 1
        """Rotate every loaded train track waypoint around a given
        pivot (Aug 19 2026) - the rotation counterpart to _shift_all_
        tracks, same reasoning for why this is separate from _rotate_
        ipl_coordinates rather than one of its own include_types."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return 0
        rad = math.radians(angle_deg)
        cos_a, sin_a = math.cos(rad), math.sin(rad)
        moved = 0
        for waypoints in getattr(loader, 'tracks', {}).values():
            for wp in waypoints:
                dx, dy = wp.x - pivot_x, wp.y - pivot_y
                wp.x = pivot_x + dx * cos_a - dy * sin_a
                wp.y = pivot_y + dx * sin_a + dy * cos_a
                moved += 1
        if moved:
            self._refresh_track_visualization()
        return moved

    def _shift_all_water(self, dx, dy, dz): #vers 1
        """Translate every loaded water.dat shape's own corners by a
        fixed offset (Aug 20 2026)"""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return 0
        moved = 0
        for shape in getattr(loader, 'water_shapes', []):
            for corner in shape.corners:
                corner.x += dx; corner.y += dy; corner.z += dz
                moved += 1
        return moved

    def _rotate_all_water(self, pivot_x, pivot_y, angle_deg): #vers 1
        """Rotate every loaded water.dat shape's own corners around a
        given pivot (Aug 20 2026) - the rotation counterpart to
        _shift_all_water, same reasoning for why this is separate
        from _rotate_ipl_coordinates rather than one of its own
        include_types, and same deliberate non-rounding of the
        resulting X/Y (a rotation is, if anything, MORE likely than a
        translate to produce non-even results, so the dialog's own
        warning matters even more here)."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return 0
        rad = math.radians(angle_deg)
        cos_a, sin_a = math.cos(rad), math.sin(rad)
        moved = 0
        for shape in getattr(loader, 'water_shapes', []):
            for corner in shape.corners:
                dx, dy = corner.x - pivot_x, corner.y - pivot_y
                corner.x = pivot_x + dx * cos_a - dy * sin_a
                corner.y = pivot_y + dx * sin_a + dy * cos_a
                moved += 1
        return moved

    def _add_ipl_section_type_checkboxes(self, form): #vers 1
        """Shared tick-box builder for the Move/Rotate IPL dialogs
        (Aug 19 2026)"""
        boxes = {}
        labels = [
            ('inst', "Instances"), ('path', "Paths"), ('zone', "Zones"),
            ('cull', "Cull"), ('grge', "Garages"), ('enex', "Entrances/Exits"),
            ('occl', "Occlusion"),
        ]
        for key, label in labels:
            chk = QCheckBox(label)
            chk.setChecked(True)
            form.addRow(chk)
            boxes[key] = chk
        tracks_chk = QCheckBox("Tracks (all loaded, global)")
        tracks_chk.setChecked(False)
        tracks_chk.setToolTip(
            "Train tracks aren't tied to any specific IPL, so this\n"
            "moves EVERY loaded track, not just ones near this IPL.\n"
            "Off by default for that reason.")
        form.addRow(tracks_chk)
        boxes['tracks'] = tracks_chk
        # Water (Aug 20 2026)
        water_chk = QCheckBox("Water (all loaded, global)")
        water_chk.setChecked(False)
        water_chk.setToolTip(
            "water.dat shapes aren't tied to any specific IPL either,\n"
            "so this moves EVERY loaded water shape, not just ones\n"
            "near this IPL. Off by default for that reason.\n\n"
            "Warning: the real water.dat format requires X/Y corner\n"
            "coordinates to be even, rounded numbers or the game can\n"
            "crash near that water - this does NOT round the result\n"
            "back to an even number for you, so check the values\n"
            "afterward if precision matters here.")
        form.addRow(water_chk)
        boxes['water'] = water_chk
        return boxes

    def _on_ipl_dragged(self, ipl_name, dx, dy, dz): #vers 1
        """Commit a completed whole-IPL click-drag in the 3D viewport
        (Aug 18 2026)"""
        self._shift_ipl_coordinates(ipl_name, dx, dy, dz)

    def _on_ipl_mode_button_clicked(self): #vers 1
        """Cycle the IPL interaction button through Off -> Drag ->
        Move -> Rotate -> Off (Aug 19 2026)"""
        states = ['off', 'drag', 'move', 'rotate']
        current = getattr(self, '_ipl_interaction_mode', 'off')
        idx = states.index(current) if current in states else 0
        new_mode = states[(idx + 1) % len(states)]
        self._ipl_interaction_mode = new_mode

        btn = getattr(self, '_ipl_mode_btn', None)
        if btn is not None:
            btn.setText(f"IPL: {new_mode.capitalize()}")

        vp = getattr(self, 'preview_widget', None)
        if vp is not None:
            if hasattr(vp, 'set_ipl_drag_mode'):
                vp.set_ipl_drag_mode(new_mode != 'off')
            if hasattr(vp, 'set_ipl_interaction_mode'):
                vp.set_ipl_interaction_mode(new_mode if new_mode != 'off' else 'drag')
        self._set_status(f"IPL interaction mode: {new_mode.capitalize()}")

    def _on_ipl_click_for_move_or_rotate(self, ipl_name): #vers 2
        """Fired by DFFViewport.set_ipl_click_callback when an
        instance is picked while the IPL mode button is in Move or
        Rotate state (Aug 19 2026)"""
        mode = getattr(self, '_ipl_interaction_mode', 'drag')
        if mode not in ('move', 'rotate'):
            return
        vp = getattr(self, 'preview_widget', None)
        selection = getattr(vp, '_multi_selected_ipl_names', None) if vp is not None else None
        target = set(selection) if selection else ipl_name
        if mode == 'move':
            self._prompt_shift_ipl_coordinates(target)
        elif mode == 'rotate':
            self._prompt_rotate_ipl_coordinates(target)

    def _on_ipl_selection_changed(self, selected_names): #vers 2
        """Fired by DFFViewport.set_ipl_selection_callback whenever a
        Shift+click adds/removes an IPL from the current multi-select
        (Aug 19 2026)"""
        if not selected_names:
            self._set_status("IPL selection cleared")
            return
        names = sorted(selected_names)
        if len(names) <= 4:
            self._set_status(f"{len(names)} entire ipl(s) selected: {', '.join(names)}")
        else:
            self._set_status(f"{len(names)} entire ipl(s) selected")

    def _on_ipl_sections_selection_changed(self): #vers 1
        """Fired whenever the IPL Sections table's own row selection
        changes (Aug 19 2026)"""
        table = getattr(self, '_ipl_sections_table', None)
        if table is None:
            return
        names = set()
        for row in {idx.row() for idx in table.selectedIndexes()}:
            item = table.item(row, 0)
            if item is not None:
                name = item.data(Qt.ItemDataRole.UserRole)
                if name:
                    names.add(name)
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_multi_selected_ipl_names'):
            vp.set_multi_selected_ipl_names(names)
        self._on_ipl_selection_changed(names)


    def _on_hover_context_menu(self, inst): #vers 5
        """Fired by DFFViewport.set_hover_context_callback on a right-
        click while an instance is currently hover-highlighted (Aug
        19 2026). Object actions work on the selection (or this object)."""
        if inst not in self._selected_instances():
            self._select_instances([inst])
        n = len(self._selected_instances())
        menu = QMenu(self)
        info_act = menu.addAction("Info")
        textures_act = menu.addAction("Show Textures")
        menu.addSeparator()
        menu.addAction(f"Duplicate ({n})\tCtrl+D", self._duplicate_selected)
        menu.addAction(f"Drop to Ground ({n})\tEnd", self._drop_selected_to_ground)
        align = menu.addMenu("Align to last selected")
        for ax in 'xyz':
            align.addAction(ax.upper(), lambda a=ax: self._align_selected(a)).setEnabled(n > 1)
        menu.addAction("Distribute evenly", self._distribute_selected).setEnabled(n > 2)
        menu.addAction("Bake Prelight to DFF...", self._bake_prelight_selected)
        menu.addAction("Generate World Prelighting...", self._generate_world_prelight)
        menu.addAction("Save World Prelighting to DFFs...", self._save_world_prelight)
        menu.addSeparator()
        menu.addAction(f"Delete ({n})\tDel", self._delete_selected_instances)
        chosen = menu.exec(QCursor.pos())
        if chosen is info_act:
            self._show_instance_edit_panel(inst)
        elif chosen is textures_act:
            self._show_textures_for_instance(inst)

    def _on_drag_ipl_context_menu(self, pos): #vers 2
        """Right-click on the IPL mode button - axis-lock choices
        (Aug 18 2026)"""
        vp = getattr(self, 'preview_widget', None)
        current = getattr(vp, '_ipl_drag_axis_lock', None) if vp is not None else None

        menu = QMenu(self)
        free_act = menu.addAction("Free movement (X and Y)")
        free_act.setCheckable(True)
        free_act.setChecked(current is None)
        lock_x_act = menu.addAction("Lock X (only Y moves)")
        lock_x_act.setCheckable(True)
        lock_x_act.setChecked(current == 'x')
        lock_y_act = menu.addAction("Lock Y (only X moves)")
        lock_y_act.setCheckable(True)
        lock_y_act.setChecked(current == 'y')

        btn = getattr(self, '_ipl_mode_btn', None)
        chosen = menu.exec(btn.mapToGlobal(pos) if btn is not None else self.mapToGlobal(pos))
        if chosen is None or vp is None or not hasattr(vp, 'set_ipl_drag_axis_lock'):
            return
        if chosen is free_act:
            vp.set_ipl_drag_axis_lock(None)
        elif chosen is lock_x_act:
            vp.set_ipl_drag_axis_lock('x')
        elif chosen is lock_y_act:
            vp.set_ipl_drag_axis_lock('y')

    def _prompt_shift_ipl_coordinates(self, ipl_names): #vers 3
        """Dialog collecting a (dx,dy,dz) offset, then applies it via
        _shift_ipl_coordinates - the entry point wired to the IPL
        Sections right-click menu's "Shift Coordinates..." action and
        to Move mode's own click interaction. ipl_names (Aug 19 2026)"""
        if isinstance(ipl_names, str):
            names = [ipl_names]
        else:
            names = sorted(set(ipl_names))
        if not names:
            return

        from PyQt6.QtWidgets import QDialog, QFormLayout, QDoubleSpinBox, QDialogButtonBox, QHBoxLayout, QToolButton

        dlg = QDialog(self)
        title = f"Move - {names[0]}" if len(names) == 1 else f"Move - {len(names)} IPLs"
        dlg.setWindowTitle(title)
        form = QFormLayout(dlg)
        if len(names) > 1:
            from PyQt6.QtWidgets import QLabel
            form.addRow(QLabel(f"Applies to all {len(names)} selected IPLs"))

        def _axis_row(label):
            spin = QDoubleSpinBox(); spin.setRange(-100000.0, 100000.0); spin.setDecimals(3)
            minus_btn = QToolButton(); minus_btn.setText("-")
            plus_btn = QToolButton(); plus_btn.setText("+")
            minus_btn.clicked.connect(lambda: spin.setValue(spin.value() - 1.0))
            plus_btn.clicked.connect(lambda: spin.setValue(spin.value() + 1.0))
            row = QHBoxLayout()
            row.addWidget(minus_btn); row.addWidget(spin); row.addWidget(plus_btn)
            form.addRow(label, row)
            return spin

        dx_spin = _axis_row("Delta X:")
        dy_spin = _axis_row("Delta Y:")
        dz_spin = _axis_row("Delta Z:")

        type_boxes = self._add_ipl_section_type_checkboxes(form)

        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                                QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        form.addRow(btns)

        if dlg.exec() == QDialog.DialogCode.Accepted:
            dx, dy, dz = dx_spin.value(), dy_spin.value(), dz_spin.value()
            if not (dx or dy or dz):
                return
            include = {k for k, chk in type_boxes.items()
                      if k not in ('tracks', 'water') and chk.isChecked()}
            if include:
                for name in names:
                    self._shift_ipl_coordinates(name, dx, dy, dz, include_types=include)
            if type_boxes['tracks'].isChecked():
                self._shift_all_tracks(dx, dy, dz)
            if type_boxes['water'].isChecked():
                self._shift_all_water(dx, dy, dz)
                self._refresh_water_visualization()

    def _prompt_rotate_ipl_coordinates(self, ipl_names): #vers 4
        """Dialog collecting a rotation angle (around a pivot
        automatically computed as the combined centroid of every
        selected IPL's own instance positions together - a sensible
        default rather than making the person manually locate and
        type a pivot point every time), then applies it via _rotate_
        ipl_coordinates. Entry point for Rotate mode's own click
        interaction (see set_ipl_drag_mode's docstring for the 3-state
        Drag/Move/Rotate cycle this belongs to) - mirrors _prompt_
        shift_ipl_coordinates' own structure and wiring pattern
        exactly, just for angle instead of XYZ."""
        if isinstance(ipl_names, str):
            names = [ipl_names]
        else:
            names = sorted(set(ipl_names))
        if not names:
            return
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        names_set = set(names)
        xs = [inst.pos_x for inst in loader.instances if inst.source_ipl in names_set]
        ys = [inst.pos_y for inst in loader.instances if inst.source_ipl in names_set]
        if not xs:
            label = names[0] if len(names) == 1 else f"the {len(names)} selected IPLs"
            self._set_status(f"No instances found in {label} to rotate around")
            return
        pivot_x, pivot_y = sum(xs) / len(xs), sum(ys) / len(ys)

        from PyQt6.QtWidgets import QDialog, QFormLayout, QDoubleSpinBox, QDialogButtonBox, QLabel, QHBoxLayout, QToolButton

        dlg = QDialog(self)
        title = f"Rotate - {names[0]}" if len(names) == 1 else f"Rotate - {len(names)} IPLs"

        dlg.setWindowTitle(title)
        form = QFormLayout(dlg)

        if len(names) > 1:
            form.addRow(QLabel(f"Applies to all {len(names)} selected IPLs"))
        pivot_desc = (f"centre of {names[0]}'s own instances"
            if len(names) == 1
            else f"combined centre of all {len(names)} selected IPLs' instances")
        form.addRow(QLabel(f"Pivot: {pivot_desc} ({pivot_x:.1f}, {pivot_y:.1f})"))

        angle_spin = QDoubleSpinBox(); angle_spin.setRange(-360.0, 360.0); angle_spin.setDecimals(2)
        angle_spin.setSuffix("\u00b0")
        minus_btn = QToolButton(); minus_btn.setText("-")
        plus_btn = QToolButton(); plus_btn.setText("+")
        minus_btn.clicked.connect(lambda: angle_spin.setValue(angle_spin.value() - 5.0))
        plus_btn.clicked.connect(lambda: angle_spin.setValue(angle_spin.value() + 5.0))
        angle_row = QHBoxLayout()
        angle_row.addWidget(minus_btn); angle_row.addWidget(angle_spin); angle_row.addWidget(plus_btn)
        form.addRow("Angle:", angle_row)

        type_boxes = self._add_ipl_section_type_checkboxes(form)

        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                                QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        form.addRow(btns)

        if dlg.exec() == QDialog.DialogCode.Accepted:
            angle = angle_spin.value()
            if not angle:
                return
            include = {k for k, chk in type_boxes.items()
                      if k not in ('tracks', 'water') and chk.isChecked()}
            if include:
                for name in names:
                    self._rotate_ipl_coordinates(name, pivot_x, pivot_y, angle, include_types=include)
            if type_boxes['tracks'].isChecked():
                self._rotate_all_tracks(pivot_x, pivot_y, angle)
            if type_boxes['water'].isChecked():
                self._rotate_all_water(pivot_x, pivot_y, angle)
                self._refresh_water_visualization()

    def _save_ipl_data_as_text(self, ipl_name): #vers 2
        """Export an IPL's actual loaded instances out as a standard
        text-format .ipl file - originally a binary-IPL diagnostic."""
        all_inst = getattr(self, '_all_instances', None) or []
        matching = [i for i in all_inst if i.source_ipl == ipl_name]
        if not matching:
            QMessageBox.information(self, "Save IPL Data As Text",
                f"No loaded instances found for {ipl_name}.")
            return
        default_name = os.path.splitext(ipl_name)[0] + ".ipl"
        path, _filter = QFileDialog.getSaveFileName(
            self, "Save IPL Data As Text", default_name, "IPL files (*.ipl);;All files (*)")
        if not path:
            return
        try:
            _rows = ["inst\n"] + [
                f"{inst.model_id}, {inst.model_name}, {inst.interior}, "
                f"{inst.pos_x:.6f}, {inst.pos_y:.6f}, {inst.pos_z:.6f}, "
                f"{inst.rot_x:.7f}, {inst.rot_y:.7f}, {inst.rot_z:.7f}, {inst.rot_w:.7f}, "
                f"{inst.lod_index}\n" for inst in matching] + ["end\n"]
            _write_ipl_lines(path, _rows)
        except Exception as e:
            QMessageBox.warning(self, "Save Binary IPL as Text", f"Failed to save: {e}")
            return
        self._set_status(f"Saved {len(matching)} instances from {ipl_name} to {path}")

    def _save_ipl_data_as_binary(self, ipl_name): #vers 1
        """Convert an IPL's actual loaded instances to real binary-
        IPL format and save the result (Aug 20 2026)"""
        all_inst = getattr(self, '_all_instances', None) or []
        matching = [i for i in all_inst if i.source_ipl == ipl_name]
        if not matching:
            QMessageBox.information(self, "Save Text as Binary IPL",
                f"No loaded instances found for {ipl_name}.")
            return

        confirm = QMessageBox.warning(
            self, "Save Text as Binary IPL - Experimental",
            f"Converting {len(matching)} instance(s) from {ipl_name} to real "
            f"binary IPL format.\n\n"
            f"Only the instance placements themselves are included - cull/"
            f"zone/path/occlusion/garage/entrance-exit data has no binary-"
            f"IPL representation at all in the real format and won't be in "
            f"the saved file.\n\n"
            f"Only part of the binary header format is actually confirmed. "
            f"The unconfirmed parts are written as 0 (the most conservative "
            f"choice available, not a verified-correct one). This has been "
            f"checked by reading the result back through this app's own "
            f"binary IPL reader and getting an exact match, but it has "
            f"never been tested in an actual running game - there's a real "
            f"chance the game itself may not accept it correctly.\n\n"
            f"Continue?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.Cancel,
            QMessageBox.StandardButton.Cancel)
        if confirm != QMessageBox.StandardButton.Yes:
            return

        default_name = os.path.splitext(ipl_name)[0] + ".ipl"
        path, _filter = QFileDialog.getSaveFileName(
            self, "Save Text as Binary IPL", default_name, "IPL files (*.ipl);;All files (*)")
        if not path:
            return
        try:
            from apps.methods.gta_dat_parser import write_binary_ipl_inst_only
            data = write_binary_ipl_inst_only(matching)
            from apps.methods.file_backup import safe_write_bytes
            safe_write_bytes(path, data)
        except Exception as e:
            QMessageBox.warning(self, "Save Text as Binary IPL", f"Failed to save: {e}")
            return
        self._set_status(
            f"Saved {len(matching)} instances from {ipl_name} to binary IPL: {path}")

    def _save_ipl_data_as_full(self, ipl_name): #vers 2
        """Save ALL of an IPL's sections to a new file, not just inst."""
        built = self._build_ipl_text_lines(ipl_name)
        if built is None:
            return
        lines_out, total_written, section_order = built
        if not lines_out:
            QMessageBox.information(self, "Save IPL Data As...",
                f"No loaded data found for {ipl_name}.")
            return
        default_name = ipl_name if ipl_name.lower().endswith('.ipl') else ipl_name + '.ipl'
        path, _filter = QFileDialog.getSaveFileName(
            self, "Save IPL Data As...", default_name, "IPL files (*.ipl);;All files (*)")
        if not path:
            return
        try:
            _write_ipl_lines(path, [l + '\n' for l in lines_out])
        except Exception as e:
            QMessageBox.warning(self, "Save IPL Data As...", f"Failed to save: {e}")
            return
        self._set_status(
            f"Saved {total_written} entries across {len(section_order)} section(s) "
            f"from {ipl_name} to {path}")

    def _build_ipl_text_lines(self, ipl_name, target_game=None): #vers 4
        """Text IPL lines for all sections; (lines, count, sections) or None.
        target_game other than the loaded game writes inst only, in that game's layout."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return None
        import re
        from apps.methods.gta_dat_parser import GTAGame
        stem = getattr(self, '_ipl_display_to_stem', {}).get(ipl_name)
        entry = loader.available_ipls.get(stem) if stem else None
        raw_text = ""
        if entry is not None and entry.exists:
            try:
                with open(entry.abs_path, 'r', encoding='ascii', errors='ignore') as f:
                    raw_text = f.read()
            except Exception:
                raw_text = ""

        section_order = []
        for raw_line in raw_text.splitlines():
            line = raw_line.split("#")[0].strip()
            low = line.lower()
            if low and low != 'end' and re.match(r'^[a-z0-9_]{2,8}$', low) and ',' not in line:
                if low not in section_order:
                    section_order.append(low)
        if not section_order:
            section_order = ['inst', 'cull', 'zone', 'path', 'grge', 'enex', 'occl']

        STRUCTURED = {'inst', 'cull', 'zone', 'path', 'grge', 'enex', 'occl', 'auzo'}
        game = getattr(loader, 'game', None)
        if target_game and target_game != game:
            game = target_game
            section_order = ['inst']                # other sections differ per game
        elif stem:
            game = getattr(loader, 'ipl_layouts', {}).get(stem.lower(), game)   # SOL VC-format IPLs
        all_inst = getattr(self, '_all_instances', None) or []

        def _fmt(v):
            return f"{v:.6f}" if isinstance(v, float) else str(v)

        lines_out = []
        total_written = 0
        for section in section_order:
            if section not in STRUCTURED:
                # Not yet parsed into structured data - copy the
                # original section's raw text through unchanged.
                raw_section = self._extract_ipl_section_text(raw_text, section)
                if raw_section:
                    lines_out.extend(raw for _lineno, raw in raw_section)
                continue

            if section == 'inst':
                matching = [i for i in all_inst if i.source_ipl == ipl_name]
                lines_out.append("inst")
                for inst in matching:
                    if game in (GTAGame.SA, GTAGame.SOL):
                        lines_out.append(
                            f"{inst.model_id}, {inst.model_name}, {inst.interior}, "
                            f"{_fmt(inst.pos_x)}, {_fmt(inst.pos_y)}, {_fmt(inst.pos_z)}, "
                            f"{_fmt(inst.rot_x)}, {_fmt(inst.rot_y)}, {_fmt(inst.rot_z)}, {_fmt(inst.rot_w)}, "
                            f"{inst.lod_index}")
                    elif game == GTAGame.VC:
                        lines_out.append(
                            f"{inst.model_id}, {inst.model_name}, {inst.interior}, "
                            f"{_fmt(inst.pos_x)}, {_fmt(inst.pos_y)}, {_fmt(inst.pos_z)}, "
                            f"{_fmt(inst.scale_x)}, {_fmt(inst.scale_y)}, {_fmt(inst.scale_z)}, "
                            f"{_fmt(inst.rot_x)}, {_fmt(inst.rot_y)}, {_fmt(inst.rot_z)}, {_fmt(inst.rot_w)}")
                    else:   # GTA3
                        lines_out.append(
                            f"{inst.model_id}, {inst.model_name}, "
                            f"{_fmt(inst.pos_x)}, {_fmt(inst.pos_y)}, {_fmt(inst.pos_z)}, "
                            f"{_fmt(inst.scale_x)}, {_fmt(inst.scale_y)}, {_fmt(inst.scale_z)}, "
                            f"{_fmt(inst.rot_x)}, {_fmt(inst.rot_y)}, {_fmt(inst.rot_z)}, {_fmt(inst.rot_w)}")
                    total_written += 1
                lines_out.append("end")

            elif section == 'cull':
                matching = [c for c in getattr(loader, 'culls', []) if c.source_ipl == ipl_name]
                lines_out.append("cull")
                for c in matching:
                    lines_out.append(
                        f"{_fmt(c.center_x)}, {_fmt(c.center_y)}, {_fmt(c.center_z)}, "
                        f"{_fmt(c.x1)}, {_fmt(c.y1)}, {_fmt(c.z1)}, "
                        f"{_fmt(c.x2)}, {_fmt(c.y2)}, {_fmt(c.z2)}, "
                        f"{c.flags}, {c.wanted_level_drop}")
                    total_written += 1
                lines_out.append("end")

            elif section == 'zone':
                matching = [z for z in getattr(loader, 'zones', [])
                           if z.get('source_ipl') == ipl_name]
                lines_out.append("zone")
                for z in matching:
                    fields = [z['name'], str(z['type']),
                             _fmt(z['min_x']), _fmt(z['min_y']), _fmt(z['min_z']),
                             _fmt(z['max_x']), _fmt(z['max_y']), _fmt(z['max_z']),
                             str(z.get('island', 0))]
                    if z.get('text_key'):
                        fields.append(z['text_key'])
                    lines_out.append(', '.join(fields))
                    total_written += 1
                lines_out.append("end")

            elif section == 'path':
                from apps.methods.path_nodes import ipl_path_section_lines
                matching = [g for g in getattr(loader, 'paths', []) if g.source_ipl == ipl_name]
                lines_out.append("path")
                lines_out.extend(ipl_path_section_lines(matching))   # x16 on disk
                total_written += len(matching)
                lines_out.append("end")

            elif section == 'grge':
                matching = [g for g in getattr(loader, 'grges', []) if g.source_ipl == ipl_name]
                lines_out.append("grge")
                for g in matching:
                    lines_out.append(
                        f"{_fmt(g.x1)}, {_fmt(g.y1)}, {_fmt(g.z1)}, "
                        f"{_fmt(g.front_x)}, {_fmt(g.front_y)}, "
                        f"{_fmt(g.x2)}, {_fmt(g.y2)}, {_fmt(g.z2)}, "
                        f"{g.door_type}, {g.garage_type}, {g.name}")
                    total_written += 1
                lines_out.append("end")

            elif section == 'enex':
                matching = [e for e in getattr(loader, 'enexes', []) if e.source_ipl == ipl_name]
                lines_out.append("enex")
                for e in matching:
                    lines_out.append(
                        f"{_fmt(e.enter_x)}, {_fmt(e.enter_y)}, {_fmt(e.enter_z)}, {_fmt(e.enter_angle)}, "
                        f"{_fmt(e.size_x)}, {_fmt(e.size_y)}, {_fmt(e.size_z)}, "
                        f"{_fmt(e.exit_x)}, {_fmt(e.exit_y)}, {_fmt(e.exit_z)}, {_fmt(e.exit_angle)}, "
                        f"{e.target_interior}, {e.flags}, \"{e.name}\", "
                        f"{e.sky}, {e.num_peds_to_spawn}, {e.time_on}, {e.time_off}")
                    total_written += 1
                lines_out.append("end")

            elif section == 'occl':
                matching = [o for o in getattr(loader, 'occls', []) if o.source_ipl == ipl_name]
                lines_out.append("occl")
                for o in matching:
                    lines_out.append(
                        f"{_fmt(o.mid_x)}, {_fmt(o.mid_y)}, {_fmt(o.bottom_z)}, "
                        f"{_fmt(o.width_x)}, {_fmt(o.width_y)}, {_fmt(o.height)}, {_fmt(o.rotation)}")
                    total_written += 1
                lines_out.append("end")

            elif section == 'auzo':
                # Real, structured AUZO write-back (Aug 20 2026)
                # TODO; auzo should show it's enties in the IPL List Display window, renamed as Auzo Entris List.'
                matching = [a for a in getattr(loader, 'auzos', []) if a.source_ipl == ipl_name]
                lines_out.append("auzo")
                for a in matching:
                    if a.is_sphere:
                        lines_out.append(
                            f"{a.name}, {a.sound_id}, {a.switch}, "
                            f"{_fmt(a.x1)}, {_fmt(a.y1)}, {_fmt(a.z1)}, {_fmt(a.radius)}")
                    else:
                        lines_out.append(
                            f"{a.name}, {a.sound_id}, {a.switch}, "
                            f"{_fmt(a.x1)}, {_fmt(a.y1)}, {_fmt(a.z1)}, "
                            f"{_fmt(a.x2)}, {_fmt(a.y2)}, {_fmt(a.z2)}")
                    total_written += 1
                lines_out.append("end")

        return lines_out, total_written, section_order

    def _write_ipl_as_game(self, ipl_name, target, path): #vers 1
        """Write one IPL in another game's inst layout; returns (count, notes)."""
        loader = self._world_loader
        built = self._build_ipl_text_lines(ipl_name, target_game=target)
        if not built or not built[0]:
            raise ValueError(f"no loaded data for {ipl_name}")
        lines, total, _order = built
        notes = []
        src_game = getattr(loader, 'game', None)
        if target != src_game:
            other = sorted({c for c, attr in (('cull', 'culls'), ('zone', 'zones'), ('path', 'paths'),
                                              ('grge', 'grges'), ('enex', 'enexes'), ('occl', 'occls'),
                                              ('auzo', 'auzos'))
                            for o in getattr(loader, attr, []) or []
                            if (o.get('source_ipl') if isinstance(o, dict) else o.source_ipl) == ipl_name})
            if other:
                notes.append("sections not converted: " + ", ".join(other))
            insts = [i for i in loader.instances if i.source_ipl == ipl_name]
            if target in ('sa', 'sol') and any((i.scale_x, i.scale_y, i.scale_z) != (1.0, 1.0, 1.0) for i in insts):
                notes.append("scale dropped (SA inst has no scale)")
            if src_game in ('sa', 'sol') and target not in ('sa', 'sol') and any(i.lod_index >= 0 for i in insts):
                notes.append("LOD indices dropped")
        _write_ipl_lines(path, [l + '\n' for l in lines])
        return total, notes

    def _save_ipl_as_game(self, ipl_name): #vers 1
        """Save As an IPL converted to another game's format."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        keys = list(self._GAME_LABELS)
        labels = [self._GAME_LABELS[k] for k in keys]
        cur = getattr(loader, 'game', 'sa')
        pick, ok = QInputDialog.getItem(self, "Convert IPL", f"{ipl_name}: save in which game's format?",
                                        labels, keys.index(cur) if cur in keys else 0, False)
        if not ok:
            return
        target = keys[labels.index(pick)]
        default = os.path.splitext(ipl_name)[0] + f"_{target}.ipl"
        path, _f = QFileDialog.getSaveFileName(self, "Convert IPL", default, "IPL files (*.ipl);;All files (*)")
        if not path:
            return
        try:
            total, notes = self._write_ipl_as_game(ipl_name, target, path)
        except Exception as e:
            QMessageBox.warning(self, "Convert IPL", f"Failed: {e}")
            return
        msg = f"Wrote {total} instance(s) as {pick} to {os.path.basename(path)}"
        if notes:
            QMessageBox.information(self, "Convert IPL", msg + "\n\n" + "\n".join(notes))
        self._set_status(msg)

    def _rebase_ipls_dialog(self, names): #vers 1
        """Shift + rotate several IPLs together, optionally writing converted copies."""
        loader = getattr(self, '_world_loader', None)
        names = [n for n in names if n]
        if loader is None or not names:
            return
        insts = [i for i in loader.instances if i.source_ipl in set(names)]
        cx = sum(i.pos_x for i in insts) / len(insts) if insts else 0.0
        cy = sum(i.pos_y for i in insts) / len(insts) if insts else 0.0
        from PyQt6.QtWidgets import QFormLayout, QDialogButtonBox
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Rebase {len(names)} IPL(s)")
        form = QFormLayout(dlg)
        spins = {}
        for key, label, val, rng in (('dx', "Shift X:", 0.0, 100000), ('dy', "Shift Y:", 0.0, 100000),
                                     ('dz', "Shift Z:", 0.0, 10000), ('ang', "Rotate (deg, Z):", 0.0, 360),
                                     ('px', "Pivot X:", cx, 100000), ('py', "Pivot Y:", cy, 100000)):
            sp = QDoubleSpinBox()
            sp.setRange(-rng, rng)
            sp.setDecimals(3)
            sp.setValue(val)
            form.addRow(label, sp)
            spins[key] = sp
        fmt = QComboBox()
        fmt.addItem("Don't export (edit in place)", None)
        for k, v in self._GAME_LABELS.items():
            fmt.addItem(f"Export copies as {v}", k)
        form.addRow("After rebase:", fmt)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept)
        bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        v = {k: sp.value() for k, sp in spins.items()}
        for n in names:
            if v['ang']:
                self._rotate_ipl_coordinates(n, v['px'], v['py'], v['ang'])
            if v['dx'] or v['dy'] or v['dz']:
                self._shift_ipl_coordinates(n, v['dx'], v['dy'], v['dz'])
        target = fmt.currentData()
        if target is None:
            self._set_status(f"Rebased {len(names)} IPL(s) - Ctrl+S to save")
            return
        folder = QFileDialog.getExistingDirectory(self, "Folder for converted IPLs")
        if not folder:
            return
        notes, done = [], 0
        for n in names:
            try:
                out = os.path.join(folder, os.path.splitext(n)[0] + ".ipl")
                _t, nn = self._write_ipl_as_game(n, target, out)
                done += 1
                notes += [f"{n}: {x}" for x in nn]
            except Exception as e:
                notes.append(f"{n}: failed - {e}")
        QMessageBox.information(self, "Rebase", f"Exported {done} of {len(names)} IPL(s) to {folder}"
                                + ("\n\n" + "\n".join(notes[:30]) if notes else ""))

    def _binary_stream_source(self, ipl_name): #vers 1
        """(archive_path, entry_name) if ipl_name is a loaded IMG binary stream."""
        for streams in getattr(self, '_ipl_names_with_binary_stream', {}).values():
            for archive_path, entry_name in streams:
                if entry_name == ipl_name:
                    return archive_path, entry_name
        return None

    # -- change tracking, save all, save points (Sep 23 2026)
    def _init_change_tracking(self): #vers 2
        """Start the changed-IPL poll and the save point timer."""
        self._ipl_baseline = {}          # ipl name -> signature at load / last save
        self._inst_baseline = {}         # id(inst) -> state tuple at load / last save
        self._baseline_loader_id = None
        self._dirty_ipls = set()
        self._ide_pending = {}           # model_id -> (IDEObject, file path, edited raw line)
        self._last_savepoint_sig = None
        self._dirty_timer = QTimer(self)
        self._dirty_timer.timeout.connect(self._refresh_dirty_ipls)
        self._dirty_timer.start(1500)
        self._savepoint_timer = QTimer(self)
        self._savepoint_timer.timeout.connect(self._auto_savepoint)
        self._apply_savepoint_settings()

    def _apply_savepoint_settings(self): #vers 1
        """Start or stop automatic save points from map settings."""
        mins = max(1, int(self.map_settings.get('savepoints_interval_min') or 10))
        if self.map_settings.get('savepoints_enabled'):
            self._savepoint_timer.start(mins * 60000)
        else:
            self._savepoint_timer.stop()

    def _refresh_dirty_ipls(self): #vers 3
        """Compare each loaded IPL with its baseline; update highlights."""
        if not hasattr(self, '_ipl_baseline'):
            return
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            if self._dirty_ipls:
                self._dirty_ipls = set()
                self._apply_dirty_highlight()
            return
        from apps.components.Map_Editor.depends.map_changes import ipl_signatures, _inst_key
        if id(loader) != self._baseline_loader_id:
            self._baseline_loader_id = id(loader)
            self._ipl_baseline, self._inst_baseline = {}, {}
            self._ide_pending = {}
            self._map_checks_dlg = None
            self._session_saved_ipls, self._session_saved_ides = set(), set()
        sigs = ipl_signatures(loader)
        new = {n for n in sigs if n not in self._ipl_baseline}
        if new:
            for n in new:
                self._ipl_baseline[n] = sigs[n]
            src = self.__dict__.setdefault('_inst_baseline_src', {})
            for i in loader.instances:
                if i.source_ipl in new:
                    self._inst_baseline[id(i)] = _inst_key(i)
                    src[id(i)] = i.source_ipl
        empty = hash(())
        dirty = {n for n, b in self._ipl_baseline.items() if sigs.get(n, empty) != b}
        if dirty != self._dirty_ipls:
            self._dirty_ipls = dirty
            self._apply_dirty_highlight()

    def _inst_changed(self, inst): #vers 1
        """True if this instance was moved, edited or added since last save."""
        if inst.source_ipl not in getattr(self, '_dirty_ipls', ()):
            return False
        from apps.components.Map_Editor.depends.map_changes import _inst_key
        b = self._inst_baseline.get(id(inst))
        return b is None or b != _inst_key(inst)

    def _apply_dirty_highlight(self): #vers 2
        """Bold orange rows for changed IPLs and instances; status count."""
        dirty = getattr(self, '_dirty_ipls', set())
        table = getattr(self, '_ipl_sections_table', None)
        if table is not None:
            orange = QBrush(QColor(230, 140, 30))
            for r in range(table.rowCount()):
                first = table.item(r, 0)
                name = first.data(Qt.ItemDataRole.UserRole) if first is not None else None
                on = name in dirty
                for c in range(table.columnCount()):
                    cell = table.item(r, c)
                    if cell is None:
                        continue
                    f = cell.font()
                    f.setBold(on)
                    cell.setFont(f)
                    cell.setData(Qt.ItemDataRole.ForegroundRole, orange if on else None)
        view = getattr(self, '_instance_table', None)
        model = view.model() if view is not None else None
        if model is not None and model.rowCount() and model.columnCount():
            model.dataChanged.emit(model.index(0, 0),
                                   model.index(model.rowCount() - 1, model.columnCount() - 1))
        pend = getattr(self, '_ide_pending', {})
        if dirty or pend:
            self._set_status(f"Unsaved: {len(dirty)} IPL(s), {len(pend)} IDE object(s) - Ctrl+S to save")

    def _mark_ipl_saved(self, ipl_name): #vers 2
        """Reset the baseline for one IPL after a successful save."""
        loader = getattr(self, '_world_loader', None)
        if loader is None or not hasattr(self, '_ipl_baseline'):
            return
        from apps.components.Map_Editor.depends.map_changes import ipl_signatures, _inst_key
        self._ipl_baseline[ipl_name] = ipl_signatures(loader).get(ipl_name, hash(()))
        self.__dict__.setdefault('_session_saved_ipls', set()).add(ipl_name)
        for i in loader.instances:
            if i.source_ipl == ipl_name:
                self._inst_baseline[id(i)] = _inst_key(i)
        self._refresh_dirty_ipls()

    def _forget_ipl_baseline(self, names): #vers 1
        """Stop tracking IPLs that are being unloaded."""
        for n in names:
            getattr(self, '_ipl_baseline', {}).pop(n, None)
            getattr(self, '_dirty_ipls', set()).discard(n)

    def _confirm_discard_ipls(self, names, action, include_ide=False): #vers 2
        """Ask before dropping unsaved edits in these IPLs; True to go ahead."""
        self._refresh_dirty_ipls()
        hit = sorted(set(names) & getattr(self, '_dirty_ipls', set()))
        ides = list(getattr(self, '_ide_pending', {}).values()) if include_ide else []
        if not hit and not ides:
            return True
        lines = hit[:20] + (["..."] if len(hit) > 20 else [])
        if ides:
            lines.append(f"{len(ides)} edited IDE object(s)")
        r = QMessageBox.question(
            self, action, "Unsaved changes:\n\n" + "\n".join(lines) + "\n\nSave them first?",
            QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard
            | QMessageBox.StandardButton.Cancel)
        if r == QMessageBox.StandardButton.Cancel:
            return False
        if r == QMessageBox.StandardButton.Save:
            ok = all([self._save_ipl_in_place(n, confirm=False) for n in hit])
            return (self._save_pending_ide(ides) if ides else True) and ok
        return True

    def confirm_close(self, action="Map Workshop"): #vers 2
        """Unsaved-work reminder for close, quit or world reload; True to continue."""
        return self._confirm_discard_ipls(list(getattr(self, '_dirty_ipls', set())) or
                                          list(getattr(self, '_ipl_baseline', {})), action,
                                          include_ide=True)

    def _save_all_ipls(self): #vers 3
        """Save every changed IPL back to its own original file."""
        self._refresh_dirty_ipls()
        names = sorted(self._dirty_ipls)
        ides = list(self._ide_pending.values())
        if not names and not ides:
            self._set_status("No unsaved map changes")
            return True
        if QMessageBox.question(
                self, "Save Map Changes",
                f"Save {len(names)} changed IPL(s) and {len(ides)} edited IDE object(s) "
                f"back to their original files?\n\n"
                + "\n".join(names[:20]) + ("\n..." if len(names) > 20 else "")
                + "\n\nBackups are made first.") != QMessageBox.StandardButton.Yes:
            return False
        from apps.components.Map_Editor.depends.map_checks import check_lod_links
        loader = self._world_loader
        broken = [r for r in check_lod_links(loader.instances, self._lod_parent_ipl)
                  if 'warning' not in r[1] and r[0].source_ipl in names]
        if broken and QMessageBox.question(
                self, "Save Map Changes",
                f"{len(broken)} broken LOD link(s) in the IPLs being saved "
                f"(see Map Checks).\n\nSave anyway?") != QMessageBox.StandardButton.Yes:
            self._show_map_checks()
            return False
        failed = [n for n in names if not self._save_ipl_in_place(n, confirm=False)]
        if ides and not self._save_pending_ide(ides):
            failed.append("IDE edits")
        if failed:
            QMessageBox.warning(self, "Save Map Changes",
                                f"{len(names) - len(failed)} saved, {len(failed)} not saved:\n\n"
                                + "\n".join(failed))
        else:
            self._set_status(f"Saved {len(names)} IPL(s)")
        return not failed

    def _popup_under(self, btn, items): #vers 1
        """Show a menu of (label, callback) under a title bar button; None = separator."""
        menu = QMenu(self)
        for it in items:
            if it is None:
                menu.addSeparator()
            else:
                menu.addAction(it[0], it[1])
        menu.exec(btn.mapToGlobal(btn.rect().bottomLeft()))

    def _map_open_menu(self): #vers 1
        """Open: game .dat, game folder or a model."""
        self._popup_under(self.open_btn, [
            ("Load Game DAT...", lambda: self._load_game_dat_file()),
            ("Load Game Folder...", lambda: self._load_game_folder()),
            None,
            ("Open Model / COL / TXD...", self._open_file)])

    def _map_import_menu(self): #vers 1
        """Import: IPL / IDE into the loaded world, or a model."""
        self._popup_under(self.tb_import_btn, [
            ("Import IPL...", self._import_ipl_files),
            ("Import IDE...", self._import_ide_files),
            None,
            ("Import Model...", self._import_model)])

    def _map_export_menu(self): #vers 1
        """Export: selected IPL, changed IPLs to a folder, or the model."""
        self._popup_under(self.export_ojs_btn, [
            ("Export Selected IPL...", self._export_selected_ipl),
            ("Export Changed IPLs to Folder...", self._export_changed_ipls),
            None,
            ("Export Model...", self._export_model_menu)])

    def _map_loader_or_warn(self, title): #vers 1
        """Loaded world loader, or None after telling the user to load one."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            QMessageBox.information(self, title, "Load a game (.dat or folder) first.")
        return loader

    def _import_ipl_files(self): #vers 1
        """Add text IPL files to the loaded world and show them."""
        from apps.methods.gta_dat_parser import DATEntry
        loader = self._map_loader_or_warn("Import IPL")
        if loader is None:
            return
        paths, _ = QFileDialog.getOpenFileNames(self, "Import IPL", getattr(self, '_game_root', '') or '',
                                                "IPL files (*.ipl);;All files (*)")
        if not paths:
            return
        names = []
        for path in paths:
            stem = os.path.splitext(os.path.basename(path))[0].lower()
            if stem in loader.available_ipls:
                self._set_status(f"{os.path.basename(path)}: an IPL with that name is already in the world")
                continue
            loader.available_ipls[stem] = DATEntry('IPL', path, abs_path=path, exists=True, source_dat='import')
            name = os.path.basename(path)
            self._ipl_display_to_stem[name] = stem
            self._hidden_ipls.add(name)
            self._ipl_display_order.append(name)
            names.append(name)
        if not names:
            return
        self._rebuild_ipl_sections_rows()
        rows = [self._ipl_display_order.index(n) for n in names]
        self._load_selected_ipl_sections(rows)
        self._set_status(f"Imported {len(names)} IPL(s)")

    def _import_ide_files(self): #vers 1
        """Add IDE object definitions to the loaded world."""
        from apps.methods.gta_dat_parser import DATEntry
        loader = self._map_loader_or_warn("Import IDE")
        if loader is None:
            return
        paths, _ = QFileDialog.getOpenFileNames(self, "Import IDE", getattr(self, '_game_root', '') or '',
                                                "IDE files (*.ide);;All files (*)")
        if not paths:
            return
        before = len(loader.objects)
        for path in paths:
            loader._load_ide(DATEntry('IDE', path, abs_path=path, exists=True, source_dat='import'), 'import')
        self._populate_object_browser(loader)
        self._set_status(f"Imported {len(paths)} IDE(s): {len(loader.objects) - before} new object id(s)")

    def _export_selected_ipl(self): #vers 1
        """Save the IPL selected in IPL Sections (all sections) to a new file."""
        table = getattr(self, '_ipl_sections_table', None)
        row = table.currentRow() if table is not None else -1
        item = table.item(row, 0) if row >= 0 else None
        if item is None:
            QMessageBox.information(self, "Export IPL", "Select an IPL in IPL Sections first.")
            return
        self._save_ipl_data_as_full(item.data(Qt.ItemDataRole.UserRole))

    def _export_changed_ipls(self): #vers 1
        """Write every changed IPL (all sections) into a chosen folder; originals untouched."""
        if self._map_loader_or_warn("Export Changed IPLs") is None:
            return
        self._refresh_dirty_ipls()
        names = sorted(getattr(self, '_dirty_ipls', set()))
        if not names:
            self._set_status("No changed IPLs to export")
            return
        folder = QFileDialog.getExistingDirectory(self, "Export Changed IPLs to Folder")
        if not folder:
            return
        done, failed = 0, []
        for name in names:
            built = self._build_ipl_text_lines(name)
            if not built or not built[0]:
                failed.append(name)
                continue
            _write_ipl_lines(os.path.join(folder, os.path.splitext(name)[0] + ".ipl"),
                             [l + '\n' for l in built[0]])
            done += 1
        msg = f"Exported {done} changed IPL(s) to {folder}"
        if failed:
            QMessageBox.warning(self, "Export Changed IPLs", msg + "\n\nNot exported: " + ", ".join(failed))
        self._set_status(msg)

    def _save_map_or_model(self): #vers 2
        """Ctrl+S / Save: changed IPLs first, otherwise the open model."""
        self._refresh_dirty_ipls()
        if getattr(self, '_dirty_ipls', None) or getattr(self, '_ide_pending', None):
            self._save_all_ipls()
        else:
            self._save_file()

    def _apply_instance_fields(self, inst, values): #vers 1
        """Set IPL instance fields from the object editor (undoable)."""
        old = {k: getattr(inst, k) for k in values}
        if old == values:
            return
        model_changed = old.get('model_id') != values.get('model_id') or \
            old.get('model_name') != values.get('model_name')

        def _set(v):
            for k, x in v.items():
                setattr(inst, k, x)
            if model_changed:
                self._apply_ipl_visibility_filter(auto_fit=False)
            else:
                self._on_instance_edited(inst)

        _set(values)
        self._push_map_undo(lambda: _set(old), lambda: _set(values),
                            f"Edit {inst.model_name} IPL line")

    def _ide_raw_line(self, obj): #vers 2
        """Current IDE line text for obj: pending edit, else the file line; None if not found."""
        pend = self._ide_pending.get(obj.model_id)
        if pend is not None:
            return pend[2]
        if self._is_stories_obj(obj):
            return self._stories_ide_line(obj)
        p = self._ide_abs_path(obj)
        if p is None:
            return None
        with open(p, 'rb') as f:
            lines = f.read().decode('latin-1').splitlines()
        return lines[obj.line_no - 1] if 0 < obj.line_no <= len(lines) else None

    def _parse_ide_raw(self, obj, text): #vers 1
        """Parse one edited IDE line in obj's section; returns the new IDEObject or None."""
        from apps.methods.gta_dat_parser import IDEParser
        parser = IDEParser(getattr(self._world_loader, 'game', 'sa'))
        body = text.split('#')[0].strip()
        return parser._parse_line(obj.section, body, obj.source_ide, obj.line_no) if body else None

    def _check_ide_raw(self, obj, text): #vers 2
        """Error text if the edited line doesn't parse or changes the model ID, else ''."""
        new = self._parse_ide_raw(obj, text)
        if new is None:
            return f"doesn't parse as a '{obj.section}' line"
        if new.model_id != obj.model_id:
            return "the model ID can't be changed here"
        if self._is_stories_obj(obj) and new.model_name.lower() != obj.model_name.lower():
            return "GAME.DTZ model names are hashes - the name can't be changed"
        return ''

    def _apply_ide_raw(self, obj, text): #vers 1
        """Replace obj's IDE line (any section) - fields updated, written on Ctrl+S; undoable."""
        new = self._parse_ide_raw(obj, text)
        path = self._ide_abs_path(obj) if obj.model_id not in self._ide_pending else self._ide_pending[obj.model_id][1]
        old_fields = (obj.model_name, obj.txd_name, dict(obj.extra))
        old_pend = self._ide_pending.get(obj.model_id)
        new_fields = (new.model_name, new.txd_name, dict(new.extra))

        def _set(fields, pend):
            obj.model_name, obj.txd_name = fields[0], fields[1]
            obj.extra.clear()
            obj.extra.update(fields[2])
            if pend is None:
                self._ide_pending.pop(obj.model_id, None)
            else:
                self._ide_pending[obj.model_id] = pend
            self._refresh_dirty_ipls()
            self._apply_dirty_highlight()

        _set(new_fields, (obj, path, text))
        self._push_map_undo(lambda: _set(old_fields, old_pend),
                            lambda: _set(new_fields, (obj, path, text)), f"Edit IDE {obj.model_name}")

    def _ide_abs_path(self, obj): #vers 2
        """Full path of an IDE object's source file (matched by name and line ID)."""
        if self._is_stories_obj(obj):
            return obj.source_ide                       # GAME.DTZ model info
        loader = getattr(self, '_world_loader', None)
        cands = [p for _ph, et, p, ok in getattr(loader, 'load_log', []) or []
                 if et == "IDE" and ok and os.path.basename(p) == obj.source_ide]
        for p in cands:
            try:
                with open(p, 'rb') as f:
                    lines = f.read().decode('latin-1').splitlines()
                if 0 < obj.line_no <= len(lines) and \
                        lines[obj.line_no - 1].split(',')[0].strip() == str(obj.model_id):
                    return p
            except OSError:
                continue
        return None

    def _write_stories_instances(self, loader, ipl_name, matching, confirm): #vers 1
        """Write moved/rotated LCS/VCS pool entities back into GAME.DTZ."""
        dtz, offs = loader.stories_dtz, loader.stories_offsets
        base = sum(1 for k, v in getattr(self, '_inst_baseline_src', {}).items() if v == ipl_name)
        lines = [i.line_no for i in matching]
        if len(set(lines)) != len(lines) or any(not 0 <= n < len(offs) for n in lines) \
                or (base and len(matching) != base):
            QMessageBox.warning(self, "Save IPL",
                f"{ipl_name}: GAME.DTZ pools are fixed - instances can be moved, rotated "
                f"or re-modelled, not added or deleted. Not saved.")
            return False
        if confirm and QMessageBox.question(self, "Save IPL",
                f"Write {len(matching)} {ipl_name[:-4]} entities into "
                f"{os.path.basename(dtz.path)}?\n(A backup is made first.)") != QMessageBox.StandardButton.Yes:
            return False
        try:
            for i in matching:
                dtz.set_instance(offs[i.line_no], i.model_id, (i.pos_x, i.pos_y, i.pos_z),
                                 (i.rot_x, i.rot_y, i.rot_z, i.rot_w), i.interior)
            dtz.save(label="Map Workshop")
        except Exception as e:
            QMessageBox.warning(self, "Save IPL", f"Failed to save {os.path.basename(dtz.path)}: {e}")
            return False
        self._set_status(f"Saved {len(matching)} {ipl_name[:-4]} into {os.path.basename(dtz.path)}")
        return True

    def _stories_ide_line(self, obj): #vers 1
        """IDE-style line for an LCS/VCS GAME.DTZ object, or None if not editable."""
        if obj.section not in ('objs', 'tobj'):
            return None
        ex = obj.extra
        n = ex.get('mesh_count', 1)
        dists = [ex.get(k, ex.get('draw_dist', 0.0)) for k in ('draw_dist', 'draw_dist2', 'draw_dist3')][:n]
        parts = [str(obj.model_id), obj.model_name, obj.txd_name, str(n)] + \
                [f"{d:g}" for d in dists] + [str(ex.get('flags', 0))]
        if obj.section == 'tobj':
            parts += [str(ex.get('time_on', 0)), str(ex.get('time_off', 24))]
        return ', '.join(parts)

    def _is_stories_obj(self, obj): #vers 1
        """True when obj came from the loaded LCS/VCS GAME.DTZ."""
        dtz = getattr(getattr(self, '_world_loader', None), 'stories_dtz', None)
        return dtz is not None and obj.source_ide == dtz.path

    def _save_pending_ide(self, entries): #vers 4
        """Write pending (obj, path, raw line) IDE edits; True if all written."""
        from apps.components.Map_Editor.depends.map_changes import write_ide_raw_lines
        by_file, missing = {}, []
        dtz = getattr(getattr(self, '_world_loader', None), 'stories_dtz', None)
        dtz_rows = [(o, r) for o, p, r in entries if dtz is not None and p == dtz.path]
        entries = [e for e in entries if not (dtz is not None and e[1] == dtz.path)]
        for obj, path, raw in entries:
            if path is None:
                missing.append(f"{obj.model_name} ({obj.source_ide})")
            else:
                by_file.setdefault(path, []).append((obj.line_no, obj.model_id, raw))
        errors = []
        if dtz_rows:
            try:
                for obj, _raw in dtz_rows:
                    dtz.set_object(obj.model_id, obj.extra, obj.txd_name)
                dtz.save(label="Map Workshop")
                for obj, _raw in dtz_rows:
                    self._ide_pending.pop(obj.model_id, None)
            except Exception as e:
                errors.append(f"{os.path.basename(dtz.path)}: {e}")
        for p, rows in by_file.items():
            try:
                write_ide_raw_lines(p, rows)
                self.__dict__.setdefault('_session_saved_ides', set()).add(p)
                for _ln, mid, _raw in rows:
                    self._ide_pending.pop(mid, None)
            except Exception as e:
                errors.append(f"{os.path.basename(p)}: {e}")
        if missing or errors:
            QMessageBox.warning(self, "Save IDE", "Not saved:\n\n" + "\n".join(missing + errors))
        self._refresh_dirty_ipls()
        self._apply_dirty_highlight()
        return not missing and not errors

    def _show_script_placements(self): #vers 1
        """Scan main.scm for script-placed objects, pickups and car generators."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            QMessageBox.information(self, "Script Placements", "Load a world first.")
            return
        root = getattr(self, '_game_root', '') or ''
        path = os.path.join(root, 'data', 'main.scm')
        if not os.path.isfile(path):
            path, _f = QFileDialog.getOpenFileName(self, "Open main.scm", root, "SCM (*.scm);;All files (*)")
            if not path:
                return
        from apps.methods.scm_placements import scan_placements
        from apps.components.Map_Editor.depends.map_integration import ScriptPlacementsDialog
        with open(path, 'rb') as f:
            data = f.read()
        self._set_status(f"Scanning {os.path.basename(path)}...")
        QApplication.processEvents()
        hits = scan_placements(data, getattr(loader, 'game', 'sa'))
        dlg = ScriptPlacementsDialog(self, path, hits)
        dlg.show()
        self._script_dlg = dlg
        self._set_status(f"{len(hits)} script placement(s) in {os.path.basename(path)}")

    def _show_engine_load_log(self): #vers 1
        """Replay the loaded world's DAT / IDE / IPL / stream load order as a log."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            QMessageBox.information(self, "Engine Load Log", "Load a world first.")
            return
        from apps.components.Map_Editor.depends.map_integration import build_load_log, EngineLoadLogDialog
        streams = {p: [en for _a, en in lst] for p, lst in getattr(self, '_ipl_names_with_binary_stream', {}).items()}
        dlg = EngineLoadLogDialog(self, build_load_log(loader, self._lod_parent_ipl, streams))
        dlg.show()
        self._load_log_dlg = dlg

    def _regen_changed_radar_tiles(self): #vers 1
        """Re-render only radar tiles covering old or new positions of unsaved edits."""
        loader = getattr(self, '_world_loader', None)
        self._refresh_dirty_ipls()
        dirty = getattr(self, '_dirty_ipls', set())
        if loader is None or not dirty:
            self._set_status("No unsaved edits - nothing to regenerate")
            return
        out = self.map_settings.get('radar_tiles_output_dir')
        if not out:
            self._on_generate_radar_tiles_clicked()
            return
        from apps.methods.gta_dat_parser import compute_radar_grid, RADAR_GRID_PRESETS
        from apps.components.Map_Editor.depends.map_integration import tiles_touching
        pts = [(i.pos_x, i.pos_y) for i in loader.instances if i.source_ipl in dirty]
        src = getattr(self, '_inst_baseline_src', {})
        pts += [(k[3], k[4]) for key, k in self._inst_baseline.items() if src.get(key) in dirty]
        preset = RADAR_GRID_PRESETS.get(getattr(loader, 'game', 'sa'), RADAR_GRID_PRESETS['sa'])
        idx = tiles_touching(compute_radar_grid(**preset), pts)
        if not idx:
            self._set_status("Edits are outside the radar grid")
            return
        self._generate_radar_tiles(out, only=idx)

    def _show_map_diff(self): #vers 1
        """Changed / added / removed objects per IPL vs last save or a save point."""
        if getattr(self, '_world_loader', None) is None:
            return
        from apps.components.Map_Editor.depends.map_workflow import MapDiffDialog
        dlg = MapDiffDialog(self)
        dlg.show()
        self._diff_dlg = dlg

    def _export_mod_package(self): #vers 2
        """Changed and saved-this-session IPL / IDE files, as game-relative copies + readme + zip."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        import time
        from apps.methods.gta_dat_parser import write_binary_ipl_inst_only, detect_ipl_format
        from apps.components.Map_Editor.depends.map_changes import apply_ide_raw_lines
        from apps.components.Map_Editor.depends.map_workflow import export_mod_package, package_name
        self._refresh_dirty_ipls()
        ipls = sorted(set(self._dirty_ipls) | getattr(self, '_session_saved_ipls', set()))
        root = getattr(self, '_game_root', '') or ''
        files, notes = {}, []

        def rel(p):
            try:
                return os.path.relpath(p, root) if root else os.path.basename(p)
            except ValueError:
                return os.path.basename(p)

        for n in ipls:
            insts = [i for i in loader.instances if i.source_ipl == n]
            src = self._binary_stream_source(n)
            if src is not None:
                files[os.path.join('img', os.path.basename(src[0]), n)] = write_binary_ipl_inst_only(insts)
                notes.append(f"img/{os.path.basename(src[0])}/{n}: import into {os.path.basename(src[0])}")
                continue
            stem = self._ipl_display_to_stem.get(n)
            entry = loader.available_ipls.get(stem) if stem else None
            if entry is None or not entry.exists:
                notes.append(f"{n}: no source file, skipped")
                continue
            with open(entry.abs_path, 'rb') as f:
                head = f.read(64)
            if detect_ipl_format(head) == 'binary':
                files[rel(entry.abs_path)] = write_binary_ipl_inst_only(insts)
            else:
                built = self._build_ipl_text_lines(n)
                files[rel(entry.abs_path)] = ('\r\n'.join(built[0]) + '\r\n').encode('latin-1', 'replace')
        ide_files = set(getattr(self, '_session_saved_ides', set()))
        pend_by = {}
        for o, p, raw in self._ide_pending.values():
            if p:
                ide_files.add(p)
                pend_by.setdefault(p, []).append((o.line_no, o.model_id, raw))
        for p in sorted(ide_files):
            files[rel(p)] = apply_ide_raw_lines(p, pend_by.get(p, []))
        if not files:
            QMessageBox.information(self, "Export Mod Package", "No changed or saved map files this session.")
            return
        folder = QFileDialog.getExistingDirectory(self, "Folder for the mod package")
        if not folder:
            return
        game = getattr(loader, 'game', 'game')
        readme = [f"Map mod package - {self._GAME_LABELS.get(game, game)}",
                  f"Created {time.strftime('%Y-%m-%d %H:%M')} with IMG Factory 1.6 Map Workshop", "",
                  "Copy the folders over your game folder (back it up first).",
                  "Files under img/<archive>/ go inside that IMG archive - import them with IMG Factory.", "",
                  "Files:"] + [f"  {k}" for k in sorted(files)] + ([""] + notes if notes else [])
        try:
            out = export_mod_package(folder, package_name(game), files, readme)
        except OSError as e:
            QMessageBox.warning(self, "Export Mod Package", f"Failed: {e}")
            return
        QMessageBox.information(self, "Export Mod Package", f"{len(files)} file(s) written to\n{out}\n(and {out}.zip)")

    def _show_map_checks(self): #vers 1
        """LOD links, IDE ID conflicts, missing assets and limits in one dialog."""
        if getattr(self, '_world_loader', None) is None:
            QMessageBox.information(self, "Map Checks", "Load a world first.")
            return
        from apps.components.Map_Editor.depends.map_checks import MapChecksDialog
        dlg = getattr(self, '_map_checks_dlg', None)
        if dlg is None:
            dlg = self._map_checks_dlg = MapChecksDialog(self)
        dlg.show()
        dlg.raise_()
        dlg._run_lod()

    def _savepoint_dir(self): #vers 1
        """Save point folder for the loaded world (per game root / dat)."""
        import hashlib
        key = getattr(self, '_loaded_dat_path', '') or getattr(self, '_game_root', '') or 'world'
        sub = hashlib.md5(os.path.abspath(key).encode('utf-8')).hexdigest()[:12]
        return str(_model_workshop_config_dir() / 'savepoints' / sub)

    def _create_savepoint(self, label="Manual"): #vers 2
        """Snapshot every changed IPL's data to a save point file."""
        loader = getattr(self, '_world_loader', None)
        self._refresh_dirty_ipls()
        names = sorted(getattr(self, '_dirty_ipls', set()))
        if loader is None or not (names or self._ide_pending):
            self._set_status("Save point: no unsaved changes to record")
            return None
        from apps.components.Map_Editor.depends.map_changes import collect_ipl_state, write_savepoint
        meta = {'game_root': getattr(self, '_game_root', ''),
                'dat_path': getattr(self, '_loaded_dat_path', ''),
                'game': getattr(loader, 'game', ''),
                'ide': [{'model_id': o.model_id, 'raw': raw} for o, _p, raw in self._ide_pending.values()]}
        try:
            path = write_savepoint(self._savepoint_dir(), label, meta, collect_ipl_state(loader, names))
        except Exception as e:
            QMessageBox.warning(self, "Save Point", f"Couldn't write save point: {e}")
            return None
        self._set_status(f"Save point: {len(names)} IPL(s) -> {os.path.basename(path)}")
        return path

    def _auto_savepoint(self): #vers 1
        """Timer: save point only if edits changed since the last one."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        from apps.components.Map_Editor.depends.map_changes import ipl_signatures, prune_savepoints
        self._refresh_dirty_ipls()
        if not self._dirty_ipls:
            return
        sigs = ipl_signatures(loader)
        sig = hash(tuple(sorted((n, sigs.get(n)) for n in self._dirty_ipls)))
        if sig == self._last_savepoint_sig:
            return
        if self._create_savepoint("Auto"):
            self._last_savepoint_sig = sig
            prune_savepoints(self._savepoint_dir(), int(self.map_settings.get('savepoints_keep') or 20))

    def _restore_savepoint_dialog(self): #vers 2
        """Pick a save point and load its IPL data back in (undoable)."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            QMessageBox.information(self, "Restore Save Point", "Load a world first.")
            return
        from datetime import datetime
        from apps.components.Map_Editor.depends.map_changes import (
            list_savepoints, read_savepoint, state_from_json, collect_ipl_state, replace_ipl_state)
        points = list_savepoints(self._savepoint_dir())
        if not points:
            QMessageBox.information(self, "Restore Save Point", "No save points for this world yet.")
            return
        labels = [f"{datetime.fromtimestamp(p['created']):%Y-%m-%d %H:%M:%S}  {p['label']}  "
                  f"({len(p['ipls'])} IPL: {', '.join(p['ipls'][:4])}{'...' if len(p['ipls']) > 4 else ''})"
                  for p in points]
        pick, ok = QInputDialog.getItem(self, "Restore Save Point", "Save point:", labels, 0, False)
        if not ok:
            return
        chosen = points[labels.index(pick)]
        try:
            body = read_savepoint(chosen['path'])
            snap = state_from_json(body.get('ipls', {}))
            ide_edits = body.get('meta', {}).get('ide', [])
        except Exception as e:
            QMessageBox.warning(self, "Restore Save Point", f"Couldn't read save point: {e}")
            return
        loaded = {i.source_ipl for i in loader.instances} | set(getattr(self, '_ipl_baseline', {}))
        missing = sorted(n for n in snap if n not in loaded)
        snap = {n: v for n, v in snap.items() if n in loaded}
        for rec in ide_edits:
            obj = loader.get_object(rec.get('model_id'))
            if obj is not None and not self._check_ide_raw(obj, rec.get('raw', '')):
                self._apply_ide_raw(obj, rec['raw'])
        if not snap and ide_edits:
            self._set_status(f"Restored {len(ide_edits)} IDE edit(s) from save point")
            return
        if not snap:
            QMessageBox.warning(self, "Restore Save Point",
                                "None of this save point's IPLs are loaded:\n\n" + "\n".join(missing))
            return
        before = collect_ipl_state(loader, list(snap))
        before = {n: {c: list(v) for c, v in cats.items()} for n, cats in before.items()}

        def _apply(state):
            replace_ipl_state(loader, state)
            self._all_instances = list(loader.instances)
            self._populate_object_browser(loader)
            self._populate_instance_list(loader)
            self._apply_ipl_visibility_filter(clear_display_lists=False)
            self._refresh_dirty_ipls()

        _apply(snap)
        self._push_map_undo(lambda: _apply(before), lambda: _apply(snap),
                            f"Restore save point ({len(snap)} IPL)")
        msg = f"Restored {len(snap)} IPL(s) from save point - not written to game files until you save"
        if missing:
            msg += f"; skipped {len(missing)} not loaded"
        self._set_status(msg)

    def _save_ipl_in_place(self, ipl_name, confirm=True): #vers 2
        """Save edits back to the original IPL: text file, loose binary, or IMG stream. Returns True on success."""
        ok = self._write_ipl_in_place(ipl_name, confirm)
        if ok:
            self._mark_ipl_saved(ipl_name)
        return ok

    def _write_ipl_in_place(self, ipl_name, confirm): #vers 3
        """Worker for _save_ipl_in_place; False on refusal, cancel or error."""
        import struct as _struct
        from apps.methods.gta_dat_parser import write_binary_ipl_inst_only, detect_ipl_format
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return False
        title = "Save IPL"
        matching = [i for i in (getattr(self, '_all_instances', None) or []) if i.source_ipl == ipl_name]

        if ipl_name.startswith('sectors_'):                        # LCS/VCS level sectors (.LVZ/.IMG)
            QMessageBox.information(self, title, f"{ipl_name}: level sector pieces are read only.")
            return False
        if getattr(loader, 'stories_dtz', None) is not None:     # LCS/VCS: pools in GAME.DTZ
            return self._write_stories_instances(loader, ipl_name, matching, confirm)
        if getattr(loader, 'bully', False):                       # Bully .ipb inside World.img
            QMessageBox.information(self, title, f"{ipl_name}: saving Bully .ipb placements is not supported yet.")
            return False

        # binary stream inside an IMG archive (SA)
        src = self._binary_stream_source(ipl_name)
        if src is not None:
            archive_path, entry_name = src
            model_cache = getattr(self, '_model_cache', None)
            img = model_cache._opened_img_files.get(archive_path) if model_cache else None
            if img is None:
                from apps.methods.img_core_classes import IMGFile
                img = IMGFile(archive_path)
                if not img.open():
                    QMessageBox.warning(self, title, f"Couldn't open {archive_path}")
                    return False
            entry = next((e for e in img.entries if getattr(e, 'name', '') == entry_name), None)
            if entry is None:
                QMessageBox.warning(self, title, f"{entry_name} not found in {archive_path}")
                return False
            old = img.read_entry_data(entry)
            if len(old) >= 24 and _struct.unpack_from("<I", old, 20)[0]:
                QMessageBox.warning(self, title,
                    f"{entry_name} has parked cars; the binary writer only writes instances. Not saved.")
                return False
            if confirm and QMessageBox.question(self, title,
                    f"Write {len(matching)} instance(s) back into {entry_name} "
                    f"inside {os.path.basename(archive_path)}?\n(The archive is backed up first.)"
                    ) != QMessageBox.StandardButton.Yes:
                return False
            if not img.add_entry(entry_name, write_binary_ipl_inst_only(matching)):
                QMessageBox.warning(self, title, f"Couldn't write {entry_name}: "
                                    f"{getattr(img, 'last_error', 'archive refused the data')}")
                return False
            self._set_status(f"Saved {len(matching)} instance(s) into {entry_name} ({os.path.basename(archive_path)})")
            return True

        stem = getattr(self, '_ipl_display_to_stem', {}).get(ipl_name)
        entry = loader.available_ipls.get(stem) if stem else None
        if entry is None or not entry.exists:
            QMessageBox.warning(self, title, f"No file on disk for {ipl_name}. Use Save IPL Data As...")
            return False
        path = entry.abs_path
        with open(path, 'rb') as f:
            head = f.read(64)

        # loose binary IPL file
        if detect_ipl_format(head) == 'binary':
            with open(path, 'rb') as f:
                old = f.read()
            if len(old) >= 24 and _struct.unpack_from("<I", old, 20)[0]:
                QMessageBox.warning(self, title,
                    f"{ipl_name} has parked cars; the binary writer only writes instances. Not saved.")
                return False
            if confirm and QMessageBox.question(self, title,
                    f"Overwrite {path} with {len(matching)} instance(s)?\n(A backup is made first.)"
                    ) != QMessageBox.StandardButton.Yes:
                return False
            from apps.methods.file_backup import safe_write_bytes
            try:
                safe_write_bytes(path, write_binary_ipl_inst_only(matching))
            except Exception as e:
                QMessageBox.warning(self, title, f"Failed to save: {e}")
                return False
            self._set_status(f"Saved {len(matching)} instance(s) to {path}")
            return True

        # text IPL
        built = self._build_ipl_text_lines(ipl_name)
        if not built or not built[0]:
            QMessageBox.information(self, title, f"No loaded data found for {ipl_name}.")
            return False
        lines_out, total_written, section_order = built
        if confirm and QMessageBox.question(self, title,
                f"Overwrite {path} ({total_written} entries, {len(section_order)} section(s))?\n"
                f"(A backup is made first.)") != QMessageBox.StandardButton.Yes:
            return False
        try:
            _write_ipl_lines(path, [l + '\n' for l in lines_out])
        except Exception as e:
            QMessageBox.warning(self, title, f"Failed to save: {e}")
            return False
        self._set_status(f"Saved {total_written} entries to {path}")
        return True

    def _verify_binary_ipl_parser(self, archive_path, entry_name): #vers 1
        """Diagnostic dry run - reads and parses one binary IPL entry
        exactly like a real load does, but never touches self._all_
        instances or the world view."""
        loader = getattr(self, '_world_loader', None)
        model_cache = getattr(self, '_model_cache', None)
        try:
            from apps.methods.img_core_classes import IMGFile
            img = model_cache._opened_img_files.get(archive_path) if model_cache else None
            if img is None:
                img = IMGFile(archive_path)
                if not img.open():
                    QMessageBox.warning(self, "Verify Binary Parser",
                        f"Could not open {archive_path}.")
                    return
                if model_cache is not None:
                    model_cache._opened_img_files[archive_path] = img
            entry = next((e for e in img.entries if getattr(e, 'name', '') == entry_name), None)
            if entry is None:
                QMessageBox.warning(self, "Verify Binary Parser",
                    f"{entry_name} not found in {archive_path}.")
                return
            data = img.read_entry_data(entry)
        except Exception as e:
            QMessageBox.warning(self, "Verify Binary Parser", f"Failed to read: {e}")
            return

        from apps.methods.gta_dat_parser import BinaryIPLParser, detect_ipl_format
        fmt = detect_ipl_format(data[:64])
        game = getattr(loader, 'game', None) if loader else None
        parser = BinaryIPLParser(game=game) if game else BinaryIPLParser()
        success = parser.parse(data, source_name=entry_name)

        lines = [
            f"File: {entry_name}",
            f"Archive: {archive_path}",
            f"Size: {len(data)} bytes",
            f"Detected format: {fmt}",
            f"Parse result: {'SUCCESS' if success else 'FAILED'}",
            f"Instances found: {len(parser.instances)}",
        ]
        if parser.stats.errors:
            lines.append(f"Errors ({len(parser.stats.errors)}):")
            lines.extend(f"  {e}" for e in parser.stats.errors[:5])
        if parser.stats.warnings:
            lines.append(f"Warnings ({len(parser.stats.warnings)}):")
            lines.extend(f"  {w}" for w in parser.stats.warnings[:5])
        if parser.instances:
            objects = getattr(loader, 'objects', {}) if loader else {}
            lines.append("First instances:")
            for inst in parser.instances[:5]:
                obj = objects.get(inst.model_id)
                name = obj.model_name if obj is not None else "(unresolved)"
                lines.append(
                    f"  ID {inst.model_id} ({name}) @ "
                    f"({inst.pos_x:.2f}, {inst.pos_y:.2f}, {inst.pos_z:.2f})")

        QMessageBox.information(self, "Verify Binary Parser", "\n".join(lines))

    def _insert_stream_into_display_order(self, parent_display_name, entry_name): #vers 1
        """Insert a newly-loaded binary stream's row immediately after
        its parent text IPL - and after any of that parent's already-
        inserted sibling streams, so multiple loaded streams for the
        same parent stay in a stable relative order rather than being
        inserted in reverse - instead of appending to the end of the
        whole list (Aug 16 2026."""
        order = self._ipl_display_order
        if entry_name in order:
            return
        try:
            parent_idx = order.index(parent_display_name)
        except ValueError:
            order.append(entry_name)
            return
        insert_at = parent_idx + 1
        known_siblings = {name for _ap, name in
                          self._ipl_names_with_binary_stream.get(parent_display_name, [])}
        while insert_at < len(order) and order[insert_at] in known_siblings:
            insert_at += 1
        order.insert(insert_at, entry_name)

    def _load_binary_ipl_stream(self, archive_path, entry_name): #vers 2
        """Actually load one binary IPL stream entry's instance data"""
        if entry_name in getattr(self, '_loaded_binary_ipls', set()):
            self._set_status(f"{entry_name} is already loaded")
            return
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        self._set_status(f"Reading {entry_name} from {os.path.basename(archive_path)}...")
        QApplication.processEvents()
        try:
            # Reuse the model_cache's already-opened archive instead of
            # always creating a fresh IMGFile here (Aug 1 2026)
            model_cache = getattr(self, '_model_cache', None)
            img = None
            if model_cache is not None:
                img = model_cache._opened_img_files.get(archive_path)
            if img is None:
                from apps.methods.img_core_classes import IMGFile
                img = IMGFile(archive_path)
                if not img.open():
                    self._set_status(f"Could not open {archive_path} to load {entry_name}")
                    return
                if model_cache is not None:
                    model_cache._opened_img_files[archive_path] = img
            entry = next((e for e in img.entries if getattr(e, 'name', '') == entry_name), None)
            if entry is None:
                self._set_status(f"{entry_name} not found in {archive_path}")
                return
            data = img.read_entry_data(entry)
        except Exception as e:
            self._set_status(f"Failed to read {entry_name}: {e}")
            return

        self._set_status(f"Parsing {entry_name} ({len(data)} bytes)...")
        QApplication.processEvents()
        from apps.methods.gta_dat_parser import BinaryIPLParser, WPLParser
        game = getattr(loader, 'game', None)
        if entry_name.lower().endswith('.wpl'):      # GTA IV streamed placements
            parser = WPLParser(getattr(loader, '_iv_hash_names', {}))
            ok = parser.parse_bytes(data, entry_name)
        else:
            parser = BinaryIPLParser(game=game) if game else BinaryIPLParser()
            ok = parser.parse(data, source_name=entry_name)
        if not ok:
            errs = "; ".join(parser.stats.errors[:3])
            self._set_status(f"Failed to parse {entry_name}: {errs}")
            return

        objects = getattr(loader, 'objects', {})
        resolved_count = 0
        for inst in parser.instances:
            obj = objects.get(inst.model_id)
            if obj is not None:
                inst.model_name = obj.model_name
                resolved_count += 1
        if isinstance(parser, WPLParser):
            loader.grges += parser.grges
            loader.culls += parser.culls

        # Merge into loader.instances (Aug 16 2026)
        loader.instances.extend(parser.instances)
        self._all_instances = list(loader.instances)

        if entry_name not in self._ipl_display_to_stem:
            stem = f"img:{os.path.basename(archive_path)}:{entry_name}".lower()
            self._ipl_display_to_stem[entry_name] = stem

        # Insert right after this stream's parent text IPL (Aug 16 2026)
        parent_display_name = None
        for parent, streams in self._ipl_names_with_binary_stream.items():
            if (archive_path, entry_name) in streams:
                parent_display_name = parent
                break

        if parent_display_name is not None:
            self._insert_stream_into_display_order(parent_display_name, entry_name)

        elif entry_name not in self._ipl_display_order:
            self._ipl_display_order.append(entry_name)
        self._hidden_ipls.discard(entry_name)
        self._binary_ipl_names.discard(entry_name)   # now genuinely loaded, not just listed
        self._loaded_binary_ipls = getattr(self, '_loaded_binary_ipls', set())
        self._loaded_binary_ipls.add(entry_name)

        self._set_status(
            f"Loaded {entry_name}: {len(parser.instances)} instances "
            f"({resolved_count} model names resolved)")
        self._rebuild_ipl_sections_rows()

        # auto_fit=False, clear_display_lists=False (Aug 1 2026)
        self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=False)
        # Explicit refresh (Aug 1 2026)

        self._refresh_ipl_inst_file_panel()

    def _populate_wpl_table(self, table, loader, entry, display_name): #vers 1
        """GTA IV .wpl INST rows; file read here if not loaded yet."""
        from apps.methods.gta_dat_parser import WPLParser
        if getattr(self, '_ipl_data_type', 'inst') != 'inst':
            table.setRowCount(0)
            return
        insts = [i for i in getattr(self, '_all_instances', []) if i.source_ipl == display_name]
        if not insts:
            parser = WPLParser(getattr(loader, '_iv_hash_names', {}))
            if parser.parse(entry.abs_path):
                insts = parser.instances
        headers = ["ID", "Model", "Int", "Pos X", "Pos Y", "Pos Z",
                   "Scale X", "Scale Y", "Scale Z", "Rot X", "Rot Y", "Rot Z", "Rot W"]
        table.setColumnCount(len(headers))
        table.setHorizontalHeaderLabels(headers)
        table.setRowCount(len(insts))
        for r, inst in enumerate(insts):
            values = [inst.model_id, inst.model_name, inst.interior,
                      f"{inst.pos_x:.6f}", f"{inst.pos_y:.6f}", f"{inst.pos_z:.6f}",
                      inst.scale_x, inst.scale_y, inst.scale_z,
                      f"{inst.rot_x:.7f}", f"{inst.rot_y:.7f}", f"{inst.rot_z:.7f}", f"{inst.rot_w:.7f}"]
            for c, value in enumerate(values):
                table.setItem(r, c, QTableWidgetItem(str(value)))

    def _load_selected_ipl_sections(self, rows): #vers 6
        """Show/load every currently-hidden row among the given table
        rows - the "Load Selected" context menu action.

         fix (Aug 20 2026,  "when selecting multiply
        ipls, all select, load all, this opens multiple dialogues. It
        should be one window with the title and process change; below
        should be the IPL data") - each selected IPL used to get its
        own, separate _preload_world_assets progress dialog, popping
        open and closing in turn as the loop below called _on_ipl_
        section_cell_clicked -> _ensure_ipl_loaded -> _preload_world_
        assets once per IPL. Now creates one real, shared QProgress
        Dialog up front instead, its own title/label updated for
        whichever IPL is currently loading, and self._shared_load_
        progress set for the duration of the loop so _preload_world_
        assets' own real, already-fixed reuse logic picks it up
        instead of creating a second, separate one per IPL."""
        table = self._ipl_sections_table
        hidden = getattr(self, '_hidden_ipls', set())
        ipl_names = []
        for row in rows:
            item = table.item(row, 0)
            if item is None:
                continue
            ipl_name = item.data(Qt.ItemDataRole.UserRole)
            if ipl_name in hidden:
                ipl_names.append(ipl_name)
        if not ipl_names:
            return

        dlg = _IplLoadDialog(self, len(ipl_names))
        self._ipl_load_dlg = dlg
        self._ipl_batch_defer, self._ipl_batch_dirty = True, False
        done = 0
        try:
            for idx, ipl_name in enumerate(ipl_names):
                if dlg.cancelled:
                    break
                if ipl_name not in getattr(self, '_hidden_ipls', set()):
                    continue   # already made visible by an earlier iteration's own load
                current_row = None
                for r in range(table.rowCount()):
                    candidate = table.item(r, 0)
                    if candidate is not None and candidate.data(Qt.ItemDataRole.UserRole) == ipl_name:
                        current_row = r
                        break
                if current_row is None:
                    continue   # row no longer exists (e.g. removed by an earlier rebuild)
                dlg.start_file(ipl_name, idx)
                try:
                    self._on_ipl_section_cell_clicked(current_row, 0)
                except Exception as e:   # report, then carry on with the rest
                    self._ipl_cell_click_in_progress = False
                    dlg.message(f"{ipl_name} - failed: {e}")
                    if self.main_window and hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message(f"[{ipl_name}] load failed: {e}")
                    continue
                done += 1
        finally:
            self._ipl_load_dlg = None
            self._ipl_batch_defer = False
            if self._ipl_batch_dirty:
                dlg.message("Refreshing world view...")
                self._apply_ipl_visibility_filter(clear_display_lists=False)
            dlg.finish(f"{'Cancelled - ' if dlg.cancelled else ''}{done} of {len(ipl_names)} IPL(s) loaded")

    def _on_ipl_sections_column_resized(self, logical_index, old_size, new_size): #vers 1
        """Persist the user's column widths for the IPL Sections table
        whenever they resize a column, so it doesn't reset to defaults
        on next open."""
        table = getattr(self, '_ipl_sections_table', None)
        if table is None:
            return
        widths = [table.columnWidth(c) for c in range(table.columnCount())]
        self.map_settings.set('ipl_sections_column_widths', widths)
        self.map_settings.save()

    def _on_ipl_tab_open_clicked(self): #vers 1
        """Open button - loads the currently selected IPL's content on
        demand, same as clicking its eye icon to show it."""
        table = getattr(self, '_ipl_sections_table', None)
        if table is None or table.currentRow() < 0:
            return
        self._on_ipl_section_cell_clicked(table.currentRow(), 0)

    def _on_ipl_tab_close_clicked(self): #vers 1
        """Close button - hides the currently selected IPL, same as
        clicking its eye icon to hide it. Its data stays loaded (not
        re-parsed if reopened), only its instances stop being shown."""
        table = getattr(self, '_ipl_sections_table', None)
        if table is None or table.currentRow() < 0:
            return
        row = table.currentRow()
        item = table.item(row, 0)
        if item is None:
            return
        ipl_name = item.data(Qt.ItemDataRole.UserRole)
        if ipl_name not in getattr(self, '_hidden_ipls', set()):
            self._on_ipl_section_cell_clicked(row, 0)

    def _on_ipl_tab_open_file_clicked(self): #vers 2
        """Open File... - lets USER pick one or more standalone .ipl
        or .zon files and adds them to the IPL Sections list (Aug 16 2026)"""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            self._set_status("Load a world first before opening a file")
            return
        from PyQt6.QtWidgets import QFileDialog
        paths, _ = QFileDialog.getOpenFileNames(
            self, "Open IPL/Zone File(s)", "",
            "IPL/Zone files (*.ipl *.zon);;IPL files (*.ipl);;"
            "Zone files (*.zon);;All files (*)")
        if not paths:
            return
        from apps.methods.gta_dat_parser import DATEntry
        added_any = False
        for path in paths:
            if not os.path.isfile(path):
                continue
            display_name = os.path.basename(path)
            stem = os.path.splitext(display_name)[0].lower()
            directive = "ZONE" if path.lower().endswith('.zon') else "IPL"
            entry = DATEntry(directive=directive, path=path, abs_path=path,
                            exists=True, source_dat="")
            loader.available_ipls[stem] = entry
            self._ipl_display_to_stem[display_name] = stem
            if display_name not in self._ipl_display_order:
                self._ipl_display_order.append(display_name)
            self._hidden_ipls.add(display_name)
            added_any = True
        if added_any:
            self._rebuild_ipl_sections_rows()
            self._set_status(f"Added {len(paths)} file(s) - click to view")

    def _on_ipl_data_type_changed(self, data_type): #vers 3
        """INST/CULL/ZONE/... changed - updates which kind of data the
        IPL Inst File panel shows for the currently selected IPL."""
        self._ipl_data_type = data_type
        if getattr(self, '_ipl_inst_file_mode', 'ipl') == 'ipl':
            dock = getattr(self, '_ipl_inst_file_dock', None)
            title_lbl = getattr(dock, '_title_label', None) if dock is not None else None
            if title_lbl is not None:
                if data_type == 'inst':
                    title_lbl.setText("IPL File Display")
                else:
                    tab_bar = getattr(self, '_ipl_tab_bar', None)
                    tab_keys = getattr(self, '_ipl_tab_keys', None)
                    label = data_type.upper()
                    if tab_bar is not None and tab_keys and data_type in tab_keys:
                        label = tab_bar.tabText(tab_keys.index(data_type))
                    title_lbl.setText(
                        "Paths Display" if data_type == 'path' else f"{label} Display")
        self._refresh_ipl_inst_file_panel()

    def _on_show_tobj_toggled(self, checked): #vers 1
        """Show Tobj checked/unchecked - refreshes the INST table to
        include/exclude TOBJ-type instances."""
        self._refresh_ipl_inst_file_panel()

    def _on_tcyc_toggled(self, checked): #vers 5
        """[TCYC] toggle - show/hide the loaded timecyc file's own
        sky/ambient effect at whatever the current Tobj/2DFX time
        happens to be, merged into the same row as [2DFX]/[TObj]
        (Aug 20 2026  own request).

         fix (Aug 20 2026,  "Timecyc playing should be
        linked to TOJB, 2DFX time button, we dont need to start time
        with TCYC button, thats only meant to toggle the sky on or
        off?") - a real, direct correction to an earlier version of
        this same method, which auto-started the shared Tobj time-
        flow timer the moment this was checked. That was wrong: this
        button's own real job is only ever to show/hide the timecyc
        effect at whatever hour the shared clock already says, not to
        also control whether that clock is running - time progression
        stays exclusively the Tobj/2DFX time Play/Stop button's own
        job. If that timer isn't running, this shows a real, correct,
        static snapshot at the current hour rather than an animated
        cycle - genuinely correct behaviour for a pure on/off toggle,
        not a gap to paper over by reaching into a control this
        button was never meant to own.

         fix (Aug 20 2026,  "turning the button on
        shows a timer inside it, we dont need that") - removed the
        earlier "live time label" this button used to grow while
        playing; the same real time already shows on the separate
        Time display right next to it in the same IPL Controls row -
        genuinely redundant, not a second, different piece of
        information."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_timecyc_playing'):
            vp.set_timecyc_playing(checked)

    def _show_alt_timecyc_menu(self, button): #vers 1
        """Middle-click [Tcyc]: pick an alternate timecyc.dat from this
        app's own depends/timecyc/ asset folder (Aug 20 2026, per
         : "each game has its own timecyc.dat, so we need to show
        that, and also a right-click option to show other timecyc.dat
        files that I'll put in an asset folder"). A manual pick here
        overrides the current, auto-detected file for this world/
        session only - the next real world load's own auto-detection
        (which already correctly re-runs every time, per game) will
        take back over, the same way the existing "Browse..." picker
        in Settings already behaves."""
        folder = self._app_asset_folder('timecyc')
        if not os.path.isdir(folder):
            self._set_status(f"No depends/timecyc/ asset folder found at {folder}")
            return
        try:
            files = sorted(f for f in os.listdir(folder) if f.lower().endswith('.dat'))
        except OSError:
            files = []
        if not files:
            self._set_status(f"No .dat files found in {folder}")
            return
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        for name in files:
            path = os.path.join(folder, name)
            menu.addAction(name, lambda p=path: self._apply_alt_timecyc(p))
        menu.exec(button.mapToGlobal(button.rect().bottomLeft()))

    def _apply_alt_timecyc(self, path): #vers 2
        """Apply a manually-picked alternate timecyc.dat (Aug 20 2026,
        same real request as _show_alt_timecyc_menu above) - same real
        set_timecyc_path call the existing Settings > Browse... picker
        already uses."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_timecyc_path'):
            known_game = getattr(getattr(self, '_world_loader', None), 'game', None)
            vp.set_timecyc_path(path, known_game=known_game)
        self._set_status(f"Timecyc: using {os.path.basename(path)}")

    def _on_ipl_tab_changed(self, index): #vers 1
        """QTabBar currentChanged - maps the tab index back to its
        section key and reuses the existing change handler. Qt still
        fires this for a click on a disabled tab in some styles/
        platforms even though the tab itself won't actually become
        current, so this stays a no-op for a disabled index rather
        than assuming the click always lands on an enabled tab."""
        bar = getattr(self, '_ipl_tab_bar', None)
        keys = getattr(self, '_ipl_tab_keys', None)
        if bar is None or keys is None or index < 0 or index >= len(keys):
            return
        if not bar.isTabEnabled(index):
            return
        self._on_ipl_data_type_changed(keys[index])

    def _create_dat_tab(self): #vers 1
        """[DAT] tab content, matching MooMapper's own "DAT Editor" -
        shows the real raw text of the loaded .dat file (comments,
        directive order, exactly as it appears on disk), re-read
        directly rather than reconstructed from parsed data. Edit/Save
        are honest stubs, same reasoning as the IDE tab."""
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(6, 6, 6, 6)
        title_row_widget = QWidget()
        title_row_widget.setObjectName("DAT_title_row")
        title_row = QHBoxLayout(title_row_widget)
        title_row.setContentsMargins(0, 0, 0, 0)
        from apps.methods.imgfactory_svg_icons import get_save_icon, get_edit_icon

        sm_buttonheight = 20

        icon_color = self._get_icon_color()

        label = QLabel("Dat Editor")
        label.setStyleSheet("font-weight: bold;")
        title_row.addWidget(label)

        _compact_18 = "padding: 0px 4px; margin: 0px; border-width: 1px;"
        from apps.methods.imgfactory_svg_icons import get_add_icon
        load_btn = QPushButton(get_add_icon(sm_buttonheight, icon_color), "Load")
        load_btn.setToolTip(
            "Load a game's root .dat file (gta.dat for SA, gta3.dat for\n"
            "LC/GTA3, gta_vc.dat for VC, gta_sol.dat/gtasol.dat for SOL) -\n"
            "works the same whether Map Workshop is running standalone or\n"
            "docked inside IMG Factory.")
        load_btn.setIconSize(QSize(18, 18))
        load_btn.setFixedHeight(18)
        load_btn.setStyleSheet(_compact_18)
        load_btn.clicked.connect(lambda: self._load_game_dat_file())

        # Recent files dropdown (Aug 1 2026)
        recent_btn = QPushButton("Recent")
        recent_btn.setToolTip("Recently loaded .dat files")
        recent_btn.setFixedHeight(18)
        recent_btn.setStyleSheet(_compact_18)
        recent_menu = QMenu(recent_btn)
        recent_btn.setMenu(recent_menu)
        self._recent_dat_menu = recent_menu
        self._refresh_recent_dat_menu()

        edit_btn = QPushButton(get_edit_icon(sm_buttonheight, icon_color), "Edit")
        edit_btn.setToolTip("STUB - no .dat editing built yet")
        edit_btn.setIconSize(QSize(18, 18))
        edit_btn.setFixedHeight(18)
        edit_btn.setStyleSheet(_compact_18)
        edit_btn.setEnabled(False)
        save_btn = QPushButton(get_save_icon(sm_buttonheight, icon_color), "Save")
        save_btn.setToolTip("STUB - no write-back to disk exists for any\n"
                            "file type in Map Workshop yet")
        save_btn.setIconSize(QSize(18, 18))
        save_btn.setFixedHeight(18)
        save_btn.setStyleSheet(_compact_18)
        save_btn.setEnabled(False)
        title_row.addWidget(load_btn)
        title_row.addWidget(recent_btn)
        title_row.addWidget(edit_btn)
        title_row.addWidget(save_btn)
        title_row.addStretch()
        lay.addWidget(title_row_widget)
        self._register_collapsible_button_row(
            title_row_widget, [(load_btn, "Load"), (recent_btn, "Recent"), (edit_btn, "Edit"), (save_btn, "Save")])

        if not hasattr(self, '_object_browser_tab_rows'):
            self._object_browser_tab_rows = {}
        self._object_browser_tab_rows['dat'] = title_row_widget

        content_container = QWidget()
        content_lay = QVBoxLayout(content_container)
        content_lay.setContentsMargins(0, 0, 0, 0)

        from PyQt6.QtWidgets import QTextEdit
        text = QTextEdit()
        text.setReadOnly(True)
        font = text.font(); font.setFamily("monospace"); text.setFont(font)
        text.setPlaceholderText("Load a game folder/DAT file to see its contents here")
        content_lay.addWidget(text)
        self._dat_tab_text = text

        lay.addWidget(content_container)
        self._make_section_collapsible(label, content_container)
        return panel


    def _refresh_dat_tab(self): #vers 1
        text_widget = getattr(self, '_dat_tab_text', None)
        dat_path = getattr(self, '_loaded_dat_path', None)
        if text_widget is None:
            return
        if not dat_path:
            text_widget.clear()
            return
        try:
            with open(dat_path, 'r', encoding='ascii', errors='ignore') as f:
                text_widget.setPlainText(f.read())
        except Exception as e:
            text_widget.setPlainText(f"(could not read {dat_path}: {e})")


    def _create_img_tab(self): #vers 1
        """[IMG] tab content, matching MooMapper's own "IMG Archive"
        tab - numbered sub-tabs (one per loaded IMG archive), each
        showing that archive's real entry list (name/type/size) via
        the already-built ModelCache index. Extract/Add/Del/Rename are
        honest stubs - these are real file-writing operations, and no
        write-back infrastructure exists for any file type in Map
        Workshop yet, IMG included."""
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(1, 1, 1, 1)
        from apps.methods.imgfactory_svg_icons import get_rebuild_icon, get_rename_icon, get_remove_icon, get_add_icon, get_dump_icon

        title_row_widget = QWidget()
        title_row_widget.setObjectName("IMG_title_row")
        title_row = QHBoxLayout(title_row_widget)
        title_row.setContentsMargins(0, 0, 0, 0)
        sm_buttonheight = 20
        icon_color = self._get_icon_color()
        _compact_18 = "padding: 0px 4px; margin: 0px; border-width: 1px;"

        label = QLabel("IMG File")
        label.setStyleSheet("font-weight: bold;")
        title_row.addWidget(label)

        ext_btn = QPushButton(get_dump_icon(sm_buttonheight, icon_color), "Extract")
        ext_btn.setToolTip("STUB - no .dat editing built yet")
        ext_btn.setIconSize(QSize(18, 18))
        ext_btn.setFixedHeight(18)
        ext_btn.setStyleSheet(_compact_18)
        ext_btn.setEnabled(False)
        add_btn = QPushButton(get_add_icon(sm_buttonheight, icon_color), "Add")
        add_btn.setToolTip("STUB - no .dat editing built yet")
        add_btn.setIconSize(QSize(18, 18))
        add_btn.setFixedHeight(18)
        add_btn.setStyleSheet(_compact_18)
        add_btn.setEnabled(False)
        del_btn = QPushButton(get_remove_icon(sm_buttonheight, icon_color), "Del")
        del_btn.setToolTip("STUB - no .dat editing built yet")
        del_btn.setIconSize(QSize(18, 18))
        del_btn.setFixedHeight(18)
        del_btn.setStyleSheet(_compact_18)
        del_btn.setEnabled(False)
        ren_btn = QPushButton(get_rename_icon(sm_buttonheight, icon_color), "Rename")
        ren_btn.setToolTip("STUB - no .dat editing built yet")
        ren_btn.setIconSize(QSize(18, 18))
        ren_btn.setFixedHeight(18)
        ren_btn.setStyleSheet(_compact_18)
        ren_btn.setEnabled(False)
        save_btn = QPushButton(get_rebuild_icon(sm_buttonheight, icon_color), "Rebuild")
        save_btn.setToolTip("STUB - no write-back to disk exists for any\n"
                            "file type in Map Workshop yet")
        save_btn.setIconSize(QSize(18, 18))
        save_btn.setFixedHeight(18)
        save_btn.setStyleSheet(_compact_18)
        save_btn.setEnabled(False)
        for b in (ext_btn, add_btn, del_btn, ren_btn, save_btn):
            title_row.addWidget(b)
        title_row.addStretch()
        lay.addWidget(title_row_widget)
        self._register_collapsible_button_row(
            title_row_widget, [(ext_btn, "Extract"), (add_btn, "Add"),
                                (del_btn, "Del"), (ren_btn, "Rename"),
                                (save_btn, "Rebuild")])
        if not hasattr(self, '_object_browser_tab_rows'):
            self._object_browser_tab_rows = {}
        self._object_browser_tab_rows['img'] = title_row_widget

        content_container = QWidget()
        content_lay = QVBoxLayout(content_container)
        content_lay.setContentsMargins(0, 0, 0, 0)

        img_tabs = QTabWidget()
        img_tabs.setTabPosition(QTabWidget.TabPosition.North)
        from PyQt6.QtGui import QFontMetrics
        tab_font = img_tabs.font()
        img_tabs.tabBar().setStyleSheet(
            f"QTabBar::tab {{ height: {QFontMetrics(tab_font).height() + 6}px; padding: 2px 8px; }}")
        content_lay.addWidget(img_tabs)
        self._img_tab_tabs = img_tabs

        lay.addWidget(content_container)
        self._make_section_collapsible(label, content_container)
        return panel


    def _refresh_img_tab(self, loader): #vers 1
        """Rebuild the numbered IMG sub-tabs from the loaded world's
        actual IMG archives (loader.get_img_paths()) - one table per
        archive, listing every entry's real name/type/size read
        directly from the archive itself."""
        img_tabs = getattr(self, '_img_tab_tabs', None)
        if img_tabs is None:
            return
        img_tabs.clear()
        from apps.methods.img_core_classes import IMGFile
        for i, img_path in enumerate(loader.get_img_paths(), 1):
            table = QTableWidget(0, 4)
            table.setHorizontalHeaderLabels(["Ind.", "Filename", "Type", "Size"])
            table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
            table.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.Stretch)
            self._apply_compact_table_style(table)
            try:
                img = IMGFile(img_path)
                if img.open():
                    table.setRowCount(len(img.entries))
                    for row, entry in enumerate(img.entries):
                        table.setItem(row, 0, QTableWidgetItem(str(row)))
                        table.setItem(row, 1, QTableWidgetItem(entry.name))
                        table.setItem(row, 2, QTableWidgetItem(entry.extension.upper() + " File"
                                                                if entry.extension else "File"))
                        size_kb = max(1, entry.size // 1024)
                        table.setItem(row, 3, QTableWidgetItem(f"{size_kb} kB"))
            except Exception:
                pass
            img_tabs.addTab(table, str(i))



    def _on_ipl_section_cell_clicked(self, row, col): #vers 5
        """Clicking the eye-icon cell (col 0) toggles that IPL's
        visibility - plain item click rather than a button, so there's
        no button widget/chrome to size or pad. Clicking anywhere else
        on the row just selects it, which is enough to refresh the IPL
        Inst File panel below)"""
        if getattr(self, '_ipl_cell_click_in_progress', False):
            return
        self._ipl_cell_click_in_progress = True
        table = self._ipl_sections_table
        try:
            item = table.item(row, 0)
            if item is None:
                return
            display_name = item.data(Qt.ItemDataRole.UserRole)
            if col == 0:
                ipl_name = display_name
                hidden = ipl_name in getattr(self, '_hidden_ipls', set())
                new_hidden = not hidden
                if not new_hidden:
                    # Disable the whole table for the duration of the
                    # actual load (Aug 1 2026)
                    table.setEnabled(False)
                    try:
                        self._ensure_ipl_loaded(ipl_name)
                    finally:
                        table.setEnabled(True)
                    # Re-locate the row by name, not the original row
                    # index (Aug 1 2026, same crash)
                    item = None
                    for r in range(table.rowCount()):
                        candidate = table.item(r, 0)
                        if candidate is not None and candidate.data(Qt.ItemDataRole.UserRole) == ipl_name:
                            item = candidate
                            row = r
                            break
                    if item is None:
                        return   # row no longer exists post-rebuild - nothing left to update
                item.setIcon(self._eye_closed_icon if new_hidden else self._eye_open_icon)
                item.setToolTip(f"Show {ipl_name}" if new_hidden else f"Hide {ipl_name}")
                name_item = table.item(row, 1)
                if name_item is not None:
                    loader = getattr(self, '_world_loader', None)
                    if ipl_name in getattr(self, '_ipl_names_with_binary_stream', {}) or \
                            display_name in getattr(self, '_loaded_binary_ipls', set()):
                        row_is_loaded = display_name in getattr(self, '_loaded_binary_ipls', set())
                    else:
                        stem_for_load_check = getattr(self, '_ipl_display_to_stem', {}).get(ipl_name)
                        row_is_loaded = (loader is not None and stem_for_load_check is not None
                                         and stem_for_load_check in loader.loaded_ipls)
                    self._style_ipl_name_item(name_item, new_hidden, row_is_loaded)
                self._toggle_ipl_section(ipl_name, new_hidden)
            self._auto_switch_ipl_tab_if_empty(display_name)
            self._refresh_ipl_inst_file_panel()
        finally:
            self._ipl_cell_click_in_progress = False

    def _ipl_section_has_data(self, raw_text, section_name): #vers 1
        """True if `section_name` exists in raw_text AND has at least
        one real data line - not just the bare keyword/end pair (Aug
        15 2026)"""
        section_lines = self._extract_ipl_section_text(raw_text, section_name)
        if not section_lines:
            return False
        for line_no, raw_line in section_lines:
            line = raw_line.split("#")[0].strip()
            low = line.lower()
            if not line or low in (section_name, 'end'):
                continue
            return True
        return False

    def _auto_switch_ipl_tab_if_empty(self, display_name): #vers 1
        """If the currently active IPL Controls tab (INST/CULL/PATH/
        etc) has no real data in the just-selected file, switch to
        the first enabled tab that does (Aug 15 2026)"""
        bar = getattr(self, '_ipl_tab_bar', None)
        keys = getattr(self, '_ipl_tab_keys', None)
        loader = getattr(self, '_world_loader', None)
        if bar is None or keys is None or loader is None or not display_name:
            return
        stem = getattr(self, '_ipl_display_to_stem', {}).get(display_name)
        if stem is None or stem.startswith('img:'):
            return
        entry = loader.available_ipls.get(stem)
        if entry is None or not entry.exists:
            return
        try:
            with open(entry.abs_path, 'r', encoding='ascii', errors='ignore') as f:
                raw_text = f.read()
        except Exception:
            return

        current_type = getattr(self, '_ipl_data_type', 'inst')
        if self._ipl_section_has_data(raw_text, current_type):
            return
        for idx, key in enumerate(keys):
            if key == current_type or not bar.isTabEnabled(idx):
                continue
            if self._ipl_section_has_data(raw_text, key):
                bar.blockSignals(True)
                bar.setCurrentIndex(idx)
                bar.blockSignals(False)
                self._ipl_data_type = key
                return

    def _on_ipl_section_cell_double_clicked(self, row, col): #vers 1
        """Double-click any cell in an IPL Sections row to open/show
        that IPL (Aug 14 2026)"""
        if getattr(self, '_ipl_cell_click_in_progress', False):
            return
        table = self._ipl_sections_table
        item = table.item(row, 0)
        if item is None:
            return
        ipl_name = item.data(Qt.ItemDataRole.UserRole)
        if ipl_name in getattr(self, '_hidden_ipls', set()):
            self._on_ipl_section_cell_clicked(row, 0)

    def _ensure_ipl_loaded(self, display_name): #vers 6
        """Actually load one IPL's content on demand, the first time
        it's toggled visible - parses its instances (GTAWorldLoader.
        load_ipl_by_name), refreshes self._all_instances/Object Browser
        with the newly-added data, and pre-loads (with a progress
        dialog scoped to just this IPL's models, not the whole world)
        the geometry/textures those new instances reference. A no-op
        if this IPL was already loaded (or lazy loading isn't active
        at all, e.g. a non-Map-Workshop caller)."""

        loader = getattr(self, '_world_loader', None)
        if loader is None or not getattr(loader, 'lazy_ipl_loading', False):
            return
        stem = getattr(self, '_ipl_display_to_stem', {}).get(display_name)
        if stem is None:
            return

        if stem.startswith('img:'):
            if display_name in getattr(self, '_loaded_binary_ipls', set()):
                return   # already loaded via _load_binary_ipl_stream
            archive_path = getattr(self, '_standalone_binary_ipl_archives', {}).get(display_name)
            if archive_path is None:
                self._set_status(f"{display_name}: no known archive to load it from")
                return
            self._load_binary_ipl_stream(archive_path, display_name)
            return

        if stem in loader.loaded_ipls:
            return   # not a lazily-tracked IPL, or already loaded

        before_count = len(loader.instances)
        result = loader.load_ipl_by_name(stem)
        if not result.success:
            self._set_status(f"{display_name} failed to load"
                             + (f" - {result.errors[0]}" if result.errors else ""))
            return

        problem_count = result.error_count + result.warning_count
        if problem_count:
            log_path = self._write_ipl_error_log(result)
            self._set_status(
                f"{display_name} loaded - {problem_count} issue(s) found"
                + (f", check {os.path.basename(log_path)} added to the maps folder"
                   if log_path else ""))
        else:
            self._set_status(f"{display_name} loaded - no errors "
                             f"({result.instance_count} instances)")

        new_instances = loader.instances[before_count:]

        self._all_instances = list(loader.instances)
        self._populate_object_browser(loader)

        model_cache = getattr(self, '_model_cache', None)

        # One shared dialog: batch loads pass theirs in via self._ipl_load_dlg
        stream_entries = getattr(self, '_ipl_names_with_binary_stream', {}).get(display_name, [])
        dlg = getattr(self, '_ipl_load_dlg', None)
        own_dlg = dlg is None
        if own_dlg:
            dlg = _IplLoadDialog(self, 1)
            dlg.start_file(display_name, 0)
        if self.map_settings.get('show_verbose_loading_dialog'):
            for inst in new_instances:
                dlg.message(f"{inst.model_id}, {inst.model_name}")
        if model_cache is not None and new_instances:
            for m in self._load_models_into_dialog(dlg, loader, model_cache, new_instances):
                dlg.message(f"missing: {m}")

        # IV parent .wpl holds little; city placements are in its streams
        if self.map_settings.get('load_text_plus_binary_ipl_set') or getattr(loader, 'game', None) == 'iv':
            for archive_path, stream_name in sorted(stream_entries, key=lambda t: t[1]):
                if stream_name in getattr(self, '_loaded_binary_ipls', set()):
                    continue
                dlg.message(f"Loading linked ipl file {stream_name}")
                self._load_binary_ipl_stream(archive_path, stream_name)
        if own_dlg:
            dlg.finish(f"{display_name} loaded ({len(new_instances)} instances)")

    def _write_ipl_error_log(self, result): #vers 1
        """Write a plain-text log of one IPL's parse errors/warnings
        alongside the IPL file itself (same folder)"""
        if not result.abs_path:
            return None
        try:
            folder = os.path.dirname(result.abs_path)
            base = os.path.splitext(os.path.basename(result.abs_path))[0]
            log_path = os.path.join(folder, f"{base}_load_errors.log")
            with open(log_path, 'w', encoding='utf-8') as f:
                f.write(f"Load errors for {result.abs_path}\n")
                f.write(f"{result.error_count} error(s), {result.warning_count} warning(s)\n\n")
                if result.errors:
                    f.write("Errors:\n")
                    for e in result.errors:
                        f.write(f"  - {e}\n")
                if result.warnings:
                    f.write("\nWarnings:\n")
                    for w in result.warnings:
                        f.write(f"  - {w}\n")
            return log_path
        except Exception:
            return None

    def _style_ipl_name_item(self, name_item, hidden, loaded=False): #vers 5
        """Grey out a disabled/hidden IPL's name text.  fix (Aug
        20 2026,  "once proloaded, mark them as loaded
        white in the obj browser ipl llst") - loaded entries now get
        a genuinely brighter/white text colour, not just the existing
        "(Loaded)" text suffix, so a loaded IPL is visually distinct
        from an unloaded one at a glance, not just readable on close
        inspection.

         fix (Aug 20 2026,  "entries should be
        displaying the theme aware white") - a hardcoded QColor(255,
        255, 255) isn't theme-aware at all; a light theme's own
        background could be close to pure white too, making loaded
        entries hard to tell apart from the background rather than
        from unloaded rows. Uses the palette's own real BrightText
        role instead - Qt's own real, standard role for exactly this
        ("distinct" emphasised text), so it stays correct whatever
        theme is active, not just this app's current dark one."""
        pal = self.palette()
        text_color = pal.color(pal.ColorGroup.Active, pal.ColorRole.Text)
        if hidden:
            bg_color = pal.color(pal.ColorGroup.Active, pal.ColorRole.Window)
            color = QColor(
                (text_color.red()   + bg_color.red())   // 2,
                (text_color.green() + bg_color.green()) // 2,
                (text_color.blue()  + bg_color.blue())  // 2)
        elif loaded:
            color = pal.color(pal.ColorGroup.Active, pal.ColorRole.BrightText)
        else:
            color = text_color
        name_item.setForeground(QBrush(color))

    def _toggle_ipl_section(self, ipl_name, hidden): #vers 3
        """Show/hide all instances from one IPL file - recomputes the
        visible instance subset from the full loaded set and re-feeds
        it to the world-view panes and Instance List, without needing
        to reload from disk.

        clear_display_lists=False (Aug 1 2026)"""
        if hidden:
            self._hidden_ipls.add(ipl_name)
        else:
            self._hidden_ipls.discard(ipl_name)
        if getattr(self, '_ipl_batch_defer', False):
            self._ipl_batch_dirty = True     # batch load: one refresh at the end
            return
        self._apply_ipl_visibility_filter(clear_display_lists=False)
