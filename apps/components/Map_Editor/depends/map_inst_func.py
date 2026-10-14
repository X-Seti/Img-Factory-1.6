#this belongs in apps/components/Map_Editor/depends/map_inst_func.py - Version: 1
# X-Seti - October 14 2026 - IMG Factory 1.6 - Map Workshop IPL inst panel

"""
IPL inst file panel, path groups, zone/cull/occl/grge boxes, IPL convert for ModelWorkshop (Map).
"""

##class MapInstMixin: -
# _add_cull
# _add_grge
# _add_occl
# _add_zone
# _apply_compact_table_style
# _box_corners_3d
# _center_viewport_on_instance
# _combined_box_list
# _convert_ipl_files_dialog
# _convert_ipl_inst_section
# _convert_loaded_ipls_dialog
# _create_ide_tab
# _cycle_selected_box
# _delete_cull
# _delete_grge
# _delete_occl
# _delete_path_group_for_row
# _delete_zone
# _edit_path_group_for_row
# _extract_ipl_section_text
# _find_instance_for_ipl_inst_file_row
# _find_path_group_for_line
# _highlight_zone_containing_instance
# _is_path_group_simple_chain
# _jump_to_ide_line
# _jump_to_ipl_line
# _new_path_group
# _on_cull_or_zone_box_picked
# _on_ide_tab_row_clicked
# _on_ipl_inst_file_cell_clicked
# _on_ipl_inst_file_cell_double_clicked
# _on_ipl_inst_file_context_menu
# _on_path_node_picked
# _on_world_instance_picked
# _optimize_dat_load_order_clicked
# _play_auzo_placeholder_tone
# _populate_auzo_table
# _populate_gta3_ide_paths_table
# _refresh_ide_objects_panel
# _refresh_ide_tab
# _refresh_ipl_inst_file_panel
# _repair_one_scale_instances
# _repair_scale_instances
# _repair_zero_scale_instances
# _resolve_path_group_for_row
# _reverse_path_group_traffic_flow
# _run_ipl_inst_conversion
# _save_culls
# _save_grges
# _save_occls
# _select_and_scroll_to_line
# _show_box_corner_coords
# _show_box_picker_menu
# _show_convert_ipl_menu
# _show_ipl_format_checker
# _show_model_search_dialog
# _show_overlay_middle_click_menu
# _show_repair_scale_menu
# _show_textures_for_instance
# _sync_ipl_inst_file_selection
# _write_back_cull_section
# _write_back_grge_section
# _write_back_occl_section

import math
import os

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QBrush, QColor
from PyQt6.QtWidgets import (
    QAbstractItemView, QApplication, QDialog, QFileDialog, QHBoxLayout, QHeaderView, QLabel,
    QLineEdit, QListWidget, QListWidgetItem, QMenu, QMessageBox, QPushButton, QTableWidget,
    QTableWidgetItem, QVBoxLayout, QWidget)

from apps.components.Map_Editor.depends.map_widgets_func import (
    _PathGroupEditDialog, _write_ipl_lines)


class MapInstMixin: #vers 1
    """IPL inst file panel, path groups, zone/cull/occl/grge boxes, IPL convert."""

    def _jump_to_ipl_line(self, source_ipl, line_no): #vers 1
        """Switch Object Browser to the IPL tab, select the instance's
        own source IPL file in IPL Sections, and highlight/scroll to
        the exact row matching its original file line number."""
        self._on_object_browser_tab_changed('ipl')
        sections_table = getattr(self, '_ipl_sections_table', None)
        if sections_table is None:
            return
        for row in range(sections_table.rowCount()):
            item = sections_table.item(row, 0)
            if item is not None and item.data(Qt.ItemDataRole.UserRole) == source_ipl:
                sections_table.setCurrentCell(row, 1)
                break
        self._refresh_ipl_inst_file_panel()
        self._select_and_scroll_to_line(line_no)

    def _jump_to_ide_line(self, source_ide, line_no): #vers 1
        """Switch Object Browser to the IDE tab, select the object's
        own source IDE file in the IDE file list, and highlight/scroll
        to the exact row matching its original file line number - same
        purpose as _jump_to_ipl_line, for the IDE side."""
        self._on_object_browser_tab_changed('ide')
        ide_paths = getattr(self, '_ide_tab_paths', {})
        target_row = None
        for row, path in ide_paths.items():
            if os.path.basename(path) == source_ide:
                target_row = row
                break
        if target_row is None:
            return
        ide_table = getattr(self, '_ide_tab_table', None)
        if ide_table is not None:
            ide_table.setCurrentCell(target_row, 1)
        self._refresh_ide_objects_panel(target_row)
        self._select_and_scroll_to_line(line_no)

    def _select_and_scroll_to_line(self, line_no): #vers 1
        """Find the shared cells table row whose tracked original file
        line number matches line_no (stored on each row's column-0
        item, see _refresh_ipl_inst_file_panel/_refresh_ide_objects_
        panel), select it, and scroll it into view. Shared helper for
        both _jump_to_ipl_line and _jump_to_ide_line, since both land
        on the same table (self._ipl_inst_file_table), just populated
        differently."""
        table = getattr(self, '_ipl_inst_file_table', None)
        if table is None:
            return
        for row in range(table.rowCount()):
            item = table.item(row, 0)
            if item is not None and item.data(Qt.ItemDataRole.UserRole) == line_no:
                table.selectRow(row)
                table.scrollToItem(item)
                return

    def _populate_gta3_ide_paths_table(self, table, loader, display_name): #vers 1
        """Build the IPL Inst File table from GTA III's own IDE-
        embedded path groups instead of raw IPL text, scoped to
        whichever groups have a real instance placement within this
        specific IPL (Aug 19 2026)"""
        instances_in_ipl = {inst.model_id for inst in getattr(loader, 'instances', [])
                            if inst.source_ipl == display_name}
        groups = [g for g in getattr(loader, 'ide_paths', [])
                 if g.model_id in instances_in_ipl]

        headers = ["Type / GroupType", "Next / ModelID", "IsCrossroad / ModelName",
                  "X Rel", "Y Rel", "Z Rel", "Median", "Left", "Right"]

        if not groups:
            table.setRowCount(1)
            table.setColumnCount(1)
            table.setHorizontalHeaderLabels([""])
            placeholder = QTableWidgetItem(
                "(no path groups attached to any object placed in this IPL)")
            placeholder.setFlags(placeholder.flags() & ~Qt.ItemFlag.ItemIsSelectable)
            table.setItem(0, 0, placeholder)
            return

        table.setColumnCount(len(headers))
        table.setHorizontalHeaderLabels(headers)
        rows = []
        for group in groups:
            rows.append((group.group_type, str(group.model_id), group.model_name,
                        "", "", "", "", "", "", True))
            for node in group.nodes:
                rows.append((str(node.node_type), str(node.next_id), str(node.is_crossroad),
                            f"{node.x_rel:.4f}", f"{node.y_rel:.4f}", f"{node.z_rel:.4f}",
                            f"{node.median:.2f}", str(node.left), str(node.right), False))

        table.setRowCount(len(rows))
        for r, row_values in enumerate(rows):
            is_group_header = row_values[-1]
            for c in range(len(headers)):
                item = QTableWidgetItem(row_values[c])
                if is_group_header:
                    font = item.font()
                    font.setBold(True)
                    item.setFont(font)
                    item.setBackground(QBrush(QColor(60, 60, 90)))
                table.setItem(r, c, item)

    def _play_auzo_placeholder_tone(self, sound_id, name): #vers 2
        """Play a sound for one audio zone (Aug 20 2026, .per:
        "auzo list play the sounds" / "i can send you the sounds,
        would that help") - the real, in-game San Andreas audio
        itself lives inside the game's own compiled audio bank
        archives, a completely separate binary format this app
        doesn't read at all (see AuzoEntry's own docstring in gta_
        dat_parser.py for the full, honest explanation) - there is no
        real sound data anywhere in the loaded IPL/IDE data this
        could actually play on its own.

         fix (Aug 20 2026, same real request as above) - checks
        this app's own depends/auzo_sounds/ asset folder first (the
        same real "depends/<name>/" pattern _app_asset_folder already
        shares with timecyc's own asset folder) for a real, actual
        sound file   can drop in - tried by both sound_id and
        zone name, in a few common real audio extensions, since the
        exact naming convention for whatever files   sends isn't
        known yet. Falls back to a synthetic sine-wave placeholder
        tone (pitch derived from sound_id, so different zones are at
        least audibly distinguishable from each other even without
        real audio) only when no real matching file is found.

        Uses QtMultimedia's own QSoundEffect, a real dependency this
        app has never needed before this feature - wrapped defensively
        since it may not be installed/available in every real Python
        environment this app runs in; fails with a clear status
        message rather than crashing if it isn't."""
        try:
            from PyQt6.QtMultimedia import QSoundEffect
            from PyQt6.QtCore import QUrl
        except ImportError:
            self._set_status(
                "Can't play audio zone sounds - PyQt6.QtMultimedia isn't "
                "installed in this Python environment.")
            return

        real_path = None
        folder = self._app_asset_folder('auzo_sounds')
        if os.path.isdir(folder):
            #  fix (Aug 20 2026  own uploaded Audiozon.
            # ipl/AudioEvents.txt, cross-referenced against AUZO_TYPES'
            # own "music description" field - e.g. sound_id 54 -> real
            # radio station "KDST") - the game's own real radio station
            # music (unlike most other sound effects) is typically
            # stored as plain, separate stream files in a real SA
            # install's own audio/ folder, not compiled into the same
            # inaccessible bank format - a real, meaningful name like
            # "KDST.wav" is a far more natural real filename for  
            # to actually use than the bare numeric sound_id alone.
            music_desc = None
            try:
                from apps.methods.gta_dat_parser import AUZO_TYPES
                info = AUZO_TYPES.get(int(sound_id))
                if info and info[1]:
                    music_desc = info[1]
            except (ImportError, ValueError):
                pass
            candidates = [c for c in (f"{sound_id}", name, music_desc) if c]
            extensions = ('.wav', '.mp3', '.ogg', '.flac')
            try:
                files = os.listdir(folder)
            except OSError:
                files = []
            files_lower = {f.lower(): f for f in files}
            for candidate in candidates:
                for ext in extensions:
                    key = f"{candidate}{ext}".lower()
                    if key in files_lower:
                        real_path = os.path.join(folder, files_lower[key])
                        break
                if real_path:
                    break

        if real_path:
            effect = QSoundEffect(self)
            effect.setSource(QUrl.fromLocalFile(real_path))
            effect.setVolume(0.7)
            effect.play()
            self._auzo_sound_effect = effect
            self._set_status(f"Playing {os.path.basename(real_path)} for audio zone '{name}'")
            return

        import struct, wave, tempfile, math
        freq = 220.0 + (int(sound_id) % 24) * 55.0
        duration = 0.35
        rate = 22050
        n_samples = int(rate * duration)
        path = os.path.join(tempfile.gettempdir(), f"_mapworkshop_auzo_tone_{sound_id}.wav")
        if not os.path.isfile(path):
            with wave.open(path, 'w') as wf:
                wf.setnchannels(1)
                wf.setsampwidth(2)
                wf.setframerate(rate)
                frames = bytearray()
                for i in range(n_samples):
                    t = i / rate
                    fade = min(1.0, (duration - t) * 6.0, t * 40.0)
                    sample = int(32767 * 0.4 * fade * math.sin(2 * math.pi * freq * t))
                    frames += struct.pack('<h', sample)
                wf.writeframes(bytes(frames))
        effect = QSoundEffect(self)
        effect.setSource(QUrl.fromLocalFile(path))
        effect.setVolume(0.6)
        effect.play()
        self._auzo_sound_effect = effect   # keep a real reference alive until playback finishes
        self._set_status(f"Playing placeholder tone for audio zone '{name}' (sound_id={sound_id}) - no real sound file found in depends/auzo_sounds/")

    def _populate_auzo_table(self, table, loader, display_name): #vers 2
        """Build the IPL Inst File table from SA's real, already-
        parsed audio zones for this specific IPL (Aug 20 2026).

        Double-click-to-play hint added (Aug 20 2026, .per:
        "auzo list play the sounds") - _on_ipl_inst_file_cell_double_
        clicked handles the actual playback; this just makes it
        discoverable."""
        table.setToolTip("Double-click a row to play a placeholder tone for that audio zone.")
        auzos = [a for a in getattr(loader, 'auzos', [])
                if a.source_ipl == display_name]

        headers = ["Name", "Sound ID", "Switch", "Shape",
                  "X1", "Y1", "Z1", "X2 / Radius", "Y2", "Z2",
                  "Environment", "Music / Ambience"]

        if not auzos:
            table.setRowCount(1)
            table.setColumnCount(1)
            table.setHorizontalHeaderLabels([""])
            placeholder = QTableWidgetItem("(no audio zones in this IPL)")
            placeholder.setFlags(placeholder.flags() & ~Qt.ItemFlag.ItemIsSelectable)
            table.setItem(0, 0, placeholder)
            return

        table.setColumnCount(len(headers))
        table.setHorizontalHeaderLabels(headers)
        table.setRowCount(len(auzos))
        for r, a in enumerate(auzos):
            shape = "Sphere" if a.is_sphere else "Cube"
            x2_or_radius = f"{a.radius:.4f}" if a.is_sphere else f"{a.x2:.4f}"
            y2 = "" if a.is_sphere else f"{a.y2:.4f}"
            z2 = "" if a.is_sphere else f"{a.z2:.4f}"
            values = [a.name, str(a.sound_id), str(a.switch), shape,
                     f"{a.x1:.4f}", f"{a.y1:.4f}", f"{a.z1:.4f}",
                     x2_or_radius, y2, z2,
                     a.environment_type or "", a.music_description or ""]
            for c, value in enumerate(values):
                table.setItem(r, c, QTableWidgetItem(value))

    def _refresh_ipl_inst_file_panel(self): #vers 4
        """Re-read the currently selected IPL's raw file content,
        filtered to the currently selected data type (INST/CULL/ZONE -
        PATH is stubbed, never reachable here since its radio button
        is disabled), and parse it into the cells table - one row per
        instance line, split on commas into its 13 fields.

        Guarded by self._ipl_inst_file_mode (Aug 1 2026)"""
        if getattr(self, '_ipl_inst_file_mode', 'ipl') != 'ipl':
            return
        table = getattr(self, '_ipl_inst_file_table', None)
        sections_table = getattr(self, '_ipl_sections_table', None)
        loader = getattr(self, '_world_loader', None)
        if table is None or sections_table is None or loader is None:
            return
        row = sections_table.currentRow()
        if row < 0:
            table.setRowCount(0)
            return
        item = sections_table.item(row, 0)
        if item is None:
            table.setRowCount(0)
            return
        display_name = item.data(Qt.ItemDataRole.UserRole)
        stem = getattr(self, '_ipl_display_to_stem', {}).get(display_name)

        # GTA III's own IDE-embedded paths (Aug 19 2026)
        if (getattr(self, '_ipl_data_type', 'inst') == 'path'
                and getattr(loader, 'game', None) == 'gta3'):
            self._populate_gta3_ide_paths_table(table, loader, display_name)
            return

        # SA audio zones (Aug 20 2026)
        if getattr(self, '_ipl_data_type', 'inst') == 'auzo':
            self._populate_auzo_table(table, loader, display_name)
            return

        # Binary IPLs (Aug 1 2026)
        if stem is not None and stem.startswith('img:'):
            if display_name not in getattr(self, '_loaded_binary_ipls', set()):
                archive_path = getattr(self, '_standalone_binary_ipl_archives', {}).get(display_name)
                if archive_path:
                    self._load_binary_ipl_stream(archive_path, display_name)
            matching = [i for i in getattr(self, '_all_instances', [])
                       if i.source_ipl == display_name]
            table.setRowCount(len(matching))
            for r, inst in enumerate(matching):
                values = [inst.model_id, inst.model_name, inst.interior,
                         f"{inst.pos_x:.6f}", f"{inst.pos_y:.6f}", f"{inst.pos_z:.6f}",
                         inst.scale_x, inst.scale_y, inst.scale_z,
                         f"{inst.rot_x:.7f}", f"{inst.rot_y:.7f}", f"{inst.rot_z:.7f}", f"{inst.rot_w:.7f}"]
                for c, value in enumerate(values):
                    table.setItem(r, c, QTableWidgetItem(str(value)))
            return

        entry = loader.available_ipls.get(stem) if stem else None
        if entry is None or not entry.exists:
            table.setRowCount(0)
            return
        if entry.abs_path.lower().endswith('.wpl'):   # GTA IV binary placements
            self._populate_wpl_table(table, loader, entry, display_name)
            return
        try:
            with open(entry.abs_path, 'r', encoding='ascii', errors='ignore') as f:
                raw_text = f.read()
        except Exception:
            table.setRowCount(0)
            return

        data_type = getattr(self, '_ipl_data_type', 'inst')
        section_lines = self._extract_ipl_section_text(raw_text, data_type)
        if section_lines is None:
            # Aug 14 2026 fix
            table.setColumnCount(1)
            table.setHorizontalHeaderLabels([""])
            placeholder = QTableWidgetItem(
                f"(no {data_type.upper()} section in this file - try another tab above)")
            placeholder.setFlags(placeholder.flags() & ~Qt.ItemFlag.ItemIsSelectable)
            table.setItem(0, 0, placeholder)
            return

        # Per-section column headers (Aug 1 2026)
        headers_by_type = {
            'inst': ["ID", "Model", "Int", "Pos X", "Pos Y", "Pos Z",
                     "Scale X", "Scale Y", "Scale Z", "Rot X", "Rot Y", "Rot Z", "Rot W"],
            'grge': ["X1", "Y1", "Z1", "Front X", "Front Y", "X2", "Y2", "Z2",
                     "Door Type", "Garage Type", "Name"],
            'enex': ["Enter X", "Enter Y", "Enter Z", "Enter Angle",
                     "Size X", "Size Y", "Size Z", "Exit X", "Exit Y", "Exit Z",
                     "Exit Angle", "Target Int", "Flags", "Name",
                     "Sky", "Num Peds", "Time On", "Time Off"],
            'path': ["Type / Group A", "Next / Group B", "Zero", "X", "Y", "Z",
                     "Median", "Left", "Right", "Flag1", "Flag2", "Flag3", "Flag4?"],
            # 'zone' (Aug 16 2026)(
            'zone': ["Name", "Type", "Min X", "Min Y", "Min Z",
                     "Max X", "Max Y", "Max Z", "Island", "Text Key"],
            # 'cull' (Aug 16 2026)
            'cull': ["Center X", "Center Y", "Center Z",
                     "X1", "Y1", "Z1", "X2", "Y2", "Z2",
                     "Flags", "Wanted Drop"],
            # 'occl' (Aug 16 2026)
            'occl': ["Mid X", "Mid Y", "Bottom Z",
                     "Width X", "Width Y", "Height", "Rotation"],
            # 'auzo' (Aug 20 2026)
            'auzo': ["Name", "Sound ID", "Switch", "Shape",
                     "X1", "Y1", "Z1", "X2 / Radius", "Y2", "Z2",
                     "Environment", "Music / Ambience"],
        }
        headers = headers_by_type.get(data_type, headers_by_type['inst'])
        if table.columnCount() != len(headers):
            table.setColumnCount(len(headers))
        table.setHorizontalHeaderLabels(headers)
        col_count = len(headers)

        ignore_scaling = getattr(self, '_ignore_scaling_chk', None)
        ignore_scaling = ignore_scaling.isChecked() if ignore_scaling is not None else False

        # LOD display mode filter (Aug 1 2026)
        lod_mode = getattr(self, '_lod_display_mode', 'normal')
        threshold = self.map_settings.get('lod_draw_dist_threshold')
        loader = getattr(self, '_world_loader', None)

        data_lines = []
        tobj_lines = []   # collected separately, appended after the loop (Show Tobj)
        show_tobj_chk = getattr(self, '_show_tobj_chk', None)
        show_tobj = show_tobj_chk.isChecked() if show_tobj_chk is not None else False
        tobj_hour = None
        if show_tobj and data_type == 'inst':
            tobj_time_spin = getattr(self, '_tobj_time_spin', None)
            if tobj_time_spin is not None:
                tobj_hour = tobj_time_spin.time().hour()
        for line_no, raw_line in section_lines:
            line = raw_line.split("#")[0].strip()
            low = line.lower()
            if not line or low in (data_type, 'end'):
                continue
            fields = [f.strip() for f in line.split(',')]

            # Path section: group-header line vs indented sub-node line (Aug 14 2026)
            is_group_header = data_type == 'path' and raw_line[:1] not in ('\t', ' ')
            if data_type == 'inst' and fields and loader is not None:
                # TOBJ handling (Aug 1 2026)
                try:
                    tobj_obj = loader.get_object(int(fields[0]))
                except ValueError:
                    tobj_obj = None
                if tobj_obj is not None and tobj_obj.section == 'tobj':
                    if not show_tobj:
                        continue
                    time_on = tobj_obj.extra.get('time_on')
                    time_off = tobj_obj.extra.get('time_off')
                    active = True
                    if tobj_hour is not None and time_on is not None and time_off is not None:
                        if time_on <= time_off:
                            active = time_on <= tobj_hour < time_off
                        else:
                            active = tobj_hour >= time_on or tobj_hour < time_off
                    if active:
                        tobj_lines.append((line_no, fields, False))
                    continue
            if data_type == 'inst' and lod_mode != 'both':
                model_name = fields[1] if len(fields) > 1 else ''
                is_lod = model_name.lower().startswith('lod')
                # Draw-distance-based LOD check too (Aug 1 2026)
                if not is_lod and loader is not None and fields:
                    try:
                        obj = loader.get_object(int(fields[0]))
                    except ValueError:
                        obj = None
                    if obj is not None:
                        draw_dist = obj.extra.get('draw_dist')
                        if draw_dist is not None and draw_dist > threshold:
                            is_lod = True
                if lod_mode == 'normal' and is_lod:
                    continue
                if lod_mode == 'lod' and not is_lod:
                    continue
            data_lines.append((line_no, fields, is_group_header))
        data_lines.extend(tobj_lines)

        table.setRowCount(len(data_lines))
        for r, (line_no, fields, is_group_header) in enumerate(data_lines):
            for c in range(col_count):
                value = fields[c] if c < len(fields) else ""
                if ignore_scaling and 6 <= c <= 8 and value == "1" and data_type == 'inst':
                    value = "0"
                item = QTableWidgetItem(value)
                if c == 0:
                    item.setData(Qt.ItemDataRole.UserRole, line_no)
                if is_group_header:
                    # Path group-header row (2 fields, not a node)
                    font = item.font()
                    font.setBold(True)
                    item.setFont(font)
                    item.setBackground(QBrush(QColor(60, 60, 90)))
                table.setItem(r, c, item)

    def _on_ipl_inst_file_context_menu(self, pos): #vers 3
        """Right-click the IPL Inst File table."""
        table = getattr(self, '_ipl_inst_file_table', None)
        if table is None:
            return
        selected = table.selectedItems()
        if not selected:
            return
        row = selected[0].row()
        menu = QMenu(self)
        copy_cells_action = menu.addAction("Copy")
        copy_rows_action = menu.addAction("Copy Row(s) as IPL line(s)")
        menu.addSeparator()
        info_action = menu.addAction("Info")
        show_textures_action = menu.addAction("Show Textures")
        menu.addSeparator()
        save_action = menu.addAction("Save IPL")
        save_as_action = menu.addAction("Save IPL Data As...")
        # Path group editing (Aug 16 2026)
        edit_path_group_action = None
        delete_path_group_action = None
        new_path_group_action = None
        reverse_flow_action = None
        loader_for_menu = getattr(self, '_world_loader', None)
        if (getattr(self, '_ipl_data_type', 'inst') == 'path'
                and getattr(loader_for_menu, 'game', None) != 'gta3'):
            menu.addSeparator()
            edit_path_group_action = menu.addAction("Edit Path Group...")
            delete_path_group_action = menu.addAction("Delete Path Group")
            new_path_group_action = menu.addAction("New Path Group...")
            # Reverse Traffic Flow (Aug 18 2026)
            reverse_flow_action = menu.addAction("Reverse Traffic Flow")
            resolved_group = self._resolve_path_group_for_row(row)
            if resolved_group is not None and self._is_path_group_simple_chain(resolved_group):
                reverse_flow_action.setToolTip(
                    "Swap this group's direction of travel end to end.")
            else:
                reverse_flow_action.setEnabled(False)
                reverse_flow_action.setToolTip(
                    "Not available for this group - it has a junction where\n"
                    "multiple nodes merge into one. Each node can only point\n"
                    "to a single 'next' node, so a merge point can't become a\n"
                    "split point after reversing; only simple chains (no\n"
                    "merging) can be reversed without losing connections.")
        chosen = menu.exec(table.viewport().mapToGlobal(pos))
        if chosen is None:
            return
        if chosen is copy_cells_action:
            clipboard = QApplication.clipboard()
            by_row = {}
            for item in selected:
                by_row.setdefault(item.row(), {})[item.column()] = item.text()
            lines = []
            for r in sorted(by_row):
                cols = by_row[r]
                lines.append('\t'.join(cols.get(c, '') for c in sorted(cols)))
            clipboard.setText('\n'.join(lines))
        elif chosen is copy_rows_action:
            clipboard = QApplication.clipboard()
            rows = sorted({item.row() for item in selected})
            lines = []
            for r in rows:
                fields = [table.item(r, c).text() if table.item(r, c) else ''
                         for c in range(table.columnCount())]
                lines.append(', '.join(fields))
            clipboard.setText('\n'.join(lines))
        elif chosen is info_action:
            inst = self._find_instance_for_ipl_inst_file_row(row)
            if inst is not None:
                self._center_viewport_on_instance(inst)
                self._center_on_instance(inst)
        elif chosen in (save_action, save_as_action):
            sections_table = getattr(self, '_ipl_sections_table', None)
            sec_row = sections_table.currentRow() if sections_table is not None else -1
            item = sections_table.item(sec_row, 0) if sec_row >= 0 else None
            display_name = item.data(Qt.ItemDataRole.UserRole) if item is not None else None
            if display_name:
                if chosen is save_action:
                    self._save_ipl_in_place(display_name)
                else:
                    self._save_ipl_data_as_full(display_name)
        elif chosen is show_textures_action:
            inst = self._find_instance_for_ipl_inst_file_row(row)
            if inst is not None:
                self._show_textures_for_instance(inst)
        elif edit_path_group_action is not None and chosen is edit_path_group_action:
            self._edit_path_group_for_row(row)
        elif delete_path_group_action is not None and chosen is delete_path_group_action:
            self._delete_path_group_for_row(row)
        elif new_path_group_action is not None and chosen is new_path_group_action:
            self._new_path_group()
        elif reverse_flow_action is not None and chosen is reverse_flow_action:
            if resolved_group is not None:
                self._reverse_path_group_traffic_flow(resolved_group)

    def _find_instance_for_ipl_inst_file_row(self, row): #vers 2
        """Look up the real IPLInstance for a row in the IPL Inst File
        table, matched by ID + Model name - factored out of
        _on_ipl_inst_file_cell_double_clicked so the context menu's
        Info/Show Textures can reuse the same lookup.

        Also disambiguates by position (Sep 5 2026,  "Could
        there be a conflict in the functions... where is 146.3 -90
        146.3 coming from" + "I've noticed some SA IPLs loading in SOL
        with the wrong data as well... only affects objects loaded in
        the GTASOL profile") - matching by name+id alone returned
        whichever instance happened to be first anywhere in the whole
        loaded world, which is fine for a standalone VC/SA/LC world
        (a given model_id rarely repeats) but wrong for SOL, which
        merges multiple cities into one world - common generic models
        (lcport43, GenVCapsteps1, etc.) legitimately appear dozens of
        times across different sub-cities, all sharing the same
        model_id+name. Double-clicking one specific row could silently
        return a completely different instance's own data instead,
        including its own different rotation - not a parsing bug at
        all, which is why it affected both SA- and VC-format objects
        equally and only ever showed up in SOL. Now reads the row's
        own Pos X/Y/Z columns (present for 'inst' rows) and picks
        whichever name+id match sits closest to that real position,
        rather than just the first one found."""
        table = getattr(self, '_ipl_inst_file_table', None)
        if table is None:
            return None
        id_item = table.item(row, 0)
        model_item = table.item(row, 1)
        if id_item is None or model_item is None:
            return None
        try:
            target_id = int(id_item.text())
        except ValueError:
            target_id = None
        target_model = model_item.text()

        target_pos = None
        if getattr(self, '_ipl_data_type', 'inst') == 'inst':
            px_item, py_item, pz_item = (table.item(row, c) for c in (3, 4, 5))
            if px_item is not None and py_item is not None and pz_item is not None:
                try:
                    target_pos = (float(px_item.text()), float(py_item.text()), float(pz_item.text()))
                except ValueError:
                    target_pos = None

        candidates = [inst for inst in getattr(self, '_all_instances', [])
                      if inst.model_name == target_model
                      and (target_id is None or inst.model_id == target_id)]
        if not candidates:
            candidates = [inst for inst in getattr(self, '_all_instances', [])
                          if inst.model_name == target_model]
        if not candidates:
            return None
        if len(candidates) == 1 or target_pos is None:
            return candidates[0]
        tx, ty, tz = target_pos
        return min(candidates, key=lambda i: (i.pos_x-tx)**2 + (i.pos_y-ty)**2 + (i.pos_z-tz)**2)

    def _find_path_group_for_line(self, display_name, line_no): #vers 1
        """Find the real, live PathGroup a clicked IPL File Display
        row belongs to, given the currently-selected file's name and
        that row's own source-file line number (Aug 16 2026)"""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return None
        candidates = [g for g in getattr(loader, 'paths', [])
                     if g.source_ipl == display_name and g.line_no <= line_no]
        if not candidates:
            return None
        return max(candidates, key=lambda g: g.line_no)

    def _resolve_path_group_for_row(self, row): #vers 1
        """Shared resolution helper (Aug 18 2026)"""
        table = getattr(self, '_ipl_inst_file_table', None)
        if table is None:
            return None
        line_item = table.item(row, 0)
        line_no = line_item.data(Qt.ItemDataRole.UserRole) if line_item is not None else None
        if line_no is None:
            return None
        sections_table = getattr(self, '_ipl_sections_table', None)
        sec_row = sections_table.currentRow() if sections_table is not None else -1
        sec_item = sections_table.item(sec_row, 0) if sec_row >= 0 else None
        display_name = sec_item.data(Qt.ItemDataRole.UserRole) if sec_item is not None else None
        if not display_name:
            return None
        group = self._find_path_group_for_line(display_name, line_no)
        if group is None:
            self._set_status("Couldn't find a path group for this row")
        return group

    def _is_path_group_simple_chain(self, group): #vers 1
        """Whether every real node in this group has at most one
        other node pointing to it (Aug 18 2026)"""
        n = len(group.nodes)
        incoming = {}
        for node in group.nodes:
            if node.node_type == 0:
                continue
            if 0 <= node.next_id < n and group.nodes[node.next_id].node_type != 0:
                incoming[node.next_id] = incoming.get(node.next_id, 0) + 1
        return all(count <= 1 for count in incoming.values())

    def _reverse_path_group_traffic_flow(self, group): #vers 1
        """Reverse a path group's direction of travel by swapping
        every edge (Aug 18 2026)"""
        n = len(group.nodes)
        new_next = [-1] * n
        for i, node in enumerate(group.nodes):
            if node.node_type == 0:
                continue
            if 0 <= node.next_id < n and group.nodes[node.next_id].node_type != 0:
                new_next[node.next_id] = i
        for i, node in enumerate(group.nodes):
            if node.node_type != 0:
                node.next_id = new_next[i]
        self._refresh_path_visualization()
        self._set_status(f"Reversed traffic flow for path group ({group.header_a}, {group.header_b})")

    def _edit_path_group_for_row(self, row): #vers 1
        """Open the Path Group Editor for whichever path group the
        given IPL File Display row belongs to (Aug 16 2026)"""
        group = self._resolve_path_group_for_row(row)
        if group is None:
            return
        dlg = _PathGroupEditDialog(group, self, parent=self)
        dlg.exec()

    def _delete_path_group_for_row(self, row): #vers 2
        """Delete whichever path group the given IPL File Display row
        belongs to, entirely (Aug 16 2026)"""
        group = self._resolve_path_group_for_row(row)
        if group is None:
            return
        loader = getattr(self, '_world_loader', None)
        if loader is not None and group in loader.paths:
            loader.paths.remove(group)
        self._refresh_path_visualization()
        self._set_status(f"Deleted path group ({group.header_a}, {group.header_b})")

    def _new_path_group(self): #vers 1
        """Create a brand-new, empty path group for the currently-
        selected IPL, then open it straight in the Path Group Editor
        to fill in (Aug 16 2026)"""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        from apps.methods.gta_dat_parser import PathGroup, PathNode
        sections_table = getattr(self, '_ipl_sections_table', None)
        sec_row = sections_table.currentRow() if sections_table is not None else -1
        sec_item = sections_table.item(sec_row, 0) if sec_row >= 0 else None
        display_name = sec_item.data(Qt.ItemDataRole.UserRole) if sec_item is not None else None
        if not display_name:
            self._set_status("Select a loaded IPL first")
            return

        pw = getattr(self, 'preview_widget', None)
        cx = -getattr(pw, '_pan_x', 0.0) if pw is not None else 0.0
        cy = -getattr(pw, '_pan_y', 0.0) if pw is not None else 0.0
        cz = 0.0

        existing = [g for g in loader.paths if g.source_ipl == display_name]
        new_line_no = (max((g.line_no for g in existing), default=0) + 1000)

        nodes = [PathNode(node_type=0, next_id=-1, x=cx, y=cy, z=cz) for _ in range(12)]
        group = PathGroup(header_a=0, header_b=-1, nodes=nodes,
                          source_ipl=display_name, line_no=new_line_no)
        loader.paths.append(group)
        self._refresh_path_visualization()
        self._set_status(f"Created new path group in {display_name} - edit its nodes now")
        dlg = _PathGroupEditDialog(group, self, parent=self)
        dlg.exec()

    def _show_textures_for_instance(self, inst): #vers 2
        """Load one instance's model's textures into the Textures
        dock - same lookup/population logic as
        _on_ipl_model_row_selected, just entered from the IPL Inst
        File table's right-click menu instead of the Models panel.

        Fixed (Aug 1 2026)"""
        loader = getattr(self, '_world_loader', None)
        model_cache = getattr(self, '_model_cache', None)
        tex_list = getattr(self, '_tex_list', None)
        if tex_list is None:
            return
        txd_name = ""
        ide_note = ""
        if loader is not None:
            obj = loader.get_object(inst.model_id)
            if obj is not None:
                txd_name = obj.txd_name or ""
                source_ide = getattr(obj, 'source_ide', '') or 'unknown.ide'
                line_no = getattr(obj, 'line_no', 0)
                ide_note = f" (TXD from {source_ide}, line {line_no})"
        if not txd_name:
            tex_list.setRowCount(0)
            if hasattr(self, '_tex_count_lbl'):
                self._tex_count_lbl.setText(
                    f"No TXD name found for {inst.model_name} (check its IDE entry)")
            return
        textures, source, _status = self._get_txd_textures(txd_name) if model_cache else (None, None, 'missing')
        textures = textures or {}
        tex_list.setRowCount(len(textures))
        for i, tex in enumerate(textures.values()):
            thumb_item = QTableWidgetItem()
            pixmap = self._create_texture_thumbnail(
                tex.get('rgba_data'), tex.get('width', 0), tex.get('height', 0))
            if pixmap is not None:
                thumb_item.setData(Qt.ItemDataRole.DecorationRole, pixmap)
            tex_list.setItem(i, 0, thumb_item)
            tex_list.setItem(i, 1, QTableWidgetItem(tex.get('name', '')))
            tex_list.setItem(i, 2, QTableWidgetItem(
                f"{tex.get('width', '?')}x{tex.get('height', '?')}"))
            tex_list.setItem(i, 3, QTableWidgetItem(tex.get('format', '')))
        if hasattr(self, '_tex_count_lbl'):
            if textures:
                self._tex_count_lbl.setText(
                    f"{len(textures)} texture{'s' if len(textures) != 1 else ''} "
                    f"({txd_name}.txd, from {source}){ide_note}")
            else:
                self._tex_count_lbl.setText(
                    f"{txd_name}.txd not found in any indexed IMG archive or as a "
                    f"loose file{ide_note}")


    def _on_ipl_inst_file_cell_clicked(self, row, col): #vers 1
        """Single-clicking any cell in a row centers the viewport on
        that row's real instance."""
        match = self._find_instance_for_ipl_inst_file_row(row)
        if match is not None:
            self._center_viewport_on_instance(match)
            self._center_on_instance(match)

    def _on_ipl_inst_file_cell_double_clicked(self, row, col): #vers 5
        """Double-clicking the Model column (1) finds that row's real
        instance and jumps the viewport to it + opens its edit panel,
        matching double-clicking a row in the Instance List.

         fix (Aug 20 2026,  "auzo list play the sounds")
        - the Auzo table's own columns (Name/Sound ID/Switch/Shape/...)
        are nothing like the Inst table's own Model-at-column-1 layout
        the col != 1 guard below assumes, so this real Auzo case is
        checked and handled first, before that guard would otherwise
        block it entirely.

         fix (Aug 21 2026  own real, uploaded zon.png
        screenshot: "wants to rename it, not take us to the zon
        entries that belongs to it") - the actual "wants to rename"
        symptom was a real, separate table-setup bug fixed above (see
        _create_ipl_inst_file_panel's own real setEditTriggers fix),
        not this handler. But this real handler itself only ever
        centered the viewport on the instance - never actually looked
        for a real zone the instance's own position falls inside, the
        real behaviour my own message describes wanting instead.
        Now also does that: finds the first loaded zone box whose own
        real (min/max) bounds contain the instance's own real (x,y)
        position, and highlights it the same real way the Cycle Zones
        button's own picker does, if one is found."""
        if getattr(self, '_ipl_data_type', 'inst') == 'auzo':
            table = getattr(self, '_ipl_inst_file_table', None)
            if table is None:
                return
            name_item = table.item(row, 0)
            sound_id_item = table.item(row, 1)
            if name_item is None or sound_id_item is None:
                return
            try:
                sound_id = int(sound_id_item.text())
            except ValueError:
                return
            self._play_auzo_placeholder_tone(sound_id, name_item.text())
            return
        if col != 1:
            return
        match = self._find_instance_for_ipl_inst_file_row(row)
        if match is not None:
            self._center_viewport_on_instance(match)
            self._center_on_instance(match)
            self._highlight_zone_containing_instance(match)

    def _highlight_zone_containing_instance(self, inst): #vers 1
        """Find the first loaded zone box whose own real bounds
        contain inst's own real (x,y) position, and highlight it the
        same real way the Cycle Zones button's own picker does (Aug
        21 2026  own real, uploaded zon.png screenshot:
        "not take us to the zon entries that belongs to it"). Doesn't
        re-center the viewport (the caller already did that, on the
        instance itself) - only sets the real highlight state, so
        both the instance and whichever zone contains it are visibly
        marked together. Silently does nothing if no loaded zone's
        own real bounds contain this instance at all - not every
        instance is inside a zone."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None:
            return
        zone_boxes = getattr(vp, '_zone_boxes', [])
        px, py = inst.pos_x, inst.pos_y
        for i, (x1, y1, z1, x2, y2, z2) in enumerate(zone_boxes):
            if min(x1, x2) <= px <= max(x1, x2) and min(y1, y2) <= py <= max(y1, y2):
                vp._selected_box = ('zone', i)
                vp.update()
                owners = getattr(vp, '_zone_box_owners', [])
                name = owners[i].get('name', f'zone {i}') if i < len(owners) and isinstance(owners[i], dict) else f'zone {i}'
                self._set_status(f"Instance is inside zone '{name}'")
                return

    def _center_viewport_on_instance(self, inst): #vers 4
        """Pan/zoom self.preview_widget (our actual active viewport)
        to focus closely on one instance's position - _center_on_
        instance (existing, ported code) only ever updated the
        Map-specific _world_panes, which this build doesn't use (see
        CHANGELOG.md - "keep using Model Workshop's existing DFF
        viewport"), so it never actually moved anything visible here.

        focus_from_above (Sep 5 2026,  "when selecting a
        model in map workshop, can we change the camera view, have a
        setting, view from 0, +200 so we dont view the model from the
        bottom, we see it from the top instead") - off by default;
        when on, also resets yaw/pitch to a fixed from-above angle
        rather than leaving whatever angle the camera already
        happened to be at, which could be looking up from
        underneath the model."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None:
            return
        vp._pan_x = -inst.pos_x
        vp._pan_y = -inst.pos_y
        if self.map_settings.get('focus_from_above', False):
            vp._yaw = 0.0
            vp._pitch = 89.0
            vp._dist = self.map_settings.get('focus_from_above_dist', 200.0)
        else:
            vp._dist = getattr(self, '_goto_zoom_distance', 40.0)
        if getattr(vp, '_projection', None) == 'ortho':
            try:
                vp.resizeGL(vp.width(), vp.height())
            except Exception:
                pass
        vp.update()

    def _on_world_instance_picked(self, index): #vers 2
        """Called by DFFViewport.mouseDoubleClickEvent when the user
        double-clicks an object in the 3D world view - Aug 1 2026)"""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not (0 <= index < len(vp._world_instances)):
            return
        inst = vp._world_instances[index].get('instance')
        if inst is not None:
            self._center_viewport_on_instance(inst)
            self._center_on_instance(inst)
            self._sync_ipl_inst_file_selection(inst)

    def _on_path_node_picked(self, node_pos, owner): #vers 1
        """Called by DFFViewport.mouseDoubleClickEvent when the user
        double-clicks a path node in the 3D world view (Aug 21 2026,
         "when clicking on paths, or zons, other then ipl
        models, nothing comes up") - centers the viewport on the node
        (same real feedback double-clicking a regular instance already
        gives) and reports its own real data via the status bar, since
        a path node has no dedicated edit dialog of its own yet (see
        TODO.md's own "Path section - status update" entry - add/
        remove/flags editing isn't built)."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None:
            return
        x, y, z = node_pos
        vp._pan_x = -x
        vp._pan_y = -y
        vp._dist = getattr(self, '_goto_zoom_distance', 40.0)
        vp.update()
        if owner is not None:
            group, node_index = owner
            node = group.nodes[node_index] if 0 <= node_index < len(group.nodes) else None
            if node is not None:
                self._set_status(
                    f"Path node {node_index} of group ({group.header_a}, "
                    f"{group.header_b}) in {group.source_ipl} - type={node.node_type}, "
                    f"pos=({x:.1f}, {y:.1f}, {z:.1f})")
                return
        self._set_status(f"Path node at ({x:.1f}, {y:.1f}, {z:.1f})")

    def _on_cull_or_zone_box_picked(self, kind, box_index): #vers 3
        """Called by DFFViewport.mouseDoubleClickEvent when the user
        double-clicks a cull/zone/occlusion box in the 3D world view
        (Aug 21 2026,  "when clicking on paths, or zons,
        other then ipl models, nothing comes up"; occl added same day
        per my own follow-up "both if you can") - centers the
        viewport on the box (same real feedback double-clicking a
        regular instance already gives) and reports its own real data
        via the status bar, since none of the three have a dedicated
        edit dialog of their own yet beyond corner-drag resizing (cull/
        zone only - see TODO.md).

        Also sets the real box highlight (Aug 21 2026 
        own follow-up "show the zon box highlighted") - the same real
        state the Cycle Zones button's own left-click/right-click
        picker set, so a direct 3D double-click and the fallback
        cycle control both leave the viewport in the same real,
        visibly-highlighted state either way.

        Occlusion uses a genuinely different real tuple shape (mid_x/
        mid_y/bottom_z/width_x/width_y/height/rotation, since it can
        be rotated) than cull/zone's own plain (x1,y1,z1,x2,y2,z2) -
        handled as its own real branch rather than trying to force it
        through the same unpacking."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None:
            return

        if kind == 'occl':
            occl_boxes = getattr(vp, '_occl_boxes', [])
            if not (0 <= box_index < len(occl_boxes)):
                return
            vp._selected_box = (kind, box_index)
            mid_x, mid_y, bottom_z, width_x, width_y, height, rotation = occl_boxes[box_index]
            vp._pan_x = -mid_x
            vp._pan_y = -mid_y
            vp._dist = max(getattr(self, '_goto_zoom_distance', 40.0), width_x, width_y)
            vp.update()
            self._set_status(
                f"Occlusion box {box_index} - centre ({mid_x:.1f}, {mid_y:.1f}, "
                f"{bottom_z:.1f}), size {width_x:.1f}x{width_y:.1f}x{height:.1f}, "
                f"rotation {rotation:.1f}\u00b0")
            return

        boxes = {'cull': vp._cull_boxes, 'zone': vp._zone_boxes,
                 'grge': getattr(vp, '_grge_boxes', [])}.get(kind, [])
        owners = getattr(vp, {'cull': '_cull_box_owners', 'zone': '_zone_box_owners',
                               'grge': '_grge_box_owners'}.get(kind, ''), [])
        if not (0 <= box_index < len(boxes)):
            return
        vp._selected_box = (kind, box_index)
        x1, y1, z1, x2, y2, z2 = boxes[box_index]
        cx, cy = (x1 + x2) / 2.0, (y1 + y2) / 2.0
        vp._pan_x = -cx
        vp._pan_y = -cy
        vp._dist = max(getattr(self, '_goto_zoom_distance', 40.0),
                       abs(x2 - x1), abs(y2 - y1))
        vp.update()
        owner = owners[box_index] if 0 <= box_index < len(owners) else None
        label = kind.capitalize()
        if kind == 'zone' and isinstance(owner, dict):
            name = owner.get('name', f'zone {box_index}')
            self._set_status(
                f"Zone '{name}' - bounds ({x1:.1f}, {y1:.1f}, {z1:.1f}) to "
                f"({x2:.1f}, {y2:.1f}, {z2:.1f})")
        elif kind == 'cull' and owner is not None:
            self._set_status(
                f"Cull zone {box_index} - bounds ({x1:.1f}, {y1:.1f}, {z1:.1f}) to "
                f"({x2:.1f}, {y2:.1f}, {z2:.1f}), flags={getattr(owner, 'flags', '?')}")
        elif kind == 'grge' and owner is not None:
            self._set_status(
                f"Garage '{getattr(owner, 'name', box_index)}' - bounds "
                f"({x1:.1f}, {y1:.1f}, {z1:.1f}) to ({x2:.1f}, {y2:.1f}, {z2:.1f}), "
                f"door_type={getattr(owner, 'door_type', '?')}, "
                f"garage_type={getattr(owner, 'garage_type', '?')}")
        else:
            self._set_status(
                f"{label} box {box_index} - bounds ({x1:.1f}, {y1:.1f}, {z1:.1f}) to "
                f"({x2:.1f}, {y2:.1f}, {z2:.1f})")

    def _combined_box_list(self): #vers 3
        """Return every loaded cull/zone/occlusion/garage box as a
        flat, consistently-ordered list of ('cull'|'zone'|'occl'|
        'grge', index) tuples (Aug 21 2026, shared by the Cycle Zones
        button's own left-click-to-advance and right-click-to-pick-
        directly handlers below) - cull boxes first, then zone, then
        occlusion, then garages, matching the order they're already
        drawn in."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None:
            return []
        return ([('cull', i) for i in range(len(getattr(vp, '_cull_boxes', [])))] +
                [('zone', i) for i in range(len(getattr(vp, '_zone_boxes', [])))] +
                [('occl', i) for i in range(len(getattr(vp, '_occl_boxes', [])))] +
                [('grge', i) for i in range(len(getattr(vp, '_grge_boxes', [])))])

    def _cycle_selected_box(self): #vers 1
        """Cycle Zones button, left-click (Aug 21 2026,  "on
        zons we could also cycle through the entries list, and show
        the zon box highlighted... this would be a failback, other
        then clicking on the zon box") - steps to the next cull/zone
        box in the combined list, wrapping around, reusing _on_cull_
        or_zone_box_picked directly for the actual centering/
        highlighting/status-reporting."""
        combined = self._combined_box_list()
        if not combined:
            self._set_status("No cull/zone boxes loaded")
            return
        vp = getattr(self, 'preview_widget', None)
        current = getattr(vp, '_selected_box', None) if vp else None
        if current in combined:
            next_index = (combined.index(current) + 1) % len(combined)
        else:
            next_index = 0
        kind, box_index = combined[next_index]
        self._on_cull_or_zone_box_picked(kind, box_index)

    def _show_box_picker_menu(self, cycle_btn): #vers 3
        """Cycle Zones button, right-click (Aug 21 2026 
        own follow-up: "with right click options, this would be a
        failback"; occl/grge added same day per my own follow-up
        "both if you can") - lists every real cull/zone/occlusion/
        garage box currently loaded, by real name where one exists
        (zones, garages) or index (cull/occl, neither has a name
        field), to jump directly to any one rather than only stepping
        through sequentially."""
        from PyQt6.QtWidgets import QMenu
        vp = getattr(self, 'preview_widget', None)
        combined = self._combined_box_list()
        menu = QMenu(self)
        if not combined or vp is None:
            act = menu.addAction("No cull/zone/occlusion/garage boxes loaded")
            act.setEnabled(False)
        else:
            zone_owners = getattr(vp, '_zone_box_owners', [])
            grge_owners = getattr(vp, '_grge_box_owners', [])
            for kind, box_index in combined:
                if kind == 'zone' and box_index < len(zone_owners) and isinstance(zone_owners[box_index], dict):
                    label = f"Zone: {zone_owners[box_index].get('name', box_index)}"
                elif kind == 'grge' and box_index < len(grge_owners):
                    label = f"Garage: {getattr(grge_owners[box_index], 'name', box_index)}"
                elif kind == 'occl':
                    label = f"Occlusion box {box_index}"
                else:
                    label = f"Cull zone {box_index}"
                act = menu.addAction(label)
                act.triggered.connect(
                    lambda checked=False, k=kind, i=box_index: self._on_cull_or_zone_box_picked(k, i))
        menu.exec(cycle_btn.mapToGlobal(cycle_btn.rect().bottomLeft()))

    def _box_corners_3d(self, kind, box_index): #vers 1
        """Return all 8 real (x,y,z) corners of one box, any type
        (Aug 21 2026,  "add show coords for all corners") -
        or None if the index doesn't resolve. Cull/zone/grge are a
        plain AABB, so this is just the 2x2x2 combination of their
        own (x1/x2, y1/y2, z1/z2); occlusion is rotated, so its own 4
        XY corners are computed the same real way _draw_occl_boxes
        already does, each duplicated at its own bottom_z/bottom_z+
        height."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None:
            return None
        if kind == 'occl':
            boxes = getattr(vp, '_occl_boxes', [])
            if not (0 <= box_index < len(boxes)):
                return None
            mid_x, mid_y, bottom_z, width_x, width_y, height, rotation = boxes[box_index]
            hw, hh = width_x / 2.0, width_y / 2.0
            rad = math.radians(rotation)
            cos_r, sin_r = math.cos(rad), math.sin(rad)
            xy = []
            for dx, dy in ((-hw, -hh), (hw, -hh), (hw, hh), (-hw, hh)):
                xy.append((mid_x + dx * cos_r - dy * sin_r, mid_y + dx * sin_r + dy * cos_r))
            z1, z2 = bottom_z, bottom_z + height
        else:
            boxes = {'cull': vp._cull_boxes, 'zone': vp._zone_boxes,
                     'grge': getattr(vp, '_grge_boxes', [])}.get(kind, [])
            if not (0 <= box_index < len(boxes)):
                return None
            x1, y1, z1, x2, y2, z2 = boxes[box_index]
            xy = [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
        return [(x, y, z1) for x, y in xy] + [(x, y, z2) for x, y in xy]

    def _show_box_corner_coords(self, kind, box_index): #vers 1
        """Show every real (x,y,z) corner of one box in a plain
        message box (Aug 21 2026,  "add show coords for all
        corners") - available from any overlay button's own middle-
        click menu, for whichever box is currently selected there."""
        corners = self._box_corners_3d(kind, box_index)
        if corners is None:
            QMessageBox.information(self, "Corner Coordinates", "No box currently selected.")
            return
        labels = ["Bottom NW", "Bottom NE", "Bottom SE", "Bottom SW",
                  "Top NW", "Top NE", "Top SE", "Top SW"]
        lines = [f"{label}: ({x:.3f}, {y:.3f}, {z:.3f})"
                 for label, (x, y, z) in zip(labels, corners)]
        QMessageBox.information(
            self, "Corner Coordinates",
            f"{kind.capitalize()} box {box_index}:\n\n" + "\n".join(lines))

    def _show_overlay_middle_click_menu(self, kind, add_fn, delete_fn, save_fn): #vers 2
        """Build and show one overlay button's own middle-click menu
        (Aug 21 2026,  "the new buttons, +zon, -zon, and
        save zon... add these to the svg icon zon button with a
        middle-click, and move those functions over. Same with cull,
        occl, auzo, grge") - one real, shared menu-builder rather than
        4 near-identical copies, parameterized by which real add/
        delete/save methods this particular box type actually uses.

         fix (Aug 21 2026,  "Show coords does not work
        on occl, zon or cull?") - "Show Corner Coordinates" used to
        only ever enable once a box of this exact type was already
        the one selected via Cycle Zones - genuinely disabled/inert
        otherwise, with no obvious reason why to anyone who hadn't
        used Cycle first. Now its own real submenu, listing every box
        of this exact type currently loaded (by real name where one
        exists - zone, garage - or index otherwise), same real
        pattern the Cycle button's own right-click picker already
        uses - pick one directly, no prior Cycle step required at
        all."""
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        menu.addAction(f"Add {kind.capitalize()}", add_fn)
        menu.addAction(f"Delete selected {kind.capitalize()}", delete_fn)
        menu.addAction(f"Save {kind.capitalize()}(s)", save_fn)
        menu.addSeparator()

        vp = getattr(self, 'preview_widget', None)
        boxes_of_kind = {'cull': getattr(vp, '_cull_boxes', []),
                          'zone': getattr(vp, '_zone_boxes', []),
                          'occl': getattr(vp, '_occl_boxes', []),
                          'grge': getattr(vp, '_grge_boxes', [])}.get(kind, []) if vp else []
        coords_menu = menu.addMenu("Show Corner Coordinates")
        if not boxes_of_kind:
            act = coords_menu.addAction(f"No {kind} boxes loaded")
            act.setEnabled(False)
        else:
            owners = {'zone': getattr(vp, '_zone_box_owners', []),
                      'grge': getattr(vp, '_grge_box_owners', [])}.get(kind, [])
            for i in range(len(boxes_of_kind)):
                if kind == 'zone' and i < len(owners) and isinstance(owners[i], dict):
                    label = owners[i].get('name', f'{kind} {i}')
                elif kind == 'grge' and i < len(owners):
                    label = getattr(owners[i], 'name', f'{kind} {i}')
                else:
                    label = f"{kind.capitalize()} {i}"
                act = coords_menu.addAction(label)
                act.triggered.connect(
                    lambda checked=False, k=kind, idx=i: self._show_box_corner_coords(k, idx))

        btn = self.sender()
        pos = btn.mapToGlobal(btn.rect().bottomLeft()) if btn is not None else self.mapToGlobal(self.rect().center())
        menu.exec(pos)

    def _repair_scale_instances(self, from_scale, to_scale): #vers 1
        """Generic version of Repair Scale - fixes every currently
        loaded instance whose scale is exactly `from_scale` to
        `to_scale`. Backs both directions on the Repair Scale ribbon
        button (Sep 5 2026,  "add another option 1, 1, 1,
        to 0, 0, 0") - the original 0,0,0 -> 1,1,1 direction (Aug 21
        2026  own real worked VC/SA/SOL example) plus its
        exact reverse. IN MEMORY ONLY - no write-back to disk for
        INST lines exists yet, same as before."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            self._set_status("No world loaded")
            return
        instances = getattr(loader, 'instances', [])
        fx, fy, fz = from_scale
        tx, ty, tz = to_scale
        broken = [inst for inst in instances
                  if inst.scale_x == fx and inst.scale_y == fy and inst.scale_z == fz]
        if not broken:
            self._set_status(f"No {from_scale} scale instances found")
            return
        originals = [(inst.scale_x, inst.scale_y, inst.scale_z) for inst in broken]

        def _do_fix():
            for inst in broken:
                inst.scale_x, inst.scale_y, inst.scale_z = tx, ty, tz
            self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=True)

        def _do_undo():
            for inst, orig in zip(broken, originals):
                inst.scale_x, inst.scale_y, inst.scale_z = orig
            self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=True)

        _do_fix()
        self._push_map_undo(_do_undo, _do_fix,
            f"Repair {len(broken)} instance(s) {from_scale}->{to_scale}")
        self._set_status(
            f"Repaired {len(broken)} instance(s) {from_scale}->{to_scale} - not yet saved to disk")

    def _repair_zero_scale_instances(self): #vers 2
        """Repair Scale button, default direction: 0,0,0 -> 1,1,1
        (Aug 21 2026  own real, worked VC/SA/SOL example
        lines and a real, broken converted line - the real, worked
        example itself confirms a real zero scale is what's actually
        broken, not the other direction, since a real, valid VC
        instance always has scale (1,1,1) unless deliberately, non-
        zero-scaled)."""
        self._repair_scale_instances((0.0, 0.0, 0.0), (1.0, 1.0, 1.0))

    def _repair_one_scale_instances(self): #vers 1
        """Repair Scale button, reverse direction: 1,1,1 -> 0,0,0
        (Sep 5 2026,  "add another option 1, 1, 1, to 0, 0,
        0") - for IPL data converted the other way round, where a
        genuinely-zero scale got mapped to (1,1,1) instead."""
        self._repair_scale_instances((1.0, 1.0, 1.0), (0.0, 0.0, 0.0))

    def _show_repair_scale_menu(self, btn): #vers 1
        """Right-click menu for Repair Scale - pick which direction to
        repair (Sep 5 2026), same right-click-for-options convention
        already used by Cycle/Convert on this same ribbon."""
        menu = QMenu(self)
        act_0to1 = menu.addAction("Repair 0,0,0 -> 1,1,1")
        act_1to0 = menu.addAction("Repair 1,1,1 -> 0,0,0")
        chosen = menu.exec(btn.mapToGlobal(btn.rect().bottomLeft()))
        if chosen is act_0to1:
            self._repair_zero_scale_instances()
        elif chosen is act_1to0:
            self._repair_one_scale_instances()

    def _add_zone(self): #vers 1
        """Add a new zone entry,  "we need to finish the
        add, del, save functions for zon" (Aug 21 2026) - IN MEMORY
        ONLY until Save Zones is used (see _write_back_zone_section
        below). Placed as a small (20x20x10) box centred on wherever
        the viewport is currently looking, in the first currently-
        loaded zone source file found (loader.zones' own source_ipl,
        my own "add" request has no way to ask which file a
        brand new zone should belong to, so this is a reasonable,
        visible default rather than silently picking nothing at
        all) - source_ipl can be changed later by editing the zone's
        own real data directly once a dedicated zone-editing UI
        exists (still open, see TODO.md)."""
        loader = getattr(self, '_world_loader', None)
        vp = getattr(self, 'preview_widget', None)
        if loader is None or vp is None:
            return
        existing = getattr(loader, 'zones', [])
        source_ipl = existing[0]['source_ipl'] if existing else 'map.zon'
        cx, cy = -vp._pan_x, -vp._pan_y
        new_zone = {
            'name': f'ZONE{len(existing)}', 'type': 0,
            'min_x': cx - 10.0, 'min_y': cy - 10.0, 'min_z': 0.0,
            'max_x': cx + 10.0, 'max_y': cy + 10.0, 'max_z': 10.0,
            'island': 0, 'text_key': '',
            'source_ipl': source_ipl, 'line_no': 0,
        }

        def _do_add():
            loader.zones.append(new_zone)
            self._refresh_zone_box_visualization()

        def _do_remove():
            if new_zone in loader.zones:
                loader.zones.remove(new_zone)
            self._refresh_zone_box_visualization()

        _do_add()
        self._push_map_undo(_do_remove, _do_add, f"Add zone '{new_zone['name']}'")
        self._set_status(f"Added zone '{new_zone['name']}' - not yet saved to disk")

    def _delete_zone(self): #vers 1
        """Delete the currently cycled/selected zone, .per:
        "we need to finish the add, del, save functions for zon" (Aug
        21 2026) - IN MEMORY ONLY until Save Zones is used. Acts on
        whichever zone the Cycle Zones button (or a direct viewport
        double-click) last selected (preview_widget._selected_box) -
        cull boxes have no delete support here since my own
        request named zon specifically."""
        loader = getattr(self, '_world_loader', None)
        vp = getattr(self, 'preview_widget', None)
        sel = getattr(vp, '_selected_box', None) if vp else None
        if loader is None or vp is None or sel is None or sel[0] != 'zone':
            self._set_status("No zone currently selected - use Cycle Zones first")
            return
        _, box_index = sel
        zones = getattr(loader, 'zones', [])
        if not (0 <= box_index < len(zones)):
            return
        removed_zone = zones[box_index]
        removed_name = removed_zone.get('name', box_index)

        def _do_remove():
            if removed_zone in loader.zones:
                loader.zones.remove(removed_zone)
            vp._selected_box = None
            self._refresh_zone_box_visualization()

        def _do_restore():
            loader.zones.insert(min(box_index, len(loader.zones)), removed_zone)
            self._refresh_zone_box_visualization()

        _do_remove()
        self._push_map_undo(_do_restore, _do_remove, f"Delete zone '{removed_name}'")
        self._set_status(f"Deleted zone '{removed_name}' - not yet saved to disk")

    def _add_cull(self): #vers 1
        """Add a new cull box,  "lets build add, del for
        zon, cull, occu and ipl changes" (Aug 21 2026) - IN MEMORY
        ONLY until Save Cull Zones is used. Placed as a small (20x20x
        10) box centred on wherever the viewport is currently
        looking, in the first currently-loaded cull source file found
        (same real "no way to ask which file" reasoning as _add_zone
        above)."""
        loader = getattr(self, '_world_loader', None)
        vp = getattr(self, 'preview_widget', None)
        if loader is None or vp is None:
            return
        from apps.methods.gta_dat_parser import CullEntry
        existing = getattr(loader, 'culls', [])
        source_ipl = existing[0].source_ipl if existing else 'cull.ipl'
        cx, cy = -vp._pan_x, -vp._pan_y
        new_cull = CullEntry(
            center_x=cx, center_y=cy, center_z=0.0,
            x1=cx - 10.0, y1=cy - 10.0, z1=0.0,
            x2=cx + 10.0, y2=cy + 10.0, z2=10.0,
            flags=0, wanted_level_drop=0,
            source_ipl=source_ipl, line_no=0)

        def _do_add():
            loader.culls.append(new_cull)
            self._refresh_cull_box_visualization()

        def _do_remove():
            if new_cull in loader.culls:
                loader.culls.remove(new_cull)
            self._refresh_cull_box_visualization()

        _do_add()
        self._push_map_undo(_do_remove, _do_add, "Add cull box")
        self._set_status("Added cull box - not yet saved to disk")

    def _delete_cull(self): #vers 1
        """Delete the currently cycled/selected cull box, .per:
        "lets build add, del for zon, cull, occu and ipl changes"
        (Aug 21 2026) - IN MEMORY ONLY until Save Cull Zones is used.
        Acts on whichever cull box the Cycle Zones button (or a
        direct viewport double-click) last selected (preview_widget.
        _selected_box)."""
        loader = getattr(self, '_world_loader', None)
        vp = getattr(self, 'preview_widget', None)
        sel = getattr(vp, '_selected_box', None) if vp else None
        if loader is None or vp is None or sel is None or sel[0] != 'cull':
            self._set_status("No cull box currently selected - use Cycle Zones first")
            return
        _, box_index = sel
        culls = getattr(loader, 'culls', [])
        if not (0 <= box_index < len(culls)):
            return
        removed_cull = culls[box_index]

        def _do_remove():
            if removed_cull in loader.culls:
                loader.culls.remove(removed_cull)
            vp._selected_box = None
            self._refresh_cull_box_visualization()

        def _do_restore():
            loader.culls.insert(min(box_index, len(loader.culls)), removed_cull)
            self._refresh_cull_box_visualization()

        _do_remove()
        self._push_map_undo(_do_restore, _do_remove, "Delete cull box")
        self._set_status("Deleted cull box - not yet saved to disk")

    def _write_back_cull_section(self, abs_path, cull_entries, game): #vers 1
        """Write a real "cull...end" section back to its own real IPL
        file on disk (Aug 21 2026,  "lets build add, del
        for zon, cull, occu and ipl changes") - same real replace-
        only-that-section/keep-everything-else/write-a-.bak-first
        approach as _write_back_zone_section (see its own docstring
        for the full, real safety reasoning, identical here).

        Real, honest limitation for SA specifically: CullEntry only
        ever stores the resolved (x1,y1,z1,x2,y2,z2) axis-aligned
        bounding box, not the original center/length/width/skew
        fields a real SA cull line actually has (see _parse_cull's
        own docstring for the full, real confirmed-against-my-
        real-file story) - a real skewed zone's own skew is genuinely
        lost on write-back, reconstructed here as an unskewed
        (Unknown1=Unknown2=0) box of the same real overall size
        instead. Full skew round-tripping would need CullEntry itself
        extended to keep the original fields - not attempted here,
        same real scope limit _parse_cull's own docstring already
        names for reading."""
        from apps.methods.gta_dat_parser import GTAGame
        try:
            with open(abs_path, 'r', encoding='latin-1', newline='') as f:
                lines = f.readlines()
        except Exception as e:
            return False, f"Couldn't read {abs_path}: {e}"

        section_start = section_end = None
        for i, raw in enumerate(lines):
            stripped = raw.split('#')[0].strip()
            if stripped.lower() == 'cull':
                section_start = i
            elif section_start is not None and section_end is None and stripped.lower() == 'end':
                section_end = i
                break

        new_lines = []
        for c in cull_entries:
            #  fix (Aug 21 2026,  "when loading SOL, are
            # you using the SA parser or VC parser?") - SOL is its own,
            # distinct GTAGame value, so this never matched it despite
            # SOL running on the SA engine with SA-format IPL sections
            # (see _parse_cull's own matching real fix, same real
            # cause). A SOL cull box saved back to disk would have
            # been silently written in the wrong, III/VC two-corner-
            # point format instead.
            if game in (GTAGame.SA, GTAGame.SOL):
                length = (c.y2 - c.y1) / 2.0
                width = (c.x2 - c.x1) / 2.0
                fields = [f"{c.center_x:.6f}", f"{c.center_y:.6f}", f"{c.center_z:.6f}",
                          "0.000000", f"{length:.6f}", f"{c.z1:.6f}",
                          f"{width:.6f}", "0.000000", f"{c.z2:.6f}", str(c.flags)]
            else:
                fields = [f"{c.center_x:.6f}", f"{c.center_y:.6f}", f"{c.center_z:.6f}",
                          f"{c.x1:.6f}", f"{c.y1:.6f}", f"{c.z1:.6f}",
                          f"{c.x2:.6f}", f"{c.y2:.6f}", f"{c.z2:.6f}",
                          str(c.flags), str(c.wanted_level_drop)]
            new_lines.append(', '.join(fields) + '\n')

        if section_start is not None and section_end is not None:
            lines = lines[:section_start + 1] + new_lines + lines[section_end:]
        else:
            if lines and not lines[-1].endswith('\n'):
                lines[-1] += '\n'
            lines += ['cull\n'] + new_lines + ['end\n']

        try:
            _write_ipl_lines(abs_path, lines)      # timestamped backup + atomic write, CRLF kept
        except Exception as e:
            return False, f"Couldn't write {abs_path}: {e}"
        return True, f"Wrote {len(cull_entries)} cull box(es) to {os.path.basename(abs_path)}"

    def _save_culls(self): #vers 1
        """Save Cull Zones button - writes every currently loaded cull
        box back to its own real source IPL file(s) on disk (Aug 21
        2026,  "lets build add, del for zon, cull, occu and
        ipl changes"). Same real grouping/resolve/confirm/write
        pattern as _save_zones (see its own docstring)."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            self._set_status("No world loaded")
            return
        culls = getattr(loader, 'culls', [])
        if not culls:
            self._set_status("No cull boxes to save")
            return
        by_file = {}
        for c in culls:
            by_file.setdefault(c.source_ipl, []).append(c)

        confirm = QMessageBox.question(
            self, "Save Cull Zones",
            f"Write {len(culls)} cull box(es) across {len(by_file)} file(s) back to "
            f"disk?\n\nA .bak backup of each file's own original content is kept "
            f"(only if one doesn't already exist).\n\n"
            f"Note: any real SA skew on a box is lost on write-back (reconstructed "
            f"as an unskewed box of the same overall size) - see the Save Cull "
            f"Zones tooltip.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if confirm != QMessageBox.StandardButton.Yes:
            return

        game = getattr(loader, 'game', None)
        written, failed = 0, []
        for source_ipl, entries in by_file.items():
            abs_path = self._resolve_ipl_abs_path(source_ipl)
            if abs_path is None:
                failed.append(f"{source_ipl} (file not found)")
                continue
            ok, msg = self._write_back_cull_section(abs_path, entries, game)
            if ok:
                written += 1
            else:
                failed.append(msg)

        if failed:
            self._set_status(f"Saved {written} file(s); {len(failed)} failed: {'; '.join(failed)}")
        else:
            self._set_status(f"Saved {len(culls)} cull box(es) across {written} file(s)")

    def _add_occl(self): #vers 1
        """Add a new occlusion box,  "lets build add, del
        for zon, cull, occu and ipl changes" / "both if you can" (Aug
        21 2026) - IN MEMORY ONLY until Save Occlusion is used. Placed
        as a small, unrotated (20x20x10) box centred on wherever the
        viewport is currently looking, in the first currently-loaded
        occlusion source file found (same real "no way to ask which
        file" reasoning as _add_zone/_add_cull above)."""
        loader = getattr(self, '_world_loader', None)
        vp = getattr(self, 'preview_widget', None)
        if loader is None or vp is None:
            return
        from apps.methods.gta_dat_parser import OcclEntry
        existing = getattr(loader, 'occls', [])
        source_ipl = existing[0].source_ipl if existing else 'occlu.ipl'
        cx, cy = -vp._pan_x, -vp._pan_y
        new_occl = OcclEntry(
            mid_x=cx, mid_y=cy, bottom_z=0.0,
            width_x=20.0, width_y=20.0, height=10.0, rotation=0.0,
            source_ipl=source_ipl, line_no=0)

        def _do_add():
            loader.occls.append(new_occl)
            self._refresh_occl_box_visualization()

        def _do_remove():
            if new_occl in loader.occls:
                loader.occls.remove(new_occl)
            self._refresh_occl_box_visualization()

        _do_add()
        self._push_map_undo(_do_remove, _do_add, "Add occlusion box")
        self._set_status("Added occlusion box - not yet saved to disk")

    def _delete_occl(self): #vers 1
        """Delete the currently cycled/selected occlusion box, per
         : "lets build add, del for zon, cull, occu and ipl
        changes" / "both if you can" (Aug 21 2026) - IN MEMORY ONLY
        until Save Occlusion is used. Acts on whichever occlusion box
        the Cycle Zones button (or a direct viewport double-click)
        last selected (preview_widget._selected_box)."""
        loader = getattr(self, '_world_loader', None)
        vp = getattr(self, 'preview_widget', None)
        sel = getattr(vp, '_selected_box', None) if vp else None
        if loader is None or vp is None or sel is None or sel[0] != 'occl':
            self._set_status("No occlusion box currently selected - use Cycle Zones first")
            return
        _, box_index = sel
        occls = getattr(loader, 'occls', [])
        if not (0 <= box_index < len(occls)):
            return
        removed_occl = occls[box_index]

        def _do_remove():
            if removed_occl in loader.occls:
                loader.occls.remove(removed_occl)
            vp._selected_box = None
            self._refresh_occl_box_visualization()

        def _do_restore():
            loader.occls.insert(min(box_index, len(loader.occls)), removed_occl)
            self._refresh_occl_box_visualization()

        _do_remove()
        self._push_map_undo(_do_restore, _do_remove, "Delete occlusion box")
        self._set_status("Deleted occlusion box - not yet saved to disk")

    def _write_back_occl_section(self, abs_path, occl_entries): #vers 1
        """Write a real "occl...end" section back to its own real IPL
        file on disk (Aug 21 2026,  "lets build add, del
        for zon, cull, occu and ipl changes" / "both if you can") -
        same real replace-only-that-section/keep-everything-else/
        write-a-.bak-first approach as _write_back_zone_section (see
        its own docstring for the full, real safety reasoning,
        identical here). Occlusion's own real 7-field format (MidX,
        MidY, BottomZ, WidthX, WidthY, Height, Rotation) is the same
        across every real game this parser handles, unlike cull - no
        game-specific branching needed here."""
        try:
            with open(abs_path, 'r', encoding='latin-1', newline='') as f:
                lines = f.readlines()
        except Exception as e:
            return False, f"Couldn't read {abs_path}: {e}"

        section_start = section_end = None
        for i, raw in enumerate(lines):
            stripped = raw.split('#')[0].strip()
            if stripped.lower() == 'occl':
                section_start = i
            elif section_start is not None and section_end is None and stripped.lower() == 'end':
                section_end = i
                break

        new_lines = []
        for o in occl_entries:
            fields = [f"{o.mid_x:.6f}", f"{o.mid_y:.6f}", f"{o.bottom_z:.6f}",
                      f"{o.width_x:.6f}", f"{o.width_y:.6f}", f"{o.height:.6f}",
                      f"{o.rotation:.6f}"]
            new_lines.append(', '.join(fields) + '\n')

        if section_start is not None and section_end is not None:
            lines = lines[:section_start + 1] + new_lines + lines[section_end:]
        else:
            if lines and not lines[-1].endswith('\n'):
                lines[-1] += '\n'
            lines += ['occl\n'] + new_lines + ['end\n']

        try:
            _write_ipl_lines(abs_path, lines)      # timestamped backup + atomic write, CRLF kept
        except Exception as e:
            return False, f"Couldn't write {abs_path}: {e}"
        return True, f"Wrote {len(occl_entries)} occlusion box(es) to {os.path.basename(abs_path)}"

    def _save_occls(self): #vers 1
        """Save Occlusion button - writes every currently loaded
        occlusion box back to its own real source IPL file(s) on disk
        (Aug 21 2026,  "lets build add, del for zon, cull,
        occu and ipl changes" / "both if you can"). Same real
        grouping/resolve/confirm/write pattern as _save_zones/_save_
        culls (see their own docstrings)."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            self._set_status("No world loaded")
            return
        occls = getattr(loader, 'occls', [])
        if not occls:
            self._set_status("No occlusion boxes to save")
            return
        by_file = {}
        for o in occls:
            by_file.setdefault(o.source_ipl, []).append(o)

        confirm = QMessageBox.question(
            self, "Save Occlusion",
            f"Write {len(occls)} occlusion box(es) across {len(by_file)} file(s) "
            f"back to disk?\n\nA .bak backup of each file's own original content "
            f"is kept (only if one doesn't already exist).",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if confirm != QMessageBox.StandardButton.Yes:
            return

        written, failed = 0, []
        for source_ipl, entries in by_file.items():
            abs_path = self._resolve_ipl_abs_path(source_ipl)
            if abs_path is None:
                failed.append(f"{source_ipl} (file not found)")
                continue
            ok, msg = self._write_back_occl_section(abs_path, entries)
            if ok:
                written += 1
            else:
                failed.append(msg)

        if failed:
            self._set_status(f"Saved {written} file(s); {len(failed)} failed: {'; '.join(failed)}")
        else:
            self._set_status(f"Saved {len(occls)} occlusion box(es) across {written} file(s)")

    def _add_grge(self): #vers 2
        """Add a new garage,  "add support for GRGE" / "lets
        build add, del for zon, cull, occu and ipl changes" / "both if
        you can" (Aug 21 2026) - IN MEMORY ONLY until Save Garages is
        used. Placed as a small (14x23x6, matching my own real
        example garages' own typical size) box centred on wherever
        the viewport is currently looking, in the first currently-
        loaded garage source file found (same real "no way to ask
        which file" reasoning as _add_zone/_add_cull/_add_occl).

         fix (Aug 21 2026  own real, uploaded GRGE
        example lines) - front_x/x2/front_y now follow the same real
        field convention every one of my own real garage lines
        actually uses (see _refresh_grge_box_visualization's own
        docstring for the full, confirmed real field meaning):
        front_x holds the box's own real second X corner, front_y
        repeats y1, and x2 repeats x1 - not the other way around, as
        this method got it backwards before."""
        loader = getattr(self, '_world_loader', None)
        vp = getattr(self, 'preview_widget', None)
        if loader is None or vp is None:
            return
        from apps.methods.gta_dat_parser import GrgeEntry
        existing = getattr(loader, 'grges', [])
        source_ipl = existing[0].source_ipl if existing else 'garages.ipl'
        cx, cy = -vp._pan_x, -vp._pan_y
        new_grge = GrgeEntry(
            x1=cx - 7.0, y1=cy - 11.5, z1=0.0,
            front_x=cx + 7.0, front_y=cy - 11.5,
            x2=cx - 7.0, y2=cy + 11.5, z2=6.0,
            door_type=0, garage_type=0, name=f'GRGE{len(existing)}',
            source_ipl=source_ipl, line_no=0)

        def _do_add():
            loader.grges.append(new_grge)
            self._refresh_grge_box_visualization()

        def _do_remove():
            if new_grge in loader.grges:
                loader.grges.remove(new_grge)
            self._refresh_grge_box_visualization()

        _do_add()
        self._push_map_undo(_do_remove, _do_add, f"Add garage '{new_grge.name}'")
        self._set_status(f"Added garage '{new_grge.name}' - not yet saved to disk")

    def _delete_grge(self): #vers 1
        """Delete the currently cycled/selected garage, .per:
        "add support for GRGE" / "lets build add, del for zon, cull,
        occu and ipl changes" / "both if you can" (Aug 21 2026) - IN
        MEMORY ONLY until Save Garages is used. Acts on whichever
        garage the Cycle Zones button (or a direct viewport double-
        click) last selected (preview_widget._selected_box)."""
        loader = getattr(self, '_world_loader', None)
        vp = getattr(self, 'preview_widget', None)
        sel = getattr(vp, '_selected_box', None) if vp else None
        if loader is None or vp is None or sel is None or sel[0] != 'grge':
            self._set_status("No garage currently selected - use Cycle Zones first")
            return
        _, box_index = sel
        grges = getattr(loader, 'grges', [])
        if not (0 <= box_index < len(grges)):
            return
        removed_grge = grges[box_index]
        removed_name = removed_grge.name

        def _do_remove():
            if removed_grge in loader.grges:
                loader.grges.remove(removed_grge)
            vp._selected_box = None
            self._refresh_grge_box_visualization()

        def _do_restore():
            loader.grges.insert(min(box_index, len(loader.grges)), removed_grge)
            self._refresh_grge_box_visualization()

        _do_remove()
        self._push_map_undo(_do_restore, _do_remove, f"Delete garage '{removed_name}'")
        self._set_status(f"Deleted garage '{removed_name}' - not yet saved to disk")

    def _write_back_grge_section(self, abs_path, grge_entries): #vers 1
        """Write a real "grge...end" section back to its own real IPL
        file on disk (Aug 21 2026,  "add support for GRGE" /
        "lets build add, del for zon, cull, occu and ipl changes" /
        "both if you can") - same real replace-only-that-section/
        keep-everything-else/write-a-.bak-first approach as _write_
        back_zone_section (see its own docstring for the full, real
        safety reasoning, identical here). Garage names aren't quoted
        on write-back - my own real example data (see GrgeEntry's
        own docstring) shows plain, unquoted names ("cjsafe"), not
        SannyBuilder-style quoted strings."""
        try:
            with open(abs_path, 'r', encoding='latin-1', newline='') as f:
                lines = f.readlines()
        except Exception as e:
            return False, f"Couldn't read {abs_path}: {e}"

        section_start = section_end = None
        for i, raw in enumerate(lines):
            stripped = raw.split('#')[0].strip()
            if stripped.lower() == 'grge':
                section_start = i
            elif section_start is not None and section_end is None and stripped.lower() == 'end':
                section_end = i
                break

        new_lines = []
        for g in grge_entries:
            fields = [f"{g.x1:.6f}", f"{g.y1:.6f}", f"{g.z1:.6f}",
                      f"{g.front_x:.6f}", f"{g.front_y:.6f}",
                      f"{g.x2:.6f}", f"{g.y2:.6f}", f"{g.z2:.6f}",
                      str(g.door_type), str(g.garage_type), g.name]
            new_lines.append(', '.join(fields) + '\n')

        if section_start is not None and section_end is not None:
            lines = lines[:section_start + 1] + new_lines + lines[section_end:]
        else:
            if lines and not lines[-1].endswith('\n'):
                lines[-1] += '\n'
            lines += ['grge\n'] + new_lines + ['end\n']

        try:
            _write_ipl_lines(abs_path, lines)      # timestamped backup + atomic write, CRLF kept
        except Exception as e:
            return False, f"Couldn't write {abs_path}: {e}"
        return True, f"Wrote {len(grge_entries)} garage(s) to {os.path.basename(abs_path)}"

    def _convert_ipl_inst_section(self, abs_path, to_game): #vers 2
        """Convert an IPL's INST lines to to_game's layout in place (backup kept)."""
        from apps.components.Map_Editor.depends.ipl_format_check import rewrite_ipl
        action = {'vc': 'to_vc', 'gta3': 'to_iii'}.get(to_game, 'to_sa')
        try:
            n = rewrite_ipl(abs_path, action)
        except Exception as e:
            return False, f"Couldn't convert {abs_path}: {e}"
        if n == 0:
            return False, f"No INST lines needed converting in {os.path.basename(abs_path)}"
        return True, f"Converted {n} INST line(s) in {os.path.basename(abs_path)}"

    def _convert_ipl_files_dialog(self, to_game): #vers 1
        """Convert IPL File(s) From Disk... - lets   pick any real
        .ipl file(s) anywhere on the hard drive and convert their own
        real INST sections to to_game's own real layout (Aug 21 2026,
         "selected ipl files to convert, anywhere on the
        harddrive"). Confirms first, since this genuinely changes real
        files on disk (with a real .bak backup, but still a real,
        deliberate action)."""
        paths, _ = QFileDialog.getOpenFileNames(
            self, f"Select IPL file(s) to convert to {to_game.upper()}",
            "", "IPL Files (*.ipl);;All Files (*)")
        if not paths:
            return
        confirm = QMessageBox.question(
            self, "Convert IPL Files",
            f"Convert INST sections in {len(paths)} file(s) to {to_game.upper()} "
            f"format?\n\nA .bak backup of each file's own original content is "
            f"kept (only if one doesn't already exist).",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if confirm != QMessageBox.StandardButton.Yes:
            return
        self._run_ipl_inst_conversion(paths, to_game)

    def _convert_loaded_ipls_dialog(self, to_game): #vers 1
        """Convert Loaded IPL(s)... - lets   pick from whichever
        real IPL files the currently-loaded world actually came from
        (Aug 21 2026,  "...or loaded ipl browser list"),
        rather than only browsing the hard drive blind. Lists every
        real path from loader.load_log (every real loaded IPL is
        already recorded there)."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            self._set_status("No world loaded")
            return
        all_paths = sorted({entry[2] for entry in getattr(loader, 'load_log', [])
                             if len(entry) >= 3 and entry[1] == 'IPL'})
        if not all_paths:
            self._set_status("No loaded IPL files found")
            return

        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QListWidget, QDialogButtonBox
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Convert Loaded IPL(s) to {to_game.upper()}")
        dlg.resize(500, 400)
        layout = QVBoxLayout(dlg)
        layout.addWidget(QLabel("Select IPL file(s) to convert:"))
        list_widget = QListWidget()
        list_widget.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        for p in all_paths:
            list_widget.addItem(p)
        layout.addWidget(list_widget)
        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        buttons.accepted.connect(dlg.accept)
        buttons.rejected.connect(dlg.reject)
        layout.addWidget(buttons)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        selected = [item.text() for item in list_widget.selectedItems()]
        if not selected:
            return
        confirm = QMessageBox.question(
            self, "Convert IPL Files",
            f"Convert INST sections in {len(selected)} file(s) to {to_game.upper()} "
            f"format?\n\nA .bak backup of each file's own original content is "
            f"kept (only if one doesn't already exist).",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if confirm != QMessageBox.StandardButton.Yes:
            return
        self._run_ipl_inst_conversion(selected, to_game)

    def _run_ipl_inst_conversion(self, paths, to_game): #vers 1
        """Shared real conversion loop for both _convert_ipl_files_
        dialog and _convert_loaded_ipls_dialog (Aug 21 2026) - calls
        _convert_ipl_inst_section once per real path, reports a
        combined real summary."""
        converted, failed = 0, []
        for path in paths:
            ok, msg = self._convert_ipl_inst_section(path, to_game)
            if ok:
                converted += 1
            else:
                failed.append(msg)
        if failed:
            self._set_status(f"Converted {converted} file(s); {len(failed)} not converted: {'; '.join(failed)}")
        else:
            self._set_status(f"Converted {converted} file(s) to {to_game.upper()}")

    def _show_ipl_format_checker(self): #vers 1
        """Scan world IPLs for layout / scale / bad lines; fix or convert them."""
        from apps.components.Map_Editor.depends.ipl_format_check import IPLFormatDialog
        loader = getattr(self, '_world_loader', None)
        paths = []
        if loader is not None:
            paths = [e.abs_path for e in getattr(loader, 'available_ipls', {}).values()
                     if getattr(e, 'exists', False)]
            paths += [e[2] for e in getattr(loader, 'load_log', []) if len(e) >= 3 and e[1] == 'IPL']
        IPLFormatDialog(self, sorted(set(paths))).exec()

    def _show_convert_ipl_menu(self, button): #vers 2
        """Convert button, left or right-click (Aug 21 2026, per
         : "having VC -> SA or SA -> VC is something I need,
        under a convertion SVG icon, right clicked for options, and
        selected ipl files to convert, anywhere on the harddrive or
        loaded ipl browser list") - both real source paths (disk
        browse, loaded-IPL list) crossed with both real target
        formats."""
        from apps.methods.gta_dat_parser import GTAGame
        menu = QMenu(self)
        menu.addAction("IPL Format Checker...", self._show_ipl_format_checker)
        menu.addSeparator()
        menu.addAction("Convert File(s) From Disk to SA/SOL...",
                        lambda: self._convert_ipl_files_dialog(GTAGame.SA))
        menu.addAction("Convert File(s) From Disk to VC...",
                        lambda: self._convert_ipl_files_dialog(GTAGame.VC))
        menu.addSeparator()
        menu.addAction("Convert Loaded IPL(s) to SA/SOL...",
                        lambda: self._convert_loaded_ipls_dialog(GTAGame.SA))
        menu.addAction("Convert Loaded IPL(s) to VC...",
                        lambda: self._convert_loaded_ipls_dialog(GTAGame.VC))
        menu.exec(button.mapToGlobal(button.rect().bottomLeft()))

    def _optimize_dat_load_order_clicked(self): #vers 1
        """Optimize Order button (Aug 21 2026,  "if the
        model names, col names, and ide/ipl entries loaded in the
        same order the game spends less work matching them up, result
        is the game loads and renders faster") - reorders the
        currently-loaded world's own real default.dat and main .dat
        files' own real IDE/IPL/COLFILE/IMG directive lines (see
        optimize_dat_load_order's own docstring for the full, real
        algorithm, confirmed against gta_vc.dat's own real header
        comments). Confirms first, since this genuinely rewrites real
        files on disk (with a real .bak backup each, but still a
        real, deliberate action)."""
        from apps.methods.gta_dat_parser import optimize_dat_load_order
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            self._set_status("No world loaded")
            return

        targets = []
        for dat in (getattr(loader, 'default_dat', None), getattr(loader, 'main_dat', None)):
            if dat is not None and getattr(dat, 'dat_path', '') and dat.entries:
                targets.append(dat)
        if not targets:
            self._set_status("No loaded .dat file to reorder")
            return

        confirm = QMessageBox.question(
            self, "Optimize Load Order",
            f"Reorder IDE/IPL/COLFILE/IMG lines in {len(targets)} .dat file(s) "
            f"to Rockstar's own documented convention (grouped by directory, "
            f"alphabetical within each)?\n\nA .bak backup of each file's own "
            f"original content is kept (only if one doesn't already exist).\n\n"
            f"Note: this reorders directive lines in the .dat file itself, "
            f"not model/instance order within IDE/IPL files.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if confirm != QMessageBox.StandardButton.Yes:
            return

        done, failed = 0, []
        for dat in targets:
            ok, msg = optimize_dat_load_order(dat.dat_path, dat.entries)
            if ok:
                done += 1
            else:
                failed.append(msg)
        if failed:
            self._set_status(f"Reordered {done} file(s); {len(failed)} failed: {'; '.join(failed)}")
        else:
            self._set_status(f"Reordered {done} .dat file(s) - reload the world to see the new order")

    def _show_model_search_dialog(self): #vers 1
        """Search loaded instances by model name (Sep 5 2026, per
         : "a search [O'] function on the ribbon bar to find a
        model name so I can see the IPL line") - live-filtered as you
        type, one row per unique model name with its own placement
        count. Double-click jumps to it the exact same way clicking a
        model in the Object Browser already does (_current_model_
        instances/_current_instance_index set the same way, so Prev/
        Next cycling through that model's own other placements
        continues to work correctly afterward, same real fix as
        _find_instance_for_ipl_inst_file_row's own position-based
        disambiguation just made for the IPL File Display table)."""
        all_instances = getattr(self, '_all_instances', [])
        if not all_instances:
            self._set_status("No world loaded")
            return

        by_name = {}
        for inst in all_instances:
            by_name.setdefault(inst.model_name, []).append(inst)

        dlg = QDialog(self)
        dlg.setWindowTitle("Search Loaded Instances")
        dlg.resize(420, 480)
        lay = QVBoxLayout(dlg)
        search_edit = QLineEdit()
        search_edit.setPlaceholderText("Type a model name...")
        lay.addWidget(search_edit)
        results = QListWidget()
        lay.addWidget(results, 1)
        info_lbl = QLabel("")
        lay.addWidget(info_lbl)

        def _refresh(text):
            results.clear()
            text_lower = text.strip().lower()
            names = sorted(by_name.keys())
            if text_lower:
                names = [n for n in names if text_lower in n.lower()]
            for name in names[:500]:   # cap - a handful of chars usually narrows this down fast
                count = len(by_name[name])
                item = QListWidgetItem(f"{name}  ({count} placement{'s' if count != 1 else ''})")
                item.setData(Qt.ItemDataRole.UserRole, name)
                results.addItem(item)
            info_lbl.setText(f"{len(names)} model name(s) match" if text_lower else
                              f"{len(names)} unique model name(s) loaded - type to filter")

        def _jump_to(item):
            name = item.data(Qt.ItemDataRole.UserRole)
            instances = by_name.get(name, [])
            if not instances:
                return
            self._current_model_instances = instances
            self._current_instance_index = 0
            self._center_on_instance(instances[0], nav_info=(0, len(instances)))
            dlg.accept()

        search_edit.textChanged.connect(_refresh)
        results.itemDoubleClicked.connect(_jump_to)
        _refresh("")
        search_edit.setFocus()
        dlg.exec()

    def _save_grges(self): #vers 1
        """Save Garages button - writes every currently loaded garage
        back to its own real source IPL file(s) on disk (Aug 21 2026,
         "add support for GRGE" / "lets build add, del for
        zon, cull, occu and ipl changes" / "both if you can"). Same
        real grouping/resolve/confirm/write pattern as _save_zones/
        _save_culls/_save_occls (see their own docstrings)."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            self._set_status("No world loaded")
            return
        grges = getattr(loader, 'grges', [])
        if not grges:
            self._set_status("No garages to save")
            return
        by_file = {}
        for g in grges:
            by_file.setdefault(g.source_ipl, []).append(g)

        confirm = QMessageBox.question(
            self, "Save Garages",
            f"Write {len(grges)} garage(s) across {len(by_file)} file(s) back to "
            f"disk?\n\nA .bak backup of each file's own original content is kept "
            f"(only if one doesn't already exist).",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if confirm != QMessageBox.StandardButton.Yes:
            return

        written, failed = 0, []
        for source_ipl, entries in by_file.items():
            abs_path = self._resolve_ipl_abs_path(source_ipl)
            if abs_path is None:
                failed.append(f"{source_ipl} (file not found)")
                continue
            ok, msg = self._write_back_grge_section(abs_path, entries)
            if ok:
                written += 1
            else:
                failed.append(msg)

        if failed:
            self._set_status(f"Saved {written} file(s); {len(failed)} failed: {'; '.join(failed)}")
        else:
            self._set_status(f"Saved {len(grges)} garage(s) across {written} file(s)")

    def _sync_ipl_inst_file_selection(self, inst): #vers 1
        """Highlight one instance's row in the IPL Inst File table,
        switching IPL Sections/the shown IPL first if the instance
        belongs to a different one than what's currently displayed -
        Aug 1 2026)"""
        sections_table = getattr(self, '_ipl_sections_table', None)
        table = getattr(self, '_ipl_inst_file_table', None)
        if sections_table is None or table is None:
            return
        if getattr(self, '_ipl_inst_file_mode', 'ipl') != 'ipl':
            return   # currently showing IDE Objects, not IPL data - nothing to sync to

        source_ipl = getattr(inst, 'source_ipl', '')
        current_item = sections_table.item(sections_table.currentRow(), 0)
        current_name = current_item.data(Qt.ItemDataRole.UserRole) if current_item else None
        if source_ipl and source_ipl != current_name:
            for r in range(sections_table.rowCount()):
                item = sections_table.item(r, 0)
                if item is not None and item.data(Qt.ItemDataRole.UserRole) == source_ipl:
                    sections_table.setCurrentCell(r, 0)
                    self._refresh_ipl_inst_file_panel()
                    break

        for r in range(table.rowCount()):
            id_item = table.item(r, 0)
            model_item = table.item(r, 1)
            if (id_item is not None and model_item is not None
                    and id_item.text() == str(inst.model_id)
                    and model_item.text() == inst.model_name):
                table.selectRow(r)
                table.scrollToItem(id_item)
                break

    def _extract_ipl_section_text(self, raw_text, section_name): #vers 2
        """Extract just one named section's lines (between the section
        keyword and its "end") from a raw IPL file's text, tracking
        each line's real 1-based file line number (Aug 1 2026)"""
        lines = raw_text.splitlines()
        out = []
        in_section = False
        found = False
        for i, raw in enumerate(lines, start=1):
            line = raw.split("#")[0].strip()
            low = line.lower()
            if not in_section and low == section_name:
                in_section = True
                found = True
                out.append((i, raw))
                continue
            if in_section:
                out.append((i, raw))
                if low == "end":
                    in_section = False
        return out if found else None

    def _apply_compact_table_style(self, table): #vers 1
        """Apply the same compact row/header height Object Browser
        uses (font-metric-based: text height + 2px, not a hardcoded
        guess) to a table in one of the merged tabs."""
        from PyQt6.QtGui import QFontMetrics
        row_h = QFontMetrics(table.font()).height() + 2
        table.verticalHeader().setVisible(False)
        table.verticalHeader().setMinimumSectionSize(row_h)
        table.verticalHeader().setDefaultSectionSize(row_h)
        header = table.horizontalHeader()
        header.setMinimumSectionSize(16)
        header.setFixedHeight(QFontMetrics(header.font()).height() + 2)

    def _create_ide_tab(self): #vers 2
        """[IDE] tab content, matching MooMapper's own "Object
        Definition" tab."""
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(1, 1, 1, 1)
        from apps.methods.imgfactory_svg_icons import get_save_icon, get_edit_icon
        title_row_widget = QWidget()
        title_row_widget.setObjectName("IDE_title_row")
        title_row = QHBoxLayout(title_row_widget)
        title_row.setContentsMargins(0, 0, 0, 0)
        sm_buttonheight = 20

        icon_color = self._get_icon_color()

        label = QLabel("IDE Definitions")
        label.setStyleSheet("font-weight: bold;")
        title_row.addWidget(label)
        #edit_btn.setStyleSheet("font-weight: bold;")
        _compact_18 = "padding: 0px 4px; margin: 0px; border-width: 1px;"
        edit_btn = QPushButton(get_edit_icon(sm_buttonheight, icon_color), "Edit")
        edit_btn.setToolTip("STUB - no IDE editing built yet")
        edit_btn.setIconSize(QSize(18, 18))
        edit_btn.setFixedHeight(18)
        edit_btn.setStyleSheet(_compact_18)
        edit_btn.setEnabled(False)
        #save_btn.setStyleSheet("font-weight: bold;")
        save_btn = QPushButton(get_save_icon(sm_buttonheight, icon_color), "Save")
        save_btn.setToolTip("STUB - no write-back to disk exists for any\n"
                            "file type in Map Workshop yet")
        save_btn.setIconSize(QSize(18, 18))
        save_btn.setFixedHeight(18)
        save_btn.setStyleSheet(_compact_18)
        save_btn.setEnabled(False)
        title_row.addWidget(edit_btn)
        title_row.addWidget(save_btn)
        title_row.addStretch()
        lay.addWidget(title_row_widget)
        self._register_collapsible_button_row(
            title_row_widget, [(edit_btn, "Edit"), (save_btn, "Save")])
        if not hasattr(self, '_object_browser_tab_rows'):
            self._object_browser_tab_rows = {}
        self._object_browser_tab_rows['ide'] = title_row_widget

        content_container = QWidget()
        content_lay = QVBoxLayout(content_container)
        content_lay.setContentsMargins(0, 0, 0, 0)

        table = QTableWidget(0, 2)
        table.setHorizontalHeaderLabels(["Ind.", "Filename"])
        table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        table.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.Stretch)
        table.cellClicked.connect(self._on_ide_tab_row_clicked)
        self._apply_compact_table_style(table)
        content_lay.addWidget(table)
        self._ide_tab_table = table
        from PyQt6.QtWidgets import QTextEdit
        preview = QTextEdit()
        preview.setReadOnly(True)
        font = preview.font(); font.setFamily("monospace"); preview.setFont(font)
        preview.setPlaceholderText("Click an IDE file above to preview its contents here")
        content_lay.addWidget(preview)
        self._ide_tab_preview = preview

        lay.addWidget(content_container)
        self._make_section_collapsible(label, content_container)
        return panel

    def _refresh_ide_tab(self, loader): #vers 2
        """Populate the IDE file list - paths shortened relative to the
        game root folder."""
        table = getattr(self, '_ide_tab_table', None)
        if table is None:
            return
        game_root = getattr(self, '_game_root', None)
        ide_paths = [abs_path for phase, entry_type, abs_path, ok in loader.load_log
                    if entry_type == "IDE" and ok]
        table.setRowCount(len(ide_paths))
        self._ide_tab_paths = {}
        for i, path in enumerate(ide_paths):
            display_path = path
            if game_root:
                try:
                    display_path = os.path.relpath(path, game_root)
                except Exception:
                    display_path = path
            table.setItem(i, 0, QTableWidgetItem(str(i)))
            table.setItem(i, 1, QTableWidgetItem(display_path))
            self._ide_tab_paths[i] = path

    def _on_ide_tab_row_clicked(self, row, col): #vers 2
        """Preview the clicked IDE file's real raw text content."""
        text_widget = getattr(self, '_ide_tab_preview', None)
        path = getattr(self, '_ide_tab_paths', {}).get(row)
        if text_widget is None or path is None:
            return
        try:
            with open(path, 'r', encoding='ascii', errors='ignore') as f:
                text_widget.setPlainText(f.read())
        except Exception as e:
            text_widget.setPlainText(f"(could not read {path}: {e})")
        if getattr(self, '_ipl_inst_file_mode', 'ipl') == 'ide':
            self._refresh_ide_objects_panel(row)

    def _refresh_ide_objects_panel(self, row): #vers 1
        """Parse the IDE file at self._ide_tab_paths[row] into the
        shared cells table (self._ipl_inst_file_table, currently
        titled "IDE Objects") - one row per entry line, same approach
        as _refresh_ipl_inst_file_panel: re-read raw text from disk,
        split each real data line on commas. Aug 1 2026)"""
        import re
        table = getattr(self, '_ipl_inst_file_table', None)
        path = getattr(self, '_ide_tab_paths', {}).get(row)
        if table is None:
            return
        if path is None:
            table.setRowCount(0)
            return
        try:
            with open(path, 'r', encoding='ascii', errors='ignore') as f:
                raw_text = f.read()
        except Exception:
            table.setRowCount(0)
            return

        data_rows = []
        for line_no, raw_line in enumerate(raw_text.splitlines(), start=1):
            line = raw_line.split("#")[0].strip()
            if not line or line.startswith("//"):
                continue
            low = line.lower()
            if low == 'end':
                continue
            if ',' not in line and re.match(r'^[a-z0-9_]{2,8}$', low):
                continue   # section header (objs/tobj/cars/etc.)
            fields = [f.strip() for f in line.split(',')]
            data_rows.append((line_no, fields))

        max_cols = max((len(r) for _ln, r in data_rows), default=0)
        table.setColumnCount(max_cols)
        table.setHorizontalHeaderLabels([f"Field {i+1}" for i in range(max_cols)])
        table.setRowCount(len(data_rows))
        for r, (line_no, fields) in enumerate(data_rows):
            for c in range(max_cols):
                item = QTableWidgetItem(fields[c] if c < len(fields) else "")
                if c == 0:
                    item.setData(Qt.ItemDataRole.UserRole, line_no)
                table.setItem(r, c, item)
