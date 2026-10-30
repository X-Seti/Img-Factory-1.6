#this belongs in apps/components/Map_Editor/depends/map_overlay_func.py - Version: 3
# X-Seti - October 14 2026 - IMG Factory 1.6 - Map Workshop overlays

"""
IPL controls dock, overlays, radar tiles, prelight, render filters for ModelWorkshop (Map).
"""

##class MapOverlayMixin: -
# _apply_interior_filter
# _apply_ipl_controls_display_style
# _apply_ipl_visibility_filter
# _apply_ipl_visibility_filter_impl
# _apply_lod_filter
# _apply_tobj_time_filter
# _bake_prelight_selected
# _build_radar_tile_txd_bytes
# _clear_world_prelight
# _create_ipl_controls_dock
# _create_ipl_inst_file_panel
# _create_texture_thumbnail
# _export_radar_tiles_as_img
# _generate_radar_tiles
# _generate_world_prelight
# _get_generic_textures
# _get_txd_textures
# _is_lod_by_draw_distance
# _is_lod_named
# _move_overlay_buttons_to_ribbon
# _on_2dfx_master_toggled
# _on_box_resized
# _on_col_render_option_toggled
# _on_crt_time_clicked
# _on_edit_boxes_toggled
# _on_edit_paths_toggled
# _on_generate_radar_tiles_clicked
# _on_ignore_scaling_toggled
# _on_interior_show_all_toggled
# _on_ipl_controls_settings_clicked
# _on_load_generic_txd_clicked
# _on_lod_test_mouse_moved
# _on_lod_test_toggled
# _on_path_node_dragged
# _on_radar_tiles_context_menu
# _on_show_auzo_toggled
# _on_show_cull_boxes_toggled
# _on_show_grge_boxes_toggled
# _on_show_occl_boxes_toggled
# _on_show_paths_toggled
# _on_show_radar_tex_layer_toggled
# _on_show_sa_nodes_toggled
# _on_show_tracks_toggled
# _on_show_water_toggled
# _on_show_zone_boxes_toggled
# _on_time_flow_tick
# _on_tobj_time_changed
# _on_tobj_time_toggled
# _on_toggle_force_prelight
# _on_toggle_mesh_col
# _on_water_layer_toggled
# _prelight_menu
# _preload_generic_ide_textures
# _refresh_2dfx_lights
# _refresh_auzo_visualization
# _refresh_cull_box_visualization
# _refresh_grge_box_visualization
# _refresh_occl_box_visualization
# _refresh_path_visualization
# _refresh_sa_node_visualization
# _refresh_track_visualization
# _refresh_water_visualization
# _refresh_zone_box_visualization
# _register_collapsible_button_row
# _resolve_ipl_abs_path
# _save_world_prelight
# _save_zones
# _send_radar_tiles_to_radar_workshop
# _send_radar_tiles_to_txd_workshop
# _set_interior_filter
# _set_lod_display_mode
# _set_world_render_mode
# _set_zone_render_style
# _show_interior_picker_menu
# _show_time_flow_settings_popup
# _start_time_flow
# _stop_time_flow
# _update_button_row_collapse
# _update_mode_button_style
# _write_back_zone_section
# _write_img_entry

import os
import shutil

from PyQt6.QtCore import QTimer, Qt
from PyQt6.QtGui import QImage, QPixmap
from PyQt6.QtWidgets import (
    QAbstractItemView, QApplication, QCheckBox, QDialog, QDockWidget, QDoubleSpinBox,
    QFileDialog, QHBoxLayout, QLabel, QMenu, QMessageBox, QProgressDialog, QPushButton,
    QSpinBox, QTableWidget, QTableWidgetItem, QTimeEdit, QToolButton, QVBoxLayout, QWidget)

from apps.components.Map_Editor.depends.map_widgets_func import (
    _FilteredLoaderStub, _MapOverlayToggleButton, _write_ipl_lines)


class MapOverlayMixin: #vers 1
    """IPL controls dock, overlays, radar tiles, prelight, render filters."""

    def _on_show_radar_tex_layer_toggled(self, checked): #vers 3
        """Show/hide the radar tex layer - real fix (Aug 20 2026, per
         : "The radar button also needs to switch the radar on
        and off, and right-clicking the radar button should generate
        the radar... moving the Radar settings from the settings, to
        the button would follow the pattern of the other buttons") -
        same real logic the settings dialog's own Apply handler
        already used, moved here so the button itself is the one,
        real place this now lives, matching every other overlay
        toggle. The settings dialog keeps the underlying setting
        (still read/written here) but no longer needs its own
        checkbox for it."""
        self.map_settings.set('show_radar_tex_layer', checked)
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_radar_tex_layer'):
            return
        if checked:
            loader = getattr(self, '_world_loader', None)
            game_key = getattr(loader, 'game', 'sa') if loader is not None else 'sa'
            tile_textures = self._load_radar_tex_tiles(game_key)
            vp.set_radar_tex_layer(True, tile_textures, game_key)
            from apps.methods.gta_dat_parser import RADAR_GRID_PRESETS
            grid = RADAR_GRID_PRESETS.get(game_key, RADAR_GRID_PRESETS['sa'])['grid_size']
            if vp._dist < grid / 12:                # inside one tile: one flat colour, zoom out
                vp.set_camera_state(dist=grid * 0.6)
                self._set_status("Radar on - zoomed out to show the map")
        else:
            vp.set_radar_tex_layer(False)
        if getattr(self, '_vp_gauges', None) is not None:
            self._vp_gauges.refresh()

    def _apply_ipl_controls_display_style(self): #vers 2
        """Switch every real overlay toggle button between its own
        text label and its own icon (Aug 20 2026,  "we could
        add a toggle in settings, Show IPL Controls = as [Buttons] or
        ribbon icons") - each button falls back to text-only on its
        own if it was never actually given a real icon yet (see
        _MapOverlayToggleButton.set_display_style's own docstring), so
        this is safe to call even before every button has real icon
        artwork. Still applies now that these buttons live on the
        Overlays ribbon (_move_overlay_buttons_to_ribbon) rather than
        IPL Controls itself - same real buttons, same real toggle,
        just a different real parent widget."""
        icon_only = bool(self.map_settings.get('ipl_controls_icon_only'))
        for btn in getattr(self, '_overlay_toggle_buttons', []):
            if hasattr(btn, 'set_display_style'):
                btn.set_display_style(icon_only)

    def _move_overlay_buttons_to_ribbon(self): #vers 2
        """Move every real overlay toggle button from IPL Controls
        onto the new "Overlays" ribbon (Aug 20 2026,  "The
        New Icons on the IPL Control pane, can be moved to the
        ribbon" - the arrow in his own screenshot pointed from these
        exact buttons up to the top toolbar area).

        Deferred (scheduled via QTimer.singleShot(0, ...) rather than
        called directly) because _create_ipl_controls_dock (where
        these buttons are actually constructed) runs before _build_
        toolbars (where the real "Overlays" ribbon they're moving
        into is actually created) in this app's own real startup
        sequence - by the time this deferred call actually runs, on
        the next event-loop tick, both are guaranteed to already
        exist, regardless of that ordering. QToolBar.addWidget on an
        already-constructed widget re-parents it automatically - no
        need to change where these buttons are originally built.

         fix (Aug 20 2026,  "in ribbon manager the new
        icons show up as Action, needs to show like the other icons,
        then name") - QToolBar.addWidget internally wraps the widget
        in a real QWidgetAction, but never copies the widget's own
        real text/icon onto it - RibbonManagerDialog's own real list-
        population line (act.text() or act.toolTip() or "Action")
        found both empty and fell all the way through to that literal
        fallback string, exactly matching what   saw. Sets both
        explicitly on the real QWidgetAction addWidget returns, the
        same real text/icon _act's own QAction-based buttons already
        carry."""
        tb = getattr(self, '_tb_overlays', None)
        buttons = getattr(self, '_overlay_toggle_buttons', [])
        if tb is None or not buttons:
            return
        for btn in buttons:
            action = tb.addWidget(btn)
            label = getattr(btn, '_label', None) or btn.text()
            action.setText(label)
            action.setToolTip(label)
            if not btn.icon().isNull():
                action.setIcon(btn.icon())

    def _on_generate_radar_tiles_clicked(self): #vers 3
        """Offer to load every IPL, pick an output folder, generate, then offer Radar Workshop."""
        hidden = sorted(getattr(self, '_hidden_ipls', set()))
        if hidden and getattr(self, '_ipl_sections_table', None) is not None:
            ans = QMessageBox.question(
                self, "Generate Radar Tiles",
                f"{len(hidden)} IPL(s) are not loaded - the radar will have gaps.\n\n"
                "Load all IPLs first?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                | QMessageBox.StandardButton.Cancel)
            if ans == QMessageBox.StandardButton.Cancel:
                return
            if ans == QMessageBox.StandardButton.Yes:
                self._load_selected_ipl_sections(
                    [self._ipl_display_order.index(n) for n in hidden if n in self._ipl_display_order])
        start_dir = self.map_settings.get('radar_tiles_output_dir') or os.path.expanduser("~")
        output_dir = QFileDialog.getExistingDirectory(
            self, "Select Output Folder for Radar Tiles", start_dir)
        if not output_dir:
            return
        self.map_settings.set('radar_tiles_output_dir', output_dir)
        self._generate_radar_tiles(output_dir)
        if getattr(self, '_last_radar_tile_paths', None) and QMessageBox.question(
                self, "Generate Radar Tiles",
                f"{len(self._last_radar_tile_paths)} tile(s) saved.\n\nOpen them as a radar map "
                "in Radar Workshop?") == QMessageBox.StandardButton.Yes:
            self._send_radar_tiles_to_radar_workshop()

    def _generate_radar_tiles(self, output_dir, only=None): #vers 4
        """Generate every real radar/minimap tile for the currently
        loaded world (Aug 20 2026). only: tile indices to redo, None = all."""
        loader = getattr(self, '_world_loader', None)
        if loader is None or not getattr(loader, 'instances', None):
            QMessageBox.information(self, "Generate Radar Tiles",
                "No world currently loaded - load a game root first.")
            return
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'capture_radar_tile'):
            QMessageBox.warning(self, "Generate Radar Tiles",
                "The 3D viewport isn't available to render tiles from.")
            return

        from apps.methods.gta_dat_parser import compute_radar_grid, RADAR_GRID_PRESETS
        game_key = getattr(loader, 'game', 'sa')
        preset = RADAR_GRID_PRESETS.get(game_key, RADAR_GRID_PRESETS['sa'])
        tiles = compute_radar_grid(**preset)
        if only is not None:
            tiles = [t for t in tiles if t.index in set(only)]
        os.makedirs(output_dir, exist_ok=True)

        progress = QProgressDialog(
            "Generating radar tiles...", "Cancel", 0, len(tiles), self)
        progress.setWindowModality(Qt.WindowModality.WindowModal)
        saved = 0

        #  paths of every tile actually saved this run (Aug 20 2026)
        if only is None or not getattr(self, '_last_radar_tile_paths', None):
            self._last_radar_tile_paths = []
        radar_layer = getattr(vp, '_show_radar_tex_layer', False)
        vp._show_radar_tex_layer = False            # old radar must not show through gaps
        for tile in tiles:
            if progress.wasCanceled():
                break
            progress.setValue(tiles.index(tile))
            progress.setLabelText(
                f"Tile {tile.index} (row {tile.row}, col {tile.col})...")
            QApplication.processEvents()
            cx = (tile.min_x + tile.max_x) / 2.0
            cy = (tile.min_y + tile.max_y) / 2.0
            tile_size = tile.max_x - tile.min_x
            try:
                show_grid = self.map_settings.get('radar_tiles_show_grid')
                image = vp.capture_radar_tile(cx, cy, tile_size, show_grid=show_grid)
                out_path = os.path.join(output_dir, f"radar{tile.index:02d}.png")
                image.save(out_path)
                if out_path not in self._last_radar_tile_paths:
                    self._last_radar_tile_paths.append(out_path)
                saved += 1
            except Exception as e:
                print(f"[Radar Generator] Failed tile {tile.index}: {e}")
        vp._show_radar_tex_layer = radar_layer
        vp.update()
        progress.setValue(len(tiles))
        self._set_status(f"Generated {saved} of {len(tiles)} radar tiles to {output_dir}")

    def _on_radar_tiles_context_menu(self, button, pos): #vers 7
        """Right-click menu on the Radar button - "Send to TXD
        Workshop" (Aug 20 2026)"""
        menu = QMenu(button)
        tiles = getattr(self, '_last_radar_tile_paths', None)
        menu.addAction("Radar / Water Alignment...").triggered.connect(self._show_radar_align_dialog)
        fix = menu.addAction("Water fix (VC -400 X shift)")
        fix.setCheckable(True)
        fix.setChecked(self._water_fix_active())
        fix.toggled.connect(self._set_water_fix)
        cur = self._water_scale()
        sm = menu.addMenu(f"Water scale ({cur * 100:.1f} %)")
        sm.addAction("Custom... (Alignment window)", self._show_radar_align_dialog)
        for label, val in (("50 %", 0.5), ("66.7 %", 2.0 / 3), ("100 %", 1.0), ("200 %", 2.0)):
            act = sm.addAction(label, lambda v=val: self._set_water_scale(v))
            act.setCheckable(True)
            act.setChecked(abs(cur - val) < 1e-6)
        menu.addSeparator()
        regen = menu.addAction("Regenerate tiles touched by unsaved edits")
        regen.setEnabled(bool(getattr(self, '_dirty_ipls', None)))
        regen.triggered.connect(self._regen_changed_radar_tiles)
        menu.addSeparator()
        act = menu.addAction("Send to TXD Workshop (pack PNGs into TXDs)")
        act.setEnabled(bool(tiles))
        if not tiles:
            act.setToolTip("Generate radar tiles first (left-click) - nothing to send yet.")
        act.triggered.connect(self._send_radar_tiles_to_txd_workshop)
        act2 = menu.addAction("Send to Radar Workshop")
        act2.setEnabled(bool(tiles))
        if not tiles:
            act2.setToolTip("Generate radar tiles first (left-click) - nothing to send yet.")
        act2.triggered.connect(self._send_radar_tiles_to_radar_workshop)
        act3 = menu.addAction("Export as RadarTex.img")
        act3.setEnabled(bool(tiles))
        if not tiles:
            act3.setToolTip("Generate radar tiles first (left-click) - nothing to send yet.")
        else:
            act3.setToolTip(
                "Packs every tile straight into one real RadarTex.img\n"
                "archive - add its own real line to your gta_xx.dat\n"
                "to load it, rather than replacing individual\n"
                "radarNN.txd files one at a time.\n\n"
                "Note: written using SA's own real archive format\n"
                "(VERSION_2) as the closest supported option - this\n"
                "app doesn't have a dedicated SOL archive creator yet,\n"
                "so this hasn't been confirmed as an exact match to\n"
                "SOL's own real DIR/IMG archive format.")
        act3.triggered.connect(self._export_radar_tiles_as_img)
        menu.exec(button.mapToGlobal(pos))

    def _build_radar_tile_txd_bytes(self, png_path, name=None): #vers 2
        """TXD bytes (D3D8 layout) for one radar tile PNG."""
        from PIL import Image
        img = Image.open(png_path).convert('RGBA')
        has_alpha = any(p[3] < 255 for p in img.getdata())
        fmt = 'ARGB8888' if has_alpha else 'RGB888'
        if name is None:
            name = os.path.splitext(os.path.basename(png_path))[0]
        tex = {
            'name': name, 'width': img.width, 'height': img.height,
            'rgba_data': img.tobytes(), 'has_alpha': has_alpha,
            'format': fmt, 'alpha_name': name + 'a' if has_alpha else '',
            'mipmaps': 1, 'mipmap_levels': [],
            'raster_format_flags': 0x2600 if not has_alpha else 0x2500,
            'depth': 32, 'platform_id': 8, 'filter_flags': 0x1102,
        }
        from apps.methods.txd_splice import build_txd
        return build_txd([tex], 0x1803FFFF, platform=8)

    def _send_radar_tiles_to_txd_workshop(self): #vers 3
        """Pack every just-generated radarNN.png into its own real
        radarNN.txd, right alongside the PNGs (Aug 20 2026)"""
        paths = getattr(self, '_last_radar_tile_paths', None)
        if not paths:
            QMessageBox.information(self, "Send to TXD Workshop",
                "No radar tiles generated yet - left-click Radar first.")
            return
        packed = 0
        failed = 0
        txd_paths = []
        for png_path in paths:
            try:
                txd_bytes = self._build_radar_tile_txd_bytes(png_path)
                name = os.path.splitext(os.path.basename(png_path))[0]
                txd_path = os.path.join(os.path.dirname(png_path), name + '.txd')
                with open(txd_path, 'wb') as f:
                    f.write(txd_bytes)
                txd_paths.append(txd_path)
                packed += 1
            except Exception as e:
                failed += 1
                print(f"[Radar->TXD] Failed to pack {png_path}: {e}")

        #  assists folder (Project Manager's own concept)
        assists_path = getattr(self.main_window, 'assists_path', None) if self.main_window else None
        copied = 0
        if assists_path and os.path.isdir(assists_path):
            radar_dir = os.path.join(assists_path, 'Radar')
            try:
                os.makedirs(radar_dir, exist_ok=True)
                for txd_path in txd_paths:
                    shutil.copy2(txd_path, os.path.join(radar_dir, os.path.basename(txd_path)))
                    copied += 1
            except Exception as e:
                print(f"[Radar->TXD] Failed copying to assists Radar folder: {e}")

        msg = f"Packed {packed} of {len(paths)} tiles into TXDs"
        if assists_path:
            msg += f" ({copied} copied to assists/Radar)"
        self._set_status(msg)
        QMessageBox.information(self, "Send to TXD Workshop", msg +
            ("" if not failed else f"\n\n{failed} tile(s) failed - see console."))

    def _export_radar_tiles_as_img(self): #vers 2
        """Pack every just-generated radar tile into one real,
        combined RadarTex.img archive (Aug 20 2026)"""
        paths = getattr(self, '_last_radar_tile_paths', None)
        if not paths:
            QMessageBox.information(self, "Export as RadarTex.img",
                "No radar tiles generated yet - left-click Radar first.")
            return
        try:
            from apps.methods.img_core_classes import IMGFile, IMGVersion
        except Exception as e:
            QMessageBox.warning(self, "Export as RadarTex.img",
                f"Required module not available: {e}")
            return

        default_dir = self.map_settings.get('radar_tiles_output_dir') or os.path.expanduser("~")
        out_path, _ = QFileDialog.getSaveFileName(
            self, "Export RadarTex.img", os.path.join(default_dir, "RadarTex.img"),
            "IMG Archives (*.img);;All Files (*)")
        if not out_path:
            return

        est_mb = max(10, (len(paths) * 64) // 1024 + 5)

        img = IMGFile()
        if not img.create_new(out_path, IMGVersion.VERSION_2, initial_size_mb=est_mb):
            QMessageBox.warning(self, "Export as RadarTex.img",
                "Failed to create the new IMG archive.")
            return

        img.remove_entry('replaceme.dff')

        packed = 0
        failed = 0
        for idx, png_path in enumerate(paths):
            try:
                # SOL's own real, confirmed 4-digit tile naming
                # (radar0000.txd...radar1295.txd) - not the 2-digit
                # convention a loose, individual .txd file uses.
                name = f"radar{idx:04d}"
                txd_bytes = self._build_radar_tile_txd_bytes(png_path, name=name)
                if img.add_entry(name + '.txd', txd_bytes, auto_save=False):
                    packed += 1
                else:
                    failed += 1
            except Exception as e:
                failed += 1
                print(f"[Radar->RadarTex.img] Failed to pack {png_path}: {e}")

        if not img.save_img_file():
            QMessageBox.warning(self, "Export as RadarTex.img",
                f"Packed {packed} tile(s) but failed to save the archive itself.")
            return

        msg = f"Exported {packed} of {len(paths)} tiles to {os.path.basename(out_path)}"
        self._set_status(msg)
        QMessageBox.information(self, "Export as RadarTex.img", msg +
            ("" if not failed else f"\n\n{failed} tile(s) failed - see console."))

    def _send_radar_tiles_to_radar_workshop(self): #vers 1
        """Open a real Radar Workshop instance and load every just-
        generated radarNN.png directly into its own matching tile
        slot (Aug 20 2026)"""
        paths = getattr(self, '_last_radar_tile_paths', None)
        if not paths:
            QMessageBox.information(self, "Send to Radar Workshop",
                "No radar tiles generated yet - left-click Radar first.")
            return
        try:
            from PIL import Image
            from apps.components.Radar_Editor.radar_workshop import RadarWorkshop
        except Exception as e:
            QMessageBox.warning(self, "Send to Radar Workshop",
                f"Required module not available: {e}")
            return

        loader = getattr(self, '_world_loader', None)
        game_key = getattr(loader, 'game', 'sa') if loader is not None else 'sa'
        preset_map = {'gta3': 'III PC', 'vc': 'VC PC', 'sa': 'SA PC'}
        preset_key = preset_map.get(game_key, 'SA PC')

        try:
            w = RadarWorkshop(None, self.main_window)
            w._on_game_changed(preset_key)
            tile_w = getattr(w, '_tw', 128)
            tile_h = getattr(w, '_th', 128)
            loaded = 0
            failed = 0
            for png_path in paths:
                try:
                    fname = os.path.splitext(os.path.basename(png_path))[0]
                    idx = int(''.join(c for c in fname if c.isdigit()))
                    img = Image.open(png_path).convert('RGBA')
                    if img.size != (tile_w, tile_h):
                        img = img.resize((tile_w, tile_h), Image.LANCZOS)
                    w.ws_commit_draw(idx, img.tobytes())
                    loaded += 1
                except Exception as e:
                    failed += 1
                    print(f"[Radar->Workshop] Failed to load {png_path}: {e}")
            w.show()
        except Exception as e:
            QMessageBox.warning(self, "Send to Radar Workshop",
                f"Failed to open Radar Workshop: {e}")
            return

        msg = f"Sent {loaded} of {len(paths)} tiles to Radar Workshop ({preset_key})"
        self._set_status(msg)
        if failed:
            QMessageBox.information(self, "Send to Radar Workshop",
                msg + f"\n\n{failed} tile(s) failed - see console.")

    def _write_img_entry(self, img_path, name, data): #vers 1
        """Replace one IMG entry (archive backed up by the IMG writer); raises on failure."""
        cache = getattr(self, '_model_cache', None)
        img = cache._opened_img_files.get(img_path) if cache is not None else None
        if img is None:
            from apps.methods.img_core_classes import IMGFile
            img = IMGFile(img_path)
            if not img.open():
                raise IOError(f"couldn't open {img_path}")
        if not img.add_entry(name, data):
            raise IOError(getattr(img, 'last_error', '') or f"{os.path.basename(img_path)} refused {name}")
        if cache is not None:
            cache._geometry_cache.pop(os.path.splitext(name)[0].lower(), None)

    def _prelight_menu(self, global_pos): #vers 2
        """Prelight button right-click: generate / clear world prelighting."""
        menu = QMenu(self)
        menu.addAction("Generate World Prelighting...", self._generate_world_prelight)
        menu.addAction("Save World Prelighting to DFFs...", self._save_world_prelight)
        menu.addAction("Clear World Prelighting", self._clear_world_prelight)
        menu.exec(global_pos)

    def _generate_world_prelight(self): #vers 1
        """Sun + ambient + directional shadows into every displayed object's vertex colours."""
        from PyQt6.QtWidgets import QProgressDialog
        from apps.components.Map_Editor.depends.map_prelight import (PrelightDialog, light_vector,
                                                                      world_prelight)
        vp = getattr(self, 'preview_widget', None)
        entries = list(getattr(vp, '_world_instances', None) or [])
        if not entries:
            self._set_status("World prelighting: load a world first")
            return
        dlg = PrelightDialog(self, self.map_settings, world=True)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        amb, dif, az, el, strength, shadows, dark, res = dlg.values()
        prog = QProgressDialog("Generating prelighting...", None, 0, 100, self)
        prog.setWindowTitle("World Prelighting")
        prog.setMinimumDuration(0)

        def tick(i, n):
            prog.setValue(int(100 * i / max(1, n)))
            QApplication.processEvents()
        n = world_prelight(entries, light_vector(az, el), amb, dif, strength, shadows, dark, res, tick)
        prog.close()
        vp._clear_world_display_lists()
        vp.update()
        self._set_status(f"World prelighting: {n} object(s) lit" + (" with shadows" if shadows else ""))

    def _save_world_prelight(self): #vers 1
        """Write generated prelighting into each model's DFF (first lit instance per model); undoable."""
        from apps.components.Map_Editor.depends.map_prelight import apply_instance_colours
        from apps.methods.dff_parser import DFFParser, detect_dff
        from apps.methods.dff_patch import patch_dff
        vp = getattr(self, 'preview_widget', None)
        cache = getattr(self, '_model_cache', None)
        lit = [e for e in (getattr(vp, '_world_instances', None) or []) if e.get('inst_prelit')]
        if not lit or cache is None:
            self._set_status("Save prelighting: generate world prelighting first")
            return
        firsts, shared = {}, {}
        for e in lit:
            name = getattr(e.get('instance'), 'model_name', '') or ''
            if name:
                key = name.lower()
                firsts.setdefault(key, e)
                shared[key] = shared.get(key, 0) + 1
        multi = sum(1 for v in shared.values() if v > 1)
        if QMessageBox.question(
                self, "Save World Prelighting",
                f"Write prelighting into {len(firsts)} DFF(s) in the loaded IMGs?\n\n"
                f"{multi} model(s) are placed more than once; they take the colours "
                f"(and shadows) of their first placement.") != QMessageBox.StandardButton.Yes:
            return
        writes, notes = [], []
        for key, e in firsts.items():
            hit = None
            for img_path, entry in cache._dff_index.get(key, []):
                data = cache._read_entry(img_path, entry)
                if data and detect_dff(data):
                    hit = (img_path, entry, data)
                    break
            if hit is None:
                notes.append(f"{key}: DFF not found in loaded IMGs")
                continue
            img_path, entry, data = hit
            model = DFFParser(data, key).parse()
            if not apply_instance_colours(model, e['inst_prelit']):
                notes.append(f"{key}: no prelit colours in the DFF")
                continue
            try:
                new, _rep = patch_dff(data, model)
            except Exception as ex:
                notes.append(f"{key}: {ex}")
                continue
            if new != data:
                writes.append((img_path, entry.name, data, new))

        def _apply(k):
            for img_path, name, old, new in writes:
                self._write_img_entry(img_path, name, (old, new)[k])

        if writes:
            try:
                _apply(1)
            except Exception as ex:
                QMessageBox.warning(self, "Save World Prelighting", f"Write failed: {ex}")
                return
            self._push_map_undo(lambda: _apply(0), lambda: _apply(1), f"Save prelighting ({len(writes)} DFF)")
        msg = f"Prelighting saved into {len(writes)} DFF(s)"
        if notes:
            QMessageBox.information(self, "Save World Prelighting", msg + "\n\n" + "\n".join(notes[:30]))
        self._set_status(msg)

    def _clear_world_prelight(self): #vers 1
        """Back to each model's own vertex colours."""
        vp = getattr(self, 'preview_widget', None)
        n = 0
        for e in getattr(vp, '_world_instances', None) or []:
            if e.pop('inst_prelit', None) is not None:
                n += 1
        if vp is not None:
            vp._clear_world_display_lists()
            vp.update()
        self._set_status(f"World prelighting cleared ({n} object(s))")

    def _bake_prelight_selected(self): #vers 1
        """Bake ambient + sun into the prelit colours of the selected objects' DFFs (undoable)."""
        insts = self._selected_instances()
        cache = getattr(self, '_model_cache', None)
        if not insts or cache is None:
            self._set_status("Bake prelight: select objects in a loaded world first")
            return
        from apps.components.Map_Editor.depends.map_prelight import PrelightDialog, bake_prelight, light_vector
        from apps.methods.dff_parser import DFFParser, detect_dff
        from apps.methods.dff_patch import patch_dff
        dlg = PrelightDialog(self, self.map_settings)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        amb, dif, az, el, strength = dlg.values()
        sun = light_vector(az, el)
        vp = self.preview_widget
        firsts = {}
        for i in insts:
            firsts.setdefault(i.model_name.lower(), i)
        writes, notes = [], []
        for key, inst in firsts.items():
            hit = None
            for img_path, entry in cache._dff_index.get(key, []):
                data = cache._read_entry(img_path, entry)
                if data and detect_dff(data):
                    hit = (img_path, entry, data)
                    break
            if hit is None:
                notes.append(f"{key}: DFF not found in loaded IMGs")
                continue
            img_path, entry, data = hit
            model = DFFParser(data, key).parse()
            eff = self._effective_rotation(inst)
            lm = vp._quat_rotate((-eff[0], -eff[1], -eff[2], eff[3]), sun)     # world sun -> model space
            if not bake_prelight(model, lm, amb, dif, strength):
                notes.append(f"{key}: no prelit colours to bake into")
                continue
            try:
                new, _rep = patch_dff(data, model)
            except Exception as e:
                notes.append(f"{key}: {e}")
                continue
            if new == data:
                notes.append(f"{key}: unchanged")
                continue
            writes.append((img_path, entry.name, data, new))

        def _apply(k):
            for img_path, name, old, new in writes:
                self._write_img_entry(img_path, name, (old, new)[k])
            self._apply_ipl_visibility_filter(auto_fit=False)

        if writes:
            try:
                _apply(1)
            except Exception as e:
                QMessageBox.warning(self, "Bake Prelight", f"Write failed: {e}")
                return
            self._push_map_undo(lambda: _apply(0), lambda: _apply(1), f"Bake prelight ({len(writes)} DFF)")
        msg = f"Baked prelight into {len(writes)} DFF(s)"
        if notes:
            QMessageBox.information(self, "Bake Prelight", msg + "\n\n" + "\n".join(notes[:30]))
        self._set_status(msg)

    def _create_ipl_controls_dock(self): #vers 12
        """Dedicated dock for IPL viewing/filtering controls."""
        panel = QWidget()
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(6, 6, 6, 6)
        lay.setSpacing(4)
        _compact_18 = "padding: 0px 4px; margin: 0px; border-width: 1px;"

        # Row 1: IPL section tab bar (Aug 1 2026)
        from PyQt6.QtWidgets import QTabBar
        ipl_tab_bar = QTabBar()
        ipl_tab_bar.setExpanding(False)
        ipl_tab_bar.setDrawBase(False)
        # Match Object Browser's IMG/DAT/IDE/IPL tab-button sizing (Aug 14 2026)
        ipl_tab_bar.setStyleSheet(
            "QTabBar::tab { height: 18px; min-width: 0px; padding: 0px 6px; margin: 0px; "
            "font-weight: bold; border-width: 1px; }"
            "QTabBar::tab:disabled { color: palette(mid); }")
        tab_specs = [
            ('inst', "INST", "INST - Item Instances", True, None),
            ('cull', "CULL", "CULL - Object Culling", True, None),
            ('zone', "ZON",  "ZONE - Map Zones", True, None),
            ('path', "PATH", "PATH - Pedestrian / Vehicle Paths (GTA3/VC)", True, None),
            ('grge', "GRGE", "GRGE - Garages (SA)", True, None),
            ('enex', "ENEX", "ENEX - Entrances / Exits (SA)", True, None),
            ('occl', "OCCL", "OCCL - Occlusion Zones (VC/SA)", True, None), # 'occl' (Aug 16 2026)
            ('pick', "PICK", "PICK - Pickup Spawns (SA)", False,
             "Not parsed yet - no verified real sample data to build this on."),
            ('cars', "CARS", "CARS - CAR placement (SA)", False,
             "Not parsed yet - no verified real sample data to build this on."),
            ('jump', "JUMP", "JUMP - Unique Stunt Jumps (SA)", False,
             "Not parsed yet - no verified real sample data to build this on."),
            ('tcyc', "TCYC", "TCYC - Timecycle/Weather Zones (SA)", False,
             "Not parsed yet - no verified real sample data to build this on."),
            ('auzo', "AUZO", "AUZO - Audio Zones (SA)", True, None),
            ('mult', "MULT", "MULT - Unused (SA)", False,
             "Documented as never actually used by the game itself\n(ignored at runtime) - stub kept for completeness only."),
        ]
        self._ipl_tab_keys = [spec[0] for spec in tab_specs]
        for key, label_text, tooltip, enabled, disabled_reason in tab_specs:
            idx = ipl_tab_bar.addTab(label_text)
            ipl_tab_bar.setTabEnabled(idx, enabled)
            ipl_tab_bar.setTabToolTip(idx, tooltip if enabled else f"{tooltip}\n\n{disabled_reason}")
        ipl_tab_bar.setCurrentIndex(0)
        ipl_tab_bar.currentChanged.connect(self._on_ipl_tab_changed)
        self._ipl_tab_bar = ipl_tab_bar
        lay.addWidget(ipl_tab_bar)
        self._ipl_data_type = 'inst'

        # Row 2: Ignore Scaling / Generic.txd / LOD view
        opts_row = QHBoxLayout()
        ignore_scaling_chk = QCheckBox("Ignore Scaling")
        ignore_scaling_chk.setFixedHeight(18)
        ignore_scaling_chk.setStyleSheet(_compact_18)
        ignore_scaling_chk.setToolTip(
            "Treat a (1,1,1) scale as equivalent to (0,0,0) for\n"
            "interpretation purposes only - never writes anything\n"
            "back to the IPL file. Useful for converted IPLs (e.g.\n"
            "GTA SA data converted to VC's format) where the Scale\n"
            "columns may be broken/placeholder rather than real data.")
        ignore_scaling_chk.toggled.connect(self._on_ignore_scaling_toggled)
        self._ignore_scaling_chk = ignore_scaling_chk
        opts_row.addWidget(ignore_scaling_chk)

        # Time switch for TOBJ objects (Aug 1 2026)
        time_chk = QToolButton()
        time_chk.setText("Time")
        time_chk.setCheckable(True)
        time_chk.setFixedHeight(24)
        time_chk.setStyleSheet(_compact_18)
        time_chk.setToolTip(
            "Filter TOBJ (timed) objects by time of day - when on,\n"
            "only shows TOBJ instances whose IDE time_on/time_off\n"
            "range includes the hour selected here. Non-TOBJ objects\n"
            "are never affected by this switch.")

        # QTimeEdit instead of a plain 0-23 spinbox (Aug 1 2026)
        from PyQt6.QtCore import QTime
        time_edit = QTimeEdit(QTime(12, 0))
        time_edit.setDisplayFormat("HH:mm")
        time_edit.setFixedHeight(24)
        time_edit.setFixedWidth(63)
        time_edit.setEnabled(False)
        time_edit.setToolTip("Simulated time of day for the Time switch above")
        time_chk.toggled.connect(time_edit.setEnabled)
        time_chk.toggled.connect(self._on_tobj_time_toggled)
        time_edit.timeChanged.connect(self._on_tobj_time_changed)
        self._tobj_time_chk = time_chk
        self._tobj_time_spin = time_edit   # kept name for existing callers

        # Play/Stop + Settings (Aug 1 2026)
        time_play_btn = QPushButton()
        time_play_btn.setIcon(self.icon_factory.launch_icon(16))
        time_play_btn.setFixedSize(24, 24)
        time_play_btn.setStyleSheet(_compact_18)
        time_play_btn.setToolTip("Start advancing time automatically")
        time_stop_btn = QPushButton()
        time_stop_btn.setIcon(self.icon_factory.stop_icon(16))
        time_stop_btn.setFixedSize(24, 24)
        time_stop_btn.setStyleSheet(_compact_18)
        time_stop_btn.setEnabled(False)
        time_stop_btn.setToolTip("Stop advancing time")
        time_settings_btn = QPushButton()
        time_settings_btn.setIcon(self.icon_factory.settings_icon(16))
        time_settings_btn.setFixedSize(24, 24)
        time_settings_btn.setStyleSheet(_compact_18)
        time_settings_btn.setToolTip("Time flow settings (speed)")
        self._time_flow_timer = QTimer(self)
        self._time_flow_minutes_per_tick = 1
        self._time_flow_interval_ms = 1000
        self._time_flow_timer.timeout.connect(self._on_time_flow_tick)
        time_play_btn.clicked.connect(lambda: self._start_time_flow(time_play_btn, time_stop_btn))
        time_stop_btn.clicked.connect(lambda: self._stop_time_flow(time_play_btn, time_stop_btn))
        time_settings_btn.clicked.connect(self._show_time_flow_settings_popup)
        self._time_play_btn = time_play_btn
        self._time_stop_btn = time_stop_btn

        #TODO; Play and Stop icon isn't theme away, Bug.

        # The Advanced button/menu that used to sit here is gone (Aug 1 2026)
        from PyQt6.QtGui import QActionGroup
        render_lod_btn = QPushButton("Render: Textured")
        render_lod_btn.setFixedHeight(18)
        render_lod_btn.setStyleSheet(_compact_18)
        render_lod_menu = QMenu(render_lod_btn)

        # Model render style (Sep 5 2026  own detailed
        # follow-up: renamed for clarity, and changed from a strict
        # exclusive radio group to independently checkable entries
        # that can also all be deselected - "Each entry in render
        # needs to be selected or unselected... Deselect Model
        # Textures to only show Col Wireframe". Soft-exclusive: picking
        # one unchecks any other (still only one model style makes
        # sense at a time), but unlike a real QActionGroup, clicking
        # the already-checked one turns it off, leaving self._mode as
        # None - _draw_world_instances' own None branch then shows
        # collision only, with no separate Col Only mode needed.
        render_specs = [
            ('textured',   "Model Textures",         "Full textures, as loaded from the game's TXDs"),
            ('solid',      "Model W/Out Textures",   "Flat/lit shading, no textures"),
            ('semi_solid', "Model Semi-Solid",       "Flat/lit shading with reduced opacity - a ghosted, see-through look"),
            ('wireframe',  "Model Wireframe",        "Edges only, no fill"),
            ('dots',       "Show models as Dots",    "Just a point at each instance's own placement - no model or\n"
                                                      "texture loading at all, for viewing/navigating a huge map fast"),
        ]
        render_labels = {m: l for m, l, t in render_specs}
        render_actions = []

        def _on_render_style_toggled(checked, mode):
            if checked:
                for action, m in render_actions:
                    if m != mode:
                        action.blockSignals(True)
                        action.setChecked(False)
                        action.blockSignals(False)
                self._set_world_render_mode(mode, render_labels[mode], render_lod_btn)
            elif not any(a.isChecked() for a, m in render_actions):
                self._set_world_render_mode(None, "None", render_lod_btn)

        for mode, label_text, tooltip in render_specs:
            action = render_lod_menu.addAction(label_text)
            action.setCheckable(True)
            action.setChecked(mode == getattr(
                getattr(self, 'preview_widget', None), '_mode', 'textured'))
            action.setToolTip(tooltip)
            render_actions.append((action, mode))
        for action, mode in render_actions:
            action.toggled.connect(lambda checked, m=mode: _on_render_style_toggled(checked, m))

        render_lod_menu.addSeparator()

        # LOD display mode (Sep 5 2026,  originally reported
        # as "Show LOD only can go, does not work" alongside removing
        # Show Col Only, then clarified as a typo - only Col Only was
        # meant to go) - back to a real 3-way exclusive radio, same
        # mechanism as before, unaffected by the Col Only retirement.
        lod_group = QActionGroup(render_lod_menu)
        lod_group.setExclusive(True)
        lod_specs = [
            ('lod',    "Show LOD only", "Show only LOD (low-detail) instances"),
            ('normal', "Show Normals",  "Show only normal-detail instances (default)"),
            ('both',   "Show Both",     "Show both normal and LOD (low-detail) instances together"),
        ]
        for mode, label_text, tooltip in lod_specs:
            action = render_lod_menu.addAction(label_text)
            action.setCheckable(True)
            action.setChecked(mode == getattr(self, '_lod_display_mode', 'normal'))
            action.setToolTip(tooltip)
            action.triggered.connect(lambda checked, m=mode: self._set_lod_display_mode(m) if checked else None)
            lod_group.addAction(action)

        render_lod_menu.addSeparator()

        # Collision overlay options (Aug 14 2026, renamed + made
        # soft-exclusive Sep 5 2026  "Only one COL entry
        # should be shown, as there is no point trying to select them
        # all" - same soft-exclusive-with-deselect pattern as the
        # model-style group above, just against the 4 show_col_*
        # viewport flags instead of self._mode.
        col_specs = [
            ('ghosted',        "Show COL Ghosted",        "set_show_col_ghosted",
             "Overlay collision geometry as a low-opacity ghost"),
            ('surface_mapped', "Show COL Surfaces Mapping", "set_show_col_surface_mapped",
             "Overlay collision geometry coloured by surface/material type"),
            ('semi_solid',     "Show COL Semi-Solid",     "set_show_col_semi_solid",
             "Overlay collision geometry at higher opacity than Ghosted"),
            ('wireframe',      "Show COL Wireframe",      "set_show_col_wireframe",
             "Overlay collision geometry as edges only"),
        ]
        col_actions = []

        def _on_col_style_toggled(checked, mode, setter_name):
            vp = getattr(self, 'preview_widget', None)
            if checked:
                for action, m, s in col_actions:
                    if m != mode:
                        action.blockSignals(True)
                        action.setChecked(False)
                        action.blockSignals(False)
                        other_setter = getattr(vp, s, None) if vp is not None else None
                        if callable(other_setter):
                            other_setter(False)
            self._on_col_render_option_toggled(setter_name, checked)

        for mode, label_text, setter_name, tooltip in col_specs:
            action = render_lod_menu.addAction(label_text)
            action.setCheckable(True)
            action.setChecked(bool(getattr(
                getattr(self, 'preview_widget', None), f'show_col_{mode}', False)))
            action.setToolTip(tooltip)
            col_actions.append((action, mode, setter_name))
        for action, mode, setter_name in col_actions:
            action.toggled.connect(
                lambda checked, m=mode, s=setter_name: _on_col_style_toggled(checked, m, s))

        render_lod_menu.addSeparator()

        # Zone render style (Aug 16 2026)
        zon_render_group = QActionGroup(render_lod_menu)
        zon_render_group.setExclusive(True)
        zon_render_specs = [
            ('ghosted',     "Zon - Ghosted",     "Filled, semi-transparent boxes with a defined outline (default)"),
            ('wireframe',   "Zon - Wireframe",   "Edges only, no fill"),
            ('translucent', "Zon - Translucent", "Filled, more see-through boxes with no outline"),
        ]
        for mode, label_text, tooltip in zon_render_specs:
            action = render_lod_menu.addAction(label_text)
            action.setCheckable(True)
            action.setChecked(mode == self.map_settings.get('zone_render_style'))
            action.setToolTip(tooltip)
            action.triggered.connect(
                lambda checked, m=mode: self._set_zone_render_style(m) if checked else None)
            zon_render_group.addAction(action)

        render_lod_btn.setMenu(render_lod_menu)
        render_lod_btn.setToolTip("Choose how the world view renders geometry, and which detail level(s) to show")
        opts_row.addWidget(render_lod_btn)

        # Drag/Move/Rotate IPL - a single button cycling through 4 states (Aug 19 2026.
        ipl_mode_btn = QPushButton("IPL: Off")
        ipl_mode_btn.setFixedHeight(18)
        ipl_mode_btn.setStyleSheet(_compact_18)
        ipl_mode_btn.setToolTip(
            "Click to cycle: Off \u2192 Drag \u2192 Move \u2192 Rotate \u2192 Off.\n"
            "Drag: Shift+click instances (or IPL Sections rows) to\n"
            "build a multi-IPL selection - no drag yet, just selecting.\n"
            "Then Ctrl+click-drag any instance to drag the whole\n"
            "selection together, or just that one IPL if nothing's\n"
            "selected. A plain click alone does nothing.\n"
            "Move: click an instance to open Shift Coordinates for its IPL.\n"
            "Rotate: click an instance to open Rotate for its IPL.\n"
            "Right-click while in Drag: axis-lock options.")
        ipl_mode_btn.clicked.connect(self._on_ipl_mode_button_clicked)
        ipl_mode_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        ipl_mode_btn.customContextMenuRequested.connect(self._on_drag_ipl_context_menu)
        self._ipl_mode_btn = ipl_mode_btn
        self._ipl_interaction_mode = 'off'
        opts_row.addWidget(ipl_mode_btn)

        opts_row.addStretch()
        lay.addLayout(opts_row)

        # Row 3: Time / Nav (Aug 1 2026)
        opts_row2 = QHBoxLayout()

        # 2DFX master toggle (Aug 1 2026)
        dfx_chk = _MapOverlayToggleButton("2DFX", supports_edit=False, icon=OverlayIcons.dfx2d_icon(24))
        dfx_chk.set_shown(True, emit=False)
        dfx_chk.setToolTip(
            "Show 2DFX lights in the world view - when off, no 2DFX\n"
            "lights show at all, regardless of the Time switch. When\n"
            "on, the existing day/night time gating still applies.")
        dfx_chk.show_toggled.connect(self._on_2dfx_master_toggled)
        self._2dfx_master_chk = dfx_chk

        # Nav settings (Aug 1 2026)
        # Show Tobj (Aug 1 2026)
        show_tobj_chk = _MapOverlayToggleButton("Tobj", supports_edit=False, icon=OverlayIcons.tobj_icon(24))
        show_tobj_chk.setToolTip(
            "Show TOBJ (timed) instances in the INST table, appended\n"
            "after the regular rows - filtered to only the ones\n"
            "currently active for the simulated hour set by the Time\n"
            "switch. Off by default, keeping TOBJ instances out of\n"
            "this view entirely.")
        show_tobj_chk.show_toggled.connect(self._on_show_tobj_toggled)
        self._show_tobj_chk = show_tobj_chk

        # Timecyc play/stop toggle (Aug 20 2026,  "the play
        # and stop for timecyc can be merged with the play stop [2DFX]
        # [TOJB] adding a new button on that line that says [TCYC]")
        tcyc_chk = _MapOverlayToggleButton("Tcyc", supports_edit=False, icon=OverlayIcons.tcyc_icon(24))
        tcyc_chk.show_toggled.connect(self._on_tcyc_toggled)
        # Middle-click picks an alternate timecyc.dat (Sep 5 2026, per
        #  : "If there is more than one timecycle, middle-click
        # the time cycle button to select it" - was wired to right-
        # click/edit_toggled, a workaround that predates this button
        # having its own real middle_clicked signal; every other
        # similar alternate-picker button (occl/grge/zon) already uses
        # middle-click, so this brings Tcyc in line with them instead
        # of being the one exception). set_middle_click_menu_available
        # sets the real flag mousePressEvent checks before emitting,
        # but its own tooltip text ("add/delete/save/corner coords")
        # describes zone/box editing, not file-picking - overridden
        # with the correct text for this button right after.
        tcyc_chk.set_middle_click_menu_available(True)
        tcyc_chk.setToolTip(
            "Left-click: play/stop the loaded timecyc file's own day/\n"
            "night sky colour cycle in the 3D view.\n"
            "Middle-click: pick a different timecyc.dat from this\n"
            "app's own depends/timecyc/ asset folder.")
        tcyc_chk.middle_clicked.connect(lambda: self._show_alt_timecyc_menu(tcyc_chk))
        self._tcyc_chk = tcyc_chk

        opts_row2.addWidget(show_tobj_chk)
        opts_row2.addWidget(tcyc_chk)
        opts_row2.addWidget(dfx_chk)
        # time_chk disconnected too (Aug 20 2026,  "[time]
        # button is still on the ipl controls, its not needed there as
        # it's on the ribbon instead") - superseding the earlier,
        # narrower call to keep it (which assumed its own TOBJ time-
        # filtering function was non-redundant) - my own follow-up
        # confirms this should go too, alongside time_edit.
        # opts_row2.addWidget(time_chk)
        # time_edit disconnected from the visible layout (Aug 20 2026,
        #  "The time/clock in IPL controls can be removed
        # since thats on the ribbons / viewpoint") - the on-viewport
        # CRT overlay already shows the current time; this QTimeEdit
        # was now purely redundant with it.
        # opts_row2.addWidget(time_edit)
        # time_play_btn/time_stop_btn/time_settings_btn disconnected
        # from the visible layout (Aug 20 2026,  "[TIME]
        # showing the time in the viewpoint like old style green CRT,
        # click on time for stop and start, right click for
        # settings") - the on-viewport CRT overlay's own left/right-
        # click now replaces these 3 buttons' own real functions.
        # Still constructed above (harmless, just not shown) since
        # _start_time_flow/_stop_time_flow's own existing signature
        # still expects real button objects to enable/disable.
        # opts_row2.addWidget(time_play_btn)
        # opts_row2.addWidget(time_stop_btn)
        # opts_row2.addWidget(time_settings_btn)

        opts_row2.addStretch()
        lay.addLayout(opts_row2)

        # Row 3: per-layer visibility toggles (Aug 1 2026)
        show_paths_btn = _MapOverlayToggleButton("Paths", supports_edit=True, icon=OverlayIcons.paths_icon(24))
        show_paths_btn.show_toggled.connect(self._on_show_paths_toggled)
        show_paths_btn.edit_toggled.connect(self._on_edit_paths_toggled)
        self._show_paths_chk = show_paths_btn

        # Show Tracks (Aug 17 2026)
        show_tracks_btn = _MapOverlayToggleButton("Tracks", supports_edit=False, icon=OverlayIcons.tracks_icon(24))
        show_tracks_btn.show_toggled.connect(self._on_show_tracks_toggled)
        self._show_tracks_chk = show_tracks_btn

        # Show Cull Zones (Aug 16 2026)
        show_cull_btn = _MapOverlayToggleButton("Cull", supports_edit=True, icon=OverlayIcons.cull_icon(24))
        show_cull_btn.show_toggled.connect(self._on_show_cull_boxes_toggled)
        show_cull_btn.edit_toggled.connect(self._on_edit_boxes_toggled)
        show_cull_btn.middle_clicked.connect(
            lambda: self._show_overlay_middle_click_menu('cull', self._add_cull, self._delete_cull, self._save_culls))
        show_cull_btn.set_middle_click_menu_available(True)
        self._show_cull_chk = show_cull_btn

        # Show Zones (Aug 16 2026)
        show_zone_btn = _MapOverlayToggleButton("Zon", supports_edit=True, icon=OverlayIcons.zon_icon(24))
        show_zone_btn.show_toggled.connect(self._on_show_zone_boxes_toggled)
        show_zone_btn.edit_toggled.connect(self._on_edit_boxes_toggled)
        show_zone_btn.middle_clicked.connect(
            lambda: self._show_overlay_middle_click_menu('zone', self._add_zone, self._delete_zone, self._save_zones))
        show_zone_btn.set_middle_click_menu_available(True)
        self._show_zone_chk = show_zone_btn

        # Show Occlusion (Aug 16 2026)
        show_occl_btn = _MapOverlayToggleButton("Occlusion", supports_edit=False, icon=OverlayIcons.occlusion_icon(24))
        show_occl_btn.show_toggled.connect(self._on_show_occl_boxes_toggled)
        show_occl_btn.middle_clicked.connect(
            lambda: self._show_overlay_middle_click_menu('occl', self._add_occl, self._delete_occl, self._save_occls))
        show_occl_btn.set_middle_click_menu_available(True)
        self._show_occl_chk = show_occl_btn

        # Show Garages (Aug 21 2026,  "add support for GRGE")
        show_grge_btn = _MapOverlayToggleButton("Grge", supports_edit=True, icon=OverlayIcons.grge_icon(24))
        show_grge_btn.show_toggled.connect(self._on_show_grge_boxes_toggled)
        show_grge_btn.edit_toggled.connect(self._on_edit_boxes_toggled)
        show_grge_btn.middle_clicked.connect(
            lambda: self._show_overlay_middle_click_menu('grge', self._add_grge, self._delete_grge, self._save_grges))
        show_grge_btn.set_middle_click_menu_available(True)
        self._show_grge_chk = show_grge_btn

        opts_row3 = QHBoxLayout()
        opts_row3.addWidget(show_paths_btn)
        opts_row3.addWidget(show_tracks_btn)
        opts_row3.addWidget(show_cull_btn)
        opts_row3.addWidget(show_zone_btn)
        opts_row3.addWidget(show_occl_btn)
        opts_row3.addWidget(show_grge_btn)
        opts_row3.addStretch()
        lay.addLayout(opts_row3)

        # Show SA Nodes (Aug 19 2026)
        show_sa_nodes_btn = _MapOverlayToggleButton("SA Nodes", supports_edit=False, icon=OverlayIcons.sa_nodes_icon(24))
        show_sa_nodes_btn.show_toggled.connect(self._on_show_sa_nodes_toggled)
        self._show_sa_nodes_chk = show_sa_nodes_btn # Show only with gta.dat loaded (SA)

        # Show Auzo (Aug 20 2026)
        show_auzo_btn = _MapOverlayToggleButton("Auzo", supports_edit=False, icon=OverlayIcons.auzo_icon(24))
        show_auzo_btn.show_toggled.connect(self._on_show_auzo_toggled)
        self._show_auzo_chk = show_auzo_btn

        # Interior filter (Aug 20 2026,  "Can we look at
        # interior values? ... We need a svg to toggle interior
        # models: show 0 only on the viewpoint; showing 0 to 14+ by
        # selecting them") - left-click toggles between the default
        # (interior 0/exterior only) and showing every interior value
        # together; right-click opens a menu of every interior value
        # actually present in the loaded world, with counts, to
        # isolate one specific interior.
        interior_btn = _MapOverlayToggleButton("Int", supports_edit=True, icon=OverlayIcons.interior_icon(24))
        interior_btn.set_shown(False, emit=False)
        interior_btn.show_toggled.connect(self._on_interior_show_all_toggled)
        interior_btn.edit_toggled.connect(lambda checked: self._show_interior_picker_menu(interior_btn))
        interior_btn.setToolTip(
            "Left-click: show all interiors / exterior only.\n"
            "Right-click: pick one interior to isolate.")
        self._interior_btn = interior_btn

        # Show Water (Aug 20 2026)
        show_water_btn = _MapOverlayToggleButton("Water", supports_edit=True, icon=OverlayIcons.water_icon(24))
        show_water_btn.show_toggled.connect(self._on_show_water_toggled)
        show_water_btn.edit_toggled.connect(self._toggle_water_edit)   # quad editing
        # Middle-click switches water layers (Sep 5 2026, .per:
        # "the right click toggle between vis_water and phy_water
        # needs to be moved to the middle button so it doesnt clash
        # with the right click menu" - was on right-click/edit_
        # toggled, a simple two-state switch with no menu at all,
        # which was occupying right-click and blocking a genuine
        # context menu from ever being added there. supports_edit
        # dropped too, since nothing here is a real edit mode.
        show_water_btn.set_middle_click_menu_available(True)
        show_water_btn.setToolTip(
            "Left-click: show/hide Water.\n"
            "Middle-click: switch between visible_map and\n"
            "physical_map layers (waterpro.dat only).\n"
            "Right-click: edit water quads (water.dat).")
        show_water_btn.middle_clicked.connect(
            lambda: self._on_water_layer_toggled(
                not getattr(self, '_water2_showing_physical', False)))
        self._show_water_chk = show_water_btn
        # Disabled until real water data is actually preloaded (Aug 20
        # 2026, re-applied) - re-enables itself automatically.
        show_water_btn.setEnabled(False)
        show_water_btn.setToolTip(
            "No water preloaded yet - File > Preload Game Data Files...\n"
            "Middle-click (once preloaded): switch to waterpro.dat's own\n"
            "other real layer (physical_map, double the resolution).")

        # Generate Radar Tiles (Aug 20 2026)
        radar_gen_btn = _MapOverlayToggleButton("Radar", supports_edit=True, icon=OverlayIcons.radar_icon(24))
        radar_gen_btn.set_shown(bool(self.map_settings.get('show_radar_tex_layer')), emit=False)
        radar_gen_btn.show_toggled.connect(self._on_show_radar_tex_layer_toggled)
        # Middle-click generates the radar tiles (Sep 5 2026, moved
        # off right-click - see below -  "keeping right
        # click menu, and middle click for other functions"). Was
        # originally on right-click (Aug 20 2026,  "right-
        # clicking the radar button should generate the radar"), which
        # blocked this button's own real context menu
        # (_on_radar_tiles_context_menu) from ever being reachable.
        radar_gen_btn.set_middle_click_menu_available(True)
        radar_gen_btn.middle_clicked.connect(lambda: self._on_generate_radar_tiles_clicked())
        # Right-click wires up this button's own real context menu
        # (Send to TXD Workshop/Radar Workshop/Export as RadarTex.img)
        # - previously completely unreachable, since right-click was
        # occupied by the simple "generate" action (now on middle-
        # click instead,  "keeping right click menu, and
        # middle click for other functions").
        radar_gen_btn.edit_toggled.connect(
            lambda checked: self._on_radar_tiles_context_menu(
                radar_gen_btn, radar_gen_btn.rect().bottomLeft()))
        radar_gen_btn.setToolTip(
            "Left-click: show/hide the radar tex layer.\n"
            "Middle-click: generate radar tiles.\n"
            "Right-click: alignment, water fix, send/export options.")
        opts_row4 = QHBoxLayout()
        opts_row4.addWidget(show_sa_nodes_btn)
        opts_row4.addWidget(show_auzo_btn)
        opts_row4.addWidget(show_water_btn)
        opts_row4.addWidget(radar_gen_btn)
        opts_row4.addWidget(interior_btn)
        lay.addLayout(opts_row4)

        # Collected for the display-style toggle (Aug 20 2026, per
        #  : "we could add a toggle in settings, Show IPL Controls
        # = as [Buttons] or ribbon icons") - every real overlay toggle
        # button in this dock, so the toggle can apply to all of them
        # at once rather than needing a separate call per button.
        self._overlay_toggle_buttons = [
            dfx_chk, show_tobj_chk, tcyc_chk, show_paths_btn, show_tracks_btn,
            show_cull_btn, show_zone_btn, show_occl_btn, show_grge_btn, show_sa_nodes_btn,
            show_auzo_btn, show_water_btn, radar_gen_btn, interior_btn,
        ]
        self._apply_ipl_controls_display_style()

        dock = QDockWidget("IPL Controls", self)
        dock.setObjectName("IPL Controls")
        dock.setWidget(panel)
        dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                        QDockWidget.DockWidgetFeature.DockWidgetFloatable |
                        QDockWidget.DockWidgetFeature.DockWidgetClosable)
        self._ipl_controls_dock = dock
        return dock

    def _get_txd_textures(self, txd_name): #vers 5
        """Fetch any named TXD's textures from the game's indexed IMG
        archives (gta3.img is always auto-indexed for every game, per
        GTAWorldLoader.load()'s own docstring: "Always enforces
        models/gta3.img... so TXD Workshop and the Dump TXDs feature
        can always find it"). Returns (textures_dict_or_None,
        source_description, status) - status is one of:
          'loaded'  - found and parsed successfully
          'missing' - not indexed in any IMG archive
          'failed'  - indexed but reading/parsing it failed."""

        model_cache = getattr(self, '_model_cache', None)
        if model_cache is None:
            return None, None, 'missing'
        key = txd_name.lower()
        entries = getattr(model_cache, '_txd_index', {}).get(key, [])
        archive_paths = "; ".join(dict.fromkeys(img_path for img_path, _entry in entries))
        textures = model_cache.get_textures(txd_name)
        loader = getattr(self, '_world_loader', None)
        for parent in (loader.txd_chain(txd_name)[1:] if hasattr(loader, 'txd_chain') else []):
            inherited = model_cache.get_textures(parent)   # txdp parent, child wins
            if inherited:
                textures = {**inherited, **(textures or {})}
        if textures:
            return textures, (archive_paths or "an indexed IMG archive"), 'loaded'
        return (None, (archive_paths or None), 'failed') if entries else (None, None, 'missing')

    def _get_generic_textures(self): #vers 4
        """Thin wrapper kept for the Generic.txd button - see
        _get_txd_textures for the actual fetch logic (no fallback,
        see its docstring). Returns the same 3-tuple (textures,
        source, status)."""
        return self._get_txd_textures('generic')

    def _preload_generic_ide_textures(self): #vers 3
        """Collect every distinct TXD name referenced by ANY loaded
        object (not just generic.ide's own entries) and preload all
        of them via _get_txd_textures. Returns (all_textures,
        covered_txd_names) - the second so callers can skip
        re-fetching these same TXDs in their own per-model loop.

        Cached after the first successful computation (Aug 1 2026)"""
        loader = getattr(self, '_world_loader', None)
        if loader is None or not hasattr(loader, 'objects'):
            return [], set()
        cache = getattr(self, '_generic_ide_textures_cache', None)
        if cache is not None and cache[0] == id(loader):
            return cache[1], cache[2]
        distinct_txds = set()
        object_count = 0
        source_ide_names = set()
        for obj in loader.objects.values():
            object_count += 1
            source = (getattr(obj, 'source_ide', '') or '')
            if source:
                source_ide_names.add(os.path.basename(source).lower())
            if obj.txd_name:
                distinct_txds.add(obj.txd_name.lower())

        # Status feedback (Aug 1 2026)
        if object_count == 0:
            self._set_status("No IDE objects loaded - textures can't be preloaded")
        all_textures = []
        fetched_count = 0
        for txd_name in sorted(distinct_txds):
            textures, _source, _status = self._get_txd_textures(txd_name)
            if textures:
                fetched_count += 1
                all_textures.extend(textures.values())
        if object_count > 0:
            self._set_status(
                f"{object_count} objects across {len(source_ide_names)} IDE file(s), "
                f"{len(distinct_txds)} distinct TXDs, {fetched_count} "
                f"found/fetched, {len(all_textures)} textures total")
        self._generic_ide_textures_cache = (id(loader), all_textures, distinct_txds)
        return all_textures, distinct_txds

    def _set_world_render_mode(self, mode, label, btn): #vers 1
        """Switch the world view's render mode"""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_render_mode'):
            vp.set_render_mode(mode)
            # Force the queued repaint to actually process now (Aug 1 2026)
            vp.repaint()
        self._world_render_mode_set = True
        btn.setText(f"Render: {label}")

        self._refresh_ipl_inst_file_panel()

    def _on_toggle_mesh_col(self, checked): #vers 1
        """Toggle Mesh button, repurposed  "toggle mesh icon
        can switch between col model and normal model" - checked
        shows the regular model mesh; unchecked hides it and shows
        collision instead (semi-solid, reusing the same real overlay
        the Render dropdown's own "Show Semi-Solid Col" option
        already provides)."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None:
            return
        vp.set_show_mesh(checked)
        if hasattr(vp, 'set_show_col_semi_solid'):
            vp.set_show_col_semi_solid(not checked)
        vp.repaint()

    def _on_col_render_option_toggled(self, setter_name, checked): #vers 3
        """One of the four collision overlay actions in the Render:
        dropdown menu was toggled (Aug 14 2026)"""
        vp = getattr(self, 'preview_widget', None)
        setter = getattr(vp, setter_name, None) if vp is not None else None
        if callable(setter):
            setter(checked)
            vp.repaint()

    def _on_toggle_force_prelight(self, checked): #vers 1
        """Force Prelighting ribbon button toggled (Sep 5 2026, per
         : "showing dark models... we need a prelighting on/off
        SVG button")."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_prelight'):
            vp.set_prelight(checked)
            vp.repaint()

    def _on_load_generic_txd_clicked(self): #vers 3
        """Load generic.txd and show it in the Textures dock. Looks
        it up in the game's indexed IMG archives only, no loose-file
        fallback (Aug 1 2026)"""
        if getattr(self, '_model_cache', None) is None:
            QMessageBox.information(self, "Generic.txd", "No world loaded yet.")
            return
        textures, source, _status = self._get_generic_textures()
        if not textures:
            QMessageBox.warning(self, "Generic.txd",
                "generic.txd not found in any indexed IMG archive (e.g. gta3.img).")
            return
        self._generic_textures = textures
        if hasattr(self, '_tex_count_lbl'):
            self._tex_count_lbl.setText(
                f"{len(textures)} texture{'s' if len(textures) != 1 else ''} (generic.txd, from {source})")
        tex_list = getattr(self, '_tex_list', None)
        if tex_list is not None:
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

    def _create_ipl_inst_file_panel(self): #vers 3
        """Editable cells table for whichever IPL is currently selected
        in the [IPL] tab, filtered to whichever data type (INST/CULL/
        ZONE) is selected there - one row per instance line, one
        column per field (ID/Model/Interior/PosX,Y,Z/ScaleX,Y,Z/
        RotX,Y,Z,W). Updates live as different IPL files are clicked."""
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(6, 6, 6, 6)
        lay.setSpacing(4)

        table = QTableWidget(0, 13)
        table.setHorizontalHeaderLabels([
            "ID", "Model", "Int",
            "Pos X", "Pos Y", "Pos Z",
            "Scale X", "Scale Y", "Scale Z",
            "Rot X", "Rot Y", "Rot Z", "Rot W"]) #TODO need to check Rot w.
        table.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectItems)
        #  fix (Aug 21 2026  own real, uploaded
        # zon.png screenshot: "clicking the model in the file list
        # window, wants to rename it, not take us to the zon entries
        # that belongs to it") - DoubleClicked as an edit trigger
        # here directly conflicted with cellDoubleClicked (below),
        # wired to jump the viewport to that row's own real instance -
        # double-clicking the Model column both opened Qt's own
        # inline text editor right over the cell (visually dominant,
        # looking exactly like "wants to rename") and tried to
        # navigate the viewport at the same time, in the background,
        # easy to miss ever happening at all. EditKeyPressed alone
        # (F2/typing to start editing a selected cell) still works
        # for real, intentional inline edits - only DoubleClicked is
        # removed, since only that one genuinely conflicted with
        # double-click's own separate, real meaning here.
        table.setEditTriggers(QTableWidget.EditTrigger.EditKeyPressed)
        self._apply_compact_table_style(table)
        font = table.font(); font.setFamily("monospace"); table.setFont(font)
        table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        table.customContextMenuRequested.connect(self._on_ipl_inst_file_context_menu)
        table.cellClicked.connect(self._on_ipl_inst_file_cell_clicked)
        table.cellDoubleClicked.connect(self._on_ipl_inst_file_cell_double_clicked)
        lay.addWidget(table)
        self._ipl_inst_file_table = table

        dock = QDockWidget("IPL File Display", self)

        # objectName kept as the old "IPL Inst File" (Aug 14 2026)
        dock.setObjectName("IPL Inst File")
        dock.setWidget(panel)
        dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                        QDockWidget.DockWidgetFeature.DockWidgetFloatable |
                        QDockWidget.DockWidgetFeature.DockWidgetClosable)
        self._ipl_inst_file_dock = dock
        return dock


    def _on_ignore_scaling_toggled(self, checked): #vers 1
        """Just re-renders the currently shown table with the new
        interpretation (see _create_ipl_inst_file_panel's docstring) -
        never touches the actual .ipl file."""
        self._refresh_ipl_inst_file_panel()

    def _on_tobj_time_toggled(self, checked): #vers 2
        """Time switch for TOBJ objects - re-applies the visibility
        filter so the new on/off state takes effect immediately.
        clear_display_lists=False (Aug 1 2026) - toggling the switch
        only changes which instances are visible, not the underlying
        model set, so there's nothing to recompile."""
        self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=False)

    def _on_tobj_time_changed(self, qtime): #vers 4
        """Simulated time-of-day edit changed - only matters while the
        Time switch is on, but re-applying is cheap and avoids a
        stale filter if the switch gets enabled right after.
        clear_display_lists=False (Aug 1 2026) - same reasoning as
        _on_tobj_time_toggled.

        Also syncs timecyc's own hour to this same real, existing
        game-time state (Aug 20 2026,  "there appears to be
        another timer running besides the tojb timer") - timecyc used
        to run its own separate QTimer/hour counter, a second,
        redundant "time of day" clock alongside this real, already-
        existing one. Fires on every real change to this spin box
        (QTimeEdit's own timeChanged signal, already connected here -
        covers both a manual edit and _on_time_flow_tick's own
        automatic advance, since that just calls setTime() on this
        same widget too), so timecyc now genuinely tracks the same
        simulated hour TObj filtering already uses, not a second,
        separate one running independently."""
        if getattr(self, '_tobj_time_chk', None) and self._tobj_time_chk.isChecked():
            self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=False)
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_timecyc_hour'):
            vp.set_timecyc_hour(qtime.hour() + qtime.minute() / 60.0)
        # CRT overlay text (Aug 20 2026,  "[TIME] showing
        # the time in the viewpoint like old style green CRT") - same
        # real single hook every other time change already flows
        # through (manual edit or _on_time_flow_tick's own automatic
        # advance), so the on-viewport display always matches.
        if vp is not None and hasattr(vp, 'set_crt_time_text'):
            vp.set_crt_time_text(qtime.toString("HH:mm"))

    def _on_2dfx_master_toggled(self, checked): #vers 2
        """2DFX master on/off switch changed - re-runs the same
        visibility pipeline so lights are added/cleared immediately.
        clear_display_lists=False (Aug 1 2026) - this only affects
        2DFX light points, never the model geometry itself."""
        self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=False)

    def _on_lod_test_toggled(self, checked): #vers 1
        """LOD Test mode on/off."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None:
            return
        if checked:
            vp._lod_test_radius = self.map_settings.get('lod_test_circle_radius')
            vp.set_lod_test_callback(self._on_lod_test_mouse_moved)
        else:
            vp.set_lod_test_callback(None)
            vp.set_lod_test_center(None)
            self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=False)

    def _on_lod_test_mouse_moved(self, viewport_ground_pos): #vers 1
        """Called on every mouse move while LOD Test mode is active,
        with the ground position (in the viewport's own Y-up local
        space) under the current cursor. Re-filters the currently
        visible instances by live distance from that point: an
        instance with a resolved LOD pair switches between its normal
        and LOD version depending on whether it's inside or outside
        the circle; a standalone LOD-type instance (draw-distance or
        name based, no paired normal-detail counterpart to switch to)
        only shows when outside; everything else is unaffected."""

        # Y-up viewport local space -> GTA's native Z-up (x,y,z) -
        # same swap _draw_cull_boxes documents in reverse (gx,gy,gz =
        # cx,cz,cy for GTA->viewport; here going the other way).
        vx, vy, vz = viewport_ground_pos
        center_x, center_y, center_z = vx, vz, vy

        all_inst = getattr(self, '_all_instances', None)
        if all_inst is None:
            return
        hidden = getattr(self, '_hidden_ipls', set())
        visible = [i for i in all_inst if i.source_ipl not in hidden] if hidden else all_inst
        pairs = getattr(self, '_lod_pairs', None) or {}
        paired_target_ids = {id(v) for v in pairs.values()}
        threshold = self.map_settings.get('lod_draw_dist_threshold')

        # Bidirectional, based on the current global mode (Aug 1 2026)
        global_mode = getattr(self, '_lod_display_mode', 'normal')
        inside_shows_lod = (global_mode == 'normal')

        result = []
        for inst in visible:
            iid = id(inst)
            if iid in paired_target_ids:
                continue   # only reached via its primary below, avoids duplicates
            dx = inst.pos_x - center_x
            dy = inst.pos_y - center_y
            dz = inst.pos_z - center_z
            dist = (dx * dx + dy * dy + dz * dz) ** 0.5
            inside = dist <= threshold
            if global_mode == 'both':
                if iid in pairs:
                    result.append(inst)
                    result.append(pairs[iid])
                else:
                    result.append(inst)
                continue
            switch = inside if inside_shows_lod else not inside
            if iid in pairs:
                result.append(pairs[iid] if switch else inst)
            elif self._is_lod_by_draw_distance(inst):
                # Standalone LOD-type instance, no paired normal-

                if switch:
                    result.append(inst)
            else:
                # Standalone normal-type instance, no paired LOD
                result.append(inst)

        self._refresh_world_view(result, auto_fit=False, clear_display_lists=False)

    def _start_time_flow(self, play_btn, stop_btn): #vers 1
        """Start the time-flow timer."""
        if not self._tobj_time_chk.isChecked():
            self._tobj_time_chk.setChecked(True)
        self._time_flow_timer.start(self._time_flow_interval_ms)
        play_btn.setEnabled(False)
        stop_btn.setEnabled(True)

    def _stop_time_flow(self, play_btn, stop_btn): #vers 1
        """Stop the time-flow timer - time stays at whatever it
        reached, doesn't reset."""
        self._time_flow_timer.stop()
        play_btn.setEnabled(True)
        stop_btn.setEnabled(False)

    def _on_time_flow_tick(self): #vers 2
        """One time-flow timer tick."""
        current = self._tobj_time_spin.time()
        new_time = current.addSecs(self._time_flow_minutes_per_tick * 60)
        self._tobj_time_spin.setTime(new_time)
        self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=False)

    def _on_crt_time_clicked(self): #vers 1
        """Left-click on the on-viewport CRT time overlay (Aug 20
        2026,  "click on time for stop and start") - toggles
        the same real time-flow timer the old, separate Play/Stop
        buttons already controlled, just from one, unified on-viewport
        control instead. Those old buttons still exist (just no
        longer shown in IPL Controls) purely so _start_time_flow/
        _stop_time_flow's own existing real enable/disable-a-button
        signature doesn't need to change."""
        play_btn = getattr(self, '_time_play_btn', None)
        stop_btn = getattr(self, '_time_stop_btn', None)
        if play_btn is None or stop_btn is None:
            return
        if self._time_flow_timer.isActive():
            self._stop_time_flow(play_btn, stop_btn)
        else:
            self._start_time_flow(play_btn, stop_btn)

    def _show_time_flow_settings_popup(self): #vers 1
        """Time flow speed settings."""
        dlg = QDialog(self)
        dlg.setWindowTitle("Time Flow Settings")
        lay = QVBoxLayout(dlg)

        lay.addWidget(QLabel("In-game minutes per tick"))
        minutes_spin = QSpinBox()
        minutes_spin.setRange(1, 120)
        minutes_spin.setValue(self._time_flow_minutes_per_tick)
        minutes_spin.setFixedHeight(24)
        def on_minutes_change(v):
            self._time_flow_minutes_per_tick = v
        minutes_spin.valueChanged.connect(on_minutes_change)
        lay.addWidget(minutes_spin)

        lay.addWidget(QLabel(" seconds per tick"))
        interval_spin = QDoubleSpinBox()
        interval_spin.setRange(0.1, 60.0)
        interval_spin.setSingleStep(0.5)
        interval_spin.setDecimals(1)
        interval_spin.setValue(self._time_flow_interval_ms / 1000)
        interval_spin.setFixedHeight(24)
        def on_interval_change(v):
            self._time_flow_interval_ms = int(v * 1000)
            if self._time_flow_timer.isActive():
                self._time_flow_timer.start(self._time_flow_interval_ms)
        interval_spin.valueChanged.connect(on_interval_change)
        lay.addWidget(interval_spin)

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dlg.accept)
        lay.addWidget(close_btn)
        dlg.exec()

    def _apply_ipl_visibility_filter(self, auto_fit=True, clear_display_lists=True): #vers 6
        """Recompute which instances are currently visible: every
        loaded instance whose source_ipl isn't in self._hidden_ipls,
        then layer LOD show/hide on top (global toggle + any per-
        instance overrides) - push the final result to the world-view
        panes and Instance List.

        Reentrancy guard, corrected (Aug 16 2026)

        clear_display_lists=False (Aug 1 2026)"""
        if getattr(self, '_apply_ipl_visibility_filter_in_progress', False):
            self._apply_ipl_visibility_filter_pending = (auto_fit, clear_display_lists)
            return
        self._apply_ipl_visibility_filter_in_progress = True
        try:
            self._apply_ipl_visibility_filter_impl(auto_fit, clear_display_lists)
            while True:
                pending = getattr(self, '_apply_ipl_visibility_filter_pending', None)
                if pending is None:
                    break
                self._apply_ipl_visibility_filter_pending = None
                self._apply_ipl_visibility_filter_impl(*pending)
        finally:
            self._apply_ipl_visibility_filter_in_progress = False

    def _apply_ipl_visibility_filter_impl(self, auto_fit, clear_display_lists): #vers 1
        """The actual body of _apply_ipl_visibility_filter, split out
        only so the reentrancy guard above can wrap it in a try/
        finally without a second level of indentation - not meant to
        be called directly by anything else; call _apply_ipl_
        visibility_filter instead."""
        all_inst = getattr(self, '_all_instances', None)
        if all_inst is None:
            return
        hidden = getattr(self, '_hidden_ipls', set())
        visible = [i for i in all_inst if i.source_ipl not in hidden] if hidden else all_inst
        visible = self._apply_lod_filter(visible)
        visible = self._apply_tobj_time_filter(visible)
        visible = self._apply_interior_filter(visible)
        for pane in getattr(self, '_world_panes', []):
            pane.set_instances(visible)
        loader_stub = _FilteredLoaderStub(visible, getattr(self, '_world_loader', None))
        self._populate_instance_list(loader_stub)
        self._refresh_world_view(visible, auto_fit=auto_fit, clear_display_lists=clear_display_lists)
        self._refresh_2dfx_lights(visible)
        self._refresh_path_visualization()
        self._refresh_cull_box_visualization()
        self._refresh_zone_box_visualization()
        self._refresh_occl_box_visualization()
        self._refresh_auzo_visualization()
        self._refresh_water_visualization()

    def _refresh_2dfx_lights(self, visible_instances): #vers 1
        """Collect 2DFX light-type (effect_type == 0) entries for the
        given visible instances, compute each one's world-space
        position, and push them to the viewport."

        "At night" (Aug 1 2026)"""
        vp = getattr(self, 'preview_widget', None)
        loader = getattr(self, '_world_loader', None)
        if vp is None or loader is None or not hasattr(vp, 'set_2dfx_lights'):
            return
        master_chk = getattr(self, '_2dfx_master_chk', None)
        if master_chk is not None and not master_chk.isChecked():
            vp.set_2dfx_lights([])
            return
        get_2dfx = getattr(loader, 'get_2dfx_for_model', None)
        if get_2dfx is None:
            vp.set_2dfx_lights([])
            return

        chk = getattr(self, '_tobj_time_chk', None)
        if chk is not None and chk.isChecked():
            hour = self._tobj_time_spin.time().hour()
            is_night = hour >= 20 or hour < 6
            if not is_night:
                vp.set_2dfx_lights([])
                return

        lights = []
        for inst in visible_instances:
            for fx in get_2dfx(inst.model_id):
                if fx.extra.get('effect_type') != 0:
                    continue
                ox = fx.extra.get('offset_x', 0.0)
                oy = fx.extra.get('offset_y', 0.0)
                oz = fx.extra.get('offset_z', 0.0)
                # Effective (conjugated) rotation, not the raw stored
                # quaternion (Aug 1 2026)
                erx, ery, erz, erw = self._effective_rotation(inst)
                wx, wy, wz = self._rotate_and_translate_offset(
                    ox, oy, oz, erx, ery, erz, erw,
                    inst.pos_x, inst.pos_y, inst.pos_z)
                r = fx.extra.get('color_r', 255)
                g = fx.extra.get('color_g', 255)
                b = fx.extra.get('color_b', 255)
                a = fx.extra.get('color_a', 255)
                size = fx.extra.get('corona_size', 1.0)
                lights.append((wx, wy, wz, r, g, b, a, size))
        vp.set_2dfx_lights(lights)

    def _refresh_path_visualization(self): #vers 2
        """Push the currently visible IPLs' path data to the
        viewport's line/node overlay (Aug 14 2026)

        Builds the REAL per-node link graph (Aug 16 2026)"""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_path_segments'):
            return
        show_paths_chk = getattr(self, '_show_paths_chk', None)
        if show_paths_chk is not None and not show_paths_chk.isChecked():
            if hasattr(vp, 'set_path_node_owners'):
                vp.set_path_node_owners({})
            vp.set_path_segments([])
            return
        # Apply the saved line colour/thickness and node colour/size
        # every refresh, not just when the Settings dialog is used to
        # change them (Aug 14/16 2026) - so a value picked in a
        # previous session is already in effect the first time paths
        # are shown this session, without needing a separate "apply
        # saved settings on startup" hook.
        if hasattr(vp, 'set_path_line_color'):
            saved_color = self.map_settings.get('path_line_color') or (255, 0, 0)
            cr, cg, cb = saved_color
            vp.set_path_line_color(cr / 255.0, cg / 255.0, cb / 255.0)
        if hasattr(vp, 'set_path_node_color'):
            saved_node_color = self.map_settings.get('path_node_color') or (255, 204, 0)
            ncr, ncg, ncb = saved_node_color
            vp.set_path_node_color(ncr / 255.0, ncg / 255.0, ncb / 255.0)
        if hasattr(vp, 'set_path_line_thickness'):
            vp.set_path_line_thickness(self.map_settings.get('path_line_thickness') or 1.2)
        if hasattr(vp, 'set_path_node_size'):
            vp.set_path_node_size(self.map_settings.get('path_node_size') or 3.5)
        if hasattr(vp, 'set_track_color'):
            tcr, tcg, tcb = self.map_settings.get('track_color') or (191, 191, 204)
            vp.set_track_color(tcr / 255.0, tcg / 255.0, tcb / 255.0)
        if hasattr(vp, 'set_track_line_thickness'):
            vp.set_track_line_thickness(self.map_settings.get('track_line_thickness') or 1.5)
        if hasattr(vp, 'set_airtrain_color'):
            acr, acg, acb = self.map_settings.get('airtrain_color') or (230, 153, 51)
            vp.set_airtrain_color(acr / 255.0, acg / 255.0, acb / 255.0)
        if hasattr(vp, 'set_airtrain_line_thickness'):
            vp.set_airtrain_line_thickness(self.map_settings.get('airtrain_line_thickness') or 1.2)
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            if hasattr(vp, 'set_path_node_owners'):
                vp.set_path_node_owners({})
            vp.set_path_segments([])
            return
        hidden = getattr(self, '_hidden_ipls', set())
        visible_groups = [g for g in getattr(loader, 'paths', [])
                           if g.source_ipl not in hidden and g.nodes]

        segments = []
        # (position -> (group, node_index)) for interactive path node
        # dragging (Aug 17 2026)
        node_owners = {}
        for group in visible_groups:
            nodes = group.nodes
            n = len(nodes)
            for i, node in enumerate(nodes):
                if node.node_type == 0:
                    continue   # Null node - ignored, per the format doc
                node_owners[(node.x, node.y, node.z)] = (group, i)
                if 0 <= node.next_id < n:
                    other = nodes[node.next_id]
                    segments.append(((node.x, node.y, node.z), (other.x, other.y, other.z)))

        # GTA III IDE-embedded paths (Aug 16 2026)

        instances_by_model = {}
        for inst in getattr(loader, 'instances', []):
            if inst.source_ipl in hidden:
                continue
            instances_by_model.setdefault(inst.model_id, []).append(inst)

        # Scope each group's model_id lookup to every currently-
        # visible instance sharing that model_id, globally - no per-
        # area scoping (Aug 16 2026)
        for group in getattr(loader, 'ide_paths', []):
            matching_instances = instances_by_model.get(group.model_id)
            if not matching_instances:
                continue
            nodes = group.nodes
            n = len(nodes)
            for inst in matching_instances:
                ex, ey, ez, ew = self._effective_rotation(inst)
                world_pos = []
                for node in nodes:
                    # /16 scale on the relative offsets (Aug 16 2026)

                    wx, wy, wz = self._rotate_vector_by_quaternion(
                        node.x_rel / 16.0, node.y_rel / 16.0, node.z_rel / 16.0,
                        ex, ey, ez, ew)
                    world_pos.append((inst.pos_x + wx, inst.pos_y + wy, inst.pos_z + wz))
                for i, node in enumerate(nodes):
                    if node.node_type == 0:
                        continue
                    if 0 <= node.next_id < n:
                        segments.append((world_pos[i], world_pos[node.next_id]))

        if hasattr(vp, 'set_path_node_owners'):
            vp.set_path_node_owners(node_owners)
        vp.set_path_segments(segments)

    def _on_path_node_dragged(self, group, node_index, new_x, new_y, new_z): #vers 1
        """Commit a completed path node drag to the real, live
        PathNode data (Aug 17 2026)"""
        try:
            node = group.nodes[node_index]
        except (IndexError, AttributeError):
            return
        node.x, node.y, node.z = new_x, new_y, new_z
        self._refresh_path_visualization()
        self._set_status(f"Moved path node to ({new_x:.1f}, {new_y:.1f}, {new_z:.1f})")

    def _on_box_resized(self, box_type, box_ref, x1, y1, x2, y2, z1, z2): #vers 1
        """Commit a completed box-corner resize drag to the real,
        live CullEntry or zone dict (Aug 19 2026)"""
        if box_type == 'cull':
            box_ref.x1, box_ref.y1, box_ref.z1 = x1, y1, z1
            box_ref.x2, box_ref.y2, box_ref.z2 = x2, y2, z2
            self._refresh_cull_box_visualization()
        elif box_type == 'zone':
            box_ref['min_x'], box_ref['min_y'], box_ref['min_z'] = x1, y1, z1
            box_ref['max_x'], box_ref['max_y'], box_ref['max_z'] = x2, y2, z2
            self._refresh_zone_box_visualization()
        else:
            return
        self._set_status(
            f"Resized {box_type} box to "
            f"({x1:.1f}, {y1:.1f}, {z1:.1f}) - ({x2:.1f}, {y2:.1f}, {z2:.1f})")

    def _on_edit_paths_toggled(self, checked): #vers 2
        """Right-click on the Paths button toggled edit mode (Aug 18
        2026 - merged from the previous separate Edit Paths checkbox
        into the Paths button's own right-click."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_path_edit_mode'):
            vp.set_path_edit_mode(checked)
        if checked:
            show_paths_chk = getattr(self, '_show_paths_chk', None)
            if show_paths_chk is not None and not show_paths_chk.isChecked():
                show_paths_chk.set_shown(True)   # triggers _on_show_paths_toggled itself

    def _on_edit_boxes_toggled(self, checked): #vers 2
        """Right-click on a box-type overlay button (Cull/Zon/Grge)
        toggled box-corner resize edit mode (Aug 19 2026).

         fix (Aug 21 2026,  "Bug: Right radar also
        highlights cull; both buttons seem to be linked when you
        right-click them") - used to force-sync every other box-type
        button's own visual state (editing dashes + shown) to match
        whichever one was actually right-clicked, including force-
        turning a box type ON that   may never have wanted shown
        at all. That syncing was left over from when self._box_edit_
        mode genuinely gated corner-picking on the viewport side - it
        no longer does (removed turns ago, corner-clicking works
        unconditionally now), so self._box_edit_mode itself is now
        write-only/inert on the viewport side; nothing there reads it
        back. Each button already updates its own real visual state
        directly in _MapOverlayToggleButton.mousePressEvent - this
        handler no longer needs to touch any other button at all."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_box_edit_mode'):
            vp.set_box_edit_mode(checked)

    def _refresh_track_visualization(self): #vers 1
        """Push loaded train track waypoints to the viewport's own
        polyline overlay (Aug 17 2026)"""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_track_polylines'):
            return
        show_tracks_chk = getattr(self, '_show_tracks_chk', None)
        if show_tracks_chk is not None and not show_tracks_chk.isChecked():
            vp.set_track_polylines([])
            return
        if hasattr(vp, 'set_track_color'):
            saved_color = self.map_settings.get('track_color') or (191, 191, 204)
            tcr, tcg, tcb = saved_color
            vp.set_track_color(tcr / 255.0, tcg / 255.0, tcb / 255.0)
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            vp.set_track_polylines([])
            return
        polylines = [[(wp.x, wp.y, wp.z) for wp in waypoints]
                    for waypoints in getattr(loader, 'tracks', {}).values()]
        vp.set_track_polylines(polylines)

    def _refresh_sa_node_visualization(self): #vers 1
        """Push SA's real vehicle/ped path node graph to the
        viewport's own segment overlay (Aug 19 2026)"""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_sa_node_segments'):
            return
        show_chk = getattr(self, '_show_sa_nodes_chk', None)
        if show_chk is not None and not show_chk.isChecked():
            vp.set_sa_node_segments([])
            return
        loader = getattr(self, '_world_loader', None)
        sa_nodes = getattr(loader, 'sa_nodes', None) if loader is not None else None
        if not sa_nodes:
            vp.set_sa_node_segments([])
            return

        segments = []
        for area_id, path_file in sa_nodes.items():
            for is_ped, node_list in ((False, path_file.vehicle_nodes),
                                      (True, path_file.ped_nodes)):
                for node in node_list:
                    key = (area_id, node.node_id, is_ped)
                    for li in range(node.link_id, node.link_id + node.link_count):
                        if li < 0 or li >= len(path_file.links):
                            continue
                        link = path_file.links[li]
                        target_key = (link.area_id, link.node_id, is_ped)
                        if target_key <= key:
                            continue   # already added from the other end, or self-referential
                        target_file = sa_nodes.get(link.area_id)
                        if target_file is None:
                            continue
                        target_list = target_file.ped_nodes if is_ped else target_file.vehicle_nodes

                        target_id = link.node_id
                        if is_ped:
                            target_id -= len(target_file.vehicle_nodes)
                        if not (0 <= target_id < len(target_list)):
                            continue
                        target_node = target_list[target_id]
                        segments.append(((node.x, node.y, node.z),
                                        (target_node.x, target_node.y, target_node.z)))
        vp.set_sa_node_segments(segments)

    def _refresh_auzo_visualization(self): #vers 2
        """Push loaded SA audio zones to the viewport's own sound-
        icon overlay (Aug 20 2026).

         bugs fixed here (Aug 20 2026,  "clicking on the
        Auzo button still doesn't show the contents on the auzo file,
        from auzo entry" - and his own real, working comparison to
        paths/zon/cull/occl: "clicked on first, then [Paths] button to
        show"):

        1. This method was never actually called from _apply_ipl_
           visibility_filter, the one, central place every other real
           overlay (_refresh_path_visualization/_refresh_cull_box_
           visualization/_refresh_zone_box_visualization/_refresh_
           occl_box_visualization, all four called there already)
           refreshes from whenever IPL visibility changes - i.e.
           exactly the "click the IPL entry" half of the workflow
             describes. Auzo only ever refreshed from its own
           toggle button's own change handler, so clicking a new
           auzo-containing IPL entry never actually pushed anything
           new to the viewport unless the toggle itself happened to
           be flipped at that exact moment too. Now called from that
           same central method (see _apply_ipl_visibility_filter's
           own real call list).

        2. Never filtered by which IPLs are actually currently
           visible/hidden at all (unlike every other overlay here,
           e.g. _refresh_occl_box_visualization's own real
           `if o.source_ipl not in hidden` check) - read every real
           auzo entry from every loaded IPL unconditionally. Now
           filters the same way."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_auzo_zones'):
            return
        show_chk = getattr(self, '_show_auzo_chk', None)
        if show_chk is not None and not show_chk.isChecked():
            vp.set_auzo_zones([])
            return
        loader = getattr(self, '_world_loader', None)
        auzos = getattr(loader, 'auzos', None) if loader is not None else None
        if not auzos:
            vp.set_auzo_zones([])
            return
        hidden = getattr(self, '_hidden_ipls', set())
        zones = []
        for a in auzos:
            if a.source_ipl in hidden:
                continue
            if a.is_sphere:
                cx, cy, cz = a.x1, a.y1, a.z1
            else:
                cx = (a.x1 + a.x2) / 2.0
                cy = (a.y1 + a.y2) / 2.0
                cz = (a.z1 + a.z2) / 2.0
            zones.append((cx, cy, cz, a.name, a.sound_id, a.environment_type, a.music_description))
        vp.set_auzo_zones(zones)

    def _refresh_water_visualization(self): #vers 2
        """Push loaded water.dat shapes AND waterpro.dat's own real
        grid to the viewport's own overlay (Aug 20 2026).

         fixes here, .per:

        1. "besides the button on the IPL control that doesnt seem to
           work" - this was never called from _apply_ipl_visibility_
           filter, the one, central place every other real overlay
           (path/cull/zone/occl/auzo, all now called there) refreshes
           from whenever IPL visibility changes. Now called there too
           (see that method's own real call list).

        2. "water function should also load the waterpro.dat, and
           display it in the same way water_workshop works" -
           waterpro.dat was already being loaded (load_waterpro_dat,
           storing into loader.waterpro) but nothing ever read it back
           out anywhere - a real, confirmed gap, not a guess. Real
           world bounds/cell size for its own grid come from the same
           RADAR_GRID_PRESETS/compute_radar_grid this app's own radar
           tex layer already uses (waterpro's own grid covers the
           same real map area, just at grid_width resolution instead
           of tiles_per_side) - not a second, separate bounds source
           invented for this."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_water_shapes'):
            return
        show_chk = getattr(self, '_show_water_chk', None)
        if show_chk is not None and not show_chk.isChecked():
            vp.set_water_shapes([])
            if hasattr(vp, 'set_waterpro_cells'):
                vp.set_waterpro_cells([])
            return
        loader = getattr(self, '_world_loader', None)
        water_shapes = getattr(loader, 'water_shapes', None) if loader is not None else None
        if not water_shapes:
            vp.set_water_shapes([])
        else:
            shapes = [
                ([(c.x, c.y, c.z) for c in w.corners], w.water_type)
                for w in water_shapes
            ]
            vp.set_water_shapes(shapes)

        if hasattr(vp, 'set_waterpro_cells'):
            waterpro = getattr(loader, 'waterpro', None) if loader is not None else None
            if not waterpro:
                vp.set_waterpro_cells([])
            else:
                from apps.methods.gta_dat_parser import RADAR_GRID_PRESETS
                game_key = getattr(loader, 'game', 'vc')
                preset = RADAR_GRID_PRESETS.get(game_key, RADAR_GRID_PRESETS['vc'])
                grid_size = preset['grid_size']
                half = grid_size / 2.0
                gw = waterpro.grid_width
                cell = grid_size / gw
                cells = []
                for row in range(gw):
                    for col in range(gw):
                        level_idx = waterpro.visible_map[row][col]
                        # Real, confirmed "cutout" sentinel (Aug 20
                        # 2026,  "waterpro.dat allowing cut
                        # out areas, making sure waterpro.dat is used
                        # is very important, look at water_workshop
                        # as resource") - water_workshop.py's own
                        # WaterGridWidget._cell_col checks exactly
                        # this: val == 128 means dry/land, no water
                        # at all at this cell, any other real value
                        # means water there. This was the real root
                        # cause of water appearing to cover the whole
                        # map regardless of the real land boundary -
                        # every dry cell was still being drawn as a
                        # flat water quad, since the only check here
                        # before was an out-of-range level index, and
                        # most real dry cells still carry an in-range
                        # index (just one that should have been
                        # skipped for a different reason).
                        if level_idx == 128:
                            continue
                        if level_idx < 0 or level_idx >= len(waterpro.levels):
                            continue
                        height = waterpro.levels[level_idx].height
                        # row 0 = north (max_y), per water_workshop.py's
                        # own real "Map screen position to data (col,
                        # row). No Y-flip." - row 0 is screen-top there,
                        # and screen-top = north in a normal, un-
                        # flipped top-down map view.
                        min_x = -half + col * cell
                        min_y = half - (row + 1) * cell
                        cells.append((min_x, min_y, min_x + cell, min_y + cell, height))
                vp.set_waterpro_cells(cells)


    def _on_show_tracks_toggled(self, checked): #vers 1
        """Show Tracks checked/unchecked."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_show_tracks'):
            vp.set_show_tracks(checked)
        if checked:
            self._refresh_track_visualization()

    def _on_show_sa_nodes_toggled(self, checked): #vers 1
        """Show SA Nodes checked/unchecked."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_show_sa_nodes'):
            vp.set_show_sa_nodes(checked)
        if checked:
            self._refresh_sa_node_visualization()

    def _on_show_auzo_toggled(self, checked): #vers 1
        """Show Auzo checked/unchecked."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_show_auzo_zones'):
            vp.set_show_auzo_zones(checked)
        if checked:
            self._refresh_auzo_visualization()

    def _on_interior_show_all_toggled(self, checked): #vers 1
        """Interior button left-click (Aug 20 2026,  "show 0
        only on the viewpoint; showing 0 to 14+ by selecting them") -
        checked shows every interior value together (no filtering);
        unchecked returns to the default, interior 0 (exterior) only."""
        self._interior_filter_value = None if checked else 0
        self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=False)
        if checked:
            self._set_status("Showing all interiors together")
        else:
            self._set_status("Showing interior 0 (exterior) only")

    def _show_interior_picker_menu(self, interior_btn): #vers 3
        """Interior button right-click (Aug 20 2026  own
        follow-up: "I have no idea how many there are for GTA 3, GTA
        VC, or SA") - lists every interior value actually present in
        the currently loaded instances, with counts, so   can see
        exactly what a given world uses rather than guessing, and
        pick one to isolate.

         names shown for VC (Aug 20 2026  own real,
        direct list of VC interior areas, cross-confirmed against
        GTAMods' own documented "Interior" page - see VC_INTERIOR_
        NAMES' own docstring in gta_dat_parser.py for the full, real
        confirmation story).

         fix for SA (Aug 20 2026,  "full list for VC,
        now im looking for SA") - unlike VC, an SA interior number
        does NOT uniquely identify one real area on its own (GTAMods'
        own documented SA list shows many unrelated real buildings
        sharing the same real number), so the filter still works by
        interior number as before, but each number's own label now
        also names the real, distinct interior *files* (source_ipl)
        actually loaded at that number, resolved via SA_INTERIOR_
        FILE_NAMES - accurate because a real interior file, unlike a
        bare number, does uniquely identify one real area."""
        from PyQt6.QtWidgets import QMenu
        from apps.methods.gta_dat_parser import GTAGame, VC_INTERIOR_NAMES, SA_INTERIOR_FILE_NAMES
        all_inst = getattr(self, '_all_instances', [])
        counts = {}
        source_names = {}   # interior value -> set of resolved real area names
        for inst in all_inst:
            counts[inst.interior] = counts.get(inst.interior, 0) + 1
            base = os.path.splitext(os.path.basename(inst.source_ipl or ''))[0].lower()
            real_name = SA_INTERIOR_FILE_NAMES.get(base)
            if real_name:
                source_names.setdefault(inst.interior, set()).add(real_name)
        loader = getattr(self, '_world_loader', None)
        game = getattr(loader, 'game', None)
        is_vc = game == GTAGame.VC
        is_sa = game in (GTAGame.SA, GTAGame.SOL)
        menu = QMenu(self)
        if not counts:
            act = menu.addAction("No world loaded")
            act.setEnabled(False)
        else:
            current = getattr(self, '_interior_filter_value', 0)
            all_act = menu.addAction(f"All interiors together ({len(all_inst)} total)")
            all_act.setCheckable(True)
            all_act.setChecked(current is None)
            all_act.triggered.connect(lambda checked=False: self._set_interior_filter(None, interior_btn))
            menu.addSeparator()
            for value in sorted(counts):
                if is_vc and value in VC_INTERIOR_NAMES:
                    label = f"{value} - {VC_INTERIOR_NAMES[value]} ({counts[value]})"
                else:
                    label = f"Interior {value} ({counts[value]})"
                    if value == 0:
                        label = f"Interior 0 - exterior ({counts[value]})"
                    elif value == 13:
                        label = f"Interior 13 - pickups ({counts[value]})"
                    if is_sa and value in source_names:
                        names = sorted(source_names[value])
                        shown = ', '.join(names[:3])
                        if len(names) > 3:
                            shown += f", +{len(names) - 3} more"
                        label += f" - {shown}"
                act = menu.addAction(label)
                act.setCheckable(True)
                act.setChecked(current == value)
                act.triggered.connect(
                    lambda checked=False, v=value: self._set_interior_filter(v, interior_btn))
        menu.exec(interior_btn.mapToGlobal(interior_btn.rect().bottomLeft()))

    def _set_interior_filter(self, value, interior_btn): #vers 1
        """Apply a specific interior filter chosen from the picker
        menu above (Aug 20 2026), and keep the button's own checked
        state in sync (checked = showing all, matching left-click's
        own real meaning) without re-emitting show_toggled."""
        self._interior_filter_value = value
        interior_btn.set_shown(value is None, emit=False)
        self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=False)
        if value is None:
            self._set_status("Showing all interiors together")
        else:
            self._set_status(f"Showing interior {value} only")

    def _on_show_water_toggled(self, checked): #vers 4
        """Show Water checked/unchecked.

        Re-applied (Aug 20 2026,  "get the water working,
        from the preloaded file, [water] button off/on toggle") -
        real data now comes only from the Preload dialog (or the
        auto-retooled loader.waterpro/water_shapes data), which
        already pushed it to the viewport and turned this button on
        the moment it was preloaded - so this is just the plain on/
        off toggle again, no separate refresh call needed."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_show_water'):
            vp.set_show_water(checked)
        if getattr(self, '_vp_gauges', None) is not None:
            self._vp_gauges.refresh()

    def _on_water_layer_toggled(self, checked): #vers 2
        """Middle-click switches between waterpro.dat's own two real
        layers (Sep 5 2026,  "the right click toggle
        between vis_water and phy_water needs to be moved to the
        middle button so it doesnt clash with the right click menu" -
        moved off right-click/edit_toggled, which also freed up
        right-click for its own real context menu) - visible_map
        (checked=False, the default/first layer) vs physical_map
        (checked=True, double the resolution). Only meaningful once a
        real waterpro.dat has actually been preloaded (water.dat has
        no second layer at all); silently does nothing otherwise,
        since there's nothing real to switch to. Tracks its own
        current state (self._water2_showing_physical) now that the
        button itself no longer does, since middle_clicked carries no
        boolean the way edit_toggled used to."""
        waterpro = getattr(self, '_water2_waterpro_source', None)
        if waterpro is None:
            return
        game = getattr(self, '_water2_game_source', 'vc')
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_water2_data'):
            return
        self._water2_showing_physical = checked
        if checked:
            cells = self._waterpro_physical_to_cells(waterpro, game)
            self._set_status("Water: showing physical_map layer (double resolution)")
        else:
            cells = self._waterpro_to_cells(waterpro, game)
            self._set_status("Water: showing visible_map layer")
        self._water2_cells_pending = cells
        vp.set_water2_data(cells)

    def _on_show_paths_toggled(self, checked): #vers 1
        """Show Paths checked/unchecked"""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_show_paths'):
            vp.set_show_paths(checked)
        if checked:
            self._refresh_path_visualization()

    def _refresh_cull_box_visualization(self): #vers 1
        """Push the currently visible IPLs' cull zones to the
        viewport's wireframe-box overlay (Aug 16 2026)"""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_cull_boxes'):
            return
        show_cull_chk = getattr(self, '_show_cull_chk', None)
        if show_cull_chk is not None and not show_cull_chk.isChecked():
            vp.set_cull_boxes([])
            return
        if hasattr(vp, 'set_cull_box_color'):
            saved_color = self.map_settings.get('cull_box_color') or (255, 217, 51)
            cr, cg, cb = saved_color
            vp.set_cull_box_color(cr / 255.0, cg / 255.0, cb / 255.0)
        if hasattr(vp, 'set_box_axis_colors'):
            vp.set_box_axis_colors(bool(self.map_settings.get('box_axis_colors')))
        if hasattr(vp, 'set_box_unique_colors'):
            vp.set_box_unique_colors(bool(self.map_settings.get('box_unique_colors')))
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            vp.set_cull_boxes([])
            return
        hidden = getattr(self, '_hidden_ipls', set())
        culls = [c for c in getattr(loader, 'culls', []) if c.source_ipl not in hidden]
        boxes = [(c.x1, c.y1, c.z1, c.x2, c.y2, c.z2) for c in culls]
        vp.set_cull_boxes(boxes)
        if hasattr(vp, 'set_cull_box_owners'):
            vp.set_cull_box_owners(culls)

    def _on_show_cull_boxes_toggled(self, checked): #vers 1
        """Show Cull Zones checked/unchecked."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_show_cull_boxes'):
            vp.set_show_cull_boxes(checked)
        if checked:
            self._refresh_cull_box_visualization()

    def _refresh_zone_box_visualization(self): #vers 1
        """Push the currently visible IPLs' map zones to the
        viewport's wireframe-box overlay (Aug 16 2026)"""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_zone_boxes'):
            return
        show_zone_chk = getattr(self, '_show_zone_chk', None)
        if show_zone_chk is not None and not show_zone_chk.isChecked():
            vp.set_zone_boxes([])
            return
        if hasattr(vp, 'set_zone_box_color'):
            saved_color = self.map_settings.get('zone_box_color') or (77, 179, 255)
            cr, cg, cb = saved_color
            vp.set_zone_box_color(cr / 255.0, cg / 255.0, cb / 255.0)
        if hasattr(vp, 'set_box_axis_colors'):
            vp.set_box_axis_colors(bool(self.map_settings.get('box_axis_colors')))
        if hasattr(vp, 'set_box_unique_colors'):
            vp.set_box_unique_colors(bool(self.map_settings.get('box_unique_colors')))
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            vp.set_zone_boxes([])
            return
        hidden = getattr(self, '_hidden_ipls', set())
        zones = [z for z in getattr(loader, 'zones', []) if z.get('source_ipl') not in hidden]
        boxes = [(z['min_x'], z['min_y'], z['min_z'], z['max_x'], z['max_y'], z['max_z'])
                for z in zones]
        vp.set_zone_boxes(boxes)
        if hasattr(vp, 'set_zone_box_owners'):
            vp.set_zone_box_owners(zones)

    def _resolve_ipl_abs_path(self, source_ipl_basename): #vers 1
        """Find the real, full, absolute path a loaded IPL's own
        basename (source_ipl, e.g. "map.zon") corresponds to on disk
        (Aug 21 2026,  "we need to finish the add, del,
        save functions for zon") - searches loader.load_log (a real
        list of (phase, "IPL", abs_path, ok) tuples every real loaded
        IPL is already recorded in, for on-demand and eager loading
        alike), matching on the real basename case-insensitively
        since GTA file references vary in case across platforms.
        Returns None if no real, loaded IPL matches."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return None
        target = source_ipl_basename.lower()
        for entry in getattr(loader, 'load_log', []):
            if len(entry) >= 3 and os.path.basename(entry[2]).lower() == target:
                return entry[2]
        return None

    def _write_back_zone_section(self, abs_path, zone_entries): #vers 1
        """Write a real "zone...end" section back to its own real IPL
        file on disk, replacing only that section's own real content -
        every other real line (other sections, comments, blank lines)
        is left completely untouched (Aug 21 2026,  "we
        need to finish the add, del, save functions for zon").

         safety: writes a real ".bak" backup of the file's own
        real, current content first (only if one doesn't already
        exist, so repeated saves in one real session don't overwrite
        an earlier, real pre-edit backup with an already-edited one).
        Section boundaries found the same real way IPLParser.parse
        itself already identifies them (a line matching a bare
        section keyword, "end" closing it) so this stays in sync with
        how the file is actually read back in. If no real "zone"
        section exists in the file at all yet, a new one is appended
        at the real end of the file instead of failing.

        Returns (success: bool, message: str)."""
        try:
            with open(abs_path, 'r', encoding='latin-1', newline='') as f:
                lines = f.readlines()
        except Exception as e:
            return False, f"Couldn't read {abs_path}: {e}"

        section_start = section_end = None
        for i, raw in enumerate(lines):
            stripped = raw.split('#')[0].strip()
            if stripped.lower() == 'zone':
                section_start = i
            elif section_start is not None and section_end is None and stripped.lower() == 'end':
                section_end = i
                break

        new_lines = []
        for z in zone_entries:
            fields = [z['name'], str(z.get('type', 0)),
                      f"{z['min_x']:.6f}", f"{z['min_y']:.6f}", f"{z['min_z']:.6f}",
                      f"{z['max_x']:.6f}", f"{z['max_y']:.6f}", f"{z['max_z']:.6f}",
                      str(z.get('island', 0))]
            if z.get('text_key'):
                fields.append(str(z['text_key']))
            new_lines.append(', '.join(fields) + '\n')

        if section_start is not None and section_end is not None:
            lines = lines[:section_start + 1] + new_lines + lines[section_end:]
        else:
            if lines and not lines[-1].endswith('\n'):
                lines[-1] += '\n'
            lines += ['zone\n'] + new_lines + ['end\n']

        try:
            _write_ipl_lines(abs_path, lines)      # timestamped backup + atomic write, CRLF kept
        except Exception as e:
            return False, f"Couldn't write {abs_path}: {e}"
        return True, f"Wrote {len(zone_entries)} zone(s) to {os.path.basename(abs_path)}"

    def _save_zones(self): #vers 1
        """Save Zones button - writes every currently loaded zone back
        to its own real source IPL file(s) on disk (Aug 21 2026, per
         : "we need to finish the add, del, save functions for
        zon"). Groups loader.zones by source_ipl (several files can
        each have their own real zone entries), resolves each real
        basename to its own real, full path via _resolve_ipl_abs_
        path, then calls _write_back_zone_section once per real file.
        A confirmation dialog is shown first, since this genuinely
        overwrites real files on disk (with a real .bak safety copy,
        but still a real, deliberate action, not something to fire
        without my own explicit go-ahead each time)."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            self._set_status("No world loaded")
            return
        zones = getattr(loader, 'zones', [])
        if not zones:
            self._set_status("No zones to save")
            return
        by_file = {}
        for z in zones:
            by_file.setdefault(z.get('source_ipl', ''), []).append(z)

        confirm = QMessageBox.question(
            self, "Save Zones",
            f"Write {len(zones)} zone(s) across {len(by_file)} file(s) back to "
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
            ok, msg = self._write_back_zone_section(abs_path, entries)
            if ok:
                written += 1
            else:
                failed.append(msg)

        if failed:
            self._set_status(f"Saved {written} file(s); {len(failed)} failed: {'; '.join(failed)}")
        else:
            self._set_status(f"Saved {len(zones)} zone(s) across {written} file(s)")

    def _on_show_zone_boxes_toggled(self, checked): #vers 1
        """Show Zones checked/unchecked."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_show_zone_boxes'):
            vp.set_show_zone_boxes(checked)
        if checked:
            self._refresh_zone_box_visualization()

    def _set_zone_render_style(self, mode): #vers 1
        """Zon render style changed via the Render dropdown (Aug 16
        2026)"""
        self.map_settings.set('zone_render_style', mode)
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_zone_render_style'):
            vp.set_zone_render_style(mode)

    def _refresh_occl_box_visualization(self): #vers 1
        """Push the currently visible IPLs' occlusion zones to the
        viewport's rotated wireframe-box overlay (Aug 16 2026)"""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_occl_boxes'):
            return
        show_occl_chk = getattr(self, '_show_occl_chk', None)
        if show_occl_chk is not None and not show_occl_chk.isChecked():
            vp.set_occl_boxes([])
            return
        if hasattr(vp, 'set_occl_box_color'):
            saved_color = self.map_settings.get('occl_box_color') or (255, 102, 204)
            cr, cg, cb = saved_color
            vp.set_occl_box_color(cr / 255.0, cg / 255.0, cb / 255.0)
        if hasattr(vp, 'set_box_axis_colors'):
            vp.set_box_axis_colors(bool(self.map_settings.get('box_axis_colors')))
        if hasattr(vp, 'set_box_unique_colors'):
            vp.set_box_unique_colors(bool(self.map_settings.get('box_unique_colors')))
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            vp.set_occl_boxes([])
            return
        hidden = getattr(self, '_hidden_ipls', set())
        boxes = [(o.mid_x, o.mid_y, o.bottom_z, o.width_x, o.width_y, o.height, o.rotation)
                for o in getattr(loader, 'occls', [])
                if o.source_ipl not in hidden]
        vp.set_occl_boxes(boxes)

    def _on_show_occl_boxes_toggled(self, checked): #vers 1
        """Show Occlusion checked/unchecked - continuing the cull/zon
        viewport work."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_show_occl_boxes'):
            vp.set_show_occl_boxes(checked)
        if checked:
            self._refresh_occl_box_visualization()

    def _refresh_grge_box_visualization(self): #vers 1
        """Push the currently visible IPLs' garages to the viewport's
        box overlay (Aug 21 2026,  "add support for GRGE"),
        same real pattern _refresh_occl_box_visualization uses."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_grge_boxes'):
            return
        show_grge_chk = getattr(self, '_show_grge_chk', None)
        if show_grge_chk is not None and not show_grge_chk.isChecked():
            vp.set_grge_boxes([])
            return
        if hasattr(vp, 'set_grge_box_color'):
            saved_color = self.map_settings.get('grge_box_color') or (255, 166, 0)
            cr, cg, cb = saved_color
            vp.set_grge_box_color(cr / 255.0, cg / 255.0, cb / 255.0)
        if hasattr(vp, 'set_box_axis_colors'):
            vp.set_box_axis_colors(bool(self.map_settings.get('box_axis_colors')))
        if hasattr(vp, 'set_box_unique_colors'):
            vp.set_box_unique_colors(bool(self.map_settings.get('box_unique_colors')))
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            vp.set_grge_boxes([])
            return
        hidden = getattr(self, '_hidden_ipls', set())
        grges = [g for g in getattr(loader, 'grges', []) if g.source_ipl not in hidden]
        #  fix (Aug 21 2026  own real, uploaded GRGE
        # example lines: "the grge data seems to be found in most of
        # the ipl's") - checking my own real data against it
        # revealed x2 doesn't hold a real, distinct second X corner at
        # all - it's always exactly equal to x1 in every real line
        #   posted. Confirmed via a real, independent source (a
        # SannyBuilder-community IPL format reference): the field this
        # codebase calls "front_x" is actually the box's own real
        # second X corner ("Lower Right Front"), and the field this
        # codebase calls "x2" is a real, redundant repeat of x1
        # ("Upper Left Rear" - same X as "Lower Left Front", by
        # definition of "Left"). front_x/x2's own real, raw field
        # values are kept exactly as parsed (round-trip fidelity for
        # write-back), only which one is treated as the box's own
        # true second X corner is corrected here.
        boxes = [(g.x1, g.y1, g.z1, g.front_x, g.y2, g.z2) for g in grges]
        vp.set_grge_boxes(boxes)
        if hasattr(vp, 'set_grge_box_owners'):
            vp.set_grge_box_owners(grges)

    def _on_show_grge_boxes_toggled(self, checked): #vers 1
        """Show Garages checked/unchecked - continuing the cull/zon/
        occl viewport work."""
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_show_grge_boxes'):
            vp.set_show_grge_boxes(checked)
        if checked:
            self._refresh_grge_box_visualization()

    def _on_ipl_controls_settings_clicked(self): #vers 2
        """IPL Controls' docked-only cog icon clicked (Aug 15 2026)"""
        self._show_workshop_settings()


    def _create_texture_thumbnail(self, rgba_data, width, height): #vers 1
        """Build a scaled QPixmap thumbnail from decoded RGBA8888 bytes
        - same approach as TXD Workshop's own _create_thumbnail (Aug 1 2026)"""
        try:
            if not rgba_data or width <= 0 or height <= 0:
                return None
            buf = bytes(rgba_data)
            image = QImage(buf, width, height, width * 4, QImage.Format.Format_RGBA8888)
            if image.isNull():
                return None
            image = image.copy()   # QImage doesn't own buf - copy so GC can't free it early
            pixmap = QPixmap.fromImage(image)
            thumb_size = 32
            mode = (Qt.TransformationMode.FastTransformation if width < thumb_size
                    and height < thumb_size else Qt.TransformationMode.SmoothTransformation)
            return pixmap.scaled(thumb_size, thumb_size,
                Qt.AspectRatioMode.KeepAspectRatio, mode)
        except Exception:
            return None

    def _apply_interior_filter(self, instances): #vers 1
        """Filter instances by their own interior value (Aug 20 2026,
         "Can we look at interior values? We see all models
        using the value 0. Still, interior rendering is hidden until
        you're inside buildings. We need a svg to toggle interior
        models: show 0 only on the viewpoint; showing 0 to 14+ by
        selecting them"). 0 = the exterior world (per GTAMods'
        documented "Interior" page); interior 13 is documented as
        reserved for pickups, which always stream regardless of the
        player's own interior - not specially handled here, since
        this is a viewport display filter, not in-game streaming
        logic, but worth knowing if a "13" count looks unexpectedly
        large or small.

        self._interior_filter_value: None means no filtering (show
        every interior value together); an int means show only
        instances with that exact interior value. Defaults to 0,
        matching my own stated default."""
        value = getattr(self, '_interior_filter_value', 0)
        if value is None:
            return instances
        return [i for i in instances if i.interior == value]

    def _apply_tobj_time_filter(self, instances): #vers 1
        """Filter TOBJ (timed) instances by the simulated hour
        selected in the Time switch."""
        chk = getattr(self, '_tobj_time_chk', None)
        if chk is None or not chk.isChecked():
            return instances
        loader = getattr(self, '_world_loader', None)
        get_object = getattr(loader, 'get_object', None)
        if get_object is None:
            return instances
        hour = self._tobj_time_spin.time().hour()
        result = []
        for inst in instances:
            obj = get_object(inst.model_id)
            if obj is None or obj.section != 'tobj':
                result.append(inst)
                continue
            time_on = obj.extra.get('time_on')
            time_off = obj.extra.get('time_off')
            if time_on is None or time_off is None:
                result.append(inst)   # no time data parsed - don't hide it
                continue
            if time_on <= time_off:
                visible = time_on <= hour < time_off
            else:
                visible = hour >= time_on or hour < time_off   # wraps past midnight
            if visible:
                result.append(inst)
        return result

    def _is_lod_by_draw_distance(self, inst): #vers 1
        """Standalone draw-distance-based LOD check."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return False
        obj = loader.get_object(inst.model_id)
        if obj is None:
            return False
        draw_dist = obj.extra.get('draw_dist')
        if draw_dist is None:
            return False
        threshold = self.map_settings.get('lod_draw_dist_threshold')
        return draw_dist > threshold

    def _is_lod_named(self, model_name): #vers 1
        """True if model_name is styled as a LOD version by naming
        convention - "LOD" prefix or suffix, case-insensitive (Sep 5
        2026,  "should only be those prefix or suffixed
        with LOD"). Matches GTAWorldLoader.resolve_lod_pairs' own
        Strategy 2 naming rule exactly."""
        name = (model_name or '').lower()
        return name.startswith('lod') or name.endswith('lod')

    def _apply_lod_filter(self, instances): #vers 4
        """Given an already-IPL-filtered instance list, decide for each
        LOD-paired primary instance which version(s) to keep - per-
        instance override (self._lod_overrides, keyed by id(primary
        instance), one of 'normal'/'lod'/'both'/None) takes precedence
        over the global mode (self._lod_display_mode).

        Instances with no detected LOD pair are now filtered directly
        (Sep 5 2026,  "in LOD only, I still see the normal
        models, should only be those prefix or suffixed with LOD") -
        'lod' mode now shows ONLY instances flagged as LOD, by name
        ("LOD" prefix/suffix, see _is_lod_named) or by the draw-
        distance heuristic; everything else is hidden, full stop.
        'normal' mode is the mirror - hides anything flagged either
        way. Previously an instance with no LOD signal at all fell
        through to an unconditional "always show" default in both
        modes, which is exactly what let plain normal objects with no
        LOD pair keep appearing in 'lod' mode."""
        pairs = getattr(self, '_lod_pairs', None)
        overrides = getattr(self, '_lod_overrides', {})
        global_mode = getattr(self, '_lod_display_mode', 'normal')
        paired_target_ids = {id(v) for v in pairs.values()} if pairs else set()
        result = []
        for inst in instances:
            iid = id(inst)
            if pairs and iid in pairs:
                mode = overrides.get(iid) or global_mode
                if mode == 'lod':
                    result.append(pairs[iid])
                elif mode == 'both':
                    result.append(inst)
                    result.append(pairs[iid])
                else:  # 'normal'
                    result.append(inst)
                continue
            if pairs and iid in paired_target_ids:
                # This instance IS someone's LOD target - it's only
                # included via its primary above (in 'lod'/'both' mode),
                # to avoid duplicates when both members of a pair pass
                # the IPL filter.
                continue
            if global_mode == 'both':
                result.append(inst)
                continue
            is_lod = (self._is_lod_named(inst.model_name)
                      or self._is_lod_by_draw_distance(inst))
            if global_mode == 'lod':
                if is_lod:
                    result.append(inst)
            else:  # 'normal'
                if not is_lod:
                    result.append(inst)
        return result

    def _set_lod_display_mode(self, mode): #vers 4
        """Global LOD display mode - 'normal' (default), 'lod', or
        'both'. Per-instance overrides (set via the Instance List)
        still take precedence over this for any instance they cover.

        Forces an immediate repaint (Aug 1 2026)"""
        self._lod_display_mode = mode
        self._apply_ipl_visibility_filter(auto_fit=False, clear_display_lists=False)
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'repaint'):
            vp.repaint()



    def _update_mode_button_style(self): #vers 2
        """Switch the Object Browser mode buttons (All/Most Used/
        Favourites/Generic) between icon+text and icon-only, based on
        whether the row currently has enough width to show all four
        with their text labels. Defaults to icon-only."""
        buttons = getattr(self, '_object_mode_buttons', None)
        text_widths = getattr(self, '_object_mode_button_text_widths', None)
        row_widget = getattr(self, '_ob_top_row_widget', None)
        action_widget = getattr(self, '_ob_action_row_widget', None)
        tab_buttons = getattr(self, '_object_browser_tab_buttons', None)
        if not buttons or not text_widths or row_widget is None:
            return
        text_style = Qt.ToolButtonStyle.ToolButtonTextBesideIcon
        icon_style = Qt.ToolButtonStyle.ToolButtonIconOnly

        needed = sum(text_widths.values())
        if tab_buttons:
            needed += sum(btn.sizeHint().width() for btn in tab_buttons.values())
        if action_widget is not None and action_widget.isVisible():
            needed += action_widget.sizeHint().width()
        available = row_widget.width()

        target_style = text_style if available >= needed else icon_style
        for btn in buttons.values():
            if btn.toolButtonStyle() != target_style:
                btn.setToolButtonStyle(target_style)

    def _register_collapsible_button_row(self, row_widget, button_specs): #vers 1
        """Register a row of icon+text QPushButtons (Edit/Save, Open/
        Close/New/Delete, Extract/Add/Del/Rename/Rebuild, etc.) to
        collapse to icon-only when the row doesn't have room to show
        full text, expanding back once there's space - same idea as
        Object Browser's mode-button collapse."""
        if not hasattr(self, '_collapsible_button_rows'):
            self._collapsible_button_rows = {}
        self._collapsible_button_rows[row_widget] = button_specs
        row_widget.installEventFilter(self)
        self._update_button_row_collapse(row_widget)

    def _update_button_row_collapse(self, row_widget): #vers 1
        """Toggle every button in a registered row between icon+text
        and icon-only, based on the row's current available width vs
        each button's estimated icon+text width (font metrics, not an
        actual style switch - avoids the same measure-during-resize
        flicker _update_mode_button_style already works around)."""
        specs = getattr(self, '_collapsible_button_rows', {}).get(row_widget)
        if not specs:
            return
        from PyQt6.QtGui import QFontMetrics
        needed = 0
        for btn, text in specs:
            fm = QFontMetrics(btn.font())
            needed += fm.horizontalAdvance(text) + btn.iconSize().width() + 24
        available = row_widget.width()
        show_text = available >= needed
        for btn, text in specs:
            btn.setText(text if show_text else "")
