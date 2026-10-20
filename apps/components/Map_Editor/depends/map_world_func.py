#this belongs in apps/components/Map_Editor/depends/map_world_func.py - Version: 2
# X-Seti - October 14 2026 - IMG Factory 1.6 - Map Workshop world load

"""
Game folder, DAT/IPL world load, preload, water/radar settings, world render for ModelWorkshop (Map).
"""

##class MapWorldMixin: -
# _add_recent_dat_file
# _app_asset_folder
# _apply_loaded_world
# _apply_loaded_world_impl
# _apply_radar_settings
# _apply_saved_preload_picks
# _apply_water2_preload
# _auto_detect_timecyc_path
# _auto_load_last_world
# _clear_recent_dat_files
# _conjugate_rotation_for_game
# _convert_collision_geometry
# _effective_rotation
# _game_data_folder_candidates
# _iv_static_bound_entries
# _load_game_dat_file
# _load_game_folder
# _load_models_into_dialog
# _load_models_parallel
# _load_preloaded_file
# _load_radar_settings
# _load_radar_tex_tiles
# _load_selected_ipls_with_log
# _load_water_texture_from_particle_txd
# _load_workers
# _model_collision
# _on_ipl_model_row_selected
# _populate_models_panel_from_ipl
# _preload_img_archives_to_os_cache
# _radar_align_offset
# _radar_settings_path
# _read_water_offsets
# _refresh_recent_dat_menu
# _refresh_world_view
# _refresh_world_view_impl
# _resolve_gamedata_role_marker
# _rotate_and_translate_offset
# _rotate_vector_by_quaternion
# _save_radar_settings
# _set_radar_align_offset
# _set_water_fix
# _set_water_scale
# _show_load_options_dialog
# _show_preload_dialog
# _show_radar_align_dialog
# _show_world_load_summary
# _to_gamedata_role_marker
# _try_auto_water2_from_loader
# _water2_offset_mode
# _water_scale
# _water_fix_active
# _water_offset_path
# _water_shapes_to_cells
# _waterpro_physical_to_cells
# _waterpro_to_cells
# _write_water_offsets

import json
import os

from PyQt6.QtCore import QEventLoop, Qt
from PyQt6.QtWidgets import (
    QAbstractItemView, QApplication, QCheckBox, QDialog, QGroupBox, QHBoxLayout, QLabel,
    QLineEdit, QListWidget, QListWidgetItem, QMessageBox, QPushButton, QRadioButton,
    QTableWidgetItem, QVBoxLayout)

from apps.components.Map_Editor.depends.map_widgets_func import (
    _FilteredLoaderStub, _IplLoadDialog)


class MapWorldMixin: #vers 1
    """Game folder, DAT/IPL world load, preload, water/radar settings, world render."""








    # --- Editing Panel: IPL/IDE/DAT/IMG tabs (ported from map_workshop_old_version.py) ---

    def _load_game_folder(self, preset_root: str = None): #vers 4
        """Load a GTA game's world data (DAT -> IDE -> IPL, full engine-
        order two-phase load) via the existing GTAWorldLoader - this is
        the Map Editor's actual data layer, already handling multi-game
        detection (GTA3/VC/SA/SOL) and object/instance cross-referencing;
        nothing new needed here beyond wiring it into the UI."""
        from PyQt6.QtWidgets import QFileDialog
        from apps.methods.gta_dat_parser import detect_game, GTAWorldLoader
        if not self.confirm_close("Load World"):
            return

        if preset_root:
            folder = preset_root
        else:
            folder = QFileDialog.getExistingDirectory(self, "Select GTA game folder")
            if not folder:
                return
        from apps.methods.gta_dat_parser import resolve_game_root
        folder = resolve_game_root(folder)

        game = detect_game(folder)
        if not game:
            QMessageBox.warning(self, "Load Game Folder",
                f"Couldn't detect a supported GTA game in:\n{folder}\n\n"
                "Expected a 'data' folder containing gta3.dat, gta_vc.dat, "
                "or gta.dat (IV: common/data/gta.dat).")
            return

        loader = GTAWorldLoader(game)
        self._apply_vc_layout_ipl_stems(loader)
        loader.lazy_ipl_loading = True   # don't load/scan any
                                         # IPL's content (or its models'
                                         # geometry/textures) until the
                                         # user actually asks for that
                                         # specific IPL, not the whole
                                         # world eagerly at load time
        ok = loader.load(folder)
        self._game_root = folder
        self._apply_loaded_world(loader, game, ok, "Load Game Folder")


    def _auto_load_last_world(self): #vers 1
        """Auto-load the most recently used game world on startup
        (Aug 20 2026  own explicit "option 2" choice).
        Uses the same real recent_dat_files list/_load_game_dat_file
        path the Recent menu's own entries already use, rather than a
        separate mechanism - the most recent entry is always index 0
        (see _add_recent_dat_file). Skips quietly (no warning popup)
        if there's nothing recent, the setting is off, or the file no
        longer exists on disk - a stale/missing path shouldn't greet
        every startup with an unexpected "Load failed" dialog.

        Real, defensive second safeguard (Aug 20 2026) - this is
        logically a startup-only action, so it should never run more
        than once per instance regardless of how it gets triggered;
        _apply_loaded_world's own new re-entrancy guard is the real
        fix for the actual bug, this is belt-and-suspenders on top of
        it, not a substitute for it."""
        print(f"[MapWorkshop-MARKER] _auto_load_last_world called (id={id(self)})")
        if getattr(self, '_auto_load_last_world_already_ran', False):
            print(f"[MapWorkshop-MARKER] _auto_load_last_world SKIPPED - "
                  f"already ran once on this instance (id={id(self)})")
            return
        self._auto_load_last_world_already_ran = True
        if not self.map_settings.get('auto_load_last_world'):
            return
        recent = self.map_settings.get('recent_dat_files') or []
        if not recent or not os.path.isfile(recent[0]):
            return
        self._set_status(f"Auto-loading last world: {recent[0]}")
        QApplication.processEvents()
        self._load_game_dat_file(preset_dat_path=recent[0])

    def _load_game_dat_file(self, preset_dat_path: str = None, force_preload_img: bool = False): #vers 5
        """Load a GTA game's world data starting from one specific .dat
        file, rather than a whole game folder."""
        from PyQt6.QtWidgets import QFileDialog
        from apps.methods.gta_dat_parser import (detect_game_from_dat_filename, GTAWorldLoader,
                                                 dat_game_root)
        if not self.confirm_close("Load World"):
            return

        if preset_dat_path:
            dat_path = preset_dat_path
        else:
            dat_path, _ = QFileDialog.getOpenFileName(
                self, "Select GTA .dat file", "",
                "GTA DAT files (gta3.dat gta_vc.dat gta.dat gta_sol.dat gtasol.dat gta_quick.dat);;All files (*.dat)")
            if not dat_path:
                return

        game = detect_game_from_dat_filename(dat_path)
        if not game:
            QMessageBox.warning(self, "Load Game DAT File",
                f"Couldn't identify the game from:\n{dat_path}\n\n"
                "Expected the game's main .dat file (gta3.dat, gta_vc.dat, "
                "gta.dat, or gta_sol.dat) - not default.dat, which is "
                "shared across games and can't be identified by name alone.")
            return

        game_root = dat_game_root(dat_path, game)   # data/, sol/, IV or flat iOS
        loader = GTAWorldLoader(game)
        self._apply_vc_layout_ipl_stems(loader)
        loader.lazy_ipl_loading = True
        ok = loader.load_from_dat(dat_path, game_root)
        self._game_root = game_root
        if ok:
            self._add_recent_dat_file(dat_path)
        self._force_preload_img_once = force_preload_img
        self._apply_loaded_world(loader, game, ok, "Load Game DAT File")

    def _add_recent_dat_file(self, dat_path): #vers 1
        """Record a successfully loaded .dat path in the recent-files
        list."""
        recent = list(self.map_settings.get('recent_dat_files') or [])
        recent = [p for p in recent if p != dat_path]
        recent.insert(0, dat_path)
        recent = recent[:10]
        self.map_settings.set('recent_dat_files', recent)
        self.map_settings.save()
        self._refresh_recent_dat_menu()

    def _refresh_recent_dat_menu(self): #vers 1
        """Rebuild the Recent button's dropdown from the stored list -
        called both on startup (existing history) and after every new
        load (so the newest entry appears immediately without needing
        the dock rebuilt)."""
        menu = getattr(self, '_recent_dat_menu', None)
        if menu is None:
            return
        menu.clear()
        recent = self.map_settings.get('recent_dat_files') or []
        if not recent:
            empty_act = menu.addAction("(no recent files)")
            empty_act.setEnabled(False)
            return
        for path in recent:
            action = menu.addAction(path)
            action.setToolTip(path)
            action.triggered.connect(
                lambda checked=False, p=path: self._load_game_dat_file(preset_dat_path=p))
        menu.addSeparator()
        clear_act = menu.addAction("Clear Recent Files")
        clear_act.triggered.connect(self._clear_recent_dat_files)

    def _clear_recent_dat_files(self): #vers 1
        self.map_settings.set('recent_dat_files', [])
        self.map_settings.save()
        self._refresh_recent_dat_menu()

    def _show_world_load_summary(self, title, summary_text, preloaded_files, load_dlg=None): #vers 3
        """Summary shown in the shared load window; waits until it closes."""
        dlg = load_dlg or _IplLoadDialog(self, 1)
        seconds = None
        if self.map_settings.get('auto_dismiss_summary_dialog'):
            seconds = int(self.map_settings.get('auto_dismiss_summary_seconds', 10))
        dlg.show_summary(title, summary_text, preloaded_files, seconds)
        if dlg.isVisible():
            loop = QEventLoop()
            dlg.finished.connect(loop.quit)
            loop.exec()

    def _apply_loaded_world(self, loader, game, ok, source_desc): #vers 2
        """ re-entrancy guard (Aug 20 2026  own
        terminal markers: two full _apply_loaded_world calls on the
        exact same instance, back to back, for the same load) - the
        real root cause: __init__'s own deferred QTimer.singleShot(0,
        self._auto_load_last_world) was still pending by the time a
        manually-triggered load reached one of this method's own
        processEvents() calls, letting Qt's event loop fire that
        pending timer right there, mid-method, before the first call
        had finished - a genuinely nested, re-entrant second call to
        the entire load pipeline on the very same world. Thin wrapper
        now - the real body (unchanged, just renamed) only runs once
        per instance at a time; a re-entrant nested call is skipped
        with a clear log line instead of silently double-processing
        everything (double dialogs, double IMG load)."""
        if getattr(self, '_applying_loaded_world', False):
            print(f"[MapWorkshop-MARKER] _apply_loaded_world RE-ENTRANT CALL "
                  f"SKIPPED (source={source_desc!r}, id={id(self)}) - "
                  f"a load was already in progress on this instance")
            return
        self._applying_loaded_world = True
        try:
            self._apply_loaded_world_impl(loader, game, ok, source_desc)
        finally:
            self._applying_loaded_world = False

    def _apply_loaded_world_impl(self, loader, game, ok, source_desc): #vers 11
        """Shared post-load handling for both _load_game_folder and
        _load_game_dat_file - status message, populating the World View
        panes/Instance List/IPL Sections panel, and the summary/error
        dialog. Factored out so both loading paths (folder-based, or a
        specific .dat file) share exactly the same result handling."""
        print(f"[MapWorkshop-MARKER] _apply_loaded_world called "
              f"(source={source_desc!r}, ok={ok}, id={id(self)})")
        self._world_loader = loader
        self._loaded_dat_path = getattr(loader.main_dat, 'dat_path', '') if hasattr(loader, 'main_dat') else ''
        # Reset the render-mode-set flag on a fresh world load (Aug 1 2026)
        self._world_render_mode_set = False
        # Keep radar-tile grid extent in sync with whichever game just
        # loaded, if that grid mode is active (Aug 20 2026)
        vp_for_grid = getattr(self, 'preview_widget', None)
        if vp_for_grid is not None and hasattr(vp_for_grid, 'set_radar_grid_extent'):
            from apps.methods.gta_dat_parser import RADAR_GRID_PRESETS, WATER_GRID_PRESETS
            game_key_for_grid = getattr(loader, 'game', 'sa')
            preset_for_grid = RADAR_GRID_PRESETS.get(game_key_for_grid, RADAR_GRID_PRESETS['sa'])
            grid_size_for_grid = preset_for_grid['grid_size']
            vp_for_grid.set_radar_grid_extent(
                grid_size_for_grid / preset_for_grid['tiles_per_side'], grid_size_for_grid / 2.0)
            if hasattr(vp_for_grid, 'set_water_map_extent'):
                # Water's own real grid size can genuinely differ from
                # radar's (Sep 5 2026, confirmed for SOL against real
                # engine source - see WATER_GRID_PRESETS' own
                # docstring) - use it here too, not radar's, so the
                # wrap-around extent below matches the actual cell
                # positions _waterpro_to_cells already uses.
                water_preset_for_grid = WATER_GRID_PRESETS.get(game_key_for_grid, preset_for_grid)
                vp_for_grid.set_water_map_extent(water_preset_for_grid['grid_size'] / 2.0)
        # Auto-detect timecyc.dat next to this world's own main .dat
        # file (Aug 20 2026,  "we need to be able to detect
        # the timecyc.dat file without the need for a path, and still
        # keep the toggle") - re-checked on every world load (not just
        # once), so switching between different games' maps picks up
        # each one's own real timecyc.dat rather than getting stuck on
        # whichever game's path was auto-detected first.
        auto_timecyc_path = self._auto_detect_timecyc_path()
        if auto_timecyc_path:
            self.map_settings.set('timecyc_path', auto_timecyc_path)
            if vp_for_grid is not None and hasattr(vp_for_grid, 'set_timecyc_path'):
                vp_for_grid.set_timecyc_path(auto_timecyc_path, known_game=getattr(loader, 'game', None))
        # Same for the radar tex layer, if it's on (Aug 20 2026) -
        # moved below, after model_cache.index_img_files() actually
        # runs (search "radar tex layer needs the freshly-indexed") -
        # ModelCache isn't indexed for this world's own IMG files yet
        # at this point in the method, so reading radarNN.txd here
        # would silently find nothing or use a stale previous index.

        if not ok:
            QMessageBox.warning(self, source_desc,
                f"Load failed:\n" + "\n".join(loader.stats.errors[:10]))
            return

        self._set_status(
            f"Loaded {game.upper()} world: {len(loader.objects)} objects, "
            f"{len(loader.instances)} instances, {loader.stats.ipl_files} IPL files - "
            f"resolving LOD pairs...")
        QApplication.processEvents()
        self._lod_pairs = loader.resolve_lod_pairs()
        self._lod_overrides = {}
        self._lod_display_mode = 'normal'

        self._set_status("Indexing IMG archives...")
        QApplication.processEvents()
        model_cache = getattr(self, '_model_cache', None)
        if model_cache is None:
            from apps.components.Map_Editor.depends.model_cache import ModelCache
            model_cache = ModelCache()
            self._model_cache = model_cache
        model_cache.set_texture_limit(self._texture_limits(), self.map_settings.get('tex_keep_dxt'))
        model_cache.index_img_files(loader.get_img_paths())
        col_in_img = model_cache.col_entries_found_in_img
        if col_in_img:
            self._set_status(
                f"Indexed {len(loader.get_img_paths())} IMG archive(s) - "
                f"found {col_in_img} .col file(s) inside them")
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Found {col_in_img} .col file(s) in IMG archives "
                    f"({len(model_cache._col_img_index)} model name(s) indexed)")
        QApplication.processEvents()

        # Retool the world load's own already-working water auto-load
        # (Aug 20 2026, re-applied  "get the water working,
        # from the preloaded file") - loader.waterpro/water_shapes are
        # already real and populated by this same world load, so
        # water2 doesn't need ,separately re-pick the same
        # file through Preload every time - only the texture still
        # does.
        self._apply_radar_settings()
        try:
            self._try_auto_water2_from_loader()
        except Exception as e:
            import traceback
            traceback.print_exc()
            self._set_status(f"Water auto-apply failed: {e}")

        # Also register each real IMG file as a real, visible tab in
        # IMG Factory's own main tab system (Aug 20 2026, .per:
        # "loading img into img factory the other tools like txd
        # workshop, rader workshop, see the files, but preloading the
        # img into map workshop, the radar workshop, model workshop,
        # col workshop don't see the files, so preloading the img
        # needs to follow the same rules as loading the img file into
        # img factory and show the tab"). ModelCache's own indexing
        # above is real and correct for Map Workshop's own internal
        # geometry/texture lookups, but it's a completely separate,
        # internal index other tools never see - only main_window.
        # open_files (the real tab registry _load_img_file_in_new_tab
        # populates) is what those other tools actually discover
        # files through. Skips any IMG path already open as a tab
        # (checked by real file path, not just name) so re-loading
        # the same world repeatedly doesn't pile up duplicate tabs.
        mw = getattr(self, 'main_window', None)
        if mw is not None and hasattr(mw, '_load_img_file_in_new_tab'):
            already_open = set()
            for info in getattr(mw, 'open_files', {}).values():
                fp = info.get('file_path') if isinstance(info, dict) else None
                if fp:
                    already_open.add(os.path.normpath(fp))
            to_open = [p for p in loader.get_img_paths()
                       if os.path.normpath(p) not in already_open]
            if to_open:                    # reported in the world load window
                self._world_load_dlg = _IplLoadDialog(self, 1)
                self._world_load_dlg.start_file(f"{game.upper()} world - loading IMG archives", 0)
                mw.load_img_files_batch(to_open, "Map Workshop - loading IMG archives",
                                        report=self._world_load_dlg)

        # radar tex layer needs the freshly-indexed ModelCache, so
        # this runs here rather than earlier in this method (Aug 20
        # 2026,  "those radar.txd files are in the gta3...
        # unless it's SOL where they're in another file").
        if (vp_for_grid is not None and hasattr(vp_for_grid, 'set_radar_tex_layer')
                and self.map_settings.get('show_radar_tex_layer')):
            game_key_for_tex = getattr(loader, 'game', 'sa')
            tile_textures_for_tex = self._load_radar_tex_tiles(game_key_for_tex)
            vp_for_grid.set_radar_tex_layer(True, tile_textures_for_tex, game_key_for_tex)

        #  in-game water texture (Aug 20 2026,  "the
        # ../model/particle.txd water textures... with the option
        # settings path for using other water textures") - same real
        # reason as radar's own tex layer just above: needs the
        # freshly-indexed ModelCache, so this runs here too. Only
        # attempted when my own "Use water texture" toggle is on
        # - matches that same toggle's own existing meaning (prefer a
        # real texture over the plain flat fill) rather than a second,
        # separate on/off switch. Skipped when a real, manually-chosen
        # custom texture path is already set - unlike timecyc.dat/
        # waterpro.dat, a custom water texture isn't game-specific, so
        # it should stay stickier across world reloads rather than
        # being silently overwritten by the next real particle.txd
        # extraction every time a new world loads.
        if (vp_for_grid is not None and hasattr(vp_for_grid, 'set_water2_texture_rgba')
                and self.map_settings.get('water2_use_texture')
                and not self.map_settings.get('water2_custom_texture_path')):
            water_tex = self._load_water_texture_from_particle_txd()
            if water_tex is not None:
                vp_for_grid.set_water2_texture_rgba(*water_tex)

        # CRT time overlay (Aug 20 2026,  "[TIME] showing
        # the time in the viewpoint like old style green CRT, click on
        # time for stop and start, right click for settings") - wired
        # once per real viewport instance (guarded, since this real
        # world-load path can run more than once per session) rather
        # than at IPL Controls dock construction time, since self.
        # preview_widget isn't guaranteed to exist yet at that earlier
        # point. Shown once a real world is actually loaded.
        if vp_for_grid is not None and hasattr(vp_for_grid, 'connect_crt_time_clicks'):
            if not getattr(vp_for_grid, '_crt_time_wired', False):
                vp_for_grid.connect_crt_time_clicks(
                    on_left=self._on_crt_time_clicked,
                    on_right=self._show_time_flow_settings_popup)
                vp_for_grid._crt_time_wired = True
            if hasattr(vp_for_grid, 'set_crt_time_visible'):
                vp_for_grid.set_crt_time_visible(True)
            time_edit = getattr(self, '_tobj_time_spin', None)
            if time_edit is not None and hasattr(vp_for_grid, 'set_crt_time_text'):
                vp_for_grid.set_crt_time_text(time_edit.time().toString("HH:mm"))

        # force_preload_img_once (Aug 16 2026) - a per-load override
        force_once = getattr(self, '_force_preload_img_once', False)
        self._force_preload_img_once = False
        if self.map_settings.get('preload_img_on_dat_load') or force_once:
            self._preload_img_archives_to_os_cache(loader.get_img_paths())

        # Standalone .col indexing (Aug 14 2026)
        col_paths = loader.get_col_paths()
        if col_paths:
            self._set_status("Indexing COL files...")
            QApplication.processEvents()
            model_cache.index_col_files(col_paths)
        # A genuine new world load - also clear previously uploaded
        # GL textures here (not just the geometry cache), since
        # _refresh_world_view_impl no longer wipes/reuploads textures
        # on every call (see its own #vers note)
        self._geometry_conversion_cache = {}
        vp_for_clear = getattr(self, 'preview_widget', None)
        if vp_for_clear is not None and hasattr(vp_for_clear, 'clear_textures'):
            if hasattr(vp_for_clear, 'makeCurrent'):
                vp_for_clear.makeCurrent()
            vp_for_clear.clear_textures()

        from collections import Counter as _Counter
        self._object_instance_counts = _Counter()
        self._object_instance_count_up_to = 0

        self._ipl_format_cache = {}

        self._set_status("Applying LOD filter...")
        QApplication.processEvents()
        visible = self._apply_lod_filter(loader.instances)
        for pane in getattr(self, '_world_panes', []):
            pane.set_instances(visible)
            pane.set_model_cache(model_cache)
            pane.set_cull_boxes(loader.culls, getattr(self, '_cull_boxes_act', None) and
                                self._cull_boxes_act.isChecked())
        self._set_status(f"Building instance list ({len(visible)} instances)...")
        QApplication.processEvents()
        self._populate_instance_list(_FilteredLoaderStub(visible, loader))
        self._set_status("Refreshing world view...")
        QApplication.processEvents()
        self._refresh_world_view(visible)
        self._set_status("Populating IPL Sections...")
        QApplication.processEvents()
        self._populate_ipl_sections(loader)

        #  fix (Aug 20 2026,  "preload from startup
        # does not work, but if I bring up preload dialog, and press
        # load, it works?") - moved here, after _populate_ipl_sections
        # actually runs, not before it. The earlier position ran a
        # full 81 real lines before the IPL Sections table was even
        # built, so the .ipl/.zon branch in _load_preloaded_file
        # (which searches that same table for a matching row) always
        # found zero rows during this automatic, startup-triggered
        # path - the exact reason it worked fine when manually
        # reopening the Preload dialog later (table already built by
        # then) but never during the automatic path itself.
        preloaded_files = []
        load_dlg = getattr(self, '_world_load_dlg', None) or _IplLoadDialog(self, 1)
        self._world_load_dlg = None             # one window: archives, preloads, summary
        load_dlg.start_file(f"{game.upper()} world - preloading", 0)
        self._ipl_load_dlg = load_dlg
        try:
            preloaded_files = self._apply_saved_preload_picks() or []
        except Exception as e:
            import traceback
            traceback.print_exc()
            self._set_status(f"Preload auto-apply failed: {e}")
            load_dlg.message(f"Preload failed: {e}")
        finally:
            self._ipl_load_dlg = None

        self._set_status(f"Populating Object Browser ({len(loader.objects)} objects)...")
        QApplication.processEvents()
        self._populate_object_browser(loader)
        self._set_status("Refreshing IDE tab...")
        QApplication.processEvents()
        self._refresh_ide_tab(loader)
        self._refresh_dat_tab()
        self._set_status("Refreshing IMG tab...")
        QApplication.processEvents()
        self._refresh_img_tab(loader)
        self._refresh_ipl_inst_file_panel()

        if hasattr(self, '_show_occl_chk'):
            self._show_occl_chk.setVisible(game != 'gta3')

        if hasattr(self, '_show_sa_nodes_chk'):
            self._show_sa_nodes_chk.setVisible(game == 'sa')
        if hasattr(self, '_show_auzo_chk'):
            self._show_auzo_chk.setVisible(game == 'sa')

        tab_bar = getattr(self, '_ipl_tab_bar', None)
        tab_keys = getattr(self, '_ipl_tab_keys', None)
        if tab_bar is not None and tab_keys:
            sa_only = {'grge', 'enex', 'jump', 'tcyc', 'auzo', 'mult'}
            vc_sa_only = {'pick', 'occl'}
            hidden_keys = set()
            if game != 'sa':
                hidden_keys |= sa_only
            if game not in ('vc', 'sa'):
                hidden_keys |= vc_sa_only
            current_idx = tab_bar.currentIndex()
            if 0 <= current_idx < len(tab_keys) and tab_keys[current_idx] in hidden_keys:
                inst_idx = tab_keys.index('inst') if 'inst' in tab_keys else 0
                tab_bar.setCurrentIndex(inst_idx)
            for idx, key in enumerate(tab_keys):
                tab_bar.setTabVisible(idx, key not in hidden_keys)
        self._set_status(
            f"Loaded {game.upper()} world: {len(loader.objects)} objects, "
            f"{len(loader.instances)} instances, {loader.stats.ipl_files} IPL files")
        self._show_world_load_summary(source_desc, loader.get_summary(), preloaded_files, load_dlg)

        if getattr(loader, 'lazy_ipl_loading', False) and loader.available_ipls:
            if self.map_settings.get('show_load_options_dialog', True):
                selection = self._show_load_options_dialog(loader)
                if selection is not None:
                    stems, load_models, load_textures = selection
                    if stems:
                        self._load_selected_ipls_with_log(
                            loader, model_cache, stems, load_models, load_textures)

    def _load_selected_ipls_with_log(self, loader, model_cache, stems, load_models, load_textures): #vers 4
        """Load a batch of specific IPLs (from the Load Options dialog)"""
        total_stems = len(stems)
        dlg = _IplLoadDialog(self, total_stems)
        verbose = self.map_settings.get('show_verbose_loading_dialog')
        any_loaded = False
        for idx, stem in enumerate(stems):
            if dlg.cancelled:
                dlg.message("Cancelled - remaining IPLs not loaded")
                break
            entry = loader.available_ipls.get(stem)
            display_name = os.path.basename(entry.abs_path) if entry else stem
            dlg.start_file(display_name, idx)

            before_count = len(loader.instances)
            result = loader.load_ipl_by_name(stem)
            if not result.success:
                dlg.message(f"{display_name} - failed to load"
                            + (f": {result.errors[0]}" if result.errors else ""))
                continue
            any_loaded = True
            for w in result.warnings:
                dlg.message(f"WARNING: {w}")
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"[{display_name}] {w}")

            new_instances = loader.instances[before_count:]
            if verbose:
                for inst in new_instances:
                    dlg.message(f"{inst.model_id}, {inst.model_name}, {inst.interior}, "
                                f"{inst.pos_x}, {inst.pos_y}, {inst.pos_z}, "
                                f"{inst.scale_x}, {inst.scale_y}, {inst.scale_z}, "
                                f"{inst.rot_x}, {inst.rot_y}, {inst.rot_z}, {inst.rot_w}")

            missing = self._load_models_into_dialog(
                dlg, loader, model_cache, new_instances, load_models, load_textures)

            problem_count = result.error_count + result.warning_count
            if missing:
                dlg.message(f"{display_name} loaded - "
                            + "/ ".join(missing) + " missing from img file")
                self._write_ipl_error_log(result)
            elif problem_count:
                log_path = self._write_ipl_error_log(result)
                dlg.message(f"{display_name} loaded - {problem_count} issue(s) found"
                            + (f", check {os.path.basename(log_path)} added to the maps folder"
                               if log_path else ""))

        if any_loaded:
            self._all_instances = list(loader.instances)
            self._populate_object_browser(loader)
            visible = self._apply_lod_filter(loader.instances)
            for pane in getattr(self, '_world_panes', []):
                pane.set_instances(visible)

        dlg.finish(f"Done - {'cancelled, ' if dlg.cancelled else ''}{total_stems} IPL(s) processed")

    def _show_load_options_dialog(self, loader): #vers 1
        """Shown right after a world's IPL list is discovered (but
        before any of their content is actually loaded) - lets the
        user choose "load from .dat file" (stay purely lazy, nothing
        loaded now, browse/load individual IPLs later via their eye
        icons - the existing behaviour) or "load with options" (pick
        specific IPLs, and whether to load their models/textures, right
        now). Returns (selected_stems, load_models, load_textures), or
        None if the user picked "load from .dat file" / cancelled."""
        print(f"[MapWorkshop-MARKER] _show_load_options_dialog shown (id={id(self)})")
        dlg = QDialog(self)
        dlg.setWindowTitle("Load Options")
        dlg.setMinimumWidth(420)
        lay = QVBoxLayout(dlg)

        dat_only_radio = QRadioButton(
            "Load from .dat file - IPL data is discovered but not loaded; "
            "no models or textures loaded into the scene\n"
            "(load individual IPLs later via their eye icon in IPL Sections)")
        dat_only_radio.setChecked(True)
        with_options_radio = QRadioButton("Load with options:")
        lay.addWidget(dat_only_radio)
        lay.addWidget(with_options_radio)

        options_box = QGroupBox()
        themecol_opt = self.app_settings.get_theme_colors()
        panel_bg_opt = themecol_opt.get('panel_bg')
        if panel_bg_opt:
            options_box.setStyleSheet(f"QGroupBox {{ background: {panel_bg_opt}; }}")
        options_lay = QVBoxLayout(options_box)
        ipl_list = QListWidget()
        ipl_list.setSelectionMode(QAbstractItemView.SelectionMode.NoSelection)
        stem_by_display = {}
        for stem, entry in sorted(loader.available_ipls.items(),
                                  key=lambda kv: os.path.basename(kv[1].abs_path).lower()):
            display_name = os.path.basename(entry.abs_path)
            item = QListWidgetItem(display_name)
            item.setFlags(item.flags() | Qt.ItemFlag.ItemIsUserCheckable)
            item.setCheckState(Qt.CheckState.Unchecked)
            ipl_list.addItem(item)
            stem_by_display[display_name] = stem
        options_lay.addWidget(ipl_list)

        select_row = QHBoxLayout()
        select_all_btn = QPushButton("Select All")
        select_none_btn = QPushButton("Select None")
        select_row.addWidget(select_all_btn)
        select_row.addWidget(select_none_btn)
        select_row.addStretch()
        options_lay.addLayout(select_row)

        def _set_all_checked(checked): #vers 1
            state = Qt.CheckState.Checked if checked else Qt.CheckState.Unchecked
            for i in range(ipl_list.count()):
                ipl_list.item(i).setCheckState(state)
        select_all_btn.clicked.connect(lambda: _set_all_checked(True))
        select_none_btn.clicked.connect(lambda: _set_all_checked(False))

        models_chk = QCheckBox("Model")
        models_chk.setChecked(True)
        textures_chk = QCheckBox("Textures")
        textures_chk.setChecked(True)
        mt_row = QHBoxLayout()
        mt_row.addWidget(models_chk)
        mt_row.addWidget(textures_chk)
        mt_row.addStretch()
        options_lay.addLayout(mt_row)
        lay.addWidget(options_box)

        def _sync_options_enabled(): #vers 1
            options_box.setEnabled(with_options_radio.isChecked())
        dat_only_radio.toggled.connect(_sync_options_enabled)
        with_options_radio.toggled.connect(_sync_options_enabled)
        _sync_options_enabled()

        btn_row = QHBoxLayout()
        btn_row.addStretch()
        continue_btn = QPushButton("Continue")
        continue_btn.setDefault(True)
        btn_row.addWidget(continue_btn)
        lay.addLayout(btn_row)

        result = {}
        def _on_continue(): #vers 1
            if with_options_radio.isChecked():
                selected_stems = [
                    stem_by_display[ipl_list.item(i).text()]
                    for i in range(ipl_list.count())
                    if ipl_list.item(i).checkState() == Qt.CheckState.Checked]
                result['value'] = (selected_stems, models_chk.isChecked(),
                                   textures_chk.isChecked())
            else:
                result['value'] = ([], True, True)   # dat-only: nothing to load
            dlg.accept()
        continue_btn.clicked.connect(_on_continue)

        dlg.exec()
        return result.get('value')

    def _show_preload_dialog(self): #vers 2
        """New "Preload" dialog (Aug 20 2026,  "we need a
        preload menu, on a right click, and map workshop menu, where
        it shows the contents on the game/data/ folder, picking the
        file >> over to the preload box, including the waterpro.dat,
        because we can't seem to find it otherwise"). Lists every
        real file in the currently loaded game's own real data
        folder(s) (via _game_data_folder_candidates' own already-
        working, SOL-aware logic) on the left; a real "preload box"
        list on the right that files get moved into via >>/<<, with
        a real Load button that parses and applies whichever real
        file types this app actually knows how to load (water.dat/
        waterpro.dat/timecyc.dat/.img right now) - unrecognised file
        types stay listed but do nothing when loaded, rather than
        silently being hidden, since my own request was to see
        the real folder contents directly, not a filtered guess at
        what he might want.

         additions (Aug 20 2026,  "need a save button
        that remembers picked entries, also able to see the path
        files, dir level up down"):
        -  directory navigation - a real, editable current-folder
          path field, an "Up" button, and double-clicking a real
          subfolder entry (shown with a trailing "/") navigates into
          it, not just the one fixed data folder.
        - A real Save button that persists the current "to preload"
          list's own real file paths to map_settings, restored
          automatically the next time this dialog opens - the same
          picks are there again without re-building the list by
          hand each time."""
        folders = self._game_data_folder_candidates()
        dlg = QDialog(self)
        dlg.setWindowTitle("Preload Game Data Files")
        dlg.resize(680, 460)
        outer = QVBoxLayout(dlg)

        if not folders:
            outer.addWidget(QLabel(
                "No game data folder known yet - load a world first,\n"
                "or activate a project with a real game root set."))
            close_btn = QPushButton("Close")
            close_btn.clicked.connect(dlg.accept)
            outer.addWidget(close_btn)
            dlg.exec()
            return

        current_folder = [folders[0]]   # mutable box so inner closures can rebind it

        path_row = QHBoxLayout()
        path_edit = QLineEdit(current_folder[0])
        up_btn = QPushButton("Up")
        tex_btn = QPushButton("App Textures")
        tex_btn.setToolTip(
            "Jump to this app's own tex/ folder (app asset, not\n"
            "part of any game's data folder).")
        path_row.addWidget(QLabel("Folder:"))
        path_row.addWidget(path_edit)
        path_row.addWidget(up_btn)
        path_row.addWidget(tex_btn)
        outer.addLayout(path_row)

        lists_row = QHBoxLayout()
        avail_list = QListWidget()
        avail_list.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)

        def _refresh_avail_list(): #vers 1
            avail_list.clear()
            path_edit.setText(current_folder[0])
            try:
                entries = sorted(os.listdir(current_folder[0]))
            except OSError:
                entries = []
            for entry_name in entries:
                full = os.path.join(current_folder[0], entry_name)
                if os.path.isdir(full):
                    item = QListWidgetItem(entry_name + "/")
                    item.setData(Qt.ItemDataRole.UserRole, ('dir', full))
                    avail_list.addItem(item)
            for entry_name in entries:
                full = os.path.join(current_folder[0], entry_name)
                if os.path.isfile(full):
                    item = QListWidgetItem(entry_name)
                    item.setData(Qt.ItemDataRole.UserRole, ('file', full))
                    avail_list.addItem(item)

        def _go_up(): #vers 1
            parent = os.path.dirname(os.path.normpath(current_folder[0]))
            if parent and parent != current_folder[0]:
                current_folder[0] = parent
                _refresh_avail_list()
        def _go_to_typed_path(): #vers 1
            typed = path_edit.text()
            if os.path.isdir(typed):
                current_folder[0] = typed
                _refresh_avail_list()
        def _go_to_app_tex(): #vers 2
            """ fix (Aug 20 2026,  "tex folder was moved
            to depends/tex/ in map_workshop") - was computing the repo
            root (3 levels up from this file) then looking for tex/
            there; the real, current location is one level down from
            this same file's own directory instead."""
            this_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))   # Map_Editor/
            tex_dir = os.path.join(this_dir, 'depends', 'tex')
            if os.path.isdir(tex_dir):
                current_folder[0] = tex_dir
                _refresh_avail_list()
            else:
                status_label.setText(f"No tex/ folder found at {tex_dir}")
        up_btn.clicked.connect(_go_up)
        path_edit.returnPressed.connect(_go_to_typed_path)
        tex_btn.clicked.connect(_go_to_app_tex)

        btn_col = QVBoxLayout()
        to_preload_btn = QPushButton(">>")
        to_avail_btn = QPushButton("<<")
        btn_col.addStretch()
        btn_col.addWidget(to_preload_btn)
        btn_col.addWidget(to_avail_btn)
        btn_col.addStretch()

        preload_list = QListWidget()
        preload_list.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)

        def _move_to_preload(): #vers 2
            for item in avail_list.selectedItems():
                kind, full = item.data(Qt.ItemDataRole.UserRole)
                if kind == 'dir':
                    continue   # folders navigate, they don't preload
                new_item = QListWidgetItem(item.text())
                new_item.setData(Qt.ItemDataRole.UserRole, full)
                preload_list.addItem(new_item)
        def _move_to_avail(): #vers 1
            for item in preload_list.selectedItems():
                row = preload_list.row(item)
                preload_list.takeItem(row)
        def _avail_double_clicked(item): #vers 1
            kind, full = item.data(Qt.ItemDataRole.UserRole)
            if kind == 'dir':
                current_folder[0] = full
                _refresh_avail_list()
            else:
                _move_to_preload()
        to_preload_btn.clicked.connect(_move_to_preload)
        to_avail_btn.clicked.connect(_move_to_avail)
        avail_list.itemDoubleClicked.connect(_avail_double_clicked)
        preload_list.itemDoubleClicked.connect(lambda item: _move_to_avail())

        avail_col = QVBoxLayout()
        avail_col.addWidget(QLabel("Available (double-click a folder to open it, a file or >> to add):"))
        avail_col.addWidget(avail_list)
        preload_col = QVBoxLayout()
        preload_col.addWidget(QLabel("To preload:"))
        preload_col.addWidget(preload_list)

        lists_row.addLayout(avail_col)
        lists_row.addLayout(btn_col)
        lists_row.addLayout(preload_col)
        outer.addLayout(lists_row)

        # Restore any previously saved picks (full real paths, so
        # they still resolve correctly even after navigating away
        # from the folder they were originally picked in).
        saved_picks = self.map_settings.get('preload_saved_files') or []
        for full in saved_picks:
            full = self._to_gamedata_role_marker(full)
            if full.startswith('<gamedata-role>/'):
                #  fix (Aug 20 2026, same real role marker
                # _apply_saved_preload_picks/_do_load below both
                # resolve - without this, os.path.isfile below would
                # always be False for a real "<gamedata-role>/..."
                # marker (it's a real, symbolic placeholder, not an
                # actual filesystem path), so these real, per-game
                # saved picks would silently vanish from this list
                # every time it's reopened, even though they're still
                # genuinely applied automatically behind the scenes.
                resolved = self._resolve_gamedata_role_marker(full)
                if resolved is None:
                    continue
                item = QListWidgetItem(os.path.basename(resolved))
                item.setData(Qt.ItemDataRole.UserRole, full)
                preload_list.addItem(item)
            elif os.path.isfile(full):
                item = QListWidgetItem(os.path.basename(full))
                item.setData(Qt.ItemDataRole.UserRole, full)
                preload_list.addItem(item)

        _refresh_avail_list()

        status_label = QLabel("")
        status_label.setWordWrap(True)
        outer.addWidget(status_label)

        btn_row = QHBoxLayout()
        save_btn = QPushButton("Save Picks")
        load_btn = QPushButton("Load")
        close_btn = QPushButton("Close")
        btn_row.addWidget(save_btn)
        btn_row.addStretch()
        btn_row.addWidget(load_btn)
        btn_row.addWidget(close_btn)
        outer.addLayout(btn_row)

        def _do_save(): #vers 3
            """ fix (Aug 20 2026,  "reload has the
            entries saved but there not being loaded") - this never
            called map_settings.save() (only .set(), which just
            updates the in-memory dict) - real persistence to disk
            only happened via the app's own quit-time auto-save, so
            anything short of a clean app exit lost it. Saves
            immediately now, the same real pattern every other
            explicit "save this setting now" action in this file
            already uses.

             fix (Aug 20 2026,  "../data/waterpro.dat -
            for LC and VC, but SA map looks for ../data/water.dat") -
            the marker used to hardcode the specific filename it was
            saved with ("<gamedata>/waterpro.dat"), which broke the
            moment the current game's own real family uses a
            genuinely different filename for the same real role - SA
            has no real waterpro.dat at all, only its own real water.
            dat. Now stores a real, logical role ("<gamedata-role>/
            water" or "<gamedata-role>/timecyc") instead of one
            specific filename, resolved against whichever real
            filename(s) that role's own real game family actually
            uses - see _resolve_gamedata_role_marker's own docstring
            for the real, confirmed per-family filenames."""
            paths = []
            for i in range(preload_list.count()):
                full = preload_list.item(i).data(Qt.ItemDataRole.UserRole)
                name = os.path.basename(full).lower()
                if name in ('waterpro.dat', 'water.dat'):
                    paths.append("<gamedata-role>/water")
                elif name == 'timecyc.dat':
                    paths.append("<gamedata-role>/timecyc")
                else:
                    paths.append(full)
            self.map_settings.set('preload_saved_files', paths)
            self.map_settings.save()
            status_label.setText(f"Saved {len(paths)} pick(s) - restored automatically next time this opens.")
        def _do_load(): #vers 4
            """ fix (Aug 20 2026, same real role marker
            _apply_saved_preload_picks/the restore-into-UI logic above
            both resolve, via the same shared _resolve_gamedata_role_
            marker) - a manually-clicked "Load" on a real, saved
            per-game marker needs the same real resolution against the
            current game's own data folder, otherwise this would try
            to load the literal string "<gamedata-role>/water" as a
            real file path and fail."""
            loaded, unrecognised = [], []
            for i in range(preload_list.count()):
                item = preload_list.item(i)
                full = self._to_gamedata_role_marker(item.data(Qt.ItemDataRole.UserRole))
                if full.startswith('<gamedata-role>/'):
                    resolved = self._resolve_gamedata_role_marker(full)
                    if resolved is None:
                        unrecognised.append(item.text())
                        continue
                    full = resolved
                if self._load_preloaded_file(full):
                    loaded.append(item.text())
                else:
                    unrecognised.append(item.text())
            #  fix (Aug 20 2026,  "I'd prefer to show
            # those one line at a time, not as a single word-wrapped
            # line") - each real loaded/unrecognised filename now gets
            # its own real line, instead of one comma-joined, wrapped
            # line that's hard to scan for a longer real Preload list
            # like SA's own real, larger set of extra files.
            msg = []
            if loaded:
                msg.append("Loaded:")
                msg.extend(f"  {name}" for name in loaded)
            if unrecognised:
                msg.append("Not a recognised type (no action taken):")
                msg.extend(f"  {name}" for name in unrecognised)
            status_label.setText("\n".join(msg) if msg else "Nothing selected.")
        save_btn.clicked.connect(_do_save)
        load_btn.clicked.connect(_do_load)
        close_btn.clicked.connect(dlg.accept)
        dlg.exec()

    def _apply_saved_preload_picks(self): #vers 2
        """Automatically re-apply the Preload dialog's own saved
        picks once a real world is available to apply them to (Aug
        20 2026,  "starting map_workshop back up, I noticed
        there is nothing in the startup, saying preloading files,
        etc") - Save Picks previously only restored the saved list
        back into the dialog's own UI the next time it was manually
        reopened; the actual loading still needed a manual Load click
        every time. Runs each saved path through the same real _load_
        preloaded_file every dialog Load/manual pick already uses, so
        behaviour is identical either way - just automatic now.

         fix (Aug 20 2026,  "preload still not working,
        and nothing in the status log, is there a conflict somewhere")
        - traced map_settings' own real save/load path (singleton,
        debounced auto-save, app-folder-relative config path) and it
        all looked correct on inspection; couldn't reproduce or
        disprove the actual failure without running the real app.
        Always leaves a real, visible status message now, even when
        there's genuinely nothing saved - previously returned silent
        on an empty list, which looked identical to "this ran and
        found nothing" as it did to "this never ran at all"."""
        paths = self.map_settings.get('preload_saved_files') or []
        if not paths:
            self._set_status(
                f"Preload: no saved picks found (settings file: "
                f"{self.map_settings._path}, exists={self.map_settings._path.exists()})")
            return []
        self._set_status(f"Preloading {len(paths)} saved file(s)...")
        QApplication.processEvents()
        loaded, unrecognised, missing = [], [], []
        for path in paths:
            # Self-healing (Aug 20 2026,  "the VC waterpro.
            # dat shows across SA and LC, so this needs fixing") -
            # normalises a real legacy absolute path (saved before the
            # marker fix existed) to the same real marker a fresh save
            # would produce, so it's resolved fresh against whichever
            # game is actually current below instead of being trusted
            # directly as-is.
            path = self._to_gamedata_role_marker(path)
            if path.startswith('<gamedata-role>/'):
                #  fix (Aug 20 2026,  "../data/waterpro.
                # dat - for LC and VC, but SA map looks for
                # ../data/water.dat") - resolved via the shared
                # _resolve_gamedata_role_marker, which tries every
                # real filename that role's own real game families
                # actually use (see its own docstring), instead of
                # one hardcoded filename (the previous version of
                # this same fix, which broke for SA specifically).
                resolved = self._resolve_gamedata_role_marker(path)
                if resolved is None:
                    missing.append(path[len('<gamedata-role>/'):])
                    continue
                path = resolved
            if not os.path.isfile(path):
                continue
            if self._load_preloaded_file(path):
                loaded.append(os.path.basename(path))
            else:
                unrecognised.append(os.path.basename(path))
        if loaded:
            self._set_status(f"Preloaded: {', '.join(loaded)}")
        elif unrecognised:
            self._set_status(f"Preload: nothing recognised ({', '.join(unrecognised)})")
        elif missing:
            self._set_status(
                f"Preload: couldn't find in the current game's own data "
                f"folder ({', '.join(missing)})")
        return loaded

    def _radar_settings_path(self): #vers 1
        """radar_settings.json in the loaded game root, or ''."""
        root = getattr(self, '_game_root', '') or ''
        return os.path.join(root, 'radar_settings.json') if root and os.path.isdir(root) else ''

    def _load_radar_settings(self): #vers 2
        """Per-game alignment: radar from radar_settings.json, water from data/water_offset.dat."""
        path = self._radar_settings_path()
        data = {'radar_offset': [0.0, 0.0], 'water_offset': [0.0, 0.0], 'water_fix': None}
        if path and os.path.isfile(path):
            try:
                with open(path, 'r', encoding='utf-8') as f:
                    data.update(json.load(f))
            except (OSError, ValueError) as e:
                self._set_status(f"radar_settings.json unreadable: {e}")
        wat = self._read_water_offsets()
        if 'nudge' in wat:
            data['water_offset'] = wat['nudge']
        if 'fix' in wat:
            data['water_fix'] = wat['fix']
        return data

    def _save_radar_settings(self, data): #vers 2
        """Radar offset to radar_settings.json; water nudge and fix to water_offset.dat."""
        path = self._radar_settings_path()
        if not path:
            self._set_status("No game loaded - alignment not saved")
            return
        radar = {k: v for k, v in data.items() if k not in ('water_offset', 'water_fix')}
        try:
            with open(path, 'w', encoding='utf-8') as f:
                json.dump(radar, f, indent=2)
        except OSError as e:
            self._set_status(f"Could not save radar_settings.json: {e}")
        self._write_water_offsets({'nudge': list(data.get('water_offset', [0.0, 0.0])),
                                   'fix': data.get('water_fix')})

    def _water_offset_path(self): #vers 1
        """<game>/data/water_offset.dat (data folder any case), or ''."""
        root = getattr(self, '_game_root', '') or ''
        if not root or not os.path.isdir(root):
            return ''
        data_dir = next((os.path.join(root, d) for d in os.listdir(root)
                         if d.lower() == 'data' and os.path.isdir(os.path.join(root, d))), '')
        return os.path.join(data_dir, 'water_offset.dat') if data_dir else ''

    def _read_water_offsets(self): #vers 2
        """Keys x, y, z, nudge, fix, scale from water_offset.dat; {} if none."""
        path = self._water_offset_path()
        out = {}
        if not path or not os.path.isfile(path):
            return out
        with open(path, 'r', encoding='utf-8') as f:
            for line in f:
                parts = line.split('#', 1)[0].split()
                if len(parts) < 2:
                    continue
                key, vals = parts[0].lower(), parts[1:]
                try:
                    if key in ('x', 'y', 'z', 'scale'):
                        out[key] = float(vals[0])
                    elif key == 'nudge' and len(vals) >= 2:
                        out['nudge'] = [float(vals[0]), float(vals[1])]
                    elif key == 'fix':
                        out['fix'] = {'on': True, 'off': False}.get(vals[0].lower())
                except ValueError:
                    self._set_status(f"water_offset.dat: bad value on '{line.strip()}'")
        return out

    def _write_water_offsets(self, changes): #vers 2
        """Merge changes into water_offset.dat (x/y/z offsets, nudge, fix, scale)."""
        path = self._water_offset_path()
        if not path:
            return
        cur = getattr(self, '_radar_settings', None) or {}
        vals = {'x': float(self.map_settings.get('water2_x_offset') or 0.0),     # current values seed a new file
                'y': float(self.map_settings.get('water2_y_offset') or 0.0),
                'z': float(self.map_settings.get('water2_height_offset') or 0.0),
                'nudge': list(cur.get('water_offset', [0.0, 0.0])), 'fix': cur.get('water_fix'),
                'scale': 1.0}
        vals.update(self._read_water_offsets())
        vals.update(changes)
        fix = {True: 'on', False: 'off'}.get(vals['fix'], 'auto')
        text = ("# IMG Factory - Map Workshop water offsets for this game\n"
                f"x {vals['x']:.3f}\ny {vals['y']:.3f}\nz {vals['z']:.3f}\n"
                f"nudge {vals['nudge'][0]:.3f} {vals['nudge'][1]:.3f}\nfix {fix}\n"
                f"scale {vals['scale']:g}\n")
        try:
            with open(path, 'w', encoding='utf-8') as f:
                f.write(text)
        except OSError as e:
            self._set_status(f"Could not save water_offset.dat: {e}")

    def _apply_radar_settings(self): #vers 2
        """Push saved radar/water offsets and fix to the viewport."""
        data = self._load_radar_settings()
        self._radar_settings = data
        vp = getattr(self, 'preview_widget', None)
        if vp is None:
            return
        if hasattr(vp, 'set_radar_tex_offset'):
            vp.set_radar_tex_offset(*data['radar_offset'])
        if hasattr(vp, 'set_water2_nudge'):
            vp.set_water2_nudge(*data['water_offset'])
        if hasattr(vp, 'set_water2_fix_override'):
            vp.set_water2_fix_override(data.get('water_fix'))
        wat = self._read_water_offsets()            # per-game X/Y/Z offsets win
        for key, setting, setter in (('x', 'water2_x_offset', 'set_water2_x_offset'),
                                     ('y', 'water2_y_offset', 'set_water2_y_offset'),
                                     ('z', 'water2_height_offset', 'set_water2_height_offset')):
            if key in wat:
                self.map_settings.set(setting, wat[key])
                if hasattr(vp, setter):
                    getattr(vp, setter)(wat[key])

    def _radar_align_offset(self, layer): #vers 1
        """Current (x, y) offset for 'radar' or 'water'."""
        data = getattr(self, '_radar_settings', None) or self._load_radar_settings()
        x, y = data.get(f'{layer}_offset', [0.0, 0.0])
        return float(x), float(y)

    def _set_radar_align_offset(self, layer, x, y): #vers 1
        """Set, apply and save one layer's offset."""
        data = getattr(self, '_radar_settings', None) or self._load_radar_settings()
        data[f'{layer}_offset'] = [float(x), float(y)]
        self._save_radar_settings(data)
        self._apply_radar_settings()

    def _water_fix_active(self): #vers 1
        """Effective water fix state for the loaded game."""
        data = getattr(self, '_radar_settings', None) or self._load_radar_settings()
        if data.get('water_fix') is not None:
            return bool(data['water_fix'])
        mode = self._water2_offset_mode()
        game = getattr(getattr(self, '_world_loader', None), 'game', '')
        return mode == 'all' or (mode == 'vc' and game == 'vc')

    def _set_water_fix(self, on): #vers 1
        """Per-game water fix on/off, saved."""
        data = getattr(self, '_radar_settings', None) or self._load_radar_settings()
        data['water_fix'] = bool(on)
        self._save_radar_settings(data)
        self._apply_radar_settings()
        dlg = getattr(self, '_radar_align_dlg', None)
        if dlg is not None and dlg.isVisible():
            dlg._refresh()

    def _show_radar_align_dialog(self): #vers 1
        """Open the radar/water alignment window."""
        from apps.gui.radar_align_dialog import RadarAlignDialog
        dlg = getattr(self, '_radar_align_dlg', None)
        if dlg is None:
            dlg = RadarAlignDialog(self, self)
            self._radar_align_dlg = dlg
        dlg._refresh()
        dlg.show()
        dlg.raise_()

    def _water_scale(self): #vers 1
        """Water grid scale for this game (water_offset.dat 'scale', default 1)."""
        return self._read_water_offsets().get('scale', 1.0)

    def _set_water_scale(self, scale): #vers 2
        """Save the water grid scale (1.0 = file size) and rebuild the water cells."""
        self._write_water_offsets({'scale': float(scale)})
        waterpro = getattr(self, '_water2_waterpro_source', None)
        if waterpro is not None:
            game = getattr(self, '_water2_game_source', 'vc')
            physical = getattr(self, '_water2_showing_physical', False)
            self._water2_cells_pending = (self._waterpro_physical_to_cells(waterpro, game) if physical
                                          else self._waterpro_to_cells(waterpro, game))
            self._apply_water2_preload()
        self._set_status(f"Water scale {scale * 100:.1f} %" + ("" if self._water_offset_path()
                                                      else " (no game data folder - not saved)"))
        dlg = getattr(self, '_radar_align_dlg', None)
        if dlg is not None and dlg.isVisible():
            dlg._refresh()

    def _water2_offset_mode(self): #vers 1
        """Radar/water fix mode; old VC-only bool when unset."""
        mode = self.map_settings.get('water2_offset_mode')
        if mode in ('vc', 'all', 'off'):
            return mode
        return 'vc' if self.map_settings.get('water2_offset_vc_only') else 'all'

    def _waterpro_to_cells(self, waterpro, game): #vers 6
        """Real, confirmed grid-to-cells logic (Aug 20 2026, re-
        applied) - shared between the manual Preload path and the
        automatic world-load retool below. Skips real "dry"/cutout
        cells (val==128, confirmed against water_workshop.py's own
        WaterGridWidget._cell_col) - already excluded by the level-
        index bounds check below too, but named explicitly for
        clarity.

         fix (Aug 20 2026,  "the water is 90 degrees
        clockwise oriented, wrong against the radar") - re-applies
        the real anticlockwise rotation, undoing the previous revert.
        That revert was the real mistake, not this rotation: matching
        compute_radar_grid's own code textually doesn't guarantee
        real alignment if waterpro.dat's own raw row/col axes don't
        actually carry the same real meaning that function's own loop
        variables do - "radar is the trusted reference" meant fix
        water to visually align with it, not copy its code verbatim
        regardless of whether the underlying data agrees. Confirmed
        directly, again, by my own real screenshot after the
        revert: water was genuinely still 90 degrees off against the
        real radar layer. (x,y) -> (-y,x) applied to every real cell
        corner reduces to row driving x directly, col driving y
        directly - see the git history for the full, real derivation
        this was first verified against."""
        from apps.methods.gta_dat_parser import RADAR_GRID_PRESETS, WATER_GRID_PRESETS
        # SOL's water grid genuinely differs from its radar grid (Sep
        # 5 2026  own real, uploaded GTASOL-CoreHacks
        # source - see WATER_GRID_PRESETS' own docstring for the full
        # derivation) - every other game's water-uses-radar's-size
        # assumption is already confirmed correct and unchanged here.
        preset = WATER_GRID_PRESETS.get(game) or RADAR_GRID_PRESETS.get(game, RADAR_GRID_PRESETS['vc'])
        grid_size = preset['grid_size'] * self._water_scale()      # per-game scale (half / normal / twice)
        half = grid_size / 2.0
        gw = waterpro.grid_width
        cell = grid_size / gw
        cells = []
        for row in range(gw):
            for col in range(gw):
                level_idx = waterpro.visible_map[row][col]
                if level_idx == 128:
                    continue
                if level_idx < 0 or level_idx >= len(waterpro.levels):
                    continue
                height = waterpro.levels[level_idx].height
                min_x = -half + row * cell
                min_y = -half + col * cell
                cells.append((min_x, min_y, min_x + cell, min_y + cell, height))
        return cells

    def _waterpro_physical_to_cells(self, waterpro, game): #vers 5
        """The other real water layer (Aug 20 2026,  "when
        you right click the water button, show the other water
        layer") - waterpro.dat's own physical_map, a real, separate
        grid exactly double visible_map's own width/height per side,
        covering the same real world area at twice the resolution.
        Same real anticlockwise rotation as _waterpro_to_cells (see
        its own docstring for the full, real reasoning), since it's
        the same real coordinate system just finer-grained. Same
        SOL-specific grid-size correction as _waterpro_to_cells too
        (Sep 5 2026) - see WATER_GRID_PRESETS' own docstring."""
        from apps.methods.gta_dat_parser import RADAR_GRID_PRESETS, WATER_GRID_PRESETS
        preset = WATER_GRID_PRESETS.get(game) or RADAR_GRID_PRESETS.get(game, RADAR_GRID_PRESETS['vc'])
        grid_size = preset['grid_size'] * self._water_scale()
        half = grid_size / 2.0
        gw = waterpro.grid_width * 2
        cell = grid_size / gw
        cells = []
        for row in range(gw):
            for col in range(gw):
                level_idx = waterpro.physical_map[row][col]
                if level_idx == 128:
                    continue
                if level_idx < 0 or level_idx >= len(waterpro.levels):
                    continue
                height = waterpro.levels[level_idx].height
                min_x = -half + row * cell
                min_y = -half + col * cell
                cells.append((min_x, min_y, min_x + cell, min_y + cell, height))
        return cells

    def _water_shapes_to_cells(self, shapes): #vers 1
        """water.dat's own shapes are real, arbitrary polygons (not a
        uniform grid) - reduced to each shape's own flat bounding box,
        since _draw_water2 only knows flat rectangular cells (Aug 20
        2026, re-applied)."""
        cells = []
        for w in shapes:
            xs = [c.x for c in w.corners]
            ys = [c.y for c in w.corners]
            zs = [c.z for c in w.corners]
            cells.append((min(xs), min(ys), max(xs), max(ys), sum(zs) / len(zs)))
        return cells

    def _apply_water2_preload(self): #vers 5
        """Combine whatever's been preloaded so far (Aug 20 2026, re-
        applied) - water cells and the water texture can be preloaded
        in either order, or independently, via separate _load_
        preloaded_file calls. Pushes whatever's available to the
        viewport and re-enables/shows [Water] the moment there's real
        water data to show. Diagnostic markers from the VC case-
        sensitivity investigation removed now that it's confirmed
        fixed."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_water2_data'):
            return
        cells = getattr(self, '_water2_cells_pending', None)
        texture_path = getattr(self, '_water2_texture_path_pending', '')
        if cells is None:
            return   # texture preloaded alone, before any real water data
        vp.set_water2_data(cells, texture_path)
        if getattr(self, '_water_gauge', None) is not None:
            self._water_gauge.update()
        if getattr(self, '_vp_gauges', None) is not None:
            self._vp_gauges.refresh()
        water_btn = getattr(self, '_show_water_chk', None)
        if water_btn is not None:
            water_btn.setEnabled(True)
            water_btn.setToolTip("Left-click: show/hide Water.")
            if hasattr(water_btn, 'set_shown'):
                water_btn.set_shown(True)

    def _try_auto_water2_from_loader(self): #vers 3
        """Retool loader.waterpro/loader.water_shapes into the new
        water2 system automatically at world-load time (Aug 20 2026,
        re-applied) - those are already populated by the existing,
        already-working auto-load pipeline (load_waterpro_dat/load_
        water_dat), so water2 doesn't need ,separately re-pick
        the same file through the Preload dialog every time - only
        the texture (a real app asset, not part of any game's own
        data) still needs a manual preload.

         fix (Aug 20 2026,  "offset should only be for
        VC, so we need a toggle to effect VC waterpro.dat only") -
        pushes the current game to the viewport via set_water2_game
        each time, so _draw_water2 can gate the X/Y offset to VC only
        rather than applying a saved VC-specific offset to whichever
        game happens to be currently loaded. Diagnostic markers from
        the earlier VC case-sensitivity investigation removed now
        that it's confirmed fixed."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        waterpro = getattr(loader, 'waterpro', None)
        water_shapes = getattr(loader, 'water_shapes', None)
        game = getattr(loader, 'game', 'vc')
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_water2_game'):
            vp.set_water2_game(game)
        if waterpro is not None:
            self._water2_waterpro_source = waterpro
            self._water2_game_source = game
            self._water2_cells_pending = self._waterpro_to_cells(waterpro, game)
            self._apply_water2_preload()
        elif water_shapes:
            self._water2_cells_pending = self._water_shapes_to_cells(water_shapes)
            self._apply_water2_preload()

    def _load_preloaded_file(self, path): #vers 3
        """Recognise and load one real file by its own real filename
        (Aug 20 2026  own Preload dialog request above).
        Returns True if this app knows how to load that real file
        type and did so, False if the type isn't recognised (the
        dialog still lists it, just can't act on it yet).

         fix (Aug 20 2026,  "if those files pre loaded,
        in objects browser the ipl and zon entries would already be
        highlighted, so the off on buttons wouldn't need to change")
        - loading the raw data alone wasn't enough on its own:
        _refresh_water_visualization (and the same real shape for
        timecyc) only actually shows anything while its own real on/
        off toggle is checked, clearing it right back out otherwise.
        Preloading a real file now also turns its own real toggle on
        via set_shown (which already emits show_toggled itself,
        triggering the real refresh the same way clicking the button
        would) - genuinely visible the moment it's preloaded, not
        loaded-but-invisible until a separate manual toggle."""
        name = os.path.basename(path).lower()
        if name.endswith('.img'):
            # Same real fix as the world-load hook (Aug 20 2026, per
            #  : "preloading the img needs to follow the same
            # rules as loading the img file into img factory and show
            # the tab so the other tools pick up those models") -
            # opens it as a real, visible tab via the same real entry
            # point manually opening one does, not just ModelCache's
            # own separate, internal index. Doesn't need a world
            # already loaded (unlike water/timecyc below), since this
            # just opens a real, independent IMG Factory tab.
            mw = getattr(self, 'main_window', None)
            if mw is None or not hasattr(mw, '_load_img_file_in_new_tab'):
                return False
            already_open = any(
                os.path.normpath(info.get('file_path', '')) == os.path.normpath(path)
                for info in getattr(mw, 'open_files', {}).values() if isinstance(info, dict))
            if not already_open:
                mw._load_img_file_in_new_tab(path)
            return True
        if name.endswith('.ipl') or name.endswith('.zon'):
            #  fix (Aug 20 2026,  "automatically search
            # for maps, paths, dat files") - routes through the same
            # real IPL Sections table entry every eye-icon click
            # already uses (_on_ipl_section_cell_clicked), rather than
            # trying to parse the file directly here. IPL/zon files
            # referenced by the game's own .dat are already real rows
            # in that table (via _rebuild_ipl_sections_rows) - matched
            # here by real filename, not re-derived from this path.
            table = getattr(self, '_ipl_sections_table', None)
            if table is None:
                return False
            for row in range(table.rowCount()):
                item = table.item(row, 0)
                if item is None:
                    continue
                ipl_name = item.data(Qt.ItemDataRole.UserRole)
                if ipl_name and ipl_name.lower() == name:
                    if ipl_name in getattr(self, '_hidden_ipls', set()):
                        self._on_ipl_section_cell_clicked(row, 0)
                    return True
            return False   # not a real row in this world's own IPL list
        if name.endswith('.png') or name.endswith('.jpg') or name.endswith('.jpeg'):
            #  texture asset (Aug 20 2026, re-applied .per:
            # "there is a tex folder now in img-factory-1.6, in there
            # is the water texture, so this can also be preloaded as
            # an asset") - doesn't need a world already loaded, just
            # the viewport. Applied immediately if water cells are
            # already preloaded, or picked up automatically the next
            # time they are.
            self._water2_texture_path_pending = path
            self._apply_water2_preload()
            return True
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return False
        if name == 'waterpro.dat':
            # Re-applied (Aug 20 2026,  "get the water
            # working, from the preloaded file, [water] button
            # off/on toggle") - pushes to the new water2 system via
            # the shared _waterpro_to_cells helper (also used by the
            # automatic world-load retool), instead of the old
            # settings-driven set_waterpro_cells path.
            from apps.methods.gta_dat_parser import parse_waterpro_dat
            result = parse_waterpro_dat(path)
            if result is None:
                return False
            game_key = getattr(loader, 'game', 'vc')
            self._water2_waterpro_source = result
            self._water2_game_source = game_key
            self._water2_cells_pending = self._waterpro_to_cells(result, game_key)
            self._apply_water2_preload()
            return True
        if name == 'water.dat':
            from apps.methods.gta_dat_parser import parse_water_dat
            shapes = parse_water_dat(path)
            if not shapes:
                return False
            self._water2_cells_pending = self._water_shapes_to_cells(shapes)
            self._apply_water2_preload()
            return True
        if name == 'timecyc.dat':
            self.map_settings.set('timecyc_path', path)
            vp = getattr(self, 'preview_widget', None)
            if vp is not None and hasattr(vp, 'set_timecyc_path'):
                known_game = getattr(getattr(self, '_world_loader', None), 'game', None)
                vp.set_timecyc_path(path, known_game=known_game)
            tcyc_btn = getattr(self, '_tcyc_chk', None)
            if tcyc_btn is not None and hasattr(tcyc_btn, 'set_shown'):
                tcyc_btn.set_shown(True)
            elif vp is not None and hasattr(vp, 'set_timecyc_playing'):
                vp.set_timecyc_playing(True)
            return True
        return False

    def _app_asset_folder(self, name): #vers 1
        """This app's own "depends/<name>/" asset folder (Aug 20
        2026,  "a right-click option to show other
        timecyc.dat files that I'll put in an asset folder" / "the
        option settings path for using other water textures") -
        shared helper for the same real "depends/<name>/" location
        the water texture's own "App Textures" shortcut already uses
        (_go_to_app_tex), rather than each real asset type re-deriving
        this same path separately. Returns the folder path whether or
        not it currently exists - callers check os.path.isdir
        themselves."""
        this_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))   # Map_Editor/
        return os.path.join(this_dir, 'depends', name)

    def _game_data_folder_candidates(self): #vers 1
        """Every real, candidate "data" folder for the currently
        loaded game, in priority order (Aug 20 2026) - extracted from
        _auto_detect_timecyc_path's own real, already-working logic
        so the new Preload dialog can list the same real folder(s)
        rather than re-deriving this separately. SOL-aware (its own
        main .dat lives at gameroot/sol, not gameroot/data, but real
        sibling files like waterpro.dat/timecyc.dat still do), plus
        the active Project Manager's own main_window.game_root as a
        second, real fallback source."""
        loader = getattr(self, '_world_loader', None)
        game_root = getattr(loader, 'game_root', '') if loader is not None else ''
        game = getattr(loader, 'game', '') if loader is not None else ''
        candidates = []
        if game_root and game == 'sol':
            candidates.append(os.path.join(game_root, 'data'))
        else:
            dat_path = getattr(self, '_loaded_dat_path', '')
            if dat_path:
                candidates.append(os.path.dirname(dat_path))
        mw_game_root = getattr(self, 'main_window', None)
        mw_game_root = getattr(mw_game_root, 'game_root', '') if mw_game_root is not None else ''
        if mw_game_root:
            candidates.append(os.path.join(mw_game_root, 'data'))
        # De-dupe while preserving order (folders can coincide)
        seen = set()
        result = []
        for c in candidates:
            norm = os.path.normpath(c)
            if norm not in seen:
                seen.add(norm)
                result.append(c)
        return result

    def _resolve_gamedata_role_marker(self, marker): #vers 1
        """Resolve a real "<gamedata-role>/<role>" marker (Aug 20
        2026,  "../data/waterpro.dat - for LC and VC, but
        SA map looks for ../data/water.dat") - each real role tries
        every real filename that role's own real game families
        actually use, in order, returning the first that genuinely
        exists in whichever game's own real data folder is actually
        current. "water": waterpro.dat (III/VC/SOL's own real binary
        format) then water.dat (SA's own real text format) - the two
        real formats are confirmed completely different (see
        WaterProFile/WaterShape's own docstrings in gta_dat_parser.py)
        but both fill the exact same real role. "timecyc": timecyc.
        dat - the one real filename every game family already shares,
        kept as its own real role here for a consistent marker shape
        rather than a special case. Returns the real, resolved path,
        or None if that role's own file doesn't exist in any real
        candidate folder for the current game."""
        if not marker.startswith('<gamedata-role>/'):
            return None
        role = marker[len('<gamedata-role>/'):]
        filenames = {
            'water': ['waterpro.dat', 'water.dat'],
            'timecyc': ['timecyc.dat'],
        }.get(role, [])
        # Case-insensitive fix (Aug 20 2026  own confirmed
        # `ls -la`: VC's own real waterpro.dat is genuinely named
        # WATERPRO.DAT on disk - a plain os.path.join+isfile only
        # tries the one, exact given case, correct on Windows' own
        # case-insensitive filesystem but silently fails on a real,
        # case-sensitive Linux one) - _resolve_ci is the same real,
        # already-existing helper gta_dat_parser.py's own SOL support
        # already relies on for this exact same real problem.
        from apps.methods.gta_dat_parser import _resolve_ci
        for folder in self._game_data_folder_candidates():
            for filename in filenames:
                candidate = _resolve_ci(folder, filename)
                if candidate:
                    return candidate
        return None

    def _to_gamedata_role_marker(self, path): #vers 1
        """Normalise a saved preload entry to a real "<gamedata-role>/
        <role>" marker if its own filename is one of the 3 real, per-
        game roles - self-healing fix (Aug 20 2026,  "the VC
        waterpro.dat shows across SA and LC, so this needs fixing") -
        a pick saved before the marker fix existed is still a real,
        plain absolute path in my own real, already-persisted
        settings file; that old fix only changed how new saves are
        written, it never migrated what was already there, so the old
        real, stale absolute path kept right on loading regardless of
        which game was actually current. Treats any saved entry whose
        own filename matches a real role the same way now, whether
        it's already the new marker or still a real legacy absolute
        path - passing an already-normalised marker straight through
        unchanged. Returns the input path unchanged if it isn't a
        recognised per-game role at all."""
        if path.startswith('<gamedata-role>/'):
            return path
        name = os.path.basename(path).lower()
        if name in ('waterpro.dat', 'water.dat'):
            return '<gamedata-role>/water'
        if name == 'timecyc.dat':
            return '<gamedata-role>/timecyc'
        return path

    def _auto_detect_timecyc_path(self): #vers 4
        """Look for a real timecyc.dat next to the currently loaded
        game's own real data folder (Aug 20 2026,  "we need
        to be able to detect the timecyc.dat file without the need
        for a path, and still keep the toggle" - "show from gameroot/
        data/timecyc.dat"). Case-insensitive match (real installs
        vary). Returns the real path if found, else ''.

         SOL fix (Aug 20 2026,  "with GTASOL looking
        for /sol/gta_sol.dat the waterpro.dat would be in gameroot/
        data/waterpro.dat" - the identical real gap applies to
        timecyc.dat too) - the folder self._loaded_dat_path's own
        directory gives is gameroot/sol for SOL specifically, not
        gameroot/data, since that's genuinely where SOL's own main
        .dat file lives. Now uses _game_data_folder_candidates' own
        real, shared SOL-aware logic rather than a second, separate
        copy of it.

         fallback added (Aug 20 2026,  "we also have
        the project profiles to fall back on") - if the loaded
        world's own folder doesn't have it, also tries self.main_
        window.game_root/data - the real, already-existing Project
        Manager sets main_window.game_root when a project is
        activated (project_manager.py's own create_project/load
        flow), a second, real source of "where this game actually
        lives" independent of whatever .dat file happened to be
        loaded this session.

         fix (Aug 21 2026  own real, uploaded
        timecyc.dat and timecycp.dat files: "timecyc.dat and
        timecycp.dat don't seem to work when SA is loaded?") - this
        only ever looked for "timecyc.dat", never "timecycp.dat" at
        all - SA installs that ship only the newer, decimal-precision
        timecycp.dat (TimecycParser's own real _detect_game already
        special-cases that exact filename, so it was already
        expecting to see it) had genuinely nothing to auto-detect at
        all. Now checks both real filenames, preferring timecycp.dat
        when both exist in the same real folder (the newer, more
        precise real format) - falls back to timecyc.dat otherwise."""
        for folder in self._game_data_folder_candidates():
            try:
                names = {n.lower(): n for n in os.listdir(folder)}
            except OSError:
                continue
            if 'timecycp.dat' in names:
                return os.path.join(folder, names['timecycp.dat'])
            if 'timecyc.dat' in names:
                return os.path.join(folder, names['timecyc.dat'])
        return ''

    def _load_water_texture_from_particle_txd(self): #vers 1
        """Read the currently loaded game's own real water texture
        directly from particle.txd (Aug 20 2026,  "Next are
        the ../model/particle.txd water textures; you have the
        screenshots to fall back on") - same real, already-working
        ModelCache.get_textures/parse_txd pipeline _load_radar_tex_
        tiles already uses (any RW format decoded to plain RGBA
        automatically). Tries every real name confirmed directly from
        my own screenshots: "waterclear256" (VC's own real 256x256
        DXT1, and SA's own real, differently-sized/formatted 128x128
        ARGB8888 - same name, two genuinely different real files, per-
        game, not a conflict since only one game's own particle.txd is
        ever loaded at a time) and "water_old" (LC's own real 128x128
        PAL8). Returns (rgba_bytes, width, height) for the first real
        match found, or None if this game's own particle.txd doesn't
        have any of them."""
        model_cache = getattr(self, '_model_cache', None)
        if model_cache is None:
            return None
        textures = model_cache.get_textures('particle')
        if not textures:
            return None
        for name in ('waterclear256', 'water_old'):
            tex = textures.get(name)
            if tex is None:
                continue
            rgba = tex.get('rgba_data')
            w, h = tex.get('width'), tex.get('height')
            if rgba and w and h:
                return (rgba, w, h)
        return None

    def _load_radar_tex_tiles(self, game_key): #vers 2
        """Read every real radarNN.txd texture for game_key directly
        from whichever IMG archive is already loaded (via ModelCache -
        the same index used for models/collision),  "those
        radar.txd files are in the gta3... unless it's SOL where
        they're in another file" - no separate tiles folder needed,
        this reads the game's own real, already-indexed archive.
        Naming per RADAR_GRID_PRESETS' own tile count: RADAR00-NN
        (uppercase, 2-digit) for SA/VC/III, radar0000-NNNN (lowercase,
        4-digit) for SOL - the same real naming radar_workshop.py's
        own _name_sa/_name_sol already use. Returns a list of
        (rgba_bytes, width, height) or None per tile, in tile-index
        order, ready for DFFViewport.set_radar_tex_layer."""
        from apps.methods.gta_dat_parser import RADAR_GRID_PRESETS
        preset = RADAR_GRID_PRESETS.get(game_key, RADAR_GRID_PRESETS['sa'])
        tile_count = preset['tiles_per_side'] ** 2
        model_cache = getattr(self, '_model_cache', None)
        if model_cache is None:
            return []
        result = []
        for i in range(tile_count):
            if game_key == 'iv':
                name = f"radar{i}"          # IV radar.img: radar0-radar143
            else:
                name = f"radar{i:04d}" if game_key == 'sol' else f"RADAR{i:02d}"
            textures = model_cache.get_textures(name)
            if not textures:
                result.append(None)
                continue
            tex = next(iter(textures.values()))
            rgba = tex.get('rgba_data')
            w, h = tex.get('width'), tex.get('height')
            if rgba and w and h:
                result.append((rgba, w, h))
            else:
                result.append(None)
        return result




    def _preload_img_archives_to_os_cache(self, img_paths): #vers 1
        """Sequentially read through every given IMG archive once,
        discarding the bytes - the only purpose is letting the OS
        cache the file in RAM ahead of time, so later per-model reads
        during actual IPL loading hit cache instead of disk (Aug 16
        2026)"""
        CHUNK = 8 * 1024 * 1024
        for img_path in img_paths:
            try:
                size = os.path.getsize(img_path)
            except OSError:
                continue
            if size <= 0:
                continue
            name = os.path.basename(img_path)
            size_mb = size / (1024 * 1024)
            try:
                with open(img_path, 'rb') as f:
                    read_so_far = 0
                    while True:
                        chunk = f.read(CHUNK)
                        if not chunk:
                            break
                        read_so_far += len(chunk)
                        pct = int(100 * read_so_far / size)
                        self._set_status(
                            f"Preloading {name} ({size_mb:.1f} MB)... {pct}%")
                        QApplication.processEvents()
            except OSError as e:
                self._set_status(f"Preloading {name} failed: {e}")
                QApplication.processEvents()
                continue
        self._set_status("Ready")

    def _load_workers(self) -> int: #vers 1
        """Parallel parse workers from settings; 0 means cores - 2."""
        import os
        n = int(self.map_settings.get('load_workers') or 0)
        return n if n > 0 else max(1, (os.cpu_count() or 1) - 2)

    def _load_models_parallel(self, dlg, model_cache, items, workers,
                              load_models, load_textures): #vers 2
        """Parse all models/TXDs in worker processes; rows update as each finishes."""
        rows, by_txd, missing = {}, {}, []
        for model_name, txd_name in items:
            rows[model_name.lower()] = (dlg.model_row(model_name), txd_name if load_textures else "")
            if load_textures and txd_name:
                by_txd.setdefault(txd_name.lower(), []).append(model_name.lower())
                if not model_cache.is_txd_indexed(txd_name):
                    missing.append(f"{txd_name}.txd")
            if load_models and not model_cache.is_dff_indexed(model_name):
                missing.append(f"{model_name}.dff")
        dff_done, txd_done = set(), {}
        total = len(items) * (2 if load_textures else 1)
        state = {'n': 0}

        def _row_state(key): #vers 1
            row, txd = rows[key]
            need_dff = load_models and key not in dff_done
            need_txd = bool(txd) and txd.lower() not in txd_done
            if need_dff:
                return
            if need_txd:
                dlg.set_model(row, 60)
            else:
                count = txd_done.get(txd.lower()) if txd else None
                dlg.set_model(row, 100, f"{count} ({txd})" if txd else "")

        def _done(kind, name, result): #vers 1
            state['n'] += 1
            dlg.set_file_progress(state['n'], total)
            if kind == 'dff':
                dff_done.add(name)
                if name in rows:
                    _row_state(name)
            else:
                txd_done[name] = len(result) if result else 0
                for key in by_txd.get(name, []):
                    _row_state(key)

        dlg.message(f"Parsing with {workers} worker processes")
        model_cache.prefetch([m for m, _ in items] if load_models else [],
                             [t for _, t in items if t] if load_textures else [],
                             workers, on_done=_done, is_cancelled=lambda: dlg.cancelled,
                             on_idle=lambda: dlg._pump(),
                             on_stall=lambda n: dlg.message(
                                 f"Workers stopped responding - parsing last {n} here, one at a time"))
        if dlg.cancelled:
            return list(dict.fromkeys(missing))
        dff_done.update(rows)                 # already-cached items got no callback
        for txd in by_txd:
            if txd not in txd_done:
                tex = model_cache.get_textures(txd)
                txd_done[txd] = len(tex) if tex else 0
        for key in rows:
            _row_state(key)
        dlg.set_file_progress(1, 1)
        return list(dict.fromkeys(missing))

    def _load_models_into_dialog(self, dlg, loader, model_cache, instances,
                                 load_models=True, load_textures=True): #vers 2
        """Load each unique model's DFF/TXD with a dialog row each; returns missing files."""
        seen = {}
        for inst in instances:
            if inst.model_name not in seen:
                obj = loader.get_object(inst.model_id)
                seen[inst.model_name] = obj.txd_name if obj else ""
        items = list(seen.items())
        workers = self._load_workers()
        if workers > 1 and len(items) > 4 and hasattr(model_cache, 'prefetch'):
            return self._load_models_parallel(dlg, model_cache, items, workers,
                                              load_models, load_textures)
        missing, tex_counts = [], {}
        for i, (model_name, txd_name) in enumerate(items):
            if dlg.cancelled:
                self._set_status(f"Model loading cancelled after {i} of {len(items)} - "
                                 f"the rest load on demand while browsing")
                break
            row = dlg.model_row(model_name)
            if load_models:
                if not model_cache.is_dff_indexed(model_name):
                    missing.append(f"{model_name}.dff")
                model_cache.get_geometry(model_name)
            tex_text = ""
            if load_textures and txd_name:
                dlg.set_model(row, 60)
                if not model_cache.is_txd_indexed(txd_name):
                    missing.append(f"{txd_name}.txd")
                if txd_name not in tex_counts:
                    tex = model_cache.get_textures(txd_name)
                    tex_counts[txd_name] = len(tex) if tex else 0
                tex_text = f"{tex_counts[txd_name]} ({txd_name})"
            dlg.set_model(row, 100, tex_text)
            dlg.set_file_progress(i + 1, len(items))
        return list(dict.fromkeys(missing))

    def _rotate_and_translate_offset(self, ox, oy, oz, qx, qy, qz, qw, px, py, pz): #vers 1
        """Rotate a local-space offset (ox,oy,oz) by quaternion
        (qx,qy,qz,qw) then translate by (px,py,pz) - standard
        quaternion-vector rotation formula
        (v' = v + 2*cross(q.xyz, cross(q.xyz, v) + q.w*v)), used for
        2DFX light offsets (see _refresh_2dfx_lights) since they're
        defined in their owning instance's local space and need to
        rotate with it, not just translate."""
        cross1_x = qy * oz - qz * oy
        cross1_y = qz * ox - qx * oz
        cross1_z = qx * oy - qy * ox
        t_x = cross1_x + qw * ox
        t_y = cross1_y + qw * oy
        t_z = cross1_z + qw * oz
        cross2_x = qy * t_z - qz * t_y
        cross2_y = qz * t_x - qx * t_z
        cross2_z = qx * t_y - qy * t_x
        return (ox + 2 * cross2_x + px,
                oy + 2 * cross2_y + py,
                oz + 2 * cross2_z + pz)

    def _effective_rotation(self, inst): #vers 2
        """Return the quaternion (x,y,z,w) to actually use for
        rendering/display, applying a conjugate (negate x,y,z; keep
        w) - universally, not just for SA/SOL (see _conjugate_
        rotation_for_game for the VC confirmation that widened this)."""
        return self._conjugate_rotation_for_game(
            inst.rot_x, inst.rot_y, inst.rot_z, inst.rot_w)

    def _conjugate_rotation_for_game(self, x, y, z, w): #vers 2
        """Raw quaternion in/out version of the same conjugate
        _effective_rotation applies, for callers that don't have an
        IPLInstance handy (specifically _InstanceEditPanel's rotation
        spin boxes, which need this in both directions - display
        reads the stored quaternion and needs the effective one;
        editing needs the reverse, going from an edited effective
        value back to what should actually be stored). A conjugate is
        its own inverse (applying it twice returns the original), so
        the same operation works both ways.

        Applied universally now, not just SA/SOL (Aug 1 2026)"""
        return (-x, -y, -z, w)

    def _rotate_vector_by_quaternion(self, vx, vy, vz, x, y, z, w): #vers 1
        """Rotate a local-space vector (vx,vy,vz) by quaternion
        (x,y,z,w), returning the world-space (rx,ry,rz) result (Aug
        16 2026)"""
        xx, yy, zz = x*x, y*y, z*z
        xy, xz, yz = x*y, x*z, y*z
        wx, wy, wz = w*x, w*y, w*z
        rx = (1.0-2.0*(yy+zz))*vx + 2.0*(xy-wz)*vy + 2.0*(xz+wy)*vz
        ry = 2.0*(xy+wz)*vx + (1.0-2.0*(xx+zz))*vy + 2.0*(yz-wx)*vz
        rz = 2.0*(xz-wy)*vx + 2.0*(yz+wx)*vy + (1.0-2.0*(xx+yy))*vz
        return rx, ry, rz

    def _model_collision(self, model_cache, model_name): #vers 1
        """(col_vertices, col_triangles) from COL, else GTA IV .wbd bounds."""
        cv, ct = self._convert_collision_geometry(model_cache.get_collision(model_name))
        if not ct and hasattr(model_cache, 'get_iv_collision'):
            iv = model_cache.get_iv_collision(model_name)
            if iv:
                cv, ct = iv
        return cv, ct

    def _iv_static_bound_entries(self, model_cache, instances): #vers 1
        """World-space GTA IV .wbn collision entries for the visible map areas."""
        if not hasattr(model_cache, 'iv_static_bound_names'):
            return []
        areas = set()
        for name in {getattr(i, 'source_ipl', '') for i in instances}:
            stem = os.path.splitext(name)[0].lower()
            for tag in ('_stream', '_strbig'):
                if tag in stem:
                    stem = stem.rsplit(tag, 1)[0]
                    break
            areas.add(stem)
        entries = []
        for area in sorted(areas):
            for stem in model_cache.iv_static_bound_names(area):
                bounds = model_cache.get_iv_static_bounds(stem)
                if not bounds:
                    continue
                entries.append({'vertices': [], 'normals': [], 'uvs': [], 'triangles': [],
                                'materials': [], 'prelit': [], 'geom_flags': 0,
                                'col_vertices': bounds[0], 'col_triangles': bounds[1],
                                'pos': (0.0, 0.0, 0.0), 'rot': (0.0, 0.0, 0.0, 1.0),
                                'scale': (1.0, 1.0, 1.0), 'model_key': f"wbn:{stem}",
                                'instance': None})
        return entries

    def _convert_collision_geometry(self, col_model): #vers 1
        """Convert a COLModel (or None) into the flat (col_vertices,
        col_triangles) shape DFFViewport's collision overlay draws
        expect (Aug 14 2026)"""
        if col_model is None or not col_model.faces or not col_model.vertices:
            return [], []
        from apps.methods.col_materials import get_material_colour, COLGame
        ver = getattr(getattr(col_model, 'header', None), 'version', None)
        game = COLGame.VC if getattr(ver, 'value', 3) == 1 else COLGame.SA
        col_vertices = [(v.x, v.y, v.z) for v in col_model.vertices]
        col_triangles = []
        for f in col_model.faces:
            try:
                hexcol = get_material_colour(f.material, game)
                r = int(hexcol[0:2], 16) / 255.0
                g = int(hexcol[2:4], 16) / 255.0
                b = int(hexcol[4:6], 16) / 255.0
            except Exception:
                r = g = b = 0.7
            col_triangles.append((f.a, f.b, f.c, r, g, b))
        return col_vertices, col_triangles

    def _refresh_world_view(self, instances, auto_fit=True, clear_display_lists=True): #vers 4
        """Push a full multi-instance 3D world view into the existing
        DFF viewport (self.preview_widget) - Aug 1 2026)"""
        if getattr(self, '_refresh_world_view_in_progress', False):
            return
        self._refresh_world_view_in_progress = True
        try:
            self._refresh_world_view_impl(instances, auto_fit, clear_display_lists)
        finally:
            self._refresh_world_view_in_progress = False

    def _refresh_world_view_impl(self, instances, auto_fit, clear_display_lists): #vers 7
        """The actual body of _refresh_world_view, split out only so
        the reentrancy guard above can wrap it in a try/finally
        without a second level of indentation across this whole
        (long) method - not meant to be called directly by anything
        else; call _refresh_world_view instead."""
        vp = getattr(self, 'preview_widget', None)
        if vp is None or not hasattr(vp, 'set_world_instances'):
            return
        if not instances:
            if vp is not None and hasattr(vp, 'clear_world_instances'):
                vp.clear_world_instances()
            return

        # Dots render mode (Aug 20 2026)
        if getattr(vp, '_mode', 'textured') == 'dots':
            dots_entries = [
                {'pos': (inst.pos_x, inst.pos_y, inst.pos_z),
                 'rot': self._effective_rotation(inst),
                 'scale': (1.0, 1.0, 1.0),
                 'model_key': inst.model_id}
                for inst in instances
            ]
            vp.set_world_instances(dots_entries, auto_fit=auto_fit,
                                   clear_display_lists=clear_display_lists)
            return

        model_cache = getattr(self, '_model_cache', None)
        loader = getattr(self, '_world_loader', None)
        if model_cache is None:
            return

        # Convert each distinct model's geometry once, reuse across
        # instances AND across calls (Aug 1 2026)
        converted = self._geometry_conversion_cache = getattr(
            self, '_geometry_conversion_cache', {})
        entries = []
        all_textures = []
        seen_txds = set()

        # Preload every TXD referenced by generic.ide, unconditionally,
        # first (Aug 1 2026)
        generic_ide_textures, generic_ide_txds = self._preload_generic_ide_textures()
        all_textures.extend(generic_ide_textures)
        seen_txds.update(generic_ide_txds)

        for inst in instances:
            model_name = inst.model_name
            if model_name not in converted:
                # Status feedback for newly-converted models (Aug 1 2026)
                txd_name_hint = ""
                if loader is not None:
                    hint_obj = loader.get_object(inst.model_id)
                    if hint_obj is not None and hint_obj.txd_name:
                        txd_name_hint = f" (texture: {hint_obj.txd_name})"
                self._set_status(f"Loading model: {model_name}{txd_name_hint}...")
                QApplication.processEvents()
                dff_model = model_cache.get_geometry(model_name)
                if dff_model is None or not getattr(dff_model, 'geometries', None):
                    # Model geometry missing/unparsed - still attempt
                    # collision independently by name (Sep 5 2026, per
                    #  : "it should still find the model name in
                    # the col, not just the img file, and show them").
                    # Previously this set converted[model_name] = None
                    # outright, which skipped the whole instance below
                    # (if base is None: continue) - meaning collision
                    # was never even looked up for any model whose own
                    # DFF failed to resolve, even though collision is
                    # indexed and resolved completely independently of
                    # geometry. A None-geometry entry with real
                    # col_vertices/col_triangles still lets this
                    # instance show up whenever a collision render
                    # mode is on, via _draw_world_instances' own
                    # self._mode is None handling for "no model style
                    # selected" - the same underlying mechanism, just
                    # reached because THIS model has no geometry
                    # rather than because the user deselected one.
                    col_vertices, col_triangles = self._model_collision(model_cache, model_name)
                    if col_vertices and col_triangles:
                        converted[model_name] = {
                            'vertices': [], 'normals': [], 'uvs': [],
                            'triangles': [], 'materials': [], 'prelit': [],
                            'geom_flags': 0,
                            'col_vertices': col_vertices,
                            'col_triangles': col_triangles,
                        }
                    else:
                        converted[model_name] = None
                else:
                    # Merge every geometry, not just the first (Aug 1 2026)
                    all_vertices, all_normals, all_uvs, all_prelit = [], [], [], []
                    all_triangles, all_materials = [], []
                    any_has_colors = any(getattr(g, 'colors', None) for g in dff_model.geometries)
                    for g in dff_model.geometries:
                        v_offset = len(all_vertices)
                        mat_offset = len(all_materials)
                        all_vertices.extend((v.x, v.y, v.z) for v in g.vertices)
                        if g.normals:
                            all_normals.extend((n.x, n.y, n.z) for n in g.normals)
                        if g.uv_layers:
                            all_uvs.extend((u.u, u.v) for u in g.uv_layers[0])

                        if getattr(g, 'colors', None):
                            all_prelit.extend((c.r, c.g, c.b, c.a) for c in g.colors)
                        elif any_has_colors:
                            all_prelit.extend((255, 255, 255, 255) for _ in g.vertices)
                        all_triangles.extend(
                            (t.v1 + v_offset, t.v2 + v_offset, t.v3 + v_offset,
                             t.material_id + mat_offset)
                            for t in g.triangles)
                        all_materials.extend(g.materials)
                    col_vertices, col_triangles = self._model_collision(model_cache, model_name)
                    # IV .wdr textures embedded in the model
                    all_textures.extend(getattr(dff_model, 'embedded_textures', {}).values())
                    converted[model_name] = {
                        'vertices':  all_vertices,
                        'normals':   all_normals,
                        'uvs':       all_uvs,
                        'triangles': all_triangles,
                        'materials': all_materials,
                        'prelit':    all_prelit,
                        #  fix (Aug 21 2026  own real,
                        # uploaded alpha_showing.png screenshot: "some
                        # alpha objects not being rendered as they
                        # should be") - this real model's own real
                        # geometry flags (rpGEOMETRYLIGHT etc.) were
                        # never captured at all here, so every world
                        # instance's own display list built with
                        # whatever self._current_geom_flags happened
                        # to still be set to from unrelated, earlier
                        # single-model editing - genuinely never this
                        # model's own real flags. The alpha-test cutout
                        # shape itself was already correct (GL_ALPHA_
                        # TEST was already real and working), but a
                        # real, wrong lighting flag left real foliage
                        # unlit/mislit dark instead of showing its own
                        # real, green cutout texture. First geometry's
                        # own flags used (matching the same real
                        # convention already used elsewhere in this
                        # codebase for single-model display, e.g.
                        # DFFViewport.load_all_geometries).
                        'geom_flags': getattr(dff_model.geometries[0], 'flags', 0),
                        'col_vertices':  col_vertices,
                        'col_triangles': col_triangles,
                    }
                if loader is not None:
                    obj = loader.get_object(inst.model_id)
                    txd_name = obj.txd_name if (obj is not None and obj.txd_name) else None
                    if txd_name and txd_name.lower() not in seen_txds:
                        seen_txds.add(txd_name.lower())

                        textures, _src, _status = self._get_txd_textures(txd_name)
                        if textures:
                            all_textures.extend(textures.values())
            base = converted[model_name]
            if base is None:
                continue   # model failed to load/parse - skip this instance
            entry = dict(base)
            entry['pos']   = (inst.pos_x, inst.pos_y, inst.pos_z)
            entry['rot']   = self._effective_rotation(inst)
            entry['scale'] = self._render_scale(inst)
            entry['model_key'] = model_name
            entry['instance'] = inst
            entries.append(entry)

        if all_textures and hasattr(vp, '_upload_textures'):
            # additive mirrors clear_display_lists exactly (Aug 1 2026)

            vp._upload_textures(all_textures, additive=True)
        # Only force the default render mode once, on the very first
        # world load (Aug 1 2026)
        if hasattr(vp, 'set_render_mode') and not getattr(self, '_world_render_mode_set', False):
            vp.set_render_mode('textured')
            self._world_render_mode_set = True
        if getattr(loader, 'game', None) == 'iv':
            entries.extend(self._iv_static_bound_entries(model_cache, instances))
        vp.set_world_instances(entries, auto_fit=auto_fit, clear_display_lists=clear_display_lists)
        self._populate_models_panel_from_ipl(instances)

        self._set_status("Ready")

    def _populate_models_panel_from_ipl(self, instances): #vers 1
        """List every distinct model referenced by the currently
        loaded/visible instances in the Models dock (self.mod_compact_
        list), one row per model (Details column: model name + how
        many instances use it)"""
        table = getattr(self, 'mod_compact_list', None)
        if table is None:
            return
        loader = getattr(self, '_world_loader', None)

        counts = {}
        order = []
        for inst in instances:
            if inst.model_name not in counts:
                order.append(inst.model_name)
                counts[inst.model_name] = 0
            counts[inst.model_name] += 1

        self._ipl_models_mode = True
        self._ipl_model_names = order

        table.itemSelectionChanged.disconnect(self._on_compact_col_selected)
        try:
            table.setRowCount(len(order))
            for row, model_name in enumerate(order):
                preview_item = QTableWidgetItem("")
                table.setItem(row, 0, preview_item)
                detail_item = QTableWidgetItem(
                    f"{model_name}\n{counts[model_name]} instance"
                    f"{'s' if counts[model_name] != 1 else ''}")
                table.setItem(row, 1, detail_item)
        finally:
            table.itemSelectionChanged.connect(self._on_compact_col_selected)

    def _on_ipl_model_row_selected(self, row): #vers 2
        """Show one model's info (Model Name/IDE/ID/TXD fields) and
        its textures (self._tex_list) - the highlighted models get
        there textures shown in the pane below."""
        names = getattr(self, '_ipl_model_names', [])
        if row < 0 or row >= len(names):
            return
        model_name = names[row]
        loader = getattr(self, '_world_loader', None)
        model_cache = getattr(self, '_model_cache', None)

        if hasattr(self, 'info_name'):
            self.info_name.setText(model_name)

        txd_name = ""
        if loader is not None:
            for inst in getattr(self, '_all_instances', []):
                if inst.model_name == model_name:
                    obj = loader.get_object(inst.model_id)
                    if obj is not None:
                        if hasattr(self, 'info_ide_section'):
                            self.info_ide_section.setText(obj.section or 'object')
                        if hasattr(self, 'info_model_id'):
                            self.info_model_id.setText(f"ID: {inst.model_id}")
                        txd_name = obj.txd_name or ""
                    break
        if hasattr(self, 'info_txd_name'):
            self.info_txd_name.setText(txd_name or '—')

        tex_list = getattr(self, '_tex_list', None)
        if tex_list is None:
            return
        if not txd_name:
            tex_list.setRowCount(0)
            if hasattr(self, '_tex_count_lbl'):
                self._tex_count_lbl.setText(f"No TXD name found for {model_name}")
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
                    f"({txd_name}.txd, from {source})")
            else:
                self._tex_count_lbl.setText(
                    f"{txd_name}.txd not found in any indexed IMG archive or as a loose file")
