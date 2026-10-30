#this belongs in apps/components/Terrain_Workshop/depends/terrain_panel_func.py - Version: 5
# X-Seti - October 28 2026 - IMG Factory 1.6 - Terrain Workshop environment panel

"""
One Environment dock (methods/env_panel.py): icon rail with Sculpt, Water,
Painting, Nature, Paths, Assets and Setup pages. Picking a page sets the
brush (sculpt tools / paint).
"""

##class TerrainPanelMixin: -
# _apply_loaded_world_impl
# _create_tw_env_dock
# _tw_page_changed

from PyQt6.QtWidgets import QDockWidget


class TerrainPanelMixin: #vers 1
    """Environment dock for TerrainWorkshop."""

    def _create_tw_env_dock(self): #vers 4
        """Build every page into one icon-rail panel; returns the dock."""
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons
        from apps.methods.env_panel import EnvPanel
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as S
        ic = self._get_icon_color()
        setup = self._build_tw_setup_page()                  # makes the brush first
        env = EnvPanel()
        env.add_page('sculpt', OverlayIcons.terrain_icon(28), "Environment", "Terrain", self._build_tw_sculpt_page())
        env.add_page('water', OverlayIcons.water_icon(28), "Environment", "Water", self._build_tw_water_page())
        env.add_page('paint', S.paint_texture_icon(28, ic), "Environment", "Painting", self._build_tw_paint_page())
        env.add_page('nature', S.surface_grass_icon(28, ic), "Environment", "Nature", self._build_tw_nature_page())
        env.add_page('paths', OverlayIcons.paths_icon(28), "Paths", "Select type", self._build_tw_paths_page())
        env.add_page('assets', S.box_icon(28, ic), "Scenery", "Assets", self._build_tw_assets_page())
        env.add_page('setup', S.settings_icon(28, ic), "Setup", "New, open, save, export", setup)
        env.pageChanged.connect(self._tw_page_changed)
        self._tw_env = env
        self._tw_set_library(self._tw_library)               # fills Painting, Paths and Nature tiles
        self._tw_game_changed()                              # radar button for the export game
        dock = QDockWidget("Environment", self)
        dock.setObjectName("TerrainEnv")
        dock.setWidget(env)
        dock.setMinimumWidth(300)
        self._tw_env_dock = dock
        return dock

    def _apply_loaded_world_impl(self, loader, game, ok, source_desc): #vers 4
        """Game world loaded (Map Workshop): export game follows it, radar re-checked."""
        result = super()._apply_loaded_world_impl(loader, game, ok, source_desc)
        self._tw_assets_refresh()                            # its objects in the Assets browser
        self._tw_nature_fill()                               # and its plants on Nature
        self._tw_water_info_update()                         # its water file
        if self._tw_terrain is not None:
            self._tw_assets_push()
        key = getattr(loader, 'game', None)
        i = self._tw_game.findData(key)
        if i >= 0:
            self._tw_game.setCurrentIndex(i)                 # also runs _tw_game_changed
        else:
            self._tw_game_changed()
        return result

    def _tw_page_changed(self, key: str): #vers 5
        """Sculpt: last sculpt tool. Painting: paint. Paths: path strips. Water: water brush."""
        if key == 'sculpt':
            mode = getattr(self, '_tw_last_sculpt', 'raise')
            self._tw_sculpt_tiles.set_current(mode)
            self._tw_set_mode(mode)
        elif key == 'water':
            self._tw_set_mode(self._tw_water_tiles.current() or 'water')
        elif key == 'nature' and self._tw_nature_pick is not None:
            self._tw_set_mode('nature')
        elif key == 'paint':
            self._tw_set_mode('paint')
        elif key == 'paths':
            self._tw_set_mode('path')
        elif key == 'assets' and self._tw_asset_model is not None:
            self._tw_set_mode('place')
