#!/usr/bin/env python3
#this belongs in apps/components/Retro_GTA_Workshop/retro_gta_workshop.py - Version: 23
# X-Seti - October 23 2026 - IMG Factory 1.6 - Retro GTA Workshop
# Built on GUIWorkshop base (same as Capture / Water Workshop)

"""
Retro GTA Workshop - all-in-one editor for the top-down GTAs. GTA1
(.cmp maps, .g24 / .gry styles) and GTA2 (.gmp maps, .sty styles): game folder
maps with their styles, PS1 GTA1 city .vsr (map and style in one),
PS1 GTA2 .gmp / .sty, GTA1 .fxt / GTA2 .gxt text, mission / script markers, PC <-> PS1 block port, 3D export, media (sounds, PS1 images / video), zones / lights / objects editing,
top-down map view, block / face editor, lid
painting, tiles (DP5 edit and back), sprites.
"""

##Methods list -
# open_retro_gta_workshop
#
##class RetroGTAWorkshop: -
# __init__
# _build_ribbons
# _create_files_dock
# _extra_settings_tab
# _hover
# _map_picked
# _open_disc
# _open_file
# _open_game_folder
# _open_map
# _open_style
# _read_map
# _read_style
# _save_map
# _save_file
# _save_style
# _save_toolbar_state
# _set_tool
# _show_folder
# _style_for
# _toolbar_context_menu
# _undo
# _update_title
# _vsr_city
# closeEvent
# setup_ui
#
##Mixins (depends/) -
# RetroBlockMixin - retro_block_func.py
# RetroMapDataMixin - retro_mapdata_func.py
# RetroMapImageMixin - retro_mapimage_func.py
# RetroMediaMixin - retro_media_func.py
# RetroMeshMixin - retro_mesh_func.py
# RetroMissionMixin - retro_mission_func.py
# RetroPortMixin - retro_port_func.py
# RetroTextMixin - retro_text_func.py
# RetroTilesMixin - retro_tiles_func.py
# RetroXRefMixin - retro_xref_func.py

import configparser
import os
import struct
import sys
from pathlib import Path

_root = Path(__file__).resolve().parents[3]       # project root for standalone runs
if str(_root) not in sys.path:
    sys.path.insert(0, str(_root))

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (QApplication, QComboBox, QFileDialog, QHBoxLayout, QLabel, QListWidget, QListWidgetItem,
                             QMessageBox, QPushButton, QTabWidget, QVBoxLayout, QWidget)

from apps.methods.statusbar_func import register_statusbar
from apps.methods.gta1_map import GTA1Map
from apps.methods.gta1_psx import GTA1PSXMap, GTA1PSXStyle
from apps.methods.gta1_style import GTA1Style
from apps.methods.gta2_map import GTA2Map
from apps.methods.gta2_psx import GTA2PSXMap, GTA2PSXStyle
from apps.methods.gta2_style import GTA2Style
from apps.methods.vsr_archive import VSRArchive
from apps.methods.gui_workshop import GUIWorkshop
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.ribbon_dialog import RibbonIconsMixin
from apps.methods.ribbon_system import RibbonMixin
from apps.components.Retro_GTA_Workshop.depends.retro_block_func import RetroBlockMixin
from apps.components.Retro_GTA_Workshop.depends.retro_mapview import MapView
from apps.components.Retro_GTA_Workshop.depends.retro_mapdata_func import RetroMapDataMixin
from apps.components.Retro_GTA_Workshop.depends.retro_mapimage_func import RetroMapImageMixin
from apps.components.Retro_GTA_Workshop.depends.retro_media_func import MEDIA_EXTS, RetroMediaMixin
from apps.components.Retro_GTA_Workshop.depends.retro_mesh_func import RetroMeshMixin
from apps.components.Retro_GTA_Workshop.depends.retro_mission_func import RetroMissionMixin
from apps.components.Retro_GTA_Workshop.depends.retro_port_func import RetroPortMixin
from apps.components.Retro_GTA_Workshop.depends.retro_text_func import TEXT_EXTS, RetroTextMixin
from apps.components.Retro_GTA_Workshop.depends.retro_xref_func import RetroXRefMixin
from apps.components.Retro_GTA_Workshop.depends.retro_tiles_func import RetroTilesMixin

App_name = "Retro GTA Workshop"
Build = "Build 1"


class RetroGTAWorkshop(RetroBlockMixin, RetroMapDataMixin, RetroMapImageMixin, RetroMediaMixin, RetroMeshMixin, RetroMissionMixin, RetroPortMixin, RetroTilesMixin,
                       RetroTextMixin, RetroXRefMixin, RibbonIconsMixin, RibbonMixin, GUIWorkshop): #vers 7

    App_name        = App_name
    App_build       = Build
    App_author      = "X-Seti"
    App_year        = "2026"
    App_description = ("All-in-one editor for the top-down GTAs.\n"
                       "GTA1 (.cmp / .g24 / .gry) and GTA2 (.gmp / .sty): maps, tiles and sprites.")
    App_icon        = "retro_gta_app_icon"
    config_key      = "retro_gta_workshop"
    WS_DEFAULTS     = {"game_folder": "", "last_map": "", "full_paths": False}
    DROP_EXTS       = ('.gmp', '.sty', '.cmp', '.g24', '.gry', '.vsr', '.fxt', '.gxt', '.sdt', '.str', '.wav')
    _ribbon_name    = "retro_gta_workshop"
    _RIBBON_LAYOUT_VERSION = 8          # New blank map, Save as, Data overlay
    _RIBBON_WIDGET_ITEMS = True         # Ribbon Manager saves moved ribbon buttons

    def __init__(self, parent=None, main_window=None): #vers 2
        self._map = self._style = None
        self._map_dirty = self._style_dirty = False
        self._data_dir, self._mmp = '', {}
        super().__init__(parent, main_window)
        self.ribbon_restore_state()
        self._apply_ribbon_layout()                     # Ribbon Manager moves / hidden buttons
        folder = self.WS.get("game_folder", "")
        if folder and os.path.isdir(folder):
            self._open_game_folder(folder)

    def setup_ui(self): #vers 7
        """Titlebar / Files dock | ribbons + Map, Sprites, Text, Media, Data tabs | Block and Tiles docks."""
        ml = QVBoxLayout(self)
        ml.setContentsMargins(*self.get_content_margins())
        ml.setSpacing(self.setspacing)
        ml.addWidget(self._create_toolbar())
        self._view = MapView()
        self._view.cell_clicked.connect(self._block_show_cell)
        self._view.cell_painted.connect(self._block_paint)
        self._view.cell_hovered.connect(self._hover)
        self._view.menu_requested.connect(self._map_menu)
        self._view.item_picked.connect(self._data_item_picked)
        self._view.item_moved.connect(self._data_item_moved)
        tabs = QTabWidget()
        tabs.setDocumentMode(True)
        tabs.addTab(self._view, "Map")
        tabs.addTab(self._create_sprites_tab(), "Sprites")
        tabs.addTab(self._create_text_tab(), "Text")
        tabs.addTab(self._create_media_tab(), "Media")
        tabs.addTab(self._create_data_tab(), "Data")
        self._centre_tabs = tabs
        docks = [("Files", self._create_files_dock(), 150, Qt.DockWidgetArea.LeftDockWidgetArea),
                 ("Block", self._create_block_dock(), 180, Qt.DockWidgetArea.RightDockWidgetArea),
                 ("Tiles", self._create_tiles_dock(), 150, Qt.DockWidgetArea.RightDockWidgetArea)]
        ml.addWidget(self._dock_layout(self.ribbon_wrap(tabs), docks), 1)
        self._inner_mw = self._ribbon_mw                  # Ribbon Manager works on these ribbons
        self._block_help_button(self._outer_docks[1])
        self._build_ribbons()
        self._status_widget = self._create_status_bar()
        ml.addWidget(self._status_widget)
        register_statusbar(self, self._status_widget)

    def _build_ribbons(self): #vers 10
        """File / View / Edit ribbons."""
        B = self.ribbon_button
        tb = self.ribbon_toolbar("File")
        B(tb, "folder_icon", "Open game folder (GTA1 / GTA2)", lambda: self._open_game_folder(''))
        B(tb, "open_icon", "Open a map (.cmp / .gmp / .vsr), style (.g24 / .gry / .sty) or text", lambda: self._open_file(None))
        B(tb, "disc_app_icon", "Open a PS1 / PC disc image in Disc Workshop", self._open_disc)
        B(tb, "new_icon", "New blank map: clear the open map's blocks to flat ground (other data kept)",
          self._map_blank)
        B(tb, "save_icon", "Save map (backup first)", self._save_map)
        B(tb, "saveas_icon", "Save map as a new file", self._save_map_as)
        B(tb, "save_all_icon", "Save style (backup first)", self._save_style)
        B(tb, "export_icon", "Export all tiles and sprites as PNG files", self._export_png)
        B(tb, "get_txd_file_icon", "Export all tiles and sprites into one TXD", self._export_txd)
        B(tb, "get_palette_icon", "Export style palettes as PNG (a row per palette)", self._palette_export)
        B(tb, "import_icon", "Import an edited palette PNG into the style", self._palette_import)
        B(tb, "paint_icon", "Edit style palettes in DP5 Workshop", self._palette_to_dp5)
        B(tb, "mesh_icon", "Export map as 3D: OBJ, sector DFF + COL, IDE / IPL, TXD (Map Workshop)",
          self._export_3d)
        tb = self.ribbon_toolbar("View")
        B(tb, "zoom_in_icon", "Zoom in", lambda: (setattr(self._view, 'scale', min(128.0, self._view.scale * 1.5)),
                                                   self._view.update()))
        B(tb, "zoom_out_icon", "Zoom out", lambda: (setattr(self._view, 'scale', max(1.0, self._view.scale / 1.5)),
                                                     self._view.update()))
        B(tb, "fit_grid_icon", "Whole map", self._view.fit)
        g = B(tb, "grid_icon", "Block grid (zoomed in)", lambda on=False: (setattr(self._view, 'show_grid', on),
                                                                          self._view.update()), checkable=True)
        g.setChecked(True)
        B(tb, "get_zone_icon", "Map zones", lambda on=False: (setattr(self._view, 'show_zones', on),
                                                              self._view.update()), checkable=True)
        B(tb, "get_paths_icon", "Ground types (road, pavement, field, water) and traffic arrows",
          lambda on=False: (setattr(self._view, 'show_ground', on), self._view.update()), checkable=True)
        db = B(tb, "light_icon", "Lights and map objects (Data tab)",
               lambda on=False: (setattr(self._view, 'show_data', on), self._view.update()), checkable=True)
        db.setChecked(True)
        mb = B(tb, "get_pin_icon", "Mission objects / script placements (pick in Files - Missions)",
               lambda on=False: (setattr(self._view, 'show_missions', on), self._view.update()), checkable=True)
        mb.setChecked(True)
        self._level_combo = QComboBox()
        self._level_combo.addItem("Top", -1)
        for z in range(8):                                  # GTA1 has 6, GTA2 8
            self._level_combo.addItem(f"Level {z}", z)
        self._level_combo.setToolTip("Lids shown: top block of each column, or one level")
        self._level_combo.currentIndexChanged.connect(lambda _i: self._view.set_level(self._level_combo.currentData()))
        tb.addWidget(self._level_combo)
        tb = self.ribbon_toolbar("Edit")
        self._sel_btn = B(tb, "dropper_icon", "Select block (click)", lambda: self._set_tool('select'), checkable=True)
        self._paint_btn = B(tb, "paint_icon", "Paint lids with the picked tile (click / drag)",
                            lambda: self._set_tool('paint'), checkable=True)
        self._sel_btn.setChecked(True)
        B(tb, "undo_icon", "Undo block edit  (Ctrl+Z)", self._block_undo)
        B(tb, "import_exchange_icon", "Port blocks from another map (PC <-> PS1, tiles matched to this style)",
          self._port_from_map)

    def _save_toolbar_state(self): #vers 1
        """Ribbon Manager OK: save the ribbon layout."""
        self.ribbon_save_state()

    def _toolbar_context_menu(self, toolbar, pos): #vers 1
        """Ribbon Manager new toolbars: same right-click menu as the ribbons."""
        self.ribbon_context_menu(toolbar, pos)

    def _set_tool(self, tool: str): #vers 1
        self._view.set_tool(tool)
        self._sel_btn.setChecked(tool == 'select')
        self._paint_btn.setChecked(tool == 'paint')

    def _undo(self): #vers 1
        self._block_undo()

    def _save_file(self): #vers 2
        """Titlebar Save: map and style, whichever changed."""
        if self._map_dirty:
            self._save_map()
        if self._style_dirty:
            self._save_style()
        if self._text_dirty:
            self._text_save()

    def _create_files_dock(self) -> QWidget: #vers 4
        """Game folder label + [Folder] button, maps list, style and missions combos."""
        page = QWidget()
        lay = QVBoxLayout(page)
        lay.setContentsMargins(4, 4, 4, 4)
        self._folder = ''
        self._folder_lbl = QLabel("No game folder")
        self._folder_lbl.setWordWrap(True)
        self._map_list = QListWidget()
        self._map_list.itemDoubleClicked.connect(lambda it: self._map_picked(it))
        self._map_list.setToolTip("Double-click to open a map with its style")
        self._style_combo = QComboBox()
        self._style_combo.setToolTip("Style the map is drawn with")
        self._style_combo.activated.connect(lambda _i: self._open_style(self._style_combo.currentData()))
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as I
        fb = QPushButton("Folder")
        fb.setIcon(I.folder_icon(16, self._get_icon_color()))
        fb.setToolTip("Open game folder (GTA1 / GTA2, PC or PS1)")
        fb.clicked.connect(lambda _c=False: self._open_game_folder(''))
        top = QHBoxLayout()
        top.addWidget(self._folder_lbl, 1)
        top.addWidget(fb)
        lay.addLayout(top)
        lay.addWidget(self._map_list, 1)
        self._mission_widgets(lay, self._style_combo)
        return page

    def _show_folder(self): #vers 1
        """Folder label: short (last two folders) or full path (Settings)."""
        f = self._folder
        if not f:
            self._folder_lbl.setText("No game folder")
            return
        parts = os.path.normpath(f).split(os.sep)
        self._folder_lbl.setText(f if self.WS.get("full_paths", False) else "/".join(parts[-2:]) + "/")
        self._folder_lbl.setToolTip(f)

    def _extra_settings_tab(self, tabs): #vers 2
        """Settings: Retro GTA tab (folder paths)."""
        from PyQt6.QtWidgets import QCheckBox
        page = QWidget()
        lay = QVBoxLayout(page)
        full = QCheckBox("Show full folder paths in the Files panel")
        full.setChecked(bool(self.WS.get("full_paths", False)))
        lay.addWidget(full)
        lay.addStretch(1)
        tabs.addTab(page, "Retro GTA")

        def apply():
            self.WS.set("full_paths", full.isChecked())
            self._show_folder()
        return apply

    def _open_game_folder(self, folder: str): #vers 8
        """List a GTA1 (GTADATA) or GTA2 (data) folder's maps and styles."""
        if not folder:
            folder = QFileDialog.getExistingDirectory(self, "GTA1 / GTA2 game folder", self.WS.get("game_folder", ""))
            if not folder:
                return
        sub = {d.lower(): d for d in os.listdir(folder)}
        data = os.path.join(folder, sub['gtadata']) if 'gtadata' in sub else \
            os.path.join(folder, sub['data']) if 'data' in sub else folder
        names = sorted(os.listdir(data))
        maps = [n for n in names if n.lower().endswith(('.gmp', '.cmp'))]
        styles = [n for n in names if n.lower().endswith(('.sty', '.g24', '.gry'))]
        if not maps:                                       # PS1 GTA1: city .vsr files
            maps = [n for n in names if n.lower().endswith('.vsr') and self._vsr_city(os.path.join(data, n))]
        if not maps:
            QMessageBox.warning(self, App_name, "No GTA1 (.cmp / PS1 .vsr) or GTA2 (.gmp) maps in that folder.")
            return
        ext = maps[0].lower()[-4:]
        game = {'.cmp': 'GTA1', '.vsr': 'GTA1 PS1'}.get(ext, 'GTA2')
        self._data_dir, self._mmp = data, {}
        for n in names:
            if n.lower().endswith('.mmp'):
                cp = configparser.ConfigParser()
                try:
                    cp.read(os.path.join(data, n), encoding='latin-1')
                    sec = cp['MapFiles']
                    self._mmp.setdefault(sec.get('GMPFile', '').strip().lower(), []).append(
                        (sec.get('STYFile', '').strip(), sec.get('Description', '').strip()))
                except (configparser.Error, KeyError):
                    continue
        self._map_list.clear()
        for n in maps:
            desc = ", ".join(d for _s, d in self._mmp.get(n.lower(), [])[:2])
            it = QListWidgetItem(n + (f"  ({desc})" if desc else ""))
            it.setData(Qt.ItemDataRole.UserRole, os.path.join(data, n))
            self._map_list.addItem(it)
        self._text_list_folder(data)
        self._mission_list_folder(data)
        self._media_list_folder(folder)
        self._style_combo.clear()
        for n in styles:
            self._style_combo.addItem(n, os.path.join(data, n))
        self._folder = folder
        self._show_folder()
        if getattr(self, '_outer_docks', None):
            self._outer_docks[0]._title_label.setText(f"Files - {game} ({len(maps)} maps, {len(styles)} styles)")
        self.WS.set("game_folder", folder)
        self.WS.save()
        self._set_status(f"{game} folder: {len(maps)} maps")

    def _style_for(self, map_path: str) -> str: #vers 3
        """Style for a map: PS1 .vsr itself; GTA1 header style number; GTA2 .mmp
        entry, same name, name before '-'; else the first style."""
        d, n = os.path.split(map_path)
        if n.lower().endswith('.vsr'):
            return map_path
        exts = ('.g24',) if n.lower().endswith('.cmp') else ('.sty',)
        names = {x.lower(): x for x in os.listdir(d) if x.lower().endswith(exts)}
        stem = os.path.splitext(n)[0].lower()
        cands = [stem + exts[0], stem.split('-')[0] + exts[0]]
        if n.lower().endswith('.cmp'):
            with open(map_path, 'rb') as f:
                cands.insert(0, f"style{f.read(5)[4]:03d}.g24")      # header style number
        else:
            cands.insert(0, next((s for s, _d in self._mmp.get(n.lower(), []) if s), '').lower())
        for cand in cands:
            if cand in names:
                return os.path.join(d, names[cand])
        return os.path.join(d, sorted(names.values())[0]) if names else ''

    @staticmethod
    def _vsr_city(path: str) -> bool: #vers 1
        """PS1 .vsr holding a city map (CMP folder)."""
        try:
            with open(path, 'rb') as f:
                return VSRArchive.from_bytes(f.read(), path).entry('CMP', 'BASE') is not None
        except (OSError, ValueError, KeyError, struct.error):
            return False

    def _open_disc(self): #vers 1
        """Disc Workshop (tab in IMG Factory, else its own window)."""
        mw = self.main_window
        if mw is not None and hasattr(mw, 'open_disc_workshop_docked'):
            mw.open_disc_workshop_docked()
        else:
            from apps.components.Disc_Workshop.disc_workshop import open_disc_workshop
            self._disc_ws = open_disc_workshop(None)

    def _map_picked(self, item): #vers 1
        self._open_map(item.data(Qt.ItemDataRole.UserRole))

    def _open_file(self, path=None): #vers 6
        """Open a .gmp (with its style) or a .sty."""
        if not path:
            path, _ = QFileDialog.getOpenFileName(self, "Open GTA1 / GTA2 file", self._data_dir or '',
                                                  "Maps, styles, text, media (*.cmp *.g24 *.gry *.gmp *.sty *.vsr *.fxt *.gxt *.sdt *.str *.wav);;"
                                                  "All files (*)")
            if not path:
                return
        if path.lower().endswith(TEXT_EXTS):
            self._text_open(path)
            self._centre_tabs.setCurrentIndex(2)
        elif path.lower().endswith(MEDIA_EXTS) and not (path.lower().endswith('.vsr') and self._vsr_city(path)):
            self._media_open(path)
            self._centre_tabs.setCurrentIndex(3)
        elif path.lower().endswith(('.sty', '.g24', '.gry')):
            self._open_style(path)
        else:
            self._open_map(path)

    def _open_map(self, path: str, style_path: str = ''): #vers 7
        """Read a map (and its style), show it."""
        if self._map_dirty and QMessageBox.question(
                self, App_name, "Discard unsaved map changes?") != QMessageBox.StandardButton.Yes:
            return
        try:
            m = self._read_map(path)
        except (OSError, ValueError, KeyError, struct.error) as ex:
            QMessageBox.warning(self, App_name, str(ex))
            return
        self._map, self._map_dirty = m, False
        self._block_map_opened()
        style_path = style_path or self._style_for(path)
        if hasattr(m, 'arch'):                             # PS1: style lives in the same .vsr
            self._open_style(style_path, redraw=False)
        elif style_path and (self._style is None or self._style.path != style_path):
            self._open_style(style_path, redraw=False)
        self._view.set_data(self._map, self._style)
        self._mission_for_map(path)
        self._data_fill()
        self.WS.set("last_map", path)
        self._update_title()
        self._set_status(f"{os.path.basename(path)}: {len(m.blocks)} blocks, {len(m.zones())} zones")

    @staticmethod
    def _read_map(path: str): #vers 1
        """Map object of a .cmp / .vsr / .gmp (PC or PS1)."""
        with open(path, 'rb') as f:
            data = f.read()
        kind = {'.cmp': GTA1Map, '.vsr': GTA1PSXMap}.get(path.lower()[-4:], GTA2Map)
        if data[:4] == b'CMAP':                            # PS1 GTA2 map
            kind = GTA2PSXMap
        return kind.from_bytes(data, path)

    @staticmethod
    def _read_style(path: str, gmap=None): #vers 2
        """Style object of a .g24 / .gry / .sty (PC or PS1); a .vsr's from its map archive."""
        if path.lower().endswith('.vsr'):
            return GTA1PSXStyle.from_archive(gmap.arch)
        with open(path, 'rb') as f:
            data = f.read()
        kind = GTA1Style if path.lower().endswith(('.g24', '.gry')) else GTA2Style
        if kind is GTA2Style and data[:4] != b'GBST':      # PS1 GTA2 style
            kind = GTA2PSXStyle
        return kind.from_bytes(data, path)

    def _open_style(self, path: str, redraw: bool = True): #vers 5
        """Read a style; tiles / sprites refilled; map redrawn with it."""
        if not path:
            return
        if self._style_dirty and QMessageBox.question(
                self, App_name, "Discard unsaved style changes?") != QMessageBox.StandardButton.Yes:
            return
        if path.lower().endswith('.vsr') and getattr(self._map, 'path', '') != path:
            self._open_map(path)                           # PS1 .vsr: map and style together
            return
        try:
            self._style = self._read_style(path, self._map)
        except (OSError, ValueError, KeyError, struct.error) as ex:
            QMessageBox.warning(self, App_name, str(ex))
            return
        self._style_dirty = False
        i = self._style_combo.findData(path)
        if i >= 0:
            self._style_combo.setCurrentIndex(i)
        self._fill_tiles()
        self._sprite_group.clear()
        self._fill_sprites()
        if redraw and self._map is not None:
            self._view.set_data(self._map, self._style)
        self._update_title()

    def _save_map(self): #vers 2
        """Write the map (.gmp, backup first)."""
        from apps.methods.file_backup import safe_write_bytes
        if self._map is None:
            return
        try:
            safe_write_bytes(self._map.path, self._map.to_bytes(), f"{App_name}: {os.path.basename(self._map.path)}")
        except Exception as ex:
            QMessageBox.critical(self, "Save map", str(ex))
            return
        self._map_dirty = False
        if getattr(self._style, 'arch', None) is getattr(self._map, 'arch', 0):
            self._style_dirty = False                      # PS1 .vsr: style saved with it
        self._update_title()
        self._set_status(f"Saved {self._map.path}")

    def _save_style(self): #vers 2
        """Write the style (.sty, backup first)."""
        from apps.methods.file_backup import safe_write_bytes
        if self._style is None:
            return
        try:
            safe_write_bytes(self._style.path, self._style.to_bytes(), f"{App_name}: {os.path.basename(self._style.path)}")
        except Exception as ex:
            QMessageBox.critical(self, "Save style", str(ex))
            return
        self._style_dirty = False
        if getattr(self._style, 'arch', None) is getattr(self._map, 'arch', 0):
            self._map_dirty = False                        # PS1 .vsr: map saved with it
        self._update_title()
        self._set_status(f"Saved {self._style.path}")

    def _update_title(self): #vers 1
        """Title: map / style names, * when unsaved."""
        parts = []
        if self._map is not None:
            parts.append(os.path.basename(self._map.path) + (" *" if self._map_dirty else ""))
        if self._style is not None:
            parts.append(os.path.basename(self._style.path) + (" *" if self._style_dirty else ""))
        text = f"{App_name}" + (f" - {'  |  '.join(parts)}" if parts else "")
        self.setWindowTitle(text)
        if hasattr(self, 'title_label'):
            self.title_label.setText(text)

    def _hover(self, x: int, y: int): #vers 5
        """Status: cell, top level, ground, zones under the cursor."""
        if self._map is None:
            return
        z, b = self._map.top_block(x, y)
        zones = [zn['name'] for zn in self._map.zones()
                 if zn['x'] <= x < zn['x'] + zn['w'] and zn['y'] <= y < zn['y'] + zn['h']]
        ground = self._map.GROUND[self._map.ground_of(b)] if b else '-'
        marks = [m['info'] for m in self._view.markers_at(x, y)]
        self._set_status(f"Block {x}, {y}   top z{z}   {ground}" + (f"   zones: {', '.join(zones)}" if zones else "")
                         + (f"   {' | '.join(marks[:3])}" if marks else ""))

    def closeEvent(self, ev): #vers 2
        """Ask before losing map / style edits."""
        if (self._map_dirty or self._style_dirty or self._text_dirty) and QMessageBox.question(
                self, App_name, "Close without saving the map / style / text?") != QMessageBox.StandardButton.Yes:
            ev.ignore()
            return
        super().closeEvent(ev)


def open_retro_gta_workshop(main_window=None, file_path=None): #vers 1
    """Open docked in an IMG Factory tab, or standalone."""
    mw = main_window
    if mw is not None and hasattr(mw, 'main_tab_widget'):
        c = QWidget()
        lay = QVBoxLayout(c)
        lay.setContentsMargins(0, 0, 0, 0)
        w = RetroGTAWorkshop(parent=c, main_window=mw)
        lay.addWidget(w)
        tw = mw.main_tab_widget
        tw.setCurrentIndex(tw.addTab(c, SVGIconFactory.retro_gta_app_icon(20), App_name))
        if hasattr(mw, 'register_tool'):
            mw.register_tool("retro_gta", "Retro GTA", SVGIconFactory.retro_gta_app_icon(20), c, App_name)
        if hasattr(mw, '_ensure_tab_area_visible'):
            mw._ensure_tab_area_visible()
    else:
        w = RetroGTAWorkshop(main_window=main_window)
        w.resize(1400, 860)
        w.show()
    if file_path:
        w._open_file(file_path)
    return w


if __name__ == "__main__":
    app = QApplication(sys.argv)
    app.setWindowIcon(SVGIconFactory.retro_gta_app_icon(64))
    win = open_retro_gta_workshop(None, sys.argv[1] if len(sys.argv) > 1 else None)
    sys.exit(app.exec())
