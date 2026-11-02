#!/usr/bin/env python3
#this belongs in apps/methods/smart_file_router.py - Version: 12
# X-Seti - May08 2026 - Img Factory 1.6 - Smart File Router

"""
Routes GTA data files to the correct editor based on filename.
Used by dat_browser and directory_tree_browser.
Falls back to plain text editor for unrecognised files.
"""

##Methods list -
# _route_entry
# annotate_checklist
# coverage
# get_editor_label
# open_smart_editor

import os
import re


# Map: lowercase filename → (editor_label, launcher_function_name)
_ROUTE_MAP = {
    # Path Workshop
    "nodes.dat":     ("Path Workshop (Nodes GTA3/VC)", "_launch_path_workshop"),
    "nodes0.dat":    ("Path Workshop (Nodes SA)",      "_launch_path_workshop"),
    "nodes1.dat":    ("Path Workshop (Nodes SA)",      "_launch_path_workshop"),
    "nodes2.dat":    ("Path Workshop (Nodes SA)",      "_launch_path_workshop"),
    "nodes3.dat":    ("Path Workshop (Nodes SA)",      "_launch_path_workshop"),
    "nodes4.dat":    ("Path Workshop (Nodes SA)",      "_launch_path_workshop"),
    "nodes5.dat":    ("Path Workshop (Nodes SA)",      "_launch_path_workshop"),
    "nodes6.dat":    ("Path Workshop (Nodes SA)",      "_launch_path_workshop"),
    "nodes7.dat":    ("Path Workshop (Nodes SA)",      "_launch_path_workshop"),
    "nodes8.dat":    ("Path Workshop (Nodes SA)",      "_launch_path_workshop"),
    "paths.ipl":     ("Path Workshop (SA Paths)",      "_launch_path_workshop"),
    "paths2.ipl":    ("Path Workshop (SA Paths)",      "_launch_path_workshop"),
    "paths3.ipl":    ("Path Workshop (SA Paths)",      "_launch_path_workshop"),
    "paths4.ipl":    ("Path Workshop (SA Paths)",      "_launch_path_workshop"),
    "paths5.ipl":    ("Path Workshop (SA Paths)",      "_launch_path_workshop"),
    "train.dat":     ("Path Workshop (Train)",         "_launch_path_workshop"),
    "train2.dat":    ("Path Workshop (Train 2)",       "_launch_path_workshop"),
    "tracks.dat":    ("Path Workshop (Train)",         "_launch_path_workshop"),
    "tracks2.dat":   ("Path Workshop (Train)",         "_launch_path_workshop"),
    "flight.dat":    ("Path Workshop (Flight)",        "_launch_path_workshop"),
    "flight2.dat":   ("Path Workshop (Flight 2)",      "_launch_path_workshop"),
    "flight3.dat":   ("Path Workshop (Flight 3)",      "_launch_path_workshop"),
    "spath0.dat":    ("Path Workshop (Static)",        "_launch_path_workshop"),
    # Water Workshop
    "waterpro.dat":  ("Water Workshop",                "_launch_water_workshop"),
    "water.dat":     ("Water Workshop",                "_launch_water_workshop"),
    "handling.cfg":  ("Vehicle Workshop (Handling)",   "_launch_vehicle_workshop"),
    "carcols.dat":   ("Vehicle Workshop (Car Colours)", "_launch_vehicle_workshop"),
    "carmods.dat":   ("Vehicle Workshop (Car Mods)",    "_launch_vehicle_workshop"),
    # Breakable objects
    "object.dat":    ("Breakable Objects Editor",       "_launch_breakable_editor"),
    # Time cycle
    "timecyc.dat":   ("Time Cycle Editor",              "_launch_timecyc_editor"),
    "timecycp.dat":  ("Time Cycle Editor",              "_launch_timecyc_editor"),
    # Surface data — opens in COL Workshop Surface tab
    "surface.dat":   ("COL Workshop (Surface Data)",    "_launch_surface_in_col"),
    # Future — known files that will get editors later
    "weapon.dat":    ("Weapon Workshop",                "_launch_weapon_workshop"),
    "ped.dat":       ("Ped Workshop",                  "_launch_ped_workshop"),
    "pedgrp.dat":    ("Ped Workshop",                  "_launch_ped_workshop"),
    "pedstats.dat":  ("Ped Workshop",                  "_launch_ped_workshop"),
    "cargrp.dat":    ("Vehicle Workshop",               "_launch_vehicle_workshop"),
    "particle.cfg":  ("Particle Editor",                None),
    "shopping.dat":  ("Ped Workshop",                  "_launch_ped_workshop"),
    "popcycle.dat":  ("Ped Workshop",                  "_launch_ped_workshop"),
    "peds.ide":      ("Ped Workshop",                  "_launch_ped_workshop"),
    "ped.ifp":       ("Ped Workshop",                  "_launch_ped_workshop"),
    "clothes.dat":   ("Ped Workshop",                  "_launch_ped_workshop"),
    "animgrp.dat":   ("Ped Workshop",                  "_launch_ped_workshop"),
    # SA data folder, grouped to the editor that owns them
    "vehicles.ide":  ("Vehicle Workshop",               "_launch_vehicle_workshop"),
    "numplate.dat":  ("Vehicle Workshop (Number plates)", None),
    "sa_water.dat":  ("Water Workshop",                "_launch_water_workshop"),
    "sa_water1.dat": ("Water Workshop",                "_launch_water_workshop"),
    "water1.dat":    ("Water Workshop",                "_launch_water_workshop"),
    "melee.dat":     ("Weapon Workshop",               "_launch_weapon_workshop"),
    "default.ide":   ("IDE Editor",                    "_launch_ide_editor"),
    "txdcut.ide":    ("IDE Editor",                    "_launch_ide_editor"),
    "gta.dat":       ("DAT Browser",                   "_launch_dat_browser"),
    "gta_quick.dat": ("DAT Browser",                   "_launch_dat_browser"),
    "default.dat":   ("DAT Browser",                   "_launch_dat_browser"),
    "main.sc":       ("SCM Workshop",                  "_launch_scm_workshop"),
    "ar_stats.dat":  ("Weapon Workshop",               "_launch_weapon_workshop"),
    "info.zon":      ("Map Workshop (Zones)",          "_launch_map_zones"),
    "map.zon":       ("Map Workshop (Zones)",          "_launch_map_zones"),
    "navig.zon":     ("Map Workshop (Zones)",          "_launch_map_zones"),
    "gta3.zon":      ("Map Workshop (Zones)",          "_launch_map_zones"),
    "surfinfo.dat":  ("COL Workshop (Surface Info)",   None),
    "surfaud.dat":   ("COL Workshop (Surface Audio)",  None),
    "procobj.dat":   ("Terrain Workshop (Procedural objects)", None),
    "plants.dat":    ("Terrain Workshop (Plants)",     None),
    "polydensity.dat": ("Terrain Workshop (Ped / car density)", None),
    "furnitur.dat":  ("Map Workshop (Interior furniture)", None),
    "fonts.dat":     ("Font Editor",                   None),
    "statdisp.dat":  ("Stats Display Editor",          None),
    "gridref.dat":   ("Map Workshop (Grid references)", None),
    "animviewer.dat": ("Ped Workshop (Anim viewer list)", "_launch_ped_workshop"),
    # III / VC data folders
    "gta3.dat":      ("DAT Browser",                   "_launch_dat_browser"),
    "gta_vc.dat":    ("DAT Browser",                   "_launch_dat_browser"),
    "main.scm":      ("SCM Workshop",                  "_launch_scm_workshop"),
    "debug.sc":      ("SCM Workshop",                  "_launch_scm_workshop"),
    "roadblox.dat":  ("Path Workshop (Roadblocks SA)", "_launch_path_workshop"),
    "gtaiii_water.dat":    ("Water Workshop",          "_launch_water_workshop"),
    "gtaiii_waterpro.dat": ("Water Workshop",          "_launch_water_workshop"),
    "org_waterpro.dat":    ("Water Workshop",          "_launch_water_workshop"),
    "water_offset.dat": ("Water Workshop (Offsets)",   None),
    "cullzone.dat":  ("Map Workshop (Cull zones)",     None),
    "cullzoneempty.dat": ("Map Workshop (Cull zones)", None),
    "fistfite.dat":  ("Ped Workshop (Fist fight)",    "_launch_ped_workshop"),
    "pedevent.txt":  ("Ped Workshop (Decision events)", "_launch_ped_workshop"),
    # Cutscenes (anim/cuts.img, .cut scripts)
    "cuts.img":      ("Cutscene Workshop",             "_launch_cutscene_workshop"),
    "cuts.dir":      ("Cutscene Workshop",             "_launch_cutscene_workshop"),
}


# Files left out of the editor checklist, name -> reason
_IGNORE_MAP = {
    "water_offset.dat": "IMG Factory own file, set in Map Workshop settings",
    "app.ico":          "Windows icon",
    "bin.ico":          "Windows icon",
    "saicon.icn":       "Windows icon",
    "saicon2.icn":      "Windows icon",
    "saicon3.icn":      "Windows icon",
}

# Checklist only: editors by extension (tree / IMG open these directly)
_EXT_COVER = {
    ".ide": ("IDE Editor", True), ".ipl": ("IPL Workshop", True),
    ".col": ("COL Workshop", True), ".img": ("IMG Factory", True),
    ".txd": ("TXD Workshop", True), ".dff": ("Model Workshop", True),
    ".ped": ("Ped Workshop (Decision makers)", True),
    ".grp": ("Ped Workshop (Decision groups)", True),
}

_ROUTE_PATTERNS = [
    (r'nodes\d+\.dat$',          "nodes0.dat"),     # SA nodes0-63
    (r'(chase\d+|tracks\d*|flight\d*)\.dat$', "train.dat"),
    (r'^map\d+\.dat$',            "gta.dat"),        # VC maps/mapN.dat load lists
    (r'\.cut$',                  "cuts.img"),
    (r'\.zon$',                  "map.zon"),
]


def _route_entry(name: str): #vers 1
    """Route-map entry for a lowercase filename, by name then pattern."""
    entry = _ROUTE_MAP.get(name)
    if entry:
        return entry
    for pattern, key in _ROUTE_PATTERNS:
        if re.search(pattern, name):
            label = _ROUTE_MAP[key][0].split(' (')[0]
            return (label, _ROUTE_MAP[key][1])
    return None


def annotate_checklist(list_path: str) -> dict: #vers 1
    """Mark each file in a tree listing with its editor; returns counts."""
    counts = {'editor': 0, 'planned': 0, 'ignore': 0, 'none': 0}
    tag = {'editor': '->', 'planned': '[planned]', 'ignore': '[ignore]', 'none': '[NO EDITOR]'}
    out = []
    with open(list_path, encoding='utf-8') as fh:
        lines = fh.read().split('\n')
    for line in lines:
        line = line.split('    #')[0].rstrip()           # drop old marks
        name = re.sub(r'^[\s\u2500-\u257f]+', '', line)  # strip tree drawing
        m = re.match(r'(\S+\.[A-Za-z0-9]+)(\s|$)', name)   # file, maybe with own note
        if m:
            status, label = coverage(m.group(1))
            counts[status] += 1
            line = f"{line:<44}    # {tag[status]} {label}".rstrip()
        out.append(line)
    with open(list_path, 'w', encoding='utf-8') as fh:
        fh.write('\n'.join(out))
    return counts


def coverage(file_name: str): #vers 1
    """(status, label): editor, planned, ignore or none for a filename."""
    name = os.path.basename(file_name).lower()
    if name in _IGNORE_MAP:
        return 'ignore', _IGNORE_MAP[name]
    entry = _route_entry(name)
    if entry:
        return ('editor' if entry[1] else 'planned'), entry[0]
    ext = _EXT_COVER.get(os.path.splitext(name)[1])
    if ext:
        return ('editor' if ext[1] else 'planned'), ext[0]
    return 'none', ''


def get_editor_label(file_path: str) -> str: #vers 3
    """Return human-readable editor name for a file, or empty string if plain text only."""
    entry = _route_entry(os.path.basename(file_path).lower())
    return entry[0] if entry else ""


def open_smart_editor(file_path: str, main_window=None) -> bool: #vers 8
    """
    Open file_path in the most appropriate editor.
    Returns True if a specialist editor was opened, False if fell back to text editor.
    """
    name = os.path.basename(file_path).lower()
    entry = _route_entry(name)

    if entry and entry[1]:
        launcher = entry[1]
        try:
            if launcher == "_launch_weapon_workshop":
                if main_window is not None and hasattr(main_window, 'open_weapon_workshop_docked'):
                    main_window.open_weapon_workshop_docked(file_path)
                else:
                    from apps.components.Weapon_Workshop.weapon_workshop import open_weapon_workshop
                    open_weapon_workshop(main_window, file_path)
                _log(main_window, f"Weapon Workshop: {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_ide_editor":
                from apps.components.Ide_Editor.ide_editor import open_ide_editor
                editor = open_ide_editor(main_window)
                editor.load_ide_file(file_path)
                _log(main_window, f"IDE Editor: {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_dat_browser":
                from apps.gui.gui_layout_custom import _show_dat_browser
                _show_dat_browser(main_window)
                _log(main_window, f"DAT Browser: {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_scm_workshop":
                from apps.components.Scm_Workshop.scm_workshop import open_scm_workshop
                open_scm_workshop(main_window, file_path)
                _log(main_window, f"SCM Workshop: {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_map_zones":
                from apps.components.Map_Editor.map_workshop import open_map_workshop
                root = os.path.dirname(os.path.dirname(os.path.abspath(file_path)))
                ws = open_map_workshop(main_window, game_root=root)
                if ws is not None:
                    ws._show_zones_dock(os.path.basename(file_path))
                _log(main_window, f"Map Workshop (Zones): {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_cutscene_workshop":
                if main_window is not None and hasattr(main_window, 'open_cutscene_workshop_docked'):
                    main_window.open_cutscene_workshop_docked(file_path)
                else:
                    from apps.components.Cutscene_Workshop.cutscene_workshop import open_cutscene_workshop
                    open_cutscene_workshop(main_window, file_path)
                _log(main_window, f"Cutscene Workshop: {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_water_workshop":
                from apps.components.Water_Editor.water_workshop import WaterWorkshop
                from PyQt6.QtWidgets import QApplication
                for w in QApplication.topLevelWidgets():
                    if isinstance(w, WaterWorkshop):
                        w._load_file(file_path)
                        w.raise_(); w.activateWindow()
                        _log(main_window, f"Water Workshop: {os.path.basename(file_path)}")
                        return True
                ww = WaterWorkshop(main_window=main_window)
                ww.resize(1300, 800)
                ww.show()
                ww._load_file(file_path)
                _log(main_window, f"Water Workshop: {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_path_workshop":
                from apps.components.Path_Workshop.path_workshop import open_path_workshop
                open_path_workshop(main_window, path=file_path)
                _log(main_window, f"Path Workshop: {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_vehicle_workshop":
                from apps.components.Vehicle_Workshop.vehicle_workshop import open_vehicle_workshop
                w = open_vehicle_workshop(main_window, path=file_path)
                _log(main_window, f"Vehicle Workshop: {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_ped_workshop":
                from apps.components.Ped_Workshop.ped_workshop import open_ped_workshop
                root = os.path.dirname(os.path.abspath(file_path))
                while os.path.basename(root).lower() != 'data' and os.path.dirname(root) != root:
                    root = os.path.dirname(root)      # Decision/sub/x.ped -> game root
                open_ped_workshop(main_window, os.path.dirname(root), file_path)
                _log(main_window, f"Ped Workshop: {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_breakable_editor":
                from apps.components.Breakable_Editor.breakable_editor import open_breakable_editor
                open_breakable_editor(main_window, path=file_path)
                _log(main_window, f"Breakable Editor: {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_timecyc_editor":
                from apps.components.Timecyc_Editor.timecyc_workshop import open_timecyc_editor
                open_timecyc_editor(main_window, path=file_path)
                _log(main_window, f"Time Cycle Editor: {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_surface_in_col":
                from apps.components.Col_Editor.col_workshop import COLWorkshop
                # Try to reuse an existing COL Workshop window
                from PyQt6.QtWidgets import QApplication
                for w in QApplication.topLevelWidgets():
                    if isinstance(w, COLWorkshop):
                        w._workshop_tabs.setCurrentIndex(1)  # Surface Data tab
                        w._surf_open(file_path)
                        w.raise_(); w.activateWindow()
                        _log(main_window, f"Surface Data: {os.path.basename(file_path)}")
                        return True
                # No existing window — open a new one
                cw = COLWorkshop(main_window)
                cw.resize(1200, 720)
                cw.show()
                cw._workshop_tabs.setCurrentIndex(1)
                cw._surf_open(file_path)
                _log(main_window, f"COL Workshop (Surface Data): {os.path.basename(file_path)}")
                return True

        except Exception as ex:
            _log(main_window, f"Editor launch error ({name}): {ex}")

    # Fallback: plain text editor
    try:
        from apps.core.notepad import open_text_file_in_editor
        open_text_file_in_editor(file_path, main_window)
        _log(main_window, f"Text Editor: {os.path.basename(file_path)}")
    except Exception as ex:
        _log(main_window, f"Text editor error: {ex}")
    return False


def _log(main_window, msg: str): #vers 1
    if main_window and hasattr(main_window, "log_message"):
        main_window.log_message(msg)
