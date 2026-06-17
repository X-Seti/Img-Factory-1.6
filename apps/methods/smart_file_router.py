#!/usr/bin/env python3
#this belongs in apps/methods/smart_file_router.py - Version: 3
# X-Seti - May08 2026 - Img Factory 1.6 - Smart File Router

"""
Routes GTA data files to the correct editor based on filename.
Used by dat_browser and directory_tree_browser.
Falls back to plain text editor for unrecognised files.
"""

##Methods list -
# get_editor_label
# open_smart_editor

import os


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
    "weapon.dat":    ("Weapon Data Editor",             None),
    "ped.dat":       ("Ped Data Workshop",              None),
    "pedgrp.dat":    ("Ped Data Workshop",              None),
    "pedstats.dat":  ("Ped Data Workshop",              None),
    "cargrp.dat":    ("Vehicle Workshop",               "_launch_vehicle_workshop"),
    "particle.cfg":  ("Particle Editor",                None),
    "shopping.dat":  ("Shop Data Editor",               None),
    "popcycle.dat":  ("Population Cycle Editor",        None),
    "clothes.dat":   ("Ped Data Workshop",              None),
    "animgrp.dat":   ("Ped Data Workshop",              None),
}


def get_editor_label(file_path: str) -> str: #vers 1
    """Return human-readable editor name for a file, or empty string if plain text only."""
    name = os.path.basename(file_path).lower()
    entry = _ROUTE_MAP.get(name)
    return entry[0] if entry else ""


def open_smart_editor(file_path: str, main_window=None) -> bool: #vers 1
    """
    Open file_path in the most appropriate editor.
    Returns True if a specialist editor was opened, False if fell back to text editor.
    """
    name = os.path.basename(file_path).lower()
    entry = _ROUTE_MAP.get(name)

    if entry and entry[1]:
        launcher = entry[1]
        try:
            if launcher == "_launch_water_workshop":
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

            elif launcher == "_launch_breakable_editor":
                from apps.components.Breakable_Editor.breakable_editor import open_breakable_editor
                open_breakable_editor(main_window, path=file_path)
                _log(main_window, f"Breakable Editor: {os.path.basename(file_path)}")
                return True

            elif launcher == "_launch_timecyc_editor":
                from apps.components.Timecyc_Editor.timecyc_editor import open_timecyc_editor
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
