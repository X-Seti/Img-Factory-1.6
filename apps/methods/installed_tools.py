#this belongs in apps/methods/installed_tools.py - Version: 1
# X-Seti - October 29 2026 - IMG Factory 1.6 - Installed tools

"""
Which workshops are present in apps/components. The Intro cards and the
Editing Options buttons only show tools whose component folder is there,
so a standalone tool dropped into components appears, a missing one hides.
"""

##Methods list -
# component_installed
# option_available

from functools import lru_cache
from pathlib import Path

COMPONENTS = Path(__file__).resolve().parents[1] / 'components'

# Editing Options button method -> component folder (None: always shown)
OPTION_COMPONENTS = {
    'toggle_dir_tree': 'File_Editor', 'edit_dat_file': 'Dat_Browser', 'edit_hex': 'Hex_Editor',
    'edit_col_file': 'Col_Editor', 'edit_dff_file': 'Model_Editor', 'edit_txd_file': 'Txd_Editor',
    'edit_radar_map': 'Radar_Editor', 'edit_waterpro': 'Water_Editor', 'open_dp5_workshop_docked': 'DP5_Workshop',
    'edit_ide_file': 'Ide_Editor', 'edit_ipl_file': 'Ipl_Editor', 'edit_objects': 'Breakable_Editor',
    'edit_peds_file': 'Ped_Workshop', 'edit_ipf_file': 'Ped_Workshop', 'edit_paths_map': 'Path_Workshop',
    'edit_vehi_file': 'Vehicle_Workshop', 'edit_handling': 'Handling_Editor', 'edit_carcols': 'Vehicle_Workshop',
    'edit_timecyc': 'Timecyc_Editor', 'editscm': 'Scm_Workshop',
    'edit_zones_cull': '', 'edit_weap_file': '', 'editgxt': '', 'editmenu': '',   # no tool yet
}


@lru_cache(maxsize=None)
def component_installed(folder: str) -> bool: #vers 1
    """True when apps/components/<folder> holds a tool (any .py besides __init__)."""
    if not folder:
        return False
    d = COMPONENTS / folder
    return d.is_dir() and any(p.name != '__init__.py' for p in d.glob('*.py'))


def option_available(method_name: str) -> bool: #vers 1
    """Editing Options button shown? Unknown methods are core actions (shown)."""
    folder = OPTION_COMPONENTS.get(method_name)
    return True if folder is None else component_installed(folder)
