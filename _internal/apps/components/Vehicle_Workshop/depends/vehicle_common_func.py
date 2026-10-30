#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_common_func.py - Version: 1
# X-Seti - October 14 2026 - IMG Factory 1.6 - Vehicle Workshop shared imports

"""
Shared imports, availability flags and build constants for Vehicle Workshop modules.
"""

##Methods list -
# (none - imports, flags and constants)

import os
import sys

from PyQt6.QtGui import QIcon

# - Path setup
# depends/ = tool-specific only (handling_editor, svg_icons, tool_menu_mixin)
# apps/ = shared (methods, utils, themes) — via project_root when in IMG Factory
_depends = os.path.join(os.path.dirname(os.path.abspath(__file__)))
if _depends not in sys.path:
    sys.path.insert(0, _depends)

# AppSettings — apps/utils/ (shipped with standalone tools per architecture)
APPSETTINGS_AVAILABLE = False
try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    try:
        from app_settings_system import AppSettings, SettingsDialog
        APPSETTINGS_AVAILABLE = True
    except ImportError:
        AppSettings = SettingsDialog = None

# SVGIconFactory — depends/ tool-specific copy
ICONS_AVAILABLE = False
try:
    from imgfactory_svg_icons import SVGIconFactory
    ICONS_AVAILABLE = True
except ImportError:
    try:
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        ICONS_AVAILABLE = True
    except ImportError:
        class SVGIconFactory:
            @staticmethod
            def _s(sz=20, c=None): return QIcon()  #vers 1
            open_icon = save_icon = export_icon = import_icon = delete_icon = \
            undo_icon = info_icon = properties_icon = minimize_icon = \
            maximize_icon = close_icon = settings_icon = search_icon = \
            zoom_in_icon = zoom_out_icon = fit_grid_icon = locate_icon = \
            paint_icon = fill_icon = dropper_icon = line_icon = rect_icon = \
            rect_fill_icon = scissors_icon = paste_brush_icon = \
            rotate_cw_icon = rotate_ccw_icon = flip_horz_icon = \
            flip_vert_icon = folder_icon = staticmethod(_s)

# ToolMenuMixin — depends/ tool-specific copy
try:
    from tool_menu_mixin import ToolMenuMixin
except ImportError:
    try:
        from apps.gui.tool_menu_mixin import ToolMenuMixin
    except ImportError:
        class ToolMenuMixin:
            def _build_menus_into_qmenu(self, pm): pass  #vers 1

# Handling file model (byte-exact) - the old import chain always finished with
# _HANDLING_AVAILABLE = False, so the Handling tab never worked.
try:
    from apps.components.Handling_Editor.handling_editor import VC_FIELDS, HANDLING_FLAGS
except Exception:
    VC_FIELDS, HANDLING_FLAGS = [("HandlingName", "str", "", "", "Handling name")], {}
_HANDLING_AVAILABLE = True

# Build information (decorative module-level copy; self.App_name/App_build
# used at runtime come from the GUIWorkshop class attributes instead)
App_name   = "Vehicle Workshop"
App_build  = "Build 2"
App_auth   = "X-Seti"
config_key = "vehicle_workshop"
