#!/usr/bin/env python3
#this belongs in apps/components/Asset_Workshop/asset_workshop.py - Version: 27
# X-Seti - October10 2025 - Img Factory 1.5 - Asset Workshop

"""
Check, Optimize, Adjust the game data.
"""

import sys
from pathlib import Path
_root = Path(__file__).resolve().parents[3]
if str(_root) not in sys.path: sys.path.insert(0, str(_root))

import os
from PyQt6.QtWidgets import (QApplication, QWidget, QVBoxLayout,
    QMessageBox
)
from PyQt6.QtCore import Qt


from apps.methods.asset_checker import check_assets, find_sibling_asset_files, find_game_asset_files


from apps.debug.debug_functions import img_debugger
from apps.components.Txd_Editor.txd_workshop import TXDWorkshop
from apps.components.Asset_Workshop.depends.asset_win_func import AssetWinMixin
from apps.components.Asset_Workshop.depends.asset_ui_func import AssetUIMixin
from apps.components.Asset_Workshop.depends.asset_logic_func import AssetLogicMixin


##Methods list -
# _register_asset_workshop_taskbar
# open_asset_workshop
#
##class AssetWorkshop: - (TXDWorkshop + depends/ asset_win_func, asset_ui_func, asset_logic_func)
# _display_mobile_textures
# _init_extra_state
# _load_txd_textures
# _open_ps2_txd
# _reload_texture_table
# _setup_context_menu

# Build information
App_name = "Asset Workshop"


class AssetWorkshop(AssetWinMixin, AssetUIMixin, AssetLogicMixin, TXDWorkshop): #vers 9
    """Asset checker on top of TXD Workshop: texture editing comes from TXDWorkshop,
    this class adds the Asset Check tab, exclusions, cross reference and its own panels."""

    App_name = App_name
    config_key = "asset_workshop"
    App_icon = "asset_app_icon"

    # Bump whenever the set of ribbon toolbars changes (added/removed/
    # renamed) so a saved layout from an older structure is cleanly
    # rejected by _restore_toolbar_state instead of Qt silently failing
    # to restore it. History: 1 = Transform/Nav/Effects only,
    # 2 = added merged Name/Format + Mipmaps, 3 = split Name/Format apart,
    # 4 = added Asset Check ribbon (Master IDE), 5 = removed Navigation
    # and Effects ribbons (viewport/background/effects tools dropped),
    # 6 = removed Name/Format/Mipmaps ribbons and Transform's flip/
    # rotate/switch/invert/generate-alpha actions.
    _RIBBON_LAYOUT_VERSION = 7

    _OUTER_LAYOUT_VERSION = 1      # Textures dock
    def _init_extra_state(self): #vers 2
        """Asset Check state, needed before setup_ui."""
        self.result = None            # AssetCheckResult, when opened as a checker
        self._checker_tab = None
        self.auto_find_gta3_img = True
        self.excluded_names = set()
        self.excluded_files = set()
        self.ignore_id_min = None
        self.ignore_id_max = None
        self._all_img_paths = []
        self._all_col_paths = []
        self._all_ide_paths = []

    def _setup_context_menu(self): #vers 1
        """Texture table menu is wired in _create_middle_panel (_show_texture_context_menu)."""

    def _load_txd_textures(self, *args, **kwargs): #vers 1
        """TXD load, then the texture / checker panel visibility."""
        result = super()._load_txd_textures(*args, **kwargs)
        self._update_texture_panel_visibility()
        return result

    def _reload_texture_table(self, *args, **kwargs): #vers 1
        """Table reload, then the texture / checker panel visibility."""
        result = super()._reload_texture_table(*args, **kwargs)
        self._update_texture_panel_visibility()
        return result

    def _display_mobile_textures(self, *args, **kwargs): #vers 1
        """Mobile texture DB, then the texture / checker panel visibility."""
        result = super()._display_mobile_textures(*args, **kwargs)
        self._update_texture_panel_visibility()
        return result

    def _open_ps2_txd(self, *args, **kwargs): #vers 1
        """PS2 TXD, then the texture / checker panel visibility."""
        result = super()._open_ps2_txd(*args, **kwargs)
        self._update_texture_panel_visibility()
        return result


    # CONTEXT MENU ICONS


    # WINDOW CONTROL ICONS


# Footer functions


# --- External AI upscaler integration helper ---
import sys


def open_asset_workshop(main_window, clicked_path: str = None, dat_path: str = None,
                         game: str = None) -> AssetWorkshop: #vers 1
    """Real entry point used by Dat_Browser/directory_tree_browser/
    imgfactory.py - opens Asset Workshop's Asset Check tab against
    either sibling files of clicked_path, or a whole game via
    dat_path. Dual-mode: embeds as a tab if main_window has one,
    standalone floating window otherwise."""
    if clicked_path:
        img_path, col_path, ide_path = find_sibling_asset_files(clicked_path)
        ext = os.path.splitext(clicked_path)[1].lower()
        if ext == '.img':
            img_path = clicked_path
        elif ext == '.col':
            col_path = clicked_path
        elif ext == '.ide':
            ide_path = clicked_path
        result = check_assets(img_path=img_path, col_path=col_path, ide_path=ide_path, game=game)
        tab_label = os.path.splitext(os.path.basename(clicked_path))[0]
    elif dat_path:
        img_path, col_path, ide_paths, game = find_game_asset_files(dat_path)
        if not img_path and not col_path and not ide_paths:
            QMessageBox.warning(main_window, App_name,
                f"Could not find any real IMG/COL/IDE files from:\n{dat_path}")
            return None
        result = check_assets(img_path=img_path, col_path=col_path, ide_path=ide_paths, game=game)
        tab_label = os.path.splitext(os.path.basename(dat_path))[0]
    else:
        result = check_assets()
        tab_label = App_name

    try:
        if not main_window or not hasattr(main_window, 'main_tab_widget'):
            workshop = AssetWorkshop(None, main_window)
            workshop.load_result(result)
            workshop.setWindowFlags(Qt.WindowType.Window)
            workshop.setWindowTitle(App_name)
            workshop.resize(1200, 800)
            workshop.show()
            return workshop

        tab_container = QWidget()
        tab_layout = QVBoxLayout(tab_container)
        tab_layout.setContentsMargins(0, 0, 0, 0)

        workshop = AssetWorkshop(tab_container, main_window)
        workshop.setWindowFlags(Qt.WindowType.Widget)
        tab_layout.addWidget(workshop)
        workshop.load_result(result)

        try:
            from apps.methods.imgfactory_svg_icons import get_asset_checker_icon
            icon = get_asset_checker_icon()
            idx = main_window.main_tab_widget.addTab(tab_container, icon, f"Assets: {tab_label}")
        except Exception:
            idx = main_window.main_tab_widget.addTab(tab_container, f"Assets: {tab_label}")
        main_window.main_tab_widget.setCurrentIndex(idx)
        if hasattr(main_window, '_ensure_tab_area_visible'):
            main_window._ensure_tab_area_visible()

        _register_asset_workshop_taskbar(tab_container, main_window)
        workshop.show()
        return workshop
    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error opening " + App_name + f": {e}")
        return None


def _register_asset_workshop_taskbar(widget, main_window): #vers 1
    """Register or activate the Asset Workshop button in the real
    tool taskbar."""
    try:
        tb = getattr(main_window, 'tool_taskbar', None)
        if not tb:
            return
        if 'asset_workshop' not in tb._tools:
            from apps.methods.imgfactory_svg_icons import get_asset_checker_icon
            icon = get_asset_checker_icon(16)
            tb.register('asset_workshop', 'Assets', icon, widget, App_name)
        else:
            tb._tools['asset_workshop']['target'] = widget
        if hasattr(tb, '_set_exclusive_active'):
            tb._set_exclusive_active('asset_workshop')
    except Exception:
        pass


if __name__ == "__main__":
    import sys
    import traceback

    print("Starting " + App_name)

    try:
        app = QApplication(sys.argv)
        print("QApplication created")

        workshop = AssetWorkshop()
        print(App_name + " instance created")

        workshop.load_result(check_assets())

        workshop.setWindowTitle(App_name + " - Standalone")
        workshop.resize(1200, 800)
        workshop.show()
        print("Window shown, entering event loop")
        print(f"Window visible: {workshop.isVisible()}")
        print(f"Window geometry: {workshop.geometry()}")

        sys.exit(app.exec())

    except Exception as e:
        img_debugger.error(f"{e}")
        traceback.print_exc()
        sys.exit(1)
