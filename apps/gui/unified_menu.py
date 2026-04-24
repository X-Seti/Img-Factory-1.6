#this belongs in apps/gui/unified_menu.py - Version: 1
# X-Seti - Apr 2026 - IMG Factory 1.6 - Unified Menu System
"""
UnifiedMenuSystem — single menu source for all apps and UI modes.

Replaces:
  - gui_menu.py        (IMGFactoryMenuBar — system UI inline QMenuBar)
  - gui_menu_custom.py (CustomMenuManager — custom UI popup QMenu)

Usage:
    # In imgfactory __init__:
    from apps.gui.unified_menu import UnifiedMenuSystem
    self.menu_system = UnifiedMenuSystem(main_window=self)
    self.menu_system.build()          # populate menus
    self.menu_system.attach_to_ui()   # wire to current UI mode

    # On tab change (docked workshop):
    self.menu_system.activate_tool(workshop)   # inserts tool menus
    self.menu_system.deactivate_tool()         # removes tool menus

    # [Menu] button (custom UI mode):
    self.menu_system.show_popup(at_widget=self.menu_btn)

    # System UI mode: menu_system.menubar is a QMenuBar — add to top bar

##Methods list -
# UnifiedMenuSystem.__init__
# UnifiedMenuSystem.build
# UnifiedMenuSystem.attach_to_ui
# UnifiedMenuSystem.show_popup
# UnifiedMenuSystem.activate_tool
# UnifiedMenuSystem.deactivate_tool
# UnifiedMenuSystem._build_file_menu
# UnifiedMenuSystem._build_edit_menu
# UnifiedMenuSystem._build_view_menu
# UnifiedMenuSystem._build_tools_menu
# UnifiedMenuSystem._build_img_menu
# UnifiedMenuSystem._build_project_menu
# UnifiedMenuSystem._build_settings_menu
# UnifiedMenuSystem._apply_to_menubar
# UnifiedMenuSystem._build_popup
# UnifiedMenuSystem._get_icon
# UnifiedMenuSystem._add
"""

from PyQt6.QtWidgets import QMenuBar, QMenu, QApplication
from PyQt6.QtGui import QAction, QKeySequence
from PyQt6.QtCore import Qt

try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVG
    _HAS_SVG = True
except ImportError:
    _HAS_SVG = False


class UnifiedMenuSystem: #vers 1
    """Single menu system for all IMG Factory apps and UI modes.

    Modes:
      'system'  — renders into a QMenuBar embedded in the top bar
      'custom'  — renders into a QMenu popup shown from [Menu] button
      'both'    — system bar present but popup also available
    """

    def __init__(self, main_window): #vers 1
        self.mw          = main_window
        self.menubar     = None     # QMenuBar for system UI mode
        self._menus      = {}       # top-level QMenu objects by key
        self._actions    = {}       # QAction objects by key
        self._tool_menu  = None     # currently injected tool QMenu
        self._tool_label = None     # label of active tool
        self._callbacks  = {}       # action key → callable

    # ── Public API ────────────────────────────────────────────────────────

    def set_callbacks(self, callbacks: dict): #vers 1
        """Register action callbacks. Keys match _add() key strings."""
        self._callbacks.update(callbacks)

    def build(self): #vers 1
        """Build all menu definitions into internal structures."""
        self._build_file_menu()
        self._build_edit_menu()
        self._build_view_menu()
        self._build_img_menu()
        self._build_tools_menu()
        self._build_project_menu()
        self._build_settings_menu()

    def attach_to_ui(self): #vers 1
        """Wire built menus to current UI mode (system bar or popup)."""
        mw = self.mw
        ui_mode = 'custom'
        try:
            ui_mode = mw.img_settings.get('ui_mode', 'custom')
        except Exception:
            pass

        if ui_mode == 'system':
            self._attach_system_bar()
        else:
            self._attach_popup_mode()

    def show_popup(self, at_widget=None): #vers 1
        """Show full menu as a QMenu popup. Called by [Menu] button."""
        popup = self._build_popup()
        if at_widget:
            pos = at_widget.mapToGlobal(at_widget.rect().bottomLeft())
        else:
            pos = QApplication.primaryScreen().geometry().center()
        popup.exec(pos)

    def activate_tool(self, workshop): #vers 1
        """Insert tool menus when a docked workshop tab becomes active."""
        self.deactivate_tool()
        if not hasattr(workshop, 'get_menu_title'):
            return
        label = workshop.get_menu_title()
        self._tool_label = label

        # Build tool submenu from workshop
        tool_menu = QMenu(label)
        if hasattr(workshop, '_build_menus_into_qmenu'):
            workshop._build_menus_into_qmenu(tool_menu)
        self._tool_menu = tool_menu

        # Inject into system bar
        if self.menubar:
            self.menubar.addMenu(tool_menu)

        # Register titlebar button (custom UI)
        gl = getattr(self.mw, 'gui_layout', None)
        if gl and hasattr(gl, 'register_tool_menu_btn'):
            def _popup():
                m = QMenu()
                if hasattr(workshop, '_build_menus_into_qmenu'):
                    workshop._build_menus_into_qmenu(m)
                btn = getattr(gl, 'tool_menu_btn', None)
                if btn:
                    m.exec(btn.mapToGlobal(btn.rect().bottomLeft()))
                else:
                    m.exec()
            gl.register_tool_menu_btn(label, _popup)

    def deactivate_tool(self): #vers 1
        """Remove tool menus when workshop tab loses focus."""
        self._tool_menu  = None
        self._tool_label = None

        # Remove from system bar
        if self.menubar and hasattr(self, '_tool_action_ref'):
            self.menubar.removeAction(self._tool_action_ref)
            self._tool_action_ref = None

        # Clear titlebar button
        gl = getattr(self.mw, 'gui_layout', None)
        if gl and hasattr(gl, 'unregister_tool_menu_btn'):
            gl.unregister_tool_menu_btn()

    # ── Menu builders ─────────────────────────────────────────────────────

    def _build_file_menu(self): #vers 1
        m = QMenu("File")
        self._add(m, "new_img",      "New IMG",         "Ctrl+N",  "new_icon")
        self._add(m, "open_img",     "Open IMG…",       "Ctrl+O",  "open_icon")
        self._add(m, "hybrid_load",  "Hybrid Load…",    None,      "open_icon")
        m.addSeparator()
        self._add(m, "scan_folder",  "Scan Folder…",    None,      "folder_icon")
        self._add(m, "recent_scans", "Recent Scans",    None,      None)
        m.addSeparator()
        self._add(m, "reload",       "Reload",          "F5",      "refresh_icon")
        m.addSeparator()
        self._add(m, "encrypt",      "Encrypt…",        None,      None)
        self._add(m, "close_img",    "Close",           "Ctrl+W",  "close_icon")
        self._add(m, "close_all",    "Close All",       None,      "close_icon")
        m.addSeparator()
        self._add(m, "rebuild",      "Rebuild",         None,      None)
        self._add(m, "rebuild_ai",   "Rebuild AI",      None,      None)
        self._add(m, "save_entry",   "Save Entry",      "Ctrl+S",  "save_icon")
        m.addSeparator()
        self._add(m, "merge",        "Merge…",          None,      None)
        self._add(m, "split_via",    "Split via…",      None,      None)
        self._add(m, "convert",      "Convert…",        None,      None)
        self._menus['file'] = m

    def _build_edit_menu(self): #vers 1
        m = QMenu("Edit")
        self._add(m, "select_all",   "Select All",      "Ctrl+A",  None)
        self._add(m, "deselect",     "Deselect",        None,      None)
        self._add(m, "inverse_sel",  "Inverse Select",  None,      None)
        m.addSeparator()
        self._add(m, "find",         "Find…",           "Ctrl+F",  "search_icon")
        self._add(m, "replace",      "Replace…",        "Ctrl+H",  None)
        m.addSeparator()
        self._add(m, "rename",       "Rename Entry",    "F2",      "edit_icon")
        self._add(m, "sort",         "Sort Entries",    None,      None)
        self._add(m, "pin",          "Pin Entry",       None,      None)
        self._menus['edit'] = m

    def _build_view_menu(self): #vers 1
        m = QMenu("View")
        self._add(m, "toggle_dir",   "Dir Tree",        None,      "folder_icon")
        self._add(m, "toggle_dat",   "DAT Browser",     None,      "database_icon")
        self._add(m, "toggle_intro", "Intro / Welcome", None,      "info_icon")
        m.addSeparator()
        self._add(m, "toggle_log",   "Activity Log",    None,      None)
        self._add(m, "toggle_status","Status Bar",      None,      None)
        self._menus['view'] = m

    def _build_img_menu(self): #vers 1
        m = QMenu("IMG")
        self._add(m, "imp",          "Import",          None,      "import_icon")
        self._add(m, "imp_via",      "Import via…",     None,      "import_icon")
        self._add(m, "exp",          "Export",          None,      "export_icon")
        self._add(m, "exp_via",      "Export via…",     None,      "export_icon")
        m.addSeparator()
        self._add(m, "dump",         "Dump",            None,      None)
        self._add(m, "extract",      "Extract",         None,      None)
        m.addSeparator()
        self._add(m, "rem",          "Remove",          "Del",     "delete_icon")
        self._add(m, "rem_via",      "Remove via…",     None,      "delete_icon")
        self._menus['img'] = m

    def _build_tools_menu(self): #vers 1
        m = QMenu("Tools")
        self._add(m, "open_txd",     "TXD Workshop",    None,      None)
        self._add(m, "open_col",     "COL Workshop",    None,      None)
        self._add(m, "open_model",   "Model Workshop",  None,      None)
        self._add(m, "open_dp5",     "DP5 Paint",       None,      None)
        self._add(m, "open_radar",   "Radar Workshop",  None,      None)
        m.addSeparator()
        self._add(m, "open_dat",     "DAT Browser",     None,      None)
        self._add(m, "open_dir",     "File Browser",    None,      "folder_icon")
        m.addSeparator()
        self._add(m, "open_ai",      "AI Workshop",     None,      None)
        self._menus['tools'] = m

    def _build_project_menu(self): #vers 1
        m = QMenu("Project")
        self._add(m, "new_project",    "New Project…",      None, None)
        self._add(m, "open_project",   "Open Project…",     None, None)
        self._add(m, "save_project",   "Save Project",      None, "save_icon")
        m.addSeparator()
        self._add(m, "set_game_root",  "Set Game Root…",    None, "folder_icon")
        self._add(m, "set_proj_folder","Set Project Folder…",None,"folder_icon")
        self._menus['project'] = m

    def _build_settings_menu(self): #vers 1
        m = QMenu("Settings")
        self._add(m, "open_settings", "App Settings…",     "Ctrl+,", "settings_icon")
        self._add(m, "open_gui_set",  "GUI Settings…",     None,      "settings_icon")
        m.addSeparator()
        self._add(m, "about",         "About IMG Factory…",None,      "info_icon")
        self._menus['settings'] = m

    # ── Rendering ─────────────────────────────────────────────────────────

    def _attach_system_bar(self): #vers 1
        """Create / populate a QMenuBar for system UI mode."""
        if not self.menubar:
            self.menubar = QMenuBar()
        else:
            self.menubar.clear()
        self._apply_to_menubar(self.menubar)

    def _attach_popup_mode(self): #vers 1
        """Custom UI mode — no persistent menubar; popup built on demand."""
        self.menubar = None

        # Also suppress native Qt menubar
        try:
            nb = self.mw.menuBar()
            nb.clear()
            nb.setVisible(False)
            nb.setFixedHeight(0)
        except Exception:
            pass

    def _apply_to_menubar(self, bar: QMenuBar): #vers 1
        """Add all top-level menus to a QMenuBar."""
        order = ['file', 'edit', 'view', 'img', 'tools', 'project', 'settings']
        for key in order:
            m = self._menus.get(key)
            if m:
                bar.addMenu(m)

    def _build_popup(self) -> QMenu: #vers 1
        """Build a flat QMenu containing all menus as submenus."""
        popup = QMenu()
        order = ['file', 'edit', 'view', 'img', 'tools', 'project', 'settings']
        for key in order:
            m = self._menus.get(key)
            if m:
                popup.addMenu(m)
        if self._tool_menu:
            popup.addSeparator()
            popup.addMenu(self._tool_menu)
        return popup

    # ── Helpers ───────────────────────────────────────────────────────────

    def _add(self, menu: QMenu, key: str, label: str,
             shortcut=None, icon_method=None): #vers 1
        """Add an action to menu, wire callback if registered."""
        action = QAction(label, menu)
        if shortcut:
            action.setShortcut(QKeySequence(shortcut))
        if icon_method and _HAS_SVG:
            try:
                fn = getattr(_SVG, icon_method, None)
                if fn:
                    action.setIcon(fn(16))
            except Exception:
                pass
        cb = self._callbacks.get(key)
        if cb:
            action.triggered.connect(cb)
        menu.addAction(action)
        self._actions[key] = action
        return action

    def _get_icon(self, method_name: str): #vers 1
        """Get a QIcon from SVGIconFactory by method name."""
        if _HAS_SVG and method_name:
            try:
                fn = getattr(_SVG, method_name, None)
                if fn:
                    return fn(16)
            except Exception:
                pass
        return None

    def wire_standard_callbacks(self): #vers 1
        """Wire standard imgfactory callbacks from the main window."""
        mw = self.mw
        standard = {
            # File
            'new_img':       getattr(mw, 'create_new_img',        None),
            'open_img':      getattr(mw, 'open_img_file',         None),
            'hybrid_load':   getattr(mw, 'open_hybrid_load',      None),
            'scan_folder':   getattr(mw, 'scan_folder',           None),
            'reload':        getattr(mw, 'reload_img',            None),
            'close_img':     getattr(mw, 'close_current_img',     None),
            'close_all':     getattr(mw, 'close_all_imgs',        None),
            'rebuild':       getattr(mw, 'rebuild_img',           None),
            'rebuild_ai':    getattr(mw, 'rebuild_img_ai',        None),
            'save_entry':    getattr(mw, 'save_img_entry',        None),
            'merge':         getattr(mw, 'merge_img',             None),
            'convert':       getattr(mw, 'convert_img',           None),
            # Edit
            'find':          getattr(mw, '_show_search_dialog',   None),
            'rename':        getattr(mw, 'rename_entry',          None),
            # View
            'toggle_dir':    getattr(mw, 'toggle_dir_tree',       None),
            'toggle_dat':    lambda: _open_dat(mw),
            'toggle_intro':  lambda: _open_intro(mw),
            # IMG
            'imp':           getattr(mw, 'import_file',           None),
            'exp':           getattr(mw, 'export_selected',       None),
            'dump':          getattr(mw, 'dump_entries',          None),
            'extract':       getattr(mw, 'extract_selected',      None),
            'rem':           getattr(mw, 'remove_selected',       None),
            # Tools
            'open_txd':      getattr(mw, 'open_txd_editor',       None),
            'open_col':      getattr(mw, 'open_col_editor',       None),
            'open_model':    getattr(mw, 'open_model_editor',     None),
            'open_dp5':      getattr(mw, 'open_dp5_workshop_docked', None),
            'open_dat':      lambda: _open_dat(mw),
            'open_dir':      getattr(mw, 'toggle_dir_tree',       None),
            'open_ai':       getattr(mw, 'open_ai_workshop',      None),
            # Settings
            'open_settings': getattr(mw, 'show_gui_settings',     None),
            'about':         getattr(mw, 'show_about',            None),
        }
        # Filter None values
        self.set_callbacks({k: v for k, v in standard.items() if v})
        # Re-wire actions that already exist
        for key, cb in self._callbacks.items():
            action = self._actions.get(key)
            if action and cb:
                try:
                    action.triggered.disconnect()
                except Exception:
                    pass
                action.triggered.connect(cb)


def _open_dat(mw):
    """Helper — open DAT browser via gui_layout."""
    try:
        from apps.gui.gui_layout_custom import _show_dat_browser
        _show_dat_browser(mw)
    except Exception:
        pass


def _open_intro(mw):
    """Helper — toggle intro panel."""
    try:
        from apps.gui.gui_layout_custom import _show_intro_panel
        _show_intro_panel(mw)
    except Exception:
        pass
