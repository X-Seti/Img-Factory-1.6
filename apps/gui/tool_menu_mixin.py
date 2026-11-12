#this belongs in apps/gui/tool_menu_mixin.py - Version: 3
# X-Seti - Apr11 2026 - IMG Factory 1.6 - Shared tool menu mixin
"""
ToolMenuMixin — shared menu orientation + titlebar button for dockable workshops.

Any tool that docks into imgfactory inherits:
  - Topbar mode:   internal QMenuBar shown inside the tool widget
  - Dropdown mode: menus injected into imgfactory's _system_menu_bar

Each tool must implement:
    get_menu_title()            -> short label e.g. 'COL', 'DFF', 'TXD', 'DP5'
    _build_menus_into_qmenu()   -> populate a QMenu with all actions
"""

from PyQt6.QtWidgets import QMenuBar, QGroupBox, QVBoxLayout, QRadioButton
from PyQt6.QtCore import Qt


class ToolMenuMixin:
    """Mixin providing topbar/dropdown menu orientation for dockable tools."""

    #    Subclass must override these                                       

    def get_menu_title(self) -> str: #vers 1
        """Short label of the tool's submenu in imgfactory's Menu, e.g. 'COL'."""
        return "Tool"

    def _build_menus_into_qmenu(self, parent_menu): #vers 1
        """Populate parent_menu (QMenu) with all tool actions.
        Called by imgfactory when injecting into its Menu.
        Must be overridden by each tool.
        """
        pass

    #    Internal helpers                                                   

    def _get_tool_menu_style(self) -> str: #vers 1
        """Read menu_style from tool's settings. Override if settings key differs."""
        ts = getattr(self, 'tool_settings', None)
        if ts and hasattr(ts, 'get'):
            return ts.get('menu_style', 'dropdown')
        return 'dropdown'

    def _init_tool_menu(self, parent_widget, layout): #vers 1
        """Create and insert the internal tool menubar into the tool's layout."""
        mb = QMenuBar(parent_widget)
        self._tool_menu_bar = mb
        self._build_menus_into_qmenu_for_bar(mb)

        style = self._get_tool_menu_style()
        if style == 'topbar':
            mb.setMinimumHeight(0)
            mb.setMaximumHeight(16777215)
            mb.setVisible(True)
        else:
            mb.setVisible(False)
            mb.setMinimumHeight(0)
            mb.setMaximumHeight(0)

        layout.insertWidget(0, mb)

    def _build_menus_into_qmenu_for_bar(self, menubar): #vers 1
        """Build menus into a QMenuBar (internal topbar mode)."""
        from PyQt6.QtWidgets import QMenu
        proxy = QMenu(self.get_menu_title())
        self._build_menus_into_qmenu(proxy)
        for action in proxy.actions():
            menubar.addAction(action)

    def set_menu_orientation(self, style: str): #vers 1
        """Switch between 'topbar' and 'dropdown' mode."""
        ts = getattr(self, 'tool_settings', None)
        if ts and hasattr(ts, 'set'):
            ts.set('menu_style', style)

        if hasattr(self, '_tool_menu_bar'):
            mb = self._tool_menu_bar
            if style == 'topbar':
                mb.setMinimumHeight(0)
                mb.setMaximumHeight(16777215)
                mb.setVisible(True)
                mb.updateGeometry()
            else:
                mb.setVisible(False)
                mb.setMinimumHeight(0)
                mb.setMaximumHeight(0)

        mw = getattr(self, 'main_window', None)
        if mw and hasattr(mw, 'menu_bar_system'):
            if style == 'dropdown':
                mw.menu_bar_system._inject_tool_menu(self)
            else:
                mw.menu_bar_system._remove_tool_menu()

    def _create_menu_orientation_group(self) -> QGroupBox: #vers 1
        """Create a 'Menu Orientation' settings group widget."""
        style = self._get_tool_menu_style()
        group = QGroupBox(f"{self.get_menu_title()} — Menu Orientation")
        layout = QVBoxLayout(group)
        layout.setSpacing(4)

        self._menu_topbar_radio   = QRadioButton("Topbar  (inside tool panel)")
        self._menu_dropdown_radio = QRadioButton("Dropdown  (in imgfactory menubar)")
        self._menu_topbar_radio.setChecked(style == 'topbar')
        self._menu_dropdown_radio.setChecked(style != 'topbar')

        def _on_changed():
            new_style = 'topbar' if self._menu_topbar_radio.isChecked() else 'dropdown'
            self.set_menu_orientation(new_style)

        self._menu_topbar_radio.toggled.connect(_on_changed)
        layout.addWidget(self._menu_topbar_radio)
        layout.addWidget(self._menu_dropdown_radio)
        return group
