#this belongs in apps/gui/tool_menu_mixin.py - Version: 1
# X-Seti - Apr11 2026 - IMG Factory 1.6 - Shared tool menu mixin
"""
ToolMenuMixin — shared menu orientation logic for all dockable workshops.

Any tool that docks into imgfactory can inherit this mixin to get:
  - Topbar mode:   internal QMenuBar shown inside the tool widget
  - Dropdown mode: menus injected into imgfactory's _system_menu_bar

Usage in a workshop class:
    class TXDWorkshop(ToolMenuMixin, QWidget):
        def get_menu_title(self): return "TXD Workshop"
        def _build_menus_into_qmenu(self, parent_menu): ...
        def _init_tool_menu(self, parent_widget, layout):
            # Call after layout is set up, passing the widget and its layout
            super()._init_tool_menu(parent_widget, layout)

The tool's per-tool settings JSON should include 'menu_style': 'topbar'|'dropdown'.
The tool must expose a `tool_settings` attribute (or override `_get_tool_menu_style()`).
"""

from PyQt6.QtWidgets import QMenuBar, QGroupBox, QVBoxLayout, QRadioButton
from PyQt6.QtCore import Qt


class ToolMenuMixin:
    """Mixin providing topbar/dropdown menu orientation for dockable tools."""

    # ── Subclass must override these ──────────────────────────────────────

    def get_menu_title(self) -> str: #vers 1
        """Return the label used in imgfactory's menu bar, e.g. 'TXD Workshop'."""
        return "Tool"

    def _build_menus_into_qmenu(self, parent_menu): #vers 1
        """Populate parent_menu (QMenu) with all tool actions.
        Called by imgfactory when injecting into dropdown.
        Must be overridden by each tool.
        """
        pass

    # ── Internal helpers ──────────────────────────────────────────────────

    def _get_tool_menu_style(self) -> str: #vers 1
        """Read menu_style from tool's settings. Override if settings key differs."""
        ts = getattr(self, 'tool_settings', None)
        if ts and hasattr(ts, 'get'):
            return ts.get('menu_style', 'dropdown')
        return 'dropdown'

    def _init_tool_menu(self, parent_widget, layout): #vers 1
        """Create and insert the internal tool menubar into the tool's layout.
        Call this from the tool's UI setup, before adding the main content widget.
        layout must be a QVBoxLayout.
        """
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
        """Build menus directly into a QMenuBar (for internal topbar).
        By default calls _build_menus_into_qmenu for each top-level group.
        Tools that need QMenuBar-specific population can override this.
        """
        # Create a single top-level menu in the bar that delegates to the tool
        # Override this in tools that have multiple top-level menus
        from PyQt6.QtWidgets import QMenu
        proxy = QMenu(self.get_menu_title())
        self._build_menus_into_qmenu(proxy)
        for action in proxy.actions():
            menubar.addAction(action)

    def set_menu_orientation(self, style: str): #vers 1
        """Switch between 'topbar' (internal bar) and 'dropdown' (imgfactory bar).
        Saves to tool settings and applies live.
        """
        # Save
        ts = getattr(self, 'tool_settings', None)
        if ts and hasattr(ts, 'set'):
            ts.set('menu_style', style)

        # Apply to internal bar
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

        # Notify imgfactory to re-inject menus
        mw = getattr(self, 'main_window', None)
        if mw and hasattr(mw, 'menu_bar_system'):
            if style == 'dropdown':
                mw.menu_bar_system._inject_tool_menu(self)
            else:
                mw.menu_bar_system._remove_tool_menu()

    def _create_menu_orientation_group(self) -> QGroupBox: #vers 1
        """Create a 'Menu Orientation' settings group widget for embedding
        in the tool's own Settings dialog.
        """
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
