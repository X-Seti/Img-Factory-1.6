#this belongs in apps/gui/button_groups.py - Version: 5
# X-Seti - October 29 2026 - IMG Factory 1.6 - Button groups

"""
IMG Factory button groups (IMG, Entries, Editing Options, user groups) shown
as dock panels with a grid or as ribbons. Hidden toolbars of QActions are the
model the Ribbon Manager edits; panels / ribbons are rebuilt from it.
"""

##Methods list -
#
##class _DockWidthWatch: -
# __init__
# eventFilter
#
##class ButtonGroupsMixin: -
# _build_button_model
# _apply_button_text_mode
# _build_button_views
# _button_display_mode
# _button_mode
# _clear_button_views
# _get_icon_color
# _on_button_dock_resized
# _outer_dock_menu
# _reflow_dock
# _save_toolbar_state
# _set_button_mode
# _set_button_display_mode
# _set_buttons_icon_text
# _set_status
# _toolbar_context_menu
# _view_button
# open_ribbon_manager

import json

from PyQt6.QtCore import QEvent, QObject, QSize, Qt
from PyQt6.QtGui import QAction, QIcon
from PyQt6.QtWidgets import (QDialog, QFrame, QGridLayout, QGroupBox, QMainWindow, QMenu,
                             QScrollArea, QToolBar)

from apps.locals.localization import tr_button
from apps.methods.dock_panels import add_outer_dock
from apps.methods.ribbon_dialog import RibbonIconsMixin, RibbonManagerDialog

# (toolbar key, title, data method, button list attribute)
GROUPS = (('img', "IMG, COL, TXD Files", '_get_img_buttons_data', 'img_buttons'),
          ('entries', "File Entries", '_get_entry_buttons_data', 'entry_buttons'),
          ('options', "Editing Options", '_get_options_buttons_data', 'options_buttons'))
GRID_COLUMNS = 3


class _DockWidthWatch(QObject): #vers 1
    """Calls back when a watched button dock is resized."""

    def __init__(self, callback, parent): #vers 1
        super().__init__(parent)
        self._callback = callback

    def eventFilter(self, obj, event): #vers 1
        if event.type() == QEvent.Type.Resize:
            self._callback()
        return False


class ButtonGroupsMixin(RibbonIconsMixin): #vers 1
    """Button groups for IMGFactoryGUILayout: model toolbars, panel / ribbon views."""

    def _build_button_model(self): #vers 1
        """Hidden toolbars of actions; saved order, hidden buttons, user groups applied."""
        self._inner_mw = QMainWindow()                   # never shown
        self._ribbon_actions, self._action_rows = [], {}
        self._button_docks, self._button_ribbons = [], []
        dark = self._is_dark_theme()
        for key, title, data_fn, store in GROUPS:
            tb = QToolBar(title, self._inner_mw)
            tb.setObjectName(key)
            self._inner_mw.addToolBar(tb)
            for row in getattr(self, data_fn)():
                act = QAction(tr_button(row[0]), tb)
                act.setIcon(self._get_svg_icon(row[2], dark, bg_color=row[3]) or QIcon())
                tb.addAction(act)
                self._action_rows[act] = (store, tuple(row))
                self._ribbon_actions.append({'name': row[4], 'action': act, 'toolbar': tb})
        self._apply_custom_icons()                       # first call applies the saved layout

    def _button_mode(self) -> str: #vers 1
        """'panels' (docks with a grid) or 'ribbons'."""
        try:
            return json.loads(self._ribbon_config_path().read_text()).get('button_layout', 'panels')
        except (OSError, ValueError):
            return 'panels'

    def _set_button_mode(self, mode: str): #vers 1
        """Switch panels / ribbons, rebuild and save."""
        self._ribbon_config_set('button_layout', mode)
        self._build_button_views()
        self._save_outer_layout()

    def _clear_button_views(self): #vers 1
        """Remove button docks / ribbons and empty the button lists."""
        mw = self._outer_mw
        for d in self._button_docks:
            mw.removeDockWidget(d)
            self._outer_docks.remove(d)
            d.setParent(None)                            # gone now, not at deleteLater
            d.deleteLater()
        for t in self._button_ribbons:
            mw.removeToolBar(t)
            t.setParent(None)
            t.deleteLater()
        self._button_docks, self._button_ribbons = [], []
        for _k, _t, _f, store in GROUPS:
            getattr(self, store).clear()
            getattr(self.backend, store).clear()

    def _build_button_views(self): #vers 5
        """Each model toolbar as a grid dock (panels) or a ribbon; positions kept."""
        mw = self._outer_mw
        rebuild = bool(self._button_docks or self._button_ribbons)
        state = mw.saveState(self._OUTER_LAYOUT_VERSION) if rebuild else None
        self._clear_button_views()
        ribbons = self._button_mode() == 'ribbons'
        icons = self._custom_icons()
        for tb in self._inner_mw.findChildren(QToolBar):
            items = [a for a in tb.actions() if a.isSeparator() or (a.isVisible() and a in self._action_rows)]
            if not any(not a.isSeparator() for a in items):
                continue                                 # empty group: nothing to show
            if ribbons:
                view = QToolBar(tb.windowTitle(), mw)
                view.setObjectName('ribbon:' + tb.objectName())
                view.setFloatable(False)                 # Wayland can't re-dock a floating toolbar
                view.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
                view.customContextMenuRequested.connect(
                    lambda pos, v=view: self._outer_dock_menu(v.mapToGlobal(pos)))
                if self._button_ribbons:                 # one ribbon per row until moved
                    mw.addToolBarBreak(Qt.ToolBarArea.TopToolBarArea)
                mw.addToolBar(Qt.ToolBarArea.TopToolBarArea, view)
                for a in items:
                    if a.isSeparator():
                        view.addSeparator()
                    else:
                        view.addWidget(self._view_button(a, icons))
                self._button_ribbons.append(view)
                continue
            w = QGroupBox()                              # group box: theme frame and panel image
            w.setStyleSheet("QGroupBox { margin-top: 0px; padding-top: 0px; }")   # no title gap
            w.setMinimumWidth(1)                         # dock narrows past the labels (auto: icons)
            grid = QGridLayout(w)
            grid.setContentsMargins(4, 4, 4, 4)
            cs = getattr(getattr(self.main_window, 'app_settings', None), 'current_settings', None) or {}
            grid.setHorizontalSpacing(cs.get('button_spacing_horizontal', 6))
            grid.setVerticalSpacing(cs.get('button_spacing_vertical', 8))
            scroll = QScrollArea()                       # narrow docks: icons wrap, never stretch the window
            scroll.setWidgetResizable(True)
            scroll.setFrameShape(QFrame.Shape.NoFrame)
            scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
            scroll.setWidget(w)
            scroll.setMinimumHeight(40)
            dock = add_outer_dock(self, mw, tb.windowTitle(), scroll, 48, Qt.DockWidgetArea.RightDockWidgetArea)
            dock._grid = grid
            dock._grid_items = [None if a.isSeparator() else self._view_button(a, icons) for a in items]
            dock._grid_cols = None
            self._reflow_dock(dock)
            dock.installEventFilter(self._dock_watch)
            self._button_docks.append(dock)
            self._outer_docks.append(dock)
        if state is not None:
            mw.restoreState(state, self._OUTER_LAYOUT_VERSION)
            for v in self._button_docks + self._button_ribbons:
                v.setVisible(True)                       # not in the old state: shown
        self._update_all_buttons_display_mode()
        self._apply_button_text_mode()
        if rebuild:                                      # new panels / buttons: image, transparency
            self.main_window._apply_panel_effects_now()

    def _view_button(self, act, icons: dict): #vers 1
        """Pastel button for a model action; added to its button list."""
        store, (label, action_type, icon, color, method_name) = self._action_rows[act]
        cs = getattr(getattr(self.main_window, 'app_settings', None), 'current_settings', None) or {}
        height = cs.get('button_height', 32)
        btn = self.create_pastel_button(label, action_type, icon, color, method_name)
        btn.setProperty('method_name', method_name)
        btn.setMaximumHeight(height)
        btn.setMinimumHeight(height - 4)
        if method_name in icons:                         # Ribbon Manager icon for icon-only mode
            btn.setProperty('stored_icon', act.icon())
        if action_type == "dat_edit":                    # right-click menu on Dat Edit
            btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            btn.customContextMenuRequested.connect(lambda pos, b=btn: self._dat_edit_context_menu(b, pos))
        getattr(self, store).append(btn)
        getattr(self.backend, store).append(btn)
        return btn

    def _button_display_mode(self) -> str: #vers 1
        """Settings Buttons tab: 'both', 'icons', 'text' or 'auto' (icons when narrow)."""
        cs = getattr(getattr(self.main_window, 'app_settings', None), 'current_settings', None) or {}
        return cs.get('button_display_mode', 'auto')

    def _apply_button_text_mode(self): #vers 3
        """Icons, text, both or auto (panels: icons when narrow)."""
        mode = self._button_display_mode()
        ribbons = self._button_mode() == 'ribbons'
        if mode == 'auto' and ribbons:
            mode = 'icons'
        if mode == 'auto' and not ribbons:
            self._right_panel_icon_only = None           # re-evaluate the width
            self._on_button_dock_resized()
            return
        if mode == 'both':
            self._set_buttons_icon_text()
        else:
            self._set_right_panel_icon_only(mode == 'icons')
        for d in self._button_docks:
            self._reflow_dock(d)

    def _set_button_display_mode(self, mode: str): #vers 1
        """Right-click choice: saved to Settings (Buttons tab) and applied."""
        st = self.main_window.app_settings
        st.current_settings['button_display_mode'] = mode
        st.save_settings()
        self.main_window._apply_button_display_mode_from_settings()

    def _set_buttons_icon_text(self): #vers 1
        """Every button shows its icon and its label."""
        for btn in self.img_buttons + self.entry_buttons + self.options_buttons:
            icon = btn.property("stored_icon")
            if icon:
                btn.setIcon(icon)
                btn.setIconSize(QSize(16, 16))
            btn.setText(btn.property("full_label") or btn.text())
            btn.setMinimumSize(0, 0)
            btn.setMaximumSize(16777215, 16777215)
            btn.setFixedHeight(max(28, btn.sizeHint().height()))
        self._right_panel_icon_only = False

    def _on_button_dock_resized(self): #vers 4
        """Auto: docks under 250 px show icons; icon grids refit their columns."""
        if self._button_mode() == 'ribbons':
            return
        mode = self._button_display_mode()
        widths = [d.width() for d in self._button_docks if d.isVisible() and not d.isFloating()]
        if mode == 'auto' and widths:
            narrow = min(widths) < 250
            if narrow is not self._right_panel_icon_only:
                if narrow:
                    self._set_right_panel_icon_only(True)
                elif mode == 'both':
                    self._set_buttons_icon_text()
                else:
                    self._set_right_panel_icon_only(False)
        for d in self._button_docks:
            self._reflow_dock(d)

    def _reflow_dock(self, dock): #vers 1
        """Lay a dock's buttons in 3 columns, or as many icons as fit."""
        grid, cols = dock._grid, GRID_COLUMNS
        if self._right_panel_icon_only:
            sp = max(0, grid.horizontalSpacing())
            cols = max(1, (dock.width() - 12 + sp) // (36 + sp))
        if dock._grid_cols == cols:
            return
        dock._grid_cols = cols
        while grid.count():
            grid.takeAt(0)                               # buttons stay, positions change
        for row in range(grid.rowCount()):
            grid.setRowStretch(row, 0)
        r = c = 0
        for b in dock._grid_items:
            if b is None:                                # divider: next row
                if c:
                    r, c = r + 1, 0
                continue
            grid.addWidget(b, r, c)
            c += 1
            if c == cols:
                r, c = r + 1, 0
        grid.setRowStretch(r + 1, 1)

    def _outer_dock_menu(self, global_pos, menu=None): #vers 4
        """Panel / ribbon toggles, panels / ribbons switch, status bar place, Ribbon Manager."""
        own = menu is None
        if own:
            menu = QMenu(self._dock_host())
        for t in self._button_ribbons:
            a = menu.addAction(t.windowTitle())
            a.setCheckable(True)
            a.setChecked(t.isVisible())
            a.toggled.connect(t.setVisible)
        super()._outer_dock_menu(global_pos, menu)
        menu.addSeparator()
        a = menu.addAction("Buttons as Ribbons")
        a.setCheckable(True)
        a.setChecked(self._button_mode() == 'ribbons')
        a.toggled.connect(lambda on: self._set_button_mode('ribbons' if on else 'panels'))
        btn_menu = menu.addMenu("Button Text")
        cur = self._button_display_mode()
        for key, label in (('icons', "Show Icons Only"), ('text', "Show Text Only"),
                           ('both', "Show Both"), ('auto', "Auto (icons when narrow)")):
            b = btn_menu.addAction(label)
            b.setCheckable(True)
            b.setChecked(cur == key)
            b.triggered.connect(lambda _c, k=key: self._set_button_display_mode(k))
        fw = menu.addAction("File Window in a Dock")
        fw.setCheckable(True)
        fw.setChecked(self._files_dock is not None)
        fw.toggled.connect(self._set_file_window_docked)
        sub = menu.addMenu("Status Bar")
        where = self._statusbar_position()
        for key, label in (('window', "Window bottom"), ('pane', "Under file pane"), ('dock', "Dock (place anywhere)")):
            s = sub.addAction(label)
            s.setCheckable(True)
            s.setChecked(where == key)
            s.triggered.connect(lambda _c, k=key: self._set_statusbar_position(k))
        if own:
            menu.exec(global_pos)

    def open_ribbon_manager(self): #vers 1
        """Ribbon Manager on the button groups; Cancel restores the saved order."""
        if RibbonManagerDialog(self, parent=self.main_window).exec() != QDialog.DialogCode.Accepted:
            self._apply_ribbon_layout()

    def _save_toolbar_state(self): #vers 1
        """Ribbon Manager OK: save order / hidden / groups, rebuild the views."""
        self._save_ribbon_layout()
        self._build_button_views()
        self._save_outer_layout()

    def _toolbar_context_menu(self, toolbar, pos): #vers 1
        """New Ribbon Manager groups: same menu as the panels."""
        self._outer_dock_menu(toolbar.mapToGlobal(pos))

    def _set_status(self, text: str): #vers 1
        self.main_window.log_message(text)

    def _get_icon_color(self) -> str: #vers 1
        """Theme text colour for icons."""
        return (self._get_theme_colors("default") or {}).get('text_primary', '#000000')
