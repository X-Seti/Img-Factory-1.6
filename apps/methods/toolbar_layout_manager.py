#this belongs in apps/methods/toolbar_layout_manager.py - Version: 1
# X-Seti - June 2026 - IMG Factory 1.6 - Toolbar Group/Divider Customization

"""
Toolbar Layout Manager - groups buttons into labelled clusters with dividers,
matching 3ds Max's toolbar customization (drag any button anywhere, right-click
to add a divider). Separate concern from DockableToolbar, which handles where
the whole bar docks/floats - this handles what's arranged inside it.

Usage:
    glayout = GroupedToolbarLayout(parent_frame, settings_key='model_left_toolbar')
    glayout.add_group('selection', "Selection")
    glayout.add_widget('selection', sel_vert_btn)
    glayout.add_widget('selection', sel_edge_btn)
    glayout.add_group('geometry', "Edit Geometry")
    glayout.add_widget('geometry', extrude_btn)
    glayout.load_layout()   # restores saved order/groups if present, else uses above
    parent_frame.setLayout(glayout.grid)
"""

import json
from pathlib import Path

from PyQt6.QtWidgets import QWidget, QGridLayout, QFrame, QSizePolicy, QMenu
from PyQt6.QtCore import Qt, QMimeData, QPoint
from PyQt6.QtGui import QDrag, QPainter, QColor, QPen

##Methods list -
# add_divider_before
# add_group
# add_widget
# load_layout
# remove_divider_before
# save_layout
# set_columns
# set_customize_mode
# _delete_widget

##Classes -
# _DividerWidget
# GroupedToolbarLayout
# _DraggableButtonMixin


_DRAG_MIME = "application/x-imgfactory-toolbar-button"


class _DividerWidget(QFrame):
    """Thin vertical line between toolbar groups, matches 3ds Max's group
    separators. Right-click to remove."""

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.setFrameShape(QFrame.Shape.VLine)
        self.setFrameShadow(QFrame.Shadow.Sunken)
        self.setFixedWidth(6)
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)


class _DraggableButtonMixin:
    """Mixin applied to existing toolbar buttons at registration time so they
    can be dragged to a new position/group without changing their own class.
    Implemented as monkey-patched event handlers rather than subclassing,
    since buttons are constructed elsewhere (icon factory, existing code)
    and we don't want to touch their creation sites.

    Matches 3ds Max's Customize UI workflow: dragging is only possible while
    the toolbar's manager has customize_mode enabled (entered via right-click
    -> Customize, same as Max's Customize User Interface dialog gate). With
    customize_mode off, a click always just runs the button's normal action -
    no modifier-key ambiguity, no behavior change from today."""

    @staticmethod
    def install(widget, manager): #vers 2
        widget._toolbar_manager = manager
        orig_press = widget.mousePressEvent

        def mousePressEvent(ev): #vers 2
            if manager.customize_mode and ev.button() == Qt.MouseButton.LeftButton:
                drag = QDrag(widget)
                mime = QMimeData()
                mime.setData(_DRAG_MIME, str(id(widget)).encode())
                drag.setMimeData(mime)
                manager._drag_source = widget
                drag.exec(Qt.DropAction.MoveAction)
                return
            orig_press(ev)

        widget.mousePressEvent = mousePressEvent
        widget.setAcceptDrops(True)
        orig_drag_enter = getattr(widget, 'dragEnterEvent', None)

        def dragEnterEvent(ev): #vers 1
            if manager.customize_mode and ev.mimeData().hasFormat(_DRAG_MIME):
                ev.acceptProposedAction()
            elif orig_drag_enter:
                orig_drag_enter(ev)

        def dropEvent(ev): #vers 1
            if manager.customize_mode and ev.mimeData().hasFormat(_DRAG_MIME):
                src = manager._drag_source
                if src is not None and src is not widget:
                    manager._move_widget_before(src, widget)
                ev.acceptProposedAction()

        widget.dragEnterEvent = dragEnterEvent
        widget.dropEvent = dropEvent

        widget.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)

        def show_button_menu(pos): #vers 1
            if not manager.customize_mode:
                return
            # Matches the exact menu confirmed from 3ds Max 2014 (right-click
            # any toolbar button while Customize UI is active): Edit Button
            # Appearance / Delete Button / Edit Macro Script / Customize...
            m = QMenu(widget)
            m.addAction("Edit Button Appearance...")
            m.addAction("Delete Button", lambda: manager._delete_widget(widget))
            macro_act = m.addAction("Edit Macro Script")
            macro_act.setEnabled(False)   # no macro system yet - greyed, matches Max when N/A
            m.addSeparator()
            cust_act = m.addAction("Customize...")
            cust_act.setCheckable(True)
            cust_act.setChecked(True)
            cust_act.toggled.connect(manager.set_customize_mode)
            m.exec(widget.mapToGlobal(pos))

        widget.customContextMenuRequested.connect(show_button_menu)


class GroupedToolbarLayout:
    """Owns a QGridLayout of labelled button groups separated by dividers.
    Not a QWidget itself - hand the grid to whatever frame should display it
    (matches the existing icon_frame._grid pattern already used throughout
    Model/COL/TXD Workshop toolbars)."""

    def __init__(self, host_frame: QWidget, settings_key: str): #vers 1
        self.host = host_frame
        self.settings_key = settings_key
        self.grid = QGridLayout(host_frame)
        self.grid.setContentsMargins(0, 0, 0, 0)
        self.grid.setSpacing(2)
        self._groups = {}          # group_id -> list of widgets (order matters)
        self._group_order = []     # list of group_id, defines left-to-right order
        self._dividers_before = set()   # set of group_id that get a divider before them
        self._drag_source = None
        self.customize_mode = False
        host_frame.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        host_frame.customContextMenuRequested.connect(self._show_context_menu)

    def set_customize_mode(self, on: bool): #vers 1
        """Enter/exit Max-style Customize UI mode. While on: buttons become
        draggable instead of clickable, and the toolbar shows a dashed
        outline so it's visually obvious the bar is in edit mode (mirrors
        3ds Max's Customize User Interface dialog gate)."""
        self.customize_mode = on
        if on:
            self.host.setStyleSheet(
                self.host.styleSheet() +
                "\nQFrame { border: 1px dashed palette(highlight); }")
        else:
            style = self.host.styleSheet()
            self.host.setStyleSheet(
                style.replace(
                    "\nQFrame { border: 1px dashed palette(highlight); }", ""))
            self.save_layout()

    def add_group(self, group_id: str, label: str = ""): #vers 1
        """Register a new group. label is currently unused for display (no
        group header widget yet, matches Max's borderless cluster style) but
        kept for future use and for clearer config/debugging output."""
        if group_id not in self._groups:
            self._groups[group_id] = []
            self._group_order.append(group_id)

    def add_widget(self, group_id: str, widget: QWidget): #vers 1
        """Add a button/label to a group and wire up its drag handling."""
        if group_id not in self._groups:
            self.add_group(group_id)
        self._groups[group_id].append(widget)
        _DraggableButtonMixin.install(widget, self)
        self._rebuild_grid()

    def add_divider_before(self, group_id: str): #vers 1
        self._dividers_before.add(group_id)
        self._rebuild_grid()

    def remove_divider_before(self, group_id: str): #vers 1
        self._dividers_before.discard(group_id)
        self._rebuild_grid()

    def _delete_widget(self, widget: QWidget): #vers 1
        """Remove a button from the toolbar entirely (Max's 'Delete Button').
        Hides rather than destroys it, since these are real functional
        buttons wired to app logic elsewhere - destroying the QObject would
        break those connections. 'Reset to default layout' brings it back."""
        for widgets in self._groups.values():
            if widget in widgets:
                widgets.remove(widget)
                break
        widget.setParent(None)
        widget.hide()
        self._rebuild_grid()
        self.save_layout()

    def _move_widget_before(self, src_widget: QWidget, target_widget: QWidget): #vers 1
        """Relocate src_widget to sit immediately before target_widget,
        regardless of which group either currently belongs to - this is
        what makes dragging cross-group (Max-style), not just reorder
        within one cluster."""
        src_group = None
        for gid, widgets in self._groups.items():
            if src_widget in widgets:
                src_group = gid
                widgets.remove(src_widget)
                break
        if src_group is None:
            return
        for gid, widgets in self._groups.items():
            if target_widget in widgets:
                idx = widgets.index(target_widget)
                widgets.insert(idx, src_widget)
                break
        self._rebuild_grid()
        self.save_layout()

    def set_columns(self, n_cols=None): #vers 1
        """Set wrap width for the toolbar - None means single row (default,
        matches existing horizontal toolbar style); an integer wraps items
        into that many columns per row, used when the parent DockableToolbar
        is floating or docked to a side edge (vertical orientation)."""
        self._n_cols = n_cols
        self._rebuild_grid()

    def _rebuild_grid(self): #vers 2
        """Clear and re-populate the grid from current group/divider state.
        Single row by default (matches the existing flat-row toolbar style);
        wraps into self._n_cols columns per row when set via set_columns()."""
        for i in reversed(range(self.grid.count())):
            item = self.grid.itemAt(i)
            if item and item.widget():
                self.grid.removeWidget(item.widget())

        n_cols = getattr(self, '_n_cols', None)
        col = 0
        row = 0

        def _place(widget):
            nonlocal col, row
            if n_cols is not None and col >= n_cols:
                col = 0
                row += 1
            self.grid.addWidget(widget, row, col)
            widget.show()
            col += 1

        for gid in self._group_order:
            if gid in self._dividers_before and (col > 0 or row > 0):
                div = _DividerWidget(self.host)
                div.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
                div.customContextMenuRequested.connect(
                    lambda p, g=gid: self._show_divider_menu(g))
                _place(div)
            for w in self._groups[gid]:
                if w.parent() is not self.host:
                    w.setParent(self.host)
                _place(w)

    def _show_context_menu(self, pos): #vers 2
        menu = QMenu(self.host)
        cust_act = menu.addAction("Customize...")
        cust_act.setCheckable(True)
        cust_act.setChecked(self.customize_mode)
        cust_act.toggled.connect(self.set_customize_mode)
        menu.addSeparator()
        menu.addAction("Save toolbar layout", self.save_layout)
        menu.addAction("Reset to default layout", self._reset_layout)
        menu.exec(self.host.mapToGlobal(pos))

    def _show_divider_menu(self, group_id): #vers 1
        menu = QMenu(self.host)
        menu.addAction("Remove divider", lambda: self.remove_divider_before(group_id))
        menu.exec_(self.host.cursor().pos())

    def _reset_layout(self): #vers 1
        path = self._settings_path()
        try:
            data = json.loads(path.read_text())
            data.pop(f'{self.settings_key}_groups', None)
            path.write_text(json.dumps(data, indent=2))
        except Exception:
            pass

    def _settings_path(self) -> Path: #vers 1
        cfg = Path.home() / '.config' / 'imgfactory'
        cfg.mkdir(parents=True, exist_ok=True)
        return cfg / 'model_toolbar_layout.json'

    def save_layout(self): #vers 1
        """Persist group membership, order, and divider positions. Widgets
        are identified by objectName - callers must setObjectName() on every
        button added via add_widget(), or it will be skipped on save (logged,
        not silently dropped)."""
        try:
            path = self._settings_path()
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            groups_out = {}
            skipped = []
            for gid in self._group_order:
                names = []
                for w in self._groups[gid]:
                    n = w.objectName()
                    if n:
                        names.append(n)
                    else:
                        skipped.append(repr(w))
                groups_out[gid] = names
            data[f'{self.settings_key}_groups'] = {
                'order': self._group_order,
                'groups': groups_out,
                'dividers_before': list(self._dividers_before),
            }
            path.write_text(json.dumps(data, indent=2))
            if skipped:
                print(f"toolbar_layout_manager: {len(skipped)} widget(s) "
                      f"skipped on save (no objectName set): {skipped}")
            return True
        except Exception as e:
            print(f"toolbar_layout_manager: save_layout failed: {e}")
            return False

    def load_layout(self) -> bool: #vers 1
        """Restore saved group/order/divider state if present. Re-sorts
        already-registered widgets (by objectName) into the saved order;
        does not create widgets that weren't already added via add_widget -
        this only reorders/regroups, callers are still responsible for
        constructing and adding every button first."""
        try:
            path = self._settings_path()
            if not path.exists():
                return False
            data = json.loads(path.read_text())
            cfg = data.get(f'{self.settings_key}_groups')
            if not cfg:
                return False

            by_name = {}
            for widgets in self._groups.values():
                for w in widgets:
                    n = w.objectName()
                    if n:
                        by_name[n] = w

            new_groups = {}
            for gid, names in cfg.get('groups', {}).items():
                new_groups[gid] = [by_name[n] for n in names if n in by_name]

            # Any widget present now but missing from saved config (e.g.
            # newly added button since the layout was last saved) gets
            # appended to its original group so it doesn't silently vanish.
            saved_names = {n for names in cfg.get('groups', {}).values() for n in names}
            for gid, widgets in self._groups.items():
                for w in widgets:
                    if w.objectName() not in saved_names:
                        new_groups.setdefault(gid, []).append(w)

            self._groups = new_groups
            self._group_order = [g for g in cfg.get('order', []) if g in new_groups]
            for gid in new_groups:
                if gid not in self._group_order:
                    self._group_order.append(gid)
            self._dividers_before = set(cfg.get('dividers_before', []))
            self._rebuild_grid()
            return True
        except Exception as e:
            print(f"toolbar_layout_manager: load_layout failed: {e}")
            return False
