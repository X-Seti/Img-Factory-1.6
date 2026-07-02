#!/usr/bin/env python3
#this belongs in apps/methods/ribbon_manager.py - Version: 5
# X-Seti - June 2026 - IMG Factory 1.6 - Ribbon Manager

"""
Ribbon Manager — create/delete/rename DockableToolbar instances and
bulk-assign buttons between them via a two-pane dialog.

Sits ABOVE the per-bar GroupedToolbarLayout/Customize system:
  - GroupedToolbarLayout / Customize...  — in-bar button order + dividers
  - RibbonManager / RibbonManagerDialog  — which buttons live on which bar,
                                            whole-bar lifecycle (create/delete)

Button stable identity: every tracked button has a _ribbon_id (UUID string)
set once at construction via tag_button(). objectName() is for Qt/layout
persistence; _ribbon_id is for the ribbon manager's cross-bar tracking.

Undo: QUndoStack with named QUndoCommand subclasses. Undo/redo works in
both the dialog AND the live UI (stack is shared, not dialog-local).
"""

import uuid
import json
from pathlib import Path
from typing import Optional

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QListWidget, QListWidgetItem,
    QPushButton, QLabel, QSplitter, QWidget, QFrame, QMessageBox,
    QDialogButtonBox, QToolBar, QAbstractItemView, QMenu,
)
from PyQt6.QtCore import Qt, QMimeData, QSize
from PyQt6.QtGui import QUndoStack, QUndoCommand, QIcon, QKeySequence

##Methods list -
# RibbonRegistry.__init__
# RibbonRegistry.all_buttons
# RibbonRegistry.find_button
# RibbonRegistry.get_ribbon
# RibbonRegistry.next_ribbon_id
# RibbonRegistry.register_ribbon
# RibbonRegistry.save_state
# RibbonRegistry.load_state
# RibbonRegistry.unregister_ribbon
# tag_button


# ------------------------------------------------------------------ #
# Button tagging utility                                               #
# ------------------------------------------------------------------ #

def tag_button(widget, icon_fn=None) -> str: #vers 1
    """Assign a stable _ribbon_id UUID to a toolbar button if it doesn't
    already have one. Also sets _icon_fn if provided. Returns the id.

    Call this once per button at construction time — the id never changes,
    even if the button moves between ribbons or into the Unassigned pool."""
    if not hasattr(widget, '_ribbon_id') or not widget._ribbon_id:
        widget._ribbon_id = str(uuid.uuid4())
    if icon_fn is not None:
        widget._icon_fn = icon_fn
    return widget._ribbon_id


# ------------------------------------------------------------------ #
# Registry                                                             #
# ------------------------------------------------------------------ #

class RibbonRegistry: #vers 1
    """Tracks every DockableToolbar and its GroupedToolbarLayout in the
    active ModelWorkshop session. Each ribbon has a stable integer ID
    (never reused after deletion) and a display name ("Ribbon N").

    The registry is workshop-instance-local (one per ModelWorkshop), not
    global, so multiple workshop windows each have their own isolated state.
    """

    _SETTINGS_FILE = Path.home() / '.config' / 'imgfactory' / 'ribbon_registry.json'

    def __init__(self, workshop): #vers 1
        self._workshop = workshop   # ModelWorkshop instance
        self._ribbons = {}          # id -> {'name', 'toolbar', 'layout', 'buttons': [rid]}
        self._buttons = {}          # ribbon_id -> QPushButton/QLabel widget
        self._unassigned = []       # list of ribbon_ids not on any ribbon
        self._next_id = 1           # monotonically increasing, never reused
        self._undo_stack = QUndoStack(workshop)
        self._undo_stack.setUndoLimit(50)

        # Default layout snapshot — taken once when the registry is first
        # populated (i.e. when Model Workshop first opens after this code
        # lands). Used by "Reset to default layout" at the whole-workshop level.
        self._default_snapshot = None

    @property
    def undo_stack(self) -> QUndoStack: #vers 1
        return self._undo_stack

    def next_ribbon_id(self) -> int: #vers 1
        """Return the next available ribbon ID, incrementing the counter.
        IDs are never reused — deleting Ribbon 3 doesn't give the next
        ribbon id=3, it gets id=4 (or whatever the counter is at)."""
        rid = self._next_id
        self._next_id += 1
        return rid

    def register_ribbon(self, toolbar, layout, name: str = '') -> int: #vers 1
        """Register a DockableToolbar + its GroupedToolbarLayout under a
        new stable ID. Scans the layout's _groups to pick up all already-
        tagged buttons into the registry."""
        rid = self.next_ribbon_id()
        display_name = name or f"Ribbon {rid}"
        button_ids = []
        if layout is not None:
            for widgets in layout._groups.values():
                for w in widgets:
                    if not hasattr(w, '_ribbon_id'):
                        tag_button(w)
                    self._buttons[w._ribbon_id] = w
                    button_ids.append(w._ribbon_id)
        self._ribbons[rid] = {
            'name': display_name,
            'toolbar': toolbar,
            'layout': layout,
            'buttons': button_ids,
        }
        if toolbar:
            toolbar._registry_id = rid
        return rid

    def unregister_ribbon(self, ribbon_id: int): #vers 1
        """Remove a ribbon from the registry, moving its buttons to the
        Unassigned pool. Does NOT destroy the actual DockableToolbar widget
        — caller is responsible for hiding/removing it from the layout."""
        if ribbon_id not in self._ribbons:
            return
        button_ids = self._ribbons[ribbon_id].get('buttons', [])
        for bid in button_ids:
            if bid not in self._unassigned:
                self._unassigned.append(bid)
            # Hide the actual widget since it no longer belongs to any bar
            w = self._buttons.get(bid)
            if w:
                w.setVisible(False)
        del self._ribbons[ribbon_id]

    def get_ribbon(self, ribbon_id: int) -> Optional[dict]: #vers 1
        return self._ribbons.get(ribbon_id)

    def all_buttons(self) -> list: #vers 1
        """Yield all tracked buttons as (ribbon_id_or_None, widget) pairs."""
        result = []
        for rid, info in self._ribbons.items():
            for bid in info['buttons']:
                result.append((rid, self._buttons.get(bid)))
        for bid in self._unassigned:
            result.append((None, self._buttons.get(bid)))
        return result

    def find_button(self, ribbon_id: str): #vers 1
        """Find a button widget by its _ribbon_id string."""
        return self._buttons.get(ribbon_id)

    def take_snapshot(self) -> dict: #vers 1
        """Snapshot current ribbon assignments for undo/default-preset use."""
        return {
            'ribbons': {
                rid: {
                    'name': info['name'],
                    'buttons': list(info['buttons']),
                }
                for rid, info in self._ribbons.items()
            },
            'unassigned': list(self._unassigned),
            'next_id': self._next_id,
        }

    def save_state(self): #vers 1
        try:
            state = self.take_snapshot()
            path = self._SETTINGS_FILE
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['ribbon_registry'] = state
            if self._default_snapshot:
                data['ribbon_default'] = self._default_snapshot
            path.write_text(json.dumps(data, indent=2))
        except Exception as e:
            print(f"[RibbonRegistry] save_state failed: {e}")

    def load_state(self) -> bool: #vers 1
        try:
            path = self._SETTINGS_FILE
            if not path.exists():
                return False
            data = json.loads(path.read_text())
            state = data.get('ribbon_registry')
            if not state:
                return False
            default = data.get('ribbon_default')
            if default:
                self._default_snapshot = default
            self._next_id = state.get('next_id', self._next_id)
            self._unassigned = state.get('unassigned', [])
            # Restore per-ribbon button lists (toolbar/layout refs stay as-is
            # since they're live Qt objects not serialisable)
            for rid_str, info in state.get('ribbons', {}).items():
                rid = int(rid_str)
                if rid in self._ribbons:
                    self._ribbons[rid]['buttons'] = info.get('buttons', [])
                    self._ribbons[rid]['name'] = info.get('name', f"Ribbon {rid}")
            return True
        except Exception as e:
            print(f"[RibbonRegistry] load_state failed: {e}")
            return False


# ------------------------------------------------------------------ #
# Undo commands                                                        #
# ------------------------------------------------------------------ #

class _MoveButtonCmd(QUndoCommand): #vers 1
    """Move one button from one ribbon (or Unassigned) to another."""

    def __init__(self, registry: RibbonRegistry,
                 button_rid: str,
                 from_ribbon_id: Optional[int],
                 to_ribbon_id: Optional[int],
                 description: str = "Move button"):
        super().__init__(description)
        self._reg = registry
        self._btn_rid = button_rid
        self._from = from_ribbon_id
        self._to   = to_ribbon_id

    def _do_move(self, src, dst):
        btn = self._reg._buttons.get(self._btn_rid)
        if not btn:
            return
        # Remove from source
        if src is None:
            if self._btn_rid in self._reg._unassigned:
                self._reg._unassigned.remove(self._btn_rid)
        else:
            info = self._reg._ribbons.get(src)
            if info and self._btn_rid in info['buttons']:
                info['buttons'].remove(self._btn_rid)
                # Remove from the live GroupedToolbarLayout too
                layout = info.get('layout')
                if layout:
                    for widgets in layout._groups.values():
                        if btn in widgets:
                            widgets.remove(btn)
                    layout._rebuild_grid()
        # Add to destination
        if dst is None:
            if self._btn_rid not in self._reg._unassigned:
                self._reg._unassigned.append(self._btn_rid)
            btn.setVisible(False)
        else:
            info = self._reg._ribbons.get(dst)
            if info:
                if self._btn_rid not in info['buttons']:
                    info['buttons'].append(self._btn_rid)
                layout = info.get('layout')
                if layout:
                    groups = list(layout._groups.keys())
                    if groups:
                        if btn not in layout._groups[groups[0]]:
                            layout._groups[groups[0]].append(btn)
                    layout._rebuild_grid()
                btn.setVisible(True)

    def redo(self):
        self._do_move(self._from, self._to)

    def undo(self):
        self._do_move(self._to, self._from)


class _CreateRibbonCmd(QUndoCommand): #vers 1
    """Create a new empty ribbon and float it."""

    def __init__(self, registry: RibbonRegistry, workshop,
                 description: str = "Create ribbon"):
        super().__init__(description)
        self._reg = registry
        self._workshop = workshop
        self._created_id = None
        self._toolbar = None

    def redo(self):
        from apps.components.Model_Editor.dockable_toolbar import DockableToolbar
        from apps.methods.toolbar_layout_manager import GroupedToolbarLayout
        rid = self._reg.next_ribbon_id()
        name = f"Ribbon {rid}"
        # Build a fresh empty DockableToolbar
        frame = QFrame()
        frame.setFrameStyle(QFrame.Shape.NoFrame)
        settings_key = f"ribbon_{rid}"
        layout = GroupedToolbarLayout(frame, settings_key=settings_key)
        layout.add_group('main', name)
        frame._grid = layout.grid
        rp = getattr(self._workshop, '_right_panel_ref', None)
        toolbar = DockableToolbar(rp or self._workshop, settings_key=settings_key)
        toolbar.set_content(frame)
        toolbar._registry_id = rid
        self._toolbar = toolbar
        # Register
        self._reg._ribbons[rid] = {
            'name': name,
            'toolbar': toolbar,
            'layout': layout,
            'buttons': [],
        }
        self._created_id = rid
        # Float in centre
        toolbar.setParent(self._workshop.window())
        toolbar.show()
        toolbar._on_drag_started(toolbar.mapToGlobal(
            toolbar.rect().center()))

    def undo(self):
        if self._created_id and self._created_id in self._reg._ribbons:
            self._reg.unregister_ribbon(self._created_id)
            if self._toolbar:
                self._toolbar.hide()
                self._toolbar.setParent(None)


class _DeleteRibbonCmd(QUndoCommand): #vers 1
    """Delete a ribbon, moving its buttons to Unassigned."""

    def __init__(self, registry: RibbonRegistry, ribbon_id: int,
                 description: str = "Delete ribbon"):
        super().__init__(description)
        self._reg = registry
        self._rid = ribbon_id
        self._snapshot = None

    def redo(self):
        info = self._reg._ribbons.get(self._rid)
        if not info:
            return
        # Snapshot before deleting so undo can reconstruct
        self._snapshot = {
            'name': info['name'],
            'buttons': list(info['buttons']),
        }
        tb = info.get('toolbar')
        if tb:
            tb.hide()
            tb.setParent(None)
        self._reg.unregister_ribbon(self._rid)

    def undo(self):
        if not self._snapshot:
            return
        # Restore the ribbon entry (toolbar is gone — re-create empty one)
        # This is intentionally minimal: buttons go back but the bar itself
        # needs to be re-docked by the user (since we can't know where it was)
        self._reg._ribbons[self._rid] = {
            'name': self._snapshot['name'],
            'toolbar': None,
            'layout': None,
            'buttons': list(self._snapshot['buttons']),
        }
        for bid in self._snapshot['buttons']:
            if bid in self._reg._unassigned:
                self._reg._unassigned.remove(bid)


# ------------------------------------------------------------------ #
# Dialog                                                               #
# ------------------------------------------------------------------ #

class RibbonManagerDialog(QDialog): #vers 1
    """Two-pane dialog: left = ribbon list + Unassigned pool,
    right = buttons on the selected ribbon. Drag between panes
    to reassign. Undo/redo via Ctrl+Z/Ctrl+Shift+Z."""

    def __init__(self, registry: RibbonRegistry, parent=None):
        super().__init__(parent)
        self._reg = registry
        self._selected_ribbon_id = None
        self.setWindowTitle("Ribbon Manager")
        self.setMinimumSize(640, 420)
        self._build_ui()
        self._refresh_ribbon_list()
        # Undo/redo shortcuts
        from PyQt6.QtGui import QShortcut
        QShortcut(QKeySequence.StandardKey.Undo, self,
                  activated=self._reg.undo_stack.undo)
        QShortcut(QKeySequence.StandardKey.Redo, self,
                  activated=self._reg.undo_stack.redo)

    def _build_ui(self): #vers 1
        outer = QVBoxLayout(self)

        # Toolbar row
        tb_row = QHBoxLayout()
        self._undo_btn = QPushButton("↩ Undo")
        self._redo_btn = QPushButton("↪ Redo")
        self._undo_btn.clicked.connect(self._reg.undo_stack.undo)
        self._redo_btn.clicked.connect(self._reg.undo_stack.redo)
        self._reg.undo_stack.canUndoChanged.connect(self._undo_btn.setEnabled)
        self._reg.undo_stack.canRedoChanged.connect(self._redo_btn.setEnabled)
        self._undo_btn.setEnabled(False)
        self._redo_btn.setEnabled(False)
        tb_row.addWidget(self._undo_btn)
        tb_row.addWidget(self._redo_btn)
        tb_row.addStretch()
        self._reset_btn = QPushButton("Reset to Default")
        self._reset_btn.setToolTip("Restore the original built-in ribbon arrangement")
        self._reset_btn.clicked.connect(self._reset_to_default)
        tb_row.addWidget(self._reset_btn)
        outer.addLayout(tb_row)

        # Splitter
        splitter = QSplitter(Qt.Orientation.Horizontal)
        outer.addWidget(splitter, stretch=1)

        # Left pane — ribbon list
        left = QWidget()
        left_lay = QVBoxLayout(left)
        left_lay.setSpacing(4)
        left_lay.addWidget(QLabel("Ribbons"))
        self._ribbon_list = QListWidget()
        self._ribbon_list.setDragDropMode(QAbstractItemView.DragDropMode.InternalMove)
        self._ribbon_list.currentRowChanged.connect(self._on_ribbon_selected)
        self._ribbon_list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._ribbon_list.customContextMenuRequested.connect(self._ribbon_context_menu)
        left_lay.addWidget(self._ribbon_list, stretch=1)
        btn_row = QHBoxLayout()
        add_btn = QPushButton("+ New Ribbon")
        add_btn.clicked.connect(self._create_ribbon)
        del_btn = QPushButton("Delete")
        del_btn.clicked.connect(self._delete_ribbon)
        btn_row.addWidget(add_btn)
        btn_row.addWidget(del_btn)
        left_lay.addLayout(btn_row)
        splitter.addWidget(left)

        # Right pane — button list for selected ribbon
        right = QWidget()
        right_lay = QVBoxLayout(right)
        right_lay.setSpacing(4)
        self._right_label = QLabel("Select a ribbon to see its buttons")
        right_lay.addWidget(self._right_label)
        self._button_list = QListWidget()
        self._button_list.setDragDropMode(QAbstractItemView.DragDropMode.DragDrop)
        self._button_list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._button_list.customContextMenuRequested.connect(self._button_context_menu)
        right_lay.addWidget(self._button_list, stretch=1)
        splitter.addWidget(right)
        splitter.setSizes([220, 400])

        # OK/Cancel
        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok |
            QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(self._on_accept)
        btns.rejected.connect(self._on_cancel)
        outer.addWidget(btns)

        # Snapshot for cancel
        self._cancel_snapshot = self._reg.take_snapshot()

    def _refresh_ribbon_list(self): #vers 2
        self._ribbon_list.clear()
        self._ribbon_list.setIconSize(QSize(20, 20))
        for rid, info in self._reg._ribbons.items():
            item = QListWidgetItem(info['name'])
            item.setData(Qt.ItemDataRole.UserRole, rid)
            # Show the first button's icon as a ribbon preview
            for bid in info.get('buttons', []):
                w = self._reg._buttons.get(bid)
                if w and hasattr(w, '_icon_fn'):
                    try:
                        icon = w._icon_fn(20)
                        if icon and not icon.isNull():
                            item.setIcon(icon)
                            break
                    except Exception:
                        pass
                elif w and hasattr(w, "icon") and not w.icon().isNull():
                    item.setIcon(w.icon())
                    break
            self._ribbon_list.addItem(item)
        # Unassigned pool
        if self._reg._unassigned:
            item = QListWidgetItem(f"Unassigned ({len(self._reg._unassigned)} buttons)")
            item.setData(Qt.ItemDataRole.UserRole, None)
            item.setForeground(Qt.GlobalColor.gray)
            self._ribbon_list.addItem(item)

    def _on_ribbon_selected(self, row): #vers 1
        item = self._ribbon_list.item(row)
        if not item:
            return
        rid = item.data(Qt.ItemDataRole.UserRole)
        self._selected_ribbon_id = rid
        self._refresh_button_list(rid)

    def _refresh_button_list(self, ribbon_id): #vers 2
        self._button_list.clear()
        self._button_list.setIconSize(QSize(24, 24))
        if ribbon_id is None:
            self._right_label.setText("Unassigned buttons")
            bids = self._reg._unassigned
        else:
            info = self._reg._ribbons.get(ribbon_id)
            if not info:
                return
            self._right_label.setText(f"{info['name']} — buttons")
            bids = info.get('buttons', [])
        for bid in bids:
            w = self._reg._buttons.get(bid)
            if w is None:
                print(f"[RibbonManager] no widget for bid={bid[:8]}... (not in _buttons dict)")
            tip  = (w.toolTip()    if w else '') or ''
            text = (w.text()       if w and hasattr(w, 'text') else '') or ''
            name = (w.objectName() if w else '') or ''
            label = tip or text or name or f"Button ({bid[:8]}...)"
            item = QListWidgetItem(label)
            item.setData(Qt.ItemDataRole.UserRole, bid)
            # Show the button's own icon at a readable size
            if w and hasattr(w, '_icon_fn'):
                try:
                    icon = w._icon_fn(24)
                    if icon and not icon.isNull():
                        item.setIcon(icon)
                except Exception:
                    pass
            elif w and hasattr(w, "icon") and not w.icon().isNull():
                # Fallback: use the button's existing baked icon
                item.setIcon(w.icon())
            self._button_list.addItem(item)

    def _ribbon_context_menu(self, pos): #vers 1
        item = self._ribbon_list.itemAt(pos)
        if not item:
            return
        rid = item.data(Qt.ItemDataRole.UserRole)
        if rid is None:
            return
        menu = QMenu(self)
        menu.addAction("Rename...", lambda: self._rename_ribbon(rid))
        menu.addAction("Delete", lambda: self._delete_ribbon())
        menu.exec(self._ribbon_list.mapToGlobal(pos))

    def _button_context_menu(self, pos): #vers 1
        item = self._button_list.itemAt(pos)
        if not item:
            return
        bid = item.data(Qt.ItemDataRole.UserRole)
        menu = QMenu(self)
        # Move to another ribbon submenu
        move_menu = menu.addMenu("Move to ribbon")
        for rid, info in self._reg._ribbons.items():
            if rid != self._selected_ribbon_id:
                move_menu.addAction(info['name'],
                    lambda _=False, r=rid, b=bid: self._move_button(b, self._selected_ribbon_id, r))
        move_menu.addSeparator()
        move_menu.addAction("Unassigned",
            lambda _=False, b=bid: self._move_button(b, self._selected_ribbon_id, None))
        menu.exec(self._button_list.mapToGlobal(pos))

    def _move_button(self, button_rid: str, from_rid, to_rid): #vers 1
        cmd = _MoveButtonCmd(self._reg, button_rid, from_rid, to_rid,
                             description="Move button")
        self._reg.undo_stack.push(cmd)
        self._refresh_ribbon_list()
        self._refresh_button_list(self._selected_ribbon_id)

    def _create_ribbon(self): #vers 1
        cmd = _CreateRibbonCmd(self._reg, self.parent(),
                               description="Create ribbon")
        self._reg.undo_stack.push(cmd)
        self._refresh_ribbon_list()

    def _delete_ribbon(self): #vers 1
        item = self._ribbon_list.currentItem()
        if not item:
            return
        rid = item.data(Qt.ItemDataRole.UserRole)
        if rid is None:
            return
        info = self._reg._ribbons.get(rid, {})
        n_btns = len(info.get('buttons', []))
        if n_btns > 0:
            ans = QMessageBox.question(
                self, "Delete Ribbon",
                f"'{info['name']}' has {n_btns} button(s).\n"
                f"They will move to the Unassigned pool.\nContinue?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.Cancel)
            if ans != QMessageBox.StandardButton.Yes:
                return
        cmd = _DeleteRibbonCmd(self._reg, rid, description="Delete ribbon")
        self._reg.undo_stack.push(cmd)
        self._refresh_ribbon_list()
        self._refresh_button_list(None)

    def _rename_ribbon(self, ribbon_id: int): #vers 1
        from PyQt6.QtWidgets import QInputDialog
        info = self._reg._ribbons.get(ribbon_id)
        if not info:
            return
        name, ok = QInputDialog.getText(self, "Rename Ribbon",
                                         "New name:", text=info['name'])
        if ok and name.strip():
            info['name'] = name.strip()
            self._refresh_ribbon_list()

    def _reset_to_default(self): #vers 1
        snap = self._reg._default_snapshot
        if not snap:
            QMessageBox.information(self, "Reset to Default",
                "No default snapshot available yet.\n"
                "The default is captured on first launch after\n"
                "the Ribbon Manager was introduced.")
            return
        ans = QMessageBox.question(
            self, "Reset to Default",
            "Restore the original ribbon arrangement?\n"
            "Current changes will be undoable via Ctrl+Z.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.Cancel)
        if ans != QMessageBox.StandardButton.Yes:
            return
        # Undo everything back to the snapshot — simpler than replaying
        # the snapshot directly since QUndoStack handles consistency
        while self._reg.undo_stack.canUndo():
            self._reg.undo_stack.undo()
        self._refresh_ribbon_list()
        self._refresh_button_list(self._selected_ribbon_id)

    def _on_accept(self): #vers 1
        self._reg.save_state()
        self.accept()

    def _on_cancel(self): #vers 1
        # Undo all changes made during this dialog session
        while self._reg.undo_stack.canUndo():
            self._reg.undo_stack.undo()
        self.reject()
