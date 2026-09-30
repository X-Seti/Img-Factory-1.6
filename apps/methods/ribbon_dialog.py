#this belongs in apps/methods/ribbon_dialog.py - Version: 1
# X-Seti - September30 2026 - IMG Factory 1.6 - Ribbon Manager dialog

"""
Ribbon Manager - shared toolbar layout dialog and custom icon support for workshops.
"""

##Methods list -

##class RibbonManagerDialog: -
# _build_ui
# _create_toolbar
# _delete_toolbar
# __init__
# _load_preset
# _move_action
# _on_accept
# _on_action_reordered
# _on_cancel
# _on_icon_size_changed
# _on_toolbar_selected
# _refresh_action_list
# _refresh_toolbar_list
# _reset_icon
# _save_preset
# _selected_entry_name
# _set_icon

##class RibbonIconsMixin: -
# _apply_custom_icons
# _custom_icons
# _icons_dir
# open_ribbon_manager
# _save_custom_icons

import json
import shutil
import sys
from pathlib import Path

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QIcon
from PyQt6.QtWidgets import (QComboBox, QDialog, QHBoxLayout, QLabel, QPushButton,
                             QVBoxLayout, QWidget)

from apps.methods.grip_splitter import GripSplitter

__all__ = ['RibbonManagerDialog', 'RibbonIconsMixin']


class RibbonManagerDialog(QDialog): #vers 2
    """Ribbon Manager — two-pane dialog for managing QToolBar layout.
    Left pane: list of toolbars. Right pane: actions in selected toolbar.
    Drag actions between toolbars to reassign. Create/delete toolbars.
    Save/load named presets. All changes apply live via QAction
    removeAction()/addAction() and QMainWindow addToolBar()."""

    def __init__(self, workshop, parent=None): #vers 2
        super().__init__(parent)
        self._ws = workshop
        self._mw = getattr(workshop, '_inner_mw', None)
        self._selected_tb = None
        self._cancel_state = None
        self.setWindowTitle("Ribbon Manager")
        self.setMinimumSize(660, 440)
        self._build_ui()
        if hasattr(self._ws, '_get_or_create_hidden_toolbar'):      # Map Workshop only
            self._ws._get_or_create_hidden_toolbar()
        self._refresh_toolbar_list()
        # Snapshot current state for cancel
        if self._mw:
            self._cancel_state = self._mw.saveState()


    def _build_ui(self): #vers 7
        from PyQt6.QtWidgets import (QListWidget, QDialogButtonBox,
            QAbstractItemView, QSlider)
        outer = QVBoxLayout(self)

        # Toolbar row
        tb_row = QHBoxLayout()
        self._new_btn = QPushButton("+ New Toolbar")
        self._del_btn = QPushButton("Delete")
        self._save_preset_btn = QPushButton("Save Preset…")
        self._load_preset_btn = QPushButton("Load Preset…")
        for b in (self._new_btn, self._del_btn,
                  self._save_preset_btn, self._load_preset_btn):
            tb_row.addWidget(b)
        tb_row.addStretch()
        self._new_btn.clicked.connect(self._create_toolbar)
        self._del_btn.clicked.connect(self._delete_toolbar)
        self._save_preset_btn.clicked.connect(self._save_preset)
        self._load_preset_btn.clicked.connect(self._load_preset)
        outer.addLayout(tb_row)

        # Icon size row - was previously only reachable via toolbar
        # right-click context menu, easy to miss.
        size_row = QHBoxLayout()
        size_row.addWidget(QLabel("Ribbon Icon Size:"))
        self._size_slider = QSlider(Qt.Orientation.Horizontal)
        self._size_slider.setRange(14, 40)
        self._size_slider.setSingleStep(2)
        _saved_px = 20
        try:
            import json
            _saved_px = json.loads(
                self._ws._ribbon_config_path().read_text()
            ).get('icon_scale', 20)
        except Exception:
            pass
        self._size_slider.setValue(_saved_px)
        self._size_value_label = QLabel(f"{_saved_px}px")
        self._size_value_label.setMinimumWidth(36)
        self._size_slider.valueChanged.connect(self._on_icon_size_changed)
        size_row.addWidget(self._size_slider, stretch=1)
        size_row.addWidget(self._size_value_label)
        outer.addLayout(size_row)

        # Splitter: left = toolbar list, right = action list
        splitter = GripSplitter(Qt.Orientation.Horizontal)
        outer.addWidget(splitter, stretch=1)

        # Left pane
        left = QWidget()
        ll = QVBoxLayout(left); ll.setSpacing(4)
        ll.addWidget(QLabel("Toolbars"))
        self._tb_list = QListWidget()
        self._tb_list.currentRowChanged.connect(self._on_toolbar_selected)
        ll.addWidget(self._tb_list)
        splitter.addWidget(left)

        # Right pane
        right = QWidget()
        rl = QVBoxLayout(right); rl.setSpacing(4)
        self._action_label = QLabel("Select a toolbar")
        rl.addWidget(self._action_label)
        self._act_list = QListWidget()
        self._act_list.setDragDropMode(
            QAbstractItemView.DragDropMode.InternalMove)
        self._act_list.setDefaultDropAction(Qt.DropAction.MoveAction)
        self._act_list.setIconSize(QSize(24, 24))
        self._act_list.model().rowsMoved.connect(self._on_action_reordered)
        rl.addWidget(self._act_list)

        # Move-to-toolbar button row
        move_row = QHBoxLayout()
        move_row.addWidget(QLabel("Move selected to:"))
        self._move_combo = QComboBox()
        move_row.addWidget(self._move_combo, stretch=1)
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        self._move_btn = QPushButton("Move")
        self._move_btn.setIcon(SVGIconFactory.arrow_right_icon())
        self._move_btn.setToolTip("Move selected actions to the chosen ribbon")
        self._move_btn.clicked.connect(self._move_action)
        move_row.addWidget(self._move_btn)
        rl.addLayout(move_row)

        # Icon row - community images from icons/
        icon_row = QHBoxLayout()
        self._set_icon_btn = QPushButton("Set Icon...")
        self._set_icon_btn.setToolTip("Use an image from the icons folder")
        self._set_icon_btn.clicked.connect(self._set_icon)
        self._reset_icon_btn = QPushButton("Reset Icon")
        self._reset_icon_btn.setToolTip("Restore the built-in SVG icon")
        self._reset_icon_btn.clicked.connect(self._reset_icon)
        icon_row.addWidget(self._set_icon_btn)
        icon_row.addWidget(self._reset_icon_btn)
        icon_row.addStretch()
        rl.addLayout(icon_row)
        splitter.addWidget(right)
        splitter.setSizes([200, 440])

        # OK / Cancel
        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok |
            QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(self._on_accept)
        btns.rejected.connect(self._on_cancel)
        outer.addWidget(btns)


    def _on_icon_size_changed(self, px: int): #vers 1
        """Apply + persist ribbon icon size live, update the px label."""
        self._size_value_label.setText(f"{px}px")
        if hasattr(self._ws, '_apply_icon_scale'):
            self._ws._apply_icon_scale(px)


    def _refresh_toolbar_list(self): #vers 1
        """Populate left pane with all QToolBar instances."""
        from PyQt6.QtWidgets import QToolBar, QListWidgetItem
        self._tb_list.clear()
        self._move_combo.clear()
        if not self._mw:
            return
        for tb in self._mw.findChildren(QToolBar):
            name = tb.windowTitle() or tb.objectName()
            item = QListWidgetItem(name)
            item.setData(Qt.ItemDataRole.UserRole, tb)
            # Show first action's icon as preview
            acts = [a for a in tb.actions() if not a.isSeparator() and a.icon()]
            if acts:
                item.setIcon(acts[0].icon())
            self._tb_list.addItem(item)
            self._move_combo.addItem(name, tb)


    def _on_toolbar_selected(self, row): #vers 1
        item = self._tb_list.item(row)
        if not item:
            return
        self._selected_tb = item.data(Qt.ItemDataRole.UserRole)
        self._refresh_action_list()


    def _refresh_action_list(self): #vers 2
        """Populate right pane with actions in the selected toolbar.

         fix (Aug 21 2026,  "Ribbon Manager icons in
        Overlays show as Action, with no icon or name for that
        function, and the selection group as an Action label") -
        every real button added to a toolbar via addWidget (Cull/Zon/
        Occlusion/Garage/Cycle/Undo and others) is genuinely wrapped
        by Qt in a plain QWidgetAction with no real text/icon/tooltip
        of its own at all - the actual label and icon live on the
        wrapped widget itself, not the QAction wrapping it, so act.
        text()/act.toolTip() were always empty for these, falling
        through to the "Action" placeholder every real time. Now
        checks act.defaultWidget() first and pulls the real label/
        icon/tooltip from there (QToolButton.text()/icon()/toolTip(),
        or the widget's own windowTitle() as a last-resort fallback)
        before ever falling back to the generic act.text()/toolTip()
        path a genuine standalone QAction still uses."""
        from PyQt6.QtWidgets import QListWidgetItem, QWidgetAction
        self._act_list.clear()
        tb = self._selected_tb
        if not tb:
            return
        name = tb.windowTitle() or tb.objectName()
        self._action_label.setText(f"{name} — actions")
        for act in tb.actions():
            if act.isSeparator():
                item = QListWidgetItem("- separator -")
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsDragEnabled)
            else:
                label, icon, tip = act.text(), act.icon(), act.toolTip()
                widget = act.defaultWidget() if isinstance(act, QWidgetAction) else None
                if widget is not None:
                    label = getattr(widget, 'text', lambda: '')() or label
                    w_icon = getattr(widget, 'icon', lambda: None)()
                    if w_icon is not None and not w_icon.isNull():
                        icon = w_icon
                    tip = getattr(widget, 'toolTip', lambda: '')() or tip
                    if not label:
                        label = widget.windowTitle() or widget.objectName()
                item = QListWidgetItem(label or tip or "Action")
                if icon and not icon.isNull():
                    item.setIcon(icon)
            item.setData(Qt.ItemDataRole.UserRole, act)
            self._act_list.addItem(item)


    def _on_action_reordered(self): #vers 1
        """After drag-reorder in the action list, apply new order to toolbar."""
        tb = self._selected_tb
        if not tb:
            return
        # Read new order from the list widget
        new_order = []
        for i in range(self._act_list.count()):
            act = self._act_list.item(i).data(Qt.ItemDataRole.UserRole)
            if act:
                new_order.append(act)
        # Remove and re-add all actions in new order
        for act in list(tb.actions()):
            tb.removeAction(act)
        for act in new_order:
            tb.addAction(act)


    def _move_action(self): #vers 1
        """Move selected action from current toolbar to the target toolbar."""
        act_item = self._act_list.currentItem()
        if not act_item:
            return
        act = act_item.data(Qt.ItemDataRole.UserRole)
        if not act or not self._selected_tb:
            return
        target_tb = self._move_combo.currentData()
        if not target_tb or target_tb is self._selected_tb:
            return
        self._selected_tb.removeAction(act)
        target_tb.addAction(act)
        self._refresh_action_list()


    def _create_toolbar(self): #vers 1
        """Create a new empty QToolBar and add it to the inner QMainWindow."""
        from PyQt6.QtWidgets import QInputDialog, QToolBar
        if not self._mw:
            return
        name, ok = QInputDialog.getText(self, "New Toolbar", "Toolbar name:")
        if not ok or not name.strip():
            return
        name = name.strip()
        tb = QToolBar(name, self._mw)
        tb.setObjectName(name)
        tb.setMovable(True)
        tb.setFloatable(True)
        tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        tb.customContextMenuRequested.connect(
            lambda pos, t=tb: self._ws._toolbar_context_menu(t, pos))
        self._mw.addToolBar(Qt.ToolBarArea.TopToolBarArea, tb)
        self._refresh_toolbar_list()


    def _delete_toolbar(self): #vers 1
        """Delete the selected toolbar, moving its actions to Unassigned."""
        from PyQt6.QtWidgets import QMessageBox
        tb = self._selected_tb
        if not tb:
            return
        n_acts = len([a for a in tb.actions() if not a.isSeparator()])
        if n_acts > 0:
            ans = QMessageBox.question(
                self, "Delete Toolbar",
                f"'{tb.windowTitle()}' has {n_acts} action(s).\n"
                "They will be removed from all toolbars.\nContinue?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.Cancel)
            if ans != QMessageBox.StandardButton.Yes:
                return
        self._mw.removeToolBar(tb)
        tb.deleteLater()
        self._selected_tb = None
        self._refresh_toolbar_list()
        self._act_list.clear()


    def _selected_entry_name(self): #vers 1
        """Ribbon entry name of the selected action, None if not iconable."""
        item = self._act_list.currentItem()
        act = item.data(Qt.ItemDataRole.UserRole) if item else None
        for e in getattr(self._ws, '_ribbon_actions', []):
            if e['action'] is act:
                return e['name']
        if act and not act.isSeparator():
            self._ws._set_status("This button's icon can't be changed")
        return None

    def _set_icon(self): #vers 2
        """Pick an image for the selected action; copied into icons/."""
        from PyQt6.QtWidgets import QFileDialog
        name = self._selected_entry_name()
        if not name:
            return
        folder = self._ws._icons_dir()
        src, _ = QFileDialog.getOpenFileName(self, f"Icon for {name}", str(folder),
                                             "Images (*.png *.jpg *.jpeg *.svg)")
        if not src:
            return
        src = Path(src)
        if src.parent.resolve() != folder.resolve():
            folder.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src, folder / src.name)
        icons = self._ws._custom_icons()
        icons[name] = src.name
        self._ws._save_custom_icons(icons)
        row = self._act_list.currentRow()
        self._refresh_action_list()
        self._act_list.setCurrentRow(row)

    def _reset_icon(self): #vers 2
        """Restore the built-in SVG icon on the selected action."""
        name = self._selected_entry_name()
        if not name:
            return
        icons = self._ws._custom_icons()
        icons.pop(name, None)
        self._ws._save_custom_icons(icons)
        row = self._act_list.currentRow()
        self._refresh_action_list()
        self._act_list.setCurrentRow(row)

    def _save_preset(self): #vers 4
        """Save current toolbar layout as a named preset."""
        from PyQt6.QtWidgets import QInputDialog
        import json
        if not self._mw:
            return
        name, ok = QInputDialog.getText(self, "Save Preset", "Preset name:")
        if not ok or not name.strip():
            return
        path = self._ws._ribbon_config_path()
        try:
            data = json.loads(path.read_text())
        except Exception:
            data = {}
        presets = data.setdefault('toolbar_presets', {})
        presets[name.strip()] = {'state': self._mw.saveState().toHex().data().decode(),
                                 'icons': self._ws._custom_icons()}
        path.write_text(json.dumps(data, indent=2))
        self._ws._set_status(f"Preset '{name.strip()}' saved")


    def _load_preset(self): #vers 4
        """Load a named preset."""
        from PyQt6.QtWidgets import QInputDialog
        from PyQt6.QtCore import QByteArray
        import json
        if not self._mw:
            return
        path = self._ws._ribbon_config_path()
        try:
            data = json.loads(path.read_text())
        except Exception:
            data = {}
        presets = data.get('toolbar_presets', {})
        if not presets:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "Load Preset", "No saved presets found.")
            return
        name, ok = QInputDialog.getItem(
            self, "Load Preset", "Select preset:",
            list(presets.keys()), editable=False)
        if not ok:
            return
        preset = presets[name]
        if isinstance(preset, str):         # presets saved before icons were added
            preset = {'state': preset}
        self._mw.restoreState(QByteArray.fromHex(preset['state'].encode()))
        if 'icons' in preset:
            self._ws._save_custom_icons(preset['icons'])
        self._refresh_toolbar_list()
        self._ws._set_status(f"Preset '{name}' loaded")

    def _on_accept(self): #vers 1
        """Apply and save state."""
        self._ws._save_toolbar_state()
        self.accept()

    def _on_cancel(self): #vers 1
        """Restore pre-dialog state."""
        if self._cancel_state and self._mw:
            self._mw.restoreState(self._cancel_state)
        self.reject()

class RibbonIconsMixin: #vers 1
    """Ribbon Manager hooks and custom icons/ images for a workshop.
    Workshop provides _ribbon_config_path, _ribbon_actions, _get_icon_color, _set_status."""

    def open_ribbon_manager(self): #vers 1
        """Open the Ribbon Manager dialog."""
        RibbonManagerDialog(self, parent=self).exec()

    def _icons_dir(self) -> Path: #vers 1
        """Community icons folder: beside the exe, else the repo root."""
        if getattr(sys, 'frozen', False):
            return Path(sys.executable).parent / 'icons'
        return Path(__file__).resolve().parents[2] / 'icons'

    def _custom_icons(self) -> dict: #vers 1
        """Ribbon action name to image file name in icons/."""
        try:
            data = json.loads(self._ribbon_config_path().read_text())
        except (OSError, ValueError):
            return {}
        return dict(data.get('custom_icons', {}))

    def _save_custom_icons(self, icons: dict): #vers 1
        """Store the icon choices and reapply every ribbon icon."""
        path = self._ribbon_config_path()
        try:
            data = json.loads(path.read_text())
        except (OSError, ValueError):
            data = {}
        data['custom_icons'] = icons
        path.write_text(json.dumps(data, indent=2))
        c = self._get_icon_color()
        for e in getattr(self, '_ribbon_actions', []):
            fn = e.get('icon_fn')
            if fn is None:
                continue
            try:
                e['action'].setIcon(fn(color=c))
            except TypeError:                       # icon lambdas without a color kwarg
                e['action'].setIcon(fn())
        self._apply_custom_icons()

    def _apply_custom_icons(self): #vers 1
        """Set chosen icons/ images on ribbon actions; report missing files."""
        icons = self._custom_icons()
        folder = self._icons_dir()
        missing = []
        for e in getattr(self, '_ribbon_actions', []):
            fname = icons.get(e['name'])
            if not fname:
                continue
            if (folder / fname).is_file():
                e['action'].setIcon(QIcon(str(folder / fname)))
            else:
                missing.append(fname)
        if missing:
            self._set_status(f"Missing icons: {', '.join(missing)}")
