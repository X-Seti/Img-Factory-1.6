#this belongs in apps/components/Col_Editor/depends/col_setup_ui_func.py - Version: 6
# X-Seti - Sept 29 2026 - IMG Factory 1.6 - COL Workshop UI setup

"""
COL Workshop UI - panes, button connects, toolbar, ribbons, menus, tabs, key shortcuts.
"""

##class COLSetupUIMixin: -
# _apply_button_font
# _apply_hotkey_settings
# _apply_icon_scale
# _apply_infobar_font
# _apply_panel_font
# _apply_theme
# _apply_title_font
# _build_menus_into_qmenu
# _build_toolbars
# _connect_all_buttons
# _create_left_panel
# _create_middle_panel
# _create_paint_bar
# _create_right_panel
# _create_status_bar
# _create_surface_tab
# _create_toolbar
# _enable_name_edit
# _get_icon_color
# get_menu_title
# _get_ui_color
# _on_theme_changed
# open_ribbon_manager
# _refresh_icons
# _reset_hotkeys_to_defaults
# _restore_toolbar_state
# _save_toolbar_state
# _set_col_buttons_enabled
# _set_status
# _setup_hotkeys
# setup_ui
# _show_collision_context_menu
# _show_settings_context_menu
# _show_sort_menu
# _toolbar_context_menu
# _update_status_indicators
# _wrap_middle_panel_with_surface_tab

##class _ColListDelegate: -
# paint
# sizeHint

##class _SurfaceEntry: -
# __init__

##class _SurfaceParser: -
# _detect_game
# dirty
# __init__
# load
# save
# to_text

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
# _save_preset


from PyQt6.QtCore import QSize, QTimer, Qt
from PyQt6.QtGui import QFont
from PyQt6.QtWidgets import QAbstractItemView, QComboBox, QDialog, QFrame, QHBoxLayout, QLabel, QLineEdit, QListWidget, QMainWindow, QMenu, QPushButton, QStyledItemDelegate, QTabWidget, QTableWidget, QVBoxLayout, QWidget
from apps.components.Col_Editor.depends.col_viewport import COL3DViewport
from apps.methods.grip_splitter import GripSplitter
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.img_factory_settings import get_user_config_dir

App_name = "Col Workshop"
App_build = "106"


class _ColListDelegate(QStyledItemDelegate): #vers 1
    """Word-wrapping delegate for the COL compact list Details column."""
    def paint(self, painter, option, index):  #vers 1
        if index.column() != 1:
            super().paint(painter, option, index)
            return
        from PyQt6.QtWidgets import QStyle, QApplication
        from PyQt6.QtCore import Qt
        # Background
        QApplication.style().drawPrimitive(
            QStyle.PrimitiveElement.PE_PanelItemViewItem, option, painter)
        text = index.data(Qt.ItemDataRole.DisplayRole) or ''
        painter.save()
        painter.setClipRect(option.rect)
        r = option.rect.adjusted(4, 4, -4, -4)
        painter.setPen(option.palette.text().color())
        painter.setFont(option.font)
        painter.drawText(r, Qt.TextFlag.TextWordWrap | Qt.AlignmentFlag.AlignTop, text)
        painter.restore()

    def sizeHint(self, option, index):  #vers 1
        if index.column() != 1:
            return super().sizeHint(option, index)
        from PyQt6.QtCore import Qt, QSize
        text = index.data(Qt.ItemDataRole.DisplayRole) or ''
        fm = option.fontMetrics
        # Measure the text in a column ~160px wide
        w = 160
        r = fm.boundingRect(0, 0, w, 9999,
            Qt.TextFlag.TextWordWrap | Qt.AlignmentFlag.AlignTop, text)
        return QSize(w, max(72, r.height() + 12))


# (name, type, min, max, tooltip)
_SURFACE_FIELDS = [
    ("SurfaceName",    "str",   "",  "",    "Surface identifier — matches IDE surface tag"),
    ("Adhesion",       "float", 0,   1,     "Tyre grip (0=none, 1=full)"),
    ("Friction",       "float", 0,   1,     "General friction coefficient"),
    ("SoftBounce",     "bool",  0,   1,     "Soft landing bounce"),
    ("WheelEffect",    "int",   0,   3,     "Wheel particle: 0=none 1=dirt 2=water 3=grass"),
    ("Skidmarks",      "bool",  0,   1,     "Leaves tyre skid marks"),
    ("Bounciness",     "float", 0,   1,     "Object bounce factor"),
    ("IsLiquid",       "bool",  0,   1,     "Treated as liquid surface"),
    ("Audio",          "str",   "",  "",    "Sound bank name (ROAD, DIRT, GRASS, METAL, WOOD…)"),
    ("Climb",          "bool",  0,   1,     "Player can climb this surface"),
    ("Sprint",         "bool",  0,   1,     "Player can sprint on this surface"),
    ("Walk",           "bool",  0,   1,     "Player can walk on this surface"),
    ("Skateboard",     "bool",  0,   1,     "SA: skateable surface"),
    ("Bike",           "bool",  0,   1,     "SA: bikeable surface"),
    ("Car",            "bool",  0,   1,     "SA: driveable surface"),
    ("Footstep",       "str",   "",  "",    "SA: footstep audio override"),
    ("Bullet",         "str",   "",  "",    "SA: bullet impact audio"),
    ("Sprayed",        "bool",  0,   1,     "SA: can be spray-painted"),
    ("Breakable",      "bool",  0,   1,     "SA: breakable surface"),
    ("GravelParticle", "bool",  0,   1,     "SA: gravel particle on impact"),
]


class _SurfaceEntry: #vers 1
    __slots__ = ('values', 'comment', 'raw_index', 'orig')
    def __init__(self):  #vers 1
        self.values:  list = []
        self.comment: str  = ""
        self.raw_index: int = -1
        self.orig:    list = []


class _SurfaceParser: #vers 3
    """surface.dat. Keeps the original lines (comments between rows, CRLF, spacing);
    only edited rows are rewritten, and only their changed values."""
    def __init__(self): #vers 1
        self.entries:      list = []
        self.header_lines: list = []
        self.game:         str  = 'VC'
        self._lines:       list = []
        self._eol:         str  = "\n"


    def _detect_game(self, entries: list) -> str: #vers 1
        for e in entries:
            if len(e.values) > 13:
                return 'SA'
        return 'VC'


    @property
    def dirty(self) -> bool:
        return any(getattr(e, 'raw_index', -1) < 0 or list(e.values) != list(getattr(e, 'orig', ()))
                   for e in self.entries) or len(self.entries) != getattr(self, '_n0', len(self.entries))


    def load(self, path: str) -> bool: #vers 2
        try:
            text = open(path, 'rb').read().decode('latin-1')
            self._eol = "\r\n" if "\r\n" in text else "\n"
            self._lines = text.split(self._eol)
            self.entries.clear(); self.header_lines.clear()
            for i, ln in enumerate(self._lines):
                s = ln.strip()
                if not s or s.startswith(';') or s.startswith('#'):
                    if not self.entries: self.header_lines.append(ln)
                    continue
                comment = ""
                if ';' in s:
                    k = s.index(';'); comment = s[k:]; s = s[:k].strip()
                parts = s.split()
                if len(parts) < 2:
                    continue
                e = _SurfaceEntry(); e.values = parts; e.comment = comment
                e.raw_index, e.orig = i, list(parts)
                self.entries.append(e)
            self._n0 = len(self.entries)
            self.game = self._detect_game(self.entries)
            return True
        except Exception as ex:
            print(f"_SurfaceParser.load: {ex}"); return False


    def to_text(self) -> str:
        import re as _re
        alive = {e.raw_index: e for e in self.entries if getattr(e, 'raw_index', -1) >= 0}
        new = [e for e in self.entries if getattr(e, 'raw_index', -1) < 0]
        out, last = [], -1
        for i, ln in enumerate(self._lines):
            e = alive.get(i)
            s = ln.strip()
            is_row = bool(s) and not s.startswith((';', '#')) and len(s.split(';')[0].split()) >= 2
            if e is None and is_row:
                continue                                     # a deleted surface
            if e is not None and list(e.values) != list(e.orig):
                body, sc, cm = ln.partition(';')
                toks = _re.split(r'(\s+)', body)
                slots = [k for k, t in enumerate(toks) if t and not t.isspace()]
                for k, (o, nv) in enumerate(zip(e.orig, e.values)):
                    if o != nv and k < len(slots):
                        toks[slots[k]] = str(nv)
                if len(e.values) > len(slots):
                    toks.append('\t' + '\t'.join(str(v) for v in e.values[len(slots):]))
                ln = ''.join(toks) + sc + cm
            out.append(ln)
            if e is not None:
                last = len(out) - 1
        if new:
            at = last + 1 if last >= 0 else len(out)
            out[at:at] = ['\t'.join(str(v) for v in e.values) + (f'  {e.comment}' if e.comment else '') for e in new]
        return self._eol.join(out)


    def save(self, path: str) -> bool: #vers 2
        try:
            from apps.methods.file_backup import safe_write_bytes      # backup + atomic write
            text = self.to_text()
            safe_write_bytes(path, text.encode('latin-1', errors='replace'))
            keep = sorted((e for e in self.entries if getattr(e, 'raw_index', -1) >= 0), key=lambda e: e.raw_index) \
                + [e for e in self.entries if getattr(e, 'raw_index', -1) < 0]
            self.load(path)                                   # re-baseline
            return True
        except Exception as ex:
            print(f"_SurfaceParser.save: {ex}"); return False


class RibbonManagerDialog(QDialog): #vers 1
    """Ribbon Manager — two-pane dialog for managing QToolBar layout.
    Left pane: list of toolbars. Right pane: actions in selected toolbar.
    Drag actions between toolbars to reassign. Create/delete toolbars.
    Save/load named presets. All changes apply live via QAction
    removeAction()/addAction() and QMainWindow addToolBar()."""

    ##Methods list -
    # RibbonManagerDialog.__init__
    # RibbonManagerDialog._build_ui
    # RibbonManagerDialog._on_icon_size_changed
    # RibbonManagerDialog._refresh_toolbar_list
    # RibbonManagerDialog._refresh_action_list
    # RibbonManagerDialog._on_toolbar_selected
    # RibbonManagerDialog._move_action
    # RibbonManagerDialog._create_toolbar
    # RibbonManagerDialog._delete_toolbar
    # RibbonManagerDialog._save_preset
    # RibbonManagerDialog._load_preset
    # RibbonManagerDialog._on_accept
    # RibbonManagerDialog._on_cancel

    def __init__(self, workshop, parent=None): #vers 1
        super().__init__(parent)
        self._ws = workshop
        self._mw = getattr(workshop, '_inner_mw', None)
        self._selected_tb = None
        self._cancel_state = None
        self.setWindowTitle("Ribbon Manager")
        self.setMinimumSize(660, 440)
        self._build_ui()
        self._refresh_toolbar_list()
        # Snapshot current state for cancel
        if self._mw:
            self._cancel_state = self._mw.saveState()


    def _build_ui(self): #vers 5
        from PyQt6.QtWidgets import (QSplitter, QListWidget, QListWidgetItem,
            QDialogButtonBox, QAbstractItemView, QSlider)
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
            from pathlib import Path
            _saved_px = json.loads(
                (get_user_config_dir()/'col_workshop.json').read_text()
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


    def _refresh_action_list(self): #vers 1
        """Populate right pane with actions in the selected toolbar."""
        from PyQt6.QtWidgets import QListWidgetItem
        self._act_list.clear()
        tb = self._selected_tb
        if not tb:
            return
        name = tb.windowTitle() or tb.objectName()
        self._action_label.setText(f"{name} — actions")
        for act in tb.actions():
            if act.isSeparator():
                item = QListWidgetItem("   separator   ")
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsDragEnabled)
            else:
                item = QListWidgetItem(act.text() or act.toolTip() or "Action")
                if act.icon():
                    item.setIcon(act.icon())
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


    def _save_preset(self): #vers 2
        """Save current toolbar layout as a named preset."""
        from PyQt6.QtWidgets import QInputDialog
        import json
        from pathlib import Path
        if not self._mw:
            return
        name, ok = QInputDialog.getText(self, "Save Preset", "Preset name:")
        if not ok or not name.strip():
            return
        path = get_user_config_dir() / 'col_workshop.json'
        try:
            data = json.loads(path.read_text())
        except Exception:
            data = {}
        presets = data.setdefault('toolbar_presets', {})
        presets[name.strip()] = self._mw.saveState().toHex().data().decode()
        path.write_text(json.dumps(data, indent=2))
        self._ws._set_status(f"Preset '{name.strip()}' saved")


    def _load_preset(self): #vers 2
        """Load a named preset."""
        from PyQt6.QtWidgets import QInputDialog
        from PyQt6.QtCore import QByteArray
        import json
        from pathlib import Path
        if not self._mw:
            return
        path = get_user_config_dir() / 'col_workshop.json'
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
        self._mw.restoreState(QByteArray.fromHex(presets[name].encode()))
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


class COLSetupUIMixin: #vers 1
    """UI builders, ribbons, menus, hotkeys and theme for COLWorkshop."""

    def _get_ui_color(self, key): #vers 2
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)

    def get_menu_title(self) -> str: #vers 1
        """Short label for imgfactory titlebar button."""
        return "COL"

    def _build_menus_into_qmenu(self, parent_menu): #vers 1
        """Populate parent_menu with COL Workshop actions."""
        # File
        fm = parent_menu.addMenu("File")
        fm.addAction("Open COL…",        self._open_file)
        fm.addAction("Save COL",         self._save_file)
        fm.addAction("Save COL As…",     self._save_file_as)
        fm.addSeparator()
        fm.addAction("Import COL…",      self._import_col_data)
        fm.addAction("Export COL…",      self._export_col_data)

        # Edit
        em = parent_menu.addMenu("Edit")
        em.addAction("Undo",             lambda: getattr(self, 'undo_action', lambda: None) and self.undo_action())

        # View
        vm = parent_menu.addMenu("View")
        vm.addAction("Sort Models",      self._show_sort_menu if hasattr(self, '_show_sort_menu') else lambda: None)

    def setup_ui(self): #vers 12
        """Setup the main UI layout"""
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(5, 5, 5, 5)
        main_layout.setSpacing(5)

        # Toolbar - hidden when embedded in main window tab
        toolbar = self._create_toolbar()
        self._workshop_toolbar = toolbar
        if not self.standalone_mode:
            toolbar.setVisible(False)
        main_layout.addWidget(toolbar)

        # Tab bar for multiple col files
        self.col_tabs = QTabWidget()
        self.col_tabs.setTabsClosable(True)
        self.col_tabs.tabCloseRequested.connect(self._close_col_tab)


        # Create initial tab with main content
        initial_tab = QWidget()
        tab_layout = QVBoxLayout(initial_tab)
        tab_layout.setContentsMargins(0, 0, 0, 0)


        # Main splitter
        self._main_splitter = GripSplitter(Qt.Orientation.Horizontal)

        # Create all panels first
        left_panel = self._create_left_panel()
        middle_panel = self._create_middle_panel()
        right_panel = self._create_right_panel()

        # Add panels to splitter based on mode
        if left_panel is not None:  # IMG Factory mode
            self._main_splitter.addWidget(left_panel)
            self._main_splitter.addWidget(middle_panel)
            self._main_splitter.addWidget(right_panel)
            # Set proportions (2:3:5)
            self._main_splitter.setStretchFactor(0, 2)
            self._main_splitter.setStretchFactor(1, 3)
            self._main_splitter.setStretchFactor(2, 5)
        else:  # Standalone mode
            self._main_splitter.addWidget(middle_panel)
            self._main_splitter.addWidget(right_panel)
            # Set proportions (1:1)
            self._main_splitter.setStretchFactor(0, 1)
            self._main_splitter.setStretchFactor(1, 1)

        # Splitter goes straight into main_layout - no top-level tab wrapper.
        # COL Models/Surface Data switching is now local to the middle
        # panel only (see _wrap_middle_panel_with_surface_tab), so the left
        # panel (file list) and right panel (ribbons + viewport) are never
        # nested inside a QTabWidget's padded content pane.
        main_layout.addWidget(self._main_splitter)
        self._main_splitter.splitterMoved.connect(self._on_splitter_moved)
        QTimer.singleShot(0, self._restore_splitter_sizes)

        # Status indicators - hidden when embedded in main window tab
        if hasattr(self, '_setup_status_indicators'):
            status_frame = self._setup_status_indicators()
            if not self.standalone_mode:
                status_frame.setVisible(False)
            main_layout.addWidget(status_frame)

        # Apply theme colours to all icons now that UI is fully built
        self._refresh_icons()
        self._connect_all_buttons()

        # Apply theme once at end so all panels inherit correct palette
        self._apply_theme()

    def _connect_all_buttons(self): #vers 2
        """Wire flip/rotate transform buttons to preview_widget.
        Called once from setup_ui after all panels are built."""
        pw = getattr(self, 'preview_widget', None)
        if not (pw and isinstance(pw, COL3DViewport)):
            return

        def _safe(btn_name, fn):  #vers 2
            btn = getattr(self, btn_name, None)
            if not btn or not hasattr(btn, 'clicked'):
                return   # QAction (new ribbon) has no .clicked - already
                         # wired directly in _build_toolbars, nothing to do
            try: btn.clicked.disconnect()
            except Exception: pass
            btn.clicked.connect(fn)

        _safe('flip_vert_btn',  pw.flip_vertical)
        _safe('flip_horz_btn',  pw.flip_horizontal)
        _safe('rotate_cw_btn',  pw.rotate_cw)
        _safe('rotate_ccw_btn', pw.rotate_ccw)

    def _update_status_indicators(self): #vers 2
        """Update status indicators"""
        if hasattr(self, 'status_collision'):
            self.status_textures.setText(f"collision: {len(self.collision_list)}")

        if hasattr(self, 'status_selected'):
            if self.selected_texture:
                name = self.selected_collision.get('name', 'Unknown')
                self.status_selected.setText(f"Selected: {name}")
            else:
                self.status_selected.setText("Selected: None")

        if hasattr(self, 'status_size'):
            if self.current_txd_data:
                size_kb = len(self.current_col_data) / 1024
                self.status_size.setText(f"COL Size: {size_kb:.1f} KB")
            else:
                self.status_size.setText("COL Size: Unknown")

        if hasattr(self, 'status_modified'):
            if self.windowTitle().endswith("*"):
                self.status_modified.setText("MODIFIED")
                self.status_modified.setStyleSheet("color: orange; font-weight: bold;")
            else:
                self.status_modified.setText("")
                self.status_modified.setStyleSheet("")

    def _create_status_bar(self): #vers 2
        """Create bottom status bar - single line compact"""
        from PyQt6.QtWidgets import QFrame, QHBoxLayout, QLabel

        status_bar = QFrame()
        status_bar.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        status_bar.setFixedHeight(22)

        layout = QHBoxLayout(status_bar)
        layout.setContentsMargins(5, 0, 5, 0)
        layout.setSpacing(15)

        # Left: Ready
        self.status_label = QLabel("Ready")
        layout.addWidget(self.status_label)


        return status_bar

    def _refresh_icons(self): #vers 2
        """Refresh all button icons after theme change — picks up current text_primary colour."""
        SVGIconFactory.clear_cache()
        c = self._get_icon_color()
        SVGIconFactory.set_theme_color(c)

        # Complete icon map — every themed button in Col Workshop
        _icon_map = [
            # Title bar / main toolbar
            ('settings_btn',         'settings_icon'),
            ('open_btn',             'open_icon'),
            ('save_btn',             'save_icon'),
            ('saveall_btn',          'saveas_icon'),
            ('export_all_btn',       'package_icon'),
            ('undo_btn',             'undo_icon'),
            ('info_btn',             'info_icon'),
            ('minimize_btn',         'minimize_icon'),
            ('maximize_btn',         'maximize_icon'),
            ('close_btn',            'close_icon'),
            ('open_img_btn',         'folder_icon'),
            ('from_img_btn',         'open_icon'),
            # Middle panel mini-toolbar (docked mode)
            ('open_col_btn',         'open_icon'),
            ('save_col_btn',         'save_icon'),
            ('export_col_btn',       'package_icon'),
            ('undo_col_btn',         'undo_icon'),
            # Left transform toolbar (all 14 icons)
            ('flip_vert_btn',        'flip_vert_icon'),
            ('flip_horz_btn',        'flip_horz_icon'),
            ('rotate_cw_btn',        'rotate_cw_icon'),
            ('rotate_ccw_btn',       'rotate_ccw_icon'),
            ('analyze_btn',          'analyze_icon'),
            ('copy_btn',             'copy_icon'),
            ('paste_btn',            'paste_icon'),
            ('create_surface_btn',   'add_icon'),
            ('delete_surface_btn',   'delete_icon'),
            ('duplicate_surface_btn','duplicate_icon'),
            ('paint_btn',            'paint_icon'),
            ('surface_type_btn',     'checkerboard_icon'),
            ('surface_edit_btn',     'surfaceedit_icon'),
            ('build_from_txd_btn',   'build_icon'),
            # Right preview toolbar (zoom/pan/view)
            ('view_spheres_btn',     'sphere_icon'),
            ('view_boxes_btn',       'box_icon'),
            ('view_mesh_btn',        'mesh_icon'),
            ('backface_btn',         'backface_icon'),
            # Info / bottom panel buttons
            ('import_btn',           'import_icon'),
            ('export_btn',           'export_icon'),
            ('switch_btn',           'flip_vert_icon'),
            ('convert_btn',          'convert_icon'),
            ('paint_undo_btn',       'undo_paint_icon'),
        ]
        for attr, method in _icon_map:
            btn = getattr(self, attr, None)
            if btn is None:
                continue
            fn = getattr(self.icon_factory, method, None)
            if fn is None:
                continue
            try:
                btn.setIcon(fn(color=c))
            except TypeError:
                try:
                    btn.setIcon(fn())
                except Exception:
                    pass


# - Settings Reusable

        # Refresh right preview bar (zoom/pan/view controls)
        try:
            tip_to_icon = {
                'Zoom In': 'zoom_in_icon', 'Zoom Out': 'zoom_out_icon',
                'Reset View': 'reset_icon', 'Fit to Window': 'fit_icon',
                'Pan Up': 'arrow_up_icon', 'Pan Down': 'arrow_down_icon',
                'Pan Left': 'arrow_left_icon', 'Pan Right': 'arrow_right_icon',
                'Render / Background Settings': 'color_picker_icon',
                'Toggle Spheres': 'sphere_icon', 'Toggle Boxes': 'box_icon',
                'Toggle Mesh': 'mesh_icon', 'Toggle Backface': 'backface_icon',
            }
            for btn in getattr(self, '_col_ctrl_buttons', []):
                fn_name = tip_to_icon.get(btn.toolTip())
                if fn_name:
                    fn = getattr(self.icon_factory, fn_name, None)
                    if fn:
                        try: btn.setIcon(fn(color=c))
                        except Exception: pass
        except Exception:
            pass

        # Refresh left icon toolbar
        try:
            tip_to_icon_left = {
                'Flip col vertically': 'flip_vert_icon',
                'Flip col horizontally': 'flip_horz_icon',
                'Rotate 90 degrees clockwise': 'rotate_cw_icon',
                'Rotate 90 degrees counter-clockwise': 'rotate_ccw_icon',
                'Analyze collision data': 'analyze_icon',
                'Copy col to clipboard': 'copy_icon',
                'Paste col from clipboard': 'paste_icon',
                'Create new blank Collision': 'add_icon',
                'Remove selected Collision': 'delete_icon',
                'Clone selected Collision': 'duplicate_icon',
                'Paint free hand on surface — assign materials': 'paint_icon',
                'Surface types': 'checkerboard_icon',
                'Surface Editor — edit mesh faces and vertices': 'surfaceedit_icon',
                'Create col surface from txd texture names': 'build_icon',
            }
            for btn in getattr(self, '_col_icon_buttons', []):
                fn_name = tip_to_icon_left.get(btn.toolTip())
                if fn_name:
                    fn = getattr(self.icon_factory, fn_name, None)
                    if fn:
                        try: btn.setIcon(fn(color=c))
                        except Exception: pass
        except Exception:
            pass

        # Sync mini toolbar visibility with current dock state
        if hasattr(self, '_middle_btn_row'):
            self._middle_btn_row.setVisible(
                self.is_docked and not self.standalone_mode)

    def _create_toolbar(self): #vers 13
        """Create toolbar - FIXED: Hide drag button when docked, ensure buttons visible"""
        # Read sizes from app_settings so they match Global App System Settings
        try:
            from apps.utils.app_settings_system import get_titlebar_sizes as _gts
            _as = getattr(self, 'app_settings', None) or getattr(
                  getattr(self, 'main_window', None), 'app_settings', None)
            _sz = _gts(_as)
            _TB_H    = _sz['tb_height']
            _BTN_SZ  = _sz['btn_size']
            _ICO_SZ  = _sz['icon_size']
            _BTN_H   = _sz['btn_height']
        except Exception:
            _TB_H, _BTN_SZ, _ICO_SZ, _BTN_H = 32, 32, 20, 24
        self.titlebar = QFrame()
        self.titlebar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.titlebar.setFixedHeight(45)
        self.titlebar.setObjectName("titlebar")

        # Install event filter for drag detection
        self.titlebar.installEventFilter(self)
        self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.titlebar.setMouseTracking(True)

        self.titlebar_layout = QHBoxLayout(self.titlebar)
        self.titlebar_layout.setContentsMargins(5, 5, 5, 5)
        self.titlebar_layout.setSpacing(5)

        # Get icon color from theme
        icon_color = self._get_icon_color()

        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setMaximumHeight(_TB_H + 10)

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(5)

        # Settings button
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(self.icon_factory.settings_icon(color=icon_color))
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(_ICO_SZ, _ICO_SZ))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip("Workshop Settings")
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        # App title in center
        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(self.title_label)

        layout.addStretch()

        # Only show "Open IMG" button if NOT standalone
        if not self.standalone_mode:
            self.open_img_btn = QPushButton("OpenIMG")
            self.open_img_btn.setFont(self.button_font)
            self.open_img_btn.setIcon(self.icon_factory.folder_icon(color=icon_color))
            self.open_img_btn.setIconSize(QSize(_ICO_SZ, _ICO_SZ))
            self.open_img_btn.clicked.connect(self.open_img_archive)
            self.open_img_btn.setToolTip("Open an IMG archive and browse its COL entries")
            layout.addWidget(self.open_img_btn)

            # "From IMG" — pick a COL entry from the currently loaded IMG in IMG Factory
            self.from_img_btn = QPushButton("From IMG")
            self.from_img_btn.setFont(self.button_font)
            self.from_img_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
            self.from_img_btn.setIconSize(QSize(_ICO_SZ, _ICO_SZ))
            self.from_img_btn.clicked.connect(self._pick_col_from_current_img)
            self.from_img_btn.setToolTip("Pick a COL entry from the currently loaded IMG")
            layout.addWidget(self.from_img_btn)

        # Open button
        self.open_btn = QPushButton()
        self.open_btn.setFont(self.button_font)
        self.open_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.open_btn.setText("Open")
        self.open_btn.setIconSize(QSize(20, 20))
        self.open_btn.setShortcut("Ctrl+O")
        if self.button_display_mode == 'icons':
            self.open_btn.setFixedSize(40, 40)
        self.open_btn.setToolTip("Open COL file (Ctrl+O)")
        self.open_btn.clicked.connect(self._open_file)
        layout.addWidget(self.open_btn)

        # Save button
        self.save_btn = QPushButton()
        self.save_btn.setFont(self.button_font)
        self.save_btn.setIcon(self.icon_factory.save_icon(color=icon_color))
        self.save_btn.setText("Save")
        self.save_btn.setIconSize(QSize(20, 20))
        self.save_btn.setShortcut("Ctrl+S")
        if self.button_display_mode == 'icons':
            self.save_btn.setFixedSize(40, 40)
        self.save_btn.setEnabled(True)
        self.save_btn.setToolTip("Save COL file (Ctrl+S)")
        self.save_btn.clicked.connect(self._save_file)
        layout.addWidget(self.save_btn)

        # Save button
        self.saveall_btn = QPushButton()
        self.saveall_btn.setFont(self.button_font)
        self.saveall_btn.setIcon(self.icon_factory.saveas_icon(color=icon_color))
        self.saveall_btn.setText("Save All")
        self.saveall_btn.setIconSize(QSize(20, 20))
        self.saveall_btn.setShortcut("Ctrl+S")
        if self.button_display_mode == 'icons':
            self.saveall_btn.setFixedSize(40, 40)
        self.saveall_btn.setEnabled(True)
        self.saveall_btn.setToolTip("Save COL file (Ctrl+S)")
        self.saveall_btn.clicked.connect(self._saveall_file)
        #layout.addWidget(self.saveall_btn)

        self.export_all_btn = QPushButton("Extract")
        self.export_all_btn.setFont(self.button_font)
        self.export_all_btn.setIcon(self.icon_factory.package_icon(color=icon_color))
        self.export_all_btn.setIconSize(QSize(20, 20))
        self.export_all_btn.setToolTip("Export all as col, cst or 3ds files")
        self.export_all_btn.clicked.connect(self.export_all)
        self.export_all_btn.setEnabled(True)
        layout.addWidget(self.export_all_btn)

        self.undo_btn = QPushButton()
        self.undo_btn.setFont(self.button_font)
        self.undo_btn.setIcon(self.icon_factory.undo_icon(color=icon_color))
        self.undo_btn.setText("Undo")
        self.undo_btn.setIconSize(QSize(20, 20))
        self.undo_btn.clicked.connect(self._undo_last_action)
        self.undo_btn.setEnabled(True)
        self.undo_btn.setToolTip("Undo last change")
        layout.addWidget(self.undo_btn)

        # Register compact-aware buttons (adaptive icon/text display)
        self._col_compact_btns = [
            (getattr(self, 'settings_btn',    None), "Settings"),
            (getattr(self, 'open_btn',        None), "Open"),
            (getattr(self, 'save_btn',        None), "Save"),
            (getattr(self, 'export_all_btn',  None), "Extract"),
            (getattr(self, 'undo_btn',        None), "Undo"),
            (getattr(self, 'open_img_btn',    None), "OpenIMG"),
            (getattr(self, 'from_img_btn',    None), "From IMG"),
        ]
        # Restore saved mode
        try:
            if self.main_window and hasattr(self.main_window, 'img_settings'):
                saved = self.main_window.img_settings.get('col_icon_display_mode', 'icons_and_text')
                self.icon_display_mode = saved
                self._apply_col_btn_display()
        except Exception:
            pass

        # Info button
        self.info_btn = QPushButton("")
        self.info_btn.setText("")  # CHANGED from "Info"
        self.info_btn.setIcon(self.icon_factory.info_icon(color=icon_color))
        self.info_btn.setMinimumWidth(40)
        self.info_btn.setMaximumWidth(40)
        self.info_btn.setMinimumHeight(30)
        self.info_btn.setToolTip("Information")

        self.info_btn.setIconSize(QSize(20, 20))
        self.info_btn.setFixedWidth(35)
        self.info_btn.clicked.connect(self._show_col_info)
        layout.addWidget(self.info_btn)

        # Properties/Theme button
        self.properties_btn = QPushButton()
        self.properties_btn.setFont(self.button_font)
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(24, icon_color))
        self.properties_btn.setToolTip("Theme")
        self.properties_btn.setFixedSize(35, 35)
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        self.properties_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.properties_btn.customContextMenuRequested.connect(self._show_settings_context_menu)
        layout.addWidget(self.properties_btn)

        # Dock button [D]
        self.dock_btn = QPushButton("D")
        #self.dock_btn.setFont(self.button_font)
        self.dock_btn.setMinimumWidth(40)
        self.dock_btn.setMaximumWidth(40)
        self.dock_btn.setMinimumHeight(30)
        self.dock_btn.setToolTip("Dock")

        self.dock_btn.clicked.connect(self.toggle_dock_mode)
        layout.addWidget(self.dock_btn)

        # Tear-off button [T] - only in IMG Factory mode
        if not self.standalone_mode:
            self.tearoff_btn = QPushButton("T")
            #self.tearoff_btn.setFont(self.button_font)
            self.tearoff_btn.setMinimumWidth(40)
            self.tearoff_btn.setMaximumWidth(40)
            self.tearoff_btn.setMinimumHeight(30)
            self.tearoff_btn.clicked.connect(self._toggle_tearoff)
            self.tearoff_btn.setToolTip("TXD Workshop - Tearoff window")

            layout.addWidget(self.tearoff_btn)

        # Window controls
        self.minimize_btn = QPushButton()
        self.minimize_btn.setIcon(self.icon_factory.minimize_icon(color=icon_color))
        self.minimize_btn.setIconSize(QSize(20, 20))
        self.minimize_btn.setMinimumWidth(40)
        self.minimize_btn.setMaximumWidth(40)
        self.minimize_btn.setMinimumHeight(30)
        self.minimize_btn.clicked.connect(self.showMinimized)
        self.minimize_btn.setToolTip("Minimize Window") # click tab to restore
        layout.addWidget(self.minimize_btn)

        self.maximize_btn = QPushButton()
        self.maximize_btn.setIcon(self.icon_factory.maximize_icon(color=icon_color))
        self.maximize_btn.setIconSize(QSize(20, 20))
        self.maximize_btn.setMinimumWidth(40)
        self.maximize_btn.setMaximumWidth(40)
        self.maximize_btn.setMinimumHeight(30)
        self.maximize_btn.clicked.connect(self._toggle_maximize)
        self.maximize_btn.setToolTip("Maximize/Restore Window")
        layout.addWidget(self.maximize_btn)

        self.close_btn = QPushButton()
        self.close_btn.setIcon(self.icon_factory.close_icon(color=icon_color))
        self.close_btn.setIconSize(QSize(20, 20))
        self.close_btn.setMinimumWidth(40)
        self.close_btn.setMaximumWidth(40)
        self.close_btn.setMinimumHeight(30)
        self.close_btn.clicked.connect(self.close)
        self.close_btn.setToolTip("Close Window") # closes tab
        layout.addWidget(self.close_btn)

        return self.toolbar

    def _create_surface_tab(self): #vers 9
        """Build the Surface Data tab — parser + editor for surface.dat."""
        from PyQt6.QtWidgets import (QSplitter, QListWidget, QListWidgetItem,
                                      QScrollArea, QFormLayout, QDoubleSpinBox,
                                      QSpinBox, QCheckBox, QLineEdit, QProgressBar)

        self._surf_parser  = _SurfaceParser()
        self._surf_cur_idx = -1
        self._surf_blocking = False
        self._surf_widgets: dict = {}

        tab = QWidget()
        root = QVBoxLayout(tab)
        root.setContentsMargins(4, 4, 4, 4)


        # Toolbar row
        tb = QHBoxLayout()
        ic = self._get_icon_color()
        open_btn = QPushButton("Open surface.dat")
        open_btn.setIcon(self.icon_factory.open_icon(color=ic))
        open_btn.setToolTip("Open surface.dat")
        open_btn.setFixedHeight(26)
        open_btn.clicked.connect(self._surf_open)
        save_btn = QPushButton("Save")
        save_btn.setIcon(self.icon_factory.save_icon(color=ic))
        save_btn.setToolTip("Save surface.dat")
        save_btn.setFixedHeight(26)
        save_btn.clicked.connect(self._surf_save)
        self._surf_top_btns = [(open_btn, "Open surface.dat"), (save_btn, "Save")]
        self._surf_status = QLabel("No file loaded")
        tb.addWidget(open_btn); tb.addWidget(save_btn)
        tb.addStretch()
        tb.addWidget(self._surf_status)
        root.addLayout(tb)

        sp = GripSplitter(Qt.Orientation.Horizontal)

        # Left — surface list
        # Updated section font help so the Add, Del, Dup can be seen.
        left = QWidget(); ll = QVBoxLayout(left); ll.setContentsMargins(2,2,2,2)
        ll.addWidget(QLabel("Surfaces"))
        self._surf_search = QLineEdit(); self._surf_search.setPlaceholderText("Search…")
        self._surf_search.textChanged.connect(lambda t: self._surf_refresh_list(t))
        ll.addWidget(self._surf_search)
        self._surf_list = QListWidget()
        self._surf_list.currentRowChanged.connect(self._surf_on_select)
        ll.addWidget(self._surf_list)
        br = QHBoxLayout()
        self._surf_list_btns = []
        for lbl, tip, icon, fn in [("Add", "Add surface", self.icon_factory.add_icon, self._surf_add),
                                   ("Del", "Delete surface", self.icon_factory.delete_icon, self._surf_delete),
                                   ("Dup", "Duplicate surface", self.icon_factory.duplicate_icon, self._surf_dup)]:
            b = QPushButton(lbl); b.setIcon(icon(color=ic)); b.setToolTip(tip)
            b.setFixedHeight(26); b.clicked.connect(fn); br.addWidget(b)
            self._surf_list_btns.append((b, lbl))
        self._surf_list_panel = left
        ll.addLayout(br)
        sp.addWidget(left)

        # Centre — field form
        scroll = QScrollArea(); scroll.setWidgetResizable(True)
        ctr = QWidget(); scroll.setWidget(ctr)
        form = QFormLayout(ctr); form.setSpacing(4); form.setContentsMargins(2,2,2,2)

        for fname, ftype, fmin, fmax, tip in _SURFACE_FIELDS:
            lbl = QLabel(fname); lbl.setToolTip(tip); lbl.setFixedWidth(160)
            if ftype == 'float':
                w = QDoubleSpinBox(); w.setRange(float(fmin), float(fmax))
                w.setDecimals(4); w.setSingleStep(0.01)
                w.valueChanged.connect(lambda v, n=fname: self._surf_changed(n, v))
            elif ftype == 'int':
                w = QSpinBox(); w.setRange(int(fmin), int(fmax))
                w.valueChanged.connect(lambda v, n=fname: self._surf_changed(n, v))
            elif ftype == 'bool':
                w = QCheckBox()
                w.stateChanged.connect(lambda v, n=fname: self._surf_changed(n, int(v > 0)))
            else:
                w = QLineEdit()
                w.textChanged.connect(lambda v, n=fname: self._surf_changed(n, v))
            w.setToolTip(tip)
            self._surf_widgets[fname] = w
            form.addRow(lbl, w)
        sp.addWidget(scroll)

        # Right — quick reference
        right = QWidget(); rl = QVBoxLayout(right); rl.setContentsMargins(2,2,2,2)
        rl.addWidget(QLabel("WheelEffect values"))
        for val, desc in [(0,"None"),(1,"Dirt/sand"),(2,"Water"),(3,"Grass")]:
            rl.addWidget(QLabel(f"  {val} = {desc}"))
        rl.addSpacing(12)
        rl.addWidget(QLabel("Common Audio IDs"))
        for a in ["ROAD","DIRT","GRASS","GRAVEL","MUD","SAND","WATER",
                  "METAL","WOOD","TILE","CARPET","FLESH"]:
            rl.addWidget(QLabel(f"  {a}"))
        rl.addStretch()
        sp.addWidget(right)

        sp.setSizes([100, 580, 200])
        root.addWidget(sp, 1)  # splitter takes spare height, no gap
        sp.splitterMoved.connect(lambda *_: self._apply_left_compact())
        return tab

    def _create_left_panel(self): #vers 5
        """Create left panel - COL file list (only in IMG Factory mode)"""
        # In standalone mode, don't create this panel
        if self.standalone_mode:
            self.col_list_widget = None  # Explicitly set to None
            return None

        if not self.main_window:
            # Standalone mode - return None to hide this panel
            return None

        # Only create panel in IMG Factory mode
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(100)
        panel.setMaximumWidth(150)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)

        # Header row with search button
        hdr_row = QHBoxLayout()
        self._col_list_header = QLabel("COL Files")
        self._col_list_header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        hdr_row.addWidget(self._col_list_header)
        hdr_row.addStretch()
        self.col_search_btn = QPushButton()
        self.col_search_btn.setFixedSize(24, 24)
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            self.col_search_btn.setIcon(SVGIconFactory.search_icon(16))
            self.col_search_btn.setIconSize(QSize(16, 16))
        except Exception:
            pass  # No icon — button still works
        self.col_search_btn.setToolTip("Search COL files")
        self.col_search_btn.clicked.connect(self._show_col_search)
        hdr_row.addWidget(self.col_search_btn)
        layout.addLayout(hdr_row)

        # Search box (hidden by default)
        self.col_search_box = QLineEdit()
        self.col_search_box.setPlaceholderText("Search COL files...")
        self.col_search_box.setVisible(False)
        self.col_search_box.textChanged.connect(self._filter_col_list)
        layout.addWidget(self.col_search_box)

        self.col_list_widget = QListWidget()
        self.col_list_widget.setAlternatingRowColors(True)
        #self.col_list_widget.setAutoFillBackground(True)
        self.col_list_widget.itemClicked.connect(self._on_col_selected)
        layout.addWidget(self.col_list_widget)
        return panel

    def _create_middle_panel(self): #vers 8
        """Create middle panel with COL models table — mini toolbar + view toggle."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(100)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(4)

        #    Header row: title + [T] view-toggle                           
        hdr_row = QHBoxLayout()
        self._col_models_header = QLabel("Collisions")
        self._col_models_header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        hdr_row.addWidget(self._col_models_header)
        hdr_row.addStretch()

        self._col_view_mode = 'detail'   # start in compact thumbnail view
        self.col_view_toggle_btn = QPushButton("[=]")
        self.col_view_toggle_btn.setFont(self.button_font)
        self.col_view_toggle_btn.setFixedWidth(32)
        self.col_view_toggle_btn.setFixedHeight(22)
        self.col_view_toggle_btn.setToolTip(
            "Toggle view: compact list / full details table")
        self.col_view_toggle_btn.clicked.connect(self._toggle_col_view)
        hdr_row.addWidget(self.col_view_toggle_btn)
        layout.addLayout(hdr_row)

        #    Mini toolbar: Open / Save / Extract / Undo                    
        icon_color = self._get_icon_color()
        self._middle_btn_row = QFrame()
        btn_layout = QHBoxLayout(self._middle_btn_row)
        btn_layout.setContentsMargins(0, 0, 0, 0)
        btn_layout.setSpacing(3)

        self.open_col_btn = QPushButton("")
        self.open_col_btn.setFont(self.button_font)
        self.open_col_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.open_col_btn.setIconSize(QSize(20, 20))
        self.open_col_btn.setToolTip("Open COL file")
        self.open_col_btn.clicked.connect(self._open_file)
        btn_layout.addWidget(self.open_col_btn)

        self.save_col_btn = QPushButton("")
        self.save_col_btn.setFont(self.button_font)
        self.save_col_btn.setIcon(self.icon_factory.save_icon(color=icon_color))
        self.save_col_btn.setIconSize(QSize(20, 20))
        self.save_col_btn.setToolTip("Save COL file")
        self.save_col_btn.clicked.connect(self._save_file)
        self.save_col_btn.setEnabled(True)
        btn_layout.addWidget(self.save_col_btn)

        self.export_col_btn = QPushButton("")
        self.export_col_btn.setFont(self.button_font)
        self.export_col_btn.setIcon(self.icon_factory.package_icon(color=icon_color))
        self.export_col_btn.setIconSize(QSize(20, 20))
        self.export_col_btn.setToolTip("Export all COL models")
        self.export_col_btn.clicked.connect(self._export_col_data)
        self.export_col_btn.setEnabled(True)
        btn_layout.addWidget(self.export_col_btn)

        self.undo_col_btn = QPushButton()
        self.undo_col_btn.setFont(self.button_font)
        self.undo_col_btn.setIcon(self.icon_factory.undo_icon(color=icon_color))
        self.undo_col_btn.setIconSize(QSize(20, 20))
        self.undo_col_btn.setToolTip("Undo last change")
        self.undo_col_btn.clicked.connect(self._undo_last_action)
        self.undo_col_btn.setEnabled(True)
        btn_layout.addWidget(self.undo_col_btn)

        btn_layout.addStretch()
        layout.addWidget(self._middle_btn_row)
        self._middle_btn_row.setVisible(self.is_docked and not self.standalone_mode)

        #    Model table (detail view)                                     
        self.collision_list = QTableWidget()

        class _GuiLayout:
            def __init__(self, table):  #vers 1
                self.table = table
        self.gui_layout = _GuiLayout(self.collision_list)

        self.collision_list.setColumnCount(8)
        self.collision_list.setHorizontalHeaderLabels([
            "Model Name", "Type", "Version", "Size",
            "Spheres", "Boxes", "Vertices", "Faces"])
        self.collision_list.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.collision_list.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)
        self.collision_list.setAlternatingRowColors(True)
        self.collision_list.itemSelectionChanged.connect(self._on_collision_selected)
        self.collision_list.horizontalHeader().setStretchLastSection(True)
        self.collision_list.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self.collision_list.customContextMenuRequested.connect(
            self._show_collision_context_menu)
        self.collision_list.setVisible(False)  # hidden at startup — compact view is default
        layout.addWidget(self.collision_list)

        #    Compact list (thumbnail + name/version/counts, single row)    
        self.col_compact_list = QTableWidget()
        self.col_compact_list.setColumnCount(2)
        self.col_compact_list.setHorizontalHeaderLabels(["Preview", "Details"])
        self.col_compact_list.horizontalHeader().setStretchLastSection(True)
        self.col_compact_list.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.col_compact_list.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)
        self.col_compact_list.setAlternatingRowColors(True)
        self.col_compact_list.setIconSize(QSize(64, 64))
        self.col_compact_list.itemSelectionChanged.connect(
            self._on_compact_col_selected)
        self.col_compact_list.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self.col_compact_list.customContextMenuRequested.connect(
            self._show_collision_context_menu)
        self.col_compact_list.setVisible(True)    # start in compact view
        self.col_compact_list.setRowCount(0)      # populated on first file load
        self.col_compact_list.setWordWrap(True)
        self.col_compact_list.setItemDelegate(_ColListDelegate(self.col_compact_list))
        layout.addWidget(self.col_compact_list)

        return self._wrap_middle_panel_with_surface_tab(panel)

    def _wrap_middle_panel_with_surface_tab(self, models_panel): #vers 3
        """Wrap the COL Models content in a local tab widget alongside
        Surface Data, so only the middle panel switches when the user picks
        a tab - the left panel (file list) and right panel (ribbons +
        viewport) stay visible and unaffected either way. Previously the
        whole 3-panel splitter was wrapped in one QTabWidget, which put the
        ribbons inside a padded tab pane and pushed them down with extra
        space below the title bar."""
        middle_tabs = QTabWidget()
        middle_tabs.setDocumentMode(True)
        middle_tabs.addTab(models_panel, "COL Models")
        self._surface_tab = self._create_surface_tab()
        middle_tabs.addTab(self._surface_tab, "Surface Data")
        middle_tabs.setMinimumWidth(180)  # Surface tab no longer forces 374px
        middle_tabs.currentChanged.connect(lambda *_: QTimer.singleShot(0, self._apply_left_compact))
        return middle_tabs

    def _create_right_panel(self): #vers 16
        """Right panel using QMainWindow + QToolBar for native docking.
        Same system as Model Workshop (Build 388+) - QMainWindow handles
        toolbar placement, row stacking, floating, and save/restore
        natively, replacing the old DockableToolbar panels."""
        icon_color = self._get_icon_color()

        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(200)
        self._right_panel_ref = panel
        outer_layout = QVBoxLayout(panel)
        outer_layout.setContentsMargins(4, 4, 4, 4)
        outer_layout.setSpacing(3)

        # Inner QMainWindow - owns the viewport as central widget and all
        # QToolBars. Embedded as a plain widget (no window chrome).
        from PyQt6.QtWidgets import QMainWindow
        inner_mw = QMainWindow()
        inner_mw.setWindowFlags(Qt.WindowType.Widget)
        inner_mw.setDockOptions(
            QMainWindow.DockOption.AllowNestedDocks |
            QMainWindow.DockOption.AllowTabbedDocks)
        self._inner_mw = inner_mw

        # Central widget: viewport wrapped in a plain QVBoxLayout (not a
        # bare setCentralWidget) so GLViewportMixin.switch_to_gl/switch_to_2d
        # can still find a real layout to swap the GL viewport into via
        # qp.parentWidget().layout().
        central = QWidget()
        central_layout = QVBoxLayout(central)
        central_layout.setContentsMargins(0, 0, 0, 0)
        central_layout.setSpacing(0)

        self.preview_widget = COL3DViewport()
        self.preview_widget._workshop_ref = self
        central_layout.addWidget(self.preview_widget, stretch=1)
        self.preview_row = central_layout   # kept for GLViewportMixin fallback path
        inner_mw.setCentralWidget(central)

        self._create_paint_bar()

        # Build all toolbars and add to the inner QMainWindow
        self._build_toolbars(inner_mw, icon_color)

        outer_layout.addWidget(inner_mw, stretch=1)

        # Restore saved toolbar state (positions, rows, floating)
        from PyQt6.QtCore import QTimer as _QT
        _QT.singleShot(400, self._restore_toolbar_state)

        # Save on close
        self.window_closed.connect(self._save_toolbar_state)

        return panel

    def _build_toolbars(self, mw: 'QMainWindow', icon_color: str): #vers 5
        """Build all QToolBar instances using QAction (Model Workshop pattern,
        Build 388+). Replaces the old DockableToolbar-based
        _create_transform_icon_panel/_create_preview_controls panels."""
        from PyQt6.QtWidgets import QToolBar
        from PyQt6.QtGui import QAction
        _saved_px = 20
        try:
            import json
            from pathlib import Path
            _saved_px = json.loads(
                (get_user_config_dir()/'col_workshop.json').read_text()
            ).get('icon_scale', 20)
        except Exception:
            pass
        icon_size = QSize(_saved_px, _saved_px)
        pw = self.preview_widget
        self._ribbon_actions = []

        def _tb(name, area=Qt.ToolBarArea.TopToolBarArea): #vers 1
            tb = QToolBar(name, mw)
            tb.setObjectName(name)
            tb.setIconSize(icon_size)
            tb.setMovable(True)
            tb.setFloatable(True)
            tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            tb.customContextMenuRequested.connect(
                lambda pos, t=tb: self._toolbar_context_menu(t, pos))
            mw.addToolBar(area, tb)
            return tb


        def _act(tb, name, icon_fn, callback=None, checkable=False,
                 checked=False, attr=None, enabled=True): #vers 1
            """Create a QAction, add to toolbar, register in _ribbon_actions."""
            try:
                icon = icon_fn(color=icon_color)
            except Exception:
                icon = self.icon_factory.settings_icon(color=icon_color)
            act = QAction(icon, name, mw)
            act.setToolTip(name)
            act.setCheckable(checkable)
            act.setEnabled(enabled)
            if checkable:
                act.setChecked(checked)
            if callback:
                if checkable:
                    act.toggled.connect(callback)
                else:
                    act.triggered.connect(callback)
            tb.addAction(act)
            self._ribbon_actions.append({
                'action': act, 'toolbar': tb, 'name': name,
                'icon_fn': icon_fn, 'checkable': checkable,
            })
            if attr:
                setattr(self, attr, act)
            return act

        #    Ribbon 1: Transform                                            
        # NOTE: attr= names match the original QPushButton names exactly
        # (flip_vert_btn etc.) so _set_col_buttons_enabled()/_refresh_icons()
        # elsewhere in this file keep working unchanged against QActions -
        # QAction supports setEnabled()/setIcon() with the same API.
        tb_xform = _tb("Transform")
        _act(tb_xform, "Flip Vertical",   self.icon_factory.flip_vert_icon,
             lambda: pw.flip_vertical(),  enabled=False, attr='flip_vert_btn')
        _act(tb_xform, "Flip Horizontal", self.icon_factory.flip_horz_icon,
             lambda: pw.flip_horizontal(),enabled=False, attr='flip_horz_btn')
        _act(tb_xform, "Rotate CW",       self.icon_factory.rotate_cw_icon,
             lambda: pw.rotate_cw(),      enabled=False, attr='rotate_cw_btn')
        _act(tb_xform, "Rotate CCW",      self.icon_factory.rotate_ccw_icon,
             lambda: pw.rotate_ccw(),     enabled=False, attr='rotate_ccw_btn')
        tb_xform.addSeparator()
        _act(tb_xform, "Analyze",  self.icon_factory.analyze_icon,
             self._analyze_collision, enabled=False, attr='analyze_btn')
        _act(tb_xform, "Copy",     self.icon_factory.copy_icon,
             self._copy_surface,     enabled=False, attr='copy_btn')
        _act(tb_xform, "Paste",    self.icon_factory.paste_icon,
             self._paste_surface,    enabled=False, attr='paste_btn')
        tb_xform.addSeparator()
        _act(tb_xform, "Create",    self.icon_factory.add_icon,
             self._create_new_surface, attr='create_surface_btn')
        _act(tb_xform, "Delete",    self.icon_factory.delete_icon,
             self._delete_surface,     enabled=False, attr='delete_surface_btn')
        _act(tb_xform, "Duplicate", self.icon_factory.duplicate_icon,
             self._duplicate_surface,  enabled=False, attr='duplicate_surface_btn')
        tb_xform.addSeparator()
        _act(tb_xform, "Paint",        self.icon_factory.paint_icon,
             self._open_paint_editor,       enabled=False, attr='paint_btn')
        _act(tb_xform, "Surface Types", self.icon_factory.checkerboard_icon,
             self._open_surface_type_dialog, attr='surface_type_btn')
        _act(tb_xform, "Surface Editor",self.icon_factory.surfaceedit_icon,
             self._open_surface_edit_dialog, attr='surface_edit_btn')
        _act(tb_xform, "Build from TXD",self.icon_factory.build_icon,
             self._build_col_from_txd,       attr='build_from_txd_btn')

        #    Ribbon 2: Navigation                                           
        tb_nav = _tb("Navigation", Qt.ToolBarArea.RightToolBarArea)
        _act(tb_nav, "Zoom In",       self.icon_factory.zoom_in_icon,  pw.zoom_in)
        _act(tb_nav, "Zoom Out",      self.icon_factory.zoom_out_icon, pw.zoom_out)
        _act(tb_nav, "Reset View",    self.icon_factory.reset_icon,    pw.reset_view)
        _act(tb_nav, "Fit to Window", self.icon_factory.fit_icon,      pw.fit_to_window)
        tb_nav.addSeparator()
        _act(tb_nav, "Pan Up",    self.icon_factory.arrow_up_icon,    lambda: pw.pan( 0,  20))
        _act(tb_nav, "Pan Down",  self.icon_factory.arrow_down_icon,  lambda: pw.pan( 0, -20))
        _act(tb_nav, "Pan Left",  self.icon_factory.arrow_left_icon,  lambda: pw.pan(-20,  0))
        _act(tb_nav, "Pan Right", self.icon_factory.arrow_right_icon, lambda: pw.pan( 20,  0))
        tb_nav.addSeparator()
        self.setup_gl_toggle(tb_nav, icon_color)

        #    Ribbon 3: Render                                               
        tb_rend = _tb("Render", Qt.ToolBarArea.RightToolBarArea)
        _act(tb_rend, "Render / Background Settings",
             self.icon_factory.color_picker_icon, self._open_render_settings_dialog)
        tb_rend.addSeparator()
        self.view_spheres_btn = _act(tb_rend, "Toggle Spheres", self.icon_factory.sphere_icon,
             lambda v: pw.set_show_spheres(v), checkable=True, checked=True, attr='_spheres_act')
        self.view_boxes_btn   = _act(tb_rend, "Toggle Boxes",   self.icon_factory.box_icon,
             lambda v: pw.set_show_boxes(v),   checkable=True, checked=True, attr='_boxes_act')
        self.view_mesh_btn    = _act(tb_rend, "Toggle Mesh",    self.icon_factory.mesh_icon,
             lambda v: pw.set_show_mesh(v),    checkable=True, checked=True, attr='_view_mesh_act')
        self.backface_btn     = _act(tb_rend, "Toggle Backface",self.icon_factory.backface_icon,
             lambda v: pw.set_backface(v),     checkable=True, checked=False, attr='_backface_act')

        #    Ribbon 4: Name                                                 
        # Replaces the old bottom info_group QFrame (COL name field + format/
        # switch/convert/compress/shadow buttons, duplicated between a wide
        # text+icon row and a narrow icon-only row that only showed below a
        # width threshold). One set of widgets now, real QToolBars.
        tb_name = _tb("Name", Qt.ToolBarArea.RightToolBarArea)
        self.info_name = QLineEdit()
        self.info_name.setText("Click to edit...")
        self.info_name.setReadOnly(True)
        self.info_name.setMinimumWidth(140)
        self.info_name.setStyleSheet("padding: 2px; border: 1px solid palette(mid);")
        self.info_name.mousePressEvent = lambda e: self._enable_name_edit(e, False)
        self.info_name.editingFinished.connect(self._apply_name_edit)
        tb_name.addWidget(self.info_name)

        #    Ribbon 5: Format                                               
        tb_format = _tb("Format", Qt.ToolBarArea.RightToolBarArea)
        self.format_combo = QComboBox()
        self.format_combo.addItems(["COL", "COL2", "COL3", "COL4"])
        self.format_combo.currentTextChanged.connect(self._change_format)
        self.format_combo.setMaximumWidth(100)
        tb_format.addWidget(self.format_combo)
        tb_format.addSeparator()
        _act(tb_format, "Cycle Render Mode", self.icon_factory.flip_vert_icon,
             self.switch_surface_view,  enabled=False, attr='switch_btn')
        _act(tb_format, "Convert Format",    self.icon_factory.convert_icon,
             self._convert_surface,     enabled=False, attr='convert_btn')
        _act(tb_format, "Compress",          self.icon_factory.compress_icon,
             self._compress_surface,    enabled=False, attr='compress_btn')
        _act(tb_format, "Uncompress",        self.icon_factory.uncompress_icon,
             self._uncompress_surface,  enabled=False, attr='uncompress_btn')
        tb_format.addSeparator()
        _act(tb_format, "Import",            self.icon_factory.import_icon,
             self._import_selected,     enabled=False, attr='import_btn')
        _act(tb_format, "Export",            self.icon_factory.export_icon,
             self.export_selected,      enabled=False, attr='export_btn')

        #    Ribbon 6: Shadow Mesh                                          
        tb_shadow = _tb("Shadow Mesh", Qt.ToolBarArea.RightToolBarArea)
        self.info_format = QLabel("Shadow Mesh:")
        self.info_format.setMinimumWidth(90)
        tb_shadow.addWidget(self.info_format)
        _act(tb_shadow, "View Shadow Mesh",   self.icon_factory.view_icon,
             self._show_shadow_mesh,    enabled=False, attr='show_shadow_btn')
        _act(tb_shadow, "Create Shadow Mesh", self.icon_factory.add_icon,
             self.shadow_dialog,        enabled=False, attr='create_shadow_btn')
        _act(tb_shadow, "Remove Shadow Mesh", self.icon_factory.delete_icon,
             self._remove_shadow,       enabled=False, attr='remove_shadow_btn')

        # Store toolbar refs
        self._tb_transform = tb_xform
        self._tb_nav       = tb_nav
        self._tb_render     = tb_rend
        self._tb_name        = tb_name
        self._tb_format      = tb_format
        self._tb_shadow      = tb_shadow

        # Collision-loaded-only actions - disabled until a COL model is loaded.
        # (Actual enable/disable on file load still goes through the existing
        # _set_col_buttons_enabled(), which uses these same attr names.)
        self._col_only_actions = [
            self.flip_vert_btn, self.flip_horz_btn,
            self.rotate_cw_btn, self.rotate_ccw_btn,
            self.analyze_btn, self.copy_btn, self.paste_btn,
            self.delete_surface_btn, self.duplicate_surface_btn,
            self.paint_btn,
        ]
        # Legacy compat for code that still walks button-list widgets directly
        self._col_icon_buttons = []
        self._col_ctrl_buttons = []

    def _toolbar_context_menu(self, toolbar, pos): #vers 2
        """Right-click context menu on any toolbar."""
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)

        # Icon Size submenu
        size_menu = menu.addMenu("Icon Size")
        from PyQt6.QtWidgets import QSlider, QWidgetAction
        slider = QSlider(Qt.Orientation.Horizontal)
        slider.setRange(14, 40)
        slider.setSingleStep(2)
        try:
            import json
            from pathlib import Path
            data = json.loads((get_user_config_dir()/'col_workshop.json').read_text())
            slider.setValue(data.get('icon_scale', 20))
        except Exception:
            slider.setValue(20)
        slider.valueChanged.connect(self._apply_icon_scale)
        wa = QWidgetAction(menu)
        wa.setDefaultWidget(slider)
        size_menu.addAction(wa)

        menu.addSeparator()
        menu.addAction("Ribbon Manager...", self.open_ribbon_manager)
        menu.addSeparator()
        from PyQt6.QtWidgets import QToolBar as _QTB
        menu.addAction("Lock All Toolbars",
            lambda: [tb.setMovable(False)
                     for tb in self._inner_mw.findChildren(_QTB)])
        menu.addAction("Unlock All Toolbars",
            lambda: [tb.setMovable(True)
                     for tb in self._inner_mw.findChildren(_QTB)])
        menu.exec(toolbar.mapToGlobal(pos))

    def _apply_icon_scale(self, px: int): #vers 2
        """Apply icon size to all toolbars live and persist it."""
        mw = getattr(self, '_inner_mw', None)
        if mw:
            from PyQt6.QtWidgets import QToolBar
            from PyQt6.QtCore import QSize as _QS
            for tb in mw.findChildren(QToolBar):
                tb.setIconSize(_QS(px, px))
        try:
            import json
            from pathlib import Path
            path = get_user_config_dir() / 'col_workshop.json'
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['icon_scale'] = px
            path.write_text(json.dumps(data, indent=2))
        except Exception:
            pass

    def open_ribbon_manager(self): #vers 1
        """Open the Ribbon Manager dialog."""
        dlg = RibbonManagerDialog(self, parent=self)
        dlg.exec()

    def _save_toolbar_state(self): #vers 3
        """Save QMainWindow toolbar state to col_workshop.json."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        try:
            import json
            from pathlib import Path
            path = get_user_config_dir() / 'col_workshop.json'
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['toolbar_state'] = mw.saveState(self._RIBBON_LAYOUT_VERSION).toHex().data().decode()
            data['toolbar_state_version'] = self._RIBBON_LAYOUT_VERSION
            path.write_text(json.dumps(data, indent=2))
            self._set_status("Ribbon config saved")
            main_wnd = getattr(self, 'main_window', None)
            if main_wnd and hasattr(main_wnd, 'log_message'):
                main_wnd.log_message("COL Workshop: Ribbon config saved")
        except Exception as _e:
            print(f"[COLWorkshop] _save_toolbar_state error: {_e}")

    def _restore_toolbar_state(self): #vers 4
        """Restore QMainWindow toolbar state from col_workshop.json.
        Uses an explicit layout version - bumped whenever ribbons are
        added/removed/renamed - so a stale save from an older ribbon
        layout is cleanly rejected instead of silently failing to
        restore (Qt's own toolbar-name hashing does this invisibly and
        without any way to detect success/failure)."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        try:
            import json
            from pathlib import Path
            from PyQt6.QtCore import QByteArray
            path = get_user_config_dir() / 'col_workshop.json'
            if not path.exists():
                return
            data = json.loads(path.read_text())
            state_hex = data.get('toolbar_state')
            saved_version = data.get('toolbar_state_version')
            if state_hex and saved_version == self._RIBBON_LAYOUT_VERSION:
                ok = mw.restoreState(QByteArray.fromHex(state_hex.encode()),
                                      self._RIBBON_LAYOUT_VERSION)
                if ok:
                    self._set_status("Ribbon config loaded")
                    main_wnd = getattr(self, 'main_window', None)
                    if main_wnd and hasattr(main_wnd, 'log_message'):
                        main_wnd.log_message("COL Workshop: Ribbon config loaded")
                else:
                    print("[COLWorkshop] _restore_toolbar_state: restoreState() returned False")
            elif state_hex:
                print(f"[COLWorkshop] Saved ribbon layout is from an older version "
                      f"({saved_version} != {self._RIBBON_LAYOUT_VERSION}) - skipping, "
                      f"will save fresh on next change.")
        except Exception as _e:
            print(f"[COLWorkshop] _restore_toolbar_state error: {_e}")
        finally:
            # Safety net: restoreState() can leave a ribbon fully hidden
            # (e.g. saved mid-drag, floating off-screen, squeezed out) with
            # no user-facing 'closed' state for these ribbons to have meant
            # intentionally - so force every one of them visible no matter
            # what happened above. Only position/floating/row should be
            # affected by the saved state, never full visibility.
            for tb in (getattr(self, '_tb_transform', None),
                       getattr(self, '_tb_nav', None),
                       getattr(self, '_tb_render', None),
                       getattr(self, '_tb_name', None),
                       getattr(self, '_tb_format', None),
                       getattr(self, '_tb_shadow', None)):
                if tb is not None:
                    tb.setVisible(True)
                    tb.toggleViewAction().setChecked(True)

    def _create_paint_bar(self): #vers 4
        """Floating paint bar — QWidget child of preview_widget, sits at top of viewport.
        Called once from _create_right_panel after preview_widget is created."""
        vp = self.preview_widget
        ic = self._get_icon_color()

        bar = QWidget(vp)
        bar.setObjectName("paint_bar")
        bar.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        bar.setStyleSheet(
            "QWidget#paint_bar { background:palette(base); border-bottom:2px solid palette(highlight); }"
            "QLabel  { color:palette(windowText); background:transparent; }"
            "QComboBox { background:palette(base); color:palette(text); border:1px solid palette(mid); }"
            "QPushButton { background:palette(button); color:palette(buttonText); border:1px solid palette(mid); border-radius:3px; }"
            "QPushButton:hover   { background:palette(dark); }"
            "QPushButton:checked { background:palette(highlight); color:palette(highlightedText); border:1px solid palette(highlight); }"
        )
        bar.setFixedHeight(34)

        lay = QHBoxLayout(bar)
        lay.setContentsMargins(6, 3, 6, 3)
        lay.setSpacing(4)

        lay.addWidget(QLabel("Mat:"))

        self.paint_swatch = QLabel()
        self.paint_swatch.setFixedSize(16, 16)
        self.paint_swatch.setStyleSheet(
            "background:#808080; border:1px solid palette(mid); border-radius:2px;")
        lay.addWidget(self.paint_swatch)

        self.paint_mat_combo = QComboBox()
        self.paint_mat_combo.setFixedHeight(26)
        self.paint_mat_combo.setMinimumWidth(160)
        self.paint_mat_combo.setMaximumWidth(260)
        lay.addWidget(self.paint_mat_combo)

        lay.addSpacing(4)

        def _tbtn(attr, icon_fn, tip, tool):  #vers 2
            b = QPushButton()
            try:
                b.setIcon(getattr(self.icon_factory, icon_fn)(color=ic))
            except Exception:
                b.setText(tool[0].upper())
            b.setIconSize(QSize(16, 16))
            b.setFixedSize(28, 28)
            b.setToolTip(tip)
            b.setCheckable(True)
            b.clicked.connect(lambda *_: self._set_paint_tool(tool))
            setattr(self, attr, b)
            lay.addWidget(b)

        _tbtn('tool_paint_btn',   'paint_icon',   'Paint faces',   'paint')
        _tbtn('tool_dropper_btn', 'dropper_icon', 'Dropper',       'dropper')
        _tbtn('tool_fill_btn',    'fill_icon',    'Flood fill',    'fill')
        if self.tool_paint_btn:
            self.tool_paint_btn.setChecked(True)

        self.paint_undo_btn = QPushButton()
        self.paint_undo_btn.setIcon(self.icon_factory.undo_paint_icon(color=ic))
        self.paint_undo_btn.setIconSize(QSize(16, 16))
        self.paint_undo_btn.setFixedSize(28, 28)
        self.paint_undo_btn.setToolTip("Undo last paint op")
        self.paint_undo_btn.setEnabled(False)
        self.paint_undo_btn.clicked.connect(self._undo_last_action)
        lay.addWidget(self.paint_undo_btn)

        lay.addStretch()

        self.paint_exit_btn = QPushButton()
        self.paint_exit_btn.setIcon(self.icon_factory.close_icon(20, self._get_icon_color()))
        self.paint_exit_btn.setFixedSize(28, 28)
        self.paint_exit_btn.setToolTip("Exit paint mode")
        self.paint_exit_btn.setStyleSheet(
            "color:palette(highlight); font-weight:bold; background:palette(base); border:1px solid palette(mid); border-radius:3px;")
        self.paint_exit_btn.clicked.connect(self._exit_paint_mode)
        lay.addWidget(self.paint_exit_btn)

        self.paint_toolbar = bar
        bar.setGeometry(0, 0, vp.width(), 34)
        bar.hide()

        # Reposition bar when viewport resizes
        _orig = vp.resizeEvent
        def _on_vp_resize(event, _o=_orig, _bar=bar, _vp=vp):  #vers 1
            _o(event)
            if _bar.isVisible():
                _bar.setGeometry(0, 0, _vp.width(), 34)
                _bar.raise_()
        vp.resizeEvent = _on_vp_resize

    def _apply_title_font(self): #vers 2
        """Apply title font to title bar labels"""
        if hasattr(self, 'title_font'):
            # Find all title labels
            for label in self.findChildren(QLabel):
                if label.objectName() == "title_label":
                    label.setFont(self.title_font)

    def _apply_panel_font(self): #vers 1
        """Apply panel font to info panels and labels"""
        if hasattr(self, 'panel_font'):
            # Apply to info labels (Mipmaps, Bumpmaps, status labels)
            for label in self.findChildren(QLabel):
                if any(x in label.text() for x in ["Mipmaps:", "Bumpmaps:", "Status:", "Type:", "Format:"]):
                    label.setFont(self.panel_font)

    def _apply_button_font(self): #vers 1
        """Apply button font to all buttons"""
        if hasattr(self, 'button_font'):
            for button in self.findChildren(QPushButton):
                button.setFont(self.button_font)

    def _apply_infobar_font(self): #vers 1
        """Apply fixed-width font to info bar at bottom"""
        if hasattr(self, 'infobar_font'):
            if hasattr(self, 'info_bar'):
                self.info_bar.setFont(self.infobar_font)

    def _show_settings_context_menu(self, pos): #vers 1
        """Show context menu for Settings button"""
        from PyQt6.QtWidgets import QMenu

        menu = QMenu(self)

        # Move window action
        move_action = menu.addAction("Move Window")
        move_action.triggered.connect(self._enable_move_mode)

        # Maximize window action
        max_action = menu.addAction("Maximize Window")
        max_action.triggered.connect(self._toggle_maximize)

        # Minimize action
        min_action = menu.addAction("Minimize")
        min_action.triggered.connect(self.showMinimized)

        menu.addSeparator()

        # Upscale Native action
        upscale_action = menu.addAction("Upscale Native")
        upscale_action.setCheckable(True)
        upscale_action.setChecked(False)
        upscale_action.triggered.connect(self._toggle_upscale_native)

        # Shaders action
        shaders_action = menu.addAction("Shaders")
        shaders_action.triggered.connect(self._show_shaders_dialog)

        menu.addSeparator()

        # Icon display mode submenu — auto-compact via resizeEvent/_update_transform_text_panel_visibility
        display_menu = menu.addMenu("Platform Display")

        icons_text_action = display_menu.addAction("Icons & Text")
        icons_text_action.setCheckable(True)
        icons_text_action.setChecked(self.icon_display_mode == "icons_and_text")
        icons_text_action.triggered.connect(lambda: self._set_icon_display_mode("icons_and_text"))

        icons_only_action = display_menu.addAction("Icons Only")
        icons_only_action.setCheckable(True)
        icons_only_action.setChecked(self.icon_display_mode == "icons_only")
        icons_only_action.triggered.connect(lambda: self._set_icon_display_mode("icons_only"))

        text_only_action = display_menu.addAction("Text Only")
        text_only_action.setCheckable(True)
        text_only_action.setChecked(self.icon_display_mode == "text_only")
        text_only_action.triggered.connect(lambda: self._set_icon_display_mode("text_only"))

        # Show menu at button position
        menu.exec(self.settings_btn.mapToGlobal(pos))

    def _get_icon_color(self): #vers 3
        """Get icon colour from current theme — returns text_primary.
        Falls back to main_window app_settings if own settings not loaded."""
        as_ = (self.app_settings
               or getattr(getattr(self, 'main_window', None), 'app_settings', None))
        if as_:
            try:
                colors = as_.get_theme_colors() or {}
                return colors.get('text_primary', '#cccccc')
            except Exception:
                pass
        return '#cccccc'

    def _on_theme_changed(self): #vers 1
        """Called when app theme switches -- reset viewport bg and repaint panels."""
        # Reset viewport so _set_theme_bg re-reads the new theme color
        pw = getattr(self, 'preview_widget', None)
        if pw and hasattr(pw, '_theme_bg_set'):
            pw._theme_bg_set = False
            pw.update()
        # Force palette refresh on left panel list widget
        if hasattr(self, 'col_list_widget') and self.col_list_widget:
            self.dff_list_widget.setStyleSheet(
                "QListWidget { background: palette(base); color: palette(windowText); "
                "border: none; } "
                "QListWidget::item:selected { background: palette(highlight); "
                "color: palette(highlightedText); }")
        # Repaint the whole workshop
        self.update()

    def _apply_theme(self): #vers 6
        """Apply global app theme — uses QApplication stylesheet set by app_settings."""
        try:
            app_settings = getattr(self, 'app_settings', None) or \
                getattr(getattr(self, 'main_window', None), 'app_settings', None)
            if app_settings and hasattr(app_settings, 'get_stylesheet'):
                from PyQt6.QtWidgets import QApplication
                ss = app_settings.get_stylesheet()
                if ss:
                    QApplication.instance().setStyleSheet(ss)
            # Clear widget-level override — children inherit from QApplication
            self.setStyleSheet("")
        except Exception as e:
            print(f"Theme application error: {e}")

    def _show_sort_menu(self): #vers 2
        """Show sort options popup."""
        from PyQt6.QtWidgets import QMenu
        m = QMenu(self)
        m.addAction("Sort by Name (A-Z)",     lambda: self._sort_models('name'))
        m.addAction("Sort by Version",         lambda: self._sort_models('version'))
        m.addAction("Sort by Faces (most)",    lambda: self._sort_models_desc('faces'))
        m.addAction("Sort by Boxes (most)",    lambda: self._sort_models_desc('boxes'))
        m.addAction("Sort by Spheres (most)",  lambda: self._sort_models_desc('spheres'))
        m.addAction("Sort by Vertices (most)", lambda: self._sort_models_desc('vertices'))
        m.exec(self.cursor().pos())

    def _show_collision_context_menu(self, position): #vers 6
        """Right-click context menu for both collision model lists."""
        # Work out which list sent the signal and find the row
        sender = self.sender()
        if sender is self.col_compact_list:
            source_list = self.col_compact_list
        else:
            source_list = self.collision_list

        item = source_list.itemAt(position)
        if not item:
            # Still show thumbnail-view submenu even on empty area
            row, model = -1, None
        else:
            row = source_list.row(item)
            if row < 0: row = -1
            models = getattr(self.current_col_file, 'models', []) if self.current_col_file else []
            model = models[row] if 0 <= row < len(models) else None

        menu = QMenu(self)

        #    Thumbnail view submenu (always shown)                          
        view_menu = menu.addMenu("Thumbnail View")
        axes = [
            ("Top  (XY — Z up)",    0,   0),
            ("Front (XZ — Y fwd)", 0,  90),
            ("Side  (YZ — X right)",90,  0),
            ("Isometric",          45,  35),
            ("Bottom",              0, 180),
            ("Back",              180,  90),
        ]
        for label, yaw, pitch in axes:
            # Tick current selection
            is_current = (abs(self._thumb_yaw - yaw) < 0.5 and
                          abs(self._thumb_pitch - pitch) < 0.5)
            act = view_menu.addAction(label)
            act.setCheckable(True)
            act.setChecked(is_current)
            act.triggered.connect(
                lambda _=False, y=yaw, p=pitch, l=label:
                    self._set_thumbnail_view(y, p, l))

        if model is not None:
            menu.addSeparator()

            #    Info                                                       
            details_action = menu.addAction("Show Details")
            details_action.triggered.connect(lambda: self._show_model_details(model, row))

            copy_action = menu.addAction("Copy Info to Clipboard")
            copy_action.triggered.connect(lambda: self._copy_model_info(model, row))

            menu.addSeparator()

            #    Rename                                                     
            rename_action = menu.addAction("Rename Model...")
            rename_action.triggered.connect(lambda: self._rename_col_model(model, row))

            menu.addSeparator()

            #    Export / Replace                                           
            export_action = menu.addAction("Export Model as COL...")
            export_action.triggered.connect(lambda: self._export_col_model(model, row))

            import_action = menu.addAction("Replace with COL file...")
            import_action.triggered.connect(lambda: self._import_replace_col_model(row))

            menu.addSeparator()

            #    Pin (protect from editing)                              
            is_pinned = self._is_model_pinned(row)
            pin_action = menu.addAction(
                "Unpin (allow editing)" if is_pinned else "Pin (protect from editing)")
            pin_action.triggered.connect(self._toggle_pin_selected)

        menu.addSeparator()

        #    Select / Sort                                               
        menu.addAction("Select All  [Ctrl+A]",  self._select_all_models)
        menu.addAction("Invert Selection  [Ctrl+Shift+I]", self._invert_selection)
        menu.addAction("Sort…",                 self._show_sort_menu)

        menu.addSeparator()

        #    IDE-linked operations                                       
        ide_menu = menu.addMenu("IDE Operations")
        ide_menu.addAction("Import matched by IDE…",    self._import_via_ide)
        ide_menu.addAction("Export matched by IDE…",    self._export_via_ide)
        ide_menu.addAction("Remove unreferenced by IDE…", self._remove_via_ide)

        menu.exec(source_list.mapToGlobal(position))

    def _setup_hotkeys(self): #vers 4
        """Keyboard shortcuts, each wired to its COL action."""
        from PyQt6.QtGui import QShortcut, QKeySequence
        SK = QKeySequence.StandardKey

        def _key(attr, seq, slot):  #vers 1
            sc = QShortcut(QKeySequence(seq), self)
            sc.activated.connect(slot)
            setattr(self, attr, sc)

        # File
        _key('hotkey_open',       SK.Open,         self._open_file)
        _key('hotkey_save',       SK.Save,         self._save_col_file)
        _key('hotkey_force_save', "Alt+Shift+S",   self._force_save_col)
        _key('hotkey_save_as',    SK.SaveAs,       self._save_as_col_file)
        _key('hotkey_close',      SK.Close,        self.close)
        # Edit
        _key('hotkey_undo',       SK.Undo,         self._undo_last_action)
        _key('hotkey_copy',       SK.Copy,         self._copy_surface)
        _key('hotkey_paste',      SK.Paste,        self._paste_surface)
        _key('hotkey_delete',     SK.Delete,       self._delete_surface)
        _key('hotkey_duplicate',  "Ctrl+D",        self._duplicate_surface)
        _key('hotkey_rename',     "F2",            lambda: self._enable_name_edit(None, False))
        # Collision operations
        _key('hotkey_import',     "Ctrl+I",        self._import_surface)
        _key('hotkey_export',     "Ctrl+E",        self.export_selected_surface)
        _key('hotkey_export_all', "Ctrl+Shift+E",  self.export_all_surfaces)
        # View
        _key('hotkey_refresh',    SK.Refresh,      self._reload_surface_table)
        _key('hotkey_properties', "Alt+Return",    self._show_detailed_info)
        _key('hotkey_settings',   SK.Preferences,  self._show_settings_dialog)
        _key('hotkey_select_all', SK.SelectAll,    self._select_all_models)
        _key('hotkey_invert',     "Ctrl+Shift+I",  self._invert_selection)
        _key('hotkey_find',       SK.Find,         self._focus_search)
        _key('hotkey_help',       SK.HelpContents, self.show_help)

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Hotkeys initialized (Plasma6 standard)")

    def _reset_hotkeys_to_defaults(self, parent_dialog): #vers 1
        """Reset all hotkeys to Plasma6 defaults"""
        from PyQt6.QtWidgets import QMessageBox
        from PyQt6.QtGui import QKeySequence

        reply = QMessageBox.question(parent_dialog, "Reset Hotkeys",
            "Reset all keyboard shortcuts to Plasma6 defaults?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)

        if reply == QMessageBox.StandardButton.Yes:
            # Reset to defaults
            self.hotkey_edit_open.setKeySequence(QKeySequence.StandardKey.Open)
            self.hotkey_edit_save.setKeySequence(QKeySequence.StandardKey.Save)
            self.hotkey_edit_force_save.setKeySequence(QKeySequence("Alt+Shift+S"))
            self.hotkey_edit_save_as.setKeySequence(QKeySequence.StandardKey.SaveAs)
            self.hotkey_edit_close.setKeySequence(QKeySequence.StandardKey.Close)
            self.hotkey_edit_undo.setKeySequence(QKeySequence.StandardKey.Undo)
            self.hotkey_edit_copy.setKeySequence(QKeySequence.StandardKey.Copy)
            self.hotkey_edit_paste.setKeySequence(QKeySequence.StandardKey.Paste)
            self.hotkey_edit_delete.setKeySequence(QKeySequence.StandardKey.Delete)
            self.hotkey_edit_duplicate.setKeySequence(QKeySequence("Ctrl+D"))
            self.hotkey_edit_rename.setKeySequence(QKeySequence("F2"))
            self.hotkey_edit_import.setKeySequence(QKeySequence("Ctrl+I"))
            self.hotkey_edit_export.setKeySequence(QKeySequence("Ctrl+E"))
            self.hotkey_edit_export_all.setKeySequence(QKeySequence("Ctrl+Shift+E"))
            self.hotkey_edit_refresh.setKeySequence(QKeySequence.StandardKey.Refresh)
            self.hotkey_edit_properties.setKeySequence(QKeySequence("Alt+Return"))
            self.hotkey_edit_find.setKeySequence(QKeySequence.StandardKey.Find)
            self.hotkey_edit_help.setKeySequence(QKeySequence.StandardKey.HelpContents)

    def _apply_hotkey_settings(self, dialog, close=False): #vers 1
        """Apply hotkey changes"""
        # Update all hotkeys with new sequences
        self.hotkey_open.setKey(self.hotkey_edit_open.keySequence())
        self.hotkey_save.setKey(self.hotkey_edit_save.keySequence())
        self.hotkey_force_save.setKey(self.hotkey_edit_force_save.keySequence())
        self.hotkey_save_as.setKey(self.hotkey_edit_save_as.keySequence())
        self.hotkey_close.setKey(self.hotkey_edit_close.keySequence())
        self.hotkey_undo.setKey(self.hotkey_edit_undo.keySequence())
        self.hotkey_copy.setKey(self.hotkey_edit_copy.keySequence())
        self.hotkey_paste.setKey(self.hotkey_edit_paste.keySequence())
        self.hotkey_delete.setKey(self.hotkey_edit_delete.keySequence())
        self.hotkey_duplicate.setKey(self.hotkey_edit_duplicate.keySequence())
        self.hotkey_rename.setKey(self.hotkey_edit_rename.keySequence())
        self.hotkey_import.setKey(self.hotkey_edit_import.keySequence())
        self.hotkey_export.setKey(self.hotkey_edit_export.keySequence())
        self.hotkey_export_all.setKey(self.hotkey_edit_export_all.keySequence())
        self.hotkey_refresh.setKey(self.hotkey_edit_refresh.keySequence())
        self.hotkey_properties.setKey(self.hotkey_edit_properties.keySequence())
        self.hotkey_find.setKey(self.hotkey_edit_find.keySequence())
        self.hotkey_help.setKey(self.hotkey_edit_help.keySequence())

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Hotkeys updated")

        # Save hotkeys to img_settings JSON
        try:
            mw = getattr(self, 'main_window', None)
            settings = getattr(mw, 'img_settings', None)
            if settings:
                settings.set('col_hotkeys', {
                    'find':    self.hotkey_edit_find.keySequence().toString(),
                    'help':    self.hotkey_edit_help.keySequence().toString(),
                })
        except Exception:
            pass

        if close:
            dialog.accept()

    def _set_status(self, msg: str): #vers 1
        """Write msg to the status label (whichever one exists)."""
        if hasattr(self, 'status_label'):
            self.status_label.setText(msg)
        elif hasattr(self, 'status_bar') and hasattr(self.status_bar, 'showMessage'):
            self.status_bar.showMessage(msg, 3000)
        else:
            print(f"[COL] {msg}")

    def _enable_name_edit(self, event, is_alpha): #vers 1
        """Enable name editing on click"""
        self.info_name.setReadOnly(False)
        self.info_name.selectAll()
        self.info_name.setFocus()

    def _set_col_buttons_enabled(self, enabled: bool): #vers 1
        """Enable/disable all transform buttons in BOTH icon and text panels.
        The text panel overwrites self.X refs, so when the icon panel is visible
        (narrow mode) those refs point to hidden buttons. Walk the icon panel too.
        """
        col_btn_attrs = [
            'flip_vert_btn', 'flip_horz_btn', 'rotate_cw_btn', 'rotate_ccw_btn',
            'analyze_btn', 'copy_btn', 'delete_surface_btn', 'duplicate_surface_btn',
            'paint_btn', 'surface_type_btn', 'surface_edit_btn', 'build_from_txd_btn',
            'show_shadow_btn', 'create_shadow_btn', 'remove_shadow_btn',
            'compress_btn', 'uncompress_btn', 'switch_btn', 'convert_btn',
        ]
        for attr in col_btn_attrs:
            btn = getattr(self, attr, None)
            if btn is not None:
                btn.setEnabled(enabled)
        icon_panel = getattr(self, '_transform_icon_panel_ref', None)
        if icon_panel:
            from PyQt6.QtWidgets import QPushButton
            for btn in icon_panel.findChildren(QPushButton):
                btn.setEnabled(enabled)
