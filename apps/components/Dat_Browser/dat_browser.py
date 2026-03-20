#this belongs in components/Dat_Browser/dat_browser.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - GTA DAT/IDE/IPL Browser
"""
DAT Browser — viewer panel for the GTA world data load chain.
Shows the parsed DAT → IDE → IPL hierarchy with object/instance tables.
Integrates into IMG Factory as a docked panel or tab.
"""

import os
from typing import Optional
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter,
    QTabWidget, QTableWidget, QTableWidgetItem, QHeaderView,
    QPushButton, QLabel, QLineEdit, QComboBox,
    QFileDialog, QTreeWidget, QTreeWidgetItem,
    QProgressBar, QTextEdit, QAbstractItemView,
    QMessageBox, QMenu, QApplication,
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal, pyqtSlot
from PyQt6.QtGui import QFont, QColor

try:
    from apps.methods.gta_dat_parser import (
        GTAGame, GTAWorldLoader, GTAWorldXRef, build_xref,
        detect_game, find_dat_file,
    )
    from apps.debug.debug_functions import img_debugger
except ImportError:
    from apps.methods.gta_dat_parser import (
        GTAGame, GTAWorldLoader, GTAWorldXRef, build_xref,
        detect_game, find_dat_file,
    )
    img_debugger = None

##Classes -
# DATBrowserWidget
# _LoadThread


# ─────────────────────────────────────────────────────────────────────────────
# Background load thread
# ─────────────────────────────────────────────────────────────────────────────

class _LoadThread(QThread): #vers 1
    progress = pyqtSignal(int, int, str)   # current, total, message
    finished = pyqtSignal(bool, str)       # success, summary

    def __init__(self, loader: GTAWorldLoader, dat_path: str, game_root: str):
        super().__init__()
        self.loader    = loader
        self.dat_path  = dat_path
        self.game_root = game_root

    def run(self): #vers 2
        def cb(cur, tot, msg):
            self.progress.emit(cur, tot, msg)
        ok = self.loader.load(self.game_root, progress_cb=cb)
        self.finished.emit(ok, self.loader.get_summary())


# ─────────────────────────────────────────────────────────────────────────────
# Main browser widget
# ─────────────────────────────────────────────────────────────────────────────

class DATBrowserWidget(QWidget): #vers 2
    """
    Full DAT/IDE/IPL browser panel.
    Drop into any QTabWidget or use standalone.
    """

    open_img_requested = pyqtSignal(str)          # emits abs path to .img
    xref_ready         = pyqtSignal(object)        # emits GTAWorldXRef after load

    def __init__(self, main_window=None, parent=None):
        super().__init__(parent)
        self.main_window = main_window
        self.loader      = GTAWorldLoader()
        self.xref:       Optional[GTAWorldXRef] = None
        self._thread:    Optional[_LoadThread]  = None
        self._setup_ui()

    # ── UI construction ────────────────────────────────────────────────────

    def _setup_ui(self): #vers 2
        # Ensure opaque background using panel_bg from theme
        self.setAutoFillBackground(True)
        from PyQt6.QtCore import Qt as _Qt
        self.setAttribute(_Qt.WidgetAttribute.WA_OpaquePaintEvent, True)
        try:
            mw = main_window
            bg = '#1e1e1e'  # safe dark default
            if mw and hasattr(mw, 'app_settings'):
                colors = mw.app_settings.get_theme_colors() or {}
                bg = colors.get('panel_bg', colors.get('bg_primary', bg))
            # Use stylesheet — survives re-parenting unlike palette
            self.setStyleSheet(f"DATBrowserWidget {{ background-color: {bg}; }}")
            from PyQt6.QtGui import QPalette, QColor
            pal = self.palette()
            pal.setColor(QPalette.ColorRole.Window, QColor(bg))
            self.setPalette(pal)
        except Exception:
            pass
        root = QVBoxLayout(self)
        root.setContentsMargins(6, 6, 6, 6)
        root.setSpacing(4)

        # Toolbar
        toolbar = QHBoxLayout()
        toolbar.setSpacing(6)

        self._game_combo = QComboBox()
        self._game_combo.addItems([
            "Auto-detect", "GTA III", "Vice City", "San Andreas", "GTASOL",
            "Game Root (Dir Tree)",
        ])
        self._game_combo.setFixedWidth(155)
        self._game_combo.setToolTip(
            "Select game, Auto-detect, or use Game Root from Dir Tree")
        self._game_combo.currentIndexChanged.connect(self._on_game_combo_changed)

        self._path_edit = QLineEdit()
        self._path_edit.setPlaceholderText("Game root folder (contains data/gta3.dat, gta_vc.dat or gta.dat)")
        self._path_edit.setReadOnly(True)

        browse_btn = QPushButton("Browse…")
        browse_btn.setToolTip("Browse for game root folder")
        browse_btn.clicked.connect(self._browse_game_root)
        self._browse_btn = browse_btn

        self._load_btn = QPushButton("Load")
        self._load_btn.setEnabled(False)
        self._load_btn.setToolTip("Load DAT/IDE/IPL world data")
        self._load_btn.clicked.connect(self._start_load)

        # Track current compact state; icons loaded lazily on first compact switch
        self._toolbar_compact = False

        toolbar.addWidget(QLabel("Game:"))
        toolbar.addWidget(self._game_combo)
        toolbar.addWidget(self._path_edit, 1)
        toolbar.addWidget(browse_btn)
        toolbar.addWidget(self._load_btn)
        root.addLayout(toolbar)

        # Progress bar (hidden when idle)
        self._progress = QProgressBar()
        self._progress.setVisible(False)
        self._progress.setTextVisible(True)
        root.addWidget(self._progress)

        # Status label
        self._status_lbl = QLabel("No game loaded.")
        self._status_lbl.setStyleSheet("font-style: italic;")
        root.addWidget(self._status_lbl)

        # Search / filter row
        search_row = QHBoxLayout()
        search_row.setSpacing(6)

        self._search_edit = QLineEdit()
        self._search_edit.setPlaceholderText("Filter by model name or ID…")
        self._search_edit.textChanged.connect(self._apply_filter)
        self._search_edit.setEnabled(False)

        self._type_filter = QComboBox()
        self._type_filter.addItems(
            ["All types", "object", "vehicle", "ped", "weapon", "hierarchy", "2dfx"])
        self._type_filter.currentTextChanged.connect(self._apply_filter)
        self._type_filter.setFixedWidth(110)
        self._type_filter.setEnabled(False)

        search_row.addWidget(QLabel("Filter:"))
        search_row.addWidget(self._search_edit, 1)
        search_row.addWidget(self._type_filter)
        root.addLayout(search_row)

        # Main splitter: left = load-order tree  |  right = data tabs
        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.setAutoFillBackground(True)
        root.addWidget(splitter, 1)

        # Left — file load-order tree
        left = QWidget()
        left.setAutoFillBackground(True)
        ll = QVBoxLayout(left)
        ll.setContentsMargins(0, 0, 0, 0)
        ll.addWidget(QLabel("Load Order:"))

        self._tree = QTreeWidget()
        self._tree.setHeaderLabels(["File", "Type", "Entries", "Status"])
        self._tree.setColumnWidth(0, 220)
        self._tree.setColumnWidth(1, 40)
        self._tree.setColumnWidth(2, 55)
        self._tree.setColumnWidth(3, 55)
        self._tree.setAlternatingRowColors(True)
        self._tree.itemClicked.connect(self._on_tree_click)
        ll.addWidget(self._tree)
        splitter.addWidget(left)

        # Enable right-click on tree to open source files
        self._setup_tree_context_menu()

        # Right — tabbed result tables
        self._tabs = QTabWidget()
        self._tabs.setAutoFillBackground(True)
        splitter.addWidget(self._tabs)
        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 3)

        self._obj_table = self._make_table(
            ["ID", "Model", "TXD", "Type", "Section", "Draw Dist", "Flags", "Source IDE"])
        self._tabs.addTab(self._obj_table, "Objects (IDE)")

        self._inst_table = self._make_table(
            ["ID", "Model", "Interior", "X", "Y", "Z", "Source IPL"])
        self._tabs.addTab(self._inst_table, "Instances (IPL)")

        self._zone_table = self._make_table(
            ["Name", "Type", "Min X", "Min Y", "Min Z",
             "Max X", "Max Y", "Max Z", "Island", "Key"])
        self._tabs.addTab(self._zone_table, "Zones")

        self._log_text = QTextEdit()
        self._log_text.setReadOnly(True)
        self._log_text.setFont(QFont("Consolas", 9))
        self._tabs.addTab(self._log_text, "Load Log")

    def _make_table(self, headers): #vers 3
        from apps.methods.populate_img_table import DragSelectTableWidget
        t = DragSelectTableWidget()
        t.setColumnCount(len(headers))
        t.setHorizontalHeaderLabels(headers)
        t.setAlternatingRowColors(True)
        t.setSortingEnabled(True)
        t.horizontalHeader().setStretchLastSection(True)
        t.horizontalHeader().setSectionsMovable(False)
        t.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        t.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        t.customContextMenuRequested.connect(
            lambda pos, tbl=t: self._table_context_menu(tbl, pos))
        return t

    # ── Responsive toolbar — compact (icon-only) below 520 px wide ────────

    # Width threshold below which Browse/Load collapse to icon-only buttons
    _COMPACT_THRESHOLD = 520

    def resizeEvent(self, event): #vers 1
        super().resizeEvent(event)
        self._update_toolbar_compact(event.size().width())

    def _update_toolbar_compact(self, width: int): #vers 2
        """Switch Browse/Load between full text and icon-only when narrow."""
        compact = width < self._COMPACT_THRESHOLD
        if compact == self._toolbar_compact:
            return
        self._toolbar_compact = compact

        from PyQt6.QtCore import QSize
        from PyQt6.QtGui import QIcon
        from PyQt6.QtWidgets import QSizePolicy

        bb = self._browse_btn
        lb = self._load_btn

        if compact:
            # Load icons lazily here (Qt display context guaranteed)
            if not getattr(bb, "_icon_loaded", False):
                try:
                    from apps.methods.imgfactory_svg_icons import get_folder_icon, get_go_icon
                    bb._icon = get_folder_icon(20)
                    lb._icon = get_go_icon(20)
                except Exception:
                    bb._icon = None
                    lb._icon = None
                bb._icon_loaded = True

            for btn, tip in (
                (bb, "Browse for game root folder"),
                (lb, "Load DAT world"),
            ):
                btn.setText("")
                icon = getattr(btn, "_icon", None)
                if icon and not icon.isNull():
                    btn.setIcon(icon)
                    btn.setIconSize(QSize(20, 20))
                # Square button: fixed at 32px (icon 20 + 6px padding each side)
                btn.setFixedSize(32, btn.sizeHint().height())
                btn.setToolTip(tip)
        else:
            for btn, text, tip in (
                (bb, "Browse…", "Browse for game root folder"),
                (lb, "Load",    "Load DAT/IDE/IPL world data"),
            ):
                btn.setIcon(QIcon())           # clear icon
                btn.setText(text)
                btn.setMinimumWidth(0)
                btn.setMaximumWidth(16777215)  # QWIDGETSIZE_MAX — let Qt size it
                btn.setSizePolicy(
                    QSizePolicy.Policy.Preferred,
                    QSizePolicy.Policy.Fixed)
                btn.setToolTip(tip)

    # ── Browse / load ──────────────────────────────────────────────────────

    def _on_game_combo_changed(self, idx: int): #vers 1
        """React immediately when the game combo selection changes.

        Index 5 = 'Game Root (Dir Tree)': grab the dir-tree path, auto-detect
        the game, switch the combo to the real entry, and start loading —
        no Browse or Load click required.
        """
        if idx != 5:
            # Show Browse/Load normally for all other entries
            self._browse_btn.setVisible(True)
            self._load_btn.setVisible(True)
            return

        # Hide Browse/Load — they are irrelevant for this mode
        self._browse_btn.setVisible(False)
        self._load_btn.setVisible(False)

        mw = self.main_window
        # Use project manager game_root only; fall back to home folder
        root = getattr(mw, "game_root", None)
        if not root or not os.path.isdir(root):
            root = os.path.expanduser("~")

        if not root or not os.path.isdir(root):
            self._status_lbl.setText(
                "No game root set — browse to find the game folder.")
            # Restore buttons so the user isn't stuck
            self._browse_btn.setVisible(True)
            self._load_btn.setVisible(True)
            return

        self._path_edit.setText(root)
        game = detect_game(root)
        if game:
            # Switch combo to the detected game (suppresses re-entrant signal)
            self._game_combo.blockSignals(True)
            real_idx = {GTAGame.GTA3: 1, GTAGame.VC: 2,
                        GTAGame.SA: 3, GTAGame.SOL: 4}.get(game, 0)
            self._game_combo.setCurrentIndex(real_idx)
            self._game_combo.blockSignals(False)
            names = {1: "GTA III", 2: "Vice City", 3: "San Andreas", 4: "GTASOL"}
            self._status_lbl.setText(
                f"Dir Tree: {names.get(real_idx, 'unknown')} — loading…")
        else:
            # Keep at 0 (Auto-detect) and let _start_load try
            self._game_combo.blockSignals(True)
            self._game_combo.setCurrentIndex(0)
            self._game_combo.blockSignals(False)
            self._status_lbl.setText("Dir Tree path set — auto-detecting game…")

        # Restore buttons now that path is filled, then kick off load
        self._browse_btn.setVisible(True)
        self._load_btn.setVisible(True)
        self._load_btn.setEnabled(True)
        self._start_load()

    def _on_split_toggle(self): #vers 1
        """Delegate split/full cycle to gui_layout."""
        try:
            mw = self._main_window
            gl = getattr(mw, 'gui_layout', None)
            if gl and hasattr(gl, '_toggle_merge_view_layout'):
                gl._toggle_merge_view_layout()
        except Exception:
            pass

    def _browse_game_root(self): #vers 2
        path = QFileDialog.getExistingDirectory(
            self, "Select GTA game root folder",
            self._path_edit.text() or os.path.expanduser("~"))
        if not path:
            return
        self._path_edit.setText(path)
        game = detect_game(path)
        if game:
            idx = {GTAGame.GTA3: 1, GTAGame.VC: 2, GTAGame.SA: 3, GTAGame.SOL: 4}.get(game, 0)
            self._game_combo.setCurrentIndex(idx)
            names = {1: "GTA III", 2: "Vice City", 3: "San Andreas", 4: "GTASOL"}
            self._status_lbl.setText(f"Detected: {names.get(idx, 'unknown')}")
        else:
            self._status_lbl.setText("Game not auto-detected — select manually.")
        self._load_btn.setEnabled(True)

    def _start_load(self): #vers 4
        game_idx = self._game_combo.currentIndex()
        game_root = self._path_edit.text().strip()
        if not game_root:
            return

        game_map = {0: None, 1: GTAGame.GTA3, 2: GTAGame.VC,
                    3: GTAGame.SA, 4: GTAGame.SOL}
        game = game_map.get(game_idx)
        if game is None:
            game = detect_game(game_root)
        if game is None:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Cannot detect game",
                "Could not find a supported game DAT file.\n"
                "Select the correct game from the dropdown.")
            return

        dat_path = find_dat_file(game_root, game)
        if not dat_path:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "DAT not found",
                f"Cannot find DAT file for {game} under {game_root}")
            return

        self.loader = GTAWorldLoader(game)
        self.xref   = None
        self._clear_ui()
        self._load_btn.setEnabled(False)
        self._search_edit.setEnabled(False)
        self._type_filter.setEnabled(False)
        self._progress.setVisible(True)
        self._progress.setValue(0)
        self._progress.setFormat("Starting…")

        self._thread = _LoadThread(self.loader, dat_path, game_root)
        self._thread.progress.connect(self._on_progress)
        self._thread.finished.connect(self._on_load_done)
        self._thread.start()

    @pyqtSlot(int, int, str)
    def _on_progress(self, cur, tot, msg): #vers 1
        if tot > 0:
            self._progress.setValue(int(100 * cur / tot))
        self._progress.setFormat(msg)

    @pyqtSlot(bool, str)
    def _on_load_done(self, ok, summary): #vers 2
        self._progress.setVisible(False)
        self._load_btn.setEnabled(True)
        if ok:
            self._status_lbl.setText(
                f"Loaded — {self.loader.stats.objects_loaded:,} objects, "
                f"{self.loader.stats.instances:,} instances, "
                f"{len(self.loader.zones):,} zones")
            # Build cross-reference index and notify listeners
            try:
                game_root = getattr(self._thread, "game_root", "")
                self.xref = build_xref(self.loader, game_root)
                self.xref.game_root = game_root   # tag xref with its game root
                self.xref_ready.emit(self.xref)
            except Exception as e:
                if img_debugger:
                    img_debugger.warning(f"XRef build failed: {e}")
        else:
            self._status_lbl.setText("Load failed — see Load Log tab.")
        self._populate_all()
        self._search_edit.setEnabled(True)
        self._type_filter.setEnabled(True)
        self._log_text.setPlainText(self._build_log_text())

    # ── Populate ───────────────────────────────────────────────────────────

    def _clear_ui(self): #vers 1
        self._obj_table.setRowCount(0)
        self._inst_table.setRowCount(0)
        self._zone_table.setRowCount(0)
        self._log_text.clear()
        self._tree.clear()

    def _populate_all(self): #vers 1
        self._populate_tree()
        self._populate_objects()
        self._populate_instances()
        self._populate_zones()

    def _populate_tree(self): #vers 2
        self._tree.clear()
        default_path = getattr(self.loader.default_dat, "dat_path", "")
        main_path    = getattr(self.loader.main_dat,    "dat_path", "")
        display_name = os.path.basename(main_path) if main_path else "unknown.dat"

        root_item = QTreeWidgetItem([display_name, "DAT", "", "✓"])
        root_item.setExpanded(True)
        self._tree.addTopLevelItem(root_item)

        if default_path:
            def_name = os.path.basename(default_path)
            def_item = QTreeWidgetItem([def_name, "DAT-1", "", "✓"])
            def_item.setForeground(0, QColor("#888888"))
            root_item.addChild(def_item)

        for phase, entry_type, path, success in self.loader.load_log:
            bname = os.path.basename(path)
            if entry_type == "IDE":
                count = sum(1 for o in self.loader.objects.values()
                            if o.source_ide == bname)
            elif entry_type == "IPL":
                count = sum(1 for i in self.loader.instances
                            if i.source_ipl == bname)
            else:
                count = 0

            status = "✓" if success else "✗ missing"
            child  = QTreeWidgetItem([bname, entry_type, str(count), status])
            if not success:
                for col in range(4):
                    child.setForeground(col, QColor("#cc4444"))
            root_item.addChild(child)

        self._tree.expandAll()

    def _populate_objects(self, filter_text="", filter_type="All types"): #vers 1
        table = self._obj_table
        table.setSortingEnabled(False)
        table.setRowCount(0)
        ft = filter_text.lower()

        for obj in self.loader.objects.values():
            if filter_type not in ("All types", "") and obj.obj_type != filter_type:
                continue
            if ft and ft not in obj.model_name.lower() and ft not in str(obj.model_id):
                continue
            row = table.rowCount()
            table.insertRow(row)
            for col, val in enumerate([
                str(obj.model_id), obj.model_name, obj.txd_name,
                obj.obj_type, obj.section,
                str(obj.extra.get("draw_dist", "")),
                str(obj.extra.get("flags", "")),
                obj.source_ide,
            ]):
                item = QTableWidgetItem(val)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                table.setItem(row, col, item)

        table.setSortingEnabled(True)
        self._tabs.setTabText(0, f"Objects ({table.rowCount():,})")

    def _populate_instances(self, filter_text=""): #vers 1
        table = self._inst_table
        table.setSortingEnabled(False)
        table.setRowCount(0)
        ft = filter_text.lower()

        for inst in self.loader.instances:
            if ft and ft not in inst.model_name.lower() and ft not in str(inst.model_id):
                continue
            row = table.rowCount()
            table.insertRow(row)
            for col, val in enumerate([
                str(inst.model_id), inst.model_name, str(inst.interior),
                f"{inst.pos_x:.3f}", f"{inst.pos_y:.3f}", f"{inst.pos_z:.3f}",
                inst.source_ipl,
            ]):
                item = QTableWidgetItem(val)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                table.setItem(row, col, item)

        table.setSortingEnabled(True)
        self._tabs.setTabText(1, f"Instances ({table.rowCount():,})")

    def _populate_zones(self): #vers 1
        table = self._zone_table
        table.setSortingEnabled(False)
        table.setRowCount(0)

        for z in self.loader.zones:
            row = table.rowCount()
            table.insertRow(row)
            for col, val in enumerate([
                z.get("name", ""), str(z.get("type", "")),
                f"{z.get('min_x',0):.1f}", f"{z.get('min_y',0):.1f}", f"{z.get('min_z',0):.1f}",
                f"{z.get('max_x',0):.1f}", f"{z.get('max_y',0):.1f}", f"{z.get('max_z',0):.1f}",
                str(z.get("island", "")), z.get("text_key", ""),
            ]):
                item = QTableWidgetItem(val)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                table.setItem(row, col, item)

        table.setSortingEnabled(True)
        self._tabs.setTabText(2, f"Zones ({table.rowCount():,})")

    def _build_log_text(self) -> str: #vers 1
        lines = [self.loader.get_summary(), "", "── Load order ──"]
        for phase, entry_type, path, success in self.loader.load_log:
            mark = "✓" if success else "✗ MISSING"
            lines.append(f"  [{entry_type}] {mark}  {path}")
        if self.loader.stats.warnings:
            lines += ["", f"── Warnings ({len(self.loader.stats.warnings)}) ──"]
            lines += [f"  {w}" for w in self.loader.stats.warnings[:100]]
            if len(self.loader.stats.warnings) > 100:
                lines.append(f"  … {len(self.loader.stats.warnings)-100} more")
        if self.loader.stats.errors:
            lines += ["", f"── Errors ({len(self.loader.stats.errors)}) ──"]
            lines += [f"  {e}" for e in self.loader.stats.errors]
        return "\n".join(lines)

    # ── Filter ─────────────────────────────────────────────────────────────

    def _apply_filter(self): #vers 1
        if not self.loader.objects:
            return
        self._populate_objects(
            self._search_edit.text(),
            self._type_filter.currentText())
        self._populate_instances(self._search_edit.text())

    # ── Tree click — filter to selected file ───────────────────────────────

    def _on_tree_click(self, item, col): #vers 1
        bname      = item.text(0)
        entry_type = item.text(1)
        if entry_type == "IDE":
            # Filter objects table to show only entries from this IDE
            self._search_edit.blockSignals(True)
            self._search_edit.setText("")
            self._search_edit.blockSignals(False)
            self._populate_objects_for_ide(bname)
        elif entry_type == "IPL":
            self._tabs.setCurrentIndex(1)
            self._populate_instances_for_ipl(bname)
        elif entry_type == "DAT":
            # Reset filters to show everything
            self._search_edit.setText("")
            self._type_filter.setCurrentIndex(0)

    def _populate_objects_for_ide(self, ide_basename: str): #vers 1
        table = self._obj_table
        table.setSortingEnabled(False)
        table.setRowCount(0)
        for obj in self.loader.objects.values():
            if obj.source_ide != ide_basename:
                continue
            row = table.rowCount()
            table.insertRow(row)
            for col, val in enumerate([
                str(obj.model_id), obj.model_name, obj.txd_name,
                obj.obj_type, obj.section,
                str(obj.extra.get("draw_dist", "")),
                str(obj.extra.get("flags", "")),
                obj.source_ide,
            ]):
                item = QTableWidgetItem(val)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                table.setItem(row, col, item)
        table.setSortingEnabled(True)
        self._tabs.setCurrentIndex(0)
        self._tabs.setTabText(0, f"Objects ({table.rowCount():,})  [{ide_basename}]")

    def _populate_instances_for_ipl(self, ipl_basename: str): #vers 1
        table = self._inst_table
        table.setSortingEnabled(False)
        table.setRowCount(0)
        for inst in self.loader.instances:
            if inst.source_ipl != ipl_basename:
                continue
            row = table.rowCount()
            table.insertRow(row)
            for col, val in enumerate([
                str(inst.model_id), inst.model_name, str(inst.interior),
                f"{inst.pos_x:.3f}", f"{inst.pos_y:.3f}", f"{inst.pos_z:.3f}",
                inst.source_ipl,
            ]):
                item = QTableWidgetItem(val)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                table.setItem(row, col, item)
        table.setSortingEnabled(True)
        self._tabs.setTabText(1, f"Instances ({table.rowCount():,})  [{ipl_basename}]")

    # ── Context menu ───────────────────────────────────────────────────────

    def _table_context_menu(self, table, pos): #vers 1
        index = table.indexAt(pos)
        if not index.isValid():
            return
        row  = index.row()
        menu = QMenu(self)

        copy_name = menu.addAction("Copy model name")
        copy_row  = menu.addAction("Copy row as text")
        menu.addSeparator()
        find_inst = menu.addAction("Find all instances of this model")

        chosen = menu.exec(table.viewport().mapToGlobal(pos))
        if not chosen:
            return

        if chosen == copy_name:
            it = table.item(row, 1)
            if it:
                QApplication.clipboard().setText(it.text())
        elif chosen == copy_row:
            parts = []
            for col in range(table.columnCount()):
                it = table.item(row, col)
                parts.append(it.text() if it else "")
            QApplication.clipboard().setText("\t".join(parts))
        elif chosen == find_inst:
            id_it   = table.item(row, 0)
            name_it = table.item(row, 1)
            if name_it:
                self._search_edit.setText(name_it.text())
                self._tabs.setCurrentIndex(1)

    # ── Tree right-click — open source file in editor ─────────────────────

    def _setup_tree_context_menu(self): #vers 1
        """Enable right-click on the load-order tree to open IDE/IPL/DAT files."""
        self._tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._tree.customContextMenuRequested.connect(self._on_tree_context_menu)

    def _on_tree_context_menu(self, pos): #vers 1
        item = self._tree.itemAt(pos)
        if not item:
            return
        entry_type = item.text(1)
        bname      = item.text(0)

        # Resolve the abs path from load_log
        abs_path = None
        for _phase, _etype, _path, _ok in self.loader.load_log:
            if os.path.basename(_path) == bname and _etype == entry_type:
                abs_path = _path
                break

        menu = QMenu(self)

        if abs_path and os.path.isfile(abs_path):
            ext = os.path.splitext(abs_path)[1].lower()
            # Text-editable types
            if ext in (".ide", ".ipl", ".dat", ".txt", ".cfg", ".ini"):
                edit_act = menu.addAction(f"Edit  {bname}")
                edit_act.triggered.connect(
                    lambda _=False, p=abs_path: self._open_path_in_editor(p))
                menu.addSeparator()

            # IDE → open structured IDE Editor
            if ext == ".ide":
                ide_act = menu.addAction("Open in IDE Editor")
                ide_act.triggered.connect(
                    lambda _=False, p=abs_path: self._open_in_ide_editor(p))
                menu.addSeparator()

        copy_act = menu.addAction("Copy path")
        copy_act.triggered.connect(
            lambda _=False, p=(abs_path or bname):
                QApplication.clipboard().setText(p))

        if menu.actions():
            menu.exec(self._tree.viewport().mapToGlobal(pos))

    def _open_path_in_editor(self, file_path: str): #vers 1
        """Open any text-type file in the IMG Factory text editor."""
        try:
            from apps.core.notepad import open_text_file_in_editor
            open_text_file_in_editor(file_path, self.main_window)
        except Exception as e:
            if self.main_window and hasattr(self.main_window, "log_message"):
                self.main_window.log_message(f"Text editor error: {e}")

    def _open_in_ide_editor(self, file_path: str): #vers 1
        """Open an .ide file in the structured IDE Editor."""
        try:
            from apps.components.Ide_Editor.ide_editor import open_ide_editor
            editor = open_ide_editor(self.main_window)
            editor.load_ide_file(file_path)
        except Exception as e:
            if self.main_window and hasattr(self.main_window, "log_message"):
                self.main_window.log_message(f"IDE Editor error: {e}")

    # ── Public API ─────────────────────────────────────────────────────────

    def load_from_game_root(self, game_root: str,
                             game: Optional[str] = None): #vers 2
        """Programmatic load (e.g. triggered when user opens a known game IMG)."""
        self._path_edit.setText(game_root)
        if game:
            idx = {GTAGame.GTA3: 1, GTAGame.VC: 2,
                   GTAGame.SA: 3, GTAGame.SOL: 4}.get(game, 0)
            self._game_combo.setCurrentIndex(idx)
        self._load_btn.setEnabled(True)
        self._start_load()


# ─────────────────────────────────────────────────────────────────────────────
# Integration hook
# ─────────────────────────────────────────────────────────────────────────────

def _wire_xref_signal(widget, main_window): #vers 2
    """Connect widget.xref_ready to apply tooltips on ALL open IMG tabs."""
    def _on_xref_ready(xref):
        # Store immediately — safe outside Qt model operations
        if main_window:
            main_window.xref = xref
            if not hasattr(main_window, 'xref_by_root'):
                main_window.xref_by_root = {}
            gr = getattr(xref, 'game_root', '')
            if gr:
                main_window.xref_by_root[gr] = xref
        # Defer table updates until after event loop is fully running
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(200, lambda: _apply_xref_to_tables(xref, main_window))

    def _apply_xref_to_tables(xref, main_window):
        try:
            from apps.methods.populate_img_table import apply_xref_tooltips, apply_xref_status
            tw = getattr(main_window, "main_tab_widget", None)
            if not tw:
                return
            total_tips = 0
            total_status = 0
            for i in range(tw.count()):
                tab = tw.widget(i)
                if not tab:
                    continue
                table = getattr(tab, "table_ref", None)
                if table and table.rowCount() > 0:
                    total_tips   += apply_xref_tooltips(table, xref)
                    total_status += apply_xref_status(table, xref)
                    for c in range(table.columnCount()):
                        h = table.horizontalHeaderItem(c)
                        if h and h.text() in ('IDE Model', 'IDE TXD'):
                            table.setColumnHidden(c, False)
            if hasattr(main_window, "log_message"):
                main_window.log_message(
                    f"XRef: {total_tips} tooltips, "
                    f"{total_status} status entries updated")
        except Exception as e:
            if hasattr(main_window, "log_message"):
                main_window.log_message(f"XRef apply error: {e}")

    widget.xref_ready.connect(_on_xref_ready)


def show_dat_browser(main_window) -> bool: #vers 2
    """Show the DAT Browser tab.

    If the tab was closed (widget still alive on main_window.dat_browser),
    re-adds it and switches to it.  If it was never created, calls
    integrate_dat_browser first.  Auto-fills game root from dir tree.
    """
    try:
        tw = getattr(main_window, "main_tab_widget", None)
        widget = getattr(main_window, "dat_browser", None)

        # Never created — create it now (integrate handles auto-fill too)
        if widget is None:
            return integrate_dat_browser(main_window)

        if tw is None:
            widget.show()
            widget.raise_()
            _auto_fill_game_root(widget, main_window)
            return True

        # Check if the tab is still in the tab widget
        for i in range(tw.count()):
            if tw.widget(i) is widget:
                tw.setCurrentIndex(i)
                _auto_fill_game_root(widget, main_window)
                _register_dat_taskbar(widget, main_window)
                return True

        # Tab was closed — re-add it
        tab_idx = tw.addTab(widget, "DAT Browser")
        tw.setCurrentIndex(tab_idx)
        widget.setAutoFillBackground(True)
        # Reapply stylesheet — palette is reset on re-parent
        try:
            mw = main_window
            bg = '#1e1e1e'
            if mw and hasattr(mw, 'app_settings'):
                colors = mw.app_settings.get_theme_colors() or {}
                bg = colors.get('panel_bg', colors.get('bg_primary', bg))
            widget.setStyleSheet(f"DATBrowserWidget {{ background-color: {bg}; }}")
        except Exception:
            pass
        widget.update()
        widget.repaint()
        _auto_fill_game_root(widget, main_window)
        _register_dat_taskbar(widget, main_window)
        if hasattr(main_window, "log_message"):
            main_window.log_message("DAT Browser re-opened")
        return True
    except Exception as e:
        if hasattr(main_window, "log_message"):
            main_window.log_message(f"DAT Browser show error: {e}")
        return False


def _auto_fill_game_root(widget: "DATBrowserWidget", main_window) -> None: #vers 1
    """Silently pre-fill the DAT Browser path field from the directory tree.

    Only updates the field if it is currently empty — never overwrites a path
    the user already set manually.  Does not trigger a load.
    """
    try:
        # Don't overwrite a path the user already chose
        if widget._path_edit.text().strip():
            return

        # Use project manager game_root only; fall back to home folder
        game_root = getattr(main_window, "game_root", None)
        if not game_root or not os.path.isdir(game_root):
            game_root = os.path.expanduser("~")
        if not game_root or not os.path.isdir(game_root):
            return

        from apps.methods.gta_dat_parser import detect_game, GTAGame
        game = detect_game(game_root)

        widget._path_edit.setText(game_root)
        if game:
            idx = {GTAGame.GTA3: 1, GTAGame.VC: 2,
                   GTAGame.SA: 3, GTAGame.SOL: 4}.get(game, 0)
            widget._game_combo.setCurrentIndex(idx)
            if hasattr(main_window, "log_message"):
                names = {GTAGame.GTA3: "GTA III", GTAGame.VC: "Vice City",
                         GTAGame.SA: "San Andreas", GTAGame.SOL: "GTASOL"}
                main_window.log_message(
                    f"DAT Browser: auto-detected {names[game]} at {game_root}")
        widget._load_btn.setEnabled(True)
    except Exception:
        pass  # Auto-fill is best-effort; never crash on it


def set_game_root_from_dir_tree(main_window) -> bool: #vers 1
    """Read current_path from the directory tree and pass it to the DAT Browser."""
    try:
        widget = getattr(main_window, "dat_browser", None)
        if widget is None:
            if hasattr(main_window, "log_message"):
                main_window.log_message("DAT Browser not open — open it first")
            return False

        # Project manager game_root takes priority over Dir Tree last-browsed path
        game_root = getattr(main_window, "game_root", None)
        if not game_root:
            dt = getattr(main_window, "directory_tree", None)
            game_root = getattr(dt, "current_path", None) if dt else None
        if not game_root:
            if hasattr(main_window, "log_message"):
                main_window.log_message("No game root set in directory tree")
            return False

        # Make browser visible first
        show_dat_browser(main_window)
        # Push the path into the browser's path field and auto-detect
        widget._path_edit.setText(game_root)
        from apps.methods.gta_dat_parser import detect_game, GTAGame
        game = detect_game(game_root)
        if game:
            idx = {GTAGame.GTA3: 1, GTAGame.VC: 2,
                   GTAGame.SA: 3, GTAGame.SOL: 4}.get(game, 0)
            widget._game_combo.setCurrentIndex(idx)
        widget._load_btn.setEnabled(True)
        if hasattr(main_window, "log_message"):
            names = {GTAGame.GTA3: "GTA III", GTAGame.VC: "Vice City",
                     GTAGame.SA: "San Andreas", GTAGame.SOL: "GTASOL"}
            main_window.log_message(
                f"DAT Browser: game root set to {game_root}"
                + (f"  [{names[game]}]" if game else " [undetected]"))
        return True
    except Exception as e:
        if hasattr(main_window, "log_message"):
            main_window.log_message(f"Set game root error: {e}")
        return False


def _register_dat_taskbar(widget, main_window): #vers 1
    """Register or activate the DAT button in the tool taskbar."""
    try:
        tb = getattr(main_window, 'tool_taskbar', None)
        if not tb:
            return
        if 'dat' not in tb._tools:
            from apps.methods.imgfactory_svg_icons import get_dat_browser_icon
            icon_color = getattr(tb, '_txt', '#e0e0e0')
            icon = get_dat_browser_icon(16, icon_color)
            tb.register('dat', 'DAT', icon, widget, 'DAT Browser')
        else:
            tb._tools['dat']['target'] = widget
        tb._set_exclusive_active('dat')
    except Exception:
        pass


def integrate_dat_browser(main_window) -> bool: #vers 5
    """Create DAT Browser widget and place it in the left_stack panel.
    Use show_dat_browser() / _show_dat_browser() to open/focus it.
    """
    try:
        from PyQt6.QtWidgets import QWidget as _QW
        parent_arg = main_window if isinstance(main_window, _QW) else None
        widget = DATBrowserWidget(main_window, parent=parent_arg)
        widget.setAutoFillBackground(True)
        main_window.dat_browser = widget

        # Place in left_stack page 1 if available
        gl = getattr(main_window, 'gui_layout', None)
        left_stack = getattr(gl, 'left_stack', None)
        if left_stack is not None:
            old = left_stack.widget(1)
            if old is not None:
                left_stack.removeWidget(old)
            left_stack.insertWidget(1, widget)
        else:
            # Fallback: floating window
            widget.setWindowTitle("GTA DAT/IDE/IPL Browser")
            widget.resize(900, 700)
            widget.show()

        _wire_xref_signal(widget, main_window)
        _auto_fill_game_root(widget, main_window)
        _register_dat_taskbar(widget, main_window)

        if hasattr(main_window, "log_message"):
            main_window.log_message("DAT Browser integrated (v5)")
        return True
    except Exception as e:
        if hasattr(main_window, "log_message"):
            main_window.log_message(f"DAT Browser integrate error: {e}")
        return False


__all__ = [
    "DATBrowserWidget",
    "integrate_dat_browser",
    "show_dat_browser",
    "set_game_root_from_dir_tree",
]
