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

    def _setup_ui(self): #vers 1
        root = QVBoxLayout(self)
        root.setContentsMargins(6, 6, 6, 6)
        root.setSpacing(4)

        # Toolbar
        toolbar = QHBoxLayout()
        toolbar.setSpacing(6)

        self._game_combo = QComboBox()
        self._game_combo.addItems(["Auto-detect", "GTA III", "Vice City", "San Andreas", "GTASOL"])
        self._game_combo.setFixedWidth(130)
        self._game_combo.setToolTip("Select game or Auto-detect from folder")

        self._path_edit = QLineEdit()
        self._path_edit.setPlaceholderText("Game root folder (contains data/gta3.dat, gta_vc.dat or gta.dat)")
        self._path_edit.setReadOnly(True)

        browse_btn = QPushButton("Browse…")
        browse_btn.setFixedWidth(72)
        browse_btn.clicked.connect(self._browse_game_root)

        self._load_btn = QPushButton("Load")
        self._load_btn.setFixedWidth(56)
        self._load_btn.setEnabled(False)
        self._load_btn.clicked.connect(self._start_load)

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
        root.addWidget(splitter, 1)

        # Left — file load-order tree
        left = QWidget()
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

        # Right — tabbed result tables
        self._tabs = QTabWidget()
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

    def _make_table(self, headers): #vers 1
        t = QTableWidget()
        t.setColumnCount(len(headers))
        t.setHorizontalHeaderLabels(headers)
        t.setAlternatingRowColors(True)
        t.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        t.setSortingEnabled(True)
        t.horizontalHeader().setStretchLastSection(True)
        t.horizontalHeader().setSectionsMovable(False)
        t.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        t.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        t.customContextMenuRequested.connect(
            lambda pos, tbl=t: self._table_context_menu(tbl, pos))
        return t

    # ── Browse / load ──────────────────────────────────────────────────────

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

    def _start_load(self): #vers 2
        game_root = self._path_edit.text().strip()
        if not game_root:
            return

        game_idx = self._game_combo.currentIndex()
        game_map = {0: None, 1: GTAGame.GTA3, 2: GTAGame.VC,
                    3: GTAGame.SA, 4: GTAGame.SOL}
        game = game_map.get(game_idx)
        if game is None:
            game = detect_game(game_root)
        if game is None:
            QMessageBox.warning(self, "Cannot detect game",
                "Could not find a supported game DAT file.\n"
                "Select the correct game from the dropdown.")
            return

        dat_path = find_dat_file(game_root, game)
        if not dat_path:
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
                self.xref = build_xref(self.loader)
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

def integrate_dat_browser(main_window) -> bool: #vers 2
    """Add a DAT Browser tab to main_window.main_tab_widget (non-closable permanent tab)."""
    try:
        widget = DATBrowserWidget(main_window, parent=main_window)

        if hasattr(main_window, "main_tab_widget"):
            tw = main_window.main_tab_widget
            tab_idx = tw.addTab(widget, "DAT Browser")
            # Make this tab non-closable (no X button)
            tw.tabBar().setTabButton(tab_idx,
                tw.tabBar().ButtonPosition.RightSide, None)
            tw.tabBar().setTabButton(tab_idx,
                tw.tabBar().ButtonPosition.LeftSide, None)
            main_window.dat_browser = widget
        else:
            widget.setWindowTitle("GTA DAT/IDE/IPL Browser")
            widget.resize(1100, 700)
            widget.show()
            main_window.dat_browser = widget

        # Wire xref_ready: apply tooltips to current IMG table when DAT loads
        def _on_xref_ready(xref):
            try:
                from apps.methods.populate_img_table import apply_xref_tooltips
                # Find the active IMG table
                tw = getattr(main_window, "main_tab_widget", None)
                if not tw:
                    return
                tab = tw.currentWidget()
                if not tab:
                    return
                table = getattr(tab, "table_ref", None)
                if table:
                    apply_xref_tooltips(table, xref)
            except Exception as e:
                if hasattr(main_window, "log_message"):
                    main_window.log_message(f"XRef tooltip error: {e}")

        widget.xref_ready.connect(_on_xref_ready)

        if hasattr(main_window, "log_message"):
            main_window.log_message("DAT Browser integrated (v2)")
        return True
    except Exception as e:
        if hasattr(main_window, "log_message"):
            main_window.log_message(f"DAT Browser integrate error: {e}")
        return False


__all__ = ["DATBrowserWidget", "integrate_dat_browser"]
