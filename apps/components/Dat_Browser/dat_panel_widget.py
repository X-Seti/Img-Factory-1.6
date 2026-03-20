#this belongs in apps/components/Dat_Browser/dat_panel_widget.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - DAT Browser Panel Widget
"""
DAT Browser Panel Widget
Structured identically to DirectoryTreeBrowser — solid QWidget subclass
with a toolbar row, content area, and no transparency issues.
Replaces DATBrowserWidget as the panel seen in the left_stack.
"""

import os
from typing import Optional

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QLabel, QComboBox,
    QLineEdit, QPushButton, QProgressBar, QSplitter,
    QTreeWidget, QTreeWidgetItem, QTabWidget, QTableWidget,
    QTableWidgetItem, QHeaderView, QAbstractItemView,
    QMenu, QFileDialog, QSizePolicy
)
from PyQt6.QtCore import Qt, QSize, QThread, pyqtSignal, pyqtSlot
from PyQt6.QtGui import QFont

##Methods list -
# DATPanel.__init__
# DATPanel.setup_ui
# DATPanel.create_toolbar
# DATPanel.setup_connections
# DATPanel.apply_styling
# DATPanel._browse_game_root
# DATPanel._start_load
# DATPanel._on_load_done
# DATPanel._on_progress
# DATPanel._on_game_combo_changed
# DATPanel._on_split_toggle
# DATPanel._sync_split_icon
# DATPanel._apply_filter
# DATPanel._populate_all
# integrate_dat_panel


class DATPanel(QWidget): #vers 1
    """DAT Browser as a clean panel widget — same structure as DirectoryTreeBrowser."""

    xref_ready = pyqtSignal(object)

    def __init__(self, main_window, parent=None): #vers 1
        super().__init__(parent)
        self._main_window  = main_window
        self._thread       = None
        self._loader       = None
        self.xref          = None
        self._toolbar_compact = False

        self.setup_ui()
        self.setup_connections()
        self.apply_styling()

    # ── UI construction ──────────────────────────────────────────────────────

    def setup_ui(self): #vers 1
        """Main layout — toolbar on top, splitter below. No margins so it fills parent."""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        layout.addWidget(self.create_toolbar())
        layout.addWidget(self._make_progress_row())
        layout.addWidget(self._make_filter_row())
        layout.addWidget(self._make_splitter(), 1)

    def create_toolbar(self): #vers 1
        """Row: [Game combo] [path] [Browse] [Load] [split/full]"""
        toolbar = QWidget()
        toolbar.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        row = QHBoxLayout(toolbar)
        row.setContentsMargins(4, 4, 4, 4)
        row.setSpacing(4)

        row.addWidget(QLabel("Game:"))

        self._game_combo = QComboBox()
        self._game_combo.addItems([
            "Auto-detect", "GTA III", "Vice City",
            "San Andreas", "GTASOL",
        ])
        self._game_combo.setFixedWidth(140)
        self._game_combo.setToolTip("Select game or auto-detect")
        row.addWidget(self._game_combo)

        self._path_edit = QLineEdit()
        self._path_edit.setPlaceholderText("Game root folder…")
        self._path_edit.setReadOnly(True)
        row.addWidget(self._path_edit, 1)

        self._browse_btn = QPushButton("Browse…")
        self._browse_btn.setToolTip("Browse for game root folder")
        row.addWidget(self._browse_btn)

        self._load_btn = QPushButton("Load")
        self._load_btn.setEnabled(False)
        self._load_btn.setToolTip("Load DAT / IDE / IPL world data")
        row.addWidget(self._load_btn)

        # Split/full button — identical to Dir Tree split_toggle_btn
        self._split_btn = QPushButton()
        self._split_btn.setFixedSize(24, 24)
        self._split_btn.setIconSize(QSize(20, 20))
        self._split_btn.setToolTip("Panel left | Files right → click to cycle layout")
        row.addWidget(self._split_btn)

        return toolbar

    def _make_progress_row(self): #vers 1
        self._progress = QProgressBar()
        self._progress.setVisible(False)
        self._progress.setTextVisible(True)
        self._progress.setFixedHeight(16)
        return self._progress

    def _make_filter_row(self): #vers 1
        w = QWidget()
        row = QHBoxLayout(w)
        row.setContentsMargins(4, 2, 4, 2)
        row.setSpacing(4)

        self._status_lbl = QLabel("No game loaded.")
        self._status_lbl.setStyleSheet("font-style: italic;")
        row.addWidget(self._status_lbl, 1)

        self._search_edit = QLineEdit()
        self._search_edit.setPlaceholderText("Filter by model name or ID…")
        self._search_edit.setEnabled(False)
        row.addWidget(self._search_edit, 1)

        self._type_filter = QComboBox()
        self._type_filter.addItem("All types")
        self._type_filter.setEnabled(False)
        row.addWidget(self._type_filter)

        return w

    def _make_splitter(self): #vers 1
        """Left = load-order tree, right = tabbed result tables."""
        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.setChildrenCollapsible(False)

        # Left — load order tree
        left = QWidget()
        ll = QVBoxLayout(left)
        ll.setContentsMargins(0, 0, 0, 0)
        ll.setSpacing(0)
        ll.addWidget(QLabel("Load Order:"))
        self._tree = QTreeWidget()
        self._tree.setHeaderLabels(["File", "Type", "Entries", "Status"])
        self._tree.setColumnWidth(0, 200)
        self._tree.setColumnWidth(1, 40)
        self._tree.setColumnWidth(2, 50)
        self._tree.setAlternatingRowColors(True)
        ll.addWidget(self._tree)
        splitter.addWidget(left)

        # Right — tabbed tables
        self._tabs = QTabWidget()
        self._obj_table   = self._make_table(
            ["ID", "Model", "TXD", "Type", "Section", "Draw Dist", "Flags", "Source IDE"])
        self._ipl_table   = self._make_table(
            ["Model", "X", "Y", "Z", "RX", "RY", "RZ", "RW", "Interior", "LOD"])
        self._zones_table = self._make_table(
            ["Name", "Type", "X1", "Y1", "Z1", "X2", "Y2", "Z2", "Level"])
        self._log_table   = self._make_table(
            ["Phase", "Type", "Path", "OK"])
        self._tabs.addTab(self._obj_table,   "Objects (IDE)")
        self._tabs.addTab(self._ipl_table,   "Instances (IPL)")
        self._tabs.addTab(self._zones_table, "Zones")
        self._tabs.addTab(self._log_table,   "Load Log")
        splitter.addWidget(self._tabs)

        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 3)
        return splitter

    def _make_table(self, headers): #vers 1
        t = QTableWidget()
        t.setColumnCount(len(headers))
        t.setHorizontalHeaderLabels(headers)
        t.setAlternatingRowColors(True)
        t.setSortingEnabled(True)
        t.horizontalHeader().setStretchLastSection(True)
        t.horizontalHeader().setSectionsMovable(False)
        t.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        t.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        return t

    # ── Connections ──────────────────────────────────────────────────────────

    def setup_connections(self): #vers 1
        self._browse_btn.clicked.connect(self._browse_game_root)
        self._load_btn.clicked.connect(self._start_load)
        self._game_combo.currentIndexChanged.connect(self._on_game_combo_changed)
        self._search_edit.textChanged.connect(self._apply_filter)
        self._split_btn.clicked.connect(self._on_split_toggle)
        self._tree.itemClicked.connect(self._on_tree_click)
        # Defer split icon until gui_layout ready
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(300, self._sync_split_icon)

    # ── Styling ──────────────────────────────────────────────────────────────

    def apply_styling(self): #vers 1
        """Apply theme-aware styling — called once and survives re-parenting."""
        try:
            mw = self._main_window
            bg = '#1a1a2e'
            if mw and hasattr(mw, 'app_settings'):
                colors = mw.app_settings.get_theme_colors() or {}
                bg = colors.get('panel_bg', colors.get('bg_primary', bg))
            self.setStyleSheet(f"""
                DATPanel {{
                    background-color: {bg};
                }}
                QWidget {{
                    background-color: {bg};
                }}
                QTabWidget::pane {{
                    background-color: {bg};
                    border: 1px solid #3a3a3a;
                }}
                QHeaderView::section {{
                    background-color: #2a2a3e;
                }}
            """)
        except Exception:
            pass

    # ── Split button ─────────────────────────────────────────────────────────

    def _sync_split_icon(self): #vers 1
        """Set icon matching current merge_view_state in gui_layout."""
        try:
            from apps.methods.imgfactory_svg_icons import (
                get_layout_w1left_icon, get_layout_w1top_icon,
                get_layout_w2left_icon, get_layout_w2top_icon
            )
            gl    = getattr(self._main_window, 'gui_layout', None)
            state = getattr(gl, '_merge_view_state', 0) if gl else 0
            color = '#cccccc'
            try:
                tb = getattr(self._main_window, 'tool_taskbar', None)
                if tb: color = getattr(tb, '_txt', color)
            except Exception:
                pass
            table = {
                0: (get_layout_w1left_icon, "Panel left | Files right"),
                1: (get_layout_w1top_icon,  "Panel top / Files bottom"),
                2: (get_layout_w2left_icon, "Files left | Panel right"),
                3: (get_layout_w2top_icon,  "Panel hidden"),
            }
            fn, tip = table.get(state, table[0])
            self._split_btn.setIcon(fn(20, color))
            self._split_btn.setToolTip(tip + " → click to cycle")
        except Exception:
            pass

    def _on_split_toggle(self): #vers 1
        try:
            gl = getattr(self._main_window, 'gui_layout', None)
            if gl and hasattr(gl, '_toggle_merge_view_layout'):
                gl._toggle_merge_view_layout()
            self._sync_split_icon()
        except Exception as e:
            if hasattr(self._main_window, 'log_message'):
                self._main_window.log_message(f"Split toggle: {e}")

    # ── Game root browsing / loading ─────────────────────────────────────────

    def _on_game_combo_changed(self, idx): #vers 1
        try:
            from apps.methods.gta_dat_parser import detect_game, GTAGame
            mw = self._main_window
            root = getattr(mw, 'game_root', None)
            if not root or not os.path.isdir(root):
                root = os.path.expanduser('~')
            self._path_edit.setText(root)
            self._load_btn.setEnabled(bool(root))
        except Exception:
            pass

    def _browse_game_root(self): #vers 1
        path = QFileDialog.getExistingDirectory(
            self, "Select GTA game root folder",
            self._path_edit.text() or os.path.expanduser('~'))
        if not path:
            return
        self._path_edit.setText(path)
        try:
            from apps.methods.gta_dat_parser import detect_game, GTAGame
            game = detect_game(path)
            if game:
                idx = {GTAGame.GTA3: 1, GTAGame.VC: 2,
                       GTAGame.SA: 3, GTAGame.SOL: 4}.get(game, 0)
                self._game_combo.setCurrentIndex(idx)
                self._status_lbl.setText(f"Detected: {game}")
            else:
                self._status_lbl.setText("Game not detected — select manually.")
        except Exception:
            pass
        self._load_btn.setEnabled(True)

    def _start_load(self): #vers 1
        try:
            from apps.methods.gta_dat_parser import (
                GTAGame, GTAWorldLoader, find_dat_file, detect_game)
            game_root = self._path_edit.text().strip()
            if not game_root:
                return
            idx  = self._game_combo.currentIndex()
            gmap = {0: None, 1: GTAGame.GTA3, 2: GTAGame.VC,
                    3: GTAGame.SA, 4: GTAGame.SOL}
            game = gmap.get(idx) or detect_game(game_root)
            if not game:
                self._status_lbl.setText("Cannot detect game.")
                return
            dat_path = find_dat_file(game_root, game)
            if not dat_path:
                self._status_lbl.setText("DAT file not found.")
                return

            self._loader = GTAWorldLoader(game)
            self._load_btn.setEnabled(False)
            self._search_edit.setEnabled(False)
            self._type_filter.setEnabled(False)
            self._progress.setVisible(True)
            self._progress.setValue(0)

            from apps.components.Dat_Browser.dat_browser import _LoadThread
            self._thread = _LoadThread(self._loader, dat_path, game_root)
            self._thread.progress.connect(self._on_progress)
            self._thread.finished.connect(self._on_load_done)
            self._thread.start()
        except Exception as e:
            self._status_lbl.setText(f"Load error: {e}")

    @pyqtSlot(int, int, str)
    def _on_progress(self, cur, tot, msg): #vers 1
        if tot > 0:
            self._progress.setValue(int(100 * cur / tot))
        self._progress.setFormat(msg)

    @pyqtSlot(bool, str)
    def _on_load_done(self, ok, summary): #vers 1
        self._progress.setVisible(False)
        self._load_btn.setEnabled(True)
        self._search_edit.setEnabled(ok)
        self._type_filter.setEnabled(ok)
        if ok:
            self._status_lbl.setText(summary or "Loaded.")
            try:
                game_root = getattr(self._thread, 'game_root', '')
                from apps.methods.gta_dat_parser import build_xref
                self.xref = build_xref(self._loader, game_root)
                self.xref.game_root = game_root
                # Store on main_window
                mw = self._main_window
                if mw:
                    mw.xref = self.xref
                    if not hasattr(mw, 'xref_by_root'):
                        mw.xref_by_root = {}
                    if game_root:
                        mw.xref_by_root[game_root] = self.xref
                self.xref_ready.emit(self.xref)
            except Exception as e:
                if hasattr(self._main_window, 'log_message'):
                    self._main_window.log_message(f"XRef error: {e}")
            self._populate_all()
        else:
            self._status_lbl.setText("Load failed.")

    # ── Population ───────────────────────────────────────────────────────────

    def _populate_all(self): #vers 1
        if not self._loader:
            return
        try:
            self._populate_tree()
            self._populate_objects()
            self._populate_ipl()
            self._populate_zones()
            self._populate_log()
        except Exception as e:
            if hasattr(self._main_window, 'log_message'):
                self._main_window.log_message(f"Populate error: {e}")

    def _populate_tree(self): #vers 1
        self._tree.clear()
        loader = self._loader
        for phase, dat in [("default", loader.default_dat),
                           ("main",    loader.main_dat)]:
            phase_item = QTreeWidgetItem([phase, "", "", ""])
            self._tree.addTopLevelItem(phase_item)
            for e in dat.entries:
                row = QTreeWidgetItem([
                    os.path.basename(e.path),
                    e.directive,
                    "",
                    "✓" if e.exists else "✗"
                ])
                phase_item.addChild(row)
            phase_item.setExpanded(True)

    def _populate_objects(self): #vers 1
        t = self._obj_table
        t.setSortingEnabled(False)
        t.setRowCount(0)
        objs = list(self._loader.objects.values())
        t.setRowCount(len(objs))
        for r, obj in enumerate(objs):
            t.setItem(r, 0, QTableWidgetItem(str(obj.model_id)))
            t.setItem(r, 1, QTableWidgetItem(obj.model_name))
            t.setItem(r, 2, QTableWidgetItem(obj.txd_name or ''))
            t.setItem(r, 3, QTableWidgetItem(getattr(obj, 'object_type', '')))
            t.setItem(r, 4, QTableWidgetItem(getattr(obj, 'source_ide', '')))
        t.setSortingEnabled(True)

    def _populate_ipl(self): #vers 1
        t = self._ipl_table
        t.setSortingEnabled(False)
        t.setRowCount(0)
        insts = self._loader.instances
        t.setRowCount(len(insts))
        for r, inst in enumerate(insts):
            t.setItem(r, 0, QTableWidgetItem(getattr(inst, 'model_name', '')))
            pos = getattr(inst, 'position', None)
            if pos:
                for i, v in enumerate(pos[:3]):
                    t.setItem(r, 1+i, QTableWidgetItem(f"{v:.2f}"))
        t.setSortingEnabled(True)

    def _populate_zones(self): #vers 1
        t = self._zones_table
        t.setSortingEnabled(False)
        t.setRowCount(0)
        zones = self._loader.zones
        t.setRowCount(len(zones))
        for r, z in enumerate(zones):
            t.setItem(r, 0, QTableWidgetItem(str(z.get('name', ''))))
            t.setItem(r, 1, QTableWidgetItem(str(z.get('type', ''))))
        t.setSortingEnabled(True)

    def _populate_log(self): #vers 1
        t = self._log_table
        t.setSortingEnabled(False)
        t.setRowCount(0)
        log = self._loader.load_log
        t.setRowCount(len(log))
        for r, (phase, typ, path, ok) in enumerate(log):
            t.setItem(r, 0, QTableWidgetItem(phase))
            t.setItem(r, 1, QTableWidgetItem(typ))
            t.setItem(r, 2, QTableWidgetItem(os.path.basename(path)))
            t.setItem(r, 3, QTableWidgetItem("✓" if ok else "✗"))
        t.setSortingEnabled(True)

    def _on_tree_click(self, item, col): #vers 1
        pass  # future: filter tables to selected dat file

    def _apply_filter(self, text): #vers 1
        t = self._obj_table
        text = text.lower()
        for row in range(t.rowCount()):
            name = t.item(row, 1)
            match = not text or (name and text in name.text().lower())
            t.setRowHidden(row, not match)

    def auto_fill_game_root(self): #vers 1
        """Pre-fill path from project manager game_root."""
        try:
            mw = self._main_window
            root = getattr(mw, 'game_root', None)
            if root and os.path.isdir(root):
                self._path_edit.setText(root)
                self._load_btn.setEnabled(True)
                from apps.methods.gta_dat_parser import detect_game, GTAGame
                game = detect_game(root)
                if game:
                    idx = {GTAGame.GTA3: 1, GTAGame.VC: 2,
                           GTAGame.SA: 3, GTAGame.SOL: 4}.get(game, 0)
                    self._game_combo.setCurrentIndex(idx)
        except Exception:
            pass


def integrate_dat_panel(main_window) -> bool: #vers 1
    """Create DATPanel and place it in left_stack page 1."""
    try:
        panel = DATPanel(main_window)
        main_window.dat_browser = panel   # keep same attribute name

        gl         = getattr(main_window, 'gui_layout', None)
        left_stack = getattr(gl, 'left_stack', None)
        if left_stack is not None:
            old = left_stack.widget(1)
            if old is not None:
                left_stack.removeWidget(old)
            left_stack.insertWidget(1, panel)
        else:
            panel.setWindowTitle("GTA DAT Browser")
            panel.resize(900, 700)
            panel.show()

        panel.auto_fill_game_root()

        # Wire xref signal
        def _on_xref(xref):
            try:
                from PyQt6.QtCore import QTimer
                QTimer.singleShot(200, lambda: _apply_xref(xref, main_window))
            except Exception:
                pass

        def _apply_xref(xref, mw):
            try:
                from apps.methods.populate_img_table import apply_xref_tooltips, apply_xref_status
                tw = getattr(mw, 'main_tab_widget', None)
                if not tw:
                    return
                for i in range(tw.count()):
                    tab = tw.widget(i)
                    table = getattr(tab, 'table_ref', None)
                    if table and table.rowCount() > 0:
                        apply_xref_tooltips(table, xref)
                        apply_xref_status(table, xref)
                        for c in range(table.columnCount()):
                            h = table.horizontalHeaderItem(c)
                            if h and h.text() in ('IDE Model', 'IDE TXD'):
                                table.setColumnHidden(c, False)
                if hasattr(mw, 'log_message'):
                    mw.log_message("XRef applied from DAT Panel")
            except Exception as e:
                if hasattr(mw, 'log_message'):
                    mw.log_message(f"XRef apply error: {e}")

        panel.xref_ready.connect(_on_xref)

        # Register in taskbar
        try:
            tb = getattr(main_window, 'tool_taskbar', None)
            if tb and 'dat' not in tb._tools:
                from apps.methods.imgfactory_svg_icons import get_dat_browser_icon
                icon = get_dat_browser_icon(16, getattr(tb, '_txt', '#e0e0e0'))
                tb.register('dat', 'DAT', icon, panel, 'DAT Browser')
        except Exception:
            pass

        if hasattr(main_window, 'log_message'):
            main_window.log_message("DAT Panel integrated (v1)")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"DAT Panel integrate error: {e}")
        return False
