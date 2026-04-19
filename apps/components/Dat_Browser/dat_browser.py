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
    QMessageBox, QMenu, QApplication, QDialog,
    QRadioButton, QCheckBox, QGroupBox, QProgressDialog,
)
from PyQt6.QtCore import QSize, Qt, QThread, pyqtSignal, pyqtSlot
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

class TXDDumpDialog(QDialog): #vers 1
    """Dialog for selectively dumping TXD files from game IMG archives.\n
    Modes per game:
      All games  — Dump ALL   : every .txd in every IMG
      All games  — World only : excludes radar.*.txd, vehicles, peds txds
      GTA3/VC    — Radar      : radar.*.txd from gta3.img
      GTA3/VC    — Vehicles   : txds listed under [cars]/[peds] in default.ide
      SA         — Radar      : radar.*.txd from gta3.img
      SA         — Vehicles   : txds from vehicles.ide / peds.ide
      SA         — Generics   : txds from generic.ide / generics.ide (SOL)
      SOL        — Radar      : radartex.img entirely + radar.*.txd in gta3.img
      SOL        — Vehicles   : vehicles.img, peds.img + vehicles/peds in gta3.ide/vehicles.ide
      SOL        — Generics   : generics.ide txds
    """

    def __init__(self, loader, main_window=None, parent=None):
        super().__init__(parent)
        self.loader       = loader
        self.main_window  = main_window
        self.setWindowTitle("Dump TXD Files")
        self.setMinimumWidth(540)
        self.setMinimumHeight(420)
        self._build_ui()

    def _build_ui(self):
        from apps.methods.gta_dat_parser import GTAGame
        game  = self.loader.game
        lay   = QVBoxLayout(self)
        lay.setSpacing(8)

        # ── Game / mode info ─────────────────────────────────────────────
        game_names = {GTAGame.GTA3: "GTA III (LC)",
                      GTAGame.VC:   "Vice City (VC)",
                      GTAGame.SA:   "San Andreas (SA)",
                      GTAGame.SOL:  "GTA SOL (multi-city)"}
        info = QLabel(f"Game: <b>{game_names.get(game, game)}</b>  —  "
                      f"{len(self.loader.objects)} IDE objects loaded")
        info.setTextFormat(Qt.TextFormat.RichText)
        lay.addWidget(info)

        # ── Mode selection ────────────────────────────────────────────────
        grp = QGroupBox("What to dump")
        grp_lay = QVBoxLayout(grp)
        self._mode_btns = {}

        def _rb(key, label, tip, enabled=True):
            rb = QRadioButton(label)
            rb.setToolTip(tip)
            rb.setEnabled(enabled)
            self._mode_btns[key] = rb
            grp_lay.addWidget(rb)
            return rb

        _rb('all',
            "Dump ALL TXDs",
            "Every .txd found in every IMG/CDIMAGE archive — no filtering")

        _rb('world',
            "World textures only",
            "All TXDs except radar, vehicle and ped textures. Excludes radar.*.txd and txds from vehicles/peds IDE sections")

        _rb('radar',
            "Radar map TXDs",
            "GTA3/VC/SA: radar.*.txd from gta3.img. SOL: also radartex.img")

        _rb('vehicles',
            "Vehicle & Ped TXDs",
            "GTA3/VC: [cars]/[peds] from default.ide. SA: vehicles.ide+peds.ide. SOL: vehicles.img, peds.img")

        is_sa_sol = game in (GTAGame.SA, GTAGame.SOL)
        is_sol    = game == GTAGame.SOL
        _rb('generics',
            "Generic / prop TXDs  (SA / SOL only)",
            "SA: txds from generic.ide. SOL: txds from generics.ide",
            enabled=is_sa_sol)

        # Select first enabled as default
        for rb in self._mode_btns.values():
            if rb.isEnabled():
                rb.setChecked(True)
                break
        lay.addWidget(grp)

        # ── Options ───────────────────────────────────────────────────────
        opt_grp = QGroupBox("Options")
        opt_lay = QVBoxLayout(opt_grp)
        self._skip_existing = QCheckBox("Skip files that already exist in output folder")
        self._skip_existing.setChecked(True)
        opt_lay.addWidget(self._skip_existing)
        self._open_in_txd = QCheckBox("Open dumped TXDs in TXD Workshop when done")
        self._open_in_txd.setChecked(False)
        self._open_in_txd.setEnabled(
            self.main_window is not None and
            hasattr(self.main_window, 'open_txd_workshop_docked'))
        opt_lay.addWidget(self._open_in_txd)
        lay.addWidget(opt_grp)

        # ── Output folder ─────────────────────────────────────────────────
        folder_row = QHBoxLayout()
        folder_row.addWidget(QLabel("Output folder:"))
        self._folder_edit = QLineEdit()
        self._folder_edit.setPlaceholderText("Choose destination…")
        folder_row.addWidget(self._folder_edit, 1)
        browse_btn = QPushButton("Browse…")
        browse_btn.clicked.connect(self._pick_folder)
        folder_row.addWidget(browse_btn)
        lay.addLayout(folder_row)

        # ── Preview label ─────────────────────────────────────────────────
        self._preview_lbl = QLabel("Select a mode to see what will be dumped.")
        self._preview_lbl.setWordWrap(True)
        self._preview_lbl.setStyleSheet("font-style: italic; color: palette(mid);")
        lay.addWidget(self._preview_lbl)

        for rb in self._mode_btns.values():
            rb.toggled.connect(self._update_preview)
        self._update_preview()

        # ── Buttons ───────────────────────────────────────────────────────
        lay.addStretch()
        btn_row = QHBoxLayout()
        self._dump_btn = QPushButton("Dump TXDs")
        self._dump_btn.setDefault(True)
        self._dump_btn.clicked.connect(self._run_dump)
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        btn_row.addStretch()
        btn_row.addWidget(cancel_btn)
        btn_row.addWidget(self._dump_btn)
        lay.addLayout(btn_row)

    def _pick_folder(self):
        folder = QFileDialog.getExistingDirectory(self, "Output folder")
        if folder:
            self._folder_edit.setText(folder)

    def _get_mode(self):
        for key, rb in self._mode_btns.items():
            if rb.isChecked():
                return key
        return 'all'

    def _update_preview(self):
        mode = self._get_mode()
        txd_names = self._collect_txd_names(mode)
        n = len(txd_names)
        previews = sorted(list(txd_names))[:5]
        sample = ", ".join(previews) + ("…" if n > 5 else "")
        self._preview_lbl.setText(
            f"{n} TXD file(s) will be extracted.  Sample: {sample}" if n
            else "No TXDs match this filter with the current game data.")

    def _collect_txd_names(self, mode: str) -> set:
        """Return set of txd filenames (e.g. 'landstal.txd') for the chosen mode."""
        from apps.methods.gta_dat_parser import GTAGame
        game    = self.loader.game
        objects = self.loader.objects       # dict: model_id → IDEObject
        log     = self.loader.load_log      # list of (phase, type, abs_path, ok)

        # All TXD stems referenced by any loaded IDE object
        all_txd_stems = {obj.txd_name.lower()
                         for obj in objects.values()
                         if obj.txd_name and obj.txd_name.lower() not in ('null', '')}

        # ── Radar stems ────────────────────────────────────────────────────
        # radar.*.txd pattern — any txd name starting with "radar"
        radar_stems = {s for s in all_txd_stems if s.startswith('radar')}

        # ── Vehicle / ped IDE stems ────────────────────────────────────────
        vehicle_ped_ide_stems = set()
        veh_ped_sources = set()

        if game == GTAGame.SA:
            # SA: vehicles.ide and peds.ide are separate files
            veh_ped_sources = {'vehicles.ide', 'peds.ide'}
        elif game == GTAGame.SOL:
            # SOL: gta3.ide in models/, + vehicles.ide/peds.ide in SOL/ if present
            veh_ped_sources = {'gta3.ide', 'vehicles.ide', 'peds.ide'}
        else:
            # GTA3/VC: [cars] and [peds] sections in default.ide
            veh_ped_sources = {'default.ide'}

        for obj in objects.values():
            src_base = os.path.basename(obj.source_ide or '').lower()
            in_veh_source = src_base in veh_ped_sources
            in_veh_section = obj.section in ('cars', 'peds', 'weap')
            # For GTA3/VC default.ide only include cars/peds sections
            if game in (GTAGame.GTA3, GTAGame.VC):
                if in_veh_source and in_veh_section:
                    vehicle_ped_ide_stems.add(obj.txd_name.lower())
            elif in_veh_source or in_veh_section:
                vehicle_ped_ide_stems.add(obj.txd_name.lower())

        # ── Generic stems (SA/SOL) ─────────────────────────────────────────
        generic_sources = {'generic.ide', 'generics.ide'}
        generic_stems = set()
        for obj in objects.values():
            src_base = os.path.basename(obj.source_ide or '').lower()
            if src_base in generic_sources:
                generic_stems.add(obj.txd_name.lower())

        # ── Build TXD file name set based on mode ─────────────────────────
        if mode == 'all':
            # Every TXD stem referenced by any IDE object → add .txd
            return {s + '.txd' for s in all_txd_stems}

        elif mode == 'world':
            exclude = radar_stems | vehicle_ped_ide_stems
            return {s + '.txd' for s in all_txd_stems - exclude}

        elif mode == 'radar':
            # Also include radartex.img for SOL (handled in _run_dump by IMG name)
            return {s + '.txd' for s in radar_stems}

        elif mode == 'vehicles':
            return {s + '.txd' for s in vehicle_ped_ide_stems}

        elif mode == 'generics':
            return {s + '.txd' for s in generic_stems}

        return set()

    def _get_img_paths(self, mode: str) -> list:
        """Return ordered list of IMG/CDIMAGE archive paths for this mode."""
        from apps.methods.gta_dat_parser import GTAGame
        game = self.loader.game

        # All available IMGs from load log
        all_imgs = []
        for _phase, etype, path, ok in self.loader.load_log:
            if ok and etype in ('IMG', 'CDIMAGE') and os.path.isfile(path):
                if path not in all_imgs:
                    all_imgs.append(path)

        if mode == 'all':
            return all_imgs

        # For targeted modes — decide which IMGs to scan
        img_stems = {os.path.splitext(os.path.basename(p))[0].lower(): p
                     for p in all_imgs}

        if mode == 'radar':
            # Always gta3.img; SOL also radartex.img
            result = []
            for stem in ('gta3', 'radartex'):
                if stem in img_stems:
                    result.append(img_stems[stem])
            return result or all_imgs  # fallback to all if not found

        elif mode == 'vehicles':
            # SOL: vehicles.img, peds.img first; all others as fallback
            if game == 'sol':
                priority = []
                for stem in ('vehicles', 'peds', 'gta3'):
                    if stem in img_stems:
                        priority.append(img_stems[stem])
                return priority or all_imgs
            else:
                return [img_stems.get('gta3', all_imgs[0])] if all_imgs else []

        # world, generics — scan all IMGs (TXD filter handles it)
        return all_imgs

    def _run_dump(self):
        """Execute the TXD dump with progress dialog."""
        out_dir = self._folder_edit.text().strip()
        if not out_dir:
            self._pick_folder()
            out_dir = self._folder_edit.text().strip()
        if not out_dir:
            return

        os.makedirs(out_dir, exist_ok=True)
        mode          = self._get_mode()
        skip_existing = self._skip_existing.isChecked()
        txd_names     = self._collect_txd_names(mode)   # set of "foo.txd"
        img_paths     = self._get_img_paths(mode)

        if not txd_names:
            QMessageBox.warning(self, "Nothing to Dump",
                "No TXD names match the selected filter for this game.")
            return
        if not img_paths:
            QMessageBox.warning(self, "No IMGs",
                "No IMG/CDIMAGE archives found for this game.")
            return

        prog = QProgressDialog("Starting…", "Cancel", 0, len(img_paths), self)
        prog.setWindowTitle(f"Dumping TXDs — {mode}")
        prog.setWindowModality(Qt.WindowModality.ApplicationModal)
        prog.show()

        extracted, skipped, errors = 0, 0, []
        remaining = set(txd_names)   # shrinks as we find each TXD

        try:
            from apps.methods.img_tools import IMGArchive
        except ImportError as e:
            prog.close()
            QMessageBox.critical(self, "Error", f"Cannot import IMGArchive:\n{e}")
            return

        for i, img_path in enumerate(img_paths):
            prog.setValue(i)
            prog.setLabelText(
                f"Scanning {os.path.basename(img_path)}…  "
                f"({extracted} extracted, {len(remaining)} remaining)")
            QApplication.processEvents()
            if prog.wasCanceled():
                break

            try:
                arc = IMGArchive(img_path)
                for entry in arc.entries:
                    ename = entry.name.lower()
                    if not ename.endswith('.txd'):
                        continue
                    # Mode 'all' skips the name filter
                    if mode != 'all' and ename not in txd_names:
                        continue
                    out_path = os.path.join(out_dir, entry.name)
                    if skip_existing and os.path.exists(out_path):
                        skipped += 1
                        remaining.discard(ename)
                        continue
                    try:
                        data = arc.read_entry(entry)
                        with open(out_path, 'wb') as f:
                            f.write(data)
                        extracted += 1
                        remaining.discard(ename)
                    except Exception as e:
                        errors.append(f"{entry.name}: {e}")
            except Exception as e:
                errors.append(f"{os.path.basename(img_path)}: {e}")

        prog.setValue(len(img_paths))
        prog.close()

        # Summary
        lines = [
            f"Mode: {mode}",
            f"Extracted: {extracted} TXD file(s)",
            f"Skipped (already existed): {skipped}",
            f"Not found in any IMG: {len(remaining)}",
            f"Output: {out_dir}",
        ]
        if remaining and len(remaining) <= 20:
            lines.append(f"Missing: {', '.join(sorted(remaining))}")
        if errors:
            lines.append(f"Errors ({len(errors)}): " + "; ".join(errors[:5]))

        QMessageBox.information(self, "TXD Dump Complete", "\n".join(lines))

        # Optionally open in TXD Workshop
        if self._open_in_txd.isChecked() and extracted > 0:
            mw = self.main_window
            if mw and hasattr(mw, 'open_txd_workshop_docked'):
                # Open the first extracted TXD
                first = sorted(os.listdir(out_dir))[0]
                mw.open_txd_workshop_docked(
                    file_path=os.path.join(out_dir, first))

        self.accept()


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
        # Let Qt palette drive the background — theme-aware
        self._apply_theme_stylesheet()
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

        # Wire theme changes
        mw = self.main_window
        if mw and hasattr(mw, 'app_settings') and hasattr(mw.app_settings, 'theme_changed'):
            mw.app_settings.theme_changed.connect(self._on_theme_changed)

        # Track current compact state; icons loaded lazily on first compact switch
        self._toolbar_compact = False

        toolbar.addWidget(QLabel("Game:"))
        toolbar.addWidget(self._game_combo)
        toolbar.addWidget(self._path_edit, 1)
        toolbar.addWidget(browse_btn)
        toolbar.addWidget(self._load_btn)

        self._dump_txd_btn = QPushButton("Dump TXDs")
        self._dump_txd_btn.setToolTip("Extract all TXD files from game IMG archives to a folder")
        self._dump_txd_btn.setEnabled(False)
        self._dump_txd_btn.clicked.connect(self._dump_all_game_txds)
        toolbar.addWidget(self._dump_txd_btn)

        # Split/full panel toggle — mirrors gui_layout.split_toggle_btn exactly
        self._split_btn = QPushButton()
        self._split_btn.setFixedSize(24, 24)
        self._split_btn.setIconSize(QSize(20, 20))
        self._split_btn.setToolTip("Panel left | Files right → click to cycle layout")
        self._split_btn.clicked.connect(self._on_split_toggle)
        self._sync_split_icon()   # set initial icon
        toolbar.addWidget(self._split_btn)

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
        self._tree.setAutoFillBackground(True)
        self._tree.viewport().setAutoFillBackground(True)
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

    def _make_table(self, headers): #vers 4
        from apps.methods.populate_img_table import DragSelectTableWidget
        t = DragSelectTableWidget()
        t.setColumnCount(len(headers))
        t.setHorizontalHeaderLabels(headers)
        t.setAlternatingRowColors(True)
        t.setSortingEnabled(True)
        t.setAutoFillBackground(True)
        t.horizontalHeader().setStretchLastSection(True)
        t.horizontalHeader().setSectionsMovable(False)
        t.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        t.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        t.customContextMenuRequested.connect(
            lambda pos, tbl=t: self._table_context_menu(tbl, pos))
        # Opaque viewport prevents bleed-through
        t.viewport().setAutoFillBackground(True)
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
        """React immediately when the game combo selection changes.\n
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

    def _sync_split_icon(self): #vers 1
        """Copy icon and tooltip from gui_layout.split_toggle_btn."""
        try:
            from apps.methods.imgfactory_svg_icons import get_layout_w1left_icon
            gl = getattr(self._main_window, 'gui_layout', None)
            src = getattr(gl, 'split_toggle_btn', None)
            if src:
                self._split_btn.setIcon(src.icon())
                self._split_btn.setToolTip(src.toolTip())
            else:
                # gui_layout not ready yet — use default icon
                self._split_btn.setIcon(get_layout_w1left_icon(20))
        except Exception:
            pass

    def _on_split_toggle(self): #vers 3
        """Cycle panel layout — calls gui_layout._toggle_merge_view_layout then syncs icon."""
        try:
            gl = getattr(self._main_window, 'gui_layout', None)
            if gl and hasattr(gl, '_toggle_merge_view_layout'):
                gl._toggle_merge_view_layout()
            self._sync_split_icon()
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
        if hasattr(self, '_dump_txd_btn'):
            self._dump_txd_btn.setEnabled(bool(self.loader and self.loader.objects))
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
            def_item.setForeground(0, self.palette().color(self.foregroundRole()).darker(150))
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
                    child.setForeground(col, QColor(204, 68, 68))  # error red — intentionally fixed
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

    def _table_context_menu(self, table, pos): #vers 2
        index = table.indexAt(pos)
        if not index.isValid():
            return
        row  = index.row()
        menu = QMenu(self)

        copy_name = menu.addAction("Copy model name")
        copy_row  = menu.addAction("Copy row as text")
        menu.addSeparator()
        find_inst = menu.addAction("Find all instances of this model")

        # TXD actions — only for Objects (IDE) table which has TXD column
        open_txd_act = None
        sel_txd_act  = None
        dump_sel_act = None
        if table is self._obj_table:
            txd_item = table.item(row, 2)
            txd_name = txd_item.text().strip() if txd_item else ''
            if txd_name and txd_name not in ('—', ''):
                menu.addSeparator()
                open_txd_act = menu.addAction(f"Open {txd_name}.txd in TXD Workshop")
            menu.addSeparator()
            sel_rows = list({i.row() for i in table.selectedItems()})
            if len(sel_rows) > 1:
                dump_sel_act = menu.addAction(
                    f"Extract TXDs for {len(sel_rows)} selected rows…")
            dump_all_act = menu.addAction("Dump ALL game TXDs to folder…")
        else:
            dump_all_act = None

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
        elif open_txd_act and chosen == open_txd_act:
            self._open_txd_from_row(table, row)
        elif dump_sel_act and chosen == dump_sel_act:
            self._dump_selected_txds(table)
        elif dump_all_act and chosen == dump_all_act:
            self._dump_all_game_txds()

    # ── Tree right-click — open source file in editor ─────────────────────

    def _apply_theme_stylesheet(self): #vers 2
        """Apply stylesheet using theme colors from app_settings when available,
        falling back to Qt palette roles otherwise."""
        mw = getattr(self, 'main_window', None)
        colors = {}
        if mw and hasattr(mw, 'app_settings'):
            try:
                colors = mw.app_settings.get_theme_colors() or {}
            except Exception:
                pass

        # Pull theme-specific colors with palette fallbacks
        bg       = colors.get('panel_bg',       '')
        row_odd  = colors.get('table_row_odd',  colors.get('bg_primary',    ''))
        row_even = colors.get('table_row_even', colors.get('alternate_row', ''))
        fg       = colors.get('text_primary',   '')
        border   = colors.get('border',         '')
        btn_bg   = colors.get('button_normal',  '')
        btn_fg   = colors.get('button_text_color', '')
        sel_bg   = colors.get('accent_primary', '')
        sel_fg   = colors.get('selection_text', '')
        panel_entries = colors.get('panel_entries', row_odd)

        # Helper — return css value or palette fallback
        def c(val, fallback):
            return val if val else f'palette({fallback})'

        ss = f"""
            DATBrowserWidget {{
                background-color: {c(bg, 'window')};
                color: {c(fg, 'windowText')};
            }}
            QTreeWidget, QTableWidget, QListWidget {{
                background-color: {c(row_odd, 'base')};
                alternate-background-color: {c(row_even, 'alternateBase')};
                color: {c(fg, 'text')};
                border: 1px solid {c(border, 'mid')};
                gridline-color: {c(border, 'mid')};
            }}
            QTreeWidget::item {{
                background-color: {c(panel_entries, 'base')};
                color: {c(fg, 'text')};
                padding: 1px 3px;
            }}
            QTreeWidget::item:alternate {{
                background-color: {c(row_even, 'alternateBase')};
            }}
            QTableWidget::item {{
                color: {c(fg, 'text')};
                padding: 1px;
            }}
            QTreeWidget::item:selected, QTableWidget::item:selected {{
                background-color: {c(sel_bg, 'highlight')};
                color: {c(sel_fg, 'highlightedText')};
            }}
            QTreeWidget::item:hover, QTableWidget::item:hover {{
                background-color: {c(row_even, 'midlight')};
            }}
            QHeaderView::section {{
                background-color: {c(btn_bg, 'button')};
                color: {c(btn_fg, 'buttonText')};
                border: none;
                border-right: 1px solid {c(border, 'mid')};
                border-bottom: 1px solid {c(border, 'mid')};
                padding: 3px 6px;
                font-weight: bold;
            }}
            QTabWidget::pane {{
                background-color: {c(bg, 'window')};
                border: 1px solid {c(border, 'mid')};
            }}
            QTabBar::tab {{
                background-color: {c(btn_bg, 'button')};
                color: {c(btn_fg, 'buttonText')};
                border: 1px solid {c(border, 'mid')};
                padding: 4px 12px;
                margin-right: 2px;
            }}
            QTabBar::tab:selected {{
                background-color: {c(bg, 'window')};
                color: {c(fg, 'windowText')};
                border-bottom: none;
            }}
            QTabBar::tab:hover {{
                background-color: {c(row_even, 'midlight')};
            }}
            QSplitter::handle {{
                background-color: {c(border, 'mid')};
            }}
            QLineEdit, QComboBox {{
                background-color: {c(row_odd, 'base')};
                color: {c(fg, 'text')};
                border: 1px solid {c(border, 'mid')};
                padding: 2px 4px;
            }}
            QProgressBar {{
                background-color: {c(row_odd, 'base')};
                color: {c(fg, 'text')};
                border: 1px solid {c(border, 'mid')};
            }}
            QScrollBar:vertical, QScrollBar:horizontal {{
                background-color: {c(row_even, 'base')};
            }}
            QLabel {{
                color: {c(fg, 'windowText')};
                background-color: transparent;
            }}
        """
        self.setStyleSheet(ss)

    def _on_theme_changed(self): #vers 3
        """Refresh all colors when theme switches."""
        self._apply_theme_stylesheet()   # DAT-specific row colors
        try:
            from apps.methods.workshop_theme import apply_workshop_theme
            mw = getattr(self, 'main_window', None)
            apply_workshop_theme(self, mw)
        except Exception:
            pass
        self.update()
        self.repaint()

    def _get_txd_names_from_img(self, img_path: str) -> list:
        """Return list of .txd entry names from an IMG archive."""
        try:
            from apps.methods.img_tools import IMGArchive
            img = IMGArchive(img_path)
            return [e.name for e in img.entries if e.name.lower().endswith('.txd')]
        except Exception as e:
            print(f"IMG read error: {e}")
            return []

    def _dump_all_game_txds(self): #vers 2
        """Open the TXD Dump dialog."""
        if not self.loader or not self.loader.objects:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "DAT Browser",
                "Load a game first before dumping TXDs.")
            return
        dlg = TXDDumpDialog(self.loader, self.main_window, parent=self)
        dlg.exec()

    def _open_txd_from_row(self, table, row): #vers 1
        """Open the TXD linked to the selected IDE row in TXD Workshop."""
        txd_item  = table.item(row, 2)   # TXD column
        if not txd_item:
            return
        txd_name = txd_item.text().strip()
        if not txd_name or txd_name in ('—', ''):
            return
        mw = self.main_window
        if not mw:
            return
        txd_file = txd_name.lower()
        if not txd_file.endswith('.txd'):
            txd_file += '.txd'

        # Look in current_img first
        img = getattr(mw, 'current_img', None)
        if img and hasattr(mw, 'open_txd_workshop_docked'):
            for entry in getattr(img, 'entries', []):
                if entry.name.lower() == txd_file:
                    mw.open_txd_workshop_docked(txd_name=txd_file)
                    return

        # Search across all game IMGs via load_log
        if self.loader:
            try:
                from apps.methods.img_tools import IMGArchive
                for _phase, etype, path, ok in self.loader.load_log:
                    if not (ok and etype in ('IMG', 'CDIMAGE') and os.path.isfile(path)):
                        continue
                    try:
                        arc = IMGArchive(path)
                        for entry in arc.entries:
                            if entry.name.lower() == txd_file:
                                if mw and hasattr(mw, 'open_txd_workshop_docked'):
                                    mw.open_txd_workshop_docked(file_path=path)
                                    # TODO: auto-select the txd entry once workshop opens
                                    return
                    except Exception:
                        continue
            except ImportError:
                pass

        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(self, "TXD Not Found",
            f"Could not find {txd_file} in any loaded IMG archive.")

    def _dump_selected_txds(self, table): #vers 1
        """Extract TXDs for selected rows' TXD names to a folder."""
        from PyQt6.QtWidgets import QFileDialog, QMessageBox, QApplication
        sel_rows = sorted({i.row() for i in table.selectedItems()})
        txd_names = set()
        for row in sel_rows:
            it = table.item(row, 2)
            if it and it.text().strip() not in ('', '—'):
                name = it.text().strip().lower()
                if not name.endswith('.txd'):
                    name += '.txd'
                txd_names.add(name)
        if not txd_names:
            QMessageBox.information(self, "No TXDs", "No TXD names found in selection.")
            return
        out_dir = QFileDialog.getExistingDirectory(
            self, f"Extract {len(txd_names)} TXD(s) to folder")
        if not out_dir:
            return
        extracted, skipped, errors = 0, 0, []
        try:
            from apps.methods.img_tools import IMGArchive
            for _phase, etype, path, ok in self.loader.load_log:
                if not (ok and etype in ('IMG', 'CDIMAGE') and os.path.isfile(path)):
                    continue
                if not txd_names:
                    break
                try:
                    arc = IMGArchive(path)
                    for entry in arc.entries:
                        if entry.name.lower() in txd_names:
                            out_path = os.path.join(out_dir, entry.name)
                            if os.path.exists(out_path):
                                skipped += 1
                            else:
                                try:
                                    with open(out_path, 'wb') as f:
                                        f.write(arc.read_entry(entry))
                                    extracted += 1
                                except Exception as e:
                                    errors.append(f"{entry.name}: {e}")
                            txd_names.discard(entry.name.lower())
                except Exception as e:
                    errors.append(str(e))
        except ImportError as e:
            QMessageBox.critical(self, "Error", str(e))
            return
        msg = f"Extracted {extracted} TXD(s) to {out_dir}"
        if skipped: msg += f"\nSkipped (exist): {skipped}"
        if txd_names: msg += f"\nNot found: {', '.join(sorted(txd_names))}"
        if errors: msg += f"\nErrors: {len(errors)}"
        QMessageBox.information(self, "TXD Extract", msg)

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
    """Show the DAT Browser tab.\n
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
            widget._apply_theme_stylesheet()
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
    """Silently pre-fill the DAT Browser path field from the directory tree.\n
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
            icon_color = getattr(tb, '_txt', None) or self.palette().color(self.foregroundRole()).name()
            icon = get_dat_browser_icon(16, icon_color)
            tb.register('dat', 'DAT', icon, widget, 'DAT Browser')
        else:
            tb._tools['dat']['target'] = widget
        tb._set_exclusive_active('dat')
    except Exception:
        pass


def integrate_dat_browser(main_window) -> bool: #vers 5
    """Create DAT Browser widget and place it in the left_stack panel.\nUse show_dat_browser() / _show_dat_browser() to open/focus it.
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
        # Register taskbar button but don't activate — panel is hidden until user clicks DAT
        try:
            tb = getattr(main_window, 'tool_taskbar', None)
            if tb and 'dat' not in tb._tools:
                from apps.methods.imgfactory_svg_icons import get_dat_browser_icon
                icon = get_dat_browser_icon(16, getattr(tb, '_txt', None) or self.palette().color(self.foregroundRole()).name())
                tb.register('dat', 'DAT', icon, widget, 'DAT Browser')
        except Exception:
            pass

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
