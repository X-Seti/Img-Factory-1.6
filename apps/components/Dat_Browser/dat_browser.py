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
        self.setMinimumWidth(680)
        self.setMinimumHeight(420)
        self._build_ui()

    # ── Category row specs ────────────────────────────────────────────────────
    _CAT_SPECS = [
        ('all',      'Dump All',           'Every .txd in every IMG — no filtering'),
        ('world',    'World Textures',      'All TXDs except radar / vehicle / ped'),
        ('radar',    'Radar Map TXDs',      'radar.*.txd (SOL: also radartex.img)'),
        ('vehicles', 'Vehicles',            'Car TXDs from vehicles.ide / [cars] section'),
        ('peds',     'Peds',                'Ped TXDs from peds.ide / [peds] section'),
        ('generics', 'Generics (SA/SOL)',   'Prop TXDs from generic.ide/generics.ide'),
    ]

    def _build_ui(self): #vers 2
        from apps.methods.gta_dat_parser import GTAGame
        import json
        game = self.loader.game
        is_sa_sol = game in (GTAGame.SA, GTAGame.SOL)

        lay = QVBoxLayout(self)
        lay.setSpacing(6)

        # ── Game info ─────────────────────────────────────────────────────
        game_names = {GTAGame.GTA3:"GTA III (LC)", GTAGame.VC:"Vice City (VC)",
                      GTAGame.SA:"San Andreas (SA)", GTAGame.SOL:"GTA SOL (multi-city)"}
        info = QLabel(f"Game: <b>{game_names.get(game,str(game))}</b>"
                      f"  —  {len(self.loader.objects)} IDE objects loaded")
        info.setTextFormat(Qt.TextFormat.RichText)
        lay.addWidget(info)

        # ── Load saved paths ──────────────────────────────────────────────
        saved = self._load_saved_paths()
        _saved_flat = saved.get('_struct_flat', False)

        # ── Per-category rows ─────────────────────────────────────────────
        self._cat_rows = {}   # key → {'txd_rb','tex_rb','folder_edit','enabled'}

        cat_box = QGroupBox("What to dump")
        cat_lay = QVBoxLayout(cat_box)
        cat_lay.setSpacing(4)

        # Header
        hdr = QHBoxLayout()
        hdr.addWidget(QLabel("Category"), 3)
        hdr.addWidget(QLabel("TXD"), 1)
        hdr.addWidget(QLabel("Textures"), 1)
        hdr.addWidget(QLabel("Output folder"), 4)
        cat_lay.addLayout(hdr)

        from PyQt6.QtWidgets import QButtonGroup
        for key, label, tip in self._CAT_SPECS:
            enabled = True if key != 'generics' else is_sa_sol

            row_w = QWidget(); row_l = QHBoxLayout(row_w)
            row_l.setContentsMargins(0,0,0,0); row_l.setSpacing(6)

            # Enable checkbox — controls whether this row is included in dump
            enable_cb = QCheckBox()
            enable_cb.setChecked(enabled)  # generics off by default if not SA/SOL
            enable_cb.setFixedWidth(22)
            enable_cb.setToolTip(f"Include {label} in dump")
            row_l.addWidget(enable_cb)

            # Label
            lbl = QLabel(label)
            lbl.setFixedWidth(140)
            lbl.setToolTip(tip)
            row_l.addWidget(lbl)

            # TXD radio
            txd_rb = QRadioButton("TXD")
            txd_rb.setChecked(True)
            txd_rb.setFixedWidth(50)

            # Textures radio (mutually exclusive per row)
            tex_rb = QRadioButton("Textures")
            tex_rb.setFixedWidth(75)

            bg = QButtonGroup(row_w)
            bg.addButton(txd_rb); bg.addButton(tex_rb)
            row_l.addWidget(txd_rb)
            row_l.addWidget(tex_rb)

            # Folder line edit
            folder_edit = QLineEdit()
            folder_edit.setPlaceholderText("(same as default output folder)")
            folder_edit.setFixedHeight(24)
            if key in saved:
                folder_edit.setText(saved[key].get('folder',''))
                if saved[key].get('mode','txd') == 'textures':
                    tex_rb.setChecked(True)
                if 'active' in saved[key]:
                    enable_cb.setChecked(saved[key]['active'])

            browse = QPushButton("…")
            browse.setFixedWidth(28); browse.setFixedHeight(24)
            browse.setToolTip(f"Choose output folder for {label}")
            browse.clicked.connect(
                lambda _=False, fe=folder_edit: fe.setText(
                    QFileDialog.getExistingDirectory(self, "Output folder") or fe.text()))

            row_l.addWidget(folder_edit, 1)
            row_l.addWidget(browse)
            cat_lay.addWidget(row_w)

            # Wire enable checkbox to grey out / ungrey the row widgets
            _row_widgets = [lbl, txd_rb, tex_rb, folder_edit, browse]
            def _set_row_enabled(checked, widgets=_row_widgets):
                for w in widgets:
                    w.setEnabled(checked)
            enable_cb.toggled.connect(_set_row_enabled)
            _set_row_enabled(enable_cb.isChecked())  # apply initial state

            self._cat_rows[key] = {
                'enable_cb': enable_cb,
                'txd_rb': txd_rb, 'tex_rb': tex_rb,
                'folder_edit': folder_edit,
                'enabled': enabled,  # base availability (e.g. generics=SA/SOL only)
                'bg': bg
            }

        lay.addWidget(cat_box)

        # ── Export formats ────────────────────────────────────────────────
        fmt_grp = QGroupBox("Export format(s)  — used when Textures is selected")
        fmt_lay = QHBoxLayout(fmt_grp)
        self._fmt_checks = {}
        for fmt, default in [('IFF/ILBM',True),('PNG',True),
                              ('TGA',False),('DDS',False),('BMP',False)]:
            cb = QCheckBox(fmt); cb.setChecked(default)
            fmt_lay.addWidget(cb)
            self._fmt_checks[fmt] = cb
        lay.addWidget(fmt_grp)

        # ── Options ───────────────────────────────────────────────────────
        opt_grp = QGroupBox("Options")
        opt_lay = QVBoxLayout(opt_grp)

        # ── Texture output structure ──────────────────────────────────────
        struct_row = QHBoxLayout()
        struct_row.addWidget(QLabel("Texture folder structure:"))
        from PyQt6.QtWidgets import QButtonGroup as _BG
        self._struct_named = QRadioButton("texlist/<txd_name>/file.iff")
        self._struct_named.setChecked(True)
        self._struct_named.setToolTip("Each TXD textures go into a subfolder: texlist/landstal/chassis.iff  Best for many TXDs.")
        self._struct_flat = QRadioButton("texlist/file.iff  (flat)")
        self._struct_flat.setToolTip("Flat: texlist/chassis.iff  Convenient for DPaint/PPaint. Warning: name collisions possible.")
        _bg_struct = _BG(opt_grp)
        _bg_struct.addButton(self._struct_named)
        _bg_struct.addButton(self._struct_flat)
        struct_row.addWidget(self._struct_named)
        struct_row.addWidget(self._struct_flat)
        struct_row.addStretch()
        opt_lay.addLayout(struct_row)

        self._skip_existing = QCheckBox("Skip files that already exist in output folder")
        self._skip_existing.setChecked(True)
        opt_lay.addWidget(self._skip_existing)
        self._open_in_txd = QCheckBox("Open first TXD in TXD Workshop when done")
        self._open_in_txd.setChecked(False)
        self._open_in_txd.setEnabled(
            self.main_window is not None and
            hasattr(self.main_window,'open_txd_workshop_docked'))
        opt_lay.addWidget(self._open_in_txd)
        lay.addWidget(opt_grp)

        # Restore saved structure choice
        if _saved_flat:
            self._struct_flat.setChecked(True)

        # ── Default output folder ─────────────────────────────────────────
        folder_row = QHBoxLayout()
        folder_row.addWidget(QLabel("Default output folder:"))
        self._folder_edit = QLineEdit()
        self._folder_edit.setPlaceholderText("Choose destination…")
        self._folder_edit.setText(saved.get('_default',''))
        folder_row.addWidget(self._folder_edit, 1)
        browse_default = QPushButton("Browse…")
        browse_default.clicked.connect(self._pick_folder)
        folder_row.addWidget(browse_default)
        lay.addLayout(folder_row)

        # ── Preview ───────────────────────────────────────────────────────
        self._preview_lbl = QLabel("")
        self._preview_lbl.setWordWrap(True)
        self._preview_lbl.setStyleSheet("font-style:italic; color:palette(mid);")
        lay.addWidget(self._preview_lbl)
        self._update_preview()

        # ── Button row ────────────────────────────────────────────────────
        lay.addStretch()
        btn_row = QHBoxLayout()

        save_paths_btn = QPushButton("Save Paths")
        save_paths_btn.setToolTip("Save all folder paths for next session")
        save_paths_btn.clicked.connect(self._save_paths)

        settings_btn = QPushButton()
        settings_btn.setFixedSize(28,28)
        settings_btn.setToolTip("Dump settings")
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            settings_btn.setIcon(SVGIconFactory.settings_icon(16))
        except Exception:
            settings_btn.setText("⚙")
        settings_btn.clicked.connect(self._open_settings)

        cancel_btn  = QPushButton("Cancel")
        self._dump_btn = QPushButton("Dump")
        self._dump_btn.setDefault(True)

        cancel_btn.clicked.connect(self.reject)
        self._dump_btn.clicked.connect(self._run_dump)

        btn_row.addWidget(save_paths_btn)
        btn_row.addWidget(settings_btn)
        btn_row.addStretch()
        btn_row.addWidget(cancel_btn)
        btn_row.addWidget(self._dump_btn)
        lay.addLayout(btn_row)

    def _pick_folder(self):
        folder = QFileDialog.getExistingDirectory(self, "Default output folder")
        if folder:
            self._folder_edit.setText(folder)

    def _get_mode(self):
        """Return first enabled+active row key, or 'all' fallback."""
        # In new UI, 'mode' is derived from which rows have Textures/TXD set
        # For _collect_txd_names compatibility, return first checked-like key
        # We iterate and dump all enabled rows, so this is a no-op compatibility shim
        return 'all'

    def _update_preview(self):
        lines = []
        for key, label, _ in self._CAT_SPECS:
            row = self._cat_rows.get(key,{})
            cb = row.get('enable_cb')
            if not cb or not cb.isChecked():
                continue
            mode_str = 'Textures' if row.get('tex_rb') and row['tex_rb'].isChecked() else 'TXD'
            n = len(self._collect_txd_names(key))
            lines.append(f"{label}: {n} TXD(s) — {mode_str}")
        self._preview_lbl.setText("  |  ".join(lines) if lines else "")

    def _cfg_path(self):
        import os
        return os.path.expanduser("~/.config/imgfactory/txd_dump_paths.json")

    def _load_saved_paths(self) -> dict:
        import json, os
        p = self._cfg_path()
        if os.path.isfile(p):
            try:
                return json.load(open(p))
            except Exception:
                pass
        return {}

    def _save_paths(self):
        import json, os
        data = {
            '_default': self._folder_edit.text().strip(),
            '_struct_flat': (getattr(self,'_struct_flat',None)
                             and self._struct_flat.isChecked()),
        }
        for key, row in self._cat_rows.items():
            data[key] = {
                'folder': row['folder_edit'].text().strip(),
                'mode': 'textures' if row['tex_rb'].isChecked() else 'txd',
                'active': row['enable_cb'].isChecked() if 'enable_cb' in row else True,
            }
        os.makedirs(os.path.dirname(self._cfg_path()), exist_ok=True)
        json.dump(data, open(self._cfg_path(),'w'), indent=2)
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(self, "Saved", "Folder paths saved.")

    def _open_settings(self):
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QDialogButtonBox
        dlg = QDialog(self); dlg.setWindowTitle("Dump Settings")
        lay = QVBoxLayout(dlg)
        lay.addWidget(QLabel(
            "Saved paths: " + self._cfg_path() + "\n\n"
            "Paths are loaded automatically on next open."))
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok)
        bb.accepted.connect(dlg.accept); lay.addWidget(bb)
        dlg.exec()

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

        # ── Vehicle stems (cars/weap) ─────────────────────────────────────
        vehicle_stems = set()
        ped_stems     = set()

        if game == GTAGame.SA:
            veh_sources = {'vehicles.ide'}
            ped_sources = {'peds.ide'}
        elif game == GTAGame.SOL:
            veh_sources = {'gta3.ide', 'vehicles.ide'}
            ped_sources = {'peds.ide'}
        else:
            # GTA3/VC: [cars]/[weap] vs [peds] sections in default.ide
            veh_sources = {'default.ide'}
            ped_sources = {'default.ide'}

        for obj in objects.values():
            src_base   = os.path.basename(obj.source_ide or '').lower()
            txd_lower  = obj.txd_name.lower() if obj.txd_name else ''
            if not txd_lower or txd_lower in ('null',''):
                continue
            in_veh_src = src_base in veh_sources
            in_ped_src = src_base in ped_sources
            section    = obj.section or ''

            if game in (GTAGame.GTA3, GTAGame.VC):
                if in_veh_src and section in ('cars','weap'):
                    vehicle_stems.add(txd_lower)
                elif in_ped_src and section == 'peds':
                    ped_stems.add(txd_lower)
            else:
                if in_veh_src or section in ('cars','weap'):
                    vehicle_stems.add(txd_lower)
                if in_ped_src or section == 'peds':
                    ped_stems.add(txd_lower)

        # Combined for world exclusion
        vehicle_ped_ide_stems = vehicle_stems | ped_stems

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
            return {s + '.txd' for s in vehicle_stems}

        elif mode == 'peds':
            return {s + '.txd' for s in ped_stems}

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
            from apps.methods.gta_dat_parser import GTAGame as _G
            if game == _G.SOL:
                return [img_stems[s] for s in ('vehicles','gta3') if s in img_stems] or all_imgs
            return [img_stems[s] for s in ('gta3',) if s in img_stems] or all_imgs

        elif mode == 'peds':
            from apps.methods.gta_dat_parser import GTAGame as _G
            if game == _G.SOL:
                return [img_stems[s] for s in ('peds','gta3') if s in img_stems] or all_imgs
            return [img_stems[s] for s in ('gta3',) if s in img_stems] or all_imgs

        # world, generics — scan all IMGs (TXD filter handles it)
        return all_imgs


    def _run_dump(self): #vers 3
        """Execute dump for all enabled categories.
        Each category uses its own output folder (falls back to default),
        and its own TXD/Textures mode."""
        default_dir = self._folder_edit.text().strip()

        # Build job list: (key, out_dir, mode='txd'|'textures')
        jobs = []
        for key, label, _ in self._CAT_SPECS:
            row = self._cat_rows.get(key, {})
            # Skip rows that are either base-disabled or unchecked by user
            if not row.get('enable_cb', None) or not row['enable_cb'].isChecked():
                continue
            cat_dir = row['folder_edit'].text().strip() or default_dir
            if not cat_dir:
                continue  # skip if no folder set
            mode_str = 'textures' if row['tex_rb'].isChecked() else 'txd'
            jobs.append((key, label, cat_dir, mode_str))

        if not jobs:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "No Output Folders",
                "Set a default output folder or per-category folders before dumping.")
            return

        # Collect format selections (for texture export)
        fmt_map  = {'IFF/ILBM':'iff','PNG':'png','TGA':'tga','DDS':'dds','BMP':'bmp'}
        sel_fmts = [fmt_map[k] for k,cb in self._fmt_checks.items() if cb.isChecked()]
        skip_existing = self._skip_existing.isChecked()

        # Check at least one texture format is selected if any job is 'textures'
        if any(m == 'textures' for _,_,_,m in jobs) and not sel_fmts:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "No Format Selected",
                "Tick at least one export format (IFF/ILBM, PNG…) for texture jobs.")
            return

        # Lazy TXD decode pipeline
        _txw = [None]
        def _get_txw():
            if _txw[0] is None:
                try:
                    from apps.components.Txd_Editor.txd_workshop import TXDWorkshop
                    _txw[0] = TXDWorkshop(main_window=None)
                except Exception: pass
            return _txw[0]

        def _parse_txd_data(data):
            try:
                from apps.components.Model_Editor.model_workshop import ModelWorkshop
                from apps.components.Txd_Editor.txd_workshop import TXDWorkshop
                parser = getattr(ModelWorkshop,'_txd_parser_cache',None)
                if parser is None:
                    parser = TXDWorkshop(main_window=None)
                    ModelWorkshop._txd_parser_cache = parser
                stub = ModelWorkshop.__new__(ModelWorkshop)
                return ModelWorkshop._parse_txd_lightweight(stub, data)
            except Exception:
                return []

        from apps.methods.img_core_classes import IMGFile

        total_txd = total_tex = total_skip = 0
        all_errors = []
        first_txd_path = None

        for key, label, cat_dir, mode_str in jobs:
            os.makedirs(cat_dir, exist_ok=True)
            txd_names = self._collect_txd_names(key)
            img_paths = self._get_img_paths(key)
            if not txd_names or not img_paths:
                continue

            # texlist subdir for decoded textures
            texlist_dir = os.path.join(cat_dir, "texlist")
            if mode_str == 'textures':
                os.makedirs(texlist_dir, exist_ok=True)

            prog = QProgressDialog(
                f"Dumping {label}…", "Cancel", 0, len(img_paths), self)
            prog.setWindowTitle(f"Dumping — {label}")
            prog.setWindowModality(Qt.WindowModality.ApplicationModal)
            prog.show()

            remaining = set(txd_names)
            extracted_txd = extracted_tex = 0

            for i, img_path in enumerate(img_paths):
                prog.setValue(i)
                prog.setLabelText(
                    f"{os.path.basename(img_path)}  "
                    f"({extracted_txd} TXDs, {extracted_tex} textures)")
                QApplication.processEvents()
                if prog.wasCanceled():
                    break
                try:
                    arc = IMGFile(img_path); arc.open()
                    for entry in arc.entries:
                        ename = entry.name.lower()
                        if not ename.endswith('.txd'):
                            continue
                        if key != 'all' and ename not in txd_names:
                            continue
                        try:
                            data = arc.read_entry_data(entry)
                        except Exception as e:
                            all_errors.append(f"{entry.name}: {e}"); continue

                        # ── Write raw TXD ────────────────────────────────
                        raw_out = os.path.join(cat_dir, entry.name)
                        if skip_existing and os.path.exists(raw_out):
                            total_skip += 1
                        else:
                            try:
                                with open(raw_out,'wb') as fh: fh.write(data)
                                extracted_txd += 1
                                if first_txd_path is None:
                                    first_txd_path = raw_out
                            except Exception as e:
                                all_errors.append(f"{entry.name}: {e}"); continue

                        remaining.discard(ename)

                        # ── Decode to image formats ───────────────────────
                        if mode_str != 'textures' or not sel_fmts:
                            continue
                        txd_stem   = os.path.splitext(entry.name)[0]
                        # Flat vs named subfolder structure
                        if getattr(self,'_struct_flat',None) and self._struct_flat.isChecked():
                            tex_subdir = texlist_dir          # flat: all in texlist/
                        else:
                            tex_subdir = os.path.join(texlist_dir, txd_stem)
                        os.makedirs(tex_subdir, exist_ok=True)
                        prog.setLabelText(f"Decoding {entry.name}…")
                        QApplication.processEvents()
                        textures = _parse_txd_data(data)
                        txw = _get_txw()
                        if not txw:
                            all_errors.append(f"{entry.name}: decoder unavailable")
                            continue
                        for tex in textures:
                            tname = (tex.get('name') or 'texture').strip('\x00').strip()
                            if not tname: tname = 'texture'
                            rgba = tex.get('rgba_data')
                            w = tex.get('width',0); h = tex.get('height',0)
                            if not rgba or w==0 or h==0:
                                for lv in (tex.get('mip_levels') or
                                           tex.get('mipmap_levels') or []):
                                    rgba=lv.get('rgba_data'); w=lv.get('width',w)
                                    h=lv.get('height',h)
                                    if rgba: break
                            if not rgba or w==0 or h==0: continue
                            rgba_b = bytes(rgba)
                            for ext in sel_fmts:
                                tex_path = os.path.join(tex_subdir,f"{tname}.{ext}")
                                if skip_existing and os.path.exists(tex_path):
                                    total_skip += 1; continue
                                try:
                                    txw._save_texture_format(rgba_b,w,h,tex_path,
                                                              ext.upper())
                                    extracted_tex += 1
                                except Exception as e:
                                    all_errors.append(f"{tname}.{ext}: {e}")
                except Exception as e:
                    all_errors.append(f"{os.path.basename(img_path)}: {e}")

            prog.setValue(len(img_paths)); prog.close()
            total_txd += extracted_txd; total_tex += extracted_tex

        # Summary
        from PyQt6.QtWidgets import QMessageBox
        lines = [f"TXD files written:   {total_txd}"]
        if total_tex:
            lines.append(f"Textures exported:   {total_tex}"
                         f"  ({', '.join(ext.upper() for ext in sel_fmts)})")
        if total_skip:
            lines.append(f"Skipped (existing):  {total_skip}")
        if all_errors:
            lines.append(f"Errors ({len(all_errors)}): "
                         + "; ".join(all_errors[:5]))
        QMessageBox.information(self, "Dump Complete", "\n".join(lines))

        if self._open_in_txd.isChecked() and first_txd_path:
            mw = self.main_window
            if mw and hasattr(mw,'open_txd_workshop_docked'):
                mw.open_txd_workshop_docked(file_path=first_txd_path)

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
        self._dat_btn_mode    = 'both'   # name | icon | both
        self._auto_load_on_root = False
        self._auto_open_imgs    = False
        # Load persisted settings (after widgets exist)
        self._load_dat_settings()

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
        # Settings button — always icon-only (left of split toggle)
        self._settings_btn = QPushButton()
        self._settings_btn.setFixedSize(24, 24)
        self._settings_btn.setIconSize(QSize(18, 18))
        self._settings_btn.setToolTip("DAT Browser settings")
        self._settings_btn.clicked.connect(self._open_dat_settings)
        toolbar.addWidget(self._settings_btn)

        toolbar.addWidget(self._split_btn)
        # Load settings icon after display is ready
        from PyQt6.QtCore import QTimer as _QT2
        _QT2.singleShot(100, self._load_toolbar_icons)

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
        ll.setSpacing(2)

        # ── Load Order toolbar ────────────────────────────────────────────
        tree_hdr = QHBoxLayout()
        tree_hdr.setContentsMargins(2, 2, 2, 0)
        tree_hdr.setSpacing(4)
        tree_hdr.addWidget(QLabel("Load Order:"))
        tree_hdr.addStretch()

        # Sort combo
        self._sort_combo = QComboBox()
        self._sort_combo.setFixedWidth(110)
        self._sort_combo.setFixedHeight(22)
        self._sort_combo.addItems(["Original", "A → Z", "Z → A",
                                   "Largest first", "Smallest first", "By type"])
        self._sort_combo.setToolTip("Sort load-order entries")
        self._sort_combo.currentIndexChanged.connect(self._on_sort_changed)
        tree_hdr.addWidget(self._sort_combo)

        # Group toggle (SOL only — shown/hidden in _on_world_loaded)
        self._group_btn = QPushButton("Group")
        self._group_btn.setCheckable(True)
        self._group_btn.setChecked(True)
        self._group_btn.setFixedHeight(22)
        self._group_btn.setFixedWidth(52)
        self._group_btn.setToolTip("Group entries by city/section (SOL)")
        self._group_btn.toggled.connect(lambda: self._populate_tree())
        self._group_btn.setVisible(False)   # shown only when SOL is loaded
        tree_hdr.addWidget(self._group_btn)

        # COL-in-IMG toggle
        self._show_col_in_img_btn = QPushButton("COL▾")
        self._show_col_in_img_btn.setCheckable(True)
        self._show_col_in_img_btn.setChecked(False)
        self._show_col_in_img_btn.setFixedHeight(22)
        self._show_col_in_img_btn.setFixedWidth(46)
        self._show_col_in_img_btn.setToolTip(
            "Show .col files embedded inside IMG archives as child nodes")
        self._show_col_in_img_btn.toggled.connect(lambda: self._populate_tree())
        tree_hdr.addWidget(self._show_col_in_img_btn)

        ll.addLayout(tree_hdr)

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

    def _make_table(self, headers): #vers 5
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
        # Double-click on Objects (IDE) table → open in Model Workshop
        t.cellDoubleClicked.connect(
            lambda row, col, tbl=t: self._on_ide_row_double_click(tbl, row))
        t.viewport().setAutoFillBackground(True)
        return t

    # ── Responsive toolbar ─────────────────────────────────────────────────
    _COMPACT_THRESHOLD = 520   # px width below which text→icon for all buttons

    # Button spec: (attr_name, text_label, icon_method, tooltip)
    # icon_method is a string — name of SVGIconFactory method to call
    _BTN_SPECS = [
        ('_browse_btn',    'Browse…',   'folder_icon',        'Browse for game root folder'),
        ('_load_btn',      'Load',      'get_import_icon',    'Load DAT/IDE/IPL world data'),
        ('_dump_txd_btn',  'Dump TXDs', 'package_icon',       'Dump all TXDs from game IMGs'),
    ]

    # Load-order tree button specs: (attr, text, icon_method, tooltip)
    _TREE_BTN_SPECS = [
        ('_group_btn',          'Group',  'get_tree_icon',     'Group by city section (SOL)'),
        ('_show_col_in_img_btn','COL▾',  'get_col_file_icon', 'Show COL files inside IMG archives'),
    ]

    def resizeEvent(self, event): #vers 1
        super().resizeEvent(event)
        self._update_toolbar_compact(event.size().width())

    def _load_toolbar_icons(self): #vers 1
        """Load all SVG icons for toolbar and settings buttons (called once after show)."""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        ic = self._get_icon_color()
        sz = 18

        for attr, _, icon_method, _ in self._BTN_SPECS:
            btn = getattr(self, attr, None)
            if btn is None:
                continue
            try:
                fn = getattr(SVGIconFactory, icon_method)
                btn._dat_icon = fn(sz, ic)
            except Exception:
                btn._dat_icon = None

        for attr, _, icon_method, _ in self._TREE_BTN_SPECS:
            btn = getattr(self, attr, None)
            if btn is None:
                continue
            try:
                fn = getattr(SVGIconFactory, icon_method)
                btn._dat_icon = fn(sz, ic)
            except Exception:
                btn._dat_icon = None

        # Settings button
        try:
            self._settings_btn._dat_icon = SVGIconFactory.settings_icon(sz, ic)
            self._settings_btn.setIcon(self._settings_btn._dat_icon)
        except Exception:
            pass

        # Apply current compact state
        self._update_toolbar_compact(self.width())
        self._apply_btn_style()   # apply name/icon/both setting

    def _get_icon_color(self): #vers 1
        """Return a suitable icon colour from current palette."""
        try:
            pal = self.palette()
            txt = pal.color(pal.ColorRole.WindowText)
            return f"#{txt.red():02x}{txt.green():02x}{txt.blue():02x}"
        except Exception:
            return '#cccccc'

    def _update_toolbar_compact(self, width: int): #vers 3
        """Switch main toolbar buttons between text+icon / icon-only when narrow."""
        from PyQt6.QtGui import QIcon
        from PyQt6.QtCore import QSize
        from PyQt6.QtWidgets import QSizePolicy

        compact = width < self._COMPACT_THRESHOLD
        if compact == self._toolbar_compact:
            return
        self._toolbar_compact = compact
        self._apply_btn_style()

    def _apply_btn_style(self): #vers 1
        """Apply name/icon/both display mode to all compact-aware buttons.
        Reads self._dat_btn_mode: 'name' | 'icon' | 'both' (default 'both')."""
        from PyQt6.QtGui import QIcon
        from PyQt6.QtCore import QSize
        from PyQt6.QtWidgets import QSizePolicy

        mode = getattr(self, '_dat_btn_mode', 'both')
        compact = self._toolbar_compact
        icon_size = QSize(18, 18)

        # In compact mode OR icon-only mode → show icons only
        icon_only = compact or mode == 'icon'
        # In name-only mode → show text only (never icons on text buttons)
        name_only = (mode == 'name')

        all_specs = list(self._BTN_SPECS) + list(self._TREE_BTN_SPECS)
        for attr, text_label, _, tooltip in all_specs:
            btn = getattr(self, attr, None)
            if btn is None:
                continue
            icon = getattr(btn, '_dat_icon', None)

            if name_only or (icon is None):
                # Text only
                btn.setIcon(QIcon())
                btn.setText(text_label)
                btn.setMinimumWidth(0)
                btn.setMaximumWidth(16_777_215)
                btn.setSizePolicy(QSizePolicy.Policy.Preferred,
                                  QSizePolicy.Policy.Fixed)
            elif icon_only:
                # Icon only — square button
                btn.setIcon(icon)
                btn.setIconSize(icon_size)
                btn.setText("")
                btn.setFixedSize(28, 28)
            else:
                # Both text and icon
                btn.setIcon(icon)
                btn.setIconSize(icon_size)
                btn.setText(text_label)
                btn.setMinimumWidth(0)
                btn.setMaximumWidth(16_777_215)
                btn.setSizePolicy(QSizePolicy.Policy.Preferred,
                                  QSizePolicy.Policy.Fixed)
            btn.setToolTip(tooltip)

    def _open_dat_settings(self): #vers 1
        """Open DAT Browser settings dialog."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout,
            QGroupBox, QRadioButton, QCheckBox, QLabel, QDialogButtonBox,
            QButtonGroup, QSpinBox)
        from PyQt6.QtCore import Qt as _Qt

        dlg = QDialog(self)
        dlg.setWindowTitle("DAT Browser — Settings")
        dlg.setMinimumWidth(360)
        lay = QVBoxLayout(dlg)
        lay.setSpacing(10)

        # ── Button display ───────────────────────────────────────────────
        disp_box = QGroupBox("Toolbar button display")
        disp_lay = QVBoxLayout(disp_box)
        disp_grp = QButtonGroup(dlg)

        cur_mode = getattr(self, '_dat_btn_mode', 'both')
        rb_both = QRadioButton("Icons and names")
        rb_icon = QRadioButton("Icons only")
        rb_name = QRadioButton("Names only")
        for rb, val in [(rb_both,'both'),(rb_icon,'icon'),(rb_name,'name')]:
            disp_grp.addButton(rb)
            disp_lay.addWidget(rb)
            if val == cur_mode:
                rb.setChecked(True)
        lay.addWidget(disp_box)

        # ── Compact threshold ────────────────────────────────────────────
        thresh_box = QGroupBox("Compact mode width threshold")
        thresh_lay = QHBoxLayout(thresh_box)
        thresh_lay.addWidget(QLabel("Collapse to icon-only below:"))
        thresh_spin = QSpinBox()
        thresh_spin.setRange(200, 1200)
        thresh_spin.setValue(getattr(self, '_COMPACT_THRESHOLD', 520))
        thresh_spin.setSuffix(" px")
        thresh_lay.addWidget(thresh_spin)
        lay.addWidget(thresh_box)

        # ── Load-order tree options ──────────────────────────────────────
        tree_box = QGroupBox("Load-order tree")
        tree_lay = QVBoxLayout(tree_box)
        cb_group = QCheckBox("Group entries by city (SOL)")
        cb_group.setChecked(
            getattr(self,'_group_btn',None) and self._group_btn.isChecked())
        cb_col_img = QCheckBox("Show COL files inside IMG archives")
        cb_col_img.setChecked(
            getattr(self,'_show_col_in_img_btn',None)
            and self._show_col_in_img_btn.isChecked())
        tree_lay.addWidget(cb_group)
        tree_lay.addWidget(cb_col_img)
        lay.addWidget(tree_box)

        # ── Auto-load ────────────────────────────────────────────────────
        auto_box = QGroupBox("Auto-load")
        auto_lay = QVBoxLayout(auto_box)
        cb_auto_load = QCheckBox("Auto-load game assets when game root is set")
        cb_auto_load.setChecked(getattr(self,'_auto_load_on_root', False))
        cb_auto_imgs = QCheckBox("Auto-open all IMGs in IMG Factory after load")
        cb_auto_imgs.setChecked(getattr(self,'_auto_open_imgs', False))
        auto_lay.addWidget(cb_auto_load)
        auto_lay.addWidget(cb_auto_imgs)
        lay.addWidget(auto_box)

        # ── Buttons ──────────────────────────────────────────────────────
        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok |
            QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        lay.addWidget(btns)

        if dlg.exec() != QDialog.DialogCode.Accepted:
            return

        # Apply settings
        if rb_both.isChecked():   mode = 'both'
        elif rb_icon.isChecked(): mode = 'icon'
        else:                     mode = 'name'

        self._dat_btn_mode = mode
        self._COMPACT_THRESHOLD = thresh_spin.value()

        if hasattr(self,'_group_btn'):
            self._group_btn.setChecked(cb_group.isChecked())
        if hasattr(self,'_show_col_in_img_btn'):
            self._show_col_in_img_btn.setChecked(cb_col_img.isChecked())
        self._auto_load_on_root = cb_auto_load.isChecked()
        self._auto_open_imgs    = cb_auto_imgs.isChecked()

        self._apply_btn_style()

        # Persist to JSON settings
        self._save_dat_settings()

    def _save_dat_settings(self): #vers 1
        """Persist DAT Browser UI settings to ~/.config/imgfactory/dat_browser.json"""
        import json
        cfg_dir = os.path.expanduser('~/.config/imgfactory')
        os.makedirs(cfg_dir, exist_ok=True)
        cfg = {
            'btn_mode':          getattr(self, '_dat_btn_mode', 'both'),
            'compact_threshold': getattr(self, '_COMPACT_THRESHOLD', 520),
            'group_sol':         (getattr(self,'_group_btn',None)
                                  and self._group_btn.isChecked()),
            'col_in_img':        (getattr(self,'_show_col_in_img_btn',None)
                                  and self._show_col_in_img_btn.isChecked()),
            'auto_load':         getattr(self,'_auto_load_on_root', False),
            'auto_open_imgs':    getattr(self,'_auto_open_imgs', False),
        }
        try:
            with open(os.path.join(cfg_dir, 'dat_browser.json'), 'w') as f:
                json.dump(cfg, f, indent=2)
        except Exception:
            pass

    def _load_dat_settings(self): #vers 1
        """Load persisted DAT Browser settings from JSON."""
        import json
        path = os.path.expanduser('~/.config/imgfactory/dat_browser.json')
        if not os.path.isfile(path):
            return
        try:
            with open(path) as f:
                cfg = json.load(f)
            self._dat_btn_mode       = cfg.get('btn_mode', 'both')
            self._COMPACT_THRESHOLD  = cfg.get('compact_threshold', 520)
            self._auto_load_on_root  = cfg.get('auto_load', False)
            self._auto_open_imgs     = cfg.get('auto_open_imgs', False)
            if hasattr(self,'_group_btn'):
                self._group_btn.setChecked(cfg.get('group_sol', True))
            if hasattr(self,'_show_col_in_img_btn'):
                self._show_col_in_img_btn.setChecked(cfg.get('col_in_img', False))
        except Exception:
            pass

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
            if getattr(self, '_auto_load_on_root', False):
                from PyQt6.QtCore import QTimer
                QTimer.singleShot(300, self._start_load)
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
        # Auto-open all IMGs if the setting is enabled
        if getattr(self, '_auto_open_imgs', False) and self.loader and self.loader.load_log:
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(800, self._load_all_game_imgs)

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

    # ── SOL group mapping ───────────────────────────────────────────────
    _SOL_GROUPS = {
        'special':    'Special',
        'generics':   'Generics',
        'game_vc':    'VC City',
        'game_lc':    'LC City',
        'game_la':    'LA (San Andreas)',
        'game_sf':    'San Fierro',
        'game_lv':    'Las Venturas',
        'game_sa':    'San Andreas',
        'game_mll':   'Mainland',
        'game_ext':   'Extended',
        'skyeffects': 'Sky Effects',
        'radartex':   'Radar Textures',
    }

    # IPL folder → city group (for SOL maps/XX/ layout)
    _SOL_IPL_FOLDERS = {
        'vc': 'VC City', 'lc': 'LC City', 'la': 'LA (San Andreas)',
        'sf': 'San Fierro', 'lv': 'Las Venturas', 'sa': 'San Andreas',
        'mll': 'Mainland', 'ext': 'Extended',
        'ifx': 'Lighting (IFX)', 'zones': 'Zones',
    }

    def _stem_group(self, bname: str, full_path: str = '') -> str:
        """Return group name for a basename/path, or empty string if ungrouped."""
        stem = bname.lower().split('.')[0]
        # Direct stem match (IMG/COL/IDE)
        grp = self._SOL_GROUPS.get(stem, '')
        if grp:
            return grp
        # For IPL files: group by parent folder name
        if full_path and bname.lower().endswith(('.ipl', '.zon')):
            parts = full_path.replace(os.sep, '/').lower().split('/')
            # Look for folder name in _SOL_IPL_FOLDERS
            for part in reversed(parts[:-1]):  # skip filename itself
                match = self._SOL_IPL_FOLDERS.get(part, '')
                if match:
                    return match
                # Handle maps/XX/ path structure
                if 'maps' in parts:
                    mi = parts.index('maps')
                    if mi + 1 < len(parts) - 1:
                        folder = parts[mi + 1]
                        match = self._SOL_IPL_FOLDERS.get(folder, '')
                        if match:
                            return match
                        return f'Maps/{folder.upper()}'
        return ''

    def _sort_log(self, log: list) -> list:
        """Sort load_log according to self._sort_combo selection."""
        mode = getattr(self, '_sort_combo', None)
        if not mode:
            return log
        idx = mode.currentIndex()
        if idx == 0:   # Original
            return log
        elif idx == 1: # A → Z
            return sorted(log, key=lambda e: os.path.basename(e[2]).lower())
        elif idx == 2: # Z → A
            return sorted(log, key=lambda e: os.path.basename(e[2]).lower(), reverse=True)
        elif idx == 3: # Largest first
            def _sz(e):
                try: return os.path.getsize(e[2]) if e[3] else 0
                except: return 0
            return sorted(log, key=_sz, reverse=True)
        elif idx == 4: # Smallest first
            def _sz2(e):
                try: return os.path.getsize(e[2]) if e[3] else 0
                except: return 999_999_999
            return sorted(log, key=_sz2)
        elif idx == 5: # By type
            order = {"IMG":0,"CDIMAGE":1,"IDE":2,"IPL":3,"COLFILE":4}
            return sorted(log, key=lambda e: (order.get(e[1],9), os.path.basename(e[2]).lower()))
        return log

    def _on_sort_changed(self): #vers 1
        """Re-populate tree when sort mode changes."""
        if self.loader and self.loader.load_log:
            self._populate_tree()

    def _make_entry_child(self, phase, entry_type, path, success): #vers 1
        """Build a QTreeWidgetItem for one load_log entry."""
        bname = os.path.basename(path)
        count = 0
        count_str = "0"

        if entry_type == "IDE":
            count = sum(1 for o in self.loader.objects.values()
                        if o.source_ide == bname)
            count_str = str(count)
        elif entry_type == "IPL":
            count = sum(1 for i in self.loader.instances
                        if i.source_ipl == bname)
            count_str = str(count)
        elif entry_type in ("IMG", "CDIMAGE"):
            try:
                sz = os.path.getsize(path) if success else 0
                count_str = (f"{sz//1024//1024} MB" if sz > 1024*1024
                             else f"{sz//1024} KB" if sz else "—")
            except Exception:
                count_str = "—"
        elif entry_type == "COLFILE":
            count_str = ""

        label = ("IMG" if entry_type == "IMG"
                 else "CDIMAGE" if entry_type == "CDIMAGE"
                 else "COL"     if entry_type == "COLFILE"
                 else entry_type)
        tag   = f" [{phase}]" if phase == "enforced" else ""
        status = "✓" if success else "✗ missing"

        child = QTreeWidgetItem([bname + tag, label, count_str, status])
        child.setData(0, Qt.ItemDataRole.UserRole, path)
        if not success:
            for col in range(4):
                child.setForeground(col, QColor(204, 68, 68))
            if entry_type == "COLFILE":
                child.setToolTip(0, f"Not found: {path}")
        return child

    def _add_col_in_img_children(self, img_item, img_path): #vers 1
        """Scan an IMG archive and add .col entries as child nodes."""
        try:
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(img_path)
            arc.open()
            col_entries = [e for e in arc.entries
                           if e.name.lower().endswith('.col')]
            for e in col_entries:
                ci = QTreeWidgetItem([e.name, "COL▾", "", "✓"])
                ci.setData(0, Qt.ItemDataRole.UserRole + 1, img_path)  # parent IMG
                ci.setData(0, Qt.ItemDataRole.UserRole + 2, e.name)    # entry name
                ci.setForeground(1, QColor("#ef5350"))
                img_item.addChild(ci)
        except Exception:
            pass

    def _populate_tree(self): #vers 3
        self._tree.clear()
        if not self.loader:
            return

        from apps.methods.gta_dat_parser import GTAGame
        is_sol   = getattr(self.loader, 'game', None) == GTAGame.SOL
        do_group = is_sol and getattr(self, '_group_btn', None) and self._group_btn.isChecked()
        do_col_in_img = (getattr(self, '_show_col_in_img_btn', None)
                         and self._show_col_in_img_btn.isChecked())

        # Show / hide group button
        if hasattr(self, '_group_btn'):
            self._group_btn.setVisible(is_sol)

        default_path = getattr(self.loader.default_dat, "dat_path", "")
        main_path    = getattr(self.loader.main_dat,    "dat_path", "")
        display_name = os.path.basename(main_path) if main_path else "unknown.dat"

        root_item = QTreeWidgetItem([display_name, "DAT", "", "✓"])
        root_item.setExpanded(True)
        self._tree.addTopLevelItem(root_item)

        if default_path:
            def_name = os.path.basename(default_path)
            def_item = QTreeWidgetItem([def_name, "DAT-1", "", "✓"])
            def_item.setForeground(0, self.palette().color(
                self.foregroundRole()).darker(150))
            root_item.addChild(def_item)

        sorted_log = self._sort_log(list(self.loader.load_log))

        if do_group:
            # Group by city section
            groups: dict = {}   # group_name → list of (phase, entry_type, path, success)
            ungrouped = []
            for entry in sorted_log:
                bname = os.path.basename(entry[2])
                grp   = self._stem_group(bname, entry[2])
                if grp:
                    groups.setdefault(grp, []).append(entry)
                else:
                    ungrouped.append(entry)

            # Group order follows _SOL_GROUPS insertion order
            seen_groups = []
            ordered_entries = []
            for entry in sorted_log:
                bname = os.path.basename(entry[2])
                grp   = self._stem_group(bname, entry[2])
                if grp and grp not in seen_groups:
                    seen_groups.append(grp)
                    ordered_entries.append(('__group__', grp, groups[grp]))
                elif not grp:
                    ordered_entries.append(('__entry__',) + entry)

            for item in ordered_entries:
                if item[0] == '__group__':
                    _, grp_name, entries = item
                    # Count files in group
                    n_img = sum(1 for e in entries if e[1] in ('IMG','CDIMAGE'))
                    n_col = sum(1 for e in entries if e[1] == 'COLFILE')
                    n_ide = sum(1 for e in entries if e[1] == 'IDE')
                    n_ipl = sum(1 for e in entries if e[1] == 'IPL')
                    summary = f"IMG:{n_img} IDE:{n_ide} COL:{n_col}"
                    grp_item = QTreeWidgetItem([grp_name, "GROUP", summary, ""])
                    grp_item.setExpanded(True)
                    from PyQt6.QtGui import QFont as _QF
                    f = _QF(); f.setBold(True)
                    grp_item.setFont(0, f)
                    root_item.addChild(grp_item)
                    for phase, entry_type, path, success in entries:
                        child = self._make_entry_child(phase, entry_type, path, success)
                        grp_item.addChild(child)
                        if do_col_in_img and entry_type in ('IMG','CDIMAGE') and success:
                            self._add_col_in_img_children(child, path)
                else:
                    _, phase, entry_type, path, success = item
                    child = self._make_entry_child(phase, entry_type, path, success)
                    root_item.addChild(child)
                    if do_col_in_img and entry_type in ('IMG','CDIMAGE') and success:
                        self._add_col_in_img_children(child, path)
        else:
            # Flat list (original behaviour + sort)
            for phase, entry_type, path, success in sorted_log:
                child = self._make_entry_child(phase, entry_type, path, success)
                root_item.addChild(child)
                if do_col_in_img and entry_type in ('IMG','CDIMAGE') and success:
                    self._add_col_in_img_children(child, path)

        self._tree.expandAll()
        self._refresh_img_loaded_indicators()

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

    def _on_tree_click(self, item, col): #vers 3
        bname      = item.text(0).split('[')[0].strip()
        entry_type = item.text(1)
        if entry_type == "IDE":
            self._search_edit.blockSignals(True)
            self._search_edit.setText("")
            self._search_edit.blockSignals(False)
            self._populate_objects_for_ide(bname)
        elif entry_type == "IPL":
            self._tabs.setCurrentIndex(1)
            self._populate_instances_for_ipl(bname)
        elif entry_type in ("IMG", "CDIMAGE"):
            # Single-click on IMG: bring its tab to front if already open,
            # otherwise open it
            self._open_img_in_factory(item)
        elif entry_type == "COL":
            self._open_col_in_workshop(item)
        elif entry_type == "COL▾":
            # Embedded COL inside an IMG — extract and open in COL Workshop
            self._open_embedded_col(item)
        elif entry_type == "GROUP":
            # Collapse / expand group
            item.setExpanded(not item.isExpanded())
        elif entry_type == "DAT":
            self._search_edit.setText("")
            self._type_filter.setCurrentIndex(0)

    def _open_embedded_col(self, tree_item): #vers 1
        """Open a .col file that's embedded inside an IMG archive."""
        img_path  = tree_item.data(0, Qt.ItemDataRole.UserRole + 1)
        col_name  = tree_item.data(0, Qt.ItemDataRole.UserRole + 2)
        mw = self.main_window
        if not img_path or not col_name or not os.path.isfile(img_path):
            return
        try:
            from apps.methods.img_core_classes import IMGFile
            import tempfile
            arc = IMGFile(img_path); arc.open()
            entry = next((e for e in arc.entries
                          if e.name.lower() == col_name.lower()), None)
            if not entry:
                if mw and hasattr(mw,'log_message'):
                    mw.log_message(f"COL entry not found: {col_name}")
                return
            data = arc.read_entry_data(entry)
            tmp  = tempfile.NamedTemporaryFile(
                delete=False, suffix='.col',
                prefix=os.path.splitext(col_name)[0]+'_')
            tmp.write(data); tmp.close()
            from apps.components.Col_Editor.col_workshop import open_col_workshop
            open_col_workshop(mw, tmp.name)
            if mw and hasattr(mw,'log_message'):
                mw.log_message(f"COL Workshop: {col_name} (from {os.path.basename(img_path)})")
        except Exception as e:
            if mw and hasattr(mw,'log_message'):
                mw.log_message(f"Embedded COL error: {e}")

    def _bring_img_tab_to_front(self, abs_path: str): #vers 1
        """If abs_path is already open as a tab, switch to it. Otherwise open it."""
        mw = self.main_window
        if not mw or not hasattr(mw, 'main_tab_widget'):
            return False
        tw = mw.main_tab_widget
        norm = os.path.normcase(abs_path)
        for i in range(tw.count()):
            w = tw.widget(i)
            if (w and getattr(w,'file_type','') == 'IMG'
                    and os.path.normcase(getattr(w,'file_path','')) == norm):
                tw.setCurrentIndex(i)
                if mw and hasattr(mw,'log_message'):
                    mw.log_message(
                        f"Switched to tab: {os.path.basename(abs_path)}")
                return True
        return False   # not open yet

    def _open_col_in_workshop(self, tree_item): #vers 1
        """Click on a COL tree entry → open in COL Workshop."""
        path = tree_item.data(0, Qt.ItemDataRole.UserRole) or ''
        if not path:
            bname = tree_item.text(0)
            for _ph, et, p, ok in self.loader.load_log:
                if et == 'COLFILE' and os.path.basename(p) == bname:
                    path = p; break
        self._open_col_in_workshop_path(path)

    def _open_col_in_workshop_path(self, abs_path: str): #vers 1
        """Open a standalone .col file in COL Workshop."""
        mw = self.main_window
        if not abs_path or not os.path.isfile(abs_path):
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(f"COL file not found: {abs_path}")
            return
        try:
            from apps.components.Col_Editor.col_workshop import open_col_workshop
            w = open_col_workshop(mw, abs_path)
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(f"COL Workshop: {os.path.basename(abs_path)}")
        except Exception as e:
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(f"COL Workshop error: {e}")

    def _open_single_img_in_factory(self, abs_path: str): #vers 1
        """Open one specific IMG file in a new IMG Factory tab."""
        mw = self.main_window
        if not mw or not abs_path or not os.path.isfile(abs_path):
            return
        if hasattr(mw, '_load_img_file_in_new_tab'):
            mw._load_img_file_in_new_tab(abs_path)
            if hasattr(mw, 'log_message'):
                mw.log_message(f"Loading {os.path.basename(abs_path)} in new tab…")
            # Refresh indicators after a short delay
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(1500, self._refresh_img_loaded_indicators)
        else:
            # Fallback path (no tab system)
            self._open_img_in_factory_item_fallback(abs_path)

    def _open_img_in_factory_item_fallback(self, abs_path: str): #vers 1
        """Fallback: load IMG directly into main_window without tab system."""
        mw = self.main_window
        if not mw:
            return
        try:
            from apps.methods.img_core_classes import IMGFile
            img = IMGFile(abs_path)
            img.open()
            mw.current_img = img
            if hasattr(mw, 'log_message'):
                mw.log_message(
                    f"Loaded {os.path.basename(abs_path)} ({len(img.entries)} entries)")
            if hasattr(mw, '_populate_real_img_table'):
                mw._populate_real_img_table(img)
        except Exception as e:
            if hasattr(mw, 'log_message'):
                mw.log_message(f"IMG load error: {e}")

    def _dump_single_img_txds(self, img_path: str): #vers 1
        """Extract all TXDs from one specific IMG to a folder."""
        from PyQt6.QtWidgets import QFileDialog, QProgressDialog, QMessageBox
        from PyQt6.QtCore import Qt
        out_dir = QFileDialog.getExistingDirectory(
            self, f"Dump TXDs from {os.path.basename(img_path)}")
        if not out_dir:
            return
        try:
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(img_path)
            arc.open()
            txd_entries = [e for e in arc.entries
                           if e.name.lower().endswith('.txd')]
            prog = QProgressDialog(
                f"Extracting TXDs from {os.path.basename(img_path)}…",
                "Cancel", 0, len(txd_entries), self)
            prog.setWindowModality(Qt.WindowModality.ApplicationModal)
            prog.show()
            extracted = skipped = 0
            from PyQt6.QtWidgets import QApplication
            for i, entry in enumerate(txd_entries):
                prog.setValue(i)
                QApplication.processEvents()
                if prog.wasCanceled():
                    break
                out_path = os.path.join(out_dir, entry.name)
                if os.path.exists(out_path):
                    skipped += 1; continue
                try:
                    with open(out_path, 'wb') as f:
                        f.write(arc.read_entry_data(entry))
                    extracted += 1
                except Exception:
                    pass
            prog.close()
            QMessageBox.information(self, "Done",
                f"Extracted {extracted} TXDs to {out_dir}\nSkipped (exist): {skipped}")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to dump TXDs:\n{e}")

    def _open_img_in_factory(self, tree_item): #vers 2
        """Open (or bring to front) the IMG archive clicked in the load-order tree."""
        mw = self.main_window
        if not mw:
            return
        # Resolve abs path: prefer UserRole data stored on IMG items
        abs_path = tree_item.data(0, Qt.ItemDataRole.UserRole) or None
        if not abs_path:
            stem = tree_item.text(0).split('[')[0].strip()
            for _phase, etype, path, ok in self.loader.load_log:
                if etype in ('IMG', 'CDIMAGE') and os.path.basename(path) == stem:
                    abs_path = path; break
        if not abs_path or not os.path.isfile(abs_path):
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(f"IMG not found on disk: {abs_path or '?'}")
            return
        # bname always derived from abs_path so it is always defined
        bname = os.path.basename(abs_path)
        # If already open as a tab, just bring it to front
        if self._bring_img_tab_to_front(abs_path):
            return
        # Open in IMG Factory
        try:
            if hasattr(mw, '_load_img_file_in_new_tab'):
                mw._load_img_file_in_new_tab(abs_path)
                if hasattr(mw, 'log_message'):
                    mw.log_message(f"Opening {bname} in new tab…")
                from PyQt6.QtCore import QTimer
                QTimer.singleShot(1500, self._refresh_img_loaded_indicators)
            elif hasattr(mw, 'open_img_file_path'):
                mw.open_img_file_path(abs_path)
            elif hasattr(mw, '_load_img_file'):
                mw._load_img_file(abs_path)
            elif hasattr(mw, 'load_img'):
                mw.load_img(abs_path)
            else:
                # Fallback: use IMGFile directly and set as current_img
                from apps.methods.img_core_classes import IMGFile
                img = IMGFile(abs_path)
                img.open()
                mw.current_img = img
                if hasattr(mw, 'log_message'):
                    mw.log_message(f"Loaded IMG: {bname} ({len(img.entries)} entries)")
                if hasattr(mw, '_populate_real_img_table'):
                    mw._populate_real_img_table(img)
        except Exception as e:
            import traceback; traceback.print_exc()
            if hasattr(mw, 'log_message'):
                mw.log_message(f"IMG open error: {e}")

    def _on_ide_row_double_click(self, table, row): #vers 1
        """Double-click on an IDE Objects row → open DFF + TXD in Model Workshop.
        Uses the XRef to find model_name.dff and txd_name.txd in the IMG archives."""
        if not self.loader or not self.xref:
            return
        model_item = table.item(row, 1)   # Model column
        txd_item   = table.item(row, 2)   # TXD column
        if not model_item:
            return

        model_name = model_item.text().strip()
        txd_name   = txd_item.text().strip() if txd_item else ''
        mw = self.main_window
        if not mw:
            return

        # Find DFF and TXD in IMG archives via xref
        load_log = self.loader.load_log
        game_root = getattr(self.xref, 'game_root', '') or ''
        try:
            found = self.xref.find_in_imgs(model_name, load_log, game_root)
        except Exception as e:
            if hasattr(mw, 'log_message'):
                mw.log_message(f"XRef search error: {e}")
            return

        dff_img = found.get('dff')
        txd_img = found.get('txd')

        if not dff_img:
            if hasattr(mw, 'log_message'):
                mw.log_message(
                    f"DFF not found: {model_name}.dff not in any loaded IMG")
            return

        # Extract DFF to tempfile
        import tempfile
        try:
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(dff_img)
            arc.open()
            dff_entry = next((e for e in arc.entries
                              if e.name.lower() == model_name.lower() + '.dff'), None)
            if not dff_entry:
                if hasattr(mw, 'log_message'):
                    mw.log_message(f"DFF entry not found in {os.path.basename(dff_img)}")
                return
            dff_data = arc.read_entry_data(dff_entry)
            tmp = tempfile.NamedTemporaryFile(
                delete=False, suffix='.dff', prefix=model_name + '_')
            tmp.write(dff_data); tmp.close()
            dff_tmp = tmp.name
        except Exception as e:
            if hasattr(mw, 'log_message'):
                mw.log_message(f"DFF extract error: {e}")
            return

        # Open Model Workshop with DFF
        from apps.components.Model_Editor.model_workshop import open_model_workshop
        workshop = open_model_workshop(mw, dff_tmp)

        # Also load TXD if found
        if workshop and txd_img:
            try:
                arc2 = IMGFile(txd_img)
                arc2.open()
                txd_stem = (found.get('txd_name') or txd_name or model_name).lower()
                txd_entry = next(
                    (e for e in arc2.entries
                     if e.name.lower() == txd_stem + '.txd'), None)
                if txd_entry:
                    txd_data = arc2.read_entry_data(txd_entry)
                    txd_tmp = tempfile.NamedTemporaryFile(
                        delete=False, suffix='.txd', prefix=txd_stem + '_')
                    txd_tmp.write(txd_data); txd_tmp.close()
                    if hasattr(workshop, '_load_txd_file'):
                        workshop._load_txd_file(txd_tmp.name)
                    if hasattr(mw, 'log_message'):
                        mw.log_message(
                            f"Model Workshop: {model_name}.dff + {txd_stem}.txd")
            except Exception as e:
                if hasattr(mw, 'log_message'):
                    mw.log_message(f"TXD load error: {e}")
        elif workshop and hasattr(mw, 'log_message'):
            mw.log_message(f"Model Workshop: {model_name}.dff (no TXD found)")

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

    def _apply_theme_stylesheet(self): #vers 4
        """Apply theme colors. Uses QApplication global stylesheet for most
        styling; sets palette background explicitly to prevent resize flicker."""
        from PyQt6.QtGui import QPalette, QColor
        mw = getattr(self, 'main_window', None)
        colors = {}
        if mw and hasattr(mw, 'app_settings'):
            try:
                colors = mw.app_settings.get_theme_colors() or {}
            except Exception:
                pass

        bg       = colors.get('panel_bg', colors.get('bg_primary', ''))
        row_odd  = colors.get('table_row_odd',  colors.get('bg_primary', ''))
        row_even = colors.get('table_row_even', colors.get('alternate_row', ''))
        fg       = colors.get('text_primary', '')

        def c(val, fallback):
            return val if val else f'palette({fallback})'

        # Set palette background explicitly — prevents Qt repaint flicker on resize
        # where Qt briefly shows a black rectangle before the stylesheet paints
        if bg:
            try:
                pal = self.palette()
                col = QColor(bg)
                pal.setColor(QPalette.ColorRole.Window,     col)
                pal.setColor(QPalette.ColorRole.Base,       QColor(row_odd)  if row_odd  else col)
                pal.setColor(QPalette.ColorRole.AlternateBase, QColor(row_even) if row_even else col)
                if fg:
                    pal.setColor(QPalette.ColorRole.WindowText, QColor(fg))
                    pal.setColor(QPalette.ColorRole.Text,       QColor(fg))
                self.setPalette(pal)
                # Propagate to tree and tables too
                for child_attr in ('_tree', '_obj_table', '_inst_table',
                                   '_zone_table', '_log_text'):
                    child = getattr(self, child_attr, None)
                    if child:
                        child.setPalette(pal)
            except Exception:
                pass

        # Minimal stylesheet override — row colors only
        ss = f"""
            QTreeWidget {{
                alternate-background-color: {c(row_even, 'alternateBase')};
                background-color: {c(row_odd, 'base')};
            }}
            QTableWidget {{
                alternate-background-color: {c(row_even, 'alternateBase')};
                background-color: {c(row_odd, 'base')};
            }}
        """
        self.setStyleSheet(ss)

    def _on_theme_changed(self): #vers 4
        """Refresh DAT-specific row colors when theme switches.
        Global stylesheet is handled by QApplication — no need to reapply here."""
        self._apply_theme_stylesheet()
        self.setStyleSheet(self.styleSheet())  # force repaint
        self.update()

    def _get_txd_names_from_img(self, img_path: str) -> list:
        """Return list of .txd entry names from an IMG archive."""
        try:
            from apps.methods.img_core_classes import IMGFile
            img = IMGFile(img_path)
            img.open()
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
                from apps.methods.img_core_classes import IMGFile
                for _phase, etype, path, ok in self.loader.load_log:
                    if not (ok and etype in ('IMG', 'CDIMAGE') and os.path.isfile(path)):
                        continue
                    try:
                        arc = IMGFile(path)
                        arc.open()
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
            from apps.methods.img_core_classes import IMGFile
            for _phase, etype, path, ok in self.loader.load_log:
                if not (ok and etype in ('IMG', 'CDIMAGE') and os.path.isfile(path)):
                    continue
                if not txd_names:
                    break
                try:
                    arc = IMGFile(path)
                    arc.open()
                    for entry in arc.entries:
                        if entry.name.lower() in txd_names:
                            out_path = os.path.join(out_dir, entry.name)
                            if os.path.exists(out_path):
                                skipped += 1
                            else:
                                try:
                                    with open(out_path, 'wb') as f:
                                        f.write(arc.read_entry_data(entry))
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

    def _get_open_img_paths(self) -> set: #vers 1
        """Return set of normalised abs paths for every IMG open in a main_window tab."""
        mw = self.main_window
        if not mw or not hasattr(mw, 'main_tab_widget'):
            return set()
        tw = mw.main_tab_widget
        open_paths = set()
        for i in range(tw.count()):
            w = tw.widget(i)
            if w and getattr(w, 'file_type', '') == 'IMG':
                fp = getattr(w, 'file_path', '') or ''
                if fp:
                    open_paths.add(os.path.normcase(fp))
        return open_paths

    def _load_all_game_imgs(self): #vers 1
        """Open every IMG/CDIMAGE in load_log as a new IMG Factory tab."""
        mw = self.main_window
        if not mw or not self.loader:
            return
        if not hasattr(mw, '_load_img_file_in_new_tab'):
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "Load all IMGs",
                "Main window does not support loading IMG tabs.")
            return
        open_paths = self._get_open_img_paths()
        queued = 0
        for _phase, etype, path, ok in self.loader.load_log:
            if not (ok and etype in ('IMG', 'CDIMAGE') and os.path.isfile(path)):
                continue
            if os.path.normcase(path) in open_paths:
                continue   # already open
            mw._load_img_file_in_new_tab(path)
            open_paths.add(os.path.normcase(path))
            queued += 1
        if hasattr(mw, 'log_message'):
            mw.log_message(f"DAT Browser: queued {queued} IMG(s) to load")

    def _setup_tree_context_menu(self): #vers 1
        """Enable right-click on the load-order tree to open IDE/IPL/DAT files."""
        self._tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._tree.customContextMenuRequested.connect(self._on_tree_context_menu)

    def _refresh_img_loaded_indicators(self): #vers 1
        """Update the Status column on IMG tree items to show [open] if loaded in IMG Factory."""
        open_paths = self._get_open_img_paths()
        def _walk(item):
            etype = item.text(1)
            if etype in ("IMG", "CDIMAGE"):
                path = item.data(0, Qt.ItemDataRole.UserRole) or ""
                if os.path.normcase(path) in open_paths:
                    item.setText(3, "◉ open")
                    from PyQt6.QtGui import QColor
                    item.setForeground(3, QColor("#16a34a"))
                elif item.text(3) == "◉ open":
                    item.setText(3, "✓")
                    item.setForeground(3, self.palette().color(
                        self.foregroundRole()))
            for i in range(item.childCount()):
                _walk(item.child(i))
        root = self._tree.invisibleRootItem()
        for i in range(root.childCount()):
            _walk(root.child(i))

    def _on_tree_context_menu(self, pos): #vers 2
        item = self._tree.itemAt(pos)
        if not item:
            return
        entry_type = item.text(1)
        bname      = item.text(0).split('[')[0].strip()

        # Resolve abs path — prefer stored UserRole data for IMG entries
        abs_path = item.data(0, Qt.ItemDataRole.UserRole) or None
        if not abs_path:
            for _phase, _etype, _path, _ok in self.loader.load_log:
                if os.path.basename(_path) == bname and _etype == entry_type:
                    abs_path = _path
                    break

        menu = QMenu(self)

        # ── IMG / CDIMAGE specific options ───────────────────────────────
        if entry_type in ("IMG", "CDIMAGE"):
            mw = self.main_window
            if abs_path and os.path.isfile(abs_path):
                open_act = menu.addAction(f"⊞  Open in IMG Factory tab")
                open_act.triggered.connect(
                    lambda _=False, p=abs_path: self._open_single_img_in_factory(p))
            load_all_act = menu.addAction("⊞  Load ALL game IMGs into IMG Factory")
            load_all_act.triggered.connect(self._load_all_game_imgs)
            menu.addSeparator()
            dump_act = menu.addAction("📦  Dump all TXDs from this IMG…")
            if abs_path and os.path.isfile(abs_path):
                dump_act.triggered.connect(
                    lambda _=False, p=abs_path: self._dump_single_img_txds(p))
            else:
                dump_act.setEnabled(False)
            menu.addSeparator()

        elif entry_type == "COL":
            abs_path = item.data(0, Qt.ItemDataRole.UserRole) or abs_path
            if abs_path and os.path.isfile(abs_path):
                open_col_act = menu.addAction("⬛  Open in COL Workshop")
                open_col_act.triggered.connect(
                    lambda _=False, p=abs_path: self._open_col_in_workshop_path(p))
            menu.addSeparator()

        elif entry_type == "COL▾":
            # Embedded COL inside IMG
            menu.addAction("⬛  Extract & open in COL Workshop").triggered.connect(
                lambda _=False, it=item: self._open_embedded_col(it))
            menu.addSeparator()

        elif abs_path and os.path.isfile(abs_path):
            ext = os.path.splitext(abs_path)[1].lower()
            if ext == ".ide":
                menu.addAction(f"📋  Filter Objects to  {bname}").triggered.connect(
                    lambda _=False, b=bname: (
                        self._search_edit.blockSignals(True),
                        self._search_edit.setText(""),
                        self._search_edit.blockSignals(False),
                        self._populate_objects_for_ide(b)))
                menu.addAction(f"✏  Edit  {bname}").triggered.connect(
                    lambda _=False, p=abs_path: self._open_path_in_editor(p))
                menu.addAction("🔍  Open in IDE Editor").triggered.connect(
                    lambda _=False, p=abs_path: self._open_in_ide_editor(p))
                menu.addSeparator()
            elif ext == ".ipl":
                menu.addAction(f"📋  Filter Instances to  {bname}").triggered.connect(
                    lambda _=False, b=bname: (
                        self._tabs.setCurrentIndex(1),
                        self._populate_instances_for_ipl(b)))
                menu.addAction(f"✏  Edit  {bname}").triggered.connect(
                    lambda _=False, p=abs_path: self._open_path_in_editor(p))
                menu.addSeparator()
            elif ext in (".dat", ".txt", ".cfg", ".ini"):
                menu.addAction(f"✏  Edit  {bname}").triggered.connect(
                    lambda _=False, p=abs_path: self._open_path_in_editor(p))
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
