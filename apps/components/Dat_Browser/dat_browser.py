#this belongs in components/Dat_Browser/dat_browser.py - Version: 2
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


#                                                                              
# Background load thread
#                                                                              

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


#                                                                              
# Main browser widget
#                                                                              

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

    #    Category row specs                                                     
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

        #    Game info                                                      
        game_names = {GTAGame.GTA3:"GTA III (LC)", GTAGame.VC:"Vice City (VC)",
                      GTAGame.SA:"San Andreas (SA)", GTAGame.SOL:"GTA SOL (multi-city)"}
        info = QLabel(f"Game: <b>{game_names.get(game,str(game))}</b>"
                      f"  —  {len(self.loader.objects)} IDE objects loaded")
        info.setTextFormat(Qt.TextFormat.RichText)
        lay.addWidget(info)

        #    Load saved paths                                               
        saved = self._load_saved_paths()
        _saved_flat = saved.get('_struct_flat', False)

        #    Per-category rows                                              
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

        #    Export formats                                                 
        fmt_grp = QGroupBox("Export format(s)  — used when Textures is selected")
        fmt_lay = QHBoxLayout(fmt_grp)
        self._fmt_checks = {}
        for fmt, default in [('IFF/ILBM',True),('PNG',True),
                              ('TGA',False),('DDS',False),('BMP',False)]:
            cb = QCheckBox(fmt); cb.setChecked(default)
            fmt_lay.addWidget(cb)
            self._fmt_checks[fmt] = cb
        lay.addWidget(fmt_grp)

        #    Options                                                        
        opt_grp = QGroupBox("Options")
        opt_lay = QVBoxLayout(opt_grp)

        #    Texture output structure                                       
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

        #    Default output folder                                          
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

        #    Preview                                                        
        self._preview_lbl = QLabel("")
        self._preview_lbl.setWordWrap(True)
        self._preview_lbl.setStyleSheet("font-style:italic; color:palette(mid);")
        lay.addWidget(self._preview_lbl)
        self._update_preview()

        #    Button row                                                     
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

        #    Radar stems                                                     
        # radar.*.txd pattern — any txd name starting with "radar"
        radar_stems = {s for s in all_txd_stems if s.startswith('radar')}

        #    Vehicle stems (cars/weap)                                      
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

        #    Generic stems (SA/SOL)                                          
        generic_sources = {'generic.ide', 'generics.ide'}
        generic_stems = set()
        for obj in objects.values():
            src_base = os.path.basename(obj.source_ide or '').lower()
            if src_base in generic_sources:
                generic_stems.add(obj.txd_name.lower())

        #    Build TXD file name set based on mode                          
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
                from apps.components.Mdl_Editor.mdl_workshop import MDLWorkshop as ModelWorkshop
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

                        #    Write raw TXD                                 
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

                        #    Decode to image formats                        
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


class DATBrowserWidget(QWidget): #vers 3
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
        self._asset_db   = None   # AssetDB for current profile
        self._setup_ui()

    #    UI construction                                                     


    def _get_ui_color(self, key): #vers 1
        """Return theme-aware QColor. No hardcoded colors - everything via app_settings."""
        from PyQt6.QtGui import QColor
        try:
            app_settings = getattr(self, 'app_settings', None) or \
                getattr(getattr(self, 'main_window', None), 'app_settings', None)
            if app_settings and hasattr(app_settings, 'get_ui_color'):
                return app_settings.get_ui_color(key)
        except Exception:
            pass
        pal = self.palette()
        if key == 'viewport_bg':
            return pal.color(pal.ColorRole.Base)
        if key == 'viewport_text':
            return pal.color(pal.ColorRole.PlaceholderText)
        if key == 'border':
            return pal.color(pal.ColorRole.Mid)
        return pal.color(pal.ColorRole.WindowText)

    def _setup_ui(self): #vers 3
        # Ensure opaque background — prevents content bleeding from widgets beneath
        self.setAutoFillBackground(True)
        from PyQt6.QtCore import Qt as _Qt
        self.setAttribute(_Qt.WidgetAttribute.WA_OpaquePaintEvent, True)
        self.setAttribute(_Qt.WidgetAttribute.WA_StyledBackground, True)
        # Apply palette background immediately before any child widgets render
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

        self._db_btn = QPushButton()
        self._db_btn.setFixedSize(28, 24)
        self._db_btn.setIconSize(QSize(18, 18))
        self._db_btn.setToolTip("Asset Database — build/update/query the game asset index")
        self._db_btn.setCheckable(True)
        self._db_btn.clicked.connect(self._toggle_db_panel)
        toolbar.addWidget(self._db_btn)

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

        # Asset DB panel built here, inserted into left panel below tree_hdr
        self._db_panel = self._build_db_panel()
        self._db_panel.setVisible(False)
        self._db_panel.setAutoFillBackground(True)
        self._db_panel.setAttribute(_Qt.WidgetAttribute.WA_OpaquePaintEvent, True)
        self._db_panel.setStyleSheet("background-color: palette(base);")

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
        splitter.setHandleWidth(4)
        splitter.setOpaqueResize(True)
        self._dat_splitter = splitter
        root.addWidget(splitter, 1)
        # Apply theme-aware handle style
        try:
            mw = self.main_window
            if mw and hasattr(mw, 'app_settings'):
                tc = mw.app_settings.get_theme_colors() or {}
                mid = tc.get('splitter_color_background', tc.get('bg_secondary', '#2a2a2a'))
                hov = tc.get('accent_primary', '#1976d2')
                splitter.setStyleSheet(f"""
                    QSplitter::handle:horizontal {{ background: {mid}; width: 4px; border: none; }}
                    QSplitter::handle:horizontal:hover {{ background: {hov}; }}
                """)
        except Exception:
            pass

        # Left — file load-order tree
        left = QWidget()
        left.setAutoFillBackground(True)
        ll = QVBoxLayout(left)
        ll.setContentsMargins(0, 0, 0, 0)
        ll.setSpacing(2)

        #    Load Order toolbar                                             
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
        ll.addWidget(self._db_panel)   # DB panel — hidden until ⬡ DB clicked

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

        # COL DB tab — populated from asset_db when built
        self._col_db_table = self._make_table(
            ["COL File (IMG Entry)", "Model Name", "Model ID",
             "COL Version", "Source IMG"])
        self._col_db_table.doubleClicked.connect(self._on_col_db_double_click)
        self._col_db_table.setToolTip(
            "COL models indexed from all IMG archives.\nDouble-click to open in COL Workshop.")
        self._tabs.addTab(self._col_db_table, "COL DB (0)")

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
            lambda row, col, tbl=t: self._on_ide_cell_double_click(tbl, row, col))
        t.viewport().setAutoFillBackground(True)
        return t

    #    Responsive toolbar                                                  
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

    def resizeEvent(self, event): #vers 2
        super().resizeEvent(event)
        self._update_toolbar_compact(event.size().width())
        if hasattr(self, '_db_panel') and self._db_panel.isVisible():
            self._db_adapt_compact()

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

        # Asset DB button
        try:
            from apps.methods.imgfactory_svg_icons import get_asset_db_icon
            self._db_btn.setIcon(get_asset_db_icon(sz, ic))
        except Exception:
            self._db_btn.setText("DB")

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

        #    Button display                                                
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

        #    Compact threshold                                             
        thresh_box = QGroupBox("Compact mode width threshold")
        thresh_lay = QHBoxLayout(thresh_box)
        thresh_lay.addWidget(QLabel("Collapse to icon-only below:"))
        thresh_spin = QSpinBox()
        thresh_spin.setRange(200, 1200)
        thresh_spin.setValue(getattr(self, '_COMPACT_THRESHOLD', 520))
        thresh_spin.setSuffix(" px")
        thresh_lay.addWidget(thresh_spin)
        lay.addWidget(thresh_box)

        #    Load-order tree options                                       
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

        #    Auto-load                                                     
        auto_box = QGroupBox("Auto-load")
        auto_lay = QVBoxLayout(auto_box)
        cb_auto_load = QCheckBox("Auto-load game assets when game root is set")
        cb_auto_load.setChecked(getattr(self,'_auto_load_on_root', False))
        cb_auto_imgs = QCheckBox("Auto-open all IMGs in IMG Factory after load")
        cb_auto_imgs.setChecked(getattr(self,'_auto_open_imgs', False))
        auto_lay.addWidget(cb_auto_load)
        auto_lay.addWidget(cb_auto_imgs)
        lay.addWidget(auto_box)

        #    Texture Sources                                                
        tex_box = QGroupBox("Texture Sources  (shared with Model Workshop)")
        tex_lay = QVBoxLayout(tex_box)
        tex_lay.addWidget(QLabel(
            "texlist/ folder for pre-exported PNG/IFF/TGA textures. "
            "Leave blank to auto-discover next to loaded DFF files."))
        tl_row = QHBoxLayout()
        tl_row.addWidget(QLabel("texlist/ folder:"))
        self._texlist_edit = QLineEdit()
        self._texlist_edit.setPlaceholderText("(auto-discover if blank)")
        self._texlist_edit.setFixedHeight(24)
        import json as _json
        _mw_cfg = os.path.expanduser('~/.config/imgfactory/model_workshop.json')
        if os.path.isfile(_mw_cfg):
            try:
                self._texlist_edit.setText(
                    _json.load(open(_mw_cfg)).get('texlist_folder',''))
            except Exception:
                pass
        tl_row.addWidget(self._texlist_edit, 1)
        tl_browse = QPushButton("…")
        tl_browse.setFixedSize(28, 24)
        tl_browse.clicked.connect(lambda: (
            lambda f: self._texlist_edit.setText(f) if f else None)(
                QFileDialog.getExistingDirectory(
                    dlg, "Select texlist/ folder",
                    self._texlist_edit.text() or os.path.expanduser('~'))))
        tl_row.addWidget(tl_browse)
        tex_lay.addLayout(tl_row)
        lay.addWidget(tex_box)

        #    Buttons                                                       
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

        # Save texlist folder to model_workshop.json (shared with Model Workshop)
        import json as _json2
        _tl = self._texlist_edit.text().strip()
        _mw_cfg2 = os.path.expanduser('~/.config/imgfactory/model_workshop.json')
        try:
            _d = {}
            if os.path.isfile(_mw_cfg2):
                _d = _json2.load(open(_mw_cfg2))
            _d['texlist_folder'] = _tl
            os.makedirs(os.path.dirname(_mw_cfg2), exist_ok=True)
            _json2.dump(_d, open(_mw_cfg2,'w'), indent=2)
        except Exception:
            pass

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

        #    Browse / load                                                       

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

            # Build IDEDatabase from already-parsed loader.objects so
            # Model Workshop can look up IDE entries without re-reading files.
            # Stored on main_window as .ide_db for global access.
            try:
                from apps.methods.gta_dat_parser import IDEDatabase
                game = self.loader.game
                ide_db = IDEDatabase(game)
                # Populate directly from loader.objects — no disk I/O needed
                for obj in self.loader.objects.values():
                    stem = obj.model_name.lower()
                    ide_db.model_map[stem]       = obj
                    ide_db.id_map[obj.model_id]  = obj
                # Record which IDE files were loaded
                ide_db.source_files = [
                    path for _, etype, path, ok in self.loader.load_log
                    if etype == "IDE" and ok
                ]
                ide_db._loaded = True
                # Attach to main_window for global access
                mw = self.main_window
                if mw:
                    mw.ide_db = ide_db
                self._ide_db = ide_db   # also keep on widget
                n = len(ide_db.model_map)
                f = len(ide_db.source_files)
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(
                        f"IDE DB: {n:,} objects from {f} IDE files  "
                        f"[max_id={ide_db.max_id}]")
            except Exception as e:
                if img_debugger:
                    img_debugger.warning(f"IDE DB build failed: {e}")
        else:
            self._status_lbl.setText("Load failed — see Load Log tab.")
        self._populate_all()
        self._search_edit.setEnabled(True)
        self._type_filter.setEnabled(True)
        if hasattr(self, '_dump_txd_btn'):
            self._dump_txd_btn.setEnabled(bool(self.loader and self.loader.objects))
        self._log_text.setPlainText(self._build_log_text())
        # Enable DB Build button now we have a game root
        if hasattr(self, '_db_build_btn'):
            self._db_build_btn.setEnabled(True)
        # Auto-update DB if already built for this profile
        if hasattr(self, '_db_panel') and self._db_panel.isVisible():
            self._db_load_stats()
        # Auto-open all IMGs if the setting is enabled
        if getattr(self, '_auto_open_imgs', False) and self.loader and self.loader.load_log:
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(800, self._load_all_game_imgs)

    #    Populate                                                            

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

    #    SOL group mapping                                                
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
            # Show model count from DB if available, else blank
            _db = getattr(self, '_asset_db', None)
            if _db is None:
                mw2 = self.main_window
                _db = getattr(mw2, 'asset_db', None) if mw2 else None
            if _db and success:
                try:
                    _bname = os.path.basename(path)
                    _n = _db._con.execute(
                        "SELECT COUNT(*) FROM col_entries WHERE entry_name=?",
                        (_bname,)).fetchone()[0]
                    count_str = str(_n) if _n else ""
                except Exception:
                    count_str = ""
            else:
                count_str = ""

        label = ("IMG" if entry_type == "IMG"
                 else "CDIMAGE" if entry_type == "CDIMAGE"
                 else "COL"     if entry_type == "COLFILE"
                 else entry_type)
        tag   = f" [{phase}]" if phase == "enforced" else ""
        if not success:
            status = "✗ missing"
        else:
            # Check if this file is indexed in the asset DB
            db = getattr(self, '_asset_db', None)
            if db is None:
                mw = self.main_window
                db = getattr(mw, 'asset_db', None) if mw else None
            if db and db.stats().get('source_files', 0) > 0:
                # Check if path is tracked as a source file
                row = db._con.execute(
                    "SELECT id FROM source_files WHERE path=?",
                    (path,)).fetchone()
                if row:
                    status = "● in DB"
                elif entry_type == "COLFILE":
                    # COL may be indexed via IMG even if not a source_file itself
                    bname2 = os.path.basename(path)
                    row2 = db._con.execute(
                        "SELECT COUNT(*) FROM col_entries WHERE entry_name=?",
                        (bname2,)).fetchone()
                    status = "● in DB" if row2 and row2[0] > 0 else "✓"
                else:
                    status = "✓"
            else:
                status = "✓"

        child = QTreeWidgetItem([bname + tag, label, count_str, status])
        child.setData(0, Qt.ItemDataRole.UserRole, path)
        if not success:
            for col in range(4):
                child.setForeground(col, self._get_ui_color('error') if hasattr(self,'_get_ui_color') else QColor(204,68,68))
            if entry_type == "COLFILE":
                child.setToolTip(0, f"Not found: {path}")
        elif status == "● in DB":
            child.setForeground(3, QColor("#4ade80"))   # green dot for DB entries
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
        lines = [self.loader.get_summary(), "", "   Load order   "]
        for phase, entry_type, path, success in self.loader.load_log:
            mark = "✓" if success else "✗ MISSING"
            lines.append(f"  [{entry_type}] {mark}  {path}")
        if self.loader.stats.warnings:
            lines += ["", f"   Warnings ({len(self.loader.stats.warnings)})   "]
            lines += [f"  {w}" for w in self.loader.stats.warnings[:100]]
            if len(self.loader.stats.warnings) > 100:
                lines.append(f"  … {len(self.loader.stats.warnings)-100} more")
        if self.loader.stats.errors:
            lines += ["", f"   Errors ({len(self.loader.stats.errors)})   "]
            lines += [f"  {e}" for e in self.loader.stats.errors]
        return "\n".join(lines)

    #    Filter                                                              

    def _apply_filter(self): #vers 1
        if not self.loader.objects:
            return
        self._populate_objects(
            self._search_edit.text(),
            self._type_filter.currentText())
        self._populate_instances(self._search_edit.text())

    #    Tree click — filter to selected file                                

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

    def _on_ide_cell_double_click(self, table, row, col): #vers 1
        """Route double-click to the right workshop based on column.
        Col 0 (ID)       → Model Workshop (DFF + TXD)
        Col 1 (Model)    → Model Workshop (DFF + TXD)
        Col 2 (TXD)      → TXD Workshop only
        Col 3-7 (others) → Model Workshop (DFF + TXD)
        """
        if col == 2:
            # TXD column — open just the TXD in TXD Workshop
            self._open_txd_only_from_row(table, row)
        else:
            # Any other column — open Model Workshop with DFF + TXD
            self._on_ide_row_double_click(table, row)

    def _open_txd_only_from_row(self, table, row): #vers 1
        """Open TXD Workshop for the TXD referenced by this IDE row."""
        txd_name = table.item(row, 2).text().strip() if table.item(row, 2) else ''
        found    = self._get_row_xref(table, row)
        txd_img  = found.get('txd')
        txd_stem = (found.get('txd_name') or txd_name).lower()
        mw = self.main_window
        if not txd_img:
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(f"TXD not found: {txd_stem}.txd")
            return
        txd_tmp = self._extract_entry_to_temp(txd_img, txd_stem + '.txd')
        if not txd_tmp:
            return
        try:
            from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
            open_txd_workshop(mw, txd_tmp)
            self._log(f"TXD Workshop: {txd_stem}.txd")
        except ImportError:
            # Fallback: open in Model Workshop texture panel
            self._open_model_workshop_for_row(table, row)
            self._log(f"TXD Workshop not available — opened in Model Workshop")

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
        from apps.components.Mdl_Editor.mdl_workshop import open_model_workshop
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

    #    Context menu                                                        

    def _table_context_menu(self, table, pos): #vers 3
        """Right-click menu on IDE Objects table.
        Full feature set — replaces IDE Editor:
          Model    : Open in Model Workshop, Export DFF, Replace DFF, Rename
          Texture  : Open in TXD Workshop, Export TXD, Replace TXD
          IDE edit : Edit draw distance, Edit flags, Add entry, Delete entry
          IDs      : Scan free IDs (range highlight), Copy ID
          Misc     : Find instances, Copy name/row, Dump TXDs
        """
        index = table.indexAt(pos)
        if not index.isValid():
            return
        row = index.row()

        # Gather row data
        def _txt(col):
            it = table.item(row, col)
            return it.text().strip() if it else ''

        model_name = _txt(1)
        txd_name   = _txt(2)
        obj_type   = _txt(3)
        draw_dist  = _txt(5)
        flags_txt  = _txt(6)
        source_ide = _txt(7)
        model_id_s = _txt(0)
        try:
            model_id = int(model_id_s)
        except ValueError:
            model_id = -1

        # Multi-selection info
        sel_rows = sorted({i.row() for i in table.selectedItems()})
        multi    = len(sel_rows) > 1

        is_ide_table = (table is self._obj_table)

        menu = QMenu(self)

        # ── Show / open ─────────────────────────────────────────────────────
        if is_ide_table:
            show_mw_act  = menu.addAction(f"⏎  Show in Model Workshop  [{model_name}]")
            if txd_name and txd_name not in ('', '—', 'null'):
                show_txd_act = menu.addAction(f"⏎  Show in TXD Workshop  [{txd_name}]")
            else:
                show_txd_act = None
            menu.addSeparator()

            # ── DFF ─────────────────────────────────────────────────────────
            export_dff_act  = menu.addAction(f"Export DFF…  [{model_name}.dff]")
            replace_dff_act = menu.addAction(f"Replace DFF…  [{model_name}.dff]")
            rename_act      = menu.addAction(f"Rename model…  [{model_name}]")
            menu.addSeparator()

            # ── TXD ─────────────────────────────────────────────────────────
            if txd_name and txd_name not in ('', '—', 'null'):
                open_txd_act    = menu.addAction(f"Open in TXD Workshop  [{txd_name}.txd]")
                export_txd_act  = menu.addAction(f"Export TXD…  [{txd_name}.txd]")
                replace_txd_act = menu.addAction(f"Replace TXD…  [{txd_name}.txd]")
            else:
                open_txd_act = export_txd_act = replace_txd_act = None
            menu.addSeparator()

            # ── IDE edit ────────────────────────────────────────────────────
            dd_act    = menu.addAction("Edit draw distance…")
            flags_act = menu.addAction("Edit flags…")
            txdn_act  = menu.addAction("Edit TXD name…")
            add_act   = menu.addAction("Add new IDE entry…")
            del_act   = menu.addAction(
                f"Delete {len(sel_rows)} selected entries" if multi else "Delete entry")
            menu.addSeparator()

            # ── ID tools ────────────────────────────────────────────────────
            copy_id_act  = menu.addAction(f"Copy ID  [{model_id}]")
            scan_ids_act = menu.addAction("Scan free IDs (0–32767)…")
            menu.addSeparator()

        else:
            show_mw_act = show_txd_act = None
            export_dff_act = replace_dff_act = rename_act = None
            open_txd_act = export_txd_act = replace_txd_act = None
            dd_act = flags_act = txdn_act = add_act = del_act = None
            copy_id_act = scan_ids_act = None

        # ── Common ──────────────────────────────────────────────────────────
        copy_name_act = menu.addAction("Copy model name")
        copy_row_act  = menu.addAction("Copy row as text")
        menu.addSeparator()
        find_inst_act = menu.addAction("Find all instances of this model")
        if is_ide_table:
            menu.addSeparator()
            dump_sel_act = menu.addAction(
                f"Extract TXDs for {len(sel_rows)} selected rows…") if multi else None
            dump_all_act = menu.addAction("Dump ALL game TXDs to folder…")
        else:
            dump_sel_act = dump_all_act = None

        # ── Execute ─────────────────────────────────────────────────────────
        chosen = menu.exec(table.viewport().mapToGlobal(pos))
        if not chosen:
            return

        # Show / open
        if chosen == show_mw_act:
            self._open_model_workshop_for_row(table, row)
        elif show_txd_act and chosen == show_txd_act:
            self._open_txd_only_from_row(table, row)
        # DFF actions
        elif chosen == export_dff_act:
            self._export_dff_from_row(table, row)
        elif chosen == replace_dff_act:
            self._replace_dff_in_img(table, row)
        elif chosen == rename_act:
            self._rename_ide_model(table, row)
        # TXD actions
        elif open_txd_act and chosen == open_txd_act:
            self._open_txd_only_from_row(table, row)
        elif export_txd_act and chosen == export_txd_act:
            self._export_txd_from_row(table, row)
        elif replace_txd_act and chosen == replace_txd_act:
            self._replace_txd_in_img(table, row)
        # IDE edit actions
        elif chosen == dd_act:
            self._edit_draw_distance(table, row)
        elif chosen == flags_act:
            self._edit_flags(table, row)
        elif chosen == txdn_act:
            self._edit_txd_name(table, row)
        elif chosen == add_act:
            self._add_ide_entry_dialog(table)
        elif chosen == del_act:
            self._delete_ide_entries(table, sel_rows)
        # ID tools
        elif chosen == copy_id_act:
            QApplication.clipboard().setText(str(model_id))
        elif chosen == scan_ids_act:
            self._scan_free_ids_dialog()
        # Common
        elif chosen == copy_name_act:
            QApplication.clipboard().setText(model_name)
        elif chosen == copy_row_act:
            parts = [_txt(c) for c in range(table.columnCount())]
            QApplication.clipboard().setText("\t".join(parts))
        elif chosen == find_inst_act:
            self._search_edit.setText(model_name)
            self._tabs.setCurrentIndex(1)
        elif dump_sel_act and chosen == dump_sel_act:
            self._dump_selected_txds(table)
        elif dump_all_act and chosen == dump_all_act:
            self._dump_all_game_txds()

    # ── Context menu action helpers ─────────────────────────────────────────

    def _get_row_xref(self, table, row):
        """Resolve DFF/TXD IMG paths for a row using xref. Returns dict."""
        model_name = table.item(row, 1).text().strip() if table.item(row, 1) else ''
        if not self.loader or not self.xref or not model_name:
            return {}
        return self.xref.find_in_imgs(model_name, self.loader.load_log,
                                       getattr(self.xref, 'game_root', ''))

    def _extract_entry_to_temp(self, img_path, entry_name):
        """Extract a named entry from an IMG to a tempfile. Returns path or None."""
        import tempfile, os
        try:
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(img_path); arc.open()
            entry = next((e for e in arc.entries
                          if e.name.lower() == entry_name.lower()), None)
            if not entry:
                return None
            data = arc.read_entry_data(entry)
            ext  = os.path.splitext(entry_name)[1]
            tmp  = tempfile.NamedTemporaryFile(
                delete=False, suffix=ext,
                prefix=os.path.splitext(entry_name)[0] + '_')
            tmp.write(data); tmp.close()
            return tmp.name
        except Exception as e:
            self._log(f"Extract error: {e}")
            return None

    def _open_model_workshop_for_row(self, table, row): #vers 1
        """Open DFF + TXD in Model Workshop from IDE row."""
        model_name = table.item(row, 1).text().strip() if table.item(row, 1) else ''
        txd_name   = table.item(row, 2).text().strip() if table.item(row, 2) else ''
        found = self._get_row_xref(table, row)
        dff_img = found.get('dff')
        txd_img = found.get('txd')
        if not dff_img:
            QMessageBox.warning(self, "Not found",
                f"{model_name}.dff not found in any loaded IMG.")
            return
        dff_tmp = self._extract_entry_to_temp(dff_img, model_name + '.dff')
        if not dff_tmp:
            return
        from apps.components.Mdl_Editor.mdl_workshop import open_model_workshop
        mw = self.main_window
        workshop = open_model_workshop(mw, dff_tmp)
        if workshop and txd_img:
            txd_stem = (found.get('txd_name') or txd_name or model_name).lower()
            txd_tmp = self._extract_entry_to_temp(txd_img, txd_stem + '.txd')
            if txd_tmp and hasattr(workshop, '_load_txd_file'):
                workshop._load_txd_file(txd_tmp)
        self._log(f"Model Workshop: {model_name}.dff")

    def _export_dff_from_row(self, table, row): #vers 1
        """Export model's DFF from IMG to a user-chosen location."""
        model_name = table.item(row, 1).text().strip() if table.item(row, 1) else ''
        found = self._get_row_xref(table, row)
        dff_img = found.get('dff')
        if not dff_img:
            QMessageBox.warning(self, "Not found",
                f"{model_name}.dff not found in any loaded IMG.")
            return
        out_path, _ = QFileDialog.getSaveFileName(
            self, "Export DFF", model_name + '.dff',
            "RenderWare DFF (*.dff);;All files (*)")
        if not out_path:
            return
        try:
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(dff_img); arc.open()
            entry = next((e for e in arc.entries
                          if e.name.lower() == model_name.lower() + '.dff'), None)
            if not entry:
                QMessageBox.warning(self, "Not found", f"DFF entry not in IMG.")
                return
            data = arc.read_entry_data(entry)
            with open(out_path, 'wb') as f:
                f.write(data)
            self._log(f"Exported DFF: {out_path}")
        except Exception as e:
            QMessageBox.critical(self, "Export Error", str(e))

    def _export_txd_from_row(self, table, row): #vers 1
        """Export TXD from IMG to a user-chosen location."""
        txd_name = table.item(row, 2).text().strip() if table.item(row, 2) else ''
        found = self._get_row_xref(table, row)
        txd_img  = found.get('txd')
        txd_stem = (found.get('txd_name') or txd_name).lower()
        if not txd_img:
            QMessageBox.warning(self, "Not found",
                f"{txd_stem}.txd not found in any loaded IMG.")
            return
        out_path, _ = QFileDialog.getSaveFileName(
            self, "Export TXD", txd_stem + '.txd',
            "TXD (*.txd);;All files (*)")
        if not out_path:
            return
        try:
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(txd_img); arc.open()
            entry = next((e for e in arc.entries
                          if e.name.lower() == txd_stem + '.txd'), None)
            if not entry:
                QMessageBox.warning(self, "Not found", "TXD entry not in IMG.")
                return
            data = arc.read_entry_data(entry)
            with open(out_path, 'wb') as f:
                f.write(data)
            self._log(f"Exported TXD: {out_path}")
        except Exception as e:
            QMessageBox.critical(self, "Export Error", str(e))

    def _replace_dff_in_img(self, table, row): #vers 1
        """Replace a model's DFF in its IMG archive with a user-supplied file."""
        model_name = table.item(row, 1).text().strip() if table.item(row, 1) else ''
        found   = self._get_row_xref(table, row)
        dff_img = found.get('dff')
        if not dff_img:
            QMessageBox.warning(self, "Not found",
                f"{model_name}.dff not found in any loaded IMG.")
            return
        src, _ = QFileDialog.getOpenFileName(
            self, f"Replace {model_name}.dff",
            "", "RenderWare DFF (*.dff);;All files (*)")
        if not src:
            return
        try:
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(dff_img); arc.open()
            with open(src, 'rb') as f:
                data = f.read()
            arc.replace_entry(model_name + '.dff', data)
            arc.save()
            self._log(f"Replaced DFF: {model_name}.dff in {os.path.basename(dff_img)}")
            QMessageBox.information(self, "Done",
                f"Replaced {model_name}.dff in {os.path.basename(dff_img)}")
        except Exception as e:
            QMessageBox.critical(self, "Replace Error", str(e))

    def _replace_txd_in_img(self, table, row): #vers 1
        """Replace a TXD in its IMG archive with a user-supplied file."""
        txd_name = table.item(row, 2).text().strip() if table.item(row, 2) else ''
        found    = self._get_row_xref(table, row)
        txd_img  = found.get('txd')
        txd_stem = (found.get('txd_name') or txd_name).lower()
        if not txd_img:
            QMessageBox.warning(self, "Not found",
                f"{txd_stem}.txd not found in any loaded IMG.")
            return
        src, _ = QFileDialog.getOpenFileName(
            self, f"Replace {txd_stem}.txd",
            "", "TXD (*.txd);;All files (*)")
        if not src:
            return
        try:
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(txd_img); arc.open()
            with open(src, 'rb') as f:
                data = f.read()
            arc.replace_entry(txd_stem + '.txd', data)
            arc.save()
            self._log(f"Replaced TXD: {txd_stem}.txd in {os.path.basename(txd_img)}")
            QMessageBox.information(self, "Done",
                f"Replaced {txd_stem}.txd in {os.path.basename(txd_img)}")
        except Exception as e:
            QMessageBox.critical(self, "Replace Error", str(e))

    def _rename_ide_model(self, table, row): #vers 1
        """Inline-rename the model name in the IDE table (in-memory)."""
        from PyQt6.QtWidgets import QInputDialog
        model_name = table.item(row, 1).text().strip() if table.item(row, 1) else ''
        new_name, ok = QInputDialog.getText(
            self, "Rename model", "New model name:", text=model_name)
        if not ok or not new_name.strip() or new_name.strip() == model_name:
            return
        new_name = new_name.strip().lower()
        table.item(row, 1).setText(new_name)
        # Update in-memory loader objects
        try:
            model_id = int(table.item(row, 0).text())
            obj = self.loader.objects.get(model_id)
            if obj:
                obj.model_name = new_name
        except Exception:
            pass
        self._log(f"Renamed IDE entry: {model_name} → {new_name}  (in-memory, save IDE to persist)")

    def _edit_draw_distance(self, table, row): #vers 1
        """Edit draw distance for the selected IDE entry."""
        from PyQt6.QtWidgets import QInputDialog
        cur = table.item(row, 5).text().strip() if table.item(row, 5) else ''
        try:
            cur_val = float(cur) if cur else 300.0
        except ValueError:
            cur_val = 300.0
        val, ok = QInputDialog.getDouble(
            self, "Edit Draw Distance",
            "Draw distance (world units):",
            value=cur_val, min=0.0, max=10000.0, decimals=2)
        if not ok:
            return
        table.item(row, 5).setText(f"{val:.2f}")
        try:
            model_id = int(table.item(row, 0).text())
            obj = self.loader.objects.get(model_id)
            if obj:
                obj.extra['draw_dist'] = val
        except Exception:
            pass
        self._log(f"Draw distance updated: {table.item(row, 1).text()} → {val:.2f}")

    def _edit_flags(self, table, row): #vers 1
        """Edit flags for the selected IDE entry."""
        from PyQt6.QtWidgets import QInputDialog
        cur = table.item(row, 6).text().strip() if table.item(row, 6) else '0'
        try:
            cur_val = int(cur) if cur else 0
        except ValueError:
            cur_val = 0
        val, ok = QInputDialog.getInt(
            self, "Edit Flags",
            "Flags (integer):", value=cur_val, min=0, max=0xFFFF)
        if not ok:
            return
        table.item(row, 6).setText(str(val))
        try:
            model_id = int(table.item(row, 0).text())
            obj = self.loader.objects.get(model_id)
            if obj:
                obj.extra['flags'] = val
        except Exception:
            pass
        self._log(f"Flags updated: {table.item(row, 1).text()} → {val}")

    def _edit_txd_name(self, table, row): #vers 1
        """Edit the TXD name for an IDE entry."""
        from PyQt6.QtWidgets import QInputDialog
        cur = table.item(row, 2).text().strip() if table.item(row, 2) else ''
        new_txd, ok = QInputDialog.getText(
            self, "Edit TXD Name", "TXD name:", text=cur)
        if not ok or new_txd.strip() == cur:
            return
        new_txd = new_txd.strip().lower()
        table.item(row, 2).setText(new_txd)
        try:
            model_id = int(table.item(row, 0).text())
            obj = self.loader.objects.get(model_id)
            if obj:
                obj.txd_name = new_txd
        except Exception:
            pass
        self._log(f"TXD name updated: {table.item(row, 1).text()} → {new_txd}")

    def _add_ide_entry_dialog(self, table): #vers 1
        """Dialog to add a new IDE entry to the in-memory world."""
        from PyQt6.QtWidgets import (QDialog, QFormLayout, QLineEdit,
                                     QSpinBox, QDoubleSpinBox, QComboBox,
                                     QDialogButtonBox)
        dlg = QDialog(self)
        dlg.setWindowTitle("Add IDE Entry")
        dlg.setMinimumWidth(340)
        form = QFormLayout(dlg)

        id_spin   = QSpinBox(); id_spin.setRange(0, 32767)
        # Suggest next free ID
        used = set(self.loader.objects.keys()) if self.loader else set()
        next_id = next((i for i in range(1987, 32768) if i not in used), 1987)
        id_spin.setValue(next_id)

        name_edit = QLineEdit(); name_edit.setPlaceholderText("model_name")
        txd_edit  = QLineEdit(); txd_edit.setPlaceholderText("txd_name")
        type_combo= QComboBox()
        type_combo.addItems(["object", "vehicle", "ped", "weapon", "hierarchy"])
        dd_spin   = QDoubleSpinBox(); dd_spin.setRange(0, 10000); dd_spin.setValue(300)
        flags_spin= QSpinBox(); flags_spin.setRange(0, 0xFFFF); flags_spin.setValue(0)

        form.addRow("ID:",           id_spin)
        form.addRow("Model name:",   name_edit)
        form.addRow("TXD name:",     txd_edit)
        form.addRow("Type:",         type_combo)
        form.addRow("Draw dist:",    dd_spin)
        form.addRow("Flags:",        flags_spin)

        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok |
            QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        form.addRow(btns)

        if dlg.exec() != QDialog.DialogCode.Accepted:
            return

        new_id   = id_spin.value()
        new_name = name_edit.text().strip().lower()
        new_txd  = txd_edit.text().strip().lower()
        if not new_name:
            return

        # Add to loader objects
        from apps.methods.gta_dat_parser import IDEObject
        obj = IDEObject(
            model_id   = new_id,
            model_name = new_name,
            txd_name   = new_txd,
            obj_type   = type_combo.currentText(),
            section    = type_combo.currentText(),
            extra      = {'draw_dist': dd_spin.value(), 'flags': flags_spin.value()},
            source_ide = '',
        )
        if self.loader:
            self.loader.objects[new_id] = obj

        # Add row to table
        r = table.rowCount()
        table.insertRow(r)
        for col, val in enumerate([
            str(new_id), new_name, new_txd,
            type_combo.currentText(), type_combo.currentText(),
            f"{dd_spin.value():.2f}", str(flags_spin.value()), ''
        ]):
            item = QTableWidgetItem(val)
            item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            table.setItem(r, col, item)

        self._log(f"Added IDE entry: ID={new_id} {new_name} txd={new_txd}")

    def _delete_ide_entries(self, table, rows): #vers 1
        """Delete selected IDE entries from table and in-memory loader."""
        from PyQt6.QtWidgets import QMessageBox
        if not rows:
            return
        n = len(rows)
        names = [table.item(r, 1).text() if table.item(r, 1) else '?' for r in rows[:5]]
        preview = ', '.join(names) + (f'  … and {n-5} more' if n > 5 else '')
        reply = QMessageBox.question(
            self, "Delete entries",
            f"Delete {n} IDE entry/entries?\n{preview}",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
        # Remove from loader
        for r in rows:
            try:
                mid = int(table.item(r, 0).text())
                if self.loader and mid in self.loader.objects:
                    del self.loader.objects[mid]
            except Exception:
                pass
        # Remove rows in reverse order so indices don't shift
        for r in sorted(rows, reverse=True):
            table.removeRow(r)
        self._log(f"Deleted {n} IDE entries")

    def _scan_free_ids_dialog(self): #vers 1
        """Show a dialog scanning free IDs from 0–32767.
        Highlights used vs free, shows largest free blocks.
        Lets user set a range to scan."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout,
            QLabel, QSpinBox, QPushButton, QTableWidget, QTableWidgetItem,
            QHeaderView, QDialogButtonBox, QTextEdit)
        from PyQt6.QtGui import QColor

        if not self.loader:
            return

        used_ids = set(self.loader.objects.keys())

        dlg = QDialog(self)
        dlg.setWindowTitle("Free ID Scanner  (0 – 32767)")
        dlg.setMinimumSize(560, 500)
        root = QVBoxLayout(dlg)

        # Range controls
        ctrl = QHBoxLayout()
        ctrl.addWidget(QLabel("From:"))
        from_spin = QSpinBox(); from_spin.setRange(0, 32767); from_spin.setValue(1987)
        ctrl.addWidget(from_spin)
        ctrl.addWidget(QLabel("To:"))
        to_spin = QSpinBox(); to_spin.setRange(0, 32767); to_spin.setValue(32767)
        ctrl.addWidget(to_spin)
        scan_btn = QPushButton("Scan"); scan_btn.setFixedWidth(72)
        ctrl.addWidget(scan_btn)
        ctrl.addStretch()
        root.addLayout(ctrl)

        # Summary text
        summary_lbl = QLabel()
        root.addWidget(summary_lbl)

        # Results table
        tbl = QTableWidget(0, 3)
        tbl.setHorizontalHeaderLabels(["ID", "Status", "Block size"])
        tbl.horizontalHeader().setSectionResizeMode(0, QHeaderView.ResizeMode.ResizeToContents)
        tbl.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        tbl.horizontalHeader().setStretchLastSection(True)
        tbl.setAlternatingRowColors(True)
        tbl.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        tbl.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        root.addWidget(tbl, 1)

        def _run_scan():
            lo = from_spin.value()
            hi = to_spin.value()
            if lo > hi:
                lo, hi = hi, lo

            # Build block list
            blocks = []   # (start, end, is_free)
            i = lo
            while i <= hi:
                is_free = i not in used_ids
                start   = i
                while i <= hi and (i not in used_ids) == is_free:
                    i += 1
                blocks.append((start, i - 1, is_free))

            # Populate table — only show FREE blocks + single used sentinel rows
            tbl.setSortingEnabled(False)
            tbl.setRowCount(0)
            free_total = 0
            used_total = 0
            for (bstart, bend, is_free) in blocks:
                blen = bend - bstart + 1
                if is_free:
                    free_total += blen
                    r = tbl.rowCount()
                    tbl.insertRow(r)
                    lbl_id   = (f"{bstart}" if blen == 1
                                else f"{bstart} – {bend}")
                    lbl_stat = "FREE"
                    item_id   = QTableWidgetItem(lbl_id)
                    item_stat = QTableWidgetItem(lbl_stat)
                    item_blk  = QTableWidgetItem(f"{blen} slot{'s' if blen>1 else ''}")
                    item_stat.setForeground(QColor("#4ade80"))  # green
                    item_id.setForeground(QColor("#4ade80"))
                    tbl.setItem(r, 0, item_id)
                    tbl.setItem(r, 1, item_stat)
                    tbl.setItem(r, 2, item_blk)
                else:
                    used_total += blen
                    # Show used blocks as a single greyed row
                    r = tbl.rowCount()
                    tbl.insertRow(r)
                    lbl_id   = (f"{bstart}" if blen == 1
                                else f"{bstart} – {bend}")
                    item_id   = QTableWidgetItem(lbl_id)
                    item_stat = QTableWidgetItem(f"USED ({blen})")
                    item_blk  = QTableWidgetItem("—")
                    for item in (item_id, item_stat, item_blk):
                        item.setForeground(QColor("#888"))
                    tbl.setItem(r, 0, item_id)
                    tbl.setItem(r, 1, item_stat)
                    tbl.setItem(r, 2, item_blk)

            tbl.setSortingEnabled(True)
            range_total = hi - lo + 1
            summary_lbl.setText(
                f"Range {lo}–{hi}  |  "
                f"Free: {free_total:,}  ({100*free_total//range_total}%)  |  "
                f"Used: {used_total:,}  |  "
                f"Total IDs loaded: {len(used_ids):,}")

        scan_btn.clicked.connect(_run_scan)
        _run_scan()   # auto-scan on open

        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Close)
        btns.rejected.connect(dlg.close)
        root.addWidget(btns)
        dlg.exec()

    def _log(self, msg: str): #vers 1
        """Log a message to main_window and load log."""
        mw = getattr(self, 'main_window', None)
        if mw and hasattr(mw, 'log_message'):
            mw.log_message(msg)
        self._set_status(msg)

    #    Tree right-click — open source file in editor                      


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

    def _on_theme_changed(self): #vers 5
        """Refresh DAT-specific row colors when theme switches."""
        self._apply_theme_stylesheet()
        self.setStyleSheet(self.styleSheet())  # force repaint
        # Re-apply splitter handle style
        try:
            if hasattr(self, '_dat_splitter') and self.main_window:
                tc = self.main_window.app_settings.get_theme_colors() or {}
                mid = tc.get('splitter_color_background', tc.get('bg_secondary', '#2a2a2a'))
                hov = tc.get('accent_primary', '#1976d2')
                self._dat_splitter.setStyleSheet(f"""
                    QSplitter::handle:horizontal {{ background: {mid}; width: 4px; border: none; }}
                    QSplitter::handle:horizontal:hover {{ background: {hov}; }}
                """)
        except Exception:
            pass
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
                                    # STUB: auto-select TXD entry after workshop open
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

        #    IMG / CDIMAGE specific options                                
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

    #    Public API                                                          

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


#                                                                              
# Integration hook
#                                                                              


    #    Asset Database panel                                                 

    _BUILTIN_PROFILES = ['GTASOL', 'GTA3', 'VC', 'SA']

    def _build_db_panel(self): #vers 2
        """Build the collapsible Asset DB panel widget.
        Buttons use SVG icons. When panel is narrow (<400px)
        labels are hidden and only icons + tooltips are shown."""
        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout,
            QComboBox, QPushButton, QLabel, QProgressBar,
            QInputDialog, QMessageBox, QSizePolicy)
        from PyQt6.QtCore import Qt as _Qt

        panel = QWidget()
        panel.setObjectName("db_panel")
        panel.setMaximumHeight(120)
        pl = QVBoxLayout(panel)
        pl.setContentsMargins(4, 4, 4, 4)
        pl.setSpacing(3)

        #    Helper: icon button                                         
        def _ibtn(label, tip, slot, icon_fn_name,
                  enabled=True, min_w=80, h=26):
            b = QPushButton(label)
            b.setFixedHeight(h)
            b.setMinimumWidth(min_w)
            b.setToolTip(tip)
            b.setEnabled(enabled)
            b.clicked.connect(slot)
            b.setIconSize(QSize(16, 16))
            b._db_label   = label       # remember for compact mode
            b._icon_fn    = icon_fn_name
            return b

        #    Row 1: profile selector + New / Del                        
        row1 = QHBoxLayout(); row1.setSpacing(4)

        self._db_profile_lbl = QLabel("Profile:")
        row1.addWidget(self._db_profile_lbl)

        self._db_profile_combo = QComboBox()
        self._db_profile_combo.setFixedHeight(26)
        self._db_profile_combo.setMinimumWidth(90)
        self._db_profile_combo.setToolTip(
            "Select which game database to use.\n"
            "Each profile stores its own asset index.")
        self._db_profile_combo.currentTextChanged.connect(
            self._on_db_profile_changed)
        row1.addWidget(self._db_profile_combo)

        self._db_new_btn = _ibtn(
            "New", "Create a new named database profile",
            self._db_new_profile, "get_db_new_icon", min_w=46)
        row1.addWidget(self._db_new_btn)

        self._db_del_btn = _ibtn(
            "Del", "Delete selected database profile",
            self._db_delete_profile, "get_db_delete_icon", min_w=40)
        row1.addWidget(self._db_del_btn)

        row1.addStretch()

        self._db_build_btn = _ibtn(
            "Build DB",
            "Index all IMG and IDE files in the current game root.\n"
            "COL model names and TXD texture names are extracted\n"
            "without full decoding — fast lightweight scan.",
            self._db_build, "get_db_build_icon",
            enabled=False, min_w=80)
        row1.addWidget(self._db_build_btn)

        self._db_update_btn = _ibtn(
            "Update",
            "Re-index only files that have changed since last build.",
            self._db_update, "get_db_update_icon",
            enabled=False, min_w=72)
        row1.addWidget(self._db_update_btn)

        pl.addLayout(row1)

        # Load icons after widget is constructed
        from PyQt6.QtCore import QTimer as _QT3
        _QT3.singleShot(0, self._db_load_panel_icons)

        #    Row 2: stats label                                          
        self._db_stats_lbl = QLabel("No database loaded.")
        self._db_stats_lbl.setStyleSheet(
            "font-size: 10px; color: palette(mid); font-style: italic;")
        pl.addWidget(self._db_stats_lbl)

        #    Row 3: progress bar (hidden when idle)                      
        self._db_progress = QProgressBar()
        self._db_progress.setFixedHeight(12)
        self._db_progress.setTextVisible(True)
        self._db_progress.setVisible(False)
        pl.addWidget(self._db_progress)

        # Populate profile list
        self._db_refresh_profile_list()

        return panel

    def _db_load_panel_icons(self): #vers 1
        """Load SVG icons onto all DB panel buttons, themed to current palette."""
        try:
            from apps.methods.imgfactory_svg_icons import (
                get_db_build_icon, get_db_update_icon,
                get_db_new_icon, get_db_delete_icon)
            ic = self._get_icon_color()
            sz = 16
            pairs = [
                (self._db_build_btn,  get_db_build_icon),
                (self._db_update_btn, get_db_update_icon),
                (self._db_new_btn,    get_db_new_icon),
                (self._db_del_btn,    get_db_delete_icon),
            ]
            for btn, fn in pairs:
                btn.setIcon(fn(sz, ic))
        except Exception:
            pass
        self._db_adapt_compact()

    def _db_adapt_compact(self): #vers 1
        """Switch DB panel buttons between icon+label and icon-only
        based on available panel width. Threshold: 420px."""
        compact = self._db_panel.width() < 420
        btns = [
            (self._db_build_btn,  "Build DB"),
            (self._db_update_btn, "Update"),
            (self._db_new_btn,    "New"),
            (self._db_del_btn,    "Del"),
        ]
        self._db_profile_lbl.setVisible(not compact)
        for btn, label in btns:
            btn.setText("" if compact else label)
            btn.setMinimumWidth(28 if compact else 60)
            btn.setFixedWidth(28 if compact else btn.sizeHint().width() + 8)

    def _toggle_db_panel(self): #vers 3
        if self._db_panel is None: return
        visible = self._db_panel.isVisible()
        self._db_panel.setVisible(not visible)
        self._db_panel.repaint()
        self._db_panel.raise_()
        self._db_btn.setChecked(not visible)
        if not visible:
            self._db_refresh_profile_list()
            self._db_load_stats()
        # Force full repaint so no ghost pixels from the panel remain
        self.update()
        self.repaint()

    def _db_refresh_profile_list(self): #vers 1
        """Reload profile dropdown from saved DBs + builtins."""
        from apps.methods.asset_db import AssetDB
        current = self._db_profile_combo.currentText()
        saved   = AssetDB.list_profiles()
        # Merge builtins with saved, deduplicate, sort
        all_p   = sorted(set(self._BUILTIN_PROFILES) | set(saved))
        self._db_profile_combo.blockSignals(True)
        self._db_profile_combo.clear()
        self._db_profile_combo.addItems(all_p)
        # Auto-select profile matching current game
        game_map = {'GTA3':'GTA3','VC':'VC','SA':'SA','SOL':'GTASOL'}
        from apps.methods.gta_dat_parser import GTAGame
        game_name = game_map.get(
            getattr(self.loader, 'game', GTAGame.SOL), 'GTASOL')
        if current in all_p:
            self._db_profile_combo.setCurrentText(current)
        elif game_name in all_p:
            self._db_profile_combo.setCurrentText(game_name)
        self._db_profile_combo.blockSignals(False)
        self._db_profile_combo.currentTextChanged.emit(
            self._db_profile_combo.currentText())

    def _on_db_profile_changed(self, profile: str): #vers 1
        self._asset_db = None   # clear cached DB object
        self._db_load_stats()

    def _get_asset_db(self): #vers 1
        """Return AssetDB for the current profile, creating if needed."""
        from apps.methods.asset_db import AssetDB
        profile = self._db_profile_combo.currentText() or 'GTASOL'
        cached  = getattr(self, '_asset_db', None)
        if cached and getattr(cached, 'profile', '') == profile:
            return cached
        db = AssetDB(profile)
        self._asset_db = db
        # Expose on main_window for global access
        mw = self.main_window
        if mw:
            mw.asset_db = db
        return db

    def _db_load_stats(self): #vers 1
        """Update the stats label from the current DB."""
        try:
            db = self._get_asset_db()
            s  = db.stats()
            if s['source_files'] == 0:
                self._db_stats_lbl.setText(
                    "Database is empty — click Build DB to index game files.")
                self._db_update_btn.setEnabled(False)
            else:
                self._db_stats_lbl.setText(
                    f"  {s['img_entries']:,} IMG entries  ·  "
                    f"{s['col_entries']:,} COL models  ·  "
                    f"{s['txd_entries']:,} textures  ·  "
                    f"{s['ide_entries']:,} IDE objects  "
                    f"({s['source_files']} files indexed)")
                self._db_update_btn.setEnabled(True)
            # Enable Build whenever we have a game root
            has_root = bool(getattr(self, '_path_edit', None) and
                            self._path_edit.text().strip())
            self._db_build_btn.setEnabled(has_root)
        except Exception as e:
            self._db_stats_lbl.setText(f"DB error: {e}")
        # Refresh tree status column to show ● in DB
        self._db_refresh_tree_status()

    def _populate_col_db_tab(self): #vers 1
        """Fill the COL DB tab from asset_db.col_entries.
        Shows every COL model indexed from any IMG in the DB."""
        tbl = self._col_db_table
        tbl.setRowCount(0)

        db = getattr(self, '_asset_db', None)
        if db is None:
            mw = self.main_window
            db = getattr(mw, 'asset_db', None) if mw else None
        if db is None:
            return

        try:
            rows = db._con.execute("""
                SELECT ce.entry_name, ce.model_name, ce.model_id,
                       ce.col_version, sf.path AS src_path
                FROM   col_entries ce
                JOIN   source_files sf ON sf.id = ce.source_id
                ORDER  BY ce.entry_name, ce.model_name
            """).fetchall()
        except Exception:
            return

        tbl.setRowCount(len(rows))
        import os
        for r, row in enumerate(rows):
            vals = [
                row['entry_name'] or '',
                row['model_name'] or '',
                str(row['model_id'] or ''),
                row['col_version'] or '',
                os.path.basename(row['src_path'] or ''),
            ]
            for c, val in enumerate(vals):
                from PyQt6.QtWidgets import QTableWidgetItem as _TWI
                item = _TWI(val)
                # Store full source path for double-click
                if c == 0:
                    item.setData(0x100, row['src_path'])   # Qt.UserRole
                    item.setData(0x101, row['entry_name'])
                tbl.setItem(r, c, item)

        tbl.resizeColumnsToContents()
        # Update tab label
        idx = self._tabs.indexOf(tbl)
        if idx >= 0:
            self._tabs.setTabText(idx, f"COL DB ({len(rows):,})")

    def _on_col_db_double_click(self, index): #vers 1
        """Open the COL entry from the double-clicked DB row in COL Workshop."""
        tbl  = self._col_db_table
        row  = index.row()
        item = tbl.item(row, 0)
        if item is None:
            return
        src_path   = item.data(0x100)   # Qt.UserRole
        entry_name = item.data(0x101)
        model_name = (tbl.item(row, 1).text() if tbl.item(row, 1) else '')

        mw = self.main_window
        if not mw:
            return

        # Try to open via COL Workshop
        if hasattr(mw, 'open_col_workshop_docked'):
            mw.open_col_workshop_docked(
                col_name=entry_name, file_path=src_path)
            if hasattr(mw, 'log_message'):
                mw.log_message(
                    f"COL DB: opening {entry_name} "
                    f"({model_name}) from {src_path}")
        else:
            if hasattr(mw, 'log_message'):
                mw.log_message("COL Workshop not available")

    def _db_refresh_tree_status(self): #vers 1
        """Walk tree items and update status column to show ● in DB / ✓."""
        if not hasattr(self, '_tree'):
            return   # called before tree is constructed
        db = getattr(self, '_asset_db', None)
        if not db:
            return
        has_db = db.stats().get('source_files', 0) > 0
        from PyQt6.QtGui import QColor as _QC

        def _walk(item):
            path = item.data(0, Qt.ItemDataRole.UserRole)
            if path and item.text(3) not in ("✗ missing", "✗ MISSING"):
                if has_db:
                    row = db._con.execute(
                        "SELECT id FROM source_files WHERE path=?",
                        (path,)).fetchone()
                    if row:
                        item.setText(3, "● in DB")
                        item.setForeground(3, _QC("#4ade80"))
                    else:
                        item.setText(3, "✓")
                        item.setForeground(3, _QC(""))
                else:
                    if item.text(3) == "● in DB":
                        item.setText(3, "✓")
                        item.setForeground(3, _QC(""))
            for i in range(item.childCount()):
                _walk(item.child(i))

        root = self._tree.invisibleRootItem()
        for i in range(root.childCount()):
            _walk(root.child(i))

    def _db_new_profile(self): #vers 1
        from PyQt6.QtWidgets import QInputDialog
        name, ok = QInputDialog.getText(
            self, "New Profile", "Profile name (e.g. MyMod):")
        if ok and name.strip():
            safe = ''.join(c for c in name.strip() if c.isalnum() or c in '-_')
            if safe:
                from apps.methods.asset_db import AssetDB
                AssetDB(safe).close()   # create DB file
                self._db_refresh_profile_list()
                self._db_profile_combo.setCurrentText(safe.upper())

    def _db_delete_profile(self): #vers 1
        from PyQt6.QtWidgets import QMessageBox
        from apps.methods.asset_db import AssetDB
        profile = self._db_profile_combo.currentText()
        if profile in self._BUILTIN_PROFILES:
            QMessageBox.information(self, "Cannot Delete",
                f"'{profile}' is a built-in profile.\n"
                "You can clear it by rebuilding with no files.")
            return
        r = QMessageBox.question(
            self, "Delete Profile",
            f"Delete database '{profile}'?\nThis cannot be undone.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if r == QMessageBox.StandardButton.Yes:
            AssetDB.delete_profile(profile)
            self._asset_db = None
            self._db_refresh_profile_list()

    def _db_build(self): #vers 1
        """Index entire game root into the asset DB."""
        game_root = self._path_edit.text().strip()
        if not game_root or not os.path.isdir(game_root):
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "No Game Root",
                "Load a game first (set the game root path).")
            return

        db = self._get_asset_db()
        mw = self.main_window

        self._db_progress.setVisible(True)
        self._db_progress.setValue(0)
        self._db_progress.setMaximum(0)   # indeterminate while scanning
        self._db_build_btn.setEnabled(False)
        self._db_update_btn.setEnabled(False)
        QApplication.processEvents()

        if mw and hasattr(mw, 'log_message'):
            mw.log_message(f"Asset DB: building '{db.profile}' from {game_root}…")

        total_added = 0
        try:
            # Count IMG+IDE files first for progress bar
            all_files = []
            for dirpath, _, fnames in os.walk(game_root):
                for fn in fnames:
                    ext = fn.rsplit('.',1)[-1].lower() if '.' in fn else ''
                    if ext in ('img','ide'):
                        all_files.append((os.path.join(dirpath,fn), ext))

            self._db_progress.setMaximum(max(len(all_files), 1))
            self._db_progress.setFormat("Scanning files… %v/%m")

            for i, (fpath, ext) in enumerate(all_files):
                self._db_progress.setValue(i)
                self._db_progress.setFormat(
                    f"{os.path.basename(fpath)}  ({i}/{len(all_files)})")
                QApplication.processEvents()

                if ext == 'img':
                    n = db.index_img(fpath)
                else:
                    n = db.index_ide(fpath)
                total_added += n

            self._db_progress.setValue(len(all_files))
            self._db_progress.setFormat("Indexing standalone COL files…")
            QApplication.processEvents()

            # Also index standalone .col files from the current load_log
            # (COLFILE entries are on disk, not inside an IMG)
            col_added = 0
            if self.loader and self.loader.load_log:
                for _phase, etype, fpath, ok in self.loader.load_log:
                    if etype == "COLFILE" and ok and os.path.isfile(fpath):
                        col_added += db.index_col(fpath)
            if col_added:
                total_added += col_added

            self._db_progress.setFormat("Complete")

            if mw and hasattr(mw, 'log_message'):
                mw.log_message(
                    f"Asset DB '{db.profile}': indexed {total_added:,} entries "
                    f"from {len(all_files)} files"
                    + (f" + {col_added} COL models from standalone .col" if col_added else ""))

            # Also expose on main_window
            if mw:
                mw.asset_db = db

        except Exception as e:
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(f"Asset DB build error: {e}")
        finally:
            self._db_progress.setVisible(False)
            self._db_build_btn.setEnabled(True)
            self._db_load_stats()
            self._populate_col_db_tab()
            # Refresh tree status column now DB has COL entries
            self._db_refresh_tree_status()

    def _db_update(self): #vers 1
        """Re-index only changed files."""
        db  = self._get_asset_db()
        mw  = self.main_window

        self._db_progress.setVisible(True)
        self._db_progress.setMaximum(0)
        self._db_progress.setFormat("Checking for changes…")
        self._db_update_btn.setEnabled(False)
        QApplication.processEvents()

        try:
            changed = db.update_changed()
            total   = sum(changed.values())
            if mw and hasattr(mw, 'log_message'):
                if changed:
                    mw.log_message(
                        f"Asset DB: updated {len(changed)} file(s), "
                        f"{total:,} entries re-indexed")
                else:
                    mw.log_message("Asset DB: all files up to date")
        except Exception as e:
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(f"Asset DB update error: {e}")
        finally:
            self._db_progress.setVisible(False)
            self._db_update_btn.setEnabled(True)
            self._db_load_stats()
            self._populate_col_db_tab()

    def _db_auto_build_after_load(self): #vers 1
        """Called after world load completes — build/update DB in background
        if a profile already exists and has indexed files."""
        try:
            db = self._get_asset_db()
            if db.stats()['source_files'] > 0:
                # Only update changed files — fast operation
                self._db_update()
        except Exception:
            pass


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
