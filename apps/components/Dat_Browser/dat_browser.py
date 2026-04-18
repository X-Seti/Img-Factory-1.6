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
        # ── Progress section (hidden until Dump starts) ──────────────────
        self._prog_frame = QWidget()
        self._prog_frame.setVisible(False)
        prog_lay = QVBoxLayout(self._prog_frame)
        prog_lay.setContentsMargins(0, 4, 0, 0)
        prog_lay.setSpacing(3)

        # Progress bar
        self._prog_bar = QProgressBar()
        self._prog_bar.setRange(0, 100)
        self._prog_bar.setValue(0)
        self._prog_bar.setTextVisible(True)
        self._prog_bar.setFormat("%p%  —  %v / %m TXDs")
        prog_lay.addWidget(self._prog_bar)

        # Status row 1: current IMG
        self._prog_img_lbl = QLabel("Processing: —")
        self._prog_img_lbl.setStyleSheet("font-weight: bold;")
        prog_lay.addWidget(self._prog_img_lbl)

        # Status row 2: current TXD
        self._prog_txd_lbl = QLabel("Extracting: —")
        prog_lay.addWidget(self._prog_txd_lbl)

        # Scrolling log (last 10 lines visible)
        self._log_box = QTextEdit()
        self._log_box.setReadOnly(True)
        self._log_box.setFixedHeight(130)
        self._log_box.setFont(QFont("Monospace", 8))
        self._log_box.setLineWrapMode(QTextEdit.LineWrapMode.NoWrap)
        prog_lay.addWidget(self._log_box)
        self._log_lines = []   # full log buffer

        lay.addWidget(self._prog_frame)

        # ── Save log checkbox ──────────────────────────────────────────────
        self._save_log_chk = QCheckBox("Save log to file when done")
        self._save_log_chk.setChecked(False)
        lay.addWidget(self._save_log_chk)

        # ── Buttons ────────────────────────────────────────────────────────
        btn_row = QHBoxLayout()
        self._dump_btn = QPushButton("Dump TXDs")
        self._dump_btn.setDefault(True)
        self._dump_btn.clicked.connect(self._run_dump)
        self._cancel_btn = QPushButton("Cancel")
        self._cancel_btn.clicked.connect(self.reject)
        self._stop_btn = QPushButton("Stop")
        self._stop_btn.setVisible(False)
        self._stop_btn.setToolTip("Stop the current extraction")
        self._stop_btn.clicked.connect(self._request_stop)
        btn_row.addStretch()
        btn_row.addWidget(self._cancel_btn)
        btn_row.addWidget(self._stop_btn)
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

    def _request_stop(self): #vers 1
        """Signal the running dump to stop after the current file."""
        self._stop_requested = True
        self._stop_btn.setEnabled(False)
        self._log_append("⏹  Stop requested — finishing current file…")

    def _log_append(self, line: str): #vers 1
        """Append a line to the scrolling log box (keeps last 200 lines)."""
        self._log_lines.append(line)
        if len(self._log_lines) > 200:
            self._log_lines = self._log_lines[-200:]
        # Show last 10 lines in the visible box
        self._log_box.setPlainText("\n".join(self._log_lines[-10:]))
        # Auto-scroll to bottom
        sb = self._log_box.verticalScrollBar()
        sb.setValue(sb.maximum())

    def _run_dump(self): #vers 2
        """Execute the TXD dump inline — drives the progress widgets in the dialog."""
        out_dir = self._folder_edit.text().strip()
        if not out_dir:
            self._pick_folder()
            out_dir = self._folder_edit.text().strip()
        if not out_dir:
            return

        os.makedirs(out_dir, exist_ok=True)
        mode          = self._get_mode()
        skip_existing = self._skip_existing.isChecked()
        txd_names     = self._collect_txd_names(mode)
        img_paths     = self._get_img_paths(mode)

        if not txd_names:
            QMessageBox.warning(self, "Nothing to Dump",
                "No TXD names match the selected filter for this game.")
            return
        if not img_paths:
            QMessageBox.warning(self, "No IMGs",
                "No IMG/CDIMAGE archives found for this game.")
            return

        # ── Switch to progress mode ────────────────────────────────────────
        self._stop_requested = False
        self._log_lines      = []
        self._log_box.clear()
        self._prog_frame.setVisible(True)
        self._dump_btn.setEnabled(False)
        self._cancel_btn.setEnabled(False)
        self._stop_btn.setVisible(True)
        self._stop_btn.setEnabled(True)
        # Grow dialog to show log
        self.setMinimumHeight(580)

        total_txds    = len(txd_names)
        self._prog_bar.setRange(0, max(total_txds, 1))
        self._prog_bar.setValue(0)
        self._prog_bar.setFormat(f"%v / {total_txds} TXDs  (%p%)")

        self._log_append(f"Mode: {mode}  |  {total_txds} TXDs targeted  |  "
                         f"{len(img_paths)} archive(s)")
        self._log_append(f"Output: {out_dir}")
        self._log_append("─" * 60)

        extracted, skipped, errors = 0, 0, []
        remaining = set(txd_names)

        try:
            from apps.methods.img_tools import IMGArchive
        except ImportError as e:
            self._log_append(f"ERROR: Cannot import IMGArchive — {e}")
            self._finish_dump(extracted, skipped, errors, remaining, out_dir, mode)
            return

        for img_path in img_paths:
            if self._stop_requested:
                self._log_append("⏹  Stopped by user.")
                break

            img_name = os.path.basename(img_path)
            self._prog_img_lbl.setText(f"Processing: {img_name}")
            self._prog_txd_lbl.setText("Scanning entries…")
            QApplication.processEvents()

            try:
                arc = IMGArchive(img_path)
                txd_entries = [e for e in arc.entries
                               if e.name.lower().endswith('.txd')]
                n_entries = len(txd_entries)
                self._log_append(f"📦 {img_name}  ({n_entries} TXD entries)")

                for entry in txd_entries:
                    if self._stop_requested:
                        break
                    ename = entry.name.lower()
                    if mode != 'all' and ename not in txd_names:
                        continue

                    self._prog_txd_lbl.setText(
                        f"Extracting: {entry.name}  ({extracted}/{total_txds})")
                    QApplication.processEvents()

                    out_path = os.path.join(out_dir, entry.name)
                    if skip_existing and os.path.exists(out_path):
                        skipped += 1
                        remaining.discard(ename)
                        self._log_append(f"  ⏭  skip  {entry.name}  (exists)")
                        continue

                    try:
                        data = arc.read_entry(entry)
                        with open(out_path, 'wb') as f:
                            f.write(data)
                        extracted += 1
                        remaining.discard(ename)
                        self._prog_bar.setValue(extracted)
                        self._log_append(
                            f"  ✓  {entry.name}  "
                            f"({len(data)//1024} KB)")
                    except Exception as e:
                        err_msg = f"{entry.name}: {e}"
                        errors.append(err_msg)
                        self._log_append(f"  ✗  ERROR  {err_msg}")

            except Exception as e:
                err_msg = f"{img_name}: {e}"
                errors.append(err_msg)
                self._log_append(f"  ✗  IMG ERROR  {err_msg}")

        self._finish_dump(extracted, skipped, errors, remaining, out_dir, mode)

    def _finish_dump(self, extracted, skipped, errors, remaining,
                     out_dir, mode): #vers 1
        """Finalise the dump — show summary, optionally save log, re-enable UI."""
        self._prog_img_lbl.setText("Done.")
        self._prog_txd_lbl.setText(
            f"Extracted {extracted}  |  Skipped {skipped}  |  "
            f"Errors {len(errors)}  |  Not found {len(remaining)}")

        summary_lines = [
            "─" * 60,
            f"DONE  mode={mode}",
            f"  Extracted : {extracted}",
            f"  Skipped   : {skipped}",
            f"  Errors    : {len(errors)}",
            f"  Not found : {len(remaining)}",
            f"  Output    : {out_dir}",
        ]
        if remaining and len(remaining) <= 30:
            summary_lines.append(
                f"  Missing   : {', '.join(sorted(remaining))}")
        if errors:
            summary_lines.append(f"  First errors: {'; '.join(errors[:3])}")

        for line in summary_lines:
            self._log_append(line)

        # Optionally save full log
        if self._save_log_chk.isChecked():
            import datetime
            log_path = os.path.join(
                out_dir,
                f"txd_dump_{mode}_{datetime.datetime.now():%Y%m%d_%H%M%S}.log")
            try:
                with open(log_path, 'w', encoding='utf-8') as f:
                    f.write("\n".join(self._log_lines))
                self._log_append(f"📄 Log saved: {os.path.basename(log_path)}")
            except Exception as e:
                self._log_append(f"  Log save error: {e}")

        # Re-enable UI
        self._dump_btn.setEnabled(True)
        self._cancel_btn.setEnabled(True)
        self._stop_btn.setVisible(False)
        self._dump_btn.setText("Dump Again")
        self._cancel_btn.setText("Close")

        # Optionally open in TXD Workshop
        if self._open_in_txd.isChecked() and extracted > 0:
            mw = self.main_window
            if mw and hasattr(mw, 'open_txd_workshop_docked'):
                try:
                    first = next(
                        f for f in sorted(os.listdir(out_dir))
                        if f.lower().endswith('.txd'))
                    mw.open_txd_workshop_docked(
                        file_path=os.path.join(out_dir, first))
                except StopIteration:
                    pass


