#this belongs in core/scan_img.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6
# Recursive IMG scanner — finds all .img / .dir files in a folder tree,
# detects their version and platform, and lets the user batch-open them.

"""
scan_img_folder(main_window)
    Opens a folder picker, then recursively scans for IMG-compatible files,
    shows a results dialog with version/platform/size info for each, and
    opens the selected files in new tabs.

Public API
----------
scan_img_folder(main_window)       - triggered from menu
"""

import os
import struct
from typing import List, Dict, Tuple

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTreeWidget, QTreeWidgetItem,
    QPushButton, QLabel, QProgressBar, QFileDialog, QAbstractItemView,
    QHeaderView, QLineEdit, QComboBox, QCheckBox, QGroupBox, QSplitter,
    QWidget, QMessageBox, QApplication
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal, QTimer
from PyQt6.QtGui import QFont, QColor

## Methods list -
# scan_img_folder
# _scan_directory
# _quick_detect
# _format_size
# ScanThread
# ScanResultsDialog

#    Extensions we care about                                                   

SCAN_CACHE_PATH = os.path.expanduser("~/.config/imgfactory/scan_cache.json")
MAX_CACHED_SCANS = 20   # keep last N scan folders

#    Cache helpers                                                              

def _save_scan_cache(folder: str, results: list, scan_date: str = "") -> None:
    """Persist scan results for a folder to the cache file."""
    import json
    from datetime import datetime
    try:
        cache = _load_scan_cache()
        cache[folder] = {
            "folder":     folder,
            "date":       scan_date or datetime.now().strftime("%Y-%m-%d %H:%M"),
            "count":      len(results),
            "results":    results,
        }
        # Trim to MAX_CACHED_SCANS most-recent entries
        if len(cache) > MAX_CACHED_SCANS:
            # Sort by date descending, keep newest
            items = sorted(cache.items(),
                           key=lambda x: x[1].get("date", ""), reverse=True)
            cache = dict(items[:MAX_CACHED_SCANS])
        os.makedirs(os.path.dirname(SCAN_CACHE_PATH), exist_ok=True)
        with open(SCAN_CACHE_PATH, "w") as f:
            json.dump(cache, f, indent=2)
    except Exception as e:
        print(f"[scan_img] Cache save error: {e}")


def _load_scan_cache() -> dict:
    """Return cached scan results dict keyed by folder path."""
    import json
    try:
        if os.path.exists(SCAN_CACHE_PATH):
            with open(SCAN_CACHE_PATH) as f:
                return json.load(f)
    except Exception:
        pass
    return {}


def _delete_scan_cache_entry(folder: str) -> None:
    """Remove one folder from the cache."""
    import json
    try:
        cache = _load_scan_cache()
        cache.pop(folder, None)
        os.makedirs(os.path.dirname(SCAN_CACHE_PATH), exist_ok=True)
        with open(SCAN_CACHE_PATH, "w") as f:
            json.dump(cache, f, indent=2)
    except Exception as e:
        print(f"[scan_img] Cache delete error: {e}")


SCAN_EXTENSIONS = {
    '.img', '.dir', '.lvz', '.hxd', '.mxd', '.agr'
}

# Extension → display label (for non-.img files)
EXT_LABELS = {
    '.dir':  'DIR',
    '.lvz':  'LVZ',
    '.hxd':  'HXD',
    '.mxd':  'MXD',
    '.agr':  'AGR',
}

#    Quick version detection (no Qt, no full parse)                             

def _quick_detect(file_path: str) -> Tuple[str, str]:
    """Return (version_label, platform_label) without importing IMGFile.

    Uses the same heuristics as detect_version() but pure file I/O only.
    Returns strings suitable for display in the results table.
    """
    ext = os.path.splitext(file_path)[1].lower()
    bn  = os.path.basename(file_path).lower()

    try:
        # Non-IMG special formats
        if ext == '.lvz':
            return 'PS2_LVZ', 'PS2'
        if ext in ('.hxd', '.mxd', '.agr'):
            return 'HXD/MXD', 'PS2'

        # .dir = V1/V1.5/Xbox DIR+IMG pair
        if ext == '.dir':
            img_path = file_path[:-4] + '.img'
            if not os.path.exists(img_path):
                img_path = file_path[:-4] + '.IMG'
            if not os.path.exists(img_path):
                return 'V1 (no .img)', 'PC'
            # Xbox LZO probe: read first entry offset from .dir, peek .img
            try:
                with open(file_path, 'rb') as df:
                    entry = df.read(8)
                if len(entry) >= 8:
                    sec_off = struct.unpack_from('<I', entry, 0)[0]
                    with open(img_path, 'rb') as f:
                        f.seek(sec_off * 2048)
                        magic4 = f.read(4)
                    if len(magic4) == 4 and struct.unpack_from('<I', magic4)[0] == 0x67A3A1CE:
                        return 'Xbox', 'Xbox'
            except Exception:
                pass
            # Size-based V1 vs V1.5
            sz = os.path.getsize(img_path)
            ver = 'V1.5' if sz > 2 * 1024**3 else 'V1'
            return ver, 'PC'

        if ext != '.img':
            return 'Unknown', '?'

        # .img — read first 4 bytes
        with open(file_path, 'rb') as f:
            header = f.read(16)
        if len(header) < 4:
            return 'Too small', '?'

        magic4 = header[:4]

        # GTA IV unencrypted
        if struct.unpack_from('<I', magic4)[0] == 0xA94E2A52:
            return 'V3 (GTA IV)', 'PC'

        # GTA IV encrypted — quick check only
        if magic4 not in (b'VER2', b'ANPK') and all(b > 0x7F for b in magic4):
            return 'V3 Enc', 'PC'

        # ANPK (PSP animation)
        if magic4 == b'ANPK':
            return 'ANPK', 'PSP'

        # VER2 — SA PC, SA Android, LCS Android
        if magic4 == b'VER2':
            directory = os.path.dirname(file_path)
            mobile_dbs = ('texdb.dat', 'texdb.toc', 'streaming.dat')
            has_mobile = any(os.path.exists(os.path.join(directory, d)) for d in mobile_dbs)
            if 'lcs' in bn or 'liberty' in bn:
                return 'LCS Android', 'Android'
            if has_mobile or 'android' in bn or 'mobile' in bn:
                return 'SA Android', 'Android'
            return 'V2 (SA)', 'PC'

        # PS2 VCS embedded-dir: printable type-code in first 4 bytes
        type_bytes = magic4
        if (all(0x20 <= b < 0x7F or b == 0 for b in type_bytes)
                and type_bytes.rstrip(b'\x00') != b''
                and header[4:8] == b'\x00\x00\x00\x00'):
            return 'PS2_VCS', 'PS2'

        # 12-byte entry format: PS2_V1 / iOS / LCS iOS
        count  = struct.unpack_from('<I', header, 0)[0]
        sec_sz = struct.unpack_from('<I', header, 8)[0]
        if 0 < count < 5000 and 0 < sec_sz < 10000:
            if '_pvr' in bn:
                if 'lcs' in bn or 'liberty' in bn:
                    return 'LCS iOS', 'iOS'
                return 'V1 iOS', 'iOS'
            return 'PS2_V1', 'PS2/Android'

        # Has matching .dir?
        dir_path = file_path[:-4] + '.dir'
        if not os.path.exists(dir_path):
            dir_path = file_path[:-4] + '.DIR'
        if os.path.exists(dir_path):
            sz = os.path.getsize(file_path)
            ver = 'V1.5' if sz > 2 * 1024**3 else 'V1'
            return ver, 'PC'

        return 'Unknown', '?'

    except Exception:
        return 'Error', '?'


def _format_size(size_bytes: int) -> str:
    """Human-readable file size."""
    if size_bytes < 1024:
        return f'{size_bytes} B'
    if size_bytes < 1024 * 1024:
        return f'{size_bytes / 1024:.1f} KB'
    if size_bytes < 1024 ** 3:
        return f'{size_bytes / 1024**2:.1f} MB'
    return f'{size_bytes / 1024**3:.2f} GB'


#    Background scan thread                                                     

class ScanThread(QThread):  # vers 1
    """Recursively scans a directory for IMG-compatible files."""

    progress   = pyqtSignal(int, int)          # (found_so_far, dirs_scanned)
    found_file = pyqtSignal(dict)              # one result dict per file
    finished   = pyqtSignal(int)               # total found

    def __init__(self, root_path: str, skip_tiny_kb: int = 4):
        super().__init__()
        self.root_path   = root_path
        self.skip_tiny   = skip_tiny_kb * 1024
        self._stop       = False

    def stop(self):
        self._stop = True

    def run(self):
        found = 0
        dirs  = 0
        for dirpath, _dirnames, filenames in os.walk(self.root_path):
            if self._stop:
                break
            dirs += 1
            for fname in filenames:
                if self._stop:
                    break
                ext = os.path.splitext(fname)[1].lower()
                if ext not in SCAN_EXTENSIONS:
                    continue
                full = os.path.join(dirpath, fname)
                try:
                    size = os.path.getsize(full)
                except OSError:
                    continue
                if size < self.skip_tiny:
                    continue
                ver, plat = _quick_detect(full)
                result = {
                    'path':     full,
                    'name':     fname,
                    'dir':      dirpath,
                    'size':     size,
                    'size_str': _format_size(size),
                    'version':  ver,
                    'platform': plat,
                    'ext':      ext,
                }
                found += 1
                self.found_file.emit(result)
                if found % 10 == 0:
                    self.progress.emit(found, dirs)
        self.finished.emit(found)


#    Results dialog                                                              


from apps.core.theme_utils import get_theme_colors as _get_theme_colors, build_dialog_stylesheet as _build_dialog_stylesheet

class ScanResultsDialog(QDialog):  # vers 1
    """Shows scan results and lets the user open selected IMG files."""

    def __init__(self, parent=None, root_path: str = '', cached_results: list = None):
        super().__init__(parent)
        self.root_path   = root_path
        self._results    = []   # list of result dicts
        self._thread     = None
        self._main_win   = parent
        self.setWindowTitle(f'IMG Scan — {os.path.basename(root_path) or root_path}')
        self.setMinimumSize(900, 580)
        self._build_ui()
        self.setStyleSheet(_build_dialog_stylesheet(_get_theme_colors(parent)))
        if cached_results is not None:
            # Restore from cache — no scan needed
            self._load_cached(cached_results)
        else:
            self._start_scan()

    #    UI                                                                   

    def _build_ui(self):
        layout = QVBoxLayout(self)

        # Toolbar row
        top = QHBoxLayout()
        self._status = QLabel('Scanning…')
        self._status.setFont(QFont('Arial', 9))
        top.addWidget(self._status)
        top.addStretch()

        # Filter
        top.addWidget(QLabel('Filter:'))
        self._filter = QLineEdit()
        self._filter.setPlaceholderText('name, version, platform…')
        self._filter.setFixedWidth(200)
        self._filter.textChanged.connect(self._apply_filter)
        top.addWidget(self._filter)

        # Platform filter
        self._plat_combo = QComboBox()
        self._plat_combo.addItems(['All platforms', 'PC', 'Xbox', 'PS2', 'Android', 'iOS', 'PSP'])
        self._plat_combo.currentTextChanged.connect(self._apply_filter)
        top.addWidget(self._plat_combo)

        layout.addLayout(top)

        # Progress bar
        self._progress = QProgressBar()
        self._progress.setMaximum(0)   # indeterminate while scanning
        self._progress.setFixedHeight(4)
        layout.addWidget(self._progress)

        # Tree
        self._tree = QTreeWidget()
        self._tree.setColumnCount(5)
        self._tree.setHeaderLabels(['Name', 'Version', 'Platform', 'Size', 'Path'])
        self._tree.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        self._tree.setSortingEnabled(True)
        self._tree.sortByColumn(0, Qt.SortOrder.AscendingOrder)
        hdr = self._tree.header()
        hdr.setSectionResizeMode(0, QHeaderView.ResizeMode.Interactive)
        hdr.setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        hdr.setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
        hdr.setSectionResizeMode(3, QHeaderView.ResizeMode.ResizeToContents)
        hdr.setSectionResizeMode(4, QHeaderView.ResizeMode.Stretch)
        self._tree.setColumnWidth(0, 240)
        self._tree.itemDoubleClicked.connect(self._open_selected)
        layout.addWidget(self._tree)

        # Bottom buttons
        bot = QHBoxLayout()

        self._sel_all = QPushButton('Select All')
        self._sel_all.clicked.connect(self._tree.selectAll)
        bot.addWidget(self._sel_all)

        self._sel_none = QPushButton('Select None')
        self._sel_none.clicked.connect(self._tree.clearSelection)
        bot.addWidget(self._sel_none)

        self._sel_plat = QPushButton('Select by Platform')
        self._sel_plat.clicked.connect(self._select_by_platform)
        bot.addWidget(self._sel_plat)

        bot.addStretch()

        self._count_label = QLabel('')
        bot.addWidget(self._count_label)

        bot.addStretch()

        self._rescan_btn = QPushButton('Rescan')
        self._rescan_btn.setToolTip('Discard results and scan this folder again')
        self._rescan_btn.clicked.connect(self._rescan)
        bot.addWidget(self._rescan_btn)

        self._stop_btn = QPushButton('Stop Scan')
        self._stop_btn.clicked.connect(self._stop_scan)
        bot.addWidget(self._stop_btn)

        self._open_btn = QPushButton('Open Selected')
        self._open_btn.setDefault(True)
        self._open_btn.clicked.connect(self._open_selected)
        bot.addWidget(self._open_btn)

        self._close_btn = QPushButton('Close')
        self._close_btn.clicked.connect(self.reject)
        bot.addWidget(self._close_btn)

        layout.addLayout(bot)

        self._tree.itemSelectionChanged.connect(self._update_count)

    #    Scan                                                                 

    def _load_cached(self, results: list):
        """Restore previously saved scan results instantly."""
        self._progress.setFixedHeight(0)
        self._stop_btn.setEnabled(False)
        for r in results:
            self._results.append(r)
            self._add_tree_item(r)
        self._status.setText(
            f'Restored {len(results)} cached results from {self.root_path}'
        )
        self._apply_filter()
        self._update_count()

    def _start_scan(self):
        self._thread = ScanThread(self.root_path)
        self._thread.found_file.connect(self._on_found)
        self._thread.progress.connect(self._on_progress)
        self._thread.finished.connect(self._on_done)
        self._thread.start()

    def _stop_scan(self):
        if self._thread and self._thread.isRunning():
            self._thread.stop()
        self._stop_btn.setEnabled(False)

    def _on_found(self, result: dict):
        self._results.append(result)
        self._add_tree_item(result)
        self._apply_filter()

    def _on_progress(self, found: int, dirs: int):
        self._status.setText(f'Scanning… {found} files found ({dirs} directories)')

    def _on_done(self, total: int):
        self._progress.setMaximum(1)
        self._progress.setValue(1)
        self._progress.setFixedHeight(0)
        self._stop_btn.setEnabled(False)
        self._status.setText(f'Scan complete — {total} file{"s" if total != 1 else ""} found in {self.root_path}')
        self._update_count()
        # Auto-save to cache after scan completes
        if self._results:
            _save_scan_cache(self.root_path, self._get_results_for_cache())

    #    Tree population                                                       

    # Colour map for version → background tint
    _VER_COLORS = {
        'V1':           '#1e3a1e',
        'V1.5':         '#1e3a1e',
        'V1 iOS':       '#1e3040',
        'V2 (SA)':      '#1a3a5c',
        'SA Android':   '#1a3a5c',
        'LCS Android':  '#3a2a1e',
        'LCS iOS':      '#3a2a1e',
        'Xbox':         '#3a1e1e',
        'PS2_V1':       '#2a2a3a',
        'PS2_VCS':      '#2a2a3a',
        'PS2_LVZ':      '#2a2a3a',
        'ANPK':         '#2a3a2a',
        'V3 (GTA IV)':  '#3a3a1e',
        'V3 Enc':       '#3a3a1e',
    }

    def _add_tree_item(self, r: dict):
        item = QTreeWidgetItem([
            r['name'],
            r['version'],
            r['platform'],
            r['size_str'],
            r['path'],
        ])
        item.setData(0, Qt.ItemDataRole.UserRole, r)

        # Sort by size numerically
        item.setData(3, Qt.ItemDataRole.UserRole + 1, r['size'])

        # Subtle background tint by version
        bg = self._VER_COLORS.get(r['version'], '')
        if bg:
            col = QColor(bg)
            for c in range(5):
                item.setBackground(c, col)

        self._tree.addTopLevelItem(item)
        return item

    #    Filter                                                                

    def _apply_filter(self):
        text  = self._filter.text().lower()
        plat  = self._plat_combo.currentText()
        shown = 0
        for i in range(self._tree.topLevelItemCount()):
            item = self._tree.topLevelItem(i)
            r    = item.data(0, Qt.ItemDataRole.UserRole)
            if not r:
                continue
            plat_ok = (plat == 'All platforms' or r['platform'] == plat
                       or plat.lower() in r['platform'].lower())
            text_ok = (not text or text in r['name'].lower()
                       or text in r['version'].lower()
                       or text in r['platform'].lower()
                       or text in r['path'].lower())
            visible = plat_ok and text_ok
            item.setHidden(not visible)
            if visible:
                shown += 1
        self._status.setText(
            f'{shown} of {len(self._results)} files shown'
            if len(self._results) > shown
            else f'{len(self._results)} files'
        )

    #    Selection helpers                                                     

    def _select_by_platform(self):
        plat = self._plat_combo.currentText()
        if plat == 'All platforms':
            self._tree.selectAll()
            return
        self._tree.clearSelection()
        for i in range(self._tree.topLevelItemCount()):
            item = self._tree.topLevelItem(i)
            r    = item.data(0, Qt.ItemDataRole.UserRole)
            if r and (plat.lower() in r['platform'].lower()) and not item.isHidden():
                item.setSelected(True)

    def _update_count(self):
        n = len(self._tree.selectedItems())
        self._count_label.setText(f'{n} selected' if n else '')
        self._open_btn.setEnabled(n > 0)

    #    Open                                                                  

    def _open_selected(self):
        items = self._tree.selectedItems()
        if not items:
            return

        # Warn if opening many files
        if len(items) > 10:
            resp = QMessageBox.question(
                self, 'Open Many Files',
                f'Open {len(items)} files? This may take a while.',
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
            )
            if resp != QMessageBox.StandardButton.Yes:
                return

        mw = self._main_win
        for item in items:
            r = item.data(0, Qt.ItemDataRole.UserRole)
            if not r:
                continue
            path = r['path']
            try:
                if hasattr(mw, '_load_img_file_in_new_tab'):
                    mw._load_img_file_in_new_tab(path)
                elif hasattr(mw, 'load_img_file_in_new_tab'):
                    mw.load_img_file_in_new_tab(path)
                else:
                    from apps.core.open import _load_img_file
                    _load_img_file(mw, path)
                if hasattr(mw, 'log_message'):
                    mw.log_message(f'Opened from scan: {r["name"]}')
            except Exception as e:
                if hasattr(mw, 'log_message'):
                    mw.log_message(f'Error opening {r["name"]}: {e}')

    def _rescan(self):
        """Discard current results and scan the same folder again."""
        self._stop_scan()
        if self._thread:
            self._thread.wait(500)
        self._results.clear()
        self._tree.clear()
        self._progress.setMaximum(0)
        self._progress.setFixedHeight(4)
        self._stop_btn.setEnabled(True)
        self._status.setText("Rescanning…")
        self._start_scan()

    def _get_results_for_cache(self) -> list:
        """Serialise current results to a JSON-safe list."""
        return [
            {k: r[k] for k in ("path","name","dir","size","size_str","version","platform","ext")}
            for r in self._results
        ]

    def closeEvent(self, event):
        self._stop_scan()
        if self._thread:
            self._thread.wait(1000)
        # Persist results to cache
        if self._results:
            _save_scan_cache(self.root_path, self._get_results_for_cache())
        super().closeEvent(event)


#    Recent Scans dialog                                                       

class RecentScansDialog(QDialog):  # vers 2
    """Combined Recent Scans + live scan results in one dialog.

    Left panel  — list of previously scanned folders (history)
    Right panel — scan results for the currently selected / active scan
    Toolbar     — New Scan, Rescan, Remove Entry, Open Selected, Close
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._main_win   = parent
        self._cache      = _load_scan_cache()
        self._scan_thread = None
        self.setWindowTitle("IMG Scan — History & Results")
        self.setMinimumSize(1100, 600)
        self._build_ui()
        self.setStyleSheet(_build_dialog_stylesheet(_get_theme_colors(parent)))
        self._populate_history()
        # Auto-select most recent entry and show its results
        if self._history_tree.topLevelItemCount() > 0:
            first = self._history_tree.topLevelItem(0)
            if first.data(0, Qt.ItemDataRole.UserRole):
                self._history_tree.setCurrentItem(first)
                self._show_entry(first.data(0, Qt.ItemDataRole.UserRole))

    #    UI                                                                    

    def _build_ui(self):
        outer = QVBoxLayout(self)
        outer.setContentsMargins(6, 6, 6, 6)
        outer.setSpacing(4)

        #    Toolbar                                                           
        tb = QHBoxLayout()

        self._new_btn    = QPushButton("+ New Scan…")
        self._new_btn.setToolTip("Pick a new folder to scan (Ctrl+N)")
        self._new_btn.clicked.connect(self._new_scan)
        tb.addWidget(self._new_btn)

        self._rescan_btn = QPushButton("↺ Rescan")
        self._rescan_btn.setToolTip("Re-run the scan on the selected folder")
        self._rescan_btn.setEnabled(False)
        self._rescan_btn.clicked.connect(self._rescan_selected)
        tb.addWidget(self._rescan_btn)

        self._del_btn = QPushButton("✕ Remove")
        self._del_btn.setToolTip("Remove this entry from history")
        self._del_btn.setEnabled(False)
        self._del_btn.clicked.connect(self._delete_selected)
        tb.addWidget(self._del_btn)

        tb.addStretch()

        self._filter = QLineEdit()
        self._filter.setPlaceholderText("Filter results…")
        self._filter.setFixedWidth(200)
        self._filter.textChanged.connect(self._apply_filter)
        tb.addWidget(self._filter)

        self._plat_combo = QComboBox()
        self._plat_combo.addItems(["All platforms","PC","Xbox","PS2","Android","iOS","PSP"])
        self._plat_combo.currentTextChanged.connect(self._apply_filter)
        tb.addWidget(self._plat_combo)

        self._stop_btn = QPushButton("■ Stop")
        self._stop_btn.setEnabled(False)
        self._stop_btn.clicked.connect(self._stop_scan)
        tb.addWidget(self._stop_btn)

        outer.addLayout(tb)

        #    Status / progress                                                  
        self._status = QLabel("Select a previous scan or start a new one.")
        self._status.setStyleSheet("color: palette(mid); font-size: 11px;")
        outer.addWidget(self._status)

        self._progress = QProgressBar()
        self._progress.setMaximum(0)
        self._progress.setFixedHeight(3)
        self._progress.setVisible(False)
        outer.addWidget(self._progress)

        #    Splitter: history list | results                                  
        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.setChildrenCollapsible(False)

        # Left: history list
        left = QWidget()
        left.setMinimumWidth(200)
        left.setMaximumWidth(320)
        ll = QVBoxLayout(left)
        ll.setContentsMargins(0, 0, 4, 0)
        ll.setSpacing(2)
        ll.addWidget(QLabel("Previous Scans:"))

        self._history_tree = QTreeWidget()
        self._history_tree.setColumnCount(3)
        self._history_tree.setHeaderLabels(["Folder", "Files", "Date"])
        self._history_tree.setSortingEnabled(True)
        self._history_tree.sortByColumn(2, Qt.SortOrder.DescendingOrder)
        self._history_tree.setAlternatingRowColors(True)
        hdr = self._history_tree.header()
        hdr.setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        hdr.setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        hdr.setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
        self._history_tree.itemSelectionChanged.connect(self._on_history_select)
        ll.addWidget(self._history_tree)
        splitter.addWidget(left)

        # Right: results table
        right = QWidget()
        rl = QVBoxLayout(right)
        rl.setContentsMargins(4, 0, 0, 0)

        self._results_tree = QTreeWidget()
        self._results_tree.setColumnCount(5)
        self._results_tree.setHeaderLabels(["Name","Version","Platform","Size","Path"])
        self._results_tree.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        self._results_tree.setSortingEnabled(True)
        self._results_tree.setAlternatingRowColors(True)
        rhdr = self._results_tree.header()
        rhdr.setSectionResizeMode(0, QHeaderView.ResizeMode.Interactive)
        rhdr.setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        rhdr.setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
        rhdr.setSectionResizeMode(3, QHeaderView.ResizeMode.ResizeToContents)
        rhdr.setSectionResizeMode(4, QHeaderView.ResizeMode.Stretch)
        self._results_tree.setColumnWidth(0, 220)
        self._results_tree.itemDoubleClicked.connect(self._open_selected)
        self._results_tree.itemSelectionChanged.connect(self._update_open_btn)
        rl.addWidget(self._results_tree)
        splitter.addWidget(right)
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 1)

        outer.addWidget(splitter, 1)

        #    Bottom buttons                                                     
        bot = QHBoxLayout()

        self._sel_all  = QPushButton("Select All")
        self._sel_all.clicked.connect(self._results_tree.selectAll)
        bot.addWidget(self._sel_all)

        self._sel_none = QPushButton("Select None")
        self._sel_none.clicked.connect(self._results_tree.clearSelection)
        bot.addWidget(self._sel_none)

        bot.addStretch()
        self._count_lbl = QLabel("")
        bot.addWidget(self._count_lbl)
        bot.addStretch()

        self._open_btn = QPushButton("Open Selected")
        self._open_btn.setDefault(True)
        self._open_btn.setEnabled(False)
        self._open_btn.clicked.connect(self._open_selected)
        bot.addWidget(self._open_btn)

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.reject)
        bot.addWidget(close_btn)

        outer.addLayout(bot)

        # Keyboard shortcut
        from PyQt6.QtGui import QKeySequence, QShortcut
        QShortcut(QKeySequence("Ctrl+N"), self).activated.connect(self._new_scan)

    #    History list                                                           

    def _populate_history(self):
        self._history_tree.clear()
        if not self._cache:
            empty = QTreeWidgetItem(["No scans yet", "", ""])
            empty.setFlags(Qt.ItemFlag.NoItemFlags)
            self._history_tree.addTopLevelItem(empty)
            return
        for folder, entry in sorted(self._cache.items(),
                                    key=lambda x: x[1].get("date",""), reverse=True):
            name = os.path.basename(folder) or folder
            item = QTreeWidgetItem([name,
                                    str(entry.get("count","?")),
                                    entry.get("date","")])
            item.setData(0, Qt.ItemDataRole.UserRole, entry)
            item.setToolTip(0, folder)
            if not os.path.exists(folder):
                for c in range(3):
                    item.setForeground(c, QColor("#888888"))
                item.setToolTip(0, f"Missing: {folder}")
            self._history_tree.addTopLevelItem(item)

    def _on_history_select(self):
        sel = self._history_tree.selectedItems()
        entry = sel[0].data(0, Qt.ItemDataRole.UserRole) if sel else None
        has = bool(entry)
        self._rescan_btn.setEnabled(has)
        self._del_btn.setEnabled(has)
        if entry:
            self._show_entry(entry)

    def _show_entry(self, entry: dict):
        """Populate the right-hand results panel from a cache entry."""
        self._results_tree.clear()
        results = entry.get("results", [])
        folder  = entry.get("folder", "")
        for r in results:
            self._add_result_item(r)
        self._status.setText(
            f"{len(results)} files  ·  {folder}  ·  {entry.get('date','')}")
        self._apply_filter()
        self._update_open_btn()

    #    Results table                                                          

    # Colour map matching ScanResultsDialog
    _VER_COLORS = {
        "V1":"#1e3a1e","V1.5":"#1e3a1e","V1 iOS":"#1e3040",
        "V2 (SA)":"#1a3a5c","SA Android":"#1a3a5c","LCS Android":"#3a2a1e",
        "LCS iOS":"#3a2a1e","Xbox":"#3a1e1e","PS2_V1":"#2a2a3a",
        "PS2_VCS":"#2a2a3a","PS2_LVZ":"#2a2a3a","ANPK":"#2a3a2a",
        "V3 (GTA IV)":"#3a3a1e","V3 Enc":"#3a3a1e",
    }

    def _add_result_item(self, r: dict):
        item = QTreeWidgetItem([r["name"], r["version"], r["platform"],
                                r["size_str"], r["path"]])
        item.setData(0, Qt.ItemDataRole.UserRole, r)
        bg = self._VER_COLORS.get(r["version"], "")
        if bg:
            col = QColor(bg)
            for c in range(5):
                item.setBackground(c, col)
        self._results_tree.addTopLevelItem(item)

    def _apply_filter(self):
        text = self._filter.text().lower()
        plat = self._plat_combo.currentText()
        shown = 0
        for i in range(self._results_tree.topLevelItemCount()):
            item = self._results_tree.topLevelItem(i)
            r    = item.data(0, Qt.ItemDataRole.UserRole)
            if not r:
                item.setHidden(False); continue
            plat_ok = (plat == "All platforms" or
                       plat.lower() in r["platform"].lower())
            text_ok = (not text or text in r["name"].lower() or
                       text in r["version"].lower() or
                       text in r["platform"].lower() or
                       text in r["path"].lower())
            item.setHidden(not (plat_ok and text_ok))
            if not item.isHidden(): shown += 1
        total = self._results_tree.topLevelItemCount()
        if shown < total:
            self._status.setText(f"{shown} of {total} files shown")

    def _update_open_btn(self):
        n = len(self._results_tree.selectedItems())
        self._open_btn.setEnabled(n > 0)
        self._count_lbl.setText(f"{n} selected" if n else "")

    #    Scan (live)                                                            

    def _run_scan(self, folder: str):
        """Start a live scan, streaming results into the right panel."""
        self._results_tree.clear()
        self._progress.setMaximum(0)
        self._progress.setVisible(True)
        self._stop_btn.setEnabled(True)
        self._new_btn.setEnabled(False)
        self._status.setText(f"Scanning {folder}…")
        self._active_folder   = folder
        self._active_results  = []

        self._scan_thread = ScanThread(folder)
        self._scan_thread.found_file.connect(self._on_scan_found)
        self._scan_thread.progress.connect(self._on_scan_progress)
        self._scan_thread.finished.connect(self._on_scan_done)
        self._scan_thread.start()

    def _on_scan_found(self, r: dict):
        self._active_results.append(r)
        self._add_result_item(r)
        self._apply_filter()

    def _on_scan_progress(self, found: int, dirs: int):
        self._status.setText(f"Scanning… {found} files found ({dirs} dirs)")

    def _on_scan_done(self, total: int):
        self._progress.setVisible(False)
        self._stop_btn.setEnabled(False)
        self._new_btn.setEnabled(True)
        folder = getattr(self, "_active_folder", "")
        self._status.setText(
            f"Scan complete — {total} file{'s' if total!=1 else ''} in {folder}")
        # Save to cache and refresh history list
        if self._active_results:
            _save_scan_cache(folder, self._active_results)
            self._cache = _load_scan_cache()
            self._populate_history()
            # Select the newly added entry
            for i in range(self._history_tree.topLevelItemCount()):
                item = self._history_tree.topLevelItem(i)
                entry = item.data(0, Qt.ItemDataRole.UserRole)
                if entry and entry.get("folder") == folder:
                    self._history_tree.setCurrentItem(item)
                    break
        self._update_open_btn()

    def _stop_scan(self):
        if self._scan_thread and self._scan_thread.isRunning():
            self._scan_thread.stop()
        self._stop_btn.setEnabled(False)

    #    History actions                                                        

    def _new_scan(self):
        """Pick a new folder and scan it live inside this dialog."""
        folder = QFileDialog.getExistingDirectory(
            self, "Select Folder to Scan",
            getattr(self._main_win, "last_open_dir", ""))
        if not folder:
            return
        if hasattr(self._main_win, "last_open_dir"):
            self._main_win.last_open_dir = folder
        self._run_scan(folder)

    def _rescan_selected(self):
        entry = self._selected_history_entry()
        if not entry:
            return
        folder = entry.get("folder", "")
        if not os.path.exists(folder):
            QMessageBox.warning(self, "Folder Missing",
                f"The folder no longer exists:\n{folder}")
            return
        self._run_scan(folder)

    def _delete_selected(self):
        entry = self._selected_history_entry()
        if not entry:
            return
        folder = entry.get("folder", "")
        if QMessageBox.question(
                self, "Remove Entry",
                f"Remove from history?\n{folder}",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        ) == QMessageBox.StandardButton.Yes:
            _delete_scan_cache_entry(folder)
            self._cache = _load_scan_cache()
            self._populate_history()
            self._results_tree.clear()
            self._status.setText("Entry removed.")

    def _selected_history_entry(self):
        sel = self._history_tree.selectedItems()
        return sel[0].data(0, Qt.ItemDataRole.UserRole) if sel else None

    #    Open files                                                             

    def _open_selected(self):
        items = [i for i in self._results_tree.selectedItems()
                 if not i.isHidden()]
        if not items:
            return
        if len(items) > 10:
            if QMessageBox.question(
                    self, "Open Many Files",
                    f"Open {len(items)} files?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
            ) != QMessageBox.StandardButton.Yes:
                return
        mw = self._main_win
        for item in items:
            r = item.data(0, Qt.ItemDataRole.UserRole)
            if not r:
                continue
            try:
                if hasattr(mw, "_load_img_file_in_new_tab"):
                    mw._load_img_file_in_new_tab(r["path"])
                elif hasattr(mw, "load_img_file_in_new_tab"):
                    mw.load_img_file_in_new_tab(r["path"])
                else:
                    from apps.core.open import _load_img_file
                    _load_img_file(mw, r["path"])
            except Exception as e:
                if hasattr(mw, "log_message"):
                    mw.log_message(f"Error opening {r['name']}: {e}")

    def closeEvent(self, event):
        self._stop_scan()
        if self._scan_thread:
            self._scan_thread.wait(1000)
        super().closeEvent(event)


#    Public entry points                                                        

def scan_img_folder(main_window):  # vers 3
    """Always show the combined Recent Scans / New Scan dialog.

    If cache is empty a folder picker opens immediately so the first
    experience is still frictionless.  Subsequent calls show the history.
    """
    cache = _load_scan_cache()
    if cache:
        dlg = RecentScansDialog(main_window)
        dlg.exec()
    else:
        # No history yet — go straight to picker, results saved automatically
        _pick_and_scan(main_window)


def scan_img_recent(main_window):  # vers 1
    """Open the Recent Scans dialog directly."""
    dlg = RecentScansDialog(main_window)
    dlg.exec()


def _pick_and_scan(main_window):  # vers 1
    """Open a folder picker then launch a fresh scan."""
    folder = QFileDialog.getExistingDirectory(
        main_window,
        'Select Folder to Scan for IMG Files',
        getattr(main_window, 'last_open_dir', ''),
    )
    if not folder:
        return
    if hasattr(main_window, 'last_open_dir'):
        main_window.last_open_dir = folder
    dlg = ScanResultsDialog(main_window, folder)
    dlg.exec()


__all__ = ['scan_img_folder', 'scan_img_recent', 'ScanResultsDialog',
           'RecentScansDialog', 'ScanThread']
