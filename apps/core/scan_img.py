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

# ── Extensions we care about ──────────────────────────────────────────────────

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

# ── Quick version detection (no Qt, no full parse) ────────────────────────────

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


# ── Background scan thread ────────────────────────────────────────────────────

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


# ── Results dialog ─────────────────────────────────────────────────────────────

class ScanResultsDialog(QDialog):  # vers 1
    """Shows scan results and lets the user open selected IMG files."""

    def __init__(self, parent=None, root_path: str = ''):
        super().__init__(parent)
        self.root_path   = root_path
        self._results    = []   # list of result dicts
        self._thread     = None
        self._main_win   = parent
        self.setWindowTitle(f'IMG Scan — {os.path.basename(root_path) or root_path}')
        self.setMinimumSize(900, 580)
        self._build_ui()
        self._start_scan()

    # ── UI ──────────────────────────────────────────────────────────────────

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

    # ── Scan ────────────────────────────────────────────────────────────────

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

    # ── Tree population ──────────────────────────────────────────────────────

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

    # ── Filter ───────────────────────────────────────────────────────────────

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

    # ── Selection helpers ────────────────────────────────────────────────────

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

    # ── Open ─────────────────────────────────────────────────────────────────

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

    def closeEvent(self, event):
        self._stop_scan()
        if self._thread:
            self._thread.wait(1000)
        super().closeEvent(event)


# ── Public entry point ────────────────────────────────────────────────────────

def scan_img_folder(main_window):  # vers 1
    """Open a folder picker then launch the scan results dialog."""
    folder = QFileDialog.getExistingDirectory(
        main_window,
        'Select Folder to Scan for IMG Files',
        getattr(main_window, 'last_open_dir', ''),
    )
    if not folder:
        return

    # Remember for next time
    if hasattr(main_window, 'last_open_dir'):
        main_window.last_open_dir = folder

    dlg = ScanResultsDialog(main_window, folder)
    dlg.exec()


__all__ = ['scan_img_folder', 'ScanResultsDialog', 'ScanThread']
