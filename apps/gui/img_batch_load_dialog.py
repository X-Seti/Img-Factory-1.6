#this belongs in apps/gui/img_batch_load_dialog.py - Version: 1
# X-Seti - October 08 2026 - IMG Factory 1.6 - Batch IMG load window

"""
One window for loading many IMG archives: header with the current file,
scrolling per-file stats (or errors), timed close once all are done.
"""

##Methods list -
# __init__
# _advance
# _close_tick
# _finish
# _line
# closeEvent
# file_done
# file_failed
# file_started

import os
import time

from PyQt6.QtCore import QTimer
from PyQt6.QtWidgets import (QDialog, QHBoxLayout, QLabel, QListWidget,
                             QListWidgetItem, QProgressBar, QPushButton,
                             QVBoxLayout)


class ImgBatchLoadDialog(QDialog):
    """Non-modal batch load log; closes itself after CLOSE_AFTER seconds."""

    CLOSE_AFTER = 5

    def __init__(self, parent=None, total=1, title="Loading IMG archives"): #vers 1
        super().__init__(parent)
        self.setWindowTitle(title)
        self.resize(640, 420)
        self.setModal(False)
        self._total, self._done, self._failed = max(1, total), 0, 0
        self._started = {}
        self._countdown = self.CLOSE_AFTER
        lay = QVBoxLayout(self)
        top = QHBoxLayout()
        self._head = QLabel("Starting…")
        f = self._head.font(); f.setBold(True); self._head.setFont(f)
        self._count = QLabel(f"0 / {self._total}")
        top.addWidget(self._head, 1)
        top.addWidget(self._count)
        lay.addLayout(top)
        self._bar = QProgressBar()
        self._bar.setRange(0, self._total)
        lay.addWidget(self._bar)
        self._list = QListWidget()
        lay.addWidget(self._list, 1)
        row = QHBoxLayout()
        self._summary = QLabel("")
        self._close_btn = QPushButton("Close")
        self._close_btn.clicked.connect(self.close)
        row.addWidget(self._summary, 1)
        row.addWidget(self._close_btn)
        lay.addLayout(row)
        self._timer = QTimer(self)
        self._timer.timeout.connect(self._close_tick)

    def _line(self, text: str) -> None: #vers 1
        """Add a log line and keep it in view."""
        self._list.addItem(QListWidgetItem(text))
        self._list.scrollToBottom()

    def file_started(self, path: str) -> None: #vers 1
        """Header shows the file being loaded."""
        self._started[path] = time.monotonic()
        self._head.setText(f"Loading {os.path.basename(path)}")

    def file_done(self, path: str, img_file) -> None: #vers 1
        """One archive loaded: entries, version, size, time."""
        secs = time.monotonic() - self._started.get(path, time.monotonic())
        try:
            size = os.path.getsize(path) / (1024 * 1024)
        except OSError:
            size = 0.0
        ver = getattr(getattr(img_file, 'version', None), 'name', '?').replace('VERSION_', 'V')
        n = len(getattr(img_file, 'entries', []) or [])
        self._line(f"{os.path.basename(path)}  —  {n:,} entries  |  {ver}  |  {size:.1f} MB  |  {secs:.1f}s")
        self._done += 1
        self._advance()

    def file_failed(self, path: str, message: str) -> None: #vers 1
        """One archive failed: reason kept in the log, no popup."""
        first = (message or "unknown error").strip().splitlines()[0]
        self._line(f"{os.path.basename(path)}  —  FAILED: {first}")
        self._done += 1
        self._failed += 1
        self._advance()

    def _advance(self) -> None: #vers 1
        """Progress, then timed close once every file has reported."""
        self._bar.setValue(self._done)
        self._count.setText(f"{self._done} / {self._total}")
        if self._done >= self._total:
            self._finish()

    def _finish(self) -> None: #vers 1
        """All loaded: summary and countdown."""
        ok = self._done - self._failed
        self._head.setText(f"Loaded {ok} of {self._total} archive(s)")
        self._summary.setText(f"{self._failed} failed" if self._failed else "All loaded")
        if self._failed:
            return                      # keep open so failures can be read
        self._close_btn.setText(f"Close ({self._countdown})")
        self._timer.start(1000)

    def _close_tick(self) -> None: #vers 1
        """Countdown on the close button."""
        self._countdown -= 1
        if self._countdown <= 0:
            self._timer.stop()
            self.close()
            return
        self._close_btn.setText(f"Close ({self._countdown})")

    def closeEvent(self, event): #vers 1
        """Stop the countdown; owner forgets this dialog."""
        self._timer.stop()
        parent = self.parent()
        if parent is not None and getattr(parent, '_img_batch_dialog', None) is self:
            parent._img_batch_dialog = None
        super().closeEvent(event)
