#this belongs in apps/methods/startup_splash.py - Version: 1
# X-Seti - October 29 2026 - IMG Factory 1.6 - Startup splash

"""
Startup splash: app icon, progress bar and the current step while IMG Factory
loads. startup_step() is safe to call before / without a splash. Each step's
time is printed ([startup] ...) so slow steps can be found.
"""

##Methods list -
# show_startup_splash
# startup_finish
# startup_step
#
##class StartupSplash: -
# __init__
# step

import time

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QApplication, QLabel, QProgressBar, QVBoxLayout, QWidget

_splash = None


class StartupSplash(QWidget): #vers 1
    """Frameless centred box: title, progress bar, step text."""

    def __init__(self, title: str): #vers 1
        super().__init__(None, Qt.WindowType.SplashScreen | Qt.WindowType.FramelessWindowHint
                         | Qt.WindowType.WindowStaysOnTopHint)
        self.setFixedSize(440, 150)
        lay = QVBoxLayout(self)
        lay.setContentsMargins(20, 16, 20, 16)
        head = QLabel(title)
        f = head.font()
        f.setPointSize(f.pointSize() + 6)
        f.setBold(True)
        head.setFont(f)
        head.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._bar = QProgressBar()
        self._bar.setRange(0, 100)
        self._bar.setTextVisible(False)
        self._bar.setFixedHeight(10)
        self._text = QLabel("Starting...")
        self._text.setAlignment(Qt.AlignmentFlag.AlignCenter)
        lay.addWidget(head)
        lay.addStretch(1)
        lay.addWidget(self._bar)
        lay.addWidget(self._text)
        self.setStyleSheet("StartupSplash { border: 1px solid palette(mid); background: palette(window); }")
        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        screen = QApplication.primaryScreen()
        if screen is not None:
            g = screen.availableGeometry()
            self.move(g.center().x() - self.width() // 2, g.center().y() - self.height() // 2)
        self._last = self._t0 = time.monotonic()
        self._label = "Starting"

    def step(self, text: str, value: int): #vers 1
        """Show a step; prints how long the previous one took."""
        now = time.monotonic()
        print(f"[startup] {self._label}: {now - self._last:.2f}s")
        self._last, self._label = now, text
        self._bar.setValue(value)
        self._text.setText(text)
        QApplication.processEvents()


def show_startup_splash(title: str = "IMG Factory 1.6"): #vers 1
    """Create and show the splash (QApplication must exist)."""
    global _splash
    _splash = StartupSplash(title)
    _splash.show()
    QApplication.processEvents()
    return _splash


def startup_step(text: str, value: int): #vers 1
    """Advance the splash if one is showing."""
    if _splash is not None:
        _splash.step(text, value)


def startup_finish(): #vers 1
    """Close the splash; prints the last step and the total time."""
    global _splash
    if _splash is None:
        return
    _splash.step("Ready", 100)
    print(f"[startup] total: {time.monotonic() - _splash._t0:.2f}s")
    _splash.close()
    _splash.deleteLater()
    _splash = None
