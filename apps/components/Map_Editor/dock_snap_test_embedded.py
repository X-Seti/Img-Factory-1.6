"""
Second dock-snap diagnostic - this time the QMainWindow is embedded as
a NON-top-level widget inside a wrapper, matching exactly how
map_workshop.py's outer_mw is set up (Qt.WindowType.Widget, placed
inside a QVBoxLayout on another widget) instead of being shown
directly as its own top-level window like the first test script.

Run this directly:

    python3 dock_snap_test_embedded.py

Then try the same drag/snap/redock test as before.

If THIS one fails (no snap preview, can't redock) while the first
vanilla test (dock_snap_test.py) worked fine, that confirms the
non-top-level embedding itself is the cause - Qt's dock-drag
hit-testing likely needs the QMainWindow to own its own native
top-level window to correctly track screen-space drop zones during a
drag, and silently breaks (drag still works, but snap detection
doesn't) when it's just an embedded widget instead.
"""
import sys
from PyQt6.QtWidgets import (QApplication, QMainWindow, QDockWidget, QLabel,
                              QTextEdit, QWidget, QVBoxLayout)
from PyQt6.QtCore import Qt

app = QApplication(sys.argv)

# Outer top-level widget - mimics MapWorkshop itself (a QWidget, not a
# QMainWindow) being the real top-level window.
outer_widget = QWidget()
outer_widget.resize(900, 600)
outer_widget.setWindowTitle("Dock Snap Test - EMBEDDED QMainWindow (matches map_workshop.py)")
layout = QVBoxLayout(outer_widget)

# Inner QMainWindow - embedded as a plain widget, NOT top-level. This
# is exactly what outer_mw is in map_workshop.py.
mw = QMainWindow()
mw.setWindowFlags(Qt.WindowType.Widget)
mw.setCentralWidget(QTextEdit("Central widget - drag the docks around this."))

dock_a = QDockWidget("Dock A", mw)
dock_a.setWidget(QLabel("Dock A content"))
dock_a.setFeatures(
    QDockWidget.DockWidgetFeature.DockWidgetMovable |
    QDockWidget.DockWidgetFeature.DockWidgetFloatable |
    QDockWidget.DockWidgetFeature.DockWidgetClosable)
mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, dock_a)

dock_b = QDockWidget("Dock B", mw)
dock_b.setWidget(QLabel("Dock B content"))
dock_b.setFeatures(
    QDockWidget.DockWidgetFeature.DockWidgetMovable |
    QDockWidget.DockWidgetFeature.DockWidgetFloatable |
    QDockWidget.DockWidgetFeature.DockWidgetClosable)
mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, dock_b)
mw.splitDockWidget(dock_a, dock_b, Qt.Orientation.Vertical)

layout.addWidget(mw)
outer_widget.show()
sys.exit(app.exec())
