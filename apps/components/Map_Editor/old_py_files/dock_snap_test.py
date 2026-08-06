"""
Minimal vanilla PyQt6 dock-widget test - no custom code, no theming,
nothing from map_workshop.py. Run this directly:

    python3 dock_snap_test.py

Then try:
  1. Drag "Dock A" or "Dock B" by its (native, unmodified) title bar.
  2. Watch for the blue/highlighted snap-preview rectangle as you drag
     near an edge of the window.
  3. Drop it there - does it redock?
  4. Drag it fully off the window to float it, then drag it back onto
     an edge - does it redock?

If this ALSO fails to show a snap preview or redock via drag, that's
airtight proof the issue is Qt/window-manager/compositor level, not
anything in map_workshop.py - nothing here is custom.
"""
import sys
from PyQt6.QtWidgets import QApplication, QMainWindow, QDockWidget, QLabel, QTextEdit
from PyQt6.QtCore import Qt

app = QApplication(sys.argv)

mw = QMainWindow()
mw.resize(900, 600)
mw.setWindowTitle("Dock Snap Test - vanilla PyQt6, no custom code")
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

mw.show()
sys.exit(app.exec())
