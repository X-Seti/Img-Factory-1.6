"""
Fourth dock-snap diagnostic - this time one of the docks contains
ANOTHER QMainWindow (with its own toolbar) as its content, exactly
matching map_workshop.py's real structure: the "Viewport" dock's
widget is inner_mw (a QMainWindow with its own ribbon toolbars),
which itself lives inside a QDockWidget, which lives inside outer_mw
(the top-level-ish QMainWindow that owns all the other docks).

Run this directly:

    python3 dock_snap_test_nested_mainwindow.py

Then try dragging "Dock B" (a normal dock, sitting alongside the
nested one) - does it show a snap preview / redock correctly?
Also try dragging the toolbar inside "Viewport" (the nested
QMainWindow's own ribbon) - does THAT snap within Viewport's own
edges?

If dragging Dock B (or the inner ribbon) breaks here while every
earlier, simpler test worked, the QMainWindow-inside-a-QDockWidget
nesting itself is the cause.
"""
import sys
from PyQt6.QtWidgets import (QApplication, QMainWindow, QDockWidget, QLabel,
                              QTextEdit, QWidget, QVBoxLayout, QToolBar)
from PyQt6.QtGui import QAction
from PyQt6.QtCore import Qt, QSize

app = QApplication(sys.argv)

outer_widget = QWidget()
outer_widget.resize(1000, 650)
outer_widget.setWindowTitle("Dock Snap Test - NESTED QMainWindow (matches Viewport dock structure)")
layout = QVBoxLayout(outer_widget)

outer_mw = QMainWindow()
outer_mw.setWindowFlags(Qt.WindowType.Widget)
outer_mw.setDockOptions(
    QMainWindow.DockOption.AllowNestedDocks |
    QMainWindow.DockOption.AllowTabbedDocks)

# Trivial empty central widget, same as the real app (outer_mw's real
# content all lives in dock widgets, not the central widget).
central_placeholder = QWidget()
central_placeholder.setMaximumSize(0, 0)
outer_mw.setCentralWidget(central_placeholder)

# --- The nested structure: inner_mw (its own QMainWindow, own toolbar)
# living inside the "Viewport" dock's widget slot ---
inner_mw = QMainWindow()
inner_mw.setWindowFlags(Qt.WindowType.Widget)
inner_mw.setCentralWidget(QTextEdit("Viewport content (e.g. the 3D view)"))

tb = QToolBar("World")
tb.setIconSize(QSize(20, 20))
tb.setMovable(True)
tb.setFloatable(True)
act = QAction("Some Tool", inner_mw)
tb.addAction(act)
inner_mw.addToolBar(Qt.ToolBarArea.TopToolBarArea, tb)

viewport_dock = QDockWidget("Viewport", outer_mw)
viewport_dock.setWidget(inner_mw)
viewport_dock.setFeatures(
    QDockWidget.DockWidgetFeature.DockWidgetMovable |
    QDockWidget.DockWidgetFeature.DockWidgetFloatable |
    QDockWidget.DockWidgetFeature.DockWidgetClosable)
outer_mw.addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, viewport_dock)

# --- A completely normal dock alongside it, same as Object
# Browser/Control Panel/etc. in the real app ---
dock_b = QDockWidget("Dock B", outer_mw)
dock_b.setWidget(QLabel("Dock B content"))
dock_b.setFeatures(
    QDockWidget.DockWidgetFeature.DockWidgetMovable |
    QDockWidget.DockWidgetFeature.DockWidgetFloatable |
    QDockWidget.DockWidgetFeature.DockWidgetClosable)
outer_mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, dock_b)

layout.addWidget(outer_mw)
outer_widget.show()
sys.exit(app.exec())
