"""
Fifth dock-snap diagnostic - replicates the REAL app's full scale and
complexity: several hidden docks sharing the same dock area as a
visible one (like Files/Models/Frame Hierarchy/Textures, all hidden,
sharing LeftDockWidgetArea with the visible Viewport dock), plus a
splitDockWidget() chain on the right side (like Object Browser -> IPL
Inst File -> Control Panel), plus setDockNestingEnabled(True) and
resizeDocks().

Run this directly:

    python3 dock_snap_test_full_scale.py

Then try dragging any of the RIGHT-side docks (Obj Browser/IPL/
Control), and the Viewport dock on the left. Does snap-preview show?
Does redocking after floating work?

If THIS one breaks while every simpler test worked, something about
the combination of hidden docks sharing an area with a visible one,
or the splitDockWidget chain, or the sheer number of docks, is the
actual cause - and we can start removing pieces from this test to
narrow down which specifically.
"""
import sys
from PyQt6.QtWidgets import (QApplication, QMainWindow, QDockWidget, QLabel,
                              QTextEdit, QWidget, QVBoxLayout)
from PyQt6.QtCore import Qt

app = QApplication(sys.argv)

outer_widget = QWidget()
outer_widget.resize(1200, 700)
outer_widget.setWindowTitle("Dock Snap Test - FULL SCALE (hidden docks + split chain)")
layout = QVBoxLayout(outer_widget)

outer_mw = QMainWindow()
outer_mw.setWindowFlags(Qt.WindowType.Widget)
outer_mw.setDockOptions(
    QMainWindow.DockOption.AllowNestedDocks |
    QMainWindow.DockOption.AllowTabbedDocks)
outer_mw.setDockNestingEnabled(True)

central_placeholder = QWidget()
central_placeholder.setMaximumSize(0, 0)
outer_mw.setCentralWidget(central_placeholder)


def make_dock(title, area, hidden=False):
    d = QDockWidget(title, outer_mw)
    d.setObjectName(title)
    d.setWidget(QLabel(f"{title} content"))
    d.setFeatures(
        QDockWidget.DockWidgetFeature.DockWidgetMovable |
        QDockWidget.DockWidgetFeature.DockWidgetFloatable |
        QDockWidget.DockWidgetFeature.DockWidgetClosable)
    outer_mw.addDockWidget(area, d)
    if hidden:
        d.setVisible(False)
    return d


# Left area: Viewport (visible) sharing the area with 4 hidden docks,
# same as Files/Models/Frame Hierarchy/Textures in the real app.
viewport = make_dock("Viewport", Qt.DockWidgetArea.LeftDockWidgetArea, hidden=False)
files_d = make_dock("Files", Qt.DockWidgetArea.LeftDockWidgetArea, hidden=True)
models_d = make_dock("Models", Qt.DockWidgetArea.LeftDockWidgetArea, hidden=True)
frame_d = make_dock("Frame Hierarchy", Qt.DockWidgetArea.LeftDockWidgetArea, hidden=True)
tex_d = make_dock("Textures", Qt.DockWidgetArea.LeftDockWidgetArea, hidden=True)
outer_mw.resizeDocks([viewport], [900], Qt.Orientation.Horizontal)

# Right area: Object Browser -> IPL Inst File -> Control Panel, chained
# with splitDockWidget exactly like the real app.
obj_browser = make_dock("Object Browser", Qt.DockWidgetArea.RightDockWidgetArea)
ipl_inst = make_dock("IPL Inst File", Qt.DockWidgetArea.RightDockWidgetArea)
outer_mw.splitDockWidget(obj_browser, ipl_inst, Qt.Orientation.Vertical)
control_panel = make_dock("Control Panel", Qt.DockWidgetArea.RightDockWidgetArea)
outer_mw.splitDockWidget(ipl_inst, control_panel, Qt.Orientation.Vertical)

layout.addWidget(outer_mw)
outer_widget.show()
sys.exit(app.exec())
