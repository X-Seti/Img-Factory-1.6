"""
Third dock-snap diagnostic - adds the remaining real differences
between the vanilla test and map_workshop.py's actual setup:
  1. setDockOptions(AllowNestedDocks | AllowTabbedDocks)
  2. The custom QMainWindow::separator stylesheet
  3. Custom title bars via setTitleBarWidget (a plain bar with a
     label + close button + double-click-to-collapse handler,
     matching _make_dock_collapsible exactly)

Run this directly:

    python3 dock_snap_test_full.py

Then try the same drag/snap/redock test as before.

If this ONE breaks while the first two (dock_snap_test.py,
dock_snap_test_embedded.py) worked fine, one or more of these three
additions is the actual cause. To narrow down which, comment out
lines under "TEST 1", "TEST 2", "TEST 3" below one at a time (each
section is marked) and re-run - whichever one you comment out that
makes snapping work again is the culprit.
"""
import sys
from PyQt6.QtWidgets import (QApplication, QMainWindow, QDockWidget, QLabel,
                              QTextEdit, QWidget, QVBoxLayout, QHBoxLayout,
                              QToolButton)
from PyQt6.QtCore import Qt

app = QApplication(sys.argv)

outer_widget = QWidget()
outer_widget.resize(900, 600)
outer_widget.setWindowTitle("Dock Snap Test - FULL (dockOptions + stylesheet + custom title bars)")
layout = QVBoxLayout(outer_widget)

mw = QMainWindow()
mw.setWindowFlags(Qt.WindowType.Widget)

# --- TEST 1: dockOptions ---
mw.setDockOptions(
    QMainWindow.DockOption.AllowNestedDocks |
    QMainWindow.DockOption.AllowTabbedDocks)

# --- TEST 2: separator stylesheet ---
mw.setStyleSheet(
    "QMainWindow::separator { "
    "background: palette(mid); width: 5px; height: 5px; } "
    "QMainWindow::separator:hover { background: palette(highlight); }")

mw.setCentralWidget(QTextEdit("Central widget - drag the docks around this."))


def make_custom_titlebar(dock, title):
    """--- TEST 3: custom title bar, matching _make_dock_collapsible ---"""
    bar = QWidget()
    lay = QHBoxLayout(bar)
    lay.setContentsMargins(6, 2, 2, 2)
    lay.setSpacing(2)
    lbl = QLabel(title)
    lay.addWidget(lbl)
    lay.addStretch()
    close_btn = QToolButton()
    close_btn.setText("×")
    close_btn.setFixedSize(20, 20)
    close_btn.setAutoRaise(True)
    close_btn.clicked.connect(dock.close)
    lay.addWidget(close_btn)

    def _dbl_click(event, d=dock):
        content = d.widget()
        if content:
            content.setVisible(not content.isVisible())
    bar.mouseDoubleClickEvent = _dbl_click
    lbl.mouseDoubleClickEvent = _dbl_click
    dock.setTitleBarWidget(bar)


dock_a = QDockWidget("Dock A", mw)
dock_a.setWidget(QLabel("Dock A content"))
dock_a.setFeatures(
    QDockWidget.DockWidgetFeature.DockWidgetMovable |
    QDockWidget.DockWidgetFeature.DockWidgetFloatable |
    QDockWidget.DockWidgetFeature.DockWidgetClosable)
make_custom_titlebar(dock_a, "Dock A")  # comment out to test without TEST 3
mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, dock_a)

dock_b = QDockWidget("Dock B", mw)
dock_b.setWidget(QLabel("Dock B content"))
dock_b.setFeatures(
    QDockWidget.DockWidgetFeature.DockWidgetMovable |
    QDockWidget.DockWidgetFeature.DockWidgetFloatable |
    QDockWidget.DockWidgetFeature.DockWidgetClosable)
make_custom_titlebar(dock_b, "Dock B")  # comment out to test without TEST 3
mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, dock_b)
mw.splitDockWidget(dock_a, dock_b, Qt.Orientation.Vertical)

layout.addWidget(mw)
outer_widget.show()
sys.exit(app.exec())
