#this belongs in apps/components/DP5_Workshop/depends/dock_helper.py - Version: 2
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 shared dock builder.

"""
make_dock(owner, title, panel_name, panel) wraps a content panel in a
QDockWidget with the DP5 collapsible title bar (double-click collapses,
float / close buttons) and the theme-aware panel stylesheet.
Used by the History, Layers and Palette Editor docks.
"""

##Methods list -
# ShrinkFrame
# make_dock
# refresh_dock_theme

from PyQt6.QtCore import QSize
from PyQt6.QtWidgets import QDockWidget, QFrame, QHBoxLayout, QLabel, QToolButton, QWidget


class ShrinkFrame(QFrame): #vers 1
    """Dock panel that can be narrowed below its content width."""

    def minimumSizeHint(self): #vers 1
        """Small width floor; content clips instead of locking the dock."""
        return QSize(100, super().minimumSizeHint().height())


def refresh_dock_theme(owner, dock): #vers 1
    """Re-apply panel stylesheet and title bar colour (theme switch)."""
    from apps.components.DP5_Workshop.depends.theme_style_helper import build_panel_stylesheet
    panel = dock.widget()
    ss = build_panel_stylesheet(owner, panel.objectName())
    if ss:
        panel.setStyleSheet(ss)
    bar = dock.titleBarWidget()
    if owner.app_settings and hasattr(owner.app_settings, 'get_theme_colors'):
        tc = owner.app_settings.get_theme_colors()
        hexval = tc.get('panel_bg') or tc.get('bg_primary')
        if hexval:
            bar.setStyleSheet(f"QWidget#{bar.objectName()} {{ background: {hexval}; }}")


def make_dock(owner, title: str, panel, min_w: int = 180): #vers 1
    """QDockWidget around panel (a QFrame with objectName set), DP5 title bar."""
    dock = QDockWidget(title, owner)
    dock.setObjectName(title)
    dock.setWidget(panel)
    dock.setMinimumWidth(min_w)
    dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                     QDockWidget.DockWidgetFeature.DockWidgetFloatable |
                     QDockWidget.DockWidgetFeature.DockWidgetClosable)
    bar = QWidget()
    bar.setObjectName(panel.objectName() + "_titlebar")
    lay = QHBoxLayout(bar)
    lay.setContentsMargins(6, 2, 2, 2)
    lay.setSpacing(2)
    lbl = QLabel(title)
    lbl.setFont(owner.button_font)
    lay.addWidget(lbl)
    lay.addStretch()
    for text, tip, fn in (("⧉", "Float/dock", lambda: owner._toggle_dock_floating(dock)),
                          ("×", "Close (View - Panels brings it back)", dock.close)):
        b = QToolButton()
        b.setText(text)
        b.setToolTip(tip)
        b.setFixedSize(20, 20)
        b.setAutoRaise(True)
        b.clicked.connect(fn)
        lay.addWidget(b)

    def dbl(_e):
        panel.setVisible(not panel.isVisible())
    bar.mouseDoubleClickEvent = dbl
    lbl.mouseDoubleClickEvent = dbl
    dock.setTitleBarWidget(bar)
    refresh_dock_theme(owner, dock)
    return dock
