#this belongs in apps/components/DP5_Workshop/depends/userpalette_widget.py - Version: 1
# X-Seti - Jul 2026 - DP5 Workshop - User Palette dock widget
"""
Self-contained User Palette dock widget: dock container, collapsible
title bar, and content panel (retro preset button + palette grid).

create_user_palette_dock(owner) builds the whole thing and returns
the QDockWidget. 'owner' is the DP5Workshop instance - UI signals wire
back to its existing callback methods (_show_retro_menu etc.).
_AutoCellPaletteGrid is imported locally (inside the function) from
dp5_workshop.py to avoid a circular import.
"""

from PyQt6.QtWidgets import (
    QDockWidget, QWidget, QFrame, QVBoxLayout, QHBoxLayout,
    QLabel, QPushButton, QToolButton, QScrollArea,
)
from PyQt6.QtCore import Qt


def create_user_palette_dock(owner): #vers 1
    """Build the User Palette dock (container + title bar + content)
    and return it. Sets owner._user_palette_dock and every widget
    attribute the rest of DP5Workshop already references
    (owner._retro_btn, owner._user_pal_grid, etc.)."""
    panel = _create_content_panel(owner)

    dock = QDockWidget("User Palette", owner)
    dock.setObjectName("User Palette")
    dock.setWidget(panel)
    dock.setMinimumWidth(180)
    dock.setFeatures(
        QDockWidget.DockWidgetFeature.DockWidgetMovable |
        QDockWidget.DockWidgetFeature.DockWidgetFloatable)

    _make_collapsible_titlebar(owner, dock, "User Palette")
    owner._user_palette_dock = dock
    return dock


def _create_content_panel(owner): #vers 1
    """Retro presets + palette grid."""
    from apps.components.DP5_Workshop.dp5_workshop import _AutoCellPaletteGrid

    panel = QFrame()
    panel.setFrameStyle(QFrame.Shape.StyledPanel)

    layout = QVBoxLayout(panel)
    layout.setContentsMargins(4, 4, 4, 4)
    layout.setSpacing(3)

    user_pal_hdr = QHBoxLayout()
    owner._retro_btn = QPushButton("Amiga AGA WB")
    owner._retro_btn.setFont(owner.button_font)
    owner._retro_btn.setFixedHeight(24)
    owner._retro_btn.setToolTip("User palette — choose retro preset")
    owner._retro_btn.clicked.connect(owner._show_retro_menu)
    user_pal_hdr.addWidget(owner._retro_btn)
    owner._pal_dither_btn = QPushButton("Dith")
    owner._pal_dither_btn.setVisible(False)  # dither via File menu instead
    layout.addLayout(user_pal_hdr)

    user_rows = owner.dp5_settings.get('user_pal_rows')
    user_pal_max_h = max(user_rows * 12 + 2, 130)
    owner._user_pal_grid = _AutoCellPaletteGrid()
    owner._user_pal_grid.color_picked.connect(owner._on_user_palette_color)
    user_pal_scroll = QScrollArea()
    user_pal_scroll.setWidget(owner._user_pal_grid)
    user_pal_scroll.setWidgetResizable(True)
    user_pal_scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
    user_pal_scroll.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
    user_pal_scroll.setMaximumHeight(user_pal_max_h)
    user_pal_scroll.setMinimumHeight(12)
    owner._user_pal_scroll = user_pal_scroll
    layout.addWidget(user_pal_scroll, stretch=1)

    # Load default retro palette
    owner._apply_retro_palette(owner.dp5_settings.get('retro_palette'))

    return panel


def _make_collapsible_titlebar(owner, dock, title): #vers 1
    """Custom title bar - double-click to collapse, float/close buttons,
    theme-aware background (panel_bg/bg_primary) built in from the
    start rather than bolted on after the fact."""
    bar = QWidget()
    bar.setObjectName("dp5_userpalette_titlebar")
    _refresh_titlebar_color(owner, bar)

    lay = QHBoxLayout(bar)
    lay.setContentsMargins(6, 2, 2, 2)
    lay.setSpacing(2)

    lbl = QLabel(title)
    lbl.setFont(owner.button_font)
    lay.addWidget(lbl)
    lay.addStretch()

    float_btn = QToolButton()
    float_btn.setText("⧉")
    float_btn.setToolTip("Float/dock")
    float_btn.setFixedSize(20, 20)
    float_btn.setAutoRaise(True)
    float_btn.clicked.connect(lambda: owner._toggle_dock_floating(dock))
    lay.addWidget(float_btn)

    close_btn = QToolButton()
    close_btn.setText("×")
    close_btn.setToolTip("Close (use the View menu or another dock's "
                          "right-click menu to bring it back)")
    close_btn.setFixedSize(20, 20)
    close_btn.setAutoRaise(True)
    close_btn.clicked.connect(dock.close)
    lay.addWidget(close_btn)

    def _dbl_click(event, d=dock):  #vers 1
        content = d.widget()
        if content:
            content.setVisible(not content.isVisible())
    bar.mouseDoubleClickEvent = _dbl_click
    lbl.mouseDoubleClickEvent = _dbl_click

    dock.setTitleBarWidget(bar)
    owner._user_palette_titlebar = bar


def _refresh_titlebar_color(owner, bar): #vers 1
    if owner.app_settings and hasattr(owner.app_settings, 'get_theme_colors'):
        tc = owner.app_settings.get_theme_colors()
        hexval = tc.get('panel_bg') or tc.get('bg_primary')
        if hexval:
            bar.setStyleSheet(
                f"QWidget#dp5_userpalette_titlebar {{ background: {hexval}; }}")


def refresh_theme(owner): #vers 1
    """Re-apply the theme-aware title bar background on a live theme
    switch. Call from DP5Workshop._apply_theme()."""
    bar = getattr(owner, '_user_palette_titlebar', None)
    if bar is not None:
        _refresh_titlebar_color(owner, bar)
