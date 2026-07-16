#this belongs in apps/components/DP5_Workshop/depends/bitmaps_widget.py - Version: 2
# X-Seti - Jul 2026 - DP5 Workshop - Bitmaps dock widget
"""
Self-contained Bitmaps dock widget: dock container, collapsible title
bar, and content panel (bitmap list + Import/Export/Delete buttons).

create_bitmaps_dock(owner) builds the whole thing and returns the
QDockWidget. 'owner' is the DP5Workshop instance - UI signals wire back
to its existing callback methods (_on_bitmap_selected etc.).
"""

from PyQt6.QtWidgets import (
    QDockWidget, QWidget, QFrame, QVBoxLayout, QHBoxLayout,
    QLabel, QPushButton, QToolButton, QListWidget,
)
from PyQt6.QtCore import Qt


def create_bitmaps_dock(owner): #vers 1
    """Build the Bitmaps dock (container + title bar + content) and
    return it. Sets owner._bitmaps_dock and every widget attribute the
    rest of DP5Workshop already references (owner._bitmap_lw)."""
    panel = _create_content_panel(owner)

    dock = QDockWidget("Bitmaps", owner)
    dock.setObjectName("Bitmaps")
    dock.setWidget(panel)
    dock.setMinimumWidth(150)
    dock.setFeatures(
        QDockWidget.DockWidgetFeature.DockWidgetMovable |
        QDockWidget.DockWidgetFeature.DockWidgetFloatable)

    _make_collapsible_titlebar(owner, dock, "Bitmaps")
    owner._bitmaps_dock = dock
    return dock


def _apply_panel_stylesheet(owner, panel): #vers 1
    """Apply a theme-aware stylesheet via the shared
    depends/theme_style_helper.py builder, scoped to #dp5_bitmaps_panel.
    No fallback colours: if the theme lookup fails, the stylesheet is
    skipped entirely rather than invented."""
    from apps.components.DP5_Workshop.depends.theme_style_helper import (
        build_panel_stylesheet)
    ss = build_panel_stylesheet(owner, 'dp5_bitmaps_panel')
    if ss:
        panel.setStyleSheet(ss)


def _create_content_panel(owner): #vers 2
    panel = QFrame()
    panel.setObjectName("dp5_bitmaps_panel")
    panel.setFrameStyle(QFrame.Shape.StyledPanel)
    panel.setMinimumWidth(150)
    panel.setMaximumWidth(240)
    _apply_panel_stylesheet(owner, panel)

    layout = QVBoxLayout(panel)
    layout.setContentsMargins(5, 5, 5, 5)
    layout.setSpacing(4)

    # Bitmap list
    owner._bitmap_lw = QListWidget()
    owner._bitmap_lw.setObjectName("bitmap_list")
    owner._bitmap_lw.currentRowChanged.connect(owner._on_bitmap_selected)
    layout.addWidget(owner._bitmap_lw, 1)

    # Bottom button row — define all buttons BEFORE connecting signals
    import_btn = QPushButton("Import")
    import_btn.setFont(owner.button_font)
    import_btn.setToolTip("Import Bitmap")

    export_btn = QPushButton("Export")
    export_btn.setFont(owner.button_font)
    export_btn.setToolTip("Export current bitmap")

    del_btn = QPushButton("Delete")
    del_btn.setFont(owner.button_font)
    del_btn.setToolTip("Remove from list")

    # Connect signals after all three are defined
    import_btn.clicked.connect(owner._import_bitmap)
    export_btn.clicked.connect(owner._export_bitmap)
    del_btn.clicked.connect(owner._delete_bitmap)

    btn_row = QHBoxLayout()
    btn_row.addWidget(import_btn)
    btn_row.addWidget(export_btn)
    btn_row.addWidget(del_btn)
    layout.addLayout(btn_row)

    return panel


def _make_collapsible_titlebar(owner, dock, title): #vers 1
    """Custom title bar - double-click to collapse, float/close buttons,
    theme-aware background (panel_bg/bg_primary) built in from the
    start rather than bolted on after the fact."""
    bar = QWidget()
    bar.setObjectName("dp5_bitmaps_titlebar")
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
    owner._bitmaps_titlebar = bar


def _refresh_titlebar_color(owner, bar): #vers 1
    if owner.app_settings and hasattr(owner.app_settings, 'get_theme_colors'):
        tc = owner.app_settings.get_theme_colors()
        hexval = tc.get('panel_bg') or tc.get('bg_primary')
        if hexval:
            bar.setStyleSheet(
                f"QWidget#dp5_bitmaps_titlebar {{ background: {hexval}; }}")


def refresh_theme(owner): #vers 2
    """Re-apply the theme-aware panel stylesheet and title bar
    background on a live theme switch. Call from DP5Workshop.
    _apply_theme()."""
    dock = getattr(owner, '_bitmaps_dock', None)
    if dock is not None and dock.widget() is not None:
        _apply_panel_stylesheet(owner, dock.widget())
    bar = getattr(owner, '_bitmaps_titlebar', None)
    if bar is not None:
        _refresh_titlebar_color(owner, bar)
