#this belongs in apps/components/DP5_Workshop/depends/brushcolors_widget.py - Version: 4
# X-Seti - Jul 2026 - DP5 Workshop - Brush & Colors dock widget
"""
Self-contained Brush & Colors dock widget: dock container, collapsible
title bar, and content panel (size slider, snap/grid toggles, FG/BG
swatch, brush thumbnail, zoom buttons, opacity slider, colour history).

Extracted from dp5_workshop.py so this widget can be built/maintained
independently and, later, toggled on/off via a widget manager in
Settings.

create_brush_colors_dock(owner) builds the whole thing and returns the
QDockWidget, ready to be added via owner's outer_mw.addDockWidget(...).
'owner' is the DP5Workshop instance - this module wires UI signals back
to owner's existing callback methods (_set_brush_size etc.) and reads
owner.dp5_settings / owner.app_settings / owner.panel_font / owner.
button_font, rather than duplicating that logic here. FGBGSwatch and
BrushThumbnail are imported locally (inside the function, not at
module load time) from dp5_workshop.py to avoid a circular import,
since dp5_workshop.py imports this module too.
"""

from PyQt6.QtWidgets import (
    QDockWidget, QWidget, QFrame, QVBoxLayout, QHBoxLayout,
    QLabel, QSlider, QCheckBox, QPushButton, QToolButton,
)
from PyQt6.QtCore import Qt, QSize
from PyQt6.QtGui import QColor


def create_brush_colors_dock(owner): #vers 1
    """Build the Brush & Colors dock (container + title bar + content)
    and return it. Also sets owner._brush_colors_dock and every widget
    attribute the rest of DP5Workshop already references (owner.
    _size_sl, owner._fgbg_swatch, owner._color_hist_btns, etc.) so
    nothing elsewhere in dp5_workshop.py needs to change."""
    panel = _create_content_panel(owner)

    dock = QDockWidget("Brush & Colors", owner)
    dock.setObjectName("Brush & Colors")
    dock.setWidget(panel)
    dock.setMinimumWidth(180)
    dock.setFeatures(
        QDockWidget.DockWidgetFeature.DockWidgetMovable |
        QDockWidget.DockWidgetFeature.DockWidgetFloatable)

    _make_collapsible_titlebar(owner, dock, "Brush & Colors")
    owner._brush_colors_dock = dock
    return dock


def _apply_panel_stylesheet(owner, panel): #vers 2
    """Apply a theme-aware stylesheet to the panel via the shared
    depends/theme_style_helper.py builder, scoped to
    #dp5_brushcolors_panel. No fallback colours: if the theme lookup
    fails, the stylesheet is skipped entirely rather than invented."""
    from apps.components.DP5_Workshop.depends.theme_style_helper import (
        build_panel_stylesheet)
    ss = build_panel_stylesheet(owner, 'dp5_brushcolors_panel')
    if ss:
        panel.setStyleSheet(ss)


def _create_content_panel(owner): #vers 1
    """Brush & Colors dock content - brush size, snap/grid toggles,
    FG/BG swatch + brush thumbnail, zoom in/out, opacity, colour
    history."""
    from apps.components.DP5_Workshop.dp5_workshop import (
        FGBGSwatch, BrushThumbnail, SVGIconFactory)

    panel = QFrame()
    panel.setObjectName("dp5_brushcolors_panel")
    panel.setFrameStyle(QFrame.Shape.StyledPanel)
    _apply_panel_stylesheet(owner, panel)
    icon_color = owner._get_icon_color()

    layout = QVBoxLayout(panel)
    layout.setContentsMargins(4, 4, 4, 4)
    layout.setSpacing(3)

    #    Brush size slider + value label
    size_hdr = QHBoxLayout()
    size_lbl = QLabel("Size")
    size_lbl.setFont(owner.panel_font)
    size_hdr.addWidget(size_lbl)
    owner._size_sl = QSlider(Qt.Orientation.Horizontal)
    owner._size_sl.setRange(1, 20)
    owner._size_sl.setValue(1)
    owner._size_sl.setMinimumHeight(24)
    owner._size_sl.valueChanged.connect(owner._set_brush_size)
    size_hdr.addWidget(owner._size_sl)
    owner._size_val_lbl = QLabel("1")
    owner._size_val_lbl.setAlignment(Qt.AlignmentFlag.AlignRight |
                                     Qt.AlignmentFlag.AlignVCenter)
    owner._size_val_lbl.setFont(owner.panel_font)
    owner._size_val_lbl.setFixedWidth(28)
    size_hdr.addWidget(owner._size_val_lbl)
    layout.addLayout(size_hdr)

    #    Snap to grid toggle
    snap_row = QHBoxLayout()
    owner._snap_chk = QCheckBox("Snap to grid")
    owner._snap_chk.setFont(owner.panel_font)
    owner._snap_chk.setChecked(False)
    owner._snap_chk.setToolTip("Snap drawing to pixel grid")
    owner._snap_chk.toggled.connect(owner._set_snap_grid)
    owner._grid_chk2 = QCheckBox("Show grid")
    owner._grid_chk2.setFont(owner.panel_font)
    owner._grid_chk2.setChecked(owner.dp5_settings.get('show_pixel_grid'))
    owner._grid_chk2.toggled.connect(owner._set_show_grid)
    owner._zoom_lbl = QLabel(f"{owner._canvas_zoom}×")
    owner._zoom_lbl.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
    owner._zoom_lbl.setFont(owner.panel_font)
    snap_row.addWidget(owner._snap_chk)
    snap_row.addWidget(owner._grid_chk2)
    snap_row.addWidget(owner._zoom_lbl)
    layout.addLayout(snap_row)

    layout.addSpacing(4)

    #    FG / BG swatch  +  brush thumbnail
    fgbg_row_lbl = QHBoxLayout()
    fgbg_lbl = QLabel("FG / BG")
    fgbg_lbl.setFont(owner.panel_font)
    fgbg_row_lbl.addWidget(fgbg_lbl)
    brush_lbl = QLabel("Brush")
    brush_lbl.setFont(owner.panel_font)
    fgbg_row_lbl.addWidget(brush_lbl)
    layout.addLayout(fgbg_row_lbl)

    fgbg_row = QHBoxLayout()
    fgbg_row.setSpacing(4)

    owner._fgbg_swatch = FGBGSwatch()
    owner._fgbg_swatch.fg_changed.connect(owner._on_fg_changed)
    owner._fgbg_swatch.bg_changed.connect(owner._on_bg_changed)
    fgbg_row.addWidget(owner._fgbg_swatch)

    owner._brush_thumb = BrushThumbnail()
    owner._brush_thumb.stamp_requested.connect(owner._activate_stamp_mode)
    owner._brush_thumb.clear_requested.connect(owner._clear_brush)
    fgbg_row.addWidget(owner._brush_thumb)

    zoom_stack = QVBoxLayout()
    zoom_stack.setSpacing(2)
    zoom_in_btn = QPushButton()
    zoom_in_btn.setFixedSize(24, 24)
    zoom_in_btn.setToolTip("Zoom in")
    zoom_in_btn.clicked.connect(lambda: owner._set_zoom(
        owner._canvas_zoom * 1.25 if owner._canvas_zoom < 1 else min(64, owner._canvas_zoom + 1)))
    try:
        zoom_in_btn.setIcon(SVGIconFactory.zoom_in_icon(14, icon_color))
        zoom_in_btn.setIconSize(QSize(14, 14))
    except Exception:
        zoom_in_btn.setText("+")
    zoom_out_btn = QPushButton()
    zoom_out_btn.setFixedSize(24, 24)
    zoom_out_btn.setToolTip("Zoom out")
    zoom_out_btn.clicked.connect(lambda: owner._set_zoom(
        max(0.05, owner._canvas_zoom * 0.8 if owner._canvas_zoom <= 1 else owner._canvas_zoom - 1)))
    try:
        zoom_out_btn.setIcon(SVGIconFactory.zoom_out_icon(14, icon_color))
        zoom_out_btn.setIconSize(QSize(14, 14))
    except Exception:
        zoom_out_btn.setText("-")
    zoom_stack.addWidget(zoom_in_btn)
    zoom_stack.addWidget(zoom_out_btn)
    fgbg_row.addLayout(zoom_stack)

    layout.addLayout(fgbg_row)

    #    Opacity
    op_row = QHBoxLayout()
    op_lbl = QLabel("Opacity")
    op_lbl.setFont(owner.panel_font)
    op_row.addWidget(op_lbl)
    owner._opacity_sl = QSlider(Qt.Orientation.Horizontal)
    owner._opacity_sl.setRange(0, 100)
    owner._opacity_sl.setValue(100)
    owner._opacity_sl.setMinimumHeight(24)
    owner._opacity_sl.valueChanged.connect(owner._set_opacity)
    op_row.addWidget(owner._opacity_sl)
    owner._opacity_val_lbl = QLabel("100%")
    owner._opacity_val_lbl.setFont(owner.panel_font)
    owner._opacity_val_lbl.setFixedWidth(34)
    owner._opacity_val_lbl.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
    op_row.addWidget(owner._opacity_val_lbl)
    layout.addLayout(op_row)

    #    Colour history
    hist_lbl = QLabel("Recent")
    hist_lbl.setFont(owner.panel_font)
    layout.addWidget(hist_lbl)
    hist_row = QHBoxLayout()
    hist_row.setSpacing(2)
    owner._color_history = []
    owner._color_hist_btns = []
    for _ in range(12):
        b = QPushButton()
        b.setFixedSize(12, 12)
        b.setObjectName("dp5_history_empty_slot")
        _style_empty_history_slot(owner, b)
        b.setEnabled(False)
        hist_row.addWidget(b)
        owner._color_hist_btns.append(b)
    layout.addLayout(hist_row)
    layout.addStretch()

    return panel


def _style_empty_history_slot(owner, btn): #vers 1
    """Theme-aware empty colour-history slot styling - reads panel_bg/
    bg_secondary + border from the active theme instead of palette(base)/
    palette(mid), which don't reflect a stylesheet-only theme system."""
    if owner.app_settings and hasattr(owner.app_settings, 'get_theme_colors'):
        tc = owner.app_settings.get_theme_colors()
        bg = tc.get('panel_bg') or tc.get('bg_secondary')
        border = tc.get('border')
        if bg and border:
            btn.setStyleSheet(
                f"QPushButton#dp5_history_empty_slot {{ background:{bg}; "
                f"border:1px solid {border}; }}")
            return
    btn.setStyleSheet(
        "QPushButton#dp5_history_empty_slot { background:palette(base); "
        "border:1px solid palette(mid); }")


def _make_collapsible_titlebar(owner, dock, title): #vers 1
    """Custom title bar for the dock: double-click anywhere on the title
    (label or empty bar area) to collapse the dock down to just this
    title bar, hiding its content; double-click again to restore. Keeps
    float/close buttons so nothing is lost versus Qt's native title bar.
    Background is theme-aware (panel_bg/bg_primary) - a plain QWidget
    isn't reached by the shared panel-effect system (which only walks
    QFrame/QGroupBox), so without this it stays fully transparent and
    lets whatever's behind the dock bleed through."""
    bar = QWidget()
    bar.setObjectName("dp5_brushcolors_titlebar")
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
    owner._brush_colors_titlebar = bar


def _refresh_titlebar_color(owner, bar): #vers 1
    if owner.app_settings and hasattr(owner.app_settings, 'get_theme_colors'):
        tc = owner.app_settings.get_theme_colors()
        hexval = tc.get('panel_bg') or tc.get('bg_primary')
        if hexval:
            bar.setStyleSheet(
                f"QWidget#dp5_brushcolors_titlebar {{ background: {hexval}; }}")


def refresh_theme(owner): #vers 2
    """Re-apply theme-aware colours to the panel stylesheet, title bar,
    and the Recent colour history's empty slots. Call from DP5Workshop.
    _apply_theme() on every theme switch - all are set once at creation
    and won't pick up a later theme change on their own otherwise."""
    dock = getattr(owner, '_brush_colors_dock', None)
    if dock is not None and dock.widget() is not None:
        _apply_panel_stylesheet(owner, dock.widget())
    bar = getattr(owner, '_brush_colors_titlebar', None)
    if bar is not None:
        _refresh_titlebar_color(owner, bar)
    for i, btn in enumerate(getattr(owner, '_color_hist_btns', [])):
        if i >= len(getattr(owner, '_color_history', [])):
            _style_empty_history_slot(owner, btn)
