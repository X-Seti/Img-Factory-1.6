#this belongs in apps/components/DP5_Workshop/depends/imagepalette_widget.py - Version: 3
# X-Seti - Jul 2026 - DP5 Workshop - Image Palette dock widget
"""
Self-contained Image Palette dock widget: dock container, collapsible
title bar, and content panel (bit depth combo, Apply/Group buttons,
palette grid).

create_image_palette_dock(owner) builds the whole thing and returns
the QDockWidget. 'owner' is the DP5Workshop instance - UI signals wire
back to its existing callback methods (_apply_bit_depth etc.).
PaletteGrid is imported locally (inside the function) from
dp5_workshop.py to avoid a circular import.
"""

from PyQt6.QtWidgets import (
    QDockWidget, QWidget, QFrame, QVBoxLayout, QHBoxLayout,
    QLabel, QPushButton, QToolButton, QComboBox, QScrollArea,
)
from PyQt6.QtCore import Qt


def create_image_palette_dock(owner): #vers 1
    """Build the Image Palette dock (container + title bar + content)
    and return it. Sets owner._img_palette_dock and every widget
    attribute the rest of DP5Workshop already references
    (owner._bit_depth_combo, owner.pal_bar, etc.)."""
    panel = _create_content_panel(owner)

    dock = QDockWidget("Image Palette", owner)
    dock.setObjectName("Image Palette")
    dock.setWidget(panel)
    dock.setMinimumWidth(180)
    dock.setFeatures(
        QDockWidget.DockWidgetFeature.DockWidgetMovable |
        QDockWidget.DockWidgetFeature.DockWidgetFloatable)

    _make_collapsible_titlebar(owner, dock, "Image Palette")
    owner._img_palette_dock = dock
    return dock


def _apply_panel_stylesheet(owner, panel): #vers 1
    """Apply a theme-aware stylesheet via the shared
    depends/theme_style_helper.py builder, scoped to
    #dp5_imagepalette_panel. No fallback colours: if the theme lookup
    fails, the stylesheet is skipped entirely rather than invented."""
    from apps.components.DP5_Workshop.depends.theme_style_helper import (
        build_panel_stylesheet)
    ss = build_panel_stylesheet(owner, 'dp5_imagepalette_panel')
    if ss:
        panel.setStyleSheet(ss)


def _create_content_panel(owner): #vers 2
    """Bit depth quantization + palette grid."""
    from apps.components.DP5_Workshop.dp5_workshop import PaletteGrid

    panel = QFrame()
    panel.setObjectName("dp5_imagepalette_panel")
    panel.setFrameStyle(QFrame.Shape.StyledPanel)
    _apply_panel_stylesheet(owner, panel)

    layout = QVBoxLayout(panel)
    layout.setContentsMargins(4, 4, 4, 4)
    layout.setSpacing(3)

    img_pal_ctrl = QHBoxLayout()
    owner._bit_depth_combo = QComboBox()
    owner._bit_depth_combo.setFont(owner.panel_font)
    owner._bit_depth_combo.addItems(["32bit", "24bit", "16bit", "8bit"])
    owner._bit_depth_combo.setFixedHeight(24)
    owner._bit_depth_combo.setFixedWidth(40)
    owner._bit_depth_combo.setToolTip("Colour depth for quantization")
    img_pal_ctrl.addWidget(owner._bit_depth_combo)
    owner._img_pal_blend_btn = QPushButton("Blend")
    owner._img_pal_blend_btn.setFont(owner.button_font)
    owner._img_pal_blend_btn.setFixedHeight(24)
    owner._img_pal_blend_btn.setToolTip(
        "Blend canvas colours into an averaged palette (quantize-cluster\n"
        "colour averaging) and update the palette grid - a creative effect,\n"
        "not the same as a correct bit-depth reduction (use Apply for that)")
    owner._img_pal_blend_btn.clicked.connect(owner._blend_palette_colors)
    img_pal_ctrl.addWidget(owner._img_pal_blend_btn)
    img_pal_apply_btn = QPushButton("Apply")
    img_pal_apply_btn.setFont(owner.button_font)
    img_pal_apply_btn.setFixedHeight(24)
    img_pal_apply_btn.setToolTip("Quantize canvas to selected bit depth")
    img_pal_apply_btn.clicked.connect(owner._apply_bit_depth)
    img_pal_ctrl.addWidget(img_pal_apply_btn)
    owner._img_pal_group_btn = QPushButton("Group")
    owner._img_pal_group_btn.setFont(owner.button_font)
    owner._img_pal_group_btn.setFixedHeight(24)
    owner._img_pal_group_btn.setToolTip("Sort palette by hue — click to toggle asc/desc")
    owner._img_pal_group_btn.clicked.connect(owner._group_palette)
    img_pal_ctrl.addWidget(owner._img_pal_group_btn)
    owner._group_palette_asc = True
    layout.addLayout(img_pal_ctrl)

    img_cols = owner.dp5_settings.get('img_pal_cols')
    owner.pal_bar = PaletteGrid(cols=img_cols, cell=12)
    owner.pal_bar.color_picked.connect(owner._on_image_palette_color)
    owner._img_pal_scroll = QScrollArea()
    owner._img_pal_scroll.setWidget(owner.pal_bar)
    owner._img_pal_scroll.setWidgetResizable(True)
    owner._img_pal_scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
    owner._img_pal_scroll.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
    owner._img_pal_scroll.setMinimumHeight(12)
    layout.addWidget(owner._img_pal_scroll, stretch=1)

    return panel


def _make_collapsible_titlebar(owner, dock, title): #vers 1
    """Custom title bar - double-click to collapse, float/close buttons,
    theme-aware background (panel_bg/bg_primary) built in from the
    start rather than bolted on after the fact."""
    bar = QWidget()
    bar.setObjectName("dp5_imgpalette_titlebar")
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
    owner._img_palette_titlebar = bar


def _refresh_titlebar_color(owner, bar): #vers 1
    if owner.app_settings and hasattr(owner.app_settings, 'get_theme_colors'):
        tc = owner.app_settings.get_theme_colors()
        hexval = tc.get('panel_bg') or tc.get('bg_primary')
        if hexval:
            bar.setStyleSheet(
                f"QWidget#dp5_imgpalette_titlebar {{ background: {hexval}; }}")


def refresh_theme(owner): #vers 2
    """Re-apply the theme-aware panel stylesheet and title bar
    background on a live theme switch. Call from DP5Workshop.
    _apply_theme()."""
    dock = getattr(owner, '_img_palette_dock', None)
    if dock is not None and dock.widget() is not None:
        _apply_panel_stylesheet(owner, dock.widget())
    bar = getattr(owner, '_img_palette_titlebar', None)
    if bar is not None:
        _refresh_titlebar_color(owner, bar)
