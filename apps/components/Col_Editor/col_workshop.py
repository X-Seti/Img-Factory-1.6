#!/usr/bin/env python3
#this belongs in apps/components/Col_Editor/col_workshop.py - Version: 127
# X-Seti - August10 2025 - Converted col editor using gui base template.

"""
components/Col_Editor/col_workshop.py
COL Editor - Main collision editor interface
"""

import os
import sys
# Force X11/GLX backend for NVIDIA on Wayland (Linux only)
if sys.platform.startswith('linux'):
    os.environ['QT_QPA_PLATFORM'] = 'xcb'
    os.environ['QSG_RHI_BACKEND'] = 'opengl'
    os.environ['LIBGL_ALWAYS_SOFTWARE'] = '0'  # Use hardware acceleration

import sys


# Add project root to path for standalone mode
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

# Import PyQt6
from PyQt6.QtWidgets import QApplication, QDialog, QLabel, QPushButton, QTextEdit, QVBoxLayout, QWidget
from PyQt6.QtCore import Qt, pyqtSignal, QPoint
from PyQt6.QtGui import QColor, QFont
# QAction location varies by PyQt6 version — try bothQStyledItemDelegate

# Import project modules AFTER path setup
from apps.methods.imgfactory_svg_icons import SVGIconFactory

from apps.gui.tool_menu_mixin import ToolMenuMixin
from apps.methods.gl_viewport_mixin import GLViewportMixin
# COL Workshop parser system

from apps.components.Col_Editor.depends.col_win_func import COLWindowMixin
from apps.components.Col_Editor.depends.col_paint_func import COLPaintMixin
from apps.components.Col_Editor.depends.col_edit_func import COLEditMixin
from apps.components.Col_Editor.depends.col_list_func import COLListMixin
from apps.components.Col_Editor.depends.col_core_logic_func import COLCoreLogicMixin
from apps.components.Col_Editor.depends.col_setup_ui_func import COLSetupUIMixin, App_name, App_build
from apps.methods.ribbon_dialog import RibbonIconsMixin
from apps.methods.grip_splitter import SplitterSizesMixin
from apps.methods.img_factory_settings import get_user_config_dir


DEBUG_STANDALONE = False

##Methods list -
# open_col_workshop

##class COLWorkshop: -
# _close_col_tab
# _dock_to_main
# __init__
# _launch_theme_settings
# _open_render_settings_dialog
# _ribbon_config_path
# _show_amiga_locale_error
# _show_col_info
# show_help
# _show_settings_dialog
# show_settings_dialog
# _show_settings_hotkeys
# _show_shaders_dialog
# _show_workshop_settings
# toggle_dock_mode
# _toggle_tearoff
# _toggle_upscale_native
# _undock_from_main
# _update_dock_button_visibility

##Mixins (depends/) -
# COLWindowMixin - col_win_func.py
# COLSetupUIMixin - col_setup_ui_func.py
# COLCoreLogicMixin - col_core_logic_func.py
# COLListMixin - col_list_func.py
# COLPaintMixin - col_paint_func.py
# COLEditMixin - col_edit_func.py
# COL3DViewport - col_viewport.py
# RibbonIconsMixin - methods/ribbon_dialog.py
# SplitterSizesMixin - methods/grip_splitter.py


class COLWorkshop(COLWindowMixin, COLSetupUIMixin, COLCoreLogicMixin, COLListMixin, COLPaintMixin, COLEditMixin, RibbonIconsMixin, SplitterSizesMixin, GLViewportMixin, ToolMenuMixin, QWidget): #vers 13
    """COL Workshop - Main window"""

    #    ToolMenuMixin implementation                                      


    workshop_closed = pyqtSignal()
    window_closed = pyqtSignal()

    # Bump whenever the set of ribbon toolbars changes (added/removed/
    # renamed) so a saved layout from an older structure is cleanly
    # rejected by _restore_toolbar_state instead of Qt silently failing
    # to restore it. History: 1 = Transform/Navigation/Render ribbons,
    # 2 = added Name/Format/Shadow Mesh ribbons (replacing the old
    # dual text/icon bottom info panel).
    _RIBBON_LAYOUT_VERSION = 4

    def __init__(self, parent=None, main_window=None): #vers 12
        """initialize_features"""
        if DEBUG_STANDALONE and main_window is None:
            print(App_name + " Initializing ...")

        super().__init__(parent)
        self.setWindowTitle(App_name)
        self.setWindowIcon(SVGIconFactory.col_workshop_icon())
        self.icon_factory = SVGIconFactory()

        self.main_window = main_window

        self.undo_stack = []
        self.button_display_mode = 'both'
        self.icon_display_mode = 'icons_and_text'  # 'icons_and_text'|'icons_only'|'text_only'
        self._col_compact_btns = []   # list of (widget, full_label) for adaptive display
        self.last_save_directory = None
        self.current_col_file = None
        # Thumbnail spin animation state
        self._spin_timer  = None
        self._spin_row    = None
        self._spin_model  = None
        self._spin_yaw    = 0.0
        self._spin_pitch  = 0.0
        self._spin_dyaw   = 1.0
        self._spin_dpitch = 0.2
        # Thumbnail view axis (applied to all static thumbnails)
        self._thumb_yaw   = 0.0    # top-down (XY plane) by default
        self._thumb_pitch = 0.0

        # Set default fonts
        from PyQt6.QtGui import QFont
        default_font = QFont("Fira Sans Condensed", 14)
        self.setFont(default_font)
        self.title_font = QFont("Arial", 14)
        self.panel_font = QFont("Arial", 10)
        self.button_font = QFont("Arial", 10)
        self.infobar_font = QFont("Courier New", 9)
        self.standalone_mode = (main_window is None)

        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        else:
            # FIXED: Create AppSettings for standalone mode
            try:
                from apps.utils.app_settings_system import AppSettings
                self.app_settings = AppSettings()
            except Exception as e:
                print(f"Could not initialize AppSettings: {e}")
                self.app_settings = None
        if hasattr(self.app_settings, 'theme_changed'):
            self.app_settings.theme_changed.connect(self._refresh_icons)
            self.app_settings.theme_changed.connect(self._on_theme_changed)

        self._show_boxes = True
        self._show_mesh = True

        self._checkerboard_size = 16
        self._overlay_opacity = 50
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)

        self.background_color = self._get_ui_color('viewport_bg')

        self.background_mode = 'solid'
        self.placeholder_text = "No Surface"
        self.setMinimumSize(200, 200)
        preview_widget = False

        # Docking state
        self.is_docked = (main_window is not None)
        self.dock_widget = None
        self.is_overlay = False
        self.overlay_table = None
        self.overlay_tab_index = -1


        self.setWindowTitle(App_name + ": No File")
        self.resize(1400, 800)
        self.use_system_titlebar = False
        self.window_always_on_top = False

        # Window flags
        self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

        self._initialize_features()

        # Corner resize variables
        self.dragging = False
        self.drag_position = None
        self.resizing = False
        self.resize_corner = None
        self.corner_size = 20
        self.hover_corner = None

        if parent:
            parent_pos = parent.pos()
            self.move(parent_pos.x() + 50, parent_pos.y() + 80)

        # Paint toolbar attrs — set by _create_paint_bar() called from _create_right_panel
        self.paint_toolbar   = None
        self.paint_mat_combo = None
        self.paint_swatch    = None
        self.paint_undo_btn  = None
        self.paint_exit_btn  = None
        self.tool_paint_btn  = None
        self.tool_dropper_btn = None
        self.tool_fill_btn   = None

        # Setup UI FIRST
        self.setup_ui()
        # Setup hotkeys
        self._setup_hotkeys()

        # Apply theme ONCE at the end
        self._apply_theme()
        self.setAcceptDrops(True)       # .col / .img dropped here open in this workshop


    #    Stub implementations (log until fully implemented)                   


    def _open_render_settings_dialog(self): #vers 1
        """Render & background settings dialog."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QLabel,
                                     QComboBox, QSlider, QPushButton, QColorDialog,
                                     QGroupBox, QDialogButtonBox, QCheckBox)
        from PyQt6.QtGui import QColor
        from PyQt6.QtCore import Qt

        pw = getattr(self, 'preview_widget', None)
        if not pw: return

        dlg = QDialog(self)
        dlg.setWindowTitle("Render Settings")
        dlg.setMinimumWidth(360)
        lay = QVBoxLayout(dlg)

        # Object rendering style
        style_grp = QGroupBox("Object Rendering")
        sg = QHBoxLayout(style_grp)
        sg.addWidget(QLabel("Style:"))
        style_combo = QComboBox()
        style_combo.addItems(["Wireframe", "Semi-transparent", "Solid"])
        mapping = {"wireframe":"Wireframe","semi":"Semi-transparent","solid":"Solid"}
        style_combo.setCurrentText(mapping.get(pw._render_style, "Semi-transparent"))
        sg.addWidget(style_combo)
        lay.addWidget(style_grp)

        # Background
        bg_grp = QGroupBox("Background")
        bg = QHBoxLayout(bg_grp)
        r,g,b = pw._bg_color
        bg_preview = QPushButton("  ")
        bg_preview.setFixedSize(60, 28)
        bg_preview.setStyleSheet(f"background-color: rgb({r},{g},{b});")
        def _pick_bg():  #vers 1
            c = QColorDialog.getColor(QColor(r,g,b), dlg, "Background Colour")
            if c.isValid():
                bg_preview.setStyleSheet(f"background-color: {c.name()};")
                bg_preview.setProperty("chosen", (c.red(), c.green(), c.blue()))
        bg_preview.clicked.connect(_pick_bg)
        bg.addWidget(QLabel("Colour:"))
        bg.addWidget(bg_preview)

        scene_cb = QComboBox()
        scene_cb.addItems(["Dark", "Mid", "Light"])
        bg.addWidget(scene_cb)
        lay.addWidget(bg_grp)

        # Buttons
        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                                QDialogButtonBox.StandardButton.Cancel)
        btns.rejected.connect(dlg.reject)
        def _apply():  #vers 1
            s = style_combo.currentText()
            rev = {"Wireframe":"wireframe","Semi-transparent":"semi","Solid":"solid"}
            pw.set_render_style(rev.get(s,"semi"))
            chosen = bg_preview.property("chosen")
            if chosen:
                pw.set_background_color(chosen)
            dlg.accept()
        btns.accepted.connect(_apply)
        lay.addWidget(btns)
        dlg.exec()


    #    Method aliases — drop Qt signal args (*_, **__) before forwarding   
    # These are called from Qt signals that pass e.g. bool(checked) as arg.
    # Target methods take only self, so we must not forward the signal args.

    def _close_col_tab(self, index): #vers 2
        """Close a COL file tab."""
        self.col_tabs.removeTab(index)
    def show_help(self, *_, **__): return self._show_col_info()  #vers 2
    def show_settings_dialog(self, *_, **__): return self._show_settings_dialog()  #vers 2


# - Panel Creation


    # STUB: dock_btn, tearoff_btn, colour swatch buttons and _svg_to_icon()
    # created icons do not yet update on theme change. Wire to _refresh_icons.

    def _show_workshop_settings(self): #vers 2
        """Show complete workshop settings dialog"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                    QTabWidget, QWidget, QGroupBox, QFormLayout,
                                    QSpinBox, QComboBox, QSlider, QLabel, QCheckBox,
                                    QFontComboBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + "Settings")
        dialog.setMinimumWidth(650)
        dialog.setMinimumHeight(550)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # TAB 1: FONTS (FIRST TAB)

        fonts_tab = QWidget()
        fonts_layout = QVBoxLayout(fonts_tab)

        # Default Font
        default_font_group = QGroupBox("Default Font")
        default_font_layout = QHBoxLayout()

        default_font_combo = QFontComboBox()
        default_font_combo.setCurrentFont(self.font())
        default_font_layout.addWidget(default_font_combo)

        default_font_size = QSpinBox()
        default_font_size.setRange(8, 24)
        default_font_size.setValue(self.font().pointSize())
        default_font_size.setSuffix(" pt")
        default_font_size.setFixedWidth(80)
        default_font_layout.addWidget(default_font_size)

        default_font_group.setLayout(default_font_layout)
        fonts_layout.addWidget(default_font_group)

        # Title Font
        title_font_group = QGroupBox("Title Font")
        title_font_layout = QHBoxLayout()

        title_font_combo = QFontComboBox()
        if hasattr(self, 'title_font'):
            title_font_combo.setCurrentFont(self.title_font)
        else:
            title_font_combo.setCurrentFont(QFont("Arial", 14))
        title_font_layout.addWidget(title_font_combo)

        title_font_size = QSpinBox()
        title_font_size.setRange(10, 32)
        title_font_size.setValue(getattr(self, 'title_font', QFont("Arial", 14)).pointSize())
        title_font_size.setSuffix(" pt")
        title_font_size.setFixedWidth(80)
        title_font_layout.addWidget(title_font_size)

        title_font_group.setLayout(title_font_layout)
        fonts_layout.addWidget(title_font_group)

        # Panel Font
        panel_font_group = QGroupBox("Panel Headers Font")
        panel_font_layout = QHBoxLayout()

        panel_font_combo = QFontComboBox()
        if hasattr(self, 'panel_font'):
            panel_font_combo.setCurrentFont(self.panel_font)
        else:
            panel_font_combo.setCurrentFont(QFont("Arial", 10))
        panel_font_layout.addWidget(panel_font_combo)

        panel_font_size = QSpinBox()
        panel_font_size.setRange(8, 18)
        panel_font_size.setValue(getattr(self, 'panel_font', QFont("Arial", 10)).pointSize())
        panel_font_size.setSuffix(" pt")
        panel_font_size.setFixedWidth(80)
        panel_font_layout.addWidget(panel_font_size)

        panel_font_group.setLayout(panel_font_layout)
        fonts_layout.addWidget(panel_font_group)

        # Button Font
        button_font_group = QGroupBox("Button Font")
        button_font_layout = QHBoxLayout()

        button_font_combo = QFontComboBox()
        if hasattr(self, 'button_font'):
            button_font_combo.setCurrentFont(self.button_font)
        else:
            button_font_combo.setCurrentFont(QFont("Arial", 10))
        button_font_layout.addWidget(button_font_combo)

        button_font_size = QSpinBox()
        button_font_size.setRange(8, 16)
        button_font_size.setValue(getattr(self, 'button_font', QFont("Arial", 10)).pointSize())
        button_font_size.setSuffix(" pt")
        button_font_size.setFixedWidth(80)
        button_font_layout.addWidget(button_font_size)

        button_font_group.setLayout(button_font_layout)
        fonts_layout.addWidget(button_font_group)

        # Info Bar Font
        infobar_font_group = QGroupBox("Info Bar Font")
        infobar_font_layout = QHBoxLayout()

        infobar_font_combo = QFontComboBox()
        if hasattr(self, 'infobar_font'):
            infobar_font_combo.setCurrentFont(self.infobar_font)
        else:
            infobar_font_combo.setCurrentFont(QFont("Courier New", 9))
        infobar_font_layout.addWidget(infobar_font_combo)

        infobar_font_size = QSpinBox()
        infobar_font_size.setRange(7, 14)
        infobar_font_size.setValue(getattr(self, 'infobar_font', QFont("Courier New", 9)).pointSize())
        infobar_font_size.setSuffix(" pt")
        infobar_font_size.setFixedWidth(80)
        infobar_font_layout.addWidget(infobar_font_size)

        infobar_font_group.setLayout(infobar_font_layout)
        fonts_layout.addWidget(infobar_font_group)

        fonts_layout.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        # TAB 2: DISPLAY SETTINGS

        display_tab = QWidget()
        display_layout = QVBoxLayout(display_tab)

        # Button display mode
        button_group = QGroupBox("Button Display Mode")
        button_layout = QVBoxLayout()

        button_mode_combo = QComboBox()
        button_mode_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])
        current_mode = getattr(self, 'button_display_mode', 'both')
        mode_map = {'both': 0, 'icons': 1, 'text': 2}
        button_mode_combo.setCurrentIndex(mode_map.get(current_mode, 0))
        button_layout.addWidget(button_mode_combo)

        button_hint = QLabel("Changes how toolbar buttons are displayed")
        button_hint.setStyleSheet("color: #888; font-style: italic;")
        button_layout.addWidget(button_hint)

        button_group.setLayout(button_layout)
        display_layout.addWidget(button_group)

        # Table display
        table_group = QGroupBox("Surface List Display")
        table_layout = QVBoxLayout()

        show_thumbnails = QCheckBox("Show Surface types")
        show_thumbnails.setChecked(True)
        table_layout.addWidget(show_thumbnails)

        show_warnings = QCheckBox("Show warning icons for suspicious files")
        show_warnings.setChecked(True)
        show_warnings.setToolTip("Shows surface types")
        table_layout.addWidget(show_warnings)

        table_group.setLayout(table_layout)
        display_layout.addWidget(table_group)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")


        # TAB 4: PERFORMANCE

        perf_tab = QWidget()
        perf_layout = QVBoxLayout(perf_tab)

        perf_group = QGroupBox("Performance Settings")
        perf_form = QFormLayout()

        preview_quality = QComboBox()
        preview_quality.addItems(["Low (Fast)", "Medium", "High (Slow)"])
        preview_quality.setCurrentIndex(1)
        perf_form.addRow("Preview Quality:", preview_quality)

        thumb_size = QSpinBox()
        thumb_size.setRange(32, 128)
        thumb_size.setValue(64)
        thumb_size.setSuffix(" px")
        perf_form.addRow("Thumbnail Size:", thumb_size)

        perf_group.setLayout(perf_form)
        perf_layout.addWidget(perf_group)

        # Caching
        cache_group = QGroupBox("Caching")
        cache_layout = QVBoxLayout()

        enable_cache = QCheckBox("Enable surface preview caching")
        enable_cache.setChecked(True)
        cache_layout.addWidget(enable_cache)

        cache_hint = QLabel("Caching improves performance but uses more memory")
        cache_hint.setStyleSheet("color: #888; font-style: italic;")
        cache_layout.addWidget(cache_hint)

        cache_group.setLayout(cache_layout)
        perf_layout.addWidget(cache_group)

        perf_layout.addStretch()
        tabs.addTab(perf_tab, "Performance")

        # TAB 5: PREVIEW SETTINGS (LAST TAB)

        preview_tab = QWidget()
        preview_layout = QVBoxLayout(preview_tab)

        # Zoom Settings
        zoom_group = QGroupBox("Zoom Settings")
        zoom_form = QFormLayout()

        zoom_spin = QSpinBox()
        zoom_spin.setRange(10, 500)
        zoom_spin.setValue(int(getattr(self, 'zoom_level', 1.0) * 100))
        zoom_spin.setSuffix("%")
        zoom_form.addRow("Default Zoom:", zoom_spin)

        zoom_group.setLayout(zoom_form)
        preview_layout.addWidget(zoom_group)

        # Background Settings
        bg_group = QGroupBox("Background Settings")
        bg_layout = QVBoxLayout()

        # Background mode
        bg_mode_layout = QFormLayout()
        bg_mode_combo = QComboBox()
        bg_mode_combo.addItems(["Solid Color", "Checkerboard", "Grid"])
        current_bg_mode = getattr(self, 'background_mode', 'solid')
        mode_idx = {"solid": 0, "checkerboard": 1, "checker": 1, "grid": 2}.get(current_bg_mode, 0)
        bg_mode_combo.setCurrentIndex(mode_idx)
        bg_mode_layout.addRow("Background Mode:", bg_mode_combo)
        bg_layout.addLayout(bg_mode_layout)

        bg_layout.addSpacing(10)

        # Checkerboard size
        cb_label = QLabel("Checkerboard Size:")
        bg_layout.addWidget(cb_label)

        cb_layout = QHBoxLayout()
        cb_slider = QSlider(Qt.Orientation.Horizontal)
        cb_slider.setMinimum(4)
        cb_slider.setMaximum(64)
        cb_slider.setValue(getattr(self, '_checkerboard_size', 16))
        cb_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        cb_slider.setTickInterval(8)
        cb_layout.addWidget(cb_slider)

        cb_spin = QSpinBox()
        cb_spin.setMinimum(4)
        cb_spin.setMaximum(64)
        cb_spin.setValue(getattr(self, '_checkerboard_size', 16))
        cb_spin.setSuffix(" px")
        cb_spin.setFixedWidth(80)
        cb_layout.addWidget(cb_spin)

        bg_layout.addLayout(cb_layout)

        # Connect checkerboard controls
        #cb_slider.valueChanged.connect(cb_spin.setValue)
        #cb_spin.valueChanged.connect(cb_slider.setValue)

        # Hint
        cb_hint = QLabel("Smaller = tighter pattern, larger = bigger squares")
        cb_hint.setStyleSheet("color: #888; font-style: italic; font-size: 10px;")
        bg_layout.addWidget(cb_hint)

        bg_group.setLayout(bg_layout)
        preview_layout.addWidget(bg_group)

        # Overlay Settings
        overlay_group = QGroupBox("Overlay View Settings")
        overlay_layout = QVBoxLayout()

        overlay_label = QLabel("Overlay Opacity (Wireframe over mesh):")
        overlay_layout.addWidget(overlay_label)

        opacity_layout = QHBoxLayout()
        opacity_slider = QSlider(Qt.Orientation.Horizontal)
        opacity_slider.setMinimum(0)
        opacity_slider.setMaximum(100)
        opacity_slider.setValue(getattr(self, '_overlay_opacity', 50))
        opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        opacity_slider.setTickInterval(10)
        opacity_layout.addWidget(opacity_slider)

        opacity_spin = QSpinBox()
        opacity_spin.setMinimum(0)
        opacity_spin.setMaximum(100)
        opacity_spin.setValue(getattr(self, '_overlay_opacity', 50))
        opacity_spin.setSuffix(" %")
        opacity_spin.setFixedWidth(80)
        opacity_layout.addWidget(opacity_spin)

        overlay_layout.addLayout(opacity_layout)

        # Connect opacity controls
        #opacity_slider.valueChanged.connect(opacity_spin.setValue)
        #opacity_spin.valueChanged.connect(opacity_slider.setValue)

        # Hint
        opacity_hint = QLabel("0")
        opacity_hint.setStyleSheet("color: #888; font-style: italic; font-size: 10px;")
        overlay_layout.addWidget(opacity_hint)

        overlay_group.setLayout(overlay_layout)
        preview_layout.addWidget(overlay_group)

        preview_layout.addStretch()
        tabs.addTab(preview_tab, "Preview")

        # Add tabs to dialog
        layout.addWidget(tabs)

        # BUTTONS

        btn_layout = QHBoxLayout()
        btn_layout.addStretch()

        # Apply button
        apply_btn = QPushButton("Apply Settings")
        apply_btn.setStyleSheet("""
            QPushButton {
                background: palette(highlight);
                color: white;
                padding: 10px 24px;
                font-weight: bold;
                border-radius: 4px;
                font-size: 13px;
            }
            QPushButton:hover {
                background: palette(highlight);
            }
        """)


        def apply_settings():  #vers 1
            # Adjusted for COL Wireframe, Mesh
            self.setFont(QFont(default_font_combo.currentFont().family(),
                            default_font_size.value()))
            self.title_font = QFont(title_font_combo.currentFont().family(),
                                title_font_size.value())
            self.panel_font = QFont(panel_font_combo.currentFont().family(),
                                panel_font_size.value())
            self.button_font = QFont(button_font_combo.currentFont().family(),
                                    button_font_size.value())
            self.infobar_font = QFont(infobar_font_combo.currentFont().family(),
                                    infobar_font_size.value())

            # Apply fonts to UI
            self._apply_title_font()
            self._apply_panel_font()
            self._apply_button_font()
            self._apply_infobar_font()

            mode_map = {0: 'both', 1: 'icons', 2: 'text'}
            self.button_display_mode = mode_map[button_mode_combo.currentIndex()]

            # EXPORT
            self.default_export_format = self.format_combo.currentText()

            # PREVIEW
            self.zoom_level = zoom_spin.value() / 100.0

            bg_modes = ['solid', 'checkerboard', 'grid']
            self.background_mode = bg_modes[bg_mode_combo.currentIndex()]

            self._checkerboard_size = cb_spin.value()
            self._overlay_opacity = opacity_spin.value()

            # Update preview widget
            if hasattr(self, 'preview_widget'):
                if self.background_mode == 'checkerboard':
                    self.preview_widget.set_checkerboard_background()
                    self.preview_widget._checkerboard_size = self._checkerboard_size
                else:
                    self.preview_widget.set_background_color(self.preview_widget.bg_color)

            # Apply button display mode
            if hasattr(self, '_update_all_buttons'):
                self._update_all_buttons()

            # Refresh display

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Workshop settings updated successfully")

        apply_btn.clicked.connect(apply_settings)
        btn_layout.addWidget(apply_btn)

        # Close button
        close_btn = QPushButton("Close")
        close_btn.setStyleSheet("padding: 10px 24px; font-size: 13px;")
        close_btn.clicked.connect(dialog.close)
        btn_layout.addWidget(close_btn)

        layout.addLayout(btn_layout)

        # Show dialog
        dialog.exec()


    def _show_amiga_locale_error(self): #vers 2
        """Show Amiga Workbench 3.1 style error dialog"""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton, QHBoxLayout
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle("Workbench Request")
        dialog.setFixedSize(450, 150)

        # Amiga Workbench styling
        dialog.setStyleSheet("""
            QDialog {
                background-color: palette(placeholderText);
                border: 2px solid palette(buttonText);
            }
            QLabel {
                color: palette(windowText);
                background-color: palette(placeholderText);
            }
            QPushButton {
                background-color: palette(mid);
                color: palette(windowText);
                border: 2px outset palette(buttonText);
                padding: 5px 15px;
                min-width: 80px;
            }
            QPushButton:pressed {
                border: 2px inset palette(mid);
            }
        """)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(15)
        layout.setContentsMargins(20, 20, 20, 20)

        # Amiga Topaz font style
        amiga_font = QFont("Courier", 10, QFont.Weight.Normal)

        # Error message
        message = QLabel("Workbench 3.1 installer\n\nPlease insert Local disk in any drive")
        message.setFont(amiga_font)
        message.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(message)

        layout.addStretch()

        # Button layout
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        # Retry and Cancel buttons (Amiga style)
        retry_btn = QPushButton("Retry")
        retry_btn.setFont(amiga_font)
        retry_btn.clicked.connect(dialog.accept)
        button_layout.addWidget(retry_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.setFont(amiga_font)
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        button_layout.addStretch()
        layout.addLayout(button_layout)

        dialog.exec()


# - Docking functions

    def _update_dock_button_visibility(self): #vers 2
        """Show/hide dock and tearoff buttons based on docked state"""
        if hasattr(self, 'dock_btn'):
            # Hide D button when docked, show when standalone
            self.dock_btn.setVisible(not self.is_docked)

        if hasattr(self, 'tearoff_btn'):
            # T button only visible when docked and not in standalone mode
            self.tearoff_btn.setVisible(self.is_docked and not self.standalone_mode)


    def toggle_dock_mode(self): #vers 2
        """Toggle between docked and standalone mode"""
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

        self._update_dock_button_visibility()


    def _dock_to_main(self): #vers 9
        """Dock handled by overlay system in imgfactory - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                self.show()
                self.raise_()
                return

            # For proper docking, we need to be called from imgfactory
            # This method should be handled by imgfactory's overlay system
            if self.main_window and hasattr(self.main_window, App_name + '_docked'):
                # If available, use the main window's docking system
                self.main_window.open_col_workshop_docked()
            else:
                # Fallback: just show the window
                self.show()
                self.raise_()

            # Update dock state
            self.is_docked = True
            self._update_dock_button_visibility()
            if hasattr(self, '_middle_btn_row'):
                self._middle_btn_row.setVisible(True)

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} docked to main window")


        except Exception as e:
            print(f"Error docking: {str(e)}")
            self.show()


    def _undock_from_main(self): #vers 4
        """Undock from overlay mode to standalone window - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                # Switch from overlay to normal window
                self.setWindowFlags(Qt.WindowType.Window)
                self.is_overlay = False
                self.overlay_table = None

            # Set proper window flags for standalone mode
            self.setWindowFlags(Qt.WindowType.Window)
            
            # Ensure proper size when undocking
            if hasattr(self, 'original_size'):
                self.resize(self.original_size)
            else:
                self.resize(1000, 700)  # Reasonable default size
                
            self.is_docked = False
            self._update_dock_button_visibility()
            if hasattr(self, '_middle_btn_row'):
                self._middle_btn_row.setVisible(False)

            self.show()
            self.raise_()

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} undocked to standalone")
                
        except Exception as e:
            print(f"Error undocking: {str(e)}")
            # Fallback
            self.setWindowFlags(Qt.WindowType.Window)
            self.show()


# - Panel Setup


    #Left side vertical panel


  #  def setup_col_table_structure(workshop): pass
  #  def populate_col_table(workshop, col_file):
  #      for model in col_file.models:
  #          print(f"Model: {model.header.name}")

# - Rest of the logic for the panels


# - Marker 5

    def _toggle_tearoff(self): #vers 2
        """Toggle tear-off state (merge back to IMG Factory) - IMPROVED"""
        try:
            if self.is_docked:
                # Undock from main window
                self._undock_from_main()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{App_name} torn off from main window")
            else:
                # Dock back to main window
                self._dock_to_main()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{App_name} docked back to main window")
                    
        except Exception as e:
            print(f"Error toggling tear-off: {str(e)}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Tear-off Error", f"Could not toggle tear-off state:\n{str(e)}")


# - Marker 6


    def _launch_theme_settings(self): #vers 2
        """Launch theme engine from app_settings_system"""
        try:
            from apps.utils.app_settings_system import AppSettings, SettingsDialog

            # Get or create app_settings
            if not hasattr(self, 'app_settings') or self.app_settings is None:
                self.app_settings = AppSettings()
                if not hasattr(self.app_settings, 'current_settings'):
                    print("AppSettings failed to initialize")
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.warning(self, "Error", "Could not initialize theme system")
                    return

            # Launch settings dialog
            dialog = SettingsDialog(self.app_settings, self)

            # Connect theme change signal to apply theme
            dialog.themeChanged.connect(lambda theme: self._apply_theme())

            if dialog.exec():
                # Apply theme after dialog closes
                self._apply_theme()
                print("Theme settings applied")
                if hasattr(self, 'main_window') and self.main_window:
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Theme settings updated")

        except Exception as e:
            print(f"Theme settings error: {e}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Theme Error", f"Could not load theme system:\n{e}")


    def _show_settings_dialog(self): #vers 6
        """Show comprehensive settings dialog with all tabs including hotkeys"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                                    QWidget, QLabel, QPushButton, QGroupBox,
                                    QCheckBox, QSpinBox, QFormLayout, QScrollArea,
                                    QKeySequenceEdit, QComboBox, QMessageBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QKeySequence

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        dialog.setMinimumWidth(700)
        dialog.setMinimumHeight(600)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # === DISPLAY TAB ===
        display_tab = QWidget()
        display_layout = QVBoxLayout(display_tab)

        # Thumbnail settings
        thumb_group = QGroupBox("Thumbnail Display")
        thumb_layout = QVBoxLayout()

        thumb_size_layout = QHBoxLayout()
        thumb_size_layout.addWidget(QLabel("Thumbnail size:"))
        thumb_size_spin = QSpinBox()
        thumb_size_spin.setRange(32, 256)
        thumb_size_spin.setValue(self.thumbnail_size if hasattr(self, 'thumbnail_size') else 64)
        thumb_size_spin.setSuffix(" px")
        thumb_size_layout.addWidget(thumb_size_spin)
        thumb_size_layout.addStretch()
        thumb_layout.addLayout(thumb_size_layout)

        thumb_group.setLayout(thumb_layout)
        display_layout.addWidget(thumb_group)

        # Table display settings
        table_group = QGroupBox("Table Display")
        table_layout = QVBoxLayout()

        row_height_layout = QHBoxLayout()
        row_height_layout.addWidget(QLabel("Row height:"))
        row_height_spin = QSpinBox()
        row_height_spin.setRange(50, 200)
        row_height_spin.setValue(getattr(self, 'table_row_height', 100))
        row_height_spin.setSuffix(" px")
        row_height_layout.addWidget(row_height_spin)
        row_height_layout.addStretch()
        table_layout.addLayout(row_height_layout)

        show_grid_check = QCheckBox("Show grid lines")
        show_grid_check.setChecked(getattr(self, 'show_grid_lines', True))
        table_layout.addWidget(show_grid_check)

        table_group.setLayout(table_layout)
        display_layout.addWidget(table_group)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")

        # === PREVIEW TAB ===
        preview_tab = QWidget()
        preview_layout = QVBoxLayout(preview_tab)

        # Preview window settings
        preview_window_group = QGroupBox("Preview Window")
        preview_window_layout = QVBoxLayout()

        show_preview_check = QCheckBox("Show preview window by default")
        show_preview_check.setChecked(getattr(self, 'show_preview_default', True))
        show_preview_check.setToolTip("Automatically open preview when selecting surface")
        preview_window_layout.addWidget(show_preview_check)

        auto_refresh_check = QCheckBox("Auto-refresh preview on selection")
        auto_refresh_check.setChecked(getattr(self, 'auto_refresh_preview', True))
        auto_refresh_check.setToolTip("Update preview immediately when clicking surface")
        preview_window_layout.addWidget(auto_refresh_check)

        preview_window_group.setLayout(preview_window_layout)
        preview_layout.addWidget(preview_window_group)

        # Preview size settings
        preview_size_group = QGroupBox("Preview Size")
        preview_size_layout = QVBoxLayout()

        preview_width_layout = QHBoxLayout()
        preview_width_layout.addWidget(QLabel("Default width:"))
        preview_width_spin = QSpinBox()
        preview_width_spin.setRange(200, 1920)
        preview_width_spin.setValue(getattr(self, 'preview_width', 512))
        preview_width_spin.setSuffix(" px")
        preview_width_layout.addWidget(preview_width_spin)
        preview_width_layout.addStretch()
        preview_size_layout.addLayout(preview_width_layout)

        preview_height_layout = QHBoxLayout()
        preview_height_layout.addWidget(QLabel("Default height:"))
        preview_height_spin = QSpinBox()
        preview_height_spin.setRange(200, 1080)
        preview_height_spin.setValue(getattr(self, 'preview_height', 512))
        preview_height_spin.setSuffix(" px")
        preview_height_layout.addWidget(preview_height_spin)
        preview_height_layout.addStretch()
        preview_size_layout.addLayout(preview_height_layout)

        preview_size_group.setLayout(preview_size_layout)
        preview_layout.addWidget(preview_size_group)

        # Preview background
        preview_bg_group = QGroupBox("Preview Background")
        preview_bg_layout = QVBoxLayout()

        bg_combo = QComboBox()
        bg_combo.addItems(["Black", "White", "Gray", "Custom Color"])
        bg_combo.setCurrentText(getattr(self, 'preview_background', 'Checkerboard'))
        preview_bg_layout.addWidget(bg_combo)

        preview_bg_group.setLayout(preview_bg_layout)
        preview_layout.addWidget(preview_bg_group)

        # Preview zoom
        preview_zoom_group = QGroupBox("Preview Zoom")
        preview_zoom_layout = QVBoxLayout()

        fit_to_window_check = QCheckBox("Fit to window by default")
        fit_to_window_check.setChecked(getattr(self, 'preview_fit_to_window', True))
        preview_zoom_layout.addWidget(fit_to_window_check)

        smooth_zoom_check = QCheckBox("Use smooth scaling")
        smooth_zoom_check.setChecked(getattr(self, 'preview_smooth_scaling', True))
        smooth_zoom_check.setToolTip("Better quality but slower for large model mesh")
        preview_zoom_layout.addWidget(smooth_zoom_check)

        preview_zoom_group.setLayout(preview_zoom_layout)
        preview_layout.addWidget(preview_zoom_group)

        preview_layout.addStretch()
        tabs.addTab(preview_tab, "Preview")

        # === EXPORT TAB ===
        export_tab = QWidget()
        export_layout = QVBoxLayout(export_tab)

        # Export format
        format_group = QGroupBox("Default Collision Export Format")
        format_layout = QVBoxLayout()

        format_combo = QComboBox()
        format_combo.addItems(["COL", "COL2", "COL3", "CST", "3DS"])
        format_combo.setCurrentText(getattr(self, 'default_export_format', 'COL'))
        format_layout.addWidget(format_combo)
        format_hint = QLabel("COL recommended for GTAIII/VC, COL2 for SA")
        format_hint.setStyleSheet("color: #888; font-style: italic;")
        format_layout.addWidget(format_hint)

        format_group.setLayout(format_layout)
        export_layout.addWidget(format_group)

        # Export options
        export_options_group = QGroupBox("Export Options")
        export_options_layout = QVBoxLayout()

        preserve_shadow_check = QCheckBox("Preserve Shadow Mesh when exporting")
        preserve_shadow_check.setChecked(getattr(self, 'export_preserve_shadow', True))
        export_options_layout.addWidget(preserve_shadow_check)

        export_shadowm_check = QCheckBox("Export shadow as separate files")
        export_shadowm_check.setChecked(getattr(self, 'export_shadow_separate', False))
        export_shadowm_check.setToolTip("Save each shadow map as _shadow.col, etc.")
        export_options_layout.addWidget(export_shadowm_check)

        create_subfolders_check = QCheckBox("Create subfolders when exporting all")
        create_subfolders_check.setChecked(getattr(self, 'export_create_subfolders', False))
        create_subfolders_check.setToolTip("Organize exports into folders by col type")
        export_options_layout.addWidget(create_subfolders_check)

        export_options_group.setLayout(export_options_layout)
        export_layout.addWidget(export_options_group)

        # Compatibility note
        compat_label = QLabel(
            "Note: Export format support varies by game version. COL1 is compatible with GTA3/VC. COL2/COL3 requires SA or later."
        )
        compat_label.setWordWrap(True)
        compat_label.setStyleSheet("padding: 10px; background-color: palette(base); border-radius: 4px;")
        export_layout.addWidget(compat_label)

        export_layout.addStretch()
        tabs.addTab(export_tab, "Export")

        # === IMPORT TAB ===
        import_tab = QWidget()
        import_layout = QVBoxLayout(import_tab)

        # Import behavior
        import_behavior_group = QGroupBox("Import Behavior")
        import_behavior_layout = QVBoxLayout()

        replace_check = QCheckBox("Replace existing collision with same name")
        replace_check.setChecked(getattr(self, 'import_replace_existing', False))
        import_behavior_layout.addWidget(replace_check)

        auto_format_check = QCheckBox("Automatically select best format")
        auto_format_check.setChecked(getattr(self, 'import_auto_format', True))
        auto_format_check.setToolTip("Choose COL/COL3 based on collision version")
        import_behavior_layout.addWidget(auto_format_check)

        import_behavior_group.setLayout(import_behavior_layout)
        import_layout.addWidget(import_behavior_group)

        # Import format
        import_format_group = QGroupBox("Default Collision Format")
        import_format_layout = QVBoxLayout()

        import_format_combo = QComboBox()
        import_format_combo.addItems(["COL", "COL2", "COL3", "CST", "3DS"])
        import_format_combo.setCurrentText(getattr(self, 'default_import_format', 'COL'))
        import_format_layout.addWidget(import_format_combo)

        format_note = QLabel("COL2/COL3: compression\n, COL type 1 always Uncompressed")
        format_note.setStyleSheet("color: #888; font-style: italic;")
        import_format_layout.addWidget(format_note)

        import_format_group.setLayout(import_format_layout)
        import_layout.addWidget(import_format_group)

        import_layout.addStretch()
        tabs.addTab(import_tab, "Import")

        # === Collision CONSTRAINTS TAB ===
        constraints_tab = QWidget()
        constraints_layout = QVBoxLayout(constraints_tab)

        # Collision naming
        naming_group = QGroupBox("Collision Naming")
        naming_layout = QVBoxLayout()

        name_limit_check = QCheckBox("Enable name length limit")
        name_limit_check.setChecked(getattr(self, 'name_limit_enabled', True))
        name_limit_check.setToolTip("Enforce maximum Collision name length")
        naming_layout.addWidget(name_limit_check)

        char_limit_layout = QHBoxLayout()
        char_limit_layout.addWidget(QLabel("Maximum characters:"))
        char_limit_spin = QSpinBox()
        char_limit_spin.setRange(8, 64)
        char_limit_spin.setValue(getattr(self, 'max_collision_name_length', 32))
        char_limit_spin.setToolTip("RenderWare default is 32 characters")
        char_limit_layout.addWidget(char_limit_spin)
        char_limit_layout.addStretch()
        naming_layout.addLayout(char_limit_layout)

        naming_group.setLayout(naming_layout)
        constraints_layout.addWidget(naming_group)

        # Format support
        format_support_group = QGroupBox("Format Support")
        format_support_layout = QVBoxLayout()

        format_support_group.setLayout(format_support_layout)
        constraints_layout.addWidget(format_support_group)

        constraints_layout.addStretch()
        tabs.addTab(constraints_tab, "Constraints")

        # === KEYBOARD SHORTCUTS TAB ===
        hotkeys_tab = QWidget()
        hotkeys_layout = QVBoxLayout(hotkeys_tab)

        # Add scroll area for hotkeys
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)

        # File Operations Group
        file_group = QGroupBox("File Operations")
        file_form = QFormLayout()

        hotkey_edit_open = QKeySequenceEdit(self.hotkey_open.key() if hasattr(self, 'hotkey_open') else QKeySequence.StandardKey.Open)
        file_form.addRow("Open col:", hotkey_edit_open)

        hotkey_edit_save = QKeySequenceEdit(self.hotkey_save.key() if hasattr(self, 'hotkey_save') else QKeySequence.StandardKey.Save)
        file_form.addRow("Save col:", hotkey_edit_save)

        hotkey_edit_force_save = QKeySequenceEdit(self.hotkey_force_save.key() if hasattr(self, 'hotkey_force_save') else QKeySequence("Alt+Shift+S"))
        force_save_layout = QHBoxLayout()
        force_save_layout.addWidget(hotkey_edit_force_save)
        force_save_hint = QLabel("(Force save even if unmodified)")
        force_save_hint.setStyleSheet("color: #888; font-style: italic;")
        force_save_layout.addWidget(force_save_hint)
        file_form.addRow("Force Save:", force_save_layout)

        hotkey_edit_save_as = QKeySequenceEdit(self.hotkey_save_as.key() if hasattr(self, 'hotkey_save_as') else QKeySequence.StandardKey.SaveAs)
        file_form.addRow("Save As:", hotkey_edit_save_as)

        hotkey_edit_close = QKeySequenceEdit(self.hotkey_close.key() if hasattr(self, 'hotkey_close') else QKeySequence.StandardKey.Close)
        file_form.addRow("Close:", hotkey_edit_close)

        file_group.setLayout(file_form)
        scroll_layout.addWidget(file_group)

        # Edit Operations Group
        edit_group = QGroupBox("Edit Operations")
        edit_form = QFormLayout()

        hotkey_edit_undo = QKeySequenceEdit(self.hotkey_undo.key() if hasattr(self, 'hotkey_undo') else QKeySequence.StandardKey.Undo)
        edit_form.addRow("Undo:", hotkey_edit_undo)

        hotkey_edit_copy = QKeySequenceEdit(self.hotkey_copy.key() if hasattr(self, 'hotkey_copy') else QKeySequence.StandardKey.Copy)
        edit_form.addRow("Copy Collision:", hotkey_edit_copy)

        hotkey_edit_paste = QKeySequenceEdit(self.hotkey_paste.key() if hasattr(self, 'hotkey_paste') else QKeySequence.StandardKey.Paste)
        edit_form.addRow("Paste Collision:", hotkey_edit_paste)

        hotkey_edit_delete = QKeySequenceEdit(self.hotkey_delete.key() if hasattr(self, 'hotkey_delete') else QKeySequence.StandardKey.Delete)
        edit_form.addRow("Delete:", hotkey_edit_delete)

        hotkey_edit_duplicate = QKeySequenceEdit(self.hotkey_duplicate.key() if hasattr(self, 'hotkey_duplicate') else QKeySequence("Ctrl+D"))
        edit_form.addRow("Duplicate:", hotkey_edit_duplicate)

        hotkey_edit_rename = QKeySequenceEdit(self.hotkey_rename.key() if hasattr(self, 'hotkey_rename') else QKeySequence("F2"))
        edit_form.addRow("Rename:", hotkey_edit_rename)

        edit_group.setLayout(edit_form)
        scroll_layout.addWidget(edit_group)

        # Collision Operations Group
        coll_group = QGroupBox("Collision Operations")
        coll_form = QFormLayout()

        hotkey_edit_import = QKeySequenceEdit(self.hotkey_import.key() if hasattr(self, 'hotkey_import') else QKeySequence("Ctrl+I"))
        coll_form.addRow("Import Collision:", hotkey_edit_import)

        hotkey_edit_export = QKeySequenceEdit(self.hotkey_export.key() if hasattr(self, 'hotkey_export') else QKeySequence("Ctrl+E"))
        coll_form.addRow("Export Collision:", hotkey_edit_export)

        hotkey_edit_export_all = QKeySequenceEdit(self.hotkey_export_all.key() if hasattr(self, 'hotkey_export_all') else QKeySequence("Ctrl+Shift+E"))
        coll_form.addRow("Export All:", hotkey_edit_export_all)

        coll_group.setLayout(coll_form)
        scroll_layout.addWidget(coll_group)

        # View Operations Group
        view_group = QGroupBox("View Operations")
        view_form = QFormLayout()

        hotkey_edit_refresh = QKeySequenceEdit(self.hotkey_refresh.key() if hasattr(self, 'hotkey_refresh') else QKeySequence.StandardKey.Refresh)
        view_form.addRow("Refresh:", hotkey_edit_refresh)

        hotkey_edit_properties = QKeySequenceEdit(self.hotkey_properties.key() if hasattr(self, 'hotkey_properties') else QKeySequence("Alt+Return"))
        view_form.addRow("Properties:", hotkey_edit_properties)

        hotkey_edit_find = QKeySequenceEdit(self.hotkey_find.key() if hasattr(self, 'hotkey_find') else QKeySequence.StandardKey.Find)
        view_form.addRow("Find/Search:", hotkey_edit_find)

        hotkey_edit_help = QKeySequenceEdit(self.hotkey_help.key() if hasattr(self, 'hotkey_help') else QKeySequence.StandardKey.HelpContents)
        view_form.addRow("Help:", hotkey_edit_help)

        view_group.setLayout(view_form)
        scroll_layout.addWidget(view_group)

        scroll_layout.addStretch()

        scroll.setWidget(scroll_widget)
        hotkeys_layout.addWidget(scroll)

        # Reset to defaults button
        reset_layout = QHBoxLayout()
        reset_layout.addStretch()
        reset_hotkeys_btn = QPushButton("Reset to Plasma6 Defaults")

        def reset_hotkeys():  #vers 1
            reply = QMessageBox.question(dialog, "Reset Hotkeys",
                "Reset all keyboard shortcuts to Plasma6 defaults?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)

            if reply == QMessageBox.StandardButton.Yes:
                hotkey_edit_open.setKeySequence(QKeySequence.StandardKey.Open)
                hotkey_edit_save.setKeySequence(QKeySequence.StandardKey.Save)
                hotkey_edit_force_save.setKeySequence(QKeySequence("Alt+Shift+S"))
                hotkey_edit_save_as.setKeySequence(QKeySequence.StandardKey.SaveAs)
                hotkey_edit_close.setKeySequence(QKeySequence.StandardKey.Close)
                hotkey_edit_undo.setKeySequence(QKeySequence.StandardKey.Undo)
                hotkey_edit_copy.setKeySequence(QKeySequence.StandardKey.Copy)
                hotkey_edit_paste.setKeySequence(QKeySequence.StandardKey.Paste)
                hotkey_edit_delete.setKeySequence(QKeySequence.StandardKey.Delete)
                hotkey_edit_duplicate.setKeySequence(QKeySequence("Ctrl+D"))
                hotkey_edit_rename.setKeySequence(QKeySequence("F2"))
                hotkey_edit_import.setKeySequence(QKeySequence("Ctrl+I"))
                hotkey_edit_export.setKeySequence(QKeySequence("Ctrl+E"))
                hotkey_edit_export_all.setKeySequence(QKeySequence("Ctrl+Shift+E"))
                hotkey_edit_refresh.setKeySequence(QKeySequence.StandardKey.Refresh)
                hotkey_edit_properties.setKeySequence(QKeySequence("Alt+Return"))
                hotkey_edit_find.setKeySequence(QKeySequence.StandardKey.Find)
                hotkey_edit_help.setKeySequence(QKeySequence.StandardKey.HelpContents)

        reset_hotkeys_btn.clicked.connect(reset_hotkeys)
        reset_layout.addWidget(reset_hotkeys_btn)
        hotkeys_layout.addLayout(reset_layout)

        tabs.addTab(hotkeys_tab, "Keyboard Shortcuts")

        # Add tabs widget to main layout
        layout.addWidget(tabs)

        # Dialog buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        def apply_settings(close_dialog=False):  #vers 1
            """Apply all settings"""
            # Apply display settings
            self.thumbnail_size = thumb_size_spin.value()
            self.table_row_height = row_height_spin.value()
            self.show_grid_lines = show_grid_check.isChecked()

            # Apply preview settings
            self.show_preview_default = show_preview_check.isChecked()
            self.auto_refresh_preview = auto_refresh_check.isChecked()
            self.preview_width = preview_width_spin.value()
            self.preview_height = preview_height_spin.value()
            self.preview_background = bg_combo.currentText()
            self.preview_fit_to_window = fit_to_window_check.isChecked()
            self.preview_smooth_scaling = smooth_zoom_check.isChecked()

            # Apply export settings
            self.default_export_format = format_combo.currentText()
            self.export_preserve_shadow = preserve_shadow_check.isChecked()
            self.export_shadow_separate = export_shadowm_check.isChecked()
            self.export_create_subfolders = create_subfolders_check.isChecked()

            # Apply import settings
            self.import_replace_existing = replace_check.isChecked()
            self.import_auto_format = auto_format_check.isChecked()
            self.default_import_format = import_format_combo.currentText()

            # Apply constraint settings
            self.name_limit_enabled = name_limit_check.isChecked()
            self.max_surface_name_length = char_limit_spin.value()

            # Apply hotkeys
            if hasattr(self, 'hotkey_open'):
                self.hotkey_open.setKey(hotkey_edit_open.keySequence())
            if hasattr(self, 'hotkey_save'):
                self.hotkey_save.setKey(hotkey_edit_save.keySequence())
            if hasattr(self, 'hotkey_force_save'):
                self.hotkey_force_save.setKey(hotkey_edit_force_save.keySequence())
            if hasattr(self, 'hotkey_save_as'):
                self.hotkey_save_as.setKey(hotkey_edit_save_as.keySequence())
            if hasattr(self, 'hotkey_close'):
                self.hotkey_close.setKey(hotkey_edit_close.keySequence())
            if hasattr(self, 'hotkey_undo'):
                self.hotkey_undo.setKey(hotkey_edit_undo.keySequence())
            if hasattr(self, 'hotkey_copy'):
                self.hotkey_copy.setKey(hotkey_edit_copy.keySequence())
            if hasattr(self, 'hotkey_paste'):
                self.hotkey_paste.setKey(hotkey_edit_paste.keySequence())
            if hasattr(self, 'hotkey_delete'):
                self.hotkey_delete.setKey(hotkey_edit_delete.keySequence())
            if hasattr(self, 'hotkey_duplicate'):
                self.hotkey_duplicate.setKey(hotkey_edit_duplicate.keySequence())
            if hasattr(self, 'hotkey_rename'):
                self.hotkey_rename.setKey(hotkey_edit_rename.keySequence())
            if hasattr(self, 'hotkey_import'):
                self.hotkey_import.setKey(hotkey_edit_import.keySequence())
            if hasattr(self, 'hotkey_export'):
                self.hotkey_export.setKey(hotkey_edit_export.keySequence())
            if hasattr(self, 'hotkey_export_all'):
                self.hotkey_export_all.setKey(hotkey_edit_export_all.keySequence())
            if hasattr(self, 'hotkey_refresh'):
                self.hotkey_refresh.setKey(hotkey_edit_refresh.keySequence())
            if hasattr(self, 'hotkey_properties'):
                self.hotkey_properties.setKey(hotkey_edit_properties.keySequence())
            if hasattr(self, 'hotkey_find'):
                self.hotkey_find.setKey(hotkey_edit_find.keySequence())
            if hasattr(self, 'hotkey_help'):
                self.hotkey_help.setKey(hotkey_edit_help.keySequence())

            # Refresh UI with new settings
            if hasattr(self, '_reload_surface_table'):
                self._reload_surface_table()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Settings applied")

            if close_dialog:
                dialog.accept()

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(lambda: apply_settings(close_dialog=False))
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(lambda: apply_settings(close_dialog=True))
        button_layout.addWidget(ok_btn)

        layout.addLayout(button_layout)

        dialog.exec()


    def _ribbon_config_path(self): #vers 1
        """Ribbon layout, presets and custom icons file."""
        return get_user_config_dir() / 'col_workshop.json'

    def _show_col_info(self): #vers 5
        """About COL Workshop: author, version, features, shortcuts."""
        dialog = QDialog(self)
        dialog.setWindowTitle("About COL Workshop")
        dialog.setMinimumWidth(600)
        dialog.setMinimumHeight(500)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(15)

        # Header
        header = QLabel(f"COL Workshop - {App_name}")
        header.setFont(QFont("Arial", 14, QFont.Weight.Bold))
        header.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(header)

        # Author info
        author_label = QLabel("Author: X-Seti")
        author_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(author_label)

        # Version info
        version_label = QLabel(f"Build {App_build} - September 2026")
        version_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(version_label)

        layout.addWidget(QLabel(""))  # Spacer

        # Capabilities section
        capabilities = QTextEdit()
        capabilities.setReadOnly(True)
        capabilities.setMaximumHeight(350)

        info_text = """<b>COL Workshop Capabilities:</b><br><br>
<b>File Operations:</b><br>
- Open COL1/COL2/COL3/COL4, loose or inside IMG archives<br>
- Save, save as; unedited models save byte-identical<br>
- Drag and drop .col files: add to current or open in new tab<br><br>
<b>Collision Viewing and Editing:</b><br>
- 3D preview: wireframe, semi, solid; move and rotate gizmo<br>
- Edit spheres, boxes, vertices and faces (mesh editor)<br>
- Shadow mesh view, create, remove<br><br>
<b>Collision Management:</b><br>
- Rename, duplicate, delete, copy, paste, sort, pin<br>
- Import/export single models, IDE import/export, build from TXD<br><br>
<b>Surface Painting:</b><br>
- Paint face materials, fill, pick; surface.dat editor<br><br>
<b>Format Conversion:</b><br>
- COL1 / COL2 / COL3 with GTA3/VC to SA surface mapping<br><br>
<b>Keyboard Shortcuts:</b><br>
- Ctrl+O / Ctrl+S / Ctrl+Shift+S: Open / Save / Save as<br>
- Ctrl+Z: Undo<br>
- Ctrl+C / Ctrl+V / Delete: Copy / paste / delete model<br>
- Ctrl+D / F2: Duplicate / rename<br>
- Ctrl+I / Ctrl+E / Ctrl+Shift+E: Import / export selected / export all<br>
- Ctrl+A / Ctrl+Shift+I / Ctrl+F: Select all / invert / find<br>
- F5: Refresh, Alt+Enter: model details, F1: this help<br>
- Viewport: G move, R rotate, F fit, V render style"""

        capabilities.setHtml(info_text)
        layout.addWidget(capabilities)

        # Close button
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.accept)
        close_btn.setDefault(True)
        layout.addWidget(close_btn)

        dialog.exec()

    def _toggle_upscale_native(self): #vers 1
        """Toggle upscale native resolution"""
        # Placeholder for upscale native functionality
        print("Upscale Native toggled")

    def _show_shaders_dialog(self): #vers 3
        """Viewport display settings: render style, sphere/box colours and fill alpha."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QLabel,
                                      QRadioButton, QButtonGroup, QPushButton,
                                      QSlider, QGroupBox, QColorDialog, QFrame)
        from PyQt6.QtCore import Qt as _Qt

        pw = getattr(self, 'preview_widget', None)

        dlg = QDialog(self)
        dlg.setWindowTitle("Viewport Display Settings")
        dlg.setFixedSize(320, 380)
        root = QVBoxLayout(dlg)
        root.setSpacing(8)

        # --- Render style ---
        grp_style = QGroupBox("Render Style")
        hlay = QHBoxLayout(grp_style)
        presets = [("Wireframe", "wireframe"), ("Semi", "semi"), ("Solid", "solid")]
        btn_grp = QButtonGroup(dlg)
        vp = pw if pw else self
        cur_style = getattr(vp, '_render_style', 'semi')
        for label, key in presets:
            rb = QRadioButton(label)
            rb.setChecked(key == cur_style)
            def _set_style(checked, k=key, v=vp):  #vers 1
                if checked and hasattr(v, 'set_render_style'):
                    v.set_render_style(k)
            rb.toggled.connect(_set_style)
            btn_grp.addButton(rb)
            hlay.addWidget(rb)
        root.addWidget(grp_style)

        # --- Colour + alpha helper ---
        def _colour_row(parent_layout, label, get_col, set_col, get_alpha, set_alpha):  #vers 1
            row = QHBoxLayout()
            swatch = QFrame()
            swatch.setFixedSize(24, 24)
            r, g, b = get_col()
            swatch.setStyleSheet(f"background: rgb({r},{g},{b}); border: 1px solid #666;")

            lbl = QLabel(label)
            lbl.setFixedWidth(60)

            def _pick():  #vers 1
                r2, g2, b2 = get_col()
                col = QColorDialog.getColor(QColor(r2, g2, b2), dlg, f"Choose {label} colour")
                if col.isValid():
                    set_col((col.red(), col.green(), col.blue()))
                    swatch.setStyleSheet(
                        f"background: rgb({col.red()},{col.green()},{col.blue()}); border: 1px solid #666;")
                    if pw: pw.update()

            pick_btn = QPushButton("Colour")
            pick_btn.setFixedWidth(60)
            pick_btn.clicked.connect(_pick)

            alpha_lbl = QLabel("Fill:")
            slider = QSlider(_Qt.Orientation.Horizontal)
            slider.setRange(0, 180)
            slider.setValue(get_alpha())
            slider.setFixedWidth(100)
            val_lbl = QLabel(str(get_alpha()))
            val_lbl.setFixedWidth(28)

            def _alpha_changed(v):  #vers 1
                set_alpha(v)
                val_lbl.setText(str(v))
                if pw: pw.update()
            slider.valueChanged.connect(_alpha_changed)

            row.addWidget(lbl)
            row.addWidget(swatch)
            row.addWidget(pick_btn)
            row.addWidget(alpha_lbl)
            row.addWidget(slider)
            row.addWidget(val_lbl)
            parent_layout.addLayout(row)

        # --- Sphere colour ---
        grp_sph = QGroupBox("Spheres")
        sph_lay = QVBoxLayout(grp_sph)
        _colour_row(
            sph_lay, "Sphere",
            lambda: tuple(getattr(pw or self, '_sphere_color', (80, 200, 220))),
            lambda c: setattr(pw or self, '_sphere_color', c),
            lambda:   getattr(pw or self, '_sphere_alpha', 25),
            lambda v: setattr(pw or self, '_sphere_alpha', v))
        root.addWidget(grp_sph)

        # --- Box colour ---
        grp_box = QGroupBox("Boxes")
        box_lay = QVBoxLayout(grp_box)
        _colour_row(
            box_lay, "Box",
            lambda: tuple(getattr(pw or self, '_box_color', (220, 180, 50))),
            lambda c: setattr(pw or self, '_box_color', c),
            lambda:   getattr(pw or self, '_box_alpha', 30),
            lambda v: setattr(pw or self, '_box_alpha', v))
        root.addWidget(grp_box)

        # --- Reset + Close ---
        btn_row = QHBoxLayout()
        reset_btn = QPushButton("Reset Defaults")
        def _reset():  #vers 1
            tgt = pw or self
            tgt._sphere_color = (80, 200, 220)
            tgt._sphere_alpha = 25
            tgt._box_color    = (220, 180, 50)
            tgt._box_alpha    = 30
            if pw: pw.update()
            dlg.accept()
            self._show_shaders_dialog()
        reset_btn.clicked.connect(_reset)
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dlg.accept)
        btn_row.addWidget(reset_btn)
        btn_row.addStretch()
        btn_row.addWidget(close_btn)
        root.addLayout(btn_row)

        dlg.exec()


# - Marker 7


#------ Col functions


    #    Selection helpers                                                  


    #    Sort                                                               


    #    Pin / lock entries from editing                                    


    #    IDE-linked operations                                              


    def _show_settings_hotkeys(self): #vers 2
        """Show settings dialog with hotkey customization"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                                    QWidget, QLabel, QLineEdit, QPushButton,
                                    QGroupBox, QFormLayout, QKeySequenceEdit)
        from PyQt6.QtCore import Qt

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        dialog.setMinimumWidth(600)
        dialog.setMinimumHeight(500)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # === HOTKEYS TAB ===
        hotkeys_tab = QWidget()
        hotkeys_layout = QVBoxLayout(hotkeys_tab)

        # File Operations Group
        file_group = QGroupBox("File Operations")
        file_form = QFormLayout()

        self.hotkey_edit_open = QKeySequenceEdit(self.hotkey_open.key())
        file_form.addRow("Open col:", self.hotkey_edit_open)

        self.hotkey_edit_save = QKeySequenceEdit(self.hotkey_save.key())
        file_form.addRow("Save col:", self.hotkey_edit_save)

        self.hotkey_edit_force_save = QKeySequenceEdit(self.hotkey_force_save.key())
        force_save_layout = QHBoxLayout()
        force_save_layout.addWidget(self.hotkey_edit_force_save)
        force_save_hint = QLabel("(Force save even if unmodified)")
        force_save_hint.setStyleSheet("color: #888; font-style: italic;")
        force_save_layout.addWidget(force_save_hint)
        file_form.addRow("Force Save:", force_save_layout)

        self.hotkey_edit_save_as = QKeySequenceEdit(self.hotkey_save_as.key())
        file_form.addRow("Save As:", self.hotkey_edit_save_as)

        self.hotkey_edit_close = QKeySequenceEdit(self.hotkey_close.key())
        file_form.addRow("Close:", self.hotkey_edit_close)

        file_group.setLayout(file_form)
        hotkeys_layout.addWidget(file_group)

        # Edit Operations Group
        edit_group = QGroupBox("Edit Operations")
        edit_form = QFormLayout()

        self.hotkey_edit_undo = QKeySequenceEdit(self.hotkey_undo.key())
        edit_form.addRow("Undo:", self.hotkey_edit_undo)

        self.hotkey_edit_copy = QKeySequenceEdit(self.hotkey_copy.key())
        edit_form.addRow("Copy Collision:", self.hotkey_edit_copy)

        self.hotkey_edit_paste = QKeySequenceEdit(self.hotkey_paste.key())
        edit_form.addRow("Paste Collision:", self.hotkey_edit_paste)

        self.hotkey_edit_delete = QKeySequenceEdit(self.hotkey_delete.key())
        edit_form.addRow("Delete:", self.hotkey_edit_delete)

        self.hotkey_edit_duplicate = QKeySequenceEdit(self.hotkey_duplicate.key())
        edit_form.addRow("Duplicate:", self.hotkey_edit_duplicate)

        self.hotkey_edit_rename = QKeySequenceEdit(self.hotkey_rename.key())
        edit_form.addRow("Rename:", self.hotkey_edit_rename)

        edit_group.setLayout(edit_form)
        hotkeys_layout.addWidget(edit_group)

        # Collision Group
        coll_group = QGroupBox("Collision Operations")
        coll_form = QFormLayout()

        self.hotkey_edit_import = QKeySequenceEdit(self.hotkey_import.key())
        coll_form.addRow("Import Collision:", self.hotkey_edit_import)

        self.hotkey_edit_export = QKeySequenceEdit(self.hotkey_export.key())
        coll_form.addRow("Export Collision:", self.hotkey_edit_export)

        self.hotkey_edit_export_all = QKeySequenceEdit(self.hotkey_export_all.key())
        coll_form.addRow("Export All:", self.hotkey_edit_export_all)

        coll_group.setLayout(coll_form)
        hotkeys_layout.addWidget(coll_group)

        # View Operations Group
        view_group = QGroupBox("View Operations")
        view_form = QFormLayout()

        self.hotkey_edit_refresh = QKeySequenceEdit(self.hotkey_refresh.key())
        view_form.addRow("Refresh:", self.hotkey_edit_refresh)

        self.hotkey_edit_properties = QKeySequenceEdit(self.hotkey_properties.key())
        view_form.addRow("Properties:", self.hotkey_edit_properties)

        self.hotkey_edit_find = QKeySequenceEdit(self.hotkey_find.key())
        view_form.addRow("Find/Search:", self.hotkey_edit_find)

        self.hotkey_edit_help = QKeySequenceEdit(self.hotkey_help.key())
        view_form.addRow("Help:", self.hotkey_edit_help)

        view_group.setLayout(view_form)
        hotkeys_layout.addWidget(view_group)

        hotkeys_layout.addStretch()

        # Reset to defaults button
        reset_hotkeys_btn = QPushButton("Reset to Plasma6 Defaults")
        reset_hotkeys_btn.clicked.connect(lambda: self._reset_hotkeys_to_defaults(dialog))
        hotkeys_layout.addWidget(reset_hotkeys_btn)

        tabs.addTab(hotkeys_tab, "Keyboard Shortcuts")

        layout.addWidget(tabs)

        # Dialog buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(lambda: self._apply_hotkey_settings(dialog))
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(lambda: self._apply_hotkey_settings(dialog, close=True))
        button_layout.addWidget(ok_btn)

        layout.addLayout(button_layout)

        dialog.exec()


#class SvgIcons: #vers 1 - Once functions are updated this class will be moved to the bottom
    """SVG icon data to QIcon with theme color support"""

#moved to scg_icon_factory


# --- External AI upscaler integration helper ---
import sys


# Compatibility alias for imports

def open_col_workshop(main_window, img_path=None): #vers 3
    """Open COL Workshop - embedded in tab if main_window has tab widget, standalone otherwise"""
    try:
        from PyQt6.QtWidgets import QVBoxLayout, QWidget

        # Standalone mode
        if not main_window or not hasattr(main_window, 'main_tab_widget'):
            workshop = COLWorkshop(None, main_window)
            workshop.setWindowFlags(Qt.WindowType.Window)
            if img_path:
                ext = img_path.lower()
                if ext.endswith('.col'):
                    if hasattr(workshop, 'open_col_file'):
                        workshop.open_col_file(img_path)
                    elif hasattr(workshop, 'load_col_file'):
                        workshop.load_col_file(img_path)
                elif ext.endswith('.img'):
                    workshop.load_from_img_archive(img_path)
            elif main_window:
                # No explicit file - load from current IMG if available,
                # matching embedded mode's fallback below.
                img = getattr(main_window, 'current_img', None)
                if img:
                    fp = getattr(img, 'file_path', '') or ''
                    if fp and os.path.isfile(fp):
                        workshop.load_from_img_archive(fp)
            workshop.setWindowTitle(f"COL Workshop - {App_name}")
            workshop.resize(1200, 800)
            workshop.show()
            return workshop

        # Embedded mode - add as tab
        tab_container = QWidget()
        tab_layout = QVBoxLayout(tab_container)
        tab_layout.setContentsMargins(0, 0, 0, 0)

        workshop = COLWorkshop(tab_container, main_window)
        workshop.setWindowFlags(Qt.WindowType.Widget)
        tab_layout.addWidget(workshop)

        if img_path:
            ext = img_path.lower()
            if ext.endswith('.col'):
                if hasattr(workshop, 'open_col_file'):
                    workshop.open_col_file(img_path)
                elif hasattr(workshop, 'load_col_file'):
                    workshop.load_col_file(img_path)
            elif ext.endswith('.img'):
                workshop.load_from_img_archive(img_path)
        elif main_window:
            # No explicit file — load from current IMG if available
            img = getattr(main_window, 'current_img', None)
            if img:
                fp = getattr(img, 'file_path', '') or ''
                if fp and os.path.isfile(fp):
                    workshop.load_from_img_archive(fp)

        tab_label = os.path.splitext(os.path.basename(img_path))[0] if img_path else "COL Workshop"
        try:
            from apps.methods.imgfactory_svg_icons import get_col_file_icon
            icon = get_col_file_icon()
            idx = main_window.main_tab_widget.addTab(tab_container, icon, tab_label)
        except Exception:
            idx = main_window.main_tab_widget.addTab(tab_container, tab_label)
        main_window.main_tab_widget.setCurrentIndex(idx)
        if hasattr(main_window, '_ensure_tab_area_visible'):
            main_window._ensure_tab_area_visible()

        workshop.show()
        return workshop

    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error opening COL Workshop: {str(e)}")
        return None


if __name__ == "__main__":
    import sys
    import traceback

    print(App_name + " Starting.")

    try:
        app = QApplication(sys.argv)
        print("QApplication created")

        workshop = COLWorkshop()
        print(App_name + " instance created")

        workshop.setWindowTitle(App_name + " - Standalone")
        workshop.resize(1200, 800)
        workshop.show()
        print("Window shown, entering event loop")
        print(f"Window visible: {workshop.isVisible()}")
        print(f"Window geometry: {workshop.geometry()}")

        sys.exit(app.exec())

    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)

