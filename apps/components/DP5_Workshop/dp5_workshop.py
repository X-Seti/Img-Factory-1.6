#!/usr/bin/env python3
# apps/components/DP5_Workshop/dp5_workshop.py - Version: 1
# X-Seti - April 2026 - Deluxe Paint 5 Clone - Img Factory 1.6 bitmap editor.

import os
from pathlib import Path
from typing import Optional, List, Tuple

os.environ['QT_QPA_PLATFORM'] = 'xcb'
os.environ['QSG_RHI_BACKEND'] = 'opengl'

import sys

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QSplitter,
    QListWidget, QListWidgetItem, QLabel, QPushButton, QFrame,
    QTextEdit, QLineEdit, QMessageBox, QGroupBox, QComboBox,
    QSpinBox, QTabWidget, QScrollArea, QCheckBox, QDialog,
    QFormLayout, QFontComboBox, QSlider, QDoubleSpinBox,
    QSizePolicy, QAbstractItemView, QMenu
)
from PyQt6.QtCore import Qt, QPoint, QRect, pyqtSignal, QSize, QThread, pyqtSlot
from PyQt6.QtGui import QImage, QPixmap, QPainter, QColor, QCursor, QAction, QMouseEvent, QWheelEvent, QFont, QIcon, QColor, QPainter, QPen, QBrush, QPainterPath, QKeySequence, QShortcut

App_name = "DP5 Workshop"
DEBUG_STANDALONE = False

TOOL_PENCIL = 'pencil'
TOOL_FILL   = 'fill'
TOOL_SPRAY  = 'spray'
TOOL_LINE   = 'line'
TOOL_RECT   = 'rect'
TOOL_CIRCLE = 'circle'
TOOL_PICKER = 'picker'
TOOL_ERASER = 'eraser'

# --- Try importing shared infrastructure ---
try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
    ICONS_AVAILABLE = True
except ImportError:
    ICONS_AVAILABLE = False
    class SVGIconFactory:
        def __getattr__(self, name):
            return lambda *a, **k: QIcon()
        @staticmethod
        def clear_cache(): pass

try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    APPSETTINGS_AVAILABLE = False
    AppSettings = None

class DP5Workshop(QWidget):
    """AI Workshop – Ollama chat UI built from COL Workshop skeleton."""

    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()


    # Init

    def __init__(self, parent=None, main_window=None):
        super().__init__(parent)

        self.main_window        = main_window
        self.standalone_mode    = (main_window is None)
        self.is_docked          = not self.standalone_mode

        # Chat state
        self.sessions: list[dict] = []   # [{name, messages, created}]
        self.current_session_index = -1
        self.worker = None  # background worker placeholder
        self._current_response   = ""
        self._pending_attachments: list[dict] = []  # files queued for next send

        # Settings
        self.selected_model   = ""
        self.temperature      = 0.7
        self.max_tokens       = 2048

        # Fonts (mirrors COL Workshop)
        self.title_font   = QFont("Arial", 14)
        self.panel_font   = QFont("Arial", 10)
        self.button_font  = QFont("Arial", 10)
        self.chat_font    = QFont("Courier New", 10)
        self.button_display_mode = 'both'

        # Window chrome
        self.use_system_titlebar  = False
        self.window_always_on_top = False
        self.dragging             = False
        self.drag_position        = None
        self.resizing             = False
        self.resize_corner        = None
        self.corner_size          = 20
        self.hover_corner         = None

        # AppSettings
        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        elif APPSETTINGS_AVAILABLE:
            try:
                self.app_settings = AppSettings()
            except Exception:
                self.app_settings = None
        else:
            self.app_settings = None

        if self.app_settings and hasattr(self.app_settings, 'theme_changed'):
            self.app_settings.theme_changed.connect(self._refresh_icons)

        # Icon factory
        self.icon_factory = SVGIconFactory()

        # Canvas state
        self._canvas_width  = 320
        self._canvas_height = 200
        self._canvas_zoom   = 4
        self._undo_stack    = __import__('collections').deque(maxlen=32)
        self._redo_stack    = __import__('collections').deque(maxlen=32)
        self.dp5_canvas     = None   # set by _create_centre_panel

        self.setWindowTitle(App_name)
        #if ICONS_AVAILABLE:
        #    self.setWindowIcon(SVGIconFactory.ai_app_icon())
        self.resize(1400, 800)
        self.setMinimumSize(800, 500)

        # Frameless in standalone (custom titlebar), widget when docked
        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        else:
            self.setWindowFlags(Qt.WindowType.Widget)

        if parent:
            p = parent.pos()
            self.move(p.x() + 50, p.y() + 80)

        self.setup_ui()
        self._apply_theme()
        # Defer Ollama network calls so UI appears immediately — not blocking on startup
        from PyQt6.QtCore import QTimer


    # UI construction

    def setup_ui(self):
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(5, 5, 5, 5)
        main_layout.setSpacing(5)

        toolbar = self._create_toolbar()
        self._workshop_toolbar = toolbar
        toolbar.setVisible(self.standalone_mode)   # shown standalone, hidden docked
        main_layout.addWidget(toolbar)

        splitter = QSplitter(Qt.Orientation.Horizontal)

        #left   = self._create_left_panel() #disabled, might not be needed, keep.
        centre = self._create_centre_panel()
        right  = self._create_right_panel()

        #splitter.addWidget(left)
        splitter.addWidget(centre)
        splitter.addWidget(right)
        splitter.setStretchFactor(0, 1)   # sessions
        splitter.setStretchFactor(1, 4)   # chat
        splitter.setStretchFactor(2, 1)   # settings

        main_layout.addWidget(splitter)

        # Initial tool selection (canvas must exist first)
        if hasattr(self, '_tool_btns') and hasattr(self, 'dp5_canvas') and self.dp5_canvas:
            from apps.components.DP5_Workshop.dp5_paint_editor import TOOL_PENCIL
            self._select_tool(TOOL_PENCIL)


    # --- Toolbar -----------------------------------------------------------

    def _create_toolbar(self):
        self.titlebar = QFrame()
        self.titlebar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.titlebar.setFixedHeight(45)
        self.titlebar.setObjectName("titlebar")
        self.titlebar.installEventFilter(self)
        self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.titlebar.setMouseTracking(True)

        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setMaximumHeight(50)

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(5)

        # Theme-aware icon colour
        icon_color = self._get_icon_color()

        # Settings button (standalone only — docked uses right-panel button)
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(SVGIconFactory.settings_icon(20, icon_color))
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip(App_name + "Workshop Settings")
        self.settings_btn.setVisible(self.standalone_mode)
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        # AI icon + title in centre
        title_row = QHBoxLayout()
        title_row.setSpacing(6)
        ai_icon_lbl = QLabel()
        if ICONS_AVAILABLE:
            pix = SVGIconFactory.ai_icon(20, icon_color).pixmap(20, 20)
            ai_icon_lbl.setPixmap(pix)
        title_row.addWidget(ai_icon_lbl)
        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_row.addWidget(self.title_label)
        layout.addLayout(title_row)

        layout.addStretch()

        # Theme / Properties
        self.properties_btn = QPushButton()
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(20, icon_color))
        self.properties_btn.setIconSize(QSize(20, 20))
        self.properties_btn.setFixedSize(35, 35)
        self.properties_btn.setToolTip("Theme Settings")
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        layout.addWidget(self.properties_btn)

        # Dock button — hidden in standalone (nothing to dock to)
        self.dock_btn = QPushButton("D")
        self.dock_btn.setMinimumWidth(40)
        self.dock_btn.setMaximumWidth(40)
        self.dock_btn.setMinimumHeight(30)
        self.dock_btn.setToolTip("Dock into IMG Factory")
        self.dock_btn.clicked.connect(self.toggle_dock_mode)
        self.dock_btn.setVisible(not self.standalone_mode)
        layout.addWidget(self.dock_btn)

        # Tear-off button — only when docked
        if not self.standalone_mode:
            self.tearoff_btn = QPushButton("T")
            self.tearoff_btn.setMinimumWidth(40)
            self.tearoff_btn.setMaximumWidth(40)
            self.tearoff_btn.setMinimumHeight(30)
            self.tearoff_btn.clicked.connect(self._toggle_tearoff)
            self.tearoff_btn.setToolTip("Tear off to standalone window")
            layout.addWidget(self.tearoff_btn)

        # Window controls (standalone only)
        if self.standalone_mode:
            for attr, icon_method, slot, tip in [
                ('minimize_btn', 'minimize_icon', self.showMinimized,    "Minimize"),
                ('maximize_btn', 'maximize_icon', self._toggle_maximize, "Maximize"),
                ('close_btn',    'close_icon',    self.close,            "Close"),
            ]:
                btn = QPushButton()
                btn.setIcon(getattr(SVGIconFactory, icon_method)(20, icon_color))
                btn.setIconSize(QSize(20, 20))
                btn.setMinimumWidth(40); btn.setMaximumWidth(40); btn.setMinimumHeight(30)
                btn.clicked.connect(slot)
                btn.setToolTip(tip)
                setattr(self, attr, btn)
                layout.addWidget(btn)

        return self.toolbar


    # --- Left panel: session list ------------------------------------------

    def _create_left_panel(self):
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(180)
        panel.setMaximumWidth(280)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(4)

        # Header row
        hdr = QHBoxLayout()
        header = QLabel("Bitmqps")
        header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        hdr.addWidget(header)
        hdr.addStretch()

        # Bottom buttons
        btn_row = QHBoxLayout()
        import_btn = QPushButton("Import")
        import_btn.setFont(self.button_font)
        import_btn.setToolTip("Import Bitmap")
        import_btn.clicked.connect(self._import_bitmap)
        btn_row.addWidget(export_btn)

        # Bottom buttons
        btn_row = QHBoxLayout()
        export_btn = QPushButton("Export")
        export_btn.setFont(self.button_font)
        export_btn.setToolTip("Export Bitmap")
        export_btn.clicked.connect(self._export_bitmap)
        btn_row.addWidget(export_btn)

        del_btn = QPushButton("Delete")
        del_btn.setFont(self.button_font)
        del_btn.setToolTip("Delete current session")
        del_btn.clicked.connect(lambda: self._delete_session(self.current_session_index))
        btn_row.addWidget(del_btn)
        layout.addLayout(btn_row)

        self._refresh_session_list()
        return panel


    # --- Centre panel: chat ------------------------------------------------

    def _create_centre_panel(self):
        """Centre panel — hosts the DP5 canvas and menubar."""
        from PyQt6.QtWidgets import QGroupBox, QVBoxLayout
        panel = QGroupBox("")
        layout = QVBoxLayout(panel)
        layout.setContentsMargins(2, 2, 2, 2)
        layout.setSpacing(2)

        # ── Menu bar ──
        from PyQt6.QtWidgets import QMenuBar
        mb = QMenuBar(panel)
        self._build_canvas_menus(mb)
        layout.addWidget(mb)

        # ── Canvas ──
        try:
            from apps.components.DP5_Workshop.dp5_paint_editor import DP5Canvas
            w, h = self._canvas_width, self._canvas_height
            self.canvas_rgba = bytearray(b'\x80\x80\x80\xff' * (w * h))
            self.dp5_canvas = DP5Canvas(w, h, self.canvas_rgba, panel)
            self.dp5_canvas._editor = self
            from PyQt6.QtWidgets import QScrollArea
            scroll = QScrollArea()
            scroll.setWidget(self.dp5_canvas)
            scroll.setWidgetResizable(True)
            layout.addWidget(scroll, 1)
            self._set_status(f"Canvas: {w}×{h}")
        except Exception as e:
            from PyQt6.QtWidgets import QLabel
            err = QLabel(f"Canvas error: {e}")
            layout.addWidget(err)
            self.dp5_canvas = None

        return panel

    def _build_canvas_menus(self, mb):
        """Build the canvas-area menu bar."""
        from PyQt6.QtWidgets import QMenu
        # File
        fm = mb.addMenu("File")
        fm.addAction("New canvas…",    self._new_canvas)
        fm.addAction("Open image…",    self._import_bitmap)
        fm.addAction("Save as PNG…",   self._export_bitmap)
        fm.addAction("Export IFF…",    self._export_iff)
        # Edit
        em = mb.addMenu("Edit")
        em.addAction("Undo	Ctrl+Z",   self._undo_canvas)
        em.addAction("Redo	Ctrl+Y",   self._redo_canvas)
        em.addSeparator()
        em.addAction("Clear",          self._clear_canvas)
        em.addAction("Fill colour",    self._fill_canvas)
        # Picture
        pm = mb.addMenu("Picture")
        pm.addAction("Flip horizontal", self._flip_h)
        pm.addAction("Flip vertical",   self._flip_v)
        pm.addAction("Invert",          self._invert)
        pm.addAction("Brighten +25",    lambda: self._adjust(25))
        pm.addAction("Darken −25",      lambda: self._adjust(-25))
        # View
        vm = mb.addMenu("View")
        vm.addAction("Zoom in  +",  lambda: self._set_zoom(self._canvas_zoom + 1))
        vm.addAction("Zoom out −",  lambda: self._set_zoom(self._canvas_zoom - 1))
        for z in (1, 2, 4, 8, 16):
            vm.addAction(f"{z}×", lambda _, zz=z: self._set_zoom(zz))


    # --- Right panel: settings ---------------------------------------------

    def _create_right_panel(self):
        """Right panel: tool buttons + colour swatch + palette strip."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(64)
        panel.setMaximumWidth(120)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(3, 3, 3, 3)
        layout.setSpacing(3)

        # ── Tool buttons ──
        from apps.components.DP5_Workshop.dp5_paint_editor import (
            TOOL_PENCIL, TOOL_ERASER, TOOL_FILL, TOOL_PICKER,
            TOOL_LINE, TOOL_RECT, TOOL_CIRCLE, TOOL_SPRAY
        )
        tools = [
            (TOOL_PENCIL, '✏',  'Pencil (P)'),
            (TOOL_ERASER, '⬜', 'Eraser (E)'),
            (TOOL_FILL,   '▓',  'Fill (F)'),
            (TOOL_PICKER, '⊕',  'Picker (K)'),
            (TOOL_LINE,   '╱',  'Line (L)'),
            (TOOL_RECT,   '▭',  'Rect (R)'),
            (TOOL_CIRCLE, '○',  'Circle (C)'),
            (TOOL_SPRAY,  '·:', 'Spray (S)'),
        ]
        self._tool_btns = {}
        for tool_id, icon, tip in tools:
            btn = QPushButton(icon)
            btn.setFixedSize(48, 28)
            btn.setCheckable(True)
            btn.setToolTip(tip)
            btn.clicked.connect(lambda _, t=tool_id: self._select_tool(t))
            self._tool_btns[tool_id] = btn
            layout.addWidget(btn)

        layout.addSpacing(4)

        # ── Brush size ──
        from PyQt6.QtWidgets import QSlider
        layout.addWidget(QLabel("Size"))
        self._size_sl = QSlider(Qt.Orientation.Vertical)
        self._size_sl.setRange(1, 10)
        self._size_sl.setValue(1)
        self._size_sl.setFixedHeight(60)
        self._size_sl.valueChanged.connect(self._set_brush_size)
        layout.addWidget(self._size_sl)

        layout.addSpacing(4)

        # ── Current colour swatch ──
        self.color_btn = QPushButton()
        self.color_btn.setFixedSize(54, 24)
        self.color_btn.setToolTip("Click to pick colour")
        self.color_btn.clicked.connect(self._pick_color)
        self._update_color_btn()
        layout.addWidget(self.color_btn)

        # ── Palette strip ──
        try:
            from apps.components.DP5_Workshop.dp5_paint_editor import DP5PaletteBar
            self.pal_bar = DP5PaletteBar(panel)
            self.pal_bar.color_picked.connect(self._on_palette_color)
            layout.addWidget(self.pal_bar, 1)
        except Exception:
            layout.addStretch(1)

        # ── Settings button (docked mode) ──
        self.docked_settings_btn = QPushButton()
        self.docked_settings_btn.setFont(self.button_font)
        self.docked_settings_btn.setIcon(self.icon_factory.settings_icon())
        self.docked_settings_btn.setText("Settings")
        self.docked_settings_btn.setIconSize(QSize(16, 16))
        self.docked_settings_btn.clicked.connect(self._show_workshop_settings)
        self.docked_settings_btn.setVisible(not self.standalone_mode)
        layout.addWidget(self.docked_settings_btn)

        return panel

    def _select_tool(self, tool_id: str):
        if self.dp5_canvas:
            self.dp5_canvas.tool = tool_id
        for tid, btn in self._tool_btns.items():
            btn.setChecked(tid == tool_id)

    def _set_brush_size(self, v: int):
        if self.dp5_canvas:
            self.dp5_canvas.brush_size = v

    def _pick_color(self):
        from PyQt6.QtWidgets import QColorDialog
        if not self.dp5_canvas: return
        c = QColorDialog.getColor(self.dp5_canvas.color, self, "Pick Colour",
                                   QColorDialog.ColorDialogOption.ShowAlphaChannel)
        if c.isValid():
            self.dp5_canvas.color = c
            self._update_color_btn()

    def _on_palette_color(self, c):
        if self.dp5_canvas:
            self.dp5_canvas.color = c
            self._update_color_btn()

    def _update_color_btn(self):
        if not self.dp5_canvas: return
        c = self.dp5_canvas.color
        luma = (c.red()*299 + c.green()*587 + c.blue()*114)//1000
        fg = "#000" if luma > 128 else "#fff"
        self.color_btn.setStyleSheet(
            f"background:{c.name()}; color:{fg}; "
            f"border: 2px solid {'#fff' if luma<128 else '#000'};")
        self.color_btn.setText(c.name().upper())

    # Settings dialog

    def _show_workshop_settings(self):
        from PyQt6.QtWidgets import QFileDialog as QFD
        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        dialog.setMinimumWidth(560)
        dialog.setMinimumHeight(480)

        layout = QVBoxLayout(dialog)
        tabs = QTabWidget()

        # --- Fonts tab ---
        fonts_tab = QWidget()
        fl = QVBoxLayout(fonts_tab)
        for label_text, attr in [
            ("Chat Font",   "chat_font"),
            ("Panel Font",  "panel_font"),
            ("Button Font", "button_font"),
        ]:
            g = QGroupBox(label_text)
            gl = QHBoxLayout(g)
            combo = QFontComboBox()
            combo.setCurrentFont(getattr(self, attr))
            spin = QSpinBox(); spin.setRange(7, 20)
            spin.setValue(getattr(self, attr).pointSize())
            gl.addWidget(combo); gl.addWidget(spin)
            fl.addWidget(g)
            setattr(dialog, f"_{attr}_combo", combo)
            setattr(dialog, f"_{attr}_spin",  spin)
        fl.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        # --- General tab ---
        gen_tab = QWidget()
        gtl = QFormLayout(gen_tab)

        # --- Display tab ---
        disp_tab = QWidget()
        disp_layout = QVBoxLayout(disp_tab)


    # Theme / AppSettings

    def _apply_theme(self):
        try:
            app_settings = self.app_settings
            if not app_settings and self.main_window:
                app_settings = getattr(self.main_window, 'app_settings', None)
            if app_settings:
                self.setStyleSheet(app_settings.get_stylesheet())
                # Apply theme colours to status labels
                colors = app_settings.get_theme_colors()
                secondary = colors.get('text_secondary', '#aaaaaa')
                if hasattr(self, 'typing_label'):
                    self.typing_label.setStyleSheet(
                        f"color: {secondary}; font-style: italic;")
            else:
                self.setStyleSheet("""
                    QWidget { background-color: #2b2b2b; color: #e0e0e0; }
                    QTextEdit, QListWidget { background-color: #1e1e1e; border: 1px solid #3a3a3a; }
                    QGroupBox { border: 1px solid #3a3a3a; margin-top: 6px; }
                """)
            # Redraw chat bubbles with updated theme colours
            self._redraw_chat()
            self._update_ollama_status()
        except Exception as e:
            print(f"[AI Workshop] Theme error: {e}")

    def _refresh_icons(self):
        SVGIconFactory.clear_cache()
        # Re-apply theme-aware icon colour to toolbar buttons
        color = self._get_icon_color()
        icon_map = [
            ('settings_btn',    'settings_icon'),
            ('new_session_btn', 'add_icon'),
            ('clear_btn',       'delete_icon'),
            ('properties_btn',  'properties_icon'),
            ('docked_settings_btn', 'settings_icon'),
        ]
        for attr, method in icon_map:
            if hasattr(self, attr):
                btn = getattr(self, attr)
                btn.setIcon(getattr(SVGIconFactory, method)(20, color))
        # Window control buttons
        for attr, method in [('minimize_btn','minimize_icon'),
                              ('maximize_btn','maximize_icon'),
                              ('close_btn','close_icon')]:
            if hasattr(self, attr):
                getattr(self, attr).setIcon(getattr(SVGIconFactory, method)(20, color))

    def _launch_theme_settings(self):
        try:
            if not APPSETTINGS_AVAILABLE:
                return
            dialog = SettingsDialog(self.app_settings, self)
            dialog.themeChanged.connect(lambda _: self._apply_theme())
            if dialog.exec():
                self._apply_theme()
        except Exception as e:
            QMessageBox.warning(self, "Theme Error", str(e))


    # Window management (mirrors COL Workshop)

    def _get_icon_color(self):
        if self.app_settings:
            colors = self.app_settings.get_theme_colors()
            return colors.get('text_primary', '#ffffff')
        return '#ffffff'

    def toggle_dock_mode(self):
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

    def _dock_to_main(self):
        self.is_docked = True
        self.standalone_mode = False
        if hasattr(self, '_workshop_toolbar'):
            self._workshop_toolbar.setVisible(False)
        if hasattr(self, 'docked_settings_btn'):
            self.docked_settings_btn.setVisible(True)
        if hasattr(self, 'dock_btn'):
            self.dock_btn.setVisible(False)
        self.show(); self.raise_()

    def _undock_from_main(self):
        self.standalone_mode = True
        self.is_docked = False
        self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        if hasattr(self, '_workshop_toolbar'):
            self._workshop_toolbar.setVisible(True)
        if hasattr(self, 'docked_settings_btn'):
            self.docked_settings_btn.setVisible(False)
        if hasattr(self, 'settings_btn'):
            self.settings_btn.setVisible(True)
        if hasattr(self, 'dock_btn'):
            self.dock_btn.setVisible(False)
        self.resize(1300, 800)
        self.show(); self.raise_()

    def _toggle_tearoff(self):
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

    def _toggle_maximize(self):
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()

    def keyPressEvent(self, e):
        from PyQt6.QtCore import Qt
        from apps.components.DP5_Workshop.dp5_paint_editor import (
            TOOL_PENCIL, TOOL_ERASER, TOOL_FILL, TOOL_PICKER,
            TOOL_LINE, TOOL_RECT, TOOL_CIRCLE, TOOL_SPRAY
        )
        k = e.key(); mod = e.modifiers()
        tool_keys = {
            Qt.Key.Key_P: TOOL_PENCIL, Qt.Key.Key_E: TOOL_ERASER,
            Qt.Key.Key_F: TOOL_FILL,   Qt.Key.Key_K: TOOL_PICKER,
            Qt.Key.Key_L: TOOL_LINE,   Qt.Key.Key_R: TOOL_RECT,
            Qt.Key.Key_C: TOOL_CIRCLE, Qt.Key.Key_S: TOOL_SPRAY,
        }
        if k in tool_keys:
            self._select_tool(tool_keys[k])
        elif mod == Qt.KeyboardModifier.ControlModifier:
            if k == Qt.Key.Key_Z: self._undo_canvas()
            elif k == Qt.Key.Key_Y: self._redo_canvas()
        elif k in (Qt.Key.Key_Plus, Qt.Key.Key_Equal):
            self._set_zoom(self._canvas_zoom + 1)
        elif k == Qt.Key.Key_Minus:
            self._set_zoom(self._canvas_zoom - 1)
        else:
            super().keyPressEvent(e)

    def closeEvent(self, event):
        self._cleanup_worker()
        self._stop_web_server()
        self.window_closed.emit()
        event.accept()


    # Corner resize + dragging (identical pattern to COL Workshop)

    def _get_resize_corner(self, pos):
        size = self.corner_size; w = self.width(); h = self.height()
        if pos.x() < size and pos.y() < size:           return "top-left"
        if pos.x() > w - size and pos.y() < size:       return "top-right"
        if pos.x() < size and pos.y() > h - size:       return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:   return "bottom-right"
        return None


    def _update_cursor(self, direction):
        cursors = {
            "top":          Qt.CursorShape.SizeVerCursor,
            "bottom":       Qt.CursorShape.SizeVerCursor,
            "left":         Qt.CursorShape.SizeHorCursor,
            "right":        Qt.CursorShape.SizeHorCursor,
            "top-left":     Qt.CursorShape.SizeFDiagCursor,
            "bottom-right": Qt.CursorShape.SizeFDiagCursor,
            "top-right":    Qt.CursorShape.SizeBDiagCursor,
            "bottom-left":  Qt.CursorShape.SizeBDiagCursor,
        }
        self.setCursor(cursors.get(direction, Qt.CursorShape.ArrowCursor))


    def _is_on_draggable_area(self, pos):
        if not hasattr(self, 'titlebar'):
            return False
        if not self.titlebar.rect().contains(pos):
            return False
        for w in self.titlebar.findChildren(QPushButton):
            if w.isVisible() and w.geometry().contains(pos):
                return False
        return True


    def mousePressEvent(self, event):
        if event.button() != Qt.MouseButton.LeftButton:
            return super().mousePressEvent(event)
        pos = event.pos()
        self.resize_corner = self._get_resize_corner(pos)
        if self.resize_corner:
            self.resizing = True
            self.drag_position = event.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            event.accept(); return
        if hasattr(self, 'titlebar') and self.titlebar.geometry().contains(pos):
            tb_pos = self.titlebar.mapFromParent(pos)
            if self._is_on_draggable_area(tb_pos):
                handle = self.windowHandle()
                if handle:
                    handle.startSystemMove()
                event.accept(); return
        super().mousePressEvent(event)


    def mouseMoveEvent(self, event):
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept(); return
        else:
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()
            self._update_cursor(corner)
        super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    def _handle_corner_resize(self, global_pos):
        if not self.resize_corner or not self.drag_position:
            return
        delta = global_pos - self.drag_position
        geometry = self.initial_geometry
        min_w, min_h = 800, 500
        if self.resize_corner == "bottom-right":
            nw = geometry.width() + delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h:
                self.resize(nw, nh)
        elif self.resize_corner == "bottom-left":
            nx = geometry.x() + delta.x()
            nw = geometry.width() - delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(nx, geometry.y(), nw, nh)
        elif self.resize_corner == "top-right":
            ny = geometry.y() + delta.y()
            nw = geometry.width() + delta.x()
            nh = geometry.height() - delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(geometry.x(), ny, nw, nh)
        elif self.resize_corner == "top-left":
            nx = geometry.x() + delta.x()
            ny = geometry.y() + delta.y()
            nw = geometry.width() - delta.x()
            nh = geometry.height() - delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(nx, ny, nw, nh)


    def paintEvent(self, event):
        super().paintEvent(event)
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        if self.app_settings:
            colors = self.app_settings.get_theme_colors()
            accent = QColor(colors.get('accent_primary', '#1976d2'))
        else:
            accent = QColor(100, 150, 255)
        accent.setAlpha(180)
        hover = QColor(accent); hover.setAlpha(255)
        w, h, size = self.width(), self.height(), self.corner_size
        corners = {
            'top-left':     [(0,0),(size,0),(0,size)],
            'top-right':    [(w,0),(w-size,0),(w,size)],
            'bottom-left':  [(0,h),(size,h),(0,h-size)],
            'bottom-right': [(w,h),(w-size,h),(w,h-size)],
        }
        for name, pts in corners.items():
            path = QPainterPath()
            path.moveTo(*pts[0]); path.lineTo(*pts[1]); path.lineTo(*pts[2])
            path.closeSubpath()
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(hover if self.hover_corner == name else accent))
            painter.drawPath(path)
        painter.end()

    def resizeEvent(self, event):
        super().resizeEvent(event)



    # ── Stub methods — to be implemented ─────────────────────────────────────

    # ── Canvas colour / tool forwarding ──────────────────────────────────────

    def _update_color_btn(self):
        """Called by DP5Canvas when colour picker selects a new colour."""
        if self.dp5_canvas:
            c = self.dp5_canvas.color
            # Propagate to palette bar if present
            pal = getattr(self, 'pal_bar', None)
            if pal and hasattr(pal, 'set_selection_by_color'):
                pal.set_selection_by_color(c)

    def _update_status(self, x, y, colour):
        """Called by DP5Canvas on mouse move."""
        self._set_status(
            f"Pos: {x},{y}  RGBA({colour.red()},{colour.green()},"
            f"{colour.blue()},{colour.alpha()})  Zoom: {self._canvas_zoom}×")

    def _update_zoom_label(self):
        self._canvas_zoom = getattr(self.dp5_canvas, 'zoom', self._canvas_zoom)

    # ── Canvas operations ─────────────────────────────────────────────────────

    def _push_undo(self):
        if self.dp5_canvas:
            self._undo_stack.append(bytes(self.dp5_canvas.rgba))
            self._redo_stack.clear()

    def _undo_canvas(self):
        if self.dp5_canvas and self._undo_stack:
            self._redo_stack.append(bytes(self.dp5_canvas.rgba))
            self.dp5_canvas.rgba[:] = self._undo_stack.pop()
            self.dp5_canvas.update()

    def _redo_canvas(self):
        if self.dp5_canvas and self._redo_stack:
            self._undo_stack.append(bytes(self.dp5_canvas.rgba))
            self.dp5_canvas.rgba[:] = self._redo_stack.pop()
            self.dp5_canvas.update()

    def _clear_canvas(self):
        if not self.dp5_canvas: return
        self._push_undo()
        self.dp5_canvas.rgba[:] = b'\x00' * len(self.dp5_canvas.rgba)
        self.dp5_canvas.update()

    def _fill_canvas(self):
        if not self.dp5_canvas: return
        self._push_undo()
        c = self.dp5_canvas.color
        for i in range(self._canvas_width * self._canvas_height):
            self.dp5_canvas.rgba[i*4:i*4+4] = [c.red(),c.green(),c.blue(),c.alpha()]
        self.dp5_canvas.update()

    def _set_zoom(self, z):
        if not self.dp5_canvas: return
        self.dp5_canvas.zoom = max(1, min(16, z))
        self._canvas_zoom = self.dp5_canvas.zoom
        self.dp5_canvas.update()

    def _flip_h(self):
        if not self.dp5_canvas: return
        self._push_undo()
        from PIL import Image
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.dp5_canvas.rgba))
        self.dp5_canvas.rgba[:] = img.transpose(Image.Transpose.FLIP_LEFT_RIGHT).tobytes()
        self.dp5_canvas.update()

    def _flip_v(self):
        if not self.dp5_canvas: return
        self._push_undo()
        from PIL import Image
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.dp5_canvas.rgba))
        self.dp5_canvas.rgba[:] = img.transpose(Image.Transpose.FLIP_TOP_BOTTOM).tobytes()
        self.dp5_canvas.update()

    def _invert(self):
        if not self.dp5_canvas: return
        self._push_undo()
        from PIL import Image, ImageOps
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.dp5_canvas.rgba))
        r,g,b,a = img.split()
        img2 = Image.merge('RGBA', (ImageOps.invert(r), ImageOps.invert(g),
                                    ImageOps.invert(b), a))
        self.dp5_canvas.rgba[:] = img2.tobytes()
        self.dp5_canvas.update()

    def _adjust(self, delta):
        if not self.dp5_canvas: return
        self._push_undo()
        for i in range(0, len(self.dp5_canvas.rgba), 4):
            for j in range(3):
                self.dp5_canvas.rgba[i+j] = max(0, min(255, self.dp5_canvas.rgba[i+j] + delta))
        self.dp5_canvas.update()

    def _new_canvas(self):
        from PyQt6.QtWidgets import QInputDialog
        w, ok1 = QInputDialog.getInt(self, "New Canvas", "Width:",  320, 8, 4096)
        if not ok1: return
        h, ok2 = QInputDialog.getInt(self, "New Canvas", "Height:", 200, 8, 4096)
        if not ok2: return
        self._canvas_width  = w
        self._canvas_height = h
        if self.dp5_canvas:
            self.dp5_canvas.tex_w = w
            self.dp5_canvas.tex_h = h
            self.dp5_canvas.rgba  = bytearray(b'\x80\x80\x80\xff' * (w * h))
            self.dp5_canvas.update()
        self._set_status(f"New canvas: {w}×{h}")

    def _import_bitmap(self):
        from PyQt6.QtWidgets import QFileDialog
        path, _ = QFileDialog.getOpenFileName(
            self, "Open Image", "",
            "Images (*.png *.bmp *.jpg *.jpeg *.iff *.lbm);;All Files (*)")
        if not path or not self.dp5_canvas: return
        try:
            from PIL import Image
            img = Image.open(path).convert('RGBA')
            w, h = img.size
            self._canvas_width, self._canvas_height = w, h
            self.dp5_canvas.tex_w = w
            self.dp5_canvas.tex_h = h
            self.dp5_canvas.rgba  = bytearray(img.tobytes())
            self.dp5_canvas.update()
            self._set_status(f"Opened: {__import__('os').path.basename(path)}  {w}×{h}")
        except Exception as e:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Open Error", str(e))

    def _export_bitmap(self):
        if not self.dp5_canvas: return
        from PyQt6.QtWidgets import QFileDialog
        path, _ = QFileDialog.getSaveFileName(
            self, "Save PNG", "untitled.png", "PNG (*.png);;BMP (*.bmp)")
        if not path: return
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                                  bytes(self.dp5_canvas.rgba))
            img.save(path)
            self._set_status(f"Saved: {__import__('os').path.basename(path)}")
        except Exception as e:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Save Error", str(e))

    def _export_iff(self):
        if not self.dp5_canvas: return
        from PyQt6.QtWidgets import QFileDialog
        path, _ = QFileDialog.getSaveFileName(
            self, "Export IFF ILBM", "untitled.iff", "IFF ILBM (*.iff)")
        if not path: return
        try:
            from PIL import Image
            from apps.methods.iff_ilbm import write_iff_ilbm
            img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                                  bytes(self.dp5_canvas.rgba))
            p_img = img.quantize(colors=256)
            pal_flat = p_img.getpalette()
            palette = [(pal_flat[i*3], pal_flat[i*3+1], pal_flat[i*3+2]) for i in range(256)]
            data = write_iff_ilbm(self._canvas_width, self._canvas_height, palette, bytes(p_img.tobytes()))
            open(path, 'wb').write(data)
            self._set_status(f"Exported IFF: {__import__('os').path.basename(path)}")
        except Exception as e:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "IFF Export Error", str(e))

    def _redraw_chat(self):
        if self.dp5_canvas: self.dp5_canvas.update()

    def _update_ollama_status(self): pass
    def _refresh_session_list(self): pass
    def _cleanup_worker(self): pass
    def _stop_web_server(self): pass

    def _show_workshop_settings(self):
        try:
            if self.app_settings and APPSETTINGS_AVAILABLE:
                from apps.utils.app_settings_system import SettingsDialog
                dlg = SettingsDialog(self.app_settings, self)
                dlg.exec()
                self._apply_theme()
        except Exception as e:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Settings", str(e))

    def _set_status(self, msg: str):
        """Show a message in the window title bar area."""
        self.setWindowTitle(f"{App_name}  —  {msg}")

# Public factory functions (mirrors col_workshop.py pattern)

def open_ai_workshop(main_window=None) -> DP5Workshop:
    """Open AI Workshop standalone or embedded."""
    try:
        workshop = DP5Workshop(None, main_window)
        workshop.setWindowFlags(Qt.WindowType.Window)
        workshop.setWindowTitle(App_name)
        workshop.resize(1300, 800)
        workshop.show()
        return workshop
    except Exception as e:
        if main_window:
            QMessageBox.critical(main_window, App_name + "Error", str(e))
        return None


# Standalone entry point

if __name__ == "__main__":
    import traceback

    print(App_name + " starting…")
    try:
        app = QApplication(sys.argv)
        w = DP5Workshop()
        w.setWindowTitle(App_name + " – Standalone")
        w.resize(1300, 800)
        w.show()
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)
