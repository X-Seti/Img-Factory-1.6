# apps/components/Img_Factory/welcome_screen.py — Version 18
# X-Seti - 25Apr2026 - IMG Factory 1.6 - Welcome / Intro screen
"""Welcome / Intro screen shown on startup.
Full documentation of all IMG Factory features and workflows.
Stays on taskbar as [Intro] after dismiss. [x] disables on next launch."""

##Methods list -
# WelcomeCard.__init__
# WelcomeCard._build
# WelcomeCard._set_normal
# WelcomeCard._set_hover
# WelcomeScreen.__init__
# WelcomeScreen._build_ui
# WelcomeScreen._dismiss
# WelcomeScreen._save_startup_pref
# WelcomeScreen._section
# WelcomeScreen._subsection
# WelcomeScreen._doc_row
# WelcomeScreen.should_show_on_startup

import os
import sys
import json as _json
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QFrame, QScrollArea, QGridLayout, QSizePolicy, QCheckBox,
    QTabWidget, QTableWidget, QTableWidgetItem, QHeaderView,
    QAbstractItemView
)
from PyQt6.QtCore import Qt, pyqtSignal
from PyQt6.QtGui import QFont
from apps.app_info import App_name, App_build, App_auth, App_build_num, get_full_build


def _get_pref_path() -> str:
    """Cross-platform config path for welcome screen prefs."""
    import sys
    if sys.platform == 'win32':
        base = os.environ.get('APPDATA', os.path.expanduser('~'))
        return os.path.join(base, 'ImgFactory', 'welcome_prefs.json')
    return os.path.expanduser('~/.config/imgfactory/welcome_prefs.json')

_PREF_PATH = _get_pref_path()


class WelcomeCard(QFrame):
    """Clickable action card — icon + title + description."""
    clicked = pyqtSignal()

    # Class-level colour cache — populated by WelcomeScreen._resolve_card_colors()
    _card_bg:       str = "palette(base)"
    _card_bg_hover: str = "palette(alternateBase)"
    _border:        str = "palette(mid)"
    _border_hover:  str = "palette(highlight)"
    _accent:        str = "palette(highlight)"
    _text_primary:  str = "palette(windowText)"
    _text_secondary:str = "palette(mid)"
    _icon_color:    str = "#6a9fc0"

    def __init__(self, icon_text: str, title: str, desc: str,
                 accent: str = "", parent=None):
        super().__init__(parent)
        self.setFrameShape(QFrame.Shape.StyledPanel)
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        self.setFixedHeight(80)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._title_lbl = None
        self._desc_lbl  = None
        self._arr_lbl   = None
        self._build(icon_text, title, desc)
        self._set_normal()

    def _build(self, icon_text, title, desc): #vers 4
        outer = QHBoxLayout(self)
        outer.setContentsMargins(0, 0, 0, 0)
        outer.setSpacing(0)

        # Accent left strip
        self._strip = QFrame()
        self._strip.setFixedWidth(4)
        self._strip.setStyleSheet(
            f"QFrame {{ background: {WelcomeCard._accent}; border-radius: 3px 0 0 3px; }}")
        outer.addWidget(self._strip)

        # Content area
        inner = QHBoxLayout()
        inner.setContentsMargins(12, 8, 12, 8)
        inner.setSpacing(12)

        # Icon
        self._ico_lbl = QLabel()
        self._ico_lbl.setFixedSize(36, 36)
        self._ico_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._ico_lbl.setStyleSheet("background: transparent;")
        if isinstance(icon_text, str) and len(icon_text) <= 3:
            self._ico_lbl.setText(icon_text)
            self._ico_lbl.setFont(QFont("Arial", 11, QFont.Weight.Bold))
        else:
            try:
                from PyQt6.QtGui import QIcon as _QI
                if isinstance(icon_text, _QI):
                    self._ico_lbl.setPixmap(icon_text.pixmap(32, 32))
                else:
                    self._ico_lbl.setText(str(icon_text)[:3])
                    self._ico_lbl.setFont(QFont("Arial", 11, QFont.Weight.Bold))
            except Exception:
                self._ico_lbl.setText("?")
        inner.addWidget(self._ico_lbl)

        # Text block
        txt = QVBoxLayout()
        txt.setSpacing(3)
        self._title_lbl = QLabel(title)
        self._title_lbl.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        self._title_lbl.setStyleSheet(
            f"color: {WelcomeCard._text_primary}; background: transparent;")
        self._desc_lbl = QLabel(desc)
        self._desc_lbl.setFont(QFont("Arial", 8))
        self._desc_lbl.setWordWrap(True)
        self._desc_lbl.setStyleSheet(
            f"color: {WelcomeCard._text_secondary}; background: transparent;")
        txt.addWidget(self._title_lbl)
        txt.addWidget(self._desc_lbl)
        inner.addLayout(txt, 1)

        # Arrow
        self._arr_lbl = QLabel("›")
        self._arr_lbl.setFont(QFont("Arial", 18))
        self._arr_lbl.setStyleSheet(
            f"color: {WelcomeCard._accent}; background: transparent;")
        inner.addWidget(self._arr_lbl)

        outer.addLayout(inner, 1)

    def _set_normal(self): #vers 3
        self.setStyleSheet(
            f"WelcomeCard {{ background: {WelcomeCard._card_bg}; "
            f"border: 1px solid {WelcomeCard._border}; border-radius: 4px; }}")
        self._strip.setStyleSheet(
            f"QFrame {{ background: {WelcomeCard._border}; "
            f"border-radius: 3px 0 0 3px; }}")

    def _set_hover(self): #vers 3
        self.setStyleSheet(
            f"WelcomeCard {{ background: {WelcomeCard._card_bg_hover}; "
            f"border: 1px solid {WelcomeCard._border_hover}; border-radius: 4px; }}")
        self._strip.setStyleSheet(
            f"QFrame {{ background: {WelcomeCard._accent}; "
            f"border-radius: 3px 0 0 3px; }}")

    def enterEvent(self, e): self._set_hover(); super().enterEvent(e)
    def leaveEvent(self, e): self._set_normal(); super().leaveEvent(e)
    def mousePressEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton: self.clicked.emit()
        super().mousePressEvent(e)


class WelcomeScreen(QWidget):
    """Full welcome / intro panel — launched on startup, lives on taskbar as [Intro]."""

    open_img_requested  = pyqtSignal()
    open_dat_browser    = pyqtSignal()
    open_col_workshop   = pyqtSignal()
    open_txd_workshop   = pyqtSignal()
    open_model_workshop = pyqtSignal()
    open_dp5_workshop      = pyqtSignal()
    open_water_workshop    = pyqtSignal()
    open_radar_workshop    = pyqtSignal()
    open_dir_tree          = pyqtSignal()

    def __init__(self, main_window=None, parent=None):
        super().__init__(parent)
        self.setAttribute(Qt.WidgetAttribute.WA_OpaquePaintEvent)
        self.setAutoFillBackground(True)
        self.main_window = main_window
        self._build_ui()
        # Do not expand vertically beyond preferred size — prevents the welcome
        # screen from stretching the host window taller than the screen.
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)

    def _build_ui(self): #vers 7
        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(0)

        # Hero banner — reads accent/bg colours from app_settings theme so it
        # works on both light and dark themes without going invisible.
        hero = QFrame()
        hero.setMinimumHeight(60)
        hero.setMaximumHeight(80)

        _hero_bg  = '#1a1a2e'   # safe dark fallback
        _hero_txt = '#ffffff'
        _hero_sub = '#aaaacc'
        try:
            mw = self.main_window
            if mw and hasattr(mw, 'app_settings'):
                tc = mw.app_settings.get_theme_colors() or {}
                cs = mw.app_settings.current_settings
                from PyQt6.QtGui import QColor as _QC

                # Detect light/dark from bg_primary
                _bg_lum = _QC(tc.get('bg_primary', '#1a1a2e')).lightness()
                _is_dark = _bg_lum < 128

                # hero_bg is a direct override key — user can set it in Colors tab
                # Falls back to gradient start, then accent, then bg_secondary
                _direct = tc.get('hero_bg') or cs.get('hero_bg') or ''
                if _direct:
                    _hero_bg = _direct
                elif _is_dark:
                    _hero_bg = (tc.get('hero_gradient_dark_start')
                                or tc.get('accent_primary')
                                or tc.get('bg_secondary', '#1a1a2e'))
                else:
                    _hero_bg = (tc.get('hero_gradient_light_start')
                                or tc.get('accent_primary')
                                or tc.get('bg_secondary', '#1a1a2e'))

                # If hero colour is same as bg (e.g. both white), use accent instead
                _hero_lum = _QC(_hero_bg).lightness()
                _bg_col   = tc.get('bg_primary', '#ffffff')
                if abs(_hero_lum - _QC(_bg_col).lightness()) < 20:
                    _hero_bg = (tc.get('accent_primary') or '#1a1a2e')
                    _hero_lum = _QC(_hero_bg).lightness()

                _hero_txt = '#ffffff' if _hero_lum < 160 else '#111111'
                _hero_sub = '#ccccee' if _hero_lum < 128 else ('#888888' if _hero_lum < 200 else '#555555')
        except Exception:
            pass

        hero.setStyleSheet(
            f"QFrame {{ background: {_hero_bg}; border-bottom: 2px solid {_hero_sub}; }}")
        hl = QHBoxLayout(hero); hl.setContentsMargins(24, 0, 24, 0)
        AppBuildLabel = (f"{App_name} - Build {App_build}")
        title_lbl = QLabel(" " + AppBuildLabel)
        title_lbl.setFont(QFont("Arial", 20, QFont.Weight.Bold))
        title_lbl.setStyleSheet(f"color: {_hero_txt}; background: transparent;")
        sub_lbl = QLabel("GTA modding toolkit — IMG · COL · TXD · DFF · DAT · IPL · IDE")
        sub_lbl.setFont(QFont("Arial", 9))
        sub_lbl.setStyleSheet(f"color: {_hero_sub}; background: transparent;")
        vtxt = QVBoxLayout(); vtxt.setSpacing(2)
        vtxt.addWidget(title_lbl); vtxt.addWidget(sub_lbl)
        hl.addLayout(vtxt, 1)
        root.addWidget(hero)

        # Tab bar: Quick Start | Functions | Workflows | Shortcuts
        tabs = QTabWidget()
        tabs.setDocumentMode(True)
        tabs.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        root.addWidget(tabs, 1)  # stretch=1 — fills available height, doesn't expand to content

        tabs.addTab(self._build_quickstart_tab(), "Quick Start")
        tabs.addTab(self._build_functions_tab(),  "All Functions")
        tabs.addTab(self._build_workflows_tab(),  "Workflows")
        tabs.addTab(self._build_shortcuts_tab(),  "Shortcuts")

        # Bottom bar — show on startup checkbox + dismiss
        bot = QFrame()
        bot.setFixedHeight(40)
        _bot_bg  = 'palette(window)'
        _bot_txt = 'palette(windowText)'
        _bot_brd = 'palette(mid)'
        try:
            mw = self.main_window
            if mw and hasattr(mw, 'app_settings'):
                tc = mw.app_settings.get_theme_colors() or {}
                _bot_bg  = tc.get('bg_primary',   _bot_bg)
                _bot_txt = tc.get('text_primary',  _bot_txt)
                _bot_brd = tc.get('border',        _bot_brd)
        except Exception:
            pass
        bot.setStyleSheet(f"""
            QFrame {{ border-top: 1px solid {_bot_brd}; background: {_bot_bg}; }}
            QCheckBox {{ color: {_bot_txt}; background: transparent; }}
            QLabel    {{ color: {_bot_txt}; background: transparent; }}
            QPushButton {{ color: {_bot_txt}; }}
        """)
        bl = QHBoxLayout(bot); bl.setContentsMargins(16, 6, 16, 6); bl.setSpacing(12)

        self._show_cb = QCheckBox("Show on startup")
        self._show_cb.setFont(QFont("Arial", 9))
        _show = True
        try: _show = _json.load(open(_PREF_PATH)).get('show_on_startup', True)
        except Exception: pass
        self._show_cb.setChecked(_show)
        self._show_cb.toggled.connect(self._save_startup_pref)
        bl.addWidget(self._show_cb)
        bl.addStretch()

        hint = QLabel("Close to dismiss — reopen via  [Intro]  in the taskbar")
        hint.setFont(QFont("Arial", 8))
        hint.setStyleSheet("color: palette(placeholderText);")
        bl.addWidget(hint)

        # Search button next to Close
        from PyQt6.QtWidgets import QToolButton
        search_btn = QToolButton()
        search_btn.setFixedSize(26, 26)
        search_btn.setToolTip("Search entries (Ctrl+F)")
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVGB
            search_btn.setIcon(_SVGB.search_icon(16, '#888888'))
            from PyQt6.QtCore import QSize as _QSB
            search_btn.setIconSize(_QSB(16, 16))
        except Exception:
            search_btn.setText("Q")
        search_btn.clicked.connect(self._search_clicked)
        bl.addWidget(search_btn)

        dismiss = QPushButton("Close")
        dismiss.setFixedHeight(26)
        dismiss.setFont(QFont("Arial", 9))
        dismiss.setToolTip("Dismiss screen — reopen via [Intro] in taskbar")
        dismiss.clicked.connect(self._dismiss)
        bl.addWidget(dismiss)
        root.addWidget(bot)

    #  Tab builders

    def _resolve_card_colors(self, container_widget=None): #vers 1
        """Pull theme colours into WelcomeCard class vars and optionally style the container."""
        try:
            mw = self.main_window
            if not (mw and hasattr(mw, 'app_settings')):
                return
            tc = mw.app_settings.get_theme_colors() or {}
            from PyQt6.QtGui import QColor as _QC

            bg_primary  = tc.get('bg_primary',  '#272727')
            panel_bg    = tc.get('panel_bg',    '#333333')
            accent      = tc.get('accent_primary', '#1976d2')
            text_pri    = tc.get('text_primary',  '#ffffff')
            text_sec    = tc.get('text_secondary', '#aaaaaa') or tc.get('text_accent', '#aaaaaa')

            lum = _QC(panel_bg).lightness()
            is_dark = lum < 128

            # Card surface: panel_bg from theme (the defined card colour)
            card_bg = panel_bg

            # Hover: slightly lighter/darker than card_bg
            qc = _QC(card_bg)
            card_bg_hover = (qc.lighter(120).name() if is_dark else qc.darker(108).name())

            # Border: subtle — halfway between card and bg
            qb = _QC(bg_primary)
            border = _QC(
                (qb.red()   + qc.red())   // 2,
                (qb.green() + qc.green()) // 2,
                (qb.blue()  + qc.blue())  // 2
            ).name()
            border_hover = accent

            # Icon colour: accent lightened a little for dark, slightly darkened for light
            icon_col = (_QC(accent).lighter(120).name() if is_dark
                        else _QC(accent).darker(110).name())

            WelcomeCard._card_bg        = card_bg
            WelcomeCard._card_bg_hover  = card_bg_hover
            WelcomeCard._border         = border
            WelcomeCard._border_hover   = border_hover
            WelcomeCard._accent         = accent
            WelcomeCard._text_primary   = text_pri
            WelcomeCard._text_secondary = text_sec
            WelcomeCard._icon_color     = icon_col

            # Outer tab background = bg_primary (darker than cards — makes cards pop)
            if container_widget:
                container_widget.setStyleSheet(
                    f"QWidget {{ background: {bg_primary}; }}")
        except Exception:
            pass

    def _build_quickstart_tab(self): #vers 5
        from PyQt6.QtCore import Qt as _Qt
        # Scroll area — vertical scrollbar always visible (ready for future cards)
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        scroll.setHorizontalScrollBarPolicy(_Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        scroll.setVerticalScrollBarPolicy(_Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        scroll.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)

        w = QWidget()
        w.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Preferred)
        lay = QVBoxLayout(w); lay.setContentsMargins(12, 12, 12, 12); lay.setSpacing(10)

        self._resolve_card_colors(w)

        lay.addWidget(self._section("Open & Browse"))
        g1 = QGridLayout(); g1.setSpacing(12)
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVG
        _ic = WelcomeCard._icon_color
        qs = [
            (_SVG.open_icon(36, _ic), "Open IMG File",
             "File → Open IMG, or drag a .img/.cd file onto the window. "
             "Entries appear in the table — double-click to view.",
             self.open_img_requested),
            (_SVG.database_icon(36, _ic), "DAT Browser",
             "Load a GTA gta.dat / gta3.dat / gta_vc.dat to index every IMG, "
             "IDE and IPL in the game. Explore 13 000+ objects by name or type.",
             self.open_dat_browser),
            (_SVG.folder_icon(36, _ic), "File Browser",
             "Navigate your disk, preview textures, and drag files into an open IMG "
             "or export entries to a folder.",
             self.open_dir_tree),
        ]
        for i, (ico, ttl, dsc, sig) in enumerate(qs):
            c = WelcomeCard(ico, ttl, dsc); c.clicked.connect(sig.emit)
            g1.addWidget(c, 0, i)
        lay.addLayout(g1)

        lay.addWidget(self._section("Asset Editors"))
        g2 = QGridLayout(); g2.setSpacing(12)
        g2.setColumnStretch(0, 1); g2.setColumnStretch(1, 1); g2.setColumnStretch(2, 1)
        eds = [
            (_SVG.paint_icon(36, _ic), "TXD Workshop",
             "Open any .txd inside an IMG or standalone. Preview, replace, export "
             "and convert textures for PC, PS2, Xbox and mobile.",
             self.open_txd_workshop),
            (_SVG.manage_icon(36, _ic), "COL Workshop",
             "Inspect and edit collision data — spheres, boxes and mesh faces. "
             "Paint surface materials, export as OBJ or COL.",
             self.open_col_workshop),
            (_SVG.view_icon(36, _ic), "Model Workshop",
             "View DFF geometry and frame hierarchy. Load the linked TXD automatically "
             "via the DAT Browser IDE entry. Textured 3D preview.",
             self.open_model_workshop),
            (_SVG.paint_icon(36, _ic), "DP5 Paint",
             "Deluxe Paint-style bitmap editor. Draw, spray, clone and edit textures "
             "directly — works on any IMG entry or standalone image.",
             self.open_dp5_workshop),
            (_SVG.view_icon(36, _ic), "Water Workshop",
             "Edit GTA water planes — adjust water zones, opacity and flow "
             "for SA, VC and III water.dat files.",
             self.open_water_workshop),
            (_SVG.view_icon(36, _ic), "Radar Workshop",
             "Browse, export and replace GTA radar tile textures. "
             "Supports SA 12×12 radar grid and VC/III formats.",
             self.open_radar_workshop),
        ]
        for i, (ico, ttl, dsc, sig) in enumerate(eds):
            c = WelcomeCard(ico, ttl, dsc); c.clicked.connect(sig.emit)
            g2.addWidget(c, i // 3, i % 3)
        lay.addLayout(g2)

        tip = QLabel(
            "Tip:  Right-click any entry in the IMG table for context menu options: "
            "Edit, Extract, Replace, Rename, Validate, Open in Workshop.")
        tip.setWordWrap(True)
        tip.setFont(QFont("Arial", 9))
        _tip_bg  = WelcomeCard._card_bg_hover
        _tip_bdr = WelcomeCard._border
        _tip_txt = WelcomeCard._text_secondary
        tip.setStyleSheet(
            f"QLabel {{ background: {_tip_bg}; border: 1px solid {_tip_bdr}; "
            f"color: {_tip_txt}; border-radius: 4px; padding: 8px 12px; }}")
        lay.addWidget(tip)
        lay.addStretch()
        scroll.setWidget(w)
        return scroll

    def _build_functions_tab(self): #vers 2
        from PyQt6.QtCore import Qt as _Qt
        scroll = QScrollArea(); scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        scroll.setHorizontalScrollBarPolicy(_Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        scroll.setVerticalScrollBarPolicy(_Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        w = QWidget()
        lay = QVBoxLayout(w); lay.setContentsMargins(16, 12, 16, 12); lay.setSpacing(12)

        #  IMG Factory core
        lay.addWidget(self._section("IMG Factory — Core"))
        lay.addWidget(self._doc_table([
            ("Open IMG",           "File → Open IMG  or  Ctrl+O",
             "Load a .img / .cd file. All entries shown in the main table."),
            ("New IMG",            "File → New IMG",
             "Create a blank Version 1 (GTA3/VC) or Version 2 (SA) archive."),
            ("Hybrid Load",        "File → Hybrid Load",
             "Open an IMG and its paired .dir side-car file simultaneously."),
            ("Extract Entry",      "Right-click → Extract  or  Ctrl+E",
             "Save the selected entry to disk. Multi-select supported."),
            ("Replace Entry",      "Right-click → Replace",
             "Swap a file inside the IMG with a file from disk."),
            ("Import Entry",       "Right-click → Import  or  Ctrl+I",
             "Add a new file to the archive."),
            ("Rebuild IMG",        "Rebuild button in toolbar",
             "Defragment and rewrite the archive — required after many replacements."),
            ("Validate",           "Right-click → Validate",
             "Check the selected entry for format errors."),
            ("Rename Entry",       "Right-click → Rename  or  F2",
             "Rename a file inside the archive."),
            ("Sort",               "Right-click → Sort",
             "Alphabetically sort all entries in the archive."),
            ("Search",             "Ctrl+F  or  search box",
             "Filter entries by name, extension or size."),
        ]))

        #  DAT Browser  
        lay.addWidget(self._section("DAT Browser"))
        lay.addWidget(self._doc_table([
            ("Load game",          "DAT button → select .dat file",
             "Parses gta.dat / gta3.dat and indexes all referenced IMG/IDE/IPL files."),
            ("Asset DB",           "⬡ DB button",
             "Build or update a SQLite index of every model, texture and COL in the game. "
             "Enables instant lookup by name."),
            ("Browse objects",     "IDE tab in DAT Browser",
             "Lists every objs / tobj / anim entry with ID, model name, TXD, draw distance."),
            ("Browse instances",   "IPL tab",
             "Shows every placed object in the game world with position and rotation."),
            ("Open model",         "Double-click any model row",
             "Opens the DFF + linked TXD in Model Workshop automatically."),
            ("Open TXD",           "Right-click → Open TXD",
             "Extracts and opens the linked TXD in TXD Workshop."),
        ]))

        #  TXD Workshop
        lay.addWidget(self._section("TXD Workshop"))
        lay.addWidget(self._doc_table([
            ("Open TXD",           "Load button  or  Ctrl+O",
             "Open a .txd from disk or extract directly from an open IMG."),
            ("Preview texture",    "Click any row",
             "Full preview in right panel. Toggle Normal / Alpha Mask / Combined."),
            ("Replace texture",    "Right-click → Replace",
             "Import a PNG/BMP/TGA/JPG and re-encode to the original format."),
            ("Export texture",     "Right-click → Export  or  Export All",
             "Save textures as PNG with optional alpha channel."),
            ("Convert format",     "Right-click → Convert",
             "Re-encode to DXT1/DXT3/DXT5/RGBA8 or change bit depth."),
            ("Upscale",            "Upscale button",
             "AI-style upscale (2× / 4×) using nearest-neighbour or bilinear filter."),
            ("Platform support",   "—",
             "PC (DXT/RGBA), PS2 (PSMT8/PSMT4), Xbox, Android, iOS, PSP, GameCube."),
        ]))

        #  COL Workshop
        lay.addWidget(self._section("COL Workshop"))
        lay.addWidget(self._doc_table([
            ("Open COL",           "Open button  or  from IMG",
             "Load a standalone .col or any .col entry inside an IMG."),
            ("3D Viewport",        "Centre panel",
             "Orbit: right-drag.  Pan: middle-drag.  Zoom: scroll wheel."),
            ("View modes",         "Toolbar buttons",
             "Toggle spheres, boxes, mesh faces and backface culling independently."),
            ("Paint surfaces",     "Paint button",
             "Click faces to assign surface material types (grass/concrete/metal etc)."),
            ("Surface types",      "Surface Type button",
             "Pick from the GTA3/VC/SA surface palette — affects audio, physics, particles."),
            ("Export COL",         "Export button",
             "Save as .col, .cst (collision text), .obs (object boundary) or .obj."),
            ("Import elements",    "Import button",
             "Load OBJ/FBX geometry as collision mesh faces.  STUB — next release."),
        ]))

        #  Model Workshop
        lay.addWidget(self._section("Model Workshop"))
        lay.addWidget(self._doc_table([
            ("Open DFF",           "Open button  or  double-click in DAT Browser",
             "Load a .dff model. Geometry list, frame hierarchy and 3D preview shown."),
            ("Load TXD",           "TXD button  or  Auto-find",
             "Auto-find searches open IMGs for the IDE-linked TXD automatically."),
            ("Render modes",       "Solid / Textured / Semi / Wire buttons",
             "Switch between solid-shaded, UV-mapped texture, semi-transparent or wireframe."),
            ("Viewport lighting",  "Lightbulb button in toolbar",
             "Set light direction (hemisphere picker), intensity and ambient. Saved to JSON."),
            ("Shading toggle",     "S button in toolbar",
             "Turn Lambertian shading on/off — off shows flat full-brightness colours."),
            ("Material Editor",    "Checkerboard button",
             "3ds Max-style material grid. Sphere/cube/flat previews. Edit texture name, "
             "colour, swap TXD set, import/export textures. Right-click slots for options."),
            ("Create primitive",   "+□ button (DFF mode)",
             "Add a Box/Sphere/Cylinder/Plane to the DFF geometry with subdivision control."),
            ("Create COL from DFF","COL-from-DFF button",
             "Generate a COL1/2/3 binary from the DFF mesh vertices automatically."),
            ("Prelighting",        "Prelight row in bottom panel",
             "Setup ambient + sun direction for vertex colour baking.  STUB — next release."),
        ]))

        lay.addStretch()
        scroll.setWidget(w)
        return scroll

    def _build_workflows_tab(self): #vers 2
        from PyQt6.QtCore import Qt as _Qt
        scroll = QScrollArea(); scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        scroll.setHorizontalScrollBarPolicy(_Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        scroll.setVerticalScrollBarPolicy(_Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        w = QWidget()
        lay = QVBoxLayout(w); lay.setContentsMargins(20, 16, 20, 16); lay.setSpacing(16)

        wf = [
            ("Replace a vehicle texture",
             "1. Open the vehicle IMG (e.g. player.img).\n"
             "2. Find the .txd entry (e.g. admiral.txd) — double-click to open in TXD Workshop.\n"
             "3. Select the texture row, right-click → Replace → pick your PNG.\n"
             "4. Click Save TXD. Back in IMG Factory, click Rebuild."),
            ("Browse the full game world",
             "1. Click DAT browser, load gta_vc.dat (or gta3.dat / gta.dat for SA).\n"
             "2. The IDE tab lists every object. Filter by name or section.\n"
             "3. Click Build DB to index all files — enables instant lookups.\n"
             "4. Double-click any model row to open its DFF + TXD in Model Workshop."),
            ("Extract and view a building model",
             "1. Open DAT Browser → find the model in the IDE list.\n"
             "2. Double-click — Model Workshop opens with the DFF and textures loaded.\n"
             "3. Switch to Textured mode to see the UV-mapped result.\n"
             "4. Open Material Editor to inspect and swap textures."),
            ("Edit COL surface types for a road",
             "1. Open the IMG, find the .col entry, right-click → COL Workshop.\n"
             "2. In COL Workshop, click Paint, then Surface Type.\n"
             "3. Click faces to paint them with the correct surface material.\n"
             "4. Export as .col and replace in the IMG, then Rebuild."),
            ("Swap a model's texture set",
             "1. Open Model Workshop with the DFF.\n"
             "2. Open Material Editor (checkerboard button).\n"
             "3. In the Use TXD field, type the new TXD name and click Apply.\n"
             "4. The viewport immediately reloads textures from the new TXD."),
            ("Batch extract all textures from an IMG",
             "1. Open the IMG, Ctrl+A to select all.\n"
             "2. Filter by extension .txd if needed.\n"
             "3. Right-click → Extract All → choose output folder.\n"
             "4. All TXDs extracted; open any in TXD Workshop to view individual textures."),
        ]

        for title, steps in wf:
            frame = QFrame()
            frame.setStyleSheet(
                "QFrame { border: 1px solid palette(mid); border-radius: 4px; "
                "background: palette(base); }")
            fl = QVBoxLayout(frame); fl.setContentsMargins(12, 10, 12, 10); fl.setSpacing(4)
            t = QLabel(title)
            t.setFont(QFont("Arial", 10, QFont.Weight.Bold))
            fl.addWidget(t)
            s = QLabel(steps)
            s.setFont(QFont("Courier New", 9))
            s.setStyleSheet("color: palette(windowText);")
            s.setWordWrap(True)
            fl.addWidget(s)
            lay.addWidget(frame)

        lay.addStretch()
        scroll.setWidget(w)
        return scroll

    def _build_shortcuts_tab(self): #vers 2
        from PyQt6.QtCore import Qt as _Qt
        scroll = QScrollArea(); scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        scroll.setHorizontalScrollBarPolicy(_Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        scroll.setVerticalScrollBarPolicy(_Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        w = QWidget()
        lay = QVBoxLayout(w); lay.setContentsMargins(16, 12, 16, 12); lay.setSpacing(12)

        lay.addWidget(self._section("Keyboard Shortcuts"))
        lay.addWidget(self._doc_table([
            ("Ctrl+O",    "Open IMG",                "Open a .img / .cd file"),
            ("Ctrl+N",    "New IMG",                 "Create a new empty archive"),
            ("Ctrl+E",    "Extract",                 "Extract selected entries to disk"),
            ("Ctrl+I",    "Import",                  "Import file(s) into the archive"),
            ("Ctrl+F",    "Find / Search",           "Filter entries by name or extension"),
            ("Ctrl+A",    "Select All",              "Select all entries in the table"),
            ("Ctrl+Z",    "Undo",                    "Undo last change (COL/TXD/Model)"),
            ("Ctrl+S",    "Save",                    "Save current file"),
            ("F2",        "Rename",                  "Rename selected entry"),
            ("Delete",    "Remove entry",            "Delete selected entries from archive"),
            ("Ctrl+T",    "Open TXD (Model Wkshp)", "Load TXD for current DFF"),
            ("G",         "Translate gizmo",         "Switch 3D gizmo to move mode"),
            ("R",         "Rotate gizmo",            "Switch 3D gizmo to rotate mode"),
            ("Escape",    "Exit mode",               "Exit paint/select mode in viewport"),
            ("Scroll",    "Zoom viewport",           "Zoom in/out in 3D preview"),
            ("Right-drag","Orbit viewport",          "Rotate view in 3D preview"),
            ("Mid-drag",  "Pan viewport",            "Pan 3D preview"),
        ], col_widths=(90, 160, None)))

        lay.addStretch()
        scroll.setWidget(w)
        return scroll

    #  Helpers  

    def _section(self, text: str) -> QLabel: #vers 1
        lbl = QLabel(text)
        lbl.setFont(QFont("Arial", 11, QFont.Weight.Bold))
        lbl.setStyleSheet(
            "QLabel { color: palette(windowText); "
            "border-bottom: 2px solid palette(mid); padding-bottom: 3px; }")
        return lbl

    def _doc_table(self, rows: list, col_widths=None) -> QTableWidget: #vers 1
        """Build a read-only 3-column reference table."""
        is_shortcut = (col_widths is not None)
        headers = (["Key", "Action", "Description"] if is_shortcut
                   else ["Feature", "How to access", "Description"])
        tbl = QTableWidget(len(rows), 3)
        tbl.setHorizontalHeaderLabels(headers)
        tbl.horizontalHeader().setStretchLastSection(True)
        tbl.horizontalHeader().setSectionResizeMode(0, QHeaderView.ResizeMode.ResizeToContents)
        tbl.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        tbl.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        tbl.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        tbl.setAlternatingRowColors(True)
        tbl.verticalHeader().setVisible(False)
        tbl.setShowGrid(False)
        tbl.setFont(QFont("Arial", 9))
        tbl.verticalHeader().setDefaultSectionSize(22)
        tbl.setFrameShape(QFrame.Shape.NoFrame)

        for r, row in enumerate(rows):
            for c, val in enumerate(row):
                item = QTableWidgetItem(val)
                if c == 0:
                    item.setFont(QFont("Courier New", 9, QFont.Weight.Bold))
                tbl.setItem(r, c, item)

        # Size hint — enough rows visible without scroll
        h = min(len(rows), 12) * 23 + tbl.horizontalHeader().height() + 4
        tbl.setFixedHeight(h)
        return tbl

    def _save_startup_pref(self, show: bool): #vers 1
        os.makedirs(os.path.dirname(_PREF_PATH), exist_ok=True)
        try:
            data = {}
            try: data = _json.load(open(_PREF_PATH))
            except Exception: pass
            data['show_on_startup'] = show
            _json.dump(data, open(_PREF_PATH, 'w'), indent=2)
        except Exception:
            pass

    def _search_clicked(self): #vers 1
        """Forward search click to imgfactory main window."""
        try:
            mw = self.main_window
            if mw and hasattr(mw, '_show_search_dialog'):
                mw._show_search_dialog()
        except Exception:
            pass

    def _dismiss(self): #vers 1
        """Hide the screen — stays on taskbar as [Intro] for re-open."""
        self.setVisible(False)

    @staticmethod
    def should_show_on_startup() -> bool: #vers 1
        try:
            return _json.load(open(_PREF_PATH)).get('show_on_startup', True)
        except Exception:
            return True
