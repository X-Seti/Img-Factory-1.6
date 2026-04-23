# apps/components/Img_Factory/welcome_screen.py — Version 4
# X-Seti - Apr 2026 - IMG Factory 1.6 - Welcome / Intro screen
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
import json as _json
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QFrame, QScrollArea, QGridLayout, QSizePolicy, QCheckBox,
    QTabWidget, QTableWidget, QTableWidgetItem, QHeaderView,
    QAbstractItemView
)
from PyQt6.QtCore import Qt, pyqtSignal
from PyQt6.QtGui import QFont, QColor


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

    def __init__(self, icon_text: str, title: str, desc: str,
                 accent: str = "palette(link)", parent=None):
        super().__init__(parent)
        self._accent = accent
        self.setFrameShape(QFrame.Shape.StyledPanel)
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        self.setFixedHeight(100)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._build(icon_text, title, desc)
        self._set_normal()

    def _build(self, icon_text, title, desc): #vers 1
        lay = QHBoxLayout(self)
        lay.setContentsMargins(12, 8, 12, 8)
        lay.setSpacing(12)
        ico = QLabel(icon_text)
        ico.setFont(QFont("Segoe UI Emoji", 24))
        ico.setFixedWidth(40)
        ico.setAlignment(Qt.AlignmentFlag.AlignCenter)
        lay.addWidget(ico)
        txt = QVBoxLayout(); txt.setSpacing(2)
        t = QLabel(title); t.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        d = QLabel(desc); d.setFont(QFont("Arial", 9))
        d.setWordWrap(True)
        d.setStyleSheet("color: palette(placeholderText);")
        txt.addWidget(t); txt.addWidget(d)
        lay.addLayout(txt, 1)
        arr = QLabel("›"); arr.setFont(QFont("Arial", 16))
        arr.setStyleSheet("color: palette(link);")
        lay.addWidget(arr)

    def _set_normal(self): #vers 1
        self.setStyleSheet(
            "WelcomeCard { border: 1px solid palette(mid); border-radius: 5px; "
            "background: palette(base); }")

    def _set_hover(self): #vers 1
        self.setStyleSheet(
            "WelcomeCard { border: 2px solid palette(link); border-radius: 5px; "
            "background: palette(alternateBase); }")

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
    open_dir_tree       = pyqtSignal()

    def __init__(self, main_window=None, parent=None):
        super().__init__(parent)
        self.main_window = main_window
        self._build_ui()
        # Do not expand vertically beyond preferred size — prevents the welcome
        # screen from stretching the host window taller than the screen.
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Preferred)

    def _build_ui(self): #vers 3
        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(0)

        # Hero banner
        hero = QFrame()
        hero.setFixedHeight(80)
        hero.setStyleSheet(
            "QFrame { background: qlineargradient("
            "x1:0,y1:0,x2:1,y2:0,"
            "stop:0 palette(dark), stop:1 palette(shadow)); }")
        hl = QHBoxLayout(hero); hl.setContentsMargins(24, 0, 24, 0)
        title_lbl = QLabel("IMG Factory 1.6")
        title_lbl.setFont(QFont("Arial", 20, QFont.Weight.Bold))
        title_lbl.setStyleSheet("color: palette(brightText); background: transparent;")
        sub_lbl = QLabel("GTA modding toolkit — IMG · COL · TXD · DFF · DAT · IPL · IDE")
        sub_lbl.setFont(QFont("Arial", 9))
        sub_lbl.setStyleSheet("color: palette(placeholderText); background: transparent;")
        vtxt = QVBoxLayout(); vtxt.setSpacing(2)
        vtxt.addWidget(title_lbl); vtxt.addWidget(sub_lbl)
        hl.addLayout(vtxt, 1)
        root.addWidget(hero)

        # Tab bar: Quick Start | Functions | Workflows | Shortcuts
        tabs = QTabWidget()
        tabs.setDocumentMode(True)
        root.addWidget(tabs)   # no stretch — let sizeHint govern height

        tabs.addTab(self._build_quickstart_tab(), "Quick Start")
        tabs.addTab(self._build_functions_tab(),  "All Functions")
        tabs.addTab(self._build_workflows_tab(),  "Workflows")
        tabs.addTab(self._build_shortcuts_tab(),  "Shortcuts")

        # Bottom bar — show on startup checkbox + dismiss
        bot = QFrame()
        bot.setFixedHeight(40)
        bot.setStyleSheet(
            "QFrame { border-top: 1px solid palette(mid); background: palette(window); }")
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

        dismiss = QPushButton("✕  Close")
        dismiss.setFixedHeight(26)
        dismiss.setFont(QFont("Arial", 9))
        dismiss.setToolTip("Dismiss screen — reopen via [Intro] in taskbar")
        dismiss.clicked.connect(self._dismiss)
        bl.addWidget(dismiss)
        root.addWidget(bot)

    # ── Tab builders ────────────────────────────────────────────────────

    def _build_quickstart_tab(self): #vers 1
        w = QWidget()
        lay = QVBoxLayout(w); lay.setContentsMargins(20, 16, 20, 16); lay.setSpacing(14)

        lay.addWidget(self._section("Open & Browse"))
        g1 = QGridLayout(); g1.setSpacing(8)
        qs = [
            ("📂", "Open IMG File",
             "File → Open IMG, or drag a .img/.cd file onto the window. "
             "Entries appear in the table — double-click to view.",
             self.open_img_requested),
            ("🌍", "DAT Browser",
             "Load a GTA gta.dat / gta3.dat / gta_vc.dat to index every IMG, "
             "IDE and IPL in the game. Explore 13 000+ objects by name or type.",
             self.open_dat_browser),
            ("📁", "File Browser",
             "Navigate your disk, preview textures, and drag files into an open IMG "
             "or export entries to a folder.",
             self.open_dir_tree),
        ]
        for i, (ico, ttl, dsc, sig) in enumerate(qs):
            c = WelcomeCard(ico, ttl, dsc); c.clicked.connect(sig.emit)
            g1.addWidget(c, 0, i)
        lay.addLayout(g1)

        lay.addWidget(self._section("Asset Editors"))
        g2 = QGridLayout(); g2.setSpacing(8)
        eds = [
            ("🖼", "TXD Workshop",
             "Open any .txd inside an IMG or standalone. Preview, replace, export "
             "and convert textures for PC, PS2, Xbox and mobile.",
             self.open_txd_workshop),
            ("🧱", "COL Workshop",
             "Inspect and edit collision data — spheres, boxes and mesh faces. "
             "Paint surface materials, export as OBJ or COL.",
             self.open_col_workshop),
            ("🗿", "Model Workshop",
             "View DFF geometry and frame hierarchy. Load the linked TXD automatically "
             "via the DAT Browser IDE entry. Textured 3D preview.",
             self.open_model_workshop),
        ]
        for i, (ico, ttl, dsc, sig) in enumerate(eds):
            c = WelcomeCard(ico, ttl, dsc); c.clicked.connect(sig.emit)
            g2.addWidget(c, 0, i)
        lay.addLayout(g2)

        tip = QLabel(
            "Tip:  Right-click any entry in the IMG table for context menu options: "
            "Edit, Extract, Replace, Rename, Validate, Open in Workshop.")
        tip.setWordWrap(True)
        tip.setFont(QFont("Arial", 9))
        tip.setStyleSheet(
            "QLabel { background: palette(alternateBase); border: 1px solid palette(mid); "
            "border-radius: 4px; padding: 8px 12px; }")
        lay.addWidget(tip)
        lay.addStretch()
        return w

    def _build_functions_tab(self): #vers 1
        w = QWidget()
        lay = QVBoxLayout(w); lay.setContentsMargins(16, 12, 16, 12); lay.setSpacing(12)

        # ── IMG Factory core ──────────────────────────────────────────────
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

        # ── DAT Browser ───────────────────────────────────────────────────
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

        # ── TXD Workshop ──────────────────────────────────────────────────
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

        # ── COL Workshop ──────────────────────────────────────────────────
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

        # ── Model Workshop ────────────────────────────────────────────────
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
        return w

    def _build_workflows_tab(self): #vers 1
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
        return w

    def _build_shortcuts_tab(self): #vers 1
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
        return w

    # ── Helpers ─────────────────────────────────────────────────────────

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

    def _dismiss(self): #vers 1
        """Hide the screen — stays on taskbar as [Intro] for re-open."""
        self.setVisible(False)

    @staticmethod
    def should_show_on_startup() -> bool: #vers 1
        try:
            return _json.load(open(_PREF_PATH)).get('show_on_startup', True)
        except Exception:
            return True
