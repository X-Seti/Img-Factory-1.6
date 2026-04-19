# apps/components/Img_Factory/welcome_screen.py — Version 1
"""Welcome / home screen shown on first tab of IMG Factory.
Replaces the bare 'No File' table with an interactive intro panel."""

import os
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QFrame, QScrollArea, QGridLayout, QSizePolicy
)
from PyQt6.QtCore import Qt, pyqtSignal, QSize
from PyQt6.QtGui import QFont, QColor, QPalette


class WelcomeCard(QFrame):
    """A clickable card with icon + title + description."""
    clicked = pyqtSignal()

    def __init__(self, icon_text: str, title: str, desc: str,
                 accent: str = "#16a34a", parent=None):
        super().__init__(parent)
        self._accent = accent
        self.setFrameShape(QFrame.Shape.StyledPanel)
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        self.setFixedHeight(110)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._build(icon_text, title, desc)
        self._set_normal()

    def _build(self, icon_text, title, desc):
        lay = QHBoxLayout(self)
        lay.setContentsMargins(14, 10, 14, 10)
        lay.setSpacing(14)

        ico = QLabel(icon_text)
        ico.setFont(QFont("Segoe UI Emoji", 26))
        ico.setFixedWidth(44)
        ico.setAlignment(Qt.AlignmentFlag.AlignCenter)
        lay.addWidget(ico)

        txt = QVBoxLayout()
        txt.setSpacing(2)
        t = QLabel(title)
        t.setFont(QFont("Arial", 11, QFont.Weight.Bold))
        d = QLabel(desc)
        d.setFont(QFont("Arial", 9))
        d.setWordWrap(True)
        d.setStyleSheet("color: palette(mid);")
        txt.addWidget(t)
        txt.addWidget(d)
        lay.addLayout(txt, 1)

        arr = QLabel("›")
        arr.setFont(QFont("Arial", 18))
        arr.setStyleSheet(f"color: {self._accent};")
        lay.addWidget(arr)

    def _set_normal(self):
        self.setStyleSheet(
            "WelcomeCard { border: 1px solid palette(mid); border-radius: 6px; "
            "background-color: palette(base); }")

    def _set_hover(self):
        self.setStyleSheet(
            f"WelcomeCard {{ border: 2px solid {self._accent}; border-radius: 6px; "
            "background-color: palette(alternateBase); }")

    def enterEvent(self, e):
        self._set_hover(); super().enterEvent(e)

    def leaveEvent(self, e):
        self._set_normal(); super().leaveEvent(e)

    def mousePressEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton:
            self.clicked.emit()
        super().mousePressEvent(e)


class WelcomeScreen(QWidget):
    """Full welcome/home panel shown as the first tab."""

    # Signals so imgfactory can connect actions
    open_img_requested       = pyqtSignal()
    open_dat_browser         = pyqtSignal()
    open_col_workshop        = pyqtSignal()
    open_txd_workshop        = pyqtSignal()
    open_model_workshop      = pyqtSignal()
    open_dir_tree            = pyqtSignal()
    load_all_game_imgs       = pyqtSignal()   # load all from current DAT

    def __init__(self, main_window=None, parent=None):
        super().__init__(parent)
        self.main_window = main_window
        self._build_ui()

    def _build_ui(self):
        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(0)

        # ── Hero banner ──────────────────────────────────────────────────
        hero = QFrame()
        hero.setFixedHeight(90)
        hero.setStyleSheet(
            "QFrame { background: qlineargradient("
            "x1:0,y1:0,x2:1,y2:0,"
            "stop:0 #15803d, stop:1 #166534); }")
        hero_lay = QHBoxLayout(hero)
        hero_lay.setContentsMargins(28, 0, 28, 0)

        title = QLabel("IMG Factory 1.6")
        title.setFont(QFont("Arial", 22, QFont.Weight.Bold))
        title.setStyleSheet("color: #f0fdf4; background: transparent;")
        sub = QLabel("GTA modding toolkit — open an IMG, browse a game world, or edit assets")
        sub.setFont(QFont("Arial", 10))
        sub.setStyleSheet("color: #bbf7d0; background: transparent;")

        vtxt = QVBoxLayout()
        vtxt.setSpacing(2)
        vtxt.addWidget(title)
        vtxt.addWidget(sub)
        hero_lay.addLayout(vtxt, 1)
        root.addWidget(hero)

        # ── Scrollable content ───────────────────────────────────────────
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        content = QWidget()
        cl = QVBoxLayout(content)
        cl.setContentsMargins(28, 20, 28, 28)
        cl.setSpacing(20)
        scroll.setWidget(content)
        root.addWidget(scroll, 1)

        # ── Quick start ──────────────────────────────────────────────────
        cl.addWidget(self._section("Quick Start"))
        qs_grid = QGridLayout()
        qs_grid.setSpacing(10)

        qs_cards = [
            ("📂", "Open IMG file",
             "Load a .img archive and browse, extract, replace or rebuild its entries.",
             "#16a34a", self.open_img_requested),
            ("🌍", "Load game world via DAT Browser",
             "Parse a GTA .dat file to load all IDEs, IPLs and IMG archives at once — "
             "browse every object, instance and zone the game sees.",
             "#0e7490", self.open_dat_browser),
            ("📁", "Browse files on disk",
             "Open the directory tree to navigate folders, preview files and drag "
             "assets into an IMG.",
             "#7c3aed", self.open_dir_tree),
        ]
        for i, (ico, ttl, dsc, acc, sig) in enumerate(qs_cards):
            card = WelcomeCard(ico, ttl, dsc, acc)
            card.clicked.connect(sig.emit)
            qs_grid.addWidget(card, 0, i)
        cl.addLayout(qs_grid)

        # ── Editors ──────────────────────────────────────────────────────
        cl.addWidget(self._section("Asset Editors"))
        ed_grid = QGridLayout()
        ed_grid.setSpacing(10)
        ed_cards = [
            ("🧱", "COL Workshop",
             "View and edit GTA collision models — spheres, boxes and mesh. "
             "Loads directly from an open IMG or standalone .col file.",
             "#b45309", self.open_col_workshop),
            ("🖼", "TXD Workshop",
             "Browse, preview and replace textures inside .txd archives. "
             "Supports PC, PS2, Xbox and mobile formats.",
             "#0e7490", self.open_txd_workshop),
            ("🗿", "Model Workshop (DFF)",
             "Inspect DFF geometry, frame hierarchy and linked TXD textures. "
             "Double-click any model row in the DAT Browser to open it here with textures.",
             "#7c3aed", self.open_model_workshop),
        ]
        for i, (ico, ttl, dsc, acc, sig) in enumerate(ed_cards):
            card = WelcomeCard(ico, ttl, dsc, acc)
            card.clicked.connect(sig.emit)
            ed_grid.addWidget(card, 0, i)
        cl.addLayout(ed_grid)

        # ── Workflow guide ───────────────────────────────────────────────
        cl.addWidget(self._section("Typical Workflows"))
        wf_grid = QGridLayout()
        wf_grid.setSpacing(10)

        wf_cards = [
            ("🔁", "Edit textures in a vehicle IMG",
             "Open IMG → select a .txd entry → right-click → Edit Textures. "
             "TXD Workshop opens, you replace images, save, and rebuild the IMG.",
             "#be185d"),
            ("🏙", "Explore a full game world",
             "DAT Browser → Load game → all 13 000+ objects listed. "
             "Click an IMG in the tree to open it. Double-click any model row "
             "to view its DFF + TXD in Model Workshop.",
             "#0e7490"),
            ("🧩", "Extract and convert COL data",
             "Open IMG → select a .col entry → COL Workshop opens. "
             "Inspect collision meshes, export as OBJ or edit surfaces "
             "and re-import into the archive.",
             "#b45309"),
        ]
        for i, (ico, ttl, dsc, acc) in enumerate(wf_cards):
            card = WelcomeCard(ico, ttl, dsc, acc)
            wf_grid.addWidget(card, 0, i)
        cl.addLayout(wf_grid)

        # ── Tip bar ──────────────────────────────────────────────────────
        tip = QLabel(
            "💡  Tip: Right-click any IMG/CDIMAGE entry in the DAT Browser load-order tree "
            "to open it in IMG Factory or dump all its TXD files to a folder.")
        tip.setFont(QFont("Arial", 9))
        tip.setWordWrap(True)
        tip.setStyleSheet(
            "QLabel { background: palette(alternateBase); border: 1px solid palette(mid); "
            "border-radius: 5px; padding: 8px 12px; color: palette(windowText); }")
        cl.addWidget(tip)
        cl.addStretch()

    def _section(self, text: str) -> QLabel:
        lbl = QLabel(text)
        lbl.setFont(QFont("Arial", 12, QFont.Weight.Bold))
        lbl.setStyleSheet(
            "QLabel { color: palette(windowText); border-bottom: 2px solid palette(mid); "
            "padding-bottom: 4px; }")
        return lbl
