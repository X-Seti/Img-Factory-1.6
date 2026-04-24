#this belongs in apps/components/templates/three_panel_widget.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6
# Reusable three-panel splitter widget — extracted from COLWorkshop
"""
ThreePanelWidget — reusable base class for IMG Factory tools.

Layout (horizontal QSplitter):
  ┌            ┬                  ┬                 ┐
  │ Left Panel │  Centre Panel    │  Right Panel    │
  │ (list/nav) │  (main content)  │  (properties)   │
  └            ┴                  ┴                 ┘

In "embedded" mode (main_window provided): all three panels visible.
In "standalone" mode (no main_window): left panel hidden by default.

Subclass and override:
    _create_left_panel()    → QWidget | None
    _create_centre_panel()  → QWidget
    _create_right_panel()   → QWidget | None
    _create_toolbar()       → QWidget | None   (hidden when docked)
    _create_statusbar()     → QWidget | None   (hidden when docked)

Key rules:
    - The splitter is the ONLY child of the root layout and gets stretch=1
    - Each panel uses setSizePolicy(Expanding, Expanding) so Qt never clips it
    - QScrollArea inside a panel MUST have setWidgetResizable(True) and
      setSizePolicy(Expanding, Expanding) — otherwise the panel clips instead
      of growing
    - Never set a fixed/maximum width on right_panel; use minimumWidth only
"""

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QSplitter, QSizePolicy,
    QScrollArea, QFrame, QLabel
)
from PyQt6.QtCore import Qt, pyqtSignal
from PyQt6.QtGui import QFont


class ThreePanelWidget(QWidget):
    """Reusable three-panel splitter base for IMG Factory tools."""

    panel_ready = pyqtSignal()

    # Default left:centre:right stretch ratios
    PANEL_PROPORTIONS = (2, 3, 5)

    def __init__(self, parent=None, main_window=None,
                 title: str = "Tool",
                 left_min_width: int = 160,
                 left_max_width: int = 320,
                 right_min_width: int = 180):
        super().__init__(parent)

        self.main_window   = main_window
        self.standalone_mode = (main_window is None)
        self._title        = title
        self._left_min_w   = left_min_width
        self._left_max_w   = left_max_width
        self._right_min_w  = right_min_width

        self.title_font  = QFont("Arial", 12, QFont.Weight.Bold)
        self.panel_font  = QFont("Arial", 10)
        self.button_font = QFont("Arial", 10)

        self.app_settings = None
        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        else:
            try:
                from apps.utils.app_settings_system import AppSettings
                self.app_settings = AppSettings()
            except Exception:
                pass

        self._build_ui()
        self.panel_ready.emit()

    #    core layout                                                           

    def _build_ui(self): #vers 1
        """Assemble toolbar + splitter + statusbar."""
        # Root layout — NO margins on the outer box so panels reach the edges
        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(0)

        toolbar = self._create_toolbar()
        if toolbar is not None:
            if not self.standalone_mode:
                toolbar.setVisible(False)
            root.addWidget(toolbar)

        # THE SPLITTER gets stretch=1 — it owns all remaining space
        self._splitter = QSplitter(Qt.Orientation.Horizontal)
        self._splitter.setChildrenCollapsible(True)
        self._splitter.setSizePolicy(
            QSizePolicy.Policy.Expanding,
            QSizePolicy.Policy.Expanding,
        )
        root.addWidget(self._splitter, 1)   # ← stretch=1: fills everything

        left   = self._create_left_panel()
        centre = self._create_centre_panel()
        right  = self._create_right_panel()

        if left is not None:
            left.setMinimumWidth(self._left_min_w)
            left.setMaximumWidth(self._left_max_w)
            _expand(left)
            self._splitter.addWidget(left)

        _expand(centre)
        self._splitter.addWidget(centre)

        if right is not None:
            right.setMinimumWidth(self._right_min_w)
            _expand(right)
            self._splitter.addWidget(right)

        # Stretch factors
        count = self._splitter.count()
        if count == 3:
            l, c, r = self.PANEL_PROPORTIONS
            self._splitter.setStretchFactor(0, l)
            self._splitter.setStretchFactor(1, c)
            self._splitter.setStretchFactor(2, r)
        elif count == 2:
            self._splitter.setStretchFactor(0, 1)
            self._splitter.setStretchFactor(1, 2)
        else:
            self._splitter.setStretchFactor(0, 1)

        status = self._create_statusbar()
        if status is not None:
            if not self.standalone_mode:
                status.setVisible(False)
            root.addWidget(status)

    #    subclass API                                                           

    def _create_toolbar(self) -> 'QWidget | None': #vers 1
        """Override to return a toolbar widget."""
        return None

    def _create_left_panel(self) -> 'QWidget | None': #vers 1
        """Override to return the left navigation/list panel.
        Return None to hide it (e.g. standalone mode)."""
        return None

    def _create_centre_panel(self) -> QWidget: #vers 1
        """Override to return the main content panel."""
        placeholder = QFrame()
        placeholder.setFrameStyle(QFrame.Shape.StyledPanel)
        lbl = QLabel("Centre panel — override _create_centre_panel()")
        lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        from PyQt6.QtWidgets import QVBoxLayout as _V
        _V(placeholder).addWidget(lbl)
        return placeholder

    def _create_right_panel(self) -> 'QWidget | None': #vers 1
        """Override to return the right properties panel."""
        return None

    def _create_statusbar(self) -> 'QWidget | None': #vers 1
        """Override to return a status bar widget."""
        return None

    #    helpers                                                                

    @property
    def splitter(self) -> QSplitter:
        return self._splitter

    def get_theme_colors(self) -> dict:
        if self.app_settings:
            return self.app_settings.get_theme_colors() or {}
        return {}

    @staticmethod
    def make_scroll_area(widget: QWidget,
                         horizontal: bool = False) -> QScrollArea:
        """Wrap a widget in a properly-configured QScrollArea.

        The critical settings that prevent the scroll area from clipping:
          setWidgetResizable(True)  — widget grows with available space
          setSizePolicy(Expanding)  — scroll area itself expands
        """
        sa = QScrollArea()
        sa.setWidgetResizable(True)                    # ← MUST be True
        sa.setWidget(widget)
        if not horizontal:
            sa.setHorizontalScrollBarPolicy(
                Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        sa.setSizePolicy(
            QSizePolicy.Policy.Expanding,
            QSizePolicy.Policy.Expanding,
        )
        return sa


def _expand(widget: QWidget) -> None:
    """Set Expanding size policy on both axes — prevents clipping."""
    widget.setSizePolicy(
        QSizePolicy.Policy.Expanding,
        QSizePolicy.Policy.Expanding,
    )


__all__ = ['ThreePanelWidget', '_expand']
