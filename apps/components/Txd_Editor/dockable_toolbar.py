"""
apps/components/Txd_Editor/dockable_toolbar.py
DockableToolbar — 3ds Max-style detachable/dockable icon toolbar

The toolbar starts docked (horizontal above preview or vertical beside it).
A |> grip handle at the start lets the user:
  - Click grip:  collapse/expand the toolbar
  - Drag grip:   detach into a floating window
  - Drop near edge of parent: snap/dock back (top/bottom/left/right)
  - Right-click: context menu (dock top/bottom/left/right, float, collapse)
"""

from PyQt6.QtWidgets import (
    QWidget, QFrame, QHBoxLayout, QVBoxLayout, QGridLayout,
    QPushButton, QSizePolicy, QApplication, QLabel, QMenu,
    QRubberBand,
)
from PyQt6.QtCore import (
    Qt, QPoint, QRect, QSize, QMimeData, QTimer, pyqtSignal, QObject,
)
from PyQt6.QtGui import QPainter, QColor, QPen, QCursor, QIcon, QPixmap

# ── Snap zones ────────────────────────────────────────────────────────────────
SNAP_NONE   = ''
SNAP_TOP    = 'top'
SNAP_BOTTOM = 'bottom'
SNAP_LEFT   = 'left'
SNAP_RIGHT  = 'right'

SNAP_THRESHOLD = 40   # px from edge to trigger snap highlight


class _GripHandle(QPushButton):
    """The |> / <| drag handle button at the start of the toolbar.

    - Click: collapse / expand
    - Press + drag: initiate toolbar detach
    """
    drag_started = pyqtSignal(QPoint)   # global pos where drag began

    def __init__(self, parent=None):
        super().__init__(parent)
        self._collapsed  = False
        self._pressing   = False
        self._press_pos  = QPoint()
        self._drag_threshold = 6
        self.setFixedSize(14, 24)
        self.setCursor(Qt.CursorShape.SizeAllCursor)
        self.setToolTip("Drag to float  |  Click to collapse  |  Right-click for dock options")
        self.setStyleSheet("""
            QPushButton {
                background: transparent;
                border: none;
                padding: 0;
            }
            QPushButton:hover {
                background: rgba(255,255,255,0.12);
                border-radius: 2px;
            }
        """)

    def set_orientation(self, vertical: bool, collapsed: bool):
        self._collapsed = collapsed
        if vertical:
            self.setFixedSize(24, 14)
        else:
            self.setFixedSize(14, 24)
        self.update()

    def paintEvent(self, event):
        super().paintEvent(event)
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        w, h = self.width(), self.height()
        c = QColor(180, 180, 180)
        p.setPen(QPen(c, 1.5))

        # Draw two vertical (or horizontal) dotted lines — classic grip
        is_wide = w > h   # horizontal orientation → tall grip; vertical → wide grip
        if not is_wide:
            # Vertical grip lines
            for x in (w//2 - 2, w//2 + 2):
                for y in range(3, h - 3, 4):
                    p.drawPoint(x, y)
        else:
            # Horizontal grip lines
            for y in (h//2 - 2, h//2 + 2):
                for x in range(3, w - 3, 4):
                    p.drawPoint(x, y)

    def mousePressEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton:
            self._pressing = True
            self._press_pos = event.globalPosition().toPoint()
        super().mousePressEvent(event)

    def mouseMoveEvent(self, event):
        if self._pressing:
            delta = event.globalPosition().toPoint() - self._press_pos
            if delta.manhattanLength() > self._drag_threshold:
                self._pressing = False
                self.drag_started.emit(self._press_pos)
                return
        super().mouseMoveEvent(event)

    def mouseReleaseEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton and self._pressing:
            self._pressing = False
            # Short click = collapse/expand (handled by toolbar)
        super().mouseReleaseEvent(event)


class _FloatingWindow(QWidget):
    """Floating toolbar window — shown when detached, hidden when docked."""

    closed = pyqtSignal()

    def __init__(self, toolbar: 'DockableToolbar'):
        super().__init__(None,
            Qt.WindowType.Tool |
            Qt.WindowType.FramelessWindowHint |
            Qt.WindowType.WindowStaysOnTopHint)
        self._toolbar = toolbar
        self._drag_offset = QPoint()
        self._dragging = False

        layout = QVBoxLayout(self)
        layout.setContentsMargins(2, 2, 2, 2)
        layout.setSpacing(0)

        # Tiny title bar for dragging
        self._title = QLabel("⠿ Toolbar")
        self._title.setFixedHeight(16)
        self._title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._title.setStyleSheet(
            "background:#333; color:#ccc; font-size:9px; border-bottom:1px solid #555;")
        layout.addWidget(self._title)
        layout.addWidget(toolbar)

        self.setStyleSheet("background:#1e1e24; border:1px solid #555;")

    def mousePressEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton:
            self._dragging = True
            self._drag_offset = event.globalPosition().toPoint() - self.pos()

    def mouseMoveEvent(self, event):
        if self._dragging:
            self.move(event.globalPosition().toPoint() - self._drag_offset)
            self._toolbar._check_snap_highlight(
                event.globalPosition().toPoint())

    def mouseReleaseEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton:
            self._dragging = False
            self._toolbar._try_snap(event.globalPosition().toPoint())

    def closeEvent(self, event):
        self.closed.emit()
        event.accept()


class DockableToolbar(QWidget):
    """
    A toolbar that can be:
      - Docked top/bottom/left/right inside a parent container
      - Detached into a floating window by dragging the grip handle
      - Collapsed to just the grip handle by clicking it
      - Re-docked by dragging the float window near a parent edge

    Usage:
        toolbar = DockableToolbar(parent_panel)
        toolbar.set_grid_widget(my_qframe_with_grid_layout)
        parent_layout.addWidget(toolbar)
        toolbar.set_dock_position('top')
    """

    dock_position_changed = pyqtSignal(str)   # 'top','bottom','left','right','float'

    def __init__(self, parent_panel: QWidget, parent=None):
        super().__init__(parent)
        self._parent_panel  = parent_panel   # the panel we dock inside
        self._dock_pos      = SNAP_TOP       # current dock position
        self._collapsed     = False
        self._floating      = False
        self._grid_widget   = None           # the actual icon grid frame
        self._float_win     = None
        self._snap_highlight = SNAP_NONE

        self._outer = QHBoxLayout(self)
        self._outer.setContentsMargins(0, 0, 0, 0)
        self._outer.setSpacing(2)

        self._grip = _GripHandle(self)
        self._grip.clicked.connect(self._toggle_collapse)
        self._grip.drag_started.connect(self._begin_float)
        self._grip.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self._grip.customContextMenuRequested.connect(
            lambda p: self._show_context_menu(self._grip.mapToGlobal(p)))
        self._outer.addWidget(self._grip)

    # ── Public API ─────────────────────────────────────────────────────────────

    def set_grid_widget(self, widget: QWidget):
        """Set the icon grid widget displayed beside the grip."""
        if self._grid_widget:
            self._outer.removeWidget(self._grid_widget)
        self._grid_widget = widget
        self._outer.addWidget(widget, stretch=1)
        widget.setVisible(not self._collapsed)

    def set_dock_position(self, pos: str):
        """pos: 'top' | 'bottom' | 'left' | 'right'"""
        self._dock_pos = pos
        vertical = pos in (SNAP_LEFT, SNAP_RIGHT)
        self._grip.set_orientation(vertical, self._collapsed)
        # Flip layout direction
        if vertical:
            self._outer.setDirection(QHBoxLayout.Direction.TopToBottom)
        else:
            self._outer.setDirection(QHBoxLayout.Direction.LeftToRight)

    def is_floating(self) -> bool:
        return self._floating

    # ── Collapse / expand ──────────────────────────────────────────────────────

    def _toggle_collapse(self):
        self._collapsed = not self._collapsed
        if self._grid_widget:
            self._grid_widget.setVisible(not self._collapsed)
        self._grip.set_orientation(
            self._dock_pos in (SNAP_LEFT, SNAP_RIGHT), self._collapsed)

    # ── Float / dock ───────────────────────────────────────────────────────────

    def _begin_float(self, global_press_pos: QPoint):
        """Detach toolbar into floating window."""
        if self._floating:
            return
        self._floating = True

        # Create float window
        self._float_win = _FloatingWindow(self)
        self._float_win.closed.connect(self._on_float_closed)

        # Position float window at current toolbar global pos
        gp = self.mapToGlobal(QPoint(0, 0))
        self._float_win.move(gp)
        self._float_win.show()
        self.dock_position_changed.emit('float')

    def _on_float_closed(self):
        self._dock_toolbar(SNAP_TOP)

    def _check_snap_highlight(self, global_cursor: QPoint):
        """Highlight a snap zone while dragging near the parent panel edge."""
        zone = self._get_snap_zone(global_cursor)
        self._snap_highlight = zone
        # TODO: draw rubber-band overlay on parent_panel for the target zone

    def _try_snap(self, global_cursor: QPoint):
        """If cursor is near a parent edge, dock there; otherwise stay floating."""
        zone = self._get_snap_zone(global_cursor)
        if zone:
            self._dock_toolbar(zone)

    def _get_snap_zone(self, global_cursor: QPoint) -> str:
        """Return snap zone ('top','bottom','left','right') or '' if none."""
        pp = self._parent_panel
        if not pp:
            return SNAP_NONE
        tl = pp.mapToGlobal(QPoint(0, 0))
        rect = QRect(tl, pp.size())
        T = SNAP_THRESHOLD

        if not rect.adjusted(-T, -T, T, T).contains(global_cursor):
            return SNAP_NONE

        lc = global_cursor - tl   # local cursor
        w, h = pp.width(), pp.height()

        # Distance to each edge
        dist_top    = lc.y()
        dist_bottom = h - lc.y()
        dist_left   = lc.x()
        dist_right  = w - lc.x()

        nearest = min(dist_top, dist_bottom, dist_left, dist_right)
        if nearest > T:
            return SNAP_NONE
        if nearest == dist_top:    return SNAP_TOP
        if nearest == dist_bottom: return SNAP_BOTTOM
        if nearest == dist_left:   return SNAP_LEFT
        return SNAP_RIGHT

    def _dock_toolbar(self, pos: str):
        """Remove from float window, re-insert into parent panel at pos."""
        self._floating = False
        self._dock_pos = pos

        # Remove from float window's layout
        if self._float_win:
            self._float_win.layout().removeWidget(self)
            self._float_win.hide()
            self._float_win.deleteLater()
            self._float_win = None

        # Re-insert into parent panel
        pp  = self._parent_panel
        plo = pp.layout() if pp else None
        if plo is None:
            return

        # Remove if already in layout
        plo.removeWidget(self)

        if pos == SNAP_TOP:
            plo.insertWidget(0, self, stretch=0)
        elif pos == SNAP_BOTTOM:
            plo.addWidget(self, stretch=0)
        elif pos in (SNAP_LEFT, SNAP_RIGHT):
            # Need to find or create the preview row and insert beside it
            self._dock_beside_preview(pos)
            self.set_dock_position(pos)
            self.dock_position_changed.emit(pos)
            return

        self.set_dock_position(pos)
        self.show()
        self.dock_position_changed.emit(pos)

    def _dock_beside_preview(self, side: str):
        """Dock left or right by inserting into the preview HBoxLayout."""
        pp  = self._parent_panel
        plo = pp.layout()
        if plo is None:
            return

        # Find the preview row (HBoxLayout containing preview_widget)
        preview_row = None
        for i in range(plo.count()):
            item = plo.itemAt(i)
            if item and item.layout():
                lo = item.layout()
                # Check if any child widget is a ZoomablePreview
                for j in range(lo.count()):
                    sub = lo.itemAt(j)
                    if sub and sub.widget() and 'Preview' in type(sub.widget()).__name__:
                        preview_row = lo
                        break
            if preview_row:
                break

        if preview_row is None:
            # Fallback: just dock at top
            plo.insertWidget(0, self, stretch=0)
            return

        preview_row.removeWidget(self)
        if side == SNAP_LEFT:
            preview_row.insertWidget(0, self, stretch=0)
        else:
            preview_row.addWidget(self, stretch=0)
        self.show()

    # ── Context menu ───────────────────────────────────────────────────────────

    def _show_context_menu(self, global_pos: QPoint):
        menu = QMenu(self)
        is_vert = self._dock_pos in (SNAP_LEFT, SNAP_RIGHT)

        dock_menu = menu.addMenu("Dock to…")
        dock_menu.addAction("Top",    lambda: self._dock_toolbar(SNAP_TOP))
        dock_menu.addAction("Bottom", lambda: self._dock_toolbar(SNAP_BOTTOM))
        dock_menu.addAction("Left",   lambda: self._dock_toolbar(SNAP_LEFT))
        dock_menu.addAction("Right",  lambda: self._dock_toolbar(SNAP_RIGHT))

        menu.addSeparator()
        if self._floating:
            menu.addAction("Dock (return to Top)", lambda: self._dock_toolbar(SNAP_TOP))
        else:
            menu.addAction("Float (detach)", lambda: self._begin_float(global_pos))

        menu.addSeparator()
        col_text = "Expand toolbar" if self._collapsed else "Collapse toolbar"
        menu.addAction(col_text, self._toggle_collapse)

        menu.exec(global_pos)
