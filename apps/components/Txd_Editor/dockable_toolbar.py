"""
apps/components/Txd_Editor/dockable_toolbar.py  — Build 2
DockableToolbar: 3ds Max-style detachable/dockable toolbar.

Grip handle (dotted lines) at the start:
  Click  → collapse / expand content
  Drag   → detach into floating window, follow the mouse
  Drop   → near parent edge: snap & re-dock; elsewhere: stay floating
  Right-click → context menu (Dock to…, Float, Collapse)
"""

from PyQt6.QtWidgets import (
    QWidget, QFrame, QHBoxLayout, QVBoxLayout,
    QPushButton, QLabel, QMenu, QApplication,
)
from PyQt6.QtCore import Qt, QPoint, QRect, QSize, pyqtSignal, QObject, QEvent
from PyQt6.QtGui import QPainter, QColor, QPen, QCursor

SNAP_NONE   = ''
SNAP_TOP    = 'top'
SNAP_BOTTOM = 'bottom'
SNAP_LEFT   = 'left'
SNAP_RIGHT  = 'right'
SNAP_THRESHOLD = 60   # px from parent edge to trigger snap


# ─────────────────────────────────────────────────────────────────────────────
class _GripHandle(QWidget):
    """Dotted-line grip — click = collapse, drag = float."""
    drag_started  = pyqtSignal(QPoint)   # global pos of press
    click_release = pyqtSignal()         # short click (no drag)

    def __init__(self, parent=None):
        super().__init__(parent)
        self._pressing       = False
        self._press_global   = QPoint()
        self._drag_threshold = 5
        self._vertical       = False     # True when toolbar is docked left/right
        self._set_size()
        self.setCursor(Qt.CursorShape.SizeAllCursor)
        self.setToolTip(
            "Drag → detach  |  Click → collapse  |  Right-click → dock options")
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)

    def _set_size(self):
        if self._vertical:
            self.setFixedSize(24, 12)
        else:
            self.setFixedSize(12, 24)

    def set_vertical(self, v: bool):
        self._vertical = v
        self._set_size()
        self.update()

    def paintEvent(self, _):
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        c = QColor(160, 160, 160)
        p.setPen(QPen(c, 1.5))
        w, h = self.width(), self.height()
        if not self._vertical:            # horizontal toolbar → tall grip
            for x in (w // 2 - 2, w // 2 + 2):
                for y in range(3, h - 3, 4):
                    p.drawPoint(x, y)
        else:                             # vertical toolbar → wide grip
            for y in (h // 2 - 2, h // 2 + 2):
                for x in range(3, w - 3, 4):
                    p.drawPoint(x, y)

    def mousePressEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton:
            self._pressing     = True
            self._press_global = e.globalPosition().toPoint()

    def mouseMoveEvent(self, e):
        if self._pressing:
            delta = e.globalPosition().toPoint() - self._press_global
            if delta.manhattanLength() > self._drag_threshold:
                self._pressing = False
                self.drag_started.emit(self._press_global)

    def mouseReleaseEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton and self._pressing:
            self._pressing = False
            self.click_release.emit()


# ─────────────────────────────────────────────────────────────────────────────
class _FloatWindow(QWidget):
    """Frameless stay-on-top window that follows the mouse while dragging."""

    redock = pyqtSignal(str)   # zone: 'top'|'bottom'|'left'|'right'|''

    def __init__(self, content: QWidget, parent_panel: QWidget,
                 initial_global: QPoint):
        super().__init__(None,
            Qt.WindowType.Tool |
            Qt.WindowType.FramelessWindowHint |
            Qt.WindowType.WindowStaysOnTopHint)
        self.setAttribute(Qt.WidgetAttribute.WA_ShowWithoutActivating)
        self._panel   = parent_panel
        self._content = content
        self._dragging     = False
        self._drag_offset  = QPoint()
        self._snap_zone    = SNAP_NONE
        self._snap_overlay = None   # future: rubber band

        lo = QVBoxLayout(self)
        lo.setContentsMargins(1, 1, 1, 1)
        lo.setSpacing(0)

        # Title bar
        self._title = QLabel("⠿  Toolbar  — drop near edge to dock")
        self._title.setFixedHeight(18)
        self._title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._title.setStyleSheet(
            "background:#2a2a3a; color:#aaa; font-size:8px;"
            "border-bottom:1px solid #555; padding:0 4px;")
        lo.addWidget(self._title)
        lo.addWidget(content)
        self.setStyleSheet("background:#1e1e24; border:1px solid #666;")
        self.adjustSize()

        # Position: offset from where the drag started
        self.move(initial_global - QPoint(10, 10))
        self.show()

        # Grab mouse immediately so we get all move events
        self.grabMouse()
        self._dragging    = True
        self._drag_offset = initial_global - self.pos()

    # Mouse events — window follows cursor globally
    def mouseMoveEvent(self, e):
        if self._dragging:
            new_pos = e.globalPosition().toPoint() - self._drag_offset
            self.move(new_pos)
            self._update_snap_zone(e.globalPosition().toPoint())

    def mouseReleaseEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton and self._dragging:
            self._dragging = False
            self.releaseMouse()
            zone = self._snap_zone
            self.redock.emit(zone)   # '' = stay floating

    def _update_snap_zone(self, global_pos: QPoint):
        pp = self._panel
        if not pp or not pp.isVisible():
            self._snap_zone = SNAP_NONE
            self._title.setStyleSheet(
                "background:#2a2a3a; color:#aaa; font-size:8px;"
                "border-bottom:1px solid #555;")
            return
        tl = pp.mapToGlobal(QPoint(0, 0))
        rect = QRect(tl, pp.size())
        T = SNAP_THRESHOLD

        if not rect.adjusted(-T, -T, T, T).contains(global_pos):
            self._snap_zone = SNAP_NONE
            self._title.setStyleSheet(
                "background:#2a2a3a; color:#aaa; font-size:8px;"
                "border-bottom:1px solid #555;")
            return

        lc = global_pos - tl
        w, h = pp.width(), pp.height()
        dists = {
            SNAP_TOP:    lc.y(),
            SNAP_BOTTOM: h - lc.y(),
            SNAP_LEFT:   lc.x(),
            SNAP_RIGHT:  w - lc.x(),
        }
        zone, dist = min(dists.items(), key=lambda x: x[1])
        if dist > T:
            self._snap_zone = SNAP_NONE
            self._title.setStyleSheet(
                "background:#2a2a3a; color:#aaa; font-size:8px;"
                "border-bottom:1px solid #555;")
        else:
            self._snap_zone = zone
            # Highlight title bar to indicate snap zone
            self._title.setStyleSheet(
                f"background:#1a5a8a; color:#fff; font-size:8px;"
                f"border-bottom:1px solid #4af; padding:0 4px;")
            self._title.setText(f"⠿  Drop to dock → {zone.upper()}")

    def closeEvent(self, e):
        self.releaseMouse()
        super().closeEvent(e)


# ─────────────────────────────────────────────────────────────────────────────
class DockableToolbar(QWidget):
    """
    Toolbar wrapper with a grip handle at the leading edge.
    Manages docking, floating, and collapsing.
    """

    dock_position_changed = pyqtSignal(str)

    def __init__(self, parent_panel: QWidget, parent=None):
        super().__init__(parent)
        self._panel     = parent_panel
        self._dock_pos  = SNAP_TOP
        self._collapsed = False
        self._floating  = False
        self._float_win = None
        self._content   = None   # the icon grid frame

        self._layout = QHBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.setSpacing(2)

        self._grip = _GripHandle(self)
        self._grip.drag_started.connect(self._on_drag_started)
        self._grip.click_release.connect(self._toggle_collapse)
        self._grip.customContextMenuRequested.connect(
            lambda p: self._show_menu(self._grip.mapToGlobal(p)))
        self._layout.addWidget(self._grip)

    # ── Public API ────────────────────────────────────────────────────────────

    def set_content(self, widget: QWidget):
        if self._content:
            self._layout.removeWidget(self._content)
        self._content = widget
        self._layout.addWidget(widget, stretch=1)
        widget.setVisible(not self._collapsed)

    def set_dock_position(self, pos: str):
        self._dock_pos = pos
        vert = pos in (SNAP_LEFT, SNAP_RIGHT)
        self._grip.set_vertical(vert)
        # Flip layout axis
        if vert:
            self._layout.setDirection(QHBoxLayout.Direction.TopToBottom)
        else:
            self._layout.setDirection(QHBoxLayout.Direction.LeftToRight)

    # ── Collapse ─────────────────────────────────────────────────────────────

    def _toggle_collapse(self):
        self._collapsed = not self._collapsed
        if self._content:
            self._content.setVisible(not self._collapsed)

    # ── Float / drag ─────────────────────────────────────────────────────────

    def _on_drag_started(self, global_press: QPoint):
        """Grip was dragged — detach toolbar into a floating window."""
        if self._floating:
            # Already floating: start dragging the existing window
            if self._float_win:
                self._float_win._dragging   = True
                self._float_win._drag_offset = global_press - self._float_win.pos()
                self._float_win.grabMouse()
            return

        self._floating = True

        # Remove from current layout
        self._remove_from_dock()

        # Create float window with self as content
        self._float_win = _FloatWindow(self, self._panel, global_press)
        self._float_win.redock.connect(self._on_redock)
        self.dock_position_changed.emit('float')

    def _on_redock(self, zone: str):
        """Float window released — dock if zone given, else stay floating."""
        if self._float_win:
            # Take content back out of float window before closing
            self._float_win.layout().removeWidget(self)
            self._float_win.hide()
            self._float_win.deleteLater()
            self._float_win = None

        if not zone:
            # Stay floating — re-create a non-dragging float window
            self._floating = True
            win = QWidget(None,
                Qt.WindowType.Tool |
                Qt.WindowType.FramelessWindowHint |
                Qt.WindowType.WindowStaysOnTopHint)
            lo = QVBoxLayout(win)
            lo.setContentsMargins(1, 1, 1, 1)
            lo.setSpacing(0)
            title = QLabel("⠿  Toolbar  (drag to move, right-click grip to dock)")
            title.setFixedHeight(18)
            title.setAlignment(Qt.AlignmentFlag.AlignCenter)
            title.setStyleSheet(
                "background:#2a2a3a; color:#aaa; font-size:8px; border-bottom:1px solid #555;")
            lo.addWidget(title)
            lo.addWidget(self)
            win.setStyleSheet("background:#1e1e24; border:1px solid #666;")
            win.adjustSize()
            win.show()
            self._float_win = win
            self.show()
        else:
            self._floating = False
            self._dock_to(zone)

    def _remove_from_dock(self):
        """Remove self from whichever layout currently contains us."""
        pp = self._panel
        if not pp:
            return
        plo = pp.layout()
        if plo:
            plo.removeWidget(self)
        # Also check preview row
        self._remove_from_preview_row()

    def _remove_from_preview_row(self):
        pp = self._panel
        if not pp:
            return
        plo = pp.layout()
        if not plo:
            return
        for i in range(plo.count()):
            item = plo.itemAt(i)
            if item and item.layout():
                item.layout().removeWidget(self)

    def _dock_to(self, pos: str):
        self._dock_pos = pos
        pp  = self._panel
        plo = pp.layout() if pp else None
        if plo is None:
            return

        if pos == SNAP_TOP:
            plo.insertWidget(0, self, stretch=0)
        elif pos == SNAP_BOTTOM:
            plo.addWidget(self, stretch=0)
        elif pos in (SNAP_LEFT, SNAP_RIGHT):
            self._dock_beside_preview(pos)
            self.set_dock_position(pos)
            self.show()
            self.dock_position_changed.emit(pos)
            return

        self.set_dock_position(pos)
        self.show()
        self.dock_position_changed.emit(pos)

    def _dock_beside_preview(self, side: str):
        pp  = self._panel
        plo = pp.layout() if pp else None
        if not plo:
            return
        # Find the HBoxLayout that contains a ZoomablePreview
        for i in range(plo.count()):
            item = plo.itemAt(i)
            if item and item.layout():
                lo = item.layout()
                for j in range(lo.count()):
                    sub = lo.itemAt(j)
                    if sub and sub.widget() and 'Preview' in type(sub.widget()).__name__:
                        lo.removeWidget(self)
                        if side == SNAP_LEFT:
                            lo.insertWidget(0, self, stretch=0)
                        else:
                            lo.addWidget(self, stretch=0)
                        return
        # Fallback
        plo.insertWidget(0, self, stretch=0)

    # ── Context menu ─────────────────────────────────────────────────────────

    def _show_menu(self, global_pos: QPoint):
        menu = QMenu(self)
        sub = menu.addMenu("Dock to…")
        sub.addAction("Top",    lambda: self._on_redock(SNAP_TOP))
        sub.addAction("Bottom", lambda: self._on_redock(SNAP_BOTTOM))
        sub.addAction("Left",   lambda: self._on_redock(SNAP_LEFT))
        sub.addAction("Right",  lambda: self._on_redock(SNAP_RIGHT))
        menu.addSeparator()
        if self._floating:
            menu.addAction("Dock to Top", lambda: self._on_redock(SNAP_TOP))
        else:
            menu.addAction("Float (detach)",
                lambda: self._on_drag_started(global_pos))
        menu.addSeparator()
        menu.addAction(
            "Expand" if self._collapsed else "Collapse",
            self._toggle_collapse)
        menu.exec(global_pos)
