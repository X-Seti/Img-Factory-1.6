"""
apps/components/Txd_Editor/dockable_toolbar.py  — Build 3
DockableToolbar: 3ds Max-style detachable/dockable toolbar.

Grip handle (dotted lines) at the start:
  Click  → collapse / expand content
  Drag   → detach into floating window, follow the mouse
  Drop   → near parent edge: highlight that edge, snap & re-dock on release
  Right-click → context menu (Dock to…, Float, Collapse)
"""

from PyQt6.QtWidgets import (
    QWidget, QHBoxLayout, QVBoxLayout,
    QPushButton, QLabel, QMenu, QApplication,
)
from PyQt6.QtCore import Qt, QPoint, QRect, QSize, pyqtSignal, QTimer
from PyQt6.QtGui import QPainter, QColor, QPen, QBrush

SNAP_NONE   = ''
SNAP_TOP    = 'top'
SNAP_BOTTOM = 'bottom'
SNAP_LEFT   = 'left'
SNAP_RIGHT  = 'right'
SNAP_THRESHOLD = 60   # px from parent edge to trigger snap
EDGE_THICKNESS = 6    # px thickness of the edge highlight band


# ─────────────────────────────────────────────────────────────────────────────
class _EdgeOverlay(QWidget):
    """Transparent overlay drawn over the parent panel that highlights
    whichever edge the floating toolbar will snap to.

    Painted as a semi-transparent coloured band along the target edge.
    """

    def __init__(self, parent_panel: QWidget):
        super().__init__(parent_panel)
        self._zone  = SNAP_NONE
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents)
        self.setAttribute(Qt.WidgetAttribute.WA_NoSystemBackground)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)
        self.setWindowFlags(Qt.WindowType.SubWindow)
        self.hide()

    def set_zone(self, zone: str):
        if zone == self._zone:
            return
        self._zone = zone
        if zone:
            self.setGeometry(self.parent().rect())
            self.raise_()
            self.show()
        else:
            self.hide()
        self.update()

    def paintEvent(self, _):
        if not self._zone:
            return
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        w, h = self.width(), self.height()
        T = EDGE_THICKNESS

        # Coloured band on the snap edge
        accent = QColor(30, 144, 255, 160)   # dodger blue, semi-transparent
        outline = QColor(30, 144, 255, 220)

        if self._zone == SNAP_TOP:
            band = QRect(0, 0, w, T * 4)
        elif self._zone == SNAP_BOTTOM:
            band = QRect(0, h - T * 4, w, T * 4)
        elif self._zone == SNAP_LEFT:
            band = QRect(0, 0, T * 4, h)
        else:  # RIGHT
            band = QRect(w - T * 4, 0, T * 4, h)

        p.fillRect(band, accent)
        p.setPen(QPen(outline, 2))
        p.drawRect(band.adjusted(1, 1, -1, -1))

        # Arrow / label hint
        p.setPen(QPen(QColor(255, 255, 255, 200), 1))
        p.setFont(self.font())
        labels = {
            SNAP_TOP:    '▼  Dock Top',
            SNAP_BOTTOM: '▲  Dock Bottom',
            SNAP_LEFT:   '▶  Dock Left',
            SNAP_RIGHT:  '◀  Dock Right',
        }
        p.drawText(band, Qt.AlignmentFlag.AlignCenter, labels[self._zone])


# ─────────────────────────────────────────────────────────────────────────────
class _GripHandle(QWidget):
    """Dotted-line grip — click = collapse, drag = float."""
    drag_started  = pyqtSignal(QPoint)   # global pos of press
    click_release = pyqtSignal()

    def __init__(self, parent=None):
        super().__init__(parent)
        self._pressing       = False
        self._press_global   = QPoint()
        self._drag_threshold = 5
        self._vertical       = False
        self._set_size()
        self.setCursor(Qt.CursorShape.SizeAllCursor)
        self.setToolTip(
            "Drag → detach  |  Click → collapse  |  Right-click → dock options")
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)

    def _set_size(self):
        self.setFixedSize(24, 12) if self._vertical else self.setFixedSize(12, 24)

    def set_vertical(self, v: bool):
        self._vertical = v
        self._set_size()
        self.update()

    def paintEvent(self, _):
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        p.setPen(QPen(QColor(160, 160, 160), 1.5))
        w, h = self.width(), self.height()
        if not self._vertical:
            for x in (w // 2 - 2, w // 2 + 2):
                for y in range(3, h - 3, 4):
                    p.drawPoint(x, y)
        else:
            for y in (h // 2 - 2, h // 2 + 2):
                for x in range(3, w - 3, 4):
                    p.drawPoint(x, y)

    def mousePressEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton:
            self._pressing     = True
            self._press_global = e.globalPosition().toPoint()

    def mouseMoveEvent(self, e):
        if self._pressing:
            if (e.globalPosition().toPoint() - self._press_global).manhattanLength() > self._drag_threshold:
                self._pressing = False
                self.drag_started.emit(self._press_global)

    def mouseReleaseEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton and self._pressing:
            self._pressing = False
            self.click_release.emit()


# ─────────────────────────────────────────────────────────────────────────────
class _FloatWindow(QWidget):
    """Frameless stay-on-top window that follows the mouse while dragging.
    Shows an edge-highlight overlay on the parent panel when near a snap zone.
    """

    redock = pyqtSignal(str)   # zone or '' to stay floating

    def __init__(self, content: QWidget, parent_panel: QWidget,
                 overlay: '_EdgeOverlay', initial_global: QPoint):
        super().__init__(None,
            Qt.WindowType.Tool |
            Qt.WindowType.FramelessWindowHint |
            Qt.WindowType.WindowStaysOnTopHint)
        self.setAttribute(Qt.WidgetAttribute.WA_ShowWithoutActivating)
        self._panel        = parent_panel
        self._overlay      = overlay
        self._dragging     = False
        self._drag_offset  = QPoint()
        self._snap_zone    = SNAP_NONE

        lo = QVBoxLayout(self)
        lo.setContentsMargins(1, 1, 1, 1)
        lo.setSpacing(0)

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
        self.move(initial_global - QPoint(10, 10))
        self.show()

        # Grab mouse so all move events reach us regardless of child widgets
        self.grabMouse()
        self._dragging   = True
        self._drag_offset = initial_global - self.pos()

    def mouseMoveEvent(self, e):
        if self._dragging:
            self.move(e.globalPosition().toPoint() - self._drag_offset)
            self._update_snap(e.globalPosition().toPoint())

    def mouseReleaseEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton and self._dragging:
            self._dragging = False
            self.releaseMouse()
            self._overlay.set_zone(SNAP_NONE)   # clear highlight
            zone = self._snap_zone
            self._snap_zone = SNAP_NONE
            self.redock.emit(zone)

    def _update_snap(self, global_pos: QPoint):
        zone = self._calc_zone(global_pos)
        if zone != self._snap_zone:
            self._snap_zone = zone
            self._overlay.set_zone(zone)   # show/hide edge highlight
            # Update title bar colour
            if zone:
                self._title.setStyleSheet(
                    "background:#1a5a8a; color:#fff; font-size:8px;"
                    "border-bottom:1px solid #4af; padding:0 4px;")
                names = {SNAP_TOP:'TOP', SNAP_BOTTOM:'BOTTOM',
                         SNAP_LEFT:'LEFT', SNAP_RIGHT:'RIGHT'}
                self._title.setText(f"⠿  Release to dock → {names[zone]}")
            else:
                self._title.setStyleSheet(
                    "background:#2a2a3a; color:#aaa; font-size:8px;"
                    "border-bottom:1px solid #555; padding:0 4px;")
                self._title.setText("⠿  Toolbar  — drop near edge to dock")

    def _calc_zone(self, global_pos: QPoint) -> str:
        pp = self._panel
        if not pp or not pp.isVisible():
            return SNAP_NONE
        tl = pp.mapToGlobal(QPoint(0, 0))
        w, h = pp.width(), pp.height()
        T = SNAP_THRESHOLD
        if not QRect(tl, QSize(w, h)).adjusted(-T, -T, T, T).contains(global_pos):
            return SNAP_NONE
        lc = global_pos - tl
        dists = {
            SNAP_TOP:    lc.y(),
            SNAP_BOTTOM: h - lc.y(),
            SNAP_LEFT:   lc.x(),
            SNAP_RIGHT:  w - lc.x(),
        }
        zone, dist = min(dists.items(), key=lambda x: x[1])
        return zone if dist <= T else SNAP_NONE

    def closeEvent(self, e):
        self.releaseMouse()
        self._overlay.set_zone(SNAP_NONE)
        super().closeEvent(e)


# ─────────────────────────────────────────────────────────────────────────────
class DockableToolbar(QWidget):
    """Toolbar wrapper with grip handle. Manages docking, floating, collapsing."""

    dock_position_changed = pyqtSignal(str)

    def __init__(self, parent_panel: QWidget, parent=None):
        super().__init__(parent)
        self._panel     = parent_panel
        self._dock_pos  = SNAP_TOP
        self._collapsed = False
        self._floating  = False
        self._float_win = None
        self._content   = None
        self._overlay   = None   # created lazily when first float happens

        self._layout = QHBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.setSpacing(2)

        self._grip = _GripHandle(self)
        self._grip.drag_started.connect(self._on_drag_started)
        self._grip.click_release.connect(self._toggle_collapse)
        self._grip.customContextMenuRequested.connect(
            lambda p: self._show_menu(self._grip.mapToGlobal(p)))
        self._layout.addWidget(self._grip)

    # ── Public ────────────────────────────────────────────────────────────────

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

    def _get_overlay(self) -> '_EdgeOverlay':
        """Return (creating if needed) the edge-highlight overlay on parent panel."""
        if self._overlay is None or not self._overlay.parent():
            self._overlay = _EdgeOverlay(self._panel)
        return self._overlay

    def _on_drag_started(self, global_press: QPoint):
        if self._floating and self._float_win:
            # Re-engage drag on existing float window
            self._float_win._dragging    = True
            self._float_win._drag_offset = global_press - self._float_win.pos()
            self._float_win.grabMouse()
            return

        self._floating = True
        self._remove_from_dock()

        overlay = self._get_overlay()
        self._float_win = _FloatWindow(self, self._panel, overlay, global_press)
        self._float_win.redock.connect(self._on_redock)
        self.dock_position_changed.emit('float')

    def _on_redock(self, zone: str):
        # Extract self from float window before destroying it
        if self._float_win:
            self._float_win.layout().removeWidget(self)
            self._float_win.hide()
            self._float_win.deleteLater()
            self._float_win = None

        if not zone:
            # Stay floating — build a non-dragging static float window
            self._floating = True
            win = QWidget(None,
                Qt.WindowType.Tool |
                Qt.WindowType.FramelessWindowHint |
                Qt.WindowType.WindowStaysOnTopHint)
            win.setAttribute(Qt.WidgetAttribute.WA_DeleteOnClose)
            lo = QVBoxLayout(win)
            lo.setContentsMargins(1, 1, 1, 1)
            lo.setSpacing(0)
            title = QLabel("⠿  Toolbar  (right-click grip to dock)")
            title.setFixedHeight(18)
            title.setAlignment(Qt.AlignmentFlag.AlignCenter)
            title.setStyleSheet(
                "background:#2a2a3a; color:#aaa; font-size:8px;"
                "border-bottom:1px solid #555;")
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
        pp = self._panel
        if not pp:
            return
        plo = pp.layout()
        if plo:
            plo.removeWidget(self)
        # Also scan HBox sub-layouts (preview row)
        if plo:
            for i in range(plo.count()):
                item = plo.itemAt(i)
                if item and item.layout():
                    item.layout().removeWidget(self)

    def _dock_to(self, pos: str):
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
        for i in range(plo.count()):
            item = plo.itemAt(i)
            if item and item.layout():
                lo = item.layout()
                for j in range(lo.count()):
                    sub = lo.itemAt(j)
                    if sub and sub.widget() and 'Preview' in type(sub.widget()).__name__:
                        lo.removeWidget(self)
                        lo.insertWidget(0 if side == SNAP_LEFT else lo.count(), self, stretch=0)
                        return
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
