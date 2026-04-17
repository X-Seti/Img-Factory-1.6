"""
apps/components/Txd_Editor/dockable_toolbar.py  — Build 4

Uses QTimer + QCursor.pos() for drag tracking instead of grabMouse(),
which fails silently on Wayland/KDE. Both the active _FloatWindow and
the static resting float window support drag-to-move.
"""

from PyQt6.QtWidgets import (
    QWidget, QHBoxLayout, QVBoxLayout, QLabel, QMenu,
)
from PyQt6.QtCore import Qt, QPoint, QRect, QSize, pyqtSignal, QTimer
from PyQt6.QtGui import QPainter, QColor, QPen, QCursor

SNAP_NONE   = ''
SNAP_TOP    = 'top'
SNAP_BOTTOM = 'bottom'
SNAP_LEFT   = 'left'
SNAP_RIGHT  = 'right'
SNAP_THRESHOLD = 60
EDGE_THICKNESS = 24   # band depth in px


# ── Edge overlay ─────────────────────────────────────────────────────────────
class _EdgeOverlay(QWidget):
    """Transparent overlay on the parent panel that highlights the snap edge."""

    def __init__(self, parent_panel: QWidget):
        super().__init__(parent_panel)
        self._zone = SNAP_NONE
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents)
        self.setAttribute(Qt.WidgetAttribute.WA_NoSystemBackground)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)
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
        w, h = self.width(), self.height()
        T = EDGE_THICKNESS
        accent  = QColor(30, 144, 255, 150)
        outline = QColor(30, 144, 255, 220)
        bands = {
            SNAP_TOP:    QRect(0,       0,       w,  T),
            SNAP_BOTTOM: QRect(0,       h - T,   w,  T),
            SNAP_LEFT:   QRect(0,       0,       T,  h),
            SNAP_RIGHT:  QRect(w - T,   0,       T,  h),
        }
        band = bands[self._zone]
        p.fillRect(band, accent)
        p.setPen(QPen(outline, 2))
        p.drawRect(band.adjusted(1, 1, -1, -1))
        p.setPen(QPen(QColor(255, 255, 255, 220), 1))
        labels = {SNAP_TOP:'▼ Dock Top', SNAP_BOTTOM:'▲ Dock Bottom',
                  SNAP_LEFT:'▶ Dock Left', SNAP_RIGHT:'◀ Dock Right'}
        p.drawText(band, Qt.AlignmentFlag.AlignCenter, labels[self._zone])


# ── Grip handle ──────────────────────────────────────────────────────────────
class _GripHandle(QWidget):
    drag_started  = pyqtSignal(QPoint)
    click_release = pyqtSignal()

    def __init__(self, parent=None):
        super().__init__(parent)
        self._pressing     = False
        self._press_global = QPoint()
        self._vertical     = False
        self._set_size()
        self.setCursor(Qt.CursorShape.SizeAllCursor)
        self.setToolTip("Drag → detach  |  Click → collapse  |  Right-click → dock options")
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)

    def _set_size(self):
        self.setFixedSize(24, 12) if self._vertical else self.setFixedSize(12, 24)

    def set_vertical(self, v: bool):
        self._vertical = v; self._set_size(); self.update()

    def paintEvent(self, _):
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        p.setPen(QPen(QColor(160, 160, 160), 1.5))
        w, h = self.width(), self.height()
        if not self._vertical:
            for x in (w//2-2, w//2+2):
                for y in range(3, h-3, 4): p.drawPoint(x, y)
        else:
            for y in (h//2-2, h//2+2):
                for x in range(3, w-3, 4): p.drawPoint(x, y)

    def mousePressEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton:
            self._pressing = True
            self._press_global = e.globalPosition().toPoint()

    def mouseMoveEvent(self, e):
        if self._pressing:
            if (e.globalPosition().toPoint() - self._press_global).manhattanLength() > 5:
                self._pressing = False
                self.drag_started.emit(self._press_global)

    def mouseReleaseEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton and self._pressing:
            self._pressing = False
            self.click_release.emit()


# ── Draggable float window ────────────────────────────────────────────────────
class _FloatWindow(QWidget):
    """Frameless stay-on-top window.

    Drag tracking uses QTimer + QCursor.pos() polling (works on Wayland/X11/XCB).
    Title bar press starts drag; release snaps or stays floating.
    """
    redock = pyqtSignal(str)   # snap zone or '' to stay floating

    def __init__(self, content: QWidget, parent_panel: QWidget,
                 overlay: _EdgeOverlay, initial_global: QPoint,
                 start_dragging: bool = True):
        super().__init__(None,
            Qt.WindowType.Tool |
            Qt.WindowType.FramelessWindowHint |
            Qt.WindowType.WindowStaysOnTopHint)
        self.setAttribute(Qt.WidgetAttribute.WA_ShowWithoutActivating)
        self._panel       = parent_panel
        self._overlay     = overlay
        self._dragging    = False
        self._drag_offset = QPoint()
        self._snap_zone   = SNAP_NONE

        lo = QVBoxLayout(self)
        lo.setContentsMargins(1, 1, 1, 1)
        lo.setSpacing(0)

        self._title = QLabel("⠿  Toolbar  — drag here · drop near edge to dock")
        self._title.setFixedHeight(18)
        self._title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._title.setStyleSheet(
            "background:#2a2a3a; color:#aaa; font-size:8px;"
            "border-bottom:1px solid #555; padding:0 4px;")
        self._title.setCursor(Qt.CursorShape.SizeAllCursor)
        lo.addWidget(self._title)
        lo.addWidget(content)
        self.setStyleSheet("background:#1e1e24; border:1px solid #666;")
        self.adjustSize()
        self.move(initial_global - QPoint(self.width()//2, 10))
        self.show()

        # Poll timer — fires every 16 ms (~60 fps) while dragging
        self._poll = QTimer(self)
        self._poll.setInterval(16)
        self._poll.timeout.connect(self._poll_drag)

        if start_dragging:
            self._start_drag(initial_global)

    def _start_drag(self, global_pos: QPoint):
        self._dragging    = True
        self._drag_offset = global_pos - self.pos()
        self._poll.start()

    def _poll_drag(self):
        """Called every 16ms while dragging — moves window and updates snap."""
        if not self._dragging:
            self._poll.stop()
            return
        # Check mouse buttons still held (works on Wayland/X11 without grabMouse)
        from PyQt6.QtWidgets import QApplication
        buttons = QApplication.mouseButtons()
        if not (buttons & Qt.MouseButton.LeftButton):
            # Button released — finalise
            self._dragging = False
            self._poll.stop()
            self._overlay.set_zone(SNAP_NONE)
            zone = self._snap_zone
            self._snap_zone = SNAP_NONE
            self.redock.emit(zone)
            return
        gp = QCursor.pos()
        self.move(gp - self._drag_offset)
        self._update_snap(gp)

    def _update_snap(self, global_pos: QPoint):
        zone = self._calc_zone(global_pos)
        if zone != self._snap_zone:
            self._snap_zone = zone
            self._overlay.set_zone(zone)
            if zone:
                names = {SNAP_TOP:'TOP',SNAP_BOTTOM:'BOTTOM',
                         SNAP_LEFT:'LEFT',SNAP_RIGHT:'RIGHT'}
                self._title.setStyleSheet(
                    "background:#1a5a8a; color:#fff; font-size:8px;"
                    "border-bottom:1px solid #4af; padding:0 4px;")
                self._title.setText(f"⠿  Release to dock → {names[zone]}")
            else:
                self._title.setStyleSheet(
                    "background:#2a2a3a; color:#aaa; font-size:8px;"
                    "border-bottom:1px solid #555; padding:0 4px;")
                self._title.setText("⠿  Toolbar  — drag here · drop near edge to dock")

    def _calc_zone(self, global_pos: QPoint) -> str:
        pp = self._panel
        if not pp or not pp.isVisible():
            return SNAP_NONE
        tl = pp.mapToGlobal(QPoint(0, 0))
        w, h = pp.width(), pp.height()
        T = SNAP_THRESHOLD
        if not QRect(tl, QSize(w, h)).adjusted(-T,-T,T,T).contains(global_pos):
            return SNAP_NONE
        lc = global_pos - tl
        dists = {SNAP_TOP:lc.y(), SNAP_BOTTOM:h-lc.y(),
                 SNAP_LEFT:lc.x(), SNAP_RIGHT:w-lc.x()}
        zone, dist = min(dists.items(), key=lambda x: x[1])
        return zone if dist <= T else SNAP_NONE

    # Title bar also allows dragging the static (non-active) float window
    def mousePressEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton:
            # Only start drag if click is on title bar area
            if e.position().toPoint().y() <= self._title.height():
                self._start_drag(e.globalPosition().toPoint())

    def closeEvent(self, e):
        self._poll.stop()
        self._overlay.set_zone(SNAP_NONE)
        super().closeEvent(e)


# ── DockableToolbar ───────────────────────────────────────────────────────────
class DockableToolbar(QWidget):
    dock_position_changed = pyqtSignal(str)

    def __init__(self, parent_panel: QWidget, parent=None):
        super().__init__(parent)
        self._panel     = parent_panel
        self._dock_pos  = SNAP_TOP
        self._collapsed = False
        self._floating  = False
        self._float_win = None
        self._content   = None
        self._overlay   = None

        self._layout = QHBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.setSpacing(2)

        self._grip = _GripHandle(self)
        self._grip.drag_started.connect(self._on_drag_started)
        self._grip.click_release.connect(self._toggle_collapse)
        self._grip.customContextMenuRequested.connect(
            lambda p: self._show_menu(self._grip.mapToGlobal(p)))
        self._layout.addWidget(self._grip)

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

    def _get_overlay(self) -> _EdgeOverlay:
        if self._overlay is None or not self._overlay.parent():
            self._overlay = _EdgeOverlay(self._panel)
        return self._overlay

    # ── Collapse ─────────────────────────────────────────────────────────────
    def _toggle_collapse(self):
        self._collapsed = not self._collapsed
        if self._content:
            self._content.setVisible(not self._collapsed)

    # ── Float ─────────────────────────────────────────────────────────────────
    def _on_drag_started(self, global_press: QPoint):
        if self._floating and isinstance(self._float_win, _FloatWindow):
            # Re-engage drag on existing window
            self._float_win._start_drag(global_press)
            return

        self._floating = True
        self._remove_from_dock()

        overlay = self._get_overlay()
        win = _FloatWindow(self, self._panel, overlay, global_press,
                           start_dragging=True)
        win.redock.connect(self._on_redock)
        self._float_win = win
        self.dock_position_changed.emit('float')

    def _on_redock(self, zone: str):
        # Remove self from float window before destroying it
        if self._float_win:
            lo = self._float_win.layout()
            if lo:
                lo.removeWidget(self)
            self._float_win.hide()
            self._float_win.deleteLater()
            self._float_win = None

        if not zone:
            # Stay floating but not dragging — re-use _FloatWindow, no drag start
            self._floating = True
            overlay = self._get_overlay()
            win = _FloatWindow(self, self._panel, overlay,
                               QCursor.pos(), start_dragging=False)
            win.redock.connect(self._on_redock)
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
            for i in range(plo.count()):
                item = plo.itemAt(i)
                if item and item.layout():
                    item.layout().removeWidget(self)

    def _dock_to(self, pos: str):
        pp  = self._panel
        plo = pp.layout() if pp else None
        if not plo:
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
                        lo.insertWidget(0 if side == SNAP_LEFT else lo.count(),
                                        self, stretch=0)
                        return
        plo.insertWidget(0, self, stretch=0)

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
