"""
apps/components/Model_Editor/dockable_toolbar.py  — Build 11

Key design:
  - NO separate title bar above icons.
  - Grip handle (⠿) is the FIRST icon in the row/column, same size
    as the other buttons, sitting inline with the tool icons.
  - Float window = just a thin border around the grip+icons, nothing else.
  - Drag from grip → float follows cursor (QTimer poll, works on Wayland).
  - Drop near parent edge → snap highlight + redock.
  - Right-click grip → context menu with Save/Load UI layout.
"""

import json
from pathlib import Path

from PyQt6.QtWidgets import (
    QWidget, QHBoxLayout, QVBoxLayout, QGridLayout,
    QLabel, QMenu, QSizePolicy, QApplication, QPushButton,
)
from PyQt6.QtCore import Qt, QPoint, QRect, QSize, pyqtSignal, QTimer
from PyQt6.QtGui import QPainter, QColor, QPen, QCursor, QBrush, QPolygon

SNAP_NONE   = ''
SNAP_TOP    = 'top'
SNAP_BOTTOM = 'bottom'
SNAP_LEFT   = 'left'
SNAP_RIGHT  = 'right'
SNAP_THRESHOLD = 60
EDGE_THICKNESS = 20


#    Edge overlay                                                              
class _EdgeOverlay(QWidget):
    def __init__(self, parent_panel: QWidget):
        super().__init__(parent_panel)
        self._zone = SNAP_NONE
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)
        self.hide()


    def _get_ui_color(self, key): #vers 1
        """Return theme-aware QColor. No hardcoded colors - everything via app_settings."""
        from PyQt6.QtGui import QColor
        try:
            app_settings = getattr(self, 'app_settings', None) or \
                getattr(getattr(self, 'main_window', None), 'app_settings', None)
            if app_settings and hasattr(app_settings, 'get_ui_color'):
                return app_settings.get_ui_color(key)
        except Exception:
            pass
        pal = self.palette()
        if key == 'viewport_bg':
            return pal.color(pal.ColorRole.Base)
        if key == 'viewport_text':
            return pal.color(pal.ColorRole.PlaceholderText)
        if key == 'border':
            return pal.color(pal.ColorRole.Mid)
        return pal.color(pal.ColorRole.WindowText)

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
        accent  = self._get_ui_color('accent_primary') if hasattr(self,'_get_ui_color') else QColor(30,144,255)
        accent.setAlpha(130) if not hasattr(self,'_get_ui_color') else None
        outline = self._get_ui_color('accent_primary') if hasattr(self,'_get_ui_color') else QColor(30,144,255)
        if not hasattr(self,'_get_ui_color'): outline.setAlpha(200)

        # Sibling-relative drop indicator
        if self._zone.startswith('sibling:'):
            parts = self._zone.split(':', 2)
            side = parts[1] if len(parts) > 1 else 'before'
            target_key = parts[2] if len(parts) > 2 else ''
            panel = self.parent()
            target_widget = None

            # Deep search — target may be nested inside row QHBoxLayouts
            def _find_in(lo):
                for i in range(lo.count()):
                    item = lo.itemAt(i)
                    if item and item.widget():
                        if getattr(item.widget(), '_settings_key', '') == target_key:
                            return item.widget()
                    elif item and item.layout():
                        result = _find_in(item.layout())
                        if result:
                            return result
                return None

            if panel and panel.layout():
                target_widget = _find_in(panel.layout())

            if target_widget:
                tl = target_widget.mapTo(panel, QPoint(0, 0))
                insert_accent = QColor(self._get_ui_color('accent_primary'))
                insert_accent.setAlpha(220)
                p.setPen(QPen(insert_accent, 3))
                p.setBrush(QBrush(insert_accent))

                if side in ('before', 'after'):
                    # Vertical stacking — horizontal insertion line at top/bottom edge
                    if side == 'before':
                        y = tl.y()
                    else:
                        y = tl.y() + target_widget.height()
                    p.drawLine(4, y, panel.width() - 4, y)
                    p.setPen(Qt.PenStyle.NoPen)
                    p.drawPolygon(QPolygon([QPoint(2, y-5), QPoint(2, y+5), QPoint(10, y)]))
                    p.drawPolygon(QPolygon([QPoint(panel.width()-2, y-5),
                                            QPoint(panel.width()-2, y+5),
                                            QPoint(panel.width()-10, y)]))
                elif side in ('left', 'right'):
                    # Horizontal stacking — vertical insertion line at left/right edge
                    if side == 'left':
                        x = tl.x()
                    else:
                        x = tl.x() + target_widget.width()
                    y0 = tl.y()
                    y1 = tl.y() + target_widget.height()
                    p.drawLine(x, y0 + 4, x, y1 - 4)
                    p.setPen(Qt.PenStyle.NoPen)
                    p.drawPolygon(QPolygon([QPoint(x-5, y0+2), QPoint(x+5, y0+2), QPoint(x, y0+10)]))
                    p.drawPolygon(QPolygon([QPoint(x-5, y1-2), QPoint(x+5, y1-2), QPoint(x, y1-10)]))
            return

        bands = {
            SNAP_TOP:    QRect(0,     0,     w, T),
            SNAP_BOTTOM: QRect(0,     h-T,   w, T),
            SNAP_LEFT:   QRect(0,     0,     T, h),
            SNAP_RIGHT:  QRect(w-T,   0,     T, h),
        }
        band = bands.get(self._zone)
        if band is None:
            return
        p.fillRect(band, accent)
        p.setPen(QPen(outline, 2))
        p.drawRect(band.adjusted(1, 1, -1, -1))
        _tc = self._get_ui_color('text_primary') if hasattr(self,'_get_ui_color') else QColor(255,255,255)
        p.setPen(QPen(_tc, 1))
        labels = {SNAP_TOP:'▼ Top', SNAP_BOTTOM:'▲ Bottom',
                  SNAP_LEFT:'▶ Left', SNAP_RIGHT:'◀ Right'}
        p.drawText(band, Qt.AlignmentFlag.AlignCenter, labels.get(self._zone, ''))


#    Grip button — lives inline with the icons                                  
class _GripHandle(QPushButton):
    """
    A ⠿ dotted grip button the same size as the tool buttons.
    Left-click = collapse/expand.
    Drag (>5px) = begin float.
    Right-click = context menu.
    """
    drag_started  = pyqtSignal(QPoint)
    click_release = pyqtSignal()

    def __init__(self, size: int = 28, parent=None):
        super().__init__(parent)
        self._btn_size  = size
        self._pressing  = False
        self._dragged   = False
        self._vertical  = False   # set by DockableToolbar.set_dock_position()
        self._press_pos = QPoint()
        self.setFixedSize(size, size)
        self.setCursor(Qt.CursorShape.SizeAllCursor)
        self.setToolTip("Drag → float  ·  Click → collapse  ·  Right-click → dock menu")
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        # No explicit stylesheet — inherit from application stylesheet like all other buttons

    def paintEvent(self, event):
        super().paintEvent(event)
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        # Use palette foreground so grip is theme-aware (light/dark)
        c = self.palette().color(self.foregroundRole())
        if not c.isValid():
            c = self._get_ui_color('viewport_text') if hasattr(self,'_get_ui_color') else QColor(190,190,190)
        p.setBrush(QBrush(c))
        w, h = self.width(), self.height()
        cx, cy = w // 2, h // 2

        if not self._vertical:
            # Horizontal bar: ||>  (bars vertical, arrow points right)
            bar_h = 10
            bar_top = cy - bar_h // 2
            p.setPen(QPen(c, 1.5))
            p.drawLine(cx - 6, bar_top, cx - 6, bar_top + bar_h)
            p.drawLine(cx - 4, bar_top, cx - 4, bar_top + bar_h)
            # Triangle pointing right
            p.setPen(Qt.PenStyle.NoPen)
            tri = QPolygon([
                QPoint(cx - 1, cy - 5),
                QPoint(cx - 1, cy + 5),
                QPoint(cx + 5, cy),
            ])
            p.drawPolygon(tri)
        else:
            # Vertical bar: = over v  (bars horizontal, arrow points down)
            bar_w = 10
            bar_left = cx - bar_w // 2
            p.setPen(QPen(c, 1.5))
            p.drawLine(bar_left, cy - 6, bar_left + bar_w, cy - 6)
            p.drawLine(bar_left, cy - 4, bar_left + bar_w, cy - 4)
            # Triangle pointing down
            p.setPen(Qt.PenStyle.NoPen)
            tri = QPolygon([
                QPoint(cx - 5, cy - 1),
                QPoint(cx + 5, cy - 1),
                QPoint(cx,     cy + 5),
            ])
            p.drawPolygon(tri)

    def set_vertical(self, v: bool):
        """Called by DockableToolbar.set_dock_position() to update icon orientation."""
        self._vertical = v
        self.update()   # repaint with correct ||> vs =v icon

    def changeEvent(self, event): #vers 1
        """Repaint grip when palette changes (theme switch)."""
        super().changeEvent(event)
        from PyQt6.QtCore import QEvent
        if event.type() in (QEvent.Type.PaletteChange, QEvent.Type.StyleChange):
            self.update()

    def mousePressEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton:
            self._pressing  = True
            self._dragged   = False
            self._press_pos = e.globalPosition().toPoint()

    def mouseMoveEvent(self, e):
        if self._pressing:
            delta = e.globalPosition().toPoint() - self._press_pos
            if delta.manhattanLength() > 5:
                self._pressing = False
                self._dragged  = True
                self.drag_started.emit(self._press_pos)

    def mouseReleaseEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton:
            if self._pressing and not self._dragged:
                self._pressing = False
                self.click_release.emit()
            self._pressing = False
            self._dragged  = False


#    Float window — thin border, no title bar                                   
class _FloatWindow(QWidget):
    redock = pyqtSignal(str)   # zone or '' to stay floating

    def __init__(self, content: QWidget, parent_panel: QWidget,
                 overlay: _EdgeOverlay, initial_global: QPoint,
                 start_dragging: bool = True,
                 extra_panels: list = None):
        super().__init__(None,
            Qt.WindowType.Tool |
            Qt.WindowType.FramelessWindowHint |
            Qt.WindowType.WindowStaysOnTopHint)
        self.setAttribute(Qt.WidgetAttribute.WA_ShowWithoutActivating)
        self._panel        = parent_panel
        self._extra_panels = extra_panels or []
        self._overlay      = overlay
        self._dragging     = False
        self._drag_offset  = QPoint()
        self._snap_zone    = SNAP_NONE

        lo = QVBoxLayout(self)
        lo.setContentsMargins(2, 2, 2, 2)
        lo.setSpacing(0)
        lo.setSizeConstraint(QVBoxLayout.SizeConstraint.SetFixedSize)
        lo.addWidget(content)

        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        self.setAutoFillBackground(True)
        # Get theme colour explicitly — palette() may not resolve for frameless Tool windows
        try:
            app_settings = getattr(parent_panel, 'app_settings', None) or                 getattr(getattr(parent_panel, 'main_window', None), 'app_settings', None)
            if app_settings:
                tc  = app_settings.get_theme_colors() or {}
                bg  = tc.get('bg_primary',  tc.get('window_bg', ''))
                brd = tc.get('border', '')
                if bg:
                    self.setStyleSheet(
                        f"background:{bg}; border:1px solid {brd if brd else bg};")
                else:
                    self.setStyleSheet("background:palette(window); border:1px solid palette(mid);")
            else:
                self.setStyleSheet("background:palette(window); border:1px solid palette(mid);")
        except Exception:
            self.setStyleSheet("background:palette(window); border:1px solid palette(mid);")
        self.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)
        self.adjustSize()
        self.move(initial_global - QPoint(self.width() // 2, self.height() // 2))
        self.show()

        self._poll = QTimer(self)
        self._poll.setInterval(16)
        self._poll.timeout.connect(self._poll_drag)

        if start_dragging:
            self._start_drag(initial_global)

    def _start_drag(self, global_pos: QPoint):
        self._dragging    = True
        self._drag_offset = global_pos - self.pos()
        self._poll.start()
        self.raise_()

    def _poll_drag(self):
        if not self._dragging:
            self._poll.stop()
            return
        buttons = QApplication.mouseButtons()
        if not (buttons & Qt.MouseButton.LeftButton):
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
        self.raise_()

    def _update_snap(self, gp: QPoint):
        zone = self._calc_zone(gp)
        if zone != self._snap_zone:
            self._snap_zone = zone
            self._overlay.set_zone(zone)

    def _calc_zone(self, gp: QPoint) -> str:
        SIBLING_T = 30
        panel = self._panel
        if panel and panel.layout():
            plo = panel.layout()
            for i in range(plo.count()):
                item = plo.itemAt(i)
                if not item:
                    continue
                candidates = []
                if item.widget() and isinstance(item.widget(), DockableToolbar):
                    candidates.append((item.widget(), False))  # (widget, in_row)
                elif item.layout():
                    for j in range(item.layout().count()):
                        sub = item.layout().itemAt(j)
                        if sub and sub.widget() and isinstance(sub.widget(), DockableToolbar):
                            candidates.append((sub.widget(), True))  # in a row layout
                for sib, in_row in candidates:
                    if sib is self or not sib.isVisible():
                        continue
                    try:
                        sib_tl = sib.mapToGlobal(QPoint(0, 0))
                    except Exception:
                        continue
                    sw, sh = sib.width(), sib.height()
                    # Extended hit rect — generous horizontal, tight vertical
                    hit = QRect(sib_tl.x() - SIBLING_T, sib_tl.y() - SIBLING_T,
                                sw + SIBLING_T * 2, sh + SIBLING_T * 2)
                    if not hit.contains(gp):
                        continue
                    lc = gp - sib_tl
                    key = getattr(sib, '_settings_key', '')

                    # Intent-based detection — NOT min-distance, which always
                    # picks top/bottom for thin horizontal bars.
                    #
                    # LEFT/RIGHT (same-row horizontal stacking):
                    # Cursor is to the left of the bar OR to the right of it,
                    # AND within SIBLING_T of that edge. Only fire if the cursor
                    # is NOT primarily above/below the bar (i.e. within its
                    # vertical span with some tolerance).
                    in_vertical_span = -SIBLING_T//2 <= lc.y() <= sh + SIBLING_T//2
                    if in_vertical_span:
                        if lc.x() < 0 and abs(lc.x()) <= SIBLING_T:
                            return f'sibling:left:{key}'
                        if lc.x() > sw and (lc.x() - sw) <= SIBLING_T:
                            return f'sibling:right:{key}'

                    # BEFORE/AFTER (new-row vertical stacking):
                    # Cursor is above or below the bar within SIBLING_T.
                    in_horizontal_span = -20 <= lc.x() <= sw + 20
                    if in_horizontal_span:
                        if lc.y() < 0 and abs(lc.y()) <= SIBLING_T:
                            return f'sibling:before:{key}'
                        if lc.y() > sh and (lc.y() - sh) <= SIBLING_T:
                            return f'sibling:after:{key}'

        # --- Original panel-edge detection ---
        for panel in [self._panel] + self._extra_panels:
            if not panel or not panel.isVisible():
                continue
            try:
                tl = panel.mapToGlobal(QPoint(0, 0))
            except Exception:
                continue
            w, h = panel.width(), panel.height()
            if w <= 0 or h <= 0:
                continue
            T = SNAP_THRESHOLD
            if not QRect(tl.x()-T, tl.y()-T, w+T*2, h+T*2).contains(gp):
                continue
            lc = gp - tl
            dists = {
                SNAP_TOP:    max(0, lc.y()),
                SNAP_BOTTOM: max(0, h - lc.y()),
                SNAP_LEFT:   max(0, lc.x()),
                SNAP_RIGHT:  max(0, w - lc.x()),
            }
            zone, dist = min(dists.items(), key=lambda x: x[1])
            if dist <= T:
                return zone
        return SNAP_NONE

    # Allow dragging the static float window anywhere (not just title bar)
    def changeEvent(self, event): #vers 1
        """Repaint grip when palette changes (theme switch)."""
        super().changeEvent(event)
        from PyQt6.QtCore import QEvent
        if event.type() in (QEvent.Type.PaletteChange, QEvent.Type.StyleChange):
            self.update()

    def mousePressEvent(self, e):
        if e.button() == Qt.MouseButton.LeftButton:
            self._start_drag(e.globalPosition().toPoint())

    def closeEvent(self, e):
        self._poll.stop()
        self._overlay.set_zone(SNAP_NONE)
        super().closeEvent(e)


#    DockableToolbar                                                            
class DockableToolbar(QWidget):
    """
    A toolbar where the grip handle (⠿) is the FIRST button,
    sitting inline with the tool icons. No separate title bar.
    """
    dock_position_changed = pyqtSignal(str)
    reflow_requested      = pyqtSignal(str)

    # Settings key — override per instance to avoid clashes
    _settings_key = 'toolbar_layout'

    def __init__(self, parent_panel: QWidget, parent=None,
                 settings_key: str = 'toolbar_layout'):
        super().__init__(parent)
        self._panel        = parent_panel
        self._extra_panels = []
        self._dock_pos     = SNAP_TOP
        self._collapsed    = False
        self._floating     = False
        self._float_win    = None
        self._content      = None
        self._overlay      = None
        self._settings_key = settings_key
        self._btn_size     = 28   # matches tool button size

        # Layout: grip + content side by side (horizontal) or top-to-bottom (vertical)
        self._layout = QHBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.setSpacing(1)

        # Grip is the first widget
        self._grip = _GripHandle(self._btn_size, self)
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
        self._grip.set_vertical(vert)   # update icon orientation
        if vert:
            self._layout.setDirection(QHBoxLayout.Direction.TopToBottom)
        else:
            self._layout.setDirection(QHBoxLayout.Direction.LeftToRight)

    def _get_overlay(self) -> _EdgeOverlay:
        if self._overlay is None or not self._overlay.parent():
            self._overlay = _EdgeOverlay(self._panel)
        return self._overlay

    def _toggle_collapse(self):
        self._collapsed = not self._collapsed
        if self._content:
            self._content.setVisible(not self._collapsed)
        # When collapsed keep the toolbar visible — just the grip button
        # Force a minimum size so the grip doesn't vanish into layout
        vert = self._dock_pos in (SNAP_LEFT, SNAP_RIGHT)
        if self._collapsed:
            if vert:
                self.setMaximumWidth(self._btn_size + 4)
                self.setMaximumHeight(16777215)
            else:
                self.setMaximumHeight(self._btn_size + 4)
                self.setMaximumWidth(16777215)
        else:
            self.setMaximumWidth(16777215)
            self.setMaximumHeight(16777215)

    #    Float                                                                  
    def _on_drag_started(self, global_press: QPoint):
        if self._floating and isinstance(self._float_win, _FloatWindow):
            self._float_win._start_drag(global_press)
            return

        self._floating = True
        self._remove_from_dock()
        self.show()

        overlay = self._get_overlay()
        win = _FloatWindow(self, self._panel, overlay, global_press,
                           start_dragging=True,
                           extra_panels=self._extra_panels)
        win.redock.connect(self._on_redock)
        self._float_win = win
        self.dock_position_changed.emit('float')
        self.reflow_requested.emit('float')

    def _on_redock(self, zone: str):
        if self._float_win:
            lo = self._float_win.layout()
            if lo:
                lo.removeWidget(self)
            self._float_win.hide()
            self._float_win.deleteLater()
            self._float_win = None

        if not zone:
            # Stay floating — static window, click anywhere to drag
            self._floating = True
            overlay = self._get_overlay()
            win = _FloatWindow(self, self._panel, overlay,
                               QCursor.pos(), start_dragging=False,
                               extra_panels=self._extra_panels)
            win.redock.connect(self._on_redock)
            self._float_win = win
            self.show()

        elif zone.startswith('sibling:'):
            # Sibling-relative insertion — parse 'sibling:before/after:<key>'
            parts = zone.split(':', 2)
            side = parts[1] if len(parts) > 1 else 'before'
            target_key = parts[2] if len(parts) > 2 else ''
            self._remove_from_dock()
            # Find the target sibling and its parent layout
            panel = self._panel
            target_widget = None
            target_layout = None
            if panel:
                def _find_in_layout(lo, key):
                    for i in range(lo.count()):
                        item = lo.itemAt(i)
                        if item and item.widget():
                            if getattr(item.widget(), '_settings_key', '') == key:
                                return item.widget(), lo
                        elif item and item.layout():
                            result = _find_in_layout(item.layout(), key)
                            if result:
                                return result
                    return None
                result = _find_in_layout(panel.layout(), target_key)
                if result:
                    target_widget, target_layout = result
            if target_widget and target_layout:
                # before/after = vertical stacking (new row above/below)
                # left/right = horizontal stacking (same row, insert beside)
                for i in range(target_layout.count()):
                    item = target_layout.itemAt(i)
                    if item and item.widget() is target_widget:
                        insert_at = i if side in ('before', 'left') else i + 1
                        target_layout.insertWidget(insert_at, self, stretch=0)
                        break
            elif panel and panel.layout():
                panel.layout().insertWidget(0, self, stretch=0)
            self._floating = False
            self.set_dock_position(SNAP_TOP)
            self.show()
            self.dock_position_changed.emit(SNAP_TOP)
            QTimer.singleShot(50, lambda: self.reflow_requested.emit(SNAP_TOP))

        else:
            self._floating = False
            self._dock_to(zone)
            QTimer.singleShot(50, lambda z=zone: self.reflow_requested.emit(z))

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
            # Insert just BELOW the preview row (HBoxLayout containing preview widget),
            # not at the absolute end (which is below info/status bars).
            preview_row_idx = -1
            for i in range(plo.count()):
                item = plo.itemAt(i)
                if item and item.layout():
                    lo = item.layout()
                    for j in range(lo.count()):
                        sub = lo.itemAt(j)
                        if sub and sub.widget() and any(
                                x in type(sub.widget()).__name__
                                for x in ('Preview', 'Viewport', '3D')):
                            preview_row_idx = i
                            break
                if preview_row_idx >= 0:
                    break
            # Insert after preview row, or at end if not found
            insert_at = preview_row_idx + 1 if preview_row_idx >= 0 else plo.count()
            plo.insertWidget(insert_at, self, stretch=0)
        elif pos in (SNAP_LEFT, SNAP_RIGHT):
            self._dock_beside_preview(pos)
            self.set_dock_position(pos)
            self.show()
            self.dock_position_changed.emit(pos)
            self.reflow_requested.emit(pos)
            return
        self.set_dock_position(pos)
        self.show()
        self.dock_position_changed.emit(pos)
        self.reflow_requested.emit(pos)

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
                    if sub and sub.widget() and any(x in type(sub.widget()).__name__ for x in ('Preview', 'Viewport', '3D')):
                        lo.removeWidget(self)
                        lo.insertWidget(0 if side == SNAP_LEFT else lo.count(),
                                        self, stretch=0)
                        return
        plo.insertWidget(0, self, stretch=0)

    #    Settings save/load                                                     
    def _settings_path(self) -> Path:
        cfg = Path.home() / '.config' / 'imgfactory'
        cfg.mkdir(parents=True, exist_ok=True)
        return cfg / 'model_toolbar_layout.json'

    def save_layout(self):
        try:
            path = self._settings_path()
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data[self._settings_key] = {
                'dock_pos':  self._dock_pos,
                'floating':  self._floating,
                'collapsed': self._collapsed,
            }
            path.write_text(json.dumps(data, indent=2))
            return True
        except Exception:
            return False

    def load_layout(self, skip_redock: bool = False) -> bool:
        """Restore saved dock state. If skip_redock=True, only restores
        collapsed state — caller is responsible for placement (used when
        the ribbon row system handles layout, so _on_redock doesn't fight it)."""
        try:
            path = self._settings_path()
            if not path.exists():
                return False
            data = json.loads(path.read_text())
            cfg  = data.get(self._settings_key, {})
            if not cfg:
                return False
            pos       = cfg.get('dock_pos', SNAP_TOP)
            collapsed = cfg.get('collapsed', False)
            floating  = cfg.get('floating',  False)

            if not skip_redock:
                if floating:
                    QTimer.singleShot(250,
                        lambda: self._on_drag_started(
                            self.mapToGlobal(QPoint(self.width()//2, self.height()//2))))
                else:
                    self._on_redock(pos)

            if collapsed != self._collapsed:
                QTimer.singleShot(60, self._toggle_collapse)

            return True
        except Exception:
            return False

    #    Context menu                                                          
    def _show_menu(self, global_pos: QPoint):
        menu = QMenu(self)

        dock_sub = menu.addMenu("Dock to…")
        dock_sub.addAction("Top",    lambda: self._on_redock(SNAP_TOP))
        dock_sub.addAction("Bottom", lambda: self._on_redock(SNAP_BOTTOM))
        dock_sub.addAction("Left",   lambda: self._on_redock(SNAP_LEFT))
        dock_sub.addAction("Right",  lambda: self._on_redock(SNAP_RIGHT))

        menu.addSeparator()

        # Snap to sibling ribbon — explicit alternative to drag-and-drop
        panel = self._panel
        siblings = []
        if panel and panel.layout():
            def _collect(lo):
                for i in range(lo.count()):
                    item = lo.itemAt(i)
                    if item and item.widget():
                        w = item.widget()
                        if isinstance(w, DockableToolbar) and w is not self:
                            siblings.append(w)
                    elif item and item.layout():
                        _collect(item.layout())
            _collect(panel.layout())

        if siblings:
            snap_sub = menu.addMenu("Stack beside ribbon…")
            for sib in siblings:
                key = getattr(sib, '_settings_key', '')
                name = key.replace('model_', '').replace('_toolbar', '').replace('_', ' ').title()
                snap_sub.addAction(f"← Before '{name}'",
                    lambda _=False, k=key: self._on_redock(f'sibling:left:{k}'))
                snap_sub.addAction(f"→ After '{name}'",
                    lambda _=False, k=key: self._on_redock(f'sibling:right:{k}'))
            snap_sub.addSeparator()
            snap_sub.addAction("Move to own row",
                lambda: self._on_redock(SNAP_TOP))

        menu.addSeparator()

        if self._floating:
            menu.addAction("Dock to Top", lambda: self._on_redock(SNAP_TOP))
        else:
            menu.addAction("Float / Detach",
                lambda: self._on_drag_started(global_pos))

        menu.addSeparator()
        menu.addAction(
            "Expand toolbar" if self._collapsed else "Collapse toolbar",
            self._toggle_collapse)

        menu.addSeparator()
        layout_sub = menu.addMenu("Layout")
        layout_sub.addAction("Save toolbar layout", self._save_layout_notify)
        layout_sub.addAction("Load saved layout",   self.load_layout)
        layout_sub.addAction("Reset to default",    lambda: self._on_redock(SNAP_TOP))

        menu.addSeparator()
        ribbon_act = menu.addAction("Ribbon Manager...")
        ribbon_act.triggered.connect(self._open_ribbon_manager)

        menu.exec(global_pos)

    def _open_ribbon_manager(self): #vers 1
        """Walk up the parent chain to find the ModelWorkshop and open
        the Ribbon Manager dialog from the grip handle right-click menu."""
        parent = self._panel
        while parent is not None:
            if hasattr(parent, 'open_ribbon_manager'):
                parent.open_ribbon_manager()
                return
            parent = parent.parent() if callable(
                getattr(parent, 'parent', None)) else None

    def _save_layout_notify(self):
        ok = self.save_layout()
        # Brief status — could hook into workshop status bar
        if ok:
            self._grip.setToolTip("Layout saved ✓  —  Drag · Click · Right-click")
            QTimer.singleShot(2000,
                lambda: self._grip.setToolTip(
                    "Drag → float  ·  Click → collapse  ·  Right-click → dock menu"))
