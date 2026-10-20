#this belongs in apps/components/Path_Workshop/depends/path_win_func.py - Version: 2
# X-Seti - October 20 2026 - IMG Factory 1.6 - Path Workshop window

"""
Path Workshop window - frameless drag/resize, dock/undock, corner overlay.
"""

##Methods list -

##class _CornerOverlay: -
# __init__
# _update_mask
# paintEvent
# resizeEvent
# setGeometry
# update_state

##class PathWindowMixin: -
# _dock_to_main
# _dropped_files
# _get_icon_color
# _get_resize_corner
# _get_resize_direction
# _handle_corner_resize
# _handle_resize
# _is_on_draggable_area
# _refresh_corner_overlay
# _setup_corner_overlay
# _toggle_maximize
# _toggle_tearoff
# _undock_from_main
# _update_cursor
# _update_transform_text_panel_visibility
# closeEvent
# dragEnterEvent
# dragMoveEvent
# dropEvent
# hideEvent
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# paintEvent
# resizeEvent
# showEvent
# toggle_dock_mode

from PyQt6.QtWidgets import QPushButton, QWidget
from PyQt6.QtCore import Qt, QTimer
from PyQt6.QtGui import QBrush, QColor, QPainter, QPainterPath

# - Corner overlay (window chrome)
class _CornerOverlay(QWidget):
    """Transparent overlay that draws corner resize triangles on top of all children.
    Uses setMask() so only the triangle pixels exist — fully transparent elsewhere.
    WA_AlwaysStackOnTop keeps it above all sibling widgets on Wayland/KDE."""

    SIZE = 20   # triangle leg size in pixels

    def __init__(self, parent): #vers 3
        super().__init__(parent)
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, True)
        self.setAttribute(Qt.WidgetAttribute.WA_NoSystemBackground, True)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground, True)
        self.setAttribute(Qt.WidgetAttribute.WA_AlwaysStackOnTop, True)
        self.setWindowFlags(Qt.WindowType.Widget)
        self._hover_corner = None
        self._app_settings = None
        self.setGeometry(0, 0, parent.width(), parent.height())
        self._update_mask()

    def _update_mask(self): #vers 1
        """Create a mask covering only the four corner triangles."""
        from PyQt6.QtGui import QRegion, QPolygon
        from PyQt6.QtCore import QPoint
        s = self.SIZE
        w, h = self.width(), self.height()
        region = QRegion()
        for pts in [
            [QPoint(0,0),    QPoint(s,0),    QPoint(0,s)],     # top-left
            [QPoint(w,0),    QPoint(w-s,0),  QPoint(w,s)],     # top-right
            [QPoint(0,h),    QPoint(s,h),    QPoint(0,h-s)],   # bottom-left
            [QPoint(w,h),    QPoint(w-s,h),  QPoint(w,h-s)],   # bottom-right
        ]:
            region = region.united(QRegion(QPolygon(pts)))
        self.setMask(region)

    def update_state(self, hover_corner, app_settings): #vers 1
        self._hover_corner = hover_corner
        self._app_settings = app_settings
        self.update()

    def setGeometry(self, *args): #vers 1
        super().setGeometry(*args)
        self._update_mask()

    def resizeEvent(self, event): #vers 1
        super().resizeEvent(event)
        self._update_mask()

    def paintEvent(self, event): #vers 2
        s = self.SIZE
        if self._app_settings:
            try:
                colors = self._app_settings.get_theme_colors()
                accent = QColor(colors.get('accent_primary', '#4682FF'))
            except Exception:
                accent = QColor(70, 130, 255)
        else:
            accent = QColor(70, 130, 255)
        accent.setAlpha(200)
        hover_c = QColor(accent); hover_c.setAlpha(255)
        w, h = self.width(), self.height()
        corners = {
            'top-left':     [(0,0),  (s,0),   (0,s)],
            'top-right':    [(w,0),  (w-s,0), (w,s)],
            'bottom-left':  [(0,h),  (s,h),   (0,h-s)],
            'bottom-right': [(w,h),  (w-s,h), (w,h-s)],
        }
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        for name, pts in corners.items():
            path = QPainterPath()
            path.moveTo(*pts[0]); path.lineTo(*pts[1]); path.lineTo(*pts[2])
            path.closeSubpath()
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(hover_c if self._hover_corner == name else accent))
            painter.drawPath(path)
        painter.end()


_RADAR_EXTS = ('.txd', '.pvr', '.png', '.jpg', '.jpeg', '.bmp', '.tga')
_PATH_EXTS = ('.ipl', '.ide', '.dat', '.dtz', '.rrr')


class PathWindowMixin: #vers 2
    """Frameless drag/resize, dock/undock, window events, drag and drop."""

    def _dropped_files(self, event): #vers 1
        """Local radar / path files in a drag."""
        md = event.mimeData()
        if not md.hasUrls():
            return []
        return [u.toLocalFile() for u in md.urls() if u.isLocalFile()
                and u.toLocalFile().lower().endswith(_RADAR_EXTS + _PATH_EXTS + ('.img',))]

    def dragEnterEvent(self, event): #vers 1
        """Accept radar and path files."""
        if self._dropped_files(event):
            event.acceptProposedAction()
        else:
            event.ignore()

    def dragMoveEvent(self, event): #vers 1
        self.dragEnterEvent(event)

    def dropEvent(self, event): #vers 1
        """Radar files load the map; path files (several, Shift adds) load paths."""
        import os
        files = self._dropped_files(event)
        if not files:
            event.ignore()
            return
        event.acceptProposedAction()
        radar, paths = [], []
        for f in files:
            low = os.path.basename(f).lower()
            if low.endswith(_RADAR_EXTS) or (low.endswith('.img') and low != 'carrec.img'):
                radar.append(f)              # gta3.img nodes: use Load Paths
            else:
                paths.append(f)              # .ipl/.dat/.dtz/.rrr, carrec.img
        if radar:
            r = radar[0]
            if r.lower().endswith(('.png', '.jpg', '.jpeg', '.bmp', '.tga')):
                self._open_radar_image(r)
            else:
                self._open_file(r)
        if paths:
            self._load_paths(paths)


    # = Window functions

    def _get_icon_color(self): #Vers 1
        if self.app_settings:
            colors = self.app_settings.get_theme_colors()
            return colors.get('text_primary', '#ffffff')
        return '#ffffff'

    def toggle_dock_mode(self): #Vers 1
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

    def _dock_to_main(self): #vers 2
        """Return a torn-off workshop to its IMG Factory tab."""
        from apps.methods.dock_panels import redock_to_tab
        if not redock_to_tab(self):      # no tab to return to
            return
        self.is_docked = True
        self.standalone_mode = False
        if hasattr(self, '_workshop_toolbar'):
            self._workshop_toolbar.setVisible(False)
        if hasattr(self, 'docked_settings_btn'):
            self.docked_settings_btn.setVisible(True)
        if hasattr(self, 'dock_btn'):
            self.dock_btn.setVisible(False)
        # Hide the same window-chrome-style toolbar elements _create_
        # toolbar itself hides at construction time when starting
        # docked (Aug 20 2026,
        for attr in ('menu_toggle_btn', 'settings_btn', 'toolbar_undo_btn',
                     'info_radar_btn', 'properties_btn'):
            if hasattr(self, attr):
                getattr(self, attr).setVisible(False)
        self.show(); self.raise_()

    def _undock_from_main(self): #vers 2
        self.standalone_mode = True
        self.is_docked = False
        self.setWindowFlags(Qt.WindowType.Window | Qt.WindowType.FramelessWindowHint)
        if hasattr(self, '_workshop_toolbar'):
            self._workshop_toolbar.setVisible(True)
        if hasattr(self, 'docked_settings_btn'):
            self.docked_settings_btn.setVisible(False)
        if hasattr(self, 'settings_btn'):
            self.settings_btn.setVisible(True)
        if hasattr(self, 'dock_btn'):
            self.dock_btn.setVisible(True)      # D docks back
        # Restore the same window-chrome-style elements _dock_to_main
        # hides (Aug 20 2026, see that method's own comment for the
        # fuller reasoning) - the reverse half of that same fix, so
        # undocking after docking correctly brings these back too,
        # not just the tool's own initial construction-time state.
        for attr in ('menu_toggle_btn', 'toolbar_undo_btn',
                     'info_radar_btn', 'properties_btn'):
            if hasattr(self, attr):
                getattr(self, attr).setVisible(True)
        self.resize(1300, 800)
        self.show(); self.raise_()

    def _toggle_tearoff(self): #Vers 1
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

    def _toggle_maximize(self): #Vers 1
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()

    # Corner resize + dragging (identical pattern to COL Workshop)

    def _update_transform_text_panel_visibility(self): #vers 2
        """Toggle between text+icon panel (wide) and icon-only strip (narrow).
        Reads threshold from IMG Factory settings. Also collapses bottom buttons."""
        tp   = getattr(self, '_transform_text_panel_ref', None)
        ip   = getattr(self, '_transform_icon_panel_ref', None)
        mode = getattr(self, 'button_display_mode', 'both')

        if mode == 'icons':
            if tp: tp.setVisible(False)
            if ip: ip.setVisible(True)
            return
        if mode == 'text':
            if tp: tp.setVisible(True)
            if ip: ip.setVisible(False)
            return

        # Measure right panel width directly
        rp = getattr(self, '_right_panel_ref', None)
        if rp:
            ref_w = rp.width()
        else:
            splitter = getattr(self, '_main_splitter', None)
            ref_w = self.width()
            if splitter and tp:
                w = tp
                while w and w.parent() is not splitter:
                    w = w.parent() if hasattr(w, 'parent') else None
                if w:
                    ref_w = w.width()

        try:
            from apps.methods.imgfactory_ui_settings import get_collapse_threshold
            threshold = get_collapse_threshold(getattr(self, 'main_window', None))
        except Exception:
            threshold = 550
        wide = ref_w >= threshold
        if tp: tp.setVisible(wide)
        if ip: ip.setVisible(not wide)

        # Toggle bottom panel rows the same way
        btr = getattr(self, '_bottom_text_row', None)
        bir = getattr(self, '_bottom_icon_row', None)
        if btr: btr.setVisible(wide)
        if bir: bir.setVisible(not wide)


    def _get_resize_corner(self, pos): #Vers 2
        if not self.standalone_mode:           # docked: no corner resize
            return None
        size = self.corner_size; w = self.width(); h = self.height()
        if pos.x() < size and pos.y() < size:           return "top-left"
        if pos.x() > w - size and pos.y() < size:       return "top-right"
        if pos.x() < size and pos.y() > h - size:       return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:   return "bottom-right"
        return None


    def _update_cursor(self, direction): #Vers 1
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


    def _get_resize_direction(self, pos): #vers 1
        """Determine resize direction based on mouse position"""
        rect = self.rect()
        margin = self.resize_margin

        left = pos.x() < margin
        right = pos.x() > rect.width() - margin
        top = pos.y() < margin
        bottom = pos.y() > rect.height() - margin

        if left and top:
            return "top-left"
        elif right and top:
            return "top-right"
        elif left and bottom:
            return "bottom-left"
        elif right and bottom:
            return "bottom-right"
        elif left:
            return "left"
        elif right:
            return "right"
        elif top:
            return "top"
        elif bottom:
            return "bottom"

        return None


    def _handle_resize(self, global_pos): #vers 1
        """Handle window resizing"""
        if not self.resize_direction or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.frameGeometry()

        min_width = 800
        min_height = 600

        # Handle horizontal resizing
        if "left" in self.resize_direction:
            new_width = geometry.width() - delta.x()
            if new_width >= min_width:
                geometry.setLeft(geometry.left() + delta.x())
        elif "right" in self.resize_direction:
            new_width = geometry.width() + delta.x()
            if new_width >= min_width:
                geometry.setRight(geometry.right() + delta.x())

        # Handle vertical resizing
        if "top" in self.resize_direction:
            new_height = geometry.height() - delta.y()
            if new_height >= min_height:
                geometry.setTop(geometry.top() + delta.y())
        elif "bottom" in self.resize_direction:
            new_height = geometry.height() + delta.y()
            if new_height >= min_height:
                geometry.setBottom(geometry.bottom() + delta.y())

        self.setGeometry(geometry)
        self.drag_position = global_pos


    def _is_on_draggable_area(self, pos): #Vers 1
        if not hasattr(self, 'titlebar'):
            return False
        if not self.titlebar.rect().contains(pos):
            return False
        for w in self.titlebar.findChildren(QPushButton):
            if w.isVisible() and w.geometry().contains(pos):
                return False
        return True


    def mousePressEvent(self, event): #Vers 1
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


    def mouseMoveEvent(self, event): #Vers 1
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept(); return
        else:
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()
            self._refresh_corner_overlay()
            self._update_cursor(corner)
        super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event): #Vers 2
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    def _handle_corner_resize(self, global_pos): #Vers 1
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


    def paintEvent(self, event): #Vers 2
        super().paintEvent(event)
        # Corner handles drawn by _corner_overlay — see _setup_corner_overlay

    def _setup_corner_overlay(self): #vers 4
        """Create or refresh the corner resize overlay."""
        if not self.standalone_mode:
            return
        # Destroy stale overlay if window was resized before it was created
        existing = getattr(self, '_corner_overlay', None)
        if existing is not None:
            existing.setGeometry(0, 0, self.width(), self.height())
            existing.raise_()
            existing.update()
            return
        overlay = _CornerOverlay(self)
        self._corner_overlay = overlay
        overlay.setGeometry(0, 0, self.width(), self.height())
        overlay.show()
        overlay.raise_()

    def _refresh_corner_overlay(self): #vers 1
        if hasattr(self, '_corner_overlay'):
            self._corner_overlay.setGeometry(0, 0, self.width(), self.height())
            self._corner_overlay.update_state(
                getattr(self, 'hover_corner', None),
                self.app_settings)
            self._corner_overlay.raise_()

    def showEvent(self, event): #vers 3
        super().showEvent(event)
        QTimer.singleShot(0, self._radar.update)       # first paint after layout settles
        QTimer.singleShot(250, self._radar.update)
        if self.standalone_mode:
            # Small delay ensures window geometry is finalised
            QTimer.singleShot(150, self._setup_corner_overlay)

    def resizeEvent(self, event): #vers 2
        super().resizeEvent(event)
        if hasattr(self,'size_grip'): self.size_grip.move(self.width()-16,self.height()-16)
        self._refresh_corner_overlay()

    def hideEvent(self, event): #vers 1
        """Docked tabs never get closeEvent: keep ribbon positions on hide."""
        self.ribbon_save_state()
        super().hideEvent(event)

    def closeEvent(self, event): #vers 5
        if not self._confirm_discard_paths():
            event.ignore(); return
        self.ribbon_save_state()
        # Save window geometry
        if self.standalone_mode:
            g = self.geometry()
            self.RAD_settings.set('window_x', g.x())
            self.RAD_settings.set('window_y', g.y())
            self.RAD_settings.set('window_w', g.width())
            self.RAD_settings.set('window_h', g.height())
            self.RAD_settings.save()
        self.window_closed.emit()
        event.accept()
