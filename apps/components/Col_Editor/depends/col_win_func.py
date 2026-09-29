#this belongs in apps/components/Col_Editor/depends/col_win_func.py - Version: 8
# X-Seti - Sept 2026 - IMG Factory 1.6 - COL Workshop window functions

from PyQt6.QtCore import Qt, QTimer
from PyQt6.QtWidgets import QPushButton
from apps.methods.img_factory_settings import get_user_config_dir

##class COLWindowMixin: -
# _apply_button_mode_to_button
# _apply_col_btn_display
# _apply_left_compact
# _apply_window_flags
# closeEvent
# _enable_move_mode
# _get_resize_corner
# _handle_corner_resize
# _initialize_features
# _is_on_draggable_area
# mouseDoubleClickEvent
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# _on_splitter_moved
# paintEvent
# resizeEvent
# _restore_splitter_sizes
# _save_splitter_sizes
# _set_icon_display_mode
# _splitter_key
# _toggle_maximize
# _update_all_buttons
# _update_cursor
# _update_transform_text_panel_visibility

# - Window functionality


class COLWindowMixin: #vers 1
    """Frameless window, resize, button mode handlers for COLWorkshop."""

    def _initialize_features(self): #vers 3
        """Initialize all features after UI setup"""
        try:
            self._apply_theme()
            self._update_status_indicators()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("All features initialized")

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Feature init error: {str(e)}")


    def _is_on_draggable_area(self, pos): #vers 7
        """Check if position is on draggable titlebar area

        Args:
            pos: Position in titlebar coordinates (from eventFilter)

        Returns:
            True if position is on titlebar but not on any button
        """
        if not hasattr(self, 'titlebar'):
            print("[DRAG] No titlebar attribute")
            return False

        # Verify pos is within titlebar bounds
        if not self.titlebar.rect().contains(pos):
            print(f"[DRAG] Position {pos} outside titlebar rect {self.titlebar.rect()}")
            return False

        # Check if clicking on any button - if so, NOT draggable
        for widget in self.titlebar.findChildren(QPushButton):
            if widget.isVisible():
                # Get button geometry in titlebar coordinates
                button_rect = widget.geometry()
                if button_rect.contains(pos):
                    print(f"[DRAG] Clicked on button: {widget.toolTip()}")
                    return False

        # Not on any button = draggable
        print(f"[DRAG] On draggable area at {pos}")
        return True


    # - From the fixed gui - move, drag

    def _update_all_buttons(self): #vers 4
        """Update all buttons to match display mode"""
        buttons_to_update = [
            # Toolbar buttons
            ('open_btn', 'Open'),
            ('save_btn', 'Save'),
            ('save_col_btn', 'Save TXD'),
        ]

        # Adjust transform panel width based on mode
        if hasattr(self, 'transform_icon_panel'):
            if self.button_display_mode == 'icons':
                self.transform_icon_panel.setMaximumWidth(50)
            else:
                self.transform_text_panel.setMaximumWidth(200)

        for btn_name, btn_text in buttons_to_update:
            if hasattr(self, btn_name):
                button = getattr(self, btn_name)
                self._apply_button_mode_to_button(button, btn_text)
        self._update_dock_button_visibility()

    def _apply_button_mode_to_button(self, button, text): #vers 1
        """Apply display mode via shared helper."""
        from apps.methods.button_mode import apply_button_mode_to_button
        apply_button_mode_to_button(button, text, self.button_display_mode)

    def paintEvent(self, event): #vers 3
        """Paint corner resize triangles"""
        super().paintEvent(event)
        if not self.standalone_mode:           # docked: no corner handles
            return

        from PyQt6.QtGui import QPainter, QColor, QPen, QBrush, QPainterPath

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Colors
        normal_color = QColor(100, 100, 100, 150)
        hover_color = QColor(150, 150, 255, 200)

        w = self.width()
        h = self.height()
        grip_size = 8  # Make corners visible (8x8px)
        size = self.corner_size

        # Define corner triangles
        corners = {
            'top-left': [(0, 0), (size, 0), (0, size)],
            'top-right': [(w, 0), (w-size, 0), (w, size)],
            'bottom-left': [(0, h), (size, h), (0, h-size)],
            'bottom-right': [(w, h), (w-size, h), (w, h-size)]
        }
        corners2 = {
            "top-left": [(0, grip_size), (0, 0), (grip_size, 0)],
            "top-right": [(w-grip_size, 0), (w, 0), (w, grip_size)],
            "bottom-left": [(0, h-grip_size), (0, h), (grip_size, h)],
            "bottom-right": [(w-grip_size, h), (w, h), (w, h-grip_size)]
        }

        # Get theme colors for corner indicators
        if self.app_settings:
            theme_colors = self.app_settings.get_theme_colors()
            accent_color = QColor(theme_colors.get('accent_primary', '#1976d2'))
            accent_color.setAlpha(180)
        else:
            accent_color = QColor(100, 150, 255, 180)

        hover_color = QColor(accent_color)
        hover_color.setAlpha(255)

        # Draw all corners with hover effect
        for corner_name, points in corners.items():
            path = QPainterPath()
            path.moveTo(points[0][0], points[0][1])
            path.lineTo(points[1][0], points[1][1])
            path.lineTo(points[2][0], points[2][1])
            path.closeSubpath()

            # Use hover color if mouse is over this corner
            color = hover_color if self.hover_corner == corner_name else accent_color

            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(color))
            painter.drawPath(path)

        painter.end()


    def _enable_move_mode(self): #vers 2
        """Enable move window mode using system move"""
        handle = self.windowHandle()
        if handle and hasattr(handle, 'startSystemMove'):
            handle.startSystemMove()

    def _get_resize_corner(self, pos): #vers 4
        """Determine which corner is under mouse position"""
        if not self.standalone_mode:           # docked: no corner resize
            return None
        size = self.corner_size; w = self.width(); h = self.height()

        if pos.x() < size and pos.y() < size:
            return "top-left"
        if pos.x() > w - size and pos.y() < size:
            return "top-right"
        if pos.x() < size and pos.y() > h - size:
            return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:
            return "bottom-right"

        return None


    def mousePressEvent(self, event): #vers 8
        """Handle ALL mouse press - dragging and resizing"""
        if event.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(event)
            return

        pos = event.pos()

        # Check corner resize FIRST
        self.resize_corner = self._get_resize_corner(pos)
        if self.resize_corner:
            self.resizing = True
            self.drag_position = event.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            event.accept()
            return

        # Check if on titlebar
        if hasattr(self, 'titlebar') and self.titlebar.geometry().contains(pos):
            titlebar_pos = self.titlebar.mapFromParent(pos)
            if self._is_on_draggable_area(titlebar_pos):
                handle = self.windowHandle()
                if handle:
                    handle.startSystemMove()
                event.accept()
                return

        super().mousePressEvent(event)


    def mouseMoveEvent(self, event): #vers 4
        """Handle mouse move for resizing and hover effects

        Window dragging is handled by eventFilter to avoid conflicts
        """
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept()
                return
        else:
            # Update hover state and cursor
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()  # Trigger repaint for hover effect
            self._update_cursor(corner)

        # Let parent handle everything else
        super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event): #vers 2
        """Handle mouse release"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = False
            self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    def _handle_corner_resize(self, global_pos): #vers 2
        """Handle window resizing from corners"""
        if not self.resize_corner or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.initial_geometry

        min_width = 800
        min_height = 600

        # Calculate new geometry based on corner
        if self.resize_corner == "top-left":
            new_x = geometry.x() + delta.x()
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() - delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, new_y, new_width, new_height)

        elif self.resize_corner == "top-right":
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() - delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(geometry.x(), new_y, new_width, new_height)

        elif self.resize_corner == "bottom-left":
            new_x = geometry.x() + delta.x()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() + delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, geometry.y(), new_width, new_height)

        elif self.resize_corner == "bottom-right":
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() + delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.resize(new_width, new_height)


    def _update_cursor(self, direction): #vers 1
        """Update cursor based on resize direction"""
        if direction == "top" or direction == "bottom":
            self.setCursor(Qt.CursorShape.SizeVerCursor)
        elif direction == "left" or direction == "right":
            self.setCursor(Qt.CursorShape.SizeHorCursor)
        elif direction == "top-left" or direction == "bottom-right":
            self.setCursor(Qt.CursorShape.SizeFDiagCursor)
        elif direction == "top-right" or direction == "bottom-left":
            self.setCursor(Qt.CursorShape.SizeBDiagCursor)
        else:
            self.setCursor(Qt.CursorShape.ArrowCursor)


    def _set_icon_display_mode(self, mode: str): #vers 1
        """Set icon display mode: 'icons_and_text' | 'icons_only' | 'text_only'.
        Persists to img_settings and immediately updates all compact-aware buttons."""
        self.icon_display_mode = mode
        try:
            if self.main_window and hasattr(self.main_window, 'img_settings'):
                self.main_window.img_settings.set('col_icon_display_mode', mode)
        except Exception:
            pass
        self._apply_col_btn_display()


    def _apply_left_compact(self): #vers 1
        """Surface tab buttons go icon-only when the left pane is narrow."""
        from apps.methods.imgfactory_ui_settings import apply_compact_buttons
        tab = getattr(self, '_surface_tab', None)
        if tab is None:
            return
        apply_compact_buttons(self._surf_top_btns, tab.width())
        apply_compact_buttons(self._surf_list_btns, self._surf_list_panel.width())

    def _apply_col_btn_display(self): #vers 1
        """Apply current icon_display_mode to all compact-aware toolbar buttons."""
        mode = getattr(self, 'icon_display_mode', 'icons_and_text')
        icon_only = (mode == 'icons_only')
        text_only = (mode == 'text_only')
        for btn, label in getattr(self, '_col_compact_btns', []):
            if btn is None:
                continue
            if icon_only:
                btn.setText("")
                btn.setMinimumWidth(26)
                btn.setMaximumWidth(40)
            elif text_only:
                btn.setText(label)
                btn.setIcon(type(btn)().icon())   # clear icon
                btn.setMinimumWidth(52)
                btn.setMaximumWidth(16777215)
            else:   # icons_and_text
                btn.setText(label)
                btn.setMinimumWidth(52)
                btn.setMaximumWidth(16777215)


    def _on_splitter_moved(self, pos, index): #vers 4
        """Main splitter dragged: save sizes, update compact buttons."""
        if not hasattr(self, '_splitter_save_timer'):
            self._splitter_save_timer = QTimer(self)
            self._splitter_save_timer.setSingleShot(True)
            self._splitter_save_timer.timeout.connect(self._save_splitter_sizes)
        self._splitter_save_timer.start(500)
        self._apply_left_compact()
        self._update_transform_text_panel_visibility()
        try:
            from apps.methods.imgfactory_ui_settings import apply_compact_buttons
            btns = getattr(self, '_col_compact_btns', [])
            if btns:
                row = getattr(self, '_middle_btn_row', None)
                w = row.width() if (row and row.width() > 0) else self.width()
                apply_compact_buttons(btns, w, compact_threshold=320)
        except Exception:
            pass

    def _splitter_key(self): #vers 1
        """Config key; docked and standalone have different panel counts."""
        return f"splitter_sizes_{self._main_splitter.count()}"

    def _restore_splitter_sizes(self): #vers 3
        """Restore saved splitter sizes; default list panel 220px."""
        import json
        from pathlib import Path
        sp = getattr(self, '_main_splitter', None)
        if sp is None:
            return
        path = get_user_config_dir() / 'col_workshop.json'
        try:
            sizes = json.loads(path.read_text()).get(self._splitter_key())
        except (OSError, ValueError):
            sizes = None
        if not sizes or len(sizes) != sp.count():
            total = max(sp.width(), 800)
            sizes = [220, total - 220] if sp.count() == 2 else [180, 220, total - 400]
        sp.setSizes(sizes)
        QTimer.singleShot(0, self._apply_left_compact)

    def _save_splitter_sizes(self): #vers 2
        """Save main splitter sizes to col_workshop.json."""
        import json
        from pathlib import Path
        sp = getattr(self, '_main_splitter', None)
        if sp is None:
            return
        path = get_user_config_dir() / 'col_workshop.json'
        try:
            data = json.loads(path.read_text())
        except (OSError, ValueError):
            data = {}
        data[self._splitter_key()] = sp.sizes()
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(data, indent=2))

    def _update_transform_text_panel_visibility(self): #vers 4
        """No-op now - the old dual text/icon bottom rows were replaced by
        real QToolBar ribbons (Name/Format/Shadow Mesh), which manage their
        own layout/compacting natively. Kept as a safe no-op since
        resizeEvent still calls it."""
        pass

    def resizeEvent(self, event): #vers 6
        """Keep resize grip in corner; auto-collapse panels; adaptive button display."""
        super().resizeEvent(event)
        if hasattr(self, 'size_grip'):
            self.size_grip.move(self.width() - 16, self.height() - 16)
        self._update_transform_text_panel_visibility()
        self._apply_left_compact()
        # Auto icon-only when window is narrow (overrides saved mode only if narrower)
        try:
            from apps.methods.imgfactory_ui_settings import apply_compact_buttons
            btns = getattr(self, '_col_compact_btns', [])
            if btns:
                row = getattr(self, '_middle_btn_row', None)
                w = row.width() if (row and row.width() > 0) else self.width()
                apply_compact_buttons(btns, w, compact_threshold=320)
        except Exception:
            pass


    def mouseDoubleClickEvent(self, event): #vers 2
        """Handle double-click - maximize/restore

        Handled here instead of eventFilter for better control
        """
        if event.button() == Qt.MouseButton.LeftButton:
            # Convert to titlebar coordinates if needed
            if hasattr(self, 'titlebar'):
                titlebar_pos = self.titlebar.mapFromParent(event.pos())
                if self._is_on_draggable_area(titlebar_pos):
                    self._toggle_maximize()
                    event.accept()
                    return

        super().mouseDoubleClickEvent(event)


    def _toggle_maximize(self): #vers 1
        """Toggle window maximize state"""
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()


    def closeEvent(self, event): #vers 2
        """Handle close event"""
        self.window_closed.emit()
        # Remove injected tool menu from imgfactory menubar
        try:
            mw = getattr(self, 'main_window', None) or getattr(self, '_imgfactory', None)
            if mw and hasattr(mw, '_update_tool_menu_for_tab'):
                mw._update_tool_menu_for_tab(None)
        except Exception:
            pass
        event.accept()

    def _apply_window_flags(self): #vers 1
        """Apply window flags based on settings"""
        # Save current geometry
        current_geometry = self.geometry()
        was_visible = self.isVisible()

        if self.use_system_titlebar:
            # Use system window with title bar
            self.setWindowFlags(
                Qt.WindowType.Window |
                Qt.WindowType.WindowMinimizeButtonHint |
                Qt.WindowType.WindowMaximizeButtonHint |
                Qt.WindowType.WindowCloseButtonHint
            )
        else:
            # Use custom frameless window
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

        # Restore geometry and visibility
        self.setGeometry(current_geometry)

        if was_visible:
            self.show()

        if self.main_window and hasattr(self.main_window, 'log_message'):
            mode = "System title bar" if self.use_system_titlebar else "Custom frameless"
            self.main_window.log_message(f"Window mode: {mode}")


__all__ = ['COLWindowMixin']
