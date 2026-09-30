#this belongs in apps/components/Txd_Editor/depends/txd_win_func.py - Version: 4
# X-Seti - September30 2026 - IMG Factory 1.6 - TXD Workshop window

"""
TXD Workshop window - frameless drag/resize, window flags, window menus.
"""

##class TXDWindowMixin: -
# _apply_left_compact
# _enable_move_mode
# _get_resize_corner
# _handle_corner_resize
# _is_on_draggable_area
# mouseDoubleClickEvent
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# _on_splitter_moved
# paintEvent
# resizeEvent
# _show_settings_context_menu
# showEvent
# _toggle_maximize
# _update_cursor
# _update_transform_text_panel_visibility

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QPushButton

class TXDWindowMixin: #vers 1
    """window methods for TXDWorkshop."""

    def _is_on_draggable_area(self, pos): #vers 5
        """Check if position is on draggable toolbar area (stretch space, not buttons)"""
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

        # Get all buttons in toolbar
        buttons_to_check = []

        if hasattr(self, 'open_img_btn'):
            buttons_to_check.append(self.open_img_btn)
        if hasattr(self, 'open_txd_btn'):
            buttons_to_check.append(self.open_txd_btn)
        if hasattr(self, 'save_txd_btn'):
            buttons_to_check.append(self.save_txd_btn)
        if hasattr(self, 'import_btn'):
            buttons_to_check.append(self.import_btn)
        if hasattr(self, 'export_btn'):
            buttons_to_check.append(self.export_btn)
        if hasattr(self, 'export_all_btn'):
            buttons_to_check.append(self.export_all_btn)
        if hasattr(self, 'switch_btn'):
            buttons_to_check.append(self.switch_btn)
        if hasattr(self, 'props_btn'):
            buttons_to_check.append(self.props_btn)
        if hasattr(self, 'info_btn'):
            buttons_to_check.append(self.info_btn)
        if hasattr(self, 'minimize_btn'):
            buttons_to_check.append(self.minimize_btn)
        if hasattr(self, 'maximize_btn'):
            buttons_to_check.append(self.maximize_btn)
        if hasattr(self, 'close_btn'):
            buttons_to_check.append(self.close_btn)
        # Should be enabled on selection:
        # Undo depends on undo stack, not selection
        self._set_undo_enabled()

        if hasattr(self, 'check_dff_btn'):
            # Always enabled if textures exist
            self.check_dff_btn.setEnabled(len(self.texture_list) > 0)

        if not hasattr(self, 'drag_btn'):
            return False

        # Convert to toolbar coordinates
        toolbar_local_pos = self.toolbar.mapFrom(self, pos)

        # Check if clicking on drag button
        return self.drag_btn.geometry().contains(toolbar_local_pos)

    def paintEvent(self, event): #vers 3
        """Paint corner resize triangles"""
        super().paintEvent(event)
        if not self.standalone_mode:           # docked: no corner handles
            return

        from PyQt6.QtGui import QPainter, QColor, QPen, QBrush, QPainterPath

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Colors

        normal_color = self._get_ui_color('viewport_text'); normal_color.setAlpha(150)
        hover_color = self._get_ui_color('accent_primary'); hover_color.setAlpha(200)

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
            accent_color = self._get_ui_color('accent_primary'); accent_color.setAlpha(180)

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
            # Move top-left corner
            new_x = geometry.x() + delta.x()
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() - delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, new_y, new_width, new_height)

        elif self.resize_corner == "top-right":
            # Move top-right corner
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() - delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(geometry.x(), new_y, new_width, new_height)

        elif self.resize_corner == "bottom-left":
            # Move bottom-left corner
            new_x = geometry.x() + delta.x()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() + delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, geometry.y(), new_width, new_height)

        elif self.resize_corner == "bottom-right":
            # Move bottom-right corner
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

    def resizeEvent(self, event): #vers 3
        """Keep resize grip in corner; auto-collapse text panel when narrow."""
        super().resizeEvent(event)
        if hasattr(self, 'size_grip'):
            self.size_grip.move(self.width() - 16, self.height() - 16)
        self._update_transform_text_panel_visibility()
        self._apply_left_compact()

    def _on_splitter_moved(self, pos, index): #vers 4
        """Main splitter dragged: save sizes, ribbon text mode, compact buttons."""
        self._queue_splitter_save()
        self._update_transform_text_panel_visibility()
        self._apply_left_compact()

    def _apply_left_compact(self): #vers 1
        """Texture pane mini toolbar goes icon-only when narrow."""
        from apps.methods.imgfactory_ui_settings import apply_compact_buttons
        row = getattr(self, '_middle_btn_row', None)
        if row is None:
            return
        apply_compact_buttons(self._middle_compact_btns, row.width())

    def showEvent(self, event): #vers 1
        """Standalone frameless window: fix Windows 11 border on show."""
        super().showEvent(event)
        if self.standalone_mode:
            from apps.methods.imgfactory_ui_settings import apply_windows_frame
            apply_windows_frame(self)

    def _update_transform_text_panel_visibility(self): #vers 6
        """Apply the icon/text/both display mode to the ribbon toolbars via
        QToolBar's native setToolButtonStyle - replaces the old approach of
        keeping two separate panels (icon-only strip + wide text panel) and
        toggling their visibility."""
        from PyQt6.QtCore import Qt as _Qt
        mode = getattr(self, 'button_display_mode', 'both')
        style = {
            'icons': _Qt.ToolButtonStyle.ToolButtonIconOnly,
            'text':  _Qt.ToolButtonStyle.ToolButtonTextOnly,
        }.get(mode, _Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
        for tb in (getattr(self, '_tb_transform', None),
                   getattr(self, '_tb_nav', None),
                   getattr(self, '_tb_effects', None),
                   getattr(self, '_tb_name', None),
                   getattr(self, '_tb_format', None),
                   getattr(self, '_tb_mipmaps', None)):
            if tb:
                tb.setToolButtonStyle(style)

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

    def _show_settings_context_menu(self, pos): #vers 3
        """Show context menu for Settings button"""
        from PyQt6.QtWidgets import QMenu

        menu = QMenu(self)

        # Move window action
        move_action = menu.addAction("Move Window")
        move_action.triggered.connect(self._enable_move_mode)

        # Maximize window action
        max_action = menu.addAction("Maximize Window")
        max_action.triggered.connect(self._toggle_maximize)

        # Minimize action
        min_action = menu.addAction("Minimize")
        min_action.triggered.connect(self.showMinimized)

        menu.addSeparator()

        # Shaders action
        shaders_action = menu.addAction("Shaders")
        shaders_action.triggered.connect(self._show_shaders_dialog)

        menu.addSeparator()

        # Icon display mode submenu — auto-compact handled by resizeEvent
        display_menu = menu.addMenu("Button Display")

        icons_text_action = display_menu.addAction("Icons & Text")
        icons_text_action.setCheckable(True)
        icons_text_action.setChecked(self.button_display_mode == 'both')
        icons_text_action.triggered.connect(lambda: self._set_icon_display_mode('both'))

        icons_only_action = display_menu.addAction("Icons Only")
        icons_only_action.setCheckable(True)
        icons_only_action.setChecked(self.button_display_mode == 'icons')
        icons_only_action.triggered.connect(lambda: self._set_icon_display_mode('icons'))

        text_only_action = display_menu.addAction("Text Only")
        text_only_action.setCheckable(True)
        text_only_action.setChecked(self.button_display_mode == 'text')
        text_only_action.triggered.connect(lambda: self._set_icon_display_mode('text'))

        # Show menu at button position
        menu.exec(self.properties_btn.mapToGlobal(pos))

    def _enable_move_mode(self): #vers 2
        """Enable move window mode using system move"""
        # Use Qt's system move which works on Windows, Linux, etc.
        if hasattr(self.windowHandle(), 'startSystemMove'):
            self.windowHandle().startSystemMove()
        else:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "Move Window",
                "Drag the titlebar to move the window")
