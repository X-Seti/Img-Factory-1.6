#this belongs in apps/components/Model_Editor/depends/model_win_func.py - Version: 7
# X-Seti - October 12 2026 - IMG Factory 1.6 - Model Workshop window frame

"""
Window frame, dragging, corner resize, status bar, window events.
"""

##class ModelWindowMixin: -
# _apply_button_mode_to_button
# _apply_window_flags
# _create_status_bar
# dragEnterEvent
# dragMoveEvent
# _dropped_files
# dropEvent
# _enable_move_mode
# _enable_name_edit
# _get_resize_corner
# _handle_corner_resize
# _initialize_features
# _is_on_draggable_area
# mouseDoubleClickEvent
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# paintEvent
# resizeEvent
# _set_icon_display_mode
# _set_status
# _show_amiga_locale_error
# showEvent
# _toggle_maximize
# _toggle_tearoff
# _update_all_buttons
# _update_cursor
# _update_status_indicators

import os
from apps.methods.corner_handles import paint_corner_handles
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont
from PyQt6.QtWidgets import QDialog, QFrame, QHBoxLayout, QLabel, QMessageBox, QPushButton, QVBoxLayout
from apps.components.Model_Editor.depends.model_setup_ui_func import App_name

class ModelWindowMixin: #vers 1
    """Window frame, dragging, corner resize, status bar, window events."""


    def _dropped_files(self, event): #vers 1
        """Local model / COL / TXD / IMG paths carried by a drag event."""
        from apps.methods.dff_parser import MODEL_EXTS
        exts = MODEL_EXTS + ('.col', '.img', '.txd', '.wtd', '.nft', '.chk', '.xtx')
        md = event.mimeData()
        if not md.hasUrls():
            return []
        return [u.toLocalFile() for u in md.urls()
                if u.isLocalFile() and u.toLocalFile().lower().endswith(exts)]

    def dragEnterEvent(self, event): #vers 1
        """Accept supported files."""
        if self._dropped_files(event):
            event.acceptProposedAction()
        else:
            event.ignore()

    def dragMoveEvent(self, event): #vers 1
        """Keep accepting while over the workshop."""
        self.dragEnterEvent(event)

    def dropEvent(self, event): #vers 2
        """Open dropped files; with a file loaded ask replace or new tab."""
        paths = self._dropped_files(event)
        if not paths:
            event.ignore()
            return
        event.acceptProposedAction()
        if self._doc_has_file():
            names = ", ".join(os.path.basename(p) for p in paths[:3]) + (" ..." if len(paths) > 3 else "")
            box = QMessageBox(self)
            box.setWindowTitle("Dropped Files")
            box.setText(f"{names}\n\nReplace the open file, or open in new tab(s)?")
            rep_btn = box.addButton("Replace", QMessageBox.ButtonRole.AcceptRole)
            new_btn = box.addButton("Open in New Tab", QMessageBox.ButtonRole.ActionRole)
            box.addButton(QMessageBox.StandardButton.Cancel)
            box.exec()
            if box.clickedButton() is new_btn:
                for path in paths:
                    self._open_new_doc(path)
                return
            if box.clickedButton() is not rep_btn:
                return
        for path in paths:
            self._open_path(path)

    def _set_status(self, msg: str): #vers 1
        """Write msg to the status label (whichever one exists)."""
        if hasattr(self, 'status_label'):
            self.status_label.setText(msg)
        elif hasattr(self, 'status_bar') and hasattr(self.status_bar, 'showMessage'):
            self.status_bar.showMessage(msg, 3000)
        else:
            print(f"[COL] {msg}")


    def _enable_name_edit(self, event, is_alpha): #vers 1
        """Enable name editing on click"""
        self.info_name.setReadOnly(False)
        self.info_name.selectAll()
        self.info_name.setFocus()


    def _update_status_indicators(self): #vers 2
        """Update status indicators"""
        if hasattr(self, 'status_collision'):
            self.status_textures.setText(f"collision: {len(self.collision_list)}")

        if hasattr(self, 'status_selected'):
            if self.selected_texture:
                name = self.selected_collision.get('name', 'Unknown')
                self.status_selected.setText(f"Selected: {name}")
            else:
                self.status_selected.setText("Selected: None")

        if hasattr(self, 'status_size'):
            if self.current_txd_data:
                size_kb = len(self.current_col_data) / 1024
                self.status_size.setText(f"COL Size: {size_kb:.1f} KB")
            else:
                self.status_size.setText("COL Size: Unknown")

        if hasattr(self, 'status_modified'):
            if self.windowTitle().endswith("*"):
                self.status_modified.setText("MODIFIED")
                self.status_modified.setStyleSheet("color: orange; font-weight: bold;")
            else:
                self.status_modified.setText("")
                self.status_modified.setStyleSheet("")
# - Panel Creation

    def _create_status_bar(self): #vers 1
        """Create bottom status bar - single line compact"""
        from PyQt6.QtWidgets import QFrame, QHBoxLayout, QLabel

        status_bar = QFrame()
        status_bar.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        status_bar.setFixedHeight(22)

        layout = QHBoxLayout(status_bar)
        layout.setContentsMargins(5, 0, 5, 0)
        layout.setSpacing(15)

        # Left: Ready
        self.status_label = QLabel("Ready")
        layout.addWidget(self.status_label)

        return status_bar


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




    def _show_amiga_locale_error(self): #vers 2
        """Show Amiga Workbench 3.1 style error dialog"""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton, QHBoxLayout
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle("Workbench Request")
        dialog.setFixedSize(450, 150)

        # Amiga Workbench styling
        dialog.setStyleSheet("""
            QDialog {
                background-color: palette(placeholderText);
                border: 2px solid palette(buttonText);
            }
            QLabel {
                color: palette(windowText);
                background-color: palette(placeholderText);
            }
            QPushButton {
                background-color: palette(midlight);
                color: palette(windowText);
                border: 2px outset palette(buttonText);
                padding: 5px 15px;
                min-width: 80px;
            }
            QPushButton:pressed {
                border: 2px inset palette(mid);
            }
        """)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(15)
        layout.setContentsMargins(20, 20, 20, 20)

        # Amiga Topaz font style
        amiga_font = QFont("Courier", 10, QFont.Weight.Normal)

        # Error message
        message = QLabel("Workbench 3.1 installer\n\nPlease insert Local disk in any drive")
        message.setFont(amiga_font)
        message.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(message)

        layout.addStretch()

        # Button layout
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        # Retry and Cancel buttons (Amiga style)
        retry_btn = QPushButton("Retry")
        retry_btn.setFont(amiga_font)
        retry_btn.clicked.connect(dialog.accept)
        button_layout.addWidget(retry_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.setFont(amiga_font)
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        button_layout.addStretch()
        layout.addLayout(button_layout)

        dialog.exec()



# - Window functionality

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

    def paintEvent(self, event): #vers 4
        """Corner handles, COL style (methods/corner_handles.py)."""
        super().paintEvent(event)
        paint_corner_handles(self)


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

    def showEvent(self, event): #vers 4
        """Fill an empty files panel (game folder or open IMG tabs), else follow IMG tabs.
        Toolbar-layout restore is handled by _restore_toolbar_state."""
        super().showEvent(event)
        lw = getattr(self, 'col_list_widget', None)
        if self.main_window is not None and lw is not None and not lw.count():
            self._populate_left_panel_from_open_imgs()
        else:
            self._sync_open_imgs()
        if not getattr(self, '_toolbar_layout_loaded', False):
            self._toolbar_layout_loaded = True

    def resizeEvent(self, event): #vers 6
        """Keep resize grip in corner; auto-collapse panels; adaptive button display."""
        super().resizeEvent(event)
        if hasattr(self, 'size_grip'):
            self.size_grip.move(self.width() - 16, self.height() - 16)
        self._update_transform_text_panel_visibility()
        self._update_tex_btn_compact()
        # Auto icon-only for middle button row when panel is narrow
        try:
            from apps.methods.imgfactory_ui_settings import apply_compact_buttons
            mid_btns = getattr(self, '_mid_compact_btns', [])
            if mid_btns:
                row = getattr(self, '_middle_btn_row', None)
                w = row.width() if (row and row.width() > 0) else self.width()
                apply_compact_buttons(mid_btns, w, compact_threshold=320)
        except Exception:
            pass
        # Auto icon-only for title bar buttons (Open/Save/Import/Export/
        # Undo) when the toolbar is narrow - docked mode especially, where
        # the tab gets a lot less width than a standalone window.
        try:
            from apps.methods.imgfactory_ui_settings import apply_compact_buttons
            tb_btns = getattr(self, '_titlebar_compact_btns', [])
            if tb_btns:
                row = getattr(self, 'toolbar', None)
                w = row.width() if (row and row.width() > 0) else self.width()
                apply_compact_buttons(tb_btns, w, compact_threshold=520)
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

# - Marker 3

    def _toggle_maximize(self): #vers 1
        """Toggle window maximize state"""
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()


  #  def setup_col_table_structure(workshop): pass
  #  def populate_col_table(workshop, col_file):
  #      for model in col_file.models:
  #          print(f"Model: {model.header.name}")

# - Rest of the logic for the panels









# - Marker 5

    def _toggle_tearoff(self): #vers 2
        """Toggle tear-off state (merge back to IMG Factory) - IMPROVED"""
        try:
            if self.is_docked:
                # Undock from main window
                self._undock_from_main()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{App_name} torn off from main window")
            else:
                # Dock back to main window
                self._dock_to_main()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{App_name} docked back to main window")
                    
        except Exception as e:
            print(f"Error toggling tear-off: {str(e)}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Tear-off Error", f"Could not toggle tear-off state:\n{str(e)}")


    def _set_icon_display_mode(self, mode: str): #vers 1
        """Set button display mode: both, icons or text."""
        if mode != self.button_display_mode:
            self.button_display_mode = mode
            self._update_all_buttons()
    def _enable_move_mode(self): #vers 2
        """Enable move window mode using system move"""
        handle = self.windowHandle()
        if handle and hasattr(handle, 'startSystemMove'):
            handle.startSystemMove()
