#this belongs in apps/components/Map_Editor/depends/map_win_func.py - Version: 3
# X-Seti - October 14 2026 - IMG Factory 1.6 - Map Workshop window frame

"""
Window frame, mouse/resize events, dock mode, status bar for ModelWorkshop (Map).
"""

##class MapWindowMixin: -
# _apply_button_mode_to_button
# _apply_window_flags
# closeEvent
# _create_status_bar
# _dock_to_main
# _get_memory_mb_stdlib
# _get_resize_corner
# _handle_corner_resize
# _initialize_features
# _is_on_draggable_area
# mouseDoubleClickEvent
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# _on_menu_btn_clicked
# paintEvent
# _refresh_icons
# resizeEvent
# _restore_dock_state
# _save_dock_state
# _show_amiga_locale_error
# showEvent
# _sync_middle_btn_row_visibility
# toggle_dock_mode
# _toggle_maximize
# _toggle_mid_btn_row_collapsed
# _toggle_tearoff
# _toggle_tex_btn_row_collapsed
# _undock_from_main
# _update_all_buttons
# _update_cursor
# _update_dock_button_visibility
# _update_memory_status_label
# _update_status_indicators
# _update_tex_btn_compact
# _update_transform_text_panel_visibility
# _update_zoom_status_label

import base64

from apps.methods.corner_handles import paint_corner_handles
from PyQt6.QtCore import QByteArray, QTimer, Qt
from PyQt6.QtWidgets import QPushButton

from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.components.Map_Editor.depends.map_widgets_func import App_name


class MapWindowMixin: #vers 1
    """Window frame, mouse/resize events, dock mode, status bar."""


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

    def _create_status_bar(self): #vers 2
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

        # Zoom level, left of memory usage (Aug 20 2026, .per:
        # "a zoom value display left of the memory usage on the
        # status bar, for the viewpoint")
        layout.addStretch()
        self.status_zoom_label = QLabel("")
        self.status_zoom_label.setToolTip("Current viewport zoom (camera distance from target)")
        layout.addWidget(self.status_zoom_label)
        self._zoom_status_timer = QTimer(self)
        self._zoom_status_timer.timeout.connect(self._update_zoom_status_label)
        self._zoom_status_timer.start(150)

        # Right: memory usage (Aug 1 2026)
        self.status_memory_label = QLabel("")
        self.status_memory_label.setToolTip("Current process memory usage (RSS)")
        layout.addWidget(self.status_memory_label)
        self._memory_status_timer = QTimer(self)
        self._memory_status_timer.timeout.connect(self._update_memory_status_label)
        self._memory_status_timer.start(2000)
        self._update_memory_status_label()

        return status_bar

    def _update_zoom_status_label(self): #vers 1
        """Refresh the status bar's zoom label (Aug 20 2026, per
         : "a zoom value display left of the memory usage on the
        status bar, for the viewpoint") - reads the viewport's own
        real camera-distance-from-target value (self._dist), the same
        real quantity every zoom in/out/wheel action already changes,
        rather than a separate, second notion of "zoom" invented for
        this display. Faster 150ms timer (vs memory's own 2000ms) so
        it visibly tracks scroll-wheel zooming live, not just settled
        afterwards."""
        label = getattr(self, 'status_zoom_label', None)
        if label is None:
            return
        vp = getattr(self, 'preview_widget', None)
        dist = getattr(vp, '_dist', None) if vp is not None else None
        label.setText(f"Zoom: {dist:.1f}" if dist is not None else "")

    def _update_memory_status_label(self): #vers 2
        """Refresh the status bar's memory usage label."""
        label = getattr(self, 'status_memory_label', None)
        if label is None:
            return
        mb = None
        try:
            import psutil
            mb = psutil.Process().memory_info().rss / (1024 * 1024)
        except ImportError:
            mb = self._get_memory_mb_stdlib()
        except Exception:
            pass
        label.setText(f"Memory: {mb:.0f} MB" if mb is not None else "")

    def _get_memory_mb_stdlib(self): #vers 1
        """Standard-library-only memory reading, used when psutil
        isn't installed - see _update_memory_status_label's docstring
        for the full per-platform reasoning. Returns None if nothing
        available works (should be rare - covers Linux/macOS/Windows,
        the only remaining gap would be an unusual/embedded Python
        build missing both `resource` and `ctypes`)."""
        import sys
        try:
            import resource
            rss = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
            return rss / (1024 * 1024) if sys.platform == 'darwin' else rss / 1024
        except ImportError:
            pass
        except Exception:
            pass
        if sys.platform == 'win32':
            try:
                import ctypes
                class PROCESS_MEMORY_COUNTERS(ctypes.Structure):
                    _fields_ = [
                        ("cb", ctypes.c_ulong),
                        ("PageFaultCount", ctypes.c_ulong),
                        ("PeakWorkingSetSize", ctypes.c_size_t),
                        ("WorkingSetSize", ctypes.c_size_t),
                        ("QuotaPeakPagedPoolUsage", ctypes.c_size_t),
                        ("QuotaPagedPoolUsage", ctypes.c_size_t),
                        ("QuotaPeakNonPagedPoolUsage", ctypes.c_size_t),
                        ("QuotaNonPagedPoolUsage", ctypes.c_size_t),
                        ("PagefileUsage", ctypes.c_size_t),
                        ("PeakPagefileUsage", ctypes.c_size_t),
                    ]
                counters = PROCESS_MEMORY_COUNTERS()
                counters.cb = ctypes.sizeof(PROCESS_MEMORY_COUNTERS)
                handle = ctypes.windll.kernel32.GetCurrentProcess()
                if ctypes.windll.psapi.GetProcessMemoryInfo(
                        handle, ctypes.byref(counters), counters.cb):
                    return counters.WorkingSetSize / (1024 * 1024)
            except Exception:
                pass
        return None


    def _refresh_icons(self): #vers 2
        """Refresh all button icons after theme change — picks up current text_primary colour."""
        SVGIconFactory.clear_cache()
        c = self._get_icon_color()
        SVGIconFactory.set_theme_color(c)

        # Map: attribute name → icon factory method
        _icon_map = [
            # Toolbar
            ('settings_btn',       'settings_icon'),
            ('open_btn',           'open_icon'),
            ('open_dff_btn',       'open_icon'),
            ('open_txd_btn',       'open_icon'),
            ('tex_load_btn',       'open_icon'),
            ('tex_browse_btn',     'folder_icon'),
            ('tex_pass_btn',       'export_icon'),
            ('tex_save_btn',       'save_icon'),
            ('save_btn',           'save_icon'),
            ('saveall_btn',        'saveas_icon'),
            ('export_all_btn',     'package_icon'),
            ('undo_btn',           'undo_icon'),
            ('info_btn',           'info_icon'),
            ('minimize_btn',       'minimize_icon'),
            ('maximize_btn',       'maximize_icon'),
            ('close_btn',          'close_icon'),
            ('open_img_btn',       'folder_icon'),
            ('from_img_btn',       'open_icon'),
            # Middle panel mini-toolbar
            ('open_col_btn',       'open_icon'),
            ('save_col_btn',       'save_icon'),
            ('export_col_btn',     'package_icon'),
            ('undo_col_btn',       'undo_icon'),
            # Transform icon panel
            ('flip_vert_btn',      'flip_vert_icon'),
            ('flip_horz_btn',      'flip_horz_icon'),
            ('rotate_cw_btn',      'rotate_cw_icon'),
            ('rotate_ccw_btn',     'rotate_ccw_icon'),
            ('analyze_btn',        'analyze_icon'),
            ('copy_btn',           'copy_icon'),
            ('paste_btn',          'paste_icon'),
            ('create_surface_btn', 'add_icon'),
            ('delete_surface_btn', 'remove_icon'),
            ('import_btn',         'import_icon'),
            ('export_btn',         'export_icon'),
            # Right panel toolbar buttons
            ('switch_btn',         'flip_vert_icon'),
            ('convert_btn',        'convert_icon'),
            ('properties_btn',     'settings_icon'),
        ]
        for attr, method in _icon_map:
            btn = getattr(self, attr, None)
            if btn is None:
                continue
            fn = getattr(self.icon_factory, method, None)
            if fn is None:
                continue
            try:
                btn.setIcon(fn(color=c))
            except TypeError:
                try:
                    btn.setIcon(fn())
                except Exception:
                    pass


# - Settings Reusable

        # Sync mini toolbar visibility with current dock state
        self._sync_middle_btn_row_visibility()
        self._apply_custom_icons()

    def _sync_middle_btn_row_visibility(self): #vers 2
        """Show/hide the mini toolbar's action icons (Open/Save/Import/
        Export/Undo/3D View) based on dock state AND collapse state - the
        row itself and its view-toggle/collapse buttons always stay
        visible regardless."""
        if not hasattr(self, '_middle_btn_row'):
            return
        docked_only = self.is_docked and not self.standalone_mode
        collapsed = getattr(self, '_mid_btn_row_collapsed', False)
        show = docked_only and not collapsed
        for attr in ('open_col_btn', 'save_col_btn', 'import_btn',
                     'export_col_btn', 'undo_col_btn', '_mini_3d_btn'):
            b = getattr(self, attr, None)
            if b:
                b.setVisible(show)

    def _toggle_mid_btn_row_collapsed(self): #vers 1
        """Collapse/expand the mini toolbar's action icons via the -/+
        button, independent of the icon-only/icon+text adaptive display."""
        self._mid_btn_row_collapsed = not getattr(self, '_mid_btn_row_collapsed', False)
        self.mid_btn_collapse_btn.setText("+" if self._mid_btn_row_collapsed else "−")
        self._sync_middle_btn_row_visibility()

    def _on_menu_btn_clicked(self): #vers 1
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        if hasattr(self, '_build_menus_into_qmenu'):
            self._build_menus_into_qmenu(menu)
        btn = getattr(self, 'menu_btn', None)
        if btn: menu.exec(btn.mapToGlobal(btn.rect().bottomLeft()))


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


# - Docking functions

    def _update_dock_button_visibility(self): #vers 3
        """Show/hide dock and tearoff buttons based on docked state"""
        if hasattr(self, 'dock_btn'):
            # Hide D button when docked, show when standalone
            self.dock_btn.setVisible(not self.is_docked)

        if hasattr(self, 'tearoff_btn'):
            # T button only visible when docked and not in standalone mode
            self.tearoff_btn.setVisible(self.is_docked and not self.standalone_mode)

        if hasattr(self, '_ipl_controls_settings_btn'):
            # IPL Controls cog icon, same visibility rule as tearoff_btn (Aug 15 2026)
            self._ipl_controls_settings_btn.setVisible(self.is_docked and not self.standalone_mode)


    def toggle_dock_mode(self): #vers 2
        """Toggle between docked and standalone mode"""
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

        self._update_dock_button_visibility()


    def _dock_to_main(self): #vers 11
        """Return a torn-off workshop to its IMG Factory tab."""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                self.show()
                self.raise_()
                return

            # For proper docking, we need to be called from imgfactory
            # This method should be handled by imgfactory's overlay system
            from apps.methods.dock_panels import redock_to_tab
            if not redock_to_tab(self):      # no tab to return to
                return

            # Update dock state
            self.is_docked = True
            self._update_dock_button_visibility()
            self._sync_middle_btn_row_visibility()

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} docked to main window")


        except Exception as e:
            print(f"Error docking: {str(e)}")
            self.show()


    def _undock_from_main(self): #vers 5
        """Undock from overlay mode to standalone window - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                # Switch from overlay to normal window
                self.setWindowFlags(Qt.WindowType.Window)
                self.is_overlay = False
                self.overlay_table = None

            # Set proper window flags for standalone mode
            self.setWindowFlags(Qt.WindowType.Window)
            
            # Ensure proper size when undocking
            if hasattr(self, 'original_size'):
                self.resize(self.original_size)
            else:
                self.resize(1000, 700)  # Reasonable default size
                
            self.is_docked = False
            self._update_dock_button_visibility()
            self._sync_middle_btn_row_visibility()

            self.show()
            self.raise_()

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} undocked to standalone")
                
        except Exception as e:
            print(f"Error undocking: {str(e)}")
            # Fallback
            self.setWindowFlags(Qt.WindowType.Window)
            self.show()



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

    def _apply_button_mode_to_button(self, button, text): #vers 2
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




    def _update_transform_text_panel_visibility(self): #vers 3
        """DockableToolbar version — toolbars manage their own visibility."""
        btr = getattr(self, '_bottom_text_row', None)
        bir = getattr(self, '_bottom_icon_row', None)
        if btr or bir:
            rp = getattr(self, '_right_panel_ref', None)
            ref_w = rp.width() if rp else self.width()
            try:
                from apps.methods.imgfactory_ui_settings import get_collapse_threshold
                threshold = get_collapse_threshold(getattr(self, 'main_window', None))
            except Exception:
                threshold = 550
            wide = ref_w >= threshold
            if btr: btr.setVisible(wide)
            if bir: bir.setVisible(not wide)

    def showEvent(self, event): #vers 2
        """On first show, mark shown. Toolbar-layout restore itself is
        handled by _restore_toolbar_state (scheduled from _build_toolbars)."""
        super().showEvent(event)
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

    def _update_tex_btn_compact(self): #vers 2
        """Icon-only when texture panel is narrow — delegates to shared helper."""
        panel = getattr(self, '_tex_panel', None)
        if not panel or not panel.isVisible():
            return
        try:
            from apps.methods.imgfactory_ui_settings import apply_compact_buttons
            meta = [(getattr(self, attr, None), label)
                    for attr, label, _ in getattr(self, '_tex_btns_meta', [])]
            apply_compact_buttons(meta, panel.width())
        except Exception:
            pass

    def _toggle_tex_btn_row_collapsed(self): #vers 1
        """Collapse/expand the texture panel's Load/Texlist/WShop/Save
        buttons via the -/+ button, independent of the icon-only/icon+text
        adaptive display."""
        self._tex_btn_row_collapsed = not getattr(self, '_tex_btn_row_collapsed', False)
        self.tex_btn_collapse_btn.setText("+" if self._tex_btn_row_collapsed else "−")
        show = not self._tex_btn_row_collapsed
        for attr, _label, _icon in getattr(self, '_tex_btns_meta', []):
            b = getattr(self, attr, None)
            if b:
                b.setVisible(show)


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


    def closeEvent(self, event): #vers 6
        """Handle close — asks about unsaved IPL edits first; _save_toolbar_state fires via window_closed
        signal. Flushes any still-pending debounced settings save
        (Aug 16 2026)

         fix (Aug 20 2026,  "remember the zoom settings,
        and view location when app is closed") - saves the viewport's
        own real camera state (zoom/_dist, pan_x/pan_y, yaw/pitch)
        here on close specifically, rather than on every zoom/pan/
        rotate change - those fire on every single mouse-drag frame,
        so debouncing wouldn't meaningfully reduce the write volume
        the way it does for a settings-dialog Apply click."""
        if not self.confirm_close():
            event.ignore()
            return
        vp = getattr(self, 'preview_widget', None)
        if vp is not None and hasattr(self, 'map_settings'):
            try:
                self.map_settings.set('viewport_dist', getattr(vp, '_dist', None))
                self.map_settings.set('viewport_pan_x', getattr(vp, '_pan_x', None))
                self.map_settings.set('viewport_pan_y', getattr(vp, '_pan_y', None))
                self.map_settings.set('viewport_yaw', getattr(vp, '_yaw', None))
                self.map_settings.set('viewport_pitch', getattr(vp, '_pitch', None))
            except Exception:
                pass
        self._save_dock_state()
        if hasattr(self, 'map_settings'):
            try:
                self.map_settings.save()
            except Exception:
                pass
        self.window_closed.emit()
        try:
            mw = getattr(self, 'main_window', None) or getattr(self, '_imgfactory', None)
            if mw and hasattr(mw, '_update_tool_menu_for_tab'):
                mw._update_tool_menu_for_tab(None)
        except Exception:
            pass
        event.accept()

    def _save_dock_state(self): #vers 1
        """Capture the current dock layout - every dock's own position,
        size, floating state, and tab grouping/order within a shared
        area - via Qt's own built-in QMainWindow.saveState(), rather
        than hand-tracking each dock's own geometry separately (Aug 19
        2026)"""
        inner_mw = getattr(self, '_inner_mw', None)
        if inner_mw is None or not hasattr(self, 'map_settings'):
            return
        try:
            state_bytes = bytes(inner_mw.saveState())
            encoded = base64.b64encode(state_bytes).decode('ascii')
            self.map_settings.set('workshop_dock_state', encoded)
        except Exception:
            pass

    def _restore_dock_state(self): #vers 1
        """Restore a previously-saved dock layout, if one exists (Aug
        19 2026)"""
        inner_mw = getattr(self, '_inner_mw', None)
        if inner_mw is None or not hasattr(self, 'map_settings'):
            return
        encoded = self.map_settings.get('workshop_dock_state')
        if not encoded:
            return
        try:
            state_bytes = base64.b64decode(encoded.encode('ascii'))
            inner_mw.restoreState(QByteArray(state_bytes))
        except Exception:
            pass


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
