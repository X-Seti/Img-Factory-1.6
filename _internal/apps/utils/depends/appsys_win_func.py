#this belongs in apps/utils/depends/appsys_win_func.py - Version: 2
# X-Seti - October 20 2026 - IMG Factory 1.6 - Settings Dialog Window

"""
Settings dialog titlebar, move, resize, corners, maximize.
Mixin for SettingsDialog in apps/utils/app_settings_system.py.
"""

##class AppsysWinMixin: -
# _apply_dialog_window_mode
# _create_dialog_titlebar
# _get_icon_color
# _get_resize_corner
# _get_resize_direction
# _handle_corner_resize
# _handle_resize
# _is_on_draggable_area
# _launch_theme_settings
# _show_dialog_info
# _show_dialog_self_settings
# _show_workshop_settings
# _titlebar_actions
# _titlebar_double_click
# _titlebar_info
# _titlebar_menu
# _titlebar_mouse_move
# _titlebar_mouse_press
# _titlebar_title
# _toggle_dialog_maximize
# _toggle_maximize
# _update_cursor
# _update_titlebar_icons
# mouseDoubleClickEvent
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# paintEvent
# resizeEvent

import json
import os
import random
from pathlib import Path

from PyQt6.QtCore import (Qt, pyqtSignal, QTimer, QThread, pyqtSlot, QRect, QDateTime,
                          QByteArray, QPoint, QSize)
from PyQt6.QtGui import QFont, QColor, QPixmap, QPainter, QCursor, QIcon
from apps.methods.titlebar_func import TitlebarMixin
from PyQt6.QtWidgets import (
    QToolButton, QSizePolicy, QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout,
    QGridLayout, QButtonGroup, QRadioButton, QLabel, QPushButton,
    QComboBox, QCheckBox, QSpinBox, QMenu, QSlider, QSplitter,
    QGroupBox, QTabWidget, QDialog, QMessageBox,
    QFileDialog, QColorDialog, QFontDialog, QTextEdit,
    QTableWidget, QTableWidgetItem, QHeaderView, QFontComboBox,
    QScrollArea, QFrame, QLineEdit, QListWidget
)


class AppsysWinMixin(TitlebarMixin): #vers 2
    """Settings dialog titlebar, move, resize, corners, maximize."""

    def _apply_dialog_window_mode(self): #vers 1
        """Apply custom window gadgets to dialog if enabled"""
        use_custom = self.app_settings.current_settings.get("use_custom_gadgets", False)

        if use_custom:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint | Qt.WindowType.Dialog)


    def _get_resize_corner(self, pos): #vers 2
        """Determine which corner is under mouse position"""
        size = self.corner_size
        w = self.width()
        h = self.height()

        # Top-left corner
        if pos.x() < size and pos.y() < size:
            return "top-left"

        # Top-right corner
        if pos.x() > w - size and pos.y() < size:
            return "top-right"

        # Bottom-left corner
        if pos.x() < size and pos.y() > h - size:
            return "bottom-left"

        # Bottom-right corner
        if pos.x() > w - size and pos.y() > h - size:
            return "bottom-right"

        return None


    def _update_titlebar_icons(self): #vers 2
        """Update titlebar icons when theme changes"""
        if not hasattr(self, 'custom_titlebar') and not hasattr(self, 'dialog_titlebar'):
            return

        # Clear icon cache and recreate provider
        if hasattr(self, 'icons'):
            self.icons.clear_cache()

        # Force refresh all titlebar button icons
        titlebar = getattr(self, 'custom_titlebar', None) or getattr(self, 'dialog_titlebar', None)
        if titlebar:
            for button in titlebar.findChildren(QPushButton):
                tooltip = button.toolTip()
                if tooltip == "Settings":
                    button.setIcon(self.icons.settings_icon(force_refresh=True))
                elif tooltip == "Minimize":
                    button.setIcon(self.icons.minimize_icon(force_refresh=True))
                elif tooltip == "Maximize":
                    if self.isMaximized():
                        button.setIcon(self.icons.restore_icon(force_refresh=True))
                    else:
                        button.setIcon(self.icons.maximize_icon(force_refresh=True))
                elif tooltip == "Close":
                    button.setIcon(self.icons.close_icon(force_refresh=True))


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


    def resizeEvent(self, event): #vers 2
        '''Keep resize grip in bottom-right corner'''
        super().resizeEvent(event)
        if hasattr(self, 'size_grip'):
            self.size_grip.move(self.width() - 16, self.height() - 16)


    def mouseDoubleClickEvent(self, event): #vers 2
        """Handle double-click on toolbar to maximize/restore"""
        if event.button() == Qt.MouseButton.LeftButton:
            if self._is_on_draggable_area(event.pos()):
                self._toggle_maximize()
                event.accept()
            else:
                super().mouseDoubleClickEvent(event)


    def _toggle_maximize(self): #vers 2
        """Toggle window maximize state"""
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()


    def _is_on_draggable_area(self, pos): #vers 5
        """Check if position is on the draggable titlebar area.
        Works for both CustomWindow (self.toolbar) and SettingsDialog
        (self.dialog_titlebar). Returns True if pos is inside the titlebar
        but NOT over any button — i.e. the stretch/label area.
        """
        # Find the titlebar widget — toolbar for main window, dialog_titlebar for dialog
        titlebar = (getattr(self, 'toolbar', None) or
                    getattr(self, 'dialog_titlebar', None) or
                    getattr(self, 'custom_titlebar', None))
        if not titlebar:
            return False

        titlebar_rect = titlebar.geometry()
        if not titlebar_rect.contains(pos):
            return False

        # Convert to titlebar-local coordinates
        local_pos = titlebar.mapFrom(self, pos)

        # Collect all child buttons — any click NOT on a button is draggable
        from PyQt6.QtWidgets import QPushButton, QComboBox, QCheckBox
        for child in titlebar.findChildren(QPushButton):
            if child.geometry().contains(local_pos):
                return False
        for child in titlebar.findChildren(QComboBox):
            if child.geometry().contains(local_pos):
                return False

        return True


    def _create_dialog_titlebar(self): #vers 8
        """Shared workshop titlebar (methods/titlebar_func.py) with dialog hooks."""
        self.standalone_mode = True                     # dialog is always a window
        self._create_toolbar()
        self.dialog_titlebar = self.toolbar
        self._dialog_menu_btn = self.menu_btn
        self._dialog_settings_btn = self.settings_btn
        self._dialog_title_lbl = self.title_label
        self._dialog_info_btn = self.info_btn
        self._dialog_min_btn = self.minimize_btn
        self._dialog_max_btn = self.dialog_maximize_btn = self.maximize_btn
        self._dialog_close_btn = self.close_btn
        self.properties_btn.setToolTip("Theme colours")

        #   Drag: startSystemMove on the stretch/title area, not on buttons
        self.dialog_titlebar.setMouseTracking(True)

        def _tb_press(event):
            if event.button() == Qt.MouseButton.LeftButton:
                local = event.pos()
                for child in self.dialog_titlebar.findChildren((QPushButton, QToolButton)):
                    if child.geometry().contains(local):
                        return
                try:
                    self.windowHandle().startSystemMove()
                except Exception:
                    self._tb_drag_pos = (event.globalPosition().toPoint()
                                         - self.frameGeometry().topLeft())

        def _tb_move(event):
            if (event.buttons() == Qt.MouseButton.LeftButton and
                    hasattr(self, '_tb_drag_pos')):
                self.move(event.globalPosition().toPoint() - self._tb_drag_pos)

        def _tb_dbl(event):
            self._toggle_dialog_maximize()

        self.dialog_titlebar.mousePressEvent  = _tb_press
        self.dialog_titlebar.mouseMoveEvent   = _tb_move
        self.dialog_titlebar.mouseDoubleClickEvent = _tb_dbl

    def _get_icon_color(self): #vers 1
        """Titlebar icon colour: theme text_primary."""
        return (self.app_settings.get_theme_colors() or {}).get('text_primary', '#cccccc')

    def _launch_theme_settings(self): #vers 1
        """Theme button: show the Colors tab."""
        self.tabs.setCurrentWidget(self.color_picker_tab)

    def _show_workshop_settings(self): #vers 1
        """Titlebar Settings button: dialog appearance."""
        self._show_dialog_self_settings()

    def _titlebar_actions(self, layout): #vers 1
        """Undo colour change button, right of title."""
        self._dialog_undo_btn = self._tb_window_button('undo_icon', "Nothing to undo",
                                                       self._undo_color_change)
        self._dialog_undo_btn.setEnabled(False)
        layout.addWidget(self._dialog_undo_btn)

    def _titlebar_info(self): #vers 1
        """Info button."""
        self._show_dialog_info()

    def _titlebar_menu(self, menu): #vers 1
        """Menu button drop-down."""
        menu.addAction("Reset to defaults",  self._reset_all_settings)
        menu.addAction("Import theme…",      lambda: None)
        menu.addAction("Export theme…",      lambda: None)
        menu.addSeparator()
        menu.addAction("Close",              self.reject)

    def _titlebar_title(self): #vers 1
        """Title: app name and build."""
        from apps.utils.app_settings_system import App_build, App_name
        app_name = (getattr(self.parent(), 'app_name', None) or
                    getattr(self.parent(), 'App_name', None) or
                    App_name) if self.parent() else App_name
        try:
            from apps.app_info import get_full_build as _gfb
            _build_str = _gfb()
        except ImportError:
            _build_str = f"v{App_build}"
        return f"{app_name}  —  {_build_str}"


    def _show_dialog_self_settings(self): #vers 1
        """Mini dialog for the settings dialog's own appearance — font, titlebar height."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout,
                                      QLabel, QSpinBox, QFontComboBox,
                                      QDialogButtonBox, QGroupBox)
        d = QDialog(self)
        d.setWindowTitle("Dialog Appearance")
        d.setMinimumWidth(340)
        lay = QVBoxLayout(d)

        font_group = QGroupBox("Dialog Font")
        font_group.setAutoFillBackground(True)
        fgl = QHBoxLayout(font_group)
        fgl.addWidget(QLabel("Family:"))
        fc = QFontComboBox()
        fc.setCurrentFont(__import__('PyQt6.QtGui', fromlist=['QFont']).QFont(
            self.app_settings.current_settings.get('font_family', 'Arial')))
        fgl.addWidget(fc)
        fgl.addWidget(QLabel("Size:"))
        fs = QSpinBox(); fs.setRange(7, 16)
        fs.setValue(self.app_settings.current_settings.get('font_size', 9))
        fgl.addWidget(fs)
        lay.addWidget(font_group)

        tb_group = QGroupBox("Titlebar")
        tb_group.setAutoFillBackground(True)
        tgl = QHBoxLayout(tb_group)
        tgl.addWidget(QLabel("Height:"))
        tbs = QSpinBox(); tbs.setRange(28, 60)
        tbs.setValue(self.app_settings.current_settings.get(
            'settings_dialog_titlebar_height', 40))
        tbs.setSuffix(" px")
        tgl.addWidget(tbs)
        lay.addWidget(tb_group)

        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                              QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(d.accept)
        bb.rejected.connect(d.reject)
        lay.addWidget(bb)

        if d.exec():
            self.app_settings.current_settings['font_family'] = fc.currentFont().family()
            self.app_settings.current_settings['font_size']   = fs.value()
            self.app_settings.current_settings['settings_dialog_titlebar_height'] = tbs.value()
            # Apply titlebar height immediately
            if hasattr(self, 'dialog_titlebar'):
                self.dialog_titlebar.setFixedHeight(tbs.value())
            self.setStyleSheet(self.app_settings.get_stylesheet())


    def _show_dialog_info(self): #vers 1
        """Show brief info about the settings dialog."""
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(self, "Settings",
            "App System Settings\n\n"
            "Colors, Fonts, Buttons, Panels, Gadgets, Shadows,\n"
            "UI Management, Interface, Localisation, Debug.\n\n"
            "Changes take effect immediately unless noted.")


    def paintEvent(self, event): #vers 3
        """Corner handles, COL style (methods/corner_handles.py)."""
        super().paintEvent(event)
        from apps.methods.corner_handles import paint_corner_handles
        paint_corner_handles(self)


    def _titlebar_double_click(self, event): #vers 1
        """Handle double click on title bar - maximize/restore"""
        if event.button() == Qt.MouseButton.LeftButton:
            self._toggle_dialog_maximize()
            event.accept()


    def _toggle_dialog_maximize(self): #vers 1
        """Toggle dialog maximize state"""
        if self.isMaximized():
            self.showNormal()
            if hasattr(self, 'dialog_maximize_btn'):
                self.dialog_maximize_btn.setIcon(self.icons.maximize_icon())
                self.dialog_maximize_btn.setToolTip("Maximize")
        else:
            self.showMaximized()
            if hasattr(self, 'dialog_maximize_btn'):
                self.dialog_maximize_btn.setIcon(self.icons.restore_icon())
                self.dialog_maximize_btn.setToolTip("Restore")


    def _titlebar_mouse_press(self, event): #vers 1
        """Handle mouse press on title bar"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.titlebar_drag_position = event.globalPosition().toPoint() - self.frameGeometry().topLeft()
            event.accept()


    def _titlebar_mouse_move(self, event): #vers 1
        """Handle mouse move on title bar - window dragging"""
        if event.buttons() == Qt.MouseButton.LeftButton and self.titlebar_drag_position:
            self.move(event.globalPosition().toPoint() - self.titlebar_drag_position)
            event.accept()


    def mousePressEvent(self, event): #vers 1
        """Handle mouse press for resizing"""
        if event.button() == Qt.MouseButton.LeftButton:
            if self.app_settings.current_settings.get("enable_corner_resize", True):
                self.resize_direction = self._get_resize_direction(event.pos())
                if self.resize_direction:
                    self.drag_position = event.globalPosition().toPoint()
                    self.initial_geometry = self.geometry()
                    event.accept()
                    return
        super().mousePressEvent(event)


    def mouseMoveEvent(self, event): #vers 1
        """Handle mouse move for resizing and cursor updates"""
        if self.app_settings.current_settings.get("enable_corner_resize", True):
            if event.buttons() == Qt.MouseButton.LeftButton and self.resize_direction:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept()
                return
            else:
                direction = self._get_resize_direction(event.pos())
                self._update_cursor(direction)

        super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event): #vers 1
        """Handle mouse release"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.resize_direction = None
            self.drag_position = None
            self.initial_geometry = None
        super().mouseReleaseEvent(event)


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


    def _handle_corner_resize(self, global_pos): #vers 2
        """Handle window resizing from any edge or corner"""
        if not self.resize_direction or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.initial_geometry

        min_width = 800
        min_height = 600

        # Create a copy of the geometry
        new_geometry = QRect(geometry.x(), geometry.y(), geometry.width(), geometry.height())

        if "left" in self.resize_direction:
            new_x = geometry.x() + delta.x()
            new_width = geometry.width() - delta.x()
            if new_width >= min_width:
                new_geometry.setLeft(new_x)

        if "right" in self.resize_direction:
            new_width = geometry.width() + delta.x()
            if new_width >= min_width:
                new_geometry.setWidth(new_width)

        if "top" in self.resize_direction:
            new_y = geometry.y() + delta.y()
            new_height = geometry.height() - delta.y()
            if new_height >= min_height:
                new_geometry.setTop(new_y)

        if "bottom" in self.resize_direction:
            new_height = geometry.height() + delta.y()
            if new_height >= min_height:
                new_geometry.setHeight(new_height)

        self.setGeometry(new_geometry)
