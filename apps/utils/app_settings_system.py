#!/usr/bin/env python3
# $vers" X-Seti - June26, 2025 - App Factory - Package theme settings

"""
App Factory - App Settings System - Clean Version
Settings management without demo code
"""

#This goes in root/ app_settings_system.py - version 62

import json
import os
from pathlib import Path
from PyQt6.QtCore import Qt, pyqtSignal, QDateTime  # Fixed: Added QDateTime
from PyQt6.QtGui import QFont

from PyQt6.QtWidgets import (
    QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout,
    QGridLayout, QButtonGroup, QRadioButton, QLabel, QPushButton,
    QComboBox, QCheckBox, QSpinBox, QMenu, QSlider, QSplitter,
    QLabel, QPushButton, QComboBox, QCheckBox, QSpinBox,
    QSlider, QGroupBox, QTabWidget, QDialog, QMessageBox,
    QFileDialog, QColorDialog, QFontDialog, QTextEdit,
    QTableWidget, QTableWidgetItem, QHeaderView, QFontComboBox,
    QScrollArea, QFrame, QLineEdit, QListWidget
)

from PyQt6.QtCore import Qt, pyqtSignal, QTimer, QThread, pyqtSlot, QRect, QDateTime, QByteArray
from PyQt6.QtGui import QFont, QColor, QPixmap, QPainter, QCursor


# Check for screen capture libraries (for robust color picking)
try:
    import mss
    MSS_AVAILABLE = True
    print("MSS library available")
except ImportError:
    MSS_AVAILABLE = False
    try:
        from PIL import ImageGrab
        PIL_AVAILABLE = True
        print("MSS not available, using PIL fallback")
    except ImportError:
        PIL_AVAILABLE = False
        print("Neither MSS nor PIL available - using Qt fallback")


##Methods to add to SettingsDialog class
# _create_buttons_tab (vers 1)
# _create_button_panel_editor (vers 1)
# _get_default_button_colors (vers 1)
# _on_button_color_changed (vers 1)
# _collect_current_button_colors (vers 1)

App_name = "GUI - App System Settings"
App_build = "December 11 - "
App_auth = "X-Seti"


class ThemeSaveDialog(QDialog):
    """Dialog for saving themes with complete metadata"""

def _apply_settings(self): #vers 3
    """Apply settings permanently and save to appfactory.settings.json"""
    new_settings = self._get_dialog_settings()
    old_theme = self.app_settings.current_settings["theme"]

    # Update settings
    self.app_settings.current_settings.update(new_settings)

    # Save font settings if modified
    if hasattr(self, 'font_controls'):
        self._save_font_settings()

    # Update modified colors if any
    if hasattr(self, '_modified_colors') and self._modified_colors:
        current_theme = self.app_settings.current_settings["theme"]
        if current_theme in self.app_settings.themes:
            if "colors" not in self.app_settings.themes[current_theme]:
                self.app_settings.themes[current_theme]["colors"] = {}
            self.app_settings.themes[current_theme]["colors"].update(self._modified_colors)

    # Save settings to appfactory.settings.json
    self.app_settings.save_settings()

    # Emit signals
    if new_settings["theme"] != old_theme:
        self.themeChanged.emit(new_settings["theme"])
    self.settingsChanged.emit()

    QMessageBox.information(
        self,
        "Applied",
        f"Settings saved to appfactory.settings.json\n\nActive theme: {new_settings['theme']}"
    )
    def __init__(self, app_settings, current_theme_data, parent=None): #vers 1
        super().__init__(parent)
        self.app_settings = app_settings
        self.current_theme_data = current_theme_data
        self.result_theme_data = None

        self.setWindowTitle("Save Theme - " + App_name)
        self.setMinimumSize(500, 600)
        self.setModal(True)

        self._setup_ui()
        self._detect_theme_type()

    def _setup_ui(self): #vers 1
        """Create the save dialog UI"""
        layout = QVBoxLayout(self)

        # Theme Detection Display
        self.detection_label = QLabel()
        self.detection_label.setStyleSheet("font-weight: bold; padding: 8px; border-radius: 4px;")
        layout.addWidget(self.detection_label)

        # Instructions
        instructions = QLabel("""
        <b>Theme Naming Guidelines:</b><br>
        - Dark themes: Add "_Dark" suffix<br>
        - Light themes: Add "_Light" suffix
        """)
        instructions.setWordWrap(True)
        instructions.setStyleSheet("padding: 6px; border-radius: 4px; margin: 4px;")
        layout.addWidget(instructions)

        # Form Group
        form_group = QGroupBox("Theme Information")
        form_layout = QVBoxLayout(form_group)

        grid_layout = QGridLayout()

        # Theme Name
        grid_layout.addWidget(QLabel("Theme Name:"), 0, 0)
        self.name_input = QLineEdit()
        self.name_input.setPlaceholderText("Enter theme name (e.g., 'Ocean_Dark')")
        self.name_input.textChanged.connect(self._validate_inputs)
        grid_layout.addWidget(self.name_input, 0, 1, 1, 2)

        # Display Name
        grid_layout.addWidget(QLabel("Display Name:"), 2, 0)
        self.display_input = QLineEdit()
        self.display_input.setPlaceholderText("Human-readable name (e.g., 'Ocean Dark Theme')")
        grid_layout.addWidget(self.display_input, 2, 1, 1, 2)

        # Description
        grid_layout.addWidget(QLabel("Description:"), 3, 0)
        self.description_input = QLineEdit()
        self.description_input.setPlaceholderText("Brief description of your theme")
        grid_layout.addWidget(self.description_input, 3, 1, 1, 2)

        # Category
        grid_layout.addWidget(QLabel("Category:"), 4, 0)
        self.category_combo = QComboBox()
        self.category_combo.setEditable(True)
        categories = [
            "Professional",
            "Dark Themes",
            "Light Themes",
            "Nature",
            "Creative",
            "Gaming",
            "Business",
            "Colorful",
            "High Contrast",
            "Custom"
        ]
        self.category_combo.addItems(categories)
        grid_layout.addWidget(self.category_combo, 4, 1, 1, 2)

        # Author
        grid_layout.addWidget(QLabel("Author:"), 5, 0)
        self.author_input = QLineEdit("X-Seti")
        grid_layout.addWidget(self.author_input, 5, 1, 1, 2)

        # Version
        grid_layout.addWidget(QLabel("Version:"), 6, 0)
        self.version_input = QLineEdit("1.0")
        self.version_input.setMaximumWidth(100)
        grid_layout.addWidget(self.version_input, 6, 1)

        grid_layout.setColumnStretch(0, 0)
        grid_layout.setColumnStretch(1, 1)
        grid_layout.setColumnStretch(2, 1)

        form_layout.addLayout(grid_layout)
        layout.addWidget(form_group)

        # Color Summary
        color_group = QGroupBox("Color Summary")
        color_layout = QVBoxLayout(color_group)

        self.color_summary = QLabel()
        self.color_summary.setWordWrap(True)
        color_layout.addWidget(self.color_summary)

        layout.addWidget(color_group)

        # Buttons
        button_layout = QHBoxLayout()

        self.auto_detect_btn = QPushButton("Auto-Detect Theme Type")
        self.auto_detect_btn.clicked.connect(self._detect_theme_type)
        button_layout.addWidget(self.auto_detect_btn)

        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)

        self.save_btn = QPushButton("Save Theme")
        self.save_btn.clicked.connect(self._save_theme)
        self.save_btn.setEnabled(False)
        button_layout.addWidget(self.save_btn)

        layout.addLayout(button_layout)

        self._populate_current_data()
        self._update_color_summary()

    def _detect_theme_type(self): #vers 1
        """Auto-detect if theme is dark or light based on colors"""
        if not self.current_theme_data or "colors" not in self.current_theme_data:
            return

        colors = self.current_theme_data["colors"]
        bg_primary = colors.get("bg_primary", "#ffffff")

        try:
            hex_color = bg_primary.lstrip('#')
            if len(hex_color) == 6:
                r = int(hex_color[0:2], 16)
                g = int(hex_color[2:4], 16)
                b = int(hex_color[4:6], 16)

                brightness = (r * 0.299 + g * 0.587 + b * 0.114)

                if brightness < 128:
                    theme_type = "Dark"
                    self.detection_label.setText("DARK THEME DETECTED")
                    self.detection_label.setStyleSheet(
                        "background: #1A1A1A; color: #FFFFFF; padding: 8px; "
                        "border-radius: 4px; font-weight: bold;"
                    )
                    self.category_combo.setCurrentText("Dark Themes")
                else:
                    theme_type = "Light"
                    self.detection_label.setText("LIGHT THEME DETECTED")
                    self.detection_label.setStyleSheet(
                        "background: #F5F5F5; color: #333333; padding: 8px; "
                        "border-radius: 4px; font-weight: bold;"
                    )
                    self.category_combo.setCurrentText("Light Themes")

                current_name = self.name_input.text()
                if current_name and not current_name.endswith(f"_{theme_type}"):
                    if not current_name.endswith("_Dark") and not current_name.endswith("_Light"):
                        self.name_input.setText(f"{current_name}_{theme_type}")

        except Exception as e:
            self.detection_label.setText("Could not detect theme type")
            print(f"Theme detection error: {e}")

    def _add_suffix(self, suffix): #vers 1
        """Add suffix to theme name"""
        current_name = self.name_input.text()
        if current_name:
            for existing_suffix in ["_Dark", "_Light"]:
                if current_name.endswith(existing_suffix):
                    current_name = current_name[:-len(existing_suffix)]
                    break
            self.name_input.setText(f"{current_name}{suffix}")

    def _populate_current_data(self): #vers 1
        """Populate form with current theme data"""
        if self.current_theme_data:
            self.display_input.setText(self.current_theme_data.get("name", ""))
            self.description_input.setText(self.current_theme_data.get("description", ""))

            category = self.current_theme_data.get("category", "Custom")
            index = self.category_combo.findText(category)
            if index >= 0:
                self.category_combo.setCurrentIndex(index)
            else:
                self.category_combo.setCurrentText(category)

            self.author_input.setText(self.current_theme_data.get("author", "X-Seti"))
            self.version_input.setText(self.current_theme_data.get("version", "1.0"))

    def _update_color_summary(self): #vers 1
        """Update color summary display"""
        if not self.current_theme_data or "colors" not in self.current_theme_data:
            return

        colors = self.current_theme_data["colors"]

        summary_html = f"""
        <b>Primary Colors:</b><br>
        - Background: <span style='background-color: {colors.get('bg_primary', '#fff')};
          padding: 2px 8px; border: 1px solid #ccc;'>{colors.get('bg_primary', '#fff')}</span><br>
        - Accent: <span style='background-color: {colors.get('accent_primary', '#000')};
          color: white; padding: 2px 8px;'>{colors.get('accent_primary', '#000')}</span><br>
        - Text: <span style='background-color: {colors.get('text_primary', '#000')};
          color: white; padding: 2px 8px;'>{colors.get('text_primary', '#000')}</span><br>
        <br>
        <b>Total Colors:</b> {len(colors)} defined
        """

        self.color_summary.setText(summary_html)

    def _validate_inputs(self): #vers 1
        """Validate form inputs"""
        name = self.name_input.text().strip()

        valid = bool(name and len(name) >= 3)

        if valid:
            import re
            valid = bool(re.match(r'^[a-zA-Z0-9_\-\s]+$', name))

        self.save_btn.setEnabled(valid)

        if valid:
            self.save_btn.setText("Save Theme")
            self.save_btn.setStyleSheet("")
        else:
            self.save_btn.setText("Invalid Name")
            self.save_btn.setStyleSheet("background-color: #ffcccb;")


    def _save_theme(self): #vers 5
        """Save the theme with COMPLETE metadata including menus, buttons, and fonts"""
        theme_name = self.name_input.text().strip()

        # Get default template structure (menus only - NO colors, fonts, or button_panels)
        default_template = self._get_complete_theme_template()

        # Collect ACTUAL colors from current theme data (user's picked colors)
        colors = self.current_theme_data.get("colors", {}).copy()

        # Collect ACTUAL font settings from app_settings (user's chosen fonts)
        fonts = self._collect_current_font_settings()

        # Collect ACTUAL button panel colors from button editor (user's chosen button colors)
        button_panels = self._collect_current_button_colors()

        # Build COMPLETE theme data with ALL required sections
        self.result_theme_data = {
            "name": self.display_input.text().strip() or theme_name,
            "description": self.description_input.text().strip() or f"Custom theme: {theme_name}",
            "category": self.category_combo.currentText(),
            "author": self.author_input.text().strip() or "X-Seti",
            "version": self.version_input.text().strip() or "1.0",

            # COLORS - Use actual colors from the theme being saved (user's color picks)
            "colors": colors,

            # MENUS - Get from current theme or use defaults
            "menus": self.current_theme_data.get("menus", default_template.get("menus", {})),

            # BUTTON PANELS - Use actual button colors from button editor (user's chosen colors)
            "button_panels": button_panels,

            # FONTS - Use actual font settings from app_settings (user's chosen fonts)
            "fonts": fonts
        }

        # Add timestamp
        from datetime import datetime
        self.result_theme_data["created"] = datetime.now().strftime("%Y-%m-%d %H:%M")

        # Save theme
        success = self.app_settings.save_theme(theme_name, self.result_theme_data)

        if success:
            # Count components for user feedback
            color_count = len(self.result_theme_data.get('colors', {}))
            menu_items = self.result_theme_data.get('menus', {}).get('menu_items', {})
            menu_count = sum(len(items) for items in menu_items.values())
            button_panels = self.result_theme_data.get('button_panels', {})
            button_count = sum(len(panel) for panel in button_panels.values())
            font_count = len(self.result_theme_data.get('fonts', {}))

            QMessageBox.information(
                self, "Theme Saved",
                f"Theme '{theme_name}' saved successfully!\n\n"
                f"Display Name: {self.result_theme_data['name']}\n"
                f"Category: {self.result_theme_data['category']}\n\n"
                f"Components exported:\n"
                f"  • Colors: {color_count}\n"
                f"  • Menu items: {menu_count}\n"
                f"  • Button panels: {button_count} buttons\n"
                f"  • Font definitions: {font_count}"
            )
            self.accept()
        else:
            QMessageBox.warning(
                self, "Save Failed",
                f"Failed to save theme '{theme_name}'.\n"
                "Please check file permissions and try again."
            )

    def _collect_current_font_settings(self): #vers 1
        """Collect current font settings from app_settings.current_settings"""
        font_ids = ['default', 'title', 'panel', 'button', 'menu', 'infobar', 'table', 'tooltip']
        fonts = {}

        for font_id in font_ids:
            # Get font settings from app_settings (where they're stored after user changes)
            family = self.app_settings.current_settings.get(f'{font_id}_font_family', 'Segoe UI')
            size = self.app_settings.current_settings.get(f'{font_id}_font_size', 9)
            weight = self.app_settings.current_settings.get(f'{font_id}_font_weight', 'Normal')

            # Store in theme format
            fonts[f'{font_id}_font_family'] = family
            fonts[f'{font_id}_font_size'] = size
            fonts[f'{font_id}_font_weight'] = weight

        return fonts

    def _get_complete_theme_template(self): #vers 1
        """Get complete theme template structure (menus ONLY) - NO hardcoded colors, fonts, or button_panels"""
        return {
            "menus": {
                "menu_structure": [
                    "File", "Edit", "Dat", "IMG", "Model", "Texture", "Collision",
                    "Item Definition", "Item Placement", "Entry", "Settings", "Help"
                ],
                "menu_items": {
                    "File": [
                        {"text": "New IMG...", "icon": "document-new", "shortcut": "Ctrl+N", "action": "create_new_img"},
                        {"text": "Open IMG...", "icon": "document-open", "shortcut": "Ctrl+O", "action": "open_img_file"},
                        {"text": "Open COL...", "icon": "document-open", "action": "open_col_file"},
                        {"separator": True},
                        {"text": "Close", "icon": "window-close", "action": "close_img_file"},
                        {"separator": True},
                        {"text": "Exit", "icon": "application-exit", "shortcut": "Ctrl+Q", "action": "close"}
                    ],
                    "IMG": [
                        {"text": "Rebuild", "icon": "document-save", "action": "rebuild_img"},
                        {"text": "Save Entry...", "icon": "document-save-entry", "action": "save_img_entry"},
                        {"text": "Rebuild As...", "icon": "document-save-as", "action": "rebuild_img_as"},
                        {"separator": True},
                        {"text": "Merge IMG Files", "icon": "document-merge"},
                        {"text": "Split IMG File", "icon": "edit-cut"},
                        {"separator": True},
                        {"text": "IMG Properties", "icon": "dialog-information"}
                    ],
                    "Entry": [
                        {"text": "Import Files...", "icon": "go-down", "action": "import_files"},
                        {"text": "Export Selected...", "icon": "go-up", "action": "export_selected"},
                        {"text": "Export All...", "icon": "go-up", "action": "export_all"},
                        {"separator": True},
                        {"text": "Remove Selected", "icon": "list-remove", "action": "remove_selected"},
                        {"text": "Rename Entry", "icon": "edit"}
                    ],
                    "Settings": [
                        {"text": "Preferences...", "icon": "preferences-other", "action": "show_settings"},
                        {"text": "Themes...", "icon": "applications-graphics", "action": "show_theme_settings"},
                        {"separator": True},
                        {"text": "Manage Templates...", "icon": "folder", "action": "manage_templates"}
                    ],
                    "Help": [
                        {"text": "User Guide", "icon": "help-contents"},
                        {"text": "About IMG Factory", "icon": "help-about", "action": "show_about"}
                    ]
                }
            }
        }

    def get_theme_data(self): #vers 1
        """Get the final theme data after dialog closes"""
        return self.result_theme_data


class CustomWindow(QMainWindow): #vers 1
    """Base window class with custom gadgets and corner resize support"""

    def __init__(self, app_name="Application", parent=None): #vers 2
        """Initialize custom window"""
        super().__init__(parent)

        self.app_name = app_name
        self.setWindowTitle(app_name)
        self.setMinimumSize(800, 600)

        # Load app settings
        self.app_settings = AppSettings()

        # Initialize icon provider WITH app_settings
        self.icons = IconProvider(self, self.app_settings)

        # Setup resize handling
        self.resize_margin = 10
        self.resize_direction = None
        self.drag_position = None
        self.initial_geometry = None
        self.setMouseTracking(True)

        # Create main layout
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        self.main_layout = QVBoxLayout(central_widget)
        self.main_layout.setContentsMargins(0, 0, 0, 0)
        self.main_layout.setSpacing(0)

        # Apply custom gadgets setting
        self._apply_window_mode()

        # Apply theme
        self._apply_theme()


    def _apply_theme(self): #vers 2
        """Apply current theme"""
        stylesheet = self.app_settings.get_stylesheet()
        self.setStyleSheet(stylesheet)

        # Update titlebar icons to match new theme
        self._update_titlebar_icons()


    def _apply_window_mode(self): #vers 1
        """Apply custom window gadgets or system mode"""
        use_custom = self.app_settings.current_settings.get("use_custom_gadgets", False)

        if use_custom:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
            if not hasattr(self, 'custom_titlebar'):
                self._create_custom_titlebar()
            else:
                self.custom_titlebar.setVisible(True)
        else:
            self.setWindowFlags(Qt.WindowType.Window)
            if hasattr(self, 'custom_titlebar'):
                self.custom_titlebar.setVisible(False)

        self.show()


    def _create_custom_titlebar(self): #vers 5
        """Create custom title bar with window controls - Simple text-based"""
        self.custom_titlebar = QWidget()
        self.custom_titlebar.setObjectName("customTitleBar")
        self.custom_titlebar.setFixedHeight(40)

        titlebar_layout = QHBoxLayout(self.custom_titlebar)
        titlebar_layout.setContentsMargins(4, 4, 4, 4)
        titlebar_layout.setSpacing(4)

        # Settings button on the left
        settings_btn = QPushButton("Settings")
        settings_btn.setFixedSize(100, 32)
        settings_btn.clicked.connect(self.open_settings)
        settings_btn.setToolTip("Settings")
        titlebar_layout.addWidget(settings_btn)

        # App title in center
        titlebar_layout.addStretch()
        title_label = QLabel(self.app_name)
        title_label.setStyleSheet("font-weight: bold; font-size: 11pt;")
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        titlebar_layout.addWidget(title_label)
        titlebar_layout.addStretch()

        # Window control buttons - Simple text
        minimize_btn = QPushButton("−")
        minimize_btn.setFixedSize(32, 32)
        minimize_btn.setStyleSheet("font-size: 16pt; font-weight: bold;")
        minimize_btn.clicked.connect(self.showMinimized)
        minimize_btn.setToolTip("Minimize")
        titlebar_layout.addWidget(minimize_btn)

        maximize_btn = QPushButton("□")
        maximize_btn.setFixedSize(32, 32)
        maximize_btn.setStyleSheet("font-size: 16pt; font-weight: bold;")
        maximize_btn.clicked.connect(self._toggle_maximize)
        maximize_btn.setToolTip("Maximize")
        self.maximize_btn = maximize_btn
        titlebar_layout.addWidget(maximize_btn)

        close_btn = QPushButton("×")
        close_btn.setFixedSize(32, 32)
        close_btn.setStyleSheet("font-size: 20pt; font-weight: bold;")
        close_btn.clicked.connect(self.close)
        close_btn.setToolTip("Close")
        titlebar_layout.addWidget(close_btn)

        # Add to top of main layout
        self.main_layout.insertWidget(0, self.custom_titlebar)

        # Enable dragging
        self.titlebar_drag_position = None
        self.custom_titlebar.mousePressEvent = self._titlebar_mouse_press
        self.custom_titlebar.mouseMoveEvent = self._titlebar_mouse_move
        self.custom_titlebar.mouseDoubleClickEvent = self._titlebar_double_click


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

    def _titlebar_double_click(self, event): #vers 1
        """Handle double click on title bar - maximize/restore"""
        if event.button() == Qt.MouseButton.LeftButton:
            self._toggle_maximize()
            event.accept()

    def _toggle_maximize(self): #vers 1
        """Toggle window maximize state"""
        if self.isMaximized():
            self.showNormal()
            if hasattr(self, 'maximize_btn'):
                self.maximize_btn.setIcon(self.icons.maximize_icon())
                self.maximize_btn.setToolTip("Maximize")
        else:
            self.showMaximized()
            if hasattr(self, 'maximize_btn'):
                self.maximize_btn.setIcon(self.icons.restore_icon())
                self.maximize_btn.setToolTip("Restore")

    def paintEvent(self, event): #vers 1   ➕ Added built-in fallback: Default Green
        """Paint corner resize handles"""
        super().paintEvent(event)

        if not self.app_settings.current_settings.get("enable_corner_resize", True):
            return

        from PyQt6.QtGui import QPainter, QPen, QColor

        painter = QPainter(self)
        pen = QPen(QColor(self.app_settings.get_theme_colors().get('border', '#cccccc')))
        pen.setWidth(2)
        painter.setPen(pen)

        # Draw corner resize indicators
        corner_size = 15

        # Top-left corner
        painter.drawLine(0, corner_size, corner_size, 0)

        # Top-right corner
        painter.drawLine(self.width() - corner_size, 0, self.width(), corner_size)

        # Bottom-left corner
        painter.drawLine(0, self.height() - corner_size, corner_size, self.height())

        # Bottom-right corner
        painter.drawLine(self.width() - corner_size, self.height(),
                        self.width(), self.height() - corner_size)


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


    def _handle_corner_resize(self, global_pos): #vers 1
        """Handle window resizing from any edge or corner"""
        if not self.resize_direction or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.initial_geometry

        min_width = 800
        min_height = 800

        new_geometry = QRect(geometry)

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

    # ===== SETTINGS DIALOG =====

    def open_settings(self): #vers 1
        """Open settings dialog"""
        dialog = SettingsDialog(self.app_settings, self)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            self._apply_theme()
            self._apply_window_mode()


class ColorPickerWidget(QWidget):
    """SAFE, simple color picker widget - NO THREADING"""
    colorPicked = pyqtSignal(str)

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.setFixedSize(280, 35)
        self.current_color = "#ffffff"
        self.picking_active = False
        self.color_display = None
        self.color_value = None
        self.pick_button = None
        self._setup_ui()

    def _setup_ui(self): #vers 1
        """Setup UI - main horizontal layout"""
        main_layout = QHBoxLayout(self)
        main_layout.setSpacing(0)
        main_layout.setContentsMargins(2, 2, 2, 2)

        # Color display square
        self.color_display = QLabel()
        self.color_display.setFixedSize(50, 30)
        self.color_display.setStyleSheet("border: 1px solid #999; border-radius: 3px; background-color: #ffffff;")
        main_layout.addWidget(self.color_display)

        # Hex value display
        self.color_value = QLabel("#FFFFFF")
        self.color_value.setFixedWidth(100)
        self.color_value.setFixedHeight(30)
        self.color_value.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.color_value.setStyleSheet("""
            font-family: monospace;
            font-size: 20px;
            font-weight: bold;
            color: #000000;
            background-color: #f0f0f0;
            border: 1px solid #999;
            border-radius: 3px;
            padding: 2px;
        """)
        main_layout.addWidget(self.color_value)

        # Pick button
        self.pick_button = QPushButton("Pick")
        self.pick_button.setFixedSize(70, 30)
        self.pick_button.clicked.connect(self.open_color_dialog)
        main_layout.addWidget(self.pick_button)

        # Initialize display
        self.update_color_display("#ffffff")

    def open_color_dialog(self): #vers 1
        """Open simple Qt color dialog - SAFE"""
        try:
            color = QColorDialog.getColor(QColor(self.current_color), self)
            if color.isValid():
                hex_color = color.name()
                self.current_color = hex_color
                self.update_color_display(hex_color)
                self.colorPicked.emit(hex_color)
                print(f"Color selected: {hex_color}")
        except Exception as e:
            print(f"Color dialog error: {e}")

    def update_color_display(self, hex_color): #vers 1
        """Update the color display safely"""
        try:
            if not hex_color or not isinstance(hex_color, str):
                hex_color = "#ffffff"

            # Ensure valid hex format
            if not hex_color.startswith('#'):
                hex_color = '#' + hex_color
            if len(hex_color) != 7:
                hex_color = "#ffffff"

            self.current_color = hex_color

            # Update color display
            if self.color_display:
                self.color_display.setStyleSheet(
                    f"background-color: {hex_color}; border: 1px solid #999; border-radius: 3px;"
                )

            # Update color value
            if self.color_value:
                self.color_value.setText(hex_color.upper())

        except Exception as e:
            print(f"Display update error: {e}")

    def closeEvent(self, event): #vers 1
        """Clean up when widget is closed"""
        super().closeEvent(event)

    def __del__(self): #vers 1
        """Clean up when object is destroyed"""
        pass


class ScreenCaptureThread(QThread):
    """Background thread for screen capture to avoid blocking UI"""
    colorCaptured = pyqtSignal(str)  # hex color

    def __init__(self, x, y, parent=None):
        super().__init__(parent)
        self.x = x
        self.y = y
        self.running = True

    def run(self):
        """Capture color at coordinates in background thread"""
        try:
            if MSS_AVAILABLE:
                color = self._capture_with_mss()
            elif PIL_AVAILABLE:
                color = self._capture_with_pil()
            else:
                color = self._capture_with_qt()

            if color and self.running:
                self.colorCaptured.emit(color)
        except Exception as e:
            print(f"Screen capture error: {e}")
            if self.running:
                self.colorCaptured.emit("#ffffff")  # Fallback color

    def _capture_with_mss(self):
        """High-performance capture using MSS library"""
        try:
            with mss.mss() as sct:
                # Capture 1x1 pixel area for maximum efficiency
                monitor = {"top": self.y, "left": self.x, "width": 1, "height": 1}
                screenshot = sct.grab(monitor)

                # MSS returns BGRA, we need RGB
                if len(screenshot.rgb) >= 3:
                    # screenshot.pixel(0, 0) returns (R, G, B) tuple
                    pixel = screenshot.pixel(0, 0)
                    return f"#{pixel[0]:02x}{pixel[1]:02x}{pixel[2]:02x}"

        except Exception as e:
            print(f"MSS capture error: {e}")
        return None

    def _capture_with_pil(self):
        """Fallback capture using PIL/Pillow"""
        try:
            # Capture small area around point for efficiency
            bbox = (self.x, self.y, self.x + 1, self.y + 1)
            screenshot = ImageGrab.grab(bbox)
            pixel = screenshot.getpixel((0, 0))

            # Handle both RGB and RGBA
            if isinstance(pixel, tuple) and len(pixel) >= 3:
                return f"#{pixel[0]:02x}{pixel[1]:02x}{pixel[2]:02x}"

        except Exception as e:
            print(f"PIL capture error: {e}")
        return None

    def _capture_with_qt(self):
        """Last resort: Qt screen capture"""
        try:
            screen = QApplication.primaryScreen()
            if screen:
                pixmap = screen.grabWindow(0, self.x, self.y, 1, 1)
                if not pixmap.isNull():
                    image = pixmap.toImage()
                    if not image.isNull():
                        color = QColor(image.pixel(0, 0))
                        return color.name()
        except Exception as e:
            print(f"Qt capture error: {e}")
        return None

    def stop(self):
        """Stop the capture thread"""
        self.running = False

class XPColorPicker(QWidget): #vers 2
    """XP Display Properties style color picker"""

    colorChanged = pyqtSignal(str, str)  # element_name, hex_color

    def __init__(self, theme_colors, parent=None): #vers 1
        super().__init__(parent)
        self.theme_colors = theme_colors
        self.current_hue = 240
        self.current_sat = 100
        self.current_bri = 25

        # UI element colors mapping
        self.element_colors = {
            'bg_primary': {'name': 'Window Background', 'h': 0, 's': 0, 'b': 100},
            'bg_secondary': {'name': 'Panel Background', 'h': 210, 's': 15, 'b': 98},
            'bg_tertiary': {'name': 'Alternate Background', 'h': 210, 's': 15, 'b': 92},
            'panel_bg': {'name': 'GroupBox Background', 'h': 210, 's': 8, 'b': 95},
            'accent_primary': {'name': 'Primary Accent', 'h': 210, 's': 85, 'b': 53},
            'accent_secondary': {'name': 'Secondary Accent', 'h': 210, 's': 85, 'b': 47},
            'text_primary': {'name': 'Primary Text', 'h': 0, 's': 0, 'b': 13},
            'text_secondary': {'name': 'Secondary Text', 'h': 210, 's': 15, 'b': 35},
            'text_accent': {'name': 'Accent Text', 'h': 210, 's': 85, 'b': 53},
            'alternate_row': {'name': 'Alternate Row Color', 'h': 210, 's': 10, 'b': 96},
            'button_normal': {'name': 'Button Face', 'h': 210, 's': 40, 'b': 95},
            'button_hover': {'name': 'Button Hover', 'h': 210, 's': 50, 'b': 85},
            'button_pressed': {'name': 'Button Pressed', 'h': 210, 's': 60, 'b': 75},
            'border': {'name': 'Border Color', 'h': 210, 's': 15, 'b': 85},
            'selection_background': {'name': 'Selection Background', 'h': 210, 's': 85, 'b': 55},
            'selection_text': {'name': 'Selection Text', 'h': 0, 's': 0, 'b': 100},
            'table_row_even': {'name': 'Table Row Even', 'h': 0, 's': 0, 'b': 99},
            'table_row_odd': {'name': 'Table Row Odd', 'h': 210, 's': 10, 'b': 95},
            'success': {'name': 'Success Color', 'h': 120, 's': 60, 'b': 50},
            'warning': {'name': 'Warning Color', 'h': 35, 's': 100, 'b': 60},
            'error': {'name': 'Error Color', 'h': 4, 's': 90, 'b': 58},
            'grid': {'name': 'Grid Line Color', 'h': 210, 's': 10, 'b': 88},
            'pin_default': {'name': 'Default Pin Color', 'h': 0, 's': 0, 'b': 46},
            'pin_highlight': {'name': 'Highlighted Pin', 'h': 210, 's': 85, 'b': 53},
            'action_import': {'name': 'Import Action', 'h': 210, 's': 60, 'b': 95},
            'action_export': {'name': 'Export Action', 'h': 120, 's': 50, 'b': 92},
            'action_remove': {'name': 'Remove Action', 'h': 4, 's': 40, 'b': 98},
            'action_update': {'name': 'Update Action', 'h': 35, 's': 50, 'b': 95},
            'action_convert': {'name': 'Convert Action', 'h': 280, 's': 45, 'b': 95},
            'panel_entries': {'name': 'Entries Panel BG', 'h': 120, 's': 20, 'b': 97},
            'panel_filter': {'name': 'Filter Panel BG', 'h': 50, 's': 20, 'b': 98},
            'toolbar_bg': {'name': 'Toolbar Background', 'h': 0, 's': 0, 'b': 98}
        }

        self._load_theme_colors()
        self._init_ui()

    def _init_ui(self): #vers 2
        """Initialize the XP style UI - FIXED: Removed duplicate global sliders"""
        main_layout = QHBoxLayout(self)
        main_layout.setContentsMargins(5, 5, 5, 5)

        # Left side - Element list
        left_widget = QWidget()
        left_layout = QVBoxLayout(left_widget)

        # Element selection
        element_label = QLabel("Select Element:")
        element_label.setFont(QFont("MS Sans Serif", 8, QFont.Weight.Bold))
        left_layout.addWidget(element_label)

        self.element_list = QListWidget()
        self.element_list.setMaximumWidth(160)
        self.element_list.setMaximumHeight(120)
        self.element_list.setStyleSheet("""
            QListWidget {
                background: white;
                border: 2px inset #f0f0f0;
                font-family: 'MS Sans Serif';
                font-size: 8pt;
            }
            QListWidget::item {
                padding: 2px 4px;
            }
            QListWidget::item:selected {
                background: #0a246a;
                color: white;
            }
        """)

        for key, data in self.element_colors.items():
            self.element_list.addItem(data['name'])

        self.element_list.setCurrentRow(0)
        self.element_list.currentRowChanged.connect(self._on_element_selected)
        left_layout.addWidget(self.element_list)

        main_layout.addWidget(left_widget)

        # Right side - Color controls
        right_widget = QWidget()
        right_layout = QVBoxLayout(right_widget)

        # Color preview
        color_label = QLabel("Color:")
        color_label.setFont(QFont("MS Sans Serif", 8, QFont.Weight.Bold))
        right_layout.addWidget(color_label)

        self.color_preview = QWidget()
        self.color_preview.setFixedSize(60, 30)
        self.color_preview.setStyleSheet("""
            QWidget {
                background: #0a246a;
                border: 2px inset #f0f0f0;
            }
        """)
        right_layout.addWidget(self.color_preview)

        # HSL Sliders - FOR INDIVIDUAL ELEMENT EDITING ONLY
        sliders_frame = QFrame()
        sliders_frame.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Sunken)
        sliders_layout = QVBoxLayout(sliders_frame)
        sliders_layout.setSpacing(3)

        # Hue Slider
        hue_layout = QHBoxLayout()
        hue_layout.addWidget(QLabel("Hue:"))
        self.hue_slider = QSlider(Qt.Orientation.Horizontal)
        self.hue_slider.setMinimum(0)
        self.hue_slider.setMaximum(360)
        self.hue_slider.setValue(240)
        self.hue_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.hue_slider.setTickInterval(30)
        hue_layout.addWidget(self.hue_slider)
        self.hue_value = QLabel("240")
        self.hue_value.setFixedWidth(40)
        self.hue_value.setAlignment(Qt.AlignmentFlag.AlignCenter)
        hue_layout.addWidget(self.hue_value)
        sliders_layout.addLayout(hue_layout)

        # Saturation Slider
        sat_layout = QHBoxLayout()
        sat_layout.addWidget(QLabel("Sat:"))
        self.sat_slider = QSlider(Qt.Orientation.Horizontal)
        self.sat_slider.setMinimum(0)
        self.sat_slider.setMaximum(100)
        self.sat_slider.setValue(100)
        self.sat_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.sat_slider.setTickInterval(10)
        sat_layout.addWidget(self.sat_slider)
        self.sat_value = QLabel("100")
        self.sat_value.setFixedWidth(40)
        self.sat_value.setAlignment(Qt.AlignmentFlag.AlignCenter)
        sat_layout.addWidget(self.sat_value)
        sliders_layout.addLayout(sat_layout)

        # Brightness Slider
        bri_layout = QHBoxLayout()
        bri_layout.addWidget(QLabel("Bri:"))
        self.bri_slider = QSlider(Qt.Orientation.Horizontal)
        self.bri_slider.setMinimum(0)
        self.bri_slider.setMaximum(100)
        self.bri_slider.setValue(25)
        self.bri_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.bri_slider.setTickInterval(10)
        bri_layout.addWidget(self.bri_slider)
        self.bri_value = QLabel("25")
        self.bri_value.setFixedWidth(40)
        self.bri_value.setAlignment(Qt.AlignmentFlag.AlignCenter)
        bri_layout.addWidget(self.bri_value)
        sliders_layout.addLayout(bri_layout)

        right_layout.addWidget(sliders_frame)

        # Connect sliders
        self.hue_slider.valueChanged.connect(self._on_hue_changed)
        self.sat_slider.valueChanged.connect(self._on_sat_changed)
        self.bri_slider.valueChanged.connect(self._on_bri_changed)

        right_layout.addStretch()
        main_layout.addWidget(right_widget)

    def _load_theme_colors(self): #vers 1
        """Load colors from current theme"""
        for element_key in self.element_colors.keys():
            if element_key in self.theme_colors:
                hex_color = self.theme_colors[element_key]
                h, s, l = rgb_to_hsl(hex_color)
                self.element_colors[element_key].update({'h': h, 's': s, 'b': l})

    def _on_element_selected(self, row): #vers 1
        """Handle element selection"""
        if row >= 0:
            element_keys = list(self.element_colors.keys())
            if row < len(element_keys):
                element_key = element_keys[row]
                color_data = self.element_colors[element_key]

                self.current_hue = color_data['h']
                self.current_sat = color_data['s']
                self.current_bri = color_data['b']

                self._update_sliders()
                self._update_color_preview()

    def _update_sliders(self): #vers 1
        """Update slider positions and values"""
        self.hue_slider.blockSignals(True)
        self.sat_slider.blockSignals(True)
        self.bri_slider.blockSignals(True)

        self.hue_slider.setValue(self.current_hue)
        self.sat_slider.setValue(self.current_sat)
        self.bri_slider.setValue(self.current_bri)

        self.hue_value.setText(str(self.current_hue))
        self.sat_value.setText(str(self.current_sat))
        self.bri_value.setText(str(self.current_bri))

        self.hue_slider.blockSignals(False)
        self.sat_slider.blockSignals(False)
        self.bri_slider.blockSignals(False)

    def _update_color_preview(self): #vers 1
        """Update the color preview widget"""
        hex_color = hsl_to_rgb(self.current_hue, self.current_sat, self.current_bri)
        self.color_preview.setStyleSheet(f"""
            QWidget {{
                background: {hex_color};
                border: 2px inset #f0f0f0;
            }}
        """)

        # Emit color change signal
        element_keys = list(self.element_colors.keys())
        current_row = self.element_list.currentRow()
        if current_row >= 0 and current_row < len(element_keys):
            element_key = element_keys[current_row]
            self.colorChanged.emit(element_key, hex_color)

    def _on_hue_changed(self, value): #vers 1
        """Handle hue slider change"""
        self.current_hue = value
        self.hue_value.setText(str(value))
        self._save_current_color()
        self._update_color_preview()

    def _on_sat_changed(self, value): #vers 1
        """Handle saturation slider change"""
        self.current_sat = value
        self.sat_value.setText(str(value))
        self._save_current_color()
        self._update_color_preview()

    def _on_bri_changed(self, value): #vers 1
        """Handle brightness slider change"""
        self.current_bri = value
        self.bri_value.setText(str(value))
        self._save_current_color()
        self._update_color_preview()

    def _save_current_color(self): #vers 1
        """Save current color to selected element"""
        element_keys = list(self.element_colors.keys())
        current_row = self.element_list.currentRow()
        if current_row >= 0 and current_row < len(element_keys):
            element_key = element_keys[current_row]
            self.element_colors[element_key].update({
                'h': self.current_hue,
                's': self.current_sat,
                'b': self.current_bri
            })

    def update_color_display(self, hex_color): #vers 1
        """Update display with picked color"""
        try:
            h, s, l = rgb_to_hsl(hex_color)
            self.current_hue = h
            self.current_sat = s
            self.current_bri = l
            self._update_sliders()
            self._update_color_preview()
            self._save_current_color()
        except Exception as e:
            print(f"Error updating color display: {e}")

    def get_all_colors(self): #vers 1
        """Get all colors as hex values"""
        colors = {}
        for element_key, data in self.element_colors.items():
            colors[element_key] = hsl_to_rgb(data['h'], data['s'], data['b'])
        return colors

class ThemeColorEditor(QWidget): #vers 4
    """Widget for editing individual theme colors with lock protection - FIXED Pick button"""
    colorChanged = pyqtSignal(str, str)  # color_key, hex_color
    lockChanged = pyqtSignal(str, bool)  # color_key, is_locked

    def __init__(self, color_key, color_name, current_value, parent=None): #vers 3
        super().__init__(parent)
        self.color_key = color_key
        self.color_name = color_name
        self.current_value = current_value
        self.is_locked = False
        self._setup_ui()

    def _setup_ui(self): #vers 4
        """Setup the editor UI with lock checkbox - FIXED: Wider Pick button"""
        layout = QHBoxLayout(self)
        layout.setContentsMargins(5, 2, 5, 2)

        # Lock checkbox
        self.lock_check = QCheckBox()
        self.lock_check.setFixedWidth(30)
        self.lock_check.setToolTip("Lock to prevent global adjustments")
        self.lock_check.stateChanged.connect(self._on_lock_changed)
        layout.addWidget(self.lock_check)

        # Color name label - FIXED WIDTH for alignment
        name_label = QLabel(self.color_name)
        name_label.setMinimumWidth(150)
        name_label.setMaximumWidth(150)
        layout.addWidget(name_label)

        # Color preview
        self.color_preview = QLabel()
        self.color_preview.setFixedSize(30, 30)
        self.update_preview(self.current_value)
        layout.addWidget(self.color_preview)

        # Color value input - FIXED WIDTH for column alignment
        self.color_input = QLineEdit(self.current_value)
        self.color_input.setMinimumWidth(85)
        self.color_input.setMaximumWidth(85)
        self.color_input.setFont(QFont("monospace", 9))
        self.color_input.textChanged.connect(self.on_color_changed)
        layout.addWidget(self.color_input)

        # Color dialog button - FIXED: Wider for better visibility
        dialog_btn = QPushButton("Pick")
        dialog_btn.setMinimumWidth(80)  # CHANGED from setFixedSize(50, 25)
        dialog_btn.setFixedHeight(30)
        dialog_btn.setToolTip("Open color picker dialog")
        dialog_btn.clicked.connect(self.open_color_dialog)
        layout.addWidget(dialog_btn)

        layout.addStretch()

    def _on_lock_changed(self, state): #vers 1
        """Handle lock state change"""
        self.is_locked = (state == Qt.CheckState.Checked.value)
        self.lockChanged.emit(self.color_key, self.is_locked)

        # Visual feedback - dim when locked
        if self.is_locked:
            self.color_input.setStyleSheet("background-color: #f0f0f0; color: #666;")
            self.lock_check.setToolTip("Locked - Click to unlock")
        else:
            self.color_input.setStyleSheet("")
            self.lock_check.setToolTip("Unlocked - Click to lock")

    def on_color_changed(self, text): #vers 1
        """Handle color input text change"""
        if text.startswith('#') and len(text) == 7:
            self.current_value = text
            self.update_preview(text)
            self.colorChanged.emit(self.color_key, text)

    def open_color_dialog(self): #vers 1
        """Open color picker dialog"""
        color = QColorDialog.getColor(QColor(self.current_value), self)
        if color.isValid():
            hex_color = color.name()
            self.color_input.setText(hex_color)

    def update_color(self, hex_color): #vers 2
        """Update color from external source - respects lock"""
        # Only update if not locked
        if not self.is_locked:
            self.color_input.setText(hex_color)
            self.current_value = hex_color
            self.update_preview(hex_color)

    def set_locked(self, locked): #vers 1
        """Programmatically set lock state"""
        self.lock_check.setChecked(locked)

    def update_preview(self, hex_color): #vers 1
        """Update the color preview"""
        self.color_preview.setStyleSheet(
            f"background-color: {hex_color}; border: 1px solid #999;"
        )

    def set_color(self, hex_color): #vers 1
        """Set color from external source (like color picker)"""
        self.color_input.setText(hex_color)
        self.current_value = hex_color
        self.update_preview(hex_color)
        self.colorChanged.emit(self.color_key, hex_color)


class DebugActionsHelper: #vers 1
    """Helper class for debug tab actions in SettingsDialog"""

    def __init__(self, settings_dialog): #vers 1
        """Initialize with reference to settings dialog"""
        self.dialog = settings_dialog
        self.main_window = settings_dialog.parent()

    def test_debug_output(self): #vers 1
        """Test debug output - sends test messages to activity log"""
        if hasattr(self.main_window, 'log_message'):
            # Send test messages
            self.main_window.log_message("Debug test message - debug system working!")
            self.main_window.log_message(
                f"[DEBUG-TEST] Debug enabled: {self.dialog.debug_enabled_check.isChecked()}"
            )
            self.main_window.log_message(
                f"[DEBUG-TEST] Debug level: {self.dialog.debug_level_combo.currentText()}"
            )

            # Get enabled categories
            enabled_categories = [
                cat for cat, cb in self.dialog.debug_categories.items()
                if cb.isChecked()
            ]
            self.main_window.log_message(
                f"[DEBUG-TEST] Active categories: {', '.join(enabled_categories)}"
            )

            # Test each category
            for category in enabled_categories:
                self.main_window.log_message(f"[DEBUG-TEST] Testing {category} category")

        else:
            QMessageBox.information(
                self.dialog,
                "Debug Test",
                "Debug test completed!\nCheck the activity log for output."
            )

    def debug_current_img(self): #vers 1
        """Debug current IMG file - analyzes loaded IMG and table state"""
        if not hasattr(self.main_window, 'current_img'):
            self._show_no_img_message()
            return

        if not self.main_window.current_img:
            self._show_no_img_message()
            return

        img = self.main_window.current_img

        # Basic IMG info
        self.main_window.log_message(f"[DEBUG-IMG] Current IMG: {img.file_path}")
        self.main_window.log_message(f"[DEBUG-IMG] Entries: {len(img.entries)}")

        # File type analysis
        file_types = {}
        all_extensions = set()

        for entry in img.entries:
            ext = entry.name.split('.')[-1].upper() if '.' in entry.name else "NO_EXT"
            file_types[ext] = file_types.get(ext, 0) + 1
            all_extensions.add(ext)

        self.main_window.log_message(f"[DEBUG-IMG] File types found:")
        for ext, count in sorted(file_types.items()):
            self.main_window.log_message(f"[DEBUG-IMG]   {ext}: {count} files")

        self.main_window.log_message(
            f"[DEBUG-IMG] Unique extensions: {sorted(all_extensions)}"
        )

        # Table state analysis
        self._debug_table_state()

        # Memory info
        total_size = sum(entry.size for entry in img.entries)
        self.main_window.log_message(
            f"[DEBUG-IMG] Total size: {self._format_size(total_size)}"
        )

    def _debug_table_state(self): #vers 1
        """Debug table widget state"""
        if not hasattr(self.main_window, 'gui_layout'):
            return

        if not hasattr(self.main_window.gui_layout, 'table'):
            return

        table = self.main_window.gui_layout.table
        table_rows = table.rowCount()

        # Count hidden rows
        hidden_rows = sum(1 for row in range(table_rows) if table.isRowHidden(row))
        visible_rows = table_rows - hidden_rows

        self.main_window.log_message(f"[DEBUG-IMG] Table Analysis:")
        self.main_window.log_message(f"[DEBUG-IMG]   Total rows: {table_rows}")
        self.main_window.log_message(f"[DEBUG-IMG]   Visible rows: {visible_rows}")
        self.main_window.log_message(f"[DEBUG-IMG]   Hidden rows: {hidden_rows}")

        # Selection info
        selected_rows = table.selectedItems()
        selected_count = len(set(item.row() for item in selected_rows))
        self.main_window.log_message(f"[DEBUG-IMG]   Selected rows: {selected_count}")

        # Column info
        column_count = table.columnCount()
        self.main_window.log_message(f"[DEBUG-IMG]   Columns: {column_count}")

        # Header info
        headers = [table.horizontalHeaderItem(i).text() for i in range(column_count)]
        self.main_window.log_message(f"[DEBUG-IMG]   Headers: {', '.join(headers)}")

    def clear_debug_log(self): #vers 1
        """Clear the activity log"""
        if hasattr(self.main_window, 'gui_layout') and hasattr(self.main_window.gui_layout, 'log'):
            self.main_window.gui_layout.log.clear()
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Debug log cleared")
        else:
            QMessageBox.information(
                self.dialog,
                "Clear Log",
                "Activity log cleared (if available)."
            )

    # Helper methods

    def _show_no_img_message(self): #vers 1
        """Show no IMG loaded message"""
        if hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("[DEBUG-IMG] No IMG file currently loaded")
        else:
            QMessageBox.information(
                self.dialog,
                "Debug IMG",
                "No IMG file loaded."
            )

    def _format_size(self, size_bytes): #vers 1
        """Format byte size to human readable"""
        for unit in ['B', 'KB', 'MB', 'GB']:
            if size_bytes < 1024.0:
                return f"{size_bytes:.2f} {unit}"
            size_bytes /= 1024.0
        return f"{size_bytes:.2f} TB"


# Integration function for SettingsDialog
def integrate_debug_actions(settings_dialog): #vers 1
    """Integrate debug actions helper into SettingsDialog"""
    helper = DebugActionsHelper(settings_dialog)

    # Add helper methods to dialog
    settings_dialog._test_debug_output = helper.test_debug_output
    settings_dialog._debug_current_img = helper.debug_current_img
    settings_dialog._clear_debug_log = helper.clear_debug_log

    return helper

class DebugSettings:
    """Debug mode settings and utilities"""

    def __init__(self, app_settings):
        self.app_settings = app_settings
        self.debug_enabled = app_settings.current_settings.get('debug_mode', False)
        self.debug_level = app_settings.current_settings.get('debug_level', 'INFO')
        self.debug_categories = app_settings.current_settings.get('debug_categories', [
            'IMG_LOADING', 'TABLE_POPULATION', 'BUTTON_ACTIONS', 'FILE_OPERATIONS'
        ])

    def is_debug_enabled(self, category='GENERAL'):
        """Check if debug is enabled for specific category"""
        return self.debug_enabled and category in self.debug_categories

    def debug_log(self, message, category='GENERAL', level='INFO'):
        """Log debug message if debug mode is enabled"""
        if self.is_debug_enabled(category):
            timestamp = QDateTime.currentDateTime().toString("hh:mm:ss.zzz")
            debug_msg = f"[DEBUG-{category}] [{timestamp}] {message}"

            # Send to main window log if available
            if hasattr(self.app_settings, 'main_window') and hasattr(self.app_settings.main_window, 'log_message'):
                self.app_settings.main_window.log_message(debug_msg)
            else:
                print(debug_msg)

    def toggle_debug_mode(self):
        """Toggle debug mode on/off"""
        self.debug_enabled = not self.debug_enabled
        self.app_settings.current_settings['debug_mode'] = self.debug_enabled
        self.app_settings.save_settings()
        return self.debug_enabled

class AppSettings:
    def __init__(self, settings_file="appfactory.settings.json"): #vers 3
        """Initialize application settings with Windows compatibility"""
        current_file_dir = Path(__file__).parent

        # FIXED: Set paths based on where we are - Windows compatible
        if current_file_dir.name == "utils":
            self.themes_dir = current_file_dir.parent / "themes"
            self.settings_file = current_file_dir.parent / settings_file
        else:
            self.themes_dir = current_file_dir / "themes"
            self.settings_file = current_file_dir / settings_file

        # FIXED: Windows-compatible default paths using Path objects
        if os.name == 'nt':  # Windows
            user_home = Path.home()
            desktop = user_home / "Desktop"
            steam_paths = [
                Path("C:/Program Files (x86)/Steam/steamapps/common/"),
                Path("C:/Program Files/Steam/steamapps/common/"),
                user_home / ".steam/steam/steamapps/common/"
            ]
            working_gta = next((str(p) for p in steam_paths if p.exists()), str(desktop / "GTA_VC"))
        else:  # Linux/Mac
            user_home = Path.home()
            desktop = user_home / "Desktop"
            working_gta = str(user_home / ".steam/steam/steamapps/common/")

        # Initialize default settings with Windows-safe paths
        self.default_settings = {
            'debug_mode': False,
            'debug_level': 'INFO',
            'current_theme': 'App_Factory',
            'theme': 'App_Factory',
            "button_display_mode": "both",  # "both", "icons", "text"
            "use_custom_gadgets": False,
            "enable_corner_resize": True,
            "use_svg_icons": True,
            "font_family": "Arial",
            "font_size": 9,
            "show_tooltips": True,
            "show_menu_icons": True,
            "auto_save": True,
            "panel_opacity": 95,
            'debug_categories': ['IMG_LOADING', 'TABLE_POPULATION', 'BUTTON_ACTIONS', 'FILE_OPERATIONS'],
            'working_gta_folder': working_gta,
            'assists_folder': str(desktop / "Assists"),
            'textures_folder': str(desktop / "Textures"),
            'collisions_folder': str(desktop / "Collisions"),
            'generics_folder': str(desktop / "Generics"),
            'water_folder': str(desktop / "Water"),
            'radar_folder': str(desktop / "Radartiles"),
            'gameart_folder': str(desktop / "Gameart"),
            'peds_folder': str(desktop / "Peds"),
            'vehicles_folder': str(desktop / "Vehicles"),
            'weapons_folder': str(desktop / "Weapons"),
            'font_family': 'Segoe UI' if os.name == 'nt' else 'Sans Serif',
            'font_size': 9,
            'show_tooltips': True,
            'show_menu_icons': True,
            'show_button_icons': True,
            'panel_opacity': 95,
            'remember_img_output_path': True,
            'remember_import_path': True,
            'remember_export_path': True,
            'last_img_output_path': '',
            'last_import_path': '',
            'last_export_path': '',
            # Button settings
            'use_pastel_buttons': True,
            'high_contrast_buttons': False,
            'button_size': 24,
            'icon_size': 16,
            'button_format': 'both'  # 'both', 'icon_only', 'text_only', 'separate'
        }

        print(f"Looking for themes in: {self.themes_dir}")
        print(f"Themes directory exists: {self.themes_dir.exists()}")
        if self.themes_dir.exists():
            theme_files = list(self.themes_dir.glob("*.json"))
            print(f"Found {len(theme_files)} theme files")

        self.themes = self._load_all_themes()
        self.current_settings = self._load_settings()

        # GTA Project Directories
        self.working_gta_folder = self.current_settings.get('working_gta_folder', self.default_settings['working_gta_folder'])
        self.assists_folder = self.current_settings.get('assists_folder', self.default_settings['assists_folder'])
        self.textures_folder = self.current_settings.get('textures_folder', self.default_settings['textures_folder'])
        self.collisions_folder = self.current_settings.get('collisions_folder', self.default_settings['collisions_folder'])
        self.generics_folder = self.current_settings.get('generics_folder', self.default_settings['generics_folder'])
        self.water_folder = self.current_settings.get('water_folder', self.default_settings['water_folder'])
        self.radar_folder = self.current_settings.get('radar_folder', self.default_settings['radar_folder'])
        self.gameart_folder = self.current_settings.get('gameart_folder', self.default_settings['gameart_folder'])
        self.peds_folder = self.current_settings.get('peds_folder', self.default_settings['peds_folder'])
        self.vehicles_folder = self.current_settings.get('vehicles_folder', self.default_settings['vehicles_folder'])
        self.weapons_folder = self.current_settings.get('weapons_folder', self.default_settings['weapons_folder'])


    def _generate_stylesheet(self, colors): #vers 2
        """Generate stylesheet from colors dict - shared by both classes"""
        if not colors:
            return ""

        # Extract all colors
        bg_primary = colors.get('bg_primary', '#ffffff')
        bg_secondary = colors.get('bg_secondary', '#f5f5f5')
        bg_tertiary = colors.get('bg_tertiary', '#e9ecef')
        panel_bg = colors.get('panel_bg', '#f0f0f0')
        text_primary = colors.get('text_primary', '#000000')
        text_secondary = colors.get('text_secondary', '#666666')
        text_accent = colors.get('text_accent', '#0066cc')
        accent_primary = colors.get('accent_primary', '#0078d4')
        accent_secondary = colors.get('accent_secondary', '#0A7Ad4')
        border = colors.get('border', '#cccccc')

        # Table/List alternating rows - use table_row_odd or alternate_row as fallback
        alternate_row = colors.get('table_row_odd', colors.get('alternate_row', '#f5f5f5'))
        table_row_even = colors.get('table_row_even', bg_primary)
        table_row_odd = colors.get('table_row_odd', bg_secondary)

        button_normal = colors.get('button_normal', '#e0e0e0')
        button_hover = colors.get('button_hover', '#d0d0d0')
        button_pressed = colors.get('button_pressed', '#b0b0b0')
        selection_bg = colors.get('selection_background', '#0078d4')
        selection_text = colors.get('selection_text', '#ffffff')
        grid = colors.get('grid', '#e0e0e0')

        # Additional colors
        success = colors.get('success', '#4caf50')
        warning = colors.get('warning', '#ff9800')
        error = colors.get('error', '#f44336')
        toolbar_bg = colors.get('toolbar_bg', bg_secondary)
        panel_entries = colors.get('panel_entries', bg_tertiary)
        panel_filter = colors.get('panel_filter', bg_tertiary)

        stylesheet = f"""
        QMainWindow {{
            background-color: {bg_primary};
            color: {text_primary};
        }}

        QDialog {{
            background-color: {bg_primary};
            color: {text_primary};
        }}

        QWidget {{
            background-color: {bg_primary};
            color: {text_primary};
        }}

        QWidget#customTitleBar {{
            background-color: {bg_secondary};
            color: {text_primary};
            border: 1px solid {border};
            border-bottom: 2px solid {border};
        }}

        QWidget#customTitleBar QLabel {{
            background-color: transparent;
            color: {text_primary};
        }}

        QWidget#customTitleBar QPushButton {{
            background-color: {button_normal};
            border: 1px solid {border};
            border-radius: 3px;
            color: {text_primary};
            font-size: 14pt;
            font-weight: bold;
        }}

        QWidget#customTitleBar QPushButton:hover {{
            background-color: {button_hover};
        }}

        QWidget#customTitleBar QPushButton:pressed {{
            background-color: {button_pressed};
        }}

        QWidget#customTitleBar QPushButton:disabled {{
            background-color: {bg_secondary};
            border: 1px solid {border};
        }}

        QGroupBox {{
            background-color: {panel_bg};
            border: 2px solid {border};
            border-radius: 5px;
            margin-top: 10px;
            padding-top: 10px;
            font-weight: bold;
        }}

        QGroupBox::title {{
            subcontrol-origin: margin;
            left: 10px;
            padding: 0 5px 0 5px;
            color: {text_accent};
        }}

        QPushButton {{
            background-color: {button_normal};
            border: 1px solid {border};
            border-radius: 4px;
            padding: 6px 12px;
            color: {text_primary};
        }}

        QPushButton:hover {{
            background-color: {button_hover};
        }}

        QPushButton:pressed {{
            background-color: {button_pressed};
        }}

        /* QTableWidget and QTableView styling */
        QTableWidget, QTableView {{
            background-color: {table_row_even};
            alternate-background-color: {alternate_row};
            selection-background-color: {selection_bg};
            selection-color: {selection_text};
            gridline-color: {grid};
            border: 1px solid {border};
        }}

        QTableWidget::item, QTableView::item {{
            padding: 4px;
        }}

        QTableWidget::item:selected, QTableView::item:selected {{
            background-color: {selection_bg};
            color: {selection_text};
        }}

        /* QListWidget and QListView styling */
        QListWidget, QListView {{
            background-color: {bg_primary};
            alternate-background-color: {alternate_row};
            selection-background-color: {selection_bg};
            selection-color: {selection_text};
            border: 1px solid {border};
        }}

        QListWidget::item:selected, QListView::item:selected {{
            background-color: {selection_bg};
            color: {selection_text};
        }}

        /* QTreeWidget and QTreeView styling */
        QTreeWidget, QTreeView {{
            background-color: {bg_primary};
            alternate-background-color: {alternate_row};
            selection-background-color: {selection_bg};
            selection-color: {selection_text};
            border: 1px solid {border};
        }}

        QTreeWidget::item:selected, QTreeView::item:selected {{
            background-color: {selection_bg};
            color: {selection_text};
        }}

        QTabWidget::pane {{
            border: 1px solid {border};
            background-color: {bg_primary};
        }}

        QTabBar::tab {{
            background-color: {bg_secondary};
            border: 1px solid {border};
            padding: 8px 16px;
            color: {text_primary};
        }}

        QTabBar::tab:selected {{
            background-color: {accent_primary};
            color: white;
        }}

        QTabBar::tab:hover {{
            background-color: {button_hover};
        }}

        QComboBox {{
            background-color: {button_normal};
            border: 1px solid {border};
            border-radius: 4px;
            padding: 4px;
            color: {text_primary};
        }}

        QComboBox:hover {{
            background-color: {button_hover};
        }}

        QComboBox::drop-down {{
            border: none;
            padding-right: 4px;
        }}

        QSpinBox, QDoubleSpinBox {{
            background-color: {button_normal};
            border: 1px solid {border};
            border-radius: 4px;
            padding: 4px;
            color: {text_primary};
        }}

        QLineEdit {{
            background-color: {bg_primary};
            border: 1px solid {border};
            border-radius: 4px;
            padding: 4px;
            color: {text_primary};
            selection-background-color: {selection_bg};
            selection-color: {selection_text};
        }}

        QTextEdit {{
            background-color: {bg_primary};
            border: 1px solid {border};
            color: {text_primary};
            selection-background-color: {selection_bg};
            selection-color: {selection_text};
        }}

        QCheckBox {{
            color: {text_primary};
        }}

        QRadioButton {{
            color: {text_primary};
        }}

        QLabel {{
            color: {text_primary};
        }}

        QToolBar {{
            background-color: {toolbar_bg};
            border: 1px solid {border};
            spacing: 3px;
        }}

        QStatusBar {{
            background-color: {bg_secondary};
            color: {text_secondary};
        }}

        QScrollBar:vertical {{
            background-color: {bg_secondary};
            width: 14px;
            border: 1px solid {border};
        }}

        QScrollBar::handle:vertical {{
            background-color: {button_normal};
            min-height: 20px;
            border-radius: 4px;
        }}

        QScrollBar::handle:vertical:hover {{
            background-color: {button_hover};
        }}

        QScrollBar:horizontal {{
            background-color: {bg_secondary};
            height: 14px;
            border: 1px solid {border};
        }}

        QScrollBar::handle:horizontal {{
            background-color: {button_normal};
            min-width: 20px;
            border-radius: 4px;
        }}

        QScrollBar::handle:horizontal:hover {{
            background-color: {button_hover};
        }}
        """

        return stylesheet


    # For creating directories if they don't exist
    def ensure_directories_exist(self):
        """Create all project directories if they don't exist"""
        directories = [
            self.working_gta_folder,
            self.assists_folder,
            self.textures_folder,
            self.collisions_folder,
            self.generics_folder,
            self.water_folder,
            self.radar_folder,
            self.gameart_folder,
            self.peds_folder,
            self.vehicles_folder,
            self.weapons_folder
        ]

        for directory in directories:
            try:
                os.makedirs(directory, exist_ok=True)
                print(f"Directory ready: {directory}")
            except Exception as e:
                print(f"Could not create directory {directory}: {e}")

    def get(self, key, default=None):
        """Get setting value for core functions compatibility"""
        # Map old 'project_folder' to assists_folder for compatibility
        if key == 'project_folder':
            return getattr(self, 'assists_folder', default)
        return getattr(self, key, default)

    def _load_settings(self): #vers 2
        """Load settings from file - Windows compatible"""
        try:
            if self.settings_file.exists():
                # FIXED: Use explicit encoding for Windows
                with open(self.settings_file, 'r', encoding='utf-8') as f:
                    loaded_settings = json.load(f)

                settings = self.default_settings.copy()
                settings.update(loaded_settings)

                theme_name = settings.get("theme")
                if theme_name and theme_name not in self.themes:
                    print(f"Theme '{theme_name}' not found, using default")
                    if self.themes:
                        settings["theme"] = list(self.themes.keys())[0]
                    else:
                        settings["theme"] = "App_Factory"

                return settings
        except Exception as e:
            print(f"Error loading settings: {e}")

        return self.default_settings.copy()


    def _load_all_themes(self): #vers 2
        """Load all theme files from apps.themes.directory - Windows compatible"""
        themes = {}
        try:
            if self.themes_dir.exists():
                for theme_file in self.themes_dir.glob("*.json"):
                    try:
                        # FIXED: Use explicit encoding for Windows
                        with open(theme_file, 'r', encoding='utf-8') as f:
                            theme_data = json.load(f)
                            theme_name = theme_file.stem
                            themes[theme_name] = theme_data
                            print(f"  Loaded: {theme_file.name}")
                    except Exception as e:
                        print(f"  Error loading {theme_file.name}: {e}")
            else:
                print(f"Themes directory not found: {self.themes_dir}")
        except Exception as e:
            print(f"Error accessing themes directory: {e}")

        if not themes:
            print("No themes loaded from files, using built-in themes")
            themes = self._get_builtin_themes()
        else:
            print(f"Successfully loaded {len(themes)} themes from files")
            builtin = self._get_builtin_themes()
            for name, data in builtin.items():
                if name not in themes:
                    themes[name] = data
                    print(f"   Added built-in fallback: {name}")

        return themes

    def save_settings(self):
        """Save current settings to file"""
        try:
            self.settings_file.parent.mkdir(parents=True, exist_ok=True)  # ADD THIS LINE
            with open(self.settings_file, 'w', encoding='utf-8') as f:  # ADD encoding='utf-8'
                json.dump(self.current_settings, f, indent=2, ensure_ascii=False)  # ADD ensure_ascii=False
            print(f"Settings saved to: {self.settings_file}")
            return True
        except Exception as e:
            print(f"Error saving settings: {e}")
            return False

        # Default settings
        self.default_settings = {
            "theme": "lightgreen",
            "name": "lightgreen",
            "font_family": "Segoe UI",
            "font_size": 9,
            "font_weight": "normal",
            "font_style": "normal",
            "panel_font_family": "Segoe UI",
            "panel_font_size": 9,
            "panel_font_weight": "normal",
            "button_font_family": "Segoe UI",
            "button_font_size": 9,
            "button_font_weight": "bold",
            "panel_opacity": 95,
            "show_tooltips": True,
            "auto_save": True,
            "grid_size": 5,
            "snap_to_grid": True,
            "show_grid": True,
            "show_perfboard": True,
            "pin_label_size": 8,
            "zoom_sensitivity": 1.2,
            "max_undo_levels": 50,
            "panel_layout": "left",
            "collapsible_panels": True,
            "remember_window_state": True,
            "voice_commands": False,
            "animations": True,
            "sound_effects": False,
            "lcars_sounds": False,
            # Custom button colors
            "custom_button_colors": False,
            "button_import_color": "#2196f3",
            "button_export_color": "#4caf50",
            "button_remove_color": "#f44336",
            "button_update_color": "#ff9800",
            "button_convert_color": "#9c27b0",
            "button_default_color": "#FFECEE",
            # Icon control settings
            "show_button_icons": False,
            "show_menu_icons": True,
            "show_emoji_in_buttons": False,
            # Path remembering settings (from your existing file)
            "remember_img_output_path": True,
            "last_img_output_path": "/home/x2",
            "remember_import_path": True,
            "last_import_path": "",
            "remember_export_path": True,
            "last_export_path": "",
            "default_img_version": "VER2",
            "default_initial_size_mb": 100,
            "auto_create_directory_structure": False,
            "compression_enabled_by_default": False,
            "remember_dialog_positions": True,
            "show_creation_tips": True,
            "validate_before_creation": True,
             # Debug settings
            "debug_mode": False,
            "debug_level": "INFO",
            "debug_categories": ["IMG_LOADING", "TABLE_POPULATION", "BUTTON_ACTIONS"]
        }
        self.defaults = {
            "theme": "Default Green Theme",
            "name": "Default Green",
            "font_family": "Segoe UI",
            "font_size": 9,
            "panel_opacity": 95,
            "show_tooltips": True,
            "auto_save": True,
            "animations": True,
            "sound_effects": False,
            "lcars_sounds": False,

            # Grid and editor settings
            "grid_size": 5,
            "snap_to_grid": True,
            "show_grid": True,
            "show_perfboard": True,
            "pin_label_size": 8,
            "zoom_sensitivity": 1.2,
            "max_undo_levels": 50,

            # Layout settings
            "panel_layout": "left",
            "collapsible_panels": True,
            "remember_window_state": True,
            "voice_commands": False,

            # NEW: Path remembering settings (from your updated file)
            "remember_img_output_path": True,
            "last_img_output_path": "/home/x2",
            "remember_import_path": True,
            "last_import_path": "",
            "remember_export_path": True,
            "last_export_path": "",

            # NEW: IMG creation preferences (from your updated file)
            "default_img_version": "VER2",
            "default_initial_size_mb": 100,
            "auto_create_directory_structure": False,
            "compression_enabled_by_default": False,

            # NEW: Dialog preferences
            "remember_dialog_positions": True,
            "show_creation_tips": True,
            "validate_before_creation": True
        }

        self.themes = self._load_all_themes()
        self.current_settings = self.load_settings()


    def _get_builtin_themes(self): #vers 2
        """Essential built-in themes as fallbacks"""
        return {
            "App_Factory": {
                "name": "App Factory Professional",
                "theme": "App Factory Professional",
                "description": "Clean, organized interface inspired by App Factory",
                "category": "Professional",
                "author": "X-Seti",
                "version": "1.0",
                "colors": {
                    "bg_primary": "#ffffff",
                    "bg_secondary": "#f8f9fa",
                    "bg_tertiary": "#e9ecef",
                    "panel_bg": "#f1f3f4",
                    "accent_primary": "#1976d2",
                    "accent_secondary": "#1565c0",
                    "text_primary": "#212529",
                    "text_secondary": "#495057",
                    "text_accent": "#1976d2",
                    "alternate_row": "#f0f4f8",
                    "button_normal": "#e3f2fd",
                    "button_hover": "#bbdefb",
                    "button_pressed": "#90caf9",
                    "border": "#dee2e6",
                    "selection_background": "#1976d2",
                    "selection_text": "#ffffff",
                    "table_row_even": "#ffffff",
                    "table_row_odd": "#f8f9fa",
                    "success": "#4caf50",
                    "warning": "#ff9800",
                    "error": "#f44336",
                    "grid": "#e9ecef",
                    "pin_default": "#757575",
                    "pin_highlight": "#1976d2",
                    "action_import": "#2196f3",
                    "action_export": "#4caf50",
                    "action_remove": "#f44336",
                    "action_update": "#ff9800",
                    "action_convert": "#9c27b0",
                    "panel_entries": "#e8f5e9",
                    "panel_filter": "#fff3e0",
                    "toolbar_bg": "#fafafa"
                }
            },
            "Default Green": {
                "theme": "Default Green",
                "name": "Default Green",
                "description": "Clean light green theme",
                "category": "Nature",
                "author": "X-Seti",
                "version": "1.0",
                "colors": {
                    "bg_primary": "#f8fff8",
                    "bg_secondary": "#f0f8f0",
                    "bg_tertiary": "#e8f5e8",
                    "panel_bg": "#f1f8f1",
                    "accent_primary": "#4caf50",
                    "accent_secondary": "#388e3c",
                    "text_primary": "#1b5e20",
                    "text_secondary": "#2e7d32",
                    "text_accent": "#388e3c",
                    "alternate_row": "#4dcb59",
                    "button_normal": "#e8f5e8",
                    "button_hover": "#c8e6c9",
                    "button_pressed": "#a5d6a7",
                    "border": "#a5d6a7",
                    "selection_background": "#4caf50",
                    "selection_text": "#ffffff",
                    "table_row_even": "#f8fff8",
                    "table_row_odd": "#e8f5e8",
                    "success": "#4caf50",
                    "warning": "#ff9800",
                    "error": "#f44336",
                    "grid": "#c8e6c9",
                    "pin_default": "#66bb6a",
                    "pin_highlight": "#4caf50",
                    "action_import": "#2196f3",
                    "action_export": "#4caf50",
                    "action_remove": "#f44336",
                    "action_update": "#ff9800",
                    "action_convert": "#9c27b0",
                    "panel_entries": "#e8f5e9",
                    "panel_filter": "#fff3e0",
                    "toolbar_bg": "#f1f8f1"
                }
            }
        }


    def get_last_img_output_path(self) -> str:
        """Get the last used IMG output path"""
        if self.current_settings.get("remember_img_output_path", True):
            return self.current_settings.get("last_img_output_path", "")
        return ""

    def set_last_img_output_path(self, path: str):
        """Set the last used IMG output path"""
        if self.current_settings.get("remember_img_output_path", True):
            self.current_settings["last_img_output_path"] = path
            self.save_settings()

    def get_last_import_path(self) -> str:
        """Get the last used import path"""
        if self.current_settings.get("remember_import_path", True):
            return self.current_settings.get("last_import_path", "")
        return ""

    def set_last_import_path(self, path: str):
        """Set the last used import path"""
        if self.current_settings.get("remember_import_path", True):
            self.current_settings["last_import_path"] = path
            self.save_settings()

    def get_last_export_path(self) -> str:
        """Get the last used export path"""
        if self.current_settings.get("remember_export_path", True):
            return self.current_settings.get("last_export_path", "")
        return ""

    def set_last_export_path(self, path: str):
        """Set the last used export path"""
        if self.current_settings.get("remember_export_path", True):
            self.current_settings["last_export_path"] = path
            self.save_settings()

    def get_default_img_settings(self) -> dict:
        """Get default IMG creation settings"""
        return {
            "version": self.current_settings.get("default_img_version", "VER2"),
            "initial_size_mb": self.current_settings.get("default_initial_size_mb", 100),
            "auto_create_structure": self.current_settings.get("auto_create_directory_structure", False),
            "compression_enabled": self.current_settings.get("compression_enabled_by_default", False)
        }

    def _load_themes_from_files(self):
        """Load themes from JSON files in themes/ directory"""
        themes_dir = Path("themes")
        if not themes_dir.exists():
            print("themes/ directory not found - using hardcoded themes")
            return

        print("Loading themes from files...")
        for theme_file in themes_dir.glob("*.json"):
            try:
                with open(theme_file, 'r') as f:
                    theme_data = json.load(f)

                # Use filename without extension as theme key
                theme_key = theme_file.stem
                self.themes[theme_key] = theme_data

                print(f"Loaded: {theme_key} - {theme_data.get('name', 'Unnamed')}")

            except Exception as e:
                print(f"Failed to load {theme_file}: {e}")

        print(f"Total themes loaded: {len(self.themes)}")

    def _get_default_settings(self):
        """Get default settings - FIXED: This method was missing"""
        return {
            "theme": "App_Factory",
            "font_family": "Arial",
            "font_size": 9,
            "show_tooltips": True,
            "auto_save": True,
            "panel_opacity": 95,
            "remember_img_output_path": True,
            "remember_import_path": True,
            "remember_export_path": True,
            "last_img_output_path": "",
            "last_import_path": "",
            "last_export_path": "",
            "default_img_version": "VER2",
            "default_initial_size_mb": 100,
            "auto_create_directory_structure": False,
            "compression_enabled_by_default": False,
            "debug_mode": False,
            "debug_level": "INFO",
            "debug_categories": [
                "IMG_LOADING",
                "TABLE_POPULATION",
                "BUTTON_ACTIONS",
                "FILE_OPERATIONS",
                "COL_LOADING",
                "COL_PARSING",
                "COL_THREADING",
                "COL_DISPLAY",
                "COL_INTEGRATION"
            ],
            "col_debug_enabled": False,
            "search_enabled": True,
            "performance_mode": True
        }

    def _load_all_themes(self):
        """Unified theme loading method"""
        themes = {}

        print(f"Looking for themes in: {self.themes_dir}")

        # Check if themes directory exists
        if self.themes_dir.exists() and self.themes_dir.is_dir():
            print(f"Found themes directory")

            # Load all .json files from apps.themes.directory
            theme_files = list(self.themes_dir.glob("*.json"))
            print(f"Found {len(theme_files)} theme files")

            for theme_file in theme_files:
                try:
                    print(f"Loading: {theme_file.name}")
                    with open(theme_file, 'r', encoding='utf-8') as f:
                        theme_data = json.load(f)

                    # Use filename without extension as theme key
                    theme_name = theme_file.stem
                    themes[theme_name] = theme_data

                    # Show theme info
                    display_name = theme_data.get('name', theme_name)
                    print(f"Loaded: {theme_name} -> '{display_name}'")

                except json.JSONDecodeError as e:
                    print(f"JSON error in {theme_file.name}: {e}")
                except Exception as e:
                    print(f"Error loading {theme_file.name}: {e}")
        else:
            print(f"Themes directory not found: {self.themes_dir}")

        # Add built-in fallback themes if no themes loaded
        if not themes:
            print("No themes loaded from files, using built-in themes")
            themes = self._get_builtin_themes()
        else:
            print(f"Successfully loaded {len(themes)} themes from files")
            # Add a few essential built-in themes as fallbacks
            builtin = self._get_builtin_themes()
            for name, data in builtin.items():
                if name not in themes:
                    themes[name] = data
                    print(f"Added built-in fallback: {name}")

        return themes

    def save_theme_to_file(self, theme_name, theme_data):
        """Save a theme to the themes folder"""
        try:
            # Ensure themes directory exists
            self.themes_dir.mkdir(exist_ok=True)

            theme_file = self.themes_dir / f"{theme_name}.json"
            with open(theme_file, 'w', encoding='utf-8') as f:
                json.dump(theme_data, f, indent=2)

            # Update local themes
            self.themes[theme_name] = theme_data
            return True

        except Exception as e:
            print(f"Error saving theme {theme_name}: {e}")
            return False

    def save_theme(self, theme_name, theme_data): #vers 2
        """Save theme data to JSON file in themes directory"""
        try:
            # Ensure themes directory exists
            self.themes_dir.mkdir(parents=True, exist_ok=True)

            # Create safe filename
            safe_name = theme_name.lower().replace(' ', '_')
            theme_file = self.themes_dir / f"{safe_name}.json"

            # Write theme to file
            with open(theme_file, 'w', encoding='utf-8') as f:
                json.dump(theme_data, f, indent=2, ensure_ascii=False)

            # Update in-memory themes dict
            self.themes[theme_name] = theme_data

            print(f"Theme saved: {theme_file}")
            return True

        except Exception as e:
            print(f"Failed to save theme: {e}")
            import traceback
            traceback.print_exc()
            return False


    def refresh_themes(self):
        """Reload themes from disk - HOT RELOAD functionality"""
        print("Refreshing themes from disk...")
        old_count = len(self.themes)
        self.themes = self._load_all_themes()
        new_count = len(self.themes)

        print(f"Theme refresh complete: {old_count} -> {new_count} themes")
        return self.themes

    def load_settings(self):
        """Load settings from file"""
        try:
            if self.settings_file.exists():
                with open(self.settings_file, 'r', encoding='utf-8') as f:
                    loaded_settings = json.load(f)

                # Merge with defaults (in case new settings were added)
                settings = self.default_settings.copy()
                settings.update(loaded_settings)

                # FIXED: Validate theme exists after themes are loaded
                theme_name = settings.get("theme")
                if theme_name and theme_name not in self.themes:
                    print(f"Theme '{theme_name}' not found, using default")
                    # Use first available theme or fallback
                    if self.themes:
                        settings["theme"] = list(self.themes.keys())[0]
                    else:
                        settings["theme"] = "lightgreen"

                return settings
        except Exception as e:
            print(f"Error loading settings: {e}")

        return self.default_settings.copy()

    def get_theme_info(self, theme_name: str) -> dict:
        """Get detailed info about a specific theme"""
        if theme_name in self.themes:
            theme = self.themes[theme_name]
            return {
                "name": theme.get("name", theme_name),
                "description": theme.get("description", "No description"),
                "category": theme.get("category", "Uncategorized"),
                "author": theme.get("author", "Unknown"),
                "version": theme.get("version", "1.0"),
                "color_count": len(theme.get("colors", {}))
            }
        return {}


    def get_theme_colors(self, theme_name=None): #vers 4
        """Get colors for specified theme with complete fallback support"""
        if theme_name is None:
            theme_name = self.current_settings.get("theme", "IMG_Factory")

        if theme_name in self.themes:
            colors = self.themes[theme_name].get("colors", {})

            # Add missing colors with smart fallbacks if not in theme
            defaults = {
                'bg_primary': '#ffffff',
                'bg_secondary': '#f5f5f5',
                'bg_tertiary': '#e9ecef',
                'panel_bg': '#f0f0f0',
                'text_primary': '#000000',
                'text_secondary': '#666666',
                'text_accent': '#0066cc',
                'accent_primary': '#0078d4',
                'accent_secondary': '#0A7Ad4',
                'alternate_row': '#fefefe',
                'border': '#cccccc',
                'button_normal': '#e0e0e0',
                'button_hover': '#d0d0d0',
                'button_pressed': '#b1b1b1',
                'selection_background': '#0188c4',
                'selection_text': '#ffffff',
                'table_row_even': '#fcfcfc',
                'table_row_odd': '#f1f1f1',
                'success': '#4caf50',
                'warning': '#ff9800',
                'error': '#f44336',
                'grid': '#e0e0e0',
                'pin_default': '#757575',
                'pin_highlight': '#0078d4',
                'action_import': '#e3f2fd',
                'action_export': '#e8f5e8',
                'action_remove': '#ffebee',
                'action_update': '#fff3e0',
                'action_convert': '#f3e5f5',
                'panel_entries': '#f0fdf4',
                'panel_filter': '#fefce8',
                'toolbar_bg': '#fafafa'
            }

            # Merge defaults with theme colors (theme colors take priority)
            for key, value in defaults.items():
                if key not in colors:
                    colors[key] = value

            return colors
        else:
            print(f"Theme '{theme_name}' not found, using fallback")
            if self.themes:
                fallback_name = list(self.themes.keys())[0]
                print(f"Using fallback theme: {fallback_name}")
                return self.get_theme_colors(fallback_name)
            else:
                print("No themes available! Using hardcoded defaults")
                return self._get_hardcoded_defaults()


    def _get_hardcoded_defaults(self): #vers 3
        """Return hardcoded default colors when no themes are available"""
        return {
            'bg_primary': '#ffffff',
            'bg_secondary': '#f5f5f5',
            'bg_tertiary': '#e9ecef',
            'panel_bg': '#f0f0f0',
            'text_primary': '#000000',
            'text_secondary': '#666666',
            'text_accent': '#0066cc',
            'accent_primary': '#0078d4',
            'accent_secondary': '#0A7Ad4',
            'border': '#cccccc',
            'alternate_row': '#fefefe',
            'button_normal': '#e0e0e0',
            'button_hover': '#d0d0d0',
            'button_pressed': '#b0b0b0',
            'selection_background': '#0078d4',
            'selection_text': '#ffffff',
            'table_row_even': '#ffffff',
            'table_row_odd': '#f5f5f5',
            'success': '#4caf50',
            'warning': '#ff9800',
            'error': '#f44336',
            'grid': '#e0e0e0',
            'pin_default': '#757575',
            'pin_highlight': '#0078d4',
            'action_import': '#e3f2fd',
            'action_export': '#e8f5e8',
            'action_remove': '#ffebee',
            'action_update': '#fff3e0',
            'action_convert': '#f3e5f5',
            'panel_entries': '#f0fdf4',
            'panel_filter': '#fefce8',
            'toolbar_bg': '#fafafa'
        }


    def get_stylesheet(self): #vers 4
        """Generate complete stylesheet for current theme"""
        colors = self.get_theme_colors()
        return self._generate_stylesheet(colors)


    def _darken_color(self, hex_color, factor=0.8): #keep
        """Darken a hex color by a factor"""
        try:
            hex_color = hex_color.lstrip('#')
            rgb = tuple(int(hex_color[i:i+2], 16) for i in (0, 2, 4))
            darkened = tuple(int(c * factor) for c in rgb)
            return f"#{darkened[0]:02x}{darkened[1]:02x}{darkened[2]:02x}"
        except:
            return hex_color

    def _lighten_color(self, hex_color, factor=1.2):
        """Lighten a hex color by a factor"""
        try:
            hex_color = hex_color.lstrip('#')
            rgb = tuple(int(hex_color[i:i+2], 16) for i in (0, 2, 4))
            lightened = tuple(min(255, int(c * factor)) for c in rgb)
            return f"#{lightened[0]:02x}{lightened[1]:02x}{lightened[2]:02x}"
        except:
            return hex_color

    def get_available_themes(self) -> dict:
        """Get all available themes with refresh option"""
        return self.themes

# ALSO UPDATE your get_theme method to handle missing themes:

    def get_theme(self, theme_name=None):
        """Get theme colors with fallback"""
        if theme_name is None:
            theme_name = self.current_settings["theme"]

        # Handle theme name mismatches (lightyellow_theme -> lightyellow)
        if theme_name.endswith('_theme'):
            theme_name = theme_name[:-6]  # Remove '_theme' suffix

        # Return theme or fallback to first available theme
        if theme_name in self.themes:
            return self.themes[theme_name]
        else:
            print(f"Theme '{theme_name}' not found, using fallback")
            fallback_theme = list(self.themes.keys())[0] if self.themes else "LCARS"
            return self.themes.get(fallback_theme, {"colors": {}})

    def get_theme_data(self, theme_name: str) -> dict:
        """Get complete theme data"""
        if theme_name in self.themes:
            return self.themes[theme_name]
        else:
            print(f"Theme '{theme_name}' not found, using fallback")
            fallback_theme = list(self.themes.keys())[0] if self.themes else "App_Factory"
            return self.themes.get(fallback_theme, {"colors": {}})


class SettingsDialog(QDialog): #vers 15
    """Settings dialog for theme and preference management"""

    themeChanged = pyqtSignal(str)  # theme_name
    settingsChanged = pyqtSignal()

    def __init__(self, app_settings, parent=None): #vers 5
        """Initialize settings dialog"""
        super().__init__(parent)

        print(f"Has _update_titlebar_icons: {hasattr(self, '_update_titlebar_icons')}")

        self.setWindowTitle("App Factory Settings")
        self.setMinimumSize(800, 600)
        self.setModal(True)

        self.app_settings = app_settings
        self.original_settings = app_settings.current_settings.copy()
        self._modified_colors = {}
        self.color_editors = {}

        # Initialize icon provider
        self.icons = IconProvider(self)

        # Set default fonts
        from PyQt6.QtGui import QFont
        default_font = QFont("Fira Sans Condensed", 14)
        self.setFont(default_font)
        self.title_font = QFont("Arial", 14)
        self.panel_font = QFont("Arial", 10)
        self.button_font = QFont("Arial", 10)
        self.infobar_font = QFont("Courier New", 9)

        # Setup resize handling
        self.dragging = False
        self.resizing = False
        self.resize_corner = None
        self.resize_margin = 10
        self.resize_direction = None
        self.corner_size = 20
        self.hover_corner = None

        self.drag_position = None
        self.initial_geometry = None
        self.setMouseTracking(True)

        # Apply window mode (custom gadgets or system)
        self._apply_dialog_window_mode()

        self._create_ui()
        self._load_current_settings()


    def _apply_quick_theme(self, theme_name: str):
        """Apply quick theme with animation effect"""
        self.demo_theme_combo.setCurrentText(theme_name)
        self._apply_demo_theme(theme_name)

        # Animate button click
        sender = self.sender()
        if sender:
            original_text = sender.text()
            sender.setText(f"Applied!")
            QTimer.singleShot(1000, lambda: sender.setText(original_text))


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


    def mousePressEvent(self, event): #vers 2
        """Handle mouse press for dragging and resizing"""
        if event.button() == Qt.MouseButton.LeftButton:
            # Check if clicking on corner
            self.resize_corner = self._get_resize_corner(event.pos())

            if self.resize_corner:
                self.resizing = True
                self.drag_position = event.globalPosition().toPoint()
                self.initial_geometry = self.geometry()
            else:
                # Check if clicking on toolbar for dragging
                if self._is_on_draggable_area(event.pos()):
                    self.dragging = True
                    self.drag_position = event.globalPosition().toPoint() - self.frameGeometry().topLeft()

            event.accept()


    def mouseMoveEvent(self, event): #vers 2
        """Handle mouse move for dragging, resizing, and hover effects"""
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
            elif self.dragging:
                self.move(event.globalPosition().toPoint() - self.drag_position)
            event.accept()
        else:
            # Update hover state and cursor
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()  # Trigger repaint for hover effect
            self._update_cursor(corner)


    def mouseReleaseEvent(self, event): #vers 2
        """Handle mouse release"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = False
            self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


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


    def _is_on_draggable_area(self, pos): #vers 3
        """Check if position is on draggable toolbar area (stretch space, not buttons)"""
        if not hasattr(self, 'toolbar'):
            return False

        toolbar_rect = self.toolbar.geometry()
        if not toolbar_rect.contains(pos):
            return False

        # Get all buttons in toolbar
        buttons_to_check = []

        if hasattr(self, 'info_btn'):
            buttons_to_check.append(self.info_btn)
        if hasattr(self, 'minimize_btn'):
            buttons_to_check.append(self.minimize_btn)
        if hasattr(self, 'maximize_btn'):
            buttons_to_check.append(self.maximize_btn)
        if hasattr(self, 'close_btn'):
            buttons_to_check.append(self.close_btn)
        # Should be enabled on selection:


        if not hasattr(self, 'drag_btn'):
            return False

        # Convert to toolbar coordinates
        toolbar_local_pos = self.toolbar.mapFrom(self, pos)

        # Check if clicking on drag button
        return self.drag_btn.geometry().contains(toolbar_local_pos)

        # Check if position is NOT on any button (i.e., on stretch area)
        for btn in buttons_to_check:
            btn_global_rect = btn.geometry()
            btn_rect = btn_global_rect.translated(toolbar_rect.topLeft())
            if btn_rect.contains(pos):
                return False  # On a button, not draggable

        return True  # On empty stretch area, draggable


    def _create_ui(self): #vers 7
        """Create the settings dialog UI"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Store original theme for reset
        self._original_theme = self.app_settings.current_settings.get("theme", "App_Factory")

        # Add custom titlebar if using custom gadgets
        if self.app_settings.current_settings.get("use_custom_gadgets", False):
            self._create_dialog_titlebar()
            layout.addWidget(self.dialog_titlebar)

        # Content area
        content_widget = QWidget()
        content_layout = QVBoxLayout(content_widget)

        # Create tab widget
        self.tabs = QTabWidget()

        # Add tabs
        self.color_picker_tab = self._create_color_picker_tab()
        self.tabs.addTab(self.color_picker_tab, "Colors")

        self.fonts_tab = self._create_fonts_tab()
        self.tabs.addTab(self.fonts_tab, "Fonts")

        self.buttons_tab = self._create_buttons_tab()
        self.tabs.addTab(self.buttons_tab, "Buttons")

        self.gadgets_tab = self._create_gadgets_tab()
        self.tabs.addTab(self.gadgets_tab, "Gadgets")

        self.debug_tab = self._create_debug_tab()
        self.tabs.addTab(self.debug_tab, "Debug")

        self.interface_tab = self._create_interface_tab()
        self.tabs.addTab(self.interface_tab, "Interface")

        self.background_tab = self._create_background_tab()
        self.tabs.addTab(self.background_tab, "Background")

        self.advanced_gadgets_tab = self._create_advanced_gadgets_tab()
        self.tabs.addTab(self.advanced_gadgets_tab, "Advanced Gadgets")

        self.transparency_tab = self._create_transparency_tab()
        self.tabs.addTab(self.transparency_tab, "Transparency")

        self.shadows_tab = self._create_shadows_tab()
        self.tabs.addTab(self.shadows_tab, "Shadows")

        self.ui_management_tab = self._create_ui_management_tab()
        self.tabs.addTab(self.ui_management_tab, "UI Management")

        content_layout.addWidget(self.tabs)

        # Buttons
        button_layout = QHBoxLayout()

        reset_btn = QPushButton("Reset to Defaults")
        reset_btn.clicked.connect(self._reset_to_defaults)
        button_layout.addWidget(reset_btn)

        button_layout.addStretch()

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(self._apply_settings)
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.clicked.connect(self._ok_clicked)
        button_layout.addWidget(ok_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)

        content_layout.addLayout(button_layout)

        layout.addWidget(content_widget)

    def _create_dialog_titlebar(self): #vers 6
        """Create custom title bar for settings dialog - TXD Workshop style"""
        self.dialog_titlebar = QWidget()
        self.dialog_titlebar.setObjectName("customTitleBar")
        self.dialog_titlebar.setFixedHeight(40)

        titlebar_layout = QHBoxLayout(self.dialog_titlebar)
        titlebar_layout.setContentsMargins(4, 4, 4, 4)
        titlebar_layout.setSpacing(4)

        # Settings icon button on the left (decorative)
        settings_icon_btn = QPushButton()
        settings_icon_btn.setIcon(self.icons.settings_icon())
        settings_icon_btn.setFixedSize(32, 32)
        settings_icon_btn.setEnabled(False)  # Just decorative
        titlebar_layout.addWidget(settings_icon_btn)

        # Get parent app name and center title
        titlebar_layout.addStretch()
        app_name = getattr(self.parent(), 'app_name', 'Application') if self.parent() else 'Application'
        title_label = QLabel(f"{app_name} - Settings")
        title_label.setStyleSheet("font-weight: bold; font-size: 11pt;")
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        titlebar_layout.addWidget(title_label)
        titlebar_layout.addStretch()

        # Window control buttons on the right
        minimize_btn = QPushButton()
        minimize_btn.setIcon(self.icons.minimize_icon())
        minimize_btn.setFixedSize(32, 32)
        minimize_btn.clicked.connect(self.showMinimized)
        minimize_btn.setToolTip("Minimize")
        titlebar_layout.addWidget(minimize_btn)

        maximize_btn = QPushButton()
        maximize_btn.setIcon(self.icons.maximize_icon())
        maximize_btn.setFixedSize(32, 32)
        maximize_btn.clicked.connect(self._toggle_dialog_maximize)
        maximize_btn.setToolTip("Maximize")
        self.dialog_maximize_btn = maximize_btn
        titlebar_layout.addWidget(maximize_btn)

        close_btn = QPushButton()
        close_btn.setIcon(self.icons.close_icon())
        close_btn.setFixedSize(32, 32)
        close_btn.clicked.connect(self.reject)
        close_btn.setToolTip("Close")
        titlebar_layout.addWidget(close_btn)

        # Enable dragging
        self.titlebar_drag_position = None
        self.dialog_titlebar.mousePressEvent = self._titlebar_mouse_press
        self.dialog_titlebar.mouseMoveEvent = self._titlebar_mouse_move
        self.dialog_titlebar.mouseDoubleClickEvent = self._titlebar_double_click


    def _random_theme(self):
        """Apply random theme"""
        import random
        themes = list(self.app_settings.themes.keys())
        current = self.demo_theme_combo.currentText()
        themes.remove(current)  # Don't pick the same theme

        random_theme = random.choice(themes)
        self.demo_theme_combo.setCurrentText(random_theme)
        self._apply_demo_theme(random_theme)

        self.demo_log.append(f"Random theme: {random_theme}")


    def get_stylesheet(self): #vers 4
        """Generate complete stylesheet for current theme"""
        colors = self.get_theme_colors()
        return self.app_settings._generate_stylesheet(colors)

    def paintEvent(self, event): #vers 2
        """Paint corner resize triangles"""
        super().paintEvent(event)

        from PyQt6.QtGui import QPainter, QColor, QPen, QBrush, QPainterPath

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Colors
        normal_color = QColor(100, 100, 100, 150)
        hover_color = QColor(150, 150, 255, 200)

        w = self.width()
        h = self.height()
        size = self.corner_size

        # Define corner triangles
        corners = {
            'top-left': [(0, 0), (size, 0), (0, size)],
            'top-right': [(w, 0), (w-size, 0), (w, size)],
            'bottom-left': [(0, h), (size, h), (0, h-size)],
            'bottom-right': [(w, h), (w-size, h), (w, h-size)]
        }

        for corner_name, points in corners.items():
            # Choose color based on hover state
            if self.hover_corner == corner_name:
                painter.setBrush(QBrush(hover_color))
                painter.setPen(QPen(hover_color.darker(120), 1))
            else:
                painter.setBrush(QBrush(normal_color))
                painter.setPen(QPen(normal_color.darker(120), 1))

            # Draw triangle
            path = QPainterPath()
            path.moveTo(points[0][0], points[0][1])
            path.lineTo(points[1][0], points[1][1])
            path.lineTo(points[2][0], points[2][1])
            path.closeSubpath()

            painter.drawPath(path)

        painter.end()

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

    # = CORNER RESIZE METHODS

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


    def _create_color_picker_tab(self): #vers 7
        """Create color picker and theme editor tab - Final layout with logical flow"""
        tab = QWidget()
        main_layout = QHBoxLayout(tab)

        # ========== LEFT PANEL ==========
        left_panel = QWidget()
        left_layout = QVBoxLayout(left_panel)
        left_panel.setMaximumWidth(350)

        # Screen Color Picker Group
        picker_group = QGroupBox("Color Picker")
        picker_layout = QVBoxLayout(picker_group)

        self.color_picker = ColorPickerWidget()
        picker_layout.addWidget(self.color_picker)

        instructions = QLabel("""
    <b>How to use:</b><br>
    1. Click 'Pick Color from Screen'<br>
    2. Move mouse over any color<br>
    3. Left-click to select or "ESC" to cancel<br>
    <br>
    <i>Picked colors can be applied to theme elements</i>
        """)
        instructions.setWordWrap(True)
        instructions.setStyleSheet("padding: 1px; border-radius: 1px;")
        picker_layout.addWidget(instructions)

        left_layout.addWidget(picker_group)


    # - PALETTE COLORS GROUP
        palette_group = QGroupBox("Quick Colors")
        palette_layout = QVBoxLayout(palette_group)

        # Top bar: Grid toggle + Retro menu
        top_bar = QHBoxLayout()
        palette_layout.addLayout(top_bar)

    # - GRID TOGGLE BUTTON
        self.palette_toggle_btn = QPushButton("Grid: 6x8")
        self.palette_toggle_btn.setCheckable(False)
        top_bar.addWidget(self.palette_toggle_btn)

    # - RETRO MENU BUTTON
        self.retro_btn = QPushButton("Retro ▼")
        top_bar.addWidget(self.retro_btn)

    # - PALETTE GRID AREA
        self.palette_grid = QGridLayout()
        palette_layout.addLayout(self.palette_grid)
        left_layout.addWidget(palette_group)

    # - RETRO PALETTE DEFINITIONS
        zx_spectrum = [
            "#000000", "#0000D7", "#D70000", "#D700D7",
            "#00D700", "#00D7D7", "#D7D700", "#D7D7D7",
            "#000000", "#0000FF", "#FF0000", "#FF00FF",
            "#00FF00", "#00FFFF", "#FFFF00", "#FFFFFF"
        ]

        commodore_64 = [
            "#000000", "#FFFFFF", "#813338", "#75CEC8",
            "#8E3C97", "#56AC4D", "#2E2C9B", "#EDF171",
            "#8E5029", "#553800", "#C46C71", "#4A4A4A",
            "#7B7B7B", "#A9FF9F", "#706DEB", "#B2B2B2"
        ]

        amstrad_cpc = [
            "#000000", "#000080", "#0000FF", "#800000", "#800080", "#8000FF",
            "#FF0000", "#FF0080", "#FF00FF", "#008000", "#008080", "#0080FF",
            "#808000", "#808080", "#8080FF", "#00FF00", "#00FF80", "#00FFFF",
            "#FF8000", "#FF8080", "#FF80FF", "#FFFF00", "#FFFF80", "#FFFFFF",
            "#008000", "#00C000", "#C0C000"
        ]

        amiga_default = [
            "#000000", "#111111", "#222222", "#333333", "#444444", "#555555", "#666666", "#777777",
            "#888888", "#999999", "#AAAAAA", "#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE", "#FFFFFF",
            "#0000AA", "#AA0000", "#00AA00", "#AAAA00", "#00AAAA", "#AA00AA", "#AAAAAA", "#FF0000",
            "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF", "#FF8800", "#FF0088", "#8888FF"
        ]

        atari_800 = [
            "#000000", "#404040", "#6C6C6C", "#909090", "#B0B0B0", "#C8C8C8", "#DCDCDC", "#ECECEC",
            "#444400", "#646410", "#848424", "#A0A034", "#B8B840", "#D0D050", "#E8E85C", "#FCFC68",
            "#702800", "#844414", "#985C28", "#AC783C", "#BC8C4C", "#CCA05C", "#DCB468", "#ECC878",
            "#841800", "#983418", "#AC502C", "#C06840", "#D07C50", "#E09060", "#F0A070", "#FFB480",
            "#880000", "#9C2020", "#B03C3C", "#C05858", "#D07070", "#E08888", "#F0A0A0", "#FFB8B8"
        ]

        amiga_aga = [
            "#A69F9E", "#CECECE", "#B9B9B9", "#949694", "#838583", "#777371", "#5C4B4A", "#685555",
            "#A8A2A1", "#898988", "#919291", "#8D8E8D", "#868886", "#818280", "#7C7B79", "#7E7C7B",
            "#8A8A89", "#B1B2B1", "#999594", "#EFEEEF", "#B5A9AA", "#FFFEFF", "#F1EDEE", "#CABFC0",
            "#B2A3A3", "#D4CBCC", "#FAF8F9", "#ECEBEC", "#D6D6D6", "#D2D3D2", "#C8C3C4", "#B8ACAC",
            "#877071", "#DED6D7", "#826A6B", "#482525", "#998585", "#F2EFF0", "#D1D2D1", "#9D9F9D",
            "#959695", "#7C7271", "#563939", "#3F1A1A", "#D8CFD0", "#6E5152", "#290000", "#2E0707",
            "#3E1F1E", "#492F2E", "#5D4242", "#725757", "#B1A2A2", "#F6F3F4", "#CACACA", "#767170",
            "#665A59", "#635655", "#6A605F", "#7A7776", "#8B8D8B", "#A1A2A1", "#AFB0AF", "#9A9C9A",
            "#848684", "#6F7E96", "#5876AC", "#5575AF", "#6079A4", "#76818F", "#828482", "#818281",
            "#808180", "#828382", "#8A8B8A", "#919391", "#8E8F8E", "#878987", "#868682", "#8D8881",
            "#918980", "#8A8782", "#988B86", "#AD9189", "#AF9289", "#A38E87", "#8D8884", "#898B89",
            "#898A89", "#957D6C", "#A87455", "#A97455", "#9E7962", "#8B8179", "#859079", "#8AA565",
            "#8CB05B", "#8BAD5D", "#879871", "#838582", "#968E6B", "#A99755", "#9D9263", "#8B897A",
            "#7E808F", "#7678A4", "#7274AF", "#7375AC", "#7C7E96", "#838484", "#88729A", "#8C61AF",
            "#896DA1", "#857F8B", "#769184", "#5FA785", "#55B086", "#59AC86", "#709784", "#3A1818",
            "#351212", "#5B4A49", "#75706F", "#A49F9E", "#D8D1D1", "#EDE8E9", "#FCFBFC", "#371514",
            "#472D2C", "#6D6463", "#969796", "#BBBBBB", "#5274B1", "#1C62E4", "#145FEC", "#2E68D3",
            "#657BA0", "#7E7E7E", "#7C7B7C", "#808080", "#939493", "#A4A5A4", "#A6A7A6", "#9D9E9D",
            "#8C8E8C", "#9C8C7E", "#A68F7C", "#A58F7C", "#948A80", "#B4938A", "#E8A291", "#ECA391",
            "#D09B8D", "#9C8C86", "#8D8F8D", "#AF714D", "#DC5C16", "#DD5C14", "#C36834", "#977C6B",
            "#889F6A", "#93D13B", "#98EB22", "#97E627", "#8DB457", "#848681", "#B19C4A", "#DDB114",
            "#C2A336", "#958E6D", "#787AA0", "#6365D3", "#5A5CEC", "#5C5EE5", "#7173B1", "#848287",
            "#8E58BA", "#9830EC", "#914BCA", "#877696", "#63A285", "#2CD588", "#14EB8A", "#1EE289",
            "#A5A7A5", "#C1C3C1", "#ACADAC", "#371515", "#645756", "#BBBCBB", "#F8F7F8", "#5D4D4C",
            "#310C0C", "#3C1D1C", "#695E5D", "#999B99", "#D7D7D7", "#C5C5C5", "#4A71B9", "#095BF6",
            "#0058FF", "#1E62E2", "#5F79A5", "#818381", "#7D7D7D", "#7B797B", "#7F7F7F", "#AAABAA",
            "#A2A3A2", "#8C8881", "#A08E7D", "#AC917B", "#978B7F", "#BE968B", "#FBA893", "#FFA994",
            "#DE9F8F", "#A18E87", "#8F918F", "#949594", "#B86D43", "#EC5502", "#EE5400", "#CF6226",
            "#9A7A66", "#89A466", "#96DF2D", "#9CFE10", "#9BF816", "#8EBC4E", "#858681", "#BAA040",
            "#EEB900", "#CDA928", "#988F69", "#7678A5", "#5D5FE2", "#5254FF", "#5557F7", "#6E70B9",
            "#848187", "#9050C4", "#9C20FF", "#9441D7", "#877399", "#5DA885", "#1CE489", "#00FE8B",
            "#0BF48A", "#4CB886", "#ABADAB", "#CDCECD", "#B4B5B4", "#310909", "#624747", "#857B7A"

        ]

    # → 256 colors extracted from saveamiga_pal.png, formatted 8 per row.

        ula_plus = [
            "#000000", "#000154", "#0000AA", "#0000FE", "#270100", "#270055", "#2700A9", "#2800FF",
            "#4A0000", "#4B0055", "#4C00AA", "#4B00FF", "#6E0000", "#700056", "#7000AA", "#6F00FF",
            "#920000", "#940056", "#9300A9", "#9300FF", "#B70100", "#B70055", "#B700AA", "#B800FF",
            "#DA0000", "#DB0056", "#DC00AA", "#DC00FF", "#FE0000", "#FF0054", "#FF00AA", "#FF00FE",
            "#012700", "#012756", "#0027AA", "#0027FF", "#272800", "#262755", "#2727A9", "#2728FF",
            "#4A2700", "#4B2755", "#4B28AA", "#4A27FF", "#6F2700", "#6F2755", "#6E27A9", "#6E27FF",
            "#932700", "#932856", "#9227A9", "#9227FF", "#B72800", "#B62755", "#B727AA", "#B728FF",
            "#DA2700", "#DB2756", "#DB28AA", "#DA27FF", "#FF2700", "#FF2756", "#FF28AA", "#FE27FF",
            "#014B00", "#014B56", "#004AA9", "#004BFF", "#274B01", "#264B54", "#274BAB", "#274CFF",
            "#4A4B00", "#4B4B55", "#4B4CA9", "#4B4CFF", "#6F4B00", "#6F4B55", "#6F4CAA", "#6E4BFF",
            "#934B00", "#934B56", "#924BA9", "#924BFF", "#B74B00", "#B64B55", "#B64BA9", "#B74BFF",
            "#DB4C00", "#DB4B55", "#DB4BAA", "#DB4CFF", "#FF4B00", "#FF4B56", "#FF4CAA", "#FE4BFF",
            "#016F00", "#016F56", "#016FAA", "#006FFF", "#276F01", "#277055", "#276FAA", "#276FFF",
            "#4B6F01", "#4B6F55", "#4B6FAB", "#4B70FF", "#6F6F00", "#6F6F55", "#6F70A9", "#6E6FFF",
            "#936F00", "#936F55", "#926FA9", "#926FFF", "#B76F00", "#B76F56", "#B66FA9", "#B76FFF",
            "#DB6F00", "#DB6F55", "#DB6FA9", "#DB6FFF", "#FF6F00", "#FF6F55", "#FF6FAA", "#FE6FFF",
            "#009300", "#019355", "#0193AA", "#0092FF", "#279301", "#279355", "#2693AA", "#2793FF",
            "#4B9301", "#4B9354", "#4B93AB", "#4B93FF", "#6F9200", "#6F9355", "#6F93AB", "#6F94FF",
            "#939300", "#939355", "#9394A9", "#9293FF", "#B79300", "#B79355", "#B693A9", "#B793FF",
            "#DB9300", "#DA9355", "#DB93A9", "#DB93FF", "#FF9400", "#FF9355", "#FF93AA", "#FF93FF",
            "#00B700", "#00B755", "#01B7AA", "#00B6FF", "#27B700", "#27B755", "#26B7AA", "#27B7FE",
            "#4BB701", "#4CB855", "#4BB7AA", "#4BB7FF", "#70B701", "#6FB754", "#6FB7AB", "#6FB7FF",
            "#93B600", "#93B755", "#93B7AB", "#92B7FE", "#B7B700", "#B7B755", "#B7B8AA", "#B7B7FF",
            "#DBB700", "#DBB756", "#DBB7A9", "#DBB7FF", "#FFB700", "#FFB755", "#FFB7A9", "#FFB7FF",
            "#00DC00", "#00DB55", "#00DCA9", "#01DBFF", "#27DB00", "#27DB54", "#27DBAB", "#27DBFE",
            "#4BDB00", "#4BDB55", "#4BDBAA", "#4BDBFE", "#70DB01", "#6FDB54", "#6FDBAA", "#6FDBFF",
            "#94DB01", "#93DB55", "#93DBAB", "#93DCFF", "#B7DA00", "#B7DB55", "#B7DBAB", "#B7DBFF",
            "#DBDB00", "#DBDB55", "#DBDBA9", "#DBDBFF", "#FFDB00", "#FFDA55", "#FFDBA9", "#FFDBFF",
            "#00FF01", "#00FF55", "#00FFAB", "#00FFFF", "#27FE00", "#27FF54", "#27FFAA", "#27FFFE",
            "#4BFF00", "#4BFF55", "#4BFEAA", "#4BFFFE", "#6FFF00", "#70FF55", "#6FFFAA", "#6FFFFF",
            "#94FF01", "#93FF54", "#93FFAA", "#93FFFF", "#B7FE00", "#B7FF55", "#B7FFAB", "#B7FFFE",
            "#DBFE01", "#DBFF55", "#DCFFAB", "#DBFFFF", "#FFFF00", "#FFFF55", "#FFFFA9", "#FFFFFF"

        ]

        amiga_aga_ex = [
            "#828781", "#2D0001", "#FFFFFF", "#0157FF", "#7C797A", "#ACACAC", "#AB917A", "#FDAA92",
            "#959597", "#EE5500", "#9EFE10", "#EABB00", "#4F53FD", "#9B20FF", "#04FD87", "#CECECE",
            "#000000", "#320001", "#000000", "#CBD800", "#414440", "#4F5551", "#646560", "#747570",
            "#898989", "#9E9B9E", "#A8ABAA", "#BAB9C0", "#D0CDCE", "#E0DFE2", "#F2EDEF", "#FFFFFF",
            "#747174", "#1F4C44", "#7B6AFF", "#FEFEAC", "#7B6D42", "#B4945B", "#FFFFFF", "#DEFEFF",
            "#C5FAFF", "#A5F5FF", "#8CF6FF", "#69F2FF", "#50EFFF", "#32EEFF", "#19E9FE", "#00E6FF",
            "#393939", "#313131", "#292929", "#1F201D", "#1A1818", "#090C08", "#0000FD", "#00029C",
            "#020055", "#010902", "#02DEFC", "#02B8FC", "#FE45FE", "#FB39D3", "#FF2DAB", "#FF2082",
            "#FF1452", "#FF072E", "#FF0002", "#FF0C01", "#FC1800", "#FF2300", "#FF2F02", "#FF4004",
            "#FF4C00", "#FF5802", "#FD6500", "#FF7300", "#FF7F01", "#FF8C04", "#FE9905", "#FDA604",
            "#FFB400", "#FFBB03", "#FCCB00", "#FDD904", "#FFE801", "#FEF102", "#FFFD00", "#F6FD03",
            "#E8FC04", "#D5FF00", "#C4FD04", "#BBFF00", "#ACFE01", "#98FC00", "#8AFF00", "#82FE00",
            "#73FF00", "#62FD01", "#53FE00", "#41FF00", "#39FD00", "#2AFE00", "#18FE00", "#06FE02",
            "#01FE00", "#01F600", "#00EC00", "#00E700", "#00DF00", "#00D600", "#03CF01", "#02C600",
            "#05BC01", "#00B801", "#01AC00", "#01A500", "#029E00", "#019600", "#018D00", "#048500",
            "#027F05", "#007600", "#036D01", "#006405", "#005C04", "#005201", "#004901", "#004600",
            "#033B00", "#003400", "#012D01", "#052300", "#021C03", "#001402", "#280101", "#3F0102",
            "#500100", "#6A0100", "#870002", "#940004", "#AC0104", "#C50004", "#D30200", "#EE0102",
            "#FD0100", "#EB0015", "#E20232", "#CC0046", "#BE025F", "#AB0076", "#9F0092", "#8C00A7",
            "#7602C9", "#6102E1", "#65CF64", "#264522", "#AFFFB0", "#D2C20D", "#BBAB13", "#AD9414",
            "#948115", "#846A1A", "#71541A", "#634222", "#522D21", "#003CF4", "#024AF6", "#004EF3",
            "#015DF4", "#0367F7", "#0771F6", "#0378F4", "#0781F7", "#068DF8", "#0695F7", "#059FF8",
            "#292222", "#F9B0B2", "#EAFEAE", "#CCFE9E", "#9DF975", "#83FB58", "#71F83B", "#53F41A",
            "#A9A6AA", "#F0F0F0", "#200021", "#2B052B", "#470734", "#540F3D", "#6F1042", "#800F4C",
            "#8D1A4E", "#9C1C57", "#B1205F", "#BE2468", "#D82A73", "#E52F83", "#5BA5A9", "#7EEAE8",
            "#F8D093", "#8B6748", "#FCFED9", "#C5996A", "#3B3004", "#FFFFBA", "#FFFFF4", "#8EC993",
            "#50674D", "#CDF0CE", "#6A936A", "#2E362F", "#B2EEAF", "#E5F0E1", "#C7C898", "#67694F",
            "#F5F2CC", "#969369", "#3B3732", "#F3F2B3", "#CCD0D3", "#52BF8C", "#334457", "#30498C",
            "#726764", "#939094", "#A08E7F", "#A49891", "#D25B8A", "#7F3F4E", "#D475B7", "#AE4969",
            "#4C3037", "#D467D2", "#D482CB", "#8A5A88", "#4C3C4C", "#B470B0", "#654967", "#37203A",
            "#976996", "#BD81BC", "#AF383C", "#A63A3F", "#9E3836", "#913637", "#893534", "#7E3B32",
            "#753333", "#6F3733", "#5C95A4", "#8E8F8A", "#827F80", "#9F9C9D", "#968E7C", "#C59A8B",
        ]

        atari_2600_ntsc = [
            "#000000", "#404040", "#6c6c6c", "#909090", "#b0b0b0", "#c8c8c8", "#dcdcdc", "#ececec",
            "#444400", "#646410", "#848424", "#a0a034", "#b8b840", "#d0d050", "#e8e85c", "#fcfc68",
            "#702800", "#844414", "#985c28", "#ac783c", "#bc8c4c", "#cca05c", "#dcb468", "#e8cc7c",
            "#841800", "#983418", "#ac5030", "#c06848", "#d0805c", "#e09470", "#eca880", "#fcbc94",
            "#880000", "#9c2020", "#b03c3c", "#c05858", "#d07070", "#e08888", "#eca0a0", "#fcb4b4",
            "#78005c", "#8c2074", "#a03c88", "#b0589c", "#c070b0", "#d084c0", "#dc9cd0", "#ecb0e0",
            "#480078", "#602090", "#783ca4", "#8c58b8", "#a070cc", "#b484dc", "#c49cec", "#d4b0fc",
            "#140084", "#302098", "#4c3cac", "#6858c0", "#7c70d0", "#9488e0", "#a8a0ec", "#bcb4fc",
            "#000088", "#1c209c", "#3840b0", "#505cc0", "#6874d0", "#7c8ce0", "#90a4ec", "#a4b8fc",
            "#00187c", "#1c3890", "#3854a8", "#5070bc", "#6888cc", "#7c9cdc", "#90b4ec", "#a4c8fc",
            "#002c5c", "#1c4c78", "#386890", "#5084ac", "#689cc0", "#7cb0d0", "#90c4e0", "#a4d4ec",
            "#00402c", "#1c5c48", "#387c64", "#509c80", "#68b494", "#7cc8a8", "#90d8bc", "#a4e8d0",
            "#003c00", "#205c20", "#407c40", "#5c9c5c", "#74b474", "#88cc88", "#9ce09c", "#b0f4b0",
            "#143800", "#345c1c", "#507c38", "#6c9850", "#84b468", "#9ccc7c", "#b0e090", "#c4f4a4",
            "#2c3000", "#4c501c", "#687034", "#848c4c", "#9ca864", "#b0bc78", "#c4d08c", "#d8e4a0",
            "#442800", "#644818", "#846830", "#a08444", "#b8a058", "#ccb46c", "#e0c880", "#f4dc94",
        ]

    # - RETRO PALETTE REGISTRY
        self.retro_palettes = {
            "Amiga OCS": amiga_default,    # 32 colors
            "Amiga AGA": amiga_aga,        # 256 colors
            "Amiga AGA WB": amiga_aga_ex,  # 256 colors
            "C64": commodore_64,           # 16 colors
            "ZX Spectrum": zx_spectrum,    # 16 colors
            "Amstrad CPC": amstrad_cpc,    # 27 colors
            "Atari 800": atari_800,        # 40 colors
            "Atari 2600 NTSC": atari_2600_ntsc,  # 128 colors
            "ULA Plus": ula_plus           # 256 colors
        }

    # - PALETTE-SPECIFIC GRID SIZES

        # Each retro palette has optimal grid dimensions based on its color count
        self.retro_palette_grids = {
            "Amiga OCS": (4, 8),      # 32 colors = 4×8
            "Amiga AGA": (16, 16),    # 256 colors = 16×16
            "Amiga AGA WB": (16, 16),  # 160 colors = 10×16
            "C64": (4, 4),            # 16 colors = 4×4
            "ZX Spectrum": (4, 4),    # 16 colors = 4×4
            "Amstrad CPC": (3, 9),    # 27 colors = 3×9
            "Atari 800": (5, 8),      # 40 colors = 5×8
            "Atari 2600 NTSC": (16, 8),  # 128 colors = 16×8
            "ULA Plus": (16, 16)      # 256 colors = 16×16
        }

    # - GRID SETTINGS
        self.palette_sizes = [(4, 6), (6, 8), (8, 10), (10,12), (8, 16), (12, 12), (16, 16)]
        self.current_palette_index = 1  # start 6x8
        self.current_retro_palette = None  # Track if a retro palette is active

    # - POPULATE GRID FUNCTION
        def populate_palette_grid(colors=None):
            # Clear grid
            while self.palette_grid.count():
                item = self.palette_grid.takeAt(0)
                widget = item.widget()
                if widget:
                    widget.deleteLater()

            # Determine rows and cols based on whether we're using a retro palette
            if self.current_retro_palette and self.current_retro_palette in self.retro_palette_grids:
                rows, cols = self.retro_palette_grids[self.current_retro_palette]
            else:
                rows, cols = self.palette_sizes[self.current_palette_index]

            if colors is None:
                # Default gradient-based palette
                base_colors = [
                    "#000000", "#1A1A1A", "#333333", "#4D4D4D", "#666666", "#808080", "#B3B3B3", "#FFFFFF",
                    "#330000", "#660000", "#990000", "#CC0000", "#FF0000", "#FF4000", "#FF8000", "#FFB366",
                    "#332600", "#664D00", "#997300", "#CC9900", "#FFBF00", "#FFD633", "#FFE680", "#FFF2B3",
                    "#003300", "#006600", "#009900", "#00CC00", "#00FF00", "#40FF40", "#80FF80", "#B3FFB3",
                    "#000033", "#000066", "#000099", "#0000CC", "#0000FF", "#0080FF", "#00BFFF", "#66D9FF",
                    "#330033", "#660066", "#990099", "#CC00CC", "#FF00FF", "#FF33FF", "#FF66FF", "#FF99FF"
                ]
                colors = (base_colors * ((rows * cols // len(base_colors)) + 1))[:rows * cols]
            else:
                colors = (colors * ((rows * cols // len(colors)) + 1))[:rows * cols]

            # Populate buttons
            for i, color in enumerate(colors):
                btn = QPushButton()
                btn.setFixedSize(25, 25)
                btn.setStyleSheet(f"background-color: {color}; border: 1px solid #999;")
                btn.setToolTip(color)
                btn.clicked.connect(lambda checked, c=color: self._apply_palette_color(c))
                r, c_ = divmod(i, cols)
                self.palette_grid.addWidget(btn, r, c_)

    # - TOGGLE GRID SIZE
        def toggle_palette_size():
            self.current_palette_index = (self.current_palette_index + 1) % len(self.palette_sizes)
            rows, cols = self.palette_sizes[self.current_palette_index]
            self.palette_toggle_btn.setText(f"Grid: {rows}x{cols}")
            self.current_retro_palette = None  # Reset retro palette when manually changing grid
            populate_palette_grid()

    # - LOAD RETRO PALETTE
        def load_retro_palette(palette_name):
            self.current_retro_palette = palette_name
            colors = self.retro_palettes[palette_name]
            rows, cols = self.retro_palette_grids[palette_name]
            self.palette_toggle_btn.setText(f"Grid: {rows}x{cols}")
            populate_palette_grid(colors)

        self.palette_toggle_btn.clicked.connect(toggle_palette_size)

        # --- RETRO MENU SETUP ---
        retro_menu = QMenu(self)
        for palette_name in self.retro_palettes.keys():
            action = retro_menu.addAction(palette_name)
            action.triggered.connect(lambda checked, name=palette_name: load_retro_palette(name))
        self.retro_btn.setMenu(retro_menu)

    # - INITIAL POPULATE
        populate_palette_grid()

        # Apply Picked Color Group - MOVED TO LEFT PANEL
        # Color mapping (needed before creating combo)
        self.theme_colors = {
            "bg_primary": "Background - Primary",
            "bg_secondary": "Background - Secondary",
            "bg_tertiary": "Background - Tertiary",
            "panel_bg": "Panel Background",
            "text_primary": "Text - Primary",
            "text_secondary": "Text - Secondary",
            "text_accent": "Text - Accent",
            "accent_primary": "Accent - Primary",
            "accent_secondary": "Accent - Secondary",
            "border": "Border - Cell Color",
            "button_normal": "Button - Normal",
            "button_hover": "Button - Hover",
            "button_pressed": "Button - Pressed",
            "selection_background": "Selection - Background",
            "selection_text": "Selection - Text",
            "table_row_even": "Table Row - Even",
            "table_row_odd": "Table Row - Odd",
            "alternate_row": "Alternate Row",
            "success": "Status - Success",
            "warning": "Status - Warning",
            "error": "Status - Error",
            "grid": "Grid Lines",
            "pin_default": "Pin - Default",
            "pin_highlight": "Pin - Highlight",
            "action_import": "Action - Import",
            "action_export": "Action - Export",
            "action_remove": "Action - Remove",
            "action_update": "Action - Update",
            "action_convert": "Action - Convert",
            "panel_entries": "Panel - Entries",
            "panel_filter": "Panel - Filter",
            "toolbar_bg": "Toolbar Background",
            "button_text_color": "Button Text - Normal",
            "button_text_hover": "Button Text - Hover",
            "button_text_pressed": "Button Text - Pressed",
            "splitter_color_background": "Splitter - Background",
            "splitter_color_shine": "Splitter - Shine",
            "splitter_color_shadow": "Splitter - Shadow",
            "scrollbar_background": "Scrollbar - Background",
            "scrollbar_handle": "Scrollbar - Handle",
            "scrollbar_handle_hover": "Scrollbar - Handle Hover",
            "scrollbar_handle_pressed": "Scrollbar - Handle Pressed",
            "scrollbar_border": "Scrollbar - Border"
        }

        selection_group = QGroupBox("Apply Picked Color")
        selection_layout = QVBoxLayout(selection_group)

        self.selected_element_combo = QComboBox()
        for color_key, color_name in self.theme_colors.items():
            self.selected_element_combo.addItem(color_name, color_key)
        selection_layout.addWidget(self.selected_element_combo)

        apply_color_btn = QPushButton("Apply Picked Color to Element")
        apply_color_btn.clicked.connect(self._apply_picked_color)
        selection_layout.addWidget(apply_color_btn)

        left_layout.addWidget(selection_group)
        left_layout.addStretch()

    # = RIGHT PANEL
        right_panel = QWidget()
        right_layout = QVBoxLayout(right_panel)

        # Theme Selector
        theme_selector_layout = QHBoxLayout()
        theme_selector_layout.addWidget(QLabel(""))

        self.instant_apply_check = QCheckBox("Apply Theme")
        theme_selector_layout.addWidget(self.instant_apply_check)

        self.theme_selector_combo = QComboBox()
        for theme_key, theme_data in self.app_settings.themes.items():
            display_name = theme_data.get("name", theme_key)
            self.theme_selector_combo.addItem(display_name, theme_key)

        current_theme = self.app_settings.current_settings.get("theme")
        if current_theme:
            for i in range(self.theme_selector_combo.count()):
                if self.theme_selector_combo.itemData(i) == current_theme:
                    self.theme_selector_combo.setCurrentIndex(i)
                    break

        self.theme_selector_combo.currentTextChanged.connect(self._on_theme_changed)
        theme_selector_layout.addWidget(self.theme_selector_combo)


        right_layout.addLayout(theme_selector_layout)

        # Scrollable Color Editors - MAIN CONTENT
        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(2)

        # Create color editors
        self.color_editors = {}
        current_theme_key = self.theme_selector_combo.currentData()
        current_colors = self.app_settings.themes.get(current_theme_key, {}).get("colors", {})

        for color_key, color_name in self.theme_colors.items():
            current_value = current_colors.get(color_key, "#ffffff")
            editor = ThemeColorEditor(color_key, color_name, current_value, self)
            editor.colorChanged.connect(self._on_theme_color_changed)
            editor.lockChanged.connect(lambda key, locked: None)  # Handle lock changes
            self.color_editors[color_key] = editor
            scroll_layout.addWidget(editor)

        scroll_layout.addStretch()
        scroll_area.setWidget(scroll_widget)
        right_layout.addWidget(scroll_area)

        # GLOBAL THEME SLIDERS - MOVED TO RIGHT PANEL BOTTOM (above Theme Actions)
        global_sliders_group = QGroupBox("Global Theme Sliders")
        global_sliders_layout = QVBoxLayout(global_sliders_group)

        info_label = QLabel("<b>Adjust ALL colors globally:</b>")
        global_sliders_layout.addWidget(info_label)

        # Global Hue Slider
        hue_layout = QHBoxLayout()
        hue_layout.addWidget(QLabel("Hue:"))
        self.global_hue_slider = QSlider(Qt.Orientation.Horizontal)
        self.global_hue_slider.setMinimum(-180)
        self.global_hue_slider.setMaximum(180)
        self.global_hue_slider.setValue(0)
        hue_layout.addWidget(self.global_hue_slider)
        self.global_hue_value = QLabel("0")
        self.global_hue_value.setFixedWidth(40)
        hue_layout.addWidget(self.global_hue_value)
        global_sliders_layout.addLayout(hue_layout)

        # Global Saturation Slider
        sat_layout = QHBoxLayout()
        sat_layout.addWidget(QLabel("Sat:"))
        self.global_sat_slider = QSlider(Qt.Orientation.Horizontal)
        self.global_sat_slider.setMinimum(-100)
        self.global_sat_slider.setMaximum(100)
        self.global_sat_slider.setValue(0)
        sat_layout.addWidget(self.global_sat_slider)
        self.global_sat_value = QLabel("0")
        self.global_sat_value.setFixedWidth(40)
        sat_layout.addWidget(self.global_sat_value)
        global_sliders_layout.addLayout(sat_layout)

        # Global Brightness Slider
        bri_layout = QHBoxLayout()
        bri_layout.addWidget(QLabel("Bri:"))
        self.global_bri_slider = QSlider(Qt.Orientation.Horizontal)
        self.global_bri_slider.setMinimum(-100)
        self.global_bri_slider.setMaximum(100)
        self.global_bri_slider.setValue(0)
        bri_layout.addWidget(self.global_bri_slider)
        self.global_bri_value = QLabel("0")
        self.global_bri_value.setFixedWidth(40)
        bri_layout.addWidget(self.global_bri_value)
        global_sliders_layout.addLayout(bri_layout)

        # Slider control buttons
        slider_buttons_layout = QHBoxLayout()

        reset_sliders_btn = QPushButton("Reset Sliders")
        reset_sliders_btn.clicked.connect(self._reset_global_sliders)
        slider_buttons_layout.addWidget(reset_sliders_btn)

        lock_all_btn = QPushButton("Lock All")
        lock_all_btn.clicked.connect(self._lock_all_colors)
        slider_buttons_layout.addWidget(lock_all_btn)

        unlock_all_btn = QPushButton("Unlock All")
        unlock_all_btn.clicked.connect(self._unlock_all_colors)
        slider_buttons_layout.addWidget(unlock_all_btn)

        global_sliders_layout.addLayout(slider_buttons_layout)

        right_layout.addWidget(global_sliders_group)

        # THEME ACTIONS GROUP - AT BOTTOM OF RIGHT PANEL
        theme_layout = QHBoxLayout(self)
        theme_actions_group = QGroupBox("Theme Actions")

        # Use horizontal layout instead of vertical
        theme_actions_layout = QHBoxLayout(theme_actions_group)

        # Random Theme button
        random_theme_btn = QPushButton("Random Theme")
        random_theme_btn.setToolTip("Apply a random theme from available themes")
        random_theme_btn.clicked.connect(self._random_theme_from_picker)
        theme_actions_layout.addWidget(random_theme_btn)

        # Save button (saves current theme modifications)
        save_theme_btn = QPushButton("Save")
        save_theme_btn.setToolTip("Save modifications to current theme")
        save_theme_btn.clicked.connect(self._save_current_theme)
        theme_actions_layout.addWidget(save_theme_btn)

        # Save Theme As button
        save_theme_as_btn = QPushButton("Save Theme As...")
        save_theme_as_btn.setToolTip("Save as a new theme file")
        save_theme_as_btn.clicked.connect(self._save_theme_as)
        theme_actions_layout.addWidget(save_theme_as_btn)

        right_layout.addWidget(theme_actions_group)

        # Add panels to main layout
        main_layout.addWidget(left_panel)
        main_layout.addWidget(right_panel, 1)

        # IMPORTANT: Connect sliders AFTER all widgets are created
        self.global_hue_slider.valueChanged.connect(self._on_global_hue_changed)
        self.global_sat_slider.valueChanged.connect(self._on_global_sat_changed)
        self.global_bri_slider.valueChanged.connect(self._on_global_bri_changed)

        return tab


    def _random_theme_from_picker(self): #vers 1
        """Apply random theme from color picker tab"""
        import random

        # Get all available themes
        themes = list(self.app_settings.themes.keys())

        # Get current theme
        current = self.theme_selector_combo.currentData()

        # Remove current theme from options
        if current in themes:
            themes.remove(current)

        if not themes:
            QMessageBox.information(
                self,
                "No Other Themes",
                "No other themes available to randomize."
            )
            return

        # Pick random theme
        random_theme = random.choice(themes)

        # Find and set the theme in combo box
        for i in range(self.theme_selector_combo.count()):
            if self.theme_selector_combo.itemData(i) == random_theme:
                self.theme_selector_combo.setCurrentIndex(i)
                break

        # Show notification
        theme_display_name = self.app_settings.themes[random_theme].get("name", random_theme)
        QMessageBox.information(
            self,
            "Random Theme Applied",
            f"Applied theme: {theme_display_name}"
        )

    def _apply_palette_color(self, color): #vers 1
        """Apply palette color to selected element"""
        selected_data = self.selected_element_combo.currentData()
        if selected_data and selected_data in self.color_editors:
            self.color_editors[selected_data].set_color(color)


    def _create_gadgets_tab(self): #vers 4
        """Create gadgets styling tab with LIVE PREVIEW and proper splitter"""
        tab = QWidget()
        main_layout = QVBoxLayout(tab)

        # Instructions at top
        info_label = QLabel(
            "<b>Widget Styling - Live Preview:</b><br>"
            "Customize widget appearance and see changes instantly in the preview panel."
        )
        info_label.setWordWrap(True)
        info_label.setStyleSheet("padding: 8px; border-radius: 4px;")
        main_layout.addWidget(info_label)

        # Create splitter for left (controls) and right (preview)
        self.gadgets_splitter = QSplitter(Qt.Orientation.Horizontal)

        # ========== LEFT SIDE - CONTROLS ==========
        left_widget = QWidget()
        layout = QVBoxLayout(left_widget)
        layout.setSpacing(10)
        layout.setContentsMargins(5, 5, 5, 5)

        # Scroll area for controls
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(15)

        # ========== BUTTON STYLING ==========
        button_group = QGroupBox("Button Styling")
        button_layout = QVBoxLayout(button_group)

        # Button Shape
        shape_layout = QHBoxLayout()
        shape_layout.addWidget(QLabel("Shape:"))
        self.button_shape_combo = QComboBox()
        self.button_shape_combo.addItems(["Rounded", "Square", "Pill", "Beveled"])
        self.button_shape_combo.currentTextChanged.connect(self._update_gadget_preview)
        shape_layout.addWidget(self.button_shape_combo)
        shape_layout.addStretch()
        button_layout.addLayout(shape_layout)

        # Border Radius
        radius_layout = QHBoxLayout()
        radius_layout.addWidget(QLabel("Border Radius:"))
        self.button_radius_slider = QSlider(Qt.Orientation.Horizontal)
        self.button_radius_slider.setRange(0, 20)
        self.button_radius_slider.setValue(4)
        self.button_radius_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.button_radius_slider.setTickInterval(5)
        self.button_radius_slider.valueChanged.connect(self._update_gadget_preview)
        radius_layout.addWidget(self.button_radius_slider)
        self.button_radius_label = QLabel("4px")
        self.button_radius_label.setFixedWidth(40)
        radius_layout.addWidget(self.button_radius_label)
        button_layout.addLayout(radius_layout)

        self.button_radius_slider.valueChanged.connect(
            lambda v: self.button_radius_label.setText(f"{v}px")
        )

        # Button Height
        height_layout = QHBoxLayout()
        height_layout.addWidget(QLabel("Min Height:"))
        self.button_height_spin = QSpinBox()
        self.button_height_spin.setRange(20, 50)
        self.button_height_spin.setValue(30)
        self.button_height_spin.setSuffix("px")
        self.button_height_spin.valueChanged.connect(self._update_gadget_preview)
        height_layout.addWidget(self.button_height_spin)
        height_layout.addStretch()
        button_layout.addLayout(height_layout)

        # Horizontal Padding
        h_padding_layout = QHBoxLayout()
        h_padding_layout.addWidget(QLabel("Horizontal Padding:"))
        self.button_h_padding_spin = QSpinBox()
        self.button_h_padding_spin.setRange(2, 30)
        self.button_h_padding_spin.setValue(12)
        self.button_h_padding_spin.setSuffix("px")
        self.button_h_padding_spin.valueChanged.connect(self._update_gadget_preview)
        h_padding_layout.addWidget(self.button_h_padding_spin)
        h_padding_layout.addStretch()
        button_layout.addLayout(h_padding_layout)

        # Vertical Padding
        v_padding_layout = QHBoxLayout()
        v_padding_layout.addWidget(QLabel("Vertical Padding:"))
        self.button_v_padding_spin = QSpinBox()
        self.button_v_padding_spin.setRange(2, 20)
        self.button_v_padding_spin.setValue(6)
        self.button_v_padding_spin.setSuffix("px")
        self.button_v_padding_spin.valueChanged.connect(self._update_gadget_preview)
        v_padding_layout.addWidget(self.button_v_padding_spin)
        v_padding_layout.addStretch()
        button_layout.addLayout(v_padding_layout)

        scroll_layout.addWidget(button_group)

        # ========== SLIDER STYLING ==========
        slider_group = QGroupBox("🎚️ Slider Styling")
        slider_layout = QVBoxLayout(slider_group)

        # Slider Height
        slider_height_layout = QHBoxLayout()
        slider_height_layout.addWidget(QLabel("Slider Height:"))
        self.slider_height_spin = QSpinBox()
        self.slider_height_spin.setRange(4, 20)
        self.slider_height_spin.setValue(8)
        self.slider_height_spin.setSuffix("px")
        self.slider_height_spin.valueChanged.connect(self._update_gadget_preview)
        slider_height_layout.addWidget(self.slider_height_spin)
        slider_height_layout.addStretch()
        slider_layout.addLayout(slider_height_layout)

        # Handle Size
        handle_size_layout = QHBoxLayout()
        handle_size_layout.addWidget(QLabel("Handle Size:"))
        self.slider_handle_size = QSpinBox()
        self.slider_handle_size.setRange(12, 30)
        self.slider_handle_size.setValue(16)
        self.slider_handle_size.setSuffix("px")
        self.slider_handle_size.valueChanged.connect(self._update_gadget_preview)
        handle_size_layout.addWidget(self.slider_handle_size)
        handle_size_layout.addStretch()
        slider_layout.addLayout(handle_size_layout)

        # Handle Radius
        slider_radius_layout = QHBoxLayout()
        slider_radius_layout.addWidget(QLabel("Handle Radius:"))
        self.slider_handle_radius = QSlider(Qt.Orientation.Horizontal)
        self.slider_handle_radius.setRange(0, 15)
        self.slider_handle_radius.setValue(8)
        self.slider_handle_radius.valueChanged.connect(self._update_gadget_preview)
        slider_radius_layout.addWidget(self.slider_handle_radius)
        self.slider_handle_radius_label = QLabel("8px")
        self.slider_handle_radius_label.setFixedWidth(40)
        slider_radius_layout.addWidget(self.slider_handle_radius_label)
        slider_layout.addLayout(slider_radius_layout)

        self.slider_handle_radius.valueChanged.connect(
            lambda v: self.slider_handle_radius_label.setText(f"{v}px")
        )

        scroll_layout.addWidget(slider_group)

        # ========== CHECKBOX STYLING ==========
        checkbox_group = QGroupBox("☑️ Checkbox Styling")
        checkbox_layout = QVBoxLayout(checkbox_group)

        # Checkbox Size
        cb_size_layout = QHBoxLayout()
        cb_size_layout.addWidget(QLabel("Checkbox Size:"))
        self.checkbox_size_spin = QSpinBox()
        self.checkbox_size_spin.setRange(12, 30)
        self.checkbox_size_spin.setValue(18)
        self.checkbox_size_spin.setSuffix("px")
        self.checkbox_size_spin.valueChanged.connect(self._update_gadget_preview)
        cb_size_layout.addWidget(self.checkbox_size_spin)
        cb_size_layout.addStretch()
        checkbox_layout.addLayout(cb_size_layout)

        # Checkbox Radius
        cb_radius_layout = QHBoxLayout()
        cb_radius_layout.addWidget(QLabel("Border Radius:"))
        self.checkbox_radius_slider = QSlider(Qt.Orientation.Horizontal)
        self.checkbox_radius_slider.setRange(0, 9)
        self.checkbox_radius_slider.setValue(3)
        self.checkbox_radius_slider.valueChanged.connect(self._update_gadget_preview)
        cb_radius_layout.addWidget(self.checkbox_radius_slider)
        self.checkbox_radius_label = QLabel("3px")
        self.checkbox_radius_label.setFixedWidth(40)
        cb_radius_layout.addWidget(self.checkbox_radius_label)
        checkbox_layout.addLayout(cb_radius_layout)

        self.checkbox_radius_slider.valueChanged.connect(
            lambda v: self.checkbox_radius_label.setText(f"{v}px")
        )

        scroll_layout.addWidget(checkbox_group)

        # ========== SCROLLBAR STYLING ==========
        scrollbar_group = QGroupBox("📜 Scrollbar Styling")
        scrollbar_layout = QVBoxLayout(scrollbar_group)

        # Scrollbar Width
        sb_width_layout = QHBoxLayout()
        sb_width_layout.addWidget(QLabel("Width:"))
        self.scrollbar_width_slider = QSlider(Qt.Orientation.Horizontal)
        self.scrollbar_width_slider.setRange(6, 20)
        self.scrollbar_width_slider.setValue(12)
        self.scrollbar_width_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.scrollbar_width_slider.setTickInterval(2)
        self.scrollbar_width_slider.valueChanged.connect(self._update_gadget_preview)
        sb_width_layout.addWidget(self.scrollbar_width_slider)
        self.scrollbar_width_label = QLabel("12px")
        self.scrollbar_width_label.setFixedWidth(40)
        sb_width_layout.addWidget(self.scrollbar_width_label)
        scrollbar_layout.addLayout(sb_width_layout)

        self.scrollbar_width_slider.valueChanged.connect(
            lambda v: self.scrollbar_width_label.setText(f"{v}px")
        )

        # Handle Radius
        handle_radius_layout = QHBoxLayout()
        handle_radius_layout.addWidget(QLabel("Handle Radius:"))
        self.scrollbar_handle_radius = QSlider(Qt.Orientation.Horizontal)
        self.scrollbar_handle_radius.setRange(0, 10)
        self.scrollbar_handle_radius.setValue(3)
        self.scrollbar_handle_radius.valueChanged.connect(self._update_gadget_preview)
        handle_radius_layout.addWidget(self.scrollbar_handle_radius)
        self.scrollbar_handle_radius_label = QLabel("3px")
        self.scrollbar_handle_radius_label.setFixedWidth(40)
        handle_radius_layout.addWidget(self.scrollbar_handle_radius_label)
        scrollbar_layout.addLayout(handle_radius_layout)

        self.scrollbar_handle_radius.valueChanged.connect(
            lambda v: self.scrollbar_handle_radius_label.setText(f"{v}px")
        )

        scroll_layout.addWidget(scrollbar_group)

        # ========== SPLITTER STYLING ==========
        splitter_group = QGroupBox("⚡ Splitter Styling")
        splitter_layout = QVBoxLayout(splitter_group)

        # Splitter Width
        sp_width_layout = QHBoxLayout()
        sp_width_layout.addWidget(QLabel("Handle Width:"))
        self.splitter_width_slider = QSlider(Qt.Orientation.Horizontal)
        self.splitter_width_slider.setRange(4, 16)
        self.splitter_width_slider.setValue(8)
        self.splitter_width_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.splitter_width_slider.setTickInterval(2)
        self.splitter_width_slider.valueChanged.connect(self._update_gadget_preview)
        sp_width_layout.addWidget(self.splitter_width_slider)
        self.splitter_width_label = QLabel("8px")
        self.splitter_width_label.setFixedWidth(40)
        sp_width_layout.addWidget(self.splitter_width_label)
        splitter_layout.addLayout(sp_width_layout)

        self.splitter_width_slider.valueChanged.connect(
            lambda v: self.splitter_width_label.setText(f"{v}px")
        )

        # Splitter Radius
        sp_radius_layout = QHBoxLayout()
        sp_radius_layout.addWidget(QLabel("Handle Radius:"))
        self.splitter_radius_slider = QSlider(Qt.Orientation.Horizontal)
        self.splitter_radius_slider.setRange(0, 8)
        self.splitter_radius_slider.setValue(3)
        self.splitter_radius_slider.valueChanged.connect(self._update_gadget_preview)
        sp_radius_layout.addWidget(self.splitter_radius_slider)
        self.splitter_radius_label = QLabel("3px")
        self.splitter_radius_label.setFixedWidth(40)
        sp_radius_layout.addWidget(self.splitter_radius_label)
        splitter_layout.addLayout(sp_radius_layout)

        self.splitter_radius_slider.valueChanged.connect(
            lambda v: self.splitter_radius_label.setText(f"{v}px")
        )

        # Show Grip
        grip_layout = QHBoxLayout()
        self.splitter_show_grip = QCheckBox("Show Grip Handle")
        self.splitter_show_grip.setChecked(True)
        self.splitter_show_grip.stateChanged.connect(self._update_gadget_preview)
        grip_layout.addWidget(self.splitter_show_grip)
        grip_layout.addStretch()
        splitter_layout.addLayout(grip_layout)

        scroll_layout.addWidget(splitter_group)

        # ========== ADVANCED STYLING ==========
        advanced_group = QGroupBox("⚙️ Advanced Styling")
        advanced_layout = QVBoxLayout(advanced_group)

        # Panel Opacity
        opacity_layout = QHBoxLayout()
        opacity_layout.addWidget(QLabel("Panel Opacity:"))
        self.panel_opacity_slider = QSlider(Qt.Orientation.Horizontal)
        self.panel_opacity_slider.setRange(50, 100)
        self.panel_opacity_slider.setValue(95)
        self.panel_opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.panel_opacity_slider.setTickInterval(10)
        self.panel_opacity_slider.valueChanged.connect(self._update_gadget_preview)
        opacity_layout.addWidget(self.panel_opacity_slider)
        self.panel_opacity_label = QLabel("95%")
        self.panel_opacity_label.setFixedWidth(40)
        opacity_layout.addWidget(self.panel_opacity_label)
        advanced_layout.addLayout(opacity_layout)

        self.panel_opacity_slider.valueChanged.connect(
            lambda v: self.panel_opacity_label.setText(f"{v}%")
        )

        # Enable Animations
        anim_layout = QHBoxLayout()
        self.enable_animations = QCheckBox("Enable Hover Animations")
        self.enable_animations.setChecked(True)
        self.enable_animations.stateChanged.connect(self._update_gadget_preview)
        anim_layout.addWidget(self.enable_animations)
        anim_layout.addStretch()
        advanced_layout.addLayout(anim_layout)

        scroll_layout.addWidget(advanced_group)

        scroll_layout.addStretch()
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        # Reset button
        reset_btn = QPushButton("Reset to Defaults")
        reset_btn.clicked.connect(self._reset_gadget_styles)
        layout.addWidget(reset_btn)

        self.gadgets_splitter.addWidget(left_widget)

        # ========== RIGHT SIDE - LIVE PREVIEW PANEL ==========
        preview_panel = QWidget()
        preview_layout = QVBoxLayout(preview_panel)
        preview_layout.setContentsMargins(10, 10, 10, 10)

        preview_title = QLabel("<b>Live Preview</b>")
        preview_title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        preview_title.setStyleSheet("font-size: 12pt; padding: 5px;")
        preview_layout.addWidget(preview_title)

        # Preview frame
        self.preview_frame = QFrame()
        self.preview_frame.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Sunken)
        self.preview_frame.setMinimumSize(300, 400)
        preview_frame_layout = QVBoxLayout(self.preview_frame)

        # Sample buttons
        preview_frame_layout.addWidget(QLabel("Button Samples:"))
        self.preview_btn_normal = QPushButton("Normal Button")
        preview_frame_layout.addWidget(self.preview_btn_normal)

        self.preview_btn_primary = QPushButton("Primary Action")
        preview_frame_layout.addWidget(self.preview_btn_primary)

        self.preview_btn_danger = QPushButton("Remove")
        preview_frame_layout.addWidget(self.preview_btn_danger)

        preview_frame_layout.addSpacing(20)

        # Sample sliders
        preview_frame_layout.addWidget(QLabel("Slider Samples:"))
        self.preview_slider_h = QSlider(Qt.Orientation.Horizontal)
        self.preview_slider_h.setRange(0, 100)
        self.preview_slider_h.setValue(50)
        preview_frame_layout.addWidget(self.preview_slider_h)

        preview_frame_layout.addSpacing(20)

        # Sample checkboxes
        preview_frame_layout.addWidget(QLabel("Checkbox Samples:"))
        self.preview_checkbox1 = QCheckBox("Enable feature A")
        self.preview_checkbox1.setChecked(True)
        preview_frame_layout.addWidget(self.preview_checkbox1)

        self.preview_checkbox2 = QCheckBox("Enable feature B")
        preview_frame_layout.addWidget(self.preview_checkbox2)

        preview_frame_layout.addSpacing(20)

        # Sample text area with scrollbar
        preview_frame_layout.addWidget(QLabel("Scrollbar Sample:"))
        self.preview_text = QTextEdit()
        self.preview_text.setPlainText(
            "IMG Factory File Browser\n"
            "======================\n\n"
            "gta3.img - 1,245 entries\n"
            "gta_int.img - 892 entries\n"
            "player.img - 156 entries\n\n"
            "DFF Models:\n"
            "• admiral.dff\n"
            "• banshee.dff\n"
            "• infernus.dff\n"
            "• patriot.dff\n"
            "• rhino.dff\n\n"
            "TXD Textures:\n"
            "• admiral.txd\n"
            "• banshee.txd\n"
            "• infernus.txd\n\n"
            "COL Collision:\n"
            "• vehicles.col\n"
            "• buildings.col\n"
            "• peds.col\n\n"
            "Total files: 2,293\n"
            "Total size: 1.2 GB\n"
            "RW Version: 3.6.0.0\n"
        )
        self.preview_text.setMaximumHeight(150)
        preview_frame_layout.addWidget(self.preview_text)

        preview_frame_layout.addSpacing(20)

        # Sample splitter
        preview_frame_layout.addWidget(QLabel("Splitter Sample:"))
        self.preview_splitter = QSplitter(Qt.Orientation.Horizontal)
        left_sample = QLabel("Left Panel")
        left_sample.setFrameStyle(QFrame.Shape.Box)
        left_sample.setAlignment(Qt.AlignmentFlag.AlignCenter)
        left_sample.setMinimumHeight(80)
        right_sample = QLabel("Right Panel")
        right_sample.setFrameStyle(QFrame.Shape.Box)
        right_sample.setAlignment(Qt.AlignmentFlag.AlignCenter)
        right_sample.setMinimumHeight(80)
        self.preview_splitter.addWidget(left_sample)
        self.preview_splitter.addWidget(right_sample)
        preview_frame_layout.addWidget(self.preview_splitter)

        preview_frame_layout.addStretch()

        preview_layout.addWidget(self.preview_frame)

        self.gadgets_splitter.addWidget(preview_panel)

        # Set initial splitter sizes (40% controls, 60% preview)
        self.gadgets_splitter.setSizes([400, 600])

        main_layout.addWidget(self.gadgets_splitter)

        # Initial preview update
        self._update_gadget_preview()

        return tab


    def _update_gadget_preview(self): #vers 2
        """Update live preview with current gadget settings"""
        if not hasattr(self, 'preview_frame'):
            return

        # Get current theme colors
        theme_colors = self.app_settings.get_theme_colors()

        # Collect gadget settings
        button_radius = self.button_radius_slider.value()
        button_height = self.button_height_spin.value()
        button_h_padding = self.button_h_padding_spin.value()
        button_v_padding = self.button_v_padding_spin.value()

        slider_height = self.slider_height_spin.value()
        slider_handle_size = self.slider_handle_size.value()
        slider_handle_radius = self.slider_handle_radius.value()

        checkbox_size = self.checkbox_size_spin.value()
        checkbox_radius = self.checkbox_radius_slider.value()

        scrollbar_width = self.scrollbar_width_slider.value()
        scrollbar_radius = self.scrollbar_handle_radius.value()

        splitter_width = self.splitter_width_slider.value()
        splitter_radius = self.splitter_radius_slider.value()
        show_grip = self.splitter_show_grip.isChecked()

        panel_opacity = self.panel_opacity_slider.value()
        enable_animations = self.enable_animations.isChecked()

        # Build button stylesheet
        button_style = f"""
        QPushButton {{
            background-color: {theme_colors.get('button_normal', '#e0e0e0')};
            color: {theme_colors.get('text_primary', '#000000')};
            border: 1px solid {theme_colors.get('border', '#cccccc')};
            border-radius: {button_radius}px;
            padding: {button_v_padding}px {button_h_padding}px;
            min-height: {button_height}px;
            font-weight: bold;
        }}
        QPushButton:hover {{
            background-color: {theme_colors.get('button_hover', '#d0d0d0')};
        }}
        QPushButton:pressed {{
            background-color: {theme_colors.get('button_pressed', '#b0b0b0')};
        }}
        """

        # Apply to preview buttons
        if hasattr(self, 'preview_btn_normal'):
            self.preview_btn_normal.setStyleSheet(button_style)

        if hasattr(self, 'preview_btn_primary'):
            primary_style = button_style.replace(
                theme_colors.get('button_normal', '#e0e0e0'),
                theme_colors.get('action_export', '#e8f5e8')
            )
            self.preview_btn_primary.setStyleSheet(primary_style)

        if hasattr(self, 'preview_btn_danger'):
            danger_style = button_style.replace(
                theme_colors.get('button_normal', '#e0e0e0'),
                theme_colors.get('action_remove', '#ffebee')
            )
            self.preview_btn_danger.setStyleSheet(danger_style)

        # Build slider stylesheet
        slider_style = f"""
        QSlider::groove:horizontal {{
            height: {slider_height}px;
            background: {theme_colors.get('bg_tertiary', '#e0e0e0')};
            border: 1px solid {theme_colors.get('border', '#d0d0d0')};
            border-radius: {slider_height // 2}px;
        }}
        QSlider::handle:horizontal {{
            background: {theme_colors.get('accent_primary', '#0078d4')};
            border: 1px solid {theme_colors.get('accent_secondary', '#0066b8')};
            width: {slider_handle_size}px;
            height: {slider_handle_size}px;
            margin: -{(slider_handle_size - slider_height) // 2}px 0;
            border-radius: {slider_handle_radius}px;
        }}
        QSlider::handle:horizontal:hover {{
            background: {theme_colors.get('accent_secondary', '#0066b8')};
        }}
        """

        if hasattr(self, 'preview_slider_h'):
            self.preview_slider_h.setStyleSheet(slider_style)

        # Build checkbox stylesheet
        checkbox_style = f"""
        QCheckBox::indicator {{
            width: {checkbox_size}px;
            height: {checkbox_size}px;
            border: 2px solid {theme_colors.get('border', '#d0d0d0')};
            border-radius: {checkbox_radius}px;
            background-color: {theme_colors.get('bg_primary', '#ffffff')};
        }}
        QCheckBox::indicator:checked {{
            background-color: {theme_colors.get('accent_primary', '#0078d4')};
            border-color: {theme_colors.get('accent_primary', '#0078d4')};
            image: url(data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMTIiIGhlaWdodD0iMTIiIHZpZXdCb3g9IjAgMCAxMiAxMiIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj48cGF0aCBkPSJNMTAuNSAyLjVMNC41IDguNSAyIDYiIHN0cm9rZT0id2hpdGUiIHN0cm9rZS13aWR0aD0iMiIgZmlsbD0ibm9uZSIvPjwvc3ZnPg==);
        }}
        QCheckBox::indicator:hover {{
            border-color: {theme_colors.get('accent_primary', '#0078d4')};
        }}
        """

        if hasattr(self, 'preview_checkbox1'):
            self.preview_checkbox1.setStyleSheet(checkbox_style)
        if hasattr(self, 'preview_checkbox2'):
            self.preview_checkbox2.setStyleSheet(checkbox_style)

        # Build scrollbar stylesheet
        scrollbar_style = f"""
        QScrollBar:vertical {{
            width: {scrollbar_width}px;
            background-color: {theme_colors.get('scrollbar_background', '#f0f0f0')};
            border: 1px solid {theme_colors.get('scrollbar_border', '#d0d0d0')};
        }}
        QScrollBar::handle:vertical {{
            background-color: {theme_colors.get('scrollbar_handle', '#c0c0c0')};
            border-radius: {scrollbar_radius}px;
            min-height: 20px;
        }}
        QScrollBar::handle:vertical:hover {{
            background-color: {theme_colors.get('scrollbar_handle_hover', '#a0a0a0')};
        }}
        QScrollBar::handle:vertical:pressed {{
            background-color: {theme_colors.get('scrollbar_handle_pressed', '#909090')};
        }}
        QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
            height: 0px;
        }}
        """

        if hasattr(self, 'preview_text'):
            self.preview_text.setStyleSheet(scrollbar_style)

        # Build splitter stylesheet
        grip_dots = ""
        if show_grip:
            grip_dots = f"""
            QSplitter::handle:horizontal {{
                background: qlineargradient(x1:0, y1:0, x2:1, y2:0,
                    stop:0.4 {theme_colors.get('bg_secondary', '#f0f0f0')},
                    stop:0.5 {theme_colors.get('border', '#d0d0d0')},
                    stop:0.6 {theme_colors.get('bg_secondary', '#f0f0f0')});
            }}
            """
        else:
            grip_dots = ""

        splitter_style = f"""
        QSplitter::handle:horizontal {{
            background-color: {theme_colors.get('bg_secondary', '#f0f0f0')};
            border: 1px solid {theme_colors.get('border', '#d0d0d0')};
            width: {splitter_width}px;
            border-radius: {splitter_radius}px;
            margin: 1px;
        }}
        {grip_dots}
        QSplitter::handle:horizontal:hover {{
            background-color: {theme_colors.get('bg_tertiary', '#e0e0e0')};
        }}
        """

        if hasattr(self, 'preview_splitter'):
            self.preview_splitter.setStyleSheet(splitter_style)

        # Apply to main gadgets splitter too
        if hasattr(self, 'gadgets_splitter'):
            self.gadgets_splitter.setStyleSheet(splitter_style)

        # Mark as modified
        self._gadget_modified = True


    def _reset_gadget_styles(self): #vers 3
        """Reset gadget styles to defaults and update preview"""
        self.button_shape_combo.setCurrentText("Rounded")
        self.button_radius_slider.setValue(4)
        self.button_height_spin.setValue(30)
        self.button_h_padding_spin.setValue(12)
        self.button_v_padding_spin.setValue(6)

        self.slider_height_spin.setValue(8)
        self.slider_handle_size.setValue(16)
        self.slider_handle_radius.setValue(8)

        self.checkbox_size_spin.setValue(18)
        self.checkbox_radius_slider.setValue(3)

        self.scrollbar_width_slider.setValue(12)
        self.scrollbar_handle_radius.setValue(3)

        self.splitter_width_slider.setValue(8)
        self.splitter_radius_slider.setValue(3)
        self.splitter_show_grip.setChecked(True)

        self.panel_opacity_slider.setValue(95)
        self.enable_animations.setChecked(True)

        # Update preview
        self._update_gadget_preview()


    def _collect_gadget_styles(self): #vers 2
        """Collect current gadget style settings"""
        return {
            "button_shape": self.button_shape_combo.currentText(),
            "button_border_radius": self.button_radius_slider.value(),
            "button_min_height": self.button_height_spin.value(),
            "button_padding_horizontal": self.button_h_padding_spin.value(),
            "button_padding_vertical": self.button_v_padding_spin.value(),

            "slider_height": self.slider_height_spin.value(),
            "slider_handle_size": self.slider_handle_size.value(),
            "slider_handle_radius": self.slider_handle_radius.value(),

            "checkbox_size": self.checkbox_size_spin.value(),
            "checkbox_border_radius": self.checkbox_radius_slider.value(),

            "scrollbar_width": self.scrollbar_width_slider.value(),
            "scrollbar_handle_radius": self.scrollbar_handle_radius.value(),

            "splitter_handle_width": self.splitter_width_slider.value(),
            "splitter_border_radius": self.splitter_radius_slider.value(),
            "splitter_show_grip": self.splitter_show_grip.isChecked(),

            "panel_opacity": self.panel_opacity_slider.value(),
            "enable_animations": self.enable_animations.isChecked()
        }


    def _browse_background_image(self, target): #vers 1
        """Browse for background image"""
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            f"Select {target.capitalize()} Background Image",
            "",
            "Images (*.png *.jpg *.jpeg *.bmp);;All Files (*)"
        )

        if file_path:
            if target == "panel":
                self.panel_bg_path.setText(file_path)
            elif target == "button":
                self.button_bg_path.setText(file_path)
            self._on_gadget_changed()


    def _clear_background_image(self, target): #vers 1
        """Clear background image"""
        if target == "panel":
            self.panel_bg_path.clear()
        elif target == "button":
            self.button_bg_path.clear()
        self._on_gadget_changed()


    def _preview_gadget_styles(self): #vers 1
        """Preview gadget style changes"""
        # Collect current gadget settings
        gadget_styles = self._collect_gadget_styles()

        # Apply to current dialog as preview
        self._apply_gadget_styles_to_dialog(gadget_styles)

        QMessageBox.information(
            self,
            "Preview Applied",
            "Gadget styles have been applied to this dialog as a preview.\n"
            "Click 'Save' to apply to theme, or 'Cancel' to discard."
        )


    def _apply_gadget_styles_to_dialog(self, styles): #vers 1
        """Apply gadget styles to current dialog for preview"""
        # This would generate and apply stylesheet based on gadget settings
        # Implementation depends on how you want to apply these styles
        pass

    def _create_buttons_tab(self): #vers 1
        """Create buttons customization tab with light/dark sub-tabs"""
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Instructions
        info_label = QLabel(
            "<b>Customize Button Colors:</b><br>"
            "Edit colors for each button panel. Colors are saved per theme.<br>"
            "Light themes use pastel colors, dark themes use solid colors."
        )
        info_label.setWordWrap(True)
        layout.addWidget(info_label)

        # Light/Dark theme toggle
        theme_type_group = QGroupBox("Button Color Mode")
        theme_type_layout = QHBoxLayout(theme_type_group)

        theme_type_layout.addWidget(QLabel("Editing colors for:"))
        self.button_theme_type_combo = QComboBox()
        self.button_theme_type_combo.addItems(["Light Theme Buttons", "Dark Theme Buttons"])
        self.button_theme_type_combo.currentTextChanged.connect(self._on_button_theme_type_changed)
        theme_type_layout.addWidget(self.button_theme_type_combo)
        theme_type_layout.addStretch()

        layout.addWidget(theme_type_group)

        # Sub-tabs for different button panels
        self.button_panels_tabs = QTabWidget()

        # IMG Files panel
        img_files_tab = self._create_button_panel_editor("img_files", [
            ("Open", "open"),
            ("Close", "close"),
            ("Close All", "close_all"),
            ("Rebuild", "rebuild"),
            ("Save Entry", "save_entry"),
            ("Rebuild All", "rebuild_all"),
            ("Merge", "merge"),
            ("Split", "split"),
            ("Convert", "convert")
        ])
        self.button_panels_tabs.addTab(img_files_tab, "IMG Files Buttons")

        # File Entries panel
        file_entries_tab = self._create_button_panel_editor("file_entries", [
            ("Import", "import"),
            ("Import via", "import_via"),
            ("Export", "export"),
            ("Export via", "export_via"),
            ("Remove", "remove"),
            ("Remove All", "remove_all"),
            ("Refresh", "update"),
            ("Quick Export", "quick_export"),
            ("Pin selected", "pin")
        ])
        self.button_panels_tabs.addTab(file_entries_tab, "File Entries Buttons")

        # Editing Options panel
        editing_options_tab = self._create_button_panel_editor("editing_options", [
            ("Col Edit", "col_edit"),
            ("Txd Edit", "txd_edit"),
            ("Dff Edit", "dff_edit"),
            ("Ipf Edit", "ipf_edit"),
            ("IPL Edit", "ipl_edit"),
            ("IDE Edit", "ide_edit"),
            ("Dat Edit", "dat_edit"),
            ("Zons Edit", "zons_edit"),
            ("Weap Edit", "weap_edit"),
            ("Vehi Edit", "vehi_edit"),
            ("Radar Map", "radar_map"),
            ("Paths Map", "paths_map"),
            ("Waterpro", "waterpro")
        ])
        self.button_panels_tabs.addTab(editing_options_tab, "Editing Options Buttons")

        layout.addWidget(self.button_panels_tabs)

        # Action buttons
        actions_layout = QHBoxLayout()

        reset_btn = QPushButton("Reset to Theme Defaults")
        reset_btn.clicked.connect(self._reset_button_colors_to_defaults)
        actions_layout.addWidget(reset_btn)

        actions_layout.addStretch()

        preview_btn = QPushButton("Preview Button Colors")
        preview_btn.clicked.connect(self._preview_button_colors)
        actions_layout.addWidget(preview_btn)

        layout.addLayout(actions_layout)

        return tab

    def _create_button_panel_editor(self, panel_id, buttons): #vers 1
        """Create editor for a specific button panel"""
        widget = QWidget()
        layout = QVBoxLayout(widget)

        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)

        # Store button color editors
        if not hasattr(self, 'button_color_editors'):
            self.button_color_editors = {}

        self.button_color_editors[panel_id] = {}

        for button_text, button_action in buttons:
            button_key = f"{panel_id}_{button_action}"

            # Get default color
            default_colors = self._get_default_button_colors()
            is_light = self.button_theme_type_combo.currentText() == "Light Theme Buttons"
            color_key = f"button_{panel_id}_{button_action}_{'light' if is_light else 'dark'}"
            default_color = self.app_settings.current_settings.get(color_key, default_colors.get(button_action, "#E3F2FD"))

            # Create color editor
            editor_group = QGroupBox(button_text)
            editor_layout = QHBoxLayout(editor_group)

            # Color preview
            color_preview = QLabel()
            color_preview.setFixedSize(40, 30)
            color_preview.setStyleSheet(f"background-color: {default_color}; border: 1px solid #999;")
            editor_layout.addWidget(color_preview)

            # Color input
            color_input = QLineEdit(default_color)
            color_input.setMaximumWidth(100)
            color_input.textChanged.connect(
                lambda c, key=button_key, prev=color_preview: self._on_button_color_changed(key, c, prev)
            )
            editor_layout.addWidget(color_input)

            # Pick button
            pick_btn = QPushButton("Pick Color")
            pick_btn.clicked.connect(
                lambda checked, inp=color_input: self._pick_button_color(inp)
            )
            editor_layout.addWidget(pick_btn)

            editor_layout.addStretch()

            scroll_layout.addWidget(editor_group)

            # Store for later access
            self.button_color_editors[panel_id][button_action] = {
                'preview': color_preview,
                'input': color_input,
                'key': button_key
            }

        scroll_area.setWidget(scroll_widget)
        layout.addWidget(scroll_area)

        return widget

    def _get_default_button_colors(self): #vers 1
        """Get default button colors based on theme type (light/dark)"""
        is_light = self.button_theme_type_combo.currentText() == "Light Theme Buttons"

        if is_light:
            # Light theme - Pastel colors
            return {
                "open": "#E3F2FD",
                "close": "#FFF3E0",
                "close_all": "#FFF3E0",
                "rebuild": "#E8F5E8",
                "save_entry": "#E8F5E8",
                "rebuild_all": "#E8F5E8",
                "merge": "#F3E5F5",
                "split": "#F3E5F5",
                "convert": "#FFF8E1",
                "import": "#E1F5FE",
                "import_via": "#E1F5FE",
                "export": "#E0F2F1",
                "export_via": "#E0F2F1",
                "remove": "#FFEBEE",
                "remove_all": "#FFEBEE",
                "update": "#F9FBE7",
                "quick_export": "#E0F2F1",
                "pin": "#FCE4EC",
                "col_edit": "#FFE0B2",
                "txd_edit": "#E8EAF6",
                "dff_edit": "#F1F8E9",
                "ipf_edit": "#FFF3E0",
                "ipl_edit": "#FFEBEE",
                "ide_edit": "#E0F2F1",
                "dat_edit": "#F3E5F5",
                "zons_edit": "#E1F5FE",
                "weap_edit": "#FFF8E1",
                "vehi_edit": "#E8F5E8",
                "radar_map": "#FCE4EC",
                "paths_map": "#F9FBE7",
                "waterpro": "#E3F2FD"
            }
        else:
            # Dark theme - Solid darker colors
            return {
                "open": "#1a3a52",
                "close": "#4a3d2d",
                "close_all": "#4a3d2d",
                "rebuild": "#2d4a2d",
                "save_entry": "#2d4a2d",
                "rebuild_all": "#2d4a2d",
                "merge": "#3d2d4a",
                "split": "#3d2d4a",
                "convert": "#4a4a2d",
                "import": "#1a3a52",
                "import_via": "#1a3a52",
                "export": "#1a4a44",
                "export_via": "#1a4a44",
                "remove": "#4a2d2d",
                "remove_all": "#4a2d2d",
                "update": "#4a4a2d",
                "quick_export": "#1a4a44",
                "pin": "#4a2d3d",
                "col_edit": "#4a3d2d",
                "txd_edit": "#2d2d4a",
                "dff_edit": "#3a4a2d",
                "ipf_edit": "#4a4a2d",
                "ipl_edit": "#4a2d2d",
                "ide_edit": "#1a4a44",
                "dat_edit": "#3d2d4a",
                "zons_edit": "#1a3a52",
                "weap_edit": "#4a4a2d",
                "vehi_edit": "#2d4a2d",
                "radar_map": "#4a2d3d",
                "paths_map": "#4a4a2d",
                "waterpro": "#1a3a52"
            }

    def _on_button_color_changed(self, button_key, hex_color, preview_label): #vers 1
        """Handle button color changes"""
        # Update preview
        preview_label.setStyleSheet(f"background-color: {hex_color}; border: 1px solid #999;")

        # Store in settings
        if not hasattr(self, '_modified_button_colors'):
            self._modified_button_colors = {}
        self._modified_button_colors[button_key] = hex_color

    def _pick_button_color(self, color_input): #vers 1
        """Open color picker for button"""
        current_color = QColor(color_input.text())
        color = QColorDialog.getColor(current_color, self, "Pick Button Color")
        if color.isValid():
            color_input.setText(color.name())

    def _on_button_theme_type_changed(self, theme_type): #vers 1
        """Handle switching between light/dark button color sets"""
        # Reload button colors for the selected theme type
        if hasattr(self, 'button_color_editors'):
            default_colors = self._get_default_button_colors()
            is_light = theme_type == "Light Theme Buttons"

            for panel_id, editors in self.button_color_editors.items():
                for button_action, editor_dict in editors.items():
                    color_key = f"button_{panel_id}_{button_action}_{'light' if is_light else 'dark'}"
                    color = self.app_settings.current_settings.get(
                        color_key,
                        default_colors.get(button_action, "#E3F2FD")
                    )
                    editor_dict['input'].setText(color)


    def _toggle_instant_apply(self, enabled: bool):
        """Enhanced instant apply toggle"""
        if enabled:
            current_theme = self.demo_theme_combo.currentText()
            self._apply_demo_theme(current_theme)
            self.preview_status.setText("Instant apply: ON")
            self.demo_log.append("Instant apply enabled")
        else:
            self.preview_status.setText("Instant apply: OFF")
            self.demo_log.append("Instant apply disabled")


    def _change_preview_scope(self, scope: str):
        """Change preview scope"""
        self.demo_log.append(f"Preview scope: {scope}")
        current_theme = self.demo_theme_combo.currentText()
        self._apply_demo_theme(current_theme)


    def _update_theme_info(self):
        """Update theme information display"""
        current_theme = self.demo_theme_combo.currentText()
        if current_theme in self.app_settings.themes:
            theme_data = self.app_settings.themes[current_theme]

            info_text = f"""
            <b>{theme_data.get('name', current_theme)}</b><br>
            <i>{theme_data.get('description', 'No description available')}</i><br><br>

            <b>Colors:</b><br>
            • Primary: {theme_data['colors'].get('accent_primary', 'N/A')}<br>
            • Background: {theme_data['colors'].get('bg_primary', 'N/A')}<br>
            • Text: {theme_data['colors'].get('text_primary', 'N/A')}<br>

            <b>Category:</b> {theme_data.get('category', 'Standard')}<br>
            <b>Author:</b> {theme_data.get('author', 'Unknown')}
            """

            if hasattr(self, 'theme_info_label'):
                self.theme_info_label.setText(info_text)


    def _reset_button_colors_to_defaults(self): #vers 1
        """Reset all button colors to theme defaults"""
        reply = QMessageBox.question(
            self, "Reset Button Colors",
            "Reset all button colors to default values?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            default_colors = self._get_default_button_colors()

            for panel_id, editors in self.button_color_editors.items():
                for button_action, editor_dict in editors.items():
                    default_color = default_colors.get(button_action, "#E3F2FD")
                    editor_dict['input'].setText(default_color)

            QMessageBox.information(self, "Reset Complete", "Button colors reset to defaults")

    def _preview_button_colors(self): #vers 1
        """Preview button color changes"""
        QMessageBox.information(
            self, "Button Preview",
            "Button color preview will be applied when you click 'Apply'.\n\n"
            "Current button colors have been updated in the editors."
        )

    def _collect_current_button_colors(self): #vers 1
        """Collect current button panel colors for theme saving"""
        button_panels = {
            "img_files": [],
            "file_entries": [],
            "editing_options": []
        }

        # Check if we have button color editors
        if not hasattr(self, 'button_color_editors'):
            return button_panels

        # Determine if we're using light or dark colors
        is_light = getattr(self, 'button_theme_type_combo', None)
        is_light = is_light.currentText() == "Light Theme Buttons" if is_light else True

        # Get button definitions
        button_definitions = {
            "img_files": [
                ("Open", "open"), ("Close", "close"), ("Close All", "close_all"),
                ("Rebuild", "rebuild"), ("Save Entry...", "save_entry"),
                ("Rebuild All", "rebuild_all"), ("Merge", "merge"),
                ("Split", "split"), ("Convert", "convert")
            ],
            "file_entries": [
                ("Import", "import"), ("Import via", "import_via"),
                ("Export", "export"), ("Export via", "export_via"),
                ("Remove", "remove"), ("Remove All", "remove_all"),
                ("Refresh", "update"), ("Quick Export", "quick_export"),
                ("Pin selected", "pin")
            ],
            "editing_options": [
                ("Col Edit", "col_edit"), ("Txd Edit", "txd_edit"),
                ("Dff Edit", "dff_edit"), ("Ipf Edit", "ipf_edit"),
                ("IPL Edit", "ipl_edit"), ("IDE Edit", "ide_edit"),
                ("Dat Edit", "dat_edit"), ("Zons Edit", "zons_edit"),
                ("Weap Edit", "weap_edit"), ("Vehi Edit", "vehi_edit"),
                ("Radar Map", "radar_map"), ("Paths Map", "paths_map"),
                ("Waterpro", "waterpro")
            ]
        }

        # Collect colors from editors
        for panel_id, button_list in button_definitions.items():
            for button_text, button_action in button_list:
                # Get color from editor or settings
                color = "#E3F2FD"  # Default

                if panel_id in self.button_color_editors:
                    if button_action in self.button_color_editors[panel_id]:
                        editor = self.button_color_editors[panel_id][button_action]
                        color = editor['input'].text()

                # Add to button panel
                button_panels[panel_id].append({
                    "text": button_text,
                    "action": button_action,
                    "icon": f"edit-{button_action}",  # Icon placeholder
                    "color": color
                })

        return button_panels


    def _create_demo_tab(self) -> QWidget:
        """Create improved demo tab with better layout"""
        tab = QWidget()
        main_layout = QHBoxLayout(tab)  # Changed to horizontal for better space usage

        # Left column - Controls
        left_widget = QWidget()
        left_layout = QVBoxLayout(left_widget)
        left_widget.setMaximumWidth(300)

        # Theme Selection Group
        theme_group = QGroupBox("Theme Selection")
        theme_layout = QVBoxLayout(theme_group)

        # Current theme display
        current_layout = QHBoxLayout()
        current_layout.addWidget(QLabel("Active Theme:"))
        self.current_theme_label = QLabel(self.app_settings.current_settings["theme"])
        self.current_theme_label.setStyleSheet("font-weight: bold; color: #2E7D32;")
        current_layout.addWidget(self.current_theme_label)
        current_layout.addStretch()
        theme_layout.addLayout(current_layout)

        # Theme selector
        preview_layout = QHBoxLayout()
        preview_layout.addWidget(QLabel("Preview:"))
        self.demo_theme_combo = QComboBox()
        available_themes = list(self.app_settings.themes.keys())
        self.demo_theme_combo.addItems(available_themes)
        refresh_themes_btn = QPushButton("Refresh Themes")
        refresh_themes_btn.setToolTip("Reload themes from apps.themes. folder")
        refresh_themes_btn.clicked.connect(self.refresh_themes_in_dialog)
        self.demo_theme_combo.setCurrentText(self.app_settings.current_settings["theme"])
        self.demo_theme_combo.currentTextChanged.connect(self._preview_theme_instantly)

        preview_layout.addWidget(self.demo_theme_combo)
        theme_layout.addLayout(preview_layout)

        left_layout.addWidget(theme_group)

        # Real-time Controls Group
        controls_group = QGroupBox("Live Controls")
        controls_layout = QVBoxLayout(controls_group)

        # Instant apply toggle
        self.instant_apply_check = QCheckBox("Apply changes instantly")
        self.instant_apply_check.setChecked(True)
        self.instant_apply_check.toggled.connect(self._toggle_instant_apply)
        controls_layout.addWidget(self.instant_apply_check)

        # Auto-preview toggle
        self.auto_preview_check = QCheckBox("Auto-preview on selection")
        self.auto_preview_check.setChecked(True)
        controls_layout.addWidget(self.auto_preview_check)

        # Preview scope
        scope_layout = QHBoxLayout()
        scope_layout.addWidget(QLabel("Preview Scope:"))
        self.preview_scope_combo = QComboBox()
        self.preview_scope_combo.addItems(["Demo Only", "Dialog Only", "Full Application"])
        self.preview_scope_combo.setCurrentIndex(2)  # Full Application
        self.preview_scope_combo.currentTextChanged.connect(self._change_preview_scope)
        scope_layout.addWidget(self.preview_scope_combo)
        controls_layout.addLayout(scope_layout)

        left_layout.addWidget(controls_group)

        # Quick Themes Group
        quick_group = QGroupBox("🚀 Quick Themes")
        quick_layout = QVBoxLayout(quick_group)

        # Popular themes
        popular_themes = ["LCARS", "App_Factory", "Deep_Purple", "Cyberpunk", "Matrix"]
        for theme_name in popular_themes:
            if theme_name in self.app_settings.themes:
                quick_btn = QPushButton(f" {theme_name}")
                quick_btn.clicked.connect(lambda checked, t=theme_name: self._apply_quick_theme(t))
                quick_btn.setMinimumHeight(35)
                quick_layout.addWidget(quick_btn)

        # Reset and randomize buttons
        button_layout = QHBoxLayout()
        reset_btn = QPushButton("Reset")
        reset_btn.clicked.connect(self._reset_demo_theme)
        random_btn = QPushButton("Random")
        random_btn.clicked.connect(self._random_theme)
        button_layout.addWidget(reset_btn)
        button_layout.addWidget(random_btn)
        quick_layout.addLayout(button_layout)

        left_layout.addWidget(quick_group)

        # Theme Info Group
        info_group = QGroupBox("Theme Info")
        info_layout = QVBoxLayout(info_group)

        self.theme_info_label = QLabel()
        self.theme_info_label.setWordWrap(True)
        self.theme_info_label.setMinimumHeight(100)
        #self.theme_info_label.setStyleSheet("padding: 8px;  border-radius: 4px;")
        info_layout.addWidget(self.theme_info_label)

        left_layout.addWidget(info_group)
        left_layout.addStretch()

        main_layout.addWidget(left_widget)

        # Right column - Preview Area
        right_widget = QWidget()
        right_layout = QVBoxLayout(right_widget)

        # Preview Header
        preview_header = QGroupBox("Live Preview - App Factory Interface")
        header_layout = QHBoxLayout(preview_header)

        self.preview_status = QLabel("Ready for preview")
        self.preview_status.setStyleSheet("color: #4CAF50; font-weight: bold;")
        header_layout.addWidget(self.preview_status)
        header_layout.addStretch()

        # Preview controls
        self.full_preview_btn = QPushButton("Full Preview")
        self.full_preview_btn.clicked.connect(self._show_full_preview)
        header_layout.addWidget(self.full_preview_btn)

        right_layout.addWidget(preview_header)

        # Sample App Factory Toolbar
        toolbar_group = QGroupBox("Sample Toolbar")
        toolbar_layout = QGridLayout(toolbar_group)

        self.demo_buttons = []
        toolbar_buttons = [
            ("Open IMG", "import", "Open IMG archive"),
            ("Import Files", "import", "Import files to archive"),
            ("Export Selected", "export", "Export selected entries"),
            ("Remove Entry", "remove", "Remove selected entry"),
            ("Refresh", "update", "Refresh entry list"),
            ("Convert Format", "convert", "Convert file format"),
            ("Save Archive", None, "Save current archive"),
            ("Settings", None, "Open settings dialog")
        ]

        for i, (text, action_type, tooltip) in enumerate(toolbar_buttons):
            btn = QPushButton(text)
            if action_type:
                btn.setProperty("action-type", action_type)
            btn.setToolTip(tooltip)
            btn.setMinimumHeight(35)
            self.demo_buttons.append(btn)
            toolbar_layout.addWidget(btn, i // 4, i % 4)

        right_layout.addWidget(toolbar_group)

        # Sample Table
        table_group = QGroupBox("Sample IMG Entries Table")
        table_layout = QVBoxLayout(table_group)

        self.demo_table = QTableWidget(5, 5)
        self.demo_table.setHorizontalHeaderLabels(["Filename", "Type", "Size", "Version", "Status"])
        self.demo_table.setMaximumHeight(180)

        # Auto-resize columns
        self.demo_table.resizeColumnsToContents()
        table_layout.addWidget(self.demo_table)

        right_layout.addWidget(table_group)

        # Sample Log Output
        log_group = QGroupBox("Sample Activity Log")
        log_layout = QVBoxLayout(log_group)

        self.demo_log = QTextEdit()
        self.demo_log.setMaximumHeight(120)
        self.demo_log.setReadOnly(True)

        # Enhanced log content
        initial_log = """App Factory 1.0 - Live Theme Preview
Current IMG: sample_archive.img (150 MB)
Entries loaded: 1,247 files
Active theme: """ + self.app_settings.current_settings["theme"] + """
Live preview mode: ACTIVE
Ready for operations..."""

        self.demo_log.setPlainText(initial_log)
        log_layout.addWidget(self.demo_log)

        right_layout.addWidget(log_group)

        # Preview Statistics
        stats_group = QGroupBox("Preview Statistics")
        stats_layout = QGridLayout(stats_group)

        self.stats_labels = {}
        stats_data = [
            ("Themes Available:", str(len(available_themes))),
            ("Preview Changes:", "0"),
            ("Last Applied:", "None"),
            ("Performance:", "Excellent")
        ]

        for i, (label, value) in enumerate(stats_data):
            stats_layout.addWidget(QLabel(label), i, 0)
            value_label = QLabel(value)
            value_label.setStyleSheet("font-weight: bold; color: #1976D2;")
            self.stats_labels[label] = value_label
            stats_layout.addWidget(value_label, i, 1)

        right_layout.addWidget(stats_group)

        main_layout.addWidget(right_widget)

        # Initialize preview
        self._update_theme_info()
        self._apply_demo_theme(self.app_settings.current_settings["theme"])

        return tab

    def _create_debug_tab(self): #vers 2
        """Create debug settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)

        # Debug Mode Group
        debug_group = QGroupBox("Debug Mode")
        debug_layout = QVBoxLayout(debug_group)

        self.debug_enabled_check = QCheckBox("Enable debug mode")
        self.debug_enabled_check.setChecked(
            self.app_settings.current_settings.get('debug_mode', False)
        )
        debug_layout.addWidget(self.debug_enabled_check)

        # Debug Level
        level_layout = QHBoxLayout()
        level_layout.addWidget(QLabel("Debug Level:"))
        self.debug_level_combo = QComboBox()
        self.debug_level_combo.addItems(["ERROR", "WARNING", "INFO", "DEBUG", "VERBOSE"])
        self.debug_level_combo.setCurrentText(
            self.app_settings.current_settings.get('debug_level', 'INFO')
        )
        level_layout.addWidget(self.debug_level_combo)
        level_layout.addStretch()
        debug_layout.addLayout(level_layout)

        layout.addWidget(debug_group)

        # Debug Categories
        categories_group = QGroupBox("Debug Categories")
        categories_layout = QGridLayout(categories_group)

        self.debug_categories = {}
        default_categories = [
            ('IMG_LOADING', 'IMG file loading'),
            ('TABLE_POPULATION', 'Table population'),
            ('BUTTON_ACTIONS', 'Button actions'),
            ('FILE_OPERATIONS', 'File operations'),
            ('COL_PARSING', 'COL parsing'),
            ('THEME_SYSTEM', 'Theme system')
        ]

        enabled_cats = self.app_settings.current_settings.get('debug_categories', [])
        for i, (cat_id, cat_name) in enumerate(default_categories):
            checkbox = QCheckBox(cat_name)
            checkbox.setChecked(cat_id in enabled_cats)
            self.debug_categories[cat_id] = checkbox
            row = i // 2
            col = i % 2
            categories_layout.addWidget(checkbox, row, col)

        layout.addWidget(categories_group)

        # Clear log button
        clear_btn = QPushButton("Clear Debug Log")
        clear_btn.clicked.connect(self._clear_debug_log)
        layout.addWidget(clear_btn)

        layout.addStretch()
        return widget



    def _update_preview_stats(self):
        """Update preview statistics"""
        if hasattr(self, 'stats_labels'):
            current_count = int(self.stats_labels["Preview Changes:"].text()) + 1
            self.stats_labels["Preview Changes:"].setText(str(current_count))
            self.stats_labels["Last Applied:"].setText(self.demo_theme_combo.currentText())


    def _show_full_preview(self):
        """Show full preview window"""
        QMessageBox.information(self, "Full Preview",
            "Full preview window would open here!\n\n"
            "This would show a complete App Factory interface\n"
            "with the selected theme applied.")


    def _preview_theme_instantly(self, theme_name: str):
        """Enhanced instant preview with better feedback"""
        if hasattr(self, 'auto_preview_check') and self.auto_preview_check.isChecked():
            self._apply_demo_theme(theme_name)
            self._update_theme_info()
            self._update_preview_stats()

            # Update status
            self.preview_status.setText(f"Previewing: {theme_name}")
            self.demo_log.append(f"Theme preview: {theme_name}")


    def _apply_demo_theme(self, theme_name: str): #vers 1
        """Apply theme to demo elements"""
        if theme_name not in self.app_settings.themes:
            return

        # Temporarily update settings for preview
        self.app_settings.current_settings["theme"] = theme_name

        # Apply theme stylesheet
        stylesheet = self.app_settings.get_stylesheet()

        # Apply to demo widgets if instant apply enabled
        if hasattr(self, 'instant_apply_check') and self.instant_apply_check.isChecked():
            self.setStyleSheet(stylesheet)
            self.themeChanged.emit(theme_name)

        if hasattr(self, 'demo_log'):
            self.demo_log.append(f"Previewing: {theme_name}")


    def _reset_demo_theme(self): #vers 1
        """Reset to original theme"""
        original = getattr(self, '_original_theme', self.app_settings.current_settings["theme"])
        if hasattr(self, 'demo_theme_combo'):
            self.demo_theme_combo.setCurrentText(original)
        self._apply_demo_theme(original)


    def _create_fonts_tab(self): #vers 2
        """Create fonts settings tab with multiple font type controls"""
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Instructions
        info_label = QLabel("Configure fonts for different UI elements. Changes are saved to appfactory.settings.json")
        info_label.setWordWrap(True)
        info_label.setStyleSheet("color: #666; font-style: italic; padding: 8px;")
        layout.addWidget(info_label)

        # Scroll area for font groups
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)

        # Font type configurations
        self.font_controls = {}

        font_types = [
            ("default", "Default Font", "General UI text and labels (was Font Family/Size)", "Segoe UI", 9, 8, 24),
            ("title", "Title Font", "Window titles and main headers", "Arial", 14, 10, 32),
            ("panel", "Panel Headers Font", "Group box titles and section headers", "Arial", 10, 8, 18),
            ("button", "Button Font", "All button text", "Arial", 10, 8, 16),
            ("menu", "Menu Font", "Menu bar and menu items", "Segoe UI", 9, 8, 14),
            ("infobar", "Info Bar Font", "Status bar and info display text", "Courier New", 9, 7, 14),
            ("table", "Table/List Font", "Data tables and list views", "Segoe UI", 9, 7, 14),
            ("tooltip", "Tooltip Font", "Hover tooltip text", "Segoe UI", 8, 7, 12)
        ]

        for font_id, title, description, default_family, default_size, min_size, max_size in font_types:
            group = self._create_font_control_group(
                font_id, title, description,
                default_family, default_size,
                min_size, max_size
            )
            scroll_layout.addWidget(group)

        scroll_layout.addStretch()
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        # Action buttons row
        actions_layout = QHBoxLayout()

        reset_fonts_btn = QPushButton("Reset All Fonts to Defaults")
        reset_fonts_btn.clicked.connect(self._reset_all_fonts)
        actions_layout.addWidget(reset_fonts_btn)

        actions_layout.addStretch()

        preview_btn = QPushButton("Preview Font Changes")
        preview_btn.clicked.connect(self._preview_font_changes)
        actions_layout.addWidget(preview_btn)

        layout.addLayout(actions_layout)

        return tab

    def _create_font_control_group(self, font_id, title, description,
                                default_family, default_size,
                                min_size, max_size): #vers 1
        """Create a font control group for specific font type"""
        group = QGroupBox(title)
        layout = QVBoxLayout(group)

        # Description
        desc_label = QLabel(description)
        desc_label.setStyleSheet("color: #888; font-style: italic; font-size: 8pt;")
        layout.addWidget(desc_label)

        # Font controls row
        controls_layout = QHBoxLayout()

        # Font family
        controls_layout.addWidget(QLabel("Font:"))
        font_combo = QFontComboBox()

        # Load current font setting from appfactory.settings.json
        current_family = self.app_settings.current_settings.get(
            f'{font_id}_font_family', default_family
        )
        font_combo.setCurrentFont(QFont(current_family))
        font_combo.currentFontChanged.connect(
            lambda f, fid=font_id: self._on_font_changed(fid, 'family', f.family())
        )
        controls_layout.addWidget(font_combo, 1)

        # Font size
        controls_layout.addWidget(QLabel("Size:"))
        size_spin = QSpinBox()
        size_spin.setRange(min_size, max_size)
        current_size = self.app_settings.current_settings.get(
            f'{font_id}_font_size', default_size
        )
        size_spin.setValue(current_size)
        size_spin.setSuffix(" pt")
        size_spin.setFixedWidth(80)
        size_spin.valueChanged.connect(
            lambda v, fid=font_id: self._on_font_changed(fid, 'size', v)
        )
        controls_layout.addWidget(size_spin)

        # Font weight
        controls_layout.addWidget(QLabel("Weight:"))
        weight_combo = QComboBox()
        weight_combo.addItems(["Normal", "Bold", "Light"])
        current_weight = self.app_settings.current_settings.get(
            f'{font_id}_font_weight', 'Normal'
        )
        weight_combo.setCurrentText(current_weight)
        weight_combo.currentTextChanged.connect(
            lambda w, fid=font_id: self._on_font_changed(fid, 'weight', w)
        )
        weight_combo.setFixedWidth(100)
        controls_layout.addWidget(weight_combo)

        layout.addLayout(controls_layout)

        # Store controls for later access
        self.font_controls[font_id] = {
            'family': font_combo,
            'size': size_spin,
            'weight': weight_combo,
            'group': group,
            'default_family': default_family,
            'default_size': default_size
        }

        return group


    def _on_font_changed(self, font_id, property_type, value): #vers 1
        """Handle font property changes - updates current_settings"""
        if property_type == 'family':
            self.app_settings.current_settings[f'{font_id}_font_family'] = value
        elif property_type == 'size':
            self.app_settings.current_settings[f'{font_id}_font_size'] = value
        elif property_type == 'weight':
            self.app_settings.current_settings[f'{font_id}_font_weight'] = value

        # Mark as modified
        if not hasattr(self, '_fonts_modified'):
            self._fonts_modified = True


    def _preview_font_changes(self): #vers 1
        """Preview font changes in dialog"""
        try:
            # Apply default font to see immediate changes
            if 'default' in self.font_controls:
                family = self.font_controls['default']['family'].currentFont().family()
                size = self.font_controls['default']['size'].value()
                weight = self.font_controls['default']['weight'].currentText()

                font = QFont(family, size)
                if weight == "Bold":
                    font.setWeight(QFont.Weight.Bold)
                elif weight == "Light":
                    font.setWeight(QFont.Weight.Light)

                self.setFont(font)

            # Show preview info
            preview_text = "Font Preview Applied!\n\n"
            for font_id, controls in self.font_controls.items():
                family = controls['family'].currentFont().family()
                size = controls['size'].value()
                weight = controls['weight'].currentText()
                preview_text += f"{controls['group'].title()}: {family} {size}pt {weight}\n"

            QMessageBox.information(
                self,
                "Font Preview",
                preview_text + "\nClick 'Apply' to save changes to appfactory.settings.json"
            )
        except Exception as e:
            QMessageBox.warning(self, "Preview Error", f"Could not preview fonts:\n{e}")


    def _reset_all_fonts(self): #vers 1
        """Reset all fonts to their default values"""
        reply = QMessageBox.question(
            self,
            "Reset Fonts",
            "Reset all fonts to their default values?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            for font_id, controls in self.font_controls.items():
                # Reset to defaults
                default_family = controls['default_family']
                default_size = controls['default_size']

                controls['family'].setCurrentFont(QFont(default_family))
                controls['size'].setValue(default_size)
                controls['weight'].setCurrentText('Normal')

                # Update settings
                self.app_settings.current_settings[f'{font_id}_font_family'] = default_family
                self.app_settings.current_settings[f'{font_id}_font_size'] = default_size
                self.app_settings.current_settings[f'{font_id}_font_weight'] = 'Normal'

            QMessageBox.information(self, "Fonts Reset", "All fonts reset to default values")


    def _load_font_settings(self): #vers 1
        """Load font settings from appfactory.settings.json into controls"""
        font_ids = ['default', 'title', 'panel', 'button', 'menu', 'infobar', 'table', 'tooltip']

        for font_id in font_ids:
            if font_id not in self.font_controls:
                continue

            controls = self.font_controls[font_id]

            # Load family
            family = self.app_settings.current_settings.get(
                f'{font_id}_font_family',
                controls['default_family']
            )
            controls['family'].setCurrentFont(QFont(family))

            # Load size
            size = self.app_settings.current_settings.get(
                f'{font_id}_font_size',
                controls['default_size']
            )
            controls['size'].setValue(size)

            # Load weight
            weight = self.app_settings.current_settings.get(
                f'{font_id}_font_weight',
                'Normal'
            )
            controls['weight'].setCurrentText(weight)

    def _save_font_settings(self): #vers 1
        """Save font settings from controls to app settings and appfactory.settings.json"""
        for font_id, controls in self.font_controls.items():
            family = controls['family'].currentFont().family()
            size = controls['size'].value()
            weight = controls['weight'].currentText()

            self.app_settings.current_settings[f'{font_id}_font_family'] = family
            self.app_settings.current_settings[f'{font_id}_font_size'] = size
            self.app_settings.current_settings[f'{font_id}_font_weight'] = weight

        # Save to appfactory.settings.json
        self.app_settings.save_settings()


    def _create_interface_tab(self): #vers 4
        """Create interface settings tab - UI display options"""
        widget = QWidget()
        layout = QVBoxLayout(widget)

        # Button Display Mode
        button_display_group = QGroupBox("Button Display Mode")
        button_display_layout = QVBoxLayout(button_display_group)

        self.button_display_combo = QComboBox()
        self.button_display_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])

        # Load current setting
        current_mode = self.app_settings.current_settings.get("button_display_mode", "both")
        mode_map = {"both": 0, "icons": 1, "text": 2}
        self.button_display_combo.setCurrentIndex(mode_map.get(current_mode, 0))

        button_display_layout.addWidget(self.button_display_combo)

        hint_label = QLabel("Controls how toolbar buttons are displayed")
        hint_label.setStyleSheet("color: #888; font-style: italic;")
        button_display_layout.addWidget(hint_label)

        layout.addWidget(button_display_group)

        # Window Controls
        window_group = QGroupBox("Window Controls")
        window_layout = QVBoxLayout(window_group)

        self.custom_gadgets_check = QCheckBox("Use custom window gadgets (TXD Workshop style)")
        self.custom_gadgets_check.setChecked(
            self.app_settings.current_settings.get("use_custom_gadgets", False)
        )
        window_layout.addWidget(self.custom_gadgets_check)

        self.corner_resize_check = QCheckBox("Enable corner resize (all 4 corners)")
        self.corner_resize_check.setChecked(
            self.app_settings.current_settings.get("enable_corner_resize", True)
        )
        window_layout.addWidget(self.corner_resize_check)

        gadget_hint = QLabel("Custom gadgets disable system title bar. Corner resize works in both modes.")
        gadget_hint.setStyleSheet("color: #888; font-style: italic;")
        gadget_hint.setWordWrap(True)
        window_layout.addWidget(gadget_hint)

        layout.addWidget(window_group)

        # Interface Options
        interface_group = QGroupBox("Interface Options")
        interface_layout = QVBoxLayout(interface_group)

        self.tooltips_check = QCheckBox("Show tooltips")
        interface_layout.addWidget(self.tooltips_check)

        self.menu_icons_check = QCheckBox("Show menu icons")
        interface_layout.addWidget(self.menu_icons_check)

        self.use_svg_icons_check = QCheckBox("Use SVG icons (never emojis)")
        self.use_svg_icons_check.setChecked(
            self.app_settings.current_settings.get("use_svg_icons", True)
        )
        interface_layout.addWidget(self.use_svg_icons_check)

        layout.addWidget(interface_group)
        layout.addStretch()

        return widget

    def _create_background_tab(self): #vers 1
        """Create background settings tab - Image backgrounds for panels, buttons, etc."""
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Instructions
        info_label = QLabel(
            "<b>Background Images:</b><br>"
            "Set background images for various UI elements with positioning options."
        )
        info_label.setWordWrap(True)
        info_label.setStyleSheet("padding: 8px; border-radius: 4px;")
        layout.addWidget(info_label)

        # Scroll area for controls
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(15)

        # Panel Background
        panel_group = QGroupBox("Panel Background")
        panel_layout = QVBoxLayout(panel_group)

        # Panel Background Image
        panel_bg_layout = QHBoxLayout()
        panel_bg_layout.addWidget(QLabel("Image Path:"))
        self.panel_bg_path = QLineEdit()
        self.panel_bg_path.setPlaceholderText("Path to background image")
        panel_bg_layout.addWidget(self.panel_bg_path)

        browse_panel_btn = QPushButton("Browse")
        browse_panel_btn.clicked.connect(lambda: self._browse_background_image("panel"))
        panel_bg_layout.addWidget(browse_panel_btn)

        clear_panel_btn = QPushButton("Clear")
        clear_panel_btn.clicked.connect(lambda: self._clear_background_image("panel"))
        panel_bg_layout.addWidget(clear_panel_btn)

        panel_layout.addLayout(panel_bg_layout)

        # Panel Background Mode
        panel_mode_layout = QHBoxLayout()
        panel_mode_layout.addWidget(QLabel("Display Mode:"))
        self.panel_bg_mode = QComboBox()
        self.panel_bg_mode.addItems(["Tiled", "Stretched", "Shaded"])
        panel_mode_layout.addWidget(self.panel_bg_mode)
        panel_mode_layout.addStretch()
        panel_layout.addLayout(panel_mode_layout)

        scroll_layout.addWidget(panel_group)

        # Primary Background
        primary_group = QGroupBox("Primary Background")
        primary_layout = QVBoxLayout(primary_group)

        # Primary Background Image
        primary_bg_layout = QHBoxLayout()
        primary_bg_layout.addWidget(QLabel("Image Path:"))
        self.primary_bg_path = QLineEdit()
        self.primary_bg_path.setPlaceholderText("Path to primary background image")
        primary_bg_layout.addWidget(self.primary_bg_path)

        browse_primary_btn = QPushButton("Browse")
        browse_primary_btn.clicked.connect(lambda: self._browse_background_image("primary"))
        primary_bg_layout.addWidget(browse_primary_btn)

        clear_primary_btn = QPushButton("Clear")
        clear_primary_btn.clicked.connect(lambda: self._clear_background_image("primary"))
        primary_bg_layout.addWidget(clear_primary_btn)

        primary_layout.addLayout(primary_bg_layout)

        # Primary Background Mode
        primary_mode_layout = QHBoxLayout()
        primary_mode_layout.addWidget(QLabel("Display Mode:"))
        self.primary_bg_mode = QComboBox()
        self.primary_bg_mode.addItems(["Tiled", "Stretched", "Shaded"])
        primary_mode_layout.addWidget(self.primary_bg_mode)
        primary_mode_layout.addStretch()
        primary_layout.addLayout(primary_mode_layout)

        scroll_layout.addWidget(primary_group)

        # Secondary Background
        secondary_group = QGroupBox("Secondary Background")
        secondary_layout = QVBoxLayout(secondary_group)

        # Secondary Background Image
        secondary_bg_layout = QHBoxLayout()
        secondary_bg_layout.addWidget(QLabel("Image Path:"))
        self.secondary_bg_path = QLineEdit()
        self.secondary_bg_path.setPlaceholderText("Path to secondary background image")
        secondary_bg_layout.addWidget(self.secondary_bg_path)

        browse_secondary_btn = QPushButton("Browse")
        browse_secondary_btn.clicked.connect(lambda: self._browse_background_image("secondary"))
        secondary_bg_layout.addWidget(browse_secondary_btn)

        clear_secondary_btn = QPushButton("Clear")
        clear_secondary_btn.clicked.connect(lambda: self._clear_background_image("secondary"))
        secondary_bg_layout.addWidget(clear_secondary_btn)

        secondary_layout.addLayout(secondary_bg_layout)

        # Secondary Background Mode
        secondary_mode_layout = QHBoxLayout()
        secondary_mode_layout.addWidget(QLabel("Display Mode:"))
        self.secondary_bg_mode = QComboBox()
        self.secondary_bg_mode.addItems(["Tiled", "Stretched", "Shaded"])
        secondary_mode_layout.addWidget(self.secondary_bg_mode)
        secondary_mode_layout.addStretch()
        secondary_layout.addLayout(secondary_mode_layout)

        scroll_layout.addWidget(secondary_group)

        # Button Background
        button_group = QGroupBox("Button Background")
        button_layout = QVBoxLayout(button_group)

        # Button Background Image
        button_bg_layout = QHBoxLayout()
        button_bg_layout.addWidget(QLabel("Image Path:"))
        self.button_bg_path = QLineEdit()
        self.button_bg_path.setPlaceholderText("Path to button background image")
        button_bg_layout.addWidget(self.button_bg_path)

        browse_button_btn = QPushButton("Browse")
        browse_button_btn.clicked.connect(lambda: self._browse_background_image("button"))
        button_bg_layout.addWidget(browse_button_btn)

        clear_button_btn = QPushButton("Clear")
        clear_button_btn.clicked.connect(lambda: self._clear_background_image("button"))
        button_bg_layout.addWidget(clear_button_btn)

        button_layout.addLayout(button_bg_layout)

        # Button Background Mode
        button_mode_layout = QHBoxLayout()
        button_mode_layout.addWidget(QLabel("Display Mode:"))
        self.button_bg_mode = QComboBox()
        self.button_bg_mode.addItems(["Tiled", "Stretched", "Shaded"])
        button_mode_layout.addWidget(self.button_bg_mode)
        button_mode_layout.addStretch()
        button_layout.addLayout(button_mode_layout)

        scroll_layout.addWidget(button_group)

        scroll_layout.addStretch()
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        return tab

    def _create_transparency_tab(self): #vers 1
        """Create transparency settings tab - Opacity controls for UI elements."""
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Instructions
        info_label = QLabel(
            "<b>Transparency Settings:</b><br>"
            "Configure opacity levels for various UI elements (0.0 = fully transparent, 1.0 = fully opaque)."
        )
        info_label.setWordWrap(True)
        info_label.setStyleSheet("padding: 8px; border-radius: 4px;")
        layout.addWidget(info_label)

        # Scroll area for controls
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(15)

        # Titlebar Transparency
        titlebar_group = QGroupBox("Titlebar Transparency")
        titlebar_layout = QVBoxLayout(titlebar_group)

        # Titlebar Opacity
        titlebar_opacity_layout = QHBoxLayout()
        titlebar_opacity_layout.addWidget(QLabel("Opacity:"))
        self.titlebar_opacity_slider = QSlider(Qt.Orientation.Horizontal)
        self.titlebar_opacity_slider.setRange(0, 100)
        self.titlebar_opacity_slider.setValue(100)  # Fully opaque by default
        self.titlebar_opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.titlebar_opacity_slider.setTickInterval(10)
        titlebar_opacity_layout.addWidget(self.titlebar_opacity_slider)
        self.titlebar_opacity_label = QLabel("1.0")
        self.titlebar_opacity_label.setFixedWidth(40)
        titlebar_opacity_layout.addWidget(self.titlebar_opacity_label)
        titlebar_layout.addLayout(titlebar_opacity_layout)

        self.titlebar_opacity_slider.valueChanged.connect(
            lambda v: self.titlebar_opacity_label.setText(f"{v/100:.1f}")
        )

        scroll_layout.addWidget(titlebar_group)

        # Panel Transparency
        panel_group = QGroupBox("Panel Transparency")
        panel_layout = QVBoxLayout(panel_group)

        # Panel Opacity
        panel_opacity_layout = QHBoxLayout()
        panel_opacity_layout.addWidget(QLabel("Opacity:"))
        self.panel_opacity_slider = QSlider(Qt.Orientation.Horizontal)
        self.panel_opacity_slider.setRange(0, 100)
        self.panel_opacity_slider.setValue(100)  # Fully opaque by default
        self.panel_opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.panel_opacity_slider.setTickInterval(10)
        panel_opacity_layout.addWidget(self.panel_opacity_slider)
        self.panel_opacity_label = QLabel("1.0")
        self.panel_opacity_label.setFixedWidth(40)
        panel_opacity_layout.addWidget(self.panel_opacity_label)
        panel_layout.addLayout(panel_opacity_layout)

        self.panel_opacity_slider.valueChanged.connect(
            lambda v: self.panel_opacity_label.setText(f"{v/100:.1f}")
        )

        scroll_layout.addWidget(panel_group)

        # Button Transparency
        button_group = QGroupBox("Button Transparency")
        button_layout = QVBoxLayout(button_group)

        # Button Opacity
        button_opacity_layout = QHBoxLayout()
        button_opacity_layout.addWidget(QLabel("Opacity:"))
        self.button_opacity_slider = QSlider(Qt.Orientation.Horizontal)
        self.button_opacity_slider.setRange(0, 100)
        self.button_opacity_slider.setValue(100)  # Fully opaque by default
        self.button_opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.button_opacity_slider.setTickInterval(10)
        button_opacity_layout.addWidget(self.button_opacity_slider)
        self.button_opacity_label = QLabel("1.0")
        self.button_opacity_label.setFixedWidth(40)
        button_opacity_layout.addWidget(self.button_opacity_label)
        button_layout.addLayout(button_opacity_layout)

        self.button_opacity_slider.valueChanged.connect(
            lambda v: self.button_opacity_label.setText(f"{v/100:.1f}")
        )

        scroll_layout.addWidget(button_group)

        # Widget Transparency
        widget_group = QGroupBox("Widget Transparency")
        widget_layout = QVBoxLayout(widget_group)

        # Widget Opacity
        widget_opacity_layout = QHBoxLayout()
        widget_opacity_layout.addWidget(QLabel("Opacity:"))
        self.widget_opacity_slider = QSlider(Qt.Orientation.Horizontal)
        self.widget_opacity_slider.setRange(0, 100)
        self.widget_opacity_slider.setValue(100)  # Fully opaque by default
        self.widget_opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.widget_opacity_slider.setTickInterval(10)
        widget_opacity_layout.addWidget(self.widget_opacity_slider)
        self.widget_opacity_label = QLabel("1.0")
        self.widget_opacity_label.setFixedWidth(40)
        widget_opacity_layout.addWidget(self.widget_opacity_label)
        widget_layout.addLayout(widget_opacity_layout)

        self.widget_opacity_slider.valueChanged.connect(
            lambda v: self.widget_opacity_label.setText(f"{v/100:.1f}")
        )

        scroll_layout.addWidget(widget_group)

        scroll_layout.addStretch()
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        return tab

    def _create_shadows_tab(self): #vers 1
        """Create shadow effects settings tab - Depth and shadow controls for UI elements."""
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Instructions
        info_label = QLabel(
            "<b>Shadow Effects:</b><br>"
            "Configure shadow depth and appearance for buttons, panels, and other UI elements."
        )
        info_label.setWordWrap(True)
        info_label.setStyleSheet("padding: 8px; border-radius: 4px;")
        layout.addWidget(info_label)

        # Scroll area for controls
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(15)

        # Button Shadow Settings
        button_shadow_group = QGroupBox("Button Shadows")
        button_shadow_layout = QVBoxLayout(button_shadow_group)

        # Shadow Depth
        shadow_depth_layout = QHBoxLayout()
        shadow_depth_layout.addWidget(QLabel("Shadow Depth:"))
        self.button_shadow_depth = QSlider(Qt.Orientation.Horizontal)
        self.button_shadow_depth.setRange(0, 20)
        self.button_shadow_depth.setValue(3)
        self.button_shadow_depth.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.button_shadow_depth.setTickInterval(5)
        shadow_depth_layout.addWidget(self.button_shadow_depth)
        self.button_shadow_depth_label = QLabel("3px")
        self.button_shadow_depth_label.setFixedWidth(40)
        shadow_depth_layout.addWidget(self.button_shadow_depth_label)
        button_shadow_layout.addLayout(shadow_depth_layout)

        self.button_shadow_depth.valueChanged.connect(
            lambda v: self.button_shadow_depth_label.setText(f"{v}px")
        )

        # Shadow Blur
        shadow_blur_layout = QHBoxLayout()
        shadow_blur_layout.addWidget(QLabel("Shadow Blur:"))
        self.button_shadow_blur = QSlider(Qt.Orientation.Horizontal)
        self.button_shadow_blur.setRange(0, 20)
        self.button_shadow_blur.setValue(5)
        self.button_shadow_blur.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.button_shadow_blur.setTickInterval(5)
        shadow_blur_layout.addWidget(self.button_shadow_blur)
        self.button_shadow_blur_label = QLabel("5px")
        self.button_shadow_blur_label.setFixedWidth(40)
        shadow_blur_layout.addWidget(self.button_shadow_blur_label)
        button_shadow_layout.addLayout(shadow_blur_layout)

        self.button_shadow_blur.valueChanged.connect(
            lambda v: self.button_shadow_blur_label.setText(f"{v}px")
        )

        # Shadow Opacity
        shadow_opacity_layout = QHBoxLayout()
        shadow_opacity_layout.addWidget(QLabel("Shadow Opacity:"))
        self.button_shadow_opacity = QSlider(Qt.Orientation.Horizontal)
        self.button_shadow_opacity.setRange(0, 100)
        self.button_shadow_opacity.setValue(30)
        self.button_shadow_opacity.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.button_shadow_opacity.setTickInterval(25)
        shadow_opacity_layout.addWidget(self.button_shadow_opacity)
        self.button_shadow_opacity_label = QLabel("30%")
        self.button_shadow_opacity_label.setFixedWidth(40)
        shadow_opacity_layout.addWidget(self.button_shadow_opacity_label)
        button_shadow_layout.addLayout(shadow_opacity_layout)

        self.button_shadow_opacity.valueChanged.connect(
            lambda v: self.button_shadow_opacity_label.setText(f"{v}%")
        )

        scroll_layout.addWidget(button_shadow_group)

        # Panel Shadow Settings
        panel_shadow_group = QGroupBox("Panel Shadows")
        panel_shadow_layout = QVBoxLayout(panel_shadow_group)

        # Shadow Depth
        panel_shadow_depth_layout = QHBoxLayout()
        panel_shadow_depth_layout.addWidget(QLabel("Shadow Depth:"))
        self.panel_shadow_depth = QSlider(Qt.Orientation.Horizontal)
        self.panel_shadow_depth.setRange(0, 20)
        self.panel_shadow_depth.setValue(5)
        self.panel_shadow_depth.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.panel_shadow_depth.setTickInterval(5)
        panel_shadow_depth_layout.addWidget(self.panel_shadow_depth)
        self.panel_shadow_depth_label = QLabel("5px")
        self.panel_shadow_depth_label.setFixedWidth(40)
        panel_shadow_depth_layout.addWidget(self.panel_shadow_depth_label)
        panel_shadow_layout.addLayout(panel_shadow_depth_layout)

        self.panel_shadow_depth.valueChanged.connect(
            lambda v: self.panel_shadow_depth_label.setText(f"{v}px")
        )

        # Shadow Blur
        panel_shadow_blur_layout = QHBoxLayout()
        panel_shadow_blur_layout.addWidget(QLabel("Shadow Blur:"))
        self.panel_shadow_blur = QSlider(Qt.Orientation.Horizontal)
        self.panel_shadow_blur.setRange(0, 20)
        self.panel_shadow_blur.setValue(8)
        self.panel_shadow_blur.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.panel_shadow_blur.setTickInterval(5)
        panel_shadow_blur_layout.addWidget(self.panel_shadow_blur)
        self.panel_shadow_blur_label = QLabel("8px")
        self.panel_shadow_blur_label.setFixedWidth(40)
        panel_shadow_blur_layout.addWidget(self.panel_shadow_blur_label)
        panel_shadow_layout.addLayout(panel_shadow_blur_layout)

        self.panel_shadow_blur.valueChanged.connect(
            lambda v: self.panel_shadow_blur_label.setText(f"{v}px")
        )

        # Shadow Opacity
        panel_shadow_opacity_layout = QHBoxLayout()
        panel_shadow_opacity_layout.addWidget(QLabel("Shadow Opacity:"))
        self.panel_shadow_opacity = QSlider(Qt.Orientation.Horizontal)
        self.panel_shadow_opacity.setRange(0, 100)
        self.panel_shadow_opacity.setValue(25)
        self.panel_shadow_opacity.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.panel_shadow_opacity.setTickInterval(25)
        panel_shadow_opacity_layout.addWidget(self.panel_shadow_opacity)
        self.panel_shadow_opacity_label = QLabel("25%")
        self.panel_shadow_opacity_label.setFixedWidth(40)
        panel_shadow_opacity_layout.addWidget(self.panel_shadow_opacity_label)
        panel_shadow_layout.addLayout(panel_shadow_opacity_layout)

        self.panel_shadow_opacity.valueChanged.connect(
            lambda v: self.panel_shadow_opacity_label.setText(f"{v}%")
        )

        scroll_layout.addWidget(panel_shadow_group)

        # General Shadow Settings
        general_shadow_group = QGroupBox("General Shadow Settings")
        general_shadow_layout = QVBoxLayout(general_shadow_group)

        # Enable Shadows
        self.enable_shadows_check = QCheckBox("Enable shadows for all UI elements")
        self.enable_shadows_check.setChecked(True)
        general_shadow_layout.addWidget(self.enable_shadows_check)

        # Shadow Color
        shadow_color_layout = QHBoxLayout()
        shadow_color_layout.addWidget(QLabel("Shadow Color:"))
        self.shadow_color_button = QPushButton("Select Color")
        self.shadow_color_button.clicked.connect(self._select_shadow_color)
        shadow_color_layout.addWidget(self.shadow_color_button)
        shadow_color_layout.addStretch()
        general_shadow_layout.addLayout(shadow_color_layout)

        scroll_layout.addWidget(general_shadow_group)

        scroll_layout.addStretch()
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        return tab

    def _create_advanced_gadgets_tab(self): #vers 1
        """Create advanced gadgets support tab - String, Gauge, Scale, Colorfield, List, Numeric, Knob, Levelmeter, Slider, Radio, Cycle, Palette, Popstring"""
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Instructions
        info_label = QLabel(
            "<b>Advanced Gadgets:</b><br>"
            "Configure Amiga MUI style gadgets with proper styling."
        )
        info_label.setWordWrap(True)
        info_label.setStyleSheet("padding: 8px; border-radius: 4px;")
        layout.addWidget(info_label)

        # Scroll area for controls
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(15)

        # String Gadgets
        string_group = QGroupBox("String Gadgets")
        string_layout = QVBoxLayout(string_group)

        # String Background Color
        string_bg_layout = QHBoxLayout()
        string_bg_layout.addWidget(QLabel("Background Color:"))
        self.string_bg_color = QPushButton("Select Color")
        self.string_bg_color.clicked.connect(lambda: self._select_color_for_gadget("string_bg"))
        string_bg_layout.addWidget(self.string_bg_color)
        string_bg_layout.addStretch()
        string_layout.addLayout(string_bg_layout)

        # String Text Color
        string_text_layout = QHBoxLayout()
        string_text_layout.addWidget(QLabel("Text Color:"))
        self.string_text_color = QPushButton("Select Color")
        self.string_text_color.clicked.connect(lambda: self._select_color_for_gadget("string_text"))
        string_text_layout.addWidget(self.string_text_color)
        string_text_layout.addStretch()
        string_layout.addLayout(string_text_layout)

        # String Border Color
        string_border_layout = QHBoxLayout()
        string_border_layout.addWidget(QLabel("Border Color:"))
        self.string_border_color = QPushButton("Select Color")
        self.string_border_color.clicked.connect(lambda: self._select_color_for_gadget("string_border"))
        string_border_layout.addWidget(self.string_border_color)
        string_border_layout.addStretch()
        string_layout.addLayout(string_border_layout)

        scroll_layout.addWidget(string_group)

        # Gauge Gadgets
        gauge_group = QGroupBox("Gauge Gadgets")
        gauge_layout = QVBoxLayout(gauge_group)

        # Gauge Background Color
        gauge_bg_layout = QHBoxLayout()
        gauge_bg_layout.addWidget(QLabel("Background Color:"))
        self.gauge_bg_color = QPushButton("Select Color")
        self.gauge_bg_color.clicked.connect(lambda: self._select_color_for_gadget("gauge_bg"))
        gauge_bg_layout.addWidget(self.gauge_bg_color)
        gauge_bg_layout.addStretch()
        gauge_layout.addLayout(gauge_bg_layout)

        # Gauge Fill Color
        gauge_fill_layout = QHBoxLayout()
        gauge_fill_layout.addWidget(QLabel("Fill Color:"))
        self.gauge_fill_color = QPushButton("Select Color")
        self.gauge_fill_color.clicked.connect(lambda: self._select_color_for_gadget("gauge_fill"))
        gauge_fill_layout.addWidget(self.gauge_fill_color)
        gauge_fill_layout.addStretch()
        gauge_layout.addLayout(gauge_fill_layout)

        # Gauge Border Color
        gauge_border_layout = QHBoxLayout()
        gauge_border_layout.addWidget(QLabel("Border Color:"))
        self.gauge_border_color = QPushButton("Select Color")
        self.gauge_border_color.clicked.connect(lambda: self._select_color_for_gadget("gauge_border"))
        gauge_border_layout.addWidget(self.gauge_border_color)
        gauge_border_layout.addStretch()
        gauge_layout.addLayout(gauge_border_layout)

        scroll_layout.addWidget(gauge_group)

        # Scale Gadgets
        scale_group = QGroupBox("Scale Gadgets")
        scale_layout = QVBoxLayout(scale_group)

        # Scale Background Color
        scale_bg_layout = QHBoxLayout()
        scale_bg_layout.addWidget(QLabel("Background Color:"))
        self.scale_bg_color = QPushButton("Select Color")
        self.scale_bg_color.clicked.connect(lambda: self._select_color_for_gadget("scale_bg"))
        scale_bg_layout.addWidget(self.scale_bg_color)
        scale_bg_layout.addStretch()
        scale_layout.addLayout(scale_bg_layout)

        # Scale Handle Color
        scale_handle_layout = QHBoxLayout()
        scale_handle_layout.addWidget(QLabel("Handle Color:"))
        self.scale_handle_color = QPushButton("Select Color")
        self.scale_handle_color.clicked.connect(lambda: self._select_color_for_gadget("scale_handle"))
        scale_handle_layout.addWidget(self.scale_handle_color)
        scale_handle_layout.addStretch()
        scale_layout.addLayout(scale_handle_layout)

        scroll_layout.addWidget(scale_group)

        # Colorfield Gadgets
        colorfield_group = QGroupBox("Colorfield Gadgets")
        colorfield_layout = QVBoxLayout(colorfield_group)

        # Colorfield Background Color
        colorfield_bg_layout = QHBoxLayout()
        colorfield_bg_layout.addWidget(QLabel("Background Color:"))
        self.colorfield_bg_color = QPushButton("Select Color")
        self.colorfield_bg_color.clicked.connect(lambda: self._select_color_for_gadget("colorfield_bg"))
        colorfield_bg_layout.addWidget(self.colorfield_bg_color)
        colorfield_bg_layout.addStretch()
        colorfield_layout.addLayout(colorfield_bg_layout)

        # Colorfield Border Color
        colorfield_border_layout = QHBoxLayout()
        colorfield_border_layout.addWidget(QLabel("Border Color:"))
        self.colorfield_border_color = QPushButton("Select Color")
        self.colorfield_border_color.clicked.connect(lambda: self._select_color_for_gadget("colorfield_border"))
        colorfield_border_layout.addWidget(self.colorfield_border_color)
        colorfield_border_layout.addStretch()
        colorfield_layout.addLayout(colorfield_border_layout)

        scroll_layout.addWidget(colorfield_group)

        # List Gadgets
        list_group = QGroupBox("List Gadgets")
        list_layout = QVBoxLayout(list_group)

        # List Background Color
        list_bg_layout = QHBoxLayout()
        list_bg_layout.addWidget(QLabel("Background Color:"))
        self.list_bg_color = QPushButton("Select Color")
        self.list_bg_color.clicked.connect(lambda: self._select_color_for_gadget("list_bg"))
        list_bg_layout.addWidget(self.list_bg_color)
        list_bg_layout.addStretch()
        list_layout.addLayout(list_bg_layout)

        # List Text Color
        list_text_layout = QHBoxLayout()
        list_text_layout.addWidget(QLabel("Text Color:"))
        self.list_text_color = QPushButton("Select Color")
        self.list_text_color.clicked.connect(lambda: self._select_color_for_gadget("list_text"))
        list_text_layout.addWidget(self.list_text_color)
        list_text_layout.addStretch()
        list_layout.addLayout(list_text_layout)

        # List Selection Color
        list_select_layout = QHBoxLayout()
        list_select_layout.addWidget(QLabel("Selection Color:"))
        self.list_select_color = QPushButton("Select Color")
        self.list_select_color.clicked.connect(lambda: self._select_color_for_gadget("list_select"))
        list_select_layout.addWidget(self.list_select_color)
        list_select_layout.addStretch()
        list_layout.addLayout(list_select_layout)

        scroll_layout.addWidget(list_group)

        # Numeric Gadgets
        numeric_group = QGroupBox("Numeric Gadgets")
        numeric_layout = QVBoxLayout(numeric_group)

        # Numeric Background Color
        numeric_bg_layout = QHBoxLayout()
        numeric_bg_layout.addWidget(QLabel("Background Color:"))
        self.numeric_bg_color = QPushButton("Select Color")
        self.numeric_bg_color.clicked.connect(lambda: self._select_color_for_gadget("numeric_bg"))
        numeric_bg_layout.addWidget(self.numeric_bg_color)
        numeric_bg_layout.addStretch()
        numeric_layout.addLayout(numeric_bg_layout)

        # Numeric Text Color
        numeric_text_layout = QHBoxLayout()
        numeric_text_layout.addWidget(QLabel("Text Color:"))
        self.numeric_text_color = QPushButton("Select Color")
        self.numeric_text_color.clicked.connect(lambda: self._select_color_for_gadget("numeric_text"))
        numeric_text_layout.addWidget(self.numeric_text_color)
        numeric_text_layout.addStretch()
        numeric_layout.addLayout(numeric_text_layout)

        scroll_layout.addWidget(numeric_group)

        # Knob Gadgets
        knob_group = QGroupBox("Knob Gadgets")
        knob_layout = QVBoxLayout(knob_group)

        # Knob Background Color
        knob_bg_layout = QHBoxLayout()
        knob_bg_layout.addWidget(QLabel("Background Color:"))
        self.knob_bg_color = QPushButton("Select Color")
        self.knob_bg_color.clicked.connect(lambda: self._select_color_for_gadget("knob_bg"))
        knob_bg_layout.addWidget(self.knob_bg_color)
        knob_bg_layout.addStretch()
        knob_layout.addLayout(knob_bg_layout)

        # Knob Handle Color
        knob_handle_layout = QHBoxLayout()
        knob_handle_layout.addWidget(QLabel("Handle Color:"))
        self.knob_handle_color = QPushButton("Select Color")
        self.knob_handle_color.clicked.connect(lambda: self._select_color_for_gadget("knob_handle"))
        knob_handle_layout.addWidget(self.knob_handle_color)
        knob_handle_layout.addStretch()
        knob_layout.addLayout(knob_handle_layout)

        scroll_layout.addWidget(knob_group)

        # Levelmeter Gadgets
        levelmeter_group = QGroupBox("Levelmeter Gadgets")
        levelmeter_layout = QVBoxLayout(levelmeter_group)

        # Levelmeter Background Color
        levelmeter_bg_layout = QHBoxLayout()
        levelmeter_bg_layout.addWidget(QLabel("Background Color:"))
        self.levelmeter_bg_color = QPushButton("Select Color")
        self.levelmeter_bg_color.clicked.connect(lambda: self._select_color_for_gadget("levelmeter_bg"))
        levelmeter_bg_layout.addWidget(self.levelmeter_bg_color)
        levelmeter_bg_layout.addStretch()
        levelmeter_layout.addLayout(levelmeter_bg_layout)

        # Levelmeter Fill Color
        levelmeter_fill_layout = QHBoxLayout()
        levelmeter_fill_layout.addWidget(QLabel("Fill Color:"))
        self.levelmeter_fill_color = QPushButton("Select Color")
        self.levelmeter_fill_color.clicked.connect(lambda: self._select_color_for_gadget("levelmeter_fill"))
        levelmeter_fill_layout.addWidget(self.levelmeter_fill_color)
        levelmeter_fill_layout.addStretch()
        levelmeter_layout.addLayout(levelmeter_fill_layout)

        scroll_layout.addWidget(levelmeter_group)

        # Radio Gadgets
        radio_group = QGroupBox("Radio Gadgets")
        radio_layout = QVBoxLayout(radio_group)

        # Radio Background Color
        radio_bg_layout = QHBoxLayout()
        radio_bg_layout.addWidget(QLabel("Background Color:"))
        self.radio_bg_color = QPushButton("Select Color")
        self.radio_bg_color.clicked.connect(lambda: self._select_color_for_gadget("radio_bg"))
        radio_bg_layout.addWidget(self.radio_bg_color)
        radio_bg_layout.addStretch()
        radio_layout.addLayout(radio_bg_layout)

        # Radio Text Color
        radio_text_layout = QHBoxLayout()
        radio_text_layout.addWidget(QLabel("Text Color:"))
        self.radio_text_color = QPushButton("Select Color")
        self.radio_text_color.clicked.connect(lambda: self._select_color_for_gadget("radio_text"))
        radio_text_layout.addWidget(self.radio_text_color)
        radio_text_layout.addStretch()
        radio_layout.addLayout(radio_text_layout)

        scroll_layout.addWidget(radio_group)

        # Cycle Gadgets
        cycle_group = QGroupBox("Cycle Gadgets")
        cycle_layout = QVBoxLayout(cycle_group)

        # Cycle Background Color
        cycle_bg_layout = QHBoxLayout()
        cycle_bg_layout.addWidget(QLabel("Background Color:"))
        self.cycle_bg_color = QPushButton("Select Color")
        self.cycle_bg_color.clicked.connect(lambda: self._select_color_for_gadget("cycle_bg"))
        cycle_bg_layout.addWidget(self.cycle_bg_color)
        cycle_bg_layout.addStretch()
        cycle_layout.addLayout(cycle_bg_layout)

        # Cycle Text Color
        cycle_text_layout = QHBoxLayout()
        cycle_text_layout.addWidget(QLabel("Text Color:"))
        self.cycle_text_color = QPushButton("Select Color")
        self.cycle_text_color.clicked.connect(lambda: self._select_color_for_gadget("cycle_text"))
        cycle_text_layout.addWidget(self.cycle_text_color)
        cycle_text_layout.addStretch()
        cycle_layout.addLayout(cycle_text_layout)

        scroll_layout.addWidget(cycle_group)

        # Palette Gadgets
        palette_group = QGroupBox("Palette Gadgets")
        palette_layout = QVBoxLayout(palette_group)

        # Palette Background Color
        palette_bg_layout = QHBoxLayout()
        palette_bg_layout.addWidget(QLabel("Background Color:"))
        self.palette_bg_color = QPushButton("Select Color")
        self.palette_bg_color.clicked.connect(lambda: self._select_color_for_gadget("palette_bg"))
        palette_bg_layout.addWidget(self.palette_bg_color)
        palette_bg_layout.addStretch()
        palette_layout.addLayout(palette_bg_layout)

        # Palette Border Color
        palette_border_layout = QHBoxLayout()
        palette_border_layout.addWidget(QLabel("Border Color:"))
        self.palette_border_color = QPushButton("Select Color")
        self.palette_border_color.clicked.connect(lambda: self._select_color_for_gadget("palette_border"))
        palette_border_layout.addWidget(self.palette_border_color)
        palette_border_layout.addStretch()
        palette_layout.addLayout(palette_border_layout)

        scroll_layout.addWidget(palette_group)

        # Popstring Gadgets
        popstring_group = QGroupBox("Popstring Gadgets")
        popstring_layout = QVBoxLayout(popstring_group)

        # Popstring Background Color
        popstring_bg_layout = QHBoxLayout()
        popstring_bg_layout.addWidget(QLabel("Background Color:"))
        self.popstring_bg_color = QPushButton("Select Color")
        self.popstring_bg_color.clicked.connect(lambda: self._select_color_for_gadget("popstring_bg"))
        popstring_bg_layout.addWidget(self.popstring_bg_color)
        popstring_bg_layout.addStretch()
        popstring_layout.addLayout(popstring_bg_layout)

        # Popstring Text Color
        popstring_text_layout = QHBoxLayout()
        popstring_text_layout.addWidget(QLabel("Text Color:"))
        self.popstring_text_color = QPushButton("Select Color")
        self.popstring_text_color.clicked.connect(lambda: self._select_color_for_gadget("popstring_text"))
        popstring_text_layout.addWidget(self.popstring_text_color)
        popstring_text_layout.addStretch()
        popstring_layout.addLayout(popstring_text_layout)

        scroll_layout.addWidget(popstring_group)

        scroll_layout.addStretch()
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        return tab

    def _select_color_for_gadget(self, gadget_type): #vers 1
        """Open color dialog to select color for specific gadget type"""
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        
        current_color = QColor(255, 255, 255)  # Default white
        color = QColorDialog.getColor(current_color, self, f"Select {gadget_type.replace('_', ' ').title()} Color")
        
        if color.isValid():
            color_name = color.name()
            # Update the button text to show selected color
            button_map = {
                "string_bg": self.string_bg_color,
                "string_text": self.string_text_color,
                "string_border": self.string_border_color,
                "gauge_bg": self.gauge_bg_color,
                "gauge_fill": self.gauge_fill_color,
                "gauge_border": self.gauge_border_color,
                "scale_bg": self.scale_bg_color,
                "scale_handle": self.scale_handle_color,
                "colorfield_bg": self.colorfield_bg_color,
                "colorfield_border": self.colorfield_border_color,
                "list_bg": self.list_bg_color,
                "list_text": self.list_text_color,
                "list_select": self.list_select_color,
                "numeric_bg": self.numeric_bg_color,
                "numeric_text": self.numeric_text_color,
                "knob_bg": self.knob_bg_color,
                "knob_handle": self.knob_handle_color,
                "levelmeter_bg": self.levelmeter_bg_color,
                "levelmeter_fill": self.levelmeter_fill_color,
                "radio_bg": self.radio_bg_color,
                "radio_text": self.radio_text_color,
                "cycle_bg": self.cycle_bg_color,
                "cycle_text": self.cycle_text_color,
                "palette_bg": self.palette_bg_color,
                "palette_border": self.palette_border_color,
                "popstring_bg": self.popstring_bg_color,
                "popstring_text": self.popstring_text_color,
            }
            
            if gadget_type in button_map:
                button_map[gadget_type].setText(f"Selected: {color_name}")
            
            # Store the color for later use
            setattr(self, f"{gadget_type}_color", color_name)
            print(f"{gadget_type} color selected: {color_name}")

    def _select_shadow_color(self): #vers 1
        """Open color dialog to select shadow color"""
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        
        current_color = QColor(0, 0, 0, 128)  # Default: black with 50% opacity
        color = QColorDialog.getColor(current_color, self, "Select Shadow Color")
        
        if color.isValid():
            # For now, just store the color for later use
            self.shadow_color = color.name()
            self.shadow_color_button.setText(f"Selected: {color.name()}")
            print(f"Shadow color selected: {color.name()}")

    def _create_ui_management_tab(self): #vers 1
        """Create UI Management Components tab - Group, Scrollbar, Listview, Register, Virtgroup, Scrollgroup, Popobject"""
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Instructions
        info_label = QLabel(
            "<b>UI Management Components:</b><br>"
            "Configure container and management components for UI layout."
        )
        info_label.setWordWrap(True)
        info_label.setStyleSheet("padding: 8px; border-radius: 4px;")
        layout.addWidget(info_label)

        # Scroll area for controls
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(15)

        # Group Components
        group_group = QGroupBox("Group Components")
        group_layout = QVBoxLayout(group_group)

        # Group Background Color
        group_bg_layout = QHBoxLayout()
        group_bg_layout.addWidget(QLabel("Background Color:"))
        self.group_bg_color = QPushButton("Select Color")
        self.group_bg_color.clicked.connect(lambda: self._select_color_for_component("group_bg"))
        group_bg_layout.addWidget(self.group_bg_color)
        group_bg_layout.addStretch()
        group_layout.addLayout(group_bg_layout)

        # Group Border Color
        group_border_layout = QHBoxLayout()
        group_border_layout.addWidget(QLabel("Border Color:"))
        self.group_border_color = QPushButton("Select Color")
        self.group_border_color.clicked.connect(lambda: self._select_color_for_component("group_border"))
        group_border_layout.addWidget(self.group_border_color)
        group_border_layout.addStretch()
        group_layout.addLayout(group_border_layout)

        scroll_layout.addWidget(group_group)

        # Scrollbar Components
        scrollbar_group = QGroupBox("Scrollbar Components")
        scrollbar_layout = QVBoxLayout(scrollbar_group)

        # Scrollbar Background Color
        scrollbar_bg_layout = QHBoxLayout()
        scrollbar_bg_layout.addWidget(QLabel("Background Color:"))
        self.scrollbar_bg_color = QPushButton("Select Color")
        self.scrollbar_bg_color.clicked.connect(lambda: self._select_color_for_component("scrollbar_bg"))
        scrollbar_bg_layout.addWidget(self.scrollbar_bg_color)
        scrollbar_bg_layout.addStretch()
        scrollbar_layout.addLayout(scrollbar_bg_layout)

        # Scrollbar Handle Color
        scrollbar_handle_layout = QHBoxLayout()
        scrollbar_handle_layout.addWidget(QLabel("Handle Color:"))
        self.scrollbar_handle_color = QPushButton("Select Color")
        self.scrollbar_handle_color.clicked.connect(lambda: self._select_color_for_component("scrollbar_handle"))
        scrollbar_handle_layout.addWidget(self.scrollbar_handle_color)
        scrollbar_handle_layout.addStretch()
        scrollbar_layout.addLayout(scrollbar_handle_layout)

        # Scrollbar Width
        scrollbar_width_layout = QHBoxLayout()
        scrollbar_width_layout.addWidget(QLabel("Width:"))
        self.scrollbar_width_slider = QSlider(Qt.Orientation.Horizontal)
        self.scrollbar_width_slider.setRange(8, 20)
        self.scrollbar_width_slider.setValue(12)
        self.scrollbar_width_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.scrollbar_width_slider.setTickInterval(2)
        scrollbar_width_layout.addWidget(self.scrollbar_width_slider)
        self.scrollbar_width_label = QLabel("12px")
        self.scrollbar_width_label.setFixedWidth(40)
        scrollbar_width_layout.addWidget(self.scrollbar_width_label)
        scrollbar_layout.addLayout(scrollbar_width_layout)

        self.scrollbar_width_slider.valueChanged.connect(
            lambda v: self.scrollbar_width_label.setText(f"{v}px")
        )

        scroll_layout.addWidget(scrollbar_group)

        # Listview Components
        listview_group = QGroupBox("Listview Components")
        listview_layout = QVBoxLayout(listview_group)

        # Listview Background Color
        listview_bg_layout = QHBoxLayout()
        listview_bg_layout.addWidget(QLabel("Background Color:"))
        self.listview_bg_color = QPushButton("Select Color")
        self.listview_bg_color.clicked.connect(lambda: self._select_color_for_component("listview_bg"))
        listview_bg_layout.addWidget(self.listview_bg_color)
        listview_bg_layout.addStretch()
        listview_layout.addLayout(listview_bg_layout)

        # Listview Text Color
        listview_text_layout = QHBoxLayout()
        listview_text_layout.addWidget(QLabel("Text Color:"))
        self.listview_text_color = QPushButton("Select Color")
        self.listview_text_color.clicked.connect(lambda: self._select_color_for_component("listview_text"))
        listview_text_layout.addWidget(self.listview_text_color)
        listview_text_layout.addStretch()
        listview_layout.addLayout(listview_text_layout)

        # Listview Selection Color
        listview_select_layout = QHBoxLayout()
        listview_select_layout.addWidget(QLabel("Selection Color:"))
        self.listview_select_color = QPushButton("Select Color")
        self.listview_select_color.clicked.connect(lambda: self._select_color_for_component("listview_select"))
        listview_select_layout.addWidget(self.listview_select_color)
        listview_select_layout.addStretch()
        listview_layout.addLayout(listview_select_layout)

        scroll_layout.addWidget(listview_group)

        # Register Components
        register_group = QGroupBox("Register Components")
        register_layout = QVBoxLayout(register_group)

        # Register Background Color
        register_bg_layout = QHBoxLayout()
        register_bg_layout.addWidget(QLabel("Background Color:"))
        self.register_bg_color = QPushButton("Select Color")
        self.register_bg_color.clicked.connect(lambda: self._select_color_for_component("register_bg"))
        register_bg_layout.addWidget(self.register_bg_color)
        register_bg_layout.addStretch()
        register_layout.addLayout(register_bg_layout)

        # Register Text Color
        register_text_layout = QHBoxLayout()
        register_text_layout.addWidget(QLabel("Text Color:"))
        self.register_text_color = QPushButton("Select Color")
        self.register_text_color.clicked.connect(lambda: self._select_color_for_component("register_text"))
        register_text_layout.addWidget(self.register_text_color)
        register_text_layout.addStretch()
        register_layout.addLayout(register_text_layout)

        scroll_layout.addWidget(register_group)

        # Virtgroup Components
        virtgroup_group = QGroupBox("Virtgroup Components")
        virtgroup_layout = QVBoxLayout(virtgroup_group)

        # Virtgroup Background Color
        virtgroup_bg_layout = QHBoxLayout()
        virtgroup_bg_layout.addWidget(QLabel("Background Color:"))
        self.virtgroup_bg_color = QPushButton("Select Color")
        self.virtgroup_bg_color.clicked.connect(lambda: self._select_color_for_component("virtgroup_bg"))
        virtgroup_bg_layout.addWidget(self.virtgroup_bg_color)
        virtgroup_bg_layout.addStretch()
        virtgroup_layout.addLayout(virtgroup_bg_layout)

        # Virtgroup Border Color
        virtgroup_border_layout = QHBoxLayout()
        virtgroup_border_layout.addWidget(QLabel("Border Color:"))
        self.virtgroup_border_color = QPushButton("Select Color")
        self.virtgroup_border_color.clicked.connect(lambda: self._select_color_for_component("virtgroup_border"))
        virtgroup_border_layout.addWidget(self.virtgroup_border_color)
        virtgroup_border_layout.addStretch()
        virtgroup_layout.addLayout(virtgroup_border_layout)

        scroll_layout.addWidget(virtgroup_group)

        # Scrollgroup Components
        scrollgroup_group = QGroupBox("Scrollgroup Components")
        scrollgroup_layout = QVBoxLayout(scrollgroup_group)

        # Scrollgroup Background Color
        scrollgroup_bg_layout = QHBoxLayout()
        scrollgroup_bg_layout.addWidget(QLabel("Background Color:"))
        self.scrollgroup_bg_color = QPushButton("Select Color")
        self.scrollgroup_bg_color.clicked.connect(lambda: self._select_color_for_component("scrollgroup_bg"))
        scrollgroup_bg_layout.addWidget(self.scrollgroup_bg_color)
        scrollgroup_bg_layout.addStretch()
        scrollgroup_layout.addLayout(scrollgroup_bg_layout)

        # Scrollgroup Border Color
        scrollgroup_border_layout = QHBoxLayout()
        scrollgroup_border_layout.addWidget(QLabel("Border Color:"))
        self.scrollgroup_border_color = QPushButton("Select Color")
        self.scrollgroup_border_color.clicked.connect(lambda: self._select_color_for_component("scrollgroup_border"))
        scrollgroup_border_layout.addWidget(self.scrollgroup_border_color)
        scrollgroup_border_layout.addStretch()
        scrollgroup_layout.addLayout(scrollgroup_border_layout)

        scroll_layout.addWidget(scrollgroup_group)

        # Popobject Components
        popobject_group = QGroupBox("Popobject Components")
        popobject_layout = QVBoxLayout(popobject_group)

        # Popobject Background Color
        popobject_bg_layout = QHBoxLayout()
        popobject_bg_layout.addWidget(QLabel("Background Color:"))
        self.popobject_bg_color = QPushButton("Select Color")
        self.popobject_bg_color.clicked.connect(lambda: self._select_color_for_component("popobject_bg"))
        popobject_bg_layout.addWidget(self.popobject_bg_color)
        popobject_bg_layout.addStretch()
        popobject_layout.addLayout(popobject_bg_layout)

        # Popobject Border Color
        popobject_border_layout = QHBoxLayout()
        popobject_border_layout.addWidget(QLabel("Border Color:"))
        self.popobject_border_color = QPushButton("Select Color")
        self.popobject_border_color.clicked.connect(lambda: self._select_color_for_component("popobject_border"))
        popobject_border_layout.addWidget(self.popobject_border_color)
        popobject_border_layout.addStretch()
        popobject_layout.addLayout(popobject_border_layout)

        scroll_layout.addWidget(popobject_group)

        scroll_layout.addStretch()
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        return tab

    def _select_color_for_component(self, component_type): #vers 1
        """Open color dialog to select color for specific UI component type"""
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        
        current_color = QColor(255, 255, 255)  # Default white
        color = QColorDialog.getColor(current_color, self, f"Select {component_type.replace('_', ' ').title()} Color")
        
        if color.isValid():
            color_name = color.name()
            # Update the button text to show selected color
            button_map = {
                "group_bg": self.group_bg_color,
                "group_border": self.group_border_color,
                "scrollbar_bg": self.scrollbar_bg_color,
                "scrollbar_handle": self.scrollbar_handle_color,
                "listview_bg": self.listview_bg_color,
                "listview_text": self.listview_text_color,
                "listview_select": self.listview_select_color,
                "register_bg": self.register_bg_color,
                "register_text": self.register_text_color,
                "virtgroup_bg": self.virtgroup_bg_color,
                "virtgroup_border": self.virtgroup_border_color,
                "scrollgroup_bg": self.scrollgroup_bg_color,
                "scrollgroup_border": self.scrollgroup_border_color,
                "popobject_bg": self.popobject_bg_color,
                "popobject_border": self.popobject_border_color,
            }
            
            if component_type in button_map:
                button_map[component_type].setText(f"Selected: {color_name}")
            
            # Store the color for later use
            setattr(self, f"{component_type}_color", color_name)
            print(f"{component_type} color selected: {color_name}")

    # ===== GLOBAL SLIDER HANDLERS =====

    def _on_global_hue_changed(self, value): #vers 2
        """Handle global hue slider change - applies to ALL colors"""
        self.global_hue_value.setText(str(value))
        hue_shift = value
        sat_shift = self.global_sat_slider.value()
        bri_shift = self.global_bri_slider.value()
        self.apply_global_hsb_to_all_colors(hue_shift, sat_shift, bri_shift)

    def _on_global_sat_changed(self, value): #vers 2
        """Handle global saturation slider change - applies to ALL colors"""
        self.global_sat_value.setText(str(value))
        hue_shift = self.global_hue_slider.value()
        sat_shift = value
        bri_shift = self.global_bri_slider.value()
        self.apply_global_hsb_to_all_colors(hue_shift, sat_shift, bri_shift)

    def _on_global_bri_changed(self, value): #vers 2
        """Handle global brightness slider change - applies to ALL colors"""
        self.global_bri_value.setText(str(value))
        hue_shift = self.global_hue_slider.value()
        sat_shift = self.global_sat_slider.value()
        bri_shift = value
        self.apply_global_hsb_to_all_colors(hue_shift, sat_shift, bri_shift)

    def apply_global_hsb_to_all_colors(self, hue_shift, sat_shift, bri_shift): #vers 3
        """Apply HSB adjustments to ALL theme colors globally - respects locks"""
        if not hasattr(self, 'color_editors'):
            return

        if not hasattr(self, 'theme_selector_combo'):
            return

        current_theme = self.theme_selector_combo.currentData()
        if not current_theme:
            return

        original_colors = self.app_settings.get_theme_colors(current_theme)

        # Apply adjustments to each color
        for color_key, original_hex in original_colors.items():
            if color_key not in self.color_editors:
                continue

            editor = self.color_editors[color_key]

            # SKIP if locked
            if editor.is_locked:
                continue

            # Convert to HSL
            h, s, l = rgb_to_hsl(original_hex)

            # Apply shifts
            h = (h + hue_shift) % 360
            s = max(0, min(100, s + sat_shift))
            l = max(0, min(100, l + bri_shift))

            # Convert back to hex
            new_hex = hsl_to_rgb(h, s, l)

            # Update the color editor
            editor.update_color(new_hex)

            # Store modified color
            if not hasattr(self, '_modified_colors'):
                self._modified_colors = {}
            self._modified_colors[color_key] = new_hex

    def _reset_global_sliders(self): #vers 1
        """Reset global HSB sliders to default (0)"""
        self.global_hue_slider.setValue(0)
        self.global_sat_slider.setValue(0)
        self.global_bri_slider.setValue(0)

        current_theme = self.theme_selector_combo.currentData()
        if current_theme:
            original_colors = self.app_settings.get_theme_colors(current_theme)
            self._modified_colors = original_colors.copy()

            for color_key, editor in self.color_editors.items():
                if color_key in original_colors:
                    editor.update_color(original_colors[color_key])

    def _lock_all_colors(self): #vers 1
        """Lock all color editors"""
        if hasattr(self, 'color_editors'):
            for editor in self.color_editors.values():
                editor.set_locked(True)

    def _unlock_all_colors(self): #vers 1
        """Unlock all color editors"""
        if hasattr(self, 'color_editors'):
            for editor in self.color_editors.values():
                editor.set_locked(False)

    # ===== THEME MANAGEMENT =====

    def _on_theme_changed(self, theme_name): #vers 2
        """Handle theme selection change"""
        theme_key = None
        for key, data in self.app_settings.themes.items():
            if data.get("name", key) == theme_name:
                theme_key = key
                break

        if theme_key:
            self._load_theme_colors(theme_key)

            # Apply instantly if checkbox is enabled (copied from _apply_demo_theme)
            if hasattr(self, 'instant_apply_check') and self.instant_apply_check.isChecked():
                # Update settings temporarily
                self.app_settings.current_settings["theme"] = theme_key

                # Get and apply stylesheet
                stylesheet = self.app_settings.get_stylesheet()
                self.setStyleSheet(stylesheet)

                # Emit signal to parent
                self.themeChanged.emit(theme_key)

    def _load_theme_colors(self, theme_key): #vers 1
        """Load colors for selected theme into editors"""
        if theme_key in self.app_settings.themes:
            colors = self.app_settings.themes[theme_key].get("colors", {})

            for color_key, editor in self.color_editors.items():
                color_value = colors.get(color_key, "#ffffff")
                editor.set_color(color_value)

    def _on_theme_color_changed(self, color_key, hex_value): #vers 1
        """Handle individual color changes"""
        if not hasattr(self, '_modified_colors'):
            self._modified_colors = {}
        self._modified_colors[color_key] = hex_value

    def _on_color_changed(self, element_key, hex_color): #vers 1
        """Handle color change from color picker"""
        if not hasattr(self, '_modified_colors'):
            self._modified_colors = self.app_settings.get_theme_colors().copy()
        self._modified_colors[element_key] = hex_color

    def _apply_picked_color(self): #vers 1
        """Apply picked color to selected element"""
        picked_color = self.color_picker.current_color
        selected_data = self.selected_element_combo.currentData()

        if selected_data and picked_color:
            if selected_data in self.color_editors:
                self.color_editors[selected_data].set_color(picked_color)

            if hasattr(self, 'demo_log'):
                element_name = self.selected_element_combo.currentText()
                self.demo_log.append(f"Applied {picked_color} to {element_name}")

    def _refresh_themes(self): #vers 1
        """Refresh themes from disk"""
        current_theme = self.theme_selector_combo.currentData()
        self.app_settings.refresh_themes()

        self.theme_selector_combo.clear()
        for theme_key, theme_data in self.app_settings.themes.items():
            display_name = theme_data.get("name", theme_key)
            self.theme_selector_combo.addItem(display_name, theme_key)

        index = self.theme_selector_combo.findData(current_theme)
        if index >= 0:
            self.theme_selector_combo.setCurrentIndex(index)

    def refresh_themes_in_dialog(self): #vers 1
        """Refresh themes in settings dialog"""
        if hasattr(self, 'demo_theme_combo'):
            current_theme = self.demo_theme_combo.currentText()
            self.app_settings.refresh_themes()

            self.demo_theme_combo.clear()
            for theme_key in self.app_settings.themes.keys():
                self.demo_theme_combo.addItem(theme_key)

            index = self.demo_theme_combo.findText(current_theme)
            if index >= 0:
                self.demo_theme_combo.setCurrentIndex(index)

            if hasattr(self, 'demo_log'):
                self.demo_log.append(f"Refreshed: {len(self.app_settings.themes)} themes")


    # ===== SETTINGS MANAGEMENT =====

    def _load_current_settings(self): #vers 5
        """Load current settings into UI"""
        # Set theme
        if hasattr(self, 'theme_selector_combo'):
            current_theme = self.app_settings.current_settings.get("theme", "App_Factory")
            index = self.theme_selector_combo.findData(current_theme)
            if index >= 0:
                self.theme_selector_combo.setCurrentIndex(index)

       # Backward compatibility: migrate old font_family/font_size to default_font_*
        if 'font_family' in self.app_settings.current_settings and 'default' in self.font_controls:
            old_family = self.app_settings.current_settings.get('font_family', 'Segoe UI')
            old_size = self.app_settings.current_settings.get('font_size', 9)

            if 'default_font_family' not in self.app_settings.current_settings:
                self.app_settings.current_settings['default_font_family'] = old_family
                self.app_settings.current_settings['default_font_size'] = old_size
                self.font_controls['default']['family'].setCurrentFont(QFont(old_family))
                self.font_controls['default']['size'].setValue(old_size)

        # Load font settings
        if hasattr(self, 'font_controls'):
            self._load_font_settings()

        # Set interface settings
        if hasattr(self, 'font_family_combo'):
            self.font_family_combo.setCurrentText(
                self.app_settings.current_settings.get("font_family", "Segoe UI")
            )
        if hasattr(self, 'font_size_spin'):
            self.font_size_spin.setValue(
                self.app_settings.current_settings.get("font_size", 9)
            )
        if hasattr(self, 'tooltips_check'):
            self.tooltips_check.setChecked(
                self.app_settings.current_settings.get("show_tooltips", True)
            )
        if hasattr(self, 'menu_icons_check'):
            self.menu_icons_check.setChecked(
                self.app_settings.current_settings.get("show_menu_icons", True)
            )
        if hasattr(self, 'button_icons_check'):
            self.button_icons_check.setChecked(
                self.app_settings.current_settings.get("show_button_icons", False)
            )

            # Button display mode
        if hasattr(self, 'button_display_combo'):
            current_mode = self.app_settings.current_settings.get("button_display_mode", "both")
            mode_map = {"both": 0, "icons": 1, "text": 2}
            self.button_display_combo.setCurrentIndex(mode_map.get(current_mode, 0))

        # Window controls
        if hasattr(self, 'custom_gadgets_check'):
            self.custom_gadgets_check.setChecked(
                self.app_settings.current_settings.get("use_custom_gadgets", False)
            )
        if hasattr(self, 'corner_resize_check'):
            self.corner_resize_check.setChecked(
                self.app_settings.current_settings.get("enable_corner_resize", True)
            )

        # Interface settings
        if hasattr(self, 'tooltips_check'):
            self.tooltips_check.setChecked(
                self.app_settings.current_settings.get("show_tooltips", True)
            )
        if hasattr(self, 'menu_icons_check'):
            self.menu_icons_check.setChecked(
                self.app_settings.current_settings.get("show_menu_icons", True)
            )
        if hasattr(self, 'use_svg_icons_check'):
            self.use_svg_icons_check.setChecked(
                self.app_settings.current_settings.get("use_svg_icons", True)
            )

    def _get_dialog_settings(self): #vers 2
        """Collect all settings from dialog controls"""
        settings = {}

        # Get theme from demo tab if available
        if hasattr(self, 'demo_theme_combo'):
            settings["theme"] = self.demo_theme_combo.currentText()
        elif hasattr(self, 'theme_selector_combo'):
            theme_data = self.theme_selector_combo.currentData()
            if theme_data:
                settings["theme"] = theme_data
        else:
            settings["theme"] = self.app_settings.current_settings["theme"]

        # Font settings
        if hasattr(self, 'font_family_combo'):
            settings["font_family"] = self.font_family_combo.currentText()
        if hasattr(self, 'font_size_spin'):
            settings["font_size"] = self.font_size_spin.value()

        # Interface settings
        if hasattr(self, 'tooltips_check'):
            settings["show_tooltips"] = self.tooltips_check.isChecked()
        if hasattr(self, 'menu_icons_check'):
            settings["show_menu_icons"] = self.menu_icons_check.isChecked()
        if hasattr(self, 'button_icons_check'):
            settings["show_button_icons"] = self.button_icons_check.isChecked()

        # Button display mode
        if hasattr(self, 'button_display_combo'):
            mode_index = self.button_display_combo.currentIndex()
            mode_map = {0: "both", 1: "icons", 2: "text"}
            settings["button_display_mode"] = mode_map.get(mode_index, "both")

        # Window controls
        if hasattr(self, 'custom_gadgets_check'):
            settings["use_custom_gadgets"] = self.custom_gadgets_check.isChecked()
        if hasattr(self, 'corner_resize_check'):
            settings["enable_corner_resize"] = self.corner_resize_check.isChecked()

        # Interface settings
        if hasattr(self, 'tooltips_check'):
            settings["show_tooltips"] = self.tooltips_check.isChecked()
        if hasattr(self, 'menu_icons_check'):
            settings["show_menu_icons"] = self.menu_icons_check.isChecked()
        if hasattr(self, 'use_svg_icons_check'):
            settings["use_svg_icons"] = self.use_svg_icons_check.isChecked()

        # Debug settings
        if hasattr(self, 'debug_enabled_check'):
            settings["debug_mode"] = self.debug_enabled_check.isChecked()
        if hasattr(self, 'debug_level_combo'):
            settings["debug_level"] = self.debug_level_combo.currentText()
        if hasattr(self, 'debug_categories'):
            enabled_categories = []
            for category, checkbox in self.debug_categories.items():
                if checkbox.isChecked():
                    enabled_categories.append(category)
            settings["debug_categories"] = enabled_categories

        return settings


    def _apply_settings(self): #vers 6
        """Apply settings permanently and save to appfactory.settings.json AND theme files"""
        new_settings = self._get_dialog_settings()
        old_theme = self.app_settings.current_settings["theme"]
        old_custom_gadgets = self.app_settings.current_settings.get("use_custom_gadgets", False)

        # Update settings
        self.app_settings.current_settings.update(new_settings)

        # Save font settings if modified
        if hasattr(self, 'font_controls'):
            self._save_font_settings()

        # ⭐ CRITICAL FIX: Save modified colors to the actual theme JSON file
        if hasattr(self, '_modified_colors') and self._modified_colors:
            current_theme = self.app_settings.current_settings["theme"]
            if current_theme in self.app_settings.themes:
                # Update in-memory theme
                if "colors" not in self.app_settings.themes[current_theme]:
                    self.app_settings.themes[current_theme]["colors"] = {}
                self.app_settings.themes[current_theme]["colors"].update(self._modified_colors)

                # THIS IS THE FIX: Actually save theme to disk
                theme_data = self.app_settings.themes[current_theme]
                success = self.app_settings.save_theme(current_theme, theme_data)

                if success:
                    print(f"Saved {len(self._modified_colors)} color changes to theme: {current_theme}")
                else:
                    print(f"Failed to save theme: {current_theme}")
                    QMessageBox.warning(
                        self,
                        "Theme Save Warning",
                        f"Settings applied but could not save theme file.\n"
                        f"Changes may be lost on restart.\n\n"
                        f"Check file permissions for themes/{current_theme}.json"
                    )

        # Save settings to appfactory.settings.json
        self.app_settings.save_settings()

        # Apply theme and update icons
        stylesheet = self.app_settings.get_stylesheet()
        self.setStyleSheet(stylesheet)

        # Emit signals
        if new_settings["theme"] != old_theme:
            self.themeChanged.emit(new_settings["theme"])

        # Check if custom gadgets setting changed
        new_custom_gadgets = new_settings.get("use_custom_gadgets", False)
        if old_custom_gadgets != new_custom_gadgets:
            QMessageBox.information(
                self,
                "Restart Required",
                "Window gadget changes will take effect after restarting the application."
            )

        self.settingsChanged.emit()

        QMessageBox.information(
            self,
            "Applied",
            f"Settings saved successfully!\n\n"
            f"Active theme: {new_settings['theme']}\n"
            f"Colors modified: {len(self._modified_colors) if hasattr(self, '_modified_colors') else 0}"
        )

        # Clear modified colors after saving
        if hasattr(self, '_modified_colors'):
            self._modified_colors = {}

    def _reset_to_defaults(self): #vers 1
        """Reset to default settings"""
        reply = QMessageBox.question(
            self,
            "Reset to Defaults",
            "This will reset all settings to their default values.\n\nAre you sure?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            self.app_settings.current_settings = self.app_settings.default_settings.copy()
            self._load_current_settings()
            QMessageBox.information(self, "Reset", "Settings reset to defaults")

    def _clear_debug_log(self): #vers 1
        """Clear the activity log"""
        if hasattr(self.parent(), 'gui_layout') and hasattr(self.parent().gui_layout, 'log'):
            self.parent().gui_layout.log.clear()
            if hasattr(self.parent(), 'log_message'):
                self.parent().log_message("Debug log cleared")
        else:
            QMessageBox.information(self, "Clear Log", "Activity log cleared (if available).")

    def _ok_clicked(self): #vers 1
        """OK button clicked"""
        self._apply_settings()
        self.accept()



    def _save_current_theme(self): #vers 2
        """Save modifications to the currently selected theme file in themes/"""
        current_theme_key = self.theme_selector_combo.currentData()

        if not current_theme_key:
            QMessageBox.warning(self, "No Theme", "No theme selected to save.")
            return

        # Get current theme data as base (deep copy to preserve everything)
        import copy
        theme_data = copy.deepcopy(self.app_settings.themes.get(current_theme_key, {}))

        # Update only the color values from editors (preserve all other color section data)
        if "colors" not in theme_data:
            theme_data["colors"] = {}

        for color_key, editor in self.color_editors.items():
            theme_data["colors"][color_key] = editor.color_input.text()

        # Collect gadget styles if modified
        if hasattr(self, '_gadget_modified') and self._gadget_modified:
            gadget_styles = self._collect_gadget_styles()
            theme_data["styles"] = gadget_styles

        # Save theme to themes/ directory
        success = self.app_settings.save_theme_to_file(current_theme_key, theme_data)

        if success:
            # Update in-memory theme
            self.app_settings.themes[current_theme_key] = theme_data

            theme_file = self.app_settings.themes_dir / f"{current_theme_key}.json"
            QMessageBox.information(
                self, "Saved",
                f"Theme '{current_theme_key}' saved successfully!\n\nFile: {theme_file}"
            )
        else:
            QMessageBox.warning(
                self, "Save Failed",
                f"Failed to save theme '{current_theme_key}' to themes/ directory."
            )


    def _save_theme_as(self): #vers 7
        """Save current theme as a new theme with file dialog - PRESERVES ALL DATA"""
        from PyQt6.QtWidgets import QInputDialog, QFileDialog, QMessageBox
        import json
        import copy
        import os

        # Ask for new theme name
        theme_name, ok = QInputDialog.getText(
            self,
            "Save Theme As",
            "Theme Naming Guidelines:\n"
            "• Dark themes: Add _Dark suffix (e.g., Ocean_Dark)\n"
            "• Light themes: Add _Light suffix (e.g., Ocean_Light)\n\n"
            "Enter new theme name:",
            text="My_Custom_Theme"
        )

        if not ok or not theme_name:
            return

        # Create safe filename
        safe_filename = "".join(c if c.isalnum() or c in (' ', '_', '-') else '_' for c in theme_name)
        safe_filename = safe_filename.replace(' ', '_').lower()

        # Get current theme as base (DEEP COPY preserves all nested structures)
        current_theme_key = self.theme_selector_combo.currentData()
        base_theme_data = self.app_settings.themes.get(current_theme_key, {})
        theme_data = copy.deepcopy(base_theme_data)

        # Update only the color values from editors (preserve layout params, labels, etc.)
        if "colors" not in theme_data:
            theme_data["colors"] = {}

        for color_key, editor in self.color_editors.items():
            # Only update actual color values, preserve everything else in colors section
            if color_key in theme_data["colors"] or color_key in self.theme_colors:
                theme_data["colors"][color_key] = editor.color_input.text()

        # Update theme metadata
        theme_data.update({
            "name": theme_name,
            "description": f"Custom theme created from {self.theme_selector_combo.currentText()}",
            "category": "Custom",
            "author": "User",
            "version": "1.0"
        })

        # Collect gadget styles if modified
        if hasattr(self, '_gadget_modified') and self._gadget_modified:
            gadget_styles = self._collect_gadget_styles()
            theme_data["styles"] = gadget_styles

        # Show file dialog
        themes_dir = str(self.app_settings.themes_dir)
        os.makedirs(themes_dir, exist_ok=True)
        default_path = os.path.join(themes_dir, f"{safe_filename}.json")

        file_path, _ = QFileDialog.getSaveFileName(
            self,
            "Save Theme As",
            default_path,
            "JSON Theme Files (*.json);;All Files (*)"
        )

        if not file_path:
            return

        # Ensure .json extension
        if not file_path.endswith('.json'):
            file_path += '.json'

        # Save theme file
        try:
            with open(file_path, 'w', encoding='utf-8') as f:
                json.dump(theme_data, f, indent=2, ensure_ascii=False)

            # Extract filename without extension for theme key
            theme_key = os.path.splitext(os.path.basename(file_path))[0]

            # Update in-memory themes
            self.app_settings.themes[theme_key] = theme_data

            # Count components for feedback
            color_count = len(theme_data.get('colors', {}))
            menu_items = theme_data.get('menus', {}).get('menu_items', {})
            menu_count = sum(len(items) for items in menu_items.values()) if menu_items else 0
            button_panels = theme_data.get('button_panels', {})
            button_count = sum(len(panel) for panel in button_panels.values()) if button_panels else 0
            font_count = len(theme_data.get('fonts', {}))
            has_styles = "Yes" if "styles" in theme_data else "No"

            QMessageBox.information(
                self, "Theme Saved",
                f"Theme '{theme_name}' saved successfully!\n\n"
                f"File: {file_path}\n\n"
                f"Components saved:\n"
                f"  • Colors section: {color_count} entries\n"
                f"  • Menu items: {menu_count}\n"
                f"  • Button panels: {button_count} buttons\n"
                f"  • Font definitions: {font_count}\n"
                f"  • Widget styles: {has_styles}"
            )

        except Exception as e:
            QMessageBox.warning(
                self, "Save Failed",
                f"Failed to save theme '{theme_name}'.\n\nError: {str(e)}"
            )

    def get_contrast_text_color(self, bg_color: str) -> str: #vers 1
        """
        Calculate whether to use black or white text based on background brightness
        Returns '#000000' for light backgrounds, '#ffffff' for dark backgrounds
        """
        # Remove # if present
        if bg_color.startswith('#'):
            bg_color = bg_color[1:]

        # Handle 3-digit hex codes
        if len(bg_color) == 3:
            bg_color = ''.join([c*2 for c in bg_color])

        # Convert to RGB
        try:
            r = int(bg_color[:2], 16)
            g = int(bg_color[2:4], 16)
            b = int(bg_color[4:6], 16)
        except (ValueError, IndexError):
            return '#000000'  # Fallback to black

        # Calculate relative luminance using WCAG formula
        def get_luminance_component(c):
            c = c / 255.0
            if c <= 0.03928:
                return c / 12.92
            else:
                return ((c + 0.055) / 1.055) ** 2.4

        r_lum = get_luminance_component(r)
        g_lum = get_luminance_component(g)
        b_lum = get_luminance_component(b)

        luminance = 0.2126 * r_lum + 0.7152 * g_lum + 0.0722 * b_lum

        # Use white text for dark backgrounds (luminance < 0.5)
        # Use black text for light backgrounds (luminance >= 0.5)
        return '#ffffff' if luminance < 0.5 else '#000000'


class IconProvider: #vers 2
    """Provides SVG icons that adapt to theme colors"""

    def __init__(self, parent_widget, app_settings=None):
        """Initialize with parent widget and optional app_settings for theme access"""
        self.parent = parent_widget
        self.app_settings = app_settings or getattr(parent_widget, 'app_settings', None)
        self._icon_cache = {}

    def _get_icon_color(self): #vers 3
        """Get appropriate icon color based on current theme - uses theme text color"""
        if not self.app_settings:
            return '#000000'

        # Use the theme's primary text color for icons
        colors = self.app_settings.get_theme_colors()
        text_color = colors.get('text_primary', '#000000')

        return text_color

    def _svg_to_icon(self, svg_data, size=24, force_refresh=False): #vers 3
        """Convert SVG data to QIcon with theme color support"""
        from PyQt6.QtGui import QIcon, QPixmap, QPainter, QColor
        from PyQt6.QtSvg import QSvgRenderer
        from PyQt6.QtCore import QByteArray

        # Create cache key
        cache_key = (svg_data, size, self._get_icon_color())

        # Return cached icon if available and not forcing refresh
        if not force_refresh and cache_key in self._icon_cache:
            return self._icon_cache[cache_key]

        try:
            icon_color = self._get_icon_color()

            # Replace currentColor with determined color
            svg_str = svg_data.decode('utf-8')
            svg_str = svg_str.replace('currentColor', icon_color)
            svg_data = svg_str.encode('utf-8')

            renderer = QSvgRenderer(QByteArray(svg_data))
            pixmap = QPixmap(size, size)
            pixmap.fill(QColor(0, 0, 0, 0))  # Transparent background

            painter = QPainter(pixmap)
            renderer.render(painter)
            painter.end()

            icon = QIcon(pixmap)
            self._icon_cache[cache_key] = icon
            return icon
        except Exception as e:
            print(f"Error creating icon: {e}")
            return QIcon()

    def clear_cache(self): #vers 1
        """Clear icon cache to force regeneration with new theme colors"""
        self._icon_cache.clear()

    def restore_icon(self): #vers 1
        """Restore - Two overlapping squares"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <rect x="7" y="7" width="10" height="10"
                stroke="currentColor" stroke-width="2"
                fill="none" rx="2"/>
            <path d="M11 5h6a2 2 0 012 2v6"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def minimize_icon(self): #vers 1
        """Minimize - Horizontal line"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <line x1="5" y1="12" x2="19" y2="12"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def maximize_icon(self): #vers 1
        """Maximize - Square"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <rect x="5" y="5" width="14" height="14"
                stroke="currentColor" stroke-width="2"
                fill="none" rx="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def close_icon(self): #vers 1
        """Close - X icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <line x1="6" y1="6" x2="18" y2="18"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
            <line x1="18" y1="6" x2="6" y2="18"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    # File Operation Icons

    def folder_icon(self): #vers 1
        """Open - Folder icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-7l-2-2H5a2 2 0 00-2 2z"
                stroke="currentColor" stroke-width="2" stroke-linejoin="round" fill="none"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def save_icon(self): #vers 1
        """Save - Floppy disk icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M19 21H5a2 2 0 01-2-2V5a2 2 0 012-2h11l5 5v11a2 2 0 01-2 2z"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M17 21v-8H7v8M7 3v5h8" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def import_icon(self): #vers 1
        """Import - Download arrow"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="7 10 12 15 17 10"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="15" x2="12" y2="3"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def export_icon(self): #vers 1
        """Export - Upload arrow"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="17 8 12 3 7 8"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="3" x2="12" y2="15"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    # Edit Icons

    def add_icon(self): #vers 1
        """Add - Plus icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <line x1="12" y1="5" x2="12" y2="19"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="5" y1="12" x2="19" y2="12"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def delete_icon(self): #vers 1
        """Delete - Trash icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <polyline points="3 6 5 6 21 6"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M19 6v14a2 2 0 01-2 2H7a2 2 0 01-2-2V6m3 0V4a2 2 0 012-2h4a2 2 0 012 2v2"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def edit_icon(self): #vers 1
        """Edit - Pencil icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M17 3a2.83 2.83 0 114 4L7.5 20.5 2 22l1.5-5.5L17 3z"
                stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def copy_icon(self): #vers 1
        """Copy icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <rect x="9" y="9" width="13" height="13" rx="2"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M5 15H4a2 2 0 01-2-2V4a2 2 0 012-2h9a2 2 0 012 2v1"
                stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    # View Icons

    def view_icon(self): #vers 1
        """View - Eye icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M12,9A3,3 0 0,0 9,12A3,3 0 0,0 12,15A3,3 0 0,0 15,12A3,3 0 0,0 12,9
                M12,17A5,5 0 0,1 7,12A5,5 0 0,1 12,7A5,5 0 0,1 17,12A5,5 0 0,1 12,17
                M12,4.5C7,4.5 2.73,7.61 1,12C2.73,16.39 7,19.5 12,19.5
                C17,19.5 21.27,16.39 23,12C21.27,7.61 17,4.5 12,4.5Z"
                fill="currentColor"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def settings_icon(self): #vers 1
        """Settings - Gear icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="3" stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M12 2v2M12 20v2M2 12h2M20 12h2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41M4.93 19.07l1.41-1.41M17.66 6.34l1.41-1.41"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def info_icon(self): #vers 1
        """Info - Circle with i"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="9" stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M12 11v6M12 8v.5" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def filter_icon(self): #vers 1
        """Filter - Sliders icon"""
        svg_data = b'''<svg viewBox="0 0 20 20">
            <circle cx="6" cy="4" r="2" fill="currentColor"/>
            <rect x="5" y="8" width="2" height="8" fill="currentColor"/>
            <circle cx="14" cy="12" r="2" fill="currentColor"/>
            <rect x="13" y="4" width="2" height="6" fill="currentColor"/>
            <circle cx="10" cy="8" r="2" fill="currentColor"/>
            <rect x="9" y="12" width="2" height="4" fill="currentColor"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    # Utility Icons

    def undo_icon(self): #vers 1
        """Undo - Curved arrow"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M3 7v6h6M3 13a9 9 0 1018 0 9 9 0 00-18 0z"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)

    def refresh_icon(self): #vers 1
        """Refresh - Circular arrow"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path d="M16 10A6 6 0 1 1 4 10M4 10l3-3m-3 3l3 3"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round" fill="none"/>
        </svg>'''
        return self._svg_to_icon(svg_data, size=20)


"""
# In your main window __init__:
from apps.utils.app_settings_system import IconProvider

self.icons = IconProvider(self)

# Then use icons like:
open_btn.setIcon(self.icons.folder_icon())
save_btn.setIcon(self.icons.save_icon())
import_btn.setIcon(self.icons.import_icon())
export_btn.setIcon(self.icons.export_icon())
settings_btn.setIcon(self.icons.settings_icon())
"""

def rgb_to_hsl(hex_color): #vers 1
    """Convert hex color to HSL"""
    hex_color = hex_color.lstrip('#')
    if len(hex_color) == 3:
        hex_color = ''.join([c*2 for c in hex_color])

    r, g, b = [int(hex_color[i:i+2], 16) / 255.0 for i in (0, 2, 4)]

    max_c = max(r, g, b)
    min_c = min(r, g, b)
    l = (max_c + min_c) / 2.0

    if max_c == min_c:
        h = s = 0.0
    else:
        d = max_c - min_c
        s = d / (2.0 - max_c - min_c) if l > 0.5 else d / (max_c + min_c)

        if max_c == r:
            h = (g - b) / d + (6 if g < b else 0)
        elif max_c == g:
            h = (b - r) / d + 2
        else:
            h = (r - g) / d + 4
        h /= 6.0

    return int(h * 360), int(s * 100), int(l * 100)


def hsl_to_rgb(h, s, l): #vers 1
    """Convert HSL to hex color"""
    h = h / 360.0
    s = s / 100.0
    l = l / 100.0

    def hue_to_rgb(p, q, t):
        if t < 0:
            t += 1
        if t > 1:
            t -= 1
        if t < 1/6:
            return p + (q - p) * 6 * t
        if t < 1/2:
            return q
        if t < 2/3:
            return p + (q - p) * (2/3 - t) * 6
        return p

    if s == 0:
        r = g = b = l
    else:
        q = l * (1 + s) if l < 0.5 else l + s - l * s
        p = 2 * l - q
        r = hue_to_rgb(p, q, h + 1/3)
        g = hue_to_rgb(p, q, h)
        b = hue_to_rgb(p, q, h - 1/3)

    return f"#{int(r*255):02x}{int(g*255):02x}{int(b*255):02x}"


def _create_debug_tab(self):
    """Create debug settings tab"""
    widget = QWidget()
    layout = QVBoxLayout(widget)

    # Debug Mode Group
    debug_group = QGroupBox("🐛 Debug Mode")
    debug_layout = QVBoxLayout(debug_group)

    self.debug_enabled_check = QCheckBox("Enable debug mode")
    self.debug_enabled_check.setChecked(self.app_settings.current_settings.get('debug_mode', False))
    self.debug_enabled_check.setToolTip("Enable detailed debug logging throughout the application")
    debug_layout.addWidget(self.debug_enabled_check)

    # Debug Level
    level_layout = QHBoxLayout()
    level_layout.addWidget(QLabel("Debug Level:"))
    self.debug_level_combo = QComboBox()
    self.debug_level_combo.addItems(["ERROR", "WARNING", "INFO", "DEBUG", "VERBOSE"])
    self.debug_level_combo.setCurrentText(self.app_settings.current_settings.get('debug_level', 'INFO'))
    self.debug_level_combo.setToolTip("Set the verbosity level for debug output")
    level_layout.addWidget(self.debug_level_combo)
    level_layout.addStretch()
    debug_layout.addLayout(level_layout)

    layout.addWidget(debug_group)

    # Debug Categories Group
    categories_group = QGroupBox("Debug Categories")
    categories_layout = QGridLayout(categories_group)

    self.debug_categories = {}
    default_categories = [
        ('IMG_LOADING', 'IMG file loading and parsing'),
        ('TABLE_POPULATION', 'Table display and entry population'),
        ('BUTTON_ACTIONS', 'Button clicks and UI actions'),
        ('FILE_OPERATIONS', 'File read/write operations'),
        ('FILTERING', 'Table filtering and search'),
        ('SIGNAL_SYSTEM', 'Unified signal system')
    ]

    enabled_categories = self.app_settings.current_settings.get('debug_categories', [cat[0] for cat in default_categories])

    for i, (category, description) in enumerate(default_categories):
        checkbox = QCheckBox(category.replace('_', ' ').title())
        checkbox.setChecked(category in enabled_categories)
        checkbox.setToolTip(description)
        self.debug_categories[category] = checkbox

        row = i // 2
        col = i % 2
        categories_layout.addWidget(checkbox, row, col)

    layout.addWidget(categories_group)

    # Debug Actions Group
    actions_group = QGroupBox("Debug Actions")
    actions_layout = QVBoxLayout(actions_group)

    # Quick debug buttons
    buttons_layout = QHBoxLayout()

    test_debug_btn = QPushButton("Test Debug")
    test_debug_btn.setToolTip("Send a test debug message")
    test_debug_btn.clicked.connect(self._test_debug_output)
    buttons_layout.addWidget(test_debug_btn)

    debug_img_btn = QPushButton("Debug IMG")
    debug_img_btn.setToolTip("Debug current IMG file (if loaded)")
    debug_img_btn.clicked.connect(self._debug_current_img)
    buttons_layout.addWidget(debug_img_btn)

    clear_log_btn = QPushButton("Clear Log")
    clear_log_btn.setToolTip("Clear the activity log")
    clear_log_btn.clicked.connect(self._clear_debug_log)
    buttons_layout.addWidget(clear_log_btn)

    actions_layout.addLayout(buttons_layout)
    layout.addWidget(actions_group)
    layout.addStretch()

    return widget

def _test_debug_output(self):
    """Test debug output"""
    if hasattr(self.parent(), 'log_message'):
        self.parent().log_message("Debug test message - debug system working!")
        self.parent().log_message(f"[DEBUG-TEST] Debug enabled: {self.debug_enabled_check.isChecked()}")
        self.parent().log_message(f"[DEBUG-TEST] Debug level: {self.debug_level_combo.currentText()}")

        enabled_categories = [cat for cat, cb in self.debug_categories.items() if cb.isChecked()]
        self.parent().log_message(f"[DEBUG-TEST] Active categories: {', '.join(enabled_categories)}")
    else:
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(self, "Debug Test", "Debug test completed!\nCheck the activity log for output.")

def _debug_current_img(self):
    """Debug current IMG file"""
    if hasattr(self.parent(), 'current_img') and self.parent().current_img:
        img = self.parent().current_img
        self.parent().log_message(f"[DEBUG-IMG] Current IMG: {img.file_path}")
        self.parent().log_message(f"[DEBUG-IMG] Entries: {len(img.entries)}")

        # Count file types
        file_types = {}
        for entry in img.entries:
            ext = entry.name.split('.')[-1].upper() if '.' in entry.name else "NO_EXT"
            file_types[ext] = file_types.get(ext, 0) + 1

        self.parent().log_message(f"[DEBUG-IMG] File types found:")
        for ext, count in sorted(file_types.items()):
            self.parent().log_message(f"[DEBUG-IMG]   {ext}: {count} files")

        # Check table rows
        if hasattr(self.parent(), 'gui_layout') and hasattr(self.parent().gui_layout, 'table'):
            table = self.parent().gui_layout.table
            table_rows = table.rowCount()
            hidden_rows = sum(1 for row in range(table_rows) if table.isRowHidden(row))
            self.parent().log_message(f"[DEBUG-IMG] Table: {table_rows} rows, {hidden_rows} hidden")

    elif hasattr(self.parent(), 'log_message'):
        self.parent().log_message("[DEBUG-IMG] No IMG file currently loaded")
    else:
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(self, "Debug IMG", "No IMG file loaded or no debug function available.")

def _clear_debug_log(self):
    """Clear the activity log"""
    if hasattr(self.parent(), 'gui_layout') and hasattr(self.parent().gui_layout, 'log'):
        self.parent().gui_layout.log.clear()
        self.parent().log_message("Debug log cleared")
    else:
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(self, "Clear Log", "Activity log cleared (if available).")

def apply_theme_to_app(app, app_settings):
    """Apply theme to entire application"""
    stylesheet = app_settings.get_stylesheet()
    app.setStyleSheet(stylesheet)


def hsl_to_rgb(h, s, l): #vers 1
    """Convert HSL to RGB and return hex color"""
    h = h / 360.0
    s = s / 100.0
    l = l / 100.0

    if s == 0:
        r = g = b = l
    else:
        def hue_to_rgb(p, q, t):
            if t < 0:
                t += 1
            if t > 1:
                t -= 1
            if t < 1/6:
                return p + (q - p) * 6 * t
            if t < 1/2:
                return q
            if t < 2/3:
                return p + (q - p) * (2/3 - t) * 6
            return p

        q = l * (1 + s) if l < 0.5 else l + s - l * s
        p = 2 * l - q

        r = hue_to_rgb(p, q, h + 1/3)
        g = hue_to_rgb(p, q, h)
        b = hue_to_rgb(p, q, h - 1/3)

    r = int(round(r * 255))
    g = int(round(g * 255))
    b = int(round(b * 255))

    return f"#{r:02x}{g:02x}{b:02x}"

def rgb_to_hsl(hex_color): #vers 1
    """Convert hex color to HSL values"""
    if hex_color.startswith('#'):
        hex_color = hex_color[1:]

    try:
        r = int(hex_color[0:2], 16) / 255.0
        g = int(hex_color[2:4], 16) / 255.0
        b = int(hex_color[4:6], 16) / 255.0
    except (ValueError, IndexError):
        return 0, 0, 50  # Default values

    max_val = max(r, g, b)
    min_val = min(r, g, b)
    diff = max_val - min_val

    # Lightness
    l = (max_val + min_val) / 2.0

    if diff == 0:
        h = s = 0  # achromatic
    else:
        # Saturation
        s = diff / (2 - max_val - min_val) if l > 0.5 else diff / (max_val + min_val)

        # Hue
        if max_val == r:
            h = (g - b) / diff + (6 if g < b else 0)
        elif max_val == g:
            h = (b - r) / diff + 2
        elif max_val == b:
            h = (r - g) / diff + 4
        h /= 6

    return int(h * 360), int(s * 100), int(l * 100)


# Clean main function for testing only
if __name__ == "__main__":
    import sys
    from PyQt6.QtWidgets import QApplication, QMainWindow

    app = QApplication(sys.argv)

    # Create settings
    settings = AppSettings()

    # Create simple test window
    main_window = QMainWindow()
    main_window.setWindowTitle("App Factory Settings Test")
    main_window.setMinimumSize(400, 300)

    # Apply theme
    apply_theme_to_app(app, settings)

    # Show settings dialog
    dialog = SettingsDialog(settings, main_window)
    if dialog.exec():
        print("Settings applied")

    sys.exit(0)
