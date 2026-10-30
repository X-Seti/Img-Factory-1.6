#!/usr/bin/env python3
#this belongs in apps/utils/app_settings_system.py - Version: 101
# $vers" X-Seti - June26, 2025 - App Factory - Package theme settings

"""
App Factory - App Settings System - Clean Version
Settings management without demo code
"""

import json
import os
from pathlib import Path
import sys

_root = str(Path(__file__).resolve().parents[2])   # run as script: find apps/
if _root not in sys.path:
    sys.path.insert(0, _root)
from PyQt6.QtCore import Qt, pyqtSignal, QDateTime  # Fixed: Added QDateTime
from PyQt6.QtGui import QFont

from PyQt6.QtWidgets import (
    QSizePolicy,
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

from apps.utils.depends.appsys_logic_func import AppsysLogicMixin
from apps.utils.depends.appsys_ui_func import AppsysUIMixin
from apps.utils.depends.appsys_win_func import AppsysWinMixin
from apps.utils.depends.appsys_panel_test_func import AppsysPanelTestMixin
from apps.utils.depends.App_System_Setting_Svg_icons import IconProvider
from apps.methods.dock_panels import DockPanelsMixin


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
        # MSS not installed — PIL fallback active (silent)
    except ImportError:
        PIL_AVAILABLE = False


##Methods to add to SettingsDialog class
# _create_buttons_tab (vers 1)
# _create_button_panel_editor (vers 1)
# _get_default_button_colors (vers 1)
# _on_button_color_changed (vers 1)
# _collect_current_button_colors (vers 1)

App_name = "Global App System Settings"


def get_titlebar_sizes(app_settings=None) -> dict: #vers 1
    """Return current titlebar/button size settings.
    All apps call this when building their toolbar so sizes stay in sync.

    Usage:
        from apps.utils.app_settings_system import get_titlebar_sizes
        sz = get_titlebar_sizes(self.app_settings)
        btn.setFixedSize(sz['btn_size'], sz['btn_size'])
        btn.setIconSize(QSize(sz['icon_size'], sz['icon_size']))
    """
    cs = {}
    if app_settings is not None:
        cs = getattr(app_settings, 'current_settings', {})
    return {
        'tb_height':  cs.get('titlebar_button_height', 25),
        'btn_size':   cs.get('titlebar_button_size',   25),
        'icon_size':  cs.get('titlebar_icon_size',     20),
        'btn_height': cs.get('button_size',            24),
    }
App_build = "23.69" #32 is from version 32 up top.
App_auth = "X-Seti"



def _readable_text(bg: str, fg: str) -> str: #vers 2
    """fg if it reads on bg (contrast 3:1), else black or white."""
    def lum(c):
        import re
        m = re.match(r'#?([0-9a-fA-F]{6})', c.strip())
        if m:
            ch = [int(m.group(1)[k:k + 2], 16) / 255 for k in (0, 2, 4)]
        else:                                            # rgba(r, g, b, a) from colour transparency
            nums = re.findall(r'[\d.]+', c)
            if not c.strip().startswith('rgb') or len(nums) < 3:
                return None
            ch = [float(v) / 255 for v in nums[:3]]
        ch = [v / 12.92 if v <= 0.03928 else ((v + 0.055) / 1.055) ** 2.4 for v in ch]
        return 0.2126 * ch[0] + 0.7152 * ch[1] + 0.0722 * ch[2]
    lb, lf = lum(bg), lum(fg)
    if lb is None or lf is None:
        return fg
    if (max(lb, lf) + 0.05) / (min(lb, lf) + 0.05) >= 3:
        return fg
    return '#000000' if lb > 0.4 else '#ffffff'

class ThemeSaveDialog(QDialog):
    """Dialog for saving themes with complete metadata"""

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
        form_group.setAutoFillBackground(True)
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
        color_group.setAutoFillBackground(True)
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
            'panel_primary': {'name': 'Card / Panel Surface', 'h': 210, 's': 8, 'b': 90},
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
            'toolbar_bg': {'name': 'Toolbar Background', 'h': 0, 's': 0, 'b': 98},
            'ribbon_bg':  {'name': 'Ribbon Background',  'h': 0, 's': 0, 'b': 98},
            'dialog_bg':  {'name': 'Dialog Background',  'h': 0, 's': 0, 'b': 99},
            'dialog_text':{'name': 'Dialog Text', 'h': 0, 's': 0, 'b': 20},
            'hero_bg': {'name': 'Hero_Background', 'h': 0, 's': 0, 'b': 20},
            'hero_gradient_dark_start': {'name': 'Hero_Gradient_Dark_Start','h': 0, 's': 0, 'b': 20},
            'hero_gradient_dark_end': {'name': 'Hero_Gradient_Dark_End','h': 0, 's': 0, 'b': 20},
            'hero_gradient_light_start': {'name': 'Hero_Gradient_Light_End','h': 0, 's': 0, 'b': 20},
            'hero_gradient_light_end': {'name': 'Hero_Gradient_Light_End','h': 0, 's': 0, 'b': 20},
            'window_bg': {'name': 'Window_Background','h': 0, 's': 0, 'b': 20},
            'window_text': {'name': 'Window_Text','h': 0, 's': 0, 'b': 20},
            'base': {'name': 'Base','h': 0, 's': 0, 'b': 20},
            'alternate_base': {'name': 'Alternate_Base','h': 0, 's': 0, 'b': 20},
            'tooltip_bg': {'name': 'Tooltip_Background','h': 0, 's': 0, 'b': 20},
            'tooltip_text': {'name': 'Tooltip_Text','h': 0, 's': 0, 'b': 20},
            'placeholder_text': {'name': 'Placeholder_Text','h': 0, 's': 0, 'b': 20},
            'disabled_text': {'name': 'Disabled_Text','h': 0, 's': 0, 'b': 20},
            'panel_fill_a': {'name': 'Panal_Fill_A','h': 0, 's': 0, 'b': 20},
            'panel_fill_b': {'name': 'Panal_Fill_B','h': 0, 's': 0, 'b': 20},
            'panel_grad_stop1': {'name': 'Panel_Grad_Stop1','h': 0, 's': 0, 'b': 20},
            'panel_grad_stop2': {'name': 'Panel_Grad_Stop2','h': 0, 's': 0, 'b': 20},
            'panel_grad_stop3': {'name': 'Panel_Grad_Stop3','h': 0, 's': 0, 'b': 20},
            'panel_pattern_light': {'name': 'Panel_Pattern_Light','h': 0, 's': 0, 'b': 20},
            'panel_pattern_dark': {'name': 'Panel_Pattern_Dark','h': 0, 's': 0, 'b': 20},
            'copper_light': {'name': 'Copper_Light','h': 0, 's': 0, 'b': 20},
            'copper_dark': {'name': 'Copper_Dark','h': 0, 's': 0, 'b': 20},
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


class PanelSectionHeader(QWidget): #vers 1
    """Bold divider row inserted into the colour list scroll layout."""
    def __init__(self, text, parent=None):
        super().__init__(parent)
        lay = QHBoxLayout(self)
        lay.setContentsMargins(4, 6, 4, 2)
        lbl = QLabel(text)
        lbl.setStyleSheet(
            "font-weight: bold; font-size: 9pt; color: palette(highlight);")
        lay.addWidget(lbl)
        line = QFrame()
        line.setFrameShape(QFrame.Shape.HLine)
        line.setFrameShadow(QFrame.Shadow.Sunken)
        lay.addWidget(line, 1)
        self.setFixedHeight(28)


class PanelControlRow(QWidget): #vers 1
    """A control row (combo/slider/button) that sits in the colour list alongside ThemeColorEditor rows."""
    def __init__(self, label, widget, extra=None, parent=None):
        super().__init__(parent)
        lay = QHBoxLayout(self)
        lay.setContentsMargins(34, 1, 5, 1)  # 34px left indent = lock(30) + small gap
        lay.setSpacing(4)
        if label:
            lbl = QLabel(label)
            lbl.setMinimumWidth(140)
            from PyQt6.QtWidgets import QSizePolicy as _SP
            lbl.setSizePolicy(_SP.Policy.Expanding, _SP.Policy.Preferred)
            lay.addWidget(lbl)
        lay.addWidget(widget)
        if extra:
            lay.addWidget(extra)
        self.setFixedHeight(28)


class _DraggableSwatch(QLabel): #vers 1
    """Colour swatch QLabel that initiates a drag when pressed and moved.
    Carries the hex colour string as mime text so any ThemeColorEditor can receive it."""

    def __init__(self, editor_parent):
        super().__init__(editor_parent)
        self._editor = editor_parent
        self._drag_start = None

    def mousePressEvent(self, event): #vers 1
        from PyQt6.QtCore import Qt
        if event.button() == Qt.MouseButton.LeftButton:
            self._drag_start = event.pos()
        super().mousePressEvent(event)

    def mouseMoveEvent(self, event): #vers 1
        from PyQt6.QtCore import Qt, QMimeData, QPoint
        from PyQt6.QtGui import QDrag, QPixmap, QPainter, QColor
        if not (event.buttons() & Qt.MouseButton.LeftButton):
            return
        if self._drag_start is None:
            return
        # Only start drag after a minimum move distance
        if (event.pos() - self._drag_start).manhattanLength() < 8:
            return

        hex_color = self._editor.current_value

        # Build a small coloured pixmap as the drag cursor
        px = QPixmap(32, 32)
        px.fill(QColor(hex_color))
        p = QPainter(px)
        p.setPen(QColor('#555555'))
        p.drawRect(0, 0, 31, 31)
        p.end()

        mime = QMimeData()
        mime.setText(hex_color)

        drag = QDrag(self)
        drag.setMimeData(mime)
        drag.setPixmap(px)
        drag.setHotSpot(QPoint(16, 16))
        drag.exec(Qt.DropAction.CopyAction)
        self._drag_start = None

class ThemeColorEditor(QWidget): #vers 7
    """Widget for editing individual theme colors.
    Swatch supports drag-and-drop: drag from swatch to copy colour to another row."""
    colorChanged = pyqtSignal(str, str)  # color_key, hex_color
    lockChanged = pyqtSignal(str, bool)  # color_key, is_locked
    alphaChanged = pyqtSignal(str, int)  # color_key, 0-100
    alphaToggled = pyqtSignal(str, bool)  # color_key, transparency on

    def __init__(self, color_key, color_name, current_value, parent=None, alpha=None, alpha_on=True): #vers 6
        super().__init__(parent)
        self.color_key = color_key
        self.color_name = color_name
        self.current_value = current_value
        self.alpha = alpha            # None = colour has no transparency
        self.alpha_on = alpha_on      # False: value kept, colour drawn solid
        self.is_locked = False
        self.setAcceptDrops(True)
        self._setup_ui()

    def _setup_ui(self): #vers 5
        """Setup the editor UI — swatch supports drag to copy colour."""
        layout = QHBoxLayout(self)
        layout.setContentsMargins(5, 2, 5, 2)

        # Lock checkbox
        self.lock_check = QCheckBox()
        self.lock_check.setFixedWidth(30)
        self.lock_check.setToolTip("Lock to prevent global adjustments")
        self.lock_check.stateChanged.connect(self._on_lock_changed)
        layout.addWidget(self.lock_check)

        # Color name label — expands to fill available space
        name_label = QLabel(self.color_name)
        name_label.setMinimumWidth(140)
        from PyQt6.QtWidgets import QSizePolicy as _SP
        name_label.setSizePolicy(_SP.Policy.Expanding, _SP.Policy.Preferred)
        layout.addWidget(name_label)

        # Transparency 0-100 (background colours only; blank keeps columns aligned)
        from PyQt6.QtWidgets import QToolButton
        self.alpha_btn = QToolButton()
        self.alpha_btn.setFixedSize(64, 28)
        self.alpha_btn.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
        if self.alpha is None:
            self.alpha_btn.setEnabled(False)
            self.alpha_btn.setAutoRaise(True)
        else:
            self.alpha_btn.setToolTip("Transparency: click to set 0-100 (100 = solid)")
            self.alpha_btn.clicked.connect(self._pick_alpha)
            self._show_alpha()
        layout.addWidget(self.alpha_btn)

        # Color preview swatch — drag source
        self.color_preview = _DraggableSwatch(self)
        self.color_preview.setFixedSize(28, 28)
        self.color_preview.setToolTip("Drag to another colour row to copy this colour")
        self.update_preview(self.current_value)
        layout.addWidget(self.color_preview)

        # Hex value input — fixed width for column alignment
        self.color_input = QLineEdit(self.current_value)
        self.color_input.setFixedWidth(82)
        self.color_input.setFont(QFont("monospace", 9))
        self.color_input.textChanged.connect(self.on_color_changed)
        layout.addWidget(self.color_input)

        # Pick button — fixed width, sits flush at right edge
        dialog_btn = QPushButton("Pick")
        dialog_btn.setFixedWidth(54)
        dialog_btn.setFixedHeight(28)
        dialog_btn.setToolTip("Open color picker dialog")
        dialog_btn.clicked.connect(self.open_color_dialog)
        layout.addWidget(dialog_btn)
        # NO addStretch() — name_label expansion handles alignment

    def _show_alpha(self): #vers 2
        """Alpha button: colour over a checkerboard at the set opacity, and the value (or off)."""
        from PyQt6.QtGui import QPixmap, QPainter, QIcon
        pm = QPixmap(16, 16)
        p = QPainter(pm)
        for y in range(0, 16, 4):
            for x in range(0, 16, 4):
                p.fillRect(x, y, 4, 4, QColor("#bbbbbb") if (x + y) // 4 % 2 else QColor("#ffffff"))
        c = QColor(self.current_value if QColor(self.current_value).isValid() else "#000000")
        c.setAlphaF(self.alpha / 100.0 if self.alpha_on else 1.0)
        p.fillRect(0, 0, 16, 16, c)
        p.end()
        self.alpha_btn.setIcon(QIcon(pm))
        self.alpha_btn.setText(f"{self.alpha}" if self.alpha_on else "off")

    def set_alpha(self, value): #vers 1
        """Set transparency 0-100 without emitting."""
        if self.alpha is not None:
            self.alpha = int(value)
            self._show_alpha()

    def set_alpha_on(self, on): #vers 1
        """Transparency on / off without emitting."""
        if self.alpha is not None:
            self.alpha_on = bool(on)
            self._show_alpha()

    def _pick_alpha(self): #vers 2
        """Pop-up on/off and slider 0-100 under the button; live update."""
        from PyQt6.QtWidgets import QMenu, QWidgetAction, QSlider
        menu = QMenu(self)
        box = QWidget(); bl = QHBoxLayout(box); bl.setContentsMargins(8, 4, 8, 4)
        on = QCheckBox("On"); on.setChecked(self.alpha_on)
        on.setToolTip("Off keeps the value, colour drawn solid")
        bl.addWidget(on)

        def _toggle(v):  #vers 1
            self.alpha_on = v
            sl.setEnabled(v)
            self._show_alpha()
            self.alphaToggled.emit(self.color_key, v)
        on.toggled.connect(_toggle)
        sl = QSlider(Qt.Orientation.Horizontal); sl.setRange(0, 100); sl.setValue(self.alpha)
        sl.setFixedWidth(160)
        lbl = QLabel(f"{self.alpha}%"); lbl.setFixedWidth(40)
        bl.addWidget(sl); bl.addWidget(lbl)

        def _set(v):  #vers 1
            lbl.setText(f"{v}%")
            self.alpha = v
            self._show_alpha()
            self.alphaChanged.emit(self.color_key, v)
        sl.valueChanged.connect(_set)
        sl.setEnabled(self.alpha_on)
        wa = QWidgetAction(menu); wa.setDefaultWidget(box); menu.addAction(wa)
        menu.exec(self.alpha_btn.mapToGlobal(self.alpha_btn.rect().bottomLeft()))

    def dragEnterEvent(self, event): #vers 1
        """Accept colour drags from other swatches."""
        if event.mimeData().hasText() and event.mimeData().text().startswith('#'):
            if not self.is_locked:
                event.acceptProposedAction()
                # Highlight to show we're a valid drop target
                self.setStyleSheet("QWidget { border: 2px solid palette(highlight); }")
        else:
            event.ignore()

    def dragLeaveEvent(self, event): #vers 1
        """Remove drop target highlight."""
        self.setStyleSheet("")

    def dropEvent(self, event): #vers 1
        """Receive a dragged colour and apply it."""
        self.setStyleSheet("")
        if self.is_locked:
            event.ignore()
            return
        hex_color = event.mimeData().text()
        if hex_color.startswith('#') and len(hex_color) == 7:
            self.set_color(hex_color)
            self.colorChanged.emit(self.color_key, hex_color)
            event.acceptProposedAction()

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

    def on_color_changed(self, text): #vers 2
        """Handle color input text change"""
        if text.startswith('#') and len(text) == 7:
            self.current_value = text
            self.update_preview(text)
            if self.alpha is not None:
                self._show_alpha()
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

# Panel effect, image and transparency keys kept in theme and settings JSON
THEME_EFFECT_KEYS = ("color_alpha", "color_alpha_off", "panel_fill_dir", "panel_grad_dir", "panel_pattern_style",
                     "panel_pattern_scale", "panel_effect_type",
                     "panel_bg_image", "panel_bg_image_mode", "panel_bg_image_opacity",
                     "panel_bg_image_all", "titlebar_opacity", "panel_opacity",
                     "button_opacity", "widget_opacity", "tabs_opacity", "splitter_opacity",
                     "see_through", "viewport_solid", "image_titlebar", "button_style", "progressbar_style", "progressbar_height")

_IMAGE_DIRS = []      # folders searched for relative panel image paths

# Background colours that take a transparency value (Colors tab, 0-100)
ALPHA_COLOR_KEYS = ("bg_primary", "bg_secondary", "bg_tertiary", "panel_bg", "toolbar_bg",
                    "ribbon_bg", "button_normal", "button_text_color", "titlebar_bg", "gadgetbar_bg",
                    "menu_bg", "selection_background", "table_row_odd", "table_row_even",
                    "alternate_row", "panel_entries", "splitter_color_background",
                    "scrollbar_background", "dialog_bg", "viewport_bg", "hero_bg", "window_bg",
                    "base", "alternate_base", "panel_primary", "button_text_hover",
                    "button_text_pressed")
# Drawn in code (palette, painters), not only the stylesheet
CODE_ALPHA_KEYS = ("viewport_bg", "hero_bg", "window_bg", "base", "alternate_base",
                   "panel_primary", "button_text_hover", "button_text_pressed")

BLANK_TILE = str(Path(__file__).resolve().parents[1] / 'images' / 'blanktile.png')
_compositor = None


def compositor_running() -> bool: #vers 1
    """Desktop compositor present, so translucent windows show the desktop."""
    global _compositor
    if _compositor is None:
        from PyQt6.QtGui import QGuiApplication
        if QGuiApplication.platformName() != 'xcb':      # Wayland, Windows, macOS always composite
            _compositor = True
        else:                                           # X11: _NET_WM_CM_Sn selection has an owner
            import ctypes
            import ctypes.util
            x11 = ctypes.cdll.LoadLibrary(ctypes.util.find_library('X11') or 'libX11.so.6')
            x11.XOpenDisplay.restype = ctypes.c_void_p
            x11.XOpenDisplay.argtypes = [ctypes.c_char_p]
            x11.XDefaultScreen.argtypes = [ctypes.c_void_p]
            x11.XInternAtom.restype = ctypes.c_ulong
            x11.XInternAtom.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_int]
            x11.XGetSelectionOwner.restype = ctypes.c_ulong
            x11.XGetSelectionOwner.argtypes = [ctypes.c_void_p, ctypes.c_ulong]
            x11.XCloseDisplay.argtypes = [ctypes.c_void_p]
            dpy = x11.XOpenDisplay(None)
            atom = x11.XInternAtom(dpy, f"_NET_WM_CM_S{x11.XDefaultScreen(dpy)}".encode(), 0)
            _compositor = bool(x11.XGetSelectionOwner(dpy, atom))
            x11.XCloseDisplay(dpy)
    return _compositor


def panel_image(cs) -> str: #vers 1
    """Panel image; see-through without a compositor uses the blank tile."""
    img = cs.get('panel_bg_image', '')
    if not img and cs.get('see_through') and not compositor_running():
        return BLANK_TILE
    return img


def see_through_on(cs) -> bool: #vers 1
    """Desktop shows through (see-through on, no image, compositor running)."""
    return bool(cs.get('see_through')) and not panel_image(cs)


def image_all_on(cs) -> bool: #vers 1
    """Image behind lists, ribbons and tabs too (always for the blank tile)."""
    return bool(panel_image(cs) and (cs.get('panel_bg_image_all') or not cs.get('panel_bg_image')))


def parse_color(value): #vers 1
    """QColor from '#rrggbb', a name or 'rgba(r, g, b, a)'."""
    from PyQt6.QtGui import QColor
    if isinstance(value, str) and value.startswith('rgba('):
        r, g, b, a = (int(float(v)) for v in value[5:-1].split(','))
        return QColor(r, g, b, a)
    return QColor(value)


def color_alpha(cs, key): #vers 1
    """Transparency 0-100 set and switched on for key, else None."""
    a = (cs.get('color_alpha') or {}).get(key)
    if a is None or a >= 100 or key not in ALPHA_COLOR_KEYS or key in cs.get('color_alpha_off', []):
        return None
    return max(0, int(a))


def color_css(cs, key, value: str) -> str: #vers 1
    """'#rrggbb' as rgba() when key has transparency on."""
    a = color_alpha(cs, key)
    if a is None or not (isinstance(value, str) and value.startswith('#') and len(value) == 7):
        return value
    r, g, b = (int(value[i:i + 2], 16) for i in (1, 3, 5))
    return f"rgba({r}, {g}, {b}, {round(a * 2.55)})"


def apply_color_alpha(colors, alpha, off=()): #vers 2
    """Copy of colors with transparent keys as rgba(); alpha = {key: 0-100}; off keys stay solid."""
    out = dict(colors)
    for key, a in (alpha or {}).items():
        val = out.get(key)
        if key in ALPHA_COLOR_KEYS and key not in off and a < 100 and isinstance(val, str) \
                and val.startswith('#') and len(val) == 7:
            r, g, b = (int(val[i:i + 2], 16) for i in (1, 3, 5))
            out[key] = f"rgba({r}, {g}, {b}, {round(max(0, a) * 2.55)})"
    return out


def resolve_panel_image(path): #vers 1
    """Absolute path of a panel image; relative paths look in settings/themes folders."""
    if not path or os.path.isabs(path):
        return path
    for d in _IMAGE_DIRS:
        cand = os.path.join(str(d), path)
        if os.path.isfile(cand):
            return cand
    return path


def store_panel_image(path, folder): #vers 1
    """Copy a panel image into folder/images; return 'images/<name>' (or '' when none)."""
    import shutil
    src = resolve_panel_image(path)
    if not src or not os.path.isfile(src):
        return path or ''
    dst_dir = Path(folder) / 'images'
    dst_dir.mkdir(parents=True, exist_ok=True)
    dst = dst_dir / os.path.basename(src)
    if Path(src).resolve() != dst.resolve():
        shutil.copy2(src, dst)
    return f"images/{dst.name}"


class AppSettings:
    def __init__(self, settings_file="appfactory.settings.json"): #vers 7
        """Initialize application settings with Windows compatibility"""
        current_file_dir = Path(__file__).parent

        # FIXED: Set paths based on where we are - Windows compatible
        if current_file_dir.name == "utils":
            self.themes_dir = current_file_dir.parent / "themes"
            self.settings_file = current_file_dir.parent / settings_file
        else:
            self.themes_dir = current_file_dir / "themes"
            self.settings_file = current_file_dir / settings_file

        # Exe build: settings/ beside the exe; bundled file is the default.
        # Self-contained here as this master file is synced to all tool repos.
        import sys
        if getattr(sys, 'frozen', False):
            import shutil
            portable_dir = Path(sys.executable).resolve().parent / 'settings'
            portable_dir.mkdir(parents=True, exist_ok=True)
            portable = portable_dir / settings_file
            if not portable.exists() and self.settings_file.exists():
                shutil.copyfile(self.settings_file, portable)
                bundled_imgs = self.settings_file.parent / 'images'
                if bundled_imgs.is_dir():
                    shutil.copytree(bundled_imgs, portable_dir / 'images', dirs_exist_ok=True)
            self.settings_file = portable
        _IMAGE_DIRS[:] = [self.settings_file.parent, self.themes_dir]

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
            'titlebar_button_height': 25,
            'titlebar_button_size':   25,
            'titlebar_icon_size':     20,
            'icon_size': 16,
            'button_format': 'both',  # 'both', 'icon_only', 'text_only', 'separate'
            # Panel / hero colours
            'hero_bg':                   '',         # direct override; empty = auto from gradient
            'hero_gradient_dark_start':  '#1a1a2e',
            'hero_gradient_dark_end':    '#2d2d5e',
            'hero_gradient_light_start': '#1a1a2e',
            'hero_gradient_light_end':   '#2d2d5e',
            'window_bg':                 '#2b2b2b',
            'window_text':               '#ffffff',
            'base':                      '#2b2b2b',
            'alternate_base':            '#3c3c3c',
            'tooltip_bg':                '#3c3c3c',
            'tooltip_text':              '#ffffff',
            'placeholder_text':          '#aaaaaa',
            'disabled_text':             '#777777',
            'panel_fill_a':              '#1a1a2e',
            'panel_fill_b':              '#16213e',
            'panel_fill_dir':            0,
            'panel_grad_dir':            1,
            'panel_grad_stop1':          '#1a1a2e',
            'panel_grad_stop2':          '#2d1b4e',
            'panel_grad_stop3':          '#16213e',
            'panel_pattern_style':       0,
            'panel_pattern_scale':       8,
            'panel_pattern_light':       '#2a2a4a',
            'panel_pattern_dark':        '#141428',
            'copper_light':              '#b87333',
            'copper_dark':               '#7a4a1a',
            'panel_bg_image':            '',
            'panel_bg_image_mode':       5,           # across window: one even image
            'panel_bg_image_opacity':    100,
            'panel_bg_image_all':        False,
            'see_through':               False,       # no image: desktop shows through
            'viewport_solid':            True,        # viewports / canvases never bleed through
            'image_titlebar':            0,           # 0 panels + titlebar, 1 panels only, 2 titlebar only
            'button_style':              'flat',
            'progressbar_style':         'system',
            'progressbar_fill':          '#4a7a9b',
            'progressbar_bg':            '#1a1a2e',
            'progressbar_text':          '#ffffff',
            'progressbar_stripe':        '#5a9abf',
            'progressbar_height':        18,
            'panel_effect_type':          'none',  # none|fill|gradient|pattern
            'locale_auto_detect':        True,
            'language':                  'en_GB',
            'date_format_idx':           0,
            'number_format_idx':         0,
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


    def _generate_stylesheet(self, colors): #vers 7
        """Generate stylesheet from colors dict - shared by both classes.
        Colour transparency (current_settings color_alpha) becomes rgba()."""
        if not colors:
            return ""
        colors = dict(colors)
        # Fallback colours resolved first so their own transparency applies
        _bg2 = colors.get('bg_secondary', '#f5f5f5')
        colors.setdefault('menu_bg', _bg2)
        colors['scrollbar_background'] = colors.get('scrollbar_background') or _bg2
        colors['dialog_bg'] = colors.get('dialog_bg') or colors.get('bg_primary', '#ffffff')
        colors['toolbar_bg'] = colors.get('toolbar_bg') or _bg2
        colors['ribbon_bg'] = colors.get('ribbon_bg') or colors['toolbar_bg']
        colors['gadgetbar_bg'] = colors.get('gadgetbar_bg') or colors['toolbar_bg']
        _alpha = dict(getattr(self, 'current_settings', {}).get('color_alpha', {}))
        _off = set(getattr(self, 'current_settings', {}).get('color_alpha_off', []))
        if 'titlebar_bg' in _alpha:                       # title bar is drawn in gadgetbar_bg
            _alpha['gadgetbar_bg'] = _alpha['titlebar_bg']
        if 'splitter_color_background' in _alpha and colors.get('handle_color'):
            colors['splitter_color_background'] = colors['handle_color']
            colors.pop('handle_color')
        colors = apply_color_alpha(colors, _alpha, _off)

        # Extract all colors
        bg_primary = colors.get('bg_primary', '#ffffff')
        bg_secondary = colors.get('bg_secondary', '#f5f5f5')
        bg_tertiary = colors.get('bg_tertiary', '#e9ecef')
        panel_bg = colors.get('panel_bg', colors.get('bg_secondary', '#f0f0f0'))
        panel_primary = colors.get('panel_primary', panel_bg)  # card surface, falls back to panel_bg
        text_primary = colors.get('text_primary', '#000000')
        text_secondary = colors.get('text_secondary', '#666666')
        text_accent = colors.get('text_accent', '#0066cc')
        accent_primary = colors.get('accent_primary', '#0078d4')
        accent_secondary = colors.get('accent_secondary', '#0A7Ad4')
        border = colors.get('border', '#cccccc')

        # Handle style — 4 types: line, gradient, dots, invisible
        cs = getattr(self, 'current_settings', {})
        _hstyle = colors.get('handle_style', cs.get('handle_style', 'line'))
        _hcol   = colors.get('handle_color',
                    colors.get('splitter_color_background',
                        cs.get('handle_color', border)))
        _hsize  = colors.get('handle_size',   cs.get('handle_size',   '4'))
        _hide_docked = cs.get('handle_hide_docked', False)
        if _hide_docked:
            _hstyle = 'invisible'
        if _hstyle == 'gradient':
            handle_h_css = f"background: qlineargradient(x1:0,y1:0,x2:1,y2:0,stop:0 transparent,stop:0.5 {_hcol},stop:1 transparent); width:{_hsize}px;"
            handle_v_css = f"background: qlineargradient(x1:0,y1:0,x2:0,y2:1,stop:0 transparent,stop:0.5 {_hcol},stop:1 transparent); height:{_hsize}px;"
        elif _hstyle == 'dots':
            handle_h_css = f"background:{_hcol}; width:{_hsize}px; border-left:2px dotted {accent_primary}; border-right:2px dotted {accent_primary};"
            handle_v_css = f"background:{_hcol}; height:{_hsize}px; border-top:2px dotted {accent_primary}; border-bottom:2px dotted {accent_primary};"
        elif _hstyle == 'invisible':
            handle_h_css = f"background:transparent; width:{_hsize}px;"
            handle_v_css = f"background:transparent; height:{_hsize}px;"
        else:  # line (default)
            handle_h_css = f"background-color:{_hcol}; width:{_hsize}px;"
            handle_v_css = f"background-color:{_hcol}; height:{_hsize}px;"

        # Table/List alternating rows - use table_row_odd or alternate_row as fallback
        alternate_row = colors.get('table_row_odd', colors.get('alternate_row', '#f5f5f5'))
        table_row_even = colors.get('table_row_even', bg_primary)
        table_row_odd = colors.get('table_row_odd', bg_secondary)

        button_normal = colors.get('button_normal', '#e0e0e0')
        button_hover = colors.get('button_hover', '#d0d0d0')
        button_pressed = colors.get('button_pressed', '#b0b0b0')
        button_text   = colors.get('button_text_color',   text_primary)
        button_text_h = colors.get('button_text_hover',   button_text)
        button_text_p = colors.get('button_text_pressed', button_text)
        selection_bg   = colors.get('selection_background', '#0078d4')
        selection_text = colors.get('selection_text', '#ffffff')
        menu_hl_bg     = colors.get('menu_highlight_bg',   selection_bg)
        menu_hl_text   = colors.get('menu_highlight_text', selection_text)
        grid = colors.get('grid', '#e0e0e0')

        # System ui - overrides
        # Derive sensible defaults from bg_primary so light themes stay light
        _QC = parse_color                               # rgba() from colour transparency
        _is_light = _QC(bg_primary).lightness() > 128
        window_bg = colors.get('window_bg') or bg_primary
        # If saved window_bg is dark but theme is light, override with bg_primary
        if _is_light and _QC(window_bg).lightness() < 64:
            window_bg = bg_primary
        window_text = colors.get('window_text') or text_primary
        base = colors.get('base') or bg_primary
        if _is_light and _QC(base).lightness() < 64:
            base = bg_primary
        alternate_base = colors.get('alternate_base') or bg_secondary
        if _is_light and _QC(alternate_base).lightness() < 64:
            alternate_base = bg_secondary
        tooltip_bg = colors.get('tooltip_bg') or bg_secondary
        tooltip_text = colors.get('tooltip_text') or text_primary
        placeholder_text = colors.get('placeholder_text', '#888888' if _is_light else '#aaaaaa')
        disabled_text = colors.get('disabled_text', '#999999' if _is_light else '#777777')

        # Additional colors
        success = colors.get('success', '#4caf50')
        warning = colors.get('warning', '#ff9800')
        error = colors.get('error', '#f44336')
        toolbar_bg = colors.get('toolbar_bg', bg_secondary)
        ribbon_bg = colors.get('ribbon_bg', toolbar_bg)
        gadgetbar_bg   = colors.get('gadgetbar_bg',   toolbar_bg)
        gadgetbar_text = colors.get('gadgetbar_text',  text_primary)
        panel_entries = colors.get('panel_entries', bg_tertiary)
        panel_filter = colors.get('panel_filter', bg_tertiary)

        # Dialog colours fall back to bg_primary/text_primary if not explicitly set
        _d_bg   = colors.get('dialog_bg',   '')
        _d_text = colors.get('dialog_text', '')
        dialog_bg   = _d_bg   if _d_bg   else bg_primary
        dialog_text = _d_text if _d_text else text_primary

        stylesheet = f"""
        QMainWindow {{
            background-color: {bg_primary};
            color: {text_primary};
        }}

        QDialog {{
            background-color: {dialog_bg};
            color: {dialog_text};
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
            color: {text_primary};
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
            color: {button_text};
            border: 1px solid {border};
            border-radius: 4px;
            padding: 6px 12px;
            color: {text_primary};
        }}

        QPushButton:hover {{
            background-color: {button_hover};
            color: {button_text_h};
        }}

        QPushButton:pressed {{
            background-color: {button_pressed};
            color: {button_text_p};
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
            margin-top: 2px;
        }}

        QTabBar::tab {{
            background-color: {bg_secondary};
            border: 1px solid {border};
            border-bottom: 3px solid transparent;
            padding: 6px 16px;
            color: {text_secondary};
            margin-bottom: 0px;
            margin-right: 2px;
        }}

        QTabBar::tab:selected {{
            background-color: {bg_primary};
            color: {text_primary};
            border-bottom: 3px solid {accent_primary};
            font-weight: bold;
        }}

        QTabBar::tab:hover:!selected {{
            background-color: {button_hover};
            color: {text_primary};
            border-bottom: 3px solid {accent_secondary};
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
            background-color: transparent;
        }}

        QLineEdit {{
            color: {text_primary};
            background-color: {bg_secondary};
            border: 1px solid {border};
            border-radius: 3px;
            padding: 2px 4px;
            selection-background-color: {selection_bg};
            selection-color: {selection_text};
        }}

        QTextEdit, QPlainTextEdit {{
            color: {text_primary};
            background-color: {bg_secondary};
            border: 1px solid {border};
            selection-background-color: {selection_bg};
            selection-color: {selection_text};
        }}

        QListWidget, QTreeWidget {{
            color: {text_primary};
            background-color: {bg_secondary};
            alternate-background-color: {table_row_odd};
            border: 1px solid {border};
            outline: none;
        }}

        QListWidget::item, QTreeWidget::item {{
            color: {text_primary};
        }}

        QListWidget::item:selected, QTreeWidget::item:selected {{
            color: {selection_text};
            background-color: {selection_bg};
        }}

        QListWidget::item:hover, QTreeWidget::item:hover {{
            background-color: {bg_tertiary};
        }}

        QTabWidget::pane {{
            border: 1px solid {border};
            background-color: {bg_primary};
        }}

        QTabBar::tab {{
            color: {text_primary};
            background-color: {bg_secondary};
            border: 1px solid {border};
            padding: 4px 8px;
        }}

        QTabBar::tab:selected {{
            color: {text_primary};
            background-color: {bg_primary};
            border-bottom: 2px solid {accent_primary};
        }}

        QTabBar::tab:hover {{
            background-color: {bg_tertiary};
        }}

        QHeaderView::section {{
            color: {text_primary};
            background-color: {bg_secondary};
            border: 1px solid {border};
            padding: 4px;
        }}

        QMenu {{
            background-color: {colors.get('menu_bg', bg_secondary)};
            color: {_readable_text(colors.get('menu_bg', bg_secondary), text_primary)};
            border: 1px solid {border};
        }}
        QMenu::item {{
            padding: 4px 20px 4px 20px;
        }}
        QMenu::item:selected {{
            background-color: {menu_hl_bg};
            color: {menu_hl_text};
        }}
        QMenu::separator {{
            height: 1px;
            background: {border};
            margin: 2px 4px;
        }}

        QToolBar {{
            background-color: {ribbon_bg};
            border: 1px solid {border};
            spacing: 3px;
        }}
        QFrame#titlebar {{
            background-color: {gadgetbar_bg};
            color: {gadgetbar_text};
            border: none;
        }}

        QStatusBar {{
            background-color: {bg_secondary};
            color: {text_secondary};
        }}

        QScrollBar:vertical {{
            background-color: {colors['scrollbar_background']};
            width: 12px;
            border: none;
            margin: 0px;
        }}
        QScrollBar::handle:vertical {{
            background-color: {border};
            min-height: 30px;
            border-radius: 6px;
            margin: 2px;
        }}
        QScrollBar::handle:vertical:hover {{
            background-color: {button_hover};
        }}
        QScrollBar::handle:vertical:pressed {{
            background-color: {accent_primary};
        }}
        QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
            height: 0px;
        }}
        QScrollBar::add-page:vertical, QScrollBar::sub-page:vertical {{
            background: none;
        }}

        QScrollBar:horizontal {{
            background-color: {colors['scrollbar_background']};
            height: 12px;
            border: none;
            margin: 0px;
        }}
        QScrollBar::handle:horizontal {{
            background-color: {border};
            min-width: 30px;
            border-radius: 6px;
            margin: 2px;
        }}
        QScrollBar::handle:horizontal:hover {{
            background-color: {button_hover};
        }}
        QScrollBar::handle:horizontal:pressed {{
            background-color: {accent_primary};
        }}
        QScrollBar::add-line:horizontal, QScrollBar::sub-line:horizontal {{
            width: 0px;
        }}
        QScrollBar::add-page:horizontal, QScrollBar::sub-page:horizontal {{
            background: none;
        }}

        QSplitter::handle:horizontal {{
            {handle_h_css}
        }}
        QSplitter::handle:horizontal:hover {{
            background-color: {accent_primary};
        }}
        QSplitter::handle:vertical {{
            {handle_v_css}
        }}
        QSplitter::handle:vertical:hover {{
            background-color: {accent_primary};
        }}
        """

        #   Button style
        cs = getattr(self, 'current_settings', {})
        btn_style  = cs.get('button_style', 'flat')
        btn_normal = colors.get('button_normal', '#e0e0e0')
        btn_hover  = colors.get('button_hover',  '#d0d0d0')
        btn_press  = colors.get('button_pressed','#b0b0b0')
        from PyQt6.QtGui import QColor as _QC
        _c = _QC(btn_normal)
        _light = _c.lighter(140).name()
        _dark  = _c.darker(130).name()

        _btn_bg = {
            'gradient_h':  f'background: qlineargradient(x1:0,y1:0,x2:1,y2:0,stop:0 {_light},stop:1 {_dark});',
            'gradient_v':  f'background: qlineargradient(x1:0,y1:0,x2:0,y2:1,stop:0 {_light},stop:1 {_dark});',
            'gradient_45': f'background: qlineargradient(x1:0,y1:0,x2:1,y2:1,stop:0 {_light},stop:0.5 {btn_normal},stop:1 {_dark});',
            'banded':      f'background: qlineargradient(x1:0,y1:0,x2:0,y2:1,stop:0 {_light},stop:0.25 white,stop:0.30 {_light},stop:0.31 {btn_normal},stop:1 {_dark});',
            'bump':        f'background: qlineargradient(x1:0,y1:0,x2:0,y2:1,stop:0 {_light},stop:1 {_dark});',
            'amiga_wb':    f'background: qlineargradient(x1:0,y1:0,x2:0,y2:1,stop:0 white,stop:0.15 {_light},stop:0.85 {_dark},stop:1 black);',
            'half_shine':  f'background: qlineargradient(x1:0,y1:0,x2:0,y2:1,stop:0 white,stop:0.48 {_light},stop:0.49 {btn_normal},stop:1 {btn_normal});',
        }.get(btn_style, f'background-color: {btn_normal};')

        if btn_style not in ('flat', 'zen', 'indented', 'shadow_dark'):
            stylesheet += f"""
        QPushButton:!flat {{
            {_btn_bg}
        }}
        """

        #   Progress bar
        pb_style = cs.get('progressbar_style', 'system')
        if pb_style != 'system':
            pb_fill  = cs.get('progressbar_fill',   '#4a7a9b')
            pb_bg    = cs.get('progressbar_bg',      bg_primary)
            pb_text  = cs.get('progressbar_text',    text_primary)
            pb_h     = cs.get('progressbar_height',  18)
            _pf      = _QC(pb_fill)
            pb_light = _pf.lighter(140).name()
            pb_dark  = _pf.darker(130).name()
            _pb_chunk = {
                'flat':       f'background-color: {pb_fill};',
                'gradient_h': f'background: qlineargradient(x1:0,y1:0,x2:1,y2:0,stop:0 {pb_light},stop:1 {pb_dark});',
                'gradient_v': f'background: qlineargradient(x1:0,y1:0,x2:0,y2:1,stop:0 {pb_light},stop:1 {pb_dark});',
                'banded':     f'background: qlineargradient(x1:0,y1:0,x2:0,y2:1,stop:0 {pb_light},stop:0.3 white,stop:0.31 {pb_fill},stop:1 {pb_dark});',
                'glow':       f'background: qlineargradient(x1:0,y1:0,x2:0,y2:1,stop:0 {pb_dark},stop:0.5 {pb_light},stop:1 {pb_dark});',
            }.get(pb_style, f'background-color: {pb_fill};')
            stylesheet += f"""
        QProgressBar {{
            background-color: {pb_bg};
            border: 1px solid {border};
            border-radius: 3px;
            color: {pb_text};
            text-align: center;
            min-height: {pb_h}px;
            max-height: {pb_h}px;
        }}
        QProgressBar::chunk {{
            {_pb_chunk}
            border-radius: 2px;
        }}
        """

        from apps.methods.ui_sizes import size_stylesheet
        stylesheet += size_stylesheet()                 # App Settings > Sizes
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


    def save_settings(self): #vers 2
        """Save current settings to file; panel image copied beside it (images/)."""
        try:
            self.settings_file.parent.mkdir(parents=True, exist_ok=True)  # ADD THIS LINE
            if self.current_settings.get('panel_bg_image'):
                self.current_settings['panel_bg_image'] = store_panel_image(
                    self.current_settings['panel_bg_image'], self.settings_file.parent)
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
            "last_img_output_path": "",
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
             # Pin warning settings
            "pin_warn_popup": True,
            "pin_warn_log": True,
            # Rename notification settings
            "rename_notify_popup": True,
            "rename_notify_log": True,
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
            "last_img_output_path": "",
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
        from apps.methods.ui_sizes import set_overrides
        set_overrides(self.current_settings.get('ui_sizes', {}))   # global height rules


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

    def apply_theme_effects(self, theme_key): #vers 3
        """Copy a theme's panel effect, image and transparency keys into current settings."""
        theme = self.themes.get(theme_key, {})
        for key in THEME_EFFECT_KEYS:
            if key in theme:
                self.current_settings[key] = theme[key]
        self.current_settings["color_alpha"] = dict(theme.get("color_alpha", {}))
        self.current_settings["color_alpha_off"] = list(theme.get("color_alpha_off", []))

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
                'menu_highlight_bg': '#0188c4',
                'menu_highlight_text': '#ffffff',
                'button_text_color': '#000000',
                'button_text_hover': '#000000',
                'button_text_pressed': '#000000',
                'splitter_color_background': '#cccccc',
                'splitter_color_shine': '#ffffff',
                'splitter_color_shadow': '#aaaaaa',
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
                'toolbar_bg': '#fafafa',
                'viewport_bg': '#ffffff',
                'viewport_text': '#808080',
                'titlebar_bg': '#f0f0f0',
                'titlebar_text': '#000000',
                'gadgetbar_bg': '#f0f0f0',
                'gadgetbar_text': '#000000',
                'handle_style': 'line',
                'handle_color': '#cccccc',
                'handle_size': '4',
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
            'menu_highlight_bg': '#0078d4',
            'menu_highlight_text': '#ffffff',
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
            'toolbar_bg': '#fafafa',
            'viewport_bg': '#ffffff',
            'viewport_text': '#808080',
            'titlebar_bg': '#f0f0f0',
            'titlebar_text': '#000000',
            'gadgetbar_bg': '#f0f0f0',
            'gadgetbar_text': '#000000',
            'handle_style': 'line',
            'handle_color': '#cccccc',
            'handle_size': '4',
        }


    def get_ui_color(self, key: str): #vers 2
        """Return a QColor for a theme key. Use this instead of hardcoding QColor values.
        Keys: viewport_bg, viewport_text, bg_primary, bg_secondary, text_primary,
              text_secondary, accent_primary, border, error, success, warning, etc.
        Falls back gracefully if app_settings or theme key is missing."""
        from PyQt6.QtGui import QColor
        try:
            colors = self.get_theme_colors()
            hex_val = colors.get(key)
            if hex_val:
                col = QColor(hex_val)
                a = color_alpha(self.current_settings, key) if key in CODE_ALPHA_KEYS else None
                if a is not None:                       # Colors tab transparency
                    col.setAlphaF(a / 100.0)
                return col
        except Exception:
            pass
        # Safe fallbacks for the most critical keys
        fallbacks = {
            'viewport_bg':   QColor(255, 255, 255),
            'viewport_text': QColor(128, 128, 128),
            'bg_primary':    QColor(255, 255, 255),
            'bg_secondary':  QColor(240, 240, 240),
            'text_primary':  QColor(0, 0, 0),
            'text_secondary':QColor(80, 80, 80),
            'accent_primary':QColor(0, 120, 212),
            'border':        QColor(200, 200, 200),
            'error':         QColor(244, 67, 54),
            'success':       QColor(76, 175, 80),
            'warning':       QColor(255, 152, 0),
            'border':        QColor(200, 200, 200),
            'titlebar_bg':   QColor(240, 240, 240),
            'titlebar_text': QColor(0, 0, 0),
            'gadgetbar_bg':  QColor(240, 240, 240),
            'gadgetbar_text':QColor(0, 0, 0),
        }
        return fallbacks.get(key, QColor(128, 128, 128))


    def get_stylesheet(self): #vers 5
        """Generate complete stylesheet for current theme; app palette follows it."""
        colors = self.get_theme_colors()
        self._apply_app_palette(colors)
        return self._generate_stylesheet(colors)

    def _apply_app_palette(self, colors): #vers 2
        """App QPalette from the theme, so palette(mid) / palette(base) ... in
        widget stylesheets and painters match it. Only set when it changes."""
        from PyQt6.QtGui import QColor, QPalette
        from PyQt6.QtWidgets import QApplication
        app = QApplication.instance()
        if app is None or not colors:
            return
        cs = getattr(self, 'current_settings', {})
        def c(*keys):
            for k in keys:
                v = colors.get(k)
                if isinstance(v, str) and v.startswith('#'):
                    col = QColor(v)
                    a = color_alpha(cs, k) if k in CODE_ALPHA_KEYS else None
                    if a is not None:                   # window / base transparency
                        col.setAlphaF(a / 100.0)
                    return col
            return None
        R = QPalette.ColorRole
        roles = {R.Window: c('window_bg', 'bg_primary'), R.WindowText: c('window_text', 'text_primary'),
                 R.Base: c('base', 'bg_secondary'), R.AlternateBase: c('alternate_base', 'bg_tertiary'),
                 R.Text: c('text_primary'), R.Button: c('button_normal', 'bg_secondary'),
                 R.ButtonText: c('button_text_color', 'text_primary'),
                 R.Highlight: c('selection_background', 'accent_primary'),
                 R.HighlightedText: c('selection_text', 'text_primary'),
                 R.Mid: c('border'), R.Dark: c('border'), R.Light: c('bg_tertiary', 'bg_secondary'),
                 R.Midlight: c('bg_tertiary', 'bg_secondary'), R.Shadow: c('border'),
                 R.PlaceholderText: c('placeholder_text', 'text_secondary'),
                 R.ToolTipBase: c('tooltip_bg', 'bg_secondary'), R.ToolTipText: c('tooltip_text', 'text_primary'),
                 R.Link: c('accent_primary'), R.BrightText: c('text_accent', 'text_primary')}
        key = tuple((r.value, q.name(QColor.NameFormat.HexArgb)) for r, q in roles.items() if q is not None)
        if key == getattr(self, '_palette_key', None):
            return
        self._palette_key = key
        pal = app.palette()
        for role, col in roles.items():
            if col is not None:
                pal.setColor(role, col)
        disabled = c('disabled_text')
        if disabled is not None:
            for role in (R.WindowText, R.Text, R.ButtonText):
                pal.setColor(QPalette.ColorGroup.Disabled, role, disabled)
        app.setPalette(pal)


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



class AppPanelEffect: #vers 2
    """Mixin installed on QWidget panels to draw fill/gradient/pattern
    effects from app_settings. Install with AppPanelEffect.install(widget, settings)."""

    _filter = None
    _alive = {}                                          # id -> widget, wrappers kept alive

    @staticmethod
    def install(widget, app_settings, effect_type="auto"): #vers 3
        """Hook panel effect painting onto widget via a shared event filter.
        Works on widgets that already painted (instance paintEvent patches did not)."""
        key = id(widget)
        if key not in AppPanelEffect._alive:                 # keep wrapper so attrs persist
            AppPanelEffect._alive[key] = widget
            widget.destroyed.connect(lambda _=None, k=key: AppPanelEffect._alive.pop(k, None))
        widget._app_settings_ref = app_settings
        widget._panel_effect_type = effect_type
        if getattr(widget, '_panel_effect_installed', False):
            return
        if AppPanelEffect._filter is None:
            from PyQt6.QtCore import QObject, QEvent
            from PyQt6.QtWidgets import QApplication

            class _PanelEffectFilter(QObject): #vers 1
                """Paint the widget normally, then the panel effect on top."""
                def eventFilter(self, obj, ev): #vers 2
                    if ev.type() != QEvent.Type.Paint:
                        return False
                    if getattr(obj, '_panel_under_tint', None) is not None:
                        AppPanelEffect._draw_under(obj)       # image + tint, content paints on top
                        return False
                    if getattr(obj, '_panel_effect_installed', False):
                        type(obj).paintEvent(obj, ev)
                        AppPanelEffect._draw_effect(obj, ev)
                        return True
                    return False

            AppPanelEffect._filter = _PanelEffectFilter(QApplication.instance())
        widget.installEventFilter(AppPanelEffect._filter)
        widget._panel_effect_installed = True

    @staticmethod
    def install_under(widget, styled, app_settings): #vers 3
        """Image behind a list/toolbar/tab page: widget made transparent, its own
        colour laid over the image as a tint. styled = widget carrying the stylesheet."""
        from PyQt6.QtGui import QColor
        if getattr(widget, '_panel_under_tint', None) is None:
            role = widget.backgroundRole()
            widget._panel_under_tint = QColor(widget.palette().color(role))
            widget._panel_under_styled = styled
            styled._panel_under_ss = styled.styleSheet()
            styled.setProperty('panelUnder', True)          # selector hits this widget only
            own = styled._panel_under_ss
            if own.strip() and '{' not in own:              # bare declarations: give them a selector
                own = '*[panelUnder="true"] { ' + own + ' }'
            styled.setStyleSheet(own + '\n*[panelUnder="true"] { background: transparent; }')
            widget.setAutoFillBackground(False)
            AppPanelEffect.install(widget, app_settings)
            widget._panel_effect_installed = False      # under mode only
        widget._app_settings_ref = app_settings
        widget.update()

    @staticmethod
    def remove_under(widget): #vers 2
        """Undo install_under: original stylesheet back, no image."""
        styled = getattr(widget, '_panel_under_styled', None)
        if styled is not None:
            styled.setProperty('panelUnder', False)
            styled.setStyleSheet(getattr(styled, '_panel_under_ss', ''))
        widget._panel_under_tint = None
        widget.update()

    @staticmethod
    def _draw_under(widget): #vers 5
        """Image then the widget's colour as a tint at Transparency opacity;
        see-through (no image): the tint alone, desktop shows behind."""
        from PyQt6.QtGui import QPainter, QColor
        from PyQt6.QtWidgets import QToolBar, QTabBar, QSplitterHandle, QStackedWidget
        cs = getattr(widget, '_app_settings_ref', None) or {}
        if not isinstance(cs, dict):
            cs = getattr(cs, 'current_settings', {})
        if not widget.isVisible() or widget.width() <= 0 or widget.height() <= 0:
            return
        p = QPainter()
        if not p.begin(widget):
            return
        try:
            r = widget.rect()
            tint = QColor(widget._panel_under_tint)
            key = ('titlebar_opacity' if widget.objectName() == 'titlebar' else
                   'widget_opacity' if isinstance(widget, QToolBar) else
                   'splitter_opacity' if isinstance(widget, QSplitterHandle) else
                   'tabs_opacity' if isinstance(widget, (QTabBar, QStackedWidget)) else
                   'panel_opacity')
            alpha = max(0, min(100, cs.get(key, 100))) / 100.0
            if panel_image(cs):
                p.fillRect(r, tint)
                if AppPanelEffect._paint_image(p, r, cs, widget):
                    tint.setAlphaF(alpha)
                    p.fillRect(r, tint)
            else:                                      # see-through: tint only
                tint.setAlphaF(alpha * tint.alphaF())
                p.fillRect(r, tint)
        finally:
            p.end()

    @staticmethod
    def _draw_effect(widget, event): #vers 3
        """Draw the panel effect on top of the widget background."""
        from PyQt6.QtGui import QPainter, QColor, QLinearGradient, QPen
        from PyQt6.QtCore import QPointF, Qt

        cs = getattr(widget, '_app_settings_ref', None)
        if not cs:
            return
        if not isinstance(cs, dict):
            cs = getattr(cs, 'current_settings', {})

        effect = cs.get('panel_effect_type', 'none')
        image = panel_image(cs)
        if (effect == 'none' or not effect) and not image:
            return

        # Guard against painting a widget that isn't currently paintable -
        # e.g. mid-scroll/resize reflow can transiently leave a widget with
        # zero size or not visible. Qt prints "paintEngine == 0" warnings
        # from its C++ layer the moment QPainter.begin() is attempted on
        # such a widget, before our Python-level check on begin()'s return
        # value even runs - checking these conditions first avoids the
        # warning being printed at all, not just handling the failure
        # after the fact.
        if not widget.isVisible() or widget.width() <= 0 or widget.height() <= 0:
            return

        p = QPainter()
        if not p.begin(widget):
            return   # widget not paintable right now — skip silently
        p.setRenderHint(QPainter.RenderHint.Antialiasing, False)
        r = widget.rect()

        try:
            if effect == 'fill':
                AppPanelEffect._paint_fill(p, r, cs)
            elif effect == 'gradient':
                AppPanelEffect._paint_gradient(p, r, cs)
            elif effect == 'pattern':
                AppPanelEffect._paint_pattern(p, r, cs)
            if image:
                AppPanelEffect._paint_image(p, r, cs, widget)
        except Exception:
            pass
        finally:
            if p.isActive():
                p.end()

    @staticmethod
    def _paint_fill(p, r, cs): #vers 4
        from PyQt6.QtGui import QColor, QLinearGradient
        from PyQt6.QtCore import QPointF

        has_explicit_a = 'panel_fill_a' in cs
        has_explicit_b = 'panel_fill_b' in cs

        # panel_bg lives in theme JSON, not in current_settings -- fetch properly
        panel_bg = cs.get('panel_bg') or cs.get('bg_primary', '')
        if not panel_bg:
            try:
                from PyQt6.QtWidgets import QApplication
                app = QApplication.instance()
                if app:
                    win_col = app.palette().color(app.palette().ColorRole.Window)
                    panel_bg = win_col.name()
            except Exception:
                pass

        # Fallback priority: explicit user setting > theme's own panel/window
        # colour (any lightness, not just light themes) > neutral grey. The
        # old hardcoded '#1a1a2e'/'#16213e' defaults were a fixed navy-purple
        # tint that showed through on any dark theme that hadn't explicitly
        # set panel_fill_a/b, regardless of what that theme's actual colours
        # were - this now matches whatever theme is active instead.
        default_col = QColor(panel_bg) if panel_bg else QColor('#2a2a2e')
        ca = QColor(cs.get('panel_fill_a')) if has_explicit_a else default_col
        cb = QColor(cs.get('panel_fill_b')) if has_explicit_b else default_col

        d = cs.get('panel_fill_dir', 0)
        if d == 0:
            p.fillRect(r, ca)
            return
        dirs = {
            1: (QPointF(r.left(), r.top()),    QPointF(r.right(), r.top())),
            2: (QPointF(r.left(), r.top()),    QPointF(r.left(),  r.bottom())),
            3: (QPointF(r.left(), r.bottom()), QPointF(r.right(), r.top())),
            4: (QPointF(r.right(),r.bottom()), QPointF(r.left(),  r.top())),
            5: (QPointF(r.left(), r.top()),    QPointF(r.right(), r.bottom())),
            6: (QPointF(r.right(),r.top()),    QPointF(r.left(),  r.bottom())),
        }
        s, e = dirs.get(d, dirs[1])
        g = QLinearGradient(s, e)
        g.setColorAt(0, ca); g.setColorAt(1, cb)
        p.fillRect(r, g)

    @staticmethod
    def _paint_gradient(p, r, cs): #vers 4
        from PyQt6.QtGui import QColor, QLinearGradient
        from PyQt6.QtCore import QPointF

        has_s1 = 'panel_grad_stop1' in cs
        has_s2 = 'panel_grad_stop2' in cs
        has_s3 = 'panel_grad_stop3' in cs

        # Fetch theme window color from QApplication palette as fallback
        panel_bg = cs.get('panel_bg') or cs.get('bg_primary', '')
        if not panel_bg:
            try:
                from PyQt6.QtWidgets import QApplication
                app = QApplication.instance()
                if app:
                    win_col = app.palette().color(app.palette().ColorRole.Window)
                    panel_bg = win_col.name()
            except Exception:
                pass

        # Same fallback priority fix as _paint_fill - theme colour at any
        # lightness beats the old hardcoded purple-tinted defaults.
        default_col = QColor(panel_bg) if panel_bg else QColor('#2a2a2e')
        s1 = QColor(cs.get('panel_grad_stop1')) if has_s1 else default_col
        s2 = QColor(cs.get('panel_grad_stop2')) if has_s2 else default_col
        s3 = QColor(cs.get('panel_grad_stop3')) if has_s3 else default_col

        d  = cs.get('panel_grad_dir', 1)
        pts = {
            0: (QPointF(r.left(), r.top()),    QPointF(r.right(), r.top())),
            1: (QPointF(r.left(), r.top()),    QPointF(r.left(),  r.bottom())),
            2: (QPointF(r.left(), r.top()),    QPointF(r.right(), r.bottom())),
            3: (QPointF(r.right(),r.top()),    QPointF(r.left(),  r.bottom())),
            4: (QPointF(r.left(), r.bottom()), QPointF(r.right(), r.top())),
            5: (QPointF(r.right(),r.bottom()), QPointF(r.left(),  r.top())),
        }
        s, e = pts.get(d, pts[1])
        g = QLinearGradient(s, e)
        g.setColorAt(0, s1); g.setColorAt(0.5, s2); g.setColorAt(1, s3)
        p.fillRect(r, g)

    _pixmap_cache = {}

    @staticmethod
    def _paint_image(p, r, cs, widget=None): #vers 5
        """Panel background image (tiled/stretched/centred/fit/fill, or one image
        across the whole window) at blend opacity; True if drawn."""
        from PyQt6.QtGui import QPixmap
        from PyQt6.QtCore import Qt, QPoint
        path = resolve_panel_image(panel_image(cs))
        if not path:
            return False
        px = AppPanelEffect._pixmap_cache.get(path)
        if px is None:
            px = QPixmap(path)
            AppPanelEffect._pixmap_cache[path] = px
        if px.isNull():
            return False
        mode = cs.get('panel_bg_image_mode', 5) if cs.get('panel_bg_image') else 0   # blank tile: tiled
        p.save()
        p.setOpacity(cs.get('panel_bg_image_opacity', 100) / 100.0)
        if mode == 5 and widget is not None:   # Across window: one image, panels show their part
            win = widget.window()
            key = (path, win.width(), win.height())
            sc = AppPanelEffect._pixmap_cache.get(key)
            if sc is None:
                sc = px.scaled(win.size(), Qt.AspectRatioMode.KeepAspectRatioByExpanding,
                               Qt.TransformationMode.SmoothTransformation)
                AppPanelEffect._pixmap_cache = {k: v for k, v in AppPanelEffect._pixmap_cache.items()
                                                if not isinstance(k, tuple)}
                AppPanelEffect._pixmap_cache[key] = sc
            off = widget.mapTo(win, QPoint(0, 0))
            p.setClipRect(r)
            p.drawPixmap(-off.x() + (win.width() - sc.width()) // 2,
                         -off.y() + (win.height() - sc.height()) // 2, sc)
        elif mode == 0:     # Tiled
            p.drawTiledPixmap(r, px)
        elif mode == 1:     # Stretched
            p.drawPixmap(r, px)
        elif mode == 2:     # Centred
            p.drawPixmap(r.left() + (r.width() - px.width()) // 2,
                         r.top() + (r.height() - px.height()) // 2, px)
        else:               # 3 Scaled fit, 4 Scaled fill (5 in previews)
            aspect = (Qt.AspectRatioMode.KeepAspectRatio if mode == 3
                      else Qt.AspectRatioMode.KeepAspectRatioByExpanding)
            sc = px.scaled(r.size(), aspect, Qt.TransformationMode.SmoothTransformation)
            p.setClipRect(r)
            p.drawPixmap(r.left() + (r.width() - sc.width()) // 2,
                         r.top() + (r.height() - sc.height()) // 2, sc)
        p.restore()
        return True

    @staticmethod
    def _paint_pattern(p, r, cs): #vers 1
        from PyQt6.QtGui import QColor, QPen, QBrush
        from PyQt6.QtCore import Qt
        style = cs.get('panel_pattern_style', 0)
        scale = max(2, cs.get('panel_pattern_scale', 8))
        cl    = QColor(cs.get('panel_pattern_light', '#2a2a4a'))
        cd    = QColor(cs.get('panel_pattern_dark',  '#141428'))
        p.fillRect(r, cd)
        if style == 1:  # Dots
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(cl)
            for x in range(r.left(), r.right(), scale):
                for y in range(r.top(), r.bottom(), scale):
                    p.drawEllipse(x, y, max(2,scale//4), max(2,scale//4))
        elif style == 2:  # Lines H
            p.setPen(QPen(cl, 1))
            for y in range(r.top(), r.bottom(), scale):
                p.drawLine(r.left(), y, r.right(), y)
        elif style == 3:  # Lines V
            p.setPen(QPen(cl, 1))
            for x in range(r.left(), r.right(), scale):
                p.drawLine(x, r.top(), x, r.bottom())
        elif style == 4:  # Diagonal
            p.setPen(QPen(cl, 1))
            for i in range(-r.height(), r.width()+r.height(), scale):
                p.drawLine(r.left()+i, r.top(), r.left()+i+r.height(), r.bottom())
        elif style == 5:  # Check
            p.setPen(Qt.PenStyle.NoPen)
            for row in range(r.height()//scale+1):
                for col in range(r.width()//scale+1):
                    p.setBrush(cl if (row+col)%2==0 else cd)
                    p.drawRect(r.left()+col*scale, r.top()+row*scale, scale, scale)
        elif style == 9:  # Odd/Even rows
            p.setPen(Qt.PenStyle.NoPen)
            for row in range(r.height()//scale+1):
                p.setBrush(cl if row%2==0 else cd)
                p.drawRect(r.left(), r.top()+row*scale, r.width(), scale)


def apply_panel_effects(window, app_settings): #vers 9
    """Walk a window's panels and apply the current panel effect to each.
    Also: translucent buttons and see-through windows. Skips the settings dialog.
    """
    from PyQt6.QtWidgets import QGroupBox, QFrame, QAbstractScrollArea
    mw = getattr(window, 'main_window', None)
    if (window.isWindow() and mw is not None and mw is not window and hasattr(mw, 'main_tab_widget')
            and not getattr(window, '_panel_fx_deferred', False)):
        from PyQt6.QtCore import QTimer                 # docked workshop not in its tab yet:
        window._panel_fx_deferred = True
        QTimer.singleShot(0, lambda w=window: apply_panel_effects(w.window(), app_settings))
        return                                          # apply once it sits in the main window
    cs = app_settings.current_settings
    effect = cs.get('panel_effect_type', 'none')
    image_all = image_all_on(cs)
    tb_mode = cs.get('image_titlebar', 0)
    bars = _titlebars(window)
    see = see_through_on(cs)
    if 'Settings' not in type(window).__name__ and 'Dialog' not in type(window).__name__:
        if see:
            _apply_image_under(window, cs, True)        # tints captured before containers clear
        _apply_see_through(window, cs, see)
        _apply_button_alpha(window, app_settings, image_all or see)
        if see:
            return

    if effect == 'none' and not panel_image(cs):
        from PyQt6.QtWidgets import QWidget
        _apply_image_under(window, cs, False)
        for w in window.findChildren(QWidget):          # repaint panels that had an effect
            if getattr(w, '_panel_effect_installed', False):
                w.update()
        return

    # Don't apply panel effects to the settings dialog itself
    window_class = type(window).__name__
    if 'Settings' in window_class or 'Dialog' in window_class:
        return

    # Merge theme colors into cs so _paint_fill can read panel_bg/bg_primary
    # (these live in the theme JSON, not in current_settings)
    try:
        theme_colors = app_settings.get_theme_colors()
        if theme_colors:
            # Only add keys not already in cs (cs values take priority)
            for key in ('panel_bg', 'bg_primary', 'bg_secondary'):
                if key not in cs and key in theme_colors:
                    cs = dict(cs)  # shallow copy so we don't mutate saved settings
                    cs[key] = theme_colors[key]
    except Exception:
        pass

    for widget in window.findChildren(QGroupBox):
        parent = widget.parent()
        skip = False
        while parent:
            if 'Settings' in type(parent).__name__ or 'Dialog' in type(parent).__name__:
                skip = True
                break
            parent = parent.parent() if hasattr(parent, 'parent') else None
        if skip:
            continue
        if tb_mode == 2:                                  # titlebar only
            widget._panel_effect_installed = False
            widget.update()
            continue
        widget._app_settings_ref = cs  # pass merged cs
        AppPanelEffect.install(widget, cs)
        widget.update()

    for widget in window.findChildren(QFrame):
        if widget.objectName() in ('titlebar', 'gadgetbar',
                                    'dp5_bitmaps_panel', 'dp5_brushcolors_panel',
                                    'dp5_imagepalette_panel', 'dp5_userpalette_panel'):
            continue
        if isinstance(widget, QAbstractScrollArea):      # painted via viewport, not the frame
            continue
        if widget.frameStyle() & QFrame.Shape.StyledPanel.value:
            is_bar = widget in bars
            if not (tb_mode == 0 or (tb_mode == 1 and not is_bar) or (tb_mode == 2 and is_bar)):
                widget._panel_effect_installed = False   # excluded by titlebar option
                widget.update()
                continue
            widget._app_settings_ref = cs
            AppPanelEffect.install(widget, cs)
            widget.update()

    _apply_image_under(window, cs, image_all_on(cs))


_VIEW_WORDS = ('Viewport', 'View', 'Canvas', 'Preview', 'Grid', 'Gauge')


def _is_view(w, solid=True): #vers 2
    """True inside a 3D view; with solid also any 2D viewport / canvas (keeps its paint)."""
    from PyQt6.QtWidgets import QScrollArea
    from PyQt6.QtOpenGLWidgets import QOpenGLWidget
    while w is not None:
        name = type(w).__name__
        if isinstance(w, QOpenGLWidget):
            return True
        if not solid:
            w = w.parent()
            continue
        if not name.startswith('Q') and any(k in name for k in _VIEW_WORDS):
            return True
        if isinstance(w, QScrollArea) and w.widget() is not None:
            inner = type(w.widget()).__name__              # scroll area around a canvas
            if not inner.startswith('Q') and any(k in inner for k in _VIEW_WORDS):
                return True
        w = w.parent()
    return False


def _titlebars(window): #vers 2
    """Title bars: widgets named 'titlebar' and each workshop's top toolbar."""
    from PyQt6.QtWidgets import QWidget
    bars = [f for f in window.findChildren(QWidget) if f.objectName() == 'titlebar']
    for w in [window] + window.findChildren(QWidget):
        tb = w.__dict__.get('_workshop_toolbar')
        if isinstance(tb, QWidget) and tb not in bars:
            bars.append(tb)
    return bars


def _gl_workshop_roots(top): #vers 1
    """Docked workshops holding a GL view (Map, Model...): kept solid, GL ghosts when translucent."""
    from PyQt6.QtOpenGLWidgets import QOpenGLWidget
    roots = set()
    for gl in top.findChildren(QOpenGLWidget):
        w = gl.parentWidget()
        while w is not None and w is not top:
            if type(w).__name__.endswith('Workshop') and not w.isWindow():
                roots.add(w)
                break
            w = w.parentWidget()
    return roots


def _apply_see_through(window, cs, on): #vers 5
    """Containers transparent and the top window translucent with its own tint:
    desktop shows through (no image). Viewports and docked GL workshops stay opaque."""
    from PyQt6.QtWidgets import (QWidget, QFrame, QMainWindow, QSplitter, QStackedWidget,
                                 QDockWidget, QScrollArea, QTabWidget, QGroupBox)
    from PyQt6.QtCore import Qt
    containers = (QWidget, QFrame, QMainWindow, QSplitter, QStackedWidget,
                  QDockWidget, QScrollArea, QTabWidget, QGroupBox)
    solid = cs.get('viewport_solid', True)
    top = window.window()
    roots = _gl_workshop_roots(top)
    for r in roots:
        r.setAutoFillBackground(on)                     # solid theme window colour under it
    for w in [top] + top.findChildren(QWidget):
        want = on and not any(w is r or r.isAncestorOf(w) for r in roots)
        if type(w) in containers and not _is_view(w, solid) and bool(w.property('seeThrough')) != want:
            w.setProperty('seeThrough', want)
            w.style().unpolish(w); w.style().polish(w)
    if on:
        AppPanelEffect.install_under(top, top, cs)      # window base tint (Panels opacity)
    elif getattr(top, '_panel_under_tint', None) is not None:
        AppPanelEffect.remove_under(top)
    rule = '\n*[seeThrough="true"] { background: transparent; }'
    ss = top.styleSheet().replace(rule, '')
    top.setStyleSheet(ss + rule if on else ss)
    if top.testAttribute(Qt.WidgetAttribute.WA_TranslucentBackground) != on:
        top.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground, on)
        if top.isVisible() and top.isWindow():         # native window must be recreated
            geo = top.geometry()
            top.hide(); top.show(); top.setGeometry(geo)


def _apply_button_alpha(window, app_settings, on): #vers 1
    """Buttons drawn at the Buttons transparency so the image / desktop shows."""
    import re
    from PyQt6.QtGui import QColor
    cs = app_settings.current_settings
    tc = app_settings.get_theme_colors() or {}
    a = max(0, min(100, cs.get('button_opacity', 100))) / 100.0
    def rgba(key, fallback, alpha): #vers 1
        c = QColor(tc.get(key, fallback))
        return f"rgba({c.red()}, {c.green()}, {c.blue()}, {round(alpha * 255)})"
    ss = re.sub(r'/\*btn-alpha\*/.*?/\*btn-alpha-end\*/', '', window.styleSheet(), flags=re.S)
    if on and a < 1.0:
        ss += ("/*btn-alpha*/ QPushButton { background-color: %s; } "
               "QPushButton:hover { background-color: %s; } "
               "QPushButton:pressed { background-color: %s; } /*btn-alpha-end*/"
               % (rgba('button_normal', '#3c3c46', a), rgba('button_hover', '#50505a', min(1.0, a + 0.2)),
                  rgba('button_pressed', '#28282e', min(1.0, a + 0.3))))
    window.setStyleSheet(ss)


def _apply_image_under(window, cs, on): #vers 7
    """Panel image behind lists, ribbons, tab bars/pages, splitters and plain panels (tinted),
    or removed when off. GL viewports are left alone."""
    from PyQt6.QtWidgets import (QAbstractScrollArea, QToolBar, QTabWidget, QStackedWidget,
                                 QTabBar, QSplitter, QFrame)
    from PyQt6.QtOpenGLWidgets import QOpenGLWidget
    solid = cs.get('viewport_solid', True)
    targets = []
    for view in window.findChildren(QAbstractScrollArea):
        vp = view.viewport()
        if vp is not None and not isinstance(vp, QOpenGLWidget) and not _is_view(view, solid):
            targets.append((vp, view))
    targets += [(tb, tb) for tb in window.findChildren(QToolBar)]
    for tw in window.findChildren(QTabWidget):
        st = tw.findChild(QStackedWidget)
        if st is not None:
            targets.append((st, st))
    targets += [(tb, tb) for tb in window.findChildren(QTabBar)]
    for sp in window.findChildren(QSplitter):
        for i in range(1, sp.count()):
            h = sp.handle(i)
            if h is not None:
                targets.append((h, h))
    in_view = lambda w: _is_view(w, solid)               # viewports / canvases keep their paint
    see = see_through_on(cs)
    bars = _titlebars(window)
    for fr in window.findChildren(QFrame):                # plain panels; viewports keep their own paint
        if in_view(fr) or fr.objectName() == 'gadgetbar' or fr in bars:
            continue
        if type(fr) is QFrame and (see or not (fr.frameStyle() & QFrame.Shape.StyledPanel.value)):
            targets.append((fr, fr))
    tb_mode = cs.get('image_titlebar', 0)             # 0 panels + titlebar, 1 panels, 2 titlebar
    if tb_mode == 1:
        keep = [t for t in targets if not any(t[0] is b or b.isAncestorOf(t[0]) for b in bars)]
        for t, _st in targets:
            if (t, _st) not in keep and getattr(t, '_panel_under_tint', None) is not None:
                AppPanelEffect.remove_under(t)
        targets = keep
    elif tb_mode == 2:
        drop = targets
        targets = []
        if not on:
            targets = drop
        else:
            for t, _st in drop:                         # clear any earlier non-titlebar tint
                if getattr(t, '_panel_under_tint', None) is not None:
                    AppPanelEffect.remove_under(t)
    if tb_mode in (0, 2):
        targets += [(b, b) for b in bars]               # Titlebar transparency slider
    elif not on or tb_mode == 1:
        for b in bars:
            if getattr(b, '_panel_under_tint', None) is not None:
                AppPanelEffect.remove_under(b)
    for target, styled in targets:
        w, skip = styled.parent(), False
        while w is not None:
            if 'Settings' in type(w).__name__ or 'Dialog' in type(w).__name__:
                skip = True
                break
            w = w.parent()
        if skip:
            continue
        if on:
            AppPanelEffect.install_under(target, styled, cs)
        elif getattr(target, '_panel_under_tint', None) is not None:
            AppPanelEffect.remove_under(target)


class PanelPreviewWidget(QWidget): #vers 1
    """Live preview widget for panel effects — fill, gradient, pattern, image, transparency."""

    def __init__(self, settings_dialog, mode="fill", parent=None):
        super().__init__(parent)
        self._dlg  = settings_dialog
        self._mode = mode   # fill | gradient | pattern | copper | image | transparency | button
        self.setMinimumHeight(100)
        self.setMaximumHeight(130)
        self.setSizePolicy(
            QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self.setAutoFillBackground(False)

    def refresh(self):
        """Call after any control changes to force a repaint."""
        self.update()

    def _get_colours(self): #vers 1
        """Build a colour dict from the live editors + current_settings.
        Editors are the source of truth while the dialog is open."""
        # Start with current_settings as base (has panel_fill_dir, pattern_scale etc.)
        cs = dict(self._dlg.app_settings.current_settings)
        # Override with live editor values — these reflect what the user has picked
        editors = getattr(self._dlg, 'color_editors', {})
        for key, editor in editors.items():
            val = editor.color_input.text() if hasattr(editor, 'color_input') else None
            if val and val.startswith('#') and len(val) == 7:
                cs[key] = val
        return cs

    def paintEvent(self, event): #vers 2
        from PyQt6.QtGui import (QPainter, QColor, QLinearGradient,
                                  QPen, QBrush, QImage, QPixmap)
        from PyQt6.QtCore import QRectF, Qt, QPointF
        p = QPainter()
        if not p.begin(self):
            return
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        r = self.rect()
        cs = self._get_colours()

        try:
            if self._mode == "fill":
                self._draw_fill(p, r, cs)
            elif self._mode == "gradient":
                self._draw_gradient(p, r, cs)
            elif self._mode == "pattern":
                self._draw_pattern(p, r, cs)
            elif self._mode == "copper":
                self._draw_copper(p, r, cs)
            elif self._mode == "image":
                self._draw_image(p, r, cs)
            elif self._mode == "transparency":
                self._draw_transparency(p, r, cs)
            elif self._mode == "hero":
                self._draw_hero(p, r, cs)
        except Exception:
            pass

        # Border
        from PyQt6.QtGui import QPen, QColor
        p.setPen(QPen(QColor("#555555"), 1))
        p.setBrush(Qt.BrushStyle.NoBrush)
        p.drawRect(r.adjusted(0, 0, -1, -1))
        if p.isActive():
            p.end()

    def _c(self, key, default="#1a1a2e"):
        return self._dlg.app_settings.current_settings.get(key, default)

    def _draw_fill(self, p, r, cs):
        from PyQt6.QtGui import QColor, QLinearGradient, QBrush
        from PyQt6.QtCore import QPointF
        ca = QColor(cs.get("panel_fill_a", "#1a1a2e"))
        cb = QColor(cs.get("panel_fill_b", "#16213e"))
        # Light theme guard
        panel_bg = cs.get('panel_bg') or cs.get('bg_primary', '')
        if panel_bg:
            tc = QColor(panel_bg)
            if tc.lightness() > 128 and ca.lightness() < 64:
                ca = cb = tc
        d  = cs.get("panel_fill_dir", 0)
        if d == 0:
            p.fillRect(r, ca)
        else:
            dirs = {
                1: (QPointF(r.left(),  r.top()),    QPointF(r.right(), r.top())),
                2: (QPointF(r.left(),  r.top()),    QPointF(r.left(),  r.bottom())),
                3: (QPointF(r.left(),  r.bottom()), QPointF(r.right(), r.top())),
                4: (QPointF(r.right(), r.bottom()), QPointF(r.left(),  r.top())),
                5: (QPointF(r.left(),  r.top()),    QPointF(r.right(), r.bottom())),
                6: (QPointF(r.right(), r.top()),    QPointF(r.left(),  r.bottom())),
            }
            s, e = dirs.get(d, (QPointF(r.left(), r.top()), QPointF(r.right(), r.top())))
            g = QLinearGradient(s, e)
            g.setColorAt(0, ca); g.setColorAt(1, cb)
            p.fillRect(r, g)
        # Sample label
        self._label(p, r, "Fill Preview")

    def _draw_gradient(self, p, r, cs):
        from PyQt6.QtGui import QColor, QLinearGradient
        from PyQt6.QtCore import QPointF
        s1 = QColor(cs.get("panel_grad_stop1", "#1a1a2e"))
        s2 = QColor(cs.get("panel_grad_stop2", "#2d1b4e"))
        s3 = QColor(cs.get("panel_grad_stop3", "#16213e"))
        d  = cs.get("panel_grad_dir", 1)
        pts = {
            0: (QPointF(r.left(),  r.top()),    QPointF(r.right(), r.top())),
            1: (QPointF(r.left(),  r.top()),    QPointF(r.left(),  r.bottom())),
            2: (QPointF(r.left(),  r.top()),    QPointF(r.right(), r.bottom())),
            3: (QPointF(r.right(), r.top()),    QPointF(r.left(),  r.bottom())),
            4: (QPointF(r.left(),  r.bottom()), QPointF(r.right(), r.top())),
            5: (QPointF(r.right(), r.bottom()), QPointF(r.left(),  r.top())),
        }
        s, e = pts.get(d, pts[1])
        g = QLinearGradient(s, e)
        g.setColorAt(0, s1); g.setColorAt(0.5, s2); g.setColorAt(1, s3)
        p.fillRect(r, g)
        self._label(p, r, "Gradient Preview")

    def _draw_pattern(self, p, r, cs):
        from PyQt6.QtGui import QColor, QBrush, QPen
        from PyQt6.QtCore import Qt
        style = cs.get("panel_pattern_style", 0)
        scale = cs.get("panel_pattern_scale", 8)
        cl    = QColor(cs.get("panel_pattern_light", "#2a2a4a"))
        cd    = QColor(cs.get("panel_pattern_dark",  "#141428"))

        p.fillRect(r, cd)

        if style == 0:   # None
            pass
        elif style == 1: # Dots
            p.setPen(Qt.PenStyle.NoPen)
            p.setBrush(cl)
            x = r.left()
            while x < r.right():
                y = r.top()
                while y < r.bottom():
                    p.drawEllipse(x, y, max(2, scale//4), max(2, scale//4))
                    y += scale
                x += scale
        elif style == 2: # Lines H
            p.setPen(QPen(cl, 1))
            y = r.top()
            while y < r.bottom():
                p.drawLine(r.left(), y, r.right(), y)
                y += scale
        elif style == 3: # Lines V
            p.setPen(QPen(cl, 1))
            x = r.left()
            while x < r.right():
                p.drawLine(x, r.top(), x, r.bottom())
                x += scale
        elif style == 4: # Diagonal
            p.setPen(QPen(cl, 1))
            step = scale
            for i in range(-r.height(), r.width() + r.height(), step):
                p.drawLine(r.left() + i, r.top(),
                           r.left() + i + r.height(), r.bottom())
        elif style == 5: # Check
            p.setPen(Qt.PenStyle.NoPen)
            for row in range(r.height() // scale + 1):
                for col in range(r.width() // scale + 1):
                    if (row + col) % 2 == 0:
                        p.setBrush(cl)
                    else:
                        p.setBrush(cd)
                    p.drawRect(r.left() + col*scale, r.top() + row*scale, scale, scale)
        elif style == 6: # Waves
            from PyQt6.QtGui import QPainterPath
            import math
            p.setPen(QPen(cl, 1))
            y = r.top() + scale//2
            while y < r.bottom():
                path = QPainterPath()
                path.moveTo(r.left(), y)
                x = r.left()
                while x < r.right():
                    path.quadTo(x + scale//2, y - scale//2,
                                x + scale, y)
                    x += scale
                p.drawPath(path)
                y += scale
        elif style == 7: # Tartan
            p.setPen(QPen(cl, 2))
            x = r.left()
            while x < r.right():
                p.drawLine(x, r.top(), x, r.bottom())
                x += scale
            y = r.top()
            while y < r.bottom():
                p.drawLine(r.left(), y, r.right(), y)
                y += scale
            # Second set offset
            p.setPen(QPen(cl.lighter(130), 1))
            x = r.left() + scale//2
            while x < r.right():
                p.drawLine(x, r.top(), x, r.bottom())
                x += scale
        elif style == 8: # Picnic
            # Red/white check
            p.setPen(Qt.PenStyle.NoPen)
            from PyQt6.QtGui import QColor
            rc = QColor("#cc4444")
            for row in range(r.height() // scale + 1):
                for col in range(r.width() // scale + 1):
                    c = rc if (row + col) % 2 == 0 else QColor("#ffffff")
                    p.setBrush(c)
                    p.drawRect(r.left() + col*scale, r.top() + row*scale, scale, scale)
        elif style == 9: # Odd/Even rows
            p.setPen(Qt.PenStyle.NoPen)
            for row in range(r.height() // scale + 1):
                p.setBrush(cl if row % 2 == 0 else cd)
                p.drawRect(r.left(), r.top() + row*scale, r.width(), scale)

        self._label(p, r, "Pattern Preview")

    def _draw_copper(self, p, r, cs):
        from PyQt6.QtGui import QColor, QLinearGradient
        from PyQt6.QtCore import QPointF
        cl = QColor(cs.get("copper_light", "#b87333"))
        cd = QColor(cs.get("copper_dark",  "#7a4a1a"))
        # Multi-stop copper shimmer
        g = QLinearGradient(QPointF(r.left(), r.top()), QPointF(r.left(), r.bottom()))
        g.setColorAt(0.0,  cd)
        g.setColorAt(0.2,  cl)
        g.setColorAt(0.4,  cl.lighter(140))
        g.setColorAt(0.5,  cl)
        g.setColorAt(0.7,  cd)
        g.setColorAt(0.85, cl.lighter(120))
        g.setColorAt(1.0,  cd)
        p.fillRect(r, g)
        self._label(p, r, "Copper Effect Preview")

    def _draw_image(self, p, r, cs): #vers 2
        from PyQt6.QtGui import QColor
        p.fillRect(r, QColor(cs.get("bg_primary", "#2a2a2a")))
        if AppPanelEffect._paint_image(p, r, cs):
            self._label(p, r, "Image Preview")
            return
        p.setPen(QColor("#888888"))
        p.drawText(r, 0x84, "No image selected")  # AlignCenter|AlignVCenter

    def _draw_transparency(self, p, r, cs):
        from PyQt6.QtGui import QColor, QLinearGradient
        from PyQt6.QtCore import QPointF
        # Checkerboard to show transparency
        sq = 12
        from PyQt6.QtGui import QBrush
        p.setPen(0)
        for row in range(r.height() // sq + 1):
            for col in range(r.width() // sq + 1):
                c = QColor("#aaaaaa") if (row+col)%2==0 else QColor("#888888")
                p.setBrush(c)
                p.drawRect(r.left()+col*sq, r.top()+row*sq, sq, sq)
        # Overlay a panel-coloured rect at current opacity
        opacity = cs.get("panel_opacity", 100) / 100.0
        p.setOpacity(opacity)
        p.fillRect(r, QColor(cs.get("bg_primary", "#1a1a2e")))
        p.setOpacity(1.0)
        self._label(p, r, f"Panel opacity: {int(opacity*100)}%")

    def _draw_hero(self, p, r, cs):
        from PyQt6.QtGui import QColor, QLinearGradient
        from PyQt6.QtCore import QPointF
        # Detect light/dark
        try:
            bg = (self._dlg.app_settings.get_theme_colors() or {}).get("bg_primary","#1a1a2e")
            is_dark = QColor(bg).lightness() < 128
        except Exception:
            is_dark = True
        if is_dark:
            c1 = QColor(cs.get("hero_gradient_dark_start", "#1a1a2e"))
            c2 = QColor(cs.get("hero_gradient_dark_end",   "#2d2d5e"))
        else:
            c1 = QColor(cs.get("hero_gradient_light_start", "#1a1a2e"))
            c2 = QColor(cs.get("hero_gradient_light_end",   "#2d2d5e"))
        g = QLinearGradient(QPointF(r.left(), r.top()), QPointF(r.right(), r.top()))
        g.setColorAt(0, c1); g.setColorAt(1, c2)
        p.fillRect(r, g)
        # Title mockup
        from PyQt6.QtGui import QFont
        p.setPen(QColor("#ffffff"))
        f = QFont("Arial", 14, QFont.Weight.Bold)
        p.setFont(f)
        from PyQt6.QtCore import Qt
        AppBuildLabel = (f"{App_name} - Build {App_build}")
        p.drawText(r.adjusted(16,0,0,0), Qt.AlignmentFlag.AlignVCenter, AppBuildLabel)
        p.setPen(QColor("#aaaacc"))
        f2 = QFont("Arial", 8)
        p.setFont(f2)
        p.drawText(r.adjusted(16, 28, 0, 0), Qt.AlignmentFlag.AlignTop, "GTA modding toolkit — IMG · COL · TXD · DFF")

    def _label(self, p, r, text):
        from PyQt6.QtGui import QColor, QFont
        from PyQt6.QtCore import Qt, QRect
        p.setOpacity(0.7)
        p.fillRect(QRect(r.left(), r.bottom()-18, r.width(), 18), QColor(0,0,0))
        p.setOpacity(1.0)
        p.setPen(QColor("#ffffff"))
        p.setFont(QFont("Arial", 7))
        p.drawText(r.adjusted(4, 0, -4, -2), Qt.AlignmentFlag.AlignBottom, text)

class SettingsDialog(AppsysWinMixin, AppsysUIMixin, AppsysLogicMixin, AppsysPanelTestMixin,
                     DockPanelsMixin, QDialog): #vers 17
    """Settings dialog for theme and preference management"""

    themeChanged = pyqtSignal(str)  # theme_name
    settingsChanged = pyqtSignal()

    def __init__(self, app_settings, parent=None, main_window=None): #vers 7
        """Initialize settings dialog"""
        super().__init__(parent)

        # Store main_window ref — may differ from parent (e.g. docked workshop)
        self.main_window = main_window or parent

        self.setWindowTitle("Global App System Settings")
        self.setMinimumSize(800, 600)
        self.setModal(True)

        # App icon — gear+palette SVG rendered at 64px
        try:
            _icon_svg = """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64">
  <!-- Outer gear ring -->
  <circle cx="32" cy="32" r="28" fill="#2a4a6a" stroke="#4a8abd" stroke-width="2"/>
  <!-- Gear teeth -->
  <g fill="#4a8abd">
    <rect x="29" y="2"  width="6" height="8" rx="1"/>
    <rect x="29" y="54" width="6" height="8" rx="1"/>
    <rect x="2"  y="29" width="8" height="6" rx="1"/>
    <rect x="54" y="29" width="8" height="6" rx="1"/>
    <rect x="10" y="8"  width="6" height="8" rx="1" transform="rotate(45 13 12)"/>
    <rect x="48" y="8"  width="6" height="8" rx="1" transform="rotate(-45 51 12)"/>
    <rect x="10" y="48" width="6" height="8" rx="1" transform="rotate(-45 13 52)"/>
    <rect x="48" y="48" width="6" height="8" rx="1" transform="rotate(45 51 52)"/>
  </g>
  <!-- Inner circle -->
  <circle cx="32" cy="32" r="16" fill="#1a2a3a"/>
  <!-- Palette dots -->
  <circle cx="26" cy="28" r="4" fill="#e05050"/>
  <circle cx="32" cy="24" r="4" fill="#50c050"/>
  <circle cx="38" cy="28" r="4" fill="#5080e0"/>
  <circle cx="38" cy="36" r="4" fill="#e0c050"/>
  <circle cx="32" cy="40" r="4" fill="#c050c0"/>
  <circle cx="26" cy="36" r="4" fill="#50c0c0"/>
</svg>"""
            from PyQt6.QtSvg import QSvgRenderer
            from PyQt6.QtGui import QIcon, QPixmap, QPainter
            from PyQt6.QtCore import QByteArray, QSize
            renderer = QSvgRenderer(QByteArray(_icon_svg.encode()))
            px = QPixmap(64, 64)
            px.fill(__import__('PyQt6.QtCore', fromlist=['Qt']).Qt.GlobalColor.transparent)
            painter = QPainter(px)
            renderer.render(painter)
            painter.end()
            self.setWindowIcon(QIcon(px))
        except Exception as _ie:
            pass  # Icon is cosmetic — never crash on it

        self.app_settings = app_settings
        self.original_settings = app_settings.current_settings.copy()
        self._modified_colors = {}
        self.color_editors = {}
        self._color_history = []   # stack of {color_key: hex} snapshots
        self._history_limit = 30   # max undo steps

        # Initialize icon provider
        self.icons = IconProvider(self)

        # Set default fonts
        from PyQt6.QtGui import QFont
        default_font = QFont("Fira Sans Condensed", 14)
        self.setFont(default_font)
        self.title_font = QFont("Arial", 14)
        self.panel_font = QFont("Arial", 10)
        self.section_title_font = self.panel_font   # dock pane titles
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


    # = CORNER RESIZE METHODS

    # ===== GLOBAL SLIDER HANDLERS =====

    # ===== THEME MANAGEMENT =====

    # ===== SETTINGS MANAGEMENT =====

def apply_theme_to_app(app, app_settings): #vers 2
    """Apply theme to entire application — force repaint all widgets."""
    stylesheet = app_settings.get_stylesheet()
    app.setStyleSheet(stylesheet)
    # Force Qt to re-evaluate styles on all existing widgets
    # Without this, widgets with WA_StyledBackground set after init
    # keep their old palette on theme switch
    for widget in app.allWidgets():
        try:
            app.style().unpolish(widget)
            app.style().polish(widget)
            widget.update()
        except Exception:
            pass


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
    if sys.platform.startswith('linux'):   # xcb: docks re-dock (Wayland cannot)
        os.environ['QT_QPA_PLATFORM'] = 'xcb'
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
