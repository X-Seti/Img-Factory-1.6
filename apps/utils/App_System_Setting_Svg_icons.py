#!/usr/bin/env python3
# apps/utils/depends/App_System_Setting_Svg_icons.py - Version: 1
# X-Seti - Apr 2026 - Global App System Settings — SVG icon provider
"""
IconProvider — theme-aware SVG icons for Global App System Settings.
Used by SettingsDialog, CustomWindow and any app that imports AppSettings.

Usage:
    from apps.utils.depends.App_System_Setting_Svg_icons import IconProvider
    self.icons = IconProvider(self, self.app_settings)
    btn.setIcon(self.icons.settings_icon())
"""

##Methods list
# IconProvider.__init__
# IconProvider._get_icon_color
# IconProvider._svg_to_icon
# IconProvider.clear_cache
# IconProvider.restore_icon
# IconProvider.minimize_icon
# IconProvider.maximize_icon
# IconProvider.close_icon
# IconProvider.folder_icon
# IconProvider.save_icon
# IconProvider.import_icon
# IconProvider.export_icon
# IconProvider.add_icon
# IconProvider.delete_icon
# IconProvider.edit_icon
# IconProvider.copy_icon
# IconProvider.view_icon
# IconProvider.settings_icon
# IconProvider.info_icon
# IconProvider.filter_icon
# IconProvider.undo_icon
# IconProvider.refresh_icon

from PyQt6.QtWidgets import QWidget
from PyQt6.QtGui import QIcon

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

    def _debug_current_img(self, *a, **kw): pass
    def _test_debug_output(self, *a, **kw): pass
