#!/usr/bin/env python3
#this belongs in apps/utils/depends/App_System_Setting_Svg_icons.py - Version: 2
# X-Seti - Apr 2026 - Global App System Settings — SVG icon provider
"""
IconProvider — theme-aware SVG icons for Global App System Settings.
Used by SettingsDialog, CustomWindow and any app that imports AppSettings.

Usage:
    from apps.utils.depends.App_System_Setting_Svg_icons import IconProvider
    self.icons = IconProvider(self, self.app_settings)
    btn.setIcon(self.icons.settings_icon())
"""

##Methods list -
# IconProvider.__init__
# IconProvider._get_icon_color
# IconProvider._svg_to_icon
# IconProvider.add_icon
# IconProvider.clear_cache
# IconProvider.close_icon
# IconProvider.copy_icon
# IconProvider.delete_icon
# IconProvider.edit_icon
# IconProvider.export_icon
# IconProvider.filter_icon
# IconProvider.folder_icon
# IconProvider.import_icon
# IconProvider.info_icon
# IconProvider.maximize_icon
# IconProvider.minimize_icon
# IconProvider.refresh_icon
# IconProvider.restore_icon
# IconProvider.save_icon
# IconProvider.settings_icon
# IconProvider.undo_icon
# IconProvider.view_icon

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
