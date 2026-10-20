# X-Seti - October26 2025 - IMG Factory 1.5 - Shared SVG Icon System
# This belongs in methods/svg_shared_icons.py - Version: 3

"""
Shared SVG Icon System - Replaces ALL emojis with clean SVG icons
Centralized icon generation for consistent UI across IMG Factory
Theme-compatible with currentColor support
"""

from PyQt6.QtGui import QIcon, QPixmap, QPainter, QColor
from PyQt6.QtSvg import QSvgRenderer
from PyQt6.QtCore import QByteArray, QSize

##Methods list -
# svg_to_icon
# get_img_file_icon
# get_col_file_icon
# get_txd_file_icon
# get_folder_icon
# get_file_icon
# get_package_icon
# get_shield_icon
# get_image_icon
# get_trash_icon
# get_refresh_icon
# get_tearoff_icon
# get_checkmark_icon
# get_palette_icon
# get_import_icon
# get_export_icon
# get_save_icon
# get_open_icon
# get_close_icon
# get_add_icon
# get_remove_icon
# get_edit_icon
# get_view_icon
# get_search_icon
# get_settings_icon
# get_info_icon
# get_warning_icon
# get_error_icon
# get_success_icon

def svg_to_icon(svg_data: bytes, size: int = 24, color: str = None) -> QIcon: #vers 3
    """Convert SVG data to QIcon with optional color override
    
    Args:
        svg_data: SVG XML as bytes
        size: Icon size in pixels (default 24)
        color: Optional hex color to override currentColor
    
    Returns:
        QIcon object
    """
    try:
        # Use fallback colors to avoid circular import with AppSettings
        bg_secondary = "#2d2d2d"
        bg_primary = "#1e1e1e"
        text_primary = "#ffffff"
        
        # Replace theme color placeholders in SVG data
        svg_string = svg_data.decode("utf-8")
        svg_string = svg_string.replace("{bg_secondary}", bg_secondary)
        svg_string = svg_string.replace("{bg_primary}", bg_primary)
        svg_string = svg_string.replace("{text_primary}", text_primary)
        
        # If a specific color is provided, replace currentColor with that color
        if color:
            svg_string = svg_string.replace("currentColor", color)
        else:
            # Replace currentColor with text_primary
            svg_string = svg_string.replace("currentColor", text_primary)
        
        svg_data = svg_string.encode("utf-8")
        
        renderer = QSvgRenderer(QByteArray(svg_data))
        pixmap = QPixmap(QSize(size, size))
        pixmap.fill(QColor(0, 0, 0, 0))  # Transparent background
        
        painter = QPainter(pixmap)
        renderer.render(painter)
        painter.end()
        
        return QIcon(pixmap)
    except Exception as e:
        print(f"Error creating icon: {e}")

class SVGIconFactory: #vers 1
    """
    SVGIconFactory - Compatibility wrapper for icon creation
    Provides a class-based interface to SVG icon functions
    """

    @staticmethod
    def create_icon(icon_name: str, size: int = 24, color: str = None) -> QIcon:
        """
        Create icon by name

        Args:
            icon_name: Icon identifier (e.g., 'save', 'open', 'close')
            size: Icon size in pixels
            color: Optional color override

        Returns:
            QIcon object
        """
        icon_map = {
            'save': get_save_icon,
            'open': get_open_icon,
            'close': get_close_icon,
            'add': get_add_icon,
            'remove': get_remove_icon,
            'edit': get_edit_icon,
            'view': get_view_icon,
            'search': get_search_icon,
            'settings': get_settings_icon,
            'info': get_info_icon,
            'warning': get_warning_icon,
            'error': get_error_icon,
            'success': get_success_icon,
            'refresh': get_refresh_icon,
            'import': get_import_icon,
            'export': get_export_icon,
            'trash': get_trash_icon,
            'folder': get_folder_icon,
            'file': get_file_icon,
            'image': get_image_icon,
            'package': get_package_icon,
        }

        icon_func = icon_map.get(icon_name.lower(), get_file_icon)
        return icon_func(size)
# = App icon

def get_app_icon(size: int = 64) -> QIcon: #vers 3
    """IMG Factory application icon - Fixed color rendering"""
    svg_data = b'''<svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
        <defs>
            <linearGradient id="bgGradient" x1="0%" y1="0%" x2="100%" y2="100%">
                <stop offset="0%" style="stop-color:#3a3a3a;stop-opacity:1" />
                <stop offset="100%" style="stop-color:#2d2d2d;stop-opacity:1" />
            </linearGradient>
        </defs>
        <rect x="0" y="0" width="64" height="64" rx="12" ry="12" fill="url(#bgGradient)"/>
        <text x="32" y="42" font-size="28" fill="#ffffff" text-anchor="middle" font-weight="bold" font-family="Arial, sans-serif">IMG</text>
    </svg>'
    return svg_to_icon(svg_data, size)


    </svg>'''
    return svg_to_icon(svg_data, size)

# = FILE TYPE ICONS (Replace emojis in tabs)

def get_img_file_icon(size: int = 24) -> QIcon: #vers 1
    """IMG archive icon - Replaces emoji"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M21 16V8a2 2 0 00-1-1.73l-7-4a2 2 0 00-2 0l-7 4A2 2 0 003 8v8a2 2 0 001 1.73l7 4a2 2 0 002 0l7-4A2 2 0 0021 16z" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        <path d="M3.27 6.96L12 12.01l8.73-5.05M12 22.08V12" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        <text x="12" y="15" font-size="6" fill="currentColor" text-anchor="middle" font-weight="bold">IMG</text>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_col_file_icon(size: int = 24) -> QIcon: #vers 1
    """COL collision icon - Replaces emoji"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M12 2L4 6v6c0 5.5 3.8 10.7 8 12 4.2-1.3 8-6.5 8-12V6l-8-4z" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        <path d="M8 12l2 2 6-6" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_txd_file_icon(size: int = 24) -> QIcon: #vers 1
    """TXD texture icon - Replaces emoji"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <rect x="3" y="3" width="18" height="18" rx="2" 
            stroke="currentColor" stroke-width="2" fill="none"/>
        <circle cx="8.5" cy="8.5" r="1.5" fill="currentColor"/>
        <path d="M21 15l-5-5L5 21" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        <text x="12" y="20" font-size="5" fill="currentColor" text-anchor="middle" font-weight="bold">TXD</text>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_folder_icon(size: int = 24) -> QIcon: #vers 1
    """Folder icon - Replaces 📁 emoji"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-7l-2-2H5a2 2 0 00-2 2z" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_file_icon(size: int = 24) -> QIcon: #vers 1
    """Generic file icon - Replaces emoji"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z" 
            stroke="currentColor" stroke-width="2" fill="none"/>
        <path d="M14 2v6h6" 
            stroke="currentColor" stroke-width="2" fill="none"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


# = ACTION ICONS

def get_trash_icon(size: int = 24) -> QIcon: #vers 1
    """Delete/trash icon - Replaces 🗑️ emoji"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <polyline points="3 6 5 6 21 6" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        <path d="M19 6v14a2 2 0 01-2 2H7a2 2 0 01-2-2V6m3 0V4a2 2 0 012-2h4a2 2 0 012 2v2" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_refresh_icon(size: int = 24) -> QIcon: #vers 1
    """Refresh icon - Replaces 🔄 emoji"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M21.5 2v6h-6M2.5 22v-6h6M2 11.5a10 10 0 0117-7l2.5 2.5M22 12.5a10 10 0 01-17 7l-2.5-2.5" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_tearoff_icon(size: int = 24) -> QIcon: #vers 1
    """Tearoff/detach icon - Replaces ↗️ emoji"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M18 13v6a2 2 0 01-2 2H5a2 2 0 01-2-2V8a2 2 0 012-2h6M15 3h6v6M10 14L21 3" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_checkmark_icon(size: int = 24) -> QIcon: #vers 1
    """Checkmark icon - Replaces ✓ emoji"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <polyline points="20 6 9 17 4 12" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_palette_icon(size: int = 24) -> QIcon: #vers 1
    """Theme/palette icon - Replaces 🎨 emoji"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M12 2a10 10 0 00-9.95 11.1C2.5 17.7 6.3 21 10.9 21h1.2a2 2 0 002-2v-.3c0-.5.2-1 .6-1.3.4-.4.6-.9.6-1.4 0-1.1-.9-2-2-2h-1.4a8 8 0 110-10.3" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        <circle cx="7.5" cy="10.5" r="1.5" fill="currentColor"/>
        <circle cx="12" cy="7.5" r="1.5" fill="currentColor"/>
        <circle cx="16.5" cy="10.5" r="1.5" fill="currentColor"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_import_icon(size: int = 24) -> QIcon: #vers 1
    """Import/download icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4M7 10l5 5 5-5M12 15V3" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_export_icon(size: int = 24) -> QIcon: #vers 1
    """Export/upload icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4M17 8l-5-5-5 5M12 3v12" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_save_icon(size: int = 24) -> QIcon: #vers 1
    """Save icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M19 21H5a2 2 0 01-2-2V5a2 2 0 012-2h11l5 5v11a2 2 0 01-2 2z" 
            stroke="currentColor" stroke-width="2" fill="none"/>
        <path d="M17 21v-8H7v8M7 3v5h8" 
            stroke="currentColor" stroke-width="2" fill="none"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_open_icon(size: int = 24) -> QIcon: #vers 1
    """Open file icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z" 
            stroke="currentColor" stroke-width="2" fill="none"/>
        <path d="M14 2v6h6M12 11v6M9 14l3 3 3-3" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_close_icon(size: int = 24) -> QIcon: #vers 1
    """Close/X icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <line x1="18" y1="6" x2="6" y2="18" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        <line x1="6" y1="6" x2="18" y2="18" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_add_icon(size: int = 24) -> QIcon: #vers 1
    """Add/plus icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <line x1="12" y1="5" x2="12" y2="19" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        <line x1="5" y1="12" x2="19" y2="12" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_remove_icon(size: int = 24) -> QIcon: #vers 1
    """Remove/minus icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <line x1="5" y1="12" x2="19" y2="12" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_edit_icon(size: int = 24) -> QIcon: #vers 1
    """Edit/pencil icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M11 4H4a2 2 0 00-2 2v14a2 2 0 002 2h14a2 2 0 002-2v-7M18.5 2.5a2.121 2.121 0 013 3L12 15l-4 1 1-4 9.5-9.5z" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_view_icon(size: int = 24) -> QIcon: #vers 1
    """View/eye icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z" 
            stroke="currentColor" stroke-width="2" fill="none"/>
        <circle cx="12" cy="12" r="3" 
            stroke="currentColor" stroke-width="2" fill="none"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_search_icon(size: int = 24) -> QIcon: #vers 1
    """Search/magnifying glass icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <circle cx="11" cy="11" r="8" 
            stroke="currentColor" stroke-width="2" fill="none"/>
        <line x1="21" y1="21" x2="16.65" y2="16.65" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_settings_icon(size: int = 24) -> QIcon: #vers 1
    """Settings/gear icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <circle cx="12" cy="12" r="3" 
            stroke="currentColor" stroke-width="2" fill="none"/>
        <path d="M12 1v6m0 6v6M5.6 5.6l4.2 4.2m4.4 4.4l4.2 4.2M1 12h6m6 0h6M5.6 18.4l4.2-4.2m4.4-4.4l4.2-4.2" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_info_icon(size: int = 24) -> QIcon: #vers 1
    """Info/information icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <circle cx="12" cy="12" r="10" 
            stroke="currentColor" stroke-width="2" fill="none"/>
        <line x1="12" y1="16" x2="12" y2="12" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        <line x1="12" y1="8" x2="12.01" y2="8" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_warning_icon(size: int = 24) -> QIcon: #vers 1
    """Warning/alert triangle icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M10.29 3.86L1.82 18a2 2 0 001.71 3h16.94a2 2 0 001.71-3L13.71 3.86a2 2 0 00-3.42 0z" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        <line x1="12" y1="9" x2="12" y2="13" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        <line x1="12" y1="17" x2="12.01" y2="17" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_error_icon(size: int = 24) -> QIcon: #vers 1
    """Error/X circle icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <circle cx="12" cy="12" r="10" 
            stroke="currentColor" stroke-width="2" fill="none"/>
        <line x1="15" y1="9" x2="9" y2="15" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        <line x1="9" y1="9" x2="15" y2="15" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_success_icon(size: int = 24) -> QIcon: #vers 1
    """Success/checkmark circle icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <circle cx="12" cy="12" r="10" 
            stroke="currentColor" stroke-width="2" fill="none"/>
        <polyline points="9 12 11 14 15 10" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_package_icon(size: int = 24) -> QIcon: #vers 1
    """Package/box icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M21 16V8a2 2 0 00-1-1.73l-7-4a2 2 0 00-2 0l-7 4A2 2 0 003 8v8a2 2 0 001 1.73l7 4a2 2 0 002 0l7-4A2 2 0 0021 16z" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        <polyline points="3.27 6.96 12 12.01 20.73 6.96" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        <line x1="12" y1="22.08" x2="12" y2="12" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_shield_icon(size: int = 24) -> QIcon: #vers 1
    """Shield/protection icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_image_icon(size: int = 24) -> QIcon: #vers 1
    """Image/picture icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <rect x="3" y="3" width="18" height="18" rx="2" ry="2" 
            stroke="currentColor" stroke-width="2" fill="none"/>
        <circle cx="8.5" cy="8.5" r="1.5" fill="currentColor"/>
        <polyline points="21 15 16 10 5 21" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


def get_rebuild_icon(size: int = 24) -> QIcon: #vers 1
    """Rebuild/refresh icon"""
    svg_data = b'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path d="M17 10V7a5 5 0 00-10 0v3" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        <line x1="12" y1="17" x2="12" y2="21" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        <line x1="8" y1="21" x2="16" y2="21" 
            stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        <path d="M8 14v-4a4 4 0 018 0v4" 
            stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>'''
    return svg_to_icon(svg_data, size)


# Export all icon functions
__all__ = [
    'svg_to_icon',
    'get_img_file_icon',
    'get_col_file_icon',
    'get_txd_file_icon',
    'get_folder_icon',
    'get_file_icon',
    'get_package_icon',
    'get_shield_icon',
    'get_image_icon',
    'get_trash_icon',
    'get_refresh_icon',
    'get_tearoff_icon',
    'get_checkmark_icon',
    'get_palette_icon',
    'get_import_icon',
    'get_export_icon',
    'get_save_icon',
    'get_open_icon',
    'get_close_icon',
    'get_add_icon',
    'get_remove_icon',
    'get_edit_icon',
    'get_view_icon',
    'get_search_icon',
    'get_settings_icon',
    'get_info_icon',
    'get_warning_icon',
    'get_error_icon',
    'get_success_icon',
    'get_app_icon',
    'get_rebuild_icon',
]
