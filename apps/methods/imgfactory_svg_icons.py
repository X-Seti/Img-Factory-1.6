#!/usr/bin/env python3
#this belongs in apps/methods/img_svg_icons.py - Version: 7
# X-Seti - December17 2025 - Img Factory - Standardized SVG Icons

"""
All icons use consistent format: viewBox only, no fixed dimensions
Scales cleanly from 22x22 to 256x256 - Theme-aware with color parameter support
"""

from PyQt6.QtSvg import QSvgRenderer
from PyQt6.QtGui import QPixmap, QPainter, QIcon, QColor
from PyQt6.QtCore import Qt

##Methods list -
# add_icon
# arrow_down_icon
# arrow_left_icon
# arrow_right_icon
# arrow_up_icon
# backface_icon
# box_icon
# checkerboard_icon
# chip_icon
# col_workshop_icon
# close_icon
# color_picker_icon
# compress_icon
# controller_icon
# convert_icon
# copy_icon
# database_icon
# delete_icon
# edit_icon
# export_icon
# file_icon
# fit_icon
# flip_horz_icon
# flip_vert_icon
# folder_icon
# globe_icon
# import_icon
# info_icon
# launch_icon
# manage_icon
# maximize_icon
# mel_app_icon
# mesh_icon
# minimize_icon
# open_icon
# package_icon
# paint_icon
# paste_icon
# pause_icon
# properties_icon
# record_icon
# reset_icon
# rotate_ccw_icon
# rotate_cw_icon
# save_icon
# saveas_icon
# screenshot_icon
# search_icon
# settings_icon
# sphere_icon
# stop_icon
# txd_workshop_icon
# trash_icon
# uncompress_icon
# undo_icon
# view_icon
# volume_down_icon
# volume_up_icon
# zoom_in_icon
# zoom_out_icon
# _create_icon

##class SVGIconFactory -


class SVGIconFactory: #vers 7
    """Factory class for creating theme-aware scalable SVG icons"""


    @staticmethod
    def _create_icon(svg_data: str, size: int = 20, color: str = None) -> QIcon:
        """Create QIcon from SVG data with theme color support"""
        if color is None:
            # Use cached theme color or fallback - avoid circular import
            if hasattr(SVGIconFactory, "_cached_color"):
                color = SVGIconFactory._cached_color
            else:
                color = "#ffffff"  # Fallback
        
        svg_data = svg_data.replace("currentColor", color)
        
        try:
            renderer = QSvgRenderer(svg_data.encode())
            if not renderer.isValid():
                return QIcon()
            
            pixmap = QPixmap(size, size)
            pixmap.fill(Qt.GlobalColor.transparent)
            painter = QPainter(pixmap)
            renderer.render(painter)
            painter.end()
            return QIcon(pixmap)
        except Exception as e:
            print(f"Error: {e}")
            return QIcon()
    
    @staticmethod
    def set_theme_color(color: str):
        """Set cached theme color for icons"""
        SVGIconFactory._cached_color = color


    @staticmethod
    def _createicon(svg_data: str, size: int = 20, color: str = None) -> QIcon: #vers 7
        """
        Create QIcon from SVG data with theme color support

        Args:
            svg_data: SVG string with 'currentColor' placeholders
            size: Icon size in pixels (22-256, default 20)
            color: Hex color for icon (e.g. '#ffffff', '#000000')
                   If None, uses currentColor (theme-aware)
        """
        from PyQt6.QtGui import QIcon, QPixmap, QPainter, QColor
        from PyQt6.QtSvg import QSvgRenderer
        from PyQt6.QtCore import QByteArray
        if color:
            svg_data = svg_data.replace('currentColor', color)

        try:
            # Get current text color from palette
            text_color = self.palette().color(self.foregroundRole())

            # Replace currentColor with actual color
            svg_str = svg_data.decode('utf-8')
            svg_str = svg_str.replace('currentColor', text_color.name())
            svg_data = svg_str.encode('utf-8')

            renderer = QSvgRenderer(QByteArray(svg_data))
            pixmap = QPixmap(size, size)
            pixmap.fill(QColor(0, 0, 0, 0))  # Transparent background

            painter = QPainter(pixmap)
            renderer.render(painter)
            painter.end()

            return QIcon(pixmap)
        except:
            # Fallback to no icon if SVG fails
            return QIcon()

        try:
            renderer = QSvgRenderer(svg_data.encode())
            if not renderer.isValid():
                print(f"Invalid SVG data in icon creation")
                return QIcon()


            pixmap = QPixmap(size, size)
            pixmap.fill(Qt.GlobalColor.transparent)

            painter = QPainter(pixmap)
            renderer.render(painter)
            painter.end()
            return QIcon(pixmap)
        except Exception as e:
            print(f"Error creating icon: {e}")
            return QIcon()



# - PLAYBACK CONTROL ICONS

    
    @staticmethod
    def launch_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Play/Launch icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor" d="M8 5v14l11-7z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def stop_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Stop icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <rect x="6" y="6" width="12" height="12" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def pause_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Pause icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <rect x="6" y="5" width="4" height="14" fill="currentColor"/>
            <rect x="14" y="5" width="4" height="14" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

# - FILE & FOLDER ICONS

    @staticmethod
    def folder_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Folder icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M10,4H4C2.89,4 2,4.89 2,6V18A2,2 0 0,0 4,20H20A2,2 0 0,0 22,18V8C22,6.89 21.1,6 20,6H12L10,4Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def save_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Save/floppy icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M15,9H5V5H15M12,19A3,3 0 0,1 9,16A3,3 0 0,1 12,13A3,3 0 0,1 15,16A3,3 0 0,1 12,19M17,3H5C3.89,3 3,3.9 3,5V19A2,2 0 0,0 5,21H19A2,2 0 0,0 21,19V7L17,3Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def saveas_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Save As icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M19 21H5a2 2 0 01-2-2V5a2 2 0 012-2h11l5 5v11a2 2 0 01-2 2z"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="17 21 17 13 7 13 7 21"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="7 3 7 8 15 8"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def file_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """File/document icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M14,2H6A2,2 0 0,0 4,4V20A2,2 0 0,0 6,22H18A2,2 0 0,0 20,20V8L14,2M18,20H6V4H13V9H18V20Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def open_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Open file icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M14 2v6h6M12 11v6M9 14l3 3 3-3"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

# - SETTINGS & CONFIGURATION ICONS


    @staticmethod
    def settings_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Settings gear icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M12,8A4,4 0 0,1 16,12A4,4 0 0,1 12,16A4,4 0 0,1 8,12A4,4 0 0,1 12,8M12,10A2,2 0 0,0 10,12A2,2 0 0,0 12,14A2,2 0 0,0 14,12A2,2 0 0,0 12,10M10,22C9.75,22 9.54,21.82 9.5,21.58L9.13,18.93C8.5,18.68 7.96,18.34 7.44,17.94L4.95,18.95C4.73,19.03 4.46,18.95 4.34,18.73L2.34,15.27C2.21,15.05 2.27,14.78 2.46,14.63L4.57,12.97L4.5,12L4.57,11L2.46,9.37C2.27,9.22 2.21,8.95 2.34,8.73L4.34,5.27C4.46,5.05 4.73,4.96 4.95,5.05L7.44,6.05C7.96,5.66 8.5,5.32 9.13,5.07L9.5,2.42C9.54,2.18 9.75,2 10,2H14C14.25,2 14.46,2.18 14.5,2.42L14.87,5.07C15.5,5.32 16.04,5.66 16.56,6.05L19.05,5.05C19.27,4.96 19.54,5.05 19.66,5.27L21.66,8.73C21.79,8.95 21.73,9.22 21.54,9.37L19.43,11L19.5,12L19.43,13L21.54,14.63C21.73,14.78 21.79,15.05 21.66,15.27L19.66,18.73C19.54,18.95 19.27,19.04 19.05,18.95L16.56,17.95C16.04,18.34 15.5,18.68 14.87,18.93L14.5,21.58C14.46,21.82 14.25,22 14,22H10M11.25,4L10.88,6.61C9.68,6.86 8.62,7.5 7.85,8.39L5.44,7.35L4.69,8.65L6.8,10.2C6.4,11.37 6.4,12.64 6.8,13.8L4.68,15.36L5.43,16.66L7.86,15.62C8.63,16.5 9.68,17.14 10.87,17.38L11.24,20H12.76L13.13,17.39C14.32,17.14 15.37,16.5 16.14,15.62L18.57,16.66L19.32,15.36L17.2,13.81C17.6,12.64 17.6,11.37 17.2,10.2L19.31,8.65L18.56,7.35L16.15,8.39C15.38,7.5 14.32,6.86 13.12,6.62L12.75,4H11.25Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def properties_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Properties/theme icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M12,15.5A3.5,3.5 0 0,1 8.5,12A3.5,3.5 0 0,1 12,8.5A3.5,3.5 0 0,1 15.5,12A3.5,3.5 0 0,1 12,15.5M19.43,12.97C19.47,12.65 19.5,12.33 19.5,12C19.5,11.67 19.47,11.34 19.43,11L21.54,9.37C21.73,9.22 21.78,8.95 21.66,8.73L19.66,5.27C19.54,5.05 19.27,4.96 19.05,5.05L16.56,6.05C16.04,5.66 15.5,5.32 14.87,5.07L14.5,2.42C14.46,2.18 14.25,2 14,2H10C9.75,2 9.54,2.18 9.5,2.42L9.13,5.07C8.5,5.32 7.96,5.66 7.44,6.05L4.95,5.05C4.73,4.96 4.46,5.05 4.34,5.27L2.34,8.73C2.21,8.95 2.27,9.22 2.46,9.37L4.57,11C4.53,11.34 4.5,11.67 4.5,12C4.5,12.33 4.53,12.65 4.57,12.97L2.46,14.63C2.27,14.78 2.21,15.05 2.34,15.27L4.34,18.73C4.46,18.95 4.73,19.03 4.95,18.95L7.44,17.94C7.96,18.34 8.5,18.68 9.13,18.93L9.5,21.58C9.54,21.82 9.75,22 10,22H14C14.25,22 14.46,21.82 14.5,21.58L14.87,18.93C15.5,18.67 16.04,18.34 16.56,17.94L19.05,18.95C19.27,19.03 19.54,18.95 19.66,18.73L21.66,15.27C21.78,15.05 21.73,14.78 21.54,14.63L19.43,12.97Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def paint_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Paint brush icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M20.71,4.63L19.37,3.29C19,2.9 18.35,2.9 17.96,3.29L9,12.25L11.75,15L20.71,6.04C21.1,5.65 21.1,5 20.71,4.63M7,14A3,3 0 0,0 4,17C4,18.31 2.84,19 2,19C2.92,20.22 4.5,21 6,21A4,4 0 0,0 10,17A3,3 0 0,0 7,14Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def manage_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Settings/manage icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M12,15.5A3.5,3.5 0 0,1 8.5,12A3.5,3.5 0 0,1 12,8.5A3.5,3.5 0 0,1 15.5,12A3.5,3.5 0 0,1 12,15.5M19.43,12.97C19.47,12.65 19.5,12.33 19.5,12C19.5,11.67 19.47,11.34 19.43,11L21.54,9.37C21.73,9.22 21.78,8.95 21.66,8.73L19.66,5.27C19.54,5.05 19.27,4.96 19.05,5.05L16.56,6.05C16.04,5.66 15.5,5.32 14.87,5.07L14.5,2.42C14.46,2.18 14.25,2 14,2H10C9.75,2 9.54,2.18 9.5,2.42L9.13,5.07C8.5,5.32 7.96,5.66 7.44,6.05L4.95,5.05C4.73,4.96 4.46,5.05 4.34,5.27L2.34,8.73C2.21,8.95 2.27,9.22 2.46,9.37L4.57,11C4.53,11.34 4.5,11.67 4.5,12C4.5,12.33 4.53,12.65 4.57,12.97L2.46,14.63C2.27,14.78 2.21,15.05 2.34,15.27L4.34,18.73C4.46,18.95 4.73,19.03 4.95,18.95L7.44,17.94C7.96,18.34 8.5,18.68 9.13,18.93L9.5,21.58C9.54,21.82 9.75,22 10,22H14C14.25,22 14.46,21.82 14.5,21.58L14.87,18.93C15.5,18.67 16.04,18.34 16.56,17.94L19.05,18.95C19.27,19.03 19.54,18.95 19.66,18.73L21.66,15.27C21.78,15.05 21.73,14.78 21.54,14.63L19.43,12.97Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def package_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Package/box icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M21,16.5C21,16.88 20.79,17.21 20.47,17.38L12.57,21.82C12.41,21.94 12.21,22 12,22C11.79,22 11.59,21.94 11.43,21.82L3.53,17.38C3.21,17.21 3,16.88 3,16.5V7.5C3,7.12 3.21,6.79 3.53,6.62L11.43,2.18C11.59,2.06 11.79,2 12,2C12.21,2 12.41,2.06 12.57,2.18L20.47,6.62C20.79,6.79 21,7.12 21,7.5V16.5M12,4.15L6.04,7.5L12,10.85L17.96,7.5L12,4.15M5,15.91L11,19.29V12.58L5,9.21V15.91M19,15.91V9.21L13,12.58V19.29L19,15.91Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


# - WINDOW CONTROL ICONS

    @staticmethod
    def info_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Info icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="10" stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M12 16v-4M12 8h.01" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def minimize_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Minimize icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <line x1="5" y1="12" x2="19" y2="12" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def maximize_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Maximize icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <rect x="5" y="5" width="14" height="14" stroke="currentColor" stroke-width="2" fill="none" rx="2"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def close_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Close icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <line x1="6" y1="6" x2="18" y2="18" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="18" y1="6" x2="6" y2="18" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

# - MEDIA CONTROLS
    
    @staticmethod
    def volume_up_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Volume up icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M14,3.23V5.29C16.89,6.15 19,8.83 19,12C19,15.17 16.89,17.84 14,18.7V20.77C18,19.86 21,16.28 21,12C21,7.72 18,4.14 14,3.23M16.5,12C16.5,10.23 15.5,8.71 14,7.97V16C15.5,15.29 16.5,13.76 16.5,12M3,9V15H7L12,20V4L7,9H3Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def volume_down_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Volume down icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M5,9V15H9L14,20V4L9,9M18.5,12C18.5,10.23 17.5,8.71 16,7.97V16C17.5,15.29 18.5,13.76 18.5,12Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def controller_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Game controller icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M6,9H8V11H10V13H8V15H6V13H4V11H6V9M18.5,9A1.5,1.5 0 0,1 20,10.5A1.5,1.5 0 0,1 18.5,12A1.5,1.5 0 0,1 17,10.5A1.5,1.5 0 0,1 18.5,9M15.5,12A1.5,1.5 0 0,1 17,13.5A1.5,1.5 0 0,1 15.5,15A1.5,1.5 0 0,1 14,13.5A1.5,1.5 0 0,1 15.5,12M17,5A7,7 0 0,1 24,12A7,7 0 0,1 17,19C15.04,19 13.27,18.2 12,16.9C10.73,18.2 8.96,19 7,19A7,7 0 0,1 0,12A7,7 0 0,1 7,5H17Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def chip_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Microchip/BIOS icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M6,4H18V5H21V7H18V9H21V11H18V13H21V15H18V17H21V19H18V20H6V19H3V17H6V15H3V13H6V11H3V9H6V7H3V5H6V4M11,15V18H13V15H11M15,15V18H17V15H15M7,15V18H9V15H7M7,6V9H9V6H7M7,10V13H9V10H7M11,10V13H13V10H11M15,10V13H17V10H15M11,6V9H13V6H11M15,6V9H17V6H15Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

# - ART & MANAGEMENT ICONS

    @staticmethod
    def search_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Search/detect/magnifying glass icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M9.5,3A6.5,6.5 0 0,1 16,9.5C16,11.11 15.41,12.59 14.44,13.73L14.71,14H15.5L20.5,19L19,20.5L14,15.5V14.71L13.73,14.44C12.59,15.41 11.11,16 9.5,16A6.5,6.5 0 0,1 3,9.5A6.5,6.5 0 0,1 9.5,3M9.5,5C7,5 5,7 5,9.5C5,12 7,14 9.5,14C12,14 14,12 14,9.5C14,7 12,5 9.5,5Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def database_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Database icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M12,3C7.58,3 4,4.79 4,7C4,9.21 7.58,11 12,11C16.42,11 20,9.21 20,7C20,4.79 16.42,3 12,3M4,9V12C4,14.21 7.58,16 12,16C16.42,16 20,14.21 20,12V9C20,11.21 16.42,13 12,13C7.58,13 4,11.21 4,9M4,14V17C4,19.21 7.58,21 12,21C16.42,21 20,19.21 20,17V14C20,16.21 16.42,18 12,18C7.58,18 4,16.21 4,14Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def screenshot_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Screenshot/camera icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M4,4H7L9,2H15L17,4H20A2,2 0 0,1 22,6V18A2,2 0 0,1 20,20H4A2,2 0 0,1 2,18V6A2,2 0 0,1 4,4M12,7A5,5 0 0,0 7,12A5,5 0 0,0 12,17A5,5 0 0,0 17,12A5,5 0 0,0 12,7M12,9A3,3 0 0,1 15,12A3,3 0 0,1 12,15A3,3 0 0,1 9,12A3,3 0 0,1 12,9Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def record_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Record icon - always red, ignores color parameter"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="8" fill="#FF3333"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, None)
    

# - EDIT & TRANSFORM ICONS

    @staticmethod # Added from Img Factory
    def editer_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create edit SVG icon"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M11 4H4a2 2 0 00-2 2v14a2 2 0 002 2h14a2 2 0 002-2v-7" stroke="currentColor" stroke-width="2"/>
            <path d="M18.5 2.5a2.121 2.121 0 013 3L12 15l-4 1 1-4 9.5-9.5z" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def edit_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Edit/pencil icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M11 4H4a2 2 0 00-2 2v14a2 2 0 002 2h14a2 2 0 002-2v-7M18.5 2.5a2.121 2.121 0 013 3L12 15l-4 1 1-4 9.5-9.5z"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def copy_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Copy icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <rect x="9" y="9" width="13" height="13" rx="2"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M5 15H4a2 2 0 01-2-2V4a2 2 0 012-2h9a2 2 0 012 2v1"
                stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def paste_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Paste icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M16 4h2a2 2 0 012 2v14a2 2 0 01-2 2H6a2 2 0 01-2-2V6a2 2 0 012-2h2"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <rect x="8" y="2" width="8" height="4" rx="1"
                stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def add_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Add/plus icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor" d="M19,13H13V19H11V13H5V11H11V5H13V11H19V13Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def _add_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Add - Plus icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <line x1="12" y1="5" x2="12" y2="19"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
            <line x1="5" y1="12" x2="19" y2="12"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _delete_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Delete - Trash icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <polyline points="3 6 5 6 21 6"
                    stroke="currentColor" stroke-width="2"
                    fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M19 6v14a2 2 0 01-2 2H7a2 2 0 01-2-2V6m3 0V4a2 2 0 012-2h4a2 2 0 012 2v2"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def delete_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Delete/minus icon"""
        svg_data =  '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M3 5h14M8 5V3h4v2M6 5v11a1 1 0 001 1h6a1 1 0 001-1V5" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def trash_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Trash/delete icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <polyline points="3 6 5 6 21 6"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M19 6v14a2 2 0 01-2 2H7a2 2 0 01-2-2V6m3 0V4a2 2 0 012-2h4a2 2 0 012 2v2"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _bin_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Delete - Trash icon"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M3 6h18M19 6v14a2 2 0 01-2 2H7a2 2 0 01-2-2V6M8 6V4a2 2 0 012-2h4a2 2 0 012 2v2" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def undo_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Undo icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M3 7v6h6M3 13a9 9 0 1018 0 9 9 0 00-18 0z"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

# - ROTATION & FLIP ICONS

    @staticmethod
    def rotate_cw_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Rotate clockwise icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21 12a9 9 0 11-9-9v6M21 3l-3 6-6-3"
                stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def rotate_ccw_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Rotate counter-clockwise icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M3 12a9 9 0 109-9v6M3 3l3 6 6-3"
                stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def flip_horz_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Flip horizontal icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M12 3v18M7 8l5-4 5 4M7 16l5 4 5-4"
                stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def flip_vert_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Flip vertical icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M3 12h18M8 7l-4 5 4 5M16 7l4 5-4 5"
                stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

# - IMPORT/EXPORT ICONS
    
    @staticmethod
    def import_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Import/download icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4M7 10l5 5 5-5M12 15V3"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def export_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Export/upload icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4M17 8l-5-5-5 5M12 3v12"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def convert_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Convert/transform icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21 10c0-1.1-.9-2-2-2h-6.3L15 5.7C15.4 5.3 15.4 4.7 15 4.3L13.7 3 9 7.7V5c0-1.1-.9-2-2-2H3c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h4c1.1 0 2-.9 2-2v-2.7l4.7 4.7 1.3-1.3c.4-.4.4-1 0-1.4L12.7 16H19c1.1 0 2-.9 2-2v-4z"
                fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _convertor_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create convert SVG icon"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M3 12h18M3 12l4-4M3 12l4 4M21 12l-4-4M21 12l-4 4" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <circle cx="12" cy="12" r="3" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


# - VIEW & ZOOM ICONS

    @staticmethod
    def view_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """View/eye icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M12,9A3,3 0 0,0 9,12A3,3 0 0,0 12,15A3,3 0 0,0 15,12A3,3 0 0,0 12,9M12,17A5,5 0 0,1 7,12A5,5 0 0,1 12,7A5,5 0 0,1 17,12A5,5 0 0,1 12,17M12,4.5C7,4.5 2.73,7.61 1,12C2.73,16.39 7,19.5 12,19.5C17,19.5 21.27,16.39 23,12C21.27,7.61 17,4.5 12,4.5Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def _viewer_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create view/eye icon"""
        svg_data = b'''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M12,9A3,3 0 0,0 9,12A3,3 0 0,0 12,15A3,3 0 0,0 15,12A3,3 0 0,0 12,9
                    M12,17A5,5 0 0,1 7,12A5,5 0 0,1 12,7A5,5 0 0,1 17,12A5,5 0 0,1 12,17
                    M12,4.5C7,4.5 2.73,7.61 1,12C2.73,16.39 7,19.5 12,19.5
                    C17,19.5 21.27,16.39 23,12
                    C21.27,7.61 17,4.5 12,4.5Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def zoom_in_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Zoom in icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="11" cy="11" r="8"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M11 8v6M8 11h6"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <path d="M21 21l-4.35-4.35"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def zoom_out_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Zoom out icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="11" cy="11" r="8"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M8 11h6"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <path d="M21 21l-4.35-4.35"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def reset_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Reset/refresh icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M16 10A6 6 0 1 1 4 10M4 10l3-3m-3 3l3 3"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def fit_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Fit to window icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <rect x="3" y="3" width="18" height="18"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M7 7l10 10M17 7L7 17"
                stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

# - 3D VIEW ICONS
    
    @staticmethod
    def sphere_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Sphere collision icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="10"
                stroke="currentColor" stroke-width="2"
                fill="none"/>
            <path d="M2 12h20M12 2a15.3 15.3 0 014 10 15.3 15.3 0 01-4 10 15.3 15.3 0 01-4-10 15.3 15.3 0 014-10z"
                stroke="currentColor" stroke-width="2"
                fill="none"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def box_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Box collision icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21 16V8a2 2 0 00-1-1.73l-7-4a2 2 0 00-2 0l-7 4A2 2 0 003 8v8a2 2 0 001 1.73l7 4a2 2 0 002 0l7-4A2 2 0 0021 16z"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="3.27 6.96 12 12.01 20.73 6.96"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="22.08" x2="12" y2="12"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def mesh_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Mesh/wireframe icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <rect x="3" y="3" width="18" height="18"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="3" y1="9" x2="21" y2="9"
                stroke="currentColor" stroke-width="2"/>
            <line x1="3" y1="15" x2="21" y2="15"
                stroke="currentColor" stroke-width="2"/>
            <line x1="9" y1="3" x2="9" y2="21"
                stroke="currentColor" stroke-width="2"/>
            <line x1="15" y1="3" x2="15" y2="21"
                stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def backface_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Backface culling icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M12 4 L20 8 L16 16 L8 16 L4 8 Z"
                fill="currentColor" opacity="0.8"/>
            <path d="M12 4 L8 16 M12 4 L16 16"
                stroke="currentColor"
                stroke-width="1.5"
                stroke-dasharray="2,2"
                opacity="0.3"
                fill="none"/>
            <path d="M4 8 L12 4 L20 8 L16 16 L8 16 Z"
                stroke="currentColor"
                stroke-width="1.5"
                fill="none"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def globe_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Globe/world icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="10"
                stroke="currentColor" stroke-width="2"
                fill="none"/>
            <path d="M2 12h20M12 2a15.3 15.3 0 014 10 15.3 15.3 0 01-4 10 15.3 15.3 0 01-4-10 15.3 15.3 0 014-10z"
                stroke="currentColor" stroke-width="2"
                fill="none"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

# - ARROW ICONS

    @staticmethod
    def arrow_up_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Arrow up"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M12 5v14M6 11l6-6 6 6"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    
    @staticmethod
    def arrow_down_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Arrow down"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M12 19V5M18 13l-6 6-6-6"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    
    @staticmethod
    def arrow_left_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Arrow left"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M5 12h14M11 6l-6 6 6 6"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    
    @staticmethod
    def arrow_right_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Arrow right"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M19 12H5M13 18l6-6-6-6"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

# - UTILITY ICONS

    @staticmethod
    def analyze_icon(size: int = 20, color: str = None) -> QIcon: #vers 1
        """Analyze/chart icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <line x1="18" y1="20" x2="18" y2="10"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="12" y1="20" x2="12" y2="4"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="6" y1="20" x2="6" y2="14"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def color_picker_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Color picker icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="10"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M12 4v6M12 14v6M4 12h6M14 12h6"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod #added from img Factory
    def _colour_picker_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Color picker icon"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none"/>
            <circle cx="10" cy="10" r="7" stroke="currentColor" stroke-width="2"/>
            <path d="M10 3v4M10 13v4M3 10h4M13 10h4" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def checkerboard_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Checkerboard pattern icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <rect x="0" y="0" width="6" height="6" fill="currentColor"/>
            <rect x="6" y="6" width="6" height="6" fill="currentColor"/>
            <rect x="12" y="0" width="6" height="6" fill="currentColor"/>
            <rect x="18" y="6" width="6" height="6" fill="currentColor"/>
            <rect x="0" y="12" width="6" height="6" fill="currentColor"/>
            <rect x="6" y="18" width="6" height="6" fill="currentColor"/>
            <rect x="12" y="12" width="6" height="6" fill="currentColor"/>
            <rect x="18" y="18" width="6" height="6" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    
    @staticmethod
    def checkerpat_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Checker pattern at 45 degree angle"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <g transform="rotate(45 12 12)">
                <rect x="6" y="6" width="4" height="4" fill="currentColor"/>
                <rect x="14" y="6" width="4" height="4" fill="currentColor"/>
                <rect x="10" y="10" width="4" height="4" fill="currentColor"/>
                <rect x="6" y="14" width="4" height="4" fill="currentColor"/>
                <rect x="14" y="14" width="4" height="4" fill="currentColor"/>
            </g>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surfaceedit_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Grid mesh with edit point"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M3 3h18v18H3z" stroke="currentColor" stroke-width="1" fill="none"/>
            <line x1="3" y1="9" x2="21" y2="9" stroke="currentColor" stroke-width="0.5"/>
            <line x1="3" y1="15" x2="21" y2="15" stroke="currentColor" stroke-width="0.5"/>
            <line x1="9" y1="3" x2="9" y2="21" stroke="currentColor" stroke-width="0.5"/>
            <line x1="15" y1="3" x2="15" y2="21" stroke="currentColor" stroke-width="0.5"/>
            <circle cx="12" cy="12" r="2" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surfaceedit_icon_b(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Checkerboard with highlighted square"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <rect x="3" y="3" width="6" height="6" fill="currentColor" opacity="0.3"/>
            <rect x="15" y="3" width="6" height="6" fill="currentColor" opacity="0.3"/>
            <rect x="9" y="9" width="6" height="6" fill="currentColor"/>
            <rect x="3" y="15" width="6" height="6" fill="currentColor" opacity="0.3"/>
            <rect x="15" y="15" width="6" height="6" fill="currentColor" opacity="0.3"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surfaceedit_icon_c(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Wireframe surface with nodes"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M4 8 Q12 4, 20 8" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <path d="M4 12 Q12 10, 20 12" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <path d="M4 16 Q12 14, 20 16" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <circle cx="4" cy="8" r="1.5" fill="currentColor"/>
            <circle cx="12" cy="6" r="1.5" fill="currentColor"/>
            <circle cx="20" cy="8" r="1.5" fill="currentColor"/>
            <circle cx="12" cy="12" r="1.5" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surfaceedit_icon_d(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Layered surfaces"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <rect x="4" y="4" width="16" height="4" fill="currentColor" opacity="0.3"/>
            <rect x="6" y="10" width="16" height="4" fill="currentColor" opacity="0.6"/>
            <rect x="4" y="16" width="16" height="4" fill="currentColor"/>
            <path d="M20 12 L22 12 L20 14 L18 12 L20 12" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    #surface types
    @staticmethod
    def surface_grass_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Grass/vegetation surface"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M4 20 L4 14 Q4 10, 8 10" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <path d="M8 20 L8 12 Q8 8, 12 8" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <path d="M12 20 L12 10 Q12 6, 16 6" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <path d="M16 20 L16 14 Q16 10, 20 10" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <line x1="2" y1="20" x2="22" y2="20" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_concrete_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Concrete/stone surface"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <rect x="3" y="8" width="18" height="10" fill="currentColor" opacity="0.3"/>
            <circle cx="7" cy="11" r="1" fill="currentColor"/>
            <circle cx="13" cy="13" r="0.8" fill="currentColor"/>
            <circle cx="17" cy="10" r="0.6" fill="currentColor"/>
            <circle cx="10" cy="15" r="0.7" fill="currentColor"/>
            <circle cx="19" cy="15" r="0.5" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_metal_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Metal surface"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <rect x="3" y="8" width="18" height="8" fill="currentColor" opacity="0.5"/>
            <line x1="3" y1="10" x2="21" y2="10" stroke="currentColor" stroke-width="0.5" opacity="0.8"/>
            <line x1="3" y1="12" x2="21" y2="12" stroke="currentColor" stroke-width="1" opacity="1"/>
            <line x1="3" y1="14" x2="21" y2="14" stroke="currentColor" stroke-width="0.5" opacity="0.8"/>
            <circle cx="6" cy="12" r="0.8" fill="currentColor"/>
            <circle cx="18" cy="12" r="0.8" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_wood_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Wood surface"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <rect x="3" y="8" width="18" height="8" fill="currentColor" opacity="0.3"/>
            <path d="M5 8 Q8 10, 5 12 Q8 14, 5 16" stroke="currentColor" stroke-width="0.8" fill="none"/>
            <path d="M10 8 Q13 10, 10 12 Q13 14, 10 16" stroke="currentColor" stroke-width="0.8" fill="none"/>
            <path d="M15 8 Q18 10, 15 12 Q18 14, 15 16" stroke="currentColor" stroke-width="0.8" fill="none"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_water_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Water surface"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M2 12 Q5 9, 8 12 T14 12 T20 12 T26 12" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <path d="M2 15 Q5 13, 8 15 T14 15 T20 15 T26 15" stroke="currentColor" stroke-width="1.5" fill="none" opacity="0.6"/>
            <path d="M2 18 Q5 17, 8 18 T14 18 T20 18 T26 18" stroke="currentColor" stroke-width="1" fill="none" opacity="0.3"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_sand_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Sand/dirt surface"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <rect x="3" y="10" width="18" height="8" fill="currentColor" opacity="0.2"/>
            <circle cx="5" cy="12" r="0.5" fill="currentColor"/>
            <circle cx="8" cy="14" r="0.6" fill="currentColor"/>
            <circle cx="11" cy="11" r="0.4" fill="currentColor"/>
            <circle cx="14" cy="15" r="0.5" fill="currentColor"/>
            <circle cx="17" cy="13" r="0.6" fill="currentColor"/>
            <circle cx="20" cy="16" r="0.4" fill="currentColor"/>
            <circle cx="7" cy="16" r="0.5" fill="currentColor"/>
            <circle cx="13" cy="13" r="0.4" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_glass_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Glass surface"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <rect x="4" y="6" width="16" height="12" fill="currentColor" opacity="0.1" stroke="currentColor" stroke-width="1.5"/>
            <line x1="8" y1="8" x2="10" y2="10" stroke="currentColor" stroke-width="1" opacity="0.6"/>
            <line x1="14" y1="12" x2="16" y2="14" stroke="currentColor" stroke-width="0.8" opacity="0.4"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_rubber_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Rubber/tire surface"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <rect x="3" y="9" width="18" height="7" fill="currentColor" opacity="0.4" rx="1"/>
            <line x1="6" y1="11" x2="6" y2="14" stroke="currentColor" stroke-width="1.5"/>
            <line x1="10" y1="11" x2="10" y2="14" stroke="currentColor" stroke-width="1.5"/>
            <line x1="14" y1="11" x2="14" y2="14" stroke="currentColor" stroke-width="1.5"/>
            <line x1="18" y1="11" x2="18" y2="14" stroke="currentColor" stroke-width="1.5"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_hex_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Hexagon/honeycomb pattern"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M12 4 L15 6 L15 10 L12 12 L9 10 L9 6 Z" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <path d="M6 8 L9 10 L9 14 L6 16 L3 14 L3 10 Z" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
            <path d="M18 8 L21 10 L21 14 L18 16 L15 14 L15 10 Z" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
            <path d="M12 12 L15 14 L15 18 L12 20 L9 18 L9 14 Z" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_triangle_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Triangle pattern"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M12 4 L18 14 L6 14 Z" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <path d="M6 14 L12 20 L3 20 Z" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
            <path d="M12 14 L18 20 L12 20 Z" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
            <path d="M18 14 L21 20 L18 20 Z" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_diamond_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Diamond pattern"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M12 4 L18 10 L12 16 L6 10 Z" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <path d="M12 16 L16 20 L12 20 L8 20 Z" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
            <path d="M4 10 L6 10 L4 12 L2 10 Z" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
            <path d="M18 10 L20 10 L18 12 L18 10 Z" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_circle_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Circle pattern"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <circle cx="12" cy="12" r="5" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <circle cx="6" cy="6" r="2" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
            <circle cx="18" cy="6" r="2" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
            <circle cx="6" cy="18" r="2" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
            <circle cx="18" cy="18" r="2" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_square_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Square grid pattern"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <rect x="7" y="7" width="10" height="10" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <rect x="3" y="3" width="6" height="6" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
            <rect x="15" y="3" width="6" height="6" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
            <rect x="3" y="15" width="6" height="6" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
            <rect x="15" y="15" width="6" height="6" stroke="currentColor" stroke-width="1" fill="none" opacity="0.6"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_brick_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Brick pattern"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <rect x="3" y="6" width="8" height="4" stroke="currentColor" stroke-width="1" fill="none"/>
            <rect x="13" y="6" width="8" height="4" stroke="currentColor" stroke-width="1" fill="none"/>
            <rect x="3" y="11" width="5" height="4" stroke="currentColor" stroke-width="1" fill="none"/>
            <rect x="9" y="11" width="6" height="4" stroke="currentColor" stroke-width="1" fill="none"/>
            <rect x="16" y="11" width="5" height="4" stroke="currentColor" stroke-width="1" fill="none"/>
            <rect x="3" y="16" width="8" height="4" stroke="currentColor" stroke-width="1" fill="none"/>
            <rect x="13" y="16" width="8" height="4" stroke="currentColor" stroke-width="1" fill="none"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_wave_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Wave pattern"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M2 8 Q4 6, 6 8 T10 8 T14 8 T18 8 T22 8" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <path d="M2 12 Q4 10, 6 12 T10 12 T14 12 T18 12 T22 12" stroke="currentColor" stroke-width="1.5" fill="none"/>
            <path d="M2 16 Q4 14, 6 16 T10 16 T14 16 T18 16 T22 16" stroke="currentColor" stroke-width="1.5" fill="none"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_dot_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Dot pattern"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <circle cx="6" cy="6" r="1.5" fill="currentColor"/>
            <circle cx="12" cy="6" r="1.5" fill="currentColor"/>
            <circle cx="18" cy="6" r="1.5" fill="currentColor"/>
            <circle cx="6" cy="12" r="1.5" fill="currentColor"/>
            <circle cx="12" cy="12" r="1.5" fill="currentColor"/>
            <circle cx="18" cy="12" r="1.5" fill="currentColor"/>
            <circle cx="6" cy="18" r="1.5" fill="currentColor"/>
            <circle cx="12" cy="18" r="1.5" fill="currentColor"/>
            <circle cx="18" cy="18" r="1.5" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_stripe_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Stripe pattern"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <line x1="2" y1="6" x2="22" y2="6" stroke="currentColor" stroke-width="2"/>
            <line x1="2" y1="10" x2="22" y2="10" stroke="currentColor" stroke-width="2"/>
            <line x1="2" y1="14" x2="22" y2="14" stroke="currentColor" stroke-width="2"/>
            <line x1="2" y1="18" x2="22" y2="18" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def surface_cross_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Cross/plus pattern"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <line x1="12" y1="4" x2="12" y2="20" stroke="currentColor" stroke-width="1.5"/>
            <line x1="4" y1="12" x2="20" y2="12" stroke="currentColor" stroke-width="1.5"/>
            <line x1="6" y1="4" x2="6" y2="8" stroke="currentColor" stroke-width="1" opacity="0.6"/>
            <line x1="18" y1="4" x2="18" y2="8" stroke="currentColor" stroke-width="1" opacity="0.6"/>
            <line x1="6" y1="16" x2="6" y2="20" stroke="currentColor" stroke-width="1" opacity="0.6"/>
            <line x1="18" y1="16" x2="18" y2="20" stroke="currentColor" stroke-width="1" opacity="0.6"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    #Missing Icons

    @staticmethod
    def _place_icon(size: int = 24, color: str = None) -> QIcon: #vers 7
        """Create/new icon"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M10 4v12M4 10h12" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def duplicate_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Duplicate/copy icon"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <rect x="6" y="6" width="10" height="10" stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M4 4h8v2H6v8H4V4z" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def check_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create check/verify icon - document with checkmark"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z"
                fill="none" stroke="currentColor" stroke-width="2"/>
            <path d="M14 2v6h6"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M9 13l2 2 4-4"
                stroke="currentColor" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _bitdepth_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create bit depth icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M3,5H9V11H3V5M5,7V9H7V7H5M11,7H21V9H11V7M11,15H21V17H11V15M5,20L1.5,16.5L2.91,15.09L5,17.17L9.59,12.59L11,14L5,20Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _resize_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create resize icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M10,21V19H6.41L10.91,14.5L9.5,13.09L5,17.59V14H3V21H10M14.5,10.91L19,6.41V10H21V3H14V5H17.59L13.09,9.5L14.5,10.91Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _warning_icon_svg(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create SVG warning icon for table display"""
        svg_data = """
        <svg width="16" height="16" viewBox="0 0 16 16">
            <path fill="#FFA500" d="M8 1l7 13H1z"/>
            <text x="8" y="12" font-size="10" fill="black" text-anchor="middle">!</text>
        </svg>
        """
        return QIcon(QPixmap.fromImage(
            QImage.fromData(QByteArray(svg_data))
        ))


    @staticmethod
    def _resize_icon2(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Resize grip icon - diagonal arrows"""
        svg_data = '''<svg viewBox="0 0 20 20" fill="none">
            <path d="M14 6l-8 8M10 6h4v4M6 14v-4h4" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _upscale_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create AI upscale icon - brain/intelligence style"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <!-- Brain outline -->
            <path d="M12 3 C8 3 5 6 5 9 C5 10 5.5 11 6 12 C5.5 13 5 14 5 15 C5 18 8 21 12 21 C16 21 19 18 19 15 C19 14 18.5 13 18 12 C18.5 11 19 10 19 9 C19 6 16 3 12 3 Z"
                fill="none" stroke="currentColor" stroke-width="1.5"/>

            <!-- Neural pathways inside -->
            <path d="M9 8 L10 10 M14 8 L13 10 M10 12 L14 12 M9 14 L12 16 M15 14 L12 16"
                stroke="currentColor" stroke-width="1" fill="none"/>

            <!-- Upward indicator -->
            <path d="M19 8 L19 4 M17 6 L19 4 L21 6"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _upscaleb_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create AI upscale icon - sparkle/magic AI style"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <!-- Large sparkle -->
            <path d="M12 2 L13 8 L12 14 L11 8 Z M8 12 L2 11 L8 10 L14 11 Z"
                fill="currentColor"/>

            <!-- Small sparkles -->
            <circle cx="18" cy="6" r="1.5" fill="currentColor"/>
            <circle cx="6" cy="18" r="1.5" fill="currentColor"/>
            <circle cx="19" cy="16" r="1" fill="currentColor"/>

            <!-- Upward arrow -->
            <path d="M16 20 L20 20 M18 18 L18 22"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _upscalec_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create AI upscale icon - neural network style"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <!-- Neural network nodes -->
            <circle cx="6" cy="6" r="2" fill="currentColor"/>
            <circle cx="18" cy="6" r="2" fill="currentColor"/>
            <circle cx="6" cy="18" r="2" fill="currentColor"/>
            <circle cx="18" cy="18" r="2" fill="currentColor"/>
            <circle cx="12" cy="12" r="2.5" fill="currentColor"/>

            <!-- Connecting lines -->
            <path d="M7.5 7.5 L10.5 10.5 M13.5 10.5 L16.5 7.5 M7.5 16.5 L10.5 13.5 M13.5 13.5 L16.5 16.5"
                stroke="currentColor" stroke-width="1.5" fill="none"/>

            <!-- Upward arrow overlay -->
            <path d="M12 3 L12 9 M9 6 L12 3 L15 6"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def uncompress_icon(size: int = 20, color: str = None) -> QIcon: #vers 7
        """Uncompress icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor" d="M11,4V2H13V4H11M13,21V19H11V21H13M4,12V10H20V12H4Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)

    @staticmethod
    def compress_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create compress icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M4,2H20V4H13V10H20V12H4V10H11V4H4V2M4,13H20V15H13V21H20V23H4V21H11V15H4V13Z"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def build_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create build/construct icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M22,9 L12,2 L2,9 L12,16 L22,9 Z M12,18 L4,13 L4,19 L12,24 L20,19 L20,13 L12,18 Z"
                fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _sphere_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Sphere - Circle icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="10"
                stroke="currentColor" stroke-width="2"
                fill="none"/>
            <path d="M2 12h20M12 2a15.3 15.3 0 014 10 15.3 15.3 0 01-4 10 15.3 15.3 0 01-4-10 15.3 15.3 0 014-10z"
                stroke="currentColor" stroke-width="2"
                fill="none"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _box_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Box - Cube icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21 16V8a2 2 0 00-1-1.73l-7-4a2 2 0 00-2 0l-7 4A2 2 0 003 8v8a2 2 0 001 1.73l7 4a2 2 0 002 0l7-4A2 2 0 0021 16z"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="3.27 6.96 12 12.01 20.73 6.96"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="22.08" x2="12" y2="12"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _mesh_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Mesh - Grid/wireframe icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <rect x="3" y="3" width="18" height="18"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="3" y1="9" x2="21" y2="9"
                stroke="currentColor" stroke-width="2"/>
            <line x1="3" y1="15" x2="21" y2="15"
                stroke="currentColor" stroke-width="2"/>
            <line x1="9" y1="3" x2="9" y2="21"
                stroke="currentColor" stroke-width="2"/>
            <line x1="15" y1="3" x2="15" y2="21"
                stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _wireframe_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Wireframe mode icon"""
        svg_data = '''<svg viewBox="0 0 20 20" fill="none">
            <path d="M5 5 L15 5 L15 15 L5 15 Z" stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M5 10 L15 10 M10 5 L10 15" stroke="currentColor" stroke-width="1.5"/>
            <circle cx="5" cy="5" r="1.5" fill="currentColor"/>
            <circle cx="15" cy="5" r="1.5" fill="currentColor"/>
            <circle cx="15" cy="15" r="1.5" fill="currentColor"/>
            <circle cx="5" cy="15" r="1.5" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _bounds_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Bounding box icon"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <rect x="3" y="3" width="14" height="14" stroke="currentColor" stroke-width="2" fill="none" stroke-dasharray="3,2"/>
            <path d="M3 3 L7 3 M17 3 L13 3 M3 17 L7 17 M17 17 L13 17 M3 3 L3 7 M3 17 L3 13 M17 3 L17 7 M17 17 L17 13" stroke="currentColor" stroke-width="2.5" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _reset_view_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Reset camera view icon"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M16 10A6 6 0 1 1 4 10M4 10l3-3m-3 3l3 3" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


# - CONTEXT MENU ICONS

    @staticmethod
    def _create_plus_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create New Entry - Plus icon"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <circle cx="12" cy="12" r="10" stroke="currentColor" stroke-width="2"/>
            <path d="M12 8v8M8 12h8" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _document_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Create New col - Document icon"""
        svg_data ='''<svg viewBox="0 0 24 24" fill="none">
            <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z" stroke="currentColor" stroke-width="2"/>
            <path d="M14 2v6h6M16 13H8M16 17H8M10 9H8" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _filter_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Filter/sliders icon"""
        svg_data = '''<svg viewBox="0 0 20 20" fill="none">
            <circle cx="6" cy="4" r="2" fill="currentColor"/>
            <rect x="5" y="8" width="2" height="8" fill="currentColor"/>
            <circle cx="14" cy="12" r="2" fill="currentColor"/>
            <rect x="13" y="4" width="2" height="6" fill="currentColor"/>
            <circle cx="10" cy="8" r="2" fill="currentColor"/>
            <rect x="9" y="12" width="2" height="4" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _pencil_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Edit - Pencil icon"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M17 3a2.83 2.83 0 114 4L7.5 20.5 2 22l1.5-5.5L17 3z" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _create_eye_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """View - Eye icon"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z" stroke="currentColor" stroke-width="2"/>
            <circle cx="12" cy="12" r="3" stroke="currentColor" stroke-width="2"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _list_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Properties List - List icon"""
        svg_data = '''<svg viewBox="0 0 24 24" fill="none">
            <path d="M8 6h13M8 12h13M8 18h13M3 6h.01M3 12h.01M3 18h.01" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _import_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Import - Download arrow icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="7 10 12 15 17 10"
                    stroke="currentColor" stroke-width="2"
                    fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="15" x2="12" y2="3"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def _export_icon(size: int = 24, color: str = None) -> QIcon: #vers 2
        """Export - Upload arrow icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4"
                stroke="currentColor" stroke-width="2"
                fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="17 8 12 3 7 8"
                    stroke="currentColor" stroke-width="2"
                    fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="3" x2="12" y2="15"
                stroke="currentColor" stroke-width="2"
                stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


# - FILE TYPE ICONS (Replace emojis in tabs)

    @staticmethod
    def get_img_file_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """IMG archive icon - Replaces emoji"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21 16V8a2 2 0 00-1-1.73l-7-4a2 2 0 00-2 0l-7 4A2 2 0 003 8v8a2 2 0 001 1.73l7 4a2 2 0 002 0l7-4A2 2 0 0021 16z"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M3.27 6.96L12 12.01l8.73-5.05M12 22.08V12"
                stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <text x="12" y="15" font-size="6" fill="currentColor" text-anchor="middle" font-weight="bold">IMG</text>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_col_file_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """COL collision icon - Replaces emoji"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M12 2L4 6v6c0 5.5 3.8 10.7 8 12 4.2-1.3 8-6.5 8-12V6l-8-4z"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M8 12l2 2 6-6"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_txd_file_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """TXD texture icon - Replaces emoji"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <rect x="3" y="3" width="18" height="18" rx="2"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <circle cx="8.5" cy="8.5" r="1.5" fill="currentColor"/>
            <path d="M21 15l-5-5L5 21"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <text x="12" y="20" font-size="5" fill="currentColor" text-anchor="middle" font-weight="bold">TXD</text>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_folder_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Folder icon - Replaces emoji"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-7l-2-2H5a2 2 0 00-2 2z"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_file_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Generic file icon - Replaces emoji"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M14 2v6h6"
                stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


# - ACTION ICONS

    @staticmethod
    def get_trash_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Delete/trash icon - Replaces emoji"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <polyline points="3 6 5 6 21 6"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M19 6v14a2 2 0 01-2 2H7a2 2 0 01-2-2V6m3 0V4a2 2 0 012-2h4a2 2 0 012 2v2"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_refresh_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Refresh icon - Replaces emoji"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21.5 2v6h-6M2.5 22v-6h6M2 11.5a10 10 0 0117-7l2.5 2.5M22 12.5a10 10 0 01-17 7l-2.5-2.5"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_tearoff_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Tearoff/detach icon - Replaces emoji"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M18 13v6a2 2 0 01-2 2H5a2 2 0 01-2-2V8a2 2 0 012-2h6M15 3h6v6M10 14L21 3"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_checkmark_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Checkmark icon - Replaces ✓ emoji"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <polyline points="20 6 9 17 4 12"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_palette_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Theme/palette icon - Replaces emoji"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M12 2a10 10 0 00-9.95 11.1C2.5 17.7 6.3 21 10.9 21h1.2a2 2 0 002-2v-.3c0-.5.2-1 .6-1.3.4-.4.6-.9.6-1.4 0-1.1-.9-2-2-2h-1.4a8 8 0 110-10.3"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <circle cx="7.5" cy="10.5" r="1.5" fill="currentColor"/>
            <circle cx="12" cy="7.5" r="1.5" fill="currentColor"/>
            <circle cx="16.5" cy="10.5" r="1.5" fill="currentColor"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_import_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Import/download icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4M7 10l5 5 5-5M12 15V3"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_export_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Export/upload icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4M17 8l-5-5-5 5M12 3v12"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_save_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Save icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M19 21H5a2 2 0 01-2-2V5a2 2 0 012-2h11l5 5v11a2 2 0 01-2 2z"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M17 21v-8H7v8M7 3v5h8"
                stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_open_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Open file icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8l-6-6z"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M14 2v6h6M12 11v6M9 14l3 3 3-3"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_close_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Close/X icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <line x1="18" y1="6" x2="6" y2="18"
                stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="6" y1="6" x2="18" y2="18"
                stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_add_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Add/plus icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <line x1="12" y1="5" x2="12" y2="19"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="5" y1="12" x2="19" y2="12"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_remove_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Remove/minus icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <line x1="5" y1="12" x2="19" y2="12"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_edit_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Edit/pencil icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M11 4H4a2 2 0 00-2 2v14a2 2 0 002 2h14a2 2 0 002-2v-7M18.5 2.5a2.121 2.121 0 013 3L12 15l-4 1 1-4 9.5-9.5z"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_view_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """View/eye icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <circle cx="12" cy="12" r="3"
                stroke="currentColor" stroke-width="2" fill="none"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_search_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Search/magnifying glass icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="11" cy="11" r="8"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <line x1="21" y1="21" x2="16.65" y2="16.65"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_settings_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Settings/gear icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="3"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <path d="M12 1v6m0 6v6M5.6 5.6l4.2 4.2m4.4 4.4l4.2 4.2M1 12h6m6 0h6M5.6 18.4l4.2-4.2m4.4-4.4l4.2-4.2"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_info_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Info/information icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="10"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <line x1="12" y1="16" x2="12" y2="12"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="12" y1="8" x2="12.01" y2="8"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_warning_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Warning/alert triangle icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M10.29 3.86L1.82 18a2 2 0 001.71 3h16.94a2 2 0 001.71-3L13.71 3.86a2 2 0 00-3.42 0z"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="9" x2="12" y2="13"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="12" y1="17" x2="12.01" y2="17"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_error_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Error/X circle icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="10"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <line x1="15" y1="9" x2="9" y2="15"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="9" y1="9" x2="15" y2="15"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_success_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Success/checkmark circle icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="10"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <polyline points="9 12 11 14 15 10"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_package_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Package/box icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M21 16V8a2 2 0 00-1-1.73l-7-4a2 2 0 00-2 0l-7 4A2 2 0 003 8v8a2 2 0 001 1.73l7 4a2 2 0 002 0l7-4A2 2 0 0021 16z"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="3.27 6.96 12 12.01 20.73 6.96"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="22.08" x2="12" y2="12"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_shield_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Shield/protection icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_image_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Image/picture icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <rect x="3" y="3" width="18" height="18" rx="2" ry="2"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <circle cx="8.5" cy="8.5" r="1.5" fill="currentColor"/>
            <polyline points="21 15 16 10 5 21"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_rebuild_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
        """Rebuild/refresh icon"""
        svg_data = '''<svg viewBox="0 0 24 24">
            <path d="M17 10V7a5 5 0 00-10 0v3"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="17" x2="12" y2="21"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="8" y1="21" x2="16" y2="21"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <path d="M8 14v-4a4 4 0 018 0v4"
                stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


# - WORKSHOP APPLICATION ICONS

    @staticmethod
    def mel_app_icon(size: int = 64, color: str = None) -> QIcon: #vers 7
        """MEL application icon"""
        svg_data = '''<svg viewBox="0 0 64 64">
            <defs>
                <linearGradient id="bgGradient" x1="0%" y1="0%" x2="100%" y2="100%">
                    <stop offset="0%" style="stop-color:#3a3a3a;stop-opacity:1" />
                    <stop offset="100%" style="stop-color:#2d2d2d;stop-opacity:1" />
                </linearGradient>
            </defs>
            <rect x="0" y="0" width="64" height="64" rx="12" ry="12" fill="url(#bgGradient)"/>
            <text x="32" y="42" font-size="28" fill="#ffffff" text-anchor="middle" font-weight="bold" font-family="Arial, sans-serif">MEL</text>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def col_workshop_icon(size: int = 64, color: str = None) -> QIcon: #vers 1
        """COL Workshop application icon"""
        svg_data = '''<svg viewBox="0 0 64 64">
            <defs>
                <linearGradient id="colGradient" x1="0%" y1="0%" x2="100%" y2="100%">
                    <stop offset="0%" style="stop-color:#2a4a7a;stop-opacity:1" />
                    <stop offset="100%" style="stop-color:#1a3a5a;stop-opacity:1" />
                </linearGradient>
            </defs>
            <rect x="0" y="0" width="64" height="64" rx="12" ry="12" fill="url(#colGradient)"/>
            <path d="M32 12 L50 20 L46 36 L28 36 L14 20 Z"
                fill="#4a7ab0" opacity="0.8"/>
            <path d="M14 20 L32 12 L50 20 L46 36 L28 36 Z"
                stroke="#ffffff" stroke-width="2" fill="none"/>
            <line x1="32" y1="12" x2="28" y2="36"
                stroke="#ffffff" stroke-width="1.5" opacity="0.6"/>
            <line x1="32" y1="12" x2="46" y2="36"
                stroke="#ffffff" stroke-width="1.5" opacity="0.6"/>
            <text x="32" y="56" font-size="14" fill="#ffffff" text-anchor="middle" 
                font-weight="bold" font-family="Arial, sans-serif">COL</text>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)
    

    @staticmethod
    def txd_workshop_icon(size: int = 64, color: str = None) -> QIcon: #vers 1
        """TXD Workshop application icon"""
        svg_data = '''<svg viewBox="0 0 64 64">
            <defs>
                <linearGradient id="txdGradient" x1="0%" y1="0%" x2="100%" y2="100%">
                    <stop offset="0%" style="stop-color:#7a2a4a;stop-opacity:1" />
                    <stop offset="100%" style="stop-color:#5a1a3a;stop-opacity:1" />
                </linearGradient>
            </defs>
            <rect x="0" y="0" width="64" height="64" rx="12" ry="12" fill="url(#txdGradient)"/>
            <rect x="10" y="10" width="44" height="34" rx="2"
                stroke="#ffffff" stroke-width="2.5" fill="none"/>
            <rect x="14" y="14" width="8" height="8" fill="#b04a7a"/>
            <rect x="22" y="22" width="8" height="8" fill="#b04a7a"/>
            <rect x="30" y="14" width="8" height="8" fill="#b04a7a"/>
            <rect x="38" y="22" width="8" height="8" fill="#b04a7a"/>
            <rect x="46" y="14" width="4" height="8" fill="#b04a7a"/>
            <rect x="14" y="30" width="8" height="8" fill="#b04a7a"/>
            <rect x="22" y="38" width="8" height="4" fill="#b04a7a"/>
            <rect x="30" y="30" width="8" height="8" fill="#b04a7a"/>
            <rect x="38" y="38" width="8" height="4" fill="#b04a7a"/>
            <rect x="46" y="30" width="4" height="8" fill="#b04a7a"/>
            <circle cx="20" cy="20" r="2.5" fill="#ffffff"/>
            <polyline points="50 40 42 28 34 36"
                stroke="#ffffff" stroke-width="2.5" fill="none" 
                stroke-linecap="round" stroke-linejoin="round"/>
            <text x="32" y="56" font-size="14" fill="#ffffff" text-anchor="middle" 
                font-weight="bold" font-family="Arial, sans-serif">TXD</text>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, color)


    @staticmethod
    def get_app_icon(size: int = 64) -> QIcon: #vers 1
        """IMG Factory application icon - Archive/Package themed"""
        svg_data = f'''<svg width="{size}" height="{size}" viewBox="0 0 64 64">
            <defs>
                <linearGradient id="grad" x1="0%" y1="0%" x2="100%" y2="100%">
                    <stop offset="0%" style="stop-color:#4A90E2;stop-opacity:1" />
                    <stop offset="100%" style="stop-color:#357ABD;stop-opacity:1" />
                </linearGradient>
            </defs>
            <rect width="64" height="64" rx="8" fill="url(#grad)"/>
            <!-- Archive box -->
            <path d="M 32 12 L 52 22 L 52 42 L 32 52 L 12 42 L 12 22 Z" fill="#ffffff" opacity="0.9"/>
            <path d="M 32 12 L 52 22 L 32 32 L 12 22 Z" fill="#ffffff" opacity="0.7"/>
            <path d="M 12 22 L 12 42 L 32 52 L 32 32 Z" fill="#ffffff" opacity="0.5"/>
            <!-- IMG text -->
            <text x="32" y="40" font-family="Arial" font-size="12" font-weight="bold" fill="#357ABD" text-anchor="middle">IMG</text>
        </svg>'''
        return SVGIconFactory._create_icon(svg_data, size, None)


# = STANDALONE FUNCTION WRAPPERS FOR BACKWARD COMPATIBILITY

def get_app_icon(size: int = 64) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_app_icon"""
    return SVGIconFactory.get_app_icon(size)

def get_add_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_add_icon"""
    return SVGIconFactory.get_add_icon(size)

def get_edit_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_edit_icon"""
    return SVGIconFactory.get_edit_icon(size)

def get_open_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_open_icon"""
    return SVGIconFactory.get_open_icon(size)

def get_refresh_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_refresh_icon"""
    return SVGIconFactory.get_refresh_icon(size)

def get_search_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_search_icon"""
    return SVGIconFactory.get_search_icon(size)

def get_export_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_export_icon"""
    return SVGIconFactory.get_export_icon(size)

def get_import_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_import_icon"""
    return SVGIconFactory.get_import_icon(size)

def get_warning_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_warning_icon"""
    return SVGIconFactory.get_warning_icon(size)

def get_success_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_success_icon"""
    return SVGIconFactory.get_success_icon(size)

def get_error_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_error_icon"""
    return SVGIconFactory.get_error_icon(size)

def get_img_file_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_img_file_icon"""
    return SVGIconFactory.get_img_file_icon(size)

def get_col_file_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_col_file_icon"""
    return SVGIconFactory.get_col_file_icon(size)

def get_txd_file_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_txd_file_icon"""
    return SVGIconFactory.get_txd_file_icon(size)

def get_close_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_close_icon"""
    return SVGIconFactory.get_close_icon(size)

def get_save_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_save_icon"""
    return SVGIconFactory.get_save_icon(size)

def get_remove_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_remove_icon"""
    return SVGIconFactory.get_remove_icon(size)

def get_view_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_view_icon"""
    return SVGIconFactory.get_view_icon(size)

def get_settings_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_settings_icon"""
    return SVGIconFactory.get_settings_icon(size)

def get_rebuild_icon(size: int = 24, color: str = None) -> QIcon: #vers 1
    """Wrapper for SVGIconFactory.get_rebuild_icon"""
    return SVGIconFactory.get_rebuild_icon(size)
