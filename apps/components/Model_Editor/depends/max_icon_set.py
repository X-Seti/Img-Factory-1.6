#!/usr/bin/env python3
#this belongs in apps/components/Model_Editor/depends/max_icon_set.py - Version: 1
# X-Seti - July 2026 - IMG Factory 1.6 - 3ds Max 2014 Style Icon Set

"""
3ds Max 2014-style icon set for Model Workshop.

Visual language (from 3dsmax2014_ui_catalog.md + UI screenshots):
  - Dark grey button background: #3c3c3c
  - Icons use HARDCODED colors — not currentColor/theme-aware
  - Color coding by category:
      Snap targets:   red magnet base (#cc3333) + bright target marker
      Selection:      white/light grey pictograms on dark
      Transform:      blue/teal (#4488cc) arrows
      Navigation:     white outlines, axis colors (X=#cc3333 Y=#44aa44 Z=#4488cc)
      Render/Shading: orange/amber (#cc8833) for active, grey for inactive
      Geometry:       green (#44aa66) for create, yellow (#ccaa33) for modify

Switching: stored in model_workshop.json 'icon_set' key.
  'default'  -> uses current theme's currentColor (SVGIconFactory)
  '3dsmax'   -> uses this module (hardcoded Max palette)

Import in model_workshop.py:
    from apps.components.Model_Editor.depends.max_icon_set import MaxIconSet
"""

from PyQt6.QtGui import QIcon
from apps.methods.imgfactory_svg_icons import SVGIconFactory

# Max palette constants
_BG      = '#3c3c3c'   # button background (not used in SVG — Qt handles button bg)
_WHITE   = '#e8e8e8'   # primary icon color — light grey, not pure white
_SNAP_R  = '#cc3333'   # snap magnet red
_AXIS_X  = '#cc4444'   # X axis red
_AXIS_Y  = '#44aa44'   # Y axis green
_AXIS_Z  = '#4488cc'   # Z axis blue
_TRANS   = '#4488cc'   # transform blue
_GEO_G   = '#44aa66'   # geometry create green
_GEO_Y   = '#ccaa33'   # geometry modify yellow
_RENDER  = '#cc8833'   # render/shading amber
_SNAP_T  = '#ffcc44'   # snap target bright yellow marker

##Methods list -
# MaxIconSet.vertex_select_icon
# MaxIconSet.edge_select_icon
# MaxIconSet.face_select_icon
# MaxIconSet.poly_select_icon
# MaxIconSet.backface_cull_icon
# MaxIconSet.snap_grid_icon
# MaxIconSet.snap_pivot_icon
# MaxIconSet.snap_vertex_icon
# MaxIconSet.snap_endpoint_icon
# MaxIconSet.snap_midpoint_icon
# MaxIconSet.snap_edge_icon
# MaxIconSet.snap_face_icon
# MaxIconSet.snap_axis_icon
# MaxIconSet.create_primitive_icon
# MaxIconSet.extrude_icon
# MaxIconSet.zoom_in_icon
# MaxIconSet.zoom_out_icon
# MaxIconSet.reset_view_icon
# MaxIconSet.fit_view_icon
# MaxIconSet.view_xy_icon
# MaxIconSet.view_xz_icon
# MaxIconSet.view_yz_icon
# MaxIconSet.view_iso_icon
# MaxIconSet.render_settings_icon
# MaxIconSet.toggle_mesh_icon
# MaxIconSet.toggle_backface_icon
# MaxIconSet.render_style_icon
# MaxIconSet.shading_icon
# MaxIconSet.light_setup_icon


def _ic(svg: str, size: int = 20) -> QIcon:
    """Render an SVG string — no color substitution, colors are hardcoded."""
    return SVGIconFactory._create_icon(svg, size, color=None)


class MaxIconSet:
    """3ds Max 2014 style icons with hardcoded palette matching Max's UI."""

    # ------------------------------------------------------------------ #
    # Selection toolbar                                                    #
    # ------------------------------------------------------------------ #

    @staticmethod
    def vertex_select_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Vertex Select — white dot on mesh triangle, Max style."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="3,20 12,4 21,20"
                stroke="{_WHITE}" stroke-width="2" fill="none"/>
            <circle cx="12" cy="4"  r="3" fill="{_SNAP_T}"/>
            <circle cx="3"  cy="20" r="2" fill="{_WHITE}" opacity="0.5"/>
            <circle cx="21" cy="20" r="2" fill="{_WHITE}" opacity="0.5"/>
        </svg>''', size)

    @staticmethod
    def edge_select_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Edge Select — highlighted edge in amber."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="3,20 12,4 21,20"
                stroke="{_WHITE}" stroke-width="1.5" fill="none" opacity="0.5"/>
            <line x1="3" y1="20" x2="21" y2="20"
                stroke="{_RENDER}" stroke-width="3.5" stroke-linecap="round"/>
        </svg>''', size)

    @staticmethod
    def face_select_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Face Select — filled face in blue."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="3,4 21,4 21,20 3,20"
                stroke="{_WHITE}" stroke-width="1.5" fill="none" opacity="0.5"/>
            <polygon points="3,4 21,20 3,20"
                fill="{_TRANS}" opacity="0.85"/>
        </svg>''', size)

    @staticmethod
    def poly_select_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Polygon Select — filled quad in teal."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="3,4 21,4 21,20 3,20"
                fill="{_TRANS}" opacity="0.85"
                stroke="{_WHITE}" stroke-width="1.5"/>
        </svg>''', size)

    @staticmethod
    def backface_cull_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Backface Culling — two overlapping faces, front solid amber."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="6,5 20,5 20,17 6,17"
                stroke="{_WHITE}" stroke-width="1.2" fill="none"
                stroke-dasharray="3,2" opacity="0.45"/>
            <polygon points="4,7 18,7 18,19 4,19"
                fill="{_RENDER}" opacity="0.35"
                stroke="{_RENDER}" stroke-width="2"/>
        </svg>''', size)

    # ------------------------------------------------------------------ #
    # Snap toolbar — red magnet base + bright yellow target marker         #
    # ------------------------------------------------------------------ #

    @staticmethod
    def _snap_base() -> str: #vers 1
        """Shared red magnet U-shape base, Max snap icon style."""
        return f'''
            <path d="M4,2 H8 V11 A4 4 0 0 0 16 11 V2 H20 V11
                     A8 8 0 0 1 4 11 Z"
                fill="{_SNAP_R}" stroke="{_SNAP_R}" stroke-width="0.3"/>
            <rect x="4"  y="0" width="4" height="3" rx="1" fill="{_SNAP_R}"/>
            <rect x="16" y="0" width="4" height="3" rx="1" fill="{_SNAP_R}"/>'''

    @staticmethod
    def snap_grid_icon(size: int = 20, **_) -> QIcon: #vers 1
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxIconSet._snap_base()}
            <circle cx="8"  cy="18" r="1.8" fill="{_SNAP_T}"/>
            <circle cx="16" cy="18" r="1.8" fill="{_SNAP_T}"/>
            <circle cx="8"  cy="22" r="1.8" fill="{_SNAP_T}"/>
            <circle cx="16" cy="22" r="1.8" fill="{_SNAP_T}"/>
        </svg>''', size)

    @staticmethod
    def snap_pivot_icon(size: int = 20, **_) -> QIcon: #vers 1
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxIconSet._snap_base()}
            <circle cx="12" cy="20" r="3.5"
                fill="none" stroke="{_SNAP_T}" stroke-width="1.8"/>
            <circle cx="12" cy="20" r="1.2" fill="{_SNAP_T}"/>
        </svg>''', size)

    @staticmethod
    def snap_vertex_icon(size: int = 20, **_) -> QIcon: #vers 1
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxIconSet._snap_base()}
            <circle cx="12" cy="20" r="3" fill="{_SNAP_T}"/>
        </svg>''', size)

    @staticmethod
    def snap_endpoint_icon(size: int = 20, **_) -> QIcon: #vers 1
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxIconSet._snap_base()}
            <line x1="6" y1="23" x2="18" y2="15"
                stroke="{_WHITE}" stroke-width="2" stroke-linecap="round"
                opacity="0.6"/>
            <circle cx="18" cy="15" r="2.5" fill="{_SNAP_T}"/>
        </svg>''', size)

    @staticmethod
    def snap_midpoint_icon(size: int = 20, **_) -> QIcon: #vers 1
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxIconSet._snap_base()}
            <line x1="6" y1="23" x2="18" y2="15"
                stroke="{_WHITE}" stroke-width="2" stroke-linecap="round"
                opacity="0.6"/>
            <circle cx="12" cy="19" r="2.5" fill="{_SNAP_T}"/>
        </svg>''', size)

    @staticmethod
    def snap_edge_icon(size: int = 20, **_) -> QIcon: #vers 1
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxIconSet._snap_base()}
            <line x1="5" y1="23" x2="19" y2="15"
                stroke="{_SNAP_T}" stroke-width="3.5" stroke-linecap="round"/>
        </svg>''', size)

    @staticmethod
    def snap_face_icon(size: int = 20, **_) -> QIcon: #vers 1
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxIconSet._snap_base()}
            <polygon points="6,15 18,15 18,23 6,23"
                fill="{_SNAP_T}" opacity="0.85"/>
        </svg>''', size)

    @staticmethod
    def snap_axis_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Axis Constraints — XY axes in Max axis colors."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxIconSet._snap_base()}
            <line x1="6" y1="22" x2="6"  y2="15"
                stroke="{_AXIS_Y}" stroke-width="2.5" stroke-linecap="round"/>
            <line x1="6" y1="22" x2="18" y2="22"
                stroke="{_AXIS_X}" stroke-width="2.5" stroke-linecap="round"/>
            <polygon points="6,14 4.5,17 7.5,17" fill="{_AXIS_Y}"/>
            <polygon points="19,22 16,20.5 16,23.5" fill="{_AXIS_X}"/>
        </svg>''', size)

    # ------------------------------------------------------------------ #
    # Edit Geometry toolbar                                                #
    # ------------------------------------------------------------------ #

    @staticmethod
    def create_primitive_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Create Primitive — green cube outline, Max create color."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="4,8 12,4 20,8 20,18 12,22 4,18"
                stroke="{_GEO_G}" stroke-width="2" fill="none"/>
            <line x1="4"  y1="8"  x2="12" y2="12"
                stroke="{_GEO_G}" stroke-width="1.5" opacity="0.6"/>
            <line x1="20" y1="8"  x2="12" y2="12"
                stroke="{_GEO_G}" stroke-width="1.5" opacity="0.6"/>
            <line x1="12" y1="12" x2="12" y2="22"
                stroke="{_GEO_G}" stroke-width="1.5" opacity="0.6"/>
        </svg>''', size)

    @staticmethod
    def extrude_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Extrude — yellow face with amber arrow pushing out."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <rect x="3" y="14" width="18" height="6"
                stroke="{_GEO_Y}" stroke-width="2"
                fill="{_GEO_Y}" fill-opacity="0.25"/>
            <line x1="12" y1="14" x2="12" y2="4"
                stroke="{_RENDER}" stroke-width="2.5" stroke-linecap="round"/>
            <polygon points="12,2 8,8 16,8" fill="{_RENDER}"/>
        </svg>''', size)

    # ------------------------------------------------------------------ #
    # Navigation toolbar                                                   #
    # ------------------------------------------------------------------ #

    @staticmethod
    def zoom_in_icon(size: int = 20, **_) -> QIcon: #vers 1
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <circle cx="10" cy="10" r="7"
                stroke="{_WHITE}" stroke-width="2" fill="none"/>
            <line x1="15.5" y1="15.5" x2="21" y2="21"
                stroke="{_WHITE}" stroke-width="2.5" stroke-linecap="round"/>
            <line x1="7" y1="10" x2="13" y2="10"
                stroke="{_SNAP_T}" stroke-width="2" stroke-linecap="round"/>
            <line x1="10" y1="7" x2="10" y2="13"
                stroke="{_SNAP_T}" stroke-width="2" stroke-linecap="round"/>
        </svg>''', size)

    @staticmethod
    def zoom_out_icon(size: int = 20, **_) -> QIcon: #vers 1
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <circle cx="10" cy="10" r="7"
                stroke="{_WHITE}" stroke-width="2" fill="none"/>
            <line x1="15.5" y1="15.5" x2="21" y2="21"
                stroke="{_WHITE}" stroke-width="2.5" stroke-linecap="round"/>
            <line x1="7" y1="10" x2="13" y2="10"
                stroke="{_SNAP_T}" stroke-width="2" stroke-linecap="round"/>
        </svg>''', size)

    @staticmethod
    def reset_view_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Reset view — home icon in Max white."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <path d="M3,12 L12,3 L21,12"
                stroke="{_WHITE}" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M5,10 L5,20 L10,20 L10,15 L14,15 L14,20 L19,20 L19,10"
                stroke="{_WHITE}" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>''', size)

    @staticmethod
    def fit_view_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Fit to window — expand arrows in white."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polyline points="3,9 3,3 9,3"
                stroke="{_WHITE}" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="15,3 21,3 21,9"
                stroke="{_WHITE}" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="21,15 21,21 15,21"
                stroke="{_WHITE}" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
            <polyline points="9,21 3,21 3,15"
                stroke="{_WHITE}" stroke-width="2" fill="none"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>''', size)

    @staticmethod
    def view_xy_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Top view — X red Y green overlapping, Max axis colors."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <text x="9" y="19" font-family="Arial,sans-serif" font-size="17"
                font-weight="bold" fill="{_AXIS_Y}" opacity="0.95">Y</text>
            <text x="1" y="19" font-family="Arial,sans-serif" font-size="17"
                font-weight="bold" fill="{_AXIS_X}">X</text>
        </svg>''', size)

    @staticmethod
    def view_xz_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Front view — X red Z blue."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <text x="9" y="19" font-family="Arial,sans-serif" font-size="17"
                font-weight="bold" fill="{_AXIS_Z}" opacity="0.95">Z</text>
            <text x="1" y="19" font-family="Arial,sans-serif" font-size="17"
                font-weight="bold" fill="{_AXIS_X}">X</text>
        </svg>''', size)

    @staticmethod
    def view_yz_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Side view — Y green Z blue."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <text x="9" y="19" font-family="Arial,sans-serif" font-size="17"
                font-weight="bold" fill="{_AXIS_Z}" opacity="0.95">Z</text>
            <text x="1" y="19" font-family="Arial,sans-serif" font-size="17"
                font-weight="bold" fill="{_AXIS_Y}">Y</text>
        </svg>''', size)

    @staticmethod
    def view_iso_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Perspective/Iso view — ISO text in amber."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <text x="1" y="11" font-family="Arial,sans-serif" font-size="10"
                font-weight="bold" fill="{_RENDER}">ISO</text>
            <line x1="1" y1="13" x2="23" y2="13"
                stroke="{_WHITE}" stroke-width="1" opacity="0.35"/>
            <text x="4" y="21" font-family="Arial,sans-serif" font-size="8"
                font-weight="bold" fill="{_WHITE}" opacity="0.6">VIEW</text>
        </svg>''', size)

    # ------------------------------------------------------------------ #
    # Render toolbar                                                       #
    # ------------------------------------------------------------------ #

    @staticmethod
    def render_settings_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Render Settings — amber sliders."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <line x1="3" y1="6"  x2="21" y2="6"
                stroke="{_WHITE}" stroke-width="2" stroke-linecap="round"
                opacity="0.6"/>
            <line x1="3" y1="12" x2="21" y2="12"
                stroke="{_WHITE}" stroke-width="2" stroke-linecap="round"
                opacity="0.6"/>
            <line x1="3" y1="18" x2="21" y2="18"
                stroke="{_WHITE}" stroke-width="2" stroke-linecap="round"
                opacity="0.6"/>
            <circle cx="8"  cy="6"  r="2.5" fill="{_RENDER}"/>
            <circle cx="16" cy="12" r="2.5" fill="{_RENDER}"/>
            <circle cx="10" cy="18" r="2.5" fill="{_RENDER}"/>
        </svg>''', size)

    @staticmethod
    def toggle_mesh_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Toggle Mesh — white wireframe grid."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <rect x="3" y="3" width="18" height="18"
                stroke="{_WHITE}" stroke-width="2" fill="none"
                stroke-linecap="round"/>
            <line x1="3"  y1="9"  x2="21" y2="9"
                stroke="{_WHITE}" stroke-width="1.5" opacity="0.7"/>
            <line x1="3"  y1="15" x2="21" y2="15"
                stroke="{_WHITE}" stroke-width="1.5" opacity="0.7"/>
            <line x1="9"  y1="3"  x2="9"  y2="21"
                stroke="{_WHITE}" stroke-width="1.5" opacity="0.7"/>
            <line x1="15" y1="3"  x2="15" y2="21"
                stroke="{_WHITE}" stroke-width="1.5" opacity="0.7"/>
        </svg>''', size)

    @staticmethod
    def toggle_backface_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Toggle Backface — triangle with amber flip arrow."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="12,3 22,19 2,19"
                stroke="{_WHITE}" stroke-width="2" fill="none"/>
            <path d="M8,19 Q12,23 16,19"
                stroke="{_RENDER}" stroke-width="2"
                fill="none" stroke-linecap="round"/>
            <polygon points="16,19 13,21 14,17" fill="{_RENDER}"/>
        </svg>''', size)

    @staticmethod
    def render_style_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Cycle Render Style — half solid amber / half white wireframe sphere."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <clipPath id="lh"><rect x="0" y="0" width="12" height="24"/></clipPath>
            <clipPath id="rh"><rect x="12" y="0" width="12" height="24"/></clipPath>
            <circle cx="12" cy="12" r="9"
                fill="{_RENDER}" opacity="0.6" clip-path="url(#lh)"/>
            <circle cx="12" cy="12" r="9"
                stroke="{_WHITE}" stroke-width="1.5" fill="none"
                clip-path="url(#rh)"/>
            <ellipse cx="12" cy="12" rx="4" ry="9"
                stroke="{_WHITE}" stroke-width="1" fill="none"
                clip-path="url(#rh)" opacity="0.6"/>
            <line x1="12" y1="3" x2="12" y2="21"
                stroke="{_WHITE}" stroke-width="1.5"/>
            <circle cx="12" cy="12" r="9"
                stroke="{_WHITE}" stroke-width="1.8" fill="none"/>
        </svg>''', size)

    @staticmethod
    def shading_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Toggle Shading — sphere with amber highlight spot."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <circle cx="12" cy="12" r="9"
                stroke="{_WHITE}" stroke-width="1.8"
                fill="{_WHITE}" fill-opacity="0.15"/>
            <circle cx="9" cy="9" r="3.5"
                fill="{_RENDER}" opacity="0.5"/>
            <circle cx="8" cy="8" r="2"
                fill="{_RENDER}" opacity="0.9"/>
        </svg>''', size)

    @staticmethod
    def light_setup_icon(size: int = 20, **_) -> QIcon: #vers 1
        """Light Setup — amber lightbulb with rays."""
        return _ic(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <path d="M9 21h6M10 17h4
                     M12 3 C8.5 3 6 5.5 6 9 C6 11.5 7.5 13.5 9 15
                     L9 17 L15 17 L15 15 C16.5 13.5 18 11.5 18 9
                     C18 5.5 15.5 3 12 3 Z"
                stroke="{_RENDER}" stroke-width="1.6"
                fill="{_RENDER}" fill-opacity="0.3"
                stroke-linecap="round" stroke-linejoin="round"/>
            <line x1="12" y1="1"   x2="12" y2="0"
                stroke="{_SNAP_T}" stroke-width="1.5" stroke-linecap="round"/>
            <line x1="21" y1="9"   x2="22" y2="9"
                stroke="{_SNAP_T}" stroke-width="1.5" stroke-linecap="round"/>
            <line x1="3"  y1="9"   x2="2"  y2="9"
                stroke="{_SNAP_T}" stroke-width="1.5" stroke-linecap="round"/>
            <line x1="18.4" y1="4.6" x2="19.4" y2="3.6"
                stroke="{_SNAP_T}" stroke-width="1.5" stroke-linecap="round"/>
            <line x1="5.6"  y1="4.6" x2="4.6"  y2="3.6"
                stroke="{_SNAP_T}" stroke-width="1.5" stroke-linecap="round"/>
        </svg>''', size)
