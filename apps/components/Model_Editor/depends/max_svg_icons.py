#!/usr/bin/env python3
#this belongs in apps/components/Model_Editor/depends/max_svg_icons.py - Version: 5
# X-Seti - June 2026 - IMG Factory 1.6 - 3ds Max-style SVG Icons for Model Workshop

"""
3ds Max-catalogue SVG icons for Model Workshop.
Kept separate from the shared imgfactory_svg_icons.py so Max-specific
icon families (snap targets, transform gizmo tools, Edit Geometry
operations, etc.) don't grow the shared 221-icon factory.

Uses the same two-tone pattern as imgfactory_svg_icons.py:
  currentColor  = normal icon colour (theme text_primary)
  currentAccent = accent colour (theme text_accent)
Both are resolved by SVGIconFactory._create_icon() before rendering.

Import in model_workshop.py:
    from apps.components.Model_Editor.depends.max_svg_icons import MaxSVGIcons
"""

from PyQt6.QtGui import QIcon
from apps.methods.imgfactory_svg_icons import SVGIconFactory

##Methods list -
# MaxSVGIcons._snap_magnet_base
# MaxSVGIcons.align_icon
# MaxSVGIcons.create_primitive_icon
# MaxSVGIcons.extrude_icon
# MaxSVGIcons.mirror_icon
# MaxSVGIcons.snap_angle_icon
# MaxSVGIcons.snap_axis_constraint_icon
# MaxSVGIcons.snap_edge_icon
# MaxSVGIcons.snap_endpoint_icon
# MaxSVGIcons.snap_face_icon
# MaxSVGIcons.snap_grid_icon
# MaxSVGIcons.snap_midpoint_icon
# MaxSVGIcons.snap_percent_icon
# MaxSVGIcons.snap_pivot_icon
# MaxSVGIcons.snap_vertex_icon


class MaxSVGIcons:
    """3ds Max-style icons for Model Workshop. Delegates rendering to
    SVGIconFactory._create_icon() so the two-tone currentColor/currentAccent
    mechanism, caching, and background-colour support are all inherited
    without duplicating that infrastructure here."""

    # ------------------------------------------------------------------ #
    # Snap target icons                                                    #
    # Visual language: magnet base (currentColor) + target pictogram      #
    # (currentAccent) — confirmed from 3dsmax2014_ui_catalog.md session   #
    # ------------------------------------------------------------------ #

    @staticmethod
    def _snap_magnet_base() -> str: #vers 3
        """No longer used as a base — kept for backwards compat only.
        Snap icons now use direct readable symbols like the working
        selection icons, not the magnet shape that was unreadable at 16px."""
        return ''

    @staticmethod
    def snap_grid_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 4
        """Snap To Grid Points Toggle — 3x3 dot grid.
        3dsmax set: centre dot + corner dots in accent color."""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <circle cx="4"  cy="4"  r="2" fill="currentColor"/>
            <circle cx="12" cy="4"  r="2" fill="currentColor"/>
            <circle cx="20" cy="4"  r="2" fill="currentColor"/>
            <circle cx="4"  cy="12" r="2" fill="currentColor"/>
            <circle cx="12" cy="12" r="3" fill="currentAccent"/>
            <circle cx="20" cy="12" r="2" fill="currentColor"/>
            <circle cx="4"  cy="20" r="2" fill="currentColor"/>
            <circle cx="12" cy="20" r="2" fill="currentColor"/>
            <circle cx="20" cy="20" r="2" fill="currentColor"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_pivot_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 4
        """Snap To Pivot Toggle — crosshair target.
        3dsmax set: centre dot and circle in accent color."""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <circle cx="12" cy="12" r="5" fill="none" stroke="currentAccent" stroke-width="2"/>
            <circle cx="12" cy="12" r="2" fill="currentAccent"/>
            <line x1="12" y1="2"  x2="12" y2="7"  stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="12" y1="17" x2="12" y2="22" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="2"  y1="12" x2="7"  y2="12" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="17" y1="12" x2="22" y2="12" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_vertex_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 4
        """Snap To Vertex Toggle — triangle with highlighted vertex.
        3dsmax set: snap-point vertex in accent color."""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="3,20 12,4 21,20" stroke="currentColor" stroke-width="2" fill="none"/>
            <circle cx="12" cy="4"  r="3" fill="currentAccent"/>
            <circle cx="3"  cy="20" r="2" fill="currentColor"/>
            <circle cx="21" cy="20" r="2" fill="currentColor"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_endpoint_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 4
        """Snap To Endpoint Toggle — line with filled dot at end.
        3dsmax set: endpoint dot in accent color."""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <line x1="4" y1="20" x2="20" y2="4" stroke="currentColor" stroke-width="2.5" stroke-linecap="round"/>
            <circle cx="20" cy="4"  r="3.5" fill="currentAccent"/>
            <circle cx="4"  cy="20" r="2"   fill="currentColor"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_midpoint_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 4
        """Snap To Midpoint Toggle — line with filled dot at midpoint.
        3dsmax set: midpoint dot in accent color."""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <line x1="4" y1="20" x2="20" y2="4" stroke="currentColor" stroke-width="2.5" stroke-linecap="round"/>
            <circle cx="12" cy="12" r="3.5" fill="currentAccent"/>
            <circle cx="20" cy="4"  r="2"   fill="currentColor"/>
            <circle cx="4"  cy="20" r="2"   fill="currentColor"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_edge_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 4
        """Snap To Edge/Segment Toggle — bold highlighted edge.
        3dsmax set: highlighted edge in accent color."""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="12,3 21,20 3,20" stroke="currentColor" stroke-width="2" fill="none"/>
            <line x1="3" y1="20" x2="21" y2="20" stroke="currentAccent" stroke-width="4" stroke-linecap="round"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_face_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 4
        """Snap To Face Toggle — filled face.
        3dsmax set: filled face in accent color."""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="3,4 21,4 21,20 3,20" stroke="currentColor" stroke-width="2" fill="none"/>
            <polygon points="3,4 21,20 3,20" fill="currentAccent"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_axis_constraint_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 4
        """Enable Axis Constraints in Snaps Toggle — XY axis.
        3dsmax set: XY label in accent color."""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <line x1="3" y1="21" x2="3"  y2="3"  stroke="currentColor" stroke-width="2.5" stroke-linecap="round"/>
            <line x1="3" y1="21" x2="21" y2="21" stroke="currentColor" stroke-width="2.5" stroke-linecap="round"/>
            <polygon points="3,2 1.5,6 4.5,6" fill="currentColor"/>
            <polygon points="22,21 18,19.5 18,22.5" fill="currentColor"/>
            <text x="7" y="15" font-size="8" fill="currentAccent"
                  font-family="sans-serif" font-weight="bold">XY</text>
        </svg>''', size, color, accent_color=accent_color)


    # ------------------------------------------------------------------ #
    # Edit Geometry operations (Phase 4 roadmap)                         #
    # ------------------------------------------------------------------ #

    @staticmethod
    def create_primitive_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Create Primitive — box/cube shape"""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="4,8 12,4 20,8 20,18 12,22 4,18"
                     stroke="currentColor" stroke-width="2" fill="none"/>
            <line x1="4"  y1="8"  x2="12" y2="12" stroke="currentColor" stroke-width="1.5"/>
            <line x1="20" y1="8"  x2="12" y2="12" stroke="currentColor" stroke-width="1.5"/>
            <line x1="12" y1="12" x2="12" y2="22" stroke="currentColor" stroke-width="1.5"/>
        </svg>''', size, color)

    @staticmethod
    def extrude_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Extrude — face with arrow pushing outward"""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <rect x="3" y="14" width="18" height="6" stroke="currentColor" stroke-width="2" fill="none"/>
            <line x1="12" y1="14" x2="12" y2="4"  stroke="currentColor" stroke-width="2.5" stroke-linecap="round"/>
            <polygon points="12,2 8,8 16,8" fill="currentColor"/>
        </svg>''', size, color)

    @staticmethod
    def snap_angle_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Angle Snap — two lines forming an angle with arc and snap dot."""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <line x1="3" y1="19" x2="21" y2="19"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <line x1="3" y1="19" x2="17" y2="5"
                stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
            <path d="M9,19 A6,6 0 0,0 6.2,12.5"
                stroke="currentColor" stroke-width="1.8"
                fill="none" stroke-linecap="round" opacity="0.7"/>
            <circle cx="6.2" cy="12.5" r="2.2" fill="currentColor"/>
        </svg>''', size, color)

    @staticmethod
    def snap_percent_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Percent Snap — % symbol with snap indicator dot."""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <text x="1" y="17" font-family="Arial,sans-serif" font-size="15"
                font-weight="bold" fill="currentColor" opacity="0.9">%</text>
            <circle cx="19" cy="19" r="3" fill="currentColor" opacity="0.5"/>
            <circle cx="19" cy="19" r="1.2" fill="currentColor"/>
        </svg>''', size, color)

    @staticmethod
    def mirror_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Mirror — two triangles reflected across a centre axis line."""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <line x1="12" y1="2" x2="12" y2="22"
                stroke="currentColor" stroke-width="1.5"
                stroke-dasharray="3,2" opacity="0.6"/>
            <polygon points="3,5 10,12 3,19"
                fill="currentColor" opacity="0.7"
                stroke="currentColor" stroke-width="1.2"/>
            <polygon points="21,5 14,12 21,19"
                fill="currentColor" opacity="0.35"
                stroke="currentColor" stroke-width="1.2"/>
        </svg>''', size, color)

    @staticmethod
    def align_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Align — dashed source box aligning to solid target, arrow showing movement."""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <rect x="11" y="4" width="10" height="16"
                stroke="currentColor" stroke-width="2" fill="none"/>
            <rect x="3" y="8" width="8" height="8"
                stroke="currentColor" stroke-width="1.8" fill="none"
                stroke-dasharray="2.5,2" opacity="0.6"/>
            <line x1="7" y1="12" x2="11" y2="12"
                stroke="currentColor" stroke-width="1.5"/>
            <polygon points="11,12 9,10.5 9,13.5" fill="currentColor"/>
        </svg>''', size, color)
