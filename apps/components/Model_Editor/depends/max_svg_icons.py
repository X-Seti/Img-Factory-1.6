#!/usr/bin/env python3
#this belongs in apps/components/Model_Editor/depends/max_svg_icons.py - Version: 2
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
# MaxSVGIcons.snap_axis_constraint_icon
# MaxSVGIcons.snap_edge_icon
# MaxSVGIcons.snap_endpoint_icon
# MaxSVGIcons.snap_face_icon
# MaxSVGIcons.snap_grid_icon
# MaxSVGIcons.snap_midpoint_icon
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
    def snap_grid_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 2
        """Snap To Grid Points Toggle — 3x3 dot grid"""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <circle cx="4"  cy="4"  r="2" fill="currentColor"/>
            <circle cx="12" cy="4"  r="2" fill="currentColor"/>
            <circle cx="20" cy="4"  r="2" fill="currentColor"/>
            <circle cx="4"  cy="12" r="2" fill="currentColor"/>
            <circle cx="12" cy="12" r="2.5" fill="currentColor"/>
            <circle cx="20" cy="12" r="2" fill="currentColor"/>
            <circle cx="4"  cy="20" r="2" fill="currentColor"/>
            <circle cx="12" cy="20" r="2" fill="currentColor"/>
            <circle cx="20" cy="20" r="2" fill="currentColor"/>
        </svg>''', size, color)

    @staticmethod
    def snap_pivot_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 2
        """Snap To Pivot Toggle — crosshair target"""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <circle cx="12" cy="12" r="4.5" fill="none" stroke="currentColor" stroke-width="1.8"/>
            <circle cx="12" cy="12" r="1.5" fill="currentColor"/>
            <line x1="12" y1="2"  x2="12" y2="7.5"  stroke="currentColor" stroke-width="1.8" stroke-linecap="round"/>
            <line x1="12" y1="16.5" x2="12" y2="22" stroke="currentColor" stroke-width="1.8" stroke-linecap="round"/>
            <line x1="2"  y1="12" x2="7.5"  y2="12" stroke="currentColor" stroke-width="1.8" stroke-linecap="round"/>
            <line x1="16.5" y1="12" x2="22" y2="12" stroke="currentColor" stroke-width="1.8" stroke-linecap="round"/>
        </svg>''', size, color)

    @staticmethod
    def snap_vertex_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 2
        """Snap To Vertex Toggle — diamond vertex marker"""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="3,19 12,3 21,19" stroke="currentColor" stroke-width="1.4"
                     fill="none" opacity="0.5"/>
            <polygon points="12,3 21,19 15,19" stroke="currentColor" stroke-width="1.2"
                     fill="none" opacity="0.3"/>
            <circle cx="12" cy="3"  r="2.5" fill="currentColor"/>
            <circle cx="3"  cy="19" r="2"   fill="currentColor" opacity="0.7"/>
            <circle cx="21" cy="19" r="2"   fill="currentColor" opacity="0.7"/>
        </svg>''', size, color)

    @staticmethod
    def snap_endpoint_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 2
        """Snap To Endpoint Toggle — line with filled dot at end"""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <line x1="4" y1="20" x2="20" y2="4" stroke="currentColor" stroke-width="2"
                  stroke-linecap="round" opacity="0.6"/>
            <circle cx="20" cy="4"  r="3" fill="currentColor"/>
            <circle cx="4"  cy="20" r="2" fill="currentColor" opacity="0.4"/>
        </svg>''', size, color)

    @staticmethod
    def snap_midpoint_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 2
        """Snap To Midpoint Toggle — line with filled dot at midpoint"""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <line x1="4" y1="20" x2="20" y2="4" stroke="currentColor" stroke-width="2"
                  stroke-linecap="round" opacity="0.6"/>
            <circle cx="12" cy="12" r="3" fill="currentColor"/>
            <circle cx="20" cy="4"  r="1.8" fill="currentColor" opacity="0.4"/>
            <circle cx="4"  cy="20" r="1.8" fill="currentColor" opacity="0.4"/>
        </svg>''', size, color)

    @staticmethod
    def snap_edge_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 2
        """Snap To Edge/Segment Toggle — bold highlighted edge"""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="12,3 21,19 3,19" stroke="currentColor" stroke-width="1.4"
                     fill="none" opacity="0.4"/>
            <line x1="3" y1="19" x2="21" y2="19"
                  stroke="currentColor" stroke-width="3" stroke-linecap="round"/>
        </svg>''', size, color)

    @staticmethod
    def snap_face_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 2
        """Snap To Face Toggle — filled face/polygon"""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <polygon points="3,3 21,3 21,21 3,21" stroke="currentColor"
                     stroke-width="1.2" fill="none" opacity="0.4"/>
            <line x1="3" y1="3" x2="21" y2="21" stroke="currentColor"
                  stroke-width="1" opacity="0.3"/>
            <polygon points="12,3 21,21 3,21" fill="currentColor" opacity="0.8"
                     stroke="currentColor" stroke-width="1"/>
        </svg>''', size, color)

    @staticmethod
    def snap_axis_constraint_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 2
        """Enable Axis Constraints in Snaps Toggle — XY axis indicator"""
        return SVGIconFactory._create_icon('''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <line x1="4" y1="20" x2="4"  y2="4"  stroke="currentColor" stroke-width="2.5"
                  stroke-linecap="round"/>
            <line x1="4" y1="20" x2="20" y2="20" stroke="currentColor" stroke-width="2.5"
                  stroke-linecap="round"/>
            <polygon points="4,2 2.5,6 5.5,6" fill="currentColor"/>
            <polygon points="22,20 18,18.5 18,21.5" fill="currentColor"/>
            <text x="8" y="14" font-size="7" fill="currentColor" opacity="0.9"
                  font-family="sans-serif" font-weight="bold">XY</text>
        </svg>''', size, color)


    # ------------------------------------------------------------------ #
    # Placeholder sections for future Max-catalogue icon families         #
    # Add here as Phase 4 (Edit Geometry), Phase 2 (Transform tools)     #
    # and other roadmap items are implemented.                            #
    # ------------------------------------------------------------------ #
    # Edit Geometry operations (Phase 4):
    #   extrude_icon, bevel_icon, weld_selected_icon, weld_target_icon,
    #   cut_icon, slice_icon, detach_icon, attach_icon, divide_icon, turn_icon
    # Transform tools (Phase 2):
    #   move_type_in_icon, rotate_type_in_icon, scale_type_in_icon
    # ------------------------------------------------------------------ #
