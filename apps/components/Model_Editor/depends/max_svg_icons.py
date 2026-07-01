#!/usr/bin/env python3
#this belongs in apps/components/Model_Editor/depends/max_svg_icons.py - Version: 1
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
    def _snap_magnet_base() -> str: #vers 2
        """Shared magnet-shape base path. Scaled to fill most of the 24x24
        viewBox so it's clearly visible at small icon sizes (16-20px).
        The magnet spans x:4-20, y:0-13 — most of the upper two thirds
        of the canvas, leaving the lower third for the target pictogram."""
        return '''<path d="M4 2 H9 V13 A3 3 0 0 0 15 13 V2 H20 V13
                   A8 8 0 0 1 4 13 Z" fill="currentColor" opacity="0.6"
                   stroke="currentColor" stroke-width="0.5"/>
                   <rect x="4" y="0" width="5" height="3" rx="1" fill="currentColor" opacity="0.6"/>
                   <rect x="15" y="0" width="5" height="3" rx="1" fill="currentColor" opacity="0.6"/>'''

    @staticmethod
    def snap_grid_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Snap To Grid Points Toggle"""
        return SVGIconFactory._create_icon(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxSVGIcons._snap_magnet_base()}
            <circle cx="8"  cy="18" r="1.5" fill="currentAccent"/>
            <circle cx="16" cy="18" r="1.5" fill="currentAccent"/>
            <circle cx="8"  cy="22" r="1.5" fill="currentAccent"/>
            <circle cx="16" cy="22" r="1.5" fill="currentAccent"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_pivot_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Snap To Pivot Toggle"""
        return SVGIconFactory._create_icon(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxSVGIcons._snap_magnet_base()}
            <circle cx="12" cy="19" r="3" fill="none" stroke="currentAccent" stroke-width="1.5"/>
            <circle cx="12" cy="19" r="1" fill="currentAccent"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_vertex_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Snap To Vertex Toggle"""
        return SVGIconFactory._create_icon(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxSVGIcons._snap_magnet_base()}
            <circle cx="12" cy="19" r="2.5" fill="currentAccent"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_endpoint_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Snap To Endpoint Toggle"""
        return SVGIconFactory._create_icon(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxSVGIcons._snap_magnet_base()}
            <line x1="7" y1="23" x2="17" y2="16" stroke="currentAccent" stroke-width="1.8" stroke-linecap="round"/>
            <circle cx="17" cy="16" r="2" fill="currentAccent"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_midpoint_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Snap To Midpoint Toggle"""
        return SVGIconFactory._create_icon(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxSVGIcons._snap_magnet_base()}
            <line x1="7" y1="23" x2="17" y2="16" stroke="currentAccent" stroke-width="1.8" stroke-linecap="round"/>
            <circle cx="12" cy="19.5" r="2" fill="currentAccent"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_edge_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Snap To Edge/Segment Toggle"""
        return SVGIconFactory._create_icon(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxSVGIcons._snap_magnet_base()}
            <line x1="6" y1="23" x2="18" y2="15" stroke="currentAccent" stroke-width="2.5" stroke-linecap="round"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_face_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Snap To Face Toggle"""
        return SVGIconFactory._create_icon(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxSVGIcons._snap_magnet_base()}
            <polygon points="12,15 18,23 6,23" fill="currentAccent" opacity="0.9"/>
        </svg>''', size, color, accent_color=accent_color)

    @staticmethod
    def snap_axis_constraint_icon(size: int = 20, color: str = None, accent_color: str = None) -> QIcon: #vers 1
        """Enable Axis Constraints in Snaps Toggle"""
        return SVGIconFactory._create_icon(f'''<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            {MaxSVGIcons._snap_magnet_base()}
            <line x1="8" y1="23" x2="8" y2="16" stroke="currentAccent" stroke-width="2" stroke-linecap="round"/>
            <line x1="8" y1="23" x2="16" y2="23" stroke="currentAccent" stroke-width="2" stroke-linecap="round"/>
        </svg>''', size, color, accent_color=accent_color)

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
