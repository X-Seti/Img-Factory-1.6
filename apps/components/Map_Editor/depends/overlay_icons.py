#!/usr/bin/env python3
#this belongs in apps/components/Map_Editor/depends/overlay_icons.py - Version: 1
# X-Seti - Aug 2026 - IMG Factory 1.6 - Detailed colour SVG icons for Map Workshop's IPL Controls overlay buttons

"""
Detailed, full-colour SVG icons for Map Workshop's own IPL Controls
overlay toggle buttons (Water, Radar, Tcyc, etc.) - per Keith: "I'd
like well detailed 24x24 colour SVG icons, the ones we had before was
too simplyed."

Kept separate from both imgfactory_svg_icons.py (the shared, 221-icon
monochrome factory) and max_svg_icons.py (3ds Max-style two-tone
currentColor/currentAccent icons) - these are neither. Each icon uses
its own fixed, explicit colours intrinsic to what it represents
(water is blue, a radar screen is green, timecyc's own day/night
split uses real sky colours) rather than a theme-driven token, since
Keith's own request was specifically for real colour, not another
monochrome set. Still delegates to SVGIconFactory._create_icon() for
the actual QSvgRenderer-to-QPixmap-to-QIcon pipeline (the currentColor/
currentAccent substitution it performs is a harmless no-op here, since
none of these SVGs contain those tokens at all).

Import in map_workshop.py:
    from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons
"""

from PyQt6.QtGui import QIcon
from apps.methods.imgfactory_svg_icons import SVGIconFactory

##Methods list -
# OverlayIcons.water_icon
# OverlayIcons.radar_icon
# OverlayIcons.tcyc_icon


class OverlayIcons:
    """Detailed, full-colour icons for Map Workshop's IPL Controls
    overlay toggle buttons. Delegates rendering to SVGIconFactory.
    _create_icon() so the same QSvgRenderer pipeline and caching are
    reused without duplicating that infrastructure here."""

    @staticmethod
    def water_icon(size: int = 24) -> QIcon: #vers 1
        """Water overlay toggle - a blue droplet with a gradient body,
        a soft highlight, and two ripple lines at the base. Verified
        at both 24px (actual size) and 96px (4x zoom) before finalising
        - reads clearly as water at both."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <defs>
    <linearGradient id="waterBody" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#5ec8e8"/>
      <stop offset="100%" stop-color="#1a5f9c"/>
    </linearGradient>
    <linearGradient id="dropShine" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="#ffffff" stop-opacity="0.85"/>
      <stop offset="100%" stop-color="#ffffff" stop-opacity="0"/>
    </linearGradient>
  </defs>
  <path d="M12 2.2 C12 2.2 5 11.5 5 15.5 C5 19.6 8.1 22.4 12 22.4 C15.9 22.4 19 19.6 19 15.5 C19 11.5 12 2.2 12 2.2 Z"
        fill="url(#waterBody)" stroke="#0d3b63" stroke-width="0.6"/>
  <path d="M9 8.5 C7.6 11 6.6 13.2 6.6 15.2 C6.6 16.3 6.9 17.3 7.4 18.1 C7.0 16.9 6.9 15.6 7.6 13.8 C8.2 12.2 9.3 10.3 9 8.5 Z"
        fill="url(#dropShine)"/>
  <path d="M6.2 16.6 Q9 15.2 12 16.6 Q15 18 17.8 16.6" fill="none" stroke="#bfe9fb" stroke-width="0.9" stroke-linecap="round" opacity="0.85"/>
  <path d="M6.8 18.6 Q9.2 17.5 12 18.6 Q14.8 19.7 17.2 18.6" fill="none" stroke="#e8f8ff" stroke-width="0.7" stroke-linecap="round" opacity="0.6"/>
</svg>''', size, color="#000000")

    @staticmethod
    def radar_icon(size: int = 24) -> QIcon: #vers 2
        """Radar overlay toggle - a green radar screen with one bold
        ring, a wide bright sweep wedge, a centre dot, and a blip.
        Redesigned once already after the first pass (3 thin rings +
        crosshairs) proved too muddy to read at actual 24px size -
        fewer, bolder shapes read far better at that size than more
        structural detail, the same lesson the water icon's own
        gradient-and-highlight (not structural complexity) approach
        already confirmed."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <defs>
    <radialGradient id="radarBg" cx="50%" cy="50%" r="50%">
      <stop offset="0%" stop-color="#144022"/>
      <stop offset="100%" stop-color="#062010"/>
    </radialGradient>
    <linearGradient id="sweep" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="#a6ffc2" stop-opacity="1"/>
      <stop offset="100%" stop-color="#a6ffc2" stop-opacity="0"/>
    </linearGradient>
  </defs>
  <circle cx="12" cy="12" r="10.4" fill="url(#radarBg)" stroke="#6bffa0" stroke-width="1.3"/>
  <circle cx="12" cy="12" r="6" fill="none" stroke="#6bffa0" stroke-width="1" opacity="0.95"/>
  <path d="M12 12 L12 1.6 A10.4 10.4 0 0 1 19.4 5.2 Z" fill="url(#sweep)"/>
  <line x1="12" y1="12" x2="19.4" y2="5.2" stroke="#ffffff" stroke-width="1.6" stroke-linecap="round"/>
  <circle cx="12" cy="12" r="1.1" fill="#eafff0"/>
  <circle cx="15.8" cy="15.6" r="1.2" fill="#ffffff"/>
</svg>''', size, color="#000000")

    @staticmethod
    def tcyc_icon(size: int = 24) -> QIcon: #vers 2
        """Timecyc overlay toggle - a disc split cleanly down the
        middle: moon + stars on the night half, sun + rays on the day
        half. Redesigned once already after the first pass (a
        gradient-based split with an offset-circle crescent) read as
        an unintentional face/eye rather than a moon - a clean,
        sharp vertical split with simple, separate sun/moon shapes
        avoids that ambiguity entirely."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <defs>
    <clipPath id="discClip">
      <circle cx="12" cy="12" r="10.3"/>
    </clipPath>
  </defs>
  <circle cx="12" cy="12" r="10.3" fill="#12183a" stroke="#080b20" stroke-width="0.8"/>
  <g clip-path="url(#discClip)">
    <path d="M12 1.7 A10.3 10.3 0 0 0 12 22.3 Z" fill="#12183a"/>
    <circle cx="6.5" cy="16.5" r="0.5" fill="#e8ecff"/>
    <circle cx="8.6" cy="8.3" r="0.4" fill="#e8ecff"/>
    <circle cx="4.9" cy="10.6" r="0.35" fill="#e8ecff"/>
    <path d="M9.4 12 a3.1 3.1 0 1 0 3.6 -4.9 a3.9 3.9 0 1 1 -3.6 4.9 Z" fill="#f4f4f8"/>
    <path d="M12 1.7 A10.3 10.3 0 0 1 12 22.3 Z" fill="#ffb35c"/>
    <circle cx="15.5" cy="11.8" r="2.9" fill="#fff1c2"/>
    <g stroke="#fff1c2" stroke-width="1" stroke-linecap="round">
      <line x1="15.5" y1="6.9" x2="15.5" y2="5.4"/>
      <line x1="19.3" y1="8.3" x2="20.3" y2="7.3"/>
      <line x1="19.3" y1="15.3" x2="20.3" y2="16.3"/>
      <line x1="15.5" y1="16.7" x2="15.5" y2="18.2"/>
    </g>
  </g>
</svg>''', size, color="#000000")
