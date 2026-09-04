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
# OverlayIcons.tobj_icon
# OverlayIcons.dfx2d_icon
# OverlayIcons.paths_icon
# OverlayIcons.tracks_icon
# OverlayIcons.cull_icon
# OverlayIcons.zon_icon
# OverlayIcons.occlusion_icon
# OverlayIcons.sa_nodes_icon
# OverlayIcons.auzo_icon
# OverlayIcons.interior_icon


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

    @staticmethod
    def tobj_icon(size: int = 24) -> QIcon: #vers 1
        """Timed objects toggle - a glowing lamp bulb with a small
        clock badge, since TOBJ entries are objects (usually lamps/
        signs) that switch appearance by time of day."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <defs>
    <radialGradient id="tobjGlow" cx="50%" cy="38%" r="60%">
      <stop offset="0%" stop-color="#fff3b0"/>
      <stop offset="60%" stop-color="#ffcf3f"/>
      <stop offset="100%" stop-color="#e0952a"/>
    </radialGradient>
  </defs>
  <circle cx="12" cy="9.6" r="7.1" fill="url(#tobjGlow)" stroke="#a86a12" stroke-width="0.6"/>
  <path d="M9.4 9.6 Q12 7 14.6 9.6" fill="none" stroke="#a8560c" stroke-width="0.9" stroke-linecap="round" opacity="0.75"/>
  <rect x="9.6" y="15.6" width="4.8" height="2.4" rx="0.5" fill="#6b6b70"/>
  <rect x="10" y="18" width="4" height="2.6" rx="0.6" fill="#4a4a4e"/>
  <circle cx="18.2" cy="17.6" r="4.4" fill="#1c2340" stroke="#0c0f24" stroke-width="0.6"/>
  <line x1="18.2" y1="17.6" x2="18.2" y2="15.1" stroke="#e8ecff" stroke-width="0.9" stroke-linecap="round"/>
  <line x1="18.2" y1="17.6" x2="20.1" y2="18.2" stroke="#e8ecff" stroke-width="0.9" stroke-linecap="round"/>
</svg>''', size, color="#000000")

    @staticmethod
    def dfx2d_icon(size: int = 24) -> QIcon: #vers 1
        """2DFX toggle - a bright light-burst sparkle with two smaller
        companion sparkles, since 2DFX entries are mostly lights and
        particle/cosmetic effects."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <defs>
    <radialGradient id="dfxCore" cx="50%" cy="50%" r="50%">
      <stop offset="0%" stop-color="#ffffff"/>
      <stop offset="45%" stop-color="#ffe27a"/>
      <stop offset="100%" stop-color="#ff8a3d"/>
    </radialGradient>
  </defs>
  <path d="M12 1.2 C12.6 6.6 13.2 8.4 20.8 9.6 C13.2 10.8 12.6 12.6 12 18 C11.4 12.6 10.8 10.8 3.2 9.6 C10.8 8.4 11.4 6.6 12 1.2 Z"
        fill="url(#dfxCore)" stroke="#e0631a" stroke-width="0.4"/>
  <path d="M18.4 3.2 C18.7 5 19 5.3 20.9 5.7 C19 6.1 18.7 6.4 18.4 8.2 C18.1 6.4 17.8 6.1 15.9 5.7 C17.8 5.3 18.1 5 18.4 3.2 Z" fill="#ffe27a"/>
  <path d="M5 15.4 C5.25 16.7 5.45 16.9 6.9 17.2 C5.45 17.5 5.25 17.7 5 19 C4.75 17.7 4.55 17.5 3.1 17.2 C4.55 16.9 4.75 16.7 5 15.4 Z" fill="#ffe27a"/>
</svg>''', size, color="#000000")

    @staticmethod
    def paths_icon(size: int = 24) -> QIcon: #vers 1
        """Paths toggle - a winding route with an arrowhead and node
        dots along it, for vehicle/ped path IPL entries."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <defs>
    <linearGradient id="pathLine" x1="0" y1="1" x2="1" y2="0">
      <stop offset="0%" stop-color="#7a4dff"/>
      <stop offset="100%" stop-color="#38d0ff"/>
    </linearGradient>
  </defs>
  <path d="M3 19 C6 19 5 12 9 12 C13 12 12 5 17 5 C19 5 20 5.5 21 6.3"
        fill="none" stroke="url(#pathLine)" stroke-width="2.3" stroke-linecap="round"/>
  <path d="M18.6 3.6 L21.6 6 L18.3 8" fill="none" stroke="#38d0ff" stroke-width="2.1" stroke-linecap="round" stroke-linejoin="round"/>
  <circle cx="3" cy="19" r="2.1" fill="#ffffff" stroke="#7a4dff" stroke-width="1.3"/>
  <circle cx="9" cy="12" r="1.7" fill="#ffffff" stroke="#5a6dff" stroke-width="1.2"/>
  <circle cx="17" cy="5" r="1.7" fill="#ffffff" stroke="#38b8ff" stroke-width="1.2"/>
</svg>''', size, color="#000000")

    @staticmethod
    def tracks_icon(size: int = 24) -> QIcon: #vers 2
        """Tracks toggle - two bold rails with sleepers between them.
        Redesigned once already - the first attempt used a gradient
        on the vertical rails, which have a zero-width bounding box
        as a stroke target and silently failed to render at all;
        solid colour rails fixed it. Also cut the sleeper count down
        (5 to 2) after the first, denser attempt read as a muddy
        blob at actual 24px size - the same "fewer, bolder shapes"
        lesson the Radar icon's own redesign already confirmed."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <g stroke="#8a5330" stroke-width="2.2" stroke-linecap="round">
    <line x1="6" y1="7" x2="18" y2="7"/>
    <line x1="6" y1="17" x2="18" y2="17"/>
  </g>
  <line x1="6.2" y1="1.6" x2="6.2" y2="22.4" stroke="#c7cedb" stroke-width="3.2" stroke-linecap="round"/>
  <line x1="17.8" y1="1.6" x2="17.8" y2="22.4" stroke="#c7cedb" stroke-width="3.2" stroke-linecap="round"/>
</svg>''', size, color="#000000")

    @staticmethod
    def cull_icon(size: int = 24) -> QIcon: #vers 1
        """Cull toggle - a translucent isometric box with a bold white
        X, for cull zones that remove geometry within their bounds."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <defs>
    <linearGradient id="cullBox" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="#ff7a6b"/>
      <stop offset="100%" stop-color="#c22b3e"/>
    </linearGradient>
  </defs>
  <path d="M12 2.4 L20.4 7.2 L20.4 16.8 L12 21.6 L3.6 16.8 L3.6 7.2 Z"
        fill="url(#cullBox)" opacity="0.28" stroke="#ff8f80" stroke-width="1.1"/>
  <path d="M12 2.4 L20.4 7.2 L12 12 L3.6 7.2 Z" fill="#ffb3a8" opacity="0.35"/>
  <path d="M12 12 L12 21.6" stroke="#ff8f80" stroke-width="1.1"/>
  <path d="M12 12 L20.4 7.2" stroke="#ff8f80" stroke-width="1.1"/>
  <line x1="7" y1="8.6" x2="17" y2="16.6" stroke="#ffffff" stroke-width="2.4" stroke-linecap="round"/>
  <line x1="17" y1="8.6" x2="7" y2="16.6" stroke="#ffffff" stroke-width="2.4" stroke-linecap="round"/>
</svg>''', size, color="#000000")

    @staticmethod
    def zon_icon(size: int = 24) -> QIcon: #vers 1
        """Zones toggle - a planted flag inside a dashed boundary
        circle, for gameplay/info zone entries."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <defs>
    <linearGradient id="zonFlag" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="#5be08a"/>
      <stop offset="100%" stop-color="#189a54"/>
    </linearGradient>
  </defs>
  <circle cx="11.5" cy="13.5" r="8.6" fill="none" stroke="#3fce7a" stroke-width="1.4" stroke-dasharray="3.2 2.6" opacity="0.8"/>
  <line x1="8.4" y1="21.4" x2="8.4" y2="3.2" stroke="#6b6b70" stroke-width="1.4" stroke-linecap="round"/>
  <path d="M8.4 3.6 L18.6 6.4 L14.6 9.4 L18.6 12.2 L8.4 15 Z" fill="url(#zonFlag)" stroke="#0e6b34" stroke-width="0.5"/>
</svg>''', size, color="#000000")

    @staticmethod
    def occlusion_icon(size: int = 24) -> QIcon: #vers 2
        """Occlusion toggle - a solid wall block with a white
        prohibition circle-slash. Redesigned once already - the first
        attempt used a full, fine brick pattern that turned into a
        muddy blob at actual size, and a soft eye-slash that didn't
        read clearly; a solid wall colour plus a universally
        recognised "blocked" symbol reads far better at this size."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <defs>
    <linearGradient id="occlWall" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#c2ad8c"/>
      <stop offset="100%" stop-color="#8a7355"/>
    </linearGradient>
  </defs>
  <rect x="2.6" y="4.4" width="18.8" height="15.2" rx="1.2" fill="url(#occlWall)" stroke="#5a4c3a" stroke-width="0.8"/>
  <line x1="2.6" y1="12" x2="21.4" y2="12" stroke="#5a4c3a" stroke-width="0.8" opacity="0.5"/>
  <circle cx="12" cy="12" r="6.4" fill="rgba(20,20,20,0.35)" stroke="#ffffff" stroke-width="1.8"/>
  <line x1="7.6" y1="16.4" x2="16.4" y2="7.6" stroke="#ffffff" stroke-width="1.8" stroke-linecap="round"/>
</svg>''', size, color="#000000")

    @staticmethod
    def grge_icon(size: int = 24) -> QIcon: #vers 1
        """Garage toggle - a simple garage door/shutter shape (Aug 21
        2026, per Keith: "add support for GRGE") - orange to match
        _grge_box_color, distinct from cull/zone/occl/paths."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <path d="M3 10 L12 3 L21 10 V20 H3 Z" fill="#ffa600" stroke="#a86a00" stroke-width="1"/>
  <rect x="6" y="11" width="12" height="9" fill="#a86a00" stroke="#5a3a00" stroke-width="0.8"/>
  <line x1="6" y1="14" x2="18" y2="14" stroke="#ffa600" stroke-width="1"/>
  <line x1="6" y1="17" x2="18" y2="17" stroke="#ffa600" stroke-width="1"/>
</svg>''', size, color="#000000")

    @staticmethod
    def sa_nodes_icon(size: int = 24) -> QIcon: #vers 1
        """SA Nodes toggle - a small orange node network/graph (5
        connected nodes), visually distinct from Paths' own single
        purple/blue route with an arrowhead, for SA-specific ped/
        vehicle node entries."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <g stroke="#ffb545" stroke-width="1.6" stroke-linecap="round" opacity="0.9">
    <line x1="5" y1="6" x2="12" y2="12"/>
    <line x1="19" y1="5.5" x2="12" y2="12"/>
    <line x1="12" y1="12" x2="6.5" y2="18.5"/>
    <line x1="12" y1="12" x2="18" y2="17.5"/>
    <line x1="19" y1="5.5" x2="18" y2="17.5"/>
  </g>
  <circle cx="5" cy="6" r="2.1" fill="#ffd27a" stroke="#c97a12" stroke-width="0.8"/>
  <circle cx="19" cy="5.5" r="2.1" fill="#ffd27a" stroke="#c97a12" stroke-width="0.8"/>
  <circle cx="12" cy="12" r="2.5" fill="#fff1c2" stroke="#c97a12" stroke-width="0.9"/>
  <circle cx="6.5" cy="18.5" r="2.1" fill="#ffd27a" stroke="#c97a12" stroke-width="0.8"/>
  <circle cx="18" cy="17.5" r="2.1" fill="#ffd27a" stroke="#c97a12" stroke-width="0.8"/>
</svg>''', size, color="#000000")

    @staticmethod
    def auzo_icon(size: int = 24) -> QIcon: #vers 1
        """Audio zones toggle - a classic purple speaker cone with
        sound waves, for audio zone entries."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <defs>
    <linearGradient id="speakerBody" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="#c9b8ff"/>
      <stop offset="100%" stop-color="#7a4dff"/>
    </linearGradient>
  </defs>
  <path d="M4 9.6 L8.8 9.6 L14.4 4.8 L14.4 19.2 L8.8 14.4 L4 14.4 Z"
        fill="url(#speakerBody)" stroke="#4e28c9" stroke-width="0.7"/>
  <path d="M17.4 8.4 A6.4 6.4 0 0 1 17.4 15.6" fill="none" stroke="#a98bff" stroke-width="1.6" stroke-linecap="round"/>
  <path d="M19.8 5.4 A10.2 10.2 0 0 1 19.8 18.6" fill="none" stroke="#c9b8ff" stroke-width="1.4" stroke-linecap="round" opacity="0.8"/>
</svg>''', size, color="#000000")

    @staticmethod
    def interior_icon(size: int = 24) -> QIcon: #vers 1
        """Interior filter toggle - a house shape with a number
        badge, for filtering the viewport by an instance's own
        interior value (0 = exterior world; 1-13+ = building
        interiors, per GTAMods - 13 is reserved for pickups)."""
        return SVGIconFactory._create_icon('''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
  <defs>
    <linearGradient id="houseWall" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#e8dcc0"/>
      <stop offset="100%" stop-color="#c2ad8c"/>
    </linearGradient>
  </defs>
  <path d="M4 21 L4 10 L12 3 L20 10 L20 21 Z" fill="url(#houseWall)" stroke="#5a4c3a" stroke-width="1.4" stroke-linejoin="round"/>
  <rect x="9.6" y="14" width="4.8" height="7" rx="0.4" fill="#5a4c3a"/>
  <circle cx="18.4" cy="17.6" r="4.4" fill="#7a4dff" stroke="#f4f1ff" stroke-width="1.2"/>
</svg>''', size, color="#000000")

