#this belongs in apps/methods/ui_color.py - Version: 3
# X-Seti - September25 2026 - IMG Factory 1.6 - Theme-aware UI colours
"""
Shared theme colour lookup for widgets (app_settings, else palette).
"""

##Methods list -
# depth_range
# get_ui_color
# water_depth_rgb


def get_ui_color(widget, key): #vers 2
    """Theme QColor for key from app_settings, else widget palette."""
    try:
        app_settings = getattr(widget, 'app_settings', None) or \
            getattr(getattr(widget, 'main_window', None), 'app_settings', None)
        if app_settings and hasattr(app_settings, 'get_ui_color'):
            return app_settings.get_ui_color(key)
    except Exception:
        pass
    pal = widget.palette()
    role = {'viewport_bg':    pal.ColorRole.Base,
            'viewport_text':  pal.ColorRole.PlaceholderText,
            'border':         pal.ColorRole.Mid,
            'accent_primary': pal.ColorRole.Highlight,
            'panel_bg':       pal.ColorRole.Window,
            'bg_primary':     pal.ColorRole.Window,
            'bg_secondary':   pal.ColorRole.AlternateBase,
            'text_primary':   pal.ColorRole.WindowText,
            }.get(key, pal.ColorRole.WindowText)
    return pal.color(role)


def water_depth_rgb(t): #vers 1
    """Depth gang colour (r, g, b 0-255) for t 0 (low/deep) .. 1 (high)."""
    t = max(0.0, min(1.0, t))
    return int(8 + 112 * t), int(30 + 170 * t), int(80 + 175 * t)


def depth_range(zs): #vers 1
    """Gauge Z range: 2nd-98th percentile, padded when flat."""
    zs = sorted(zs) or [0.0]
    lo, hi = zs[int(len(zs) * 0.02)], zs[min(len(zs) - 1, int(len(zs) * 0.98))]
    return (lo - 5.0, hi + 5.0) if hi - lo < 10 else (lo, hi)
