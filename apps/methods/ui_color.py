#this belongs in apps/methods/ui_color.py - Version: 2
# X-Seti - September25 2026 - IMG Factory 1.6 - Theme-aware UI colours
"""
Shared theme colour lookup for widgets (app_settings, else palette).
"""

##Methods list -
# get_ui_color


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
