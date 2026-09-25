#this belongs in apps/methods/ui_color.py - Version: 1
# X-Seti - September25 2026 - IMG Factory 1.6 - Theme-aware UI colours
"""
Shared theme colour lookup for widgets (app_settings, else palette).
"""

##Methods list -
# get_ui_color


def get_ui_color(widget, key): #vers 1
    """Theme QColor for key from app_settings, else widget palette."""
    try:
        app_settings = getattr(widget, 'app_settings', None) or \
            getattr(getattr(widget, 'main_window', None), 'app_settings', None)
        if app_settings and hasattr(app_settings, 'get_ui_color'):
            return app_settings.get_ui_color(key)
    except Exception:
        pass
    pal = widget.palette()
    if key == 'viewport_bg':
        return pal.color(pal.ColorRole.Base)
    if key == 'viewport_text':
        return pal.color(pal.ColorRole.PlaceholderText)
    if key == 'border':
        return pal.color(pal.ColorRole.Mid)
    return pal.color(pal.ColorRole.WindowText)
