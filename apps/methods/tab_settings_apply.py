#this belongs in methods/tab_settings_apply.py - Version: 1
# X-Seti - February21 2026 - IMG Factory 1.6 - Tab Settings Apply

"""
Tab Settings Apply - Applies tab height, width, style and position to all QTabWidgets
"""

from PyQt6.QtWidgets import QTabWidget

##Methods list -
# apply_tab_settings
# _build_tab_stylesheet
# _get_tab_widgets


def _get_tab_widgets(main_window): #vers 1
    """Find all QTabWidget instances in the main window"""
    tabs = []
    for attr in vars(main_window).values():
        if isinstance(attr, QTabWidget):
            tabs.append(attr)
    if hasattr(main_window, 'gui_layout'):
        for attr in vars(main_window.gui_layout).values():
            if isinstance(attr, QTabWidget):
                tabs.append(attr)
    return tabs


def _build_tab_stylesheet(height, min_width, style, theme_colors): #vers 1
    """Build QTabBar stylesheet from settings and theme colors"""
    bg = theme_colors.get('bg_secondary', '#f0f0f0')
    bg_active = theme_colors.get('accent_primary', '#0078d4')
    text = theme_colors.get('text_primary', '#000000')
    text_active = theme_colors.get('selection_text', '#ffffff')
    border = theme_colors.get('border', '#cccccc')

    if style == "rounded":
        radius = "6px"
        border_bottom = "none"
    elif style == "square":
        radius = "0px"
        border_bottom = f"1px solid {border}"
    else:  # default
        radius = "3px 3px 0 0"
        border_bottom = "none"

    return f"""
    QTabBar::tab {{
        background: {bg};
        color: {text};
        height: {height}px;
        min-width: {min_width}px;
        padding: 2px 8px;
        border: 1px solid {border};
        border-bottom: {border_bottom};
        border-radius: {radius};
        margin-right: 2px;
    }}
    QTabBar::tab:selected {{
        background: {bg_active};
        color: {text_active};
        border-bottom: none;
    }}
    QTabBar::tab:hover:!selected {{
        background: {theme_colors.get('bg_tertiary', '#e0e0e0')};
    }}
    """


def apply_tab_settings(main_window, img_settings): #vers 1
    """Apply tab height, width, style and position to all QTabWidgets"""
    try:
        height = img_settings.get("tab_height", 24)
        min_width = img_settings.get("tab_min_width", 100)
        style = img_settings.get("tab_style", "default")
        position = img_settings.get("tab_position", "top")

        theme_colors = {}
        if hasattr(main_window, 'app_settings'):
            theme_name = main_window.app_settings.current_settings.get('theme', 'default')
            theme_colors = main_window.app_settings.get_theme_colors(theme_name) or {}

        stylesheet = _build_tab_stylesheet(height, min_width, style, theme_colors)

        pos_map = {
            "top": QTabWidget.TabPosition.North,
            "bottom": QTabWidget.TabPosition.South,
        }
        tab_pos = pos_map.get(position, QTabWidget.TabPosition.North)

        for tab_widget in _get_tab_widgets(main_window):
            tab_widget.setTabPosition(tab_pos)
            tab_widget.setStyleSheet(stylesheet)

        print(f"Tab settings applied: height={height} min_width={min_width} style={style} position={position}")

    except Exception as e:
        print(f"apply_tab_settings failed: {e}")
