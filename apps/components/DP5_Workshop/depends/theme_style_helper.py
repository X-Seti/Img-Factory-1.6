#this belongs in apps/components/DP5_Workshop/depends/theme_style_helper.py - Version: 1
# X-Seti - Jul 2026 - DP5 Workshop - shared theme-aware panel stylesheet builder
"""
Shared helper so every dock panel module (bitmaps_widget.py,
brushcolors_widget.py, imagepalette_widget.py, userpalette_widget.py)
builds its theme-aware stylesheet the same way, instead of duplicating
the same QSS-construction logic four times.

build_panel_stylesheet(owner, object_name, extra_qss='') returns a QSS
string scoped to the given object_name via descendant selectors (e.g.
'QFrame#dp5_bitmaps_panel QLabel { ... }'), covering the common widget
types every panel has (QFrame background/border, QLabel, QCheckBox,
QPushButton, QSlider). extra_qss lets a specific panel append rules for
widgets only it has (QListWidget for Bitmaps, QComboBox/QScrollArea for
Image Palette, etc.) - already using the same object_name scoping, so
callers should write e.g. f"QFrame#{object_name} QListWidget {{ ... }}"
using the object_name passed back to them (see get_theme_values()).

No fallback colours: if the theme lookup doesn't return enough to
build a coherent style, returns None and the caller should skip
setStyleSheet() entirely rather than inventing colours.
"""


def get_theme_values(owner): #vers 1
    """Fetch the common colour set every panel stylesheet needs. Returns
    None if app_settings/get_theme_colors isn't reachable."""
    if not (owner.app_settings and hasattr(owner.app_settings, 'get_theme_colors')):
        return None
    tc = owner.app_settings.get_theme_colors()
    return {
        'panel_bg':   tc.get('panel_bg') or tc.get('bg_primary'),
        'border':     tc.get('border'),
        'text_col':   tc.get('text_primary'),
        'accent':     tc.get('accent_primary'),
        'btn_normal': tc.get('button_normal'),
        'btn_hover':  tc.get('button_hover'),
        'btn_press':  tc.get('button_pressed'),
        'btn_text':   tc.get('button_text_color') or tc.get('text_primary'),
        'alt_base':   tc.get('alternate_base'),
        'radius':     tc.get('button_border_radius', 4),
    }


def build_panel_stylesheet(owner, object_name, extra_qss=''): #vers 1
    """Build the full QSS string for a panel, or None if the theme
    lookup doesn't have enough to build a coherent style. object_name
    must match what the caller set via panel.setObjectName(...)."""
    v = get_theme_values(owner)
    if v is None:
        return None
    panel_bg, border, text_col = v['panel_bg'], v['border'], v['text_col']
    if not (panel_bg and border and text_col):
        return None

    ss = f"""
        QFrame#{object_name} {{
            background: {panel_bg};
            border: 1px solid {border};
        }}
        QFrame#{object_name} QLabel {{
            color: {text_col};
            background: transparent;
        }}
        QFrame#{object_name} QCheckBox {{
            color: {text_col};
            background: transparent;
        }}
    """
    accent, alt_base = v['accent'], v['alt_base']
    if accent and alt_base:
        ss += f"""
        QFrame#{object_name} QSlider::groove:horizontal {{
            background: {alt_base};
            height: 4px;
            border-radius: 2px;
        }}
        QFrame#{object_name} QSlider::handle:horizontal {{
            background: {accent};
            width: 12px;
            margin: -5px 0;
            border-radius: 6px;
        }}
        """
    btn_normal, btn_hover = v['btn_normal'], v['btn_hover']
    btn_press, btn_text = v['btn_press'], v['btn_text']
    if btn_normal and btn_hover and btn_press and btn_text:
        ss += f"""
        QFrame#{object_name} QPushButton {{
            background: {btn_normal};
            color: {btn_text};
            border: 1px solid {border};
            border-radius: {v['radius']}px;
        }}
        QFrame#{object_name} QPushButton:hover {{
            background: {btn_hover};
        }}
        QFrame#{object_name} QPushButton:pressed {{
            background: {btn_press};
        }}
        """
    if accent:
        ss += f"""
        QFrame#{object_name} QComboBox {{
            background: {btn_normal or panel_bg};
            color: {btn_text or text_col};
            border: 1px solid {border};
        }}
        QFrame#{object_name} QListWidget {{
            background: {panel_bg};
            color: {text_col};
            border: 1px solid {border};
        }}
        QFrame#{object_name} QListWidget::item:selected {{
            background: {accent};
        }}
        """
    return ss + extra_qss
