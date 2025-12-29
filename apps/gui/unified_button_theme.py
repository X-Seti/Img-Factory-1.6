#this belongs in gui/ unified_button_theme.py - Version: 1
# X-Seti - Aug06 2025 - IMG Factory 1.5 - Unified Button Theme System
# REPLACES: gui/pastel_button_theme.py with universal light/dark theme support
# SUPPORTS: Light themes (pastel), Dark themes (inverted/high-contrast)

#!/usr/bin/env python3
"""
Unified Button Theme System - Light & Dark Theme Support
Replaces old apply_pastel_theme_to_buttons with universal theme system
Automatically detects light/dark themes and applies appropriate styling
"""

##Methods list -
# apply_unified_button_theme
# get_button_theme_colors
# is_dark_theme
# lighten_color
# darken_color

def apply_unified_button_theme(app, app_settings):
    """
    Apply unified button theme that works for both light and dark themes
    Replaces old apply_pastel_theme_to_buttons function
    """
    # Get theme colors and detect if dark theme
    theme_colors = app_settings.get_theme_colors()
    is_dark = is_dark_theme(theme_colors)
    
    # Get appropriate button colors based on theme type
    button_colors = get_button_theme_colors(theme_colors, is_dark)
    
    # Build unified button stylesheet
    unified_style = f"""
    /* Base button styling */
    QPushButton {{
        color: {button_colors['text_color']};
        font-weight: bold;
        border: 1px solid {button_colors['border_color']};
        border-radius: 4px;
        padding: 6px 12px;
        min-height: 28px;
    }}
    
    QPushButton:hover {{
        background-color: {button_colors['hover_bg']};
        border-color: {button_colors['hover_border']};
    }}
    
    QPushButton:pressed {{
        background-color: {button_colors['pressed_bg']};
        border-color: {button_colors['pressed_border']};
    }}
    
    QPushButton:disabled {{
        color: {button_colors['disabled_text']};
        background-color: {button_colors['disabled_bg']};
        border-color: {button_colors['disabled_border']};
    }}
    
    /* Action-specific button styling */
    QPushButton[action-type="import"] {{
        background-color: {button_colors['import_bg']};
        color: {button_colors['import_text']};
        border-color: {button_colors['import_border']};
    }}
    
    QPushButton[action-type="import"]:hover {{
        background-color: {button_colors['import_hover']};
    }}
    
    QPushButton[action-type="export"] {{
        background-color: {button_colors['export_bg']};
        color: {button_colors['export_text']};
        border-color: {button_colors['export_border']};
    }}
    
    QPushButton[action-type="export"]:hover {{
        background-color: {button_colors['export_hover']};
    }}
    
    QPushButton[action-type="remove"] {{
        background-color: {button_colors['remove_bg']};
        color: {button_colors['remove_text']};
        border-color: {button_colors['remove_border']};
    }}
    
    QPushButton[action-type="remove"]:hover {{
        background-color: {button_colors['remove_hover']};
    }}
    
    QPushButton[action-type="update"] {{
        background-color: {button_colors['update_bg']};
        color: {button_colors['update_text']};
        border-color: {button_colors['update_border']};
    }}
    
    QPushButton[action-type="update"]:hover {{
        background-color: {button_colors['update_hover']};
    }}
    
    QPushButton[action-type="convert"] {{
        background-color: {button_colors['convert_bg']};
        color: {button_colors['convert_text']};
        border-color: {button_colors['convert_border']};
    }}
    
    QPushButton[action-type="convert"]:hover {{
        background-color: {button_colors['convert_hover']};
    }}
    
    /* Special button styling for dark themes */
    {button_colors.get('extra_dark_css', '')}
    """
    
    # Apply the unified style
    existing_style = app.styleSheet()
    app.setStyleSheet(existing_style + "\n" + unified_style)


def get_button_theme_colors(theme_colors, is_dark):
    """
    Get appropriate button colors based on light/dark theme
    Returns different color schemes for light vs dark themes
    """
    if is_dark:
        # DARK THEME - High contrast with light text
        return {
            # Base button colors (dark theme)
            'text_color': theme_colors.get('text_primary', '#ffffff'),
            'border_color': theme_colors.get('border', '#555555'),
            'hover_bg': theme_colors.get('button_hover', '#505050'),
            'hover_border': theme_colors.get('border', '#666666'),
            'pressed_bg': theme_colors.get('button_pressed', '#404040'),
            'pressed_border': theme_colors.get('border', '#777777'),
            
            # Disabled state
            'disabled_text': '#888888',
            'disabled_bg': '#2d2d2d',
            'disabled_border': '#333333',
            
            # Action buttons (dark theme) - Solid colors with white text
            'import_bg': theme_colors.get('action_import', '#1565c0'),
            'import_text': '#ffffff',
            'import_border': darken_color(theme_colors.get('action_import', '#1565c0')),
            'import_hover': lighten_color(theme_colors.get('action_import', '#1565c0')),
            
            'export_bg': theme_colors.get('action_export', '#2e7d32'),
            'export_text': '#ffffff', 
            'export_border': darken_color(theme_colors.get('action_export', '#2e7d32')),
            'export_hover': lighten_color(theme_colors.get('action_export', '#2e7d32')),
            
            'remove_bg': theme_colors.get('action_remove', '#c62828'),
            'remove_text': '#ffffff',
            'remove_border': darken_color(theme_colors.get('action_remove', '#c62828')),
            'remove_hover': lighten_color(theme_colors.get('action_remove', '#c62828')),
            
            'update_bg': theme_colors.get('action_update', '#e65100'),
            'update_text': '#ffffff',
            'update_border': darken_color(theme_colors.get('action_update', '#e65100')),
            'update_hover': lighten_color(theme_colors.get('action_update', '#e65100')),
            
            'convert_bg': theme_colors.get('action_convert', '#6a1b9a'),
            'convert_text': '#ffffff',
            'convert_border': darken_color(theme_colors.get('action_convert', '#6a1b9a')),
            'convert_hover': lighten_color(theme_colors.get('action_convert', '#6a1b9a')),
            
            # Extra CSS for dark theme polish
            'extra_dark_css': """
            /* Dark theme enhancements */
            QPushButton {
                background-color: #404040;
            }
            QPushButton:focus {
                border-color: #FFECEE;
                outline: none;
            }
            """
        }
    else:
        # LIGHT THEME - Pastel colors with dark text
        return {
            # Base button colors (light theme)
            'text_color': theme_colors.get('text_primary', '#000000'),
            'border_color': theme_colors.get('border', '#cccccc'),
            'hover_bg': theme_colors.get('button_hover', '#e0e0e0'),
            'hover_border': theme_colors.get('border', '#bbbbbb'),
            'pressed_bg': theme_colors.get('button_pressed', '#d0d0d0'),
            'pressed_border': theme_colors.get('border', '#aaaaaa'),
            
            # Disabled state
            'disabled_text': '#999999',
            'disabled_bg': '#f5f5f5',
            'disabled_border': '#e0e0e0',
            
            # Action buttons (light theme) - Pastel backgrounds with dark text
            'import_bg': theme_colors.get('action_import', '#e3f2fd'),
            'import_text': theme_colors.get('action_import_text', '#1565c0'),
            'import_border': theme_colors.get('action_import', '#e3f2fd'),
            'import_hover': lighten_color(theme_colors.get('action_import', '#e3f2fd')),
            
            'export_bg': theme_colors.get('action_export', '#e8f5e8'),
            'export_text': theme_colors.get('action_export_text', '#2e7d32'),
            'export_border': theme_colors.get('action_export', '#e8f5e8'),
            'export_hover': lighten_color(theme_colors.get('action_export', '#e8f5e8')),
            
            'remove_bg': theme_colors.get('action_remove', '#ffebee'),
            'remove_text': theme_colors.get('action_remove_text', '#c62828'),
            'remove_border': theme_colors.get('action_remove', '#ffebee'),
            'remove_hover': lighten_color(theme_colors.get('action_remove', '#ffebee')),
            
            'update_bg': theme_colors.get('action_update', '#fff3e0'),
            'update_text': theme_colors.get('action_update_text', '#e65100'),
            'update_border': theme_colors.get('action_update', '#fff3e0'),
            'update_hover': lighten_color(theme_colors.get('action_update', '#fff3e0')),
            
            'convert_bg': theme_colors.get('action_convert', '#f3e5f5'),
            'convert_text': theme_colors.get('action_convert_text', '#6a1b9a'),
            'convert_border': theme_colors.get('action_convert', '#f3e5f5'),
            'convert_hover': lighten_color(theme_colors.get('action_convert', '#f3e5f5')),
            
            # No extra CSS needed for light theme
            'extra_dark_css': ''
        }


def is_dark_theme(theme_colors):
    """
    Detect if the current theme is a dark theme
    Returns True for dark themes, False for light themes
    """
    # Check primary background color luminance
    bg_color = theme_colors.get('bg_primary', '#ffffff')
    
    # Remove # if present
    if bg_color.startswith('#'):
        bg_color = bg_color[1:]
    
    # Handle 3-character hex codes
    if len(bg_color) == 3:
        bg_color = ''.join([c*2 for c in bg_color])
    
    try:
        # Convert to RGB
        r = int(bg_color[0:2], 16) / 255.0
        g = int(bg_color[2:4], 16) / 255.0  
        b = int(bg_color[4:6], 16) / 255.0
        
        # Calculate relative luminance
        def luminance_component(c):
            return c / 12.92 if c <= 0.03928 else pow((c + 0.055) / 1.055, 2.4)
        
        luminance = 0.2126 * luminance_component(r) + 0.7152 * luminance_component(g) + 0.0722 * luminance_component(b)
        
        # Dark theme if luminance < 0.5
        return luminance < 0.5
    except:
        # Fallback: check for common dark theme names/colors
        theme_name = theme_colors.get('name', '').lower()
        return 'dark' in theme_name or bg_color.lower() in ['000000', '1e1e1e', '2d2d30']


def lighten_color(hex_color, factor=1.2):
    """Lighten a hex color by a factor"""
    if not hex_color.startswith('#'):
        hex_color = '#' + hex_color
        
    try:
        # Convert to RGB
        r = int(hex_color[1:3], 16)
        g = int(hex_color[3:5], 16)  
        b = int(hex_color[5:7], 16)
        
        # Lighten each component
        r = min(255, int(r * factor))
        g = min(255, int(g * factor))
        b = min(255, int(b * factor))
        
        return f"#{r:02x}{g:02x}{b:02x}"
    except:
        return hex_color


def darken_color(hex_color, factor=0.8):
    """Darken a hex color by a factor"""  
    if not hex_color.startswith('#'):
        hex_color = '#' + hex_color
        
    try:
        # Convert to RGB
        r = int(hex_color[1:3], 16)
        g = int(hex_color[3:5], 16)
        b = int(hex_color[5:7], 16)
        
        # Darken each component
        r = max(0, int(r * factor))
        g = max(0, int(g * factor))
        b = max(0, int(b * factor))
        
        return f"#{r:02x}{g:02x}{b:02x}"
    except:
        return hex_color


# Backward compatibility alias
def apply_pastel_theme_to_buttons(app, app_settings):
    """Backward compatibility alias for old function name"""
    apply_unified_button_theme(app, app_settings)