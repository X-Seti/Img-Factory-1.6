# this belongs in gui/pastel_button_theme.py - version 3
#!/usr/bin/env python3
"""
X-Seti - June26 2025 - Pastel Button Theme - Fixed to use theme text colors
"""

def apply_pastel_theme_to_buttons(app, app_settings):
    """
    Apply pastel theme that respects JSON theme button text colors
    """
    # Get theme colors
    theme = app_settings.get_theme()
    colors = theme["colors"]
    
    # Use theme's button text color
    button_text = colors.get("button_text_color", "#000000")
    
    # Simple pastel stylesheet that uses theme text color
    pastel_style = f"""
    QPushButton {{
        color: {button_text} !important;
        font-weight: bold;
    }}
    
    QPushButton[action-type="import"] {{
        background-color: #E3F2FD;
        color: {button_text} !important;
    }}
    
    QPushButton[action-type="export"] {{
        background-color: #E8F5E8;
        color: {button_text} !important;
    }}
    
    QPushButton[action-type="remove"] {{
        background-color: #FFEBEE;
        color: {button_text} !important;
    }}
    
    QPushButton[action-type="update"] {{
        background-color: #FFF3E0;
        color: {button_text} !important;
    }}
    
    QPushButton[action-type="convert"] {{
        background-color: #F3E5F5;
        color: {button_text} !important;
    }}
    """
    
    # Apply the style
    existing_style = app.styleSheet()
    app.setStyleSheet(existing_style + "\n" + pastel_style)
