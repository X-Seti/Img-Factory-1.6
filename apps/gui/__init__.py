#this belongs in gui/ __init__.py - Version: 19
# X-Seti - Aug06 2025 - IMG Factory 1.5 - Clean GUI Module with Universal Theme
# UPDATED: Removed old StatusBarTheme, now uses apply_universal_status_theme
# PRESERVES: All functionality while fixing dark theme issues

#!/usr/bin/env python3
"""
IMG Factory GUI Module - Clean Version with Universal Theme Support
Modular GUI components for IMG Factory 1.5 with proper dark/light theme integration
"""

# Import main GUI components
from .main_window import IMGFactoryMainWindow, create_main_window
from .panel_manager import PanelManager
from .tear_off import (
    TearOffPanel,
    TearOffPanelManager,
    add_panel_menu_to_menubar
)
from .log_panel import create_log_panel, setup_logging_for_main_window
from .status_bar import create_status_bar, create_enhanced_status_bar, apply_universal_status_theme
from apps.core.dialogs import (
    show_about_dialog, show_export_options_dialog,
    show_import_options_dialog, show_error_dialog, show_warning_dialog,
    show_question_dialog, show_info_dialog, show_progress_dialog
)

# Version info
__version__ = "1.5.0"
__author__ = "X-Seti"

##Methods list -
# apply_gui_theme
# create_complete_gui
# get_module_info

def apply_gui_theme(main_window, theme_name="default"):
    """Apply universal theme to GUI components - FIXED for dark theme support"""
    try:
        # Apply universal theme to status bar using AppSettings
        if hasattr(main_window, 'app_settings') and hasattr(main_window, 'statusBar'):
            # Use the universal theme system
            apply_universal_status_theme(main_window.statusBar(), main_window.app_settings)
            main_window.log_message(f"✅ Applied universal theme to status bar: {theme_name}")
        else:
            # Fallback: Create theme colors based on theme name
            colors = {}
            if theme_name == "dark" or "dark" in theme_name.lower():
                colors = {
                    'bg_secondary': '#2d2d30',
                    'border': '#3e3e42', 
                    'text_secondary': '#ffffff',
                    'bg_tertiary': '#383838',
                    'accent_primary': '#FFECEE',
                    'button_normal': '#404040',
                    'button_hover': '#505050',
                    'button_pressed': '#606060'
                }
            else:
                colors = {
                    'bg_secondary': '#f8f9fa',
                    'border': '#dee2e6',
                    'text_secondary': '#495057',
                    'bg_tertiary': '#e9ecef',
                    'accent_primary': '#1976d2',
                    'button_normal': '#e3f2fd',
                    'button_hover': '#bbdefb',
                    'button_pressed': '#90caf9'
                }
            
            # Create minimal AppSettings-like object for fallback
            class ThemeFallback:
                def get_theme_colors(self):
                    return colors
            
            # Apply fallback theme
            if hasattr(main_window, 'statusBar'):
                apply_universal_status_theme(main_window.statusBar(), ThemeFallback())
                main_window.log_message(f"✅ Applied fallback theme to status bar: {theme_name}")
        
        # Apply theme to table if available
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'apply_table_theme'):
            main_window.gui_layout.apply_table_theme()
        
        main_window.log_message(f"Applied GUI theme: {theme_name}")
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"⚠️ Theme application error: {str(e)}")
        else:
            print(f"Theme application error: {str(e)}")
            
        # Emergency fallback with direct CSS
        if hasattr(main_window, 'statusBar'):
            if "dark" in theme_name.lower():
                main_window.statusBar().setStyleSheet("""
                    QStatusBar {
                        background-color: #2d2d30;
                        border-top: 1px solid #3e3e42;
                        color: #ffffff;
                    }
                    QStatusBar::item { border: none; }
                    QLabel { color: #ffffff; }
                    QProgressBar {
                        border: 1px solid #3e3e42;
                        background-color: #383838;
                        color: #ffffff;
                    }
                    QProgressBar::chunk {
                        background-color: #FFECEE;
                    }
                """)
            else:
                main_window.statusBar().setStyleSheet("""
                    QStatusBar {
                        background-color: #f8f9fa;
                        border-top: 1px solid #dee2e6;
                        color: #495057;
                    }
                    QStatusBar::item { border: none; }
                    QLabel { color: #495057; }
                    QProgressBar {
                        border: 1px solid #dee2e6;
                        background-color: #e9ecef;
                        color: #495057;
                    }
                    QProgressBar::chunk {
                        background-color: #1976d2;
                    }
                """)


def create_complete_gui(app_settings=None):
    """Create complete GUI with all components and proper theme integration"""
    # Create main window
    main_window = create_main_window(app_settings)
    
    # Setup logging
    setup_logging_for_main_window(main_window)
    
    # Apply initial theme if app_settings available
    if app_settings:
        current_theme = app_settings.current_settings.get("theme", "img_factory")
        apply_gui_theme(main_window, current_theme)
    
    # Initial status
    main_window.log_message("GUI components initialized")
    main_window.show_status("Ready")
    
    return main_window


def get_module_info():
    """Get module information"""
    return MODULE_INFO


# Export main classes and functions
__all__ = [
    # Main window
    'IMGFactoryMainWindow',
    'create_main_window',
    
    # Panel controls
    'create_control_panel',
    
    # Panel classes
    'PanelManager',
    
    # Tear-off system
    'TearOffPanel',
    'TearOffPanelManager', 
    'add_panel_menu_to_menubar',
    
    # GUI components
    'create_log_panel',
    'create_status_bar',
    'create_enhanced_status_bar',
    'apply_universal_status_theme',
    
    # Utility functions
    'setup_logging_for_main_window',
    'apply_gui_theme',
    'create_complete_gui',
    'get_module_info',
    
    # Dialogs
    'show_about_dialog',
    'show_export_options_dialog',
    'show_import_options_dialog',
    'show_error_dialog',
    'show_warning_dialog',
    'show_question_dialog',
    'show_info_dialog',
    'show_progress_dialog',
]

# Module information
MODULE_INFO = {
    'name': 'IMG Factory GUI',
    'version': __version__,
    'author': __author__,
    'description': 'GUI components with universal theme support for IMG Factory',
    'components': [
        'main_window - Main application window with theme integration',
        'tear_off - Tear-off panel functionality',
        'log_panel - Activity log and message display',
        'status_bar - Status information with universal theme support',
        'dialogs - Common dialog windows',
    ]
}
