#this belongs in core/theme_integration.py - Version: 3
# X-Seti - February21 2026 - IMG Factory 1.6 - Theme Integration

"""
Theme Integration Fix - Connect app_settings_system.py theme switching to GUI updates
"""

##Methods list -
# connect_theme_system
# on_theme_changed
# update_all_ui_elements
# update_gui_layout_theme
# update_status_bar_theme

def connect_theme_system(main_window): #vers 1
    """Connect theme system to main window for live updates"""
    try:
        # Connect to settings dialog theme change signal
        if hasattr(main_window, 'app_settings') and hasattr(main_window.app_settings, 'themeChanged'):
            main_window.app_settings.themeChanged.connect(lambda theme_name: on_theme_changed(main_window, theme_name))
            print("Theme system connected to main window")
            return True
            
        # Alternative: Connect to settings dialog if it exists
        if hasattr(main_window, 'settings_dialog'):
            main_window.settings_dialog.themeChanged.connect(lambda theme_name: on_theme_changed(main_window, theme_name))
            print("Theme system connected via settings dialog")
            return True
            
        print("Theme system connection not found")
        return False
        
    except Exception as e:
        print(f"Theme system connection failed: {str(e)}")
        return False


def on_theme_changed(main_window, theme_name: str): #vers 3
    """Handle theme change signal from settings system"""
    try:
        print(f"Theme changed to: {theme_name}")

        # Update SVG icon color from text_primary
        icon_color = '#000000'
        if hasattr(main_window, 'app_settings'):
            theme_colors = main_window.app_settings.get_theme_colors(theme_name)
            if theme_colors and 'text_primary' in theme_colors:
                from apps.methods.imgfactory_svg_icons import SVGIconFactory
                icon_color = theme_colors.get('text_primary', '#000000')
                SVGIconFactory.set_theme_color(icon_color)
                print(f"SVG icon color updated to: {icon_color}")

        # Refresh icons on whichever layout is active
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'refresh_icons'):
            main_window.gui_layout.refresh_icons(icon_color)

        # Re-apply menu bar styling
        if hasattr(main_window, 'menu_bar_system') and hasattr(main_window.menu_bar_system, '_apply_menu_bar_styling'):
            main_window.menu_bar_system._apply_menu_bar_styling()

        # Update GUI layout buttons and colors
        update_gui_layout_theme(main_window, theme_name)

        # Update status bar theme
        update_status_bar_theme(main_window, theme_name)

        # Update all other UI elements
        update_all_ui_elements(main_window, theme_name)

    except Exception as e:
        print(f"Theme change failed: {str(e)}")


def update_gui_layout_theme(main_window, theme_name: str): #vers 1
    """Update GUI layout with new theme"""
    try:
        # Update GUI layout if it exists and has theme method
        if hasattr(main_window, 'gui_layout'):
            if hasattr(main_window.gui_layout, 'set_theme_mode'):
                main_window.gui_layout.set_theme_mode(theme_name)
                print(f"GUI layout updated to {theme_name} theme")
                return True
            elif hasattr(main_window.gui_layout, 'apply_theme'):
                main_window.gui_layout.apply_theme(theme_name) 
                return True
                
        print("GUI layout theme method not found")
        return False
        
    except Exception as e:
        print(f"GUI layout theme update failed: {str(e)}")
        return False

def update_status_bar_theme(main_window, theme_name: str): #vers 1
    """Update status bar/info bar theme"""
    try:
        updated_elements = []
        
        # Update status bar elements
        status_elements = [
            'file_count_label', 'file_size_label', 'format_version_label',
            'additional_info_label', 'status_label', 'progress_bar'
        ]
        
        for element_name in status_elements:
            if hasattr(main_window, element_name):
                element = getattr(main_window, element_name)
                if hasattr(element, 'setStyleSheet'):
                    # Apply theme stylesheet
                    if hasattr(main_window, 'app_settings'):
                        stylesheet = main_window.app_settings.get_stylesheet()
                        element.setStyleSheet(stylesheet)
                        updated_elements.append(element_name)
        
        # Update GUI layout status elements if they exist
        if hasattr(main_window, 'gui_layout'):
            gui_status_elements = [
                'file_count_label', 'file_size_label', 'format_version_label'
            ]
            for element_name in gui_status_elements:
                if hasattr(main_window.gui_layout, element_name):
                    element = getattr(main_window.gui_layout, element_name)
                    if hasattr(element, 'setStyleSheet'):
                        if hasattr(main_window, 'app_settings'):
                            stylesheet = main_window.app_settings.get_stylesheet()
                            element.setStyleSheet(stylesheet)
                            updated_elements.append(f"gui_layout.{element_name}")
        
        if updated_elements:
            print(f"Status bar updated: {', '.join(updated_elements)}")
            return True
        else:
            print("No status bar elements found to update")
            return False
            
    except Exception as e:
        print(f"Status bar theme update failed: {str(e)}")
        return False

def update_all_ui_elements(main_window, theme_name: str): #vers 1
    """Update all other UI elements with new theme"""
    try:
        updated_count = 0
        
        # Get new stylesheet and theme colors
        if hasattr(main_window, 'app_settings'):
            stylesheet = main_window.app_settings.get_stylesheet()
            theme_colors = main_window.app_settings.get_theme_colors(theme_name)
        else:
            print("No app_settings found for stylesheet")
            return False
        
        # Update main table with theme-specific styling - CRITICAL FIX
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            
            # Apply base stylesheet
            table.setStyleSheet(stylesheet)
            
            # Apply theme-specific table styling
            if theme_colors and 'bg_primary' in theme_colors:
                bg_color = theme_colors.get('bg_primary', '#FFFFFF')
                text_color = theme_colors.get('text_primary', '#000000')
                accent_color = theme_colors.get('accent_primary', '#0078D4')
                
                table_style = f"""
                QTableWidget {{
                    background-color: {bg_color};
                    color: {text_color};
                    gridline-color: {accent_color};
                    selection-background-color: {accent_color};
                    selection-color: white;
                    alternate-background-color: {bg_color}AA;
                }}
                QTableWidget::item {{
                    padding: 4px;
                    border-bottom: 1px solid {accent_color}40;
                }}
                QHeaderView::section {{
                    background-color: {accent_color};
                    color: white;
                    font-weight: bold;
                    border: 1px solid {accent_color};
                    padding: 6px;
                }}
                """
                table.setStyleSheet(table_style)
                updated_count += 1
        
        # Update log window if it exists
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'log_output'):
            main_window.gui_layout.log_output.setStyleSheet(stylesheet)
            updated_count += 1
            
        # Update splitters
        if hasattr(main_window, 'gui_layout'):
            splitters = ['main_splitter', 'vertical_splitter', 'horizontal_splitter']
            for splitter_name in splitters:
                if hasattr(main_window.gui_layout, splitter_name):
                    splitter = getattr(main_window.gui_layout, splitter_name)
                    if hasattr(splitter, 'setStyleSheet'):
                        splitter.setStyleSheet(stylesheet)
                        updated_count += 1
        
        # Update file tree/directory tree if it exists
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'file_tree'):
            main_window.gui_layout.file_tree.setStyleSheet(stylesheet)
            updated_count += 1
            
        print(f"Updated {updated_count} additional UI elements")
        return updated_count > 0
        
    except Exception as e:
        print(f"UI elements theme update failed: {str(e)}")
        return False

# Integration function to call from main IMG Factory
def integrate_theme_system(main_window): #vers 1
    """Main integration function - call this AFTER GUI setup is complete"""
    try:
        success = connect_theme_system(main_window)
        if success:
            print("Theme integration system ready")
        else:
            print("Theme integration partially ready")
        return success
        
    except Exception as e:
        print(f"Theme integration failed: {str(e)}")
        return False

# Delayed integration for calling early in startup
def integrate_theme_system_delayed(main_window): #vers 1
    """Delayed integration - call this early, checks when GUI is ready"""
    def try_integration():
        if hasattr(main_window, 'gui_layout'):
            return integrate_theme_system(main_window)
        return False
    
    # Try immediate integration
    if try_integration():
        return True
    
    # If failed, try with a small delay
    from PyQt6.QtCore import QTimer
    timer = QTimer()
    timer.singleShot(100, lambda: try_integration())
    print("Theme integration scheduled for when GUI is ready")
    return True
