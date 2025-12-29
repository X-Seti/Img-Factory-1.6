#!/usr/bin/env python3
"""
X-Seti - October23 2025 - Complete Theme Updater
Add scrollbar, selection, menu, and button data to all JSON theme files
"""
#this belongs in root /update_themes_script.py - Version: 2

import json
import os
from pathlib import Path
from typing import Dict, Any

def get_smart_colors_for_theme(theme_colors: Dict[str, str]) -> Dict[str, str]:
    """
    Generate smart scrollbar and selection colors based on existing theme colors.
    Also adds missing base colors with smart defaults.
    """
    # Get base colors from theme
    bg_primary = theme_colors.get('bg_primary', '#ffffff')
    bg_secondary = theme_colors.get('bg_secondary', '#f5f5f5')
    bg_tertiary = theme_colors.get('bg_tertiary', '#e9ecef')
    panel_bg = theme_colors.get('panel_bg', '#f0f0f0')
    accent_primary = theme_colors.get('accent_primary', '#FFECEE')
    accent_secondary = theme_colors.get('accent_secondary', '#FFD4D9')
    border_color = theme_colors.get('border', '#cccccc')
    text_primary = theme_colors.get('text_primary', '#000000')
    text_secondary = theme_colors.get('text_secondary', '#666666')
    text_accent = theme_colors.get('text_accent', '#0066cc')
    button_normal = theme_colors.get('button_normal', '#e0e0e0')
    button_hover = theme_colors.get('button_hover', '#d0d0d0')

    # Helper function to darken/lighten colors
    def adjust_color(hex_color: str, factor: float) -> str:
        """Darken (negative factor) or lighten (positive factor) a hex color"""
        hex_color = hex_color.lstrip('#')
        if len(hex_color) == 3:
            hex_color = ''.join([c*2 for c in hex_color])

        try:
            r = int(hex_color[0:2], 16)
            g = int(hex_color[2:4], 16)
            b = int(hex_color[4:6], 16)

            if factor > 0:  # Lighten
                r = min(255, int(r + (255 - r) * factor))
                g = min(255, int(g + (255 - g) * factor))
                b = min(255, int(b + (255 - b) * factor))
            else:  # Darken
                factor = abs(factor)
                r = max(0, int(r * (1 - factor)))
                g = max(0, int(g * (1 - factor)))
                b = max(0, int(b * (1 - factor)))

            return f"{r:02x}{g:02x}{b:02x}"
        except:
            return "777777"  # Fallback gray

    # Generate smart colors - includes base colors AND calculated colors
    return {
        # Base colors (add if missing)
        "bg_primary": bg_primary.lstrip('#'),
        "bg_secondary": bg_secondary.lstrip('#'),
        "bg_tertiary": bg_tertiary.lstrip('#'),
        "panel_bg": panel_bg.lstrip('#'),
        "text_primary": text_primary.lstrip('#'),
        "text_secondary": text_secondary.lstrip('#'),
        "text_accent": text_accent.lstrip('#'),
        "accent_primary": accent_primary.lstrip('#'),
        "accent_secondary": accent_secondary.lstrip('#'),
        "border": border_color.lstrip('#'),
        "button_normal": button_normal.lstrip('#'),
        "button_hover": button_hover.lstrip('#'),

        # NEW: Calculated colors based on existing theme
        "button_pressed": adjust_color(button_hover, -0.15),
        "button_text_color": text_primary.lstrip('#'),
        "button_text_hover": text_primary.lstrip('#'),
        "button_text_pressed": text_primary.lstrip('#'),

        # Splitter colors
        "splitter_color_background": adjust_color(bg_secondary, -0.1),
        "splitter_color_shine": adjust_color(bg_secondary, 0.1),
        "splitter_color_shadow": adjust_color(bg_secondary, -0.2),

        # Scrollbar colors
        "scrollbar_background": adjust_color(bg_primary, -0.05),
        "scrollbar_handle": adjust_color(border_color, -0.1),
        "scrollbar_handle_hover": adjust_color(border_color, -0.2),
        "scrollbar_handle_pressed": adjust_color(border_color, -0.3),
        "scrollbar_border": border_color.lstrip('#'),

        # File window selection colors
        "selection_background": accent_primary.lstrip('#'),
        "selection_text": "ffffff" if is_dark_color(accent_primary) else "000000",

        # File window background alternating row colors (zebra striping)
        "table_row_even": bg_primary.lstrip('#'),
        "table_row_odd": adjust_color(bg_primary, 0.03),
        "alternate_row": adjust_color(bg_primary, 0.03),  # Alias for table_row_odd
        
        # Grid and UI element colors
        "grid": adjust_color(border_color, -0.05),
        
        # Status colors
        "success": "4caf50",
        "warning": "ff9800", 
        "error": "f44336",
        
        # Pin colors
        "pin_default": "757575",
        "pin_highlight": accent_primary.lstrip('#'),
        
        # Action button colors (light for light themes, solid for dark themes)
        "action_import": "e3f2fd" if not is_dark_color(bg_primary) else "2196f3",
        "action_export": "e8f5e8" if not is_dark_color(bg_primary) else "4caf50",
        "action_remove": "ffebee" if not is_dark_color(bg_primary) else "f44336",
        "action_update": "fff3e0" if not is_dark_color(bg_primary) else "ff9800",
        "action_convert": "f3e5f5" if not is_dark_color(bg_primary) else "9c27b0",
        
        # Panel background colors
        "panel_entries": adjust_color(bg_tertiary, 0.05),
        "panel_filter": adjust_color(bg_tertiary, 0.08),
        "toolbar_bg": bg_secondary.lstrip('#'),

        # Hardcoded layout values that should be themeable
        "panel_margins": "5",
        "panel_spacing": "2",
        "button_min_height": "30",
        "button_padding": "6",
        "button_border_radius": "4",
        "groupbox_border_radius": "5",
        "splitter_handle_width": "8",
        "splitter_handle_height": "6",

        # Table styling
        "table_header_height": "25",
        "table_row_height": "22",
        "table_border_width": "1",
        "table_grid_line_width": "1",

        # Status messages
        "status_ready": "Ready",
        "status_no_img": "No IMG loaded",
        "status_loading": "Loading...",
        "status_complete": "Complete",
        "status_error": "Error",

        # Default tab labels
        "tab_dff": "DFF",
        "tab_col": "COL",
        "tab_both": "Both",

        # Information bar labels
        "info_file_label": "File:",
        "info_type_label": "Type:",
        "info_items_label": "Items:",
        "info_size_label": "Size:",
        "info_no_file": "No file loaded",
        "info_unknown_type": "Unknown",

        # Group box titles
        "group_file_info": "File Information",
        "group_img_files": "IMG Files",
        "group_file_entries": "File Entries",
        "group_editing_options": "Editing Options",
        "group_filter_search": "Filter & Search",
        "group_status_log": "Status & Activity Log",

        # Filter options
        "filter_all_files": "All Files",
        "filter_dff_models": "DFF Models",
        "filter_txd_textures": "TXD Textures",
        "filter_col_collision": "COL Collision",
        "filter_ifp_animations": "IFP Animations",

        # Search placeholder
        "search_placeholder": "Search filename...",
        "log_placeholder": "Activity log will appear here...",

        # Column headers
        "col_filename": "Filename",
        "col_type": "Type",
        "col_size": "Size",
        "col_offset": "Offset",
        "col_version": "Version",
        "col_compression": "Compression",
        "col_status": "Status"
    }

def is_dark_color(hex_color: str) -> bool:
    """Check if a color is dark (luminance < 0.5)"""
    hex_color = hex_color.lstrip('#')
    if len(hex_color) == 3:
        hex_color = ''.join([c*2 for c in hex_color])
    
    try:
        r = int(hex_color[0:2], 16) / 255.0
        g = int(hex_color[2:4], 16) / 255.0
        b = int(hex_color[4:6], 16) / 255.0
        
        # Calculate relative luminance
        def luminance_component(c):
            return c / 12.92 if c <= 0.03928 else pow((c + 0.055) / 1.055, 2.4)
        
        luminance = 0.2126 * luminance_component(r) + 0.7152 * luminance_component(g) + 0.0722 * luminance_component(b)
        return luminance < 0.5
    except:
        return False

def add_menu_structure_to_theme(theme_data: Dict[str, Any]):
    """Add menu structure to theme if it doesn't exist"""
    if 'menus' not in theme_data:
        theme_data['menus'] = {
            "menu_structure": [
                "File", "Edit", "Dat", "IMG", "Model", 
                "Texture", "Collision", "Item Definition",
                "Item Placement", "Entry", "Settings", "Help"
            ],
            "menu_items": {
                "File": [
                    {"text": "New IMG...", "icon": "document-new", "shortcut": "Ctrl+N", "action": "create_new_img"},
                    {"text": "Open IMG...", "icon": "document-open", "shortcut": "Ctrl+O", "action": "open_img_file"},
                    {"text": "Open COL...", "icon": "document-open", "action": "open_col_file"},
                    {"separator": True},
                    {"text": "Close", "icon": "window-close", "action": "close_img_file"},
                    {"separator": True},
                    {"text": "Exit", "icon": "application-exit", "shortcut": "Ctrl+Q", "action": "close"}
                ],
                "IMG": [
                    {"text": "Rebuild", "icon": "document-save", "action": "rebuild_img"},
                    {"text": "Save Entry...", "icon": "document-save-entry", "action": "save_img_entry"},
                    {"separator": True},
                    {"text": "Merge IMG Files", "icon": "document-merge"},
                    {"text": "Split IMG File", "icon": "edit-cut"},
                    {"separator": True},
                    {"text": "IMG Properties", "icon": "dialog-information"}
                ],
                "Entry": [
                    {"text": "Import Files...", "icon": "go-down", "action": "import_files"},
                    {"text": "Export Selected...", "icon": "go-up", "action": "export_selected"},
                    {"text": "Export All...", "icon": "go-up", "action": "export_all"},
                    {"separator": True},
                    {"text": "Remove Selected", "icon": "list-remove", "action": "remove_selected"},
                    {"text": "Rename Entry", "icon": "edit"}
                ],
                "Settings": [
                    {"text": "Preferences...", "icon": "preferences-other", "action": "show_settings"},
                    {"text": "Themes...", "icon": "applications-graphics", "action": "show_theme_settings"},
                    {"separator": True},
                    {"text": "Manage Templates...", "icon": "folder", "action": "manage_templates"}
                ],
                "Help": [
                    {"text": "User Guide", "icon": "help-contents"},
                    {"text": "About IMG Factory", "icon": "help-about", "action": "show_about"}
                ]
            }
        }

def add_fonts_to_theme(theme_data: Dict[str, Any]):
    """Add fonts section to theme if it doesn't exist"""
    if 'fonts' not in theme_data:
        theme_data['fonts'] = {
            "default_font_family": "Segoe UI",
            "default_font_size": 9,
            "default_font_weight": "Normal",
            "title_font_family": "Arial",
            "title_font_size": 14,
            "title_font_weight": "Bold",
            "panel_font_family": "Arial",
            "panel_font_size": 10,
            "panel_font_weight": "Bold",
            "button_font_family": "Arial",
            "button_font_size": 10,
            "button_font_weight": "Normal",
            "menu_font_family": "Segoe UI",
            "menu_font_size": 9,
            "menu_font_weight": "Normal",
            "infobar_font_family": "Courier New",
            "infobar_font_size": 9,
            "infobar_font_weight": "Normal",
            "table_font_family": "Segoe UI",
            "table_font_size": 9,
            "table_font_weight": "Normal",
            "tooltip_font_family": "Segoe UI",
            "tooltip_font_size": 8,
            "tooltip_font_weight": "Normal"
        }

def add_button_structure_to_theme(theme_data: Dict[str, Any]):
    """Add button panel structure to theme if it doesn't exist"""
    if 'button_panels' not in theme_data:
        theme_data['button_panels'] = {
            "img_files": [
                {"text": "Open", "action": "open", "icon": "document-open", "color": "#E3F2FD"},
                {"text": "Close", "action": "close", "icon": "window-close", "color": "#FFF3E0"},
                {"text": "Close All", "action": "close_all", "icon": "edit-clear", "color": "#FFF3E0"},
                {"text": "Rebuild", "action": "rebuild", "icon": "view-refresh", "color": "#E8F5E8"},
                {"text": "Save Entry...", "icon": "document-save-entry", "action": "save_img_entry", "color": "#E8F5E8"},
                {"text": "Rebuild All", "action": "rebuild_all", "icon": "document-save", "color": "#E8F5E8"},
                {"text": "Merge", "action": "merge", "icon": "document-merge", "color": "#F3E5F5"},
                {"text": "Split", "action": "split", "icon": "edit-cut", "color": "#F3E5F5"},
                {"text": "Convert", "action": "convert", "icon": "transform", "color": "#FFF8E1"}
            ],
            "file_entries": [
                {"text": "Import", "action": "import", "icon": "document-import", "color": "#E1F5FE"},
                {"text": "Import via", "action": "import_via", "icon": "document-import", "color": "#E1F5FE"},
                {"text": "Export", "action": "export", "icon": "document-export", "color": "#E0F2F1"},
                {"text": "Export via", "action": "export_via", "icon": "document-export", "color": "#E0F2F1"},
                {"text": "Remove", "action": "remove", "icon": "edit-delete", "color": "#FFEBEE"},
                {"text": "Remove All", "action": "remove_all", "icon": "edit-clear", "color": "#FFEBEE"},
                {"text": "Refresh", "action": "update", "icon": "view-refresh", "color": "#F9FBE7"},
                {"text": "Quick Export", "action": "quick_export", "icon": "document-export", "color": "#E0F2F1"},
                {"text": "Pin selected", "action": "pin", "icon": "view-pin", "color": "#FCE4EC"}
            ],
            "editing_options": [
                {"text": "Col Edit", "action": "col_edit", "icon": "edit-col", "color": "#FFE0B2"},
                {"text": "Txd Edit", "action": "txd_edit", "icon": "edit-txd", "color": "#E8EAF6"},
                {"text": "Dff Edit", "action": "dff_edit", "icon": "edit-dff", "color": "#F1F8E9"},
                {"text": "Ipf Edit", "action": "ipf_edit", "icon": "edit-ipf", "color": "#FFF3E0"},
                {"text": "IPL Edit", "action": "ipl_edit", "icon": "edit-ipl", "color": "#FFEBEE"},
                {"text": "IDE Edit", "action": "ide_edit", "icon": "edit-ide", "color": "#E0F2F1"},
                {"text": "Dat Edit", "action": "dat_edit", "icon": "dat-editor", "color": "#F3E5F5"},
                {"text": "Zons Edit", "action": "zons_edit", "icon": "zon-editor", "color": "#E1F5FE"},
                {"text": "Weap Edit", "action": "weap_edit", "icon": "weap-editor", "color": "#FFF8E1"},
                {"text": "Vehi Edit", "action": "vehi_edit", "icon": "vehi-editor", "color": "#E8F5E8"},
                {"text": "Radar Map", "action": "radar_map", "icon": "radar-map", "color": "#FCE4EC"},
                {"text": "Paths Map", "action": "paths_map", "icon": "paths-map", "color": "#F9FBE7"},
                {"text": "Waterpro", "action": "waterpro", "icon": "waterpro", "color": "#E3F2FD"}
            ]
        }

def update_theme_file(theme_path: Path) -> bool:
    """
    Update a single theme JSON file with new colors, menus, and layout data
    """
    try:
        print(f"Processing: {theme_path.name}")
        
        # Read existing theme
        with open(theme_path, 'r', encoding='utf-8') as f:
            theme_data = json.load(f)
        
        # Ensure colors section exists
        if 'colors' not in theme_data:
            theme_data['colors'] = {}
        
        colors = theme_data['colors']
        
        # Generate smart colors based on existing theme
        new_colors = get_smart_colors_for_theme(colors)
        
        # Add new colors (only if they don't already exist)
        added_colors = []
        for color_key, color_value in new_colors.items():
            if color_key not in colors:
                colors[color_key] = color_value
                added_colors.append(color_key)
        
        # Add menu structure
        menu_added = False
        if 'menus' not in theme_data:
            add_menu_structure_to_theme(theme_data)
            menu_added = True
        
        # Add button panel structure
        buttons_added = False
        if 'button_panels' not in theme_data:
            add_button_structure_to_theme(theme_data)
            buttons_added = True
        
        # Add fonts structure
        fonts_added = False
        if 'fonts' not in theme_data:
            add_fonts_to_theme(theme_data)
            fonts_added = True
        
        if added_colors or menu_added or buttons_added or fonts_added:
            # Write updated theme back to file
            with open(theme_path, 'w', encoding='utf-8') as f:
                json.dump(theme_data, f, indent=2, ensure_ascii=False)
            
            summary = []
            if added_colors:
                summary.append(f"{len(added_colors)} colors")
            if menu_added:
                summary.append("menu structure")
            if buttons_added:
                summary.append("button panels")
            if fonts_added:
                summary.append("fonts")
            
            print(f"✅ Added {', '.join(summary)}")
            return True
        else:
            print(f"✅ No new data needed")
            return False
            
    except Exception as e:
        print(f"❌ Error processing {theme_path.name}: {str(e)}")
        return False

def backup_themes_folder():
    """Create a backup of the themes folder"""
    themes_dir = Path("themes")
    backup_dir = Path("themes_backup")
    
    if themes_dir.exists():
        try:
            if backup_dir.exists():
                import shutil
                shutil.rmtree(backup_dir)
            
            import shutil
            shutil.copytree(themes_dir, backup_dir)
            print(f"✅ Backup created: {backup_dir}")
            return True
        except Exception as e:
            print(f"⚠️  Backup failed: {str(e)}")
            return False
    return False

def main():
    """Main function to update all theme files"""
    print("=" * 60)
    print("IMG Factory Complete Theme Updater - Version 2")
    print("Adds colors, menus, buttons, and layout data to JSON themes")
    print("=" * 60)
    
    # Find themes directory
    themes_dir = Path("themes")
    if not themes_dir.exists():
        print("❌ themes/ directory not found!")
        print("   Make sure you're running this script from the IMG Factory root folder")
        return
    
    # Create backup
    print("\nCreating backup...")
    backup_themes_folder()
    
    # Find all JSON files in themes directory
    theme_files = list(themes_dir.glob("*.json"))
    if not theme_files:
        print("❌ No JSON theme files found in themes/ directory")
        return
    
    print(f"\n🎨 Found {len(theme_files)} theme files")
    print("-" * 60)
    
    # Process each theme file
    updated_count = 0
    for theme_file in sorted(theme_files):
        if update_theme_file(theme_file):
            updated_count += 1
    
    # Summary
    print("=" * 60)
    print(f"📊 Summary:")
    print(f"   • Files processed: {len(theme_files)}")
    print(f"   • Files updated: {updated_count}")
    print(f"   • Files unchanged: {len(theme_files) - updated_count}")
    
    if updated_count > 0:
        print(f"\n✅ Successfully updated {updated_count} theme files!")
        print(f"💾 Original files backed up to: themes_backup/")
        print(f"\n📋 New data added to themes:")
        print(f"   COLORS:")
        print(f"      • Base colors (bg_primary, bg_secondary, bg_tertiary, panel_bg)")
        print(f"      • Text colors (text_primary, text_secondary, text_accent)")
        print(f"      • Accent colors (accent_primary, accent_secondary)")
        print(f"      • Button colors (button_normal, button_hover, button_pressed)")
        print(f"      • Selection colors (selection_background, selection_text)")
        print(f"      • Table row colors (table_row_even, table_row_odd, alternate_row)")
        print(f"      • Status colors (success, warning, error)")
        print(f"      • Pin colors (pin_default, pin_highlight)")
        print(f"      • Action colors (action_import, action_export, etc.)")
        print(f"      • Grid and panel colors (grid, panel_entries, panel_filter)")
        print(f"      • Splitter colors (background, shine, shadow)")
        print(f"      • Scrollbar colors (background, handle, hover, pressed, border)")
        print(f"      • Layout values (margins, spacing, sizes)")
        print(f"      • UI text labels (status messages, tooltips, etc.)")
        print(f"   MENUS:")
        print(f"      • Complete menu structure with icons and shortcuts")
        print(f"   BUTTON PANELS:")
        print(f"      • IMG Files, File Entries, and Editing Options buttons")
        print(f"   FONTS:")
        print(f"      • Default, Title, Panel, Button, Menu fonts")
        print(f"      • Infobar, Table, and Tooltip fonts")
        print(f"      • Font family, size, and weight for each type")
    else:
        print(f"\n✅ All theme files already have the required data!")

    print(f"\n🔄 Restart IMG Factory to see the updated theme data.")
    print("=" * 60)


if __name__ == "__main__":
    main()
