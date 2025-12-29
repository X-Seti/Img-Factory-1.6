#this belongs in components/file_extraction_functions.py - version 12
# X-Seti - July10 2025 - Img Factory 1.5
# Integration for enhanced file filtering and extraction

from PyQt6.QtWidgets import QMenu, QMessageBox
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QAction
from typing import List, Dict

# list methods -
# add_extraction_to_menu
# analyze_col_from_table
# edit_col_from_table
# edit_ide_file
# get_selected_entries_for_extraction
# integrate_extraction
# open_ide_in_text_editor
# patch_img_loading_for_extraction
# setup_complete_extraction
# setup_extraction_context_menu
# setup_extraction_methods
# show_dff_info
# view_ide_definitions
# view_txd_textures


def add_extraction_to_menu(main_window): #vers 3
    """Add extraction options to main menu"""
    try:
        if not hasattr(main_window, 'menuBar') or not main_window.menuBar():
            return False

        menubar = main_window.menuBar()

        # Find or create Tools menu
        tools_menu = None
        for action in menubar.actions():
            if action.text() == "Tools":
                tools_menu = action.menu()
                break

        if not tools_menu:
            tools_menu = menubar.addMenu("Tools")

        # Add extraction submenu
        extract_submenu = tools_menu.addMenu("🚀 File Extraction")

        # Extract selected
        extract_selected_action = QAction("Extract Selected Files", main_window)
        extract_selected_action.setShortcut("Ctrl+E")
        extract_selected_action.triggered.connect(main_window.extract_selected_files)
        extract_submenu.addAction(extract_selected_action)

        # Extract all
        extract_all_action = QAction("Extract All Files", main_window)
        extract_all_action.setShortcut("Ctrl+Shift+E")
        extract_all_action.triggered.connect(main_window.extract_all_files)
        extract_submenu.addAction(extract_all_action)

        extract_submenu.addSeparator()

        # Quick extract by type
        quick_extract_submenu = extract_submenu.addMenu("📁 Quick Extract by Type")

        type_actions = [
            ("Models (DFF)", main_window.quick_extract_dff_files),
            ("Textures (TXD)", main_window.quick_extract_txd_files),
            ("Collision (COL)", main_window.quick_extract_col_files),
            ("Definitions (IDE)", main_window.quick_extract_ide_files),
        ]

        for name, method in type_actions:
            action = QAction(name, main_window)
            action.triggered.connect(method)
            quick_extract_submenu.addAction(action)

        return True

    except Exception as e:
        main_window.log_message(f"❌ Error adding extraction to menu: {str(e)}")
        return False


def analyze_col_from_table(main_window, row: int): #vers 15
    """Analyze COL file from table row"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                if entry.name.lower().endswith('.col'):
                    # Use existing COL integration
                    from apps.components.col_integration import analyze_col_from_img_entry
                    analyze_col_from_img_entry(main_window, row)
                else:
                    QMessageBox.warning(main_window, "Not a COL File", "Selected file is not a COL file.")
    except Exception as e:
        QMessageBox.critical(main_window, "Error", f"Failed to analyze COL file: {str(e)}")


def edit_col_from_table(main_window, row: int): #vers 22
    """Edit COL file from table row"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                if entry.name.lower().endswith('.col'):
                    # Use existing COL integration
                    from apps.components.col_integration import edit_col_from_img_entry
                    edit_col_from_img_entry(main_window, row)
                else:
                    QMessageBox.warning(main_window, "Not a COL File", "Selected file is not a COL file.")
    except Exception as e:
        QMessageBox.critical(main_window, "Error", f"Failed to edit COL file: {str(e)}")


def edit_ide_file(main_window, row: int): #vers 13
    """Edit IDE file in IDE editor"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                if entry.name.lower().endswith('.ide'):
                    # Use IDE editor if available
                    if hasattr(main_window, 'open_ide_editor'):
                        main_window.open_ide_editor(entry.data, entry.name)
                    else:
                        # Fallback to text editor
                        open_ide_in_text_editor(main_window, entry)
                else:
                    QMessageBox.warning(main_window, "Not an IDE File", "Selected file is not an IDE definition file.")
    except Exception as e:
        QMessageBox.critical(main_window, "Error", f"Failed to edit IDE file: {str(e)}")


def get_selected_entries_for_extraction(main_window) -> List: #vers 2
    """Get currently selected entries for extraction"""
    try:
        selected_entries = []
        
        if hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            selected_rows = []
            
            # Get selected rows
            selection = table.selectionModel()
            if selection:
                for index in selection.selectedRows():
                    selected_rows.append(index.row())
            
            # Get entries for selected rows
            if hasattr(main_window, 'current_img') and main_window.current_img:
                for row in selected_rows:
                    if 0 <= row < len(main_window.current_img.entries):
                        selected_entries.append(main_window.current_img.entries[row])
        
        return selected_entries
        
    except Exception as e:
        main_window.log_message(f"⚠️ Error getting selected entries: {str(e)}")
        return []


def integrate_extraction_system(main_window): #vers 3
    """Integrate file extraction system into main window"""
    try:
        from apps.core.file_type_filter import (
            integrate_file_filtering, show_extraction_dialog,
            update_filter_statistics
        )

        # Setup file filtering
        if integrate_file_filtering(main_window):
            main_window.log_message("✅ File filtering integrated")

            # Add extraction methods to main window
            setup_extraction_methods(main_window)

            # Add extraction context menu
            setup_extraction_context_menu(main_window)

            # Add extraction to main menu
            add_extraction_to_menu(main_window)

            # Update filter statistics when IMG loads
            if hasattr(main_window, 'current_img') and main_window.current_img:
                update_filter_statistics(main_window)

            return True

        return False

    except Exception as e:
        main_window.log_message(f"❌ Failed to integrate extraction system: {str(e)}")
        return False


def open_ide_in_text_editor(main_window, entry): #vers 3
    """Fallback to open IDE in system text editor"""
    import tempfile
    import subprocess
    import sys
    
    temp_file = tempfile.NamedTemporaryFile(suffix='.ide', delete=False, mode='wb')
    temp_file.write(entry.data)
    temp_file.close()
    
    try:
        # Try to open with default text editor
        if sys.platform.startswith('win'):
            subprocess.run(['notepad', temp_file.name])
        elif sys.platform.startswith('darwin'):
            subprocess.run(['open', '-t', temp_file.name])
        else:
            # Linux - try common editors
            editors = ['gedit', 'kate', 'mousepad', 'nano']
            for editor in editors:
                try:
                    subprocess.run([editor, temp_file.name])
                    break
                except FileNotFoundError:
                    continue
            else:
                QMessageBox.information(main_window, "Editor Not Found", 
                                      f"File extracted to: {temp_file.name}\nPlease open with your preferred text editor.")
        
        main_window.log_message(f"📝 Opened IDE file for editing: {entry.name}")
        
    except Exception as e:
        QMessageBox.warning(main_window, "Editor Error", 
                          f"Could not open text editor.\nFile extracted to: {temp_file.name}")
        main_window.log_message(f"⚠️ Editor error for {entry.name}: {str(e)}")


def patch_img_loading_for_extraction(main_window): #vers 15
    """Patch IMG loading to update extraction filter statistics"""
    try:
        # Store original method if it exists
        if hasattr(main_window, 'populate_entries_table'):
            original_populate = main_window.populate_entries_table
            
            def populate_with_extraction_update():
                # Call original populate method
                result = original_populate()
                
                # Update extraction filter statistics
                if hasattr(main_window, 'update_extraction_filter_stats'):
                    main_window.update_extraction_filter_stats()
                
                return result
            
            main_window.populate_entries_table = populate_with_extraction_update
        
        return True
        
    except Exception as e:
        main_window.log_message(f"Error patching IMG loading for extraction: {str(e)}")
        return False


# Main integration function
def setup_complete_extraction_integration(main_window): #vers 25
    """Complete integration of extraction system"""
    try:
        # Integrate GTA file editors first
        from apps.components.File_Editor.gta_file_editors import integrate_gta_file_editors
        integrate_gta_file_editors(main_window)
        
        # Integrate extraction system
        if integrate_extraction_system(main_window):
            # Patch IMG loading to update filter stats
            patch_img_loading_for_extraction(main_window)
            
            main_window.log_message("✅ Complete extraction system integration finished")
            return True
        
        return False
        
    except Exception as e:
        main_window.log_message(f"❌ Complete extraction integration failed: {str(e)}")
        return False

def setup_extraction_methods(main_window): #vers 7
    """Add extraction methods to main window"""
    try:
        from apps.core.file_type_filter import show_extraction_dialog, update_filter_statistics

        def extract_selected_files():
            """Extract currently selected files"""
            selected_entries = get_selected_entries_for_extraction(main_window)
            if selected_entries:
                show_extraction_dialog(main_window, selected_entries)
            else:
                QMessageBox.information(main_window, "No Selection", "Please select files to extract.")

        def extract_all_files():
            """Extract all files from current IMG"""
            if hasattr(main_window, 'current_img') and main_window.current_img:
                all_entries = main_window.current_img.entries
                show_extraction_dialog(main_window, all_entries)
            else:
                QMessageBox.warning(main_window, "No IMG", "Please load an IMG file first.")

        def extract_by_type(file_type: str):
            """Extract files of specific type"""
            if not hasattr(main_window, 'current_img') or not main_window.current_img:
                QMessageBox.warning(main_window, "No IMG", "Please load an IMG file first.")
                return

            # Filter entries by type
            filtered_entries = []
            for entry in main_window.current_img.entries:
                entry_type = entry.name.split('.')[-1].upper() if '.' in entry.name else 'Unknown'
                if entry_type == file_type.upper():
                    filtered_entries.append(entry)

            if filtered_entries:
                show_extraction_dialog(main_window, filtered_entries)
            else:
                QMessageBox.information(main_window, "No Files", f"No {file_type} files found in current IMG.")

        def quick_extract_col_files():
            """Quick extract all COL files"""
            extract_by_type('COL')

        def quick_extract_dff_files():
            """Quick extract all DFF files"""
            extract_by_type('DFF')

        def quick_extract_ide_files():
            """Quick extract all IDE files"""
            extract_by_type('IDE')

        def quick_extract_txd_files():
            """Quick extract all TXD files"""
            extract_by_type('TXD')

        def update_extraction_filter_stats():
            """Update filter statistics when IMG changes"""
            update_filter_statistics(main_window)

        # Add methods to main window
        main_window.extract_selected_files = extract_selected_files
        main_window.extract_all_files = extract_all_files
        main_window.extract_by_type = extract_by_type
        main_window.quick_extract_ide_files = quick_extract_ide_files
        main_window.quick_extract_col_files = quick_extract_col_files
        main_window.quick_extract_dff_files = quick_extract_dff_files
        main_window.quick_extract_txd_files = quick_extract_txd_files
        main_window.update_extraction_filter_stats = update_extraction_filter_stats

        return True

    except Exception as e:
        main_window.log_message(f"❌ Error setting up extraction methods: {str(e)}")
        return False


def show_dff_info(main_window, row: int): #vers 4
    """Show DFF model information"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                if entry.name.lower().endswith('.dff'):
                    # Basic DFF info for now
                    info_text = f"""DFF Model Information:

Name: {entry.name}
Size: {len(entry.data):,} bytes
Type: RenderWare DFF Model

This feature will be expanded to show:
- Model geometry details
- Texture references
- Animation data
- LOD information"""

                    QMessageBox.information(main_window, f"DFF Info - {entry.name}", info_text)
                else:
                    QMessageBox.warning(main_window, "Not a DFF File", "Selected file is not a DFF model.")
    except Exception as e:
        QMessageBox.critical(main_window, "Error", f"Failed to show DFF info: {str(e)}")


def view_ide_definitions(main_window, row: int): #vers 3
    """View IDE file definitions"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                if entry.name.lower().endswith('.ide'):
                    # Parse basic IDE info
                    try:
                        ide_content = entry.data.decode('ascii', errors='ignore')
                        lines = ide_content.split('\n')

                        # Count different section types
                        sections = {}
                        current_section = None

                        for line in lines:
                            line = line.strip()
                            if line and not line.startswith('#'):
                                if line in ['objs', 'tobj', 'weap', 'hier', 'anim', 'cars', 'peds', 'end']:
                                    if line == 'end':
                                        current_section = None
                                    else:
                                        current_section = line
                                        sections[line] = 0
                                elif current_section:
                                    sections[current_section] = sections.get(current_section, 0) + 1

                        # Format summary
                        summary_parts = []
                        for section, count in sections.items():
                            section_names = {
                                'objs': 'Objects',
                                'tobj': 'Timed Objects',
                                'weap': 'Weapons',
                                'hier': 'Hierarchies',
                                'anim': 'Animations',
                                'cars': 'Vehicles',
                                'peds': 'Pedestrians'
                            }
                            name = section_names.get(section, section.upper())
                            summary_parts.append(f"{name}: {count}")

                        summary = "\n".join(summary_parts) if summary_parts else "No recognized sections found"

                    except Exception:
                        summary = "Could not parse IDE content"

                    info_text = f"""IDE Item Definition File:

Name: {entry.name}
Size: {len(entry.data):,} bytes
Type: GTA Item Definition File

Content Summary:
{summary}

IDE files define:
- Object properties (ID, model, texture, flags)
- Vehicle specifications
- Weapon definitions
- Animation hierarchies
- Pedestrian data"""

                    QMessageBox.information(main_window, f"IDE Info - {entry.name}", info_text)
                else:
                    QMessageBox.warning(main_window, "Not an IDE File", "Selected file is not an IDE definition file.")
    except Exception as e:
        QMessageBox.critical(main_window, "Error", f"Failed to view IDE definitions: {str(e)}")


def view_txd_textures(main_window, row: int): #vers 4
    """View TXD textures"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                if entry.name.lower().endswith('.txd'):
                    # Basic TXD info for now
                    info_text = f"""TXD Texture Dictionary:

Name: {entry.name}
Size: {len(entry.data):,} bytes
Type: RenderWare TXD Texture Dictionary

This feature will be expanded to show:
- Texture thumbnails
- Texture dimensions
- Compression formats
- Mipmap information"""

                    QMessageBox.information(main_window, f"TXD Info - {entry.name}", info_text)
                else:
                    QMessageBox.warning(main_window, "Not a TXD File", "Selected file is not a TXD texture dictionary.")
    except Exception as e:
        QMessageBox.critical(main_window, "Error", f"Failed to view TXD textures: {str(e)}")


# Export functions
__all__ = [
    'add_extraction_to_menu',
    'get_selected_entries_for_extraction',
    'integrate_extraction_system',
    'open_ide_in_text_editor',
    'patch_img_loading_for_extraction',
    'setup_complete_extraction_integration',
    'setup_extraction_context_menu'
]
