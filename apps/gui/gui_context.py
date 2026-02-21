#this belongs in gui/gui_context.py - Version: 9
# X-Seti - August13 2025 - IMG Factory 1.5 - Context Menu Functions - WORKING COL IMPLEMENTATION

"""
Context Menu Functions - Handles right-click context menus
UPDATED: Replace stubs with working COL functionality using existing components
"""

import sys
import os
import tempfile
import mimetypes
import shutil
from typing import Optional, List, Dict, Any
from pathlib import Path
from PyQt6.QtWidgets import QFileDialog, QMenu, QFileDialog, QMessageBox, QInputDialog
from PyQt6.QtCore import pyqtSignal, QMimeData, Qt, QThread, QTimer, QSettings
from PyQt6.QtGui import QAction, QContextMenuEvent, QDragEnterEvent, QDropEvent, QFont, QIcon, QPixmap, QShortcut
from apps.methods.img_core_classes import format_file_size

##Methods list -
# add_col_context_menu_to_entries_table
# add_img_context_menu_to_entries_table
# analyze_col_file_dialog
# analyze_col_from_img_entry
# edit_col_collision
# edit_col_from_img_entry
# edit_dff_model
# edit_txd_textures
# enhanced_context_menu_event
# get_selected_entry_info
# open_col_batch_proc_dialog
# open_col_editor_dialog
# open_col_file_dialog
# replace_selected_entry
# show_entry_properties
# view_col_collision
# view_dff_model
# view_txd_textures


def menu_system_and_functionality(main_window):
    """
    Comprehensive fix for menu system and functionality
    """
    try:
        # Fix the rename functionality to work from both right-click and double-click
        fix_rename_functionality(main_window)

        # Implement context menu for active tab
        implement_tab_context_menu(main_window)

        # Add requested file operations to main window
        add_file_operations_to_main_window(main_window)

        # Set up proper double-click rename functionality
        setup_double_click_rename(main_window)

        main_window.log_message("Comprehensive menu system and functionality fix applied")
        return True

    except Exception as e:
        main_window.log_message(f"Error applying comprehensive fix: {str(e)}")
        return False


def add_file_operations_to_main_window(main_window):
    """
    Add the requested file operations as methods to the main window
    """
    try:
        # Add move_selected_file method
        main_window.move_selected_file = lambda: move_selected_file(main_window)

        # Add analyze_selected_file method
        main_window.analyze_selected_file = lambda: analyze_selected_file(main_window)

        # Add show_hex_editor_selected method
        main_window.show_hex_editor_selected = lambda: show_hex_editor_selected(main_window)

        # Add show_dff_texture_list method (as a general method that handles current selection)
        main_window.show_dff_texture_list = lambda: show_dff_texture_list_from_selection(main_window)

        # Add show_dff_model_viewer method (as a general method that handles current selection)
        main_window.show_dff_model_viewer = lambda: show_dff_model_viewer_from_selection(main_window)

        # Add set_game_path method
        main_window.set_game_path = lambda: set_game_path(main_window)

        main_window.log_message("File operations added to main window")

    except Exception as e:
        main_window.log_message(f"Error adding file operations: {str(e)}")


def set_game_path(main_window):
    """
    Set game path with support for custom paths including Linux paths
    """
    try:
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        import os

        # Get current path if it exists
        current_path = getattr(main_window, 'game_root', None)
        if not current_path or current_path == "C:/":
            # Default to home directory instead of C:/
            current_path = os.path.expanduser("~")

        # Open directory dialog without restricting to Windows paths
        folder = QFileDialog.getExistingDirectory(
            main_window,
            "Select Game Root Directory (Supports Windows and Linux paths)",
            current_path,
            QFileDialog.Option.ShowDirsOnly
        )

        if folder:
            # Validate that it's a game directory by checking for common game files
            game_files = [
                "gta3.exe", "gta_vc.exe", "gta_sa.exe", "gtasol.exe", "solcore.exe",
                "gta3.dat", "gta_vc.dat", "gta_sa.dat", "gta_sol.dat", "SOL/gta_sol.dat",
                "default.ide", "Data/default.dat", "models/", "textures/", "data/"
            ]

            # Check if the folder contains game-related files/directories
            is_game_dir = False
            for item in os.listdir(folder):
                item_lower = item.lower()
                if any(game_file.split('/')[0] in item_lower for game_file in game_files if '/' not in game_file) or \
                   any(game_file in item_lower for game_file in game_files if '/' not in game_file):
                    is_game_dir = True
                    break

            # Also check subdirectories
            if not is_game_dir:
                for root, dirs, files in os.walk(folder):
                    for d in dirs:
                        if d.lower() in ['models', 'textures', 'data', 'sfx', 'audio']:
                            is_game_dir = True
                            break
                    if is_game_dir:
                        break

            main_window.game_root = folder
            main_window.log_message(f"Game path set: {folder}")

            # Update directory tree if it exists
            if hasattr(main_window, 'directory_tree'):
                main_window.directory_tree.game_root = folder
                main_window.directory_tree.current_root = folder
                if hasattr(main_window.directory_tree, 'path_label'):
                    main_window.directory_tree.path_label.setText(folder)
                # Auto-populate the tree
                if hasattr(main_window.directory_tree, 'populate_tree'):
                    main_window.directory_tree.populate_tree(folder)
                    main_window.log_message("Directory tree auto-populated")

            # Save settings
            if hasattr(main_window, 'save_settings'):
                main_window.save_settings()
            else:
                # Create a simple save settings if not available
                try:
                    from PyQt6.QtCore import QSettings
                    settings = QSettings("IMG_Factory", "IMG_Factory_Settings")
                    settings.setValue("game_root", folder)
                except:
                    pass

            # Show success message
            QMessageBox.information(
                main_window,
                "Game Path Set",
                f"Game path configured:\n{folder}\n\nDirectory tree will now show game files.\nSwitch to the 'Merge View' tab to browse."
            )
        else:
            main_window.log_message("Game path selection cancelled")

    except Exception as e:
        main_window.log_message(f"Error setting game path: {str(e)}")
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.critical(
            main_window,
            "Error Setting Game Path",
            f"An error occurred while setting the game path:\n\n{str(e)}"
        )


def show_dff_texture_list_from_selection(main_window):
    """
    Show DFF texture list for currently selected entry
    """
    try:
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            selected_items = table.selectedItems()
            if selected_items:
                row = selected_items[0].row()
                entry_info = get_entry_info(main_window, row)
                if entry_info and entry_info['is_dff']:
                    show_dff_texture_list(main_window, row, entry_info)
                else:
                    # Check if it's a DFF file in the IMG that we need to extract and parse
                    if entry_info and entry_info['name'].lower().endswith('.dff'):
                        show_dff_texture_list_from_img_dff(main_window, row, entry_info)
                    else:
                        from PyQt6.QtWidgets import QMessageBox
                        QMessageBox.information(main_window, "DFF Texture List",
                                              "Please select a DFF file to view texture list")
    except Exception as e:
        main_window.log_message(f"Error showing DFF texture list from selection: {str(e)}")


def show_dff_texture_list_from_img_dff(main_window, row, entry_info):
    """
    Extract and show DFF texture list from DFF files in IMG
    """
    try:
        from PyQt6.QtWidgets import QMessageBox, QDialog, QVBoxLayout, QTextEdit, QPushButton, QHBoxLayout
        from PyQt6.QtCore import QThread, pyqtSignal
        import tempfile
        import os

        # Get the DFF data from the IMG entry
        if hasattr(main_window, 'current_img') and main_window.current_img:
            entry = main_window.current_img.entries[row]
            dff_data = entry.get_data() if hasattr(entry, 'get_data') else None

            if dff_data:
                # Create a temporary file to extract the DFF
                with tempfile.NamedTemporaryFile(delete=False, suffix='.dff', mode='wb') as temp_file:
                    temp_file.write(dff_data)
                    temp_dff_path = temp_file.name

                try:
                    # Parse the DFF file for texture information
                    textures = parse_dff_textures_from_data(temp_dff_path)

                    # Create dialog to show texture list
                    dialog = QDialog(main_window)
                    dialog.setWindowTitle(f"Textures in {entry.name}")
                    dialog.resize(500, 400)

                    layout = QVBoxLayout(dialog)

                    # Create text area for texture list
                    text_area = QTextEdit()
                    text_area.setReadOnly(True)

                    if textures:
                        texture_list = "\n".join([f"  • {tex}" for tex in textures])
                        text_content = f"Textures found in {entry.name}:\n\n{texture_list}"
                    else:
                        text_content = f"No textures found in {entry.name}"

                    text_area.setPlainText(text_content)
                    layout.addWidget(text_area)

                    # Close button
                    close_btn = QPushButton("Close")
                    close_btn.clicked.connect(dialog.close)
                    layout.addWidget(close_btn)

                    dialog.exec()

                finally:
                    # Clean up temporary file
                    if os.path.exists(temp_dff_path):
                        os.remove(temp_dff_path)
            else:
                QMessageBox.warning(main_window, "DFF Texture List",
                                  f"Could not extract data from {entry.name}")
    except Exception as e:
        main_window.log_message(f"Error showing DFF texture list from IMG: {str(e)}")


def parse_dff_textures_from_data(dff_path):
    """
    Parse a DFF file to extract texture names
    """
    try:
        textures = []

        # This is a simplified implementation - in a real application,
        # you'd need a proper DFF parser
        with open(dff_path, 'rb') as f:
            data = f.read()

        # Look for texture-related patterns in the DFF data
        # DFF files often contain texture names in certain sections
        import re

        # Search for potential texture names in the binary data
        # DFF files often have ASCII texture names in certain sections
        text_data = data.decode('ascii', errors='ignore')

        # Look for potential texture names (alphanumeric with underscores, hyphens, dots)
        # Common patterns in DFF files for texture names
        potential_textures = re.findall(r'[A-Za-z0-9_\-]{3,20}\.(?:txd|png|jpg|bmp|dxt|tga)', text_data, re.IGNORECASE)

        # Also look for names without extensions that might be texture names
        # Look for names that could be texture names (avoiding too many false positives)
        potential_names = re.findall(r'[A-Za-z][A-Za-z0-9_\-]{3,19}(?=\.|\s|\0|$)', text_data)

        # Combine and deduplicate
        all_matches = list(set(potential_textures + potential_names))

        # Filter for likely texture names
        for name in all_matches:
            # Check if it contains common texture-related terms
            if any(tex in name.lower() for tex in ['tex', 'texture', 'material', 'diffuse', 'specular', 'normal', 'bump']):
                textures.append(name)
            # Check if it's a common texture naming pattern (not too generic)
            elif len(name) > 3 and len(name) < 20 and not any(c.isdigit() for c in name[:2]):  # Avoid names starting with numbers
                # Additional check: avoid common non-texture words
                if not any(common_word in name.lower() for common_word in ['object', 'model', 'geometry', 'data', 'file', 'name']):
                    textures.append(name)

        # Return unique textures, removing duplicates
        unique_textures = list(set(textures))

        # Sort for consistent output
        unique_textures.sort()

        return unique_textures

    except Exception as e:
        print(f"Error parsing DFF textures: {str(e)}")
        return []


def show_dff_model_viewer_from_selection(main_window):
    """
    Show DFF model viewer for currently selected entry
    """
    try:
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            selected_items = table.selectedItems()
            if selected_items:
                row = selected_items[0].row()
                entry_info = get_entry_info(main_window, row)
                if entry_info and entry_info['is_dff']:
                    show_dff_model_viewer(main_window, row, entry_info)
                else:
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.information(main_window, "DFF Model Viewer",
                                          "Please select a DFF file to view in model viewer")
    except Exception as e:
        main_window.log_message(f"Error showing DFF model viewer from selection: {str(e)}")


def fix_rename_functionality(main_window):
    """
    Fix rename functionality to work from both right-click and double-click
    """
    try:
        # Ensure rename_selected function is properly connected
        if not hasattr(main_window, 'rename_selected'):
            from apps.core.imgcol_rename import integrate_imgcol_rename_functions
            integrate_imgcol_rename_functions(main_window)

        # Connect double-click event to table for rename
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            # Connect double-click to rename function
            table.cellDoubleClicked.connect(lambda row, col: handle_double_click_rename(main_window, row, col))

        main_window.log_message("Rename functionality fixed")

    except Exception as e:
        main_window.log_message(f"Error fixing rename functionality: {str(e)}")


def edit_col_from_img_entry(main_window, row): #vers 2
    """Edit COL file from IMG entry - WORKING VERSION"""
    try:
        entry_info = get_selected_entry_info(main_window, row)
        if not entry_info or not entry_info['is_col']:
            main_window.log_message("Selected entry is not a COL file")
            return False
        
        entry = entry_info['entry']
        main_window.log_message(f"Opening COL editor for: {entry.name}")
        
        # Use methods from col_operations
        from apps.methods.col_operations import extract_col_from_img_entry, create_temporary_col_file, cleanup_temporary_file
        
        # Extract COL data
        extraction_result = extract_col_from_img_entry(main_window, row)
        if not extraction_result:
            main_window.log_message("Failed to extract COL data")
            return False
        
        col_data, entry_name = extraction_result
        
        # Create temporary COL file
        temp_path = create_temporary_col_file(col_data, entry_name)
        if not temp_path:
            main_window.log_message("Failed to create temporary COL file")
            return False
        
        try:
            # Import and open COL editor
            from apps.components.Col_Editor.col_workshop import COLWorkshop as COLEditorDialog
            editor = COLEditorDialog(main_window)
            
            # Load the temporary COL file
            if editor.load_col_file(temp_path):
                editor.setWindowTitle(f"COL Editor - {entry.name}")
                editor.show()  # Use show() instead of exec() for non-modal
                main_window.log_message(f"COL editor opened for: {entry.name}")
                return True
            else:
                main_window.log_message("Failed to load COL file in editor")
                return False
                
        finally:
            # Clean up temporary file
            cleanup_temporary_file(temp_path)
        
    except ImportError:
        QMessageBox.information(main_window, "COL Editor", 
            "COL editor component not available. Please check components.Col_Editor.col_workshop.py")
        return False
    except Exception as e:
        main_window.log_message(f"Error editing COL file: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Failed to edit COL file: {str(e)}")
        return False


def show_main_context_menu(main_window, position):
    """
    Show context menu for the main window/active tab
    """
    try:
        menu = QMenu(main_window)

        # Add file operations that were requested
        add_requested_file_operations(main_window, menu)

        # Add common operations
        add_common_operations(main_window, menu)

        # Show the menu
        menu.exec(main_window.mapToGlobal(position))

    except Exception as e:
        main_window.log_message(f"Error showing main context menu: {str(e)}")


def show_table_context_menu(main_window, position):
    """
    Show context menu for the table
    """
    try:
        table = main_window.gui_layout.table
        item = table.itemAt(position)

        if not item:
            # Show generic menu if no item clicked
            show_main_context_menu(main_window, position)
            return

        row = item.row()
        menu = QMenu(table)

        # Add file operations that were requested
        add_requested_file_operations(main_window, menu, row)

        # Add common operations
        add_common_operations(main_window, menu, row)

        # Show the menu
        menu.exec(table.mapToGlobal(position))

    except Exception as e:
        main_window.log_message(f"Error showing table context menu: {str(e)}")


def implement_tab_context_menu(main_window):
    """
    Implement context menu for active tab with file operations
    This integrates with the existing context menu system to avoid conflicts
    """
    try:
        # Add context menu to the main window
        main_window.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        main_window.customContextMenuRequested.connect(lambda pos: show_main_context_menu(main_window, pos))

        # For the table, we need to integrate with the existing context menu system
        # rather than replacing it to avoid conflicts with the existing setup
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table

            # Instead of replacing the context menu policy, we'll enhance the existing one
            # by making sure our features are available through the existing system

            # The existing setup_table_context_menu should already handle the basic context menu
            # We'll enhance it by making sure our operations are available

            # Store a reference to our enhanced functionality
            table._enhanced_context_menu = True

        main_window.log_message("Tab context menu enhanced with additional operations")

    except Exception as e:
        main_window.log_message(f"Error implementing tab context menu: {str(e)}")


def view_col_collision(main_window, row): #vers 2
    """View COL collision - WORKING VERSION"""
    try:
        entry_info = get_selected_entry_info(main_window, row)
        if not entry_info or not entry_info['is_col']:
            main_window.log_message("Selected entry is not a COL file")
            return False
        
        entry = entry_info['entry']
        main_window.log_message(f"Viewing COL collision for: {entry.name}")
        
        # Use methods from col_operations
        from apps.methods.col_operations import extract_col_from_img_entry, get_col_basic_info
        
        # Extract COL data
        extraction_result = extract_col_from_img_entry(main_window, row)
        if not extraction_result:
            main_window.log_message("Failed to extract COL data")
            return False
        
        col_data, entry_name = extraction_result
        
        # Get basic info
        basic_info = get_col_basic_info(col_data)
        
        if 'error' in basic_info:
            main_window.log_message(f"COL analysis error: {basic_info['error']}")
            return False
        
        # Build info display
        info_text = f"COL File: {entry.name}\n"
        info_text += f"Size: {format_file_size(len(col_data))}\n"
        info_text += f"Version: {basic_info.get('version', 'Unknown')}\n"
        info_text += f"Models: {basic_info.get('model_count', 0)}\n"
        info_text += f"Signature: {basic_info.get('signature', b'Unknown')}\n"
        
        # Show info dialog
        from gui.col_dialogs import show_col_info_dialog
        show_col_info_dialog(main_window, info_text, f"COL Collision Info - {entry.name}")
        
        main_window.log_message(f"COL collision viewed for: {entry.name}")
        return True
        
    except ImportError:
        main_window.log_message("COL operations not available")
        return False
    except Exception as e:
        main_window.log_message(f"Error viewing COL collision: {str(e)}")
        return False


def analyze_col_from_img_entry(main_window, row): #vers 2
    """Analyze COL file from IMG entry - WORKING VERSION"""
    try:
        entry_info = get_selected_entry_info(main_window, row)
        if not entry_info or not entry_info['is_col']:
            main_window.log_message("Selected entry is not a COL file")
            return False
        
        entry = entry_info['entry']
        main_window.log_message(f"Analyzing COL file: {entry.name}")
        
        # Use methods from col_operations
        from apps.methods.col_operations import extract_col_from_img_entry, validate_col_data, create_temporary_col_file, cleanup_temporary_file, get_col_detailed_analysis
        
        # Extract COL data
        extraction_result = extract_col_from_img_entry(main_window, row)
        if not extraction_result:
            main_window.log_message("Failed to extract COL data")
            return False
        
        col_data, entry_name = extraction_result
        
        # Validate COL data
        validation_result = validate_col_data(col_data)
        
        # Get detailed analysis if possible
        temp_path = create_temporary_col_file(col_data, entry_name)
        analysis_data = {}
        
        if temp_path:
            try:
                detailed_analysis = get_col_detailed_analysis(temp_path)
                if 'error' not in detailed_analysis:
                    analysis_data.update(detailed_analysis)
            finally:
                cleanup_temporary_file(temp_path)
        
        # Combine validation and analysis data
        final_analysis = {
            'size': len(col_data),
            **analysis_data,
            **validation_result
        }
        
        # Show analysis dialog
        from gui.col_dialogs import show_col_analysis_dialog
        show_col_analysis_dialog(main_window, final_analysis, entry.name)
        
        main_window.log_message(f"COL analysis completed for: {entry.name}")
        return True
        
    except ImportError:
        main_window.log_message("COL analysis components not available")
        return False
    except Exception as e:
        main_window.log_message(f"Error analyzing COL file: {str(e)}")
        return False


def edit_col_collision(main_window, row): #vers 2
    """Edit COL collision - WORKING VERSION (alias for edit_col_from_img_entry)"""
    return edit_col_from_img_entry(main_window, row)


def open_col_editor_dialog(main_window): #vers 3
    """Open COL editor - WORKING VERSION"""
    try:
        # Try to import and open COL editor
        from apps.components.Col_Editor.col_workshop import COLWorkshop as COLEditorDialog
        
        main_window.log_message("Opening COL editor...")
        editor = COLEditorDialog(main_window)
        editor.setWindowTitle("COL Editor - IMG Factory 1.5")
        
        # Show the editor
        editor.show()  # Use show() for non-modal
        main_window.log_message("COL editor opened successfully")
        return True
        
    except ImportError:
        QMessageBox.information(main_window, "COL Editor", 
            "COL editor component not available.\n\nPlease ensure components.Col_Editor.col_workshop.py is properly installed.")
        main_window.log_message("COL editor component not found")
        return False
    except Exception as e:
        main_window.log_message(f"Error opening COL editor: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Failed to open COL editor:\n{str(e)}")
        return False


def open_col_batch_proc_dialog(main_window): #vers 3
    """Open COL batch processor - WORKING VERSION"""
    try:
        # Try to import and open batch processor
        from apps.methods.col_utilities import COLBatchProcessor
        
        main_window.log_message("Opening COL batch processor...")
        processor = COLBatchProcessor(main_window)
        processor.setWindowTitle("COL Batch Processor - IMG Factory 1.5")
        
        # Show the processor
        result = processor.exec()
        if result == 1:
            main_window.log_message("COL batch processor completed")
        else:
            main_window.log_message("COL batch processor closed")
        
        return result == 1
        
    except ImportError:
        QMessageBox.information(main_window, "Batch Processor", 
            "COL batch processor component not available.\n\nPlease ensure methods.col_utilities.py is properly installed.")
        main_window.log_message("COL batch processor component not found")
        return False
    except Exception as e:
        main_window.log_message(f"Error opening batch processor: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Failed to open batch processor:\n{str(e)}")
        return False


def open_col_file_dialog(main_window): #vers 3
    """Open COL file dialog - WORKING VERSION"""
    try:
        file_path, _ = QFileDialog.getOpenFileName(
            main_window,
            "Open COL File",
            "",
            "COL Files (*.col);;All Files (*)"
        )

        if file_path:
            main_window.log_message(f"Opening COL file: {os.path.basename(file_path)}")
            
            # Check if main window has a COL loading method
            if hasattr(main_window, 'load_col_file_safely'):
                return main_window.load_col_file_safely(file_path)
            else:
                # Try to load using COL parsing functions
                try:
                    from apps.methods.populate_col_table import load_col_file_safely
                    return load_col_file_safely(main_window, file_path)
                except ImportError:
                    # Fallback: open in COL editor
                    try:
                        from apps.components.Col_Editor.col_editor import open_col_editor
                        editor = open_col_editor(main_window, file_path)
                        return editor is not None
                    except ImportError:
                        QMessageBox.warning(main_window, "COL Support", 
                            "COL file loading not available.\n\nPlease ensure COL integration is properly installed.")
                        return False
        
        return False

    except Exception as e:
        main_window.log_message(f"Error opening COL file: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Failed to open COL file:\n{str(e)}")
        return False


def analyze_col_file_dialog(main_window): #vers 3
    """Analyze COL file dialog - WORKING VERSION"""
    try:
        file_path, _ = QFileDialog.getOpenFileName(
            main_window,
            "Analyze COL File",
            "",
            "COL Files (*.col);;All Files (*)"
        )

        if file_path:
            main_window.log_message(f"Analyzing COL file: {os.path.basename(file_path)}")
            
            try:
                from apps.methods.col_operations import get_col_detailed_analysis, validate_col_data
                
                # Read file data
                with open(file_path, 'rb') as f:
                    col_data = f.read()
                
                # Get detailed analysis
                analysis_data = get_col_detailed_analysis(file_path)
                if 'error' in analysis_data:
                    QMessageBox.warning(main_window, "Analysis Error", f"Analysis failed: {analysis_data['error']}")
                    return False
                
                # Get validation data
                validation_data = validate_col_data(col_data)
                
                # Combine data
                final_analysis = {
                    'size': len(col_data),
                    **analysis_data,
                    **validation_data
                }
                
                # Show analysis dialog
                from gui.col_dialogs import show_col_analysis_dialog
                show_col_analysis_dialog(main_window, final_analysis, os.path.basename(file_path))

                main_window.log_message(f"COL analysis completed for: {os.path.basename(file_path)}")
                return True
                
            except ImportError:
                QMessageBox.warning(main_window, "COL Analysis", 
                    "COL analysis components not available.\n\nPlease ensure COL integration is properly installed.")
                return False
        
        return False

    except Exception as e:
        main_window.log_message(f"Error analyzing COL file: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Failed to analyze COL file:\n{str(e)}")
        return False

# Other file type functions (keeping existing stubs for now)
def edit_dff_model(main_window, row): #vers 1
    """Edit DFF model"""
    main_window.log_message(f"Edit DFF model from row {row} - not yet implemented")


def edit_txd_textures(main_window, row): #vers 1
    """Edit TXD textures"""
    main_window.log_message(f"Edit TXD textures from row {row} - not yet implemented")


def view_dff_model(main_window, row): #vers 1
    """View DFF model"""
    main_window.log_message(f"View DFF model from row {row} - not yet implemented")


def view_txd_textures(main_window, row): #vers 1
    """View TXD textures"""
    main_window.log_message(f"View TXD textures from row {row} - not yet implemented")


def replace_selected_entry(main_window, row): #vers 1
    """Replace selected entry"""
    main_window.log_message(f"Replace entry from row {row} - not yet implemented")


def show_entry_properties(main_window, row): #vers 1
    """Show entry properties"""
    entry_info = get_selected_entry_info(main_window, row)
    if entry_info:
        props_text = f"Entry Properties:\n\n"
        props_text += f"Name: {entry_info['name']}\n"
        props_text += f"Size: {format_file_size(entry_info['size'])}\n"
        props_text += f"Offset: 0x{entry_info['offset']:08X}\n"
        props_text += f"Type: {'COL' if entry_info['is_col'] else 'DFF' if entry_info['is_dff'] else 'TXD' if entry_info['is_txd'] else 'Other'}\n"
        
        QMessageBox.information(main_window, "Entry Properties", props_text)
        main_window.log_message(f"Properties shown for: {entry_info['name']}")
    else:
        main_window.log_message(f"Unable to get properties for row {row}")


def validate_new_name(main_window, new_name):
    """
    Validate new name for file entry
    """
    try:
        # Check for empty name
        if not new_name or not new_name.strip():
            return False

        # Check for invalid characters
        invalid_chars = '<>:"/\\|?*'
        if any(char in new_name for char in invalid_chars):
            return False

        # Check length (typically IMG entries have 24 char limit)
        if len(new_name) > 24:
            return False

        return True
    except Exception:
        return False


def check_duplicate_name(main_window, new_name, current_entry):
    """
    Check if new name would create duplicate
    """
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            for entry in main_window.current_img.entries:
                if entry != current_entry and getattr(entry, 'name', '') == new_name:
                    return True
        return False
    except Exception:
        return True  # Return True on error to be safe


def handle_double_click_rename(main_window, row, col):
    """
    Handle double-click rename functionality
    """
    try:
        # Only allow renaming when clicking on the name column (usually column 0)
        if col == 0:  # Assuming name column is first column
            if hasattr(main_window, 'current_img') and main_window.current_img:
                if 0 <= row < len(main_window.current_img.entries):
                    # Get the current entry
                    entry = main_window.current_img.entries[row]
                    current_name = entry.name

                    # Show input dialog for new name
                    new_name, ok = QInputDialog.getText(
                        main_window,
                        "Rename File",
                        f"Enter new name for '{current_name}':",
                        text=current_name
                    )

                    if ok and new_name and new_name != current_name:
                        # Validate the new name
                        if validate_new_name(main_window, new_name):
                            # Check for duplicates
                            if not check_duplicate_name(main_window, new_name, entry):
                                # Perform the rename
                                entry.name = new_name

                                # Update the table display
                                if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
                                    table = main_window.gui_layout.table
                                    table.item(row, 0).setText(new_name)

                                # Mark as modified
                                if hasattr(main_window.current_img, 'modified'):
                                    main_window.current_img.modified = True

                                main_window.log_message(f"Renamed '{current_name}' to '{new_name}'")
                                QMessageBox.information(main_window, "Rename Successful",
                                                      f"Successfully renamed to '{new_name}'")
                            else:
                                QMessageBox.warning(main_window, "Duplicate Name",
                                                  f"An entry named '{new_name}' already exists")
                        else:
                            QMessageBox.warning(main_window, "Invalid Name",
                                              "The name provided is invalid")
        else:
            # For other columns, we might want to handle different actions
            main_window.log_message(f"Double-clicked on row {row}, column {col}")

    except Exception as e:
        main_window.log_message(f"Error handling double-click rename: {str(e)}")


# Context menu setup functions (enhanced versions)
def context_menu_event(main_window, event): #vers 2
    """Context menu with working COL functions"""
    try:
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            return

        table = main_window.gui_layout.table
        # Apply hover stylesheet\
        existing_style = table.styleSheet()
        hover_style = """
                QTableWidget::item:hover {
                    background-color: rgba(100, 150, 255, 0.25);
                }
                QTableWidget::item:selected:hover {
                    background-color: rgba(90, 150, 250, 0.5);
                }
                """

        # Get the item at the position where the right-click occurred
        item = table.itemAt(event.pos())
        if not item:
            return

        row = item.row()
        entry_info = get_selected_entry_info(main_window, row)
        if not entry_info:
            return

        # Create context menu
        menu = QMenu(table)
        
        # Add file-type specific actions
        if entry_info['is_col']:
            # COL file actions
            view_action = QAction("View Collision", table)
            view_action.triggered.connect(lambda: view_col_collision(main_window, row))
            menu.addAction(view_action)
            
            edit_action = QAction("Edit COL File", table)
            edit_action.triggered.connect(lambda: edit_col_from_img_entry(main_window, row))
            menu.addAction(edit_action)
            
            analyze_action = QAction("Analyze COL", table)
            analyze_action.triggered.connect(lambda: analyze_col_from_img_entry(main_window, row))
            menu.addAction(analyze_action)
            
            analyze_action = QAction("Analyze DFF", table)
            analyze_action.triggered.connect(lambda: analyze_dff_from_img_entry(main_window, row))
            menu.addAction(analyze_action)

            analyze_action = QAction("Analyze TXD", table)
            analyze_action.triggered.connect(lambda: analyze_txd_from_img_entry(main_window, row))
            menu.addAction(analyze_action)

            analyze_action = QAction("Analyze IMG", table)
            analyze_action.triggered.connect(lambda: analyze_img_from_img_entry(main_window, row))
            menu.addAction(analyze_action)

            analyze_action = QAction("Analyze File", table)
            analyze_action.triggered.connect(lambda: analyze_file_from_img_entry(main_window, row))
            menu.addAction(analyze_action)

            menu.addSeparator()
            
        elif entry_info['is_dff']:
            # DFF model actions
            view_action = QAction("View Model", table)
            view_action.triggered.connect(lambda: view_dff_model(main_window, row))
            menu.addAction(view_action)
            
            edit_action = QAction("Edit Model", table)
            edit_action.triggered.connect(lambda: edit_dff_model(main_window, row))
            menu.addAction(edit_action)
            
            menu.addSeparator()
            
        elif entry_info['is_txd']:
            # TXD texture actions
            view_action = QAction("View Textures", table)
            view_action.triggered.connect(lambda: view_txd_textures(main_window, row))
            menu.addAction(view_action)
            
            edit_action = QAction("Edit Textures", table)
            edit_action.triggered.connect(lambda: edit_txd_textures(main_window, row))
            menu.addAction(edit_action)
            
            menu.addSeparator()
        
        # Common actions
        props_action = QAction("Properties", table)
        props_action.triggered.connect(lambda: show_entry_properties(main_window, row))
        menu.addAction(props_action)
        
        replace_action = QAction("Replace Entry", table)
        replace_action.triggered.connect(lambda: replace_selected_entry(main_window, row))
        menu.addAction(replace_action)
        
        # Show menu at the global position of the event
        menu.exec(event.globalPos())

    except Exception as e:
        main_window.log_message(f"Error showing context menu: {str(e)}")

