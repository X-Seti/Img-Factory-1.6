#this belongs in methods/table_handlers.py - Version: 1
# X-Seti - December11 2025 - IMG Factory 1.5 - Table Handler Functions
"""
Table Handler Functions - Unified table interaction handlers
Extracted from imgfactory.py to eliminate duplicates
"""

from apps.methods.img_core_classes import format_file_size

##Methods list -
# setup_missing_utility_functions
# unified_double_click_handler
# unified_selection_handler

def setup_missing_utility_functions(main_window): #vers 1
    """Add missing utility functions that selection callbacks need"""

    # Simple file type detection functions
    main_window.has_col = lambda name: name.lower().endswith('.col') if name else False
    main_window.has_dff = lambda name: name.lower().endswith('.dff') if name else False
    main_window.has_txd = lambda name: name.lower().endswith('.txd') if name else False
    main_window.get_entry_type = lambda name: name.split('.')[-1].upper() if name and '.' in name else "Unknown"

    # Add missing functions for menu system
    main_window.save_img_as = main_window._save_img_as
    main_window.save_img_entry = main_window._save_img_entry
    main_window.find_entries = main_window._find_entries
    main_window.find_next_entries = main_window._find_next_entries
    main_window.duplicate_selected = main_window._duplicate_selected
    main_window.rename_entry = main_window._rename_entry
    main_window.rename_selected = main_window._rename_selected
    main_window.remove_selected = main_window._remove_selected_entries
    main_window.select_inverse_entries = main_window._select_inverse_entries

    # Extract textures function
    from apps.core.extract import extract_textures_function
    main_window.extract_textures = lambda: extract_textures_function(main_window)

    # Extract DFF texture lists functionality
    from apps.core.extract import extract_dff_texture_lists
    main_window.extract_dff_texture_lists = lambda: extract_dff_texture_lists(main_window)

    main_window.undo = main_window._undo_action
    main_window.redo = main_window._redo_action

    main_window.log_message("Missing utility functions added")


def unified_double_click_handler(main_window, row, filename, item): #vers 1
    """Handle double-click through unified system"""
    # Get the actual filename from the first column (index 0)
    if row < main_window.gui_layout.table.rowCount():
        name_item = main_window.gui_layout.table.item(row, 0)
        if name_item:
            actual_filename = name_item.text()
            main_window.log_message(f"Double-clicked: {actual_filename}")

            # Show file info if IMG is loaded
            if main_window.current_img and row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                main_window.log_message(f"File info: {entry.name} ({format_file_size(entry.size)})")
        else:
            main_window.log_message(f"Double-clicked row {row} (no filename found)")
    else:
        main_window.log_message(f"Double-clicked: {filename}")


def unified_selection_handler(main_window, selected_rows, selection_count): #vers 1
    """Handle selection changes through unified system"""
    # Update button states based on selection
    has_selection = selection_count > 0
    main_window._update_button_states(has_selection)

    # Log selection (unified approach - no spam)
    if selection_count == 0:
        # Don't log "Ready" for empty selection to reduce noise
        pass
    elif selection_count == 1:
        # Get filename of selected item
        if selected_rows and len(selected_rows) > 0:
            row = selected_rows[0]
            if row < main_window.gui_layout.table.rowCount():
                name_item = main_window.gui_layout.table.item(row, 0)
                if name_item:
                    main_window.log_message(f"Selected: {name_item.text()}")
