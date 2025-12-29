#this belongs in methods/img_operations_routing.py - Version: 3
# X-Seti - August10 2025 - IMG Factory 1.5 - Route All IMG Operations Through Shared System

"""
IMG OPERATIONS ROUTING SYSTEM
Routes all existing IMG operation functions through the shared placeholder system.
Prevents broken functions from corrupting files.
"""

from apps.methods.img_shared_operations import *

##Methods list -
# route_remove_entry
# route_remove_via_list
# route_import_file
# route_import_via_list
# route_split_img
# route_rebuild_img
# route_save_img
# install_operation_routing
# disable_broken_functions
# create_safe_function_wrappers

def route_remove_entry(main_window): #vers 1
    """Route remove entry function through placeholder"""
    try:
        if not main_window.current_img:
            main_window.log_message("No IMG file loaded")
            return False
            
        # Get selected entries from table
        selected_entries = get_selected_entry_names(main_window)
        if not selected_entries:
            main_window.log_message("No entries selected for removal")
            return False
            
        if len(selected_entries) == 1:
            return main_window.shared_remove_entry(main_window.current_img, selected_entries[0])
        else:
            return main_window.shared_remove_entries_via(main_window.current_img, selected_entries)
            
    except Exception as e:
        main_window.log_message(f"Error routing remove: {str(e)}")
        return False


def route_remove_via_list(main_window, entry_list): #vers 1
    """Route remove via list function through placeholder"""
    try:
        if not main_window.current_img:
            main_window.log_message("No IMG file loaded")
            return False
            
        return main_window.shared_remove_entries_via(main_window.current_img, entry_list)
        
    except Exception as e:
        main_window.log_message(f"Error routing remove via list: {str(e)}")
        return False


def route_import_file(main_window, file_path, target_name=None): #vers 1
    """Route import file function through placeholder"""
    try:
        if not main_window.current_img:
            main_window.log_message("No IMG file loaded")
            return False
            
        return main_window.shared_import_file(main_window.current_img, file_path, target_name)
        
    except Exception as e:
        main_window.log_message(f"Error routing import: {str(e)}")
        return False


def route_import_via_list(main_window, file_paths): #vers 1
    """Route import via list function through placeholder"""
    try:
        if not main_window.current_img:
            main_window.log_message("No IMG file loaded")
            return False
            
        return main_window.shared_import_files_via(main_window.current_img, file_paths)
        
    except Exception as e:
        main_window.log_message(f"Error routing import via list: {str(e)}")
        return False


def route_split_img(main_window, split_criteria): #vers 1
    """Route split IMG function through placeholder"""
    try:
        if not main_window.current_img:
            main_window.log_message("No IMG file loaded")
            return False
            
        return main_window.shared_split_img(main_window.current_img, split_criteria)
        
    except Exception as e:
        main_window.log_message(f"Error routing split: {str(e)}")
        return False


def route_rebuild_img(main_window, options=None): #vers 1
    """Route rebuild IMG function through placeholder"""
    try:
        if not main_window.current_img:
            main_window.log_message("No IMG file loaded")
            return False
            
        return main_window.shared_rebuild_img(main_window.current_img, options)
        
    except Exception as e:
        main_window.log_message(f"Error routing rebuild: {str(e)}")
        return False


def route_save_img(main_window, backup=True): #vers 1
    """Route save IMG function through placeholder"""
    try:
        if not main_window.current_img:
            main_window.log_message("No IMG file loaded")
            return False
            
        return main_window.shared_save_img(main_window.current_img, backup)
        
    except Exception as e:
        main_window.log_message(f"Error routing save: {str(e)}")
        return False


def get_selected_entry_names(main_window): #vers 1
    """Get selected entry names from table"""
    try:
        if not hasattr(main_window.gui_layout, 'table') or not main_window.gui_layout.table:
            return []
            
        table = main_window.gui_layout.table
        selected_rows = table.selectionModel().selectedRows()
        
        entry_names = []
        for index in selected_rows:
            row = index.row()
            name_item = table.item(row, 1)  # Column 1 is typically the name column
            if name_item:
                entry_names.append(name_item.text())
                
        return entry_names
        
    except Exception as e:
        print(f"Error getting selected entries: {str(e)}")
        return []


def disable_broken_functions(main_window): #vers 1
    """Disable broken IMG operation functions to prevent corruption"""
    try:
        # List of broken functions to disable
        broken_functions = [
            'remove_entry',
            'remove_via',
            'import_file', 
            'import_via',
            'split_via',
            'rebuild_img',
            'save_img',
            '_rebuild_version1',
            '_rebuild_version2',
            'add_entry',
            'add_multiple_entries'
        ]
        
        disabled_count = 0
        
        # Disable on main window
        for func_name in broken_functions:
            if hasattr(main_window, func_name):
                setattr(main_window, f"_disabled_{func_name}", getattr(main_window, func_name))
                setattr(main_window, func_name, lambda *args, **kwargs: main_window.log_message(f"🚫 {func_name} disabled - use shared operations"))
                disabled_count += 1
                
        # Disable on current_img if it exists
        if hasattr(main_window, 'current_img') and main_window.current_img:
            for func_name in broken_functions:
                if hasattr(main_window.current_img, func_name):
                    setattr(main_window.current_img, f"_disabled_{func_name}", getattr(main_window.current_img, func_name))
                    setattr(main_window.current_img, func_name, lambda *args, **kwargs: print(f"🚫 {func_name} disabled on IMG object"))
                    disabled_count += 1
                    
        main_window.log_message(f"🚫 Disabled {disabled_count} broken IMG functions")
        main_window.log_message("All operations now route through safe placeholder system")
        
        return disabled_count > 0
        
    except Exception as e:
        main_window.log_message(f"Error disabling broken functions: {str(e)}")
        return False


def create_safe_function_wrappers(main_window): #vers 1
    """Create safe wrapper functions for all IMG operations"""
    try:
        # Main operations
        main_window.remove_entry = lambda: route_remove_entry(main_window)
        main_window.remove_via = lambda entry_list: route_remove_via_list(main_window, entry_list)
        main_window.import_file = lambda file_path, name=None: route_import_file(main_window, file_path, name)
        main_window.import_via = lambda file_paths: route_import_via_list(main_window, file_paths)
        main_window.split_via = lambda criteria: route_split_img(main_window, criteria)
        main_window.rebuild_img = lambda options=None: route_rebuild_img(main_window, options)
        main_window.save_img = lambda backup=True: route_save_img(main_window, backup)
        
        # Additional convenience wrappers
        main_window.remove_selected_entries = lambda: route_remove_entry(main_window)
        main_window.import_single_file = lambda: route_import_file_dialog(main_window)
        main_window.import_multiple_files = lambda: route_import_files_dialog(main_window)
        main_window.validate_img = lambda: main_window.shared_validate_img(main_window.current_img) if main_window.current_img else (False, ["No IMG loaded"])
        
        main_window.log_message("Safe function wrappers created")
        main_window.log_message("All IMG operations protected from corruption")
        
        return True
        
    except Exception as e:
        main_window.log_message(f"Error creating safe wrappers: {str(e)}")
        return False


def route_import_file_dialog(main_window): #vers 1
    """Show file dialog and route import through placeholder"""
    try:
        from PyQt6.QtWidgets import QFileDialog
        
        file_path, _ = QFileDialog.getOpenFileName(
            main_window,
            "Import File to IMG",
            "",
            "All Files (*.*)"
        )
        
        if file_path:
            return route_import_file(main_window, file_path)
        else:
            main_window.log_message("Import cancelled")
            return False
            
    except Exception as e:
        main_window.log_message(f"Error in import dialog: {str(e)}")
        return False


def route_import_files_dialog(main_window): #vers 1
    """Show multiple files dialog and route import through placeholder"""
    try:
        from PyQt6.QtWidgets import QFileDialog
        
        file_paths, _ = QFileDialog.getOpenFileNames(
            main_window,
            "Import Multiple Files to IMG",
            "",
            "All Files (*.*)"
        )
        
        if file_paths:
            return route_import_via_list(main_window, file_paths)
        else:
            main_window.log_message("Import cancelled")
            return False
            
    except Exception as e:
        main_window.log_message(f"Error in import files dialog: {str(e)}")
        return False


def install_operation_routing(main_window): #vers 1
    """Install complete IMG operation routing system"""
    try:
        # Step 1: Install shared operations
        from apps.methods.img_operations_shared import install_shared_img_operations
        install_shared_img_operations(main_window)
        
        # Step 2: Disable broken functions
        disable_broken_functions(main_window)
        
        # Step 3: Create safe wrappers
        create_safe_function_wrappers(main_window)
        
        # Step 4: Install routing methods
        main_window.route_remove_entry = lambda: route_remove_entry(main_window)
        main_window.route_import_file = lambda path, name=None: route_import_file(main_window, path, name)
        main_window.route_rebuild_img = lambda options=None: route_rebuild_img(main_window, options)
        main_window.show_operation_status = lambda: main_window.show_img_operation_status()
        
        main_window.log_message("IMG operation routing system installed")
        main_window.log_message("All IMG edits route through placeholder system")
        main_window.log_message("Files protected from corruption until bugs fixed")
        
        return True
        
    except Exception as e:
        main_window.log_message(f"Error installing operation routing: {str(e)}")
        return False


__all__ = [
    'route_remove_entry',
    'route_remove_via_list', 
    'route_import_file',
    'route_import_via_list',
    'route_split_img',
    'route_rebuild_img',
    'route_save_img',
    'install_operation_routing',
    'disable_broken_functions',
    'create_safe_function_wrappers'
]
