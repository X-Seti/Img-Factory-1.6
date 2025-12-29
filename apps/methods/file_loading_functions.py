#this belongs in methods/file_loading_functions.py - Version: 1
# X-Seti - November10 2025 - IMG Factory 1.5 - File Loading with Tab System

"""
File Loading Functions - Uses unified tab system to ensure each file gets its own tab
"""

import os
from typing import Optional

##Methods list -
# integrate_file_loading_functions
# load_col_file_in_new_tab
# load_img_file_in_new_tab
# load_txd_file_in_new_tab

def load_img_file_in_new_tab(main_window, file_path: str) -> bool: #vers 1
    """
    Load IMG file in new tab using unified tab system
    
    Args:
        file_path: Path to IMG file
    
    Returns:
        bool: Success
    """
    try:
        from apps.methods.tab_system import create_tab, update_tab_info
        
        main_window.log_message(f"Loading IMG: {os.path.basename(file_path)}")
        
        # STEP 1: Create new tab (or reuse empty one)
        tab_index = create_tab(main_window, file_path, 'IMG', None)
        
        if tab_index < 0:
            main_window.log_message("❌ Failed to create tab")
            return False
        
        # STEP 2: Load IMG file using existing thread loader
        if hasattr(main_window, 'load_thread') and main_window.load_thread and main_window.load_thread.isRunning():
            main_window.log_message("⚠️ Load already in progress")
            return False
        
        # Import thread loader
        from apps.components.Img_Factory.img_factory_thread import IMGLoadThread
        
        # Create and start load thread
        main_window.load_thread = IMGLoadThread(file_path)
        main_window.load_thread.progress_updated.connect(main_window._on_img_load_progress)
        main_window.load_thread.loading_finished.connect(
            lambda img_file: _on_img_loaded_with_tab(main_window, img_file, tab_index)
        )
        main_window.load_thread.loading_error.connect(main_window._on_img_load_error)
        main_window.load_thread.start()
        
        main_window.log_message(f"✅ IMG loading started in tab {tab_index}")
        return True
        
    except Exception as e:
        main_window.log_message(f"❌ Error loading IMG in new tab: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def _on_img_loaded_with_tab(main_window, img_file, tab_index: int): #vers 1
    """
    Handle IMG loading completion - stores file in correct tab
    
    Args:
        img_file: Loaded IMGFile object
        tab_index: Index of tab where file should be stored
    """
    try:
        from apps.methods.tab_system import update_tab_info
        
        # Update tab with loaded file object
        update_tab_info(main_window, tab_index, file_object=img_file)
        
        # Set current IMG reference
        main_window.current_img = img_file
        
        # Update UI for loaded IMG
        if hasattr(main_window.gui_layout, 'update_file_window_only'):
            main_window.gui_layout.update_file_window_only(img_file)
        
        # Hide progress
        if hasattr(main_window.gui_layout, 'hide_progress_properly'):
            main_window.gui_layout.hide_progress_properly()
        
        entry_count = len(img_file.entries) if img_file.entries else 0
        main_window.log_message(f"✅ IMG loaded in tab {tab_index}: {entry_count} entries")
        
    except Exception as e:
        main_window.log_message(f"❌ Error handling IMG load completion: {str(e)}")


def load_col_file_in_new_tab(main_window, file_path: str) -> bool: #vers 1
    """
    Load COL file in new tab using unified tab system
    
    Args:
        file_path: Path to COL file
    
    Returns:
        bool: Success
    """
    try:
        from apps.methods.tab_system import create_tab, update_tab_info
        
        main_window.log_message(f"Loading COL: {os.path.basename(file_path)}")
        
        # STEP 1: Create new tab (or reuse empty one)
        tab_index = create_tab(main_window, file_path, 'COL', None)
        
        if tab_index < 0:
            main_window.log_message("❌ Failed to create tab")
            return False
        
        # STEP 2: Load COL file using existing loader
        if hasattr(main_window, 'load_col_file_safely'):
            # Use existing COL loader but update tab afterwards
            success = main_window.load_col_file_safely(file_path)
            
            if success and hasattr(main_window, 'current_col'):
                # Update tab with loaded COL object
                update_tab_info(main_window, tab_index, file_object=main_window.current_col)
                main_window.log_message(f"✅ COL loaded in tab {tab_index}")
            
            return success
        else:
            main_window.log_message("❌ COL loader not available")
            return False
        
    except Exception as e:
        main_window.log_message(f"❌ Error loading COL in new tab: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def load_txd_file_in_new_tab(main_window, file_path: str) -> bool: #vers 1
    """
    Load TXD file in new tab using unified tab system
    
    Args:
        file_path: Path to TXD file
    
    Returns:
        bool: Success
    """
    try:
        from apps.methods.tab_system import create_tab, update_tab_info
        
        main_window.log_message(f"Loading TXD: {os.path.basename(file_path)}")
        
        # STEP 1: Create new tab (or reuse empty one)
        file_name = os.path.basename(file_path)[:-4]  # Remove .txd extension
        tab_index = create_tab(main_window, file_path, 'TXD', None)
        
        if tab_index < 0:
            main_window.log_message("❌ Failed to create tab")
            return False
        
        # STEP 2: Open TXD Workshop for this file
        from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
        
        workshop = open_txd_workshop(main_window, file_path)
        
        if workshop:
            # Store workshop reference in tab
            update_tab_info(main_window, tab_index, file_object=workshop)
            main_window.log_message(f"✅ TXD loaded in tab {tab_index}")
            return True
        else:
            main_window.log_message("❌ Failed to open TXD workshop")
            return False
        
    except Exception as e:
        main_window.log_message(f"❌ Error loading TXD in tab: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def integrate_file_loading_functions(main_window) -> bool: #vers 1
    """
    Install file loading functions on main window
    
    Returns:
        bool: Success
    """
    try:
        # Install as methods on main window
        main_window._load_img_file_in_new_tab = lambda path: load_img_file_in_new_tab(main_window, path)
        main_window._load_col_file_in_new_tab = lambda path: load_col_file_in_new_tab(main_window, path)
        main_window._load_txd_file_in_new_tab = lambda path: load_txd_file_in_new_tab(main_window, path)
        
        main_window.log_message("✅ File loading functions integrated")
        return True
        
    except Exception as e:
        main_window.log_message(f"❌ Error integrating file loading functions: {str(e)}")
        return False


# Export all functions
__all__ = [
    'load_img_file_in_new_tab',
    'load_col_file_in_new_tab',
    'load_txd_file_in_new_tab',
    'integrate_file_loading_functions'
]
