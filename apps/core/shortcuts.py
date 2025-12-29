#this belongs in core/ shortcuts.py - Version: 14
# X-Seti - July16 2025 - IMG Factory 1.5 - Keyboard Shortcuts

"""
Keyboard Shortcuts Functions
Centralizes all keyboard shortcut definitions and setup
"""

from PyQt6.QtGui import QShortcut, QKeySequence
from PyQt6.QtCore import Qt

## Methods list
# create_debug_keyboard_shortcuts
# get_shortcuts_help_text
# setup_all_shortcuts
# setup_col_shortcuts
# setup_debug_shortcuts
# setup_main_shortcuts
# setup_search_shortcuts


def create_debug_keyboard_shortcuts(main_window): #vers 3
    """Create keyboard shortcuts for debug functions - CLEAN VERSION"""
    try:
        # F12 - Quick performance mode toggle
        def toggle_performance():
            """Toggle between performance and debug mode"""
            try:
                from apps.methods.col_core_classes import is_col_debug_enabled

                if is_col_debug_enabled():
                    main_window.performance_mode()
                else:
                    main_window.minimal_debug_mode()
            except:
                if hasattr(main_window, 'toggle_col_debug'):
                    main_window.toggle_col_debug()

        perf_shortcut = QShortcut(QKeySequence("F12"), main_window)
        perf_shortcut.activated.connect(toggle_performance)

        # Ctrl+F12 - Show debug settings
        debug_shortcut = QShortcut(QKeySequence("Ctrl+F12"), main_window)
        debug_shortcut.activated.connect(main_window.show_debug_settings)

        main_window.log_message("Debug keyboard shortcuts created")
        main_window.log_message("F12: Toggle performance mode")
        main_window.log_message("Ctrl+F12: Debug settings")

    except Exception as e:
        main_window.log_message(f"Keyboard shortcuts error: {e}")


def get_shortcuts_help_text(): #vers 10
    """Get formatted shortcuts help text for display"""
    return """
    <h2>Keyboard Shortcuts</h2>
    <table border="1" cellpadding="5" style="border-collapse: collapse;">
    <tr><th>Action</th><th>Shortcut</th></tr>
    <tr><td>New IMG</td><td>Ctrl+N</td></tr>
    <tr><td>Open IMG</td><td>Ctrl+O</td></tr>
    <tr><td>Close IMG</td><td>Ctrl+W</td></tr>
    <tr><td>Save</td><td>Ctrl+S</td></tr>
    <tr><td>Import Files</td><td>Ctrl+I</td></tr>
    <tr><td>Export Selected</td><td>Ctrl+E</td></tr>
    <tr><td>Remove Selected</td><td>Delete</td></tr>
    <tr><td>Rename Entry</td><td>F2</td></tr>
    <tr><td>Select All</td><td>Ctrl+A</td></tr>
    <tr><td>Find</td><td>Ctrl+F</td></tr>
    <tr><td>Find Next</td><td>F3</td></tr>
    <tr><td>Find Previous</td><td>Shift+F3</td></tr>
    <tr><td>IMG Information</td><td>F4</td></tr>
    <tr><td>Validate IMG</td><td>F5</td></tr>
    <tr><td>Rebuild IMG</td><td>F6</td></tr>
    <tr><td>View Model</td><td>F7</td></tr>
    <tr><td>View Texture</td><td>F8</td></tr>
    <tr><td>View Collision</td><td>F9</td></tr>
    <tr><td>Fullscreen</td><td>F11</td></tr>
    <tr><td>Performance Mode</td><td>F12</td></tr>
    <tr><td>COL Editor</td><td>Ctrl+Shift+C</td></tr>
    <tr><td>Batch Processor</td><td>Ctrl+Shift+B</td></tr>
    <tr><td>Analyze COL</td><td>Ctrl+Shift+A</td></tr>
    <tr><td>Preferences</td><td>Ctrl+,</td></tr>
    <tr><td>Debug Settings</td><td>Ctrl+F12</td></tr>
    <tr><td>Exit</td><td>Ctrl+Q</td></tr>
    </table>
    """


def setup_all_shortcuts(main_window): #vers 11
    """Setup all keyboard shortcuts"""
    try:
        main_window.log_message("Setting up keyboard shortcuts...")

        # Setup different categories
        setup_main_shortcuts(main_window)
        setup_col_shortcuts(main_window)
        setup_search_shortcuts(main_window)
        setup_debug_shortcuts(main_window)
        create_debug_keyboard_shortcuts(main_window)

        main_window.log_message("All keyboard shortcuts setup complete")
        return True

    except Exception as e:
        main_window.log_message(f"Shortcuts setup failed: {str(e)}")
        return False


def setup_col_shortcuts(main_window): #vers 11
    """Setup COL-specific shortcuts"""
    try:
        col_shortcuts = [
            ("Ctrl+Shift+C", "open_col_editor_dialog", "COL Editor"),
            ("Ctrl+Shift+B", "open_col_batch_proc_dialog", "Batch Processor"),
            ("Ctrl+Shift+A", "analyze_col_file_dialog", "Analyze COL"),
            ("Ctrl+Shift+O", "open_col_file_dialog", "Open COL file"),
        ]
        
        created_shortcuts = []
        
        for key_sequence, method_name, description in col_shortcuts:
            # Try to get method from various sources
            method = None
            
            # Check main window
            if hasattr(main_window, method_name):
                method = getattr(main_window, method_name)
            
            # Check gui_context module
            if not method:
                try:
                    from gui.gui_context import (
                        open_col_file_dialog, 
                        open_col_batch_proc_dialog,
                        open_col_editor_dialog,
                        analyze_col_file_dialog
                    )
                    
                    method_map = {
                        'open_col_file_dialog': lambda: open_col_file_dialog(main_window),
                        'open_col_batch_proc_dialog': lambda: open_col_batch_proc_dialog(main_window),
                        'open_col_editor_dialog': lambda: open_col_editor_dialog(main_window),
                        'analyze_col_file_dialog': lambda: analyze_col_file_dialog(main_window)
                    }
                    
                    method = method_map.get(method_name)
                except ImportError:
                    pass
            
            if method and callable(method):
                shortcut = QShortcut(QKeySequence(key_sequence), main_window)
                shortcut.activated.connect(method)
                created_shortcuts.append(f"{key_sequence} → {description}")
            else:
                # Create placeholder
                placeholder = lambda desc=description: main_window.log_message(f"Fail: {desc} - not yet implemented")
                shortcut = QShortcut(QKeySequence(key_sequence), main_window)
                shortcut.activated.connect(placeholder)
                created_shortcuts.append(f"{key_sequence} → {description} (placeholder)")
        
        if created_shortcuts:
            main_window.log_message(f"Created {len(created_shortcuts)} COL shortcuts")
        
        return True
        
    except Exception as e:
        main_window.log_message(f"COL shortcuts setup error: {str(e)}")
        return False


def setup_debug_shortcuts(main_window): #vers 4
    """Setup debug and performance shortcuts"""
    try:
        # F12 - Toggle performance mode
        def toggle_performance():
            """Toggle between performance and debug mode"""
            try:
                from apps.components.col_debug_control import is_col_debug_enabled

                if is_col_debug_enabled():
                    if hasattr(main_window, 'performance_mode'):
                        main_window.performance_mode()
                    else:
                        main_window.log_message("Performance mode activated")
                else:
                    if hasattr(main_window, 'minimal_debug_mode'):
                        main_window.minimal_debug_mode()
                    else:
                        main_window.log_message("Debug mode activated")
            except:
                if hasattr(main_window, 'toggle_col_debug'):
                    main_window.toggle_col_debug()
                else:
                    main_window.log_message("Debug toggle - not available")

        perf_shortcut = QShortcut(QKeySequence("F12"), main_window)
        perf_shortcut.activated.connect(toggle_performance)

        # Ctrl+F12 - Show debug settings
        debug_shortcut = QShortcut(QKeySequence("Ctrl+F12"), main_window)
        if hasattr(main_window, 'show_debug_settings'):
            debug_shortcut.activated.connect(main_window.show_debug_settings)
        else:
            debug_shortcut.activated.connect(lambda: main_window.log_message("🔧 Debug settings - not yet implemented"))

        main_window.log_message("Debug shortcuts created")
        main_window.log_message("F12: Toggle performance mode")
        main_window.log_message("Ctrl+F12: Debug settings")

        return True

    except Exception as e:
        main_window.log_message(f"Debug shortcuts error: {str(e)}")
        return False


def setup_main_shortcuts(main_window): #vers 5
    """Setup main application shortcuts"""
    try:
        shortcuts = [
            # File operations
            ("Ctrl+N", "create_new_img", "Create new IMG"),
            ("Ctrl+O", "open_img_file", "Open IMG file"),
            ("Ctrl+W", "close_img_file", "Close IMG file"),
            ("Ctrl+S", "save_img", "Save IMG"),
            ("Ctrl+Q", "close", "Exit application"),

            # Entry operations
            ("Ctrl+I", "import_files", "Import files"),
            ("Ctrl+E", "export_selected", "Export selected"),
            ("Delete", "remove_selected", "Remove selected"),
            ("F2", "rename_selected", "Rename entry"),
            ("Ctrl+A", "select_all_entries", "Select all"),
            ("Ctrl+Z", "undo", "Undo last action"),
            ("Ctrl+Y", "redo", "Redo last action"),

            # View operations
            ("F4", "show_img_info", "IMG information"),
            ("F5", "validate_img", "Validate IMG"),
            ("F6", "rebuild_img", "Rebuild IMG"),
            ("F7", "view_model", "View model"),
            ("F8", "view_texture", "View texture"),
            ("F9", "view_collision", "View collision"),
            ("F11", "toggle_fullscreen", "Toggle fullscreen"),
            ("F12", "toggle_performance_mode", "Toggle performance mode"),

            # Search operations
            ("Ctrl+F", "show_search_dialog", "Find/Search"),
            ("F3", "find_next", "Find next"),
            ("Shift+F3", "find_previous", "Find previous"),

            # Settings
            ("Ctrl+,", "show_settings", "Preferences"),
            ("Ctrl+F12", "show_debug_settings", "Debug settings"),
        ]

        created_shortcuts = []

        for key_sequence, method_name, description in shortcuts:
            method = getattr(main_window, method_name, None)
            if method and callable(method):
                shortcut = QShortcut(QKeySequence(key_sequence), main_window)
                shortcut.activated.connect(method)
                created_shortcuts.append(f"{key_sequence} → {description}")
            else:
                # Create placeholder for missing methods
                placeholder = lambda desc=description: main_window.log_message(f"Fail: {desc} - not yet implemented")
                shortcut = QShortcut(QKeySequence(key_sequence), main_window)
                shortcut.activated.connect(placeholder)
                created_shortcuts.append(f"{key_sequence} → {description} (placeholder)")

        main_window.log_message(f"Created {len(created_shortcuts)} main shortcuts")
        return True

    except Exception as e:
        main_window.log_message(f"Main shortcuts setup error: {str(e)}")
        return False


def setup_search_shortcuts(main_window): #vers 4
    """Setup search-specific shortcuts"""
    try:
        # Try to get search manager
        search_manager = getattr(main_window, 'search_manager', None)
        
        if search_manager:
            # Advanced search dialog (Ctrl+F)
            search_shortcut = QShortcut(QKeySequence("Ctrl+F"), main_window)
            search_shortcut.activated.connect(search_manager.show_search_dialog)
            
            # Find next (F3)
            find_next_shortcut = QShortcut(QKeySequence("F3"), main_window)
            find_next_shortcut.activated.connect(search_manager.find_next)
            
            # Find previous (Shift+F3)
            find_prev_shortcut = QShortcut(QKeySequence("Shift+F3"), main_window)
            find_prev_shortcut.activated.connect(search_manager.find_previous)
            
            main_window.log_message("Search shortcuts connected to search manager")
        else:
            # Create placeholder shortcuts
            search_shortcut = QShortcut(QKeySequence("Ctrl+F"), main_window)
            search_shortcut.activated.connect(lambda: main_window.log_message("Search dialog - not yet implemented"))
            
            find_next_shortcut = QShortcut(QKeySequence("F3"), main_window)
            find_next_shortcut.activated.connect(lambda: main_window.log_message("Find next - not yet implemented"))
            
            find_prev_shortcut = QShortcut(QKeySequence("Shift+F3"), main_window)
            find_prev_shortcut.activated.connect(lambda: main_window.log_message("Find previous - not yet implemented"))
            
            main_window.log_message("Search shortcuts created (placeholders)")
        
        return True
        
    except Exception as e:
        main_window.log_message(f"Search shortcuts setup error: {str(e)}")
        return False


# Export functions
__all__ = [
    'create_debug_keyboard_shortcuts',
    'get_shortcuts_help_text',
    'setup_all_shortcuts',
    'setup_col_shortcuts',
    'setup_debug_shortcuts',
    'setup_main_shortcuts',
    'setup_search_shortcuts'
]
