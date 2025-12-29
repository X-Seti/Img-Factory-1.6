
def double_click_rename(main_window):
    """
    Setup double-click rename functionality
    """
    try:
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table

            # Store original double-click handler if it exists
            if hasattr(table, '_original_double_click_handler'):
                return  # Already set up

            # Connect double-click event
            table.cellDoubleClicked.connect(lambda row, col: handle_double_click_rename(main_window, row, col))

            # Mark as set up
            table._original_double_click_handler = True

            main_window.log_message("Double-click rename functionality set up")

    except Exception as e:
        main_window.log_message(f"Error setting up double-click rename: {str(e)}")
