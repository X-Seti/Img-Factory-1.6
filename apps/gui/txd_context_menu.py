# X-Seti - October14 2025 - IMG Factory 1.5 - TXD Workshop Context Menu
# This belongs in gui/ txd_context_menu.py - Version: 2
"""
TXD Workshop Context Menu System
Right-click menu for TXD Workshop - works in both docked and standalone modes
FIXED: Corrected all method names to match txd_workshop.py
"""

from PyQt6.QtWidgets import QMenu, QMessageBox
from PyQt6.QtGui import QAction
from PyQt6.QtCore import Qt

##Methods list -
# create_txd_context_menu
# setup_txd_context_menu


def create_txd_context_menu(workshop, position): #vers 2
    """
    Create comprehensive context menu for TXD Workshop
    Works in both docked (IMG Factory) and standalone modes
    """
    menu = QMenu(workshop)

    # Determine if texture is selected
    has_selection = workshop.selected_texture is not None
    has_textures = len(workshop.texture_list) > 0
    has_alpha = has_selection and workshop.selected_texture.get('has_alpha', False)

    # FILE OPERATIONS
    file_menu = menu.addMenu("File")

    if not workshop.standalone_mode:
        open_img_action = file_menu.addAction("Open IMG Archive")
        open_img_action.triggered.connect(workshop.open_img_archive)

    open_txd_action = file_menu.addAction("Open TXD File")
    open_txd_action.triggered.connect(workshop.open_txd_file)

    file_menu.addSeparator()

    save_action = file_menu.addAction("Save TXD")
    save_action.triggered.connect(workshop.save_txd_file)
    save_action.setEnabled(has_textures)

    save_as_action = file_menu.addAction("Save TXD As...")
    save_as_action.triggered.connect(workshop._save_as_new_txd)
    save_as_action.setEnabled(has_textures)

    # TEXTURE OPERATIONS
    menu.addSeparator()
    texture_menu = menu.addMenu("Texture")

    import_action = texture_menu.addAction("Import Texture...")
    import_action.triggered.connect(workshop._import_normal_texture)
    import_action.setEnabled(has_textures)

    export_action = texture_menu.addAction("Export Selected")
    export_action.triggered.connect(workshop.export_selected_texture)
    export_action.setEnabled(has_selection)

    export_all_action = texture_menu.addAction("Export All Textures")
    export_all_action.triggered.connect(workshop.export_all_textures)
    export_all_action.setEnabled(has_textures)

    texture_menu.addSeparator()

    create_action = texture_menu.addAction("Create New Texture")
    create_action.triggered.connect(workshop._create_new_texture_entry)
    create_action.setEnabled(has_textures)

    duplicate_action = texture_menu.addAction("Duplicate Selected")
    duplicate_action.triggered.connect(workshop._duplicate_texture)
    duplicate_action.setEnabled(has_selection)

    delete_action = texture_menu.addAction("Delete Selected")
    delete_action.triggered.connect(workshop._delete_texture)
    delete_action.setEnabled(has_selection)

    # EDIT OPERATIONS
    menu.addSeparator()
    edit_menu = menu.addMenu("Edit")

    rename_action = edit_menu.addAction("Rename Texture")
    rename_action.triggered.connect(workshop._rename_texture)
    rename_action.setEnabled(has_selection)

    resize_action = edit_menu.addAction("Resize Texture")
    resize_action.triggered.connect(workshop._resize_texture)
    resize_action.setEnabled(has_selection)

    upscale_action = edit_menu.addAction("Upscale Texture")
    upscale_action.triggered.connect(workshop._upscale_texture_advanced)
    upscale_action.setEnabled(has_selection)

    edit_menu.addSeparator()

    copy_action = edit_menu.addAction("Copy Texture")
    copy_action.triggered.connect(workshop._copy_texture)
    copy_action.setEnabled(has_selection)

    paste_action = edit_menu.addAction("Paste Texture")
    paste_action.triggered.connect(workshop._paste_texture)
    paste_action.setEnabled(has_selection)

    # TRANSFORM OPERATIONS
    menu.addSeparator()
    transform_menu = menu.addMenu("Transform")

    flip_v_action = transform_menu.addAction("Flip Vertical")
    flip_v_action.triggered.connect(workshop._flip_vertical)
    flip_v_action.setEnabled(has_selection)

    flip_h_action = transform_menu.addAction("Flip Horizontal")
    flip_h_action.triggered.connect(workshop._flip_horizontal)
    flip_h_action.setEnabled(has_selection)

    transform_menu.addSeparator()

    rotate_cw_action = transform_menu.addAction("Rotate 90° CW")
    rotate_cw_action.triggered.connect(workshop._rotate_clockwise)
    rotate_cw_action.setEnabled(has_selection)

    rotate_ccw_action = transform_menu.addAction("Rotate 90° CCW")
    rotate_ccw_action.triggered.connect(workshop._rotate_counterclockwise)
    rotate_ccw_action.setEnabled(has_selection)

    # ALPHA CHANNEL OPERATIONS
    if has_selection:
        menu.addSeparator()
        alpha_menu = menu.addMenu("Alpha Channel")

        if not has_alpha:
            gen_alpha_action = alpha_menu.addAction("Generate Alpha Mask")
            gen_alpha_action.triggered.connect(workshop._generate_alpha_mask)

            import_alpha_action = alpha_menu.addAction("Import Alpha Channel")
            import_alpha_action.triggered.connect(workshop._import_alpha_texture)
        else:
            view_alpha_action = alpha_menu.addAction("View Alpha Channel")
            view_alpha_action.triggered.connect(lambda: workshop.switch_texture_view())

            export_alpha_action = alpha_menu.addAction("Export Alpha Channel")
            export_alpha_action.triggered.connect(workshop._export_alpha_only)

            replace_alpha_action = alpha_menu.addAction("Replace Alpha Channel")
            replace_alpha_action.triggered.connect(workshop._import_alpha_texture)

            alpha_menu.addSeparator()

            check_alpha_action = alpha_menu.addAction("Check Alpha Validity")
            check_alpha_action.triggered.connect(lambda: workshop._check_alpha_validity(workshop.selected_texture))

    # FORMAT & COMPRESSION
    menu.addSeparator()
    format_menu = menu.addMenu("Format")

    convert_format_action = format_menu.addAction("Convert Format...")
    convert_format_action.triggered.connect(workshop._convert_format)
    convert_format_action.setEnabled(has_selection)

    compress_action = format_menu.addAction("Compress Texture")
    compress_action.triggered.connect(workshop._compress_texture)
    compress_action.setEnabled(has_selection)

    uncompress_action = format_menu.addAction("Uncompress Texture")
    uncompress_action.triggered.connect(workshop._uncompress_texture)
    uncompress_action.setEnabled(has_selection)

    format_menu.addSeparator()

    change_depth_action = format_menu.addAction("Change Bit Depth...")
    change_depth_action.triggered.connect(workshop._change_bit_depth)
    change_depth_action.setEnabled(has_selection)

    # MIPMAPS
    menu.addSeparator()
    mipmap_menu = menu.addMenu("Mipmaps")

    has_mipmaps = has_selection and len(workshop.selected_texture.get('mipmap_levels', [])) > 1

    create_mipmaps_action = mipmap_menu.addAction("Create Mipmaps")
    create_mipmaps_action.triggered.connect(workshop._create_mipmaps_dialog)
    create_mipmaps_action.setEnabled(has_selection and not has_mipmaps)

    show_mipmaps_action = mipmap_menu.addAction("Show Mipmap Levels")
    show_mipmaps_action.triggered.connect(workshop._open_mipmap_manager)
    show_mipmaps_action.setEnabled(has_mipmaps)

    remove_mipmaps_action = mipmap_menu.addAction("Remove Mipmaps")
    remove_mipmaps_action.triggered.connect(workshop._remove_mipmaps)
    remove_mipmaps_action.setEnabled(has_mipmaps)

    # BUMPMAPS (if supported)
    if has_selection and hasattr(workshop, '_has_bumpmap_data'):
        has_bumpmap = workshop._has_bumpmap_data(workshop.selected_texture)

        menu.addSeparator()
        bumpmap_menu = menu.addMenu("Bumpmap")

        if not has_bumpmap:
            gen_bumpmap_action = bumpmap_menu.addAction("Generate Bumpmap")
            gen_bumpmap_action.triggered.connect(workshop._generate_bumpmap_from_texture)

            import_bumpmap_action = bumpmap_menu.addAction("Import Bumpmap")
            import_bumpmap_action.triggered.connect(workshop._import_bumpmap)
        else:
            view_bumpmap_action = bumpmap_menu.addAction("View/Manage Bumpmap")
            view_bumpmap_action.triggered.connect(workshop._view_bumpmap)

            export_bumpmap_action = bumpmap_menu.addAction("Export Bumpmap")
            export_bumpmap_action.triggered.connect(workshop._export_bumpmap)

            delete_bumpmap_action = bumpmap_menu.addAction("Delete Bumpmap")
            delete_bumpmap_action.triggered.connect(workshop._delete_bumpmap)

    # DFF INTEGRATION
    menu.addSeparator()
    dff_menu = menu.addMenu("DFF Integration")

    check_dff_action = dff_menu.addAction("Check Against DFF...")
    check_dff_action.triggered.connect(workshop._check_txd_vs_dff)
    check_dff_action.setEnabled(has_textures)

    build_txd_action = dff_menu.addAction("Build TXD from DFF...")
    build_txd_action.triggered.connect(workshop._build_txd_from_dff)

    # VIEW OPTIONS
    menu.addSeparator()
    view_menu = menu.addMenu("View")

    if has_selection:
        cycle_view_action = view_menu.addAction("Cycle View Mode")
        cycle_view_action.triggered.connect(workshop.switch_texture_view)
        cycle_view_action.setShortcut("Space")

        view_menu.addSeparator()

    checkerboard_action = view_menu.addAction("Toggle Checkerboard")
    checkerboard_action.setCheckable(True)
    checkerboard_action.setChecked(getattr(workshop, '_show_checkerboard', False))
    checkerboard_action.triggered.connect(lambda: workshop._toggle_checkerboard())

    # TOOLS
    menu.addSeparator()
    tools_menu = menu.addMenu("Tools")

    props_action = tools_menu.addAction("Texture Properties")
    props_action.triggered.connect(workshop.show_properties)
    props_action.setEnabled(has_selection)

    stats_action = tools_menu.addAction("TXD Statistics")
    stats_action.triggered.connect(workshop._texture_statistics)
    stats_action.setEnabled(has_textures)

    tools_menu.addSeparator()

    undo_action = tools_menu.addAction("Undo")
    undo_action.triggered.connect(workshop._undo_last_action)
    undo_action.setEnabled(len(workshop.undo_stack) > 0 if hasattr(workshop, 'undo_stack') else False)
    undo_action.setShortcut("Ctrl+Z")

    # SETTINGS
    menu.addSeparator()
    settings_action = menu.addAction("Workshop Settings")
    settings_action.triggered.connect(workshop._show_workshop_settings)

    # Show menu at cursor position
    menu.exec(workshop.mapToGlobal(position))


def setup_txd_context_menu(workshop): #vers 1
    """
    Setup context menu for TXD Workshop
    Call this during workshop initialization
    """
    try:
        # Enable custom context menu on the main workshop widget
        workshop.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        workshop.customContextMenuRequested.connect(
            lambda pos: create_txd_context_menu(workshop, pos)
        )

        # Also add to texture table
        if hasattr(workshop, 'texture_table'):
            workshop.texture_table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            workshop.texture_table.customContextMenuRequested.connect(
                lambda pos: create_txd_context_menu(workshop, pos)
            )

        if workshop.main_window and hasattr(workshop.main_window, 'log_message'):
            workshop.main_window.log_message("TXD Workshop context menu enabled")

        return True

    except Exception as e:
        if workshop.main_window and hasattr(workshop.main_window, 'log_message'):
            workshop.main_window.log_message(f"Error setting up TXD context menu: {str(e)}")
        return False


# Export functions
__all__ = [
    'create_txd_context_menu',
    'setup_txd_context_menu'
]
