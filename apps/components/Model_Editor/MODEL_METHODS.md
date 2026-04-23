# MODEL WORKSHOP — Method List
# Auto-generated. Each entry: ClassName.method_name(args) #vers N — summary
# File: apps/components/Model_Editor/model_workshop.py


## _DFFGeometryAdapter
#    Line 88

    __init__(geometry, geometry_index) #vers 1
    vertex_count() 
    face_count() 
    materials() #vers 1 — Expose DFF geometry materials for use by the viewport renderer.
    __repr__() #vers 1

## COL3DViewport
#    Line 129

    __init__(parent) #vers 1
    set_current_file(col_file)  [STUB]
    set_view_options() 
    set_current_model(model, index) #vers 1
    zoom_in() #vers 1
    zoom_out() #vers 1
    reset_view() #vers 1
    fit_to_window() #vers 1
    pan(dx, dy) #vers 1
    rotate_cw() #vers 1
    rotate_ccw() #vers 1
    flip_horizontal() #vers 1
    flip_vertical() #vers 1
    set_background_color(rgb) #vers 1
    _set_theme_bg(palette) #vers 1 — Set background from palette — light theme=white, dark=near-black.
    set_show_spheres(v) #vers 1
    set_show_boxes(v) #vers 1
    set_show_mesh(v) #vers 1
    set_backface(v) #vers 1
    load_textures(mod_textures) #vers 1 — Build QImage cache from _mod_textures list for textured rendering.
    set_render_style(s) #vers 1 — Set render style: wireframe / semi / solid / textured.
    toggle_gizmo_mode() #vers 1
    _set_gizmo(mode) #vers 1
    _proj(x, y, z) #vers 2 — Project 3D world point → 2D screen pixel.
    _get_scale_origin() #vers 1 — Return (scale, ox, oy) mapping 3D projected coords to screen pixels.
    _to_screen(x, y, z) #vers 1
    _gizmo_centre() #vers 1 — Screen coords of gizmo origin (model centroid).
    _gizmo_arm() #vers 1
    _hit_gizmo(mx, my) #vers 1 — Return axis 'X'/'Y'/'Z' if click near a gizmo handle, else None.
    set_paint_mode(enabled, material_id) #vers 1 — Enable/disable paint mode. In paint mode LMB click paints a face.
    _pick_face(mx, my) #vers 1 — Return (face_index, face) of the closest face whose projected centroid
    mousePressEvent(event) #vers 1
    mouseMoveEvent(event) #vers 1
    mouseReleaseEvent(event) #vers 1
    resizeEvent(event) #vers 1
    wheelEvent(event) #vers 1
    keyPressEvent(event) #vers 1
    _cycle_render_style() #vers 1
    contextMenuEvent(event) #vers 1
    _set_angles(yaw, pitch) #vers 1
    paintEvent(event) #vers 1 — Fully self-contained paint — grid, mesh, boxes, spheres, bounds, gizmo, HUD.
    _apply_to_selected_faces() #vers 1
    _apply_to_all_faces() #vers 1
    _show_face_context_menu(global_pos, face_index, face) #vers 1 — Right-click context menu for a picked face — material operations.
    _find_workshop() 

## ModelListWidget
#    Line 1539

    __init__(parent) 
    populate_models() #vers 1 — Populate model list
    on_selection_changed(row) #vers 1 — Handle selection change
    show_context_menu(position) #vers 1 — Show context menu

## _ModelListDelegate
#    Line 1595

    paint(painter, option, index) #vers 1
    sizeHint(option, index) #vers 1

## ModelWorkshop
#    Line 1629

    get_menu_title() #vers 1 — Return menu label for imgfactory menu bar.
    _build_menus_into_qmenu(parent_menu) #vers 1 — Populate parent_menu with Model Workshop actions.
    __init__(parent, main_window) #vers 10 — initialize_features
    setup_ui() #vers 8 — Setup the main UI layout
    _connect_all_buttons() #vers 2 — Wire flip/rotate transform buttons to preview_widget.
    _create_new_model() #vers 1
    _delete_selected_model() #vers 2 — Delete selected collision model(s) — uses currentRow() for reliability.
    _duplicate_selected_model() #vers 1
    _copy_model_to_clipboard() #vers 1
    _paste_model_from_clipboard() #vers 1
    _open_surface_paint_dialog() #vers 2 — Open material paint dialog (delegates to _open_paint_editor).
    _open_dff_material_editor(geom_idx, mat_idx) #vers 2 — Alias — opens the unified Material Editor, pre-selecting geom_idx/mat_idx.
    _open_material_list_or_surface_types() #vers 1 — Toolbar: Material List in DFF mode, Surface Types in COL mode.
    _open_material_editor_or_surface_edit() #vers 1 — Toolbar: Material Editor in DFF mode, Surface Editor in COL mode.
    _open_dff_material_list() #vers 9 — Material Editor — 3ds Max style.
    _open_surface_type_dialog() #vers 1 — Show surface material type picker for selected model.
    _cycle_view_render_style() #vers 1 — Cycle viewport render: wireframe -> semi -> solid.
    _open_paint_editor() #vers 5 — Enter paint mode — works on DFF model materials or COL faces.
    _open_paint_mat_popup() #vers 2 — Searchable material popup anchored below the mat chip.
    _apply_to_selected_faces_paint() #vers 1 — Apply current paint material to all selected faces (F key in paint mode).
    _paint_cycle_mat(delta) #vers 1 — Cycle active paint material by delta steps (+1 next / -1 prev).
    _show_paint_toolbar(mat_id, model) #vers 5 — Populate combo then show the floating paint bar.
    _on_painted_face(face_index, face) #vers 2 — Called by viewport when a face is painted. Status update only —
    _set_paint_tool(mode) #vers 1 — Switch active paint tool: 'paint' | 'dropper' | 'fill'.
    _exit_paint_mode() #vers 2 — Exit paint mode — hide toolbar, restore paint button.
    _on_paint_mode_exited() #vers 1 — Called by viewport Escape key — sync button state.
    _get_selected_model() #vers 4 — Return the currently selected COLModel or None.
    _set_status(msg) #vers 1 — Write msg to the status label (whichever one exists).
    _create_new_surface() #vers 1 — Add a new empty COL model to the loaded file.
    _open_surface_edit_dialog() #vers 2 — Open the COL Mesh Editor for the currently selected model.
    _build_col_from_txd() #vers 2 [STUB] — Create stub COL models for each texture name in a loaded TXD.
    _dff_to_col_surfaces(single) #vers 1 — Generate COL from DFF model — maps texture names to COL surface types.
    _cycle_render_mode() #vers 1
    _convert_surface() #vers 2 — Convert selected model to a different COL version.
    _show_shadow_mesh() #vers 2 — Show shadow mesh info for selected model.
    _create_shadow_mesh() #vers 2 — Auto-generate shadow mesh as a copy of the main collision mesh.
    _remove_shadow_mesh() #vers 2 — Remove shadow mesh data from the selected COL model.
    _compress_col() #vers 2 — Mark COL file for compressed output (sets flags on export).
    _uncompress_col() #vers 2 — Reload COL file (parses fresh from disk, clears in-memory edits).
    _open_render_settings_dialog() #vers 1 — Render & background settings dialog.
    _compress_surface() #vers 1
    _copy_surface() #vers 1
    _delete_surface() #vers 1
    _duplicate_surface() #vers 1
    _force_save_col() #vers 1
    _import_selected() #vers 1
    _import_surface() #vers 1
    _open_col_file() #vers 1
    _open_mipmap_manager() #vers 1
    _paste_surface() #vers 1
    _reload_surface_table() #vers 1
    _remove_shadow() #vers 1
    _save_as_col_file() #vers 1
    _save_col_file() #vers 1
    _saveall_file() #vers 1
    _uncompress_surface() #vers 1
    export_all() #vers 1
    export_all_surfaces() #vers 1
    export_selected() #vers 1
    export_selected_surface() #vers 1
    refresh() #vers 1
    reload_surface_table() #vers 1
    save_col_file() #vers 1
    shadow_dialog() #vers 1
    switch_surface_view() #vers 1
    _change_format()  [STUB]
    _close_col_tab()  [STUB]
    _edit_main_surface()  [STUB]
    _focus_search()  [STUB]
    _rename_shadow_shortcut()  [STUB]
    _save_surface_name()  [STUB]
    _show_detailed_info()  [STUB]
    _show_surface_info()  [STUB]
    show_help()  [STUB]
    show_settings_dialog() 
    _set_thumbnail_view(yaw, pitch, label) #vers 1 — Change the view angle for all thumbnails and regenerate them.
    _regenerate_all_thumbnails() #vers 1 — Redraw every thumbnail in both lists at current _thumb_yaw/pitch.
    _start_thumbnail_spin(row, model) #vers 1 — Start slowly rotating the thumbnail of the selected row.
    _stop_thumbnail_spin() #vers 1 — Stop any running thumbnail rotation.
    _tick_thumbnail_spin() #vers 1 — Advance the spin angle and update the thumbnail.
    _enable_name_edit(event, is_alpha) #vers 1 — Enable name editing on click
    _update_status_indicators() #vers 2 — Update status indicators
    _create_status_bar() #vers 1 — Create bottom status bar - single line compact
    _refresh_icons() #vers 2 — Refresh all button icons after theme change — picks up current text_primary colo
    _show_workshop_settings() #vers 1 — Show complete workshop settings dialog
    _apply_window_flags() #vers 1 — Apply window flags based on settings
    _apply_always_on_top() #vers 1 — Apply always on top window flag
    _scan_available_locales() #vers 2 — Scan locale folder and return list of available languages
    _show_amiga_locale_error() #vers 2 — Show Amiga Workbench 3.1 style error dialog
    _update_dock_button_visibility() #vers 2 — Show/hide dock and tearoff buttons based on docked state
    toggle_dock_mode() #vers 2 — Toggle between docked and standalone mode
    _dock_to_main() #vers 9 — Dock handled by overlay system in imgfactory - IMPROVED
    _undock_from_main() #vers 4 — Undock from overlay mode to standalone window - IMPROVED
    _apply_button_mode(dialog) #vers 1 — Apply button display mode
    _initialize_features() #vers 3 — Initialize all features after UI setup
    _is_on_draggable_area(pos) #vers 7 — Check if position is on draggable titlebar area
    _update_all_buttons() #vers 4 — Update all buttons to match display mode
    paintEvent(event) #vers 2 — Paint corner resize triangles
    _get_resize_corner(pos) #vers 3 — Determine which corner is under mouse position
    mousePressEvent(event) #vers 8 — Handle ALL mouse press - dragging and resizing
    mouseMoveEvent(event) #vers 4 — Handle mouse move for resizing and hover effects
    mouseReleaseEvent(event) #vers 2 — Handle mouse release
    _handle_corner_resize(global_pos) #vers 2 — Handle window resizing from corners
    _get_resize_direction(pos) #vers 1 — Determine resize direction based on mouse position
    _update_cursor(direction) #vers 1 — Update cursor based on resize direction
    _handle_resize(global_pos) #vers 1 — Handle window resizing
    resizeEvent(event) #vers 2 — Update layout on resize.
    _on_splitter_moved(pos, index) #vers 1 — Called when main splitter is dragged - update text panel visibility.
    _update_transform_text_panel_visibility() #vers 3 — DockableToolbar version — toolbars manage their own visibility.
    _load_mod_toolbar_layouts() #vers 1 — Restore saved toolbar layouts on startup.
    showEvent(event) #vers 1 — On first show, restore toolbar layouts after Qt has laid out everything.
    resizeEvent(event) #vers 4 — Keep resize grip in corner; auto-collapse panels when narrow.
    _update_tex_btn_compact() #vers 1 — Show/hide label text on texture panel buttons based on available width.
    mouseDoubleClickEvent(event) #vers 2 — Handle double-click - maximize/restore
    _toggle_maximize() #vers 1 — Toggle window maximize state
    closeEvent(event) #vers 1 — Handle close event
    _create_toolbar() #vers 13 — Create toolbar - Hide drag button when docked, ensure buttons visible
    _create_col_from_dff() #vers 1 — Generate a COL collision file from the currently loaded DFF model.
    _toggle_viewport_shading(enabled) #vers 1 — Toggle Lambertian shading on/off in the viewport.
    _open_light_setup_dialog() #vers 2 — Viewport light setup — visual position picker + sliders.
    _load_viewport_light_settings() #vers 1 — Load saved viewport light settings from model_workshop.json.
    _apply_prelighting() #vers 1 [STUB] — Apply vertex prelighting to DFF model — stub, full impl in next session.
    _prelight_setup_dialog() #vers 1 [STUB] — Light source setup for prelighting — stub.
    _enable_dff_toolbar(dff_mode) #vers 2 — Switch left toolbar between DFF mode and COL mode.
    _set_select_mode(mode) #vers 1 — Set viewport selection mode: vertex / edge / face / poly / object.
    _toggle_backface_cull() #vers 1 — Toggle backface culling — when ON only the front face is visible/selectable.
    _toggle_front_only_paint() #vers 1 — Toggle front-face-only paint — prevents painting geometry behind the current vie
    _create_primitive_dialog() #vers 1 — Show dialog to create a primitive shape (box or sphere) as a new DFF geometry.
    _build_primitive(shape, w, h, d, nx, ny, nz)  — Build vertex + triangle lists for a primitive.
    _add_geometry_to_dff(verts, tris, name)  — Add a new geometry to the current DFF model from raw verts+tris.
    _create_transform_icon_panel() #vers 13 — Icon grid panel - DockableToolbar pattern (same as COL Workshop).
    _mod_place_icon_grid(n_cols) #vers 1
    _reflow_mod_left_toolbar(pos) #vers 1
    _create_transform_text_panel() #vers 12 — Create transform panel with text - aligned with icon panel
    _create_left_panel() #vers 5 — Create left panel - COL file list (only in IMG Factory mode)
    _create_middle_panel() #vers 6 — Create middle panel with COL models table — mini toolbar + view toggle.
    _create_right_panel() #vers 12 — Create right panel — DockableToolbar layout (same as COL Workshop).
    _create_paint_bar() #vers 3 — Floating paint bar — QWidget child of preview_widget, sits at top of viewport.
    _create_preview_controls() #vers 7 — Right toolbar icon grid — DockableToolbar pattern.
    _mod_place_ctrl_grid(n_cols) #vers 1
    _reflow_mod_right_toolbar(pos) #vers 1
    _update_toolbar_for_docking_state() #vers 1 — Update toolbar visibility based on docking state
    _apply_title_font() #vers 1 — Apply title font to title bar labels
    _apply_panel_font() #vers 1 — Apply panel font to info panels and labels
    _apply_button_font() #vers 1 — Apply button font to all buttons
    _apply_infobar_font() #vers 1 — Apply fixed-width font to info bar at bottom
    _load_img_col_list() #vers 2 — Load COL files from IMG archive
    _pan_preview(dx, dy) #vers 2 — Pan preview by dx, dy pixels - FIXED
    _pick_background_color() #vers 1 — Open color picker for background
    _set_checkerboard_bg() #vers 1 — Set checkerboard background
    _create_level_card(level_data) #vers 2 — Create modern level card matching mockup
    _create_preview_widget(level_data) #vers 5 — Create preview widget - CollisionPreviewWidget for col_workshop
    _create_info_section(level_data) #vers 1 — Create info section with stats grid
    _create_stats_grid(level_data) #vers 1 — Create stats grid
    _create_stat_box(label, value, value_color) #vers 1 — Create individual stat box
    _create_action_section(level_data) #vers 1 — Create action buttons section
    _toggle_tearoff() #vers 2 — Toggle tear-off state (merge back to IMG Factory) - IMPROVED
    _open_settings_dialog() #vers 1 — Open settings dialog and refresh on save
    _launch_theme_settings() #vers 2 — Launch theme engine from app_settings_system
    _setup_settings_button() #vers 1 — Setup settings button in UI
    _show_settings_dialog() #vers 5 — Show comprehensive settings dialog with all tabs including hotkeys
    _show_settings_context_menu(pos) #vers 1 — Show context menu for Settings button
    _enable_move_mode() #vers 2 — Enable move window mode using system move
    _toggle_upscale_native() #vers 1 — Toggle upscale native resolution
    _show_shaders_dialog() #vers 2 — Viewport render style presets.
    _show_window_context_menu(pos) #vers 1 — Show context menu for titlebar right-click
    _get_icon_color() #vers 2 — Get icon colour from current theme — returns text_primary.
    _apply_fonts_to_widgets() #vers 1 — Apply fonts from AppSettings to all widgets
    _apply_theme() #vers 5 — Apply global app theme — uses QApplication stylesheet set by app_settings.
    _apply_settings(dialog) #vers 5 — Apply settings from dialog
    _refresh_main_window() #vers 1 — Refresh the main window to show changes
    _import_model() #vers 1 — Import model from external format (MDL, OBJ, FBX…).
    _import_obj(path) #vers 1 — Import Wavefront OBJ as a new DFF geometry.
    _export_model_menu() #vers 1 — Show export format menu: COL, CST, OBS, 3DS, OBJ…
    _export_not_implemented(fmt) #vers 1
    _open_txd_combined(_checked) #vers 1 — Open TXD — smart loader for DFF+TXD workflow.
    _open_dff_standalone() #vers 2 — Open DFF + optionally TXD in one combined dialog sequence.
    _open_txd_standalone() #vers 2 — Open a TXD file — loads textures into Model Workshop AND opens TXD Workshop.
    _open_file() #vers 1 — Open file dialog — supports DFF (model) and COL (collision) files.
    _create_texture_panel() #vers 2 — Collapsible texture panel in middle column.
    _toggle_tex_view() #vers 1 — Switch texture panel between list view and 64×64 thumbnail grid.
    _populate_tex_thumbnails() #vers 1 — Fill the thumbnail grid with 64×64 previews of all loaded textures.
    _show_tex_popup(tex) #vers 1 — Show a texture at up to 128×128 (or native size if smaller) in a
    _show_tex_hover(anchor_widget, tex) #vers 1 — Show a frameless texture preview that follows the mouse into the popup.
    _hide_tex_hover(anchor_widget) #vers 1 — Close the hover popup if open.
    _compute_face_shade(v0, v1, v2, ambient, light)  — Return a shade factor [0..1] for a triangle via Lambertian diffuse.
    _auto_load_txd_from_imgs(txd_stem) #vers 1 — Search all open IMG tabs and current_img for txd_stem.txd.
    _auto_load_from_texlist(txd_stem) #vers 1 — Scan the configured texlist/ folder recursively for image files
    _load_iff_as_qimage(path) #vers 1 — Load an Amiga IFF ILBM file and return a QImage (RGBA8888), or None.
    _set_texlist_folder() #vers 1 — Browse for a texlist/ root folder and persist to settings.
    _save_texlist_setting() #vers 1 — Persist the texlist folder path to ~/.config/imgfactory/model_workshop.json
    _load_texlist_setting() #vers 1 — Load the texlist folder path from ~/.config/imgfactory/model_workshop.json
    _load_txd_into_workshop() #vers 1 — Open a TXD file and load its textures into Model Workshop.
    _parse_txd_lightweight(data) #vers 1 — Lightweight TXD parser — no UI, returns list of texture dicts.
    _load_txd_file(path) #vers 3 — Parse a TXD file, populate texture panel, and feed textures into viewport.
    _add_textures_from_txd(path) #vers 2 — Add textures from a TXD file to existing texture list (no replace).
    _populate_texture_list() #vers 1 — Fill the texture QTableWidget from self._mod_textures.
    _browse_texlist_folder() #vers 1 — Browse a Texlist folder — shows all TXDs, lets user add individual textures.
    _on_tex_selected() #vers 1 — Texture row selected — show preview in status.
    _tex_context_menu(pos) #vers 1 — Right-click context menu on texture list.
    _remove_selected_textures() #vers 1
    _export_textures_as_png() #vers 1 — Export selected textures to PNG files.
    _pass_textures_to_txd_workshop() #vers 1 — Send current textures to TXD Workshop for editing.
    _build_txd_from_textures() #vers 1 — Build a minimal TXD binary from self._mod_textures.
    _save_textures_as_txd() #vers 1 — Save current textures as a new TXD file.
    _find_col_via_db(model_name) #vers 1 — Look up COL for model_name in asset_db and open in COL Workshop.
    _get_ide_db() #vers 2 — Return IDEDatabase — prefers the one built by DAT Browser on load
    _lookup_ide_from_db(stem) #vers 1 — Look up a model in the standalone IDEDatabase fallback.
    _get_xref() #vers 1 — Return GTAWorldXRef from DAT Browser if loaded, else None.
    _lookup_ide_for_dff(dff_path) #vers 2 — Look up IDEObject for a DFF file path via DAT Browser xref.
    _open_linked_txd() #vers 1 — Open the IDE-linked TXD in TXD Workshop.
    _find_in_ide() #vers 1 — Switch to DAT Browser tab and highlight this model's IDE entry.
    open_dff_file(file_path) #vers 1 — Open and display a GTA DFF model file.
    _populate_dff_detail_table(model) #vers 1 — Fill the detail table (collision_list) with DFF geometry info.
    _tbl_item(text) #vers 1 — Helper: create a non-editable QTableWidgetItem.
    _populate_frame_tree(model) #vers 1 — Populate the frame/bone hierarchy tree widget.
    _on_frame_tree_clicked(item, column) #vers 1 — When user clicks a frame in the tree, highlight the associated geometry.
    _display_dff_model(model) #vers 3 — Populate the workshop UI with a loaded DFF model.
    _on_dff_geom_selected_tbl() #vers 1 — QTableWidget selection changed → update viewport.
    _on_dff_geom_selected(row) #vers 1 — Called when user selects a geometry in the mesh list.
    _show_dff_geometry(index) #vers 1 — Push a DFF geometry adapter into the 3D viewport.
    _export_dff_obj() #vers 1 — Export the currently loaded DFF model to Wavefront OBJ + MTL.
    _toggle_col_view() #vers 1 — Toggle between detail table and compact thumbnail+name list.
    _populate_compact_col_list() #vers 1 — Fill compact two-column list (icon + name/version/counts).
    _on_compact_col_selected() #vers 4 — Handle compact [=] list selection — routes to DFF or COL handler.
    _push_undo(model_index, description) #vers 1 — Deep-copy model[model_index] onto undo stack before any edit.
    _undo_last_action() #vers 2 — Restore the last deep-copied model from the undo stack.
    _select_all_models() #vers 1 — Select all entries in the active list (Ctrl+A).
    _invert_selection() #vers 1 — Invert the current selection.
    _sort_models(key) #vers 1 — Sort collision models in place by key: 'name','version','faces','boxes','spheres
    _show_sort_menu() #vers 1 — Show sort options popup.
    _sort_models_desc(key) #vers 1 — Sort descending (largest first).
    _toggle_pin_selected() #vers 1 — Toggle pin (edit-lock) on selected models. Pinned models can't be deleted/rename
    _is_model_pinned(row) #vers 1 — Return True if model at row is pinned.
    _import_via_ide() #vers 1 — Import COL entries referenced by the currently loaded IDE file.
    _remove_via_ide() #vers 1 — Remove collision models NOT referenced by an IDE file (cleanup).
    _export_via_ide() #vers 1 — Export only models referenced by an IDE file.
    _export_col_data() #vers 2 — Extract/export selected COL models (or all) to individual .col files.
    _save_file() #vers 2 — Save current COL file — serialises all models via COLWriter.
    _save_file_as() #vers 1 — Save As dialog
    _load_settings() #vers 1 — Load settings from config file
    _save_settings() #vers 1 — Save settings to config file
    open_col_file(file_path) #vers 3 — Open standalone COL file - supports COL1, COL2, COL3
    _find_all_paint_btns() #vers 1 — Return all paint buttons from both icon and text panels.
    _set_col_buttons_enabled(enabled) #vers 1 — Enable/disable all transform buttons in BOTH icon and text panels.
    _pick_col_from_current_img() #vers 1 — Pick a COL entry from the IMG currently loaded in IMG Factory and open it.
    _open_col_from_img_entry(img, entry) #vers 1 — Extract a COL entry from an IMGFile and load it into the workshop.
    open_img_archive() #vers 1 — Open file dialog to select an IMG archive and load COL entries from it
    load_from_img_archive(img_path) #vers 3 — Load IMG archive — populates left panel with all DFF/COL/TXD entries.
    _analyze_collision() #vers 1 — Analyze current COL file
    _populate_left_panel_from_img(img) #vers 4 — Populate the left panel list from an already-open IMGFile object.
    _load_txd_file_from_data(data, name) #vers 1 — Load TXD from raw bytes into the texture panel.
    _on_col_selected(item) #vers 2 — Handle entry selection from left panel — routes by extension.
    _extract_col_from_img(entry) #vers 2 — Extract TXD data from IMG entry
    _paint_model_onto(painter, model, W, H, yaw, pitch, zoom, pan_x, pan_y, flip_h, flip_v, show_spheres, show_boxes, show_mesh, backface, render_style, bg_color, gizmo_mode, viewport)  — Paint COL model: grid → geometry → gizmo → HUD.
    _get_view_coords(model, view) #vers 1 — Get all geometry points projected to 2D using the selected view axis.
    _project_model_2d(model, width, height, padding, yaw, pitch, flip_h, flip_v)  — Project COL model geometry to 2D canvas using yaw/pitch rotation.
    _draw_col_model(painter, model, width, height, padding, yaw, pitch, flip_h, flip_v)  — Draw COL model onto a QPainter — used by both thumbnail and preview.
    _generate_collision_thumbnail(model, width, height, yaw, pitch)  — Generate a small QPixmap thumbnail of a COL model.
    _render_collision_preview(model, width, height, yaw, pitch, flip_h, flip_v)  — Render a full-size QPixmap preview of a COL model.
    _on_collision_selected() #vers 8 — Handle [T] detail table selection.
    _select_model_by_row(row) #vers 3 — Load model by row index into preview — COL mode only.
    _show_collision_context_menu(position) #vers 5 — Right-click on mod_compact_list — DFF material/texture mode or COL mode.
    _show_dff_material_context_menu(position, source_list, row) #vers 1 — DFF mode right-click — texture and material channel operations.
    _rename_col_model(model, row) #vers 1 — Rename a collision model entry in the list.
    _export_col_model(model, row) #vers 1 — Export a single collision model as a standalone COL file.
    _import_replace_col_model(row) #vers 1 — Replace a collision model entry from an external COL file.
    _show_model_details(model, index) #vers 1 — Show detailed model information dialog
    _copy_model_info(model, index) #vers 1 — Copy model info to clipboard
    _copy_text_to_clipboard(text) #vers 1 — Copy text to system clipboard
    _populate_collision_list() #vers 7 — Populate [T] detail table — 8 columns, icon badges on counts > 0.
    _create_preview_widget(level_data) #vers 3 — Create preview widget - large collision preview like TXD Workshop
    _toggle_spheres(checked) #vers 3 — Toggle sphere visibility
    _toggle_boxes(checked) #vers 3 — Toggle box visibility
    _toggle_mesh(checked) #vers 3 — Toggle mesh visibility
    _setup_hotkeys() #vers 3 — Setup Plasma6-style keyboard shortcuts for this application - checks for existin
    _reset_hotkeys_to_defaults(parent_dialog) #vers 1 — Reset all hotkeys to Plasma6 defaults
    _apply_hotkey_settings(dialog, close) #vers 1 — Apply hotkey changes
    _show_settings_hotkeys() #vers 1 — Show settings dialog with hotkey customization
    _show_col_info() #vers 4 — Show TXD Workshop information dialog - About and capabilities

## ZoomablePreview
#    Line 13304

    __init__(parent) #vers 1
    setPixmap(pixmap) #vers 2 — Set pixmap and update display
    set_model(model) #vers 1 — Set collision model to display
    render_collision() #vers 2 — Render the collision model with current view settings
    _update_scaled_pixmap()  — Update scaled pixmap based on zoom
    paintEvent(event) #vers 2 — Paint the preview with background and image
    set_checkerboard_background() #vers 1 — Enable checkerboard background
    set_background_color(color) #vers 1 — Set solid background color
    _draw_checkerboard(painter) #vers 1 — Draw checkerboard background pattern
    zoom_in() #vers 1 — Zoom in
    zoom_out() #vers 1 — Zoom out
    reset_view() #vers 1 — Reset to default view
    fit_to_window() #vers 2 — Fit image to window size
    pan(dx, dy) #vers 1 — Pan the view by dx, dy pixels
    rotate_x(degrees) #vers 1 — Rotate around X axis
    rotate_y(degrees) #vers 1 — Rotate around Y axis
    rotate_z(degrees) #vers 1 — Rotate around Z axis
    mousePressEvent(event) #vers 1 — Handle mouse press
    mouseMoveEvent(event) #vers 1 — Handle mouse drag
    mouseReleaseEvent(event) #vers 1 — Handle mouse release
    wheelEvent(event) #vers 1 — Handle mouse wheel for zoom

## COLEditorDialog
#    Line 13569

    __init__(parent) #vers 1
    setup_ui() #vers 1 — Setup editor UI
    connect_signals() #vers 1 — Connect UI signals
    load_col_file(file_path) #vers 2 — Load COL file - ENHANCED VERSION
    open_file() #vers 1 — Open file dialog
    save_file() #vers 1 — Save current file
    save_file_as() #vers 1 — Save file as dialog
    analyze_file() #vers 1 — Analyze current COL file
    on_model_selected(model_index) #vers 1 — Handle model selection
    _create_viewport_controls() #vers 1
    _set_camera_view(view_type) #vers 1 — Set predefined camera view
    _svg_to_icon(svg_data, size) #vers 1 — Convert SVG to QIcon
    on_property_changed(property_name, new_value) #vers 2 — Handle property changes from properties widget
    _set_camera_view(view_type) #vers 1 — Set predefined camera view
    closeEvent(event) #vers 1 — Handle close event
    _add_import_export_functionality() #vers 1 — Add import/export functionality when docked to img factory
    _import_col_data() #vers 2 — Import one or more COL models from .col file(s) into the current archive.

## _FaceAdapter
#    Line 97

    __init__(tri) 

## _PosPicker
#    Line 5530

    __init__() 
    _az_el_to_xy(az, el) 
    _xy_to_az_el(x, y) 
    paintEvent(ev) 
    _drag(x, y) 
    mousePressEvent(ev) 
    mouseMoveEvent(ev) 

## _Filter
#    Line 6493

    eventFilter(obj, ev) 

## _GuiLayout
#    Line 6830

    __init__(table) 