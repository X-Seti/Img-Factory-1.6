#this belongs in apps/components/Model_Editor/MODEL_METHODS.md - Version: 2
# X-Seti - Apr 2026 - Model Workshop method index

##Key methods (model_workshop.py) -
# _apply_prelighting          STUB: bake ambient+directional into DFF vertex colours
# _auto_load_from_texlist     scan texlist/ folder for pre-exported textures
# _auto_load_txd_from_imgs    search open IMG tabs for IDE-linked TXD
# _browse_texlist_folder      open texlist/ browser dialog
# _build_primitive            generate vertices+triangles for Box/Sphere/Cylinder/Plane
# _compute_face_shade         Lambertian per-face shade factor (ambient + diffuse) #vers 2
# _create_col_from_dff        generate COL1/2/3 binary from DFF geometry #vers 1
# _create_primitive_dialog    dialog to add Box/Sphere/Cylinder/Plane to DFF #vers 1
# _display_dff_model          populate model list + 3D viewport from parsed DFF #vers 3
# _enable_dff_toolbar         show/hide DFF-only toolbar buttons #vers 2
# _export_dff_obj             export DFF to Wavefront OBJ + MTL #vers 1
# _find_in_ide                look up model in DAT Browser IDE entries
# _hide_tex_hover             close texture hover popup #vers 1
# _load_txd_file              load TXD file → texture panel + viewport cache
# _load_txd_file_from_data    load TXD from raw bytes
# _load_viewport_light_settings  restore saved light from model_workshop.json #vers 1
# _lookup_ide_for_dff         find IDE entry via xref or IDEDatabase #vers 2
# _on_dff_geom_selected_tbl   handle model table row click → show geometry #vers 1
# _open_dff_material_list     unified Material Editor (3ds Max style) #vers 5
# _open_light_setup_dialog    hemisphere position picker + brightness sliders #vers 2
# _open_linked_txd            open IDE-linked TXD in TXD Workshop
# _open_model_workshop        open from main window / IMG entry
# _open_paint_editor          open paint mode for face surface editing #vers 5
# _open_txd_combined          smart DFF+TXD load (DB→IMG→browse)
# _populate_tex_thumbnails    64×64 thumbnail grid in texture panel #vers 1
# _populate_texture_list      fill texture panel table from _mod_textures
# _prelight_setup_dialog      light source setup for prelighting STUB #vers 1
# _rebuild_grid               rebuild material editor slot grid on column change
# _refresh_icons              refresh all SVG icons after theme change
# _save_textures_as_txd       save current textures as new TXD file
# _set_texlist_folder         set texlist/ folder via dialog
# _show_dff_geometry          push _DFFGeometryAdapter into COL3DViewport #vers 1
# _show_tex_hover             hover texture preview popup #vers 1
# _toggle_tex_view            switch texture panel list/thumbnail view #vers 1
# _toggle_viewport_shading    toggle Lambertian shading on/off #vers 1
# _update_tex_btn_compact     icon-only when texture panel narrow #vers 1
# apply_changes               STUB: commit pending edits to DFF/COL data
# export_model                STUB: write DFF to file
# import_elements             STUB: import OBJ/FBX geometry into DFF
# open_dff_file               parse and display a DFF file #vers 2
# open_model_workshop         factory method — open workshop with optional DFF #vers 3

##Classes -
# _DFFGeometryAdapter   adapts DFF Geometry for COL3DViewport
# COL3DViewport         2D projected 3D viewport (wireframe/solid/semi/textured)
# ModelListWidget       enhanced QListWidget for model entries
# ModelWorkshop         main workshop widget (DFF + COL + TXD)
# ZoomablePreview       zoom/pan preview label
