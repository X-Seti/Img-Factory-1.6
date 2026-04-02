#this belongs in root /ChangeLog.md - Version: 32

## March–April 2026 — TXD GTA3/VC fixes, COL ops, log dedup

### Build 180 — Fix PAL8/PAL4 pixel data offset (4-byte size prefix)
- **Root cause**: GTA3/VC TXD PAL8 layout is `palette(1024 raw) + pixel_size(4) + pixels(w*h)`
- DragonFF `read_palette()` reads palette raw with no prefix; `read_pixels()` always reads a 4-byte size prefix first
- Our code set `has_data_size_field=False` for GTA3/VC non-DXT, so `expected = 1024 + w*h` — no room for the pixel prefix
- `pix_data = level_data[1024:]` landed on the size field bytes `[0x00,0x10,0x00,0x00]`, treating them as pixel indices 0,16,0,0 — shifting every row by 4 pixels
- Fix: `expected = pal_size + 4 + pixel_data_size`; `pix_data = level_data[pal_size + 4:]`
- Verified against all 110 PAL8 textures in `generic.txd` — every one has `prefix == w*h`

### Build 179 — Align PAL4/PAL8 alpha with DragonFF txd.py
- **PAL4 palette size**: DragonFF `read_palette()` uses 64 bytes when `depth==4`, 128 bytes otherwise — we always used 64
- **PAL8/PAL4 alpha forcing**: DragonFF calls `pal8_noalpha()` (force alpha=255) when `has_alpha()==False`, which triggers for `RASTER_888`, `RASTER_565`, `RASTER_555`, `RASTER_LUM` pixel types (bits 8–11 of `raster_format_flags`)
- Old code only forced opaque for `palette_entry_fmt=='RGB888'`; now checks `raster_format_flags & 0x0F00` against no-alpha type set `{0x0600, 0x0200, 0x0A00, 0x0400}`
- Generic.txd `carlites64` has `raster_flags=0x2600` → pixel_type `0x0600` (RGB888) → `force_opaque=True` ✓

### Build 178 — Fix GTA3/VC PAL8+PAL4 wrong colours (BGRA vs RGBA)
- **Root cause confirmed** via `generic.txd` `carlites64`: GTA3/VC TXDs store palette entries as **RGBA**, not BGRA
- SA TXDs (`>= 0x1803FFFF`) store palettes as **BGRA** — our pre-existing B↔R swap was correct for SA only
- `carlites64` palette[0] stored bytes: `48,24,8,255`
  - Before fix: output `R=8, G=24, B=48` → cold blue (wrong)
  - After fix:  output `R=48, G=24, B=8` → warm brown (correct)
- Fix: `tex['palette_is_bgra'] = is_sa_plus` during header parse; `_decompress_uncompressed` v7 adds `palette_is_bgra` param and branches on it for both PAL8 and PAL4 decode paths
- DragonFF never swaps (outputs BGRA to Blender which handles it); our fix is more precise

### Build 177 — Fix duplicate timestamps in activity log
- `img_debugger._write()` formats lines as `[HH:MM:SS] LEVEL message` then calls `log_message()`
- `gui_layout.log_message()` prepended another `[HH:MM:SS]` unconditionally → double-stamped output
- Fix: `re.match(r'^\[\d{2}:\d{2}:\d{2}\]', message)` — skip adding timestamp if already present

### Build 176 — ChangeLog v31
- Documented Builds 162–175

## March–April 2026 — COL parser overhaul, material paint, writer, IDE ops

### Build 175 — Select All, Invert, Sort, Pin, IDE ops, multi-delete
- **Ctrl+A**: Select All entries in active list
- **Ctrl+I**: Invert selection (QItemSelection toggle)
- **Sort menu** (right-click → Sort…): by name A→Z, version, faces/boxes/spheres/vertices descending
- **Pin / protect** (right-click → 📌 Pin): marks models as edit-locked, tracked in `_pinned_models` set
- **IDE Operations** submenu (right-click):
  - Import matched by IDE — parses `objs/tobj/anim` sections, highlights matching models
  - Export matched by IDE — writes IDE-referenced models to new .col archive
  - Remove unreferenced by IDE — deletes models not in IDE (archive cleanup)
- **Multi-delete** (`_delete_selected_model` v2): uses `currentRow()` + `selectedRows()` on visible list, deletes all selected from highest index down

### Build 174 — Fix selection, multi-select, IMG Factory COL fallback
- `_get_selected_model` v4: uses `currentRow()` (reliable with custom delegates) on **visible** list only; hidden list may have stale state
- Both `collision_list` and `col_compact_list` changed to `ExtendedSelection` — Ctrl+click, Shift+click now work
- Export reads all `selectedRows()` + `currentRow()` from visible list
- `_open_col_entry_smart()` in `imgfactory.py`: double-clicking `.col` in IMG file list tries COL Workshop first; falls back to inline QDialog showing model table (name, version, sphere/box/vertex/face/shadow counts) if COL Workshop not installed

### Build 173 — COL binary writer + working Import/Extract buttons
- **New file**: `apps/methods/col_workshop_writer.py` — serialises `COLModel` → binary .col
  - `model_to_bytes()`, `models_to_bytes()`, `save_col_file()`
  - Mirrors DragonFF `__write_col/__write_col_new` exactly
  - COL1: sequential count+data blocks with skip-4 between spheres and boxes
  - COL2/3: 36-byte `<HHHBxIIIIIII>` header, data blocks without count prefix, int16 fixed-point vertex compression, offsets as `(file_pos - 4) - start_offset`
  - Round-trip verified: 154-byte COL2 blob writes bit-identically
- **Import button** (`_import_col_data` v2): open .col files, append all models to current archive
- **Extract button** (`_export_col_data` v2): single → SaveDialog; multi/none → FolderDialog, each model as `modelname.col` with `_N` suffix for duplicates

### Build 172 — Fix COL2/3 multi-model archive advance
- `parse_model` was returning mid-parse offset for COL2/3 (data blocks read by jumping, not linearly)
- Loader used this wrong offset and jumped into the middle of the next model's data
- Fix: return `start_offset + header.size + 8` — the header-declared next model position (DragonFF: `pos + file_size + 8`)

### Build 171 — Fix COL2/3 SA parsing — 4 bugs from DragonFF reference
All SA COL2/3 files were silently failing. Root causes vs DragonFF `__read_new_col`:
1. **Wrong block_base**: was `start_offset + 8`; correct is `start_offset` (fourcc position). Stored offsets are `(file_pos - 4) - start_offset`, `data_at(off) = start_offset + off + 4`
2. **Missing flags field**: was reading `<HHHH>` (4 uint16) then 9 offsets; correct is `<HHHBxIIIIIII>` — sphere/box/face counts + line_count(B) + pad + flags(I) + 6 offsets
3. **Wrong offset order**: had `cones_off` between boxes and verts; DragonFF order is `spheres_off, boxes_off, lines_off, verts_off, faces_off, tri_off`
4. **Wrong vertex count method**: used `(faces_off - verts_off) / 6`; DragonFF derives from face indices: `max(a, b, c) + 1` across all faces

### Build 170 — Fix paint selection + cursor + drag-select (3 issues)
- `_get_selected_model` (initial attempt): tries both list widgets using row as index
- Default cursor changed from `OpenHandCursor` to `ArrowCursor` so pointer tip is precise during face selection
- Pan retains `ClosedHandCursor`; paint mode uses `CrossCursor`
- `_drag_selecting` flag: LMB held after clicking a face → `_pick_face()` called on every `mouseMoveEvent` → brush-style add to selection; in paint mode also paints each passed face

### Build 169 — Fix `_get_selected_model` wrong list lookup
- `col_compact_list` stores no `UserRole` — row index IS model index
- `collision_list` stores `UserRole` on col 0, not col 1
- Both paths now use row number directly, matching `_select_model_by_row`

### Build 168 — COL viewport face picking + material paint mode
- `_pick_face()`: centroid-based hit test, 20px pick radius; face loop uses `enumerate`
- Click face → selects it (yellow highlight); Ctrl+click multi-select; Escape deselects
- Material paint mode (`_open_paint_editor`): searchable picker of all 179 SA / 35 VC materials with group colour swatches; enter paint mode → crosshair cursor, HUD banner shows current material + `[Esc to exit]`; click/drag faces to paint; Escape exits; paint button shows 🔴 Exit Paint while active
- Viewport material colours: upgraded from 14-entry hardcoded dict to full `col_materials.py` group palette; auto-detects SA vs VC from COL version

### Build 167 — Rewrite `col_workshop_loader.py` — 6 bugs fixed
1. `_load_multi_model_archive()`: dead code referencing undefined `signatures` variable — removed
2. `is_multi_model()`: called `is_multi_model_archive()` as unbound function with no args — now `len(models) > 1`
3. Three divergent load paths (`load`, `load_from_file`, `load_from_data`) — all now funnel through `_load_bytes()` → `_parse_all_models()`
4. Missing `import struct` at module level
5. `get_info()` / `validate()`: inconsistent `model.name` access — unified to shortcut
6. `_parse_all_models` offset advance: was `max(parsed, next)` risking model skips — uses `parsed_end` when it advances, `next_offset` as fallback

### Build 166 — Fix COL1 parse order: skip-4 must follow sphere DATA not box data
- Build 163 moved the skip-4 to the right concept but wrong position — between boxes and vertices
- DragonFF `__read_legacy_col` places skip-4 **after sphere data, before `num_boxes`**
- Correct COL1 order: `num_spheres → spheres[] → skip4 → num_boxes → boxes[] → num_verts → verts[] → num_faces → faces[]`
- Fixed in both `parse_model()` and `parse_col1_model()`
- Verified with synthetic COL1: S=0 B=1 V=3 F=1 parses correctly

### Build 165 — Fix PAL4 nibble order + DXT5 alpha interpolation (TXD)
- **PAL4 decoder** (`_decompress_uncompressed`): nibble order was reversed — low nibble decoded first, swapping every pixel pair. High nibble = first pixel, low = second. Matches DragonFF: `idx1, idx2 = (i >> 4) & 0xf, i & 0xf`. Fixes shifted/scrambled PAL4 textures (GTA III era)
- **DXT5 alpha decoder**: interpolation uses `round()` with fractional weights matching DragonFF `bc3`; both `a0>a1` (6 intermediates) and `a0<=a1` (4 intermediates + 0/255) fixed
- **DXT5 alpha encoder** (`_encode_alpha_block`): was building wrong number of intermediates (5 or 3 instead of 6 or 4) causing encoder/decoder mismatch on exported textures

### Build 164 — Mesh editor: replace stale material dicts with `col_materials.py`
- Remove hardcoded `SURFACE_MATERIALS` (35 wrong entries) and `MATERIAL_COLORS`
- Import `col_materials`: `get_material_name`, `get_materials_for_version`, group colour cache
- Game version selector (SA / VC+GTA3) auto-detected from COL version on open
- `_refresh_material_combo()`: rebuilds Add Face combo (179 SA or 35 VC entries)
- `_on_game_changed()`: switches game, refreshes combo + face table + viewport
- Viewport face colours driven by group colour map (14 groups)

### Build 163 — Fix 4 COL1 parser bugs + add `col_materials.py`
Parser fixes verified against DragonFF:
- COL1 `parse_model`: removed bogus facegroups block, correct skip-4 after spheres
- `parse_boxes_alt`: COL1 box size was 32 bytes, correct is 28 (`VVS` = 12+12+4); surface reading fixed
- `parse_faces_alt`: COL1 faces read as uint16 (wrong), fixed to uint32×3 + surface = 16 bytes (`IIIS`)
- `COLBounds` dataclass: field defaults added so `COLBounds()` works

**New file**: `apps/methods/col_materials.py`
- Full SA material table (179 materials, 0–178 + vehicle presets)
- Full VC/GTA3 material table (35 materials + vehicle presets)
- Group colour map (14 groups with hex colours for viewport)
- API: `get_material_name/colour/group/qcolor`, `get_materials_for_version`, `get_vehicle_presets`, `material_id_from_name`

### Build 162 — Sync repos + ChangeLog v30
- Col-Workshop and Txd-Workshop repos synced to Build 161

### Duplicate log line bug (pending fix)
- `img_debugger._write()` formats `[HH:MM:SS] LEVEL message` then calls `log_message(line)`
- `gui_layout.log_message()` (line 2736) prepends another `[HH:MM:SS]` before appending to QTextEdit
- Results in double-stamped lines: `[18:23:50] [18:23:50] DEBUG Populating...`
- Fix: check if message already starts with `[DD:DD:DD]` before prepending timestamp

## March 2026 — COL Workshop list views, parser fixes, repo sync

### Build 161 — Fix viewport not rendering on model click
- `open_col_file` called `_on_collision_selected()` which read from
  hidden detail table (always empty) — now calls `_select_model_by_row(0)`
- `set_current_model` resets pan/zoom so each model is centred on load

### Build 160 — Three bugs fixed
- Bug 1: `table.setVisible(True)` NoneType crash in colour_ui_for_loaded_img
- Bug 2: `open_col_editor(entry)` — method lacked entry param, fixed
- Bug 3: COL2/3 parser rewritten with correct offset-table layout:
    counts: num_spheres(u16) num_boxes(u16) num_faces(u16) num_lines(u16)
    offsets: 9 x uint32 relative to block_base
    Previously read as COL1 sequential uint32 — completely wrong

### Build 159 — Fix preview not updating on model click (3 compounding bugs)
- Compact list read selection from hidden detail table → empty
- Model index read from col 1 (version string) not col 0 (model index)
- Thumbnail spin set DecorationRole on text cell (no longer a pixmap)
- Fix: unified `_select_model_by_row(row)` for both list handlers

### Build 158 — Remove 200-model hard cap in COL loader
- Hardcoded `if model_count >= 200: break` in col_workshop_loader.py
- generics.col has 1146 models — 946 were silently truncated

### Build 157 — Fix Type vs Version columns (were identical)
- Type = fourcc decoded: COLL / COL2 / COL3 / COL4
- Version = game target: GTA III/VC / SA PS2 / SA PC/Xbox / SA (unused)

### Build 156 — Detail table: SVG icons on non-zero counts
- Spheres/Boxes/Vertices/Faces show 14px SVG icon when count > 0
- Built once per populate from SVGIconFactory, theme-colour aware

### Build 155 — Detail table: 8 proper columns, no thumbnail
- Columns: Model Name, Type, Version, Size, Spheres, Boxes, Vertices, Faces
- Row height 22px, model index in col 0 UserRole

### Build 154 — [=] compact view: COLCompactDelegate
- Custom QStyledItemDelegate draws each row directly
- Name | COL type badge (colour-coded) | 4 inline icon badges
- No thumbnail, single column, 38px row height

### Build 153 — Compact list stats single-row layout
- Name + version left, 4 icon badges right, all on one horizontal row

### Build 152 — Thumbnail right-click: view axis menu
- Both lists: "Thumbnail View" submenu — Top/Front/Side/Iso/Bottom/Back
- _set_thumbnail_view regenerates all thumbs at new angle instantly

### Build 151 — Fix model selection: unified _select_model_by_row
- Both _on_compact_col_selected and _on_collision_selected delegate to it
- Accesses models[] by row index directly, no table dependency

### Build 150 — Debug build (box/sphere counts on select)

### Build 149 — Fix COL Workshop startup crash
- preview_widget not yet created when icon panel connects buttons
- Fix: lambdas so lookup happens at click time

### Build 148 — COL viewport: proper box/sphere/bounds rendering + render modes
- Boxes: 8 corners projected, 12 AABB edges (was invisible 2D rect)
- Spheres: 3 projected rings, 48-segment polylines each
- Bounds AABB: dashed purple, bounding sphere: dotted purple
- Grid extent includes all geometry types
- Render modes: Wireframe/Semi/Solid — V key, right-click, switch btn
- Left nav buttons all wired: flip, rotate, copy, delete, duplicate, create

### Build 147 — Full COL mesh editing
- Click-to-select in viewport (point-in-triangle + 8px vert radius)
- Ctrl+click multi-select, _proj_cache for hit-testing
- Keyboard: Del, Ctrl+A, Ctrl+Z, Ctrl+D
- Flip Face(s): swap B↔C reverses winding
- Select Connected: BFS flood-fill along shared verts
- Merge Close Vertices: weld + remap + remove degenerate faces

### Build 146 — Thumbnail view axis: right-click menu on both lists

### Build 145 — Full COL editing: boxes/spheres/bounds + gizmo fixes
- Gizmo translate/rotate moves ALL geometry
- Mesh Editor: 4 tabs — Mesh, Boxes, Spheres, Bounds
- Recalculate Bounds from geometry button

### Build 144 — ChangeLog v29


## March 2026 — COL Mesh Editor, 3D viewport, gizmo system

### Build 143 — COL3DViewport fully self-contained
- `paintEvent` now draws everything directly using `self._yaw/pitch/zoom/pan`
- No dependency on `_find_workshop()` or `_paint_model_onto` — removes silent
  failure mode where the workshop reference was missing and the view appeared frozen
- Draws grid, mesh, boxes, spheres, gizmo, HUD all in one method
- Right-drag / middle-drag = rotate, left-drag = pan, scroll = zoom now reliable

### Build 142 — Middle mouse rotates scene
- Middle-drag: pan → rotate (matches right-drag)
- Left-drag remains pan only

### Build 141 — Fix zoom not affecting mesh
- `_paint_model_onto` was calling `_draw_col_model()` which recomputes its own
  `scale` from bounds, ignoring `_zoom` — mesh was fixed-size while grid/gizmo zoomed
- Fix: geometry now drawn inline via `to_screen()` (includes zoom + pan)

### Build 140 — Fix COL viewport: full 3D interaction
- Root cause of locked view: `_draw_col_model` used `_project_model_2d` which
  computed its own origin ignoring pan/zoom — grid and geometry in different spaces
- `COL3DViewport` v2 rewritten as fully self-contained:
  `_proj(x,y,z)`, `_get_scale_origin()`, `_to_screen()` — no workshop dependency
- Right-drag: free rotate (yaw + pitch), scroll: zoom, left/middle: pan
- `_paint_model_onto` v4 uses `vp._get_scale_origin()/_proj()` when available

### Build 139 — World-space gizmo at model centre, translate + rotate modes
- Gizmo moved from bottom-left corner to model centroid in 3D world space
- Translate mode (G key): coloured arrows along X/Y/Z, drag to move vertices
- Rotate mode (R key): projected ellipse rings, drag to spin vertices around axis
- Toggle button top-right of viewport, right-click menu, G/R keyboard shortcuts
- Depth-sorted axes (nearer axis draws on top)

### Build 138 — XYZ gizmo + reference grid
- Reference grid: XY plane (Z=0), auto-sized to model extent, behind geometry
- XYZ gizmo (bottom-left corner): red X, green Y, blue Z arrows with arrowheads
- Both in `_paint_model_onto` and `COLMeshEditorViewport`

### Build 137 — COL Mesh Editor with full undo
- New file: `apps/components/Col_Editor/col_mesh_editor.py`
- `COLMeshEditorViewport`: mini 2D preview with selection highlight
- `COLMeshEditor` dialog: face table, vertex table, add/delete/move operations
- Add face: set A/B/C vertex indices + surface material (71 GTA materials)
- Delete face(s): multi-select; delete vertex: remaps face indices automatically
- Remove orphan vertices; local 50-step undo stack; Apply writes to COLFile
- `_push_undo` / `_undo_last_action` v2 on COLWorkshop (deep-copy model)
- Surface Edit button now opens COLMeshEditor (was "coming soon")

### Build 136 — Fix all missing connected methods across codebase
- Full scan: all 221 files clean, no missing methods, no syntax errors
- `imgfactory.py`: `rebuild_img/fast_rebuild/quick_rebuild/safe_rebuild` delegate
  to `apps/core/rebuild.py`; `rebuild_all_img` placeholder; `toggle_debug_mode`
- `gta_file_editors.py` (`IDEFileEditor`): `reload_file/save_file/find_text` real
  implementations; 6 pass stubs
- `Map_Workshop.py` (`MainGUI`): 64 pass stubs for TXD-style methods
- `gui_layout_custom.py`, `gui_menu.py`, `tear_off.py`, `app_settings_system.py`,
  `gui_template.py`: stubs added


## March 2026 — COL Workshop viewport, panel collapse, TXD button layout

### Build 135 — COL Workshop: compact list as default view
- Middle panel now starts in compact thumbnail view (was detail table)
- `_col_view_mode` initialised to `'detail'`, compact list visible, detail table hidden
- `open_col_file` populates both lists on load; selects first row in active view
- `[T]` / `[=]` button toggles between views as before

### Build 134 — COL viewport middle-mouse pan + thumbnail spin + compact settings
- COL3DViewport: middle-mouse drag pans the view on any axis (same as left-drag)
- Thumbnail spin: selecting a model with geometry starts a 20fps QTimer
  rotating its list thumbnail on a random slow axis (yaw 0.8–1.4°/frame,
  pitch rocks ±35°, stops for bounds-only models)
- IMG Factory Settings → Interface tab v2: compact 2–3 items per row
  (H+V spacing on one row; font+size+bold+italic on one row;
  tab height+width on row 1, style+content+position on row 2)

### Build 133 — Fix COL Workshop preview rendering
- COL3DViewport.paintEvent called `_paint_model_onto` which didn't exist
- Added `_paint_model_onto` to COLWorkshop: applies zoom/pan via painter
  transform, filters geometry by visibility flags, draws HUD in screen space
- `preview_widget._workshop_ref = self` set at creation — direct reference
  instead of unreliable parent-chain walk when docked in IMG Factory
- QSlider and Qt added to imgfactory_ui_settings imports (settings crash fix)

### Build 132 — Fix IMG Factory settings dialog crash
- `QSlider` and `Qt` missing from imgfactory_ui_settings.py imports

### Build 131 — Fix TXD Workshop missing method aliases
- 14 aliases + 4 stubs for methods connected but never defined
  (copy/delete/duplicate/paste_texture, export variants, undo, save-as, refresh)

### Build 130 — COL Workshop: restore clean panel methods, proper show/hide collapse
- Restored `_create_transform_icon_panel/text_panel/_create_right_panel`
  from Build 116 (were broken by earlier refactoring)
- Bottom panel: build both text-row and icon-row at startup, show/hide on resize
  (no more `_create_merged_icons_line` / `_rebuild_bottom_panel` that crashed)
- Side panels: `_transform_icon_panel_ref` stored, starts hidden; text panel visible
- `_update_transform_text_panel_visibility` v2: reads threshold from settings,
  toggles all four containers (side icon, side text, bottom text, bottom icon)

### Build 129 — Panel collapse threshold settings slider + both workshops
- IMG Factory Settings → Interface tab: Workshop Panel Collapse group
  with slider 200–900px (default 550), saved to `panel_collapse_threshold`
- `get_collapse_threshold(main_window)` helper in imgfactory_ui_settings
- Both TXD and COL workshops read threshold from settings
- TXD: collapsing also triggers `_update_all_buttons()` for bottom buttons
- COL: bottom buttons rebuilt as icon row when narrow

### Build 128 — TXD panel collapse threshold 550px
- Text panel only shows when right panel ≥ 550px (was 230px — too tight)
- Preview gets ~90% of space in normal use

### Build 127 — TXD Workshop buttons: centred text, icon-only collapse
- Button text centred (removed left-align override)
- Text panel widened to 170px; collapse threshold 230px
- Visibility method v3: measures `_right_panel_ref.width()` directly

### Build 126 — Fix filter_icon in SVGIconFactory
- `filter_icon` used `_svg_to_icon` (doesn't exist); changed to `_create_icon`

### Build 125 — Add filter_icon to SVGIconFactory
- New `filter_icon` (sliders/EQ icon) for Filters button in TXD text panel

### Build 124 — Fix COL Workshop load crash (QWidget viewport)
- COL3DViewport was still the old QLabel version — replaced with QWidget
  implementing zoom_in/zoom_out/fit_to_window/pan/paintEvent/wheelEvent

### Build 123 — TXD Workshop text panel: icon+text buttons, auto-swap
- Text panel buttons gain icons (left-aligned); never shown simultaneously
  with icon strip — threshold 550px drives which panel is visible
- _create_transform_text_panel v13

### Build 122 — Roll back TXD Workshop to Build 73 + safe splitter collapse
- TXD panels were broken by Build 120; restored to last known-good state
- Re-applied only: splitter ref, splitterMoved signal, collapse methods

### Build 121 — Fix COL Workshop load crash (missing method aliases)
- 25 aliases + 10 stubs for methods connected but never implemented
  (_saveall_file, _save_col_file, _open_col_file, export variants, etc.)

### Build 120 — TXD+COL auto-collapse text panel on splitter drag
- `self._main_splitter` stored; `splitterMoved` connected
- `_on_splitter_moved` → `_update_transform_text_panel_visibility`
- Collapses text panel when right panel < 600px

### Build 119 — COL Workshop: rotate/flip views, proper viewport, all buttons wired
- COL3DViewport replaced with QWidget + paintEvent (zoom/pan/rotate)
- Left-drag=pan, right-drag=free rotate, wheel=zoom, context menu presets
- `_create_transform_icon_panel` v13: all buttons wired at creation
- Bottom panel text+icon buttons connected
- Resize behaviour: text panel hides when workshop width < 700px

### Build 118 — COL Workshop: rotate/flip buttons + lazy thumbnails
- Rotate CW/CCW cycles through XY/XZ/YZ projection planes
- Flip H/V mirror the view
- `_populate_collision_list` v5: >100 models use placeholder thumbnails,
  render on first selection


## March 2026 — COL Workshop rendering, parser RE, routing fixes

### Build 116 — COL Workshop: fix thumbnail/preview renderer + COL routing
- `col_workshop.py`: renderer now supports both `COLVertex.x/y/z` (col_workshop_classes)
  and `COLVertex.position.x` (col_core_classes) — was always hitting AttributeError
- Face drawing: support `a/b/c` fields as well as `vertex_indices` tuple on COLFace
- Box/sphere drawing: dual-attribute support for both class hierarchies
- `open.py` `_load_col_file` v5: all COL open paths now call `open_col_workshop()`
  so `.col` files always open with the full Workshop UI, not a blank table tab

### Build 115 — Fix COL open showing blank tab
- Unified file dialog and `open_col_workshop_docked` now call `open_col_workshop()`
  instead of `_load_col_file_in_new_tab()` — full Workshop UI always shown

### Build 114 — Fix double COL tab + "Send to Col Workshop" context menu
- `_load_col_file_in_new_tab` v4: parse COL first, create exactly ONE tab
- Tab right-click → "Send to Col Workshop" for COL tabs
- Deduplicates: switches to existing workshop tab if already open

### Build 113 — COL parser fully verified against special.col + CollEditor2 RE
- `parse_model` v3: correct interleaved layout
  (n_sph→spheres→n_box→boxes→n_fg→facegroups→n_vert→verts→n_face→faces)
- `parse_spheres` v3: 20 bytes all versions (center+radius+surface+piece+pad)
- `parse_faces` COL1: 16 bytes (uint32 x3 + mat + light + pad)
- Fix Vector3/COLMaterial import; fix COLSphere construction
- All 5 models in special.col parse correctly
- `docs/ref_db/col_format_analysis.md`: complete verified COL1 struct layout

### Build 112 — COL Workshop: use _populate_collision_list + real thumbnails
- `open_col_file` calls `_populate_collision_list()` (with thumbnails)
- Compact list `[T]` view also uses `_generate_collision_thumbnail()`

### Build 111 — COL Workshop: implement thumbnail + preview renderer
- `_generate_collision_thumbnail()`: 64×64 QPainter top-down projection
- `_render_collision_preview()`: full-size render with legend
- `_draw_col_model()`: shared renderer (mesh=green, boxes=yellow, spheres=cyan)
- `COL3DViewport` replaced with working 2D QLabel preview
- `VIEWPORT_AVAILABLE = True`


## March 16, 2026 (3) — PS2/LCS/VCS/iOS formats, COL Workshop integration, theme dialogs

### Build 62 — RW Reference dialog: theme-aware code boxes
**Updated**: apps/gui/gui_layout_custom.py

Colour variables now read from the app theme dict (`_tc`) first, with QPalette only as
last-resort fallback. `<code>` and `<pre>` boxes use `panel_filter` (e.g. `#ffe7e5`)
instead of `QPalette.AlternateBase` which returned dark system colours on custom themes.
Table `<td>` backgrounds use `bg_primary`, even rows use `panel_entries`, headers use
`button_normal`. Added `c_code` variable specifically for inline code boxes. Fonts
improved to Segoe UI/Arial. `apply_dialog_theme()` applied to the dialog frame.

---

### Build 61 — theme_utils.py v2: real theme colour keys
**New/Updated**: apps/core/theme_utils.py

Replaced `_dark_defaults()` (hardcoded `#1e1e1e` etc.) with `_default_colors()` using
the same keys as `app_settings_system.get_theme_colors()`: `bg_primary`, `panel_entries`,
`panel_filter`, `button_normal/hover/pressed`, `table_row_even/odd`, `selection_background`,
`text_primary` etc. `panel_entries` used for list/tree backgrounds, `panel_filter` for
input/filter backgrounds. Dark background in ScanDialog and themed dialogs eliminated.

---

### Build 60 — custom File menu parity + theme-aware dialogs
**Updated**: apps/gui/gui_menu_custom.py, apps/core/scan_img.py, apps/core/theme_utils.py,
apps/core/create.py, apps/core/img_merger.py, apps/core/img_split.py,
apps/core/rebuild_all.py, apps/core/extract.py, apps/core/gui_search.py

Custom hamburger menu (frameless/custom UI mode) was missing "Open COL in COL Workshop"
(`Ctrl+Shift+L`). Added with matching shortcut — both menu systems now have identical
File menu entries.

`apps/core/theme_utils.py` created as shared module with `get_theme_colors(widget)`,
`build_dialog_stylesheet(colors)`, and `apply_dialog_theme(dialog)`. Walks widget parent
chain to find `app_settings.get_theme_colors()`. `apply_dialog_theme(self)` added to:
`ScanResultsDialog`, `RecentScansDialog`, `NewIMGDialog`, `IMGMergeDialog`, `SplitDialog`,
`BatchRebuildDialog`, `ExtractDialog`, `ASearchDialog`.

---

### Build 59 — COL Workshop open integration
**Updated**: apps/components/Col_Editor/col_workshop.py, apps/gui/gui_layout.py,
apps/gui/gui_menu.py, apps/components/Img_Factory/imgfactory.py,
apps/components/Img_Factory/depends/comprehensive.py

Four ways to open a COL file in COL Workshop:

1. **Filelist right-click → "Open in COL Workshop"** — now extracts the actual entry
   bytes from the IMG to a temp file and opens directly (was ignoring the clicked row
   and using dir tree priority).
2. **File menu → "Open COL in COL Workshop..."** `Ctrl+Shift+L` — new item in both
   standard and custom File menus, opens a file dialog for standalone `.col` files.
3. **COL Workshop toolbar → "From IMG" button** — new button that shows a picker dialog
   listing all `.col` entries in the currently loaded IMG; double-click to load.
4. **Col Edit toolbar / taskbar COL button** — `edit_col_file` Priority 2 now extracts
   entry data to temp file instead of passing just the IMG path (browse mode).

Collision list right-click context menu — three TODO stubs implemented:
- **Rename Model** — `QInputDialog`, updates table, marks file modified
- **Export Model as COL** — saves single model as standalone `.col`
- **Replace with COL file** — loads external `.col`, picks model if multiple, swaps in

---

### Build 58 — iOS LCS/VCS LVZ support
**Updated**: apps/core/img_ps2_vcs.py, apps/methods/img_core_classes.py

iOS LVZ uses a completely different two-level DLRW tree vs PS2:
- Outer cells at 0x28: `[L1_blob_offset(4), float_LOD(4)]`
- Level-1 DLRW blocks per grid cell with sub_record_count
- L2 records (0x20 bytes): geometry ptr, sector_count, img_byte_offset, `0x57524C44` sentinel
- 8412 stream entries extracted from `commer.lvz`

`open_ios_lvz()` added. `_open_ps2()` now tries iOS parser first (strict DLRW sentinel),
falls back to PS2 parser. `VERSION_IOS_LVZ = 45` added.

No GAME.DTZ on iOS — uses standard `gta3.dir` + `gta3.img` (VERSION_1, already works).

---

### Build 58b — iOS disc layout, game_trees reference directory
**New**: apps/core/lcs_vcs_disc_layout.py (iOS_LCS entry), apps/game_trees/

`iOS_LCS` added to `DISC_LAYOUTS`: no DTZ, streaming in `Models/`, textures as individual
`.pvr` files in `Textures_PVR/`. `apps/game_trees/` created with:
- `ios_lcs_texture_mapping.csv` — 4549 PVR→TXD name mappings
- `tree_ios_lcs.txt` — full iOS LCS file tree
- `TODO_txd_workshop.md` — TXD Workshop iOS texture features TODO

---

### Build 57 — LCS/VCS disc layout reference
**New**: apps/core/lcs_vcs_disc_layout.py

Disc layout reference for LCS and VCS PS2 with helper functions:
- `find_disc_root(path)` — walks up from any file to find disc root and game type
- `find_game_dtz(disc_root, game)` — locates GAME.DTZ
- `find_cutscene_img(disc_root, game)` — finds CUTS.DIR+IMG (LCS) or MOCAPPS2 (VCS)

Key structural differences encoded:
- VCS: `GAME.DTZ` at disc root; streaming areas in root; `PS2/MOCAPPS2.DIR+IMG`; `.XTX` textures
- LCS: `CHK/PS2/GAME.DTZ`; streaming areas in `MODELS/`; `ANIM/CUTS.DIR+IMG`; `.CHK` textures

`CHK` added to `FileType` enum for LCS PS2 texture format. `_build_mocap_dir()` updated
to find correct companion file per game. `open_dtz()` uses `find_disc_root()` for LCS/VCS detection.

---

### Build 56 — GAME.DTZ v4: all four name sources working
**Updated**: apps/core/img_dtz.py, apps/methods/img_core_classes.py,
apps/components/Img_Factory/imgfactory.py

All four name sources now read directly from the decompressed GAME.DTZ blob:

1. **MDL** — `CBaseModelInfo.m_hashname` (CRC32) resolved via `lcs_vcs_names.py`
2. **TEX** — `CPool<TexListDef>.m_entries[i].m_name` (plain ASCII) at `texListPool` `blob[0x60]`
3. **COL** — `CPool<ColDef>.m_entries[i].m_name` at `colPool` `blob[0x68]`
4. **ANIM** — `CAnimBlock.m_name` array at `blob[0x323DD0]`, 48 bytes/entry, 90 streaming blocks

Source column (col 9) added — visible only for LVZ/DTZ versions, shows companion IMG file
and sector/byte offset. LVZ entries: `AREA.IMG @ 0xOFFSET`. DTZ entries: `GTA3PS2.IMG @ sector N`.

---

### Builds 41–55 — PS2/LCS/VCS format research and UI fixes

**Summary** (see session 5 transcript for full detail):

**UI fixes (Builds 41–45):**
- DAT button `_open_dat_browser` passes correct `target=widget`
- IMG tabs: `QTabWidget > QTabBar::tab` selector fixes tab styling
- DAT bleed-through: `WA_OpaquePaintEvent` removed, `setAutoFillBackground(True)` added
- Per-tool taskbar context menus: DAT/Dir (Show/Close), TXD (Open/Save/Close),
  COL (Open in COL Workshop/Save/Close)
- Filelist right-click: `.col` → "Open in COL Workshop"; `.txd` → "Open in TXD Workshop"

**PS2/LCS/VCS format research (Builds 46–55):**
- `GAME.DTZ`: zlib-compressed blob, magic `GTAG`, contains streaming directory for all models/textures/col/anim
- `apps/core/img_dtz.py` v3: GAME.DTZ parser; 1580 entries (654 mdl + 823 xtx + 16 col2 + 87 anim)
- `apps/core/lcs_vcs_names.py`: CRC32→name lookup, 3658 LCS + 4033 VCS entries from g3DTZ v2.1 (MIT)
- LVZ streaming structure: outer DLRW cells → nested DLRW → `IMG @ sector` offsets; scan stops at `0xAAAAAAAA`; 46 real cells in UNDERG.LVZ
- `VERSION_DTZ_VCS = 46`, `VERSION_DTZ_LCS = 47`, `VERSION_IRX = 48`, `FileType.STRM`
- Build 48: streaming IMG detection — `BEACH.IMG` falsely detected as PS2_V1; companion `.lvz` check added
- Build 49: MALL/COMMER fallback also checks for `.lvz` companion
- Build 50: `open_lvz` IMG byte offset fixed (was storing nested DLRW blob offset, not `DLRW[0x18]`)
- Build 52: scan-based cell parsing (count field was slot count not cell count); stops at `0xAAAAAAAA`
- Build 54: stale `cell_count` variable fixed; `_find_companion` handles `GTA3PS2.IMG ↔ GTA3.DIR` cross-stem
- Build 55: `.strm` extension for LVZ stream entries; `STRM` added to `FileType`


## March 16, 2026 (2) — DLRW parser rewrite, tab status bar, LVZ chain fix

### open_lvz v2 — correct DLRW binary layout

**Updated**: apps/core/img_ps2_vcs.py v10

Previous `open_lvz` read `entry_count` from offset 0x14 (= 3128 for BEACH)
then immediately iterated 3128 * 8 bytes from 0x28 reading random data as
entries — producing rows like "bin_3928 = 2,096,776 MB".

**Correct DLRW layout (researched from BEACH/MALL/MAINLA.LVZ):**
```
[0x00]  "DLRW" magic
[0x04]  0 reserved
[0x08]  total decompressed size
[0x0C]  index table offset  (= end of entry header area)
[0x10]  index table offset  (duplicate)
[0x14]  entry_count         (number of streaming cells)
[0x18]  0 padding
[0x1C]  0 padding
[0x20]  header record: data_area_start(4) + nested_dlrw_area_start(4)
[0x28 + i*8]  cell i: sub_entry_count(4) + nested_dlrw_byte_offset(4)
[index_table] entry_count × 4-byte back-pointers
```

Each cell points to a nested DLRW containing audio/visual stream chunks.
MALL.LVZ now parses to 918 cells, BEACH.LVZ to 3127 cells.

`open_lvz` now returns correct `cell_NNNN` entries with:
- `offset`: byte offset of nested DLRW within decompressed blob
- `size`: approximate byte size (next_cell_offset − this_cell_offset)
- `sub_count`: number of stream chunks within the nested DLRW

---

### LVZ open resolves .img → .lvz companion path

**Updated**: apps/methods/img_core_classes.py

When `VERSION_PS2_LVZ` is opened and `file_path` ends in `.img`, the code now
calls `_find_companion(file_path, '.lvz')` to locate the real archive.
Previously it called `open_lvz(self.file_path)` on the `.IMG` file which
decompresses to nothing useful.

---

### Status bar updates correctly after tab switch

**Updated**: apps/components/Img_Factory/imgfactory.py `_on_tab_changed` v9

`update_img_status` and `selection_status_widget.update_selection` are now
deferred via `QTimer.singleShot(0, ...)` so they run *after* Qt has finished
processing the table population. Previously the status update fired before
the table rows were committed, reading a stale entry count.

IMG branch also skips `_populate_real_img_table` if the table already has
rows (first-visit-only population), preventing redundant reloads on every
tab switch.

TXD Workshop status update also deferred the same way.


---

## March 16, 2026 — Bug fixes: MALL.IMG detection, TXD Workshop, taskbar icons, status bar

### MALL.IMG (VCS PS2 LVZ) now detects correctly

**Updated**: apps/methods/img_core_classes.py, apps/core/img_ps2_vcs.py

**Problem**: MALL.IMG (all-zeros header) fell through to VERSION_1 fallback instead
of VERSION_PS2_LVZ, causing a failed load.

**Root cause 1**: `detect_lvz` used escaped string literals `b'\x78\xda'` which
were being matched as literal text rather than byte values. Fixed to use
`magic[0] == 0x78 and magic[1] in (0xDA, 0x9C, 0x01)`.

**Root cause 2**: `detect_version` never checked for a `.LVZ` companion file before
the standalone V1 fallback. Added `_find_companion(path, '.lvz')` check before
the final size-based V1/V1.5 decision — if a valid LVZ companion exists the file
is detected as VERSION_PS2_LVZ.

---

### TXD Workshop opens without requiring a selection

**Updated**: apps/gui/gui_layout.py `edit_txd_file` v6

New priority order:
1. Dir tree .txd selection → open that file
2. IMG table .txd entry highlighted → open workshop with that file
3. **No selection** → open workshop in browse mode against current IMG
4. No IMG loaded → open workshop empty (was: "No TXD file selected" error)

---

### Taskbar icons visible at startup

**Updated**: apps/components/Img_Factory/imgfactory.py

`refresh_icons(icon_color)` is now called immediately after `_create_toolbar()`
at startup, using `text_primary` from the active theme. Fixes black-on-dark
invisible icons caused by the hardcoded `"#000000"` default colour that was
baked in during toolbar construction.

---

### Status bar updates correctly on tab switch

**Updated**: apps/components/Img_Factory/imgfactory.py `_on_tab_changed` v8,
apps/gui/status_bar.py `update_img_status` v2

**Fixes:**
- `update_img_status` now called *after* `_populate_real_img_table` so entry
  count is accurate (was called before, getting a stale zero)
- Selection count widget (`"N of M selected"`) updated in every branch of
  `_on_tab_changed` (IMG, COL, TXD workshop, empty tab)
- COL branch also resets selection to 0/0 on switch
- Empty/unknown tab branch resets both status bar and selection count

### Status bar version labels expanded

**Updated**: apps/gui/status_bar.py

`_vmap` in `IMGStatusWidget.update_img_status` now covers all 18 `VERSION_`
enum values with human-readable names, e.g.:
- VERSION_PS2_LVZ → "PS2 LVZ/DLRW"
- VERSION_XBOX → "Xbox (LZO)"
- VERSION_SA_ANDROID → "Android SA"

Previously only V1, V1.5, V2, SOL were named; everything else showed the raw
enum name.


---

## March 16, 2026 — VCS PC format research, XTX texture decoder, LVZ/DLRW analysis

### XTX Texture Format — Fully Decoded

**New file**: apps/methods/xtx_reader.py  
**Updated**: apps/components/Txd_Editor/txd_workshop.py

VCS PS2/PC splash screen textures (.XTX) researched from binary analysis of
actual VCS PC files (LOADSC*.XTX, SPLASH*.XTX, SCEE*.XTX, MEMCARD.XTX).

**Format discovered:**
- Magic: `"xet\x00"` (bytes 0x78 0x65 0x74 0x00)
- Header: 1228 bytes total
- CLUT (256-entry RGBA palette): at offset 0xCC (204)
- Pixel data (1 byte/pixel, palette-indexed): at offset 0x4CC (1228)
- Dimensions derived from formula: `file_size = width × height + 1228`
  - 512×512 → 263,372 bytes (LOADSC*, SCEE* logos)
  - 512×256 → 132,300 bytes (MEMCARD, SPLASH1)
  - 256×256 → 66,764 bytes (SPLASH2–10)
- Pixel format field [20:24] = 7 (PS2 PSMT8 — 8-bit indexed)
- 0xCC fill padding (PS2 SDK uninitialised memory marker)
- Alpha: PS2 range 0–128, scale ×2 for standard 0–255
- No pixel tile-swizzle (linear row-major)
- Colour order: RGBA (standard)

**TXD Workshop changes:**
- Open file filter now includes `*.xtx`
- `.xtx` files routed to `_open_xtx_file()` before TXD parser
- `_open_xtx_file()`: reads XTX, decodes to QImage, shows in workshop
- `_display_xtx_texture()`: populates texture list item, preview, info labels
- Title bar shows: `[XTX — VCS PS2 Palettized 512×512]`

**xtx_reader.py exports:**
- `is_xtx(path)` — magic check
- `read_xtx(path)` → dict with clut, pixels, rgba_data, width, height
- `xtx_dimensions(file_size)` → (width, height)
- `xtx_to_qimage(path)` → QImage (RGBA8888)
- `xtx_to_qpixmap(path, max_size)` → scaled QPixmap

---

### VCS PC Streaming Format Research

Analysed BEACH.IMG, MAINLA.IMG, MALL.IMG and their companion .LVZ files.

**BEACH/MAINLA/MALL.IMG:**
- Custom VCS PC streaming format, magic 0x54 at offset 0
- NOT a standard GTA IMG — does not contain a readable directory
- Companion .LVZ is the compressed index

**BEACH/MAINLA/MALL.LVZ:**
- zlib-compressed (0x78DA deflate header)
- Decompresses to DLRW format
- DLRW structure: `magic(4) "DLRW"` + 8-byte (offset, size) entry pairs
- Offsets point within the decompressed blob itself
- The blob IS the streaming data; the .IMG contains bulk asset data
- Beach: 5.1 MB decompressed → 219 MB .IMG (44× larger)
- STATUS: Read-only detection added; full DLRW index parsing TODO

**VCS_FILES_OF_INTEREST:**
- `.IRX` = PS2 IOP processor MIPS ELF modules (system files, not GTA archives)
- `IOPRP300.IMG` = PS2 IOP kernel replacement image (NOT a GTA IMG)

---

### RW Version Labels Added

apps/methods/rw_versions.py:
- `0x35002000` → `"3.5.0.2 (VCS PS2/PSP)"`
- `0x35001000` → `"3.5.0.1 (LCS PS2/PSP)"`


---

## March 15, 2026 (final) — RW Reference rewrite, ChangeLog v22

### RW Reference Dialog — Complete Rewrite (v3)

**Updated**: apps/gui/gui_layout_custom.py `_show_rw_reference` #vers 3

Expanded from 5 tabs to 7 tabs, replacing sparse placeholder content with
exhaustive human-readable documentation of every format researched during
IMG Factory development. All content is theme-aware (QPalette colours, no
hardcoded hex values).

**New tabs:**
- **IMG Formats** — binary layouts for every supported format with byte offsets,
  field names, and "how we derived this" annotations (e.g. the GTA3 PS2 hex dump
  analysis that revealed the 12-byte PS2_V1 format)
- **RW Versions** — all three encoding styles (plain-int / packed 0xFFFF /
  non-standard), full version table, RW chunk header layout, how version
  scanning works in IMG Factory (multi-offset probe at +0/+4/+8)
- **TXD / Textures** — RwTexDictionary structure tree, D3D9 native header offsets,
  platform IDs (PS2=4, Xbox=5, iOS/PVRTC=8, Android/ETC1=9), mobile TXD notes
- **COL Collision** — COLL/COL2/COL3/COL4 magic bytes, COL1 header layout, SA
  extension summary, COL Workshop notes
- **DAT / IDE / IPL** — section keywords for IDE (objs/tobj/cars/peds/txdp/2dfx)
  and IPL (inst/cull/zone/pick/path), key .dat files, DAT Browser integration
- **Platform Matrix** — full table of all 18 VERSION_ enums with container type,
  sector size, entry format, and current support status; disambiguation table for
  PS2_V1 vs Bully vs PS2_VCS; false-positive case studies; rebuild logic diagrams
- **Troubleshooting** — "file opens as wrong format", "RW version shows Unknown",
  "export produces wrong content", "rebuild makes file larger", LCS iOS streaming
  segments, Ctrl+Up/Down reordering, IMG Factory component version matrix


---

## March 15, 2026 (evening) — RW version display, move entries, detection fixes

### RW Version Column Fixed

**Updated**: apps/methods/rw_versions.py, apps/methods/populate_img_table.py,
apps/methods/img_core_classes.py

**Problem**: TXD/DFF files from Bully PS2 showed "Unknown" in the RW Version column.

**Root causes (3):**

1. `is_valid_rw_version()` v5 didn't recognise `0x1C02000A` (Bully) or PS2 plain-int
   versions (`0x310` etc). Fixed in v6: adds Bully entries, PS2 plain-ints, and a
   fallback that checks `get_rw_version_name()` — so any future named version
   automatically validates without updating this function.

2. `_is_valid_rw_version()` in `img_core_classes.py` was a local duplicate that also
   missed Bully. Fixed to delegate to the canonical `rw_versions.is_valid_rw_version()`.

3. `get_rw_version_light()` v5 only read from `entry._cached_data` (often empty for
   lazy-loaded entries). Fixed in v6: when no cached data, reads the first 16 bytes
   directly from the IMG file (resolving `.dir` → `.img` via `_find_companion`).

---

### Move Entries Up / Down

**New**: `_move_entries()`, `_move_entries_up()`, `_move_entries_down()` in
apps/components/Img_Factory/imgfactory.py

**Keyboard**: `Ctrl+Up` / `Ctrl+Down` in the file list window.

**Context menu**: Right-click → "Move Up" / "Move Down" (appears when entries selected).

Moves selected entries one position in `img_file.entries` list, refreshes the table,
and restores the selection on the new positions. Aborts silently if any selected row
would go out of bounds. Multi-selection moves all selected rows together.

---

### Bully PS2 Format — open_bully v2

**Updated**: apps/core/img_ps2_vcs.py v8

`open_bully()` updated with correct format understanding:
- Bully WORLD.IMG / GTA3.IMG: standard V1 DIR+IMG pair — handled by `_open_version_1`
- CUTS.IMG: count[4] + N*64-byte name-only entries (no offsets) — handled here
- Added sanity check on count (1–10000), data_start approximation, and note about
  HXD block scanning needed for accurate sizes.

---


---

## March 15, 2026 (late) — PS2 format deep-dive, case-sensitivity fix, Bully analysis

### Critical Fix: Case-Sensitive .dir/.img Extension on Linux

**Updated**: apps/methods/img_core_classes.py

Root cause of GTA3 PS2 (LC) showing garbage entries: opening `GTA3.IMG` directly on
Linux checked for `GTA3.dir` (lowercase) which doesn't exist — the companion is `GTA3.DIR`.
`os.path.exists()` is case-sensitive on Linux. The check failed silently, fell through
all format checks, and `detect_ps2_v1` matched the raw TXD data at offset 0 as a PS2_V1
directory — producing 938 garbage "entries" with Korean/binary decoded names.

**Fix**: Added `_find_companion(base_path, ext)` helper that tries lowercase, uppercase,
then does a case-insensitive directory scan as last resort. Applied to every `.dir`/`.img`
companion lookup throughout `img_core_classes.py`, `img_shared_operations.py`, and
`rebuild.py`.

---

### Bully PS2 Format (WORLD.IMG / GTA3.IMG)

**Analysis of actual Bully PS2 binary** (WORLD.DIR + WORLD.IMG, 275 MB):

Bully Canis Canem Edit PS2 uses **standard V1 DIR+IMG format** — identical to GTA3/VC PC:
- `.DIR`: 32-byte entries (`offset[4] size[4] name[24]`)
- `.IMG`: 2048-byte sectors, data starts at sector 0
- 10,680 entries (WORLD.IMG), named `ASY_Building.txd`, `ASY_Lobby.dff` etc.
- RW version: `0x1C02000A` (Bully-specific, added to rw_versions.py)

Previous `VERSION_BULLY` (value 44) was based on incorrect format assumptions
(64-byte name-only entries with no offsets). The actual files load correctly as
`VERSION_1` once `_find_companion` handles the uppercase `.DIR` extension on Linux.

`rw_versions.py` updated with Bully entries:
- `0x1C02000A` → `"3.7.0.2-Bully (PS2/PC)"`
- `0x1C020085` → `"3.7.0.2-Bully variant"`

---

### PS2 Detection Fixes (continued from previous entries)

`detect_ps2_v1` v3 — confirmed working on actual GTA3 PS2 (LC) files:
- Rejects files that have a `.DIR` companion (handled by `_find_companion` → VERSION_1)
- Correctly handles standalone PS2_V1 files (12-byte entries, no count header)

`detect_bully` v2 — Bully files don't need special detection:
- Bully WORLD.IMG has a `.DIR` companion → detected as VERSION_1 (correct)
- `VERSION_BULLY` / `open_bully` retained for CUTS.IMG variant (different format)

---

### Outstanding PS2 Work

- LCS PS2 / VCS PS2: `detect_ps2_vcs` logic to be verified with actual files
- `VERSION_BULLY` open_bully: may need revision if CUTS.IMG format differs from assumptions


---

## March 15, 2026 (evening) — Export fix, Rebuild rewrite, PS2 detection fixes

### Export Bug Fixed — entries exported individually

**Updated**: apps/methods/img_export_entry.py v3, apps/methods/img_export_functions.py v2

**Bug**: Exporting 7 selected entries produced 7 files but with wrong content — all reading
from `img_archive.file_path` directly. For DIR+IMG pairs (V1) this opened the `.dir` file
and read raw directory bytes as entry data.

**Fix**: `export_entry()` now calls `img_archive.read_entry_data(entry)` which correctly
resolves `.dir` → `.img` path, handles Xbox LZO decompression, and covers all format variants.

**Fix**: `_get_selected_img_entries()` now uses `get_active_table()` (active tab's table,
not the shared `gui_layout.table`) and maps by entry name instead of row index, so sorting
and filtering don't cause row/index mismatches.

---

### Rebuild Size Growth Fixed — V1 DIR+IMG now rebuilt as pair

**Updated**: apps/core/rebuild.py, apps/methods/img_shared_operations.py

**Bug**: Rebuilding a V1 (DIR+IMG pair) file grew from 104 MB → 312 MB. The rebuild wrote
header + directory + all entry data into a single temp file then replaced the `.dir` file
with it. The `.dir` became a full IMG file (116 MB) while the original `.img` was untouched.

**Fix**: `_perform_native_rebuild` now detects V1/V1.5/SOL formats and routes to
`_rebuild_v1_pair()` which:
1. Writes all entry data sequentially into a `.img` temp file, recording each entry's
   sector offset as it goes
2. Writes a proper `.dir` temp file (32 bytes × N entries, offsets from step 1)
3. Atomically replaces both `.dir` and `.img` files

V2/V3 single-file formats use `_rebuild_single_file()` which writes data first then seeks
back to fill in the directory with correct offsets (no more placeholder pass).

`get_entry_data_safely()` also fixed — now calls `read_entry_data()` first, then falls back
to a corrected direct read that resolves `.dir` → `.img` path.

---

### PS2 IMG Detection Fixes (continued)

**Updated**: apps/core/img_ps2_vcs.py v7

`detect_ps2_v1` v3 — complete rewrite based on actual format understanding:
- Format has NO count header — entries start at byte 0 (sec_off / asset_id / sec_sz)
- Entry count derived from `first_entry.sec_off * 512 / 12`
- Removed `0x16` from RW type rejection list (22 = valid sector offset)
- Cross-validates: asset_id high byte must not be printable ASCII (rules out Bully names)

`detect_bully` v2 — requires first byte of name field (byte 4) to be printable ASCII with
4+ consecutive printable chars; rejects non-ASCII first bytes that PS2_V1 asset_ids produce.

`open_ps2_v1` — removed erroneous `dir_data = dir_data[4:]` offset shift; now correctly
skips null/zero-size padding entries.

---


---

## March 15, 2026 — IMG Format Audit, Folder Scanner, Layout Fixes

### IMG Format Audit — New Version Enums and Detection

**Updated**: apps/methods/img_core_classes.py

Added missing `IMGVersion` enum values covering all known GTA platforms:

- `VERSION_XBOX = 50` — GTA3/VC Xbox, DIR+IMG pair with LZO-compressed entries
- `VERSION_SA_ANDROID = 51` — GTA SA Android, VER2 header + mobile texture DB
- `VERSION_LCS_ANDROID = 52` — LCS Android, VER2 header with 0x1005FFFF TXDs embedded
- `VERSION_LCS_IOS = 53` — LCS iOS, 12-byte entries, 512-byte sectors, `*_pvr.img`
- `VERSION_STREAMING_SEG = 60` — Raw streaming segment (LCS/VCS iOS/PSP), no internal directory

**`detect_version()` v5** — new detection paths:
- `.dir` files: Xbox LZO magic probed before falling back to V1/V1.5
- VER2 files: LCS Android detected by filename (`lcs`/`liberty`); SA Android by mobile DB sibling files (`texdb.dat`, `texdb.toc`, `streaming.dat`)
- `*_pvr.img` files: LCS iOS distinguished from GTA3/VC iOS by filename
- Standalone `.img` with no header and sibling `gta3.img` (VER2) → `VERSION_STREAMING_SEG`

**`detect_img_platform()` v3** — content-based probes:
1. Xbox LZO magic in first entry (95% confidence)
2. Mobile texture DB files alongside (Android, 90% confidence)
3. `*_pvr` suffix → iOS (85% confidence)
4. Filename keywords as fallback

**`open()` dispatch** — new branches for all new versions:
- `VERSION_XBOX` → `_open_xbox()` (reuses V1 dir parser, tags entries as LZO)
- `VERSION_SA_ANDROID` → `_open_version_2()` (same as PC SA, correct sector size)
- `VERSION_LCS_ANDROID` → `_open_lcs()` (VER2 path)
- `VERSION_LCS_IOS` → `_open_lcs()` (12-byte PS2_V1 path)
- `VERSION_STREAMING_SEG` → sets `_streaming_segment_error`, returns `False`

**New methods**:
- `_open_xbox()` v1 — reuses `_open_version_1()`, sets platform=XBOX, marks entries as LZO
- `_open_lcs()` v1 — VER2 for Android, PS2_V1 12-byte for iOS
- `read_entry_data()` v3 — correctly resolves `.img` from `.dir` for all DIR+IMG variants (V1, V1.5, SOL, Xbox)

**Removed**: 368-line duplicate block (second `IMGEntry` class + duplicate `_scan_rw_version`, `detect_img_platform`, `detect_img_platform_inline`, `get_platform_specific_specs`). Python was running the old broken v1 versions of everything because last-definition-wins.

**Added**: `detect_lcs_android()` in `apps/core/img_ps2_vcs.py` — probes VER2 files for 0x1005FFFF TXD entries

---

### LCS iOS Streaming Segment Handling

**Updated**: apps/components/Img_Factory/imgfactory.py

`indust.img`, `suburb.img`, `underg.img`, `commer.img` in LCS iOS `Models/` are **raw streaming segment files** — they contain asset data but have no internal directory. The directory for all segments lives in `gta3.img`. These files cannot be opened as standalone archives.

- Previously: generic "Failed to open IMG file: VERSION_1" error
- Now: clear message — _"X.img is a streaming segment file. Open gta3.img from the same folder instead."_
- `VERSION_STREAMING_SEG` detected when: no magic header, no `.dir`, but sibling `gta3.img` (VER2) exists in same directory

`gta3.img` itself opens correctly as VER2 — the ~497,000 entries are real (LCS streams the whole game from one master archive split across segment files).

---

### Recursive IMG Folder Scanner

**New file**: apps/core/scan_img.py

**File → Scan Folder for IMGs…** (`Ctrl+Shift+F`) — recursively scans any folder for IMG-compatible files.

- Background `ScanThread` walks entire tree, results appear live as found
- Shows: Name, Version (V1/V2/Xbox/SA Android/LCS iOS/etc.), Platform, Size, Path
- Subtle background tint per version family for at-a-glance identification
- Filter bar — narrow by name, version, or platform as you type
- Platform dropdown — PC / Xbox / PS2 / Android / iOS / PSP
- Select by Platform button
- Double-click or **Open Selected** to open files in new tabs
- Warns before opening more than 10 files at once
- `ScanResultsDialog` and `ScanThread` exported for reuse

**Updated**: apps/gui/gui_menu.py — `MenuAction("scan_img_folder", ...)` added to File menu

**Updated**: apps/components/Img_Factory/imgfactory.py — `scan_img_folder()` method wired to menu

---

### Hybrid Load + Scan Folder added to toolbar and Settings

**Updated**: apps/gui/gui_layout.py `_get_img_buttons_data()` v4

- **Hybrid Load** button added to IMG Files toolbar panel
- **Scan Folder** button added to IMG Files toolbar panel
- Both added to `_create_method_mappings()` so they fire correctly

**Updated**: apps/utils/app_settings_system.py `_create_buttons_tab()`

- Hybrid Load and Scan Folder added to **Settings → Buttons → IMG Files Buttons** colour editor

---

### Settings Colors Tab Layout Fix

**Updated**: apps/utils/app_settings_system.py  
**Updated**: App-Settings-System repo (github.com/X-Seti/App-Settings-System)

Three bugs fixed in the Colors tab:

1. **Colour list not stretching** — `right_layout.addWidget(scroll_area)` had no stretch factor. Changed to `addWidget(scroll_area, 1)` so the colour list expands when the window grows; sliders and action buttons stay at natural height.

2. **`QHBoxLayout(self)` corrupting dialog layout** — `theme_layout = QHBoxLayout(self)` passed the dialog as parent, silently replacing its top-level layout. Changed to `QHBoxLayout()`.

3. **Left panel not resizable** — replaced the `QHBoxLayout` containing left/right panels with a `QSplitter(Horizontal)`. Left panel has `min=220, max=400`, right panel gets `setStretchFactor(1, 1)`. User can now drag the divider.

---

### Version Matrix (complete as of March 15, 2026)

| Version | Value | Platform | Format | Status |
|---------|-------|----------|--------|--------|
| VERSION_1 | 1 | PC | DIR+IMG 32-byte entries 2048-byte sectors | ✅ |
| VERSION_1_5 | 15 | PC | DIR+IMG extended >2GB | ✅ |
| VERSION_SOL | 25 | PC | DIR+IMG (SOL) | ✅ |
| VERSION_2 | 2 | PC | VER2 single file | ✅ |
| VERSION_3 | 3 | PC | GTA IV unencrypted | ✅ |
| VERSION_3_ENC | 30 | PC | GTA IV AES-256 ECB | ✅ |
| VERSION_XBOX | 50 | Xbox | DIR+IMG LZO-compressed | ✅ |
| VERSION_SA_ANDROID | 51 | Android | VER2 + mobile texture DB | ✅ |
| VERSION_LCS_ANDROID | 52 | Android | VER2 + 0x1005FFFF TXDs | ✅ |
| VERSION_PS2_VCS | 40 | PS2 | Embedded-dir 512-byte sectors | ✅ |
| VERSION_PS2_LVZ | 41 | PS2 | zlib DLRW streaming | ✅ |
| VERSION_PS2_V1 | 42 | PS2/Android | 12-byte entries 512-byte sectors | ✅ |
| VERSION_1_IOS | 47 | iOS | 12-byte entries *_pvr.img | ✅ |
| VERSION_LCS_IOS | 53 | iOS | 12-byte entries LCS variant | ✅ |
| VERSION_STREAMING_SEG | 60 | iOS/PSP | Raw segment, no directory | ℹ️ (shows info, open gta3.img) |
| VERSION_ANPK | 43 | PSP | Named DGAN clips | ✅ |
| VERSION_BULLY | 44 | PS2 | Named 64-byte entries | ✅ |
| VERSION_HXD | 45 | PS2 | Bone/animation data | ✅ |

---

## March 10, 2026 (evening) — Hybrid Load, COL column, DAT Browser xref improvements

### Hybrid Load

**Added**: File → **Hybrid Load (IMG + COL)...** `Ctrl+Shift+H`

**Updated**: apps/components/Img_Factory/imgfactory.py
- `open_hybrid_load()` v2: opens an IMG file and automatically pairs every DFF entry with its matching COL data before the table renders. COL sources checked in priority order:
  1. COL entries **inside the IMG itself** (GTA III / VC / SA world props)
  2. **Sibling `.col` file** in the same directory (e.g. `game_sa.col` next to `game_sa.img`, `game_vc.col` next to `game_vc.img`)
  3. **`models/coll/`** external category archives for SA/SOL (`vehicles.col`, `peds.col`, `weapons.col`) — sub-model names read directly from COL binary headers
- Game type detected from DAT Browser / `game_root`; SA/SOL external scan only runs when appropriate
- Log summary e.g. `Hybrid Load: game_sa.img  |  2734 DFF entries  |  2190 paired  |  544 no COL  |  312 sibling COL sub-models`
- Pairs stored in `self._pending_hybrid_pairs` before async thread starts; consumed in `_on_img_loaded()` once table is actually populated (fixes timing bug where column was filled before table existed)
- `_on_img_loaded()` v5: checks `_pending_hybrid_pairs` after `_populate_img_table_widget()`; calls `populate_col_column()`, logs match count, clears pending data

**Updated**: apps/gui/gui_menu.py
- `MenuAction("hybrid_load", "Hybrid &Load (IMG + COL)...", "Ctrl+Shift+H")` added to File menu after Open Multiple

### COL column (column 8)

**Updated**: apps/methods/populate_img_table.py
- `setup_table_for_img_data()` v4: column count → 9; added **COL** header at index 8; `setColumnHidden(8, True)` — invisible on normal open, revealed only by hybrid load; resize mode set for all 9 columns
- `populate_table_with_img_data()`: same 9-column setup with COL hidden by default
- **Added `populate_col_column(table, paired)`** v1: iterates all rows; for DFF rows looks up stem in paired list — `✓ stem (source)` in green if matched, `✗ missing` in red if not; non-DFF rows left blank; calls `setColumnHidden(8, False)` to reveal column; sortable so missing entries can be grouped by clicking header

### DAT Browser / xref improvements

**Updated**: apps/methods/gta_dat_parser.py
- `build_xref()` v2: accepts optional `game_root`; for SA/SOL also scans `models/coll/` COL archives and indexes sub-model name stems into `col_stems` — tooltips now correctly show `has col` for vehicle/ped/weapon DFFs whose collision lives in an external archive rather than inside the IMG

**Updated**: apps/components/Dat_Browser/dat_browser.py
- `_on_load_done()`: passes `_thread.game_root` to `build_xref()` so SA/SOL external COL scan runs automatically on DAT load

---

## March 10, 2026 — DAT Browser enhancements, Dir Tree improvements, click-drag multi-select

### DAT Browser

**Updated**: apps/components/Dat_Browser/dat_browser.py
- `_game_combo` v2: added **"Game Root (Dir Tree)"** entry (index 5); combo width increased to 155 px
- `_on_game_combo_changed()` v1: new slot wired to `currentIndexChanged` — selecting "Game Root (Dir Tree)" immediately reads `directory_tree.current_path` (falls back to `main_window.game_root`), fills the path field, detects the game, switches the combo to the detected game entry, and calls `_start_load()` automatically — no Browse or Load click required
- `_start_load()` v4: cleaned up (removed dead index-5 block now handled by `_on_game_combo_changed`)
- `_auto_fill_game_root()` v1: silently pre-fills path and game combo from dir tree on every open/re-open; only fires if path field is currently empty (never overwrites a manually set path)
- `show_dat_browser()` v2: calls `_auto_fill_game_root` on show and on tab re-add
- `integrate_dat_browser()` v4: calls `_auto_fill_game_root` after widget creation
- `_make_table()` v3: switched base class to `DragSelectTableWidget`; added `ExtendedSelection` — all three tables (Objects, Instances, Zones) now support click-drag row selection
- Responsive toolbar v2: `Browse…` / `Load` buttons collapse to 32×32 icon-only squares below 520 px width; icons loaded lazily on first compact transition (folder icon / go-arrow icon); no fixed widths in full mode — Qt sizes naturally
- Load-order tree right-click context menu: **Edit** (opens text editor) for `.ide .ipl .dat .txt .cfg .ini`; **Open in IDE Editor** for `.ide`; **Copy path** for all entries
- `_setup_tree_context_menu()` v1, `_on_tree_context_menu()` v1, `_open_path_in_editor()` v1, `_open_in_ide_editor()` v1

**Updated**: apps/methods/gta_dat_parser.py
- `GTAWorldXRef.tooltip_for()` v2: richer hover bubble — `"Model defined in: vehicles.ide"`, `"Type: Vehicle"`, `"TXD: landstal.txd"`, optional `"COL: landstal.col  [present]"`; TXD-only files list up to 5 model names that share the TXD; COL files confirm COLFILE directive presence

**Updated**: apps/methods/populate_img_table.py
- `apply_xref_tooltips()` v2: sets tooltip on **all columns** of each matching row (not just Name column) so the info bubble appears wherever the cursor lands on the row
- **Added `DragSelectTableWidget`** v1: `QTableWidget` subclass implementing click-hold-drag row selection via `mousePressEvent` / `mouseMoveEvent` / `mouseReleaseEvent` overrides; `DragEnabled=False`, `NoDragDrop`, `ExtendedSelection`, `SelectRows` set by default; Shift+Click and Ctrl+Click still work normally; exported in `__all__`

### Dir Tree

**Updated**: apps/components/File_Editor/directory_tree_browser.py
- `populate_tree_recursive()`: permissions column now shows **`755  rwxr-xr-x`** (octal + symbolic) for both files and folders via new `_perms_str(mode)` inner helper
- `show_context_menu()` v3: **Edit** action for `.ide .ipl .dat .txt .cfg .ini .zon .cut .fxt` files (opens `IMGFactoryTextEditor`); **Open in IDE Editor** action for `.ide` files
- `_edit_text_file()` v1, `_open_ide_editor()` v1: delegate to `notepad.open_text_file_in_editor` and `ide_editor.open_ide_editor`

### IMG Factory file window / IDE Editor buttons

**Updated**: apps/gui/gui_layout.py
- `edit_ipl_file` mapping: wired to `_open_selected_text_file('.ipl')` (was `_log_missing_method`)
- `_open_selected_text_file()` v1: opens currently selected file from dir tree if it is a text-editable type; falls back to QFileDialog filtered by extension
- `_get_dir_tree_selected_file()` v1: returns currently selected file path from dir tree widget or `_dir_tree_selected_file` attr
- `_open_file_in_text_editor()` v1, `_open_file_in_ide_editor()` v1: helpers for dir-list context menu
- `_on_directory_list_context_menu()` v2: **Edit** and **Open in IDE Editor** actions for `.ide .ipl .dat .txt .cfg .ini .zon .cut .fxt` in the directory file list; COL Workshop action preserved
- Main table: switched to `DragSelectTableWidget` (was plain `QTableWidget`)

### Click-drag multi-select (all tables)

**Added**: apps/methods/populate_img_table.py — `DragSelectTableWidget` (see above)

**Updated**: apps/methods/img_core_classes.py
- `IMGEntriesTable` v2: now inherits `DragSelectTableWidget` instead of `QTableWidget`; redundant `setSelectionBehavior/Mode` calls removed (set by base class)

**Updated**: apps/components/Img_Factory/imgfactory.py
- `_create_initial_tab()`: tab tables now use `DragSelectTableWidget`

**Updated**: apps/methods/dragdrop_functions.py
- `setup_table_entry_drag()` v2: **removed `setDragEnabled(True)` and `DragDrop` mode** — these were stealing the left-button gesture away from row selection and showing a `+` drag cursor on first click; drag-out logic preserved as `table._explicit_start_drag()` (callable from right-click menu); drop-in (files → import) still works via `acceptDrops=True`

**Updated**: apps/core/right_click_actions.py
- Added **"Drag to Desktop / Folder…"** action under Extract Selected; calls `table._explicit_start_drag()` if present

---

## March 09, 2026 — RW button icon/visibility, SIGSEGV fix, stylesheet bug fix

**Updated**: apps/methods/imgfactory_svg_icons.py
- `SVGIconFactory.rw_scan_icon()`: magnifying glass with "RW" label inside lens; themed via `_create_icon`

**Updated**: apps/gui/gui_layout.py
- `rw_scan_btn`: now uses `rw_scan_icon()` SVG — no more blank button
- `refresh_icons()` v2: updates rw_scan_btn icon on theme change
- `_update_rw_btn_visibility()` v1: shows button only when active tab is IMG; connected to `main_tab_widget.currentChanged`
- `_apply_status_window_theme_styling()`: fixed `color: #{text_primary}` → `color: {text_primary}` (was emitting `##hex`, causing stylesheet parse error)

**Updated**: apps/methods/column_width_manager.py
- `setup_column_width_tracking()`: guard `new_size > 0` on sectionResized; `setSectionsMovable(False)` now set here — column drag-reorder disabled (was causing SIGSEGV when signal fired mid-move on C++ Qt object)

---


## March 09, 2026 — RW Version Scan dialog implemented in activity bar

**Updated**: apps/gui/gui_layout_custom.py
- `_show_rw_scan_dialog()` fully implemented (was a stub wired to a button but had no body)
- Shows table of all DFF/TXD entries: Name, Type, RW Hex, RW Version name
- Summary counts by version at top (e.g. `3.1.0: 120  |  Unknown: 12`)
- Unknown entries shown in dimmed placeholder-text colour
- **Force Rescan All** button: resets cached rw_version on every DFF/TXD entry, re-runs detect_file_type_and_version() from disk, refreshes column 5 of active table without full reload
- All colours from QPalette (theme-aware)

---

## March 09, 2026 — RW Scan button: forced rescan + version frequency dialog

**Updated**: apps/gui/gui_layout.py
- `RW` button added to activity log header (24×24, sits left of the log button)
- `_show_rw_scan_dialog()`: QPalette-themed dialog showing:
  - File name, format version, total entries, DFF/TXD count, detected count, unknown count
  - Version frequency table — RW version string, count, % of DFF/TXD; unknown rows highlighted orange
  - **Rescan RW Versions**: re-reads first 128 bytes of every DFF/TXD entry from disk and probes every 4-byte-aligned offset 0..64 (wider net than the original 8/12/16 probe)
  - **Reload Table**: repopulates the active IMG table widget with updated `entry.rw_version` data after a rescan
  - Progress bar during rescan; status label shows scanned/updated counts
- `_rescan_rw_versions(img_file, progress_cb)`: standalone method; handles `.dir`→`.img` path redirect; returns `(scanned, updated)` counts

---

## March 09, 2026 — VERSION_1_IOS: separate iOS detection, fix _read_header_data for V1 family

**Updated**: apps/methods/img_core_classes.py
- `IMGVersion.VERSION_1_IOS = 47` — dedicated enum for iOS GTA3/VC (`*_pvr.img`); same 12-byte/512-byte-sector format as `VERSION_PS2_V1` but kept separate so iOS files are never conflated with Android or PS2 files
- Removed `VERSION_1_MOBILE = 46` — this was architecturally wrong (iOS files do not use a companion `.dir`; they are self-contained with embedded 12-byte entries, same as `VERSION_PS2_V1`)
- Removed `_probe_dir_sector_size()` — no longer needed; restored `_detect_v1_or_v1_5()` to its original simple form
- Detection in `detect_version()`: before `detect_ps2_v1()` check, test if filename contains `_pvr` → assign `VERSION_1_IOS`; otherwise `VERSION_PS2_V1` (Android/PS2)
- `_open_ps2()`: `VERSION_1_IOS` now routes to `open_ps2_v1()` alongside `VERSION_PS2_V1`
- Open dispatch: `VERSION_1_IOS` added to PS2 block; removed from V1 block
- `_read_header_data()` v2: V1-family (VERSION_1, VERSION_1_5, VERSION_SOL) opened via `.dir` path now correctly redirects to companion `.img`; uses `endswith('.dir')` check instead of unconditional `.replace('.dir', '.img')` which could corrupt paths that don't end in `.dir`

---

## March 09, 2026 — iOS/Android GTA3/VC: V1_MOBILE detection (512-byte sectors)

**Fixed**: apps/methods/img_core_classes.py
- `IMGVersion.VERSION_1_MOBILE = 46` — new enum for .dir+.img pairs using 512-byte sectors (iOS/Android GTA3 and VC ports)
- `_probe_dir_sector_size(dir_path, img_path)` — new helper: reads first 32 .dir entries, computes max byte address at both 2048 and 512 sectors, returns 512 if 2048-sector layout overshoots the .img file size but 512-sector layout fits
- `_detect_v1_or_v1_5()` v2: calls `_probe_dir_sector_size` before name inspection; returns `'V1_MOBILE'` when 512-byte sectors detected
- `detect_version()` v5: maps `'V1_MOBILE'` result to `IMGVersion.VERSION_1_MOBILE` in both the .dir entry path and the .img-with-companion-.dir path
- `_open_version_1()` v6: reads `sector_size = 512` when `version == VERSION_1_MOBILE`, otherwise `2048`; all entry `offset` and `size` calculations use the probed sector size
- Open dispatch: `VERSION_1_MOBILE` added to the V1/V1.5/SOL routing block

**Updated**: apps/gui/gui_layout_custom.py — format reference dialog
- iOS/Android table: GTA III and VC rows updated to show `.dir+.img pair, 512-byte sectors (V1_MOBILE)`; SA row retains VER2/2048; LCS still under investigation
- Sector addressing table: added explicit row for `iOS / Android GTA3/VC (.dir+.img) → 512 bytes, detected automatically as V1_MOBILE`
- Status tab: replaced `Version 2 — GTA III/VC iOS/Android` rows with `Version 1 Mobile — GTA III/VC iOS/Android (512-byte sectors)` — read ✓, write ✗ (read-only)

---

## March 08, 2026 — Format Reference: iOS platform docs, palette-based colours

**Updated**: apps/gui/gui_layout_custom.py — `_show_rw_reference` v3
- **IMG Archive tab**: added iOS/Android platform table covering all War Drum Studios ports (GTA III Dec 2011, VC Dec 2012, SA Dec 2013 — all confirmed loading OK; LCS Jun 2015 — fails, under investigation); VCS noted as never released on iOS/Android (PSP/PS2 only); sector size table updated to distinguish PC/iOS/Android (2048 B) from PS2/PSP (512 B)
- **Status tab**: IMG archive section expanded with individual rows for GTA III iOS/Android ✓, VC iOS/Android ✓, SA iOS/Android ✓, LCS iOS/Android ~ (investigating); VCS never-released note added
- **All colour values**: replaced every hardcoded hex fallback with live `QPalette` role lookups (`Base`, `AlternateBase`, `Text`, `PlaceholderText`, `Highlight`, `Mid`, `Button`, `ButtonText`, `Link`, `LinkVisited`) — dialog renders correctly on any Qt theme (light, dark, system, custom)
- About dialog labels: replaced `color: #666` / `color: #888` with `color: palette(placeholder-text)` — no hardcoded hex anywhere in gui_layout_custom.py

---

## March 08, 2026 — RW Reference Dialog, RW Version Detection Complete (GTA III PC)

**Added**: apps/components/Txd_Editor/txd_workshop.py
- `_show_rw_reference` v1: RW Reference dialog — 6-tab reference covering everything researched for GTA III PC support in IMG Factory 1.6
  - **RW Versions tab**: all three encoding formats (plain 0x300–0x3FF, old compact 0x30000–0x3FFFF, packed 0xFFFF low-word); full version table with platform annotations; validation rules including Xbox offset scan and 0-byte guard
  - **Section Types tab**: all RW section IDs (0x0001–0x001F + Criterion plugin extensions 0x0253F2xx); header layout (type/size/version at offsets 0/4/8)
  - **TXD Format tab**: TXD container structure; tex_count differences (GTA III/VC uint32 vs SA uint16+device_id); Texture Native struct field-by-field platform comparison; pixel format detection priority order; data_size field presence rules per platform
  - **Platforms / Xbox tab**: full Xbox compression byte map (0x00/0x0B/0x0C/0x0E/0x0F/0x10/0x11); Xbox name corruption explanation and known-extension whitelist; platform_id table; PS2 vs PC differences
  - **IMG Archive tab**: V1 (GTA III/VC), V2 (SA), V1.5 (extended) formats with field layouts; sector addressing rules (2048-byte sectors); special entry display rules (Empty, Unknown)
  - **Status tab**: complete read/write support matrix for all texture formats, RW version ranges, and IMG archive versions
- Toolbar: `rw_ref_btn` added immediately after `settings_btn` — "RW Ref" with info icon and tooltip

**Added**: apps/methods/rw_versions.py (commits 5e0a45b → 2ae2999)
- `0x00000300` = "3.0.0 (GTA3 PC early)" — plain integer, earliest builds
- `0x00000304` = "3.0.4 (GTA3 PC early)" — confirmed by GTAElift.DFF
- `is_valid_rw_version` v5: plain-integer range widened from single 0x310 special case to full 0x300–0x3FF range, covering all GTA III PC pre-packed versions

**Added**: apps/methods/img_core_classes.py
- `_is_valid_rw_version` v2: mirrors rw_versions change — 0x300–0x3FF range replacing 0x310 single check

---

**Previous session commits (March 08, 2026 — earlier):**

commit 4f384de — Xbox IMG RW version detection
- `_scan_rw_version()` module-level helper: scans bytes at offsets 8/12/16 for valid RW version; handles standard and Xbox-prefixed header layouts
- `_is_valid_rw_version()` module-level helper in img_core_classes.py
- `_detect_rw_version()` / `detect_rw_version()` on IMGEntry use scanner
- `detect_rw_version_from_data()` in rw_detection.py scans multiple offsets
- `get_rw_version_light()` in populate_img_table.py validates before accepting
- `is_valid_rw_version()` in rw_versions.py: range extended to include 0x1400FFFF (Xbox)
- `0x1400FFFF` = "3.4.0.0 (GTA III/VC Xbox)" added to name table

commit a8d40e6 — V1 DIR name corruption (split-on-null)
- `rstrip(b'\x00')` → `split(b'\x00')[0]` at all four name-decode sites

commit 3d889c7 — V1 DIR name corruption (known-extension whitelist)
- `_KNOWN_GTA_EXTENSIONS` set and `_parse_entry_name()` module-level helper
- Stops at first null, finds last dot, checks chars after against known extension set; handles non-null garbage bytes like 0x77 0x78 after extension
- Fallback: keep only `[A-Za-z0-9_\-@+.]` chars
- Applied to all four name-decode sites; `detect_file_type_and_version` re-sanitizes via `_parse_entry_name`

commit 5e0a45b — Early GTA3 RW versions
- `0x00000310` = "3.1.0 (GTA3 PC)" — plain integer, not packed
- `0x0401FFFF` = "2.0.0.1 (GTA3 early TXD)" — packed but below old 0x0800FFFF lower bound
- `is_valid_rw_version` v4: uses `(v & 0xFFFF) == 0xFFFF AND 0x0400 <= (v >> 16) <= 0x1C03` discriminator for packed format; explicit special cases for 0x310 and 0x1C020037

commit 6fd5e3a — 0-byte entry display
- `get_rw_version_light()` v5: DFF/TXD entries with size==0 return "Empty"
- `get_rw_address_light()` v3: same 0-byte guard
- `get_version_text()` v3 on IMGEntry: same 0-byte guard



**Fixed**: apps/methods/dragdrop_functions.py
- drag_move v2: fixed auto-scroll to use viewport coordinates; scroll speed increased to 12px; drop now shows Copy/Move/Cancel dialog before acting

**Updated**: apps/components/File_Editor/directory_tree_browser.py
- _setup_tree_columns v2: 7 columns - Name, Type, Size, Created, Modified, Perms, RW Ver; all Interactive resize mode with fixed default widths
- _show_column_toggle_menu v2: updated for 7-column labels
- _toggle_column v1: toggles visibility across both panels; state persisted per session
- _read_rw_version v1: reads first 12 bytes of DFF/TXD, extracts RW version word at offset 8
- _move_selected_to_parent v1: moves selected items up one directory level; refreshes tree
- populate_tree_recursive v2: fills all 7 columns using os.stat(); Created=ctime, Modified=mtime, Perms=octal mode
- show_context_menu v3: added Move to /parent option under copy/cut section

**Fixed**: apps/components/Img_Factory/imgfactory.py
- autoload_game_root v5: defers dir tree placement via QTimer.singleShot(200) after show()
- _autoload_dir_tree v1: places dir tree into content_splitter at full width (state 2)
- _create_initial_tab: removed No File tab - tab bar starts empty until file loaded
- Panel toggle button: TopRightCorner setCornerWidget; cycles tabs-full/split/tree-full via _dirtree_state
- showEvent v1: repositions panel toggle button after show
- resizeEvent v2: repositions panel toggle button on resize

**Fixed**: apps/components/File_Editor/directory_tree_browser.py
- integrate_directory_tree_browser v5: removed Tab 0 splitter-hunting block; placement handled by _autoload_dir_tree
- _toggle_tree_maximise v2: cycles 3 states matching tab bar corner button

**Fixed**: apps/methods/tab_system.py
- create_tab v7: auto-switches to split view when new file tab opens with dir tree at full width

## March 03, 2026 - Unified Debug System, Date Stamping, Bug Fixes

**Added**: apps/debug/debug_functions.py
- IMGDebugger v2: routes output to terminal, log file, and activity window; all controlled by Settings > Debug Log
- set_debug_main_window v1: call once at startup to attach main window; syncs flags from img_settings
- debug_log v1: single call point for all feature debug output; silent if feature not ticked or debug mode off
- is_feature_enabled v1: returns True if feature key is enabled in Settings > Debug Log
- FEATURE_KEYS list: canonical list of all feature keys matching Settings toggles
- col_debug_log v2: routes through unified debug_log instead of standalone system
- integrate_col_debug_with_main_window v2: uses unified system
- All helper functions (create_debug_menu, add_status_indicators, etc.) rewritten to v2; removed emojis

**Added**: apps/methods/img_factory_settings.py
- debug_mode default False: master debug on/off switch
- debug_output_terminal default True: route to stdout
- debug_output_file default False: route to log file
- debug_output_activity default True: route to activity window
- debug_log_functions default []: per-feature enable list

**Updated**: apps/methods/imgfactory_ui_settings.py
- _create_debug_log_tab v2: added Debug Mode master switch group; added Output Destinations group (Terminal / Log file / Activity window); feature list expanded to include split and merge keys
- _save_settings v2: saves debug_mode, debug_output_terminal/file/activity; re-syncs live img_debugger after save

**Fixed**: apps/components/Img_Factory/imgfactory.py
- startup: calls set_debug_main_window(self) after _restore_settings so debugger is live from launch
- load_file_unified v8: COL branch now routes to _load_col_file_in_new_tab (was calling load_col_file_safely directly causing double tab); removed 51 lines of dead unreachable IMG fallback code after return True

**Fixed**: apps/methods/update_ui_for_loaded_img.py
- update_ui_for_loaded_img v6: added None guard on gui_layout.table before calling setVisible; was crashing with NoneType has no attribute setVisible when COL loaded with no active IMG tab

**Fixed**: apps/gui/gui_menu.py
- _update_recent_files_submenu: two inline QAction imports corrected from PyQt6.QtWidgets to PyQt6.QtGui; was raising ImportError on every recent file update

**Fixed**: apps/methods/img_core_classes.py
- add_entry v4: stamps date_modified on new entries at creation time; all debug prints removed; structure repaired after regex damage
- set_entry_date v5: fallback img_path resolution (file_path → img_path → dir_path); debug prints removed

**Fixed**: apps/core/undo_system.py
- set_entry_date v5: removed debug print calls; silent on persist error

**Fixed**: apps/core/pin_entries.py
- save_pin_file v1: removed debug print loop; silent on error

**Fixed**: apps/gui/gui_layout.py
- load_and_apply_pins v4: removed debug print calls

**Fixed**: apps/core/import_via.py
- _import_log: debug_only path now calls debug_log(main_window, 'import_via', msg) instead of inline settings check

**Fixed**: apps/methods/populate_img_table.py, apps/methods/col_structure_manager.py, apps/methods/col_workshop_loader.py, apps/methods/col_workshop_structures.py, apps/components/Txd_Editor/txd_workshop.py
- Removed all DummyDebugger/DebugFallback/img_debug_logger fallback classes; these printed unconditionally bypassing the debug gate; replaced with direct import of img_debugger from apps.debug.debug_functions


## February 25, 2026 - Date Tracking, Pin File Integrity, Row Colours, Version Detection

**Fixed**: apps/core/rename.py
- rename_img_entry v5: debug logging at each step; active table passed to _populate_real_img_table; rename notify uses settings popup/log flags
- _rename_with_img_archive v4: calls pin_file_sync_rename to update pin file key after rename
- rename_col_model v3: rename notify uses settings popup/log flags
- integrate_rename_functions v3: all 4 rename routes (right-click, toolbar, menu, custom) point to single rename_entry

**Fixed**: apps/core/remove.py
- remove_selected_function v6: pin filter runs before confirm dialog; skipped pins logged/popup per settings
- _remove_entries_with_tracking v3: calls pin_file_sync_remove to delete removed entry keys from pin file
- remove_entries_by_name v4: filters pinned entries during name lookup

**Fixed**: apps/core/undo_system.py
- set_entry_date v4: persists date_modified to pin file via load/update/save round-trip
- get_pin_row_colours v2: theme-aware QColor for pinned rows (dark: amber, light: pale amber)
- get_import_row_colours v1 Fixed: new=blue, replaced=red; theme-aware dark/light variants
- pin_file_sync_rename v1 Fixed: renames entry key in pin file on entry rename
- pin_file_sync_remove v1 Fixed: removes entry keys from pin file on entry delete
- check_pinned_lock v3: reads pin_warn_popup/pin_warn_log from app_settings
- refresh_after_undo v3: passes active table to _populate_real_img_table

**Fixed**: apps/core/pin_entries.py
- _save_pin_config v2: was writing plain JSON list overwriting v2.0 structure; now loads existing pin data, updates only pinned state, preserves date_modified and all other fields
- _migrate_from_v1 v2: preserves date_modified, source_file, notes when migrating old format entries

**Fixed**: apps/core/right_click_actions.py
- show_context_menu v4: uses get_active_table() instead of hardcoded gui_layout.table; duplicate Remove action removed

**Fixed**: apps/gui/gui_layout.py
- load_and_apply_pins v3: restores date_modified from pin file to all entries on load; date cell updated in table

**Fixed**: apps/gui/status_bar.py
- update_img_status v2: shows "Version 1", "Version 1.5", "Version 2", "Version SOL" instead of raw enum names

**Fixed**: apps/methods/tab_system.py
- refresh_current_tab_data v4: uses get_active_table() + _populate_real_img_table instead of shared gui_layout.table + populate_img_table; ensures rebuild refreshes correct tab

**Fixed**: apps/components/Img_Factory/imgfactory.py
- _populate_real_img_table v5: row colours applied for new (blue), replaced (red), pinned (amber); import refresh passes active table
- get_entry_rw_version v4: V1 files read from .img not .dir (was seeking directory index instead of file data)
- is_pinned second pass: only sets is_pinned=True when pin_data pinned=True (was pinning any entry present in pin file)
- import loop: case-insensitive name match + fallback to last entry for date stamp

**Fixed**: apps/methods/img_core_classes.py
- IMGVersion enum: VERSION_1_5 = 15 added for extended DIR/IMG (up to 4GB, long filenames)
- _detect_v1_or_v1_5: detects V1.5 by IMG size >2GB or entry name field with no null terminator
- detect_img_version: .dir path now calls _detect_v1_or_v1_5 to distinguish V1 from V1.5

**Added**: apps/utils/app_settings_system.py
- Settings > Interface > Rename Notifications group: popup and log toggles (rename_notify_popup, rename_notify_log)
- Settings > Interface > Pinned Entry Warnings group: popup and log toggles (pin_warn_popup, pin_warn_log)


## February 24, 2026 - Icons, Buttons, Startup Fixes

**Added**: imgfactory_svg_icons.py
- All 217 icons updated: stroke-width 2.5, stroke-linecap round, stroke-linejoin round (handdrawn style)
- _create_icon: injects coloured square background rect (rx=4) behind icon paths
- All 67 wrapper functions accept bg_color parameter, composite via QPainter
- get_close_all_icon v1: two X marks side by side, distinct from single close
- get_rebuild_all_icon v1: circular arrows with stacked lines, distinct from rebuild and save
- undo_icon v8: clear curved-arrow with filled arrowhead, replaces circular blob
- get_undobar_icon v2: same curved-arrow style, consistent with undo_icon
- get_extract_icon v2: dotted-border box with downward arrow, distinct from export

**Fixed**: gui_layout.py
- create_pastel_button: stores icon as Qt property stored_icon/stored_bg/full_label
- _get_svg_icon: passes size=24, theme colour, bg_color per button
- icon_map: extract key now maps to get_extract_icon (was get_export_icon)
- icon_map: rebuild-all maps to get_rebuild_all_icon (was document-save)
- icon_map: window-close-all maps to get_close_all_icon
- Button tuples: Close All uses window-close-all, Rebuild All uses rebuild-all
- _set_right_panel_icon_only: buttons 36x36 icon-only when panel narrow (<250px)
- _update_button_display_mode: text-only default, splitter resize triggers icon-only
- create_status_window: f_entries_btn uses safe hasattr lambda (method only in subclass)

**Added**: apps/app_info.py
- App_name, App_build, App_auth constants, avoids circular imports across 15 files

**Fixed**: imgfactory.py
- _apply_button_display_mode_from_settings v2: text_only at startup, no icon+text flash
- All hardcoded "IMG Factory 1.5" runtime strings replaced with App_name/App_build

**Restored**: apps/gui/directory_tree_system.py
- File was missing from working tree, restored from 43fcb09


## February 22, 2026 - Dir Tree Twin Panel, Drag/Drop, Undo/Trash

**Added**: directory_tree_browser.py
- _single_container wraps address bar + tree as one unit for reliable hide/show
- Twin panel (_enable_twin_panel v3): two independent panels each with address bar, Go button, tree
- 4-state layout cycle button (hidden in single mode, visible in twin): W1L|W2R, W1T/W2B, W2L|W1R, W2T/W1B
- _cycle_layout v1: cycles _twin_splitter orientation and panel order, updates SVG icon each state
- _enable_single_panel v3: restores tree to _single_container, layout stretch correct
- _refresh_all_panels v2: refreshes left and right panels independently using _right_current_path
- _populate_second_tree v2: uses finally block to safely restore self.tree after swap, tracks _right_current_path
- _active_tree tracking in show_context_menu v2: uses self.sender() to know which panel triggered
- delete_selected v4: uses _active_tree so right panel deletions get correct selected items
- _copy_selected_files v2: pushes undo entry after copy
- delete_selected v3→v4: uses send2trash for system trash instead of permanent delete
- Drag/drop wired on both trees via _setup_tree_dragdrop
- Refresh button and context menu refresh both call _refresh_all_panels

**Added**: dragdrop_functions.py
- setup_tree_drag_drop v1: drag from tree, drop onto folder with shutil copy, pushes undo entry
- setup_table_entry_drag v1: drag IMG/COL entries from table, drop files onto table to import
- setup_tree_as_extract_target v2: drop table entries onto dir tree folder to extract them
- Hover highlight on drag_move using theme palette highlight colour at 50% opacity
- drag_leave clears hover, drop clears hover
- Fixed: hasData → hasFormat for QMimeData
- Fixed: import_multiple_files now gets img_archive from current tab before calling
- Desktop drag: entries extracted to temp dir first so OS gets real files not sticky note

**Added**: imgfactory_svg_icons.py
- get_arrow_right_icon v3: filled polygon arrowhead + double shaft lines
- get_arrow_left_icon v3: filled polygon arrowhead + double shaft lines
- get_go_icon v1: arrow-in-circle for Go buttons
- get_layout_w1left_icon v1: filled left panel, outline right
- get_layout_w1top_icon v1: filled top panel, outline bottom
- get_layout_w2left_icon v1: outline left, filled right panel
- get_layout_w2top_icon v1: outline top, filled bottom panel

**Fixed**: gui_layout.py
- _toggle_merge_view_layout v3: 4-state cycle matching dir tree layout cycle
- Split toggle button uses layout SVG icons instead of split_horizontal/vertical
- self.file_window stored as instance variable (was local) for hide/show from imgfactory.py
- file_window.hide() on dir tree active, show() on file tab active (was hiding main_tab_widget only)

**Fixed**: imgfactory.py
- _on_tab_changed v5: hides file_window entirely when dir tree active, content_splitter 50/50 when file tab open

**Fixed**: tab_system.py
- setup_table_entry_drag wired on every new table widget

**Added**: requirements.txt
- send2trash, PyQt6



## February 21, 2026 - COL Loading, Tab System & UI Fixes

**Fixed**: col_core_classes.py
- COL1 parser: removed incorrect num_unknown field from counts struct (4 fields not 5)
- Garbage billion-sized counts no longer produced on COL1 parse

**Fixed**: col_parsing_functions.py
- col_file.load() replaced with load_from_file(file_path) - correct API call
- COLFile(file_path) replaced with COLFile() - correct constructor

**Fixed**: col_loader.py
- Same load()/COLFile(file_path) fixes as above
- Broken load_col_file_object repaired (mismatched try/except/if)

**Fixed**: populate_col_table.py
- Removed duplicate load_col_file_safely (vers 1) that shadowed the fixed vers 2
- load_col_file_safely now uses create_tab() and populates tab's own table
- setup_col_tab no longer requires close_manager to create a new tab
- Broken load_col_file_object repaired

**Fixed**: tab_system.py
- create_tab() no longer calls create_main_ui_with_splitters - prevented DIR Tree overwrite
- Each tab now gets its own standalone QTableWidget instead of shared GUI components

**Fixed**: gui_layout.py
- _create_file_window now contains content_splitter for merge view toggle
- _on_directory_file_selected for .col now calls load_file_unified instead of col_workshop
- Added right-click context menu to directory file list (Open in COL Workshop for .col files)
- Added _toggle_merge_view_layout: toggles content_splitter between horizontal/vertical
- All log bar buttons now icon-only (File Entries, Merge View, Search, Show Logs)
- Show Logs: opens log file dialog when file logging enabled, toggles widget otherwise
- Added _show_log_file method with Clear Log File option

**Fixed**: gui_menu.py
- Added Open Selected in COL Workshop to COL menu (enabled only when .col highlighted)
- Added _open_selected_in_workshop static method to COLMenuBuilder

**Fixed**: imgfactory.py
- _on_tab_changed v3→v4: uses _dir_tree_tab_widget reference not index==0
- DIR Tree tab insertion stores self._dir_tree_tab_widget for reliable identification
- Removed QTimer.singleShot(200, setCurrentIndex(0)) that hijacked first file load
- create_main_ui_with_splitters called once in _create_ui, right panel/log outside tabs
- content_splitter.replaceWidget(0) correctly swaps table for main_tab_widget
- _load_img_file_in_new_tab v2: uses create_tab first, stores _loading_img_tab_index
- _on_img_loaded v5: uses stored tab index, populates tab's own table_ref
- _populate_real_img_table v4: accepts optional table parameter
- _populate_img_table_widget added as wrapper for tab-specific population
- log_message v3: writes to file when log_to_file setting enabled
- Window title updated to IMG Factory 1.6

**Fixed**: img_factory_settings.py
- Added log_to_file (default False) and log_file_path defaults

**Added**: imgfactory_svg_icons.py
- get_split_horizontal_icon: two panels side by side
- get_split_vertical_icon: two panels stacked

**Renamed**: Dir Tree / Directory Tree button → Merge View (button/tooltip only, tab keeps Dir Tree label)



**Fixed**: status_bar.py
- File size now reads from os.path.getsize() - IMGFile has no file_size attribute
- Version display uses enum .name correctly
- update_img_status() called in _on_img_loaded() - status now updates on file load
- set_ready_status() called in _update_ui_for_no_img() - resets on file close
- All emojis replaced with SVG icons (file, info, reset, checkmark, close)

**Fixed**: imgfactory_ui_settings.py
- Dialog now uses main_window.img_settings instance instead of creating a new one
- Tab settings (height, min width, style, position) now save correctly to JSON

**Fixed**: imgfactory.py
- apply_tab_settings() called at startup after GUI build - settings now load on start

## February 21, 2026 - Theme & SVG Icon Fixes

**Fixed**: theme_integration.py
- SVG icon color now reads text_primary from theme on every theme change
- refresh_icons() called on active layout after theme switch
- Menu bar re-applies styling on theme change via _apply_menu_bar_styling()
- Fixed missing except block in on_theme_changed()

**Fixed**: gui_layout.py
- Added refresh_icons(color) method - updates all toolbar button icons on theme change
- set_theme_mode() now reads text_primary and calls refresh_icons()
- Toolbar buttons pass icon_color at creation time

**Fixed**: gui_layout_custom.py
- Removed hardcoded #ffffff icon color - now reads text_primary from theme
- Added refresh_icons(color) method for custom toolbar buttons
- All icon_factory calls pass icon_color at creation time

**Fixed**: gui_menu_custom.py
- _get_themed_stylesheet() was using wrong color keys (background, text)
- Corrected to use bg_secondary, text_primary, text_secondary

**Fixed**: imgfactory_svg_icons.py
- Default fallback color changed from #ffffff to #000000

**Fixed**: imgfactory.py
- SVGIconFactory.set_theme_color() now called at startup before any UI builds

## December 24, 2025 - SVG Icon System Consolidation

### Fixed
- Consolidated svg_shared_icons.py and svg_icon_factory.py into single imgfactory_svg_icons.py
- Added backward compatibility wrappers for get_*_icon() functions
- Fixed circular import in _create_icon() with cached theme color approach
- All SVG icons now use SVGIconFactory class with standalone function wrappers

### Technical
- Added SVGIconFactory.set_theme_color() for cached color management
- Prevents circular AppSettings import during icon creation
- Maintained theme-aware icon support without initialization loops

# X-Seti - October22 - December 04 2025 - IMG Factory 1.5 ChangeLog

# IMG Factory 1.5/1.6 - ChangeLog - (New System)

Complete history of fixes, updates, and improvements.

**Fixed**: - December 28, 2025
- Many functions have been fixed and not documented
- Search function has been fixed, botton and menus
- Replaced old memory core for storing img data
- inverse has been fixed.
- import. remove functions now show clean file window lists, no more corruption.
- reload has been fixed, now safely reloads the img file.
- Tab system has been reworked.

---
**Fixed**: - December 13, 2025
- Collision boxes will now render correctly with proper min/max point coordinates
- Collision mesh faces will now render correctly with proper vertex positions
- Shadow mesh faces will now render correctly with proper vertex positions
- The 3D viewport will properly display collision geometry as intended

**Fixed**: - December 07, 2025
methods/svg_shared_icons.py - get_app_icon() Version: 2
- Fixed color placeholder replacement
- Changed from runtime theme colors to hardcoded values
- Icon now renders correctly with gradient background

**Fixed**: - December 04, 2025
gui_menu.py
gui_layout.py
- Fixed search functions
- Fixed tab check for search functions
- Rewrite on the GUI interface.
- Local langauge settings - needs work
- Icon settings, Button Settings
- Extaction functions, export png images for textures in the img file.

**Fixed**: - December 02, 2025
img_core_classes.py
img_entry_operations.py
rename.py
- Rename has been fixed for the menu bar, right click and panel rename button.
- Tabbing across all opened files, works flowlessly.
- Pin selected entries - locking files from being changed

**Fixed**: - December 02, 2025
gui/gui_menu.py
- optimized menu bar, shows only img related menu's unless other apps are docked.

**Fixed**: - November 29, 2025
methods/common_functions.py
- Created new shared function module to consolidate duplicate functions
- Consolidated sanitize_filename, detect_file_type, and detect_rw_version from core/impotr.py and core/import_via.py
- Eliminates function duplication between import modules

methods/img_core_classes.py
- Added missing rebuild_img_file() method to IMGFile class
- Fixes error: 'IMGFile' object has no attribute 'rebuild_img_file'
- Method calls appropriate version-specific rebuild (_rebuild_version1 or _rebuild_version2)
- Version updated to 2 for save_img_file and 1 for rebuild_img_file

**New**: - November 21, 2025
Added AI access to help resolve bugs I can not seem to fix myself.

**Fixed**: - November 20, 2025

core/impotr.py
methods/img_import_functions.py
methods/img_entry_operations.py
- IMG Import Functions - NO AUTO-SAVE during import
- ✅ No rebuild_img_file() calls during import
- ✅ Marks entries as is_new_entry for Save Entry
- ✅ Uses tab-aware import

**Fixed**: - November 18, 2025
- Tab system is finally fixed once and for all.

core/rebuild.py
methods/remove.py
methods/remove_via.py
- ✅ FIXED: Now distinguishes between found and missing entries
- ✅ Accurate user feedback ("Removed 1, 15 not found")
- ✅ Uses tab-aware file detection
- ✅ No global file_object/file_type

methods/export.py
- ✅ Uses tab-aware file detection from active tab
- ✅ Removed global file_object/file_type
- ✅ Now imports get_current_file_from_active_tab

methods/export_via.py
- ✅ 'str' object has no attribute 'name' error
- ✅ Exports real IMG entry objects (not strings)
- ✅ Uses correct export_entry (offset/size)
- ✅ NO dependency on gui.ide_dialog
- ✅ Uses apps.methods.ide_parser_functions
- ✅ Tab-aware with proper imports
- ✅ Handles both IDE and text file export lists
- ✅ Fixes "IDE dialog system not available" error

methods/rw_versions.py
- ✅ Comprehensive version mapping for all GTA games
- ✅ Prevents IMG corruption by preserving correct RW versions
- ✅ Syntax error in get_model_format_version function

methods/populate_img_table.py
- ✅ Clean separator for status info
- ✅ Proper highlighting of new entries

**Unresolved**: - November 17, 2025
core/impotr - still bugged - filelist corruption
- ✅ Now sets is_new_entry=True on imported entries
- ✅ Uses tab-aware refresh
- ✅ Handles highlighting correctly
- ✅ Tuple unpacking for import count

core/import_via.py
- ✅ Uses tab-aware file detection
- ✅ Marks imported entries as is_new_entry=True
- ✅ Proper duplicate handling

**Unresolved**: - November 15, 2025
- ❌ Tab system is still creating problems, Trying to export entries we get error messages "loaded img file can not be found". ??

### 1. tab_system.py (Version 6)
**Location**: apps/methods/tab_system.py
**Changes**:
- `validate_tab_before_operation` (vers 3) - Now checks tab widget data directly
- `get_current_file_from_active_tab` (vers 2) - Gets data from tab widget, not current_img
- `get_tab_file_data` (vers 4) - Removed fallback to current_img
- `get_current_active_tab_info` (vers 2) - Uses tab widget exclusively

**Key Fix**: Validation now checks the actual tab widget data instead of main_window.current_img

### 2. file_validation.py (Version 1)
**Location**: apps/methods/file_validation.py
**Purpose**: Universal file validation that works with IMG, COL, and TXD files

**Functions**:
- `validate_img_file()` - For IMG-only operations
- `validate_col_file()` - For COL-only operations
- `validate_txd_file()` - For TXD-only operations
- `validate_any_file()` - For operations that work with any file type
- `get_selected_entries_for_operation()` - Validates AND gets selected entries

**Update**: - November 15, 2025
- ✅ Dynamic file type detection
- ✅ Proper error messages per file type
- ✅ Works with tab system automatically
- ✅ No more hardcoded "Current tab does not contain an IMG file"


**Fixed**: - November 14, 2025
- Sussussfully fixed the tab systen, each img, col and txd gets its own tab.
- ✅ SVG icons integration for the img factory app.

**Fixed**: - November 11, 2025
- Sussussfully moved img Factory to its new location with all the other tools.
- ✅ moved all file paths from methods to apps.methods
- ✅ moved all file paths from core to apps.core
- ✅ moved all file paths from components to apps.components
- ✅ moved all file paths from debug to apps.debug
- ✅ Added better tab handling

## October 2025 - small break. 

**Fixed**: - Oct 24, 2025
- app_settings_system updated, Theme save function repaired
- ✅ Added all QT6 colors, no more buggy looking app windows. 
- ✅ Added Gadgets tab, Customizable gadgets, buttons and scrollbars.

**Added**: - Oct 22, 2025
- New color variables for complete theme support:
- ✅ button_pressed - Pressed button state color
- ✅ selection_background - Selection highlight color for tables/trees
- ✅ selection_text - Text color for selected items
- ✅ table_row_even - Even row background color
- ✅ table_row_odd - Odd row background color

- Oct 22, 2025
- ✅ update_themes_script.py:
- ✅ get_smart_colors_for_theme() - Added base colors and new calculated colors
- ✅ Updated script output messages (removed emojis, using brackets)
- ✅ Script now ensures all 17 base colors exist in theme files

- ✅ utils/app_settings_system.py:
- ✅ get_theme_colors() #vers 2 - Added fallback support for missing colors
- ✅ _get_hardcoded_defaults() #vers 1 - NEW METHOD - Returns complete default color set
- ✅ _generate_stylesheet() #vers 1 - NEW METHOD - Shared stylesheet generator
- ✅ get_stylesheet() #vers 4 (AppSettings class) - Now calls _generate_stylesheet()
- ✅ get_stylesheet() #vers 4 (SettingsDialog class) - Now calls _generate_stylesheet()
- ✅ Updated stylesheet to use new color variables (button_pressed, selection_background, selection_text)

- ✅ components/File_Browser/dolphin_dialog.py - NEW FILE:
- ✅ Complete Dolphin-style file browser dialog
- ✅ Replaces native Qt dialogs with themed custom browser
- ✅ Full theme integration from IMG Factory
- ✅ SVG icons (no emojis)
- ✅ Features: single/multi-select, create folder, rename, delete, properties
- ✅ Places sidebar with common locations
- ✅ Project Folders sidebar (replaces Devices)
- ✅ File preview with system command integration (file/mdls/PowerShell)

**Fixed**: - Oct 22, 2025
- ✅ Black rows in file dialogs on light themes (native Qt dialog theme conflict)
- ✅ Missing color definitions causing fallback to hardcoded values
- ✅ Inconsistent selection colors across widgets
- ✅ Button pressed state not using theme colors

**Updated**: - Oct 22, 2025
- ✅ 5 theme JSON files updated with missing color variables
- ✅ 26 theme files already had complete color sets
- ✅ All 31 themes backed up to themes_backup/


### October 22, 2025 - COL Viewer Complete
**Added**:
- ✅ Complete COL 3D Viewer from scratch
- ✅ COL_Parser.py - Clean parser (no legacy bugs)
- ✅ COL_Materials.py - Material database (214 materials)
- ✅ col_viewer.py - OpenGL 3D viewport
- ✅ col_viewer_integration.py - Right-click integration
- ✅ Material groups organized by type
- ✅ Auto game detection (GTA III/VC/SA)
- ✅ Theme integration support
- ✅ Camera controls (orbit, pan, zoom)
- ✅ Complete documentation

**Features**:
- View COL files in 3D
- Show mesh, spheres, boxes, bounds
- Material names display
- Right-click context menu
- Theme-aware colors
- 3DS Max style controls

---
  
## September 2025

### September 4, 2025 - Dump Command Fix
**Fixed**:
- ✅ Dump command has been fixed
- ✅ Proper file dumping functionality
- ✅ Error handling improved

---

## August 2025

### August 26, 2025 - Rebuild System
**Fixed**:
- ✅ Rebuild system is fixed
- ✅ Rebuild all now works with menu
- ✅ Rebuild open tabs option
- ✅ Rebuild folder contents option
- ✅ Better progress feedback

---

### August 15, 2025 - Export & Dump Functions
**Fixed**:
- ✅ Fixed Export functions
- ✅ Fixed Dump functions
- ✅ Better error handling

**Removed**:
- ❌ Quick Export (replaced with improved Export)

---

### August 14, 2025 - IDE Editor & Menu
**Fixed**:
- ✅ IDE Editor - Updated and bugs fixed
- ✅ Menu Options fixed
- ✅ Better IDE file handling
- ✅ Improved menu navigation

---

### August 12, 2025 - COL Editor Core
**Fixed**:
- ✅ Col Editor - Core utility ready
- ✅ Collision system restored
- ✅ Collision system working
- ✅ Basic COL editing functional

**Note**: This was the foundation. October 2025 COL Viewer is complete rewrite.

---

### August 10, 2025 - Tab System
**Fixed**:
- ✅ Tab system for IMG's fixed
- ✅ Close first tab fixed
- ✅ Multipl**Fixed**:e tabs work properly
- ✅ Tab switching improved

---

### August 9, 2025 - Startup System
**Fixed**:
- ✅ Init startup order fixed
- ✅ Smoother IMG loading
- ✅ Better initialization sequence
- ✅ Reduced startup errors

---

### August 7, 2025 - Theme System Update
**Fixed**:
- ✅ Light/Dark theming system updated
- ✅ core/theme_integration.py improved

**Partial Fix**:
- 🔶 Import function needs work
- 🔶 import_via ide error handling
- 🔶 Still needs additional work (see TODO.md)

**Still Needs Work**:
- Theme system needs adjusting for other styles
- More theme variations needed

---

### August 6, 2025 - Multiple Fixes
**Fixed**:
- ✅ File Window Display List
- ✅ Status Window theming
- ✅ File Window Theming
- ✅ Reload function works again
- ✅ Status/Progress Bar fixed and moved to methods/Progressbar.py

**Removed**:
- ❌ Just Green Theme Base
- ❌ Rebuild_As removed from all files

**Added**:
- ✅ New theme functions
- ✅ Save Entry menu option
- ✅ Shared progressbar function

**Theme Changes**:
```json
// Added Save Entry with themed colors
{
  "text": "Save Entry...",
  "icon": "document-save-entry",
  "action": "save_img_entry",
  "color": "#E8F5E8"
}
```

---

### August 4, 2025 - Testing & Verification
**Checked**:
- ✅ Loading single IMG
- ✅ Loading multiple IMG
- ✅ Closing single IMG  
- ✅ Closing multiple IMG
- ✅ All core operations verified

---

## July 2025

### July 31, 2025 - UI Improvements
**Changed**:
- ✅ Rebuild_As removed
- ✅ "Save Entries" seemed more logical
- ✅ Update_list renamed to refresh_table

**Old Code**:
```python
("Refresh", "update", "view-refresh", "#F9FBE7", "refresh_table")
```

**New Code**:
```json
{
  "text": "Refresh",
  "action": "update",
  "icon": "view-refresh",
  "color": "#F9FBE7"
}
```

**Reason**: Better naming convention, more logical structure

---

### July 2025 - Project Start
**Initialized**:
- ✅ IMG Factory 1.5 project started
- ✅ New changelog system
- ✅ Clean code approach
- ✅ No legacy bugs philosophy
- ✅ Proper documentation standards

---

## Version History Summary

### Version 1.5 (Current - October 2025)
**Major Features**:
- Complete COL 3D Viewer
- Material database (214 materials)
- Theme system improvements
- Better file operations
- Enhanced error handling
- Comprehensive documentation

**Line Count**: ~70KB of clean code for COL viewer alone
**Files Added**: 10+ new components
**Bugs Fixed**: 20+ issues resolved


### Version 1.0-1.4 (July-September 2025)
**Foundation Work**:
- Core IMG functionality
- Basic COL support
- Theme system foundation
- File operations
- UI improvements
- Menu system
- Tab management

---

## Statistics

### June 2025
- **Conception**: Img Factory 1.4 - X-Seti 
- **Successer to**: Img Factory 1.2 - MexUK
- **Revision**: Img Factory 1.3 (Patched) MexUK / X-Seti
- **Proof of conception**: Img Factory 1.4 was mean't to be a stand alone img editor: Plan and Simple.

### June 2025
- **Conception**: Img Factory 1.5 - X-Seti 
- **Proof of conception**: IMG Factory 1.5 main aim is to replace all existing gta tools.

### August 2025
- **Issues Fixed**: 15+
- **Features Added**: 10+
- **Code Cleaned**: Multiple files
- **Documentation**: Updated

### September 2025
- **Issues Fixed**: 5+
- **Features Added**: 3+

### October 2025  
- **Major Feature**: COL Viewer (complete)
- **Files Created**: 10+
- **Documentation**: 6 files
- **Materials Added**: 214 definitions

---

## Naming Conventions Applied
Throughout development, these rules have been enforced:

✅ **DO USE**:
- Simple, clear names
- Version numbers on methods
- Proper headers

❌ **DO NOT USE**:
- "Enhanced"
- "Fallback" 
- "Improved"
- "Fixed"
- "Fix"
- "Patch"
- "Patched"
- "Updated"
- "Integrated"
- "Clean"

**Reason**: Avoid confusion and duplication

---

## Known Issues (Moving to TODO)

Items from old changelog moved to TODO.md:
1. Tab system export/dump issues
2. Export combining files incorrectly
3. Dump function needs same logic as export
4. COL dialog hardcoded backgrounds
5. Import via IDE errors
6. Folder import options needed
7. Text file import needed
8. Drag and drop support
9. Highlighting function inaccuracy
10. Save Entry function issues
11. Theme switching from first page

See `TODO.md` for complete task list.

---

## Development Philosophy

**Established Standards**:
1. ✅ Clean code - no legacy bugs
2. ✅ No fallback code - works or doesn't
3. ✅ No patch files
4. ✅ Simple, clear naming
5. ✅ Check for duplicates first
6. ✅ Files under 90k
7. ✅ Proper version tracking
8. ✅ Complete documentation
9. ✅ User-first approach
10. ✅ Community-focused

---

## Contributors

**Primary Developer**: X-Seti (2025)
**Original COL Data**: Steve M., illspirit (2005)
**Community**: Testing and feedback

See `Credits.md` for complete attribution.

---

## Next Release

See `TODO.md` for planned features and fixes.

**Target Areas**:
- Tab system improvements
- Export/Dump fixes
- Theme system enhancements
- Import system improvements
- DFF texture mapping (future)

---

**Last Updated**: October 22, 2025
**Total Commits**: 100+ improvements
**Lines of Code**: 10,000+ (clean, documented)
**Community Impact**: Ongoing

---

### Build 78-95 — DFF Texture List, IDE Cross-Reference, Missing TXD Scanner

**New files**:
- apps/core/dff_texlist.py — RW chunk walker (linear scan), parse_dff_textures, check_txd_in_img, ide_txd_in_img, find_missing_txds
- apps/gui/dff_texlist_dialog.py — DFFTexListDialog, MissingTXDDialog (batch scanner, large-IMG threshold)
- apps/core/img_ide_find.py — find_not_in_ide, find_orphan_txd, find_orphan_col, find_all_col, find_all_dff_in_ide

**Updated**:
- apps/core/right_click_actions.py — show_context_menu v6 (clean layout, SVG icons, submenus per file type); show_dff_texture_list v3 (real parser, IDE xref lookup); _xref_for_img (os.path.commonpath matching)
- apps/components/Dat_Browser/dat_browser.py — project game_root priority over Dir Tree; xref stored per game_root in xref_by_root dict; table updates deferred via QTimer to prevent segfault
- apps/methods/populate_img_table.py — apply_xref_status v2 (Status column updated with In IDE/Not in IDE/Orphan TXD/In COLFILE, detects column by name); IDE Model + IDE TXD hidden columns (cols 9/10 for standard layout); _set_cell guard
- apps/methods/gta_dat_parser.py — xref tagged with game_root
- apps/gui/gui_menu.py — Tools > Find in IDE/COL submenu (5 entries)
- apps/components/Img_Factory/imgfactory.py — column count 9->11 for standard layout, 10->12 for LVZ/DTZ

**Fixes**:
- Segfault from column out-of-bounds access (_set_cell guard, deferred xref apply)
- IDE TXD check was checking texture names not the IDE-declared TXD name
- DAT Browser was using Dir Tree last-browsed path instead of project game_root
- xref game mismatch (VC files showing orphan when SA xref loaded) — os.path.commonpath fix

---

### Builds 162–196 — TXD Workshop overhaul, COL Workshop fixes, PyInstaller support

**Session**: March–April 2026 (Claude AI pair programming)

---

#### TXD Workshop (txd_workshop.py)

**Parser fixes (Build 163–165, 178–180, 189):**
- COL1 parse order fixed (skip-4 after spheres, before boxes)
- COL2/3 SA: 5 bugs fixed vs DragonFF reference (block_base, flags field, offset order, vertex count, multi-model advance)
- PAL4 nibble order: high nibble = first pixel
- DXT5 alpha: correct 6/4 intermediates
- GTA3/VC palette RGBA vs SA BGRA (`palette_is_bgra` flag)
- PAL8 pixel offset: palette raw + pixel data 4-byte size prefix
- **Build 189**: D3D9 SA textures (platform_id=9) wrongly detected as DXT1 — D3D8 `platform_prop==1/3/5` check now guarded by `platform_id==8`
- **Build 189**: Name/mask fields: `find(b'\x00')` first-null termination instead of `rstrip` — fixes garbled alpha names from garbage bytes

**Navbar buttons (Build 184–188):**
- Flip/Rotate: pure Python pixel loops replaced with PIL `Image.transpose()` (instant vs frozen)
- Thumbnail (col 0) now refreshed after flip/rotate/filters — was only updating text col 1
- `_reload_texture_table` v4: restores row selection after rebuild
- Copy/Paste/Clone/Delete: selection restored after table reload
- `_convert_texture` v3: actual PIL conversion for all 8 formats (was only setting format flag)
- `_edit_main_texture` v2: selects texture in parent workshop, raises window
- Removed duplicate `MipmapManagerWindow` class (428 dead lines)
- `edit_btn` removed from `_update_all_buttons` (local var, not `self.X`)
- **Build 188**: Icon panel buttons never enabling — text panel overwrites `self.X` refs; `_set_transform_buttons_enabled()` now walks `findChildren(QPushButton)` on both panels

**Import button (Build 187):**
- Always enabled (was disabled until TXD loaded)
- Single file + selected texture: Replace/Add/Cancel dialog
- Replace: LANCZOS resample to original dims, keep format/name
- `_validate_texture_dimensions` defined (was called but missing → AttributeError on every import)
- New texture dict includes all required fields (mipmap_levels, raster_format_flags, etc.)

---

#### COL Workshop (col_workshop.py)

**Icon panel buttons (Build 190):**
- Same overwrite bug as TXD: `_set_col_buttons_enabled()` walks both panels via `findChildren`
- `surface_type_btn` and `build_from_txd_btn` wired to handlers (both panels)

**Delete/wrapper crash (Build 191):**
- 22 alias wrappers: `(*a, **kw)` → `(*_, **__)`, drop Qt signal args before forwarding
- Qt `clicked` signal passes `bool(checked)` — was forwarded to methods taking only `self` → TypeError + core dump

**Material editor (Build 191–196):**
- Three modes: Apply to Selected Faces / Apply to ALL Faces / Enter Paint Mode
- Shows current face material at top of dialog
- Undo pushed before every paint operation
- Paint toolbar (Build 192–196): replaces yellow HUD text banner
  - Material dropdown with 16×16 colour swatch icons per item
  - 22×22 colour swatch label beside combo (updates live)
  - ↩ Undo button, ✕ Exit Paint button
  - **Build 196**: toolbar is a floating overlay parented to COLWorkshop, positioned via `mapToGlobal` over viewport (fixes Qt layout reflow issue)
  - **Build 196**: 🖌 Paint | 💧 Dropper (pick material, auto-reverts) | ▣ Fill (flood same-material faces) tools

**Right-click face context menu (Build 194):**
- Right-click a face in viewport (normal or paint mode): info header with colour icon, Copy material, Paste material (shows clipboard value), Apply to N selected faces, Clear this face, Clear all faces, Open material editor
- All mutations push undo; right-click on empty space = rotation drag (unchanged)

**Large file support (Build 190):**
- Files >64 MB: `mmap.mmap(ACCESS_READ)` — no 2GB RAM spike
- Progress reporting every 5% + `QApplication.processEvents()`
- >512 MB: confirmation dialog; >32 MB: WaitCursor
- Sanity limits raised: vertices/faces 200k→2M, spheres/boxes 10k→50k

**Stubs implemented (Build 185):**
- `_convert_surface`: version change dialog (COL1/2/3)
- `_build_col_from_txd`: scan TXD → create stub COL models
- `_show/_create/_remove_shadow_mesh`: full shadow mesh management
- `_compress_col`: informative; `_uncompress_col`: reload from disk
- `_show_shaders_dialog`: wireframe/solid/painted preset picker

---

#### PyInstaller support (Build 195)

- `imgfactory.spec` created with `PyQt6.QtNetwork` in hiddenimports (file:// protocol handler), `apps/` and `assets/` in datas, WebEngine excluded (~200MB saving)
- `_qurl_from_path()` wrapper in `dragdrop_functions.py`: `os.path.abspath()` before `QUrl.fromLocalFile()` — fixes "unknown protocol file://" in frozen builds
- 4 call sites in dragdrop_functions, 1 in img_browser replaced

---

**Files changed this session:**
- `apps/components/Txd_Editor/txd_workshop.py`
- `apps/components/Col_Editor/col_workshop.py`
- `apps/methods/col_workshop_loader.py`
- `apps/methods/col_workshop_parser.py`
- `apps/methods/dragdrop_functions.py`
- `apps/components/Img_Browser/img_browser.py`
- `imgfactory.spec` (new)

