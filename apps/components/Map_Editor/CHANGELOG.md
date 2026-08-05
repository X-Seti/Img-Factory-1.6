# Changelog

History extracted from map_workshop.py's header comment block (moved
out per Keith, Aug 1 2026, to keep the source file focused on code).

## Origin (Model Workshop base, before the Map Workshop fork)

- **Apr 2026** — Model Workshop (based on COL Workshop) created.
  - [FIX] `_make_slot_pix` crash: imported `QPolygonF` into local scope.
  - [FIX] Material Editor cube preview crash: added missing `QPolygonF`
    import to `_open_dff_material_list` scope.
  - [FIX] `_rebuild_grid` `QWidget` crash: removed redundant
    `deleteLater` (`QScrollArea` auto-deletes old widget).
  - [FIX] `_rebuild_grid` `QFrame` deletion crash: reparent slots
    before scroll widget swap.
- **May 8, 2026** — Model editor work.
- **Jul 7, 2026** — Added 3ds Max-style 4-pane viewport (Top/Front/
  Side/Perspective) via `QStackedWidget` central widget; user-assignable
  per pane by right-click; splitter-resizable; layout persists to
  `model_workshop.json`.
- **Jul 11, 2026** — Diagnostic rollback to pre-4-pane state then
  restored: black-window/`QOpenGLWidget` context failure confirmed via
  `journalctl` to be a hardware/driver issue (PCIe BadTLP errors +
  NVIDIA GSP firmware load failure on the GPU, starting Jul 10) - not
  caused by this file or `dff_viewport.py`. The `QT_QPA_PLATFORM=xcb` /
  `QSG_RHI_BACKEND=opengl` forcing in this file doesn't help since the
  GPU itself is failing at the hardware level.

## Map Workshop fork (from here on, this file is Map Workshop, not Model Workshop)

See `map_workshop_old.py` in this same folder for the full, detailed
history of the Map Workshop port from `map_workshop_old_version.py`
(Object Browser, Instance List, Control Panel, Editing Panel,
World Viewport, ribbon framework, the dock/ribbon snap-drag
investigation, etc.) - that file carries its own extensive dated
header covering all of that work.

This copy of `map_workshop.py` (the current, active one) started as a
fresh copy of `model_workshop.py` (Aug 1, 2026), with Map Workshop's
real content (Object Browser, IPL Inst File, Control Panel docks)
grafted in piece by piece, since Model Workshop's own dock/ribbon
snapping was confirmed working live while the evolved `map_workshop.py`
fork's was not, and the exact cause of that regression was never
conclusively found despite extensive isolated testing.

- **Aug 1, 2026** — Grafted Object Browser, IPL Inst File, Control
  Panel docks in; fixed dock-wrapping parity issues; found and fixed
  the actual docking regression (a mix of `dock.setFeatures()`,
  `installEventFilter`, and other factors - see git history for the
  full investigation); tidied up Control Panel's layout (18px controls,
  grouped sections); fixed Object Browser's width lock (a
  `QStackedWidget` sizing itself to its largest page - the merged IMG
  tab - regardless of which page was visible); renamed all "Model
  Workshop" labels to "Map Workshop"; added collapsible sections to
  the IMG/IDE/IPL/DAT tabs inside Object Browser (double-click each
  tab's bold title label to collapse/restore its content, matching
  the same interaction already used for dock title bars); fixed 18px
  text clipping and added icon-only collapse (wired to live resize)
  for every action-button row across all 4 tabs (Edit/Save,
  Open/Close/New/Delete, Extract/Add/Del/Rename/Rebuild), via a new
  general-purpose `_register_collapsible_button_row` helper; fixed a
  follow-up bug where switching to a tab (e.g. IMG) didn't re-check
  its row's collapse state, since a `QStackedWidget` page that isn't
  current doesn't reliably react to resize events while hidden -
  `_on_object_browser_tab_changed` now force-refreshes the newly
  shown tab's row right away.

- **Aug 1, 2026 (cont'd)** — Fixed an `ImportError` when opening Map
  Workshop from IMG Factory's DAT Browser (`open_map_workshop` didn't
  exist, only `open_model_workshop` did) by adding it as an alias.
  Extended the "Open" button's file dialog to also accept a GTA
  game's main `.dat` file, routing it through the already-working
  `_load_game_dat_file` (map-loading) logic instead of adding a
  separate button - the actual file-import logic
  (`_load_game_folder`, `_load_game_dat_file`, `_apply_loaded_world`,
  `_load_selected_ipls_with_log`, etc.) already existed from the
  earlier graft, just wasn't wired to any visible UI element yet.
  Found and fixed a second gap: `open_model_workshop`/
  `open_map_workshop` itself (the entry point actually called when
  opening with a specific file path, e.g. from the DAT Browser) only
  routed `.dff`/`.col`/`.img` extensions, never `.dat` - added that
  routing too. Verified the full chain end-to-end (with `QMessageBox`
  mocked to avoid blocking modal dialogs in headless testing):
  `open_map_workshop(main_window, "some.dat")` -> `_load_game_dat_file`
  -> `GTAWorldLoader.load_from_dat` -> `_apply_loaded_world` -> the
  entire UI population chain (Object Browser, IPL Sections, Instance
  List, IDE/DAT/IMG tabs, IPL Inst File panel) all ran without
  crashing, and the per-IPL lazy-load path
  (`_on_ipl_section_cell_clicked` -> `_ensure_ipl_loaded`) was
  confirmed intact and complete. World Viewport panes intentionally
  not wired in for this pass, per Keith - keeping Model Workshop's
  existing DFF viewport, data-only (Object Browser/IPL Sections/etc.)
  is enough for now.

- **Aug 1, 2026 (cont'd)** — Fixed a "weird cycling loop" Keith found
  when actually loading real IPLs live (screenshot confirmed real
  parsed instance data showing correctly in the IPL Inst File panel -
  the core load chain does work). Root cause:
  `_ensure_ipl_loaded`'s own `_preload_world_assets` shows a
  `QProgressDialog` whose `setValue()` calls `processEvents()`
  internally to keep the UI responsive; with `setMinimumDuration(500)`,
  that dialog often never actually becomes visible/modal for a fast
  preload (few models), so `processEvents()` still pumps the event
  queue with no real modal blocking in effect - a queued duplicate
  click event could re-enter `_on_ipl_section_cell_clicked` mid-flight,
  before the first call finished toggling visibility state, producing
  repeated/cycling behaviour. Added a simple re-entrancy guard
  (`self._ipl_cell_click_in_progress`) around the whole method.


- **Aug 1, 2026 (cont'd)** — Fixed a second, deeper bug in the same
  area: `open_map_workshop` was just `= open_model_workshop` (a plain
  alias) - fixed the `ImportError`, but the actual caller
  (`apps/components/Img_Factory/imgfactory.py`'s
  `open_map_workshop_docked`) calls
  `open_map_workshop(self, game_root=game_root, dat_path=dat_path)`,
  keyword arguments `open_model_workshop`'s own signature
  (`dff_path`/`original_dff_name`) doesn't accept at all - so the
  alias still failed, now with `got an unexpected keyword argument
  'game_root'`. Replaced the alias with a real, separate
  `open_map_workshop(main_window, game_root=None, dat_path=None)`
  implementation - docks/opens a `ModelWorkshop` the same way
  `open_model_workshop` does, then routes to `_load_game_folder` or
  `_load_game_dat_file` depending on which argument was given.
  Verified against the exact call signature `imgfactory.py` uses
  (keyword args, both the `dat_path` and no-args cases confirmed
  working end-to-end).

- **Aug 1, 2026 (cont'd)** — Per Keith's question ("are there
  functions missing that we still need to add, not counting the
  viewport, as we're using the existing instead, can you change the
  functions needed... to use the viewport"): ran a systematic audit
  (AST-based - every `self.method()` call in the `ModelWorkshop`
  class checked against every method actually defined on it) rather
  than guessing. First confirmed every `self._world_panes` reference
  (the deliberately-unused Map-specific viewport) is safely
  `getattr`-guarded - all silently no-op, nothing crashes from not
  using it. Then found two genuinely missing methods in active code
  paths for the viewport we ARE using: `_get_ui_color` (called 20+
  times throughout `ModelWorkshop`'s own DFF-viewport painting code,
  but only ever defined on `COL3DViewport`, a different class this
  one doesn't inherit from - every call would have raised
  `AttributeError` the moment any painting happened) and
  `_set_render_mode` (Control Panel's Wireframe Mode toggle called
  this, but it never existed anywhere - `DFFViewport.set_render_mode`
  does exist though, on the actual viewport widget
  (`self.preview_widget`), so added a thin delegating method). Both
  added and verified working. Ten other "missing" methods found by
  the same audit are all either safely guarded, only referenced in
  already-parked/unused code, or gated behind explicit clicks on
  Model-Workshop-specific features (COL level import/export, icon
  display mode, platform scanning) unrelated to Map Workshop's
  loading functionality - pre-existing gaps in the base app, not
  addressed here.

- **Aug 1, 2026 (cont'd)** — Per Keith's screenshot: the right-click/
  Panels menu correctly showed tick marks for every dock, but
  clicking an item didn't actually toggle it ("right click pane
  selection not working"). Root cause: this is the exact same
  regression found and fixed earlier in the session
  (`toggleViewAction()` requires `DockWidgetClosable` to be enabled -
  without it, Qt disables the action entirely, so it shows correctly
  but does nothing when clicked) - it came back when
  `DockWidgetClosable` was later deliberately removed from every
  dock's `setFeatures()` call during the snap-drag debugging (for
  parity with Model Workshop's own docks, which also lack it).
  Verified empirically with a minimal isolated test:
  `toggleViewAction().isEnabled()` is `False` without
  `DockWidgetClosable`, `True` with it. Since `DockWidgetClosable`
  was already conclusively ruled out as a cause of the snap-drag
  issue (compared directly against Model Workshop, which also lacks
  it and still drags/snaps fine), adding it back is safe. Added
  `DockWidgetClosable` back to all 9 active docks (Files/Models/
  Frame Hierarchy/Textures/Object Browser/Instance List/IPL Inst
  File/Editing Panel/World View). Left Control Panel's `setFeatures()`
  call (still commented out) untouched - since it's never called,
  it already gets Qt's full default feature set including
  `DockWidgetClosable` automatically.


- **Aug 1, 2026 (cont'd)** — Per Keith: "the panel list with the tick
  marks to indicate the loaded panes needs work, I should be able to
  hide panels." The View menu already existed (Menu button -> View)
  but only had a "Sort" action - close_btn's tooltip on every dock's
  collapsible title bar promises "use the View menu... to bring it
  back", but there was nothing there to back that up. Added a
  "Panels" submenu under View, dynamically listing every dock
  currently on `_outer_mw` (via `findChildren(QDockWidget)`,
  alphabetically sorted) with its own `toggleViewAction()` - Qt's
  standard built-in mechanism for exactly this (checkable, shows a
  tick mark for visible panels, click to hide/show). Automatically
  reflects whatever docks exist at the time, including Control Panel
  once it's re-enabled.

- **Aug 1, 2026 (cont'd)** — Per Keith: "need to save unticked
  panes." `_restore_outer_layout` had a leftover safety net that
  unconditionally force-showed Files/Models/Frame Hierarchy/Textures
  on every startup, regardless of what was actually saved -
  overriding a hidden dock's state right after `restoreState()`
  correctly restored it. Removed the force-visible block entirely
  (kept the unrelated window-width clamp logic below it) - Qt's own
  `restoreState()` already correctly restores each dock's saved
  visibility on its own, no manual re-application needed. Verified
  end-to-end: hid Object Browser, called `_save_outer_layout()`,
  created a fresh `ModelWorkshop` instance, let its delayed restore
  timer fire, confirmed Object Browser was still hidden (both
  `isVisible()` and the toggle action's checked state).

- **Aug 1, 2026 (cont'd)** — Per Keith: "wire every pane into the
  viewport, when I load ipl, these dont show" - a full multi-instance
  3D world view (confirmed scope, not just single-selection preview).
  `DFFViewport` only ever supported showing one model at a time
  (`set_current_model`); `_draw_assembly`'s multi-geometry path draws
  everything at one shared origin (for a single DFF's assembled
  parts, not positioned world objects). Added real multi-instance
  support to `apps/methods/dff_viewport.py`: `set_world_instances()`
  / `clear_world_instances()` store a list of per-instance geometry +
  transform dicts; `_draw_world_instances()` applies its own
  `glPushMatrix`/`glTranslatef`/quaternion rotation (via
  `glMultMatrixf` and a new `_quat_to_gl_matrix` helper, verified
  mathematically correct against known identity and 90°-rotation
  cases) / `glScalef`/`glPopMatrix` per instance, reusing the
  existing `_draw_wireframe`/`_draw_solid`/`_draw_textured` draw
  calls via the same temp-swap trick `_draw_assembly` already uses;
  `_auto_fit_world()` frames the camera across every instance's
  position (map-scale, not vertex-level detail); `paintGL` checks for
  world instances first. Added `ModelWorkshop._refresh_world_view()`
  (`map_workshop.py`) - converts each distinct model's cached
  `DFFModel` geometry (`self._model_cache.get_geometry`, already
  loaded by `_preload_world_assets` when an IPL loads) into the
  entry format once, reused across every instance sharing that
  model, then hands the whole list to
  `preview_widget.set_world_instances()`. Wired into both
  `_apply_ipl_visibility_filter` (covers every existing
  toggle/LOD-change call site) and `_apply_loaded_world` (the
  initial-load path, which built its own visible-instance list
  separately). Verified end-to-end with mock instance/geometry data:
  correct conversion, correct camera auto-fit distance, and a
  missing/unparseable model correctly skipped rather than crashing.
  Could not verify actual live OpenGL rendering or GTA-specific
  rotation/coordinate conventions (PyOpenGL isn't installed in this
  environment) - needs Keith's visual confirmation with real data
  once pulled.

- **Aug 1, 2026 (cont'd)** — Keith confirmed the multi-instance world
  view works live with real data (screenshot: a real wireframe map
  rendering), but reported bottlenecking when interacting with the
  viewport (rotating/panning during drag), and asked for the loaded
  IPL's models to show in the Models dock with textures shown below
  on selection.

  Performance fix (`apps/methods/dff_viewport.py`): every triangle of
  every instance was being fully re-executed via immediate-mode
  OpenGL (`glBegin`/`glVertex` in a Python loop) on every single
  repaint, including every frame during an interactive camera drag.
  Added a display-list cache (`self._world_display_lists`, keyed by
  `(model_key, render mode)`) - each distinct model's geometry is
  compiled into a GL display list once, then every instance of it
  (however many share that model) just replays the pre-compiled list
  (`glCallList`) - the expensive per-triangle Python/GL work now only
  happens once per model per mode, not once per instance per frame.
  `set_world_instances`/`clear_world_instances` discard old lists
  when replacing the world, freeing GPU memory rather than growing
  the cache across every load. `ModelWorkshop._refresh_world_view`
  now tags each entry with `model_key` (the model name) so instances
  sharing a model correctly share one compiled list.

  Models/Textures panel wiring (`map_workshop.py`): added
  `_populate_models_panel_from_ipl` (lists every distinct model
  referenced by the currently visible instances in the existing
  Models dock's table, one row per model with an instance count) and
  `_on_ipl_model_row_selected` (shows that model's Model Name/IDE/
  ID/TXD info and its actual textures - name/size/format - in the
  Textures dock, pulled from the same `model_cache` already loading
  geometry). Routes through the existing selection handler
  (`_on_compact_col_selected`) via a new `self._ipl_models_mode`
  flag, checked first so the existing DFF/COL browsing behaviour is
  untouched when not in this mode. Both wired into the same
  `_refresh_world_view` call site as the viewport update, so the
  Models panel and viewport always stay in sync.

  Verified end-to-end with mock data: display-list grouping confirmed
  (50 instances of one model correctly share a single `model_key`);
  Models panel population confirmed (2 distinct models -> 2 rows,
  correct instance counts); selection flow confirmed (selecting a
  row correctly updates Model Name/TXD fields and populates the
  Textures table with the right name/size/format, texture count
  label updates too). Full `QApplication` instantiation clean,
  `ast.parse` clean on both files. Could not verify actual live
  OpenGL display-list performance (no PyOpenGL in this environment) -
  needs Keith's confirmation that dragging feels smoother once
  pulled.

- **Aug 1, 2026 (cont'd)** — Per Keith, using real GTA SOL IPL data
  as reference (SA data converted to VC's format across all cities):
  "We need to change how the IPL inst at displayed in the IPL panel
  ... The IPL would need to be in a cells table, so we can highlight
  what we want to change, rename, prefix, suffix names, move X, Y, Z
  cords in batches to any location." Converted the IPL Inst File
  panel from a plain read-only `QTextEdit` to an editable
  `QTableWidget` - one row per instance line, 13 columns (ID/Model/
  Interior/Pos X,Y,Z/Scale X,Y,Z/Rot X,Y,Z,W), multi-selectable
  (`ExtendedSelection`), foundation for the batch rename/prefix/
  suffix/move operations still to come. Cell edits are currently
  display-only - nothing writes back to the actual `.ipl` file yet
  (no write-back infrastructure exists for any file type in Map
  Workshop, a known, already-documented gap - see TODO.md).

  Also added an "Ignore Scaling" checkbox: some converted IPLs (his
  GTA SOL example - SA data converted to VC's format) have a broken/
  placeholder `(0,0,0)` scale in the Scale columns instead of the
  normal `(1,1,1)` unit scale his VC/LC data correctly shows.
  Checking it treats a `(1,1,1)` scale as equivalent to `(0,0,0)` for
  interpretation purposes only - confirmed with Keith this should
  never write anything back to the file ("leaving the ipl
  untouched").

  Verified end-to-end with Keith's own real example data (a
  temporary IPL file built from his `vgncarshow1`/`man_backside`
  lines): correct 13-field parsing, correct row count, and confirmed
  the Ignore Scaling toggle only affects cells that actually show
  `1` (the SA-converted row's already-`0,0,0` scale stayed
  untouched, the VC row's `1,1,1` correctly became `0,0,0`). Full
  `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith's larger multi-part request
  (using real GTA SOL IPL data, plus reference screenshots of a
  MooMapper-style "Item Editor Dialog"): implemented the first,
  lower-risk subset this pass -

  1. INST/CULL/ZON/PATH replaced the vertical `QRadioButton` list
     with compact horizontal buttons + tooltips ("This needs to be
     buttons under IPL sections... with tooltips showing a
     description").
  2. Right-click context menu on the IPL Inst File table: Copy
     (selected cells, tab-separated) and Copy Row(s) as IPL line(s)
     (comma-separated, matching the real file format) to the
     clipboard - "have right-click options to copy to the clipboard;
     we can start to add editing options."
  3. Double-clicking a Model cell in the IPL Inst File table now
     finds the matching real instance, centres the actual active
     viewport (`self.preview_widget`) on it via a new
     `_center_viewport_on_instance`, and opens the existing (already-
     ported) `_InstanceEditPanel` via `_center_on_instance` - "Clicking
     on the model name in IPL Inst File brings up the model in the
     viewport and shows the Object Editor Dialogue." `_center_on_
     instance` previously only updated the unused `_world_panes`
     (this build uses Model Workshop's own DFF viewport instead, per
     an earlier decision), so it never visibly did anything here
     until now.

  While wiring #3, found and fixed two real, pre-existing bugs that
  would have crashed `_InstanceEditPanel` (comprehensive, already-
  ported code - Identity/IDE Info/Position+Rotation nudge controls/
  2DFX/TOBJ sections) the moment it was ever actually used, entirely
  unrelated to anything built today: `QGridLayout` was never imported
  at module level, and `quat_to_euler_degrees`/`euler_degrees_to_quat`
  (used to present an instance's quaternion rotation as editable X/Y/Z
  degrees) were referenced but never defined anywhere - ported both
  from `map_workshop_old_version.py`, where they'd always existed.

  Verified end-to-end with Keith's own real example data (a real
  `IPLInstance` for `vgncarshow1`): double-click correctly finds the
  instance, centres the viewport (`pan_x`/`pan_y` computed correctly),
  and opens a genuinely visible edit panel - confirming both new bugs
  are now fixed and the whole chain works.

  Deferred to a follow-up pass (each a substantial feature on its
  own): double-click picking objects directly in the 3D viewport
  (needs real 3D ray-casting against world instances, not built
  yet); double-click a model in the Models dock jumping to it in the
  viewport; merging the Models panel into IPL Inst File; a texture
  tile view (matching TXD Workshop's style) plus a "Show model
  textures" button in the edit panel; actually binding/rendering
  textures on models in the viewport (currently untextured
  wireframe/solid only); a Validation checklist section in the edit
  panel matching the reference dialog image; and the VC/LC IPL
  display issue, which Keith asked to address last since the above
  changes might resolve it as a side effect.

- **Aug 1, 2026 (cont'd)** — Confirmed by Keith testing against a
  real copy of the PC version of Vice City: the multi-instance 3D
  world view renders objects correctly. **Works on PC version of
  Vice City.**

- **Aug 1, 2026 (cont'd)** — Per Keith: "lets start implementing
  functions like textures on models and texture tiles, shown in
  texture pane." Two related pieces -

  Texture tiles: the Textures dock's table (`self._tex_list`) had
  already been built with `setIconSize(QSize(32,32))` and a 56px-wide
  thumbnail column ready to go, but was never actually populated with
  real thumbnails - `_on_ipl_model_row_selected` only ever set an
  empty string item there. Added `_create_texture_thumbnail`
  (`map_workshop.py`), reusing the exact same approach as TXD
  Workshop's own `_create_thumbnail` ("the same way as in TXD
  workshop") - `model_cache.get_textures()` already returns fully
  decoded `rgba_data` (`parse_txd` handles DXT1/DXT3/DXT5/etc.
  decompression itself), so this just builds/scales a `QPixmap` from
  it, no extra decoding needed. Set as each row's `DecorationRole`.

  Textures on models in the viewport: per Keith, "we need to load the
  textures with the models in the viewport." `DFFViewport._draw_
  textured()` already existed (used for regular single-model preview)
  and looks up textures via a single shared `self._tex_ids` dict
  (texture name -> GL id) - not per-model, so for a whole world of
  different models each needing their own textures, every texture any
  model might need has to be uploaded into that dict before any
  model's display list gets built (the bound texture id is baked into
  the list at compile time). Extended `ModelWorkshop._refresh_world_
  view` to, for each distinct model, look up its TXD via its IDE
  object (same lookup `_on_ipl_model_row_selected` already does),
  fetch its textures from the same `model_cache` already loading
  geometry, collect them all (de-duplicated by TXD), and upload the
  whole batch via `preview_widget._upload_textures()` before pushing
  the world instances - also switches the viewport to `'textured'`
  render mode so `_draw_world_instances` actually uses this path.

  Verified end-to-end with mock data: thumbnail creation confirmed
  (4x4 and 8x8 synthetic RGBA images correctly scaled to 32x32);
  full `_on_ipl_model_row_selected` flow confirmed populating a real
  thumbnail alongside name/size/format; full `_refresh_world_view`
  flow confirmed correct texture de-duplication (2 instances sharing
  one model -> exactly 1 texture upload, not 2), correct render-mode
  switch to `'textured'`, correct instance count pushed. Full
  `QApplication` instantiation clean, `ast.parse` clean. Could not
  verify actual live OpenGL texture binding (no PyOpenGL in this
  environment) - needs Keith's visual confirmation with real data.

- **Aug 1, 2026 (cont'd)** — Per Keith: "the INST CULL ZON PATH
  buttons need to be in there own pane, with [ignore scaling]
  [Generic.txd] [LOD view] the [LOD view] button has 3 toggles,
  [Show All] [Show Norm] [Show LOD] the Generic.txd button should
  load the generic.txd from gta3.img and root/models/generic.txd" -
  confirmed with Keith as a brand new, separate dock (own title bar,
  dockable/movable like every other one), not folded into IPL Inst
  File or Object Browser where these pieces previously lived.

  Added `_create_ipl_controls_dock`: Row 1 is INST/CULL/ZON/PATH
  (moved out of the IPL tab); Row 2 is Ignore Scaling (moved out of
  IPL Inst File), Generic.txd, and LOD view. LOD view maps directly
  onto already-existing, already-working logic
  (`_set_lod_display_mode`) - Show All = `'both'`, Show Norm =
  `'normal'`, Show LOD = `'lod'` - just needed a 3-way toggle menu
  wired to it. Generic.txd tries `model_cache.get_textures('generic')`
  first (which searches every indexed IMG archive - `gta3.img` is
  always auto-indexed for every game, per `GTAWorldLoader.load()`'s
  own docstring: "Always enforces models/gta3.img... so TXD Workshop
  and the Dump TXDs feature can always find it"), confirmed by Keith
  as the right order, then falls back to `{game root}/models/
  generic.txd` as a loose file (parsed directly via `parse_txd`) if
  not found there - `self._game_root` was already tracked from
  earlier loading code, reused rather than re-derived.

  Hit and fixed two mistakes made while extracting this code from its
  old locations: the method definition line for
  `_create_ipl_inst_file_panel` was accidentally dropped during the
  edit that removed its Ignore Scaling checkbox (caught immediately
  by the next instantiation test - `AttributeError: no attribute
  '_create_ipl_inst_file_panel'`); and `QButtonGroup` needed a local
  import in the new method (not available at module level, matching
  the existing pattern elsewhere in this file).

  Verified end-to-end: all 6 docks (Models/Frame Hierarchy/Textures/
  Object Browser/IPL Inst File/IPL Controls) present via
  `createPopupMenu()`; INST/CULL/ZON/PATH buttons and Ignore Scaling
  checkbox both confirmed present in the new dock; Generic.txd tested
  both paths (found via a fake IMG-search model_cache, and the
  fallback-to-loose-file path with a real temp file on disk, correctly
  handling a parse failure without crashing); LOD view menu confirmed
  correct initial state (Show Norm checked, matching the default) and
  correct behaviour on trigger (`Show All` -> `self._lod_display_mode
  == 'both'`). Full `QApplication` instantiation clean, `ast.parse`
  clean.

- **Aug 1, 2026 (cont'd)** — Per Keith: "when selecting the IDE tab,
  the IPL inst file, turns into IDE Objects, and displays the IDE
  entries in cells just like the IPLs." Added `self._ipl_inst_file_
  mode` ('ipl'/'ide') to `_on_object_browser_tab_changed`: selecting
  the IDE tab now updates the shared IPL Inst File dock's visible
  title to "IDE Objects" (needed exposing the custom title bar's
  `QLabel` as `dock._title_label` in `_make_dock_collapsible`, since
  it was only ever set once at construction and never stored
  anywhere reachable) and shows whichever IDE file is currently
  selected in the IDE tab's own list; selecting any other tab
  switches back to "IPL Inst File" and restores the normal IPL
  Sections view (also restoring the table's fixed 13-column schema,
  since IDE mode changes the column count dynamically).

  Added `_refresh_ide_objects_panel`: unlike IPL's INST/CULL/ZONE
  (each always exactly 13 fields), IDE has several different section
  types (objs/tobj/cars/peds/anim/weap/txdp/2dfx/hier, etc.) with
  genuinely different field counts each - rather than building a
  separate fixed schema per section type, every real data line
  becomes its own row with however many comma-separated fields it
  actually has, columns numbered ("Field 1", "Field 2", ...) up to
  the widest row found across the whole file. Section header lines
  and "end" are skipped using the same detection the real IDE parser
  itself uses (`DATParser.parse_ide` - any short comma-free lowercase
  token). `_on_ide_tab_row_clicked` now also triggers this refresh
  when IDE Objects mode is active, matching how clicking an IPL
  Sections row already refreshes IPL Inst File.

  Verified end-to-end with a real mixed-section test IDE file (two
  `objs` entries plus one `tobj` entry): title correctly switches to
  "IDE Objects"; column count correctly comes out as 7 (matching the
  widest row, the 7-field `tobj` entry); all 3 rows correctly parsed
  with section headers/`end` correctly skipped and shorter rows
  correctly padded with empty cells; switching back to IPL mode
  correctly restores both the title and the fixed 13-column schema.

- **Aug 1, 2026 (cont'd)** — Per Keith's screenshot: "IPL secton
  buttons are a perfect size so the IPL control buttons need to be
  the same size." The IPL Controls dock's buttons (INST/CULL/ZON/
  PATH, Ignore Scaling, Generic.txd, LOD view) had been built without
  the 18px compact-button treatment (`setFixedHeight(18)` + the
  padding-stripped stylesheet) already applied everywhere else -
  IPL Sections' Open/Close/New/Delete, Control Panel, all four tabs'
  action rows. Applied the same treatment to all 6 IPL Controls
  buttons/checkbox, matching exactly. Verified: every button in the
  dock confirmed `height() == 18`.

- **Aug 1, 2026 (cont'd)** — Confirmed by Keith: the multi-instance
  world view now renders real Vice City docks geometry with textures
  correctly (screenshot showed cranes, containers, buildings all
  textured properly). Per Keith: "im trying to select a tree double
  clicking on it, so I can see its edit dialog window" - implemented
  double-click-to-select directly in the 3D viewport, the deferred
  item from earlier in the session.

  `apps/methods/dff_viewport.py` already had a complete, working
  ray-picking toolkit built for vertex/edge picking in single-model
  editing mode (`_pick_ray` - unprojects a screen pixel to a world-
  space ray via `gluUnProject`, using the exact same camera transform
  `paintGL` uses; `_closest_point_on_ray`; even a full Möller–Trumbore
  `_ray_triangle_intersect` for later if needed) - all directly
  reusable rather than building picking from scratch. Added
  `_pick_world_instance(mx, my)`, following the same pattern as the
  existing `_pick_vertex`/`_pick_edge`: finds the closest world
  instance's *position* to the ray within a camera-distance-scaled
  tolerance (not full per-triangle mesh intersection - re-testing
  every triangle of every instance on every click would be
  considerably slower across a whole loaded map; distance-to-origin
  is fast and good enough for clicking roughly on/near an object).
  Added `mouseDoubleClickEvent`, calling `_workshop_ref._on_world_
  instance_picked(index)` (the viewport already carries `_workshop_ref`
  back to `ModelWorkshop`, set at construction) when a world view is
  loaded and something was picked.

  `ModelWorkshop._refresh_world_view` now also carries each entry's
  original `IPLInstance` (`entry['instance']`), needed to map a picked
  index back to something `_center_on_instance`/`_show_instance_edit_
  panel` can actually use. Added `_on_world_instance_picked`, reusing
  the exact same centering + edit-panel flow the IPL Inst File table's
  own double-click already uses - one consistent way to select and
  inspect an instance regardless of which panel you click it from.

  Verified end-to-end: the ray-math selection logic itself confirmed
  correct (a ray pointing straight down onto two candidate positions
  correctly picked the one it was aimed at); the full picked-index ->
  real `IPLInstance` -> centering + edit panel chain confirmed working
  with a real `IPLInstance` (viewport pan correctly computed, edit
  panel confirmed genuinely visible); an out-of-range index confirmed
  handled safely without crashing. Could not test the actual
  `gluUnProject` ray generation itself (needs a real OpenGL context,
  not available in this sandbox) - but that's pre-existing,
  already-proven code being reused here, not new. Full `QApplication`
  instantiation clean, `ast.parse` clean on both files.


  Full `QApplication` instantiation clean, `ast.parse` clean.













