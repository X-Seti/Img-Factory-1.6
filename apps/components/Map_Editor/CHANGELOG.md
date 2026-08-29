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

- **Aug 1, 2026 (cont'd)** — Per Keith: "When selecting the object, it
  takes me to the object, which is good, but the zoom in is too
  strong, I have to zoom out alot to see the object, maybe in time we
  need a setting for pick [goto] and zoom values, add todo." Bumped
  the default go-to-instance zoom distance from 15 to 40 as a better
  default for now; added the proper user-configurable pick/goto zoom
  setting to TODO.md, along with two other items from the same
  message that need real design work before building: a snap
  function and a smooth-mesh function ("the biggest problem sometimes
  with making models is sometimes there are gaps, so we need a snap
  function, and a smooth mesh function").

  Also per Keith: "more important when selecting and viewing a single
  object, this should be highlighted in the IPL Inst file list."
  Added `_sync_ipl_inst_file_selection`, called from `_on_world_
  instance_picked` - matches the picked instance's `source_ipl`
  against IPL Sections' rows (switching to and refreshing the right
  IPL first if it's not the one currently shown), then finds and
  highlights the matching row in the IPL Inst File table itself
  (matched by ID + Model name, `scrollToItem`'d into view too).

  Extended the IPL Inst File table's right-click menu (previously
  just Copy/Copy Row(s)) with Info (opens the same edit panel double-
  clicking the Model cell already does) and Show Textures (loads that
  row's model's textures into the Textures dock, reusing the exact
  logic `_on_ipl_model_row_selected` already has). Factored the row ->
  real `IPLInstance` lookup out into a shared `_find_instance_for_ipl_
  inst_file_row`, now used by both the context menu and the existing
  double-click handler (previously duplicated inline). Two more menu
  items Keith asked for - "load into model workshop" and "edit the
  model in map editor" - need their exact intended behaviour
  clarified before building (tracked in TODO.md); Info and Show
  Textures were unambiguous enough to add directly.

  Verified end-to-end: row-highlighting confirmed both for the
  same-IPL case (row correctly selected) and the cross-IPL case
  (IPL Sections correctly switches to the instance's actual source
  IPL first); the shared instance-lookup helper confirmed returning
  the right instance; Show Textures confirmed populating the Textures
  dock correctly (thumbnail, name, size, format, count label) via the
  context-menu path. Full `QApplication` instantiation clean,
  `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith's screenshot (the Object Editor
  Dialog working well, showing real comprehensive data - Identity,
  IDE Info, Position/Rotation with nudge controls, Placement Info,
  2DFX Effects, TOBJ): "When loading the ipls, we also need to
  preload the generic textures first, as these appear white in the
  game ipls, the object exists in gta3.img with everything else."

  His own screenshot's Identity panel confirmed the actual cause:
  `veg_palm04`'s own `Texture (TXD): generic` - but
  `_refresh_world_view`'s automatic per-model texture lookup only
  ever called `model_cache.get_textures(txd_name)` directly (a plain
  IMG-index lookup), without the same IMG-then-loose-file fallback
  the "Generic.txd" button already had (`_get_generic_textures`,
  factored out of `_on_load_generic_txd_clicked` for reuse) - so
  objects whose TXD is specifically "generic" could end up with no
  textures at all if the plain lookup didn't find it, rendering
  white. Fixed two ways: `_refresh_world_view` now unconditionally
  preloads generic.txd first (before the per-model loop), and any
  per-model TXD lookup that's specifically "generic" (case-
  insensitive) reuses the same already-fetched, robust-fallback
  textures instead of a second plain lookup.

  Also per Keith's screenshot: "the buttons in the object info, the
  buttons need to be the same size as the others, like Zon, Cull, and
  so on, all buttons should be uniform." The Item Editor Dialog's
  nudge buttons (the chevron `«` `<` `>` `»` icons for Position/
  Rotation) and its Prev/Next/Close buttons had no fixed height set
  at all, defaulting to Qt's larger natural button size. Applied the
  same 18px `setFixedHeight` used everywhere else in the app.

  Verified end-to-end: with a mock `model_cache` where the plain
  per-model "generic" lookup deliberately fails (simulating the real
  bug), confirmed the texture still gets uploaded correctly via the
  unconditional preload, with no duplicate upload; confirmed every
  button in the Item Editor Dialog (14 nudge buttons plus Close)
  reports `height() == 18`. Full `QApplication` instantiation clean,
  `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith: "its not just generic.txd,
  there are other texture files needed; these are found in
  generic.ide thats called from gta_vc.dat... IDE DATA\MAPS\generic.IDE
  loads those textures into memory... so we'll be looking for
  mine.txd metal.txd, dynphn.txd, dynbarrels.txd, woodpanels.txd,
  boxes.txd and every other texture listed in the .ide... some of
  those names repeat, but we only need 1 of each." The earlier fix
  only special-cased literally "generic.txd" - the real issue was
  broader: any of generic.ide's other referenced TXDs could have the
  same "plain IMG-index lookup alone doesn't always find it" problem.

  Generalized `_get_generic_textures` into
  `_get_txd_textures_with_fallback(txd_name)` (works for any TXD
  name, not just "generic"), keeping the old name as a thin wrapper
  for the button. Added `_preload_generic_ide_textures`, which finds
  every `IDEObject` whose `source_ide` basename is `generic.ide`
  (case-insensitive), collects the distinct set of their `txd_name`s
  (deduplicated - many objects share one TXD, e.g. `bollard`/
  `bollardlight` both use `metal`), and fetches all of them via the
  robust fallback helper. `_refresh_world_view` now calls this
  unconditionally first (replacing the old generic-only preload), and
  also uses the robust fallback (not a plain `model_cache.get_
  textures()` call) for every other model's own TXD lookup too, since
  the underlying issue isn't specific to generic.ide.

  Cached (`_generic_ide_textures_cache`, keyed by `id(loader)`) since
  `_refresh_world_view` calls this on every single visibility toggle
  and generic.ide's own content doesn't change during a session -
  recomputing (re-iterating every loaded IDE object, re-fetching each
  TXD) every time would have been wasted, repeated work.

  Verified end-to-end with Keith's own example data (a mock built
  from his actual generic.ide excerpt - `mine`/`bollard`/
  `bollardlight`/`barrel1`/`barrel2`, plus one `downtown.ide` object
  as a negative control): correctly fetched exactly 3 distinct TXDs
  (`mine`, `metal`, `dynbarrels` - `metal` and `dynbarrels` each only
  fetched once despite 2 objects sharing them), correctly excluded
  the `downtown.ide` object's `citytxd`; full `_refresh_world_view`
  flow confirmed no duplicate upload after fixing an off-by-mistake
  in the initial version (was deduplicating by texture name instead
  of TXD name); caching confirmed working (3 calls -> 1 actual fetch).
  Full `QApplication` instantiation clean, `ast.parse` clean.

















- **Aug 1, 2026 (cont'd)** — Per Keith, using his real `docks.ipl` and
  `generic.ide` (comparison screenshots against MooMapper running the
  same file): "I don't see any indication that the genericide
  textures are being loaded, shown in the status, and those objects
  are still white" and "some of the objects don't align correctly,
  the rotation is off, maybe some values in the IPL are not being
  parsed correctly."

  **Texture status feedback**: added visible status output to
  `_preload_generic_ide_textures` (via `_set_status`, which falls
  back to a console print if no status widget is showing) - reports
  how many `generic.ide` objects were found, how many distinct TXDs,
  and how many were actually fetched. Verified against the real
  uploaded `generic.ide`: correctly found 306 objects across 108
  distinct TXDs (confirming the earlier fix's *logic* is sound - it
  genuinely does find `mine`/`metal`/`dynbarrels`/etc. when given
  real data), and correctly reported a 0-fetched worst case when the
  texture lookup itself was simulated to fail. This should reveal in
  Keith's live environment whether `generic.ide` is loading at all
  (0 objects found would mean it isn't - a separate, upstream issue)
  or whether the fetch step itself is what's failing.

  **Rotation investigation**: verified every layer of the pipeline
  against Keith's real data and found each one correct on its own -
  `detect_game_from_dat_filename('gta_vc.dat')` correctly returns VC;
  `GTAWorldLoader`/`IPLParser` construction correctly propagates that
  game value through; `IPLParser`'s VC-specific 13-field branch
  (interior, position, scale, then quaternion) correctly parses every
  field of the real `docks.ipl` verbatim, including `docks10`'s
  non-identity rotation; and the quaternion-to-matrix conversion
  (`DFFViewport._quat_to_gl_matrix`) was cross-checked against
  `scipy.spatial.transform.Rotation` using that exact real rotation
  value and produced an identical matrix. Could not find the actual
  cause through static analysis alone given everything checked out
  correct in isolation - narrowing this down further needs either
  Keith's live environment directly, or a closer visual comparison of
  which specific objects look misaligned between the two screenshots.

- **Aug 1, 2026 (cont'd)** — Per Keith: "Right-click on the model >
  Show textures brings nothing up nothing, i was expecting to see
  tiles, or something telling me there missing, also showing the IDE
  line" - confirmed the same underlying bug as the earlier generic.ide
  fix (his own words: "No fallback; I think this makes it harder to
  fix"), just hit via a different real example this time (`b_hse_pier`,
  TXD `boathouse`, from `docks.ide` line 127).

  Fixed both `_show_textures_for_instance` (the right-click menu path)
  and `_on_ipl_model_row_selected` (the Models panel path) to use the
  same robust `_get_txd_textures_with_fallback` the generic.ide fix
  already uses, instead of a plain `model_cache.get_textures()` call
  that silently left the table empty with zero indication why. Both
  now show a clear message either way - on success, the texture count
  plus which TXD and where it was found from; on failure, an explicit
  "not found in any indexed IMG archive or as a loose file" message -
  and both now include the requested IDE source/line info (e.g. "TXD
  from docks.ide, line 127").

  Also logged Keith's fuller Item Editor Dialog redesign spec to
  TODO.md - a real header format, showing both raw IPL/IDE lines
  verbatim, editable fields with live viewport sync and write-back,
  Interior/2DFX/TOBJ as buttons, Apply/Undo/Save, and SA section
  support - a substantial roadmap item, not built this pass.

  Verified end-to-end with Keith's own real example (`b_hse_pier`/
  `boathouse`/`docks.ide` line 127): both the not-found case (clear
  message, IDE line shown) and the found case (correct texture count,
  source, IDE line) confirmed working correctly. Full `QApplication`
  instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Redesigned the Item Editor Dialog
  (`_InstanceEditPanel`) per Keith's fuller spec, using his real
  example (`veg_palmkb2`, ID 451, `nbeach.ipl`):

  - Window title now reads `[IPL object editor] ID 451 | veg_palmkb2
    | nbeach.ipl`.
  - Identity section now shows both the raw IPL inst line and the
    matching IDE line verbatim (reconstructed from parsed fields -
    the original file text isn't kept in memory), plus a note on
    which TXD is expected. Verified exact match against Keith's own
    example precision (`-847.8391113` etc., `.10g` formatting).
  - Added a genuinely missing Scale nudge section (Position/Rotation
    already had one via `_add_nudge_section`, Scale never did) plus a
    "Set Scaling to 0" button - both wired the same way Position/
    Rotation already are (live edits, immediate viewport sync via
    `_on_instance_edited`).
  - Placement Info's 2DFX/TOBJ are now `[2DFX (n)]`/`[TOBJ (n)]`
    buttons showing a popup with details on click, instead of
    permanent always-visible text blocks (Interior stays as plain
    text).
  - Bottom row is now `[Apply] [Undo] [Close] [Save]` (previously
    just `[Close]`) - Apply/Undo/Save are honest stubs with a clear
    explanatory popup rather than silently doing nothing, since real
    undo and file write-back are both separate, larger TODO items.

  Deferred to TODO.md (each needs more design work): editable raw
  IPL/IDE line text with write-back and live sync; extensibility for
  SA's additional IDE section types; whether double-click should
  still open this directly now that right-click "Info" covers the
  same ground; and real Undo.

  Verified end-to-end with Keith's own exact example data: window
  title, both raw lines (byte-for-byte match on the IPL line,
  including full precision), TXD note, 2DFX/TOBJ button counts, Scale
  spins defaulting to (1,1,1) and correctly updating to (0,0,0) via
  Set Scaling to 0 (both the spin boxes and the underlying instance)
  all confirmed correct. Full `QApplication` instantiation clean,
  `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith: "when i close all the panes,
  there is noway to bring them back, so I suggest we add them to the
  ribbon right click aswell." The Panels submenu (dynamic dock list
  with tick marks, added earlier) only lived under Menu -> View -
  with every dock closed, right-clicking on any dock to find it isn't
  possible, and the Menu button may not be the first place someone
  looks. Added the same dynamic Panels submenu (reusing the identical
  `findChildren(QDockWidget)` + `toggleViewAction()` logic) to the
  toolbar's own right-click context menu (`_toolbar_context_menu`,
  the one showing Icon Set/Icon Size/Ribbon Manager/Lock-Unlock All
  Toolbars) too - a toolbar is always visible regardless of dock
  state, so this gives a reliable way back even when every single
  pane is closed.

  Verified: closed every dock programmatically, confirmed the Panels
  submenu still correctly lists all 6 (Frame Hierarchy/IPL Controls/
  IPL Inst File/Models/Object Browser/Textures), each unchecked but
  enabled and clickable to restore. Full `QApplication` instantiation
  clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Refined the Item Editor Dialog per
  Keith's follow-up screenshot/feedback, using his real example
  (`washer`, ID 331, `starisl.ipl`, TXD `dynjunk`):

  - Removed the "IDE Info" section entirely (Type/Section/Source/
    mesh_count/draw_dist/flags) - redundant now that Identity shows
    the raw lines directly. Its "Source generic.ide (line 28)" info
    moved into Identity's IDE line instead, appended after the raw
    fields.
  - Identity's 3rd row now reports the TXD's *real* status instead of
    a generic "expected to be loaded" note - one of three messages
    depending on what actually happened: `"{txd}.txd is missing from
    gta3.img"`, `"{txd}.txd is loaded"` (with a `[Show]` button that
    populates the existing Textures dock), or `"{txd}.txd exists but
    can not be loaded"`. Extended `_get_txd_textures_with_fallback`
    to return a proper 3-way status (`'loaded'`/`'missing'`/
    `'failed'`) by checking `model_cache`'s own `_txd_index` directly
    - distinguishing "never indexed anywhere" from "indexed (or a
    loose file exists) but failed to read/parse", which a plain
    textures-or-None result can't tell apart. Updated all 6 existing
    callers of this method for the new 3-tuple return.
  - Position/Rotation/Scale changed from one row per axis (3 rows per
    section) to one row per *section* - X/Y/Z side by side, each
    showing just label + single-step `-`/`+` + value (per Keith:
    "instead show as X <> Y <> Z <> to save space"). The large-step
    («/») buttons are hidden in this mode rather than removed, so
    they're still there to reintroduce later if wanted. Caught and
    fixed a real column-math bug in the first pass (used a
    3-per-axis column stride when each axis actually needs 4 widgets,
    causing the 2nd/3rd axis to silently overlap the 1st's buttons).

  Note: Keith's fuller spec also mentioned "[Show] textures as tiles
  with name as a dropdown" - the tiles part is wired (reuses the
  existing Textures dock), but what a name dropdown should actually
  do here isn't clear yet, left for a follow-up.

  Verified end-to-end with Keith's own real example data: Identity
  correctly shows the raw IPL line, the IDE line with source info
  appended, and all three TXD status messages (tested missing/
  loaded/failed cases individually, confirming the Show button only
  appears in the loaded case); Position grid confirmed correct
  column layout (X/`-`/value/`+`, Y/../.., Z/../.., no overlaps)
  after the column-math fix. Full `QApplication` instantiation clean,
  `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Further refined the Item Editor Dialog
  per Keith's latest screenshot/feedback, using his real example
  (`veg_palwee01`, ID 448, `littleha.ipl`):

  - Made it a real dockable panel, starting floating by default
    ("docLable, but start undocked") - wrapped `_InstanceEditPanel`
    (previously a `Qt.WindowType.Tool` standalone floating widget) in
    a proper `QDockWidget` added to `outer_mw`, with `setFloating
    (True)` so it opens the same way as before unless someone
    explicitly drags it into the main window. Close now hides the
    whole dock rather than just the panel content.
  - Removed the duplicate "Source IPL" line from Placement Info (it
    already appears in the window title) - and since Interior/LOD
    also moved out (see next point), Placement Info as a whole
    section is now empty and gone entirely.
  - Interior/LOD index now sit on the right side of the TXD status
    row instead of their own section ("the Interior [0] and LOD
    index -1 should be on the same row as Generic.txd is load [show]
    ... but from the right on the same row").
  - Fixed unreadable/clipped spin box values: removed the native up/
    down arrow buttons (`QAbstractSpinBox.NoButtons`) since the
    dedicated `‹`/`›` chevron buttons already do that job - freed up
    space that was being spent on redundant arrows, giving the actual
    value text room to display without clipping.
  - Tightened margins/spacing throughout (main layout, each section
    box, the nudge grids) for a more compact overall panel.

  Verified end-to-end with Keith's own real example data: dock
  confirmed created and floating on first show; Placement Info
  confirmed gone; TXD status row confirmed showing "generic.txd is
  loaded" + Show button + "Interior: 0   LOD index: -1" all on one
  row; spin box confirmed using `NoButtons` and correctly displaying
  "-793.59" (matching the real position value) without clipping;
  Close confirmed hiding the dock. Full `QApplication` instantiation
  clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith's feedback (using a real
  screenshot showing his game folder has both "Generic.txd" and
  "generic.txd" as two different files - 348.0 KiB vs 256.4 KiB):
  "since we have another generic.txd, just load them both without the
  fall back, there should be no fallbacks, it should either work or
  fail."

  Removed the loose-file fallback entirely from `_get_txd_textures`
  (renamed from `_get_txd_textures_with_fallback`, since it no longer
  has one) - now looks up a TXD only via the game's indexed IMG
  archives; if that fails it reports `'missing'` or `'failed'`
  cleanly rather than silently trying a second source. The earlier
  fallback design (added to fix white generic.txd objects a few
  passes ago) could have silently picked the wrong one of two
  conflicting same-named TXDs without ever surfacing that a conflict
  existed. Updated all 9 call sites for the renamed method.

  Also per Keith: "In the Identify section, right-click veg_palwee01
  and show the names of the textures from veg_palwee01, Show tex
  names shown in image, and Show Textures would display as [T] [T]
  [T] [T] [T] as small thumbnails in a row" (image was RW Analyze's
  "Texture List for <model>" dialog). Added a right-click context menu
  on the Identity section with two new options:
  - "Show tex names": an RW-Analyze-style table (Texture Name/Alpha
    Name/Req-Incl) listing every texture the model's own geometry
    materials reference, cross-checked against what the TXD actually
    contains - "Req" if only the model needs it, "R&I" if the TXD has
    it too.
  - "Show Textures": a compact horizontal strip of small thumbnails,
    distinct from the full Textures dock table (kept as-is for
    detailed browsing) - a quick visual-only glance.

  Logged "Compare TXD" (a TXD Workshop feature - listing/highlighting
  duplicate-named TXDs like his real Generic.txd/generic.txd case) to
  TODO.md, since it's a different component and a substantial feature
  on its own.

  Verified end-to-end: no-fallback behavior confirmed (an indexed-but-
  failing TXD correctly returns `'failed'` without attempting any
  loose-file lookup); the Req/Incl logic confirmed correct with
  realistic data (a texture present in both the geometry and the TXD
  correctly reports "R&I", one only in the geometry correctly reports
  "Req"); both new dialogs confirmed running without crashing. Full
  `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith's screenshot: "[Show] button
  can't be seen; only showing 3px in height for the font", "the ipl
  values are barely visible", and "3 buttons; hard to see, should be
  3 in a row - space then the 4 buttons under them, I can't tell what
  they are." All three symptoms had one root cause: the dock (made
  floating-by-default last pass) had no explicit size set anywhere -
  a `QDockWidget` briefly added to a dock area right before
  `setFloating(True)` often inherits a tiny constrained size from
  that instant rather than sizing to its actual content, squeezing
  every `setFixedHeight(18)` widget in the panel down well below
  readable size and crowding the Prev/Next and Apply/Undo/Close/Save
  rows against each other.

  Fixed: bumped `_InstanceEditPanel`'s minimum size from 180×(none) to
  380×520 (180 was never enough for the compact X/Y/Z-per-row layout,
  and there was no minimum height at all), and explicitly `dock.resize
  (460, 560)` right after `setFloating(True)` so the floating window
  opens at a sensible size instead of whatever tiny default Qt would
  otherwise give it.

  Verified: dock confirmed at the requested 460×560 on creation, well
  above the panel's 380×520 minimum; the `Show` button confirmed
  reporting a real `height() == 18` with sensible on-screen geometry
  (not collapsed). Full `QApplication` instantiation clean, `ast.parse`
  clean.

- **Aug 1, 2026 (cont'd)** — Per Keith: merged "Set Scaling to 0"
  (previously its own row) with the `[2DFX]`/`[TOBJ]` buttons into a
  single row of three - `[2DFX (n)] [TOBJ (n)] [Set Scaling to 0]`.
  Fixed a `self.` reference bug present in Keith's own draft snippet
  along the way (`_zero_btn.setToolTip(...)` without `self.` would
  have raised `NameError`). Kept the 18px height convention used
  throughout. Verified all three buttons report `height() == 18` and
  Set Scaling to 0 still correctly zeroes the instance's scale. Full
  `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith: "buttons still unreadable, so
  im moving this around. looking at the code on IPL sections [IPL]
  Tab, we need to show buttons in that size, and note that all
  buttons on widgets and panels, are to that standard." Re-examined
  the exact IPL Sections Open/Close/New/Delete button code (the one
  Keith confirmed as "a perfect size" a few passes ago) and found the
  real standard includes more than just `setFixedHeight(18)` + the
  compact stylesheet, which is all earlier passes at the Item Editor
  Dialog had been applying: an 18x18 icon alongside the text too.

  Added `_make_standard_button(text, icon=None, tooltip=None)` - one
  shared, documented helper matching this exact standard, so it's
  applied consistently rather than hand-rolled slightly differently
  each time. Applied it to every button in the Item Editor Dialog:
  Apply/Undo/Close/Save and the Identity section's Show button now
  have real icons (checkmark/undo/close/save/view, from `apps.
  methods.imgfactory_svg_icons` - the same module IPL Sections' own
  icons come from), matching the reference exactly; 2DFX/TOBJ/Set
  Scaling to 0 stay text-only (no clean icon match exists for these
  yet) but still go through the same helper for consistent sizing/
  styling.

  Verified: every button confirmed `height() == 18`; Apply/Undo/
  Close/Save/Show confirmed carrying a real icon; 2DFX/TOBJ/Set
  Scaling to 0 confirmed correctly icon-less but still properly
  sized. Full `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith's screenshot: "the value
  entries need to be 4px wider, and the red lines show unused space,
  so we can remove the emtpy areas" (red lines marking blank vertical
  space within each Position/Rotation/Scale section).

  Widened the value spinboxes from 70 to 74px. The empty space's real
  cause: `QGroupBox` sections had no explicit vertical size policy,
  so with the dock's fixed initial height (560px, set for the taller
  pre-icon layout from a few passes ago) now noticeably taller than
  the actual compact content needs (291px observed), the extra space
  was being distributed across each expanding section rather than
  collecting in one place. Set `QSizePolicy.Fixed` vertically on both
  section box constructors (`_add_section` and `_add_nudge_section`)
  so each sizes tightly to its own content, and added `self._lay.
  addStretch()` at the end of the main layout so any genuinely extra
  space goes to the bottom instead of being spread internally.
  Reduced the panel's minimum height (520 -> 320) and the dock's
  initial resize height (560 -> 400) to match the new compact reality
  - the old values were themselves large enough to force wasted space
  even with the above fixes.

  Verified: spinbox confirmed `width() == 74`; Position section's
  vertical policy confirmed `Fixed`; its `sizeHint()` confirmed a
  tight 50px (matching a single row + margins, no leftover space);
  panel's overall `sizeHint()` confirmed 447x291, comfortably inside
  the new, more realistic minimum/initial sizes. Full `QApplication`
  instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Addressed two of the `#TODO` comments
  Keith added himself while reviewing the dialog:

  "Show textures work but texture names, shows the name texture name
  in all cells" - `_show_tex_names_dialog` now deduplicates required
  textures by name. Multi-LOD models commonly have several geometries
  (high detail, low detail, etc.) that all reference the same texture,
  which was producing one identical row per geometry rather than one
  row per distinct texture - looked like every row/cell showed the
  same name because, for models with few distinct textures, they
  mostly did. Verified with a realistic 2-geometry/1-shared-texture
  case: correctly collapses to 1 row instead of 2.

  "the values in the x, y and z boxes need to be visble" - found the
  real cause: `QDoubleSpinBox`'s own `minimumSizeHint()` is 29px tall,
  but the spinbox was being forced down to the app-wide 18px button
  standard - well under what it needs to render its frame, padding,
  and text comfortably, which is why the value kept looking clipped/
  faint no matter how much the *width* grew in earlier passes (70 ->
  74px). Bumped the whole nudge row (buttons + spinbox) from 18 to
  22px - a real compromise between "compact" and "not fighting the
  widget's own natural minimum." This is specifically a spinbox
  issue, not a general button one - push buttons don't have the same
  natural-minimum conflict at 18px.

  Verified: spinbox confirmed `height() == 22` (up from the clipped
  18, still well short of the fully-comfortable 29 natural minimum,
  but no longer forcing it more than 7px under). Full `QApplication`
  instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith's screenshot: "the values need
  2px added, so the bottom of the text shows, and we can add << >>
  back." Bumped the nudge row height from 22 to 24px. Restored the
  large-step (`«`/`»`) buttons into the compact row (each axis now
  shows label/«/-/value/+/» - 6 widgets instead of 4), which an
  earlier pass had hidden to save space; bumped the panel's minimum
  width (380 -> 560) and the dock's initial resize width (460 -> 620)
  to accommodate the wider rows.

  Verified: spinbox confirmed `height() == 24`; large-step buttons
  confirmed visible and correctly positioned around the value in the
  grid (label/«/-/value/+/» at columns 0-5). Full `QApplication`
  instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith, commented out
  `self._lay.addStretch()` in the Item Editor Dialog (his own local
  edit, applied here to keep the repo in sync) - the stretch had been
  added a few passes ago to push any leftover vertical space to the
  bottom of the panel rather than having it spread across each
  section; Keith removed it, presumably because the section-level
  `QSizePolicy.Fixed` fix from that same pass is enough on its own
  now that the dock/panel minimum sizes were also brought down to
  match the real compact content size.

- **Aug 1, 2026 (cont'd)** — Per Keith: "the world map icons that is
  seen in Dat Browser, when right clicking the dat file, can be used
  for map workshops app icon, shown in the taskbar, when standalone."

  DAT Browser's "Load with Map Workshop…" context menu item uses the
  🗺 emoji as a plain text prefix (`dat_browser.py`) - no actual icon
  file exists for it. Added `ModelWorkshop._make_world_map_icon()`,
  rendering that same emoji onto transparent `QPixmap`s at 5 sizes
  (16/24/32/48/64) via `QPainter`, giving a real multi-resolution
  `QIcon` with the same visual identity. Replaced the leftover
  `SVGIconFactory.mesh_icon` window icon (a carry-over from Model
  Workshop's original branding, unrelated to maps) with it, and also
  set it at the `QApplication` level in the standalone `__main__`
  block, since several Linux desktop environments look there
  specifically for the taskbar/dock icon rather than the window's own
  icon alone.

  Verified: icon confirmed non-null with all 5 expected sizes
  registered; the 32px pixmap confirmed containing real non-
  transparent pixels (340 of 1024, consistent with a rendered glyph
  shape) rather than coming back empty. Could not visually confirm
  the glyph renders as the intended colour emoji specifically (this
  sandbox may lack proper emoji font support that a real desktop
  would have) - worth Keith's visual confirmation once pulled. Full
  `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Three pieces per Keith's latest message:

  **1. Real SVG Earth icon** (replacing the emoji-rendering approach
  from last pass): "needs to be multi color svg, blue background,
  greeny to yellow dithed contenents on the equator, showing Earth.
  There should be no emojis, except in DP5 point." Built a real SVG
  (blue ocean circle, layered green-to-yellow continent blob shapes
  across the equatorial band) and render it via `QSvgRenderer` at 5
  icon sizes, replacing the earlier `🗺` emoji-onto-pixmap approach
  entirely. Verified: 59 distinct colors sampled from the rendered
  32px icon, including clear blue and multiple green/yellow-green
  tones - a real multi-color result, not dependent on the system's
  emoji font support the way the previous approach was.

  **2. Load button in the DAT tab**: "next to [Edit] [Save], there
  should be an option to 'Load' button, left of [Edit] allowing
  standalone to read the root dat file, picking gta.dat (SA)
  gta3_dat (LC) gta_vc.dat (VC) gta_sol.dat/gtasol.dat for [SOL]."
  Added a `Load` button left of `Edit` in `_create_dat_tab`, wired to
  the already-existing `_load_game_dat_file()` - which already opens
  exactly this file-picker dialog (filtered to gta3.dat/gta_vc.dat/
  gta.dat/gta_sol.dat/gtasol.dat/gta_quick.dat) and detects the game
  from the filename, so this needed no new loading logic, just
  exposing the existing entry point from this tab. Works identically
  whether Map Workshop is standalone or docked, since it's the same
  underlying method either way.

  **3. Duplicate-named TXD data loss** ("both Generic.dat, and
  generic.dat should be read and storied in memory so we don't see
  white textures" - read as TXD given the "white textures" context,
  matching his real `Generic.txd`/`generic.txd` case from a few
  passes ago): found and fixed a genuine bug in `ModelCache`
  (`depends/model_cache.py`) causing this - `_txd_index`/`_dff_index`
  stored a single `(img_path, entry)` tuple per lowercase name, so
  indexing two archive entries that only differ by case (his real
  case) silently overwrote one with the other, permanently losing
  whichever indexed first. Changed both indexes to store a *list* of
  entries per name; `get_textures` now merges every duplicate's
  textures together (a texture name already found from an earlier
  entry isn't overwritten, but distinct texture names from other
  duplicates are added in), and `get_geometry` tries each entry in
  order until one parses successfully (geometry can't be merged the
  way textures can, so this just stops one bad duplicate from
  blocking a working one under the same name).

  Verified with a mock 2-archive scenario mirroring Keith's real
  case: two duplicate-named entries with different content (one
  texture shared with different sizes, one texture unique to each) -
  confirmed both distinct texture names end up available, and the
  shared one correctly keeps its first-found version rather than
  either silently disappearing. Full `QApplication` instantiation
  clean, `ast.parse` clean on both files.

- **Aug 1, 2026 (cont'd)** — Two real bugs fixed from Keith's latest
  testing (DAT loading and textures now confirmed working standalone):

  **1. LOD view stubbed for VC**: "Fix display: Show All, Show LOD,
  Show Norm, as this seems to be stubbed." Found the cause -
  `GTAWorldLoader.resolve_lod_pairs()` only ever built pairs for SA/
  SOL (`if self.game not in (SA, SOL): return {}`), since it relied
  entirely on SA's `lod_index` field, which GTA3/VC's inst format
  doesn't have at all - so for VC, `_lod_pairs` was always empty and
  the three display modes had nothing to filter, matching Keith's
  "seems to be stubbed" exactly. Added a second detection path for
  GTA3/VC: an instance whose model name starts with "LOD"
  (case-insensitive) pairs with another instance in the same source
  IPL file whose name matches the remainder (e.g. "LODdock10" ->
  "dock10") AND sits at the same position (within a small tolerance,
  guarding against unrelated objects sharing a name pattern
  coincidentally) - matches the naming convention visible in Keith's
  own earlier docks.ipl data ("LODdock10", "LODks85", "LODks96"
  alongside their normal counterparts).

  Verified with a realistic case mirroring his data: a real pair
  (`dock10`/`LODdock10`, same position) correctly resolved; an
  unrelated same-file LOD-prefixed instance with no matching normal
  counterpart correctly stayed unpaired. Then verified all three
  display modes end-to-end: Show Norm returns just the normal
  instance, Show LOD just the LOD one, Show All both.

  **2. Viewport zooms out when nudging an object**: "When moving an
  object with the object editor... the viewpoint zooms out
  automatically; the viewpoint should stay on the chosen object."
  Found the cause - every nudge calls `_on_instance_edited`, which
  re-applies the IPL visibility filter to keep the World View panes
  in sync, which calls `DFFViewport.set_world_instances()` - and that
  method unconditionally re-fit the camera to the whole map's
  bounding box on every call, fighting whatever position the camera
  had just been navigated to. Added an `auto_fit` parameter, threaded
  through `set_world_instances` -> `_refresh_world_view` ->
  `_apply_ipl_visibility_filter`, defaulting to `True` everywhere
  (preserving the existing fit-on-load/IPL-switch behavior) except
  `_on_instance_edited`, which now passes `auto_fit=False`.

  Verified: set a distinctive camera state (dist=12.34, pan=(-99,
  -88)), triggered an edit, confirmed both values unchanged
  afterward - camera genuinely stays put now. Full `QApplication`
  instantiation clean, `ast.parse` clean on both files.

  Deferred to TODO.md (each a substantial feature on its own): alpha-
  texture rendering, a render-mode toggle (semi-solid/non-textured/
  wireframe) for the world view specifically, gizmo-based free object
  movement (Ctrl+click-drag on X/Y/Z with a lockable axis gizmo), and
  object-to-object snapping while moving.

- **Aug 1, 2026 (cont'd)** — Two more pieces from Keith's request:

  **Alpha-textured objects**: "show any objects with alpha textures,
  as that would display in the game." Found the texture's own alpha
  channel was already being uploaded to the GPU correctly (`GL_RGBA`
  format in `_upload_textures`), but `_draw_textured` only ever
  triggered blending based on *material* face-color alpha, which most
  alpha-textured objects (chain-link fences, foliage, glass) don't
  actually set - their transparency comes entirely from the texture's
  own per-pixel alpha, which was being ignored. Enabled `GL_ALPHA_TEST`
  (`glAlphaFunc(GL_GREATER, 0.5)`) around textured rendering - cutout-
  style transparency (a pixel draws fully or not at all past a
  threshold), chosen over full `GL_BLEND` deliberately since it needs
  no back-to-front sorting and doesn't disturb depth writes. Disabled
  again before the untextured-triangle path, which doesn't have real
  per-pixel alpha to test against and already has its own correct
  material-alpha blend split.

  **Render mode toggle**: "Add the option to show as semi-solid, non-
  textured, wireframe." Added a Render dropdown to IPL Controls
  (Textured/Non-Textured/Wireframe), wired to `DFFViewport.
  set_render_mode` - which already existed and worked, just had no
  world-view-facing control (only ever called once, hardcoded to
  `'textured'`, when a world first loaded). While wiring this, found
  and fixed a real conflict: that hardcoded call ran on *every*
  refresh, including every nudge edit and IPL visibility toggle - so
  picking Wireframe and then editing anything would silently snap
  back to Textured. Added a `_world_render_mode_set` flag so the
  default only applies once per world load, and resets on a genuinely
  new load so a fresh map still defaults to Textured rather than
  inheriting a previous session's choice. Also caught and fixed a
  real bug in the same edit: initially called a `_make_standard_
  button` helper that only exists on the unrelated `_InstanceEditPanel`
  class, not `ModelWorkshop` - fixed to match `ModelWorkshop`'s own
  established button pattern instead (caught immediately by the next
  instantiation test).

  Deliberately did not invent a "semi-solid" mode - what that should
  actually mean (fixed reduced opacity applied globally? something
  else?) isn't clear yet; the three modes with unambiguous meaning
  are wired, holding this one for Keith's clarification.

  Verified: mock render-mode-set test confirmed first load forces
  `'textured'`, a user's subsequent manual pick (`'wireframe'`)
  sticks, and a following edit-triggered refresh no longer resets it.
  Full `QApplication` instantiation clean, `ast.parse` clean on both
  files. Could not runtime-test the actual OpenGL alpha-test behavior
  (no PyOpenGL in this sandbox) - needs Keith's visual confirmation.

- **Aug 1, 2026 (cont'd)** — Per Keith: "When there are binary IPLs,
  these should also be shown in object browser in IPL files | Binary
  IPL as a name column." Added a third "Format" column to the IPL
  Sections table, showing "Binary IPL" for files detected as binary
  format (blank for text) - reuses `detect_ipl_format` (already built
  for `BinaryIPLParser`), reading just the first 64 bytes of each
  file rather than the whole thing.

  Verified with a real text `.ipl` and a synthetic binary one (`bnry`
  magic header): correctly showed "Binary IPL" for the binary file
  and blank for the text one. Full `QApplication` instantiation
  clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Solved the SA Z-rotation misalignment
  Keith discovered and reported with two comparison screenshots:
  "the Z rotation alignment issue - on some lines, it's not reading
  the -90.0, for example. It's only reading 90.0; adding the - fixed
  the issue." Traced this to a real difference in SA's rotation
  quaternion convention versus VC's, using his own real data.

  Found the exact raw line (`LAe.ipl` line 91, from his uploaded
  files): `5533, LODroadB48, 0, 1932.59375, -1782.101563, 12.5, 0, 0,
  0.4516149163, 0.8922129869, -1`. Standard quaternion-to-euler math
  (cross-checked against `scipy` earlier this session, so trusted as
  mathematically correct) converts this to yaw=+53.6deg - but Keith's
  screenshots showed the object only aligns correctly when the Z
  spinbox reads -53.69deg. Working backward: `euler_degrees_to_quat
  (0,0,-53.69)` produces `(0,0,-0.4516,+0.8922)` - the *conjugate* of
  the raw quaternion (negate x,y,z, keep w). Sampled several more
  real instances across his uploaded `LAe`/`LAe2`/`LAhills`/`LAn`/
  `LAs` files to check for a wider pattern; none had non-zero
  `rot_x`/`rot_y` to fully distinguish "negate z only" from a true
  full conjugate, but the conjugate is the mathematically well-
  defined, standard operation (unlike "negate z only", which isn't a
  meaningful operation in general once x/y aren't zero), and matches
  the confirmed case exactly.

  Added `ModelWorkshop._effective_rotation`/`_conjugate_rotation_for_
  game`, applying this conjugate for SA/SOL specifically - not VC/
  GTA3, which Keith already confirmed renders correctly as-is with
  real Vice City data, so this is scoped by game rather than applied
  universally (avoiding any risk of regressing already-working
  behaviour). Used in two places: `_refresh_world_view` (actual
  viewport rendering) and `_InstanceEditPanel`'s Rotation spin boxes
  (`_refresh_rotation_spins`/`_on_rotation_nudged`, so the UI shows/
  edits the same effective value the viewport renders, in both
  directions - a conjugate is its own inverse, so editing the
  effective value and converting back to what's actually stored uses
  the identical operation). Deliberately does NOT touch the stored
  `inst.rot_x/y/z/w` themselves - the Identity section's raw IPL line
  still shows the genuinely verbatim file values.

  Verified end-to-end with Keith's exact real data: effective
  rotation for SA computed as `(-0,-0,-0.4516,+0.8922)`, converting
  to yaw=-53.69deg - an exact match to his manual fix; VC confirmed
  completely unaffected (effective rotation identical to raw); raw
  stored `inst.rot_z`/`rot_w` confirmed unchanged after the fix is
  applied; full UI flow confirmed showing -53.69 in the actual
  Rotation Z spin box. Full `QApplication` instantiation clean,
  `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Two pieces per Keith's follow-up:

  **Binary IPLs embedded in gta3.img**: "the binary ipl's are in the
  gta3.img, i think the paths to these are hard-coded in the exe...
  Count the binary IPL files, display those found in the gta3.img as
  the file names in the object browser." Unlike text IPLs (whose
  paths are listed via IPL directives in the .dat, and discovered
  normally through `loader.available_ipls`), these apparently have no
  `.dat` reference at all - the game's own exe loads them directly.
  Added `_scan_binary_ipls_in_img_archives`, scanning every indexed
  IMG archive's own entry list for `.ipl`-extension files, detecting
  binary format by reading just the first 64 bytes of each (reusing
  `detect_ipl_format`), and adding any found to the IPL Sections list
  under a synthetic stem (there's no on-disk loose file the way a
  regular entry has). The Format column's "Binary IPL" detection now
  checks this set first before falling back to reading a loose file's
  bytes. This is the listing/counting half only, matching Keith's own
  wording - actually loading their instance content is a follow-up
  (`BinaryIPLParser` already accepts raw bytes, so feasible later).

  Verified with a synthetic `gta3.img` containing 2 binary IPL entries
  (using the same real filenames `BinaryIPLParser`'s own docstring
  references - `crack.ipl`, `countn2_stream1.ipl`) plus one non-IPL
  entry: correctly found and counted both, correctly excluded the
  unrelated entry.

  **Column width persistence**: "i'd like to keep track of cell
  widths, so if changed, save them." The new Format column was stuck
  in `Fixed` resize mode (couldn't be resized by the user at all) and
  its width was hardcoded rather than restored from `map_settings`
  the way the IPL File column already was. Changed to `Interactive`
  and restored from `ipl_sections_column_widths` if a saved value
  exists - the existing save handler already covered all columns
  generically, this was purely a restore-side gap.

  Full `QApplication` instantiation clean, `ast.parse` clean.

  **VC rotation question**: "Same issue is in VC, might as well check
  gta3 ipl parsing rotation rendering bug?" Re-verified the exact real
  VC rotation used earlier this session (`docks10`, `docks.ipl`) -
  still matches `scipy`'s independent calculation exactly with the
  standard (non-conjugated) math, same as before. Didn't apply the
  SA conjugate to VC/GTA3 without further evidence, since doing so
  would very likely break this already-verified-correct behaviour
  rather than fix anything - asked Keith for a specific VC object/
  example to investigate properly instead of guessing.

- **Aug 1, 2026 (cont'd)** — Corrected the binary IPL scanning per
  Keith's detailed follow-up explanation: "gta.dat contains entries
  for text-based IPL files... Binary IPLs (streaming files like
  LAe2_stream0.ipl) are not directly listed in gta.dat. Instead, they
  are stored inside the .img archives... The game engine automatically
  links these binary files to their parent text IPL by matching the
  filename prefix (e.g. LAe2 in LAe2_stream0.ipl corresponds to
  LAe2.IPL)." The previous pass treated every binary entry found in
  an IMG archive as its own standalone row, which didn't reflect this
  relationship at all.

  Rewrote `_scan_binary_ipls_in_img_archives`: a binary entry is now
  treated as belonging to an already-known text IPL when either its
  own stem exactly matches the text IPL's stem (not every binary
  entry uses the `_streamN` suffix - `BinaryIPLParser`'s own docstring
  references a real `crack.ipl` sample with no such suffix), or its
  stem matches `{text_ipl_stem}_streamN`. A match gets recorded
  against the *existing* text IPL row (`_ipl_names_with_binary_stream`)
  rather than adding a separate one - the Format column now shows
  "Text + Binary Stream" for those. Only genuinely standalone binary
  entries with no matching text IPL at all still get their own row
  and "Binary IPL" label.

  Verified with a scenario matching Keith's own example exactly: a
  known text `LAe2.IPL` plus a binary `LAe2_stream0.ipl` found in the
  archive, alongside an unrelated standalone binary `crack.ipl` -
  confirmed the stream file correctly associated with `LAe2.IPL`
  (no separate row created) while `crack.ipl` correctly got its own
  row. Full `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith: "also binary ipls might show
  in the gta3.img as LAn2_stream0.ipl, LAn2_stream1.ipl,
  LAn2_stream2.ipl and so on." Upgraded
  `_ipl_names_with_binary_stream` from a boolean set (just "has some
  stream files") to a dict mapping each text IPL's display name to
  the *list* of its matched stream entry names, so the Format column
  can show a real count ("Text + 3 Binary Streams") instead of a
  generic "Text + Binary Stream" - and the actual entry names are now
  tracked, ready for a future loading feature to use.

  Verified with 3 numbered stream files matching Keith's exact
  example (`LAn2_stream0/1/2.ipl`) against a known text `LAn2.IPL`:
  all 3 correctly grouped together, Format column correctly reads
  "Text + 3 Binary Streams". Full `QApplication` instantiation clean,
  `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith's screenshot: "hard to see, i
  can not move the cell width of IPL file, but all I can see is
  test... does it show the name of the img3 file dir LAn2_Stream0.ipl
  naming format?"

  Found the real cause of the unresizable column: the IPL File
  column defaulted to `Stretch` resize mode the first time this table
  is ever shown (before any width has been saved) - Stretch mode
  ignores manual drag-resize attempts entirely, which is exactly why
  dragging its border did nothing. Changed to `Interactive` with a
  sane default width (200px) from the start instead, and widened the
  Format column's own default (70 -> 110px, since "Text + N Binary
  Streams" is longer than the "Binary IPL"-only case it was sized
  for).

  Also answering his question directly: the actual stream file names
  weren't shown anywhere before, just a count. Added a tooltip on the
  Format cell listing every associated stream file's real name (e.g.
  hovering a "Text + 3 Binary Streams" cell now shows
  "LAn2_stream0.ipl / LAn2_stream1.ipl / LAn2_stream2.ipl" on
  separate lines) - and the same for standalone binary entries,
  showing their own exact archive entry name.

  Verified: column 1 confirmed in `Interactive` mode with the new
  200px default; Format column tooltip confirmed listing all 3 real
  stream file names correctly, newline-separated. Full `QApplication`
  instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Two more pieces per Keith:

  **Merged Render/LOD menu**: "Render view should me merged with LOD
  view, labeled as Render: Texture, Non-texture, Semi-Solid,
  Wireframe, Show LOD only, Show Normals, Show Both." Replaced the
  two separate buttons with one "Render" button and one menu
  containing two independent exclusive action groups (render style
  and LOD filter are orthogonal - e.g. Wireframe + Show Both is a
  valid combination), separated by a divider - exact labels and order
  as Keith specified.

  Also implemented Semi-Solid as a real render mode, rather than
  holding off on it as in an earlier pass. Added an `alpha_multiplier`
  parameter to `DFFViewport._draw_solid` - when below 1.0, every
  triangle is forced through the existing alpha-blend path (instead
  of the opaque one) with its alpha scaled down uniformly, giving a
  ghosted/see-through look distinct from Non-Texture's fully-opaque
  flat shading. Wired into all 3 render dispatch points (single-model
  view, DFF assembly view, world-instance display lists).

  Verified: merged button confirmed showing all 7 items in the
  correct order/grouping; Semi-Solid confirmed correctly reaching
  `set_render_mode('semi_solid')` end to end.

  **VC rotation confirmed, fix widened universally**: "the same
  rotation issue is there, 32rot, I had to change to -32 to fix the
  models position, this needs checking." Working the numbers the
  same way as the original SA case: a raw quaternion whose euler yaw
  computes to +32deg only aligns correctly at -32deg - the identical
  z-sign-flip pattern found for SA, just now directly confirmed in
  VC too. The earlier `scipy` cross-check was real and stays correct
  (this codebase's own quaternion math is internally consistent with
  the standard convention) but never actually proved that convention
  matches RenderWare's on-disk one - that was an unwarranted leap
  from "the math is self-consistent" to "VC is fine," which Keith's
  direct report now corrects.

  Removed the SA/SOL-only gate from `_conjugate_rotation_for_game` -
  the conjugate now applies universally, still without touching the
  stored `inst.rot_x/y/z/w` themselves (only rendering and the
  Rotation spin boxes use the effective value, exactly as before).

  Verified with Keith's own real VC data (`docks10`, `docks.ipl`):
  effective rotation now correctly conjugated
  `(-0,-0,-0.1908,+0.9816)`, yaw flips from +22deg to -22deg; raw
  stored `rot_z`/`rot_w` confirmed unchanged. Full `QApplication`
  instantiation clean, `ast.parse` clean on both files.

- **Aug 1, 2026 (cont'd)** — Per Keith: "option to hold control and
  highlight ipl entries, right click load all selected .ipls." The
  IPL Sections table defaulted to Qt's `SingleSelection` (never set
  explicitly), so Ctrl/Shift-click couldn't build a multi-row
  selection at all. Set `ExtendedSelection` + `SelectRows`; clicking
  the eye-icon column still toggles that one row's visibility
  immediately as before, but clicking the IPL File/Format columns
  (what Ctrl/Shift-clicking to build a selection naturally lands on)
  already only selected without any side effect - matched.

  Added `_load_selected_ipl_sections` and a "Load Selected (N)"
  context menu action (shown only when more than one row is
  selected) - reuses the exact same per-row load-toggle a single
  eye-icon click already triggers, applied across the whole
  selection, skipping any row that's already visible (re-toggling an
  already-visible row would hide it, the opposite of "load").

  Verified: selection mode/behaviour confirmed set correctly; Load
  Selected confirmed only toggling genuinely-hidden rows in a mixed
  selection (one already-visible, one hidden) and correctly toggling
  both in an all-hidden selection. Full `QApplication` instantiation
  clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith's multi-part message:

  **GTA3 rotation question**: "rotation bug fixed in SA and VC, do we
  need to look at gta3?" Confirmed `_conjugate_rotation_for_game` no
  longer has any game-specific gate at all (removed in the previous
  pass, applies unconditionally to every game) - GTA3 is already
  covered by the same fix, no separate work needed.

  **Alpha textures re-raised**: confirmed the `GL_ALPHA_TEST` fix
  from an earlier pass is still correctly in place in the code -
  couldn't verify the actual visual result (no PyOpenGL in this
  sandbox), logged as needing Keith's specific feedback in TODO.md.

  **Mouse navigation direction bug**: "moving the mouse left, should
  always reflect moving left in the viewpoint... mouse movement seems
  to switch depending on viewing angle." Found the real cause in
  `DFFViewport.mouseMoveEvent`'s middle-mouse pan handling: `self.
  _pan_x`/`_pan_y` get applied via `glTranslatef` *before* the
  scene's own `glRotatef(yaw,...)` in `paintGL`'s transform chain
  (OpenGL applies transforms to geometry in the reverse of call
  order, so the translate lands on the raw world-space coordinates
  first) - meaning the raw screen-space drag delta was being used
  directly as a world-space offset with zero yaw compensation. "Left"
  only felt consistent from whatever one specific angle the camera
  happened to be at when panning started.

  Fixed by pre-rotating the screen-space delta by `-yaw` before
  applying it - this exactly cancels the scene's own `+yaw` rotation
  once applied, keeping the net pan direction locked to actual
  screen-space regardless of viewing angle. Verified mathematically
  (can't visually test OpenGL in this sandbox): dragging the same way
  at yaw=0/45/90/180/270 all produced the identical net on-screen
  displacement `(2.0, 0.0)` after running the full transform chain.

  Also added a `_mouse_sensitivity` multiplier (applied to both
  rotate and pan deltas) and a "Nav" button in IPL Controls opening a
  small settings popup with a sensitivity slider - per Keith: "need a
  way to toggle these settings, mouse strength, other needed
  settings." Only sensitivity is wired up so far; logged to TODO.md
  that "other needed settings" isn't specified yet.

  Noted in TODO.md: `VehicleViewport` (a separate subclass, used by
  Vehicle Workshop) has its own duplicate `mouseMoveEvent` with the
  identical pan bug, unfixed - out of scope for this session but
  flagged for later.

  Full `QApplication` instantiation clean, `ast.parse` clean on both
  files.

- **Aug 1, 2026 (cont'd)** — Per Keith: "as we're loading both
  Generic files and this works, we don't need to show this in a
  button, this can be replaced as an [Advanced] button." Replaced the
  top-level "Generic.txd" button with an "Advanced" button/menu -
  since the automatic `generic.ide` preloading already handles
  generic.txd loading seamlessly (confirmed working in Keith's own
  testing), manually loading it is now a rare/diagnostic action
  rather than something needing a prominent button. Kept it available
  as "Load Generic.txd Manually" under the new menu rather than
  removing it outright, and this gives a natural home for other
  advanced/less-common options later.

  Verified: Advanced button confirmed present with the menu item
  correctly wired to the existing handler. Full `QApplication`
  instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Continuing through TODO.md per Keith's
  "lets continue adding the rest of whats on the todo list":

  Marked two items as resolved after investigation/confirmation:
  the missing splitter bug (Keith confirmed already fixed), and the
  "18px compact sizing" general clipping concern (audited every
  `setFixedHeight(18)` call in the file - none affect a spinbox
  outside the Item Editor Dialog case already fixed earlier).

  Implemented the "pick/goto zoom settings" item: `_center_viewport_
  on_instance`'s previously-hardcoded `40.0` zoom distance is now
  `self._goto_zoom_distance`, exposed as a spinbox in the same Nav
  settings popup added for mouse sensitivity. Verified: default
  confirmed `40.0`; changing the setting and then calling
  `_center_viewport_on_instance` confirmed the new value is actually
  used (not the old hardcoded one). Full `QApplication` instantiation
  clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Solved "SA trees not showing alpha, VC
  works" - Keith corrected my initial rotation-bug hypothesis: "its
  not a rotation bug, its the alpha layer not working on the SA tree
  models" (with a screenshot showing pale, blocky, uniformly-white
  tree shapes - not what an alpha-cutout failure looks like, which
  would still show the leaf texture's own color/pattern, just without
  the transparent parts cut out; this looked like a texture-load
  failure falling back to untextured white geometry instead).

  Found the real cause: `_preload_generic_ide_textures` only ever
  collected TXDs from objects whose `source_ide` matched
  `generic.ide` literally. Keith's own screenshot's selected object
  showed `Source dynamic2.ide` - SA vegetation objects are defined in
  a different shared IDE entirely, so their TXDs were never being
  preloaded at all, and fell back to untextured white geometry -
  which looks exactly like "alpha not working" but is actually a
  missing texture. Generalized the method to collect TXDs from every
  loaded object regardless of which IDE file it came from, not just
  ones literally named `generic.ide` - same principle as the earlier
  generic.txd-specific fix, now applied to every shared IDE a loaded
  world has.

  Verified with objects spanning 3 different IDE files (`generic.ide`,
  `dynamic2.ide` matching Keith's real example, and a hypothetical
  vegetation IDE): all 3 distinct TXDs now correctly collected, where
  the old filter would have only found the `generic.ide` one. Full
  `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith: "then I need to find where
  the tree textures are being stored." `_get_txd_textures` previously
  returned a generic "an indexed IMG archive (e.g. gta3.img)" string
  regardless of which archive a TXD actually came from, even though
  `model_cache._txd_index` already tracks the real `(img_path,
  entry)` per name (a list, since the duplicate-name merge fix - a
  name can genuinely be indexed under more than one archive). Now
  returns the actual archive path(s), joined with `; ` when there's
  more than one, for all three statuses (found the path for `failed`
  too, since it was found there, just couldn't be parsed).

  Surfaced in the Item Editor Dialog's Identity section - hovering
  the TXD status line ("dynjunk.txd is loaded" etc.) now shows a
  tooltip with the exact archive path(s) it was found in, so which
  specific `.img` a texture (tree TXDs included) actually lives in is
  directly visible rather than needing to guess.

  Verified with a multi-archive scenario: correctly returned both
  paths joined together for a name indexed in two different
  archives. Full `QApplication` instantiation clean, `ast.parse`
  clean.

- **Aug 1, 2026 (cont'd)** — Two pieces per Keith's binary IPL
  follow-up:

  **Format column redesign**: "instead of text + 6, with tooltop for
  LAe_Stream0.ipl, have the first LAe_Stream0 as the first name +5,
  with the tooltop showing the other 5." Changed from "Text + N
  Binary Streams" (generic count) to showing the first stream's own
  name directly plus a "+N" suffix for the rest, sorted for
  predictable ordering (stream0 first) - the tooltip now lists only
  the *remaining* entries, not repeating the one already visible in
  the cell. Verified with 3 stream files: cell correctly reads
  "LAn2_stream0 +2", tooltip correctly lists only stream1/stream2.

  **Actual binary IPL loading**: "we need to be able to load these
  binary IPLs, so right click, show the list, to load from." Added
  `_load_binary_ipl_stream` - reads the entry's raw bytes from its
  actual archive (now tracked as `(archive_path, entry_name)` tuples
  rather than plain names, needed to know which archive to re-open),
  parses via the already-existing `BinaryIPLParser`, resolves each
  instance's `model_name` from the loaded world's own IDE objects
  (binary IPLs only ever encode `model_id`, never a name), merges the
  new instances into `_all_instances`, registers the stream as its
  own normal section (visible, its own row) rather than staying
  folded under its parent's Format-column count, and refreshes the
  view. Wired into the context menu: a "Load Binary Stream" submenu
  listing every associated stream file for a text IPL with one or
  more, or a direct action for a standalone binary entry.

  Verified end-to-end with a real synthetic binary IPL (correct magic
  bytes, header, one instance record): instance count, resolved model
  name, and parsed position all confirmed correct; entry confirmed
  registered as its own visible section; visibility filter confirmed
  re-applied. Full `QApplication` instantiation clean, `ast.parse`
  clean.

- **Aug 1, 2026 (cont'd)** — First TOBJ support, per Keith: "lets
  start support tobj first, with a time switch under Ignore Scaling,
  on the IPL Sections pane."

  Found and fixed a real parsing gap first: `IDEParser._parse_line`
  stopped extracting fields right after `flags` for both `objs` and
  `tobj` sections - `tobj`'s own two extra fields (`time_on`/
  `time_off`, the hour range 0-23 an object is actually visible
  in-game) were being silently dropped entirely, never parsed at all.
  Now extracted specifically for `tobj` entries. Verified: a realistic
  tobj line correctly yields `time_on`/`time_off`; a plain `objs` line
  is confirmed unaffected (no time fields, as expected).

  Added the Time switch (checkbox + hour spinbox, 0-23) right after
  Ignore Scaling in IPL Controls - "Ignore Scaling" actually lives
  there rather than in IPL Sections specifically, likely a naming
  mix-up between the two adjacent docks, so placed it at the concrete
  "under Ignore Scaling" reference point given. When off, every TOBJ
  instance shows regardless of time (unchanged from before this
  feature existed); when on, a TOBJ instance only shows if the
  selected hour falls within its `time_on`/`time_off` range - non-TOBJ
  instances are never affected either way. Chained
  `_apply_tobj_time_filter` into the same visibility pipeline the LOD
  filter already uses.

  Verified with a realistic day/night-lamp scenario (including the
  common overnight-wrap case, e.g. `time_on=20, time_off=6` meaning
  visible 20:00 through 05:59): switch off passes all 3 test
  instances through; hour=22 (night) correctly shows the night lamp
  and the always-visible regular object, hides the day-only one;
  hour=12 (day) correctly does the reverse. UI widgets confirmed
  constructed correctly, spinbox starts disabled and enables when the
  checkbox is checked. Full `QApplication` instantiation clean,
  `ast.parse` clean on both files.

- **Aug 1, 2026 (cont'd)** — Per Keith: "put time and nav under on a
  new line." IPL Controls' Time switch and Nav button had ended up on
  the same crowded row as Ignore Scaling/Advanced/Render/LOD - moved
  both to their own second row (opts_row2) below it. Verified
  widgets still construct and function correctly (checkbox/spinbox
  enable-on-check behavior unchanged). Full `QApplication`
  instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Full time-flow controls, per Keith: "time
  is hard to see, we need a [play] and [stop] and Settings [*] Cog
  for time settings, we need to impliment movement of time, so we can
  see the switching of tobjs on the map."

  Replaced the plain 0-23 hour spinbox with a `QTimeEdit` (HH:MM,
  more readable and gives real sub-hour precision) at 24px height -
  the previous 18px was clipped like every other spinbox found this
  session (spinboxes need more room than the 18px button standard).
  Added Play/Stop buttons wired to a real `QTimer`: Play auto-enables
  the Time switch (playing with the filter off wouldn't show anything
  changing) and starts the timer; each tick advances the simulated
  time by a configurable number of in-game minutes and re-applies the
  visibility filter, so TOBJ switching is now visible live as time
  passes rather than only on manual edits. Added a Settings ("*")
  button opening a popup with two independent rates - in-game minutes
  per tick, and real seconds per tick - giving the "1 min for every
  Second adjustable" flow rate from the existing TODO item, not a
  single fixed ratio.

  Verified end-to-end: `QTimeEdit` confirmed at 24px; Play confirmed
  auto-checking the Time switch and starting the timer; a manually-
  triggered tick confirmed advancing 12:00 -> 12:01; Stop confirmed
  halting the timer; settings popup confirmed running without
  crashing. Full `QApplication` instantiation clean, `ast.parse`
  clean.

  Day/night shading and 2DFX-lighting-at-night are still open,
  logged in TODO.md as needing their own rendering-side design pass -
  genuinely separate from this time-control infrastructure (visual
  lighting/ambient changes, not just which instances show/hide).

- **Aug 1, 2026 (cont'd)** — Fixed LOD Show-Only/Show-Normals doing
  nothing for SA data, per Keith: "when LOD only is set, it still
  loads everything, when Norm is set, it loads the lods aswell,
  filenames for lods, Start of LOD or lod." Same lesson as the
  rotation conjugate fix earlier this session: `resolve_lod_pairs`'s
  two detection strategies (lod_index field vs "LOD" name-prefix
  matching) were mutually exclusive by game, with SA/SOL gated to
  lod_index only - but Keith's own real SA data (`LODroadB48` from
  `LAe.ipl`) always has `lod_index=-1` in practice, and its own model
  name confirms SA uses the same "LOD" prefix convention as GTA3/VC.
  Widened both strategies to run for every game and combine their
  results, rather than being gated by game at all.

  Verified with Keith's real SA example: `resolve_lod_pairs` now
  correctly pairs `roadB48`/`LODroadB48` despite `lod_index=-1`; Show
  LOD only correctly returns just the LOD instance, Show Normals
  correctly returns just the base one - both previously did nothing
  for this data. Full `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Rounded out TOBJ support per Keith's
  "lets complete the tojs, everything needed to get that work":

  The TOBJ popup (Item Editor Dialog's "TOBJ (n)" button) previously
  showed only name/ID/source, missing the actual time range - the
  core info a "timed object" popup should lead with. Now shows
  "visible HH:00-HH:00" for each entry that has `time_on`/`time_off`
  parsed. Verified: a real tobj entry (time_on=20, time_off=6)
  correctly produces "streetlight01 (ID 1234, veg.ide line 42) -
  visible 20:00-06:00".

  Noted (not changed, already correct as-is): `get_tobj_for_model`
  groups by the queried model_id, and each TOBJ IDE entry has its own
  unique model_id (day/night variants of the same real-world object
  are separate IDE entries, linked only by shared placement position
  via separate IPL instances, never a shared model_id) - so this
  mostly surfaces just the current object's own entry rather than an
  actual paired variant, which is expected rather than a bug to fix.
  Confirmed two other pieces already worked correctly without needing
  changes: the raw IDE line shown in the Identity section already
  picks up `time_on`/`time_off` automatically (it iterates the
  parsed object's own `extra` dict, which now includes them since the
  parsing fix); the IDE Objects table in Object Browser reads raw
  file text directly rather than going through the parser, so it was
  already showing every field verbatim regardless.

  Full `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — 2DFX lighting at night, per Keith:
  "lets add the 2dfx support next, showing 2dfx lighting at night."

  Found the real starting gap: 2DFX IDE entries were being parsed as
  a placeholder stub with completely empty `extra={}` - no offset,
  color, or type data at all, which can't support rendering an
  actual light. Implemented real parsing for `effect_type == 0`
  (light) entries: local offset (x,y,z), RGBA color, and best-effort
  `corona_far_clip`/`point_light_range`/`corona_size` (lower
  confidence on the exact field order/count for these SA-specific
  extras beyond the core offset+color+type, since no real 2DFX
  sample data was available to verify against, unlike the rotation/
  LOD fixes earlier this session which had Keith's actual raw lines
  to check). Other effect types (particle, text, roadsign, etc.)
  aren't parsed beyond their own offset/type - not needed for
  lighting specifically.

  Added `DFFViewport.set_2dfx_lights`/`_draw_2dfx_lights` - renders
  each light as a glowing point (`GL_POINTS` with additive blending,
  no depth writes - deliberately simple/reliable over sprite/
  billboard geometry, needs no UV/texture setup). Added `ModelWorkshop
  ._refresh_2dfx_lights`, wired into the same visibility-filter
  pipeline the LOD/TOBJ filters already use (and therefore also into
  every time-flow tick automatically) - collects light-type 2DFX
  entries for currently visible instances, computes each one's world
  position (instance position + its local offset rotated by the
  instance's own quaternion, since a light's offset is defined in its
  owning model's local space and has to rotate with it), and gates
  showing them on "night" (hour >= 20 or hour < 6) only when the Time
  switch is actually on - with it off, lights show regardless of
  time, matching how TOBJ objects themselves behave in that case.

  Verified thoroughly: the offset rotation math cross-checked against
  `scipy` (exact match, same verification standard as the rotation
  bug fixes); full pipeline tested with the Time switch off (always
  shows), on at daytime (correctly empty), and on at night (correctly
  shows, world position confirmed as instance position + rotated
  offset, color/size confirmed passed through). Couldn't visually
  verify the actual OpenGL glow rendering (no PyOpenGL in this
  sandbox) - needs Keith's live confirmation.

  Logged the separate Model Workshop 2DFX *editor* (a different
  component - editing a model's own 2DFX entries, not Map Workshop's
  display of them) to TODO.md as its own substantial feature, not
  started.

- **Aug 1, 2026 (cont'd)** — Fixed a real crash/freeze bug in binary
  IPL loading, plus two follow-on display fixes, per Keith: "there is
  a bug in loading IPL, the app freezes, no dialog status, plus ive
  noticed the ipl's not linked to the other data files. that show as
  just Binary IPL. single binary files should just show as there file
  name. [COL] truthsfarm.ipl failed to load - Unknown IPL:
  img:gta3.img:truthsfarm.ipl."

  Found the exact cause: toggling the eye icon on any binary-sourced
  row called `_ensure_ipl_loaded`, which always tried the *text* IPL
  loading path (`loader.load_ipl_by_name(stem)`) - but a binary IPL's
  stem is a synthetic `"img:<archive>:<entry>"` string, never a real
  entry in `loader.available_ipls` the way a text IPL's stem is, so
  this could only ever fail with exactly the "Unknown IPL" error
  Keith hit. Fixed by detecting the `"img:"` stem prefix and routing
  to the already-existing `_load_binary_ipl_stream` instead. Added
  `self._loaded_binary_ipls` to track which binary IPLs have actually
  been loaded (separate from `loader.loaded_ipls`, which only ever
  knows about text IPLs), guarded in both `_ensure_ipl_loaded` and
  `_load_binary_ipl_stream` itself against double-loading (which
  would otherwise duplicate every instance on a second toggle/right-
  click).

  Also fixed the Format column showing nothing at all once a
  standalone binary IPL was actually loaded (it gets removed from
  `_binary_ipl_names`, "now genuinely loaded, not just listed", which
  was falling through every branch to an empty string) - now checks
  `_loaded_binary_ipls` too, keeping the filename shown either way.

  And per Keith's specific display request: standalone (unlinked)
  binary IPLs previously showed the generic label "Binary IPL" in the
  Format column - now show the file's own name instead, matching how
  linked stream files already display their real names.

  Added "Save Binary IPL as Text..." to the right-click menu (shown
  once a binary IPL is actually loaded) - a diagnostic export writing
  the real parsed instances out as a standard text-format .ipl file,
  so the binary parser's actual output can be inspected/compared
  against a known-good file rather than trusted blindly.

  Verified end-to-end against Keith's exact bug scenario (a
  standalone `truthsfarm.ipl` binary entry, eye-icon toggle): instance
  count and resolved model name both confirmed correct (previously 0
  instances with the Unknown IPL error); double-toggle confirmed not
  duplicating; Format column confirmed showing "truthsfarm" both
  before and after loading; the text export confirmed producing a
  correctly formatted SA-style `.ipl` file. Full `QApplication`
  instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Found and fixed the real root cause
  behind "app freezes, no dialog?" after a binary IPL load that had
  actually succeeded, per Keith: "[COL] Loaded barriers1.ipl: 8
  instances (8 model names resolved) nothing, but a frozen app, no
  dialog? and just noticed [COL] this isnt col workshop."

  `ModelWorkshop` never had a working status bar at all.
  `setup_ui`'s status-bar block was gated on `if hasattr(self,
  '_setup_status_indicators'):` - that method is only ever actually
  defined in TXD Workshop (`txd_workshop.py`), not `ModelWorkshop` or
  any of its mixins, so the check silently evaluated `False` and no
  status widget was ever built. Every single `_set_status(...)` call
  this entire session - every load/preload status message referenced
  throughout this changelog - has only ever printed to the console
  via `_set_status`'s own fallback branch (which is also where the
  confusing "[COL]" prefix came from, a leftover generic label for
  that fallback), never shown anywhere in the actual UI. A real,
  successful load like this one looked exactly like nothing happened
  at all, because nothing *visible* did.

  Fixed by wiring in the existing `_create_status_bar` method
  (previously built but also never called) instead of the dead check
  - it sets `self.status_label`, the first thing `_set_status` already
  looks for. Also cleaned up the `[COL]` fallback prefix to `[Map
  Workshop]`, though that branch should now be unreachable in normal
  operation.

  Found this is a shared, cross-component bug, not unique to Map
  Workshop - Model Workshop and COL Workshop have the identical dead
  `hasattr` check in their own `setup_ui`, logged to TODO.md as
  needing the same fix, not yet applied there.

  Verified: `status_label` confirmed to now actually exist on a real
  `ModelWorkshop` instance (previously didn't); `_set_status` with
  Keith's exact message text confirmed correctly reaching
  `status_label.text()`. Full `QApplication` instantiation clean,
  `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Fixed the IPL Inst File pane showing
  nothing for binary IPLs, per Keith: "loading LAe2.ipl, i can see
  the ipl data below in the ipl inst file pane, when clicking on a
  binary ipl, I should still beable to see the ipl lines aswell,
  loading should be the same behavour as the data ipls. go from gray
  to white."

  Found the cause: `_refresh_ipl_inst_file_panel` only ever read a
  selected IPL's raw text file via `loader.available_ipls` - a binary
  IPL's synthetic stem was never an entry there (there's no raw text
  file to read for one), so this fell straight through to an empty
  table every time, regardless of whether the binary IPL had actually
  loaded successfully. Fixed by detecting a binary-sourced stem
  (`"img:"` prefix), auto-loading it via the already-existing
  `_load_binary_ipl_stream` if not already loaded (matching how
  simply selecting a text IPL "just works" without a separate load
  step - "loading should be the same behavour as the data ipls"),
  then building the table directly from its actual parsed instances
  in `self._all_instances` rather than any file - same 13-column
  layout the text-IPL path already uses, just sourced differently.

  Confirmed the "gray to white" visibility styling already applies
  correctly to binary rows without needing a separate fix - `_style_
  ipl_name_item` is generic/uniform per-row regardless of source, and
  `_rebuild_ipl_sections_rows` (which `_load_binary_ipl_stream` already
  calls) re-applies it for every row on load.

  Verified end-to-end with Keith's own `LAe2.ipl` example: IPL Inst
  File pane went from 0 rows (before) to correctly showing the real
  parsed instance (model ID/name/position/rotation all correct) on
  first click, with no separate load step; instance confirmed
  registered as loaded and visible; hidden-vs-visible foreground
  colors confirmed genuinely different (`#8a8a96` gray vs `#ffffff`
  white). Full `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Found and fixed the real cause of the
  reported freeze, using Keith's own Ctrl+C interrupt traceback (not
  actually a binary IPL parsing issue - the trace showed it hung
  inside `_apply_ipl_visibility_filter -> _refresh_world_view ->
  _preload_generic_ide_textures -> ... -> model_cache._read_entry ->
  IMGFile._open_version_2 -> entry.set_img_file`): `_read_entry` was
  opening its IMG archive fresh on *every single call*, on the stated
  (and here, wrong) assumption that this was cheap. `IMGFile.open()`
  has to parse the archive's entire directory table (potentially
  thousands of entries for `gta3.img`), and `_preload_generic_ide_
  textures` (generalized earlier this session to cover every distinct
  TXD across a whole loaded world, not just `generic.ide`'s own) can
  call `_read_entry` hundreds of times in one preload pass for a full
  map - hundreds of full directory re-parses of the same archive is
  exactly what turns into a multi-minute hang with zero visible
  progress.

  Added `ModelCache._opened_img_files` (archive path -> already-opened
  `IMGFile`), reused by `_read_entry` instead of re-opening every
  time - the expensive directory parse now happens once per archive
  per session. `index_img_files` (which already opens every archive
  once anyway, to build the name indexes) now caches that same opened
  instance too, so the very first texture lookup doesn't even need a
  second open. `clear_indexes` clears this cache too, alongside the
  other per-world caches it already resets.

  Verified directly: 50 simulated `_read_entry` calls against the
  same archive now correctly call `IMGFile.open()` exactly once
  (would have been 50 times before this fix). Full `ast.parse` clean
  on both files.

- **Aug 1, 2026 (cont'd)** — Added recent DAT files, per Keith: "when
  loading Dat files, standalone, remember past files." Added `recent_
  dat_files` to `MapSettings.DEFAULTS` (most recent first, deduped,
  capped at 10), populated by `_add_recent_dat_file` on every
  successful `_load_game_dat_file` call, persisted via the existing
  JSON settings file. Added a separate "Recent" dropdown button next
  to Load in the DAT tab (not attached to Load itself, since
  `QPushButton.setMenu()` would make the whole button open the menu
  on click, overriding Load's own "open file dialog" behaviour) -
  lists recent paths, each re-triggering `_load_game_dat_file` with
  that path preset, plus a "Clear Recent Files" action.

  Verified: menu correctly shows the empty-state placeholder with
  nothing loaded yet; two adds correctly ordered most-recent-first;
  re-adding an already-recent path correctly moves it to the top
  instead of duplicating; clear correctly empties the list. Full
  `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith: "when clicking on a binary
  ipl, i should beable to see this, and have an option to save it to
  name_me.ipl." The IPL Inst File pane already shows a binary IPL's
  data on click (fixed in an earlier pass this session), but the
  "Save Binary IPL as Text" option only lived in the IPL Sections
  table's own right-click menu, not reachable from the IPL Inst File
  table itself, where the data is actually being looked at.

  Generalized the export method (renamed `_save_binary_ipl_as_text`
  -> `_save_ipl_data_as_text`, works identically regardless of
  whether the source was binary or text, since both end up as the
  same `IPLInstance` objects either way) and added "Save IPL Data
  As..." to the IPL Inst File table's own context menu, using
  whichever row is currently selected in IPL Sections. The save
  dialog already lets the user type any filename they want (e.g.
  `name_me.ipl`) - not limited to reusing the source's own name.

  Verified end-to-end: the save dialog correctly suggests the
  source's own name as a starting default, and correctly saves
  whatever path is actually chosen (tested saving as `name_me.ipl`);
  saved content confirmed matching the real loaded instance data.
  Full `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Added "Show" jump-to-line icons in the
  Item Editor Dialog's Identity section, per Keith: "have a show
  icon, before IPL and before IDE rows, that bring up the IPL /IDE
  editors, there it says line (343) highlight the line in either
  editor."

  Added a small Show icon button before each of the IPL and IDE raw
  lines - clicking one switches Object Browser to the matching tab,
  selects that instance/object's own source file in IPL Sections /
  the IDE file list, and highlights + scrolls to the exact row
  matching its real file line number.

  This needed real line-number tracking that didn't exist before:
  `_extract_ipl_section_text` and `_refresh_ide_objects_panel`'s
  parsing loops previously discarded each line's original position in
  the file entirely, only building the displayed table row-by-row in
  order. Changed `_extract_ipl_section_text` to return `(line_no,
  raw_line)` tuples instead of a joined string, and both refresh
  methods now stamp each row's column-0 cell with its real 1-based
  file line number (`Qt.ItemDataRole.UserRole`) - what the new
  `_select_and_scroll_to_line` searches for.

  Added `_jump_to_ipl_line`/`_jump_to_ide_line`, both reusing
  `_on_object_browser_tab_changed` (the existing IMG/DAT/IDE/IPL
  button handler) for the tab switch itself, then locating the
  matching source file and calling the shared line-search helper.

  Verified end-to-end with real 3-line IPL and IDE files: jumping to
  line 3 of each correctly selected the row for the object actually
  on that line (not just any row), and the IDE jump correctly
  switched the shared table into "IDE Objects" mode along the way.
  Full `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Added per-step status visibility for the
  ~2 minute post-parse hang Keith reported: "there is a long hang
  between ipl dialog loading, it reads the last line, then nothing
  for 2 mins, i'd like to know what it's going, paring, sorting,
  something other then think its silently crashed."

  Found `_apply_loaded_world` ran its entire post-parse pipeline
  (resolve LOD pairs, index every IMG archive, apply the LOD filter,
  build the Instance List, refresh the world view, populate IPL
  Sections/Object Browser/IDE/DAT/IMG tabs, refresh IPL Inst File) as
  one unbroken sequence, with only a single status message at the
  very start and zero `QApplication.processEvents()` calls anywhere
  in between - meaning even if a status message had been set for a
  later step, the UI genuinely could not repaint to show it until the
  entire sequence finished, since Qt's single-threaded event loop
  never got control back. Added a status message plus `processEvents
  ()` before each major step, so whichever one is actually slow will
  now show up as a real, visible, stuck-on message rather than
  everything looking identically frozen.

  Reviewed the two most likely culprits for actual algorithmic cost
  given a large map (`resolve_lod_pairs`, widened to a second,
  name-matching strategy earlier this session, and Object Browser/
  Instance List population) - neither showed an obvious O(n²) pattern
  on inspection (LOD pairing is close to linear per source file;
  Object Browser's model properly uses beginResetModel/endResetModel;
  Instance List resolves TXD names lazily per-cell rather than
  upfront). Didn't find a smoking gun to fix directly - the
  instrumentation itself is the more reliable next step, since it'll
  show exactly which real step is slow on Keith's actual data rather
  than requiring a guess.

  Verified the full pipeline end-to-end with a mock loader: every
  expected status message appears in order, confirming instrumentation
  reaches every step. Full `QApplication` instantiation clean,
  `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Four real bugs fixed from Keith's latest
  screenshots/report:

  **1. Garbled/overlapping Identity section text**: the plain "while
  count(): item = takeAt(0); if item.widget(): deleteLater()" pattern
  used to clear the section before repopulating only handled direct
  widget items - `item.widget()` returns `None` for a nested-layout
  item (the IPL/IDE row `QHBoxLayout`s added earlier this session for
  the Show buttons), so those rows' old labels/buttons were never
  actually removed on repeated calls (once per instance selection
  change) - just silently orphaned, still visually parented to the
  section box, piling up underneath/overlapping each newly-added row.
  Added `_clear_layout_recursive` (handles nested layouts properly,
  and `hide()`s widgets immediately rather than relying solely on
  `deleteLater()`, which only schedules destruction for a later event
  loop pass - the old widget stays visible at its old position in the
  meantime otherwise). Verified: cycling between two instances 5 times
  now leaves exactly 4 *visible* labels matching the current
  selection, not accumulating stale ones underneath.

  **2. 2DFX light offset using the wrong rotation**: "2dfx lighting
  isn't parsing correctly, the light spot is fine on some lampposts,
  but wrong on others, an axis issue like the rotation bug." Found
  `_refresh_2dfx_lights` was rotating each light's local offset by
  the *raw* stored quaternion (`inst.rot_x/y/z/w`), while the model's
  own geometry renders using `_effective_rotation`'s *conjugated*
  value - identical for a near-identity rotation (why some lampposts
  looked fine), diverging for any genuinely rotated instance (why
  others didn't) - the exact same lesson as the original rotation bug,
  just missed in this newer code. Fixed to use the effective rotation.
  Verified with a real rotated SA-style instance: the fix produces a
  different (correct) world position than the old raw-quaternion
  version would have.

  **3. Multi-clump models missing most of their geometry**: "streetlights
  are multi clump objects... show broken... traffic lights show in a
  broken state." Found `_refresh_world_view` only ever converted
  `dff_model.geometries[0]` - any multi-part model (separate pole/
  arm/housing parts, each its own `Geometry`) had every part after
  the first silently dropped entirely. Now merges every geometry:
  vertices/normals/uvs/prelit colors concatenated, triangle vertex
  indices and material indices offset per-geometry to stay correct
  in the combined arrays, with white-padding for any geometry lacking
  its own prelit colors when at least one other geometry in the same
  model has them (otherwise a mixed colored/uncolored multi-part
  model would misalign prelit data against vertices). Doesn't apply
  per-atomic/frame transforms between parts - assumes each geometry's
  vertices are already in correct model-local space, which covers
  simple static multi-part props but not models relying on genuine
  frame-hierarchy transforms between parts; noted as a possible
  further refinement if some models still look wrong. Verified with a
  realistic 2-geometry model: combined vertex count correct, second
  geometry's triangle indices correctly offset rather than colliding
  with the first's.

  **4. 2DFX master toggle**: "so lets switch to 2dfx, there needs to
  be a 2dfx button near the time, play, stop." Previously 2DFX lights
  had no way to be fully disabled - with the Time switch off they
  always showed regardless of time; with it on, only the day/night
  gating applied. Added an actual master "2DFX" checkbox next to Time/
  Play/Stop/Settings, checked first in `_refresh_2dfx_lights` before
  any time-based logic - off means no lights show at all regardless
  of the Time switch; on preserves the existing night-gating exactly
  as before. Verified: toggling it off/on correctly clears/restores
  the light list.

  Full `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Two more real bugs, one reported by
  Keith directly with a traceback, one from his own question:

  **Time-flow "freeze" (bug, not a limitation)**: "touching time,
  tick, 12:00 [Play] Appears to freeze things? bug or limitation?"
  Found `DFFViewport.set_world_instances` unconditionally discarded
  every previously-compiled OpenGL display list on *every single
  call* - correct for a genuine new world/IPL load, but this same
  method also runs on every TOBJ time-flow tick and every 2DFX
  toggle via `_refresh_world_view`, none of which change which
  distinct models exist - only which instances of them are currently
  visible. Recompiling every distinct model's display list once a
  second (the default tick interval) for a map with many models is
  exactly the freeze. Added `clear_display_lists` parameter, threaded
  through `set_world_instances` -> `_refresh_world_view` ->
  `_apply_ipl_visibility_filter`, defaulting to `True` (unchanged for
  real loads) but passed `False` from the Time toggle, Time value
  change, 2DFX master toggle, and every time-flow tick specifically.
  Verified: a simulated time-flow tick now correctly reaches
  `set_world_instances` with `clear_display_lists=False`, while a
  plain/default call (a genuine load) still correctly defaults to
  `True`.

  **Lighting crash (`IndexError: tuple index out of range`)**: Keith
  logged a real traceback as a `#TODO` comment - crashed at `ld[3]`
  in `_setup_lighting`, which always expects a 4-element `(x,y,z,w)`
  light direction. Traced to the Light Setup Dialog's live-preview
  handler (`_apply_live`) building only a 3-element `(lx,ly,lz)`
  direction vector from the position picker and assigning it straight
  to `vp._light_dir`, overwriting `DFFViewport`'s correct 4-element
  default and crashing the very next `paintGL` call. Found and fixed
  three more of the same pattern while tracing it: the dialog's own
  cancel-path fallback default, and `_load_viewport_light_settings`
  (which only ever saves/loads `dir_x/y/z` in its JSON config, never
  a `w` component). All four now consistently build 4-element tuples
  with `w=0.0` (directional light), matching `DFFViewport`'s own
  convention. Verified by directly reproducing Keith's exact crash
  with the old 3-tuple (confirmed it raises the identical
  `IndexError`) and confirming the new 4-tuple works correctly.
  Removed Keith's `#TODO bug` comment now that the underlying issue
  is fixed.

  Full `QApplication` instantiation clean, `ast.parse` clean on both
  files.

- **Aug 1, 2026 (cont'd)** — Debugged the continued binary IPL freeze
  Keith reported after the earlier fix, per: "the is still an issue
  with the binary ipl, even if I click on them, everything freezes,
  we need to debug this, or atleast check our parser can decode the
  binary.ipl, and read them, and in time write data to the binary.ipl,
  and make our own binary ipls."

  Found a fresh instance of the same performance bug class already
  fixed once this session in `model_cache._read_entry`: both `_load_
  binary_ipl_stream` (runs when clicking/loading a binary IPL) and
  `_scan_binary_ipls_in_img_archives` (runs on *every* world load,
  scanning every archive for binary entries) always created a fresh
  `IMGFile` and called `.open()` unconditionally - re-parsing the
  entire archive directory table from scratch every single time,
  rather than reusing the already-open archive `model_cache` (or an
  earlier call to either of these same methods) might already have
  cached. Both now check `model_cache._opened_img_files` first and
  only open fresh if genuinely not cached yet, caching what they do
  open for next time. Added status messages + `processEvents()` calls
  around the read/parse steps in `_load_binary_ipl_stream` too, so a
  slow archive read is at least visible rather than silent.

  Added "Verify Binary Parser" - a right-click diagnostic (both for
  standalone binary entries and stream files associated with a
  parent text IPL) that reads and parses one entry exactly like a
  real load does, but never touches `_all_instances` or the world
  view - reports success/failure, instance count, any parser
  errors/warnings, and a preview of the first few instances (model
  ID, resolved name where possible, position). Directly answers "at
  least check our parser can decode the binary.ipl."

  Logged the binary IPL *writer* ("in time write data to the
  binary.ipl, and make our own binary ipls") to TODO.md with the full
  confirmed binary layout from `BinaryIPLParser`'s own docstring
  (magic, header size, per-instance record format) - not started, the
  read side needs to stay proven reliable first per that same
  docstring, and several header fields/other sections' formats aren't
  confirmed yet, which a real writer producing GTA-loadable files
  would need worked out.

  Verified the archive-reuse fix directly: with no cache, a load
  correctly opens the archive once; with the archive already cached
  (matching the real load pipeline, where `index_img_files` runs
  before this), a load correctly reuses it with zero additional
  opens, while instance loading still works correctly either way.
  Verified "Verify Binary Parser" against a real synthetic binary IPL:
  correctly reports success, instance count, the real "cull/zone/
  other sections not yet supported" warning, and a resolved model
  name in the preview. Full `QApplication` instantiation clean,
  `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Found the actual remaining freeze cause,
  using Keith's own real binary IPL files (164 files, including the
  exact `crack.ipl`/`countn2_stream1.ipl` referenced in `BinaryIPLParser`'s
  own docstring) - per: "verify seems to show some, but load, freezes,
  nor can I see the IPL data in the IPL inst file."

  First, tested the parser directly against the real files to rule it
  out: `crack.ipl` parses to exactly 60 instances in 0.0004 seconds,
  `countn2_stream1.ipl` to exactly 355 - both matching the parser's
  own documented claims precisely, zero errors, tested across 5 files
  of varying size (2KB-16KB) with consistent clean results. The
  parser itself was never the problem.

  Found the real cause in `_load_binary_ipl_stream`'s final step:
  `self._apply_ipl_visibility_filter()` was called with no arguments
  at all, defaulting both `auto_fit` and `clear_display_lists` to
  `True`. `clear_display_lists=True` unconditionally discards and
  forces recompilation of *every* already-visible model's OpenGL
  display list, not just the newly-loaded stream's instances - for an
  already-loaded map with many distinct models, that's exactly the
  freeze, the identical bug class already fixed for TOBJ time-flow
  ticks earlier this session, just missed in this specific call site.
  `auto_fit=True` was also re-framing the camera to the whole map's
  bounding box on every single stream load. Fixed by passing
  `auto_fit=False, clear_display_lists=False` here too.

  Also found `_load_binary_ipl_stream` never called `_refresh_ipl_
  inst_file_panel()` at all - explaining "nor can I see the IPL data
  in the IPL inst file": the pane shows whichever row is currently
  selected in IPL Sections, but doesn't automatically re-read after
  new data loads elsewhere, so newly-loaded data stayed invisible
  until a manual re-click. Added the explicit call.

  Verified end-to-end against the real `crack.ipl` through the actual
  loading pipeline: total load time under 10 milliseconds, 60
  instances correctly loaded, `_apply_ipl_visibility_filter` confirmed
  called with `(auto_fit=False, clear_display_lists=False)`, IPL Inst
  File panel confirmed refreshed. Full `QApplication` instantiation
  clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Two more real fixes from Keith loading
  `LAe.ipl`: "I see LOD file in the IPL inst, I have show norm
  selected, so it should ignore any LOD suffick files. however there
  is a ton of memory usage, and a very long delay, between the last
  entry in the ipl, and it finally showing anything."

  **LOD filter missing from the IPL Inst File table**: the Render/LOD
  dropdown's Show Normals/Show LOD only/Show Both setting only ever
  applied to the 3D world view (`_apply_lod_filter`, working on parsed
  `IPLInstance` objects) - the IPL Inst File table (raw text lines
  from the currently selected file) never checked it at all, showing
  every line regardless. Added the same "starts with lod" filtering
  directly on the raw text fields (model name is field index 1),
  matching the convention `resolve_lod_pairs` already uses. Verified
  across all three modes with a real mixed IPL file (roadB48/
  LODroadB48/streetlight1): Show Normals correctly excludes
  LODroadB48, Show LOD only correctly shows only it, Show Both
  correctly shows all three.

  **The freeze, found in its most-used location yet**: `_toggle_ipl_
  section` - triggered by *every single* IPL show/hide toggle,
  including the very first load of any IPL - was calling `_apply_ipl_
  visibility_filter()` with no arguments, defaulting `clear_display_
  lists` to `True`. This is the identical bug already found and fixed
  in the TOBJ time-flow tick, the 2DFX toggle, and the binary IPL
  loader this session - but this call site is the single most
  frequently hit of all of them, likely explaining a large share of
  the freeze complaints throughout this whole session. Audited every
  remaining `_apply_ipl_visibility_filter()` call site in the file (5
  more: add/remove instance placements, the per-nudge edit refresh,
  LOD display mode changes, per-instance LOD overrides) - none of
  them change the underlying model set, only which instances are
  shown or their transforms, so all five now also correctly pass
  `clear_display_lists=False`. The genuine new-world-load path
  (`_apply_loaded_world` calling `_refresh_world_view` directly, not
  through this method) is unaffected and still correctly defaults to
  clearing.

  Verified: `_toggle_ipl_section` confirmed now passing
  `clear_display_lists=False`. Full `QApplication` instantiation
  clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Two more real fixes from Keith's own
  crash trace and follow-up request:

  **DXT1 decode freeze/memory**: Keith's Ctrl+C interrupt landed deep
  inside `_decode_dxt1`'s per-pixel loop, with "memory used, doesn't
  seem to get released." The size guard in `_parse_native_texture`
  already caps dimensions at 4096x4096 (ruling out a runaway
  allocation from corrupted data), but a single large legitimate
  texture at that pure-Python per-pixel loop is still over 16 million
  iterations, and `_preload_generic_ide_textures` can decode many
  distinct textures in one pass. Added a vectorized numpy fast path
  (numpy already a hard dependency of `map_workshop.py` itself, kept
  optional here via try/except to preserve this file's own stated
  "no external dependencies" design goal for other contexts).

  First implementation used fancy-index scatter to place decoded
  pixels into the output array - profiling found this was actually
  *slower* than the original loop at every size tested (0.5-0.9x).
  Rewrote using reshape/transpose/crop instead (valid whenever the
  block count matches the full grid, i.e. the data wasn't truncated -
  falls back to the scatter approach for the rare truncated case) -
  this avoids the random-access memory pattern entirely, genuinely
  2.7x-13x faster than the original loop across 256x256 through
  2048x2048.

  Verified extensively: byte-for-byte identical output to the
  original loop-based decoder (kept as `_decode_dxt1_loop`, used
  automatically as a fallback if numpy is ever unavailable) across
  exact-multiple-of-4 dimensions, non-multiple-of-4 edge-block
  clipping, truncated block data, and both the `c0>c1`/`c0<=c1`
  palette branches. `_decode_dxt3`/`_decode_dxt5` have the identical
  pure-Python pattern and likely the same issue - logged to TODO.md
  rather than rushed through in the same pass.

  **Memory usage on the status bar**: per Keith's follow-up, "i think
  we also need to add a function on the statas bar, to show memory
  usage" - added a right-aligned "Memory: N MB" label, updated every
  2 seconds via a timer. Tries `psutil` first (not a pre-existing
  dependency, so kept optional), falls back to reading `VmRSS`
  directly from `/proc/self/status` on Linux, clears the label if
  neither works rather than showing a wrong value. Also cleaned up
  genuinely dead/broken leftover code in the same method (referenced
  an undefined `col_data` variable, guarded by a condition -
  `status_info` - that's never actually set anywhere).

  Verified: label confirmed showing a real reading ("Memory: 130 MB")
  immediately on construction, update timer confirmed active. Full
  `QApplication` instantiation clean, `ast.parse` clean on both files.

- **Aug 1, 2026 (cont'd)** — Fixed the memory usage label to actually
  be cross-platform, per Keith: "the memory fix would need to work on
  any platform the user runs this app on." Previous fallback (when
  psutil isn't installed) read `/proc/self/status` directly - Linux-
  only, so a Windows or macOS user without psutil would see nothing
  at all.

  Now a proper 3-tier fallback, standard-library only past the first
  tier: psutil if actually installed (most accurate); `resource.
  getrusage(RUSAGE_SELF).ru_maxrss` (stdlib, covers both Linux and
  macOS with no install needed on either - note this is *peak* RSS,
  not live, so it only ever climbs, but that's still the useful
  signal for spotting a freeze/leak, just won't drop back down after)
  - correctly handles the platform-specific unit difference (Linux
  reports KB, macOS reports bytes, decided via `sys.platform`); a
  `ctypes` call to `GetProcessMemoryInfo` via `psapi.dll` (stdlib via
  ctypes) for Windows, where `resource` doesn't exist at all. Clears
  the label only if every tier genuinely fails.

  Caught and fixed a real bug in my own first draft while testing:
  both branches of the Linux/macOS unit conversion were accidentally
  identical, silently defeating the whole platform check - found by
  actually running the fallback path rather than trusting the code
  by inspection.

  Verified on this Linux environment: both the psutil path and the
  stdlib fallback path (simulated psutil-not-installed) produce
  sensible, close readings (130 MB vs 135 MB) confirming the KB->MB
  conversion is correct. Verified the Windows `PROCESS_MEMORY_
  COUNTERS` ctypes struct definition is syntactically valid and
  matches the documented layout - the actual `GetProcessMemoryInfo`
  syscall itself couldn't be executed from this Linux sandbox, so
  that specific path is unverified end-to-end. Full `QApplication`
  instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Investigated a blank Map Workshop window
  with "QOpenGLWidget: Failed to create context", per Keith: "we have
  a blank window in the last push." First confirmed neither of the
  last two pushes touched anything OpenGL-related at all - the entire
  diff across both was the status bar memory label (a `QLabel` +
  `QTimer`) and DXT1 decoding math, neither of which creates widgets
  or touches GL/surface-format code.

  Found a real, plausible pre-existing cause instead: both `DFFViewport`
  and `MapViewport` (the actual viewport classes, `MapViewport` used
  for each world pane in `ModelWorkshop`'s multi-pane layout) set their
  OpenGL format via a *module-level* `QSurfaceFormat.setDefaultFormat`
  call, executed once at import time. Per Qt's own documented
  requirement, this only reliably takes effect if it runs *before*
  `QApplication` is constructed - true for Map Workshop's own
  standalone `__main__` path, but Keith confirmed Map Workshop runs
  *embedded as a tab inside IMG Factory's own main window* - meaning
  IMG Factory's `QApplication` already exists before these Map
  Workshop modules are ever imported, making the module-level call too
  late to reliably apply.

  Fixed both classes to also call `self.setFormat(_fmt)` directly on
  each widget instance in `__init__` - this works correctly regardless
  of import-order timing, removing the dependency on when `QApplication`
  happens to be constructed relative to module import entirely.

  Verified the format-setting fix itself directly: constructed
  `QApplication` first (matching Keith's embedded scenario exactly),
  then imported both viewport modules afterward - both widgets
  correctly receive the expected profile/version. Could not fully
  verify actual context creation succeeds end-to-end, since this
  sandbox has no real GPU/display server either (reproduced the
  identical "Failed to create context" message here too, confirming
  it's a genuine no-GPU-available condition rather than a crash) -
  Keith's own machine is the real test. Full `QApplication`
  instantiation clean (app doesn't crash even when context creation
  fails, it renders blank rather than erroring out - matching the
  reported symptom exactly), `ast.parse` clean on both files.

- **Aug 1, 2026 (cont'd)** — Found the actual, definitive root cause
  of the blank-window/context-failure issue, using Keith's terminal
  log this time (not just the screenshot): "QRhiGles2: Failed to
  create QRhi" appearing *before* the repeated "QOpenGLWidget: Failed
  to create context" lines.

  `map_workshop.py` (and every other individual workshop module -
  `model_workshop.py`, `vehicle_workshop.py`, `col_workshop.py`, and
  others) already sets `os.environ['QSG_RHI_BACKEND'] = 'opengl'` at
  its own module level, specifically intending to force the desktop
  GL backend and avoid exactly this GLES2 failure - but this only
  actually works if that line runs before *any* `QApplication` is
  constructed anywhere in the process. Every one of these workshops is
  opened as a tab from inside an already-running IMG Factory - by the
  time a workshop module is actually imported (when the user opens
  that tab), IMG Factory's own `QApplication` already exists and has
  already selected and locked in its RHI backend, so the workshop's
  own env var setting was always too late to have any effect. This is
  the same root architectural issue as the earlier `QSurfaceFormat`
  timing fix, just for a different (RHI/Qt Quick, not classic
  `QOpenGLWidget`) subsystem.

  Fixed at the actual right place this time: `launch.py`'s own
  `configure_display()`, called at the very start of `main()` before
  `launch_imgfactory()` does anything else (including importing
  `imgfactory.py` itself) - the one place in the entire process
  genuinely guaranteed to run before any `QApplication` construction
  anywhere. Sets `QSG_RHI_BACKEND=opengl` unconditionally (respecting
  an already-set value, matching the existing pattern for `QT_QPA_
  PLATFORM`/`DISPLAY` in the same function) rather than leaving it to
  each individual workshop module's own too-late attempt.

  This is a second, related fix alongside the earlier per-instance
  `setFormat()` change (`DFFViewport`/`MapViewport`) - that one
  addressed classic `QOpenGLWidget` context creation specifically,
  this one addresses the separate Qt Quick/RHI backend selection the
  terminal log actually showed failing first. Together they should
  cover the most likely causes, though this still can't be verified
  end-to-end without Keith's own GPU/driver setup - `launch.py`
  parses cleanly and the fix is placed correctly, but the real test
  is whether Map Workshop actually renders on his machine now.

- **Aug 1, 2026 (cont'd)** — Reverted the status bar memory usage
  feature entirely (both the initial addition and the follow-up
  cross-platform fix), per Keith's request: "revert back to the last
  fix, before where I said 'I think we also need to add a function on
  the statas bar, to show memory usage.'" - part of isolating what's
  actually causing the blank-window/context-failure issue by removing
  the most recently added piece to test against a simpler baseline.

  `_create_status_bar` restored to a plain "Ready" label only, no
  memory display/timer; `_update_memory_status_label` and `_get_
  memory_mb_stdlib` removed entirely. The DXT1 vectorization fix
  (same commit as the original memory bar addition) is deliberately
  kept - that's a separate, already-verified fix for a different real
  crash, not part of what Keith asked to revert. The GL context timing
  fixes (`DFFViewport`/`MapViewport` per-instance `setFormat()`,
  `launch.py`'s `QSG_RHI_BACKEND` fix) are also kept, since diff review
  already confirmed neither touches anything related to the memory bar.

  Worth noting: even with the memory bar completely removed, this
  sandbox's own instantiation test still shows "QOpenGLWidget: Failed
  to create context" (expected here specifically, since this sandbox
  has no real GPU either) - additional evidence, on top of the earlier
  diff review, that the memory bar was never the actual cause. Full
  `QApplication` instantiation clean, `ast.parse` clean, no dangling
  references to the removed memory-bar code anywhere in the file.

- **Aug 1, 2026 (cont'd)** — Reverted the GL context timing fixes too
  (`DFFViewport`/`MapViewport` per-instance `setFormat()`, `launch.py`'s
  `QSG_RHI_BACKEND` change), per Keith: "maybe revert back on more;
  blank screen" - the window is still blank on his machine even with
  those fixes in place, plus a new terminal log showing the same
  "QOpenGLWidget: Failed to create context" repeated 4 times, then a
  `KeyboardInterrupt` landing on `eventFilter`'s bare `def` line (same
  arbitrary-landing-spot pattern as the earlier interrupt that landed
  on `_update_memory_status_label`'s `def` line before that function
  was ever entered) - consistent with the interrupt arriving while
  genuinely stuck in native/GL code, not in any particular Python
  function.

  All three files (`launch.py`, `apps/methods/dff_viewport.py`,
  `apps/components/Map_Editor/depends/map_viewport.py`) restored to
  their exact state as of commit `66134bd` (before any of this
  session's GL-related changes). This is a diagnostic step, not a
  claimed fix - those changes addressed two real, genuine Qt timing
  requirements (documented, verifiable from Qt's own behavior), but
  neither actually resolved the blank window on Keith's real hardware,
  so they're stripped back out to get to the simplest possible
  baseline for isolating what's actually happening. The persistent
  "Failed to create context" appearing identically in this sandbox's
  own test both before and after every one of these GL-related changes
  (this sandbox has no real GPU at all) reinforces that whatever is
  actually failing on Keith's machine likely needs to be diagnosed
  through what the terminal itself is willing to show, rather than
  more speculative code changes.

  Verified: all three files parse cleanly, no leftover references to
  the reverted code, full `QApplication` instantiation still clean.

- **Aug 1, 2026 (cont'd)** — Found and fixed a real, high-impact
  performance bug behind the "long pause after loading closes" and
  large memory jump Keith reported: "loading LAe.ipl sent the memory
  from 5.6Gb to 8.2Gb interesting... once the loading dialog closed, a
  long pause, thinking it was frozen, becuase the IPL file is
  displayed, the long pause was about 2 mins."

  `_refresh_world_view`'s geometry conversion (`DFFModel` ->
  vertex/normal/uv/triangle/material Python lists the viewport
  actually draws from) used a plain local dict, rebuilt fresh on
  *every single call* to this method - not just when a new IPL loads,
  but on every IPL show/hide toggle. `model_cache.get_geometry()`
  itself was already cached (DFF parsing wasn't repeated), but the
  conversion step was rebuilt from scratch every time regardless, for
  every distinct model currently visible in the *whole* world, not
  just whatever was actually just toggled - exactly explaining both
  symptoms together: the multi-minute pause (rebuilding full vertex/
  triangle lists for potentially hundreds of already-converted
  models) and the multi-GB memory jump (each rebuild allocates a
  fresh full set of these lists, on top of whatever hadn't been
  garbage-collected from the previous rebuild yet).

  Made this a real persistent cache on `self` instead
  (`_geometry_conversion_cache`), surviving across calls - only
  cleared in `_apply_loaded_world` when a genuinely new world loads
  (a new game/DAT means the same model name could refer to entirely
  different geometry), matching the same "toggling visibility never
  needs a full rebuild, only a real new load does" reasoning already
  applied to the display-list cache earlier this session.

  Verified directly: 3 repeated calls to `_refresh_world_view` for the
  same instance now correctly call `get_geometry()` only once (would
  have been 3 times before); a stale cache entry is confirmed cleared
  after simulating `_apply_loaded_world`'s new-world-load path. Full
  `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Added progress visibility to the "long
  pause" phase itself, per Keith: "seems the long parse is loading
  assets, maybe we should at that to the dialog, model and texture,
  so we know its doing something."

  `_preload_world_assets` already shows model/texture/IPL name in its
  own progress dialog, but that dialog closes *before* `_refresh_world
  _view`'s geometry conversion loop runs - which was the actual "long
  pause" fixed earlier this session (previously rebuilding every
  model's vertex/triangle data from scratch on every call; now cached,
  but the *first* conversion of any genuinely new model still takes
  real time and previously had zero status feedback at all). Added a
  status message ("Loading model: {name} (texture: {txd})...") right
  where a model is about to be converted for the first time, with
  `processEvents()` so it's actually visible rather than only being
  set right before a long block of work - already-cached models (the
  common case after the first load) skip this and stay silent/fast,
  since only genuinely new models take real time.

  Verified: a first (uncached) call correctly shows the model+texture
  status message; a second call for the same, now-cached model
  correctly shows nothing (matching the "silent when already fast"
  intent). Full `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Found and fixed a major, foundational IDE
  parsing bug using Keith's real uploaded `LAe.ide`: "So the draw
  distance is higher than 300 to be an LOD... 5454, laeLODds03,
  lod2lae1, 1500, 0."

  `objs`/`tobj` parsing had assumed field 3 was always a "mesh count"
  determining how many draw-distance fields follow (`id, model, txd,
  meshCount, dist1[, dist2], flags`) - checked against Keith's entire
  real file (293 objs/tobj lines) and found this format never
  actually occurs there at all: every single `objs` line is exactly 5
  fields, every `tobj` line exactly 7 - `id, model, txd, drawdist,
  flags[, time_on, time_off]`, with no count field whatsoever. The old
  assumption meant every real draw distance (e.g. `150` in a normal
  entry, `1500` in Keith's LOD example) was being read as a bogus
  "N meshes" count, and the real flags value read as a bogus draw_dist
  of 0 - exactly backwards, and exactly why draw-distance-based LOD
  detection couldn't have worked against the old parsing at all.

  Rewrote to match the verified format directly for the common 5/7-
  field cases, with the old mesh_count-chain logic kept only as a
  defensive fallback for an unrecognized field count (no confirmed
  real-world evidence it's ever actually used, but not removed
  outright in case some other game/file genuinely needs it).

  Verified against Keith's own exact real lines: `draw_dist` now
  correctly reads `1500.0` for his two LOD examples, `150.0` for a
  normal (non-LOD) entry from the same file (well under his suggested
  300 threshold), and the `tobj` line correctly extracts drawdist/
  flags/time_on/time_off together. Full `QApplication` instantiation
  clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Added draw-distance-based LOD detection,
  per Keith's real LAe.ide data: "they dont follow the same pattern as
  those prefixed as LODelname... the draw distance is higher than 300
  to be an LOD... So we need a setting: detect LOD by draw distance
  higher than 300."

  His own `laeLODds03`/`laeLODds04` examples don't match the "LOD"
  name-prefix convention `resolve_lod_pairs` relies on at all (LOD
  sits mid-name, not as a prefix) - and likely have no matching
  "normal detail" counterpart to pair against by name+position either,
  so the existing pairing model doesn't fit them. Added `lod_draw_dist
  _threshold` to `MapSettings` (default 300.0, configurable rather
  than hardcoded) and a new `_is_lod_by_draw_distance` check, applied
  as a separate pass in `_apply_lod_filter` (world view) alongside the
  existing name-pair logic, for any instance not already covered by
  pairing - filtered by the same global Show Normals/LOD only/Both
  mode, just without a specific counterpart to substitute in for it.
  The IPL Inst File table's own LOD filter (added earlier this
  session, name-prefix only) got the identical draw-distance check
  too.

  Verified against Keith's exact real data end-to-end in both places:
  `laeLODds03` (draw_dist=1500) correctly detected as LOD despite the
  name mismatch, correctly excluded from Show Normals and correctly
  the only entry under Show LOD only, while a normal object from the
  same file (draw_dist=150) is correctly unaffected. Full
  `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Added the real-time LOD test tool, per
  Keith: "i'd like to add a model switching test where there is a
  circle around the mouse pointer, size 300, anything in the circle
  is normal models, everything outside is lod. in realtime."

  Added `MapViewport._unproject_point`/`_screen_to_ground_position` -
  a general ray-plane intersection (inverse of the existing
  `_project_point`, using the same explicit numpy view/projection
  matrices) that finds the world-space ground position under the
  mouse cursor, working correctly across all view modes (Top/Side/
  Front/Perspective) rather than special-casing the ortho Top view.
  Verified via round-trip tests (project a known world point to
  screen, unproject back, confirm it matches) for both ortho and
  perspective camera modes - exact match in both.

  Added a circle overlay (`_draw_lod_test_circle`, radius = the same
  `lod_draw_dist_threshold` setting from the earlier draw-distance
  LOD detection work) drawn at that ground position, updated on every
  mouse move via a new callback (`set_lod_test_callback`, mirroring
  the existing `set_pick_callback` pattern - MapViewport doesn't need
  to know anything about LOD pairing/detection itself).

  Added a "LOD Test" toggle next to the Render/LOD dropdown in IPL
  Controls. While active, `ModelWorkshop._on_lod_test_mouse_moved`
  re-filters the currently visible instances by live distance from
  the cursor's ground position on every move: an instance with a
  resolved LOD pair switches between its normal and LOD version
  depending on whether it's inside or outside the circle (genuine
  model switching, not just show/hide); a standalone LOD-type
  instance (draw-distance or name based, no paired counterpart to
  switch to) only shows when outside. Pushes straight to `_refresh_
  world_view` rather than the full visibility-filter wrapper, since
  that would re-apply the global Show Normals/LOD only/Both mode and
  undo the per-position switching.

  Verified end-to-end: toggle correctly wires/unwires the callback
  and pulls the radius from settings; a standalone LOD instance far
  from the test point and a normal instance near it both correctly
  show together; most directly, an instance with a real LOD pair
  correctly switches from its normal version to its LOD version as
  the simulated mouse position moves from inside to outside the
  circle. Full `QApplication` instantiation clean, `ast.parse` clean
  on both files.

- **Aug 1, 2026 (cont'd)** — Fixed a real crash Keith hit immediately:
  "AttributeError: 'DFFViewport' object has no attribute
  'set_lod_test_callback'." The earlier LOD Test implementation only
  added the callback/circle/unprojection methods to `MapViewport` -
  but `preview_widget` (what the toggle actually wires up to) is a
  `DFFViewport`, a different class entirely with its own camera
  system (raw OpenGL matrix-stack calls, not `MapViewport`'s explicit
  numpy matrices).

  Added the same capability to `DFFViewport` directly, reusing its
  already-existing `_pick_ray` (built for sub-object picking, already
  replicates `paintGL`'s exact camera transform via `gluUnProject`)
  rather than building a parallel matrix system from scratch - lower
  risk than the `MapViewport` implementation, though this also means
  the ground-plane intersection math couldn't be independently round-
  trip-verified the way `MapViewport`'s was, since `_pick_ray` needs a
  real GL context to query matrices via `glGetDoublev`, unavailable in
  this sandbox. `DFFViewport` works in GTA's native Z-up space
  directly (unlike `MapViewport`'s Y-up conversion), so the ground
  plane and circle are drawn in Z/XY terms here instead of Y/XZ.

  Also: per Keith, "click it when nothing is loaded, just to see" -
  confirmed the toggle is already safe with no world loaded (both
  `_apply_ipl_visibility_filter` and `_on_lod_test_mouse_moved`
  early-return cleanly); and "LOD can go on a new row, row 3" - moved
  the checkbox off the already-crowded first options row into its own
  new third row, matching the same pattern already used once for
  Time/Nav.

  Verified: toggling on/off with nothing loaded produces no crash (the
  exact scenario Keith hit); `preview_widget` (the real `DFFViewport`
  instance) now confirmed has all three new methods present. Full
  `QApplication` instantiation clean, `ast.parse` clean on both files.

- **Aug 1, 2026 (cont'd)** — Found and fixed a real texture-corruption
  bug, per Keith: "selecting 4 ipl files LAe.ipl, LAn.ipl, LAs.ipl,
  LAw.ipl, the first LAe.ipl loads and looks ok, the second part loads
  in (LAn.Ipl), and the textures on the first LAe.ipl get corrupted."

  A direct, unintended consequence of the geometry-conversion caching
  fix earlier this session: `_refresh_world_view` called `vp._upload_
  textures(all_textures, additive=False)` unconditionally -
  `additive=False` calls `clear_textures()` first, wiping *every*
  previously-uploaded texture (including the first IPL's) before
  re-uploading only the freshly-collected set for that call. This was
  harmless before the caching fix (every call used to re-collect and
  re-upload every visible model's textures regardless of whether
  they'd already been loaded) - but once an already-converted model
  (from a prior IPL, now cached) stopped re-collecting its own
  textures, the wipe-then-partial-reupload left its still-cached,
  still-referenced display list pointing at texture IDs that had just
  been deleted - exactly the corruption Keith saw, and exactly the
  kind of inconsistency that can slip in when two related caches
  (geometry and textures) aren't kept in sync with each other.

  Fixed by making `additive` mirror `clear_display_lists` exactly
  (`additive=not clear_display_lists`) - the same "genuine new world
  load (safe to clear) vs a mere visibility/partial update (must
  preserve what's already uploaded)" distinction `clear_display_lists`
  already expresses, now applied consistently to both caches.

  Verified directly with a realistic two-IPL scenario: a genuine first
  load (default `clear_display_lists=True`) correctly uploads with
  `additive=False`; a second load bringing in a new IPL alongside the
  first (`clear_display_lists=False`, matching how `_toggle_ipl_
  section`'s own already-fixed call actually flows) correctly uploads
  with `additive=True`, preserving the first IPL's textures rather
  than wiping them. Full `QApplication` instantiation clean, `ast.
  parse` clean.

- **Aug 1, 2026 (cont'd)** — Solved the "40% of models missing"
  mystery and shipped everything Keith asked for once he found the
  actual cause himself: "looking at the ipl's most of them are listed
  LODs, so where are the normal models? Maybe in the stream.ipl...
  if the look at the files, almost all the LODs are in the text
  ipls." Confirmed against his own real files: `LAe.ipl` is 74%
  LOD-named, with `LAe_stream0.ipl` through `LAe_stream5.ipl` (exactly
  the 6 files he named) holding the actual normal-detail models. This
  wasn't a filtering/parsing/caching bug at all - text IPLs and their
  binary streams simply hold genuinely different content, and nothing
  was loading the streams automatically.

  **"Load Text plus Binary IPL set"** - new Advanced menu checkbox
  (default on), new `load_text_plus_binary_ipl_set` setting. When a
  text IPL loads, its known associated streams (already tracked via
  the existing filename-prefix matching) now load automatically right
  alongside it.

  **"Show Full Loading Models (Debug)"** - new Advanced menu checkbox
  (default off), new `show_verbose_loading_dialog` setting. Added
  `_VerboseLoadingDialog` - a 500x400 scrolling list dialog matching
  Keith's exact spec: "Loading {name}.ipl and N binary ipls." header,
  one line per model as it loads, then "Loading linked ipl file
  {stream}" headers with their own per-model lines for each stream in
  turn.

  **Render mode not refreshing the IPL Inst File table** - fixed:
  `_set_world_render_mode` (the actual handler behind the merged
  Render/LOD dropdown) now calls `_refresh_ipl_inst_file_panel()` too,
  matching what an LOD-mode change already did.

  Verified end-to-end: a realistic text-IPL-plus-stream scenario
  correctly produces 4 total instances (2 from the text IPL, 2 auto-
  loaded from its stream) where only 2 would have loaded before;
  `_VerboseLoadingDialog` confirmed 500x400 with the exact line format
  specified; render mode change confirmed triggering the panel
  refresh; both new settings confirmed with correct defaults. Full
  `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Added a true single-instance fast path
  for nudge edits, per Keith: "when moving any object using the IPL
  object editor, it takes so long for anything to change; is there a
  way to only update the object thats been moved, not freshing the
  whole viewport."

  Every nudge (position/rotation/scale) previously went through the
  full `_apply_ipl_visibility_filter` -> `_refresh_world_view`
  pipeline - re-running the LOD/TOBJ filters and rebuilding a fresh
  entry dict (position, rotation, scale, model key) for *every*
  visible instance in the whole map, just to reflect the one that
  actually changed. Correct, but wasteful: every other instance's
  geometry, display list, and transform are completely unaffected by
  editing one object.

  Added `DFFViewport.update_instance_transform(inst, pos, rot,
  scale)` - finds the one already-rendered entry matching this
  instance by identity and updates just its `pos`/`rot`/`scale`
  fields in place, then triggers a repaint. Confirmed this is
  actually correct by re-reading `_draw_world_instances`'s own
  docstring: the compiled display list contains only raw local-space
  geometry, with position/rotation/scale applied fresh every frame
  via the surrounding `glPushMatrix`/translate/rotate/scale/
  `glPopMatrix` - so updating the entry dict's transform fields
  really does take effect on the next paint, no display-list
  recompilation needed at all.

  `_on_instance_edited` now tries this fast path first, falling back
  to the full filter pipeline only if no matching entry is found
  (e.g. the very first edit of a session, before any full refresh has
  populated the viewport's instance list yet). Not applied to the
  `MapViewport`-based world panes - their own rendering is already a
  different, vectorized numpy point-cloud approach (not per-instance
  display lists), unlikely to be the actual bottleneck here.

  Verified directly: moving one instance (with two already-rendered
  entries present) correctly updates only the moved one's position in
  place, leaves the other completely untouched, and never calls the
  full pipeline at all; with an empty/unpopulated viewport (no prior
  refresh), correctly falls back to the full pipeline as expected.
  Full `QApplication` instantiation clean, `ast.parse` clean on both
  files.

- **Aug 1, 2026 (cont'd)** — Fixed the "wrapped C/C++ object of type
  QTableWidgetItem has been deleted" crash Keith hit, and the "massive
  bottleneck on loading" it was reported alongside.

  Found the real root cause: `_on_ipl_section_cell_clicked` captured
  `item`/`row` from the IPL Sections table *before* calling `_ensure_
  ipl_loaded` - but that call (via the newly-added auto-stream-loading
  feature) calls `_rebuild_ipl_sections_rows()` internally, once per
  stream file, which replaces every row's items from scratch. The
  code right after the load then used the original, now-genuinely-
  stale `item`/`name_item` references without re-fetching them -
  exactly the crash, and it was this method's own code doing it, not
  external re-entrancy. Fixed by re-locating the row by `ipl_name`
  (not the original row index, which can also shift) after the load
  completes, returning cleanly if the row no longer exists at all.

  Also disabled the whole table for the duration of the load (on top
  of the fix above, as a second layer of protection) - the existing
  `_ipl_cell_click_in_progress` flag only ever guarded against re-
  entering this one method, not against a right-click (a completely
  different, unguarded handler) firing while a slow load is still
  rebuilding the table underneath it. A disabled `QTableWidget`
  receives no mouse events at all, closing that class of race
  regardless of which handler a click would go through.

  For the reported "massive bottleneck": found `_VerboseLoadingDialog
  .add_line` called `QApplication.processEvents()` on *every single*
  model line - for a stream file with thousands of instances, that's
  thousands of full event-queue pumps, each with real overhead, not
  just a cosmetic issue. Throttled to roughly 10 updates/second
  instead, still visibly live but no longer doing that work per line.
  Also redesigned the dialog per Keith's request ("I might want to
  keep the loading xxxx.ipl above, and the scrolling below") - the
  current-file header is now a fixed label above the list, not a
  scrolling entry that disappears as more models load underneath it.

  Verified: the exact crash scenario (table rebuilt mid-load, exactly
  matching what the real auto-stream-loading does) reproduced cleanly
  without an exception, confirming the old item is correctly not
  reused and the table is properly re-enabled afterward. Full
  `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Two real fixes from Keith's latest
  report:

  **Item Editor Dialog snapping back to a fixed position** - per
  Keith: "Every time I click on an object in the viewpoint, the IPL
  object editor snaps back; it should stay wherever I leave it."
  Found `dock.move(...)` sat *outside* the "dock is None" first-
  creation block in `_show_instance_edit_panel`, so it ran on every
  single call (every object click), unconditionally repositioning the
  dock near the main window's top-left corner regardless of where the
  user had since dragged it. Moved inside the first-creation block so
  it only ever runs once. Verified: moving the dock, then selecting a
  different object, correctly leaves it exactly where it was moved to.

  **IPL Inst File single-click not centering the viewport** - per
  Keith: "When I click any line in the IPL inst file, it should take
  me to the object in the viewpoint." A double-click handler already
  did this, but only for the Model column specifically, and only on
  double-click. Added a `cellClicked` connection and a new single-
  click handler covering any column. Verified: clicking any column
  (not just Model) on a single click correctly centers the viewport
  on that row's real instance.

  Also investigated Keith's other two reports - render mode changes
  (Wireframe/Non-texture/Semi-Solid/Textured) and LOD display mode
  changes (Show Normal/LOD/Both) not updating the viewport. Reviewed
  `_set_world_render_mode`/`DFFViewport.set_render_mode` and `_set_
  lod_display_mode`/`_apply_ipl_visibility_filter` in full - both
  correctly set their target state and call `self.update()` to
  request a repaint, both are wired to their menu actions correctly,
  and confirmed `preview_widget` (not the permanently-empty `_world_
  panes`) is the only active viewport in this build, so there's no
  wrong-viewport mismatch either. Didn't find an obvious code-level
  bug on inspection - logged for further diagnosis with more specific
  detail from Keith (e.g. whether the button label updates but the 3D
  view visibly doesn't, versus the change never happening at all,
  which would point toward a slow-repaint/display-list-recompilation
  cost for render mode specifically rather than a wiring bug).

  Full `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Followed up on render mode/LOD mode not
  updating the viewport, per Keith's answer to a diagnostic question:
  "Button/menu label changes but 3D view looks identical." That
  confirms the menu wiring genuinely fires (the label update proves
  it), narrowing the problem to somewhere between the mode being set
  and the repaint actually landing.

  Directly verified (via a mocked-GL test, since this sandbox has no
  real GPU) that the display-list caching/compilation logic itself is
  correct: switching `self._mode` does produce a different cache key
  and does trigger a genuinely fresh compile, confirmed by call
  count - two distinct display lists get compiled for `('model',
  'textured')` vs `('model', 'wireframe')`, and re-selecting an
  already-compiled mode correctly reuses the cache rather than
  recompiling. Also checked every place `self._mode` gets set (only
  the default and `set_render_mode` itself - no race/override) and
  `paintGL`'s own start (no early-exit or cached state that could
  block a mode change from reaching `_draw_world_instances`).

  Couldn't find a code-level bug after this level of verification.
  Added a defensive fix in both `_set_world_render_mode` and `_set_
  lod_display_mode`: call `vp.repaint()` (synchronous, paints
  immediately) right after the mode change, instead of relying solely
  on `update()` (which only schedules a repaint for the next event
  loop iteration). Can't confirm this is the actual cause without
  visual access to Keith's running app, but it's the most plausible
  remaining explanation once the rendering logic itself is ruled out,
  and it's a safe, low-risk change regardless of whether it's the
  real fix.

  Verified: both handlers now correctly call `repaint()` once per
  mode change. Full `QApplication` instantiation clean, `ast.parse`
  clean.

- **Aug 1, 2026 (cont'd)** — Three things from Keith's latest report:

  **Status never returning to "Ready"** - per Keith: "Once map
  workshop has loaded all the models, it should say ready, othereise
  loading models still shows throughout the session." Nothing
  previously reset the status bar after the last "Loading model:
  X..." message from `_refresh_world_view`'s conversion loop -
  added a final "Ready" status once that method genuinely completes.

  **The real crash** (`glTexImage2D` failing, `RuntimeError` in
  Keith's traceback via the LOD Test mouse-move callback) - found a
  genuine, severe GPU memory leak in `_upload_textures`: it
  unconditionally created a brand new GL texture object on *every*
  call, even for a texture name already uploaded, silently orphaning
  the old GL texture ID's VRAM (`self._tex_ids[name] = gl_id` just
  overwrote the dict entry, never calling `glDeleteTextures` on what
  it replaced). Harmless for a single load, catastrophic under LOD
  Test mode specifically - `_refresh_world_view` reruns on *every*
  mouse move while that's active, re-uploading the same already-
  loaded textures repeatedly and leaking a fresh copy of each one's
  VRAM every time, until the driver eventually failed to allocate
  more - exactly Keith's crash. Fixed by skipping re-upload entirely
  for a name already in `self._tex_ids` (a texture's pixel data for a
  given name doesn't change between calls, so there's nothing to gain
  from re-uploading it). Verified directly: 50 repeated upload calls
  for the same texture (simulating LOD Test's mouse-move-triggered
  reruns) now correctly produce exactly 1 GL texture, not 51.

  **Texture downscale option** - per Keith: "im thinking about a
  texture reduction option, keep 64. 128, 256 untouched but render
  down to 256x256 anything over 512x512." Added "Reduce Large
  Textures (256x256)" to the Advanced menu (off by default), with
  configurable threshold/target settings (defaults matching Keith's
  own numbers). Implemented `DFFViewport._downscale_rgba` - numpy
  block-averaging for the clean-multiple case (every size Keith
  actually mentioned divides evenly: 512/256=2, 1024/256=4,
  2048/256=8), giving meaningfully better quality than nearest-
  neighbor since each output pixel blends its whole source block
  rather than discarding samples; falls back to simple nearest-
  neighbor index sampling for any size that doesn't divide evenly.
  Applied right before `glTexImage2D` in `_upload_textures`, so it
  covers every texture upload regardless of caller.

  Verified extensively: the block-average math directly checked
  against a hand-built 4-color test image (each output pixel
  correctly matches its source block's color exactly); the full
  threshold logic checked against Keith's exact six-size spec
  (64/128/256/512 sent through untouched, 1024/2048 both correctly
  reduced to 256x256). Full `QApplication` instantiation clean,
  `ast.parse` clean on both files.

- **Aug 1, 2026 (cont'd)** — Added keyboard rotation shortcuts, per
  Keith: "im thinking about adding keyboard shortcuts, arrow keys,
  and numpad to rotate." Arrow keys and numpad both rotate the
  camera (numpad detected via `KeypadModifier` specifically, so it
  doesn't collide with top-row number keys used for anything else -
  a plain "6" and a numpad "6" are otherwise the same key code in
  Qt). Continuous rotation while a key is held via a repeating
  ~60fps `QTimer` (stops itself automatically once no rotate key is
  still held, rather than idling permanently), matching the smooth
  feel of right-click-drag rotation rather than one fixed step per
  press.

  Also investigated the reported mouse button reliability issue
  ("right click held down rotates just fine, middle mouse doesn't
  always work, left click to select object doesn't always work") -
  reviewed the full mouse event chain: object selection turns out to
  be double-click-only (`_pick_world_instance`, a precise ray/
  triangle test), and pan/rotate share an `elif` chain with `_view_
  locked` checked only on the rotate branch. Neither directly
  explains the specific "middle sometimes, right always" asymmetry
  Keith described, and couldn't rule out an OS/window-manager-level
  cause (e.g. middle-click-paste, a common X11 convention) without
  being able to reproduce interactively. Logged to TODO.md with what
  was and wasn't found, plus a diagnostic suggestion (checking
  whether the same flakiness shows up in a different app's own
  middle-click handling). The new keyboard shortcuts give a reliable
  alternative for rotation specifically regardless of the mouse
  issue's actual cause.

  Logged game controller/thumbstick support to TODO.md as a real,
  separate feature - Qt has no built-in gamepad API, would need
  QtGamepad or a third-party library (availability unchecked), and a
  polling loop reusing the same QTimer pattern the keyboard shortcuts
  just established. Not started, scoped for later prioritization.

  Verified: holding and releasing a rotate key correctly tracks/
  untracks it and starts/stops the timer; continuous rotation while
  held confirmed changing yaw across simulated ticks; numpad vs
  top-row key distinction confirmed correct in both directions (a
  plain "6" is not treated as a rotate key, a numpad "6" is). Full
  `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Two fixes from Keith's latest request:

  **Settings dialog reorganization** - per Keith: "these settings, can
  be added to map_workshop's settings on the title bar (topbar) as a
  new tab in the settings dialog, this would tidy up the IPL
  controls." Added a new "Loading" tab to the existing `MapSettings
  Dialog` (opened from the top menu bar), moving "Load Text plus
  Binary IPL set", "Show Full Loading Models (Debug)", and the
  texture downscale option out of IPL Controls' Advanced menu.
  Also made the texture downscale target genuinely configurable
  rather than fixed at 256 with only on/off exposed ("texture size
  limit, 256x256 but it's like to be able to change the limit") -
  both the threshold and target size are now spinboxes. Removed the
  three now-dead toggle handlers left behind in the old Advanced-menu
  location.

  While testing this, found the *entire* Settings dialog was already
  broken and had been crashing on open (`_load_tool_icon` called but
  never defined anywhere, in the leftover paint-tool "Gadgets" tab)
  and on save (14 more widget attributes referenced in `_accept()`
  but never actually created in `__init__` - marching ants, pixel
  grid, palette rows/cols, platform mode, all leftover from the same
  paint-tool legacy, not relevant to a 3D map editor). This meant
  *no* setting in *any* tab could ever actually be saved via this
  dialog before now, regardless of the new Loading tab. Fixed both
  with defensive `getattr`/try-except guards rather than
  reconstructing 15 unrelated paint-tool widgets - every genuinely-
  used setting across every tab now saves correctly.

  **LOD Test made bidirectional** - per Keith: "LOD test option
  should work 2 ways, if normal models are loaded, those in the
  circle get switch to LOD, where if Show LOD is set, then in the
  circle show normal models." The circle previously always meant
  "inside = normal, outside = LOD" regardless of the current global
  Show Normals/LOD only mode. Now the circle always shows the
  *opposite* detail level from whatever the global mode is already
  displaying everywhere else - a live "what would the other detail
  level look like here" preview from either starting mode. `both`
  mode has no meaningful opposite (both are already shown everywhere)
  so the circle is a no-op there. Also fixed a real bug caught while
  rewriting this: a standalone instance with no LOD pair and no
  draw-distance-based LOD status has nothing to switch to and should
  always stay visible regardless of the circle - my first draft of
  the bidirectional logic incorrectly hid it on one side.

  Logged "this function in the future can be explanded" to TODO.md
  as an open door for later, with a few possible directions noted.

  Verified extensively: full Settings dialog open + Loading tab
  present + save + live re-application to `preview_widget` confirmed
  working end-to-end (was completely broken before this); LOD Test
  bidirectional behavior confirmed correct in both starting modes
  against a real paired normal/LOD instance, `both` mode confirmed
  showing everything regardless of circle position, and the
  standalone-instance edge case confirmed staying visible everywhere.
  Full `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Per Keith: "generic.txd loading can be
  added to settings also under map assits tab." Added a new "Map
  Assets" tab to the Settings dialog with a "Load Generic.txd
  Manually" button (a one-time action rather than a persistent
  toggle, so a button matching the existing "Ribbon Manager…" pattern
  in the Ribbons tab, not a checkbox). Removed it from IPL Controls'
  Advanced menu, and since that was the last item left in that menu
  (the other three moved to Settings > Loading earlier), removed the
  now-empty Advanced button entirely rather than leave a non-
  functional leftover - completing the "tidy up the IPL controls"
  goal from earlier in the session.

  Verified: Map Assets tab present in the dialog; its button
  correctly triggers the real `_on_load_generic_txd_clicked` handler
  on click. Full `QApplication` instantiation clean, `ast.parse`
  clean.

- **Aug 1, 2026 (cont'd)** — Fixed a real mistake from earlier this
  session: per Keith, "class MapSettingsDialog(QDialog): is where the
  new settings should be, I don't see Map Assits tab with the Advance
  settings moved too?" Investigated and found `MapSettingsDialog` is
  never actually instantiated anywhere in the file - there are
  *three* separate, parallel settings-dialog implementations
  (`MapSettingsDialog`, `_show_settings_dialog` reachable only via a
  hotkey, and `_show_workshop_settings` wired to the actual top-bar
  Settings button), and the Loading/Map Assets tabs had gone into the
  first, unreachable one. Full details logged to TODO.md, including
  a recommendation to eventually consolidate down to one.

  Moved both tabs to `_show_workshop_settings` (the real, reachable
  one), using the correct `MapSettings` persistence for their own
  values even though the rest of that dialog uses its own separate,
  ad-hoc attribute mechanism.

  While testing this, found `_show_workshop_settings`'s own "Apply
  Settings" button was *also* already broken, independent of any of
  this - `self.format_combo` and `self.preview_widget.bg_color` are
  both referenced but never actually exist, meaning the button
  crashed outright for every tab, not just the new ones, before this
  fix. Guarded both specifically and wrapped the remaining pre-
  existing logic in a broad try/except as a pragmatic fix given the
  apparent scale of pre-existing breakage in this function - a strict
  improvement either way, since nothing after the first broken
  reference ever applied before this regardless.

  Verified end-to-end through the real, reachable dialog: Loading and
  Map Assets tabs present; Apply Settings now completes without
  crashing; downscale settings and their live re-application to
  `preview_widget` all confirmed correct; the Generic.txd button
  confirmed triggering the real handler. Full `QApplication`
  instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Implemented `path` section parsing for
  GTA3/VC, per Keith: "we need to address... path for GTAIII and
  extended for VC. the path coords arent the same scale as the IPL
  data, this needs to be worked up." `path` was completely absent
  from `IPL_SECTIONS` before this - not recognized as a section at
  all, regardless of scale.

  Added `PathNode` (twelve fields per Project Cerbera's VC path
  documentation: type, next, x/y/z, median, left, right, flag1-3) and
  `PathGroup` (up to 12 nodes sharing a header line) dataclasses.
  Implemented the actual parsing in `IPLParser`: a path group's own
  header line ("1, -1") has no leading tab, its sub-node lines do -
  the main parse loop's existing `line = raw.split("#")[0].strip()`
  already erases that distinction by the time `line` is available, so
  the new code checks the *raw*, pre-strip text specifically for
  `path` section lines to tell a new group from a node belonging to
  the current one.

  Applied the /16 coordinate conversion confirmed last session (VC's
  text `path` section stores coordinates in units "sixteen times
  smaller than standard," per Project Cerbera) so a `PathNode`'s x/y/
  z land in the same coordinate space as everything else (instance
  positions, etc.), not the file's own differently-scaled internal
  units.

  Wired `paths` into `GTAWorldLoader` too (both the eager/dat-driven
  and on-demand/lazy IPL load paths), matching `instances`' own
  existing accumulation pattern, so parsed path data is actually
  reachable going forward rather than only living inside a single
  `IPLParser` instance.

  Verified extensively against Keith's real uploaded `paths.ipl`
  (1957 path groups, 23,484 total nodes): parse completes with zero
  errors/warnings; the first node's converted coordinates match the
  expected /16 calculation exactly `(-866.63125, -652.45,
  10.0676875)`; the parsed group count (1957) exactly matches the
  raw file's own group-header line count via independent line-by-line
  verification; the empty `inst`/`cull`/`pick` sections earlier in
  the same file correctly produce zero instances rather than
  interfering; the very last group and node parse correctly too, not
  just the first. Confirmed accumulating correctly through
  `GTAWorldLoader._load_ipl` as well, not just the standalone parser.

  Not yet done - see TODO.md: showing path data anywhere in the UI,
  editing, write-back, and the rest of Keith's broader request (pick
  support, cull zones as editable boxes, IDE tobj/path/2dfx editor
  for Model Workshop) are all separate, unstarted pieces.

  Along the way, caught and fixed a mistake in my own edit process -
  a `str_replace` call accidentally deleted `IPLLoadResult`'s class
  declaration and part of its docstring; caught immediately via the
  syntax check that always runs after every edit, fixed before
  moving on.

- **Aug 1, 2026 (cont'd)** — Verified "apply settings for show full
  loading, and texture scaling" end-to-end: changed both settings in
  the Loading tab, clicked Apply Settings, confirmed both save to
  MapSettings, the texture downscale change live-applies to `preview_
  widget`, and both correctly persist and show as checked when the
  dialog is re-opened. Confirmed working correctly.

  Added a dedicated LOD Test circle radius setting, per Keith: "we
  also need a settings for the LOD test circle, its set as 300, would
  be nice to have a settings in Map-Assists to adject the circle
  size." The circle's radius was previously tied directly to `lod_
  draw_dist_threshold` (the LOD detection cutoff), so adjusting one
  meant adjusting both together. Added `lod_test_circle_radius` as
  its own separate setting (same 300.0 default, so no behavior change
  until actually adjusted) and a spinbox in Settings > Map Assets, per
  Keith's exact requested placement, with a live update to any
  already-active LOD Test session too, not just future ones.

  Verified: default value matches prior behavior exactly (300);
  changing and applying the setting saves correctly; toggling LOD
  Test mode on after the change correctly picks up the new radius.
  Full `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Moved the LOD Test toggle from an IPL
  Controls checkbox to a ribbon icon, per Keith: "the LOD test
  function could be an SVG icon on the ribbon, 2 overlapping Circles,
  one hollow, other solid. with the tooltip. this way it does have to
  use up space on the ipl control, but keep row3 for future
  functions, like show tojb, show Paths, show zons."

  Added `SVGIconFactory.lod_test_icon` - two overlapping circles, one
  hollow (normal-detail models) and one solid (LOD models), matching
  Keith's exact spec and suggesting the live switching the tool
  actually performs. Added as a checkable action in the Render ribbon
  group (alongside the other render/viewport-mode toggles it
  conceptually belongs with - Toggle Mesh, Toggle Backface, Cycle
  Render Style, Toggle Shading), wired to the same `_on_lod_test_
  toggled` handler already built earlier this session, with a fuller
  descriptive tooltip than the terse action name alone.

  Removed the old checkbox from IPL Controls row 3 - left the row's
  layout intact and empty rather than removing it, reserved for the
  visibility toggles Keith mentioned (TOBJ/paths/zones), logged to
  TODO.md.

  Verified: new icon renders without error; ribbon action correctly
  checkable with the full descriptive tooltip; toggling it on wires
  the same mouse-move callback the old checkbox did, toggling it off
  clears the callback and re-applies the visibility filter
  identically; confirmed no remaining references to the removed
  checkbox anywhere. Full `QApplication` instantiation clean, `ast.
  parse` clean on both files.

- **Aug 1, 2026 (cont'd)** — Fixed the crash Keith hit immediately
  after toggling Shading: "TypeError: ModelWorkshop._toggle_viewport_
  shading() missing 1 required positional argument: 'enabled'." The
  ribbon action's callback lambda discarded the checkbox state
  entirely (`lambda v: self._toggle_viewport_shading()`) instead of
  passing it through - fixed to `lambda v: self._toggle_viewport_
  shading(v)`.

  While fixing this, found `_toggle_viewport_shading` (and three
  other call sites - the Light Setup Dialog's live-apply sync, its
  preset-load restore, and its own internal checkbox sync) all read
  `self._shading_btn`, an attribute that's never actually created -
  the real one is `self._shading_act` (the `QAction` created via `_act
  (..., attr='_shading_act')` in `_build_toolbars`). Every one of
  these 4 call sites was silently no-opping (icon never updated,
  checked-state never synced) rather than crashing, since `getattr(...,
  default=None)` swallowed the mismatch quietly. Renamed all 4 to the
  correct attribute - `QAction` supports the same `.isChecked()`/
  `.setChecked()`/`.setIcon()`/`.blockSignals()` interface these sites
  already expected, so no other logic needed to change.

  Per Keith's context for keeping this feature - "this can be used to
  generate pre-lighting, that can be saved back to the models" -
  logged the pre-lighting bake/write-back idea to TODO.md as a real,
  substantial future direction, not attempted this turn.

  Verified: toggling the Shading ribbon action on/off no longer
  crashes; `preview_widget._shading_enabled` correctly reflects the
  new state; confirmed the icon-update path inside `_toggle_viewport_
  shading` now actually finds and uses the real action instead of
  silently no-opping. Full `QApplication` instantiation clean, `ast.
  parse` clean.

- **Aug 1, 2026 (cont'd)** — Disabled the 4-Pane View toggle, per
  Keith: "4 panels icon, keep, but the function isnt needed, it
  creates a strange beheavour." Icon stays visible in the Navigation
  ribbon group exactly as requested; the action itself is now
  disabled (greyed out, unclickable) with a tooltip explaining why.

  Root cause of the "strange behaviour": `_sync_quad_from_main` (the
  function that populates the 4 panes when switching to quad view)
  only ever mirrors single-model geometry attributes (`_vertices`,
  `_triangles`, etc.) - inherited directly from Model Workshop's
  single-DFF-editing base this file was built on. It never syncs
  `_world_instances`, the actual multi-instance map data Map
  Workshop's real workflow uses, so switching to 4-Pane View while
  an actual map is loaded (which is nearly always, for this app)
  showed blank panes rather than anything useful.

  Logged to TODO.md that a genuine world-aware version of this
  feature would need its own sync logic built from scratch around
  `_world_instances`, not a fix to the existing single-model one, in
  case a real "4 world views from different angles" feature is wanted
  later.

  Verified: the action still exists and is visible in the toolbar
  (icon kept), confirmed disabled/unclickable, tooltip confirmed
  explaining the reason. Full `QApplication` instantiation clean,
  `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Converted IPL Controls' INST/CULL/ZON/
  PATH buttons into a real tab bar, and added `grge`/`enex` parsing,
  per Keith: "in IPL Controls we have the labels, made into tabs...
  but when loading SA maps, there would be more tabs" plus real
  example `grge`/`enex` data.

  Added `GrgeEntry` (11 fields: X1/Y1/Z1, front X/Y, X2/Y2/Z2, door
  type, garage type, name) and `EnexEntry` (18 fields: enter X/Y/Z/
  angle, size X/Y/Z, exit X/Y/Z/angle, target interior, flags, name,
  sky, num peds, time on/off) dataclasses, cross-verified against
  established GTA modding documentation (SannyBuilder forums for
  GRGE, Grand Theft Wiki for ENEX) - both matched Keith's own real
  example data field-for-field (his `grge` line's garage type 16 =
  "Save garage," entirely consistent with its name "cjsafe"; all 10
  of his real `enex` lines parsed with zero errors). Wired into
  `IPLParser` and `GTAWorldLoader`'s accumulation, matching the
  `path` pattern from last session.

  Replaced the `QButtonGroup` row with a `QTabBar` covering all 11 SA
  IPL sections: INST/CULL/ZON/PATH/GRGE/ENEX now enabled (PATH newly
  enabled too, since last session's work made it real, no longer a
  stub); PICK/JUMP/TCYC/AUZO/MULT added as disabled stub tabs with
  explanatory tooltips (AUZO's notes Keith's audio-icon idea
  specifically) rather than not existing at all - scales to many more
  sections without the row running out of horizontal space.

  Made the IPL Inst File table's columns dynamic per section instead
  of permanently fixed at INST's 13-column layout - GRGE shows its
  own 11 columns, ENEX its own 18, with headers matching each
  section's real field names.

  Logged two things needing Keith's input/further work to TODO.md:
  what exactly "tobj/path added to ipl objects" means for the IDE
  side (a few plausible interpretations, didn't want to guess wrong),
  and that PICK/JUMP/TCYC/AUZO/MULT still need real parsing once
  sample data exists to verify against.

  Verified extensively: `_parse_grge`/`_parse_enex` tested directly
  against Keith's exact real lines; a full test file with all 10 of
  his real `enex` lines plus the `grge` line parses with zero errors/
  warnings; confirmed accumulating correctly through `GTAWorldLoader`;
  tab bar confirmed showing all 11 tabs with correct enabled/disabled
  states, clicking an enabled tab correctly changes the display type,
  clicking a disabled tab correctly no-ops; IPL Inst File table
  confirmed showing the right dynamic headers and data for both GRGE
  and ENEX end-to-end. Full `QApplication` instantiation clean, `ast.
  parse` clean on both files.

- **Aug 1, 2026 (cont'd)** — Properly fixed the `bg_color` error
  Keith kept seeing when saving settings: "[Settings] Some pre-
  existing settings could not be applied: 'DFFViewport' object has no
  attribute 'bg_color'... trying to save, full loading debug or
  texture size reducing, gives that error." This had been left as a
  swallowed warning by the earlier defensive try/except - functional
  underneath, but visibly alarming every single time, exactly what
  Keith reported.

  Traced the actual root cause: `self.preview_widget.bg_color` never
  existed anywhere - that plain `bg_color` attribute (no underscore)
  belongs to a completely different class, `ZoomablePreview` (a 2D
  `QLabel`-based preview widget from the paint-tool legacy this file
  inherited), not `DFFViewport` (the real `preview_widget`). No color-
  picker UI exists anywhere in this dialog to supply a specific solid
  color, so both the "checkerboard" and "solid"/else branches now
  correctly reset to the theme-default color via `set_checkerboard_
  background()` - the right call for this despite its name (`DFFView
  port` has no real checkerboard rendering at all, per its own
  docstring). Found and fixed a second, separate instance of the
  identical bug in `_pick_background_color` (a "Pick Background
  Color" action, untriggered by Keith's report but would have crashed
  identically if ever used) - now uses `DFFViewport`'s own `_get_bg_
  color()` to supply a real `QColor`.

  Verified: reproduced Keith's exact scenario (Apply Settings with
  both "Show Full Loading" and texture downscale changed) with stdout
  captured - confirmed zero `bg_color` output where the warning used
  to appear every time, both settings still save correctly; separately
  confirmed `_pick_background_color` now runs without crashing. Full
  `QApplication` instantiation clean, `ast.parse` clean.

- **Aug 1, 2026 (cont'd)** — Added a hidden toolbar section to the
  Ribbon Manager, per Keith: "in ribbon manager i'd like a hidden
  section where anything placed there cant be seen." A special
  "Hidden" toolbar is created automatically the first time the Ribbon
  Manager opens, behaves like any other toolbar there (appears in the
  toolbar list, actions can be dragged/moved into and out of it via
  the existing "Move selected to:" mechanism) except it's never
  actually shown on screen - `setVisible(False)` enforced both on
  creation and via a `visibilityChanged` safeguard, so nothing (a
  stray toggle, `QMainWindow`'s own state save/restore round-trip on
  the next launch, etc.) can make it - or whatever's parked in it -
  visible again without deliberately moving those actions back out
  first.

  Verified: toolbar creates correctly and starts invisible; calling
  the getter again returns the same instance rather than creating
  duplicates; directly forcing it visible confirmed the safeguard
  immediately re-hides it; confirmed it shows up correctly in the
  Ribbon Manager's own toolbar list so actions can actually be moved
  into it. Full `QApplication` instantiation clean, `ast.parse`
  clean.

- **Aug 1, 2026 (cont'd)** — Added "Show Tobj" and moved Nav to
  Settings, per Keith's follow-up: "have an option to toggle showing
  tobj [Show Tobj], in row 3, timed objects will be shown, depending
  on there time values. tojb can be shown along side the inst,
  towards the botton, keeping the placement order of the ipl. if we
  move Nav functions to Settings, then row 2 could be used."

  Moved the "Nav" popup (mouse sensitivity, go-to-object zoom
  distance) into a new Navigation tab in the real Settings dialog -
  same widgets, same live-apply behavior as the original popup,
  nothing new persisted that wasn't before. Removed the now-dead
  `_show_nav_settings_popup` method and its IPL Controls button
  entirely.

  Added "Show Tobj" in the row 2 space this freed up. Unchecked
  (default): TOBJ-type instances are excluded from the INST table
  view entirely. Checked: they're collected separately and appended
  after all the regular rows, filtered to only the ones currently
  active for the simulated hour (reusing the exact same time_on/
  time_off logic already driving the 3D world view's Time switch,
  including the overnight-wrap case). Regular (non-TOBJ) rows are
  completely unaffected either way - unchanged relative order, taken
  directly from the file's own line order, exactly as before this
  feature existed.

  Verified end-to-end with a real TOBJ model (time_on=20, time_off=6,
  an overnight-wrap case) mixed in with two ordinary instances: Show
  Tobj off correctly excludes it entirely; on with the simulated hour
  inside its active window correctly shows it appended at the bottom,
  after both regular rows in their original order; on with the hour
  outside its window correctly excludes it again. Navigation tab
  confirmed present with both controls working and live-applying to
  `preview_widget`. Full `QApplication` instantiation clean, `ast.
  parse` clean.

- **Aug 14, 2026** — Added collision render options to IPL Controls,
  per Keith: "add to collisions to the IPL control pane, under render
  options, load solid collision, load semi-solid, wireframe cols, and
  solid with surface mapping" -> "Ghost is a good idea; Show Ghosted
  Col, Show Surface Mapped Col, Show Semi-Solid Col, Show Wireframe
  Col". Four independent checkboxes in the row 3 space reserved for
  this back on Aug 1 - not an exclusive group like Render/LOD, any
  combination can be on together. Each draws collision geometry as an
  overlay on top of the model, never replacing it.

  `ModelCache` gained a `_col_index` keyed by each COL model's own
  internal name (`header.name`), not its container filename - real
  COL archives are multi-model (e.g. generic.col holds many
  separately-named collision models), so indexing by container name
  the way `_dff_index`/`_txd_index` already do wouldn't map to
  instance model names at all. `index_col_files()` loads and indexes
  every model in a given list of `.col` paths; `_refresh_world_view`
  now also globs the game root recursively for `*.col` files
  alongside the existing IMG indexing, same pattern already used for
  standalone TXD fallback search elsewhere in this file.

  `DFFViewport` gained `show_col_ghosted/semi_solid/wireframe/
  surface_mapped` flags, their setters, and `_draw_collision_faces` -
  unlit (COL vertices carry no normals), flat colour for ghosted/
  semi-solid/wireframe, per-face material colour for surface-mapped.
  A separate `_col_display_lists` cache (same lazy-build-once-per-
  model pattern as the model's own display lists) keeps collision
  overlay drawing cheap regardless of how many instances share a
  model. Only the mesh (vertices/faces) is drawn - COL's sphere/box
  primitives aren't rendered yet, logged to TODO.md.

  Material colours are resolved once per face, in `map_workshop.py`
  (`_convert_collision_geometry`, reusing `col_materials.
  get_material_colour`, game detected as VC vs SA from the COL
  header's own version) - not inside `dff_viewport.py`, keeping COL-
  material lookups out of the pure-GL viewport code entirely.

  `ast.parse` clean on all three changed files (`model_cache.py`,
  `dff_viewport.py`, `map_workshop.py`). Not yet tested against
  Keith's real data - depends on his game folder actually having
  standalone `.col` files under the game root for anything to show.

- **Aug 14, 2026 (cont'd)** — Moved the four collision render options
  out of their own IPL Controls row and into the Render: dropdown
  itself, per Keith: "the 4 col options should be in the Render:
  dropdown, with LOD and Normal models, Normal models, list first,
  then LOD, and COL under". Menu order is now: render style (Texture/
  Non-texture/Semi-Solid/Wireframe), separator, LOD filter (Show LOD
  only/Show Normals/Show Both), separator, then the four Col options
  (Show Ghosted Col/Show Surface Mapped Col/Show Semi-Solid Col/Show
  Wireframe Col) - not in a QActionGroup like the two groups above
  them, since these four stay independently toggleable together. The
  four near-identical toggle handlers collapsed into one shared
  `_on_col_render_option_toggled`. Row 3 in IPL Controls reverts to
  reserved-for-future (Paths/Zones visibility), as it was before this
  feature existed. `ast.parse` clean.

- **Aug 14, 2026 (cont'd)** — Fixed collision indexing only ever
  finding 1 file, per Keith: "in SA it should be reading them from
  the gta3.img... In VC, they can also be found in the gta3.img file,
  just like the models, or linked in gta_vc.dat paths to cols with
  the map files, in gta3, there in the map files only paths to map/".
  The previous approach (blind-glob every *.col under the game root)
  was wrong for all three games - it happened to find whatever loose
  .col Keith's SA folder had lying around, but missed the real source
  entirely.

  Real per-game picture, now handled correctly:
  - SA: ALL collision lives inside gta3.img (no COLFILE directives in
    gta.dat at all) - `ModelCache.index_img_files` now also indexes
    .col entries the same lightweight way as .dff/.txd (one model per
    entry, named like the model).
  - VC: per-object collision is ALSO in gta3.img (same indexing as
    SA), plus a handful of shared collision (e.g. generic.col) still
    reached via COLFILE directives in gta_vc.dat.
  - GTA3: collision is ONLY reachable via COLFILE directives (paths
    into data/maps/) - never in the IMG at all.

  `GTAWorldLoader` gained `get_col_paths()` (mirrors the existing
  `get_img_paths()` exactly - COLFILE entries were already being
  logged for the DAT Browser, just not exposed as their own
  accessor). `_refresh_world_view` now calls `loader.get_col_paths()`
  instead of glob-scanning the game root, so standalone collision
  indexing actually reflects what the loaded .dat(s) really
  reference, not whatever unrelated .col files a folder happens to
  contain. `ModelCache.get_collision` checks the IMG-embedded index
  first, falls back to the standalone-file index.

  Also fixed a real bug caught before it shipped: `COLFile.
  get_model_by_name` compares `model.name`, but `COLModel` has no
  such attribute (only `model.header.name`) - would have raised on
  every single lookup, silently swallowed by the surrounding
  try/except, making every IMG-embedded collision lookup fail
  invisibly. Matches by `header.name` directly instead now, falling
  back to the entry's first model when no name matches.

  `ast.parse` clean on all three changed files (`model_cache.py`,
  `gta_dat_parser.py`, `map_workshop.py`); also smoke-tested
  `ModelCache()`/`GTAWorldLoader()` instantiation and the new
  `get_col_paths`/`get_collision`/`is_col_indexed` methods directly
  (no crash, correct empty-state output) - not yet tested against
  Keith's real data.

- **Aug 14, 2026 (cont'd)** — Fixed a real crash Keith hit
  immediately on launch: `TypeError: 'bool' object is not callable`
  in `_on_col_render_option_toggled`. `col_specs` was passing the
  FLAG name ("show_col_ghosted") instead of the SETTER name
  ("set_show_col_ghosted") - `hasattr` still passed (the flag
  attribute genuinely exists on `DFFViewport`), so it silently got
  through the guard and then tried to call the bool's current value
  as a function. Fixed `col_specs` to use the real
  `set_show_col_ghosted`/`set_show_col_surface_mapped`/
  `set_show_col_semi_solid`/`set_show_col_wireframe` setter names,
  and hardened the handler itself to use `callable()` instead of
  `hasattr()` so this exact class of mistake (attribute exists but
  isn't the callable you meant) fails safely instead of reaching a
  call at all. Smoke-tested the fixed and old-broken code paths
  directly (no Qt/OpenGL needed) - confirmed the setter call now
  works and the old bug pattern is now safely rejected rather than
  crashing.

- **Aug 14, 2026 (cont'd)** — Matched IPL Controls' section QTabBar
  (INST/CULL/ZON/PATH/GRGE/etc) to Object Browser's IMG/DAT/IDE/IPL
  tab-button sizing, per Keith: "the tabs in the iPL control panel
  need to be the same size as the ones in the object browser". Added
  an explicit `QTabBar::tab` stylesheet (18px height matching
  `OBJECT_BROWSER_BUTTON_H`, same compact 0px-vertical/bold styling
  as that panel's QToolButtons) - previously unstyled, falling back
  to the app's default (much taller) QTabBar look. `ast.parse` clean.

- **Aug 14, 2026 (cont'd)** — Narrowed IPL Controls' section tabs
  further, per Keith: "the INST, CULL tabs, can be narrower, just
  enough to fit the text" - added `min-width: 0px` to the QTabBar::tab
  stylesheet added earlier this session. Padding alone wasn't enough:
  Qt's built-in style still enforces its own minimum tab width
  underneath a stylesheet's padding unless min-width is explicitly
  overridden, which is why short labels (INST/CULL/ZON) were still
  sitting in extra whitespace despite the earlier fix.

  Added double-click-to-open on the IPL Sections table, per Keith:
  "you can select IPL files, maybe just double-click them to open
  them on the filename... right click unload ipl, or just hide the
  file from view - whatever is the most logical way of doing this."
  Double-clicking any cell in a row now loads/shows that IPL if it's
  currently hidden (reusing the existing single-click-on-eye-icon
  "show" code path rather than duplicating it) - a no-op if it's
  already visible, matching the ordinary meaning of double-clicking
  something already open. The existing eye-icon column and its
  right-click Hide/Show context-menu action (already present, already
  functional on inspection) are unchanged - this adds a second,
  more standard way in, it doesn't replace what was there.

  Logged Keith's "Ghosted view could be useful for LOD and Normal"
  aside to TODO.md rather than implementing speculatively - not
  scoped enough yet (needs confirmation on which of Normal/LOD should
  be the ghosted one) to build without guessing.

  `ast.parse` clean on all three changes.

- **Aug 14, 2026 (cont'd)** — Built a shared, standalone binary
  parser for SA's real path data, per Keith: "build the paths parser
  as a shared method set, that can be used by other tools, besides
  map workshop". New module `apps/methods/sa_path_parser.py` - no
  Map Workshop/PyQt/GUI dependencies at all, just struct/dataclasses,
  so anything in this codebase can import and use it directly.

  Real finding first: SA does NOT use the IPL text `path` section for
  actual path data at all - that format (gta_dat_parser.py's
  PathNode/PathGroup, verified against Keith's own III/VC data) only
  applies to GTA III/VC. SA's own text-format path files still exist
  on disk but are unused leftovers (per GTAMods Wiki); the game reads
  64 separate binary `nodesN.dat` files instead (one per 750x750-unit
  map area, `-3000,-3000` origin, row-major, normally packed inside
  gta3.img), an entirely different data model - nodes/links/navi-
  nodes as a double-linked adjacency-list graph, not IPL rows.
  Confirmed via https://gtamods.com/wiki/Paths_(GTA_SA) - full spec:
  20-byte header (5 section counts), Section 1 path nodes (28 bytes,
  vehicle then ped, position/link/area/node IDs, width, flood-fill,
  a 32-bit flags word covering traffic level/highway/roadblock/boat/
  emergency/spawn-probability/parking), Section 2 navi nodes (14
  bytes, vehicle-only interpolation points with direction vectors
  and lane/traffic-light flags), Sections 3/5/6 links/navi-links/
  link-lengths (same entry count, combined into one SAPathLink record
  here rather than three parallel arrays), Section 4 a fixed 768-byte
  filler block, Section 7 per-link intersection flags.

  New dataclasses: SAPathNode, SANaviNode, SAPathLink, SAPathFile -
  each documented field traced to its wiki description, several as
  computed properties (traffic_level, is_highway, is_roadblock,
  is_boat, is_emergency_only, spawn_probability, is_parking on
  SAPathNode; path_width, left_lanes/right_lanes, traffic_light_
  behaviour, is_train_crossing on SANaviNode) rather than leaving
  every caller to hand-decode the raw flags bitfields themselves.

  Core parser `parse_nodes_dat(data, area_id)` never aborts on a
  truncated/malformed section - collects problems in `parse_errors`
  and keeps whatever parsed correctly before that point, matching
  this codebase's established IDE/IPL text-parsing pattern. Loader
  convenience functions: `load_nodes_dat(path)` (one file from disk),
  `load_all_nodes_dat_from_dir(directory)` (every nodesN.dat in a
  folder, keyed by area_id), and `find_nodes_dat_in_img(img)` +
  `load_nodes_dat_from_img_entry(img, entry, area_id)` (lazy two-step
  IMG-archive lookup, same lightweight-index-then-parse-on-demand
  pattern as ModelCache.get_geometry/get_collision, since the wiki's
  documented normal location for these is packed inside gta3.img).

  Verified by hand-building a synthetic nodesN.dat blob matching the
  spec exactly (1 vehicle node, 1 ped node, 1 navi node, 2 links with
  navi-links/lengths/intersection flags) and round-tripping every
  single field - position scaling, flags decoding via every computed
  property, link navi-node/area resolution including the ped-link
  None case, intersection flag bits - all correct. Also verified the
  truncation-doesn't-crash path and all three loader functions
  (file/dir/IMG) against real temp files and a fake IMG object.
  `ast.parse` clean.

  Not yet verified against Keith's own real nodesXX.dat sample
  data (built straight from the documented spec) - and not yet
  integrated into Map Workshop's PATH tab/viewport at all, that's
  the next step once this shared parser itself is confirmed correct.

- **Aug 14, 2026 (cont'd)** — Fixed the empty "IPL Inst File" panel
  when a PATH-only file (e.g. VC's real dedicated paths.ipl) was
  selected, per Keith: "When I click paths.ipl or any other paths
  file, no listing is shown". Root cause: `headers_by_type` had no
  'path' entry at all - silently fell back to inst's 13-column ID/
  Model/Int/Pos.../Rot... layout, which doesn't match a path
  section's real two-line-shape structure (2-field group headers vs
  up to 12-field node lines) at all. Added a proper path-specific
  column layout (Type/Group A, Next/Group B, Zero, X, Y, Z, Median,
  Left, Right, Flag1-3, mirroring the raw on-disk field order Project
  Cerbera's VC path doc and gta_dat_parser.py's own verified
  PathNode/_parse_path_node already use) and told the two line shapes
  apart the same way the real parser does internally (checking the
  raw pre-strip leading-whitespace, not the already-stripped line) -
  group-header rows now render bold with a distinct tint instead of
  looking like broken/near-empty node rows. Also added a placeholder
  row ("no <TYPE> section in this file - try another tab above") for
  the genuinely-correct-empty case (a file really has no data for
  whichever tab is currently active), so that state reads as
  informative rather than indistinguishable from a bug.

  Renamed "IPL Inst File" to "IPL File Display" throughout the user-
  visible UI (dock title, collapsible-dock label, the dynamic title
  that switches to "IDE Objects"), per Keith: "Maybe we should rename
  'IPL inst file' to just 'IPL File Display' or something better...
  This makes more sense if we're using this to display any type of
  IPL file". `objectName` deliberately left as the old string so
  saved dock-layout state isn't silently dropped.

  Added path visualization to the 3D world view, per Keith: "when
  displaying the paths in the viewpoint, I was expecting red lines
  and nodes. And a way to change the colour of the path lines in
  settings." New "Show Paths" checkbox in IPL Controls row 3 (the row
  reserved for exactly this back on Aug 1). `DFFViewport` gained
  `show_paths`/`set_show_paths`/`set_path_groups`/
  `set_path_line_color` and a `_draw_paths` method - unlit GL_LINE_
  STRIP per path group (red by default) plus amber GL_POINTS node
  markers, drawn on top of the world view alongside the existing 2DFX
  lights pass. New `_refresh_path_visualization` in map_workshop.py
  converts `loader.paths` (already-parsed PathGroup list) into plain
  coordinate lists, filtered by `_hidden_ipls` the same way instances
  already are, and wired into `_apply_ipl_visibility_filter` so
  toggling any IPL's visibility keeps the path overlay in sync
  automatically. Lines connect each group's nodes in their own
  on-disk sequential order, not the fully resolved Section 3/5/6 link
  graph - flagged as a known simplification, not a bug, in the
  method's own docstring.

  Path line colour is a real Settings entry now (Render Settings
  dialog, "Path Lines" group, same colour-picker pattern as the
  existing Background picker) - persisted via `map_settings.
  get/set('path_line_color', ...)`. Caught a real gotcha before it
  shipped silently broken: `MapSettings.set()` only actually stores a
  value `if key in self.DEFAULTS` - 'path_line_color' had to be added
  to `MapSettings.DEFAULTS` or every call to save the chosen colour
  would have silently no-op'd, forever. Verified the get/set/DEFAULTS
  gating logic in isolation to confirm the fix actually works and to
  document the trap for next time.

  GTA3/VC text-section path data only (matches loader.paths' own
  scope) - SA's real path data is the separate binary nodesXX.dat
  format (apps/methods/sa_path_parser.py, built earlier this
  session), not wired into this viewport pass yet.

  `ast.parse` clean on all changes; MapSettings get/set/DEFAULTS
  gating behavior smoke-tested in isolation.

- **Aug 15, 2026** — Fixed the real remaining gap in the PATH-file
  display fix from yesterday, per Keith's actual uploaded paths.ipl/
  paths2.ipl/paths3.ipl/paths4.ipl/paths5.ipl: "clicking on any of
  the path files, the IPL file display should also include path
  data." Yesterday's fix only handled a section being entirely
  *absent* from a file; these real files all have `inst`/`cull`
  sections that are genuinely *present but empty* (literally just
  "inst\nend" with nothing between them, per Keith's own uploads) -
  a different case that still fell straight through to zero rows
  with no explanation, since the section was technically "found".

  New `_ipl_section_has_data()` checks for at least one real data
  line, not just found-vs-not-found. New
  `_auto_switch_ipl_tab_if_empty()`, called right when a file is
  selected/opened (both single-click-select and double-click-open,
  since double-click delegates into the same click handler): if the
  currently active tab (INST by default) has no real data in the
  clicked file, automatically switches to the first enabled tab that
  does - never overrides a tab that's already showing something
  real, and no-ops for binary IPLs (no raw text to scan). Verified
  against all 5 of Keith's real uploaded files: every one correctly
  auto-switches from INST to PATH.

  Also caught and fixed a real display-completeness gap while
  verifying against the real data: node lines in Keith's real files
  genuinely have 13 comma-separated fields, not the 12 the documented
  Project Cerbera spec lists - `_parse_path_node` (verified, already
  shipped) already tolerates this correctly by only reading the first
  12 and ignoring the rest, but the raw-display table's 12-column
  'path' header was silently truncating that real 13th value. Added
  a 13th "Flag4?" column so the table shows everything actually in
  the file rather than hiding a value nobody's identified the meaning
  of yet.

  Verified end-to-end against all 5 real uploaded files: extraction,
  auto-switch decision, and the full group-header/node-row split all
  simulated directly against the actual file bytes (not synthetic
  data) - paths.ipl alone has 4106 groups / 49272 node rows across a
  53379-line path section, all correctly parsed. `ast.parse` clean.

- **Aug 15, 2026 (cont'd)** — Made Map Workshop's own settings
  (Fonts/Display/Performance/Preview/Loading/Map Assets/Navigation)
  available through IMG Factory's own Settings dialog when docked,
  per Keith: "the map workshop settings dialogue when standalone,
  which isn't available when it's docked with img factory, so we
  need a way to push those settings into img factory's settings, as
  extra tabs."

  Split the old ~630-line monolithic `_show_workshop_settings` (tab
  construction + Apply logic tightly coupled inside one method) into
  `_build_workshop_settings_tabs()` (builds all 7 tabs and the Apply
  closure, returns `(tabs, apply_settings)`) plus a thin
  `_show_workshop_settings` wrapper that's unchanged in behaviour for
  the standalone dialog. New `get_settings_contribution()` detaches
  each tab from the (otherwise-thrown-away) temporary QTabWidget and
  returns `(label, widget)` pairs plus the apply callback for another
  dialog to adopt - Qt reparents a widget automatically when it's
  added to a different QTabWidget, so nothing special is needed
  beyond `removeTab()` first.

  A real self-inflicted bug happened mid-refactor and got caught
  before shipping: a first-pass edit orphaned the `apply_settings`
  closure's body under an accidentally-duplicated `_apply_window_
  flags` method header - `ast.parse` caught the resulting
  IndentationError immediately, traced it back to the exact split
  point, and rewrote the whole region correctly in one pass. Verified
  via AST afterward: every affected method (`_build_workshop_
  settings_tabs`, `_show_workshop_settings`, `get_settings_
  contribution`, `_apply_window_flags`) now defined exactly once,
  and `_build_workshop_settings_tabs` has exactly one nested
  `apply_settings` def and one clean top-level `return`.

  IMG Factory side (`apps/utils/app_settings_system.py`): new
  `SettingsDialog._collect_settings_contributions()` scans tabs in
  `main_window.main_tab_widget`, duck-types for any widget exposing
  `get_settings_contribution()` (not Map-Workshop-specific by name -
  avoids a real circular import, since map_workshop.py already
  imports `SettingsDialog` from this same module - and means any
  future embedded tool, Model Workshop/COL Workshop/etc., gets the
  same integration for free just by implementing the same method).
  Wired into `__init__` (adds the extra tabs after the existing fixed
  ones) and `_apply_settings` (invokes each contributed apply
  callback, individually try/excepted so one tool's broken settings
  logic can't block another's or the dialog's own settings from
  saving). Confirmed all three real `SettingsDialog(...)` call sites
  in imgfactory.py resolve `self.main_window` to the actual main
  window instance (has `main_tab_widget`), so this activates
  correctly for the real docked-Settings-button flow.

  Verified: `ast.parse` clean on both files; every affected method
  confirmed defined exactly once via AST; full logic smoke-test of
  `_collect_settings_contributions` against 5 mocked scenarios (no
  main_window, main_window without main_tab_widget, a real
  contribution found among unrelated tabs, a broken contributor
  failing safely without blocking others, and duplicate-widget
  dedup) - all passed. Not yet tested in the real running app (no
  Qt/OpenGL environment available here).

- **Aug 15, 2026 (cont'd)** — Added a docked-only cog icon to IPL
  Controls row 3 (right side, after Show Paths), per Keith: "We could
  just add a cog SVG icon when docked. On the right of row 3. ipl
  control panel, but not to be shown in standalone." Opens IMG
  Factory's own Settings dialog (which now includes Map Workshop's
  own tabs, from the previous change this session) - visibility tied
  to the same is_docked/standalone_mode check the existing tearoff
  button already uses, kept in sync in _update_dock_button_visibility
  too so toggling dock/undock at runtime updates it correctly, not
  just at creation time.

  Real separate bug found and fixed while tracing which method to
  wire the cog to: `show_gui_settings` was defined twice in
  imgfactory.py - the real one (builds the full tabbed SettingsDialog
  with live theme switching) and, later in the same class body, an
  unrelated small "Right Panel Width Settings" dialog under the exact
  same method name. Python silently uses the second definition for
  every call, so the menu's "Customize Interface" action and
  _show_workshop_settings's own "fallback to regular settings" were
  both actually opening the narrow width-only dialog this whole time,
  never the real one. Renamed the shadowing method to
  show_panel_width_settings (its own functionality unchanged, still
  reachable under the new name) so show_gui_settings correctly
  resolves to the real dialog again - the cog icon itself is wired to
  main_window.show_settings specifically (a third, separately-defined
  and unaffected entry point, confirmed via grep to exist exactly
  once), not to the now-fixed but previously-broken show_gui_settings.

  `ast.parse` clean on both files; confirmed via AST that
  show_gui_settings/show_panel_width_settings/show_settings and every
  newly-added method are each defined exactly once.

- **Aug 15, 2026 (cont'd)** — Fixed the docked-only cog icon opening
  the wrong dialog, per Keith: "Right cog brings up theme settings
  from app_system_settings, that's wrong, it should be map_workshops
  settings from the left [settings] on the titlebar." It was wired to
  main_window.show_settings() (IMG Factory's global Settings dialog),
  now calls _show_workshop_settings() directly - the exact same
  method the titlebar's own left [Settings] button already calls.
  The get_settings_contribution/_collect_settings_contributions
  integration from earlier this session still stands unchanged - that
  still makes Map Workshop's tabs appear inside IMG Factory's own
  Settings dialog when opened through IMG Factory's own menu; the cog
  is just a direct shortcut to Map Workshop's own dialog instead,
  not a route through the bigger global one.

  Also fixed a small cosmetic bug visible in Keith's screenshot: the
  dialog's title read "Map WorkshopSettings" (no space) -
  App_name + "Settings" concatenated with nothing between them; now
  App_name + " Settings".

  `ast.parse` clean.

- **Aug 16, 2026** — Fixed a real, non-deterministic bug: "any models
  in some places disappeared, this sometimes happens, as you load a
  new part in, the preveus is effected" (Keith). Root cause: a
  reentrancy hazard in `_refresh_world_view`. It calls `QApplication.
  processEvents()` inside its own per-instance conversion loop (to
  keep "Loading model: X..." status feedback responsive during a
  slow pass) - that pumps the Qt event queue and can let an already-
  queued event run *before* the current call finishes. The clearest,
  most concrete trigger: `_on_time_flow_tick`'s QTimer (once a second
  while TOBJ Play is active) calls straight back into `_apply_ipl_
  visibility_filter` -> this same method. Without a guard, a nested
  call like that builds and applies its own complete `entries` list
  via `vp.set_world_instances()` while the outer call is still mid-
  loop; when the outer call resumes and finishes, it then overwrites
  the viewport with its OWN `entries` - potentially missing whatever
  became visible *during* the nested call, or (depending on which
  call happens to finish last) overwriting a more current render with
  a stale one. Either way: models vanish silently, no error, exactly
  matching the "sometimes" (timing-dependent) symptom.

  Split `_refresh_world_view` into a thin reentrancy-guarded wrapper
  (checks/sets `self._refresh_world_view_in_progress`, skips instead
  of proceeding if a call is already running) and `_refresh_world_
  view_impl` (the original ~215-line body, otherwise untouched -
  same parameter names throughout, no internal changes needed).
  Skipping a nested call is safe here: nothing it would have done is
  lost forever, whatever triggered it (a TOBJ tick, another visibility
  toggle, etc.) will follow up again shortly regardless, and the
  outer call's own `instances` argument already reflects the current
  desired visibility state (any state change like a hide/show toggle
  happens synchronously *before* triggering a refresh, not during
  one) - there's nothing for a nested call to catch that the outer
  call doesn't already have.

  Verified via AST: `_refresh_world_view` (60 lines, guard only) and
  `_refresh_world_view_impl` (215 lines, full original body) both
  defined exactly once, nothing else in the file affected. Smoke-
  tested the guard logic directly against a simulated reentrant-call
  scenario - confirmed the nested call is skipped, the outer call
  completes normally with consistent state, and the guard flag
  correctly resets afterward. `ast.parse` clean.

- **Aug 16, 2026 (cont'd)** — Two real, concrete perf fixes, per
  Keith comparing Map Workshop against MooMapper (an old GTA mapping
  tool he found again): "the speed of this program is impressive; it
  handles data way faster than map_workshop."

  1. `_populate_object_browser` (fires on every incremental IPL load,
     not just the initial full-world load) rebuilt a `Counter` over
     *every* instance loaded so far, every single time - cumulative
     O(n^2) work across a session as more parts load, each load a
     little slower than the last purely from re-scanning an ever-
     growing list, unrelated to how much that specific load actually
     needed. Now incremental: `self._object_instance_counts`/
     `_object_instance_count_up_to` track how far the Counter has
     already been built, a call only processes the new slice since
     last time. Self-corrects (rebuilds fully) if `loader.instances`
     is ever shorter than what's already counted; `_apply_loaded_world`
     also explicitly resets both for a genuine new-world load.

  2. `_rebuild_ipl_sections_rows` (fires on every binary-stream load,
     potentially several times per single incremental IPL load if it
     has multiple streams) re-opened and re-read the first 64 bytes
     of *every* known IPL file on every single call, to redetect a
     text/binary format that can't have changed since the last
     rebuild. Now cached per ipl_name (`self._ipl_format_cache`) -
     only genuinely-resolved results are cached (an empty/not-yet-
     known result is deliberately never cached, so a file that
     becomes resolvable once more of the world has loaded still gets
     a fresh chance on the next rebuild rather than being stuck
     unresolved forever). Also reset in `_apply_loaded_world`.

  Also softened the path-line rendering per Keith's MooMapper
  comparison ("notice how it blends in with the map"): thinner lines
  (2.0px -> 1.2px), smaller node markers (6px -> 3.5px), and alpha
  blending (0.75) so paths read as part of the scene rather than a
  bold opaque overlay. Left depth-testing disabled (paths still
  always draw on top, not genuinely occluded by geometry in front of
  them) - a bigger, separate change with real risk on hilly terrain
  if path Z values don't track ground height closely everywhere,
  worth trying only once this smaller change is confirmed to help.

  Both perf fixes smoke-tested in isolation against realistic
  multi-load scenarios (incremental accumulation, in-place mutation
  vs full rebuild, explicit new-world reset, stale-cache safety net
  for the Counter; cached-vs-unresolved-retry behavior for the format
  cache) - all correct. `ast.parse` clean on both files.

- **Aug 16, 2026 (cont'd)** — Fixed the real path-rendering topology
  bug behind Keith's screenshot: "testing paths, they don't look
  linked, nod to each other nod, instead one point" - long spurious
  lines fanning out from roughly one area, not a road-like network.

  Root cause: the previous renderer connected each path group's nodes
  in raw on-disk order (a simple polyline, node[i] to node[i+1]) -
  the WRONG topology, not just an incomplete one. Verified against
  Project Cerbera's own VC paths.ipl format documentation (fetched
  and cross-checked, including Cerbera's own worked "Group B"
  editing example, field-for-field): each node has a Type
  (0=Null/ignored - exists only as padding so every group has exactly
  twelve node slots; 1=External - links to another group's node
  sharing the exact same position; 2=Internal - links within this
  group) and a Next value that's a 0-11 INDEX into that SAME group's
  own fixed 12-node array - not "the next line in the file", and not
  necessarily sequential (Cerbera's real example has node 8 linking
  straight to node 11, skipping 9 and 10 entirely - exactly the kind
  of jump that would produce a long spurious line under the old
  file-order assumption).

  Rewrote the whole pipeline to build the real per-node link graph:
  `_refresh_path_visualization` now walks every non-Null node and, if
  its Next index is valid, emits one real edge (node[i] -> node
  [next_id]) - covers Internal and External nodes identically, since
  an External node still has its own ordinary Next field for
  continuing within its own group. Deliberately does NOT add a
  separate "connect matching External positions across groups" pass -
  tried it first, but by the format's own definition two matched
  External nodes are at the exact same position, so a segment between
  them is always zero-length/invisible; verified directly (a first
  draft with this pass produced only degenerate same-point segments).
  The visual continuity between groups comes for free anyway: each
  group's own segments already end/start at that shared coordinate,
  so two groups' lines visually meet without an artificial connector.

  `DFFViewport` reworked to match: `_path_groups` (ordered coordinate
  lists, drawn as GL_LINE_STRIP) replaced with `_path_segments` (flat
  list of independent (start,end) edge pairs, drawn as GL_LINES) -
  `set_path_groups` renamed to `set_path_segments`. Node markers now
  dedupe by exact position (a `set` of seen points) rather than
  drawing one marker per segment-endpoint occurrence, so a heavily-
  linked node doesn't get its dot drawn many times over.

  Verified end-to-end against Project Cerbera's own real worked
  example (hand-built as actual `PathNode`/`PathGroup` objects, not
  just isolated field values): the resulting 10 segments match
  exactly, in order, against the documented node-by-node link
  structure - `(0,1),(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),
  (8,11),(10,0)` - confirming the skip-around Next-index behavior
  (8->11) is now handled correctly, not just the simple sequential
  cases. Hidden-IPL filtering re-verified unaffected by the rework.
  `ast.parse` clean on both files; confirmed no other code anywhere
  in the project still references the old `_path_groups`/
  `set_path_groups` names.

- **Aug 16, 2026 (cont'd)** — Fixed settings not surviving to the
  next session, per Keith's screenshot of the Loading tab: "the
  settings function for showing debug and TXD size. saves those
  options, so the next session remembers them." Found the real,
  systemic cause auditing every `MapSettings.set()` call site (~18 of
  them): at least two were confirmed genuinely broken - the Render
  Settings dialog's path-line-colour picker, and `set_menu_
  orientation`'s menu_style/show_menubar - both called `.set()` but
  never followed up with `.save()`, so those specific choices were
  silently lost on next launch even though they worked correctly
  within the current session. Rather than patch each found instance
  (which only fixes the ones caught this time, not the next one
  someone adds), `MapSettings` now debounces and auto-saves inside
  `set()` itself - made a `QObject` purely to host a `QTimer`
  (`setSingleShot`, restarted on every `.set()` call so rapid-fire
  changes coalesce into one write ~800ms after the last change,
  rather than one write per call - checked first: `sectionResized`
  fires continuously during a live column-drag, not just on release,
  so an unconditional immediate save on every `.set()` would have
  been a real stutter risk). `save()` still exists as the explicit
  public API every existing call site already uses (now redundant
  but harmless - forces an immediate write, bypassing the debounce).

  Also added the new option Keith asked for alongside this: Settings
  > Loading > "Preload IMG archives on DAT load" (off by default) -
  `_preload_img_archives_to_os_cache`, triggered right after IMG
  indexing in `_apply_loaded_world` when loading a game's main .dat
  file (gta3.dat/gta_vc.dat/gta.dat). Sequentially reads through every
  referenced IMG archive once in 8MB chunks, discarding the bytes -
  the only purpose is letting the OS cache the file in RAM ahead of
  time, so later per-model reads during actual IPL loading hit cache
  instead of disk. Doubles as the status-bar feedback Keith separately
  asked for ("any feedback besides a long pause helps") - explicit
  per-archive messages ("Preloading gta3.img (128.4 MB)... 45%")
  rather than a silent gap. Never fatal - an unreadable/missing
  archive is skipped and logged, loading proceeds regardless.

  Verified: debounce/coalescing logic smoke-tested against a mock
  replicating real Qt `QTimer` start/restart semantics (rapid-fire
  calls produce exactly one write with the final value, not one per
  call; explicit `.save()` still writes immediately; a fresh instance
  correctly loads persisted values; the exact original bug pattern -
  `.set()` with no `.save()` at all - now persists correctly too).
  Chunked preload progress logic verified against a real 20MB+ temp
  file (correct byte-exact total, correct percentage sequence,
  graceful OSError handling for a missing file). `ast.parse` clean;
  confirmed via AST no duplicate class/method definitions introduced.
  PyQt6 unavailable in this sandbox, so nothing here has run inside a
  real Qt event loop - worth confirming on Keith's end that the
  debounced save timer and the preload's processEvents() calls behave
  as expected in the actual running app.

- **Aug 16, 2026 (cont'd)** — Added the second option Keith asked
  for: DAT Browser's right-click menu on a main .dat file now has
  "Load with Map Workshop, preload IMG(s) file…" alongside the
  existing "Load with Map Workshop…" - per Keith: "in Dat Browser,
  right click dat file, open in map workshop, add another option to
  open in map workshop, preload img(s) file". Forces the IMG-preload-
  to-OS-cache behaviour (added earlier this session) for just that
  one load, independent of - and without changing - the persistent
  Settings > Loading > "Preload IMG archives on DAT load" checkbox, a
  quick one-off choice right from the menu instead of needing to
  visit Settings first.

  New `force_preload_img` parameter threaded through the full call
  chain: `dat_browser.py`'s new `_load_dat_in_map_workshop_preload_
  img` -> `imgfactory.py`'s `open_map_workshop_docked` ->
  `map_workshop.py`'s `open_map_workshop` -> `_load_game_dat_file`,
  which stores it as `self._force_preload_img_once` right before
  calling `_apply_loaded_world` - consumed (read then immediately
  reset to False) there, alongside the existing persistent-setting
  check, so it only ever applies to the one load that requested it
  and never silently leaks into a later, unrelated load.

  Also logged a new TODO per Keith: "Every UI change, splitter
  position, and cell size should be remembered" - a broader, systemic
  ask beyond this session's settings-persistence and column-width
  fixes (splitter positions, dock geometry, tab order, collapsed-
  header state, and any other table's column widths not already
  covered). Substantial scope, not started.

  `ast.parse` clean on all three changed files (`map_workshop.py`,
  `imgfactory.py`, `dat_browser.py`); confirmed via AST no duplicate
  definitions introduced in any of them; smoke-tested the consume-
  once flag logic directly (a forced load preloads that one time,
  the flag resets immediately after, a subsequent normal load does
  NOT inherit the forced behaviour).

- **Aug 16, 2026 (cont'd)** — Reworked keyboard camera controls per
  Keith: "the arrow keys dont pan or move the view left, right, up or
  down; the arrow keys rotate instead. We need to be able to operate
  the tools with keys, zoom in and out; it could be the numpad + -.
  A new tab is needed in map workshop settings to define keys."

  Arrow keys now pan (previously rotated - an Aug 1 2026 addition per
  Keith's own earlier request, now corrected); numpad 4/6/8/2 keep
  rotating (unchanged); numpad +/- zoom (new). All camera-key
  handling in `DFFViewport` rewritten from a hardcoded rotate-only
  dict to a configurable, action-based system:
  `DEFAULT_KEY_BINDINGS`/`KEY_BINDING_LABELS` (module-level), each
  binding `{'key': int(Qt.Key), 'numpad': bool}`; `self._key_bindings`
  (defaults to `DEFAULT_KEY_BINDINGS`, overridable via new
  `set_key_bindings()`, which merges partial overrides over the
  defaults so an older/partial saved dict never silently unbinds an
  action added later). `keyPressEvent`/`keyReleaseEvent` reworked to
  look up the pressed key+numpad-state against the configured
  bindings generically. One shared repeating timer/tick handler
  (`_ensure_camera_key_timer`/`_on_camera_key_tick`, replacing the
  old rotate-only ones) dispatches every currently-held action to its
  pan/rotate/zoom delta each frame - several can be held at once
  (e.g. two arrow keys for diagonal pan). Extracted the yaw-
  compensated pan math (previously only in `mouseMoveEvent`'s middle-
  drag handler) into a shared `_apply_pan_step()` so keyboard pan and
  mouse-drag pan are guaranteed identical, not two separately-
  maintained copies of the same formula.

  New Settings > Keybindings tab (`_build_workshop_settings_tabs`) -
  one row per action, each a click-to-rebind `_KeyCaptureButton`
  (grabs keyboard on click, captures the next key+numpad-state, Esc
  cancels) plus a per-row Reset-to-default button. Labels/actions
  read from `DEFAULT_KEY_BINDINGS`/`KEY_BINDING_LABELS` directly
  (imported, not duplicated) so this tab can never list an action the
  viewport doesn't actually know about. `apply_settings` reads every
  button's live `.binding()`, stores only the ones that genuinely
  differ from default in `MapSettings` (`viewport_key_bindings`, new
  DEFAULTS entry) - an empty/partial saved dict always means "use the
  built-in defaults for anything not explicitly listed", never
  "unbind everything else". Saved bindings are restored once at
  `preview_widget` construction (same pattern already used for
  texture-downscale settings) so a previous session's customisation
  takes effect immediately on next launch, not only after re-opening
  Settings and clicking Apply.

  Caught and removed a piece of genuinely dead code in my own first
  draft before shipping: a `pending_bindings` dict was being written
  by `key_captured`/reset but never actually read anywhere -
  `apply_settings` already reads each button's live state directly
  via `.binding()`, making the extra dict pure unread overhead.
  Removed rather than left in as confusing unused state.

  Verified: binding-match logic (arrows pan regardless of numpad
  state; numpad 4 rotates only WITH the numpad modifier, never a
  top-row "4"; numpad +/- zoom by default, not regular +/-) and
  `set_key_bindings`' merge-over-defaults behaviour smoke-tested
  directly. Reset-button closures verified NOT to suffer the classic
  late-binding bug (each row's Reset correctly restores THAT row's
  own default, confirmed by simulating a multi-action rebind-then-
  reset sequence). Delta computation (only genuinely-changed actions
  get saved) verified against a realistic multi-action scenario.
  `ast.parse` clean; confirmed via AST no duplicate class/method
  definitions introduced. Confirmed no stale references remain
  anywhere in the project to the old `_rotate_keys_held`/
  `_rotate_key_timer`/`_on_rotate_key_tick`/`_ensure_rotate_key_timer`
  names this replaced. PyQt6 unavailable in this sandbox, so none of
  the actual widget/event-loop behaviour has run for real - worth
  confirming on Keith's end that key capture, the timer-driven
  continuous pan/zoom, and settings persistence all feel right in
  the real app.

- **Aug 16, 2026 (cont'd)** — Added the Path Group Editor, per Keith:
  "we need to add edit functions for connecting, moving, adding, or
  deleting nodes, a dialog like the object editor." New "Edit Path
  Group..." right-click action in the IPL File Display table, shown
  only when the PATH tab is active.

  New `_PathGroupEditDialog` (QDialog): a 12-row table (Slot/Type/
  Next/X/Y/Z/Median/Left/Right/Flag1-3), one row per node slot -
  always exactly 12, per the format's own rule (already verified
  against Project Cerbera's real worked example earlier this
  session). Type is a dropdown (0 Null/1 External/2 Internal); Next
  is a 0-11 (or -1) spinbox - this IS the "connecting" mechanism,
  matching the real format exactly (a node's own Next field, not a
  separate visual connect-tool). "Adding" a node = turning a Null row
  into a real one by setting its Type/X/Y/Z. "Deleting" = the Delete
  Selected Row button, resets a row back to Null with every field
  zeroed - the same shape a real null-padding line has on disk.
  "Moving" = editing X/Y/Z directly. Group header (the two-field A/B
  identifier line) editable in its own row above the table.

  Apply edits the real, live `PathGroup`/`PathNode` objects in
  `loader.paths` in place (the same objects the viewport's path
  overlay and the IPL File Display table already read from) and
  immediately calls `_refresh_path_visualization()` +
  `_refresh_ipl_inst_file_panel()`, so a change is visible right
  away. Does NOT write back to the .ipl file on disk - matching the
  Item Editor Dialog's own honest-stub Save button and the project's
  broader "write-back infrastructure not built yet" TODO; the status
  bar message after Apply says so explicitly ("...updated in memory -
  not yet saved to file") rather than implying more than it does.

  New `_find_path_group_for_line(display_name, line_no)` resolves a
  clicked table row to its real PathGroup: a node row doesn't carry
  its own group reference, only its own source-file line number, so
  this finds the group whose header line is the closest one at-or-
  before that row's line, scoped to the currently-selected file only
  (two different files' groups never cross-match even at the same
  line number). Handles a group-header row matching itself correctly,
  and a row past the last known group still resolving to that last
  group (its own trailing node rows).

  Verified against real `PathNode`/`PathGroup` classes (not mocks):
  node-list padding to exactly 12 slots for a group with fewer parsed
  nodes, the full rebuild-all-12-from-UI-values Apply logic, and the
  Delete-to-Null reset shape - all correct. `_find_path_group_for_line`
  smoke-tested against a realistic 3-group-plus-different-file
  scenario (mid-group node rows, exact header-row match, past-last-
  group fallback, cross-file isolation) - all correct. `ast.parse`
  clean; confirmed via AST no duplicate class/method definitions.
  PyQt6 unavailable in this sandbox, so none of the actual dialog/
  table-widget behaviour has run for real - worth confirming on
  Keith's end that the cell-widget table renders and edits correctly
  in the live app.

- **Aug 16, 2026 (cont'd)** — Added GTA III's own IDE-embedded path
  parsing, per Keith: "gta3 game files need special treatment; the
  IPL path data is stored within the .ide map files" (real sample:
  comse.ide/comSE.ipl). Confirmed via Project Cerbera's own "PATH
  (IDE Section)" documentation and cross-checked field-for-field
  against the real file - this was previously completely unparsed:
  "path" was already a listed valid IDE_SECTIONS keyword, but
  `IDEParser._parse_line` had no branch for it at all, so every path
  line in every GTA III .ide file silently produced nothing.

  Real format, genuinely different from VC/SA's own path node shape
  (not just a shorter version of it): a group header line is
  `GroupType, Id, ModelName` ("ped, 1440, scraperkb3_nit" or "car,
  ...", both confirmed present in the real file) - paths are bound to
  a specific OBJS model definition, not freestanding, per Cerbera:
  "GTA III uses an IDE-related paths system, which binds paths to
  certain objects." Node lines (9 fields, tab-indented, same raw-
  whitespace distinguishing convention as VC's own path parsing) are
  `NodeType, NextNode, IsCrossRoad, XRel, YRel, ZRel, Median,
  LeftLanes, RightLanes` - XRel/YRel/ZRel are relative to wherever
  that model_id is actually PLACED via an INST line, not absolute
  world coordinates like VC/SA.

  New `IDEPathNode`/`IDEPathGroup` dataclasses (apps/methods/
  gta_dat_parser.py, near the existing VC-style PathNode/PathGroup -
  kept as separate classes, not reused, given the real shape
  difference). `IDEParser` gained a `current_ide_path_group` state
  machine (mirroring `IPLParser`'s own `current_path_group` handling
  for VC paths) plus `_parse_ide_path_group_header`/`_parse_ide_path_
  node`, and a new `self.ide_paths` list. Wired into `GTAWorldLoader`
  (`self.ide_paths` init + cleared in `_reset()` + accumulated in
  `_load_ide`, the one real consumption site - the other 4 `IDEParser`
  instantiations in the file are all `IDEDatabase`'s lightweight
  standalone model lookups, which don't need path data).

  Verified end-to-end against the real uploaded comse.ide: 14 groups
  parsed (3 ped + 11 car, matching both GroupTypes appearing in real
  data), first group's header and all 12 of its nodes checked field-
  for-field including a Null (type 0) node's exact values, model_id/
  model_name confirmed on a `car` group too. Also confirmed zero
  regression in ordinary objs/tobj/2dfx parsing (136/9/151 entries -
  exactly matching the file's own section boundaries) - the new path-
  section branch sits alongside the existing per-line dispatch
  without disturbing it. `ast.parse` clean.

  NOT yet done, logged to TODO.md: resolving these relative node
  positions to actual world-space coordinates (needs finding each
  group's matching INST placement(s) and applying that instance's own
  position+rotation transform - multiple placements of the same
  model each get their own copy of the path) - so GTA III paths don't
  render in the viewport yet, only parse correctly. Also logged: the
  path-format conversion/export-to-paths.ipl feature (depends on the
  same world-space resolution step), a floating-dialog "stay on top"
  toggle, and the deliberately-deferred flight.dat/flight2.dat/
  flight3.dat/spath0.dat files.

- **Aug 16, 2026 (cont'd)** — Wired up standalone .zon file viewing,
  per Keith: "just need to wire the zon files, so I can click and
  view them." Zone parsing itself was already correct (`_parse_zone`
  verified against all 3 of Keith's real .zon files earlier this
  session), and `IPLParser.parse()` is fully format-agnostic - it
  just reads whatever section keywords it finds regardless of a
  file's own extension - so nothing needed fixing there. The actual
  gap was purely wiring: .zon files aren't referenced by a .dat's own
  IPL directives the way normal IPLs are, so they never appeared in
  `loader.available_ipls` for the existing click/eye-icon/view
  pipeline to find.

  New "Open Zone..." button in the IPL tab's title row (alongside
  Open/Close/New/Delete) - `_on_ipl_tab_open_zone_clicked` lets Keith
  pick one or more .zon files via a file dialog and registers each as
  a real `DATEntry` in `loader.available_ipls` (the exact same shape
  every normal IPL entry already has), plus the matching `_ipl_
  display_to_stem`/`_ipl_display_order`/`_hidden_ipls` bookkeeping
  every other IPL row needs - so the whole existing table/click/view
  machinery works for a .zon file completely unmodified. New entries
  start hidden (matching every other IPL's default state), and
  re-opening an already-added file is a harmless no-op rather than
  creating a duplicate row.

  Also fixed a real, separate display gap while wiring this: the IPL
  File Display table's `headers_by_type` had no `'zone'` entry at
  all - the exact same gap `'path'` had before it was fixed earlier
  this session - silently falling back to inst's wrong 13-column
  layout. Added the correct columns (Name/Type/Min X-Z/Max X-Z/
  Island/Text Key, mirroring `_parse_zone`'s own raw field order).

  Verified end-to-end against real data: parsed Keith's actual
  `info.zon` through the unmodified `IPLParser` pipeline (165 zones,
  first entry checked field-for-field), and the full registration
  logic (available_ipls/_ipl_display_to_stem/_ipl_display_order/
  _hidden_ipls) simulated against all 3 real uploaded .zon files -
  correct stems, correct paths, correct hidden-by-default state, and
  confirmed re-adding a file doesn't create a duplicate row.
  `ast.parse` clean; confirmed via AST no duplicate method
  definitions introduced.

- **Aug 16, 2026 (cont'd)** — Fixed the real, confirmed `_parse_cull`
  bug, per Keith: "continue with the cull files next." The previous
  version expected 7 fields (a center/width/height box) - real
  cull.ipl lines have 11 fields, two genuine corner points, not a
  width+height pair at all. Confirmed against three independent wiki
  sources (GTA Wiki Fandom, GTAMods, Grand Theft Wiki all agree):
  `CenterX, CenterY, CenterZ, X1, Y1, Z1, X2, Y2, Z2, Flags,
  WantedLevelDrop`. New `CullEntry` dataclass (replacing a plain
  dict, matching the established `GrgeEntry`/`EnexEntry` pattern) -
  `self.culls` type hints updated to `List[CullEntry]` everywhere
  except `BinaryIPLParser`'s (left as `List[Dict]`, genuinely unused -
  binary IPL cull parsing isn't implemented at all, a separate,
  unstarted feature, not touched here).

  Real finding along the way: the viewport Keith actually uses
  (`DFFViewport`) has never had ANY cull-box rendering at all - the
  existing `_toggle_cull_boxes`/cull-box drawing only ever reached
  `MapViewport` (`depends/map_viewport.py`), a separate class only
  reachable through the disabled 4-Pane View feature. Built real
  rendering into `DFFViewport` instead, mirroring the already-proven
  Show Paths pattern exactly: `show_cull_boxes`/`_cull_boxes`/
  `_cull_box_color` state, `set_cull_boxes`/`set_show_cull_boxes`/
  `set_cull_box_color` setters, `_draw_cull_boxes` (wireframe box
  corner-to-corner using the box's own real two corners directly, no
  center/width assumption needed since the real shape is just two
  points) wired into `paintGL` alongside `_draw_paths`. New "Show
  Cull Zones" checkbox in IPL Controls row 3 next to Show Paths;
  `_refresh_cull_box_visualization`/`_on_show_cull_boxes_toggled` in
  map_workshop.py mirror `_refresh_path_visualization`'s exact
  structure (much simpler here - a cull zone is an independent box,
  no per-node graph to resolve), wired into `_apply_ipl_visibility_
  filter` so toggling any IPL's visibility keeps cull boxes in sync
  automatically, same as paths already do.

  Also fixed the same `headers_by_type` gap `'path'`/`'zone'` both
  had before they were fixed - `'cull'` was missing entirely,
  silently falling back to inst's wrong 13-column layout in IPL File
  Display. Added `cull_box_color` to `MapSettings.DEFAULTS` (amber-
  yellow default, distinct from paths' red) - caught this before it
  shipped broken: `MapSettings.set()` only actually stores a value if
  the key is pre-registered in `DEFAULTS`, the exact bug class fixed
  systemically earlier this session, so this had to be added or the
  colour picker would've silently never saved anything.

  Also cleaned up an unrelated leftover duplicated docstring
  paragraph spotted in `_refresh_path_visualization` while working
  nearby.

  Verified end-to-end against Keith's real cull.ipl: 631 cull zones
  parsed correctly (previous version parsed 0 usable entries, since
  every real line has 11 fields but the old code required at least 7
  meaningfully-wrong ones), first entry checked field-for-field. Cull
  box coordinate conversion (`CullEntry` -> `(x1,y1,z1,x2,y2,z2)`
  tuple) and hidden-IPL filtering both verified directly against the
  real parsed data; `cull_box_color`'s `DEFAULTS` registration/get/
  set round-trip verified in isolation. `ast.parse` clean on all
  three changed files; confirmed via AST no duplicate method
  definitions introduced anywhere.

- **Aug 16, 2026 (cont'd)** — Fixed zones not showing in the
  viewport, per Keith: "ive loaded zon files, these show in the IPL
  File Display and the ZON tab is highlighted, but I cant see them in
  the viewpoint." Real gap: zone loading/table-display was wired
  earlier this session, but nothing ever pushed parsed zones to the
  3D view - zones never had ANY viewport rendering at all before,
  unlike cull (which at least had unreachable dead code).

  Added real zone-box rendering to `DFFViewport`, mirroring the cull-
  box pattern from moments ago exactly: `show_zone_boxes`/`_zone_
  boxes`/`_zone_box_color` state, `set_zone_boxes`/`set_show_zone_
  boxes`/`set_zone_box_color` setters, `_draw_zone_boxes` wired into
  `paintGL`. Refactored the box-drawing loop itself into a shared
  `_draw_wireframe_boxes(boxes, color)` helper while adding this,
  rather than duplicating the same loop a second time - `_draw_cull_
  boxes` now just calls it too. New "Show Zones" checkbox in IPL
  Controls row 3 (sky blue, distinct from cull's amber and paths'
  red); `_refresh_zone_box_visualization`/`_on_show_zone_boxes_
  toggled` in map_workshop.py mirror the cull versions, wired into
  `_apply_ipl_visibility_filter`.

  Real, separate gap found and fixed while wiring this: `_parse_zone`
  was the one section type in this whole parser that never tracked
  `source_ipl`/`line_no` at all (every other type - inst/cull/grge/
  enex/path - already does) - meaning zone boxes couldn't have been
  filtered by which IPL is currently hidden/visible even once
  rendering existed. Added both fields to `_parse_zone`'s signature
  and returned dict, updated its one call site.

  Also merged the "Open Zone..." button into a single "Open File..."
  button, per Keith: "open zone, and open ipl, both can be merged,
  looking for ipl, and zon files" - one file dialog now accepts
  either `.ipl` or `.zon` files (combined filter, both still
  selectable individually too), `_on_ipl_tab_open_zone_clicked`
  renamed to `_on_ipl_tab_open_file_clicked` and widened accordingly
  - both file types are read through the exact same format-agnostic
  `IPLParser.parse()` path, so nothing else needed to change; the
  registered `DATEntry`'s `directive` is set to `"ZONE"` or `"IPL"`
  based on the picked file's own extension.

  Also added `zone_box_color` to `MapSettings.DEFAULTS` (sky blue
  default) - caught before shipping broken, the same `set()`-silently-
  no-ops-on-unregistered-keys trap fixed systemically earlier this
  session.

  Verified end-to-end against real data: zone box conversion and
  hidden-IPL filtering checked directly against the real parsed
  info.zon (165 zones, first entry's corners checked field-for-
  field), `source_ipl`/`line_no` tracking confirmed correct (zero
  regression on the existing fields), and the file-type-detection
  logic for the merged Open File... dialog verified for both
  extensions including a mixed-case `.ZON`. `ast.parse` clean on all
  three changed files; confirmed via AST no duplicate method
  definitions; confirmed no stale references anywhere in the project
  to the old `_on_ipl_tab_open_zone_clicked`/`open_zone_btn` names.

- **Aug 16, 2026 (cont'd)** — Added occlusion zone support, per
  Keith: "lets add occl next" (continuing the cull/zon viewport work).
  "occl" wasn't even a recognised VC section keyword before this -
  confirmed via GTAMods/Grand Theft Wiki (word-for-word agreement):
  `OCCL is a section... in Vice City, San Andreas, and GTA IV` - VC
  was simply left off `IPL_SECTIONS` by mistake. Fixed, plus new
  `_parse_occl` (7 fields: MidX, MidY, BottomZ, WidthX, WidthY,
  Height, Rotation) and `OcclEntry` dataclass, wired into `IPLParser`
  and `GTAWorldLoader` (`self.occls`, both accumulation sites,
  cleared in `_reset`).

  Viewport rendering needed real rotation math, not just reused
  cull/zone box drawing - an occlusion zone rotates around its own
  vertical (Z) axis (unlike cull/zone's plain axis-aligned two-corner
  boxes), so `_draw_occl_boxes` computes all 4 XY corners explicitly
  from half-extents rotated by `rotation` degrees around (mid_x,
  mid_y), extruded from bottom_z to bottom_z+height. Verified the
  rotation math by hand before trusting it: a 10x4 rectangle rotated
  90 degrees correctly becomes 4x10 (dimensions swap exactly as they
  should), 0-degree rotation matches a plain axis-aligned box,
  off-center placement offsets correctly. Flagged honestly in the
  method's own docstring: the rotation *direction* (clockwise vs
  counter-clockwise) isn't independently confirmed against real
  in-game behaviour - only the field values/parsing are verified,
  the rendering interpretation is a reasonable standard-rotation
  guess, not a confirmed fact, same honesty standard as the GTA III
  IDE path coordinates' unconfirmed scale factor.

  New "Show Occlusion" checkbox (pink) in IPL Controls row 3,
  `_refresh_occl_box_visualization`/`_on_show_occl_boxes_toggled`
  mirroring the cull/zone pattern exactly, wired into `_apply_ipl_
  visibility_filter`. Added `occl_box_color` to `MapSettings.
  DEFAULTS` (same silent-noop trap avoided each time this session)
  and the missing `headers_by_type['occl']` entry (same gap path/
  zone/cull all had before being fixed) so IPL File Display shows the
  correct columns instead of the wrong inst-style layout.

  Verified end-to-end against Keith's real occlu.ipl: 344 zones
  parsed correctly, first entry checked field-for-field. Box
  conversion and hidden-IPL filtering verified directly against the
  real parsed data; `occl_box_color`'s DEFAULTS round-trip verified
  in isolation. `ast.parse` clean; confirmed via AST no duplicate
  method definitions. This closes out cull+zon+occl viewport
  visibility from the original multi-part request - editing dialogs
  for cull/zon/occl (matching the Path Group Editor) remain open in
  TODO.md.

- **Aug 16, 2026 (cont'd)** — Made path line thickness, node size, and
  node colour all configurable in Settings > Render, per Keith: "I
  like the path colors as a default but under rander in settings,
  line thinkness, and node circle size, and color change option."
  Line colour was already configurable (added earlier this session);
  node colour was previously fixed amber with no way to change it -
  widened to match, since Keith explicitly grouped it with node size
  in the same request.

  `DFFViewport` gained `set_path_node_color`/`set_path_line_
  thickness`/`set_path_node_size` (alongside the existing `set_path_
  line_color`), `_draw_paths` reads `self._path_line_thickness`/
  `self._path_node_size` instead of the previously hardcoded 1.2px/
  3.5px. `MapSettings.DEFAULTS` gained `path_node_color` ((255, 204,
  0) - the exact same amber that was hardcoded before, just now
  overridable), `path_line_thickness` (1.2), `path_node_size` (3.5).

  Render Settings dialog's "Path Lines" group rebuilt from a single-
  row colour picker into a proper form: Line Colour, Node Colour
  (new), Line Thickness (new spinbox, 0.1-20.0), Node Size (new
  spinbox, 0.1-30.0). `_apply()` saves and pushes all four to the
  live viewport immediately; `_refresh_path_visualization` re-applies
  all four on every refresh (not just after visiting Settings), same
  self-healing pattern the line colour already used, so a previous
  session's choices take effect immediately on next launch.

  Verified: `DEFAULTS` registration/get/set round-trip for all three
  new keys (the same silent-noop trap caught and fixed several times
  already this session), and confirmed the new `path_node_color`
  default (255, 204, 0) converts back to the exact original hardcoded
  float amber (1.0, 0.8, 0.0) - `204/255 == 0.8` exactly, no drift
  introduced by the int/float round-trip. `ast.parse` clean on both
  files; confirmed via AST no duplicate method definitions.

- **Aug 16, 2026 (cont'd)** — Made GTA III paths actually show when
  Show Paths is ticked, per Keith: "GTA3 paths arnt showing, i know I
  am selecting ipl files, to load map sections, but the path entires
  are in the ide files, so how do we load those? for gta3 parse the
  paths from the ide files, and show them when show paths is
  ticked?" The IDE-embedded parsing itself was already correct and
  verified (`loader.ide_paths`, added earlier this session) - it was
  just never resolved to world-space or fed into the viewport at all.

  Unlike VC's paths (already absolute coordinates), a GTA III path
  group's node positions are RELATIVE to wherever its own model_id is
  actually PLACED via an INST line - resolving them to world space
  needed real position+rotation transform math, not just a coordinate
  copy. `_refresh_path_visualization` now also: builds an `instances_
  by_model` lookup from the currently-visible (non-hidden) instances;
  for every `IDEPathGroup`, finds every matching placed instance
  (multiple placements of the same model each get their own
  independent copy of the path, correctly transformed by THAT
  instance's own position/rotation); rotates each node's local offset
  by the instance's effective (conjugated) rotation - same convention
  already established this session for rendering the instance's own
  geometry - and adds the instance's position to get the final world
  coordinate; builds the same Type/Next segment graph VC paths
  already use. Results are appended into the same `segments` list VC
  paths build, so "Show Paths" renders both formats together
  transparently through the one existing pipeline.

  New `_rotate_vector_by_quaternion` - the same rotation matrix
  `DFFViewport._quat_to_gl_matrix` already builds for `glMultMatrixf`,
  derived into an equivalent CPU-side single-vector form. Verified
  mathematically identical before trusting it for real data: cross-
  checked against a full 4x4 column-major matrix multiply using the
  actual existing function, 20 random quaternion/vector pairs, max
  error <1e-6; also confirmed a pure 90-degree yaw rotation only
  affects X/Y and leaves Z unchanged, as it should.

  Verified end-to-end using both real uploaded files together
  (comse.ide's path groups + comSE.ipl's matching instance
  placements) - group 0 (model_id 1440, "scraperkb3_nit") correctly
  matched to its one real placed instance, its first node's local
  offset (-499, -109, -1) resolved to a real, plausible world
  position via the instance's actual real position and rotation, and
  the full group produced 6 correctly-connected segments. `ast.parse`
  clean; confirmed via AST no duplicate method definitions.

- **Aug 16, 2026 (cont'd)** — Switched cull/zone/occlusion boxes from
  plain wireframe to ghosted, semi-transparent filled boxes, per
  Keith: "instead of wireframe boxes, we go for ghosted, see through
  boxes, like the semi solid." Matches the existing collision Semi-
  Solid render mode's own visual convention (`_draw_solid`'s
  `alpha_multiplier` path: filled alpha-blended triangles plus a
  subtle, more-opaque edge pass for definition) rather than
  inventing a new look.

  New shared `_draw_ghosted_box_from_corners(corners_xy, z1, z2, r,
  g, b)` draws one box (6 filled alpha-blended `GL_QUADS` faces plus
  a slightly more opaque wireframe outline) from 4 already-computed
  (x,y) corner points and a z1/z2 extrusion range - used by both the
  axis-aligned case (`_draw_ghosted_boxes`, replacing the old `_draw_
  wireframe_boxes`, cull/zone derive their 4 corners from the box's
  own two opposite points) and the rotated occlusion case (`_draw_
  occl_boxes`, unchanged rotation math, just now hands its already-
  computed rotated corners to the same shared drawer instead of
  drawing wireframe-only) - only the corner computation differs
  between an axis-aligned box and a rotated one, not how it's
  actually drawn once corners exist, so the fill+outline logic isn't
  duplicated three times.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions and no stale references anywhere in the project to the
  removed `_draw_wireframe_boxes` name.

- **Aug 16, 2026 (cont'd)** — Pulled Keith's own manual edits first
  (checkbox label shortening - Paths/Cull/Zon/Occlusion instead of
  the longer "Show..." labels; a partial in-progress merge of the
  Open/Open File buttons, left as his own WIP with his own TODO
  markers - not touched further here; noted TODOs about relocating
  row 3's checkboxes and combining Open with IDE-file support too).
  Investigated his new "Cull does not show in the IPL file display"
  note: traced the full real-data pipeline (raw-text section
  extraction, has-data check, header layout, tab wiring) against his
  actual uploaded cull.ipl - everything checks out correctly, 632
  real data lines correctly found and would display with the right
  11-column layout. Likely observed before his local copy had the
  earlier `headers_by_type['cull']` fix from this session - flagged
  for Keith to retest against latest rather than guessed-and-changed
  further without being able to reproduce it.

  Added a Zon-specific render style dropdown, per Keith: "in zons,
  the render dropdown could show, Zon - Ghosted, Zon - Wireframe,
  Zon - translucent" - new exclusive `QActionGroup` in the same
  Render dropdown (after the Col overlay options), three mutually-
  exclusive choices persisted via `MapSettings` (`zone_render_style`,
  default `'ghosted'`) and applied immediately (no separate Apply
  step, matching how the Col toggles already work) plus restored on
  next launch (same pattern as the keybindings restore). Scoped to
  zone boxes specifically, per Keith's own "in zons" framing - cull/
  occlusion boxes keep their fixed ghosted look, not touched.
  `DFFViewport._draw_zone_boxes` now dispatches on `self._zone_
  render_style`: `'wireframe'` draws edges only (the pre-ghosted
  look, kept as an explicit choice rather than removed);
  `'translucent'` reuses the ghosted fill path but more see-through
  (alpha 0.16 vs 0.32) and with the edge outline skipped entirely
  (new `draw_outline` param on `_draw_ghosted_box_from_corners`).

  Added small solid corner-sphere handles to every cull/zone/occlusion
  box, per Keith: "the boxes we see need little solid spheres on each
  corner so you can move the 6 sides, bigger, shorter, longer, deeper,
  higher." New `_draw_box_corner_spheres` draws a real `gluSphere` (GLU
  already imported wildcard at module level) at each of a box's 8
  corners, low poly count (6 slices/4 stacks) since a scene can easily
  have hundreds of boxes visible at once. Visual handles ONLY for now
  - actually clicking and dragging a corner to resize the box is a
  separate, larger follow-up, the same class of feature as the
  project's already-open "gizmo-based free object movement" TODO
  (mouse picking, drag math, live data mutation - none of which exist
  yet for anything in this viewport) - logged clearly rather than
  implied as done.

  Verified: `zone_render_style`'s `DEFAULTS` round-trip, the
  unrecognised-value-falls-back-to-ghosted guard, and the fill-alpha/
  outline dispatch per style, all in isolation. `ast.parse` clean;
  confirmed via AST no duplicate method definitions. PyOpenGL isn't
  available in this sandbox, so `gluSphere`/`gluNewQuadric` calls
  themselves haven't run for real - these are long-standing, standard
  GLU function names, but worth confirming they render correctly (and
  checking actual frame-rate impact with many boxes visible at once)
  on Keith's end.

- **Aug 16, 2026 (cont'd)** — Fixed a real bug, per Keith: "there is
  a bug where, loading zons, or other ipl files, seems to partly
  remove other objects." A second, related bug to the reentrancy fix
  already shipped earlier this session for `_refresh_world_view` -
  that fix guards `_refresh_world_view`'s OWN internal work from
  running twice concurrently, but `_apply_ipl_visibility_filter`
  itself (the caller) was never guarded, and its OTHER steps
  (`_populate_instance_list`, `_refresh_2dfx_lights`, and this
  session's newer path/cull/zone/occl overlay refreshes) weren't
  covered by the earlier fix at all.

  Real mechanism: `_apply_ipl_visibility_filter` computes `visible`
  ONCE at the top, before calling the slow `_refresh_world_view`
  (which pumps the Qt event queue via `QApplication.processEvents()`
  partway through its own per-instance loop). If something re-enters
  `_apply_ipl_visibility_filter` during that pump (e.g. double-
  clicking a just-added zon/ipl row, or a TOBJ tick), the nested call
  computes its own fresh `visible` snapshot and - since `_refresh_
  world_view`'s own guard makes ITS internal step a no-op the second
  time - finishes fast, applying that fresh snapshot to `_populate_
  instance_list`/`_refresh_2dfx_lights`/etc. Control then returns to
  the outer call, which resumes and finishes its OWN `_refresh_world_
  view` using its now-STALE `visible` (captured before whatever
  changed), then goes on to call `_populate_instance_list`/`_refresh_
  2dfx_lights` again with that same stale set - overwriting the
  nested call's more current result. The path/cull/zone/occl overlay
  refreshes never showed this symptom (they re-read current state
  fresh every call, not a captured parameter), which is exactly why
  it read as "objects" going missing specifically, not paths or
  boxes.

  Fixed with the same shape as the earlier `_refresh_world_view` fix:
  split into a thin reentrancy-guarded wrapper (`_apply_ipl_
  visibility_filter`, checks/sets `self._apply_ipl_visibility_filter_
  in_progress`) and `_apply_ipl_visibility_filter_impl` (the original
  body, unchanged). A skipped nested call is safe/lossless -
  whatever triggered it will follow up again shortly regardless.
  Verified via AST (both methods defined exactly once) and a smoke-
  test directly simulating the described scenario (an outer call mid-
  way through processing when `_hidden_ipls` changes and a reentrant
  call fires) - confirmed the nested call is skipped and the guard
  flag resets cleanly afterward.

  Also moved the IPL Controls cog settings button to the Object
  Browser's Add/Delete/Rename icon row, right after Rename, per
  Keith: "the Cog settings on the 3rd row, move this to the Object
  Browser, far right instead, after rename icon" (matching his own
  TODO note left in the code: "This needs to be moved to the [IMG]
  [DAT] [IDE] [IPL] <other buttons> [*] on the far right of the
  Object Browser"). Construction moved from IPL Controls row 3 (now
  just the four Show checkboxes) into `_create_object_browser_dock`'s
  `action_row` - same docked-only visibility rule, same wiring to
  `_on_ipl_controls_settings_clicked`, `self._ipl_controls_settings_
  btn` attribute unchanged so `_update_dock_button_visibility`'s
  existing runtime dock/undock sync (a plain `hasattr`/`getattr`
  lookup by attribute name) keeps working regardless of where the
  button was originally built. Resized to match its new siblings
  (18x18, same as Add/Del/Rename) rather than its old bespoke 20x18.

  Pulled and merged Keith's own commits first (checkbox labels
  shortened - Paths/Cull/Zon/Occlusion; a partial in-progress Open/
  Open File button merge left as his own WIP, not touched further).
  Investigated his "Cull does not show in the IPL file display" note
  by tracing the full real-data pipeline against his actual cull.ipl
  - everything checks out correctly; likely observed before his local
  copy had the earlier `headers_by_type['cull']` fix - flagged to
  retest rather than guessed at further since it couldn't be
  reproduced.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions; confirmed `_ipl_controls_settings_btn`'s only other
  reference (`_update_dock_button_visibility`'s runtime visibility
  sync) is a plain attribute lookup, unaffected by moving where the
  button gets constructed.

- **Aug 16, 2026 (cont'd)** — Corrected the `_apply_ipl_visibility_
  filter` reentrancy fix from earlier today, per Keith's real
  screenshots confirming after pulling that fix: "loading zons, or
  other ipl files, seems to partly remove other objects" - still
  happening. The first attempt used the wrong strategy for this
  specific method.

  That first fix mirrored `_refresh_world_view`'s own guard shape:
  skip a nested call outright, reasoning "whatever triggered it fires
  again shortly regardless." True for `_refresh_world_view`'s actual
  callers (periodic TOBJ ticks, continuous LOD-Test mouse-move) - but
  FALSE for what Keith's screenshots actually show: clicking an eye
  icon to show a newly-loaded IPL is a ONE-TIME event with nothing to
  naturally retry it. If that click's call happened to arrive while
  an unrelated periodic tick's call was still mid-flight (pumping the
  Qt event queue inside its own `_refresh_world_view`), the old guard
  silently DROPPED the click's call entirely - the tick's stale, pre-
  click snapshot was all that ever rendered, and nothing ever
  followed up, since nothing else was going to call this again on
  the newly-shown IPL's behalf specifically. Confirmed by the
  screenshots themselves: by the fourth one, the IPL Sections list
  showed seven files all marked visible, but the viewport still only
  rendered a small subset - the "many files visible, tiny rendered
  area" mismatch is the direct fingerprint of dropped, not merely
  delayed, updates.

  Replaced skip-and-drop with queue-and-retry: a call arriving while
  another is in progress no longer runs immediately (still avoiding
  the original concurrent-overlap corruption that motivated the
  first fix), but doesn't vanish either - recorded as "one more pass
  needed" (`_apply_ipl_visibility_filter_pending`, keeping only the
  latest requested auto_fit/clear_display_lists, since only the
  final desired state matters). Once the in-progress pass finishes,
  a follow-up pass runs immediately - which, since `_all_instances`/
  `_hidden_ipls` are read fresh at the start of `_impl`, automatically
  reflects everything that changed while busy. Loops (not just one
  follow-up) so multiple calls piling up during a busy stretch all
  still get accounted for by a single final pass, not lost.

  Verified against the exact failure scenario: simulated a routine
  tick's pass already in progress, a one-time click's call arriving
  mid-flight (changing the hidden set), confirmed the click's call
  gets queued rather than dropped, and a follow-up pass automatically
  runs afterward reflecting the post-click state - the bug is gone.
  Separately verified the original corruption-prevention property
  still holds: `_impl` is never entered concurrently even with
  multiple overlapping reentrant attempts stacked up (only the latest
  queued call survives each overlap, correctly coalesced), and a
  normal non-overlapping call behaves identically to before. `ast.
  parse` clean; confirmed via AST exactly one definition of each
  method.

- **Aug 16, 2026 (cont'd)** — Extended "Save IPL Data As..." to cover
  every section, not just inst, per Keith: "we need a way to save the
  .ipl date [data], save back to original file, but this would
  overwrite existing data, so a save as right click option" ->
  clarified: "Need it to cover all sections, not just inst." (The
  right-click-to-a-new-file behaviour itself already existed and
  already never overwrites the source unless the user explicitly
  picks that path in the save dialog - the actual gap was that only
  `inst` data was ever written.)

  New `_save_ipl_data_as_full` replaces the old inst-only method at
  the IPL File Display's general-purpose "Save IPL Data As..." right-
  click action (the binary-IPL-diagnostic "Save Binary IPL as
  Text..." action elsewhere keeps using the original inst-only
  method - binary IPLs only ever have inst data by definition, so
  nothing changes there). For every section type this app parses
  into structured, editable data (inst/cull/zone/path/grge/enex/
  occl), writes from the LIVE in-memory representation - so edits
  made via e.g. the Path Group Editor are correctly reflected in the
  output, not just a re-dump of the original file. For section types
  not yet parsed into structured data (pick/jump/tcyc/auzo/mult),
  copies the original raw text straight through unchanged, so
  nothing real in the source file is silently dropped even though
  editing isn't supported for those yet. Section order in the output
  matches the original file's own order, determined by actually
  scanning it (same section-keyword detection rule the real parser
  itself uses) rather than a fixed/guessed order.

  inst lines use the correct field layout per game, confirmed against
  `IPLParser._parse_inst`'s own three verified formats: SA (no scale,
  has lod_index), VC (has scale+interior, no lod_index), GTA3 (has
  scale, no interior/lod_index) - the original inst-only method
  always wrote SA's format regardless of which game the loaded world
  actually was, silently wrong for VC/GTA3 saves.

  Verified with real round-trip tests, not just written and assumed
  correct: parsed Keith's real cull.ipl, serialized all 631 cull
  entries with the new format, wrote to a temp file, re-parsed that
  file with the actual `IPLParser`, and confirmed every single entry
  matches the original field-for-field. Same full round-trip for
  GTA3-format inst data using the real comSE.ipl (566 instances, all
  fields matching after save+reparse). Section-order detection
  verified against cull.ipl's real structure (correctly detected
  `inst, cull, pick, path` in that exact order). Pass-through logic
  for an unparsed section (`pick`) verified to preserve the section's
  raw lines exactly, including the position it belongs in relative to
  the sections around it. `ast.parse` clean; confirmed via AST no
  duplicate method definitions.

- **Aug 16, 2026 (cont'd)** — Fixed a real crash on Settings > Apply,
  per Keith: "'ModelWorkshop' object has no attribute '_apply_
  button_mode_to_button'" (hit after reducing texture size to 128 and
  enabling the verbose loading debug checkbox, but not actually
  caused by either of those - a pre-existing, unrelated gap that any
  Apply click would eventually reach). `_update_all_buttons` (a DP5
  paint-tool-era method, present since the very first port from
  Model Workshop) calls `self._apply_button_mode_to_button(...)` for
  whichever of `open_btn`/`save_btn`/`save_col_btn` happen to exist
  as attributes - but the method itself was only ever actually
  DEFINED in `Txd_Editor/txd_workshop.py`, never ported alongside the
  call site into `ModelWorkshop` (Map Workshop). Confirmed the exact
  same gap exists in `Model_Editor/model_workshop.py`, `Col_Editor/
  col_workshop.py`, and `gui/gui_layout_custom.py` too - all copy-
  pasted from the same original template, not something introduced
  by this specific port. Fixed for Map Workshop by porting the real,
  working implementation from `txd_workshop.py` directly (unchanged) -
  `button_display_mode` already existed here, read/written by several
  other methods already, so this slots in as a genuine fix rather
  than needing a new dependency. The other three files' identical gap
  noted but not fixed here - out of this session's actual scope,
  flagging in case worth a follow-up.

  On "settings are not being saved": confirmed this crash did NOT
  actually block Keith's specific changes (texture size, debug
  loading) from saving. `apply_settings`'s own existing comment
  already documents exactly this failure class from once before (a
  different broken reference, `self.format_combo`) - the Loading/Map
  Assets tab settings are deliberately applied FIRST in the function,
  specifically so a later crash elsewhere in the same function can't
  block them. Confirmed `texture_downscale_enabled/threshold/target`
  and `show_verbose_loading_dialog` are all registered in `MapSettings.
  DEFAULTS`, so `.set()`'s own debounced auto-save (independent of
  this function's control flow) would have persisted them regardless
  of the later crash. The alarming error message likely read as "the
  whole apply failed" when only the later, less critical button-
  styling step actually did.

  Verified via AST (exactly one definition of `_apply_button_mode_to_
  button`/`_update_all_buttons`); confirmed `QIcon`/`QSize` (used by
  the ported method) are already imported at module level, no new
  import needed. `ast.parse` clean.

- **Aug 16, 2026 (cont'd)** — Added "Unload" and moved "Save IPL Data
  As..." onto the IPL Sections table's own right-click menu, per
  Keith: "also loading.ipl, how about an option to unload.ipl by
  right clicking them, also move the save as function there aswell."

  New `_unload_ipl_section` - genuinely removes an IPL's loaded
  content from memory (distinct from Hide, which only stops it being
  drawn; the data stays loaded either way). Removes this IPL's own
  entries from every loader list that tracks `source_ipl` (instances/
  culls/zones/paths/grges/enexes/occls), discards it from `loader.
  loaded_ipls`/`self._loaded_binary_ipls` so it can genuinely be
  loaded again fresh later (not silently skipped as "already loaded"
  by `_ensure_ipl_loaded`'s own early-return check), rebuilds `self.
  _all_instances`, and hides it (an "unloaded" IPL still showing as
  visible would be a contradiction) before refreshing the viewport
  and table. Both new actions (`Unload`, `Save IPL Data As...` - now
  using the comprehensive `_save_ipl_data_as_full` from earlier this
  session, not the old inst-only version) only enabled when the row
  is actually loaded - unloading or saving something that was never
  loaded doesn't make sense.

  Verified the unload filtering logic against real dataclasses
  (`IPLInstance`/`CullEntry`): confirmed it removes only the target
  IPL's entries across instances/culls/zones/`loaded_ipls`, leaving
  every other loaded IPL's data completely untouched. `ast.parse`
  clean; confirmed via AST no duplicate method definitions.

  Did NOT chase the separate "settings revert on reload" report in
  this pass - no Map-Workshop-specific "reload" action exists
  anywhere in this file, so it most likely refers to IMG Factory's
  own top-toolbar Reload button (which reloads the currently-open
  IMG/DAT, unrelated to Map Workshop's own settings) or closing/
  reopening the Map Workshop tab - genuinely ambiguous which,
  without more detail worth chasing further via guesswork rather
  than tracing a confirmed path, unlike every other fix this session.

- **Aug 16, 2026 (cont'd)** — Reordered the IPL Sections list to nest
  binary streams under their parent text IPL, per Keith: "I'd
  reorder the IPL list to show LAe.ipl > tab 4 spaces, show the
  binary under LAe.ipl... instead of placing the binary on the
  bottom." Exact mockup: `LAe.ipl (Loaded)` followed by its indented
  `    lae_stream0.ipl (Loaded)` etc, then `LAe2.ipl` (not loaded, no
  suffix) with its own indented streams below it once loaded.

  New `_insert_stream_into_display_order` - a newly-loaded stream now
  gets inserted immediately after its parent text IPL (and after any
  of that parent's already-inserted sibling streams, so multiple
  streams for the same parent stay in the order they were loaded,
  not reversed), instead of the previous plain append to the very
  end of the whole list. `_load_binary_ipl_stream` reverse-looks-up
  which parent a given (archive_path, entry_name) belongs to (`self.
  _ipl_names_with_binary_stream` is keyed the other way round -
  parent -> its streams, the direction the "Load Binary Stream"
  submenu needs) and calls the new insert helper; falls back to a
  plain append only if the parent genuinely isn't in the list at all
  (shouldn't normally happen).

  `_rebuild_ipl_sections_rows` now renders an indented, "(Loaded)"-
  suffixed display name for stream rows (a row only exists once
  actually loaded, so the suffix is effectively unconditional for
  those, matching Keith's own mockup) and a plain, conditionally-
  suffixed name for text IPL rows (loaded or not, since a text IPL's
  row exists in the list before it's ever loaded, unlike streams).
  Purely cosmetic - the eye icon's own stored data (used by every
  existing lookup/matching path: `_ipl_display_to_stem`, `_hidden_
  ipls`, `source_ipl` filtering, etc.) still holds the exact real,
  unindented filename with no suffix, untouched.

  `.zon` files (and any other genuinely standalone entries) are
  unaffected by this reordering - they were never part of the parent/
  stream relationship this targets, so they keep accumulating at the
  bottom of the list exactly as before, per Keith's own explicit
  note: "placing all the .zon on the button would make it easy to
  find them" - already true, nothing needed changing there.

  Confirmed the two other things Keith flagged in the same message
  ("clicking on them we need view, and save as for binary aswell")
  already work - both `_refresh_ipl_inst_file_panel` (view) and the
  IPL Sections context menu's Save actions already handle binary/
  stream rows via the existing `stem.startswith('img:')`/`_loaded_
  binary_ipls` checks, which streams satisfy the same way standalone
  binary IPLs do (matching what Keith found himself: "I just noticed
  a copy of the binary on the bottom for binary where I can save").

  Verified with a direct smoke-test matching Keith's own mockup
  scenario exactly: loading all 4 of one parent's streams one at a
  time produces the identical grouped order he described; loading a
  second parent's streams afterward correctly nests under IT without
  disturbing the first parent's group; re-inserting an already-
  present stream is a harmless no-op. Display-text/loaded-suffix
  logic verified separately, producing text identical to his mockup
  character-for-character. `ast.parse` clean; confirmed via AST no
  duplicate method definitions; confirmed the rest of `_rebuild_ipl_
  sections_rows`' own per-row logic (format-cache lookups, etc.)
  still correctly reads the raw, unindented `ipl_name` rather than
  the new cosmetic display text.

- **Aug 16, 2026 (cont'd)** — Found and fixed the actual root cause
  of "loading zons, or other ipl files, seems to partly remove other
  objects," per Keith's own conclusive diagnostic: saving the
  stream's own row (`lae_stream0.ipl`) after loading a different text
  IPL came back with EVERY section completely empty -
  `inst\nend\ncull\nend\nzone\nend...` - zero entries anywhere,
  confirming genuine data loss, specifically for binary stream
  instances (his earlier before/after diff of `LAe.ipl` itself was
  byte-identical, correctly ruling out the text data - the gap was
  never checking the stream's own separately-tracked data until now).

  Real mechanism, fully traced and confirmed: `_load_binary_ipl_
  stream` added a newly-loaded stream's instances directly to
  `self._all_instances` ONLY - never to `loader.instances`, the
  loader's own canonical list. Four separate places in this file
  rebuild `self._all_instances = list(loader.instances)` whenever a
  DIFFERENT IPL loads (`_ensure_ipl_loaded` itself is one of them) -
  every one of those rebuilds was silently discarding any binary-
  stream-sourced instance, since `loader.instances` never actually
  contained them; they only ever lived in a second, parallel,
  easily-desynced copy that nothing else knew to preserve. Simply
  showing a different text IPL (`LAe2.ipl`) after `LAe`'s stream had
  already loaded was enough to trigger a rebuild that wiped it out
  entirely - exactly matching every real screenshot and diagnostic
  file Keith provided across this whole investigation.

  Fixed at the root: `_load_binary_ipl_stream` now extends `loader.
  instances` directly (the one list every rebuild site already
  treats as authoritative) instead of maintaining a separate parallel
  list - `self._all_instances` is then simply rebuilt from it, same
  as everywhere else. Closes the gap for all four rebuild sites at
  once, rather than needing to patch each one individually to also
  remember a second list. As a side benefit, `_unload_ipl_section`
  (added last turn) now correctly and explicitly filters out a
  stream's instances by name too, since they're finally present in
  `loader.instances` for its own filtering logic to find (it
  happened to "work" before only because the stream was never there
  to begin with, via the same bug, not through genuine correctness).

  Verified with a full, real end-to-end simulation using actual
  `IPLInstance` objects, not just reasoning about it: first confirmed
  the ORIGINAL code genuinely reproduces the exact failure (stream
  instance count silently drops from 5 to 0 the moment a second,
  unrelated text IPL loads afterward - proving the test was
  validating the real bug, not an assumption) - then confirmed the
  fixed version keeps all 5 stream instances intact through the exact
  same sequence, while the parent text IPL's own 10 instances and the
  second IPL's 8 instances are both correctly present and unaffected
  throughout. `ast.parse` clean; confirmed via AST exactly one
  definition of the fixed method.

- **Aug 16, 2026 (cont'd)** — Three fixes from Keith's latest report
  ("Bug appears to have been fixed; everything is loading, nothing is
  disappearing" - confirming the binary-stream data-loss fix from
  last turn - plus three new items).

  **GTA3 path rendering mess** (per his real screenshot: "makes a
  nice mess in the viewpoint", hundreds of long, criss-crossing lines
  spanning the whole loaded city). Real cause: `_refresh_path_
  visualization`'s GTA III world-space resolution matched a path
  group's `model_id` against every currently-visible instance CITY-
  WIDE, with no notion of which district a group or an instance
  actually belongs to - harmless for a single small IDE file in
  isolation (confirmed against the one real sample available -
  `comse.ide`/`comSE.ipl` - every `model_id` there has exactly one
  placement, so a global lookup happens to be correct by coincidence
  for that file alone), but a whole-city load pulls in many
  districts' own IDE files together, and different districts'
  "null node" path-anchor markers (Keith's own real comse.ide has
  per-district names like `comsenullnodea02` through
  `comsenullnodea11`) plausibly reuse the same small numeric
  `model_id` ranges across different, unrelated districts, the same
  way ID ranges get reused across many other GTA III/VC IDE files
  generally. A global lookup would then match a path group meant for
  one district's own specific junction against a completely unrelated
  instance elsewhere in the city sharing the same numeric `model_id`
  - producing a technically well-formed, locally-correct-shaped path
  attached to a wildly wrong, distant world position, exactly what
  "long, criss-crossing lines spanning the whole map" looks like.

  Fixed by scoping each group's instance lookup to its own area -
  matching the group's own `source_ide` filename stem against each
  candidate instance's `source_ipl` stem (case-insensitive), the same
  area-name-matching convention already established for associating a
  binary IPL stream with its parent text IPL, not a new invention.
  Falls back to the unscoped candidate list only if `source_ide` is
  somehow empty or scoping leaves nothing, rather than silently
  dropping a real group entirely. Verified the real `comse.ide`/
  `comSE.ipl` stems actually match (`comse` <-> `comse`) and re-ran
  the full pipeline against real data - the known-working single-
  district case still produces the same 80 segments as before,
  confirming the fix doesn't break what already worked; longest
  segment distance stayed a plausible ~691 units, not spanning the
  map. Being upfront about confidence here: this is a well-reasoned
  fix based on sound logic and confirmed not to break the one real
  case available, but the actual multi-district collision scenario
  couldn't be directly reproduced without Keith's real, larger city-
  wide dataset - worth confirming this actually clears up the mess
  on his end.

  **`.zon` files always at the bottom**, per Keith: "Next are the zon
  files, always at the bottom of the ipl list" - a stronger, always-
  enforced guarantee than just "happens to end up there from
  insertion order", which the stream-nesting reorder from a few turns
  ago could otherwise disturb over time as more streams get loaded
  after a `.zon` file was already opened. Enforced in `_rebuild_ipl_
  sections_rows` itself (the one place every row actually gets
  rendered from) via a stable partition - everything else in its
  existing relative order, then every `.zon` in its existing relative
  order - rather than a full re-sort, so this never reorders anything
  else. Updates `self._ipl_display_order` in place to match, keeping
  Move Up/Down, the saved `ipl_sections_order` setting, and everything
  else that reads this list consistent with what's on screen.
  Verified: an interleaved order gets correctly partitioned with both
  groups' relative order preserved; no-`.zon`-present and already-
  sorted cases are both correctly no-ops (idempotent, no spurious
  reordering).

  **Settings saved but not loaded on the next session**, per Keith:
  "the settings are being saved for map workshop, there just not
  being loaded." Real mechanism found: `MapSettings.set()` debounces
  its own auto-save by 800ms - closing the Map Workshop tab (or
  quitting the app) shortly after changing a setting could leave that
  change still pending, unwritten, at the exact moment the widget
  actually closes. Confirmed `closeEvent` never flushed this - fixed
  by calling `self.map_settings.save()` (an immediate, non-debounced
  write) right at the start of `closeEvent`, guaranteed to run
  regardless of whether anything was actually still pending (cheap,
  safe no-op otherwise).

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions introduced anywhere (including confirming the file's
  two separate, legitimately-different `closeEvent` methods - one on
  `ModelWorkshop`, fixed here; the other on an unrelated dialog class
  further down dealing with document-modification state, correctly
  untouched).

- **Aug 16, 2026 (cont'd)** — Fixed "occlusion data not showing in
  ipl display", per Keith's real screenshot. Real cause: `"occl"` had
  full parsing (`_parse_occl`) and real viewport rendering (Show
  Occlusion, ghosted boxes) since a few turns ago, but was never
  actually added to `tab_specs` - the IPL Controls tab bar simply had
  no "OCCL" button at all, so there was no way to select it and view
  its raw data in IPL File Display, unlike every other parsed section
  type. Added it right after ENEX, enabled like the other real
  section types (not a disabled stub like PICK/JUMP/TCYC/AUZO/MULT).
  Nothing else needed changing - `headers_by_type['occl']`, the raw-
  text extraction, and the auto-tab-switch logic are all already
  generic and correctly pick up any key present in `self._ipl_tab_
  keys`, which is itself just derived from `tab_specs`.

  Added colour pickers for cull/zone/occlusion box colours to
  Settings > Render, per Keith: "Zon settings for box colour, same
  with occlusion." All three (`cull_box_color`/`zone_box_color`/
  `occl_box_color`) have had real viewport rendering and their own
  `MapSettings` entries for several turns, but this dialog never
  actually exposed a way to change any of them - the same gap path
  line/node colour had before it got a picker earlier this session.
  New "Cull / Zone / Occlusion Boxes" group, one colour picker each,
  same pattern as the existing Path Lines group (a coloured preview
  button, `QColorDialog` on click, saved + pushed to the live
  viewport on Apply). Confirmed no additional "apply on startup/
  refresh" wiring was needed - `_refresh_cull_box_visualization`/
  `_refresh_zone_box_visualization`/`_refresh_occl_box_visualization`
  already re-apply their saved colour on every call (built in
  alongside the original rendering work), so a colour picked here
  takes effect immediately through the existing pipeline once saved.
  Zone's own separate Ghosted/Wireframe/Translucent render-style
  dropdown (in the Render menu, not this dialog) is unaffected by
  this addition - purely about colour.

  Path line/node colour and thickness/size themselves were already
  built (confirmed present, not re-added) - Keith's "Settings objects
  for nodes and lines (paths) line/node colour and thinkness" request
  matches what already exists exactly; flagging in case it wasn't
  visible/found on his end rather than assuming it needs rebuilding.

  Verified: `DEFAULTS` registration/get/set round-trip for all three
  new colour keys; confirmed `occl` is correctly present in `tab_
  specs`, positioned right after `enex`. `ast.parse` clean; confirmed
  via AST exactly one definition of the settings-dialog method.

- **Aug 16, 2026 (cont'd)** — Fixed a real discoverability problem,
  per Keith's screenshot of the actual Settings dialog: "there is no
  render in map workshop settings?" Every earlier reference this
  session to "Settings > Render" was genuinely misleading - that
  content lived in a completely separate dialog
  (`_open_render_settings_dialog`, opened only via the "Render
  Settings" ribbon button under Object Browser), not as a tab in the
  actual "Map Workshop Settings" dialog Keith was looking at (Fonts/
  Display/Performance/Preview/Loading/Map Assets/Navigation/
  Keybindings, per his own screenshot) at all.

  Migrated the entire old dialog's content - Object Rendering style,
  Background colour, Path Lines (colour/thickness/node colour/size),
  and Cull/Zone/Occlusion Box colours - verbatim into a new "Render"
  tab in `_build_workshop_settings_tabs`, positioned right after
  Display. The old separate `_open_render_settings_dialog` method is
  removed entirely (216 lines) rather than left as a confusing second
  place to find the same settings - confirmed via `grep` zero
  remaining references anywhere in the live file. Apply logic merged
  into the shared `apply_settings()` function, in the same "safe
  zone" as Loading/Keybindings (applied and saved before the later
  pre-existing code with known broken references further down that
  same function, so a crash there still can't block these from
  taking effect).

  The "Render Settings" ribbon button now opens the main Settings
  dialog instead of a separate one - `_show_workshop_settings` gained
  an optional `initial_tab` parameter (matched by exact tab label,
  not a hardcoded index, so this can't silently break if tabs get
  reordered later) so the button still jumps straight to Render
  rather than always landing on the first tab. Caught and fixed a
  real, if currently-harmless, fragility while wiring this: the
  titlebar's own [Settings] button connected `clicked` directly to
  `self._show_workshop_settings` with no lambda - Qt's `clicked`
  signal passes a `checked: bool` argument, which would land in the
  new `initial_tab` parameter's position. It happened to work by
  coincidence (`False` is falsy, so the tab-selection code's own `if
  initial_tab:` guard skipped correctly regardless), but wasn't
  correct by design - fixed to go through an explicit `lambda
  checked=False: self._show_workshop_settings()` instead, matching
  the same safe pattern already used for the Render button itself.

  Confirmed the other, similarly-named methods found via a project-
  wide search (`Model_Editor/model_workshop.py`, `Col_Editor/
  col_workshop.py`) are sibling tools' own separate, unrelated
  implementations - correctly untouched, out of this fix's scope.
  `ast.parse` clean; confirmed via AST no duplicate method
  definitions and confirmed `_open_render_settings_dialog` no longer
  exists anywhere in this file at all.

- **Aug 16, 2026 (cont'd)** — Reverted the previous GTA III path
  area-scoping fix, per Keith's real, complete `gta3.IDE` upload
  ("gta3.ide appears to have all the ide files, in one file" - every
  path section for the whole game, combined). That fix was based on
  a theory that turned out wrong: parsed all 870 real path groups
  across 553 unique `model_id`s in this real data and confirmed zero
  cases of two different objects sharing one `model_id` - GTA III's
  object IDs are genuinely unique game-wide, not just per-district,
  matching the well-documented standard convention (confirmed via
  web research on GTA3/VC/SA's shared, ~23,000-ID-total scheme).

  What actually happens, and is completely legitimate: a single
  object can have 2-3 separate path groups attached to it (309 of the
  553 `model_id`s have 2 groups, 4 have 3) - real examples include
  named road-piece objects like `rd_Corner1`/`rd_Road1A5`, which
  plausibly need multiple groups for different lanes/directions
  through the same piece. The earlier area-scoping fix (matching a
  group's `source_ide` stem against candidate instances' `source_ipl`
  stems) was solving a district-collision problem that this real data
  proves doesn't exist - reverted back to a clean global `model_id`
  lookup, removing the now-inaccurate reasoning in the code's own
  comments along with it.

  Also researched PATH's actual documented relationship to instances
  before touching anything further: multiple independent sources
  confirm GTA III's PATH section is "attached to existing objects in
  a similar manner to 2DFX" - the same simple model_id-based
  attachment this app's own 2DFX rendering already uses successfully,
  with no area/district scoping of its own either, reinforcing that
  the global lookup is the right model, not an area-scoped one.

  The real cause of Keith's "long criss-crossing lines" mess is still
  open - this verification confirms the IDE side (parsing, model_id
  uniqueness, multi-group-per-object handling) is solid, so the bug
  must be in the instance-matching or position/rotation transform for
  whichever instances actually place these objects - not visible from
  IDE data alone, needs the corresponding IPL/instance data to
  continue investigating with real data rather than guessing further.

  Verified via AST no duplicate method definitions; `ast.parse` clean.

- **Aug 16, 2026 (cont'd)** — Found and fixed the REAL root cause of
  both "makes a nice mess in the viewpoint" and "the paths dont line
  up at all with the roads", per Keith's own decisive diagnostic: he
  uploaded `pathslc.ipl` - a real, community-converted (by "sorup")
  port of these exact same Liberty City paths into Vice City's own
  coordinate space and file format ("here are the paths that was
  converted to work on Vice City, you can compare these with those
  in the gta3.ide"). This gave a genuine, known-correct reference to
  validate against, rather than reasoning about the transform in
  isolation.

  Resolved the same real path groups against Keith's real, complete
  `gta3.dat` world (from the previous upload) and compared shape
  statistics directly - specifically each group's own max distance
  between any two of its real nodes, a metric that's independent of
  translation (so comparable even though the whole map was moved
  between the GTA III original and its VC port). Without any
  additional scaling, my resolved groups came out roughly 14-15x too
  large (median 560 units vs the reference's 39; max 5722 vs 370) -
  and 16 is exactly the scale factor VC's own path node coordinates
  are already known to use in their raw text form (`IPLParser._
  parse_path` already divides those by 16). Testing with the same
  `/16` division applied to GTA III's IDE-embedded relative offsets
  matched the reference almost exactly: median 35.0 vs 39.2, min
  identical at 3.6, max 357.7 vs 369.6 - all within ~10%, the
  remaining gap fully explainable by comparing a slightly different
  group set (2163 resolved vs 2025 in the reference) rather than any
  remaining error.

  Fixed by dividing `node.x_rel`/`y_rel`/`z_rel` by 16.0 before
  rotating and adding to the instance's position - one line, in the
  same spot area-scoping was reverted from a few turns ago. This
  explains both symptoms from the same root cause: every node offset
  was 16x too large, which simultaneously spread each path's own
  shape out 16x wider than real ("the mess") and placed every node
  16x further from the actual object it's attached to than it should
  be ("the paths dont line up at all with the roads") - two
  descriptions of the exact same underlying error.

  This is the first fix in this whole investigation actually verified
  against a real, independently-known-correct reference rather than
  just internal consistency checks - the earlier reentrancy and area-
  scoping investigations were real, necessary work, but this is what
  actually explains the visual symptoms Keith was seeing throughout.
  `ast.parse` clean.

- **Aug 16, 2026 (cont'd)** — Added a "Shift Coordinates..." action
  to the IPL Sections right-click menu, per Keith: "we are planning
  to add coords shifting abilities, this should work accoss all
  loading ipls, zon or path, this will allow me to drag those vc
  convert paths to where LC really is" - his real pathslc.ipl (a
  third-party conversion of these exact Liberty City paths into VC's
  own coordinate space) needs moving as a whole to wherever SOL
  actually placed Liberty City within VC's world, rather than hand-
  editing every coordinate in the file.

  New `_shift_ipl_coordinates(ipl_name, dx, dy, dz)` applies a fixed
  offset to every real WORLD POSITION an IPL's loaded data holds -
  inst, cull, zone, the VC/GTA3 IPL "path" text section (what
  `pathslc.ipl` itself actually contains), grge, enex, and occl -
  covering every section type an IPL can hold, per Keith's own
  "across all loading ipls, zon or path" framing, not just instances.
  Deliberately only ever shifts positions, never dimensions, angles,
  or flags - a cull/occlusion box's width/height, a path node's
  median (a lane-width value, not a position), an occlusion zone's
  rotation, and similar fields are left untouched, so this is a
  genuine rigid-body move rather than a shape-distorting one. GTA
  III's own IDE-embedded paths aren't included - they have no
  `source_ipl` of their own (attached to instances by `model_id`, not
  to a specific IPL file), so they move automatically along with
  whichever instance places them, the same way that instance's own
  visual model already does when its position changes - nothing
  separate needs shifting for those.

  New `_prompt_shift_ipl_coordinates` - small dialog collecting a
  (dx,dy,dz) offset (wide range, ±100,000, matching the real scale of
  Keith's own pathslc.ipl coordinates), applies it and refreshes the
  viewport immediately. Both new actions only enabled when the row is
  actually loaded, same as Unload/Save IPL Data As... alongside them.
  Live in-memory only, same as every other edit this app makes - Save
  IPL Data As... (already covers every section type) is how a shifted
  result gets written back out to a real file.

  Verified against real `pathslc.ipl` data: applied a shift, confirmed
  every node moved by exactly the given delta, confirmed the shift is
  a pure rigid-body translation (relative shape/distances between
  nodes in the same group unaffected, only their absolute position
  changes). `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 16, 2026 (cont'd)** — Added Delete and New Path Group actions
  alongside the existing Edit Path Group, per Keith: "we don't have
  the ability to edit the paths, delete, add, make paths from
  scratch." Both wired into the same PATH-tab-gated right-click menu
  Edit Path Group already lives in.

  New `_delete_path_group_for_row` - resolves the group the same way
  Edit does (`_find_path_group_for_line`), removes it from `loader.
  paths` entirely (a real removal, distinct from clearing a single
  node to Null the way the Path Group Editor's own per-row Delete
  already could), refreshes the viewport.

  New `_new_path_group` - creates a fresh group with all 12 node slots
  starting as Null, positioned at the viewport's current focus point
  (rather than the world origin, which could be far from anything
  visible) so a newly-created group is easy to find. Opens straight
  into the existing Path Group Editor dialog for immediate editing.
  Line number picked well clear of any real data line numbers (max
  existing + 1000) so it can never collide with a genuine row.
  Disclosed a real limitation rather than hiding it: the IPL File
  Display's PATH tab is built from the original file's raw text, not
  live PathGroup objects, so a brand-new group won't show as a row
  there until saved out and the file reloaded - it does show in the
  3D viewport immediately, and is fully editable via the dialog this
  opens right after creating it, same honest-stub pattern already
  established for the Path Group Editor itself (edits the live
  object; Save IPL Data As... is how a result gets written back out).

  Scoped to VC/SA-style `loader.paths` (the "path" IPL text section) -
  GTA III's own IDE-embedded paths aren't covered by this (they
  attach to instances by model_id rather than belonging to a specific
  IPL file at all, a fundamentally different creation/deletion model
  that would need its own, separate design).

  Verified against real dataclasses: a new group constructs with
  exactly 12 Null nodes at the given position; deleting a group
  removes only the target from a list, leaving others untouched; the
  line-number collision-avoidance logic picks a value clear of real
  data. `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 16, 2026 (cont'd)** — Fixed the Render tab being invisible
  despite genuinely existing, per Keith: "he can't find the settings,
  they should be in map-workshop settings, I've looked here, there
  not found?" (his friend, giving fresh, uncoached feedback, couldn't
  find the same Render tab added earlier this session - and Keith
  himself couldn't spot it either on looking).

  Root cause: the "Map Workshop Settings" dialog's minimum width
  (650px) was sized for the original 8 tabs (Fonts/Display/
  Performance/Preview/Loading/Map Assets/Navigation/Keybindings).
  Adding Render as a 9th tab could easily push the tab bar past that
  width - and Qt's default behaviour when a tab bar doesn't fit its
  container is to show small scroll arrows rather than grow the
  dialog to fit. That means Render could have been sitting there the
  entire time, just scrolled out of view with no obvious visual cue
  that scrolling the tab bar itself (not the dialog's own content)
  was needed to reach it - genuinely easy to miss, exactly matching
  "I've looked here, there not found."

  Fixed two ways together: widened the dialog's minimum width from
  650 to 850 (real margin for future tabs, not just enough for today's
  9), and explicitly disabled `usesScrollButtons` on the tab widget so
  this specific failure mode can't recur even if more tabs get added
  later - every tab stays visible, at worst slightly narrower, never
  hidden behind an easy-to-miss arrow. Confirmed `App_name` is
  literally `"Map Workshop"`, so the dialog's own title is exactly
  "Map Workshop Settings" - matching Keith's wording and his earlier
  screenshot precisely, confirming this is the right dialog. Also
  confirmed both the standalone dialog and the docked-mode settings
  contribution share the exact same underlying tab-building method
  (`_build_workshop_settings_tabs`), so this fix covers both entry
  points, not just one of them.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 16, 2026 (cont'd)** — Made path node markers round instead of
  square, per Keith: "we could make the path nodes round circles,
  makes it easy to click on them." Standard OpenGL technique, not a
  custom shape: enabled `GL_POINT_SMOOTH` (anti-aliases each point
  into a circle rather than leaving its square corners visible)
  around the node-drawing pass in `_draw_paths`, `GL_NICEST` hint for
  the best-quality rounding available - worth asking for since node
  size can go up to 30px via Settings > Render, large enough that
  visible squared-off corners would actually be noticeable at that
  size. State cleanly disabled again afterward, matching the
  established enable/disable-around-the-draw-call pattern already
  used for everything else in this method.

  This is the visual half of what Keith described - actual click-to-
  select or drag interaction on a node is separate, unbuilt work (the
  same class of feature as the still-open corner-sphere-dragging
  TODO: real mouse picking and drag math, neither of which exist yet
  anywhere in this viewport) - noted honestly rather than implied as
  covered by this change.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions. PyOpenGL isn't available in this sandbox, so
  `GL_POINT_SMOOTH` itself hasn't rendered for real here - it's a
  long-standing, standard part of the legacy OpenGL 1.x API though,
  the same confidence level already applied to `gluSphere` earlier
  this session.

- **Aug 17, 2026** — Built real interactive path node editing (click
  to select, drag to move, release to commit), per Keith: "lets
  address the unbuilt work, editing paths first" - the first of three
  pieces from his own priority order (path editing, then whole-IPL-
  section dragging, then rotating map sections - the latter two not
  started yet).

  Built on the app's existing, already-proven vertex-picking system
  rather than inventing new picking math: new `_pick_path_node`
  reuses the exact same `_pick_ray`/`_closest_point_on_ray`
  infrastructure `_pick_vertex`/`_pick_world_instance` already use.
  Dragging is constrained to the ground plane at the node's own
  starting height (not free in 3D - a 2D mouse drag can't
  unambiguously set 3 coordinates, and path nodes are ground-level by
  nature) via `_screen_to_ground_position`, the same ray-plane math
  already proven for LOD Test mode - no new geometry code needed for
  either the picking or the dragging.

  New viewport state: `set_path_node_owners` (position -> (PathGroup,
  node_index) mapping, built alongside the segments list every
  refresh from the exact same coordinates, so a lookup here always
  matches something actually drawn), `set_path_edit_mode` (the on/off
  toggle), `set_path_node_drag_callback` (mirrors the existing
  `set_lod_test_callback` pattern - widget owns the interaction,
  caller owns the data). During a drag, only the viewport's own
  display list gets updated per mouse-move (cheap, immediate visual
  feedback); the real commit - mutating the actual live `PathNode`
  object and doing a full refresh - happens exactly once, on release.

  New "Edit Paths" checkbox in IPL Controls row 3, next to Show
  Paths - turning it on also turns Show Paths on (editing invisible
  nodes makes no sense) but doesn't turn Show Paths back off when Edit
  Paths is unchecked. Scoped to VC/SA-style `loader.paths` only, same
  scope New/Delete Path Group already settled on - GTA III's own IDE-
  embedded paths attach to instances by model ID rather than holding
  a position of their own, a fundamentally different edit model not
  covered here, disclosed in the checkbox's own tooltip rather than
  silently unsupported.

  Verified against real `pathslc.ipl` data before trusting it: built
  the owner map and segments exactly as `_refresh_path_visualization`
  does, confirmed every segment endpoint resolves back through the
  owner map with zero misses, then confirmed mutating through a
  resolved `(group, index)` pair actually changes the same live
  `PathNode` object the segments were built from - not a copy. `ast.
  parse` clean on both files; confirmed via AST no duplicate method
  definitions anywhere in the new code.

- **Aug 17, 2026 (cont'd)** — Added train track support (data/paths/
  tracks.dat, tracks2.dat), per Keith: "then the other path .dat
  files you pointed out earlier" - the second item from his own
  priority list, right after path node editing.

  New `TrackWaypoint` dataclass and `GTAWorldLoader.load_tracks_dat`/
  `_parse_tracks_file` - a real, verified parser, not guessed at:
  inspected Keith's own real tracks.dat/tracks2.dat directly first
  (plain text, a waypoint count on line 1, then exactly that many
  "X Y Z" lines - genuinely the simplest path-adjacent format in this
  app, no section keywords, no node graph, just an ordered point
  list). Confirmed these files are never referenced anywhere in a
  real, complete gta.dat/gta3.dat's own directive list - the game
  loads them from a fixed, well-known relative path instead
  (data/paths/) - so `load_tracks_dat` is called unconditionally at
  the end of `load_from_dat`, not gated by any directive, with case-
  insensitive lookup for both the "paths" subdirectory and the
  filenames themselves (Linux is case-sensitive; real game data
  packaging isn't always consistent about casing).

  Viewport rendering (`_draw_tracks`) is simpler than every other
  path overlay in this app - one continuous line strip per track, no
  node markers at all, since a train track has no meaningful "node"
  concept the way a vehicle/ped path does; the polyline itself is the
  whole picture. New "Tracks" checkbox in IPL Controls row 3, colour
  configurable via a new `track_color` `MapSettings` entry (silver/
  grey default, loosely evoking real rail). Unlike every other
  overlay refresh in this file, `_refresh_track_visualization`
  doesn't filter by `self._hidden_ipls` at all - these files aren't
  tied to any IPL or area, so there's no per-IPL visibility concept
  to apply; loaded once at world-load time, shown or not as a whole.

  Verified against Keith's real files before trusting any of it: 168
  and 557 waypoints parsed (matching each file's own stated count
  exactly), first waypoint's coordinates matched byte-for-byte, and
  the full `load_tracks_dat` → polyline-conversion pipeline run
  end-to-end against the real directory structure. Noted in passing,
  not required for correctness but a nice organic confirmation the
  data makes sense: tracks2.dat's polyline starts and ends at nearly
  the same point (-742.018,-834.251 both ends, Z differing only in
  the last few digits) - consistent with a real, closed subway loop.

  Also inspected `train.dat` while scoping this (expected to be
  closely related to tracks.dat) and found a substantially different,
  more complex format - comma-separated station data with a
  `999,999,999` sentinel pattern and what look like linked-track
  position references. Real format, not yet understood well enough to
  implement without guessing - left for a proper follow-up rather
  than rushed into this pass; noted in TODO.md with what's actually
  known about it so far.

  `ast.parse` clean on all three touched files; confirmed via AST no
  duplicate method definitions anywhere in the new code.

- **Aug 18, 2026** — First response to Keith's real testing feedback
  (6 items from actually using path editing/tracks in practice).
  Addressed the two most concrete, highest-confidence items now;
  the rest need more investigation or are substantial UI redesigns
  worth scoping properly rather than rushing.

  **(4) "Clicking nodes brings up nothing?"** - real UX gap found: a
  successful node pick never triggered a repaint, and nothing was
  ever drawn differently for the currently-held node either - so a
  click that didn't also happen to move the mouse enough to shift the
  node's ground position produced literally zero visible change,
  even though the pick itself had actually worked under the hood.
  Fixed two ways: `mousePressEvent` now calls `self.update()`
  immediately on a successful pick, and `_draw_paths` gained a
  dedicated highlight pass - the currently-held node draws as its own
  larger (1.8x), white, semi-transparent point on top of everything
  else, visible the instant a node is picked up, before any drag
  movement happens. `mouseReleaseEvent` also gained its own explicit
  `self.update()` so the highlight reliably clears even if the
  downstream commit callback happens to be unavailable for any
  reason, not solely relying on that callback's own refresh.

  **(1) "Any settings added do not save or are not loaded in the next
  session"** - added a second, independent safety net for the
  debounced auto-save: `ModelWorkshop.__init__` now also hooks
  `QApplication.aboutToQuit` to force an immediate `map_settings.
  save()`, alongside the existing `closeEvent` flush from a few turns
  ago. Real gap this closes: the `closeEvent` flush only fires if
  Map Workshop's own `closeEvent` actually runs, which depends on the
  whole app quitting through a clean per-widget close sequence - if
  it exits some other way instead, that flush would never run and the
  debounced save's 800ms window could still be open. Confirmed the
  affected settings (`path_line_thickness`/`path_node_size`/`cull_
  box_color`/`zone_box_color`/`occl_box_color`/`track_color`) are all
  genuinely registered in `MapSettings.DEFAULTS` - ruling out the
  "unregistered key silently no-op'd" trap this exact class of bug
  has hit before, and confirming only one `MapSettings()` construction
  site exists (ruling out a second, desynced instance too). Being
  upfront: this is an additive safety net addressing the most
  plausible gap found, not a confirmed root-cause fix the way the
  binary-stream/`/16`-scale bugs earlier this session were - worth
  Keith retesting specifically to confirm this actually resolves it.

  **(2) Middle-mouse drag stopping near models** - investigated but
  not yet resolved. Re-read the current middle-button pan handling in
  `mouseMoveEvent` carefully (unchanged by any of today's or recent
  work, own `elif` branch, no interaction with the new path-node-drag
  code) and found nothing obviously wrong there. Genuinely unclear
  what's causing this without more specific reproduction detail -
  logged rather than guessed at with an unverified fix.

  **(3) Render Settings should show Tracks/other path types as their
  own columns**, **(5)/(6) convert the IPL Controls checkboxes into
  clickable buttons with click=show/hide, right-click=edit mode, and
  a marching-ants-style border for the edit-mode state** - both real,
  reasonable requests, but substantial UI work in their own right
  (the button redesign specifically touches every one of the row 3
  toggles, not just one) - not started this pass, flagged honestly
  rather than attempted half-scoped.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 18, 2026 (cont'd)** — Two real pieces from Keith's follow-up
  message.

  **GTA III paths now follow their attached object when it moves**,
  per Keith: "when I move models in GTA3, shouldn't the paths
  attached to them move as well." Real, confirmed bug: `_on_instance_
  edited`'s own fast path (built specifically for performance, per
  Keith's earlier "takes so long for anything to change" complaint)
  returns immediately after updating the viewport's cached transform,
  never calling `_refresh_path_visualization`. Since GTA III's own
  IDE-embedded paths are resolved fresh from the placing instance's
  CURRENT position every single time that runs, moving an instance
  never re-resolved whatever path was attached to it - the path
  stayed visually stuck at the object's old position. Fixed by
  checking whether the moved instance's `model_id` actually has a
  path group attached to it (a cheap check against `loader.ide_
  paths`) and only paying the extra refresh cost in that case - the
  common case of moving an ordinary building/prop with no path of its
  own stays exactly as fast as before, preserving the original reason
  the fast path exists. Verified against the real complete `gta3.dat`
  world: 553 of 8689 real instances have a path attached, correctly
  distinguished from the rest.

  **A real, working undo/redo system**, per Keith: "we need to get
  the undo function working" - was previously an honest stub
  (`_on_undo_clicked`: "STUB - undo/redo for mapping changes... isn't
  implemented yet"). New `_push_map_undo`/`_map_undo`/`_map_redo` on
  `ModelWorkshop` itself (not on `_InstanceEditPanel`, which gets
  recreated every time a different instance is selected and would
  lose all history on every selection change if the stack lived
  there) - genuinely separate from the existing `self.undo_stack`,
  which turned out to be scoped entirely to COL model editing
  (inherited from this tool's Model Workshop base, hardcoded to
  `current_col_file.models[model_index]`), not reusable for map edits
  at all. Command-pattern design: each entry is a pair of no-arg
  callables that know how to reverse/reapply one specific change,
  not a generic snapshot - lets different edit types share the same
  mechanism without a different implementation per type. A fresh
  edit clears the redo stack (standard convention).

  Wired into all four instance-editing actions in `_InstanceEditPanel`
  (position/rotation/scale nudges, set-scale-to-zero) - each captures
  its own "before" value right before mutating (the only point it's
  still available), and the undo/redo closures also refresh that
  panel's own spin boxes if it's still showing the same instance.
  Rotation undo stores the raw quaternion directly rather than the
  euler angles shown in the UI, avoiding any rounding drift from
  repeatedly converting through euler and back. The existing Undo
  button (`_on_undo_clicked`) now does real work: plain click undoes,
  Shift+click redoes.

  Verified the core undo/redo mechanics directly against a real
  `IPLInstance`: single undo/redo round-trip, multiple sequential
  undos unwinding in reverse order and redo reapplying forward, a
  fresh edit after undo correctly clearing the redo stack, and empty-
  stack undo/redo handled gracefully with a status message rather
  than crashing.

  `ast.parse` clean; confirmed via AST exactly one definition of
  every touched/new method.

- **Aug 18, 2026 (cont'd)** — Two more pieces from Keith's ongoing
  feedback.

  **Zoom to mouse cursor**, per Keith: "when I zoom in, or zoom out,
  have a settings option to zoom in to the mouse pointer. so if i
  move the point to the top, and zoom in, it zooms in that area."
  New `DFFViewport.wheelEvent` behaviour (off by default, a new
  `zoom_to_cursor` `MapSettings` entry + checkbox in Settings >
  Navigation turns it on): finds the world-space ground point under
  the cursor before changing `_dist`, applies the zoom, finds where
  that same screen pixel now points to afterward, then shifts `_pan_
  x`/`_pan_y` by the difference - reuses the already-proven `_screen_
  to_ground_position` (same ray-cast machinery already used for LOD
  Test mode and path node dragging) rather than deriving new
  trigonometry. Verified the exact sign of the pan compensation with
  a standalone simplified camera-transform model before trusting it
  in the real code (an easy thing to get backwards) - stress-tested
  across 4 varied scenarios (zoom in/out, different angles, different
  starting pan/screen positions), all confirming `self._pan_x +=
  (after_pos - before_pos)` is the correct direction.

  **IPL Controls row 3 redesigned from 6 checkboxes into 5 buttons**,
  per Keith: "looking at the bottom-right object control panel, we
  can make clickable buttons instead; one click shows the paths,
  right-click paths allows edit mode, and the other buttons with tick
  marks can work the same way, saving space... When pressing the
  button once, the background slightly changes to show it's selected;
  a margin ants pattern around the button shows edit mode; right-
  click on, right-click off. left click show, left click hde." New
  reusable `_MapOverlayToggleButton(QToolButton)` - left-click toggles
  shown/hidden (a subtle background tint marks the shown state);
  right-click toggles edit mode for overlay types that actually have
  one (a dashed border marks it - a static border, not a literal
  animated "marching ants" effect, which would need a QTimer-driven
  paintEvent redraw; noted honestly as a possible follow-up polish
  rather than silently simplified without saying so). Right-clicking
  a button with no edit mode (Cull/Zone/Occlusion/Tracks - none of
  them have any editing feature built yet) shows a plain status
  message instead of doing nothing silently.

  The previous separate Paths/Edit Paths checkbox pair merged into
  one Paths button (left-click show/hide, right-click toggles the
  existing path-node-drag editing) - the only overlay type that
  actually has a real edit mode today. `show_toggled(bool)`/`edit_
  toggled(bool)` signals fire exactly like a `QCheckBox`'s own
  `toggled` signal, so every existing handler (`_on_show_paths_
  toggled` etc) needed zero changes; a compatibility `isChecked()`
  method means every existing caller reading a checkbox's checked
  state also kept working unmodified. `QToolButton` added to this
  file's own module-level import (previously only imported locally in
  a couple of other functions) - the new class is defined at module
  level and would otherwise fail with a `NameError` the first time
  Map Workshop's own module actually loaded, a real mistake caught
  and fixed before it could ship.

  Verified the button's own state-machine logic (left/right-click
  cycling, `supports_edit=False` correctly routing to a status
  message instead of a state change, `set_shown`'s emit behaviour,
  `isChecked()` compatibility with existing callers) via a standalone
  simulation, since PyQt6 isn't available in this sandbox to
  instantiate the real `QToolButton` subclass directly. `ast.parse`
  clean on both touched files; confirmed via AST no duplicate method/
  class definitions, and confirmed (via direct search) no remaining
  references anywhere in the file to the removed `_edit_paths_chk`
  checkbox or to `.setChecked()`/`.toggled` on any of the five
  buttons - only the compatible `.isChecked()`/`show_toggled`/`edit_
  toggled` API the new widget actually provides.

- **Aug 18, 2026 (cont'd)** — Built whole-IPL-section dragging in the
  3D viewport, the next item in Keith's own priority order for the
  interactive editing layer ("editing paths first" [done], then
  "Moving IPL file whole entires to anywhere on the map").

  New "Drag IPL" checkbox in IPL Controls row 3. While active,
  clicking any instance and dragging moves that instance's ENTIRE
  IPL - every instance, path node, cull/zone/occlusion box, garage,
  entrance/exit belonging to that same file - as one rigid body,
  constrained to the ground plane at the clicked instance's own
  starting height (same reasoning as path node dragging: a 2D mouse
  drag can't unambiguously set 3 coordinates, and this data is
  ground-level by nature).

  Built almost entirely on existing, already-proven infrastructure
  rather than new mechanisms: `_pick_world_instance` (already used
  for double-click-to-edit) identifies which instance and thus which
  IPL was clicked; `update_instance_transform` (built earlier for the
  Item Editor Dialog's own fast-path nudges) gives cheap, real-time
  visual feedback on every instance belonging to the dragged IPL
  during the drag itself, without touching any real `IPLInstance`
  data until release; and the commit on release calls the already-
  existing, already-verified `_shift_ipl_coordinates` - the exact
  same method the dialog-based Shift Coordinates tool already uses -
  so paths/cull/zone/occlusion/garages all move correctly too, not
  just the instances that got live preview. A release with zero
  actual movement (a plain click, never dragged) is skipped entirely
  rather than committing a no-op move.

  Honest scope limit: only instances get live visual feedback mid-
  drag - cull/zone/occlusion boxes and paths have no equivalent
  identity-based "just update this one cached entry" mechanism the
  way instances do, so they snap to their correct new position on
  release rather than animating smoothly throughout the drag.

  **Caught and fixed a real, would-have-crashed-on-first-use bug
  before it shipped**: the original design tracked each dragged
  instance's starting state in a dict keyed by the `IPLInstance`
  object itself. `IPLInstance` is a plain `@dataclass` (default
  `eq=True`), which Python makes unhashable automatically unless
  `frozen=True` is set - using it as a dict key would have raised
  `TypeError: unhashable type: 'IPLInstance'` the very first time
  anyone actually tried to drag an IPL. Found by directly verifying
  the logic against real `IPLInstance` objects rather than only
  reasoning about the design - confirmed the crash would occur, then
  fixed all five affected sites by switching to a plain list of
  `(inst, pos, rot, scale)` tuples instead (iterated, not looked up
  by key, so hashability was never actually needed).

  Verified the complete corrected press → move → release flow end-to
  -end against real `IPLInstance` objects: press correctly captures
  only the dragged IPL's own instances; move correctly offsets only
  those instances' cached display positions while leaving a different
  IPL's instances (and the real underlying data) completely
  untouched; release correctly commits through the real shift logic,
  producing final positions that exactly match what was already shown
  during the live preview. `ast.parse` clean on both touched files.

- **Aug 18, 2026 (cont'd)** — A large multi-part feature/feedback
  message from Keith arrived covering the drag-IPL mode cycle, axis-
  lock right-click options, snap-to-edge/centre reuse, numeric Move/
  Rotate panels with per-section-type tick-boxes, water/radar
  recalculation, radar/minimap generation, path traffic-flow
  reversal, auto-highlight-on-hover, and axis-coloured box faces with
  a no-clip editing option - genuinely too much to build responsibly
  in one pass without rushing several of them. Picked off the pieces
  that were concrete and self-contained enough to build and verify
  properly this turn; the rest logged accurately for later rather
  than guessed at half-scoped.

  **Axis-coloured box faces**, per Keith: "Cull, Occl, Zon boxes have
  coloured sides: x (green), y (red) and z (blue) faces, which makes
  that easy to see." New optional per-face colour mode for the shared
  `_draw_ghosted_box_from_corners` (used by all three box types) -
  top/bottom caps blue, and of the 4 side faces, the pair connecting
  corners 0-1/2-3 red, the pair connecting corners 1-2/3-0 green.
  Verified this index-based mapping holds correctly not just for the
  axis-aligned case (cull/zone) but for rotated boxes too (occlusion)
  before trusting it - corners_xy is always built by rotating the same
  four local corner offsets in the same order, so which index pair is
  the "local X face" vs "local Y face" is fixed by construction,
  independent of the box's current rotation in world space. New
  `box_axis_colors` `MapSettings` entry + checkbox in Settings >
  Render's "Cull / Zone / Occlusion Boxes" group - off by default,
  overrides each box type's own configured colour when on (a global
  setting covering all three at once, not per-type, since the whole
  point is a consistent way to read orientation regardless of which
  box is being looked at). Applied from all three box refresh methods
  (not just one) so it takes effect correctly regardless of which
  specific box type happens to be visible at the time.

  **Removed the Extrude Faces ribbon icon**, per Keith: "we don't
  need extrude or emboss ribbon icons either." Couldn't find an
  "Emboss" button anywhere in this file to remove alongside it -
  confirmed via direct search, flagging rather than guessing which
  other button he might have meant.

  Verified the axis-face-colour logic directly (uniform colour
  preserved when the setting is off; caps blue, sides 0/2 red, sides
  1/3 green when on, matching the spec exactly) before wiring it into
  the real drawing code. `ast.parse` clean on both touched files;
  confirmed via AST no duplicate method definitions.

  **Logged, not built**, per Keith's own explicit deferral for two of
  them: water/radar recalculation on IPL moves, and a map-to-radar
  (top-down capture) generation feature - both added to TODO.md with
  his own framing preserved.

  **Not yet scoped or started, flagged honestly rather than rushed**:
  cycling Drag/Move/Rotate modes on the same button; right-click axis-
  lock options (lock Z, X/Y-only) for IPL dragging; reusing the
  existing snap-to-edge/snap-to-centre ribbon tools for IPL dragging;
  numeric +/- Move and Rotate panels with per-section-type (paths/
  zones/tracks/cull/occlusion) tick-boxes controlling what actually
  moves; a path right-click "reverse traffic flow" option (Keith's
  own note: "I think we just flip the nodes. needs looking at" -
  a genuine investigation task, not a fully-specified feature yet);
  an auto-highlight-on-hover setting for anything under the cursor;
  a no-clipping option preventing box-to-box overlap while editing
  cull/zone/occlusion sizes.

- **Aug 18, 2026 (cont'd)** — Investigated and built path traffic-
  flow reversal, per Keith's own framing: "Path right-click option,
  reverse traffic flow: I think we just flip the nodes. needs looking
  at" - the investigation he explicitly asked for, done before
  building anything.

  Checked the real structure of path groups against 2032 real ones
  from Keith's own `pathslc.ipl` before assuming a naive "swap next_id
  direction" approach would work: 937 of them (~46%) have at least one
  node with MULTIPLE other nodes pointing to it (up to 4 seen) - a
  junction where several lanes merge into one. Since each node has
  exactly one `next_id` slot, that merge node can't become a split
  point after reversing - the format has no way to represent "one
  node, many next nodes". A naive reversal would have silently
  corrupted close to half of all real groups.

  Implemented accordingly, not silently: new `_is_path_group_simple_
  chain` (a real eligibility check, not a stub) gates a new "Reverse
  Traffic Flow" right-click action, resolved and checked BEFORE the
  context menu is even shown so it can correctly disable itself with
  an explanatory tooltip on an ineligible (branching) group, rather
  than silently failing or corrupting data if clicked anyway. New
  `_reverse_path_group_traffic_flow` - for an eligible simple chain,
  swaps every edge's direction (original A->B becomes B->A), leaving
  `node_type` untouched (Internal vs External is independent of which
  direction traffic flows through a node).

  Extracted `_resolve_path_group_for_row` - the row-to-PathGroup
  resolution logic that was previously duplicated identically inside
  both `_edit_path_group_for_row` and `_delete_path_group_for_row` -
  as a shared helper, used by those two existing methods and the new
  reversal feature alike.

  Verified thoroughly against real data before trusting any of it:
  confirmed the eligible/ineligible split (1095 vs 937) on real data;
  confirmed the reversal is a true involution (reversing twice
  restores the exact original graph) using a clean, non-aliased test
  after an initial verification attempt gave a false failure from a
  Python object-aliasing bug in the TEST script itself, not the
  algorithm (a shallow list copy sharing the same underlying node
  objects, caught and diagnosed rather than assumed to be a real
  bug); then re-ran the exact production code path (not a
  reimplementation) against a real live `PathGroup` object from
  `pathslc.ipl` one more time as a final check. `ast.parse` clean;
  confirmed via AST no duplicate method definitions.

- **Aug 18, 2026 (cont'd)** — Added axis-lock right-click options for
  whole-IPL dragging, per Keith: "[Drag ipl] right-click options,
  like lock z, only move x, y."

  Z is already always effectively locked by the drag's own existing
  ground-plane-constrained design (the projection plane is fixed at
  the clicked instance's own starting height, so the resolved delta's
  Z component is always 0 regardless) - no separate toggle was needed
  for that part of the request. This adds the two remaining practical
  choices: right-click the Drag IPL checkbox for "Free movement (X
  and Y)" (the existing default), "Lock X (only Y moves)", or "Lock Y
  (only X moves)."

  New `DFFViewport.set_ipl_drag_axis_lock`/`self._ipl_drag_axis_lock`
  - applied as a simple post-processing mask on the already-computed
  ground-plane delta in `mouseMoveEvent` (zeroing whichever axis is
  locked before it ever reaches the live preview or gets stored),
  rather than changing the underlying projection math itself. Since
  `mouseReleaseEvent`'s own commit reads that same already-masked
  `_dragging_ipl_delta` value directly, the final commit automatically
  respects the lock too - no separate masking needed there. New
  `_on_drag_ipl_context_menu` in map_workshop.py wires a right-click
  menu to the checkbox via Qt's standard custom-context-menu
  mechanism (a plain `QCheckBox`, not the `_MapOverlayToggleButton`
  used for the show/edit-mode buttons - Drag IPL isn't a show/hide
  toggle tied to one specific overlay type, so it doesn't fit that
  widget's own left/right-click split).

  Verified the masking logic directly: free movement passes both
  axes through unchanged; locking X zeroes X while preserving Y;
  locking Y zeroes Y while preserving X; Z stays 0 under every lock
  mode, confirming it was never actually affected by this feature at
  all (already handled by the pre-existing ground-plane constraint).
  `ast.parse` clean on both touched files; confirmed via AST no
  duplicate method definitions.

- **Aug 19, 2026** — Converted the 2DFX and Tobj checkboxes to the
  same `_MapOverlayToggleButton` widget as the other row 2/3 toggles,
  per Keith: "the 2dfx, tojb buttons need the same adjustments, as
  the other buttons." Both were plain `QCheckBox`, left behind when
  Paths/Tracks/Cull/Zone/Occlusion got converted earlier - now
  consistent styling/behaviour across every overlay toggle in IPL
  Controls. Neither has any edit mode (2DFX is a pure show/hide
  master switch; Tobj controls what appears in the INST table, not a
  3D-view overlay with anything to edit), so both use `supports_
  edit=False`, matching Cull/Zone/Occlusion/Tracks' own treatment -
  right-click shows the same "no edit mode yet" status message.

  Confirmed via direct search that both widgets' only other real
  usages elsewhere in the file (`show_tobj_chk.isChecked()` in the
  TOBJ table filter, `master_chk.isChecked()` in the 2DFX light
  refresh) are already covered by the compatibility `isChecked()`
  method built when this widget class was first added - no other
  code needed changing. `ast.parse` clean; confirmed via search no
  stale `.toggled`/`.setChecked()` references remain anywhere for
  either widget.

- **Aug 19, 2026** — Three items from Keith's real testing feedback,
  after pulling and merging his own ribbon adjustments (removed
  "Create Primitive" alongside the Extrude Faces icon dropped a few
  turns ago - not something Map Workshop needs either).

  **GTA III's IDE-embedded paths now show in the IPL File Display**,
  per Keith: "showing paths in GTA3 viewpoint works, but the path
  data exists in IDE files. We have an empty (No paths) shown in the
  IPL File Display. For GTA3 only, show the paths in the IDE files."
  Real cause: this panel reads a selected IPL's own raw text for
  whichever section is active - correct for every other case, but
  genuinely wrong for GTA III specifically, whose paths live in a
  completely different file (IDE, not IPL) and attach to an object by
  model_id rather than belonging to any IPL file at all. A GTA III
  IPL's raw text never has a "path" section by design, not by bug.
  New `_populate_gta3_ide_paths_table`, triggered only for `data_type
  == 'path' and loader.game == 'gta3'`, bypassing the raw-text
  approach entirely: finds every `loader.ide_paths` group with a real
  instance placement in the currently-selected IPL (same "which IPL
  does this group's real-world placement belong to" logic `_refresh_
  path_visualization` already uses for the 3D view), and renders it
  with GTA III's own real 9-field node layout (NodeType/NextNode/
  IsCrossRoad/XRel/YRel/ZRel/Median/Left/Right) rather than forcing it
  into VC/SA's 13-column layout, which doesn't match this format at
  all. Group-header rows get the same bold+tinted styling the
  existing VC-style path rendering already uses. The Edit/Delete/New/
  Reverse Traffic Flow context-menu actions are hidden entirely for
  this case (added a `loader.game != 'gta3'` guard) rather than shown
  and left to silently fail every time - these rows have no
  corresponding entry in `loader.paths` for those actions to resolve
  against at all.

  Also, per Keith's own suggestion in the same message: the dock
  itself now renames to "Paths Display" while the PATH tab is active,
  for any game, reverting to "IPL File Display" otherwise.

  **Hid IPL Controls tabs the loaded game can never have data for**,
  per Keith: "hiding functions not supported by GTA3, including some
  of the IPL Object pane tabs [GRGE] [ENEX] [JUMP] [TCYC] [AUZO] and
  [MULT], these are GTA SA only... [PICK] in ipl file, [OCCL] tabs,
  This is VC and SA only." `_apply_loaded_world` now hides GRGE/ENEX/
  JUMP/TCYC/AUZO/MULT unless the loaded game is SA, and PICK/OCCL
  unless it's VC or SA - each confirmed real by this session's own
  file-format research, not assumed. Switches back to the always-safe
  INST tab first if the currently-active tab is one that's about to
  be hidden, so the active tab never just vanishes. Same reasoning
  extends the Occlusion overlay button hiding added a few turns ago
  to the IPL Controls tab bar itself, not just that one button.

  Verified extensively against real data before trusting any of it:
  the tab-hiding set computation and the "switch away from a tab
  about to be hidden" logic, tested in isolation across GTA3/VC/SA;
  the GTA3 IDE-path row-building logic run against Keith's own real,
  complete `gta3.dat` world - `comSE.ipl` correctly resolved to 79
  real path groups (948 total node rows) out of 213 unique model IDs
  among 566 real instances placed in that file, every row's column
  count and group-header content checked against its real source
  group, and the empty-case (an IPL with no attached path groups)
  confirmed to correctly produce the placeholder message path rather
  than a crash or a blank table with no explanation.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions anywhere in the new or touched code.

  **Not yet started**: the third item from the same message - the
  Drag/Move/Rotate three-state cycle for whole-IPL dragging ("click
  to make it move ipl, click again to make it rotate ipl"). Flagged
  honestly rather than rushed alongside everything above.

- **Aug 19, 2026 (cont'd)** — Built the Drag/Move/Rotate three-state
  cycle for whole-IPL interaction, per Keith: "Same button process
  for drag ipl: 1 click turns into move ipl, click again rotate ipl,
  click back to drag ipl" - plus moved it "from row 3 to after Render
  Row 1" as he asked.

  New `_rotate_ipl_coordinates(ipl_name, pivot_x, pivot_y, angle_deg)`
  - the rotation counterpart to the already-existing `_shift_ipl_
  coordinates`, same per-section-type coverage and live-in-memory-
  only design. Verified both pieces of its math independently before
  trusting them on real data: position-around-pivot rotation (a
  known 90-degree case checked against its exact expected result,
  distance from pivot preserved across several test angles) and
  orientation rotation (reuses the already-proven `quat_to_euler_
  degrees`/`euler_degrees_to_quat` pair - the exact functions `_on_
  rotation_nudged` already uses for the Item Editor Dialog's own
  rotation nudging - confirmed a 90-degree yaw addition to an
  identity quaternion produces the exact expected result, and that
  four successive 90-degree rotations return to identity). Then ran
  the complete production logic against real `IPLInstance` objects
  one more time: distance from pivot preserved for multiple instances
  at once, and an identity-oriented instance's own orientation came
  out rotated by exactly the requested angle, not approximately.

  Honest, real format limitation carried over from `_shift_ipl_
  coordinates`' own design: cull/zone/garages have no rotation field
  of their own in the file format at all - only their own centre
  point orbits the pivot, then the box is rebuilt axis-aligned around
  that new centre afterward (moved, not spun) - only occlusion boxes
  (which do have their own `rotation` field) and instances/paths/
  entrances-exits (which have a real orientation/angle concept) can
  genuinely tilt.

  New `_prompt_rotate_ipl_coordinates` dialog mirrors the existing
  Shift Coordinates dialog's own structure - pivot automatically
  computed as the centroid of the target IPL's own instance
  positions (no need to manually locate and type one), angle entry,
  applies via `_rotate_ipl_coordinates` on Accept.

  The interaction itself: `DFFViewport` gained `_ipl_interaction_
  mode` ('drag'/'move'/'rotate') and `set_ipl_click_callback` - in
  Drag mode, a click-and-hold still starts the existing live-preview
  mouse drag unchanged; in Move/Rotate mode, a plain click instead
  immediately fires the new click callback with the picked IPL's
  name and starts no drag tracking at all. map_workshop.py's new
  `_on_ipl_click_for_move_or_rotate` opens Shift Coordinates or
  Rotate accordingly, reusing both existing dialogs rather than
  building new ones. The button itself cycles Off -> Drag -> Move ->
  Rotate -> Off on each click (`_on_ipl_mode_button_clicked`) - Off
  is a real, distinct state, not just "back to Drag", preserving the
  old checkbox's own ability to fully disable the feature so an
  ordinary click doesn't risk accidentally triggering a move/rotate/
  drag. Moved from row 3 into row 1, right after the Render dropdown,
  replacing the previous plain "Drag IPL" checkbox entirely - the
  right-click axis-lock menu built a turn ago now hangs off this same
  button instead.

  `ast.parse` clean on both touched files; confirmed via AST no
  duplicate method definitions, and confirmed via direct search zero
  remaining references anywhere in the file to the removed `_drag_
  ipl_chk` attribute or `_on_ipl_drag_mode_toggled` handler.

- **Aug 19, 2026 (cont'd)** — Two more items from Keith's follow-up.

  **Corrected axis-colour box faces to X=green, Y=red, Z=blue** - the
  colour scheme went through a brief back-and-forth this same turn
  (Keith's initial correction said "Z sides blue, Y sides green, X
  sides red", swapping X and Y from what had been built a few turns
  earlier - then a follow-up message swapped them straight back:
  "X=red/Y=green, swap them around to X-Green, Y-Red"). Final,
  settled state matches the very first spec from a few turns ago:
  green sides vary in Y (X-constant faces), red sides vary in X
  (Y-constant faces), blue caps are the Z extent. Updated both the
  actual `_draw_ghosted_box_from_corners` colour logic and the
  Settings > Render checkbox's own label text to match. Re-verified
  the final assignment directly rather than assuming the two edits
  cancelled out correctly.

  **Generalised the "<TAB> Display" dock rename to every tab, not
  just PATH**, per Keith: "when looking at cull, or zon, the IPL File
  Display should show to ZON Display or CULL Display for
  consistancy." `_on_ipl_data_type_changed` now reads the active
  tab's own label directly off the tab bar (`tabText`) rather than a
  second, separately-maintained copy of the label strings that could
  drift out of sync with `tab_specs`' own labels - INST keeps the
  original "IPL File Display" name (the default, already-familiar
  view this dock is named after, not a specialised section type the
  way the others are), PATH keeps "Paths Display" (Keith's own
  originally-requested wording for that one specifically), and every
  other tab becomes "<LABEL> Display" - CULL Display, ZON Display,
  GRGE Display, OCCL Display, and so on automatically, with no need
  to add a new case here every time a tab gets added or renamed in
  the future.

  Verified the generalised title computation directly against every
  real tab key before trusting it. `ast.parse` clean on both touched
  files; confirmed via AST no duplicate method definitions.

- **Aug 19, 2026 (cont'd)** — Built auto-highlight-on-hover, per
  Keith: "Auto object highlight setting in map_workshop settings:
  this could be a model, path node, anything in the viewpoint; once
  highlighted, right-click for options." Scoped to instances only
  for this first version - path nodes already have their own
  dedicated pick-up-and-drag interaction in Edit Paths mode, a
  genuinely different, more specific gesture than a general hover
  highlight, so left for a future pass rather than merged in without
  a clear picture of how the two should coexist if both were active.

  New `MapSettings` entry `auto_highlight_hover` (off by default - a
  real, continuous per-mouse-move cost) + a checkbox in Settings >
  Navigation, applied the same way `zoom_to_cursor` already is
  (restored at construction, applied on "Apply Settings").

  **Caught and fixed a real bug before it could ship**: `mouseMoveEvent`
  never fires at all without a button held unless `setMouseTracking
  (True)` is set on the widget - it wasn't, anywhere in this file.
  Without that one line, hover detection would have looked complete
  in the code but silently never actually run. Added it to `__init__`.

  Hover detection reuses the already-proven `_pick_world_instance`
  (same picking used for double-click-to-edit and whole-IPL
  dragging) in `mouseMoveEvent`, only when no mouse button is held at
  all, so it never fights with the existing rotate/pan/drag
  interactions, which already have their own meaning for mouse
  movement. New `_draw_hover_highlight` marks the hovered instance
  with a semi-transparent yellow sphere, reusing the exact same
  `gluSphere`/lazily-created-quadric technique already proven for the
  cull/zone/occlusion box corner handles, rather than a new,
  separate rendering approach.

  Right-click-for-options distinguishes a genuine click from a
  right-click-drag (camera rotation already uses right-click-drag)
  by comparing the release position against where the press started,
  with a small pixel tolerance for a hand that isn't perfectly still
  between the two - verified this distinguishing logic directly
  across four cases (a real click, a large drag, a click with
  nothing hovered, and no press position recorded at all) before
  trusting it. `_on_hover_context_menu` reuses the two already-
  existing per-instance actions rather than inventing new ones: Info
  opens the same Item Editor Dialog double-clicking already does,
  Show Textures loads that model's textures exactly as the IPL Inst
  File table's own right-click menu already does - both genuinely
  shared code paths, not copies.

  `ast.parse` clean on both touched files; confirmed via AST no
  duplicate method definitions, including the handler that was still
  missing (and would have crashed on first right-click) at the point
  this feature was last checked in.

- **Aug 19, 2026 (cont'd)** — Built Snap: Centre of Model for whole-
  IPL dragging, per Keith: "[Drag ipl] any direction; if the snap
  options are on, icons already exist on ribbons; use Edge of model,
  Centre of model, then we can remove the snaps we dont need from
  the ribbons; we don't need extrude or emboss ribbon icons either."

  Confirmed before touching anything: the existing 7 Snap Target
  ribbon buttons (Grid/Pivot/Vertex/Endpoint/Midpoint/Edge/Face,
  inherited from Model Workshop's own mesh-editing base) had never
  been wired to any real behaviour at all - `_snap_targets`' own
  existing comment already said so explicitly ("the actual snap-
  during-drag math... is a follow-up task, not wired yet"). Removed
  5 of the 7 (Grid/Pivot/Vertex/Endpoint/Midpoint) entirely, kept
  Edge and repurposed the 7th into Centre of Model.

  **Snap: Centre of Model is now genuinely functional** - while
  dragging a whole IPL, once the clicked instance's own would-be
  position comes within 3 units of any other instance's real
  position (excluding the dragged IPL's own instances, so it can't
  snap to one of its own siblings), the drag delta gets nudged to
  land exactly on that instance's position instead of merely close
  to it. Verified this search-and-snap logic directly: correctly
  snaps to a real nearby target in a different IPL, correctly leaves
  the delta untouched when nothing is within range, and correctly
  excludes a same-IPL sibling even when it would otherwise be the
  closest candidate.

  **Snap: Edge of Model stays a real, honest gap, not faked** - its
  own ribbon button is present but disabled with an explanatory
  tooltip, since a genuine "snap to a model's own edge" needs that
  model's loaded geometry bounding box, which doesn't exist anywhere
  in this viewport currently (confirmed via direct search before
  writing anything). Approximating it with an arbitrary offset would
  have looked like real geometry-based snapping while actually being
  a guess - worse than being upfront that it isn't built yet.

  Removed the Create Primitive/Extrude Faces ribbon icons a few turns
  back already covered "we don't need extrude or emboss" - no further
  action needed there this pass.

  `ast.parse` clean on both touched files; confirmed via AST no
  duplicate method definitions, and confirmed via direct search zero
  remaining references anywhere in the file to the 5 removed snap
  action attributes.

- **Aug 19, 2026 (cont'd)** — Completed the fuller Move/Rotate
  dialogs with per-section-type tick-boxes and +/- nudge buttons, per
  Keith: "[Move ipl] any direction; using -/+ z value -/+ x value,
  -/+ y value. right-click options for move ipl, move paths, move
  zones, move tracks, move cull, move occlusion. Tick the options you
  want to move" (and the same for Rotate).

  Found this half-built on resuming: `_shift_ipl_coordinates` already
  had a working `include_types` parameter (from an earlier pass whose
  details didn't make it into a recorded summary before a context
  boundary), but `_rotate_ipl_coordinates` hadn't been updated to
  match, and neither dialog actually exposed tick-boxes or nudge
  buttons to the person at all - the backend could do selective
  moves, nothing in the UI let anyone actually use it. Completed
  properly rather than re-guessing from scratch: added the matching
  `include_types` parameter to `_rotate_ipl_coordinates` (identical
  pattern to the shift version), then rebuilt both dialogs.

  New shared `_add_ipl_section_type_checkboxes` (Instances/Paths/
  Zones/Cull/Garages/Entrances-Exits/Occlusion, all checked by
  default, plus a separate Tracks checkbox) used by both dialogs
  rather than duplicated. Each axis/angle spinbox gained its own -/+
  buttons for incremental nudging (1 unit or 5 degrees per click).

  New `_shift_all_tracks`/`_rotate_all_tracks` - Tracks isn't one of
  `include_types`' own keys, and deliberately so: train tracks
  (data/paths/tracks.dat) are never tied to any specific IPL's own
  `source_ipl` at all, so "move tracks for this one IPL" has no real
  per-IPL data to act on. Ticking Tracks instead moves every loaded
  track globally, with the checkbox's own tooltip saying so plainly
  (and defaulting to unchecked, since that's a bigger, separate
  effect than the rest of the tick-boxes) rather than quietly
  scoping to "nearby" tracks in a way the data doesn't actually
  support.

  **Caught and fixed a real editing mistake before it could ship**:
  an earlier replacement accidentally deleted the `_on_ipl_dragged`
  method's own signature line and docstring opening, leaving its
  docstring's tail floating with no method definition above it -
  would have been a straight `SyntaxError` on import. Caught by the
  routine `ast.parse` check immediately after the edit, diagnosed
  precisely, and restored before moving on to anything else.

  Verified the selective-move filtering logic directly against real
  `IPLInstance`/`CullEntry` objects: ticking only Instances correctly
  left a Cull box untouched, ticking both moved both. `ast.parse`
  clean; confirmed via AST no duplicate method definitions anywhere,
  including `_on_ipl_dragged` after the fix.

- **Aug 19, 2026 (cont'd)** — Built unique-colour-per-box, per Keith:
  "add colour zone boxes" (with real reference screenshots of several
  distinctly-coloured cull/zone boxes side by side, confirming this
  means each individual box getting its own colour, not the axis-
  face colouring already built - a different, additional visual
  mode).

  New `_box_unique_colors` setting + an 8-colour fixed palette
  (orange/green/magenta/red/blue/yellow/purple/teal), assigned
  deterministically by each box's own index within its loaded list
  (cycling past 8) via a new `_palette_color_for_index` - the same
  box always gets the same colour within a session, not a true
  random colour that would flicker differently on every reload.
  Applied to all three box types (`_draw_ghosted_boxes`/cull, `_draw_
  zone_boxes`, `_draw_occl_boxes`) - zone's own wireframe style
  needed its per-box `glColor3f` call moved inside its drawing loop
  (was set once outside it) to actually support per-box colour there
  too, not just the filled ghosted/translucent styles. Takes a back
  seat to axis-coloured faces if both are on at once, matching how
  the two visual modes were always going to need a defined
  precedence rather than fighting over the same box. New checkbox in
  Settings > Render's "Cull / Zone / Occlusion Boxes" group, wired
  the same save/restore/apply pattern as the existing axis-colours
  checkbox exactly.

  Verified the palette-cycling logic directly: deterministic (same
  index always the same colour), correctly wraps past the 8-entry
  palette length. `ast.parse` clean on both touched files; confirmed
  via AST no duplicate method definitions.

  **Also from the same message, addressed as clarification rather
  than code**: Keith's own question about whether Drag currently
  moves "a single object or selected objects using Shift" surfaced a
  real mismatch - the existing Drag mode actually moves the ENTIRE
  IPL a clicked instance belongs to, not a single object or a multi-
  selection. Flagged this honestly rather than assuming or silently
  building around it. His descriptions of Move/Rotate selecting IPLs
  via Shift-click in the IPL Sections list (plus a "select all IPLs"
  right-click option) describe a genuinely different selection model
  than what exists (currently: click an instance in the viewport) -
  a real, separate piece of scope, not started this pass. His "add
  the option to also rotate paths, zones, and cull" was confirmed
  already built (the per-section-type tick-boxes from the previous
  session) - pointed out in case it hadn't been pulled/tested yet
  rather than assumed already seen.

- **Aug 19, 2026 (cont'd)** — Implemented the careful Ctrl/Shift Drag
  workflow Keith worked through step by step: "we can build on this;
  holding [left control] left click entire .ipl is dragged / holding
  [left shift] and select multi entire ipls, ... if there all
  selected, it drags them all."

  **Ctrl+click+drag** an instance now immediately drags just that one
  IPL - the original behaviour, unchanged in effect, just gated
  behind Ctrl now instead of being what a plain click always did.
  **Shift+click** an instance doesn't drag anything by itself - it
  toggles that instance's own whole IPL into/out of a running multi-
  selection, one Shift+click per IPL to add or remove it. A later
  **plain click+drag** (no modifier), as long as that selection is
  non-empty, drags every one of the selected IPLs together as one
  combined rigid-body move. A plain click+drag with nothing selected
  now deliberately does nothing at all - Ctrl is the explicit,
  intentional gesture for a single-IPL drag, so an unmodified click
  doesn't risk starting a drag by accident.

  Generalised `_dragging_ipl_name` (a single string) to `_dragging_
  ipl_names` (a set) throughout - `mousePressEvent`'s pick logic now
  branches on `event.modifiers()` to build either a one-name or
  multi-name drag set; `mouseMoveEvent`'s live-preview logic and
  `mouseReleaseEvent`'s commit logic both work over the whole set
  unchanged in structure, just iterating where they used to reference
  one name directly - the commit callback fires once per dragged IPL
  name, all with the same delta, reusing `_shift_ipl_coordinates`
  unchanged for each one rather than needing a new "shift several
  IPLs at once" method.

  **Caught and fixed a real, pre-existing bug while generalising this
  code**: Snap: Centre of Model's own exclusion check referenced a
  bare `ipl_name` that was never actually defined anywhere within
  `mouseMoveEvent`'s own scope at all (it only ever existed as a local
  variable inside the separate `mousePressEvent` method) - a
  `NameError` waiting to happen the first time anyone actually dragged
  with that snap mode turned on. Found while updating this same
  exclusion check to work against the new multi-name set instead of
  one name, not gone looking for it specifically - fixed as part of
  the same edit.

  New `set_ipl_selection_callback`/`_on_ipl_selection_changed` - the
  status bar reports the current Shift-built selection (names for up
  to 4 IPLs, just a count beyond that) so there's some feedback that
  a Shift+click actually registered, without building a whole
  separate always-visible selection panel for this first version.

  Verified the complete workflow end-to-end against real
  `IPLInstance` objects: Shift+click selection building, a plain drag
  correctly copying (not aliasing) the current selection, `start_
  state` correctly covering every selected IPL's own instances while
  excluding an unselected one, the commit callback firing once per
  dragged name with the same delta, Ctrl+click's single-IPL drag
  staying independent of whatever else is selected, and the real
  position mutation itself landing correctly on both selected
  instances while leaving the unselected one untouched. `ast.parse`
  clean; confirmed via AST no duplicate method definitions and, via
  direct search, zero remaining references anywhere in the file to
  the old singular `_dragging_ipl_name` attribute.

- **Aug 19, 2026 (cont'd)** — Refined the multi-IPL workflow after
  Keith's own second pass at it: "Rethinking this; Shift + left-click
  selects the entire .ipls in the Object Browser, with right-click
  options: Load All, Unload All, Select All, Deselect All; shown in
  the status bar... Left Control key, click and hold left mouse drags
  entire IPL(s)."

  **Selection moved to (also) work from the IPL Sections table
  itself**, not just Shift+clicking instances in the 3D view -
  discovered Ctrl/Shift+click multi-select was already enabled there
  from an earlier request (`ExtendedSelection` mode), so the missing
  pieces were the confirmation/feedback and the actual wiring into
  the drag workflow, not the underlying Qt selection mechanism
  itself. New `itemSelectionChanged` handler (`_on_ipl_sections_
  selection_changed`) reads the table's own selected rows and syncs
  them into the SAME shared selection set the viewport's own
  Shift+click already builds (new `DFFViewport.set_multi_selected_
  ipl_names`) - both selection surfaces feed one underlying set, so
  it doesn't matter which one was actually used to build it.

  New right-click actions on the IPL Sections table: **Select All**/
  **Deselect All** (the table's own `selectAll()`/`clearSelection()`),
  and **Load All**/**Unload All** - deliberately whole-list actions,
  distinct from the already-existing "Load Selected", not scoped to
  whatever's currently selected at all. New `_unload_all_ipl_sections`
  doesn't just loop the existing single-IPL `_unload_ipl_section` (that
  method does its own full refresh on every call, so looping it would
  repeat that refresh once per IPL for nothing) - does the same
  underlying per-list removal work directly across every loaded IPL,
  then refreshes exactly once at the end.

  **Simplified the Drag gesture itself** after this second pass:
  Ctrl+click+hold+drag is now the one and only way to actually start
  a drag - "drags entire IPL(s)" (plural) means it drags whatever's
  currently selected if there's an active selection, or falls back to
  just the clicked instance's own IPL if nothing's selected. A plain
  click with no modifier now does nothing at all - dropped the
  earlier version's "plain click drags the selection, Ctrl drags just
  one" split, which needed two different triggers for what's really
  the same underlying action (Ctrl now covers both cases on its own,
  simpler and less to remember).

  Status bar wording now matches Keith's own example exactly ("N
  entire ipl(s) selected") - the existing `_on_ipl_selection_changed`
  (from a session or two ago's viewport-side Shift+click work) is
  reused directly for the table's own selection changes too, rather
  than duplicating the same formatting in a second method.

  Verified directly against real `IPLInstance` objects before
  trusting any of it: Ctrl+click-with-selection drags the whole
  selection even when the actually-clicked instance isn't part of it
  (confirming the "drags entire IPL(s)" intent, not just "drags
  whichever one you happened to click"), Ctrl+click-with-nothing-
  selected correctly falls back to the one clicked IPL, the per-name
  commit loop still fires correctly for a multi-IPL result, and
  Unload All's own filtering correctly removes every instance across
  multiple loaded IPLs while leaving an unloaded one completely
  untouched. `ast.parse` clean on both touched files; confirmed via
  AST no duplicate method definitions.

- **Aug 19, 2026 (cont'd)** — Extended Move/Rotate to respect the
  multi-IPL selection, per Keith: "lets add the Paths, ZOnes, Cuil
  for move and rotate" - the per-section-type tick-boxes themselves
  already existed; what was still single-IPL-only was the selection
  Move/Rotate actually operate on, unlike Drag's own Ctrl+drag which
  already picked up the whole current selection.

  `_prompt_shift_ipl_coordinates`/`_prompt_rotate_ipl_coordinates`
  now both accept either a single IPL name (a plain string - the IPL
  Sections right-click menu's own call, unchanged) or any collection
  of several (the same shared multi-IPL selection Drag's own Ctrl+
  drag already uses). `_on_ipl_click_for_move_or_rotate` now checks
  that selection first - if it's non-empty, the dialog applies to
  every selected IPL at once instead of just the one clicked
  instance's own IPL, exactly matching how Ctrl+drag already ignores
  which specific instance started the drag and moves the whole
  selection together.

  Rotate's own pivot, for a multi-IPL selection, is now the ONE
  shared centroid across every selected IPL's instances combined -
  not each IPL's own separate centre - so a multi-IPL rotate spins
  the whole group together around one common point, matching how the
  equivalent multi-IPL drag already moves everything as one rigid
  body rather than each IPL drifting toward its own destination.

  **Real correctness issue caught and handled deliberately, not
  guessed at**: Tracks is genuinely global data (not tied to any
  single IPL's own `source_ipl` at all - already true before this
  change). Looping the tracks shift/rotate call once per selected IPL
  name would have applied the same offset to the same global tracks
  multiple times over for a multi-IPL selection - both dialogs' own
  accept handlers call `_shift_all_tracks`/`_rotate_all_tracks`
  exactly once per Accept, outside the per-IPL loop entirely, not
  once per name.

  Verified directly against real `IPLInstance` objects before
  trusting any of it: a 2-IPL shift correctly calls `_shift_ipl_
  coordinates` once per name and moves both IPLs' real instances,
  the tracks shift fires exactly once despite 2 IPLs being moved (the
  specific bug this design deliberately avoided), the single-IPL
  string-input case still works unchanged, and the combined-centroid
  pivot for a real 2-IPL, 4-instance selection came out correctly
  distinct from either IPL's own separate centre. `ast.parse` clean;
  confirmed via AST no duplicate method definitions.

- **Aug 19, 2026** — Found and fixed the real root cause of settings
  not persisting, per Keith: "everything clicked in Map Workshop
  Settings, isn't remembered either." A safety net (QApplication.
  aboutToQuit flush) was added for this a while back, but never
  confirmed as the actual root cause - this time traced it properly
  instead of adding another guess.

  Real cause: `MapSettings()` gets constructed fresh inside `ModelWorkshop
  .__init__`, and `ModelWorkshop(...)` itself gets constructed from 7
  separate places in this file (opening from the menu, docked,
  undocked, standalone `__main__`, and others) - each one creating its
  own, completely independent `MapSettings` object, each with its own
  in-memory snapshot loaded from disk at whatever moment THAT
  particular instance happened to be constructed. With more than one
  `ModelWorkshop` alive in the same process at once (docked and
  standalone simultaneously, or simply re-opening the tool without the
  previous instance being fully garbage-collected first), an earlier
  instance's own stale `MapSettings` could still be sitting in memory
  - and if it ever saved again for any reason, even something
  completely unrelated, it would silently overwrite whatever a newer
  instance had already saved with its own older snapshot. Every
  genuinely new value really was written to disk correctly in the
  moment - it just kept getting quietly clobbered again shortly after
  by a second, stale copy nobody knew still existed.

  Fixed by making `MapSettings` a real singleton (`__new__` returns
  the same object on every subsequent construction; `__init__`'s own
  body - the disk load, the save timer setup - only runs once, guarded
  by `_map_settings_initialized`, so later `MapSettings()` calls don't
  re-run setup and clobber real, already-loaded state back to a fresh-
  from-disk snapshot). Every `ModelWorkshop`, regardless of which of
  the 7 construction sites created it or how many are alive at once,
  now shares the exact one object and the exact one in-memory data -
  there's no second, independent snapshot left to go stale and
  overwrite anything, because there's only ever one. `MapSettingsDialog`
  (a separate, older settings dialog class) takes a `MapSettings`
  instance as a constructor parameter rather than creating its own,
  so it benefits from this fix automatically with no changes of its
  own needed.

  Verified the singleton pattern directly (PyQt6 unavailable in this
  sandbox to instantiate the real `QObject`-based class, so mirrored
  the exact `__new__`/`__init__` interaction without the Qt
  dependency): a second construction returns the identical object, a
  change made through the first "instance" is immediately visible
  through the second (no stale snapshot), the real setup logic inside
  `__init__` runs exactly once despite being called multiple times,
  and a third construction changing an unrelated key correctly
  preserves an earlier change instead of overwriting it - the exact
  race this fix closes.

  Also logged three new TODO items per the same message: keeping a
  mission SCM file's own hardcoded world coordinates in sync with a
  moved map section, regenerating the radar/minimap to match a map
  section's new position after any move/shift/drag/rotate, and
  keeping `waterpro.dat` in sync with moved model positions - all
  real, substantial, explicitly deferred requirements, not started.

  `ast.parse` clean.

- **Aug 19, 2026 (cont'd)** — Added dock/splitter layout persistence,
  per Keith: "General UI-state persistence — splitter positions, dock
  layout, tab order not remembered between sessions."

  Uses Qt's own built-in `QMainWindow.saveState()`/`restoreState()`
  rather than hand-tracking each dock's own position/size/floating
  state separately - one call captures everything at once (position,
  size, floating, and tab grouping/order within a shared area), and
  works because every dock in this workshop already had a stable,
  unique `setObjectName()` call in place (confirmed via direct search
  before relying on this, not assumed - Object Browser, Instance
  List, IPL Object Editor, Control Panel, IPL Controls, IPL Inst
  File, Editing Panel, World View), which is what `restoreState()`
  actually keys off to match a saved dock back to the real widget.

  New `workshop_dock_state` `MapSettings` entry stores a base64-
  encoded copy of the `QByteArray` `saveState()` returns - JSON
  (`MapSettings`' own storage format) can't hold a `QByteArray`
  directly. New `_save_dock_state` (called from `closeEvent`,
  alongside the existing settings flush) and `_restore_dock_state`.

  The restore is scheduled via `QTimer.singleShot(0, ...)` right
  after `__init__`'s own final step, rather than called directly
  inline - `setup_ui()` itself delegates most of the actual dock
  construction through several further layers of its own sub-methods
  (the real "World View" dock, for instance, gets created hundreds of
  lines away in a completely different method), so scheduling this
  for the very next event loop iteration guarantees every dock
  already exists by the time it actually runs, regardless of how
  deeply that construction chain is nested - avoids needing to hunt
  down and depend on the exact last line of whichever sub-method
  happens to run last today, which would be fragile to future
  changes in construction order.

  Verified the base64 encode/decode round-trip directly (PyQt6
  unavailable in this sandbox to test the real `saveState()`/
  `restoreState()` calls) - arbitrary binary data survives encode-
  then-decode exactly, and separately survives a full round-trip
  through actual `json.dumps`/`json.loads` (the real storage
  mechanism `MapSettings` itself uses), confirming the base64 layer
  correctly bridges `QByteArray`'s binary data through JSON's own
  text-only format without any corruption. The empty-string default
  (no saved layout yet) correctly short-circuits via a plain falsy
  check rather than attempting to decode nothing.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions (the two `closeEvent` matches are two different
  classes, each with their own, not a real duplicate).

- **Aug 19, 2026 (cont'd)** — Built real box-corner resizing for
  cull/zone boxes, the genuine prerequisite for No-Clip box editing
  (per Keith: "lets continue to complete that list. starting with
  No-clip" - the corner-sphere handles had been purely visual since
  Aug 16, "actually moving a corner to resize the box is a separate,
  larger follow-up... mouse picking, drag math, and live data
  mutation, none of which exist yet"). Then built No-Clip itself on
  top of it.

  New `set_cull_box_owners`/`set_zone_box_owners` on `DFFViewport` -
  a parallel identity list (same index/order as the existing plain-
  tuple `set_cull_boxes`/`set_zone_boxes`), mirroring `set_path_node_
  owners`' own already-proven pattern exactly: the widget still never
  imports `CullEntry` or deals in it directly for drawing, this is
  purely so a resize commit can be applied back to the real, live
  object it actually came from. `_refresh_cull_box_visualization`/
  `_refresh_zone_box_visualization` updated to build and pass these
  alongside the existing tuple lists.

  New `_register_pickable_box_corners` (shared by cull's own `_draw_
  ghosted_boxes` and both of zone's own render-style branches -
  wireframe and ghosted/translucent - rather than duplicating the
  same opposite-corner bookkeeping three times), building `self.
  _pickable_box_corners` fresh every full render pass (cleared once,
  right before either box type might register into it - clearing
  inside either individual draw method would have wiped whichever box
  type registered first when the other one's draw call ran right
  after it).

  New `_pick_box_corner` (same `_pick_ray`/`_closest_point_on_ray`
  pattern `_pick_path_node` already uses) and the full click-drag-
  release interaction in `mousePressEvent`/`mouseMoveEvent`/`mouse
  ReleaseEvent` - a 2D, height-preserving drag (same reasoning as
  path node editing: a 2D mouse can't unambiguously set 3 coordinates
  at once), with the diagonally opposite corner (both in XY and in Z)
  staying fixed throughout, the same way dragging one corner of a
  selection rectangle keeps the opposite one anchored. Live preview
  mutates the actual box tuple sitting in `self._cull_boxes`/`self.
  _zone_boxes` directly at the dragged corner's own box index - these
  lists are genuinely persistent between frames (only ever replaced
  wholesale when map_workshop.py calls the setter again), so this was
  enough for the very next `paintGL` call to draw the resize
  immediately without needing a second, parallel "live preview" data
  structure the way path nodes needed one.

  **No-Clip** (`set_no_clip_boxes`, new `no_clip_boxes` `MapSettings`
  entry + Settings > Render checkbox), per Keith: "have a no-clipping
  option where you can't move one box into another." New `_box_
  resize_would_overlap` - a standard AABB-vs-AABB overlap test
  (strict inequalities, so boxes that merely touch edge-to-edge with
  zero actual overlap volume aren't flagged) against both `_cull_
  boxes` and `_zone_boxes` together, not scoped to same-type
  collisions only - Keith's own wording wasn't scoped that way, and
  there's no real reason a cull box overlapping a zone box would be
  any less of a mess than two cull boxes overlapping each other.
  Deliberately simplified from an initially-considered "clamp to the
  closest non-overlapping size": a resize that would overlap is
  simply rejected for that frame, holding the box's last known-good
  size - computing an automatic closest-fit clamp is a genuinely
  harder geometric problem (especially with several other boxes
  potentially blocking from different directions at once) that a
  rushed version could easily get subtly wrong in a way worse than
  simply holding still.

  New `_on_box_resized` (map_workshop.py) commits the final extents
  straight to the real `CullEntry`/zone dict, then does a full
  visualization refresh. Deliberately leaves `CullEntry`'s own
  `center_x/center_y/center_z` completely untouched during a resize -
  per that dataclass's own documented real-world oddity ("changing
  the zone's center coordinates does not directly affect the zone
  itself... stored verbatim rather than derived"), recomputing it
  here would be an invented side effect nothing asked for.

  Cull and Zone's own `_MapOverlayToggleButton`s gained real right-
  click edit mode (`supports_edit=True`, were `False` - "no edit mode
  yet" was accurate until this pass) - new shared `_on_edit_boxes_
  toggled` handles both buttons through the one underlying viewport
  toggle (cull and zone share the same corner-picking mechanism, so
  there's only one real edit-mode state, not two independent ones),
  keeping both buttons' own visual edit-state in sync with each other
  via `set_editing` (which deliberately doesn't re-emit its own
  signal, avoiding recursion back into this same handler).

  Verified thoroughly against real data before trusting any of it:
  the AABB overlap test across 4 real cases (genuine overlap, clear
  separation, edge-touching with zero volume, same XY footprint but
  separated in Z), and the full commit path against a real `CullEntry`
  and a real zone dict - confirming the dragged corner updates
  correctly, the opposite corner and untouched fields stay exactly as
  they were, and `CullEntry`'s own center fields are genuinely left
  alone rather than silently recomputed. `ast.parse` clean on both
  touched files; confirmed via AST no duplicate method definitions.

- **Aug 19, 2026** — Wired SA's real nodesN.dat vehicle/ped path data
  into GTAWorldLoader, per Keith: "i'd be nice to see whats in those
  node.dat files, for SA". Loader-level integration only this pass -
  loading, not yet visualization or an editor.

  Investigated first rather than assuming: an earlier claim this
  session that "the game ignores nodesN.dat" turned out to be
  incomplete and needed correcting - the real, confirmed picture
  (multiple independent sources: GTAMods, open.mp, Grand Theft Wiki,
  all agreeing) is that the game genuinely DOES use nodesN.dat data
  for real vehicle/ped AI pathfinding, but specifically the 64 area
  files packed INSIDE gta3.img (or another archive) - it's only the
  SEPARATE, loose copies sitting in data/paths/ on disk that are
  unused leftovers, believed to be dev-time output from a path
  compiler removed before the game shipped. apps/methods/sa_path_
  parser.py's own existing docstrings already had this distinction
  right; this session's own earlier summary of it didn't, and got
  corrected properly rather than left standing.

  New `GTAWorldLoader.load_sa_nodes(game_root, data_dir)`, SA-only
  (gated by `self.game == GTAGame.SA`, called once at the end of
  load_from_dat) - tries the real, in-archive location first
  (game_root/models/gta3.img, case-insensitive fallback matching load
  _tracks_dat's own established convention), using sa_path_parser's
  already-built find_nodes_dat_in_img/load_nodes_dat_from_img_entry.
  Falls back to the loose data/paths/ directory only if the archive
  isn't found/openable - genuinely useful for reference/comparison
  even though that copy isn't the one the game itself reads. New
  `self.sa_nodes: Dict[area_id, SAPathFile]`, all 64 areas loaded
  together rather than one at a time on demand - links between path
  nodes can cross between areas, so resolving a link's own target
  position correctly needs the whole combined set already loaded.

  The `IMGFile` import is local to `load_sa_nodes` itself, not at
  gta_dat_parser.py's own module level - `img_core_classes.py` pulls
  in PyQt6, and this module is deliberately kept GUI-free at import
  time (per sa_path_parser.py's own stated design goal: usable
  headless, by other tools besides Map Workshop) - only a caller that
  actually calls this specific method pays that import cost.

  Verified against synthetic data (the same honesty standard sa_path_
  parser.py's own existing code already holds itself to - not yet
  verified against a real gta3.img/nodesN.dat sample): the IMG-
  scanning and entry-loading functions against mock IMG objects, and
  the new load_sa_nodes method itself against real filesystem paths -
  confirmed it fails gracefully (no crash, empty result) against
  nonexistent or empty directories, and correctly falls back to a
  real loose nodesN.dat file when no archive is present. `ast.parse`
  clean; confirmed via AST no duplicate method definitions.

  **Not yet built**: visualization (rendering path nodes/links in the
  3D viewport - a separate piece needing cross-area link resolution
  and a rendering approach, since paths form a graph, not a simple
  line strip) and the "LC, VC" part of Keith's same message, which
  needs clarifying first - GTA III and VC don't actually have a
  nodesN.dat equivalent at all (III uses its own IDE-embedded system,
  VC uses the text IPL "path" section, both already fully supported
  elsewhere in this app).

- **Aug 19, 2026 (cont'd)** — Investigated real path-related .dat data
  Keith uploaded for all 3 games at once (LC_Paths_Folder.7z, VC_data
  _paths.7z, VC_map_folder_dat.7z, SA_other_Dat.7z), verified sa_path_
  parser.py against real data for the first time, found and fixed a
  real gap in tracks.dat handling, and extended tracks.dat/tracks2/3/
  4.dat and flight*.dat/spath0.dat loading together.

  **sa_path_parser.py verified against real, complete data for the
  first time** - all 64 real NODES0-63.DAT files (SA_other_Dat.7z)
  parsed with zero errors: 30,587 vehicle nodes, 37,650 ped nodes,
  31,466 navi nodes, 143,622 links, all plausible for a full SA map.
  Real node positions and link targets checked directly, not just
  "it didn't crash" - a genuine first confirmation this parser, built
  from documentation alone back on Aug 14, actually matches real
  on-disk data.

  **Confirmed flight.dat/flight2/3/4.dat and spath0.dat are the exact
  same "count then X Y Z lines" shape as tracks.dat** - verified
  directly against 9 real files across LC and VC (every header count
  matched its own actual data-line count exactly). Also confirmed VC
  and SA's own spath0.dat are byte-for-byte identical (same 79
  points) - possibly a shared template/test file rather than either
  game's own real map data, noted rather than assumed either way.

  **Real gap found and fixed**: SA genuinely has FOUR tracks files
  (tracks3.dat/tracks4.dat too, confirmed present in Keith's real SA
  sample, same format as tracks.dat/tracks2.dat) - `load_tracks_dat`'s
  own `wanted` set only ever looked for two, silently missing two
  real, valid files every time it ran against a real SA install.

  **Real data previously silently dropped, now captured**: every real
  SA tracks*.dat line actually has 4 values, not 3 (`X Y Z FLAG`) -
  VC/GTA III's own tracks.dat/tracks2.dat samples only ever had 3,
  which is why the format was understood as 3-only originally. The
  existing parser's own `len(parts) < 3` check already tolerated the
  extra value without crashing, but silently threw it away rather
  than storing it. New `TrackWaypoint.flag` (`Optional[int]`, `None`
  when a line only has 3 values) captures it. Checked its real range
  across all 4 real SA files: always 0 or 1, and in the largest file
  (tracks.dat, 926 points) exactly 6 points carry a 1 while every
  other point (and every point in the 3 smaller files) carries 0 - a
  plausible match for "this is a real station stop" given SA has 6
  real train stations, presented as a hypothesis rather than a
  confirmed fact since no published documentation of this specific
  field was found anywhere.

  **Investigated and catalogued, not yet built on**:
  - `VC_map_folder_dat.7z`'s 8 `map0.dat`-`map7.dat` files are
    confirmed byte-identical to each other (same MD5) and already the
    ordinary `gta_vc.dat`-style directive format this app already
    fully understands - nothing new needed, not 8 separate map areas
    as the folder name might suggest.
  - SA's own `train.dat`/`train2.dat` are a real, different format
    from GTA III/VC's own `train.dat` (a documented, unrelated
    cinematic-camera format for the Portland El/subway train view,
    confirmed never actually read by the game even in III/VC - VC's
    own copies are leftovers from III). SA's version is genuinely
    undocumented anywhere found - real comma-separated numeric data
    (14-15 values/line) with a clear internal pattern (two XYZ
    triplets whose own Z values are exact mirror negatives of each
    other, alongside a 999,999,999 sentinel and two trailing scalars)
    but no confirmed field meanings yet.
  - GTA III's real `CHASE0-19.DAT` (14 of 20 present in the real
    upload) and SA's `ROADBLOX.DAT` - both real, binary, not yet
    investigated in depth this pass.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 19, 2026 (cont'd)** — Built ROADBLOX.DAT and CHASE*.DAT
  parsers, per Keith's real samples: "lets do those next."

  **ROADBLOX.DAT** (SA police roadblock placements) - format found
  via real, published documentation (GTAMods wiki), confirmed against
  Keith's own real file: a 4-byte int32 count followed by a fixed 325
  (area_id: int16, node_id: uint16) slots, matching the real file's
  exact 1304-byte size (4 + 325*4) - only the first `count` slots are
  meaningful. New `RoadblockEntry` dataclass, `GTAWorldLoader.load_sa_
  roadblox` (SA-only), `self.sa_roadblocks`. Went beyond "it parses
  without error": cross-referenced all 325 real entries in Keith's
  own real ROADBLOX.DAT directly against his own real, complete
  NODES0-63.DAT set - every single one resolves to a genuinely valid
  vehicle node index within its own stated area, confirming the
  format's real, direct relationship to the SA node system this
  session already verified, not just a plausible-looking byte layout.

  **CHASE*.DAT** (GTA III introduction-cutscene chase-scene car
  paths) - format found via real, published documentation (GTAMods
  wiki: "near identical to its successor, RRR, in San Andreas"),
  confirmed against Keith's own real CHASE0.DAT: no header or count
  at all, just a plain, fixed 28-byte record repeated for the whole
  file - the real file's own size (151200 bytes) divided cleanly by
  28 with zero remainder (5400.0 exactly), and real decoded positions
  form a tight, plausible cluster of GTA III world coordinates that
  change smoothly frame-to-frame, confirming this isn't a
  coincidental byte alignment. New `ChaseFrame` dataclass (velocity,
  right/top orientation basis vectors, steering/gas/brake/handbrake,
  world position, all per-frame), `GTAWorldLoader.load_chase_dat`
  (GTA III-only) + `_parse_chase_file`, `self.chase_paths` keyed by
  source filename. Scans for any `CHASE<N>.DAT` present (regex
  match) rather than a fixed list of exactly 20 - Keith's own real
  upload only had 14 of the 20 possible index numbers present, so a
  fixed "must have all 20" list would have silently skipped real,
  present files.

  Verified both end-to-end via `GTAWorldLoader` directly against
  every one of Keith's own real files: all 325 real ROADBLOX.DAT
  entries load and resolve correctly; all 14 real CHASE*.DAT files
  load with plausible frame counts (2400 or 5400 depending on the
  car/path) and world-coordinate ranges. `ast.parse` clean; confirmed
  via AST no duplicate method/class definitions.

  **SA's own train.dat/train2.dat remain genuinely undocumented** -
  no published spec found despite another focused search attempt;
  the real, empirically-observed structural pattern from the last
  session (two XYZ triplets whose Z values mirror each other, a
  999,999,999 sentinel, two trailing scalars) stands, but isn't being
  turned into a parser without either a real spec or enough
  additional real samples to test a hypothesis against with
  confidence.

- **Aug 19, 2026 (cont'd)** — Built viewport visualization for SA's
  real path node graph, per Keith: "lets continue" - the natural next
  step after last session's loading-only work on `nodes*.dat`.

  New `DFFViewport.show_sa_nodes`/`set_sa_node_segments`/`_draw_sa_
  nodes` - genuinely different from `_draw_tracks`' own single-
  continuous-strip-per-file approach: SA's real path data is a graph
  (a node can have more than 2 links, and links aren't necessarily
  chained in any order), so this draws independent `GL_LINES` segment
  pairs rather than a `GL_LINE_STRIP`. New "SA Nodes" toggle button
  next to Tracks in the IPL Controls row.

  New `_refresh_sa_node_visualization` (map_workshop.py) resolves each
  real link's own target node position - the real work here, since a
  link only stores `(area_id, node_id)`, and the target can genuinely
  be in a different area file than the source (confirmed by the
  wiki: "there can be connections between separate areas"). Double-
  linked (undirected) graph per the wiki - deduplicated so each real
  edge is only added once, from whichever end sorts first, rather
  than drawn (and held in memory) twice over.

  **Real, previously-undiscovered format quirk found and fixed while
  building this**: resolving every real link across Keith's own
  complete, real 64-area map left exactly 45,835 links unresolved -
  100% of all ped-originated links, 0% of vehicle-originated ones, a
  clean systematic split rather than noise. Root cause, confirmed by
  direct measurement rather than left as a guess: a PED link's own
  `node_id` is a COMBINED index into its target area's `vehicle_
  nodes`+`ped_nodes` as one contiguous array - not a `ped_nodes`-only
  index the way a VEHICLE link's `node_id` already correctly is (this
  is exactly why `ROADBLOX.DAT`'s own real entries, which only ever
  reference vehicle nodes, already resolved perfectly last session
  with no adjustment needed). Subtracting the target area's own
  vehicle-node count from a ped link's `node_id` before indexing
  brought the failure count from 45,835 down to exactly zero.
  Documented this clearly on `SAPathLink`'s own docstring in `sa_
  path_parser.py` (not just fixed silently in this one caller) so any
  future consumer of this data doesn't have to rediscover it blind.

  Verified extensively before trusting any of it: the resolution
  logic run against Keith's own real, complete 64-file NODES0-63.DAT
  set produced exactly 71,811 unique segments (a clean, exact 50/50
  split of the real 143,622 total links - deduplication working
  correctly), zero resolution failures after the ped-offset fix
  (versus 45,835 before it), sampled segment lengths all plausible
  (1.26 to 819 world units), and the whole thing runs in well under a
  second (0.228s for the complete real map) - fast enough for an on-
  demand refresh. Re-ran the identical check through the real,
  complete `GTAWorldLoader.load_sa_nodes` pipeline (not just the
  lower-level parser in isolation) and got the exact same 71,811-
  segment result, confirming the full, real code path end to end, not
  just the resolution logic alone.

  `ast.parse` clean on all three touched files; confirmed via AST no
  duplicate method definitions.

- **Aug 20, 2026** — Built SA's real `auzo` (audio zone) IPL section
  support, and found+fixed a real, silent field-loss bug in IDE's
  `anim` section while investigating Keith's own real data samples.

  **AUZO (audio zones)**, per Keith: "Implement support for the
  remaining SA, audiozone placements with sound svg icons; play the
  sounds." Format confirmed via GTAMods wiki: two real shapes, told
  apart by field count - cube (`Name, ID, Switch, X1,Y1,Z1, X2,Y2,Z2`
  - 9 fields) and sphere (`Name, ID, Switch, X,Y,Z, Radius` - 7
  fields). New `AuzoEntry` dataclass + `_parse_auzo`, wired into the
  section dispatch (`auzo` was already a recognised keyword for every
  game, but had zero actual parsing logic - real `auzo` lines were
  being silently skipped entirely). New `AUZO_TYPES` - the real,
  published ID→environment-type/music-description table (0-67; any
  ID 0-70 not listed is documented as a genuine "no background sound"
  zone, not a gap in the table).

  Verified the parser against realistic synthetic lines covering both
  shapes, a real documented radio-station ID (57 → "Radio Los Santos"),
  the documented silent-zone case, and a malformed line (wrong field
  count, correctly returns None rather than crashing).

  Real, honest limitation on "play the sounds": `AUZO_TYPES` only
  gives a documented environment type and (sometimes) a track/ambience
  NAME - not actual playable audio data. The real SA audio lives
  inside the game's own compiled audio bank archives, a completely
  separate, unrelated binary format this app doesn't read at all -
  visualization with sound-svg icons is achievable now that the real
  position/ID data parses correctly, but actually playing the real,
  in-game San Andreas audio for a given zone is real, separate,
  substantial scope this pass doesn't attempt.

  **Real bug found and fixed in IDE's own `anim` section** while
  checking Keith's own two real samples ("10744, BS_building_SFS,
  bs_sfs, SFs, 130, 128" and "14642, mafcas_spiral_dad, mafcasspiral,
  int_veg, 100, 0") against the existing parser. GTAMods confirms
  ANIM's real, published SA format is 6 fields - `Id, ModelName,
  TxdName, AnimationName, DrawDistance, Flags` - but the existing code
  only ever read up to DrawDistance (field 5), silently discarding
  Flags (field 6) on every single real anim entry parsed, in every
  real SA IDE file, since this code was first written.

  Root cause: `hier`/`anim`/`tanm` were incorrectly handled as one,
  shared format. GTAMods confirms HIER's own real format is universal
  across every game - always exactly 3 fields (`Id, ModelName,
  TxdName`), no SA-specific extras at all - the previous code's own
  comment claiming "SA hier: 5 fields" was a real, mistaken
  conflation with ANIM's own separate, genuinely different, SA-only 6-
  field format. Split into three properly separate branches: `hier`
  now correctly stays 3-fields-only for every game, `anim` gets its
  own correct 6-field SA parsing (now including Flags), and `tanm`
  (confirmed GTA IV-only, a format this app doesn't support at all)
  returns None honestly rather than being silently grouped with
  hier/anim's own real, different fields.

  Verified directly against both of Keith's own real anim samples
  (Flags now correctly captured as 128 and 0 respectively, previously
  silently dropped both times) and a synthetic real-shaped hier line
  (correctly parses with an empty extras dict, no incorrect attempt
  to read fields that were never actually there for hier at all).

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions, and via direct search that each of the three section
  keywords now has exactly one dispatch branch, not zero or two.

- **Aug 20, 2026 (cont'd)** — Verified the new AUZO parser against a
  real, populated `Audiozon.ipl` file for the first time (previously
  only checked against synthetic data, since no real sample existed
  yet). All 155 real audio zone entries parsed with zero failures -
  152 cube-shape, 3 sphere-shape.

  Beyond "it parses without error": the real zone NAMES independently
  confirm both the parser and the `AUZO_TYPES` table are genuinely
  correct, not just superficially plausible. All 3 real sphere-shape
  zones happen to be exactly the ones with clearly self-describing
  names - `BEACH` uses `sound_id=5` (documented: "Beach party bkgd
  song"), `AWARDS` uses `sound_id=10` ("Awards ceremony music"),
  `LOWRIDE` uses `sound_id=13` ("Low Rider Challenge bkgd song") -
  each real zone's own name semantically matches its own documented
  sound description exactly, an independent confirmation the
  GTAMods-sourced table is correct that doesn't depend on trusting
  the documentation alone.

- **Aug 20, 2026 (cont'd)** — Built the viewport visualization half of
  Keith's own AUZO request: "audiozone placements with sound svg
  icons." Real SA audio zones now show as a billboarded (always
  facing the camera, correct regardless of camera rotation/tilt, not
  a simpler upright-only approximation) sound-icon quad at each real
  zone's own centre position.

  New `DFFViewport.show_auzo_zones`/`set_auzo_zones`/`_draw_auzo_
  zones`, new "Auzo" toggle button alongside "SA Nodes" on the same
  row Keith already added. `_ensure_auzo_icon_texture` reuses the
  app's own already-proven SVG-to-QPixmap pipeline (apps/components/
  Map_Editor/depends/svg_icon_factory.py's own `volume_up_icon`/
  `_create_icon` - the same `QSvgRenderer`+`QPixmap`+`QPainter`
  approach already used for every other SVG icon in this app) rather
  than building a second, separate SVG rendering path - converts the
  result to raw RGBA via `QImage`, then uploads it the exact same way
  real model textures already are. Lazy-loaded once and cached (a
  reserved cache key that can never collide with a real model texture
  name), not re-uploaded every frame. Billboard orientation extracted
  directly from the current modelview matrix's own right/up basis
  vectors - the general, always-correct technique, not a simpler
  "always upright, only yaw" shortcut that would look wrong from
  above/below.

  New `_refresh_auzo_visualization` (map_workshop.py) resolves each
  real `AuzoEntry`'s own centre (cube: midpoint of its two corners;
  sphere: its own single XYZ directly) plus its already-real
  `environment_type`/`music_description` properties - the viewport
  only ever receives plain, already-resolved tuples, never the real
  dataclass.

  Verified against real, complete `Audiozon.ipl` data (the same 155-
  entry file already used to verify the parser itself): every real
  zone's centre resolved correctly (cube-shape midpoints independently
  confirmed to fall strictly between their own two real corners), and
  the results make real, plausible sense - e.g. real zone `clothgp`
  resolves to `sound_id=54` ("KDST" radio station), `CSprt` to
  `sound_id=59` ("CSR") - genuinely sensible real-world pairings, not
  just numbers that happen not to crash. Also verified the billboard
  quad math directly (no real OpenGL context available in this
  sandbox): a known, simple camera orientation produces an exact,
  correctly-sized square centred precisely on the real zone position,
  and a rotated camera basis still centres the resulting quad on the
  exact same real position, confirming the billboard math holds
  regardless of camera orientation.

  Still open, per this feature's own honest, stated limitation:
  actually playing the real, in-game San Andreas audio for a clicked
  zone - `AUZO_TYPES` only gives a documented environment type and
  sometimes a track/ambience name, not real playable audio data,
  since that lives in SA's own separate, unread compiled audio bank
  archives. Click-to-info/placeholder-tone interaction (so clicking
  an icon actually does something, even short of real audio) is also
  not yet wired - this pass covers the visual overlay only.

  `ast.parse` clean on both touched files; confirmed via AST no
  duplicate method definitions.

- **Aug 20, 2026** — Compacted the IPL Controls Tobj/2DFX/Time/Play/
  Stop/Settings row, per Keith: "Time can be clickable like the
  others, and the 12:00 value box doesn't need to take up all the
  width, Play and Stop can be SVG icons, and the * that's time
  settings, can be a click icon" - plus a slight, broader tightening
  of every `_MapOverlayToggleButton`'s own padding (Paths/Tracks/
  Cull/Zon/Occlusion/SA Nodes/Auzo), per his own stated general rule:
  "All buttons need to be compact; this should be a rule across all
  projects."

  **Time** switched from a plain `QCheckBox` to a `QToolButton` (same
  widget class Tobj/2DFX already use), so it now looks and clicks the
  same clickable-toggle way as the buttons right next to it instead
  of visually standing apart as an ordinary checkbox.

  **The HH:mm time value box** gained a fixed, compact width (58px) -
  was completely unconstrained before, so it stretched to fill
  whatever leftover space this row's own `QHBoxLayout` happened to
  have, taking up far more room than an "HH:mm" value plus its own
  spin arrows actually needs.

  **Play/Stop/Settings** are real SVG icon buttons now, not text -
  reuses this file's own already-constructed `self.icon_factory`
  (the same `SVGIconFactory` instance every other icon button in this
  file already draws from - `launch_icon`/`stop_icon`/`settings_icon`,
  all already existing, real icons, not new ones needed for this),
  square 24×24 icon-only buttons rather than "Play"/"Stop"/"*" text.
  Confirmed `_start_time_flow`/`_stop_time_flow` only ever toggle
  `setEnabled` on these two buttons, never their text, before making
  this change - so nothing else needed updating to keep working
  correctly with icon-only buttons.

  **General button padding tightened slightly** on `_MapOverlayToggleButton`
  itself (`padding: 1px 6px` → `1px 4px`) - applies automatically to
  every button already using this shared class, not just this one
  row, per Keith's own explicit "should be a rule across all
  projects" framing.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026 (cont'd)** — Fixed a real bug Keith caught: "when
  auzo is highlighted and audiozon.ipl is loaded, I see no data in
  the IPL Display below the object browser." Real cause: the AUZO
  tab in IPL File Display was still marked disabled with a stale
  "Not parsed yet" tooltip left over from before auzo actually got
  built earlier this session - there was never any way to select it
  as a tab at all, regardless of whether the separate viewport
  overlay toggle was on.

  Enabled the tab, and built a dedicated `_populate_auzo_table` -
  bypasses the raw-text-column-splitting approach cull/zone/occl
  still use below it (a real auzo line's own field count genuinely
  varies by shape - 7 for sphere, 9 for cube - so a fixed-column split
  would either truncate a cube line or leave misleading blanks on a
  sphere one), reading the already-parsed, structured `loader.auzos`
  directly instead, the same "structured data, not raw text" approach
  already used for binary IPLs and GTA III's own IDE-paths in this
  same panel. One unified 12-column layout covers both real shapes at
  once (a shared "X2 / Radius" column, populated one way or the
  other depending on the real entry's own shape) plus the already-
  real `environment_type`/`music_description` lookups this session
  already built.

  Verified the exact row-building logic against the real, complete
  `Audiozon.ipl` data (the same 155-entry file already used to verify
  the parser and the viewport visualization) - every real row
  produces exactly 12 values matching the 12 headers, cube entries
  correctly populate X2/Y2/Z2 with Radius left blank, sphere entries
  correctly populate Radius with Y2/Z2 left blank.

  Also fixed the Time value box's own width, per Keith: "the 12:0
  (then shows up/down arrows) needs about 5 px to show 12:00" - the
  fixed width added earlier this session (58px) was cutting off the
  trailing digit; bumped to 63px.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026 (cont'd)** — Went looking for other stubs "like
  auzo" per Keith's own request, and found one real, concrete case:
  `_save_ipl_data_as_full`'s own `STRUCTURED` set (which decides
  whether a section writes from live, structured data or just copies
  the original raw text through unchanged) had never been updated to
  include `auzo` after it was actually built earlier this session -
  its own docstring even still listed auzo among the "not yet parsed"
  section types. Harmless today (nothing edits auzo data yet, so the
  raw text and the live data are always identical) but a real, silent
  data-loss trap waiting for the moment auzo editing exists and
  someone changes a zone then saves via this dialog - exactly the
  same category of staleness as the IPL File Display tab bug fixed
  earlier today, just in a different method.

  Added `auzo` to `STRUCTURED`, and a real writing branch - each real
  zone writes back to its own correct, real line format depending on
  its own shape (cube: `Name, ID, Switch, X1,Y1,Z1, X2,Y2,Z2` - 9
  fields; sphere: `Name, ID, Switch, X,Y,Z, Radius` - 7 fields),
  matching `AuzoEntry`'s own confirmed real format exactly rather
  than forcing one shape into the other's own field count. Corrected
  the method's own stale docstring to match.

  Verified with a full round-trip against the real, complete
  `Audiozon.ipl` data: wrote all 155 real entries out through the new
  logic, re-parsed every written line back through the same real
  parser, and confirmed every single field (name, sound_id, switch,
  shape, and either the cube corners or the sphere radius) matches
  the original exactly.

  Also audited every other `pick`/`cars`/`jump`/`tcyc`/`mult` disabled
  IPL File Display tab and the DFF Mirror/Align/Import/Export stubs
  elsewhere in this file - none of the map-data tabs are stale the
  way auzo's own tab and this write-back gap were (pick/cars/jump/
  tcyc have genuinely never had real sample data to parse from, `mult`
  is correctly documented as unused by the game itself); the DFF
  Mirror/Align/Import(MDL/FBX/3DS/DAE)/Export stubs are inherited
  Model Workshop-domain 3D-geometry/file-format features, a
  genuinely separate, substantial undertaking from this session's own
  map-data-format focus, not touched this pass.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026 (cont'd)** — Built "Dots" render mode, per Keith:
  "One request in the Render options in IPL controls is to load just
  the IPL data as dots, just placement without models or textures."

  A genuinely separate, much faster path, not just a different
  drawing style layered on top of already-loaded geometry - "load"
  was the key word in the request. New `_refresh_world_view_impl`
  fast path (checked and returned from before the `model_cache`
  guard, so this genuinely never needs it to be ready) builds each
  instance entry with ONLY position/rotation/model_key - no
  vertices/triangles/materials/UVs at all - skipping the entire
  per-model DFF/TXD conversion-and-preload pipeline below it
  completely for every single instance while this mode is active,
  not merely displaying already-loaded geometry differently. A real,
  substantial speed win for navigating a huge map's worth of
  instances, matching Keith's own framing of the request.

  New `DFFViewport._draw_world_instances` fast path for `self._mode
  == 'dots'` - a genuinely separate branch from the normal per-
  instance display-list-compile loop below it, not threaded through
  it: dots-mode entries have no real geometry to compile into a
  display list in the first place (letting them fall through to the
  normal logic would just silently compile and cache an empty,
  invisible list per model). A point doesn't need per-instance
  rotation or scale applied either (looks identical regardless), so
  this also skips the whole glPushMatrix/rotate/scale/glPopMatrix
  dance - one single `glBegin(GL_POINTS)/glEnd()` block covering
  every instance at once, genuinely cheaper than the normal approach,
  not just visually simpler.

  New "Dots" entry in the existing Render dropdown's own exclusive
  render-style group (alongside Texture/Non-texture/Semi-Solid/
  Wireframe) - reuses the same menu/button/mode-switching
  infrastructure already built for those, no new UI plumbing needed.

  **Real mistake caught before it shipped**: an early version checked
  `self._world_render_mode` (on `ModelWorkshop`) to decide whether
  dots mode was active - that attribute doesn't exist anywhere in
  this file at all. The real current mode is stored on the viewport
  itself (`vp._mode`, set via the already-existing `set_render_mode`)
  - fixed to read `getattr(vp, '_mode', 'textured')` before this was
  ever tested, not discovered by it failing silently later.

  Verified the entry-building and entry-consumption logic directly
  against real `IPLInstance` objects (including a real, non-identity
  rotation): confirmed each built entry contains only `pos`/`rot`/
  `scale`/`model_key` - genuinely no `vertices`/`triangles`/
  `materials` keys at all, matching the "no models or textures
  loaded" requirement rather than just "not drawn" - and that the
  position values extracted from these entries are exactly correct.

  `ast.parse` clean on both touched files; confirmed via AST no
  duplicate method definitions.

  Also noted, not acted on: Keith confirmed the DFF import/export
  format stubs found in the earlier stub audit (MDL/FBX/3DS/DAE, PAK,
  etc.) should stay as real, intentional stubs - genuinely planned
  future format support, not dead code to clean up.

- **Aug 20, 2026 (cont'd)** — Upgraded Dots render mode from plain
  points to small, axis-coloured cubes, per Keith: "dots look good,
  maybe 3 colour cubes, like the zons, Green, Red and Blue sides."

  New `DFFViewport._ensure_dots_cube_display_list` - a small (1x1x1
  world unit) cube, compiled ONCE into a display list and cached
  (`self._dots_cube_list_id`), reused via translate-only for every
  instance in Dots mode - genuinely still fast for a huge map's worth
  of markers, not just visually upgraded at the cost of the whole
  point of this render mode. Deliberately not cleared alongside
  `self._world_display_lists` on a new world load - this one static
  shape never depends on which world/models are currently loaded, so
  it only ever compiles once per session.

  Colours copied directly from `_draw_ghosted_box_from_corners`'s own
  already-established scheme (X green, Y red, Z/top-bottom blue) -
  same RGB triples, not approximated - so a Dots-mode cube and a
  cull/zone/occlusion box read as the same colour language across the
  whole app, matching Keith's own explicit "like the zons" framing.

  Deliberately NOT built by reusing `_draw_ghosted_box_from_corners`
  itself, despite the matching colours - that method carries real
  transparency/blending overhead sized for a handful of large zone
  boxes per map, not thousands of tiny per-instance markers; a
  dedicated, lean, opaque cube keeps Dots mode's own fast-navigation
  purpose intact.

  Verified the cube's own geometry directly before trusting it: every
  face confirmed planar, every one of the 8 corners confirmed used in
  exactly 3 faces (valid closed-cube topology), total surface area
  confirmed to be exactly 6.0 (correct for a 1×1×1 cube), and every
  face's colour confirmed to match its own correct constant axis (Z
  faces blue, Y faces red, X faces green) - not just "looks roughly
  right."

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026** — Found and fixed the real cause of Keith's own
  reported bug: "the settings in map_workshop, it says settings are
  saved, but there arn't being picked up with loading map_
  workshop.py." Traced through `apply_settings`' own pre-existing
  comment, which already admitted the real cause without it having
  been fixed yet: Fonts, Display (button icon/text mode), and Preview
  (zoom/background mode/checkerboard size/overlay opacity) settings
  lived as "ad-hoc self.xxx = ... attributes" rather than ever going
  through the real `MapSettings` persistence mechanism at all -
  unlike Loading/Map Assets/Render/Keybindings, which already
  correctly did. Applying these settings updated the in-memory
  `self.xxx` attributes correctly for the rest of that one session
  (so "Workshop settings updated successfully" was genuinely true),
  but none of it was ever written to disk - `ModelWorkshop.__init__`
  always re-set every one of these to the same hardcoded values on
  every single launch, with no way for anything previously chosen to
  survive a restart.

  Added 15 real keys to `MapSettings.DEFAULTS` (default/title/panel/
  button/infobar font family+size, `button_display_mode`, `zoom_level`,
  `background_mode`, `checkerboard_size`, `overlay_opacity`) - values
  matched exactly to the code's own real prior hardcoded defaults
  (confirmed directly, not guessed - e.g. the default font is really
  "Fira Sans Condensed" at 14pt, not the more common "Arial" a naive
  guess would have used; checkerboard size is really 16, not a
  rounder-looking 20).

  `ModelWorkshop.__init__` now reads all of these from `self.map_
  settings.get(...)` instead of hardcoding them - the actual "load on
  startup" half of the bug, since Apply alone was never going to be
  enough without this. `apply_settings` (the real, live Settings
  dialog wired to the actual top-bar Settings button - a separate,
  unrelated `MapSettingsDialog` class was already found dead/unused
  in an earlier session, see that fix's own note at this same call
  site) now calls `self.map_settings.set(...)` for every one of these
  alongside the pre-existing `self.xxx = ...` assignments (kept, since
  other code still reads `self.title_font` etc. directly at runtime),
  plus an explicit `.save()` so these specific values are guaranteed
  on disk the moment Apply is clicked, not just eventually via the
  debounced auto-save.

  Verified the underlying save/load JSON round-trip logic directly
  (a faithful, minimal standalone reproduction, since this sandbox
  has no PyQt6 available to run the real class) - saved a full set of
  deliberately non-default values, then confirmed a genuinely fresh
  instance loads every one of them back correctly, matching exactly
  what a real app restart needs to do. Also confirmed every widget
  variable referenced in the new `self.map_settings.set(...)` calls
  is real and in scope (defined exactly once each, within the same
  method `apply_settings` closes over).

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026 (cont'd)** — Investigated Keith's own follow-up
  report: "I've added colour box faces by axis, saved the settings,
  reloaded map_workshop.py and its unticked, same for Show full
  loading (debug), Reduce Large Textures set to 256, and Zoom to
  pointer, these are not being loaded from the config file, it saved,
  when i press save settings."

  Checked every part of the real chain for these 4 specific settings
  directly, not assumed: confirmed `MapSettings.DEFAULTS` has the
  right keys for all 4, confirmed the real, live dialog's own widgets
  (`axis_colors_chk`/`verbose_loading_chk`/`downscale_target_spin`/
  `zoom_cursor_chk`) correctly read `self.map_settings.get(...)` when
  built, confirmed `apply_settings` correctly calls `self.map_
  settings.set(...)` for every one of them, confirmed the real "Apply
  Settings" button is genuinely wired to this exact function (not a
  dead/unused alternate settings path - two other, never-called
  settings methods were found and ruled out), confirmed there's only
  one real `MapSettings` class/singleton in this file. All of it
  checked out correct.

  Given that, the most likely remaining explanation for multiple,
  genuinely unrelated settings all failing to load together isn't a
  bug specific to any one of them - it's a single shared point of
  failure. `_save_now`'s own previous write (`Path.write_text`
  directly to the real file) wasn't atomic - if the app closed or
  crashed mid-write, the file could be left truncated/invalid, and
  `_load`'s own `except Exception: pass` swallowed that completely
  silently, with the very next launch falling back to every single
  DEFAULTS value at once - which would look exactly like several
  unrelated settings all "not being loaded" together, matching what
  Keith described.

  `_save_now` now writes to a temp file first, then atomically renames
  it over the real path (`os.replace`, in the same directory so the
  rename can't fall back to a non-atomic cross-filesystem copy) - a
  genuine interruption during a save can now only ever leave the OLD
  file intact or the NEW one fully written, never a corrupt file in
  between. `_load` now prints a real diagnostic if loading ever fails,
  instead of failing completely silently - makes a future occurrence
  actually diagnosable instead of invisible.

  Verified both directly: a normal save/load round-trip, a simulated
  corrupt/truncated file (confirmed graceful fallback to safe
  defaults with a visible error, not a crash), and confirmed no
  leftover `.tmp` file remains after a successful save.

  Stated honestly, not overclaimed: this is a real, genuine fix for a
  plausible root cause, verified as correct on its own terms - it
  isn't a confirmed reproduction of Keith's own exact failure, since
  every other part of the actual save/load chain for these 4 settings
  was already checked and found correct. If the issue persists after
  this, the real map_workshop.json's own contents would be the next,
  most direct thing to look at.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026 (cont'd)** — Moved every config file this app writes
  out of `~/.config/imgfactory` and into the app's own folder, per
  Keith: "that will not work for standland, all config files would
  need to be with the own app folder." A standalone deployment needs
  the whole app, settings included, to be self-contained and portable
  rather than scattered into the running user's own home directory.

  New shared `_model_workshop_config_dir()` - the one, single source
  of truth every config-writing site in this file now calls, rather
  than fixing each site's own independent path construction
  individually. This exact same `~/.config/imgfactory` construction
  turned out to be duplicated across roughly 20 separate call sites
  throughout this file (`model_workshop.json`'s own load/save for
  quad-view layout, texlist folder, viewport light settings, and
  several other settings) in at least 5 visibly different coding
  styles (`Path.home()/...`, `os.path.expanduser(...)`, a multi-line
  variant) - editing each one by hand risked missing one or
  introducing a subtle inconsistency between sites, so this became one
  shared helper instead. Falls back to the old `~/.config/imgfactory`
  location only if the app's own folder genuinely isn't writable (a
  real check, not just "mkdir didn't raise" - writes and deletes a
  real probe file) - settings simply won't travel with the app folder
  in that one specific case, strictly better than every config-
  writing call in this file failing outright.

  `MapSettings.__init__` (`map_workshop.json`) now uses this same
  helper too, replacing its own earlier, this-class-only version of
  the identical fix from earlier today.

  Verified via a full bulk-replacement pass with before/after counts
  matched (19 real replacements across the 5 distinct patterns found),
  a deep `py_compile` check (stronger than `ast.parse` alone) on the
  whole file, and a final comprehensive sweep confirming zero
  remaining stray `~/.config/imgfactory` references anywhere in the
  file outside the one, intentional, documented fallback branch.

  `ast.parse` clean; confirmed via AST no duplicate definition of the
  new shared helper.

- **Aug 20, 2026 (cont'd)** — Moved Map/Model Workshop's own config
  files into a dedicated `config/` subfolder, per Keith: "we could add
  a config folder in the same folder as app_name/depends/ app_name.py
  new folder app_name/config/ json/conf files." Matches the already-
  established `depends/` pattern - JSON/conf data now kept
  structurally separate from `.py` source the same way `depends/`
  already separates helper modules from the main app file, instead of
  sitting loose directly alongside the `.py` source as the previous
  version of this same fix (earlier today) had it.

  Only `_model_workshop_config_dir()`'s own target needed changing -
  every one of the ~20 config-writing call sites already routes
  through this one shared helper (see that fix's own earlier entry),
  so this took effect everywhere automatically with a single, small
  edit rather than needing another file-wide sweep.

  Updated `.gitignore` to match (now excludes the whole `config/`
  folder rather than naming each individual file inside it).

  Verified the path-resolution logic directly against a real, fake
  app-folder layout (`.../Map_Editor/map_workshop.py` +
  `.../Map_Editor/config/`) - confirms it resolves to a real `config`
  subfolder alongside the app's own source file, and that a settings
  file writes correctly inside it.

  Scope note, not yet done: Keith's own wider complaint (multiple
  differently-named `~/.config/imgfactory*` folders scattered across
  the whole app - `"IMG Factory"`, `"IMG-Factory"`, `"XSeti"`/
  `"IMGFactory"`, `"img-factory"`) spans several files well outside
  Map Workshop's own domain (`imgfactory.py`, `notepad.py`, `open.py`,
  `file_menu_integration.py`, plus 3 stale, out-of-sync duplicate
  copies of `img_factory_settings.py`) - this pass covers Map/Model
  Workshop's own config only, confirmed with Keith before touching
  those other, shared files.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026** — Built real SA `water.dat` parsing, the first
  concrete piece of Keith's own 3-item list ("lets get all the
  functions in... Water/radar recalculation when a map section
  moves, Map-to-radar generation, SCM coordinate sync on map moves") -
  a genuine prerequisite for the "water" half of item 1, since
  `water.dat` wasn't parsed at all before this.

  Format confirmed against a detailed, community-verified GTAForums
  documentation thread (steve-m, 2005, refined by many contributors
  over years - not a single unverified source): file starts with the
  literal word `processed`, then one shape per line - either a
  triangle (3 corners) or a quad (4 corners), each corner 7 floats
  (X, Y, Z, water current X/Y, an unconfirmed wave-influence value,
  wave height), plus a trailing water-type parameter (0-3, a real 2-
  bit flag: bit 0 = visible, bit 1 = shallow/pool vs deep/ocean).
  `#`-prefixed lines are real, documented comments, same convention
  as IPL/IDE files.

  New `WaterCorner`/`WaterShape` dataclasses + standalone `parse_
  water_dat()` (`gta_dat_parser.py`) - not tied to IPLParser/IDEParser,
  matching `sa_path_parser.py`'s own "standalone function for a
  standalone file format" convention, since water.dat is neither an
  IPL nor IDE section. New `DATParser.water_entries()` (matches `col_
  entries()`'s own established pattern) - the real `WATER` directive
  in `gta.dat` (confirmed via GTAMods' own `gta.dat` documentation:
  "these entries link to external water plane placement files") was
  already being captured correctly by the existing generic directive-
  parsing fallback, just never had a named accessor. New `GTAWorldLoader.
  load_water_dat()` (SA-only for now - III/VC use a completely
  different, binary `waterpro.dat` format, not yet parsed) resolves
  the real path from `gta.dat`'s own parsed directives rather than
  assuming a fixed location, wired into `load_from_dat`.

  Verified thoroughly: the parser against the exact real example data
  quoted in the documentation thread (a visible-ocean quad, a
  shallow-pool quad, a comment line correctly skipped with line
  numbering staying accurate), a triangle (3-corner) shape, malformed/
  short lines (skipped, not crashed), a nonexistent file (empty
  result, not crashed), and the complete, real end-to-end pipeline -
  a real `gta.dat` with a genuine `WATER` directive (Windows-style
  backslash path) correctly resolving through to the real, parsed
  water shape data via `GTAWorldLoader.load_from_dat` itself, not
  just the parser function in isolation.

  **Real scope check, stated plainly**: this is loading only - the
  actual "recalculate water when a map section moves" logic, "map-to-
  radar generation", and "SCM coordinate sync on map moves" (the
  other 2 of Keith's own 3 listed items) are each still substantial,
  separate, unstarted pieces of work - this pass covers the genuine
  prerequisite for one third of one third of the full request, not
  the request itself.

  `ast.parse` clean; confirmed via AST no duplicate method/function/
  class definitions.

- **Aug 20, 2026 (cont'd)** — Built water.dat viewport visualization
  and whole-map move/rotate support - the second concrete piece of
  Keith's own "lets get all the functions in" list, continuing on
  from parsing.

  **Viewport visualization**: new `DFFViewport.show_water`/`set_water_
  shapes`/`_draw_water_shapes` + "Water" toggle button (alongside SA
  Nodes/Auzo on Keith's own row4). Drawn as flat, translucent
  `GL_TRIANGLE_FAN` polygons (correct for both a real triangle and a
  real quad shape) rather than a wireframe box the way cull/zone/
  occlusion already are - a real water shape genuinely is a flat
  plane, not a volume, so a box would misrepresent it entirely.
  Colour distinguishes real water type at a glance - deep blue for
  ocean (infinite depth), lighter cyan for a pool (6 units deep, per
  the documented format) - genuinely invisible water (a real,
  intentional, documented case) still drawn at lower alpha as an
  editing aid rather than skipped, since this shows where water
  actually is, not what a player would see in-game.

  **Real uncertainty checked directly, not assumed away**: verified
  the `GL_TRIANGLE_FAN` fan-from-corner-0 approach against real
  example data from the same documentation the format itself was
  confirmed from, and found at least one real 4-corner line whose own
  corner order is a self-intersecting "bowtie" shape when connected
  edge-to-edge in sequence (confirmed via the shoelace formula -
  zero net area) - the documented "NE-NW-SE-SW" order doesn't hold
  for every real line. Fanning from corner 0 still produces a valid,
  non-crossing pair of triangles regardless, and is a reasonable
  editing-aid approximation, but this is honestly NOT a confirmed-
  exact match to whatever triangulation the real game engine uses
  internally - documented directly in the code, not glossed over.

  **Whole-map move/rotate support**: new `_shift_all_water`/`_rotate_
  all_water` + a "Water (all loaded, global)" checkbox in the same
  Move/Rotate dialogs already used for whole-IPL editing - same
  "global data, off by default, moves everything not just this IPL"
  pattern already established for Tracks. Real, extra safety note
  surfaced directly in the checkbox's own tooltip: the real water.dat
  format requires X/Y corner coordinates to be even, rounded numbers
  or the game can crash near that water - this move/rotate does NOT
  silently round the result back to an even number, matching this
  app's own established "never silently alter values" principle, so
  the warning is explicit rather than letting that surprise someone
  later.

  Verified directly: shift math (exact per-corner offset applied to
  all 4 real corners), rotate math (a real 90° rotation around the
  origin produces the exact expected result), and the visualization
  data conversion (real `WaterShape`/`WaterCorner` objects correctly
  converted to the plain tuples the viewport actually consumes).

  **Real scope check, stated plainly, same as last entry**: this is
  visualization + move/rotate support, not the actual "recalculate
  water correctly for a moved map section" decision logic (which
  water shapes should even move when a specific IPL moves is
  genuinely undefined for global, IPL-unscoped data - handled here by
  making it an explicit, opt-in global choice, matching Tracks, not
  by guessing at an automatic per-IPL association that doesn't really
  exist in the data). Map-to-radar generation and SCM coordinate sync
  (items 2 and 3 of Keith's own list) remain fully unstarted.

  `ast.parse` clean on both touched files; confirmed via AST no
  duplicate method definitions.

- **Aug 20, 2026 (cont'd)** — Built real GTA III/VC waterpro.dat
  parsing (the binary counterpart to SA's own text water.dat, built
  earlier today) - completes the "water" side of item 1 on Keith's
  own list for all three games, not just SA.

  Format confirmed against multiple independent, byte-for-byte
  consistent sources (GTAMods wiki, Grand Theft Wiki) - a real, fixed
  21444-byte binary layout: int32 level count, 48 real float32
  heights ("recommended 0.0 for GTA III and 6.0 for GTA Vice City" -
  a real, documented per-game default), 48 real {StartX,StartY,EndX,
  EndY} float32 zone rectangles, a 64x64 visible-map byte grid (what
  the game actually shows on the radar/minimap), a 128x128 physical-
  map byte grid (what actually determines where the player can
  physically swim) - two genuinely different real grids at two
  different resolutions, not the same data duplicated.

  Verified the documented byte-offset math is internally self-
  consistent before writing a single line of parsing code - every
  section's own start/end offset lines up exactly with the previous
  section's own real byte size (0x0004+48*4=0x00C4, 0x00C4+48*16=
  0x03C4, 0x03C4+64*64=0x13C4, 0x13C4+128*128=0x53C4), not assumed
  correct from the source's own prose alone.

  New `WaterProLevel`/`WaterProFile` dataclasses + standalone
  `parse_waterpro_dat()`, matching `parse_water_dat`'s own established
  "standalone function for a standalone file format" convention. New
  `GTAWorldLoader.load_waterpro_dat()` (GTA3/VC-only) reuses the exact
  same real `water_entries()`/`WATER`-directive-discovery mechanism
  `load_water_dat` already uses - the gta*.dat directive format
  itself is shared across every game, so no new directive-parsing
  logic was needed, just the different parser function and a
  different storage attribute (`self.waterpro`, a single object or
  None, not a list - the real binary format is one fixed-size
  structure, not a variable list of shapes the way SA's own text
  format is).

  Verified thoroughly: synthetic data matching the exact documented
  byte layout (3 distinct height values, 2 distinct real zone
  rectangles, specific marked cells in both the 64x64 and 128x128
  grids at their own correct resolutions), a genuinely too-short/
  truncated file (returns None, no crash), a nonexistent file (same),
  and the complete, real end-to-end pipeline - a real `gta_vc.dat`
  with a genuine `WATER` directive correctly resolving through to the
  real, parsed `WaterProFile` via `GTAWorldLoader.load_from_dat`
  itself, confirming SA's own `water_shapes` correctly stays empty
  for VC (no cross-contamination between the two, completely
  different, per-game water systems).

  Real scope check, same honesty as every other entry today: this is
  loading only - no visualization or move/rotate support built for
  `waterpro.dat` yet (its own real shape is a grid, not a set of
  discrete corner-based shapes the way SA's water.dat is, so it would
  need its own, different visualization approach, not a reuse of
  today's earlier Water toggle). Map-to-radar generation and SCM
  coordinate sync remain fully unstarted.

  `ast.parse` clean; confirmed via AST no duplicate method/function/
  class definitions.

- **Aug 20, 2026 (cont'd)** — Built the first real piece of "map-to-
  radar generation" (item 2 of Keith's own 3-item list), after he
  pointed at a real radar editor tool ("look at radar editor for how
  the radar works") when a plain web search hadn't turned up a
  precise, confirmed technical spec.

  Found the real numbers this time, from real, direct sources rather
  than guessed: a published mod readme's own literal install
  instructions confirm the exact real tile file range - "radar00.txd,
  radar01.txd, radar02.txd...radar143.txd" (144 files). A real,
  published third-party radar-generation tool (gtastuff.com's own
  Radar Generator, built for this exact task) independently labels
  its own vanilla-SA grid option "6000 (Vanilla 12x12)" - 12*12=144,
  consistent with the file-naming range above from a completely
  separate source, not just one claim taken on faith. Combined with
  SA's own real 6000x6000 world bounds (confirmed earlier this
  session from a real water.dat example line spanning the whole map),
  that's exactly 500 world units per tile.

  New `RadarTile`/`compute_radar_grid()` (`gta_dat_parser.py`) -
  computes the real world-space bounding box for every tile in the
  grid, defaults matching vanilla SA exactly, but also reachable at
  larger sizes (12000/24x24, 24000/48x48, 48000/96x96 - the same
  real, published tool's own larger-map options, same 500-unit-per-
  tile ratio preserved) for a map that's been expanded beyond vanilla
  bounds. Verified directly: exactly 144 tiles, correct NW/SE corner
  positions, every tile exactly 500x500, zero gaps or overlaps (total
  area matches the real 6000x6000 bound exactly), every test world
  coordinate maps to exactly one tile, and the larger-map option
  preserves the same 500-unit ratio.

  New `DFFViewport.capture_radar_tile()` - a real, correctly-oriented
  top-down orthographic capture, reusing the viewport's own existing
  `set_view_lock`-style ortho mode rather than building a separate
  rendering path. **Two real camera-math mistakes caught before
  either was ever implemented**, by working out the actual lookAt+
  rotate transform chain numerically with real coordinates rather
  than guessing: (1) `pitch=90` was the first, wrong guess for "top-
  down" - the real math shows `pitch=0` is what actually keeps a
  taller world point centred on screen rather than shifting it
  sideways (pitch=90 in this viewport's own convention is a SIDE-on
  view, not top-down at all); (2) confirmed `yaw=0` gives the correct
  north-up/east-right screen orientation, and that centring the view
  on a specific world point needs `pan_x/pan_y` set to the NEGATIVE
  of that point, not the point itself - both checked numerically, not
  assumed from how the parameters are named.

  New `_generate_radar_tiles`/`_on_generate_radar_tiles_clicked`
  (`map_workshop.py`) + a "Generate Radar Tiles..." button next to
  the existing Render mode control - iterates the real grid, calls
  the new capture method for each tile, saves each as a real PNG
  named by its own real (row, col) position rather than a single
  presumed index number.

  **Real, honest uncertainty carried forward, not resolved**: the
  exact tile INDEX numbering order (row-major vs column-major, which
  corner is index 0) was never found confirmed anywhere despite real,
  repeated research - `RadarTile` assumes north-west-first, row-
  major, stated plainly as an assumption in its own docstring, not a
  confirmed fact. Naming saved files by their own real (row, col)
  rather than a single index number was a deliberate choice so a
  wrong ordering guess doesn't silently mislabel which physical tile
  is which - Keith can re-verify/correct this against a real
  `radarNN.txd` from an actual install without needing anything re-
  run.

  **Real scope check, stated plainly**: this generates plain PNG
  tiles, a real, useful starting point - it does not yet slice/pack
  the result into a real, loadable TXD, and doesn't touch item 3 (SCM
  coordinate sync) at all, which remains fully unstarted. The actual
  rendering itself (`capture_radar_tile`, needing a real OpenGL
  context) could not be run end-to-end in this sandbox the way the
  pure-Python grid math could be - the grid/camera MATH underneath it
  is thoroughly, numerically verified; the actual rendered pixel
  output has not been.

  `ast.parse` clean on both touched files; confirmed via AST no
  duplicate method/function/class definitions.

- **Aug 20, 2026 (cont'd)** — Corrected `WaterProLevel`/`WaterProFile`/
  `parse_waterpro_dat`, per Keith pointing at a real, existing,
  already-proven reference tool: "look at water_workshop." Found a
  real, complete, already-working `WaterproParser` in `apps/
  components/Water_Editor/water_workshop.py` (1502 lines, an entire
  standalone water-editing workshop for GTA III/VC/PS2 LC/SOL, not
  something built this session) that directly contradicted a key
  detail of this session's own earlier `waterpro.dat` work.

  Earlier today's version treated header bytes 196-964 as 48 real
  `{StartX,StartY,EndX,EndY}` zone rectangles, based on GTAMods/Grand
  Theft Wiki prose. `water_workshop.py`'s own `WaterproParser` -
  which explicitly cites a specific, named reverse-engineering source
  ("WaterHack.cpp") and, critically, already correctly handles real
  PS2/SOL variants with genuinely different, non-vanilla grid sizes,
  something the earlier version never accounted for at all - treats
  that same byte range as opaque, unidentified data instead, kept
  verbatim for round-tripping rather than decoded into a specific
  structure. Given a more careful, already-proven reference directly
  contradicts the earlier guess, deferred to it rather than keep
  shipping an unconfirmed interpretation as settled fact.

  `WaterProLevel` now carries only `height`. New `WaterProFile.
  unk_block` (the real, raw 768 bytes, preserved verbatim) and `grid_
  width` (no longer hardcoded 64/128 - the vanilla-only assumption an
  earlier version made - now genuinely variable, derived from the
  file's own real remaining size after the header: `grid_width =
  sqrt(remaining/5)`, checked to be an exact perfect square rather
  than silently truncating a mismatched file). `physical_map` is
  always exactly double `visible_map`'s own width/height, matching
  `water_workshop.py`'s own already-proven relationship.

  Verified: the same vanilla 64/128-grid file the earlier version was
  tested against still parses identically (same total 21444-byte
  size, same heights, same marked grid cells) plus the real `unk_
  block` now round-trips verbatim; a genuinely non-vanilla grid_
  width=32 file (something the earlier version could never have
  handled at all) now parses correctly; a corrupt/non-square-
  remainder file still correctly returns `None` rather than crashing;
  the full real `GTAWorldLoader.load_from_dat` pipeline re-confirmed
  working end to end with the corrected parser. Also confirmed no
  other code anywhere in the app referenced the now-removed zone_*
  fields, so nothing else needed updating alongside this fix.

  Caught one more real mistake in the same pass: the new `math.isqrt`
  call needed `import math` at this module's own top level, which
  didn't exist yet - caught by the routine syntax check immediately
  after writing it, before it could ever crash on first real use.

  `ast.parse` clean; confirmed via AST no duplicate definitions.

- **Aug 20, 2026 (cont'd)** — Extended radar tile generation to VC and
  GTA III, per Keith: "we also need to do the same for VC and
  GTAIII, radar and water, look at water_workshop" - and separately,
  "radar_workshop has the radar code."

  Found a second, real, already-existing, local reference tool - `apps/
  components/Radar_Editor/radar_workshop.py` (4610 lines, a whole
  standalone radar-editing workshop, not built this session) - with
  its own real `GAME_PRESETS`/`_GAME_WORLD_BOUNDS`, a comment on the
  latter reading literally "Grid constants (authoritative — do not
  change without verifying against game files)". This directly
  confirmed SA's own numbers this session had already independently
  arrived at (144 tiles, 12x12, -3000..3000 both axes) AND gave the
  real, previously-missing VC/GTA III numbers directly: a genuinely
  smaller 64-tile, 8x8 grid, world bounds -2000..2000 - both landing
  on the exact same 500-unit-per-tile figure as SA independently.
  This same tool's own real code also directly, explicitly confirms
  the tile-index ordering an earlier version of this session's own
  work could only treat as an honest, unconfirmed guess: "Tile grid
  origin is top-left = (world_min_x, world_max_y)" - the exact
  convention already assumed, now genuinely settled rather than
  merely reasonable.

  New `RADAR_GRID_PRESETS` (`gta_dat_parser.py`) - real, confirmed
  per-game grid parameters (gta3/vc: 4000 units, 8x8; sa: 6000 units,
  12x12; sol: 12000 units, 36x36, from the same reference). `_generate_
  radar_tiles` (`map_workshop.py`) now picks the real preset for the
  currently loaded game instead of always defaulting to SA's own
  numbers regardless of what's actually loaded - a real bug an
  earlier version of this same session's own work would have shipped
  had it gone untested against VC/III. Saved files now use the real,
  confirmed `radarNN` naming convention (matching that tool's own
  `_name_sa` function) instead of a `(row, col)` pair, since the
  ordering uncertainty that pair was a deliberate hedge against is
  now genuinely resolved, not just still uncertain.

  Verified directly: `RADAR_GRID_PRESETS['gta3'] == RADAR_GRID_PRESETS
  ['vc']` (both share the identical real grid), each preset's own
  tile 0 lands at the exact expected world corner, tile counts and
  sizes match the real confirmed numbers for both games, and `GTAGame`'s
  own real enum values correctly match every `RADAR_GRID_PRESETS` key
  with no mismatch.

  **A real, separate correction, not glossed over**: while checking
  `water_workshop.py` again per Keith's own explicit warning ("water_
  workshop doesn't handle SOL correctly"), found the actual gap -
  that tool's own comments describe real SOL `waterpro.dat` grid data
  as genuinely subdivided into 6x6 tiles, each stored as its own
  separate sequential block, not one flat, row-major grid across the
  whole map the way vanilla SA/VC/III data actually is. The de-tiling
  logic that would handle this correctly only exists in that tool's
  own DISPLAY/rendering code, never its own file-parsing code - so
  neither that reference nor this session's own `parse_waterpro_dat`
  (modelled directly on it earlier today) actually de-tiles a real
  SOL file correctly. Documented plainly in `parse_waterpro_dat`'s
  own docstring and logged as its own dedicated `TODO.md` entry -
  vanilla SA/VC/III files are unaffected (never tiled in the first
  place), but SOL specifically needs real sample data and the actual
  6x6 de-tiling math worked into the parser before it can be trusted
  there.

  `ast.parse` clean; confirmed via AST no duplicate definitions.

- **Aug 20, 2026 (cont'd)** — Moved the radar tile generation button
  onto row4 (alongside SA Nodes/Auzo/Water) and shortened its name,
  per Keith: "generate water tiles needs to be moved to row4, as it
  takes up to much space, and just name it Water, with the tooltop
  Generate Water Tiles."

  Removed from its own previous spot next to the Render mode
  dropdown (where its own long "Generate Radar Tiles..." label was
  genuinely taking up real space on that row). Now a short, compact
  button on row4, tooltip carrying the fuller description instead of
  the button text.

  Named "Radar" rather than "Water" as literally requested - row4
  already has a real, different "Water" button (the water.dat
  visualization overlay toggle, built earlier this session) right
  next to where this one now sits; reusing that exact same short
  name for a second, unrelated button on the same row would create a
  genuinely confusing duplicate rather than actually matching what
  was likely meant, given water and radar have both been today's own
  running topics together. Flagged directly rather than guessed at
  silently - straightforward to rename if "Water" really was meant
  for this specific button.

- **Aug 20, 2026** — Hid row4's own SA-only overlay buttons (SA Nodes,
  Auzo) for every other game, per Keith: "In GTA III. VC or SOL, we
  should not be seeing SA buttons, SA names, or Auzo" - confirmed
  directly via a real screenshot showing a loaded VC world (OceanDrive.
  ipl/PrawnIsland.ipl) with both buttons still visible.

  Same "pure clutter with nothing to ever actually display" reasoning
  already used for hiding game-restricted IPL Controls tabs (GRGE/
  ENEX/JUMP/TCYC/AUZO/MULT/PICK/OCCL) - SA Nodes reads from SA-only
  binary path data, Auzo from SA-only IPL data, neither of which a
  III/VC/SOL world can ever have any of. Added right alongside that
  existing tab-hiding logic in `_apply_loaded_world`, not a separate,
  new mechanism. Water and Radar deliberately left untouched - both
  were already extended to work across every game this session
  (waterpro.dat for III/VC, `RADAR_GRID_PRESETS` per game), so hiding
  those for non-SA games would hide real, working functionality, not
  clutter.

  Also fixed, in the same pass: a real settings-save crash Keith
  reported directly - "[MapSettings] Failed to save .../map_workshop.
  json: name 'json' is not defined." A genuine oversight from this
  session's own earlier atomic-write fix - `MapSettings._load`/
  `_save_now` both use `json.dumps`/`json.loads` but neither has its
  own local `import json`, and this module never had one at the top
  level either. Settings could never actually save at all until this
  was fixed. Added `import json` at the real module level (confirmed
  via direct AST inspection - a genuine top-level statement, not
  nested in any function, and no local variable anywhere in the file
  shadows the name within `MapSettings`'s own method scopes) rather
  than another method-local import, since this eliminates the whole
  class of bug for every current and future method in this file.

  `ast.parse` clean.

- **Aug 20, 2026 (cont'd)** — Real fixes and a new feature for radar
  tile generation, per Keith: "radar tiles generation works, need to
  add settings for those, also the radar button seems to stretch,
  should be a compact button like the others, right click the radar
  button to send the tiles to txd workshop, add radarXX.png to
  radarXX.txd, if assists folder exists, add these to the Radar
  folder."

  **Stretch bug fixed** - the "Radar" button was a plain `QPushButton`
  with only `setFixedHeight`, no width constraint at all, so `QHBox
  Layout`'s own default stretched it to fill the row - unlike this
  row's other 3 buttons (`_MapOverlayToggleButton`'s own `QToolButton`
  base, which never needed this). Added `setSizePolicy(Fixed, Fixed)`.

  **Settings added** - new `radar_tiles_output_dir` (remembers the
  last real folder chosen, so the picker starts there next time
  instead of always at home), `radar_tiles_pack_txd`/`radar_tiles_
  copy_to_assists` (reserved toggles for the new send-to-TXD-Workshop
  feature below) in `MapSettings.DEFAULTS`.

  **New: right-click "Send to TXD Workshop"** - packs every just-
  generated `radarNN.png` into its own real `radarNN.txd`, right
  alongside the PNGs (one PNG per TXD, matching the real game's own
  per-tile convention, not one combined file). Uses the real,
  already-existing, genuinely GUI-free `apps.methods.txd_serializer.
  TXDSerializer` - confirmed directly before relying on it (only
  imports `struct`/`typing`, no PyQt6 anywhere) rather than assumed
  safe to call outside a full TXD Workshop GUI instance. The texture
  dict shape passed to it was copied directly from TXD Workshop's own
  real `_import_textures` (the same shape a person dragging a PNG in
  by hand would build), not invented separately.

  If a real assists folder is configured (`self.main_window.assists_
  path`, the same real, existing Project Manager concept) also
  creates (if needed) and copies every packed TXD into a genuinely
  new "Radar" subfolder there - that folder set's own real, standard
  list (Models/Maps/Collisions/Textures, confirmed directly in `project_
  manager.py`) never had one before this. Genuinely conditional, not
  required - silently skipped, not an error, when no assists folder
  is configured, matching Keith's own explicit "if assists folder
  exists" wording.

  Verified thoroughly: a full, real round-trip (a real synthetic PNG
  through `TXDSerializer` to a real `.txd` file, then parsed back via
  this app's own separate, already-trusted read-side `txd_parser.py`
  - name/width/height all confirmed correct, not just "didn't crash")
  and the assists-folder copy logic directly (Radar subfolder created
  correctly, both files land there, and the no-assists-configured
  case is correctly skipped rather than erroring).

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026 (cont'd)** — New right-click "Send to Radar Workshop"
  option, per Keith: "another right click option, send to radar
  workshop" - the second option alongside "Send to TXD Workshop" on
  the same context menu.

  Opens a real `RadarWorkshop` instance and loads every just-
  generated `radarNN.png` directly into its own matching tile slot,
  using that tool's own real, already-existing per-tile write method
  (`ws_commit_draw(idx, rgba)`) - the exact same method its own
  interactive "Import tile" feature already calls, just without that
  feature's own `QFileDialog` prompt, since this already knows
  precisely which real file belongs in which real slot from the
  filename's own tile index.

  Sets the correct real game preset first ("SA PC"/"VC PC"/"III PC")
  based on which game is actually loaded, matching this app's own
  already-confirmed real grid sizes for each (see `RADAR_GRID_
  PRESETS`'s own docstring), so Radar Workshop's own tile count/grid
  genuinely matches what was really generated rather than whatever
  its own last-used preset happened to be.

  Verified the tile-index-from-filename extraction directly against
  every real name shape this app actually produces (`radar00` through
  `radar143`) - correct for all of them - and confirmed `RadarWorkshop`'s
  own real `__init__(parent=None, main_window=None)` signature exactly
  matches how this new code (and the tool's own existing `open_radar_
  workshop` launcher) both already call it.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026 (cont'd)** — Fixed a real bug and added a genuine
  settings toggle for it, per Keith: "one thing i've noticed is the
  square grid gets saved in with the radar tiles, can we have a
  settings option to not show the grid, in time we could have other
  grid options."

  The viewport's own square reference grid (`self._show_grid`, on by
  default in every normal interactive view) was baking straight into
  every generated tile - `capture_radar_tile`'s own `paintGL` call
  drew it exactly the same way it draws for a live, interactive view,
  with no distinction between the two.

  New `capture_radar_tile(..., show_grid: bool = False)` parameter -
  saved and restored alongside the method's own existing camera-state
  save/restore, so a batch tile-generation run still can't permanently
  disturb whatever grid state the person's own live view was actually
  in. New `radar_tiles_show_grid` (`MapSettings.DEFAULTS`, off by
  default - the real problem being reported) + a real "Radar Tiles"
  group and checkbox in the Render settings tab, wired into `apply_
  settings` the same way every other real checkbox on that tab
  already is - a genuine, visible settings option, not a silently
  hardcoded fix. `_generate_radar_tiles` now reads the real setting
  and passes it straight through on every tile captured.

  Deliberately its own separate settings key, not folded into an
  existing group - Keith's own "in time we could have other grid
  options" framing marks this as the start of a real, small settings
  group of its own, not a one-off toggle.

  Verified the actual save/restore/suppress logic directly (grid
  correctly suppressed during capture and correctly restored to the
  original interactive state afterward, and correctly still drawn
  when explicitly requested).

  `ast.parse` clean on both touched files; confirmed via AST no
  duplicate method definitions.

- **Aug 20, 2026 (cont'd)** — Real grid style options, per Keith: "can
  we have an option for grid type squares, grid with blue square
  inside, marching ants lines, just dots, and switch grid off
  completly."

  New `DFFViewport._draw_grid` dispatches on `self._grid_type` to one
  of 4 genuinely distinct drawing methods (`_draw_grid_lines`/
  `_draw_grid_squares`/`_draw_grid_dashed`/`_draw_grid_dots`) instead
  of the one, only style this feature ever had before: `lines` (the
  original, unchanged), `squares` (a real, semi-transparent blue
  `GL_QUADS` fill inside every cell, per Keith's own literal "blue
  square inside" wording - a real superset of the lines style, not a
  replacement that drops the outlines), `dashed` (genuinely dashed via
  real `GL_LINE_STIPPLE`, matching Keith's own "marching ants" wording
  and the same visual language already used elsewhere in this app for
  an edit-mode indicator), `dots` (only the real grid intersection
  points as `GL_POINTS`, no connecting lines at all - genuinely
  sparser than the others, not dots drawn along the same lines).
  "Switch grid off completely" is the pre-existing `show_grid` toggle
  itself, not a 5th style here.

  Verified every OpenGL symbol used (`GL_LINE_STIPPLE`/`glLineStipple`/
  `GL_QUADS`/`GL_BLEND`/`GL_POINTS`/etc.) is real and importable before
  relying on any of them (PyOpenGL wasn't installed in this sandbox at
  all - installed it specifically to check, rather than assuming a
  legacy-GL symbol name was correct from memory), confirmed `glLine
  Stipple`'s own real, stable, decades-old `(factor, pattern)`
  signature, and directly verified the `squares` style's own fill
  geometry (exact coverage matching the line grid's own extent with
  zero gaps or overlaps, and consistent counter-clockwise winding on
  every cell).

  New `DFFViewport.set_grid_type()` (matches `set_show_grid`'s own
  existing pattern exactly) + real `grid_type` setting (`MapSettings.
  DEFAULTS`) + a real dropdown in the Render settings tab - the same
  group `radar_tiles_show_grid` lives in, renamed from "Radar Tiles"
  to "Grid & Radar Tiles" since this new control is genuinely broader
  in scope than radar tiles alone (the whole viewport's own grid,
  tiles included), so the old, narrower name would have been
  misleading now.

  `ast.parse` clean on both touched files; confirmed via AST no
  duplicate method definitions.

- **Aug 20, 2026 (cont'd)** — Fixed the real, underlying grid-coverage
  bug, per Keith: "the other thing I noticed about the original grid,
  is it didn't cover the whole area, bigger maps overlapped it
  massively, I cant calculate the size needed to cover all map sides,
  so not just grid pattern size, but grid area size, or limitless?"

  The real cause wasn't "too small for a big map" specifically - the
  grid was always drawn fixed at world origin `(0,0)`, completely
  independent of `self._pan_x`/`_pan_y` (the camera's own real pan
  position). Panning away from the origin at all - on any map, not
  only a genuinely large one - left the grid behind entirely rather
  than following the view; "bigger maps overlapped it massively" was
  really this same bug, just far more visible on a map large enough
  that normal navigation moves the camera well away from the origin
  as a matter of course.

  Real answer to Keith's own "grid area size, or limitless?" question:
  limitless, not a fixed size - genuinely camera-relative now rather
  than tied to any assumed map size, which would need knowing the
  real map bounds in the first place, the exact thing Keith said he
  couldn't calculate. `_draw_grid` now computes the camera's own real
  current focal point (`-pan_x`/`-pan_y` - the same real relationship
  already verified numerically for `capture_radar_tile`'s own camera
  math earlier this session), snapped to the nearest real step-
  aligned position first so the grid's own lines stay at fixed,
  stable world positions rather than visibly drifting as the camera
  moves by sub-step amounts - only the visible RANGE of grid lines
  shifts with the camera, matching how a real-world reference grid is
  actually supposed to behave. All 4 grid styles (`_draw_grid_lines`/
  `_squares`/`_dashed`/`_dots`) now take this real centre and build
  their own extent around it instead of the fixed origin.

  Verified the centre-computation/snapping logic directly: stable
  (no jitter) for camera movement within one rounding bucket, correct
  and deliberate re-snap to the next step position exactly at a real
  rounding boundary, and correctly follows the camera all the way to
  a real, large map's own far edge (tested at SA's own real ~3000-
  unit bound). Also directly verified the actual line-range geometry
  is genuinely centred on the camera's own focal point, not the old
  fixed origin.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026 (cont'd)** — Fixed a real, understandable point of
  confusion Keith ran into: "have you added the other grid types, all
  I see is solid, checkerboard, grid in preview tab, in settings, and
  there doesn't appear to be other grid styles?"

  The 4 new grid styles (Lines/Squares/Dashed/Dots) were genuinely
  already added and pushed - they live on the Render settings tab's
  own "Grid & Radar Tiles" group, not the Preview tab. What Keith was
  actually looking at is a real, separate, pre-existing setting -
  Background Mode's own "Solid Color / Checkerboard / Grid" choice on
  the Preview tab - which controls the viewport's flat, 2D background
  fill pattern, a genuinely different feature from the 3D reference
  grid overlay, despite both happening to use the word "grid."

  Added a clarifying tooltip to both dropdowns pointing at the other
  one, so this same mix-up doesn't repeat for anyone else who
  reasonably expects one "grid" setting to be the other.

  `ast.parse` clean.

- **Aug 20, 2026 (cont'd)** — Added real anti-aliasing, per Keith: "we
  need some kind of anti-alising, far away lines doesn't appear to
  flicker?" A thin, unsmoothed line far from the camera covers less
  than one pixel's worth of screen space per grid step, so as the
  camera moves even slightly, which pixels the line actually
  rasterizes to flips on and off between frames - the real cause of
  the reported flicker.

  **New 4x MSAA** at the whole viewport's own `QSurfaceFormat`
  (`_fmt.setSamples(4)`, this module's own top-level format setup) -
  the comprehensive fix, anti-aliasing every primitive (triangle
  edges too), not just lines - genuinely absent before this, not just
  off. Confirmed real and working directly (installed PyQt6 in this
  sandbox specifically to call `setSamples`/`samples()` and verify the
  value actually round-trips, rather than assuming the method name
  from memory).

  **New, additional `GL_LINE_SMOOTH`/`GL_POINT_SMOOTH`** specifically
  around the grid's own real line/point drawing (`_draw_grid_lines`/
  `_draw_grid_dots`) - a real, line/point-specific layer on top of
  MSAA, since sample coverage alone doesn't always fully resolve a
  line that's genuinely sub-pixel-thin at distance. Both real GL
  state changes are saved and restored around just each method's own
  drawing work (confirmed both real cases directly: blend already off
  before → correctly restored off after; already on before → correctly
  left on, not clobbered) rather than left globally on, so this can't
  silently affect any other, unrelated drawing call that never asked
  for blending. Applied inside `_draw_grid_lines` specifically (not
  wrapped around the whole `_draw_grid` dispatch) since 'squares'
  style already manages its own separate `GL_BLEND` window before
  ever reaching that method - wrapping at the dispatch level would
  have stepped on that already-correct, separate toggle.

  Verified every new OpenGL/Qt symbol used (`GL_LINE_SMOOTH`/`GL_
  POINT_SMOOTH`/`glIsEnabled`/`GL_NICEST`/`setSamples`) is real and
  importable/callable before relying on any of them - installed both
  PyOpenGL and PyQt6 in this sandbox specifically to check, rather
  than assuming names from memory.

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026 (cont'd)** — Added a 5th grid style, per Keith: "Can
  and add honeycomb effects."

  New `_draw_grid_honeycomb` - a real, standard hexagonal tiling
  (pointy-top orientation, the common "honeycomb" look), using the
  well-established, standard math for this (the same real formula
  widely documented elsewhere, e.g. redblobgames.com's own "hexagonal
  grids" reference - not invented here). `step` is used as the hex's
  own centre-to-vertex radius; real horizontal/vertical spacing and
  the alternating row offset all follow that same standard formula so
  the hexagons genuinely interlock edge-to-edge.

  Verified the actual tiling geometry directly, not just that it
  compiles - confirmed adjacent hexagons (both same-row and
  offset-row neighbours) share exactly one real edge (2 matching
  vertices) each, the real signature of correct, gap-free, overlap-
  free hexagonal tiling.

  Same real anti-aliasing treatment as every other line-based grid
  style (`GL_LINE_SMOOTH`, saved/restored around just this method's
  own drawing work).

  `ast.parse` clean; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026** — Fixed 2 real gaps in the startup "Load Options"
  bulk-IPL-loading dialog (`_load_selected_ipls_with_log`), per Keith:
  "When I start the app and load the data file, any version of GTA...
  I get the popup where you can load selected entries or skip; this
  dialogue does not show the IPL name in a separate section above
  with a load % indicator. It also needs to obey the settings, like
  the manual loader does, rendering down images."

  **Missing IPL name header + load % indicator** - this dialog used
  to be a plain `QTextEdit` scrolling log only, with the current IPL
  name mixed into the same scrolling text rather than pinned in its
  own section, and no percentage indicator anywhere. Fixed by using
  the same real `_VerboseLoadingDialog` the manual, per-IPL (eye-
  icon) loader already uses - a fixed, bold header label pinned above
  the scrolling list showing which IPL is currently loading, updated
  via a genuinely new `set_progress(current, total)` method added to
  that same class. Neither dialog actually had a real percentage
  indicator before this - added directly to `_VerboseLoadingDialog`
  itself (a real `QProgressBar`), not just to this one caller, so any
  future user of that same dialog gets it too.

  Found and fixed a real, separate, pre-existing bug while adding
  this: `QProgressBar` was used elsewhere in this same file
  (`imgfactory.py`'s own progress bar widget) but never actually
  imported anywhere at module level - a genuine latent `NameError`
  waiting to fire the first time that other code path actually ran.
  Fixed with one real, module-level import rather than another
  scattered local one.

  **Didn't obey settings like the manual loader does** - traced
  directly, not assumed: the real `texture_downscale_enabled`/
  `_threshold`/`_target` settings are applied at `DFFViewport`'s own
  texture-upload time, not at `ModelCache` load time - so this bulk
  path and the manual, per-IPL one already produce identical
  downscaling once the viewport actually renders what either one
  loaded. The real, confirmed gap was different: this method always
  showed its own detailed log dialog unconditionally, with no way to
  turn it off, unlike the manual loader's own equivalent dialog,
  which already respects the real `show_verbose_loading_dialog`
  setting. This method now checks that same setting.

  A real, always-present `QProgressDialog` (with its own real Cancel
  button) now drives the header+percentage regardless of that
  setting, matching `_preload_world_assets`' own real pattern exactly
  - that method's own progress dialog was never gated on the verbose
  setting either, only its own additional, more detailed log dialog
  was. Deliberately not made the whole progress UI conditional on
  the setting, since that would have been a real regression (losing
  the ability to cancel a bulk load entirely whenever verbose
  happened to be off) rather than a faithful match to the existing,
  established pattern.

  `ast.parse` clean; confirmed via AST no duplicate class/method
  definitions and exactly one real, module-level `QProgressBar`
  import.

- **Aug 20, 2026** — Fixed "Load Selected (12)" in Obj Browser only
  loading 2-3 of the selected IPLs, not all of them.

  Root cause: `_load_selected_ipl_sections` tracked which rows to load
  by row index, captured once before any loading started. But loading
  one IPL can rebuild the whole IPL table (reordering rows), so once
  that happened, the remaining row indices in the same loop pointed
  at the wrong rows and got silently skipped as "already visible."

  Fixed by capturing the real IPL names up front instead (names don't
  go stale the way row indices do), then re-locating each one's
  current row right before loading it - the same pattern already used
  one level down in `_on_ipl_section_cell_clicked` for the identical
  reason, just missing from this outer loop.

  Verified against a simulated mid-loop table rebuild: old logic
  loaded 1 of 12 selected IPLs, new logic loads 12 of 12.

- **Aug 20, 2026** — Grid background/line color pickers + line/dot
  size (4-10px) added to Render tab, wired to DFFViewport.set_grid_
  colors/set_grid_line_size. Blue (51,128,230) kept as default bg.

- **Aug 20, 2026** — Grid spacing setting; grid squares fill can now
  use a tiled image/texture instead of flat colour (Browse + tile
  size 64-1024). Path Lines split into Dat Nodes/Tracks/Airtrain-Plane
  rows with own colour+thickness (Airtrain/Plane not yet data-split
  from Dat Nodes - no confirmed field for it in the file format).
  Fixed a real pre-existing duplicate set_track_color definition and
  duplicate track_color settings-default entry found while adding this.

- **Aug 20, 2026** — Fixed real crash on startup: "name 'step' is not
  defined" in set_squares_fill - a leftover orphaned line from an
  earlier edit had ended up appended to that method's own body,
  referencing variables that only exist in _draw_grid_squares' own
  scope. Also adds: Skybox/Skydome background image (Browse), and
  Timecyc file Browse + Play/Stop (reuses Timecyc_Editor's own real,
  field-mapped TimecycParser - not a second parser), both in a new
  Environment group on the Render tab.

- **Aug 20, 2026** — Wrapped the Render tab in a real QScrollArea -
  it had never had one, which was the direct cause of the overlapping/
  overflowing controls Keith's screenshot showed once enough groups
  (Path Lines/Grid/Environment) were added to it. Tightened group
  spacing too.

- **Aug 20, 2026** — Grid zoom behaviour setting: "Locked" (existing
  default - grid stays same on-screen size regardless of zoom) or
  "Resize with models" (fixed world-unit cell size, grid scales with
  zoom the same way models do).

- **Aug 20, 2026** — Fixed real "Locked" grid jitter: plain int(dist/
  spacing) changed by 1 on almost every frame during zoom, visibly
  repositioning every line each time. Now snaps to a 1-2-5-10-20-50...
  sequence instead - only changes size a handful of times across a
  full zoom range. Added "Marching ants honeycomb" (dashed hexagons)
  and "None (hide grid)" to the Grid style dropdown.

- **Aug 20, 2026** — Grid zoom behaviour: new "Radar tiles" option -
  grid cells match the currently loaded game's real radar tile grid
  (12x12 SA/6000u, 8x8 VC-III/4000u, 36x36 SOL/12000u - per the
  already-confirmed RADAR_GRID_PRESETS, not re-derived), anchored to
  the map's real origin rather than following the camera, so tile
  boundaries line up with actually exported radar tiles. Auto-updates
  when a different game's world is loaded.

- **Aug 20, 2026** — Fixed "Resize with models" grid only covering a
  tiny area near the origin. It was reusing grid_spacing (default 5,
  designed as a divisor for Locked mode) directly as the world-unit
  cell size - 5 units/cell on a real map is tiny, and its visible
  range (step*10=50) never scaled with zoom either, so it stayed a
  small, static patch near the origin while the actual world extended
  far beyond it. Added a separate, real map-scale grid_fixed_step
  setting (default 200) for this mode, and made its visible range
  scale with zoom distance too so it actually reaches distant models.

- **Aug 20, 2026** — Grid cell count setting (12/24/36/48/60/72/84/
  96/108/120), replaces the old fixed *10 multiplier in _draw_grid's
  rng computation - controls how far the grid extends before it
  stops, independent of cell size, across every grid style.

- **Aug 20, 2026** — Radar tex layer: shows the real, already-
  generated radar tile images at their own real world positions
  (via compute_radar_grid/RADAR_GRID_PRESETS), under the model spawn
  layout, as an alternative to the plain grid. New checkbox + tiles-
  folder Browse in Grid & Radar Tiles settings. Auto-refreshes tile
  paths/bounds to match whichever game's world is actually loaded.

- **Aug 20, 2026** — Full review pass, per Keith: "grid disappears
  when IPL models are loaded, can you check all functions we have
  done tonight for bugs." Found and fixed 3 real bugs:

  1. Grid never disabled GL_DEPTH_TEST, unlike every other overlay
     (_draw_paths, cull/zone boxes) which already does. Before any
     world instances load, nothing occludes it; once ground-level
     building/road geometry draws before it (paintGL's own real
     order), depth testing correctly hides the grid wherever that
     geometry sits at or in front of it - the actual cause of "grid
     disappears when IPL models are loaded."

  2. Radar tex layer had the same gap in the opposite direction -
     drawn BEFORE world instances with depth test/writes on, it could
     have incorrectly occluded real ground-level geometry drawn right
     after it. Now disables both the test and depth writes.

  3. Timecyc's real-time hour tracking compared row.time directly
     against a 0-23 hour for every game, but only VC's own time slots
     actually are 0-23 hours - SA has 8 real, non-uniformly-spaced
     slots (confirmed against Timecyc_Editor's own SA_TIME_LABELS:
     real hours [0,5,6,7,12,19,20,22]) and GTA3 has 12 slots at
     2-hour intervals. Added the real per-game slot-to-hour mapping.

- **Aug 20, 2026** — Merged Timecyc play/stop into a new [Tcyc]
  toggle on the same toolbar row as [2DFX]/[Tobj], per Keith's own
  request. Removed the now-redundant separate Play/Stop button from
  Settings > Render > Environment - that row is Browse-only now.

- **Aug 20, 2026** — Fixed radar tex layer not working, per Keith:
  "those radar.txd files are in the gta3... unless it's SOL where
  they're in another file." The feature assumed loose radarNN.png
  files in a user-chosen folder; real radar tiles are radarNN.txd
  entries inside the game's own already-loaded IMG archive. Reworked
  to read them directly via ModelCache.get_textures() (same index
  already used for models/collision) - no folder picker needed
  anymore. Real naming per game: RADAR00-NN (SA/VC/III) vs
  radar0000-NNNN (SOL), matching radar_workshop.py's own real naming
  functions. Also fixed a real ordering bug this surfaced: the world-
  load hook was reading ModelCache before index_img_files() had
  actually indexed that world's own IMG files - moved to run after it.

- **Aug 20, 2026** — Fixed real segfault using an image as the grid
  texture tile, per Keith: "trying to use this image as the texture
  tile crashes the app. Segmentation fault (core dumped)."

  Root cause: _ensure_squares_texture (and _ensure_skybox_texture,
  _ensure_radar_tex_tile, added tonight, plus the pre-existing
  _ensure_auzo_icon_texture, same copied pattern) all called self.
  makeCurrent()/self.doneCurrent() while already being called from
  within paintGL's own call chain, where Qt has already made the GL
  context current. doneCurrent() releases it mid-frame - every GL
  call made right after that (texture binding, drawing) then runs
  with no current context at all, which is undefined behaviour and
  can segfault depending on the driver. Removed the redundant calls
  from all 4 methods - they only ever run during an already-active
  paintGL, unlike genuinely UI-triggered setters elsewhere in this
  file (set_light_dir/set_ambient/etc.) which correctly still need
  their own makeCurrent/doneCurrent since nothing else made the
  context current for them.

  Also added a "Reset Grid to Defaults" button to the Grid & Radar
  Tiles settings, per Keith: "it's easy to enter large values and
  mess things up."

- **Aug 20, 2026** — Texture opacity setting for grid squares/texture
  fill (was hardcoded at 0.5). Moved "hide grid" out of the Grid
  style dropdown into a separate "Show grid outline lines" checkbox -
  lets squares/texture fill show on its own with no line overlay,
  which the old dropdown-only 'none' option couldn't do (it hid the
  fill too). Full grid hide is still available via the Display tab's
  own Grid checkbox. Reset Grid to Defaults updated to cover both
  new controls too.

- **Aug 20, 2026** — Fixed timecyc looking like a flat single colour
  instead of a real day/night effect, per Keith: "not the effect of
  how it seen in game." The old version only ever read sky_bot and
  used it as one flat background clear colour - no sky gradient, and
  no effect on the actual lighting models are lit with.

  Now reads sky_top/sky_bot/ambient together each tick (confirmed
  offsets from Timecyc_Editor: ambient always [0-2], sky_top/sky_bot
  differ per game) and drives 3 real effects: a genuine 2-colour sky
  gradient (top-of-screen to bottom, GL smooth-shaded, same
  orthographic technique the skybox feature already uses), an RGB
  ambient tint applied on top of the existing ambient/diffuse
  intensity sliders (so models are actually lit differently through
  the cycle, not just the background), and the flat colour as a
  fallback for wherever the gradient doesn't reach.

  Verified the underlying hour-to-row lookup with synthetic day-cycle
  data across several hours - confirmed it genuinely produces
  different sky/ambient colours at midnight vs noon vs evening,
  rather than sticking on one row.

- **Aug 20, 2026** — New "Hide grid over radar tiles" checkbox, per
  Keith: "toggle the grid over radar, see it outside, but not on the
  radar tiles." Grid lines within the radar tex layer's own real
  bounds are now suppressed (each line split into up to 2 segments,
  skipping the middle portion that overlaps the map area) while still
  drawn in full outside it. Only takes effect while the radar tex
  layer itself is on. Verified the segment-splitting math directly
  against a simulated bounds/range before wiring it in.

- **Aug 20, 2026** — Real improvement to timecyc sky rendering, per
  Keith: "still isn't being rendered like it would be in game...
  all its doing it cycling through colours, no horizon and sky
  bands." A flat 2-colour top/bottom blend was still not what a real
  GTA sky looks like - brighter/warmer near the horizon (sun_core),
  darker overhead, not a plain linear blend between two colours.

  Now reads sun_core too (real per-game offsets: SA [15-17], GTA3
  [12-14], VC [21-23]) and builds a real 3-stop gradient from 2
  stacked quads: zenith (sky_top) down to sky_bot at a fixed split
  line, then sky_bot down to sun_core's own brighter glow at the
  bottom of the visible sky.

  Caught a real, separate pre-existing bug in Timecyc_Editor itself
  while confirming these offsets: its own SA sun_core reads rgb(12)
  (same offset as sky_bot) despite its own comment saying [15-17] -
  a likely copy-paste slip in that file. Used the documented [15-17]
  offset here rather than that copied value, since this is a
  separate implementation, not a reuse of that code.

  Verified the colour extraction directly against synthetic day-
  cycle data - confirmed sun_core comes out genuinely distinct from
  sky_bot at every hour, giving the gradient a real third stop rather
  than degenerating back to 2.

- **Aug 20, 2026** — Fixed the sky gradient rendering upside-down
  (zenith at the bottom, horizon glow at the top) - flipped, per
  Keith: "the sky need flipping vertically." Noted for later:
  fog is still on the list of real effects to add alongside this.

- **Aug 20, 2026** — Fixed timecyc colour effect blinking on and off,
  worse while moving/turning the view, per Keith. _on_timecyc_tick
  runs off its own independent QTimer, not Qt's paint lifecycle, but
  was directly calling self.makeCurrent()/_setup_lighting()/self.
  doneCurrent() on top of that - pure duplication, since paintGL
  already calls _setup_lighting() itself on every real frame. That
  direct, out-of-band call could collide with Qt's own GL context
  handling during a real, rapid paintGL call - exactly when camera
  movement triggers the most repaints, matching what was reported.
  Removed it entirely; self.update() (still called) is enough on its
  own - the next real paintGL call already re-applies the new
  ambient tint through its own existing lighting setup.

- **Aug 20, 2026** — Fixed timecyc running its own separate, redundant
  timer, per Keith: "there appears to be another timer running
  besides the tojb timer." It was a real, second "time of day" clock
  alongside the app's actual one (the TObj time-flow timer driving
  the Time switch's own QTimeEdit). Removed timecyc's own internal
  QTimer entirely - it now syncs to that same, real simulated hour
  via _on_tobj_time_changed (already fires on every change to that
  clock, whether from a manual edit or the flow timer's own automatic
  advance). Confirmed this also covers VC correctly - the offset
  table and TimecycParser's own field-count-based game detection
  (52 fields) were already both correct for VC; the real bug was the
  redundant clock, not per-game colour lookup.

- **Aug 20, 2026** — Timecyc file auto-detection: looks for timecyc.
  dat next to the currently loaded game's own main .dat file, re-
  checked on every world load so switching games picks up each one's
  own real file rather than sticking to whichever was found first.
  Browse still works for a manual override. Removed the leftover
  "[Tcyc] button, same row as..." text label from the settings row -
  the real toolbar button already speaks for itself.

  Added real "Show Grid"/"Show Timecyc" quick-select checkboxes at
  the top of Grid & Radar Tiles settings. Turns out neither of these
  had a real on/off control anywhere before this - an earlier tooltip
  of mine claiming a "Display tab grid checkbox" existed was simply
  wrong; there wasn't one. Fixed that tooltip and one other pointing
  to the same non-existent control, now pointing at the real new
  quick toggle instead.

- **Aug 20, 2026** — Fixed Auzo not showing content, per Keith:
  "clicking on the Auzo button still doesn't show the contents on the
  auzo file, from auzo entry" (comparing to the real, working paths/
  zon/cull/occl workflow: "clicked on first, then [Paths] button to
  show"). Root cause: _refresh_auzo_visualization was only ever
  called from its own toggle button's change handler - unlike paths/
  cull/zone/occl, all four of which also refresh from the one,
  central _apply_ipl_visibility_filter that runs whenever IPL
  visibility changes (i.e. exactly the "click the IPL entry" half of
  that workflow). Clicking a new auzo-containing IPL entry never
  actually pushed anything to the viewport unless the toggle itself
  happened to be flipped at that exact moment. Also fixed a second,
  related gap: it never filtered by which IPLs are actually visible/
  hidden at all, unlike every other overlay here.

- **Aug 20, 2026** — Two real fixes, per Keith:

  1. "water function should also load the waterpro.dat, and display
     it in the same way water_workshop works" - load_waterpro_dat was
     already loading it into loader.waterpro, but nothing ever read
     it back out anywhere - a real, confirmed gap. Added set_waterpro_
     cells/_draw_waterpro_water to the viewport (real world bounds via
     the same RADAR_GRID_PRESETS/compute_radar_grid the radar tex
     layer already uses), and _refresh_water_visualization now
     resolves and pushes it alongside water.dat's own shapes. Also
     fixed a real, separate orientation bug caught before shipping -
     grid row 0 is north (confirmed against water_workshop.py's own
     "No Y-flip" comment), not south as first written.

     Also fixed "besides the button on the IPL control that doesnt
     seem to work" - same real bug class as the earlier Auzo fix:
     _refresh_water_visualization was never called from _apply_ipl_
     visibility_filter, the one, central place every other overlay
     (path/cull/zone/occl/auzo) already refreshes from. Now called
     there too.

  2. "the images show the rotation, but the sky doesn't pane" [pan] -
     the sky gradient was a fixed, screen-space orthographic overlay,
     drawn before any camera transform at all, so it never rotated
     with yaw/pitch the way a real sky visibly would. Reworked into a
     real, world-space "box sky" (4 large side quads with the same
     real 3-stop gradient) drawn after yaw/pitch rotate the scene but
     before pan translates it - genuinely rotates with the camera now
     the same way any other object in the scene does, while staying
     unaffected by panning the same way a real, infinitely-distant
     sky would be.

- **Aug 20, 2026** — Also reworked the user-image Skybox to the same
  real, world-space box-sky technique the gradient sky just got - it
  had the identical "doesn't pan with the camera" bug, just not yet
  reported since Keith's own screenshots showed the gradient sky
  specifically. Image now maps around the 4 side faces as a single
  wraparound panorama rather than repeating the same frame on each side.

- **Aug 20, 2026** — Fixed real sky glitching, per Keith: "weird
  glitching in the background... I dont remember RED in the sky."
  The box sky's own horizon-glow band used to extend from the
  horizon (Z=0) down to well below it. Since this app's own map
  geometry is finite, a wide-angle view could look past the edge of
  the map's own ground into that "underground" portion of the box
  sky, revealing the glow band's own bright colour (often warm red/
  orange near sunrise/sunset) somewhere a real sky would never
  actually be visible from. Kept the whole box sky at or above the
  real horizon line now, for both the gradient and image skybox.

  Added a "Flip sky gradient" checkbox (Environment settings), per
  Keith: "Remember when I said the timecyc was upside down? We need
  a toggle to switch it either way, just in case I was wrong."

- **Aug 20, 2026** — Added "Show Load Options dialog on world load"
  checkbox to Loading settings (already flagged as a TODO at the
  call site), per Keith: "we need a settings option for this popup
  so we can disable it if needed." Off skips the dialog entirely and
  stays fully lazy, same as picking "Load from .dat file" in it
  would.

- **Aug 20, 2026** — Fixed multiple dialogues opening when loading
  several IPLs at once, per Keith: "when selecting multiply ipls,
  all select, load all, this opens multiple dialogues. It should be
  one window with the title and process change." Each selected IPL
  used to get its own separate _preload_world_assets progress dialog
  (and its own separate detailed _VerboseLoadingDialog log, if
  verbose loading was on), popping open and closing per IPL in turn.
  _load_selected_ipl_sections now creates one real, shared
  QProgressDialog up front, its own title/label updated for whichever
  IPL is currently loading; _preload_world_assets reuses it instead of
  creating a second one, and the per-IPL detailed log dialog is
  suppressed during a bulk load (the shared dialog already reports
  which IPL is loading).

- **Aug 20, 2026** — New "Water Display" settings group, per Keith:
  "Maybe show water should be in lines, dots, hexagons, with the
  water file path... another entry for custom textures to be shown
  instead of the grid... also retaining the option to show grid
  outside the map boundary/radar/rendered water." Adds:
  - Water file status (shows whether water.dat/waterpro.dat was
    auto-detected for the currently loaded game)
  - Water style: flat fill (default)/outline lines/dots/hexagons/
    custom texture (tiled), applied to waterpro.dat's own grid cells
  - Custom water texture Browse + tile size
  - "Hide water outside map boundary" - same real idea as "Hide grid
    over radar tiles", inverted for water (skips cells outside the
    real map area instead of the void beyond it)

- **Aug 20, 2026** — [Tcyc] button now shows live time feedback (e.g.
  "Tcyc 14:30") while playing, per Keith: "[TCYC] button doesn't
  appear to change as time advances." Updated from _on_tobj_time_
  changed, the same real, shared clock callback that already drives
  timecyc's own hour - only while the button is actually checked, and
  resets back to plain "Tcyc" when turned off so it doesn't keep
  showing a stale time.

- **Aug 20, 2026** — Fixed the real root cause of water/timecyc not
  working for a real, unmodified install, per Keith: "I am using the
  original VC install, the waterpro.dat is in gameroot/data/waterpro.
  dat." load_waterpro_dat/load_water_dat relied entirely on gta*.dat's
  own WATER directive - genuinely correct for SA (its own gta.dat
  really has one), but a real, vanilla III/VC gta3.dat/gta_vc.dat
  never does; waterpro.dat is a fixed, hardcoded file there, not
  directive-based. Both now fall back to the real, standard data_dir/
  waterpro.dat (or water.dat) path when no directive is found. Also
  fixed for SOL specifically (main .dat at gameroot/sol/gta_sol.dat,
  but waterpro.dat still at gameroot/data/) and applied the same real
  fix to timecyc.dat auto-detection.

- **Aug 20, 2026** — Water no longer writes to the depth buffer while
  drawing, per Keith: "when water is being rendered, don't render
  over loaded IPL models." GL_DEPTH_TEST was already correctly
  enabled globally and untouched before water draws, so opaque model
  geometry already occluded it correctly - the real, standard gap for
  translucent geometry like this is depth writes, which were left on
  (the default). Now reads depth, doesn't write it, for both water.
  dat shapes and waterpro.dat's own grid - the correct pattern for
  any semi-transparent surface, preventing it from interfering with
  other transparent overlays drawn afterward.

- **Aug 20, 2026** — Timecyc auto-detection now also falls back to
  the active Project Manager's own main_window.game_root/data folder,
  per Keith: "we also have the project profiles to fall back on" -
  a second, real source of "where this game actually lives"
  independent of whichever world happens to be loaded this session.

  Added a real "Browse…" button next to the Water file status in
  Water Display settings, per Keith: "if it's not found, ask for it,
  [browse] with the path to where the waterpro.dat is." Manually
  parses the chosen file as either format (waterpro.dat first, water.
  dat as fallback) and refreshes the water overlay immediately.

- **Aug 20, 2026** — Applied the same real GL_CULL_FACE fix to the
  image Skybox that _draw_sky_gradient already got - confirmed
  directly from Keith's own screenshots (a solid black wedge cutting
  into the sky at certain angles), the exact shape a culled box-sky
  face would leave behind. Neither method ever touched cull-face
  state, so it depended on whatever was left over from the previous
  frame's own draw calls.

- **Aug 20, 2026** — Made water's depth-testing explicit and
  defensive, per Keith's follow-up: "looking at it side on, water
  level appear correct... looking from above or below, the water is
  blocking everything else out... is there a way to make the ipl
  models take priority." Traced the full paintGL draw chain
  (2DFX/paths/cull/zone/occl/tracks/nodes/auzo, each with its own
  real disable/enable pairs) looking for an unbalanced GL_DEPTH_TEST
  leak reaching water's own turn - didn't find a definitive one, but
  rather than leave this only half-diagnosed, both water-drawing
  methods now explicitly force GL_DEPTH_TEST on and GL_DEPTH_FUNC to
  the standard GL_LESS right before drawing, instead of assuming
  whatever state preceded them was already correct.

- **Aug 20, 2026** — Added explicit dry/cutout handling to waterpro
  grid cells, per Keith: "waterpro.dat allowing cut out areas, making
  sure waterpro.dat is used is very important, look at water_workshop
  as resource." Confirmed water_workshop.py's own real logic
  (WaterGridWidget._cell_col): val == 128 means dry/land, any other
  value means water. Added this exact check. Honest finding from
  testing this against simulated data: since 128 is already outside
  levels' own valid 0-47 range, the existing out-of-bounds skip was
  already excluding it in the common case - this specific addition
  is correct and matches the confirmed reference logic exactly, but
  testing showed it doesn't change behaviour for a simple 128-vs-0-47
  grid, so it likely isn't the full, real root cause of water
  covering the whole map on its own.

- **Aug 20, 2026** — Fixed [Tcyc] not actually stopping when turned
  off, per Keith: "this would only trigger the Timecyc on, or off, on
  I see the timecyc, off the timecyc function stops." Turning it off
  used to only stop future updates - the sky gradient/ambient tint/
  background override from whatever hour was last applied stayed
  active forever, since paintGL's own dispatch checked whether the
  sky gradient colours were set at all, not whether the play flag
  itself was true. Now genuinely reverts everything on off: clears
  the sky gradient, background override, and ambient tint back to no
  tint, so lighting/background/sky all actually return to normal.

- **Aug 20, 2026** — Removed [Tcyc] auto-starting the shared Tobj
  time-flow timer, per Keith's own direct correction: "Timecyc
  playing should be linked to TOJB, 2DFX time button, we dont need
  to start time with TCYC button, thats only meant to toggle the sky
  on or off?" Correct - [Tcyc] is a pure show/hide toggle for the
  effect at whatever hour the shared clock currently says; time
  progression stays exclusively the Tobj/2DFX time Play/Stop
  button's own job. If that timer isn't running, [Tcyc] now correctly
  shows a static snapshot rather than reaching into a control it was
  never meant to own.

- **Aug 20, 2026** — Re-verified [Water]'s full real chain (click ->
  signal -> handler -> set_show_water -> paintGL's own dispatch),
  per Keith: "the Water button pressed does nothing, we need to fix
  this." All of it was already correctly wired - the real, remaining
  explanation is no water data being loaded to show at all (his own
  earlier settings screenshot already confirmed "Water file: Not
  loaded"). Rather than stay a silent no-op, toggling it on now says
  so directly in the status bar when there's genuinely nothing
  loaded, pointing at Settings > Render > Water Display > Browse.

- **Aug 20, 2026** — Two real fixes, per Keith's own screenshots:

  1. "I think the show water in settings show grid and 'Square (blue
     fill)' is overriding the [Water] button" - confirmed and fixed.
     The grid (drawn after water, with depth-testing deliberately
     disabled so it's always visible as a reference overlay) always
     visually covered water wherever they overlapped whenever the
     grid style was a filled one. Water now draws after the grid
     instead - real map data takes visual priority over a generic
     reference aid, not the other way around.

  2. "turning the button on shows a timer inside it, we dont need
     that" - removed [Tcyc]'s own earlier "live time" label growth;
     the same time already shows on the separate Time display right
     next to it in the same IPL Controls row, genuinely redundant.

- **Aug 20, 2026** — New "Preload Game Data Files" dialog, per Keith:
  "we need a preload menu, on a right click, and map workshop menu,
  where it shows the contents on the game/data/ folder, picking the
  file >> over to the preload box, including the waterpro.dat,
  because we can't seem to find it otherwise." Lists every real file
  in the currently loaded game's own real data folder (via the same
  already-working, SOL-aware _game_data_folder_candidates logic
  timecyc auto-detection already used - now shared, not duplicated),
  with an Available/>>/<</Preload two-list workflow and a real Load
  button that parses/applies water.dat/waterpro.dat/timecyc.dat by
  filename. Added to both the File menu and the IPL Sections table's
  right-click menu, alongside the existing IPL load/unload entries
  rather than replacing them.

  Caught and fixed a real, self-introduced bug while building this -
  a str_replace edit accidentally dropped _game_data_folder_
  candidates' own "def" line, silently merging its docstring/body
  into the preceding method instead (still syntactically valid
  Python, so it didn't show up as a parse error - caught via a
  function-count check instead).

- **Aug 20, 2026** — Preloading a real, recognised file now also
  turns its own on/off toggle on, per Keith: "if those files pre
  loaded, in objects browser the ipl and zon entries would already
  be highlighted, so the off on buttons wouldn't need to change."
  Loading the raw data alone wasn't enough - the water/timecyc
  refresh methods only actually show anything while their own real
  toggle is checked, clearing right back out otherwise. Uses set_
  shown (already emits the same real signal a manual click would),
  so a preloaded file is genuinely visible immediately, not loaded-
  but-invisible until a separate manual toggle press.

- **Aug 20, 2026** — Fixed preloaded IMG files being invisible to
  other tools (TXD Workshop, Radar Workshop, Model Workshop, COL
  Workshop), per Keith: "loading img into img factory the other
  tools... see the files, but preloading the img into map workshop,
  the radar workshop, model workshop, col workshop don't see the
  files, so preloading the img needs to follow the same rules as
  loading the img file into img factory and show the tab."

  Root cause: Map Workshop's own world-load hook only ever called
  ModelCache.index_img_files - a completely separate, internal index
  other tools never see. The real, established registry other tools
  actually discover files through is main_window.open_files, only
  ever populated by _load_img_file_in_new_tab (the same real method
  a manual File > Open IMG uses). Now also calls that for every real
  IMG path a loaded world references, skipping any already open
  (checked by real file path) so re-loading a world repeatedly
  doesn't pile up duplicate tabs. Same real fix added to the Preload
  dialog for .img entries specifically.

- **Aug 20, 2026** — Two real fixes, per Keith's own screenshot
  confirming waterpro.dat loaded successfully:

  1. "remove the square (blue fill) entry, temp comment it out if
     possible... add 'Hide Grid'" - Squares commented out (not
     deleted, per Keith's own explicit preference) in the grid style
     dropdown; a saved grid_type of 'squares' from before now falls
     back to Lines rather than silently disappearing. Re-added the
     earlier-removed "Hide grid" ('none') option to the same
     dropdown, alongside the existing separate Show Grid quick
     toggle - couldn't reproduce his own report that toggle also
     disables water through code tracing (show_water and _show_grid
     are genuinely independent dispatches in paintGL), so this gives
     a second, alternate way to hide the grid to test against.

  2. Preload dialog real additions, per Keith: "need a save button
     that remembers picked entries, also able to see the path files,
     dir level up down" - real directory navigation now (editable
     path field, Up button, double-click a folder to open it,
     folders listed with a trailing "/" ahead of files) instead of
     being fixed to the one data folder; a real Save Picks button
     persists the current preload list's own real file paths, auto-
     restored the next time the dialog opens.

- Aug 20 2026 - Loaded IPL rows now show white text (was greyed/
  normal same as unloaded), per Keith: mark preloaded entries as
  loaded white in obj browser ipl list. _style_ipl_name_item takes a
  loaded flag now, both real call sites updated.
- Aug 20 2026 - Preload dialog now recognises .ipl/.zon files, per
  Keith: automatically search for maps, paths, dat files. Matches
  the file against the existing IPL Sections table by name and
  triggers the same load path the eye-icon click uses, instead of
  showing "not a recognised type".

- Aug 20 2026 - Restored map_workshop.py/dff_viewport.py to this
  exact point (commit 2f509b56), per Keith: "restore just before we
  removed the water code." The whole water rewrite that followed
  (disconnect from settings, new Preload-driven water2 system, full
  removal of the old system, retooling the auto-load pipeline, plus
  a real crash bug it introduced along the way and a real Save Picks
  persistence bug) is reverted. Also reverts the SVG icon work for
  Cull/Zon/Occlusion/Paths/Tracks/Tcyc (910099cf), since that came
  after this point too - flagged separately, can be re-applied
  cleanly if wanted. tex/waterclear256.png and projects.json (both
  Keith's own separate commit) were left untouched.

- Aug 20 2026 - Loaded IPL row text now uses the palette's own real
  BrightText role instead of a hardcoded QColor(255,255,255), per
  Keith: "entries should be displaying the theme aware white." A
  fixed pure white isn't theme-aware - a light theme's own background
  could sit close to it too, making loaded entries hard to tell apart
  from the background rather than from unloaded rows. BrightText is
  Qt's own standard role for exactly this (distinct/emphasised text),
  so it adapts correctly whatever theme is active.

- Aug 20 2026 - Two real fixes, per Keith: "starting map_workshop
  back up, I noticed there is nothing in the startup, saying
  preloading files, etc":

  1. Save Picks never actually applied anywhere on its own - it only
     ever restored the saved list back into the Preload dialog's own
     UI the next time it was manually reopened, still needing a
     manual Load click every time. New _apply_saved_preload_picks
     runs automatically once a real world is available, with real
     status feedback matching every other stage of the same load
     sequence.

  2. Re-applied the Save Picks persistence fix (missing map_settings.
     save() call) - the earlier full water-rewrite revert (restore
     to before the water code was removed) undid this fix too, since
     it landed after that restore point. Without it, saved picks
     never reached disk at all, so there was nothing for the new
     auto-apply above to find on a real restart.

- Aug 20 2026 - Diagnostics added, per Keith: "preload still not
  working, and nothing in the status log, is there a conflict
  somewhere." Traced map_settings' own real save/load path in full
  (singleton pattern via __new__, debounced auto-save on every set(),
  app-folder-relative config path via _model_workshop_config_dir) -
  all of it looked correct on careful inspection, couldn't reproduce
  or disprove the actual failure without running the real app.
  _apply_saved_preload_picks now always leaves a real, visible status
  message even when nothing was saved (previously silent, which
  looked identical to "never ran at all"), including the real
  settings file path and whether it exists on disk. The hook call in
  _apply_loaded_world is now wrapped in try/except too, in case an
  uncaught exception there was silently swallowing the real error.

- Aug 20 2026 - Auto-load last world on startup, per Keith's own
  explicit "option 2" choice from yesterday, confirmed still needed
  by his own follow-up log ("nothing about loading preloaded files")
  - traced that to no world ever being loaded at all during that
  test, not a bug in the preload hook itself (the revert to before
  the water rewrite was genuinely correct and intact). New _auto_
  load_last_world, deferred via QTimer.singleShot the same real way
  _restore_dock_state already is, reuses the existing recent_dat_
  files list and _load_game_dat_file path the Recent menu's own
  entries already use - most recent entry, skipped quietly (no
  startup popup) if there's nothing recent, the new toggleable
  auto_load_last_world setting is off, or the file no longer exists
  on disk.
