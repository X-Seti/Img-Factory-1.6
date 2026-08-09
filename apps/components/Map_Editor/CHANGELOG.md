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
