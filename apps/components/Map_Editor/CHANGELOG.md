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









