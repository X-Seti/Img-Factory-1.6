#this belongs in root /TODO.md - Version: 10

## July 2026 - Map Editor (Map Workshop) - status and plan

**Status**: IN PROGRESS - UI shell + data layer + viewport foundation
built and tested; real per-instance rendering and editing still to come.

### What's done so far (apps/components/Map_Editor/)
- `map_workshop.py` forked from DP5_Workshop's UI shell (ribbons,
  Ribbon Manager with move/save-load presets/right-click access,
  docking, settings dialog) - rebranded MapWorkshop/MapCanvas/
  MapSettings, own settings file (map_workshop.json) and ribbon
  presets folder (map_ribbon_presets/), separate from DP5's.
- Data layer wired up via the EXISTING apps/methods/gta_dat_parser.py
  (GTAWorldLoader/detect_game/IDEObject/IPLInstance) - already does the
  full engine-accurate two-phase load (default.dat -> base IDEs, then
  main .dat -> map IDEs+IPLs), multi-game detection (GTA3/VC/SA/SOL),
  and object/instance cross-referencing. Nothing new needed to parse
  DAT/IDE/IPL - just wiring. "Load Game Folder..." in the File menu
  runs it and reports a summary.
- `depends/map_viewport.py` - MapViewport, an OpenGL widget reusing
  DFFViewport's camera/projection architecture (yaw/pitch/pan/dist,
  ortho vs perspective, set_view_lock for pane presets). Currently
  renders a placeholder marker cube per instance + a ground grid, not
  real per-instance geometry yet.
- World View dock - Top/Side/3D triple-pane (one horizontal splitter,
  NOT a 2x2 quad - confirmed this explicitly per Keith), each pane
  right-click-on-LABEL-only reassignable (Top/Side/Front/3D), double-
  click or menu to maximize/restore any pane to fill the whole dock
  (shows a "Full View" label while maximized, which is also the
  exclusive right-click target so drag-rotate works everywhere else).
- Instance List dock - browsable table (Model/TXD/Position/Interior/
  Source IPL) of every loaded placement, click a row to centre all
  three World View cameras on it.
- Paint canvas (forked in from DP5, not relevant to map editing) is
  hidden by default, toggleable via a "Show Canvas" icon in the
  Canvas Tabs ribbon.
- Recovered real KED/MooMapper Delphi source (`ked_reference_source/`)
  for UX/feature reference - not directly portable (Pascal), but a
  good checklist: Item/Item-Instance/Object-Definition/Archive editors,
  radar calibration, path editing, file validation, multiple render
  toggles, Vice City mode support.
- Along the way, fixed several real bugs in shared code this surfaced:
  a DFFViewport GL-context-guard bug (resizeGL called before a context
  existed), the quad-pane maximize feature not existing at all in
  Model Workshop, and the pane context menu covering the whole pane
  instead of just its label (which broke right-click-drag rotate).
- Rendering fixed for real-world scale (tested against Keith's actual
  GTASOL data: 13,178 objects, 51,711 instances, 106 IPL files) -
  first pass used glVertexPointer/glDrawArrays (client-side vertex
  arrays) for a single fast draw call, which crashed hard (process
  abort/core dump) on Keith's actual hardware since that legacy API
  isn't exposed by his driver. Reverted to glBegin(GL_POINTS)/
  glVertex3f/glEnd (confirmed working elsewhere in this codebase,
  e.g. Model Workshop) - still points not cubes (24x fewer vertices
  per instance than the original approach), just without the
  crash-prone upload step. Instance List also switched from
  QTableWidget (2.6s UI freeze at 51,711 rows) to QTableView + a lazy
  QAbstractTableModel (0.002s) - both confirmed at Keith's real scale.
- IPL Sections panel - repurposes the central-widget area the canvas
  leaves empty when hidden (Keith's own suggestion from a screenshot):
  lists every IPL that contributed instances, each with a Show/Hide
  toggle, filtering both World View and Instance List in-memory
  without reloading.
- Load Game DAT File (standalone "ask for a specific gta_xx.dat" flow)
  and DAT Browser's tree "Load with Map Workshop" right-click, both
  going through GTAWorldLoader.load_from_dat - the old top-level "Map
  Editor" button in DAT Browser was removed as redundant.
- Panels ribbon - show/hide toggles (using each QDockWidget's own
  toggleViewAction()) for World View, Instance List, and Object
  Browser, plus the paint-canvas toggle. Found and fixed a real,
  previously-unnoticed bug here: the canvas toggle had been living in
  the Canvas Tabs ribbon, which calls ribbon.clear() on every tab
  switch - silently wiping the toggle out. Moved to its own ribbon
  that nothing else touches.
- Object Browser dock - search box + four filter modes (All/Most
  Used/Favourites/Generic) over the loaded object catalog, with
  per-model instance counts and persisted favourites.
- Confirmed, via Keith's real default.dat/gta_vc.dat/gta3.dat/gta.dat/
  gta_quick.dat files, that every .dat directive the engine actually
  uses across GTA3/VC/SA is now handled (IDE/IPL/MAPZONE/COLFILE/IMG/
  TEXDICTION/MODELFILE/SPLASH) - see the two gotchas below for what
  that surfaced. SOL specifically still rests on inference (same
  engine as SA) rather than a directly verified real file.
- LOD support - GTAWorldLoader.resolve_lod_pairs() (SA/SOL only, GTA3/
  VC's inst format has no lod_index field) resolves each instance's
  paired LOD counterpart. Global "Normal | LOD | Both" dropdown in the
  Panels ribbon, plus a per-instance override via Instance List right-
  click (correctly resolves back to the right pair whichever member is
  currently displayed). **Caveat**: the exact lod_index semantics are
  a best-effort interpretation of community-documented format, not
  verified against official docs or a real sample - see the item below
  for what would help confirm this.
- Removed user-visible DP5 branding: the actual literal "[DP5]"
  titlebar button (get_menu_title() -> now "MAP", matching the DFF/
  TXD/COL convention other editors use), a few tooltips/status
  messages, a stale comment. Left the paint-tool ribbon contents
  themselves (Pencil/Eraser/etc) and some internal-only references
  (comments, private method names, a backward-compat alias) alone -
  whether those should also go is a separate scope question.
- Binary IPL detection (detect_ipl_format) - some SA ports pack IPL
  data in binary form inside IMG archives (e.g. gta3.img) rather than
  as loose text files; this distinguishes the two without needing to
  know the exact binary struct layout. A BinaryIPLParser stub exists
  but deliberately does NOT attempt real parsing yet - see below.

### Needs Keith's input / real data to proceed safely

- **LOD pairing semantics** - the interpretation above (lod_index as a
  0-based position among a file's own inst entries) produced correct
  results against synthetic test data built to match it, but hasn't
  been checked against a real SA .ipl with known LOD pairs. Worth
  spot-checking once real data/an in-game reference is available.

### Binary IPL - RESOLVED (read side), verified against real data

Keith provided 165 real binary .ipl files (crack.ipl, countn2_stream*,
vegasw_stream*, etc, all standard SA "streaming" IPL naming). Read-
side format confirmed empirically, not from documentation:
- Magic b"bnry" + 76-byte header (18 x int32 LE) - only inst_count
  (index 0) and a constant 76 (index 6, the header size itself) are
  confirmed; the other 16 fields likely locate cull/zone/other
  sections but aren't identified yet.
- Each inst record: 40 bytes - 7 float32 (pos xyz + quaternion xyzw),
  then 3 int32 (model_id, a flags-like field, lod_index).
- Verified across all 164 binary files in the sample (1 crack.ipl-style
  file plus 163 stream files): 36,569 total instances, zero parse
  failures, zero quaternion-magnitude sanity failures (every single
  instance's rotation is a valid unit quaternion), and world positions
  fall exactly within San Andreas' known map bounds (X/Y roughly
  ±3000, Z -76 to 1382) across the whole dataset. The "flags" field
  (previously guessed as "interior") shows a clean bitmask pattern
  (0, then powers of 2, then their OR'd combinations) confirming it's
  some kind of per-instance flags, not an interior number.
- BinaryIPLParser in gta_dat_parser.py now actually parses inst data
  (previously a stub). Cull/zone/other binary sections and write-back
  are NOT done yet - the next steps once this is wired into the actual
  loading pipeline (GTAWorldLoader currently only reads text-format
  .ipl files directly off disk; reading these binary ones out of an
  actual gta3.img still needs the IMG-extraction + detect_ipl_format +
  BinaryIPLParser pieces connected together).

### Next up, in priority order

0. **Requested ribbon features not yet built** (Keith's own list, for
   a "new icons for one of the ribbons" pass once real geometry exists
   enough to make some of these meaningful):
   - Search icon - a quick-access icon/shortcut to the Object Browser's
     existing search box (currently only reachable by opening that
     dock), or a dedicated search-everything (objects+instances+IPLs)
     entry point.
   - Show cull boxes - GTAWorldLoader.culls is already parsed (position/
     size from the IPL's cull section) but nothing visualises it yet;
     needs a toggle + rendering the box bounds in World View.
   - Overlay collision boxes - apps/methods/col_core_classes.py/
     col_loader.py already parse COL data fully (item 6 below covers
     wiring COL loading in at all) - once that exists, an overlay
     toggle showing collision bounds alongside/instead of instance
     markers.
   - Shift map sections / rotate map sections - moving or rotating a
     whole IPL's worth of instances together (not per-instance editing,
     which is item 5's "Editing + editor shortcuts" - this is closer to
     a bulk transform tool, e.g. for repositioning an entire imported
     map section). Needs its own design: selection model (which IPL/
     instances count as "the section"), a transform gizmo or numeric
     input, and how this interacts with LOD-paired instances (moving
     one member of a pair presumably needs to move both).

0.5 **Inst editing suite - fully scoped/confirmed with Keith, nothing
   built yet.** This is a big, multi-part feature; breaking it out here
   so each piece can be picked up independently:

   - **Instance List redesign**: FULLY DONE - table simplified to just
     ID + Model columns; single-click AND double-click both centre the
     World View camera, show a red/green/blue XYZ gizmo at the
     instance's position, and show/update a non-modal edit panel (top-
     left corner, Qt.WindowType.Tool - not a blocking modal) with
     Identity/IDE/2DFX/TOBJ/Placement info, cross-referenced by
     model_id via get_2dfx_for_model()/get_tobj_for_model(). Position
     and rotation are live-editable via small/large-step nudge buttons
     either side of a directly-editable value per axis (X/Y/Z) -
     rotation edited as degrees, converted to/from the underlying
     quaternion via new, round-trip-verified quat_to_euler_degrees()/
     euler_degrees_to_quat() utilities. Edits mutate the IPLInstance in
     memory and refresh World View immediately. NOT yet done: writing
     edits back to the actual IPL file on disk (this is in-memory only
     for now) - that's part of the bigger Editing + editor shortcuts /
     undo item below; ALSO NOT yet done - double-clicking the rendered
     marker directly in a World View pane (as opposed to the Instance
     List row) to open the same edit panel. This needs CPU-side
     projection/picking matching each pane's exact camera transform
     (view + projection matrices) - the underlying matrix math (gluLookAt/
     gluPerspective/glOrtho-equivalent) was verified correct via
     targeted numeric tests during this session, but the full picking
     implementation (projecting every visible instance to screen space,
     finding the closest one to a click within a pixel threshold, for
     all of Top/Side/Front/3D) wasn't completed - configurable movement
     settings (below) took priority as the more directly actionable
     item once a real, reported bug came up.

   - **Configurable per-viewport movement settings**: DONE - new
     "Viewport" tab in the settings dialog lets pan/rotate mouse button
     assignment and per-mode (Top/Side/Front/3D) pan-axis inversion be
     adjusted directly, rather than guessing at the correct sign
     blindly (this sandbox has no real GPU to visually verify camera
     behaviour against). Added in response to a real reported bug (Top
     view's left/right felt switched, other views' up/down felt
     switched) - defaults are unchanged/non-inverted; Keith can now
     tune each mode's feel directly through the UI. Applies immediately
     on save, no restart needed.

   - **IPL Sections eye icons**: FULLY DONE - went through several
     iterations before landing on something solid. Final approach:
     replaced the QPushButton toggle entirely with a plain icon on a
     QTableWidgetItem (toggled via cellClicked) - eliminates button
     chrome/padding as a source of unwanted space entirely, rather than
     continuing to fight it via size/stylesheet tweaks. Column order is
     icon-first, name-second (was reversed); hidden entries grey out
     their name text (had to blend the active text colour toward the
     background programmatically, since this app's QSS-based dark
     theme leaves QPalette's Disabled colour group reporting the same
     white as Active - a real Qt/stylesheet interaction quirk, not
     just picking the "wrong" role name); panel capped at 200px max
     width (was unconstrained, competing with World View/Object
     Browser for space). Also discovered and reverted a genuine
     regression from a parallel session's earlier work - the icon
     column had been changed back to Stretch to fix a *different*
     padding complaint, which reintroduced this one.

   - **Right-click Add/Remove Favourites**: DONE - available on the
     merged panel (see below), sharing the same favourite_objects
     setting throughout.

   - **Map Workshop SVG icon**: DONE - found while investigating that
     get_map_workshop_icon had never actually been implemented despite
     being referenced (wrapped in try/except, silently falling through
     to a generic fallback) in several places since early in the Map
     Editor work - added a proper folded-map icon matching the
     established COL/TXD Workshop icon style.

   - **Object Browser + Instance List merge**: DONE. Design decisions
     made (Keith said "continue" without answering the open questions
     from the previous write-up, so proceeded with documented,
     reasonable choices rather than blocking): the merged panel is
     Object Browser's UI (search + All/Most Used/Favourites/Generic),
     now showing Star/ID/Model/TXD/Instances per Keith's exact spec;
     the standalone Instance List dock is retired (code left in place,
     unused, not deleted, for reversibility). Since a row now
     represents a MODEL that may have zero, one, or many placements,
     selecting a row centres the camera + edit panel on that model's
     FIRST instance, with Prev/Next cycling (shown in the edit panel,
     hidden entirely for single-instance models) through the rest.
     Right-click Add/Remove Favourites added to the merged panel.

   - **Still not built**: Add/Delete/Rename SVG icons above the merged
     panel, and the adaptive row layout that collapses those icons plus
     the All/Most Used/Favourites/Generic filter buttons onto one row
     when there's enough width (icon-only when there isn't). Smaller,
     more mechanical pieces than the merge itself - worth picking up
     next.

   - **Add**: browse the desktop for new DFF/TXD/COL files and import
     them into the game's canonical archive - gta3.img for GTA3/VC/SA
     (matches the engine's own always-loaded archive, already tracked
     via GTAWorldLoader._inject_enforced_imgs); for SOL specifically
     (multi-city, no single canonical IMG) let the user choose which
     IMG file to target instead. Needs new IMG-writing capability
     (apps/methods/img_core_classes.py currently reads IMG archives;
     adding/writing entries into one is a separate piece of work) plus
     an ID-collision check against the loaded ID database before
     committing.

   - **Del**: user choice each time (needs a small confirm dialog) -
     remove just the IPL placement, or fully purge the object from
     IPL+IDE+COL+IMG and free the model_id for reuse.

   - **Rename**: propagates the new name across IDE, IPL (for GTA3/VC's
     text format, which redundantly stores the model name per
     placement - SA's format doesn't), COL, and the actual filenames
     inside the IMG archive. A true global rename, not per-instance.

   - **Undo/redo** across all of the above, with ID-safe restoration
     (undoing a delete should be able to restore the exact ID rather
     than risk it having been reused meanwhile).

   - **"Find a free ID" icon**: scan the current game's valid ID range
     (GTAGame.ID_RANGES already defines per-game bounds) against every
     currently-loaded ID and suggest the next unused one.

   - **"Shift IDs +2000/-2000" icon**: bulk-renumber selected entities'
     IDs by a fixed offset, for merging maps or avoiding collisions.
     Confirmed with Keith: this needs to renumber the whole chain
     consistently (the IDE object definition too, and every instance/
     reference using that ID) - not just the IPL instance's own
     reference, or model lookups would break.

   - **Rotate map sections (90°/180°/arbitrary degrees)** and **shift/
     rotate selected IPLs including linked vehicle paths, cull zones,
     and other connected .zon data**, with a per-operation checklist of
     what to include. Rotation pivot point (section origin? bounding-
     box centre? user-specified?) is still an open question - wasn't
     answered in the confirmation round, so default to something
     reasonable (bounding-box centre is the least surprising choice)
     and let Keith redirect if that's wrong once this is actually built.

1. **Real per-instance DFF/TXD geometry** (the big one) - replace
   MapViewport's placeholder marker cubes with actual model rendering:
   - Resolve each IPLInstance's model_name -> its DFF file, via the
     IMG(s) GTAWorldLoader's load_log already identified as involved
     (or from a user-selected/detected set of game IMGs) - reuse
     apps/methods/img_core_classes.py (IMGEntry) rather than writing
     new IMG-reading code.
   - Load geometry via apps/methods/dff_parser.py (load_dff) and
     textures via apps/methods/txd_parser.py (load_txd/parse_txd) -
     both already handle this well, including DXT decompression and
     multiple platforms.
   - Cache loaded geometry/textures by model/TXD name - many instances
     share the same model, don't reload per-instance.
   - Apply each instance's position + quaternion rotation (already
     parsed into IPLInstance) as the per-instance world transform when
     drawing - conceptually close to VehicleViewport's
     load_all_geometries, which already does per-part world transforms
     from a frame hierarchy; here the transform comes from the IPL
     instance instead.
   - Do this incrementally - start with just the FIRST few loaded
     instances rendering correctly before trying to handle a whole
     city's worth at once (performance/culling is a separate concern
     to tackle after correctness).
   - **VC-specific gotcha, confirmed against Keith's real default.dat/
     gta_vc.dat**: default.dat references some TXD/DFF files directly
     via TEXDICTION/MODELFILE directives (generic wheel/aircraft
     models+textures), not through an IMG archive at all - these are
     genuinely not present anywhere in gta_vc.dat itself, loaded only
     once at engine startup from default.dat before the main .dat.
     GTAWorldLoader now tracks these entries in load_log (previously
     silently dropped entirely - parsed but never surfaced), but
     doesn't load their actual TXD/DFF content yet - when building the
     real model-loading step above, these standalone-referenced files
     need their own resolution path (direct file load) alongside the
     IMG-archive-based one, or any model built from generic.txd/
     wheels.txd will silently fail to find its textures for VC.
   - **GTA3-specific gotcha, found and FIXED, confirmed against Keith's
     real gta3.dat**: GTA3 loads its zone file (MAP.ZON) via a
     different directive keyword entirely - MAPZONE, not IPL (VC/SA
     both use IPL DATA\MAP.ZON for the equivalent file). This was a
     genuine functional bug, not just a visibility gap: MAPZONE entries
     were parsed into dat.entries by the generic directive fallback but
     _process_dat only ever iterated entries with directive == "IPL",
     so MAP.ZON never got processed at all for GTA3 - self.zones would
     have silently stayed empty even with a valid MAP.ZON file present.
     Fixed by including MAPZONE alongside IPL in _process_dat's
     processing loop (both go through the same _load_ipl/IPLParser path,
     which already handles zone/cull sections correctly regardless of
     which directive pointed at the file) - load_log still tags these
     as "IPL" type (not "MAPZONE") for compatibility with DAT Browser's
     existing tree-building code, which specifically checks for
     entry_type == "IPL" at several points. Verified end-to-end with a
     synthetic MAP.ZON containing a real zone entry - correctly parsed
     into self.zones after the fix (was 0 zones, always, before it).

2. **2D top-down map view** - was in the original phased plan but got
   skipped over jumping straight to the 3D viewport. Ipl_Editor's
   existing IPLMapView (apps/components/Ipl_Editor/ipl_workshop.py)
   already has world<->screen coordinate transforms, grid drawing, and
   radar-image-as-background support - reuse/adapt rather than
   rebuilding. Useful as a lighter-weight, faster-to-navigate
   alternative to the 3D view once real geometry loading (item 1) makes
   the 3D view heavier.

3. **Object Browser deeper integration** (smaller follow-up) - rows
   currently only support favouriting; a natural next step is wiring
   row selection/double-click (once a non-favourite-toggle interaction
   is free) to filter Instance List / World View down to just that
   model's placements, and/or a right-click "jump to editor" shortcut
   once item 5 below (editor shortcuts) exists.

4. **Object palette - category tree** - Favourites and search are now
   covered by the Object Browser dock; what's left from the original
   requirements list is a category tree (lamps, posts, etc.). IDE files
   don't inherently carry categories, so this likely needs a curated
   name-pattern-based categorisation scheme (or a small user-editable
   mapping file) rather than anything derivable purely from parsed data.

5. **Editing + editor shortcuts** - actually moving/adding/deleting
   placements and writing back to the IPL (currently everything is
   read-only/view-only); "shortcuts to edit TXDs, COLs, or DFF files"
   - buttons that jump into Txd_Editor/Col_Editor/Model_Editor for
   the currently-selected instance's model, reusing the same
   open_txd_workshop_docked/open_col_workshop_docked/
   open_model_workshop_docked entry points DAT Browser already uses.

6. **COL collision loading** - apps/methods/col_core_classes.py/
   col_loader.py already parse this fully; not yet wired into the
   world view at all (no visualisation, no use in placement/collision
   checks).

7. Icon sets/styles (light theme, mono theme) - explicitly deferred
   earlier pending the UI work being further along; Model Workshop's
   existing per-ribbon context menu already has an "Icon Set" submenu
   pattern (Default/3ds Max style) worth referencing when this comes up.

## July 2026 - Next up: DP5 Workshop

### DP5 Workshop - ribbon rebuild + sidebar sectioning
**Status**: PLANNED
Apply the same treatment Model Workshop just went through:
- Keep all existing functionality intact - this is a UI/layout pass, not
  a feature change.
- Add proper ribbon areas (QToolBar-based, matching the Model/COL/TXD
  Workshop pattern - movable, floatable, collapsible via the dock-title
  double-click, adaptive icon-only when space is limited).
- Split the side panel into independent, dockable sections (same as
  Model Workshop's Files/Models/Frame Hierarchy/Textures split) instead
  of one combined panel.
- Note from the Model Workshop pass: the "dotted grip handle" splitter
  look (the old style, before the current flat separator) should ideally
  be restored consistently across all tools (DP5, COL, TXD, Model
  Workshop) and IMG Factory itself in one pass, rather than one-off per
  tool - hold off on that specific piece until it can be done cleanly
  everywhere at once.

## May 2026 - Model Workshop TODOs

### Model Workshop - Texture rendering on 3D model
**Status**: DONE (May 2026)
- DFFViewport now shared OpenGL renderer for all tools
- Texture wrap/filter flags parsed from DFF+TXD
- Suffix stripping handles `name_fehihwm` style texture names
- IDE DB used for exact TXD lookup before heuristics
- Game-version-aware shared TXD loading (VC/GTA3/SA)

### Model Workshop - Bleed-through when docked in IMG Factory
**Status**: CONFIRMED FIXED (May 2026)

## Future Projects

### Vehicle Editor (Model Workshop Extension)
**Status**: PLANNED
Vehicles have 16-24 geometries per DFF (body, 4 wheels, doors, bonnet, boot, extras).
Current model_workshop only renders geometry[0]. Full vehicle editor needs:

**Rendering:**
- Render ALL atomics composited in one viewport (chassis + all parts)
- Frame hierarchy: atomics link to frames via frame_index, frames have parent hierarchy
- Frame names currently not parsed (extension 0x0253F2FE / Frame Name plugin) — needed
  to identify chassis, wheel_lf, wheel_rf, wheel_lb, wheel_rb, door_lf, door_rf etc
- Colour remapping: vehicles use 2 paint job colours (primary/secondary from IDE)

**IDE vehicle fields (already parsed in gta_dat_parser.py):**
- handling_id, game_name, anim_file, veh_class, freq, level, wheel_model, wheel_scale
- Show these in the info panel alongside IDE/ID/TXD

**Handling.cfg editor:**
- Parse handling.cfg entries (one line per vehicle, 23 fields)
- Fields: mass, drag, centre_of_mass(x,y,z), multipliers, top_speed, etc
- Edit and save back to handling.cfg
- Could use Radar Workshop as layout base (has similar tabular editor pattern)

**Ped Editor:**
- peds.ide section: id, model, txd, pedType, behaviour, animGroup, carsDriveMask,
  animFile, radio1, radio2
- pedstats.dat for ped stats
- Simpler than vehicles — single geometry, no frame hierarchy needed

## Future Projects

### Path Editor
**Status**: PLANNED
GTA path nodes (.dat files - NODES, PEDGRP, CARGRP etc) visual editor.
- Load and display path nodes on a 2D/3D map overlay
- Edit node positions, connections, flags
- Import/export to GTA path dat format
- Integration with Map Editor for context
- Reference: IPL/IDE integration already in IMG Factory
- Related: Paths Map button already stubbed in gui_layout.py

### Map Editor
**Status**: EARLY STUB
`apps/components/Map_Editor/Map_Workshop.py` exists but is a placeholder.
`moomapper-090-src-r2_repacked` source is present for reference.
- Visual placement of IPL objects on the map
- IDE lookup for object names and models
- Import/export IPL sections (INST, ZONE, CULL etc)
- Integration with Model Workshop for DFF preview
- Radar overlay from Radar Workshop tiles (1296 tiles)
- Consider using existing moomapper as base

### Radar Workshop - Path overlay
**Status**: PLANNED
Radar Workshop (`radar_workshop.py` v20, 4612 lines) handles the 1296 radar
tile grid. Addition needed:
- Overlay path nodes on radar tile view
- Click tile to open corresponding area in Map Editor
- Export radar tiles as single composited image
- Mini-map mode for use within Model Workshop / Map Editor

---

## Model Workshop TODOs

### Model Workshop - Opened models should keep a tab/reopen affordance instead of closing automatically
**Status**: PENDING
**Priority**: Medium

Currently switching to a different model in the files list appears to just
replace the current one - no way back except reopening from scratch.

- [ ] Give each opened model its own tab, OR (Keith's preferred simpler
  alternative) show an "opened" SVG icon next to already-opened models in
  the files list, so clicking it swaps back to that model instead of the
  app silently discarding it
- [ ] Per-model close affordance - a small [x] next to the opened
  indicator/tab
- [ ] On close, if the model has unsaved changes, prompt with
  [Save] / [Cancel] / [Close] rather than silently discarding

### Model Workshop - Progressive lag/freeze when switching between many loaded models
**Status**: PENDING - partially investigated, root cause not confirmed
**Priority**: High (freezes the app)

Reported: lag builds up the more you switch between models (with many
loaded), eventually freezing the app. Keith's question was whether this is
a Python/memory issue from handling too much data.

Investigated (static code reading, not live profiling) in
apps/methods/dff_viewport.py and apps/components/Model_Editor/model_workshop.py:
- GL texture upload/cleanup (`_upload_textures`/`clear_textures`) is
  correctly paired (old textures deleted before new ones uploaded) - but
  that path only runs on TXD file load, not on model switch, so it's not
  a per-switch texture leak.
- `on_model_selected` -> `set_current_model` -> `load_geometry` only
  rebuilds plain Python lists (vertices/normals/UVs/triangles) each
  switch - no GL calls at all in that path.
- Found one confirmed inefficiency: both `model_list.model_selected` and
  `viewer_3d.model_selected` are connected to the same `on_model_selected`
  handler. That handler calls `model_list.setCurrentRow(...)` when the
  row doesn't match, which likely re-fires the list's own selection
  signal and re-enters the handler a second time (guarded against
  infinite recursion, but still doing the geometry/properties work twice
  per viewer-originated switch).
- Could not confirm or rule out an actual accumulating leak (old geometry
  arrays, list widget items, or cached references not being released)
  without live profiling - needs `tracemalloc` or RSS/VRAM monitoring
  while reproducing, next time this is picked up.

### Model Workshop - Stub buttons in viewport toolbar (do nothing)
**Status**: PENDING
The following buttons in the bottom viewport toolbar are stubs (setEnabled(False)
or connected to _export_not_implemented). Need implementing:
- flip_vert_btn / flip_horz_btn - flip DFF geometry vertically/horizontally
- rotate_cw_btn / rotate_ccw_btn - rotate DFF geometry 90 degrees
- analyze_btn - analyse mesh stats
- copy_btn / paste_btn - copy/paste surfaces
- delete_surface_btn / duplicate_surface_btn - surface operations
- paint_btn - vertex/face paint mode
- load_txd_btn - load TXD directly
- find_in_ide_btn - find entry in IDE
- prelight_apply_btn - apply prelight colours
- Export stubs: 3DS, CST, OBS, FBX

### Model Workshop - Texture rendering on 3D model
**Status**: PENDING
Textures load correctly from IMG (confirmed auto-loaded from generics.img),
texture list shows names and sizes, but textures do not render on the 3D mesh
in the viewport. The _load_txd_file path works, _mod_textures is populated,
but the COL3DViewport is not applying them to the DFF geometry surfaces.
Next session: check how textures are passed to the viewport and why
the textured render mode has no effect on DFF geometry.

### Model Workshop - Bleed-through when docked in IMG Factory
**Status**: PENDING - partially fixed
_apply_theme sets stylesheet on self + child panel palettes. Still showing
some bleed on certain themes. Investigate whether container QWidget in
open_model_workshop needs explicit background or whether the splitter
handle area is the remaining source.
# X-Seti - October22 2025 - IMG Factory 1.5 TODO List

# IMG Factory 1.5 - TODO List

## Completed Today - October 22, 2025



### ✅ Theme System Color Expansion - COMPLETED
**Status**: COMPLETE
**Completed**: October 22, 2025

**What Was Done**:
- [x] Added 5 new color variables to theme system:
  - button_pressed - Pressed button state color
  - selection_background - Selection highlight color for tables/trees
  - selection_text - Text color for selected items
  - table_row_even - Even row background color
  - table_row_odd - Odd row background color

- [x] Updated update_themes_script.py to add new colors to all themes
- [x] Updated 31 theme JSON files (5 needed updates, 26 already complete)
- [x] Created themes_backup/ with all original files

- [x] Updated utils/app_settings_system.py:
  - get_theme_colors() #vers 2 - Added fallback support
  - _get_hardcoded_defaults() #vers 1 - NEW METHOD
  - _generate_stylesheet() #vers 1 - NEW METHOD (shared)
  - get_stylesheet() #vers 4 - Both classes now use shared method

- [x] Created components/File_Browser/dolphin_dialog.py:
  - Complete custom file browser (Dolphin-style)
  - Replaces native Qt dialogs (fixes black row issue on light themes)
  - Full theme integration with IMG Factory themes
  - SVG icons only (no emojis)
  - Features: single/multi-select, create folder, rename, delete
  - Places sidebar with common locations
  - Project Folders sidebar (uses IMG Factory project paths)
  - File preview with system command integration (Linux/Mac/Windows)
  - 70+ methods, fully documented

**Impact**: MAJOR - Fixed dialog theming issues, added complete custom file browser

---

## High Priority

## Menu System Overhaul — Planned Rework

**Priority:** High
**Status:** Pending — temp fixes in place (Builds 319–335)

### Problem summary
Two competing menu systems exist and conflict depending on UI mode:

- **System UI mode** — `_system_menu_bar` (inline `QMenuBar` in top button row)
  works correctly. Tool injection via `ToolMenuMixin._inject_tool_menu()` adds
  to this bar and is visible.
- **Custom UI mode** — `menu_bar_system.menu_bar` points at the native
  `self.menuBar()` which is hard-clamped hidden. Tool menus injected there
  are never visible. Workaround: titlebar `[COL]/[DFF]/[TXD]/[DP5]` button
  pops up a `QMenu` built from the tool's `_build_menus_into_qmenu()`.
- **DP5 standalone** has its own internal `_menu_bar` / `_menu_bar_container`
  separate from the `ToolMenuMixin` system entirely.

### What the rework should achieve
1. **One menu system** — `menu_bar_system` owns all menus in all modes.
2. **Custom UI mode** — `[Menu]` button popup and titlebar tool buttons
   both pull from `menu_bar_system` directly. No hidden native bar involved.
3. **System UI mode** — inline `_system_menu_bar` as now (already works).
4. **All workshops** — `get_menu_title()` + `_build_menus_into_qmenu()` fully
   wired; tab switching injects/removes cleanly via `_update_tool_menu_for_tab`.
5. **DP5** — retire the internal `_menu_bar` / `_menu_bar_container`; use
   `ToolMenuMixin` path exclusively for both standalone and docked modes.
6. **Radar Workshop** — currently uses a stub `ToolMenuMixin` fallback class;
   needs real `get_menu_title()` + `_build_menus_into_qmenu()`.

### Files to touch
- `apps/gui/gui_menu.py` — `_inject_tool_menu`, `_remove_tool_menu`, popup path
- `apps/gui/tool_menu_mixin.py` — unify topbar/dropdown/popup into one flow
- `apps/gui/gui_layout_custom.py` — Menu button popup wired to `menu_bar_system`
- `apps/components/Img_Factory/imgfactory.py` — remove dual-bar logic
- `apps/components/DP5_Workshop/dp5_workshop.py` — retire internal menubar
- `apps/components/Radar_Editor/radar_workshop.py` — implement real menu methods
- All other workshops — review and confirm `_build_menus_into_qmenu` completeness

---


### 1. Custom Dialog Integration - NEW TASK
**Priority**: High
**Status**: Next Task

**Tasks**:
- [ ] Replace QFileDialog calls with DolphinFileDialog in IMG Factory main
- [ ] Replace QFileDialog calls in TXD Workshop
- [ ] Replace QFileDialog calls in COL Workshop
- [ ] Update all import/export functions to use DolphinFileDialog
- [ ] Test custom dialog on Linux
- [ ] Test custom dialog on Windows (if available)
- [ ] Test custom dialog on macOS (if available)

**Files to Update**:
- gui/gui_layout.py - Main IMG factory dialogs
- components/TXD_Editor/txd_workshop.py - TXD open/save dialogs
- components/Col_Editor/col_workshop.py - COL open/save dialogs
- methods/import.py - Import file dialogs
- methods/export.py - Export file dialogs
- Any other files using QFileDialog.getOpenFileName, etc.

**Impact**: High - Completes theme system integration

---

### 2. Theme Color Variable Updates - NEW TASK
**Priority**: Medium-High
**Status**: Pending

**Tasks**:
- [ ] Update TXD Workshop to use new color variables (button_pressed, selection_*, table_row_*)
- [ ] Update COL Workshop to use new color variables
- [ ] Update IMG Factory main GUI to use new color variables
- [ ] Update any remaining stylesheets with hardcoded colors
- [ ] Test all themes (light/dark) with new color variables

**Impact**: Medium-High - Ensures consistent theming across all tools

---

### 3. Tab System Issues
**Issue**: Multiple tabs open (IMG/COL), export_via.py, export.py and dump.py functions can't see the current selected tab.

IDE import error: name 'get_current_file_from_active_tab' is not defined
**Tasks**:
- [ ] Fix tab detection in export functions
- [ ] Fix tab detection in dump functions
- [ ] Fix tab detection in export_via functions
- [ ] Add active tab tracking system
- [ ] Test with multiple IMG files open
- [ ] Test with COL files in different tabs

**Impact**: High - Affects core functionality

---

### 4. Export Function Issues
**Issue**: export.py functions export all files combined!! 12Mb file for each, should be single files.

**Problem**: Selecting 7 entries gives 7 combined files instead of 7 separate files.

**Tasks**:
- [ ] Fix export.py to export selected entries individually
- [ ] Add option to combine files if user wants
- [ ] Add "Export as single file" option
- [ ] Add "Export as separate files" option (default)
- [ ] Update export dialog with clear options
- [ ] Test with various file counts

**Expected Behavior**:
- Select 7 entries → Export 7 separate files
- Optional: "Combine into single COL" checkbox

**Impact**: High - Core export functionality

---

### 5. Dump Function Logic
**Issue**: dump should follow same logic as export - single or combined files.

**Tasks**:
- [ ] Update dump.py with same logic as export fix
- [ ] Add "Dump as single file" option
- [ ] Add "Dump as separate files" option (default)
- [ ] Match export.py behavior for consistency
- [ ] Update dump dialog

**Impact**: Medium-High - Consistency issue

---

### 6. COL Dialog Theme Issues
**Issue**: Background box on COL dialog is hardcoded, dark themes can't see text.

**Tasks**:
- [ ] Remove hardcoded background colors
- [ ] Connect COL dialogs to theme system
- [ ] Test with all themes (light/dark)
- [ ] Check text contrast in dark themes
- [ ] Update all COL-related dialogs
- [ ] Add theme change detection

**Files to Fix**:
- COL dialogs in components/
- COL analysis dialogs
- COL editor dialogs

**Impact**: Medium - Usability with dark themes

---

## Medium Priority

### 7. Import System Improvements

#### 7a. Import via IDE
**Status**: Partly Fixed - Aug7
**Issue**: import_via ide gives an error, no .ide file found or no files in .ide.

**Tasks**:
- [ ] Better error messages for missing IDE files
- [ ] Better error messages for empty IDE files
- [ ] Validate IDE file before import
- [ ] Show IDE file contents preview
- [ ] Add IDE file format validation

---

#### 7b. Folder Import Options
**Status**: TODO
**Request**: Add options for folder contents import.

**Tasks**:
- [ ] Create folder import dialog
- [ ] Add file type filters
- [ ] Add recursive/non-recursive option
- [ ] Add file preview list
- [ ] Add size estimation
- [ ] Add import order options
- [ ] Test with large folders

---

#### 7c. Text File List Import
**Status**: TODO
**Request**: Add import via textfile.txt list - modelname.dff, texturename.txd in any order.

**Tasks**:
- [ ] Create smart text file parser
- [ ] Auto-detect file types from extensions
- [ ] Handle mixed file types
- [ ] Handle paths (relative/absolute)
- [ ] Handle missing files gracefully
- [ ] Add validation before import
- [ ] Show import preview

**Example Input**:
```
vehicle.dff
vehicle.txd
wheel.dff
interior.dff
texture_pack.txd
```

**Function Requirements**:
- Smart enough to understand file contents
- No specific order required
- Mixed file types supported
- Skip missing files with warning

---

### 8. Drag and Drop Support
**Status**: TODO
**Request**: Drag and Drop files/folders onto imgfactory app to import.

**Tasks**:
- [ ] Enable drag-drop on main window
- [ ] Handle single file drops
- [ ] Handle multiple file drops
- [ ] Handle folder drops
- [ ] Show drop preview overlay
- [ ] Confirm before import
- [ ] Show progress during import
- [ ] Support all file types (DFF, TXD, COL, etc.)

**Impact**: Medium - Nice UX improvement

---

### 9. File Highlighting Issues
**Status**: TODO
**Issue**: Highlighting shows "28/28 files" when 10 already existed.

**Tasks**:
- [ ] Fix duplicate detection logic
- [ ] Show accurate "new vs existing" count
- [ ] Highlight only truly new files
- [ ] Update status message accuracy
- [ ] Test with various scenarios

**Expected**:
- Import 28 files, 10 exist → Show "18 new, 10 skipped"
- Highlight only the 18 new files

**Impact**: Low-Medium - Accuracy issue

---

### 10. Save Entry Function
**Status**: TODO
**Issue**: Fix the Save Entry function.

**Tasks**:
- [ ] Identify current Save Entry issues
- [ ] Fix save functionality
- [ ] Test with various file types
- [ ] Add error handling
- [ ] Add success feedback
- [ ] Update documentation

**Impact**: Medium - Important feature

---

### 11. Theme Switching
**Status**: TODO
**Request**: Theme switching from first page.

**Tasks**:
- [ ] Add theme selector to main page/toolbar
- [ ] Quick theme dropdown menu
- [ ] Show theme preview
- [ ] Apply theme immediately
- [ ] Remember theme selection
- [ ] Add keyboard shortcut

**Impact**: Low - Convenience feature

---

## Low Priority

### 12. Code Organization
**Status**: Planning
**Note**: Some files in components are shared functions, like img_core_classes, col_core_classes.

**Planned Split**:
- [ ] methods/img_entry_operations.py - Entry management (add, remove, get)
- [ ] methods/img_file_operations.py - File I/O operations  
- [ ] methods/img_detection.py - RW version detection
- [ ] methods/img_validation.py - File validation

**Important**: Before creating these files, check existing functions in methods/ to avoid duplicates.

**Impact**: Low - Code organization

---

## Future Features

### 13. DFF Texture to COL Material Mapping
**Status**: Idea Documented
**Priority**: Medium-High (when COL viewer is stable)

See: `components/col_viewer/TODO_DFF_TEXTURE_MAPPING.md`

**Summary**:
- Read DFF texture names
- Map to COL material IDs
- Auto-assign materials based on textures
- Visual validation of material assignments

**Estimated Time**: 2-3 weeks
**Dependencies**: COL viewer (✅ Complete)

---

### 14. Advanced COL Viewer Features
**Status**: Future Enhancement

**Possible Additions**:
- [ ] Color faces by material group
- [ ] Filter by material type
- [ ] Material statistics panel
- [ ] Export screenshot
- [ ] Measurement tools
- [ ] Wireframe/solid toggle
- [ ] Multiple model support
- [ ] Animation/rotation controls
- [ ] Lighting controls
- [ ] Export to OBJ format

---

### 15. Batch Processing Improvements
**Status**: Future Enhancement

**Ideas**:
- [ ] Batch COL material assignment
- [ ] Batch file validation
- [ ] Batch format conversion
- [ ] Progress reporting
- [ ] Error logging
- [ ] Undo/redo support

---

## Completed Tasks - October 22, 2025

### ✅ Custom File Browser (Dolphin Dialog)
- Created complete custom file browser system
- Fixed black rows in dialogs on light themes
- Full theme integration
- Project folder integration
- 70+ methods with full SVG icon support

### ✅ Theme System Color Expansion
- Added 5 new color variables
- Updated all 31 theme files
- Updated app_settings_system.py with fallback support
- Created shared stylesheet generator

See `ChangeLog.md` for complete history of all fixed issues.

---

## Priority Legend

- 🔴 **High Priority** - Affects core functionality, needs immediate attention
- 🟡 **Medium Priority** - Important features, should be addressed soon
- 🟢 **Low Priority** - Nice to have, quality of life improvements
- 🔵 **Future** - Long-term enhancements, not blocking

---

## Task Assignment

When working on tasks:
1. ✅ Check for duplicate functions first
2. ✅ Follow naming conventions (no "Enhanced", "Fixed", etc.)
3. ✅ Keep files under 90k
4. ✅ Update version numbers in methods
5. ✅ Add proper headers to all files
6. ✅ Test thoroughly before marking complete
7. ✅ Update this TODO when completing tasks
8. ✅ Move completed items to ChangeLog.md

---

## Notes

- **No Patch Files** - Fix issues properly, not with patches
- **No Duplicates** - Check existing functions before creating new ones
- **Clean Code** - No fallback code, works or doesn't work
- **Proper Naming** - Simple, clear filenames
- **Documentation** - Keep docs updated
- **No Emojis** - Use SVG icons only in code

---

**Last Updated**: October 22, 2025 - 23:45
**Active Tasks**: 15 high/medium priority items
**Future Features**: 3 documented ideas
**Completed Today**: 2 major tasks (Custom File Browser + Theme System Expansion)

---

### 16. DFF Texture List — right-click on DFF entry
**Priority**: High
**Status**: In Progress

- [ ] apps/core/dff_texlist.py — RW chunk walker (Texture 0x0006 > String 0x0002)
- [ ] apps/gui/dff_texlist_dialog.py — table: name | in IMG | TXD on disk
- [ ] Wire into apps/core/right_click_actions.py — "Show Texture List" for .dff
- [ ] Delete apps/components/Img_Factory/depends/comprehensive.py after migration

---

### 17. TXD Workshop — Build TXD from DFF scan
**Priority**: Medium-High
**Status**: Planned

- [ ] Scan DFF for texture names via RW chunk walk
- [ ] User picks folder of PNG/BMP/TGA files matched by name
- [ ] Build new TXD from found images
- [ ] Add to TXD Workshop as "Build TXD from DFF..."

---

### 18. Model Workshop — new component
**Priority**: Medium
**Status**: Planned — base on Col_Workshop UI framework
**Location**: apps/components/Model_Editor/model_workshop.py

- [ ] Load/view/edit DFF, MDL, NIF
- [ ] Texture mapping (links to TXD Workshop)
- [ ] Prelighting — vertex colour editing
- [ ] LOD generation: Largebuilding.dff → LODgebuilding.dff
- [ ] Export DFF and other formats

---

### 19. COL Workshop — Generate COL from DFF
**Priority**: Medium
**Status**: Planned — depends on Model Workshop DFF parser

- [ ] Read material/surface flags from DFF faces
- [ ] Map texture names to GTA surface types (GTA3/VC ~16, SA 180+)
- [ ] Grass texture on model → grass surface in COL
- [ ] Concrete, mud, stone, pavement etc.
- [ ] Generate COL mesh from DFF geometry
- [ ] Create LOD COL alongside LOD model

---

**Last Updated**: March 2026

---

## Post-session TODO (after Builds 162–196)

### COL Workshop
- [ ] Paint toolbar: verify overlay positioning on all screen sizes / dock states
- [ ] Paint toolbar: keyboard shortcut to switch tools (P=paint, D=dropper, F=fill)
- [ ] Material editor: show material colour in the "Apply to Selection" confirmation
- [ ] `export_model` / `import_elements` module-level helpers (currently print stubs)
- [ ] COL3DViewport: vertex selection mode (currently only face selection)

### TXD Workshop  
- [ ] Bumpmap preview ("Preview coming soon" label still present)
- [ ] DXT re-compression on save (currently stores RGBA, notes "compression applied on save")

### IMG Factory
- [ ] Future editors still stubbed: DFF, IPF, IPL, DAT, zones, weapons, vehicles, radar, paths, water

### PyInstaller
- [ ] Test `imgfactory.spec` on Windows — may need platform-specific binary paths
- [ ] Icon file: ensure `assets/icon.ico` exists for Windows build


## Vehicle Workshop bug day list (May 16 2026)
- [ ] Double window chrome — inner workshop + outer tab frame both showing min/max/close
- [ ] "Car Mods (SA)" tab label — should be "Car Mods" for GTA3/VC
- [ ] Edit buttons text truncated — need more width or icon-only below threshold
- [ ] generic.txd loaded unnecessarily for GTA3/VC — only load for vehicle-relevant textures
- [ ] First model load timing — GL context not ready, textures occasionally miss
- [ ] Right-click "Open in TXD Workshop" sometimes dead in IMG entries table
- [ ] Vehicle Workshop standalone open workflow — needs cleaner DAT Browser integration
- [ ] _handling_loaded flag reset needed when switching game/IMG
- [ ] VC: animFile field in default.ide cars section — currently parsed but not used
- [ ] Wheel steer slider not wired to actual wheel rotation in viewport
- [ ] CoM/Dummy/Suspension/Seats/Bounds edit buttons — overlay rendering not yet implemented

## Vehicle Workshop — Wheel Editor (planned)
- [ ] Wheel position offset (X/Y/Z per dummy) — edit dummy frame position in DFF
- [ ] Wheel track width (left/right spacing) — scale X offset of wheel_lf/wheel_rf dummies
- [ ] Wheel size override (front/rear scale sliders independent of IDE values)
- [ ] Wheel type picker (dropdown showing all types from wheels.DFF with preview)
- [ ] Live preview — sliders update viewport in real time
- [ ] Write back to vehicles.ide / default.ide on save
- [ ] Write back to DFF frame positions for dummy offsets

## Vehicle Workshop — resolved May 17 2026
- [x] SA vehicles.ide wheel scale not read — vehicles.ide overwritten by default.ide
- [x] Frame tree checkboxes unresponsive — missing ItemIsEnabled flag
- [x] Camera resets on frame visibility toggle
- [x] Handling % $ ! entries showing in list (SA boats/planes/bikes)
- [x] carcols.dat SA format not parsed
- [x] Wheel size slider range too small (±10%)
- [x] sentinel.txd loading 4x per vehicle switch
- [x] Streaming suffix in DFF list display names

## Next Session Options (choose one)

**A: Vehicle Workshop editing**
- Build vehicles from primitive mesh
- Move single polygons to shape vehicle
- Wheel position editor (move dummy frames)
- Create/edit vehicle parts: doors, bonnet, boot
- Tie into DFF frame export

**B: Model Workshop editing functions**
- Polygon/vertex editing in model_workshop.py
- UV editing
- Material/texture assignment

**C: Paths Workshop**
- Repo: https://github.com/X-Seti/Paths-Workshop
- GTA path node editor (car/ped nodes)

**D: Zon Workshop**
- Repo: https://github.com/X-Seti/Zon-Workshop
- Zone/cullzone editor
- Files: Cullzone.dat, Cullzoneempty.dat, info.zon, map.zon, navig.zon

**E: Peds Workshop**
- LC data: fistfite.dat, ped.dat, pedgrp.dat, pedstats.dat, IPF animation editor
- VC data: fistfite.dat, ped.dat, pedgrp.dat, pedstats.dat, IPF animation editor
- SA data: animgrp.dat, clothes.dat, ped.dat, peds.ide, pedgrp.dat, pedstats.dat, IPF animation editor

**F: Resident Evil Workshop (change of pace)**
- Repo: https://github.com/X-Seti/OG-Res-Bio-Evil-Workshop
