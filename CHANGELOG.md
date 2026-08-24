
- **Aug 19, 2026** — Added Game Path Presets, per Keith: "I'd like to
  add, in [Menu] Project settings -> Game path presets, the locations
  of those games installed on your system, so you can pick them in
  Dat Browser for quick access." GTA III/Vice City/San Andreas only -
  "lets ignore GTASOL for now" means GTASOL is genuinely never one of
  the preset keys at all, not merely hidden from a list that still
  has room for it.

  Investigated the existing DAT Browser first rather than assuming
  what needed building: `detect_game()`/`find_dat_file()` already
  correctly handle all three games' own main DAT filenames (gta3.dat/
  gta_vc.dat/gta.dat) - Keith's own framing ("We know GTAIII uses
  gta3.dat and automatically load the dat file...") was confirming
  already-working behaviour as context for the new feature, not
  describing a gap to fix. What was actually missing was a quick-
  access layer on top - storing each game's install path once instead
  of browsing to it fresh every session.

  New `get_game_path_presets`/`set_game_path_preset` (apps/gui/
  file_menu_integration.py), stored in the same `QSettings("IMG
  Factory", "Project Settings")` group every other project setting on
  that page already uses, rather than a new, separate storage
  mechanism. `set_game_path_preset` only ever stores a known key
  ('GTA3'/'VC'/'SA') - an unrecognised key is silently ignored, same
  "only known keys get stored" convention `MapSettings.set()` already
  uses in Map Workshop, for the same reason: a typo shouldn't create
  a permanent settings entry nothing will ever read back.

  New "Game Path Presets" group added to the existing Project
  Settings dialog (`handle_project_settings`), between the existing
  Override Settings and Quick Actions groups - one row per game, a
  read-only path field plus its own Browse... button, saving directly
  through `set_game_path_preset` the moment a folder's picked.

  New "Presets ▾" button in the DAT Browser's own toolbar, right next
  to the existing Game selector. Reads whatever's actually been saved
  and shows it as a small dropdown menu; picking one populates `_path_
  edit` and the Game combo exactly the way `_browse_game_root` already
  does after a manual folder pick, then auto-loads if that preference
  is already on - reuses that exact, already-established behaviour
  rather than a second, parallel reaction path. With nothing saved
  yet, offers a direct shortcut to open Project Settings instead of
  just showing an empty, unhelpful menu.

  Verified the storage logic directly (no real `QSettings`/Qt app
  available in this sandbox, mirrored with an in-memory substitute):
  empty-state returns nothing, partial presets (some games set, some
  not) return correctly, GTASOL is confirmed to never become a
  storable key regardless of what's passed in, and overwriting an
  existing preset correctly updates it. `ast.parse` clean on all
  three touched files; confirmed via AST no duplicate method
  definitions.

- **Aug 20, 2026** — Tackled all three of Keith's own real TODO
  comments left in the code, then wrote them up here: merged Game
  Path Presets into the DAT Browser's own Game combo, built real
  text-IPL-to-binary-IPL conversion, and made the directory tree
  browser lazy-load.

  **Game combo IS the presets now** (`dat_browser.py`), per Keith's
  own comment: "\"GTA III\", \"Vice City\", \"San Andreas\", should be
  the presets. so merge the presets into the game_combo." Selecting
  GTA III/Vice City/San Andreas directly from the Game dropdown now
  checks Menu > Project Settings > Game Path Presets and, if a path's
  been saved for that game, fills it straight into the path field and
  starts loading - no separate button needed any more. Removed the
  "Presets ▾" button and its own menu handler entirely, per Keith's
  own follow-up note ("this can go, once merged into game_combo"),
  not just hidden. **Caught a real bug while wiring this up**:
  `_browse_game_root` was setting the combo programmatically (after
  detecting a game from a real, manually-browsed folder) without
  blocking signals - my own new preset-population logic would have
  fired on that programmatic change and silently overwritten the
  real, just-browsed path with a stale preset path instead. Fixed by
  blocking signals around that one `setCurrentIndex` call, the same
  pattern the existing Dir-Tree handling in the same method already
  used for the identical reason.

  **Text IPL → Binary IPL conversion is real now** (`map_workshop.py`
  + new `write_binary_ipl_inst_only` in `gta_dat_parser.py`), per
  Keith's own comment: "When working with SA files, have the ability
  to click on a text ipl, convert to binary.ipl. save options, save
  to img file, save to desktop." The menu item calling `_save_ipl_
  data_as_binary` already existed in the code - the method itself
  didn't, so clicking it would have thrown an unhandled error. This
  pass covers "save to desktop" (a plain save dialog); saving
  directly into an open IMG archive is real, separate scope (needs
  IMG write support for adding a brand new entry, which this app
  doesn't currently expose) not attempted here.

  Built directly from `BinaryIPLParser`'s own already-confirmed read-
  side structure - same magic, header size, 40-byte inst record
  layout. Instances only: real binary IPL only ever supports `inst`
  and `cars` sections at all (confirmed via research earlier this
  session) - every other section (cull/zone/path/occl/grge/enex)
  genuinely has no binary representation to convert to, and `cars`
  isn't parsed by this app on the read side either, so there's
  nothing loaded to write for it.

  Real, honest limitation put directly in the confirmation dialog
  itself, not just a code comment: only 2 of the binary header's 18
  fields are actually confirmed from documentation; the other 16 are
  written as 0 here - the most conservative choice available, not a
  verified-correct one. Verified what CAN be verified without a real
  game to test against: wrote synthetic instances (including a real,
  non-identity 45-degree rotation quaternion and negative
  coordinates), round-tripped the result back through this app's own
  already-trusted `BinaryIPLParser`, and confirmed an exact match on
  every single field (model_id, full position, full quaternion, lod_
  index) - plus the empty-instance-list edge case producing a
  correct, valid 76-byte header-only file. That proves the writer's
  own output is internally correct and self-consistent; it does NOT
  prove a real, unmodified game would accept the whole file without
  incident, since the file has never been tested in an actual running
  game - the dialog says so plainly, not just this changelog entry.

  **Directory tree browser now lazy-loads** (`directory_tree_browser
  .py`), per Keith's own comment: "The folder list folders need to be
  closed, until the folder is opened to show it's contents." Was
  eagerly recursing 3 whole levels deep on every single load before
  this - for a large real game install's own directory tree, that
  could mean walking and building thousands of tree items the person
  may never even look at, every time a root path was set. `populate_
  tree`/`populate_tree_recursive` now only ever populate one level;
  new `_add_lazy_placeholder` gives any folder with real contents a
  single, cheap placeholder child (via `os.scandir`'s own iterator,
  just checking whether at least one entry exists rather than
  building a full list) so its expand arrow shows without actually
  reading its contents yet. New `_on_tree_item_expanded`, wired to
  both the main tree AND the second (twin-panel) tree - the second
  panel reuses `populate_tree`/`populate_tree_recursive` via a
  temporary `self.tree` swap already in place, but needed its OWN
  `itemExpanded` connection or its own placeholders would have sat
  there forever, never actually populating.

  **Caught and fixed a real, live bug introduced mid-edit**: an
  intermediate version of this same change left one old recursive
  call (`self.populate_tree_recursive(tree_item, item_path, max_depth,
  current_depth + 1)`) referencing `max_depth`/`current_depth` after
  those parameters had already been removed from the method's own
  signature - a `NameError` waiting to happen the moment any real
  folder was actually populated. Caught by the routine `ast.parse`+
  duplicate-definition check immediately after, before being
  presented as finished, and confirmed via direct search afterward
  that zero references to either stale parameter name remained
  anywhere in the file.

  Verified the core lazy-loading logic directly against a real,
  populated directory tree (not just reasoning about it): the "does
  this folder have real contents" check correctly matched real entry
  counts across several real subfolders, correctly returned false for
  a genuinely empty directory and a nonexistent one (no crash), and
  the placeholder-detection logic (used to decide whether an expanded
  folder still needs populating) was checked against 4 real cases
  including the specific edge case of a folder with exactly one REAL
  child rather than a placeholder - confirming the dedicated marker
  flag, not just a child-count-equals-one check, correctly tells the
  two apart.

  The "different View options" half of Keith's own directory-tree
  comment (icons/detailed-list/short-list view modes, "replicating a
  proper file browser") is real, separate scope not started this
  pass.

  `ast.parse` clean on all three touched files; confirmed via AST no
  duplicate method definitions in any of them.

- **Aug 20, 2026** — Consolidated IMG Factory's own config files into
  an app-folder-relative `config/` subfolder, per Keith: "lets fix map
  workshop and img factory first since we're working on those" -
  same real fix, same reasoning, already applied to Map/Model
  Workshop just before this.

  New `apps/components/Img_Factory/config/`. `IMGFactorySettings`
  (`apps/methods/img_factory_settings.py`) - its own JSON settings
  (last project/game root, fonts, window mode, panel thresholds,
  etc.) - now writes there via new `_img_factory_config_dir()`, the
  same shared-helper approach already used for Map Workshop, adapted
  for the fact this settings class lives in a shared `apps/methods/`
  module rather than directly inside the one app file it configures -
  navigates from this module's own known location up to `apps/` then
  down into `components/Img_Factory/config/`. Same real-write-test
  fallback to `~/.config/img-factory` if that folder genuinely isn't
  writable.

  **Real bug found and fixed along the way**: `IMGFactorySettings`
  had `def set(...)` defined TWICE (once untyped, once with a type
  hint) - the second silently shadowed the first, dead duplicate code
  doing the exact same thing. Removed the redundant one.

  **Made the write atomic**, same real reasoning/fix as `MapSettings`
  earlier today - was a direct `write_text` with no protection
  against a torn write; a crash mid-save could have silently
  corrupted this file the same way, reverting every IMG Factory
  setting at once on the next launch. Now temp file + `os.replace`.

  **Consolidated 4 separate `QSettings(...)` call sites in
  `imgfactory.py`** (window geometry/splitter state via `QSettings(
  "XSeti", "IMGFactory")`, `game_root` via `QSettings("IMG-Factory",
  "IMG-Factory")` - two different native-location (org, app) pairs,
  each creating its own separate `~/.config` subfolder on top of
  several OTHER differently-named ones already found elsewhere in
  this app) into one shared, app-folder-relative `.ini` file via new
  `get_img_factory_qsettings()` - `QSettings.Format.IniFormat` with
  an explicit file path, which is what actually makes this app-
  folder-relative rather than just picking yet another differently-
  worded (org, app) pair. The two groups' own keys never collide
  (`game_root` vs `geometry`/`splitter_state`), so one shared file is
  correct, not a risk.

  Verified directly: path resolution (confirms it lands in `apps/
  components/Img_Factory/config/`), a full save/load round-trip
  through a fresh `IMGFactorySettings` instance simulating a real
  restart, no leftover `.tmp` file after a successful save, `ast.
  parse` + a deep `py_compile` pass on both touched files, and a
  direct search confirming zero remaining old-style `QSettings(...)`
  call sites in `imgfactory.py` outside the explanatory comments.
  `.gitignore` updated for the new `config/` folder.

  **Scope note, not yet done**: the wider, app-wide audit (several
  OTHER differently-named `~/.config/imgfactory*` folders still found
  in `notepad.py`, `open.py`, `file_menu_integration.py`, `file_
  dirtree_browser.py`, `directory_tree_browser.py`, plus 3 stale
  duplicate copies of `img_factory_settings.py` in `Map_Editor`/
  `Model_Editor`/`Col_Editor`'s own `depends/` folders) - Keith's own
  explicit plan is Map Workshop + IMG Factory first (this pass), then
  a second audit pass to convert the remaining apps over, alongside
  whatever other layout/bug/data-saving work still needs doing in
  each.

- **Aug 20, 2026** — Fixed two real bugs in Water Workshop, per Keith:
  "many issues with Waterpro, first the titlebar should not be
  showing when docked, the other is the parsing of SOL waterpro
  files, you can see this in the image."

  **SOL waterpro.dat parsing** - real, confirmed fix using two real
  uploaded SOL sample files (previously flagged as an honest, unfixed
  gap since only vanilla sample data existed). New `_detile_sol_grid()`
  in `gta_dat_parser.py` - SOL's own real grid data is genuinely
  subdivided into 6x6 separate tiles, each stored as its own
  contiguous block, rather than one flat, row-major grid across the
  whole map the way vanilla SA/VC/III data actually is; `parse_
  waterpro_dat` now detects this (grid_width evenly divisible by 6 -
  vanilla SA/VC/III's own real grid_width of 64 never is) and de-
  tiles both grids using the exact same real logic `water_workshop.
  py`'s own reference tool already uses correctly in its DISPLAY code,
  just never in its own file-parsing code until now.

  Verified directly against both real uploaded SOL files: grid_width
  (384) and all 4 real water level heights (6.0, 68.5435, 46.0, 14.5)
  exactly match the real screenshot Keith provided. Rendered the de-
  tiled visible/physical grids as real images and confirmed directly
  by eye - a coherent, real map-shaped landmass, not the striped/
  banded, scrambled pattern the real screenshot showed before this
  fix. Confirmed via a regression test that a real vanilla (non-SOL,
  grid_width=64) file is correctly left untouched - 64 isn't evenly
  divisible by 6, so `is_tiled` correctly stays False and the
  existing, already-correct flat-grid reading still applies.

  **Titlebar showing when docked** - a real, confirmed bug directly
  visible in Keith's own screenshot: Water Workshop, docked inside
  IMG Factory, still showing its own full internal titlebar (Menu/
  Settings/title text/undo/info/settings/D) duplicating the outer
  tab's own title. Traced to the shared `_create_toolbar` method -
  builds the toolbar/titlebar unconditionally regardless of docked
  state, despite `self.standalone_mode`/`self.is_docked` already
  existing and already being used for other, smaller decisions in
  that same method (which controls - minimize/maximize/close vs a
  dock/undock button - show at the toolbar's own right end). Added
  `self.toolbar.setVisible(self.standalone_mode)` right before the
  method's own return.

  Real, important discovery made while tracing this: this app's own
  workshop tools deliberately do NOT share one central `GUIWorkshop`
  base class import - `apps/methods/gui_workshop.py` is explicitly
  labelled a TEMPLATE ("DO NOT IMPORT THIS FILE INTO YOUR WORKSHOP...
  Each workshop MUST be standalone and self-contained"), meant to be
  copied into each tool's own folder and maintained independently.
  Water Workshop's own real copy is `apps/components/Water_Editor/
  gui_workshop.py`, not the methods/ one - the actual fix was applied
  there, matching where Keith's own real bug report is. A separate,
  genuinely stale, unused duplicate was also found at `apps/
  components/Water_Editor/depends/gui_workshop.py` (an older Aug 14
  copy, confirmed via direct search that nothing anywhere imports it)
  - noted for a future cleanup pass, not touched now since it's
  already dead code, not the source of this real bug.

  Real, honest scope note: given the deliberate "each workshop owns
  its own copy" design just confirmed, this same titlebar-when-docked
  bug likely exists identically in every other `*_Workshop` tool's
  own copy of this same method too - not fixed here, since Keith's
  own report was specifically about Water Workshop; extending this
  same fix to the other tools would be its own, separate, deliberate
  pass rather than something to guess at doing silently here.

  `ast.parse` clean on both touched files; confirmed via AST no
  duplicate method/function definitions.

- **Aug 20, 2026** — Fixed two real bugs Keith reported directly.

  **MapSettings save crash** - "Failed to save .../map_workshop.json:
  name 'json' is not defined." A real, genuine oversight from this
  session's own earlier atomic-write fix: `MapSettings._load`/
  `_save_now` (`map_workshop.py`) both use `json.dumps`/`json.loads`
  but neither has its own local `import json`, and this module never
  had one at the top level either (only scattered, method-local
  imports elsewhere in the same file) - settings could never actually
  save at all until this was fixed. Added `import json` at the real
  module level rather than another method-local import, since this
  eliminates the whole class of bug for every current and future
  method in this file, not just these two. Confirmed via direct AST
  inspection that it's a genuine top-level statement (not nested
  inside any function), positioned near the very top of the file, and
  that no local variable anywhere in the file shadows the name within
  `MapSettings`'s own method scopes.

  **Radar Workshop's own titlebar showing when docked** - the same
  real complaint already fixed for Water Workshop, but genuinely
  different here, not the same fix copy-pasted: Radar Workshop's own
  toolbar carries real, functional controls (the Game selector, cols/
  rows spin boxes) that aren't duplicated anywhere else in that
  tool's own UI (confirmed via direct search) - hiding the whole
  toolbar frame the way Water Workshop's own fix did would make those
  genuinely inaccessible while docked, a real regression rather than
  just a cosmetic fix. Hid only the window-chrome-style elements that
  actually duplicate the outer tab's own title/controls (Menu,
  Settings, Undo, Info, Theme/Properties - two of which were
  explicitly hardcoded "show in both standalone and docked" before
  this), leaving the functional Game/cols/rows controls untouched and
  visible either way.

  Caught and fixed a second, real gap in the same pass: the visibility
  check for these 5 elements only ran once, at the tool's own initial
  construction - `_dock_to_main`/`_undock_from_main` (the real,
  already-existing dock/undock toggle methods) never touched them at
  all, so dynamically toggling dock state after the tool was already
  open would have left them stuck at whatever visibility they started
  with, not actually reacting to the real, current dock state the way
  the rest of those same methods already correctly do for the other
  toolbar elements. Added the matching hide/show calls to both
  transition methods so this now works correctly for genuine runtime
  toggling too, not just the tool's own first-load state.

  `ast.parse` clean on both touched files; confirmed via AST no
  duplicate method definitions.
