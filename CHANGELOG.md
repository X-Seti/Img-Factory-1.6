
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
