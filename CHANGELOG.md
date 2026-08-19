
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
