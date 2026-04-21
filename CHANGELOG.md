# IMG Factory 1.6 — Changelog

## Build 317  (current)

### Model Workshop
- **DFF toolbar context-switching** — when a DFF is loaded the left toolbar now
  hides COL-only buttons (flip, rotate, analyze, copy, paste, create/delete/duplicate)
  and shows DFF-mode buttons only. COL buttons are fully hidden, not just greyed.
- **Shadow Mesh → Prelighting** — bottom-right panel row replaced:
  - Removed: Shadow Mesh View / Create / Remove buttons
  - Added: **Prelight: [Apply] [Setup…]** row
  - Setup dialog configures ambient colour (RGB), sun direction (XYZ), sun intensity
  - Apply stub ready for full vertex-colour baking (next session)
- **Prelighting stubs** — `_apply_prelighting()` and `_prelight_setup_dialog()` added
- **Toolbar position saving** — timing improved (100ms → 400ms + `showEvent` hook)
  so dockable toolbar positions reliably restore on startup
- **SVG icons** for V/E/F/P select mode buttons, front-paint, backface, primitive
- **Create COL from DFF** — generates COL1/2/3 binary from DFF geometry,
  picks up per-geometry verts+tris, opens result in COL Workshop
- **_enable_dff_toolbar #vers 2** — controls prelight row visibility,
  hides/shows COL buttons, updates info_format label text

### DAT Browser / Asset DB
- **COL DB tab** — shows all COL models indexed from IMGs, double-click opens in COL Workshop
- **COL indexing fixed** — `_index_col_data #vers 2` robust multi-model parser
  (no longer stops at first unknown signature in chained COL blocks)
- **Standalone .col files** — indexed during `_db_build` from DAT load_log COLFILE entries
- **Status column** — COL files show `● in DB` when indexed (checks col_entries by basename)
- **Model count** — COL entries column shows model count from DB

### Welcome Screen
- **Show on startup toggle** — "Show this screen on startup" checkbox persists to
  `~/.config/imgfactory/welcome_prefs.json`
- **Dismiss button** — hides welcome screen, re-openable via taskbar
- **[Intro] taskbar button** — always available to re-open the welcome/intro screen
- `WelcomeScreen.should_show_on_startup()` — static method queried at startup

### Taskbar
- **Force into view** — clicking any taskbar button now also calls
  `_ensure_content_visible()` which expands the content splitter if collapsed
  and brings the main window to front (`raise_()` + `activateWindow()`)

### Asset DB (asset_db.py)
- `index_col(col_path)` — index standalone .col files
- `find_col_file(entry_name)` — look up .col IMG entry by filename
- `_index_col_data #vers 2` — robust chained-block parser with forward-skip on bad data
- `index_game_root()` — now indexes .col files alongside .img and .ide

---

## Build 316

### DAT Browser
- Asset DB panel with profile selector, Build DB, Update buttons
- ⬡ DB toolbar button (database cylinder SVG icon)
- Adaptive compact mode (icon-only when panel < 420px)

### Model Workshop
- `_create_primitive_dialog()` — Box/Sphere/Cylinder/Plane with subdivision counts
- `_build_primitive()` — generates vertex+triangle lists
- `_add_geometry_to_dff()` — appends to live DFF model, refreshes viewport
- V/E/F/P select mode buttons, backface cull, front-only paint toggles
- `_find_col_via_db()`, `_find_txd_via_db()` — DB-first asset lookup
- Paint freehand → opens Material List (DFF mode) not COL face-paint

### IDE Editor
- Analysis cross-references asset DB for missing DFF/TXD files

---

## Build 232–315  (earlier sessions)

See per-session commit messages in git log.

Key milestones:
- **232** DP5 Workshop container
- **231** DP5 Paint editor (IFF ILBM I/O, 16-level undo)
- **Build 226–230** Android/PS2/Xbox TXD platform fixes
- **Build 200+** DAT Browser major overhaul, SOL city grouping, SCM Workshop
- **Build 150+** COL Workshop 3D viewport, compact list view
- **Build 1** Project started — IMG Factory 1.5 → 1.6 PyQt6 migration
