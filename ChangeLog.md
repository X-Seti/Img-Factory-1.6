#this belongs in root /ChangeLog.md - Version: 18

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
