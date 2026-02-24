#this belongs in root /ChangeLog.md - Version: 8

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
