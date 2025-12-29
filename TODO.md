#this belongs in root /TODO.md - Version: 2
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
