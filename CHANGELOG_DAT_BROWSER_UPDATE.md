# DAT Browser — Major Update

## Load Order Tree Overhaul

### New Toolbar above Load Order tree
- **Sort combo** — 6 sort modes:
  - Original (DAT load order)
  - A → Z (filename ascending)
  - Z → A (filename descending)
  - Largest first (by file size)
  - Smallest first
  - By type (IMG → IDE → IPL → COL)
- **Group button** *(SOL only)* — collapses entries into city/section groups:
  - `▶ VC City` (game_vc.img, game_vc.col, game_vc.ide, VC IPLs)
  - `▶ LC City`, `▶ LA (San Andreas)`, `▶ San Fierro`, `▶ Las Venturas`
  - `▶ San Andreas`, `▶ Mainland`, `▶ Extended`, `▶ Generics`, `▶ Special`
  - Each group header shows file counts (IMG:N IDE:N COL:N)
  - Auto-shown/hidden based on game type
- **COL▾ toggle** — scans each IMG archive and adds embedded `.col` files
  as child nodes. Useful for VC and SA where COL data lives inside gta3.img.
  Optional scan for SOL (disabled by default since SOL avoids COL-in-IMG).

### Right-click Context Menu — expanded
- **IMG/CDIMAGE entries:**
  - ⊞ Open in IMG Factory tab
  - ⊞ Load ALL game IMGs
  - 📦 Dump all TXDs from this IMG
- **IDE entries (new):**
  - 📋 Filter Objects to [filename] — populates Objects table instantly
  - ✏ Edit [filename] — opens in text editor
  - 🔍 Open in IDE Editor
- **IPL entries (new):**
  - 📋 Filter Instances to [filename] — populates Instances table
  - ✏ Edit [filename]
- **COL entries (standalone .col from COLFILE directive):**
  - ⬛ Open in COL Workshop
- **COL▾ entries (embedded inside IMG):**
  - ⬛ Extract & open in COL Workshop — extracts to temp file, opens workshop

### Single-click behaviour
- **IMG/CDIMAGE:** If already open as a tab, brings that tab to front.
  If not open, opens it in a new tab.
- **IDE:** Filters the Objects table to that IDE file
- **IPL:** Filters the Instances table to that IPL file  
- **COL:** Opens standalone .col in COL Workshop
- **COL▾:** Extracts from IMG and opens in COL Workshop
- **GROUP:** Collapses/expands the city group

### COLFILE entries in load_log
- All three games now log COLFILE directives:
  - **SOL:** standalone .col files (sol/cdimages/game_vc.col etc.)
  - **LC:** DATA/MAPS/ subfolder .col files (INDUSTNE.col, COMNtop.col, etc.)
  - **VC:** MODELS/COLL/GENERIC.COL
- Show as COL type rows with ✓/✗ status

## Workshop Left/Middle Panel Counts
- **TXD Workshop left panel:** shows `TXD Files  (N)` after scanning IMG
- **TXD Workshop middle panel:** shows `Textures  (N)` after loading a TXD
- **COL Workshop left panel:** shows `COL Files  (N)` after scanning IMG
- **COL Workshop middle panel:** shows `COL Models  (N)` after file load

## Model Workshop
- Left panel header: `Model Files — DFF (N)  TXD (N)  COL (N)`
- TXD auto-lookup: searches all open IMG tabs for IDE-linked TXD name
- Material List dialog: "Auto-find from IMGs" button
- Textured rendering: real TXDWorkshop instance for DXT decoding
- _FaceAdapter fixed (was broken by indentation fixer)

## TODO (planned)
- IPL Editor: world map view, cube objects at x/y/z, bulk translate/rotate
- TXD bulk export: PNG, IFF/ILBM, TGA, DDS, BMP
- SCM Workshop: decompile → edit IPL coords → recompile
- Water plane and radar tile repositioning
