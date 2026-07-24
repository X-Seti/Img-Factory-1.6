# KED / MooMapper reference source (recovered)

This is real Delphi/Pascal source code for **KEd** (aka **MooMapper**), a
2003-2005 era GTA III/VC/SA map editor by Alastair Burr (with contributions
from Nicola L Robinson and "PatrickW"). Recovered from `Ked code (rar).tar.gz`
and `Ked_rev.7z` in this folder.

## Why this exists
Both archives' `.pas` files extracted as 0 bytes with `unar` and `7z` (their
old-format RAR support is incomplete). The real, non-free `unrar` (v7.0.7,
installed via `apt-get install unrar`) extracted everything correctly - the
data was intact in the archives the whole time, it was a tooling issue.

## What's here
- `moomapper/` - the full KEd editor source. Entry point: `KEd.dpr`.
  - `Main.pas` - main app window/orchestration
  - `EditorIPL.pas` / `.dfm` - the actual IPL (item placement) editor UI+logic
  - `EditorIDE.pas` / `.dfm` - IDE (object definition) editor
  - `EditorItem.pas` / `.dfm` - item instance editor (also has PATRICKW/
    ORIGINAL and PATRICKW/CHANGED variants - two versions of this file,
    presumably before/after a contributed change)
  - `EditorArchive.pas` / `.dfm` - IMG archive browser
  - `EditorDAT.pas` / `.dfm` - .dat file editor
  - `GTADff.pas` - RenderWare DFF (model) parser - real RW chunk ID
    constants (rwCLUMP=16, rwFRAMELIST=14, rwGEOMETRY=15, rwATOMIC=20,
    rwMATERIAL=7, rwMATERIALLIST=8, rwMATERIALSPLIT=1294, rwFRAME=39056126,
    etc) and binary record layouts (TDFFFace, TDFFUV, ...)
  - `GTATxd.pas` - TXD (texture dictionary) parser
  - `GTAImg.pas` - IMG archive reader
  - `GTAIde.pas` / `GTAText.pas` / `GTAZon.pas` / `GTACol.pas` - IDE/text/
    zone/collision format handling
  - `GLView.pas` / `GLViewDetached.pas` - the OpenGL 3D viewport
  - `COLADDON/` - a separate COL (collision) format addon (ColClass.pas,
    U_records.pas, FastTextCRC.pas, Glut.pas)
  - `no_sa/`, `foo/` - alternate/older variants of GTATxd.pas and
    RequiredTypes.pas kept alongside the main ones
- `GTAIMG.PAS`, `gtainterface_Unit1.pas` - a separate, simpler standalone
  IMG-archive tool ("gtainterface"), not part of KEd itself
- `gtainterface_GTATxd.pas`, `updated_RenderWareDFF.pas` - an updated/
  alternate pair of TXD+DFF loaders bundled separately from the main KEd
  source, possibly a later revision

All originally binary/compiled clutter (`.dcu`, `.exe`, `.dsm` disassembly
listings, `.rar`/`.jpg`/`.bmp` assets, editor backup files) was stripped
out - only source text and `.dfm` form definitions were kept.

## Known feature set (from strings in the compiled KEd.exe)
Item Editor / Item Instance Editor / Object Definition Editor / Archive
Editor / Texture Display, radar calibration, path editing (line width/node
size/lanes), file validation (duplicate entries, invalid values), camera
navigation (first person, rotate/move camera, reset view, zoom/view
distance), rendering toggles (wireframe, lighting, reflection, lens flare,
particles), Vice City mode.

This is Pascal/Delphi, not directly usable in the Python/PyQt6 codebase -
treat it as a reference for file-format parsing logic and editor UX/
workflow design when building out `Map_Workshop.py`.
