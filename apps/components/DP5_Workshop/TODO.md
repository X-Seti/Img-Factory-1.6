# DP5 Workshop — TODO
# X-Seti - April 2026 - IMG Factory 1.6

## Priority (next session)
- [ ] Character/font editor — 8×8 or 8×16 char sets, export binary/C header/ASM
- [ ] Sprite editor mode — platform native constraints, multi-frame
- [ ] Colour clash visualiser — ZX Spectrum attribute clash highlighting

## Drawing Tools
- [ ] Curve tool — bezier/spline (currently polygon only)
- [ ] Text on canvas — click to place text directly on image, no dialog
- [ ] Gradient fill — linear and radial
- [ ] Pattern/tile brush — stamp selection as repeating tile
- [ ] Magic wand select — flood select by colour tolerance

## Workflow
- [ ] History panel — undo stack visible, click to restore state
- [ ] Layers — add/delete/reorder/merge/opacity
- [ ] Canvas rulers — pixel/unit display on edges
- [ ] Guides — drag from ruler to place snap guides
- [ ] Screen eyedropper — pick colour from anywhere on screen

## Export / Integration
- [ ] Sprite sheet splitter/exporter — cut canvas into equal tiles, export grid
- [ ] Tile map editor — stamp tiles onto grid canvas
- [ ] Palette editor — edit swatch colours with HSV/RGB sliders in panel

## Retro Audio (tape systems)
- [ ] ZX Spectrum TAP export (already exists — verify)
- [ ] TZX export — Spectrum TZX with proper block headers
- [ ] WAV export — audio tone encoding for tape loading
- [ ] C64 TAP audio encoding
- [ ] Amstrad CDT format

## Img Factory Integration
- [ ] Radar map editor — SA, VC, LC, SOL maps using 1296 tiles
  - [ ] 36×36 tile grid (1296 tiles)
  - [ ] Tile picker panel
  - [ ] Place/erase/fill tiles
  - [ ] Export as GTA radar texture

## Font/Text
- [ ] Click-to-place text directly on canvas (no dialog)
- [ ] Font picker with preview
- [ ] Bold/italic/underline
- [ ] Font size slider
- [ ] Colour picker for text

## Amiga Specific
- [ ] NewIcon export (PNG embedded in ToolTypes)
- [ ] OS3.5 ICONFACE import (needs format research)
- [ ] IFF animation (ANIM) import/export


## Side Projects

### KDE6 Amiga .info Thumbnail/Display Handler
- [ ] KDE6 thumbnail plugin — show .info icon previews in Dolphin/KDE file manager
- [ ] Uses KIO/KDE thumbnail framework (C++ plugin or Python via KF6)
- [ ] Extract bitplane image from DiskObject using same decoder logic
- [ ] Support Classic bitplane (2/4/8bp) and NewIcon (IM1= PNG)
- [ ] Report icon type in file properties (WBTOOL/WBPROJECT/WBDRAWER etc)
- [ ] Optional: context menu "View .info details" showing ToolTypes, DefaultTool, stack size
- [ ] Package as .so thumbnail plugin for KDE6
- [ ] Possible Python approach via dolphin-plugins or kio-extras extension
