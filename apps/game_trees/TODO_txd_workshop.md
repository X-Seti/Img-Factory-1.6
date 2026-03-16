# TXD Workshop — iOS LCS Texture Mapping TODO

## Source file
`ios_lcs_texture_mapping.csv` — 4549 entries in format:
```
folder/texture_name, FOLDER_NAME, texture_name
```
e.g. `comhospital/er, comhospital, ER`

## What this represents
iOS LCS uses individual `.pvr` files in `Textures_PVR/<txd_name>/<texture_name>.pvr`
instead of packed `.TXD` archives.

The CSV maps:  **PVR file path → TXD name → texture name**

## Suggested features to implement
1. **PVR texture import** — when user opens a TXD from iOS LCS, allow importing
   matching PVR textures by looking up the CSV mapping table
2. **TXD ↔ PVR export** — export TXD textures as PVR files in the iOS folder structure
3. **Texture preview** — show PVR textures in the TXD Workshop preview panel
   (PVRTC decode support — already on TODO from session 5)
4. **Batch TXD rebuild** — use the CSV to rebuild all TXDs from the PVR tree,
   or vice versa (PVR tree from TXD archive)

## CSV stats
- 4549 texture mappings
- Covers all world geometry TXDs (commer, indust, suburb etc.)
- Does NOT cover: SPLASH/*.TXD, FONTS.TXD, HUD.TXD (those are standard TXD)
