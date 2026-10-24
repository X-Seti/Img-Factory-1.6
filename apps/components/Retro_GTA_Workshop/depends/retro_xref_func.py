#this belongs in apps/components/Retro_GTA_Workshop/depends/retro_xref_func.py - Version: 1
# X-Seti - October 24 2026 - IMG Factory 1.6 - Retro GTA Workshop format reference

"""
[X-Ref] title bar button: reference of every file format the Retro GTA
Workshop reads / writes (GTA1, GTA2, PS1, discs), one tab per group.
"""

##Methods list -
#
##class RetroXRefMixin: -
# _show_xref
# _titlebar_left_extras

from PyQt6.QtWidgets import QDialog, QTabWidget, QTextBrowser, QVBoxLayout

XREF = [
    ("GTA1 PC", """
<h2>.cmp map (version 331)</h2>
<p>256 x 256 columns of up to 6 blocks. File level 0 is the top, 5 the ground.</p>
<table><tr><th>Part</th><th>Layout</th></tr>
<tr><td>Block (8 bytes)</td><td>type_map u16, type_map_ext u8, left, right, top, bottom, lid u8</td></tr>
<tr><td>After columns</td><td>object positions, routes, locations, navigation zones (35-byte records)</td></tr></table>
<h2>.g24 style (version 336)</h2>
<p>64 x 64 8-bit tiles (side, lid, aux) in rows of four, tile animations, CLUTs in 64 KB pages of
64 palettes (BGRA), palette index (4 remaps per tile, then sprite / car / font CLUTs), object and
car info, sprite info + graphics (256-wide pages), sprite counts per type.</p>
<h2>.gry style (version 325, 8-bit)</h2>
<p>52-byte header, the same tiles unpadded, one 6-bit RGB palette, remap tables,
sprite info of 10 bytes (+ deltas).</p>
<h2>MISSION.INI</h2>
<p><code>[n]</code> sections, a <code>name, n, map.cmp, n</code> line, then objects
<code>id [1] (x,y,z) TYPE args</code>. Block coordinates, or 1/64 block when z is above 8.</p>
<h2>.fxt text</h2>
<p><code>[key]text</code> strings, 0-terminated, every byte +1. First 8 bytes also
+ 0x63, 0xC6, 0x8C ... (0x63 doubled each byte).</p>
<h2>.sdt + .raw sounds</h2>
<p>12-byte entries (offset, size, rate); samples 8-bit unsigned mono.</p>
"""),
    ("GTA2 PC", """
<h2>.gmp map (GBMP 500)</h2>
<p>Chunked file. The PC game reads DMAP (32-bit compressed: 256 x 256 columns of up to 8 blocks).</p>
<table><tr><th>Chunk</th><th>Layout</th></tr>
<tr><td>DMAP</td><td>base u32[256 x 256], columns, blocks (5 face words, arrows u8, slope_type u8)</td></tr>
<tr><td>ZONE</td><td>type u8, x, y, w, h u8, name length u8, name</td></tr>
<tr><td>LGHT</td><td>16 bytes: ARGB, x, y, z, radius (1.8.7 fixed), intensity, shape, on / off time</td></tr>
<tr><td>MOBJ</td><td>x, y (fixed point) i16, rotation u8 (0-255), object type u8</td></tr>
<tr><td>ANIM, RGEN, PSXM</td><td>kept unchanged (RGEN sizes unknown)</td></tr></table>
<h2>Face word (u16)</h2>
<table><tr><th>Bits</th><th>Meaning</th></tr>
<tr><td>0-9</td><td>tile 0-1023</td></tr>
<tr><td>10</td><td>wall (sides) / light bit 0 (lid)</td></tr>
<tr><td>11</td><td>bullet wall (sides) / light bit 1 (lid)</td></tr>
<tr><td>12</td><td>flat</td></tr>
<tr><td>13</td><td>flip</td></tr>
<tr><td>14-15</td><td>rotation (clockwise, applied before flip)</td></tr></table>
<p>slope_type: bits 0-1 ground type, 2-7 slope (1-44 gradients, 45+ diagonals / partial).
Arrows: bits 0-3 green left, right, up, down; bits 4-7 red.</p>
<h2>.sty style (GBST 700)</h2>
<table><tr><th>Chunk</th><th>Layout</th></tr>
<tr><td>TILE</td><td>64 x 64 8-bit tiles in 256 x 256 pages</td></tr>
<tr><td>SPRG / SPRX</td><td>sprite pages (256 wide) / sprite index</td></tr>
<tr><td>PPAL</td><td>physical palettes, 64 per 64 KB page, BGRA</td></tr>
<tr><td>PALX / PALB</td><td>virtual palette index / palette bases</td></tr></table>
<p>Colour 0 is transparent.</p>
<h2>.scr script (compiled)</h2>
<p>6000 u16 command offsets; command data at byte 12000 + offset. Each command: 8-byte header
(this, type, next, return), then data. Positions are 32-bit, 16384 per block.
Layouts from the Epic GTA2 Script Decompiler.</p>
<h2>.gxt text</h2>
<p><code>GBL</code> + language letter, u16 version, TKEY (u32 offset into TDAT + 8-byte key),
TDAT (UTF-16LE, 0-terminated).</p>
<h2>.sdt + .raw sounds</h2>
<p>24-byte entries (offset, size, rate, loop data); samples 16-bit mono.</p>
"""),
    ("PS1 GTA1", """
<h2>City .vsr (NYC, SANB, MIAMI, London)</h2>
<p>One .vsr holds map and style.</p>
<table><tr><th>Folder</th><th>Parts</th></tr>
<tr><td>CMP</td><td>HEADER, BASE (u16 word offsets), COLUMNS, BLOCK_INFO, OBJECTS, ROUTES,
LOCATIONS, NAV_DATA (PC block layout)</td></tr>
<tr><td>TILE_GFX / SPRITE_GFX</td><td>4-bit 256 x 256 pages, 16-colour CLUTs (PS1 15-bit colour),
12-byte index entry per 32 x 32 tile or sprite</td></tr></table>
<h2>AUDIO.VSR</h2>
<p><code>name_sbi</code>: u16 SPU address / 8, size / 8, rate per sample; bank loaded at SPU 0x1000.
<code>name_sbk</code>: PS-ADPCM bank. Filters (0,0) (60,0) (115,-52) (98,-55) (122,-60).</p>
<h2>Image .vsr</h2>
<p>INDEX + pages + CLUTs: fonts, front end, loading screens.</p>
<h2>PS1 colour</h2>
<p>15-bit BGR (5:5:5) + STP bit; 0x0000 is transparent.</p>
"""),
    ("PS1 GTA2", """
<h2>.gmp map (WIL / STE / BIL)</h2>
<p>CMAP chunk first (no GBMP header).</p>
<table><tr><th>Part</th><th>Layout</th></tr>
<tr><td>Base</td><td>u16[256 x 256] word offsets</td></tr>
<tr><td>Columns</td><td>u8 height, u8 offset, u16 block ids (0x8000 = lid-only block)</td></tr>
<tr><td>Blocks</td><td>0x400 pad, u16 count + 12-byte blocks</td></tr>
<tr><td>Lid-only</td><td>0x600 pad, u16 count + 4-byte blocks (lid, arrows, slope)</td></tr>
<tr><td>Other</td><td>ZONE, ANIM, RGEN; each padded with 0xAA</td></tr></table>
<h2>.sty style</h2>
<p>Header: 7 sprite group counts. Sprite palette table (u16 per sprite) after an 0xFFFF marker,
then 8-byte sprite entries: w, h, pixel width (0 = 8-bit), mirror flag, offset, page.
7.5 sprite pages 256 bytes wide, 256-colour palettes (512 bytes each), then 384 tiles of
32 x 32 at the file end: 6 pages of 256 x 256 4-bit pixels, each followed by 64 CLUTs.
Mirrored sprites (cars) hold the left half only.</p>
<h2>.str video</h2>
<p>2336-byte Mode 2 sectors; wrapped to 2352 bytes and converted by ffmpeg (psxstr) to MP4.</p>
"""),
    ("Discs", """
<h2>Disc images</h2>
<table><tr><th>Type</th><th>Sectors</th></tr>
<tr><td>CloneCD .img / raw .bin</td><td>2352 bytes, Mode 1 or Mode 2 (PS1)</td></tr>
<tr><td>.iso</td><td>2048 bytes</td></tr></table>
<p>ISO 9660 file tree from the volume descriptor at sector 16. Volume size at descriptor
offset 80 (LE + BE). Files replaced in place when they fit; bigger and new files go after the
last sector, volume size and the .ccd lead-out (A2) updated. Sector EDC / ECC rewritten.
XA / STR files extracted as raw 2336-byte sectors, CD audio tracks as .wav.</p>
"""),
]


class RetroXRefMixin: #vers 1
    """Format reference of the Retro GTA Workshop."""

    def _titlebar_left_extras(self, layout): #vers 1
        """[X-Ref] after Settings, then the base extras."""
        super()._titlebar_left_extras(layout)
        self.xref_btn = self._tb_button('info_icon', "X-Ref",
                                        "Format reference: GTA1, GTA2, PS1, disc images", self._show_xref,
                                        keep_text=True)
        layout.addWidget(self.xref_btn)

    def _show_xref(self): #vers 1
        """Dialog: one tab of format notes per group."""
        acc = self._get_accent_color()
        css = (f"<style>h2{{color:{acc};font-size:14px;margin:12px 0 4px 0;}}"
               "table{border-collapse:collapse;margin:4px 0 8px 0;}"
               "th,td{border:1px solid #777;padding:3px 8px;text-align:left;vertical-align:top;}"
               "code{font-family:monospace;}</style>")
        dlg = QDialog(self)
        dlg.setWindowTitle(f"{self.App_name} - Format Reference")
        dlg.resize(820, 680)
        lay = QVBoxLayout(dlg)
        tabs = QTabWidget()
        for title, html in XREF:
            t = QTextBrowser()
            t.setHtml(css + html)
            tabs.addTab(t, title)
        lay.addWidget(tabs)
        dlg.exec()
