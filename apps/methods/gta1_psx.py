#this belongs in apps/methods/gta1_psx.py - Version: 5
# X-Seti - October 23 2026 - IMG Factory 1.6 - PS1 GTA1 / London city .vsr map and style

"""
PS1 GTA1 / London cities (NYC.VSR, SANB.VSR, MIAMI.VSR): one .vsr holds
the map (CMP folder: HEADER, BASE (u16 word offsets), COLUMNS, BLOCK_INFO,
OBJECTS, ROUTES, LOCATIONS, NAV_DATA - PC block layout) and its style
(TILE_GFX / SPRITE_GFX: 4-bit 256 x 256 texture pages, 16-colour CLUTs of
PS1 15-bit colour, a 12-byte index entry per 32 x 32 tile or sprite).
Map and style edits are written back into the same .vsr.
"""

##Methods list -
# vsr_gfx
# vsr_gfx_rgba
#
##class GTA1PSXMap: -
# _sync
# from_archive
# from_bytes
# set_zones
# to_bytes
# zones
#
##class GTA1PSXStyle: -
# __init__
# _set_image
# _sync
# face_global
# face_rgba
# from_archive
# from_bytes
# global_face
# palette_count
# palette_rgba
# set_palette_rgba
# set_sprite_rgba
# set_tile_rgba
# sprite_bases
# sprite_count
# sprite_rgba
# tile_count
# tile_rgba
# to_bytes

import struct
from typing import Dict, List, Optional

import numpy as np

from apps.methods.gta1_map import SIZE, GTA1Map
from apps.methods.retro_palette import ps1_rgba, put_4bit, quantize, rgba_ps1
from apps.methods.vsr_archive import VSRArchive

TILE = 32
SPRITE_TYPES = ('arrow', 'digits', 'boat', 'box', 'bus', 'car', 'object', 'ped', 'speedo', 'tank',
                'traffic lights', 'train', 'train doors', 'bike', 'tram', 'wbus', 'wcar', 'ex',
                'tumcar', 'tumtruck', 'ferry', 'extra')


def vsr_gfx(arch: VSRArchive, folder: str) -> dict: #vers 2
    """Index entries, texture pages (by directory entry number, with bit depth) and CLUT pages of a folder."""
    idx = arch.blob(folder, 'INDEX')
    ents = [struct.unpack_from('<BBBBHHHBB', idx, i * 12) for i in range(len(idx) // 12)]
    pages, bpp, cluts = {}, {}, {}
    for i, e in enumerate(arch.entries):
        if e['folder'] != folder:
            continue
        if e['kind'] == 0x19:
            w, h, b = e['params'][0], e['params'][1], e['params'][2]
            pages[i] = np.frombuffer(e['data'], np.uint8).reshape(h, w * b // 8).copy()
            bpp[i] = b
        elif e['kind'] == 0x1B:
            cluts[i] = np.frombuffer(e['data'], '<u2').copy()
    return {'index': ents, 'pages': pages, 'bpp': bpp, 'cluts': cluts, 'dirty': set(), 'clut_dirty': set()}


def vsr_gfx_rgba(g: dict, n: int) -> np.ndarray: #vers 2
    """RGBA of index entry n (4- or 8-bit page; 0 width / height = 256)."""
    w, h, x, y, tp, cp, cx, _f, _x = g['index'][n]
    if tp not in g['pages'] or cp not in g['cluts']:
        return np.zeros((max(h, 1), max(w, 1), 4), np.uint8)
    w, h = w or 256, h or 256
    pg = g['pages'][tp]
    if g['bpp'][tp] == 8:
        idx = pg[y:y + h, x:x + w]
        return ps1_rgba(g['cluts'][cp][cx:cx + 256])[idx]
    sub = pg[y:y + h, x // 2:(x + w + 1) // 2]
    idx = np.empty((sub.shape[0], sub.shape[1] * 2), np.uint8)
    idx[:, 0::2], idx[:, 1::2] = sub & 15, sub >> 4
    idx = idx[:, (x & 1):(x & 1) + w]
    return ps1_rgba(g['cluts'][cp][cx:cx + 16])[idx]


class GTA1PSXMap(GTA1Map): #vers 1
    """GTA1 map read from / written to the CMP folder of a PS1 .vsr."""

    GAME = 'GTA1 PS1'

    @classmethod
    def from_archive(cls, arch: VSRArchive) -> 'GTA1PSXMap': #vers 2
        m = cls()
        m.arch, m.path = arch, arch.path
        base = [w * 2 for w in struct.unpack(f'<{SIZE * SIZE}H', arch.blob('CMP', 'BASE'))]
        m._unpack(base, arch.blob('CMP', 'COLUMNS'), arch.blob('CMP', 'BLOCK_INFO'))
        m.header = (331, 0, 0, 0, 0, 0, 0, 0, 0)
        arch.syncs.append(m._sync)
        return m

    @classmethod
    def from_bytes(cls, data: bytes, path: str = '') -> 'GTA1PSXMap': #vers 1
        return cls.from_archive(VSRArchive.from_bytes(data, path))

    def _sync(self): #vers 1
        """Map edits into BASE / COLUMNS / BLOCK_INFO (untouched maps stay byte for byte)."""
        if not self.edited:
            return
        base, cols, blocks = self._pack()
        if len(cols) // 2 > 0xFFFF:
            raise ValueError("Columns too big for a PS1 map (65535 words)")
        self.arch.set_blob('CMP', 'BASE', struct.pack(f'<{SIZE * SIZE}H', *[b // 2 for b in base]))
        self.arch.set_blob('CMP', 'COLUMNS', cols)
        self.arch.set_blob('CMP', 'BLOCK_INFO', blocks)

    def to_bytes(self) -> bytes: #vers 1
        return self.arch.to_bytes()

    def zones(self) -> List[dict]: #vers 1
        """PS1 NAV_DATA is packed differently from the PC zones - not read yet."""
        return []

    def set_zones(self, zones: List[dict]): #vers 1
        raise ValueError("PS1 GTA1 zones (NAV_DATA) can't be written yet")


class GTA1PSXStyle: #vers 1
    """Tiles and sprites of a PS1 GTA1 city .vsr (same API as GTA1Style)."""

    def __init__(self): #vers 1
        self.arch = None
        self.path = ''

    @classmethod
    def from_archive(cls, arch: VSRArchive) -> 'GTA1PSXStyle': #vers 1
        s = cls()
        s.arch, s.path = arch, arch.path
        h = struct.unpack_from('<10H', arch.blob('STYLE', 'HEADER'))
        s.nside, s.lid_base, s.naux = h[2], h[3], h[4]
        s.tiles = vsr_gfx(arch, 'TILE_GFX')
        s.sprites = vsr_gfx(arch, 'SPRITE_GFX')
        nums = arch.blob('STYLE', 'SPRITE_NUMBERS')
        s.counts = struct.unpack(f'<{len(nums) // 2}H', nums)
        arch.syncs.append(s._sync)
        return s

    @classmethod
    def from_bytes(cls, data: bytes, path: str = '') -> 'GTA1PSXStyle': #vers 1
        return cls.from_archive(VSRArchive.from_bytes(data, path))

    def tile_count(self) -> int: #vers 1
        return len(self.tiles['index'])

    def tile_rgba(self, n: int) -> np.ndarray: #vers 2
        if not 0 <= n < self.tile_count():
            return np.zeros((TILE, TILE, 4), np.uint8)
        a = vsr_gfx_rgba(self.tiles, n)
        return a if a.shape[:2] == (TILE, TILE) else np.zeros((TILE, TILE, 4), np.uint8)

    def face_global(self, face: str, n: int) -> Optional[int]: #vers 1
        """Lid tiles start at the style's lid base, side tiles at 0."""
        if face == 'lid':
            return self.lid_base + n if n > 0 and self.lid_base + n < self.tile_count() else None
        return n if 0 < n < self.nside else None

    def global_face(self, face: str, g: int) -> Optional[int]: #vers 1
        if face == 'lid':
            return g - self.lid_base if self.lid_base <= g < self.tile_count() else None
        return g if 0 <= g < self.nside else None

    def face_rgba(self, face: str, n: int) -> np.ndarray: #vers 1
        g = self.face_global(face, n)
        return self.tile_rgba(g) if g is not None else np.zeros((TILE, TILE, 4), np.uint8)

    def _set_image(self, g: dict, n: int, rgba: np.ndarray): #vers 1
        """Index entry n of a gfx folder from RGBA: nearest of its 16 CLUT colours."""
        w, h, x, y, tp, cp, cx, _f, _x = g['index'][n]
        if (w, h) != rgba.shape[1::-1]:
            raise ValueError(f"PS1 image {n} is {w} x {h}")
        put_4bit(g['pages'][tp], x, y, quantize(rgba, ps1_rgba(g['cluts'][cp][cx:cx + 16])))
        g['dirty'].add(tp)

    def _pals(self) -> list: #vers 1
        """(gfx, clut entry, start, colours) of every CLUT used by tiles / sprites."""
        if not hasattr(self, '_pal_list'):
            seen = {}
            for g in (self.tiles, self.sprites):
                for e in g['index']:
                    tp, cp, cx = e[4], e[5], e[6]
                    if cp in g['cluts'] and tp in g['bpp']:
                        seen.setdefault((id(g), cp, cx), (g, cp, cx, 256 if g['bpp'][tp] == 8 else 16))
            self._pal_list = list(seen.values())
        return self._pal_list

    def palette_count(self) -> int: #vers 1
        return len(self._pals())

    def palette_rgba(self, i: int) -> np.ndarray: #vers 1
        g, cp, cx, n = self._pals()[i]
        return ps1_rgba(g['cluts'][cp][cx:cx + n])

    def set_palette_rgba(self, i: int, rgba: np.ndarray): #vers 1
        g, cp, cx, n = self._pals()[i]
        g['cluts'][cp][cx:cx + n] = rgba_ps1(rgba[:n], g['cluts'][cp][cx:cx + n])
        g['clut_dirty'].add(cp)

    def set_tile_rgba(self, n: int, rgba: np.ndarray): #vers 3
        self._set_image(self.tiles, n, rgba)

    def set_sprite_rgba(self, n: int, rgba: np.ndarray): #vers 1
        self._set_image(self.sprites, n, rgba)

    def sprite_count(self) -> int: #vers 1
        return len(self.sprites['index'])

    def sprite_bases(self) -> Dict[str, int]: #vers 1
        out, at = {}, 0
        for name, n in zip(SPRITE_TYPES, self.counts):
            if n:
                out[name] = at
            at += n
        return out

    def sprite_rgba(self, n: int) -> np.ndarray: #vers 2
        return vsr_gfx_rgba(self.sprites, n)

    def _sync(self): #vers 2
        """Edited texture pages and CLUTs back into the archive."""
        for g in (self.tiles, self.sprites):
            for tp in g['dirty']:
                self.arch.entries[tp]['data'] = g['pages'][tp].tobytes()
            for cp in g['clut_dirty']:
                self.arch.entries[cp]['data'] = g['cluts'][cp].tobytes()
            g['dirty'].clear()
            g['clut_dirty'].clear()

    def to_bytes(self) -> bytes: #vers 1
        return self.arch.to_bytes()
