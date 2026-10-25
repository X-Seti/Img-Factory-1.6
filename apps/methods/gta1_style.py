#this belongs in apps/methods/gta1_style.py - Version: 3
# X-Seti - October 23 2026 - IMG Factory 1.6 - GTA1 .g24 / .gry style read / write

"""
GTA1 (PC) .g24 style, version 336: 64 x 64 8-bit tiles (side, lid, aux)
in rows of four, tile animations, CLUTs in 64 KB pages of 64 palettes
(BGRA), palette index (tiles use 4 remaps each, then sprite / new car /
font cluts), object and car info, sprite info + sprite graphics (256-wide
pages) and sprite counts per type. Tile edits are written back.
.gry (version 325, 8-bit mode): 52-byte header, the same tiles unpadded,
one 6-bit RGB palette, remap tables, sprite info of 10 bytes (+ deltas)
drawn with that palette.
"""

##Methods list -
#
##class GTA1Style: -
# _from_gry
# _sgfx_pages
# __init__
# face_global
# face_rgba
# from_bytes
# global_face
# nlid
# nside
# palette
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

from apps.methods.retro_palette import quantize

TILE, ROW = 64, 256
SPRITE_TYPES = ('arrow', 'digits', 'boat', 'box', 'bus', 'car', 'object', 'ped', 'speedo', 'tank',
                'traffic lights', 'train', 'train doors', 'bike', 'tram', 'wbus', 'wcar', 'ex',
                'tumcar', 'tumtruck', 'ferry')
_HDR = ('version', 'side', 'lid', 'aux', 'anim', 'clut', 'tileclut', 'spriteclut', 'newcarclut',
        'fontclut', 'palidx', 'objinfo', 'car', 'spriteinfo', 'spritegfx', 'spritenum')
_GRY = ('version', 'side', 'lid', 'aux', 'anim', 'palette', 'remap', 'remapidx', 'objinfo', 'car',
        'spriteinfo', 'spritegfx', 'spritenum')


class GTA1Style: #vers 1
    """Tiles, palettes and sprites of a GTA1 .g24."""

    def __init__(self): #vers 1
        self.h: Dict[str, int] = {}
        self.raw = b''
        self.tiles = np.zeros((0, ROW), np.uint8)
        self.path = ''
        self.sprites: List[tuple] = []          # (w, h, clut, ptr)

    @classmethod
    def from_bytes(cls, data: bytes, path: str = '') -> 'GTA1Style': #vers 3
        if struct.unpack_from('<I', data, 0)[0] == 325:
            return cls._from_gry(data, path)
        h = dict(zip(_HDR, struct.unpack_from('<16I', data, 0)))
        if h['version'] != 336:
            raise ValueError(f"GTA1 .g24 / .gry style expected (version 336 / 325), got {h['version']}")
        s = cls()
        s.h, s.raw, s.path = h, data, path
        tiles_len = (h['side'] + h['lid'] + h['aux'] + 16383) // 16384 * 16384
        s.tiles_at, s.tiles_len = 64, tiles_len
        s.tiles = np.frombuffer(data, np.uint8, tiles_len, 64).reshape(-1, ROW).copy()
        o = 64 + tiles_len + h['anim']
        s.clut_at = o
        clut = np.frombuffer(data, np.uint8, h['clut'], o)
        clut = np.concatenate([clut, np.zeros((-len(clut)) % 65536, np.uint8)])
        s.ppal = clut.reshape(-1, 256, 64, 4).transpose(0, 2, 1, 3).reshape(-1, 256, 4)
        o += (h['clut'] + 65535) // 65536 * 65536
        s.palidx = np.frombuffer(data, '<u2', h['palidx'] // 2, o)
        o += h['palidx'] + h['objinfo'] + h['car']
        info_end = o + h['spriteinfo']
        while o + 12 <= info_end:                   # w, h, deltas, pad, size, clut, ptr, deltas
            w, ht, deltas, _p, _size, clut_i, ptr = struct.unpack_from('<4BHHI', data, o)
            s.sprites.append((w, ht, clut_i, ptr))
            o += 12 + deltas * 6
        s.sgfx_at = info_end
        s.counts = struct.unpack_from(f'<{h["spritenum"] // 2}H', data, info_end + h['spritegfx'])
        s._sgfx_pages()
        return s

    def _sgfx_pages(self): #vers 1
        """Sprite graphics as 256-wide rows (padded to whole pages), editable."""
        g = np.frombuffer(self.raw, np.uint8, self.h['spritegfx'], self.sgfx_at)
        self.sgfx = np.concatenate([g, np.zeros((-len(g)) % (ROW * ROW), np.uint8)]).reshape(-1, ROW)

    @classmethod
    def _from_gry(cls, data: bytes, path: str) -> 'GTA1Style': #vers 2
        """8-bit .gry: one palette for tiles and sprites."""
        h = dict(zip(_GRY, struct.unpack_from('<13I', data, 0)))
        s = cls()
        s.raw, s.path = data, path
        s.h = dict(h, tileclut=0)
        s.tiles_at, s.tiles_len = 52, h['side'] + h['lid'] + h['aux']
        s.tiles = np.frombuffer(data, np.uint8, s.tiles_len, 52).reshape(-1, ROW).copy()
        o = 52 + s.tiles_len + h['anim']
        s.gry_pal_at = o
        rgb = np.frombuffer(data, np.uint8, 768, o).reshape(256, 3).astype(np.uint16) * 255 // 63
        s.ppal = np.zeros((1, 256, 4), np.uint8)
        s.ppal[0, :, :3] = rgb[:, ::-1]                      # stored as BGRA like the .g24 cluts
        s.palidx = np.zeros(0, '<u2')
        o += h['palette'] + h['remap'] + h['remapidx'] + h['objinfo'] + h['car']
        info_end = o + h['spriteinfo']
        while o + 10 <= info_end:                   # w, h, deltas, pad, size, ptr, deltas
            w, ht, deltas, _p, _size, ptr = struct.unpack_from('<4BHI', data, o)
            s.sprites.append((w, ht, 0, ptr))
            o += 10 + deltas * 6
        s.sgfx_at = info_end
        s.counts = struct.unpack_from(f'<{h["spritenum"] // 2}H', data, info_end + h['spritegfx'])
        s._sgfx_pages()
        return s

    @property
    def nside(self) -> int:
        return self.h['side'] // (TILE * TILE)

    @property
    def nlid(self) -> int:
        return self.h['lid'] // (TILE * TILE)

    def tile_count(self) -> int: #vers 1
        return (self.h['side'] + self.h['lid'] + self.h['aux']) // (TILE * TILE)

    def palette(self, virtual: int) -> np.ndarray: #vers 1
        """256 x RGBA of a virtual palette (colour 0 transparent)."""
        phys = int(self.palidx[virtual]) if virtual < len(self.palidx) else 0
        rgba = self.ppal[min(phys, len(self.ppal) - 1)][:, [2, 1, 0, 3]].copy()
        rgba[:, 3] = 255
        rgba[0, 3] = 0
        return rgba

    def palette_count(self) -> int: #vers 1
        """Physical palettes (CLUTs) in the style."""
        return len(self.ppal)

    def palette_rgba(self, i: int) -> np.ndarray: #vers 1
        """256 x RGBA of physical palette i."""
        rgba = self.ppal[i][:, [2, 1, 0, 3]].copy()
        rgba[:, 3] = 255
        return rgba

    def set_palette_rgba(self, i: int, rgba: np.ndarray): #vers 1
        """Physical palette i from 256 x RGBA (.gry: 6-bit kept)."""
        if hasattr(self, 'gry_pal_at'):
            rgba = ((rgba.astype(np.uint16) * 63 + 127) // 255 * 255 // 63).astype(np.uint8)
        self.ppal[i, :, :3] = rgba[:256, [2, 1, 0]]

    def tile_rgba(self, n: int, remap: int = 0) -> np.ndarray: #vers 1
        """64 x 64 x 4 of global tile n (side tiles, then lid, then aux)."""
        if not 0 <= n < self.tile_count():
            return np.zeros((TILE, TILE, 4), np.uint8)
        r, c = divmod(n, 4)
        idx = self.tiles[r * TILE:(r + 1) * TILE, c * TILE:(c + 1) * TILE]
        return self.palette(n * 4 + remap)[idx]

    def face_global(self, face: str, n: int) -> Optional[int]: #vers 1
        """Global tile of a face's tile number (lid numbers follow the side tiles)."""
        if face == 'lid':
            return self.nside + n if 0 < n < self.nlid else None
        return n if 0 < n < self.nside else None

    def global_face(self, face: str, g: int) -> Optional[int]: #vers 1
        """Face tile number for a global tile, None when that face can't use it."""
        if face == 'lid':
            return g - self.nside if self.nside <= g < self.nside + self.nlid else None
        return g if 0 <= g < self.nside else None

    def face_rgba(self, face: str, n: int) -> np.ndarray: #vers 1
        g = self.face_global(face, n)
        return self.tile_rgba(g) if g is not None else np.zeros((TILE, TILE, 4), np.uint8)

    def set_tile_rgba(self, n: int, rgba: np.ndarray): #vers 2
        """Write global tile n from RGBA (nearest colour of its palette; alpha < 128 -> 0)."""
        idx = quantize(rgba, self.palette(n * 4))
        r, c = divmod(n, 4)
        self.tiles[r * TILE:(r + 1) * TILE, c * TILE:(c + 1) * TILE] = idx

    def sprite_count(self) -> int: #vers 1
        return len(self.sprites)

    def sprite_bases(self) -> Dict[str, int]: #vers 1
        """First sprite number of each sprite type that has sprites."""
        out, at = {}, 0
        for name, n in zip(SPRITE_TYPES, self.counts):
            if n:
                out[name] = at
            at += n
        return out

    def sprite_rgba(self, n: int) -> np.ndarray: #vers 2
        """h x w x 4 of sprite n (sprite cluts follow the tile cluts)."""
        w, h, clut, ptr = self.sprites[n]
        page, off = divmod(ptr, ROW * ROW)
        y0, x0 = divmod(off, ROW)
        idx = self.sgfx[page * ROW + y0:page * ROW + y0 + h, x0:x0 + w]
        return self.palette(self.h['tileclut'] // 1024 + clut)[idx]

    def set_sprite_rgba(self, n: int, rgba: np.ndarray): #vers 1
        """Write sprite n from RGBA of its size (nearest colour of its palette)."""
        w, h, clut, ptr = self.sprites[n]
        if rgba.shape[:2] != (h, w):
            raise ValueError(f"GTA1 sprite {n} is {w} x {h}")
        page, off = divmod(ptr, ROW * ROW)
        y0, x0 = divmod(off, ROW)
        self.sgfx[page * ROW + y0:page * ROW + y0 + h, x0:x0 + w] = \
            quantize(rgba, self.palette(self.h['tileclut'] // 1024 + clut))

    def to_bytes(self) -> bytes: #vers 4
        """Style with the edited tiles, palettes and sprites; everything else as read."""
        g_end = self.sgfx_at + self.h['spritegfx']
        if not hasattr(self, 'gry_pal_at'):                  # .g24: CLUT pages
            pal_at, pal = self.clut_at, self.ppal.reshape(-1, 64, 256, 4).transpose(0, 2, 1, 3).tobytes()
            pal = pal[:self.h['clut']]
        else:                                                # .gry: 6-bit RGB
            pal_at = self.gry_pal_at
            pal = ((self.ppal[0][:, [2, 1, 0]].astype(np.uint16) * 63 + 127) // 255).astype(np.uint8).tobytes()
        mid = self.raw[self.tiles_at + self.tiles_len:self.sgfx_at]
        k = pal_at - self.tiles_at - self.tiles_len
        mid = mid[:k] + pal + mid[k + len(pal):]
        return (self.raw[:self.tiles_at] + self.tiles.tobytes() + mid
                + self.sgfx.tobytes()[:self.h['spritegfx']] + self.raw[g_end:])
