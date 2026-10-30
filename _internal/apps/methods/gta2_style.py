#this belongs in apps/methods/gta2_style.py - Version: 3
# X-Seti - October 23 2026 - IMG Factory 1.6 - GTA2 .sty style read / write

"""
GTA2 .sty (GBST 700): 64 x 64 8-bit tiles in 256 x 256 pages (TILE),
sprites in 256-wide pages (SPRG + SPRX index), physical palettes (PPAL,
64 palettes per 64 KB page, BGRA), virtual palette index (PALX) and the
palette bases (PALB). Colour 0 is transparent. Tile edits are written
back into TILE; every other chunk is kept as read.
"""

##Methods list -
# _chunks
#
##class GTA2Style: -
# __init__
# face_global
# face_rgba
# from_bytes
# global_face
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
from typing import Dict, List, Optional, Tuple

import numpy as np

from apps.methods.retro_palette import quantize

TILE, PAGE = 64, 256
PALB_NAMES = ('tile', 'sprite', 'car_remap', 'ped_remap', 'code_obj_remap', 'map_obj_remap',
              'user_remap', 'font_remap')
SPRB_NAMES = ('car', 'ped', 'code_obj', 'map_obj', 'user', 'font')


def _chunks(data: bytes) -> List[Tuple[bytes, bytes]]: #vers 1
    out, o = [], 6
    while o + 8 <= len(data):
        tag, n = data[o:o + 4], struct.unpack_from('<I', data, o + 4)[0]
        out.append((tag, data[o + 8:o + 8 + n]))
        o += 8 + n
    return out


class GTA2Style: #vers 1
    """Tiles, sprites and palettes of a .sty."""

    def __init__(self): #vers 1
        self.header = b'GBST' + struct.pack('<H', 700)
        self.chunks: List[Tuple[bytes, bytes]] = []
        self.c: Dict[bytes, bytes] = {}
        self.tiles = np.zeros((0, PAGE), np.uint8)
        self.palx = np.zeros(0, np.uint16)
        self.palb: Dict[str, int] = {}
        self.ppal = np.zeros((0, 256, 4), np.uint8)
        self.path = ''

    @classmethod
    def from_bytes(cls, data: bytes, path: str = '') -> 'GTA2Style': #vers 2
        if data[:4] != b'GBST':
            raise ValueError("Not a GTA2 .sty style")
        s = cls()
        s.header, s.path = data[:6], path
        s.chunks = _chunks(data)
        s.c = {t: p for t, p in s.chunks}
        if b'TILE' in s.c:
            s.tiles = np.frombuffer(s.c[b'TILE'], np.uint8).reshape(-1, PAGE).copy()
        s.sprg = np.frombuffer(s.c.get(b'SPRG', b''), np.uint8).reshape(-1, PAGE).copy()
        s.palx = np.frombuffer(s.c.get(b'PALX', b''), '<u2')
        s.palb = dict(zip(PALB_NAMES, struct.unpack_from('<8H', s.c[b'PALB']))) if b'PALB' in s.c else {}
        pp = np.frombuffer(s.c.get(b'PPAL', b''), np.uint8)
        pages = pp.reshape(-1, 256, 64, 4)                  # page, colour, palette, BGRA
        s.ppal = pages.transpose(0, 2, 1, 3).reshape(-1, 256, 4).copy()   # physical palette, colour, BGRA
        return s

    def tile_count(self) -> int: #vers 1
        return len(self.tiles) * PAGE // (TILE * TILE)

    def palette(self, virtual: int) -> np.ndarray: #vers 1
        """256 x RGBA (colour 0 alpha 0) of a virtual palette."""
        phys = int(self.palx[virtual]) if virtual < len(self.palx) else 0
        bgra = self.ppal[phys] if phys < len(self.ppal) else np.zeros((256, 4), np.uint8)
        rgba = bgra[:, [2, 1, 0, 3]].copy()
        rgba[:, 3] = 255
        rgba[0, 3] = 0
        return rgba

    def palette_count(self) -> int: #vers 1
        """Physical palettes (PPAL)."""
        return len(self.ppal)

    def palette_rgba(self, i: int) -> np.ndarray: #vers 1
        """256 x RGBA of physical palette i."""
        rgba = self.ppal[i][:, [2, 1, 0, 3]].copy()
        rgba[:, 3] = 255
        return rgba

    def set_palette_rgba(self, i: int, rgba: np.ndarray): #vers 1
        self.ppal[i, :, :3] = rgba[:256, [2, 1, 0]]

    def tile_rgba(self, n: int) -> np.ndarray: #vers 1
        """64 x 64 x 4 of tile n (tile palettes start at virtual 0); blank past the last tile."""
        if n >= self.tile_count():
            return np.zeros((TILE, TILE, 4), np.uint8)
        page, k = divmod(n, 16)
        ty, tx = divmod(k, 4)
        idx = self.tiles[page * PAGE + ty * TILE:page * PAGE + (ty + 1) * TILE, tx * TILE:(tx + 1) * TILE]
        return self.palette(n)[idx]

    def face_global(self, face: str, n: int) -> Optional[int]: #vers 1
        """Global tile of a face tile number (GTA2: the same)."""
        return n if 0 <= n < self.tile_count() else None

    def global_face(self, face: str, g: int) -> Optional[int]: #vers 1
        return g if 0 <= g < self.tile_count() else None

    def face_rgba(self, face: str, n: int) -> np.ndarray: #vers 1
        return self.tile_rgba(n)

    def set_tile_rgba(self, n: int, rgba: np.ndarray): #vers 2
        """Write tile n from 64 x 64 RGBA, nearest colour of its own palette (alpha 0 -> 0)."""
        idx = quantize(rgba, self.palette(n))
        page, k = divmod(n, 16)
        ty, tx = divmod(k, 4)
        self.tiles[page * PAGE + ty * TILE:page * PAGE + (ty + 1) * TILE, tx * TILE:(tx + 1) * TILE] = idx

    def sprite_count(self) -> int: #vers 1
        return len(self.c.get(b'SPRX', b'')) // 8

    def sprite_bases(self) -> Dict[str, int]: #vers 1
        """First sprite number of each group (car, ped, code_obj, map_obj, user, font)."""
        counts = struct.unpack_from('<6H', self.c[b'SPRB']) if b'SPRB' in self.c else (0,) * 6
        out, at = {}, 0
        for name, n in zip(SPRB_NAMES, counts):
            out[name] = at
            at += n
        return out

    def sprite_rgba(self, n: int, remap: Optional[int] = None) -> np.ndarray: #vers 2
        """h x w x 4 of sprite n (palette: sprite base + n, or a remap palette)."""
        ptr, w, h = struct.unpack_from('<IBB', self.c[b'SPRX'], n * 8)
        sprg = self.sprg
        page, off = divmod(ptr, PAGE * PAGE)
        y0, x0 = divmod(off, PAGE)
        idx = sprg[page * PAGE + y0:page * PAGE + y0 + h, x0:x0 + w]
        pal = remap if remap is not None else self.palb.get('tile', 0) + n
        return self.palette(pal)[idx]

    def set_sprite_rgba(self, n: int, rgba: np.ndarray): #vers 1
        """Write sprite n from RGBA of its size (nearest colour of its own palette)."""
        ptr, w, h = struct.unpack_from('<IBB', self.c[b'SPRX'], n * 8)
        if rgba.shape[:2] != (h, w):
            raise ValueError(f"GTA2 sprite {n} is {w} x {h}")
        page, off = divmod(ptr, PAGE * PAGE)
        y0, x0 = divmod(off, PAGE)
        self.sprg[page * PAGE + y0:page * PAGE + y0 + h, x0:x0 + w] = \
            quantize(rgba, self.palette(self.palb.get('tile', 0) + n))

    def to_bytes(self) -> bytes: #vers 3
        """Style with the TILE, SPRG and PPAL chunks from the edited tiles / sprites / palettes."""
        out = bytearray(self.header)
        for tag, payload in self.chunks:
            if tag == b'TILE':
                payload = self.tiles.tobytes()
            elif tag == b'SPRG':
                payload = self.sprg.tobytes()
            elif tag == b'PPAL':
                payload = self.ppal.reshape(-1, 64, 256, 4).transpose(0, 2, 1, 3).tobytes()
            out += tag + struct.pack('<I', len(payload)) + payload
        return bytes(out)
