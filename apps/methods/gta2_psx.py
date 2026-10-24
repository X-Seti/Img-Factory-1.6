#this belongs in apps/methods/gta2_psx.py - Version: 5
# X-Seti - October 24 2026 - IMG Factory 1.6 - PS1 GTA2 .gmp map and .sty tiles, sprites

"""
PS1 GTA2 districts (WIL / STE / BIL .gmp + .sty).
Map: 'CMAP' chunk first (no GBMP header) - base u16[256 x 256] word offsets,
column words u16, columns (u8 height, u8 offset, u16 block ids; 0x8000 =
lid-only block), 0x400 pad, u16 count + 12-byte blocks, 0x600 pad,
u16 count + 4-byte lid-only blocks (lid, arrows, slope). Other chunks
(ZONE, ANIM, RGEN) follow, each padded with 0xAA; kept unchanged.
Style: 384 tiles of 32 x 32 at the file end, 6 pages of 256 x 256 4-bit
pixels each followed by 64 CLUTs (one 16-colour PS1 CLUT per tile).
Before them: 256-colour palettes (512 bytes each), and before those 7.5
sprite pages 256 bytes wide. Header: 7 sprite group counts. Sprite palette
table (u16 per sprite) follows an 0xFFFF marker, then 8-byte sprite
entries: w bytes, h, pixel width (0 = 8-bit), mirror flag, offset, page.
Mirrored sprites (cars) hold the left half only.
"""

##Methods list -
#
##class GTA2PSXMap: -
# _cmap
# from_bytes
# set_chunk
# to_bytes
# zones
#
##class GTA2PSXStyle: -
# __init__
# _clut
# _pal_at
# _place
# _sprite_pal
# palette_count
# palette_rgba
# set_palette_rgba
# _sprites
# face_global
# face_rgba
# from_bytes
# global_face
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

from apps.methods.gta2_map import SIZE, Block, GTA2Map
from apps.methods.retro_palette import ps1_rgba, put_4bit, quantize, rgba_ps1

TILE, TILES, PER_PAGE = 32, 384, 64
PAGE_PIX = 256 * 128                     # 256 x 256 4-bit
PAGE = PAGE_PIX + PER_PAGE * 32          # pixels + 64 CLUTs
PAD1, PAD2, LID_ONLY = 0x400, 0x600, 0x8000
SPRITE_GROUPS = ('car', 'ped', 'code_obj', 'map_obj', 'user', 'font', 'delta')
SPRITE_PIX = 0x78000                     # 7.5 pages of 256 x 256 bytes
DELTA_PAL = 0                            # car deltas: base car palette


class GTA2PSXMap(GTA2Map): #vers 1
    """GTA2 PS1 map: CMAP chunk read into the PC column / block model."""

    GAME = 'GTA2 PS1'

    @classmethod
    def from_bytes(cls, data: bytes, path: str = '') -> 'GTA2PSXMap': #vers 1
        if data[:4] != b'CMAP':
            raise ValueError("Not a PS1 GTA2 .gmp map")
        m = cls()
        m.path, m.header = path, b''
        m.cmap_size = struct.unpack_from('<I', data, 4)[0]
        o = 8
        base = struct.unpack_from(f'<{SIZE * SIZE}H', data, o)
        o += SIZE * SIZE * 2
        cw = struct.unpack_from('<H', data, o)[0]
        cols = struct.unpack_from(f'<{cw}H', data, o + 2)
        o += 2 + cw * 2 + PAD1
        nc = struct.unpack_from('<H', data, o)[0]
        full = [Block(*b) for b in struct.iter_unpack('<5H2B', data[o + 2:o + 2 + nc * 12])]
        o += 2 + nc * 12 + PAD2
        npart = struct.unpack_from('<H', data, o)[0]
        part = [Block(0, 0, 0, 0, *b) for b in struct.iter_unpack('<H2B', data[o + 2:o + 2 + npart * 4])]
        o += 2 + npart * 4
        m.blocks = full + part
        for y in range(SIZE):
            row = m.columns[y]
            for x in range(SIZE):
                w = base[y * SIZE + x]
                height, offset = cols[w] & 0xFF, cols[w] >> 8
                ids = cols[w + 1:w + 1 + height - offset]
                row[x] = {offset + i: (nc + (b & 0x7FFF) if b & LID_ONLY else b) for i, b in enumerate(ids)}
        while o < len(data) and data[o] == 0xAA:                # CMAP padding
            o += 1
        m.tail = data[o:]
        m.chunks = []
        t = 0
        while t + 8 <= len(m.tail):
            tag, n = m.tail[t:t + 4], struct.unpack_from('<I', m.tail, t + 4)[0]
            m.chunks.append((tag, m.tail[t + 8:t + 8 + n]))
            t += 8 + n
            while t < len(m.tail) and m.tail[t] == 0xAA:
                t += 1
        return m

    def _cmap(self) -> bytes: #vers 1
        """CMAP payload: full / lid-only blocks split, columns shared."""
        empty = Block().key()
        full, part = {}, {empty: 0}
        for b in self.blocks:
            k = b.key()
            if k[:4] == (0, 0, 0, 0):
                part.setdefault(k, len(part))
            else:
                full.setdefault(k, len(full))

        def blockd(i):
            k = self.blocks[i].key()
            return LID_ONLY | part[k] if k in part else full[k]

        words, seen, base = [], {}, []
        for y in range(SIZE):
            for x in range(SIZE):
                col = {z: i for z, i in self.columns[y][x].items() if self.blocks[i].key() != empty}
                if col:
                    lo, hi = min(col), max(col) + 1
                    ids = tuple(blockd(col[z]) if z in col else LID_ONLY for z in range(lo, hi))
                else:
                    lo = hi = 0
                    ids = ()
                key = (lo, hi, ids)
                if key not in seen:
                    seen[key] = len(words)
                    words.append(hi | (lo << 8))
                    words.extend(ids)
                base.append(seen[key])
        if len(words) > 0xFFFF or len(full) > 0x7FFF or len(part) > 0x7FFF:
            raise ValueError("Map too big for a PS1 CMAP (65535 column words / 32767 blocks)")
        out = bytearray(struct.pack(f'<{SIZE * SIZE}H', *base))
        out += struct.pack(f'<H{len(words)}H', len(words), *words) + bytes(PAD1)
        out += struct.pack('<H', len(full)) + b''.join(struct.pack('<5H2B', *k) for k in full)
        out += bytes(PAD2)
        out += struct.pack('<H', len(part)) + b''.join(struct.pack('<H2B', *k[4:]) for k in part)
        return bytes(out)

    def to_bytes(self) -> bytes: #vers 1
        """CMAP rebuilt, 0xAA pad (at least one dword), other chunks as read."""
        cmap = self._cmap()
        out = bytearray(b'CMAP' + struct.pack('<I', len(cmap)) + cmap)
        out += b'\xAA' * (4 - len(out) % 4 if len(out) % 4 else 4)
        return bytes(out + self.tail)

    def set_chunk(self, tag: bytes, payload: bytes): #vers 1
        """Replace (or add) a chunk after CMAP; the tail is rebuilt with 0xAA padding."""
        GTA2Map.set_chunk(self, tag, payload)
        out = bytearray()
        for t, p in self.chunks:
            out += t + struct.pack('<I', len(p)) + p
            out += b'\xAA' * (4 - len(out) % 4 if len(out) % 4 else 4)
        self.tail = bytes(out)

    def zones(self) -> List[dict]: #vers 1
        """ZONE chunk (same layout as the PC map)."""
        return GTA2Map.zones(self)


class GTA2PSXStyle: #vers 2
    """Tiles and sprites of a PS1 GTA2 .sty (same API as GTA2Style)."""

    def __init__(self): #vers 2
        self.data = bytearray()
        self.path = ''
        self.tile_at = 0
        self.counts = ()
        self.entries = []
        self.palx = ()
        self.pal_at = 0
        self.pix_at = 0

    @classmethod
    def from_bytes(cls, data: bytes, path: str = '') -> 'GTA2PSXStyle': #vers 2
        if data[:4] == b'GBST':
            raise ValueError("PC GTA2 style, not PS1")
        if len(data) < TILES // PER_PAGE * PAGE:
            raise ValueError("Too small for a PS1 GTA2 .sty")
        s = cls()
        s.data, s.path = bytearray(data), path
        s.tile_at = len(data) - TILES // PER_PAGE * PAGE
        s._sprites()
        return s

    def _sprites(self): #vers 1
        """Sprite counts, palette table, entries, palette and pixel offsets."""
        d = bytes(self.data)
        self.counts = struct.unpack_from('<7H', d, 0)
        n = sum(self.counts)
        mark = next((o for o in range(0x70, 0x4000, 2) if d[o:o + 2] == b'\xff\xff'), None)
        if mark is None:
            raise ValueError("PS1 GTA2 .sty: sprite palette table not found")
        best, run_at, o, run = 0, 0, mark + 2, 0
        while o + 8 <= self.tile_at and o < mark + 0x10000:          # longest run of valid entries
            w, h, _a, _b, _p, pg = struct.unpack_from('<4BHH', d, o)
            if w and h and pg < 8:
                run += 1
                if run > best:
                    best, run_at = run, o - 8 * (run - 1)
                o += 8
            else:
                o, run = o + 2 - 8 * run if run else o + 2, 0
        if best < n:
            raise ValueError("PS1 GTA2 .sty: sprite table not found")
        at = run_at + 8 * (best - n)
        palx = struct.unpack_from(f'<{(at - mark - 2) // 2}H', d, mark + 2)
        self.palx = palx[:next((i for i, v in enumerate(palx) if v >= 1024), len(palx))]
        self.entries = [struct.unpack_from('<4BHH', d, at + i * 8) for i in range(n)]
        npal = max(self.palx) + 1
        self.pal_at = self.tile_at - npal * 512
        self.pix_at = self.pal_at - SPRITE_PIX
        if self.pix_at < at + 8 * n:
            raise ValueError("PS1 GTA2 .sty: sprite pages overlap the sprite table")

    def tile_count(self) -> int: #vers 1
        return TILES

    def _place(self, n: int) -> Tuple[int, int, int]: #vers 1
        """(page start, tile x, tile y) of tile n."""
        page, i = divmod(n, PER_PAGE)
        return self.tile_at + page * PAGE, i % 8, i // 8

    def _clut(self, n: int) -> np.ndarray: #vers 1
        at = self._place(n)[0] + PAGE_PIX + (n % PER_PAGE) * 32
        return np.frombuffer(bytes(self.data[at:at + 32]), '<u2')

    def palette_count(self) -> int: #vers 1
        """Tile CLUTs (16 colours, one per tile), then sprite palettes (256)."""
        return TILES + max(self.palx) + 1

    def _pal_at(self, i: int) -> Tuple[int, int]: #vers 1
        """(offset, colours) of palette i."""
        if i < TILES:
            return self._place(i)[0] + PAGE_PIX + (i % PER_PAGE) * 32, 16
        return self.pal_at + (i - TILES) * 512, 256

    def palette_rgba(self, i: int) -> np.ndarray: #vers 1
        at, n = self._pal_at(i)
        return ps1_rgba(np.frombuffer(bytes(self.data[at:at + n * 2]), '<u2'))

    def set_palette_rgba(self, i: int, rgba: np.ndarray): #vers 1
        at, n = self._pal_at(i)
        old = np.frombuffer(bytes(self.data[at:at + n * 2]), '<u2')
        self.data[at:at + n * 2] = rgba_ps1(rgba[:n], old).astype('<u2').tobytes()

    def tile_rgba(self, n: int) -> np.ndarray: #vers 1
        if not 0 <= n < TILES:
            return np.zeros((TILE, TILE, 4), np.uint8)
        at, tx, ty = self._place(n)
        pg = np.frombuffer(bytes(self.data[at:at + PAGE_PIX]), np.uint8).reshape(256, 128)
        sub = pg[ty * TILE:(ty + 1) * TILE, tx * 16:(tx + 1) * 16]
        idx = np.empty((TILE, TILE), np.uint8)
        idx[:, 0::2], idx[:, 1::2] = sub & 15, sub >> 4
        return ps1_rgba(self._clut(n))[idx]

    def face_global(self, face: str, n: int) -> Optional[int]: #vers 1
        return n if 0 <= n < TILES else None

    def global_face(self, face: str, g: int) -> Optional[int]: #vers 1
        return g if 0 <= g < TILES else None

    def face_rgba(self, face: str, n: int) -> np.ndarray: #vers 1
        return self.tile_rgba(n)

    def set_tile_rgba(self, n: int, rgba: np.ndarray): #vers 2
        """Tile n from 32 x 32 RGBA: nearest of its CLUT colours (alpha < 128 -> transparent)."""
        if rgba.shape[:2] != (TILE, TILE):
            raise ValueError(f"PS1 GTA2 tiles are {TILE} x {TILE}")
        at, tx, ty = self._place(n)
        page = np.frombuffer(self.data, np.uint8, PAGE_PIX, at).reshape(256, 128)
        put_4bit(page, tx * TILE, ty * TILE, quantize(rgba, ps1_rgba(self._clut(n))))

    def _sprite_pal(self, n: int) -> np.ndarray: #vers 1
        """256 x RGBA palette of sprite n (car deltas: base car palette)."""
        pal = self.palx[n] if n < sum(self.counts[:-1]) else DELTA_PAL
        return ps1_rgba(np.frombuffer(bytes(self.data[self.pal_at + pal * 512:self.pal_at + pal * 512 + 512]), '<u2'))

    def set_sprite_rgba(self, n: int, rgba: np.ndarray): #vers 1
        """Sprite n from RGBA of sprite_rgba's size (mirrored sprites: left half kept)."""
        w, h, px, mirror, off, page = self.entries[n]
        want = self.sprite_rgba(n).shape[:2]
        if rgba.shape[:2] != want:
            raise ValueError(f"PS1 GTA2 sprite {n} is {want[1]} x {want[0]}")
        width = px if px else w
        pal = self._sprite_pal(n)
        if px:
            pal = pal[:16]
        idx = quantize(np.ascontiguousarray(rgba[:, :width]), pal)
        at = self.pix_at + page * 65536
        rows = np.frombuffer(self.data, np.uint8, len(self.data) - at, at)
        rows = rows[:(len(rows) // 256) * 256].reshape(-1, 256)
        y, x = divmod(off, 256)
        if px:
            put_4bit(rows, x * 2, y, idx)
        else:
            rows[y:y + h, x:x + w] = idx

    def sprite_count(self) -> int: #vers 2
        return len(self.entries)

    def sprite_bases(self) -> Dict[str, int]: #vers 2
        """First sprite of each group (car, ped, code_obj, map_obj, user, font, delta)."""
        out, at = {}, 0
        for name, n in zip(SPRITE_GROUPS, self.counts):
            out[name] = at
            at += n
        return out

    def sprite_rgba(self, n: int) -> np.ndarray: #vers 3
        """h x w x 4 of sprite n; 8-bit or 4-bit, mirrored halves joined."""
        w, h, px, mirror, off, page = self.entries[n]
        if w == 1 and h == 1:                                  # unused slot
            return np.zeros((1, 1, 4), np.uint8)
        at = self.pix_at + page * 65536 + off
        rows = np.frombuffer(bytes(self.data[at:at + (h - 1) * 256 + w]) + bytes(256 - w), np.uint8)
        sub = rows.reshape(h, 256)[:, :w]
        if px:                                                 # 4-bit
            idx = np.empty((h, w * 2), np.uint8)
            idx[:, 0::2], idx[:, 1::2] = sub & 15, sub >> 4
            idx = idx[:, :px]
        else:
            idx = sub
        a = self._sprite_pal(n)[idx]
        a[idx == 0, 3] = 0
        return np.ascontiguousarray(np.hstack([a, a[:, ::-1]]) if mirror else a)

    def to_bytes(self) -> bytes: #vers 1
        return bytes(self.data)
