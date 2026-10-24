#this belongs in apps/methods/gta1_map.py - Version: 4
# X-Seti - October 23 2026 - IMG Factory 1.6 - GTA1 .cmp map read / write

"""
GTA1 (PC) .cmp map, version 331: 256 x 256 columns of up to 6 blocks
(file level 0 is the top, 5 the ground; shown here as 0 ground .. 5 top),
8-byte blocks (type_map, type_map_ext, left, right, top, bottom, lid),
then object positions, routes, locations and navigation zones, which are
written back unchanged. Faces are shown in the GTA2 word layout so one
block editor serves both games.
"""

##Methods list -
#
##class Block1: -
# copy
# key
#
##class GTA1Map: -
# __init__
# _pack
# _unpack
# arrow_dirs
# arrows_of
# block_at
# column
# face_caps
# face_word
# from_bytes
# ground_of
# new_block
# set_arrows
# set_block
# set_face_word
# set_ground_slope
# set_zones
# slope_of
# tile_max
# to_bytes
# top_block
# zones

import struct
from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple

SIZE, LEVELS = 256, 6
GROUND = ('air', 'water', 'road', 'pavement', 'field', 'building')
ARROWS = ('Up', 'Down', 'Left', 'Right')
_HEADER = '<IBBH5I'                    # version, style, sample, reserved, route, objpos, column, block, nav


@dataclass
class Block1: #vers 1
    """GTA1 block: type_map, type_map_ext and five u8 face tiles."""
    type_map: int = 0
    type_map_ext: int = 0
    left: int = 0
    right: int = 0
    top: int = 0
    bottom: int = 0
    lid: int = 0

    def key(self) -> tuple: #vers 1
        return (self.type_map, self.type_map_ext, self.left, self.right, self.top, self.bottom, self.lid)

    def copy(self) -> 'Block1': #vers 1
        return Block1(*self.key())


class GTA1Map: #vers 1
    """GTA1 map: columns[y][x] = {level (0 ground): block index}."""

    GAME, LEVELS, GROUND, ARROWS, SLOPE_MAX = 'GTA1', LEVELS, GROUND, ARROWS, 63

    def __init__(self): #vers 2
        self.header = ()
        self.blocks: List[Block1] = []
        self.columns: List[List[Dict[int, int]]] = [[{} for _x in range(SIZE)] for _y in range(SIZE)]
        self.tail = b''
        self.path = ''
        self.edited = False                     # PS1 maps rebuild only after an edit

    @classmethod
    def from_bytes(cls, data: bytes, path: str = '') -> 'GTA1Map': #vers 2
        h = struct.unpack_from(_HEADER, data, 0)
        if h[0] != 331:
            raise ValueError(f"GTA1 .cmp map expected (version 331), got {h[0]}")
        m = cls()
        m.header, m.path = h, path
        hs = struct.calcsize(_HEADER)
        col_size, blk_size = h[6], h[7]
        base = struct.unpack_from(f'<{SIZE * SIZE}I', data, hs)
        cols = data[hs + SIZE * SIZE * 4:hs + SIZE * SIZE * 4 + col_size]
        b_at = hs + SIZE * SIZE * 4 + col_size
        m.tail = data[b_at + blk_size:]
        m._unpack(base, cols, data[b_at:b_at + blk_size])
        return m

    def _unpack(self, base, cols: bytes, blocks: bytes): #vers 1
        """Blocks and columns from base (byte offsets into cols), cols, block bytes."""
        self.blocks = [Block1(tm, ext, *faces) for tm, ext, *faces in struct.iter_unpack('<HB5B', blocks)]
        for y in range(SIZE):
            row = self.columns[y]
            for x in range(SIZE):
                off = base[y * SIZE + x]
                height = struct.unpack_from('<H', cols, off)[0]
                ids = struct.unpack_from(f'<{LEVELS - height}H', cols, off + 2)
                row[x] = {LEVELS - 1 - (height + i): b for i, b in enumerate(ids)}

    def _pack(self) -> tuple: #vers 1
        """(base byte offsets, column bytes, block bytes): blocks / columns de-duplicated."""
        uniq, blocks, remap = {}, [], {}
        for i, b in enumerate(self.blocks):
            k = b.key()
            if k not in uniq:
                uniq[k] = len(blocks)
                blocks.append(b)
            remap[i] = uniq[k]
        cols, seen, base = bytearray(), {}, []
        empty = uniq.get(Block1().key())
        for y in range(SIZE):
            for x in range(SIZE):
                col = self.columns[y][x]
                top = max(col) if col else -1
                ids = []
                for lv in range(top, -1, -1):              # top first, down to the ground
                    if lv in col:
                        ids.append(remap[col[lv]])
                    else:
                        if empty is None:
                            empty = len(blocks)
                            blocks.append(Block1())
                        ids.append(empty)
                while len(ids) > 1 and blocks[ids[0]].key() == Block1().key():   # trim empty top
                    ids.pop(0)
                key = tuple(ids)
                if key not in seen:
                    seen[key] = len(cols)
                    cols += struct.pack(f'<H{len(ids)}H', LEVELS - len(ids), *ids)
                base.append(seen[key])
        if len(blocks) > 65535:
            raise ValueError("Too many different blocks for a GTA1 map (65535)")
        return base, bytes(cols), b''.join(struct.pack('<HB5B', *b.key()) for b in blocks)

    def column(self, x: int, y: int) -> Dict[int, int]: #vers 1
        return self.columns[y][x]

    def block_at(self, x: int, y: int, z: int) -> Optional[Block1]: #vers 1
        i = self.columns[y][x].get(z)
        return self.blocks[i] if i is not None else None

    def top_block(self, x: int, y: int) -> Tuple[int, Optional[Block1]]: #vers 1
        col = self.columns[y][x]
        for z in sorted(col, reverse=True):
            b = self.blocks[col[z]]
            if b.lid:
                return z, b
        return -1, None

    def set_block(self, x: int, y: int, z: int, block: Optional[Block1]): #vers 2
        """Put a block or empty the level (an empty block keeps the column whole)."""
        col = self.columns[y][x]
        self.edited = True
        self.blocks.append(block if block is not None else Block1())
        col[z] = len(self.blocks) - 1
        for lv in range(0, z):                         # levels below must exist
            if lv not in col:
                self.blocks.append(Block1())
                col[lv] = len(self.blocks) - 1

    def new_block(self) -> Block1: #vers 1
        return Block1(type_map=4 << 4, lid=1)          # field with lid tile 1

    def tile_max(self, face: str) -> int: #vers 1
        return 255

    def face_caps(self, face: str) -> set: #vers 1
        """Editable flags of a face."""
        return {'rotation', 'flat'} if face == 'lid' else {'flip'}

    def face_word(self, b: Block1, face: str) -> int: #vers 1
        """Face as a GTA2-style word: tile | flat 0x1000 | flip 0x2000 | rotation << 14."""
        tile = getattr(b, face)
        if face == 'lid':
            return tile | (0x1000 if b.type_map & 0x80 else 0) | ((b.type_map >> 14) & 3) << 14
        flip = b.type_map_ext & (0x40 if face in ('left', 'right') else 0x20)
        return tile | (0x2000 if flip else 0)

    def set_face_word(self, b: Block1, face: str, word: int): #vers 1
        setattr(b, face, word & 0xFF)
        if face == 'lid':
            b.type_map = (b.type_map & ~0xC080 & 0xFFFF) | (0x80 if word & 0x1000 else 0) | (((word >> 14) & 3) << 14)
        else:
            bit = 0x40 if face in ('left', 'right') else 0x20
            b.type_map_ext = (b.type_map_ext & ~bit & 0xFF) | (bit if word & 0x2000 else 0)

    def ground_of(self, b: Block1) -> int: #vers 1
        return min((b.type_map >> 4) & 7, len(GROUND) - 1)

    def slope_of(self, b: Block1) -> int: #vers 1
        return (b.type_map >> 8) & 0x3F

    def set_ground_slope(self, b: Block1, ground: int, slope: int): #vers 1
        b.type_map = (b.type_map & ~0x3F70 & 0xFFFF) | (ground << 4) | (slope << 8)

    def arrow_dirs(self, b) -> list: #vers 1
        """(dx, dy, red) per road direction: bits 0-3 up, down, left, right."""
        dirs = ((0, -1), (0, 1), (-1, 0), (1, 0))
        return [(*dirs[i], False) for i in range(4) if b.type_map & (1 << i)]

    def arrows_of(self, b: Block1) -> int: #vers 1
        return b.type_map & 0x0F

    def set_arrows(self, b: Block1, bits: int): #vers 1
        b.type_map = (b.type_map & ~0x0F & 0xFFFF) | (bits & 0x0F)

    def zones(self) -> List[dict]: #vers 2
        """Navigation zones (x, y, w, h, sample, 30-char name) at the end of the file."""
        nav = self.header[8]
        data = self.tail[len(self.tail) - nav:] if nav else b''
        out = []
        for o in range(0, len(data) - 34, 35):
            x, y, w, h, _s = data[o:o + 5]
            if not w or not h:
                continue                                   # unused slot
            out.append({'type': 1, 'x': x, 'y': y, 'w': w, 'h': h, 'sample': _s,
                        'name': data[o + 5:o + 35].split(b'\0', 1)[0].decode('latin-1')})
        return out

    def set_zones(self, zones: List[dict]): #vers 1
        """Navigation zones written back (35-byte records at the end of the file)."""
        nav = self.header[8]
        rec = b''.join(bytes([z['x'], z['y'], z['w'], z['h'], z.get('sample', 0) & 0xFF])
                       + z['name'].encode('latin-1')[:29].ljust(30, b'\0') for z in zones)
        self.tail = (self.tail[:len(self.tail) - nav] if nav else self.tail) + rec
        h = list(self.header)
        h[8] = len(rec)
        self.header = tuple(h)
        self.edited = True

    def to_bytes(self) -> bytes: #vers 2
        """Rebuild columns and blocks (shared, de-duplicated); the rest as read."""
        base, cols, blocks = self._pack()
        h = list(self.header)
        h[6], h[7] = len(cols), len(blocks)
        return (struct.pack(_HEADER, *h) + struct.pack(f'<{SIZE * SIZE}I', *base) + cols + blocks + self.tail)
