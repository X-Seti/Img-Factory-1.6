#this belongs in apps/methods/gta2_map.py - Version: 3
# X-Seti - October 23 2026 - IMG Factory 1.6 - GTA2 .gmp map read / write

"""
GTA2 .gmp (GBMP 500): chunked file. The PC game reads the DMAP chunk
(32-bit compressed map: 256 x 256 columns of up to 8 blocks). Blocks have
five faces (left, right, top, bottom, lid), arrows and slope_type.
Saving rebuilds DMAP; every other chunk is written back unchanged.
"""

##Methods list -
# _chunks
# face_flags
# face_tile
#
##class Block: -
# copy
# key
#
##class GTA2Map: -
# __init__
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
# set_chunk
# set_face_word
# set_ground_slope
# slope_of
# tile_max
# to_bytes
# top_block
# zones

import struct
from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple

SIZE, LEVELS = 256, 8
GROUND = ('air', 'road', 'pavement', 'field')
ZONE_TYPES = ('general', 'navigation', 'traffic light', 'unused 3', 'unused 4', 'arrow trigger',
              'railway station', 'bus stop', 'general trigger', 'unused 9', 'information',
              'railway entry', 'railway exit', 'railway stop', 'gang', 'local navigation',
              'restart', 'unused 17', 'unused 18', 'unused 19', 'arrest restart')


def face_tile(face: int) -> int: #vers 1
    """Tile number (bits 0-9) of a face or lid word."""
    return face & 0x3FF


def face_flags(face: int) -> dict: #vers 1
    """Wall / bullet / flat / flip / rotation (lid: light in bits 10-11)."""
    return {'wall': bool(face & 0x400), 'bullet': bool(face & 0x800), 'light': (face >> 10) & 3,
            'flat': bool(face & 0x1000), 'flip': bool(face & 0x2000), 'rotation': (face >> 14) & 3}


@dataclass
class Block: #vers 1
    """One map block: faces left, right, top, bottom, lid; arrows; slope_type."""
    left: int = 0
    right: int = 0
    top: int = 0
    bottom: int = 0
    lid: int = 0
    arrows: int = 0
    slope_type: int = 0

    def key(self) -> tuple: #vers 1
        return (self.left, self.right, self.top, self.bottom, self.lid, self.arrows, self.slope_type)

    def copy(self) -> 'Block': #vers 1
        return Block(*self.key())


def _chunks(data: bytes) -> List[Tuple[bytes, bytes]]: #vers 1
    """[(tag, payload)] after the 6-byte file header."""
    out, o = [], 6
    while o + 8 <= len(data):
        tag, n = data[o:o + 4], struct.unpack_from('<I', data, o + 4)[0]
        out.append((tag, data[o + 8:o + 8 + n]))
        o += 8 + n
    return out


class GTA2Map: #vers 1
    """GTA2 map: columns[y][x] = [(z, block index)], blocks, other chunks kept."""

    GAME, SLOPE_MAX = 'GTA2', 63
    LEVELS = LEVELS
    GROUND = GROUND
    ARROWS = ('Green left', 'Green right', 'Green up', 'Green down',
              'Red left', 'Red right', 'Red up', 'Red down')

    def __init__(self): #vers 1
        self.header = b'GBMP' + struct.pack('<H', 500)
        self.chunks: List[Tuple[bytes, bytes]] = []
        self.blocks: List[Block] = []
        self.columns: List[List[Dict[int, int]]] = [[{} for _x in range(SIZE)] for _y in range(SIZE)]
        self.path = ''

    @classmethod
    def from_bytes(cls, data: bytes, path: str = '') -> 'GTA2Map': #vers 1
        """Read a .gmp (DMAP chunk)."""
        if data[:4] != b'GBMP':
            raise ValueError("Not a GTA2 .gmp map")
        m = cls()
        m.header, m.path = data[:6], path
        m.chunks = _chunks(data)
        dmap = next((p for t, p in m.chunks if t == b'DMAP'), None)
        if dmap is None:
            raise ValueError("Map has no DMAP chunk (PC map data)")
        base = struct.unpack_from(f'<{SIZE * SIZE}I', dmap, 0)
        cw = struct.unpack_from('<I', dmap, SIZE * SIZE * 4)[0]
        col_at = SIZE * SIZE * 4 + 4
        cols = struct.unpack_from(f'<{cw}I', dmap, col_at)
        nb_at = col_at + cw * 4
        nb = struct.unpack_from('<I', dmap, nb_at)[0]
        raw = struct.iter_unpack('<5H2B', dmap[nb_at + 4:nb_at + 4 + nb * 12])
        m.blocks = [Block(*b) for b in raw]
        for y in range(SIZE):
            row = m.columns[y]
            for x in range(SIZE):
                w = base[y * SIZE + x]
                height, offset = cols[w] & 0xFF, (cols[w] >> 8) & 0xFF
                row[x] = {z: cols[w + 1 + z - offset] for z in range(offset, height)}
        return m

    def column(self, x: int, y: int) -> Dict[int, int]: #vers 1
        return self.columns[y][x]

    def block_at(self, x: int, y: int, z: int) -> Optional[Block]: #vers 1
        i = self.columns[y][x].get(z)
        return self.blocks[i] if i is not None else None

    def top_block(self, x: int, y: int) -> Tuple[int, Optional[Block]]: #vers 1
        """(z, block) of the highest block with a lid, else (-1, None)."""
        col = self.columns[y][x]
        for z in sorted(col, reverse=True):
            b = self.blocks[col[z]]
            if face_tile(b.lid):
                return z, b
        return -1, None

    def set_block(self, x: int, y: int, z: int, block: Optional[Block]): #vers 1
        """Put a block (new entry, shared blocks untouched) or clear the cell."""
        col = self.columns[y][x]
        if block is None or not any(block.key()):
            col.pop(z, None)
            return
        self.blocks.append(block)
        col[z] = len(self.blocks) - 1

    def new_block(self) -> Block: #vers 1
        return Block(lid=1, slope_type=1)                # road with lid tile 1

    def tile_max(self, face: str) -> int: #vers 1
        return 1023

    def face_caps(self, face: str) -> set: #vers 1
        """Editable flags of a face."""
        return {'rotation', 'flip', 'flat', 'light'} if face == 'lid' else \
            {'rotation', 'flip', 'flat', 'wall', 'bullet'}

    def face_word(self, b: Block, face: str) -> int: #vers 1
        return getattr(b, face)

    def set_face_word(self, b: Block, face: str, word: int): #vers 1
        setattr(b, face, word & 0xFFFF)

    def ground_of(self, b: Block) -> int: #vers 1
        return b.slope_type & 3

    def slope_of(self, b: Block) -> int: #vers 1
        return b.slope_type >> 2

    def set_ground_slope(self, b: Block, ground: int, slope: int): #vers 1
        b.slope_type = (ground & 3) | (slope << 2)

    def arrow_dirs(self, b: Block) -> list: #vers 1
        """(dx, dy, red) per traffic arrow: bits 0-3 green L R U D, 4-7 red."""
        dirs = ((-1, 0), (1, 0), (0, -1), (0, 1))
        return [(*dirs[i % 4], i >= 4) for i in range(8) if b.arrows & (1 << i)]

    def arrows_of(self, b: Block) -> int: #vers 1
        return b.arrows

    def set_arrows(self, b: Block, bits: int): #vers 1
        b.arrows = bits & 0xFF

    def zones(self) -> List[dict]: #vers 1
        """ZONE chunk: type, x, y, w, h, name."""
        data = next((p for t, p in self.chunks if t == b'ZONE'), b'')
        out, o = [], 0
        while o + 6 <= len(data):
            zt, x, y, w, h, n = data[o:o + 6]
            out.append({'type': zt, 'x': x, 'y': y, 'w': w, 'h': h,
                        'name': data[o + 6:o + 6 + n].decode('latin-1')})
            o += 6 + n
        return out

    def set_chunk(self, tag: bytes, payload: bytes): #vers 1
        """Replace (or add) a chunk."""
        for i, (t, _p) in enumerate(self.chunks):
            if t == tag:
                self.chunks[i] = (tag, payload)
                return
        self.chunks.append((tag, payload))

    def to_bytes(self) -> bytes: #vers 1
        """Rebuild DMAP (blocks and columns de-duplicated); other chunks unchanged."""
        uniq, remap = {}, {}
        blocks = []
        for i, b in enumerate(self.blocks):
            k = b.key()
            if k not in uniq:
                uniq[k] = len(blocks)
                blocks.append(b)
            remap[i] = uniq[k]
        words, seen, base, empty = [], {}, [], None
        for y in range(SIZE):
            for x in range(SIZE):
                col = {z: remap[i] for z, i in self.columns[y][x].items()}
                lo, hi, ids = 0, 0, []
                if col:
                    lo, hi = min(col), max(col) + 1
                    for z in range(lo, hi):
                        if z not in col and empty is None:       # gap in a column: empty block
                            empty = len(blocks)
                            blocks.append(Block())
                        ids.append(col.get(z, empty))
                key = (lo, hi, tuple(ids))
                if key not in seen:
                    seen[key] = len(words)
                    words.append(hi | (lo << 8))
                    words.extend(ids)
                base.append(seen[key])
        if len(blocks) > 131072 or len(words) > 131072:
            raise ValueError("Map too big for DMAP (131072 blocks / words)")
        dmap = (struct.pack(f'<{SIZE * SIZE}I', *base) + struct.pack('<I', len(words)) +
                struct.pack(f'<{len(words)}I', *words) + struct.pack('<I', len(blocks)) +
                b''.join(struct.pack('<5H2B', *b.key()) for b in blocks))
        out = bytearray(self.header)
        for tag, payload in self.chunks:
            if tag == b'DMAP':
                payload = dmap
            out += tag + struct.pack('<I', len(payload)) + payload
        return bytes(out)
