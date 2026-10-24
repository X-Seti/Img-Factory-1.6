#this belongs in apps/methods/retro_mapdata.py - Version: 1
# X-Seti - October 24 2026 - IMG Factory 1.6 - top-down GTA map zones, lights, objects

"""
Editable map data besides blocks.
Zones: GTA1 navigation zones (35-byte records), GTA2 ZONE chunk (type, x,
y, w, h, name length, name - sorted smallest area first).
GTA2 LGHT lights (ARGB, x, y, z, radius as 1.8.7 fixed point, intensity,
shape, on / off time) and MOBJ map objects (x, y fixed point, rotation
0-255, object type). Blank blocks: every cell one ground block.
Layouts from the GTA2 GMP map format document (gtamp.com).
"""

##Methods list -
# _fix
# _unfix
# blank_blocks
# gta2_chunk
# lights
# objects
# set_lights
# set_objects
# set_zones

import struct
from typing import List

FIX = 128.0                              # 1.8.7 fixed point: 128 per block


def _fix(v: float) -> int: #vers 1
    return max(-32768, min(32767, int(round(v * FIX))))


def _unfix(v: int) -> float: #vers 1
    return v / FIX


def gta2_chunk(m, tag: bytes) -> bytes: #vers 1
    return next((p for t, p in getattr(m, 'chunks', []) if t == tag), b'')


def set_zones(m, zones: List[dict]): #vers 1
    """Zones written into the map (GTA1 nav zones / GTA2 ZONE chunk)."""
    if hasattr(m, 'set_zones'):
        m.set_zones(zones)
        return
    if not hasattr(m, 'set_chunk'):
        raise ValueError(f"Zones of a {m.GAME} map can't be written yet")
    out = bytearray()
    for z in sorted(zones, key=lambda z: z['w'] * z['h']):
        name = z['name'].encode('latin-1')[:255]
        out += bytes([z['type'], z['x'], z['y'], z['w'], z['h'], len(name)]) + name
    m.set_chunk(b'ZONE', bytes(out))


def lights(m) -> List[dict]: #vers 1
    """GTA2 LGHT lights (positions in blocks)."""
    d = gta2_chunk(m, b'LGHT')
    out = []
    for o in range(0, len(d) - 15, 16):
        argb, x, y, z, r, inten, shape, on, off = struct.unpack_from('<I4h4B', d, o)
        out.append({'r': (argb >> 16) & 255, 'g': (argb >> 8) & 255, 'b': argb & 255,
                    'x': _unfix(x), 'y': _unfix(y), 'z': _unfix(z), 'radius': _unfix(r),
                    'intensity': inten, 'shape': shape, 'on': on, 'off': off})
    return out


def set_lights(m, items: List[dict]): #vers 1
    if not hasattr(m, 'set_chunk'):
        raise ValueError(f"{m.GAME} maps have no lights")
    if not items and not gta2_chunk(m, b'LGHT'):
        return
    out = b''.join(struct.pack('<I4h4B', (l['r'] << 16) | (l['g'] << 8) | l['b'],
                               _fix(l['x']), _fix(l['y']), _fix(l['z']), _fix(min(8.0, l['radius'])),
                               l['intensity'], l['shape'], l['on'], l['off']) for l in items)
    m.set_chunk(b'LGHT', out)


def objects(m) -> List[dict]: #vers 1
    """GTA2 MOBJ map objects (positions in blocks)."""
    d = gta2_chunk(m, b'MOBJ')
    return [{'x': _unfix(x), 'y': _unfix(y), 'rotation': rot, 'type': t}
            for x, y, rot, t in struct.iter_unpack('<hhBB', d[:len(d) // 6 * 6])]


def set_objects(m, items: List[dict]): #vers 1
    if not hasattr(m, 'set_chunk'):
        raise ValueError(f"{m.GAME} maps have no map objects")
    if not items and not gta2_chunk(m, b'MOBJ'):
        return
    m.set_chunk(b'MOBJ', b''.join(struct.pack('<hhBB', _fix(o['x']), _fix(o['y']), o['rotation'] & 255,
                                              o['type'] & 255) for o in items))


def blank_blocks(m, z: int = 1): #vers 1
    """Every cell one new ground block at level z (lid tile 1); other map data kept."""
    m.blocks = []
    m.columns = [[{} for _x in range(256)] for _y in range(256)]
    b = m.new_block()
    for y in range(256):
        for x in range(256):
            m.set_block(x, y, z, b)
    m.edited = True
