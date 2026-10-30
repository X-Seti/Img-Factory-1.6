#this belongs in apps/methods/retro_missions.py - Version: 3
# X-Seti - October 24 2026 - IMG Factory 1.6 - GTA1 MISSION.INI / GTA2 .scr map markers

"""
Mission objects of the top-down GTAs as map markers.
GTA1 MISSION.INI: '[n]' sections, a 'name, n, map.cmp, n' line, then
objects 'id [1] (x,y,z) TYPE args' - block coordinates, or 1/64 block
(pixel) coordinates when z is above the 8 levels.
GTA2 .scr (compiled): 6000 u16 command offsets from byte 12000, each
command an 8-byte header (this, type, next, return) then its data;
placement commands hold x, y, z as 32-bit values, 16384 per block.
Command numbers / layouts from the Epic GTA2 Script Decompiler (T.M.).
"""

##Methods list -
# _scr_kind
# gta1_line
# gta1_missions
# gta2_put
# gta2_script_markers
# marker_kind

import re
import struct
from typing import List

SCR_ONE = 16384.0                        # GTA2 script coordinate units per block
SCR_BASE = 12000                         # command data start (offsets count from here)

# GTA2 command type -> (name, layout, data offset): 'xyz' x, y, z; 'rect' x, y, z, w, h; 'xy' x, y
# (32-bit, 16384 per block); 'block' u8 x, y, z
_XYZ = {5: 'PLAYER_PED', 7: 'CHAR_DATA', 8: 'CHAR_DATA', 10: 'CAR_DATA', 11: 'CAR_DATA', 12: 'CAR_DATA',
        13: 'CAR_DATA', 425: 'PARKED_CAR_DATA', 426: 'PARKED_CAR_DATA', 427: 'PARKED_CAR_DATA',
        428: 'PARKED_CAR_DATA', 15: 'OBJ_DATA', 16: 'OBJ_DATA', 17: 'OBJ_DATA', 18: 'OBJ_DATA', 19: 'OBJ_DATA',
        20: 'OBJ_DATA', 29: 'GENERATOR', 30: 'GENERATOR', 31: 'GENERATOR', 32: 'GENERATOR', 327: 'SOUND',
        328: 'SOUND', 219: 'LIGHT', 301: 'LIGHT', 302: 'LIGHT'}
SCR_PLACE = {t: (n, 'xyz', 12) for t, n in _XYZ.items()}
SCR_PLACE.update({26: ('CONVEYOR', 'rect', 12), 27: ('CONVEYOR', 'rect', 12),
                  34: ('DESTRUCTOR', 'rect', 12), 35: ('DESTRUCTOR', 'rect', 12),
                  40: ('CRUSHER', 'xy', 12), 37: ('CRANE', 'xy', 16), 223: ('GANG_INFO', 'xyz', 16),
                  212: ('TRIGGER_BLOCK', 'block', 18), 214: ('TRIGGER_AREA', 'rect', 16),
                  434: ('TRIGGER_AREA', 'rect', 16)})
SCR_PLACE.update({t: ('DOOR_DATA', 'block', 12) for t in range(375, 381)})
_N = {'xyz': 3, 'rect': 5, 'xy': 2}

KIND_WORDS = (('player', ('PLAYER',)), ('phone', ('PHONE',)), ('trigger', ('TRIG',)),
              ('door', ('DOOR',)), ('ped', ('PED', 'CHAR')), ('car', ('CAR', 'PARKED', 'CHOPPER')),
              ('light', ('LIGHT',)), ('area', ('CONVEYOR', 'DESTRUCTOR', 'CRUSHER')))

_OBJ = re.compile(r'^(\s*\d+\s+(?:\d+\s+)?)\((-?\d+),(-?\d+),(-?\d+)\)(\s+)(\w+)(.*)$')


def marker_kind(name: str) -> str: #vers 1
    """Colour group of an object / command name."""
    for kind, words in KIND_WORDS:
        if any(w in name for w in words):
            return kind
    return 'object'


def gta1_missions(text: str) -> List[dict]: #vers 2
    """[{'id', 'name', 'map', 'markers'}] from MISSION.INI text."""
    out, cur = [], None
    for ln, line in enumerate(text.splitlines()):
        s = line.strip()
        if s.startswith('[') and s.endswith(']'):
            cur = {'id': s[1:-1], 'name': '', 'map': '', 'markers': []}
            out.append(cur)
            continue
        if cur is None or not s:
            continue
        if not cur['name'] and ',' in s and '(' not in s:
            parts = [p.strip() for p in s.split(',')]
            cur['name'] = parts[0]
            cur['map'] = next((p for p in parts if p.lower().endswith('.cmp')), '')
            continue
        m = _OBJ.match(line)
        if not m:
            continue
        head, x, y, z, gap, typ, args = m.groups()
        x, y, z = int(x), int(y), int(z)
        pixel = z > 8                                   # pixel (1/64 block) coordinates
        if pixel:
            x, y, z = x / 64.0, y / 64.0, z / 64.0
        else:                                           # block: centre of it
            x, y = x + 0.5, y + 0.5
        cur['markers'].append({'x': x, 'y': y, 'z': z, 'w': 0, 'h': 0, 'label': typ,
                               'kind': marker_kind(typ), 'info': f"{head.split()[0]} {typ}{args.rstrip()}",
                               'line': ln, 'fmt': (head, gap, pixel), 'args': args})
    return out


def gta1_line(m: dict) -> str: #vers 1
    """MISSION.INI line of an edited GTA1 marker (coordinates, type, args)."""
    head, gap, pixel = m['fmt']
    if pixel:
        x, y, z = (int(round(v * 64)) for v in (m['x'], m['y'], m['z']))
    else:
        x, y, z = int(m['x']), int(m['y']), int(round(m['z']))
    return f"{head}({x},{y},{z}){gap}{m['label']}{m['args']}"


def _scr_kind(t: int) -> str: #vers 2
    return marker_kind(SCR_PLACE[t][0])


def gta2_script_markers(data: bytes) -> List[dict]: #vers 2
    """Placement commands of a compiled GTA2 script as markers."""
    if len(data) < SCR_BASE + 2:
        raise ValueError("Too small for a GTA2 .scr script")
    ptrs = struct.unpack_from('<6000H', data, 0)
    out = []
    for i, off in enumerate(ptrs):
        at = SCR_BASE + off
        if not off or at + 8 > len(data):
            continue
        this, t = struct.unpack_from('<HH', data, at)
        if this != i or t not in SCR_PLACE:
            continue
        name, lay, fo = SCR_PLACE[t]
        if lay == 'block':
            x, y, z = data[at + fo:at + fo + 3]
            x, y, w, h = x + 0.5, y + 0.5, 0, 0
        else:
            n = _N[lay]
            v = [c / SCR_ONE for c in struct.unpack_from(f'<{n}i', data, at + fo)]
            x, y = v[0], v[1]
            z = v[2] if n >= 3 else 0
            w, h = (v[3], v[4]) if n == 5 else (0, 0)
        if not (0 <= x < 256 and 0 <= y < 256):
            continue
        out.append({'x': x, 'y': y, 'z': z, 'w': w, 'h': h, 'label': name, 'kind': _scr_kind(t),
                    'info': f"{i} {name} ({x:.2f}, {y:.2f}, {z:.2f})", 'at': at + fo, 'lay': lay})
    return out


def gta2_put(data: bytearray, m: dict): #vers 1
    """Write an edited GTA2 marker's position (and area size) back into script bytes."""
    if m['lay'] == 'block':
        data[m['at']:m['at'] + 3] = bytes([int(m['x']) & 255, int(m['y']) & 255, int(round(m['z'])) & 255])
        return
    v = [m['x'], m['y'], m['z'], m['w'], m['h']][:_N[m['lay']]]
    struct.pack_into(f'<{len(v)}i', data, m['at'], *(int(round(c * SCR_ONE)) for c in v))
