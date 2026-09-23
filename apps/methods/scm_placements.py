#!/usr/bin/env python3
#this belongs in apps/methods/scm_placements.py - Version: 1
# X-Seti - September23 2026 - IMG Factory 1.6 - SCM script placement scanner

"""
SCM script placement scanner - finds CREATE_OBJECT, CREATE_PICKUP,
CREATE_PICKUP_WITH_AMMO and CREATE_CAR_GENERATOR in a compiled main.scm
(III / VC / SA) by opcode pattern and typed parameters. Shared by Map
Workshop and SCM Workshop.
"""

##Methods list -
# read_param
# scan_placements
# used_object_names

import struct
from typing import List

# opcode: (kind, param count, index of x in params, index of model param or None)
OPCODES = {
    0x0107: ('object',  5, 1, 0),
    0x0213: ('pickup',  6, 2, 0),
    0x032B: ('pickup',  7, 3, 0),
    0x014B: ('cargen', 13, 0, 4),
}


def read_param(data: bytes, pos: int, game: str): #vers 1
    """(value, next pos, kind) for one typed parameter; kind 'var' for variables; None if not numeric."""
    if pos >= len(data):
        return None
    t = data[pos]
    p = pos + 1
    try:
        if t == 0x01:
            return struct.unpack_from('<i', data, p)[0], p + 4, 'int'
        if t == 0x04:
            return struct.unpack_from('<b', data, p)[0], p + 1, 'int'
        if t == 0x05:
            return struct.unpack_from('<h', data, p)[0], p + 2, 'int'
        if t == 0x06:
            if game == 'gta3':                             # III: 16-bit fixed point / 16
                return struct.unpack_from('<h', data, p)[0] / 16.0, p + 2, 'float'
            return struct.unpack_from('<f', data, p)[0], p + 4, 'float'
        if t in (0x02, 0x03):
            return None, p + 2, 'var'
    except struct.error:
        return None
    return None


def used_object_names(data: bytes) -> List[str]: #vers 1
    """Header 'used objects' table (for negative model IDs); [] if not found."""
    try:
        if data[0:3] != b'\x02\x00\x01':
            return []
        seg2 = struct.unpack_from('<i', data, 3)[0]
        if data[seg2:seg2 + 3] != b'\x02\x00\x01':
            return []
        for skip in (8, 7):                                 # VC / SA have a segment id byte, III not
            count = struct.unpack_from('<i', data, seg2 + skip)[0]
            start = seg2 + skip + 4
            if 0 < count < 5000 and start + count * 24 <= len(data):
                names = [data[start + k * 24:start + (k + 1) * 24].split(b'\0', 1)[0].decode('latin-1', 'replace')
                         for k in range(count)]
                if all(n == '' or n.isprintable() for n in names):
                    return names
    except (struct.error, IndexError):
        pass
    return []


def scan_placements(data: bytes, game: str, world=4000.0) -> List[dict]: #vers 1
    """[{kind, offset, x, y, z, model, model_name}] for script-placed objects, pickups, car generators."""
    names = used_object_names(data)
    out = []
    n = len(data) - 2
    for off in range(0, n):
        op = data[off] | (data[off + 1] << 8)
        spec = OPCODES.get(op)
        if spec is None:
            continue
        kind, count, xi, mi = spec
        pos, vals, kinds = off + 2, [], []
        ok = True
        for _ in range(count):
            r = read_param(data, pos, game)
            if r is None:
                ok = False
                break
            v, pos, k = r
            vals.append(v)
            kinds.append(k)
        if not ok or kinds[xi:xi + 3] != ['float'] * 3:
            continue
        x, y, z = vals[xi:xi + 3]
        if not (abs(x) <= world and abs(y) <= world and -300.0 <= z <= 2000.0):
            continue
        model = vals[mi] if mi is not None and kinds[mi] == 'int' else None
        mname = ''
        if isinstance(model, int) and model < 0 and -model < len(names):
            mname = names[-model]
        out.append({'kind': kind, 'offset': off, 'x': x, 'y': y, 'z': z,
                    'model': model, 'model_name': mname})
    return out
