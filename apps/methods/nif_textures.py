#this belongs in apps/methods/nif_textures.py - Version: 2
# X-Seti - October05 2026 - IMG Factory 1.6 - Gamebryo texture packs

"""
Bully PC Gamebryo texture packs (.nft / .txd, NIF 20.3): read and write.
"""

##Methods list -
# _blocks
# _decode
# _encode
# _level_size
# _pal_block
# _pixel_block
# _rename_nif
# _source_textures
# _string_table
# is_nif_textures
# parse_nif_textures
# write_nif_textures

import struct
from typing import Dict, List, Optional

import numpy as np

_MAGIC = b'Gamebryo File Format'
# NiPixelData pixel format -> workshop format name
_FMT = {0: 'RGB888', 1: 'ARGB8888', 2: 'PAL8', 3: 'PAL8', 4: 'DXT1', 5: 'DXT3', 6: 'DXT5'}


def is_nif_textures(data: bytes) -> bool: #vers 1
    """True for a Gamebryo NIF texture pack."""
    return data[:20] == _MAGIC


def _blocks(d: bytes): #vers 1
    """(types, offsets, sizes, strings) from a NIF 20.x header."""
    i = d.index(b'\n') + 1
    ver = struct.unpack_from('<I', d, i)[0]
    if ver < 0x14020007:
        raise ValueError(f"Gamebryo version {ver:#x} not supported")
    p = i + 9
    nb = struct.unpack_from('<I', d, p)[0]; p += 4
    nt = struct.unpack_from('<H', d, p)[0]; p += 2
    types = []
    for _ in range(nt):
        n = struct.unpack_from('<I', d, p)[0]
        types.append(d[p + 4:p + 4 + n].decode('latin1')); p += 4 + n
    idx = struct.unpack_from(f'<{nb}H', d, p); p += 2 * nb
    sizes = struct.unpack_from(f'<{nb}I', d, p); p += 4 * nb
    ns = struct.unpack_from('<I', d, p)[0]; p += 8
    strs = []
    for _ in range(ns):
        n = struct.unpack_from('<I', d, p)[0]
        strs.append(d[p + 4:p + 4 + n].decode('latin1')); p += 4 + n
    p += 4 + 4 * struct.unpack_from('<I', d, p)[0]
    offs = []
    for s in sizes:
        offs.append(p); p += s
    return [types[t & 0x7FFF] for t in idx], offs, sizes, strs


def _pixel_block(d: bytes, o: int) -> Dict: #vers 1
    """NiPixelData fields (20.3 layout)."""
    fmt = struct.unpack_from('<I', d, o)[0]
    p = o + 19 + 40
    pal, nm, _bpp = struct.unpack_from('<iII', d, p); p += 12
    mips = [struct.unpack_from('<III', d, p + 12 * k) for k in range(nm)]
    p += 12 * nm + 8
    return {'fmt': fmt, 'pal': pal, 'mips': mips, 'data': p}


def _pal_block(d: bytes, o: int): #vers 1
    """(offset of RGBA entries, entry count) of a NiPalette."""
    return o + 5, struct.unpack_from('<I', d, o + 1)[0]


def _source_textures(d: bytes): #vers 1
    """[(name, pixel block index)] for each embedded NiSourceTexture."""
    types, offs, _sizes, strs = _blocks(d)
    out = []
    for t, o in zip(types, offs):
        if t != 'NiSourceTexture':
            continue
        p = o + 4
        nx = struct.unpack_from('<I', d, p)[0]; p += 4 + 4 * nx + 4
        if d[p]:                                   # external file, no pixels
            continue
        sidx, ref = struct.unpack_from('<iI', d, p + 1)
        name = strs[sidx] if 0 <= sidx < len(strs) else f"tex_{len(out)}"
        name = name.replace('\\', '/').rsplit('/', 1)[-1].rsplit('.', 1)[0]
        if ref < len(types) and types[ref] == 'NiPixelData':
            out.append((name, ref))
    return types, offs, out


def _level_size(fmt: int, w: int, h: int) -> int: #vers 1
    """Bytes of one stored level."""
    if fmt in (4, 5, 6):
        return ((w + 3) // 4) * ((h + 3) // 4) * (8 if fmt == 4 else 16)
    return w * h * {0: 3, 1: 4, 2: 1, 3: 1}[fmt]


def _decode(fmt: int, raw: bytes, w: int, h: int, pal: Optional[np.ndarray]) -> bytes: #vers 1
    """One level to RGBA bytes."""
    from apps.methods.mobile_texture_decode import decode_dxt, GL_DXT1A, GL_DXT3, GL_DXT5
    if fmt in (4, 5, 6):
        enc = {4: GL_DXT1A, 5: GL_DXT3, 6: GL_DXT5}[fmt]
        return np.ascontiguousarray(decode_dxt(raw, w, h, enc)).tobytes()
    if fmt == 1:
        return bytes(raw[:w * h * 4])
    if fmt == 0:
        a = np.frombuffer(raw, np.uint8, w * h * 3).reshape(-1, 3)
        return np.concatenate([a, np.full((len(a), 1), 255, np.uint8)], 1).tobytes()
    return pal[np.frombuffer(raw, np.uint8, w * h)].tobytes()


def _encode(fmt: int, rgba: bytes, w: int, h: int, pal: Optional[np.ndarray]) -> bytes: #vers 1
    """RGBA bytes to one stored level."""
    from apps.methods.txd_dxt_encode import _encode_dxt1, _encode_dxt3, _encode_dxt5
    a = np.frombuffer(rgba, np.uint8, w * h * 4).reshape(-1, 4)
    if fmt == 4:
        return _encode_dxt1(rgba, w, h, alpha=bool((a[:, 3] < 128).any()))
    if fmt == 5:
        return _encode_dxt3(rgba, w, h)
    if fmt == 6:
        return _encode_dxt5(rgba, w, h)
    if fmt == 1:
        return bytes(rgba[:w * h * 4])
    if fmt == 0:
        return a[:, :3].tobytes()
    from apps.methods.txd_splice import _palette_index
    return _palette_index(rgba, pal.tobytes()).astype(np.uint8).tobytes()


def parse_nif_textures(data: bytes) -> List[Dict]: #vers 1
    """Gamebryo texture pack to workshop texture dicts."""
    types, offs, srcs = _source_textures(data)
    texs = []
    for name, ref in srcs:
        px = _pixel_block(data, offs[ref])
        if px['fmt'] not in _FMT:
            raise ValueError(f"{name}: pixel format {px['fmt']} not supported")
        pal = None
        if px['fmt'] in (2, 3):
            po, n = _pal_block(data, offs[px['pal']])
            pal = np.frombuffer(data, np.uint8, n * 4, po).reshape(-1, 4).copy()
            if px['fmt'] == 2:
                pal[:, 3] = 255
        lv = []
        for i, (w, h, off) in enumerate(px['mips']):
            raw = data[px['data'] + off:px['data'] + off + _level_size(px['fmt'], w, h)]
            lv.append({'level': i, 'width': w, 'height': h,
                       'rgba_data': _decode(px['fmt'], raw, w, h, pal)})
        w, h = px['mips'][0][:2]
        fmt = _FMT[px['fmt']]
        texs.append({'name': name, 'width': w, 'height': h, 'format': fmt,
                     'depth': {'RGB888': 24, 'ARGB8888': 32, 'PAL8': 8, 'DXT1': 4}.get(fmt, 8),
                     'has_alpha': any(b != 255 for b in lv[0]['rgba_data'][3::4]),
                     'alpha_name': '', 'mipmaps': len(lv), 'rgba_data': lv[0]['rgba_data'],
                     'mipmap_levels': lv, 'raster_format_flags': 0, 'platform': 'Bully PC',
                     'compressed_size': sum(_level_size(px['fmt'], a, b) for a, b, _ in px['mips'])})
    return texs


def write_nif_textures(original: bytes, textures: List[Optional[Dict]],
                       names: Optional[List[Optional[str]]] = None) -> bytes: #vers 2
    """Rewrite edited textures in place; None keeps a texture unchanged."""
    from apps.methods.txd_splice import _level_rgba, _palette
    renamed = names and any(names)
    if all(t is None for t in textures) and not renamed:
        return original
    out = bytearray(original)
    types, offs, srcs = _source_textures(original)
    if len(textures) != len(srcs):
        raise ValueError("Bully texture packs can't add or remove textures")
    pal_users = {}
    for _n, ref in srcs:
        px = _pixel_block(original, offs[ref])
        if px['fmt'] in (2, 3):
            pal_users[px['pal']] = pal_users.get(px['pal'], 0) + 1
    for (name, ref), t in zip(srcs, textures):
        if t is None:
            continue
        px = _pixel_block(original, offs[ref])
        w0, h0 = px['mips'][0][:2]
        if (t['width'], t['height']) != (w0, h0):
            raise ValueError(f"'{name}': Bully textures keep their size")
        if t.get('format') != _FMT[px['fmt']]:
            raise ValueError(f"'{name}': Bully textures keep their format")
        top = bytes(t['rgba_data'])
        pal = old_pal = None
        if px['fmt'] in (2, 3):
            po, n = _pal_block(original, offs[px['pal']])
            old_pal = np.frombuffer(original, np.uint8, n * 4, po).reshape(-1, 4).copy()
            if px['fmt'] == 2:
                old_pal[:, 3] = 255
            if pal_users[px['pal']] == 1:
                new = np.frombuffer(_palette(top, w0, h0, n), np.uint8).reshape(-1, 4).copy()
                if px['fmt'] == 2:
                    new[:, 3] = 255
                out[po:po + n * 4] = new.tobytes()
                pal = new
            else:
                pal = old_pal
        given = {l.get('level'): l for l in (t.get('mipmap_levels') or [])}
        for i, (w, h, off) in enumerate(px['mips']):
            src = top if i == 0 else None
            l = given.get(i)
            pos = px['data'] + off
            if src is None and l and (l.get('width'), l.get('height')) == (w, h):
                old = original[pos:pos + _level_size(px['fmt'], w, h)]
                if bytes(l['rgba_data']) != _decode(px['fmt'], old, w, h, old_pal):
                    src = bytes(l['rgba_data'])
            if src is None:
                src = _level_rgba(top, w0, h0, w, h)
            enc = _encode(px['fmt'], src, w, h, pal)
            out[pos:pos + len(enc)] = enc
    return _rename_nif(bytes(out), names) if renamed else bytes(out)


def _string_table(d: bytes): #vers 1
    """(start, end) byte span of the header string table."""
    i = d.index(b'\n') + 1
    p = i + 9
    nb = struct.unpack_from('<I', d, p)[0]; p += 4
    nt = struct.unpack_from('<H', d, p)[0]; p += 2
    for _ in range(nt):
        p += 4 + struct.unpack_from('<I', d, p)[0]
    p += 6 * nb
    start, ns = p, struct.unpack_from('<I', d, p)[0]
    p += 8
    for _ in range(ns):
        p += 4 + struct.unpack_from('<I', d, p)[0]
    return start, p


def _rename_nif(data: bytes, names: List[Optional[str]]) -> bytes: #vers 1
    """New texture names: file name and Filename extra string stems."""
    types, offs, _sizes, strs = _blocks(data)
    strs = list(strs)
    owner = {}
    k = 0
    for t, o in zip(types, offs):
        if t != 'NiSourceTexture':
            continue
        p = o + 4
        nx = struct.unpack_from('<I', data, p)[0]
        extras = struct.unpack_from(f'<{nx}i', data, p + 4)
        p += 4 + 4 * nx + 4
        if data[p]:
            continue
        idxs = [struct.unpack_from('<i', data, p + 1)[0]]
        for ref in extras:
            if 0 <= ref < len(types) and types[ref] == 'NiStringExtraData':
                idxs.append(struct.unpack_from('<i', data, offs[ref] + 4)[0])
        new = names[k] if k < len(names) else None
        k += 1
        for si in set(i for i in idxs if 0 <= i < len(strs)):
            if si in owner and owner[si] != k and new:
                raise ValueError(f"'{new}': name string shared with another texture")
            owner[si] = k
            if not new:
                continue
            path = strs[si].replace('\\', '/')
            head, base = (path.rsplit('/', 1) if '/' in path else ('', path))
            ext = base[base.rfind('.'):] if '.' in base else ''
            sep = '\\' if '\\' in strs[si] else '/'
            strs[si] = (head.replace('/', sep) + sep if head else '') + new + ext
    start, end = _string_table(data)
    raw = [x.encode('latin1') for x in strs]
    table = struct.pack('<II', len(raw), max((len(x) for x in raw), default=0))
    table += b''.join(struct.pack('<I', len(x)) + x for x in raw)
    return data[:start] + table + data[end:]
