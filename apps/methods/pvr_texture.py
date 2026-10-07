#this belongs in apps/methods/pvr_texture.py - Version: 1
# X-Seti - October 07 2026 - IMG Factory 1.6 - PowerVR PVR v2 textures

"""
Loose PowerVR .pvr textures (legacy v2 header, PVRTC 2/4bpp): read and write.
"""

##Methods list -
# _gl_encoding
# _header
# is_pvr_texture
# parse_pvr_texture
# write_pvr_texture

import struct
import numpy as np
from typing import Dict, List

_TAG = b'PVR!'
_HEADER = 52
# legacy pixel format -> (format name, bpp)
_FMT = {0x18: ('PVRTC2', 2), 0x19: ('PVRTC4', 4)}
_MIPMAP, _ALPHA = 0x100, 0x8000


def is_pvr_texture(data: bytes) -> bool: #vers 1
    """True for a legacy PVR v2 file."""
    return len(data) >= _HEADER and data[44:48] == _TAG and \
        struct.unpack_from('<I', data, 0)[0] == _HEADER


def _header(data: bytes) -> Dict: #vers 1
    """Header fields plus per-level (w, h, offset, size)."""
    from apps.methods.mobile_texture_decode import level_size, mip_dims
    h, w, mips, flags, dsize = struct.unpack_from('<5I', data, 4)
    if flags & 0xFF not in _FMT:
        raise ValueError(f"PVR pixel format 0x{flags & 0xFF:02X} not supported")
    fmt, bpp = _FMT[flags & 0xFF]
    alpha = bool(flags & _ALPHA)
    enc = _gl_encoding(bpp, alpha)
    dims = mip_dims(w, h, True)[:mips + 1] if flags & _MIPMAP else [(w, h)]
    levels, off = [], _HEADER
    for lw, lh in dims:
        size = level_size(enc, lw, lh)
        levels.append((lw, lh, off, size))
        off += size
    if off - _HEADER != dsize or off > len(data):
        raise ValueError(f"PVR data size {dsize} != levels {off - _HEADER}")
    return {'width': w, 'height': h, 'format': fmt, 'bpp': bpp, 'alpha': alpha,
            'encoding': enc, 'levels': levels}


def _gl_encoding(bpp: int, alpha: bool) -> int: #vers 1
    """GL PVRTC enum for bpp and alpha flag."""
    from apps.methods.mobile_texture_decode import (GL_PVRTC2_RGB, GL_PVRTC2_RGBA,
                                                    GL_PVRTC4_RGB, GL_PVRTC4_RGBA)
    if bpp == 2:
        return GL_PVRTC2_RGBA if alpha else GL_PVRTC2_RGB
    return GL_PVRTC4_RGBA if alpha else GL_PVRTC4_RGB


def parse_pvr_texture(data: bytes, name: str) -> List[Dict]: #vers 1
    """One workshop texture dict with every mip level decoded."""
    from apps.methods.mobile_texture_decode import decode_level
    hd = _header(data)
    lv = []
    for i, (lw, lh, off, size) in enumerate(hd['levels']):
        rgba = decode_level(hd['encoding'], data[off:off + size], lw, lh).tobytes()
        lv.append({'level': i, 'width': lw, 'height': lh, 'rgba_data': rgba})
    stem = name.replace('\\', '/').rsplit('/', 1)[-1].rsplit('.', 1)[0]
    return [{'name': stem, 'width': hd['width'], 'height': hd['height'],
             'format': hd['format'], 'depth': hd['bpp'], 'has_alpha': hd['alpha'],
             'alpha_name': '', 'mipmaps': len(lv), 'rgba_data': lv[0]['rgba_data'],
             'mipmap_levels': lv, 'raster_format_flags': 0, 'platform': 'PowerVR',
             'compressed_size': len(data) - _HEADER}]


def write_pvr_texture(original: bytes, tex: Dict) -> bytes: #vers 1
    """Re-encode an edited texture in place; size and format fixed."""
    from apps.methods.mobile_texture_decode import decode_level, encode_level
    from apps.methods.txd_splice import _level_rgba
    hd = _header(original)
    if tex is None:
        return original
    if (tex['width'], tex['height']) != (hd['width'], hd['height']):
        raise ValueError(f"'{tex.get('name')}': PVR textures keep their size")
    if tex.get('format') != hd['format']:
        raise ValueError(f"'{tex.get('name')}': PVR textures keep their format")
    out = bytearray(original)
    top = bytes(tex['rgba_data'])
    given = {l.get('level'): l for l in (tex.get('mipmap_levels') or [])}
    for i, (lw, lh, off, size) in enumerate(hd['levels']):
        src = top if i == 0 else None
        l = given.get(i)
        if src is None and l and (l.get('width'), l.get('height')) == (lw, lh) and \
                bytes(l['rgba_data']) != decode_level(hd['encoding'], original[off:off + size], lw, lh).tobytes():
            src = bytes(l['rgba_data'])
        if src is None:
            src = _level_rgba(top, hd['width'], hd['height'], lw, lh)
        out[off:off + size] = encode_level(hd['encoding'], np.frombuffer(src, np.uint8), lw, lh)
    return bytes(out)
