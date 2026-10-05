#!/usr/bin/env python3
#this belongs in apps/methods/txd_platform_xbox.py - Version: 2
# X-Seti - October05 2026 - IMG Factory 1.6 - Xbox TXD platform

"""
Xbox texture native (platform 5) read and write, GTA III / VC / SA Xbox.

Struct body:
  +000 u32 platform (5)   +004 u32 filter/addressing
  +008 char[32] name      +040 char[32] mask
  +072 u32 raster format  +076 u16 hasAlpha  +078 u16 cube flag
  +080 u16 width  +082 u16 height  +084 u8 depth  +085 u8 levels
  +086 u8 raster type     +087 u8 compression (0 raw, 0xC..0x10 DXT1..5)
  +088 u32 total pixel bytes (all levels)
  +092 palette (PAL8 1024, PAL4 128 bytes), then all levels back to back.
Raw and palette textures are swizzled (Morton order, x bit first);
PAL4 indices are stored one per byte.
"""

##Methods list -
# _decode_raw
# _dxt_to_rgba
# _level_bytes
# _swizzle_map
# build_xbox_chunk
# detect_xbox_txd
# parse_xbox_nativetex

import io
import struct
from typing import Dict, Optional

import numpy as np

_COMP_FMT = {0x0C: 'DXT1', 0x0D: 'DXT3', 0x0E: 'DXT3', 0x0F: 'DXT5', 0x10: 'DXT5'}
_FMT_COMP = {'DXT1': 0x0C, 'DXT3': 0x0E, 'DXT5': 0x10}
_RASTER_PIX = {0x0100: 'ARGB1555', 0x0200: 'RGB565', 0x0300: 'ARGB4444', 0x0400: 'LUM8',
               0x0500: 'ARGB8888', 0x0600: 'RGB888', 0x0A00: 'RGB555'}
_PIX_RASTER = {v: k for k, v in _RASTER_PIX.items()}
_BPP = {'ARGB8888': 4, 'RGB888': 4, 'ARGB1555': 2, 'RGB565': 2, 'ARGB4444': 2,
        'RGB555': 2, 'LUM8': 1, 'PAL8': 1, 'PAL4': 1}
_SWZ_CACHE: Dict[tuple, np.ndarray] = {}


def detect_xbox_txd(platform_id: int) -> bool: #vers 1
    """True when the NativeTexture platform id is Xbox."""
    return platform_id == 5


def _swizzle_map(w: int, h: int) -> np.ndarray: #vers 1
    """Swizzled element offset for each linear pixel (y*w+x)."""
    key = (w, h)
    if key not in _SWZ_CACHE:
        xs, ys = np.arange(w), np.arange(h)
        mx, my = np.zeros(w, dtype=np.int64), np.zeros(h, dtype=np.int64)
        i = j = 1
        while i < w or i < h:
            if i < w:
                mx |= np.where(xs & i, j, 0)
                j <<= 1
            if i < h:
                my |= np.where(ys & i, j, 0)
                j <<= 1
            i <<= 1
        _SWZ_CACHE[key] = (my[:, None] | mx[None, :]).ravel()
    return _SWZ_CACHE[key]


def _dxt_to_rgba(data: bytes, w: int, h: int, fmt: str) -> bytes: #vers 1
    """DXT block data to RGBA via PIL's DDS reader."""
    from PIL import Image
    four = fmt.encode()
    hdr = (b'DDS ' + struct.pack('<7I', 124, 0x1007, h, w, len(data), 0, 1) + b'\0' * 44
           + struct.pack('<2I4s5I', 32, 4, four, 0, 0, 0, 0, 0) + struct.pack('<5I', 0x1000, 0, 0, 0, 0))
    return Image.open(io.BytesIO(hdr + data)).convert('RGBA').tobytes()


def _level_bytes(fmt: str, w: int, h: int) -> int: #vers 1
    """Stored size of one level."""
    if fmt == 'DXT1':
        return max(1, (w + 3) // 4) * max(1, (h + 3) // 4) * 8
    if fmt in ('DXT3', 'DXT5'):
        return max(1, (w + 3) // 4) * max(1, (h + 3) // 4) * 16
    return w * h * _BPP[fmt]


def _decode_raw(fmt: str, data: bytes, w: int, h: int, pal: Optional[np.ndarray]) -> bytes: #vers 1
    """Linear (unswizzled) element data to RGBA."""
    n = w * h
    if fmt in ('PAL8', 'PAL4'):
        return pal[np.frombuffer(data[:n], dtype=np.uint8).astype(np.int32) % len(pal)].tobytes()
    if fmt in ('ARGB8888', 'RGB888'):
        a = np.frombuffer(data[:n * 4], dtype=np.uint8).reshape(-1, 4)
        out = a[:, [2, 1, 0, 3]].copy()
        if fmt == 'RGB888':
            out[:, 3] = 255
        return out.tobytes()
    if fmt == 'LUM8':
        l = np.frombuffer(data[:n], dtype=np.uint8)
        return np.stack([l, l, l, np.full_like(l, 255)], 1).tobytes()
    v = np.frombuffer(data[:n * 2], dtype='<u2').astype(np.uint32)
    if fmt == 'ARGB1555':
        r, g, b, a = (v >> 10) & 31, (v >> 5) & 31, v & 31, np.where(v >> 15, 255, 0)
        r, g, b = r * 255 // 31, g * 255 // 31, b * 255 // 31
    elif fmt == 'RGB555':
        r, g, b, a = ((v >> 10) & 31) * 255 // 31, ((v >> 5) & 31) * 255 // 31, (v & 31) * 255 // 31, 255 + 0 * v
    elif fmt == 'RGB565':
        r, g, b, a = ((v >> 11) & 31) * 255 // 31, ((v >> 5) & 63) * 255 // 63, (v & 31) * 255 // 31, 255 + 0 * v
    else:                                                   # ARGB4444
        a, r, g, b = ((v >> 12) & 15) * 17, ((v >> 8) & 15) * 17, ((v >> 4) & 15) * 17, (v & 15) * 17
    return np.stack([r, g, b, a], 1).astype(np.uint8).tobytes()


def parse_xbox_nativetex(txd_data: bytes, chunk_offset: int, index: int) -> Optional[Dict]: #vers 2
    """One Xbox NativeTexture chunk -> texture dict with RGBA for every level."""
    if struct.unpack_from('<I', txd_data, chunk_offset)[0] != 0x15:
        return None
    st = chunk_offset + 24
    ssize = struct.unpack_from('<I', txd_data, chunk_offset + 16)[0]
    plat, flt = struct.unpack_from('<II', txd_data, st)
    name = txd_data[st + 8:st + 40].split(b'\0', 1)[0].decode('ascii', 'ignore')
    mask = txd_data[st + 40:st + 72].split(b'\0', 1)[0].decode('ascii', 'ignore')
    rf, has_a, _cube, w, h, depth, levels, _rt, comp, total = \
        struct.unpack_from('<IHHHHBBBBI', txd_data, st + 72)
    pos = st + 92
    pal = None
    if comp in _COMP_FMT:
        fmt = _COMP_FMT[comp]
    elif rf & 0x2000:
        fmt = 'PAL8'
    elif rf & 0x4000:
        fmt = 'PAL4'
    else:
        fmt = _RASTER_PIX.get(rf & 0x0F00, 'ARGB8888')
    if fmt in ('PAL8', 'PAL4'):
        psize = 1024 if fmt == 'PAL8' else 128
        pal = np.frombuffer(txd_data[pos:pos + psize], dtype=np.uint8).reshape(-1, 4)[:, [2, 1, 0, 3]].copy()
        if (rf & 0x0F00) == 0x0600:
            pal[:, 3] = 255
        pos += psize
    data = txd_data[pos:pos + total]
    mips, off, lw, lh = [], 0, w, h
    for lv in range(max(1, levels)):
        n = _level_bytes(fmt, lw, lh)
        raw = data[off:off + n]
        if len(raw) < n:
            break
        if fmt.startswith('DXT'):
            rgba = _dxt_to_rgba(raw, lw, lh, fmt)
        else:
            bpp = _BPP[fmt]
            el = np.frombuffer(raw, dtype=np.uint8).reshape(-1, bpp)
            lin = el[_swizzle_map(lw, lh)].tobytes()
            rgba = _decode_raw(fmt, lin, lw, lh, pal)
        mips.append({'level': lv, 'width': lw, 'height': lh, 'rgba_data': rgba,
                     'compressed_data': raw if fmt.startswith('DXT') else None,
                     'compressed_size': n})
        off += n
        if lw == 1 and lh == 1:
            break
        lw, lh = max(1, lw // 2), max(1, lh // 2)
    rgba0 = mips[0]['rgba_data'] if mips else bytes(w * h * 4)
    return {
        'name': name or f'texture_{index}', 'alpha_name': mask,
        'width': w, 'height': h, 'depth': depth, 'format': fmt,
        'has_alpha': bool(has_a), 'mipmaps': levels, 'rgba_data': rgba0,
        'compressed_data': mips[0]['compressed_data'] if mips and mips[0]['compressed_data'] else b'',
        'mipmap_levels': mips, 'raster_format_flags': rf,
        'platform_id': plat, 'platform': 'XBOX', 'filter_flags': flt,
        'bumpmap_data': b'', 'has_bumpmap': False,
        'reflection_map': b'', 'has_reflection': False,
        'alpha_mask': np.frombuffer(rgba0, dtype=np.uint8)[3::4].tobytes(),
        '_struct_end': st + ssize,
    }


def build_xbox_chunk(tex: Dict, rw_ver: int, ext_payload: bytes = b'') -> bytes: #vers 1
    """Xbox NativeTexture chunk from tex['rgba_data'] in tex['format']."""
    from apps.methods.txd_splice import (_encode_level, _join_native, _level_rgba,
                                         _palette, _with_ext, _RAW, _DXT)
    fmt = str(tex.get('format') or 'DXT1')
    if fmt not in _DXT and fmt not in _PIX_RASTER and fmt not in ('PAL8', 'PAL4'):
        raise ValueError(f"Texture '{tex.get('name')}': format '{fmt}' can't be saved for Xbox")
    if fmt == 'A8L8' or (fmt in _RAW and fmt not in _PIX_RASTER):
        raise ValueError(f"Texture '{tex.get('name')}': format '{fmt}' not supported on Xbox")
    w, h = int(tex.get('width') or 0), int(tex.get('height') or 0)
    rgba = bytes(tex.get('rgba_data') or b'')[:w * h * 4]
    if w <= 0 or h <= 0 or len(rgba) < w * h * 4:
        raise ValueError(f"Texture '{tex.get('name')}' has no pixel data")
    if fmt not in _DXT and (w & (w - 1) or h & (h - 1)):
        raise ValueError(f"Texture '{tex.get('name')}': Xbox needs power-of-two sizes "
                         f"for uncompressed formats ({w}x{h})")
    has_alpha = bool(np.frombuffer(rgba, dtype=np.uint8)[3::4].min() < 255)

    pal = b''
    if fmt in ('PAL8', 'PAL4'):
        pal = _palette(rgba, w, h, 256 if fmt == 'PAL8' else 16)
    src_levels = tex.get('mipmap_levels') or []
    out, lw, lh = [], w, h
    for i in range(max(1, len(src_levels))):
        lv = src_levels[i] if i < len(src_levels) else {}
        src = bytes(lv.get('rgba_data') or b'') if i else rgba
        if len(src) != lw * lh * 4:
            src = _level_rgba(rgba, w, h, lw, lh)
        if fmt in _DXT:
            out.append(_encode_level(fmt, src, lw, lh))
        else:
            if fmt == 'PAL4':
                from apps.methods.txd_splice import _palette_index
                lin = _palette_index(src, pal).tobytes()          # one index per byte
            else:
                lin = _encode_level(fmt, src, lw, lh, pal)
            bpp = _BPP[fmt]
            el = np.frombuffer(lin, dtype=np.uint8).reshape(-1, bpp)
            swz = np.empty_like(el)
            swz[_swizzle_map(lw, lh)] = el
            out.append(swz.tobytes())
        if lw == 1 and lh == 1:
            break
        lw, lh = max(1, lw // 2), max(1, lh // 2)

    if fmt in _DXT:
        rf = 0x0300 if fmt != 'DXT1' else (0x0100 if has_alpha else 0x0200)
        depth, comp = 16, _FMT_COMP[fmt]
    elif fmt in ('PAL8', 'PAL4'):
        rf = (0x2000 if fmt == 'PAL8' else 0x4000) | (0x0500 if has_alpha else 0x0600)
        depth, comp = 8, 0
    else:
        rf, depth, comp = _PIX_RASTER[fmt], {4: 32, 2: 16, 1: 8}[_BPP[fmt]], 0
    if len(out) > 1:
        rf |= 0x8000 | (int(tex.get('raster_format_flags') or 0) & 0x1000)
    stored_pal = b''
    if pal:
        p = np.frombuffer(pal, dtype=np.uint8).reshape(-1, 4)[:, [2, 1, 0, 3]]
        stored_pal = p.tobytes().ljust(1024 if fmt == 'PAL8' else 128, b'\0')
    pixels = b''.join(out)
    name = str(tex.get('name', 'texture')).encode('ascii', 'ignore')[:31].ljust(32, b'\0')
    mask = str(tex.get('alpha_name', '') or '').encode('ascii', 'ignore')[:31].ljust(32, b'\0')
    body = (struct.pack('<II', 5, int(tex.get('filter_flags') or 0x1106)) + name + mask
            + struct.pack('<IHHHHBBBBI', rf, 1 if has_alpha else 0, 0, w, h, depth, len(out), 4,
                          comp, len(pixels)) + stored_pal + pixels)
    return _join_native(rw_ver, [(0x01, body), (0x03, _with_ext(ext_payload, tex, rw_ver))])
