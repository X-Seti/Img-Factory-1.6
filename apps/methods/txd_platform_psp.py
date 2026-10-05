#!/usr/bin/env python3
#this belongs in apps/methods/txd_platform_psp.py - Version: 2
# X-Seti - October05 2026 - IMG Factory 1.6 - PSP TXD Platform Parser

"""
PSP NativeTexture read/write plus shared PS2/PSP indexed helpers.
"""

# Native 0x15: Struct(platform 'PSP\0', filter), name, mask strings
# Raster Struct: info(w, h, depth, mips, unk), data Struct
# Data: indices then palette; native size field 24 short
# Pixels PS2 style: PSMT8 swizzle, CSM1 PAL8, alpha 0..128
# PAL8 swizzled when 16 <= w <= 256 and h >= 4
# PAL4 swizzled when 32 <= w <= 256 and h >= 32

import struct
from typing import Dict, Tuple

import numpy as np

##Methods list -
# _chunk_header
# _native_layout
# _nearest
# _pal_lookup
# _quantize
# csm1_map
# decode_indexed
# decode_palette
# detect_psp_txd
# encode_indexed
# encode_palette
# ge_swizzle
# ge_unswizzle
# pack_indices
# parse_psp_nativetex
# ps2_swizzle8_map
# psp_native_end
# psp_txd_swizzled
# rebuild_psp_chunk
# unpack_indices

PSP_PLATFORM_ID = 0x00505350   # 'PSP\0' little-endian
PSP_PLATFORM_TAG = b'PSP\x00'


def _chunk_header(data: bytes, pos: int) -> Tuple[int, int, int]: #vers 1
    """Return (type, size, version) of RW chunk at pos."""
    if pos + 12 > len(data):
        raise ValueError(f"PSP native: chunk header past end at 0x{pos:X}")
    return struct.unpack_from('<3I', data, pos)


def _native_layout(data: bytes, off: int) -> Dict: #vers 1
    """Walk PSP NativeTexture children; return offsets and raster info."""
    t, _, _ = _chunk_header(data, off)
    if t != 0x15:
        raise ValueError(f"PSP native: expected chunk 0x15 at 0x{off:X}, got 0x{t:X}")
    q = off + 12
    t, s, _ = _chunk_header(data, q)
    if t != 0x01 or s < 8:
        raise ValueError("PSP native: missing platform struct")
    if data[q + 12:q + 16] != PSP_PLATFORM_TAG:
        raise ValueError(f"PSP native: platform is {data[q+12:q+16]!r}, not PSP")
    filter_flags = struct.unpack_from('<I', data, q + 16)[0]
    q += 12 + s
    names = []
    for _ in range(2):
        t, s, _ = _chunk_header(data, q)
        if t != 0x02:
            raise ValueError(f"PSP native: expected string chunk at 0x{q:X}")
        names.append(data[q + 12:q + 12 + s].split(b'\x00')[0].decode('latin-1'))
        q += 12 + s
    t, s, _ = _chunk_header(data, q)
    if t != 0x01:
        raise ValueError("PSP native: missing raster struct")
    raster_end = q + 12 + s
    r = q + 12
    t1, s1, _ = _chunk_header(data, r)
    if t1 != 0x01 or s1 < 20:
        raise ValueError("PSP native: bad raster info struct")
    w, h, depth, mips, unk = struct.unpack_from('<5I', data, r + 12)
    t2, s2, _ = _chunk_header(data, r + 12 + s1)
    if t2 != 0x01:
        raise ValueError("PSP native: missing raster data struct")
    data_off = r + 24 + s1
    end = raster_end
    if end + 12 <= len(data) and _chunk_header(data, end)[0] == 0x03:
        end += 12 + _chunk_header(data, end)[1]
    if depth not in (4, 8):
        raise ValueError(f"PSP native '{names[0]}': depth {depth} not supported (PAL4/PAL8 only)")
    if mips != 1:
        raise ValueError(f"PSP native '{names[0]}': {mips} mip levels not supported")
    pix = w * h * depth // 8
    pal = (1 << depth) * 4
    if s2 != pix + pal or data_off + s2 > len(data):
        raise ValueError(f"PSP native '{names[0]}': data size {s2} != {pix + pal}")
    return {'name': names[0], 'mask': names[1], 'filter': filter_flags,
            'width': w, 'height': h, 'depth': depth, 'mips': mips, 'unk': unk,
            'pix_off': data_off, 'pix_size': pix,
            'pal_off': data_off + pix, 'pal_size': pal, 'end': end}


def _nearest(cols: np.ndarray, pal: np.ndarray) -> np.ndarray: #vers 1
    """Nearest palette index per colour, chunked for memory."""
    out = np.empty(len(cols), np.int64)
    for i in range(0, len(cols), 4096):
        c = cols[i:i + 4096]
        out[i:i + 4096] = np.argmin(((c[:, None, :] - pal[None, :, :]) ** 2).sum(2), axis=1)
    return out


def _pal_lookup(rgba: np.ndarray, pal: np.ndarray) -> np.ndarray: #vers 1
    """Index of first exact palette match per pixel, -1 if none."""
    keys = rgba.reshape(-1, 4).copy().view('<u4').ravel()
    pkeys = pal.reshape(-1, 4).copy().view('<u4').ravel()
    order = np.argsort(pkeys, kind='stable')
    sk = pkeys[order]
    pos = np.searchsorted(sk, keys)
    pos_c = np.minimum(pos, len(sk) - 1)
    hit = sk[pos_c] == keys
    return np.where(hit, order[pos_c], -1)


def _quantize(px: np.ndarray, colors: int) -> Tuple[np.ndarray, np.ndarray]: #vers 1
    """Exact palette if it fits, else octree plus k-means refine."""
    keys = px.copy().view('<u4').ravel()
    uniq, inv, cnt = np.unique(keys, return_inverse=True, return_counts=True)
    ucol = uniq.view(np.uint8).reshape(-1, 4)
    pal = np.zeros((colors, 4), np.uint8)
    if len(uniq) <= colors:
        pal[:len(uniq)] = ucol
        return inv.astype(np.uint8), pal
    from PIL import Image
    img = Image.frombytes('RGBA', (len(ucol), 1), ucol.tobytes())
    q = img.quantize(colors=colors, method=Image.Quantize.FASTOCTREE,
                     dither=Image.Dither.NONE)
    cent = np.zeros((colors, 4), np.float64)
    got = np.array(q.getpalette(rawmode='RGBA'), np.float64).reshape(-1, 4)[:colors]
    cent[:len(got)] = got
    uf = ucol.astype(np.float32)
    sel = np.arange(len(uf))
    if len(sel) > 16384:
        sel = np.sort(np.random.default_rng(0).choice(len(uf), 16384, replace=False))
    sf, sc = uf[sel], cnt[sel]
    for _ in range(12):
        lab = _nearest(sf, cent.astype(np.float32))
        w = np.bincount(lab, weights=sc, minlength=colors)
        for c in range(4):
            s = np.bincount(lab, weights=sf[:, c] * sc, minlength=colors)
            cent[:, c] = np.where(w > 0, s / np.maximum(w, 1), cent[:, c])
    pal = np.clip(np.rint(cent), 0, 255).astype(np.uint8)
    lab = _nearest(uf, pal.astype(np.float32))
    return lab[inv].astype(np.uint8), pal


def csm1_map(count: int = 256) -> np.ndarray: #vers 1
    """PS2 CSM1 CLUT order map; self-inverse."""
    p = np.arange(count)
    return (p & 0xE7) | ((p & 8) << 1) | ((p & 16) >> 1)


def decode_indexed(idx: np.ndarray, pal: np.ndarray) -> bytes: #vers 1
    """Indices plus Nx4 palette to RGBA bytes."""
    return np.ascontiguousarray(pal[idx]).tobytes()


def decode_palette(raw: bytes, ps2_alpha: bool, csm1: bool) -> np.ndarray: #vers 1
    """Stored palette bytes to Nx4 RGBA array in index order."""
    pal = np.frombuffer(raw, np.uint8).reshape(-1, 4).copy()
    if csm1:
        pal = pal[csm1_map(len(pal))]
    if ps2_alpha:
        pal[:, 3] = np.minimum(255, pal[:, 3].astype(np.int32) * 2)
    return pal


def detect_psp_txd(platform_id: int) -> bool: #vers 2
    """True if native platform field is 'PSP\\0'."""
    return platform_id == PSP_PLATFORM_ID


def encode_indexed(rgba: bytes, width: int, height: int, old_idx: np.ndarray,
                   old_pal: np.ndarray, colors: int) -> Tuple[np.ndarray, object]: #vers 1
    """RGBA to (indices, new palette or None if old kept)."""
    if len(rgba) != width * height * 4:
        raise ValueError(f"RGBA size {len(rgba)} != {width}x{height}x4")
    px = np.frombuffer(rgba, np.uint8).reshape(-1, 4)
    old_rgba = old_pal[old_idx.ravel()]
    same = np.all(px == old_rgba, axis=1)
    if same.all():
        return old_idx.ravel().astype(np.uint8), None
    look = _pal_lookup(px, old_pal)
    if (look >= 0).all():
        idx = np.where(same, old_idx.ravel(), look).astype(np.uint8)
        return idx, None
    return _quantize(px, colors)


def encode_palette(pal: np.ndarray, ps2_alpha: bool, csm1: bool) -> bytes: #vers 1
    """Nx4 RGBA palette to stored bytes."""
    out = pal.astype(np.uint8).copy()
    if ps2_alpha:
        out[:, 3] = np.minimum(128, (out[:, 3].astype(np.int32) + 1) // 2)
    if csm1:
        out = out[csm1_map(len(out))]
    return out.tobytes()


def ge_swizzle(buf: np.ndarray, stride: int, rows: int) -> np.ndarray: #vers 1
    """Linear rows to PSP GE 16-byte x 8-row block order."""
    if stride % 16 or rows % 8:
        raise ValueError(f"GE swizzle needs stride%16==0, rows%8==0 ({stride}x{rows})")
    b = np.asarray(buf, np.uint8).reshape(rows // 8, 8, stride // 16, 16)
    return b.transpose(0, 2, 1, 3).reshape(-1)


def ge_unswizzle(buf: np.ndarray, stride: int, rows: int) -> np.ndarray: #vers 1
    """PSP GE block order to linear rows (rows x stride)."""
    if stride % 16 or rows % 8:
        raise ValueError(f"GE unswizzle needs stride%16==0, rows%8==0 ({stride}x{rows})")
    b = np.asarray(buf, np.uint8).reshape(rows // 8, stride // 16, 8, 16)
    return b.transpose(0, 2, 1, 3).reshape(rows, stride)


def pack_indices(idx: np.ndarray, depth: int) -> np.ndarray: #vers 1
    """Index array to bytes; PAL4 low nibble first."""
    idx = np.asarray(idx, np.uint8).ravel()
    if depth == 8:
        return idx
    if depth == 4:
        return (idx[0::2] & 15) | ((idx[1::2] & 15) << 4)
    raise ValueError(f"Index depth {depth} not supported")


def parse_psp_nativetex(txd_data: bytes, chunk_offset: int, index: int) -> Dict: #vers 2
    """Decode one PSP NativeTexture chunk to a texture dict."""
    lay = _native_layout(txd_data, chunk_offset)
    w, h, depth = lay['width'], lay['height'], lay['depth']
    raw = np.frombuffer(txd_data, np.uint8, lay['pix_size'], lay['pix_off'])
    lin = unpack_indices(raw, depth)
    if psp_txd_swizzled(w, h, depth):
        lin = lin[ps2_swizzle8_map(w, h)]
    pal = decode_palette(txd_data[lay['pal_off']:lay['pal_off'] + lay['pal_size']],
                         ps2_alpha=True, csm1=(depth == 8))
    rgba = decode_indexed(lin, pal)
    used = np.unique(lin)
    fmt = 'PAL8' if depth == 8 else 'PAL4'
    return {
        'name': lay['name'] or f'texture_{index}',
        'alpha_name': lay['mask'],
        'width': w, 'height': h, 'depth': depth,
        'format': fmt,
        'has_alpha': bool((pal[used, 3] < 255).any()),
        'mipmaps': 1,
        'rgba_data': rgba,
        'mipmap_levels': [{'level': 0, 'width': w, 'height': h, 'rgba_data': rgba}],
        'filter_flags': lay['filter'],
        'platform_id': PSP_PLATFORM_ID,
        'platform': 'PSP',
        'palette': pal.tobytes(),
        'indices': lin.astype(np.uint8).tobytes(),
        'chunk_end': lay['end'],
    }


def ps2_swizzle8_map(width: int, height: int) -> np.ndarray: #vers 1
    """Stored index for each linear PSMT8 pixel."""
    y = np.arange(height)[:, None]
    x = np.arange(width)[None, :]
    block_y = (y & ~0xF) * width
    pos_y = (((y & ~3) >> 1) + (y & 1)) & 0x7
    swap_sel = (((y + 2) >> 2) & 0x1) * 4
    col = pos_y * width * 2 + ((x + swap_sel) & 0x7) * 4
    byte = ((y >> 1) & 1) + ((x >> 2) & 2)
    ids = (block_y + (x & ~0xF) * 2 + col + byte).ravel()
    if ids.max() >= width * height or len(np.unique(ids)) != ids.size:
        raise ValueError(f"PS2 swizzle invalid for {width}x{height}")
    return ids


def psp_native_end(txd_data: bytes, chunk_offset: int) -> int: #vers 1
    """Real end offset of a PSP native, including extension."""
    return _native_layout(txd_data, chunk_offset)['end']


def psp_txd_swizzled(width: int, height: int, depth: int) -> bool: #vers 1
    """Swizzle rule observed in LCS iOS PSP natives."""
    if depth == 8:
        return 16 <= width <= 256 and height >= 4
    if depth == 4:
        return 32 <= width <= 256 and height >= 32
    return False


def rebuild_psp_chunk(chunk_bytes: bytes, tex: Dict) -> bytes: #vers 2
    """Write tex rgba into chunk; same size, format and length."""
    lay = _native_layout(chunk_bytes, 0)
    w, h, depth = lay['width'], lay['height'], lay['depth']
    name = tex.get('name', lay['name'])
    if (int(tex.get('width') or 0), int(tex.get('height') or 0)) != (w, h):
        raise ValueError(f"'{name}': PSP texture size is fixed at {w}x{h}")
    fmt = 'PAL8' if depth == 8 else 'PAL4'
    if tex.get('format') and tex['format'] != fmt:
        raise ValueError(f"'{name}': PSP texture format is fixed at {fmt}, got {tex['format']}")
    rgba = tex.get('rgba_data') or b''
    swz = psp_txd_swizzled(w, h, depth)
    raw = np.frombuffer(chunk_bytes, np.uint8, lay['pix_size'], lay['pix_off'])
    old = unpack_indices(raw, depth)
    smap = ps2_swizzle8_map(w, h) if swz else None
    if swz:
        old = old[smap]
    pal_raw = chunk_bytes[lay['pal_off']:lay['pal_off'] + lay['pal_size']]
    pal = decode_palette(pal_raw, ps2_alpha=True, csm1=(depth == 8))
    idx, new_pal = encode_indexed(rgba, w, h, old, pal, 1 << depth)
    if swz:
        stored = np.empty_like(idx)
        stored[smap] = idx
        idx = stored
    out = bytearray(chunk_bytes)
    out[lay['pix_off']:lay['pix_off'] + lay['pix_size']] = pack_indices(idx, depth).tobytes()
    if new_pal is not None:
        out[lay['pal_off']:lay['pal_off'] + lay['pal_size']] = \
            encode_palette(new_pal, ps2_alpha=True, csm1=(depth == 8))
    return bytes(out)


def unpack_indices(raw: np.ndarray, depth: int) -> np.ndarray: #vers 1
    """Bytes to index array; PAL4 low nibble first."""
    raw = np.asarray(raw, np.uint8).ravel()
    if depth == 8:
        return raw.copy()
    if depth == 4:
        out = np.empty(raw.size * 2, np.uint8)
        out[0::2] = raw & 15
        out[1::2] = raw >> 4
        return out
    raise ValueError(f"Index depth {depth} not supported")


__all__ = ['PSP_PLATFORM_ID', 'csm1_map', 'decode_indexed', 'decode_palette',
           'detect_psp_txd', 'encode_indexed', 'encode_palette', 'ge_swizzle',
           'ge_unswizzle', 'pack_indices', 'parse_psp_nativetex', 'ps2_swizzle8_map',
           'psp_native_end', 'psp_txd_swizzled', 'rebuild_psp_chunk', 'unpack_indices']
