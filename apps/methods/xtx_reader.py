#this belongs in apps/methods/xtx_reader.py - Version: 2
# X-Seti - October05 2026 - IMG Factory 1.6 - Stories XTX Texture Reader Writer

"""
Stories xet texture files (.xtx VCS, .chk LCS), PS2 and PSP.
"""

# Header 0x20: ident, shrink, file_end, data_end, reloc_tab, num_relocs
# 0x20 RslTexList type 6; +0x08 texture link list head
# RslTexture: raster, dict, link next/prev, name[32], mask[32]
# Raster platform chosen by which data pointer is relocated
# PSP: 0, data, u16 stride, log2w, log2h, flags(depth, mips)
# PSP pixels GE swizzled 16x8 blocks, alpha 0..255
# PS2 VCS: 0, gs, data, flags; PS2 LCS: data, flags
# PS2 flags: log2w 0-5, log2h 6-11, depth 12-17, mips 20-23
# PS2 pixels linear, alpha 0..128, PAL8 CLUT CSM1 order
# Palette always follows pixel data

import os
import struct
from typing import Dict, List, Optional, Tuple

import numpy as np

from apps.methods.txd_platform_psp import (
    decode_indexed, decode_palette, encode_indexed, encode_palette,
    ge_swizzle, ge_unswizzle, pack_indices, unpack_indices)

##Methods list -
# _raster_layout
# _raster_try
# is_xtx
# parse_stories_textures
# parse_xtx
# read_xtx
# write_stories_textures
# write_xtx
# xtx_dimensions
# xtx_to_qimage
# xtx_to_qpixmap

XTX_MAGIC = b'xet\x00'
STORIES_MAGIC = 0x00746578


def _raster_layout(data: bytes, r: int, relocs: set, data_end: int) -> Dict: #vers 1
    """Identify raster platform via reloc table; return pixel layout."""
    found = []
    for kind, off in (('PSP', r + 4), ('PS2_VCS', r + 8), ('PS2_LCS', r)):
        if off not in relocs:
            continue
        lay = _raster_try(data, r, kind, data_end)
        if lay is not None:
            found.append(lay)
    if len(found) != 1:
        raise ValueError(f"Stories raster at 0x{r:X}: platform unclear "
                         f"({[f['raster_kind'] for f in found]})")
    lay = found[0]
    if lay['depth'] not in (4, 8):
        raise ValueError(f"Stories raster at 0x{r:X}: depth {lay['depth']} not supported")
    if lay['mips'] != 1:
        raise ValueError(f"Stories raster at 0x{r:X}: {lay['mips']} mip levels not supported")
    return lay


def _raster_try(data: bytes, r: int, kind: str, data_end: int) -> Optional[Dict]: #vers 1
    """Raster layout for one platform guess; None if fields inconsistent."""
    if kind == 'PSP':
        _u, ptr, stride, lw, lh, fl = struct.unpack_from('<IIHBBI', data, r)
        depth, mips = fl & 0xFF, (fl >> 8) & 0xF
        if _u != 0 or lw > 10 or lh > 10:
            return None
        w, h = 1 << lw, 1 << lh
        rows, swz, ps2 = (h + 7) & ~7, True, False
    else:
        if kind == 'PS2_VCS':
            if struct.unpack_from('<I', data, r)[0] != 0:
                return None
            ptr, fl = struct.unpack_from('<II', data, r + 8)
        else:
            ptr, fl = struct.unpack_from('<II', data, r)
        if (fl & 0x3F) > 10 or ((fl >> 6) & 0x3F) > 10:
            return None
        w, h = 1 << (fl & 0x3F), 1 << ((fl >> 6) & 0x3F)
        depth, mips = (fl >> 12) & 0x3F, (fl >> 20) & 0xF
        stride, rows, swz, ps2 = w * depth // 8, h, False, True
    if depth not in (4, 8) or stride < w * depth // 8 or stride == 0:
        return None
    if swz and stride % 16:
        return None
    pix = stride * rows
    pal = (1 << depth) * 4
    if ptr < 0x20 or ptr + pix + pal > data_end:
        return None
    return {'platform': 'PSP' if kind == 'PSP' else 'PS2', 'raster_kind': kind,
            'raster_offset': r, 'flags': fl, 'width': w, 'height': h,
            'depth': depth, 'mips': mips, 'stride': stride, 'rows': rows,
            'swizzled': swz, 'ps2_alpha': ps2, 'csm1': ps2 and depth == 8,
            'pix_off': ptr, 'pix_size': pix, 'pal_off': ptr + pix, 'pal_size': pal}


def is_xtx(path: str) -> bool: #vers 2
    """True if file starts with the Stories 'xet' ident."""
    with open(path, 'rb') as f:
        return f.read(4) == XTX_MAGIC


def parse_stories_textures(data: bytes) -> List[Dict]: #vers 1
    """Decode every texture in a Stories xet texture list."""
    if len(data) < 0x30 or data[:4] != XTX_MAGIC:
        raise ValueError("Not a Stories texture file ('xet' ident missing)")
    _id, _sh, file_end, data_end, reloc_tab, n_rel = struct.unpack_from('<6I', data, 0)
    if file_end != len(data) or reloc_tab + n_rel * 4 > len(data):
        raise ValueError(f"Stories header size mismatch (file_end {file_end}, len {len(data)})")
    relocs = set(struct.unpack_from(f'<{n_rel}I', data, reloc_tab))
    if data[0x20] != 6:
        raise ValueError(f"Stories file: object type {data[0x20]} is not a TexList")
    out, link, seen = [], struct.unpack_from('<I', data, 0x28)[0], set()
    while link != 0x28:
        if link in seen or link < 0x28 or link + 56 > data_end:
            raise ValueError(f"Stories texture list broken at link 0x{link:X}")
        seen.add(link)
        t = link - 8
        r, _dict, nxt = struct.unpack_from('<3I', data, t)
        name = data[t + 16:t + 48].split(b'\x00')[0].decode('latin-1')
        mask = data[t + 48:t + 80].split(b'\x00')[0].decode('latin-1')
        lay = _raster_layout(data, r, relocs, data_end)
        w, h, depth = lay['width'], lay['height'], lay['depth']
        raw = np.frombuffer(data, np.uint8, lay['pix_size'], lay['pix_off'])
        if lay['swizzled']:
            raw = ge_unswizzle(raw, lay['stride'], lay['rows'])
        else:
            raw = raw.reshape(lay['rows'], lay['stride'])
        idx = unpack_indices(raw[:h, :w * depth // 8], depth)
        pal = decode_palette(data[lay['pal_off']:lay['pal_off'] + lay['pal_size']],
                             lay['ps2_alpha'], lay['csm1'])
        rgba = decode_indexed(idx, pal)
        lay.update({'name': name, 'alpha_name': mask, 'texture_offset': t,
                    'format': 'PAL8' if depth == 8 else 'PAL4',
                    'has_alpha': bool((pal[np.unique(idx), 3] < 255).any()),
                    'rgba_data': rgba, 'palette': pal.tobytes(),
                    'indices': idx.tobytes()})
        out.append(lay)
        link = nxt
    if not out:
        raise ValueError("Stories texture list is empty")
    return out


def parse_xtx(data: bytes, name: str = '') -> Dict: #vers 1
    """Parse XTX bytes; first texture fields plus 'textures' list."""
    texs = parse_stories_textures(data)
    first = texs[0]
    return {
        'name': name or first['name'],
        'width': first['width'], 'height': first['height'],
        'depth': first['depth'], 'format': first['format'],
        'platform': first['platform'], 'has_alpha': first['has_alpha'],
        'pixel_format': first['depth'],
        'rgba_data': first['rgba_data'],
        'clut': first['palette'], 'palette': first['palette'],
        'pixels': first['indices'], 'indices': first['indices'],
        'textures': texs,
        'error': None,
    }


def read_xtx(path: str) -> Dict: #vers 2
    """Load XTX file; dict with 'error' set on failure."""
    try:
        with open(path, 'rb') as f:
            data = f.read()
        return parse_xtx(data, os.path.splitext(os.path.basename(path))[0])
    except (OSError, ValueError, struct.error) as e:
        return {'width': 0, 'height': 0, 'rgba_data': b'', 'textures': [],
                'error': str(e)}


def write_stories_textures(original: bytes, rgba) -> bytes: #vers 1
    """Rewrite pixels/palettes in place; rgba is bytes or per-texture list."""
    texs = parse_stories_textures(original)
    if isinstance(rgba, (bytes, bytearray)):
        if len(texs) != 1:
            raise ValueError(f"File holds {len(texs)} textures; pass a list of RGBA")
        rgba = [rgba]
    if len(rgba) != len(texs):
        raise ValueError(f"Got {len(rgba)} RGBA buffers for {len(texs)} textures")
    out = bytearray(original)
    for tex, new in zip(texs, rgba):
        if new is None:
            continue
        w, h, depth = tex['width'], tex['height'], tex['depth']
        if len(new) != w * h * 4:
            raise ValueError(f"'{tex['name']}': RGBA must be {w}x{h} (size is fixed)")
        old_idx = np.frombuffer(tex['indices'], np.uint8)
        old_pal = np.frombuffer(tex['palette'], np.uint8).reshape(-1, 4)
        idx, new_pal = encode_indexed(bytes(new), w, h, old_idx, old_pal, 1 << depth)
        row = pack_indices(idx, depth).reshape(h, w * depth // 8)
        lin = np.frombuffer(original, np.uint8, tex['pix_size'],
                            tex['pix_off']).copy()
        if tex['swizzled']:
            lin = ge_unswizzle(lin, tex['stride'], tex['rows']).copy()
        else:
            lin = lin.reshape(tex['rows'], tex['stride'])
        lin[:h, :w * depth // 8] = row
        stored = ge_swizzle(lin, tex['stride'], tex['rows']) if tex['swizzled'] else lin.ravel()
        out[tex['pix_off']:tex['pix_off'] + tex['pix_size']] = stored.tobytes()
        if new_pal is not None:
            out[tex['pal_off']:tex['pal_off'] + tex['pal_size']] = \
                encode_palette(new_pal, tex['ps2_alpha'], tex['csm1'])
    return bytes(out)


def write_xtx(original_bytes: bytes, rgba) -> bytes: #vers 1
    """Return XTX bytes with new RGBA; header and size unchanged."""
    return write_stories_textures(original_bytes, rgba)


def xtx_dimensions(data: bytes) -> Tuple[int, int]: #vers 2
    """(width, height) of first texture, read from header fields."""
    t = parse_stories_textures(data)[0]
    return t['width'], t['height']


def xtx_to_qimage(path: str): #vers 2
    """Decode first XTX texture to QImage; None on read error."""
    info = read_xtx(path)
    if info['error']:
        return None
    from PyQt6.QtGui import QImage
    return QImage(info['rgba_data'], info['width'], info['height'],
                  info['width'] * 4, QImage.Format.Format_RGBA8888).copy()


def xtx_to_qpixmap(path: str, max_size: int = 512): #vers 2
    """Decode XTX to QPixmap scaled within max_size."""
    qimg = xtx_to_qimage(path)
    if qimg is None:
        return None
    from PyQt6.QtGui import QPixmap
    from PyQt6.QtCore import Qt
    px = QPixmap.fromImage(qimg)
    if px.width() > max_size or px.height() > max_size:
        px = px.scaled(max_size, max_size, Qt.AspectRatioMode.KeepAspectRatio,
                       Qt.TransformationMode.SmoothTransformation)
    return px


__all__ = ['is_xtx', 'parse_stories_textures', 'parse_xtx', 'read_xtx',
           'write_stories_textures', 'write_xtx', 'xtx_dimensions',
           'xtx_to_qimage', 'xtx_to_qpixmap']
