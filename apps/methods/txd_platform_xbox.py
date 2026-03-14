#!/usr/bin/env python3
# apps/methods/txd_platform_xbox.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - Xbox TXD Platform Parser
#
# Handles Xbox TXD format (GTA III, Vice City, San Andreas Xbox).
# Platform ID: 5 (XBOX) inside the NativeTexture STRUCT.
# RW versions: 0x35000 (GTA III/VC Xbox), 0x1803FFFF (SA Xbox)
#
# Key differences from PC:
#   - platform_id = 5
#   - All mip levels stored in ONE contiguous block with a single u32 total_size prefix
#   - Compression identified by the compressionOrFlags byte:
#       0x00        = uncompressed (use raster pixel bits)
#       0x0B/0x0C   = DXT1 (linear / swizzled)
#       0x0E/0x0F   = DXT3 (swizzled / linear)
#       0x10/0x11   = DXT5 (swizzled / linear)
#   - Swizzled textures (0x0C, 0x0E, 0x10) need deswizzle before display
#     (deswizzle not yet implemented - stored as-is, flagged)

"""
Xbox TXD Platform Parser

NativeTexture STRUCT body layout (same 88-byte header as PC):
  +000  u32   platformId   = 5
  +004  u32   filterMode
  +008  char[32] name
  +040  char[32] maskName
  +072  u32   rasterFormat
  +076  u32   d3dFormat    (usually 0 for Xbox; compression in compressionFlags)
  +080  u16   width
  +082  u16   height
  +084  u8    depth
  +085  u8    numLevels
  +086  u8    rasterType
  +087  u8    compressionFlags   <-- key field
  +088  u32   totalDataSize      <-- single prefix for ALL mip levels combined
  +092  bytes pixel data (all mip levels concatenated)
"""

import struct
from typing import List, Dict, Optional, Tuple

## Methods list -
# detect_xbox_txd
# parse_xbox_nativetex
# _xbox_compression_fmt
# _split_xbox_mip_levels
# _calc_mip_size

RW_GTA3_VC_XBOX = 0x00035000
RW_SA_XBOX      = 0x1803FFFF

XBOX_VERSIONS = {RW_GTA3_VC_XBOX, RW_SA_XBOX}

# compressionFlags -> (format_str, has_alpha, is_swizzled)
_XBOX_COMP = {
    0x00: ('',      False, False),   # uncompressed - use raster bits
    0x0B: ('DXT1',  False, False),   # LIN_DXT1 (linear)
    0x0C: ('DXT1',  False, True),    # DXT1 (swizzled)
    0x0E: ('DXT3',  True,  True),    # DXT3 (swizzled)
    0x0F: ('DXT3',  True,  False),   # LIN_DXT3 (linear)
    0x10: ('DXT5',  True,  True),    # DXT5 (swizzled)
    0x11: ('DXT5',  True,  False),   # LIN_DXT5 (linear)
}

_RASTER_PIX = {
    0x0100: 'ARGB1555',
    0x0200: 'RGB565',
    0x0300: 'ARGB4444',
    0x0400: 'LUM8',
    0x0500: 'ARGB8888',
    0x0600: 'RGB888',
    0x0A00: 'RGB555',
}


def detect_xbox_txd(platform_id: int) -> bool:  # vers 1
    """Return True if the NativeTexture platform_id indicates Xbox."""
    return platform_id == 5


def _calc_mip_size(fmt: str, w: int, h: int, depth: int) -> int:  # vers 1
    """Calculate byte size of one mip level."""
    if fmt == 'DXT1':
        return max(1, (w+3)//4) * max(1, (h+3)//4) * 8
    if fmt in ('DXT3', 'DXT5'):
        return max(1, (w+3)//4) * max(1, (h+3)//4) * 16
    if fmt == 'ARGB8888':
        return w * h * 4
    if fmt == 'RGB888':
        return w * h * (4 if depth == 32 else 3)
    if fmt in ('RGB565', 'ARGB1555', 'ARGB4444', 'RGB555'):
        return w * h * 2
    if fmt == 'LUM8':
        return w * h
    return w * h * (depth // 8 if depth >= 8 else 1)


def _xbox_compression_fmt(comp_flags: int,
                          raster_flags: int) -> Tuple[str, bool, bool]:  # vers 1
    """
    Determine format, has_alpha, is_swizzled from the Xbox compressionFlags byte.

    Returns:
        (format_str, has_alpha, is_swizzled)
    """
    if comp_flags in _XBOX_COMP:
        fmt, has_alpha, swizzled = _XBOX_COMP[comp_flags]
        if not fmt:
            # Uncompressed — fall back to raster pixel bits
            pix = raster_flags & 0x0F00
            fmt = _RASTER_PIX.get(pix, 'ARGB8888')
            has_alpha = fmt in ('ARGB8888', 'ARGB1555', 'ARGB4444')
        return fmt, has_alpha, swizzled
    # Unknown flags — guess uncompressed ARGB8888
    return 'ARGB8888', True, False


def _split_xbox_mip_levels(data_block: bytes, fmt: str, width: int,
                            height: int, num_levels: int,
                            depth: int) -> List[Dict]:  # vers 1
    """
    Split Xbox's single contiguous mip block into per-level dicts.

    Xbox stores all mip levels concatenated, largest first, with NO
    per-level size prefix (just one u32 total at the top).

    Args:
        data_block: Raw bytes for all mip levels combined
        fmt:        Format string (DXT1, DXT3, DXT5, ARGB8888, etc.)
        width:      Mip-0 width
        height:     Mip-0 height
        num_levels: Number of mip levels declared in the header
        depth:      Bit depth

    Returns:
        List of mip dicts: {level, width, height, data}
    """
    mips = []
    pos  = 0
    w, h = width, height

    for level in range(num_levels):
        sz = _calc_mip_size(fmt, w, h, depth)
        if pos + sz > len(data_block):
            break
        mips.append({
            'level':  level,
            'width':  w,
            'height': h,
            'data':   data_block[pos:pos + sz],
        })
        pos += sz
        if w == 1 and h == 1:
            break
        w = max(1, w // 2)
        h = max(1, h // 2)

    return mips


def parse_xbox_nativetex(txd_data: bytes, chunk_offset: int,
                          index: int) -> Optional[Dict]:  # vers 1
    """
    Parse a single Xbox NativeTexture chunk.

    Args:
        txd_data:     Full TXD file bytes
        chunk_offset: Offset of the NativeTexture (0x15) chunk header
        index:        Texture index for fallback naming

    Returns:
        Texture dict compatible with IMG Factory's internal format, or None.
    """
    tex = {
        'name': f'texture_{index}',
        'alpha_name': '',
        'width': 0, 'height': 0, 'depth': 32,
        'format': 'DXT1',
        'has_alpha': False, 'force_opaque': False,
        'is_swizzled': False,
        'mipmaps': 1,
        'rgba_data': b'',
        'compressed_data': b'',
        'mipmap_levels': [],
        'raster_format_flags': 0,
        'platform_id': 5,
        'filter_mode': 0,
        'u_addr': 0, 'v_addr': 0,
        'bumpmap_data': b'', 'has_bumpmap': False,
        'reflection_map': b'', 'has_reflection': False,
    }

    try:
        nt_type = struct.unpack_from('<I', txd_data, chunk_offset)[0]
        if nt_type != 0x15:
            return None

        sp = chunk_offset + 12
        st_type = struct.unpack_from('<I', txd_data, sp)[0]
        if st_type != 0x01:
            return None

        pos = sp + 12  # struct body start

        # --- 88-byte header ---
        platform_id = struct.unpack_from('<I', txd_data, pos)[0]
        tex['platform_id'] = platform_id

        filter_mode = txd_data[pos + 4]
        u_addr      = txd_data[pos + 5]
        v_addr      = txd_data[pos + 6]
        tex['filter_mode'] = filter_mode
        tex['u_addr']      = u_addr
        tex['v_addr']      = v_addr
        pos += 8

        name = txd_data[pos:pos+32].rstrip(b'\x00').decode('ascii', errors='ignore')
        tex['name'] = name or f'texture_{index}'
        pos += 32

        mask = txd_data[pos:pos+32].rstrip(b'\x00').decode('ascii', errors='ignore')
        if mask:
            tex['alpha_name'] = mask
        pos += 32

        raster_flags, d3d_fmt, width, height, depth, num_levels, raster_type = \
            struct.unpack_from('<IIHHBBB', txd_data, pos)
        tex['width']  = width
        tex['height'] = height
        tex['depth']  = depth
        tex['mipmaps'] = num_levels
        tex['raster_format_flags'] = raster_flags
        pos += 15

        comp_flags = txd_data[pos]
        pos += 1

        # --- Format detection ---
        fmt, has_alpha, is_swizzled = _xbox_compression_fmt(comp_flags, raster_flags)
        tex['format']      = fmt
        tex['has_alpha']   = has_alpha or bool(mask)
        tex['is_swizzled'] = is_swizzled

        # --- Single total-size prefix ---
        if pos + 4 > len(txd_data):
            return tex

        total_size = struct.unpack_from('<I', txd_data, pos)[0]
        pos += 4

        data_block = txd_data[pos:pos + total_size]

        # --- Split into per-level mips ---
        mips = _split_xbox_mip_levels(data_block, fmt, width, height, num_levels, depth)
        tex['mipmap_levels']   = mips
        tex['compressed_data'] = b''.join(m['data'] for m in mips)

        return tex

    except Exception:
        return None
