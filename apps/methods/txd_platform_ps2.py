#!/usr/bin/env python3
# apps/methods/txd_platform_ps2.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - PS2 TXD Platform Parser
#
# Handles PlayStation 2 TXD format (GTA III, Vice City, San Andreas PS2).
# Platform ID: 6 inside NativeTexture STRUCT, or FourCC 0x00325350 ("PS2\0")
# RW versions:
#   GTA III PS2 : 0x00000310
#   Vice City   : 0x0C02FFFF  (shared with GTA III PC)
#   San Andreas : 0x1803FFFF  (shared with all SA platforms)
#
# STATUS: Struct layout confirmed from RenderWare spec and community research.
#         Pixel decode (palette unswizzle, GS_PSM decode) NOT yet confirmed
#         from binary analysis of real PS2 TXD files in this project.
#         Current implementation reads raw mip bytes only — STUB: full decode pending

"""
PS2 TXD Platform Parser

PS2 NativeTexture STRUCT body layout (differs significantly from PC/Xbox):
  +000  u32   platformId         = 6, or FourCC 0x00325350 ("PS2\\0")
  +004  u32   filterMode
  +008  u32   uv_addressing
  +012  [4 bytes padding]
  +016  char[32] name
  +048  char[32] maskName
  +080  u32   rasterFormat       (pixel format flags)
  +084  u8    depth
  +085  u8    width_log2         (log2 of width — NOT raw pixel width for old format)
  +086  u8    height_log2
  +087  u8    numLevels
  +088  u8    rasterType
  +089  u8    paletteFormat
  +090  u8    hasAlpha
  +091  u8    isCubeMap
  +092  u32   gpuDataSize        (total pixel data size on GS memory)
  +096  u32   skyMipMapValue     (SA only — sky mipmap k/l values)
  +100  bytes pixel data (GS swizzled, palette first if paletted)

Pixel formats (rasterFormat bits 8-11):
  0x0000 = no format (paletted)
  0x0100 = ARGB1555
  0x0200 = RGB565
  0x0300 = ARGB4444
  0x0400 = LUM8
  0x0500 = ARGB8888
  0x0600 = RGB888

Palette (paletteFormat):
  0x00 = no palette (direct colour)
  0x01 = PAL8  (256-entry palette, each entry 4-byte RGBA)
  0x02 = PAL4  (16-entry palette)

GS swizzle: PS2 stores pixels in GPU tile order (swizzled).
Unswizzle NOT yet implemented — raw bytes are stored as-is.
"""

import struct
from typing import List, Dict, Optional

## Methods list -
# detect_ps2_txd
# parse_ps2_nativetex
# _ps2_raster_format
# _ps2_mip_size
# _ps2_read_mips

# NOTE: No confirmed binary test files analysed yet for PS2 in this project.
# Format spec sourced from RenderWare GTA community documentation (rwgta.com,
# gtamodding.com wiki, Silent's RW format notes).

RW_GTA3_PS2  = 0x00000310
RW_GTAVC_PS2 = 0x0C02FFFF   # shared with GTA III PC
RW_GTASA_PS2 = 0x1803FFFF   # shared with all SA platforms

PS2_PLATFORM_IDS = {6, 0x00325350}   # 6 or "PS2\0" FourCC

_RASTER_PIX = {
    0x0000: 'PAL',       # paletted — actual format from paletteFormat
    0x0100: 'ARGB1555',
    0x0200: 'RGB565',
    0x0300: 'ARGB4444',
    0x0400: 'LUM8',
    0x0500: 'ARGB8888',
    0x0600: 'RGB888',
}

_PAL_FMT = {
    0: None,      # no palette
    1: 'PAL8',    # 256 entries * 4 bytes = 1024 bytes
    2: 'PAL4',    # 16 entries * 4 bytes = 64 bytes
}


def detect_ps2_txd(platform_id: int) -> bool:  # vers 1
    """Return True if the NativeTexture platform_id indicates PS2."""
    return platform_id in PS2_PLATFORM_IDS


def _ps2_raster_format(raster_flags: int, pal_fmt_byte: int) -> tuple:  # vers 1
    """
    Decode PS2 raster format flags into (format_str, has_alpha).

    Args:
        raster_flags:  u32 rasterFormat field
        pal_fmt_byte:  u8 paletteFormat field

    Returns:
        (format_str, has_alpha)
    """
    pix_bits = raster_flags & 0x0F00
    is_pal8  = bool(raster_flags & 0x2000) or (pal_fmt_byte == 1)
    is_pal4  = bool(raster_flags & 0x4000) or (pal_fmt_byte == 2)

    if is_pal8:
        return 'PAL8', True
    if is_pal4:
        return 'PAL4', False

    fmt = _RASTER_PIX.get(pix_bits, f'UNKNOWN_0x{raster_flags:08X}')
    has_alpha = fmt in ('ARGB8888', 'ARGB1555', 'ARGB4444')
    return fmt, has_alpha


def _ps2_mip_size(fmt: str, w: int, h: int) -> int:  # vers 1
    """
    Calculate raw byte size of one PS2 mip level (pre-swizzle layout).

    NOTE: Real GS memory layout differs due to swizzling. This gives the
    logical (unswizzled) size used for reading raw bytes from the file.
    """
    if fmt == 'PAL8':
        return 1024 + w * h         # 256-entry RGBA palette + 1bpp indices
    if fmt == 'PAL4':
        return 64 + (w * h + 1) // 2  # 16-entry RGBA palette + 4bpp indices
    if fmt == 'ARGB8888':
        return w * h * 4
    if fmt in ('RGB565', 'ARGB1555', 'ARGB4444'):
        return w * h * 2
    if fmt in ('LUM8', 'RGB888'):
        return w * h
    return w * h * 2  # safe fallback


def _ps2_read_mips(data: bytes, pos: int, fmt: str,
                   width: int, height: int, num_levels: int) -> List[Dict]:  # vers 1
    """
    Read all PS2 mip levels as raw bytes.

    PS2 mip data has NO per-level size prefix — raw bytes only.
    Swizzle is not undone here; caller receives raw GS data.

    Args:
        data:       Full TXD file bytes
        pos:        Byte offset to start of pixel data
        fmt:        Format string (PAL8, ARGB8888, etc.)
        width:      Mip-0 width in pixels
        height:     Mip-0 height in pixels
        num_levels: Number of mip levels

    Returns:
        List of mip dicts: {level, width, height, data, swizzled}
    """
    mips = []
    w, h = width, height

    for level in range(num_levels):
        sz = _ps2_mip_size(fmt, w, h)
        if pos + sz > len(data):
            break
        mips.append({
            'level':    level,
            'width':    w,
            'height':   h,
            'data':     data[pos:pos + sz],
            'swizzled': True,   # always — deswizzle not yet implemented
        })
        pos += sz
        if w == 1 and h == 1:
            break
        w = max(1, w // 2)
        h = max(1, h // 2)

    return mips


def parse_ps2_nativetex(txd_data: bytes, chunk_offset: int,
                        index: int) -> Optional[Dict]:  # vers 1
    """
    Parse a single PS2 NativeTexture chunk.

    Returns raw pixel bytes in mipmap_levels — not decoded to RGBA.
    rgba_data will be empty until a PS2 pixel decoder is implemented.

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
        'format': 'PAL8',
        'has_alpha': False,
        'mipmaps': 1,
        'rgba_data': b'',          # empty until PS2 decode implemented
        'compressed_data': b'',
        'mipmap_levels': [],
        'raster_format_flags': 0,
        'platform_id': 6,
        'filter_mode': 0,
        'u_addr': 0, 'v_addr': 0,
        'ps2_swizzled': True,      # flag: pixel data is GS-swizzled
        'ps2_palette_fmt': 0,
    }

    try:
        nt_type = struct.unpack_from('<I', txd_data, chunk_offset)[0]
        if nt_type != 0x15:
            return None

        sp = chunk_offset + 12
        st_type = struct.unpack_from('<I', txd_data, sp)[0]
        if st_type != 0x01:
            return None

        pos = sp + 12   # struct body start

        # --- PS2 header ---
        platform_id  = struct.unpack_from('<I', txd_data, pos)[0]
        filter_mode  = struct.unpack_from('<I', txd_data, pos + 4)[0]
        uv_addr      = struct.unpack_from('<I', txd_data, pos + 8)[0]
        # +12 = 4 bytes padding
        tex['platform_id'] = platform_id
        tex['filter_mode'] = filter_mode
        tex['u_addr']      = uv_addr & 0xFF
        tex['v_addr']      = (uv_addr >> 8) & 0xFF
        pos += 16

        name = txd_data[pos:pos+32].rstrip(b'\x00').decode('latin-1', errors='replace')
        tex['name'] = name or f'texture_{index}'
        pos += 32

        mask = txd_data[pos:pos+32].rstrip(b'\x00').decode('latin-1', errors='replace')
        if mask:
            tex['alpha_name'] = mask
        pos += 32

        raster_flags = struct.unpack_from('<I', txd_data, pos)[0]
        tex['raster_format_flags'] = raster_flags
        pos += 4

        depth       = txd_data[pos]
        width_log2  = txd_data[pos + 1]
        height_log2 = txd_data[pos + 2]
        num_levels  = txd_data[pos + 3]
        raster_type = txd_data[pos + 4]
        pal_fmt     = txd_data[pos + 5]
        has_alpha_b = txd_data[pos + 6]
        pos += 8    # +7 is is_cube_map, consumed here

        # Width/height encoded as log2 in some versions
        # Sanity: if log2 values look reasonable, use them; else treat as raw
        if 1 <= width_log2 <= 11:
            width  = 1 << width_log2
        else:
            width  = width_log2  # older format stores raw width
        if 1 <= height_log2 <= 11:
            height = 1 << height_log2
        else:
            height = height_log2

        tex['width']           = width
        tex['height']          = height
        tex['depth']           = depth
        tex['mipmaps']         = num_levels
        tex['ps2_palette_fmt'] = pal_fmt

        gpu_data_size = struct.unpack_from('<I', txd_data, pos)[0]
        pos += 4
        # SA adds skyMipMapValue u32 here — skip if present
        # (detected by rw_version >= 0x1803FFFF in caller)
        # For now, advance past it always to avoid corruption
        # STUB: make swizzle conditional on rw_version parameter
        pos += 4

        fmt, has_alpha = _ps2_raster_format(raster_flags, pal_fmt)
        tex['format']    = fmt
        tex['has_alpha'] = has_alpha or bool(mask) or bool(has_alpha_b)

        # Read raw mip bytes — no decode yet
        mips = _ps2_read_mips(txd_data, pos, fmt, width, height, num_levels)
        tex['mipmap_levels']   = mips
        tex['compressed_data'] = b''.join(m['data'] for m in mips)
        # rgba_data intentionally left empty — requires GS unswizzle + CLUT decode

        return tex

    except Exception:
        return None
