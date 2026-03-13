#!/usr/bin/env python3
# apps/methods/txd_platform_psp.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - PSP TXD Platform Parser
#
# Handles PSP TXD format (LCS PSP, VCS PSP, Chinatown Wars PSP).
# Platform ID: 9 (DEVICE_PSP) inside NativeTexture STRUCT.
# RW versions:
#   LCS PSP : 0x00035000  (same as GTA III/VC Xbox)
#   VCS PSP : 0x00035002
#   CTW PSP : unknown — not yet confirmed
#
# STATUS: Header layout sourced from RenderWare spec and community docs.
#         NOT yet confirmed from binary analysis of real PSP TXD files
#         in this project. Pixel formats are swizzled (Morton order).
#         Swizzle decode NOT yet implemented.

"""
PSP TXD Platform Parser

PSP shares the standard 88-byte RW NativeTexture header with PC/Xbox but
uses swizzled (Morton-order) pixel storage and a different compression scheme.

NativeTexture STRUCT body layout (88 bytes — same as PC):
  +000  u32   platformId   = 9
  +004  u32   filterMode
  +008  char[32] name
  +040  char[32] maskName
  +072  u32   rasterFormat     (pixel format flags)
  +076  u32   d3dFormat        (PSP GE format code, not D3D FourCC)
  +080  u16   width
  +082  u16   height
  +084  u8    depth
  +085  u8    numLevels
  +086  u8    rasterType
  +087  u8    swizzledFlag     (1 = swizzled, 0 = linear)
  +088  bytes pixel data (each level: u32 size prefix then data)

PSP GE pixel formats (d3dFormat field — not standard D3D):
  0x00 = GE_CMODE_16BIT_BGR5650   (RGB565)
  0x01 = GE_CMODE_16BIT_ABGR5551  (ARGB1555)
  0x02 = GE_CMODE_16BIT_ABGR4444  (ARGB4444)
  0x03 = GE_CMODE_32BIT_ABGR8888  (ARGB8888)
  0x04 = GE_CMODE_CLUT4           (PAL4 — 4bpp paletted)
  0x05 = GE_CMODE_CLUT8           (PAL8 — 8bpp paletted)
  0x06 = GE_TFMT_DXT1
  0x07 = GE_TFMT_DXT3
  0x08 = GE_TFMT_DXT5

Swizzle: PSP stores pixels in 16-byte column tiles (Morton-like).
Unswizzle NOT yet implemented — raw bytes stored as-is, flagged.
"""

import struct
from typing import List, Dict, Optional

## Methods list -
# detect_psp_txd
# parse_psp_nativetex
# _psp_ge_format
# _psp_mip_size
# _psp_read_mips

# NOTE: No confirmed binary test files analysed yet for PSP in this project.
# Format spec from: gtamodding.com wiki, PSP GE documentation, writerside
# notes from Silent and aap, and rwgta community research.

RW_LCS_PSP = 0x00035000
RW_VCS_PSP = 0x00035002

PSP_VERSIONS = {RW_LCS_PSP, RW_VCS_PSP}

# PSP GE format code -> (internal format name, has_alpha)
_PSP_GE_FMT = {
    0x00: ('RGB565',   False),
    0x01: ('ARGB1555', True),
    0x02: ('ARGB4444', True),
    0x03: ('ARGB8888', True),
    0x04: ('PAL4',     False),
    0x05: ('PAL8',     True),
    0x06: ('DXT1',     False),
    0x07: ('DXT3',     True),
    0x08: ('DXT5',     True),
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


def detect_psp_txd(platform_id: int) -> bool:  # vers 1
    """Return True if the NativeTexture platform_id indicates PSP."""
    return platform_id == 9


def _psp_ge_format(d3d_fmt: int, raster_flags: int) -> tuple:  # vers 1
    """
    Decode PSP GE format code to (format_str, has_alpha).

    Falls back to raster pixel bits if format code unrecognised.
    """
    if d3d_fmt in _PSP_GE_FMT:
        return _PSP_GE_FMT[d3d_fmt]
    # Fallback to raster format pixel bits
    pix = raster_flags & 0x0F00
    fmt = _RASTER_PIX.get(pix, f'UNKNOWN_GE_0x{d3d_fmt:02X}')
    has_alpha = fmt in ('ARGB8888', 'ARGB1555', 'ARGB4444')
    return fmt, has_alpha


def _psp_mip_size(fmt: str, w: int, h: int) -> int:  # vers 1
    """Calculate logical (unswizzled) byte size of one PSP mip level."""
    if fmt == 'DXT1':
        return max(1, (w+3)//4) * max(1, (h+3)//4) * 8
    if fmt in ('DXT3', 'DXT5'):
        return max(1, (w+3)//4) * max(1, (h+3)//4) * 16
    if fmt == 'ARGB8888':
        return w * h * 4
    if fmt in ('RGB565', 'ARGB1555', 'ARGB4444', 'RGB555'):
        return w * h * 2
    if fmt == 'PAL8':
        return 1024 + w * h        # 256-entry RGBA8 palette + 8bpp indices
    if fmt == 'PAL4':
        return 64 + (w * h + 1) // 2  # 16-entry RGBA8 palette + 4bpp indices
    if fmt == 'LUM8':
        return w * h
    return w * h * 2  # safe fallback


def _psp_read_mips(data: bytes, pos: int, fmt: str,
                   width: int, height: int, num_levels: int) -> List[Dict]:  # vers 1
    """
    Read PSP mip levels, each with a u32 size prefix.

    Returns raw (still swizzled) bytes per level.

    Args:
        data:       Full TXD file bytes
        pos:        Byte offset to start of first mip level
        fmt:        Format string
        width:      Mip-0 width
        height:     Mip-0 height
        num_levels: Number of mip levels

    Returns:
        List of mip dicts: {level, width, height, data, swizzled}
    """
    mips = []
    w, h = width, height

    for level in range(num_levels):
        if pos + 4 > len(data):
            break
        declared = struct.unpack_from('<I', data, pos)[0]
        pos += 4

        expected = _psp_mip_size(fmt, w, h)
        read_sz  = min(declared, expected, len(data) - pos)
        if read_sz <= 0:
            break

        mips.append({
            'level':    level,
            'width':    w,
            'height':   h,
            'data':     data[pos:pos + read_sz],
            'swizzled': True,   # always — deswizzle not yet implemented
        })
        pos += read_sz

        if w == 1 and h == 1:
            break
        w = max(1, w // 2)
        h = max(1, h // 2)

    return mips


def parse_psp_nativetex(txd_data: bytes, chunk_offset: int,
                        index: int) -> Optional[Dict]:  # vers 1
    """
    Parse a single PSP NativeTexture chunk.

    Returns raw (swizzled) pixel bytes in mipmap_levels — not decoded to RGBA.
    rgba_data will be empty until a PSP deswizzle + decode is implemented.

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
        'rgba_data': b'',           # empty until PSP deswizzle implemented
        'compressed_data': b'',
        'mipmap_levels': [],
        'raster_format_flags': 0,
        'platform_id': 9,
        'filter_mode': 0,
        'u_addr': 0, 'v_addr': 0,
        'psp_swizzled': True,       # flag: pixel data uses PSP tile swizzle
        'psp_ge_format': 0,
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

        # --- 88-byte header (same layout as PC) ---
        platform_id = struct.unpack_from('<I', txd_data, pos)[0]
        tex['platform_id'] = platform_id

        filter_mode = txd_data[pos + 4]
        u_addr      = txd_data[pos + 5]
        v_addr      = txd_data[pos + 6]
        tex['filter_mode'] = filter_mode
        tex['u_addr']      = u_addr
        tex['v_addr']      = v_addr
        pos += 8

        name = txd_data[pos:pos+32].rstrip(b'\x00').decode('latin-1', errors='replace')
        tex['name'] = name or f'texture_{index}'
        pos += 32

        mask = txd_data[pos:pos+32].rstrip(b'\x00').decode('latin-1', errors='replace')
        if mask:
            tex['alpha_name'] = mask
        pos += 32

        raster_flags, d3d_fmt, width, height, depth, num_levels, raster_type = \
            struct.unpack_from('<IIHHBBB', txd_data, pos)
        tex['width']               = width
        tex['height']              = height
        tex['depth']               = depth
        tex['mipmaps']             = num_levels
        tex['raster_format_flags'] = raster_flags
        tex['psp_ge_format']       = d3d_fmt
        pos += 15

        swizzled_flag = txd_data[pos]
        tex['psp_swizzled'] = bool(swizzled_flag)
        pos += 1

        fmt, has_alpha = _psp_ge_format(d3d_fmt, raster_flags)
        tex['format']    = fmt
        tex['has_alpha'] = has_alpha or bool(mask)

        # Read raw mip levels (with u32 size prefix each)
        mips = _psp_read_mips(txd_data, pos, fmt, width, height, num_levels)
        tex['mipmap_levels']   = mips
        tex['compressed_data'] = b''.join(m['data'] for m in mips)
        # rgba_data intentionally empty — requires PSP deswizzle implementation

        return tex

    except Exception:
        return None
