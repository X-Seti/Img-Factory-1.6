#!/usr/bin/env python3
# apps/methods/txd_platform_pc.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - PC TXD Platform Parser
#
# Handles Direct3D 8 (GTA III, Vice City) and Direct3D 9 (San Andreas) TXD formats.
# Platform IDs: D3D8 = 0x08 (or 0x01 in older files), D3D9 = 0x09
# RW versions:
#   GTA III PC : 0x0C02FFFF
#   Vice City  : 0x1003FFFF
#   San Andreas: 0x1803FFFF

"""
PC TXD Platform Parser (D3D8 / D3D9)

NativeTexture STRUCT body layout (88 bytes, same for D3D8 and D3D9):
  +000  u32   platformId      (1=D3D8 old, 8=D3D8, 9=D3D9)
  +004  u32   filterMode      (filter flags + addressing)
  +008  char[32] name
  +040  char[32] maskName
  +072  u32   rasterFormat    (pixel format flags + mipmap flag)
  +076  u32   d3dFormat       (FourCC for DXT, or D3DFMT_* enum for uncompressed)
  +080  u16   width
  +082  u16   height
  +084  u8    depth
  +085  u8    numLevels
  +086  u8    rasterType      (always 4 = RASTER_TEXTURE)
  +087  u8    compressionOrFlags
  +088  bytes pixel data (with per-level u32 size prefix for DXT and all SA formats)

D3D8 mip data:
  - DXT formats only: u32 mipSize per level then data
  - Uncompressed: raw data directly (no size prefix)

D3D9 (SA) mip data:
  - ALL formats: u32 mipSize per level then data
"""

import struct
from typing import List, Dict, Optional, Tuple

## Methods list -
# detect_pc_txd
# parse_pc_nativetex
# _decode_format
# _read_mip_levels
# _calc_mip_size

RW_GTA3_PC  = 0x0C02FFFF
RW_GTAVC_PC = 0x1003FFFF
RW_GTASA    = 0x1803FFFF

PC_VERSIONS = {RW_GTA3_PC, RW_GTAVC_PC, RW_GTASA}

# D3D9 format enum -> internal name
_D3D9_FMT = {
    20: 'RGB888',
    21: 'ARGB8888',
    22: 'ARGB8888',   # X8R8G8B8 - treat as ARGB8888, force_opaque=True
    23: 'RGB565',
    24: 'RGB555',
    25: 'ARGB1555',
    26: 'ARGB4444',
    28: 'A8',
    32: 'ARGB8888',   # A8B8G8R8
    50: 'LUM8',
    51: 'A8L8',
    52: 'A4L4',
    41: 'PAL8',
    0x31545844: 'DXT1',
    0x33545844: 'DXT3',
    0x35545844: 'DXT5',
}

# rasterFormat pixel bits (bits 8-11) -> internal name
_RASTER_PIX = {
    0x0100: 'ARGB1555',
    0x0200: 'RGB565',
    0x0300: 'ARGB4444',
    0x0400: 'LUM8',
    0x0500: 'ARGB8888',
    0x0600: 'RGB888',
    0x0A00: 'RGB555',
}


def detect_pc_txd(data: bytes, rw_version: int) -> bool:  # vers 1
    """Return True if rw_version is a known PC version."""
    return rw_version in PC_VERSIONS


def _calc_mip_size(fmt: str, w: int, h: int, depth: int) -> int:  # vers 1
    """Calculate byte size of one mip level for a given format."""
    if fmt == 'DXT1':
        return max(1, (w+3)//4) * max(1, (h+3)//4) * 8
    if fmt in ('DXT3', 'DXT5'):
        return max(1, (w+3)//4) * max(1, (h+3)//4) * 16
    if fmt == 'ARGB8888':
        return w * h * 4
    if fmt == 'RGB888':
        return w * h * (4 if depth == 32 else 3)
    if fmt in ('RGB565', 'ARGB1555', 'ARGB4444', 'RGB555', 'A8L8', 'A4L4'):
        return w * h * 2
    if fmt in ('LUM8', 'A8', 'PAL8'):
        return w * h
    if fmt == 'PAL4':
        return (w * h + 1) // 2
    # fallback
    return w * h * (depth // 8 if depth >= 8 else 1)


def _decode_format(raster_flags: int, d3d_fmt: int,
                   platform_prop: int, rw_version: int) -> Tuple[str, bool, bool]:  # vers 1
    """
    Determine the texture format string, has_alpha, and force_opaque flags.

    Returns:
        (format_str, has_alpha, force_opaque)
    """
    is_sa  = (rw_version >= RW_GTASA)
    is_pal8 = bool(raster_flags & 0x2000)
    is_pal4 = bool(raster_flags & 0x4000)
    pix_bits = raster_flags & 0x0F00

    if is_pal8:
        return 'PAL8', True, False
    if is_pal4:
        return 'PAL4', False, False

    # SA (D3D9): d3d_format FourCC / enum is authoritative
    if is_sa:
        fmt = _D3D9_FMT.get(d3d_fmt)
        if fmt:
            force_opaque = (d3d_fmt == 22)  # X8R8G8B8
            has_alpha = fmt in ('ARGB8888', 'ARGB1555', 'ARGB4444',
                                'DXT3', 'DXT5', 'A8L8', 'A4L4', 'A8', 'PAL8')
            return fmt, has_alpha, force_opaque

    # D3D8 DXT FourCC
    if d3d_fmt == 0x31545844:
        return 'DXT1', False, False
    if d3d_fmt == 0x33545844:
        return 'DXT3', True, False
    if d3d_fmt == 0x35545844:
        return 'DXT5', True, False

    # D3D8 platform_prop compression byte (GTA3/VC)
    if platform_prop == 1:
        return 'DXT1', False, False
    if platform_prop == 3:
        return 'DXT3', True, False
    if platform_prop == 5:
        return 'DXT5', True, False

    # Fallback: raster pixel bits
    fmt = _RASTER_PIX.get(pix_bits, f'UNKNOWN_0x{raster_flags:08X}')
    has_alpha = fmt in ('ARGB8888', 'ARGB1555', 'ARGB4444', 'A8L8')
    return fmt, has_alpha, False


def _read_mip_levels(data: bytes, pos: int, fmt: str, width: int, height: int,
                     num_levels: int, depth: int, is_sa: bool) -> Tuple[List[Dict], int]:  # vers 1
    """
    Read all mip levels from the data stream.

    D3D8: DXT formats have u32 size prefix per level; uncompressed do not.
    D3D9 (SA): ALL formats have u32 size prefix per level.

    Returns:
        (mip_list, final_pos)
        Each mip dict: {level, width, height, data}
    """
    is_dxt = 'DXT' in fmt
    has_prefix = is_dxt or is_sa

    mips = []
    w, h = width, height

    for level in range(num_levels):
        expected = _calc_mip_size(fmt, w, h, depth)

        if has_prefix:
            if pos + 4 > len(data):
                break
            declared = struct.unpack_from('<I', data, pos)[0]
            pos += 4
            read_sz = min(declared, expected, len(data) - pos)
        else:
            read_sz = min(expected, len(data) - pos)

        if read_sz <= 0:
            break

        mip_bytes = data[pos:pos + read_sz]
        pos += read_sz

        mips.append({
            'level':  level,
            'width':  w,
            'height': h,
            'data':   mip_bytes,
        })

        if w == 1 and h == 1:
            break
        w = max(1, w // 2)
        h = max(1, h // 2)

    return mips, pos


def parse_pc_nativetex(txd_data: bytes, chunk_offset: int,
                       index: int, rw_version: int) -> Optional[Dict]:  # vers 1
    """
    Parse a single PC NativeTexture chunk (D3D8 or D3D9).

    Args:
        txd_data:     Full TXD file bytes
        chunk_offset: Offset of the NativeTexture (0x15) chunk header
        index:        Texture index (for fallback naming)
        rw_version:   RW version from the outer TexDict header

    Returns:
        Texture dict compatible with IMG Factory's internal format, or None.
    """
    tex = {
        'name': f'texture_{index}',
        'alpha_name': '',
        'width': 0, 'height': 0, 'depth': 32,
        'format': 'DXT1',
        'has_alpha': False, 'force_opaque': False,
        'mipmaps': 1,
        'rgba_data': b'',
        'compressed_data': b'',
        'mipmap_levels': [],
        'raster_format_flags': 0,
        'platform_id': 0,
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
        st_type, st_size = struct.unpack_from('<II', txd_data, sp)[:2]
        if st_type != 0x01:
            return None

        pos = sp + 12  # struct body start

        # --- 88-byte header ---
        platform_id = struct.unpack_from('<I', txd_data, pos)[0]
        tex['platform_id'] = platform_id

        #    Xbox: delegate to dedicated parser                             
        if platform_id == 5:
            try:
                from apps.methods.txd_platform_xbox import parse_xbox_nativetex
                xbox_tex = parse_xbox_nativetex(txd_data, chunk_offset, index)
                if xbox_tex:
                    return xbox_tex
            except Exception as _xe:
                print(f"[Xbox TXD] Parse error texture {index}: {_xe}")
            return None  # failed — don't fall through to PC parser
        #    End Xbox                                                       

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

        # Version-aware header read:
        # No middle field (width at +76): GTA3 PC (0x0C02FFFF) and VC PC (0x1003FFFF) only
        # Middle field present (width at +80): all other versions including 0x0800FFFF, 0x34005, SA
        is_d3d8_no_middle = rw_version in (RW_GTA3_PC, RW_GTAVC_PC)
        is_sa = (rw_version >= RW_GTASA)

        if is_d3d8_no_middle:
            raster_flags, width, height = struct.unpack_from('<IHH', txd_data, pos)
            depth, num_levels, raster_type, platform_prop = \
                struct.unpack_from('<BBBB', txd_data, pos + 8)
            d3d_fmt = 0
            pos += 12
        else:
            raster_flags, d3d_fmt, width, height, depth, num_levels, raster_type = \
                struct.unpack_from('<IIHHBBB', txd_data, pos)
            pos += 15
            platform_prop = txd_data[pos]
            pos += 1
            # For non-SA old versions the middle field is hasAlpha not a D3D FourCC
            if not is_sa and d3d_fmt in (0, 1):
                tex['has_alpha'] = bool(d3d_fmt) or tex.get('has_alpha', False)
                d3d_fmt = 0

        tex['width']               = width
        tex['height']              = height
        tex['depth']               = depth
        tex['mipmaps']             = num_levels
        tex['raster_format_flags'] = raster_flags

        if raster_flags & 0x10:
            tex['has_bumpmap'] = True

        # --- Format detection ---
        fmt, has_alpha, force_opaque = _decode_format(
            raster_flags, d3d_fmt, platform_prop, rw_version)
        tex['format']       = fmt
        tex['has_alpha']    = has_alpha or bool(mask)
        tex['force_opaque'] = force_opaque

        # --- Mip data ---
        mips, pos = _read_mip_levels(
            txd_data, pos, fmt, width, height, num_levels, depth, is_sa)
        tex['mipmap_levels'] = mips

        if mips:
            tex['compressed_data'] = b''.join(m['data'] for m in mips)

        return tex

    except Exception:
        return None
