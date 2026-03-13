#!/usr/bin/env python3
# apps/methods/txd_lc_android.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - LC Android TXD Parser
#
# Liberty City Stories Android TXD format
# Version: 0x1005FFFF (distinct from SA Android 0x1400FFFF / VC Android 0x34005)
# TXD files are embedded directly in the .img archive (not in a separate mobile DB)
# Pixel format: DXT1 only (confirmed across all tested files)
# Platform ID inside NativeTexture STRUCT: 0x00000001

"""
LC Android TXD Parser

Parses RenderWare TXD files from Liberty City Stories (Android).
This format differs from all other mobile GTA ports:
  - TXDs live directly inside the .img file (not a separate .dat/.toc database)
  - RW version 0x1005FFFF (not 0x34005 like VC Android, not 0x1400FFFF like SA Android)
  - NativeTexture STRUCT uses a custom 116-byte header with DXT1 pixel data at +116
  - TexDict claimed size can exceed actual file size when split across two IMG entries
  - Must scan linearly for NativeTexture (0x15) chunks rather than trusting container bounds

Confirmed STRUCT body layout (offsets from STRUCT body start):
  +000  u32   = 0x0C  (raster type flags)
  +004  u32   = 0x06  (compression type indicator)
  +008  u32   = 0x01  (platform sub-id)
  +012  u32   = hasAlpha (0 or 1)
  +016  [16 bytes 0xCC padding]
  +032  char[32]  texture name (null-terminated)
  +064  char[32]  mask/alpha name (null-terminated)
  +096  u8    filterMode
  +097  u8    uAddr
  +098  u8    vAddr
  +099  u8    pad
  +100  u16   width
  +102  u16   height
  +104  u32   (depth/mip flags, typically 0)
  +108  u32   (unknown, typically 0x2AAA or similar)
  +116  bytes DXT1 pixel data (mip0, then subsequent mip levels)
"""

import struct
from typing import List, Dict, Optional, Tuple

## Methods list -
# detect_lc_android_txd
# parse_lc_android_txd
# parse_lc_android_nativetex
# decode_dxt1_to_rgba
# get_mip_size_dxt1
# _scan_nativetex_chunks

RW_VERSION_LC_ANDROID = 0x1005FFFF

# Header field offsets within the NativeTexture STRUCT body
_HDR_RASTER_FLAGS  = 0    # u32
_HDR_COMPRESSION   = 4    # u32  (6 = DXT1 in this format)
_HDR_PLATFORM_SUB  = 8    # u32
_HDR_HAS_ALPHA     = 12   # u32
# +16..+31 = 0xCC padding
_HDR_NAME          = 32   # char[32]
_HDR_MASK_NAME     = 64   # char[32]
_HDR_FILTER        = 96   # u8
_HDR_U_ADDR        = 97   # u8
_HDR_V_ADDR        = 98   # u8
_HDR_PAD           = 99   # u8
_HDR_WIDTH         = 100  # u16
_HDR_HEIGHT        = 102  # u16
_HDR_DEPTH_FLAGS   = 104  # u32
_HDR_UNKNOWN       = 108  # u32
_HDR_PIXEL_DATA    = 116  # bytes — DXT1 data starts here


def detect_lc_android_txd(data: bytes) -> bool:  # vers 1
    """
    Return True if this data is an LC Android TXD (RW version 0x1005FFFF).

    Args:
        data: Raw TXD file bytes

    Returns:
        True if this is an LC Android TXD
    """
    if len(data) < 12:
        return False
    chunk_type = struct.unpack_from('<I', data, 0)[0]
    rw_version  = struct.unpack_from('<I', data, 8)[0]
    return chunk_type == 0x16 and rw_version == RW_VERSION_LC_ANDROID


def get_mip_size_dxt1(width: int, height: int) -> int:  # vers 1
    """
    Calculate byte size of one DXT1 mip level.

    Args:
        width:  Mip width in pixels
        height: Mip height in pixels

    Returns:
        Size in bytes
    """
    bw = max(1, (width  + 3) // 4)
    bh = max(1, (height + 3) // 4)
    return bw * bh * 8  # 8 bytes per 4x4 DXT1 block


def decode_dxt1_to_rgba(data: bytes, width: int, height: int) -> bytes:  # vers 1
    """
    Decode DXT1 compressed data to raw RGBA8888 bytes.

    Args:
        data:   DXT1 compressed bytes
        width:  Image width in pixels
        height: Image height in pixels

    Returns:
        RGBA8888 bytes (width * height * 4), or empty bytes on failure
    """
    if not data or width <= 0 or height <= 0:
        return b''

    bw = max(1, (width  + 3) // 4)
    bh = max(1, (height + 3) // 4)
    needed = bw * bh * 8
    if len(data) < needed:
        return b''

    # Output buffer: RGBA8888
    out = bytearray(width * height * 4)

    pos = 0
    for by in range(bh):
        for bx in range(bw):
            if pos + 8 > len(data):
                break

            c0_raw, c1_raw = struct.unpack_from('<HH', data, pos)
            idx_bits = struct.unpack_from('<I', data, pos + 4)[0]
            pos += 8

            # Expand RGB565 to RGB888
            def rgb565(v: int) -> Tuple[int, int, int]:
                r = ((v >> 11) & 31) * 255 // 31
                g = ((v >>  5) & 63) * 255 // 63
                b = ( v        & 31) * 255 // 31
                return r, g, b

            r0, g0, b0 = rgb565(c0_raw)
            r1, g1, b1 = rgb565(c1_raw)

            # Build 4-colour palette
            if c0_raw > c1_raw:
                # Opaque: 4 colours
                colours = [
                    (r0, g0, b0, 255),
                    (r1, g1, b1, 255),
                    ((2*r0 + r1) // 3, (2*g0 + g1) // 3, (2*b0 + b1) // 3, 255),
                    ((r0 + 2*r1) // 3, (g0 + 2*g1) // 3, (b0 + 2*b1) // 3, 255),
                ]
            else:
                # 1-bit alpha: 3 colours + transparent
                colours = [
                    (r0, g0, b0, 255),
                    (r1, g1, b1, 255),
                    ((r0 + r1) // 2, (g0 + g1) // 2, (b0 + b1) // 2, 255),
                    (0, 0, 0, 0),
                ]

            # Write 4x4 block pixels
            for row in range(4):
                py = by * 4 + row
                if py >= height:
                    continue
                for col in range(4):
                    px = bx * 4 + col
                    if px >= width:
                        continue
                    ci = (idx_bits >> (row * 8 + col * 2)) & 3
                    r, g, b, a = colours[ci]
                    off = (py * width + px) * 4
                    out[off]     = r
                    out[off + 1] = g
                    out[off + 2] = b
                    out[off + 3] = a

    return bytes(out)


def _scan_nativetex_chunks(data: bytes) -> List[int]:  # vers 1
    """
    Linearly scan for all NativeTexture (0x15) chunk offsets.

    Uses a linear scan rather than trusting the TexDict container size,
    because the TexDict claimed size can exceed the actual file size when
    an LC Android TXD is split across two IMG entries.

    Args:
        data: Raw TXD file bytes

    Returns:
        List of byte offsets where valid NativeTexture chunks start
    """
    offsets = []
    pos = 0
    while pos + 12 <= len(data):
        chunk_type = struct.unpack_from('<I', data, pos)[0]
        chunk_size = struct.unpack_from('<I', data, pos + 4)[0]
        chunk_ver  = struct.unpack_from('<I', data, pos + 8)[0]

        if (chunk_type == 0x15
                and chunk_ver == RW_VERSION_LC_ANDROID
                and 80 < chunk_size < 4_000_000):
            offsets.append(pos)
            # Advance past this chunk (clamp to file end)
            pos = min(pos + 12 + chunk_size, len(data))
        else:
            pos += 4  # byte-stride scan to handle misalignment / split files

    return offsets


def parse_lc_android_nativetex(data: bytes, chunk_offset: int) -> Optional[Dict]:  # vers 1
    """
    Parse a single NativeTexture chunk from an LC Android TXD.

    Args:
        data:         Full TXD file bytes
        chunk_offset: Offset of the NativeTexture (0x15) chunk header

    Returns:
        Texture dict compatible with IMG Factory's texture list, or None on failure.
        Keys match those used by _parse_single_texture() in txd_workshop.py:
          name, width, height, depth, format, has_alpha, mipmaps,
          rgba_data, compressed_data, mipmap_levels, filter_mode,
          u_addr, v_addr, raster_format_flags, platform_id
    """
    try:
        # NativeTexture chunk header
        nt_type = struct.unpack_from('<I', data, chunk_offset)[0]
        nt_size = struct.unpack_from('<I', data, chunk_offset + 4)[0]
        if nt_type != 0x15:
            return None

        # First child must be a STRUCT (0x01)
        sp = chunk_offset + 12
        if sp + 12 > len(data):
            return None
        st_type = struct.unpack_from('<I', data, sp)[0]
        st_size = struct.unpack_from('<I', data, sp + 4)[0]
        if st_type != 0x01 or st_size < _HDR_PIXEL_DATA:
            return None

        sb_off = sp + 12          # struct body absolute offset
        sb     = data[sb_off:]    # struct body view

        # Decode header fields
        raster_flags = struct.unpack_from('<I', sb, _HDR_RASTER_FLAGS)[0]
        compression  = struct.unpack_from('<I', sb, _HDR_COMPRESSION)[0]
        platform_sub = struct.unpack_from('<I', sb, _HDR_PLATFORM_SUB)[0]
        has_alpha    = bool(struct.unpack_from('<I', sb, _HDR_HAS_ALPHA)[0])

        name_raw = sb[_HDR_NAME     : _HDR_NAME      + 32]
        mask_raw = sb[_HDR_MASK_NAME: _HDR_MASK_NAME + 32]
        name = name_raw.split(b'\x00')[0].decode('latin-1', errors='replace')
        mask = mask_raw.split(b'\x00')[0].decode('latin-1', errors='replace')

        filter_mode = sb[_HDR_FILTER]
        u_addr      = sb[_HDR_U_ADDR]
        v_addr      = sb[_HDR_V_ADDR]
        width       = struct.unpack_from('<H', sb, _HDR_WIDTH)[0]
        height      = struct.unpack_from('<H', sb, _HDR_HEIGHT)[0]

        if width == 0 or height == 0 or width > 4096 or height > 4096:
            return None

        # Pixel data starts at body offset _HDR_PIXEL_DATA
        pixel_abs = sb_off + _HDR_PIXEL_DATA

        # Collect mip levels — DXT1 only for this format
        mip_levels   = []
        mip_data_all = bytearray()
        w, h = width, height
        pos  = pixel_abs
        level_idx = 0

        while w >= 1 and h >= 1 and pos < len(data):
            mip_sz = get_mip_size_dxt1(w, h)
            if pos + mip_sz > len(data):
                break
            mip_bytes = data[pos : pos + mip_sz]
            # Decode each mip to RGBA so _create_preview_widget and mipmap viewer work
            mip_rgba = decode_dxt1_to_rgba(mip_bytes, w, h)
            mip_levels.append({
                'level':           level_idx,
                'width':           w,
                'height':          h,
                'data':            mip_bytes,       # raw DXT1 (LC Android)
                'rgba_data':       mip_rgba,        # decoded RGBA (workshop standard)
                'compressed_data': mip_bytes,       # alias for workshop compat
                'compressed_size': len(mip_bytes),
            })
            mip_data_all.extend(mip_bytes)
            pos += mip_sz
            level_idx += 1
            if w == 1 and h == 1:
                break
            w = max(1, w // 2)
            h = max(1, h // 2)

        # Top-level rgba_data from mip0 (already decoded above)
        rgba = mip_levels[0]['rgba_data'] if mip_levels else b''

        return {
            # Core identity
            'name':               name or f'texture_{chunk_offset:x}',
            'alpha_name':         mask,
            'width':              width,
            'height':             height,
            'depth':              32,
            'format':             'DXT1',
            'has_alpha':          has_alpha or bool(mask),

            # Pixel data
            'rgba_data':          rgba,
            'compressed_data':    bytes(mip_data_all),
            'mipmap_levels':      mip_levels,
            'mipmaps':            len(mip_levels),

            # Format metadata
            'raster_format_flags': raster_flags,
            'platform_id':         platform_sub,
            'filter_mode':         filter_mode,
            'u_addr':              u_addr,
            'v_addr':              v_addr,

            # LC Android identifiers
            'lc_android':          True,        # flag for write-back path
            'rw_version':          RW_VERSION_LC_ANDROID,
            'compression_code':    compression, # 0x06 = DXT1 in this format
        }

    except Exception as e:
        # Non-fatal — return None and let the caller log/skip
        return None


def parse_lc_android_txd(data: bytes) -> List[Dict]:  # vers 1
    """
    Parse all textures from an LC Android TXD file.

    Handles the case where the TexDict claimed size exceeds the actual file
    size (split-IMG scenario) by scanning linearly for NativeTexture chunks.

    Args:
        data: Raw TXD file bytes

    Returns:
        List of texture dicts (may be empty on failure).
        Each dict is compatible with IMG Factory's internal texture format.
    """
    if not detect_lc_android_txd(data):
        return []

    textures = []
    offsets = _scan_nativetex_chunks(data)

    for off in offsets:
        tex = parse_lc_android_nativetex(data, off)
        if tex is not None:
            textures.append(tex)

    return textures


def get_lc_android_info(data: bytes) -> Dict:  # vers 1
    """
    Return summary info about an LC Android TXD without full decode.
    Useful for the file-list display before any texture is selected.

    Args:
        data: Raw TXD file bytes

    Returns:
        Dict with keys: valid, version, texture_count, texture_names, split_file
    """
    info = {
        'valid':         False,
        'version':       0,
        'texture_count': 0,
        'texture_names': [],
        'split_file':    False,
    }

    if len(data) < 12 or not detect_lc_android_txd(data):
        return info

    info['valid']   = True
    info['version'] = RW_VERSION_LC_ANDROID

    # Check for split-file condition
    texdict_claimed = struct.unpack_from('<I', data, 4)[0]
    if texdict_claimed + 12 > len(data):
        info['split_file'] = True

    offsets = _scan_nativetex_chunks(data)
    info['texture_count'] = len(offsets)

    for off in offsets:
        sp = off + 12
        if sp + 12 > len(data):
            continue
        st_type = struct.unpack_from('<I', data, sp)[0]
        if st_type != 0x01:
            continue
        sb = data[sp + 12:]
        if len(sb) < _HDR_NAME + 32:
            continue
        name = sb[_HDR_NAME:_HDR_NAME + 32].split(b'\x00')[0].decode('latin-1', errors='replace')
        info['texture_names'].append(name)

    return info
