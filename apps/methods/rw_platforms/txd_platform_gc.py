#!/usr/bin/env python3
# apps/methods/txd_platform_gc.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - GameCube TXD Platform Parser
#
# Handles GameCube TXD format (unknown GTA title — no confirmed retail game).
# Platform ID: 3 (DEVICE_GC) inside NativeTexture STRUCT.
# RW version: 0x00035000 (shared with GTA III/VC Xbox and LCS PSP)
#
# STATUS: No confirmed binary files or retail game found using this format.
#         Structure is entirely unconfirmed. File exists only to prevent
#         txd_workshop.py from silently falling through to the PC parser
#         on GC platform IDs. Returns raw bytes only with no pixel decode.

"""
GameCube TXD Platform Parser (STUB)

GameCube NativeTexture format:
  - Platform ID 3 inside NativeTexture STRUCT
  - Uses Nintendo GX texture formats (CMPR = S3TC-like, RGBA8, RGB565, etc.)
  - Tile-based storage (EFB/XFB format — not linear)
  - No confirmed GTA retail release on GameCube — format is theoretical/test only

This file is intentionally minimal. If real GC TXD files are ever obtained,
the format should be reverse-engineered from binary and this file rewritten.
"""

import struct
from typing import Dict, Optional

## Methods list -
# detect_gc_txd
# parse_gc_nativetex

RW_GC = 0x00035000


def detect_gc_txd(platform_id: int) -> bool:  # vers 1
    """Return True if the NativeTexture platform_id indicates GameCube."""
    return platform_id == 3


def parse_gc_nativetex(txd_data: bytes, chunk_offset: int,
                       index: int) -> Optional[Dict]:  # vers 1
    """
    Parse a GameCube NativeTexture chunk — STUB, returns raw bytes only.

    No pixel decode is implemented. rgba_data will be empty.
    mipmap_levels[0]['data'] contains the raw struct body for inspection.

    Args:
        txd_data:     Full TXD file bytes
        chunk_offset: Offset of the NativeTexture (0x15) chunk header
        index:        Texture index for fallback naming

    Returns:
        Minimal texture dict, or None if chunk header is invalid.
    """
    tex = {
        'name': f'gc_texture_{index}',
        'alpha_name': '',
        'width': 0, 'height': 0, 'depth': 32,
        'format': 'GC_UNKNOWN',
        'has_alpha': False,
        'mipmaps': 0,
        'rgba_data': b'',           # not implemented
        'compressed_data': b'',
        'mipmap_levels': [],
        'raster_format_flags': 0,
        'platform_id': 3,
        'filter_mode': 0,
        'u_addr': 0, 'v_addr': 0,
        'gc_unconfirmed': True,     # flag: format not confirmed from real files
    }

    try:
        nt_type = struct.unpack_from('<I', txd_data, chunk_offset)[0]
        if nt_type != 0x15:
            return None

        sp = chunk_offset + 12
        st_type = struct.unpack_from('<I', txd_data, sp)[0]
        if st_type != 0x01:
            return None

        st_size = struct.unpack_from('<I', txd_data, sp + 4)[0]
        body    = txd_data[sp + 12 : sp + 12 + st_size]

        # Store raw struct body for manual inspection
        tex['mipmap_levels'] = [{
            'level':    0,
            'width':    0,
            'height':   0,
            'data':     body,
            'swizzled': True,
        }]
        tex['compressed_data'] = body

        # Attempt to read name from offset +8 (standard position)
        if len(body) >= 40:
            name = body[8:40].rstrip(b'\x00').decode('latin-1', errors='replace')
            if name:
                tex['name'] = name

        return tex

    except Exception:
        return None
