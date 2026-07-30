#!/usr/bin/env python3
#this belongs in apps/methods/chk_parser.py - Version: 1
# X-Seti - Apr 2026 - IMG Factory 1.6
# GTA III PS2 CHK splash texture parser
"""
Parses GTA III Liberty City Stories PS2 SPLASH/*.CHK texture files.

Format ('xet' — 'tex' reversed):
  Header (28 bytes):
    +00  u32  magic = 0x00746578  ('xet\\0')
    +04  u32  reserved = 0
    +08  u32  file_size
    +12  u32  data_size  (= file_size - 28)
    +16  u32  data_size  (duplicate)
    +20  u32  sub_type   (usually 7)
    +24  u32  reserved2  = 0
  Sub-header (60 bytes): metadata — skipped for now
  Pixel data (W × H bytes, 8bpp linear, Y-major)
  CLUT (1024 bytes): 256 × RGBA32 entries
    Alpha stored as PS2-style 0x00–0x80 (0x80 = fully opaque = 255)

All textures use 512-wide rows.  Height is derived from file size:
    pixel_bytes = file_size - 28 (main hdr) - 60 (sub hdr) - 1024 (CLUT)
    height      = pixel_bytes // 512
"""

import struct
import os
from typing import Optional, Dict, Any, List

## Methods list -
# detect_chk
# parse_chk
# chk_to_rgba

CHK_MAGIC = 0x00746578   # 'xet\0' LE — 'tex\0' in big-endian
CHK_MAIN_HEADER  = 28
CHK_SUB_HEADER   = 60
CHK_CLUT_SIZE    = 1024
CHK_WIDTH        = 512   # All GTA3 PS2 splash textures are 512px wide


def detect_chk(data: bytes) -> bool:
    """Return True if data begins with a CHK ('xet') header."""
    if len(data) < 12:
        return False
    magic = struct.unpack_from('<I', data, 0)[0]
    return magic == CHK_MAGIC


def parse_chk(data: bytes, name: str = '') -> Optional[Dict[str, Any]]:
    """
    Parse a GTA III PS2 CHK splash texture from raw bytes.

    Returns a texture dict with keys:
        name, width, height, depth, format,
        pixel_data (raw 8bpp indices), clut (256×RGBA bytes),
        rgba_data (decoded RGBA32 bytes)
    or None on failure.
    """
    if not detect_chk(data):
        return None

    try:
        magic, _res0, file_sz, data_sz, _dup, sub_type, _res1 = \
            struct.unpack_from('<7I', data, 0)

        if file_sz != len(data):
            # Accept minor mismatch — some tools write the wrong size
            pass

        px_offset = CHK_MAIN_HEADER + CHK_SUB_HEADER
        px_bytes  = file_sz - CHK_MAIN_HEADER - CHK_SUB_HEADER - CHK_CLUT_SIZE

        if px_bytes <= 0 or px_offset + px_bytes + CHK_CLUT_SIZE > len(data):
            return None

        W = CHK_WIDTH
        H = px_bytes // W
        if H <= 0:
            return None

        pixel_data = data[px_offset : px_offset + W * H]
        clut_raw   = data[len(data) - CHK_CLUT_SIZE:]

        # Decode PS2 CLUT: alpha 0x80 = fully opaque = 255
        clut_rgba = bytearray(CHK_CLUT_SIZE)
        for i in range(256):
            r, g, b, a_ps2 = clut_raw[i*4 : i*4+4]
            clut_rgba[i*4 : i*4+4] = (r, g, b, min(255, a_ps2 * 2))
        clut_rgba = bytes(clut_rgba)

        # Decode pixel data → RGBA32
        rgba = bytearray(W * H * 4)
        for i in range(W * H):
            idx  = pixel_data[i]
            base = i * 4
            rgba[base : base+4] = clut_rgba[idx*4 : idx*4+4]

        return {
            'name':        name or os.path.splitext(os.path.basename(''))[0] or 'splash',
            'width':       W,
            'height':      H,
            'depth':       8,
            'format':      'PAL8',
            'has_alpha':   True,
            'mipmaps':     1,
            'sub_type':    sub_type,
            'pixel_data':  bytes(pixel_data),   # raw 8bpp indices
            'clut':        clut_rgba,            # 256 RGBA entries (decoded)
            'rgba_data':   bytes(rgba),          # decoded RGBA32 for display
            'mipmap_levels': [],
            'raster_format_flags': 0,
            'platform_id': 0,
        }

    except Exception as e:
        print(f"[CHKParser] Error parsing CHK: {e}")
        return None


def load_chk(path: str) -> Optional[Dict[str, Any]]:
    """Load a CHK file from disk. Returns texture dict or None."""
    try:
        with open(path, 'rb') as f:
            data = f.read()
        name = os.path.splitext(os.path.basename(path))[0]
        return parse_chk(data, name)
    except Exception as e:
        print(f"[CHKParser] Failed to load {path}: {e}")
        return None


def chk_to_rgba(tex: Dict) -> Optional[bytes]:
    """Extract RGBA32 bytes from a parsed CHK texture dict."""
    return tex.get('rgba_data') if tex else None


__all__ = ['detect_chk', 'parse_chk', 'load_chk', 'chk_to_rgba']
