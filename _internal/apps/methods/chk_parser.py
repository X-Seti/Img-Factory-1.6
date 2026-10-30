#!/usr/bin/env python3
#this belongs in apps/methods/chk_parser.py - Version: 2
# X-Seti - October05 2026 - IMG Factory 1.6 - LCS CHK Texture Parser Writer

"""
LCS .CHK textures (PS2/PSP); shares xet decoding with xtx_reader.
"""

import os
import struct
from typing import Any, Dict, Optional

from apps.methods.xtx_reader import parse_stories_textures, write_stories_textures

##Methods list -
# chk_to_rgba
# detect_chk
# load_chk
# parse_chk
# write_chk

CHK_MAGIC = 0x00746578   # 'xet\0' little-endian


def chk_to_rgba(tex: Dict) -> Optional[bytes]: #vers 2
    """RGBA bytes from a parsed CHK texture dict."""
    return tex.get('rgba_data') if tex else None


def detect_chk(data: bytes) -> bool: #vers 2
    """True if data begins with the 'xet' ident."""
    return len(data) >= 4 and struct.unpack_from('<I', data, 0)[0] == CHK_MAGIC


def load_chk(path: str) -> Optional[Dict[str, Any]]: #vers 2
    """Load CHK file; None and printed error on failure."""
    try:
        with open(path, 'rb') as f:
            data = f.read()
        return parse_chk(data, os.path.splitext(os.path.basename(path))[0])
    except (OSError, ValueError, struct.error) as e:
        print(f"[CHKParser] Failed to load {path}: {e}")
        return None


def parse_chk(data: bytes, name: str = '') -> Dict[str, Any]: #vers 2
    """Parse CHK bytes; first texture fields plus 'textures' list."""
    if not detect_chk(data):
        raise ValueError("Not a CHK file ('xet' ident missing)")
    texs = parse_stories_textures(data)
    t = texs[0]
    return {
        'name': name or t['name'],
        'texture_name': t['name'],
        'width': t['width'], 'height': t['height'],
        'depth': t['depth'], 'format': t['format'],
        'has_alpha': t['has_alpha'],
        'mipmaps': 1,
        'platform': t['platform'],
        'pixel_data': t['indices'],
        'clut': t['palette'],
        'palette': t['palette'],
        'rgba_data': t['rgba_data'],
        'mipmap_levels': [],
        'raster_format_flags': 0,
        'platform_id': 0,
        'textures': texs,
    }


def write_chk(original_bytes: bytes, rgba) -> bytes: #vers 1
    """Return CHK bytes with new RGBA; header and size unchanged."""
    if not detect_chk(original_bytes):
        raise ValueError("Not a CHK file ('xet' ident missing)")
    return write_stories_textures(original_bytes, rgba)


__all__ = ['chk_to_rgba', 'detect_chk', 'load_chk', 'parse_chk', 'write_chk']
