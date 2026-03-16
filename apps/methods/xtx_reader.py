#this belongs in methods/xtx_reader.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6
# XTX Texture Format Reader — VCS PS2/PC Splash Screens
"""
XTX — Custom Rockstar/VCS PS2 palettized texture format.

Format (researched from binary analysis of VCS PC SPLASH/ folder):
  Magic   : "xet\\x00"  (bytes 0x78 0x65 0x74 0x00)
  Header  : 1228 bytes total
    [0:4]   magic "xet\\x00"
    [4:8]   0x00000000 (reserved)
    [8:12]  file_size  (= total file size)
    [12:16] data_size  (= file_size - 28)
    [16:20] data_size  (duplicate)
    [20:24] pixel_format = 7  (PS2 PSMT8 — 8-bit palette-indexed)
    [24:28] 0x00000000
    [28:32] 0x00000000
    [32:40] GS register data
    [40:48] more GS data
    [0x30–0xCB] : 0xCC-filled padding (PS2 SDK uninit marker)
    [0xCC:0x4CC] : CLUT — 256 RGBA entries (4 bytes each = 1024 bytes)
    [0x4CC:EOF] : pixel data, 1 byte per pixel (palette index)

Dimensions derived from:
    file_size = width * height + 1228
    → 512×512 = 263372 bytes
    → 512×256 = 132300 bytes
    → 256×256 = 66764 bytes

Colour order: RGBA (standard, no channel swap needed)
Pixel data: no tile swizzle (linear row-major)
Alpha: PS2 range 0–128 (0x80 = fully opaque), scale by ×2 for 0–256

Files of interest:
    LOADSC*.XTX   — loading screens (512×512)
    SPLASH*.XTX   — splash screens  (256×256 or 512×256)
    SCEE*.XTX     — Sony SCEE logos (512×512)
    MEMCARD.XTX   — memory card icon (512×256)
"""

import os
import struct
from typing import Optional, Tuple

XTX_MAGIC       = b'xet\x00'
XTX_HEADER_SIZE = 1228
XTX_CLUT_OFFSET = 0xCC   # 204
XTX_DATA_OFFSET = 0x4CC  # 1228


def is_xtx(path: str) -> bool:
    """Return True if file is an XTX texture."""
    try:
        with open(path, 'rb') as f:
            magic = f.read(4)
        return magic == XTX_MAGIC
    except Exception:
        return False


def xtx_dimensions(file_size: int) -> Tuple[int, int]:
    """Derive width×height from file size.
    
    Returns (width, height) or (0, 0) if not a recognised size.
    """
    data_bytes = file_size - XTX_HEADER_SIZE
    if data_bytes <= 0:
        return 0, 0

    # Known sizes (all confirmed from actual VCS files)
    known = {
        512 * 512: (512, 512),   # LOADSC*.XTX, SCEE*.XTX
        512 * 256: (512, 256),   # MEMCARD.XTX, SPLASH1.XTX
        256 * 256: (256, 256),   # SPLASH2–10.XTX
    }
    if data_bytes in known:
        return known[data_bytes]

    # Generic fallback: assume square if perfect square root
    import math
    sq = int(math.isqrt(data_bytes))
    if sq * sq == data_bytes:
        return sq, sq

    # Try common widths
    for w in (512, 256, 128, 1024):
        if data_bytes % w == 0:
            h = data_bytes // w
            return w, h

    return 0, 0


def read_xtx(path: str) -> dict:
    """Parse an XTX file and return decoded texture info.

    Returns:
        {
          'width': int,
          'height': int,
          'pixel_format': int,     # always 7 = PSMT8
          'clut': bytes,           # 1024 bytes, 256×RGBA
          'pixels': bytes,         # width×height bytes, palette indices
          'rgba_data': bytes,      # width×height×4 bytes, decoded RGBA
          'error': str or None,
        }
    """
    result = {
        'width': 0, 'height': 0,
        'pixel_format': 7,
        'clut': b'', 'pixels': b'', 'rgba_data': b'',
        'error': None,
    }
    try:
        file_size = os.path.getsize(path)
        with open(path, 'rb') as f:
            raw = f.read()

        if raw[:4] != XTX_MAGIC:
            result['error'] = f"Not an XTX file (magic={raw[:4].hex()})"
            return result

        if file_size < XTX_HEADER_SIZE + 1:
            result['error'] = "File too small"
            return result

        w, h = xtx_dimensions(file_size)
        if w == 0:
            result['error'] = f"Unknown dimensions for file size {file_size}"
            return result

        pixel_format = struct.unpack_from('<I', raw, 20)[0]
        clut   = raw[XTX_CLUT_OFFSET : XTX_CLUT_OFFSET + 1024]
        pixels = raw[XTX_DATA_OFFSET  : XTX_DATA_OFFSET + w * h]

        # Decode palette → RGBA
        rgba = _decode_pixels(pixels, clut)

        result.update({
            'width':        w,
            'height':       h,
            'pixel_format': pixel_format,
            'clut':         clut,
            'pixels':       pixels,
            'rgba_data':    rgba,
        })
    except Exception as e:
        result['error'] = str(e)
    return result


def _decode_pixels(pixels: bytes, clut: bytes) -> bytes:
    """Convert 8-bit palette indices + CLUT to RGBA bytes."""
    out = bytearray(len(pixels) * 4)
    for i, px in enumerate(pixels):
        base = px * 4
        r = clut[base]
        g = clut[base + 1]
        b = clut[base + 2]
        a = clut[base + 3]
        # PS2 alpha: 0x80 = fully opaque → scale to 0–255
        a_out = min(255, a * 2)
        out[i*4]     = r
        out[i*4 + 1] = g
        out[i*4 + 2] = b
        out[i*4 + 3] = a_out
    return bytes(out)


def xtx_to_qimage(path: str):
    """Decode XTX to a PyQt6 QImage (RGBA8888). Returns None on failure."""
    try:
        info = read_xtx(path)
        if info['error'] or not info['rgba_data']:
            return None
        from PyQt6.QtGui import QImage
        return QImage(
            info['rgba_data'],
            info['width'],
            info['height'],
            info['width'] * 4,
            QImage.Format.Format_RGBA8888,
        ).copy()  # .copy() detaches from the buffer
    except Exception:
        return None


def xtx_to_qpixmap(path: str, max_size: int = 512):
    """Decode XTX to a QPixmap, scaled to max_size if larger. Returns None on failure."""
    try:
        qimg = xtx_to_qimage(path)
        if qimg is None:
            return None
        from PyQt6.QtGui import QPixmap
        from PyQt6.QtCore import Qt
        px = QPixmap.fromImage(qimg)
        if px.width() > max_size or px.height() > max_size:
            px = px.scaled(max_size, max_size,
                           Qt.AspectRatioMode.KeepAspectRatio,
                           Qt.TransformationMode.SmoothTransformation)
        return px
    except Exception:
        return None


__all__ = ['is_xtx', 'read_xtx', 'xtx_dimensions', 'xtx_to_qimage', 'xtx_to_qpixmap']
